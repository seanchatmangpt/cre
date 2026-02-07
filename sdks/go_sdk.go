// Package creapi provides a client SDK for CRE Master API v0.2.0
//
// The Cuneiform runtime environment (CRE) is a distributed execution environment
// for programming languages implemented in distributed Erlang.
package creapi

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"time"
)

// APIError represents an API error response
type APIError struct {
	Error   string                 `json:"error"`
	Message string                 `json:"message"`
	Details map[string]interface{} `json:"details,omitempty"`
	TraceID string                 `json:"trace_id,omitempty"`
}

func (e *APIError) Error() string {
	return fmt.Sprintf("API Error (%s): %s", e.Error, e.Message)
}

// Client is the main API client for CRE Master API
type Client struct {
	BaseURL    string
	Timeout    time.Duration
	HTTPClient *http.Client
}

// NewClient creates a new CRE API client
func NewClient(baseURL string) *Client {
	if baseURL == "" {
		baseURL = "http://localhost:4142"
	}
	return &Client{
		BaseURL: baseURL,
		Timeout: 30 * time.Second,
		HTTPClient: &http.Client{
			Timeout: 30 * time.Second,
		},
	}
}

// request makes an HTTP request and returns the response body
func (c *Client) request(ctx context.Context, method, path string, params url.Values) ([]byte, error) {
	urlStr := c.BaseURL + path
	if len(params) > 0 {
		urlStr += "?" + params.Encode()
	}

	req, err := http.NewRequestWithContext(ctx, method, urlStr, nil)
	if err != nil {
		return nil, err
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode >= 400 {
		var apiErr APIError
		json.Unmarshal(respBody, &apiErr)
		if apiErr.Message == "" {
			apiErr.Message = fmt.Sprintf("HTTP %d", resp.StatusCode)
		}
		return nil, &apiErr
	}

	return respBody, nil
}

// CREStatus represents the status response
type CREStatus struct {
	IdleWorkers   []string          `json:"idle_workers"`
	BusyWorkers   []BusyWorker      `json:"busy_workers"`
	PendingQueue  []PendingApp      `json:"pending_queue"`
	Subscriptions []string          `json:"subscriptions"`
	WorkerCount   int               `json:"worker_count"`
	IdleCount     int               `json:"idle_count"`
	BusyCount     int               `json:"busy_count"`
	QueueLength   int               `json:"queue_length"`
	Timestamp     string            `json:"timestamp"`
}

// BusyWorker represents a worker executing an application
type BusyWorker struct {
	WorkerID  string `json:"worker_id"`
	AppID     string `json:"app_id"`
	StartTime string `json:"start_time"`
	ElapsedMS int64  `json:"elapsed_ms"`
}

// PendingApp represents a pending application
type PendingApp struct {
	AppID      string `json:"app_id"`
	QueuedTime string `json:"queued_time"`
	Priority   int    `json:"priority"`
}

// CREHistory represents the history response
type CREHistory struct {
	CacheEntries  int           `json:"cache_entries"`
	TotalExecutes int           `json:"total_executions"`
	CacheHits     int           `json:"cache_hits"`
	CacheMisses   int           `json:"cache_misses"`
	HitRate       float64       `json:"hit_rate"`
	CacheSizeB    int64         `json:"cache_size_bytes"`
	OldestEntry   *CacheEntry  `json:"oldest_entry"`
	LatestEntry   *CacheEntry  `json:"latest_entry"`
	Entries       []CacheEntry `json:"entries,omitempty"`
}

// CacheEntry represents a cached application-result pair
type CacheEntry struct {
	AppID           string `json:"app_id"`
	Timestamp       string `json:"timestamp"`
	ResultSizeB     int64  `json:"result_size_bytes"`
	ExecutionTimeMS int64  `json:"execution_time_ms"`
}

// GetStatus retrieves the current CRE Master status
//
// Returns the status of the CRE Master process, including:
// - Active worker list with status
// - Idle worker list
// - Pending applications queue
// - Subscriptions information
//
// Example:
//
//	client := creapi.NewClient("http://localhost:4142")
//	status, err := client.GetStatus(context.Background())
//	if err != nil {
//		log.Fatal(err)
//	}
//	fmt.Printf("Idle workers: %d\n", status.IdleCount)
func (c *Client) GetStatus(ctx context.Context) (*CREStatus, error) {
	resp, err := c.request(ctx, http.MethodGet, "/status.json", url.Values{})
	if err != nil {
		return nil, err
	}

	var status CREStatus
	if err := json.Unmarshal(resp, &status); err != nil {
		return nil, fmt.Errorf("failed to unmarshal response: %w", err)
	}

	return &status, nil
}

// GetHistory retrieves the execution history and cache information
//
// Returns the execution history and cache map containing all memoized
// application-result pairs. Shows caching performance metrics.
//
// Example:
//
//	client := creapi.NewClient("http://localhost:4142")
//	history, err := client.GetHistory(context.Background())
//	if err != nil {
//		log.Fatal(err)
//	}
//	fmt.Printf("Cache hit rate: %.1f%%\n", history.HitRate*100)
func (c *Client) GetHistory(ctx context.Context) (*CREHistory, error) {
	resp, err := c.request(ctx, http.MethodGet, "/history.json", url.Values{})
	if err != nil {
		return nil, err
	}

	var history CREHistory
	if err := json.Unmarshal(resp, &history); err != nil {
		return nil, fmt.Errorf("failed to unmarshal response: %w", err)
	}

	return &history, nil
}

// StatusMonitor continuously monitors CRE status
type StatusMonitor struct {
	client   *Client
	ticker   *time.Ticker
	ctx      context.Context
	cancel   context.CancelFunc
	StatusCh chan *CREStatus
	ErrorCh  chan error
}

// NewStatusMonitor creates a new status monitor
func (c *Client) NewStatusMonitor(interval time.Duration) *StatusMonitor {
	ctx, cancel := context.WithCancel(context.Background())
	return &StatusMonitor{
		client:   c,
		ticker:   time.NewTicker(interval),
		ctx:      ctx,
		cancel:   cancel,
		StatusCh: make(chan *CREStatus),
		ErrorCh:  make(chan error),
	}
}

// Start begins monitoring
func (sm *StatusMonitor) Start() {
	go func() {
		for {
			select {
			case <-sm.ctx.Done():
				sm.ticker.Stop()
				return
			case <-sm.ticker.C:
				status, err := sm.client.GetStatus(sm.ctx)
				if err != nil {
					sm.ErrorCh <- err
				} else {
					sm.StatusCh <- status
				}
			}
		}
	}()
}

// Stop stops monitoring
func (sm *StatusMonitor) Stop() {
	sm.cancel()
}
