# CRE Client SDKs

This directory contains auto-generated client SDKs for the Cuneiform Runtime Environment (CRE) APIs in multiple programming languages.

## Available SDKs

### Python SDK
- **File:** `python_sdk.py`
- **Dependencies:** `requests`
- **Python Version:** 3.6+

### Go SDK
- **File:** `go_sdk.go`
- **Package:** `creapi`
- **Go Version:** 1.13+

### Node.js SDK
- **File:** `nodejs_sdk.js`
- **Dependencies:** `node-fetch`
- **Node Version:** 12.0.0+

## Quick Start

### Python

```python
from python_sdk import CREMasterClient

# Initialize client
client = CREMasterClient("http://localhost:4142")

# Get status
status = client.get_status()
print(f"Idle workers: {status['idle_count']}")
print(f"Busy workers: {status['busy_count']}")

# Get history
history = client.get_history()
print(f"Cache hit rate: {history['hit_rate'] * 100:.1f}%")

# Use as context manager
with CREMasterClient() as client:
    status = client.get_status()
    print(status)
```

### Go

```go
package main

import (
	"context"
	"fmt"
	"log"

	"your-module/creapi"
)

func main() {
	client := creapi.NewClient("http://localhost:4142")

	// Get status
	status, err := client.GetStatus(context.Background())
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Idle workers: %d\n", status.IdleCount)
	fmt.Printf("Busy workers: %d\n", status.BusyCount)

	// Get history
	history, err := client.GetHistory(context.Background())
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Cache hit rate: %.1f%%\n", history.HitRate*100)

	// Monitor status continuously
	monitor := client.NewStatusMonitor(5 * time.Second)
	monitor.Start()

	go func() {
		for status := range monitor.StatusCh {
			fmt.Printf("Status: %d idle, %d busy\n", status.IdleCount, status.BusyCount)
		}
	}()

	// Stop monitoring later
	monitor.Stop()
}
```

### Node.js

```javascript
const CREClient = require('./nodejs_sdk');
const { YAWLDashboardClient } = require('./nodejs_sdk');

// CRE Master API
const creClient = new CREClient('http://localhost:4142');

// Get status
creClient.getStatus()
  .then(status => {
    console.log(`Idle workers: ${status.idle_count}`);
    console.log(`Busy workers: ${status.busy_count}`);
  })
  .catch(error => console.error('Error:', error));

// Get history
creClient.getHistory()
  .then(history => {
    console.log(`Cache hit rate: ${(history.hit_rate * 100).toFixed(1)}%`);
  })
  .catch(error => console.error('Error:', error));

// Monitor status
const stop = creClient.monitorStatus(5000, (status) => {
  console.log(`Status: ${status.idle_count} idle, ${status.busy_count} busy`);
});

// YAWL Dashboard API
const yawlClient = new YAWLDashboardClient('http://localhost:8081');

// List events
yawlClient.listEvents({ limit: 50 })
  .then(events => console.log('Events:', events))
  .catch(error => console.error('Error:', error));

// Get statistics
yawlClient.getStatistics({ time_window: '24h' })
  .then(stats => console.log('Stats:', stats))
  .catch(error => console.error('Error:', error));
```

## API Reference

### CRE Master API

#### Methods

##### `get_status()` / `GetStatus(ctx)` / `getStatus()`

Get the current status of the CRE Master.

**Returns:**
- `idle_workers` (list): Idle worker process identifiers
- `busy_workers` (list): Currently executing workers
- `pending_queue` (list): Applications waiting to be scheduled
- `subscriptions` (list): Connected clients
- `worker_count` (int): Total workers
- `idle_count` (int): Idle workers
- `busy_count` (int): Busy workers
- `queue_length` (int): Pending applications
- `timestamp` (str): ISO 8601 timestamp

**Example:**
```python
status = client.get_status()
if status['queue_length'] > 0:
    print(f"Warning: {status['queue_length']} applications pending")
```

##### `get_history()` / `GetHistory(ctx)` / `getHistory()`

Get execution history and cache information.

**Returns:**
- `cache_entries` (int): Total cached entries
- `total_executions` (int): Total executions
- `cache_hits` (int): Cache hits
- `cache_misses` (int): Cache misses
- `hit_rate` (float): Hit rate (0.0-1.0)
- `cache_size_bytes` (int): Cache size
- `oldest_entry` (object): First cached entry
- `latest_entry` (object): Latest cached entry
- `entries` (list): Detailed entries

**Example:**
```python
history = client.get_history()
efficiency = history['cache_hits'] / history['total_executions']
print(f"Efficiency: {efficiency * 100:.1f}%")
```

### YAWL Dashboard API

#### Methods (Node.js Only)

##### `listEvents(options)`

Get workflow events with pagination and filtering.

**Options:**
- `limit` (int): Max events to return
- `offset` (int): Skip N events
- `filter` (str): Event type filter
- `trace_id` (str): Filter by trace ID

##### `getTraceEvents(traceId)`

Get all events for a specific trace.

##### `listTraces(options)`

Get workflow traces with filtering.

**Options:**
- `limit` (int): Max traces to return
- `offset` (int): Skip N traces
- `status` (str): running, completed, failed
- `min_duration_ms` (int): Minimum duration
- `max_duration_ms` (int): Maximum duration

##### `getStatistics(options)`

Get aggregated workflow statistics.

**Options:**
- `time_window` (str): 1h, 24h, 7d, 30d, all

##### `clearData()`

Clear all events and traces (destructive operation).

## Error Handling

All SDKs raise/throw exceptions for errors:

### Python

```python
from python_sdk import CREMasterClient, APIException, BadRequest, NotFound, ServerError

try:
    client = CREMasterClient()
    status = client.get_status()
except BadRequest as e:
    print(f"Bad request: {e}")
except NotFound as e:
    print(f"Not found: {e}")
except ServerError as e:
    print(f"Server error: {e}")
except APIException as e:
    print(f"API error: {e}")
```

### Go

```go
import "your-module/creapi"

status, err := client.GetStatus(context.Background())
if err != nil {
    if apiErr, ok := err.(*creapi.APIError); ok {
        fmt.Printf("API Error: %s - %s\n", apiErr.Error, apiErr.Message)
    } else {
        fmt.Printf("Error: %v\n", err)
    }
}
```

### Node.js

```javascript
const { APIError } = require('./nodejs_sdk');

try {
    const status = await client.getStatus();
} catch (error) {
    if (error instanceof APIError) {
        console.error(`API Error (${error.error}): ${error.message}`);
    } else {
        console.error('Error:', error);
    }
}
```

## Installation

### Python

```bash
pip install requests
cp sdks/python_sdk.py /path/to/your/project/
```

### Go

```bash
# Copy the Go SDK file to your project
cp sdks/go_sdk.go /path/to/your/project/

# Import in your code
import "your-module/creapi"
```

### Node.js

```bash
npm install node-fetch

# Copy the SDK file
cp sdks/nodejs_sdk.js /path/to/your/project/

# Require in your code
const CREClient = require('./nodejs_sdk');
```

## Advanced Usage

### Python - Continuous Monitoring

```python
import time
from python_sdk import CREMasterClient

client = CREMasterClient()

while True:
    try:
        status = client.get_status()
        print(f"[{status['timestamp']}] Workers: {status['idle_count']} idle, "
              f"{status['busy_count']} busy, Queue: {status['queue_length']}")
        time.sleep(5)
    except Exception as e:
        print(f"Error: {e}")
        time.sleep(10)
```

### Go - Status Monitoring with Channels

```go
ctx, cancel := context.WithCancel(context.Background())
defer cancel()

monitor := client.NewStatusMonitor(5 * time.Second)
monitor.Start()

go func() {
    for {
        select {
        case status := <-monitor.StatusCh:
            fmt.Printf("Workers: %d idle, %d busy\n", status.IdleCount, status.BusyCount)
        case err := <-monitor.ErrorCh:
            log.Printf("Monitor error: %v", err)
        case <-ctx.Done():
            return
        }
    }
}()

// Stop monitor
monitor.Stop()
```

### Node.js - Async/Await Pattern

```javascript
async function monitorSystem() {
  const client = new CREClient();

  try {
    while (true) {
      const status = await client.getStatus();
      const history = await client.getHistory();

      console.log('Status:', {
        workers: { idle: status.idle_count, busy: status.busy_count },
        queue: status.queue_length,
        cache: { hitRate: `${(history.hit_rate * 100).toFixed(1)}%` }
      });

      await new Promise(resolve => setTimeout(resolve, 5000));
    }
  } catch (error) {
    console.error('Error:', error);
  }
}

monitorSystem();
```

## Configuration

### Custom Base URLs

All SDKs accept a custom base URL:

**Python:**
```python
client = CREMasterClient("http://cre.example.com:4142")
```

**Go:**
```go
client := creapi.NewClient("http://cre.example.com:4142")
```

**Node.js:**
```javascript
const client = new CREClient('http://cre.example.com:4142');
```

### Request Timeout

**Python:**
```python
client = CREMasterClient(base_url="http://localhost:4142", timeout=60)
```

**Go:**
```go
client := creapi.NewClient("http://localhost:4142")
client.Timeout = 60 * time.Second
client.HTTPClient.Timeout = 60 * time.Second
```

**Node.js:**
```javascript
const client = new CREClient('http://localhost:4142', 60000); // 60 seconds
```

## Best Practices

1. **Error Handling:** Always wrap API calls in try-catch blocks
2. **Connection Pooling:** Reuse client instances instead of creating new ones
3. **Rate Limiting:** Implement backoff for failed requests
4. **Monitoring:** Use polling interval of 5-10 seconds minimum
5. **Caching:** Cache responses when appropriate to reduce load
6. **Timeouts:** Set reasonable timeouts for your use case
7. **Logging:** Log API errors and responses for debugging

## SDK Generation

To regenerate SDKs from OpenAPI specifications:

```bash
cd /path/to/cre
python3 scripts/generate-sdks.py \
  --output-dir ./sdks \
  --api-specs ./docs/openapi-cre-master.yaml ./docs/openapi-yawl-dashboard.yaml
```

## Support

For issues or questions:
- GitHub: https://github.com/ruvnet/claude-flow
- Email: support@cuneiform-lang.org

## License

Apache License 2.0 - See LICENSE file in repository root
