# CRE API Reference Documentation

## Table of Contents

1. [Overview](#overview)
2. [CRE Master API](#cre-master-api)
3. [YAWL Dashboard API](#yawl-dashboard-api)
4. [Authentication](#authentication)
5. [Error Handling](#error-handling)
6. [Examples](#examples)

---

## Overview

The Cuneiform Runtime Environment (CRE) provides two main HTTP APIs:

1. **CRE Master API** - System status and execution history monitoring
2. **YAWL Dashboard API** - Workflow execution tracing and analytics

Both APIs are implemented using the Erlang Cowboy web framework and return JSON responses.

### Base URLs

- CRE Master API: `http://localhost:4142` (default)
- YAWL Dashboard API: `http://localhost:8081` (default)

### API Versioning

Currently at version 0.2.0 for CRE Master and 1.0.0 for YAWL Dashboard.

---

## CRE Master API

### Server Status Monitoring

The CRE Master API provides endpoints for monitoring the distributed runtime environment.

#### Base URL
```
http://localhost:4142
```

#### Port Configuration
The default port is 4142, but the CRE Master automatically selects the next available port if 4142 is in use.

---

### Endpoints

#### 1. Get System Status

**GET** `/` or `/status.json`

Returns the current status of the CRE Master, including active workers, idle workers, pending applications queue, and client subscriptions.

**Request:**
```bash
curl http://localhost:4142/status.json
```

**Response:**
```json
{
  "idle_workers": [
    "worker@node1.example.com",
    "worker@node2.example.com"
  ],
  "busy_workers": [
    {
      "worker_id": "worker@node3.example.com",
      "app_id": "app_123",
      "start_time": "2024-02-06T18:40:20Z",
      "elapsed_ms": 300000
    }
  ],
  "pending_queue": [
    {
      "app_id": "app_456",
      "queued_time": "2024-02-06T18:42:20Z",
      "priority": 5
    }
  ],
  "subscriptions": [
    "client@node1.example.com",
    "client@node2.example.com"
  ],
  "worker_count": 3,
  "idle_count": 2,
  "busy_count": 1,
  "queue_length": 1,
  "timestamp": "2024-02-06T18:45:20Z"
}
```

**Status Code:** `200 OK`

**Response Schema:**
- `idle_workers` (array): List of idle worker process identifiers
- `busy_workers` (array): List of workers currently executing applications
- `pending_queue` (array): Applications waiting to be scheduled
- `subscriptions` (array): Connected client processes
- `worker_count` (integer): Total registered workers
- `idle_count` (integer): Number of idle workers
- `busy_count` (integer): Number of busy workers
- `queue_length` (integer): Number of pending applications
- `timestamp` (string): ISO 8601 timestamp

**Error Response:**
```json
{
  "error": "internal_server_error",
  "message": "Failed to retrieve status from CRE Master",
  "trace_id": "trace_abc123def456"
}
```

---

#### 2. Get Execution History and Cache

**GET** `/history.json`

Returns the execution history and cache map containing all memoized application-result pairs. This shows caching performance metrics and historical execution information.

**Request:**
```bash
curl http://localhost:4142/history.json
```

**Response:**
```json
{
  "cache_entries": 42,
  "total_executions": 156,
  "cache_hits": 114,
  "cache_misses": 42,
  "hit_rate": 0.73,
  "cache_size_bytes": 524288,
  "oldest_entry": {
    "app_id": "app_001",
    "timestamp": "2024-02-06T10:30:45Z",
    "result_size_bytes": 2048,
    "execution_time_ms": 5000
  },
  "latest_entry": {
    "app_id": "app_987",
    "timestamp": "2024-02-06T18:45:20Z",
    "result_size_bytes": 4096,
    "execution_time_ms": 2500
  },
  "entries": [
    {
      "app_id": "app_001",
      "timestamp": "2024-02-06T10:30:45Z",
      "result_size_bytes": 2048,
      "execution_time_ms": 5000
    },
    {
      "app_id": "app_002",
      "timestamp": "2024-02-06T10:35:12Z",
      "result_size_bytes": 1024,
      "execution_time_ms": 3200
    }
  ]
}
```

**Status Code:** `200 OK`

**Response Schema:**
- `cache_entries` (integer): Total number of cached application-result pairs
- `total_executions` (integer): Total number of application executions
- `cache_hits` (integer): Number of times cached results were used
- `cache_misses` (integer): Number of cache misses requiring computation
- `hit_rate` (number): Cache efficiency (0.0 to 1.0)
- `cache_size_bytes` (integer): Total cache size in bytes
- `oldest_entry` (object): First cached entry with metadata
- `latest_entry` (object): Most recent cached entry with metadata
- `entries` (array): Detailed list of cached entries

**Performance Insights:**
- High hit_rate (>0.7) indicates effective memoization
- Total cache size is useful for memory management
- Execution time metrics help identify bottlenecks

---

## YAWL Dashboard API

### Workflow Execution Monitoring

The YAWL Dashboard API provides real-time monitoring of workflow executions with OpenTelemetry integration.

#### Base URL
```
http://localhost:8081
```

---

### Endpoints

#### 1. Get Dashboard Interface

**GET** `/`

Returns the interactive HTML dashboard for monitoring workflow executions.

**Request:**
```bash
curl http://localhost:8081/
```

**Response:**
HTML dashboard interface (Content-Type: text/html)

**Status Code:** `200 OK`

---

#### 2. List All Events

**GET** `/api/events`

Retrieves a paginated list of all recorded workflow execution events.

**Query Parameters:**
- `limit` (integer, default: 100): Maximum events to return (1-10000)
- `offset` (integer, default: 0): Number of events to skip
- `filter` (string): Event type filter (task_started, task_completed, state_changed, error, all)
- `trace_id` (string): Filter by OpenTelemetry trace ID

**Request:**
```bash
curl "http://localhost:8081/api/events?limit=50&offset=0&filter=task_completed"
```

**Response:**
```json
{
  "events": [
    {
      "event_id": "evt_001",
      "type": "task_started",
      "trace_id": "trace_abc123",
      "timestamp": "2024-02-06T18:45:00Z",
      "workflow_id": "wf_001",
      "task_name": "validate_input"
    },
    {
      "event_id": "evt_002",
      "type": "task_completed",
      "trace_id": "trace_abc123",
      "timestamp": "2024-02-06T18:45:05Z",
      "workflow_id": "wf_001",
      "task_name": "validate_input",
      "result": "success",
      "duration_ms": 5000
    },
    {
      "event_id": "evt_003",
      "type": "error",
      "trace_id": "trace_abc123",
      "timestamp": "2024-02-06T18:45:10Z",
      "workflow_id": "wf_001",
      "error_message": "Processing failed"
    }
  ],
  "total": 1524,
  "limit": 50,
  "offset": 0
}
```

**Status Code:** `200 OK`

**Event Types:**
- `task_started` - Task execution started
- `task_completed` - Task execution completed successfully
- `state_changed` - Workflow state changed
- `error` - An error occurred
- `warning` - A warning was logged
- `info` - Informational message

---

#### 3. Get Events for Specific Trace

**GET** `/api/events/{traceId}`

Retrieves all events associated with a specific OpenTelemetry trace, providing a detailed view of a single workflow execution.

**Path Parameters:**
- `traceId` (string, required): OpenTelemetry trace identifier

**Request:**
```bash
curl http://localhost:8081/api/events/trace_abc123
```

**Response:**
```json
{
  "trace_id": "trace_abc123",
  "workflow_id": "wf_001",
  "start_time": "2024-02-06T18:45:00Z",
  "end_time": "2024-02-06T18:50:30Z",
  "duration_ms": 330000,
  "status": "completed",
  "events": [
    {
      "event_id": "evt_001",
      "timestamp": "2024-02-06T18:45:00Z",
      "type": "task_started",
      "task_name": "validate_input",
      "duration_ms": 5000
    },
    {
      "event_id": "evt_002",
      "timestamp": "2024-02-06T18:45:05Z",
      "type": "task_completed",
      "task_name": "validate_input",
      "result": "success"
    }
  ]
}
```

**Status Code:** `200 OK`

**Error Response (404 Not Found):**
```json
{
  "error": "not_found",
  "message": "Trace trace_xyz789 not found"
}
```

---

#### 4. List All Traces

**GET** `/api/traces`

Retrieves a paginated list of all OpenTelemetry traces (workflow executions).

**Query Parameters:**
- `limit` (integer, default: 50): Maximum traces to return (1-1000)
- `offset` (integer, default: 0): Number of traces to skip
- `status` (string): Filter by status (running, completed, failed, all)
- `min_duration_ms` (integer): Minimum execution duration
- `max_duration_ms` (integer): Maximum execution duration

**Request:**
```bash
curl "http://localhost:8081/api/traces?status=completed&min_duration_ms=1000&max_duration_ms=60000"
```

**Response:**
```json
{
  "traces": [
    {
      "trace_id": "trace_abc123",
      "workflow_id": "wf_001",
      "start_time": "2024-02-06T18:45:00Z",
      "end_time": "2024-02-06T18:50:30Z",
      "duration_ms": 330000,
      "status": "completed",
      "event_count": 15
    },
    {
      "trace_id": "trace_xyz789",
      "workflow_id": "wf_002",
      "start_time": "2024-02-06T18:40:00Z",
      "end_time": null,
      "status": "running",
      "event_count": 8
    }
  ],
  "total": 187,
  "limit": 50,
  "offset": 0
}
```

**Status Code:** `200 OK`

**Trace Statuses:**
- `running` - Workflow currently executing
- `completed` - Workflow finished successfully
- `failed` - Workflow encountered an error
- `cancelled` - Workflow was manually cancelled

---

#### 5. Get Statistics

**GET** `/api/stats`

Retrieves aggregated statistics about workflow executions.

**Query Parameters:**
- `time_window` (string, default: 24h): Aggregation window (1h, 24h, 7d, 30d, all)

**Request:**
```bash
curl "http://localhost:8081/api/stats?time_window=24h"
```

**Response:**
```json
{
  "time_window": "24h",
  "total_executions": 156,
  "completed": 142,
  "failed": 12,
  "running": 2,
  "success_rate": 0.91,
  "average_duration_ms": 5432,
  "min_duration_ms": 100,
  "max_duration_ms": 87500,
  "throughput_per_minute": 5.4,
  "error_types": {
    "timeout": 7,
    "validation_error": 3,
    "resource_error": 2
  }
}
```

**Status Code:** `200 OK`

**Key Metrics:**
- `success_rate` - Percentage of successful executions
- `throughput_per_minute` - Execution rate for capacity planning
- `error_types` - Error breakdown for troubleshooting

---

#### 6. Clear All Data

**DELETE** `/api/clear`

Clears all recorded events and traces from the dashboard. **Warning:** This operation is permanent.

**Query Parameters:**
- `confirm` (string, required): Must be "yes" to confirm deletion

**Request:**
```bash
curl -X DELETE "http://localhost:8081/api/clear?confirm=yes"
```

**Response:**
```json
{
  "cleared": true,
  "events_deleted": 1524,
  "traces_deleted": 187,
  "timestamp": "2024-02-06T18:45:20Z"
}
```

**Status Code:** `200 OK`

**Error Response (Missing Confirmation):**
```json
{
  "error": "bad_request",
  "message": "Missing required parameter: confirm=yes"
}
```

---

## Authentication

Currently, both APIs do not require authentication. However, you should:

1. Deploy behind a reverse proxy (nginx, Apache) with authentication
2. Use network-level access controls
3. Restrict API access to trusted networks

Future versions may include:
- JWT token validation
- API key authentication
- TLS/SSL encryption

---

## Error Handling

All API errors return JSON error responses with the following structure:

**Error Response Format:**
```json
{
  "error": "error_code",
  "message": "Human-readable error description",
  "details": {
    "field": "additional information"
  },
  "trace_id": "trace_identifier_for_debugging"
}
```

**Common HTTP Status Codes:**
- `200 OK` - Request successful
- `400 Bad Request` - Invalid parameters
- `404 Not Found` - Resource not found
- `500 Internal Server Error` - Server error

**Error Codes:**
- `bad_request` - Invalid request parameters
- `not_found` - Resource not found
- `internal_server_error` - Server error occurred
- `invalid_parameter` - Invalid parameter value
- `timeout` - Request timeout

---

## Examples

### Example 1: Monitor System Status

```bash
#!/bin/bash

# Check CRE Master status every 5 seconds
while true; do
  STATUS=$(curl -s http://localhost:4142/status.json)

  IDLE=$(echo "$STATUS" | jq '.idle_count')
  BUSY=$(echo "$STATUS" | jq '.busy_count')
  QUEUED=$(echo "$STATUS" | jq '.queue_length')

  echo "Idle Workers: $IDLE | Busy Workers: $BUSY | Queued: $QUEUED"

  sleep 5
done
```

### Example 2: Export Cache Statistics

```bash
#!/bin/bash

HISTORY=$(curl -s http://localhost:4142/history.json)

echo "Cache Statistics:"
echo "  Entries: $(echo "$HISTORY" | jq '.cache_entries')"
echo "  Hit Rate: $(echo "$HISTORY" | jq '.hit_rate * 100 | round')%"
echo "  Size: $(echo "$HISTORY" | jq '.cache_size_bytes / 1024 / 1024 | round')MB"
```

### Example 3: Get Recent Workflow Executions

```bash
#!/bin/bash

# Get recent completed traces
curl -s "http://localhost:8081/api/traces?status=completed&limit=10" | jq '
  .traces[] |
  {
    id: .trace_id,
    workflow: .workflow_id,
    duration_s: (.duration_ms / 1000),
    events: .event_count
  }
'
```

### Example 4: Monitor Workflow Success Rate

```bash
#!/bin/bash

STATS=$(curl -s "http://localhost:8081/api/stats?time_window=24h")

SUCCESS_RATE=$(echo "$STATS" | jq '.success_rate * 100')
FAILED=$(echo "$STATS" | jq '.failed')
TOTAL=$(echo "$STATS" | jq '.total_executions')

echo "24h Workflow Statistics:"
echo "  Success Rate: ${SUCCESS_RATE}%"
echo "  Failed: $FAILED of $TOTAL"

if (( $(echo "$SUCCESS_RATE < 95" | bc -l) )); then
  echo "WARNING: Success rate below 95%"
fi
```

---

## Rate Limiting

Currently, there is no rate limiting enforced. However, in production:
- Implement rate limiting at the reverse proxy level
- Recommended limits: 1000 requests/minute per IP

---

## Pagination

Endpoints supporting pagination use:
- `limit` - Maximum items to return
- `offset` - Number of items to skip
- Responses include `total` for result count

**Example:**
```bash
# Get items 50-99
curl "http://localhost:8081/api/events?limit=50&offset=50"

# Get next page
curl "http://localhost:8081/api/events?limit=50&offset=100"
```

---

## Best Practices

1. **Caching:** Use ETags and HTTP caching headers to reduce API calls
2. **Polling:** Use reasonable intervals (5-10 seconds minimum)
3. **Error Handling:** Implement exponential backoff for retries
4. **Monitoring:** Set up alerts for high error rates
5. **Logging:** Log API requests for troubleshooting
6. **Security:** Always use HTTPS in production

---

## Support

For issues or questions:
- GitHub: https://github.com/ruvnet/claude-flow
- Email: support@cuneiform-lang.org
