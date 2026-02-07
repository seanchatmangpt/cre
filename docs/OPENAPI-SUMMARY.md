# OpenAPI Documentation & SDK Summary

## Overview

This document provides a complete overview of the OpenAPI specifications, API reference documentation, and auto-generated client SDKs for the Cuneiform Runtime Environment (CRE).

## Deliverables

### 1. OpenAPI Specifications

Located in `/docs/`:

#### a. `openapi-cre-master.yaml`
- Complete OpenAPI 3.0 specification for CRE Master API
- **Endpoints:**
  - `GET /` - Get system status
  - `GET /status.json` - Get system status (alias)
  - `GET /history.json` - Get execution history and cache
- **Port:** 4142 (default)
- **Schemas:** CREStatus, BusyWorker, PendingApplication, CREHistory, CacheEntry, Error

#### b. `openapi-yawl-dashboard.yaml`
- Complete OpenAPI 3.0 specification for YAWL Dashboard API
- **Endpoints:**
  - `GET /` - Dashboard HTML interface
  - `GET /api/events` - List workflow events
  - `GET /api/events/{traceId}` - Get events for specific trace
  - `GET /api/traces` - List all traces
  - `GET /api/stats` - Get statistics
  - `DELETE /api/clear` - Clear all data
- **Port:** 8081 (default)
- **Schemas:** Event, EventList, TraceEvents, Trace, TraceList, Statistics, ClearResponse, Error

**Usage:**
```bash
# View in Swagger UI
swagger-ui openapi-cre-master.yaml
swagger-ui openapi-yawl-dashboard.yaml

# Validate specs
openapi-generator-cli validate -i openapi-cre-master.yaml
```

---

### 2. API Reference Documentation

Located in `/docs/API-REFERENCE.md`

**Contents:**
- Overview and base URLs
- Detailed endpoint documentation with examples
- Request/response examples for each endpoint
- Error handling information
- Rate limiting guidelines
- Pagination details
- Best practices
- Bash script examples for common tasks

**Quick Links:**
- CRE Master API endpoints: GET /, GET /status.json, GET /history.json
- YAWL Dashboard API endpoints: All event, trace, and stats endpoints
- Error codes and status codes
- Authentication and security notes

---

### 3. Client SDKs

Located in `/sdks/`:

#### a. Python SDK (`python_sdk.py`)

**Features:**
- Full-featured HTTP client
- Exception hierarchy (APIException, BadRequest, NotFound, ServerError)
- Data models with from_dict conversion
- Context manager support
- Convenience functions

**Classes:**
- `CREMasterClient` - Main API client
- `WorkerStatus` - Worker status model
- `ApplicationStatus` - Application status model
- `CacheEntry` - Cache entry model

**Methods:**
- `get_status()` - Get CRE status
- `get_history()` - Get execution history

**Example:**
```python
from python_sdk import CREMasterClient

with CREMasterClient() as client:
    status = client.get_status()
    print(f"Idle: {status['idle_count']}, Busy: {status['busy_count']}")
```

**Dependencies:** `requests`

#### b. Go SDK (`go_sdk.go`)

**Features:**
- Context-aware HTTP client
- Structured data types
- Error handling with APIError
- Status monitoring with channels
- Type-safe responses

**Types:**
- `Client` - API client
- `CREStatus` - Status response
- `CREHistory` - History response
- `BusyWorker`, `PendingApp`, `CacheEntry` - Models

**Methods:**
- `GetStatus(ctx)` - Get CRE status
- `GetHistory(ctx)` - Get execution history
- `NewStatusMonitor(interval)` - Create status monitor

**Example:**
```go
client := creapi.NewClient("http://localhost:4142")
status, err := client.GetStatus(context.Background())
if err != nil {
    log.Fatal(err)
}
fmt.Printf("Idle: %d, Busy: %d\n", status.IdleCount, status.BusyCount)
```

**Package:** `creapi` (go_sdk.go)

#### c. Node.js SDK (`nodejs_sdk.js`)

**Features:**
- Promise-based async API
- Automatic JSON parsing
- Flexible parameter handling
- Built-in monitoring support
- YAWL Dashboard integration

**Classes:**
- `CREClient` - CRE Master API client
- `YAWLDashboardClient` - YAWL Dashboard client
- `APIError` - Error class

**CREClient Methods:**
- `getStatus()` - Get CRE status
- `getHistory()` - Get execution history
- `monitorStatus(interval, callback)` - Monitor with polling

**YAWLDashboardClient Methods:**
- `listEvents(options)` - Get workflow events
- `getTraceEvents(traceId)` - Get trace events
- `listTraces(options)` - Get traces
- `getStatistics(options)` - Get stats
- `clearData()` - Clear data

**Example:**
```javascript
const CREClient = require('./nodejs_sdk');

const client = new CREClient('http://localhost:4142');
const status = await client.getStatus();
console.log(`Idle: ${status.idle_count}, Busy: ${status.busy_count}`);
```

**Dependencies:** `node-fetch`

---

### 4. SDK Documentation

Located in `/sdks/README.md` and `/docs/SDK-GUIDE.md`

**SDK README (`/sdks/README.md`):**
- Quick start for each language
- API reference for all methods
- Error handling examples
- Installation instructions
- Configuration options
- Advanced usage patterns
- Best practices

**SDK Guide (`/docs/SDK-GUIDE.md`):**
- Detailed implementation guide
- Common patterns (polling, retries, health checks)
- Language-specific best practices
- Performance optimization tips
- Error handling strategies
- Circuit breaker pattern
- Connection pooling

---

### 5. SDK Generator Script

Located in `/scripts/generate-sdks.py`

**Purpose:** Auto-generate client SDKs from OpenAPI specifications

**Features:**
- Parses OpenAPI YAML files
- Generates Python, Go, and Node.js SDKs
- Creates data models from schemas
- Generates HTTP request methods
- Configurable output directory

**Usage:**
```bash
python3 scripts/generate-sdks.py \
  --output-dir ./sdks \
  --api-specs ./docs/openapi-cre-master.yaml ./docs/openapi-yawl-dashboard.yaml
```

**Output Files:**
- `cre_master_api_client.py` (Python)
- `cre_master_api_client.go` (Go)
- `cre_master_api_client.js` (Node.js)
- Similar for YAWL Dashboard API

---

## File Structure

```
/home/user/cre/
├── docs/
│   ├── API-REFERENCE.md              # Complete API documentation
│   ├── OPENAPI-SUMMARY.md             # This file
│   ├── SDK-GUIDE.md                   # SDK implementation guide
│   ├── openapi-cre-master.yaml        # OpenAPI spec for CRE Master
│   └── openapi-yawl-dashboard.yaml    # OpenAPI spec for YAWL Dashboard
├── sdks/
│   ├── README.md                      # SDK quick start guide
│   ├── python_sdk.py                  # Python client SDK
│   ├── go_sdk.go                      # Go client SDK
│   └── nodejs_sdk.js                  # Node.js client SDK
└── scripts/
    └── generate-sdks.py               # SDK generation tool
```

---

## Quick Start

### 1. View OpenAPI Specs

Use any OpenAPI viewer:
- Swagger UI: https://swagger.io/tools/swagger-ui/
- ReDoc: https://redoc.ly/
- Local tools: `swagger-cli`, `redoc-cli`

```bash
# Using swagger-ui (requires installation)
swagger-ui ./docs/openapi-cre-master.yaml
```

### 2. Read API Documentation

```bash
cat ./docs/API-REFERENCE.md
```

### 3. Use Python SDK

```bash
# Install dependencies
pip install requests

# Use in your code
python3 << 'EOF'
from sdks.python_sdk import CREMasterClient

client = CREMasterClient()
status = client.get_status()
print(f"Status: {status}")
EOF
```

### 4. Use Go SDK

```bash
# Copy SDK to your project
cp ./sdks/go_sdk.go ./myproject/

# Use in your code
# See /sdks/README.md for examples
```

### 5. Use Node.js SDK

```bash
# Install dependencies
npm install node-fetch

# Use in your code
node << 'EOF'
const CREClient = require('./sdks/nodejs_sdk');

(async () => {
    const client = new CREClient();
    const status = await client.getStatus();
    console.log(status);
})();
EOF
```

---

## API Endpoints Summary

### CRE Master API (Port 4142)

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/` | Get system status |
| GET | `/status.json` | Get system status (alias) |
| GET | `/history.json` | Get execution history |

### YAWL Dashboard API (Port 8081)

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/` | Dashboard interface |
| GET | `/api/events` | List events |
| GET | `/api/events/{traceId}` | Get trace events |
| GET | `/api/traces` | List traces |
| GET | `/api/stats` | Get statistics |
| DELETE | `/api/clear` | Clear data |

---

## Common Usage Examples

### Monitor CRE Status (Python)

```python
from sdks.python_sdk import CREMasterClient
import time

client = CREMasterClient()
while True:
    status = client.get_status()
    print(f"Workers: {status['idle_count']} idle, {status['busy_count']} busy")
    time.sleep(5)
```

### Check Cache Efficiency (Go)

```go
client := creapi.NewClient("http://localhost:4142")
history, _ := client.GetHistory(context.Background())
fmt.Printf("Cache hit rate: %.1f%%\n", history.HitRate*100)
```

### Monitor Workflow Execution (Node.js)

```javascript
const { YAWLDashboardClient } = require('./sdks/nodejs_sdk');

const client = new YAWLDashboardClient();
const stats = await client.getStatistics({ time_window: '24h' });
console.log(`Success rate: ${(stats.success_rate * 100).toFixed(1)}%`);
```

---

## Integration Patterns

### 1. Monitoring Dashboard

Create a web dashboard that:
1. Polls CRE status API every 5 seconds
2. Displays worker status and queue length
3. Shows cache efficiency metrics
4. Integrates YAWL Dashboard for detailed traces

### 2. Alerting System

Create alerts when:
1. Queue length exceeds threshold
2. All workers are busy
3. No workers are available
4. Cache hit rate drops below threshold

### 3. Performance Analysis

Collect and analyze:
1. Historical status data
2. Cache statistics trends
3. Execution time metrics
4. Error rates and types

---

## Documentation Files Checklist

- [x] OpenAPI 3.0 specifications (2 files)
- [x] API reference documentation with examples
- [x] Python client SDK with full features
- [x] Go client SDK with context support
- [x] Node.js client SDK with async/await
- [x] SDK generator script (Python)
- [x] SDK quick start guide
- [x] SDK implementation guide
- [x] Common patterns and best practices
- [x] Error handling guidelines

---

## Next Steps

1. **Review OpenAPI Specs:** Open the YAML files in an OpenAPI viewer
2. **Read API Documentation:** Review API-REFERENCE.md for detailed endpoint info
3. **Choose Your Language:** Select Python, Go, or Node.js SDK
4. **Follow Quick Start:** Check /sdks/README.md for setup instructions
5. **Implement Integration:** Build your application using the SDK
6. **Deploy:** Use SDKs in production with proper error handling

---

## Support

For issues or questions:
- GitHub: https://github.com/ruvnet/claude-flow
- Documentation: ./API-REFERENCE.md
- SDK Guide: ./SDK-GUIDE.md
- SDK README: ../sdks/README.md

---

## Version Information

- **CRE Master API:** v0.2.0
- **YAWL Dashboard API:** v1.0.0
- **OpenAPI Version:** 3.0.0
- **Last Updated:** 2024-02-06

---

## License

Apache License 2.0 - See LICENSE file in repository root

All OpenAPI specifications, documentation, and SDKs are provided under the same license as the CRE project.
