# CRE Workflow Management REST API

This document describes the REST API endpoints for managing CRE workflow instances.

## Overview

The CRE HTTP Handler (`cre_http_handler.erl`) provides a comprehensive REST API for creating, monitoring, and controlling YAWL workflow instances. All endpoints use JSON for request and response bodies.

## Starting the API Server

```erlang
%% Start on default port 8080
{ok, Port} = cre_http_handler:start_listener().

%% Start on custom port
{ok, Port} = cre_http_handler:start_listener(9090).

%% Stop the server
ok = cre_http_handler:stop_listener().
```

## API Endpoints

### 1. Create Workflow

Create a new workflow instance.

**Endpoint:** `POST /workflows`

**Request Body:**
```json
{
  "workflow_module": "my_workflow",
  "case_id": "case-001",
  "init_args": {
    "key": "value"
  },
  "options": []
}
```

**Parameters:**
- `workflow_module` (string, required) - The name of the Erlang module implementing the workflow
- `case_id` (string, required) - Unique identifier for this workflow instance
- `init_args` (object, optional) - Arguments passed to the workflow's init/1 function
- `options` (array, optional) - gen_yawl options (e.g., `[{fire_timeout, 10000}]`)

**Success Response:** `201 Created`
```json
{
  "status": "created",
  "case_id": "case-001",
  "pid": "<0.123.0>",
  "message": "Workflow created successfully"
}
```

**Error Response:** `409 Conflict` (workflow already exists)
```json
{
  "status": "error",
  "message": "Failed to create workflow",
  "details": "Workflow with this case_id already exists"
}
```

**Error Response:** `400 Bad Request` (invalid request)
```json
{
  "status": "error",
  "message": "Invalid request",
  "details": "Missing required field: workflow_module"
}
```

**cURL Example:**
```bash
curl -X POST http://localhost:8080/workflows \
  -H "Content-Type: application/json" \
  -d '{
    "workflow_module": "my_workflow",
    "case_id": "case-001",
    "init_args": {},
    "options": []
  }'
```

---

### 2. Get Workflow Status

Retrieve the current status of a workflow instance.

**Endpoint:** `GET /workflows/:id`

**Success Response:** `200 OK`
```json
{
  "status": "running",
  "case_id": "case-001",
  "pid": "<0.123.0>",
  "marking": {
    "p1": [],
    "p2": ["token"]
  },
  "usr_info": {
    "state": "active"
  }
}
```

**Parameters:**
- `marking` - Current Petri net marking (tokens in places)
- `usr_info` - User-defined state information

**Error Response:** `404 Not Found`
```json
{
  "status": "error",
  "message": "Workflow not found",
  "details": "case-001"
}
```

**cURL Example:**
```bash
curl http://localhost:8080/workflows/case-001
```

---

### 3. Start Workflow

Explicitly start a workflow execution (workflows typically start automatically on creation).

**Endpoint:** `POST /workflows/:id/start`

**Success Response:** `200 OK`
```json
{
  "status": "started",
  "case_id": "case-001",
  "message": "Workflow started successfully"
}
```

**Error Response:** `404 Not Found`
```json
{
  "status": "error",
  "message": "Workflow not found",
  "details": "case-001"
}
```

**cURL Example:**
```bash
curl -X POST http://localhost:8080/workflows/case-001/start
```

---

### 4. Stop Workflow

Stop and terminate a workflow instance.

**Endpoint:** `POST /workflows/:id/stop`

**Success Response:** `200 OK`
```json
{
  "status": "stopped",
  "case_id": "case-001",
  "message": "Workflow stopped successfully"
}
```

**Error Response:** `404 Not Found`
```json
{
  "status": "error",
  "message": "Workflow not found",
  "details": "case-001"
}
```

**Error Response:** `500 Internal Server Error`
```json
{
  "status": "error",
  "message": "Failed to stop workflow",
  "details": "not_found"
}
```

**cURL Example:**
```bash
curl -X POST http://localhost:8080/workflows/case-001/stop
```

---

### 5. List Workflows

List all active workflow instances.

**Endpoint:** `GET /workflows`

**Success Response:** `200 OK`
```json
{
  "status": "ok",
  "workflows": [
    {
      "case_id": "case-001",
      "pid": "<0.123.0>"
    },
    {
      "case_id": "case-002",
      "pid": "<0.124.0>"
    }
  ],
  "count": 2
}
```

**cURL Example:**
```bash
curl http://localhost:8080/workflows
```

---

## HTTP Status Codes

| Code | Meaning | Usage |
|------|---------|-------|
| 200 | OK | Successful GET, POST (non-creation) |
| 201 | Created | Successful workflow creation |
| 400 | Bad Request | Invalid JSON, missing required fields |
| 404 | Not Found | Workflow not found |
| 405 | Method Not Allowed | Unsupported HTTP method |
| 409 | Conflict | Workflow with case_id already exists |
| 500 | Internal Server Error | Server-side error |

---

## Error Handling

All error responses follow this structure:

```json
{
  "status": "error",
  "message": "Human-readable error message",
  "details": "Additional error details"
}
```

The `details` field can contain:
- String error messages
- Erlang terms (converted to JSON-safe format)
- Nested objects with additional context

---

## Integration Examples

### Erlang Integration

```erlang
%% Start the API server
{ok, Port} = cre_http_handler:start_listener(8080),

%% Use the direct API (without HTTP)
Request = #{
    <<"workflow_module">> => <<"my_workflow">>,
    <<"case_id">> => <<"test-001">>,
    <<"init_args">> => #{},
    <<"options">> => []
},

{ok, Response} = cre_http_handler:handle_request(
    <<"POST">>,
    [],
    Request
),

io:format("Created workflow: ~p~n", [Response]).
```

### Python Integration

```python
import requests
import json

# Create workflow
response = requests.post(
    'http://localhost:8080/workflows',
    json={
        'workflow_module': 'my_workflow',
        'case_id': 'case-001',
        'init_args': {},
        'options': []
    }
)

if response.status_code == 201:
    data = response.json()
    print(f"Created workflow: {data['case_id']}")

    # Get workflow status
    status = requests.get(f"http://localhost:8080/workflows/{data['case_id']}")
    print(f"Status: {status.json()}")

    # Stop workflow
    stop = requests.post(f"http://localhost:8080/workflows/{data['case_id']}/stop")
    print(f"Stopped: {stop.json()}")
```

### JavaScript Integration

```javascript
// Create workflow
const createWorkflow = async () => {
    const response = await fetch('http://localhost:8080/workflows', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({
            workflow_module: 'my_workflow',
            case_id: 'case-001',
            init_args: {},
            options: []
        })
    });

    const data = await response.json();
    console.log('Created:', data);

    // Get status
    const status = await fetch(`http://localhost:8080/workflows/${data.case_id}`);
    console.log('Status:', await status.json());
};

// List all workflows
const listWorkflows = async () => {
    const response = await fetch('http://localhost:8080/workflows');
    const data = await response.json();
    console.log(`Found ${data.count} workflows:`, data.workflows);
};
```

---

## Architecture

### Components

1. **cre_http_handler** - Cowboy HTTP handler implementing REST endpoints
2. **yawl_registry** - ETS-based registry mapping case_id → Pid
3. **yawl_workflow_supervisor** - Dynamic supervisor for workflow instances
4. **gen_yawl** - OTP behavior for YAWL workflow execution

### Request Flow

```
HTTP Request
    ↓
cre_http_handler (Cowboy)
    ↓
Route matching (Method + Path)
    ↓
Endpoint handler
    ↓
yawl_registry (lookup/register)
    ↓
yawl_workflow_supervisor (start/stop)
    ↓
gen_yawl process (workflow instance)
    ↓
JSON Response
```

### Workflow Lifecycle

1. **Creation**: POST /workflows
   - Validates request
   - Starts gen_yawl process under supervisor
   - Registers case_id → Pid in registry
   - Returns 201 Created

2. **Execution**: Automatic or POST /workflows/:id/start
   - gen_yawl automatically begins execution
   - Fires enabled transitions
   - Updates marking and user info

3. **Monitoring**: GET /workflows/:id
   - Looks up Pid from registry
   - Queries gen_yawl for marking and usr_info
   - Returns current state

4. **Termination**: POST /workflows/:id/stop
   - Looks up Pid from registry
   - Stops workflow via supervisor
   - Unregisters from registry

---

## Configuration

The API server can be configured via application environment:

```erlang
%% In sys.config
{cre, [
    {http_port, 8080},
    {http_max_connections, 1000},
    {workflow_timeout, 60000}  %% 60 seconds
]}.
```

---

## Security Considerations

**Note:** The current implementation does not include authentication or authorization. For production use, consider adding:

1. **Authentication**: Bearer tokens, API keys, or OAuth2
2. **Authorization**: Role-based access control (RBAC)
3. **Rate Limiting**: Prevent API abuse
4. **TLS/SSL**: Use HTTPS instead of HTTP
5. **Input Validation**: Strict validation of workflow_module names
6. **CORS**: Configure Cross-Origin Resource Sharing policies

---

## Performance

### Benchmarks (Estimated)

- **Create workflow**: ~1-5ms per request
- **Get status**: ~0.5-2ms per request
- **List workflows**: ~1-10ms (depends on number of workflows)
- **Stop workflow**: ~1-3ms per request

### Scaling

- **Concurrent requests**: Cowboy handles 10,000+ concurrent connections
- **Workflow instances**: Limited by BEAM VM process limit (~1M processes)
- **Registry**: ETS table with read_concurrency for fast lookups

---

## Troubleshooting

### Common Issues

**1. Workflow creation fails with "module does not exist"**

Ensure the workflow module is compiled and loaded:
```erlang
code:ensure_loaded(my_workflow).
```

**2. Registry not found**

Ensure the CRE application is started:
```erlang
application:ensure_all_started(cre).
```

**3. Port already in use**

Change the port or stop the conflicting service:
```erlang
cre_http_handler:stop_listener().
cre_http_handler:start_listener(9090).
```

**4. Workflow not found**

Check if the workflow process is alive:
```erlang
yawl_registry:list().
```

---

## Testing

Run the test suite:

```bash
rebar3 eunit --module=cre_http_handler_test
```

### Manual Testing

```bash
# Start CRE application
erl -pa _build/default/lib/*/ebin

# In Erlang shell
application:ensure_all_started(cre).
cre_http_handler:start_listener(8080).

# In another terminal
curl -X POST http://localhost:8080/workflows \
  -H "Content-Type: application/json" \
  -d '{"workflow_module":"my_workflow","case_id":"test-001","init_args":{},"options":[]}'
```

---

## Future Enhancements

- [ ] WebSocket support for real-time workflow events
- [ ] GraphQL API for complex queries
- [ ] Authentication and authorization
- [ ] Workflow templates and versioning
- [ ] Bulk operations (create/stop multiple workflows)
- [ ] Workflow scheduling (delayed start, cron-like)
- [ ] Metrics and monitoring integration (Prometheus)
- [ ] API rate limiting and throttling

---

## References

- [Cowboy HTTP Server](https://ninenines.eu/docs/en/cowboy/2.14/guide/)
- [gen_yawl Behavior](../src/core/gen_yawl.erl)
- [YAWL Registry](../src/yawl/yawl_registry.erl)
- [Workflow Supervisor](../src/app/yawl_workflow_supervisor.erl)
