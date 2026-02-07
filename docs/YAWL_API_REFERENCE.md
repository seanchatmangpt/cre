# YAWL HTTP API Reference

## Overview

This document provides the complete HTTP API reference for the YAWL workflow engine. The API follows OpenAPI 3.0 specifications and supports both RESTful operations and JSON-RPC style requests.

## Base URLs

```
http://localhost:8080/api/v1
```

## Authentication

Currently, the YAWL API supports basic authentication and API key authentication. Include credentials in request headers:

```
Authorization: Bearer <api_key>
```

---

## Core Endpoints

### Status and Health

#### GET /status

Get the current status of the YAWL engine.

**Request:**
```bash
curl -X GET http://localhost:8080/status
```

**Response (200 OK):**
```json
{
  "status": "running",
  "version": "0.2.0",
  "uptime_seconds": 3600,
  "active_workflows": 5,
  "completed_workflows": 42,
  "failed_workflows": 2,
  "timestamp": 1640000000000
}
```

**Error Responses:**
- `500 Internal Server Error` - Server error occurred

---

### Workflow Management

#### POST /workflows

Create a new workflow instance.

**Request Headers:**
```
Content-Type: application/json
```

**Request Body:**
```json
{
  "workflow_id": "order-processing-001",
  "workflow_type": "order_processing",
  "start_task": "validate_order",
  "input_data": {
    "order_id": "ORD-12345",
    "customer_id": "CUST-789",
    "items": [
      {
        "product_id": "PROD-001",
        "quantity": 2,
        "price": 29.99
      }
    ]
  }
}
```

**Response (201 Created):**
```json
{
  "workflow_id": "order-processing-001",
  "case_id": "case-001",
  "status": "running",
  "created_at": 1640000000000,
  "started_at": 1640000000100
}
```

**Error Responses:**
- `400 Bad Request` - Invalid workflow definition
- `409 Conflict` - Workflow ID already exists
- `500 Internal Server Error` - Server error

---

#### GET /workflows/:workflow_id

Retrieve workflow status and information.

**Request:**
```bash
curl -X GET http://localhost:8080/api/v1/workflows/order-processing-001
```

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "case_id": "case-001",
  "status": "running",
  "current_task": "check_inventory",
  "created_at": 1640000000000,
  "started_at": 1640000000100,
  "last_updated": 1640000000500,
  "input_data": {
    "order_id": "ORD-12345",
    "customer_id": "CUST-789"
  },
  "output_data": null,
  "completion_percentage": 35
}
```

**Error Responses:**
- `404 Not Found` - Workflow not found
- `500 Internal Server Error` - Server error

---

#### GET /workflows

List all workflows with optional filtering.

**Query Parameters:**
- `status` (optional) - Filter by status: `running`, `completed`, `failed`, `suspended`
- `limit` (optional) - Maximum number of results (default: 100)
- `offset` (optional) - Pagination offset (default: 0)
- `sort_by` (optional) - Sort field: `created_at`, `updated_at`, `status`
- `sort_order` (optional) - Sort order: `asc`, `desc`

**Request:**
```bash
curl -X GET "http://localhost:8080/api/v1/workflows?status=running&limit=10&sort_by=created_at&sort_order=desc"
```

**Response (200 OK):**
```json
{
  "total": 47,
  "limit": 10,
  "offset": 0,
  "workflows": [
    {
      "workflow_id": "order-processing-001",
      "case_id": "case-001",
      "status": "running",
      "created_at": 1640000000000,
      "progress": 35
    }
  ]
}
```

---

#### POST /workflows/:workflow_id/suspend

Suspend a running workflow.

**Request:**
```bash
curl -X POST http://localhost:8080/api/v1/workflows/order-processing-001/suspend
```

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "status": "suspended",
  "suspended_at": 1640000000600,
  "current_task": "check_inventory"
}
```

---

#### POST /workflows/:workflow_id/resume

Resume a suspended workflow.

**Request:**
```bash
curl -X POST http://localhost:8080/api/v1/workflows/order-processing-001/resume
```

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "status": "running",
  "resumed_at": 1640000000700
}
```

---

#### POST /workflows/:workflow_id/cancel

Cancel a running workflow.

**Request Body:**
```json
{
  "reason": "Order cancelled by customer"
}
```

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "status": "cancelled",
  "cancelled_at": 1640000000800,
  "cancel_reason": "Order cancelled by customer"
}
```

---

### Task Management

#### GET /workflows/:workflow_id/tasks

Get all tasks in a workflow.

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "tasks": [
    {
      "task_id": "validate_order",
      "type": "atomic",
      "status": "completed",
      "started_at": 1640000000100,
      "completed_at": 1640000000200,
      "duration_ms": 100,
      "result": {
        "valid": true,
        "warnings": []
      }
    },
    {
      "task_id": "check_inventory",
      "type": "atomic",
      "status": "running",
      "started_at": 1640000000300,
      "current_progress": 60
    }
  ]
}
```

---

#### GET /workflows/:workflow_id/tasks/:task_id

Get details of a specific task.

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "task_id": "check_inventory",
  "type": "atomic",
  "status": "running",
  "started_at": 1640000000300,
  "input": {
    "order_id": "ORD-12345",
    "items": [
      {"product_id": "PROD-001", "quantity": 2}
    ]
  },
  "output": null,
  "error": null,
  "retries": 0,
  "timeout_ms": 30000
}
```

---

#### POST /workflows/:workflow_id/tasks/:task_id/complete

Complete a task with output data.

**Request Body:**
```json
{
  "result": {
    "available": true,
    "stock_level": 15,
    "backorder_items": []
  }
}
```

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "task_id": "check_inventory",
  "status": "completed",
  "completed_at": 1640000001000
}
```

---

### Patterns and Pattern Templates

#### GET /patterns

List all available YAWL workflow patterns.

**Response (200 OK):**
```json
{
  "patterns": [
    {
      "pattern_id": "wcp-01",
      "name": "Sequence",
      "category": "Basic Control Flow",
      "description": "Execute tasks in a strict sequential order"
    },
    {
      "pattern_id": "wcp-02",
      "name": "Parallel Split",
      "category": "Basic Control Flow",
      "description": "Split workflow execution into multiple parallel branches"
    },
    {
      "pattern_id": "wcp-11",
      "name": "Implicit Termination",
      "category": "Multiple Instance",
      "description": "Terminate subprocess when no work remains"
    }
  ]
}
```

---

#### GET /patterns/:pattern_id

Get detailed information about a specific pattern.

**Response (200 OK):**
```json
{
  "pattern_id": "wcp-02",
  "name": "Parallel Split",
  "category": "Basic Control Flow",
  "description": "Split workflow execution into multiple parallel branches",
  "implementation": "parallel_split",
  "parameters": [
    {
      "name": "split_task_id",
      "type": "element_id",
      "required": true,
      "description": "The task that performs the split"
    },
    {
      "name": "branch_task_ids",
      "type": "list(element_id)",
      "required": true,
      "description": "List of task IDs for parallel branches"
    }
  ],
  "petri_net_places": ["p_start", "p_branch1", "p_branch2", "p_branch3", "p_merged"],
  "petri_net_transitions": ["t_split", "t_execute1", "t_execute2", "t_execute3", "t_join"],
  "example": {
    "erlang": "Pattern = cre_yawl:parallel_split()"
  }
}
```

---

### Telemetry and Monitoring

#### GET /telemetry/metrics

Get current metrics.

**Query Parameters:**
- `pattern_type` (optional) - Filter metrics by pattern type
- `time_window` (optional) - Time window in seconds (default: 3600)

**Response (200 OK):**
```json
{
  "timestamp": 1640000000000,
  "time_window_seconds": 3600,
  "metrics": {
    "workflows_started": 47,
    "workflows_completed": 42,
    "workflows_failed": 2,
    "workflows_suspended": 3,
    "average_execution_time_ms": 2500,
    "pattern_metrics": {
      "wcp-01": {
        "executions": 15,
        "avg_duration_ms": 500,
        "success_rate": 0.95
      },
      "wcp-11": {
        "executions": 8,
        "avg_duration_ms": 1200,
        "success_rate": 0.98
      }
    }
  }
}
```

---

#### GET /telemetry/health

Get system health status.

**Response (200 OK):**
```json
{
  "status": "healthy",
  "timestamp": 1640000000000,
  "components": {
    "engine": "healthy",
    "memory": "healthy",
    "database": "healthy",
    "network": "healthy"
  },
  "metrics": {
    "memory_usage_percent": 45,
    "active_workflows": 5,
    "queue_depth": 12,
    "error_rate": 0.02
  }
}
```

---

#### GET /telemetry/execution/:workflow_id

Get execution telemetry for a specific workflow.

**Response (200 OK):**
```json
{
  "workflow_id": "order-processing-001",
  "case_id": "case-001",
  "trace_id": "abc123def456",
  "execution_timeline": [
    {
      "timestamp": 1640000000100,
      "event": "task_started",
      "task_id": "validate_order",
      "details": {}
    },
    {
      "timestamp": 1640000000200,
      "event": "task_completed",
      "task_id": "validate_order",
      "duration_ms": 100
    }
  ],
  "execution_graph": {
    "nodes": ["validate_order", "check_inventory", "process_payment", "ship_order"],
    "edges": [
      {"from": "validate_order", "to": "check_inventory"},
      {"from": "check_inventory", "to": "process_payment"},
      {"from": "process_payment", "to": "ship_order"}
    ]
  }
}
```

---

### Error Handling

#### Common Error Response Format

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Workflow validation failed",
    "details": [
      {
        "field": "workflow_definition",
        "message": "Missing required field: start_task"
      }
    ],
    "timestamp": 1640000000000,
    "trace_id": "abc123def456"
  }
}
```

#### Error Codes

| Code | HTTP Status | Description |
|------|------------|-------------|
| `VALIDATION_ERROR` | 400 | Invalid request parameters or workflow definition |
| `NOT_FOUND` | 404 | Workflow or resource not found |
| `CONFLICT` | 409 | Resource already exists or operation conflicts with current state |
| `TIMEOUT` | 408 | Request timeout or workflow execution timeout |
| `INTERNAL_ERROR` | 500 | Internal server error |
| `SERVICE_UNAVAILABLE` | 503 | Service temporarily unavailable |

---

## Data Types and Schemas

### Workflow

```json
{
  "workflow_id": "string (uuid or custom ID)",
  "case_id": "string (auto-generated case ID)",
  "status": "string (running|completed|failed|suspended|cancelled)",
  "input_data": "object",
  "output_data": "object | null",
  "error": "object | null",
  "created_at": "integer (timestamp ms)",
  "started_at": "integer (timestamp ms)",
  "completed_at": "integer (timestamp ms) | null"
}
```

### Task

```json
{
  "task_id": "string",
  "type": "string (atomic|composite)",
  "status": "string (ready|running|completed|failed|suspended)",
  "input": "object",
  "output": "object | null",
  "started_at": "integer (timestamp ms)",
  "completed_at": "integer (timestamp ms) | null",
  "error": "object | null",
  "retries": "integer",
  "timeout_ms": "integer"
}
```

### Pattern

```json
{
  "pattern_id": "string",
  "name": "string",
  "category": "string",
  "description": "string",
  "parameters": [
    {
      "name": "string",
      "type": "string",
      "required": "boolean",
      "description": "string"
    }
  ]
}
```

---

## Rate Limiting

The YAWL API implements rate limiting to prevent abuse:

- **Default limit**: 1000 requests per minute per API key
- **Rate limit headers**:
  ```
  X-RateLimit-Limit: 1000
  X-RateLimit-Remaining: 999
  X-RateLimit-Reset: 1640000060000
  ```

When rate limit is exceeded, the API returns:
```
HTTP/1.1 429 Too Many Requests
```

---

## Pagination

List endpoints support pagination using query parameters:

- `limit` - Maximum number of results (default: 100, max: 1000)
- `offset` - Pagination offset (default: 0)

Response includes:
```json
{
  "total": 1000,
  "limit": 100,
  "offset": 0,
  "items": [...]
}
```

---

## Webhooks

Configure webhooks to receive notifications about workflow events:

#### POST /webhooks

Register a webhook.

**Request Body:**
```json
{
  "url": "https://example.com/callback",
  "events": [
    "workflow_started",
    "workflow_completed",
    "workflow_failed",
    "task_completed",
    "task_failed"
  ],
  "headers": {
    "Authorization": "Bearer webhook-secret"
  }
}
```

**Response (201 Created):**
```json
{
  "webhook_id": "webhook-001",
  "url": "https://example.com/callback",
  "events": [
    "workflow_started",
    "workflow_completed",
    "workflow_failed",
    "task_completed",
    "task_failed"
  ],
  "created_at": 1640000000000
}
```

---

## Examples

### Example 1: Complete Order Processing Workflow

```bash
# Create workflow
curl -X POST http://localhost:8080/api/v1/workflows \
  -H "Content-Type: application/json" \
  -d '{
    "workflow_id": "order-001",
    "workflow_type": "order_processing",
    "start_task": "validate_order",
    "input_data": {
      "order_id": "ORD-12345",
      "customer_id": "CUST-789",
      "items": [{"product_id": "PROD-001", "quantity": 2}]
    }
  }'

# Check status
curl -X GET http://localhost:8080/api/v1/workflows/order-001

# Get tasks
curl -X GET http://localhost:8080/api/v1/workflows/order-001/tasks

# Complete a task
curl -X POST http://localhost:8080/api/v1/workflows/order-001/tasks/check_inventory/complete \
  -H "Content-Type: application/json" \
  -d '{
    "result": {
      "available": true,
      "stock_level": 15
    }
  }'
```

### Example 2: Workflow with Parallel Branches

```bash
curl -X POST http://localhost:8080/api/v1/workflows \
  -H "Content-Type: application/json" \
  -d '{
    "workflow_id": "parallel-001",
    "workflow_type": "parallel_processing",
    "start_task": "initiate",
    "pattern": "parallel_split",
    "pattern_config": {
      "split_task_id": "initiate",
      "branch_task_ids": ["process_a", "process_b", "process_c"]
    },
    "input_data": {
      "data": "process me"
    }
  }'
```

---

## Best Practices

1. **Always include tracing headers** - Include `X-Request-ID` or `X-Trace-ID` headers for debugging
2. **Implement exponential backoff** - Retry failed requests with exponential backoff
3. **Use webhooks for notifications** - Don't poll for status updates frequently
4. **Handle timeouts gracefully** - Set appropriate timeouts and handle timeout errors
5. **Validate input data** - Validate workflow input before creating workflows
6. **Monitor metrics** - Regularly check telemetry metrics to identify issues
7. **Implement error handling** - Handle all possible error responses

---

## Related Documentation

- [YAWL Patterns Reference](/docs/YAWL_PATTERNS_REFERENCE.md)
- [YAWL Integration Guide](/docs/YAWL_INTEGRATION_GUIDE.md)
- [YAWL Telemetry](/docs/YAWL_TELEMETRY.md)
