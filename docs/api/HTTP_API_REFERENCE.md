# CRE HTTP API Reference

**Version**: 0.3.0
**Last Updated**: 2025-02-11
**Status**: Ready for GCP Marketplace

## Table of Contents

1. [Overview](#overview)
2. [Authentication](#authentication)
3. [Health Check Endpoints](#health-check-endpoints)
4. [Workflow Management](#workflow-management)
5. [Case Management](#case-management)
6. [Task and Work Item Management](#task-and-work-item-management)
7. [Pattern Management](#pattern-management)
8. [System Status](#system-status)
9. [Error Handling](#error-handling)
10. [Rate Limiting](#rate-limiting)
11. [Pagination](#pagination)
12. [Request/Response Examples](#requestresponse-examples)

## Overview

The CRE (Common Runtime Environment) HTTP API provides a comprehensive REST interface for workflow management and execution. It enables:

- **Workflow Lifecycle Management**: Create, validate, execute, and monitor YAWL workflows
- **Case Execution**: Launch and control workflow case instances
- **Task Management**: Access work items and task execution results
- **Pattern Support**: Query and compose YAWL control-flow patterns
- **System Monitoring**: Real-time health checks and status reporting

### Base URL

The API is accessible at the configured HTTP endpoint:

```
http://localhost:8080              # Local development
https://api.cre.example.com        # Production
http://cre-master:8080             # Docker/Kubernetes
```

### Content Types

All endpoints support and return:
- **Request**: `application/json`, `application/xml` (where applicable)
- **Response**: `application/json`

### API Version

The current API version is **v1** (implicit in URL structure). Version negotiation via `Accept` header is planned for future releases.

## Authentication

All endpoints except health checks require authentication via one of two mechanisms:

### Basic Authentication

Provide credentials in the `Authorization` header:

```http
Authorization: Basic base64(username:password)
```

Example:
```bash
curl -u "username:password" http://localhost:8080/status.json
```

### Bearer Token (Future)

Coming in a future release for OAuth 2.0 and JWT support:

```http
Authorization: Bearer <token>
```

### No Authentication (Health Checks)

Health check endpoints (`/health`, `/ready`, `/startup`) are unauthenticated and accessible without credentials.

## Health Check Endpoints

Health checks are critical for production deployments and are used by:
- Kubernetes liveness/readiness/startup probes
- Load balancer health checks
- Monitoring systems

### GET /health

**Liveness Probe**: Is the service running?

```http
GET /health
```

**Response** (200 OK - Healthy):
```json
{
  "status": "healthy",
  "timestamp": 1707641400000,
  "subsystems": [
    {
      "name": "beam",
      "status": "healthy",
      "message": "BEAM VM is responsive",
      "details": {
        "uptime_seconds": 3600,
        "process_count": 245,
        "memory_total": 104857600
      }
    },
    {
      "name": "mnesia",
      "status": "healthy",
      "message": "Mnesia cluster is operational",
      "details": {
        "running_nodes": 1,
        "tables": 5
      }
    },
    {
      "name": "cre_master",
      "status": "healthy",
      "message": "CRE master is running"
    }
  ]
}
```

**Response** (503 Service Unavailable - Unhealthy):
```json
{
  "status": "unhealthy",
  "timestamp": 1707641400000,
  "subsystems": [
    {
      "name": "cre_master",
      "status": "unhealthy",
      "message": "CRE master process crashed"
    }
  ]
}
```

### GET /ready

**Readiness Probe**: Is the service ready to accept requests?

Verifies that critical subsystems are operational:
- Mnesia database cluster
- Worker pool availability
- CRE master process
- EPMD connectivity

```http
GET /ready
```

Returns 200 only if the service is fully operational, 503 otherwise.

### GET /startup

**Startup Probe**: Has the service completed initialization?

Used by Kubernetes to determine when to start sending traffic to a new container.

```http
GET /startup
```

Returns:
- **200**: Service has started successfully
- **202**: Service is still starting up
- **503**: Service startup failed

## Workflow Management

### GET /api/yawl/specifications

List all loaded workflow specifications.

```http
GET /api/yawl/specifications?limit=50&offset=0
Authorization: Basic base64(username:password)
```

**Query Parameters**:
- `limit` (optional, default: 50): Maximum results to return
- `offset` (optional, default: 0): Pagination offset

**Response** (200 OK):
```json
{
  "specifications": [
    {
      "id": "spec_001",
      "name": "Payment Processing Workflow",
      "description": "Handles payment validation and processing",
      "version": "1.0.0",
      "created_at": "2025-02-11T10:00:00Z",
      "updated_at": "2025-02-11T11:00:00Z",
      "status": "validated",
      "tasks_count": 12,
      "patterns": ["sequence", "exclusive_choice", "parallel_split"]
    }
  ]
}
```

### POST /api/yawl/specifications

Upload a new workflow specification.

**Status**: Currently returns 501 (Not Implemented)

```http
POST /api/yawl/specifications
Authorization: Basic base64(username:password)
Content-Type: application/json

{
  "name": "Invoice Processing",
  "content": "<?xml version=\"1.0\"...>",
  "format": "yawl"
}
```

### GET /api/yawl/specifications/{specification_id}

Retrieve details of a specific workflow specification.

```http
GET /api/yawl/specifications/spec_001
Authorization: Basic base64(username:password)
```

**Response** (200 OK):
```json
{
  "id": "spec_001",
  "name": "Payment Processing Workflow",
  "description": "Handles payment validation and processing",
  "version": "1.0.0",
  "created_at": "2025-02-11T10:00:00Z",
  "status": "validated",
  "tasks_count": 12,
  "patterns": ["sequence", "exclusive_choice", "parallel_split"]
}
```

**Response** (404 Not Found):
```json
{
  "error": "not_found",
  "message": "Specification spec_001 not found"
}
```

### POST /api/yawl/specifications/{specification_id}/validate

Validate a workflow specification for correctness.

```http
POST /api/yawl/specifications/spec_001/validate
Authorization: Basic base64(username:password)
```

**Response** (200 OK - Valid):
```json
{
  "is_valid": true,
  "errors": [],
  "warnings": [],
  "timestamp": "2025-02-11T10:00:00Z",
  "checks_performed": [
    "syntax",
    "soundness",
    "liveness",
    "boundedness"
  ]
}
```

**Response** (200 OK - Invalid):
```json
{
  "is_valid": false,
  "errors": [
    {
      "code": "syntax_error",
      "message": "Invalid task definition",
      "element": "Task_PaymentProcessing"
    },
    {
      "code": "soundness_error",
      "message": "Unreachable place detected",
      "element": "Place_CompletionGate"
    }
  ],
  "warnings": [
    {
      "code": "unused_element",
      "message": "Task never referenced",
      "element": "Task_Legacy"
    }
  ],
  "timestamp": "2025-02-11T10:00:00Z",
  "checks_performed": ["syntax", "soundness"]
}
```

### POST /api/yawl/specifications/{specification_id}/launch

Launch a new workflow case instance.

**Status**: Currently returns 501 (Not Implemented). Use `/api/yawl/cases` instead.

## Case Management

### GET /api/yawl/cases

List all workflow case instances.

```http
GET /api/yawl/cases?status=running&limit=50&offset=0
Authorization: Basic base64(username:password)
```

**Query Parameters**:
- `status` (optional): Filter by status (running, completed, failed, suspended, cancelled)
- `limit` (optional, default: 50): Maximum results
- `offset` (optional, default: 0): Pagination offset

**Response** (200 OK):
```json
{
  "cases": [
    {
      "id": "case_001",
      "specification_id": "spec_payment",
      "status": "running",
      "created_at": "2025-02-11T10:00:00Z",
      "started_at": "2025-02-11T10:00:15Z",
      "active_work_items": 3,
      "completed_tasks": 5
    },
    {
      "id": "case_002",
      "specification_id": "spec_approval",
      "status": "completed",
      "created_at": "2025-02-11T09:00:00Z",
      "completed_at": "2025-02-11T10:30:00Z",
      "completed_tasks": 8
    }
  ],
  "total": 2,
  "limit": 50,
  "offset": 0
}
```

### GET /api/yawl/cases/{case_id}

Retrieve detailed information about a specific case.

```http
GET /api/yawl/cases/case_001
Authorization: Basic base64(username:password)
```

**Response** (200 OK):
```json
{
  "id": "case_001",
  "specification_id": "spec_payment",
  "status": "running",
  "created_at": "2025-02-11T10:00:00Z",
  "started_at": "2025-02-11T10:00:15Z",
  "active_work_items": 3,
  "completed_tasks": 5,
  "work_items": [
    {
      "id": "wi_001",
      "case_id": "case_001",
      "task_name": "payment_validation",
      "task_id": "t_001",
      "status": "executing",
      "assigned_to": "user_123",
      "created_at": "2025-02-11T10:05:00Z",
      "started_at": "2025-02-11T10:10:00Z",
      "input_data": {
        "amount": 1000,
        "currency": "USD"
      }
    }
  ],
  "task_history": [
    {
      "task_id": "t_001",
      "task_name": "payment_validation",
      "status": "completed",
      "timestamp": "2025-02-11T10:10:00Z",
      "duration_ms": 5000,
      "result": {
        "valid": true
      }
    }
  ],
  "case_data": {
    "amount": 1000,
    "currency": "USD",
    "account": "ACC123456",
    "timestamp": "2025-02-11T10:00:00Z"
  }
}
```

### POST /api/yawl/cases/{case_id}/cancel

Cancel a running workflow case.

```http
POST /api/yawl/cases/case_001/cancel
Authorization: Basic base64(username:password)
Content-Type: application/json

{
  "reason": "User initiated cancellation"
}
```

**Response** (200 OK):
```json
{
  "id": "case_001",
  "specification_id": "spec_payment",
  "status": "cancelled",
  "created_at": "2025-02-11T10:00:00Z",
  "cancelled_at": "2025-02-11T10:15:00Z",
  "completed_tasks": 5
}
```

**Response** (409 Conflict):
```json
{
  "error": "invalid_state_transition",
  "message": "Cannot cancel a case in status: completed"
}
```

### POST /api/yawl/cases/{case_id}/suspend

Suspend a running workflow case.

**Status**: Currently returns 501 (Not Implemented)

```http
POST /api/yawl/cases/case_001/suspend
Authorization: Basic base64(username:password)
```

### POST /api/yawl/cases/{case_id}/resume

Resume a suspended workflow case.

**Status**: Currently returns 501 (Not Implemented)

```http
POST /api/yawl/cases/case_001/resume
Authorization: Basic base64(username:password)
```

## Task and Work Item Management

### GET /api/yawl/worklist/{user_id}

Get work items assigned to a specific user.

```http
GET /api/yawl/worklist/user_123?filter=enabled&limit=50
Authorization: Basic base64(username:password)
```

**Query Parameters**:
- `filter` (optional): Work item state (enabled, fired, started, executing, completed)
- `limit` (optional, default: 50): Maximum results

**Response** (200 OK):
```json
{
  "work_items": [
    {
      "id": "wi_001",
      "case_id": "case_001",
      "task_name": "payment_validation",
      "task_id": "t_001",
      "status": "enabled",
      "assigned_to": "user_123",
      "created_at": "2025-02-11T10:05:00Z",
      "input_data": {
        "amount": 1000,
        "currency": "USD"
      }
    }
  ],
  "total": 1
}
```

## Pattern Management

### GET /patterns

List all available YAWL patterns.

```http
GET /patterns?category=control-flow
Authorization: Basic base64(username:password)
```

**Query Parameters**:
- `category` (optional): Pattern category (control-flow, data, resource, exception, all)

**Response** (200 OK):
```json
{
  "patterns": [
    {
      "name": "sequence",
      "category": "control-flow",
      "display_name": "Sequence",
      "description": "Sequential execution of tasks",
      "supported": true
    },
    {
      "name": "parallel_split",
      "category": "control-flow",
      "display_name": "Parallel Split",
      "description": "Split execution into parallel branches",
      "supported": true
    },
    {
      "name": "synchronization",
      "category": "control-flow",
      "display_name": "Synchronization",
      "description": "Join parallel branches",
      "supported": true
    },
    {
      "name": "exclusive_choice",
      "category": "control-flow",
      "display_name": "Exclusive Choice",
      "description": "Select one path based on conditions",
      "supported": true
    },
    {
      "name": "simple_merge",
      "category": "control-flow",
      "display_name": "Simple Merge",
      "description": "Merge multiple paths",
      "supported": true
    }
  ],
  "total": 43
}
```

### GET /patterns/{pattern_name}

Get detailed information about a specific pattern.

```http
GET /patterns/parallel_split
Authorization: Basic base64(username:password)
```

**Response** (200 OK):
```json
{
  "name": "parallel_split",
  "category": "control-flow",
  "display_name": "Parallel Split",
  "description": "Split execution into parallel branches",
  "supported": true,
  "petri_net": {
    "places": [
      {
        "id": "input",
        "name": "Input Place"
      },
      {
        "id": "output_1",
        "name": "Output Place 1"
      },
      {
        "id": "output_2",
        "name": "Output Place 2"
      }
    ],
    "transitions": [
      {
        "id": "split",
        "name": "Split Transition"
      }
    ],
    "arcs": [
      {
        "from": "input",
        "to": "split"
      },
      {
        "from": "split",
        "to": "output_1"
      },
      {
        "from": "split",
        "to": "output_2"
      }
    ]
  },
  "input_places": ["input"],
  "output_places": ["output_1", "output_2"],
  "usage_example": "{parallel_split, [task_1, task_2, task_3]}",
  "requirements": ["All branches execute concurrently", "No synchronization point"]
}
```

## System Status

### GET /status.json

Get current status of the CRE master process.

```http
GET /status.json
Authorization: Basic base64(username:password)
```

**Response** (200 OK):
```json
{
  "status": "running",
  "cre_info": {
    "n_wrk": 8,
    "load": 0.45,
    "uptime_seconds": 3600
  },
  "timestamp": 1707641400000
}
```

### GET /history.json

Get workflow execution history (cached results).

```http
GET /history.json
Authorization: Basic base64(username:password)
```

**Response** (200 OK):
```json
{
  "history": [
    {
      "app": "payment_workflow",
      "delta": {
        "status": "completed",
        "result": "success",
        "amount": 1000,
        "timestamp": "2025-02-11T10:00:00Z"
      }
    },
    {
      "app": "approval_workflow",
      "delta": {
        "status": "completed",
        "approved": true,
        "approver": "manager_001"
      }
    }
  ]
}
```

## Error Handling

All errors follow a consistent JSON structure:

```json
{
  "error": "error_code",
  "message": "Human-readable description",
  "details": {
    "additional": "information"
  }
}
```

### Common Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `bad_request` | 400 | Invalid request parameters or format |
| `unauthorized` | 401 | Missing or invalid authentication |
| `forbidden` | 403 | Insufficient permissions |
| `not_found` | 404 | Requested resource not found |
| `conflict` | 409 | Resource state conflict or constraint violation |
| `invalid_state_transition` | 409 | Cannot perform operation in current state |
| `invalid_workflow` | 400 | Workflow specification is invalid |
| `workflow_not_found` | 404 | Workflow specification not found |
| `case_not_found` | 404 | Workflow case not found |
| `pattern_not_found` | 404 | Pattern not found |
| `internal_error` | 500 | Internal server error |
| `service_unavailable` | 503 | Service temporarily unavailable |

### Error Response Examples

**400 Bad Request**:
```json
{
  "error": "bad_request",
  "message": "Invalid workflow specification: Missing required field 'name'",
  "details": {
    "field": "name",
    "constraint": "required"
  }
}
```

**401 Unauthorized**:
```json
{
  "error": "unauthorized",
  "message": "Authentication required. Provide credentials in Authorization header."
}
```

**404 Not Found**:
```json
{
  "error": "not_found",
  "message": "Workflow case 'case_001' not found",
  "details": {
    "resource_type": "case",
    "resource_id": "case_001"
  }
}
```

**409 Conflict**:
```json
{
  "error": "invalid_state_transition",
  "message": "Cannot cancel a case in status: completed",
  "details": {
    "current_status": "completed",
    "requested_operation": "cancel"
  }
}
```

**503 Service Unavailable**:
```json
{
  "error": "service_unavailable",
  "message": "CRE master process is not available",
  "details": {
    "unavailable_component": "cre_master"
  }
}
```

## Rate Limiting

Rate limiting is not currently implemented but is planned for future releases. When implemented, the following headers will be used:

```http
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 999
X-RateLimit-Reset: 1707641400
```

## Pagination

Endpoints that return lists support pagination via query parameters:

- `limit` (integer, default: 50, max: 1000): Number of results to return
- `offset` (integer, default: 0): Number of results to skip

Response includes pagination metadata:

```json
{
  "items": [...],
  "total": 250,
  "limit": 50,
  "offset": 0
}
```

## Request/Response Examples

### Example 1: List Cases and Get Details

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"
USER="admin"
PASS="password"

# List running cases
curl -s -u "$USER:$PASS" "$BASE_URL/api/yawl/cases?status=running" | jq

# Get details of first case
CASE_ID=$(curl -s -u "$USER:$PASS" "$BASE_URL/api/yawl/cases?limit=1" | jq -r '.cases[0].id')
curl -s -u "$USER:$PASS" "$BASE_URL/api/yawl/cases/$CASE_ID" | jq
```

### Example 2: Validate Workflow and Get Pattern Info

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"
USER="admin"
PASS="password"

# Validate workflow
curl -s -u "$USER:$PASS" -X POST "$BASE_URL/api/yawl/specifications/spec_001/validate" | jq

# Get parallel_split pattern details
curl -s -u "$USER:$PASS" "$BASE_URL/patterns/parallel_split" | jq
```

### Example 3: Check Health and Status

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"

# Check liveness (no auth required)
curl -s "$BASE_URL/health" | jq '.status'

# Check readiness (no auth required)
curl -s "$BASE_URL/ready" | jq '.status'

# Get system status (auth required)
curl -s -u "admin:password" "$BASE_URL/status.json" | jq '.cre_info'
```

### Example 4: Get User Worklist

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"
USER="admin"
PASS="password"
USER_ID="user_123"

# Get enabled work items for user
curl -s -u "$USER:$PASS" "$BASE_URL/api/yawl/worklist/$USER_ID?filter=enabled" | jq '.work_items'
```

## Integration Examples

### Docker Compose Health Check

```yaml
version: '3'
services:
  cre:
    image: cre:0.3.0
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 5s
      retries: 3
      start_period: 40s
```

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cre
spec:
  template:
    spec:
      containers:
      - name: cre
        image: cre:0.3.0
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 20
          periodSeconds: 5
        startupProbe:
          httpGet:
            path: /startup
            port: 8080
          failureThreshold: 30
          periodSeconds: 10
```

### Monitoring with cURL

```bash
#!/bin/bash

# Continuous health monitoring script
while true; do
  STATUS=$(curl -s http://localhost:8080/health | jq -r '.status')
  TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')
  echo "[$TIMESTAMP] Health Status: $STATUS"
  sleep 30
done
```

## OpenAPI Specification

The complete OpenAPI 3.0 specification is available in `/docs/api/openapi.yaml`.

This specification can be used to:
- Generate client SDKs in multiple languages
- Create interactive API documentation with Swagger UI or ReDoc
- Validate requests and responses
- Generate server stubs

### Generate Swagger UI Documentation

```bash
docker run -p 8081:8080 \
  -e SWAGGER_JSON=/docs/openapi.yaml \
  -v $(pwd)/docs/api:/docs \
  swaggerapi/swagger-ui
```

Visit http://localhost:8081 to view interactive API documentation.

## Future Enhancements

The following features are planned for future releases:

1. **OAuth 2.0 / OpenID Connect**: Replace basic auth with industry-standard OAuth
2. **Rate Limiting**: Per-user request rate limits
3. **API Keys**: Simplified authentication for scripts and integrations
4. **GraphQL API**: Query and mutation support for flexible data retrieval
5. **Server-Sent Events (SSE)**: Real-time event streaming for case progress
6. **WebSocket Support**: Bi-directional communication for long-running operations
7. **Request/Response Caching**: Improved performance for repeated queries
8. **Bulk Operations**: Create/update multiple resources in single request

## Support and Feedback

For issues, feature requests, or feedback:

- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Email**: support@cre-project.org
- **Documentation**: https://github.com/joergen7/cre/tree/master/docs
