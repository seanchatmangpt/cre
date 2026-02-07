# CRE API Documentation Index

## Welcome to the CRE API Documentation

This is your complete guide to the Cuneiform Runtime Environment (CRE) APIs, including OpenAPI specifications, detailed API reference documentation, and client SDKs for Python, Go, and Node.js.

---

## Quick Navigation

### For API Users

1. **[OpenAPI Summary](./OPENAPI-SUMMARY.md)** - Overview of all deliverables
2. **[API Reference](./API-REFERENCE.md)** - Complete API documentation with examples
3. **[SDK Quick Start](../sdks/README.md)** - Get started with client SDKs

### For Developers

1. **[SDK Implementation Guide](./SDK-GUIDE.md)** - Deep dive into SDK usage
2. **[OpenAPI Specifications](#openapi-specifications)** - Raw specification files
3. **[SDK Generation](./OPENAPI-SUMMARY.md#5-sdk-generator-script)** - How to regenerate SDKs

### For Operations

1. **[API Reference - Monitoring](./API-REFERENCE.md#examples)** - Example monitoring scripts
2. **[SDK Guide - Common Patterns](./SDK-GUIDE.md#common-patterns)** - Polling, retries, health checks
3. **[CRE Master API Status](./API-REFERENCE.md#get-system-status)** - Monitor system health

---

## OpenAPI Specifications

### CRE Master API

- **Specification:** [`openapi-cre-master.yaml`](./openapi-cre-master.yaml)
- **Version:** 0.2.0
- **Port:** 4142 (default)
- **Endpoints:**
  - `GET /` - System status
  - `GET /history.json` - Execution history

**Quick Start:**
```bash
curl http://localhost:4142/status.json | jq .
```

**View in Swagger UI:**
```bash
swagger-ui openapi-cre-master.yaml
```

### YAWL Dashboard API

- **Specification:** [`openapi-yawl-dashboard.yaml`](./openapi-yawl-dashboard.yaml)
- **Version:** 1.0.0
- **Port:** 8081 (default)
- **Endpoints:**
  - `GET /api/events` - Workflow events
  - `GET /api/traces` - Execution traces
  - `GET /api/stats` - Statistics

**Quick Start:**
```bash
curl http://localhost:8081/api/traces | jq .
```

---

## Documentation Files

### Complete Reference Documents

| File | Purpose | Audience |
|------|---------|----------|
| [`API-REFERENCE.md`](./API-REFERENCE.md) | Full API documentation with examples | API Users |
| [`SDK-GUIDE.md`](./SDK-GUIDE.md) | Implementation guide for SDKs | Developers |
| [`OPENAPI-SUMMARY.md`](./OPENAPI-SUMMARY.md) | Overview of specifications and SDKs | Everyone |
| [`INDEX.md`](./INDEX.md) | This file - navigation guide | Everyone |

### OpenAPI Specification Files

| File | API | Port | Endpoints |
|------|-----|------|-----------|
| [`openapi-cre-master.yaml`](./openapi-cre-master.yaml) | CRE Master | 4142 | Status, History |
| [`openapi-yawl-dashboard.yaml`](./openapi-yawl-dashboard.yaml) | YAWL Dashboard | 8081 | Events, Traces, Stats |

---

## Client SDKs

### Available SDKs

All SDKs are located in the [`/sdks/`](../sdks/) directory:

| Language | File | Status | Notes |
|----------|------|--------|-------|
| **Python** | [`python_sdk.py`](../sdks/python_sdk.py) | Complete | Requires `requests` |
| **Go** | [`go_sdk.go`](../sdks/go_sdk.go) | Complete | Package: `creapi` |
| **Node.js** | [`nodejs_sdk.js`](../sdks/nodejs_sdk.js) | Complete | Requires `node-fetch` |

### SDK Documentation

- **[SDK Quick Start Guide](../sdks/README.md)** - Installation and basic usage for all languages
- **[SDK Implementation Guide](./SDK-GUIDE.md)** - Advanced patterns and best practices

---

## Getting Started

### Step 1: Choose Your Language

**Python?** → [Python SDK Quick Start](../sdks/README.md#python)

**Go?** → [Go SDK Quick Start](../sdks/README.md#go)

**Node.js?** → [Node.js SDK Quick Start](../sdks/README.md#nodejs)

### Step 2: Read the API Reference

Review the endpoints you need:
- **Status/History?** → [CRE Master API](./API-REFERENCE.md#cre-master-api)
- **Events/Traces/Stats?** → [YAWL Dashboard API](./API-REFERENCE.md#yawl-dashboard-api)

### Step 3: Implement Your Integration

Follow examples in:
- **[SDK Quick Start](../sdks/README.md)** - Basic usage
- **[SDK Guide](./SDK-GUIDE.md)** - Advanced patterns
- **[API Reference Examples](./API-REFERENCE.md#examples)** - API-level examples

---

## Common Tasks

### Monitor CRE Status

**Python:**
```python
from sdks.python_sdk import CREMasterClient

client = CREMasterClient()
status = client.get_status()
print(f"Idle: {status['idle_count']}, Busy: {status['busy_count']}")
```

**Go:**
```go
client := creapi.NewClient("http://localhost:4142")
status, _ := client.GetStatus(context.Background())
fmt.Printf("Idle: %d\n", status.IdleCount)
```

**Node.js:**
```javascript
const client = new CREClient();
const status = await client.getStatus();
console.log(`Idle: ${status.idle_count}`);
```

See [API-REFERENCE Examples](./API-REFERENCE.md#examples) for more patterns.

### Check Workflow Statistics

**YAWL Dashboard API:**
```bash
curl "http://localhost:8081/api/stats?time_window=24h" | jq .
```

See [YAWL Dashboard Documentation](./API-REFERENCE.md#yawl-dashboard-api) for details.

### Monitor Workflow Execution

**Node.js Example:**
```javascript
const { YAWLDashboardClient } = require('./nodejs_sdk');

const client = new YAWLDashboardClient();
const events = await client.listEvents({ limit: 50 });
console.log(`Recent events: ${events.total}`);
```

---

## API Endpoints at a Glance

### CRE Master API

**Base URL:** `http://localhost:4142`

```
GET  /                    → System status
GET  /status.json         → System status (alias)
GET  /history.json        → Execution history & cache
```

### YAWL Dashboard API

**Base URL:** `http://localhost:8081`

```
GET  /                           → Dashboard HTML
GET  /api/events                 → List events
GET  /api/events/{traceId}       → Get trace events
GET  /api/traces                 → List traces
GET  /api/stats                  → Get statistics
DELETE /api/clear?confirm=yes    → Clear data
```

For full endpoint details, see [API-REFERENCE.md](./API-REFERENCE.md).

---

## Learning Paths

### Path 1: Basic Monitoring (15 minutes)

1. Read: [API Reference - Status Endpoint](./API-REFERENCE.md#1-get-system-status)
2. Try: `curl http://localhost:4142/status.json`
3. Code: [SDK Quick Start - Your Language](../sdks/README.md#quick-start)
4. Deploy: Basic monitoring script

### Path 2: Workflow Analytics (30 minutes)

1. Read: [YAWL Dashboard API](./API-REFERENCE.md#yawl-dashboard-api)
2. Try: `curl http://localhost:8081/api/traces`
3. Code: [YAWL Dashboard Examples](./SDK-GUIDE.md#nodejs---yawl-dashboard-integration)
4. Deploy: Analytics dashboard

### Path 3: Advanced Integration (1-2 hours)

1. Study: [SDK Implementation Guide](./SDK-GUIDE.md#common-patterns)
2. Choose: Your integration pattern (polling, monitoring, etc.)
3. Implement: Using your language SDK
4. Test: Error cases and edge conditions
5. Deploy: With production-grade error handling

---

## Troubleshooting

### API Not Responding

1. Check if services are running:
   ```bash
   curl http://localhost:4142/status.json
   curl http://localhost:8081/api/traces
   ```

2. Review: [Error Handling](./API-REFERENCE.md#error-handling)

3. Debug: Enable verbose logging in SDK

### SDK Issues

1. Check: [SDK Troubleshooting](./SDK-GUIDE.md#error-handling)
2. Verify: Dependencies are installed
3. Try: [SDK Examples](../sdks/README.md#advanced-usage)

---

## Reference Materials

### Quick Reference

- **[CRE Master Endpoints](./API-REFERENCE.md#cre-master-api)** - 3 main endpoints
- **[YAWL Dashboard Endpoints](./API-REFERENCE.md#yawl-dashboard-api)** - 6 main endpoints
- **[Error Codes](./API-REFERENCE.md#error-handling)** - How to handle errors
- **[Status Codes](./API-REFERENCE.md#common-http-status-codes)** - HTTP status meanings

### Advanced Reference

- **[OpenAPI Spec - CRE Master](./openapi-cre-master.yaml)** - Raw OpenAPI definition
- **[OpenAPI Spec - YAWL Dashboard](./openapi-yawl-dashboard.yaml)** - Raw OpenAPI definition
- **[SDK Schemas](./OPENAPI-SUMMARY.md#response-schema)** - Data model definitions

---

## SDK Comparison

| Feature | Python | Go | Node.js |
|---------|--------|----|---------|
| Async Support | No | Yes | Yes |
| Type Safety | Partial | Full | None |
| Monitoring | Manual | Built-in | Built-in |
| Error Types | Custom | Custom | Custom |
| Dependencies | 1 | 0 | 1 |
| Best For | Scripts | Services | Web |

---

## Production Checklist

- [ ] Choose appropriate SDK for your platform
- [ ] Install all required dependencies
- [ ] Review error handling patterns
- [ ] Implement retry logic with exponential backoff
- [ ] Add request timeouts
- [ ] Set up logging for API calls
- [ ] Configure rate limiting if needed
- [ ] Test failure scenarios
- [ ] Document API usage in your codebase
- [ ] Monitor API performance in production
- [ ] Set up alerts for API errors

See [SDK Guide - Best Practices](./SDK-GUIDE.md#best-practices) for more details.

---

## Frequently Asked Questions

**Q: Which SDK should I use?**
> A: Python for scripts/tools, Go for services, Node.js for web apps.

**Q: How often can I call the API?**
> A: No rate limiting currently, but use 5-10 second polling intervals in production.

**Q: What are the default ports?**
> A: CRE Master: 4142, YAWL Dashboard: 8081

**Q: Can I use these APIs from anywhere?**
> A: Yes, but deploy behind a reverse proxy with authentication in production.

**Q: How do I regenerate SDKs?**
> A: Run `python3 scripts/generate-sdks.py` - see [SDK Generator](./OPENAPI-SUMMARY.md#5-sdk-generator-script).

**Q: Where can I get help?**
> A: Check [API Reference](./API-REFERENCE.md), [SDK Guide](./SDK-GUIDE.md), or GitHub issues.

---

## Additional Resources

- **GitHub Repository:** https://github.com/ruvnet/claude-flow
- **Issue Tracker:** https://github.com/ruvnet/claude-flow/issues
- **Documentation:** This directory (`/docs/`)
- **SDKs:** `/sdks/` directory
- **Tools:** `/scripts/` directory

---

## File Location Summary

```
/home/user/cre/
│
├── docs/                          ← You are here
│   ├── INDEX.md                   ← This file
│   ├── API-REFERENCE.md           ← Full API documentation
│   ├── SDK-GUIDE.md               ← SDK implementation guide
│   ├── OPENAPI-SUMMARY.md         ← Overview of all deliverables
│   ├── openapi-cre-master.yaml    ← CRE Master API spec
│   └── openapi-yawl-dashboard.yaml ← YAWL Dashboard API spec
│
├── sdks/                          ← Client SDKs
│   ├── README.md                  ← SDK quick start
│   ├── python_sdk.py              ← Python SDK
│   ├── go_sdk.go                  ← Go SDK
│   └── nodejs_sdk.js              ← Node.js SDK
│
└── scripts/
    └── generate-sdks.py           ← SDK generator tool
```

---

## Last Updated

- **Date:** 2024-02-06
- **CRE Master API Version:** 0.2.0
- **YAWL Dashboard API Version:** 1.0.0
- **OpenAPI Version:** 3.0.0

---

## How to Use This Documentation

1. **I want to use the API** → Start with [API-REFERENCE.md](./API-REFERENCE.md)
2. **I want to build a client** → Check [SDK Quick Start](../sdks/README.md)
3. **I want advanced patterns** → Read [SDK-GUIDE.md](./SDK-GUIDE.md)
4. **I want to understand everything** → Review [OPENAPI-SUMMARY.md](./OPENAPI-SUMMARY.md)
5. **I want the raw specifications** → See the `.yaml` files
6. **I'm lost** → You're reading the right file! (INDEX.md)

---

## Next Steps

1. Choose your approach above
2. Follow the link provided
3. Start building!

**Happy coding!**
