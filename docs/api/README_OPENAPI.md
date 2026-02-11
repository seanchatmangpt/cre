# CRE API Documentation

**Version**: 0.3.0
**Status**: Ready for GCP Marketplace Submission
**Last Updated**: 2025-02-11

## Quick Navigation

This directory contains comprehensive API documentation for the CRE (Common Runtime Environment) HTTP API:

- **[openapi.yaml](#openapiyaml)** - Complete OpenAPI 3.0 specification
- **[HTTP_API_REFERENCE.md](#http_api_referencemd)** - Detailed endpoint reference with examples
- **[INTEGRATION_GUIDE.md](#integration_guidemd)** - Practical integration examples for multiple languages

## openapi.yaml

Complete OpenAPI 3.0.3 specification for the CRE API.

### Use Cases

1. **API Documentation**: View interactive documentation via Swagger UI or ReDoc
2. **Client Generation**: Generate SDKs for Python, Java, Go, Node.js, etc.
3. **Testing**: Use in Postman, REST Client, or other API testing tools
4. **Contract Testing**: Validate API responses against specification
5. **Monitoring**: Track API changes and breaking changes

### Viewing the Specification

#### Option 1: Swagger UI (Interactive Documentation)

```bash
# Using Docker
docker run -p 8081:8080 \
  -v $(pwd)/docs/api:/docs \
  -e SWAGGER_JSON=/docs/openapi.yaml \
  swaggerapi/swagger-ui

# Visit http://localhost:8081
```

#### Option 2: ReDoc (Clean Documentation)

```bash
# Using Docker
docker run -p 8081:80 \
  -v $(pwd)/docs/api:/usr/share/nginx/html/docs \
  -e SPEC_URL=/docs/openapi.yaml \
  redocly/redoc

# Visit http://localhost:8081
```

#### Option 3: Postman

1. In Postman, select "File" → "Import"
2. Choose "Link" tab
3. Enter: `file:///path/to/openapi.yaml`
4. Click Import

#### Option 4: Command Line

```bash
# Install swagger-cli (Node.js)
npm install -g swagger-cli

# Validate specification
swagger-cli validate openapi.yaml

# Bundle with references
swagger-cli bundle openapi.yaml -o bundled.yaml
```

### Generating Client SDKs

#### Python

```bash
# Install code generator
pip install openapi-generator-cli

# Generate Python client
openapi-generator generate \
  -i openapi.yaml \
  -g python \
  -o ./generated/python-client

# Install generated client
cd generated/python-client
pip install -e .
```

#### Java

```bash
# Generate Java client
openapi-generator generate \
  -i openapi.yaml \
  -g java \
  -o ./generated/java-client \
  --additional-properties=artifactId=cre-client

# Build with Maven
cd generated/java-client
mvn clean package
```

#### TypeScript/JavaScript

```bash
# Generate TypeScript client
openapi-generator generate \
  -i openapi.yaml \
  -g typescript-axios \
  -o ./generated/ts-client

# Install dependencies
cd generated/ts-client
npm install
npm publish
```

#### Go

```bash
# Generate Go client
openapi-generator generate \
  -i openapi.yaml \
  -g go \
  -o ./generated/go-client
```

### API Specification Overview

The OpenAPI specification documents:

#### Authentication
- HTTP Basic Authentication (username:password)
- Bearer Token (future OAuth 2.0 support)

#### Health Check Endpoints
- `GET /health` - Liveness probe
- `GET /ready` - Readiness probe
- `GET /startup` - Startup probe

#### Workflow Management
- `GET /api/yawl/specifications` - List specifications
- `POST /api/yawl/specifications` - Upload specification (future)
- `GET /api/yawl/specifications/{id}` - Get specification details
- `POST /api/yawl/specifications/{id}/validate` - Validate workflow
- `POST /api/yawl/specifications/{id}/launch` - Launch case (future)

#### Case Management
- `GET /api/yawl/cases` - List cases
- `GET /api/yawl/cases/{id}` - Get case details
- `POST /api/yawl/cases/{id}/cancel` - Cancel case
- `POST /api/yawl/cases/{id}/suspend` - Suspend case (future)
- `POST /api/yawl/cases/{id}/resume` - Resume case (future)

#### Task Management
- `GET /api/yawl/worklist/{user_id}` - Get user's work items

#### Pattern Management
- `GET /patterns` - List patterns
- `GET /patterns/{name}` - Get pattern details

#### System Status
- `GET /status.json` - Master process status
- `GET /history.json` - Workflow execution history

### Schema Definitions

The specification includes detailed schema definitions for:

- **HealthResponse** - Health check response structure
- **MasterStatus** - CRE master process status
- **WorkflowSpecification** - Workflow definition and metadata
- **CaseInstance** - Workflow case instance
- **CaseDetail** - Detailed case information with work items
- **WorkItem** - Individual work item/task
- **TaskExecution** - Task execution history entry
- **ValidationResult** - Workflow validation results
- **PatternDefinition** - YAWL pattern definition
- **PatternDetail** - Detailed pattern information with Petri Net
- **ErrorResponse** - Standard error response format

### Examples

The specification includes concrete examples for:
- Healthy/unhealthy health checks
- Workflow case listing and details
- Case cancellation
- Workflow validation (valid and invalid cases)
- Pattern listings
- System status responses

## HTTP_API_REFERENCE.md

Comprehensive reference documentation for all HTTP endpoints.

### Contents

1. **Overview** - API capabilities and base URLs
2. **Authentication** - Basic auth, Bearer tokens, environment variables
3. **Health Check Endpoints** - `/health`, `/ready`, `/startup`
4. **Workflow Management** - List, retrieve, validate specifications
5. **Case Management** - Create, monitor, control workflow cases
6. **Task Management** - Access work items and assignments
7. **Pattern Management** - Query available YAWL patterns
8. **System Status** - Master status and execution history
9. **Error Handling** - Error codes and response formats
10. **Pagination** - Limit, offset, and response metadata
11. **Request/Response Examples** - Practical usage examples

### Key Sections

#### Health Monitoring Loop

Example script for continuous service health monitoring:

```bash
#!/bin/bash
while true; do
  STATUS=$(curl -s http://localhost:8080/health | jq -r '.status')
  echo "[$(date)] Health Status: $STATUS"
  sleep 30
done
```

#### Case Execution Workflow

Complete example from specification validation to results:

```bash
# 1. List specifications
# 2. Validate specification
# 3. List existing cases
# 4. Get case details
# 5. Get system status
```

#### Error Handling with Retry Logic

Robust error handling with exponential backoff:

```bash
function call_api_with_retry() {
  local endpoint=$1
  local attempt=1

  while [ $attempt -le 3 ]; do
    RESPONSE=$(curl -s -w "\n%{http_code}" "$endpoint")
    HTTP_CODE=$(echo "$RESPONSE" | tail -n 1)

    case $HTTP_CODE in
      200|201|202) return 0 ;;
      503) sleep $((2 ** attempt)); ((attempt++)) ;;
      *) return 1 ;;
    esac
  done
  return 1
}
```

## INTEGRATION_GUIDE.md

Practical integration examples for real-world development.

### Contents

1. **Quick Start** - cURL and Postman examples
2. **Authentication** - Multiple auth mechanisms
3. **Common Patterns** - Health monitoring, batch processing, error handling
4. **Language Examples**:
   - Python
   - Node.js/JavaScript
   - Java
   - Go
5. **Production Considerations** - Connection pooling, timeouts, circuit breakers
6. **Troubleshooting** - Common issues and solutions

### Client Libraries

Complete working examples for:

#### Python CREClient

```python
client = CREClient()
health = client.get_health()
cases = client.list_cases(status='running')
```

#### JavaScript/Node.js CREClient

```javascript
const client = new CREClient();
const health = await client.getHealth();
const cases = await client.listCases('running', 5, 0);
```

#### Java CREClient

```java
CREClient client = new CREClient(
  "http://localhost:8080",
  "admin",
  "password"
);
JsonObject health = client.getHealth();
JsonObject cases = client.listCases(5, 0);
```

#### Go CREClient

```go
client := NewCREClient(
  "http://localhost:8080",
  "admin",
  "password",
)
health, _ := client.GetHealth()
cases, _ := client.ListCases("running", 5, 0)
```

### Production Patterns

- Connection pooling for HTTP reuse
- Timeout configuration
- Circuit breaker implementation
- Rate limiting
- Logging and monitoring

## API Documentation Workflow

### For API Consumers

1. **Start Here**: Read [HTTP_API_REFERENCE.md](#http_api_referencemd) overview
2. **Find Endpoint**: Browse endpoint documentation
3. **Check Examples**: Review request/response examples
4. **Implement**: Use [INTEGRATION_GUIDE.md](#integration_guidemd) for your language
5. **Reference**: Consult [openapi.yaml](#openapiyaml) for detailed schema

### For API Developers

1. **Update Specification**: Modify [openapi.yaml](#openapiyaml)
2. **Add Examples**: Include request/response examples in spec
3. **Document**: Update [HTTP_API_REFERENCE.md](#http_api_referencemd)
4. **Add Integration**: Add language example to [INTEGRATION_GUIDE.md](#integration_guidemd)
5. **Validate**: Use `swagger-cli validate openapi.yaml`
6. **Generate Docs**: Create interactive documentation with Swagger UI

### For Operations Teams

1. **Monitor Health**: Use `/health`, `/ready`, `/startup` endpoints
2. **Check Status**: Query `/status.json` for system metrics
3. **Review History**: Access `/history.json` for execution audit trail
4. **Integrate**: Use client libraries for custom monitoring

## Using OpenAPI with Tools

### Validation

```bash
# Install validation tools
npm install -g swagger-cli

# Validate specification
swagger-cli validate openapi.yaml

# Check for issues
swagger-cli validate --verbose openapi.yaml
```

### Documentation Generation

```bash
# Generate HTML documentation
docker run --rm \
  -v $(pwd)/docs/api:/input \
  -v $(pwd)/generated:/output \
  redocly/redoc-cli build -o /output/index.html /input/openapi.yaml

# Generate PDF
docker run --rm \
  -v $(pwd)/docs/api:/input \
  -v $(pwd)/generated:/output \
  redocly/redoc-cli build --pdf -o /output/api.pdf /input/openapi.yaml
```

### Code Generation

```bash
# Generate clients in multiple languages
for lang in python javascript java go; do
  openapi-generator generate \
    -i openapi.yaml \
    -g $lang \
    -o ./generated/$lang-client
done
```

### Testing

```bash
# Install test tools
npm install -g dredd

# Run API tests against running server
dredd openapi.yaml http://localhost:8080

# Generate test suite
dredd openapi.yaml http://localhost:8080 --generateHooksFile hooks.js
```

## API Versioning

The current API version is **0.3.0** (implicit in URL structure).

### Future Versions

When new API versions are released:

1. Create new specification file: `openapi_v2.yaml`
2. Update all endpoints with `/api/v2` prefix
3. Maintain backward compatibility with `v1` endpoints
4. Document migration path for clients
5. Set deprecation timeline for old versions

## OpenAPI Tools and Resources

### Online Tools
- **Editor**: https://editor.swagger.io/
- **Validator**: https://validator.swagger.io/
- **UI**: https://petstore.swagger.io/

### Documentation Tools
- **Swagger UI**: https://github.com/swagger-api/swagger-ui
- **ReDoc**: https://github.com/Redocly/redoc
- **OpenAPI Generator**: https://openapi-generator.tech/

### Client Generation
- **OpenAPI Generator**: https://openapi-generator.tech/
- **Swagger Codegen**: https://github.com/swagger-api/swagger-codegen
- **Spectacle**: https://sourcey.com/spectacle/

## Integration Checklist

When integrating with the CRE API:

- [ ] Read HTTP_API_REFERENCE.md overview
- [ ] Choose authentication method (Basic or Bearer)
- [ ] Select client library (Python, Node.js, Java, Go, or HTTP)
- [ ] Implement error handling and retries
- [ ] Configure connection pooling and timeouts
- [ ] Set up health checks and monitoring
- [ ] Test against staging environment
- [ ] Deploy to production with proper logging
- [ ] Monitor API usage and performance
- [ ] Subscribe to API change notifications

## Support and Feedback

For issues, questions, or feedback:

- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Email**: support@cre-project.org
- **Documentation**: https://github.com/joergen7/cre/tree/master/docs
- **Community**: CRE Discussion Forum (coming soon)

## OpenAPI Specification Compliance

This API specification adheres to:

- **OpenAPI Version**: 3.0.3
- **HTTP/REST**: RESTful principles with JSON payloads
- **Status Codes**: Standard HTTP status codes
- **Error Format**: Consistent JSON error responses
- **Security**: HTTP Basic Auth, Bearer Token support
- **CORS**: Cross-Origin Resource Sharing (future)

## Summary

CRE provides a production-ready HTTP API with:

✅ **Complete Documentation**
- OpenAPI 3.0.3 specification
- Detailed HTTP endpoint reference
- Practical integration examples

✅ **Multiple Client Options**
- Python, JavaScript/Node.js, Java, Go
- cURL, Postman, REST Client
- Auto-generated SDKs

✅ **Production Ready**
- Health checks (liveness, readiness, startup)
- Error handling and retry patterns
- Rate limiting (planned)
- Comprehensive logging

✅ **Marketplace Ready**
- GCP Artifact Registry support
- Kubernetes probe compatibility
- Comprehensive API documentation
- Security and compliance features

Start integrating with the CRE API today!
