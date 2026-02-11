# CRE API Integration Guide

**Version**: 0.3.0
**Last Updated**: 2025-02-11

## Introduction

This guide provides practical examples for integrating with the CRE HTTP API from various platforms and languages. It covers authentication, error handling, and common integration patterns.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Authentication](#authentication)
3. [Common Patterns](#common-patterns)
4. [Language-Specific Examples](#language-specific-examples)
5. [Production Considerations](#production-considerations)
6. [Troubleshooting](#troubleshooting)

## Quick Start

### Using cURL

```bash
# Basic health check (no authentication required)
curl http://localhost:8080/health

# List workflow cases (with authentication)
curl -u "admin:password" http://localhost:8080/api/yawl/cases

# Pretty-print JSON output
curl -s -u "admin:password" http://localhost:8080/api/yawl/cases | jq
```

### Using Postman

1. Import the OpenAPI specification: `/docs/api/openapi.yaml`
2. Create a new environment with variables:
   - `base_url`: http://localhost:8080
   - `username`: admin
   - `password`: password
3. Configure Basic Auth in Postman with `{{username}}` and `{{password}}`

## Authentication

### Basic Authentication

The easiest method for development and testing:

```bash
# Encode credentials as base64
echo -n "username:password" | base64
# Output: dXNlcm5hbWU6cGFzc3dvcmQ=

# Use in request
curl -H "Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=" \
  http://localhost:8080/status.json
```

### Token-Based Authentication (Future)

When OAuth 2.0 support is added:

```bash
# Get token from auth endpoint (future)
TOKEN=$(curl -X POST http://localhost:8080/auth/token \
  -d "username=admin&password=password" | jq -r '.access_token')

# Use token in requests
curl -H "Authorization: Bearer $TOKEN" \
  http://localhost:8080/status.json
```

### Environment Variables

Store credentials securely:

```bash
# Set environment variables
export CRE_BASE_URL="http://localhost:8080"
export CRE_USERNAME="admin"
export CRE_PASSWORD="password"

# Use in requests
curl -u "$CRE_USERNAME:$CRE_PASSWORD" \
  "$CRE_BASE_URL/status.json"
```

## Common Patterns

### 1. Health Monitoring Loop

Monitor service health continuously:

```bash
#!/bin/bash

BASE_URL="${CRE_BASE_URL:-http://localhost:8080}"
INTERVAL="${CHECK_INTERVAL:-30}"

while true; do
  # Check health
  RESPONSE=$(curl -s "$BASE_URL/health")
  STATUS=$(echo "$RESPONSE" | jq -r '.status')
  TIMESTAMP=$(echo "$RESPONSE" | jq -r '.timestamp')

  # Log result
  echo "[$(date)] Status: $STATUS"

  # Alert if unhealthy
  if [ "$STATUS" != "healthy" ]; then
    echo "WARNING: Service is $STATUS"
    # Send alert (Slack, email, etc.)
  fi

  sleep "$INTERVAL"
done
```

### 2. Workflow Case Execution Flow

Complete workflow from specification to results:

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"
AUTH="-u admin:password"

# Step 1: List available specifications
echo "Step 1: Listing workflow specifications..."
SPECS=$(curl -s $AUTH "$BASE_URL/api/yawl/specifications")
SPEC_ID=$(echo "$SPECS" | jq -r '.specifications[0].id')
echo "Using specification: $SPEC_ID"

# Step 2: Validate specification
echo "Step 2: Validating specification..."
VALIDATION=$(curl -s $AUTH -X POST \
  "$BASE_URL/api/yawl/specifications/$SPEC_ID/validate")
IS_VALID=$(echo "$VALIDATION" | jq -r '.is_valid')
echo "Validation result: $IS_VALID"

if [ "$IS_VALID" != "true" ]; then
  echo "Validation failed. Errors:"
  echo "$VALIDATION" | jq '.errors'
  exit 1
fi

# Step 3: List existing cases
echo "Step 3: Listing existing cases..."
CASES=$(curl -s $AUTH "$BASE_URL/api/yawl/cases?status=running")
CASE_COUNT=$(echo "$CASES" | jq '.cases | length')
echo "Found $CASE_COUNT running cases"

# Step 4: Get details of latest case
if [ "$CASE_COUNT" -gt 0 ]; then
  CASE_ID=$(echo "$CASES" | jq -r '.cases[0].id')
  echo "Step 4: Getting details for case $CASE_ID..."
  CASE_DETAIL=$(curl -s $AUTH "$BASE_URL/api/yawl/cases/$CASE_ID")
  echo "Case status: $(echo "$CASE_DETAIL" | jq -r '.status')"
  echo "Active work items: $(echo "$CASE_DETAIL" | jq -r '.active_work_items')"
  echo "Completed tasks: $(echo "$CASE_DETAIL" | jq -r '.completed_tasks')"
fi

# Step 5: Get system status
echo "Step 5: Getting system status..."
STATUS=$(curl -s $AUTH "$BASE_URL/status.json")
echo "Worker count: $(echo "$STATUS" | jq -r '.cre_info.n_wrk')"
echo "System load: $(echo "$STATUS" | jq -r '.cre_info.load')"
```

### 3. Error Handling and Retry Logic

Handle errors gracefully with exponential backoff:

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"
AUTH="-u admin:password"
MAX_RETRIES=3
RETRY_DELAY=1

function call_api_with_retry() {
  local endpoint=$1
  local method=${2:-GET}
  local data=$3

  local attempt=1

  while [ $attempt -le $MAX_RETRIES ]; do
    echo "Attempt $attempt: $method $endpoint"

    if [ "$method" = "POST" ]; then
      RESPONSE=$(curl -s -w "\n%{http_code}" $AUTH -X POST \
        -H "Content-Type: application/json" \
        -d "$data" \
        "$BASE_URL$endpoint")
    else
      RESPONSE=$(curl -s -w "\n%{http_code}" $AUTH \
        "$BASE_URL$endpoint")
    fi

    # Split response and HTTP code
    HTTP_CODE=$(echo "$RESPONSE" | tail -n 1)
    BODY=$(echo "$RESPONSE" | head -n -1)

    case $HTTP_CODE in
      200|201|202)
        echo "$BODY"
        return 0
        ;;
      400)
        echo "Error: Bad request" >&2
        echo "$BODY" | jq '.' >&2
        return 1
        ;;
      401)
        echo "Error: Unauthorized. Check credentials." >&2
        return 1
        ;;
      404)
        echo "Error: Resource not found" >&2
        return 1
        ;;
      409)
        echo "Error: Conflict" >&2
        echo "$BODY" | jq '.' >&2
        return 1
        ;;
      503)
        echo "Service unavailable. Retrying in ${RETRY_DELAY}s..."
        attempt=$((attempt + 1))
        sleep $RETRY_DELAY
        RETRY_DELAY=$((RETRY_DELAY * 2))
        ;;
      *)
        echo "Error: HTTP $HTTP_CODE" >&2
        echo "$BODY" >&2
        return 1
        ;;
    esac
  done

  echo "Max retries exceeded" >&2
  return 1
}

# Usage
call_api_with_retry "/status.json"
```

### 4. Batch Processing

Process multiple cases or patterns:

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"
AUTH="-u admin:password"

# Function to process a single case
process_case() {
  local case_id=$1

  echo "Processing case: $case_id"

  # Get case details
  CASE=$(curl -s $AUTH "$BASE_URL/api/yawl/cases/$case_id")
  STATUS=$(echo "$CASE" | jq -r '.status')

  case $STATUS in
    running)
      echo "  Status: Running - $(($(echo "$CASE" | jq '.completed_tasks') + 1)) tasks completed"
      ;;
    completed)
      echo "  Status: Completed successfully"
      ;;
    failed|cancelled)
      echo "  Status: $STATUS - No further action needed"
      ;;
    *)
      echo "  Status: Unknown ($STATUS)"
      ;;
  esac
}

# Get all cases
echo "Fetching all cases..."
CASES=$(curl -s $AUTH "$BASE_URL/api/yawl/cases?limit=100")
CASE_IDS=$(echo "$CASES" | jq -r '.cases[].id')

# Process each case
echo "Processing cases..."
echo "$CASE_IDS" | while read -r case_id; do
  process_case "$case_id"
done

echo "Batch processing complete"
```

## Language-Specific Examples

### Python

```python
#!/usr/bin/env python3

import requests
import json
import time
from datetime import datetime
from requests.auth import HTTPBasicAuth

class CREClient:
    """CRE API Client"""

    def __init__(self, base_url="http://localhost:8080",
                 username="admin", password="password"):
        self.base_url = base_url
        self.auth = HTTPBasicAuth(username, password)
        self.session = requests.Session()
        self.session.auth = self.auth
        self.session.headers.update({"Content-Type": "application/json"})

    def get_health(self):
        """Check service health (no auth required)"""
        response = requests.get(f"{self.base_url}/health")
        response.raise_for_status()
        return response.json()

    def get_status(self):
        """Get CRE master status"""
        response = self.session.get(f"{self.base_url}/status.json")
        response.raise_for_status()
        return response.json()

    def list_cases(self, status=None, limit=50, offset=0):
        """List workflow cases"""
        params = {"limit": limit, "offset": offset}
        if status:
            params["status"] = status

        response = self.session.get(
            f"{self.base_url}/api/yawl/cases",
            params=params
        )
        response.raise_for_status()
        return response.json()

    def get_case(self, case_id):
        """Get case details"""
        response = self.session.get(
            f"{self.base_url}/api/yawl/cases/{case_id}"
        )
        response.raise_for_status()
        return response.json()

    def cancel_case(self, case_id, reason=None):
        """Cancel a case"""
        data = {}
        if reason:
            data["reason"] = reason

        response = self.session.post(
            f"{self.base_url}/api/yawl/cases/{case_id}/cancel",
            json=data
        )
        response.raise_for_status()
        return response.json()

    def get_specifications(self, limit=50, offset=0):
        """List workflow specifications"""
        response = self.session.get(
            f"{self.base_url}/api/yawl/specifications",
            params={"limit": limit, "offset": offset}
        )
        response.raise_for_status()
        return response.json()

    def validate_specification(self, spec_id):
        """Validate a workflow specification"""
        response = self.session.post(
            f"{self.base_url}/api/yawl/specifications/{spec_id}/validate"
        )
        response.raise_for_status()
        return response.json()

    def get_patterns(self, category="all"):
        """List YAWL patterns"""
        response = self.session.get(
            f"{self.base_url}/patterns",
            params={"category": category}
        )
        response.raise_for_status()
        return response.json()

    def health_check_loop(self, interval=30):
        """Continuous health monitoring"""
        while True:
            try:
                health = self.get_health()
                status = health.get("status")
                timestamp = datetime.now().isoformat()
                print(f"[{timestamp}] Health: {status}")

                if status != "healthy":
                    print(f"WARNING: Service is {status}")

                time.sleep(interval)
            except Exception as e:
                print(f"Error: {e}")
                time.sleep(interval)

# Usage example
if __name__ == "__main__":
    client = CREClient()

    # Check health
    print("Health Check:")
    health = client.get_health()
    print(json.dumps(health, indent=2))

    print("\n" + "="*50 + "\n")

    # Get status
    print("System Status:")
    status = client.get_status()
    print(json.dumps(status, indent=2))

    print("\n" + "="*50 + "\n")

    # List cases
    print("Listing Cases:")
    cases = client.list_cases(limit=5)
    print(f"Found {cases['total']} cases")
    for case in cases["cases"]:
        print(f"  - {case['id']}: {case['status']}")

    # Start continuous health monitoring in background
    # import threading
    # monitor = threading.Thread(target=client.health_check_loop, daemon=True)
    # monitor.start()
```

### Node.js / JavaScript

```javascript
// cre-client.js
const axios = require('axios');

class CREClient {
  constructor(baseUrl = 'http://localhost:8080', username = 'admin', password = 'password') {
    this.baseUrl = baseUrl;
    this.auth = { username, password };
    this.client = axios.create({
      baseURL: baseUrl,
      auth: this.auth,
      headers: { 'Content-Type': 'application/json' }
    });
  }

  async getHealth() {
    // Health endpoint doesn't require auth
    const response = await axios.get(`${this.baseUrl}/health`);
    return response.data;
  }

  async getStatus() {
    const response = await this.client.get('/status.json');
    return response.data;
  }

  async listCases(status = null, limit = 50, offset = 0) {
    const params = { limit, offset };
    if (status) params.status = status;

    const response = await this.client.get('/api/yawl/cases', { params });
    return response.data;
  }

  async getCase(caseId) {
    const response = await this.client.get(`/api/yawl/cases/${caseId}`);
    return response.data;
  }

  async cancelCase(caseId, reason = null) {
    const data = {};
    if (reason) data.reason = reason;

    const response = await this.client.post(
      `/api/yawl/cases/${caseId}/cancel`,
      data
    );
    return response.data;
  }

  async getSpecifications(limit = 50, offset = 0) {
    const response = await this.client.get('/api/yawl/specifications', {
      params: { limit, offset }
    });
    return response.data;
  }

  async validateSpecification(specId) {
    const response = await this.client.post(
      `/api/yawl/specifications/${specId}/validate`
    );
    return response.data;
  }

  async getPatterns(category = 'all') {
    const response = await this.client.get('/patterns', {
      params: { category }
    });
    return response.data;
  }

  async healthCheckLoop(interval = 30000) {
    while (true) {
      try {
        const health = await this.getHealth();
        console.log(`[${new Date().toISOString()}] Health: ${health.status}`);

        if (health.status !== 'healthy') {
          console.warn(`WARNING: Service is ${health.status}`);
        }
      } catch (error) {
        console.error(`Error: ${error.message}`);
      }

      await new Promise(resolve => setTimeout(resolve, interval));
    }
  }
}

// Usage example
(async () => {
  const client = new CREClient();

  try {
    // Check health
    console.log('Health Check:');
    const health = await client.getHealth();
    console.log(JSON.stringify(health, null, 2));

    console.log('\n' + '='.repeat(50) + '\n');

    // Get status
    console.log('System Status:');
    const status = await client.getStatus();
    console.log(JSON.stringify(status, null, 2));

    console.log('\n' + '='.repeat(50) + '\n');

    // List cases
    console.log('Listing Cases:');
    const cases = await client.listCases(null, 5);
    console.log(`Found ${cases.total} cases`);
    cases.cases.forEach(c => {
      console.log(`  - ${c.id}: ${c.status}`);
    });
  } catch (error) {
    console.error('Error:', error.message);
  }
})();

module.exports = CREClient;
```

### Java

```java
// CREClient.java
import okhttp3.*;
import com.google.gson.*;
import java.io.IOException;
import java.util.Base64;

public class CREClient {
    private String baseUrl;
    private OkHttpClient httpClient;
    private Gson gson;

    public CREClient(String baseUrl, String username, String password) {
        this.baseUrl = baseUrl;
        this.gson = new Gson();

        // Create HTTP client with basic auth interceptor
        String credentials = username + ":" + password;
        String encodedCredentials = Base64.getEncoder().encodeToString(
            credentials.getBytes()
        );

        this.httpClient = new OkHttpClient.Builder()
            .addInterceptor(chain -> {
                Request original = chain.request();
                Request request = original.newBuilder()
                    .header("Authorization", "Basic " + encodedCredentials)
                    .header("Content-Type", "application/json")
                    .build();
                return chain.proceed(request);
            })
            .build();
    }

    public JsonObject getHealth() throws IOException {
        Request request = new Request.Builder()
            .url(baseUrl + "/health")
            .get()
            .build();

        try (Response response = httpClient.newCall(request).execute()) {
            String body = response.body().string();
            return JsonParser.parseString(body).getAsJsonObject();
        }
    }

    public JsonObject getStatus() throws IOException {
        Request request = new Request.Builder()
            .url(baseUrl + "/status.json")
            .get()
            .build();

        try (Response response = httpClient.newCall(request).execute()) {
            String body = response.body().string();
            return JsonParser.parseString(body).getAsJsonObject();
        }
    }

    public JsonObject listCases(Integer limit, Integer offset) throws IOException {
        String url = baseUrl + "/api/yawl/cases";
        if (limit != null || offset != null) {
            StringBuilder sb = new StringBuilder(url).append("?");
            if (limit != null) sb.append("limit=").append(limit);
            if (offset != null) {
                if (limit != null) sb.append("&");
                sb.append("offset=").append(offset);
            }
            url = sb.toString();
        }

        Request request = new Request.Builder()
            .url(url)
            .get()
            .build();

        try (Response response = httpClient.newCall(request).execute()) {
            String body = response.body().string();
            return JsonParser.parseString(body).getAsJsonObject();
        }
    }

    public JsonObject getCase(String caseId) throws IOException {
        Request request = new Request.Builder()
            .url(baseUrl + "/api/yawl/cases/" + caseId)
            .get()
            .build();

        try (Response response = httpClient.newCall(request).execute()) {
            String body = response.body().string();
            return JsonParser.parseString(body).getAsJsonObject();
        }
    }

    public JsonObject cancelCase(String caseId, String reason) throws IOException {
        JsonObject data = new JsonObject();
        if (reason != null) {
            data.addProperty("reason", reason);
        }

        RequestBody body = RequestBody.create(
            data.toString(),
            MediaType.get("application/json")
        );

        Request request = new Request.Builder()
            .url(baseUrl + "/api/yawl/cases/" + caseId + "/cancel")
            .post(body)
            .build();

        try (Response response = httpClient.newCall(request).execute()) {
            String responseBody = response.body().string();
            return JsonParser.parseString(responseBody).getAsJsonObject();
        }
    }

    // Usage example
    public static void main(String[] args) {
        try {
            CREClient client = new CREClient(
                "http://localhost:8080",
                "admin",
                "password"
            );

            // Check health
            System.out.println("Health Check:");
            JsonObject health = client.getHealth();
            System.out.println(health.get("status"));

            // Get status
            System.out.println("\nSystem Status:");
            JsonObject status = client.getStatus();
            System.out.println(status);

            // List cases
            System.out.println("\nListing Cases:");
            JsonObject cases = client.listCases(5, 0);
            System.out.println(cases);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Go

```go
// cre_client.go
package main

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"time"
)

type CREClient struct {
	BaseURL  string
	Username string
	Password string
	Client   *http.Client
}

type HealthResponse struct {
	Status     string `json:"status"`
	Timestamp  int64  `json:"timestamp"`
	Subsystems []struct {
		Name    string      `json:"name"`
		Status  string      `json:"status"`
		Message string      `json:"message"`
		Details interface{} `json:"details"`
	} `json:"subsystems"`
}

type CaseInstance struct {
	ID               string `json:"id"`
	SpecificationID  string `json:"specification_id"`
	Status           string `json:"status"`
	CreatedAt        string `json:"created_at"`
	StartedAt        string `json:"started_at"`
	CompletedAt      string `json:"completed_at"`
	ActiveWorkItems  int    `json:"active_work_items"`
	CompletedTasks   int    `json:"completed_tasks"`
}

type CasesResponse struct {
	Cases  []CaseInstance `json:"cases"`
	Total  int            `json:"total"`
	Limit  int            `json:"limit"`
	Offset int            `json:"offset"`
}

func NewCREClient(baseURL, username, password string) *CREClient {
	return &CREClient{
		BaseURL:  baseURL,
		Username: username,
		Password: password,
		Client:   &http.Client{Timeout: 10 * time.Second},
	}
}

func (c *CREClient) getBasicAuth() string {
	auth := c.Username + ":" + c.Password
	return "Basic " + base64.StdEncoding.EncodeToString([]byte(auth))
}

func (c *CREClient) doRequest(method, path string) ([]byte, error) {
	url := c.BaseURL + path
	req, err := http.NewRequest(method, url, nil)
	if err != nil {
		return nil, err
	}

	req.Header.Add("Authorization", c.getBasicAuth())
	req.Header.Add("Content-Type", "application/json")

	resp, err := c.Client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode >= 400 {
		return nil, fmt.Errorf("HTTP %d: %s", resp.StatusCode, string(body))
	}

	return body, nil
}

func (c *CREClient) GetHealth() (*HealthResponse, error) {
	// Health endpoint doesn't require auth
	resp, err := http.Get(c.BaseURL + "/health")
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	var health HealthResponse
	err = json.NewDecoder(resp.Body).Decode(&health)
	return &health, err
}

func (c *CREClient) GetStatus() (map[string]interface{}, error) {
	body, err := c.doRequest("GET", "/status.json")
	if err != nil {
		return nil, err
	}

	var result map[string]interface{}
	err = json.Unmarshal(body, &result)
	return result, err
}

func (c *CREClient) ListCases(status string, limit, offset int) (*CasesResponse, error) {
	path := "/api/yawl/cases"
	query := url.Values{}
	query.Add("limit", fmt.Sprintf("%d", limit))
	query.Add("offset", fmt.Sprintf("%d", offset))
	if status != "" {
		query.Add("status", status)
	}
	if len(query) > 0 {
		path += "?" + query.Encode()
	}

	body, err := c.doRequest("GET", path)
	if err != nil {
		return nil, err
	}

	var result CasesResponse
	err = json.Unmarshal(body, &result)
	return &result, err
}

func (c *CREClient) GetCase(caseID string) (map[string]interface{}, error) {
	body, err := c.doRequest("GET", "/api/yawl/cases/"+caseID)
	if err != nil {
		return nil, err
	}

	var result map[string]interface{}
	err = json.Unmarshal(body, &result)
	return result, err
}

// Usage example
func main() {
	client := NewCREClient(
		"http://localhost:8080",
		"admin",
		"password",
	)

	// Check health
	fmt.Println("Health Check:")
	health, err := client.GetHealth()
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	fmt.Printf("Status: %s\n\n", health.Status)

	// Get status
	fmt.Println("System Status:")
	status, err := client.GetStatus()
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	fmt.Printf("%+v\n\n", status)

	// List cases
	fmt.Println("Listing Cases:")
	cases, err := client.ListCases("running", 5, 0)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	fmt.Printf("Found %d cases\n", cases.Total)
	for _, c := range cases.Cases {
		fmt.Printf("  - %s: %s\n", c.ID, c.Status)
	}
}
```

## Production Considerations

### 1. Connection Pooling

Reuse HTTP connections for better performance:

```python
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

session = requests.Session()
retry = Retry(connect=3, backoff_factor=0.5)
adapter = HTTPAdapter(max_retries=retry, pool_connections=10, pool_maxsize=10)
session.mount('http://', adapter)
session.mount('https://', adapter)
```

### 2. Timeout Configuration

Always set timeouts to prevent hanging requests:

```bash
# cURL timeout: 30 seconds
curl --max-time 30 http://localhost:8080/health

# Python timeout: connection 5s, read 30s
response = requests.get(url, timeout=(5, 30))
```

### 3. Circuit Breaker Pattern

Prevent cascading failures:

```python
from pybreaker import CircuitBreaker

breaker = CircuitBreaker(fail_max=5, reset_timeout=60)

@breaker
def call_cre_api():
    return client.get_status()

try:
    result = call_cre_api()
except Exception as e:
    # Use cached/fallback value
    logger.error(f"CRE API call failed: {e}")
```

### 4. Rate Limiting

Implement client-side rate limiting:

```python
from ratelimit import limits, sleep_and_retry

@sleep_and_retry
@limits(calls=100, period=60)
def call_api():
    return client.get_status()
```

### 5. Logging and Monitoring

Log all API interactions:

```python
import logging

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

logger = logging.getLogger('cre-client')
logger.info(f"GET {endpoint} -> {status_code}")
```

## Troubleshooting

### Service Unavailable (503)

```bash
# Check if service is running
curl -v http://localhost:8080/health

# Check logs
docker logs cre-master

# Verify connectivity
nc -zv localhost 8080
```

### Authentication Failure (401)

```bash
# Verify credentials
echo -n "admin:password" | base64

# Test with explicit header
curl -H "Authorization: Basic YWRtaW46cGFzc3dvcmQ=" \
  http://localhost:8080/status.json
```

### Timeout Issues

```bash
# Increase timeout
curl --max-time 60 http://localhost:8080/api/yawl/cases

# Check network latency
ping -c 4 localhost
```

### SSL/TLS Issues (HTTPS)

```bash
# Disable certificate verification (development only)
curl -k https://cre.example.com/health

# Verify certificate
openssl s_client -connect cre.example.com:443
```

## Support

For additional help:
- Check `/docs/api/HTTP_API_REFERENCE.md` for endpoint documentation
- Review `/docs/api/openapi.yaml` for complete specification
- Open an issue at https://github.com/joergen7/cre/issues
