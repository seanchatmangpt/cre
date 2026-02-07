# Client SDK Implementation Guide

## Overview

This guide provides comprehensive documentation for implementing and using the CRE client SDKs in Python, Go, and Node.js.

## Table of Contents

1. [Python SDK](#python-sdk)
2. [Go SDK](#go-sdk)
3. [Node.js SDK](#nodejs-sdk)
4. [Common Patterns](#common-patterns)
5. [Error Handling](#error-handling)
6. [Performance Tips](#performance-tips)

---

## Python SDK

### Installation

```bash
pip install requests
```

### Basic Usage

```python
from python_sdk import CREMasterClient, APIException

# Create client
client = CREMasterClient("http://localhost:4142", timeout=30)

# Get status
try:
    status = client.get_status()
    print(f"Active workers: {status['busy_count']}")
    print(f"Idle workers: {status['idle_count']}")
    print(f"Pending jobs: {status['queue_length']}")
except APIException as e:
    print(f"Error: {e}")
finally:
    client.close()
```

### Using Context Manager

```python
from python_sdk import CREMasterClient

with CREMasterClient() as client:
    status = client.get_status()
    history = client.get_history()

# Connection automatically closed
```

### Data Models

```python
from python_sdk import CacheEntry, ApplicationStatus, WorkerStatus

# Parse cache entries
history = client.get_history()
for entry_data in history.get('entries', []):
    entry = CacheEntry.from_dict(entry_data)
    print(f"App {entry.app_id}: {entry.execution_time_ms}ms")
```

### Monitoring Pattern

```python
import time
from python_sdk import CREMasterClient

def monitor_cre():
    client = CREMasterClient()

    while True:
        try:
            status = client.get_status()

            if status['queue_length'] > 10:
                print(f"WARNING: Queue backed up ({status['queue_length']} pending)")

            if status['idle_count'] == 0 and status['busy_count'] > 0:
                print("WARNING: No idle workers available")

            time.sleep(5)
        except Exception as e:
            print(f"Monitor error: {e}")
            time.sleep(10)

if __name__ == "__main__":
    monitor_cre()
```

### Batch Operations

```python
from python_sdk import CREMasterClient

client = CREMasterClient()

# Collect multiple status readings
readings = []
for i in range(10):
    status = client.get_status()
    readings.append({
        'timestamp': status['timestamp'],
        'idle_count': status['idle_count'],
        'queue_length': status['queue_length']
    })
    time.sleep(1)

# Analyze
avg_queue = sum(r['queue_length'] for r in readings) / len(readings)
print(f"Average queue length: {avg_queue:.1f}")
```

---

## Go SDK

### Installation

```bash
# Copy go_sdk.go to your project
# Import in your code:
import "your-module/creapi"
```

### Basic Usage

```go
package main

import (
    "context"
    "fmt"
    "log"
    "time"

    "your-module/creapi"
)

func main() {
    client := creapi.NewClient("http://localhost:4142")

    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()

    status, err := client.GetStatus(ctx)
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Busy workers: %d\n", status.BusyCount)
    fmt.Printf("Idle workers: %d\n", status.IdleCount)
    fmt.Printf("Pending queue: %d\n", status.QueueLength)
}
```

### Error Handling

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

    status, err := client.GetStatus(context.Background())
    if err != nil {
        if apiErr, ok := err.(*creapi.APIError); ok {
            log.Printf("API Error: %s\nMessage: %s\n", apiErr.Error, apiErr.Message)
        } else {
            log.Printf("Network error: %v\n", err)
        }
        return
    }

    fmt.Printf("Status: %+v\n", status)
}
```

### Status Monitoring

```go
package main

import (
    "context"
    "fmt"
    "log"
    "time"

    "your-module/creapi"
)

func main() {
    client := creapi.NewClient("http://localhost:4142")

    // Create monitor with 5 second interval
    monitor := client.NewStatusMonitor(5 * time.Second)
    monitor.Start()

    go func() {
        ticker := time.NewTicker(30 * time.Second)
        defer ticker.Stop()

        for {
            select {
            case status := <-monitor.StatusCh:
                fmt.Printf("[%s] Idle: %d, Busy: %d, Queue: %d\n",
                    status.Timestamp, status.IdleCount, status.BusyCount, status.QueueLength)

            case err := <-monitor.ErrorCh:
                log.Printf("Monitor error: %v\n", err)

            case <-ticker.C:
                monitor.Stop()
                return
            }
        }
    }()

    // Keep main alive
    select {}
}
```

### Custom HTTP Client

```go
package main

import (
    "context"
    "net/http"
    "time"

    "your-module/creapi"
)

func main() {
    client := creapi.NewClient("http://localhost:4142")

    // Customize HTTP client
    client.HTTPClient = &http.Client{
        Timeout: 60 * time.Second,
        Transport: &http.Transport{
            MaxIdleConns:        100,
            MaxIdleConnsPerHost: 10,
        },
    }

    status, _ := client.GetStatus(context.Background())
    // ... use status
}
```

---

## Node.js SDK

### Installation

```bash
npm install node-fetch
```

### Basic Usage

```javascript
const CREClient = require('./nodejs_sdk');

const client = new CREClient('http://localhost:4142', 30000);

client.getStatus()
    .then(status => {
        console.log(`Idle: ${status.idle_count}`);
        console.log(`Busy: ${status.busy_count}`);
        console.log(`Queue: ${status.queue_length}`);
    })
    .catch(error => {
        console.error('Error:', error);
    });
```

### Async/Await Pattern

```javascript
const CREClient = require('./nodejs_sdk');
const { APIError } = require('./nodejs_sdk');

async function main() {
    const client = new CREClient('http://localhost:4142');

    try {
        const status = await client.getStatus();
        const history = await client.getHistory();

        console.log(`Workers: ${status.idle_count} idle, ${status.busy_count} busy`);
        console.log(`Cache: ${(history.hit_rate * 100).toFixed(1)}% hit rate`);

    } catch (error) {
        if (error instanceof APIError) {
            console.error(`API Error (${error.statusCode}):`, error.message);
        } else {
            console.error('Error:', error);
        }
    }
}

main();
```

### YAWL Dashboard Integration

```javascript
const { YAWLDashboardClient } = require('./nodejs_sdk');

const yawlClient = new YAWLDashboardClient('http://localhost:8081');

(async () => {
    try {
        // Get recent traces
        const traces = await yawlClient.listTraces({
            limit: 10,
            status: 'completed'
        });

        console.log(`Found ${traces.total} completed traces`);
        traces.traces.forEach(trace => {
            console.log(`- ${trace.trace_id}: ${trace.duration_ms}ms`);
        });

        // Get statistics
        const stats = await yawlClient.getStatistics({
            time_window: '24h'
        });

        console.log(`Success rate: ${(stats.success_rate * 100).toFixed(1)}%`);
        console.log(`Throughput: ${stats.throughput_per_minute.toFixed(2)} exec/min`);

    } catch (error) {
        console.error('Error:', error);
    }
})();
```

### Continuous Monitoring

```javascript
const CREClient = require('./nodejs_sdk');

const client = new CREClient('http://localhost:4142');

// Monitor status with callback
const stopMonitoring = client.monitorStatus(5000, (status) => {
    if (status.queue_length > 10) {
        console.warn(`⚠️ Queue backup: ${status.queue_length} pending`);
    }

    if (status.idle_count === 0 && status.busy_count > 0) {
        console.warn('⚠️ No idle workers available');
    }

    console.log(`✓ Status: ${status.idle_count} idle, ${status.busy_count} busy`);
});

// Stop after 5 minutes
setTimeout(stopMonitoring, 5 * 60 * 1000);
```

---

## Common Patterns

### Polling Strategy

```python
# Python example
import time
from python_sdk import CREMasterClient

client = CREMasterClient()
last_alert_time = 0
alert_cooldown = 300  # 5 minutes

while True:
    try:
        status = client.get_status()

        # Only alert once every 5 minutes
        if status['queue_length'] > 20:
            current_time = time.time()
            if current_time - last_alert_time > alert_cooldown:
                print(f"ALERT: Queue length {status['queue_length']}")
                last_alert_time = current_time

        time.sleep(10)

    except Exception as e:
        print(f"Error: {e}")
        time.sleep(30)  # Back off on error
```

### Exponential Backoff Retry

```javascript
// Node.js example
async function fetchWithRetry(asyncFn, maxRetries = 3) {
    let lastError;
    let delay = 1000; // Start with 1 second

    for (let attempt = 0; attempt < maxRetries; attempt++) {
        try {
            return await asyncFn();
        } catch (error) {
            lastError = error;
            console.log(`Attempt ${attempt + 1} failed, retrying in ${delay}ms...`);
            await new Promise(resolve => setTimeout(resolve, delay));
            delay *= 2; // Exponential backoff
        }
    }

    throw lastError;
}

// Usage
const client = new CREClient();
const status = await fetchWithRetry(() => client.getStatus());
```

### Health Check

```go
// Go example
func healthCheck(client *creapi.Client) error {
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()

    status, err := client.GetStatus(ctx)
    if err != nil {
        return fmt.Errorf("health check failed: %w", err)
    }

    if status.WorkerCount == 0 {
        return fmt.Errorf("no workers available")
    }

    if status.BusyCount == status.WorkerCount {
        return fmt.Errorf("all workers are busy")
    }

    return nil
}
```

---

## Error Handling

### Graceful Degradation

```python
from python_sdk import CREMasterClient, APIException

def get_cre_status_safe(timeout=5):
    """Get status with fallback"""
    try:
        client = CREMasterClient(timeout=timeout)
        return client.get_status()
    except APIException as e:
        # Return cached or default status
        return {
            'idle_count': -1,
            'busy_count': -1,
            'queue_length': -1,
            'error': str(e)
        }
```

### Circuit Breaker Pattern

```javascript
class CREClientWithCircuitBreaker {
    constructor() {
        this.client = new CREClient();
        this.failureCount = 0;
        this.failureThreshold = 5;
        this.isCircuitOpen = false;
        this.resetTimeout = 60000; // 1 minute
    }

    async getStatus() {
        if (this.isCircuitOpen) {
            throw new Error('Circuit breaker is open');
        }

        try {
            const result = await this.client.getStatus();
            this.failureCount = 0;
            return result;
        } catch (error) {
            this.failureCount++;

            if (this.failureCount >= this.failureThreshold) {
                this.isCircuitOpen = true;
                setTimeout(() => {
                    this.isCircuitOpen = false;
                    this.failureCount = 0;
                }, this.resetTimeout);

                throw new Error('Circuit breaker opened after multiple failures');
            }

            throw error;
        }
    }
}
```

---

## Performance Tips

### Connection Pooling

**Python:**
```python
from python_sdk import CREMasterClient

# Reuse single client instance
client = CREMasterClient()

for i in range(100):
    status = client.get_status()
    # Process...

client.close()
```

### Response Caching

```python
import time
from functools import wraps

def cache_response(ttl=60):
    def decorator(func):
        cache = {'result': None, 'timestamp': 0}

        def wrapper(*args, **kwargs):
            current_time = time.time()
            if current_time - cache['timestamp'] < ttl:
                return cache['result']

            cache['result'] = func(*args, **kwargs)
            cache['timestamp'] = current_time
            return cache['result']

        return wrapper
    return decorator

@cache_response(ttl=5)
def get_cre_status():
    client = CREMasterClient()
    return client.get_status()
```

### Batch Operations

```go
// Go example - batch multiple requests
func batchGetStatus(client *creapi.Client, count int) []*creapi.CREStatus {
    statuses := make([]*creapi.CREStatus, count)
    ctx := context.Background()

    for i := 0; i < count; i++ {
        status, _ := client.GetStatus(ctx)
        statuses[i] = status
    }

    return statuses
}
```

---

## Best Practices

1. **Always handle errors** - Don't ignore API exceptions
2. **Use context managers** (Python) or `defer` (Go) for cleanup
3. **Set reasonable timeouts** - Prevent indefinite hangs
4. **Implement retries** - Use exponential backoff for transient failures
5. **Monitor rate limits** - Respect API rate limiting
6. **Log important events** - Track API calls and errors
7. **Use connection pooling** - Reuse client instances
8. **Cache responses** - Reduce unnecessary API calls
9. **Implement health checks** - Verify API availability
10. **Test error cases** - Ensure graceful degradation

---

## See Also

- [API Reference](./API-REFERENCE.md)
- [OpenAPI Specifications](./openapi-cre-master.yaml)
- [SDK README](../sdks/README.md)
