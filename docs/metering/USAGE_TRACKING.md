# CRE Usage Tracking

## Overview

CRE collects usage metrics to understand deployment patterns and prepare for future usage-based billing (v2). This document explains what's tracked, how it's stored, and how to access usage data.

## What's Tracked

### Metrics

CRE tracks the following usage metrics:

| Metric | Description | Purpose |
|--------|-------------|---------|
| **node_count** | Number of CRE nodes in cluster | Infrastructure sizing |
| **active_workflows** | Number of running workflows | Workload analysis |
| **memory_bytes** | Memory consumption in bytes | Resource planning |
| **cpu_utilization** | CPU usage percentage | Performance tuning |
| **workflow_hours** | Workflow execution time (v2 metering unit) | Future billing |
| **node_hours** | Node uptime (v2 metering unit) | Future billing |

### Collection Frequency

Usage metrics are collected:

- **On-demand**: Via `/usage` endpoint
- **Periodic**: Every 60 seconds (configurable via `cre_cost_reporter`)

### Data Retention

- **In-memory**: Current metrics only
- **On-disk**: Stored in `/opt/cre/data/usage/usage_metrics.jsonl`
- **Retention**: Unlimited (until v2 metering API integration)

## Accessing Usage Data

### HTTP Endpoint

```bash
curl http://cre-service.cre.svc.cluster.local:4142/usage
```

Response:
```json
{
  "usage": {
    "node_count": 3,
    "active_workflows": 5,
    "memory_bytes": 1073741824,
    "cpu_utilization_percent": 45.2,
    "workflow_hours": 5.0,
    "node_hours": 3.0
  },
  "cost": {
    "estimated_daily_cost": 7.2,
    "estimated_monthly_cost": 216.0,
    "cost_breakdown": {
      "compute": 6.0,
      "storage": 1.2
    }
  },
  "timestamp": 1737148800000
}
```

### Raw Usage Data File

```bash
# SSH into CRE pod
kubectl exec -it cre-0 -- sh

# View usage metrics file
cat /opt/cre/data/usage/usage_metrics.jsonl
```

Output (JSONL format):
```
#{<<"environment">> => <<"production">>,<<"metrics">> => #{<<"active_workflows">> => 5,<<"cpu_utilization_percent">> => 45.2,<<"memory_bytes">> => 1073741824,<<"node_count">> => 3,<<"node_hours">> => 3.0,<<"workflow_hours">> => 5.0},<<"timestamp">> => 1737148800}
#{<<"environment">> => <<"production">>,<<"metrics">> => #{<<"active_workflows">> => 7,<<"cpu_utilization_percent">> => 52.1,<<"memory_bytes">> => 2147483648,<<"node_count">> => 3,<<"node_hours">> => 3.0,<<"workflow_hours">> => 7.0},<<"timestamp">> => 1737148860}
```

### Parsing Usage Data (Python Example)

```python
import json
import re

def parse_erlang_map(line):
    """Convert Erlang map syntax to JSON"""
    # Replace Erlang map syntax with JSON
    line = line.replace('=>', ':')
    line = line.replace('<<', '"').replace('>>', '"')
    line = re.sub(r'(\w+)', r'"\1"', line)  # Quote keys
    return json.loads(line)

with open('/opt/cre/data/usage/usage_metrics.jsonl', 'r') as f:
    for line in f:
        try:
            data = parse_erlang_map(line)
            timestamp = data['timestamp']
            workflows = data['metrics']['active_workflows']
            print(f"Timestamp: {timestamp}, Workflows: {workflows}")
        except Exception as e:
            print(f"Error parsing line: {e}")
```

## Cost Estimation

### Estimated Costs

CRE provides cost estimates based on GCP pricing:

- **e2-medium**: ~$0.10/hour (~$72/month per node)
- **PD-standard SSD**: ~$0.0004/GB/hour (~$0.29/GB/month)

These are **estimates only**. Actual costs depend on:

- GCP region
- Sustained use discounts
- Committed use discounts
- Network egress
- Other GCP services

### Optimization Recommendations

CRE provides cost optimization suggestions:

```bash
curl http://cre-service.cre.svc.cluster.local:4142/usage | jq '.cost.optimization_recommendations'
```

Example recommendations:
```json
[
  {
    "type": "node_over_provision",
    "severity": "medium",
    "description": "Node count may be over-provisioned for current workload",
    "potential_suggestion": "Consider reducing nodes from 3 to 2",
    "potential_savings": "~50% compute cost reduction"
  },
  {
    "type": "idle_cluster",
    "severity": "high",
    "description": "No active workflows but nodes are running",
    "potential_suggestion": "Scale to zero or use cluster autoscaler",
    "potential_savings": "100% compute cost during idle periods"
  }
]
```

## Future: Usage-Based Billing (v2)

### Metering Units

For v2 usage-based billing, CRE will use:

- **Primary Unit**: Workflow-execution-hour (1 workflow running for 1 hour)
- **Secondary Unit**: Node-hour (1 CRE node running for 1 hour)

### Pricing (Planned)

Pricing will be determined based on v1 usage data:

- **Free Tier**: 100 workflow-hours/month
- **Pay-as-you-go**: $0.01 per workflow-hour
- **Enterprise**: Custom pricing with volume discounts

### Migration Path

When v2 usage-based billing is available:

1. Existing BYOL customers can continue on BYOL
2. Migrate to usage-based with 6-month transition period
3. Usage data collected in v1 will inform pricing structure

## Data Privacy

### What's Sent to CRE

Nothing. Usage metrics are stored locally and never sent to CRE servers.

### What's Sent to Google Cloud

In v1 (BYOL):

- Nothing. You pay GCP infrastructure costs directly.

In v2 (usage-based):

- Aggregated usage metrics to Marketplace Metering API
- No workflow data or sensitive information

### Data Ownership

You own all usage data. CRE only collects metrics for billing purposes.

## Troubleshooting

### Usage Metrics Missing

**Problem**: `/usage` endpoint returns empty data

**Solution**:

1. Check if `cre_cost_reporter` is running:
   ```bash
   kubectl logs cre-0 | grep cost_reporter
   ```
2. Verify usage data directory:
   ```bash
   kubectl exec cre-0 -- ls -la /opt/cre/data/usage
   ```
3. Restart CRE pod:
   ```bash
   kubectl delete pod cre-0
   ```

### Incorrect Node Count

**Problem**: `node_count` doesn't match actual replicas

**Solution**:

1. Check Mnesia cluster status:
   ```bash
   kubectl exec cre-0 -- erl -eval "mnesia:info()."
   ```
2. Verify pod connectivity:
   ```bash
   kubectl exec cre-0 -- ping -c 3 cre-1.cre.cre.svc.cluster.local
   ```

### High Memory Usage

**Problem**: `memory_bytes` shows unusually high usage

**Solution**:

1. Check for memory leaks:
   ```bash
   kubectl exec cre-0 -- erl -eval "erlang:memory(total)."
   ```
2. Review workflow patterns for excessive token accumulation
3. Consider increasing memory limits in Helm chart

## API Reference

### GET /usage

Returns current usage and cost metrics.

**Response Format**:
```json
{
  "usage": {
    "node_count": integer,
    "active_workflows": integer,
    "memory_bytes": integer,
    "cpu_utilization_percent": float,
    "workflow_hours": float,
    "node_hours": float
  },
  "cost": {
    "estimated_daily_cost": float,
    "estimated_monthly_cost": float,
    "cost_breakdown": {
      "compute": float,
      "storage": float
    }
  },
  "timestamp": integer
}
```

**Status Codes**:

- `200 OK`: Usage data retrieved successfully
- `503 Service Unavailable`: Cost reporter not running

## FAQ

**Q: Is my usage data sent to CRE?**

A: No, usage data is stored locally and never transmitted.

**Q: Will I be charged for usage in v1?**

A: No, v1 is BYOL. You only pay GCP infrastructure costs.

**Q: Can I delete usage data?**

A: Yes, delete `/opt/cre/data/usage/usage_metrics.jsonl`.

**Q: When will v2 usage-based billing be available?**

A: Target: Q2 2025. Sign up for updates at cre.example.com/v2.

**Q: Can I opt out of usage tracking?**

A: Usage tracking is minimal and required for v2 migration. You can disable cost reporter, but this may affect v2 transition.

**Q: How accurate are cost estimates?**

A: Estimates are based on public GCP pricing. Actual costs vary by region, discounts, and usage patterns.

**Q: Can I export usage data to external systems?**

A: Yes, read `/opt/cre/data/usage/usage_metrics.jsonl` or query `/usage` endpoint programmatically.

**Q: What format is the usage data file in?**

A: JSONL (JSON Lines) format - one JSON object per line. Erlang map syntax is used in v1.

**Q: Can I integrate usage data with my monitoring system?**

A: Yes, use the `/usage` endpoint or parse the usage metrics file directly.

**Q: How far back does usage data go?**

A: From deployment start, assuming pods weren't terminated (data stored in emptyDir).
