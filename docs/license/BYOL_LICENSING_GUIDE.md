# CRE BYOL Licensing Guide

## Overview

CRE uses a Bring Your Own License (BYOL) model for Google Cloud Marketplace deployment. This guide explains the licensing model, compliance requirements, and how to manage licenses.

## Licensing Model

### What is BYOL?

Bring Your Own License (BYOL) means you license CRE software under the Apache License 2.0. You are responsible for understanding and complying with the license terms.

### What's Included

- **Full CRE Software**: Complete workflow engine with all 36 patterns
- **No Usage Limits**: Run unlimited workflows, tasks, and nodes
- **Community Support**: Access to community forums and GitHub issues
- **Regular Updates**: Automatic updates via Marketplace

### What's NOT Included

- **No Support SLA**: Best-effort community support only
- **No Enterprise Features**: Advanced features require enterprise license
- **No Usage-Based Billing**: You pay GCP infrastructure costs only

## EULA Acceptance

### During Marketplace Deployment

When deploying CRE from Google Cloud Marketplace, you must accept the End User License Agreement (EULA):

1. Navigate to CRE listing in Google Cloud Marketplace
2. Click "Configure" to start deployment
3. Set `license.acceptEula = true` in the configuration
4. Complete deployment

### Grace Period

If you don't accept the EULA during deployment, CRE enters a 30-day grace period:

- **Days 1-30**: CRE runs normally with warnings
- **After Day 30**: CRE stops accepting new workflows

### Accepting EULA After Deployment

If you're in the grace period, you can accept the EULA at any time:

```bash
# Access CRE pod
kubectl exec -it cre-0 -- sh

# Accept EULA via Erlang console
erl -eval "license_enforcer:accept_eula(<<\"your-name@company.com\">>)"
```

## License Validation

### Startup Validation

CRE validates the license at startup. If the license is invalid:

- Kubernetes startup probe fails
- Pod enters `CrashLoopBackOff` state
- You must accept the EULA to proceed

### Runtime Validation

CRE periodically checks license status during operation. If the license expires:

- New workflows are rejected
- Existing workflows complete
- Warning messages appear in logs

## License Status

### Check License Status

Use the `/startup` endpoint to check license status:

```bash
curl http://cre-service.cre.svc.cluster.local:4142/startup
```

Response (valid license):
```json
{
  "status": "healthy",
  "timestamp": 1737148800000,
  "subsystems": [
    {
      "name": "license",
      "status": "healthy",
      "message": "License valid",
      "details": {
        "eula_accepted": true,
        "eula_version": "1.0"
      }
    }
  ]
}
```

Response (grace period):
```json
{
  "status": "healthy",
  "timestamp": 1737148800000,
  "subsystems": [
    {
      "name": "license",
      "status": "healthy",
      "message": "License in grace period, 15 days remaining",
      "details": {
        "grace_period_days_remaining": 15,
        "eula_accepted": false,
        "action": "Accept EULA to avoid service interruption"
      }
    }
  ]
}
```

## Usage Tracking

### What's Tracked

CRE collects the following usage metrics for future usage-based billing (v2):

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

### View Usage Metrics

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

### Usage Data Storage

Usage metrics are stored locally at `/opt/cre/data/usage/usage_metrics.jsonl` for future migration to usage-based billing (v2).

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

## Compliance

### Apache License 2.0

CRE is licensed under the Apache License 2.0. Key points:

- ✅ Commercial use allowed
- ✅ Modification allowed
- ✅ Distribution allowed
- ✅ Private use allowed
- ⚠️ License and copyright notice required
- ❌ No warranty provided

### Marketplace Terms

By deploying CRE from Google Cloud Marketplace, you agree to:

- Use CRE in compliance with Apache License 2.0
- Manage your own deployment and upgrades
- Rely on community support (no SLA)

## Support

### Community Support

- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Documentation**: https://github.com/joergen7/cre/blob/main/docs
- **Community Forum**: [Link to forum]

### Enterprise Support

For enterprise-grade support with SLA, contact us at:

- Email: enterprise@example.com
- Website: https://cre.example.com/enterprise

## Troubleshooting

### License Validation Fails

**Problem**: Pod fails to start with license error

**Solution**:

1. Check startup probe: `kubectl logs cre-0 | grep license`
2. Accept EULA: Set `license.acceptEula=true` in Marketplace UI
3. Redeploy CRE

### Grace Period Expiring

**Problem**: Warning about grace period expiration

**Solution**:

1. Accept EULA before grace period expires
2. Restart pods after accepting EULA

### Usage Metrics Missing

**Problem**: `/usage` endpoint returns empty data

**Solution**:

1. Check if `cre_cost_reporter` is running: `kubectl logs cre-0 | grep cost_reporter`
2. Verify usage data directory exists: `kubectl exec cre-0 -- ls -la /opt/cre/data/usage`
3. Restart CRE pod

## Migration to Usage-Based Billing (v2)

CRE will offer usage-based billing in v2. Current usage metrics are collected to:

1. Understand usage patterns
2. Define appropriate metering units
3. Enable smooth migration from BYOL to usage-based

You'll be able to migrate to usage-based billing without losing data or functionality.

## FAQ

**Q: Do I need a license key?**

A: No, BYOL doesn't require license keys. Just accept the EULA.

**Q: Can I use CRE in production?**

A: Yes, CRE is production-ready under Apache License 2.0.

**Q: Is there a limit on workflows or nodes?**

A: No, CRE has no usage limits in BYOL model.

**Q: What happens if I don't accept the EULA?**

A: CRE enters a 30-day grace period, then stops accepting new workflows.

**Q: Can I upgrade to enterprise support later?**

A: Yes, contact enterprise@example.com for enterprise license options.

**Q: Will my usage data be shared with CRE?**

A: No, usage data is stored locally and never transmitted to CRE servers.

**Q: When will v2 usage-based billing be available?**

A: Target: Q2 2025. Sign up for updates at cre.example.com/v2.
