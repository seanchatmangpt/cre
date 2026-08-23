# CRE Operations Guide

This guide covers operating CRE (Common Runtime Environment) in production on Google Cloud Platform.

## Table of Contents

- [Overview](#overview)
- [Scaling Operations](#scaling-operations)
- [Backup and Restore](#backup-and-restore)
- [Monitoring and Observability](#monitoring-and-observability)
- [Troubleshooting](#troubleshooting)
- [Maintenance Tasks](#maintenance-tasks)

---

## Overview

CRE is designed for production operation with:

- **High Availability**: 3+ replicas with automatic failover
- **Fault Tolerance**: Automatic task retry and recovery
- **Observability**: Comprehensive monitoring and logging
- **Scalability**: Horizontal and vertical autoscaling
- **Data Durability**: Automated backups and point-in-time recovery

This guide links to detailed runbooks for each operational area.

---

## Scaling Operations

CRE supports multiple scaling strategies:

### Horizontal Scaling (Add Pods)

Increase workflow throughput by adding CRE pods:

```bash
# Manual scaling
kubectl scale deployment cre -n cre --replicas=5

# Verify scaling
kubectl get pods -n cre

# Mnesia cluster automatically includes new pods
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status
```

**When to Scale Horizontally**:
- Increased workflow submission rate
- Need more concurrent workflow executions
- Higher throughput requirements

**See**: [Scaling Runbook](../../docs/gcp/runbooks/scaling.md) for detailed procedures.

### Vertical Scaling (Add Resources)

Increase CPU/memory per pod for complex workflows:

```bash
# Edit deployment
kubectl edit deployment cre -n cre

# Modify resources:
resources:
  requests:
    cpu: "4000m"      # Increase from 2000m
    memory: "8Gi"     # Increase from 4Gi
  limits:
    cpu: "8000m"
    memory: "16Gi"
```

**When to Scale Vertically**:
- Workflows with complex task logic
- Large workflow state (many tokens)
- Memory-intensive operations

### Autoscaling

Configure Horizontal Pod Autoscaler (HPA):

```bash
# Create HPA based on CPU
kubectl autoscale deployment cre \
  -n cre \
  --min=3 \
  --max=10 \
  --cpu-percent=70

# Create HPA based on custom metrics (workflow queue length)
# See: /docs/gcp/runbooks/scaling.md#custom-metrics-autoscaling
```

**See**: [Scaling Runbook](../../docs/gcp/runbooks/scaling.md) for:
- Custom metrics autoscaling
- Cluster autoscaling
- Decision matrix for scaling strategies

---

## Backup and Restore

CRE provides automated and manual backup capabilities.

### Automated Backups

Enable automated daily backups:

```bash
# Apply backup cron job
kubectl apply -f marketplace/k8s/backup-cronjob.yaml

# Verify backup job is scheduled
kubectl get cronjob -n cre

# View backup jobs
kubectl get jobs -n cre | grep backup
```

**Backup Retention**:
- Daily backups retained for 30 days
- Weekly backups retained for 12 weeks
- Monthly backups retained for 12 months

### Manual Backup

Take an immediate backup:

```bash
# Execute backup on specific pod
kubectl exec -n cre cre-0 -- /app/bin/cre backup

# Backup file is created in /var/lib/cre/backup/
kubectl exec -n cre cre-0 -- ls -lh /var/lib/cre/backup/

# Copy backup to local machine
kubectl cp -n cre cre-0:/var/lib/cre/backup/cre-backup-$(date +%Y%m%d).tar.gz ./cre-backup.tar.gz
```

### Restore from Backup

Restore CRE from a backup:

```bash
# Stop CRE pods
kubectl scale deployment cre -n cre --replicas=0

# Restore backup on each pod
kubectl exec -n cre cre-0 -- /app/bin/cre restore /var/lib/cre/backup/cre-backup-TIMESTAMP.tar.gz

# Restart pods
kubectl scale deployment cre -n cre --replicas=3

# Verify Mnesia cluster is healthy
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status
```

**See**: [Backup Runbook](../../docs/gcp/runbooks/backup.md) for:
- Complete backup procedures
- Disaster recovery strategies
- Backup verification
- Cross-region replication

---

## Monitoring and Observability

CRE integrates with Google Cloud Operations Suite for comprehensive observability.

### Metrics (Cloud Monitoring)

CRE exports metrics to Cloud Monitoring:

**Workflow Metrics**:
- `cre/workflow/queue_length` - Number of workflows waiting to execute
- `cre/workflow/execution_time` - Workflow execution duration
- `cre/workflow/throughput` - Workflows completed per second
- `cre/workflow/error_rate` - Failed workflows per second

**System Metrics**:
- `cre/cpu/usage` - CPU utilization per pod
- `cre/memory/usage` - Memory utilization per pod
- `cre/mnesia/replication_lag` - Mnesia replication delay

**View Metrics**:

```bash
# View metrics in Cloud Monitoring
gcloud monitoring dashboards create --config-from-file=marketplace/monitoring/dashboard.json

# Query metrics
gcloud monitoring time-series query \
  --format='table(point.value)' \
  "fetch cre_workflow \
   | metric 'cre/workflow/queue_length' \
   | align delta(1m)"
```

### Logging (Cloud Logging)

CRE exports structured JSON logs to Cloud Logging:

**Log Types**:
- **Application Logs**: Workflow events, errors, warnings
- **Access Logs**: HTTP API access
- **Audit Logs**: Administrative actions
- **XES Logs**: Process mining events

**View Logs**:

```bash
# Stream logs to console
kubectl logs -n cre -l app=cre --tail=100 -f

# View logs in Cloud Logging
gcloud logging read "resource.labels.container_name=cre" \
  --limit=50 \
  --format=json

# Filter logs by severity
gcloud logging read "resource.labels.container_name=cre AND severity>=ERROR" \
  --limit=50
```

### Tracing (Cloud Trace)

CRE integrates with Cloud Trace via OpenTelemetry for distributed tracing:

**View Traces**:

```bash
# View traces in Cloud Trace console
# https://console.cloud.google.com/traces

# Export trace ID from logs
kubectl logs -n cre cre-0 | grep "trace_id"

# Query specific trace
gcloud alpha trace get TRACE_ID
```

### Health Checks

CRE provides health check endpoints:

```bash
# Liveness probe (is the pod running?)
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health

# Readiness probe (is the pod ready to serve traffic?)
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/ready

# Both endpoints return:
# {
#   "status": "ok",
#   "mnesia": "connected",
#   "uptime_seconds": 123456,
#   "workflow_queue_length": 42
# }
```

**See**: [Scaling Runbook - Monitoring](../../docs/gcp/runbooks/scaling.md#monitoring) for:
- Creating custom dashboards
- Configuring alerting policies
- Setting up uptime checks

---

## Troubleshooting

This section covers common issues and solutions. For comprehensive troubleshooting, see the [Troubleshooting Runbook](../../docs/gcp/runbooks/troubleshooting.md).

### Quick Diagnostics

Run diagnostic commands:

```bash
# Check pod status
kubectl get pods -n cre -l app=cre

# Describe pod to see events
kubectl describe pod -n cre cre-0

# Check recent logs
kubectl logs -n cre cre-0 --tail=100

# Check Mnesia cluster status
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status

# Check resource usage
kubectl top pods -n cre -l app=cre
```

### Common Issues

#### Pods Not Starting

**Symptoms**: Pods stuck in `Pending` or `ImagePullBackOff`

**Diagnosis**:
```bash
kubectl describe pod -n cre cre-0
```

**Solutions**:
- Verify node has sufficient resources (`kubectl describe nodes`)
- Check image pull secrets (if using private registry)
- Verify network policies allow image pull

#### Pods Not Ready

**Symptoms**: Pods in `Running` state but not `Ready`

**Diagnosis**:
```bash
kubectl logs -n cre cre-0 --tail=100
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health
```

**Solutions**:
- Check persistent volume is mounted (`kubectl describe pvc -n cre`)
- Verify environment variables are correct
- Check Mnesia cluster connectivity

#### High Memory Usage

**Symptoms**: Pods approaching memory limit

**Diagnosis**:
```bash
kubectl top pods -n cre -l app=cre
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia info
```

**Solutions**:
- Increase memory limit (vertical scaling)
- Scale horizontally to distribute load
- Review workflows for memory leaks (large token lists)

#### Workflow Execution Slow

**Symptoms**: Workflows taking longer than expected

**Diagnosis**:
```bash
# Check workflow queue length
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/ready | jq .workflow_queue_length

# Check for CPU throttling
kubectl top pods -n cre -l app=cre

# View traces in Cloud Trace
```

**Solutions**:
- Scale horizontally to increase throughput
- Increase CPU limit (vertical scaling)
- Optimize slow task implementations

#### Mnesia Cluster Partitioned

**Symptoms**: Mnesia nodes not connected

**Diagnosis**:
```bash
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status
```

**Solutions**:
- Verify DNS resolution between pods
- Check network policies allow Erlang distribution ports (4369, 9100+)
- Restart partitioned pods

**See**: [Troubleshooting Runbook](../../docs/gcp/runbooks/troubleshooting.md) for:
- Complete troubleshooting procedures
- Escalation paths
- Known issues and workarounds

---

## Maintenance Tasks

### Rolling Updates

Update CRE with zero downtime:

```bash
# Update container image
kubectl set image deployment cre cre=ghcr.io/joergen7/cre:NEW_VERSION -n cre

# Watch rollout status
kubectl rollout status deployment cre -n cre

# If issues occur, rollback immediately
kubectl rollout undo deployment cre -n cre
```

**See**: [Upgrade Guide](../../docs/gcp/marketplace/UPGRADE.md) for complete upgrade procedures.

### Node Maintenance

Drain nodes for maintenance without downtime:

```bash
# Cordon node (no new pods scheduled)
kubectl cordon NODE_NAME

# Drain node (existing pods evicted)
kubectl drain NODE_NAME --ignore-daemonsets --delete-emptydir-data

# After maintenance, uncordon node
kubectl uncordon NODE_NAME
```

### Certificate Rotation

If using TLS certificates:

```bash
# Update certificate secret
kubectl create secret tls cre-tls \
  --cert=/path/to/tls.crt \
  --key=/path/to/tls.key \
  -n cre \
  --dry-run=client -o yaml | kubectl apply -f -

# Rolling restart to pick up new certificate
kubectl rollout restart deployment cre -n cre
```

### Log Rotation

CRE logs are automatically rotated by Kubernetes, but you can configure retention:

```bash
# Configure log retention in deployment
kubectl edit deployment cre -n cre

# Add lifecycle hook for log rotation:
lifecycle:
  preStop:
    exec:
      command: ["/bin/sh", "-c", "logrotate /etc/logrotate.conf"]
```

---

## Operational Runbooks Index

This guide provides an overview. For detailed procedures, see:

| Runbook | Purpose |
|---------|---------|
| **[Deployment Runbook](../../docs/gcp/runbooks/deployment.md)** | Initial deployment, validation, configuration |
| **[Scaling Runbook](../../docs/gcp/runbooks/scaling.md)** | Horizontal/vertical autoscaling, cluster scaling |
| **[Backup Runbook](../../docs/gcp/runbooks/backup.md)** | Automated backups, manual backups, restore procedures |
| **[Rollback Runbook](../../docs/gcp/runbooks/rollback.md)** | Rollback procedures for failed deployments |
| **[Troubleshooting Runbook](../../docs/gcp/runbooks/troubleshooting.md)** | Common issues, debugging, escalation |

### Quick Reference

| Issue | Runbook | Section |
|-------|---------|---------|
| Deploying CRE for first time | [deployment.md](../../docs/gcp/runbooks/deployment.md) | Full document |
| Pods need more resources | [scaling.md](../../docs/gcp/runbooks/scaling.md) | Vertical Scaling |
| High traffic load | [scaling.md](../../docs/gcp/runbooks/scaling.md) | Horizontal Scaling |
| Need to backup data | [backup.md](../../docs/gcp/runbooks/backup.md) | Automated Backups |
| Deployment failed | [rollback.md](../../docs/gcp/runbooks/rollback.md) | Rollback Procedures |
| Pods not starting | [troubleshooting.md](../../docs/gcp/runbooks/troubleshooting.md) | Quick Diagnostics |
| Mnesia cluster issues | [troubleshooting.md](../../docs/gcp/runbooks/troubleshooting.md) | Mnesia Issues |

---

## Incident Response Flow

```
Issue Reported
      ↓
Is it a deployment issue?
  YES → [Deployment Runbook](../../docs/gcp/runbooks/deployment.md)
  NO  ↓
Is it a scaling issue?
  YES → [Scaling Runbook](../../docs/gcp/runbooks/scaling.md)
  NO  ↓
Is it data loss/corruption?
  YES → [Backup Runbook](../../docs/gcp/runbooks/backup.md)
  NO  ↓
[Troubleshooting Runbook](../../docs/gcp/runbooks/troubleshooting.md)
```

---

## Escalation Contacts

- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Documentation**: https://github.com/joergen7/cre/blob/main/docs/
- **Support**: See [Support Terms](../../docs/gcp/marketplace/SUPPORT.md)

---

## Related Documentation

- **[Security Model](security-model.md)** - Security and compliance
- **[Cost Model](cost-model.md)** - Pricing and cost optimization
- **[Upgrade Guide](../../docs/gcp/marketplace/UPGRADE.md)** - Version upgrade procedures

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
