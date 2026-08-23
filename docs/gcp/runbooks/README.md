# CRE GCP Operational Runbooks

This directory contains operational procedures for running CRE on Google Cloud Platform.

## Runbooks

### Deployment
**[Deployment Runbook](deployment.md)**
- Initial deployment procedures
- Configuration validation
- Health check verification
- Production readiness checklist

### Scaling
**[Scaling Runbook](scaling.md)**
- Horizontal pod autoscaling (HPA)
- Vertical pod autoscaling (VPA)
- Cluster autoscaling
- Custom metrics autoscaling
- Decision matrix for scaling strategies

### Backup & Restore
**[Backup Runbook](backup.md)**
- Automated backups (cron job)
- Manual backup procedures
- Restore procedures
- Disaster recovery strategies
- Cross-region replication

### Rollback
**[Rollback Runbook](rollback.md)**
- Rolling upgrade rollback
- Blue-green deployment rollback
- Data restoration procedures
- Emergency rollback steps

### Troubleshooting
**[Troubleshooting Runbook](troubleshooting.md)**
- Common issues and solutions
- Quick diagnostics
- Mnesia cluster issues
- Performance issues
- Network problems
- Escalation procedures

## Quick Reference

| Issue | Runbook | Section |
|-------|---------|---------|
| Deploying CRE for first time | [deployment.md](deployment.md) | Full document |
| Pods need more resources | [scaling.md](scaling.md) | Vertical Scaling |
| High traffic load | [scaling.md](scaling.md) | Horizontal Scaling |
| Need to backup data | [backup.md](backup.md) | Automated Backups |
| Deployment failed | [rollback.md](rollback.md) | Rollback Procedures |
| Pods not starting | [troubleshooting.md](troubleshooting.md) | Quick Diagnostics |
| Mnesia cluster issues | [troubleshooting.md](troubleshooting.md) | Mnesia Issues |
| Workflow execution slow | [troubleshooting.md](troubleshooting.md) | Performance |

## Incident Response Flow

```
Issue Reported
      ↓
Is it a deployment issue?
  YES → [Deployment Runbook](deployment.md)
  NO  ↓
Is it a scaling issue?
  YES → [Scaling Runbook](scaling.md)
  NO  ↓
Is it data loss/corruption?
  YES → [Backup Runbook](backup.md)
  NO  ↓
[Troubleshooting Runbook](troubleshooting.md)
```

## Escalation Contacts

- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Documentation**: https://github.com/joergen7/cre/blob/main/docs/
- **Support**: See [Support Terms](../marketplace/SUPPORT.md)

## Related Documentation

- **[GCP Marketplace Readiness](../GCP_MARKETPLACE_READINESS.md)** - Technical assessment, infrastructure, security
- **[Security Whitepaper](../SECURITY_WHITEPAPER.md)** - Security architecture
- **[Deployment Guide](../../DEPLOYMENT.md)** - Comprehensive deployment guide
- **[Upgrade Guide](../marketplace/UPGRADE.md)** - Version upgrade procedures

## Running CRE in Production

### Pre-Deployment Checklist

Before deploying CRE to production, ensure:

- [ ] GKE cluster meets minimum requirements (1.25+, 3+ nodes)
- [ ] Workload Identity configured (no service account keys)
- [ ] Network policies applied (default-deny)
- [ ] Pod Security Standards enforced (restricted)
- [ ] Monitoring and alerting configured
- [ ] Automated backups scheduled
- [ ] Health checks verified
- [ ] Rollback procedure tested

### Day 2 Operations

After deploying CRE, regularly:

- **Monitor**: Check Cloud Monitoring dashboards for errors and performance
- **Scale**: Adjust replica count based on workload
- **Backup**: Verify automated backups are running
- **Update**: Apply security patches and updates
- **Review**: Audit logs for suspicious activity

### Maintenance Windows

Schedule regular maintenance windows for:

- Kubernetes node upgrades
- CRE version upgrades
- Certificate rotation
- Disaster recovery testing

## Common Commands

### Health Checks

```bash
# Check pod status
kubectl get pods -n cre -l app=cre

# Check pod health
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/ready

# Check Mnesia cluster status
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status
```

### Logs

```bash
# Stream logs from all pods
kubectl logs -n cre -l app=cre --tail=100 -f

# Check logs for errors
kubectl logs -n cre -l app=cre --tail=100 | grep -i error

# View logs in Cloud Logging
gcloud logging read "resource.labels.container_name=cre" --limit=50
```

### Scaling

```bash
# Manual scaling
kubectl scale deployment cre -n cre --replicas=5

# Check HPA status
kubectl get hpa -n cre

# Check cluster autoscaler status
kubectl get configmap -n kube-system cluster-autoscaler-status -o yaml
```

### Backup

```bash
# Execute manual backup
kubectl exec -n cre cre-0 -- /app/bin/cre backup

# List backup files
kubectl exec -n cre cre-0 -- ls -lh /var/lib/cre/backup/

# Copy backup to local machine
kubectl cp -n cre cre-0:/var/lib/cre/backup/cre-backup.tar.gz ./cre-backup.tar.gz
```

## Metrics and Monitoring

### Key Metrics to Monitor

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| **Pod Availability** | Percentage of pods in Ready state | < 66% (2 of 3 pods) |
| **Workflow Queue Length** | Number of workflows waiting | > 1000 |
| **Workflow Execution Time** | Average workflow duration | > 5 seconds |
| **Error Rate** | Failed workflows per second | > 10/sec |
| **CPU Usage** | Pod CPU utilization | > 80% |
| **Memory Usage** | Pod memory utilization | > 80% |
| **Mnesia Replication Lag** | Delay in state replication | > 1 second |

### Creating Dashboards

See [Scaling Runbook - Monitoring](scaling.md#monitoring) for dashboard creation instructions.

### Alerting

Create Cloud Monitoring alert policies for:

1. **Pod Not Ready**: Alert if < 2 pods ready for > 5 minutes
2. **High Error Rate**: Alert if error rate > 10/sec for > 5 minutes
3. **High Queue Length**: Alert if workflow queue > 1000 for > 10 minutes
4. **Mnesia Partitioned**: Alert if Mnesia nodes not connected

## Troubleshooting Tips

### Quick Diagnosis

When issues occur, run these commands first:

```bash
# 1. Check pod status
kubectl get pods -n cre -l app=cre

# 2. Check pod events
kubectl describe pod -n cre cre-0

# 3. Check recent logs
kubectl logs -n cre cre-0 --tail=100

# 4. Check Mnesia status
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status

# 5. Check resource usage
kubectl top pods -n cre -l app=cre
```

### Common Issues

| Issue | Symptom | Quick Fix |
|-------|---------|-----------|
| Pod not starting | Stuck in Pending | Check node resources |
| Pod not ready | Ready 0/1 | Check logs, health endpoints |
| High memory | OOMKilled | Increase memory limit |
| Slow execution | High latency | Scale horizontally |
| Mnesia issues | Nodes not connected | Check network policies |

For detailed troubleshooting, see [Troubleshooting Runbook](troubleshooting.md).

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
**Runbook Maintainer**: CRE Operations Team (ops@common-runtime.org)
