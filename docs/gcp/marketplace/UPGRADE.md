# CRE Upgrade Guide

## Overview

This guide explains how to upgrade CRE between versions when deployed from Google Cloud Marketplace.

**Current Version**: 0.3.0
**Minimum OTP Version**: 25.0
**Tested on**: OTP 25, 26, 27, 28

This guide covers upgrade strategies, procedures, rollback processes, and validation steps for CRE deployments on Google Kubernetes Engine (GKE) via Google Cloud Marketplace.

---

## Upgrade Strategies

CRE supports multiple upgrade strategies depending on your availability requirements and cluster size.

### Strategy 1: Rolling Upgrade (Recommended)

For CRE deployments with **3+ nodes**, use rolling upgrades to maintain availability during upgrades.

**Prerequisites**:
- CRE cluster with 3+ nodes (recommended for production)
- Persistent volumes configured for Mnesia data
- Health checks configured (`/health` and `/ready` endpoints)
- Pre-upgrade backup completed
- Pod Disruption Budget configured (min 2 pods available)

**Procedure**:

1. **Take Backup**:
   ```bash
   # Execute backup before starting upgrade
   kubectl exec -n cre cre-0 -- /app/bin/cre backup

   # Verify backup completed successfully
   kubectl exec -n cre cre-0 -- ls -lh /cre/backup/
   ```

   See [Backup Runbook](../../runbooks/backup.md) for detailed backup procedures.

2. **Verify Current Health**:
   ```bash
   # Check all pods are healthy
   kubectl get pods -n cre -l app=cre

   # Verify health endpoints
   kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health
   kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/ready

   # Check Mnesia cluster status
   kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status
   ```

3. **Update Container Image**:
   ```bash
   # Update the deployment to use new image version
   kubectl set image deployment/cre cre=ghcr.io/joergen7/cre:NEW_VERSION -n cre

   # Or edit the deployment directly
   kubectl edit deployment cre -n cre
   # Change: image: ghcr.io/joergen7/cre:NEW_VERSION
   ```

4. **Monitor Rollout**:
   ```bash
   # Watch rollout status in real-time
   kubectl rollout status deployment/cre -n cre

   # Watch pod status
   kubectl get pods -n cre -l app=cre -w
   ```

5. **Verify Upgrade Success**:
   ```bash
   # Check all pods are Running and Ready
   kubectl get pods -n cre -l app=cre

   # Run health checks on new pods
   kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health
   kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/ready

   # Verify Mnesia cluster is connected
   kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status

   # Check application logs for errors
   kubectl logs -n cre -l app=cre --tail=100 | grep -i error
   ```

6. **Post-Upgrade Validation**:
   - Verify workflow executions are processing correctly
   - Check monitoring dashboards for error spikes
   - Validate OpenTelemetry telemetry is being exported
   - Confirm no performance regressions

**Downtime**: Zero (for 3+ node clusters with proper PDB)

**Advantages**:
- Zero downtime if cluster is healthy
- Easy rollback if issues occur
- Gradual rollout allows early issue detection

**Disadvantages**:
- Requires 3+ nodes for true zero downtime
- Requires pre-upgrade backup
- Health checks must be properly configured

---

### Strategy 2: Blue-Green Upgrade

For critical production environments requiring **maximum safety** and isolation.

**Prerequisites**:
- Sufficient GKE cluster capacity for 2x CRE pods
- Separate namespace for blue-green deployment
- Load balancer with traffic switching capability
- Backup completed before upgrade

**Procedure**:

1. **Deploy New Version**:
   ```bash
   # Create new namespace for new version
   kubectl create namespace cre-green

   # Deploy CRE new version to cre-green namespace
   # Follow deployment guide in marketplace/listing-package/deployment-guide.md
   ```

2. **Migrate Data** (if required):
   ```bash
   # For major version upgrades requiring data migration:
   # Follow migration procedures for Mnesia schema changes
   # See backup runbook for data export/import procedures

   # Export data from old cluster
   kubectl exec -n cre cre-0 -- /app/bin/cre backup

   # Import data to new cluster
   kubectl cp /tmp/cre-backup.tar.gz cre-green/cre-0:/tmp/backup.tar.gz
   kubectl exec -n cre-green cre-0 -- /app/bin/cre restore /tmp/backup.tar.gz
   ```

3. **Verify New Deployment**:
   ```bash
   # Run smoke tests on new deployment
   kubectl exec -n cre-green cre-0 -- curl -f http://localhost:4142/health

   # Verify Mnesia cluster is healthy
   kubectl exec -n cre-green cre-0 -- /app/bin/cre mnesia status
   ```

4. **Switch Traffic**:
   ```bash
   # Update load balancer/ingress to point to cre-green namespace
   kubectl patch ingress cre-ingress -n cre -p '{"spec":{"rules":[{"host":"cre.example.com","http":{"paths":[{"path":"/","backend":{"serviceName":"cre-service-green","servicePort":4142}}]}}]}}'

   # Or update Service selector (if using service mesh)
   kubectl patch svc cre-service -n cre -p '{"spec":{"selector":{"app":"cre","version":"green"}}}'
   ```

5. **Monitor for 24 Hours**:
   - Check all health metrics
   - Monitor error rates
   - Validate workflow processing
   - Confirm no data inconsistencies

6. **Decommission Old Deployment**:
   ```bash
   # After 24 hours of successful operation:
   kubectl delete namespace cre
   ```

**Downtime**: Minimal (only during traffic switch, typically < 30 seconds)

**Advantages**:
- Maximum isolation between versions
- Easy rollback (switch traffic back)
- Extended validation window before cutover

**Disadvantages**:
- Requires 2x infrastructure capacity
- More complex traffic management
- Longer deployment timeline

---

### Strategy 3: Recreate Upgrade (Not Recommended)

Terminates all pods and starts new version. **Not recommended for production** due to downtime.

```bash
# ⚠️ USE ONLY IN NON-PRODUCTION ENVIRONMENTS
kubectl delete pods -n cre -l app=cre
# Kubernetes Deployment controller will recreate pods with new image
```

**Downtime**: Significant (5-10 minutes depending on startup time)

**When to Use**: Development/test environments only

---

## Why Hot Code Loading is Not Recommended

Erlang/OTP supports **hot code loading**, which allows code upgrades without restarting the VM. However, hot code loading is **NOT recommended** for CRE Marketplace deployments.

### Reasons to Avoid Hot Code Loading:

1. **Containerized Deployments**: CRE runs in containers where hot-loaded code is lost on container restart
2. **State Synchronization**: Hot code loading requires careful state conversion; errors can corrupt workflow state
3. **Rollback Complexity**: Rolling back hot-loaded code is complex and error-prone
4. **Not Tested**: CRE has not been tested with hot code loading in Marketplace environment
5. **Deployment Model**: GKE Marketplace uses immutable container images, not live code patching

### Use Instead:
- **Rolling upgrades** (recommended for production)
- **Blue-green deployments** (for critical environments)

---

## Version Compatibility Matrix

| From Version | To Version | OTP Compatibility | Data Migration | Rollback Strategy | Notes |
|--------------|------------|-------------------|----------------|-------------------|-------|
| 0.3.0 | 0.4.0 | OTP 25-28 | Automatic | Immediate rollback | Minor version upgrade |
| 0.2.x | 0.3.0 | OTP 25-28 | Manual review required | Backup restore | Major version upgrade |
| 0.1.x | 0.3.0 | OTP 25-28 | Not supported | N/A | Skip intermediate versions |

### OTP Version Compatibility

| CRE Version | Minimum OTP | Tested OTP | Maximum OTP |
|-------------|-------------|------------|-------------|
| 0.3.0 | 25.0 | 25, 26, 27, 28 | 28.x |

**Note**: Always test upgrades in staging environment before production deployment, especially when upgrading OTP versions.

---

## Rollback Procedures

### Immediate Rollback (Rolling Upgrade)

If upgrade fails or issues are detected, immediately rollback:

```bash
# Rollback to previous deployment
kubectl rollout undo deployment/cre -n cre

# Verify rollback completed
kubectl rollout status deployment/cre -n cre

# Check pods are healthy
kubectl get pods -n cre -l app=cre

# Verify health endpoints
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health
```

### Data Restoration (If Data Corruption Occurred)

If upgrade caused data corruption:

```bash
# 1. Identify the backup to restore from
kubectl exec -n cre cre-0 -- ls -lh /cre/backup/

# 2. Stop CRE pods to prevent writes
kubectl scale deployment cre -n cre --replicas=0

# 3. Restore backup on each pod
kubectl exec -n cre cre-0 -- /app/bin/cre restore /cre/backup/cre-backup-TIMESTAMP.tar.gz

# 4. Restart pods
kubectl scale deployment cre -n cre --replicas=3

# 5. Verify Mnesia cluster is healthy
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status
```

See [Backup Runbook](../../runbooks/backup.md#restore-procedures) for detailed restore procedures.

### Blue-Green Rollback

If using blue-green strategy, rollback is simple:

```bash
# Switch traffic back to old deployment
kubectl patch ingress cre-ingress -n cre-green -p '{"spec":{"rules":[{"host":"cre.example.com","http":{"paths":[{"path":"/","backend":{"serviceName":"cre-service","servicePort":4142,"namespace":"cre"}}]}}]}}'
```

---

## Pre-Upgrade Checklist

Complete all items before starting upgrade:

- [ ] **Backup Completed**: Full backup taken and verified
- [ ] **Staging Test**: New version tested in staging environment
- [ ] **Rollback Tested**: Rollback procedure tested in staging
- [ ] **Sufficient Capacity**: Cluster has capacity for additional pods during rollout
- [ ] **Monitoring Configured**: Alerting configured for error rates, latency, pod health
- [ ] **Maintenance Window**: Maintenance window scheduled (if required)
- [ ] **Documentation Reviewed**: Read upgrade notes in release changelog
- [ ] **Team Notified**: Engineering team notified of upgrade
- [ ] **Health Checks Verified**: `/health` and `/ready` endpoints functioning
- [ ] **Mnesia Healthy**: All Mnesia nodes connected and synchronized
- [ ] **Network Policies**: Verified network policies allow pod communication
- [ ] **Resource Limits**: Verified CPU/memory limits are sufficient for new version

---

## Post-Upgrade Validation

Validate all items after upgrade completes:

- [ ] **Pods Healthy**: All pods in `Running` state with `1/1 Ready`
- [ ] **Health Checks Passing**: `/health` and `/ready` endpoints return 200 OK
- [ ] **Mnesia Connected**: All Mnesia nodes connected (run `mnesia status`)
- [ ] **Workflows Executing**: Verify workflows are being processed
- [ ] **No Error Spikes**: Check logs for error spikes (Cloud Logging)
- [ ] **Metrics Normal**: Check metrics for anomalies (Cloud Monitoring)
- [ ] **Telemetry Exporting**: Verify OpenTelemetry traces are being exported
- [ ] **API Endpoints Working**: Test REST API endpoints
- [ ] **Performance Baseline**: No performance regression compared to pre-upgrade
- [ ] **Smoke Tests Passed**: Run smoke test suite for critical workflows

---

## Known Issues

### OTP Version Upgrades

When upgrading OTP versions (e.g., OTP 26 → 27):

**Risks**:
- Dependency compatibility issues
- Performance changes (JIT compiler differences)
- NIF compatibility (Rust NIFs must be recompiled)

**Mitigation**:
- Test thoroughly in staging for 48+ hours
- Verify all dependencies support new OTP version
- Check `rebar.config` overrides for compatibility
- Monitor for performance regressions (latency, throughput)
- Prepare rollback plan

**Procedure**:
1. Deploy new OTP version in staging
2. Run full test suite
3. Load test with production-like traffic
4. Monitor for 24 hours
5. Upgrade production following rolling upgrade procedure

### Mnesia Schema Changes

Major version upgrades may include Mnesia schema changes:

**Risks**:
- Data corruption if migration fails
- Extended downtime during migration
- Incompatible schema versions

**Mitigation**:
- **ALWAYS** backup before schema-changing upgrades
- Review release notes for schema changes
- Test migration in staging first
- Plan for extended downtime (may be required)

**Procedure**:
1. Review changelog for schema changes
2. Test migration in staging with production data copy
3. Schedule maintenance window for production
4. Take backup immediately before upgrade
5. Follow migration guide (if provided)
6. Validate data integrity after migration
7. Monitor for data inconsistencies

### Network Policy Changes

If CRE networking configuration changes:

**Risks**:
- Pods cannot communicate with each other
- External services cannot reach CRE API
- Mnesia cluster formation fails

**Mitigation**:
- Review network policy changes in release notes
- Test network policies in staging first
- Verify pod-to-pod communication
- Verify service connectivity

---

## Troubleshooting

### Upgrade Fails to Start

**Symptoms**: Pods stuck in `Pending` or `ImagePullBackOff` state

**Diagnosis**:
```bash
# Describe pod to see events
kubectl describe pod -n cre cre-XXXX

# Check deployment events
kubectl describe deployment cre -n cre
```

**Solutions**:
- Verify container image exists in registry
- Check image pull secrets are configured
- Verify GKE node has sufficient resources
- Check network policies allow image pull

### Pods Not Becoming Ready

**Symptoms**: Pods in `Running` state but not `Ready`

**Diagnosis**:
```bash
# Check pod logs
kubectl logs -n cre cre-XXXX --tail=100

# Check pod events
kubectl describe pod -n cre cre-XXXX
```

**Solutions**:
- Verify health check endpoints are responding
- Check for application startup errors in logs
- Verify environment variables are configured correctly
- Check resource limits (CPU/memory)
- Verify persistent volumes are mounted

### Mnesia Cluster Issues

**Symptoms**: Mnesia nodes not connecting, data inconsistencies

**Diagnosis**:
```bash
# Check Mnesia status
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status

# Check Mnesia logs
kubectl logs -n cre cre-0 | grep -i mnesia
```

**Solutions**:
- Verify DNS resolution between pods
- Check network policies allow Erlang distribution ports
- Verify all pods can communicate on EPMD port (4369) and distribution ports
- Consider restoring from backup if cluster is corrupted

See [Troubleshooting Runbook](../../runbooks/troubleshooting.md) for detailed troubleshooting procedures.

---

## Getting Help

### Documentation

- **[Deployment Guide](../../../DEPLOYMENT.md)** - Comprehensive deployment procedures
- **[Backup Runbook](../../runbooks/backup.md)** - Backup and restore procedures
- **[Scaling Runbook](../../runbooks/scaling.md)** - Scaling operations
- **[Troubleshooting Runbook](../../runbooks/troubleshooting.md)** - Common issues and solutions

### Support

- **[Support Terms](SUPPORT.md)** - Support scope, SLA, contacts
- **[GitHub Issues](https://github.com/joergen7/cre/issues)** - Bug reports and feature requests
- **[Documentation](https://github.com/joergen7/cre/blob/main/docs/)** - Full documentation

### Emergency Contacts

For critical production issues:
- Review [Support Terms](SUPPORT.md) for escalation procedures
- Check GitHub Issues for known upgrade issues
- Contact support via GitHub Issues with **UPGRADE** tag

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
**Next Review**: 2025-02-10
