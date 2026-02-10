# CRE Troubleshooting Guide

## Quick Diagnostics

### Health Check Script

```bash
# Run comprehensive health check
kubectl exec -it deployment/cre -- /app/bin/health_check.sh
```

### Common Diagnostic Commands

```bash
# Check CRE pod status
kubectl get pods -l app=cre

# View CRE logs
kubectl logs -l app=cre --tail=100 -f

# Check resource usage
kubectl top pods -l app=cre

# Describe pod for events
kubectl describe pod -l app=cre
```

## Common Issues

### Issue 1: Pods Not Starting

**Symptoms:**
- `CrashLoopBackOff` status
- Pods restart repeatedly

**Diagnosis:**
```bash
kubectl logs -l app=cre --previous
kubectl describe pod -l app=cre
```

**Causes and Solutions:**

| Cause | Solution |
|-------|----------|
| Image pull error | Check image name/tag in deployment.yaml |
| Insufficient resources | Increase CPU/memory limits |
| Missing ConfigMap | Apply configmap.yaml: `kubectl apply -f k8s/gcp/configmap.yaml` |
| Missing secrets | Create secrets via Secret Manager or external-secrets |
| Liveness probe failing | Increase `initialDelaySeconds` or adjust timeout |

**Fix - Resource Limits:**
```yaml
# Edit deployment.yaml
resources:
  requests:
    cpu: "500m"
    memory: "512Mi"
  limits:
    cpu: "1000m"
    memory: "1Gi"
```

---

### Issue 2: Workflows Stuck or Not Executing

**Symptoms:**
- Workflows remain in "running" state indefinitely
- No progress in workflow execution

**Diagnosis:**
```bash
# Check CRE logs for errors
kubectl logs -l app=cre | grep -i error

# Check Petri Net state
kubectl exec -it deployment/cre -- erl -noshell -eval "gen_pnet:status()."
```

**Causes and Solutions:**

| Cause | Solution |
|-------|----------|
| Deadlock in workflow | Review workflow for circular dependencies |
| Missing tokens | Check initial marking in workflow definition |
| Task handler timeout | Increase timeout in workflow configuration |
| Mnesia not ready | Ensure all pods are running before starting workflows |

**Fix - Check Workflow Status:**
```erlang
% In CRE Erlang shell
gen_yawl:status(WorkflowId).
gen_pnet:mode(WorkflowId).
```

---

### Issue 3: High Memory Usage

**Symptoms:**
- Pods being OOMKilled
- Memory usage at limits

**Diagnosis:**
```bash
kubectl top pods -l app=cre
kubectl logs -l app=cre | grep -i memory
```

**Causes and Solutions:**

| Cause | Solution |
|-------|----------|
| Large workflow state | Increase memory limits |
| ETS table growth | Review workflow patterns for memory leaks |
| Too many concurrent workflows | Configure HPA to scale horizontally |
| Memory leak in custom handler | Profile handler with `recon` |

**Fix - Increase Memory:**
```yaml
# Edit deployment.yaml
resources:
  limits:
    memory: "2Gi"  # Increase from 1Gi
```

**Fix - Enable HPA:**
```bash
kubectl apply -f k8s/gcp/hpa.yaml
```

---

### Issue 4: Cluster Connectivity Issues

**Symptoms:**
- Nodes cannot communicate
- EPMD connection failures
- Distributed Erlang not working

**Diagnosis:**
```bash
# Check pod-to-pod connectivity
kubectl exec -it deployment/cre -- ping <other-pod-ip>

# Check Erlang distribution
kubectl logs -l app=cre | grep -i epmd
```

**Causes and Solutions:**

| Cause | Solution |
|-------|----------|
| Firewall blocking EPMD | Add firewall rule for port 4369 |
| Firewall blocking distribution | Add rule for port range 9100-9200 |
| Network policy blocking | Update network policies to allow pod traffic |
| Wrong hostnames | Use headless service for DNS resolution |

**Fix - Network Policy:**
```yaml
# Allow CRE pods to communicate
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: cre-allow-internal
spec:
  podSelector:
    matchLabels:
      app: cre
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: cre
  egress:
  - to:
    - podSelector:
        matchLabels:
          app: cre
```

---

### Issue 5: Database Connection Failures

**Symptoms:**
- Spanner connection errors
- Mnesia table creation failures

**Diagnosis:**
```bash
# Check Spanner adapter logs
kubectl logs -l app=cre | grep -i spanner

# Test Spanner connectivity
gcloud spanner databases execute-sql <database> --instance=<instance> \
  --sql="SELECT 1"
```

**Causes and Solutions:**

| Cause | Solution |
|-------|----------|
| Wrong Spanner credentials | Update secret with correct credentials |
| Insufficient IAM permissions | Grant Cloud Spanner User role |
| Network not configured | Enable Private Service Connect or VPC access |
| Schema not created | Apply spanner_schema.sql |

**Fix - IAM Permissions:**
```bash
# Grant Workload Identity Spanner access
gcloud iam service-accounts add-iam-policy-binding \
  ${GSA_NAME}@${PROJECT_ID}.iam.gserviceaccount.com \
  --role="roles/iam.workloadIdentityUser" \
  --member="serviceAccount:${PROJECT_ID}.svc.id.goog[${NAMESPACE}/${KSA_NAME}]"

# Grant Spanner permissions
gcloud spanner databases add-iam-policy-binding \
  ${DATABASE} --instance=${INSTANCE} \
  --member="serviceAccount:${GSA_NAME}@${PROJECT_ID}.iam.gserviceaccount.com" \
  --role="roles/spanner.databaseUser"
```

---

### Issue 6: Spot VM Preemption

**Symptoms:**
- Pods terminated unexpectedly
- "Preempted" node events

**Diagnosis:**
```bash
kubectl get events | grep -i preempt
kubectl describe pod | grep -i preempt
```

**Solution:**

CRE handles Spot VM preemption gracefully:
1. Shutdown hook activates (25 second grace period)
2. In-flight workflows complete gracefully
3. State persists to Mnesia/Spanner
4. Pod reschedules on another node

**Verify Graceful Shutdown:**
```bash
# Check shutdown logs
kubectl logs -l app=cre | grep -i shutdown
```

---

## Performance Issues

### Slow Workflow Execution

**Diagnosis:**
```bash
# Check Cloud Trace for bottlenecks
gcloud trace spans list --filter="cre/*"

# Check workflow duration
kubectl exec -it deployment/cre -- \
  erl -noshell -eval "gen_yawl:stats()."
```

**Common Causes:**

| Issue | Solution |
|-------|----------|
| Synchronous task handlers | Use async patterns |
| Large workflow state | Split into smaller workflows |
| Database latency | Use Spanner stale reads |
| Network overhead | Enable regional cluster |

---

## Emergency Procedures

### Emergency Rollback

```bash
# Rollback to previous deployment
kubectl rollout undo deployment/cre

# Or rollback to specific revision
kubectl rollout history deployment/cre
kubectl rollout undo deployment/cre --to-revision=<revision>
```

### Emergency Scale Down

```bash
# Scale to zero (stop processing)
kubectl scale deployment/cre --replicas=0

# Scale back up
kubectl scale deployment/cre --replicas=3
```

### Export State Before Restart

```bash
# Export Mnesia tables
kubectl exec -it deployment/cre -- \
  /app/scripts/mnesia-export.sh /tmp/mnesia-backup

# Copy to local
kubectl cp deployment/cre:/tmp/mnesia-backup ./mnesia-backup
```

---

## Getting More Help

### Collect Diagnostic Information

```bash
# Create diagnostic bundle
kubectl logs -l app=cre > cre-logs.txt
kubectl describe pod -l app=cre > cre-pods.txt
kubectl get events > cre-events.txt
kubectl top pods -l app=cre > cre-metrics.txt
```

### Open Support Request

1. Gather diagnostic information (above)
2. Check [Known Issues Runbook](/docs/gcp/runbooks/troubleshooting.md)
3. Search [GitHub Issues](https://github.com/joergen7/cre/issues)
4. Create new issue with:
   - CRE version
   - GKE version
   - Diagnostic bundle
   - Steps to reproduce

---

## Related Documentation

| Document | Description |
|----------|-------------|
| [Deployment Runbook](/docs/gcp/runbooks/deployment.md) | Step-by-step deployment guide |
| [Rollback Runbook](/docs/gcp/runbooks/rollback.md) | Emergency rollback procedures |
| [Scaling Runbook](/docs/gcp/runbooks/scaling.md) | Horizontal and vertical scaling |
| [Backup Runbook](/docs/gcp/runbooks/backup.md) | Backup and restoration |
| [GCP Marketplace Readiness](/docs/gcp/GCP_MARKETPLACE_READINESS.md) | Full deployment documentation |
