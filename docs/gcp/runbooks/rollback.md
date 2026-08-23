# CRE GCP Rollback Runbook

**Procedures for rolling back CRE deployments on Google Cloud Platform.**

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Rollback Decision Matrix](#rollback-decision-matrix)
3. [Application Rollback](#application-rollback)
4. [Infrastructure Rollback](#infrastructure-rollback)
5. [Data Rollback](#data-rollback)
6. [Validation After Rollback](#validation-after-rollback)
7. [Post-Rollback Procedures](#post-rollback-procedures)
8. [Escalation Contacts](#escalation-contacts)

---

## Prerequisites

### Required Access

```bash
# Verify kubectl access
kubectl cluster-info
kubectl get pods -n cre

# Verify gcloud access
gcloud auth list
gcloud config get-value project

# Verify Terraform access
cd /path/to/cre/terraform/gcp
terraform version
```

### Backup Verification

Before initiating rollback, verify backups exist:

```bash
# Check recent GKE backups
gcloud compute snapshots list \
  --filter="creationTimestamp > '-1 day'" \
  --project=${PROJECT_ID}

# Check Mnesia backups
kubectl exec -n cre deployment/cre -- \
  ls -la /opt/cre/backup/

# Check Terraform state backups
ls -la /path/to/cre/terraform/gcp/terraform.tfstate.backup*
```

---

## Rollback Decision Matrix

### When to Rollback

| Condition | Action | Time to Rollback |
|-----------|--------|------------------|
| Critical bugs in production | Immediate | < 5 minutes |
| Performance degradation > 50% | Immediate | < 5 minutes |
| Data corruption detected | Immediate | < 5 minutes |
| Feature not working as expected | Evaluate | < 1 hour |
| Minor bugs | Consider hotfix | N/A |

### Rollback Flowchart

```
Issue Detected
      |
      v
Health Check Failed?
      |
      +-- YES --> Impact Assessment
      |              |
      |              v
      |         Critical/High?
      |              |
      |              +-- YES --> INITIATE ROLLBACK
      |              |
      |              +-- NO --> Document & Monitor
      |
      +-- NO --> False Alarm -> Resume
```

### Automated Rollback Triggers

Configure these conditions for auto-rollback:

```yaml
# k8s/rollback-policy.yaml
apiVersion: policy.k8s.io/v1
kind: PodDisruptionBudget
metadata:
  name: cre-rollback-pdb
  namespace: cre
spec:
  minAvailable: 80%
  selector:
    matchLabels:
      app: cre

# Use with readinessProbe failures to trigger rollback
```

---

## Application Rollback

### Kubernetes Deployment Rollback

#### Option 1: Rollback to Previous Version

```bash
# Check rollout history
kubectl rollout history deployment/cre -n cre

# Rollback to previous version
kubectl rollout undo deployment/cre -n cre

# Watch the rollback
kubectl rollout status deployment/cre -n cre

# Verify pods are running
kubectl get pods -n cre -w
```

#### Option 2: Rollback to Specific Revision

```bash
# Find the target revision
kubectl rollout history deployment/cre -n cre

# Rollback to specific revision
kubectl rollout undo deployment/cre --to-revision=3 -n cre

# Verify the rollback
kubectl describe deployment/cre -n cre
```

### Container Image Rollback

```bash
# List available image versions
gcloud container images list-tags gcr.io/${PROJECT_ID}/cre \
  --project=${PROJECT_ID}

# Set specific image version
kubectl set image deployment/cre \
  cre=gcr.io/${PROJECT_ID}/cre:v1.0.0 \
  --namespace=cre

# Verify
kubectl rollout status deployment/cre -n cre
```

### Configuration Rollback

```bash
# List ConfigMap revisions
kubectl get cm -n cre -o yaml

# Restore from backup
kubectl apply -f k8s/configmap.v1.0.0.yaml -n cre

# Force pod restart
kubectl rollout restart deployment/cre -n cre
```

---

## Infrastructure Rollback

### Terraform State Rollback

```bash
cd /path/to/cre/terraform/gcp

# Pull latest state
terraform pull

# Review current state
terraform show

# Option 1: Restore from backup state file
cp terraform.tfstate.backup.YYYYMMDD terraform.tfstate

# Option 2: Use version control
git log --oneline --all
git checkout <previous-commit>

# Apply the rollback
terraform apply -auto-approve

# Verify resources
terraform plan
```

### GKE Cluster Rollback

```bash
# Check cluster version
gcloud container clusters describe cre-cluster \
  --region=${REGION} \
  --project=${PROJECT_ID} \
  --format="table(currentMasterVersion, currentNodeVersion)"

# List available versions
gcloud container clusters get-server-config \
  --region=${REGION} \
  --project=${PROJECT_ID}

# Downgrade cluster master
gcloud container clusters update cre-cluster \
  --region=${REGION} \
  --cluster-version=1.27.3-gke.100 \
  --project=${PROJECT_ID}

# Downgrade node pool
gcloud container node-pools update general \
  --cluster=cre-cluster \
  --region=${REGION} \
  --cluster-version=1.27.3-gke.100 \
  --project=${PROJECT_ID}
```

### VPC/Network Rollback

```bash
# List VPC configurations
gcloud compute networks list --project=${PROJECT_ID}

# Restore firewall rules from backup
gcloud compute firewall-rules import cre-firewall-rules \
  --source=backup/firewall-rules.yaml \
  --project=${PROJECT_ID}

# Verify rules
gcloud compute firewall-rules list --project=${PROJECT_ID}
```

---

## Data Rollback

### Mnesia Database Rollback

```bash
# List available backups
kubectl exec -n cre deployment/cre -- \
  ls -lah /opt/cre/backup/

# Stop CRE (graceful shutdown)
kubectl scale deployment/cre --replicas=0 -n cre

# Wait for pods to terminate
kubectl wait --for=delete pod -l app=cre -n cre --timeout=60s

# Restore backup
kubectl exec -it -n cre $(kubectl get pod -n cre -l app=cre -o jsonpath='{.items[0].metadata.name}') -- \
  /opt/cre/bin/cre_eval "mnesia:restore('/opt/cre/backup/backup_20250209.', [])."

# Restore pods
kubectl scale deployment/cre --replicas=3 -n cre

# Verify
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:table_info(wf_cases, size)."
```

### Persistent Volume Rollback

```bash
# Identify PVC
kubectl get pvc -n cre

# List snapshots
gcloud compute snapshots list \
  --filter="labels.snapshot_group:cre-pv" \
  --project=${PROJECT_ID}

# Create new disk from snapshot
gcloud compute disks create cre-restored-disk \
  --source-snapshot=<snapshot-name> \
  --zone=${REGION}-a \
  --project=${PROJECT_ID}

# Create new PVC from restored disk
kubectl apply -f - <<EOF
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: cre-data-restored
  namespace: cre
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: standard
  volumeName: cre-restored-pv
  resources:
    requests:
      storage: 10Gi
EOF

# Update deployment to use restored PVC
kubectl patch deployment/cre -n cre -p '{"spec":{"template":{"spec":{"volumes":[{"name":"cre-data","persistentVolumeClaim":{"claimName":"cre-data-restored"}}]}}}}'
```

### Cloud Spanner Rollback (if applicable)

```bash
# List backup operations
gcloud spanner backups list \
  --instance=cre-instance \
  --database=cre-db \
  --project=${PROJECT_ID}

# Restore from backup
gcloud spanner databases restore \
  --async \
  --destination-db=cre-db-restored \
  --backup=cre-backup-20250209 \
  --instance=cre-instance \
  --project=${PROJECT_ID}

# Verify restore operation
gcloud spanner operations list \
  --instance=cre-instance \
  --project=${PROJECT_ID}
```

---

## Validation After Rollback

### Health Check Validation

```bash
# Run automated health check
./scripts/runbooks/health_check.sh

# Expected output:
# ✓ Cluster health: OK
# ✓ Pod health: OK
# ✓ Service connectivity: OK
# ✓ Data integrity: OK
# ✓ Performance baseline: OK
```

### Data Integrity Check

```bash
# Verify Mnesia tables
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    lists:foreach(fun(T) ->
      io:format('~p: ~p records~n', [T, mnesia:table_info(T, size)])
    end, mnesia:system_info(tables)).
  "

# Expected output: All tables with expected record counts
```

### Functional Validation

```bash
# Run smoke tests
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "cre_test:smoke()."

# Test API endpoints
export LB_IP=$(kubectl get svc -n cre cre-service -o jsonpath='{.status.loadBalancer.ingress[0].ip}')
curl -f http://${LB_IP}/api/v1/health
curl -f http://${LB_IP}/api/v1/workflows
```

### Performance Validation

```bash
# Compare metrics to baseline
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "recon:proc_count()."

# Check response times
for i in {1..10}; do
  time curl -f http://${LB_IP}/api/v1/health
done
```

---

## Post-Rollback Procedures

### Documentation

```bash
# Create rollback report
cat > rollback-report-$(date +%Y%m%d).md <<EOF
# Rollback Report - $(date)

## Summary
- **Version**: v1.1.0 -> v1.0.0
- **Reason**: [Fill in reason]
- **Duration**: [Fill in duration]
- **Impact**: [Fill in impact]

## Root Cause Analysis
[Fill in analysis]

## Actions Taken
[Fill in actions]

## Prevention Measures
[Fill in prevention]

## Review Date
[Schedule review]
EOF
```

### Root Cause Analysis Template

1. **Problem Statement**: What happened?
2. **Impact Assessment**: Who/what was affected?
3. **Timeline**: When did it happen?
4. **Detection**: How was it detected?
5. **Root Cause**: Why did it happen?
6. **Resolution**: How was it fixed?
7. **Prevention**: How to prevent recurrence?

### Incident Retro Template

```markdown
# Incident Retrospective - [INCIDENT-ID]

## What went well?
-
-

## What didn't go well?
-
-

## What surprised us?
-
-

## What should we do differently?
-
-

## Action Items
- [ ] [Owner] - Action item
```

---

## Escalation Contacts

| Role | Name | Contact | Hours |
|------|------|---------|-------|
| On-Call Engineer | CRE Ops | oncall@company.com | 24/7 |
| Engineering Manager | CRE Leadership | eng-manager@company.com | Business Hours |
| VP Engineering | Leadership | vp-engineering@company.com | Business Hours |
| CTO | Executive | cto@company.com | Business Hours |

### Emergency Escalation Path

```
On-Call Engineer (15 min)
      |
      +-- No resolution -> Engineering Manager (30 min)
                           |
                           +-- No resolution -> VP Engineering (1 hour)
                                                |
                                                +-- No resolution -> CTO
```

---

## Quick Reference Commands

### Rollback Commands Quick Reference

```bash
# Kubernetes
kubectl rollout undo deployment/cre -n cre
kubectl rollout undo deployment/cre --to-revision=N -n cre
kubectl set image deployment/cre cre=gcr.io/$PROJECT_ID/cre:v1.0.0 -n cre

# GKE
gcloud container clusters update cre-cluster --cluster-version=VERSION --region=$REGION

# Terraform
terraform apply -backup=-  # Disable backup
cp terraform.tfstate.backup.YYYYMMDD terraform.tfstate

# Mnesia
kubectl exec -n cre deployment/cre -- /opt/cre/bin/cre_eval "mnesia:restore('/path/to/backup.', [])."
```

### Verification Commands

```bash
# Health check
curl http://$LB_IP/api/v1/health

# Pod status
kubectl get pods -n cre

# Logs
kubectl logs -f -l app=cre -n cre

# Metrics
kubectl top pods -n cre
```

---

## Runbook Checklist

### Pre-Rollback Checklist

- [ ] Root cause identified
- [ ] Rollback target version identified
- [ ] Backups verified and accessible
- [ ] Stakeholders notified
- [ ] Maintenance window approved (if needed)
- [ ] Rollback plan documented

### During Rollback Checklist

- [ ] Rollback command executed
- [ ] Progress monitored
- [ ] Errors documented
- [ ] Communication updated

### Post-Rollback Checklist

- [ ] Health checks passing
- [ ] Data integrity verified
- [ ] Functionality validated
- [ ] Metrics reviewed
- [ ] Incident report created
- [ ] Retro scheduled
- [ ] Prevention measures identified

---

*Last Updated: 2025-02-09*
*For CRE version 0.3.0+*
