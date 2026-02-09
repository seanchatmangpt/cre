# CRE GCP Deployment Runbook

**Operational procedures for deploying CRE workflow engine to Google Cloud Platform.**

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Initial Deployment](#initial-deployment)
3. [Rolling Updates](#rolling-updates)
4. [Validation Steps](#validation-steps)
5. [Troubleshooting](#troubleshooting)
6. [Rollback Procedures](#rollback-procedures)
7. [Escalation Contacts](#escalation-contacts)

---

## Prerequisites

### Required Tools

```bash
# Verify installations
gcloud --version          # Google Cloud SDK 400.0.0+
terraform --version       # Terraform 1.5.0+
kubectl version --client  # kubectl 1.27.0+
docker --version          # Docker 24.0.0+
```

### Required Permissions

| IAM Role | Purpose |
|----------|---------|
| `roles/container.admin` | GKE cluster management |
| `roles/iam.serviceAccountAdmin` | Service account management |
| `roles/compute.admin` | VPC and firewall configuration |
| `roles/storage.admin` | Cloud Storage and backups |
| `roles/monitoring.admin` | Cloud Monitoring and alerting |
| `roles/secretmanager.admin` | Secret management |

### Environment Setup

```bash
# Set project
export PROJECT_ID="your-project-id"
gcloud config set project ${PROJECT_ID}

# Set region
export REGION="us-central1"
gcloud config set compute/region ${REGION}

# Get credentials (after cluster exists)
gcloud container clusters get-credentials cre-cluster --region=${REGION}

# Verify connection
kubectl cluster-info
```

---

## Initial Deployment

### Step 1: Prepare Infrastructure

```bash
cd /path/to/cre/terraform/gcp

# Copy and customize variables
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars with your values

# Initialize Terraform
terraform init

# Validate configuration
terraform validate

# Plan deployment
terraform plan -out=tfplan

# Deploy infrastructure
terraform apply tfplan
```

### Step 2: Build and Push Container Image

```bash
# Build Docker image
docker build -t gcr.io/${PROJECT_ID}/cre:v1.0.0 .

# Tag for GCR
docker tag gcr.io/${PROJECT_ID}/cre:v1.0.0 gcr.io/${PROJECT_ID}/cre:latest

# Push to GCR
docker push gcr.io/${PROJECT_ID}/cre:v1.0.0
docker push gcr.io/${PROJECT_ID}/cre:latest
```

### Step 3: Deploy Kubernetes Workloads

```bash
# Create namespace
kubectl create namespace cre

# Create secrets
kubectl create secret generic cre-secrets \
  --from-literal=cookie="${CRE_COOKIE}" \
  --from-literal=db-password="${DB_PASSWORD}" \
  --namespace=cre

# Deploy ConfigMap
kubectl apply -f k8s/configmap.yaml --namespace=cre

# Deploy CRE pods
kubectl apply -f k8s/deployment.yaml --namespace=cre

# Deploy services
kubectl apply -f k8s/service.yaml --namespace=cre

# Deploy ingress
kubectl apply -f k8s/ingress.yaml --namespace=cre
```

### Step 4: Verify Deployment

```bash
# Check pod status
kubectl get pods -n cre -w

# Describe pod for details
kubectl describe pod -l app=cre -n cre

# View logs
kubectl logs -f -l app=cre -n cre

# Check services
kubectl get svc -n cre

# Test health endpoint
kubectl port-forward -n cre svc/cre-service 4142:4142
curl http://localhost:4142/api/v1/health
```

---

## Rolling Updates

### Update Container Image

```bash
# Apply new image
kubectl set image deployment/cre \
  cre=gcr.io/${PROJECT_ID}/cre:v1.1.0 \
  --namespace=cre

# Watch rollout status
kubectl rollout status deployment/cre -n cre

# Check revision history
kubectl rollout history deployment/cre -n cre
```

### Update Configuration

```bash
# Update ConfigMap
kubectl apply -f k8s/configmap.yaml --namespace=cre

# Force pod restart to pick up config changes
kubectl rollout restart deployment/cre -n cre
```

### Managed Update Strategy

The deployment uses the following update strategy:

```yaml
strategy:
  type: RollingUpdate
  rollingUpdate:
    maxSurge: 1        # Create 1 new pod at a time
    maxUnavailable: 0  # Never have unavailable pods
```

This ensures zero-downtime deployments.

---

## Validation Steps

### Health Check Validation

```bash
# Run comprehensive health check
./scripts/runbooks/health_check.sh

# Expected output:
# ✓ Cluster health: OK
# ✓ Pod health: OK
# ✓ Service connectivity: OK
# ✓ Database connectivity: OK
# ✓ Memory usage: 45%
# ✓ CPU usage: 23%
```

### Smoke Tests

```bash
# Execute smoke tests
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "cre_test:smoke()."

# Expected: All tests pass
```

### Performance Validation

```bash
# Check response times
for i in {1..10}; do
  time curl -f http://$(kubectl get svc -n cre cre-service -o jsonpath='{.status.loadBalancer.ingress[0].ip}')/api/v1/health
done

# Expected: < 100ms per request
```

### Data Validation

```bash
# Verify Mnesia tables
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:table_info(wf_cases, size)."

# Expected: Integer value (table size)
```

---

## Troubleshooting

### Pod Not Starting

```bash
# Check pod status
kubectl get pods -n cre

# Describe pod for events
kubectl describe pod <pod-name> -n cre

# Common issues:
# - ImagePullBackOff: Check image name and credentials
# - CrashLoopBackOff: Check logs for application errors
# - Pending: Check resource requests/limits

# View logs
kubectl logs <pod-name> -n cre
kubectl logs <pod-name> -n cre --previous  # Previous instance
```

### Service Not Accessible

```bash
# Check service endpoints
kubectl get endpoints <service-name> -n cre

# Check ingress status
kubectl get ingress -n cre

# Test pod-to-pod connectivity
kubectl run -it --rm debug --image=busybox --restart=Never -n cre -- \
  wget -O- http://cre-service:4142/api/v1/health
```

### Cluster Issues

```bash
# Check node status
kubectl get nodes

# Check cluster events
kubectl get events -n cre --sort-by='.lastTimestamp'

# Check resource quotas
kubectl describe quota -n cre

# Check limit ranges
kubectl describe limitrange -n cre
```

---

## Rollback Procedures

### Quick Rollback (Image)

```bash
# Rollback to previous version
kubectl rollout undo deployment/cre -n cre

# Rollback to specific revision
kubectl rollout history deployment/cre -n cre
kubectl rollout undo deployment/cre --to-revision=3 -n cre

# Verify rollback
kubectl rollout status deployment/cre -n cre
```

### Full Infrastructure Rollback

```bash
cd /path/to/cre/terraform/gcp

# Review current state
terraform show

# Rollback to previous Terraform state
terraform rollback <backup-file>

# Or use version control
git checkout <previous-commit>
terraform apply
```

### Data Rollback

```bash
# List available backups
gcloud compute snapshots list \
  --filter="labels.snapshot_group:*" \
  --project=${PROJECT_ID}

# Restore from snapshot
gcloud compute disks snapshot <disk-name> \
  --snapshot-names=cre-restore-$(date +%Y%m%d) \
  --project=${PROJECT_ID}

# For Mnesia data backup/restore
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:backup('/opt/cre/backup/rollback')."

kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:restore('/opt/cre/backup/rollback.', [])."
```

---

## Escalation Contacts

| Role | Name | Contact | Hours |
|------|------|---------|-------|
| On-Call Engineer | CRE Ops | oncall@company.com | 24/7 |
| Cloud Infrastructure Lead | Infrastructure Team | infra@company.com | Business Hours |
| GCP Support | Google Cloud | gcp-support | 24/7 |
| Engineering Manager | CRE Leadership | eng-manager@company.com | Business Hours |

### Severity Levels

| Severity | Response Time | Examples |
|----------|---------------|----------|
| S1 - Critical | 15 minutes | Complete service outage, data loss |
| S2 - High | 1 hour | Major functionality broken |
| S3 - Medium | 4 hours | Partial functionality affected |
| S4 - Low | 1 business day | Minor issues, questions |

---

## Command Reference

### Useful kubectl Commands

```bash
# Port forward to local
kubectl port-forward -n cre svc/cre-service 4142:4142

# Execute command in pod
kubectl exec -it -n cre deployment/cre -- /bin/bash

# Copy files to/from pod
kubectl cp /path/to/file -n cre <pod-name>:/path/in/pod

# Get resource usage
kubectl top pods -n cre
kubectl top nodes

# Network debugging
kubectl run -it --rm debug --image=nicolaka/netshoot --restart=Never -n cre -- /bin/bash
```

### Useful gcloud Commands

```bash
# SSH into node
gcloud compute ssh <node-name> --zone=${REGION}-a

# View logs
gcloud logging read "resource.type=k8s_container" \
  --project=${PROJECT_ID} \
  --format="table(timestamp,jsonPayload.message)" \
  --limit=50

# Get cluster credentials
gcloud container clusters get-credentials cre-cluster --region=${REGION}

# Update node pools
gcloud container node-pools upgrade general \
  --cluster=cre-cluster \
  --region=${REGION} \
  --project=${PROJECT_ID}
```

---

## Runbook Checklist

### Pre-Deployment Checklist

- [ ] All tests passing locally
- [ ] Code review completed
- [ ] Terraform changes reviewed
- [ ] Backup completed
- [ ] Maintenance window approved (if needed)
- [ ] Stakeholders notified
- [ ] Runbook reviewed

### Post-Deployment Checklist

- [ ] All pods running
- [ ] Health checks passing
- [ ] Smoke tests passed
- [ ] Logs reviewed for errors
- [ ] Metrics within normal range
- [ ] Alerting verified
- [ ] Rollback capability confirmed

---

*Last Updated: 2025-02-09*
*For CRE version 0.3.0+*
