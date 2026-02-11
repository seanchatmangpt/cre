# CRE Deployment Guide for Google Cloud Marketplace

This guide walks through deploying CRE (Common Runtime Environment) on Google Kubernetes Engine (GKE) via Google Cloud Marketplace.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Quick Start](#quick-start)
- [Deployment Steps](#deployment-steps)
- [Configuration](#configuration)
- [Verification](#verification)
- [Next Steps](#next-steps)

---

## Prerequisites

### GCP Requirements

Before deploying CRE from Google Cloud Marketplace, ensure you have:

- **GCP Project** with billing enabled
- **GKE Cluster** (version 1.25 or higher)
  - Private cluster recommended for production
  - 3+ nodes for high availability
  - Sufficient node capacity (see [System Requirements](#system-requirements))
- **Permission** to create deployments, services, and ingress resources
- **Workload Identity** enabled (recommended for GCP service access)

### Tools Required

- **gcloud CLI** - Google Cloud command-line tool
- **kubectl** - Kubernetes command-line tool
- **helm** - Kubernetes package manager (optional, for advanced configuration)

```bash
# Install gcloud CLI
curl https://sdk.cloud.google.com | bash
exec -l $SHELL
gcloud init

# Install kubectl
gcloud components install kubectl

# Verify installations
gcloud --version
kubectl version --client
```

---

## System Requirements

### Minimum Requirements (Development/Test)

| Resource | Minimum |
|----------|---------|
| **GKE Version** | 1.25+ |
| **Node Count** | 1 node |
| **CPU per Node** | 2 cores |
| **Memory per Node** | 4 GiB |
| **CRE Pods** | 1 replica |
| **Storage** | 10 GiB persistent volume |
| **Network** | Standard network |

### Recommended Requirements (Production)

| Resource | Recommended |
|----------|-------------|
| **GKE Version** | 1.27+ |
| **Node Count** | 3+ nodes (regional cluster) |
| **CPU per Node** | 4+ cores |
| **Memory per Node** | 8+ GiB |
| **CRE Pods** | 3 replicas |
| **Storage** | 50 GiB persistent volume (SSD) |
| **Network** | VPC with private cluster |
| **Monitoring** | Cloud Monitoring, Cloud Logging, Cloud Trace |
| **Backup** | Daily automated backups to Cloud Storage |

---

## Quick Start

### Deploy CRE from Marketplace

1. **Navigate to CRE Listing**:
   - Go to [Google Cloud Marketplace](https://console.cloud.google.com/marketplace)
   - Search for "CRE Workflow Engine"
   - Click on CRE listing

2. **Configure Deployment**:
   ```
   Cluster:          your-gke-cluster
   Namespace:        cre (recommended)
   Replicas:         3 (production) or 1 (development)
   CPU:              2000m (2 cores)
   Memory:           4Gi
   Storage:          50Gi (SSD)
   ```

3. **Deploy**:
   - Click "Deploy"
   - Review configuration
   - Click "Deploy" again to confirm
   - Wait for deployment to complete (~5-10 minutes)

4. **Access CRE**:
   ```bash
   # Get external IP (if using LoadBalancer)
   kubectl get svc cre-api-svc -n cre

   # Or port-forward for local access
   kubectl port-forward -n cre svc/cre-service 4142:4142

   # Access web dashboard
   open http://localhost:4142/dashboard
   ```

That's it! CRE is now deployed and ready to use.

---

## Deployment Steps

### Step 1: Prepare GKE Cluster

Ensure your GKE cluster meets the requirements:

```bash
# Set your project and cluster
export PROJECT_ID=your-project-id
export CLUSTER_NAME=your-cluster-name
export REGION=us-central1

# Configure kubectl to access your cluster
gcloud container clusters get-credentials $CLUSTER_NAME --region $REGION

# Verify cluster version
kubectl version --short  # Server version should be 1.25+

# Verify node capacity
kubectl top nodes
```

### Step 2: Create Namespace (Optional but Recommended)

Create a dedicated namespace for CRE:

```bash
# Create namespace
kubectl create namespace cre

# Set as default namespace for subsequent commands
kubectl config set-context --current --namespace=cre
```

### Step 3: Deploy from Marketplace

**Via Google Cloud Console:**

1. Navigate to [Google Cloud Marketplace](https://console.cloud.google.com/marketplace)
2. Search for "CRE Workflow Engine"
3. Click "Configure"
4. Fill in deployment parameters:
   - **Cluster**: Select your GKE cluster
   - **Namespace**: `cre`
   - **Deployment Name**: `cre`
   - **Replicas**: `3` (production)
   - **CPU**: `2000m`
   - **Memory**: `4Gi`
   - **Storage Class**: `standard-rwo` or `premium-rwo` (SSD)
   - **Storage Size**: `50Gi`
5. Click "Deploy"

**Via gcloud CLI (Alternative):**

```bash
# Deploy using Marketplace deployer
gcloud deployment-manager deployments create cre-deployment \
  --template=marketplace/deployer.yaml \
  --properties=namespace:cre,replicas:3,cpu:2000m,memory:4Gi,storage:50Gi
```

### Step 4: Verify Deployment

Check that all pods are running:

```bash
# Check pods
kubectl get pods -n cre

# Expected output:
# NAME    READY   STATUS    RESTARTS   AGE
# cre-0   1/1     Running   0          5m
# cre-1   1/1     Running   0          4m
# cre-2   1/1     Running   0          3m

# Check services
kubectl get svc -n cre

# Expected output:
# NAME           TYPE           EXTERNAL-IP      PORT(S)
# cre-service    ClusterIP      10.0.0.10        4142/TCP
# cre-api-svc    LoadBalancer   34.123.45.67     4142:4142/TCP

# Check persistent volumes
kubectl get pvc -n cre

# Expected output:
# NAME           STATUS   VOLUME                                     CAPACITY
# cre-data-cre-0 Bound    pvc-12345678-1234-1234-1234-123456789abc   50Gi
```

### Step 5: Access CRE

Get the external IP or set up port forwarding:

```bash
# Option 1: Access via external IP (LoadBalancer)
export CRE_IP=$(kubectl get svc cre-api-svc -n cre -o jsonpath='{.status.loadBalancer.ingress[0].ip}')
echo "CRE API available at: http://$CRE_IP:4142"

# Option 2: Port-forward for local access
kubectl port-forward -n cre svc/cre-service 4142:4142
echo "CRE API available at: http://localhost:4142"
```

---

## Configuration

### Environment Variables

CRE configuration is managed via environment variables. The Marketplace deployment sets sensible defaults, but you can customize:

| Variable | Default | Description |
|----------|---------|-------------|
| `CRE_LOG_LEVEL` | `info` | Log level: `debug`, `info`, `warning`, `error` |
| `CRE_PORT` | `4142` | HTTP API port |
| `CRE_MNESIA_DIR` | `/var/lib/cre/mnesia` | Mnesia data directory |
| `CRE_BACKUP_DIR` | `/var/lib/cre/backup` | Backup directory |
| `CRE_OTEL_EXPORTER` | `gcp` | OpenTelemetry exporter: `gcp`, `stdout`, `none` |
| `CRE_OTEL_SAMPLING` | `1.0` | Trace sampling rate (0.0 to 1.0) |

### Customizing Configuration

**Via Marketplace Console:**
1. Go to CRE deployment in Cloud Console
2. Click "Edit Configuration"
3. Modify environment variables
4. Click "Update Deployment"

**Via kubectl:**
```bash
# Edit deployment
kubectl edit deployment cre -n cre

# Add or modify environment variables in the container spec
env:
  - name: CRE_LOG_LEVEL
    value: "debug"
  - name: CRE_OTEL_SAMPLING
    value: "0.1"
```

### Resource Limits

Adjust CPU and memory based on workload:

```bash
# Edit deployment
kubectl edit deployment cre -n cre

# Modify resources:
resources:
  requests:
    cpu: "2000m"
    memory: "4Gi"
  limits:
    cpu: "4000m"
    memory: "8Gi"
```

### Autoscaling

Enable Horizontal Pod Autoscaler (HPA):

```bash
# Create HPA (requires custom metrics from Cloud Monitoring)
kubectl autoscale deployment cre \
  -n cre \
  --min=3 \
  --max=10 \
  --cpu-percent=70
```

For advanced autoscaling based on workflow queue length, see [Operations Guide - Scaling](operations-guide.md).

---

## Verification

### Health Checks

CRE provides Kubernetes health check endpoints:

```bash
# Liveness probe (is the pod running?)
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health

# Readiness probe (is the pod ready to serve traffic?)
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/ready

# Both endpoints return JSON:
# {
#   "status": "ok",
#   "mnesia": "connected",
#   "uptime_seconds": 1234
# }
```

### Test Workflow Submission

Submit a test workflow to verify CRE is working:

```bash
# Get CRE API endpoint
export CRE_ENDPOINT=$(kubectl get svc cre-api-svc -n cre -o jsonpath='{.status.loadBalancer.ingress[0].ip}')

# Submit a simple workflow (using curl)
curl -X POST http://$CRE_ENDPOINT:4142/api/workflows \
  -H "Content-Type: application/json" \
  -d '{
    "name": "test_workflow",
    "tasks": [
      {"id": "task1", "type": "atomic", "module": "erlang", "function": "now"}
    ]
  }'

# Expected response:
# {"workflow_id":"wf_123456","status":"submitted"}
```

### Check Logs

View CRE logs to verify operation:

```bash
# Stream logs from all pods
kubectl logs -n cre -l app=cre --tail=100 -f

# Check for errors
kubectl logs -n cre -l app=cre --tail=100 | grep -i error

# View logs in Cloud Logging
gcloud logging read "resource.labels.container_name=cre" --limit=50 --format=json
```

### Check Mnesia Cluster Status

Verify Mnesia (distributed database) is connected:

```bash
# Check Mnesia status on each pod
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status

# Expected output:
# Mnesia is running. Nodes: ['cre@cre-0.cre.cre.svc.cluster.local',
#                            'cre@cre-1.cre.cre.svc.cluster.local',
#                            'cre@cre-2.cre.cre.svc.cluster.local']
```

---

## Networking

### Service Types

CRE deployment creates two Kubernetes services:

1. **cre-service** (ClusterIP)
   - Internal service for pod-to-pod communication
   - Used for Erlang distribution
   - Not exposed externally

2. **cre-api-svc** (LoadBalancer)
   - External access to CRE API
   - Exposes port 4142
   - Use this for external client access

### Ingress (Optional)

For custom domain names and TLS:

```yaml
# Example Ingress resource
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: cre-ingress
  namespace: cre
  annotations:
    kubernetes.io/ingress.global-static-ip-name: cre-static-ip
    networking.gke.io/managed-certificates: cre-cert
spec:
  rules:
  - host: cre.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: cre-api-svc
            port:
              number: 4142
```

### Network Policies

For production, restrict pod-to-pod communication:

```yaml
# Example NetworkPolicy (optional)
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: cre-network-policy
  namespace: cre
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
    ports:
    - protocol: TCP
      port: 4142
    - protocol: TCP
      port: 4369  # EPMD
    - protocol: TCP
      port: 9100  # Erlang distribution
  egress:
  - to:
    - podSelector:
        matchLabels:
          app: cre
    ports:
    - protocol: TCP
    - protocol: UDP
```

---

## Storage

### Persistent Volumes

CRE uses PersistentVolumes for:

- **Mnesia Data**: Distributed database state
- **Backups**: Automated and manual backups
- **Configuration**: Custom configuration files

### Storage Classes

Recommended storage classes:

| Environment | Storage Class | Description |
|-------------|---------------|-------------|
| Development | `standard-rwo` | Standard HDD (cost-effective) |
| Production | `premium-rwo` | SSD (high performance) |
| Mission-Critical | `premium-rwo` + backup | SSD with frequent backups |

### Backup Configuration

Configure automated backups (see [Operations Guide - Backup](operations-guide.md)):

```bash
# Enable automated backups (cron job)
kubectl apply -f marketplace/k8s/backup-cronjob.yaml

# Verify backup job
kubectl get cronjob -n cre
```

---

## Security

### Workload Identity

Enable Workload Identity for secure GCP service access:

```bash
# Create Google service account
gcloud iam service-accounts create cre-sa \
  --display-name="CRE Service Account"

# Bind Workload Identity
gcloud iam service-accounts add-iam-policy-binding cre-sa@$PROJECT_ID.iam.gserviceaccount.com \
  --role=roles/iam.workloadIdentityUser \
  --member="serviceAccount:$PROJECT_ID.svc.id.goog[cre/cre-sa]"

# Annotate Kubernetes service account
kubectl annotate serviceaccount cre-sa \
  -n cre \
  iam.gke.io/gcp-service-account=cre-sa@$PROJECT_ID.iam.gserviceaccount.com
```

### RBAC

Configure Role-Based Access Control (RBAC):

```yaml
# Example RBAC (restrictive)
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: cre-role
  namespace: cre
rules:
- apiGroups: [""]
  resources: ["pods", "services", "configmaps"]
  verbs: ["get", "list", "watch"]
```

See [Security Model](security-model.md) for complete security configuration.

---

## Troubleshooting

### Pod Not Starting

**Symptoms**: Pod stuck in `Pending` or `ImagePullBackOff`

**Diagnosis**:
```bash
# Describe pod to see events
kubectl describe pod -n cre cre-0
```

**Solutions**:
- Check image pull secrets
- Verify node has sufficient resources
- Check network policies

### Pod Not Ready

**Symptoms**: Pod in `Running` state but not `Ready`

**Diagnosis**:
```bash
# Check logs
kubectl logs -n cre cre-0 --tail=100

# Check health endpoints
kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health
```

**Solutions**:
- Verify environment variables
- Check persistent volume is mounted
- Verify Mnesia cluster is connected

### Mnesia Cluster Issues

**Symptoms**: Mnesia nodes not connecting

**Diagnosis**:
```bash
# Check Mnesia status
kubectl exec -n cre cre-0 -- /app/bin/cre mnesia status
```

**Solutions**:
- Verify DNS resolution between pods
- Check network policies allow Erlang distribution ports
- Verify all pods can communicate on ports 4369 (EPMD) and 9100+ (distribution)

For more troubleshooting, see [Operations Guide - Troubleshooting](operations-guide.md) and the [Troubleshooting Runbook](../../docs/gcp/runbooks/troubleshooting.md).

---

## Next Steps

After deploying CRE:

1. **[Operations Guide](operations-guide.md)** - Learn how to operate CRE in production
2. **[Security Model](security-model.md)** - Configure security and compliance
3. **[Cost Model](cost-model.md)** - Understand infrastructure costs
4. **[Upgrade Guide](../../docs/gcp/marketplace/UPGRADE.md)** - Plan for future upgrades

### Additional Resources

- **[Main Deployment Guide](../../docs/DEPLOYMENT.md)** - Comprehensive deployment documentation
- **[Deployment Runbook](../../docs/gcp/runbooks/deployment.md)** - Detailed deployment procedures
- **[GCP Marketplace Readiness](../../docs/gcp/GCP_MARKETPLACE_READINESS.md)** - Technical assessment

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
