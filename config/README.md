# Cloud Build & Kubernetes Deployment Configuration

Complete Cloud Build CI/CD pipeline with automated testing, security scanning, multi-stage deployment, and rollback automation.

## Directory Structure

```
config/
├── cloudbuild.yaml                 # Main Cloud Build configuration
├── configmap.yaml                  # Application configuration for both environments
├── rbac.yaml                       # RBAC definitions (ServiceAccounts, Roles, RoleBindings)
├── security-scanning.yaml          # Security policies and scanning configuration
├── staging/
│   ├── deployment.yaml             # Staging deployment configuration
│   ├── kustomization.yaml          # Kustomize overlay for staging
│   └── secrets.env                 # Staging secrets (gitignore)
├── prod/
│   ├── deployment.yaml             # Production deployment configuration
│   ├── kustomization.yaml          # Kustomize overlay for production
│   └── secrets.env                 # Production secrets (gitignore)
├── CLOUD_BUILD_GUIDE.md           # Comprehensive guide
└── README.md                       # This file
```

## Quick Start

### 1. Prerequisites

```bash
# Required tools
- gcloud CLI
- kubectl
- docker
- git

# Required GCP services
- Cloud Build
- Artifact Registry
- Google Kubernetes Engine (GKE)
- Cloud Logging
- Cloud Monitoring
```

### 2. Initial Setup

```bash
# Set project variables
export PROJECT_ID="your-gcp-project"
export REGION="us-central1"
export STAGING_CLUSTER="staging-cluster"
export PROD_CLUSTER="prod-cluster"

# Enable required APIs
gcloud services enable \
  cloudbuild.googleapis.com \
  container.googleapis.com \
  artifactregistry.googleapis.com

# Create Artifact Registry
gcloud artifacts repositories create admin-console-repo \
  --repository-format=docker \
  --location=$REGION

# Create namespaces
kubectl create namespace staging
kubectl create namespace production
```

### 3. Deploy Configuration

```bash
# Apply RBAC
kubectl apply -f config/rbac.yaml

# Apply ConfigMaps
kubectl apply -f config/configmap.yaml

# Deploy staging
kubectl apply -f config/staging/deployment.yaml

# Deploy production (requires approval gate)
kubectl apply -f config/prod/deployment.yaml
```

### 4. Update Substitutions

Edit `cloudbuild.yaml` and update:

```yaml
substitutions:
  _REGION: 'us-central1'              # Your GCP region
  _PROJECT_ID: 'your-project-id'      # Your GCP project
  _STAGING_CLUSTER: 'staging-cluster' # Your staging cluster
  _PROD_CLUSTER: 'prod-cluster'       # Your production cluster
```

### 5. Create Build Trigger

```bash
gcloud builds triggers create github \
  --repo-name=your-repo \
  --repo-owner=your-org \
  --branch-pattern="^main$" \
  --build-config=config/cloudbuild.yaml
```

## Configuration Files

### cloudbuild.yaml

Main Cloud Build orchestration pipeline.

**Key Features:**
- Multi-stage build (17 steps)
- Parallel execution where possible
- Automated security scanning
- Staged deployment (staging → production)
- Approval gates
- Automatic rollback triggers

**Build Steps:**
1. Docker build
2. Push to Artifact Registry
3. Unit tests
4. Linting & type checks
5. Container vulnerability scanning
6. OWASP dependency check
7. SonarQube analysis
8. SBOM generation
9-11. Staging deployment & tests
12. Approval gate (manual)
13-17. Production deployment & verification

### Deployment Configurations

#### staging/deployment.yaml
- 2 replicas (HPA: 2-5)
- 512Mi memory, 500m CPU limits
- Debug logging enabled
- Suitable for testing

#### prod/deployment.yaml
- 3 replicas (HPA: 3-10)
- 1Gi memory, 1000m CPU limits
- Production logging
- Pod Disruption Budget: minimum 2
- Node affinity: production nodes only

### Security Configurations

#### rbac.yaml
- ServiceAccounts for each environment
- Roles with minimum required permissions
- RoleBindings for deployment and monitoring

#### security-scanning.yaml
- Network policies
- Pod Security Policies
- Resource quotas
- DependencyCheck rules
- SAST rules
- SBOM configuration

### Kustomization Files

#### staging/kustomization.yaml
```bash
# Apply with Kustomize
kubectl apply -k config/staging/

# Or with Cloud Build
gke-deploy run --filename=config/staging/
```

#### prod/kustomization.yaml
```bash
# Apply with Kustomize
kubectl apply -k config/prod/

# Or with Cloud Build
gke-deploy run --filename=config/prod/
```

## Scripts

### /scripts/rollback.sh

Manages deployment rollback with comprehensive validation.

**Usage:**
```bash
# Rollback to previous version
./scripts/rollback.sh

# Rollback to specific image SHA
./scripts/rollback.sh -s abc123def456

# Show deployment history
./scripts/rollback.sh --history

# Show current status
./scripts/rollback.sh --status
```

**Features:**
- Automatic rollback to previous stable revision
- Manual rollback by image SHA
- Health check verification
- Deployment report generation
- Event logging

### /scripts/deploy.sh

Deployment automation with validation and verification.

**Usage:**
```bash
# Deploy to staging
./scripts/deploy.sh -e staging

# Deploy to production
./scripts/deploy.sh -e production

# Dry-run (preview changes)
./scripts/deploy.sh -e production --dry-run

# Force deployment
./scripts/deploy.sh -e production --force
```

**Features:**
- Pre-deployment validation
- Credential setup
- Configuration verification
- Health checks
- Automatic rollback on failure

### /scripts/health-check.sh

Container health check endpoints.

**Usage:**
```bash
# Health check
./scripts/health-check.sh health

# Readiness check
./scripts/health-check.sh readiness

# Startup check
./scripts/health-check.sh startup
```

### /scripts/startup.sh

Container startup and initialization.

**Features:**
- Environment setup
- Dependency verification
- Database migrations (if needed)
- File permissions
- Graceful shutdown handling

## Secrets Management

### Storing Secrets

```bash
# Create staging secrets
kubectl create secret generic admin-console-secrets \
  -n staging \
  --from-literal=DB_PASSWORD=*** \
  --from-literal=API_KEY=***

# Create production secrets
kubectl create secret generic admin-console-secrets \
  -n production \
  --from-literal=DB_PASSWORD=*** \
  --from-literal=API_KEY=***
```

### Using Secrets in Deployments

Secrets are referenced in deployment YAML:

```yaml
env:
  - name: DB_PASSWORD
    valueFrom:
      secretKeyRef:
        name: admin-console-secrets
        key: DB_PASSWORD
```

### GCP Secret Manager Integration

```bash
# Store in Secret Manager
gcloud secrets create admin-console-db-password \
  --replication-policy="automatic" \
  --data-file=-

# Reference in Cloud Build
gcloud builds create \
  --substitutions=_DB_PASSWORD=\$_DB_PASSWORD \
  --secrets="id=admin-console-db-password,versionId=latest"
```

## Environment Variables

### Staging Environment
```
NODE_ENV=staging
LOG_LEVEL=debug
API_URL=https://staging-api.example.com
ENVIRONMENT=staging
PORT=3000
```

### Production Environment
```
NODE_ENV=production
LOG_LEVEL=info
API_URL=https://api.example.com
ENVIRONMENT=production
PORT=3000
ENABLE_TRACING=true
ENABLE_METRICS=true
```

## Build Substitutions

Customize build behavior by setting substitutions:

```bash
gcloud builds submit \
  --substitutions=_ENVIRONMENT=production,_ZONE=us-central1-a
```

### Available Substitutions

| Variable | Default | Description |
|----------|---------|-------------|
| `_REGION` | `us-central1` | GCP region |
| `_ZONE` | `us-central1-a` | GCP zone |
| `_REPOSITORY` | `admin-console-repo` | Artifact Registry repo |
| `_IMAGE_NAME` | `admin-console` | Docker image name |
| `_STAGING_CLUSTER` | `staging-cluster` | Staging cluster name |
| `_STAGING_NAMESPACE` | `staging` | Staging namespace |
| `_PROD_CLUSTER` | `prod-cluster` | Production cluster name |
| `_PROD_NAMESPACE` | `production` | Production namespace |

## Monitoring

### Cloud Build

```bash
# View build logs
gcloud builds log BUILD_ID

# List recent builds
gcloud builds list --limit=10

# Get build status
gcloud builds describe BUILD_ID
```

### Kubernetes Deployments

```bash
# Check deployment status
kubectl get deployment admin-console -n production

# View pod logs
kubectl logs -n production deployment/admin-console --tail=100 -f

# Describe deployment
kubectl describe deployment admin-console -n production

# View events
kubectl get events -n production --sort-by='.lastTimestamp'
```

### Cloud Logging

```bash
# Query build logs
gcloud logging read "resource.type=build" --limit=10

# Query deployment logs
gcloud logging read "resource.type=k8s_cluster" --limit=10
```

## Troubleshooting

### Build Fails

1. Check Cloud Build logs: `gcloud builds log BUILD_ID --stream`
2. Verify configuration: `gcloud builds validate`
3. Check substitutions are correct
4. Verify Docker image builds locally

### Deployment Fails

1. Check pod status: `kubectl describe pod POD_NAME -n NAMESPACE`
2. Check pod logs: `kubectl logs POD_NAME -n NAMESPACE`
3. Verify image pull: `kubectl get events -n NAMESPACE`
4. Check resource limits

### Health Check Failures

1. Verify endpoints exist: `curl http://localhost:3000/health`
2. Check port configuration
3. Verify application is running
4. Check resource limits

See [CLOUD_BUILD_GUIDE.md](./CLOUD_BUILD_GUIDE.md) for detailed troubleshooting.

## Best Practices

### Security

- Always use non-root users
- Set resource limits and requests
- Use network policies
- Scan containers for vulnerabilities
- Rotate secrets regularly
- Use Pod Security Policies

### Performance

- Use rolling updates (zero downtime)
- Set appropriate resource requests
- Use HPA for automatic scaling
- Cache Docker layers
- Minimize image size

### Reliability

- Use health checks (liveness, readiness)
- Set up Pod Disruption Budgets
- Use multiple replicas
- Test deployments in staging first
- Maintain deployment history for rollback

### Compliance

- Generate SBOM for all images
- Run security scans
- Audit access logs
- Maintain deployment records
- Document changes

## Cost Optimization

- Use appropriate machine types
- Set resource limits
- Use preemptible nodes for non-critical workloads
- Set node auto-scaling limits
- Monitor and optimize resource usage

## Related Documentation

- [CLOUD_BUILD_GUIDE.md](./CLOUD_BUILD_GUIDE.md) - Comprehensive guide
- [Google Cloud Build](https://cloud.google.com/build/docs)
- [GKE Documentation](https://cloud.google.com/kubernetes-engine/docs)
- [Kubernetes Best Practices](https://kubernetes.io/docs/concepts/cluster-administration/manage-deployment/)

## Support

For issues or questions:

1. Check documentation and troubleshooting guides
2. Review Cloud Build logs
3. Check Kubernetes events
4. Contact your platform team

## License

See LICENSE file in repository root.
