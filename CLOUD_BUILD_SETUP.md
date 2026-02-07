# Cloud Build Pipeline Setup - Complete Implementation

Comprehensive Cloud Build CI/CD pipeline with automated testing, security scanning, multi-stage deployment, and rollback automation.

## What Has Been Created

This implementation provides a production-ready Cloud Build pipeline with the following components:

### Configuration Files (in `/config`)

#### Core Build Configuration
- **`cloudbuild.yaml`** (17-step pipeline)
  - Docker image build and push
  - Unit testing (Vitest)
  - Code linting and type checking
  - Container vulnerability scanning
  - OWASP dependency checking
  - SonarQube code quality analysis
  - SBOM generation
  - Staging deployment
  - Integration testing
  - Production deployment with approval gate
  - Smoke testing
  - Deployment verification
  - Artifact generation

#### Deployment Configurations
- **`staging/deployment.yaml`**
  - 2 replicas with HPA (2-5 range)
  - 512Mi memory, 500m CPU limits
  - Rolling update strategy
  - Health checks and probes
  - Pod Disruption Budget
  - Debug-level logging
  - Non-root security context

- **`prod/deployment.yaml`**
  - 3 replicas with HPA (3-10 range)
  - 1Gi memory, 1000m CPU limits
  - Rolling update strategy (zero-downtime)
  - Startup, liveness, and readiness probes
  - Pod Disruption Budget (minimum 2 pods)
  - Node affinity for production nodes
  - Production logging
  - Metrics and tracing enabled
  - BackendConfig for load balancer

#### RBAC & Security
- **`rbac.yaml`**
  - ServiceAccounts for staging and production
  - Roles with minimum required permissions
  - RoleBindings
  - Cluster roles for metrics

- **`configmap.yaml`**
  - Environment-specific configurations
  - API endpoints
  - Logging configurations
  - Cache settings
  - Debug flags

- **`security-scanning.yaml`**
  - Network policies
  - Pod Security Policies
  - Resource quotas
  - Limit ranges
  - DependencyCheck rules
  - SAST security rules
  - SBOM configuration

#### Kustomization Files
- **`staging/kustomization.yaml`**
  - Staging-specific overlays
  - Config generators
  - Image patching
  - Replica configuration

- **`prod/kustomization.yaml`**
  - Production-specific overlays
  - Security hardening
  - Resource quotas
  - Network policies
  - Replica configuration

### Docker Configuration
- **`Dockerfile`**
  - Multi-stage build
  - Node.js 18-Alpine base
  - Security hardened
  - Non-root user execution
  - Health checks
  - Optimized for size

### Deployment Scripts (in `/scripts`)

#### Core Scripts
- **`rollback.sh`** (executable)
  - Automatic rollback to previous version
  - Manual rollback by image SHA
  - Health check verification
  - Deployment report generation
  - Extensive error checking
  - Detailed logging

- **`deploy.sh`** (executable)
  - Pre-deployment validation
  - Credential setup
  - Configuration verification
  - Health checks
  - Automatic rollback on failure
  - Dry-run capability

- **`health-check.sh`** (executable)
  - Health endpoint checking
  - Readiness probe verification
  - Startup probe validation
  - Configurable retries

- **`startup.sh`** (executable)
  - Container initialization
  - Environment setup
  - Database migrations support
  - Graceful shutdown handling

### Documentation

- **`CLOUD_BUILD_GUIDE.md`**
  - Complete implementation guide
  - Architecture diagrams
  - Setup instructions
  - Build pipeline explanation
  - Security scanning details
  - Deployment strategies
  - Rollback procedures
  - Monitoring setup
  - Troubleshooting guide

- **`README.md`**
  - Quick start guide
  - Directory structure
  - Configuration overview
  - Scripts usage
  - Environment setup
  - Best practices
  - Cost optimization

## File Summary

```
Total Files Created: 18

Configuration Directory (/config):
├── cloudbuild.yaml                    (570 lines) - Main build orchestration
├── configmap.yaml                     (60 lines)  - App configuration
├── rbac.yaml                          (100 lines) - RBAC definitions
├── security-scanning.yaml             (200 lines) - Security policies
├── staging/
│   ├── deployment.yaml                (170 lines) - Staging deployment
│   └── kustomization.yaml             (60 lines)  - Staging overlay
├── prod/
│   ├── deployment.yaml                (250 lines) - Production deployment
│   └── kustomization.yaml             (80 lines)  - Production overlay
├── CLOUD_BUILD_GUIDE.md              (800+ lines) - Comprehensive guide
└── README.md                          (400+ lines) - Quick reference

Root Directory:
├── Dockerfile                         (70 lines)  - Multi-stage build
└── CLOUD_BUILD_SETUP.md              (this file) - Setup overview

Scripts Directory (/scripts):
├── rollback.sh                        (450 lines) - Rollback automation
├── deploy.sh                          (350 lines) - Deployment automation
├── health-check.sh                    (50 lines)  - Health checks
└── startup.sh                         (80 lines)  - Container startup
```

## Key Features

### Automated Testing
- Unit tests with Vitest
- Code linting with ESLint
- TypeScript type checking
- Integration tests
- Smoke tests (staging and production)

### Security Scanning
- Container vulnerability scanning
- OWASP DependencyCheck
- SonarQube code quality analysis
- SAST rules for security issues
- Network policies
- Pod Security Policies
- Resource quotas and limits

### Multi-Stage Deployment
1. **Build Phase**: Docker build and push
2. **Test Phase**: Unit and lint tests (parallel)
3. **Security Phase**: Vulnerability and code analysis
4. **Staging Phase**: Deploy to staging, run integration tests
5. **Approval Gate**: Manual approval required
6. **Production Phase**: Deploy to production with verification

### Rollback Automation
- Automatic rollback on failure
- Manual rollback to previous version
- Rollback by specific image SHA
- Health check verification after rollback
- Deployment history tracking
- Comprehensive reporting

### High Availability
- Rolling updates (zero-downtime)
- Pod Disruption Budgets
- Horizontal Pod Autoscaling (HPA)
- Pod anti-affinity (spread across nodes)
- Liveness and readiness probes
- Startup probes for slow containers

### Monitoring & Logging
- Structured JSON logging
- Prometheus metrics exposure
- Event logging
- Deployment history
- Build artifacts and reports
- SBOM generation

## Quick Start Guide

### 1. Prerequisites Setup

```bash
# Set environment variables
export PROJECT_ID="your-gcp-project"
export REGION="us-central1"
export ZONE="us-central1-a"

# Enable required GCP APIs
gcloud services enable \
  cloudbuild.googleapis.com \
  container.googleapis.com \
  artifactregistry.googleapis.com

# Create Artifact Registry
gcloud artifacts repositories create admin-console-repo \
  --repository-format=docker \
  --location=$REGION
```

### 2. Configure Kubernetes Namespaces

```bash
# Create namespaces
kubectl create namespace staging
kubectl create namespace production

# Apply RBAC
kubectl apply -f config/rbac.yaml

# Apply ConfigMaps
kubectl apply -f config/configmap.yaml
```

### 3. Update Cloud Build Configuration

```bash
# Edit cloudbuild.yaml and update:
# - PROJECT_ID
# - _REGION
# - _ZONE
# - _STAGING_CLUSTER
# - _PROD_CLUSTER
# - _STAGING_API_URL
# - _PROD_API_URL
```

### 4. Create Cloud Build Trigger

```bash
gcloud builds triggers create github \
  --repo-name=your-repo \
  --repo-owner=your-org \
  --branch-pattern="^main$" \
  --build-config=config/cloudbuild.yaml
```

### 5. Deploy to Staging

```bash
kubectl apply -f config/staging/deployment.yaml
```

### 6. Deploy to Production

```bash
# After approval gate
kubectl apply -f config/prod/deployment.yaml
```

## Configuration Highlights

### Build Pipeline Configuration

The `cloudbuild.yaml` includes:

```yaml
steps:
  1. Docker build and push
  2. Push image to Artifact Registry
  3. Unit tests (parallel)
  4. Linting and type checks (parallel)
  5. Container vulnerability scanning
  6. OWASP dependency check
  7. SonarQube analysis
  8. SBOM generation
  9. Deploy to staging
  10. Update staging deployment
  11. Run smoke tests
  12. Approval gate (manual)
  13. Deploy to production
  14. Update production deployment
  15. Verify production rollout
  16. Run production smoke tests
  17. Create deployment report

Options:
  - Machine type: N1_HIGHCPU_8
  - Timeout: 3600s (1 hour)
  - Cloud logging enabled
  - Artifacts: Reports, SBOMs, security reports
```

### Deployment Configuration

**Staging Environment:**
- Replicas: 2 (HPA: 2-5)
- Memory: 512Mi requests, 512Mi limits
- CPU: 100m requests, 500m limits
- Health checks: liveness, readiness
- Rolling update: maxSurge=1, maxUnavailable=0

**Production Environment:**
- Replicas: 3 (HPA: 3-10)
- Memory: 512Mi requests, 1Gi limits
- CPU: 250m requests, 1000m limits
- Health checks: startup, liveness, readiness
- Rolling update: zero-downtime
- Pod Disruption Budget: minimum 2
- Node affinity: production nodes only

### Security Configuration

**Network Policies:**
- Ingress from load balancer only
- Egress to external APIs (443, 80)
- DNS resolution (UDP 53)
- Database connectivity (TCP 5432)

**Pod Security:**
- Non-root user (UID 1000)
- Read-only root filesystem
- No privilege escalation
- Limited capabilities
- Resource quotas
- Limit ranges

## Monitoring & Troubleshooting

### View Build Status

```bash
# List recent builds
gcloud builds list --limit=10

# View build logs
gcloud builds log BUILD_ID --stream

# Get build details
gcloud builds describe BUILD_ID
```

### Monitor Deployments

```bash
# Check deployment status
kubectl get deployment admin-console -n production

# View pod logs
kubectl logs -n production deployment/admin-console -f

# Describe deployment
kubectl describe deployment admin-console -n production

# Watch rollout
kubectl rollout status deployment/admin-console -n production --watch
```

### Troubleshoot Issues

```bash
# Check pod events
kubectl describe pod POD_NAME -n NAMESPACE

# View all events
kubectl get events -n NAMESPACE --sort-by='.lastTimestamp'

# Check resource usage
kubectl top pods -n NAMESPACE -l app=admin-console

# Check HPA status
kubectl get hpa -n NAMESPACE
```

## Rollback Procedures

### Automatic Rollback

The pipeline automatically triggers rollback when:
- Deployment verification fails
- Health checks fail
- Production smoke tests fail

### Manual Rollback

```bash
# Rollback to previous version
./scripts/rollback.sh

# Rollback to specific image
./scripts/rollback.sh -s abc123def456

# View rollback status
./scripts/rollback.sh --status
```

## Best Practices Implemented

✓ **Security**
- Non-root user execution
- Read-only root filesystem
- Resource limits and requests
- Network policies
- Pod Security Policies
- Regular vulnerability scanning
- SPDX SBOM generation

✓ **Reliability**
- Rolling updates (zero-downtime)
- Multiple replicas
- Health checks
- Pod Disruption Budgets
- HPA for automatic scaling
- Health check verification

✓ **Performance**
- Multi-stage Docker build
- Caching strategies
- Resource optimization
- HPA auto-scaling
- Connection pooling ready

✓ **Compliance**
- OWASP compliance checks
- Code quality analysis
- Audit logging
- Deployment tracking
- SBOM generation
- Security scanning

✓ **Operations**
- Comprehensive logging
- Structured JSON logs
- Metrics exposure
- Health checks
- Deployment reports
- Rollback capability

## Integration Points

### GitHub Integration
- Automatic builds on push
- PR-based deployments
- Branch-based triggers
- Status checks

### GCP Integration
- Artifact Registry
- Cloud Build
- GKE Clusters
- Cloud Logging
- Cloud Monitoring
- Cloud Storage (artifacts)

### Kubernetes Integration
- GKE Deploy
- kubectl automation
- Helm charts (optional)
- Kustomize overlays
- Network policies
- RBAC

## Next Steps

1. **Customize Substitutions**: Update `cloudbuild.yaml` with your GCP project details
2. **Configure Secrets**: Set up secret management for sensitive data
3. **Set Up Monitoring**: Configure Cloud Monitoring alerts
4. **Test Pipeline**: Run initial build to verify setup
5. **Document Runbooks**: Create operational runbooks for your team
6. **Enable Logging**: Configure centralized logging

## Support Resources

- **Cloud Build Guide**: See `config/CLOUD_BUILD_GUIDE.md`
- **Configuration Guide**: See `config/README.md`
- **GCP Documentation**: https://cloud.google.com/build/docs
- **GKE Best Practices**: https://cloud.google.com/kubernetes-engine/docs/best-practices
- **Kubernetes Docs**: https://kubernetes.io/docs/

## Summary

This complete implementation provides:

- 18+ configuration and script files
- 17-step automated build pipeline
- Multi-stage deployment (staging → production)
- Comprehensive security scanning
- Automatic rollback capability
- Zero-downtime deployments
- Complete documentation
- Production-ready configurations

All files are organized, documented, and ready for customization to your specific environment and requirements.
