# Cloud Build Pipeline Guide

Comprehensive guide for the automated Cloud Build CI/CD pipeline with automated testing, security scanning, multi-stage deployment, and rollback automation.

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Setup Instructions](#setup-instructions)
4. [Build Pipeline](#build-pipeline)
5. [Security Scanning](#security-scanning)
6. [Deployment Strategy](#deployment-strategy)
7. [Rollback Procedures](#rollback-procedures)
8. [Monitoring and Alerts](#monitoring-and-alerts)
9. [Troubleshooting](#troubleshooting)

## Overview

This Cloud Build pipeline automates the entire application lifecycle from code commit to production deployment with comprehensive testing, security scanning, and built-in rollback capabilities.

### Key Features

- **Automated Testing**: Unit tests, linting, and type checking
- **Security Scanning**: Container vulnerability scanning, dependency checks, SPDX SBOM generation
- **Multi-Stage Deployment**: Staging → Production pipeline with approval gates
- **Rollback Automation**: Instant rollback to previous stable version
- **Health Checks**: Automated smoke tests after deployment
- **Compliance**: OWASP compliance, code quality analysis, security audits

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Git Push / PR                            │
└────────────────────────────┬────────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────────┐
│                    PHASE 1: BUILD & TEST                        │
├─────────────────────────────────────────────────────────────────┤
│  ✓ Docker Build                                                 │
│  ✓ Push to Artifact Registry                                    │
│  ✓ Unit Tests (Vitest)                                          │
│  ✓ Linting (ESLint)                                             │
│  ✓ Type Checking (TypeScript)                                   │
└────────────────────────────┬────────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────────┐
│                  PHASE 2: SECURITY SCANNING                     │
├─────────────────────────────────────────────────────────────────┤
│  ✓ Container Vulnerability Scan                                 │
│  ✓ Dependency Analysis (OWASP DependencyCheck)                 │
│  ✓ SonarQube Analysis                                           │
│  ✓ SBOM Generation (Syft)                                       │
└────────────────────────────┬────────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────────┐
│               PHASE 3: STAGING DEPLOYMENT                       │
├─────────────────────────────────────────────────────────────────┤
│  ✓ Deploy to Staging Cluster                                    │
│  ✓ Integration Tests                                            │
│  ✓ Smoke Tests                                                  │
└────────────────────────────┬────────────────────────────────────┘
                             │
                    (APPROVAL GATE)
                             │
┌────────────────────────────▼────────────────────────────────────┐
│             PHASE 4: PRODUCTION DEPLOYMENT                      │
├─────────────────────────────────────────────────────────────────┤
│  ✓ Deploy to Production Cluster                                 │
│  ✓ Verify Rollout                                               │
│  ✓ Production Smoke Tests                                       │
│  ✓ Deployment Report Generation                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Setup Instructions

### Prerequisites

- GCP Project with Cloud Build enabled
- Kubernetes clusters (staging and production)
- Artifact Registry repository
- GKE clusters with proper networking
- Required IAM roles and permissions

### 1. Enable Required APIs

```bash
gcloud services enable \
  cloudbuild.googleapis.com \
  container.googleapis.com \
  artifactregistry.googleapis.com \
  clouddeploy.googleapis.com \
  containerregistry.googleapis.com
```

### 2. Create Artifact Registry Repository

```bash
gcloud artifacts repositories create admin-console-repo \
  --repository-format=docker \
  --location=us-central1 \
  --description="Admin Console Docker Repository"
```

### 3. Configure Service Account Permissions

```bash
# Create service account for Cloud Build
gcloud iam service-accounts create cloud-build-sa \
  --display-name="Cloud Build Service Account"

# Grant necessary permissions
gcloud projects add-iam-policy-binding ${PROJECT_ID} \
  --member="serviceAccount:cloud-build-sa@${PROJECT_ID}.iam.gserviceaccount.com" \
  --role="roles/container.developer"

gcloud projects add-iam-policy-binding ${PROJECT_ID} \
  --member="serviceAccount:cloud-build-sa@${PROJECT_ID}.iam.gserviceaccount.com" \
  --role="roles/artifactregistry.writer"
```

### 4. Create Cloud Build Configuration

```bash
# Copy Cloud Build configuration
cp config/cloudbuild.yaml .

# Update substitutions in cloudbuild.yaml
# Replace PROJECT_ID, regions, and cluster names
```

### 5. Create Build Trigger

```bash
gcloud builds triggers create github \
  --repo-name=your-repo \
  --repo-owner=your-org \
  --branch-pattern="^main$" \
  --build-config=cloudbuild.yaml \
  --service-account=cloud-build-sa@${PROJECT_ID}.iam.gserviceaccount.com
```

### 6. Setup Namespaces

```bash
# Create staging namespace
kubectl create namespace staging
kubectl label namespace staging environment=staging

# Create production namespace
kubectl create namespace production
kubectl label namespace production environment=production
```

## Build Pipeline

### Step-by-Step Execution

#### Phase 1: Build & Test (Parallel Execution)

1. **Build Docker Image** (Step 1)
   - Builds multi-stage Docker image
   - Optimized for size and security
   - Non-root user execution

2. **Push to Artifact Registry** (Step 2)
   - Pushes built image with SHA tag
   - Tags as 'latest' for quick reference
   - Immutable image references

3. **Unit Tests** (Step 3 - Parallel)
   - Runs Vitest suite
   - Coverage reporting
   - Fails build if tests fail

4. **Linting & Type Checks** (Step 4 - Parallel)
   - ESLint configuration validation
   - TypeScript type checking
   - Code quality enforcement

#### Phase 2: Security Scanning

1. **Container Vulnerability Scanning** (Step 5)
   - Google Cloud scanning
   - Identifies known vulnerabilities
   - Requires successful push

2. **OWASP Dependency Check** (Step 6)
   - Checks for known vulnerabilities
   - Generates JSON report
   - Continues on failure (non-blocking)

3. **SonarQube Analysis** (Step 7)
   - Code quality metrics
   - Security hotspot analysis
   - Technical debt assessment
   - Optional (requires SonarQube setup)

4. **SBOM Generation** (Step 8)
   - Creates Software Bill of Materials
   - JSON and SPDX formats
   - Full dependency tracking

#### Phase 3: Staging Deployment

1. **Deploy to Staging** (Step 9)
   - Uses GKE Deploy
   - Rolling update strategy
   - Waits for previous steps

2. **Run Integration Tests** (Step 10-11)
   - Updates deployment image
   - Runs smoke tests
   - Continues on failure

#### Phase 4: Production Deployment

1. **Approval Gate** (Step 12)
   - Manual approval required
   - Prevents automatic production pushes
   - Can be bypassed with `--continue`

2. **Deploy to Production** (Step 13)
   - Deploys to production cluster
   - Rolling update with no downtime
   - Maintained pod disruption budget

3. **Verify Rollout** (Step 14-15)
   - Verifies all pods are ready
   - 5-minute timeout
   - Triggers rollback if fails

4. **Production Smoke Tests** (Step 16)
   - Tests against production environment
   - Non-blocking (continues on failure)
   - Quick sanity checks

## Security Scanning

### Container Vulnerability Scanning

Scans Docker image for known vulnerabilities from CVE databases.

```bash
# Manual scanning
gcloud container images scan IMAGE_URL
```

### OWASP Dependency Check

Identifies project dependencies with known vulnerabilities.

```bash
# Report location
gs://your-bucket/builds/BUILD_ID/dependency-check-report.json
```

### SonarQube Integration

Code quality and security analysis. Requires SonarQube instance.

**Setup:**
```bash
# Set environment variables in Cloud Build
_SONAR_HOST_URL=https://sonarqube.example.com
_SONAR_TOKEN=<your-token>
```

### Software Bill of Materials (SBOM)

Generated in both JSON and SPDX formats for compliance.

```bash
# Generated artifacts
gs://your-bucket/builds/BUILD_ID/sbom.json
gs://your-bucket/builds/BUILD_ID/sbom.spdx
```

## Deployment Strategy

### Staging Environment

- **Replicas**: 2
- **Resource Limits**: 512Mi memory, 500m CPU
- **Update Strategy**: Rolling update (maxSurge: 1, maxUnavailable: 0)
- **HPA**: 2-5 replicas based on CPU/memory
- **Purpose**: Testing and validation

Configuration: `/config/staging/deployment.yaml`

### Production Environment

- **Replicas**: 3 (minimum)
- **Resource Limits**: 1Gi memory, 1000m CPU
- **Update Strategy**: Rolling update (zero-downtime)
- **HPA**: 3-10 replicas based on CPU/memory
- **Pod Disruption Budget**: Minimum 2 available
- **Node Affinity**: Production node pool only

Configuration: `/config/prod/deployment.yaml`

### Rolling Update Strategy

```yaml
strategy:
  type: RollingUpdate
  rollingUpdate:
    maxSurge: 1        # One extra pod during update
    maxUnavailable: 0  # Zero downtime
```

### Health Checks

**Liveness Probe**: Checks if container is alive
- Path: `/health`
- Interval: 10 seconds
- Failure threshold: 3

**Readiness Probe**: Checks if container is ready for traffic
- Path: `/ready`
- Interval: 5 seconds
- Failure threshold: 2

**Startup Probe**: Checks if application has started (production only)
- Path: `/startup`
- Maximum 30 attempts (5 minutes)

## Rollback Procedures

### Automatic Rollback

Triggered automatically when:
- Deployment verification fails
- Health checks fail repeatedly
- Production smoke tests fail

### Manual Rollback

#### Using Script

```bash
# Rollback to previous version
./scripts/rollback.sh

# Rollback to specific image SHA
./scripts/rollback.sh -s abc123def456

# Rollback to specific revision
./scripts/rollback.sh --revision 5

# Check deployment history
./scripts/rollback.sh --history

# Show current status
./scripts/rollback.sh --status
```

#### Using kubectl

```bash
# View rollout history
kubectl rollout history deployment/admin-console -n production

# Undo to previous version
kubectl rollout undo deployment/admin-console -n production

# Undo to specific revision
kubectl rollout undo deployment/admin-console -n production --to-revision=5

# Watch rollout progress
kubectl rollout status deployment/admin-console -n production --watch
```

### Rollback Validation

The script validates:
1. Previous revision availability
2. Pod readiness after rollback
3. Health check endpoints
4. Service connectivity
5. Recent error events

### Rollback Workflow

```
┌─────────────────────────────┐
│  Identify Problem           │
│  (Health check failed)      │
└────────────┬────────────────┘
             │
┌────────────▼────────────────┐
│  Trigger Rollback           │
│  ./scripts/rollback.sh      │
└────────────┬────────────────┘
             │
┌────────────▼────────────────┐
│  Get Previous Revision      │
│  (from kubectl history)     │
└────────────┬────────────────┘
             │
┌────────────▼────────────────┐
│  Execute Rollback           │
│  (kubectl rollout undo)     │
└────────────┬────────────────┘
             │
┌────────────▼────────────────┐
│  Monitor Rollout Status     │
│  (wait for pods ready)      │
└────────────┬────────────────┘
             │
┌────────────▼────────────────┐
│  Verify Health              │
│  (check endpoints)          │
└────────────┬────────────────┘
             │
┌────────────▼────────────────┐
│  Generate Report            │
│  (document the issue)       │
└─────────────────────────────┘
```

## Monitoring and Alerts

### Cloud Build Logs

```bash
# View build logs
gcloud builds log BUILD_ID

# Stream build logs
gcloud builds log BUILD_ID --stream

# List recent builds
gcloud builds list --limit=10
```

### Deployment Logs

```bash
# View Pod logs
kubectl logs -n production deployment/admin-console --tail=100 -f

# View specific pod
kubectl logs -n production POD_NAME

# View previous pod logs (if crashed)
kubectl logs -n production POD_NAME --previous
```

### Metrics and Monitoring

```bash
# View resource usage
kubectl top nodes
kubectl top pods -n production -l app=admin-console

# Get deployment events
kubectl describe deployment admin-console -n production

# Check HPA status
kubectl get hpa -n production
```

### Cloud Logging

```bash
# Query build logs in Cloud Logging
gcloud logging read "resource.type=build" \
  --format=json \
  --limit=10 \
  --freshness=24h
```

### Alerts Configuration

Set up Google Cloud Monitoring alerts for:
- Build failures
- Deployment failures
- High error rates
- Pod crashes
- Resource exhaustion

```bash
# Create alert policy
gcloud alpha monitoring policies create \
  --notification-channels=CHANNEL_ID \
  --display-name="Build Failure Alert" \
  --condition-display-name="Build Status" \
  --condition-threshold-value=1
```

## Troubleshooting

### Build Failures

#### Docker Build Fails

```bash
# Check Docker build locally
docker build -t admin-console:test .

# View build step logs
gcloud builds log BUILD_ID --stream
```

**Common Issues:**
- Missing Dockerfile
- Invalid base image
- Insufficient resources
- Network connectivity

#### Test Failures

```bash
# Run tests locally
npm test -- --run

# Run linting
npm run lint

# Run type checks
npm run type-check
```

**Common Issues:**
- Missing dependencies
- Test timeouts
- Environment variables not set
- Port conflicts

### Deployment Issues

#### Pod Not Starting

```bash
# Check pod status
kubectl describe pod POD_NAME -n NAMESPACE

# Check logs
kubectl logs POD_NAME -n NAMESPACE

# Check events
kubectl get events -n NAMESPACE --sort-by='.lastTimestamp'
```

**Common Issues:**
- Image not found (incorrect image name)
- Resource limits too low
- Image pull failures
- Volume mounting issues

#### Service Not Available

```bash
# Check service
kubectl get svc admin-console -n NAMESPACE

# Check endpoints
kubectl get endpoints admin-console -n NAMESPACE

# Test connectivity
kubectl run -it --rm debug --image=alpine --restart=Never -- \
  wget http://admin-console:80
```

**Common Issues:**
- Service selector mismatch
- Port configuration errors
- Network policy restrictions
- Load balancer not configured

#### Health Check Failures

```bash
# Test health endpoint
kubectl exec POD_NAME -n NAMESPACE -- \
  curl -v http://localhost:3000/health

# Check health check configuration
kubectl get deployment admin-console -n NAMESPACE -o yaml | \
  grep -A 10 "livenessProbe"
```

**Common Issues:**
- Application not listening
- Health endpoint not implemented
- Port mismatch
- Too aggressive timeout

### Rollback Issues

#### Rollback Fails

```bash
# Check rollout history
kubectl rollout history deployment/admin-console -n NAMESPACE

# Check current status
kubectl rollout status deployment/admin-console -n NAMESPACE

# Check events
kubectl describe deployment admin-console -n NAMESPACE
```

**Common Issues:**
- No previous revision available
- Insufficient cluster resources
- Image pull failures
- Persistent storage issues

### Security Scanning Issues

#### Vulnerability Scanning Timeout

```bash
# Increase timeout in cloudbuild.yaml
timeout: '7200s'  # 2 hours

# Check scan status
gcloud container images describe IMAGE_URL --show-package-vulnerability
```

#### SonarQube Connection Issues

```bash
# Verify SonarQube endpoint
curl -u admin:admin https://sonarqube.example.com/api/system/health

# Check credentials
echo $SONAR_TOKEN
```

### Performance Issues

#### Slow Build Times

```bash
# Enable caching in cloudbuild.yaml
# Use machine type: N1_HIGHCPU_8

# Check build step logs
gcloud builds log BUILD_ID | grep "Step"
```

#### High Resource Usage

```bash
# Check pod resource requests
kubectl describe pod POD_NAME -n NAMESPACE

# Monitor HPA scaling
kubectl get hpa admin-console -n NAMESPACE -w

# Check metrics
kubectl top pods -n NAMESPACE -l app=admin-console
```

## Additional Resources

- [Cloud Build Documentation](https://cloud.google.com/build/docs)
- [GKE Best Practices](https://cloud.google.com/kubernetes-engine/docs/best-practices)
- [Container Security](https://cloud.google.com/container-analysis)
- [Kubernetes Deployment Strategies](https://kubernetes.io/docs/concepts/workloads/controllers/deployment/)

## Support and Escalation

### Contact Information

- **Build Issues**: Cloud Build team
- **Deployment Issues**: Platform team
- **Security Concerns**: Security team

### Escalation Path

1. Check logs and troubleshooting guide
2. Review recent changes in git history
3. Contact platform team
4. Escalate to incident management if P1
