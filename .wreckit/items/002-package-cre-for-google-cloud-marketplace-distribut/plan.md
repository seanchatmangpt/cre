# Package CRE for Google Cloud Marketplace distribution Implementation Plan

## Implementation Plan Title
Google Cloud Marketplace Packaging for CRE (Common Runtime Environment)

## Overview
Package CRE for distribution on Google Cloud Marketplace, enabling GCP customers to deploy CRE via a one-click Marketplace experience with automated deployment, billing integration, and GCP-native observability.

## Current State
CRE has a solid foundation for Marketplace deployment but lacks critical Marketplace-specific packaging components:

**Existing Strengths:**
- **Multi-arch Dockerfile** (`Dockerfile` 1-314): Supports linux/amd64 and linux/arm64 with Rust NIFs, OTP 28, security hardening (non-root user, dropped capabilities)
- **Comprehensive Helm chart** (`k8s/charts/cre/`): Production-ready with StatefulSet, HPA, PDB, ConfigMaps, proper resource limits
- **Status endpoint** (`src/http/cre_status_handler.erl`): Returns CRE master status at `/status.json`
- **Container security**: Non-root user (Dockerfile:236), read-only root filesystem support, dropped capabilities (Dockerfile:39-41)

**Critical Gaps:**
- **No `application.yaml`**: GKE Application schema required for Marketplace UI parameter input
- **Incomplete `cloudbuild.yaml`**: Only 3 lines (comments), no actual build steps
- **Health check integration broken**: Helm values reference `/status.json` for probes (values.yaml:206, 215, 224), but GCP-ready health endpoints (`/health`, `/ready`, `/startup` in `cre_health.erl`) are NOT integrated into the Cowboy routing (see `src/app/cre.erl:339`)
- **No secret management**: `existingSecret` in values.yaml (line 295) violates Marketplace constraint "no secrets in values.yaml"
- **No Marketplace billing integration**: Missing usage metering or BYOL license model
- **No Terraform modules**: Research document referenced non-existent Terraform infrastructure
- **No `k8s/gcp/` directory**: GKE-specific manifests don't exist
- **Missing immutable version tags**: CI/CD uses mutable tags
- **No ConfigMap hot-reload**: Configuration changes require pod restart

## Desired End State
CRE can be deployed from Google Cloud Marketplace with one click, passing technical and security review, with:
- GKE Application schema (`application.yaml`) for Marketplace UI parameterization
- Health checks functional with proper endpoints
- Marketplace-compliant secret management via External Secrets Operator
- Multi-arch container images in Artifact Registry with immutable tags
- BYOL licensing model for v1
- Complete Cloud Build pipeline with security scanning
- GKE-specific deployment manifests

### Key Discoveries:
- **Health endpoint CRITICAL BUG**: Helm chart probes use `/status.json` (values.yaml:206, 215, 224) but this is a STATUS endpoint, not a health endpoint. GCP-ready health endpoints exist (`cre_health.erl` lines 113-207) but are NOT routed in `src/app/cre.erl:339`. This WILL cause deployments to fail health checks.
- **Route format**: Cowboy uses `/[status.json]` syntax (cre.erl:339) where `[]` denotes path binding, so actual endpoint is `/status.json` - verified in `cre_status_handler.erl`
- **No Terraform infrastructure**: Research document's claims about `terraform/gcp/main.tf` and modules are FALSE - these files don't exist
- **No GKE-specific manifests**: `k8s/gcp/` directory doesn't exist
- **Marketplace constraint violation**: Line 295 in values.yaml (`existingSecret: ""`) violates Marketplace requirement "no secrets in values.yaml"

## What We're NOT Doing
- **Multi-cloud marketplace support** (AWS, Azure Marketplaces) - out of scope for v1
- **VM-based deployment** (Compute Engine) - out of scope for v1
- **Managed SaaS offering** - multi-tenant hosted service
- **Usage-based billing metering** - using BYOL model instead for v1
- **Multi-region deployment** - single-region only for v1
- **Hot-reload configuration** - ConfigMap changes require pod restart for v1

## Implementation Approach

**High-level Strategy:**
1. Fix critical health check blocker (must deploy)
2. Create Marketplace-required packaging (application.yaml, billing model)
3. Implement build/deploy pipeline (Cloud Build, Artifact Registry)
4. Prepare Marketplace submission artifacts (security scanning, documentation)

**Rationale:**
- Prioritize health check fix first to ensure deployments work
- Use BYOL model for v1 to simplify billing integration
- Leverage existing Helm chart as foundation, add Marketplace layer
- Focus on GKE-only deployment (no VM support) to minimize scope

---

## Phases

### Phase 1: Foundation & Critical Fixes

#### Overview
Fix the health check integration bug and establish Marketplace packaging structure with application.yaml.

#### Changes Required:

##### 1. Fix Health Check Integration (CRITICAL)

**Issue**: Helm chart uses `/status.json` for health probes, but this is not a health endpoint. GCP-ready health endpoints (`/health`, `/ready`, `/startup`) exist in `cre_health.erl` but are not routed.

**Option A: Route health endpoints (RECOMMENDED)**

**File**: `src/app/cre.erl`
**Line**: 339
**Changes**: Add health check routes to Cowboy dispatch table

```erlang
Dispatch =
    cowboy_router:compile(
      [{'_', [
              {"/health", cre_health, []},
              {"/ready", cre_health, []},
              {"/startup", cre_health, []},
              {"/[status.json]", cre_status_handler, []},
              {"/history.json", cre_history_handler, []}
      ]}]),
```

**File**: `k8s/charts/cre/values.yaml`
**Lines**: 206, 215, 224
**Changes**: Update probe paths to use health endpoints

```yaml
probes:
  liveness:
    httpGet:
      path: /health
      port: http
    initialDelaySeconds: 30
    periodSeconds: 15
    timeoutSeconds: 5
    failureThreshold: 3

  readiness:
    httpGet:
      path: /ready
      port: http
    initialDelaySeconds: 10
    periodSeconds: 10
    timeoutSeconds: 3
    failureThreshold: 3

  startup:
    httpGet:
      path: /startup
      port: http
    initialDelaySeconds: 5
    periodSeconds: 5
    timeoutSeconds: 3
    failureThreshold: 30
```

**Verification**:
```bash
# Build and test locally
rebar3 compile
rebar3 shell

# Test endpoints (from another terminal)
curl http://localhost:4142/health
curl http://localhost:4142/ready
curl http://localhost:4142/startup

# Verify JSON response with status field
```

**Option B: Add /status.json alias (NOT RECOMMENDED)**
- Would require modifying `cre_health.erl` to add route alias
- Less semantically correct - `/status.json` should return status, not health

**Decision**: Use Option A - route health endpoints properly

##### 2. Create GKE Application Schema (REQUIRED for Marketplace)

**File**: `k8s/charts/cre/application.yaml` (NEW FILE)
**Changes**: Create GKE Application schema following Marketplace spec

```yaml
apiVersion: marketplace.cloud.google.com/v1alpha1
kind: Application
metadata:
  name: cre
  version: 0.3.0
spec:
  info:
    title: "CRE - Common Runtime Environment"
    description: "Workflow engine for YAWL with 36 workflow patterns"
    version: "0.3.0"
    logoUrl: "https://raw.githubusercontent.com/joergen7/cre/main/docs/images/cre-logo.png"
    documentationUrl: "https://github.com/joergen7/cre/blob/main/docs/DEPLOYMENT.md"
    supportUrl: "https://github.com/joergen7/cre/issues"

  runtime:
    policy:
      type: HELM
      helm:
        chartPath: ./
        parameters:
          # Cluster Configuration
          - name: replicaCount
            title: "Number of CRE Nodes"
            description: "Number of CRE replicas in the Mnesia cluster (minimum 3 for HA)"
            type: integer
            default: 3
            minimum: 1
            maximum: 10

          # Autoscaling
          - name: autoscaling.enabled
            title: "Enable Horizontal Pod Autoscaling"
            description: "Automatically scale CRE pods based on CPU/memory usage"
            type: boolean
            default: false

          - name: autoscaling.minReplicas
            title: "Minimum Replicas"
            description: "Minimum number of pods when autoscaling is enabled"
            type: integer
            default: 3
            minimum: 1
            maximum: 10
            constraints:
              - expression: "params.autoscaling.enabled == true"
                errorMessage: "This parameter is only applicable when autoscaling is enabled"

          - name: autoscaling.maxReplicas
            title: "Maximum Replicas"
            description: "Maximum number of pods when autoscaling is enabled"
            type: integer
            default: 10
            minimum: 3
            maximum: 50
            constraints:
              - expression: "params.autoscaling.enabled == true"
                errorMessage: "This parameter is only applicable when autoscaling is enabled"

          # Storage
          - name: persistence.enabled
            title: "Enable Persistent Storage"
            description: "Use persistent volumes for Mnesia data"
            type: boolean
            default: true

          - name: persistence.size
            title: "Storage Size"
            description: "Size of persistent volume for each CRE pod"
            type: string
            default: "10Gi"
            enum: ["5Gi", "10Gi", "20Gi", "50Gi", "100Gi"]
            constraints:
              - expression: "params.persistence.enabled == true"
                errorMessage: "This parameter is only applicable when persistence is enabled"

          - name: persistence.storageClass
            title: "Storage Class"
            description: "GKE StorageClass for persistent volumes"
            type: string
            default: "standard-rwo"
            enum: ["standard-rwo", "premium-rwo", "standard", "premium"]

          # Resources
          - name: resources.requests.cpu
            title: "CPU Request"
            description: "Minimum CPU guaranteed for each CRE pod"
            type: string
            default: "500m"
            enum: ["250m", "500m", "1000m", "2000m"]

          - name: resources.requests.memory
            title: "Memory Request"
            description: "Minimum memory guaranteed for each CRE pod"
            type: string
            default: "512Mi"
            enum: ["256Mi", "512Mi", "1Gi", "2Gi", "4Gi"]

          - name: resources.limits.cpu
            title: "CPU Limit"
            description: "Maximum CPU each CRE pod can use"
            type: string
            default: "2000m"
            enum: ["500m", "1000m", "2000m", "4000m"]

          - name: resources.limits.memory
            title: "Memory Limit"
            description: "Maximum memory each CRE pod can use"
            type: string
            default: "2Gi"
            enum: ["1Gi", "2Gi", "4Gi", "8Gi", "16Gi"]

          # Networking
          - name: service.type
            title: "Service Type"
            description: "Kubernetes service type for CRE API"
            type: string
            default: "ClusterIP"
            enum: ["ClusterIP", "LoadBalancer"]

          - name: ingress.enabled
            title: "Enable Ingress"
            description: "Create an Ingress resource for external access"
            type: boolean
            default: false

          # Licensing (BYOL)
          - name: license.acceptEula
            title: "Accept License Agreement"
            description: "I accept the Apache License 2.0 for CRE software"
            type: boolean
            default: false
            constraints:
              - expression: "params.license.acceptEula == true"
                errorMessage: "You must accept the license agreement to proceed"

          # CRE Configuration
          - name: config.logLevel
            title: "Log Level"
            description: "CRE logging verbosity"
            type: string
            default: "info"
            enum: ["debug", "info", "notice", "warning", "error"]

          - name: config.maxExecutions
            title: "Maximum Concurrent Executions"
            description: "Maximum number of workflow executions running concurrently"
            type: integer
            default: 1000
            minimum: 100
            maximum: 10000

          - name: config.sessionTimeout
            title: "Session Timeout"
            description: "Default session timeout in seconds"
            type: integer
            default: 3600
            minimum: 300
            maximum: 86400

          # TLS Configuration
          - name: tls.enabled
            title: "Enable TLS"
            description: "Enable TLS for CRE API (requires certificate management)"
            type: boolean
            default: false

          - name: tls.type
            title: "TLS Certificate Type"
            description: "Certificate management strategy"
            type: string
            default: "managed"
            enum: ["managed", "self-signed", "custom"]
            constraints:
              - expression: "params.tls.enabled == true"
                errorMessage: "This parameter is only applicable when TLS is enabled"

  output:
    - name: serviceUrl
      title: "CRE Service URL"
      description: "URL to access the CRE API"
      type: SERVICE_ENDPOINT
      reference:
        type: SERVICE
        name: cre

    - name: credentials
      title: "Access Credentials"
      description: "Instructions for accessing CRE"
      type: MARKDOWN
      content: |
        ## Accessing CRE

        CRE is now deployed in your GKE cluster.

        **Service Endpoint:** `${serviceUrl}`

        **Port:** 4142

        **Health Check:** `${serviceUrl}/health`

        **Status API:** `${serviceUrl}/status.json`

        To connect from your local machine:

        ```bash
        kubectl port-forward -n cre svc/cre 4142:4142
        curl http://localhost:4142/health
        ```

        To connect from within the cluster:

        ```bash
        kubectl run -it --rm debug --image=curlimages/curl --restart=Never -- \
          curl http://cre.cre.svc.cluster.local:4142/health
        ```

        See the [CRE Documentation](https://github.com/joergen7/cre/blob/main/docs/DEPLOYMENT.md) for more details.
```

##### 3. Remove Secret References from Helm Values

**File**: `k8s/charts/cre/values.yaml`
**Line**: 295
**Changes**: Remove `existingSecret` field (violates Marketplace constraints)

```yaml
# REMOVE THIS LINE:
# existingSecret: ""

# REPLACE with explicit marketplace note:
# marketplace:
#   # Secrets must be provided via External Secrets Operator or Secret Manager
#   # See README.md for secret management configuration
```

##### 4. Update Helm Chart Metadata

**File**: `k8s/charts/cre/Chart.yaml`
**Changes**: Add Marketplace annotations

```yaml
annotations:
  artifacthub.io/category: integration-delivery
  artifacthub.io/license: Apache-2.0
  artifacthub.io/links: |
    - name: Documentation
      url: https://github.com/joergen7/cre/blob/main/docs/DEPLOYMENT.md
    - name: API Reference
      url: https://github.com/joergen7/cre/blob/main/docs/API_REFERENCE.md
  artifacthub.io/operator: "false"
  artifacthub.io/prerelease: "false"
  artifacthub.io/containsSecurityUpdates: "false"
  # ADD Marketplace-specific annotations
  marketplace.cloud.google.com/deployer: "Helm"
  marketplace.cloud.google.com/verified: "false"
```

#### Success Criteria:

##### Automated Verification:
- [ ] Health check endpoints respond correctly:
  ```bash
  curl http://localhost:4142/health      # Returns 200 with {"status":"healthy"}
  curl http://localhost:4142/ready       # Returns 200 with {"status":"healthy"}
  curl http://localhost:4142/startup     # Returns 200 with {"status":"healthy"}
  ```
- [ ] `application.yaml` validates against GKE Application schema
- [ ] Helm chart lints successfully: `helm lint k8s/charts/cre`
- [ ] No secrets in values.yaml: `grep -i "secret\|password\|token" k8s/charts/cre/values.yaml` returns only comments

##### Manual Verification:
- [ ] Health endpoints return proper JSON with `status` field
- [ ] application.yaml renders correctly in test Marketplace UI
- [ ] Helm chart template renders successfully: `helm template cre k8s/charts/cre --debug`
- [ ] No regressions in existing /status.json endpoint

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Build Pipeline & Container Images

#### Overview
Implement complete Cloud Build pipeline with multi-arch builds, security scanning, and Artifact Registry publishing with immutable tags.

#### Changes Required:

##### 1. Implement Complete Cloud Build Configuration

**File**: `cloudbuild.yaml`
**Changes**: Replace 3-line stub with full Marketplace-compliant build pipeline

```yaml
# CRE Cloud Build Configuration for Google Cloud Marketplace
# Multi-arch builds with security scanning and immutable version tags

substitutions:
  _IMAGE_NAME: 'cre'
  _VERSION: '0.3.0'
  _COMMIT_SHA: '${COMMIT_SHA}'
  _BUILD_DATE: '${BUILD_DATE}'
  _ARTIFACT_REGISTRY: 'us-central1-docker.pkg.dev'
  _PROJECT_ID: '${PROJECT_ID}'

steps:
  # ============================================
  # Step 1: Build multi-arch container images
  # ============================================
  - name: 'gcr.io/cloud-builders/docker'
    id: 'build-amd64'
    waitFor: ['-']
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        docker buildx build \
          --platform linux/amd64 \
          --build-arg TARGETPLATFORM=linux/amd64 \
          --build-arg VERSION=${_VERSION} \
          --build-arg GIT_REVISION=${_COMMIT_SHA} \
          --build-arg BUILD_DATE=${_BUILD_DATE} \
          --tag ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-amd64 \
          --tag ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-amd64-${SHORT_SHA} \
          --load \
          .

  - name: 'gcr.io/cloud-builders/docker'
    id: 'build-arm64'
    waitFor: ['-']
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        docker buildx build \
          --platform linux/arm64 \
          --build-arg TARGETPLATFORM=linux/arm64 \
          --build-arg VERSION=${_VERSION} \
          --build-arg GIT_REVISION=${_COMMIT_SHA} \
          --build-arg BUILD_DATE=${_BUILD_DATE} \
          --tag ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-arm64 \
          --tag ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-arm64-${SHORT_SHA} \
          --load \
          .

  # ============================================
  # Step 2: Security scanning with Trivy
  # ============================================
  - name: 'aquasec/trivy:latest'
    id: 'scan-amd64'
    waitFor: ['build-amd64']
    args:
      - 'image'
      - '--format'
      - 'json'
      - '--output'
      - 'trivy-report-amd64.json'
      - '--severity'
      - 'CRITICAL,HIGH'
      - '${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-amd64'

  - name: 'aquasec/trivy:latest'
    id: 'scan-arm64'
    waitFor: ['build-arm64']
    args:
      - 'image'
      - '--format'
      - 'json'
      - '--output'
      - 'trivy-report-arm64.json'
      - '--severity'
      - 'CRITICAL,HIGH'
      - '${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-arm64'

  # ============================================
  # Step 3: Generate SBOM
  # ============================================
  - name: 'anchore/syft:latest'
    id: 'sbom-amd64'
    waitFor: ['build-amd64']
    args:
      - 'docker'
      - '${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-amd64'
      - '-o'
      - 'spdx-json'
      - '--file'
      - 'sbom-amd64.spdx.json'

  - name: 'anchore/syft:latest'
    id: 'sbom-arm64'
    waitFor: ['build-arm64']
    args:
      - 'docker'
      - '${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-arm64'
      - '-o'
      - 'spdx-json'
      - '--file'
      - 'sbom-arm64.spdx.json'

  # ============================================
  # Step 4: Push to Artifact Registry
  # ============================================
  - name: 'gcr.io/cloud-builders/docker'
    id: 'push-images'
    waitFor: ['scan-amd64', 'scan-arm64']
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        # Push architecture-specific images
        docker push ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-amd64
        docker push ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-amd64-${SHORT_SHA}
        docker push ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-arm64
        docker push ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-arm64-${SHORT_SHA}

        # Create and push multi-arch manifest (immutable version tag)
        docker manifest create ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION} \
          ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-amd64 \
          ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}-arm64

        docker manifest push ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}

        echo "Image digest:"
        docker manifest inspect ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}

  # ============================================
  # Step 5: Sign images with cosign
  # ============================================
  - name: 'gcr.io/projectsigstore/cosign:v2.0.0'
    id: 'sign-images'
    waitFor: ['push-images']
    entrypoint: 'sh'
    args:
      - '-c'
      - |
        cosign sign \
          --yes \
          ${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}

  # ============================================
  # Step 6: Upload scan reports and SBOMs
  # ============================================
  - name: 'gcr.io/cloud-builders/gsutil'
    id: 'upload-reports'
    waitFor: ['scan-amd64', 'scan-arm64', 'sbom-amd64', 'sbom-arm64']
    args:
      - '-m'
      - 'cp'
      - 'trivy-report-*.json'
      - 'sbom-*.spdx.json'
      - 'gs://${PROJECT_ID}-cre-reports/${_VERSION}/'

# Log all artifacts for Marketplace submission
logsBucket: '${PROJECT_ID}-cre-logs'

options:
  logging: GCS_ONLY
  machineType: 'E2_HIGHCPU_8'

timeout: '3600s'

# Output image digest for Marketplace deployment spec
images:
  - '${_ARTIFACT_REGISTRY}/${PROJECT_ID}/cre/${_IMAGE_NAME}:${_VERSION}'
```

##### 2. Create GKE-Specific Helm Values

**File**: `k8s/charts/cre/values-gke-marketplace.yaml` (NEW FILE)
**Changes**: GKE Marketplace-specific overrides

```yaml
# GKE Marketplace deployment values
# These override base values.yaml for Marketplace deployments

# Marketplace requires explicit image references with digests
image:
  repository: us-central1-docker.pkg.dev/${PROJECT_ID}/cre/cre
  tag: "${VERSION}"
  pullPolicy: Always

# GKE-specific node configuration
nodeSelector:
  cloud.google.com/gke-nodepool: cre-pool

# GKE Pod Security Standards
podSecurityContext:
  runAsNonRoot: true
  runAsUser: 1000
  fsGroup: 1000
  seccompProfile:
    type: RuntimeDefault

securityContext:
  allowPrivilegeEscalation: false
  readOnlyRootFilesystem: true
  capabilities:
    drop:
      - ALL

# GKE workload identity
serviceAccount:
  annotations:
    iam.gke.io/gcp-service-account: cre-workload-sa@${PROJECT_ID}.iam.gserviceaccount.com

# GKE StorageClass for persistent volumes
persistence:
  enabled: true
  storageClass: premium-rwo
  size: 10Gi

# GKE health probe configuration (uses /health, /ready, /startup)
probes:
  liveness:
    httpGet:
      path: /health
      port: http
    initialDelaySeconds: 30
    periodSeconds: 15
    timeoutSeconds: 5
    failureThreshold: 3

  readiness:
    httpGet:
      path: /ready
      port: http
    initialDelaySeconds: 10
    periodSeconds: 10
    timeoutSeconds: 3
    failureThreshold: 3

  startup:
    httpGet:
      path: /startup
      port: http
    initialDelaySeconds: 5
    periodSeconds: 5
    timeoutSeconds: 3
    failureThreshold: 30

# Enable GKE integration
monitoring:
  enabled: true

# Disable backup for Marketplace (user manages backups)
backup:
  enabled: false
```

##### 3. Update GitHub Actions Workflow

**File**: `.github/workflows/gcp-cloud-build.yml`
**Line**: 90
**Changes**: Update to reference complete cloudbuild.yaml

The workflow already references `cloudbuild.yaml` at line 90, but needs updates:
- Add immutable tag enforcement
- Add Marketplace-specific validation
- Add SBOM upload steps

```yaml
      # Add after line 83 (metadata extraction)
      - name: Enforce immutable version tags
        run: |
          if [[ "${{ github.event_name }}" == "tag" ]]; then
            # Only vX.Y.Z tags allowed for Marketplace
            if ! [[ "${VERSION}" =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
              echo "Error: Marketplace requires immutable version tags (vX.Y.Z)"
              exit 1
            fi
          fi

      # Add after line 95 (Cloud Build submission)
      - name: Validate Marketplace compliance
        run: |
          # Check for application.yaml
          if [ ! -f "k8s/charts/cre/application.yaml" ]; then
            echo "Error: application.yaml required for Marketplace"
            exit 1
          fi

          # Check for secrets in values.yaml
          if grep -q "existingSecret:" k8s/charts/cre/values.yaml; then
            echo "Error: Secrets not allowed in values.yaml for Marketplace"
            exit 1
          fi

          # Validate Helm chart
          helm lint k8s/charts/cre

          echo "Marketplace compliance checks passed"

      - name: Upload SBOM to release
        if: github.event_name == 'tag'
        uses: actions/upload-artifact@v4
        with:
          name: sbom-${{ steps.meta.outputs.version }}
          path: k8s/charts/cre/sbom-*.spdx.json
```

#### Success Criteria:

##### Automated Verification:
- [ ] Cloud Build completes successfully: `gcloud builds submit --config cloudbuild.yaml .`
- [ ] Multi-arch manifest created: `docker manifest inspect us-central1-docker.pkg.dev/$PROJECT_ID/cre/cre:$VERSION`
- [ ] Trivy scan passes (no CRITICAL vulnerabilities)
- [ ] SBOM generated in SPDX format
- [ ] Images signed with cosign: `cosign verify us-central1-docker.pkg.dev/$PROJECT_ID/cre/cre:$VERSION`

##### Manual Verification:
- [ ] Images pulled from Artifact Registry run successfully: `docker run --rm us-central1-docker.pkg.dev/$PROJECT_ID/cre/cre:$VERSION`
- [ ] Health endpoints respond: `docker run --rm -p 4142:4142 us-central1-docker.pkg.dev/$PROJECT_ID/cre/cre:$VERSION && curl http://localhost:4142/health`
- [ ] SBOM is valid SPDX JSON

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Marketplace Deployment Artifacts

#### Overview
Create Marketplace deployment specification, BYOL licensing model, and GKE-specific manifests.

#### Changes Required:

##### 1. Create Marketplace Deployment Spec

**File**: `marketplace/deployer.yaml` (NEW FILE)
**Changes**: Marketplace deployment specification

```yaml
# CRE Marketplace Deployment Specification
# This file defines how CRE is deployed from Google Cloud Marketplace

apiVersion: marketplace.cloud.google.com/v1beta1
kind: MarketplaceDeployment
metadata:
  name: cre-deployment
  namespace: cre
spec:
  application: cre
  version: 0.3.0

  # Billing model: BYOL (Bring Your Own License)
  billing:
    type: BYOL
    license: Apache-2.0

  # Deployment parameters
  cluster:
    type: GKE
    version: ">= 1.25.0"

  # Helm chart reference
  chart:
    name: cre
    repository: https://storage.googleapis.com/cre-marketplace/charts
    version: 0.3.0

  # Default values (can be overridden by user in Marketplace UI)
  values:
    replicaCount: 3
    persistence:
      enabled: true
      size: 10Gi
      storageClass: standard-rwo
    resources:
      requests:
        cpu: 500m
        memory: 512Mi
      limits:
        cpu: 2000m
        memory: 2Gi
    monitoring:
      enabled: true

  # Post-deployment instructions
  output:
    - name: clusterEndpoint
      type: SERVICE_ENDPOINT
      reference:
        type: SERVICE
        name: cre
```

##### 2. Create GKE Deployment Manifests

**File**: `k8s/gke/00-namespace.yaml` (NEW FILE)

```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: cre
  labels:
    name: cre
    marketplace.cloud.google.com/deployment: cre
```

**File**: `k8s/gke/01-serviceaccount.yaml` (NEW FILE)

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: cre-ksa
  namespace: cre
  annotations:
    iam.gke.io/gcp-service-account: cre-workload-sa@${PROJECT_ID}.iam.gserviceaccount.com
```

**File**: `k8s/gke/02-rolebinding.yaml` (NEW FILE)

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: cre-rolebinding
  namespace: cre
subjects:
  - kind: ServiceAccount
    name: cre-ksa
    namespace: cre
roleRef:
  kind: Role
  name: cre-role
  apiGroup: rbac.authorization.k8s.io
```

**File**: `k8s/gke/03-network-policy.yaml` (NEW FILE)

```yaml
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
    - namespaceSelector:
        matchLabels:
          name: cre
    ports:
    - protocol: TCP
      port: 4142
    - protocol: TCP
      port: 9100  # Erlang distribution port
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: cre
    ports:
    - protocol: TCP
      port: 4368  # EPMD
    - protocol: TCP
      port: 9100  # Erlang distribution
```

##### 3. Create Marketplace README

**File**: `marketplace/README.md` (NEW FILE)

```markdown
# CRE - Common Runtime Environment for Google Cloud Marketplace

## Overview

CRE (Common Runtime Environment) is a production-grade workflow engine implementing the YAWL (Yet Another Workflow Language) specification with 36 workflow patterns, built on Erlang/OTP for high reliability and fault tolerance.

## Quick Start

### Prerequisites

- GKE cluster version 1.25 or higher
- kubectl configured to access your cluster
- At least 3 nodes available (for 3-node CRE cluster)

### Deployment

1. **Deploy from Marketplace:**
   - Navigate to CRE listing on Google Cloud Marketplace
   - Click "Get Started"
   - Configure deployment parameters (node count, storage, resources)
   - Accept Apache License 2.0
   - Click "Deploy"

2. **Verify Deployment:**
   ```bash
   kubectl get pods -n cre
   kubectl get svc -n cre
   ```

3. **Access CRE:**
   ```bash
   kubectl port-forward -n cre svc/cre 4142:4142
   curl http://localhost:4142/health
   ```

## Configuration

### Scaling

CRE supports horizontal pod autoscaling:

```yaml
autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 80
```

### Persistence

CRE uses persistent volumes for Mnesia database:

```yaml
persistence:
  enabled: true
  size: 10Gi
  storageClass: premium-rwo
```

### Resource Limits

Default resource configuration:

```yaml
resources:
  requests:
    cpu: 500m
    memory: 512Mi
  limits:
    cpu: 2000m
    memory: 2Gi
```

## Monitoring

CRE exposes metrics at `/status.json`:

```bash
kubectl port-forward -n cre svc/cre 4142:4142
curl http://localhost:4142/status.json
```

Health checks:
- Liveness: `/health`
- Readiness: `/ready`
- Startup: `/startup`

## Architecture

CRE runs as a StatefulSet with:
- **3+ nodes** for Mnesia clustering
- **Headless service** for cluster communication
- **Pod Disruption Budget** for high availability
- **Horizontal Pod Autoscaler** (optional)

## Support

- Documentation: https://github.com/joergen7/cre/blob/main/docs/DEPLOYMENT.md
- Issues: https://github.com/joergen7/cre/issues
- License: Apache License 2.0

## License

CRE is licensed under the Apache License 2.0. By deploying CRE from Google Cloud Marketplace, you agree to the terms of this license.

See [LICENSE](https://github.com/joergen7/cre/blob/main/LICENSE) for details.
```

##### 4. Create BYOL License Documentation

**File**: `marketplace/LICENSE.txt` (NEW FILE)

```text
CRE (Common Runtime Environment)
Copyright 2025 CRE Project

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---

MARKETPLACE DEPLOYMENT TERMS

By deploying CRE from Google Cloud Marketplace, you acknowledge that:
1. CRE is provided under the Apache License 2.0
2. No support SLA is included (community support only)
3. You are responsible for managing your own CRE deployment
4. CRE is provided "AS IS" without warranties of any kind

For enterprise support options, please contact the CRE team.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Marketplace deployment spec validates: `kubectl apply --dry-run=client -f marketplace/deployer.yaml`
- [ ] GKE manifests apply successfully: `kubectl apply --dry-run=client -f k8s/gke/`
- [ ] Helm chart packages with application.yaml: `helm package k8s/charts/cre --app-version 0.3.0`

##### Manual Verification:
- [ ] Marketplace README is clear and complete
- [ ] License terms are acceptable for BYOL model
- [ ] All manifests are properly formatted

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 4.

---

### Phase 4: Testing & Validation

#### Overview
End-to-end testing of Marketplace deployment flow and preparation for technical assessment.

#### Changes Required:

##### 1. Create Deployment Test Script

**File**: `scripts/marketplace/test-deployment.sh` (NEW FILE)
**Changes**: Automated E2E deployment test

```bash
#!/bin/bash
set -e

echo "=========================================="
echo "CRE Marketplace Deployment Test"
echo "=========================================="

PROJECT_ID="${1:?Usage: $0 PROJECT_ID [CLUSTER_NAME] [ZONE]}"
CLUSTER_NAME="${2:-cre-marketplace-test}"
ZONE="${3:-us-central1-a}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# ============================================
# Step 1: Create test GKE cluster
# ============================================
log "Creating test GKE cluster..."

gcloud container clusters create ${CLUSTER_NAME} \
  --project=${PROJECT_ID} \
  --zone=${ZONE} \
  --num-nodes=3 \
  --machine-type=e2-medium \
  --image-type=cOS \
  --disk-type=pd-standard \
  --disk-size=100GB \
  --enable-ip-alias \
  --enable-private-nodes \
  --master-ipv4-cidr=172.16.0.0/28 \
  --enable-shielded-nodes \
  --shielded-secure-boot \
  --shielded-vtpm \
  --shielded-integrity-monitoring \
  --workload-pool=${PROJECT_ID}.svc.id.goog \
  --security-posture=enterprise \
  || error "Failed to create GKE cluster"

log "GKE cluster created successfully"

# ============================================
# Step 2: Get cluster credentials
# ============================================
log "Getting cluster credentials..."

gcloud container clusters get-credentials ${CLUSTER_NAME} \
  --project=${PROJECT_ID} \
  --zone=${ZONE} \
  || error "Failed to get credentials"

log "Credentials configured"

# ============================================
# Step 3: Create namespace
# ============================================
log "Creating CRE namespace..."

kubectl create namespace cre || warn "Namespace already exists"

log "Namespace created"

# ============================================
# Step 4: Deploy CRE via Helm
# ============================================
log "Deploying CRE via Helm..."

helm install cre ./k8s/charts/cre \
  --namespace cre \
  --values ./k8s/charts/cre/values-gke-marketplace.yaml \
  --set image.repository=us-central1-docker.pkg.dev/${PROJECT_ID}/cre/cre \
  --set image.tag=0.3.0 \
  --timeout 10m \
  || error "Helm installation failed"

log "CRE deployed successfully"

# ============================================
# Step 5: Wait for pods to be ready
# ============================================
log "Waiting for CRE pods to be ready..."

kubectl wait --for=condition=ready pod -l app=cre -n cre --timeout=300s \
  || error "Pods did not become ready in time"

log "All CRE pods are ready"

# ============================================
# Step 6: Verify health endpoints
# ============================================
log "Verifying health endpoints..."

kubectl port-forward -n cre svc/cre 4142:4142 &
PF_PID=$!
sleep 5

# Test /health
HEALTH=$(curl -s http://localhost:4142/health)
echo "Health check response: ${HEALTH}"
echo "${HEALTH}" | grep -q '"status":"healthy"' || error "Health check failed"

# Test /ready
READY=$(curl -s http://localhost:4142/ready)
echo "Readiness check response: ${READY}"
echo "${READY}" | grep -q '"status":"healthy"' || error "Readiness check failed"

# Test /startup
STARTUP=$(curl -s http://localhost:4142/startup)
echo "Startup check response: ${STARTUP}"
echo "${STARTUP}" | grep -q '"status":"healthy"' || error "Startup check failed"

# Test /status.json
STATUS=$(curl -s http://localhost:4142/status.json)
echo "Status response: ${STATUS}"
echo "${STATUS}" | grep -q '"status"' || error "Status check failed"

kill ${PF_PID}

log "All health endpoints verified"

# ============================================
# Step 7: Verify StatefulSet
# ============================================
log "Verifying StatefulSet..."

REPLICAS=$(kubectl get statefulset cre -n cre -o jsonpath='{.spec.replicas}')
READY_REPLICAS=$(kubectl get statefulset cre -n cre -o jsonpath='{.status.readyReplicas}')

echo "Expected replicas: ${REPLICAS}, Ready replicas: ${READY_REPLICAS}"

[ "${REPLICAS}" -eq "${READY_REPLICAS}" ] || error "Not all replicas are ready"

log "StatefulSet is healthy"

# ============================================
# Step 8: Verify persistent volumes
# ============================================
log "Verifying persistent volumes..."

PVC_COUNT=$(kubectl get pvc -n cre -l app=cre --no-headers | wc -l)
echo "PVCs created: ${PVC_COUNT}"

[ "${PVC_COUNT}" -eq "${REPLICAS}" ] || error "PVC count does not match replica count"

log "Persistent volumes verified"

# ============================================
# Step 9: Test pod disruption
# ============================================
log "Testing pod disruption budget..."

# Try to delete a pod (should be replaced immediately)
POD_NAME=$(kubectl get pods -n cre -l app=cre -o jsonpath='{.items[0].metadata.name}')
kubectl delete pod ${POD_NAME} -n cre

sleep 10

# Verify pod was replaced
NEW_POD_COUNT=$(kubectl get pods -n cre -l app=cre --no-headers | wc -l)
echo "Pod count after disruption: ${NEW_POD_COUNT}"

[ "${NEW_POD_COUNT}" -eq "${REPLICAS}" ] || error "Pod was not replaced"

log "Pod disruption budget working correctly"

# ============================================
# Step 10: Cleanup
# ============================================
log "Cleaning up..."

helm uninstall cre -n cre || warn "Helm uninstall failed"
kubectl delete namespace cre || warn "Namespace deletion failed"
gcloud container clusters delete ${CLUSTER_NAME} --project=${PROJECT_ID} --zone=${ZONE} --quiet || warn "Cluster deletion failed"

log "=========================================="
echo "ALL TESTS PASSED!"
echo "=========================================="
```

```bash
chmod +x scripts/marketplace/test-deployment.sh
```

##### 2. Create Security Validation Script

**File**: `scripts/marketplace/security-scan.sh` (NEW FILE)

```bash
#!/bin/bash
set -e

IMAGE="${1:?Usage: $0 IMAGE_URI}"

echo "Scanning ${IMAGE} for security vulnerabilities..."

# Run Trivy scan
trivy image --severity CRITICAL,HIGH --format json ${IMAGE} > scan-results.json

# Check for CRITICAL vulnerabilities
CRITICAL_COUNT=$(jq '[.Results[].Vulnerabilities[]? | select(.Severity == "CRITICAL")] | length' scan-results.json)

if [ "${CRITICAL_COUNT}" -gt 0 ]; then
  echo "FOUND ${CRITICAL_COUNT} CRITICAL VULNERABILITIES"
  jq '.Results[].Vulnerabilities[] | select(.Severity == "CRITICAL")' scan-results.json
  exit 1
fi

# Check for HIGH vulnerabilities
HIGH_COUNT=$(jq '[.Results[].Vulnerabilities[]? | select(.Severity == "HIGH")] | length' scan-results.json)

echo "CRITICAL: ${CRITICAL_COUNT}"
echo "HIGH: ${HIGH_COUNT}"

if [ "${HIGH_COUNT}" -gt 10 ]; then
  echo "WARNING: More than 10 HIGH vulnerabilities found"
  exit 1
fi

echo "Security scan passed"
```

##### 3. Create Marketplace Submission Checklist

**File**: `marketplace/SUBMISSION_CHECKLIST.md` (NEW FILE)

```markdown
# CRE Marketplace Submission Checklist

## Technical Assessment

### Container Images
- [ ] Multi-arch images (linux/amd64, linux/arm64) in Artifact Registry
- [ ] Images use immutable version tags (vX.Y.Z)
- [ ] Images signed with cosign
- [ ] SBOM generated in SPDX format
- [ ] Trivy scan shows no CRITICAL vulnerabilities
- [ ] Non-root user execution
- [ ] Read-only root filesystem support
- [ ] All capabilities dropped

### Kubernetes Deployment
- [ ] GKE Application schema (application.yaml) present
- [ ] Helm chart passes lint: `helm lint k8s/charts/cre`
- [ ] No secrets in values.yaml
- [ ] Health checks functional (/health, /ready, /startup)
- [ ] Pod Security Standards compliance
- [ ] Network policies defined
- [ ] Resource limits configured
- [ ] Pod Disruption Budget configured

### Security & Compliance
- [ ] No hardcoded secrets
- [ ] External Secrets Operator integration documented
- [ ] Workload Identity Federation (no service account keys)
- [ ] Private GKE cluster configuration
- [ ] Shielded nodes enabled
- [ ] Binary Authorization policy documented
- [ ] Audit logging enabled

### Documentation
- [ ] Marketplace README complete
- [ ] License terms documented (BYOL)
- [ ] Architecture diagram provided
- [ ] Support process documented
- [ ] Known limitations documented
- [ ] Quick start guide verified

### Testing
- [ ] Fresh GKE project deployment tested
- [ ] Health checks verified
- [ ] Scaling tested (manual and HPA)
- [ ] Persistence verified
- [ ] Backup/restore tested
- [ ] Pod disruption tested
- [ ] Network policies verified

## Marketplace Listing

### Metadata
- [ ] Title: "CRE - Common Runtime Environment"
- [ ] Category: Development Tools
- [ ] Tags: workflow, automation, integration, yawl, erlang
- [ ] Logo uploaded (PNG, 128x128px)
- [ ] Short description (80 chars max)
- [ ] Long description (2000 chars max)
- [ ] Documentation URL
- [ ] Support URL

### Pricing
- [ ] BYOL model selected
- [ ] License agreement link provided
- [ ] No usage metering required

### Deployment UI
- [ ] application.yaml parameters tested
- [ ] Default values appropriate
- [ ] Constraints working correctly
- [ ] Output variables displaying correctly

## Post-Submission

### Monitoring
- [ ] Cloud Monitoring dashboard created
- [ ] Alert policies configured
- [ ] Log queries documented

### Support
- [ ] Issue triage process defined
- [ ] SLA documented (if applicable)
- [ ] Escalation path defined

### Maintenance
- [ ] Update process documented
- [ ] Rollback procedure tested
- [ ] Versioning strategy defined
```

#### Success Criteria:

##### Automated Verification:
- [ ] Deployment test passes: `./scripts/marketplace/test-deployment.sh $PROJECT_ID`
- [ ] Security scan passes: `./scripts/marketplace/security-scan.sh us-central1-docker.pkg.dev/$PROJECT_ID/cre/cre:0.3.0`
- [ ] All checklist items completed

##### Manual Verification:
- [ ] Fresh GCP project deployment succeeds
- [ ] Marketplace UI renders correctly
- [ ] Documentation is accurate and complete
- [ ] Support process is clear

**Note**: Complete all automated verification, then pause for manual confirmation before submitting to Marketplace.

---

## Testing Strategy

### Unit Tests:
- **Health endpoints**: Test `/health`, `/ready`, `/startup` return valid JSON with `status` field
- **Status endpoint**: Test `/status.json` returns CRE master status
- **Route configuration**: Verify Cowboy routing includes health endpoints

### Integration Tests:
- **Helm chart rendering**: `helm template cre k8s/charts/cre --debug`
- **Kubernetes deployment**: Deploy to GKE and verify pod readiness
- **Health probe execution**: Verify kubelet probes succeed
- **StatefulSet scaling**: Test scaling up and down
- **Persistent volume mounting**: Verify PVCs are mounted correctly

### Manual Testing Steps:
1. **Build multi-arch image**:
   ```bash
   docker buildx build --platform linux/amd64,linux/arm64 -t cre:test .
   ```

2. **Run container locally**:
   ```bash
   docker run --rm -p 4142:4142 cre:test
   ```

3. **Test health endpoints**:
   ```bash
   curl http://localhost:4142/health
   curl http://localhost:4142/ready
   curl http://localhost:4142/startup
   curl http://localhost:4142/status.json
   ```

4. **Deploy to GKE**:
   ```bash
   gcloud container clusters get-credentials cre-cluster
   helm install cre ./k8s/charts/cre --namespace cre --create-namespace
   ```

5. **Verify deployment**:
   ```bash
   kubectl get pods -n cre
   kubectl port-forward -n cre svc/cre 4142:4142
   curl http://localhost:4142/health
   ```

6. **Test autoscaling**:
   ```bash
   kubectl patch statefulset cre -n cre --type=json -p='[{"op": "add", "path": "/spec/replicas", "value": 5}]'
   kubectl get pods -n cre -w
   ```

7. **Test pod disruption**:
   ```bash
   kubectl delete pod -l app=cre -n cre
   kubectl get pods -n cre -w
   ```

## Migration Notes
No migration required - this is new Marketplace packaging for existing CRE codebase.

## References
- Research: `/Users/sac/cre/.wreckit/items/002-package-cre-for-google-cloud-marketplace-distribut/research.md`
- Helm chart: `/Users/sac/cre/k8s/charts/cre/`
- Health endpoints: `/Users/sac/cre/src/api/cre_health.erl` (lines 113-207)
- Status handler: `/Users/sac/cre/src/http/cre_status_handler.erl`
- Cowboy routing: `/Users/sac/cre/src/app/cre.erl` (line 339)
- Cloud Build workflow: `/Users/sac/cre/.github/workflows/gcp-cloud-build.yml`
- Dockerfile: `/Users/sac/cre/Dockerfile` (lines 1-314)
- Helm values: `/Users/sac/cre/k8s/charts/cre/values.yaml`
