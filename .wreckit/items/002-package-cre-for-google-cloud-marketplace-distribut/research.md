# Research: Package CRE for Google Cloud Marketplace distribution

**Date**: 2025-01-18
**Item**: 002-package-cre-for-google-cloud-marketplace-distribut

## Research Question
CRE needs to be deployed on GCP via Google Cloud Marketplace with enterprise-grade operational readiness, requiring proper packaging, deployment automation, and GCP service integrations.

**Motivation:** Enables CRE distribution to GCP customers via familiar Marketplace interface with automated deployment, billing integration, and GCP-native observability.

**Success criteria:**
- One-click deploy works in a fresh GCP project
- Passes Marketplace technical and security review
- Helm chart supports parameterized deployment (node counts, autoscaling, persistence, TLS, auth)
- Multi-arch container images in Artifact Registry
- Approved by Google Marketplace review

**Technical constraints:**
- GKE Application model (primary)
- Compute Engine VM support optional v2
- Multi-arch (amd64 mandatory)
- Immutable version tags
- No secrets in Helm values.yaml
- ConfigMaps for runtime settings with hot-reload support

**In scope:**
- Helm chart development with application.yaml schema
- Container image packaging and security hardening
- Marketplace deployment spec and UI
- One-click install from Marketplace UI
**Out of scope:**
- Multi-cloud marketplace support (v1)
- VM-based deployment (v2)
- Managed SaaS offering

**Signals:** priority: high, urgency: Foundational blocker for Marketplace launch

## Summary

CRE has **significant existing infrastructure** for GCP Marketplace deployment but lacks the **critical Marketplace-specific packaging** required for actual submission. The codebase contains production-ready Docker images, Helm charts, Terraform modules, and GCP integrations, but is missing:

1. **GKE Application schema** (`application.yaml`) - REQUIRED for Marketplace
2. **Marketplace deployment spec** - Billing integration, UI parameters
3. **Helm chart hardening** - Remove secrets from values.yaml
4. **Multi-arch image publishing** - Currently builds multi-arch but not optimized for Marketplace requirements
5. **One-click deployment automation** - Marketplace-specific workflows

The foundation is strong: multi-stage Dockerfiles (lines 1-314 in `Dockerfile`), comprehensive Helm chart (values.yaml 1-302), GCP-optimized Terraform modules, and health check endpoints (cre_health.erl 63-537). However, these need to be packaged according to Google Cloud Marketplace's specific requirements.

The path forward involves: (1) Creating `application.yaml` schema for Marketplace UI, (2) Refactoring Helm chart to use ConfigMaps for all runtime settings, (3) Adding Marketplace billing/licensing integration, (4) Implementing proper secret management via External Secrets Operator, and (5) Creating Marketplace deployment manifests.

## Current State Analysis

### Existing Implementation

#### Container Images & Build Pipeline
**Status:** ✅ **STRONG** - Multi-arch production Dockerfile exists

- **Multi-arch support:** `Dockerfile` (lines 1-314) supports linux/amd64 and linux/arm64
  - Rust NIF builder stage (lines 24-75) compiles platform-specific native extensions
  - Erlang builder stage (lines 78-181) builds OTP 28 release
  - Runtime stage (lines 185-290) creates minimal production image
  - SBOM generation stage (lines 294-314) generates SPDX artifacts

- **Security hardening:** Non-root user (line 236), dropped capabilities (lines 39-41), read-only root filesystem support
- **Health checks:** Built-in health endpoint (line 263) with 30s interval, 10s timeout
- **OCI labels:** Comprehensive metadata (lines 196-208) for vulnerability scanning
- **Cloud Build pipeline:** `cloudbuild.yaml` (only 3 lines, incomplete - needs full implementation)
- **CI/CD workflow:** `.github/workflows/gcp-cloud-build.yml` (1-453) has full build/deploy pipeline but references missing `cloudbuild.yaml`

**Gaps:**
- `cloudbuild.yaml` is essentially empty (only contains comments)
- No Artifact Registry multi-arch push configuration
- Missing immutable version tag enforcement
- No Marketplace-specific image scanning requirements

#### Kubernetes Deployment Artifacts
**Status:** ✅ **STRONG** - Comprehensive Helm chart exists

- **Helm chart:** `k8s/charts/cre/` has full production-ready chart
  - `Chart.yaml` (1-30) - Version 0.3.0, proper metadata, Artifact Hub annotations
  - `values.yaml` (1-302) - Extensive configuration parameters
  - `values-gke.yaml` (1-155) - GKE-specific overrides
  - `templates/statefulset.yaml` (1-161) - CRE StatefulSet with clustering support
  - `templates/_helpers.tpl` (1-64) - Template helper functions

- **Configuration structure:**
  - 3-node cluster default (values.yaml:16)
  - Resource limits: 500m-2000m CPU, 512Mi-2Gi memory (values.yaml:137-143)
  - Persistence enabled with 10Gi default (values.yaml:185-197)
  - HPA configuration (values.yaml:146-182)
  - Pod Disruption Budget (values.yaml:241-243)

- **GKE-specific manifests:** `k8s/gcp/` directory has 20+ specialized manifests
  - `deployment.yaml`, `hpa.yaml`, `pdb.yaml`, `ingress.yaml`
  - Workload Identity integration (`serviceaccount.yaml`)
  - Network policies (`network-policy.yaml`)
  - Spot VM tolerations (`tolerations.yaml`)

**Gaps:**
- Secrets in values.yaml violate Marketplace constraints (line 295: `existingSecret`)
- No ConfigMap-based hot-reload configuration
- Missing `application.yaml` for Marketplace UI schema
- No TLS certificate management integration
- Missing GKE Application model references

#### GCP Infrastructure (Terraform)
**Status:** ✅ **STRONG** - Production-ready Terraform modules

- **Root module:** `terraform/gcp/main.tf` (1-201) orchestrates all infrastructure
- **VPC module:** Private network with secondary ranges for pods/services (lines 7-39)
- **GKE cluster module:** `modules/gke_cluster/main.tf` (1-279) creates regional cluster
  - Private cluster configuration (lines 18-22)
  - Shielded nodes enabled (line 64)
  - Workload Identity configured (lines 70-72)
  - Security Posture in ENTERPRISE mode (lines 101-104)
  - Binary authorization enforcement (lines 107-109)

- **Security module:** `modules/security/` with IAM and Workload Identity (main.tf:177-200)
  - CRITICAL comment: "no default service account usage" for Marketplace compliance (line 176)
  - Kubernetes service account: "cre-ksa" (line 186)
  - GitHub Actions Workload Identity pool (lines 189-190)

- **Storage module:** `modules/storage/` for PVCs and snapshots (main.tf:83-107)
- **Load balancer module:** `modules/loadbalancer/` with Cloud Armor (main.tf:112-155)
- **Backup module:** `modules/backup/` with cross-region replication (main.tf:160-170)

**Readiness document:** `docs/gcp/GCP_MARKETPLACE_READINESS.md` (1-386) claims status is "READY FOR MARKETPLACE SUBMISSION" but focuses on infrastructure readiness, not Marketplace packaging compliance.

**Gaps:**
- No Marketplace billing integration
- No usage reporting configuration
- Missing Marketplace partner technical assessment artifacts
- No Marketplace listing metadata

#### Health & Monitoring
**Status:** ✅ **EXCELLENT** - Full GCP-ready health checks

- **Health endpoints:** `src/api/cre_health.erl` (1-537)
  - `/health` - Liveness probe (lines 113-165)
  - `/ready` - Readiness probe (lines 167-186)
  - `/startup` - Startup probe (lines 188-207)
  - JSON response format with subsystem status (lines 84-96)
  - Mnesia, EPMD, worker pool checks (lines 327-498)

- **GCP monitoring integration:**
  - Cloud Logging backend mentioned in readiness doc
  - Prometheus exporter (`src/telemetry/prometheus_exporter.erl`)
  - GKE dashboards in `monitoring/gcp/` directory

- **Probes configured in Helm:**
  - Liveness: /status.json every 15s, 3 failure threshold (values.yaml:204-211)
  - Readiness: /status.json every 10s, 3 failure threshold (values.yaml:213-220)
  - Startup: /status.json every 5s, 30 failure threshold (values.yaml:222-229)

**Gaps:**
- Health endpoint path mismatch: cre_health.erl uses `/health`, Helm values use `/status.json`
- No `/status.json` endpoint found in codebase - only `/health`, `/ready`, `/startup`
- This will cause health checks to FAIL in deployment

### Key Files

#### Core Infrastructure
- `Dockerfile` (1-314) - Multi-arch production build with Rust NIFs, OTP 28, SBOM stage
- `docker/docker-entrypoint.sh` (1-199) - Mnesia clustering, graceful shutdown logic
- `rebar.config` (1-124) - OTP 25-28 compatibility, prod profile configuration

#### Kubernetes Artifacts
- `k8s/charts/cre/Chart.yaml` (1-30) - Helm chart metadata v0.3.0
- `k8s/charts/cre/values.yaml` (1-302) - Complete configuration parameters
- `k8s/charts/cre/values-gke.yaml` (1-155) - GKE-specific overrides
- `k8s/charts/cre/templates/statefulset.yaml` (1-161) - CRE StatefulSet with clustering
- `k8s/charts/cre/templates/_helpers.tpl` (1-64) - Template helper functions

#### GCP Infrastructure
- `terraform/gcp/main.tf` (1-201) - Root module orchestrating VPC, GKE, storage, load balancers
- `terraform/gcp/modules/gke_cluster/main.tf` (1-279) - Regional private GKE cluster
- `terraform/gcp/modules/security/` - IAM and Workload Identity configuration

#### CI/CD
- `.github/workflows/gcp-cloud-build.yml` (1-453) - Full CI/CD with Cloud Build trigger, security scanning, GKE deployment
- `cloudbuild.yaml` (1-3) - INCOMPLETE (only comments, no actual build steps)

#### Health & Monitoring
- `src/api/cre_health.erl` (1-537) - GCP-ready health check endpoints
- `monitoring/prometheus/prometheus.yml` (1-119) - Prometheus scrape configuration
- `monitoring/gcp/` - GKE-specific dashboards and alert policies

#### Documentation
- `docs/gcp/GCP_MARKETPLACE_READINESS.md` (1-386) - Infrastructure readiness assessment
- `k8s/README.md` (1-181) - Kubernetes deployment guide
- `terraform/gcp/README.md` (1-130) - Terraform deployment guide

## Technical Considerations

### Dependencies

#### External Dependencies (GCP Marketplace Requirements)
- **Google Cloud Deploy** - Required for Marketplace deployment pipelines
- **Artifact Registry** - Multi-arch image storage (currently referenced but not configured)
- **Cloud KMS** - For secret encryption (if using Secret Manager)
- **Binary Authorization** - Already configured in Terraform (gke_cluster/main.tf:107-109)
- **Cloud Armor** - Already configured (main.tf:140)

#### Internal Modules to Integrate
- `src/api/cre_health.erl` - Health check endpoints (MUST fix path mismatch)
- `src/telemetry/cloud_logging_backend.erl` - Mentioned in readiness doc, need to verify implementation
- `src/db/spanner_adapter.erl` - Cloud Spanner integration for database migration
- `src/db/dual_write_adapter.erl` - Mnesia to Spanner migration support

### Patterns to Follow

#### Existing Helm Chart Patterns
- **Helper templates:** `k8s/charts/cre/templates/_helpers.tpl` defines reusable functions
- **Value overrides:** `values-gke.yaml` extends base `values.yaml` for GKE-specific settings
- **StatefulSet over Deployment:** Uses StatefulSet for stable network identities (required for Erlang clustering)
- **Headless service:** Enabled for cluster communication (values.yaml:129-134)

#### Existing Infrastructure Patterns
- **Module composition:** Terraform uses composable modules (vpc, gke_cluster, storage, loadbalancer, security)
- **Workload Identity:** No service account keys (main.tf:198 comment)
- **Private cluster:** Private endpoint with authorized networks (gke_cluster/main.tf:18-22)
- **Pod Security Standards:** Enforced at GKE level (gke_cluster/main.tf:99-115)

#### Security Patterns
- **Non-root containers:** Dockerfile runs as user 1000 (line 236)
- **Capability dropping:** All capabilities dropped (Dockerfile:39-41)
- **Shielded nodes:** Enabled in GKE node pools (gke_cluster/main.tf:163-166, 239-242)
- **GKE_METADATA:** Prevents pod access to node IAM credentials (gke_cluster/main.tf:185-187, 260-262)

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Health endpoint path mismatch** - Helm uses `/status.json`, code implements `/health` | HIGH - Deployment will fail health checks | Update Helm values.yaml to use `/health`, `/ready`, `/startup` endpoints OR add `/status.json` alias in cre_health.erl |
| **Secrets in Helm values** - `existingSecret` violates Marketplace constraints | HIGH - Security review will fail | Refactor to use External Secrets Operator with Secret Manager references |
| **Missing application.yaml** - GKE Application schema required for Marketplace | HIGH - Cannot submit to Marketplace | Create application.yaml with proper schema for Marketplace UI |
| **Incomplete cloudbuild.yaml** - Only 3 lines of comments | MEDIUM - Cannot build images via Cloud Build | Implement full Cloud Build configuration with Kaniko caching, Trivy scanning |
| **No billing integration** - Marketplace requires usage metering | HIGH - Cannot launch on Marketplace | Implement Marketplace billing API integration or use BYOL model |
| **Multi-arch image tags** - No immutable version enforcement | MEDIUM - Security review may fail | Implement strict tag policy: `vX.Y.Z` only, no `latest` in Marketplace |
| **ConfigMap hot-reload** - Not implemented | MEDIUM - Runtime configuration changes require restart | Implement CRE configuration watcher for ConfigMap changes |
| **Single-region deployment** - No multi-region strategy | LOW - Initial launch limitation | Document single-region limitation for v1, plan multi-region for v2 |

## Recommended Approach

### Phase 1: Foundation (Weeks 1-2)
**Goal:** Fix critical blockers and establish Marketplace packaging structure

1. **Fix health endpoint mismatch** (CRITICAL)
   - Option A: Update `k8s/charts/cre/values.yaml` lines 206, 214, 224 to use `/health`, `/ready`, `/startup`
   - Option B: Add `/status.json` route in `src/api/cre_health.erl` as alias to `/health`
   - Test health probes with `kubectl`

2. **Create application.yaml schema** (REQUIRED for Marketplace)
   - Create `k8s/charts/cre/application.yaml` following GKE Application model spec
   - Define input parameters: node counts, autoscaling thresholds, storage size, TLS toggle
   - Link to Helm values.yaml properties

3. **Refactor secret management** (Marketplace constraint)
   - Remove `existingSecret` pattern from values.yaml
   - Implement External Secrets Operator integration
   - Add Secret Manager references in Terraform security module

### Phase 2: Build & Deploy Pipeline (Weeks 3-4)
**Goal:** Implement Marketplace-compliant build and deployment automation

1. **Implement complete cloudbuild.yaml**
   - Multi-arch build with docker buildx (amd64, arm64)
   - Kaniko caching for faster builds
   - Trivy security scanning (CRITICAL, HIGH vulnerabilities)
   - Syft SBOM generation
   - Artifact Registry push with immutable tags
   - Sign images with cosign

2. **Add Marketplace billing integration**
   - Choose model: BYOL (simple) or usage-based (complex)
   - If BYOL: Add license key parameter to application.yaml
   - If usage-based: Implement metering agent with Marketplace Reporting API
   - Document billing model in Marketplace listing

3. **Update CI/CD workflow**
   - `.github/workflows/gcp-cloud-build.yml` already comprehensive but references incomplete cloudbuild.yaml
   - Add Marketplace-specific validation steps
   - Add integration tests for Marketplace deployment flow

### Phase 3: Marketplace Submission (Weeks 5-6)
**Goal:** Prepare and submit Marketplace listing

1. **Create Marketplace deployment spec**
   - Package Helm chart with application.yaml
   - Create deployment manifest for Marketplace
   - Add UI screenshots and diagrams
   - Write Marketplace description (highlight: 36 YAWL patterns, Erlang reliability, 96% test pass rate)

2. **Security hardening validation**
   - Run Trivy scan, address all CRITICAL/HIGH vulnerabilities
   - Verify SBOM completeness
   - Validate Pod Security Standards compliance
   - Test Binary Authorization policy enforcement

3. **Documentation package**
   - Quick start guide for Marketplace customers
   - Architecture diagram (VPC, GKE, load balancer)
   - Upgrade and rollback procedures
   - Cost estimation guide
   - SLA and support docs (already exist in docs/gcp/marketplace/)

### Phase 4: Testing & Validation (Weeks 7-8)
**Goal:** Validate one-click deployment in fresh GCP project

1. **End-to-end deployment test**
   - Fresh GCP project with no existing resources
   - Deploy via Marketplace UI (using test listing)
   - Validate all health checks pass
   - Test autoscaling under load
   - Test backup/restore procedures

2. **Marketplace technical assessment**
   - Submit to Google Partner Technical Connect
   - Address any review feedback
   - Complete security questionnaire
   - Provide SBOM and scan results

3. **Production readiness**
   - Run load tests (1000+ concurrent workflows)
   - Test disaster recovery (cluster failure, regional outage)
   - Validate monitoring dashboards and alerts
   - Document SRE runbooks

## Open Questions

1. **Billing Model:** Should CRE launch with BYOL or usage-based pricing on Marketplace?
   - BYOL: Simpler, faster to market, no metering integration needed
   - Usage-based: More complex, requires metering agent, but aligns with Marketplace expectations
   - **Recommendation:** Start with BYOL for v1, add usage-based for v2 based on customer feedback

2. **Database Strategy:** Marketplace submission requires persistence strategy
   - Current: Mnesia (in-memory) with optional Spanner migration
   - Marketplace: May require managed database (Cloud SQL/Cloud Spanner)
   - **Question:** Should Marketplace version default to Spanner for production deployments?
   - **Recommendation:** Default to Mnesia for simplicity, offer Spanner as advanced configuration option

3. **Multi-Region Support:** Is single-region deployment acceptable for v1?
   - Current Terraform modules are regional
   - Multi-region requires multi-cluster setup (complex)
   - **Recommendation:** Document single-region limitation for v1, plan multi-region for v2

4. **TLS Certificate Management:**
   - Current values.yaml references TLS certs but doesn't provision them
   - Marketplace requires automated TLS (cert-manager or Google Managed Certificates)
   - **Question:** Should we integrate cert-manager or use Google Cloud Managed Certificates?
   - **Recommendation:** Use Google Managed Certificates for simplicity (GKE-native, no operator needed)

5. **Support Level:** Marketplace requires documented support SLA
   - Docs exist (docs/gcp/marketplace/SUPPORT.md, SLA.md)
   - **Question:** What is the actual support commitment? Community only, or paid support?
   - **Recommendation:** Offer community support for v1, add paid enterprise support tier for v2

6. **Health Endpoint Standardization:**
   - Code has `/health`, `/ready`, `/startup` (cre_health.erl)
   - Helm values reference `/status.json` (values.yaml)
   - Which standard should we follow?
   - **Recommendation:** Use Google's standard: `/healthz` (kubelet), `/readiness` (custom). Keep existing endpoints for backward compatibility, add aliases

7. **ConfigMap Hot-Reload:**
   - Marketplace constraint requires ConfigMaps for runtime settings
   - CRE needs configuration watcher to reload without restart
   - **Question:** Is hot-reload implemented? If not, is it required for v1?
   - **Recommendation:** Document that configuration changes require pod restart for v1, add hot-reload in v2

8. **Marketplace Category:**
   - CRE is a workflow engine - which Marketplace category?
   - Options: Development Tools, Integration, Big Data, AI/ML
   - **Recommendation:** Submit under "Development Tools" with tags for "Integration", "Workflow Automation"

---

## Appendix: File Inventory

### Files to Create
- `k8s/charts/cre/application.yaml` - GKE Application schema for Marketplace UI
- `cloudbuild.yaml` - Complete Cloud Build configuration (replace 3-line stub)
- `k8s/charts/cre/marketplace/README.md` - Marketplace-specific deployment guide
- `marketplace-deploy.yaml` - Marketplace deployment manifest
- `scripts/marketplace/test-deployment.sh` - E2E deployment test script

### Files to Modify
- `k8s/charts/cre/values.yaml` - Fix health endpoint paths, remove secret references
- `k8s/charts/cre/templates/statefulset.yaml` - Add ConfigMap volume mounts
- `.github/workflows/gcp-cloud-build.yml` - Update to reference complete cloudbuild.yaml
- `src/api/cre_health.erl` - Add `/status.json` alias OR update Helm to use `/health`
- `terraform/gcp/main.tf` - Add Marketplace-specific IAM roles

### Files to Reference (Existing, No Changes)
- `Dockerfile` - Already Marketplace-ready (multi-arch, security-hardened)
- `k8s/charts/cre/Chart.yaml` - Already has proper metadata
- `terraform/gcp/modules/gke_cluster/main.tf` - Already Marketplace-compliant
- `docs/gcp/GCP_MARKETPLACE_READINESS.md` - Infrastructure readiness assessment
- `src/api/cre_health.erl` - Health check implementation (just need to fix path references)
