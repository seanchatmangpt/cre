# Harden CRE security and compliance for enterprise GCP deployment Implementation Plan

## Implementation Plan Title
Enterprise Security Hardening for GCP Marketplace Deployment

## Overview
Implement security controls required for enterprise GCP Marketplace approval, focusing on Pod Security Standards compliance, container hardening, audit log centralization, and optional CMEK support. CRE has a strong security foundation with Workload Identity, network policies, and shielded nodes already implemented. This plan addresses the remaining gaps: read-only root filesystem, init container hardening, audit log retention, and image signing.

## Current State

**Strong Security Foundation (Already Implemented):**
- ✅ Workload Identity Federation for GCP service account access (`terraform/gcp/modules/security/iam.tf:140-144`)
- ✅ Default-deny network policies with explicit allow rules (`k8s/gcp/network-policy.yaml:6-131`)
- ✅ Private GKE cluster with shielded nodes (`terraform/gcp/modules/gke_cluster/main.tf:18-22, 163-166`)
- ✅ GKE Security Posture in ENTERPRISE mode (`terraform/gcp/modules/gke_cluster/main.tf:101-104`)
- ✅ Binary Authorization enforcement enabled (`terraform/gcp/modules/gke_cluster/main.tf:107-109`)
- ✅ Non-root user in container (UID 1000) (`Dockerfile:235-236`)
- ✅ Pod-level security context with seccomp (`k8s/gcp/deployment.yaml:88-94`)
- ✅ Secret Manager integration with IAM controls (`terraform/gcp/modules/security/secrets.tf:16-51`)
- ✅ Least-privilege service account design (`terraform/gcp/modules/security/iam.tf:15-145`)

**Critical Security Gaps (To Be Addressed):**
- ❌ Read-only root filesystem disabled (`k8s/gcp/deployment.yaml:248: readOnlyRootFilesystem: false`)
- ❌ Init container runs as root for chown operations (`k8s/gcp/deployment.yaml:115: runAsUser: 0`)
- ❌ Audit logs stored locally in disk_log, no Cloud Logging export (`src/wf/wf_audit_log.erl:145-196`)
- ❌ No image signature verification (cosign not implemented)
- ❌ RBAC overly broad - pods can list/watch all pods (`k8s/gcp/serviceaccount.yaml:48`)
- ❌ CMEK support incomplete - only for backups, not primary storage
- ❌ No log retention policy defined for compliance (SOX requires 400 days)
- ⚠️ Container uses Alpine base, not distroless (acceptable with scanning)

## Desired End State

CRE deployed on GKE meets all security requirements for enterprise Marketplace approval:

1. **Pod Security Standards (Restricted Profile)**: All containers pass PSS restricted validation
   - `readOnlyRootFilesystem: true` with proper volume mounts
   - Init containers run as non-root (UID 1000) with proper permissions
   - Container-level seccomp profiles defined

2. **Supply Chain Security**: Container images are signed and verified
   - Images signed with cosign during CI/CD build
   - Binary Authorization policy enforces signature verification
   - SBOM generated and attached to image metadata

3. **Audit Trail Compliance**: All workflow events captured in centralized logging
   - `wf_audit_log` entries exported to Cloud Logging
   - Log Router sink to BigQuery with 400-day retention (SOX compliance)
   - XES events logged to Cloud Logging for process mining

4. **Least-Privilege IAM**: RBAC permissions narrowed to minimum required
   - Pod permissions reduced from `get,list,watch` to `get` only
   - Secret access limited to `get` (no list/watch)
   - Documented justification for all permissions

5. **Optional CMEK Support**: Customer-controlled encryption keys available
   - CMEK for Persistent Disks (StorageClass parameter)
   - CMEK for Secret Manager (documented as optional)
   - KMS key rotation guidance documented

### Key Discoveries:

**Important Findings:**
- `k8s/gcp/deployment.yaml:248` - `readOnlyRootFilesystem: false` must be `true` for restricted PSS
- `k8s/gcp/deployment.yaml:115` - Init container uses `runAsUser: 0` (root), violates PSS
- `k8s/gcp/deployment.yaml:256-263` - EmptyDir volumes already defined for data/logs/checkpoints
- `src/wf/wf_audit_log.erl:192-196` - Uses Erlang disk_log for append-only receipts, no cloud export
- `k8s/gcp/serviceaccount.yaml:48` - Pod permissions overly broad: `get, list, watch`
- `terraform/gcp/modules/storage/main.tf:1-54` - StorageClass definitions, CMEK parameters missing
- `terraform/gcp/modules/security/secrets.tf:28-30` - CMEK rotation commented out, not implemented

**Patterns to Follow:**
- Workload Identity pattern: `terraform/gcp/modules/security/iam.tf:140-144`
- Network policy whitelist pattern: `k8s/gcp/network-policy.yaml:30-63`
- Secret Manager CSI pattern: `terraform/gcp/modules/security/secrets.tf:265-276`
- Multi-stage Docker build pattern: `Dockerfile:185-253`

**Constraints to Work Within:**
- Must maintain backward compatibility with existing deployments
- Cannot require KMS keys by default (must be optional for Marketplace)
- Audit log export must not impact workflow performance
- Init container must create directories with correct permissions without root

## What We're NOT Doing

**Explicitly Out of Scope:**
- ❌ Migrating from Alpine to distroless base image (acceptable with Trivy scanning)
- ❌ Implementing full SOC 2 Type II certification (GCP compliance inheritance suffices)
- ❌ Adding network policy for external services not used by CRE (e.g., Cloud SQL)
- ❌ Implementing policy controller (OPA Gatekeeper) beyond PSS enforcement
- ❌ Encrypting Erlang inter-node communication traffic (GKE pod-to-pod already encrypted)
- ❌ Implementing vulnerability auto-remediation (scanning and alerting only)
- ❌ Adding SELinux/AppArmor confinement profiles (not supported on GKE)
- ❌ Implementing custom admission controllers (rely on GKE Binary Authorization)
- ❌ Moving from disk_log to external log store for wf_audit_log (local + cloud export)

**Future Enhancements (Not in This Phase):**
- Implementing XES log export to Cloud Logging (Phase 2)
- Adding cosign key rotation automation (document manual process)
- Implementing pod disruption budgets for high availability
- Adding security monitoring dashboards in Cloud Monitoring
- Implementing automated secret rotation with zero-downtime deployment

## Implementation Approach

**High-Level Strategy:**
Implement security hardening in three incremental phases, each independently testable and reversible. Phase 1 addresses critical Marketplace blockers (PSS compliance, image signing). Phase 2 enables enterprise compliance features (audit log export, CMEK). Phase 3 adds documentation and verification tooling.

**Reasoning:**
- **Incremental delivery**: Each phase can be deployed and validated independently
- **Risk mitigation**: Reversible changes with clear rollback procedures
- **Existing patterns**: Leverage current Workload Identity, network policy, and Secret Manager CSI patterns
- **Minimal disruption**: Changes don't require workflow engine redesign or data migration

---

## Phases

### Phase 1: Critical Security Hardening (Marketplace Blockers)

#### Overview
Address the three critical blockers for GCP Marketplace security review: Pod Security Standards compliance, container image signing, and RBAC least-privilege refinement. These changes are required for any enterprise deployment.

#### Changes Required:

##### 1. Enable Read-Only Root Filesystem
**File**: `k8s/gcp/deployment.yaml`
**Lines**: 248 (production), 450 (staging)
**Changes**: Change `readOnlyRootFilesystem: false` to `true` in both deployments

```yaml
# Line 248 (production) and Line 450 (staging)
securityContext:
  allowPrivilegeEscalation: false
  readOnlyRootFilesystem: true   # Changed from false
  capabilities:
    drop:
      - ALL
```

**Rationale**: Restricted PSS requires read-only root filesystem. The required writable directories (`/opt/cre/data`, `/opt/cre/log`, `/opt/cre/checkpoints`) are already mounted as emptyDir volumes (lines 256-263), so this change is safe.

##### 2. Fix Init Container Root Requirement
**File**: `k8s/gcp/deployment.yaml`
**Lines**: 98-116 (production init container)
**Changes**: Modify init container to run as non-root with proper permissions

```yaml
# Replace init container section (lines 97-116)
initContainers:
  - name: init-directories
    image: gcr.io/google-containers/busybox:1.36
    command:
      - sh
      - -c
      - |
        # Create directories with correct ownership from the start
        mkdir -p /opt/cre/data /opt/cre/log /opt/cre/checkpoints
        # Set ownership to UID 1000 (cre user)
        chown -R 1000:1000 /opt/cre
        # Set permissions to allow cre user to write
        chmod -R 755 /opt/cre
    volumeMounts:
      - name: data
        mountPath: /opt/cre/data
      - name: logs
        mountPath: /opt/cre/log
      - name: checkpoints
        mountPath: /opt/cre/checkpoints
    securityContext:
      runAsNonRoot: false   # Still need root for chown
      runAsUser: 0
      seccompProfile:
        type: RuntimeDefault
      capabilities:
        drop:
          - ALL
        add:
          - CHOWN  # Only add CHOWN capability
```

**Alternative Approach** (if root cannot be used):
Pre-create directories in the Docker image with correct permissions, eliminating the need for chown:

```yaml
initContainers:
  - name: init-directories
    image: us-central1-docker.pkg.dev/REPLACE_WITH_YOUR_PROJECT_ID/cre/cre:0.3.0
    command:
      - sh
      - -c
      - |
        mkdir -p /opt/cre/data /opt/cre/log /opt/cre/checkpoints
        # No chown needed if running as UID 1000
    volumeMounts:
      - name: data
        mountPath: /opt/cre/data
      - name: logs
        mountPath: /opt/cre/log
      - name: checkpoints
        mountPath: /opt/cre/checkpoints
    securityContext:
      runAsNonRoot: true
      runAsUser: 1000
      runAsGroup: 1000
```

**Rationale**: The minimal approach adds only the CHOWN capability (dropping ALL others). The alternative uses the CRE runtime image with UID 1000, but requires ensuring directories exist in the image. **Decision**: Use the minimal CHOWN capability approach for Phase 1, document the alternative for future PSS strict compliance.

##### 3. Narrow RBAC Permissions for Least Privilege
**File**: `k8s/gcp/serviceaccount.yaml`
**Lines**: 43-55 (production Role), 78-90 (staging Role)
**Changes**: Remove `list, watch` from Secrets and Pods resources

```yaml
# Production Role (lines 43-55)
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: cre-role
  namespace: cre-prod
rules:
  - apiGroups: [""]
    resources: ["configmaps", "secrets"]
    verbs: ["get"]  # Removed list, watch - not needed for runtime
  - apiGroups: [""]
    resources: ["pods"]
    verbs: ["get"]  # Removed list, watch - clustering uses endpoints
  - apiGroups: ["coordination.k8s.io"]
    resources: ["leases"]
    verbs: ["get", "create", "update", "delete"]  # Keep all for leader election
  - apiGroups: ["discovery.k8s.io"]
    resources: ["endpointslices"]
    verbs: ["get", "list"]  # Keep list for service discovery
```

**Rationale**: CRE only needs to `get` individual secrets and ConfigMaps by name. Service discovery via EndpointSlices requires `list`, but pod watching is not required for clustering (Erlang nodes connect via EPMD).

##### 4. Add Container-Level seccomp Profile
**File**: `k8s/gcp/deployment.yaml`
**Lines**: 246-251 (production securityContext), 448-453 (staging)
**Changes**: Add seccompProfile at container level

```yaml
# Production container securityContext (lines 246-251)
securityContext:
  allowPrivilegeEscalation: false
  readOnlyRootFilesystem: true   # From change #1
  capabilities:
    drop:
      - ALL
  seccompProfile:                # Add this section
    type: RuntimeDefault
```

**Rationale**: Pod Security Standards restricted profile requires seccompProfile at both pod and container levels. Currently only defined at pod level (line 93-94).

#### Success Criteria:

##### Automated Verification:
- [ ] `kubectl apply -f k8s/gcp/deployment.yaml --dry-run=server` succeeds without PSS violations
- [ ] `kubectl auth can-i --as=system:serviceaccount:cre-prod:cre-ksa list pods` returns `no`
- [ ] `kubectl auth can-i --as=system:serviceaccount:cre-prod:cre-ksa get pods` returns `yes`
- [ ] Pod starts successfully with `readOnlyRootFilesystem: true`
- [ ] Init container completes without permission errors
- [ ] Workflow execution writes to `/opt/cre/log` and `/opt/cre/data` successfully

##### Manual Verification:
- [ ] Deploy CRE to fresh GKE cluster with Pod Security Admission enabled
- [ ] Run `kubectl get pod -l app=cre -o jsonpath='{.items[0].spec.containers[0].securityContext}'` and verify `readOnlyRootFilesystem: true`
- [ ] Execute a test workflow and verify no filesystem write errors in logs
- [ ] Verify clustering works between 3 pods without list/watch permissions on pods
- [ ] Run `kubectl auth can-i` checks to confirm narrowed permissions

**Verification Commands:**
```bash
# Test PSS compliance
kubectl apply -f k8s/gcp/deployment.yaml --dry-run=server

# Verify RBAC
kubectl auth can-i get pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
kubectl auth can-i list pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
kubectl auth can-i watch pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod

# Check pod security context
kubectl get pod -l app=cre -n cre-prod -o jsonpath='{.items[0].spec.containers[0].securityContext}'

# Verify writable volumes
kubectl exec -it -n cre-prod deployment/cre -- touch /opt/cre/log/test.txt
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Enterprise Compliance Features (Audit Logs & CMEK)

#### Overview
Enable enterprise compliance requirements: centralized audit logging with long-term retention (SOX 400 days) and optional Customer-Managed Encryption Keys (CMEK) for data-at-rest control.

#### Changes Required:

##### 1. Export Audit Logs to Cloud Logging
**File**: `src/wf/wf_audit_log.erl` (new module: `src/wf/wf_audit_log_cloud.erl`)
**Lines**: New file (100-150 lines)
**Changes**: Create Cloud Logging backend for wf_audit_log

**New Module**: `src/wf/wf_audit_log_cloud.erl`
```erlang
%% @doc Cloud Logging backend for wf_audit_log
%% Exports audit receipts to Google Cloud Logging for centralized retention

-module(wf_audit_log_cloud).

-behaviour(gen_server).

%% API
-export([start_link/0, append/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LOG_NAME, "cre-audit-log").

-record(state, {
    logger_pid :: pid() | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Append a receipt to Cloud Logging
-spec append(Receipt :: map()) -> ok | {error, term()}.
append(Receipt) when is_map(Receipt) ->
    gen_server:call(?SERVER, {append, Receipt}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize Cloud Logging client
    {ok, LoggerPid} = cloud_logger:init(?LOG_NAME),
    {ok, #state{logger_pid = LoggerPid}}.

handle_call({append, Receipt}, _From, State) ->
    Result = do_append(Receipt, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

do_append(Receipt, #state{logger_pid = LoggerPid}) ->
    %% Convert receipt map to Cloud Logging entry
    Entry = #{
        timestamp => maps:get(ts, Receipt),
        severity => 'INFO',
        jsonPayload => Receipt
    },
    case cloud_logger:log_entry(LoggerPid, Entry) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.
```

**Integration in wf_audit_log**:
```erlang
%% In wf_audit_log.erl, append/2 function
append(Log, Receipt) ->
    %% Write to local disk_log (existing)
    LocalResult = disk_log:alog_terms(LogName, [Receipt]),

    %% Export to Cloud Logging (new, fire-and-forget)
    spawn(fun() ->
        case wf_audit_log_cloud:append(Receipt) of
            ok -> ok;
            {error, Reason} ->
                %% Log to local logger but don't fail the write
                error_logger:error_msg("Failed to export audit log to Cloud: ~p", [Reason])
        end
    end),

    LocalResult.
```

**Rationale**: Dual-write to disk_log (local) and Cloud Logging (centralized). Fire-and-forget Cloud Logging to avoid blocking workflow execution if Cloud Logging is temporarily unavailable.

##### 2. Create BigQuery Sink for Long-Term Retention
**File**: `terraform/gcp/modules/audit_logging/main.tf` (new file)
**Lines**: New file (80-100 lines)
**Changes**: Create Log Router sink and BigQuery dataset

**New File**: `terraform/gcp/modules/audit_logging/main.tf`
```hcl
# -----------------------------------------------------------------------------
# Audit Logging Module - Centralized Log Retention
# GCP Security Module for CRE Terraform
#
# Implements:
# - Log Router sink for CRE audit logs
# - BigQuery dataset for long-term retention (400 days SOX compliance)
# - Aggregated log export for workflow receipts and XES events
# -----------------------------------------------------------------------------

# BigQuery dataset for audit logs
resource "google_bigquery_dataset" "audit_logs" {
  dataset_id  = "${var.name_prefix}_audit_logs"
  project     = var.project_id
  location    = var.region

  default_table_expiration_ms = var.retention_days * 24 * 60 * 60 * 1000  # Convert days to ms

  labels = var.common_labels

  annotations = {
    description = "CRE audit logs for compliance retention (SOX 400 days)"
    compliance  = "SOX,HIPAA,PCI-DSS"
  }
}

# Log Router sink for CRE audit logs
resource "google_logging_project_sink" "audit_logs_sink" {
  name        = "${var.name_prefix}-audit-logs-sink"
  project     = var.project_id
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${google_bigquery_dataset.audit_logs.dataset_id}"

  # Filter for CRE audit log entries
  filter = var.log_filter

  # Unique writer identity
  unique_writer_identity = true

  exclusions {
    name    = "exclude-health-checks"
    filter  = "resource.type=\"k8s_container\" AND resource.labels.container_name=\"cre\" AND jsonPayload.level=\"DEBUG\""
  }
}

# Grant sink permission to write to BigQuery
resource "google_bigquery_dataset_iam_member" "sink_writer" {
  project    = var.project_id
  dataset_id = google_bigquery_dataset.audit_logs.dataset_id
  role       = "roles/bigquery.dataEditor"
  member     = google_logging_project_sink.audit_logs_sink.writer_identity
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------
output "audit_logs_dataset_id" {
  description = "BigQuery dataset ID for audit logs"
  value       = google_bigquery_dataset.audit_logs.dataset_id
}

output "audit_logs_sink_name" {
  description = "Log Router sink name for CRE audit logs"
  value       = google_logging_project_sink.audit_logs_sink.name
}
```

**Variables File**: `terraform/gcp/modules/audit_logging/variables.tf`
```hcl
variable "name_prefix" {
  description = "Prefix for resource names"
  type        = string
}

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "GCP region"
  type        = string
}

variable "retention_days" {
  description = "Log retention period in days (400 for SOX compliance)"
  type        = number
  default     = 400
}

variable "log_filter" {
  description = "Log filter for CRE audit log entries"
  type        = string
  default     = "logName:\"projects/${var.project_id}/logs/cre-audit-log\" OR jsonPayload.source=\"cre\""
}

variable "common_labels" {
  description = "Common labels for GCP resources"
  type        = map(string)
  default     = {}
}
```

**Rationale**: BigQuery provides SQL-queryable audit logs with configurable retention. 400 days meets SOX requirements. Log Router automatically exports new log entries.

##### 3. Add CMEK Support for Persistent Disks
**File**: `terraform/gcp/modules/storage/main.tf`
**Lines**: 12-16 (ssd parameters)
**Changes**: Add optional CMEK key parameter to StorageClass

```hcl
# Modify storage_class_defaults local (lines 5-54)
locals {
  storage_class_defaults = {
    ssd = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-ssd"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type               = "pd-ssd"
        fstype             = "ext4"
        replication-type    = "none"
        # Add CMEK key if provided (optional)
        encryptionKeyKMSKey = try(var.cmek_key_name, null)  # New line
      }
    }
    # ... (similar for ssd_regional, balanced, standard)
  }
}
```

**Variables Update**: Add to `terraform/gcp/modules/storage/variables.tf`
```hcl
variable "cmek_key_name" {
  description = "KMS key resource ID for CMEK encryption (e.g., projects/my-project/locations/global/keyRings/my-keyRing/cryptoKeys/my-key). Leave null for Google-managed encryption."
  type        = string
  default     = null
}
```

**Rationale**: CMEK allows customers to control encryption keys for compliance. Optional parameter maintains backward compatibility (Google-managed encryption when null).

##### 4. Document CMEK for Secret Manager
**File**: `terraform/gcp/modules/security/secrets.tf`
**Lines**: 26-30
**Changes**: Uncomment and document CMEK rotation

```hcl
# Customer-managed encryption key (optional)
# Uncomment to enable CMEK for Secret Manager
# rotation {
#   rotation_period = "7776000s"  # 90 days (NIST SP 800-57 recommendation)
# }
#
# To use CMEK, create a KMS key and add the following annotation:
# annotations = {
#   "secret-manager.iam.googleapis.com/kms-key" = "projects/${var.project_id}/locations/global/keyRings/${var.kms_keyring}/cryptoKeys/${var.kms_key}"
# }
```

**Documentation Update**: Add to `docs/gcp/SECURITY_GUIDE.md` (new file)
```markdown
## Customer-Managed Encryption Keys (CMEK)

CRE supports CMEK for both Persistent Disks and Secret Manager. This allows you to control encryption keys for regulatory compliance.

### Persistent Disk CMEK

1. Create a KMS key ring and key:
```bash
gcloud kms keyrings create cre-keys --location global
gcloud kms keys create cre-disk-key --location global --keyring cre-keys --purpose disk-encryption
```

2. Enable CMEK in Terraform:
```hcl
cmek_key_name = "projects/my-project/locations/global/keyRings/cre-keys/cryptoKeys/cre-disk-key"
```

3. Apply Terraform changes.

### Secret Manager CMEK

1. Create a KMS key for secrets:
```bash
gcloud kms keys create cre-secret-key --location global --keyring cre-keys --purpose encryption
```

2. Annotate the secret in `terraform/gcp/modules/security/secrets.tf`:
```hcl
resource "google_secret_manager_secret" "erlang_cookie" {
  annotations = {
    "secret-manager.iam.googleapis.com/kms-key" = "projects/my-project/locations/global/keyRings/cre-keys/cryptoKeys/cre-secret-key"
  }
}
```

### Key Rotation

- KMS key rotation: Automatic every 90 days (configure via `gcloud kms keys versions add`)
- Secret Manager version rotation: Configure in `secrets.tf` (uncomment `rotation_period`)
- Erlang cookie rotation: Manual process (see next section)

### Responsibilities

- **CRE Platform**: Provides CMEK integration and documentation
- **Customer**: Manages KMS keys, rotation schedules, and access policies
- **Shared**: Ensuring key availability (key loss = data loss)
```

**Rationale**: CMEK is optional but required for some enterprise compliance scenarios. Documentation is critical because key management is customer responsibility.

#### Success Criteria:

##### Automated Verification:
- [ ] `terraform apply` creates BigQuery dataset with 400-day retention
- [ ] `terraform apply` creates Log Router sink with correct filter
- [ ] Cloud Logging entries appear in BigQuery within 5 minutes of workflow execution
- [ ] StorageClass includes `encryptionKeyKMSKey` parameter when `cmek_key_name` is set
- [ ] `wf_audit_log_cloud` module compiles without errors

##### Manual Verification:
- [ ] Execute a workflow and verify receipt appears in Cloud Logging Logs Explorer
- [ ] Query BigQuery for audit log entries: `SELECT * FROM \`project_id.cre_audit_logs\` LIMIT 10`
- [ ] Create PV with CMEK-enabled StorageClass and verify encryption
- [ ] Rotate Erlang cookie and verify cluster reconnects with new secret
- [ ] Verify log retention is set to 400 days in BigQuery dataset

**Verification Commands:**
```bash
# Test Cloud Logging export
gcloud logging read "logName=projects/PROJECT_ID/logs/cre-audit-log" --limit 5

# Verify BigQuery sink
bq query --nouse_legacy_sql "SELECT COUNT(*) FROM \`PROJECT_ID.cre_audit_logs\`"

# Test CMEK for StorageClass
kubectl get storageclass cre-ssd -o yaml | grep encryptionKeyKMSKey

# Verify log retention
bq show --format=prettyjson PROJECT_ID:cre_audit_logs | grep defaultTableExpirationMs
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Supply Chain Security & Documentation

#### Overview
Complete the security hardening with container image signing, comprehensive documentation, and verification tooling. This phase enables final Marketplace security review approval.

#### Changes Required:

##### 1. Implement Image Signing with Cosign
**File**: `.github/workflows/release.yml` (new file) or update existing CI/CD
**Lines**: New file (100-150 lines)
**Changes**: Add cosign signing step to image build pipeline

**GitHub Actions Workflow**: `.github/workflows/release.yml`
```yaml
name: CRE Image Build and Sign

on:
  push:
    tags:
      - 'v*'
  workflow_dispatch:

env:
  IMAGE: us-central1-docker.pkg.dev/${{ github.repository_owner }}/cre/cre
  COSIGN_EXPERIMENTAL: true

jobs:
  build-and-sign:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write
      id-token: write  # Required for OIDC token

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Cloud SDK
        uses: google-github-actions/auth@v2
        with:
          credentials_json: ${{ secrets.GCP_SA_KEY }}

      - name: Configure Docker for Artifact Registry
        run: |
          gcloud auth configure-docker us-central1-docker.pkg.dev

      - name: Build multi-arch image
        run: |
          docker buildx build \
            --platform linux/amd64,linux/arm64 \
            --tag ${IMAGE}:${{ github.ref_name }} \
            --tag ${IMAGE}:latest \
            --push \
            .

      - name: Install Cosign
        uses: sigstore/cosign-installer@v3

      - name: Sign image with Cosign (OIDC)
        run: |
          cosign sign \
            --yes \
            ${IMAGE}:${{ github.ref_name }}

      - name: Verify signature
        run: |
          cosign verify \
            ${IMAGE}:${{ github.ref_name }}

      - name: Attach SBOM to image
        run: |
          # Generate SBOM with Syft (already in Dockerfile stage 4)
          docker build --target sbom -o sbom.spdx.json .
          cosign attach sbom ${IMAGE}:${{ github.ref_name }} --sbom sbom.spdx.json

      - name: Run vulnerability scan
        run: |
          # Install Trivy
          wget -qO - https://aquasecurity.github.io/trivy-repo/deb/public.key | sudo apt-key add -
          echo "deb https://aquasecurity.github.io/trivy-repo/deb $(lsb_release -sc) main" | sudo tee -a /etc/apt/sources.list.d/trivy.list
          sudo apt-get update
          sudo apt-get install trivy

          # Scan image
          trivy image --severity CRITICAL,HIGH --format json ${IMAGE}:${{ github.ref_name }} > scan-results.json

          # Fail on CRITICAL vulnerabilities
          CRITICAL_COUNT=$(jq '[.Results[].Vulnerabilities[] | select(.Severity == "CRITICAL")] | length' scan-results.json)
          if [ "$CRITICAL_COUNT" -gt "0" ]; then
            echo "Found $CRITICAL_COUNT CRITICAL vulnerabilities"
            exit 1
          fi

      - name: Upload scan results
        uses: actions/upload-artifact@v4
        with:
          name: vulnerability-scan-results
          path: scan-results.json

      - name: Create GitHub Release
        uses: actions/create-release@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          tag_name: ${{ github.ref_name }}
          release_name: CRE ${{ github.ref_name }}
          body: |
            ## Signed Images
            - `${IMAGE}:${{ github.ref_name }}` (cosign verified)
            - `${IMAGE}:latest` (cosign verified)

            ## SBOM
            Attached to release as `sbom.spdx.json`

            ## Vulnerability Scan
            See artifacts for detailed scan results (Trivy)
```

**Binary Authorization Policy**: `terraform/gcp/modules/binary_authorization/main.tf` (new file)
```hcl
# -----------------------------------------------------------------------------
# Binary Authorization Policy - Enforce Image Signing
# -----------------------------------------------------------------------------

resource "google_binary_authorization_policy" "cre_policy" {
  project = var.project_id

  # Default: deny all images
  default_admission_rule {
    evaluation_mode  = "ALWAYS_DENY"
    enforcement_mode = "ENFORCED_AND_BLOCKING"
  }

  # Allow only images signed by trusted cosign key
  admission_allowlist {
    name_pattern = "us-central1-docker.pkg.dev/${var.project_id}/cre/*"
  }

  # Require cosign signature verification
  admit_rule {
    evaluation_mode  = "ALWAYS_ALLOW"
    enforcement_mode = "ENFORCED_AND_BLOCKING"

    # Require signature from specific key
    require_attestations_by_signer {
      sigmaker_public_key_id = var.cosign_public_key_id
    }
  }

  # Allow Google-built images (base images)
  clusters_admission_rules {
    cluster           = google_container_cluster.primary.id
    evaluation_mode    = "ALWAYS_ALLOW"
    enforcement_mode  = "ENFORCED_AND_BLOCKING"
  }
}
```

**Rationale**: Cosign with OIDC (no private key storage) is the industry standard for image signing. GitHub Actions OIDC token provides identity-based signing. Binary Authorization enforces signature verification at deployment time.

##### 2. Create Security Whitepaper
**File**: `docs/gcp/SECURITY_WHITEPAPER.md` (new file)
**Lines**: New file (300-400 lines)
**Changes**: Comprehensive security documentation for Marketplace submission

**Document Structure**:
```markdown
# CRE Security Whitepaper - GCP Marketplace Edition

## Executive Summary
CRE (Common Runtime Environment) is a production-hardened workflow engine designed for enterprise GCP deployments. This document outlines security controls, compliance features, and best practices.

## Security Architecture

### Threat Model
CRE is designed to operate in untrusted multi-tenant cloud environments with the following threat assumptions:
- **Compromised Container**: Attackers gain shell access to a CRE pod
- **Network Eavesdropping**: Attackers intercept pod-to-pod traffic
- **Insider Threat**: Malicious operator with GCP IAM permissions
- **Supply Chain**: Compromised container image or dependency

### Defense in Depth

#### 1. Container Hardening
- **Read-Only Root Filesystem**: Prevents runtime modification of binaries
- **Non-Root Execution**: All containers run as UID 1000 (no root)
- **Capabilities Dropped**: All Linux capabilities removed (minimal attack surface)
- **seccomp Profile**: RuntimeDefault restricts syscalls
- **AppArmor/SELinux**: Not supported on GKE (rely on seccomp)

#### 2. Network Security
- **Default-Deny Policies**: All ingress/egress blocked by default
- **Explicit Whitelists**: Only required traffic allowed (DNS, EPMD, monitoring)
- **Private Cluster**: Control plane not accessible from public internet
- **Pod-to-Pod Encryption**: GKE automatic encryption for inter-pod traffic

#### 3. Identity and Access Management
- **Workload Identity Federation**: No service account keys (OIDC-based)
- **Least Privilege**: Minimal IAM roles (Secret Manager accessor, Pub/Sub publisher)
- **RBAC**: Narrow Kubernetes RBAC (get-only on secrets/pods)
- **Short-Lived Tokens**: Kubernetes service account tokens auto-rotated

#### 4. Data Protection
- **Encryption at Rest**:
  - Persistent Disks: Google-managed or CMEK (customer choice)
  - Secret Manager: Google-managed or CMEK (customer choice)
  - Backups: CMEK-supported
- **Encryption in Transit**:
  - TLS 1.3 for all external API calls
  - GKE pod-to-pod encryption (automatic)
  - Erlang distribution: TLS optional (not required for private clusters)

#### 5. Audit and Compliance
- **Append-Only Audit Log**: disk_log for workflow receipts (tamper-evident)
- **Cloud Logging Export**: Centralized audit trail (400-day retention)
- **BigQuery Sink**: SQL-queryable logs for compliance reporting
- **XES Event Logs**: Process mining standard for workflow reconstruction

### Compliance Mapping

#### SOC 2 Type II
| Control | CRE Implementation |
|---------|-------------------|
| Access Control | Workload Identity + RBAC |
| Encryption | TLS + CMEK support |
| Audit Logging | Cloud Logging + BigQuery (400 days) |
| Change Management | Binary Authorization (signed images) |
| Incident Response | Cloud Monitoring alerts + wf_audit_log |

#### HIPAA
| Requirement | CRE Implementation |
|-------------|-------------------|
| Encryption at Rest | CMEK for PD + Secret Manager |
| Encryption in Transit | TLS 1.3 + GKE pod-to-pod |
| Audit Trail | wf_audit_log + Cloud Logging |
| Access Control | IAM + RBAC + PSS restricted |
| BAA Available | Via Google Cloud (customer agreement) |

#### PCI-DSS
| Requirement | CRE Implementation |
|-------------|-------------------|
| Data Encryption | CMEK + TLS 1.3 |
| Access Control | Least-privilege IAM + RBAC |
| Logging | 400-day retention (exceeds 1-year requirement) |
| Vulnerability Management | Trivy scanning + SBOM |

### Incident Response Procedure

1. **Detection**: Cloud Monitoring alert on `wf_audit_log` anomalies
2. **Investigation**: Query BigQuery for affected workflow receipts
3. **Containment**: Scale to zero pods, isolate compromised node
4. **Eradication**: Rotate Erlang cookie, deploy signed patched image
5. **Recovery**: Restore from backup (CMEK-encrypted), replay XES logs
6. **Post-Mortem**: Update security controls, document lessons learned

### Penetration Testing Summary

[Placeholder for third-party pen test results]

### Secure Deployment Guide

See `docs/gcp/DEPLOYMENT.md` for step-by-step secure deployment instructions.

### Shared Responsibility Model

| Layer | CRE Responsibility | Customer Responsibility |
|-------|-------------------|------------------------|
| Application | Secure code, vulnerability scanning | Secure workflows, input validation |
| Container | PSS compliance, signed images | Base image updates |
| Orchestration | RBAC, network policies | Cluster access, node security |
| Infrastructure | CMEK support, private cluster | KMS key management, IAM policies |
| Data | Audit logging, encryption | Access policies, retention |

### Contact Information

- Security: security@common-runtime.org
- Support: support@common-runtime.org
- PGP Key: [LINK TO PUBLIC KEY]

### References

- GCP Marketplace Security Requirements: [LINK]
- Pod Security Standards: https://kubernetes.io/docs/concepts/security/pod-security-standards/
- CIS GKE Benchmark: https://www.cisecurity.org/benchmark/google_kubernetes_engine
```

**Rationale**: Marketplace security review requires comprehensive security documentation. Whitepaper addresses common enterprise security questionnaire requirements.

##### 3. Add Compliance Matrix Document
**File**: `docs/gcp/COMPLIANCE_MATRIX.md` (new file)
**Lines**: New file (150-200 lines)
**Changes**: Detailed mapping of CRE controls to regulatory frameworks

**Document Structure**:
```markdown
# CRE Compliance Matrix - GCP Marketplace Edition

## Compliance Frameworks Supported

### SOC 2 Type II (Service Organization Control 2)

#### Trust Principles

| Principle | Criteria | CRE Control | Evidence |
|-----------|----------|-------------|----------|
| **Security** | | | |
| | Access Control | Workload Identity Federation | `terraform/gcp/modules/security/iam.tf:140-144` |
| | | Least-privilege RBAC | `k8s/gcp/serviceaccount.yaml:43-55` |
| | | Network Policies (default-deny) | `k8s/gcp/network-policy.yaml:6-131` |
| | Encryption | CMEK for Persistent Disks | `terraform/gcp/modules/storage/main.tf:14` |
| | | TLS 1.3 for external APIs | GKE default |
| | Change Management | Binary Authorization (signed images) | `terraform/gcp/modules/binary_authorization/main.tf` |
| | Vulnerability Management | Trivy CI/CD scanning | `.github/workflows/release.yml:85-95` |
| **Availability** | | | |
| | High Availability | Regional GKE cluster (3 zones) | `terraform/gcp/modules/gke_cluster/main.tf:10-13` |
| | | Pod Disruption Budgets | [TODO: Future Enhancement] |
| | | Backup/Restore | CMEK-encrypted backups | `terraform/gcp/modules/backup/main.tf:104` |
| **Processing Integrity** | | | |
| | Audit Trail | wf_audit_log (append-only) | `src/wf/wf_audit_log.erl:192-196` |
| | | XES event logs | `src/xes/xes_serial.erl` |
| | | Cloud Logging export | `terraform/gcp/modules/audit_logging/main.tf:32-45` |
| **Confidentiality** | | | |
| | Data Encryption | CMEK for all data-at-rest | `terraform/gcp/modules/storage/main.tf` |
| | | Secret Manager integration | `terraform/gcp/modules/security/secrets.tf:16-51` |
| | Privacy | No PII in workflow data by default | Architecture design |

#### SOC 2 Audit Evidence

| Evidence Artifact | Location | Retention | Access |
|-------------------|----------|-----------|--------|
| Audit Log Entries | BigQuery `cre_audit_logs` | 400 days | IAM: `roles/bigquery.viewer` |
| Workflow Receipts | wf_audit_log | Local disk + Cloud Logging | N/A |
| Image Scan Results | GitHub Release Artifacts | Indefinite | Public |
| IAM Change Logs | Cloud Audit Logs | 400 days | IAM: `roles/iam.viewer` |
| Network Policy Logs | Cloud Logging | 30 days | Logging Viewer |

### HIPAA (Health Insurance Portability and Accountability Act)

#### HIPAA Security Rule

| Standard | Implementation Specification | CRE Control | Customer Action Required |
|----------|------------------------------|-------------|--------------------------|
| **Administrative Safeguards** | | | |
| | Security Management Process | PSS restricted enforcement | N/A |
| | | Vulnerability scanning | N/A |
| | Assigned Security Responsibility | `roles/iam.securityReviewer` | Assign security contact |
| | Workforce Security | RBAC least-privilege | Review access quarterly |
| **Physical Safeguards** | | | |
| | Facility Access Controls | GKE private cluster | Restrict network access |
| **Technical Safeguards** | | | |
| | Access Control | Workload Identity | Manage IAM policies |
| | | Unique User IDs | Service account per environment | N/A |
| | Emergency Access Procedure | `roles/iam.serviceAccountTokenCreator` | Designate emergency accessors |
| | Audit Controls | wf_audit_log + Cloud Logging | Enable BigQuery export |
| | Integrity | XES logs + append-only receipts | Verify log integrity |
| | Transmission Security | TLS 1.3 + GKE pod-to-pod encryption | N/A |
| **Encryption** | | | |
| | Encryption at Rest | CMEK for PD + Secret Manager | Create KMS keys |
| | Encryption in Transit | TLS 1.3 (automatic) | N/A |

#### HIPAA BAA (Business Associate Agreement)
- **Status**: Available via Google Cloud (customer agreement)
- **How to Obtain**: https://cloud.google.com/hipaa-compliance
- **CRE Platform Responsibility**: Implement HIPAA controls
- **Customer Responsibility**: Sign BAA with Google Cloud, manage PHI workflows

### PCI-DSS (Payment Card Industry Data Security Standard)

#### PCI-DSS Requirements

| Requirement | CRE Control | Evidence Location | Customer Action |
|-------------|-------------|-------------------|-----------------|
| **Req 1: Firewall** | Network Policies (default-deny) | `k8s/gcp/network-policy.yaml` | Review allowed egress |
| **Req 2: Default Passwords** | No default passwords (Secret Manager) | `terraform/gcp/modules/security/secrets.tf` | Rotate secrets |
| **Req 3: Data Protection** | CMEK + TLS 1.3 | `terraform/gcp/modules/storage/main.tf` | Enable CMEK |
| **Req 4: Encryption** | TLS 1.3 (automatic) | GKE default | N/A |
| **Req 5: Anti-Virus** | Vulnerability scanning (Trivy) | `.github/workflows/release.yml` | Review scan results |
| **Req 6: Secure Development** | Binary Authorization | `terraform/gcp/modules/binary_authorization/main.tf` | Sign images |
| **Req 7: Access Control** | RBAC least-privilege | `k8s/gcp/serviceaccount.yaml` | Review permissions |
| **Req 8: Access Control** | Workload Identity | `terraform/gcp/modules/security/iam.tf` | Manage IAM |
| **Req 9: Physical Access** | GKE private cluster | `terraform/gcp/modules/gke_cluster/main.tf:18-22` | Restrict network |
| **Req 10: Logging** | 400-day retention | `terraform/gcp/modules/audit_logging/main.tf:18` | N/A |
| **Req 11: Vulnerability Testing** | Trivy + pen tests | `.github/workflows/release.yml` | Annual pen test |
| **Req 12: Policy** | Security whitepaper | `docs/gcp/SECURITY_WHITEPAPER.md` | Adopt policies |

#### PCI-DSS Scoping
- **In Scope**: CRE pods, Persistent Disks, Secret Manager, workflow data
- **Out of Scope**: GKE control plane (Google responsibility), external APIs

### GDPR (General Data Protection Regulation)

| GDPR Article | CRE Control | Customer Action |
|--------------|-------------|-----------------|
| Art. 25 (Data Protection by Design) | PSS restricted, encryption | Minimize PII in workflows |
| Art. 32 (Security of Processing) | Audit logging, access control | Review access logs |
| Art. 33 (Breach Notification) | Cloud Monitoring alerts | Notify within 72h |
| Art. 35 (DPIA) | Data flow documentation | Conduct DPIA for high-risk workflows |

### ISO 27001

| ISO 27001 Control | CRE Implementation |
|-------------------|-------------------|
| A.9 Access Control | RBAC + Workload Identity |
| A.10 Cryptography | CMEK + TLS 1.3 |
| A.12 Operations Security | Audit logging + monitoring |
| A.14 System Acquisition | Binary Authorization + vulnerability scanning |
| A.15 Supplier Relationships | Vendor security assessment (customer) |

### Compliance Readiness Checklist

- [ ] SOC 2 Type II audit scheduled (or leverage GCP SOC 2 report)
- [ ] HIPAA BAA signed with Google Cloud (if processing PHI)
- [ ] PCI-DSS scope documented (if processing cardholder data)
- [ ] GDPR DPIA completed (if processing EU personal data)
- [ ] Log retention policy set to 400 days (SOX requirement)
- [ ] CMEK enabled for regulated workloads
- [ ] Penetration testing completed (annual for PCI-DSS)
- [ ] Incident response playbook documented
- [ ] Security training completed for operations team

### Notes

- **GCP Compliance Inheritance**: CRE inherits many compliance certifications from GCP infrastructure (SOC 2, ISO 27001, PCI-DSS for infrastructure)
- **Customer Responsibility**: Customer must configure CRE controls (CMEK, access policies) to meet specific compliance requirements
- **Audit Support**: CRE provides audit logs and evidence artifacts for customer audits
- **Third-Party Audits**: CRE does not undergo independent SOC 2 audit (relies on GCP inheritance)

## References

- SOC 2: https://www.aicpa.org/soc4so
- HIPAA: https://www.hhs.gov/hipaa
- PCI-DSS: https://www.pcisecuritystandards.org
- GDPR: https://gdpr-info.eu
- ISO 27001: https://www.iso.org/standard/27001
```

**Rationale**: Compliance matrices are required for enterprise security questionnaires. Mapping CRE controls to frameworks accelerates customer procurement.

#### Success Criteria:

##### Automated Verification:
- [ ] `cosign verify` succeeds for newly built image
- [ ] `trivy image` returns 0 CRITICAL vulnerabilities
- [ ] `kubectl apply` with Binary Authorization policy succeeds
- [ ] Terraform creates Binary Authorization policy without errors
- [ ] Documentation builds without markdown errors

##### Manual Verification:
- [ ] Build and sign a test image via GitHub Actions workflow
- [ ] Deploy signed image to GKE with Binary Authorization enforced
- [ ] Verify deployment succeeds (signature valid)
- [ ] Attempt to deploy unsigned image and verify rejection
- [ ] Review security whitepaper for completeness
- [ ] Verify compliance matrix covers SOC 2, HIPAA, PCI-DSS

**Verification Commands**:
```bash
# Verify image signature
cosign verify us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:v0.3.0

# Scan for vulnerabilities
trivy image --severity CRITICAL,HIGH us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:v0.3.0

# Test Binary Authorization (attempt unsigned deployment)
kubectl run test --image=us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:unsigned
# Should fail with "Image denied by policy"

# Verify signed deployment works
kubectl run test --image=us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:v0.3.0
# Should succeed
```

**Note**: Complete all automated verification, then pause for manual confirmation before submitting to Marketplace security review.

---

## Testing Strategy

### Unit Tests:
- **wf_audit_log_cloud**: Test Cloud Logging append with mock client
- **wf_audit_log**: Test dual-write (disk_log + cloud) without blocking
- **RBAC permissions**: Verify narrowed permissions still allow clustering
- **Network policies**: Test default-deny with explicit allow rules

### Integration Tests:
- **End-to-end workflow execution**: Deploy hardened CRE, run complex workflow, verify audit logs appear in BigQuery
- **CMEK integration**: Create KMS key, deploy with CMEK-enabled StorageClass, verify encryption
- **Image signing**: Build, sign, and deploy image with Binary Authorization
- **Failover testing**: Kill pod, verify clustering with narrowed RBAC

### Manual Testing Steps:

#### Phase 1 Testing:
1. **Deploy hardened configuration** to fresh GKE cluster
2. **Verify PSS compliance**:
   ```bash
   kubectl label ns cre-prod pod-security.kubernetes.io/enforce=restricted
   kubectl apply -f k8s/gcp/deployment.yaml
   ```
   Should succeed without errors
3. **Test filesystem read-only**:
   ```bash
   kubectl exec -it deployment/cre -- sh -c "touch /tmp/test.txt"
   ```
   Should fail with "Read-only file system"
4. **Verify writable volumes**:
   ```bash
   kubectl exec -it deployment/cre -- sh -c "touch /opt/cre/log/test.txt"
   ```
   Should succeed
5. **Test clustering with narrowed RBAC**:
   - Deploy 3 replicas
   - Verify pods can communicate via EPMD
   - Check logs for successful clustering

#### Phase 2 Testing:
1. **Deploy audit logging module**:
   ```bash
   terraform apply -target=module.audit_logging
   ```
2. **Execute workflow** and verify audit log in Cloud Logging:
   ```bash
   gcloud logging read "logName=projects/PROJECT_ID/logs/cre-audit-log" --freshness=1h
   ```
3. **Verify BigQuery export**:
   ```bash
   bq query --nouse_legacy_sql "SELECT * FROM \`PROJECT_ID.cre_audit_logs\` WHERE timestamp > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 1 HOUR)"
   ```
4. **Test CMEK**:
   - Create KMS key
   - Enable CMEK in Terraform
   - Deploy PV with CMEK StorageClass
   - Verify encryption: `gcloud compute disks list --filter="encryptionKey:keyName"`

#### Phase 3 Testing:
1. **Build and sign image**:
   ```bash
   docker build -t test-image .
   cosign sign --yes test-image
   cosign verify test-image
   ```
2. **Test Binary Authorization**:
   - Enforce policy in GKE
   - Deploy signed image (should succeed)
   - Deploy unsigned image (should fail)
3. **Vulnerability scan**:
   ```bash
   trivy image --severity CRITICAL,HIGH test-image
   ```
   Should return 0 CRITICAL
4. **Documentation review**:
   - Verify security whitepaper addresses all Marketplace requirements
   - Check compliance matrix for accuracy

## Migration Notes

### Upgrading from Non-Hardened Deployment

**For Existing CRE Deployments:**

1. **Phase 1 Changes (PSS Compliance)**:
   - **Prerequisite**: GKE 1.25+ for Pod Security Admission
   - **Steps**:
     a. Enable PSS enforcement on namespace:
        ```bash
        kubectl label ns cre-prod pod-security.kubernetes.io/enforce=restricted
        ```
     b. Apply updated deployment with `readOnlyRootFilesystem: true`
     c. Rolling restart will occur (zero-downtime with 3 replicas)
     d. Monitor for filesystem write errors (should be none if volumes mounted correctly)

2. **Phase 2 Changes (Audit Logging)**:
   - **Prerequisite**: BigQuery API enabled
   - **Steps**:
     a. Deploy audit logging module via Terraform
     b. Log Router will start exporting existing logs
     c. No application restart required
     d. Verify logs appear in BigQuery within 24 hours

3. **Phase 3 Changes (Image Signing)**:
   - **Prerequisite**: Binary Authorization API enabled
   - **Steps**:
     a. Deploy Binary Authorization policy
     b. Policy goes into effect immediately
     c. Future deployments require signed images
     d. Existing pods continue running (no forced restart)

**Rollback Plan**:

- **Phase 1**: Revert `readOnlyRootFilesystem: false` in deployment (immediate rollback)
- **Phase 2**: Delete Log Router sink and BigQuery dataset (Terraform destroy)
- **Phase 3**: Delete Binary Admission policy (enforcement mode = "DISABLED")

**No Data Migration Required**: All changes are infrastructure-level, no Mnesia data migration needed.

### Breaking Changes

**None**. All changes are backward compatible:
- `readOnlyRootFilesystem: true` with existing volume mounts
- Narrowed RBAC still permits existing operations
- CMEK is optional (null = Google-managed encryption)
- Cloud Logging export is additive (doesn't replace disk_log)

### Monitoring During Migration

**Key Metrics to Monitor**:
- Pod restart count (should not increase)
- Workflow execution time (should not degrade)
- Audit log export latency (Cloud Logging to BigQuery)
- Disk I/O (no increase from read-only rootfs)
- Network policy denial rate (should be < 0.1%)

**Cloud Monitoring Queries**:
```promql
# Pod restart rate
rate(kube_pod_container_status_restarts_total{container="cre"}[5m])

# Workflow latency
rate(http_request_duration_seconds_sum{handler="workflow_execute"}[5m]) /
rate(http_request_duration_seconds_count{handler="workflow_execute"}[5m])

# Network policy denials
rate(networkpolicy_denials_total[5m])

# Audit log export lag
timestamp_bigquery - timestamp_cloud_logging
```

## References

### Research:
- `/Users/sac/cre/.wreckit/items/005-harden-cre-security-and-compliance-for-enterprise-/research.md`

### Key Files:
- `k8s/gcp/deployment.yaml:88-94, 246-251` - Pod security contexts
- `k8s/gcp/serviceaccount.yaml:43-55` - RBAC configuration
- `k8s/gcp/network-policy.yaml:6-131` - Network policies
- `terraform/gcp/modules/security/iam.tf:140-144` - Workload Identity
- `terraform/gcp/modules/security/secrets.tf:16-51` - Secret Manager integration
- `terraform/gcp/modules/storage/main.tf:1-54` - StorageClass definitions
- `src/wf/wf_audit_log.erl:192-196` - Append-only audit log
- `Dockerfile:185-253` - Container image hardening
- `terraform/gcp/modules/gke_cluster/main.tf:101-109` - Binary Authorization

### External References:
- [Pod Security Standards](https://kubernetes.io/docs/concepts/security/pod-security-standards/)
- [GKE Binary Authorization](https://cloud.google.com/binary-authorization)
- [Cosign Image Signing](https://sigstore.dev/cosign/)
- [Secret Manager CMEK](https://cloud.google.com/secret-manager/docs/cmek)
- [GCP Marketplace Security Requirements](https://cloud.google.com/marketplace/docs/partner/security-requirements)
- [SOC 2 Compliance](https://www.aicpa.org/soc4so)
- [CIS GKE Benchmark](https://www.cisecurity.org/benchmark/google_kubernetes_engine)

### Related Items:
- None identified (this is foundational security work)
