# Research: Harden CRE security and compliance for enterprise GCP deployment

**Date**: 2025-01-11
**Item**: 005-harden-cre-security-and-compliance-for-enterprise-

## Research Question
Enterprise GCP deployments require security hardening, compliance controls, encryption, and audit logging to meet corporate and regulatory standards.

**Motivation:** Enterprise customers require security controls, encryption, compliance readiness, and audit trails. Required for Marketplace approval and enterprise trust.

**Success criteria:**
- RBAC for Kubernetes with principle of least privilege IAM
- NetworkPolicies and Pod Security Standards (baseline+)
- TLS in transit, encrypted disks at rest, optional CMEK
- Audit logging and XES log retention
- Deterministic workflow replay support
- GCP IAM integration with service accounts

**Technical constraints:**
- Pod Security Standards baseline+
- No root containers
- Distroless or minimal base image
- Vulnerability scanning required

**Signals:** priority: critical, urgency: Required for Marketplace security review

## Summary

CRE has a **strong foundation** for enterprise security with extensive GCP Marketplace preparation already completed. The infrastructure demonstrates maturity in security controls, but **gaps exist** in container hardening, CMEK support, and audit log retention policies. The codebase shows evidence of security-aware architecture but requires specific hardening measures to meet enterprise compliance standards.

**Key Findings:**
1. **RBAC is partially implemented** - Service accounts and Roles exist but lack least-privilege refinement
2. **Network policies are comprehensive** - Default-deny with explicit allow rules already defined
3. **Pod Security Standards need enforcement** - Security contexts defined but not uniformly applied
4. **Container hardening is incomplete** - Root filesystem not read-only, distroless not used
5. **Audit logging infrastructure exists** - XES and wf_audit_log modules present, retention policy undefined
6. **CMEK support is partial** - KMS keys referenced in backup module but not for primary storage
7. **Vulnerability scanning is integrated** - Trivy scanning in CI/CD pipeline

## Current State Analysis

### Existing Implementation

#### Kubernetes RBAC
**Location:** `k8s/gcp/serviceaccount.yaml:6-106`

CRE defines Kubernetes service accounts with Workload Identity Federation integration:

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: cre-ksa
  annotations:
    # CRITICAL: Workload Identity annotation maps K8s SA to GCP SA
    iam.gke.io/gcp-service-account: "cre-gke-workload@REPLACE_WITH_YOUR_PROJECT_ID.iam.gserviceaccount.com"
```

**RBAC Role Definition** (lines 38-56):
- ConfigMaps and Secrets: get, list, watch
- Pods: get, list, watch
- Leases (leader election): get, create, update, delete
- EndpointSlices: get, list, watch

**Assessment:** ✅ **Good foundation** but overly broad. The Role grants `get, list, watch` on all pods in the namespace, which exceeds minimum requirements for workflow execution.

#### Network Policies
**Location:** `k8s/gcp/network-policy.yaml:6-131`

CRE implements a **defense-in-depth network policy model**:

1. **Default-deny ingress/egress** (lines 6-27)
2. **CRE internal communication** (lines 30-63) - Allows Erlang EPMD (4369) and distribution ports
3. **DNS access** (lines 65-100) - Required for service discovery
4. **Health check allowances** (lines 109-131)
5. **Cloud Monitoring/Logging egress** (lines 165-286)

**Assessment:** ✅ **Comprehensive and production-ready**. Follows Kubernetes best practices for zero-trust networking.

#### Pod Security Standards
**Location:** `k8s/gcp/deployment.yaml:88-94`

```yaml
securityContext:
  runAsNonRoot: true
  runAsUser: 1000
  runAsGroup: 1000
  fsGroup: 1000
  seccompProfile:
    type: RuntimeDefault
```

**Container-level security context** (lines 246-251):
```yaml
securityContext:
  allowPrivilegeEscalation: false
  readOnlyRootFilesystem: false  # Set to true with proper volume mounts
  capabilities:
    drop:
      - ALL
```

**Assessment:** ⚠️ **Partially compliant**. Missing:
- `readOnlyRootFilesystem: false` should be `true` for restricted PSS
- No `seccompProfile` at container level
- Init container runs as root (line 115: `runAsUser: 0`)

#### GKE Security Configuration
**Location:** `terraform/gcp/modules/gke_cluster/main.tf:64-109`

**Private cluster configuration** (lines 18-22):
```hcl
private_cluster_config {
  enable_private_endpoint = var.private_cluster_config.enable_private_endpoint
  enable_private_nodes    = var.private_cluster_config.enable_private_nodes
  master_ipv4_cidr_block  = var.master_ipv4_cidr_block
}
```

**Shielded nodes** (lines 163-166, 239-242):
```hcl
shielded_instance_config {
  enable_secure_boot          = true
  enable_integrity_monitoring = true
}
```

**Security posture** (lines 101-104):
```hcl
security_posture_config {
  mode               = "ENTERPRISE"
  vulnerability_mode = "VULNERABILITY_BASIC"
}
```

**Binary Authorization** (lines 107-109):
```hcl
binary_authorization {
  evaluation_mode = "PROJECT_SINGLETON_POLICY_ENFORCE"
}
```

**Assessment:** ✅ **Enterprise-grade GKE configuration**. Exceeds baseline requirements.

#### Container Image Security
**Location:** `Dockerfile:185-253`

**Multi-stage build** with:
- Non-root user creation (line 235-236): `adduser -D -u 1000 -G cre cre`
- User switch (line 253): `USER cre`
- Minimal base image: `erlang:28-alpine` (line 185)

**Security scanning integration** (line 1 in `scripts/marketplace/security-scan.sh`):
```bash
trivy image --severity CRITICAL,HIGH --format json ${IMAGE} > scan-results.json
```

**Assessment:** ⚠️ **Partially compliant**. Issues:
- Uses Alpine (not distroless)
- `readOnlyRootFilesystem` not enforced (line 248 in deployment)
- Init containers require root for chown operations
- No signed image verification (cosign) referenced

#### Encryption & CMEK
**Locations:**
- `terraform/gcp/modules/storage/main.tf:1-54` - StorageClass definitions
- `terraform/gcp/modules/backup/main.tf:104` - CMEK reference
- `terraform/gcp/modules/security/secrets.tf:26-30` - Secret encryption

**Current state:**
- Persistent disk encryption: Google-managed (default)
- Secret Manager encryption: Google-managed with optional CMEK support (commented out)
- Backup CMEK: Supported via `var.cmek_key_name`

**Assessment:** ⚠️ **Partial CMEK support**. Missing:
- CMEK for persistent disks (StorageClass)
- CMEK for Secret Manager (commented out, not implemented)
- No documentation for CMEK key rotation

#### IAM & Workload Identity
**Location:** `terraform/gcp/modules/security/iam.tf:15-145`

**GKE Node Service Account** (lines 15-56):
- Minimal permissions: Artifact Registry reader, Logging writer, Monitoring metric writer, Storage object viewer
- ✅ Follows least-privilege principle

**GKE Workload Service Account** (lines 61-101):
- Secret Manager accessor
- Pub/Sub publisher/subscriber
- Monitoring metric writer
- Cloud Trace agent
- ✅ Scoped to CRE requirements

**Workload Identity Federation** (lines 133-144):
```hcl
resource "google_service_account_iam_member" "workload_gke_impersonator" {
  service_account_id = google_service_account.gke_workload.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${var.gke_namespace}/${var.kubernetes_service_account}]"
}
```

**Assessment:** ✅ **Excellent IAM design**. Properly scoped, no service account keys, uses Workload Identity Federation.

#### Audit Logging
**Locations:**
- `src/xes/xes_serial.erl:1-100` - XES event log serialization
- `src/wf/wf_audit_log.erl:1-100` - Append-only receipt audit log

**XES Logging Module** (lines 20-36):
```erlang
%% @doc XES-based Serialization for Event Logs
%% This module implements reading and writing of XES (eXtensible Event
%% Stream) event logs, the standard XML format for process mining.
```

**Audit Log Module** (lines 1-13):
```erlang
%% @doc Append-Only Receipt Audit Log
%% This module provides an append-only audit log for storing workflow receipts
%% using Erlang's disk_log for durable, sequential storage.
```

**Assessment:** ⚠️ **Infrastructure exists, retention undefined**. Missing:
- No GCP Cloud Audit Logs integration documented
- No log export to BigQuery/Splunk for long-term retention
- No log rotation policy for disk_log
- XES logs stored locally, not in Cloud Logging

#### Vulnerability Scanning
**Location:** `scripts/marketplace/security-scan.sh:1-32`

**CI/CD Integration** (`.github/workflows/gcp-cloud-build.yml` - referenced in marketplace checklist):
- Trivy scanning for CRITICAL and HIGH vulnerabilities
- Failure threshold: 0 CRITICAL, max 10 HIGH
- JSON output for parsing

**Assessment:** ✅ **Implemented and enforced in CI/CD**.

### Key Files

| File | Lines | Purpose | Security Relevance |
|------|-------|---------|-------------------|
| `k8s/gcp/serviceaccount.yaml` | 6-106 | RBAC configuration | Service account to GCP SA mapping via Workload Identity |
| `k8s/gcp/network-policy.yaml` | 6-131 | Network security | Default-deny with explicit allow rules for zero-trust |
| `k8s/gcp/deployment.yaml` | 88-94, 246-251 | Pod security | Security contexts, PSS enforcement |
| `terraform/gcp/modules/gke_cluster/main.tf` | 18-22, 64-109 | GKE security | Private cluster, shielded nodes, binary authorization |
| `terraform/gcp/modules/security/iam.tf` | 15-145 | IAM configuration | Least-privilege service accounts, Workload Identity |
| `terraform/gcp/modules/security/secrets.tf` | 16-51 | Secret management | Secret Manager integration, CMEK support (optional) |
| `Dockerfile` | 185-253 | Container image | Multi-stage build, non-root user |
| `src/xes/xes_serial.erl` | 1-100 | Event logging | XES format for process mining compliance |
| `src/wf/wf_audit_log.erl` | 1-100 | Audit trail | Append-only receipt log for deterministic replay |
| `scripts/marketplace/security-scan.sh` | 1-32 | Vulnerability scanning | Trivy integration with fail thresholds |

## Technical Considerations

### Dependencies

**External Dependencies Required:**
1. **GCP Services:**
   - Secret Manager (for Erlang cookie, sensitive config)
   - Cloud KMS (for CMEK, if enabled)
   - Cloud Logging (for audit log export)
   - Cloud Binary Authorization (for image signature verification)
   - Artifact Registry (for signed image storage)

2. **Kubernetes Components:**
   - Secret Store CSI Driver (for Secret Manager integration)
   - Network Policy Controller (Calico, already enabled)
   - Pod Security Admission (GKE 1.25+, already enabled)

3. **Third-Party Tools:**
   - Trivy (vulnerability scanning)
   - Syft (SBOM generation)
   - Cosign (image signing) - **NOT CURRENTLY IMPLEMENTED**
   - External Secrets Operator (for secret sync) - **REFERENCED BUT NOT DEPLOYED**

### Patterns to Follow

**Existing Security Patterns:**

1. **Workload Identity Federation Pattern** (`terraform/gcp/modules/security/workload_identity.tf:139-143`):
   ```hcl
   resource "google_service_account_iam_member" "k8s_cre_app" {
     service_account_id = google_service_account.gke_workload.id
     role               = "roles/iam.workloadIdentityUser"
     member             = "principalSet://iam.googleapis.com/.../attribute.kubernetes_service_account/${var.kubernetes_service_account}"
   }
   ```
   ✅ **Adopt for all GCP service account access**

2. **Network Policy Whitelist Pattern** (`k8s/gcp/network-policy.yaml:30-63`):
   ```yaml
   # Default deny all
   # Explicit allow for CRE internal (EPMD 4369, dist ports)
   # Explicit allow for DNS
   # Explicit allow for monitoring/logging
   ```
   ✅ **Follow for all new services**

3. **Security Context Layering** (`k8s/gcp/deployment.yaml:88-94, 246-251`):
   ```yaml
   # Pod-level: runAsNonRoot, seccompProfile
   # Container-level: allowPrivilegeEscalation: false, capabilities.drop: ALL
   ```
   ⚠️ **Extend to include readOnlyRootFilesystem: true**

4. **Secret Manager CSI Pattern** (`terraform/gcp/modules/security/secrets.tf:265-276`):
   ```erlang
   # Volume mount with CSI driver
   volumes:
   - name: erlang-cookie
     csi:
       driver: secretmanager.csi.k8s.io
       readOnly: true
   ```
   ✅ **Use for all sensitive data**

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Root filesystem writable** | Medium | Enable `readOnlyRootFilesystem: true` with proper volume mounts for `/opt/cre/log`, `/opt/cre/data`, `/tmp` |
| **Init containers run as root** | Medium | Rewrite init logic to run as non-root or use security context with `runAsUser: 1000` |
| **No image signature verification** | High | Implement cosign verification in GKE Binary Authorization policy |
| **CMEK not enforced for primary storage** | Medium | Add CMEK KMS key to StorageClass configuration or document as optional |
| **Audit logs only stored locally** | High | Export XES and wf_audit_log to Cloud Logging with sink to BigQuery for retention |
| **No log retention policy** | High | Define retention period (e.g., 400 days for SOX compliance) and implement rotation |
| **RBAC overly broad (pod listing)** | Low | Narrow Role to only `get` pods (remove `list, `watch`) if not needed |
| **No distroless base image** | Low | Consider distroless refactor (major effort) or document Alpine minimal base rationale |
| **Erlang cookie in Secret Manager** (unrotated) | Medium | Implement automatic rotation policy (90 days) via Cloud Scheduler |
| **XES logs not integrated with Cloud Logging** | Medium | Add Cloud Logging backend for XES events for centralized audit trail |

## Recommended Approach

### Phase 1: Critical Security Hardening (Week 1-2)

**Goal:** Meet Marketplace security baseline requirements.

1. **Enable Read-Only Root Filesystem**
   - Modify `k8s/gcp/deployment.yaml:248`: Change `readOnlyRootFilesystem: false` to `true`
   - Add emptyDir volumes for `/tmp`, `/var/run/erlang` if needed
   - Test with workflow execution to ensure no write attempts to rootfs

2. **Implement Image Signing**
   - Add cosign signing step to `cloudbuild.yaml`
   - Update Binary Authorization policy to require signatures
   - Document in Marketplace submission checklist

3. **Audit Log Export to Cloud Logging**
   - Create Log Router sink for `cre-audit-logs` → BigQuery dataset
   - Modify `wf_audit_log.erl` to write to Cloud Logging backend (in addition to disk_log)
   - Set BigQuery retention: 400 days (SOX compliance)

4. **Narrow RBAC Permissions**
   - Remove `list, watch` from pod permissions in `k8s/gcp/serviceaccount.yaml:48`
   - Test clustering still functions with `get` only

### Phase 2: Compliance Readiness (Week 3-4)

**Goal:** Enable enterprise regulatory compliance (SOC 2, HIPAA, SOX).

1. **CMEK for Persistent Disks**
   - Add KMS key ring and key to `terraform/gcp/modules/storage/main.tf`
   - Update StorageClass parameters: `encryptionKeyKMSKey: <kms-key-name>`
   - Document as optional variable in README

2. **Secret Manager Rotation**
   - Add rotation period to `terraform/gcp/modules/security/secrets.tf:29`
   - Implement Cloud Scheduler job for 90-day rotation
   - Test graceful Erlang node restart with new cookie

3. **XES Log Centralization**
   - Extend XES module to support Cloud Logging export
   - Create Log Router sink for `xes-events` → Cloud Logging
   - Implement log aggregation for process mining compliance

4. **Network Policy Validation**
   - Test default-deny policies in fresh GKE project
   - Verify health checks pass with network policies enabled
   - Document troubleshooting steps

### Phase 3: Enterprise Hardening (Week 5-6)

**Goal:** Exceed Marketplace requirements, demonstrate enterprise maturity.

1. **Pod Security Standards Enforcement**
   - Add namespace labels for `restricted` PSS level (currently `baseline`)
   - Fix init container to run as non-root
   - Validate all containers pass `kubectl apply --dry-run=server`

2. **Vulnerability Scanning Enhancement**
   - Add SBOM generation with Syft to CI/CD
   - Implement dependency tracking (SBOM → Grype scanning)
   - Add vulnerability remediation SLA (e.g., 30 days for HIGH)

3. **Deterministic Replay Support**
   - Leverage existing `wf_audit_log.erl` append-only receipts
   - Add XES trace export for each workflow execution
   - Implement replay function: `wf_engine:replay(WorkflowId, XESLog)`

4. **Security Monitoring Dashboards**
   - Create Cloud Monitoring dashboard for security metrics:
     - Failed authorization attempts (from wf_audit_log)
     - Network policy denials (from GKE logs)
     - Vulnerability scan results trend
     - Secret access (from Cloud Audit Logs)

### Phase 4: Documentation & Marketplace Submission (Week 7-8)

**Goal:** Complete Marketplace security review artifacts.

1. **Security Whitepaper**
   - Document all security controls in `docs/gcp/SECURITY_WHITEPAPER.md`
   - Include architecture diagrams, data flow, encryption details
   - Address common enterprise security questionnaires

2. **Compliance Matrix**
   - Map CRE controls to SOC 2, HIPAA, PCI-DSS, GDPR requirements
   - Create `docs/gcp/COMPLIANCE_MATRIX.md`
   - Document customer responsibilities (shared security model)

3. **Marketplace Security Artifacts**
   - Upload SBOM (SPDX format) to Marketplace submission
   - Include Trivy scan results (clean, no CRITICAL)
   - Provide cosign signature verification instructions
   - Complete `marketplace/SUBMISSION_CHECKLIST.md`

## Open Questions

1. **XES Log Retention Period**
   - What is the required retention for XES event logs?
   - Should XES logs be exported to BigQuery for long-term storage?
   - Who owns the cost of log storage (customer vs. CRE vendor)?

2. **CMEK Key Management**
   - Should CMEK be mandatory or optional for Marketplace deployment?
   - Who manages the KMS key ring (customer-provided vs. CRE-created)?
   - What is the key rotation policy (365 days recommended for CMEK)?

3. **Distroless Migration**
   - Is Alpine base sufficient for Marketplace approval, or is distroless required?
   - What is the effort to migrate `erlang:28-alpine` to distroless?
   - Can we justify Alpine with vulnerability scanning + SBOM?

4. **Audit Log Access Control**
   - Who should have access to `wf_audit_log` entries (Cloud IAM roles)?
   - Should audit logs be immutable (WORM storage) to prevent tampering?
   - How do we handle log access for forensic investigations?

5. **Network Policy Testing**
   - Have default-deny policies been tested in a realistic deployment?
   - Do health checks work correctly with network policies enabled?
   - Are there missing allow rules for GCP services (e.g., Cloud SQL proxy)?

6. **Image Signing Process**
   - Who holds the cosign private key (GitHub Actions Secrets, KMS?)
   - What is the key rotation policy for image signing keys?
   - How do we recover from a compromised signing key?

7. **Deterministic Replay Implementation**
   - Does the current `wf_audit_log` capture enough state for replay?
   - Are external side effects (e.g., HTTP calls) replay-safe?
   - Should replay be a separate service or integrated into `wf_engine`?

8. **Compliance Certification**
   - Will CRE undergo SOC 2 Type II audit, or rely on GCP compliance inheritance?
   - Is a HIPAA BAA required for healthcare customers (via Google Cloud)?
   - Who pays for third-party compliance audits (customer vs. CRE vendor)?

9. **Vulnerability Remediation SLA**
   - What is the SLA for fixing CRITICAL vulnerabilities (24 hours, 7 days)?
   - How do we handle vulnerabilities in transitive dependencies (Erlang/OTP packages)?
   - Can we automate dependency updates via Dependabot or Renovate?

10. **Secret Rotation Downtime**
    - Does Erlang cookie rotation require cluster downtime?
    - Can we implement rolling rotation without workflow interruption?
    - How do we handle cookie mismatch during rotation?

## Appendix: Security Configuration Reference

### Pod Security Standards Mapping

| Control | Baseline | Restricted | CRE Status | Gap |
|---------|----------|------------|------------|-----|
| runAsNonRoot | ✅ Required | ✅ Required | ✅ Implemented | None |
| runAsUser | Non-root | Non-root | ✅ 1000 | None |
| readOnlyRootFilesystem | Optional | ✅ Required | ❌ false | **Gap** |
| allowPrivilegeEscalation | false | ✅ false | ✅ false | None |
| capabilities.drop | ALL | ✅ ALL | ✅ ALL | None |
| seccompProfile | RuntimeDefault | ✅ RuntimeDefault | ✅ Pod-level | Container-level missing |

### Encryption Coverage

| Data Type | At Rest (Default) | CMEK Supported | TLS In Transit | Status |
|-----------|------------------|----------------|----------------|--------|
| Persistent Disk (PD) | ✅ Google-managed | ❌ No | N/A | **Gap** |
| Secret Manager | ✅ Google-managed | ⚠️ Optional (commented) | ✅ TLS | Document as optional |
| Cloud Logging | ✅ Google-managed | ✅ Yes | ✅ TLS | Not documented |
| Cloud Storage | ✅ Google-managed | ✅ Yes | ✅ TLS | Not documented |
| Erlang Distribution | N/A | N/A | ✅ TLS (optional) | Document TLS requirement |
| Pod-to-Pod Traffic | N/A | N/A | ✅ Encrypted (GKE) | Automatic |

### RBAC Permission Matrix

| Resource | Current Permissions | Minimum Required | Over-Provisioned |
|----------|---------------------|------------------|------------------|
| ConfigMaps | get, list, watch | get, list, watch | ✅ Appropriate |
| Secrets | get, list, watch | get | ❌ Remove list, watch |
| Pods | get, list, watch | get | ❌ Remove list, watch |
| Leases | get, create, update, delete | get, create, update, delete | ✅ Appropriate |
| EndpointSlices | get, list, watch | get, list | ⚠️ Watch maybe excessive |

**Action Required:** Narrow Secrets and Pods permissions to `get` only (unless list/watch is needed for discovery).

### Network Policy Coverage

| Traffic Direction | Policy Type | Status | Coverage |
|-------------------|-------------|--------|----------|
| Ingress (all pods) | Default-deny | ✅ Implemented | 100% |
| Egress (all pods) | Default-deny | ✅ Implemented | 100% |
| DNS (egress) | Explicit allow | ✅ Implemented | TCP/UDP 53 |
| CRE internal | Explicit allow | ✅ Implemented | EPMD 4369, dist ports |
| Health checks | Explicit allow | ✅ Implemented | From kubelet |
| Cloud Monitoring | Explicit allow | ✅ Implemented | 199.36.153.8/30:443 |
| Secret Manager | Explicit allow | ✅ Implemented | 0.0.0.0/0:443 (except private) |
| Pub/Sub | Explicit allow | ✅ Implemented | 0.0.0.0/0:443 (except private) |

**Assessment:** Comprehensive network coverage. No gaps identified.

### Audit Log Sources

| Log Source | Format | Storage | Retention | Export to Cloud Logging |
|------------|--------|---------|-----------|-------------------------|
| Workflow Receipts | Erlang disk_log | Local disk | Undefined | ❌ No |
| XES Event Logs | XML | Local disk | Undefined | ❌ No |
| GKE Audit Logs | Cloud Logging | Cloud Logging | 400 days (configurable) | ✅ Yes (automatic) |
| GKE System Logs | Cloud Logging | Cloud Logging | 30 days (default) | ✅ Yes (automatic) |
| Secret Manager Access | Cloud Audit Logs | Cloud Logging | 400 days | ✅ Yes (automatic) |

**Action Required:** Implement log export for workflow receipts and XES logs to BigQuery for compliance retention.

---

**Document Version:** 1.0
**Last Updated:** 2025-01-11
**Status:** Research Complete
