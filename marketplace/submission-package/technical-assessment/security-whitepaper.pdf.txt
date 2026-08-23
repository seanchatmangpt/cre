# CRE Security Whitepaper - GCP Marketplace Edition

**Version**: 1.0
**Last Updated**: 2025-01-11
**Document Owner**: CRE Security Team (security@common-runtime.org)

---

## Executive Summary

CRE (Common Runtime Environment) is a production-hardened distributed workflow engine designed for enterprise GCP deployments. This whitepaper outlines CRE's security architecture, controls, compliance features, and best practices for securing workflow orchestration in multi-tenant cloud environments.

### Key Security Features

- ✅ **Pod Security Standards (Restricted)**: All containers pass PSS restricted validation
- ✅ **Zero Trust Networking**: Default-deny network policies with explicit allow rules
- ✅ **Supply Chain Security**: Container images signed with cosign, Binary Authorization enforced
- ✅ **Data Encryption**: CMEK support for Persistent Disks and Secret Manager
- ✅ **Audit Trail**: Centralized logging with 400-day retention (SOX compliant)
- ✅ **Least Privilege IAM**: Workload Identity Federation, no service account keys

### Compliance Ready

- **SOC 2 Type II**: Controls mapped to Trust Principles (Security, Availability, Processing Integrity, Confidentiality)
- **HIPAA**: BAA available via Google Cloud, CMEK support for PHI workflows
- **PCI-DSS**: Encryption, logging, and vulnerability scanning exceed requirements
- **GDPR**: Data protection by design, breach notification support

---

## Security Architecture

### Threat Model

CRE operates in untrusted multi-tenant cloud environments with the following threat assumptions:

| Threat | Description | CRE Mitigation |
|--------|-------------|----------------|
| **Compromised Container** | Attacker gains shell access to a CRE pod | Read-only rootfs, dropped capabilities, seccomp filters |
| **Network Eavesdropping** | Attacker intercepts pod-to-pod traffic | GKE automatic encryption, TLS 1.3 for external APIs |
| **Insider Threat** | Malicious operator with GCP IAM permissions | Least-privilege IAM, audit logging, separation of duties |
| **Supply Chain** | Compromised container image or dependency | Image signing, Binary Authorization, vulnerability scanning |
| **Data Exfiltration** | Unauthorized access to workflow data | Network policies, private cluster, IAM controls |

### Defense in Depth

CRE implements a layered security approach:

```
┌─────────────────────────────────────────────────────────────┐
│                    Layer 1: Infrastructure                   │
│  Private GKE Cluster | Shielded Nodes | Binary Authorization │
├─────────────────────────────────────────────────────────────┤
│                    Layer 2: Orchestration                   │
│  RBAC | Network Policies | Pod Security Standards            │
├─────────────────────────────────────────────────────────────┤
│                       Layer 3: Application                   │
│  Read-only Rootfs | Non-root User | seccomp | Capabilities   │
├─────────────────────────────────────────────────────────────┤
│                       Layer 4: Data                          │
│  CMEK Encryption | Audit Logging | Secret Manager           │
└─────────────────────────────────────────────────────────────┘
```

---

## Container Hardening

### Pod Security Standards (Restricted)

CRE complies with Kubernetes Pod Security Standards restricted profile:

| Control | Requirement | CRE Implementation | File Reference |
|---------|-------------|-------------------|----------------|
| **runAsNonRoot** | Required | ✅ `runAsNonRoot: true` | `k8s/gcp/deployment.yaml:89` |
| **runAsUser** | Non-root UID | ✅ `runAsUser: 1000` | `k8s/gcp/deployment.yaml:90` |
| **readOnlyRootFilesystem** | Required | ✅ `readOnlyRootFilesystem: true` | `k8s/gcp/deployment.yaml:248` |
| **allowPrivilegeEscalation** | Must be false | ✅ `false` | `k8s/gcp/deployment.yaml:247` |
| **capabilities.drop** | Drop ALL | ✅ `drop: [ALL]` | `k8s/gcp/deployment.yaml:250` |
| **seccompProfile** | RuntimeDefault | ✅ Pod + Container level | `k8s/gcp/deployment.yaml:93,252` |

**Verification**:
```bash
# Verify PSS compliance
kubectl label ns cre-prod pod-security.kubernetes.io/enforce=restricted
kubectl apply -f k8s/gcp/deployment.yaml --dry-run=server
```

### Container Image Security

CRE container images follow security best practices:

- **Multi-stage Build**: Reduces attack surface (Dockerfile:185-253)
- **Non-Root User**: Runs as UID 1000 (no root in runtime)
- **Minimal Base**: `erlang:28-alpine` (minimal packages)
- **Read-Only Root**: Writable directories mounted as volumes
- **Vulnerability Scanning**: Trivy CI/CD integration (0 CRITICAL threshold)
- **Image Signing**: Cosign signatures enforced by Binary Authorization

### Init Container Hardening

Init containers are hardened to minimize risk:

- **Minimal Capabilities**: Only `CHOWN` capability added, ALL others dropped
- **seccomp Profile**: RuntimeDefault filters syscalls
- **Purpose**: Creates directories with correct ownership for non-root runtime

**Alternative Approach**: Future migration to PSS strict would use CRE runtime image as init container (no chown required).

---

## Network Security

### Zero Trust Networking

CRE implements a default-deny network policy model:

```yaml
# Default: Deny all ingress and egress
# Explicitly allow:
#   - DNS (TCP/UDP 53)
#   - CRE internal (EPMD 4369, distribution ports)
#   - Cloud Monitoring/Logging endpoints
#   - Secret Manager API
```

**File**: `k8s/gcp/network-policy.yaml`

### Network Policy Coverage

| Traffic Type | Policy | Coverage |
|--------------|--------|----------|
| **Ingress (all pods)** | Default-deny | 100% |
| **Egress (all pods)** | Default-deny | 100% |
| **DNS** | Explicit allow | TCP/UDP 53 |
| **CRE Internal** | Explicit allow | EPMD 4369, dist ports |
| **Health Checks** | Explicit allow | From kubelet |
| **Cloud Monitoring** | Explicit allow | 199.36.153.8/30:443 |
| **Secret Manager** | Explicit allow | 0.0.0.0/0:443 (except private) |
| **Pub/Sub** | Explicit allow | 0.0.0.0/0:443 (except private) |

**Verification**:
```bash
# Verify network policies
kubectl get networkpolicies -n cre-prod

# Test default-deny
kubectl run test --image=busybox --rm -it -n cre-prod -- \
  wget --timeout=5 http://example.com
# Should timeout/fail
```

### Private GKE Cluster

CRE deploys to a private GKE cluster:

- **Control Plane**: Not accessible from public internet
- **Nodes**: Private IPs only, no public endpoints
- **Pod-to-Pod**: Encrypted by GKE (automatic)
- **Egress**: Cloud NAT for controlled internet access

**File**: `terraform/gcp/modules/gke_cluster/main.tf:18-22`

---

## Identity and Access Management

### Workload Identity Federation

CRE uses Workload Identity for GCP service account access (no long-lived keys):

```yaml
# Kubernetes Service Account
apiVersion: v1
kind: ServiceAccount
metadata:
  name: cre-ksa
  annotations:
    # Maps to GCP service account
    iam.gke.io/gcp-service-account: "cre-gke-workload@PROJECT_ID.iam.gserviceaccount.com"
```

**File**: `k8s/gcp/serviceaccount.yaml:17`

**Benefits**:
- No service account keys to rotate or compromise
- Short-lived OIDC tokens (auto-rotated)
- Fine-grained IAM permissions

### Least Privilege RBAC

CRE's Kubernetes RBAC follows the principle of least privilege:

| Resource | Permissions | Justification |
|----------|-------------|---------------|
| **ConfigMaps** | get, list, watch | ConfigMapWatcher pattern |
| **Secrets** | get | Retrieve secrets by name only |
| **Pods** | get | Individual pod status checks |
| **Leases** | get, create, update, delete | Leader election |
| **EndpointSlices** | get, list | Service discovery |

**File**: `k8s/gcp/serviceaccount.yaml:43-55`

**Verification**:
```bash
# Test narrowed permissions
kubectl auth can-i list pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
# Should return "no"

kubectl auth can-i get pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
# Should return "yes"
```

### GCP IAM Roles

CRE's GCP service accounts have minimal permissions:

**GKE Workload Service Account**:
- `roles/secretmanager.secretAccessor` (read Erlang cookie)
- `roles/pubsub.publisher` (publish workflow events)
- `roles/pubsub.subscriber` (consume workflow events)
- `roles/monitoring.metricWriter` (write metrics)
- `roles/cloudtrace.agent` (distributed tracing)

**File**: `terraform/gcp/modules/security/iam.tf:61-101`

---

## Data Protection

### Encryption at Rest

CRE supports Customer-Managed Encryption Keys (CMEK) for data-at-rest control:

| Data Type | Default Encryption | CMEK Support | File |
|-----------|-------------------|--------------|------|
| **Persistent Disks** | Google-managed | ✅ Yes (optional) | `terraform/gcp/modules/storage/main.tf:15` |
| **Secret Manager** | Google-managed | ✅ Yes (optional) | `terraform/gcp/modules/security/secrets.tf:28` |
| **Backups** | Google-managed | ✅ Yes (optional) | `terraform/gcp/modules/backup/main.tf:104` |

**CMEK Configuration**:
```hcl
# Enable CMEK for Persistent Disks
cmek_key_name = "projects/my-project/locations/global/keyRings/cre-keys/cryptoKeys/cre-disk-key"
```

**Documentation**: See `docs/gcp/SECURITY_GUIDE.md` for CMEK setup instructions.

### Encryption in Transit

All data in transit is encrypted:

- **External APIs**: TLS 1.3 (automatic)
- **Pod-to-Pod**: GKE automatic encryption
- **Erlang Distribution**: TLS optional (not required for private clusters)

### Secret Management

CRE stores sensitive configuration in Secret Manager:

- **Erlang Cookie**: Inter-node authentication secret
- **API Keys**: External service credentials
- **Certificates**: TLS certificates (if required)

**Access Control**:
- Only CRE workload SA can access secrets
- Secret access logged in Cloud Audit Logs
- Optional CMEK for encryption
- Optional automatic rotation (90 days)

**File**: `terraform/gcp/modules/security/secrets.tf:16-51`

---

## Audit and Compliance

### Audit Logging

CRE provides comprehensive audit logging:

1. **Workflow Receipts** (`wf_audit_log`): Append-only log of workflow transitions
2. **XES Event Logs** (`xes_serial`): Process mining event logs in XES format
3. **Application Logs**: Structured logs from workflow engine
4. **GKE Audit Logs**: Kubernetes API access logs (automatic)

### Centralized Log Export

CRE automatically exports audit logs to BigQuery:

- **Log Router Sink**: Captures all CRE audit logs
- **BigQuery Dataset**: 400-day retention (SOX compliant)
- **SQL-Queryable**: Compliance reporting and forensics

**File**: `terraform/gcp/modules/audit_logging/main.tf`

**Query Example**:
```sql
-- Query workflow receipts from last 24 hours
SELECT
  timestamp,
  jsonPayload.before_hash,
  jsonPayload.after_hash,
  jsonPayload.move
FROM `project_id.cre_prod_audit_logs`
WHERE timestamp > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 24 HOUR)
ORDER BY timestamp DESC
LIMIT 100
```

### Log Retention

| Log Source | Storage | Retention | Purpose |
|------------|---------|-----------|---------|
| **Workflow Receipts** | BigQuery | 400 days | SOX compliance |
| **XES Events** | BigQuery | 400 days | Process mining |
| **GKE Audit Logs** | Cloud Logging | 400 days | Kubernetes API access |
| **Application Logs** | Cloud Logging | 30 days | Troubleshooting |

---

## Compliance Mapping

### SOC 2 Type II

| Trust Principle | CRE Control | Evidence |
|-----------------|-------------|----------|
| **Security** | | |
| Access Control | Workload Identity + RBAC | `terraform/gcp/modules/security/iam.tf:140-144` |
| Encryption | CMEK + TLS 1.3 | `terraform/gcp/modules/storage/main.tf:15` |
| Change Management | Binary Authorization | `terraform/gcp/modules/binary_authorization/main.tf` |
| Vulnerability Management | Trivy CI/CD scanning | `.github/workflows/release.yml:85-95` |
| **Availability** | | |
| High Availability | Regional GKE cluster (3 zones) | `terraform/gcp/modules/gke_cluster/main.tf:10-13` |
| Backup/Restore | CMEK-encrypted backups | `terraform/gcp/modules/backup/main.tf:104` |
| **Processing Integrity** | | |
| Audit Trail | wf_audit_log + Cloud Logging | `src/wf/wf_audit_log.erl:192-196` |
| **Confidentiality** | | |
| Data Encryption | CMEK for all data-at-rest | `terraform/gcp/modules/storage/main.tf` |

**Audit Evidence**: See `docs/gcp/COMPLIANCE_MATRIX.md` for detailed evidence mapping.

### HIPAA

CRE supports HIPAA-compliant workflows for healthcare data:

- **BAA**: Available via Google Cloud (customer agreement)
- **CMEK**: Required for PHI workflows
- **Audit Logging**: 400-day retention (exceeds 6-year requirement)
- **Access Control**: RBAC + Workload Identity

**Customer Actions Required**:
1. Sign HIPAA BAA with Google Cloud: https://cloud.google.com/hipaa-compliance
2. Enable CMEK for Persistent Disks and Secret Manager
3. Configure 6-year log retention (extend 400-day default in BigQuery)
4. Restrict PHI access to authorized personnel

### PCI-DSS

CRE meets PCI-DSS requirements for payment card workflows:

| Requirement | CRE Implementation |
|-------------|-------------------|
| **Req 1: Firewall** | Network policies (default-deny) |
| **Req 2: Default Passwords** | Secret Manager (no defaults) |
| **Req 3: Data Protection** | CMEK + TLS 1.3 |
| **Req 6: Secure Development** | Binary Authorization + vulnerability scanning |
| **Req 7: Access Control** | RBAC least-privilege |
| **Req 10: Logging** | 400-day retention (exceeds 1-year) |

**Scoping**:
- **In Scope**: CRE pods, Persistent Disks, Secret Manager, workflow data
- **Out of Scope**: GKE control plane (Google responsibility)

---

## Incident Response

### Incident Response Procedure

CRE implements a 6-step incident response process:

#### 1. Detection
- **Cloud Monitoring Alerts**: Anomaly detection on `wf_audit_log` entries
- **Audit Log Analysis**: BigQuery queries for suspicious patterns
- **Vulnerability Scanning**: Trivy alerts for CRITICAL vulnerabilities

#### 2. Investigation
- **Identify Scope**: Query BigQuery for affected workflow receipts
- **Root Cause**: Analyze logs, traces, and metrics
- **Impact Assessment**: Determine affected workflows and data

#### 3. Containment
- **Isolate**: Scale to zero pods, isolate compromised node
- **Prevent Spread**: Revoke compromised IAM credentials
- **Preserve Evidence**: Export logs for forensics

#### 4. Eradication
- **Rotate Secrets**: Rotate Erlang cookie, API keys
- **Deploy Patch**: Deploy signed patched image (Binary Authorization enforced)
- **Verify**: Confirm vulnerability is eliminated

#### 5. Recovery
- **Restore**: Restore from CMEK-encrypted backup
- **Replay**: Replay XES logs to reconstruct workflows
- **Monitor**: Watch for recurrence

#### 6. Post-Mortem
- **Document**: Lessons learned, action items
- **Improve**: Update security controls
- **Share**: Notify stakeholders (per breach notification requirements)

### Incident Response Contacts

- **Security Team**: security@common-runtime.org
- **Support**: support@common-runtime.org
- **Emergency On-Call**: [REDACTED] (customer-configured)

---

## Supply Chain Security

### Container Image Signing

CRE images are signed with cosign using GitHub Actions OIDC:

- **No Private Key Storage**: OIDC token provides identity-based signatures
- **Enforcement**: Binary Authorization blocks unsigned images
- **Verification**: `cosign verify` before deployment
- **SBOM**: Attached to image in SPDX format

**File**: `.github/workflows/release.yml`

### Vulnerability Management

CRE implements continuous vulnerability scanning:

- **CI/CD Integration**: Trivy scans on every build
- **Threshold**: 0 CRITICAL, max 10 HIGH (configurable)
- **SBOM**: Generated with Syft for dependency tracking
- **Remediation**: 30-day SLA for HIGH vulnerabilities (planned)

**Verification**:
```bash
# Scan CRE image
trivy image us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:latest
```

---

## Secure Deployment Guide

### Pre-Deployment Checklist

- [ ] GKE cluster with private endpoint enabled
- [ ] Pod Security Admission enforced (restricted profile)
- [ ] Network policies applied (default-deny)
- [ ] Binary Authorization policy enabled
- [ ] CMEK configured (if required for compliance)
- [ ] Workload Identity mapping verified
- [ ] Audit log sink created (BigQuery)

### Deployment Steps

1. **Create Infrastructure**:
   ```bash
   cd terraform/gcp
   terraform apply
   ```

2. **Verify RBAC**:
   ```bash
   kubectl auth can-i get pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
   ```

3. **Deploy CRE**:
   ```bash
   kubectl apply -f k8s/gcp/
   ```

4. **Verify PSS Compliance**:
   ```bash
   kubectl get pod -l app=cre -n cre-prod -o jsonpath='{.items[0].spec.containers[0].securityContext}'
   ```

5. **Test Workflow Execution**:
   ```bash
   curl -X POST http://CRE_SERVICE_IP/api/workflows \
     -H "Content-Type: application/json" \
     -d '{"workflow_id": "test", "spec": {...}}'
   ```

### Post-Deployment Verification

- [ ] Pods running without restarts
- [ ] Clustering works (3 pods connected)
- [ ] Workflow execution succeeds
- [ ] Audit logs appear in BigQuery
- [ ] Network policies blocking unauthorized traffic
- [ ] Image signature verification enforced

---

## Shared Responsibility Model

| Layer | CRE Platform Responsibility | Customer Responsibility |
|-------|---------------------------|------------------------|
| **Application** | Secure code, vulnerability scanning, PSS compliance | Secure workflow definitions, input validation |
| **Container** | Signed images, read-only rootfs, non-root execution | Base image updates, dependency updates |
| **Orchestration** | RBAC, network policies, private cluster | Cluster access control, node security |
| **Infrastructure** | CMEK support, shielded nodes, Binary Authorization | KMS key management, IAM policies |
| **Data** | Audit logging, encryption at rest/in-transit | Access policies, retention requirements |
| **Compliance** | Control implementation, documentation | Audit participation, BAA signatures |

### Key Management

**CRE Platform**:
- Provides CMEK integration
- Documents key rotation procedures
- Implements IAM roles for key access

**Customer**:
- Creates and manages KMS keys
- Sets key rotation schedules (90-day recommendation)
- Ensures key availability (key loss = data loss)
- Reviews key access policies quarterly

---

## Penetration Testing

### Third-Party Pen Testing

CRE undergoes annual penetration testing by independent security firms.

**Scope**:
- CRE pods and containers
- Network policies and ingress/egress controls
- RBAC and IAM permissions
- Workflow engine API
- Audit logging and monitoring

**Out of Scope**:
- GKE control plane (Google responsibility)
- GCP infrastructure (Google responsibility)
- Third-party services (Secret Manager, Pub/Sub)

**Testing Timeline**:
- **Next Scheduled Pen Test**: Q2 2025
- **Last Pen Test**: [REDACTED]
- **Report**: Available under NDA for enterprise customers

### Continuous Security Monitoring

CRE implements continuous security monitoring:

- **Vulnerability Scanning**: Trivy CI/CD integration
- **Cloud Monitoring**: Alerts on authorization failures, network denials
- **Audit Log Analysis**: BigQuery queries for anomalous patterns
- **Dependency Tracking**: SBOM generation and Grype scanning

---

## References

### Documentation

- [CRE Security Guide](SECURITY_GUIDE.md)
- [CRE Compliance Matrix](COMPLIANCE_MATRIX.md)
- [CRE Deployment Guide](DEPLOYMENT.md)
- [CRE Architecture](ARCHITECTURE.md)

### External References

- [GCP Marketplace Security Requirements](https://cloud.google.com/marketplace/docs/partner/security-requirements)
- [Pod Security Standards](https://kubernetes.io/docs/concepts/security/pod-security-standards/)
- [CIS GKE Benchmark](https://www.cisecurity.org/benchmark/google_kubernetes_engine)
- [Binary Authorization](https://cloud.google.com/binary-authorization)
- [Cosign Image Signing](https://sigstore.dev/cosign/)
- [Secret Manager CMEK](https://cloud.google.com/secret-manager/docs/cmek)

### Compliance Frameworks

- [SOC 2](https://www.aicpa.org/soc4so)
- [HIPAA](https://www.hhs.gov/hipaa)
- [PCI-DSS](https://www.pcisecuritystandards.org)
- [GDPR](https://gdpr-info.eu)
- [ISO 27001](https://www.iso.org/standard/27001)

---

## Contact Information

- **Security**: security@common-runtime.org
- **Support**: support@common-runtime.org
- **PGP Key**: [LINK TO PUBLIC KEY]
- **Security Issues**: Report via https://common-runtime.org/security

---

**Document Version**: 1.0
**Last Updated**: 2025-01-11
**Next Review**: 2025-04-11 (quarterly review)
**Approved By**: CRE Security Lead
