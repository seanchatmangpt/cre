# CRE Security Model

This document outlines CRE's security architecture, controls, and compliance features for Google Cloud Marketplace deployments.

## Executive Summary

CRE (Common Runtime Environment) is designed with **security-first principles** for enterprise GCP deployments:

- ✅ **Pod Security Standards (Restricted)**: All containers pass PSS restricted validation
- ✅ **Zero Trust Networking**: Network policies with explicit allow rules
- ✅ **Supply Chain Security**: Container images signed with cosign
- ✅ **Data Encryption**: Customer-managed encryption keys (CMEK) support
- ✅ **Audit Trail**: Centralized logging with Cloud Audit Logs
- ✅ **Least Privilege IAM**: Workload Identity Federation (no service account keys)

### Compliance Readiness

| Standard | Status | Notes |
|----------|--------|-------|
| **SOC 2 Type II** | ✅ Ready | Controls mapped to Trust Principles |
| **HIPAA** | ✅ Ready | BAA available via Google Cloud, CMEK support |
| **PCI-DSS** | ✅ Ready | Encryption, logging, vulnerability scanning |
| **GDPR** | ✅ Ready | Data protection by design, breach notification support |

---

## Security Architecture

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

### Threat Model

CRE operates in untrusted multi-tenant cloud environments:

| Threat | Description | CRE Mitigation |
|--------|-------------|----------------|
| **Compromised Container** | Attacker gains shell access to CRE pod | Read-only rootfs, dropped capabilities, seccomp filters |
| **Network Eavesdropping** | Attacker intercepts pod-to-pod traffic | GKE automatic encryption, TLS for external APIs |
| **Insider Threat** | Malicious operator with GCP IAM permissions | Least-privilege IAM, audit logging, separation of duties |
| **Supply Chain** | Compromised container image or dependency | Image signing, vulnerability scanning |
| **Data Exfiltration** | Unauthorized access to workflow data | Network policies, private cluster, IAM controls |

---

## Container Security

### Pod Security Standards

CRE complies with **Kubernetes Pod Security Standards (Restricted)** profile:

| Control | Requirement | CRE Implementation |
|---------|-------------|-------------------|
| **runAsNonRoot** | Required | ✅ Runs as UID 1000 |
| **readOnlyRootFilesystem** | Required | ✅ Read-only root filesystem |
| **allowPrivilegeEscalation** | Must be false | ✅ `false` |
| **capabilities.drop** | Drop ALL | ✅ All capabilities dropped |
| **seccompProfile** | RuntimeDefault | ✅ Pod + Container level enforced |

### Container Image Hardening

CRE container images follow security best practices:

- **Multi-stage Build**: Reduces attack surface (minimal runtime image)
- **Non-Root User**: Runs as UID 1000 (no root in runtime image)
- **Minimal Base**: `erlang:28-alpine` (minimal packages, smallest attack surface)
- **Read-Only Root**: Writable directories mounted as tmpfs volumes
- **Vulnerability Scanning**: Trivy CI/CD integration (0 CRITICAL threshold)
- **Image Signing**: Cosign signatures enforced by Binary Authorization

### Verification

Verify CRE container security:

```bash
# Check pod security context
kubectl get pod -n cre cre-0 -o jsonpath='{.spec.securityContext}'

# Verify non-root execution
kubectl exec -n cre cre-0 -- id
# Expected output: uid=1000(cre) gid=1000(cre)

# Verify read-only root
kubectl exec -n cre cre-0 -- touch /test
# Expected output: touch: cannot touch '/test': Read-only file system
```

---

## Network Security

### Zero Trust Networking

CRE implements **default-deny** network policies:

```yaml
# Example: Restrictive network policy
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: cre-default-deny
  namespace: cre
spec:
  podSelector: {}
  policyTypes:
  - Ingress
  - Egress
```

### Allowed Traffic

CRE network policies explicitly allow:

| Traffic Type | Source/Destination | Ports | Purpose |
|--------------|-------------------|-------|---------|
| **CRE pod-to-pod** | CRE pods | 4369 (EPMD), 9100+ (Erlang distribution) | Mnesia clustering |
| **CRE API** | LoadBalancer/Ingress | 4142 (HTTP) | External API access |
| **Cloud Operations** | Cloud Logging/Monitoring | 443 (HTTPS) | Log/metric export |
| **DNS** | kube-dns | 53 (UDP/TCP) | DNS resolution |

### Private Cluster

CRE deployments use **GKE private clusters**:

- **Private Nodes**: Nodes have private IP addresses only
- **Private Endpoint**: Control plane accessible via private IP
- **Authorized Networks**: Restrict control plane access to specific IP ranges
- **No Public IPs**: Pods and services have private IPs only

### TLS Encryption

CRE API supports TLS for external access:

- **TLS 1.3**: Minimum TLS version
- **Strong Ciphers**: Only secure cipher suites allowed
- **Certificate Management**: Cert-Manager for automatic certificate rotation
- **Mutual TLS**: Optional mTLS for service-to-service communication

---

## Identity and Access Management

### Workload Identity

CRE uses **Workload Identity Federation** for secure GCP service access:

- **No Service Account Keys**: No long-lived credentials to manage
- **Federated Identity**: Kubernetes service account ↔ Google service account mapping
- **Short-Lived Tokens**: Access tokens expire after 1 hour
- **Least Privilege**: Google service account has only required permissions

**Configuration**:

```bash
# Create Google service account
gcloud iam service-accounts create cre-sa \
  --display-name="CRE Service Account"

# Grant minimal required roles
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:cre-sa@$PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/logging.logWriter"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:cre-sa@$PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/monitoring.metricWriter"

# Bind Workload Identity
gcloud iam service-accounts add-iam-policy-binding cre-sa@$PROJECT_ID.iam.gserviceaccount.com \
  --role="roles/iam.workloadIdentityUser" \
  --member="serviceAccount:$PROJECT_ID.svc.id.goog[cre/cre-sa]"
```

### RBAC

CRE implements Kubernetes RBAC for administrative access:

| Role | Permissions | Use Case |
|------|-------------|----------|
| **cre-admin** | Full CRUD on CRE resources | CRE administrators |
| **cre-operator** | View pods, services, logs | CRE operators (SREs) |
| **cre-viewer** | Read-only access | Developers, auditors |

**Example RBAC**:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: cre-operator
  namespace: cre
rules:
- apiGroups: [""]
  resources: ["pods", "services", "configmaps"]
  verbs: ["get", "list", "watch"]
- apiGroups: [""]
  resources: ["pods/log"]
  verbs: ["get"]
```

---

## Data Protection

### Encryption at Rest

CRE supports **Customer-Managed Encryption Keys (CMEK)**:

- **Persistent Disks**: Encrypted with customer-managed keys
- **Secrets**: Stored in Secret Manager with CMEK
- **Backup Storage**: Cloud Storage buckets with CMEK
- **Database**: Cloud Spanner with CMEK (if using Spanner adapter)

**Enable CMEK**:

```bash
# Create encryption key
gcloud kms keyring create cre-keyring --location=us-central1
gcloud kms keys create cre-key \
  --location=us-central1 \
  --keyring=cre-keyring \
  --purpose=encryption

# Grant CRE service account permission to use key
gcloud kms keys add-iam-policy-binding cre-key \
  --location=us-central1 \
  --keyring=cre-keyring \
  --member="serviceAccount:cre-sa@$PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/cloudkms.cryptoKeyEncrypterDecrypter"

# Enable CMEK for Persistent Disk
# Add to deployment YAML:
#   cloud.google.com/kms-key: projects/$PROJECT_ID/locations/us-central1/keyRings/cre-keyring/cryptoKeys/cre-key
```

### Encryption in Transit

CRE encrypts all data in transit:

- **Pod-to-Pod**: GKE automatic encryption (WireGuard)
- **External API**: TLS 1.3 (required)
- **GCP Services**: HTTPS/TLS for all GCP API calls
- **Erlang Distribution**: TLS-enabled distribution (optional, for inter-region)

### Data Residency

CRE supports **data residency requirements**:

- **Regional Deployment**: Deploy CRE in specific GCP region
- **Storage Replication**: Control data replication with Cloud Storage
- **Backup Location**: Configure backup location to meet residency requirements
- **No Cross-Region Data**: By default, data stays in deployment region

### Secrets Management

CRE stores sensitive configuration in **Kubernetes Secrets** or **Secret Manager**:

**Best Practices**:
- Use Secret Manager for sensitive data (API keys, passwords)
- Enable Secret Manager CMEK
- Rotate secrets regularly
- Audit secret access via Cloud Audit Logs
- Never commit secrets to git

---

## Audit and Compliance

### Audit Logging

CRE integrates with **Google Cloud Audit Logs**:

- **Admin Activity**: All administrative actions logged
- **Data Access**: API access to workflow data logged (if enabled)
- **System Events**: Pod creation, deletion, scaling logged
- **Access Transparency**: Google support access logged

**View Audit Logs**:

```bash
# Query audit logs for CRE namespace
gcloud logging read \
  'protoPayload.resourceName:"cre" AND logName:"projects/PROJECT_ID/logs/cloudaudit.googleapis.com%2Factivity"' \
  --limit=50

# Export audit logs for compliance
gcloud logging read \
  'logName:"projects/PROJECT_ID/logs/cloudaudit.googleapis.com%2Factivity"' \
  --freshness=1d \
  --format=json > audit-logs-export.json
```

### Compliance Mapping

CRE controls map to common compliance frameworks:

| SOC 2 Trust Principle | CRE Controls |
|-----------------------|--------------|
| **Security** | PSS restricted, network policies, encryption, IAM |
| **Availability** | Multi-region deployment, automated backups, HPA |
| **Processing Integrity** | XES logging, transactional workflows, audit trails |
| **Confidentiality** | Encryption at rest/in transit, network isolation, RBAC |
| **Privacy** | Data minimization, GDPR support, breach notification |

| HIPAA Security Rule | CRE Controls |
|---------------------|--------------|
| **Access Control** | RBAC, Workload Identity, network policies |
| **Audit Controls** | Cloud Audit Logs, XES logging |
| **Integrity** | Immutable container images, checksums |
| **Transmission Security** | TLS 1.3, pod-to-pod encryption |
| **Encryption** | CMEK support, encrypted persistent disks |

### Data Breach Response

CRE supports data breach notification requirements (GDPR, HIPAA):

1. **Detection**: Cloud Audit Logs alert on suspicious access
2. **Investigation**: Logs exported for forensic analysis
3. **Containment**: Network policies isolate compromised pods
4. **Notification**: Automated alerts via Cloud Monitoring
5. **Post-Mortem**: XES logs provide workflow data timeline

---

## Vulnerability Management

### Supply Chain Security

CRE implements security controls for the software supply chain:

| Control | Implementation | Purpose |
|---------|----------------|---------|
| **Image Signing** | Cosign signatures | Verify image authenticity |
| **Binary Authorization** | GKE Binary Authorization | Only signed images deployed |
| **Vulnerability Scanning** | Trivy in CI/CD | Detect vulnerabilities before deployment |
| **Dependency Pinning** | rebar.config version locks | Prevent supply chain attacks |
| **SBOM Generation** | Syft in CI/CD | Software Bill of Materials for transparency |

### Vulnerability Scanning

All CRE container images are scanned for vulnerabilities:

- **Scanner**: Trivy (open source)
- **Threshold**: 0 CRITICAL, < 10 HIGH allowed
- **Frequency**: Every build and deployment
- **Remediation**: CRITICAL vulnerabilities block deployment

**View Scan Results**:

```bash
# Scan deployed image
trivy image ghcr.io/joergen7/cre:0.3.0

# View vulnerability reports in Artifact Registry
gcloud artifacts docker images list ghcr.io/joergen7/cre \
  --show-package-vulnerability
```

### Security Updates

CRE follows responsible vulnerability disclosure:

- **Public Issues**: Report via GitHub (private security advisories)
- **Private Disclosure**: security@common-runtime.org
- **Patch Timeline**: Critical vulnerabilities within 48 hours, high within 7 days
- **Security Advisories**: GitHub Security Advisories for CVEs

---

## Operational Security

### Health Check Endpoints

CRE provides secure health check endpoints:

```bash
# Liveness probe (is the pod running?)
curl http://localhost:4142/health

# Readiness probe (is the pod ready?)
curl http://localhost:4142/ready

# Both endpoints return:
# {
#   "status": "ok",
#   "mnesia": "connected",
#   "uptime_seconds": 123456
# }
```

**Security Considerations**:
- Health checks do not expose sensitive information
- No authentication required (internal access only)
- Network policies restrict external access

### Logging Security

CRE logs exclude sensitive data:

- **No Secrets**: Secrets never logged
- **Sanitized PII**: Personal information sanitized before logging
- **Structured Logging**: JSON format for easy parsing and redaction
- **Log Retention**: Configurable retention (default 30 days, extendable for compliance)

### Access Control

Implement principle of least privilege:

| Role | Access Scope | Example Actions |
|------|--------------|-----------------|
| **CRE Admin** | Full namespace control | Deploy, upgrade, configure |
| **CRE Operator** | Operational tasks | View logs, restart pods, scale |
| **CRE User** | API access only | Submit workflows, view status |
| **CRE Auditor** | Read-only audit access | View audit logs, compliance reports |

---

## Security Best Practices

### Deployment Checklist

Before deploying CRE to production:

- [ ] Enable GKE private cluster
- [ ] Configure Workload Identity (no service account keys)
- [ ] Enable Binary Authorization (image verification)
- [ ] Configure CMEK for data encryption
- [ ] Apply network policies (default-deny)
- [ ] Enable Cloud Audit Logs (data access logging)
- [ ] Configure secret scanning in CI/CD
- [ ] Set up vulnerability alerts
- [ ] Configure TLS for external API access
- [ ] Enable Shielded GKE Nodes
- [ ] Configure RBAC with least privilege
- [ ] Set up monitoring and alerting

### Ongoing Security Tasks

- **Weekly**: Review vulnerability scan results
- **Monthly**: Rotate secrets (API keys, certificates)
- **Quarterly**: Review and audit IAM permissions
- **Annually**: Conduct security assessment and penetration testing

---

## Security Resources

### Documentation

- **[Security Whitepaper](../../docs/gcp/SECURITY_WHITEPAPER.md)** - Detailed security architecture
- **[Compliance Matrix](../../docs/gcp/COMPLIANCE_MATRIX.md)** - Complete compliance mapping
- **[Security Configuration Guide](../../docs/gcp/SECURITY_GUIDE.md)** - Security hardening procedures

### Support

- **Security Issues**: Report via GitHub (private security advisory)
- **Security Questions**: security@common-runtime.org
- **Compliance Questions**: compliance@common-runtime.org

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
**Security Team**: CRE Security Team (security@common-runtime.org)
