# CRE Compliance Matrix - GCP Marketplace Edition

**Version**: 1.0
**Last Updated**: 2025-01-11
**Document Owner**: CRE Security Team (security@common-runtime.org)

---

## Compliance Frameworks Supported

CRE provides controls mapped to the following regulatory frameworks:

- **SOC 2 Type II** (Service Organization Control 2)
- **HIPAA** (Health Insurance Portability and Accountability Act)
- **PCI-DSS** (Payment Card Industry Data Security Standard)
- **GDPR** (General Data Protection Regulation)
- **ISO 27001** (Information Security Management)

---

## SOC 2 Type II

### Trust Principles

#### Security

| Criteria | CRE Control | Evidence Location | Evidence Type |
|----------|-------------|-------------------|---------------|
| **Access Control** | | | |
| Logical and Physical Access Controls | Workload Identity Federation | `terraform/gcp/modules/security/iam.tf:140-144` | Terraform config |
| Least Privilege RBAC | Narrowed Kubernetes RBAC | `k8s/gcp/serviceaccount.yaml:43-55` | Kubernetes manifest |
| Network Access Controls | Default-deny network policies | `k8s/gcp/network-policy.yaml:6-131` | Kubernetes manifest |
| **Encryption** | | | |
| Data at Rest Encryption | CMEK for Persistent Disks | `terraform/gcp/modules/storage/main.tf:15` | Terraform config |
| Data in Transit Encryption | TLS 1.3 for external APIs | GKE default (automatic) | GKE configuration |
| **Change Management** | | | |
| Change Approval Process | Binary Authorization (signed images) | `terraform/gcp/modules/binary_authorization/main.tf` | Terraform config |
| Configuration Management | Infrastructure as Code (Terraform) | `terraform/gcp/` | Terraform modules |
| **Vulnerability Management** | | | |
| Vulnerability Scanning | Trivy CI/CD integration | `.github/workflows/release.yml:85-95` | GitHub Actions workflow |
| Patch Management | Signed image deployments | `.github/workflows/release.yml` | GitHub Actions workflow |
| **Monitoring** | | | |
| Security Monitoring | Cloud Monitoring alerts | `terraform/gcp/modules/monitoring/` | Terraform config |
| Audit Logging | wf_audit_log + Cloud Logging | `src/wf/wf_audit_log.erl:192-196` | Source code |

#### Availability

| Criteria | CRE Control | Evidence Location | Evidence Type |
|----------|-------------|-------------------|---------------|
| **High Availability** | | | |
| Multi-Zone Deployment | Regional GKE cluster (3 zones) | `terraform/gcp/modules/gke_cluster/main.tf:10-13` | Terraform config |
| Disaster Recovery | CMEK-encrypted backups | `terraform/gcp/modules/backup/main.tf:104` | Terraform config |
| **Performance Monitoring** | | | |
| Health Checks | GKE liveness/readiness probes | `k8s/gcp/deployment.yaml:195-220` | Kubernetes manifest |
| Metrics Collection | Cloud Monitoring integration | `terraform/gcp/modules/monitoring/` | Terraform config |
| **Incident Response** | | | |
| Incident Response Plan | Documented in Security Whitepaper | `docs/gcp/SECURITY_WHITEPAPER.md` | Documentation |
| escalation Procedures | On-call rotation and paging | [Customer-configured] | Operational procedure |

#### Processing Integrity

| Criteria | CRE Control | Evidence Location | Evidence Type |
|----------|-------------|-------------------|---------------|
| **Audit Trail** | | | |
| Append-Only Receipt Log | wf_audit_log (disk_log) | `src/wf/wf_audit_log.erl:192-196` | Source code |
| Centralized Logging | Cloud Logging export | `terraform/gcp/modules/audit_logging/main.tf` | Terraform config |
| **Data Integrity** | | | |
| XES Event Logs | Process mining standard | `src/xes/xes_serial.erl` | Source code |
| Deterministic Replay | Workflow replay support | `src/wf/wf_engine.erl` | Source code |

#### Confidentiality

| Criteria | CRE Control | Evidence Location | Evidence Type |
|----------|-------------|-------------------|---------------|
| **Data Encryption** | | | |
| Customer-Controlled Keys | CMEK for all data-at-rest | `terraform/gcp/modules/storage/main.tf` | Terraform config |
| Secret Management | Secret Manager integration | `terraform/gcp/modules/security/secrets.tf:16-51` | Terraform config |
| **Privacy** | | | |
| Data Minimization | No PII in workflow data by default | Architecture design | Architecture documentation |
| Access Logging | Secret Manager access logs | Cloud Audit Logs (automatic) | GCP logs |

### SOC 2 Audit Evidence

| Evidence Artifact | Location | Retention | Access |
|-------------------|----------|-----------|--------|
| Audit Log Entries | BigQuery `cre_audit_logs` | 400 days | IAM: `roles/bigquery.viewer` |
| Workflow Receipts | wf_audit_log + BigQuery | 400 days | IAM: `roles/bigquery.viewer` |
| Image Scan Results | GitHub Release Artifacts | Indefinite | Public (for transparency) |
| IAM Change Logs | Cloud Audit Logs | 400 days | IAM: `roles/iam.viewer` |
| Network Policy Logs | Cloud Logging | 30 days | Logging Viewer |
| Configuration Changes | Terraform state | Indefinite | Terraform admin |
| Incident Reports | Security ticketing system | 7 years | Security team |

---

## HIPAA

### HIPAA Security Rule

#### Administrative Safeguards

| Implementation Specification | CRE Control | Customer Action Required | Evidence |
|------------------------------|-------------|--------------------------|----------|
| **Security Management Process** | | | |
| Risk Analysis | PSS restricted enforcement | Review workflow data for PHI | `k8s/gcp/deployment.yaml:88-94` |
| Risk Management | Vulnerability scanning + patching | Review scan results quarterly | `.github/workflows/release.yml` |
| Sanction Policy | Employee security training | Document staff training | HR records |
| **Assigned Security Responsibility** | `roles/iam.securityReviewer` | Assign security contact | IAM policy |
| **Workforce Security** | RBAC least-privilege | Review access quarterly | `k8s/gcp/serviceaccount.yaml` |
| **Information Access Management** | Workload Identity | Revoke access for terminated staff | IAM policies |
| **Security Awareness and Training** | Security documentation | Train staff on HIPAA controls | `docs/gcp/SECURITY_WHITEPAPER.md` |

#### Physical Safeguards

| Implementation Specification | CRE Control | Customer Action Required | Evidence |
|------------------------------|-------------|--------------------------|----------|
| **Facility Access Controls** | GKE private cluster | Restrict network access | `terraform/gcp/modules/gke_cluster/main.tf:18-22` |
| **Workstation Use** | No local workstations (cloud-only) | N/A | N/A |
| **Workstation Security** | GKE shielded nodes | N/A | `terraform/gcp/modules/gke_cluster/main.tf:163-166` |

#### Technical Safeguards

| Implementation Specification | CRE Control | Customer Action Required | Evidence |
|------------------------------|-------------|--------------------------|----------|
| **Access Control** | | | |
| Unique User Identification | Workload Identity (per environment) | Use separate GCP projects for prod/dev | `terraform/gcp/modules/security/iam.tf` |
| Emergency Access Procedure | `roles/iam.serviceAccountTokenCreator` | Designate emergency accessors | IAM policy |
| Automatic Logoff | Short-lived OIDC tokens (auto-rotate) | N/A | GKE automatic |
| Encryption and Decryption | CMEK for PD + Secret Manager | Create KMS keys | `terraform/gcp/modules/storage/main.tf:15` |
| **Audit Controls** | wf_audit_log + Cloud Logging | Enable BigQuery export | `terraform/gcp/modules/audit_logging/main.tf` |
| **Integrity** | XES logs + append-only receipts | Verify log integrity | `src/wf/wf_audit_log.erl` |
| **Transmission Security** | TLS 1.3 + GKE pod-to-pod encryption | N/A | GKE automatic |

#### HIPAA BAA (Business Associate Agreement)

- **Status**: Available via Google Cloud (customer agreement)
- **How to Obtain**: https://cloud.google.com/hipaa-compliance
- **CRE Platform Responsibility**: Implement HIPAA controls (documented in this matrix)
- **Customer Responsibility**:
  - Sign BAA with Google Cloud
  - Enable CMEK for PHI workflows
  - Configure 6-year log retention (extend 400-day default)
  - Manage PHI access controls

---

## PCI-DSS

### PCI-DSS Requirements

| Requirement | CRE Control | Evidence Location | Customer Action |
|-------------|-------------|-------------------|-----------------|
| **Req 1: Firewall Configuration** | Network Policies (default-deny) | `k8s/gcp/network-policy.yaml:6-131` | Review allowed egress rules |
| **Req 2: Default Passwords** | No default passwords (Secret Manager) | `terraform/gcp/modules/security/secrets.tf:16-51` | Rotate secrets quarterly |
| **Req 3: Data Protection** | CMEK + TLS 1.3 | `terraform/gcp/modules/storage/main.tf:15` | Enable CMEK |
| **Req 4: Encryption** | TLS 1.3 (automatic) | GKE default | N/A |
| **Req 5: Anti-Virus** | Vulnerability scanning (Trivy) | `.github/workflows/release.yml:85-95` | Review scan results |
| **Req 6: Secure Development** | Binary Authorization | `terraform/gcp/modules/binary_authorization/main.tf` | Sign images |
| **Req 7: Access Control** | RBAC least-privilege | `k8s/gcp/serviceaccount.yaml:43-55` | Review permissions quarterly |
| **Req 8: Access Control** | Workload Identity | `terraform/gcp/modules/security/iam.tf:140-144` | Manage IAM policies |
| **Req 9: Physical Access** | GKE private cluster | `terraform/gcp/modules/gke_cluster/main.tf:18-22` | Restrict network access |
| **Req 10: Logging** | 400-day retention | `terraform/gcp/modules/audit_logging/main.tf:18` | N/A (exceeds 1-year requirement) |
| **Req 11: Vulnerability Testing** | Trivy + pen tests | `.github/workflows/release.yml` | Annual pen test |
| **Req 12: Policy** | Security whitepaper | `docs/gcp/SECURITY_WHITEPAPER.md` | Adopt policies |

### PCI-DSS Scoping

- **In Scope**:
  - CRE pods
  - Persistent Disks (with CMEK)
  - Secret Manager (with CMEK)
  - Workflow data (cardholder data if present)

- **Out of Scope**:
  - GKE control plane (Google responsibility)
  - GCP infrastructure (Google responsibility)
  - External APIs (cardholder data not stored by CRE platform)

---

## GDPR

| GDPR Article | CRE Control | Customer Action |
|--------------|-------------|-----------------|
| **Art. 25 (Data Protection by Design)** | PSS restricted, encryption, minimal data collection | Minimize PII in workflows |
| **Art. 32 (Security of Processing)** | Audit logging, access control, encryption | Review access logs monthly |
| **Art. 33 (Breach Notification)** | Cloud Monitoring alerts + wf_audit_log | Notify within 72 hours of breach |
| **Art. 35 (DPIA - Data Protection Impact Assessment)** | Data flow documentation | Conduct DPIA for high-risk workflows |
| **Art. 28 (Processor)** | CRE as data processor | Sign DPA with CRE vendor |
| **Art. 17 (Right to Erasure)** | Delete workflow data via API | Implement data deletion workflows |

---

## ISO 27001

| ISO 27001 Control | CRE Implementation | Evidence |
|-------------------|-------------------|----------|
| **A.9 Access Control** | RBAC + Workload Identity | `k8s/gcp/serviceaccount.yaml` |
| **A.10 Cryptography** | CMEK + TLS 1.3 | `terraform/gcp/modules/storage/main.tf` |
| **A.12 Operations Security** | Audit logging + monitoring | `terraform/gcp/modules/audit_logging/main.tf` |
| **A.14 System Acquisition** | Binary Authorization + vulnerability scanning | `.github/workflows/release.yml` |
| **A.15 Supplier Relationships** | Vendor security assessment (customer) | Procurement process |
| **A.16 Incident Management** | Incident response playbook | `docs/gcp/SECURITY_WHITEPAPER.md` |
| **A.18 Compliance** | Compliance controls mapped in this document | This matrix |

---

## Compliance Readiness Checklist

### SOC 2 Type II

- [ ] Review SOC 2 controls mapped in this matrix
- [ ] Conduct annual SOC 2 audit (or leverage GCP SOC 2 report)
- [ ] Review access logs quarterly
- [ ] Document incident response procedures
- [ ] Complete penetration testing (annual)
- [ ] Review vulnerability scan results (monthly)
- [ ] Verify least-privilege RBAC (quarterly)

### HIPAA

- [ ] Sign HIPAA BAA with Google Cloud: https://cloud.google.com/hipaa-compliance
- [ ] Enable CMEK for all PHI workflows
- [ ] Configure 6-year log retention (extend 400-day default in BigQuery)
- [ ] Review IAM access policies (quarterly)
- [ ] Conduct HIPAA training for staff with PHI access
- [ ] Implement HIPAA-specific incident response procedures
- [ ] Complete risk analysis for PHI workflows

### PCI-DSS

- [ ] Scope CRE deployment for cardholder data
- [ ] Enable CMEK for all cardholder data storage
- [ ] Review network policy allow rules (quarterly)
- [ ] Conduct annual penetration testing
- [ ] Review vulnerability scan results (monthly)
- [ ] Verify TLS 1.3 for all external connections
- [ ] Implement PCI-DSS incident response procedures
- [ ] Complete self-assessment questionnaire (SAQ)

### GDPR

- [ ] Conduct Data Protection Impact Assessment (DPIA) if needed
- [ ] Document data flows for EU personal data
- [ ] Implement breach notification procedures (72 hours)
- [ ] Configure data erasure workflows (Art. 17)
- [ ] Sign Data Processing Agreement (DPA) with CRE vendor
- [ ] Minimize PII in workflow definitions
- [ ] Implement consent management (if required)

### ISO 27001

- [ ] Review ISO 27001 controls mapped in this matrix
- [ ] Conduct annual ISO 27001 audit (or leverage GCP ISO 27001 certification)
- [ ] Document information security policies
- [ ] Implement risk assessment process
- [ ] Conduct internal audits (semi-annual)
- [ ] Review access controls (quarterly)
- [ ] Complete management review (annual)

---

## GCP Compliance Inheritance

CRE inherits compliance certifications from GCP infrastructure:

- **SOC 2 Type II**: Google Cloud SOC 2 report available
- **ISO 27001**: Google Cloud ISO 27001 certification
- **PCI-DSS**: Google Cloud PCI-DSS certification (infrastructure layer)

**Customer Benefit**: CRE does not require independent SOC 2 or ISO 27001 audits for infrastructure controls. Rely on Google's certifications and implement CRE-specific controls documented in this matrix.

**Third-Party Audits**: CRE platform does not undergo independent SOC 2 Type II audit. Customers leverage GCP's SOC 2 report for infrastructure compliance.

---

## Shared Responsibility Model

| Compliance Requirement | CRE Platform Responsibility | Customer Responsibility |
|------------------------|---------------------------|------------------------|
| **SOC 2 Audit** | Provide control evidence, documentation | Conduct annual audit (or use GCP report) |
| **HIPAA BAA** | Implement HIPAA controls | Sign BAA with Google Cloud |
| **PCI-DSS Scoping** | Document in-scope components | Complete SAQ, annual pen test |
| **GDPR DPIA** | Provide data flow documentation | Conduct DPIA for high-risk workflows |
| **Log Retention** | Provide 400-day retention | Extend to required period (e.g., 6 years for HIPAA) |
| **Access Reviews** | Document RBAC permissions | Review and approve access quarterly |
| **Incident Response** | Provide incident response playbook | Execute IR procedures, notify affected parties |
| **Vulnerability Management** | Scan images, publish results | Review and remediate in workflows |

---

## Evidence Artifacts

### Automated Evidence Collection

```bash
# Export CRE configuration for audit
kubectl get all -n cre-prod -o yaml > cre-k8s-config.yaml
kubectl get networkpolicies -n cre-prod -o yaml > cre-network-policies.yaml
kubectl get role,rolebinding,serviceaccount -n cre-prod -o yaml > cre-rbac.yaml

# Export Terraform state
terraform show -json > terraform-state.json

# Export audit logs from BigQuery
bq query --nouse_legacy_sql "SELECT * FROM \`PROJECT_ID.cre_audit_logs\`" > audit-logs.csv

# Verify image signatures
cosign verify us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:latest > image-verification.txt

# Scan for vulnerabilities
trivy image us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:latest > vulnerability-scan.txt
```

---

## References

### Compliance Frameworks

- [SOC 2](https://www.aicpa.org/soc4so) - AICPA Trust Services Criteria
- [HIPAA](https://www.hhs.gov/hipaa) - HHS HIPAA Home
- [PCI-DSS](https://www.pcisecuritystandards.org) - PCI Security Standards Council
- [GDPR](https://gdpr-info.eu) - GDPR text and resources
- [ISO 27001](https://www.iso.org/standard/27001) - ISO/IEC 27001:2013

### GCP Compliance

- [GCP Compliance](https://cloud.google.com/security/compliance) - GCP compliance offerings
- [GCP SOC 2 Report](https://cloud.google.com/security/compliance/soc2) - How to access Google's SOC 2 report
- [GCP HIPAA BAA](https://cloud.google.com/hipaa-compliance) - Sign HIPAA BAA with Google
- [GCP PCI-DSS](https://cloud.google.com/security/compliance/pci-dss) - PCI-DSS on GCP

### CRE Documentation

- [CRE Security Whitepaper](SECURITY_WHITEPAPER.md) - Comprehensive security overview
- [CRE Security Guide](SECURITY_GUIDE.md) - Security configuration instructions
- [CRE Architecture](ARCHITECTURE.md) - System architecture and data flows

---

## Change Log

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0 | 2025-01-11 | Initial compliance matrix for SOC 2, HIPAA, PCI-DSS, GDPR, ISO 27001 | CRE Security Team |

---

**Document Version**: 1.0
**Last Updated**: 2025-01-11
**Next Review**: 2025-04-11 (quarterly review)
**Approved By**: CRE Compliance Officer
**Questions**: compliance@common-runtime.org
