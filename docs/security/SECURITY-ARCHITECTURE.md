# GCP Security Architecture

## Executive Summary

This document defines the comprehensive security architecture for the CRE (Containerized Runtime Environment) platform, implementing defense-in-depth across network, identity, compute, and data layers.

## Threat Model

### Attack Surface Analysis

```
┌─────────────────────────────────────────────────┐
│          EXTERNAL THREATS (Internet)            │
├─────────────────────────────────────────────────┤
│  VPC Service Controls Perimeter                 │
│  ├─ Ingress Policy (API Gateway)                │
│  ├─- Egress Policy (External Services)          │
│  └─ Bridge Perimeters (Cross-Project)           │
├─────────────────────────────────────────────────┤
│          GKE CLUSTER SECURITY                   │
│  ├─ Binary Authorization (Image Validation)     │
│  ├─ Pod Security Standards (Restricted)         │
│  ├─ Network Policies (Zero Trust)               │
│  └─ Workload Identity (No Service Account Keys) │
├─────────────────────────────────────────────────┤
│          DATA PROTECTION                        │
│  ├─ Secret Manager (Encrypted Secrets)          │
│  ├─ Cloud KMS (Key Management)                  │
│  └─ Encryption at Rest (CMEK)                   │
└─────────────────────────────────────────────────┘
```

### Critical Threats

| Threat | Mitigation | Control |
|--------|-----------|---------|
| **Data Exfiltration** | VPC Service Controls egress policies | VPC-SC-001 |
| **Unauthorized Access** | Workload Identity + IAM least privilege | IAM-001 |
| **Malicious Images** | Binary Authorization with attestation | BINAUTH-001 |
| **Lateral Movement** | NetworkPolicy deny-all-by-default | NET-001 |
| **Privilege Escalation** | Pod Security Standards restricted profile | PSS-001 |
| **Credential Theft** | Secret Manager + automatic rotation | SEC-001 |

## Security Boundaries

### 1. VPC Service Controls Perimeter

**Purpose**: Prevent data exfiltration and unauthorized API access

**Implementation**:
- Perimeter around sensitive GCP services (GCS, BigQuery, Cloud SQL)
- Ingress rules for authorized clients only
- Egress rules with explicit allowlisting
- Bridge perimeters for cross-project communication

**Key Features**:
- Project-level isolation
- Service-level access control
- Context-aware access policies
- Audit logging of boundary violations

### 2. Binary Authorization

**Purpose**: Ensure only trusted container images run in GKE

**Implementation**:
- Attestation-based policy enforcement
- CI/CD pipeline integration for signing
- Exemption patterns for development
- Break-glass procedures for emergencies

**Key Features**:
- Cryptographic signature verification
- Multi-party attestation support
- Deployment-time enforcement
- Audit trail of all deployments

### 3. Workload Identity

**Purpose**: Enable GKE workloads to access GCP services without service account keys

**Implementation**:
- Kubernetes ServiceAccount to Google ServiceAccount binding
- Token exchange at runtime
- Automatic credential rotation
- Fine-grained IAM permissions

**Benefits**:
- No long-lived credentials in pods
- Automatic credential management
- Audit trail via Cloud Logging
- Integration with IAM Conditions

### 4. Secret Manager

**Purpose**: Centralized secret storage with automatic rotation

**Implementation**:
- External Secrets Operator for Kubernetes integration
- Secrets CSI Driver for volume mounting
- Automatic rotation with Cloud Functions
- Version management and rollback

**Key Features**:
- Encryption at rest with Cloud KMS
- Audit logging of all access
- IAM-based access control
- Regional replication for HA

### 5. Network Policies

**Purpose**: Zero-trust networking within Kubernetes

**Implementation**:
- Default deny-all policies per namespace
- Explicit allow rules for application traffic
- Egress controls to external services
- Namespace isolation

**Policy Strategy**:
- Start with deny-all
- Add specific allow rules
- Monitor and audit violations
- Progressive rollout with testing

### 6. Pod Security Standards

**Purpose**: Enforce secure pod configurations

**Implementation**:
- Pod Security Admission controller
- Restricted profile for production namespaces
- Baseline profile for staging
- Privileged profile for system namespaces only

**Restrictions**:
- No privileged containers
- No host namespaces (network, PID, IPC)
- No host path mounts
- Run as non-root
- Drop all capabilities
- Read-only root filesystem

## Security Controls Matrix

| Control ID | Name | Type | Priority | Status |
|-----------|------|------|----------|--------|
| VPC-SC-001 | VPC Service Controls | Network | Critical | Implementing |
| BINAUTH-001 | Binary Authorization | Compute | Critical | Implementing |
| WI-001 | Workload Identity | Identity | High | Implementing |
| SEC-001 | Secret Manager | Data | High | Implementing |
| NET-001 | Network Policies | Network | High | Implementing |
| PSS-001 | Pod Security Standards | Compute | High | Implementing |
| KMS-001 | Cloud KMS Encryption | Data | Medium | Planned |
| IAM-001 | Least Privilege IAM | Identity | Critical | Existing |

## Implementation Phases

### Phase 1: Foundation (Week 1)
- [ ] VPC Service Controls perimeter setup
- [ ] Binary Authorization policy configuration
- [ ] Workload Identity basic setup
- [ ] Security documentation

### Phase 2: Integration (Week 2)
- [ ] Secret Manager integration with External Secrets
- [ ] Network Policies for all namespaces
- [ ] Pod Security Standards enforcement
- [ ] CI/CD pipeline updates for Binary Authorization

### Phase 3: Hardening (Week 3)
- [ ] Cloud KMS integration for CMEK
- [ ] Advanced VPC-SC policies
- [ ] Security testing and validation
- [ ] Runbook and operational procedures

## Operational Security

### Monitoring and Alerting

**Security Events to Monitor**:
- VPC-SC boundary violations
- Binary Authorization policy violations
- Unauthorized Secret Manager access
- Network Policy denials
- Pod Security Policy violations

**Alert Channels**:
- Cloud Monitoring alerts
- Security Command Center findings
- Cloud Logging sinks to SIEM

### Incident Response

**Detection**: Cloud Logging + Security Command Center
**Analysis**: Log Explorer + forensics bucket
**Containment**: VPC-SC lockdown + pod termination
**Eradication**: Image scanning + policy updates
**Recovery**: Validated deployment rollback
**Lessons Learned**: Post-mortem documentation

### Compliance

**Standards Addressed**:
- CIS GCP Benchmark
- CIS Kubernetes Benchmark
- NIST Cybersecurity Framework
- SOC 2 Type II controls

## Security Testing

### Validation Tests

1. **VPC-SC**: Attempt unauthorized API access from external IPs
2. **Binary Authorization**: Deploy unsigned image (should fail)
3. **Workload Identity**: Verify no service account keys in pods
4. **Secret Manager**: Test secret rotation and rollback
5. **Network Policies**: Test pod-to-pod communication restrictions
6. **Pod Security**: Deploy privileged pod (should be rejected)

### Penetration Testing Scope

- External perimeter testing
- Internal lateral movement testing
- Privilege escalation attempts
- Data exfiltration scenarios
- Supply chain attack simulation

## Security Responsibilities

| Role | Responsibilities |
|------|------------------|
| **Security Architect** | Design, policy definition, threat modeling |
| **Platform Engineer** | Implementation, infrastructure as code |
| **SRE** | Monitoring, incident response, operations |
| **Developer** | Secure coding, secret management, compliance |
| **Auditor** | Compliance validation, security testing |

## References

- [GCP VPC Service Controls](https://cloud.google.com/vpc-service-controls)
- [Binary Authorization](https://cloud.google.com/binary-authorization)
- [Workload Identity](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)
- [Secret Manager](https://cloud.google.com/secret-manager)
- [Kubernetes Network Policies](https://kubernetes.io/docs/concepts/services-networking/network-policies/)
- [Pod Security Standards](https://kubernetes.io/docs/concepts/security/pod-security-standards/)

## Approval

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Security Architect | _______________ | _______________ | ________ |
| Platform Lead | _______________ | _______________ | ________ |
| CISO | _______________ | _______________ | ________ |

---

**Document Version**: 1.0
**Last Updated**: 2026-02-06
**Next Review**: 2026-05-06
