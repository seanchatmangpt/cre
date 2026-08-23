# CRE Security Guide - GCP Deployment

## Overview

This guide explains how to configure and manage security features for CRE deployments on Google Cloud Platform (GCP). It covers Customer-Managed Encryption Keys (CMEK), secret rotation, network security, and compliance best practices.

## Table of Contents

1. [Customer-Managed Encryption Keys (CMEK)](#cmek)
2. [Secret Rotation](#secret-rotation)
3. [Network Security](#network-security)
4. [IAM and Access Control](#iam)
5. [Compliance](#compliance)
6. [Shared Responsibility Model](#shared-responsibility)

---

## Customer-Managed Encryption Keys (CMEK) <a name="cmek"></a>

CRE supports CMEK for both Persistent Disks and Secret Manager, allowing you to control encryption keys for regulatory compliance.

### What is CMEK?

CMEK allows you to create and manage encryption keys in Cloud KMS, giving you control over:
- Key access and permissions
- Key rotation schedules
- Key destruction (data becomes inaccessible)

**Important**: If you disable or destroy a CMEK key, all data encrypted with that key becomes permanently inaccessible. CRE cannot recover your data without the key.

### Persistent Disk CMEK

#### Step 1: Create a KMS Key Ring and Key

```bash
# Set your project ID
export PROJECT_ID="your-project-id"

# Create a key ring for CRE encryption keys
gcloud kms keyrings create cre-keys \
  --location global \
  --project ${PROJECT_ID}

# Create a KMS key for persistent disk encryption
gcloud kms keys create cre-disk-key \
  --location global \
  --keyring cre-keys \
  --purpose disk-encryption \
  --rotation-period 7776000s \
  --next-rotation-time $(date -d '+90 days' +%Y-%m-%d) \
  --project ${PROJECT_ID}
```

#### Step 2: Grant GKE Service Account Access to CMEK Key

The GKE node service account needs permission to use the CMEK key:

```bash
# Get the GKE node service account email
export GKE_NODE_SA=$(gcloud container clusters describe cre-cluster \
  --region us-central1 \
  --format value(nodeConfig.serviceAccount)

# Grant the GKE node SA permission to use the CMEK key
gcloud kms keys add-iam-policy-binding cre-disk-key \
  --location global \
  --keyring cre-keys \
  --member serviceAccount:${GKE_NODE_SA} \
  --role roles/cloudkms.cryptoKeyEncrypterDecrypter \
  --project ${PROJECT_ID}
```

#### Step 3: Enable CMEK in Terraform

Add the CMEK key to your Terraform configuration:

```hcl
# In your main Terraform configuration or variables file
module "storage" {
  source = "./modules/storage"

  project_id = var.project_id
  region     = var.region

  # Enable CMEK for persistent disks
  cmek_key_name = "projects/${var.project_id}/locations/global/keyRings/cre-keys/cryptoKeys/cre-disk-key"
}
```

#### Step 4: Apply and Verify

```bash
# Apply Terraform changes
terraform apply

# Verify the StorageClass includes CMEK
kubectl get storageclass cre-ssd -o yaml | grep encryptionKeyKMSKey

# Create a test PV and verify encryption
kubectl apply -f - <<EOF
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: test-cmek-pvc
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: cre-ssd
  resources:
    requests:
      storage: 1Gi
EOF

# Check the underlying disk for CMEK encryption
kubectl get pvc test-cmek-pvc -o jsonpath='{.spec.volumeName}' | xargs -I {} gcloud compute disks list --filter="name:{}" --format="table(name,diskEncryptionKey)"
```

### Secret Manager CMEK

#### Step 1: Create a KMS Key for Secrets

```bash
# Create a KMS key for Secret Manager encryption
gcloud kms keys create cre-secret-key \
  --location global \
  --keyring cre-keys \
  --purpose encryption \
  --rotation-period 7776000s \
  --next-rotation-time $(date -d '+90 days' +%Y-%m-%d) \
  --project ${PROJECT_ID}
```

#### Step 2: Enable CMEK in Terraform

In `terraform/gcp/modules/security/secrets.tf`, add the KMS key annotation:

```hcl
resource "google_secret_manager_secret" "erlang_cookie" {
  secret_id = "erlang-cookie"

  # ... other configuration ...

  annotations = {
    # Enable CMEK for this secret
    "secret-manager.iam.googleapis.com/kms-key" = "projects/${var.project_id}/locations/global/keyRings/cre-keys/cryptoKeys/cre-secret-key"
  }
}
```

#### Step 3: Enable Secret Rotation (Optional)

Uncomment and configure the rotation block in the same file:

```hcl
resource "google_secret_manager_secret" "erlang_cookie" {
  secret_id = "erlang-cookie"

  # ... other configuration ...

  # Automatic secret rotation (90 days = NIST SP 800-57 recommendation)
  rotation {
    rotation_period = "7776000s"  # 90 days in seconds
  }
}
```

---

## Secret Rotation <a name="secret-rotation"></a>

### Erlang Cookie Rotation

The Erlang cookie is used for inter-node authentication in distributed Erlang. CRE stores this in Secret Manager.

**Warning**: Erlang cookie rotation requires cluster downtime. Plan accordingly.

#### Manual Rotation Procedure

```bash
# Step 1: Generate a new Erlang cookie
export NEW_COOKIE=$(openssl rand -base64 32 | tr -d "=+/" | cut -c1-32)

# Step 2: Update the secret in Secret Manager
gcloud secrets versions add erlang-cookie \
  --data-file=- \
  --project ${PROJECT_ID} \
  --metadata=cookie.txt <<< ${NEW_COOKIE}

# Step 3: Scale down the CRE deployment to zero
kubectl scale deployment cre --replicas=0 -n cre-prod

# Step 4: Wait for all pods to terminate
kubectl wait --for=delete pods -l app=cre -n cre-prod --timeout=60s

# Step 5: Scale the deployment back up
kubectl scale deployment cre --replicas=3 -n cre-prod

# Step 6: Verify clustering works
kubectl logs -l app=cre -n cre-prod --tail=50 | grep -i "cluster"
```

**Verification**: Check that pods can communicate and that the workflow engine is functioning:

```bash
# Check pod status
kubectl get pods -n cre-prod

# Check clustering
kubectl exec -it -n cre-prod deployment/cre -- /opt/cre/bin/cre cluster status

# Run a test workflow
curl -X POST http://$(kubectl get svc cre -n cre-prod -o jsonpath='{.status.loadBalancer.ingress[0].ip}')/api/workflows \
  -H "Content-Type: application/json" \
  -d '{"workflow_id": "test", "spec": {...}}'
```

### Automatic Secret Rotation with Cloud Scheduler

You can automate secret rotation using Cloud Scheduler and Cloud Functions:

```bash
# Create a Cloud Scheduler job for 90-day rotation
gcloud scheduler jobs create http erlang-cookie-rotation \
  --schedule="0 2 1 */3 *" \
  --time-zone="UTC" \
  --uri="https://PROJECT_ID-REGION.cloudfunctions.net/rotate-erlang-cookie" \
  --http-method=POST \
  --oidc-service-account-email="${PROJECT_ID}@appspot.gserviceaccount.com" \
  --description="Rotate Erlang cookie every 90 days"
```

**Note**: The Cloud Function would need to handle:
1. Generating a new cookie
2. Adding a new secret version
3. Triggering a rolling restart of CRE pods
4. Verifying cluster health

---

## Network Security <a name="network-security"></a>

### Private Cluster

CRE deploys to a private GKE cluster by default:

- **Control plane**: Not accessible from the public internet
- **Nodes**: Private IPs only, no public endpoints
- **Pod-to-pod traffic**: Encrypted by GKE

### Network Policies

CRE implements a default-deny network policy model:

```yaml
# Default deny all ingress/egress
# Explicit allow for:
# - DNS (TCP/UDP 53)
# - EPMD (4369) and Erlang distribution ports
# - Cloud Monitoring/Logging endpoints
# - Secret Manager API
```

**Verification**:

```bash
# Verify network policies are applied
kubectl get networkpolicies -n cre-prod

# Test default-deny (should timeout/fail)
kubectl run test --image=busybox --rm -it --restart=Never -n cre-prod -- \
  wget --timeout=5 http://example.com
```

### VPC Firewall Rules

CRE requires the following VPC firewall rules (automatically created by Terraform):

```bash
# Allow IAP for SSH access (bastion host)
gcloud compute firewall-rules list --filter="name:allow-iap-*"

# Allow Cloud NAT for egress traffic
gcloud compute routers list --filter="name:cre-nat-router*"

# Allow health checks from GKE control plane
gcloud compute firewall-rules list --filter="name:*health-check*"
```

---

## IAM and Access Control <a name="iam"></a>

### Workload Identity Federation

CRE uses Workload Identity (no service account keys):

```yaml
# Kubernetes service account mapped to GCP service account
apiVersion: v1
kind: ServiceAccount
metadata:
  name: cre-ksa
  annotations:
    iam.gke.io/gcp-service-account: "cre-gke-workload@PROJECT_ID.iam.gserviceaccount.com"
```

**Benefits**:
- No long-lived service account keys
- Automatic token rotation
- Fine-grained IAM permissions

### RBAC Permissions

CRE's Kubernetes RBAC follows the principle of least privilege:

| Resource | Permissions | Justification |
|----------|-------------|---------------|
| ConfigMaps | get, list, watch | ConfigMapWatcher pattern |
| Secrets | get | Only retrieve secrets by name |
| Pods | get | Individual pod status checks |
| Leases | get, create, update, delete | Leader election |
| EndpointSlices | get, list | Service discovery |

**Verification**:

```bash
# Test RBAC permissions
kubectl auth can-i get pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
kubectl auth can-i list pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
kubectl auth can-i delete pods --as=system:serviceaccount:cre-prod:cre-ksa -n cre-prod
```

---

## Compliance <a name="compliance"></a>

### SOC 2 Type II

CRE provides controls for SOC 2 compliance:

- **Access Control**: Workload Identity + RBAC
- **Encryption**: CMEK + TLS 1.3
- **Change Management**: Binary Authorization (signed images)
- **Audit Logging**: 400-day retention (BigQuery)

**Customer Actions Required**:
- Conduct annual SOC 2 audit (or leverage GCP's SOC 2 report)
- Review access logs quarterly
- Document incident response procedures

### HIPAA

For healthcare workflows processing PHI:

- **BAA**: Sign HIPAA BAA with Google Cloud
- **CMEK**: Enable CMEK for all data-at-rest
- **Audit**: Enable 400-day log retention (exceeds 6-year HIPAA requirement with BigQuery extension)
- **Access**: Restrict PHI access to authorized personnel only

**HIPAA BAA**:
- Available via Google Cloud (customer agreement)
- Link: https://cloud.google.com/hipaa-compliance

### PCI-DSS

For payment card workflows:

- **Scope**: CRE pods, Persistent Disks, workflow data
- **Encryption**: CMEK required
- **Logging**: 1-year minimum (CRE provides 400 days)
- **Vulnerability Scanning**: Trivy CI/CD integration
- **Penetration Testing**: Annual pen test required (customer responsibility)

---

## Shared Responsibility Model <a name="shared-responsibility"></a>

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

### Incident Response

**CRE Platform**:
- Provides audit logs (wf_audit_log, XES)
- Implements Cloud Monitoring alerts
- Documents incident response procedures

**Customer**:
- Monitors audit logs
- Responds to security incidents
- Notifies affected parties (breach notification)
- Conducts post-mortem reviews

---

## Troubleshooting

### CMEK Issues

**Problem**: Pods fail to start with "KMS key not found" error

**Solution**:
```bash
# Verify KMS key exists
gcloud kms keys describe cre-disk-key \
  --location global \
  --keyring cre-keys \
  --project ${PROJECT_ID}

# Verify GKE node SA has permission
gcloud kms keys get-iam-policy cre-disk-key \
  --location global \
  --keyring cre-keys \
  --project ${PROJECT_ID}

# Re-grant permission if missing
gcloud kms keys add-iam-policy-binding cre-disk-key \
  --location global \
  --keyring cre-keys \
  --member serviceAccount:${GKE_NODE_SA} \
  --role roles/cloudkms.cryptoKeyEncrypterDecrypter
```

### Secret Rotation Issues

**Problem**: Pods fail to start after secret rotation

**Solution**:
```bash
# Check secret version
gcloud secrets versions list erlang-cookie --project ${PROJECT_ID}

# Verify secret is accessible
gcloud secrets versions access latest --secret=erlang-cookie --project ${PROJECT_ID}

# Restart pods with new secret
kubectl rollout restart deployment cre -n cre-prod

# Check pod logs for clustering errors
kubectl logs -l app=cre -n cre-prod --tail=100
```

---

## References

- [GCP Marketplace Security Requirements](https://cloud.google.com/marketplace/docs/partner/security-requirements)
- [CMEK Documentation](https://cloud.google.com/kms/docs/encrypting-data)
- [Secret Manager CMEK](https://cloud.google.com/secret-manager/docs/cmek)
- [Pod Security Standards](https://kubernetes.io/docs/concepts/security/pod-security-standards/)
- [Network Policies](https://kubernetes.io/docs/concepts/services-networking/network-policies/)
- [Workload Identity](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)

---

**Document Version**: 1.0
**Last Updated**: 2025-01-11
**Maintained By**: CRE Security Team (security@common-runtime.org)
