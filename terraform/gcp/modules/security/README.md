# GCP Security Module for CRE

Complete security module for CRE (Common Runtime Environment) deployment on Google Kubernetes Engine. Implements defense-in-depth security with service account keyless authentication, secret management, and network policies.

## Features

- **Keyless Authentication**: Workload Identity Federation for GitHub Actions and GKE workloads
- **Least Privilege IAM**: Minimal permissions assigned to all service accounts
- **Secret Manager**: Secure storage for Erlang cookie and sensitive configuration
- **Network Policies**: Pod-to-pod communication control with default-deny posture
- **Pod Security Standards**: Enforced at namespace level

## Usage

```hcl
module "cre_security" {
  source = "./modules/security"

  project_id    = "my-cre-project"
  region        = "us-central1"
  name_prefix   = "cre-prod"
  cluster_name  = "cre-cluster"

  # GitHub Actions
  github_repository = "myorg/cre"

  # GKE
  gke_namespace             = "cre"
  kubernetes_service_account = "cre-sa"

  # Network policies
  enable_default_deny_policies = true
  cre_app_selector = {
    app = "cre"
  }

  # Pod Security
  pod_security_enforce_level = "restricted"
}
```

## Components

### IAM (iam.tf)
- `gke-node` service account: For GKE node operations (image pull, logging)
- `gke-workload` service account: For CRE application pods
- `terraform` service account: For CI/CD infrastructure management

### Workload Identity (workload_identity.tf)
- GitHub Actions pool: Enable keyless authentication from GitHub
- GKE pool: Enable Kubernetes service accounts to impersonate GCP service accounts

### Secrets (secrets.tf)
- `erlang-cookie`: Critical for Erlang distributed node communication
- `app-config`: Sensitive application configuration
- Automatic rotation support
- Secret Store CSI Driver annotations

### Network Policies (network_policy.tf)
- Default deny ingress/egress (optional, recommended for production)
- Allow DNS resolution
- Allow Erlang inter-node communication
- Allow GCP service access (monitoring, secret manager, pub/sub)

## GitHub Actions Configuration

The module outputs the configuration needed for GitHub Actions:

```yaml
- name: Authenticate to GCP
  uses: google-github-actions/auth@v2
  with:
    workload_identity_provider: projects/123456789/locations/global/workloadIdentityPools/github-actions-pool/providers/github-provider
    service_account: cre-prod-terraform@my-cre-project.iam.gserviceaccount.com
```

## Secret Access in Kubernetes

### Using Secret Store CSI Driver

```yaml
volumes:
- name: erlang-cookie
  csi:
    driver: secretmanager.csi.k8s.io
    readOnly: true
    volumeAttributes:
      secret-name: cre-prod-erlang-cookie
```

### Using External Secrets Operator

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: erlang-cookie
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: gcpsm-secretstore
    kind: SecretStore
  data:
  - secretKey: cookie
    remoteRef:
      key: cre-prod-erlang-cookie
```

## Security Best Practices Implemented

1. **No Service Account Keys**: All authentication uses Workload Identity Federation
2. **Least Privilege**: Service accounts have only the permissions they need
3. **Secrets Not in Code**: All sensitive data stored in Secret Manager
4. **Network Segmentation**: Default-deny policies with explicit allow rules
5. **Audit Logging**: All secret access is logged in Cloud Audit Logs
6. **Pod Security**: Restricted policies prevent privilege escalation
7. **Secret Rotation**: TTL-based automatic cleanup of old versions

## Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `project_id` | GCP project ID | required |
| `region` | GCP region | required |
| `name_prefix` | Prefix for resource names | required |
| `cluster_name` | GKE cluster name | required |
| `github_repository` | GitHub repo for Workload Identity | required |
| `gke_namespace` | Kubernetes namespace | `"cre"` |
| `enable_default_deny_policies` | Enable default-deny network policies | `false` |
| `pod_security_enforce_level` | Pod Security enforce level | `"baseline"` |

See `variables.tf` for complete list.

## Outputs

| Output | Description |
|--------|-------------|
| `service_accounts` | All service account emails |
| `workload_identity_pools` | Pool configurations |
| `secrets` | Secret Manager secret IDs |
| `secret_annotations` | CSI Driver annotations |
| `network_policies` | Network policy names |
| `github_actions_config` | Configuration for GitHub Actions |

## Requirements

- Terraform >= 1.3.0
- GKE cluster with Workload Identity enabled
- Kubernetes provider configured for cluster access
- Appropriate permissions to create IAM resources
