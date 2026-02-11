# Terraform Provisioning Guide for CRE GCP Infrastructure

## Overview

The `terraform/gcp/provision.sh` script provides a complete, production-ready workflow for provisioning the CRE (Common Runtime Environment) infrastructure on Google Cloud Platform using Terraform.

This guide explains how to use the script to initialize, plan, and apply the GKE infrastructure with automatic state backup, validation, and output of critical values like cluster endpoints and service account emails.

## Features

- **Full Terraform Workflow**: init → plan → apply with validation at each step
- **Automatic State Backup**: Backup GCS-stored state before applying changes
- **Validation Steps**: Configuration validation, GCP authentication checks
- **Sensitive Output Protection**: Automatic handling of sensitive outputs (cluster certificates, API keys)
- **Dry-Run Mode**: Preview changes without executing them
- **Multiple Commands**: Support for init, plan, apply, destroy, and output operations
- **Key Output Display**: Automatically display cluster endpoint, service accounts, load balancer IPs
- **Environment Management**: Support for dev, staging, and production environments
- **Error Handling**: Comprehensive error checking with meaningful exit codes

## Prerequisites

### GCP Setup

1. **GCP Project** with the following APIs enabled:
   - Kubernetes Engine API
   - Compute Engine API
   - Cloud Resource Manager API
   - Cloud Storage API (for Terraform state backend)
   - Secret Manager API
   - IAM API

2. **GCS Bucket** for Terraform state storage (must exist):
   ```bash
   gsutil mb gs://your-terraform-state-bucket/
   ```

3. **GCP Service Account** with appropriate permissions:
   - Editor or custom role with Kubernetes, Compute, Storage, IAM permissions
   - For production: use Workload Identity Federation instead of service account keys

### Local Setup

1. **Terraform** >= 1.5.0:
   ```bash
   terraform version  # Should show >= 1.5.0
   ```

2. **Google Cloud CLI** (gcloud):
   ```bash
   gcloud --version
   gcloud auth login
   gcloud config set project YOUR_PROJECT_ID
   ```

3. **gsutil** (included with gcloud):
   ```bash
   gsutil version -l
   ```

4. **jq** (optional, for JSON output parsing):
   ```bash
   jq --version
   ```

## Quick Start

### 1. Initialize and Apply (Full Workflow)

```bash
cd terraform/gcp

# Run full workflow (init → plan → apply)
./provision.sh \
  --project my-gcp-project \
  --bucket my-terraform-state \
  --region us-central1 \
  --environment production

# You'll be prompted to confirm the plan before applying
```

### 2. Plan Only (Review Changes)

```bash
./provision.sh \
  --project my-gcp-project \
  --bucket my-terraform-state \
  plan

# Review the plan output
```

### 3. Apply Pre-Generated Plan

```bash
./provision.sh \
  --project my-gcp-project \
  --auto-approve \
  apply

# Applies the previously generated plan without confirmation
```

### 4. View Infrastructure Details

```bash
# Display cluster endpoint, service accounts, and load balancer IPs
./provision.sh output

# Display as JSON (for parsing by scripts)
./provision.sh --json output
```

## Command Reference

### Full Workflow (Default)

```bash
./provision.sh --project my-project --bucket tf-state
```

Runs: init → validate → plan → apply with confirmations and backups.

**Exit codes**:
- 0: Success
- 3: Init failed
- 2: Validation failed
- 4: Plan failed
- 5: Apply failed
- 6: Backup failed

### Init Command

```bash
./provision.sh --project my-project --bucket tf-state init
```

Initializes Terraform with GCS backend configuration.

**Required flags**:
- `--project`: GCP project ID
- `--bucket`: GCS bucket for state storage

### Plan Command

```bash
./provision.sh --project my-project plan
```

Generates a Terraform plan without applying changes.

Creates `terraform.tfplan` file for later application.

### Apply Command

```bash
./provision.sh --project my-project --auto-approve apply
```

Applies a previously generated plan.

**Flags**:
- `--auto-approve`: Skip confirmation prompt (use with caution)
- `--backup`: Backup state before applying (default: true)
- `--no-backup`: Skip state backup

### Destroy Command

```bash
./provision.sh --project my-project destroy
```

Destroys all Terraform-managed resources.

**Warning**: This action cannot be undone!

Requires explicit confirmation or `--auto-approve` flag.

### Output Command

```bash
./provision.sh output
```

Display Terraform outputs including:
- GCP project ID
- GKE cluster name and endpoint
- VPC network configuration
- Load balancer IPs
- Service account emails (for security module)
- Commands for kubectl configuration

**Example output**:
```
GCP Project:
  my-gcp-project

GKE Cluster:
  cre-cluster

Cluster Endpoint (sensitive):
  Use 'terraform output gke_cluster_endpoint' to view

Cluster Access Command:
  gcloud container clusters regional get-credentials cre-cluster \
    --region us-central1

Service Accounts:
  Use 'terraform output security.service_accounts' to view

Load Balancer IPs:
  Internal: 10.0.1.100
  External: 35.192.0.1
```

### Validate Command

```bash
./provision.sh validate
```

Validates Terraform configuration syntax without initializing.

### Format Check Command

```bash
./provision.sh fmt-check
```

Checks if all Terraform files are properly formatted.

### State Backup Command

```bash
./provision.sh --bucket tf-state state-backup
```

Manually backup Terraform state from GCS to local directory.

Backups are stored in `.terraform-state-backups/` with timestamps.

**Features**:
- Automatic compression (gzip)
- Keep last 5 backups
- Timestamp naming for easy identification

### State Restore Command

```bash
./provision.sh state-restore /path/to/backup.tfstate.gz
```

Restore Terraform state from backup file.

**Warning**: This overwrites the current state!

Requires explicit confirmation.

## Configuration Options

### Flags

| Flag | Description | Default | Required |
|------|-------------|---------|----------|
| `--project` | GCP project ID | From gcloud config | For init |
| `--bucket` | GCS bucket for state | None | For init |
| `--prefix` | State file prefix | gcp/production | No |
| `--region` | Primary GCP region | us-central1 | No |
| `--environment` | Environment (dev/staging/prod) | production | No |
| `--auto-approve` | Skip confirmation prompts | false | No |
| `--backup` | Backup state before apply | true | No |
| `--no-backup` | Skip state backup | false | No |
| `--dry-run` | Preview without executing | false | No |
| `--verbose` | Enable debug output | false | No |
| `--json` | JSON output format | false | No |

### Environment Variables

```bash
# Set these instead of using flags
export TF_VAR_project_id="my-gcp-project"
export TF_VAR_region="us-central1"
export TF_VAR_environment="production"
export TERRAFORM_STATE_BUCKET="my-tf-state"
export TERRAFORM_STATE_PREFIX="gcp/production"

# GCP authentication
export GOOGLE_APPLICATION_CREDENTIALS="/path/to/credentials.json"
export GCP_PROJECT="my-gcp-project"  # Fallback for project_id
```

### Terraform Variables

The script uses Terraform variables defined in `variables.tf`:

```hcl
# Required
variable "project_id" { }

# Optional with defaults
variable "region" { default = "us-central1" }
variable "zone" { default = "us-central1-a" }
variable "environment" { default = "production" }

# Complex configurations (use terraform.tfvars)
variable "vpc_config" { }
variable "gke_config" { }
variable "storage_config" { }
variable "lb_config" { }
```

Create a `terraform.tfvars` file:

```hcl
project_id = "my-gcp-project"
region     = "us-central1"
environment = "production"

# Optional: customize VPC, GKE, storage, load balancer configs
# See terraform.tfvars.example for full configuration
```

## Workflow Examples

### Example 1: Production Deployment

```bash
cd terraform/gcp

# Validate configuration
./provision.sh --project cre-prod validate

# Run full workflow
./provision.sh \
  --project cre-prod \
  --bucket cre-terraform-state \
  --region us-central1 \
  --environment production

# Review outputs
./provision.sh output

# Output:
# GKE cluster endpoint, service accounts, load balancer IPs
```

### Example 2: Development Environment

```bash
cd terraform/gcp

# Plan changes for dev environment
./provision.sh \
  --project cre-dev \
  --bucket cre-terraform-state \
  --prefix gcp/development \
  --region us-east1 \
  --environment dev \
  plan

# Review terraform.tfplan

# Apply if satisfied
./provision.sh \
  --project cre-dev \
  --auto-approve \
  apply
```

### Example 3: Dry-Run Preview

```bash
# See what would happen without making changes
./provision.sh \
  --project cre-prod \
  --bucket cre-terraform-state \
  --dry-run \
  --verbose
```

### Example 4: State Management

```bash
# Backup before major changes
./provision.sh \
  --bucket cre-terraform-state \
  state-backup

# List available backups
ls -lh .terraform-state-backups/

# Restore from backup if needed
./provision.sh \
  state-restore .terraform-state-backups/terraform.tfstate.20250211_152800.backup.gz
```

## Understanding the Output

### Key Outputs Explained

After applying successfully, you'll see important values:

```
GCP Project: my-gcp-project

GKE Cluster: cre-cluster

Cluster Endpoint (sensitive):
  Use 'terraform output gke_cluster_endpoint' to view

  # To retrieve manually:
  # terraform output -raw gke_cluster_endpoint
```

### Retrieving Specific Outputs

```bash
# Cluster endpoint (will prompt for approval due to sensitive flag)
terraform output gke_cluster_endpoint

# Cluster name
terraform output -raw gke_cluster_name

# Service account details
terraform output security.service_accounts

# Internal load balancer IP
terraform output -raw internal_lb_ip

# All outputs as JSON
terraform output -json
```

### Service Accounts Created

The security module creates three service accounts:

```bash
# Get service account emails
terraform output security.service_accounts

# Output format:
# {
#   "gke_node" = "cre-gke-node@my-gcp-project.iam.gserviceaccount.com",
#   "gke_workload" = "cre-gke-workload@my-gcp-project.iam.gserviceaccount.com",
#   "terraform" = "cre-terraform@my-gcp-project.iam.gserviceaccount.com"
# }
```

## State Backup and Recovery

### Automatic Backups

By default, the script backs up state before applying changes:

```bash
# Backup location (relative to project root)
.terraform-state-backups/terraform.tfstate.YYYYMMDD_HHMMSS.backup.gz
```

### Manual Backups

```bash
# Create a manual backup
./provision.sh --bucket cre-terraform-state state-backup

# List all backups
ls -lh .terraform-state-backups/

# Restore a specific backup
./provision.sh state-restore .terraform-state-backups/terraform.tfstate.20250211_150000.backup.gz
```

### Backup Retention

- **Kept**: Last 5 backups
- **Location**: GCS bucket (remote) + `.terraform-state-backups/` (local)
- **Format**: gzip compressed JSON
- **Naming**: `terraform.tfstate.TIMESTAMP.backup.gz`

## Post-Deployment Steps

After successful provisioning:

### 1. Configure kubectl

```bash
# Get the command from outputs
CLUSTER_ENDPOINT=$(terraform output -raw cluster_access_command)
eval "$CLUSTER_ENDPOINT"

# Verify access
kubectl cluster-info
kubectl get nodes
```

### 2. Deploy CRE Application

```bash
# Apply Kubernetes manifests
kubectl apply -f ../../k8s/gcp/

# Verify deployment
kubectl get pods -n cre-prod
kubectl get services -n cre-prod
```

### 3. Configure Monitoring and Logging

```bash
# Enable Cloud Logging and Cloud Monitoring
# (Already configured in Terraform)

# View logs
gcloud logging read "resource.type=k8s_cluster" --limit 50

# View metrics
gcloud monitoring dashboards list
```

### 4. Verify Load Balancers

```bash
# Get load balancer IPs
terraform output internal_lb_ip
terraform output external_lb_ip

# Test connectivity
curl http://$(terraform output -raw external_lb_ip):8080/health
```

## Troubleshooting

### Authentication Issues

```bash
# Error: "Not authenticated with gcloud"
Solution:
  gcloud auth application-default login
  gcloud auth login
  gcloud config set project YOUR_PROJECT_ID
```

### GCS Bucket Not Accessible

```bash
# Error: "GCS bucket not accessible"
Solution:
  # Verify bucket exists
  gsutil ls gs://your-bucket/

  # Check permissions
  gcloud projects get-iam-policy PROJECT_ID \
    --flatten="bindings[].members" \
    --format='table(bindings.role)' \
    --filter="bindings.members:YOUR_SERVICE_ACCOUNT"
```

### Terraform Validation Fails

```bash
# Error: "Terraform configuration validation failed"
Solution:
  # Check syntax
  terraform fmt -recursive
  terraform validate

  # Check variables
  terraform plan -var-file=terraform.tfvars
```

### State Mismatch

```bash
# Error: "State refresh failed"
Solution:
  # Refresh state
  terraform refresh

  # Or restore from backup
  ./provision.sh state-restore backup.gz
```

### Insufficient Quotas

```bash
# Error: "Quota exceeded"
Solution:
  # Check quotas
  gcloud compute project-info describe --project=PROJECT_ID

  # Request quota increase in GCP console
  # Console → IAM & Admin → Quotas
```

## Security Considerations

### State File Security

- **Storage**: Terraform state is stored in GCS (encrypted at rest by default)
- **Access**: Restrict GCS bucket access via IAM roles
- **Backup**: Local backups stored in `.terraform-state-backups/` - keep secure
- **Sensitive Data**: Service account keys, certificates stored in state

### Secret Management

The configuration includes:
- Secret Manager integration for sensitive data
- Workload Identity Federation (no service account keys needed)
- Secret rotation policies

### GCP Permissions

Service accounts created:
- `cre-gke-node`: GKE node service account (minimal permissions)
- `cre-gke-workload`: Kubernetes workload service account
- `cre-terraform`: Terraform provisioning service account

Use least-privilege IAM roles per the security module.

## Advanced Usage

### Using Environment Variables

```bash
# Set all variables via environment
export TF_VAR_project_id="my-project"
export TF_VAR_region="us-east1"
export TF_VAR_environment="staging"
export TERRAFORM_STATE_BUCKET="my-tf-state"
export TERRAFORM_STATE_PREFIX="gcp/staging"

# Run script without flags
./provision.sh
```

### Integration with CI/CD

```bash
# GitHub Actions example
- name: Apply Terraform
  run: |
    cd terraform/gcp
    ./provision.sh \
      --project ${{ secrets.GCP_PROJECT }} \
      --bucket ${{ secrets.TF_STATE_BUCKET }} \
      --auto-approve \
      apply
  env:
    GOOGLE_APPLICATION_CREDENTIALS: ${{ secrets.GCP_CREDENTIALS }}
```

### Custom Terraform Variables

```bash
# Create environment-specific tfvars
cp terraform.tfvars.example terraform.tfvars.prod
cp terraform.tfvars.example terraform.tfvars.staging

# Edit each file with appropriate values

# Use with script (requires manual terraform apply)
terraform init
terraform plan -var-file=terraform.tfvars.prod
terraform apply -var-file=terraform.tfvars.prod
```

## Reference

### Script Location

```
terraform/gcp/provision.sh
```

### Related Files

- `variables.tf`: Terraform input variables
- `outputs.tf`: Terraform output values
- `main.tf`: Root module configuration
- `versions.tf`: Provider versions and backend configuration
- `terraform.tfvars.example`: Example variable values

### GCP Resources Created

- VPC network with subnets
- GKE regional cluster
- Node pools (general and memory-optimized)
- Storage classes and PVCs
- Internal and external load balancers
- Cloud NAT
- Cloud Armor policies
- Service accounts with Workload Identity
- Secret Manager secrets
- Network policies

### Documentation

- [GCP Marketplace Readiness](./GCP_MARKETPLACE_READINESS.md)
- [Terraform README](../../terraform/gcp/README.md)
- [CRE Documentation](../../docs/)

## Support

For issues or questions:

1. Check [Troubleshooting](#troubleshooting) section
2. Review Terraform state: `terraform show`
3. Check GCP Cloud Logging: `gcloud logging read`
4. Review script logs: Run with `--verbose` flag
5. Create GitHub issue: https://github.com/joergen7/cre/issues

---

**Document Version**: 1.0
**Last Updated**: 2025-02-11
**Script Version**: 1.0.0
