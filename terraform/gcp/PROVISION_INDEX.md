# CRE Terraform Provisioning - Complete Documentation Index

## Overview

The `provision.sh` script automates the complete Terraform workflow for provisioning the CRE (Common Runtime Environment) on Google Cloud Platform. It provides a production-ready solution for infrastructure as code with automatic validation, state backup, and comprehensive output of critical values.

## Files Created

### 1. Provisioning Script
**Location**: `/home/user/cre/terraform/gcp/provision.sh`

**Size**: ~26 KB | **Lines**: 908 | **Functions**: 62

Comprehensive Bash script that provides:
- Full Terraform workflow automation (init → plan → apply)
- Automatic GCS state backup with versioning
- Multi-step validation (GCP auth, Terraform syntax, configuration)
- Sensitive output protection (cluster certs, API keys)
- Dry-run preview mode
- Support for dev/staging/production environments
- Error handling with meaningful exit codes
- Color-coded logging output

**Key Features**:
- Validates GCP authentication and credentials
- Validates backend GCS bucket accessibility
- Backs up state before applying (with automatic compression and rotation)
- Supports manual state restoration from backups
- Displays critical outputs (cluster endpoint, service accounts, LB IPs)
- Works with environment variables or command-line flags
- Idempotent - safe to run multiple times

### 2. Quick Reference Guide
**Location**: `/home/user/cre/terraform/gcp/PROVISION_QUICK_REFERENCE.md`

**Size**: ~6 KB

Fast lookup guide for developers including:
- Basic commands and usage patterns
- All available flags and their usage
- All available commands (init, plan, apply, destroy, output, validate, fmt-check, state-backup, state-restore)
- Common workflows (first-time setup, plan before apply, backup/restore)
- Environment-specific examples (dev vs production)
- Output retrieval commands
- Troubleshooting quick fixes
- Exit codes reference
- File location map

**Best For**: Quick lookups while working at the command line

### 3. Comprehensive Provisioning Guide
**Location**: `/home/user/cre/docs/gcp/TERRAFORM_PROVISIONING.md`

**Size**: ~16 KB

Complete documentation including:
- Feature overview
- Prerequisites (GCP setup, local setup)
- Quick start examples
- Command reference with detailed explanations
- Configuration options (flags, environment variables, Terraform variables)
- Workflow examples (production, development, dry-run, state management)
- Understanding outputs (cluster endpoint, service accounts, load balancers)
- State backup and recovery procedures
- Post-deployment steps (kubectl, monitoring, application deployment)
- Troubleshooting section with solutions
- Security considerations
- Advanced usage patterns
- CI/CD integration examples
- Reference information

**Best For**: Learning how to use the script and understanding the infrastructure

### 4. This Index
**Location**: `/home/user/cre/terraform/gcp/PROVISION_INDEX.md`

Navigation and summary of all documentation.

## Quick Navigation

### I Need To...

| Goal | Go To |
|------|-------|
| Deploy infrastructure for the first time | [Quick Start](#quick-start) |
| Understand the script capabilities | [Script Features](#script-features) |
| Learn what commands are available | [Commands](#commands) |
| Configure environments (dev/staging/prod) | [Configuration](#configuration) |
| Understand outputs and how to retrieve them | [Outputs](#outputs) |
| Backup or restore Terraform state | [State Management](#state-management) |
| Deploy the CRE application after provisioning | [Post-Deployment](#post-deployment) |
| Fix an error or issue | [Troubleshooting](#troubleshooting) |
| Use the script in CI/CD pipelines | [Advanced Usage](#advanced-usage) |
| Look up a specific command quickly | [PROVISION_QUICK_REFERENCE.md](./PROVISION_QUICK_REFERENCE.md) |

## Quick Start

### Minimal Example (5 minutes)

```bash
cd terraform/gcp

# Configure project (required)
export TF_VAR_project_id="my-gcp-project"
export TERRAFORM_STATE_BUCKET="my-terraform-state"

# Run provisioning (will prompt for confirmation)
./provision.sh
```

### With All Options

```bash
cd terraform/gcp

./provision.sh \
  --project my-gcp-project \
  --bucket my-terraform-state \
  --region us-central1 \
  --environment production
```

## Script Features

The provision.sh script includes 62 functions organized into these categories:

### Utility Functions (6)
- `log_info()` - Information logging with color
- `log_success()` - Success logging
- `log_warning()` - Warning logging
- `log_error()` - Error logging to stderr
- `log_debug()` - Debug logging (with --verbose flag)
- `show_progress()` - Progress tracking

### Argument Parsing (2)
- `parse_arguments()` - Command-line argument parsing
- `print_usage()` - Usage information display

### Validation Functions (4)
- `validate_requirements()` - Check for required tools (terraform, gcloud, gsutil)
- `validate_gcp_auth()` - Verify GCP authentication
- `validate_terraform_config()` - Validate Terraform syntax
- `validate_backend_config()` - Verify GCS bucket accessibility

### State Management (3)
- `backup_state()` - Backup state to GCS with compression
- `list_state_backups()` - List available backups
- `restore_state()` - Restore state from backup file

### Terraform Operations (7)
- `terraform_init()` - Initialize Terraform with GCS backend
- `terraform_validate()` - Validate configuration
- `terraform_fmt_check()` - Check code formatting
- `terraform_plan()` - Generate execution plan
- `terraform_apply()` - Apply Terraform changes
- `terraform_destroy()` - Destroy resources
- `terraform_output()` - Display outputs

### Workflow Management (2)
- `run_full_workflow()` - Execute init → plan → apply workflow
- `run_command()` - Dispatch to appropriate command handler
- `main()` - Script entry point

## Commands

### Available Commands

```
init              Initialize Terraform backend and providers
plan              Generate and review execution plan
apply             Apply Terraform configuration
destroy           Destroy Terraform resources
full              Run init → plan → apply workflow (default)
output            Display key outputs
validate          Validate configuration syntax
fmt-check         Check code formatting
state-backup      Backup Terraform state file
state-restore     Restore Terraform state from backup
```

### Default Behavior

If no command is specified, the script runs the **full** workflow:

```bash
./provision.sh --project x --bucket y
# Equivalent to:
./provision.sh --project x --bucket y full
```

Full workflow includes:
1. Validate requirements
2. Validate GCP authentication
3. Validate backend configuration
4. Terraform init
5. Terraform validate
6. Terraform plan (generates terraform.tfplan)
7. Terraform apply (with confirmation prompt)
8. Display key outputs

## Configuration

### Command-Line Flags

```bash
--project ID          GCP project ID (required for init)
--bucket BUCKET       GCS bucket for state (required for init)
--prefix PREFIX       State file prefix (default: gcp/production)
--region REGION       Primary GCP region (default: us-central1)
--environment ENV     Environment: dev, staging, production (default: production)
--auto-approve        Auto-approve apply without confirmation
--backup              Backup state before apply (default)
--no-backup           Skip state backup
--dry-run             Preview without executing
--verbose             Enable debug output
--json                Output in JSON format
--help                Show help message
```

### Environment Variables

```bash
TF_VAR_project_id             # GCP project ID
TF_VAR_region                 # Primary region
TF_VAR_environment            # Environment name
TERRAFORM_STATE_BUCKET        # GCS bucket for state
TERRAFORM_STATE_PREFIX        # State file prefix
GCP_PROJECT                   # Fallback for project_id
GOOGLE_APPLICATION_CREDENTIALS # Path to credentials JSON
```

### Terraform Variables (terraform.tfvars)

See `terraform.tfvars.example` for full configuration:

```hcl
project_id  = "my-gcp-project"
region      = "us-central1"
environment = "production"

# Optional configurations
vpc_config             # VPC, subnets, NAT
gke_config             # Cluster, node pools
storage_config         # Storage classes, snapshots
lb_config              # Load balancers, Cloud Armor
backup_config          # Backup infrastructure
```

## Outputs

### Key Outputs Displayed

After successful provisioning:

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

### Retrieving Specific Outputs

```bash
# Cluster endpoint
terraform output gke_cluster_endpoint

# Service accounts
terraform output security.service_accounts

# All outputs as JSON
terraform output -json

# Specific sensitive output
terraform output -raw internal_lb_ip
```

### Output Values Available

The Terraform configuration outputs:
- `project_id` - GCP project ID
- `region` - Primary region
- `gke_cluster_name` - GKE cluster name
- `gke_cluster_endpoint` - Kubernetes API endpoint (sensitive)
- `gke_cluster_ca_certificate` - Cluster CA cert (sensitive)
- `gke_node_pools` - Node pool names
- `vpc_network_name` - VPC network name
- `internal_lb_ip` - Internal load balancer IP
- `external_lb_ip` - External load balancer IP
- `cluster_access_command` - kubectl configuration command
- `security.service_accounts` - Created service accounts

## State Management

### Automatic Backups

By default, the script backs up state before applying changes:

**Location**: `.terraform-state-backups/terraform.tfstate.YYYYMMDD_HHMMSS.backup.gz`

**Features**:
- Compressed with gzip
- Timestamped for easy identification
- Last 5 backups kept
- Stored locally and in GCS

### Manual Backup

```bash
./provision.sh --bucket my-bucket state-backup
```

### List Backups

```bash
ls -lh .terraform-state-backups/
```

### Restore from Backup

```bash
./provision.sh state-restore .terraform-state-backups/terraform.tfstate.20250211_150000.backup.gz
```

## Post-Deployment

After successful provisioning:

### 1. Configure kubectl

```bash
# Execute the command from outputs
gcloud container clusters regional get-credentials cre-cluster \
  --region us-central1 --project my-gcp-project

# Verify access
kubectl cluster-info
kubectl get nodes
```

### 2. Create Namespaces and RBAC

```bash
kubectl create namespace cre-prod
kubectl apply -f k8s/gcp/namespace.yaml
kubectl apply -f k8s/gcp/serviceaccount.yaml
```

### 3. Deploy CRE Application

```bash
kubectl apply -f k8s/gcp/
```

### 4. Verify Deployment

```bash
kubectl get pods -n cre-prod
kubectl get services -n cre-prod
kubectl logs -n cre-prod deployment/cre
```

## Troubleshooting

### Common Issues

| Issue | Symptom | Solution |
|-------|---------|----------|
| Not authenticated | "Not authenticated with gcloud" | `gcloud auth login` |
| GCS bucket error | "GCS bucket not accessible" | `gsutil ls gs://bucket/` |
| Terraform version | "Terraform >= 1.5.0 required" | Update Terraform |
| State mismatch | "State refresh failed" | Run `terraform refresh` |
| Quota exceeded | "Quota exceeded" | Request quota increase in GCP console |
| Permission denied | "Permission denied" on resources | Check IAM roles on service account |

### Debug Mode

```bash
# Run with verbose output
./provision.sh --project x --bucket y --verbose full

# Dry-run to preview
./provision.sh --project x --bucket y --dry-run full

# Manual Terraform operations
terraform init -backend-config=bucket=x -backend-config=prefix=y
terraform plan -var-file=terraform.tfvars
terraform validate
```

## Advanced Usage

### Environment-Specific Deployments

```bash
# Development
./provision.sh \
  --project cre-dev \
  --bucket cre-tf-state \
  --prefix gcp/development \
  --environment dev \
  --region us-east1

# Staging
./provision.sh \
  --project cre-staging \
  --bucket cre-tf-state \
  --prefix gcp/staging \
  --environment staging

# Production
./provision.sh \
  --project cre-prod \
  --bucket cre-tf-state \
  --prefix gcp/production \
  --environment production
```

### CI/CD Integration (GitHub Actions)

```yaml
- name: Provision GCP Infrastructure
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

### Custom Terraform Workflows

```bash
# Plan only without applying
./provision.sh --project x --bucket y plan

# Review the plan
cat terraform.tfplan

# Apply manually
terraform apply terraform.tfplan

# Or use the script to apply
./provision.sh --auto-approve apply
```

## Exit Codes

| Code | Meaning | When It Occurs |
|------|---------|----------------|
| 0 | Success | All operations completed successfully |
| 1 | General error | Unexpected script error |
| 2 | Validation error | Configuration or argument validation failed |
| 3 | Init failed | Terraform init or requirement validation failed |
| 4 | Plan failed | Terraform plan generation failed |
| 5 | Apply failed | Terraform apply or destroy failed |
| 6 | Backup failed | State backup or restore failed |
| 7 | Credentials error | GCP authentication or credentials error |

## File Structure

```
terraform/gcp/
├── provision.sh                      (Main provisioning script)
├── PROVISION_QUICK_REFERENCE.md      (Quick lookup guide)
├── PROVISION_INDEX.md                (This file)
│
├── main.tf                           (Root module)
├── variables.tf                      (Input variables)
├── outputs.tf                        (Output values)
├── versions.tf                       (Provider configuration)
├── terraform.tfvars.example          (Example configuration)
│
└── modules/
    ├── gke_cluster/                  (GKE cluster module)
    ├── vpc/                          (VPC module)
    ├── storage/                      (Storage module)
    ├── loadbalancer/                 (Load balancer module)
    ├── backup/                       (Backup module)
    ├── monitoring/                   (Monitoring module)
    └── security/                     (Security module)
```

## Documentation Files

| File | Location | Purpose | Size |
|------|----------|---------|------|
| provision.sh | terraform/gcp/ | Main provisioning script | 26 KB |
| PROVISION_QUICK_REFERENCE.md | terraform/gcp/ | Quick lookup guide | 6 KB |
| TERRAFORM_PROVISIONING.md | docs/gcp/ | Comprehensive guide | 16 KB |
| PROVISION_INDEX.md | terraform/gcp/ | This index | 10 KB |

## Related Documentation

- [GCP Marketplace Readiness](../docs/gcp/GCP_MARKETPLACE_READINESS.md)
- [Terraform Module README](./README.md)
- [GCP Runbooks](../docs/gcp/runbooks/)
- [CRE Documentation](../docs/)

## Support and Resources

### Getting Help

1. **Quick questions**: Check [PROVISION_QUICK_REFERENCE.md](./PROVISION_QUICK_REFERENCE.md)
2. **Detailed guidance**: See [TERRAFORM_PROVISIONING.md](../docs/gcp/TERRAFORM_PROVISIONING.md)
3. **Issues**: Run with `--verbose` and check Terraform logs
4. **Community**: GitHub issues at https://github.com/joergen7/cre/issues

### Useful Commands

```bash
# Check script syntax
bash -n provision.sh

# Show help
./provision.sh --help

# Validate configuration
./provision.sh validate

# Check formatting
./provision.sh fmt-check

# View current state
terraform show

# View outputs
terraform output -json

# Check GCS backend
gsutil ls gs://my-bucket/

# View Terraform logs
TF_LOG=DEBUG terraform plan
```

## Version Information

- **Script Version**: 1.0.0
- **Last Updated**: 2025-02-11
- **Terraform Version Required**: >= 1.5.0
- **Google Provider Version**: ~> 6.0
- **Google Beta Provider Version**: >= 7.0

## Checklist for New Users

- [ ] Prerequisites installed (terraform, gcloud, gsutil)
- [ ] GCP project created with APIs enabled
- [ ] GCS bucket created for Terraform state
- [ ] terraform.tfvars configured with project_id
- [ ] GCP authentication configured (`gcloud auth login`)
- [ ] Script made executable (`chmod +x provision.sh`)
- [ ] Read PROVISION_QUICK_REFERENCE.md for overview
- [ ] Run validation: `./provision.sh validate`
- [ ] Run plan: `./provision.sh --project X --bucket Y plan`
- [ ] Apply: `./provision.sh --project X --bucket Y apply`
- [ ] Configure kubectl and verify access
- [ ] Deploy CRE application

---

**Document Status**: Complete
**Maintenance**: Update when script version changes or new commands added
**Contact**: See GitHub repository for support
