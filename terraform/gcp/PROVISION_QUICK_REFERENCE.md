# provision.sh Quick Reference

Fast lookup guide for the Terraform provisioning script.

## Basic Commands

```bash
# Full workflow (init → plan → apply)
./provision.sh --project my-project --bucket tf-state

# Plan only (review changes)
./provision.sh --project my-project plan

# Apply existing plan
./provision.sh --auto-approve apply

# View outputs
./provision.sh output

# Destroy everything
./provision.sh destroy
```

## Configuration Flags

| Flag | Usage | Example |
|------|-------|---------|
| `--project` | GCP project ID | `--project my-gcp-project` |
| `--bucket` | GCS state bucket | `--bucket my-tf-state` |
| `--prefix` | State file prefix | `--prefix gcp/production` |
| `--region` | GCP region | `--region us-east1` |
| `--environment` | Environment | `--environment staging` |
| `--auto-approve` | Skip confirmations | (flag, no value) |
| `--backup` | Enable state backup | (default) |
| `--no-backup` | Skip state backup | (flag, no value) |
| `--dry-run` | Preview only | (flag, no value) |
| `--verbose` | Debug output | (flag, no value) |
| `--json` | JSON output | (flag, no value) |

## All Commands

| Command | Purpose |
|---------|---------|
| `full` | Init → plan → apply (default) |
| `init` | Initialize backend and providers |
| `plan` | Generate execution plan |
| `apply` | Apply Terraform changes |
| `destroy` | Destroy all resources |
| `output` | Display cluster details |
| `validate` | Validate configuration |
| `fmt-check` | Check code formatting |
| `state-backup` | Backup current state |
| `state-restore` | Restore from backup |

## Common Workflows

### First-Time Setup

```bash
cd terraform/gcp

# 1. Prepare configuration
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars with your values

# 2. Run full provisioning
./provision.sh \
  --project my-gcp-project \
  --bucket cre-terraform-state \
  --region us-central1

# 3. Configure kubectl
gcloud container clusters regional get-credentials cre-cluster --region us-central1

# 4. Deploy application
kubectl apply -f ../../k8s/gcp/
```

### Plan Before Apply

```bash
# 1. Review plan
./provision.sh \
  --project my-gcp-project \
  --bucket cre-terraform-state \
  plan

# 2. Inspect terraform.tfplan and review output

# 3. Apply when ready
./provision.sh --auto-approve apply
```

### Manual Backup and Restore

```bash
# Backup state
./provision.sh --bucket cre-terraform-state state-backup

# List backups
ls -lh .terraform-state-backups/

# Restore if needed
./provision.sh state-restore .terraform-state-backups/terraform.tfstate.*.backup.gz
```

### Development vs Production

```bash
# Dev environment
./provision.sh \
  --project cre-dev \
  --bucket cre-terraform-state \
  --prefix gcp/development \
  --environment dev \
  --region us-east1

# Production environment
./provision.sh \
  --project cre-prod \
  --bucket cre-terraform-state \
  --prefix gcp/production \
  --environment production \
  --region us-central1
```

## Output Values

After successful deployment, retrieve:

```bash
# GKE cluster endpoint
terraform output -raw gke_cluster_endpoint

# Cluster name
terraform output -raw gke_cluster_name

# Cluster access command
terraform output -raw cluster_access_command

# Service account emails
terraform output security.service_accounts

# Load balancer IPs
terraform output -raw internal_lb_ip
terraform output -raw external_lb_ip

# All outputs as JSON
terraform output -json
```

## Environment Variables

```bash
# Alternative to command-line flags
export TF_VAR_project_id="my-project"
export TF_VAR_region="us-central1"
export TF_VAR_environment="production"
export TERRAFORM_STATE_BUCKET="my-tf-state"
export TERRAFORM_STATE_PREFIX="gcp/production"

# Then run without flags
./provision.sh
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Validation error |
| 3 | Init failed |
| 4 | Plan failed |
| 5 | Apply failed |
| 6 | State backup failed |
| 7 | Credentials error |

## Troubleshooting

```bash
# Validate configuration
./provision.sh validate

# Check formatting
./provision.sh fmt-check

# Verbose output
./provision.sh --verbose full

# Dry-run preview
./provision.sh --dry-run full

# Reset Terraform state
rm -rf .terraform/
./provision.sh --project my-project --bucket my-bucket init

# Check GCS backend
gsutil ls gs://my-tf-state/gcp/production/

# Refresh state
terraform refresh
```

## File Locations

```
terraform/gcp/
├── provision.sh                (This script)
├── PROVISION_QUICK_REFERENCE.md (This file)
├── main.tf                      (Root module)
├── variables.tf                 (Input variables)
├── outputs.tf                   (Output values)
├── versions.tf                  (Provider config)
├── terraform.tfplan             (Generated plan file)
└── modules/
    ├── gke_cluster/
    ├── vpc/
    ├── storage/
    ├── loadbalancer/
    ├── backup/
    ├── monitoring/
    └── security/
```

## Key Variables

Edit `terraform.tfvars`:

```hcl
project_id = "my-gcp-project"
region     = "us-central1"
environment = "production"

# Optional: customize these
vpc_config = { ... }
gke_config = { ... }
storage_config = { ... }
lb_config = { ... }
```

## Example: Minimal Usage

```bash
# Assuming terraform.tfvars exists with project_id set:
./provision.sh --bucket my-tf-state

# Or with environment variables:
export TF_VAR_project_id="my-project"
export TERRAFORM_STATE_BUCKET="my-tf-state"
./provision.sh
```

## Pre-requisites Checklist

- [ ] Terraform >= 1.5.0 installed
- [ ] gcloud CLI installed and authenticated
- [ ] GCS bucket created for state
- [ ] GCP project with APIs enabled
- [ ] terraform.tfvars configured with project_id
- [ ] Appropriate IAM permissions on service account

## Next Steps

1. Run full provisioning: `./provision.sh --project X --bucket Y`
2. View outputs: `./provision.sh output`
3. Configure kubectl: `eval $(terraform output -raw cluster_access_command)`
4. Deploy application: `kubectl apply -f ../../k8s/gcp/`

---

For detailed documentation, see [TERRAFORM_PROVISIONING.md](../../docs/gcp/TERRAFORM_PROVISIONING.md)
