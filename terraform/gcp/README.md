# CRE GCP Terraform Deployment

Production-ready Terraform configuration for deploying CRE (Common Runtime Environment) to Google Cloud Platform.

## Architecture

This configuration deploys:

- **VPC**: Private network with subnets, Cloud NAT, and firewall rules for Erlang distribution
- **GKE Cluster**: Regional private cluster with node pools for general and memory-optimized workloads
- **Storage**: StorageClasses for SSD, regional SSD, and balanced disks; PVCs for Mnesia and data
- **Load Balancers**: Internal (regional) and external (global) with Cloud Armor protection

## Prerequisites

1. **GCP Project** with the following APIs enabled:
   - Kubernetes Engine API
   - Compute Engine API
   - Cloud Resource Manager API
   - Cloud Storage API (for state backend)

2. **Terraform** >= 1.5.0

3. **gcloud** CLI configured with appropriate credentials

## Quick Start

1. **Configure backend** (edit `versions.tf`):
   ```hcl
   backend "gcs" {
     bucket = "your-terraform-state-bucket"
     prefix = "gcp/production"
   }
   ```

2. **Copy and customize variables**:
   ```bash
   cp terraform.tfvars.example terraform.tfvars
   # Edit terraform.tfvars with your project_id and other settings
   ```

3. **Initialize Terraform**:
   ```bash
   terraform init
   terraform validate
   ```

4. **Plan and apply**:
   ```bash
   terraform plan
   terraform apply
   ```

## Post-Deployment Steps

1. **Configure kubectl**:
   ```bash
   gcloud container clusters regional get-credentials cre-cluster --region us-central1
   ```

2. **Apply StorageClasses**:
   ```bash
   kubectl apply -f modules/storage/storage-classes.yaml.tpl
   ```

3. **Apply PVCs**:
   ```bash
   kubectl apply -f modules/storage/pvcs.yaml.tpl
   ```

## Module Structure

```
terraform/gcp/
├── main.tf                   # Root module
├── variables.tf              # Root variables
├── outputs.tf                # Root outputs
├── versions.tf               # Provider versions and backend
├── terraform.tfvars.example  # Example variables
├── modules/
│   ├── gke_cluster/          # GKE cluster module
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   └── versions.tf
│   ├── vpc/                  # VPC module
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   └── versions.tf
│   ├── storage/              # Storage module
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   ├── versions.tf
│   │   ├── storage-classes.yaml.tpl
│   │   └── pvcs.yaml.tpl
│   └── loadbalancer/         # Load balancer module
│       ├── main.tf
│       ├── variables.tf
│       ├── outputs.tf
│       └── versions.tf
```

## Erlang-Specific Configuration

The firewall rules are configured for Erlang/OTP distributed systems:

- **Port 4369**: EPMD (Erlang Port Mapper Daemon)
- **Ports 9100-9200**: Erlang distribution for node communication
- **UDP 9100-9200**: Erlang distribution UDP traffic

## Security Features

- Private GKE cluster (no public endpoint)
- Cloud NAT for egress traffic
- Cloud Armor WAF protection
- Network policies with Calico
- Shielded nodes
- Binary authorization

## Outputs

After deployment, you'll get:

- GKE cluster endpoint and CA certificate
- Internal/external load balancer IPs
- Subnet information
- Commands for kubectl configuration
