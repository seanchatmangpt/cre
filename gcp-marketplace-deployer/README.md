# GCP Marketplace Deployer Container

This is a complete GCP Marketplace deployer container with automated provisioning for:
- Service Accounts & IAM
- GKE (Google Kubernetes Engine) Clusters
- Cloud SQL Databases
- VPC Networking & Security

## Overview

The deployer automates the entire infrastructure provisioning and application deployment process for GCP Marketplace applications, following Google Cloud's best practices for security, networking, and scalability.

## Architecture

```
┌─────────────────────────────────────────┐
│         GCP Marketplace UI              │
│         (schema.yaml)                   │
└────────────────┬────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────┐
│      Deployer Container                 │
│  ┌───────────────────────────────────┐  │
│  │  1. Validate Prerequisites        │  │
│  │  2. Provision Networking (VPC)    │  │
│  │  3. Configure IAM & Service Accts │  │
│  │  4. Create GKE Cluster            │  │
│  │  5. Provision Cloud SQL Database  │  │
│  │  6. Deploy Application            │  │
│  │  7. Configure Workload Identity   │  │
│  │  8. Setup Monitoring              │  │
│  └───────────────────────────────────┘  │
└─────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────┐
│        Provisioned Resources            │
│  • VPC Network with Private Subnets     │
│  • GKE Cluster (Private/Public)         │
│  • Cloud SQL (PostgreSQL)               │
│  • Service Accounts with IAM Roles      │
│  • Load Balancer & Cloud NAT            │
│  • Monitoring & Logging                 │
└─────────────────────────────────────────┘
```

## Directory Structure

```
gcp-marketplace-deployer/
├── Dockerfile                      # Deployer container image
├── schema.yaml                     # GCP Marketplace UI configuration
├── scripts/
│   ├── deploy.sh                  # Main deployment orchestration
│   ├── utils.sh                   # Utility functions
│   ├── validate.sh                # Prerequisites validation
│   ├── provision-networking.sh    # VPC, subnets, firewall rules
│   ├── provision-iam.sh           # Service accounts & IAM roles
│   ├── provision-gke.sh           # GKE cluster creation
│   ├── provision-database.sh      # Cloud SQL provisioning
│   ├── apply-manifests.sh         # Kubernetes resource deployment
│   ├── configure-workload-identity.sh  # Workload Identity setup
│   ├── post-deploy.sh             # Post-deployment configuration
│   └── display-info.sh            # Deployment summary
├── manifests/
│   └── application.yaml           # Application CRD
└── config/
    └── (additional configuration files)
```

## Features

### 🔐 Security
- **Workload Identity**: Secure authentication between GKE and GCP services
- **Private GKE Clusters**: Nodes without public IP addresses
- **IAM Roles**: Least-privilege service accounts
- **Network Policies**: Segmented network access
- **Secrets Management**: Kubernetes secrets for sensitive data

### 🌐 Networking
- **Custom VPC**: Isolated network infrastructure
- **Private Subnets**: IP aliasing for pods and services
- **Cloud NAT**: Outbound internet access for private nodes
- **Load Balancer**: Automatic external access configuration
- **Firewall Rules**: Secure ingress/egress policies

### 💾 Database
- **Cloud SQL**: Managed PostgreSQL database
- **Private IP**: VPC-native database connectivity
- **Automated Backups**: Point-in-time recovery
- **High Availability**: Multi-zone deployment option
- **Connection Pooling**: Cloud SQL Proxy integration

### ☸️ Kubernetes
- **Auto-scaling**: HPA for application scaling
- **Node Auto-scaling**: Cluster auto-scaling
- **Health Checks**: Liveness and readiness probes
- **Resource Limits**: CPU and memory management
- **Rolling Updates**: Zero-downtime deployments

## schema.yaml Configuration

The `schema.yaml` file defines the GCP Marketplace UI and configurable parameters:

### Main Configuration Sections

1. **GKE Configuration**
   - Cluster name
   - Node count and machine type
   - Auto-scaling settings
   - Region selection

2. **Database Configuration**
   - Instance tier
   - Database version (PostgreSQL)
   - Storage size
   - Backup settings

3. **Networking Configuration**
   - VPC name
   - Subnet CIDR ranges
   - Private cluster settings
   - Master IP ranges

4. **IAM Configuration**
   - Service account creation
   - Workload Identity enablement

5. **Application Configuration**
   - Replica count
   - Resource limits (CPU/memory)

## Building the Deployer

```bash
# Build the deployer image
docker build -t gcr.io/[PROJECT_ID]/[APP_NAME]/deployer:1.0 .

# Push to Container Registry
docker push gcr.io/[PROJECT_ID]/[APP_NAME]/deployer:1.0
```

## Testing Locally

```bash
# Set required environment variables
export PROJECT_ID="your-gcp-project"
export REGION="us-central1"
export ZONE="us-central1-a"
export APP_NAME="my-app"
export NAMESPACE="default"

# Run the deployer
docker run --rm \
  -e PROJECT_ID=$PROJECT_ID \
  -e REGION=$REGION \
  -e ZONE=$ZONE \
  -e APP_NAME=$APP_NAME \
  -e NAMESPACE=$NAMESPACE \
  -v ~/.config/gcloud:/root/.config/gcloud \
  gcr.io/[PROJECT_ID]/[APP_NAME]/deployer:1.0
```

## Deployment Flow

1. **Validation Phase**
   - Verify GCP project access
   - Check required APIs
   - Validate quotas
   - Confirm prerequisites

2. **Network Provisioning**
   - Create VPC network
   - Configure subnets with secondary ranges
   - Setup firewall rules
   - Configure Cloud NAT and Router

3. **IAM Configuration**
   - Create GKE service account
   - Create application service account
   - Grant necessary IAM roles
   - Setup Workload Identity bindings

4. **GKE Cluster Creation**
   - Provision cluster with specified configuration
   - Configure private nodes
   - Enable auto-scaling
   - Setup monitoring and logging

5. **Database Provisioning** (if enabled)
   - Create Cloud SQL instance
   - Configure private IP
   - Setup backups
   - Create database and users

6. **Application Deployment**
   - Create Kubernetes namespace
   - Apply ConfigMaps and Secrets
   - Deploy application
   - Create Services and Ingress

7. **Post-Deployment**
   - Configure Workload Identity
   - Setup monitoring and alerts
   - Run database migrations
   - Display access information

## Environment Variables

### Required
- `PROJECT_ID`: GCP project ID
- `APP_NAME`: Application name
- `NAMESPACE`: Kubernetes namespace

### Optional
- `REGION`: GCP region (default: us-central1)
- `ZONE`: GCP zone (default: us-central1-a)
- `CLUSTER_NAME`: GKE cluster name
- `NODE_COUNT`: Number of nodes (default: 3)
- `MACHINE_TYPE`: GCE machine type (default: n1-standard-4)
- `DATABASE_ENABLED`: Enable database (default: true)
- `WORKLOAD_IDENTITY_ENABLED`: Enable Workload Identity (default: true)

## Monitoring and Logging

The deployer automatically configures:
- **Cloud Monitoring**: Uptime checks and metrics
- **Cloud Logging**: Application and system logs
- **Log-based Metrics**: Custom metrics from logs
- **Alerting**: Email notifications for critical events

## Troubleshooting

### Check Deployment Status
```bash
kubectl get pods -n [NAMESPACE]
kubectl get svc -n [NAMESPACE]
kubectl logs -f -n [NAMESPACE] -l app=[APP_NAME]
```

### View Deployer Logs
```bash
kubectl logs -n [NAMESPACE] -l app.kubernetes.io/component=deployer
```

### Common Issues

1. **Quota Exceeded**: Request quota increase in GCP Console
2. **API Not Enabled**: Run `gcloud services enable [API_NAME]`
3. **Permission Denied**: Verify IAM roles for service accounts
4. **Cluster Creation Failed**: Check network configuration and quotas

## Security Best Practices

1. **Use Private Clusters**: Enable `ENABLE_PRIVATE_CLUSTER=true`
2. **Enable Workload Identity**: Set `WORKLOAD_IDENTITY_ENABLED=true`
3. **Restrict Network Access**: Use custom firewall rules
4. **Regular Updates**: Keep GKE version updated
5. **Secrets Management**: Never hardcode credentials
6. **Audit Logging**: Enable Cloud Audit Logs
7. **Least Privilege**: Grant minimum required IAM roles

## Customization

### Adding Custom Resources

1. Add Kubernetes manifests to `manifests/` directory
2. Update `apply-manifests.sh` to include new resources
3. Update `schema.yaml` for UI configuration

### Custom Initialization

1. Create custom scripts in `scripts/`
2. Call from `post-deploy.sh`
3. Make scripts executable

### Database Migrations

1. Create `scripts/db-migrate.sh`
2. Add migration logic
3. Will be automatically executed during deployment

## Support

For issues or questions:
- GitHub: https://github.com/your-org/your-app
- Email: support@yourcompany.com
- Documentation: https://docs.yourcompany.com

## License

Copyright © 2024 Your Company. All rights reserved.
