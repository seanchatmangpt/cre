# Quick Start Guide

This guide will help you quickly build, test, and deploy the GCP Marketplace deployer container.

## Prerequisites

Before you begin, ensure you have:

- [ ] Google Cloud Platform account with billing enabled
- [ ] `gcloud` CLI installed and authenticated
- [ ] `docker` installed
- [ ] `kubectl` installed
- [ ] A GCP project with appropriate permissions

## Installation

### 1. Install Required Tools

```bash
# Install gcloud SDK (if not already installed)
curl https://sdk.cloud.google.com | bash
exec -l $SHELL

# Install kubectl
gcloud components install kubectl

# Authenticate with GCP
gcloud auth login
gcloud auth application-default login
```

### 2. Set Up Your GCP Project

```bash
# Set your project ID
export PROJECT_ID="your-gcp-project-id"
gcloud config set project $PROJECT_ID

# Enable required APIs (takes 2-3 minutes)
make enable-apis

# Or manually:
gcloud services enable \
    compute.googleapis.com \
    container.googleapis.com \
    sqladmin.googleapis.com \
    iam.googleapis.com \
    servicenetworking.googleapis.com
```

## Quick Start (5 Minutes)

### Option 1: Using Make (Recommended)

```bash
# 1. Validate prerequisites
make validate

# 2. Build the deployer image
make build

# 3. Push to Google Container Registry
make push

# 4. Test locally (dry run)
make test-local
```

### Option 2: Manual Steps

```bash
# 1. Set variables
export PROJECT_ID="your-gcp-project-id"
export APP_NAME="my-marketplace-app"
export APP_VERSION="1.0.0"

# 2. Build the deployer
docker build -t gcr.io/$PROJECT_ID/$APP_NAME/deployer:$APP_VERSION .

# 3. Configure Docker for GCR
gcloud auth configure-docker

# 4. Push the image
docker push gcr.io/$PROJECT_ID/$APP_NAME/deployer:$APP_VERSION
```

## Testing the Deployer

### Local Test (Recommended for Development)

```bash
# Copy example environment file
cp config/default.env.example .env

# Edit .env with your configuration
vim .env

# Run deployer locally
make test-local
```

### Deploy to GCP

```bash
# Set environment variables
export PROJECT_ID="your-gcp-project-id"
export REGION="us-central1"
export APP_NAME="my-app"

# Run the deployer
docker run --rm \
  -e PROJECT_ID=$PROJECT_ID \
  -e REGION=$REGION \
  -e APP_NAME=$APP_NAME \
  -e NAMESPACE=default \
  -v ~/.config/gcloud:/root/.config/gcloud \
  gcr.io/$PROJECT_ID/$APP_NAME/deployer:latest
```

## What Gets Provisioned?

When you run the deployer, it automatically creates:

### 🌐 Networking (2-3 minutes)
- Custom VPC network
- Private subnet with secondary IP ranges
- Firewall rules
- Cloud NAT and Router

### 🔐 IAM & Security (1-2 minutes)
- GKE service account
- Application service account
- IAM role bindings
- Workload Identity configuration

### ☸️ GKE Cluster (5-10 minutes)
- Private GKE cluster
- Auto-scaling node pool
- Monitoring and logging enabled
- Workload Identity enabled

### 💾 Database (5-8 minutes) - Optional
- Cloud SQL (PostgreSQL) instance
- Private IP configuration
- Automated backups
- Database and user creation

### 📦 Application Deployment (1-2 minutes)
- Kubernetes namespace
- ConfigMaps and Secrets
- Deployment with auto-scaling
- LoadBalancer service

**Total Time: ~15-25 minutes** (depending on options)

## Verify Deployment

```bash
# Check cluster
gcloud container clusters list --project=$PROJECT_ID

# Get cluster credentials
gcloud container clusters get-credentials <cluster-name> \
  --region=$REGION \
  --project=$PROJECT_ID

# Check pods
kubectl get pods -n default

# Check service (wait for external IP)
kubectl get svc -n default

# View logs
kubectl logs -f -l app=my-app -n default
```

## Access Your Application

```bash
# Get external IP
EXTERNAL_IP=$(kubectl get svc my-app -n default \
  -o jsonpath='{.status.loadBalancer.ingress[0].ip}')

echo "Application URL: http://$EXTERNAL_IP"

# Test the application
curl http://$EXTERNAL_IP/healthz
```

## Clean Up

To avoid incurring charges, clean up the resources:

```bash
# Delete GKE cluster
gcloud container clusters delete <cluster-name> \
  --region=$REGION \
  --project=$PROJECT_ID

# Delete Cloud SQL instance
gcloud sql instances delete <instance-name> \
  --project=$PROJECT_ID

# Delete VPC network (delete firewall rules first)
gcloud compute firewall-rules list --filter="network:<vpc-name>"
gcloud compute firewall-rules delete <firewall-rule-name>
gcloud compute networks delete <vpc-name> --project=$PROJECT_ID
```

## Common Issues

### Issue: "Insufficient quota"
**Solution:** Request quota increase in GCP Console
```bash
# Check current quotas
gcloud compute project-info describe --project=$PROJECT_ID
```

### Issue: "API not enabled"
**Solution:** Enable required APIs
```bash
make enable-apis
```

### Issue: "Permission denied"
**Solution:** Grant necessary IAM roles
```bash
# Grant yourself necessary roles
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="user:your-email@example.com" \
  --role="roles/owner"
```

### Issue: "Cluster creation failed"
**Solution:** Check logs and network configuration
```bash
# View detailed logs
gcloud logging read "resource.type=gke_cluster" --limit 50
```

## Next Steps

1. **Customize the Deployment**
   - Edit `schema.yaml` for custom parameters
   - Modify provisioning scripts in `scripts/`
   - Add custom Kubernetes manifests

2. **Set Up CI/CD**
   ```bash
   # Configure Cloud Build
   gcloud builds submit --config=cloudbuild.yaml
   ```

3. **Submit to GCP Marketplace**
   ```bash
   # Package for submission
   make package
   ```

4. **Add Monitoring**
   - Configure Cloud Monitoring alerts
   - Set up custom dashboards
   - Enable Cloud Logging

## Resources

- [GCP Marketplace Documentation](https://cloud.google.com/marketplace/docs)
- [GKE Best Practices](https://cloud.google.com/kubernetes-engine/docs/best-practices)
- [Cloud SQL Documentation](https://cloud.google.com/sql/docs)
- [Workload Identity Guide](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)

## Support

For issues or questions:
- Review the [README.md](README.md)
- Check the [troubleshooting guide](README.md#troubleshooting)
- Open an issue on GitHub

## Configuration Reference

### Minimal Configuration
```bash
export PROJECT_ID="my-project"
export APP_NAME="my-app"
export NAMESPACE="default"
```

### Full Configuration
See `config/default.env.example` for all available options.

### Schema.yaml Configuration
The `schema.yaml` file controls the GCP Marketplace UI and supports:
- GKE cluster configuration
- Database settings
- Network configuration
- IAM settings
- Application parameters

Edit this file to customize the deployment UI.

---

**Ready to deploy?** Run `make build && make push` to get started!
