#!/bin/bash
set -euo pipefail

# GCP Marketplace Deployer - Main Deployment Script
# This script orchestrates the entire deployment process

echo "=================================================="
echo "GCP Marketplace Automated Provisioning"
echo "=================================================="

# Source utility functions
source /scripts/utils.sh

# Get configuration from environment variables
export PROJECT_ID="${GOOGLE_CLOUD_PROJECT:-}"
export CLUSTER_NAME="${GKE_CLUSTER_NAME:-}"
export REGION="${GCP_REGION:-us-central1}"
export ZONE="${GCP_ZONE:-us-central1-a}"
export NAMESPACE="${NAMESPACE:-default}"
export APP_NAME="${APP_NAME:-marketplace-app}"

log_info "Starting deployment for project: ${PROJECT_ID}"
log_info "Region: ${REGION}, Zone: ${ZONE}"
log_info "Application: ${APP_NAME} in namespace: ${NAMESPACE}"

# Step 1: Validate prerequisites
log_section "Validating Prerequisites"
/scripts/validate.sh

# Step 2: Set up networking
log_section "Provisioning Network Infrastructure"
/scripts/provision-networking.sh

# Step 3: Create service accounts and configure IAM
log_section "Configuring IAM and Service Accounts"
/scripts/provision-iam.sh

# Step 4: Provision GKE cluster
log_section "Provisioning GKE Cluster"
/scripts/provision-gke.sh

# Step 5: Provision database (if enabled)
if [[ "${DATABASE_ENABLED:-true}" == "true" ]]; then
    log_section "Provisioning Cloud SQL Database"
    /scripts/provision-database.sh
fi

# Step 6: Configure cluster connectivity
log_section "Configuring Cluster Access"
gcloud container clusters get-credentials "${CLUSTER_NAME}" \
    --region="${REGION}" \
    --project="${PROJECT_ID}"

# Step 7: Create namespace if it doesn't exist
log_info "Creating namespace: ${NAMESPACE}"
kubectl create namespace "${NAMESPACE}" --dry-run=client -o yaml | kubectl apply -f -

# Step 8: Apply Kubernetes manifests
log_section "Deploying Application Manifests"
/scripts/apply-manifests.sh

# Step 9: Configure Workload Identity (if enabled)
if [[ "${WORKLOAD_IDENTITY_ENABLED:-true}" == "true" ]]; then
    log_section "Configuring Workload Identity"
    /scripts/configure-workload-identity.sh
fi

# Step 10: Wait for deployment to be ready
log_section "Waiting for Deployment"
kubectl rollout status deployment/"${APP_NAME}" -n "${NAMESPACE}" --timeout=600s

# Step 11: Post-deployment configuration
log_section "Post-Deployment Configuration"
/scripts/post-deploy.sh

# Step 12: Display access information
log_section "Deployment Complete"
/scripts/display-info.sh

log_success "Deployment completed successfully!"
log_info "Application URL: $(kubectl get svc ${APP_NAME} -n ${NAMESPACE} -o jsonpath='{.status.loadBalancer.ingress[0].ip}')"

echo "=================================================="
