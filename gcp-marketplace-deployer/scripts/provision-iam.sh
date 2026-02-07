#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Starting IAM and service account provisioning..."

# Set defaults
default_env GKE_SA_NAME "${APP_NAME}-gke"
default_env APP_SA_NAME "${APP_NAME}-app"
default_env WORKLOAD_IDENTITY_ENABLED "true"

# Enable required APIs
enable_api "iam.googleapis.com"
enable_api "iamcredentials.googleapis.com"

# Function to create service account if it doesn't exist
create_service_account() {
    local sa_name=$1
    local display_name=$2
    local sa_email="${sa_name}@${PROJECT_ID}.iam.gserviceaccount.com"

    if ! resource_exists "service-account" "${sa_email}"; then
        log_info "Creating service account: ${sa_name}"
        gcloud iam service-accounts create "${sa_name}" \
            --project="${PROJECT_ID}" \
            --display-name="${display_name}"
        log_success "Service account created: ${sa_name}"
    else
        log_info "Service account already exists: ${sa_name}"
    fi

    echo "${sa_email}"
}

# Function to grant IAM role to service account
grant_iam_role() {
    local sa_email=$1
    local role=$2

    log_info "Granting role ${role} to ${sa_email}"
    gcloud projects add-iam-policy-binding "${PROJECT_ID}" \
        --member="serviceAccount:${sa_email}" \
        --role="${role}" \
        --condition=None \
        --quiet 2>/dev/null || log_warning "Role may already be granted"
}

# Create GKE service account
GKE_SA_EMAIL=$(create_service_account "${GKE_SA_NAME}" "GKE Cluster Service Account")

# Grant required roles for GKE
log_info "Configuring GKE service account permissions..."
grant_iam_role "${GKE_SA_EMAIL}" "roles/logging.logWriter"
grant_iam_role "${GKE_SA_EMAIL}" "roles/monitoring.metricWriter"
grant_iam_role "${GKE_SA_EMAIL}" "roles/monitoring.viewer"
grant_iam_role "${GKE_SA_EMAIL}" "roles/stackdriver.resourceMetadata.writer"

# Create application service account
APP_SA_EMAIL=$(create_service_account "${APP_SA_NAME}" "Application Service Account")

# Grant required roles for application
log_info "Configuring application service account permissions..."
grant_iam_role "${APP_SA_EMAIL}" "roles/cloudsql.client"
grant_iam_role "${APP_SA_EMAIL}" "roles/storage.objectViewer"

# Grant custom roles if specified
if [[ -n "${CUSTOM_IAM_ROLES:-}" ]]; then
    IFS=',' read -ra ROLES <<< "$CUSTOM_IAM_ROLES"
    for role in "${ROLES[@]}"; do
        grant_iam_role "${APP_SA_EMAIL}" "$role"
    done
fi

# Configure Workload Identity if enabled
if [[ "${WORKLOAD_IDENTITY_ENABLED}" == "true" ]]; then
    log_info "Configuring Workload Identity bindings..."

    # This will be completed after cluster creation
    # Store the service account emails for later use
    export GKE_SA_EMAIL
    export APP_SA_EMAIL

    log_info "Workload Identity will be configured after cluster creation"
fi

# Create secret with service account key for backward compatibility
log_info "Creating service account key for application..."
SA_KEY_FILE="/tmp/${APP_SA_NAME}-key.json"
gcloud iam service-accounts keys create "${SA_KEY_FILE}" \
    --iam-account="${APP_SA_EMAIL}" \
    --project="${PROJECT_ID}"

# Store key as base64 for later use in Kubernetes secret
export SA_KEY_BASE64=$(base64 -w 0 "${SA_KEY_FILE}")
rm -f "${SA_KEY_FILE}"

log_success "IAM and service accounts configured successfully"

# Export variables for other scripts
export GKE_SA_EMAIL
export APP_SA_EMAIL
export GKE_SA_NAME
export APP_SA_NAME
