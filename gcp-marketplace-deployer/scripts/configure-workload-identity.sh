#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Configuring Workload Identity..."

# Skip if not enabled
if [[ "${WORKLOAD_IDENTITY_ENABLED:-true}" != "true" ]]; then
    log_info "Workload Identity is not enabled, skipping..."
    exit 0
fi

# Create Kubernetes service account
log_info "Creating Kubernetes service account..."
kubectl create serviceaccount "${APP_SA_NAME}" \
    --namespace="${NAMESPACE}" \
    --dry-run=client -o yaml | kubectl apply -f -

# Annotate Kubernetes service account
log_info "Annotating Kubernetes service account for Workload Identity..."
kubectl annotate serviceaccount "${APP_SA_NAME}" \
    --namespace="${NAMESPACE}" \
    "iam.gke.io/gcp-service-account=${APP_SA_EMAIL}" \
    --overwrite

# Bind GCP service account to Kubernetes service account
log_info "Binding GCP service account to Kubernetes service account..."
gcloud iam service-accounts add-iam-policy-binding "${APP_SA_EMAIL}" \
    --project="${PROJECT_ID}" \
    --role="roles/iam.workloadIdentityUser" \
    --member="serviceAccount:${PROJECT_ID}.svc.id.goog[${NAMESPACE}/${APP_SA_NAME}]" \
    --quiet 2>/dev/null || log_warning "Binding may already exist"

# Update deployment to use Workload Identity
log_info "Updating deployment to use Workload Identity..."
kubectl patch deployment "${APP_NAME}" \
    --namespace="${NAMESPACE}" \
    --type='json' \
    -p='[{
        "op": "add",
        "path": "/spec/template/spec/serviceAccountName",
        "value": "'"${APP_SA_NAME}"'"
    }]' || log_warning "Could not patch deployment"

log_success "Workload Identity configured successfully"
log_info "GCP SA: ${APP_SA_EMAIL}"
log_info "K8s SA: ${APP_SA_NAME} (namespace: ${NAMESPACE})"
