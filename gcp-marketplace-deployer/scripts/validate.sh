#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Validating prerequisites..."

# Check required commands
REQUIRED_COMMANDS=("gcloud" "kubectl" "jq")
for cmd in "${REQUIRED_COMMANDS[@]}"; do
    if ! command_exists "$cmd"; then
        log_error "Required command not found: $cmd"
        exit 1
    fi
    log_success "Found: $cmd"
done

# Validate PROJECT_ID
require_env "PROJECT_ID"
log_success "Project ID: ${PROJECT_ID}"

# Verify project exists and user has access
if ! gcloud projects describe "${PROJECT_ID}" &>/dev/null; then
    log_error "Cannot access project: ${PROJECT_ID}"
    log_error "Please verify the project exists and you have appropriate permissions"
    exit 1
fi
log_success "Project access verified"

# Check if user is authenticated
if ! gcloud auth list --filter=status:ACTIVE --format="value(account)" | grep -q .; then
    log_error "No active gcloud authentication found"
    log_error "Please run: gcloud auth login"
    exit 1
fi
log_success "Authentication verified"

# Validate region
if ! gcloud compute regions describe "${REGION}" --project="${PROJECT_ID}" &>/dev/null; then
    log_error "Invalid region: ${REGION}"
    exit 1
fi
log_success "Region verified: ${REGION}"

# Check required APIs are enabled or can be enabled
log_info "Checking required GCP APIs..."
REQUIRED_APIS=(
    "compute.googleapis.com"
    "container.googleapis.com"
    "iam.googleapis.com"
    "sqladmin.googleapis.com"
    "servicenetworking.googleapis.com"
)

for api in "${REQUIRED_APIS[@]}"; do
    if gcloud services list --enabled --project="${PROJECT_ID}" --filter="name:${api}" --format="value(name)" | grep -q "${api}"; then
        log_success "API enabled: ${api}"
    else
        log_warning "API not enabled: ${api} (will be enabled during deployment)"
    fi
done

# Check quotas (basic check)
log_info "Checking resource quotas..."
REQUIRED_CPUS=12  # For 3 n1-standard-4 nodes
CURRENT_CPUS=$(gcloud compute project-info describe --project="${PROJECT_ID}" \
    --format="value(quotas.filter(metric:CPUS).limit)" 2>/dev/null | head -1 || echo "0")

if [[ ${CURRENT_CPUS} -lt ${REQUIRED_CPUS} ]]; then
    log_warning "CPU quota may be insufficient. Required: ${REQUIRED_CPUS}, Available: ${CURRENT_CPUS}"
    log_warning "You may need to request a quota increase"
else
    log_success "Sufficient CPU quota available"
fi

# Validate configuration parameters
if [[ -n "${SUBNET_CIDR:-}" ]]; then
    if ! echo "${SUBNET_CIDR}" | grep -qE '^([0-9]{1,3}\.){3}[0-9]{1,3}/[0-9]{1,2}$'; then
        log_error "Invalid SUBNET_CIDR format: ${SUBNET_CIDR}"
        exit 1
    fi
    log_success "SUBNET_CIDR format valid"
fi

log_success "All prerequisites validated successfully"
