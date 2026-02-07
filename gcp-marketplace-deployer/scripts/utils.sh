#!/bin/bash
# Utility functions for GCP deployment scripts

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_section() {
    echo ""
    echo "=================================================="
    echo -e "${GREEN}$1${NC}"
    echo "=================================================="
}

# Error handling
handle_error() {
    log_error "Command failed: $1"
    log_error "Line: $2"
    exit 1
}

trap 'handle_error "$BASH_COMMAND" "$LINENO"' ERR

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Wait for resource to be ready
wait_for_resource() {
    local resource_type=$1
    local resource_name=$2
    local timeout=${3:-300}

    log_info "Waiting for ${resource_type}/${resource_name} to be ready..."
    kubectl wait --for=condition=ready "${resource_type}/${resource_name}" \
        --timeout="${timeout}s" 2>/dev/null || true
}

# Check if GCP resource exists
resource_exists() {
    local resource_type=$1
    local resource_name=$2

    case $resource_type in
        "vpc")
            gcloud compute networks describe "${resource_name}" \
                --project="${PROJECT_ID}" &>/dev/null
            ;;
        "subnet")
            gcloud compute networks subnets describe "${resource_name}" \
                --region="${REGION}" \
                --project="${PROJECT_ID}" &>/dev/null
            ;;
        "cluster")
            gcloud container clusters describe "${resource_name}" \
                --region="${REGION}" \
                --project="${PROJECT_ID}" &>/dev/null
            ;;
        "service-account")
            gcloud iam service-accounts describe "${resource_name}" \
                --project="${PROJECT_ID}" &>/dev/null
            ;;
        "database")
            gcloud sql instances describe "${resource_name}" \
                --project="${PROJECT_ID}" &>/dev/null
            ;;
        *)
            log_error "Unknown resource type: ${resource_type}"
            return 1
            ;;
    esac
}

# Generate random suffix for resource names
generate_suffix() {
    echo $(date +%s | sha256sum | base64 | head -c 8 | tr '[:upper:]' '[:lower:]')
}

# Get current timestamp
get_timestamp() {
    date +"%Y-%m-%d %H:%M:%S"
}

# Validate required environment variable
require_env() {
    local var_name=$1
    local var_value="${!var_name:-}"

    if [[ -z "$var_value" ]]; then
        log_error "Required environment variable not set: ${var_name}"
        exit 1
    fi
}

# Set default value for environment variable
default_env() {
    local var_name=$1
    local default_value=$2

    if [[ -z "${!var_name:-}" ]]; then
        export "$var_name"="$default_value"
        log_info "Using default value for ${var_name}: ${default_value}"
    fi
}

# Enable GCP API
enable_api() {
    local api=$1
    log_info "Enabling API: ${api}"
    gcloud services enable "${api}" --project="${PROJECT_ID}" 2>/dev/null || true
}

# Retry command with exponential backoff
retry_command() {
    local max_attempts=${1:-5}
    local delay=${2:-5}
    local command="${@:3}"
    local attempt=1

    while [[ $attempt -le $max_attempts ]]; do
        log_info "Attempt ${attempt}/${max_attempts}: ${command}"
        if eval "$command"; then
            return 0
        fi

        if [[ $attempt -lt $max_attempts ]]; then
            log_warning "Command failed, retrying in ${delay}s..."
            sleep "$delay"
            delay=$((delay * 2))
        fi

        attempt=$((attempt + 1))
    done

    log_error "Command failed after ${max_attempts} attempts"
    return 1
}

# Create secret from environment variable
create_secret_from_env() {
    local secret_name=$1
    local env_var=$2
    local namespace=${3:-$NAMESPACE}

    if [[ -n "${!env_var:-}" ]]; then
        kubectl create secret generic "${secret_name}" \
            --from-literal=value="${!env_var}" \
            --namespace="${namespace}" \
            --dry-run=client -o yaml | kubectl apply -f -
        log_success "Created secret: ${secret_name}"
    fi
}

# Export function for use in subshells
export -f log_info log_success log_warning log_error log_section
export -f command_exists wait_for_resource resource_exists
export -f generate_suffix get_timestamp require_env default_env
export -f enable_api retry_command create_secret_from_env
