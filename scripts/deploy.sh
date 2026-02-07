#!/bin/bash

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Default values
ENVIRONMENT="${ENVIRONMENT:-staging}"
DRY_RUN="${DRY_RUN:-false}"
FORCE_DEPLOY="${FORCE_DEPLOY:-false}"
WAIT_FOR_ROLLOUT="${WAIT_FOR_ROLLOUT:-true}"
TIMEOUT="${TIMEOUT:-300}"

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -e|--environment)
                ENVIRONMENT="$2"
                shift 2
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --force)
                FORCE_DEPLOY=true
                shift
                ;;
            --no-wait)
                WAIT_FOR_ROLLOUT=false
                shift
                ;;
            -h|--help)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

# Show help
show_help() {
    cat << EOF
Usage: $0 [OPTIONS]

Deploy application to specified environment.

OPTIONS:
    -e, --environment ENV   Target environment (staging|production) [default: staging]
    --dry-run               Show what would be deployed without making changes
    --force                 Force deployment even if checks fail
    --no-wait               Don't wait for rollout to complete
    -h, --help              Show this help message

EXAMPLES:
    # Deploy to staging
    $0 -e staging

    # Deploy to production with dry-run
    $0 -e production --dry-run

    # Force deploy to production
    $0 -e production --force

EOF
}

# Validate environment
validate_environment() {
    case "$ENVIRONMENT" in
        staging|production)
            log_info "Target environment: $ENVIRONMENT"
            ;;
        *)
            log_error "Invalid environment: $ENVIRONMENT"
            exit 1
            ;;
    esac
}

# Setup credentials
setup_credentials() {
    log_info "Setting up credentials..."

    if [ -z "${KUBECONFIG:-}" ] && [ -z "${CLOUDSDK_CONTAINER_CLUSTER:-}" ]; then
        log_error "Either KUBECONFIG or CLOUDSDK_CONTAINER_CLUSTER must be set"
        exit 1
    fi

    # Test kubectl access
    if ! kubectl cluster-info &> /dev/null; then
        log_error "Cannot access Kubernetes cluster"
        exit 1
    fi

    log_success "Credentials configured"
}

# Get deployment configuration path
get_config_path() {
    local config_path="$PROJECT_DIR/config/$ENVIRONMENT"

    if [ ! -d "$config_path" ]; then
        log_error "Configuration directory not found: $config_path"
        exit 1
    fi

    echo "$config_path"
}

# Validate configuration
validate_config() {
    local config_path="$1"
    log_info "Validating configuration..."

    # Check for required files
    if [ ! -f "$config_path/deployment.yaml" ]; then
        log_error "deployment.yaml not found in $config_path"
        exit 1
    fi

    # Validate YAML syntax
    for yaml_file in "$config_path"/*.yaml; do
        if ! kubectl apply -f "$yaml_file" --dry-run=client &> /dev/null; then
            log_error "Invalid YAML in $yaml_file"
            exit 1
        fi
    done

    log_success "Configuration validated"
}

# Pre-deployment checks
pre_deployment_checks() {
    log_info "Running pre-deployment checks..."

    # Check resource availability
    local nodes=$(kubectl get nodes --no-headers 2>/dev/null | wc -l)
    if [ "$nodes" -lt 1 ]; then
        log_error "No Kubernetes nodes available"
        return 1
    fi
    log_info "Available nodes: $nodes"

    # Check namespace
    local namespace
    if [ "$ENVIRONMENT" = "staging" ]; then
        namespace="staging"
    else
        namespace="production"
    fi

    if ! kubectl get namespace "$namespace" &> /dev/null; then
        log_warning "Namespace '$namespace' does not exist, creating..."
        kubectl create namespace "$namespace"
    fi

    # Check storage availability
    local pvc_status=$(kubectl get pvc -n "$namespace" 2>/dev/null | wc -l)
    log_info "Storage claims: $pvc_status"

    # Check resource quotas
    if kubectl get resourcequota -n "$namespace" &> /dev/null; then
        log_info "Resource quotas:"
        kubectl describe resourcequota -n "$namespace" | head -20
    fi

    log_success "Pre-deployment checks passed"
    return 0
}

# Perform deployment
perform_deployment() {
    local config_path="$1"
    local namespace
    local deployment_name="admin-console"

    if [ "$ENVIRONMENT" = "staging" ]; then
        namespace="staging"
    else
        namespace="production"
    fi

    log_info "Deploying to namespace: $namespace"

    # Apply RBAC
    if [ -f "$PROJECT_DIR/config/rbac.yaml" ]; then
        log_info "Applying RBAC configuration..."
        if [ "$DRY_RUN" = "true" ]; then
            kubectl apply -f "$PROJECT_DIR/config/rbac.yaml" -n "$namespace" --dry-run=client -o yaml
        else
            kubectl apply -f "$PROJECT_DIR/config/rbac.yaml" -n "$namespace"
        fi
    fi

    # Apply ConfigMap
    if [ -f "$PROJECT_DIR/config/configmap.yaml" ]; then
        log_info "Applying ConfigMap..."
        if [ "$DRY_RUN" = "true" ]; then
            kubectl apply -f "$PROJECT_DIR/config/configmap.yaml" -n "$namespace" --dry-run=client -o yaml
        else
            kubectl apply -f "$PROJECT_DIR/config/configmap.yaml" -n "$namespace"
        fi
    fi

    # Apply deployment configuration
    log_info "Applying deployment configuration..."
    if [ "$DRY_RUN" = "true" ]; then
        kubectl apply -f "$config_path/deployment.yaml" --dry-run=client -o yaml
        log_success "Dry-run completed successfully"
        return 0
    else
        kubectl apply -f "$config_path/deployment.yaml"
        log_success "Deployment configuration applied"
    fi

    # Get deployment info
    log_info "Deployment status:"
    kubectl get deployment "$deployment_name" -n "$namespace"

    return 0
}

# Wait for rollout
wait_rollout() {
    local namespace="$1"
    local deployment_name="admin-console"

    if [ "$WAIT_FOR_ROLLOUT" = "false" ]; then
        log_info "Skipping rollout wait (--no-wait specified)"
        return 0
    fi

    log_info "Waiting for deployment rollout (timeout: ${TIMEOUT}s)..."

    if kubectl rollout status deployment/"$deployment_name" \
        -n "$namespace" \
        --timeout="${TIMEOUT}s"; then
        log_success "Deployment rollout completed successfully"
        return 0
    else
        log_error "Deployment rollout failed or timed out"
        return 1
    fi
}

# Post-deployment verification
post_deployment_verification() {
    local namespace="$1"
    log_info "Performing post-deployment verification..."

    # Check pod status
    log_info "Pod status:"
    kubectl get pods -n "$namespace" -l app=admin-console -o wide

    # Check service
    log_info "Service status:"
    kubectl get svc -n "$namespace" -l app=admin-console

    # Show recent events
    log_info "Recent events:"
    kubectl get events -n "$namespace" --sort-by='.lastTimestamp' | tail -10

    # Check resource usage
    log_info "Resource usage:"
    kubectl top pods -n "$namespace" -l app=admin-console 2>/dev/null || log_warning "Metrics server not available"

    log_success "Post-deployment verification complete"
}

# Rollback on failure
rollback_on_failure() {
    local namespace="$1"
    log_error "Deployment failed, initiating rollback..."

    if [ "$FORCE_DEPLOY" = "false" ]; then
        bash "$SCRIPT_DIR/rollback.sh" -n "$namespace"
        exit 1
    else
        log_warning "Force deploy enabled, skipping rollback"
        exit 1
    fi
}

# Main function
main() {
    parse_args "$@"

    log_info "Starting deployment process..."
    echo ""

    validate_environment
    setup_credentials

    local config_path
    config_path=$(get_config_path)

    validate_config "$config_path"

    local namespace
    if [ "$ENVIRONMENT" = "staging" ]; then
        namespace="staging"
    else
        namespace="production"
    fi

    if ! pre_deployment_checks; then
        if [ "$FORCE_DEPLOY" = "false" ]; then
            log_error "Pre-deployment checks failed"
            exit 1
        else
            log_warning "Pre-deployment checks failed but force deploy is enabled"
        fi
    fi

    if ! perform_deployment "$config_path"; then
        rollback_on_failure "$namespace"
    fi

    if [ "$DRY_RUN" = "false" ]; then
        if ! wait_rollout "$namespace"; then
            rollback_on_failure "$namespace"
        fi

        post_deployment_verification "$namespace"
    fi

    log_success "Deployment completed successfully!"
    echo ""
}

# Run main function
main "$@"
