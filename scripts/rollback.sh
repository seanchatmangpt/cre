#!/bin/bash

set -euo pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
PROJECT_ID="${GCP_PROJECT_ID:-}"
ZONE="${GCP_ZONE:-us-central1-a}"
CLUSTER="${CLUSTER_NAME:-prod-cluster}"
NAMESPACE="${NAMESPACE:-production}"
DEPLOYMENT="admin-console"
IMAGE_SHA="${1:-}"
ROLLBACK_TIMEOUT=300

# Functions
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

# Validate prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl is not installed"
        exit 1
    fi

    if ! command -v gcloud &> /dev/null; then
        log_warning "gcloud CLI is not installed, skipping GCP setup"
    fi

    if [ -z "$PROJECT_ID" ] && command -v gcloud &> /dev/null; then
        PROJECT_ID=$(gcloud config get-value project)
        log_info "Using GCP Project: $PROJECT_ID"
    fi
}

# Get cluster credentials
setup_cluster_access() {
    log_info "Setting up cluster access..."

    if command -v gcloud &> /dev/null && [ -n "$PROJECT_ID" ]; then
        gcloud container clusters get-credentials "$CLUSTER" \
            --zone "$ZONE" \
            --project "$PROJECT_ID" 2>/dev/null || log_warning "Failed to get cluster credentials"
    fi

    # Verify kubectl access
    if ! kubectl cluster-info &> /dev/null; then
        log_error "Cannot access cluster. Make sure you have proper credentials configured."
        exit 1
    fi

    log_success "Cluster access configured"
}

# Get current deployment info
get_deployment_info() {
    log_info "Retrieving current deployment information..."

    CURRENT_REVISION=$(kubectl rollout history deployment/$DEPLOYMENT \
        -n $NAMESPACE | tail -1 | awk '{print $1}')
    log_info "Current revision: $CURRENT_REVISION"

    CURRENT_IMAGE=$(kubectl get deployment $DEPLOYMENT -n $NAMESPACE \
        -o jsonpath='{.spec.template.spec.containers[0].image}')
    log_info "Current image: $CURRENT_IMAGE"

    CURRENT_REPLICAS=$(kubectl get deployment $DEPLOYMENT -n $NAMESPACE \
        -o jsonpath='{.spec.replicas}')
    log_info "Current replicas: $CURRENT_REPLICAS"
}

# Get previous stable revision
get_previous_revision() {
    log_info "Checking previous revisions..."

    HISTORY=$(kubectl rollout history deployment/$DEPLOYMENT -n $NAMESPACE)
    echo "$HISTORY"

    # Get the second-to-last revision
    PREVIOUS_REVISION=$(echo "$HISTORY" | tail -2 | head -1 | awk '{print $1}')

    if [ -z "$PREVIOUS_REVISION" ] || [ "$PREVIOUS_REVISION" = "REVISION" ]; then
        log_error "No previous revision found"
        return 1
    fi

    log_success "Previous revision: $PREVIOUS_REVISION"
    return 0
}

# Perform rollback
perform_rollback() {
    local target_revision=$1

    log_info "Initiating rollback to revision $target_revision..."

    kubectl rollout undo deployment/$DEPLOYMENT \
        -n $NAMESPACE \
        --to-revision=$target_revision

    log_success "Rollback command executed"
}

# Wait for rollout completion
wait_for_rollout() {
    log_info "Waiting for deployment to stabilize (timeout: ${ROLLBACK_TIMEOUT}s)..."

    if kubectl rollout status deployment/$DEPLOYMENT \
        -n $NAMESPACE \
        --timeout=${ROLLBACK_TIMEOUT}s; then
        log_success "Deployment rollback completed successfully"
        return 0
    else
        log_error "Deployment rollout failed or timed out"
        return 1
    fi
}

# Verify rollback
verify_rollback() {
    log_info "Verifying rollback..."

    # Check pod status
    READY_REPLICAS=$(kubectl get deployment $DEPLOYMENT -n $NAMESPACE \
        -o jsonpath='{.status.readyReplicas}')
    DESIRED_REPLICAS=$(kubectl get deployment $DEPLOYMENT -n $NAMESPACE \
        -o jsonpath='{.spec.replicas}')

    log_info "Ready replicas: $READY_REPLICAS / $DESIRED_REPLICAS"

    if [ "$READY_REPLICAS" != "$DESIRED_REPLICAS" ]; then
        log_warning "Not all replicas are ready"
        return 1
    fi

    # Check for recent errors
    ERRORS=$(kubectl get events -n $NAMESPACE \
        --sort-by='.lastTimestamp' | grep -i "error\|failed" | tail -5)

    if [ -n "$ERRORS" ]; then
        log_warning "Recent errors found:"
        echo "$ERRORS"
        return 1
    fi

    log_success "Rollback verification passed"
    return 0
}

# Health check
perform_health_check() {
    log_info "Performing health check..."

    # Get service endpoint
    SERVICE_IP=$(kubectl get service $DEPLOYMENT -n $NAMESPACE \
        -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    if [ -z "$SERVICE_IP" ]; then
        log_warning "Could not retrieve service IP"
        return 1
    fi

    log_info "Service IP: $SERVICE_IP"

    # Try health check
    if curl -sf http://$SERVICE_IP/health &> /dev/null; then
        log_success "Health check passed"
        return 0
    else
        log_warning "Health check failed"
        return 1
    fi
}

# Generate rollback report
generate_report() {
    local status=$1
    local report_file="rollback-report-$(date +%Y%m%d-%H%M%S).log"

    log_info "Generating rollback report: $report_file"

    {
        echo "====== Rollback Report ======"
        echo "Timestamp: $(date)"
        echo "Cluster: $CLUSTER"
        echo "Namespace: $NAMESPACE"
        echo "Deployment: $DEPLOYMENT"
        echo "Status: $status"
        echo ""
        echo "====== Deployment Status ======"
        kubectl get deployment $DEPLOYMENT -n $NAMESPACE -o yaml
        echo ""
        echo "====== Pod Status ======"
        kubectl get pods -n $NAMESPACE -l app=$DEPLOYMENT -o wide
        echo ""
        echo "====== Recent Events ======"
        kubectl get events -n $NAMESPACE --sort-by='.lastTimestamp' | tail -20
        echo ""
        echo "====== Deployment History ======"
        kubectl rollout history deployment/$DEPLOYMENT -n $NAMESPACE
    } > "$report_file"

    log_success "Report saved to: $report_file"
}

# Automatic rollback (when health check fails)
trigger_automatic_rollback() {
    log_warning "Triggering automatic rollback due to health check failure..."

    if get_previous_revision; then
        perform_rollback "$PREVIOUS_REVISION"
        wait_for_rollout
        verify_rollback
    fi
}

# Manual rollback by image SHA
rollback_to_image() {
    local target_sha=$1
    log_info "Attempting to rollback to image SHA: $target_sha..."

    # Find revision with matching image
    local revision=$(kubectl rollout history deployment/$DEPLOYMENT -n $NAMESPACE | \
        awk 'NR>1 {print $1}' | while read rev; do
            image=$(kubectl rollout history deployment/$DEPLOYMENT -n $NAMESPACE --revision=$rev | \
                grep -oP '${IMAGE}:[^ ]+' | head -1)
            if [[ "$image" == *"$target_sha"* ]]; then
                echo "$rev"
                break
            fi
        done)

    if [ -z "$revision" ]; then
        log_error "No revision found for image SHA: $target_sha"
        return 1
    fi

    log_info "Found matching revision: $revision"
    perform_rollback "$revision"
    wait_for_rollout
    verify_rollback
}

# Display current status
show_status() {
    log_info "Current Deployment Status"
    echo ""
    kubectl get deployment $DEPLOYMENT -n $NAMESPACE
    echo ""
    log_info "Pod Status"
    echo ""
    kubectl get pods -n $NAMESPACE -l app=$DEPLOYMENT -o wide
}

# Main function
main() {
    log_info "Starting rollback process..."
    echo ""

    # Check prerequisites
    check_prerequisites

    # Setup cluster access
    setup_cluster_access

    # Get current deployment info
    get_deployment_info

    # Determine rollback target
    if [ -n "$IMAGE_SHA" ]; then
        # Rollback to specific image SHA
        rollback_to_image "$IMAGE_SHA"
        ROLLBACK_STATUS=$?
    else
        # Rollback to previous revision
        if get_previous_revision; then
            perform_rollback "$PREVIOUS_REVISION"
            wait_for_rollout
            verify_rollback
            ROLLBACK_STATUS=$?
        else
            ROLLBACK_STATUS=1
        fi
    fi

    # Show final status
    show_status

    # Perform health check
    if perform_health_check; then
        generate_report "SUCCESS"
        log_success "Rollback completed successfully!"
        exit 0
    else
        log_warning "Health check failed, attempting automatic recovery..."
        generate_report "HEALTH_CHECK_FAILED"
        exit 1
    fi
}

# Show usage
show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Rollback admin-console deployment to previous stable version.

OPTIONS:
    -h, --help              Show this help message
    -s, --sha SHA           Rollback to specific image SHA
    -c, --cluster NAME      Cluster name (default: prod-cluster)
    -n, --namespace NS      Namespace (default: production)
    -z, --zone ZONE         GCP zone (default: us-central1-a)
    --status                Show current deployment status
    --history               Show deployment history

EXAMPLES:
    # Rollback to previous revision
    $0

    # Rollback to specific image SHA
    $0 -s abc123def456

    # Show deployment status
    $0 --status

    # Show deployment history
    $0 --history

EOF
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_usage
            exit 0
            ;;
        -s|--sha)
            IMAGE_SHA="$2"
            shift 2
            ;;
        -c|--cluster)
            CLUSTER="$2"
            shift 2
            ;;
        -n|--namespace)
            NAMESPACE="$2"
            shift 2
            ;;
        -z|--zone)
            ZONE="$2"
            shift 2
            ;;
        --status)
            check_prerequisites
            setup_cluster_access
            show_status
            exit 0
            ;;
        --history)
            check_prerequisites
            setup_cluster_access
            log_info "Deployment History"
            kubectl rollout history deployment/$DEPLOYMENT -n $NAMESPACE
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Run main function
main "$@"
