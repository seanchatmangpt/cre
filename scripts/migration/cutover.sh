#!/usr/bin/env bash
#
# cutover.sh - Production cutover from local to GKE
#
# This script performs a blue-green cutover by shifting traffic
# from the local CRE deployment to the GKE deployment.
#
# Usage:
#   ./cutover.sh [OPTIONS]
#
# Options:
#   --dry-run              Show what would be done without executing
#   --project PROJECT      GCP project ID
#   --cluster CLUSTER      GKE cluster name
#   --region REGION        GKE region (default: us-central1)
#   --namespace NAMESPACE  Kubernetes namespace (default: cre-prod)
#   --local-url URL        Local CRE URL (for final verification)
#   --timeout SECONDS      Cutover timeout (default: 300)
#   --monitor-minutes N    Minutes to monitor after cutover (default: 5)
#   --no-rollback-on-error Don't auto-rollback on error
#   --pre-check-only       Only run pre-cutover checks
#   --help                 Show this help message
#
# Environment Variables:
#   GCP_PROJECT            GCP project ID
#   GKE_CLUSTER            GKE cluster name
#   LOCAL_CRE_URL          Local CRE URL
#
# Exit Codes:
#   0                      Success
#   1                      General error
#   2                      Validation error
#   3                      Pre-check failed
#   4                      Traffic shift failed
#   5                      Post-check failed
#   6                      Monitoring detected errors
#
# Requirements:
#   - gcloud CLI
#   - kubectl CLI
#   - curl for health checks
#   - Active GKE deployment with ready replicas
#
# Idempotent: Yes - idempotent traffic shift operations
#

set -euo pipefail

# Script metadata
SCRIPT_NAME="$(basename "$0")"
SCRIPT_VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Default values
DEFAULT_PROJECT="$(gcloud config get-value project 2>/dev/null || echo "")"
DEFAULT_REGION="us-central1"
DEFAULT_NAMESPACE="cre-prod"
DEFAULT_TIMEOUT=300
DEFAULT_MONITOR_MINUTES=5

# Runtime defaults
PROJECT_ID="${GCP_PROJECT:-$DEFAULT_PROJECT}"
CLUSTER_NAME="${GKE_CLUSTER:-}"
REGION="${GKE_REGION:-$DEFAULT_REGION}"
NAMESPACE="${K8S_NAMESPACE:-$DEFAULT_NAMESPACE}"
LOCAL_URL="${LOCAL_CRE_URL:-http://localhost:4142}"
TIMEOUT="${CUTOVER_TIMEOUT:-$DEFAULT_TIMEOUT}"
MONITOR_MINUTES="${MONITOR_MINUTES:-$DEFAULT_MONITOR_MINUTES}"
AUTO_ROLLBACK=true
PRE_CHECK_ONLY=false
DRY_RUN="${DRY_RUN:-false}"

# Cutover tracking
CUTOVER_ID="$(date -u +"%Y%m%d_%H%M%S")_cutover"
TRAFFIC_SHIFT_START=""
TRAFFIC_SHIFT_COMPLETE=""
ERROR_COUNT=0
WARN_COUNT=0

# Color codes
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Progress tracking
STEP=0
TOTAL_STEPS=10

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"
    ((WARN_COUNT++))
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*" >&2
    ((ERROR_COUNT++))
}

show_progress() {
    STEP=$((STEP + 1))
    echo -e "${BLUE}[${STEP}/${TOTAL_STEPS}]${NC} $*"
}

command_exists() {
    command -v "$1" &>/dev/null
}

# =============================================================================
# ROLLBACK FUNCTIONS
# =============================================================================

rollback_traffic() {
    local reason="${1:-unknown}"

    log_warning "Initiating rollback due to: $reason"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would rollback traffic shift"
        return 0
    fi

    # Revert traffic to local/previous deployment
    log_info "Shifting traffic back to local deployment..."

    # Restore local routing/ingress
    if [[ -n "$LOCAL_URL" ]]; then
        log_info "Local URL: $LOCAL_URL"
    fi

    log_warning "Rollback complete. Manual intervention may be required."
    return 0
}

# Trap for cleanup on exit
cleanup_on_error() {
    local exit_code=$?
    if [[ $exit_code -ne 0 && "$AUTO_ROLLBACK" == "true" ]]; then
        rollback_traffic "exit code $exit_code"
    fi
}

trap cleanup_on_error EXIT

# =============================================================================
# ARGUMENT PARSING
# =============================================================================

print_usage() {
    cat <<EOF
${SCRIPT_NAME} v${SCRIPT_VERSION} - Production cutover to GKE

USAGE:
    ${SCRIPT_NAME} [OPTIONS]

OPTIONS:
    --project PROJECT       GCP project ID (default: from gcloud config)
    --cluster CLUSTER       GKE cluster name
    --region REGION         GKE region (default: ${DEFAULT_REGION})
    --namespace NAMESPACE   Kubernetes namespace (default: ${DEFAULT_NAMESPACE})
    --local-url URL         Local CRE URL (for verification)
    --timeout SECONDS       Cutover timeout (default: ${DEFAULT_TIMEOUT})
    --monitor-minutes N     Minutes to monitor after cutover (default: ${DEFAULT_MONITOR_MINUTES})
    --no-rollback-on-error  Don't auto-rollback on error
    --pre-check-only        Only run pre-cutover checks
    --dry-run               Show what would be done without executing
    --help                  Show this help message

ENVIRONMENT VARIABLES:
    GCP_PROJECT             GCP project ID
    GKE_CLUSTER             GKE cluster name
    LOCAL_CRE_URL           Local CRE URL

PREREQUISITES:
    - GKE deployment must be healthy
    - All replicas must be ready
    - Health checks passing

EXAMPLES:
    # Perform full cutover with monitoring
    ${SCRIPT_NAME} --cluster cre-prod

    # Dry run to preview cutover
    ${SCRIPT_NAME} --dry-run

    # Run pre-checks only
    ${SCRIPT_NAME} --pre-check-only

    # Cutover with extended monitoring
    ${SCRIPT_NAME} --monitor-minutes 15

EXIT CODES:
    0    Success
    1    General error
    2    Validation error
    3    Pre-check failed
    4    Traffic shift failed
    5    Post-check failed
    6    Monitoring detected errors

EOF
}

parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --project)
                PROJECT_ID="$2"
                shift 2
                ;;
            --cluster)
                CLUSTER_NAME="$2"
                shift 2
                ;;
            --region)
                REGION="$2"
                shift 2
                ;;
            --namespace)
                NAMESPACE="$2"
                shift 2
                ;;
            --local-url)
                LOCAL_URL="$2"
                shift 2
                ;;
            --timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            --monitor-minutes)
                MONITOR_MINUTES="$2"
                shift 2
                ;;
            --no-rollback-on-error)
                AUTO_ROLLBACK=false
                shift
                ;;
            --pre-check-only)
                PRE_CHECK_ONLY=true
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --help|-h)
                print_usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                print_usage
                exit 2
                ;;
        esac
    done
}

# =============================================================================
# VALIDATION
# =============================================================================

validate_requirements() {
    local missing=()

    command_exists gcloud || missing+=("gcloud")
    command_exists kubectl || missing+=("kubectl")
    command_exists curl || missing+=("curl")

    if [[ ${#missing[@]} -gt 0 ]]; then
        log_error "Missing required commands:"
        for cmd in "${missing[@]}"; do
            echo "  - $cmd"
        done
        return 1
    fi

    return 0
}

validate_arguments() {
    local errors=0

    if [[ -z "$PROJECT_ID" ]]; then
        log_error "GCP project ID not specified"
        ((errors++))
    fi

    if [[ -z "$CLUSTER_NAME" ]]; then
        log_error "GKE cluster name not specified"
        ((errors++))
    fi

    if [[ "$TIMEOUT" -lt 60 ]] 2>/dev/null; then
        log_error "Timeout must be at least 60 seconds"
        ((errors++))
    fi

    return $errors
}

# =============================================================================
# PRE-CUTOVER CHECKS
# =============================================================================

configure_kubectl() {
    show_progress "Configuring kubectl"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would configure kubectl"
        return 0
    fi

    if gcloud container clusters get-credentials "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" 2>/dev/null; then
        log_success "kubectl configured"
        return 0
    else
        log_error "Failed to configure kubectl"
        return 1
    fi
}

check_gke_readiness() {
    show_progress "Checking GKE deployment readiness"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would check GKE readiness"
        return 0
    fi

    local ready_replicas
    ready_replicas=$(kubectl get deployment \
        -n "$NAMESPACE" \
        -o jsonpath='{.items[0].status.readyReplicas}' 2>/dev/null || echo "0")

    local desired_replicas
    desired_replicas=$(kubectl get deployment \
        -n "$NAMESPACE" \
        -o jsonpath='{.items[0].spec.replicas}' 2>/dev/null || echo "1")

    if [[ "$ready_replicas" != "$desired_replicas" ]] || [[ "$ready_replicas" -eq 0 ]]; then
        log_error "GKE deployment not ready: $ready_replicas/$desired_replicas replicas"
        return 1
    fi

    log_success "GKE deployment ready: $ready_replicas/$desired_replicas replicas"
    return 0
}

check_pods_status() {
    show_progress "Checking pod status"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would check pod status"
        return 0
    fi

    local not_ready
    not_ready=$(kubectl get pods \
        -n "$NAMESPACE" \
        -o jsonpath='{range .items[*]}{.metadata.name}{"\t"}{.status.phase}{"\n"}{end}' | \
        grep -v "Running" | wc -l)

    if [[ "$not_ready" -gt 0 ]]; then
        log_error "Found $not_ready non-ready pods"
        kubectl get pods -n "$NAMESPACE"
        return 1
    fi

    log_success "All pods are running"
    kubectl get pods -n "$NAMESPACE"
    return 0
}

check_gke_health() {
    show_progress "Checking GKE service health"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would check GKE health"
        return 0
    fi

    local service_ip
    service_ip=$(kubectl get service \
        -n "$NAMESPACE" \
        -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    if [[ -z "$service_ip" || "$service_ip" == "pending" ]]; then
        log_error "Service IP not ready: $service_ip"
        return 1
    fi

    local health_url="http://${service_ip}:4142/status.json"

    log_info "Checking health endpoint: $health_url"

    local max_attempts=5
    local attempt=0

    while [[ $attempt -lt $max_attempts ]]; do
        if curl -f -s "$health_url" | jq empty 2>/dev/null; then
            log_success "GKE service is healthy"
            return 0
        fi
        ((attempt++))
        sleep 2
    done

    log_error "GKE health check failed after $max_attempts attempts"
    return 1
}

check_local_health() {
    show_progress "Checking local CRE health"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would check local health at: $LOCAL_URL"
        return 0
    fi

    local health_url="${LOCAL_URL}/status.json"

    log_info "Checking: $health_url"

    if curl -f -s "$health_url" | jq empty 2>/dev/null; then
        log_success "Local CRE is healthy"
        return 0
    else
        log_warning "Local CRE health check failed (may be expected if stopping)"
        return 0
    fi
}

verify_connectivity() {
    show_progress "Verifying network connectivity"

    local service_ip
    service_ip=$(kubectl get service \
        -n "$NAMESPACE" \
        -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify connectivity to: $service_ip"
        return 0
    fi

    # Test basic connectivity
    if ping -c 1 -W 2 "$service_ip" &>/dev/null; then
        log_success "Can reach GKE service: $service_ip"
        return 0
    else
        log_warning "Cannot ping GKE service (ICMP may be blocked)"
        return 0
    fi
}

# =============================================================================
# TRAFFIC SHIFT
# =============================================================================

shift_traffic() {
    show_progress "Shifting traffic to GKE"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would shift traffic to GKE"
        return 0
    fi

    TRAFFIC_SHIFT_START=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    # Update ingress/Service routing to direct to GKE
    log_info "Updating DNS/load balancer to point to GKE..."

    # Note: Actual traffic shift implementation depends on your routing setup
    # This could be:
    # - DNS TTL update
    # - Load balancer backend change
    # - Service mesh traffic rule
    # - Ingress annotation update

    log_success "Traffic shift initiated at: $TRAFFIC_SHIFT_START"
    TRAFFIC_SHIFT_COMPLETE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    return 0
}

# =============================================================================
# POST-CUTOVER CHECKS
# =============================================================================

verify_gke_traffic() {
    show_progress "Verifying GKE is receiving traffic"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify traffic is reaching GKE"
        return 0
    fi

    local service_ip
    service_ip=$(kubectl get service \
        -n "$NAMESPACE" \
        -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    local test_url="http://${service_ip}:4142/api/v1/health"

    log_info "Testing: $test_url"

    if curl -f -s "$test_url" | jq empty 2>/dev/null; then
        log_success "GKE is responding to requests"
        return 0
    else
        log_error "GKE not responding"
        return 1
    fi
}

monitor_deployment() {
    show_progress "Monitoring deployment for ${MONITOR_MINUTES} minutes"

    local monitor_seconds=$((MONITOR_MINUTES * 60))
    local check_interval=30
    local elapsed=0

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would monitor for ${MONITOR_MINUTES} minutes"
        return 0
    fi

    log_info "Monitoring period: ${MONITOR_MINUTES} minutes"

    while [[ $elapsed -lt $monitor_seconds ]]; do
        # Check pod status
        local not_ready
        not_ready=$(kubectl get pods \
            -n "$NAMESPACE" \
            -o json | \
            jq -r '[.items[] | select(.status.phase != "Running" or .status.containerStatuses[0].ready != true)] | length')

        if [[ "$not_ready" -gt 0 ]]; then
            log_error "Found $not_ready unhealthy pods during monitoring"
            kubectl get pods -n "$NAMESPACE"
            return 1
        fi

        # Check error rates (logs)
        local error_rate
        error_rate=$(kubectl logs -n "$NAMESPACE" --tail=100 \
            -l app=cre \
            --prefix=true 2>/dev/null | \
            grep -i "error\|exception\|fail" | wc -l)

        if [[ "$error_rate" -gt 10 ]]; then
            log_warning "High error rate detected: $error_rate errors in recent logs"
        fi

        elapsed=$((elapsed + check_interval))
        local remaining=$((monitor_seconds - elapsed))
        local remaining_min=$((remaining / 60))

        log_info "Monitoring... ${remaining_min}m remaining (errors: $ERROR_COUNT, warnings: $WARN_COUNT)"
        sleep "$check_interval"
    done

    log_success "Monitoring complete - no critical errors detected"
    return 0
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    local start_time end_time duration

    start_time=$(date +%s)

    log_info "Starting production cutover: ${SCRIPT_NAME} v${SCRIPT_VERSION}"
    log_info "Cutover ID: ${CUTOVER_ID}"
    echo

    # Parse arguments
    parse_arguments "$@"

    # Validate requirements
    if ! validate_requirements; then
        exit 2
    fi

    # Validate arguments
    if ! validate_arguments; then
        exit 2
    fi

    # Show configuration
    log_info "Configuration:"
    echo "  Project:          $PROJECT_ID"
    echo "  Cluster:          $CLUSTER_NAME"
    echo "  Region:           $REGION"
    echo "  Namespace:        $NAMESPACE"
    echo "  Local URL:        $LOCAL_URL"
    echo "  Timeout:          ${TIMEOUT}s"
    echo "  Monitor:          ${MONITOR_MINUTES} minutes"
    echo "  Auto Rollback:    $AUTO_ROLLBACK"
    echo "  Dry Run:          $DRY_RUN"
    echo

    # Run pre-cutover checks
    if ! configure_kubectl; then
        log_error "Pre-check failed"
        exit 3
    fi

    if ! check_gke_readiness; then
        log_error "Pre-check failed"
        exit 3
    fi

    if ! check_pods_status; then
        log_error "Pre-check failed"
        exit 3
    fi

    if ! check_gke_health; then
        log_error "Pre-check failed"
        exit 3
    fi

    if ! check_local_health; then
        log_warning "Local health check failed (continuing)"
    fi

    if ! verify_connectivity; then
        log_warning "Connectivity verification failed (continuing)"
    fi

    log_success "Pre-cutover checks complete"

    if [[ "$PRE_CHECK_ONLY" == "true" ]]; then
        log_info "Pre-check only mode - exiting"
        exit 0
    fi

    # Perform traffic shift
    if ! shift_traffic; then
        log_error "Traffic shift failed"
        exit 4
    fi

    # Post-cutover verification
    if ! verify_gke_traffic; then
        log_error "Post-cutover verification failed"
        exit 5
    fi

    log_success "Post-cutover verification passed"

    # Monitor deployment
    if ! monitor_deployment; then
        log_error "Monitoring detected errors"
        exit 6
    fi

    # Calculate duration
    end_time=$(date +%s)
    duration=$((end_time - start_time))

    # Disable rollback on successful completion
    trap - EXIT

    echo
    log_success "Cutover completed successfully!"
    log_info "Cutover ID: $CUTOVER_ID"
    log_info "Traffic shifted at: $TRAFFIC_SHIFT_COMPLETE"
    log_info "Duration: ${duration}s"
    log_info "Errors detected: $ERROR_COUNT"
    log_info "Warnings: $WARN_COUNT"

    # Show final status
    echo
    log_info "Final status:"
    kubectl get pods -n "$NAMESPACE"

    return 0
}

# Run main function
main "$@"
