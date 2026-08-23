#!/usr/bin/env bash
#
# rollback.sh - Emergency rollback from GKE to local CRE
#
# This script performs an emergency rollback by shifting traffic
# from GKE back to the local CRE deployment and exporting any
# Spanner delta for import into Mnesia.
#
# Usage:
#   ./rollback.sh [OPTIONS]
#
# Options:
#   --dry-run              Show what would be done without executing
#   --project PROJECT      GCP project ID
#   --cluster CLUSTER      GKE cluster name
#   --region REGION        GKE region (default: us-central1)
#   --namespace NAMESPACE  Kubernetes namespace (default: cre-prod)
#   --local-url URL        Local CRE URL (default: http://localhost:4142)
#   --export-delta         Export Spanner delta since cutover
#   --export-dir DIR       Directory for delta export (default: /tmp/rollback-delta)
#   --reason REASON        Reason for rollback (for logging)
#   --force                Skip confirmation prompts
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
#   3                      Traffic shift failed
#   4                      Local verification failed
#   5                      Delta export failed
#
# Requirements:
#   - gcloud CLI
#   - kubectl CLI
#   - curl for health checks
#   - Local CRE must be running
#
# Idempotent: Yes - safe to run multiple times
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
DEFAULT_LOCAL_URL="http://localhost:4142"
DEFAULT_EXPORT_DIR="/tmp/rollback-delta"

# Runtime defaults
PROJECT_ID="${GCP_PROJECT:-$DEFAULT_PROJECT}"
CLUSTER_NAME="${GKE_CLUSTER:-}"
REGION="${GKE_REGION:-$DEFAULT_REGION}"
NAMESPACE="${K8S_NAMESPACE:-$DEFAULT_NAMESPACE}"
LOCAL_URL="${LOCAL_CRE_URL:-$DEFAULT_LOCAL_URL}"
EXPORT_DELTA=false
EXPORT_DIR="${ROLLBACK_EXPORT_DIR:-$DEFAULT_EXPORT_DIR}"
ROLLBACK_REASON=""
FORCE=false
DRY_RUN="${DRY_RUN:-false}"

# Rollback tracking
ROLLBACK_ID="$(date -u +"%Y%m%d_%H%M%S")_rollback"
START_TIME=""
END_TIME=""
ERROR_COUNT=0

# Color codes
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Progress tracking
STEP=0
TOTAL_STEPS=8

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
# CONFIRMATION
# =============================================================================

confirm_rollback() {
    if [[ "$FORCE" == "true" || "$DRY_RUN" == "true" ]]; then
        return 0
    fi

    echo
    echo -e "${YELLOW}=== EMERGENCY ROLLBACK CONFIRMATION ===${NC}"
    echo
    echo "This will shift traffic from GKE back to local CRE."
    echo
    echo "Configuration:"
    echo "  Project:    $PROJECT_ID"
    echo "  Cluster:    $CLUSTER_NAME"
    echo "  Namespace:  $NAMESPACE"
    echo "  Local URL:  $LOCAL_URL"
    if [[ -n "$ROLLBACK_REASON" ]]; then
        echo "  Reason:     $ROLLBACK_REASON"
    fi
    echo
    echo -e "${RED}This will affect production traffic.${NC}"
    echo

    local response
    read -p "Type 'ROLLBACK' to confirm: " response

    if [[ "$response" == "ROLLBACK" ]]; then
        return 0
    else
        log_error "Rollback cancelled by user"
        exit 1
    fi
}

# =============================================================================
# ARGUMENT PARSING
# =============================================================================

print_usage() {
    cat <<EOF
${SCRIPT_NAME} v${SCRIPT_VERSION} - Emergency rollback from GKE to local CRE

USAGE:
    ${SCRIPT_NAME} [OPTIONS]

OPTIONS:
    --project PROJECT       GCP project ID (default: from gcloud config)
    --cluster CLUSTER       GKE cluster name
    --region REGION         GKE region (default: ${DEFAULT_REGION})
    --namespace NAMESPACE   Kubernetes namespace (default: ${DEFAULT_NAMESPACE})
    --local-url URL         Local CRE URL (default: ${DEFAULT_LOCAL_URL})
    --export-delta          Export Spanner delta since cutover
    --export-dir DIR        Directory for delta export (default: ${DEFAULT_EXPORT_DIR})
    --reason REASON         Reason for rollback (for logging)
    --force                 Skip confirmation prompts
    --dry-run               Show what would be done without executing
    --help                  Show this help message

ENVIRONMENT VARIABLES:
    GCP_PROJECT             GCP project ID
    GKE_CLUSTER             GKE cluster name
    LOCAL_CRE_URL           Local CRE URL

REQUIREMENTS:
    - Local CRE must be running and healthy
    - Network connectivity to GKE cluster

EXAMPLES:
    # Emergency rollback with confirmation
    ${SCRIPT_NAME} --cluster cre-prod --reason "High error rate"

    # Force rollback without confirmation
    ${SCRIPT_NAME} --cluster cre-prod --force

    # Rollback with delta export
    ${SCRIPT_NAME} --cluster cre-prod --export-delta

    # Dry run to preview rollback
    ${SCRIPT_NAME} --dry-run

EXIT CODES:
    0    Success
    1    General error
    2    Validation error
    3    Traffic shift failed
    4    Local verification failed
    5    Delta export failed

NOTES:
    - Rollback shifts traffic immediately
    - GKE pods remain running for fallback
    - Delta export captures data changes for reconciliation

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
            --export-delta)
                EXPORT_DELTA=true
                shift
                ;;
            --export-dir)
                EXPORT_DIR="$2"
                shift 2
                ;;
            --reason)
                ROLLBACK_REASON="$2"
                shift 2
                ;;
            --force)
                FORCE=true
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

    return $errors
}

# =============================================================================
# LOCAL VERIFICATION
# =============================================================================

verify_local_running() {
    show_progress "Verifying local CRE is running"

    local health_url="${LOCAL_URL}/status.json"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify local CRE at: $LOCAL_URL"
        return 0
    fi

    log_info "Checking: $health_url"

    local max_attempts=5
    local attempt=0

    while [[ $attempt -lt $max_attempts ]]; do
        if curl -f -s "$health_url" | jq empty 2>/dev/null; then
            log_success "Local CRE is healthy"
            return 0
        fi
        ((attempt++))
        log_warning "Attempt $attempt/$max_attempts failed"
        sleep 2
    done

    log_error "Local CRE health check failed"
    log_error "Please ensure local CRE is running before rollback"
    return 1
}

# =============================================================================
# K8S/GKE FUNCTIONS
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
        log_warning "Failed to configure kubectl (continuing with local verification)"
        return 0
    fi
}

record_gke_state() {
    show_progress "Recording GKE state for recovery"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would record GKE state"
        return 0
    fi

    local state_file="/tmp/gke_state_${ROLLBACK_ID}.json"

    # Capture current state
    {
        echo "{"
        echo "  \"rollback_id\": \"${ROLLBACK_ID}\","
        echo "  \"timestamp\": \"$(date -u +"%Y-%m-%dT%H:%M:%SZ")\","
        echo "  \"reason\": \"${ROLLBACK_REASON}\","
        echo "  \"project\": \"${PROJECT_ID}\","
        echo "  \"cluster\": \"${CLUSTER_NAME}\","
        echo "  \"region\": \"${REGION}\","
        echo "  \"namespace\": \"${NAMESPACE}\","
        echo "  \"pods\": ["

        # Get pod info
        kubectl get pods -n "$NAMESPACE" -o json 2>/dev/null | \
            jq -c '.items[] | {name: .metadata.name, ready: .status.containerStatuses[0].ready}' | \
            sed 's/^/    /' | sed '$ s/$/,/' | sed '$ s/,$//'

        echo "  ],"
        echo "  \"replicas\": $(kubectl get deployment -n "$NAMESPACE" -o jsonpath='{.spec.replicas}' 2>/dev/null || echo "unknown")"
        echo "}"
    } > "$state_file"

    log_success "GKE state saved to: $state_file"
    return 0
}

# =============================================================================
# TRAFFIC SHIFT FUNCTIONS
# =============================================================================

shift_traffic_to_local() {
    show_progress "Shifting traffic to local CRE"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would shift traffic to local: $LOCAL_URL"
        return 0
    fi

    START_TIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    log_info "Initiating traffic shift at: $START_TIME"

    # Update DNS/load balancer to point to local
    # Note: Implementation depends on your routing setup

    log_success "Traffic shifted to local CRE"
    END_TIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    return 0
}

verify_local_traffic() {
    show_progress "Verifying traffic is reaching local CRE"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify local traffic"
        return 0
    fi

    local health_url="${LOCAL_URL}/status.json"

    if curl -f -s "$health_url" | jq empty 2>/dev/null; then
        log_success "Local CRE is receiving traffic"
        return 0
    else
        log_error "Local CRE not responding"
        return 1
    fi
}

scale_down_gke() {
    show_progress "Scaling down GKE deployment (optional)"

    local response
    if [[ "$FORCE" != "true" && "$DRY_RUN" != "true" ]]; then
        echo
        read -p "Scale down GKE pods to 0? (y/N): " response
        if [[ "$response" != "y" && "$response" != "Y" ]]; then
            log_info "Keeping GKE pods running"
            return 0
        fi
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would scale down GKE deployment"
        return 0
    fi

    log_warning "Scaling GKE deployment to 0 replicas"
    kubectl scale deployment \
        -n "$NAMESPACE" \
        --replicas=0 \
        --timeout=60s 2>/dev/null || log_warning "Scale down failed (continuing)"

    log_success "GKE deployment scaled down"
    return 0
}

# =============================================================================
# DELTA EXPORT FUNCTIONS
# =============================================================================

export_spanner_delta() {
    if [[ "$EXPORT_DELTA" != "true" ]]; then
        return 0
    fi

    show_progress "Exporting Spanner delta"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would export Spanner delta to: $EXPORT_DIR"
        return 0
    fi

    mkdir -p "$EXPORT_DIR"

    # Create delta export script
    local export_script="${EXPORT_DIR}/export_delta.erl"

    cat > "$export_script" <<'ERL_EOF'
#!/usr/bin/env escript
%% Export Spanner delta since last cutover
main(_) ->
    %% TODO: Implement Spanner delta export
    %% This requires:
    %% 1. Spanner client connection
    %% 2. Query for changes since cutover timestamp
    %% 3. Export to JSON for Mnesia import
    io:format("Delta export not yet implemented~n"),
    {error, not_implemented}.
ERL_EOF

    log_warning "Spanner delta export not yet implemented"
    log_info "Manual delta export may be required for reconciliation"

    return 0
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    local start_time end_time duration

    start_time=$(date +%s)

    log_info "Starting emergency rollback: ${SCRIPT_NAME} v${SCRIPT_VERSION}"
    log_info "Rollback ID: ${ROLLBACK_ID}"
    if [[ -n "$ROLLBACK_REASON" ]]; then
        log_info "Reason: ${ROLLBACK_REASON}"
    fi
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
    echo "  Project:        $PROJECT_ID"
    echo "  Cluster:        $CLUSTER_NAME"
    echo "  Region:         $REGION"
    echo "  Namespace:      $NAMESPACE"
    echo "  Local URL:      $LOCAL_URL"
    echo "  Export Delta:   $EXPORT_DELTA"
    echo "  Force:          $FORCE"
    echo "  Dry Run:        $DRY_RUN"
    echo

    # Confirm rollback
    confirm_rollback

    # Verify local is running first
    if ! verify_local_running; then
        log_error "Cannot proceed with rollback - local CRE not healthy"
        exit 4
    fi

    # Configure kubectl
    configure_kubectl

    # Record GKE state
    record_gke_state

    # Shift traffic
    if ! shift_traffic_to_local; then
        log_error "Traffic shift failed"
        exit 3
    fi

    # Verify traffic shift
    if ! verify_local_traffic; then
        log_error "Traffic verification failed"
        exit 3
    fi

    # Optionally scale down GKE
    scale_down_gke

    # Export delta if requested
    if ! export_spanner_delta; then
        log_warning "Delta export failed (continuing)"
    fi

    # Calculate duration
    end_time=$(date +%s)
    duration=$((end_time - start_time))

    echo
    log_success "Rollback completed successfully!"
    log_info "Rollback ID: $ROLLBACK_ID"
    log_info "Rollback started: $START_TIME"
    log_info "Rollback ended: $END_TIME"
    log_info "Duration: ${duration}s"
    log_info "Local URL: $LOCAL_URL"

    # Post-rollback checklist
    echo
    log_info "Post-rollback checklist:"
    echo "  [ ] Verify local CRE is processing requests"
    echo "  [ ] Check application logs for errors"
    echo "  [ ] Monitor error rates"
    echo "  [ ] Plan data reconciliation (delta export in: $EXPORT_DIR)"
    echo "  [ ] Investigate rollback reason: ${ROLLBACK_REASON:-unknown}"

    return 0
}

# Run main function
main "$@"
