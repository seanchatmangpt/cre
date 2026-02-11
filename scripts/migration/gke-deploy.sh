#!/usr/bin/env bash
#
# gke-deploy.sh - Deploy CRE to Google Kubernetes Engine
#
# This script builds the CRE Docker image, pushes it to GCR,
# and applies Kubernetes manifests to deploy on GKE.
#
# Usage:
#   ./gke-deploy.sh [OPTIONS]
#
# Options:
#   --dry-run              Show what would be done without executing
#   --project PROJECT      GCP project ID
#   --cluster CLUSTER      GKE cluster name
#   --region REGION        GKE region (default: us-central1)
#   --namespace NAMESPACE  Kubernetes namespace (default: cre-prod)
#   --image IMAGE          Full image name (default: auto-built)
#   --tag TAG              Image tag (default: git commit SHA)
#   --context CONTEXT      Kubernetes context (default: auto-detected)
#   --skip-build           Skip Docker build
#   --skip-health-check    Skip deployment health check
#   --smoke-test           Run smoke tests after deployment
#   --help                 Show this help message
#
# Environment Variables:
#   GCP_PROJECT            GCP project ID
#   GKE_CLUSTER            GKE cluster name
#   GKE_REGION             GKE region
#   IMAGE_TAG              Docker image tag
#
# Exit Codes:
#   0                      Success
#   1                      General error
#   2                      Validation error
#   3                      Build failed
#   4                      Push failed
#   5                      Deployment failed
#   6                      Health check failed
#   7                      Smoke tests failed
#
# Requirements:
#   - gcloud CLI
#   - kubectl CLI
#   - docker or podman
#   - Active GCP project with GKE cluster
#
# Idempotent: Yes - safe to run multiple times
#

set -euo pipefail

# Script metadata
SCRIPT_NAME="$(basename "$0")"
SCRIPT_VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Default values
DEFAULT_PROJECT="$(gcloud config get-value project 2>/dev/null || echo "")"
DEFAULT_REGION="us-central1"
DEFAULT_NAMESPACE="cre-prod"
DEFAULT_IMAGE_NAME="cre"
DEFAULT_REGISTRY="${DEFAULT_PROJECT}"
DEFAULT_K8S_MANIFEST_DIR="k8s/gcp"

# Runtime defaults
PROJECT_ID="${GCP_PROJECT:-$DEFAULT_PROJECT}"
CLUSTER_NAME="${GKE_CLUSTER:-}"
REGION="${GKE_REGION:-$DEFAULT_REGION}"
NAMESPACE="${K8S_NAMESPACE:-$DEFAULT_NAMESPACE}"
IMAGE_NAME="$DEFAULT_IMAGE_NAME"
IMAGE_TAG="${IMAGE_TAG:-}"
CONTEXT=""
SKIP_BUILD=false
SKIP_HEALTH_CHECK=false
RUN_SMOKE_TESTS=false
DRY_RUN="${DRY_RUN:-false}"
AUTO_ROLLBACK=true
ROLLBACK_ON_FAILURE=true

# Deployment tracking
DEPLOYMENT_ID="$(date -u +"%Y%m%d_%H%M%S")_deploy"
HEALTH_CHECK_TIMEOUT=300
HEALTH_CHECK_INTERVAL=10
POD_READY_TIMEOUT=600
MAX_FAILED_CHECKS=3
FAILED_CHECKS=0

# State tracking for rollback
DEPLOYMENT_STARTED=false
PREVIOUS_IMAGE=""
PREVIOUS_REPLICAS=""
PREVIOUS_STATE_FILE=""

# Color codes
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Progress tracking
STEP=0
TOTAL_STEPS=14

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

show_progress() {
    STEP=$((STEP + 1))
    echo -e "${BLUE}[${STEP}/${TOTAL_STEPS}]${NC} $*"
}

command_exists() {
    command -v "$1" &>/dev/null
}

print_usage() {
    cat <<EOF
${SCRIPT_NAME} v${SCRIPT_VERSION} - Deploy CRE to Google Kubernetes Engine

USAGE:
    ${SCRIPT_NAME} [OPTIONS]

OPTIONS:
    --project PROJECT       GCP project ID (default: from gcloud config)
    --cluster CLUSTER       GKE cluster name
    --region REGION         GKE region (default: ${DEFAULT_REGION})
    --namespace NAMESPACE   Kubernetes namespace (default: ${DEFAULT_NAMESPACE})
    --image IMAGE           Full image name (default: ${DEFAULT_IMAGE_NAME})
    --tag TAG               Image tag (default: git commit SHA)
    --context CONTEXT       Kubernetes context (default: auto-detected)
    --manifest-dir DIR      K8s manifest directory (default: ${DEFAULT_K8S_MANIFEST_DIR})
    --skip-build            Skip Docker build
    --skip-health-check     Skip deployment health check
    --no-rollback           Disable automatic rollback on failure
    --smoke-test            Run smoke tests after deployment
    --dry-run               Show what would be done without executing
    --help                  Show this help message

ENVIRONMENT VARIABLES:
    GCP_PROJECT             GCP project ID
    GKE_CLUSTER             GKE cluster name
    GKE_REGION              GKE region
    K8S_NAMESPACE           Kubernetes namespace
    IMAGE_TAG               Docker image tag

REQUIREMENTS:
    - gcloud CLI with Kubernetes config
    - kubectl CLI
    - docker or podman for image building
    - jq for JSON processing

EXAMPLES:
    # Deploy to default cluster with all checks and rollback
    ${SCRIPT_NAME}

    # Deploy to specific cluster and run smoke tests
    ${SCRIPT_NAME} --cluster cre-prod --region us-east1 --smoke-test

    # Skip build and deploy existing image
    ${SCRIPT_NAME} --skip-build --tag v1.2.3

    # Deploy without automatic rollback
    ${SCRIPT_NAME} --cluster cre-prod --no-rollback

    # Dry run to preview deployment
    ${SCRIPT_NAME} --dry-run

    # Deploy to staging namespace
    ${SCRIPT_NAME} --namespace cre-staging

EXIT CODES:
    0    Success
    1    General error
    2    Validation error
    3    Build failed
    4    Push failed
    5    Deployment failed
    6    Health check failed
    7    Smoke tests failed
    8    Rollback failed

FEATURES:
    - Automated kubectl apply for all K8s manifests
    - Comprehensive pod health checks
    - Automatic rollback on failure
    - Readiness and liveness probe verification
    - Service endpoint validation
    - Complete deployment state capture for recovery

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
            --image)
                IMAGE_NAME="$2"
                shift 2
                ;;
            --tag)
                IMAGE_TAG="$2"
                shift 2
                ;;
            --context)
                CONTEXT="$2"
                shift 2
                ;;
            --manifest-dir)
                DEFAULT_K8S_MANIFEST_DIR="$2"
                shift 2
                ;;
            --skip-build)
                SKIP_BUILD=true
                shift
                ;;
            --skip-health-check)
                SKIP_HEALTH_CHECK=true
                shift
                ;;
            --no-rollback)
                AUTO_ROLLBACK=false
                ROLLBACK_ON_FAILURE=false
                shift
                ;;
            --smoke-test)
                RUN_SMOKE_TESTS=true
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
    command_exists jq || missing+=("jq")

    if [[ "$SKIP_BUILD" != "true" ]]; then
        command_exists docker || missing+=("docker")
    fi

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

    # Auto-detect image tag from git if not set
    if [[ -z "$IMAGE_TAG" ]]; then
        if git rev-parse --git-dir &>/dev/null; then
            IMAGE_TAG="$(git rev-parse --short HEAD)"
            log_info "Auto-detected image tag from git: $IMAGE_TAG"
        else
            IMAGE_TAG="latest"
            log_warning "Could not detect git tag, using: $IMAGE_TAG"
        fi
    fi

    return $errors
}

# =============================================================================
# GKE/GCP FUNCTIONS
# =============================================================================

configure_kubectl() {
    show_progress "Configuring kubectl for GKE cluster"

    local cluster_endpoint="gke_${PROJECT_ID}_${REGION}_${CLUSTER_NAME}"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would configure kubectl for cluster: $cluster_endpoint"
        return 0
    fi

    # Get cluster credentials
    if gcloud container clusters get-credentials "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" 2>/dev/null; then
        log_success "kubectl configured for cluster: $CLUSTER_NAME"
        return 0
    else
        log_error "Failed to configure kubectl for cluster: $CLUSTER_NAME"
        return 1
    fi
}

verify_cluster_access() {
    show_progress "Verifying cluster access"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify cluster access"
        return 0
    fi

    if kubectl cluster-info &>/dev/null; then
        log_success "Cluster access verified"
        kubectl cluster-info
        return 0
    else
        log_error "Cannot access Kubernetes cluster"
        return 1
    fi
}

ensure_namespace() {
    show_progress "Ensuring namespace exists: $NAMESPACE"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would create namespace if needed: $NAMESPACE"
        return 0
    fi

    if kubectl get namespace "$NAMESPACE" &>/dev/null; then
        log_info "Namespace already exists: $NAMESPACE"
        return 0
    fi

    log_info "Creating namespace: $NAMESPACE"
    kubectl create namespace "$NAMESPACE"
    log_success "Namespace created: $NAMESPACE"
    return 0
}

# =============================================================================
# BUILD FUNCTIONS
# =============================================================================

build_docker_image() {
    show_progress "Building Docker image"

    local full_image="${IMAGE_NAME}:${IMAGE_TAG}"
    local gcr_image="${REGION}-docker.pkg.dev/${PROJECT_ID}/cre/${IMAGE_NAME}:${IMAGE_TAG}"
    local dockerfile="${PROJECT_ROOT}/Dockerfile"

    if [[ ! -f "$dockerfile" ]]; then
        log_error "Dockerfile not found at: $dockerfile"
        return 1
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would build Docker image:"
        log_info "[DRY-RUN]   Source: $dockerfile"
        log_info "[DRY-RUN]   Tag: $full_image"
        return 0
    fi

    log_info "Building image: $full_image"
    log_info "Dockerfile: $dockerfile"

    # Build with BuildKit for better caching
    export DOCKER_BUILDKIT=1

    if docker build \
        --file "$dockerfile" \
        --tag "$full_image" \
        --tag "$gcr_image" \
        --build-arg "VERSION=${IMAGE_TAG}" \
        --build-arg "BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")" \
        --build-arg "GIT_REVISION=$(git rev-parse HEAD 2>/dev/null || echo unknown)" \
        --progress=plain \
        "$PROJECT_ROOT"; then
        log_success "Docker image built successfully"
        return 0
    else
        log_error "Docker build failed"
        return 1
    fi
}

push_docker_image() {
    show_progress "Pushing Docker image to GCR"

    local gcr_image="${REGION}-docker.pkg.dev/${PROJECT_ID}/cre/${IMAGE_NAME}:${IMAGE_TAG}"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would push image to GCR:"
        log_info "[DRY-RUN]   $gcr_image"
        return 0
    fi

    log_info "Pushing to: $gcr_image"

    # Authenticate with GCR
    if ! gcloud auth configure-docker "${REGION}-docker.pkg.dev" --quiet; then
        log_error "Failed to configure docker authentication"
        return 1
    fi

    # Push image
    if docker push "$gcr_image"; then
        log_success "Image pushed successfully"
        echo "$gcr_image" > "/tmp/cre_deploy_image_${DEPLOYMENT_ID}.txt"
        return 0
    else
        log_error "Failed to push image"
        return 1
    fi
}

# =============================================================================
# STATE CAPTURE FUNCTIONS
# =============================================================================

capture_current_state() {
    show_progress "Capturing current deployment state for rollback"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would capture current deployment state"
        return 0
    fi

    PREVIOUS_STATE_FILE="/tmp/cre_deploy_state_${DEPLOYMENT_ID}.json"

    {
        echo "{"
        echo "  \"deployment_id\": \"${DEPLOYMENT_ID}\","
        echo "  \"timestamp\": \"$(date -u +"%Y-%m-%dT%H:%M:%SZ")\","
        echo "  \"namespace\": \"${NAMESPACE}\","
        echo "  \"deployments\": ["

        # Capture deployment info
        kubectl get deployments -n "$NAMESPACE" -o json 2>/dev/null | \
            jq -c '.items[] | {name: .metadata.name, image: .spec.template.spec.containers[0].image, replicas: .spec.replicas}' | \
            sed 's/^/    /' | sed '$ s/$/,/' | sed '$ s/,$//'

        echo "  ],"
        echo "  \"pods\": ["

        # Capture pod info
        kubectl get pods -n "$NAMESPACE" -o json 2>/dev/null | \
            jq -c '.items[] | {name: .metadata.name, phase: .status.phase, image: .spec.containers[0].image}' | \
            sed 's/^/    /' | sed '$ s/$/,/' | sed '$ s/,$//'

        echo "  ]"
        echo "}"
    } > "$PREVIOUS_STATE_FILE"

    # Extract previous image for rollback
    PREVIOUS_IMAGE=$(kubectl get deployment -n "$NAMESPACE" -o jsonpath='{.items[0].spec.template.spec.containers[0].image}' 2>/dev/null || echo "unknown")
    PREVIOUS_REPLICAS=$(kubectl get deployment -n "$NAMESPACE" -o jsonpath='{.items[0].spec.replicas}' 2>/dev/null || echo "1")

    log_success "State captured: $PREVIOUS_STATE_FILE"
    log_info "Previous image: $PREVIOUS_IMAGE"
    log_info "Previous replicas: $PREVIOUS_REPLICAS"

    return 0
}

# =============================================================================
# DEPLOYMENT FUNCTIONS
# =============================================================================

discover_manifests() {
    local manifest_dir="$1"
    local manifests=()

    if [[ ! -d "$manifest_dir" ]]; then
        log_error "Manifest directory not found: $manifest_dir"
        return 1
    fi

    # Define manifest application order (critical resources first)
    local ordered_manifests=(
        "namespace.yaml"
        "serviceaccount.yaml"
        "configmap.yaml"
        "secret.yaml"
        "network-policy.yaml"
        "deployment.yaml"
        "service.yaml"
        "backend-config.yaml"
        "ingress.yaml"
        "hpa.yaml"
        "vpa.yaml"
        "pdb.yaml"
        "hpa-custom-metrics.yaml"
        "spot-nodepool.yaml"
        "tolerations.yaml"
        "backup-cronjob.yaml"
    )

    # Add ordered manifests if they exist
    for manifest in "${ordered_manifests[@]}"; do
        local manifest_file="${manifest_dir}/${manifest}"
        if [[ -f "$manifest_file" ]]; then
            manifests+=("$manifest")
        fi
    done

    # Print discovered manifests
    printf '%s\n' "${manifests[@]}"
}

apply_kubernetes_manifests() {
    show_progress "Applying all Kubernetes manifests"

    local gcr_image="${REGION}-docker.pkg.dev/${PROJECT_ID}/cre/${IMAGE_NAME}:${IMAGE_TAG}"
    local manifest_dir="${PROJECT_ROOT}/${DEFAULT_K8S_MANIFEST_DIR}"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would apply manifests from: $manifest_dir"
        log_info "[DRY-RUN]   Image: $gcr_image"
        log_info "[DRY-RUN]   Namespace: $NAMESPACE"

        # Show which manifests would be applied
        local manifests
        manifests=$(discover_manifests "$manifest_dir")
        log_info "[DRY-RUN] Manifests to apply:"
        echo "$manifests" | while read -r manifest; do
            log_info "[DRY-RUN]   - $manifest"
        done
        return 0
    fi

    # Capture current state before applying
    if ! capture_current_state; then
        log_warning "Failed to capture previous state (continuing with deployment)"
    fi

    # Ensure namespace exists
    ensure_namespace

    # Discover manifests in application order
    local manifests
    manifests=$(discover_manifests "$manifest_dir")

    if [[ -z "$manifests" ]]; then
        log_error "No manifests found in: $manifest_dir"
        return 1
    fi

    local manifest_count=0
    local success_count=0
    local failed_manifests=()

    # Apply each manifest
    while IFS= read -r manifest; do
        local manifest_file="${manifest_dir}/${manifest}"
        ((manifest_count++))

        log_info "[$manifest_count] Applying manifest: $manifest"

        # Substitute image and namespace in manifest
        if kubectl apply \
            -n "$NAMESPACE" \
            -f <(sed "s|REPLACE_WITH_YOUR_PROJECT_ID|${PROJECT_ID}|g;s|us-central1-docker.pkg.dev/REPLACE_WITH_YOUR_PROJECT_ID/cre/cre:0.3.0|${gcr_image}|g" "$manifest_file") \
            2>/dev/null; then
            log_success "  Applied: $manifest"
            ((success_count++))
        else
            log_warning "  Failed to apply: $manifest"
            failed_manifests+=("$manifest")
        fi
    done <<< "$manifests"

    log_info "Manifests applied: $success_count/$manifest_count successful"

    if [[ ${#failed_manifests[@]} -gt 0 ]]; then
        log_warning "Failed to apply the following manifests:"
        for failed in "${failed_manifests[@]}"; do
            echo "  - $failed"
        done
        log_warning "Continuing with deployment (some resources may not be configured)"
    fi

    DEPLOYMENT_STARTED=true
    log_success "Manifest application completed"
    return 0
}

wait_for_deployment() {
    show_progress "Waiting for deployment rollout"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would wait for deployment rollout"
        return 0
    fi

    if kubectl rollout status deployment \
        -n "$NAMESPACE" \
        --timeout="${HEALTH_CHECK_TIMEOUT}s"; then
        log_success "Deployment rolled out successfully"
        return 0
    else
        log_error "Deployment rollout failed or timed out"
        return 1
    fi
}

# =============================================================================
# HEALTH CHECK FUNCTIONS
# =============================================================================

verify_pods_ready() {
    show_progress "Verifying all pods are ready"

    if [[ "$SKIP_HEALTH_CHECK" == "true" ]]; then
        log_warning "Skipping pod health check as requested"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify all pods are ready"
        return 0
    fi

    local max_attempts=$((POD_READY_TIMEOUT / HEALTH_CHECK_INTERVAL))
    local attempt=0
    local pod_check_failed=0

    while [[ $attempt -lt $max_attempts ]]; do
        # Get all pods in namespace
        local pods_json
        pods_json=$(kubectl get pods -n "$NAMESPACE" -o json 2>/dev/null)

        if [[ -z "$pods_json" ]]; then
            ((attempt++))
            log_warning "No pods found yet (attempt $attempt/$max_attempts)"
            sleep "$HEALTH_CHECK_INTERVAL"
            continue
        fi

        # Check pod status
        local total_pods
        local ready_pods
        local running_pods

        total_pods=$(echo "$pods_json" | jq '.items | length')
        ready_pods=$(echo "$pods_json" | jq '[.items[] | select(.status.conditions[] | select(.type=="Ready" and .status=="True"))] | length')
        running_pods=$(echo "$pods_json" | jq '[.items[] | select(.status.phase=="Running")] | length')

        log_info "Pod status (attempt $((attempt + 1))/$max_attempts): $ready_pods ready, $running_pods running, $total_pods total"

        # Display pod details
        if [[ $total_pods -gt 0 ]]; then
            echo "$pods_json" | jq -r '.items[] | "\(.metadata.name) (\(.status.phase)): " + (.status.containerStatuses[]? | "Ready=\(.ready)")' | \
                while read -r pod_info; do
                    log_info "  $pod_info"
                done
        fi

        # Check if all pods are ready
        if [[ $total_pods -gt 0 && $ready_pods -eq $total_pods && $running_pods -eq $total_pods ]]; then
            log_success "All pods ready: $ready_pods/$total_pods"
            pod_check_failed=0
            return 0
        fi

        # Track consecutive failures
        ((attempt++))
        ((pod_check_failed++))

        if [[ $pod_check_failed -gt $MAX_FAILED_CHECKS ]]; then
            log_warning "Pods not ready after $((MAX_FAILED_CHECKS * HEALTH_CHECK_INTERVAL)) seconds"
            pod_check_failed=0
        fi

        sleep "$HEALTH_CHECK_INTERVAL"
    done

    log_error "Pod readiness check timed out after ${POD_READY_TIMEOUT}s"
    log_error "Final pod status:"
    kubectl get pods -n "$NAMESPACE" -o wide 2>/dev/null || true
    return 1
}

verify_deployment_replicas() {
    show_progress "Verifying deployment replicas"

    if [[ "$SKIP_HEALTH_CHECK" == "true" ]]; then
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify deployment replicas"
        return 0
    fi

    local max_attempts=$((HEALTH_CHECK_TIMEOUT / HEALTH_CHECK_INTERVAL))
    local attempt=0

    while [[ $attempt -lt $max_attempts ]]; do
        local deployments_json
        deployments_json=$(kubectl get deployments -n "$NAMESPACE" -o json 2>/dev/null)

        if [[ -z "$deployments_json" ]] || [[ "$(echo "$deployments_json" | jq '.items | length')" -eq 0 ]]; then
            ((attempt++))
            log_warning "No deployments found yet (attempt $attempt/$max_attempts)"
            sleep "$HEALTH_CHECK_INTERVAL"
            continue
        fi

        # Check each deployment
        local all_ready=true
        echo "$deployments_json" | jq -c '.items[] | {name: .metadata.name, desired: .spec.replicas, ready: .status.readyReplicas, updated: .status.updatedReplicas, available: .status.availableReplicas}' | \
            while IFS= read -r deployment_info; do
                local name=$(echo "$deployment_info" | jq -r '.name')
                local desired=$(echo "$deployment_info" | jq -r '.desired')
                local ready=$(echo "$deployment_info" | jq -r '.ready // 0')
                local updated=$(echo "$deployment_info" | jq -r '.updated // 0')
                local available=$(echo "$deployment_info" | jq -r '.available // 0')

                if [[ "$ready" -eq "$desired" && "$updated" -eq "$desired" && "$available" -eq "$desired" ]]; then
                    log_success "Deployment $name ready: $ready/$desired replicas"
                else
                    log_warning "Deployment $name not ready: Ready=$ready, Updated=$updated, Available=$available, Desired=$desired"
                    all_ready=false
                fi
            done

        if [[ "$all_ready" == "true" ]]; then
            return 0
        fi

        ((attempt++))
        sleep "$HEALTH_CHECK_INTERVAL"
    done

    log_error "Deployment replica check timed out"
    return 1
}

verify_service_endpoints() {
    show_progress "Verifying service endpoints"

    if [[ "$SKIP_HEALTH_CHECK" == "true" ]]; then
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify service endpoints"
        return 0
    fi

    local services_json
    services_json=$(kubectl get services -n "$NAMESPACE" -o json 2>/dev/null)

    if [[ -z "$services_json" ]] || [[ "$(echo "$services_json" | jq '.items | length')" -eq 0 ]]; then
        log_info "No services found in namespace"
        return 0
    fi

    # Check each service
    echo "$services_json" | jq -c '.items[] | select(.spec.selector != null) | {name: .metadata.name, type: .spec.type, clusterIP: .status.loadBalancer.ingress[0].ip // .spec.clusterIP}' | \
        while IFS= read -r service_info; do
            local name=$(echo "$service_info" | jq -r '.name')
            local svc_type=$(echo "$service_info" | jq -r '.type')
            local endpoint=$(echo "$service_info" | jq -r '.clusterIP // "pending"')

            log_success "Service $name ($svc_type): $endpoint"
        done

    return 0
}

verify_deployment_health() {
    show_progress "Verifying complete deployment health"

    if [[ "$SKIP_HEALTH_CHECK" == "true" ]]; then
        log_warning "Skipping health check as requested"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify deployment health"
        return 0
    fi

    # Verify pods are ready
    if ! verify_pods_ready; then
        log_error "Pod readiness check failed"
        return 1
    fi

    # Verify deployment replicas
    if ! verify_deployment_replicas; then
        log_error "Deployment replica check failed"
        return 1
    fi

    # Verify service endpoints
    if ! verify_service_endpoints; then
        log_warning "Service endpoint check failed (continuing)"
    fi

    log_success "Deployment health verification completed"
    return 0
}

# =============================================================================
# ROLLBACK FUNCTIONS
# =============================================================================

perform_automatic_rollback() {
    if [[ "$AUTO_ROLLBACK" != "true" || "$ROLLBACK_ON_FAILURE" != "true" ]]; then
        log_warning "Automatic rollback disabled"
        return 0
    fi

    if [[ "$DEPLOYMENT_STARTED" != "true" ]]; then
        log_info "Deployment did not start, no rollback needed"
        return 0
    fi

    show_progress "Performing automatic rollback"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would rollback to previous state"
        log_info "[DRY-RUN] Previous image: $PREVIOUS_IMAGE"
        log_info "[DRY-RUN] Previous replicas: $PREVIOUS_REPLICAS"
        return 0
    fi

    if [[ -z "$PREVIOUS_IMAGE" || "$PREVIOUS_IMAGE" == "unknown" ]]; then
        log_warning "No previous image state captured, cannot rollback"
        return 1
    fi

    log_warning "Rolling back to previous image: $PREVIOUS_IMAGE"

    # Get all deployments
    local deployments
    deployments=$(kubectl get deployments -n "$NAMESPACE" -o jsonpath='{.items[*].metadata.name}' 2>/dev/null)

    if [[ -z "$deployments" ]]; then
        log_warning "No deployments found to rollback"
        return 0
    fi

    # Rollback each deployment
    for deployment in $deployments; do
        log_info "Rolling back deployment: $deployment"

        if kubectl rollout undo deployment "$deployment" \
            -n "$NAMESPACE" \
            --to-revision=0 \
            --timeout="${HEALTH_CHECK_TIMEOUT}s" 2>/dev/null; then
            log_success "Rollback successful: $deployment"
        else
            log_error "Failed to rollback: $deployment"
            return 1
        fi
    done

    # Wait for rollback to complete
    if kubectl rollout status deployment \
        -n "$NAMESPACE" \
        --timeout="${HEALTH_CHECK_TIMEOUT}s" 2>/dev/null; then
        log_success "Rollback completed successfully"
        return 0
    else
        log_error "Rollback did not complete within timeout"
        return 1
    fi
}

save_deployment_log() {
    if [[ "$DRY_RUN" == "true" ]]; then
        return 0
    fi

    local log_file="/tmp/cre_deploy_log_${DEPLOYMENT_ID}.txt"

    {
        echo "Deployment Log: $DEPLOYMENT_ID"
        echo "Timestamp: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
        echo "Project: $PROJECT_ID"
        echo "Cluster: $CLUSTER_NAME"
        echo "Namespace: $NAMESPACE"
        echo "Image: ${REGION}-docker.pkg.dev/${PROJECT_ID}/cre/${IMAGE_NAME}:${IMAGE_TAG}"
        echo ""
        echo "=== Pod Logs ==="
        kubectl logs -n "$NAMESPACE" --all-containers=true --tail=50 2>/dev/null || true
        echo ""
        echo "=== Pod Descriptions ==="
        kubectl describe pods -n "$NAMESPACE" 2>/dev/null || true
        echo ""
        echo "=== Events ==="
        kubectl get events -n "$NAMESPACE" --sort-by='.lastTimestamp' 2>/dev/null || true
    } > "$log_file"

    log_info "Deployment logs saved: $log_file"
}

# =============================================================================
# SMOKE TEST FUNCTIONS
# =============================================================================

run_smoke_tests() {
    if [[ "$RUN_SMOKE_TESTS" != "true" ]]; then
        return 0
    fi

    show_progress "Running smoke tests"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would run smoke tests"
        return 0
    fi

    # Get service endpoint
    local service_ip
    service_ip=$(kubectl get service -n "$NAMESPACE" -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    if [[ -z "$service_ip" ]]; then
        log_warning "Service LoadBalancer IP not assigned yet (may be pending)"
        return 0
    fi

    local service_url="http://${service_ip}:4142"

    log_info "Testing service endpoint: $service_url"

    # Test health endpoint with retries
    local max_attempts=5
    local attempt=0

    while [[ $attempt -lt $max_attempts ]]; do
        if curl -f -s --connect-timeout 5 "${service_url}/status.json" | jq empty 2>/dev/null; then
            log_success "Health check passed"
            return 0
        fi
        ((attempt++))
        log_warning "Health check attempt $attempt/$max_attempts failed"
        sleep 3
    done

    log_error "Health check failed after $max_attempts attempts"
    return 1
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    local start_time end_time duration
    local exit_code=0

    start_time=$(date +%s)

    log_info "Starting GKE deployment: ${SCRIPT_NAME} v${SCRIPT_VERSION}"
    log_info "Deployment ID: ${DEPLOYMENT_ID}"
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
    echo "  Manifest Dir:   ${DEFAULT_K8S_MANIFEST_DIR}"
    echo "  Image:          ${IMAGE_NAME}:${IMAGE_TAG}"
    echo "  Auto Rollback:  $AUTO_ROLLBACK"
    echo "  Skip Build:     $SKIP_BUILD"
    echo "  Skip Health:    $SKIP_HEALTH_CHECK"
    echo "  Dry Run:        $DRY_RUN"
    echo

    # Configure kubectl
    if ! configure_kubectl; then
        exit_code=5
    fi

    # Verify cluster access
    if [[ $exit_code -eq 0 ]] && ! verify_cluster_access; then
        exit_code=5
    fi

    # Ensure namespace
    if [[ $exit_code -eq 0 ]] && ! ensure_namespace; then
        exit_code=5
    fi

    # Build image
    if [[ $exit_code -eq 0 && "$SKIP_BUILD" != "true" ]]; then
        if ! build_docker_image; then
            exit_code=3
        fi
    fi

    # Push image
    if [[ $exit_code -eq 0 && "$SKIP_BUILD" != "true" ]]; then
        if ! push_docker_image; then
            exit_code=4
        fi
    fi

    # Apply manifests
    if [[ $exit_code -eq 0 ]] && ! apply_kubernetes_manifests; then
        exit_code=5
    fi

    # Wait for rollout
    if [[ $exit_code -eq 0 ]] && ! wait_for_deployment; then
        exit_code=5
    fi

    # Verify comprehensive deployment health
    if [[ $exit_code -eq 0 ]] && ! verify_deployment_health; then
        exit_code=6
        log_error "Deployment health verification failed"
        save_deployment_log
    fi

    # Run smoke tests
    if [[ $exit_code -eq 0 && "$RUN_SMOKE_TESTS" == "true" ]]; then
        if ! run_smoke_tests; then
            exit_code=7
            log_error "Smoke tests failed"
            save_deployment_log
        fi
    fi

    # Handle success or failure
    if [[ $exit_code -eq 0 ]]; then
        # Calculate duration
        end_time=$(date +%s)
        duration=$((end_time - start_time))

        echo
        log_success "GKE deployment completed successfully!"
        log_info "Namespace: $NAMESPACE"
        log_info "Image: ${REGION}-docker.pkg.dev/${PROJECT_ID}/cre/${IMAGE_NAME}:${IMAGE_TAG}"
        log_info "Duration: ${duration}s"
        log_info "Deployment ID: $DEPLOYMENT_ID"

        # Show access info
        local service_ip
        service_ip=$(kubectl get service -n "$NAMESPACE" -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "pending")
        log_info "Service IP: $service_ip"

        echo
        log_info "Deployment resources:"
        kubectl get all -n "$NAMESPACE" 2>/dev/null || true

        return 0
    else
        # Handle failure
        echo
        log_error "Deployment failed with exit code: $exit_code"

        if [[ "$AUTO_ROLLBACK" == "true" && "$ROLLBACK_ON_FAILURE" == "true" ]]; then
            echo
            log_warning "Attempting automatic rollback..."

            if perform_automatic_rollback; then
                log_success "Automatic rollback completed successfully"
                log_info "Previous deployment has been restored"
            else
                log_error "Automatic rollback failed"
                log_error "Manual intervention may be required"
                exit_code=8
            fi
        else
            log_warning "Automatic rollback is disabled"
            log_info "Manual rollback may be required"
        fi

        save_deployment_log

        return $exit_code
    fi
}

# Trap errors
trap 'log_error "Script failed at line $LINENO"' ERR

# Run main function
main "$@"
