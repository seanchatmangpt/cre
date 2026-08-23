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

# Deployment tracking
DEPLOYMENT_ID="$(date -u +"%Y%m%d_%H%M%S")_deploy"
HEALTH_CHECK_TIMEOUT=300
HEALTH_CHECK_INTERVAL=10

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
    --skip-build            Skip Docker build
    --skip-health-check     Skip deployment health check
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

EXAMPLES:
    # Deploy to default cluster with all checks
    ${SCRIPT_NAME}

    # Deploy to specific cluster and run smoke tests
    ${SCRIPT_NAME} --cluster cre-prod --region us-east1 --smoke-test

    # Skip build and deploy existing image
    ${SCRIPT_NAME} --skip-build --tag v1.2.3

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
            --skip-build)
                SKIP_BUILD=true
                shift
                ;;
            --skip-health-check)
                SKIP_HEALTH_CHECK=true
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
# DEPLOYMENT FUNCTIONS
# =============================================================================

apply_kubernetes_manifests() {
    show_progress "Applying Kubernetes manifests"

    local gcr_image="${REGION}-docker.pkg.dev/${PROJECT_ID}/cre/${IMAGE_NAME}:${IMAGE_TAG}"
    local manifest_dir="${PROJECT_ROOT}/k8s/gcp"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would apply manifests from: $manifest_dir"
        log_info "[DRY-RUN]   Image: $gcr_image"
        log_info "[DRY-RUN]   Namespace: $NAMESPACE"
        return 0
    fi

    # Apply namespace if needed
    ensure_namespace

    # Apply manifests in order
    local manifests=(
        "serviceaccount.yaml"
        "configmap.yaml"
        "secret.yaml"
        "deployment.yaml"
        "service.yaml"
        "hpa.yaml"
        "pdb.yaml"
        "ingress.yaml"
    )

    for manifest in "${manifests[@]}"; do
        local manifest_file="${manifest_dir}/${manifest}"

        if [[ ! -f "$manifest_file" ]]; then
            log_warning "Manifest not found: $manifest_file (skipping)"
            continue
        fi

        log_info "Applying: $manifest"

        # Substitute image and namespace
        if kubectl apply \
            -n "$NAMESPACE" \
            -f "$manifest_file" \
            --dry-run=client \
            -o yaml 2>/dev/null | \
            sed "s|REPLACE_WITH_YOUR_PROJECT_ID|${PROJECT_ID}|g" | \
            sed "s|us-central1-docker.pkg.dev/REPLACE_WITH_YOUR_PROJECT_ID/cre/cre:0.3.0|${gcr_image}|g" | \
            kubectl apply -n "$NAMESPACE" -f -; then
            log_success "  Applied: $manifest"
        else
            log_warning "  Failed to apply: $manifest (continuing)"
        fi
    done

    log_success "Manifests applied"
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

verify_deployment_health() {
    show_progress "Verifying deployment health"

    if [[ "$SKIP_HEALTH_CHECK" == "true" ]]; then
        log_warning "Skipping health check as requested"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify deployment health"
        return 0
    fi

    local max_attempts=$((HEALTH_CHECK_TIMEOUT / HEALTH_CHECK_INTERVAL))
    local attempt=0

    while [[ $attempt -lt $max_attempts ]]; do
        local ready_replicas
        ready_replicas=$(kubectl get deployment \
            -n "$NAMESPACE" \
            -o jsonpath='{.items[0].status.readyReplicas}' 2>/dev/null || echo "0")

        local desired_replicas
        desired_replicas=$(kubectl get deployment \
            -n "$NAMESPACE" \
            -o jsonpath='{.items[0].spec.replicas}' 2>/dev/null || echo "1")

        if [[ "$ready_replicas" == "$desired_replicas" && "$ready_replicas" -gt 0 ]]; then
            log_success "All replicas ready: $ready_replicas/$desired_replicas"

            # Check pod status
            kubectl get pods -n "$NAMESPACE"
            return 0
        fi

        ((attempt++))
        log_info "Waiting for replicas ($attempt/$max_attempts): $ready_replicas/$desired_replicas"
        sleep "$HEALTH_CHECK_INTERVAL"
    done

    log_error "Health check timed out"
    log_error "Pods status:"
    kubectl get pods -n "$NAMESPACE"
    return 1
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

    local service_ip
    service_ip=$(kubectl get service \
        -n "$NAMESPACE" \
        -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    local service_url="http://${service_ip}:4142"

    log_info "Testing service endpoint: $service_url"

    # Test health endpoint
    if curl -f -s "${service_url}/status.json" | jq empty 2>/dev/null; then
        log_success "Health check passed"
    else
        log_error "Health check failed"
        return 1
    fi

    # Test API version endpoint
    if curl -f -s "${service_url}/api/v1/health" | jq empty 2>/dev/null; then
        log_success "API health check passed"
    else
        log_warning "API health check failed (endpoint may not exist)"
    fi

    log_success "Smoke tests passed"
    return 0
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    local start_time end_time duration

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
    echo "  Project:      $PROJECT_ID"
    echo "  Cluster:      $CLUSTER_NAME"
    echo "  Region:       $REGION"
    echo "  Namespace:    $NAMESPACE"
    echo "  Image:        ${IMAGE_NAME}:${IMAGE_TAG}"
    echo "  Skip Build:   $SKIP_BUILD"
    echo "  Dry Run:      $DRY_RUN"
    echo

    # Configure kubectl
    if ! configure_kubectl; then
        exit 5
    fi

    # Verify cluster access
    if ! verify_cluster_access; then
        exit 5
    fi

    # Ensure namespace
    if ! ensure_namespace; then
        exit 5
    fi

    # Build image
    if [[ "$SKIP_BUILD" != "true" ]]; then
        if ! build_docker_image; then
            exit 3
        fi
    fi

    # Push image
    if [[ "$SKIP_BUILD" != "true" ]]; then
        if ! push_docker_image; then
            exit 4
        fi
    fi

    # Apply manifests
    if ! apply_kubernetes_manifests; then
        exit 5
    fi

    # Wait for rollout
    if ! wait_for_deployment; then
        exit 5
    fi

    # Verify health
    if ! verify_deployment_health; then
        exit 6
    fi

    # Run smoke tests
    if [[ "$RUN_SMOKE_TESTS" == "true" ]]; then
        if ! run_smoke_tests; then
            exit 7
        fi
    fi

    # Calculate duration
    end_time=$(date +%s)
    duration=$((end_time - start_time))

    echo
    log_success "GKE deployment completed successfully!"
    log_info "Namespace: $NAMESPACE"
    log_info "Image: ${REGION}-docker.pkg.dev/${PROJECT_ID}/cre/${IMAGE_NAME}:${IMAGE_TAG}"
    log_info "Duration: ${duration}s"

    # Show access info
    local service_ip
    service_ip=$(kubectl get service -n "$NAMESPACE" -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "pending")
    log_info "Service IP: $service_ip"

    return 0
}

# Trap errors
trap 'log_error "Script failed at line $LINENO"' ERR

# Run main function
main "$@"
