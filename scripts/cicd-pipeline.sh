#!/bin/bash
#
# CRE Platform-Agnostic CI/CD Pipeline
#
# Build → SBOM → (optional) Scan → (optional) Push
# Runs on GCP, AWS, Azure, or any environment with Docker.
#
# Usage:
#   ./scripts/cicd-pipeline.sh [OPTIONS]
#
# Environment (set per cloud):
#   CICD_REGISTRY     - Full registry URL (e.g. us-central1-docker.pkg.dev/PROJECT/cre)
#   CICD_IMAGE        - Image name (default: cre)
#   CICD_TAG         - Tag (default: $VERSION or short SHA)
#   CICD_PUSH        - Set to "true" to push after build
#   CICD_SKIP_SCAN   - Set to "true" to skip Trivy scan
#   CICD_OUTPUT_DIR  - Artifacts output dir (default: ./cicd-artifacts)
#
# Cloud Examples:
#   GCP:  CICD_REGISTRY=us-central1-docker.pkg.dev/myproject/cre CICD_PUSH=true ./scripts/cicd-pipeline.sh
#   AWS:  CICD_REGISTRY=123456789.dkr.ecr.us-east-1.amazonaws.com/cre CICD_PUSH=true ./scripts/cicd-pipeline.sh
#   Azure: CICD_REGISTRY=myregistry.azurecr.io/cre CICD_PUSH=true ./scripts/cicd-pipeline.sh
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

# Configuration from env
VERSION="${VERSION:-0.3.0}"
GIT_SHA="${GIT_SHA:-$(git rev-parse --short HEAD 2>/dev/null || echo "dev")}"
CICD_REGISTRY="${CICD_REGISTRY:-}"
CICD_IMAGE="${CICD_IMAGE:-cre}"
CICD_TAG="${CICD_TAG:-${VERSION}-${GIT_SHA}}"
CICD_PUSH="${CICD_PUSH:-false}"
CICD_SKIP_SCAN="${CICD_SKIP_SCAN:-false}"
CICD_OUTPUT_DIR="${CICD_OUTPUT_DIR:-${PROJECT_ROOT}/cicd-artifacts}"

# Resolve full image reference
if [ -n "$CICD_REGISTRY" ]; then
  IMAGE_FULL="${CICD_REGISTRY}/${CICD_IMAGE}:${CICD_TAG}"
  IMAGE_LATEST="${CICD_REGISTRY}/${CICD_IMAGE}:latest"
else
  IMAGE_FULL="${CICD_IMAGE}:${CICD_TAG}"
  IMAGE_LATEST="${CICD_IMAGE}:latest"
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[CICD]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[CICD]${NC} $*" >&2; }
log_err()  { echo -e "${RED}[CICD]${NC} $*" >&2; }

command_exists() { command -v "$1" >/dev/null 2>&1; }

# =============================================================================
# Step 1: Build
# =============================================================================
build_image() {
  log_info "Step 1/4: Building Docker image..."
  ARCH=$(uname -m | sed 's/x86_64/amd64/;s/aarch64/arm64/')
  PLATFORM="linux/${ARCH}"

  if command_exists docker; then
    if docker buildx bake --help >/dev/null 2>&1; then
      BUILDX_BAKE_ENTITLEMENTS_FS="${BUILDX_BAKE_ENTITLEMENTS_FS:-0}" \
        docker buildx bake --load "$ARCH" 2>/dev/null || \
        docker buildx bake --load 2>/dev/null || true
    fi
    if ! docker image inspect "$IMAGE_FULL" >/dev/null 2>&1; then
      log_info "Using docker build for $PLATFORM..."
      docker build --platform "$PLATFORM" --target runtime -t "$IMAGE_FULL" -t "$IMAGE_LATEST" -f Dockerfile .
    fi
  else
    log_err "Docker not found"
    exit 1
  fi
  log_info "Build complete: $IMAGE_FULL"
}

# =============================================================================
# Step 2: SBOM (runs after build)
# =============================================================================
generate_sbom() {
  log_info "Step 2/4: Generating SBOM..."
  mkdir -p "$CICD_OUTPUT_DIR"

  if ! command_exists syft; then
    log_info "Installing Syft..."
    CURDIR="$(pwd)"
    cd /tmp
    curl -sSfL https://raw.githubusercontent.com/anchore/syft/main/install.sh | sh -s -- -b /usr/local/bin 2>/dev/null || {
      SYFT_VER="1.18.1"
      ARCH=$(uname -m | sed 's/x86_64/amd64/;s/aarch64/arm64/')
      curl -sSL "https://github.com/anchore/syft/releases/download/v${SYFT_VER}/syft_${SYFT_VER}_linux_${ARCH}.tar.gz" | tar -xzf - -C /usr/local/bin
    }
    cd "$CURDIR"
  fi

  syft "$IMAGE_FULL" -o spdx-json > "${CICD_OUTPUT_DIR}/sbom.spdx.json" || log_warn "SPDX SBOM had warnings"
  syft "$IMAGE_FULL" -o cyclonedx-json > "${CICD_OUTPUT_DIR}/sbom.cyclonedx.json" || log_warn "CycloneDX SBOM had warnings"

  log_info "SBOM generated: ${CICD_OUTPUT_DIR}/sbom.spdx.json, sbom.cyclonedx.json"
  ls -la "${CICD_OUTPUT_DIR}"/sbom.* 2>/dev/null || true
}

# =============================================================================
# Step 3: Security scan (optional)
# =============================================================================
run_scan() {
  if [ "$CICD_SKIP_SCAN" = "true" ]; then
    log_info "Step 3/4: Skipping Trivy scan (CICD_SKIP_SCAN=true)"
    return 0
  fi

  log_info "Step 3/4: Running Trivy scan..."
  mkdir -p "$CICD_OUTPUT_DIR"

  if ! command_exists trivy; then
    log_warn "Trivy not found, skipping scan. Install or set CICD_SKIP_SCAN=true"
    return 0
  fi

  trivy image --severity HIGH,CRITICAL --format json --output "${CICD_OUTPUT_DIR}/trivy.json" "$IMAGE_FULL" || true
  trivy image --severity HIGH,CRITICAL --format table "$IMAGE_FULL" || true
  log_info "Scan results: ${CICD_OUTPUT_DIR}/trivy.json"
}

# =============================================================================
# Step 4: Push (optional)
# =============================================================================
push_image() {
  if [ "$CICD_PUSH" != "true" ]; then
    log_info "Step 4/4: Skipping push (CICD_PUSH not set)"
    return 0
  fi

  if [ -z "$CICD_REGISTRY" ]; then
    log_warn "CICD_REGISTRY not set, cannot push"
    return 0
  fi

  log_info "Step 4/4: Pushing to registry..."
  docker push "$IMAGE_FULL"
  docker push "$IMAGE_LATEST"
  log_info "Pushed: $IMAGE_FULL"
}

# =============================================================================
# Main
# =============================================================================
main() {
  log_info "CRE CI/CD Pipeline (platform-agnostic)"
  log_info "Image: $IMAGE_FULL"
  log_info "Output: $CICD_OUTPUT_DIR"
  echo ""

  build_image
  generate_sbom
  run_scan
  push_image

  log_info "Pipeline complete."
  echo ""
  log_info "Artifacts:"
  ls -la "$CICD_OUTPUT_DIR" 2>/dev/null || true
}

main "$@"
