#!/bin/bash
#
# CRE SBOM Generation Script
# Generates Software Bill of Materials in multiple formats
#
# Usage:
#   ./scripts/generate-sbom.sh [OPTIONS]
#
# Options:
#   -i, --image IMAGE       Docker image to scan (required)
#   -v, --version VERSION   Version tag (default: latest)
#   -o, --output DIR        Output directory (default: ./sbom-output)
#   -f, --formats FORMATS   Formats to generate (default: all)
#                           Options: spdx, cyclonedx-json, cyclonedx-xml
#   -s, --scan             Run Trivy security scan
#   -h, --help             Show this help message
#
# Examples:
#   ./scripts/generate-sbom.sh -i myimage:1.0.0
#   ./scripts/generate-sbom.sh -i gcr.io/myproject/app:latest -s
#   ./scripts/generate-sbom.sh -i registry/image:v1.0 -o ./artifacts -f spdx
#
# Environment:
#   SYFT_VERSION           Syft version to use (default: 1.18.1)
#   TRIVY_VERSION          Trivy version to use (default: 0.48.1)

set -euo pipefail

# Default values
DOCKER_IMAGE=""
VERSION="latest"
OUTPUT_DIR="./sbom-output"
GENERATE_FORMATS=("spdx" "cyclonedx-json" "cyclonedx-xml")
RUN_SCAN=false
SYFT_VERSION="${SYFT_VERSION:-1.18.1}"
TRIVY_VERSION="${TRIVY_VERSION:-0.48.1}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}ℹ${NC} $*"
}

log_success() {
    echo -e "${GREEN}✓${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}⚠${NC} $*"
}

log_error() {
    echo -e "${RED}✗${NC} $*" >&2
}

show_help() {
    grep '^#' "$0" | grep -v '#!/bin/bash' | sed 's/^# //' | sed 's/^#//'
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -i|--image)
                DOCKER_IMAGE="$2"
                shift 2
                ;;
            -v|--version)
                VERSION="$2"
                shift 2
                ;;
            -o|--output)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            -f|--formats)
                IFS=',' read -ra GENERATE_FORMATS <<< "$2"
                shift 2
                ;;
            -s|--scan)
                RUN_SCAN=true
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

# Validate prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check Docker
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed. Please install Docker to continue."
        exit 1
    fi
    log_success "Docker found: $(docker --version)"

    # Check if image exists
    if ! docker image inspect "$DOCKER_IMAGE" &> /dev/null; then
        log_warn "Docker image not found locally, will attempt to pull..."
        if ! docker pull "$DOCKER_IMAGE"; then
            log_error "Failed to pull Docker image: $DOCKER_IMAGE"
            exit 1
        fi
    fi
    log_success "Docker image available: $DOCKER_IMAGE"
}

# Install Syft if not present
install_syft() {
    log_info "Checking for Syft v${SYFT_VERSION}..."

    if command -v syft &> /dev/null && syft version | grep -q "$SYFT_VERSION"; then
        log_success "Syft v${SYFT_VERSION} already installed"
        return 0
    fi

    log_info "Installing Syft v${SYFT_VERSION}..."

    ARCH=$(uname -m)
    case "$ARCH" in
        x86_64)
            SYFT_ARCH="amd64"
            ;;
        aarch64|arm64)
            SYFT_ARCH="arm64"
            ;;
        *)
            log_error "Unsupported architecture: $ARCH"
            exit 1
            ;;
    esac

    DOWNLOAD_URL="https://github.com/anchore/syft/releases/download/v${SYFT_VERSION}/syft_${SYFT_VERSION}_linux_${SYFT_ARCH}.tar.gz"

    if ! curl -sSL "$DOWNLOAD_URL" -o /tmp/syft.tar.gz; then
        log_error "Failed to download Syft from $DOWNLOAD_URL"
        exit 1
    fi

    tar -xzf /tmp/syft.tar.gz -C /tmp
    rm -f /tmp/syft.tar.gz

    if ! sudo mv /tmp/syft /usr/local/bin/syft; then
        # Try without sudo if that fails
        mv /tmp/syft "$HOME/.local/bin/syft" || {
            log_error "Failed to install Syft to a location in PATH"
            exit 1
        }
    fi

    chmod +x /usr/local/bin/syft 2>/dev/null || chmod +x "$HOME/.local/bin/syft"

    log_success "Syft v${SYFT_VERSION} installed"
}

# Install Trivy if scanning is requested
install_trivy() {
    if [ "$RUN_SCAN" != true ]; then
        return 0
    fi

    log_info "Checking for Trivy v${TRIVY_VERSION}..."

    if command -v trivy &> /dev/null && trivy version | grep -q "$TRIVY_VERSION"; then
        log_success "Trivy v${TRIVY_VERSION} already installed"
        return 0
    fi

    log_info "Installing Trivy v${TRIVY_VERSION}..."

    ARCH=$(uname -m)
    case "$ARCH" in
        x86_64)
            TRIVY_ARCH="64bit"
            ;;
        aarch64|arm64)
            TRIVY_ARCH="ARM64"
            ;;
        *)
            log_error "Unsupported architecture: $ARCH"
            exit 1
            ;;
    esac

    DOWNLOAD_URL="https://github.com/aquasecurity/trivy/releases/download/v${TRIVY_VERSION}/trivy_${TRIVY_VERSION}_Linux-${TRIVY_ARCH}.tar.gz"

    if ! curl -sSL "$DOWNLOAD_URL" -o /tmp/trivy.tar.gz; then
        log_error "Failed to download Trivy from $DOWNLOAD_URL"
        exit 1
    fi

    tar -xzf /tmp/trivy.tar.gz -C /tmp trivy
    rm -f /tmp/trivy.tar.gz

    if ! sudo mv /tmp/trivy /usr/local/bin/trivy; then
        mv /tmp/trivy "$HOME/.local/bin/trivy" || {
            log_error "Failed to install Trivy to a location in PATH"
            exit 1
        }
    fi

    chmod +x /usr/local/bin/trivy 2>/dev/null || chmod +x "$HOME/.local/bin/trivy"

    log_success "Trivy v${TRIVY_VERSION} installed"
}

# Create output directory
setup_output_dir() {
    if [ ! -d "$OUTPUT_DIR" ]; then
        mkdir -p "$OUTPUT_DIR"
        log_success "Created output directory: $OUTPUT_DIR"
    else
        log_info "Using output directory: $OUTPUT_DIR"
    fi
}

# Generate SBOM in SPDX format
generate_spdx() {
    local output_file="$OUTPUT_DIR/sbom.spdx.json"
    log_info "Generating SPDX SBOM..."

    if syft "$DOCKER_IMAGE" --output spdx-json --file "$output_file"; then
        local size=$(stat -c%s "$output_file" 2>/dev/null || stat -f%z "$output_file")
        log_success "SPDX SBOM generated ($(( size / 1024 )) KB)"
        return 0
    else
        log_error "Failed to generate SPDX SBOM"
        return 1
    fi
}

# Generate SBOM in CycloneDX JSON format
generate_cyclonedx_json() {
    local output_file="$OUTPUT_DIR/sbom.cyclonedx.json"
    log_info "Generating CycloneDX JSON SBOM..."

    if syft "$DOCKER_IMAGE" --output cyclonedx-json --file "$output_file"; then
        local size=$(stat -c%s "$output_file" 2>/dev/null || stat -f%z "$output_file")
        log_success "CycloneDX JSON SBOM generated ($(( size / 1024 )) KB)"
        return 0
    else
        log_error "Failed to generate CycloneDX JSON SBOM"
        return 1
    fi
}

# Generate SBOM in CycloneDX XML format
generate_cyclonedx_xml() {
    local output_file="$OUTPUT_DIR/sbom.cyclonedx.xml"
    log_info "Generating CycloneDX XML SBOM..."

    if syft "$DOCKER_IMAGE" --output cyclonedx --file "$output_file"; then
        local size=$(stat -c%s "$output_file" 2>/dev/null || stat -f%z "$output_file")
        log_success "CycloneDX XML SBOM generated ($(( size / 1024 )) KB)"
        return 0
    else
        log_error "Failed to generate CycloneDX XML SBOM"
        return 1
    fi
}

# Generate metadata manifest
generate_metadata() {
    local output_file="$OUTPUT_DIR/sbom-manifest.json"
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    log_info "Generating SBOM metadata..."

    cat > "$output_file" << EOF
{
  "metadata": {
    "timestamp": "${timestamp}",
    "version": "${VERSION}",
    "image": "${DOCKER_IMAGE}",
    "generator": "syft",
    "syft_version": "${SYFT_VERSION}"
  },
  "artifacts": {
    "sbom_formats": [
EOF

    local first=true
    for format in "${GENERATE_FORMATS[@]}"; do
        if [ "$first" = false ]; then
            echo "," >> "$output_file"
        fi
        case "$format" in
            spdx)
                cat >> "$output_file" << 'EOF'
      "SPDX JSON v2.3"
EOF
                ;;
            cyclonedx-json)
                cat >> "$output_file" << 'EOF'
      "CycloneDX JSON v1.4"
EOF
                ;;
            cyclonedx-xml)
                cat >> "$output_file" << 'EOF'
      "CycloneDX XML v1.4"
EOF
                ;;
        esac
        first=false
    done

    cat >> "$output_file" << EOF
    ]
  }
}
EOF

    log_success "SBOM metadata generated"
}

# Run security scan with Trivy
run_security_scan() {
    if [ "$RUN_SCAN" != true ]; then
        return 0
    fi

    local output_file="$OUTPUT_DIR/trivy-results.json"
    log_info "Running security scan with Trivy..."

    if trivy image \
        --severity CRITICAL,HIGH \
        --format json \
        --output "$output_file" \
        "$DOCKER_IMAGE"; then

        local size=$(stat -c%s "$output_file" 2>/dev/null || stat -f%z "$output_file")
        log_success "Security scan completed ($(( size / 1024 )) KB)"

        # Print summary
        echo ""
        log_info "Vulnerability Summary:"
        trivy image \
            --severity CRITICAL,HIGH \
            --format table \
            "$DOCKER_IMAGE" || true
        echo ""

        return 0
    else
        log_error "Security scan failed"
        return 1
    fi
}

# Validate generated files
validate_sbom() {
    log_info "Validating SBOM files..."

    local has_error=false

    for format in "${GENERATE_FORMATS[@]}"; do
        case "$format" in
            spdx)
                if [ ! -f "$OUTPUT_DIR/sbom.spdx.json" ]; then
                    log_error "Missing SPDX SBOM file"
                    has_error=true
                elif ! jq empty "$OUTPUT_DIR/sbom.spdx.json" 2>/dev/null; then
                    log_error "Invalid SPDX JSON syntax"
                    has_error=true
                else
                    log_success "SPDX SBOM valid"
                fi
                ;;
            cyclonedx-json)
                if [ ! -f "$OUTPUT_DIR/sbom.cyclonedx.json" ]; then
                    log_error "Missing CycloneDX JSON SBOM file"
                    has_error=true
                elif ! jq empty "$OUTPUT_DIR/sbom.cyclonedx.json" 2>/dev/null; then
                    log_error "Invalid CycloneDX JSON syntax"
                    has_error=true
                else
                    log_success "CycloneDX JSON SBOM valid"
                fi
                ;;
            cyclonedx-xml)
                if [ ! -f "$OUTPUT_DIR/sbom.cyclonedx.xml" ]; then
                    log_error "Missing CycloneDX XML SBOM file"
                    has_error=true
                else
                    log_success "CycloneDX XML SBOM valid"
                fi
                ;;
        esac
    done

    [ "$has_error" = false ] && return 0 || return 1
}

# Print summary
print_summary() {
    echo ""
    echo -e "${BLUE}════════════════════════════════════════${NC}"
    echo -e "${GREEN}   SBOM Generation Complete${NC}"
    echo -e "${BLUE}════════════════════════════════════════${NC}"
    echo ""
    echo "Generated artifacts:"
    ls -lh "$OUTPUT_DIR"/ | grep -v '^total' | awk '{print "  " $9 " (" $5 ")"}'
    echo ""
    echo "Output directory: $OUTPUT_DIR"
    echo ""
    echo "Next steps:"
    echo "  1. Review SBOM files for completeness"
    if [ "$RUN_SCAN" = true ]; then
        echo "  2. Review Trivy security findings"
        echo "  3. Address any critical/high severity vulnerabilities"
    fi
    echo "  4. Use SBOM for GCP Marketplace submission"
    echo ""
    echo -e "${BLUE}════════════════════════════════════════${NC}"
}

# Main execution
main() {
    echo -e "${BLUE}╔════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║    CRE SBOM Generation Script          ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════╝${NC}"
    echo ""

    parse_args "$@"

    # Validate required arguments
    if [ -z "$DOCKER_IMAGE" ]; then
        log_error "Docker image is required"
        echo ""
        show_help
        exit 1
    fi

    log_info "Configuration:"
    echo "  Image:    $DOCKER_IMAGE"
    echo "  Version:  $VERSION"
    echo "  Output:   $OUTPUT_DIR"
    echo "  Formats:  ${GENERATE_FORMATS[*]}"
    echo "  Scan:     $RUN_SCAN"
    echo ""

    check_prerequisites
    install_syft
    install_trivy
    setup_output_dir

    echo ""

    # Generate SBOM in requested formats
    for format in "${GENERATE_FORMATS[@]}"; do
        case "$format" in
            spdx)
                generate_spdx || exit 1
                ;;
            cyclonedx-json)
                generate_cyclonedx_json || exit 1
                ;;
            cyclonedx-xml)
                generate_cyclonedx_xml || exit 1
                ;;
            *)
                log_error "Unknown format: $format"
                exit 1
                ;;
        esac
    done

    generate_metadata
    run_security_scan || true
    validate_sbom || exit 1

    print_summary
}

# Run main function
main "$@"
