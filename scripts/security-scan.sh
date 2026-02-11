#!/bin/bash
#
# CRE Security Scanning Script
# Validates Docker images with Trivy and generates SBOMs
#
# Usage:
#   ./scripts/security-scan.sh [OPTIONS]
#
# Options:
#   --image IMAGE              Docker image to scan (default: cre:0.3.0)
#   --severity LEVEL           Severity level to report (HIGH,CRITICAL default)
#   --format FORMAT            Output format (table, json, sarif, cyclonedx, spdx)
#   --output DIR               Output directory for reports (default: ./security-reports)
#   --install-tools            Install Trivy and Syft if missing
#   --skip-trivy               Skip Trivy vulnerability scanning
#   --skip-sbom                Skip SBOM generation
#   --docker-build             Build the Docker image before scanning
#   --help                     Show this help message

set -euo pipefail

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Configuration
IMAGE="${IMAGE:-cre:0.3.0}"
SEVERITY="${SEVERITY:-HIGH,CRITICAL}"
OUTPUT_FORMAT="${OUTPUT_FORMAT:-table}"
OUTPUT_DIR="${OUTPUT_DIR:-${PROJECT_ROOT}/security-reports}"
INSTALL_TOOLS=false
SKIP_TRIVY=false
SKIP_SBOM=false
DOCKER_BUILD=false
HELP=false

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --image)
                IMAGE="$2"
                shift 2
                ;;
            --severity)
                SEVERITY="$2"
                shift 2
                ;;
            --format)
                OUTPUT_FORMAT="$2"
                shift 2
                ;;
            --output)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            --install-tools)
                INSTALL_TOOLS=true
                shift
                ;;
            --skip-trivy)
                SKIP_TRIVY=true
                shift
                ;;
            --skip-sbom)
                SKIP_SBOM=true
                shift
                ;;
            --docker-build)
                DOCKER_BUILD=true
                shift
                ;;
            --help)
                HELP=true
                shift
                ;;
            *)
                echo "Unknown option: $1"
                exit 1
                ;;
        esac
    done
}

# Show help
show_help() {
    grep "^#" "$0" | head -n 20
    exit 0
}

# Print colored output
print_header() {
    echo -e "${BLUE}=== $1 ===${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Install Trivy
install_trivy() {
    print_header "Installing Trivy"
    
    if command_exists trivy; then
        print_success "Trivy already installed"
        return 0
    fi
    
    OS=$(uname -s | tr '[:upper:]' '[:lower:]')
    ARCH=$(uname -m)
    
    case "$ARCH" in
        x86_64) ARCH="64bit" ;;
        aarch64) ARCH="ARM64" ;;
        arm64) ARCH="ARM64" ;;
    esac
    
    TRIVY_VERSION="0.50.1"
    TRIVY_URL="https://github.com/aquasecurity/trivy/releases/download/v${TRIVY_VERSION}/trivy_${TRIVY_VERSION}_${OS}-${ARCH}.tar.gz"
    
    echo "Downloading Trivy from $TRIVY_URL"
    
    if command_exists curl; then
        curl -L -o /tmp/trivy.tar.gz "$TRIVY_URL" || {
            print_error "Failed to download Trivy"
            return 1
        }
    elif command_exists wget; then
        wget -q -O /tmp/trivy.tar.gz "$TRIVY_URL" || {
            print_error "Failed to download Trivy"
            return 1
        }
    else
        print_error "Neither curl nor wget available"
        return 1
    fi
    
    tar -xzf /tmp/trivy.tar.gz -C /tmp || {
        print_error "Failed to extract Trivy"
        return 1
    }
    
    # Try to install to standard location (may require sudo)
    if [ -w /usr/local/bin ]; then
        mv /tmp/trivy /usr/local/bin/trivy || {
            print_error "Failed to install Trivy to /usr/local/bin"
            return 1
        }
    elif [ -w "${HOME}/.local/bin" ]; then
        mkdir -p "${HOME}/.local/bin"
        mv /tmp/trivy "${HOME}/.local/bin/trivy"
        export PATH="${HOME}/.local/bin:$PATH"
    else
        print_error "Cannot install Trivy - no writable bin directory found"
        return 1
    fi
    
    chmod +x /usr/local/bin/trivy 2>/dev/null || chmod +x "${HOME}/.local/bin/trivy" 2>/dev/null
    
    print_success "Trivy installed successfully"
    trivy version
}

# Install Syft
install_syft() {
    print_header "Installing Syft"
    
    if command_exists syft; then
        print_success "Syft already installed"
        return 0
    fi
    
    OS=$(uname -s | tr '[:upper:]' '[:lower:]')
    ARCH=$(uname -m)
    
    case "$ARCH" in
        x86_64) ARCH="amd64" ;;
        aarch64) ARCH="arm64" ;;
        arm64) ARCH="arm64" ;;
    esac
    
    SYFT_VERSION="1.18.1"
    SYFT_URL="https://github.com/anchore/syft/releases/download/v${SYFT_VERSION}/syft_${SYFT_VERSION}_${OS}_${ARCH}.tar.gz"
    
    echo "Downloading Syft from $SYFT_URL"
    
    if command_exists curl; then
        curl -L -o /tmp/syft.tar.gz "$SYFT_URL" || {
            print_error "Failed to download Syft"
            return 1
        }
    elif command_exists wget; then
        wget -q -O /tmp/syft.tar.gz "$SYFT_URL" || {
            print_error "Failed to download Syft"
            return 1
        }
    else
        print_error "Neither curl nor wget available"
        return 1
    fi
    
    tar -xzf /tmp/syft.tar.gz -C /tmp || {
        print_error "Failed to extract Syft"
        return 1
    }
    
    # Try to install to standard location (may require sudo)
    if [ -w /usr/local/bin ]; then
        mv /tmp/syft /usr/local/bin/syft || {
            print_error "Failed to install Syft to /usr/local/bin"
            return 1
        }
    elif [ -w "${HOME}/.local/bin" ]; then
        mkdir -p "${HOME}/.local/bin"
        mv /tmp/syft "${HOME}/.local/bin/syft"
        export PATH="${HOME}/.local/bin:$PATH"
    else
        print_error "Cannot install Syft - no writable bin directory found"
        return 1
    fi
    
    chmod +x /usr/local/bin/syft 2>/dev/null || chmod +x "${HOME}/.local/bin/syft" 2>/dev/null
    
    print_success "Syft installed successfully"
    syft --version
}

# Build Docker image
build_docker_image() {
    print_header "Building Docker Image"
    
    if [ ! -f "${PROJECT_ROOT}/Dockerfile" ]; then
        print_error "Dockerfile not found at ${PROJECT_ROOT}/Dockerfile"
        return 1
    fi
    
    if command_exists docker; then
        echo "Building image: $IMAGE"
        docker build -t "$IMAGE" -f "${PROJECT_ROOT}/Dockerfile" "${PROJECT_ROOT}" || {
            print_error "Docker build failed"
            return 1
        }
        print_success "Docker image built successfully"
    else
        print_warning "Docker not available - skipping build"
        return 0
    fi
}

# Check if Docker image exists locally
check_image_exists() {
    if command_exists docker; then
        if docker image inspect "$IMAGE" >/dev/null 2>&1; then
            return 0
        fi
    fi
    return 1
}

# Scan with Trivy
scan_with_trivy() {
    print_header "Scanning with Trivy"
    
    if [ "$SKIP_TRIVY" = true ]; then
        print_warning "Trivy scanning skipped"
        return 0
    fi
    
    if ! command_exists trivy; then
        print_error "Trivy not found"
        if [ "$INSTALL_TOOLS" = true ]; then
            install_trivy || {
                print_error "Failed to install Trivy"
                return 1
            }
        else
            print_error "Use --install-tools to install Trivy"
            return 1
        fi
    fi
    
    if ! check_image_exists; then
        print_warning "Docker image $IMAGE not found locally"
        print_warning "Attempting to scan with Trivy (may fail without image)"
    fi
    
    mkdir -p "$OUTPUT_DIR"
    
    # Scan image with Trivy
    echo "Scanning $IMAGE with Trivy..."
    
    # Table format for console output
    trivy image --severity "$SEVERITY" "$IMAGE" 2>&1 | tee "${OUTPUT_DIR}/trivy-scan.txt" || {
        print_warning "Trivy scan completed with warnings"
    }
    
    # JSON format for structured output
    trivy image --severity "$SEVERITY" --format json "$IMAGE" \
        -o "${OUTPUT_DIR}/trivy-scan.json" 2>&1 || {
        print_warning "Trivy JSON output generation had issues"
    }
    
    # SARIF format for CI/CD integration
    trivy image --severity "$SEVERITY" --format sarif "$IMAGE" \
        -o "${OUTPUT_DIR}/trivy-scan.sarif" 2>&1 || {
        print_warning "Trivy SARIF output generation had issues"
    }
    
    print_success "Trivy scan completed"
    echo "Results saved to:"
    echo "  - ${OUTPUT_DIR}/trivy-scan.txt (text format)"
    echo "  - ${OUTPUT_DIR}/trivy-scan.json (JSON format)"
    echo "  - ${OUTPUT_DIR}/trivy-scan.sarif (SARIF format)"
}

# Generate SBOM with Syft
generate_sbom() {
    print_header "Generating SBOM"
    
    if [ "$SKIP_SBOM" = true ]; then
        print_warning "SBOM generation skipped"
        return 0
    fi
    
    if ! command_exists syft; then
        print_error "Syft not found"
        if [ "$INSTALL_TOOLS" = true ]; then
            install_syft || {
                print_error "Failed to install Syft"
                return 1
            }
        else
            print_error "Use --install-tools to install Syft"
            return 1
        fi
    fi
    
    if ! check_image_exists; then
        print_warning "Docker image $IMAGE not found locally"
        print_warning "Cannot generate SBOM without image"
        return 1
    fi
    
    mkdir -p "$OUTPUT_DIR"
    
    # Generate SPDX JSON format
    echo "Generating SPDX JSON SBOM..."
    syft "$IMAGE" -o spdx-json \
        > "${OUTPUT_DIR}/sbom.spdx.json" 2>&1 || {
        print_warning "SPDX JSON SBOM generation had issues"
    }
    
    # Generate CycloneDX JSON format
    echo "Generating CycloneDX JSON SBOM..."
    syft "$IMAGE" -o cyclonedx-json \
        > "${OUTPUT_DIR}/sbom.cyclonedx.json" 2>&1 || {
        print_warning "CycloneDX JSON SBOM generation had issues"
    }
    
    # Generate CycloneDX XML format
    echo "Generating CycloneDX XML SBOM..."
    syft "$IMAGE" -o cyclonedx \
        > "${OUTPUT_DIR}/sbom.cyclonedx.xml" 2>&1 || {
        print_warning "CycloneDX XML SBOM generation had issues"
    }
    
    print_success "SBOM generation completed"
    echo "SBOMs saved to:"
    echo "  - ${OUTPUT_DIR}/sbom.spdx.json (SPDX JSON format)"
    echo "  - ${OUTPUT_DIR}/sbom.cyclonedx.json (CycloneDX JSON format)"
    echo "  - ${OUTPUT_DIR}/sbom.cyclonedx.xml (CycloneDX XML format)"
}

# Generate summary report
generate_summary_report() {
    print_header "Generating Summary Report"
    
    local report_file="${OUTPUT_DIR}/security-report.md"
    
    mkdir -p "$OUTPUT_DIR"
    
    {
        echo "# CRE Security Scan Report"
        echo ""
        echo "**Generated**: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
        echo ""
        echo "**Docker Image**: $IMAGE"
        echo ""
        
        echo "## Vulnerability Scan (Trivy)"
        echo ""
        
        if [ -f "${OUTPUT_DIR}/trivy-scan.json" ]; then
            # Parse JSON for vulnerability summary
            if command_exists jq; then
                local critical=$(jq '[.Results[]?.Misconfigurations[]? | select(.Severity=="CRITICAL")] | length' "${OUTPUT_DIR}/trivy-scan.json" 2>/dev/null || echo "N/A")
                local high=$(jq '[.Results[]?.Misconfigurations[]? | select(.Severity=="HIGH")] | length' "${OUTPUT_DIR}/trivy-scan.json" 2>/dev/null || echo "N/A")
                
                echo "**Summary**:"
                echo ""
                echo "| Severity | Count |"
                echo "|----------|-------|"
                echo "| CRITICAL | $critical |"
                echo "| HIGH     | $high |"
                echo ""
            else
                echo "See detailed results in trivy-scan.json"
                echo ""
            fi
        else
            echo "Trivy scan not available"
            echo ""
        fi
        
        echo "## Software Bill of Materials (SBOM)"
        echo ""
        
        if [ -f "${OUTPUT_DIR}/sbom.spdx.json" ]; then
            echo "**SPDX SBOM Generated**: ✓"
            echo ""
            if command_exists jq; then
                local pkg_count=$(jq '.packages | length' "${OUTPUT_DIR}/sbom.spdx.json" 2>/dev/null || echo "N/A")
                echo "**Total Packages**: $pkg_count"
                echo ""
            fi
        fi
        
        if [ -f "${OUTPUT_DIR}/sbom.cyclonedx.json" ]; then
            echo "**CycloneDX SBOM Generated**: ✓"
            echo ""
        fi
        
        echo "## Artifact Files"
        echo ""
        echo "### Vulnerability Scans"
        echo ""
        if [ -f "${OUTPUT_DIR}/trivy-scan.txt" ]; then
            echo "- **trivy-scan.txt** - Text format vulnerability report"
        fi
        if [ -f "${OUTPUT_DIR}/trivy-scan.json" ]; then
            echo "- **trivy-scan.json** - JSON format vulnerability report"
        fi
        if [ -f "${OUTPUT_DIR}/trivy-scan.sarif" ]; then
            echo "- **trivy-scan.sarif** - SARIF format for CI/CD integration"
        fi
        echo ""
        
        echo "### Software Bill of Materials"
        echo ""
        if [ -f "${OUTPUT_DIR}/sbom.spdx.json" ]; then
            echo "- **sbom.spdx.json** - SPDX JSON format"
        fi
        if [ -f "${OUTPUT_DIR}/sbom.cyclonedx.json" ]; then
            echo "- **sbom.cyclonedx.json** - CycloneDX JSON format"
        fi
        if [ -f "${OUTPUT_DIR}/sbom.cyclonedx.xml" ]; then
            echo "- **sbom.cyclonedx.xml** - CycloneDX XML format"
        fi
        echo ""
        
        echo "## Compliance Notes"
        echo ""
        echo "- All scans performed with industry-standard tools (Trivy, Syft)"
        echo "- Severity levels: $SEVERITY"
        echo "- SBOM formats support GCP Artifact Registry requirements"
        echo "- Results suitable for GCP Marketplace security review"
        echo ""
        
        echo "## Next Steps"
        echo ""
        echo "1. Review vulnerability report: \`cat trivy-scan.txt\`"
        echo "2. Examine SBOM details: \`jq . sbom.cyclonedx.json\`"
        echo "3. Upload to GCP Artifact Registry for integration"
        echo "4. Include SBOM with marketplace submission"
        echo ""
        
    } > "$report_file"
    
    print_success "Summary report generated"
    echo "Report saved to: $report_file"
    cat "$report_file"
}

# Main function
main() {
    parse_args "$@"
    
    if [ "$HELP" = true ]; then
        show_help
    fi
    
    print_header "CRE Security Scanning Validation"
    echo ""
    echo "Image: $IMAGE"
    echo "Severity: $SEVERITY"
    echo "Output Directory: $OUTPUT_DIR"
    echo ""
    
    # Build image if requested
    if [ "$DOCKER_BUILD" = true ]; then
        build_docker_image || exit 1
    fi
    
    # Install tools if requested
    if [ "$INSTALL_TOOLS" = true ]; then
        if [ "$SKIP_TRIVY" = false ]; then
            install_trivy || print_warning "Trivy installation failed"
        fi
        if [ "$SKIP_SBOM" = false ]; then
            install_syft || print_warning "Syft installation failed"
        fi
    fi
    
    # Run scans
    scan_with_trivy
    generate_sbom
    generate_summary_report
    
    echo ""
    print_header "Scanning Complete"
    echo ""
    echo "All results saved to: $OUTPUT_DIR"
    echo ""
    ls -lh "$OUTPUT_DIR" || true
}

# Run main function
main "$@"
