#!/bin/bash
#
# CRE Security Scan Execution Examples
# Shows example commands and expected outputs
#
# Usage: bash EXAMPLE_SCAN_EXECUTION.sh

set -e

echo "=== CRE Security Scanning Examples ==="
echo ""

# Example 1: Local scanning with installation
echo "Example 1: Local image scan with automatic tool installation"
echo "=============================================================="
echo ""
echo "Command:"
echo '  bash scripts/security-scan.sh \'
echo '    --docker-build \'
echo '    --install-tools'
echo ""
echo "This will:"
echo "  1. Build the Docker image (cre:0.3.0)"
echo "  2. Install Trivy if not present"
echo "  3. Install Syft if not present"
echo "  4. Scan the image for vulnerabilities"
echo "  5. Generate SBOMs in 3 formats"
echo "  6. Create a summary report"
echo ""
echo "Expected output:"
echo "  - security-reports/trivy-scan.txt"
echo "  - security-reports/trivy-scan.json"
echo "  - security-reports/trivy-scan.sarif"
echo "  - security-reports/sbom.spdx.json"
echo "  - security-reports/sbom.cyclonedx.json"
echo "  - security-reports/sbom.cyclonedx.xml"
echo "  - security-reports/security-report.md"
echo ""
echo ""

# Example 2: Scan only (image already built)
echo "Example 2: Scan an already-built image"
echo "======================================"
echo ""
echo "Command:"
echo '  docker build -t cre:0.3.0 -f Dockerfile .'
echo '  bash scripts/security-scan.sh --image cre:0.3.0 --install-tools'
echo ""
echo "This will:"
echo "  1. Use existing Docker image"
echo "  2. Install tools if needed"
echo "  3. Perform vulnerability scan"
echo "  4. Generate SBOMs"
echo ""
echo ""

# Example 3: Trivy-only scan
echo "Example 3: Vulnerability scanning only (skip SBOM)"
echo "==================================================="
echo ""
echo "Command:"
echo '  bash scripts/security-scan.sh \'
echo '    --image cre:0.3.0 \'
echo '    --skip-sbom \'
echo '    --install-tools'
echo ""
echo "Output:"
echo "  - security-reports/trivy-scan.txt"
echo "  - security-reports/trivy-scan.json"
echo "  - security-reports/trivy-scan.sarif"
echo ""
echo ""

# Example 4: SBOM-only generation
echo "Example 4: SBOM generation only (skip vulnerability scan)"
echo "=========================================================="
echo ""
echo "Command:"
echo '  bash scripts/security-scan.sh \'
echo '    --image cre:0.3.0 \'
echo '    --skip-trivy \'
echo '    --install-tools'
echo ""
echo "Output:"
echo "  - security-reports/sbom.spdx.json"
echo "  - security-reports/sbom.cyclonedx.json"
echo "  - security-reports/sbom.cyclonedx.xml"
echo ""
echo ""

# Example 5: Analyzing scan results
echo "Example 5: Analyzing vulnerability scan results"
echo "================================================"
echo ""
echo "View vulnerabilities (requires jq):"
echo ""
echo "  # Count vulnerabilities by severity"
echo '  cat security-reports/trivy-scan.json | jq \'
echo '    ".Results[]?.Vulnerabilities[] | .Severity" | sort | uniq -c'
echo ""
echo "  # Find CRITICAL vulnerabilities"
echo '  cat security-reports/trivy-scan.json | jq \'
echo '    ".Results[]?.Vulnerabilities[] | select(.Severity==\"CRITICAL\")"'
echo ""
echo "  # Extract CVE details"
echo '  cat security-reports/trivy-scan.json | jq \'
echo '    ".Results[]?.Vulnerabilities[] | {id: .VulnerabilityID, severity: .Severity, fixed: .FixedVersion}"'
echo ""
echo ""

# Example 6: Analyzing SBOM
echo "Example 6: Analyzing Software Bill of Materials"
echo "==============================================="
echo ""
echo "Extract package information (requires jq):"
echo ""
echo "  # Count packages"
echo '  cat security-reports/sbom.cyclonedx.json | jq ".components | length"'
echo ""
echo "  # List all packages"
echo '  cat security-reports/sbom.cyclonedx.json | jq \'.components[] | "\(.name)@\(.version)"\' | sort'
echo ""
echo "  # Extract Package URLs (PURLs) for supply chain analysis"
echo '  cat security-reports/sbom.cyclonedx.json | jq ".components[] | .purl" | sort'
echo ""
echo "  # Find packages with embedded vulnerabilities"
echo '  cat security-reports/sbom.cyclonedx.json | jq ".vulnerabilities[] | .id" | sort'
echo ""
echo ""

# Example 7: Cloud Build integration
echo "Example 7: GCP Cloud Build integration"
echo "====================================="
echo ""
echo "Command:"
echo "  gcloud builds submit --config=cloudbuild.yaml"
echo ""
echo "This will:"
echo "  1. Trigger Cloud Build pipeline"
echo "  2. Build multi-architecture images"
echo "  3. Run Trivy scanning"
echo "  4. Generate SBOMs"
echo "  5. Push images to Artifact Registry"
echo "  6. Upload reports to Cloud Storage"
echo ""
echo "View results:"
echo "  gsutil ls gs://PROJECT-security-reports/cre/"
echo ""
echo ""

# Example 8: Custom severity levels
echo "Example 8: Scanning with custom severity levels"
echo "================================================"
echo ""
echo "Only show CRITICAL vulnerabilities:"
echo '  bash scripts/security-scan.sh \'
echo '    --image cre:0.3.0 \'
echo '    --severity CRITICAL \'
echo '    --skip-sbom'
echo ""
echo "Show CRITICAL and HIGH:"
echo '  bash scripts/security-scan.sh \'
echo '    --image cre:0.3.0 \'
echo '    --severity CRITICAL,HIGH \'
echo '    --skip-sbom'
echo ""
echo "Show all (CRITICAL, HIGH, MEDIUM, LOW):"
echo '  bash scripts/security-scan.sh \'
echo '    --image cre:0.3.0 \'
echo '    --severity CRITICAL,HIGH,MEDIUM,LOW \'
echo '    --skip-sbom'
echo ""
echo ""

# Example 9: Custom output directory
echo "Example 9: Directing output to custom location"
echo "==============================================="
echo ""
echo "Command:"
echo '  bash scripts/security-scan.sh \'
echo '    --image cre:0.3.0 \'
echo '    --output /tmp/cre-scan-results \'
echo '    --install-tools'
echo ""
echo "Results will be saved to: /tmp/cre-scan-results/"
echo ""
echo ""

# Example 10: Full compliance workflow
echo "Example 10: Complete compliance workflow"
echo "========================================="
echo ""
echo "Step 1: Build image"
echo "  docker build -t cre:0.3.0 -f Dockerfile ."
echo ""
echo "Step 2: Run security scan"
echo '  bash scripts/security-scan.sh \'
echo '    --image cre:0.3.0 \'
echo '    --output ./security-scan-2024-02-11'
echo ""
echo "Step 3: Review results"
echo "  cat ./security-scan-2024-02-11/security-report.md"
echo "  cat ./security-scan-2024-02-11/trivy-scan.txt | head -50"
echo ""
echo "Step 4: Analyze vulnerabilities (if jq available)"
echo '  cat ./security-scan-2024-02-11/trivy-scan.json | jq \'.Results[].Vulnerabilities[] | select(.Severity=="CRITICAL")\''
echo ""
echo "Step 5: Archive for compliance"
echo "  tar -czf cre-0.3.0-security-scan.tar.gz ./security-scan-2024-02-11/"
echo "  sha256sum cre-0.3.0-security-scan.tar.gz > cre-0.3.0-security-scan.tar.gz.sha256"
echo ""
echo "Step 6: Upload to marketplace submission package"
echo "  # Include SBOMs and scan reports in submission"
echo ""
echo ""

echo "=== Additional Commands ==="
echo ""
echo "Install Trivy only:"
echo "  bash scripts/security-scan.sh --install-tools --skip-sbom --skip-trivy"
echo ""
echo "Install Syft only:"
echo "  bash scripts/security-scan.sh --install-tools --skip-trivy --skip-sbom"
echo ""
echo "View help:"
echo "  bash scripts/security-scan.sh --help"
echo ""

