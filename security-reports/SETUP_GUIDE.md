# CRE Security Scanning Setup & Validation Guide

This document provides instructions for setting up and using the security scanning infrastructure for CRE (Common Runtime Environment).

## Overview

The security scanning validates Docker images with:
- **Trivy** - Container vulnerability scanner
- **Syft** - SBOM (Software Bill of Materials) generator

### Artifacts Generated
1. **Vulnerability Reports** (Trivy)
   - JSON format for machine processing
   - Text format for human review
   - SARIF format for CI/CD integration

2. **Software Bill of Materials** (Syft)
   - SPDX JSON format (industry standard)
   - CycloneDX JSON format (GCP compatible)
   - CycloneDX XML format (alternative)

## Prerequisites

### Local Development
```bash
# Docker (for image building and scanning)
which docker

# Optional: Trivy (for vulnerability scanning)
which trivy || bash scripts/security-scan.sh --install-tools --skip-sbom

# Optional: Syft (for SBOM generation)
which syft || bash scripts/security-scan.sh --install-tools --skip-trivy
```

### GCP Cloud Build
- Project with Cloud Build enabled
- Artifact Registry for image storage
- Cloud Storage bucket for reports
- Appropriate IAM permissions

## Quick Start

### 1. Scan a Docker Image Locally

```bash
# Build CRE image first
docker build -t cre:0.3.0 -f Dockerfile .

# Run security scan with automatic tool installation
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --output ./security-reports \
  --install-tools

# Review results
cat security-reports/security-report.md
cat security-reports/trivy-scan.txt
```

### 2. Generate SBOM Only

```bash
# Install Syft if needed
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --skip-trivy \
  --install-tools

# Results
ls -lh security-reports/sbom.*
```

### 3. Scan with Trivy Only

```bash
# Install Trivy if needed
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --skip-sbom \
  --install-tools

# Results
cat security-reports/trivy-scan.json | jq .
```

## Script Options

```bash
./scripts/security-scan.sh [OPTIONS]

Options:
  --image IMAGE              Docker image to scan (default: cre:0.3.0)
  --severity LEVEL           Severity level (HIGH,CRITICAL default)
  --format FORMAT            Output format (table, json, sarif, cyclonedx, spdx)
  --output DIR               Output directory (default: ./security-reports)
  --install-tools            Install Trivy and Syft if missing
  --skip-trivy               Skip vulnerability scanning
  --skip-sbom                Skip SBOM generation
  --docker-build             Build Docker image before scanning
  --help                     Show help message
```

## Output Files Explained

### Trivy Vulnerability Reports

**trivy-scan.txt** - Human-readable vulnerability report
```
Database: vuln-list
Severity: HIGH,CRITICAL
2024-02-11T10:00:00.000Z

(Output shows CVEs with descriptions, severity, and remediation)
```

**trivy-scan.json** - Machine-readable JSON format
```json
{
  "SchemaVersion": 2,
  "ArtifactName": "cre:0.3.0",
  "Results": [
    {
      "Type": "image",
      "Vulnerabilities": [
        {
          "VulnerabilityID": "CVE-2024-XXXXX",
          "Severity": "CRITICAL",
          "Title": "Vulnerability description",
          "FixedVersion": "X.X.X"
        }
      ]
    }
  ]
}
```

**trivy-scan.sarif** - SARIF format for CI/CD
```json
{
  "version": "2.1.0",
  "runs": [
    {
      "tool": {
        "driver": {
          "name": "Trivy",
          "version": "0.50.1"
        }
      },
      "results": [
        {
          "ruleId": "CVE-2024-XXXXX",
          "level": "error",
          "message": {
            "text": "Vulnerability description"
          }
        }
      ]
    }
  ]
}
```

### SBOM Reports

**sbom.spdx.json** - SPDX format (international standard)
```json
{
  "SPDXID": "SPDXRef-DOCUMENT",
  "spdxVersion": "SPDX-2.3",
  "creationInfo": {
    "created": "2024-02-11T10:00:00Z",
    "creators": ["Tool: syft-1.18.1"]
  },
  "packages": [
    {
      "SPDXID": "SPDXRef-Package",
      "name": "package-name",
      "versionInfo": "1.0.0",
      "downloadLocation": "NOASSERTION"
    }
  ]
}
```

**sbom.cyclonedx.json** - CycloneDX format (GCP compatible)
```json
{
  "bomFormat": "CycloneDX",
  "specVersion": "1.4",
  "version": 1,
  "metadata": {
    "timestamp": "2024-02-11T10:00:00Z",
    "tools": [
      {
        "vendor": "anchore",
        "name": "syft",
        "version": "1.18.1"
      }
    ]
  },
  "components": [
    {
      "type": "library",
      "name": "package-name",
      "version": "1.0.0"
    }
  ]
}
```

## Integration with GCP Cloud Build

### 1. Configure Cloud Build

Update project substitutions in `cloudbuild.yaml`:
```yaml
substitutions:
  _REPO_NAME: 'cre'
  _REPORTS_BUCKET: '${PROJECT_ID}-security-reports'
```

### 2. Create Storage Bucket

```bash
gsutil mb -p YOUR_PROJECT_ID gs://YOUR_PROJECT_ID-security-reports
```

### 3. Grant Cloud Build Permissions

```bash
PROJECT_NUMBER=$(gcloud projects describe YOUR_PROJECT_ID --format='value(projectNumber)')
gcloud projects add-iam-policy-binding YOUR_PROJECT_ID \
  --member="serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com" \
  --role="roles/storage.admin"
```

### 4. Trigger Cloud Build

```bash
# Manual trigger
gcloud builds submit --config=cloudbuild.yaml

# Or configure GitHub integration for automatic triggers
gcloud builds create --project=YOUR_PROJECT_ID \
  --github-owner=joergen7 \
  --github-name=cre \
  --build-config=cloudbuild.yaml
```

## Analyzing Results

### Check Vulnerability Count

```bash
# Count CVEs by severity (requires jq)
cat security-reports/trivy-scan.json | jq '.Results[].Vulnerabilities[] | .Severity' | sort | uniq -c
```

### Extract Package List

```bash
# List all packages from SBOM
cat security-reports/sbom.cyclonedx.json | jq '.components[] | .name' | sort
```

### Find Specific CVE

```bash
# Search for specific CVE
cat security-reports/trivy-scan.json | jq '.Results[].Vulnerabilities[] | select(.VulnerabilityID=="CVE-2024-XXXXX")'
```

## Compliance & Marketplace Readiness

### GCP Marketplace Requirements

✓ **Vulnerability Scanning** - Trivy scans for known CVEs
✓ **SBOM Generation** - Multiple formats for transparency
✓ **CI/CD Integration** - SARIF format for GitHub/Cloud Build
✓ **Documentation** - Detailed security reports

### Security Review Process

1. **Scan Image** - Run Trivy to identify CVEs
2. **Generate SBOM** - Create Syft SBOM for supply chain
3. **Review Results** - Analyze security-report.md
4. **Remediate Issues** - Update Dockerfile/dependencies
5. **Re-scan** - Validate fixes
6. **Archive** - Store reports for audit trail

## Troubleshooting

### Trivy Installation Failed

```bash
# Manual installation
TRIVY_VERSION="0.50.1"
curl -L -o /tmp/trivy.tar.gz \
  "https://github.com/aquasecurity/trivy/releases/download/v${TRIVY_VERSION}/trivy_${TRIVY_VERSION}_Linux-64bit.tar.gz"
tar -xzf /tmp/trivy.tar.gz -C /usr/local/bin/
chmod +x /usr/local/bin/trivy
```

### Syft Installation Failed

```bash
# Manual installation
SYFT_VERSION="1.18.1"
curl -L -o /tmp/syft.tar.gz \
  "https://github.com/anchore/syft/releases/download/v${SYFT_VERSION}/syft_${SYFT_VERSION}_linux_amd64.tar.gz"
tar -xzf /tmp/syft.tar.gz -C /usr/local/bin/
chmod +x /usr/local/bin/syft
```

### Docker Image Not Found

```bash
# Build the image first
docker build -t cre:0.3.0 -f Dockerfile .

# Then scan
bash scripts/security-scan.sh --image cre:0.3.0 --install-tools
```

### Scan Timeout

Increase timeout for large images:
```bash
# Edit cloudbuild.yaml timeout
timeout: 7200s  # 2 hours for large images
```

## Next Steps

1. **Run Initial Scan**
   ```bash
   bash scripts/security-scan.sh --docker-build --install-tools
   ```

2. **Review Results**
   ```bash
   cat security-reports/security-report.md
   ```

3. **Fix Critical Issues**
   - Update base images
   - Apply security patches
   - Remove unnecessary packages

4. **Integrate with CI/CD**
   - Add gcloud command to GitHub Actions
   - Monitor Cloud Build for security checks

5. **Archive for Compliance**
   - Store SBOMs with releases
   - Include in marketplace submission
   - Track vulnerability trends over time

## References

- [Trivy Documentation](https://aquasecurity.github.io/trivy/)
- [Syft Documentation](https://github.com/anchore/syft)
- [SPDX Specification](https://spdx.github.io/)
- [CycloneDX Specification](https://cyclonedx.org/)
- [GCP Artifact Registry Security](https://cloud.google.com/artifact-registry/docs/security)

