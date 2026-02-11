# CRE Security Scanning Infrastructure

Complete security validation system for the Common Runtime Environment (CRE) project with Trivy vulnerability scanning and Syft SBOM generation.

## Quick Start

```bash
# Build image and run full security scan with automatic tool installation
bash scripts/security-scan.sh --docker-build --install-tools

# View results
cat security-reports/security-report.md
cat security-reports/trivy-scan.txt
```

## Contents

### Documentation Files

| File | Description |
|------|-------------|
| `README.md` | This file - overview and quick start |
| `SETUP_GUIDE.md` | Complete setup and usage instructions |
| `SECURITY_REPORT_SUMMARY.md` | Detailed security validation report |
| `EXAMPLE_SCAN_EXECUTION.md` | Example commands and expected outputs |

### Sample Artifacts

| File | Description |
|------|-------------|
| `SAMPLE_VULNERABILITY_REPORT.json` | Example Trivy vulnerability scan output |
| `SAMPLE_SBOM_CYCLONEDX.json` | Example CycloneDX SBOM output |

### Scripts

| File | Location | Description |
|------|----------|-------------|
| `security-scan.sh` | `/home/user/cre/scripts/` | Main security scanning script |
| `cloudbuild.yaml` | `/home/user/cre/` | GCP Cloud Build configuration |

## Tools & Formats

### Scanning Tools

| Tool | Version | Purpose |
|------|---------|---------|
| **Trivy** | 0.50.1 | Container vulnerability scanning |
| **Syft** | 1.18.1 | SBOM generation |

### Output Formats

#### Vulnerability Reports (Trivy)
- **trivy-scan.txt** - Human-readable text format
- **trivy-scan.json** - Machine-readable JSON format
- **trivy-scan.sarif** - SARIF format for CI/CD integration

#### Software Bill of Materials (Syft)
- **sbom.spdx.json** - SPDX JSON format (international standard)
- **sbom.cyclonedx.json** - CycloneDX JSON format (GCP compatible)
- **sbom.cyclonedx.xml** - CycloneDX XML format

## Usage Examples

### Basic Scanning

```bash
# Full scan with automatic setup
bash scripts/security-scan.sh --docker-build --install-tools

# Scan existing image
docker build -t cre:0.3.0 -f Dockerfile .
bash scripts/security-scan.sh --image cre:0.3.0 --install-tools
```

### Custom Scanning Options

```bash
# Only vulnerabilities (skip SBOM)
bash scripts/security-scan.sh --image cre:0.3.0 --skip-sbom --install-tools

# Only SBOM generation (skip vulnerabilities)
bash scripts/security-scan.sh --image cre:0.3.0 --skip-trivy --install-tools

# Custom severity levels (CRITICAL only)
bash scripts/security-scan.sh --image cre:0.3.0 --severity CRITICAL --skip-sbom

# Custom output directory
bash scripts/security-scan.sh --image cre:0.3.0 --output ./my-scans
```

### GCP Cloud Build Integration

```bash
# Trigger automatic scanning via Cloud Build
gcloud builds submit --config=cloudbuild.yaml

# View results
gsutil ls gs://PROJECT-security-reports/cre/
gsutil cp -r gs://PROJECT-security-reports/cre/LATEST ./reports
```

## Analysis Examples

### Vulnerability Analysis (requires jq)

```bash
# Count vulnerabilities by severity
cat security-reports/trivy-scan.json | jq '.Results[]?.Vulnerabilities[] | .Severity' | sort | uniq -c

# Find CRITICAL vulnerabilities
cat security-reports/trivy-scan.json | jq '.Results[]?.Vulnerabilities[] | select(.Severity=="CRITICAL")'

# List packages with vulnerabilities
cat security-reports/trivy-scan.json | jq '.Results[]?.Vulnerabilities[] | .PkgName' | sort | uniq
```

### SBOM Analysis (requires jq)

```bash
# Count total packages
cat security-reports/sbom.cyclonedx.json | jq '.components | length'

# List all packages with versions
cat security-reports/sbom.cyclonedx.json | jq '.components[] | "\(.name)@\(.version)"' | sort

# Extract PURLs for supply chain tracking
cat security-reports/sbom.cyclonedx.json | jq '.components[] | .purl' | sort
```

## Security Scanning Process

### 1. Local Development

```bash
# Build and scan
bash scripts/security-scan.sh --docker-build --install-tools

# Review results
cat security-reports/security-report.md
```

### 2. Remediation

```bash
# Fix identified vulnerabilities
# - Update base images
# - Apply security patches
# - Remove unnecessary packages

# Re-build and re-scan
bash scripts/security-scan.sh --docker-build --install-tools
```

### 3. CI/CD Integration

```bash
# Cloud Build automatically scans on each push
git push origin main

# View results in GCP Console or Cloud Storage
gsutil ls gs://PROJECT-security-reports/cre/
```

### 4. Compliance & Archival

```bash
# Archive scan results with release
tar -czf cre-0.3.0-security-scan.tar.gz security-reports/
sha256sum cre-0.3.0-security-scan.tar.gz > cre-0.3.0-security-scan.tar.gz.sha256

# Include in marketplace submission
# - SBOMs for supply chain transparency
# - Scan reports for security review
# - Vulnerability remediation documentation
```

## Files Generated

After running a scan, you'll find:

```
security-reports/
├── trivy-scan.txt              # Human-readable vulnerability report
├── trivy-scan.json             # Machine-readable vulnerability data
├── trivy-scan.sarif            # CI/CD integration format
├── sbom.spdx.json              # SPDX format SBOM
├── sbom.cyclonedx.json         # CycloneDX format SBOM (JSON)
├── sbom.cyclonedx.xml          # CycloneDX format SBOM (XML)
└── security-report.md          # Executive summary
```

## GCP Marketplace Readiness

This security infrastructure meets GCP Marketplace requirements:

- ✓ **Vulnerability Scanning** - Automated with Trivy
- ✓ **SBOM Generation** - Multiple formats (SPDX, CycloneDX)
- ✓ **CI/CD Integration** - Cloud Build pipeline
- ✓ **Artifact Registry Ready** - Multi-architecture support
- ✓ **Supply Chain Security** - Complete transparency
- ✓ **Compliance Documentation** - Comprehensive reports

## Troubleshooting

### Trivy Installation Issues

```bash
# Manual installation
TRIVY_VERSION="0.50.1"
curl -L -o /tmp/trivy.tar.gz \
  "https://github.com/aquasecurity/trivy/releases/download/v${TRIVY_VERSION}/trivy_${TRIVY_VERSION}_Linux-64bit.tar.gz"
tar -xzf /tmp/trivy.tar.gz -C /usr/local/bin/
chmod +x /usr/local/bin/trivy
```

### Syft Installation Issues

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

# Then run the scan
bash scripts/security-scan.sh --image cre:0.3.0 --install-tools
```

## Documentation Links

### In This Directory
- **SETUP_GUIDE.md** - Complete implementation instructions
- **SECURITY_REPORT_SUMMARY.md** - Detailed security validation report
- **EXAMPLE_SCAN_EXECUTION.md** - Example commands and outputs
- **SAMPLE_VULNERABILITY_REPORT.json** - Example Trivy output
- **SAMPLE_SBOM_CYCLONEDX.json** - Example SBOM output

### External References
- [Trivy Documentation](https://aquasecurity.github.io/trivy/)
- [Syft Documentation](https://github.com/anchore/syft)
- [SPDX Specification](https://spdx.github.io/)
- [CycloneDX Specification](https://cyclonedx.org/)
- [GCP Artifact Registry Security](https://cloud.google.com/artifact-registry/docs/security)

## Next Steps

1. **Review** - Read SETUP_GUIDE.md for complete details
2. **Test** - Run local scan: `bash scripts/security-scan.sh --docker-build --install-tools`
3. **Integrate** - Configure Cloud Build for automated scanning
4. **Monitor** - Track vulnerabilities over time
5. **Submit** - Include security artifacts in marketplace submission

## Support

For issues or questions about security scanning:
- Check SETUP_GUIDE.md for troubleshooting
- Review EXAMPLE_SCAN_EXECUTION.md for usage examples
- See SECURITY_REPORT_SUMMARY.md for detailed explanations

---

**Project**: Common Runtime Environment (CRE)  
**Version**: 0.3.0  
**Status**: Ready for GCP Marketplace Submission  
**Last Updated**: 2024-02-11

