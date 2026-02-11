# Trivy Security Scanning in Cloud Build

## Overview

The enhanced `cloudbuild.yaml` integrates **Trivy**, a comprehensive vulnerability scanner for containers and build artifacts. This document describes the Trivy security scanning enhancements and how to use them.

## What is Trivy?

[Trivy](https://github.com/aquasecurity/trivy) is an open-source vulnerability scanner that:

- Scans container images for known vulnerabilities
- Detects exposed secrets and misconfigurations
- Generates reports in multiple formats (JSON, SARIF, HTML)
- Integrates with CI/CD pipelines for automated security checks
- Supports severity-based filtering (CRITICAL, HIGH, MEDIUM, LOW)

## Cloud Build Integration

The enhanced pipeline includes the following Trivy-related steps:

### Step 7: Security Scanning with Trivy

Scans the Docker image for vulnerabilities and generates:
- **JSON Report** (`trivy-results.json`) - Machine-readable detailed findings
- **Table Report** (stdout) - Human-readable summary
- **Extended Report** (stdout) - All severities for reference

```yaml
- id: scan-image-trivy
  name: gcr.io/cloud-builders/docker
  entrypoint: bash
  args:
    - -c
    - |
      trivy image \
        --severity CRITICAL,HIGH \
        --format json \
        --output /workspace/trivy-results.json \
        ${_IMAGE_NAME}:${_VERSION}
```

### Step 8: Validate Trivy Results

This critical step ensures the build fails if vulnerabilities are found:

**Features:**
- Parses JSON report for vulnerability counts
- Separates findings by severity level (CRITICAL, HIGH, MEDIUM, LOW)
- Generates a summary JSON report (`trivy-summary.json`)
- **FAILS BUILD** if any CRITICAL or HIGH vulnerabilities detected
- Logs detailed vulnerability counts and remediation steps

```bash
CRITICAL_COUNT=$(jq '[.Results[]?.Vulnerabilities[]? | select(.Severity=="CRITICAL")] | length' /workspace/trivy-results.json)
HIGH_COUNT=$(jq '[.Results[]?.Vulnerabilities[]? | select(.Severity=="HIGH")] | length' /workspace/trivy-results.json)

if [ "${CRITICAL_COUNT}" -gt 0 ] || [ "${HIGH_COUNT}" -gt 0 ]; then
  echo "❌ BUILD FAILED - Security threshold exceeded"
  exit 1
fi
```

### Step 9: Generate Trivy HTML Report

Produces a professional HTML report (`trivy-report.html`) with:
- Visual vulnerability summary (counts by severity)
- Scan metadata (image, timestamp, scanner version)
- Remediation guidance
- Links to security resources

### Step 10: Upload SBOM and Scan Results to GCS

Uploads multiple report formats to Google Cloud Storage:

| File | Format | Purpose |
|------|--------|---------|
| `trivy-results.json` | JSON | Detailed findings (parsing, analysis) |
| `trivy-summary.json` | JSON | Vulnerability counts and build status |
| `trivy-results.sarif` | SARIF | GitHub Code Scanning integration |
| `trivy-report.html` | HTML | Visual report for stakeholders |

**GCS Location Pattern:**
```
gs://${PROJECT_ID}-cre-artifacts/security/${VERSION}/${COMMIT_SHA}/
```

## Severity Levels and Build Failure Criteria

### Failure Threshold

**Build FAILS on:**
- ❌ CRITICAL vulnerabilities (any count)
- ❌ HIGH severity vulnerabilities (any count)

**Build PASSES on:**
- ✅ Only MEDIUM and LOW vulnerabilities
- ✅ No vulnerabilities

### Severity Definitions

| Severity | CVSS Score | Impact | Action |
|----------|-----------|--------|--------|
| **CRITICAL** | 9.0-10.0 | System compromise possible | Block deployment |
| **HIGH** | 7.0-8.9 | Significant impact | Block deployment |
| **MEDIUM** | 4.0-6.9 | Moderate impact | Review & monitor |
| **LOW** | 0.1-3.9 | Low impact | Document & track |

## Running Locally

### Build and Scan Locally

```bash
# 1. Build image locally
docker build -t cre:test .

# 2. Install Trivy
curl -sfL https://raw.githubusercontent.com/aquasecurity/trivy/main/contrib/install.sh | sh -s -- -b /usr/local/bin

# 3. Scan image
trivy image \
  --severity CRITICAL,HIGH \
  --format json \
  --output trivy-results.json \
  cre:test

# 4. View human-readable report
trivy image \
  --severity CRITICAL,HIGH \
  --format table \
  cre:test
```

### Docker-Based Scanning

Per the CLAUDE.md docker-only workflow:

```bash
# Run scan in Docker
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 sh -c "
  # Install Trivy
  apk add --no-cache curl
  curl -sfL https://raw.githubusercontent.com/aquasecurity/trivy/main/contrib/install.sh | sh -s -- -b /usr/local/bin

  # Scan the image (if available)
  trivy image cre:0.3.0 --severity CRITICAL,HIGH --format json
"
```

## Remediation Workflow

When Trivy finds HIGH or CRITICAL vulnerabilities:

### 1. Identify Vulnerable Components

```bash
# Parse JSON to find affected packages
jq '.Results[].Vulnerabilities[] | {
  "Package": .PkgName,
  "Version": .InstalledVersion,
  "Severity": .Severity,
  "CVE": .VulnerabilityID,
  "FixedVersion": .FixedVersion
}' trivy-results.json
```

### 2. Update Dependencies

- **Base Image:** Update `Dockerfile` to use a newer, patched base image
  ```dockerfile
  # Old
  FROM erlang:28-alpine

  # New (if patch available)
  FROM erlang:28-alpine-OTP-28.2  # Newer OTP patch
  ```

- **Alpine Packages:** Update apk dependencies
  ```dockerfile
  RUN apk add --no-cache \
    openssl-libs-static  # Ensure latest version
  ```

- **Rust/Erlang Dependencies:** Update `Cargo.lock` and `rebar.lock`
  ```bash
  cargo update
  rebar3 upgrade
  ```

### 3. Rebuild and Re-Scan

```bash
# Trigger Cloud Build with security checks
gcloud builds submit . \
  --config cloudbuild.yaml \
  --substitutions=_IMAGE_NAME="us-central1-docker.pkg.dev/PROJECT_ID/cre/cre:v0.3.1"
```

### 4. Verify Results

```bash
# Check GCS for vulnerability summary
gsutil cat gs://PROJECT_ID-cre-artifacts/security/0.3.1/COMMIT_SHA/trivy-summary.json
```

## Report Formats

### JSON Format (`trivy-results.json`)

Complete vulnerability details for programmatic processing:

```json
{
  "Results": [
    {
      "Target": "alpine (3.18.0)",
      "Type": "alpine",
      "Vulnerabilities": [
        {
          "VulnerabilityID": "CVE-2024-1234",
          "PkgName": "openssl",
          "InstalledVersion": "3.1.0",
          "FixedVersion": "3.1.2",
          "Severity": "HIGH",
          "Description": "...",
          "References": ["https://nvd.nist.gov/vuln/detail/CVE-2024-1234"]
        }
      ]
    }
  ]
}
```

### SARIF Format (`trivy-results.sarif`)

Standard format for GitHub Code Scanning and other tools:
- Automatically imported into GitHub security tab
- Compatible with tools like CodeQL and Fortify
- Enables policy enforcement through branch protection

### HTML Report (`trivy-report.html`)

Visual summary for stakeholders:
- Color-coded severity levels
- Vulnerability counts at a glance
- Links to remediation resources
- Professional presentation for compliance reviews

## GCS Artifact Organization

```
gs://PROJECT_ID-cre-artifacts/
├── sbom/
│   └── 0.3.0/
│       └── abc123def/
│           ├── sbom.spdx.json
│           ├── sbom.cyclonedx.json
│           ├── sbom.cyclonedx.xml
│           └── sbom-manifest.json
├── security/
│   └── 0.3.0/
│       └── abc123def/
│           ├── trivy-results.json
│           ├── trivy-summary.json
│           ├── trivy-results.sarif
│           └── trivy-report.html
└── compliance/
    └── 0.3.0/
        └── abc123def/
            └── marketplace-compliance-report.json
```

## Integration with GCP Marketplace

### Trivy in Compliance

Marketplace submission requirements include:
- ✅ Container image vulnerability scanning
- ✅ Security assessment documentation
- ✅ SBOM (Software Bill of Materials)
- ✅ Vulnerability disclosure

The enhanced cloudbuild.yaml provides all these automatically.

### Using Trivy Results in Marketplace Listing

1. Download HTML report from GCS
2. Include vulnerability summary in listing:
   - "Regular security scans with Trivy"
   - "No HIGH/CRITICAL vulnerabilities in latest build"
   - "Full vulnerability reports available upon request"

3. Maintain scan history for compliance audits
4. Use SBOM for supply chain transparency

## Troubleshooting

### Build Fails on Vulnerabilities

**Problem:** Build exits with "BUILD FAILED - Security threshold exceeded"

**Solution:**
```bash
# 1. Review detailed results
gsutil cat gs://PROJECT_ID-cre-artifacts/security/VERSION/COMMIT/trivy-results.json | jq '.'

# 2. Identify affected packages
jq '.Results[].Vulnerabilities[]' trivy-results.json

# 3. Update and rebuild
docker build -t cre:patched .
trivy image cre:patched --severity CRITICAL,HIGH

# 4. Once verified, re-submit build
```

### Trivy Installation Fails

**Problem:** "Downloading Trivy v0.48.1... (failed)"

**Solution:**
```bash
# Check availability manually
curl -I https://github.com/aquasecurity/trivy/releases/download/v0.48.1/trivy_0.48.1_Linux-64bit.tar.gz

# Or update to newer version in cloudbuild.yaml
sed -i 's/TRIVY_VERSION=0.48.1/TRIVY_VERSION=0.49.0/' cloudbuild.yaml
```

### Empty Vulnerability Lists

**Problem:** Trivy scan completes but no vulnerabilities in JSON

**Possible Causes:**
- Image uses very latest base image with patches applied
- Trivy database is outdated (update Trivy version)
- Vulnerabilities not yet in public databases

**Action:** Review detailed scan output:
```bash
trivy image ${IMAGE} --severity LOW
```

## Performance Considerations

### Scan Time

Typical scan times for CRE image:
- First scan: 30-60 seconds (downloads vulnerability DB)
- Subsequent scans: 10-20 seconds (cached DB)

### Optimization Tips

```yaml
# In cloudbuild.yaml - use persistent cache
options:
  machineType: N1_HIGHCPU_8  # 8-CPU for faster scanning
```

### Database Updates

Trivy automatically updates its vulnerability database:
```bash
trivy image --download-db-only  # Pre-download for faster scans
```

## Security Best Practices

1. **Regular Scanning**
   - Scan on every build (not just tagged releases)
   - Scan dependencies weekly
   - Monitor new CVEs for deployed versions

2. **Vulnerability Response**
   - CRITICAL: Fix immediately (within 24 hours)
   - HIGH: Fix within 1-2 weeks
   - MEDIUM: Include in next regular update
   - LOW: Document and track

3. **Policy Enforcement**
   - Reject builds with HIGH/CRITICAL (enforced by cloudbuild.yaml)
   - Document accepted risks for unpatched vulnerabilities
   - Maintain change log for security updates

4. **Compliance**
   - Keep vulnerability reports for audit trails
   - Use SBOM for supply chain transparency
   - Integrate with monitoring and alerting systems

## References

- [Trivy GitHub Repository](https://github.com/aquasecurity/trivy)
- [CVSS Scoring](https://www.first.org/cvss/)
- [NIST NVD](https://nvd.nist.gov/)
- [OWASP Container Security](https://owasp.org/www-community/Container_Security)
- [GCP Marketplace Security Requirements](https://cloud.google.com/marketplace/docs/partners/selling-through-gcp-marketplace)

## Related Documentation

- `/home/user/cre/docs/gcp/SBOM_GENERATION.md` - Software Bill of Materials
- `/home/user/cre/docs/gcp/GCP_MARKETPLACE_READINESS.md` - Marketplace compliance
- `/home/user/cre/docs/gcp/SBOM_INTEGRATION_GUIDE.md` - SBOM integration patterns
- `/home/user/cre/CLAUDE.md` - Docker-only workflow requirements
