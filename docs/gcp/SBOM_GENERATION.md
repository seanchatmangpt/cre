# CRE SBOM Generation for GCP Marketplace Submission

## Overview

This document describes the Software Bill of Materials (SBOM) generation process integrated into the CRE Cloud Build pipeline. The SBOM is a critical requirement for GCP Marketplace submission, providing transparency about all dependencies and components in the CRE Docker image.

**Key Features:**
- Multi-format SBOM generation (SPDX JSON, CycloneDX JSON, CycloneDX XML)
- Automated security scanning with Trivy
- GCS artifact storage with versioning
- GCP Marketplace compliance reporting
- OCI image label compliance

---

## What is an SBOM?

A Software Bill of Materials (SBOM) is a complete list of all software components, libraries, and dependencies that make up a software product. It includes:

- **Direct Dependencies**: Libraries explicitly declared in the project
- **Transitive Dependencies**: Dependencies of dependencies
- **System Libraries**: OS-level packages and libraries
- **Container Metadata**: Base image information, layers, and environment

**Why SBOM matters for GCP Marketplace:**
1. **Security**: Enables vulnerability tracking and management
2. **Compliance**: Satisfies software supply chain requirements
3. **Transparency**: Customers know what's in the image
4. **Provenance**: Audit trail of component sources

---

## SBOM Formats Supported

### 1. SPDX (Software Package Data Exchange)

**Format**: JSON (SPDX v2.3)
**File**: `sbom.spdx.json`
**Use Case**: Industry standard for regulatory compliance

**Advantages:**
- ISO/IEC 5962:2021 standard
- Widely recognized by security tools
- Comprehensive package metadata
- License information included

**Example SPDX Entry:**
```json
{
  "spdxVersion": "SPDX-2.3",
  "dataLicense": "CC0-1.0",
  "SPDXID": "SPDXRef-DOCUMENT",
  "name": "CRE-0.3.0",
  "documentNamespace": "https://github.com/joergen7/cre/sbom/...",
  "packages": [
    {
      "SPDXID": "SPDXRef-Package-erlang",
      "name": "erlang",
      "version": "28",
      "downloadLocation": "docker.io/library/erlang:28-alpine",
      "filesAnalyzed": false
    }
  ]
}
```

### 2. CycloneDX (JSON)

**Format**: JSON (CycloneDX v1.4)
**File**: `sbom.cyclonedx.json`
**Use Case**: Modern DevSecOps and SaaS applications

**Advantages:**
- Optimized for DevSecOps workflows
- Excellent tooling support
- Lightweight format
- Dependency graph support

**Example CycloneDX Entry:**
```json
{
  "bomFormat": "CycloneDX",
  "specVersion": "1.4",
  "serialNumber": "urn:uuid:...",
  "version": 1,
  "metadata": {
    "timestamp": "2025-01-15T10:30:00Z",
    "component": {
      "type": "application",
      "name": "cre",
      "version": "0.3.0"
    }
  },
  "components": [
    {
      "type": "library",
      "name": "erlang",
      "version": "28",
      "purl": "pkg:docker/library/erlang@28"
    }
  ]
}
```

### 3. CycloneDX (XML)

**Format**: XML (CycloneDX v1.4)
**File**: `sbom.cyclonedx.xml`
**Use Case**: Enterprise systems and legacy integrations

**Advantages:**
- XML parsing in all enterprise environments
- Digital signature support
- Maximum compatibility
- Human-readable format

---

## Cloud Build Pipeline Integration

### Pipeline Architecture

The enhanced `cloudbuild.yaml` implements an 11-step pipeline:

```
┌─────────────────────────────────────────────────────────┐
│  Step 1: Build Docker Image (Multi-Platform)            │
│  - Build for linux/amd64 and linux/arm64                │
│  - Push to Artifact Registry                            │
└─────────────┬───────────────────────────────────────────┘
              │
    ┌─────────┴─────────┬────────────────────┐
    │                   │                    │
┌───▼───────┐   ┌──────▼──────┐   ┌────────▼──────┐
│ Step 2:   │   │ Step 3:     │   │ Step 4:       │
│ SBOM SPDX │   │ SBOM CDXJSON│   │ SBOM CDXXML   │
└───┬───────┘   └──────┬──────┘   └────────┬──────┘
    │                   │                    │
    └─────────┬─────────┴────────────────────┘
              │
    ┌─────────▼──────────────────────────────┐
    │ Step 5: Generate SBOM Metadata         │
    │ Step 6: Validate SBOM Files            │
    └─────────┬──────────────────────────────┘
              │
    ┌─────────▼──────────────────────────────┐
    │ Step 7: Security Scanning (Trivy)      │
    └─────────┬──────────────────────────────┘
              │
    ┌─────────▼──────────────────────────────┐
    │ Step 8-11: Upload & Report             │
    │ - Upload artifacts to GCS              │
    │ - Generate compliance report           │
    │ - Build summary & notifications        │
    └────────────────────────────────────────┘
```

### Step-by-Step Details

#### Step 1: Build Docker Image
**Purpose**: Build multi-platform Docker images
**Tools**: docker buildx
**Output**: Multi-arch images pushed to Artifact Registry

**Command**:
```bash
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  --build-arg VERSION=${_VERSION} \
  --build-arg GIT_REVISION=${_COMMIT_SHA} \
  --build-arg BUILD_DATE=${_BUILD_DATE} \
  --tag ${_IMAGE_NAME}:${_VERSION} \
  --tag ${_IMAGE_NAME}:latest \
  --output type=registry \
  -f ./Dockerfile .
```

#### Steps 2-4: Generate SBOM in Multiple Formats
**Purpose**: Create SBOM with Syft in 3 formats
**Tool**: Syft v1.18.1
**Output**: `sbom.spdx.json`, `sbom.cyclonedx.json`, `sbom.cyclonedx.xml`

**SPDX Command**:
```bash
syft "${IMAGE_NAME}:${VERSION}" \
  --output spdx-json \
  --file sbom.spdx.json
```

**CycloneDX JSON Command**:
```bash
syft "${IMAGE_NAME}:${VERSION}" \
  --output cyclonedx-json \
  --file sbom.cyclonedx.json
```

**CycloneDX XML Command**:
```bash
syft "${IMAGE_NAME}:${VERSION}" \
  --output cyclonedx \
  --file sbom.cyclonedx.xml
```

#### Step 5: Generate SBOM Metadata
**Purpose**: Create manifest of SBOM artifacts and metadata
**Output**: `sbom-manifest.json`

**Content**:
```json
{
  "metadata": {
    "timestamp": "2025-01-15T10:30:00Z",
    "build_date": "2025-01-15T09:00:00Z",
    "version": "0.3.0",
    "commit_sha": "abc123def456",
    "image_name": "us-central1-docker.pkg.dev/project/cre/cre:0.3.0",
    "sbom_generator": "syft",
    "sbom_spec_version": "1.4"
  },
  "artifacts": {
    "spdx_json": { ... },
    "cyclonedx_json": { ... },
    "cyclonedx_xml": { ... }
  },
  "gcp_marketplace": {
    "submission_ready": true,
    "sbom_provided": true,
    "security_scanning_required": true,
    "scan_with_trivy": true
  }
}
```

#### Step 6: Validate SBOM Files
**Purpose**: Ensure all SBOM files are present and valid
**Checks**:
- File existence
- File size (KB)
- JSON syntax validation with jq

**Example Output**:
```
✓ sbom.spdx.json (234 KB)
  ✓ Valid JSON syntax
✓ sbom.cyclonedx.json (189 KB)
  ✓ Valid JSON syntax
✓ sbom.cyclonedx.xml (456 KB)
✓ sbom-manifest.json (5 KB)
  ✓ Valid JSON syntax

All SBOM files validated successfully
```

#### Step 7: Security Scanning with Trivy
**Purpose**: Scan image for vulnerabilities
**Tool**: Trivy v0.48.1
**Output**: `trivy-results.json`

**Scan Configuration**:
- Severity levels: CRITICAL, HIGH
- Format: JSON for machine processing
- Also generates human-readable table output

#### Steps 8-11: Upload and Reporting
**Purpose**: Store artifacts and generate compliance report
**Outputs**:
- GCS locations with versioning
- Compliance report
- Build summary

---

## Running SBOM Generation

### Option 1: Via GitHub Actions

The `.github/workflows/gcp-cloud-build.yml` automatically triggers builds on:
- Push to `master` branch
- Push of version tags (`v*`)
- Manual workflow dispatch

**To trigger manually**:
1. Go to GitHub repository
2. Click "Actions" tab
3. Select "GCP Cloud Build CI/CD"
4. Click "Run workflow"
5. Select environment (staging/production)
6. Click "Run workflow"

### Option 2: Via gcloud CLI

```bash
# Set up authentication
gcloud auth application-default login
gcloud config set project YOUR_PROJECT_ID

# Submit build with substitutions
gcloud builds submit . \
  --config cloudbuild.yaml \
  --substitutions=\
_IMAGE_NAME="us-central1-docker.pkg.dev/YOUR_PROJECT/cre/cre",\
_VERSION="0.3.0",\
_COMMIT_SHA="$(git rev-parse --short HEAD)",\
_BUILD_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  --region=us-central1
```

### Option 3: Manual Docker Build (Development)

```bash
# Use docker-bake.hcl for SBOM generation
docker buildx bake sbom

# SBOM files will be in ./sbom directory
ls -la ./sbom/
```

---

## Accessing Generated SBOM Files

### From GCS (Production)

All artifacts are automatically uploaded to GCS with this structure:

```
gs://${PROJECT_ID}-cre-artifacts/
├── sbom/
│   └── ${VERSION}/
│       └── ${COMMIT_SHA}/
│           ├── sbom.spdx.json
│           ├── sbom.cyclonedx.json
│           ├── sbom.cyclonedx.xml
│           └── sbom-manifest.json
├── security/
│   └── ${VERSION}/
│       └── ${COMMIT_SHA}/
│           └── trivy-results.json
└── compliance/
    └── ${VERSION}/
        └── ${COMMIT_SHA}/
            └── marketplace-compliance-report.json
```

**Download SBOM files**:
```bash
# Download SPDX SBOM
gsutil cp "gs://${PROJECT_ID}-cre-artifacts/sbom/0.3.0/abc123/sbom.spdx.json" .

# Download all SBOM formats
gsutil -m cp -r "gs://${PROJECT_ID}-cre-artifacts/sbom/0.3.0/abc123/*" ./sbom-artifacts/

# Download security scan results
gsutil cp "gs://${PROJECT_ID}-cre-artifacts/security/0.3.0/abc123/trivy-results.json" .
```

### From Cloud Build Console

1. Go to Cloud Build > History
2. Find your build
3. Click "Artifacts" tab
4. Download files directly

---

## Validating SBOM Quality

### Using Syft to Validate

```bash
# Count packages in SPDX SBOM
jq '.packages | length' sbom.spdx.json

# List all package names
jq '.packages[].name' sbom.spdx.json

# Check for vulnerabilities mentioned
jq '.packages[] | select(.externalRefs[]?.referenceType=="security-advisory")' sbom.spdx.json
```

### Using CycloneDX Validation

```bash
# Validate CycloneDX JSON against schema
npm install -g @cyclonedx/npm
cyclonedx validate --input-file sbom.cyclonedx.json --input-version 1.4

# or with curl
curl -X POST https://cyclonedx.org/validate/ \
  -F "bom=@sbom.cyclonedx.json"
```

### Trivy Vulnerability Analysis

```bash
# Convert Trivy JSON to human-readable report
jq '.Results[] | select(.Vulnerabilities | length > 0)' trivy-results.json

# Count vulnerabilities by severity
jq '[.Results[].Vulnerabilities[]?.Severity] | group_by(.) |
  map({severity: .[0], count: length})' trivy-results.json
```

---

## GCP Marketplace Submission Checklist

Before submitting to GCP Marketplace, ensure:

- [x] **SBOM Generated**: All three formats available
  - [x] SPDX JSON (v2.3)
  - [x] CycloneDX JSON (v1.4)
  - [x] CycloneDX XML (v1.4)

- [x] **Security Scanning Complete**: Trivy results available
  - [x] CRITICAL vulnerabilities identified
  - [x] HIGH vulnerabilities identified
  - [x] Remediation plan for findings

- [x] **Image Metadata**: OCI labels present
  - [x] `org.opencontainers.image.title`
  - [x] `org.opencontainers.image.description`
  - [x] `org.opencontainers.image.version`
  - [x] `org.opencontainers.image.source`
  - [x] `org.opencontainers.image.licenses`
  - [x] All other standard labels

- [x] **Multi-Platform Support**: Both architectures built
  - [x] linux/amd64
  - [x] linux/arm64

- [x] **Artifact Storage**: GCS backup ready
  - [x] SBOM artifacts in GCS
  - [x] Security scan results in GCS
  - [x] Compliance report in GCS

---

## Troubleshooting

### Issue: SBOM Generation Fails

**Symptom**: Cloud Build step fails with "Syft not found"

**Solution**:
```bash
# Verify Syft installation
syft version

# If not installed, manual installation:
curl -sSL https://raw.githubusercontent.com/anchore/syft/main/install.sh | sh
```

### Issue: Empty SBOM File

**Symptom**: Generated SBOM is 0 bytes or contains no packages

**Solution**:
1. Verify image exists in Artifact Registry: `gcloud artifacts docker images list`
2. Check image accessibility: `docker pull ${IMAGE_NAME}:${VERSION}`
3. Run Syft with verbose output: `syft -v ${IMAGE_NAME}:${VERSION}`

### Issue: Trivy Scan Timeout

**Symptom**: Security scanning step takes >10 minutes

**Solution**:
1. Reduce timeout threshold: Change `--timeout` in Cloud Build step
2. Cache Trivy DB: Add persistent GCS bucket for Trivy cache
3. Run scan asynchronously in separate step

### Issue: GCS Upload Fails

**Symptom**: "Permission denied" uploading to GCS

**Solution**:
1. Verify Cloud Build service account has GCS write permissions
2. Check bucket exists: `gsutil ls gs://${PROJECT_ID}-cre-artifacts`
3. Grant permissions:
   ```bash
   gsutil iam ch \
     serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com:roles/storage.objectAdmin \
     gs://${PROJECT_ID}-cre-artifacts
   ```

---

## Best Practices

### 1. Regular SBOM Updates
- Regenerate SBOM on every release
- Track changes in dependencies over time
- Compare SBOMs between versions for impact analysis

### 2. Vulnerability Response
- Review Trivy results for every build
- Address CRITICAL findings before release
- Document remediation steps in release notes

### 3. SBOM Distribution
- Include SBOM in product documentation
- Provide SBOM to customers on request
- Use SBOM for supply chain security attestation

### 4. Version Management
- Use semantic versioning for images
- Keep GCS artifacts organized by version
- Archive old builds for audit trail

### 5. Compliance Documentation
- Keep marketplace compliance report updated
- Document any exceptions to requirements
- Maintain audit trail of approvals

---

## Integration with Other Tools

### Grype (Vulnerability Detection)
```bash
# Find vulnerabilities in SBOM
grype sbom:sbom.spdx.json --output json > grype-results.json
```

### OWASP Dependency-Check
```bash
# Alternative vulnerability scanning
dependency-check.sh --project CRE --scan sbom.spdx.json
```

### Artifact Registry Integration
```bash
# View SBOM in Artifact Registry UI
# Images > cre > 0.3.0 > SBOM tab
```

---

## References

- **SPDX**: https://spdx.org/
- **CycloneDX**: https://cyclonedx.org/
- **Syft**: https://github.com/anchore/syft
- **Trivy**: https://github.com/aquasecurity/trivy
- **GCP Marketplace**: https://cloud.google.com/marketplace
- **OCI Image Spec**: https://github.com/opencontainers/image-spec

---

## Support & Feedback

For issues or questions about SBOM generation:
1. Check GCP Marketplace documentation
2. Review Syft/Trivy GitHub issues
3. Consult CRE project documentation
4. File issues: https://github.com/joergen7/cre/issues

---

**Document Version**: 1.0
**Last Updated**: 2025-01-15
**Status**: Production Ready
