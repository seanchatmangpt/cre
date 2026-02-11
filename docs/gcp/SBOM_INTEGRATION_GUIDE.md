# SBOM Integration Guide for GCP Marketplace

## Overview

This guide explains how SBOM (Software Bill of Materials) generation has been integrated into the CRE Cloud Build pipeline and how to use it for GCP Marketplace submission.

## Architecture Changes

### Before: Basic Cloud Build

The previous `cloudbuild.yaml` had minimal configuration with only placeholder content.

### After: Enhanced SBOM Pipeline

The new `cloudbuild.yaml` implements an 11-step pipeline:

```
┌──────────────────────────────┐
│ 1. Build Docker Image        │ (Multi-platform: amd64, arm64)
└──────────────┬───────────────┘
               ↓
    ┌──────────┴──────────┬──────────────┐
    ↓                     ↓              ↓
┌─────────┐    ┌──────────────┐   ┌──────────────┐
│ SBOM    │    │ SBOM         │   │ SBOM         │
│ SPDX    │    │ CycloneDX    │   │ CycloneDX    │
│ JSON    │    │ JSON         │   │ XML          │
└─────────┘    └──────────────┘   └──────────────┘
    │                   │              │
    └───────────┬───────┴──────────────┘
                ↓
    ┌──────────────────────────┐
    │ 5. SBOM Metadata         │
    │ 6. Validation            │
    └──────────────┬───────────┘
                   ↓
    ┌──────────────────────────┐
    │ 7. Security Scan (Trivy)  │
    └──────────────┬───────────┘
                   ↓
    ┌──────────────────────────┐
    │ 8. Upload to GCS         │
    │ 9. Tag with Metadata     │
    │ 10. Generate Report      │
    │ 11. Build Summary        │
    └──────────────────────────┘
```

## New Files Added

### 1. Enhanced Cloud Build Configuration
**File**: `/home/user/cre/cloudbuild.yaml`

**Key Features**:
- Multi-platform Docker builds (amd64, arm64)
- SBOM generation in 3 formats (SPDX, CycloneDX JSON, CycloneDX XML)
- Security scanning with Trivy
- Automated GCS upload with versioning
- Compliance reporting for GCP Marketplace
- Build summary with artifact locations

**Key Sections**:
- `build-image`: Multi-platform Docker build using buildx
- `generate-sbom-*`: SBOM generation in each format using Syft v1.18.1
- `validate-sbom`: JSON validation and file integrity checks
- `scan-image-trivy`: Vulnerability scanning using Trivy v0.48.1
- `upload-artifacts-gcs`: Upload all artifacts to GCS buckets
- `generate-marketplace-report`: Compliance checklist for Marketplace
- `build-summary`: Completion report with next steps

### 2. Documentation Files

**A. `/home/user/cre/docs/gcp/SBOM_GENERATION.md`**
- Comprehensive SBOM guide
- Detailed explanation of SBOM formats
- Pipeline architecture and step details
- Running SBOM generation (3 methods)
- Accessing generated files
- Validating SBOM quality
- GCP Marketplace checklist
- Troubleshooting guide
- Best practices

**B. `/home/user/cre/docs/gcp/SBOM_QUICK_START.md`**
- 5-minute quick start
- Prerequisites checklist
- Step-by-step build triggering
- File locations and downloads
- Using the SBOM
- Integration examples
- Quick troubleshooting

**C. `/home/user/cre/docs/gcp/SBOM_INTEGRATION_GUIDE.md`** (this file)
- Architecture overview
- Files and configuration changes
- Integration instructions
- CI/CD workflow updates
- GCP setup requirements
- Testing and validation

### 3. Helper Scripts

**File**: `/home/user/cre/scripts/generate-sbom.sh`

**Purpose**: Local SBOM generation without Cloud Build

**Features**:
- SBOM generation in multiple formats
- Optional Trivy security scanning
- Automatic tool installation (Syft, Trivy)
- SBOM validation
- Colored output and logging
- Error handling

**Usage**:
```bash
./scripts/generate-sbom.sh -i myimage:1.0.0
./scripts/generate-sbom.sh -i registry/image:latest -s -o ./artifacts
./scripts/generate-sbom.sh -i gcr.io/project/app:v1 -f spdx,cyclonedx-json
```

## Integration with Existing Systems

### GitHub Actions Workflow

The existing `.github/workflows/gcp-cloud-build.yml` has been enhanced:

**Previous Behavior**:
- Basic SBOM generation in GitHub Actions
- Limited format support
- Manual Syft installation

**New Behavior** (with updated cloudbuild.yaml):
- All SBOM generation delegated to Cloud Build
- Multiple format support (3 formats)
- Automatic tool installation via Cloud Build steps
- GCS artifact storage
- Compliance reporting
- Better integration with GCP services

**How They Work Together**:
1. GitHub Actions triggers Cloud Build
2. Cloud Build runs full pipeline including SBOM generation
3. All artifacts uploaded to GCS
4. GitHub Actions can retrieve artifacts from GCS if needed

### Artifact Registry Integration

**Image Metadata**:
- OCI labels include version and build info
- Images tagged with version and commit SHA
- Artifact Registry stores image metadata

**SBOM Association**:
- SBOM stored separately in GCS (not embedded in image)
- Manifest file provides mapping between image and SBOM
- Allows independent SBOM distribution

### Docker Build Configuration

The existing `docker-bake.hcl` already has SBOM target:

```hcl
target "sbom" {
  inherits = ["cre"]
  target = "sbom"
  output = ["type=local,dest=./sbom"]
}
```

**Usage**:
```bash
# Generate SBOM locally without pushing
docker buildx bake sbom

# Output: ./sbom/ directory contains SBOM artifacts
```

## GCP Environment Setup

### Prerequisites

1. **GCP Project**:
   ```bash
   export PROJECT_ID=my-project
   gcloud config set project $PROJECT_ID
   ```

2. **Enable APIs**:
   ```bash
   # Enable Cloud Build
   gcloud services enable cloudbuild.googleapis.com

   # Enable Artifact Registry
   gcloud services enable artifactregistry.googleapis.com

   # Enable Cloud Storage
   gcloud services enable storage-api.googleapis.com
   ```

3. **Create Artifact Registry Repository**:
   ```bash
   gcloud artifacts repositories create cre \
     --repository-format=docker \
     --location=us-central1
   ```

4. **Create GCS Bucket for Artifacts**:
   ```bash
   gsutil mb -p $PROJECT_ID -l us-central1 \
     gs://${PROJECT_ID}-cre-artifacts/

   # Optional: Enable versioning
   gsutil versioning set on gs://${PROJECT_ID}-cre-artifacts/
   ```

5. **Grant Cloud Build Service Account Permissions**:
   ```bash
   PROJECT_NUMBER=$(gcloud projects describe $PROJECT_ID \
     --format='value(projectNumber)')

   # Grant Artifact Registry Writer
   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member="serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com" \
     --role="roles/artifactregistry.writer"

   # Grant GCS Storage Object Creator
   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member="serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com" \
     --role="roles/storage.objectCreator"
   ```

### GitHub Actions Integration

1. **Store GCP Secrets in GitHub**:
   - `GCP_PROJECT_ID`: Your GCP project ID
   - `GCP_WORKLOAD_IDENTITY_PROVIDER`: WIF provider configuration
   - `GCP_CLOUD_BUILD_SA`: Cloud Build service account email
   - `GCP_GKE_SA`: GKE service account email (for deployments)

2. **GitHub Actions File**:
   - Already configured in `.github/workflows/gcp-cloud-build.yml`
   - Uses Workload Identity Federation (no service account keys)
   - Automatic Cloud Build triggering on push to master or tags

## Running SBOM Generation

### Method 1: GitHub Actions (Automatic)

**Triggered by**:
1. Push to master branch
2. Tag creation (v*)
3. Manual workflow dispatch

**Steps**:
1. Go to GitHub Actions tab
2. Click "GCP Cloud Build CI/CD"
3. Click "Run workflow"
4. Select environment and options
5. Monitor in Cloud Build console

**Artifacts**:
- Uploaded to GCS automatically
- Available in GitHub Actions artifacts section
- Linked in build summary

### Method 2: gcloud CLI (Manual)

```bash
# Set up variables
PROJECT_ID=$(gcloud config get-value project)
COMMIT_SHA=$(git rev-parse --short HEAD)
BUILD_DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ)

# Submit build
gcloud builds submit . \
  --config cloudbuild.yaml \
  --substitutions=\
_IMAGE_NAME="us-central1-docker.pkg.dev/${PROJECT_ID}/cre/cre",\
_VERSION="0.3.0",\
_COMMIT_SHA="${COMMIT_SHA}",\
_BUILD_DATE="${BUILD_DATE}" \
  --region=us-central1

# Monitor progress
gcloud builds log <BUILD_ID> --stream
```

### Method 3: Local Script

```bash
# Generate SBOM locally
./scripts/generate-sbom.sh \
  -i us-central1-docker.pkg.dev/${PROJECT_ID}/cre/cre:0.3.0 \
  -o ./sbom-output \
  -s

# Output in ./sbom-output/
```

### Method 4: Docker Bake

```bash
# Generate SBOM via docker buildx
docker buildx bake sbom

# Output in ./sbom/
```

## Artifact Organization

### GCS Structure

```
gs://{PROJECT_ID}-cre-artifacts/
├── sbom/                      # SBOM artifacts
│   └── {VERSION}/
│       └── {COMMIT_SHA}/
│           ├── sbom.spdx.json
│           ├── sbom.cyclonedx.json
│           ├── sbom.cyclonedx.xml
│           └── sbom-manifest.json
├── security/                  # Security scan results
│   └── {VERSION}/
│       └── {COMMIT_SHA}/
│           └── trivy-results.json
├── compliance/                # Compliance reports
│   └── {VERSION}/
│       └── {COMMIT_SHA}/
│           └── marketplace-compliance-report.json
└── build-artifacts/           # All artifacts from build
    └── {VERSION}/
        └── {COMMIT_SHA}/
            └── (all files)
```

## Validation and Testing

### 1. Verify Cloud Build Configuration

```bash
# Validate YAML syntax
gcloud builds submit --config cloudbuild.yaml --dry-run

# Check substitutions
grep "^substitutions:" cloudbuild.yaml -A 10
```

### 2. Test SBOM Generation

```bash
# Run local generation
./scripts/generate-sbom.sh -i alpine:latest -o ./test-sbom

# Verify files
ls -la ./test-sbom/
file ./test-sbom/sbom.*.{json,xml}

# Validate JSON
jq . ./test-sbom/sbom.spdx.json > /dev/null && echo "Valid"
```

### 3. Check Artifact Upload

```bash
# List GCS artifacts
gsutil ls -r "gs://${PROJECT_ID}-cre-artifacts/sbom/"

# Verify file integrity
gsutil stat "gs://${PROJECT_ID}-cre-artifacts/sbom/0.3.0/abc123/sbom.spdx.json"
```

### 4. Test Security Scanning

```bash
# Run Trivy locally
trivy image us-central1-docker.pkg.dev/${PROJECT_ID}/cre/cre:0.3.0 \
  --severity CRITICAL,HIGH \
  --format json \
  -o trivy-test.json

# Review results
jq . trivy-test.json
```

## Troubleshooting Integration Issues

### Cloud Build Step Fails

**Symptom**: Build fails at SBOM generation step

**Diagnostics**:
```bash
# Check Cloud Build logs
gcloud builds log <BUILD_ID> --stream

# Verify image exists
gcloud artifacts docker images list \
  --repository=cre \
  --location=us-central1
```

**Solutions**:
1. Verify Artifact Registry repo exists
2. Check Cloud Build service account permissions
3. Verify image was pushed successfully in previous step

### GCS Upload Permission Denied

**Symptom**: "Permission denied" error on GCS upload steps

**Diagnostics**:
```bash
# Check bucket exists
gsutil ls gs://${PROJECT_ID}-cre-artifacts/

# Check service account permissions
gsutil iam get gs://${PROJECT_ID}-cre-artifacts/
```

**Solutions**:
```bash
# Grant necessary permissions
PROJECT_NUMBER=$(gcloud projects describe $PROJECT_ID \
  --format='value(projectNumber)')

gsutil iam ch \
  serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com:roles/storage.objectAdmin \
  gs://${PROJECT_ID}-cre-artifacts
```

### Trivy or Syft Download Fails

**Symptom**: "Failed to download Trivy/Syft from GitHub"

**Causes**:
- GitHub API rate limiting
- Network connectivity issues
- Unsupported architecture

**Solutions**:
1. Verify network access to github.com
2. Cache tools in Container Registry
3. Pre-download tools for unsupported architectures

## Performance Optimization

### Build Time Optimization

1. **Parallel SBOM Generation**: Steps 2-4 could run in parallel (currently sequential)
2. **Trivy Cache**: Cache Trivy DB in GCS to speed up scans
3. **Layer Caching**: Enable Docker layer caching in Cloud Build

### Example Parallel Configuration

```yaml
# Currently: sequential steps
- id: generate-sbom-spdx
  ...
- id: generate-sbom-cyclonedx
  depends_on: ["generate-sbom-spdx"]
  ...

# Could be: parallel execution (remove dependencies)
- id: generate-sbom-spdx
  ...
- id: generate-sbom-cyclonedx
  ...  # No waitFor dependency
```

### Storage Optimization

```bash
# Compress artifacts before upload
tar -czf sbom-archive.tar.gz sbom-output/
gsutil cp sbom-archive.tar.gz gs://${PROJECT_ID}-cre-artifacts/

# Or: Set lifecycle policy to archive old versions
gsutil lifecycle set - <<EOF
{
  "lifecycle": {
    "rule": [
      {
        "action": {"type": "SetStorageClass", "storageClass": "NEARLINE"},
        "condition": {"age": 30}
      }
    ]
  }
}
EOF
```

## Next Steps for GCP Marketplace

1. **Collect Artifacts**:
   - Download SBOM files from GCS
   - Download compliance report
   - Download Trivy scan results

2. **Prepare Submission**:
   - Create marketplace listing
   - Include SBOM in documentation
   - Document security findings

3. **Submit to GCP**:
   - Use Partner Portal to submit
   - Attach SBOM artifacts
   - Include compliance report

4. **Obtain Approval**:
   - Wait for GCP review (typically 2-4 weeks)
   - Address feedback from security team
   - Re-submit if required

## References

- **Cloud Build Documentation**: https://cloud.google.com/build/docs
- **Artifact Registry**: https://cloud.google.com/artifact-registry/docs
- **Syft**: https://github.com/anchore/syft
- **Trivy**: https://github.com/aquasecurity/trivy
- **SPDX**: https://spdx.org/
- **CycloneDX**: https://cyclonedx.org/
- **GCP Marketplace**: https://cloud.google.com/marketplace/docs

---

**Document Version**: 1.0
**Last Updated**: 2025-01-15
**Status**: Production Ready
