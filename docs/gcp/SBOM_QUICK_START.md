# SBOM Generation - Quick Start Guide

## Prerequisites

1. **GCP Project** with Cloud Build API enabled
2. **Artifact Registry** repo created: `us-central1-docker.pkg.dev/{PROJECT}/cre/cre`
3. **GCS Bucket** for artifacts: `{PROJECT_ID}-cre-artifacts`
4. **Service Account** with permissions:
   - Cloud Build Editor
   - Artifact Registry Writer
   - Storage Object Creator

## Quick Start (5 minutes)

### Step 1: Verify Cloud Build Configuration

```bash
# Check if cloudbuild.yaml exists
ls -la cloudbuild.yaml

# Validate YAML syntax
gcloud builds submit --config cloudbuild.yaml --dry-run
```

### Step 2: Trigger Build with SBOM Generation

```bash
# Set variables
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
  --region=us-central1 \
  --async
```

### Step 3: Monitor Build Progress

```bash
# Watch build in real-time
gcloud builds log <BUILD_ID> --stream

# Or check status in console
gcloud builds list --limit=5
```

### Step 4: Download Generated SBOM Files

Once build completes:

```bash
# Download SPDX SBOM
gsutil cp "gs://${PROJECT_ID}-cre-artifacts/sbom/0.3.0/${COMMIT_SHA}/sbom.spdx.json" .

# Download all SBOM formats
gsutil -m cp -r "gs://${PROJECT_ID}-cre-artifacts/sbom/0.3.0/${COMMIT_SHA}/" ./sbom-artifacts/

# Verify downloads
ls -la sbom-artifacts/
```

### Step 5: Review Security Scan Results

```bash
# Download Trivy results
gsutil cp "gs://${PROJECT_ID}-cre-artifacts/security/0.3.0/${COMMIT_SHA}/trivy-results.json" .

# Pretty-print JSON
jq . trivy-results.json

# Count vulnerabilities by severity
jq '[.Results[].Vulnerabilities[]?.Severity] | group_by(.) |
  map({severity: .[0], count: length})' trivy-results.json
```

## Files Generated

After a successful build, you'll find these files:

### SBOM Artifacts (3 formats)

```
sbom.spdx.json              # SPDX v2.3 JSON format
sbom.cyclonedx.json         # CycloneDX v1.4 JSON format
sbom.cyclonedx.xml          # CycloneDX v1.4 XML format
sbom-manifest.json          # Metadata about SBOM generation
```

### Security Reports

```
trivy-results.json          # Vulnerability scan in JSON format
marketplace-compliance-report.json  # GCP Marketplace compliance check
```

## Using the SBOM

### For GCP Marketplace Submission

1. **Package artifacts**:
   ```bash
   tar -czf cre-sbom-0.3.0.tar.gz sbom-artifacts/
   ```

2. **Submit to GCP Partner Portal**:
   - Go to Partner Onboarding Portal
   - Upload SBOM artifacts
   - Include compliance report

### For Vulnerability Analysis

```bash
# Find critical vulnerabilities
jq '.Results[] | select(.Vulnerabilities[]?.Severity=="CRITICAL")' trivy-results.json

# Export as CSV
jq -r '.Results[] | .Vulnerabilities[] |
  [.VulnerabilityID, .Title, .Severity] | @csv' trivy-results.json > vulnerabilities.csv
```

### For Integration with Tools

```bash
# Use SBOM in Grype vulnerability scanner
grype sbom:sbom.spdx.json

# Use SBOM in dependency auditing
syft-cli analyze sbom.cyclonedx.json

# Use SBOM in license compliance checks
licensefinder --spdx sbom.spdx.json
```

## GitHub Actions Workflow

### Automatic SBOM Generation on Release

The `.github/workflows/gcp-cloud-build.yml` automatically generates SBOM when:
1. Push to `master` branch
2. Create version tag (`v0.3.0`)
3. Manual workflow dispatch

### View Artifacts

1. Go to GitHub Actions tab
2. Click on the completed workflow
3. Scroll to "Artifacts"
4. Download SBOM files

## Troubleshooting

### Build Fails: "Permission Denied"

```bash
# Grant Cloud Build service account permissions
PROJECT_NUMBER=$(gcloud projects describe $PROJECT_ID --format='value(projectNumber)')

gsutil iam ch \
  serviceAccount:${PROJECT_NUMBER}@cloudbuild.gserviceaccount.com:roles/storage.objectAdmin \
  gs://${PROJECT_ID}-cre-artifacts
```

### SBOM Not Generated

```bash
# Check Cloud Build logs
gcloud builds log <BUILD_ID> --stream

# Verify Artifact Registry access
gcloud artifacts docker images list \
  --repository=cre \
  --location=us-central1

# Test image pull
docker pull us-central1-docker.pkg.dev/${PROJECT_ID}/cre/cre:0.3.0
```

### GCS Bucket Doesn't Exist

```bash
# Create bucket
gsutil mb -p ${PROJECT_ID} -l us-central1 \
  gs://${PROJECT_ID}-cre-artifacts/

# Enable versioning (optional)
gsutil versioning set on gs://${PROJECT_ID}-cre-artifacts/
```

## Next Steps

1. **Review SBOM**: Check generated files for completeness
2. **Validate**: Run validation tools on SBOM files
3. **Scan**: Review Trivy security findings
4. **Remediate**: Address any vulnerabilities
5. **Submit**: Provide SBOM to GCP Marketplace

## File Locations

### In GCS

```
gs://{PROJECT_ID}-cre-artifacts/
├── sbom/{VERSION}/{COMMIT_SHA}/
│   ├── sbom.spdx.json
│   ├── sbom.cyclonedx.json
│   ├── sbom.cyclonedx.xml
│   └── sbom-manifest.json
├── security/{VERSION}/{COMMIT_SHA}/
│   └── trivy-results.json
└── compliance/{VERSION}/{COMMIT_SHA}/
    └── marketplace-compliance-report.json
```

### In Cloud Build Artifacts

Artifacts are also attached to Cloud Build step for 90-day retention

## Resources

- **SBOM Documentation**: `docs/gcp/SBOM_GENERATION.md`
- **GCP Marketplace**: https://cloud.google.com/marketplace
- **Syft Documentation**: https://github.com/anchore/syft
- **Trivy Documentation**: https://github.com/aquasecurity/trivy
- **SPDX Format**: https://spdx.org/
- **CycloneDX Format**: https://cyclonedx.org/

---

**Version**: 1.0
**Last Updated**: 2025-01-15
