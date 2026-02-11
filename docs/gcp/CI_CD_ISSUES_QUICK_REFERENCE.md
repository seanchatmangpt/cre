# CI/CD Pipeline Issues - Quick Reference

## Critical Issues (Must Fix)

### 1. cloudbuild.yaml is Incomplete (46 bytes)
**File:** `/home/user/cre/cloudbuild.yaml`

**Current Content:**
```
=== Build Steps ===
=== Security Scanning ===
```

**What's Missing:**
- Docker buildx multi-arch build steps
- Unit test execution (rebar3 eunit)
- Security scanning configuration
- SBOM generation
- Artifact uploads

**Fix Time:** 1-2 hours

---

### 2. GKE Credentials Bug (Line 208-209)
**File:** `.github/workflows/gcp-cloud-build.yml`

**Current Code (WRONG):**
```yaml
gcloud container clusters get-credentials "${{ env.GKE_CLUSTER }}" \
  --region="${{ env.GKE_CLUSTER }}" \  # BUG: cluster name as region!
  --project="${PROJECT_ID}"
```

**Correct Code:**
```yaml
gcloud container clusters get-credentials "${{ env.GKE_CLUSTER }}" \
  --region="${{ env.REGION }}" \
  --project="${PROJECT_ID}"
```

**Why It Fails:** `--region=cre-prod-cluster` is not a valid region name

**Fix Time:** 5 minutes

---

### 3. No Unit Tests in Pipeline
**Missing:** `rebar3 eunit` execution in Cloud Build

**Why It Matters:** 
- No build verification before Docker image creation
- Compilation errors won't be caught
- Broken code can be deployed

**Add To cloudbuild.yaml:**
```yaml
- name: 'erlang:28-alpine'
  id: 'unit-tests'
  entrypoint: sh
  args:
    - -c
    - |
      apk add --no-cache curl git build-base openssl-dev
      curl -L -o /usr/local/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3
      chmod +x /usr/local/bin/rebar3
      cd /workspace && rebar3 compile && rebar3 eunit
  timeout: '1800s'
```

**Fix Time:** 30 minutes

---

### 4. Missing Kubernetes Resources
**Issue:** Deployment references resources that don't exist

**Missing:**
- Service (for network access)
- ServiceAccount (referenced as cre-ksa)

**Add To deploy-gke Job:**
```yaml
- name: Create Kubernetes resources
  run: |
    NAMESPACE="cre-${{ github.event.inputs.deploy_env || 'production' }}"
    
    # ServiceAccount
    kubectl create serviceaccount cre-ksa -n "${NAMESPACE}" --dry-run=client -o yaml | kubectl apply -f -
    
    # Service
    kubectl apply -f - <<'EOFK8S'
    apiVersion: v1
    kind: Service
    metadata:
      name: cre
      namespace: ${NAMESPACE}
    spec:
      selector:
        app: cre
      ports:
      - name: http
        port: 4142
        targetPort: 4142
      - name: epmd
        port: 4369
        targetPort: 4369
      type: LoadBalancer
    EOFK8S
```

**Fix Time:** 30 minutes

---

### 5. Build Digest Capture is Racy (Line 95-96)
**File:** `.github/workflows/gcp-cloud-build.yml`

**Current Code (UNRELIABLE):**
```bash
gcloud builds submit . \
  --config cloudbuild.yaml \
  --substitutions=_IMAGE_NAME="${IMAGE_NAME}",... \
  --region=${{ env.REGION }} \
  --format='value(imageSummary.digest)'

echo "digest=${IMAGE_NAME}@$(gcloud builds list --limit=1 --format='value(imageSummary.digest)' --region=${{ env.REGION }})" >> $GITHUB_OUTPUT
```

**Problems:**
1. Doesn't capture digest from the build that was just submitted
2. `gcloud builds list` might return a different build
3. No validation that build succeeded

**Better Approach:**
```bash
BUILD_ID=$(gcloud builds submit . \
  --config cloudbuild.yaml \
  --substitutions=... \
  --region=${{ env.REGION }} \
  --async \
  --format='value(id)')

# Wait for build to complete
gcloud builds log "${BUILD_ID}" --region=${{ env.REGION }} --stream

# Get digest from the build
BUILD_JSON=$(gcloud builds describe "${BUILD_ID}" --region=${{ env.REGION }} --format=json)
DIGEST=$(echo "${BUILD_JSON}" | jq -r '.images[0].digest')
```

**Fix Time:** 1 hour

---

## High Priority Issues (Should Fix)

### 6. Trivy Should Fail on Vulnerabilities
**File:** `.github/workflows/gcp-cloud-build.yml` (security-scan job)

**Current Code:**
```yaml
trivy image \
  --severity CRITICAL,HIGH \
  --format json \
  --output trivy-results.json \
  "${IMAGE_DIGEST}"
```

**Problem:** Pipeline continues even if CRITICAL vulnerabilities found

**Fix:**
```yaml
trivy image \
  --exit-code 1 \
  --severity CRITICAL,HIGH \
  --format json \
  --output trivy-results.json \
  "${IMAGE_DIGEST}"
```

**Impact:** Prevents deploying vulnerable images

---

### 7. Missing SBOM Format
**Issue:** Only generating SPDX, but many compliance frameworks require CycloneDX too

**Fix in generate-sbom job:**
```yaml
- name: Generate SBOM with Syft
  run: |
    syft "${IMAGE}" -o spdx-json --file sbom.spdx.json
    syft "${IMAGE}" -o cyclonedx-json --file sbom.cyclonedx.json
```

---

## Testing Recommendations

### Local Docker Build Test
```bash
# Requires docker buildx setup
docker buildx build \
  --file Dockerfile \
  --tag cre:0.3.0 \
  --platform linux/amd64,linux/arm64 \
  .
```

### Validate Workflow Syntax
```bash
# Using act (GitHub Actions emulator)
act -l -W .github/workflows/gcp-cloud-build.yml
```

---

## Secrets That Must Be Configured

In GitHub repo Settings → Secrets and variables → Actions:

| Secret | Purpose |
|--------|---------|
| `GCP_PROJECT_ID` | GCP project ID for Artifact Registry |
| `GCP_WORKLOAD_IDENTITY_PROVIDER` | WIF provider for OIDC token exchange |
| `GCP_CLOUD_BUILD_SA` | Service account for Cloud Build operations |
| `GCP_GKE_SA` | Service account for GKE deployment |

---

## Implementation Roadmap

1. **Phase 1 (Critical - 2 hours):**
   - [ ] Create complete cloudbuild.yaml
   - [ ] Fix GKE credentials bug
   - [ ] Add unit tests to pipeline

2. **Phase 2 (High Priority - 2 hours):**
   - [ ] Create missing Kubernetes resources
   - [ ] Fix build digest capture logic
   - [ ] Add Trivy failure threshold

3. **Phase 3 (Polish - 1 hour):**
   - [ ] Add SBOM format (CycloneDX)
   - [ ] Test locally with docker buildx
   - [ ] Verify all secrets configured

**Total Estimated Time:** 4-6 hours

---

## Files Affected

| File | Action | Issue Count |
|------|--------|------------|
| `cloudbuild.yaml` | CREATE | 5+ (completely missing) |
| `.github/workflows/gcp-cloud-build.yml` | MODIFY | 3 (credentials, digest, Trivy) |
| `.github/workflows/gcp-cloud-build.yml` (deploy-gke) | ADD STEP | 1 (missing resources) |

---

## Validation Status Summary

```
✅ GOOD:
  - GitHub workflow structure (4-stage pipeline)
  - Docker entrypoint script
  - docker-bake.hcl configuration
  - Dockerfile (multi-arch with OTP 28)
  - Error handling (if: always(), if: failure())
  - Health checks and rollback logic

❌ BROKEN:
  - cloudbuild.yaml (incomplete)
  - GKE credentials (wrong region variable)
  - No unit tests in pipeline
  - Missing Kubernetes Service
  - Racy digest capture

⚠️  SHOULD IMPROVE:
  - Trivy missing --exit-code flag
  - SBOM missing CycloneDX format
  - No post-build validation
```

For detailed analysis, see: `/home/user/cre/docs/gcp/CI_CD_PIPELINE_VALIDATION.md`
