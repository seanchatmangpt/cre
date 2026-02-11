# Binary Authorization Module

## Overview

This Terraform module implements Google Cloud Binary Authorization for CRE deployments, enforcing that only signed container images can be deployed to GKE clusters.

## Features

- **Whitelist approach**: Deny all images by default, allow only signed images
- **Cosign verification**: Enforces cosign signature verification
- **Google images allowlist**: Allows Google-built base images (distroless, etc.)
- **Flexible enforcement**: Support for blocking, audit-only, or disabled modes

## Security Benefits

- **Supply chain security**: Prevents deployment of unsigned or tampered images
- **Compliance**: Meets SOC 2, HIPAA, and PCI-DSS requirements for change management
- **Zero trust**: No implicit trust for any image without verified signature

## Usage

```hcl
module "binary_authorization" {
  source = "./modules/binary_authorization"

  project_id    = "my-cre-project"
  cluster_name  = "projects/my-project/locations/us-central1/clusters/cre-cluster"

  # Cosign public key for signature verification
  cosign_public_key_id = "https://raw.githubusercontent.com/my-org/cre/main/cosign.pub"

  # Enforcement mode (ENFORCED_AND_BLOCKING for production)
  enforcement_mode = "ENFORCED_AND_BLOCKING"
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| `project_id` | GCP project ID | `string` | - | Yes |
| `cluster_name` | GKE cluster resource path | `string` | - | Yes |
| `enforcement_mode` | Policy enforcement mode | `string` | `"ENFORCED_AND_BLOCKING"` | No |
| `cosign_public_key_id` | Cosign public key URL | `string` | `null` | No |
| `allowlist_patterns` | Image patterns to allow without signature | `list(string)` | `[]` | No |

## Enforcement Modes

### ENFORCED_AND_BLOCKING
Deployments of unsigned images are **blocked** at admission time. Use for production.

### ENFORCED_AND_AUDIT_ONLY
Deployments are logged but **not blocked**. Use for testing or monitoring.

### DISABLED
Policy is not enforced. Use for development clusters only.

## Generating a Cosign Key Pair

### Option 1: Generate and Store Locally

```bash
# Generate key pair
cosign generate-key-pair

# Public key (save to GitHub repo or GCS)
cat cosign.pub

# Private key (save to GitHub Secrets or Secret Manager)
cat cosign.key | gh secret set COSIGN_PRIVATE_KEY
```

### Option 2: Use KMS-based Key (Recommended for Production)

```bash
# Create KMS key
gcloud kms keys create cosign-signing-key \
  --location global \
  --keyring cre-keys \
  --purpose asymmetric-signing \
  --default-algorithm ec-sign-p256-sha256

# Use KMS key for signing (no private key storage)
export COSIGN_PRIVATE_KEY="gcpkms://projects/$PROJECT_ID/locations/global/keyRings/cre-keys/cryptoKeys/cosign-signing-key"

# Sign image
cosign sign --key $COSIGN_PRIVATE_KEY IMAGE_TAG

# Get public key for verification
gcloud kms keys get-public-key cosign-signing-key \
  --location global \
  --keyring cre-keys > cosign.pub
```

## Storing the Cosign Public Key

### GitHub Repository (Recommended)

```bash
# Add cosign.pub to your repository
cp cosign.pub /path/to/cre/.github/cosign.pub
git add .github/cosign.pub
git commit -m "Add cosign public key"

# Reference in Terraform
cosign_public_key_id = "https://raw.githubusercontent.com/my-org/cre/main/.github/cosign.pub"
```

### Google Cloud Storage

```bash
# Upload to GCS
gsutil cp cosign.pub gs://my-bucket/cosign.pub

# Make public read-only
gsutil acl ch -u AllUsers:R gs://my-bucket/cosign.pub

# Reference in Terraform
cosign_public_key_id = "https://storage.googleapis.com/my-bucket/cosign.pub"
```

## Testing the Policy

### Test 1: Verify Signed Image Deployment

```bash
# Deploy signed image (should succeed)
kubectl run test-signed \
  --image=us-central1-docker.pkg.dev/my-project/cre/cre:v1.0.0 \
  --dry-run=server -n cre-prod
```

### Test 2: Verify Unsigned Image Rejection

```bash
# Deploy unsigned image (should fail with "Image denied by policy")
kubectl run test-unsigned \
  --image=us-central1-docker.pkg.dev/my-project/cre/cre:unsigned \
  --dry-run=server -n cre-prod

# Expected error:
# Error from server (Forbidden): pods "test-unsigned" is forbidden:
# image policy webhook blocked image
```

### Test 3: Check Policy Status

```bash
# View Binary Authorization policy
gcloud container binauthz policy export

# Check cluster enforcement status
gcloud container clusters describe cre-cluster \
  --region us-central1 \
  --format "yaml(binaryAuthorization)"
```

## Troubleshooting

### "Image denied by policy" Error

**Cause**: Image is not signed or signature verification failed.

**Solution**:
```bash
# Verify image signature locally
cosign verify IMAGE_TAG --key cosign.pub

# Check if image is signed
cosign triangulate IMAGE_TAG

# View policy details
gcloud container binauthz policy export
```

### "Public key not found" Error

**Cause**: Cosign public key URL is inaccessible.

**Solution**:
```bash
# Test public key URL
curl -I https://raw.githubusercontent.com/my-org/cre/main/.github/cosign.pub

# Or use GCS
gsutil acl get gs://my-bucket/cosign.pub
```

### Policy Not Enforced on Cluster

**Cause**: Binary Authorization not enabled on GKE cluster.

**Solution**:
```bash
# Check cluster configuration
gcloud container clusters describe cre-cluster --region us-central1 | grep binaryAuthorization

# Binary Authorization must be enabled during cluster creation
# (see terraform/gcp/modules/gke_cluster/main.tf:107-109)
```

## CI/CD Integration

The GitHub Actions workflow (`.github/workflows/release.yml`) automatically:
1. Builds and pushes the image
2. Signs with cosign using OIDC (no private key storage)
3. Verifies the signature
4. Attaches SBOM
5. Runs vulnerability scan

When this workflow completes successfully, the image is ready for deployment to Binary Authorization-enabled clusters.

## References

- [Binary Authorization Documentation](https://cloud.google.com/binary-authorization)
- [Cosign Documentation](https://sigstore.dev/cosign/)
- [CRE Image Signing Workflow](../../.github/workflows/release.yml)
- [CRE Security Whitepaper](../../docs/gcp/SECURITY_WHITEPAPER.md)
