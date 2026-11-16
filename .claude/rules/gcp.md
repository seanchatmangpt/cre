---
paths:
  - "k8s/**"
  - "terraform/**"
  - "docs/gcp/**"
  - "cloudbuild.yaml"
  - "docker-bake.hcl"
---

# GCP Marketplace Deployment

**Target:** GCP Marketplace submission with first-time approval

## Deployment Artifacts

| Artifact | Location |
|----------|----------|
| Multi-arch Docker images | Dockerfile, docker-bake.hcl |
| GKE manifests | k8s/gcp/*.yaml |
| Terraform modules | terraform/gcp/ |
| Security scanning | cloudbuild.yaml |

## Required Components
- Multi-arch images (linux/amd64, linux/arm64)
- GKE Regional Cluster manifests (15+ YAML files)
- Terraform infrastructure modules
- SBOM generation (SPDX, CycloneDX)
- Security scanning (Trivy)

## OTP 28 Migration

**Status:** IN PROGRESS - Current Dockerfile uses OTP 27

**Breaking Changes:**
- User permission issues in erlang:28-alpine during build
- Doc attribute compatibility (use @doc comments, not -doc attributes)
- NIF loading changes for Rust components
- Logger API changes (check for deprecated functions)

## Reference

See @docs/gcp/GCP_MARKETPLACE_READINESS.md for complete deployment checklist.
