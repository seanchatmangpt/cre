# CRE Platform-Agnostic CI/CD

Build → SBOM → Scan → Push pipeline that runs on **GCP, AWS, Azure**, or any environment. No GitHub Actions required.

## Quick Start

```bash
# Local run (build + SBOM, no push)
./scripts/cicd-pipeline.sh

# With push to GCP Artifact Registry
CICD_REGISTRY=us-central1-docker.pkg.dev/myproject/cre CICD_PUSH=true ./scripts/cicd-pipeline.sh

# With push to AWS ECR
CICD_REGISTRY=123456789.dkr.ecr.us-east-1.amazonaws.com/cre CICD_PUSH=true ./scripts/cicd-pipeline.sh

# With push to Azure ACR
CICD_REGISTRY=myregistry.azurecr.io/cre CICD_PUSH=true ./scripts/cicd-pipeline.sh
```

## Pipeline Flow

1. **Build** – Docker image (runtime stage)
2. **SBOM** – Syft generates SPDX + CycloneDX (runs after build)
3. **Scan** – Trivy vulnerability scan (optional)
4. **Push** – Push to registry (optional, when `CICD_PUSH=true`)

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CICD_REGISTRY` | Full registry URL | (none) |
| `CICD_IMAGE` | Image name | `cre` |
| `CICD_TAG` | Tag | `$VERSION-$GIT_SHA` |
| `CICD_PUSH` | Push after build | `false` |
| `CICD_SKIP_SCAN` | Skip Trivy | `false` |
| `CICD_OUTPUT_DIR` | Artifacts dir | `./cicd-artifacts` |

## Cloud-Specific Setup

### GCP (Cloud Build, GKE)

```bash
source cicd/cloud/gcp.env
./scripts/cicd-pipeline.sh
```

Or in Cloud Build: use `cicd/cloud/cloudbuild-cicd.yaml` (invokes the script).

### AWS (CodeBuild, EKS)

```bash
source cicd/cloud/aws.env
# Login: aws ecr get-login-password | docker login --username AWS --password-stdin $CICD_REGISTRY
./scripts/cicd-pipeline.sh
```

### Azure (Azure DevOps, AKS)

```bash
source cicd/cloud/azure.env
# Login: az acr login --name myregistry
./scripts/cicd-pipeline.sh
```

## Tekton (Kubernetes)

Runs on GKE, EKS, AKS, or any Kubernetes cluster:

```bash
kubectl apply -f cicd/tekton/
tkn pipeline start cre-pipeline -w name=source,volumeClaimTemplateFile=cicd/tekton/workspace.yaml
```

## Outputs

- `cicd-artifacts/sbom.spdx.json` – SPDX SBOM
- `cicd-artifacts/sbom.cyclonedx.json` – CycloneDX SBOM
- `cicd-artifacts/trivy.json` – Vulnerability scan (if Trivy installed)
