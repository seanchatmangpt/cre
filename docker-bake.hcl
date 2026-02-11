# CRE Multi-Platform Docker Build Configuration
# Usage:
#   docker buildx bake --load cre              # Build for current platform
#   docker buildx bake cre                     # Build all platforms (no load)
#   docker buildx bake cre --push              # Build and push to registry
#   docker buildx bake --set *.args.GCP_PROJECT=myproject cre
#
# GCP Marketplace builds:
#   docker buildx bake --set "*.output=type=registry" \
#     --set "*.tags=us-central1-docker.pkg.dev/myproject/cre/cre:0.3.0" cre

variable "REGISTRY" {
  default = "ghcr.io"
}

variable "GCP_REGISTRY" {
  default = "us-docker.pkg.dev"
}

variable "GCP_PROJECT" {
  default = "your-project-id"
}

variable "GCP_REGION" {
  default = "us-central1"
}

variable "REPO_NAME" {
  default = "cre"
}

variable "VERSION" {
  default = "0.3.0"
}

variable "GIT_REVISION" {
  default = "dev"
}

variable "BUILD_DATE" {
  default = "unknown"
}

function "tags" {
  params = [registry, project, repo, version]
  result = [
    "${registry}/${project}/${repo}:${version}",
    "${registry}/${project}/${repo}:latest",
  ]
}

function "gcp_tags" {
  params = [registry, region, project, repo, version]
  result = [
    "${registry}/${region}-${project}/${repo}/cre:${version}",
    "${registry}/${region}-${project}/${repo}/cre:latest",
  ]
}

group "default" {
  targets = ["cre"]
}

# Main CRE target - multi-architecture
target "cre" {
  target = "runtime"

  args = {
    BUILD_DATE     = "${BUILD_DATE}"
    VERSION        = "${VERSION}"
    GIT_REVISION   = "${GIT_REVISION}"
  }

  platforms = [
    "linux/amd64",
    "linux/arm64"
  ]

  tags = tags(REGISTRY, "joergen7", REPO_NAME, VERSION)

  contexts = {
    "." = "."
  }

  cache-from = [
    "type=gha",
    "type=local,src=/tmp/.buildx-cache"
  ]

  cache-to = [
    "type=gha,mode=max",
    "type=local,dest=/tmp/.buildx-cache,mode=max"
  ]

  output = [
    "type=image,push=false"
  ]
}

# Local development target (single platform, loads to Docker)
target "local" {
  inherits = ["cre"]
  output = ["type=docker"]
  platforms = ["local"]
}

# Release target with push enabled (for GitHub Container Registry)
target "release" {
  inherits = ["cre"]
  output = ["type=registry"]
}

# GCP Artifact Registry target (for GCP Marketplace)
target "gcp" {
  args = {
    BUILD_DATE     = "${BUILD_DATE}"
    VERSION        = "${VERSION}"
    GIT_REVISION   = "${GIT_REVISION}"
  }

  platforms = [
    "linux/amd64",
    "linux/arm64"
  ]

  tags = gcp_tags(GCP_REGISTRY, GCP_REGION, GCP_PROJECT, REPO_NAME, VERSION)

  cache-from = [
    "type=gha",
    "type=local,src=/tmp/.buildx-cache"
  ]

  cache-to = [
    "type=gha,mode=max"
  ]

  output = ["type=registry"]
}

# AMD64 only target (for faster testing)
target "amd64" {
  inherits = ["cre"]
  platforms = ["linux/amd64"]
  output = ["type=docker"]
}

# ARM64 only target (for Apple Silicon testing)
target "arm64" {
  inherits = ["cre"]
  platforms = ["linux/arm64"]
  output = ["type=docker"]
}

# Security scanning target (for Trivy/Grype)
target "scan" {
  inherits = ["cre"]
  platforms = ["linux/amd64"]
  output = ["type=docker"]
  target = "runtime"
}

# Minimal runtime target (for production)
target "runtime" {
  inherits = ["cre"]
  target = "runtime"
}
