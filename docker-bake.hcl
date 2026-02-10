# CRE Multi-Platform Docker Build Configuration
# Usage:
#   docker buildx bake --load cre              # Build for current platform
#   docker buildx bake cre                     # Build all platforms (no load)
#   docker buildx bake cre --push              # Build and push to registry
#   docker buildx bake --set *.args.GCP_REGION=us-central1 cre

variable "REGISTRY" {
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

group "default" {
  targets = ["cre"]
}

target "cre" {
  args = {
    BUILD_DATE     = "${BUILD_DATE}"
    VERSION        = "${VERSION}"
    GIT_REVISION   = "${GIT_REVISION}"
  }

  platforms = [
    "linux/amd64",
    "linux/arm64"
  ]

  tags = [
    "${REGISTRY}/${GCP_PROJECT}/${REPO_NAME}/cre:${VERSION}",
    "${REGISTRY}/${GCP_PROJECT}/${REPO_NAME}/cre:latest",
  ]

  output = [
    "type=image,push=false"
  ]

  cache-from = [
    "type=local,src=/tmp/.buildx-cache"
  ]

  cache-to = [
    "type=local,dest=/tmp/.buildx-cache"
  ]
}

# Local development target (single platform, loads to Docker)
target "local" {
  inherits = ["cre"]
  output = ["type=docker"]
}

# Release target with push enabled
target "release" {
  inherits = ["cre"]
  output = ["type=registry"]
}

# AMD64 only target
target "amd64" {
  inherits = ["cre"]
  platforms = ["linux/amd64"]
}

# ARM64 only target
target "arm64" {
  inherits = ["cre"]
  platforms = ["linux/arm64"]
}

# SBOM generation target
target "sbom" {
  inherits = ["cre"]
  target = "sbom"
  output = ["type=local,dest=./sbom"]
}

# GKE deployment target (optimizations for GKE)
target "gke" {
  inherits = ["cre"]
  args = {
    GCP_REGION = "${GCP_REGION}"
  }
}
