# -----------------------------------------------------------------------------
# GCP Security Module - Main Configuration
# -----------------------------------------------------------------------------

terraform {
  required_version = ">= 1.3.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 4.80.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 4.80.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = ">= 2.23.0"
    }
    random = {
      source  = "hashicorp/random"
      version = ">= 3.5.0"
    }
  }
}

# -----------------------------------------------------------------------------
# Google Provider
# -----------------------------------------------------------------------------
provider "google" {
  project = var.project_id
  region  = var.region

  # Use Application Default Credentials (ADC)
  # For local development: gcloud auth application-default login
  # For CI/CD: Use Workload Identity Federation (no keys!)
}

# -----------------------------------------------------------------------------
# Google Beta Provider
# Required for Workload Identity Federation resources
# -----------------------------------------------------------------------------
provider "google-beta" {
  project = var.project_id
  region  = var.region
}

# -----------------------------------------------------------------------------
# Kubernetes Provider
# Used for Network Policies and Pod Security Standards
# Configuration is done via alias below with dynamic GKE authentication
# -----------------------------------------------------------------------------
# provider "kubernetes" {
#   Configuration done via "gke" alias below
# }

# -----------------------------------------------------------------------------
# Data Sources
# -----------------------------------------------------------------------------

# Get current project info
data "google_project" "current" {
  project_id = var.project_id
}

# Get GKE cluster info for Workload Identity Federation
data "google_container_cluster" "current" {
  name     = var.cluster_name
  location = var.region
}

# -----------------------------------------------------------------------------
# Kubernetes Provider from GKE
# Dynamically configure Kubernetes provider from GKE cluster data
# -----------------------------------------------------------------------------
data "google_client_config" "default" {}

data "google_container_cluster" "cre_cluster" {
  name     = var.cluster_name
  location = var.region
}

provider "kubernetes" {
  alias = "gke"

  host  = "https://${data.google_container_cluster.cre_cluster.endpoint}"
  token = data.google_client_config.default.access_token
  cluster_ca_certificate = base64decode(
    data.google_container_cluster.cre_cluster.master_auth[0].cluster_ca_certificate
  )
}

# -----------------------------------------------------------------------------
# Module Resources
# Actual resources are defined in the component .tf files:
# - iam.tf: Service accounts and IAM bindings
# - workload_identity.tf: Workload Identity Federation pools
# - secrets.tf: Secret Manager secrets
# - network_policy.tf: Kubernetes network policies
# - iap.tf: Identity-Aware Proxy configuration
# -----------------------------------------------------------------------------
