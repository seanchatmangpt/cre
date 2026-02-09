# terraform/gcp/modules/gke_cluster/versions.tf
# Provider version constraints for GKE cluster module

terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 6.0"
    }
  }

  required_version = ">= 1.5.0"
}
