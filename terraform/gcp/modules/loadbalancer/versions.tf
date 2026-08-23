# terraform/gcp/modules/loadbalancer/versions.tf
# Provider version constraints for load balancer module

terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 6.0"
    }
  }

  required_version = ">= 1.5.0"
}
