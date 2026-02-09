# terraform/gcp/versions.tf
# Provider versions and GCS backend configuration

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 6.0"
    }
  }

  # GCS backend for state storage
  backend "gcs" {
    # These values should be configured via CLI or terraform.tfvars
    # bucket = "cre-terraform-state"
    # prefix = "gcp/production"
  }
}

# Google provider configuration
provider "google" {
  project = var.project_id
  region  = var.region
  zone    = var.zone

  # Use user-supplied credentials if available
  # credentials = file(var.credentials_file)

  default_labels = {
    environment = var.environment
    managed_by  = "terraform"
    project     = "cre"
  }
}

provider "google-beta" {
  project = var.project_id
  region  = var.region
  zone    = var.zone
}
