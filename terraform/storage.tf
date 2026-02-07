terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

variable "project_id" {
  description = "Google Cloud Project ID"
  type        = string
}

variable "region" {
  description = "Google Cloud region"
  type        = string
  default     = "us-central1"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "dev"
}

variable "buckets" {
  description = "GCS bucket configurations"
  type = map(object({
    location           = string
    storage_class      = string
    versioning         = bool
    lifecycle_rules    = optional(list(object({
      age_days          = optional(number)
      num_newer_versions = optional(number)
      action            = string
    })), [])
    iam_bindings = optional(map(list(string)), {})
  }))
  default = {}
}

resource "google_storage_bucket" "main" {
  for_each = var.buckets

  name          = "${var.project_id}-${each.key}-${var.environment}"
  location      = each.value.location
  storage_class = each.value.storage_class

  force_destroy = var.environment == "dev" ? true : false

  uniform_bucket_level_access = true

  versioning {
    enabled = each.value.versioning
  }

  dynamic "lifecycle_rule" {
    for_each = each.value.lifecycle_rules

    content {
      action {
        type = lifecycle_rule.value.action
      }

      condition {
        age                     = lifecycle_rule.value.age_days
        num_newer_versions      = lifecycle_rule.value.num_newer_versions
        matches_storage_class   = [each.value.storage_class]
        matches_prefix          = []
      }
    }
  }

  labels = {
    environment = var.environment
    managed_by  = "terraform"
  }
}

resource "google_storage_bucket_iam_binding" "main" {
  for_each = merge([
    for bucket_key, bucket_config in var.buckets : {
      for role, members in bucket_config.iam_bindings : "${bucket_key}-${role}" => {
        bucket  = google_storage_bucket.main[bucket_key].name
        role    = role
        members = members
      }
    }
  ]...)

  bucket = each.value.bucket
  role   = each.value.role
  members = each.value.members
}

resource "google_storage_bucket_iam_member" "owner" {
  for_each = {
    for key, bucket in google_storage_bucket.main : key => bucket.name
  }

  bucket = each.value
  role   = "roles/storage.admin"
  member = "serviceAccount:${data.google_client_config.current.project}@appspot.gserviceaccount.com"
}

data "google_client_config" "current" {}

output "bucket_names" {
  description = "Names of created GCS buckets"
  value = {
    for key, bucket in google_storage_bucket.main : key => bucket.name
  }
}

output "bucket_self_links" {
  description = "Self links of created GCS buckets"
  value = {
    for key, bucket in google_storage_bucket.main : key => bucket.self_link
  }
}

output "bucket_urls" {
  description = "URLs of created GCS buckets"
  value = {
    for key, bucket in google_storage_bucket.main : key => "gs://${bucket.name}"
  }
}
