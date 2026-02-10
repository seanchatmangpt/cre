# terraform/gcp/modules/monitoring/main.tf
# Main entry point for CRE monitoring module

# Data sources for existing resources
data "google_project" "project" {
  project_id = var.project_id
}

# Ensure BigQuery dataset exists for cost tracking
resource "google_bigquery_dataset" "cost_tracking" {
  count         = var.enable_cost_tracking ? 1 : 0
  project       = var.project_id
  dataset_id    = var.cost_dataset_id
  location      = var.region
  friendly_name = "CRE Cost Tracking"

  description = "Dataset for tracking CRE deployment costs and optimizations"

  labels = var.labels

  default_table_expiration_ms = 5364800000 # 60 days

  access {
    role          = "roles/bigquery.dataViewer"
    special_group = "projectReaders"
  }

  access {
    role          = "roles/bigquery.dataEditor"
    special_group = "projectEditors"
  }

  access {
    role          = "roles/bigquery.admin"
    special_group = "projectOwners"
  }
}
