# -----------------------------------------------------------------------------
# Audit Logging Module - Centralized Log Retention
# GCP Security Module for CRE Terraform
#
# Implements:
# - Log Router sink for CRE audit logs
# - BigQuery dataset for long-term retention (400 days SOX compliance)
# - Aggregated log export for workflow receipts and XES events
# -----------------------------------------------------------------------------

# BigQuery dataset for audit logs
resource "google_bigquery_dataset" "audit_logs" {
  dataset_id  = "${var.name_prefix}_audit_logs"
  project     = var.project_id
  location    = var.region

  # Set default table expiration (convert days to milliseconds)
  # 400 days for SOX compliance (configurable via variable)
  default_table_expiration_ms = var.retention_days * 24 * 60 * 60 * 1000

  labels = var.common_labels

  annotations = {
    description = "CRE audit logs for compliance retention (SOX 400 days)"
    compliance  = "SOX,HIPAA,PCI-DSS"
    managed_by  = "terraform"
  }

  # Access control for audit logs
  # Dataset owner (project owner/editor) has full access
  # Log Router sink has write access (granted below)
}

# Log Router sink for CRE audit logs
resource "google_logging_project_sink" "audit_logs_sink" {
  name        = "${var.name_prefix}-audit-logs-sink"
  project     = var.project_id
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${google_bigquery_dataset.audit_logs.dataset_id}"

  # Filter for CRE audit log entries
  # Includes:
  # - cre-audit-log log name (from wf_audit_log_cloud)
  # - XES event logs (from xes_serial)
  # - Workflow engine logs with jsonPayload.source=cre
  filter = var.log_filter

  # Use unique writer identity for security (automatic IAM management)
  unique_writer_identity = true

  # Exclude debug logs from sink to reduce storage costs
  exclusions {
    name    = "exclude-health-check-debug"
    filter  = "resource.type=\"k8s_container\" AND resource.labels.container_name=\"cre\" AND jsonPayload.level=\"DEBUG\""
  }

  # Exclude noisy readiness/liveness probe logs
  exclusions {
    name    = "exclude-health-check-probes"
    filter  = "resource.type=\"k8s_container\" AND httpRequest.requestPath=\"/status.json\""
  }
}

# Grant sink permission to write to BigQuery dataset
# The sink's unique writer identity needs BigQuery dataEditor role
resource "google_bigquery_dataset_iam_member" "sink_writer" {
  project    = var.project_id
  dataset_id = google_bigquery_dataset.audit_logs.dataset_id
  role       = "roles/bigquery.dataEditor"
  member     = google_logging_project_sink.audit_logs_sink.writer_identity
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------

output "audit_logs_dataset_id" {
  description = "BigQuery dataset ID for audit logs"
  value       = google_bigquery_dataset.audit_logs.dataset_id
}

output "audit_logs_sink_name" {
  description = "Log Router sink name for CRE audit logs"
  value       = google_logging_project_sink.audit_logs_sink.name
}

output "audit_logs_sink_writer_identity" {
  description = "Log Router sink writer identity (for debugging IAM issues)"
  value       = google_logging_project_sink.audit_logs_sink.writer_identity
}

output "audit_logs_retention_days" {
  description = "Log retention period in days"
  value       = var.retention_days
}
