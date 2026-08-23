# -----------------------------------------------------------------------------
# Outputs - Audit Logging Module
# (Duplicate outputs for convenience - also defined in main.tf)
# -----------------------------------------------------------------------------

output "audit_logs_dataset_id" {
  description = "BigQuery dataset ID for audit logs (format: name_prefix_audit_logs)"
  value       = google_bigquery_dataset.audit_logs.dataset_id
}

output "audit_logs_dataset_full_name" {
  description = "Full BigQuery dataset name (format: project_id:dataset_id)"
  value       = "${var.project_id}.${google_bigquery_dataset.audit_logs.dataset_id}"
}

output "audit_logs_sink_name" {
  description = "Log Router sink name for CRE audit logs"
  value       = google_logging_project_sink.audit_logs_sink.name
}

output "audit_logs_sink_destination" {
  description = "Log Router sink destination (BigQuery dataset)"
  value       = google_logging_project_sink.audit_logs_sink.destination
}

output "audit_logs_sink_writer_identity" {
  description = "Log Router sink writer identity (service account email)"
  value       = google_logging_project_sink.audit_logs_sink.writer_identity
  sensitive   = true
}

output "audit_logs_retention_days" {
  description = "Log retention period in days"
  value       = var.retention_days
}

output "audit_logs_query_example" {
  description = "Example SQL query for audit logs"
  value       = <<-EOT
# Query audit logs from BigQuery
SELECT
  timestamp,
  jsonPayload.before_hash,
  jsonPayload.after_hash,
  jsonPayload.move,
  jsonPayload.ts
FROM \`${var.project_id}.${google_bigquery_dataset.audit_logs.dataset_id}\`
WHERE timestamp > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 1 HOUR)
ORDER BY timestamp DESC
LIMIT 100
EOT
}
