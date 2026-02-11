# -----------------------------------------------------------------------------
# Variables - Audit Logging Module
# -----------------------------------------------------------------------------

variable "name_prefix" {
  description = "Prefix for resource names (e.g., 'cre', 'cre-prod')"
  type        = string
  validation {
    condition     = can(regex("^[a-z0-9-]+$", var.name_prefix))
    error_message = "name_prefix must contain only lowercase letters, numbers, and hyphens."
  }
}

variable "project_id" {
  description = "GCP project ID where CRE is deployed"
  type        = string
}

variable "region" {
  description = "GCP region for BigQuery dataset (e.g., 'us-central1')"
  type        = string
}

variable "retention_days" {
  description = "Log retention period in days (400 for SOX compliance, 365 for HIPAA, 90 for PCI-DSS minimum)"
  type        = number
  default     = 400

  validation {
    condition     = var.retention_days >= 1
    error_message = "retention_days must be at least 1 day."
  }

  validation {
    condition     = var.retention_days <= 3660  # BigQuery max (10 years)
    error_message = "retention_days cannot exceed 3660 days (BigQuery maximum)."
  }
}

variable "log_filter" {
  description = "Log filter for CRE audit log entries (uses Cloud Logging filter syntax)"
  type        = string
  default     = <<EOT
logName:"projects/PROJECT_ID/logs/cre-audit-log" OR
jsonPayload.source="cre" OR
jsonPayload.logName="xes-events"
EOT

  # Note: The default filter above references PROJECT_ID which should be replaced
  # via templatefile() or var.project_id interpolation in the calling module
}

variable "common_labels" {
  description = "Common labels to apply to GCP resources (e.g., {environment = \"prod\"})"
  type        = map(string)
  default     = {}
}
