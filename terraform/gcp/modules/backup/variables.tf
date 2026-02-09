# terraform/gcp/modules/backup/variables.tf
# Variables for backup infrastructure module

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "backup_location" {
  description = "Primary region for backup storage"
  type        = string
  default     = "us-central1"
}

variable "replication_location" {
  description = "Secondary region for backup replication"
  type        = string
  default     = "us-east1"
}

variable "labels" {
  description = "Labels to apply to backup resources"
  type        = map(string)
  default     = {}
}

# ============================================
# Backup Configuration
# ============================================

variable "retention_days" {
  description = "Default retention period for backups in days"
  type        = number
  default     = 30

  validation {
    condition     = var.retention_days >= 1 && var.retention_days <= 3650
    error_message = "Retention days must be between 1 and 3650 (10 years)."
  }
}

variable "replica_retention_days" {
  description = "Retention period for replica backups in days"
  type        = number
  default     = 7
}

variable "soft_delete_seconds" {
  description = "Soft delete retention period for backup bucket"
  type        = number
  default     = 604800  # 7 days
}

variable "enable_cross_region_replication" {
  description = "Enable cross-region backup replication"
  type        = bool
  default     = true
}

variable "enable_storage_class_transitions" {
  description = "Enable automatic storage class transitions for cost optimization"
  type        = bool
  default     = true
}

variable "enable_logging" {
  description = "Enable access logging for backup operations"
  type        = bool
  default     = true
}

# ============================================
# Encryption Configuration
# ============================================

variable "create_cmek" {
  description = "Create a customer-managed encryption key (CMEK) for backups"
  type        = bool
  default     = false
}

variable "cmek_key_name" {
  description = "Existing CMEK key name (if not creating one)"
  type        = string
  default     = null
}

# ============================================
# Spanner Configuration
# ============================================

variable "create_spanner_resources" {
  description = "Create Spanner instance and database with backup configuration"
  type        = bool
  default     = true
}

variable "spanner_instance_name" {
  description = "Name of the Spanner instance"
  type        = string
  default     = "cre-spanner"
}

variable "spanner_database_name" {
  description = "Name of the Spanner database"
  type        = string
  default     = "cre-db"
}

variable "spanner_config" {
  description = "Spanner instance configuration"
  type        = string
  default     = "regional-us-central1"

  validation {
    condition     = can(regex("^regional-", var.spanner_config)) || can(regex("^nam[0-9]$", var.spanner_config))
    error_message = "Spanner config must be a valid regional or multi-region configuration."
  }
}

variable "spanner_num_nodes" {
  description = "Number of nodes for Spanner instance"
  type        = number
  default     = 1

  validation {
    condition     = var.spanner_num_nodes >= 1 && var.spanner_num_nodes <= 1000
    error_message = "Spanner nodes must be between 1 and 1000."
  }
}

# ============================================
# Filestore Configuration
# ============================================

variable "create_filestore" {
  description = "Create Filestore instance for shared backup storage"
  type        = bool
  default     = false
}

variable "filestore_capacity_gb" {
  description = "Capacity of Filestore instance in GB"
  type        = number
  default     = 1024

  validation {
    condition     = var.filestore_capacity_gb >= 2560 || var.filestore_capacity_gb == 1024
    error_message = "Filestore capacity must be at least 2560 GB (except for testing)."
  }
}

variable "network_name" {
  description = "VPC network name for Filestore"
  type        = string
  default     = "cre-vpc"
}

# ============================================
# Monitoring and Alerting
# ============================================

variable "enable_alerting" {
  description = "Enable backup monitoring and alerting"
  type        = bool
  default     = true
}

variable "alert_email" {
  description = "Email address for backup alerts"
  type        = string
  default     = ""
}

variable "alert_documentation" {
  description = "Documentation content for backup alerts"
  type        = string
  default     = <<EOF
# Backup Failure Alert

A backup operation has failed. Please investigate:
1. Check Cloud Logging for backup job logs
2. Verify GCS bucket access
3. Check service account permissions
4. Review backup CronJob status in GKE

## Recovery Steps
- Run manual backup: `kubectl create job --from=cronjob/cre-mnesia-backup-daily manual-backup`
- Verify backup GCS bucket: `gsutil ls gs://PROJECT_ID-cre-backups/`

## SLA Impact
- RPO may be exceeded if backup continues to fail
- Data loss risk increases with each failed backup
EOF
}

# ============================================
# Scheduler Configuration
# ============================================

variable "backup_endpoint" {
  description = "Endpoint for backup HTTP triggers"
  type        = string
  default     = "https://backup.example.com/api/v1"
}

variable "backup_audience" {
  description = "Audience for OIDC token authentication"
  type        = string
  default     = "backup-service"
}
