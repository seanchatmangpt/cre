# terraform/gcp/modules/backup/main.tf
# Backup infrastructure module for CRE on GCP
# Provides automated backups for Mnesia (via GCS) and Spanner

locals {
  # Backup retention schedules
  retention_policies = {
    hourly  = { retention_days = 2, schedule = "0 * * * *" }
    daily   = { retention_days = 30, schedule = "0 2 * * *" }
    weekly  = { retention_days = 90, schedule = "0 3 * * 0" }
    monthly = { retention_days = 365, schedule = "0 4 1 * *" }
  }

  # Backup storage classes based on access frequency
  storage_classes = {
    hourly  = "NEARLINE" # Accessed frequently
    daily   = "NEARLINE" # Accessed occasionally
    weekly  = "COLDLINE" # Accessed rarely
    monthly = "ARCHIVE"  # Accessed very rarely
  }
}

# ============================================
# Enable Required APIs
# ============================================

resource "google_project_service" "enabled" {
  for_each = toset([
    "storage-component.googleapis.com",
    "spanner.googleapis.com",
    "file.googleapis.com",
    "cloudkms.googleapis.com",
    "cloudscheduler.googleapis.com",
    "monitoring.googleapis.com",
  ])

  project            = var.project_id
  service            = each.key
  disable_on_destroy = false
}

# ============================================
# GCS Bucket for Backups
# ============================================

resource "google_storage_bucket" "backup" {
  name          = "${var.project_id}-cre-backups"
  location      = var.backup_location
  force_destroy = false

  uniform_bucket_level_access = true
  public_access_prevention    = "enforced"

  versioning {
    enabled = true
  }

  # Enable soft delete for recovery
  soft_delete_policy {
    retention_duration_seconds = var.soft_delete_seconds
  }

  # Lifecycle management for retention
  lifecycle_rule {
    condition {
      age = var.retention_days
    }
    action {
      type = "Delete"
    }
  }

  # Auto-transition to cheaper storage classes
  dynamic "lifecycle_rule" {
    for_each = var.enable_storage_class_transitions ? [1] : []
    content {
      condition {
        age = 30
      }
      action {
        type          = "SetStorageClass"
        storage_class = "NEARLINE"
      }
    }
  }

  dynamic "lifecycle_rule" {
    for_each = var.enable_storage_class_transitions ? [1] : []
    content {
      condition {
        age = 90
      }
      action {
        type          = "SetStorageClass"
        storage_class = "COLDLINE"
      }
    }
  }

  # Encryption configuration
  dynamic "encryption" {
    for_each = var.cmek_key_name != null ? [1] : []
    content {
      default_kms_key_name = var.cmek_key_name
    }
  }

  # Logging
  dynamic "logging" {
    for_each = var.enable_logging ? [1] : []
    content {
      log_bucket = google_storage_bucket.logs[0].id
    }
  }

  labels = merge(var.labels, {
    type       = "backup"
    managed_by = "terraform"
  })
}

# ============================================
# Cross-Region Replication Bucket
# ============================================

resource "google_storage_bucket" "backup_replica" {
  count = var.enable_cross_region_replication ? 1 : 0

  name          = "${var.project_id}-cre-backups-replica"
  location      = var.replication_location
  force_destroy = false

  uniform_bucket_level_access = true
  public_access_prevention    = "enforced"

  versioning {
    enabled = true
  }

  # Replica has shorter retention (for DR only)
  lifecycle_rule {
    condition {
      age = var.replica_retention_days
    }
    action {
      type = "Delete"
    }
  }

  labels = merge(var.labels, {
    type       = "backup-replica"
    managed_by = "terraform"
  })
}

# ============================================
# Logging Bucket
# ============================================

resource "google_storage_bucket" "logs" {
  count = var.enable_logging ? 1 : 0

  name          = "${var.project_id}-cre-backup-logs"
  location      = var.backup_location
  force_destroy = false

  uniform_bucket_level_access = true

  lifecycle_rule {
    condition {
      age = 90
    }
    action {
      type = "Delete"
    }
  }

  labels = merge(var.labels, {
    type = "backup-logs"
  })
}

# ============================================
# Backup Service Account
# ============================================

resource "google_service_account" "backup" {
  project      = var.project_id
  account_id   = "cre-backup-sa"
  display_name = "CRE Backup Service Account"
  description  = "Service account for automated CRE backups to GCS"

  depends_on = [google_project_service.enabled]
}

# ============================================
# Custom Encryption Key (CMEK)
# ============================================

resource "google_kms_key_ring" "backup" {
  count    = var.create_cmek ? 1 : 0
  name     = "cre-backup-keyring"
  location = var.backup_location

  depends_on = [google_project_service.enabled]
}

resource "google_kms_crypto_key" "backup" {
  count    = var.create_cmek ? 1 : 0
  name     = "cre-backup-key"
  key_ring = google_kms_key_ring.backup[0].id
  purpose  = "ENCRYPT_DECRYPT"

  version_template {
    algorithm        = "GOOGLE_SYMMETRIC_ENCRYPTION"
    protection_level = "SOFTWARE"
  }

  rotation_period = "7776000s" # 90 days

  lifecycle {
    prevent_destroy = true
  }
}

# ============================================
# Cloud Spanner Backup Configuration
# ============================================

resource "google_spanner_database" "cre_db" {
  count = var.create_spanner_resources ? 1 : 0

  name                = var.spanner_database_name
  instance            = google_spanner_instance.cre[0].name
  deletion_protection = true

  version_retention_period = "7d"
  enable_drop_protection   = true

  ddl = [
    "CREATE TABLE workflow_cases (",
    "  case_id STRING(36) NOT NULL,",
    "  workflow_id STRING(64) NOT NULL,",
    "  spec BYTES(MAX) NOT NULL,",
    "  status STRING(20) NOT NULL,",
    "  data JSON,",
    "  created_at INT64 NOT NULL,",
    "  started_at INT64,",
    "  completed_at INT64,",
    "  updated_at INT64 NOT NULL",
    ") PRIMARY KEY(case_id)",
  ]

  depends_on = [google_project_service.enabled]
}

resource "google_spanner_instance" "cre" {
  count = var.create_spanner_resources ? 1 : 0

  name         = var.spanner_instance_name
  config       = var.spanner_config
  display_name = "CRE Spanner Instance"
  num_nodes    = var.spanner_num_nodes

  processing_units = null

  labels = var.labels

  depends_on = [google_project_service.enabled]
}

# ============================================
# Point-in-Time Recovery
# ============================================

resource "google_spanner_database_iam_member" "pit_access" {
  count = var.create_spanner_resources ? 1 : 0

  database = google_spanner_database.cre_db[0].name
  instance = google_spanner_instance.cre[0].name
  role     = "roles/spanner.databaseUser"
  member   = "serviceAccount:${google_service_account.backup.email}"
}

# ============================================
# Filestore Instance (for shared storage backup)
# ============================================

resource "google_filestore_instance" "backup" {
  count = var.create_filestore ? 1 : 0

  name     = "cre-filestore-backup"
  location = var.backup_location
  tier     = "BASIC_HDD"

  file_shares {
    name        = "cre-backups"
    capacity_gb = var.filestore_capacity_gb
  }

  networks {
    network = var.network_name
    modes   = ["MODE_IPV4"]
  }

  labels = var.labels
}

# ============================================
# IAM Permissions
# ============================================

# Service account permissions for backup operations
resource "google_project_iam_member" "backup_storage_object_admin" {
  project = var.project_id
  role    = "roles/storage.objectAdmin"
  member  = "serviceAccount:${google_service_account.backup.email}"
}

resource "google_storage_bucket_iam_member" "backup_object_admin" {
  bucket = google_storage_bucket.backup.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.backup.email}"
}

resource "google_storage_bucket_iam_member" "backup_replica" {
  count  = var.enable_cross_region_replication ? 1 : 0
  bucket = google_storage_bucket.backup_replica[0].name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.backup.email}"
}

resource "google_storage_bucket_iam_member" "backup_logging" {
  count  = var.enable_logging ? 1 : 0
  bucket = google_storage_bucket.logs[0].name
  role   = "roles/storage.objectCreator"
  member = "serviceAccount:${google_service_account.backup.email}"
}

# Spanner backup permissions
resource "google_project_iam_member" "backup_spanner_admin" {
  count   = var.create_spanner_resources ? 1 : 0
  project = var.project_id
  role    = "roles/spanner.backupWriter"
  member  = "serviceAccount:${google_service_account.backup.email}"
}

# KMS permissions for CMEK
resource "google_kms_crypto_key_iam_member" "backup_encrypter" {
  count         = var.create_cmek ? 1 : 0
  crypto_key_id = google_kms_crypto_key.backup[0].id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:${google_service_account.backup.email}"
}

resource "google_kms_crypto_key_iam_member" "backup_viewer" {
  count         = var.create_cmek ? 1 : 0
  crypto_key_id = google_kms_crypto_key.backup[0].id
  role          = "roles/cloudkms.viewer"
  member        = "serviceAccount:${google_service_account.backup.email}"
}

# ============================================
# Cloud Monitoring
# ============================================

resource "google_monitoring_notification_channel" "backup_alerts" {
  count        = var.enable_alerting && var.alert_email != "" ? 1 : 0
  display_name = "Backup Alert Channel"
  type         = "email"
  labels = {
    email_address = var.alert_email
  }

  force_delete = false
}

resource "google_monitoring_alert_policy" "backup_failure" {
  count        = var.enable_alerting && var.alert_email != "" ? 1 : 0
  display_name = "CRE Backup Failure Alert"
  combiner     = "OR"

  conditions {
    display_name = "Backup job failure"
    condition_threshold {
      filter = "resource.type=\"k8s_container\" AND metric.type=\"custom.googleapis.com/cre/backup/complete\" AND metric.labels.status=\"failed\""
      aggregations {
        alignment_period   = "300s"
        per_series_aligner = "ALIGN_COUNT"
      }
      comparison      = "COMPARISON_GT"
      threshold_value = 0
      duration        = "300s"
      trigger {
        count = 1
      }
    }
  }

  notification_channels = var.enable_alerting ? [google_monitoring_notification_channel.backup_alerts[0].id] : []

  documentation {
    mime_type = "text/markdown"
    content   = var.alert_documentation
  }
}

resource "google_monitoring_alert_policy" "backup_age" {
  count        = var.enable_alerting && var.alert_email != "" ? 1 : 0
  display_name = "CRE Backup Age Warning"
  combiner     = "OR"

  conditions {
    display_name = "Backup too old"
    condition_threshold {
      filter = "resource.type=\"k8s_container\" AND metric.type=\"custom.googleapis.com/cre/backup/age_hours\""
      aggregations {
        alignment_period   = "3600s"
        per_series_aligner = "ALIGN_MEAN"
      }
      comparison      = "COMPARISON_GT"
      threshold_value = 48 # Alert if backup is older than 48 hours
      duration        = "3600s"
      trigger {
        count = 1
      }
    }
  }

  notification_channels = var.enable_alerting ? [google_monitoring_notification_channel.backup_alerts[0].id] : []
}

# ============================================
# Cloud Scheduler for Backup Jobs
# ============================================

resource "google_cloud_scheduler_job" "mnesia_backup_daily" {
  name        = "cre-mnesia-backup-daily"
  description = "Daily Mnesia backup job"
  schedule    = "0 2 * * *"
  time_zone   = "UTC"

  retry_config {
    retry_count   = 3
    max_doublings = 5
  }

  http_target {
    http_method = "POST"
    uri         = "${var.backup_endpoint}/mnesia/daily"
    oidc_token {
      service_account_email = google_service_account.backup.email
      audience              = var.backup_audience
    }
  }

  depends_on = [google_project_service.enabled]
}

resource "google_cloud_scheduler_job" "mnesia_backup_hourly" {
  name        = "cre-mnesia-backup-hourly"
  description = "Hourly Mnesia backup job for RPO compliance"
  schedule    = "0 * * * *"
  time_zone   = "UTC"

  retry_config {
    retry_count   = 2
    max_doublings = 3
  }

  http_target {
    http_method = "POST"
    uri         = "${var.backup_endpoint}/mnesia/hourly"
    oidc_token {
      service_account_email = google_service_account.backup.email
      audience              = var.backup_audience
    }
  }

  depends_on = [google_project_service.enabled]
}
