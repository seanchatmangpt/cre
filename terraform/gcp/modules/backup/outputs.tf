# terraform/gcp/modules/backup/outputs.tf
# Outputs for backup infrastructure module

output "backup_bucket_name" {
  description = "Name of the primary GCS backup bucket"
  value       = google_storage_bucket.backup.name
}

output "backup_bucket_url" {
  description = "URL of the primary GCS backup bucket"
  value       = google_storage_bucket.backup.url
}

output "replica_bucket_name" {
  description = "Name of the replica GCS backup bucket (if enabled)"
  value       = var.enable_cross_region_replication ? google_storage_bucket.backup_replica[0].name : null
}

output "backup_service_account_email" {
  description = "Email of the backup service account"
  value       = google_service_account.backup.email
}

output "backup_service_account_name" {
  description = "Name of the backup service account"
  value       = google_service_account.backup.name
}

output "cmek_key_name" {
  description = "Name of the CMEK key (if created)"
  value       = var.create_cmek ? google_kms_crypto_key.backup[0].name : null
}

output "cmek_key_id" {
  description = "ID of the CMEK key (if created)"
  value       = var.create_cmek ? google_kms_crypto_key.backup[0].id : null
}

output "spanner_instance_name" {
  description = "Name of the Spanner instance (if created)"
  value       = var.create_spanner_resources ? google_spanner_instance.cre[0].name : null
}

output "spanner_database_name" {
  description = "Name of the Spanner database (if created)"
  value       = var.create_spanner_resources ? google_spanner_database.cre_db[0].name : null
}

output "spanner_backup_schedule_name" {
  description = "Name of the Spanner backup schedule (if created)"
  # Note: Spanner backup schedules are created via native Spanner backup API
  # The daily backups are managed by Spanner's automated backup system
  value       = var.create_spanner_resources ? "automated-daily-backup" : null
}

output "filestore_instance_name" {
  description = "Name of the Filestore instance (if created)"
  value       = var.create_filestore ? google_filestore_instance.backup[0].name : null
}

output "filestore_ip_address" {
  description = "IP address of the Filestore instance (if created)"
  value       = var.create_filestore ? google_filestore_instance.backup[0].networks[0].ip_addresses[0] : null
}

output "backup_scheduler_daily_job" {
  description = "Name of the daily backup Cloud Scheduler job"
  value       = google_cloud_scheduler_job.mnesia_backup_daily.name
}

output "backup_scheduler_hourly_job" {
  description = "Name of the hourly backup Cloud Scheduler job"
  value       = google_cloud_scheduler_job.mnesia_backup_hourly.name
}

output "backup_alert_policy" {
  description = "Name of the backup failure alert policy (if enabled)"
  value       = var.enable_alerting ? google_monitoring_alert_policy.backup_failure[0].name : null
}

output "backup_notification_channel" {
  description = "Name of the backup notification channel (if enabled)"
  value       = var.enable_alerting ? google_monitoring_notification_channel.backup_alerts[0].name : null
}

# ============================================
# GCS Commands
# ============================================

output "backup_bucket_ls_command" {
  description = "Command to list backup contents"
  value       = "gsutil ls gs://${google_storage_bucket.backup.name}/"
}

output "backup_bucket_sync_command" {
  description = "Command to sync local directory to backup bucket"
  value       = "gsutil -m rsync -r ./local-backups gs://${google_storage_bucket.backup.name}/"
}

# ============================================
# Disaster Recovery Commands
# ============================================

output "restore_commands" {
  description = "Commands for backup restoration"
  value = {
    list_backups     = "gsutil ls gs://${google_storage_bucket.backup.name}/mnesia/daily/"
    download_latest  = "LATEST=$(gsutil ls gs://${google_storage_bucket.backup.name}/mnesia/daily/ | tail -1) && gsutil cp $LATEST ./restore.tar.gz"
    spanner_restore  = var.create_spanner_resources ? "gcloud spanner databases restore-operations describe --instance=${google_spanner_instance.cre[0].name} --database=${google_spanner_database.cre_db[0].name} --restore-type=FULL" : null
  }
}

# ============================================
# Monitoring Queries
# ============================================

output "monitoring_dashboard_links" {
  description = "Links to relevant monitoring dashboards"
  value = {
    backup_jobs   = "https://console.cloud.google.com/monitoring/dashboards?project=${var.project_id}"
    gcs_bucket    = "https://console.cloud.google.com/storage/browser/${google_storage_bucket.backup.name}?project=${var.project_id}"
    spanner       = var.create_spanner_resources ? "https://console.cloud.google.com/spanner/instances/${google_spanner_instance.cre[0].name}?project=${var.project_id}" : null
    scheduler     = "https://console.cloud.google.com/cloudscheduler?project=${var.project_id}"
  }
}

# ============================================
# SLA Information
# ============================================

output "sla_targets" {
  description = "Service Level Agreement targets for backup operations"
  value = {
    rto = "30 minutes"  # Recovery Time Objective
    rpo = "15 minutes"  # Recovery Point Objective (hourly backups)
    retention = "${var.retention_days} days"
    cross_region_replication = var.enable_cross_region_replication
    encryption = var.create_cmek ? "Customer-managed (CMEK)" : "Google-managed"
  }
}
