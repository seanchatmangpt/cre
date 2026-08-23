# terraform/gcp/modules/storage/main.tf
# StorageClasses and PVCs for CRE Mnesia and data storage

locals {
  storage_class_defaults = {
    ssd = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-ssd"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type               = "pd-ssd"
        fstype             = "ext4"
        replication-type    = "none"
        # Add CMEK key if provided (optional)
        encryptionKeyKMSKey = try(var.cmek_key_name, null)
      }
    }
    ssd_regional = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-ssd"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type               = "pd-ssd"
        fstype             = "ext4"
        replication-type    = "regional-pd"
        # Add CMEK key if provided (optional)
        encryptionKeyKMSKey = try(var.cmek_key_name, null)
      }
    }
    balanced = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-balanced"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type               = "pd-balanced"
        fstype             = "ext4"
        replication-type    = "none"
        # Add CMEK key if provided (optional)
        encryptionKeyKMSKey = try(var.cmek_key_name, null)
      }
    }
    standard = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-standard"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type               = "pd-standard"
        fstype             = "ext4"
        replication-type    = "none"
        # Add CMEK key if provided (optional)
        encryptionKeyKMSKey = try(var.cmek_key_name, null)
      }
    }
  }
}

# Merge defaults with user-provided storage classes
locals {
  merged_storage_classes = {
    for key, defaults in local.storage_class_defaults :
    key => merge(defaults, try(var.storage_classes[key], {}))
  }
}

# StorageClasses - Note: These are data sources, resources will be created via Kubernetes manifests
# The actual StorageClass resources are managed by the GKE control plane
# This module outputs manifests that can be applied via kubectl or Helm

# Data source for the cluster (to get endpoint for kubectl provider)
data "google_client_config" "default" {}

# Persistent disk snapshot schedule for Mnesia data
resource "google_compute_disk_resource_policy_attachment" "mnesia_snapshot" {
  count = var.enable_snapshots && var.snapshot_schedule.enabled ? 1 : 0

  name    = google_compute_resource_policy.snapshot_schedule[0].name
  project = var.project_id
  zone    = "${var.region}-a"
  disk    = "" # Will be attached to dynamically created disks
}

# Snapshot schedule policy
resource "google_compute_resource_policy" "snapshot_schedule" {
  count   = var.enable_snapshots && var.snapshot_schedule.enabled ? 1 : 0
  name    = "${var.cluster_name}-snapshot-schedule"
  project = var.project_id
  region  = var.region

  snapshot_schedule_policy {
    schedule {
      daily_schedule {
        days_in_cycle = 1
        start_time    = "03:00"
      }
    }

    retention_policy {
      max_retention_days    = var.snapshot_schedule.retention_days
      on_source_disk_delete = "KEEP_AUTO_SNAPSHOTS"
    }

    snapshot_properties {
      guest_flush       = true
      storage_locations = [var.snapshot_schedule.snapshot_location]
      labels            = var.labels
    }
  }
}

# Backup scheduling (using Cloud Scheduler + Cloud Functions)
# This is a placeholder for backup implementation
# Actual backup would be implemented via Kubernetes CronJob

resource "google_cloud_scheduler_job" "mnesia_backup" {
  count       = var.backup_config.enabled ? 1 : 0
  name        = "${var.cluster_name}-mnesia-backup"
  project     = var.project_id
  region      = var.region
  description = "Scheduled backup for Mnesia data"

  schedule  = var.backup_config.schedule
  time_zone = "UTC"

  http_target {
    http_method = "POST"
    uri         = "https://${var.cluster_name}.endpoints.${var.region}.cloud.goog/api/v1/backup/mnesia"
    oidc_token {
      service_account_email = "" # To be filled with actual service account
    }
  }
}

# Kubernetes provider configuration (optional, for direct Kubernetes resource creation)
# This requires the cluster to exist and credentials to be available
# Uncomment and configure if direct Kubernetes resource creation is needed

# provider "kubernetes" {
#   host                   = "https://${google_container_cluster.primary.endpoint}"
#   token                  = data.google_client_config.default.access_token
#   cluster_ca_certificate = base64decode(google_container_cluster.primary.master_auth[0].cluster_ca_certificate)
# }
