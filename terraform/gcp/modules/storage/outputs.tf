# terraform/gcp/modules/storage/outputs.tf
# Output specifications for storage module

output "storage_classes" {
  description = "StorageClass configurations"
  value = {
    for name, config in local.merged_storage_classes : name => {
      name                   = "${var.cluster_name}-${name}"
      provisioner            = config.provisioner
      type                   = config.type
      volume_binding_mode    = config.volume_binding_mode
      allow_volume_expansion = config.allow_volume_expansion
      reclaim_policy         = config.reclaim_policy
      parameters             = config.parameters
    }
  }
}

output "pvcs" {
  description = "PVC configurations"
  value = {
    for name, config in var.pvcs : name => {
      name               = "${var.cluster_name}-${name}"
      storage_class_name = config.storage_class_name
      size               = config.size
      access_modes       = config.access_modes
      labels             = merge(config.labels, var.labels)
    }
  }
}

output "snapshot_policy_name" {
  description = "Snapshot resource policy name"
  value       = var.enable_snapshots && var.snapshot_schedule.enabled ? google_compute_resource_policy.snapshot_schedule[0].name : null
}

output "backup_job_name" {
  description = "Cloud Scheduler job name for backups"
  value       = var.backup_config.enabled ? google_cloud_scheduler_job.mnesia_backup[0].name : null
}

output "recommended_mount_options" {
  description = "Recommended mount options for Mnesia disks"
  value = {
    ssd = {
      mount_options = ["discard", "noatime"]
      fs_type       = "ext4"
    }
    ssd_regional = {
      mount_options = ["discard", "noatime"]
      fs_type       = "ext4"
    }
  }
}
