# terraform/gcp/modules/storage/variables.tf
# Variable specifications for storage module

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "Region for storage resources"
  type        = string
  default     = "us-central1"
}

variable "cluster_name" {
  description = "Name of the GKE cluster (used for naming)"
  type        = string
  default     = "cre-cluster"
}

variable "storage_classes" {
  description = "Storage class configurations"
  type = map(object({
    provisioner           = string
    type                  = string
    volume_binding_mode   = string
    allow_volume_expansion = bool
    reclaim_policy        = string
    parameters            = map(string)
  }))
  default = {
    ssd = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-ssd"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type                      = "pd-ssd"
        fstype                    = "ext4"
        replication-type          = "none"
      }
    }
    ssd_regional = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-ssd"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type                      = "pd-ssd"
        fstype                    = "ext4"
        replication-type          = "regional-pd"
      }
    }
    balanced = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-balanced"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type                      = "pd-balanced"
        fstype                    = "ext4"
        replication-type          = "none"
      }
    }
    standard = {
      provisioner            = "kubernetes.io/gce-pd"
      type                   = "pd-standard"
      volume_binding_mode    = "WaitForFirstConsumer"
      allow_volume_expansion = true
      reclaim_policy         = "Delete"
      parameters = {
        type                      = "pd-standard"
        fstype                    = "ext4"
        replication-type          = "none"
      }
    }
  }
}

variable "pvcs" {
  description = "Persistent volume claim configurations"
  type = map(object({
    storage_class_name = string
    size               = string
    access_modes       = list(string)
    labels             = map(string)
  }))
  default = {
    mnesia_data = {
      storage_class_name = "ssd-regional"
      size               = "100Gi"
      access_modes       = ["ReadWriteOnce"]
      labels = {
        app       = "cre"
        component = "mnesia"
      }
    }
    mnesia_logs = {
      storage_class_name = "ssd"
      size               = "50Gi"
      access_modes       = ["ReadWriteOnce"
]
      labels = {
        app       = "cre"
        component = "mnesia"
      }
    }
    cre_data = {
      storage_class_name = "balanced"
      size               = "200Gi"
      access_modes       = ["ReadWriteOnce"]
      labels = {
        app       = "cre"
        component = "data"
      }
    }
  }
}

variable "backup_config" {
  description = "Backup configuration for persistent disks"
  type = object({
    enabled        = bool
    schedule       = string
    retention_days = number
  })
  default = {
    enabled        = false
    schedule       = "0 2 * * *"  # Daily at 2 AM
    retention_days = 30
  }
}

variable "labels" {
  description = "Labels to apply to all resources"
  type        = map(string)
  default     = {
    environment = "production"
    managed_by  = "terraform"
    project     = "cre"
  }
}

variable "enable_snapshots" {
  description = "Enable persistent disk snapshots"
  type        = bool
  default     = true
}

variable "snapshot_schedule" {
  description = "Snapshot schedule configuration"
  type = object({
    enabled           = bool
    schedule          = string
    retention_days    = number
    snapshot_location = string
  })
  default = {
    enabled           = false
    schedule          = "0 3 * * *"
    retention_days    = 7
    snapshot_location = "us-central1"
  }
}
