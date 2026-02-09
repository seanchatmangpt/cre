# terraform/gcp/variables.tf
# Root module variables for CRE GCP deployment

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "Primary region for resources"
  type        = string
  default     = "us-central1"
}

variable "zone" {
  description = "Primary zone for zonal resources"
  type        = string
  default     = "us-central1-a"
}

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  default     = "production"
  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production."
  }
}

variable "credentials_file" {
  description = "Path to GCP service account credentials file"
  type        = string
  default     = ""
}

# VPC Configuration
variable "vpc_config" {
  description = "VPC configuration"
  type = object({
    name        = string
    cidr        = string
    enable_nat  = bool
    subnets = map(object({
      cidr                           = string
      availability_zones             = list(string)
      enable_flow_logs              = bool
      enable_private_ip_google_access = bool
    }))
  })
  default = {
    name       = "cre-vpc"
    cidr       = "10.0.0.0/16"
    enable_nat = true
    subnets = {
      primary = {
        cidr                        = "10.0.1.0/24"
        availability_zones          = ["us-central1-a", "us-central1-b", "us-central1-c"]
        enable_flow_logs           = true
        enable_private_ip_google_access = true
      }
    }
  }
}

# GKE Cluster Configuration
variable "gke_config" {
  description = "GKE cluster configuration"
  type = object({
    cluster_name = string
    release_channel = string
    master_ipv4_cidr_block = string
    private_cluster = object({
      enable_private_endpoint  = bool
      enable_private_nodes     = bool
      master_global_access     = bool
      master_authorized_networks = list(string)
    })
    node_pools = map(object({
      machine_type    = string
      node_count      = number
      min_count       = number
      max_count       = number
      disk_size_gb    = number
      disk_type       = string
      auto_repair     = bool
      auto_upgrade    = bool
      spot           = bool
      preemptible    = bool
      max_pods_per_node = number
    }))
  })
  default = {
    cluster_name = "cre-cluster"
    release_channel = "STABLE"
    master_ipv4_cidr_block = "172.16.0.0/28"
    private_cluster = {
      enable_private_endpoint  = true
      enable_private_nodes     = true
      master_global_access     = false
      master_authorized_networks = []
    }
    node_pools = {
      general = {
        machine_type      = "e2-medium"
        node_count        = 3
        min_count         = 1
        max_count         = 10
        disk_size_gb      = 100
        disk_type         = "pd-standard"
        auto_repair       = true
        auto_upgrade      = true
        spot             = false
        preemptible      = false
        max_pods_per_node = 110
      }
      memory_optimized = {
        machine_type      = "e2-highmem-4"
        node_count        = 2
        min_count         = 1
        max_count         = 5
        disk_size_gb      = 200
        disk_type         = "pd-ssd"
        auto_repair       = true
        auto_upgrade      = true
        spot             = false
        preemptible      = false
        max_pods_per_node = 110
      }
    }
  }
}

# Storage Configuration
variable "storage_config" {
  description = "Storage configuration"
  type = object({
    enable_snapshots = bool
    backup_enabled   = bool
    backup_schedule  = string
    retention_days   = number
  })
  default = {
    enable_snapshots = true
    backup_enabled   = false
    backup_schedule  = "0 2 * * *"
    retention_days   = 30
  }
}

# Backup Configuration
variable "backup_config" {
  description = "Backup infrastructure configuration"
  type = object({
    replication_region = string
    enable_cmek        = bool
    create_spanner     = bool
    create_filestore   = bool
    enable_alerting    = bool
    alert_email        = string
  })
  default = {
    replication_region = "us-east1"
    enable_cmek        = false
    create_spanner     = true
    create_filestore   = false
    enable_alerting    = true
    alert_email        = ""
  }
}

# Load Balancer Configuration
variable "lb_config" {
  description = "Load balancer configuration"
  type = object({
    internal = object({
      enabled           = bool
      name              = string
      ip_address        = string
      ports             = list(number)
      health_check_path = string
    })
    external = object({
      enabled           = bool
      name              = string
      ports             = map(object({
        port     = number
        target   = number
        protocol = string
      }))
      health_check_path = string
      ssl_enabled       = bool
    })
    enable_cloud_armor = bool
    enable_cdn         = bool
  })
  default = {
    internal = {
      enabled           = true
      name              = "cre-internal-lb"
      ip_address        = "10.0.1.100"
      ports             = [8080, 9100, 4369]
      health_check_path = "/health"
    }
    external = {
      enabled           = true
      name              = "cre-external-lb"
      ports = {
        http = {
          port     = 80
          target   = 8080
          protocol = "HTTP"
        }
        https = {
          port     = 443
          target   = 8080
          protocol = "HTTPS"
        }
      }
      health_check_path = "/health"
      ssl_enabled       = false
    }
    enable_cloud_armor = true
    enable_cdn         = false
  }
}

# Labels
variable "labels" {
  description = "Common labels for all resources"
  type        = map(string)
  default     = {
    environment = "production"
    managed_by  = "terraform"
    project     = "cre"
  }
}
