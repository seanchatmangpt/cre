variable "project_id" {
  description = "GCP Project ID"
  type        = string
  nullable    = false
}

# AWS Configuration (for hybrid deployments)
variable "aws_region" {
  description = "AWS region for EKS cluster and resources"
  type        = string
  default     = "us-east-1"
}

variable "region" {
  description = "GCP region for resources"
  type        = string
  default     = "us-central1"

  validation {
    condition     = can(regex("^[a-z]+-[a-z]+[0-9]$", var.region))
    error_message = "Region must be a valid GCP region format (e.g., us-central1)."
  }
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "dev"

  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be one of: dev, staging, prod."
  }
}

variable "labels" {
  description = "Labels to apply to all resources"
  type        = map(string)
  default = {
    project     = "cre"
    managed-by  = "terraform"
  }
}

# Network Configuration
variable "primary_subnet_cidr" {
  description = "CIDR range for primary subnet"
  type        = string
  default     = "10.0.0.0/20"
}

variable "secondary_subnet_cidr" {
  description = "CIDR range for secondary subnet"
  type        = string
  default     = "10.0.16.0/20"
}

variable "ssh_source_ranges" {
  description = "List of source IP ranges allowed for SSH"
  type        = list(string)
  default     = ["0.0.0.0/0"]
}

# GKE Cluster Configuration
variable "cluster_name" {
  description = "Name of the GKE cluster"
  type        = string
  default     = "cre-cluster"
}

variable "subnet_cidr" {
  description = "CIDR range for GKE subnet"
  type        = string
  default     = "10.1.0.0/20"
}

variable "pods_cidr" {
  description = "CIDR range for Kubernetes pods"
  type        = string
  default     = "172.16.0.0/14"
}

variable "services_cidr" {
  description = "CIDR range for Kubernetes services"
  type        = string
  default     = "172.20.0.0/14"
}

variable "deletion_protection" {
  description = "Enable deletion protection for the GKE cluster"
  type        = bool
  default     = false
}

variable "disable_cloud_run" {
  description = "Disable Cloud Run addon on GKE cluster"
  type        = bool
  default     = true
}

# Cluster Autoscaling
variable "cluster_autoscaling_min_cpu" {
  description = "Minimum CPU for cluster autoscaling"
  type        = number
  default     = 1
}

variable "cluster_autoscaling_max_cpu" {
  description = "Maximum CPU for cluster autoscaling"
  type        = number
  default     = 64
}

variable "cluster_autoscaling_min_memory" {
  description = "Minimum memory (GB) for cluster autoscaling"
  type        = number
  default     = 1
}

variable "cluster_autoscaling_max_memory" {
  description = "Maximum memory (GB) for cluster autoscaling"
  type        = number
  default     = 256
}

# Primary Node Pool
variable "primary_node_count" {
  description = "Initial number of nodes in primary node pool"
  type        = number
  default     = 3
}

variable "primary_min_nodes" {
  description = "Minimum number of nodes in primary node pool"
  type        = number
  default     = 1
}

variable "primary_max_nodes" {
  description = "Maximum number of nodes in primary node pool"
  type        = number
  default     = 10
}

variable "primary_machine_type" {
  description = "Machine type for primary node pool"
  type        = string
  default     = "n1-standard-2"
}

variable "primary_disk_size_gb" {
  description = "Disk size in GB for primary node pool"
  type        = number
  default     = 50
}

variable "primary_preemptible" {
  description = "Use preemptible VMs in primary node pool"
  type        = bool
  default     = false
}

# Compute Node Pool
variable "compute_node_count" {
  description = "Initial number of nodes in compute node pool"
  type        = number
  default     = 2
}

variable "compute_min_nodes" {
  description = "Minimum number of nodes in compute node pool"
  type        = number
  default     = 1
}

variable "compute_max_nodes" {
  description = "Maximum number of nodes in compute node pool"
  type        = number
  default     = 20
}

variable "compute_machine_type" {
  description = "Machine type for compute node pool"
  type        = string
  default     = "n1-highmem-4"
}

variable "compute_disk_size_gb" {
  description = "Disk size in GB for compute node pool"
  type        = number
  default     = 100
}

variable "compute_preemptible" {
  description = "Use preemptible VMs in compute node pool"
  type        = bool
  default     = true
}

# Kubernetes Namespaces
variable "gke_namespace" {
  description = "Kubernetes namespace for GKE workloads"
  type        = string
  default     = "default"
}

variable "cloud_run_namespace" {
  description = "Kubernetes namespace for Cloud Run workloads"
  type        = string
  default     = "cloud-run"
}

# IAM Configuration
variable "workload_identity_location" {
  description = "Location for workload identity pool (global or region)"
  type        = string
  default     = "global"
}

# Secrets Manager
variable "database_password" {
  description = "Database password to store in Secret Manager"
  type        = string
  sensitive   = true
}

variable "api_key" {
  description = "API key to store in Secret Manager"
  type        = string
  sensitive   = true
}

variable "jwt_secret" {
  description = "JWT secret to store in Secret Manager"
  type        = string
  sensitive   = true
}

variable "oauth_client_secret" {
  description = "OAuth client secret to store in Secret Manager"
  type        = string
  sensitive   = true
}

variable "service_account_email" {
  description = "Service account email for Secret Manager access"
  type        = string
}

# Monitoring
variable "notification_channel_email" {
  description = "Email address for alert notifications"
  type        = string
}

variable "log_sink_dataset" {
  description = "BigQuery dataset name for log sink"
  type        = string
  default     = "monitoring_logs"
}

# Storage Configuration
variable "buckets" {
  description = "Map of GCS bucket configurations"
  type = map(object({
    location           = string
    storage_class      = string
    versioning         = optional(bool, false)
    lifecycle_rules    = optional(list(object({
      age_days           = optional(number)
      num_newer_versions = optional(number)
      action             = string
    })), [])
    iam_bindings = optional(map(list(string)), {})
    labels       = optional(map(string), {})
  }))
  default  = {}
  nullable = false
}
