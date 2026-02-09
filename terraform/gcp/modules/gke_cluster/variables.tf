# terraform/gcp/modules/gke_cluster/variables.tf
# Variable specifications for GKE cluster module

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "cluster_name" {
  description = "Name of the GKE cluster"
  type        = string
  default     = "cre-cluster"
}

variable "region" {
  description = "Region for the GKE cluster"
  type        = string
  default     = "us-central1"
}

variable "network_name" {
  description = "Name of the VPC network"
  type        = string
}

variable "subnet_name" {
  description = "Name of the subnet"
  type        = string
}

variable "master_ipv4_cidr_block" {
  description = "CIDR block for GKE master access"
  type        = string
  default     = "172.16.0.0/28"
}

variable "private_cluster_config" {
  description = "Private cluster configuration"
  type = object({
    enable_private_endpoint    = bool
    enable_private_nodes       = bool
    master_global_access       = bool
    master_authorized_networks = list(string)
  })
  default = {
    enable_private_endpoint    = true
    enable_private_nodes       = true
    master_global_access       = false
    master_authorized_networks = []
  }
}

variable "release_channel" {
  description = "GKE release channel"
  type        = string
  default     = "STABLE"
  validation {
    condition     = contains(["RAPID", "REGULAR", "STABLE", "UNSUPPORTED"], var.release_channel)
    error_message = "Release channel must be RAPID, REGULAR, STABLE, or UNSUPPORTED."
  }
}

variable "node_pools" {
  description = "Node pool configurations"
  type = map(object({
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
  default = {
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

variable "logging_config" {
  description = "Logging configuration"
  type = object({
    enable_components = list(string)
  })
  default = {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]
  }
}

variable "monitoring_config" {
  description = "Monitoring configuration"
  type = object({
    enable_components = list(string)
    managed_prometheus = bool
  })
  default = {
    enable_components = ["SYSTEM_COMPONENTS"]
    managed_prometheus = true
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
