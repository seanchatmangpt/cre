# terraform/gcp/modules/loadbalancer/variables.tf
# Variable specifications for load balancer module

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "Region for load balancer resources"
  type        = string
  default     = "us-central1"
}

variable "cluster_name" {
  description = "Name of the GKE cluster"
  type        = string
  default     = "cre-cluster"
}

variable "network_name" {
  description = "Name of the VPC network"
  type        = string
}

variable "subnetwork_name" {
  description = "Name of the subnetwork"
  type        = string
}

variable "internal_lb_config" {
  description = "Internal load balancer configuration"
  type = object({
    enabled           = bool
    name              = string
    ip_address        = string
    ports             = list(number)
    health_check_path = string
    backend_protocol  = string
  })
  default = {
    enabled           = true
    name              = "cre-internal-lb"
    ip_address        = "10.0.1.100"
    ports             = [8080, 9100, 4369]
    health_check_path = "/health"
    backend_protocol  = "TCP"
  }
}

variable "external_lb_config" {
  description = "External load balancer configuration"
  type = object({
    enabled           = bool
    name              = string
    ports             = map(object({
      port     = number
      target   = number
      protocol = string
    }))
    health_check_path = string
    ssl_enabled       = bool
    ssl_certificates  = list(string)
  })
  default = {
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
    ssl_certificates  = []
  }
}

variable "neg_config" {
  description = "Network Endpoint Group configuration"
  type = object({
    enabled          = bool
    cloud_run_enabled = bool
    gke_enabled      = bool
  })
  default = {
    enabled          = true
    cloud_run_enabled = false
    gke_enabled      = true
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

variable "enable_cloud_armor" {
  description = "Enable Cloud Armor for external LB"
  type        = bool
  default     = true
}

variable "cdn_config" {
  description = "Cloud CDN configuration"
  type = object({
    enabled            = bool
    cache_policy       = string
    custom_response_headers = map(string)
  })
  default = {
    enabled  = false
    cache_policy = "CACHE_ALL_STATIC"
    custom_response_headers = {}
  }
}
