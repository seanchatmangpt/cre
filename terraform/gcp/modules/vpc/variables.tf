# terraform/gcp/modules/vpc/variables.tf
# Variable specifications for VPC module

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "Region for VPC resources"
  type        = string
  default     = "us-central1"
}

variable "network_name" {
  description = "Name of the VPC network"
  type        = string
  default     = "cre-vpc"
}

variable "vpc_cidr" {
  description = "CIDR block for the VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "subnets" {
  description = "Subnet configurations"
  type = map(object({
    cidr                            = string
    availability_zones              = list(string)
    enable_flow_logs                = bool
    enable_private_ip_google_access = bool
  }))
  default = {
    primary = {
      cidr                            = "10.0.1.0/24"
      availability_zones              = ["us-central1-a", "us-central1-b", "us-central1-c"]
      enable_flow_logs                = true
      enable_private_ip_google_access = true
    }
  }
}

variable "secondary_ranges" {
  description = "Secondary IP ranges for GKE pods and services"
  type = map(list(object({
    range_name    = string
    ip_cidr_range = string
  })))
  default = {
    primary = [
      {
        range_name    = "pods"
        ip_cidr_range = "10.1.0.0/16"
      },
      {
        range_name    = "services"
        ip_cidr_range = "10.2.0.0/16"
      }
    ]
  }
}

variable "cloud_nat_enabled" {
  description = "Enable Cloud NAT for private nodes"
  type        = bool
  default     = true
}

variable "enable_shared_vpc_host" {
  description = "Enable VPC as Shared VPC host"
  type        = bool
  default     = false
}

variable "firewall_rules" {
  description = "Custom firewall rules"
  type = map(object({
    description             = string
    direction               = string
    priority                = number
    ranges                  = list(string)
    allow_rules             = list(string)
    deny_rules              = list(string)
    source_tags             = list(string)
    target_tags             = list(string)
    source_service_accounts = list(string)
    target_service_accounts = list(string)
  }))
  default = {}
}

variable "labels" {
  description = "Labels to apply to all resources"
  type        = map(string)
  default = {
    environment = "production"
    managed_by  = "terraform"
    project     = "cre"
  }
}

variable "router_name" {
  description = "Name of the Cloud Router"
  type        = string
  default     = "cre-router"
}

variable "nat_name" {
  description = "Name of the Cloud NAT"
  type        = string
  default     = "cre-nat"
}

# =============================================================================
# Firewall Configuration Variables
# =============================================================================

variable "master_ipv4_cidr_block" {
  description = "CIDR block for GKE master authorized network access"
  type        = string
  default     = "172.16.0.0/28"
}

variable "cre_node_service_accounts" {
  description = "Service accounts for CRE nodes (for service account targeting)"
  type        = list(string)
  default     = []
}

variable "cre_api_authorized_networks" {
  description = "Authorized networks for CRE API access (VPN, bastion, corporate CIDRs)"
  type        = list(string)
  default = [
    "10.0.0.0/8",     # Private networks
    "172.16.0.0/12",  # Private networks
    "192.168.0.0/16", # Private networks
  ]
}

variable "gcp_health_check_ranges" {
  description = "GCP health check IP ranges"
  type        = list(string)
  default = [
    "130.211.0.0/22",  # US health check ranges
    "35.191.0.0/16",   # Health check ranges
    "209.85.152.0/22", # Health check ranges
    "209.85.204.0/22", # Health check ranges
  ]
}

variable "gcp_lb_ranges" {
  description = "GCP load balancer IP ranges"
  type        = list(string)
  default = [
    "35.191.0.0/16",  # LB ranges
    "130.211.0.0/22", # LB ranges
  ]
}

variable "proxy_subnet_ranges" {
  description = "Proxy subnet ranges for internal load balancer (ILB)"
  type        = list(string)
  default = [
    "10.0.128.0/24", # Example proxy subnet (configure based on your VPC)
  ]
}

variable "pod_ip_cidr_range" {
  description = "CIDR range for GKE pods (intra-node visibility source)"
  type        = string
  default     = "10.1.0.0/16"
}

variable "dns_servers" {
  description = "DNS server IPs for egress rules"
  type        = list(string)
  default = [
    "169.254.169.254", # Metadata server (internal DNS resolver)
    "8.8.8.8",         # Google DNS (fallback)
    "8.8.4.4",         # Google DNS (fallback)
  ]
}

variable "enable_cloud_sql_egress" {
  description = "Enable Cloud SQL egress rules"
  type        = bool
  default     = false
}

variable "cloud_sql_private_ranges" {
  description = "Private IP ranges for Cloud SQL instances"
  type        = list(string)
  default     = []
}

variable "cloud_sql_ports" {
  description = "Cloud SQL database ports"
  type        = list(string)
  default     = ["3306", "5432"] # MySQL, PostgreSQL
}

variable "private_google_access_exceptions" {
  description = "Exceptions for private Google access (ranges that bypass direct internet deny)"
  type        = list(string)
  default = [
    "199.36.153.8/30", # Restricted Google API
    "199.36.153.4/30", # Private Google Access
  ]
}

variable "cre_metrics_ports" {
  description = "Ports for CRE metrics/monitoring scraping"
  type        = list(string)
  default     = ["9090", "9100", "9101"] # Prometheus, node exporter, custom
}
