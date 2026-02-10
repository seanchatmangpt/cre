# terraform/gcp/main.tf
# Root module for CRE GCP deployment

# ============================================
# VPC Module
# ============================================
module "vpc" {
  source = "./modules/vpc"

  project_id = var.project_id
  region     = var.region

  network_name      = var.vpc_config.name
  vpc_cidr          = var.vpc_config.cidr
  cloud_nat_enabled = var.vpc_config.enable_nat

  subnets = var.vpc_config.subnets

  secondary_ranges = {
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

  enable_shared_vpc_host = false

  firewall_rules = {}

  labels = merge(var.labels, {
    module = "vpc"
  })
}

# ============================================
# GKE Cluster Module
# ============================================
module "gke_cluster" {
  source = "./modules/gke_cluster"

  project_id = var.project_id
  region     = var.region

  cluster_name = var.gke_config.cluster_name
  network_name = module.vpc.network_name
  subnet_name  = "primary" # Matches the subnet key in VPC module

  master_ipv4_cidr_block = var.gke_config.master_ipv4_cidr_block

  private_cluster_config = var.gke_config.private_cluster

  release_channel = var.gke_config.release_channel

  node_pools = var.gke_config.node_pools

  logging_config = {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]
  }

  monitoring_config = {
    enable_components  = ["SYSTEM_COMPONENTS"]
    managed_prometheus = true
  }

  labels = merge(var.labels, {
    module = "gke"
  })

  depends_on = [
    module.vpc
  ]
}

# ============================================
# Storage Module
# ============================================
module "storage" {
  source = "./modules/storage"

  project_id   = var.project_id
  region       = var.region
  cluster_name = var.gke_config.cluster_name

  enable_snapshots = var.storage_config.enable_snapshots
  snapshot_schedule = {
    enabled           = var.storage_config.enable_snapshots
    schedule          = var.storage_config.backup_schedule
    retention_days    = var.storage_config.retention_days
    snapshot_location = var.region
  }

  backup_config = {
    enabled        = var.storage_config.backup_enabled
    schedule       = var.storage_config.backup_schedule
    retention_days = var.storage_config.retention_days
  }

  labels = merge(var.labels, {
    module = "storage"
  })
}

# ============================================
# Load Balancer Module
# ============================================
module "loadbalancer" {
  source = "./modules/loadbalancer"

  project_id   = var.project_id
  region       = var.region
  cluster_name = var.gke_config.cluster_name

  network_name    = module.vpc.network_name
  subnetwork_name = "primary" # Matches the subnet key

  internal_lb_config = {
    enabled           = var.lb_config.internal.enabled
    name              = var.lb_config.internal.name
    ip_address        = var.lb_config.internal.ip_address
    ports             = var.lb_config.internal.ports
    health_check_path = var.lb_config.internal.health_check_path
    backend_protocol  = "TCP"
  }

  external_lb_config = {
    enabled           = var.lb_config.external.enabled
    name              = var.lb_config.external.name
    ports             = var.lb_config.external.ports
    health_check_path = var.lb_config.external.health_check_path
    ssl_enabled       = var.lb_config.external.ssl_enabled
    ssl_certificates  = []
  }

  enable_cloud_armor = var.lb_config.enable_cloud_armor
  cdn_config = {
    enabled                 = var.lb_config.enable_cdn
    cache_policy            = "CACHE_ALL_STATIC"
    custom_response_headers = {}
  }

  labels = merge(var.labels, {
    module = "loadbalancer"
  })

  depends_on = [
    module.vpc,
    module.gke_cluster
  ]
}

# ============================================
# Backup Module
# ============================================
module "backup" {
  source = "./modules/backup"

  project_id           = var.project_id
  backup_location      = var.region
  replication_location = var.backup_config.replication_region

  labels = merge(var.labels, {
    module = "backup"
  })
}

# ============================================
# Security Module (GCP Marketplace Compliance)
# ============================================
# CRITICAL: This module creates explicit service accounts with minimal IAM
# GCP Marketplace requires no default service account usage
module "security" {
  source = "./modules/security"

  project_id   = var.project_id
  region       = var.region
  cluster_name = var.gke_config.cluster_name

  name_prefix                = "cre"
  gke_namespace              = "cre-prod"
  kubernetes_service_account = "cre-ksa"

  # GitHub Actions Workload Identity
  github_pool_id    = "github-actions-pool"
  github_repository = var.github_repository

  # GKE Workload Identity
  gke_cluster_issuer_uri  = ""
  gke_allowed_audiences   = []
  gke_attribute_condition = ""

  # Admin access (optional)
  enable_admin_impersonation = false
  admin_impersonator_email   = ""
}
