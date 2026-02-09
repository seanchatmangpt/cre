# terraform/gcp/modules/gke_cluster/main.tf
# GKE regional cluster with private nodes and multiple node pools

locals {
  # Regional cluster endpoint
  cluster_endpoint = "https://${google_container_cluster.primary.endpoint}"
}

# Regional GKE Cluster
resource "google_container_cluster" "primary" {
  name               = var.cluster_name
  location           = var.region
  project            = var.project_id
  network            = var.network_name
  subnetwork         = var.subnet_name

  # Private cluster configuration
  private_cluster_config {
    enable_private_endpoint    = var.private_cluster_config.enable_private_endpoint
    enable_private_nodes       = var.private_cluster_config.enable_private_nodes
    master_ipv4_cidr_block     = var.master_ipv4_cidr_block
  }

  # Release channel for automatic upgrades
  release_channel {
    channel = var.release_channel
  }

  # IP allocation policy for pod and service CIDRs
  ip_allocation_policy {
    cluster_secondary_range_name  = "pods"
    services_secondary_range_name = "services"
  }

  # Master authorized networks (when not using private endpoint exclusively)
  dynamic "master_authorized_networks_config" {
    for_each = length(var.private_cluster_config.master_authorized_networks) > 0 ? [1] : []
    content {
      gcp_public_cidrs_access_enabled = false
      dynamic "cidr_blocks" {
        for_each = var.private_cluster_config.master_authorized_networks
        content {
          cidr_block   = cidr_blocks.value
          display_name = "authorized-network-${cidr_blocks.key}"
        }
      }
    }
  }

  # Logging configuration
  logging_config {
    enable_components = var.logging_config.enable_components
  }

  # Monitoring configuration with managed Prometheus
  monitoring_config {
    enable_components = var.monitoring_config.enable_components
    managed_prometheus {
      enabled = var.monitoring_config.managed_prometheus
    }
  }

  # Security and networking
  enable_shielded_nodes       = true
  enable_intranode_visibility = true
  enable_l4_ilb_subsetting    = true
  datapath_provider           = "ADVANCED_DATAPATH"

  # Workload identity configuration
  workload_identity_config {
    workload_pool = "${var.project_id}.svc.id.goog"
  }

  # Maintenance window
  maintenance_policy {
    recurring_window {
      start_time = "2024-01-01T00:00:00Z"
      end_time   = "2024-01-01T04:00:00Z"
      recurrence = "Every Saturday"
    }
  }

  # Resource labels
  resource_labels = var.labels

  # Remove default node pool
  remove_default_node_pool = true

  # Initial node count (will be removed)
  initial_node_count       = 1

  # Authentication and authorization
  master_auth {
    client_certificate_config {
      issue_client_certificate = false
    }
  }

  # Binary authorization
  binary_authorization {
    evaluation_mode = "PROJECT_SINGLETON_POLICY_ENFORCE"
  }

  # Network policy
  network_policy {
    enabled  = true
    provider = "CALICO"
  }

  # Timeout and lifecycle
  timeouts {
    create = "45m"
    update = "60m"
    delete = "45m"
  }

  lifecycle {
    ignore_changes = [
      initial_node_count,
      node_config[0].taint
    ]
  }
}

# General purpose node pool
resource "google_container_node_pool" "general" {
  count     = contains(keys(var.node_pools), "general") ? 1 : 0
  name      = "general"
  project   = var.project_id
  location  = var.region
  cluster   = google_container_cluster.primary.name

  node_count = var.node_pools["general"].node_count

  management {
    auto_repair  = var.node_pools["general"].auto_repair
    auto_upgrade = var.node_pools["general"].auto_upgrade
  }

  node_config {
    machine_type = var.node_pools["general"].machine_type
    disk_size_gb = var.node_pools["general"].disk_size_gb
    disk_type    = var.node_pools["general"].disk_type

    labels = merge(var.labels, {
      node_pool = "general"
    })

    taint {
      key    = "workload"
      value  = "general"
      effect = "NO_SCHEDULE"
    }

    # Enable shielded nodes
    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }

    # Spot instance configuration
    spot = var.node_pools["general"].spot

    preemptible  = var.node_pools["general"].preemptible

    # OAuth scopes
    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    # Metadata
    metadata = {
      disable-legacy-endpoints = "true"
    }
  }

  autoscaling {
    min_node_count = var.node_pools["general"].min_count
    max_node_count = var.node_pools["general"].max_count
  }

  upgrade_settings {
    max_surge       = 1
    max_unavailable = 0
  }

  lifecycle {
    ignore_changes = [node_count]
  }
}

# Memory optimized node pool for Mnesia
resource "google_container_node_pool" "memory_optimized" {
  count     = contains(keys(var.node_pools), "memory_optimized") ? 1 : 0
  name      = "memory-optimized"
  project   = var.project_id
  location  = var.region
  cluster   = google_container_cluster.primary.name

  node_count = var.node_pools["memory_optimized"].node_count

  management {
    auto_repair  = var.node_pools["memory_optimized"].auto_repair
    auto_upgrade = var.node_pools["memory_optimized"].auto_upgrade
  }

  node_config {
    machine_type = var.node_pools["memory_optimized"].machine_type
    disk_size_gb = var.node_pools["memory_optimized"].disk_size_gb
    disk_type    = var.node_pools["memory_optimized"].disk_type

    labels = merge(var.labels, {
      node_pool = "memory-optimized"
    })

    taint {
      key    = "workload"
      value  = "mnesia"
      effect = "NO_SCHEDULE"
    }

    # Local SSDs for Mnesia performance
    local_ssd_count = 0

    # Enable shielded nodes
    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }

    # Spot instance configuration
    spot = var.node_pools["memory_optimized"].spot

    preemptible  = var.node_pools["memory_optimized"].preemptible

    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    metadata = {
      disable-legacy-endpoints = "true"
    }
  }

  autoscaling {
    min_node_count = var.node_pools["memory_optimized"].min_count
    max_node_count = var.node_pools["memory_optimized"].max_count
  }

  upgrade_settings {
    max_surge       = 1
    max_unavailable = 0
  }

  lifecycle {
    ignore_changes = [node_count]
  }
}
