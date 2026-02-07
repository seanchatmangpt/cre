terraform {
  required_version = ">= 1.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.0"
    }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

provider "kubernetes" {
  host                   = "https://${google_container_cluster.primary.endpoint}"
  token                  = data.google_client_config.default.access_token
  cluster_ca_certificate = base64decode(google_container_cluster.primary.master_auth[0].cluster_ca_certificate)
}

data "google_client_config" "default" {}

resource "google_compute_network" "vpc" {
  name                    = "${var.cluster_name}-vpc"
  auto_create_subnetworks = false
  routing_mode            = "REGIONAL"
}

resource "google_compute_subnetwork" "subnet" {
  name                     = "${var.cluster_name}-subnet"
  ip_cidr_range            = var.subnet_cidr
  region                   = var.region
  network                  = google_compute_network.vpc.id
  private_ip_google_access = true

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_logs_enabled    = true
    metadata             = "INCLUDE_ALL_METADATA"
  }
}

resource "google_compute_firewall" "allow_internal" {
  name    = "${var.cluster_name}-allow-internal"
  network = google_compute_network.vpc.id

  allow {
    protocol = "tcp"
  }

  allow {
    protocol = "udp"
  }

  allow {
    protocol = "icmp"
  }

  source_ranges = [var.subnet_cidr]
}

resource "google_container_cluster" "primary" {
  name       = var.cluster_name
  location   = var.region
  network    = google_compute_network.vpc.name
  subnetwork = google_compute_subnetwork.subnet.name

  initial_node_count       = 1
  remove_default_node_pool = true

  deletion_protection = var.deletion_protection

  ip_allocation_policy {
    cluster_secondary_range_name  = google_compute_secondary_range.pods.range_name
    services_secondary_range_name = google_compute_secondary_range.services.range_name
  }

  addons_config {
    http_load_balancing {
      disabled = false
    }
    horizontal_pod_autoscaling {
      disabled = false
    }
    network_policy_config {
      disabled = false
    }
    cloudrun_config {
      disabled = var.disable_cloud_run
    }
  }

  network_policy {
    enabled  = true
    provider = "PROVIDER_UNSPECIFIED"
  }

  workload_identity_config {
    workload_pool = "${var.project_id}.svc.id.goog"
  }

  logging_service  = "logging.googleapis.com/kubernetes"
  monitoring_service = "monitoring.googleapis.com/kubernetes"

  cluster_autoscaling {
    enabled = true
    resource_limits {
      resource_type = "cpu"
      minimum       = var.cluster_autoscaling_min_cpu
      maximum       = var.cluster_autoscaling_max_cpu
    }
    resource_limits {
      resource_type = "memory"
      minimum       = var.cluster_autoscaling_min_memory
      maximum       = var.cluster_autoscaling_max_memory
    }
  }

  maintenance_policy {
    daily_maintenance_window {
      start_time = "03:00"
    }
  }

  resource_labels = var.labels
}

resource "google_compute_secondary_range" "pods" {
  name            = "${var.cluster_name}-pods"
  ip_cidr_range   = var.pods_cidr
  region          = var.region
  network_name    = google_compute_network.vpc.name
}

resource "google_compute_secondary_range" "services" {
  name            = "${var.cluster_name}-services"
  ip_cidr_range   = var.services_cidr
  region          = var.region
  network_name    = google_compute_network.vpc.name
}

resource "google_container_node_pool" "primary_nodes" {
  name       = "${var.cluster_name}-primary-pool"
  location   = var.region
  cluster    = google_container_cluster.primary.name
  node_count = var.primary_node_count

  autoscaling {
    min_node_count = var.primary_min_nodes
    max_node_count = var.primary_max_nodes
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  node_config {
    preemptible  = var.primary_preemptible
    machine_type = var.primary_machine_type
    disk_size_gb = var.primary_disk_size_gb
    disk_type    = "pd-standard"

    oauth_scopes = [
      "https://www.googleapis.com/auth/compute",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
      "https://www.googleapis.com/auth/service.management.readonly",
      "https://www.googleapis.com/auth/servicecontrol",
      "https://www.googleapis.com/auth/trace.append",
    ]

    workload_metadata_config {
      mode = "GKE_METADATA"
    }

    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }

    labels = merge(
      var.labels,
      {
        "node-pool" = "primary"
      }
    )

    tags = ["gke-node", var.cluster_name]

    metadata = {
      disable-legacy-endpoints = "true"
    }

    service_account = google_service_account.kubernetes_nodes.email
  }
}

resource "google_container_node_pool" "compute_nodes" {
  name       = "${var.cluster_name}-compute-pool"
  location   = var.region
  cluster    = google_container_cluster.primary.name
  node_count = var.compute_node_count

  autoscaling {
    min_node_count = var.compute_min_nodes
    max_node_count = var.compute_max_nodes
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  node_config {
    preemptible  = var.compute_preemptible
    machine_type = var.compute_machine_type
    disk_size_gb = var.compute_disk_size_gb
    disk_type    = "pd-ssd"

    oauth_scopes = [
      "https://www.googleapis.com/auth/compute",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
      "https://www.googleapis.com/auth/service.management.readonly",
      "https://www.googleapis.com/auth/servicecontrol",
      "https://www.googleapis.com/auth/trace.append",
    ]

    workload_metadata_config {
      mode = "GKE_METADATA"
    }

    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }

    taint {
      key    = "workload-type"
      value  = "compute"
      effect = "NO_SCHEDULE"
    }

    labels = merge(
      var.labels,
      {
        "node-pool" = "compute"
      }
    )

    tags = ["gke-node", var.cluster_name]

    metadata = {
      disable-legacy-endpoints = "true"
    }

    service_account = google_service_account.kubernetes_nodes.email
  }
}

resource "google_service_account" "kubernetes_nodes" {
  account_id   = "${var.cluster_name}-nodes"
  display_name = "GKE nodes service account for ${var.cluster_name}"
}

resource "google_project_iam_member" "kubernetes_nodes_log_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.kubernetes_nodes.email}"
}

resource "google_project_iam_member" "kubernetes_nodes_metric_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.kubernetes_nodes.email}"
}

resource "google_project_iam_member" "kubernetes_nodes_monitoring_viewer" {
  project = var.project_id
  role    = "roles/monitoring.viewer"
  member  = "serviceAccount:${google_service_account.kubernetes_nodes.email}"
}

resource "google_service_account_iam_member" "workload_identity_user" {
  service_account_id = google_service_account.kubernetes_nodes.name
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[default/default]"
}
