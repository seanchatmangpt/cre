# terraform/gcp/modules/vpc/main.tf
# VPC, subnets, NAT, and firewall rules for CRE deployment

# VPC Network
resource "google_compute_network" "primary" {
  name                            = var.network_name
  project                         = var.project_id
  auto_create_subnetworks         = false
  routing_mode                    = "REGIONAL"
  mtu                             = 1460
  delete_default_routes_on_create = false

  depends_on = [
    google_compute_shared_vpc_host_project.shared_vpc_host
  ]

  lifecycle {
    create_before_destroy = false
  }
}

# Shared VPC Host Project (optional)
resource "google_compute_shared_vpc_host_project" "shared_vpc_host" {
  count   = var.enable_shared_vpc_host ? 1 : 0
  project = var.project_id
}

# Subnets
resource "google_compute_subnetwork" "primary" {
  for_each = var.subnets

  name                     = each.key
  project                  = var.project_id
  region                   = var.region
  network                  = google_compute_network.primary.id
  ip_cidr_range            = each.value.cidr

  dynamic "secondary_ip_range" {
    for_each = try(var.secondary_ranges[each.key], [])
    content {
      range_name    = secondary_ip_range.value.range_name
      ip_cidr_range = secondary_ip_range.value.ip_cidr_range
    }
  }

  private_ip_google_access = each.value.enable_private_ip_google_access
  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_sampling        = 0.5
    metadata             = "INCLUDE_ALL_METADATA"
  }

  lifecycle {
    create_before_destroy = true
  }
}

# Cloud Router for Cloud NAT
resource "google_compute_router" "primary" {
  count   = var.cloud_nat_enabled ? 1 : 0
  name    = var.router_name
  project = var.project_id
  region  = var.region
  network = google_compute_network.primary.id

  bgp {
    asn = 65000
  }
}

# Cloud NAT
resource "google_compute_router_nat" "primary" {
  count   = var.cloud_nat_enabled ? 1 : 0
  name    = var.nat_name
  project = var.project_id
  router  = google_compute_router.primary[0].name
  region  = var.region

  source_subnetwork_ip_ranges_to_nat = "LIST_OF_SUBNETWORKS"

  dynamic "subnetwork" {
    for_each = google_compute_subnetwork.primary
    content {
      name                    = subnetwork.value.id
      source_ip_ranges_to_nat = ["ALL_IP_RANGES"]
    }
  }

  nat_ip_allocate_option = "AUTO_ONLY"

  # Enable logging for NAT
  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }
}

# Firewall: Erlang Distribution Protocol (EPMD) - Port 4369
resource "google_compute_firewall" "erlang_epmd_internal" {
  name        = "erlang-epmd-internal"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow Erlang EPMD (port 4369) for node discovery"

  direction = "INGRESS"
  priority  = 1000

  source_ranges = [
    "10.0.0.0/8",
    "172.16.0.0/12",
    "192.168.0.0/16"
  ]

  allow {
    protocol = "tcp"
    ports    = ["4369"]
  }

  target_tags = ["erlang-node"]
}

# Firewall: Erlang Distribution - Port Range for Node Communication
resource "google_compute_firewall" "erlang_distribution_internal" {
  name        = "erlang-distribution-internal"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow Erlang distribution port range (9100-9200)"

  direction = "INGRESS"
  priority  = 1000

  source_ranges = [
    "10.0.0.0/8",
    "172.16.0.0/12",
    "192.168.0.0/16"
  ]

  allow {
    protocol = "tcp"
    ports    = ["9100-9200"]
  }

  target_tags = ["erlang-node"]
}

# Firewall: Erlang Distribution UDP
resource "google_compute_firewall" "erlang_distribution_udp_internal" {
  name        = "erlang-distribution-udp-internal"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow Erlang distribution UDP (9100-9200)"

  direction = "INGRESS"
  priority  = 1000

  source_ranges = [
    "10.0.0.0/8",
    "172.16.0.0/12",
    "192.168.0.0/16"
  ]

  allow {
    protocol = "udp"
    ports    = ["9100-9200"]
  }

  target_tags = ["erlang-node"]
}

# Firewall: Mnesia Replication
resource "google_compute_firewall" "mnesia_replication_internal" {
  name        = "mnesia-replication-internal"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow Mnesia database replication traffic"

  direction = "INGRESS"
  priority  = 1000

  source_ranges = [
    "10.0.0.0/8",
    "172.16.0.0/12",
    "192.168.0.0/16"
  ]

  allow {
    protocol = "tcp"
    ports    = ["4369", "9100-9200", "11500-11520"]
  }

  target_tags = ["mnesia-node"]
}

# Firewall: GKE to Cloud APIs (via NAT or Private Access)
resource "google_compute_firewall" "gke_egress" {
  name        = "gke-egress-to-google-apis"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow GKE nodes to access Google APIs"

  direction = "EGRESS"
  priority  = 1000

  destination_ranges = [
    "199.36.153.8/30",   # Restricted Google API
    "199.36.153.4/30"    # Private Google Access
  ]

  allow {
    protocol = "tcp"
    ports    = ["443"]
  }

  target_tags = ["gke-node"]
}

# Firewall: IAP for SSH access
resource "google_compute_firewall" "iap_ssh" {
  name        = "iap-ssh-access"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow IAP for SSH access to GKE nodes"

  direction = "INGRESS"
  priority  = 1000

  source_ranges = ["35.235.240.0/20"]

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  target_tags = ["gke-node"]
}

# Firewall: Health checks
resource "google_compute_firewall" "health_checks" {
  name        = "health-checks"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow GCP health checks"

  direction = "INGRESS"
  priority  = 1000

  source_ranges = ["130.211.0.0/22", "35.191.0.0/16"]

  allow {
    protocol = "tcp"
    ports    = ["80", "443"]
  }

  target_tags = ["load-balancer"]
}

# Default deny all ingress
resource "google_compute_firewall" "deny_all_ingress" {
  name        = "deny-all-ingress"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Deny all ingress traffic (default deny)"

  direction = "INGRESS"
  priority  = 65535

  source_ranges = ["0.0.0.0/0"]

  deny {
    protocol = "tcp"
    ports    = ["0-65535"]
  }

  deny {
    protocol = "udp"
    ports    = ["0-65535"]
  }

  deny {
    protocol = "icmp"
  }
}

# Default allow egress
resource "google_compute_firewall" "allow_all_egress" {
  name        = "allow-all-egress"
  project     = var.project_id
  network     = google_compute_network.primary.id
  description = "Allow all egress traffic"

  direction = "EGRESS"
  priority  = 65535

  destination_ranges = ["0.0.0.0/0"]

  allow {
    protocol = "tcp"
    ports    = ["0-65535"]
  }

  allow {
    protocol = "udp"
    ports    = ["0-65535"]
  }

  allow {
    protocol = "icmp"
  }
}
