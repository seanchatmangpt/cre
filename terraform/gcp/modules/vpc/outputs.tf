# terraform/gcp/modules/vpc/outputs.tf
# Output specifications for VPC module

output "network_name" {
  description = "VPC network name"
  value       = google_compute_network.primary.name
}

output "network_id" {
  description = "VPC network ID"
  value       = google_compute_network.primary.id
}

output "subnets" {
  description = "Subnet details"
  value = {
    for k, v in google_compute_subnetwork.primary : k => {
      name            = v.name
      id              = v.id
      cidr            = v.ip_cidr_range
      region          = v.region
      gateway_ip      = v.gateway_address
      secondary_ranges = v.secondary_ip_range
    }
  }
}

output "subnet_names" {
  description = "List of subnet names"
  value       = [for s in google_compute_subnetwork.primary : s.name]
}

output "subnet_ids" {
  description = "List of subnet IDs"
  value       = [for s in google_compute_subnetwork.primary : s.id]
}

output "router_name" {
  description = "Cloud Router name"
  value       = var.cloud_nat_enabled ? google_compute_router.primary[0].name : null
}

output "nat_name" {
  description = "Cloud NAT name"
  value       = var.cloud_nat_enabled ? google_compute_router_nat.primary[0].name : null
}

output "firewall_rules" {
  description = "Firewall rule names"
  value = {
    erlang_epmd_internal            = google_compute_firewall.erlang_epmd_internal.name
    erlang_distribution_internal    = google_compute_firewall.erlang_distribution_internal.name
    erlang_distribution_udp_internal = google_compute_firewall.erlang_distribution_udp_internal.name
    mnesia_replication_internal     = google_compute_firewall.mnesia_replication_internal.name
    gke_egress                      = google_compute_firewall.gke_egress.name
    iap_ssh                         = google_compute_firewall.iap_ssh.name
    health_checks                   = google_compute_firewall.health_checks.name
    deny_all_ingress                = google_compute_firewall.deny_all_ingress.name
    allow_all_egress                = google_compute_firewall.allow_all_egress.name
  }
}

output "erlang_ports" {
  description = "Erlang distribution ports used in firewall rules"
  value = {
    epmd_port        = 4369
    distribution_tcp = "9100-9200"
    distribution_udp = "9100-9200"
    mnesia_extra     = "11500-11520"
  }
}

output "vpc_cidr" {
  description = "VPC CIDR block"
  value       = var.vpc_cidr
}
