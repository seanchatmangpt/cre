# terraform/gcp/modules/gke_cluster/outputs.tf
# Output specifications for GKE cluster module

output "cluster_name" {
  description = "GKE cluster name"
  value       = google_container_cluster.primary.name
}

output "cluster_endpoint" {
  description = "GKE cluster API endpoint"
  value       = google_container_cluster.primary.endpoint
  sensitive   = true
}

output "cluster_ca_certificate" {
  description = "GKE cluster CA certificate"
  value       = google_container_cluster.primary.master_auth[0].cluster_ca_certificate
  sensitive   = true
}

output "cluster_region" {
  description = "GKE cluster region"
  value       = var.region
}

output "cluster_id" {
  description = "GKE cluster ID"
  value       = google_container_cluster.primary.id
}

output "location" {
  description = "GKE cluster location"
  value       = google_container_cluster.primary.location
}

output "node_pool_names" {
  description = "List of node pool names"
  value = compact([
    contains(keys(var.node_pools), "general") ? google_container_node_pool.general[0].name : null,
    contains(keys(var.node_pools), "memory_optimized") ? google_container_node_pool.memory_optimized[0].name : null
  ])
}

output "master_ipv4_cidr_block" {
  description = "CIDR block for GKE master access"
  value       = var.master_ipv4_cidr_block
}

output "private_endpoint" {
  description = "Private cluster endpoint status"
  value       = var.private_cluster_config.enable_private_endpoint
}

output "network_name" {
  description = "VPC network name"
  value       = var.network_name
}

output "subnet_name" {
  description = "Subnet name"
  value       = var.subnet_name
}
