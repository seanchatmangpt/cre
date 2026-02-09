# terraform/gcp/outputs.tf
# Root module outputs for CRE GCP deployment

output "project_id" {
  description = "GCP project ID"
  value       = var.project_id
}

output "region" {
  description = "Primary region"
  value       = var.region
}

# VPC Outputs
output "vpc" {
  description = "VPC configuration"
  value = module.vpc
}

output "vpc_network_name" {
  description = "VPC network name"
  value       = module.vpc.network_name
}

output "vpc_subnets" {
  description = "VPC subnet details"
  value       = module.vpc.subnets
}

# GKE Outputs
output "gke_cluster" {
  description = "GKE cluster details"
  value = {
    name       = module.gke_cluster.cluster_name
    endpoint   = module.gke_cluster.cluster_endpoint
    ca_cert    = module.gke_cluster.cluster_ca_certificate
    region     = module.gke_cluster.cluster_region
    node_pools = module.gke_cluster.node_pool_names
  }
  sensitive = true
}

output "gke_cluster_name" {
  description = "GKE cluster name"
  value       = module.gke_cluster.cluster_name
}

output "gke_cluster_endpoint" {
  description = "GKE cluster endpoint"
  value       = module.gke_cluster.cluster_endpoint
  sensitive   = true
}

output "gke_cluster_ca_certificate" {
  description = "GKE cluster CA certificate"
  value       = module.gke_cluster.cluster_ca_certificate
  sensitive   = true
}

output "gke_node_pools" {
  description = "GKE node pool names"
  value       = module.gke_cluster.node_pool_names
}

# Storage Outputs
output "storage" {
  description = "Storage configuration"
  value = {
    storage_classes = module.storage.storage_classes
    pvcs            = module.storage.pvcs
    snapshot_policy = module.storage.snapshot_policy_name
  }
}

# Load Balancer Outputs
output "load_balancers" {
  description = "Load balancer details"
  value = {
    internal = module.loadbalancer.internal_lb
    external = module.loadbalancer.external_lb
  }
}

output "internal_lb_ip" {
  description = "Internal load balancer IP"
  value       = try(module.loadbalancer.internal_lb.ip_address, null)
}

output "external_lb_ip" {
  description = "External load balancer IP"
  value       = try(module.loadbalancer.external_lb_ip, null)
}

output "external_lb_url" {
  description = "External load balancer URL"
  value       = try(module.loadbalancer.external_lb_url, null)
}

# Combined Outputs
output "cluster_access_command" {
  description = "Command to get cluster credentials"
  value       = "gcloud container clusters regional get-credentials ${module.gke_cluster.cluster_name} --region ${module.gke_cluster.cluster_region} --project ${var.project_id}"
}

output "kubectl_context_set_command" {
  description = "Command to set kubectl context"
  value       = "kubectl config set-context ${var.project_id}_${module.gke_cluster.cluster_name}_${module.gke_cluster.cluster_region}"
}

# Important Notes
output "next_steps" {
  description = "Next steps after deployment"
  value = <<-EOT
    1. Configure kubectl:
       gcloud container clusters regional get-credentials ${module.gke_cluster.cluster_name} --region ${module.gke_cluster.cluster_region}

    2. Apply storage classes:
       kubectl apply -f modules/storage/storage-classes.yaml.tpl

    3. Apply PVCs:
       kubectl apply -f modules/storage/pvcs.yaml.tpl

    4. Deploy CRE application
  EOT
}
