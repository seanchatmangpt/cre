# terraform/gcp/modules/loadbalancer/outputs.tf
# Output specifications for load balancer module

output "internal_lb" {
  description = "Internal load balancer details"
  value = var.internal_lb_config.enabled ? {
    name         = var.internal_lb_config.name
    ip_address   = google_compute_address.internal_lb[0].address
    ports        = var.internal_lb_config.ports
    backend_id   = google_compute_region_backend_service.internal[0].id
    health_check = google_compute_region_health_check.internal[0].id
  } : null
}

output "external_lb" {
  description = "External load balancer details"
  value = var.external_lb_config.enabled ? {
    name       = var.external_lb_config.name
    ip_address = google_compute_global_address.external_lb[0].address
    url_map    = google_compute_url_map.external[0].name
    backend_id = google_compute_backend_service.external[0].id
  } : null
}

output "external_lb_ip" {
  description = "External load balancer IP address"
  value       = var.external_lb_config.enabled ? google_compute_global_address.external_lb[0].address : null
}

output "external_lb_url" {
  description = "External load balancer URL"
  value       = var.external_lb_config.enabled ? "http://${google_compute_global_address.external_lb[0].address}" : null
}

output "security_policy" {
  description = "Cloud Armor security policy"
  value = var.external_lb_config.enabled && var.enable_cloud_armor ? {
    name = google_compute_security_policy.cloud_armor[0].name
    id   = google_compute_security_policy.cloud_armor[0].id
  } : null
}

output "forwarding_rules" {
  description = "List of forwarding rule names"
  value = concat(
    [for r in google_compute_forwarding_rule.internal : r.name],
    var.external_lb_config.enabled && contains(keys(var.external_lb_config.ports), "http") ? [google_compute_global_forwarding_rule.http[0].name] : [],
    var.external_lb_config.enabled && var.external_lb_config.ssl_enabled ? [for r in google_compute_global_forwarding_rule.https : r.name] : []
  )
}

output "health_checks" {
  description = "Health check names"
  value = {
    internal = var.internal_lb_config.enabled ? google_compute_region_health_check.internal[0].name : null
    external = var.external_lb_config.enabled ? google_compute_health_check.external[0].name : null
  }
}
