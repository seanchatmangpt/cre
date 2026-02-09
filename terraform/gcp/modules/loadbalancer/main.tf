# terraform/gcp/modules/loadbalancer/main.tf
# Internal and external load balancers for CRE

# ============================================
# Internal Load Balancer (Regional)
# ============================================

# Reserved IP for internal LB
resource "google_compute_address" "internal_lb" {
  count   = var.internal_lb_config.enabled ? 1 : 0
  name    = "${var.internal_lb_config.name}-ip"
  project = var.project_id
  region  = var.region
  subnetwork = var.subnetwork_name
  address_type = "INTERNAL"
  address      = var.internal_lb_config.ip_address
  purpose      = "GCE_ENDPOINT"
}

# Internal backend service
resource "google_compute_region_backend_service" "internal" {
  count                 = var.internal_lb_config.enabled ? 1 : 0
  name                  = var.internal_lb_config.name
  project               = var.project_id
  region                = var.region
  load_balancing_scheme = "INTERNAL"
  protocol              = var.internal_lb_config.backend_protocol

  health_checks = [google_compute_region_health_check.internal[0].id]

  session_affinity = "NONE"

  timeout_sec = 30

  depends_on = [
    google_compute_region_health_check.internal
  ]
}

# Internal health check
resource "google_compute_region_health_check" "internal" {
  count   = var.internal_lb_config.enabled ? 1 : 0
  name    = "${var.internal_lb_config.name}-hc"
  project = var.project_id
  region  = var.region

  http_health_check {
    port         = var.internal_lb_config.ports[0]
    request_path = var.internal_lb_config.health_check_path
  }

  check_interval_sec  = 5
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3

  log_config {
    enable = true
  }
}

# Internal forwarding rules
resource "google_compute_forwarding_rule" "internal" {
  count                = var.internal_lb_config.enabled ? length(var.internal_lb_config.ports) : 0
  name                 = "${var.internal_lb_config.name}-${var.internal_lb_config.ports[count.index]}-${count.index}"
  project              = var.project_id
  region               = var.region
  network              = var.network_name
  subnetwork           = var.subnetwork_name

  load_balancing_scheme = "INTERNAL"
  ip_protocol           = "TCP"
  port_range            = var.internal_lb_config.ports[count.index]
  allow_global_access   = true

  backend_service = google_compute_region_backend_service.internal[0].id
  labels          = var.labels
}

# ============================================
# External Load Balancer (Global)
# ============================================

# Reserved IP for external LB
resource "google_compute_global_address" "external_lb" {
  count   = var.external_lb_config.enabled ? 1 : 0
  name    = "${var.external_lb_config.name}-ip"
  project = var.project_id

  address_type = "EXTERNAL"

  labels = var.labels
}

# External health check
resource "google_compute_health_check" "external" {
  count   = var.external_lb_config.enabled ? 1 : 0
  name    = "${var.external_lb_config.name}-hc"
  project = var.project_id

  http_health_check {
    port         = contains(keys(var.external_lb_config.ports), "http") ? var.external_lb_config.ports["http"].target : 8080
    request_path = var.external_lb_config.health_check_path
  }

  check_interval_sec  = 5
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3

  log_config {
    enable = true
  }
}

# External backend service
resource "google_compute_backend_service" "external" {
  count                   = var.external_lb_config.enabled ? 1 : 0
  name                    = var.external_lb_config.name
  project                 = var.project_id

  protocol                = "HTTP"
  port_name               = "http"
  load_balancing_scheme   = "EXTERNAL"
  timeout_sec             = 30
  enable_cdn              = var.cdn_config.enabled

  health_checks = [google_compute_health_check.external[0].id]

  session_affinity = "NONE"

  cdn_policy {
    cache_key_policy {
      include_protocol           = true
      include_host               = true
      include_query_string       = true
      include_http_headers      = []
    }

    default_ttl = 3600
    max_ttl     = 86400
    client_ttl  = 3600
  }

  depends_on = [
    google_compute_health_check.external
  ]
}

# Cloud Armor security policy
resource "google_compute_security_policy" "cloud_armor" {
  count       = var.external_lb_config.enabled && var.enable_cloud_armor ? 1 : 0
  name        = "${var.external_lb_config.name}-security"
  project     = var.project_id
  description = "Cloud Armor security policy for CRE"

  # Default: Allow all
  rule {
    action   = "allow"
    priority = 2147483647  # Lowest priority (default rule)
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    description = "Default rule: allow all traffic"
  }

  # Pre-configured WAF rules
  rule {
    action   = "deny"
    priority = 1000
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["0.0.0.0/0"]
      }
      expr {
        expression = "evaluatePreconfiguredExpr('xss-stable')"
      }
    }
    description = "XSS attack detection"
  }

  rule {
    action   = "deny"
    priority = 1001
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["0.0.0.0/0"]
      }
      expr {
        expression = "evaluatePreconfiguredExpr('sqli-stable')"
      }
    }
    description = "SQL injection detection"
  }

  # Rate limiting
  rule {
    action   = "throttle"
    priority = 2000
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    rate_limit_options {
      rate_limit_threshold {
        count        = 100
        interval_sec = 60
      }
      ban_duration_sec = 3600
      enforce_on_key = "IP"
      exceed_action    = "deny(403)"
      conform_action  = "allow"
    }
    description = "Rate limit: 100 requests per minute per IP"
  }
}

# External URL map
resource "google_compute_url_map" "external" {
  count          = var.external_lb_config.enabled ? 1 : 0
  name           = var.external_lb_config.name
  project        = var.project_id

  default_service = google_compute_backend_service.external[0].id

  # Path-based routing
  host_rule {
    hosts        = ["*"]
    path_matcher = "paths"
  }

  path_matcher {
    name            = "paths"
    default_service = google_compute_backend_service.external[0].id

    path_rule {
      paths   = ["/api/*", "/v1/*"]
      route_action {
        url_rewrite {
          path_prefix_rewrite = "/"
        }
      }
    }

    path_rule {
      paths   = ["/health", "/ready", "/alive"]
      route_action {
        # Health checks get default timeout
      }
    }
  }
}

# HTTP target proxy
resource "google_compute_target_http_proxy" "external" {
  count   = var.external_lb_config.enabled ? 1 : 0
  name    = "${var.external_lb_config.name}-http-proxy"
  project = var.project_id

  url_map = google_compute_url_map.external[0].id
}

# HTTPS target proxy (if SSL enabled)
resource "google_compute_target_https_proxy" "external" {
  count   = var.external_lb_config.enabled && var.external_lb_config.ssl_enabled ? 1 : 0
  name    = "${var.external_lb_config.name}-https-proxy"
  project = var.project_id

  url_map        = google_compute_url_map.external[0].id
  ssl_certificates = var.external_lb_config.ssl_certificates
}

# HTTP forwarding rule
resource "google_compute_global_forwarding_rule" "http" {
  count   = var.external_lb_config.enabled && contains(keys(var.external_lb_config.ports), "http") ? 1 : 0
  name    = "${var.external_lb_config.name}-http"
  project = var.project_id

  load_balancing_scheme = "EXTERNAL"
  ip_protocol           = "TCP"
  port_range            = var.external_lb_config.ports["http"].port
  target                = google_compute_target_http_proxy.external[0].id
  ip_address            = google_compute_global_address.external_lb[0].id

  labels = var.labels
}

# HTTPS forwarding rule (if SSL enabled)
resource "google_compute_global_forwarding_rule" "https" {
  count   = var.external_lb_config.enabled && var.external_lb_config.ssl_enabled && contains(keys(var.external_lb_config.ports), "https") ? 1 : 0
  name    = "${var.external_lb_config.name}-https"
  project = var.project_id

  load_balancing_scheme = "EXTERNAL"
  ip_protocol           = "TCP"
  port_range            = var.external_lb_config.ports["https"].port
  target                = google_compute_target_https_proxy.external[0].id
  ip_address            = google_compute_global_address.external_lb[0].id

  labels = var.labels
}
