resource "google_project_service" "dns" {
  service            = "dns.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "clouddns" {
  service            = "clouddns.googleapis.com"
  disable_on_destroy = false
}

resource "google_compute_address" "app_lb_ip" {
  name                = "${var.environment}-app-lb-ip"
  address_type        = "EXTERNAL"
  network_tier        = "PREMIUM"
  description         = "Static IP for application load balancer"
}

resource "google_compute_address" "nlb_ip" {
  name                = "${var.environment}-nlb-ip"
  address_type        = "EXTERNAL"
  network_tier        = "PREMIUM"
  description         = "Static IP for network load balancer"
  region              = var.region
}

resource "google_compute_address" "api_lb_ip" {
  name                = "${var.environment}-api-lb-ip"
  address_type        = "EXTERNAL"
  network_tier        = "PREMIUM"
  description         = "Static IP for API load balancer"
}

resource "google_compute_health_check" "app_health_check" {
  name                = "${var.environment}-app-health-check"
  check_interval_sec  = 10
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3

  http_health_check {
    port               = 8080
    request_path       = "/health"
    proxy_header       = "NONE"
  }

  description = "Health check for app instances"
}

resource "google_compute_health_check" "api_health_check" {
  name                = "${var.environment}-api-health-check"
  check_interval_sec  = 10
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3

  http_health_check {
    port               = 3000
    request_path       = "/api/health"
    proxy_header       = "NONE"
  }

  description = "Health check for API instances"
}

resource "google_compute_health_check" "tcp_health_check" {
  name                = "${var.environment}-tcp-health-check"
  check_interval_sec  = 10
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3

  tcp_health_check {
    port = 9000
  }

  description = "Health check for TCP services"
}

resource "google_compute_instance_group" "app_ig" {
  name        = "${var.environment}-app-ig"
  description = "Instance group for app tier"
  zone        = "${var.region}-a"
  named_port {
    name = "http"
    port = 8080
  }
}

resource "google_compute_instance_group" "api_ig" {
  name        = "${var.environment}-api-ig"
  description = "Instance group for API tier"
  zone        = "${var.region}-b"
  named_port {
    name = "api"
    port = 3000
  }
}

resource "google_compute_backend_service" "app_backend" {
  name                    = "${var.environment}-app-backend"
  protocol                = "HTTP"
  port_name               = "http"
  timeout_sec             = 30
  connection_draining_timeout_sec = 300
  health_checks           = [google_compute_health_check.app_health_check.id]

  backend {
    group           = google_compute_instance_group.app_ig.self_link
    balancing_mode  = "RATE"
    max_rate_per_endpoint = 1000
  }

  session_affinity = "NONE"
  enable_cdn       = true

  cdn_policy {
    cache_mode        = "CACHE_ALL_STATIC"
    client_ttl        = 3600
    default_ttl       = 3600
    max_ttl           = 86400
    negative_caching  = true
  }

  log_config {
    enable      = true
    sample_rate = 1.0
  }

  description = "Backend service for app tier"
}

resource "google_compute_backend_service" "api_backend" {
  name                    = "${var.environment}-api-backend"
  protocol                = "HTTP"
  port_name               = "api"
  timeout_sec             = 30
  connection_draining_timeout_sec = 300
  health_checks           = [google_compute_health_check.api_health_check.id]

  backend {
    group           = google_compute_instance_group.api_ig.self_link
    balancing_mode  = "RATE"
    max_rate_per_endpoint = 500
  }

  session_affinity = "CLIENT_IP"

  log_config {
    enable      = true
    sample_rate = 1.0
  }

  description = "Backend service for API tier"
}

resource "google_compute_url_map" "app_url_map" {
  name            = "${var.environment}-app-url-map"
  default_service = google_compute_backend_service.app_backend.id
  description     = "URL map for app load balancer"

  host_rule {
    hosts        = ["app.example.com"]
    path_matcher = "app"
  }

  host_rule {
    hosts        = ["api.example.com"]
    path_matcher = "api"
  }

  path_matcher {
    name            = "app"
    default_service = google_compute_backend_service.app_backend.id

    path_rule {
      paths   = ["/static/*"]
      service = google_compute_backend_service.app_backend.id
    }
  }

  path_matcher {
    name            = "api"
    default_service = google_compute_backend_service.api_backend.id

    path_rule {
      paths   = ["/api/*"]
      service = google_compute_backend_service.api_backend.id
    }
  }
}

resource "google_compute_ssl_policy" "policy" {
  name            = "${var.environment}-ssl-policy"
  profile         = "MODERN"
  min_tls_version = "TLS_1_2"
  description     = "SSL policy for HTTPS load balancer"
}

resource "google_compute_ssl_certificate" "app_cert" {
  name        = "${var.environment}-app-cert"
  private_key = file("${path.module}/certs/private.key")
  certificate = file("${path.module}/certs/certificate.crt")
  description = "SSL certificate for app domain"

  lifecycle {
    create_before_destroy = true
  }
}

resource "google_compute_target_https_proxy" "app_https_proxy" {
  name             = "${var.environment}-app-https-proxy"
  url_map          = google_compute_url_map.app_url_map.id
  ssl_certificates = [google_compute_ssl_certificate.app_cert.id]
  ssl_policy       = google_compute_ssl_policy.policy.id
  description      = "HTTPS proxy for app load balancer"
}

resource "google_compute_target_http_proxy" "app_http_proxy" {
  name        = "${var.environment}-app-http-proxy"
  url_map     = google_compute_url_map.app_url_map.id
  description = "HTTP proxy for app load balancer"
}

resource "google_compute_global_forwarding_rule" "app_https" {
  name       = "${var.environment}-app-https-rule"
  ip_protocol = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range = "443"
  target     = google_compute_target_https_proxy.app_https_proxy.id
  address    = google_compute_address.app_lb_ip.id
  description = "Forwarding rule for HTTPS traffic to app"
}

resource "google_compute_global_forwarding_rule" "app_http" {
  name       = "${var.environment}-app-http-rule"
  ip_protocol = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range = "80"
  target     = google_compute_target_http_proxy.app_http_proxy.id
  address    = google_compute_address.app_lb_ip.id
  description = "Forwarding rule for HTTP traffic to app"
}

resource "google_compute_backend_service" "nlb_backend" {
  name                    = "${var.environment}-nlb-backend"
  protocol                = "TCP"
  timeout_sec             = 30
  connection_draining_timeout_sec = 300
  health_checks           = [google_compute_health_check.tcp_health_check.id]

  backend {
    group           = google_compute_instance_group.app_ig.self_link
    balancing_mode  = "CONNECTION"
  }

  session_affinity = "NONE"

  log_config {
    enable      = true
    sample_rate = 1.0
  }

  description = "Backend service for network load balancer"
}

resource "google_compute_target_tcp_proxy" "nlb_proxy" {
  name            = "${var.environment}-nlb-proxy"
  backend_service = google_compute_backend_service.nlb_backend.id
  proxy_header    = "NONE"
  description     = "TCP proxy for network load balancer"
}

resource "google_compute_global_forwarding_rule" "nlb" {
  name       = "${var.environment}-nlb-rule"
  ip_protocol = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range = "9000"
  target     = google_compute_target_tcp_proxy.nlb_proxy.id
  address    = google_compute_address.nlb_ip.id
  description = "Forwarding rule for network load balancer"
}

resource "google_compute_managed_ssl_certificate" "api_cert" {
  name        = "${var.environment}-api-managed-cert"
  description = "Managed SSL certificate for API domain"

  managed {
    domains = ["api.example.com"]
  }

  lifecycle {
    create_before_destroy = true
  }
}

resource "google_compute_target_https_proxy" "api_https_proxy" {
  name             = "${var.environment}-api-https-proxy"
  url_map          = google_compute_url_map.api_url_map.id
  ssl_certificates = [google_compute_managed_ssl_certificate.api_cert.id]
  ssl_policy       = google_compute_ssl_policy.policy.id
  description      = "HTTPS proxy for API load balancer"
}

resource "google_compute_url_map" "api_url_map" {
  name            = "${var.environment}-api-url-map"
  default_service = google_compute_backend_service.api_backend.id
  description     = "URL map for API load balancer"
}

resource "google_compute_global_forwarding_rule" "api_https" {
  name       = "${var.environment}-api-https-rule"
  ip_protocol = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range = "443"
  target     = google_compute_target_https_proxy.api_https_proxy.id
  address    = google_compute_address.api_lb_ip.id
  description = "Forwarding rule for API HTTPS traffic"
}

resource "google_compute_global_forwarding_rule" "api_http" {
  name       = "${var.environment}-api-http-rule"
  ip_protocol = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range = "80"
  target     = google_compute_target_http_proxy.api_http_proxy.id
  address    = google_compute_address.api_lb_ip.id
  description = "Forwarding rule for API HTTP traffic"
}

resource "google_compute_target_http_proxy" "api_http_proxy" {
  name        = "${var.environment}-api-http-proxy"
  url_map     = google_compute_url_map.api_url_map.id
  description = "HTTP proxy for API load balancer"
}

resource "google_dns_managed_zone" "primary" {
  name        = "${var.environment}-dns-zone"
  dns_name    = "example.com."
  description = "Primary DNS zone for ${var.environment} environment"

  labels = {
    environment = var.environment
  }
}

resource "google_dns_managed_zone" "secondary" {
  name        = "${var.environment}-dns-zone-secondary"
  dns_name    = "api.example.com."
  description = "Secondary DNS zone for API"

  labels = {
    environment = var.environment
  }
}

resource "google_dns_record_set" "app_a_record" {
  name         = google_dns_managed_zone.primary.dns_name
  managed_zone = google_dns_managed_zone.primary.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.app_lb_ip.address]

  depends_on = [google_compute_address.app_lb_ip]
}

resource "google_dns_record_set" "app_cname_record" {
  name         = "app.${google_dns_managed_zone.primary.dns_name}"
  managed_zone = google_dns_managed_zone.primary.name
  type         = "CNAME"
  ttl          = 300
  rrdatas      = [google_dns_managed_zone.primary.dns_name]
}

resource "google_dns_record_set" "api_a_record" {
  name         = google_dns_managed_zone.secondary.dns_name
  managed_zone = google_dns_managed_zone.secondary.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.api_lb_ip.address]

  depends_on = [google_compute_address.api_lb_ip]
}

resource "google_dns_record_set" "api_txt_record" {
  name         = google_dns_managed_zone.secondary.dns_name
  managed_zone = google_dns_managed_zone.secondary.name
  type         = "TXT"
  ttl          = 300
  rrdatas      = ["v=spf1 include:_spf.google.com ~all"]
}

resource "google_dns_record_set" "mx_record" {
  name         = google_dns_managed_zone.primary.dns_name
  managed_zone = google_dns_managed_zone.primary.name
  type         = "MX"
  ttl          = 3600
  rrdatas      = ["10 aspmx.l.google.com."]
}

resource "google_dns_policy" "dns_policy" {
  name                      = "${var.environment}-dns-policy"
  enable_inbound_forwarding = true
  enable_logging            = true

  inbound_forwarding_config {
    target_name_servers {
      ipv4_address = "8.8.8.8"
    }
    target_name_servers {
      ipv4_address = "8.8.4.4"
    }
  }

  logging_config {
    enable_logging = true
  }

  description = "DNS policy for ${var.environment} environment"
}

resource "google_compute_firewall" "allow_app_lb_health_check" {
  name    = "${var.environment}-allow-app-lb-health-check"
  network = google_compute_network.vpc.name

  allow {
    protocol = "tcp"
    ports    = ["8080"]
  }

  source_ranges = ["35.191.0.0/16", "130.211.0.0/22"]
  target_tags   = ["app-server"]
  description   = "Allow health checks from Google Cloud LB for app"
}

resource "google_compute_firewall" "allow_api_lb_health_check" {
  name    = "${var.environment}-allow-api-lb-health-check"
  network = google_compute_network.vpc.name

  allow {
    protocol = "tcp"
    ports    = ["3000"]
  }

  source_ranges = ["35.191.0.0/16", "130.211.0.0/22"]
  target_tags   = ["api-server"]
  description   = "Allow health checks from Google Cloud LB for API"
}

resource "google_compute_firewall" "allow_nlb_health_check" {
  name    = "${var.environment}-allow-nlb-health-check"
  network = google_compute_network.vpc.name

  allow {
    protocol = "tcp"
    ports    = ["9000"]
  }

  source_ranges = ["35.191.0.0/16", "130.211.0.0/22"]
  target_tags   = ["nlb-server"]
  description   = "Allow health checks from Google Cloud NLB"
}

resource "google_compute_firewall" "allow_from_app_lb" {
  name    = "${var.environment}-allow-from-app-lb"
  network = google_compute_network.vpc.name

  allow {
    protocol = "tcp"
    ports    = ["8080"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["app-server"]
  description   = "Allow traffic from app load balancer"
}

resource "google_compute_firewall" "allow_from_api_lb" {
  name    = "${var.environment}-allow-from-api-lb"
  network = google_compute_network.vpc.name

  allow {
    protocol = "tcp"
    ports    = ["3000"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["api-server"]
  description   = "Allow traffic from API load balancer"
}

resource "google_compute_firewall" "allow_from_nlb" {
  name    = "${var.environment}-allow-from-nlb"
  network = google_compute_network.vpc.name

  allow {
    protocol = "tcp"
    ports    = ["9000"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["nlb-server"]
  description   = "Allow traffic from network load balancer"
}
