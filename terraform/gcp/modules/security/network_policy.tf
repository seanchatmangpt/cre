# -----------------------------------------------------------------------------
# Network Policy Module
# GCP Security Module for CRE Terraform
#
# Implements:
# - Pod-to-pod communication policies
# - Default deny ingress/egress for defense-in-depth
# - Specific allow rules for CRE workflow communication
# - Integration with GKE Network Policy Controller
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Default Deny All Ingress Policy
# Applied to all pods in the CRE namespace by default
# Creates a deny-by-default security posture
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "default_deny_ingress" {
  count = var.enable_default_deny_policies ? 1 : 0

  metadata {
    name      = "${var.name_prefix}-default-deny-ingress"
    namespace = var.gke_namespace
  }

  spec {
    # Select all pods in the namespace
    pod_selector {
      match_labels = {}
    }

    # Deny all incoming traffic by default
    policy_types = ["Ingress"]
  }
}

# -----------------------------------------------------------------------------
# Default Deny All Egress Policy
# Blocks all outbound traffic from pods unless explicitly allowed
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "default_deny_egress" {
  count = var.enable_default_deny_policies ? 1 : 0

  metadata {
    name      = "${var.name_prefix}-default-deny-egress"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = {}
    }

    # Deny all outgoing traffic by default
    policy_types = ["Egress"]
  }
}

# -----------------------------------------------------------------------------
# Allow DNS Resolution
# Required for all pods to resolve service names
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "allow_dns" {
  metadata {
    name      = "${var.name_prefix}-allow-dns"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = {}
    }

    # Allow egress to DNS servers
    egress {
      to {
        namespace_selector {
          match_labels = {
            name = "kube-system"
          }
        }
        pod_selector {
          match_labels = {
            k8s-app = "kube-dns"
          }
        }
      }

      ports {
        protocol = "UDP"
        port     = 53
      }
      ports {
        protocol = "TCP"
        port     = 53
      }
    }

    policy_types = ["Egress"]
  }
}

# -----------------------------------------------------------------------------
# CRE Application Internal Communication Policy
# Allows Erlang nodes to communicate with each other
# Required for EPMD (4369) and Erlang distribution ports
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "cre_internal_communication" {
  metadata {
    name      = "${var.name_prefix}-cre-internal"
    namespace = var.gke_namespace
  }

  spec {
    # Select all CRE application pods
    pod_selector {
      match_labels = var.cre_app_selector
    }

    # Allow ingress from other CRE pods
    ingress {
      from {
        pod_selector {
          match_labels = var.cre_app_selector
        }
      }
      # Erlang EPMD port
      ports {
        protocol = "TCP"
        port     = 4369
      }
      # Erlang distribution port range
      dynamic "ports" {
        for_each = var.erlang_port_range
        content {
          protocol = "TCP"
          port     = ports.value
        }
      }
    }

    # Allow egress to other CRE pods
    egress {
      to {
        pod_selector {
          match_labels = var.cre_app_selector
        }
      }
      ports {
        protocol = "TCP"
        port     = 4369
      }
      dynamic "ports" {
        for_each = var.erlang_port_range
        content {
          protocol = "TCP"
          port     = ports.value
        }
      }
    }

    policy_types = ["Ingress", "Egress"]
  }
}

# -----------------------------------------------------------------------------
# Allow Inbound Traffic from Istio Ingress Gateway
# If using Istio service mesh for external access
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "allow_ingress_gateway" {
  count = var.enable_istio ? 1 : 0

  metadata {
    name      = "${var.name_prefix}-allow-ingress-gateway"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = var.cre_app_selector
    }

    ingress {
      from {
        namespace_selector {
          match_labels = {
            "istio-injection" = "enabled"
          }
        }
        pod_selector {
          match_labels = {
            app = "istio-ingressgateway"
          }
        }
      }
      ports {
        protocol = "TCP"
        port     = var.cre_app_port
      }
    }

    policy_types = ["Ingress"]
  }
}

# -----------------------------------------------------------------------------
# Allow Traffic from Load Balancer Health Checks
# GCP health checks originate from specific IP ranges
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "allow_health_checks" {
  metadata {
    name      = "${var.name_prefix}-allow-health-checks"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = var.cre_app_selector
    }

    ingress {
      from {
        ip_block {
          # GCP health check IP ranges (US regions)
          # These should be configured based on the cluster region
          cidr = var.health_check_cidr
        }
      }
      ports {
        protocol = "TCP"
        port     = var.cre_health_check_port
      }
    }

    policy_types = ["Ingress"]
  }
}

# -----------------------------------------------------------------------------
# Allow Egress to Cloud Monitoring and Logging
# Required for application telemetry and observability
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "allow_monitoring_egress" {
  metadata {
    name      = "${var.name_prefix}-allow-monitoring-egress"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = var.cre_app_selector
    }

    egress {
      to {
        ip_block {
          # Google Cloud Monitoring and Logging IPs
          # Using private.googleapis.com for VPC access
          cidr = "199.36.153.4/30"
        }
      }
      ports {
        protocol = "TCP"
        port     = 443
      }
    }

    # Allow Stackdriver/OTLP collector
    egress {
      to {
        ip_block {
          cidr = "0.0.0.0/0"
        }
      }
      ports {
        protocol = "TCP"
        port     = 4317  # OTLP gRPC
      }
      ports {
        protocol = "TCP"
        port     = 4318  # OTLP HTTP
      }
    }

    policy_types = ["Egress"]
  }
}

# -----------------------------------------------------------------------------
# Allow Egress to Secret Manager
# Required for CSI driver and workload identity authentication
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "allow_secret_manager_egress" {
  metadata {
    name      = "${var.name_prefix}-allow-secret-manager-egress"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = var.cre_app_selector
    }

    egress {
      to {
        ip_block {
          # Secret Manager API endpoints
          # Allow access to secretmanager.googleapis.com
          cidr = "0.0.0.0/0"
          # Except for private networks
          except = var.private_network_cidrs
        }
      }
      ports {
        protocol = "TCP"
        port     = 443
      }
    }

    policy_types = ["Egress"]
  }
}

# -----------------------------------------------------------------------------
# Allow Egress to Pub/Sub
# Required for CRE workflow event publishing
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "allow_pubsub_egress" {
  metadata {
    name      = "${var.name_prefix}-allow-pubsub-egress"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = var.cre_app_selector
    }

    egress {
      to {
        ip_block {
          cidr   = "0.0.0.0/0"
          except = var.private_network_cidrs
        }
      }
      ports {
        protocol = "TCP"
        port     = 443
      }
    }

    policy_types = ["Egress"]
  }
}

# -----------------------------------------------------------------------------
# Database Access Policy
# Allow CRE pods to connect to Cloud SQL or external databases
# -----------------------------------------------------------------------------
resource "kubernetes_network_policy" "allow_database_egress" {
  count = var.enable_database_secrets ? 1 : 0

  metadata {
    name      = "${var.name_prefix}-allow-database-egress"
    namespace = var.gke_namespace
  }

  spec {
    pod_selector {
      match_labels = var.cre_app_selector
    }

    egress {
      to {
        # Cloud SQL private IP or database endpoint
        ip_block {
          cidr = var.database_cidr
        }
      }
      ports {
        protocol = "TCP"
        port     = var.database_port
      }
    }

    policy_types = ["Egress"]
  }
}

# -----------------------------------------------------------------------------
# Pod Security Policy Configuration
# Note: PSP is deprecated in favor of Pod Security Standards
# This uses the newer Pod Security Admission feature
# -----------------------------------------------------------------------------
# The kubernetes_pod_security_policy resource has been removed in kubernetes provider v3.x
# Use Pod Security Admission (PSA) labels on namespaces instead
# See: https://kubernetes.io/docs/concepts/security/pod-security-admission/
# -----------------------------------------------------------------------------
# Resource disabled due to deprecation
# resource "kubernetes_pod_security_policy" "cre_restricted" {
#   count = var.enable_pod_security_policy ? 1 : 0
#   ... (removed due to deprecation)
# }

# -----------------------------------------------------------------------------
# Pod Security Standards Labels (GKE 1.25+)
# Apply baseline security standards to the namespace
# -----------------------------------------------------------------------------
resource "kubernetes_namespace" "cre_with_security" {
  count = var.create_namespace ? 1 : 0

  metadata {
    name = var.gke_namespace

    labels = {
      # Enforce baseline Pod Security Standards
      "pod-security.kubernetes.io/enforce" = var.pod_security_enforce_level
      "pod-security.kubernetes.io/audit"   = var.pod_security_audit_level
      "pod-security.kubernetes.io/warn"    = var.pod_security_warn_level
    }
  }
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------
output "network_policy_names" {
  description = "Names of all created network policies"
  value = {
    default_deny_ingress    = var.enable_default_deny_policies ? "${var.name_prefix}-default-deny-ingress" : null
    default_deny_egress     = var.enable_default_deny_policies ? "${var.name_prefix}-default-deny-egress" : null
    allow_dns              = "${var.name_prefix}-allow-dns"
    cre_internal           = "${var.name_prefix}-cre-internal"
    allow_ingress_gateway  = var.enable_istio ? "${var.name_prefix}-allow-ingress-gateway" : null
    allow_health_checks    = "${var.name_prefix}-allow-health-checks"
    allow_monitoring_egress = "${var.name_prefix}-allow-monitoring-egress"
    allow_secret_manager    = "${var.name_prefix}-allow-secret-manager-egress"
    allow_pubsub_egress     = "${var.name_prefix}-allow-pubsub-egress"
    allow_database_egress   = var.enable_database_secrets ? "${var.name_prefix}-allow-database-egress" : null
  }
}

output "security_recommendations" {
  description = "Security recommendations for CRE deployment"
  value = {
    network_policies = [
      "Enable default-deny policies for production environments",
      "Regularly audit and update network policy rules",
      "Use network policies in conjunction with service mesh for zero-trust"
    ]
    pod_security = [
      "Use 'restricted' Pod Security Standard for production",
      "Run containers as non-root users",
      "Enable read-only root filesystem",
      "Drop all Linux capabilities except those explicitly needed"
    ]
    monitoring = [
      "Set up alerts for policy violations",
      "Audit log all network traffic denials",
      "Regular penetration testing of network policies"
    ]
  }
}
