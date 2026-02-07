# VPC Service Controls for CRE Platform
# Implements data exfiltration prevention and API access control

locals {
  vpc_sc_enabled = var.enable_vpc_service_controls

  # Services to protect within the perimeter
  restricted_services = [
    "storage.googleapis.com",
    "bigquery.googleapis.com",
    "sqladmin.googleapis.com",
    "container.googleapis.com",
    "secretmanager.googleapis.com",
    "cloudkms.googleapis.com",
    "pubsub.googleapis.com",
  ]
}

# Access Policy (organization-level resource)
resource "google_access_context_manager_access_policy" "cre_policy" {
  count  = local.vpc_sc_enabled ? 1 : 0
  parent = "organizations/${var.organization_id}"
  title  = "cre-access-policy"
}

# Access Level: Corporate Network
resource "google_access_context_manager_access_level" "corporate_network" {
  count  = local.vpc_sc_enabled ? 1 : 0
  parent = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}"
  name   = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}/accessLevels/corporate_network"
  title  = "corporate_network"

  basic {
    conditions {
      ip_subnetworks = var.corporate_ip_ranges
    }
  }
}

# Access Level: Authorized Service Accounts
resource "google_access_context_manager_access_level" "authorized_service_accounts" {
  count  = local.vpc_sc_enabled ? 1 : 0
  parent = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}"
  name   = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}/accessLevels/authorized_service_accounts"
  title  = "authorized_service_accounts"

  basic {
    conditions {
      members = [
        "serviceAccount:${google_service_account.gke_workload_identity.email}",
        "serviceAccount:${var.project_number}@cloudservices.gserviceaccount.com",
      ]
    }
  }
}

# Access Level: GKE Nodes
resource "google_access_context_manager_access_level" "gke_nodes" {
  count  = local.vpc_sc_enabled ? 1 : 0
  parent = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}"
  name   = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}/accessLevels/gke_nodes"
  title  = "gke_nodes"

  basic {
    conditions {
      vpc_network_sources {
        vpc_subnetwork {
          network = google_compute_network.vpc_network.self_link
        }
      }
    }
  }
}

# Service Perimeter: Production
resource "google_access_context_manager_service_perimeter" "cre_production" {
  count  = local.vpc_sc_enabled ? 1 : 0
  parent = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}"
  name   = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}/servicePerimeters/cre_production"
  title  = "cre_production"

  status {
    restricted_services = local.restricted_services
    resources = [
      "projects/${var.project_number}",
    ]

    # Ingress Policy: Allow from corporate network and authorized service accounts
    ingress_policies {
      ingress_from {
        identity_type = "ANY_IDENTITY"
        sources {
          access_level = google_access_context_manager_access_level.corporate_network[0].name
        }
        sources {
          access_level = google_access_context_manager_access_level.authorized_service_accounts[0].name
        }
      }

      ingress_to {
        resources = ["*"]
        operations {
          service_name = "storage.googleapis.com"
          method_selectors {
            method = "*"
          }
        }
        operations {
          service_name = "bigquery.googleapis.com"
          method_selectors {
            method = "*"
          }
        }
      }
    }

    # Egress Policy: Allow to approved external services
    egress_policies {
      egress_from {
        identity_type = "ANY_SERVICE_ACCOUNT"
      }

      egress_to {
        resources = ["*"]
        operations {
          service_name = "storage.googleapis.com"
          method_selectors {
            method = "google.storage.objects.get"
          }
          method_selectors {
            method = "google.storage.objects.list"
          }
        }
      }
    }

    # VPC Accessible Services: Restrict to essential services
    vpc_accessible_services {
      enable_restriction = true
      allowed_services = [
        "storage.googleapis.com",
        "bigquery.googleapis.com",
        "sqladmin.googleapis.com",
        "container.googleapis.com",
        "secretmanager.googleapis.com",
        "cloudkms.googleapis.com",
        "logging.googleapis.com",
        "monitoring.googleapis.com",
      ]
    }
  }

  # Lifecycle to prevent accidental deletion
  lifecycle {
    prevent_destroy = true
  }
}

# Service Perimeter: Development (separate from production)
resource "google_access_context_manager_service_perimeter" "cre_development" {
  count  = local.vpc_sc_enabled && var.create_dev_perimeter ? 1 : 0
  parent = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}"
  name   = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}/servicePerimeters/cre_development"
  title  = "cre_development"

  status {
    restricted_services = local.restricted_services
    resources = [
      "projects/${var.dev_project_number}",
    ]

    # More permissive policies for development
    vpc_accessible_services {
      enable_restriction = true
      allowed_services   = concat(local.restricted_services, [
        "compute.googleapis.com",
        "cloudbuild.googleapis.com",
      ])
    }
  }
}

# Bridge Perimeter: Connect production and development for CI/CD
resource "google_access_context_manager_service_perimeter" "cre_bridge" {
  count  = local.vpc_sc_enabled && var.create_bridge_perimeter ? 1 : 0
  parent = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}"
  name   = "accessPolicies/${google_access_context_manager_access_policy.cre_policy[0].name}/servicePerimeters/cre_bridge"
  title  = "cre_bridge"

  perimeter_type = "PERIMETER_TYPE_BRIDGE"

  status {
    resources = [
      google_access_context_manager_service_perimeter.cre_production[0].status[0].resources[0],
      google_access_context_manager_service_perimeter.cre_development[0].status[0].resources[0],
    ]
  }
}

# Outputs
output "vpc_sc_access_policy_name" {
  description = "VPC Service Controls Access Policy name"
  value       = local.vpc_sc_enabled ? google_access_context_manager_access_policy.cre_policy[0].name : null
}

output "vpc_sc_production_perimeter" {
  description = "Production service perimeter name"
  value       = local.vpc_sc_enabled ? google_access_context_manager_service_perimeter.cre_production[0].name : null
}

output "vpc_sc_development_perimeter" {
  description = "Development service perimeter name"
  value       = local.vpc_sc_enabled && var.create_dev_perimeter ? google_access_context_manager_service_perimeter.cre_development[0].name : null
}
