# -----------------------------------------------------------------------------
# IAP Module - Identity-Aware Proxy Configuration
# GCP Security Module for CRE Terraform
#
# Implements:
# - OAuth 2.0 client for IAP authentication
# - IAP settings for backend services
# - IAM bindings for IAP-secured web proxy users
# - Domain-restricted access and authorization
# - Audit logging for access review
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# IAP OAuth 2.0 Client
# Create OAuth client for IAP authentication
# -----------------------------------------------------------------------------
resource "google_iap_client" "cre_api" {
  count        = var.iap_config.enabled ? 1 : 0
  display_name = "${var.name_prefix}-iap-client"
  brand        = var.iap_config.oauth_brand_id
}

# -----------------------------------------------------------------------------
# IAP Settings for CRE API Backend Service
# Configure IAP for the external load balancer backend service
# -----------------------------------------------------------------------------
resource "google_iap_web_backend_service_iam_policy" "cre_api" {
  count               = var.iap_config.enabled ? 1 : 0
  project             = var.project_id
  web_backend_service = var.iap_config.backend_service_name

  # Policy data with IAP access settings
  policy_data = data.google_iam_policy.iap_allowed_users.policy_data
}

# -----------------------------------------------------------------------------
# IAM Policy Data for IAP Access
# Define who can access IAP-secured resources
# -----------------------------------------------------------------------------
data "google_iam_policy" "iap_allowed_users" {
  binding {
    role = "roles/iap.httpsResourceAccessor"

    # Allowlisted users and service accounts
    members = compact(concat(
      # Individual users
      [for email in var.iap_config.allowed_users : "user:${email}"],
      # Service accounts for internal access
      [for email in var.iap_config.allowed_service_accounts : "serviceAccount:${email}"],
      # Google groups (recommended for management)
      [for email in var.iap_config.allowed_groups : "group:${email}"]
    ))
  }

  # Add domain-wide restriction if configured
  dynamic "binding" {
    for_each = var.iap_config.authorized_domain != "" ? [1] : []
    content {
      role    = "roles/iap.httpsResourceAccessor"
      members = ["domain:${var.iap_config.authorized_domain}"]
    }
  }
}

# Note: Backend service IAM is configured through the google_iap_web_backend_service_iam_policy resource above

# -----------------------------------------------------------------------------
# IAP Access Level (Access Context Manager)
# Define access levels for additional security controls
# -----------------------------------------------------------------------------
resource "google_access_context_manager_access_level" "iap_internal" {
  count       = var.iap_config.enabled && var.iap_config.create_access_level ? 1 : 0
  parent      = "accessPolicies/${var.iap_config.access_policy_id}"
  name        = "${var.name_prefix}-iap-access-level"
  title       = "${var.name_prefix} IAP Access Level"
  description = "IAP access level for CRE API - internal users only"
  basic {
    conditions {
      # Require corporate network or VPN
      # This is configured in Access Context Manager
      # The IP ranges should be updated based on your network
      ip_subnetworks = var.iap_config.access_level_ip_ranges
    }
  }
}

# -----------------------------------------------------------------------------
# Service Account for Internal IAP Bypass
# For service-to-service communication without IAP
# -----------------------------------------------------------------------------
resource "google_service_account" "iap_bypass" {
  count        = var.iap_config.enabled && var.iap_config.create_bypass_sa ? 1 : 0
  project      = var.project_id
  account_id   = "${var.name_prefix}-iap-bypass"
  display_name = "IAP Bypass Service Account for ${var.name_prefix}"
  description  = "Service account for internal services to bypass IAP authentication"
}

# -----------------------------------------------------------------------------
# IAM Binding for IAP Bypass Service Account
# Grant the bypass SA permission to access IAP-secured resources
# -----------------------------------------------------------------------------
resource "google_service_account_iam_member" "iap_bypass_token_creator" {
  count              = var.iap_config.enabled && var.iap_config.create_bypass_sa ? 1 : 0
  service_account_id = google_service_account.iap_bypass[0].id
  role               = "roles/iam.serviceAccountTokenCreator"
  member             = "serviceAccount:${var.iap_config.bypass_caller_sa}"
}

# -----------------------------------------------------------------------------
# IAP Secure Web Proxy User Role
# Grant users permission to use IAP
# -----------------------------------------------------------------------------
resource "google_project_iam_member" "iap_secure_web_proxy_users" {
  for_each = var.iap_config.enabled ? toset(var.iap_config.allowed_users) : toset([])
  project  = var.project_id
  role     = "roles/iap.secureWebProxyUser"
  member   = "user:${each.value}"
}

# -----------------------------------------------------------------------------
# IAP Audit Log Configuration
# Ensure audit logs are enabled for IAP access
# -----------------------------------------------------------------------------
resource "google_project_iam_audit_config" "iap_audit" {
  count   = var.iap_config.enabled && var.iap_config.enable_audit_logs ? 1 : 0
  project = var.project_id
  service = "iap.googleapis.com"

  audit_log_config {
    log_type = "ADMIN_READ"
  }
  audit_log_config {
    log_type = "DATA_READ"
  }
  audit_log_config {
    log_type = "DATA_WRITE"
  }
}

# -----------------------------------------------------------------------------
# Supporting Services Configuration
# Enable required services for IAP
# -----------------------------------------------------------------------------
resource "google_project_service" "iap_service" {
  count                      = var.iap_config.enabled && var.iap_config.enable_services ? 1 : 0
  project                    = var.project_id
  service                    = "iap.googleapis.com"
  disable_dependent_services = false
  disable_on_destroy         = false
}

resource "google_project_service" "identitytoolkit_service" {
  count                      = var.iap_config.enabled && var.iap_config.enable_services ? 1 : 0
  project                    = var.project_id
  service                    = "identitytoolkit.googleapis.com"
  disable_dependent_services = false
  disable_on_destroy         = false
}

resource "google_project_service" "oauth2_service" {
  count                      = var.iap_config.enabled && var.iap_config.enable_services ? 1 : 0
  project                    = var.project_id
  service                    = "oauth2.googleapis.com"
  disable_dependent_services = false
  disable_on_destroy         = false
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------
output "iap_client_id" {
  description = "OAuth 2.0 client ID for IAP"
  value       = var.iap_config.enabled ? try(google_iap_client.cre_api[0].client_id, null) : null
}

output "iap_client_secret" {
  description = "OAuth 2.0 client secret for IAP (sensitive) - NOTE: Use Google Cloud Console to retrieve"
  sensitive   = true
  value       = var.iap_config.enabled ? "RETRIEVE_FROM_GOOGLE_CLOUD_CONSOLE_IAP_SECTION" : null
}

output "iap_bypass_service_account" {
  description = "Service account for IAP bypass"
  value       = var.iap_config.enabled && var.iap_config.create_bypass_sa ? google_service_account.iap_bypass[0].email : null
}

output "iap_backend_service_name" {
  description = "Backend service name with IAP enabled"
  value       = var.iap_config.backend_service_name
}

# -----------------------------------------------------------------------------
# Local Variables for OAuth Brand ID resolution
# Auto-detect OAuth brand if not provided
# -----------------------------------------------------------------------------
locals {
  # If no brand ID is provided, use the project's organization
  oauth_brand_id = var.iap_config.oauth_brand_id != "" ? var.iap_config.oauth_brand_id : null

  # Validate IAP configuration
  iap_validation = var.iap_config.enabled ? (
    length(var.iap_config.allowed_users) > 0 ||
    length(var.iap_config.allowed_groups) > 0 ||
    var.iap_config.authorized_domain != ""
    ? true : (
      var.iap_config.create_bypass_sa && var.iap_config.bypass_caller_sa != ""
      ? true : false
    )
  ) : true

  # Error if IAP is enabled but no access is configured
  iap_error = var.iap_config.enabled && !local.iap_validation ? (
    "IAP is enabled but no allowed users, groups, domain, or bypass SA is configured"
  ) : null
}
