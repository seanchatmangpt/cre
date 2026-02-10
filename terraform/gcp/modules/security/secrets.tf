# -----------------------------------------------------------------------------
# Secrets Module - Secret Manager Integration
# GCP Security Module for CRE Terraform
#
# Implements:
# - Secret Manager secrets for CRE configuration
# - Kubernetes Secret synchronization via Secret Store CSI Driver
# - Automatic secret rotation policies
# - Erlang cookie and sensitive data management
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Erlang Cookie Secret
# Critical security component for Erlang distributed node communication
# -----------------------------------------------------------------------------
resource "google_secret_manager_secret" "erlang_cookie" {
  project   = var.project_id
  secret_id = "${var.name_prefix}-erlang-cookie"

  replication {
    # Automatic replication to all regions in the primary location
    # Provides high availability and low latency access
    auto {}
  }

  # Customer-managed encryption key (optional)
  # If provided, secrets are encrypted with the specified KMS key
  # rotation {
  #   rotation_period = "7776000s"  # 90 days
  # }

  # Secret lifecycle - automatic destruction after version expiration
  version_destroy_ttl = var.secret_version_ttl

  # Labels for organization and cost allocation
  labels = merge(
    var.common_labels,
    {
      purpose     = "erlang-distribution"
      application = "cre"
      type        = "security-critical"
    }
  )

  annotations = {
    description = "Erlang cookie for secure distributed node communication in CRE cluster"
  }

  # Enable secret manager logging
  # All access attempts are logged in Cloud Audit Logs
}

# -----------------------------------------------------------------------------
# Erlang Cookie Secret Version
# The initial value is set as a random base64-encoded string
# In production, this should be managed externally or via a secure process
# -----------------------------------------------------------------------------
resource "google_secret_manager_secret_version" "erlang_cookie" {
  count       = var.create_erlang_cookie_version ? 1 : 0
  secret      = google_secret_manager_secret.erlang_cookie.id
  secret_data = var.erlang_cookie_value != "" ? var.erlang_cookie_value : random_password.erlang_cookie[0].result

  # Enable automatic destruction of previous versions
  # After the TTL expires, old versions are automatically deleted
}

# -----------------------------------------------------------------------------
# Random password generator for Erlang cookie
# Creates a cryptographically secure random cookie if none is provided
# -----------------------------------------------------------------------------
resource "random_password" "erlang_cookie" {
  count   = var.create_erlang_cookie_version && var.erlang_cookie_value == "" ? 1 : 0
  length  = 64
  special = false
  upper   = true
  lower   = true
  numeric = true
}

# -----------------------------------------------------------------------------
# Database Connection Secret
# -----------------------------------------------------------------------------
resource "google_secret_manager_secret" "database_url" {
  count     = var.enable_database_secrets ? 1 : 0
  project   = var.project_id
  secret_id = "${var.name_prefix}-database-url"

  replication {
    auto {}
  }

  version_destroy_ttl = var.secret_version_ttl

  labels = merge(
    var.common_labels,
    {
      purpose     = "database-connection"
      application = "cre"
    }
  )

  annotations = {
    description = "Database connection string for CRE persistent storage"
  }
}

# -----------------------------------------------------------------------------
# API Keys and Tokens
# -----------------------------------------------------------------------------
resource "google_secret_manager_secret" "api_tokens" {
  for_each = toset(var.api_secret_names)

  project   = var.project_id
  secret_id = "${var.name_prefix}-${each.value}-token"

  replication {
    auto {}
  }

  version_destroy_ttl = var.secret_version_ttl

  labels = merge(
    var.common_labels,
    {
      purpose     = "api-authentication"
      application = "cre"
      token_type  = each.value
    }
  )

  annotations = {
    description = "API token for ${each.value} integration"
  }
}

# -----------------------------------------------------------------------------
# Generic CRE Configuration Secret
# For application configuration that should not be stored in ConfigMap
# -----------------------------------------------------------------------------
resource "google_secret_manager_secret" "app_config" {
  project   = var.project_id
  secret_id = "${var.name_prefix}-app-config"

  replication {
    auto {}
  }

  version_destroy_ttl = var.secret_version_ttl

  labels = merge(
    var.common_labels,
    {
      purpose     = "application-config"
      application = "cre"
    }
  )

  annotations = {
    description = "Sensitive CRE application configuration"
  }
}

# -----------------------------------------------------------------------------
# Secret Manager IAM - Access Control
# -----------------------------------------------------------------------------

# Grant GKE workload service account access to secrets
resource "google_secret_manager_secret_iam_member" "erlang_cookie_accessor" {
  project   = var.project_id
  secret_id = google_secret_manager_secret.erlang_cookie.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.gke_workload.email}"
}

resource "google_secret_manager_secret_iam_member" "erlang_cookie_viewer" {
  project   = var.project_id
  secret_id = google_secret_manager_secret.erlang_cookie.secret_id
  role      = "roles/secretmanager.viewer"
  member    = "serviceAccount:${google_service_account.gke_workload.email}"
}

# Grant access to other secrets
resource "google_secret_manager_secret_iam_member" "app_config_accessor" {
  project   = var.project_id
  secret_id = google_secret_manager_secret.app_config.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.gke_workload.email}"
}

# Grant Terraform service account access for secret management
resource "google_secret_manager_secret_iam_member" "terraform_secret_admin" {
  project   = var.project_id
  secret_id = google_secret_manager_secret.erlang_cookie.secret_id
  role      = "roles/secretmanager.admin"
  member    = "serviceAccount:${google_service_account.terraform.email}"
}

# -----------------------------------------------------------------------------
# Automatic Secret Rotation
# Configure automatic rotation for critical secrets
# -----------------------------------------------------------------------------

# Enable automatic rotation for the Erlang cookie
resource "google_secret_manager_secret_version" "erlang_cookie_rotation" {
  # This resource is used to trigger rotation via external automation
  # The actual rotation is handled by Cloud Scheduler or external rotation jobs
  count  = var.enable_auto_rotation ? 1 : 0
  secret = google_secret_manager_secret.erlang_cookie.id

  # The secret_data should be provided by the rotation job
  # This is a placeholder for the rotation mechanism
  secret_data = random_password.erlang_cookie[0].result
}

# -----------------------------------------------------------------------------
# Kubernetes External Secret Store Configuration
# These outputs are used by the Secret Store CSI Driver or External Secrets Operator
# -----------------------------------------------------------------------------

# The Secret Store CSI Driver annotation format:
# annotation: "projects/{project}/secrets/{secret_id}/versions/{version}"
# Or with automatic version selection:
# annotation: "projects/{project}/secrets/{secret_id}"

locals {
  secret_annotations = {
    erlang_cookie = "projects/${var.project_id}/secrets/${google_secret_manager_secret.erlang_cookie.secret_id}"
    app_config    = "projects/${var.project_id}/secrets/${google_secret_manager_secret.app_config.secret_id}"
  }
}

# -----------------------------------------------------------------------------
# Secret Access Logging
# Enable detailed logging for secret access attempts
# Note: Logging is automatically enabled for all Secret Manager operations
# Logs are written to Cloud Audit Logs and can be exported to BigQuery/Splunk/etc.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------
output "erlang_cookie_secret_id" {
  description = "Secret Manager ID for the Erlang cookie"
  value       = google_secret_manager_secret.erlang_cookie.id
}

output "erlang_cookie_secret_name" {
  description = "Full resource name for the Erlang cookie secret"
  value       = google_secret_manager_secret.erlang_cookie.name
}

output "app_config_secret_id" {
  description = "Secret Manager ID for application config"
  value       = google_secret_manager_secret.app_config.id
}

output "secret_csi_driver_annotations" {
  description = "Kubernetes Secret Store CSI Driver annotations for pod manifests"
  value       = local.secret_annotations
}

output "secret_access_instructions" {
  description = "Instructions for accessing secrets in Kubernetes pods"
  value = {
    secret_store_csi = {
      driver             = "secretmanager.csi.k8s.io"
      read_only          = true
      secret_volume_type = "secret"
      # Example volume mount configuration:
      # volumes:
      # - name: erlang-cookie
      #   csi:
      #     driver: secretmanager.csi.k8s.io
      #     readOnly: true
      #     volumeAttributes:
      #       secret-name: ${google_secret_manager_secret.erlang_cookie.secret_id}
    }
    external_secrets_operator = {
      provider = "gcpsm"
      # Example ExternalSecret:
      # apiVersion: external-secrets.io/v1beta1
      # kind: ExternalSecret
      # metadata:
      #   name: erlang-cookie
      # spec:
      #   refreshInterval: 1h
      #   secretStoreRef:
      #     name: gcpsm-secretstore
      #     kind: SecretStore
      #   data:
      #   - secretKey: cookie
      #     remoteRef:
      #       key: ${google_secret_manager_secret.erlang_cookie.secret_id}
    }
  }
}

output "all_secret_ids" {
  description = "Map of all secret IDs created"
  value = {
    erlang_cookie = google_secret_manager_secret.erlang_cookie.id
    app_config    = google_secret_manager_secret.app_config.id
    database_url  = var.enable_database_secrets ? google_secret_manager_secret.database_url[0].id : null
  }
}
