# -----------------------------------------------------------------------------
# Outputs - GCP Security Module
# Consolidated outputs from all security resources
# -----------------------------------------------------------------------------

output "service_accounts" {
  description = "All service account emails created by this module"
  value = {
    gke_node    = google_service_account.gke_node.email
    gke_workload = google_service_account.gke_workload.email
    terraform   = google_service_account.terraform.email
  }
}

output "service_account_ids" {
  description = "All service account resource IDs"
  value = {
    gke_node    = google_service_account.gke_node.id
    gke_workload = google_service_account.gke_workload.id
    terraform   = google_service_account.terraform.id
  }
}

output "workload_identity_pools" {
  description = "Workload Identity Pool configurations"
  value = {
    github = {
      id          = google_iam_workload_identity_pool.github.workload_identity_pool_id
      name        = google_iam_workload_identity_pool.github.name
      provider_id = google_iam_workload_identity_pool_provider.github_oidc.workload_identity_pool_provider_id
    }
    gke = {
      id          = google_iam_workload_identity_pool.gke.workload_identity_pool_id
      name        = google_iam_workload_identity_pool.gke.name
      provider_id = google_iam_workload_identity_pool_provider.gke_oidc.workload_identity_pool_provider_id
    }
  }
}

output "secrets" {
  description = "All Secret Manager secrets created"
  sensitive   = true
  value = {
    erlang_cookie = {
      id   = google_secret_manager_secret.erlang_cookie.id
      name = google_secret_manager_secret.erlang_cookie.name
    }
    app_config = {
      id   = google_secret_manager_secret.app_config.id
      name = google_secret_manager_secret.app_config.name
    }
  }
}

output "secret_annotations" {
  description = "Kubernetes Secret Store CSI Driver annotations"
  value = {
    erlang_cookie = "projects/${var.project_id}/secrets/${google_secret_manager_secret.erlang_cookie.secret_id}"
    app_config    = "projects/${var.project_id}/secrets/${google_secret_manager_secret.app_config.secret_id}"
  }
}

output "network_policies" {
  description = "Network policies created"
  value = {
    default_deny_ingress     = var.enable_default_deny_policies ? "${var.name_prefix}-default-deny-ingress" : null
    default_deny_egress      = var.enable_default_deny_policies ? "${var.name_prefix}-default-deny-egress" : null
    allow_dns               = "${var.name_prefix}-allow-dns"
    cre_internal            = "${var.name_prefix}-cre-internal"
    allow_ingress_gateway   = var.enable_istio ? "${var.name_prefix}-allow-ingress-gateway" : null
    allow_health_checks     = "${var.name_prefix}-allow-health-checks"
    allow_monitoring_egress = "${var.name_prefix}-allow-monitoring-egress"
    allow_secret_manager    = "${var.name_prefix}-allow-secret-manager-egress"
    allow_pubsub_egress     = "${var.name_prefix}-allow-pubsub-egress"
    allow_database_egress   = var.enable_database_secrets ? "${var.name_prefix}-allow-database-egress" : null
  }
}

output "github_actions_config" {
  description = "Configuration for GitHub Actions Workload Identity Federation"
  value = {
    provider                = "google"
    project_number          = var.project_number != "" ? var.project_number : null
    project_id              = var.project_id
    pool_id                 = google_iam_workload_identity_pool.github.workload_identity_pool_id
    provider_id             = google_iam_workload_identity_pool_provider.github_oidc.workload_identity_pool_provider_id
    service_account_email   = google_service_account.terraform.email
    # Example usage in GitHub Actions:
    # - name: Authenticate to GCP
    #   uses: google-github-actions/auth@v2
    #   with:
    #     workload_identity_provider: projects/${var.project_number}/locations/global/workloadIdentityPools/${google_iam_workload_identity_pool.github.workload_identity_pool_id}/providers/${google_iam_workload_identity_pool_provider.github_oidc.workload_identity_pool_provider_id}
    #     service_account: ${google_service_account.terraform.email}
  }
}

output "iam_bindings" {
  description = "Summary of IAM bindings created"
  value = {
    gke_node = [
      "roles/artifactregistry.reader",
      "roles/logging.logWriter",
      "roles/monitoring.metricWriter",
      "roles/storage.objectViewer"
    ]
    gke_workload = [
      "roles/secretmanager.secretAccessor",
      "roles/pubsub.publisher",
      "roles/pubsub.subscriber",
      "roles/monitoring.metricWriter",
      "roles/cloudtrace.agent"
    ]
    terraform = [
      "roles/editor",
      "roles/iam.serviceAccountAdmin"
    ]
  }
}

output "security_best_practices" {
  description = "Security best practices implemented by this module"
  value = [
    "Service account keyless authentication via Workload Identity Federation",
    "Least privilege IAM roles assigned to service accounts",
    "Secrets stored in Secret Manager with automatic access logging",
    "Network policies for pod-to-pod communication control",
    "Default-deny network policies for defense-in-depth",
    "Pod Security Standards enforced at namespace level",
    "Secret version TTL for automatic cleanup of old versions",
    "No service account keys stored in code or configuration"
  ]
}

output "github_actions_workflow_example" {
  description = "Example GitHub Actions workflow configuration"
  value = {
    project_number        = var.project_number != "" ? var.project_number : "YOUR_PROJECT_NUMBER"
    pool_id               = google_iam_workload_identity_pool.github.workload_identity_pool_id
    provider_id           = google_iam_workload_identity_pool_provider.github_oidc.workload_identity_pool_provider_id
    service_account_email = google_service_account.terraform.email
    project_id            = var.project_id
    cluster_name          = var.cluster_name
    region                = var.region
    notes                 = "Use google-github-actions/auth@v2 with workload_identity_provider"
  }
}

# -----------------------------------------------------------------------------
# IAP Outputs
# -----------------------------------------------------------------------------
output "iap_configuration" {
  description = "IAP configuration summary"
  value = {
    enabled              = var.iap_config.enabled
    backend_service      = var.iap_config.backend_service_name
    oauth_brand          = var.iap_config.oauth_brand_id
    authorized_domain    = var.iap_config.authorized_domain
    audit_logs_enabled   = var.iap_config.enable_audit_logs
    access_level_created = var.iap_config.create_access_level
    bypass_sa_created    = var.iap_config.create_bypass_sa
  }
}

output "iap_oauth_brand_id" {
  description = "OAuth brand ID for IAP"
  value       = var.iap_config.oauth_brand_id
}

output "iap_allowed_users" {
  description = "List of allowed users for IAP access"
  value       = var.iap_config.allowed_users
}
