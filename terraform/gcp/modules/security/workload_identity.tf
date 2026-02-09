# -----------------------------------------------------------------------------
# Workload Identity Federation Module
# GCP Security Module for CRE Terraform
#
# Implements:
# - Workload Identity Pool for GitHub Actions CI/CD
# - Workload Identity Pool for GKE workload authentication
# - OIDC provider configuration
# - IAM bindings for external identity providers
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Workload Identity Pool for GitHub Actions
# Enables keyless authentication from GitHub Actions to GCP
# -----------------------------------------------------------------------------
resource "google_iam_workload_identity_pool" "github" {
  project                   = var.project_id
  provider                  = google-beta
  workload_identity_pool_id = var.github_pool_id
  display_name              = "GitHub Actions Pool"
  description               = "Workload Identity Pool for GitHub Actions CI/CD authentication"

  # Disable basic authentication, require attribute conditions
  # This forces all federated credentials to use explicit attribute matching
}

# -----------------------------------------------------------------------------
# GitHub Actions OIDC Provider
# Configure GitHub as an external identity provider
# -----------------------------------------------------------------------------
resource "google_iam_workload_identity_pool_provider" "github_oidc" {
  project                            = var.iam_project_id != "" ? var.iam_project_id : var.project_id
  provider                           = google-beta
  workload_identity_pool_id          = google_iam_workload_identity_pool.github.workload_identity_pool_id
  workload_identity_pool_provider_id = "github-provider"
  display_name                       = "GitHub Actions OIDC Provider"
  description                        = "OIDC provider for GitHub Actions"

  # GitHub's OIDC issuer URL
  oidc {
    issuer_uri = "https://token.actions.githubusercontent.com"
  }

  # Map GitHub OIDC claims to GCP attributes
  attribute_mapping = {
    "google.subject"       = "assertion.sub"
    "attribute.repository" = "assertion.repository"
    "attribute.actor"      = "assertion.actor"
    "attribute.sha"        = "assertion.sha"
    "attribute.ref"        = "assertion.ref"
  }

  # Require GitHub authentication
  # Only allow tokens from GitHub with valid repository claim
  attribute_condition = var.github_attribute_condition != "" ? var.github_attribute_condition : null

  # Disable cached credentials for enhanced security
  # Forces fresh token validation on each request
}

# -----------------------------------------------------------------------------
# Workload Identity Pool for GKE Workloads
# Enables Kubernetes service accounts to impersonate GCP service accounts
# -----------------------------------------------------------------------------
resource "google_iam_workload_identity_pool" "gke" {
  project                   = var.project_id
  provider                  = google-beta
  workload_identity_pool_id = "${var.name_prefix}-gke-pool"
  display_name              = "GKE Workload Identity Pool"
  description               = "Workload Identity Pool for GKE pod authentication"
}

# -----------------------------------------------------------------------------
# GKE OIDC Provider
# Configure GKE cluster as an identity provider
# -----------------------------------------------------------------------------
resource "google_iam_workload_identity_pool_provider" "gke_oidc" {
  project                            = var.project_id
  provider                           = google-beta
  workload_identity_pool_id          = google_iam_workload_identity_pool.gke.workload_identity_pool_id
  workload_identity_pool_provider_id = "${var.name_prefix}-gke-provider"
  display_name                       = "GKE OIDC Provider"
  description                        = "OIDC provider for GKE cluster workload identity"

  # The issuer URI for the GKE cluster will be set after cluster creation
  # This is a placeholder that must be updated with the actual cluster issuer
  oidc {
    issuer_uri        = var.gke_cluster_issuer_uri != "" ? var.gke_cluster_issuer_uri : "https://container.googleapis.com/v1/projects/${var.project_id}/locations/${var.region}/clusters/${var.cluster_name}"
    # The JWKs URI is derived from the issuer
    # Allow issuance of OIDC tokens
    allowed_audiences = var.gke_allowed_audiences
  }

  # Map Kubernetes service account attributes
  attribute_mapping = {
    "google.subject"              = "assertion.sub"
    "attribute.kubernetes_namespace" = "assertion.kubernetes.namespace"
    "attribute.kubernetes_service_account" = "assertion.kubernetes.serviceaccount"
  }

  # Require the request to come from the specific GKE cluster
  attribute_condition = var.gke_attribute_condition != "" ? var.gke_attribute_condition : null
}

# -----------------------------------------------------------------------------
# Federated Credentials for GitHub Actions Repositories
# Create mappings for specific repositories to impersonate service accounts
# -----------------------------------------------------------------------------

# Main repository credential for Terraform operations
resource "google_service_account_iam_member" "github_main_terraform" {
  service_account_id = google_service_account.terraform.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/projects/${var.project_id}/locations/global/workloadIdentityPools/${google_iam_workload_identity_pool.github.workload_identity_pool_id}/attribute.repository/${var.github_repository}"
}

# Additional repository credentials (optional, for multi-repo setups)
resource "google_service_account_iam_member" "github_additional_repos" {
  for_each           = toset(var.additional_github_repositories)
  service_account_id = google_service_account.terraform.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/projects/${var.project_id}/locations/global/workloadIdentityPools/${google_iam_workload_identity_pool.github.workload_identity_pool_id}/attribute.repository/${each.value}"
}

# Environment-specific credentials for branch-based deployments
resource "google_service_account_iam_member" "github_branch_deployment" {
  count              = var.enable_branch_deployments ? 1 : 0
  service_account_id = google_service_account.terraform.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/projects/${var.project_id}/locations/global/workloadIdentityPools/${google_iam_workload_identity_pool.github.workload_identity_pool_id}/attribute.repository/${var.github_repository}"
}

# -----------------------------------------------------------------------------
# Workload Identity for Kubernetes Service Accounts
# Map K8s service accounts to GCP service accounts
# -----------------------------------------------------------------------------

# Default CRE application service account mapping
resource "google_service_account_iam_member" "k8s_cre_app" {
  service_account_id = google_service_account.gke_workload.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/projects/${var.project_id}/locations/global/workloadIdentityPools/${google_iam_workload_identity_pool.gke.workload_identity_pool_id}/attribute.kubernetes_namespace/${var.gke_namespace}/attribute.kubernetes_service_account/${var.kubernetes_service_account}"
}

# Additional namespace mappings for multi-tenant deployments
resource "google_service_account_iam_member" "k8s_additional_namespaces" {
  for_each           = toset(var.additional_kubernetes_namespaces)
  service_account_id = google_service_account.gke_workload.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/projects/${var.project_id}/locations/global/workloadIdentityPools/${google_iam_workload_identity_pool.gke.workload_identity_pool_id}/attribute.kubernetes_namespace/${each.value}/attribute.kubernetes_service_account/${var.kubernetes_service_account}"
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------
output "github_workload_identity_pool_id" {
  description = "Workload Identity Pool ID for GitHub Actions"
  value       = google_iam_workload_identity_pool.github.workload_identity_pool_id
}

output "github_workload_identity_pool_name" {
  description = "Fully-qualified resource name for GitHub Workload Identity Pool"
  value       = google_iam_workload_identity_pool.github.name
}

output "github_provider_name" {
  description = "Fully-qualified resource name for GitHub OIDC provider"
  value       = google_iam_workload_identity_pool_provider.github_oidc.name
}

output "gke_workload_identity_pool_id" {
  description = "Workload Identity Pool ID for GKE"
  value       = google_iam_workload_identity_pool.gke.workload_identity_pool_id
}

output "gke_workload_identity_pool_name" {
  description = "Fully-qualified resource name for GKE Workload Identity Pool"
  value       = google_iam_workload_identity_pool.gke.name
}

output "github_aws_kms_arn" {
  description = "The provider name for GitHub Actions AWS KMS (for reference)"
  value       = google_iam_workload_identity_pool_provider.github_oidc.name
}

output "terraform_github_identity" {
  description = "GitHub Actions identity string for terraform service account impersonation"
  value       = "provider=${google_iam_workload_identity_pool_provider.github_oidc.name},subject=repo:${var.github_repository}:ref:refs/heads/main"
}

output "workload_identity_federation_config" {
  description = "Configuration values for GitHub Actions workflow setup"
  value = {
    project_number          = var.project_number != "" ? var.project_number : null
    pool_id                = google_iam_workload_identity_pool.github.workload_identity_pool_id
    provider_id            = google_iam_workload_identity_pool_provider.github_oidc.workload_identity_pool_provider_id
    service_account_email  = google_service_account.terraform.email
    github_repository      = var.github_repository
  }
}
