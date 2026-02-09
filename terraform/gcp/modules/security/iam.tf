# -----------------------------------------------------------------------------
# IAM Module - Service Accounts and Role Bindings
# GCP Security Module for CRE Terraform
#
# Implements:
# - GKE node service account with minimal permissions
# - Terraform service account with keyless authentication
# - Workload Identity Federation for GitHub Actions
# - Least privilege IAM bindings
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# GKE Node Service Account
# -----------------------------------------------------------------------------
resource "google_service_account" "gke_node" {
  project      = var.project_id
  account_id   = "${var.name_prefix}-gke-node"
  display_name = "GKE Node Service Account for ${var.name_prefix}"
  description  = "Service account used by GKE nodes for pulling images and writing logs"

  # Ensure the account is deleted when the project is destroyed
  # but prevent accidental deletion of active accounts
  # This is managed by Terraform state, not by this flag
}

# -----------------------------------------------------------------------------
# GKE Node IAM Roles - Minimal Permissions Principle
# -----------------------------------------------------------------------------

# Allow pulling images from Artifact Registry
resource "google_project_iam_member" "gke_node_artifact_reader" {
  project = var.project_id
  role    = "roles/artifactregistry.reader"
  member  = "serviceAccount:${google_service_account.gke_node.email}"
}

# Allow writing logs and metrics
resource "google_project_iam_member" "gke_node_log_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.gke_node.email}"
}

resource "google_project_iam_member" "gke_node_metric_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.gke_node.email}"
}

# Allow accessing GCR (legacy, still often needed)
resource "google_project_iam_member" "gke_node_storage_viewer" {
  project = var.project_id
  role    = "roles/storage.objectViewer"
  member  = "serviceAccount:${google_service_account.gke_node.email}"
}

# -----------------------------------------------------------------------------
# GKE Workload Service Account
# Used by application pods to access GCP services (Secret Manager, Pub/Sub, etc.)
# -----------------------------------------------------------------------------
resource "google_service_account" "gke_workload" {
  project      = var.project_id
  account_id   = "${var.name_prefix}-gke-workload"
  display_name = "GKE Workload Service Account for ${var.name_prefix}"
  description  = "Service account used by CRE application pods running in GKE"
}

# Workload IAM - Secret Manager Access (for Erlang cookie and sensitive config)
resource "google_project_iam_member" "gke_workload_secret_accessor" {
  project = var.project_id
  role    = "roles/secretmanager.secretAccessor"
  member  = "serviceAccount:${google_service_account.gke_workload.email}"
}

# Workload IAM - Pub/Sub access for CRE workflow events
resource "google_project_iam_member" "gke_workload_pubsub_publisher" {
  project = var.project_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.gke_workload.email}"
}

resource "google_project_iam_member" "gke_workload_pubsub_subscriber" {
  project = var.project_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.gke_workload.email}"
}

# Workload IAM - Cloud Monitoring for application metrics
resource "google_project_iam_member" "gke_workload_monitoring_metric_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.gke_workload.email}"
}

# Workload IAM - Cloud Trace for distributed tracing
resource "google_project_iam_member" "gke_workload_cloud_trace_agent" {
  project = var.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.gke_workload.email}"
}

# -----------------------------------------------------------------------------
# Terraform Service Account - Keyless Authentication
# Used by CI/CD to manage infrastructure
# -----------------------------------------------------------------------------
resource "google_service_account" "terraform" {
  project      = var.project_id
  account_id   = "${var.name_prefix}-terraform"
  display_name = "Terraform Service Account for ${var.name_prefix}"
  description  = "Service account for Terraform CI/CD with Workload Identity Federation"
}

# Terraform IAM - Full infrastructure management
resource "google_project_iam_member" "terraform_editor" {
  project = var.project_id
  role    = "roles/editor"
  member  = "serviceAccount:${google_service_account.terraform.email}"
}

# Additional service account admin role for managing other service accounts
resource "google_project_iam_member" "terraform_sa_admin" {
  project = var.project_id
  role    = "roles/iam.serviceAccountAdmin"
  member  = "serviceAccount:${google_service_account.terraform.email}"
}

# -----------------------------------------------------------------------------
# Workload Identity Federation IAM Bindings
# Allow external identities (GitHub Actions) to impersonate service accounts
# -----------------------------------------------------------------------------

# Grant GitHub Actions the ability to impersonate the Terraform service account
resource "google_service_account_iam_member" "terraform_github_impersonator" {
  service_account_id = google_service_account.terraform.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/projects/${var.project_id}/locations/global/workloadIdentityPools/${var.github_pool_id}/attribute.repository/${var.github_repository}"
}

# Grant GKE pods the ability to impersonate the workload service account
resource "google_service_account_iam_member" "workload_gke_impersonator" {
  service_account_id = google_service_account.gke_workload.id
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${var.gke_namespace}/${var.kubernetes_service_account}]"
}

# -----------------------------------------------------------------------------
# Service Account Impersonation Roles for Admin Access
# Optional: Allow designated humans to impersonate service accounts for debugging
# -----------------------------------------------------------------------------
resource "google_service_account_iam_member" "gke_workload_admin_impersonator" {
  count              = var.enable_admin_impersonation ? 1 : 0
  service_account_id = google_service_account.gke_workload.id
  role               = "roles/iam.serviceAccountTokenCreator"
  member             = "user:${var.admin_impersonator_email}"
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------
output "gke_node_service_account_email" {
  description = "Email of the GKE node service account"
  value       = google_service_account.gke_node.email
}

output "gke_node_service_account_id" {
  description = "ID of the GKE node service account"
  value       = google_service_account.gke_node.id
}

output "gke_workload_service_account_email" {
  description = "Email of the GKE workload service account"
  value       = google_service_account.gke_workload.email
}

output "gke_workload_service_account_id" {
  description = "ID of the GKE workload service account"
  value       = google_service_account.gke_workload.id
}

output "terraform_service_account_email" {
  description = "Email of the Terraform service account"
  value       = google_service_account.terraform.email
}

output "terraform_service_account_name" {
  description = "Fully-qualified name of the Terraform service account for Workload Identity"
  value       = google_service_account.terraform.name
}
