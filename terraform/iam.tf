# Service Accounts
resource "google_service_account" "app_sa" {
  account_id   = "app-service-account"
  display_name = "Application Service Account"
  description  = "Service account for application workloads"

  tags = var.labels
}

resource "google_service_account" "cloud_run_sa" {
  account_id   = "cloud-run-service-account"
  display_name = "Cloud Run Service Account"
  description  = "Service account for Cloud Run services"

  tags = var.labels
}

resource "google_service_account" "gke_sa" {
  account_id   = "gke-service-account"
  display_name = "GKE Service Account"
  description  = "Service account for GKE workloads"

  tags = var.labels
}

resource "google_service_account" "ci_cd_sa" {
  account_id   = "ci-cd-service-account"
  display_name = "CI/CD Service Account"
  description  = "Service account for CI/CD pipelines"

  tags = var.labels
}

# Service Account Keys
resource "google_service_account_key" "ci_cd_key" {
  service_account_id = google_service_account.ci_cd_sa.name
  public_key_type    = "TYPE_X509_PEM_FILE"
}

# IAM Bindings for App SA
resource "google_project_iam_member" "app_sa_storage_viewer" {
  project = var.project_id
  role    = "roles/storage.objectViewer"
  member  = "serviceAccount:${google_service_account.app_sa.email}"
}

resource "google_project_iam_member" "app_sa_logging_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.app_sa.email}"
}

resource "google_project_iam_member" "app_sa_monitoring_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.app_sa.email}"
}

resource "google_project_iam_member" "app_sa_trace_agent" {
  project = var.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.app_sa.email}"
}

# IAM Bindings for Cloud Run SA
resource "google_project_iam_member" "cloud_run_sa_logging_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

resource "google_project_iam_member" "cloud_run_sa_monitoring_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

resource "google_project_iam_member" "cloud_run_sa_artifactregistry_reader" {
  project = var.project_id
  role    = "roles/artifactregistry.reader"
  member  = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

# IAM Bindings for GKE SA
resource "google_project_iam_member" "gke_sa_logging_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.gke_sa.email}"
}

resource "google_project_iam_member" "gke_sa_monitoring_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.gke_sa.email}"
}

resource "google_project_iam_member" "gke_sa_trace_agent" {
  project = var.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.gke_sa.email}"
}

# IAM Bindings for CI/CD SA
resource "google_project_iam_member" "ci_cd_sa_compute_admin" {
  project = var.project_id
  role    = "roles/compute.admin"
  member  = "serviceAccount:${google_service_account.ci_cd_sa.email}"
}

resource "google_project_iam_member" "ci_cd_sa_storage_admin" {
  project = var.project_id
  role    = "roles/storage.admin"
  member  = "serviceAccount:${google_service_account.ci_cd_sa.email}"
}

resource "google_project_iam_member" "ci_cd_sa_artifactregistry_writer" {
  project = var.project_id
  role    = "roles/artifactregistry.writer"
  member  = "serviceAccount:${google_service_account.ci_cd_sa.email}"
}

resource "google_project_iam_member" "ci_cd_sa_container_developer" {
  project = var.project_id
  role    = "roles/container.developer"
  member  = "serviceAccount:${google_service_account.ci_cd_sa.email}"
}

# Workload Identity Binding - Cloud Run
resource "google_service_account_iam_binding" "cloud_run_workload_identity" {
  service_account_id = google_service_account.cloud_run_sa.name
  role               = "roles/iam.workloadIdentityUser"

  members = [
    "serviceAccount:${var.project_id}.svc.id.goog[${var.cloud_run_namespace}/cloud-run-wi]"
  ]
}

# Workload Identity Binding - GKE
resource "google_service_account_iam_binding" "gke_workload_identity" {
  service_account_id = google_service_account.gke_sa.name
  role               = "roles/iam.workloadIdentityUser"

  members = [
    "serviceAccount:${var.project_id}.svc.id.goog[${var.gke_namespace}/*]"
  ]
}

# Workload Identity Pool for External OIDC
resource "google_iam_workload_identity_pool" "external_identity_pool" {
  workload_identity_pool_id = "external-pool"
  location                  = var.workload_identity_location
  display_name              = "External OIDC Identity Pool"
  description               = "OIDC provider for external services"
  disabled                  = false
}

# OIDC Provider Configuration
resource "google_iam_workload_identity_pool_provider" "github" {
  workload_identity_pool_id          = google_iam_workload_identity_pool.external_identity_pool.workload_identity_pool_id
  workload_identity_pool_provider_id = "github"
  location                           = var.workload_identity_location
  display_name                       = "GitHub OIDC Provider"
  disabled                           = false

  attribute_mapping = {
    "google.subject"       = "assertion.sub"
    "attribute.actor"      = "assertion.actor"
    "attribute.repo"       = "assertion.repository"
    "attribute.repo_owner" = "assertion.repository_owner"
    "attribute.environment" = "assertion.environment"
  }

  oidc {
    issuer_uri = "https://token.actions.githubusercontent.com"
  }
}

# Service Account IAM Binding for GitHub
resource "google_service_account_iam_member" "github_workload_identity_user" {
  service_account_id = google_service_account.ci_cd_sa.name
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/projects/${var.project_id}/locations/${var.workload_identity_location}/workloadIdentityPools/${google_iam_workload_identity_pool.external_identity_pool.workload_identity_pool_id}/attribute.repo_owner/your-org"
}

# Custom Role for Application
resource "google_project_custom_role" "app_role" {
  role_id     = "appCustomRole"
  title       = "Application Custom Role"
  description = "Custom role for application requirements"

  permissions = [
    "storage.buckets.get",
    "storage.buckets.list",
    "storage.objects.get",
    "storage.objects.list",
    "logging.logEntries.create",
    "monitoring.timeSeries.create",
    "cloudtrace.traces.patch",
  ]
}

# Bind Custom Role to App SA
resource "google_project_iam_member" "app_sa_custom_role" {
  project = var.project_id
  role    = "projects/${var.project_id}/roles/${google_project_custom_role.app_role.role_id}"
  member  = "serviceAccount:${google_service_account.app_sa.email}"
}

# Kubernetes Service Account Annotation for GKE Workload Identity
resource "kubernetes_service_account" "gke_ksa" {
  metadata {
    name      = "gke-workload-identity"
    namespace = var.gke_namespace

    annotations = {
      "iam.gke.io/gcp-service-account" = google_service_account.gke_sa.email
    }
  }

  depends_on = [google_service_account.gke_sa]
}

# Output Service Account Emails
output "app_service_account_email" {
  value       = google_service_account.app_sa.email
  description = "Email of the application service account"
}

output "cloud_run_service_account_email" {
  value       = google_service_account.cloud_run_sa.email
  description = "Email of the Cloud Run service account"
}

output "gke_service_account_email" {
  value       = google_service_account.gke_sa.email
  description = "Email of the GKE service account"
}

output "ci_cd_service_account_email" {
  value       = google_service_account.ci_cd_sa.email
  description = "Email of the CI/CD service account"
}

output "workload_identity_pool_name" {
  value       = google_iam_workload_identity_pool.external_identity_pool.name
  description = "Full resource name of the workload identity pool"
}

output "github_provider_name" {
  value       = google_iam_workload_identity_pool_provider.github.name
  description = "Full resource name of the GitHub OIDC provider"
}
