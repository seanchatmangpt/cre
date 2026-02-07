terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}

resource "google_secret_manager_secret" "database_password" {
  secret_id = "database-password"

  labels = {
    environment = var.environment
    managed-by  = "terraform"
  }

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "database_password" {
  secret      = google_secret_manager_secret.database_password.id
  secret_data = var.database_password
}

resource "google_secret_manager_secret" "api_key" {
  secret_id = "api-key"

  labels = {
    environment = var.environment
    managed-by  = "terraform"
  }

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "api_key" {
  secret      = google_secret_manager_secret.api_key.id
  secret_data = var.api_key
}

resource "google_secret_manager_secret" "jwt_secret" {
  secret_id = "jwt-secret"

  labels = {
    environment = var.environment
    managed-by  = "terraform"
  }

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "jwt_secret" {
  secret      = google_secret_manager_secret.jwt_secret.id
  secret_data = var.jwt_secret
}

resource "google_secret_manager_secret" "oauth_client_secret" {
  secret_id = "oauth-client-secret"

  labels = {
    environment = var.environment
    managed-by  = "terraform"
  }

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "oauth_client_secret" {
  secret      = google_secret_manager_secret.oauth_client_secret.id
  secret_data = var.oauth_client_secret
}

resource "google_secret_manager_secret_iam_member" "database_password_accessor" {
  secret_id = google_secret_manager_secret.database_password.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${var.service_account_email}"
}

resource "google_secret_manager_secret_iam_member" "api_key_accessor" {
  secret_id = google_secret_manager_secret.api_key.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${var.service_account_email}"
}

resource "google_secret_manager_secret_iam_member" "jwt_secret_accessor" {
  secret_id = google_secret_manager_secret.jwt_secret.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${var.service_account_email}"
}

resource "google_secret_manager_secret_iam_member" "oauth_client_secret_accessor" {
  secret_id = google_secret_manager_secret.oauth_client_secret.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${var.service_account_email}"
}

output "database_password_secret_id" {
  description = "The ID of the database password secret"
  value       = google_secret_manager_secret.database_password.id
  sensitive   = true
}

output "api_key_secret_id" {
  description = "The ID of the API key secret"
  value       = google_secret_manager_secret.api_key.id
  sensitive   = true
}

output "jwt_secret_id" {
  description = "The ID of the JWT secret"
  value       = google_secret_manager_secret.jwt_secret.id
  sensitive   = true
}

output "oauth_client_secret_id" {
  description = "The ID of the OAuth client secret"
  value       = google_secret_manager_secret.oauth_client_secret.id
  sensitive   = true
}
