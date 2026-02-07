terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.0"
    }
  }
}

variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "region" {
  description = "GCP Region"
  type        = string
  default     = "us-central1"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "dev"
}

resource "random_id" "db_suffix" {
  byte_length = 4
}

resource "google_sql_database_instance" "mysql_primary" {
  name             = "mysql-${var.environment}-${random_id.db_suffix.hex}"
  database_version = "MYSQL_8_0"
  region           = var.region

  settings {
    tier              = "db-f1-micro"
    availability_type = "ZONAL"
    disk_type         = "PD_SSD"
    disk_size         = 20
    disk_autoresize   = true

    backup_configuration {
      enabled                        = true
      point_in_time_recovery_enabled = true
      transaction_log_retention_days = 7
      backup_retention_settings {
        retained_backups = 30
        retention_unit   = "COUNT"
      }
    }

    ip_configuration {
      ipv4_enabled    = true
      private_network = null
      require_ssl     = false

      authorized_networks {
        name  = "allow_all"
        value = "0.0.0.0/0"
      }
    }

    database_flags {
      name  = "character_set_server"
      value = "utf8mb4"
    }

    database_flags {
      name  = "max_connections"
      value = "100"
    }

    user_labels = {
      environment = var.environment
      managed_by  = "terraform"
    }
  }

  deletion_protection = false

  depends_on = [google_service_networking_connection.private_vpc_connection]
}

resource "google_sql_database_instance" "postgresql_primary" {
  name             = "postgresql-${var.environment}-${random_id.db_suffix.hex}"
  database_version = "POSTGRES_15"
  region           = var.region

  settings {
    tier              = "db-f1-micro"
    availability_type = "ZONAL"
    disk_type         = "PD_SSD"
    disk_size         = 20
    disk_autoresize   = true

    backup_configuration {
      enabled                        = true
      point_in_time_recovery_enabled = true
      transaction_log_retention_days = 7
      backup_retention_settings {
        retained_backups = 30
        retention_unit   = "COUNT"
      }
    }

    ip_configuration {
      ipv4_enabled    = true
      private_network = null
      require_ssl     = false

      authorized_networks {
        name  = "allow_all"
        value = "0.0.0.0/0"
      }
    }

    database_flags {
      name  = "max_connections"
      value = "100"
    }

    user_labels = {
      environment = var.environment
      managed_by  = "terraform"
    }
  }

  deletion_protection = false

  depends_on = [google_service_networking_connection.private_vpc_connection]
}

resource "google_sql_database" "mysql_main" {
  name     = "maindb"
  instance = google_sql_database_instance.mysql_primary.name
  charset  = "utf8mb4"
  collation = "utf8mb4_unicode_ci"
}

resource "google_sql_database" "mysql_analytics" {
  name     = "analyticsdb"
  instance = google_sql_database_instance.mysql_primary.name
  charset  = "utf8mb4"
  collation = "utf8mb4_unicode_ci"
}

resource "google_sql_database" "postgresql_main" {
  name     = "maindb"
  instance = google_sql_database_instance.postgresql_primary.name
}

resource "google_sql_database" "postgresql_analytics" {
  name     = "analyticsdb"
  instance = google_sql_database_instance.postgresql_primary.name
}

resource "random_password" "mysql_root" {
  length  = 32
  special = true
}

resource "random_password" "mysql_app_user" {
  length  = 32
  special = true
}

resource "random_password" "postgresql_root" {
  length  = 32
  special = true
}

resource "random_password" "postgresql_app_user" {
  length  = 32
  special = true
}

resource "google_sql_user" "mysql_root" {
  name     = "root"
  instance = google_sql_database_instance.mysql_primary.name
  password = random_password.mysql_root.result
  type     = "BUILT_IN"
}

resource "google_sql_user" "mysql_app" {
  name     = "appuser"
  instance = google_sql_database_instance.mysql_primary.name
  password = random_password.mysql_app_user.result
  type     = "BUILT_IN"
}

resource "google_sql_user" "mysql_analytics" {
  name     = "analyticsuser"
  instance = google_sql_database_instance.mysql_primary.name
  password = random_password.mysql_app_user.result
  type     = "BUILT_IN"
}

resource "google_sql_user" "postgresql_root" {
  name     = "postgres"
  instance = google_sql_database_instance.postgresql_primary.name
  password = random_password.postgresql_root.result
  type     = "BUILT_IN"
}

resource "google_sql_user" "postgresql_app" {
  name     = "appuser"
  instance = google_sql_database_instance.postgresql_primary.name
  password = random_password.postgresql_app_user.result
  type     = "BUILT_IN"
}

resource "google_sql_user" "postgresql_analytics" {
  name     = "analyticsuser"
  instance = google_sql_database_instance.postgresql_primary.name
  password = random_password.postgresql_app_user.result
  type     = "BUILT_IN"
}

resource "google_service_networking_connection" "private_vpc_connection" {
  network                 = "default"
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = []
}

output "mysql_instance_connection_name" {
  value       = google_sql_database_instance.mysql_primary.connection_name
  description = "MySQL instance connection name"
}

output "mysql_instance_self_link" {
  value       = google_sql_database_instance.mysql_primary.self_link
  description = "MySQL instance self link"
}

output "mysql_instance_ip_address" {
  value       = google_sql_database_instance.mysql_primary.public_ip_address
  description = "MySQL instance public IP address"
}

output "mysql_root_password" {
  value       = random_password.mysql_root.result
  description = "MySQL root user password"
  sensitive   = true
}

output "mysql_appuser_password" {
  value       = random_password.mysql_app_user.result
  description = "MySQL appuser password"
  sensitive   = true
}

output "postgresql_instance_connection_name" {
  value       = google_sql_database_instance.postgresql_primary.connection_name
  description = "PostgreSQL instance connection name"
}

output "postgresql_instance_self_link" {
  value       = google_sql_database_instance.postgresql_primary.self_link
  description = "PostgreSQL instance self link"
}

output "postgresql_instance_ip_address" {
  value       = google_sql_database_instance.postgresql_primary.public_ip_address
  description = "PostgreSQL instance public IP address"
}

output "postgresql_root_password" {
  value       = random_password.postgresql_root.result
  description = "PostgreSQL root user password"
  sensitive   = true
}

output "postgresql_appuser_password" {
  value       = random_password.postgresql_app_user.result
  description = "PostgreSQL appuser password"
  sensitive   = true
}

output "databases_info" {
  value = {
    mysql = {
      instance  = google_sql_database_instance.mysql_primary.name
      databases = [
        google_sql_database.mysql_main.name,
        google_sql_database.mysql_analytics.name
      ]
      users = [
        google_sql_user.mysql_root.name,
        google_sql_user.mysql_app.name,
        google_sql_user.mysql_analytics.name
      ]
    }
    postgresql = {
      instance  = google_sql_database_instance.postgresql_primary.name
      databases = [
        google_sql_database.postgresql_main.name,
        google_sql_database.postgresql_analytics.name
      ]
      users = [
        google_sql_user.postgresql_root.name,
        google_sql_user.postgresql_app.name,
        google_sql_user.postgresql_analytics.name
      ]
    }
  }
  description = "Database instances, databases, and users information"
}
