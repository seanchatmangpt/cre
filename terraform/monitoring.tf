terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region"
  default     = "us-central1"
}

variable "notification_channel_email" {
  type        = string
  description = "Email address for alert notifications"
}

variable "log_sink_dataset" {
  type        = string
  description = "BigQuery dataset for log sink"
  default     = "monitoring_logs"
}

data "google_monitoring_notification_channel" "email" {
  display_name = "Email Notification"
  type         = "email"
  labels = {
    email_address = var.notification_channel_email
  }

  depends_on = [google_monitoring_notification_channel.email_channel]
}

resource "google_monitoring_notification_channel" "email_channel" {
  display_name = "Email Notification - ${var.notification_channel_email}"
  type         = "email"
  enabled      = true
  labels = {
    email_address = var.notification_channel_email
  }
}

resource "google_monitoring_dashboard" "main" {
  dashboard_json = jsonencode({
    displayName = "Cloud Monitoring Dashboard"
    mosaicLayout = {
      columns = 12
      tiles = [
        {
          width  = 6
          height = 4
          widget = {
            title = "CPU Usage"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/cpu/usage_time\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                      }
                    }
                  }
                  plotType = "LINE"
                  targetAxis = "Y1"
                }
              ]
              timeshiftDuration = "0s"
              yAxis = {
                label = "CPU Usage (seconds)"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          xPos   = 6
          width  = 6
          height = 4
          widget = {
            title = "Memory Usage"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/memory/usage\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_MEAN"
                      }
                    }
                  }
                  plotType = "LINE"
                  targetAxis = "Y1"
                }
              ]
              yAxis = {
                label = "Memory (bytes)"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          yPos   = 4
          width  = 6
          height = 4
          widget = {
            title = "Network In"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/network/received_bytes_count\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                      }
                    }
                  }
                  plotType = "LINE"
                  targetAxis = "Y1"
                }
              ]
              yAxis = {
                label = "Bytes/sec"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          xPos   = 6
          yPos   = 4
          width  = 6
          height = 4
          widget = {
            title = "Network Out"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/network/sent_bytes_count\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                      }
                    }
                  }
                  plotType = "LINE"
                  targetAxis = "Y1"
                }
              ]
              yAxis = {
                label = "Bytes/sec"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          yPos   = 8
          width  = 12
          height = 4
          widget = {
            title = "Disk I/O Operations"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/disk/read_ops_count\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                      }
                    }
                  }
                  plotType = "LINE"
                  targetAxis = "Y1"
                }
              ]
              yAxis = {
                label = "Operations/sec"
                scale = "LINEAR"
              }
            }
          }
        }
      ]
    }
  })
}

resource "google_monitoring_dashboard" "application" {
  dashboard_json = jsonencode({
    displayName = "Application Metrics Dashboard"
    mosaicLayout = {
      columns = 12
      tiles = [
        {
          width  = 6
          height = 4
          widget = {
            title = "Uptime"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type = \"uptime_url\" AND metric.type = \"monitoring.googleapis.com/uptime_check/check_passed\""
                  aggregation = {
                    alignmentPeriod  = "300s"
                    perSeriesAligner = "ALIGN_FRACTION_TRUE"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
            }
          }
        },
        {
          xPos   = 6
          width  = 6
          height = 4
          widget = {
            title = "Error Rate"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type = \"logging.googleapis.com/user/error_count\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
                  }
                }
              }
            }
          }
        },
        {
          yPos   = 4
          width  = 6
          height = 4
          widget = {
            title = "Request Count"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type = \"logging.googleapis.com/user/request_count\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                      }
                    }
                  }
                  plotType = "STACKED_AREA"
                  targetAxis = "Y1"
                }
              ]
            }
          }
        },
        {
          xPos   = 6
          yPos   = 4
          width  = 6
          height = 4
          widget = {
            title = "Latency (p95)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type = \"logging.googleapis.com/user/latency\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_PERCENTILE_95"
                      }
                    }
                  }
                  plotType = "LINE"
                  targetAxis = "Y1"
                }
              ]
              yAxis = {
                label = "Latency (ms)"
                scale = "LINEAR"
              }
            }
          }
        }
      ]
    }
  })
}

resource "google_monitoring_alert_policy" "high_cpu" {
  display_name = "High CPU Usage Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "CPU usage above 80%"
    condition_threshold {
      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/cpu/utilization\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 0.8

      aggregations {
        alignment_period  = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.email_channel.id]

  documentation {
    content   = "CPU usage exceeded 80% threshold. Investigate instance performance and consider scaling."
    mime_type = "text/markdown"
  }
}

resource "google_monitoring_alert_policy" "high_memory" {
  display_name = "High Memory Usage Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Memory usage above 90%"
    condition_threshold {
      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/memory/utilization\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 0.9

      aggregations {
        alignment_period  = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.email_channel.id]

  documentation {
    content   = "Memory usage exceeded 90% threshold. Review running processes and consider increasing instance size."
    mime_type = "text/markdown"
  }
}

resource "google_monitoring_alert_policy" "disk_space" {
  display_name = "Low Disk Space Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Disk usage above 85%"
    condition_threshold {
      filter          = "resource.type = \"gce_instance\" AND metric.type = \"compute.googleapis.com/instance/disk/utilization\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 0.85

      aggregations {
        alignment_period  = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.email_channel.id]

  documentation {
    content   = "Disk usage exceeded 85% threshold. Clean up unused files or expand disk size."
    mime_type = "text/markdown"
  }
}

resource "google_monitoring_alert_policy" "uptime_check_failed" {
  display_name = "Uptime Check Failed"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Uptime check failure"
    condition_threshold {
      filter          = "resource.type = \"uptime_url\" AND metric.type = \"monitoring.googleapis.com/uptime_check/check_passed\""
      duration        = "60s"
      comparison      = "COMPARISON_LT"
      threshold_value = 1

      aggregations {
        alignment_period  = "60s"
        per_series_aligner = "ALIGN_FRACTION_TRUE"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.email_channel.id]

  documentation {
    content   = "Uptime check failed. Service may be unavailable. Verify service health immediately."
    mime_type = "text/markdown"
  }
}

resource "google_monitoring_alert_policy" "error_rate" {
  display_name = "High Error Rate Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Error rate above 5%"
    condition_threshold {
      filter          = "metric.type = \"logging.googleapis.com/user/error_rate\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 0.05

      aggregations {
        alignment_period  = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.email_channel.id]

  documentation {
    content   = "Error rate exceeded 5%. Review application logs and investigate failures."
    mime_type = "text/markdown"
  }
}

resource "google_monitoring_alert_policy" "latency_spike" {
  display_name = "Latency Spike Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "p95 latency above 1000ms"
    condition_threshold {
      filter          = "metric.type = \"logging.googleapis.com/user/latency\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 1000

      aggregations {
        alignment_period  = "60s"
        per_series_aligner = "ALIGN_PERCENTILE_95"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.email_channel.id]

  documentation {
    content   = "p95 latency exceeded 1000ms. Investigate performance bottlenecks."
    mime_type = "text/markdown"
  }
}

resource "google_logging_project_sink" "all_logs" {
  name        = "all-logs-sink"
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${google_bigquery_dataset.monitoring_logs.dataset_id}"
  filter      = "severity >= DEFAULT"

  unique_writer_identity = true
}

resource "google_logging_project_sink" "error_logs" {
  name        = "error-logs-sink"
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${google_bigquery_dataset.monitoring_logs.dataset_id}"
  filter      = "severity >= ERROR"

  unique_writer_identity = true
}

resource "google_logging_project_sink" "critical_logs" {
  name        = "critical-logs-sink"
  destination = "storage.googleapis.com/${google_storage_bucket.critical_logs.name}"
  filter      = "severity = CRITICAL"

  unique_writer_identity = true
}

resource "google_logging_project_sink" "app_logs" {
  name        = "app-logs-sink"
  destination = "pubsub.googleapis.com/projects/${var.project_id}/topics/${google_pubsub_topic.app_logs.name}"
  filter      = "resource.type = \"gae_app\" OR resource.type = \"cloud_function\" OR resource.type = \"k8s_container\""

  unique_writer_identity = true
}

resource "google_bigquery_dataset" "monitoring_logs" {
  dataset_id                  = var.log_sink_dataset
  friendly_name               = "Monitoring Logs"
  description                 = "Dataset for Cloud Logging sinks"
  location                    = "US"
  default_table_expiration_ms = 7776000000

  labels = {
    environment = "monitoring"
    managed_by  = "terraform"
  }
}

resource "google_bigquery_dataset_iam_member" "logging_sink" {
  dataset_id = google_bigquery_dataset.monitoring_logs.dataset_id
  role       = "roles/bigquery.dataEditor"
  member     = google_logging_project_sink.all_logs.writer_identity
}

resource "google_storage_bucket" "critical_logs" {
  name          = "${var.project_id}-critical-logs"
  location      = var.region
  force_destroy = false

  lifecycle_rule {
    condition {
      num_newer_versions = 3
    }
    action {
      type = "Delete"
    }
  }

  lifecycle_rule {
    condition {
      age = 30
    }
    action {
      type          = "SetStorageClass"
      storage_class = "NEARLINE"
    }
  }

  lifecycle_rule {
    condition {
      age = 90
    }
    action {
      type          = "SetStorageClass"
      storage_class = "COLDLINE"
    }
  }

  uniform_bucket_level_access = true

  labels = {
    environment = "monitoring"
    managed_by  = "terraform"
  }
}

resource "google_storage_bucket_iam_member" "critical_logs_sink" {
  bucket = google_storage_bucket.critical_logs.name
  role   = "roles/storage.objectCreator"
  member = google_logging_project_sink.critical_logs.writer_identity
}

resource "google_pubsub_topic" "app_logs" {
  name                       = "app-logs-topic"
  message_retention_duration = "604800s"

  labels = {
    environment = "monitoring"
    managed_by  = "terraform"
  }
}

resource "google_pubsub_topic_iam_member" "app_logs_sink" {
  topic  = google_pubsub_topic.app_logs.name
  role   = "roles/pubsub.publisher"
  member = google_logging_project_sink.app_logs.writer_identity
}

resource "google_pubsub_subscription" "app_logs_subscription" {
  name                 = "app-logs-subscription"
  topic                = google_pubsub_topic.app_logs.name
  ack_deadline_seconds = 20

  message_retention_duration = "604800s"

  labels = {
    environment = "monitoring"
    managed_by  = "terraform"
  }
}

resource "google_logging_project_sink" "audit_logs" {
  name        = "audit-logs-sink"
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${google_bigquery_dataset.audit_logs.dataset_id}"
  filter      = "protoPayload.serviceName = \"cloudsql.googleapis.com\" OR protoPayload.serviceName = \"compute.googleapis.com\""

  unique_writer_identity = true
}

resource "google_bigquery_dataset" "audit_logs" {
  dataset_id            = "audit_logs"
  friendly_name         = "Audit Logs"
  description           = "Dataset for Cloud Audit Logs"
  location              = "US"
  default_table_expiration_ms = 31536000000

  labels = {
    environment = "audit"
    managed_by  = "terraform"
  }
}

resource "google_bigquery_dataset_iam_member" "audit_logs_sink" {
  dataset_id = google_bigquery_dataset.audit_logs.dataset_id
  role       = "roles/bigquery.dataEditor"
  member     = google_logging_project_sink.audit_logs.writer_identity
}

resource "google_logging_project_sink" "security_logs" {
  name        = "security-logs-sink"
  destination = "storage.googleapis.com/${google_storage_bucket.security_logs.name}"
  filter      = "protoPayload.methodName = \"storage.buckets.setIamPolicy\" OR protoPayload.methodName = \"compute.instances.delete\" OR protoPayload.status.code != 0"

  unique_writer_identity = true
}

resource "google_storage_bucket" "security_logs" {
  name          = "${var.project_id}-security-logs"
  location      = var.region
  force_destroy = false

  uniform_bucket_level_access = true

  versioning {
    enabled = true
  }

  labels = {
    environment = "security"
    managed_by  = "terraform"
  }
}

resource "google_storage_bucket_iam_member" "security_logs_sink" {
  bucket = google_storage_bucket.security_logs.name
  role   = "roles/storage.objectCreator"
  member = google_logging_project_sink.security_logs.writer_identity
}

output "monitoring_dashboard_url" {
  value       = "https://console.cloud.google.com/monitoring/dashboards/custom/${google_monitoring_dashboard.main.id}?project=${var.project_id}"
  description = "URL to the main monitoring dashboard"
}

output "application_dashboard_url" {
  value       = "https://console.cloud.google.com/monitoring/dashboards/custom/${google_monitoring_dashboard.application.id}?project=${var.project_id}"
  description = "URL to the application monitoring dashboard"
}

output "alert_policies" {
  value = {
    high_cpu           = google_monitoring_alert_policy.high_cpu.id
    high_memory        = google_monitoring_alert_policy.high_memory.id
    disk_space         = google_monitoring_alert_policy.disk_space.id
    uptime_check       = google_monitoring_alert_policy.uptime_check_failed.id
    error_rate         = google_monitoring_alert_policy.error_rate.id
    latency_spike      = google_monitoring_alert_policy.latency_spike.id
  }
  description = "Alert policy IDs"
}

output "log_sinks" {
  value = {
    all_logs      = google_logging_project_sink.all_logs.name
    error_logs    = google_logging_project_sink.error_logs.name
    critical_logs = google_logging_project_sink.critical_logs.name
    app_logs      = google_logging_project_sink.app_logs.name
    audit_logs    = google_logging_project_sink.audit_logs.name
    security_logs = google_logging_project_sink.security_logs.name
  }
  description = "Log sink names"
}

output "bigquery_datasets" {
  value = {
    monitoring_logs = google_bigquery_dataset.monitoring_logs.dataset_id
    audit_logs      = google_bigquery_dataset.audit_logs.dataset_id
  }
  description = "BigQuery dataset IDs"
}

output "storage_buckets" {
  value = {
    critical_logs = google_storage_bucket.critical_logs.name
    security_logs = google_storage_bucket.security_logs.name
  }
  description = "Cloud Storage bucket names"
}

output "pubsub_topic" {
  value       = google_pubsub_topic.app_logs.name
  description = "Pub/Sub topic name for app logs"
}
