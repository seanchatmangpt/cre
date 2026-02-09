# terraform/gcp/modules/monitoring/anomaly-alert.tf
# Alert Policies for Cost Anomalies in CRE Deployment

# Locals for notification channels
locals {
  # Combine notification channels based on configuration
  cost_alert_channels = concat(
    var.enable_slack_notifications && var.enable_alerting ? [google_monitoring_notification_channel.cost_alerts[0].id] : [],
    length(var.alert_email_addresses) > 0 && var.enable_alerting ? [google_monitoring_notification_channel.email_alerts[0].id] : []
  )
}

# Notification channel for alerts
resource "google_monitoring_notification_channel" "cost_alerts" {
  count          = var.enable_alerting ? 1 : 0
  project        = var.project_id
  type           = "pubsub"
  display_name   = "CRE Cost Anomaly Alerts"
  labels = {
    topic = google_pubsub_topic.budget_alerts.id
  }

  sensitive_labels {
    # Pub/Sub doesn't require sensitive_labels
  }

  depends_on = [google_pubsub_topic.budget_alerts]
}

# Notification channel for email alerts
resource "google_monitoring_notification_channel" "email_alerts" {
  count        = length(var.alert_email_addresses) > 0 && var.enable_alerting ? 1 : 0
  project      = var.project_id
  type         = "email"
  display_name = "CRE Cost Email Alerts"
  labels = {
    email_address = var.alert_email_addresses[0]
  }

  # Force verification (in production, this should be verified via UI)
  force_verification = false

  enabled = true
}

# Alert policy: Node count anomaly
resource "google_monitoring_alert_policy" "node_count_anomaly" {
  count        = var.enable_alerting ? 1 : 0
  project      = var.project_id
  display_name = "CRE Node Count Anomaly - ${var.environment}"
  combiner     = "OR"
  enabled      = var.alerts_enabled

  conditions {
    display_name = "Node count above threshold"
    condition_threshold {
      filter          = "resource.type=\"gce_instance\" AND metric.type=\"compute.googleapis.com/instance/cpu/utilization\""
      aggregation {
        alignment_period     = "300s"
        per_series_aligner   = "ALIGN_FRACTION_TRUE"
        cross_series_reducer = "REDUCE_COUNT"
        group_by_fields      = ["resource.label.project_id", "resource.label.zone"]
      }
      comparison      = "COMPARISON_GT"
      threshold_value = var.node_count_anomaly_threshold
      duration        = "900s"
      trigger {
        count = 1
      }
    }
  }

  conditions {
    display_name = "Node count sudden increase"
    condition_threshold {
      filter = "resource.type=\"k8s_container\" AND metric.type=\"kubernetes.io/container/cpu/request_cores\""
      aggregation {
        alignment_period     = "600s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.label.cluster_name", "resource.label.namespace"]
      }
      comparison      = "COMPARISON_GT"
      threshold_value = var.node_count_growth_threshold
      duration        = "600s"
      trigger {
        count = 1
      }
    }
  }

  # Alert severity and documentation
  severity       = "WARNING"
  documentation {
    content = <<-EOT
      Node count anomaly detected in ${var.environment} environment.

      Possible causes:
      - Cluster autoscaler triggered
      - Unexpected pod replication
      - Deployment misconfiguration

      Action items:
      1. Check GKE cluster node pools
      2. Review recent deployments
      3. Verify autoscaler settings
      4. Check cost implications

      View details: https://console.cloud.google.com/kubernetes/list?project=${var.project_id}
    EOT
  }

  # Add notification channels
  dynamic "notification_channels" {
    for_each = length(local.cost_alert_channels) > 0 ? [1] : []
    content {
      notification_channels = local.cost_alert_channels
    }
  }

  alert_strategy {
    # Auto-close alerts when resolved
    auto_close = "3600s"
  }

  labels = var.labels
}

# Alert policy: Storage growth anomaly
resource "google_monitoring_alert_policy" "storage_growth_anomaly" {
  count        = var.enable_alerting ? 1 : 0
  project      = var.project_id
  display_name = "CRE Storage Growth Anomaly - ${var.environment}"
  combiner     = "OR"
  enabled      = var.alerts_enabled

  conditions {
    display_name = "Rapid storage increase"
    condition_threshold {
      filter = "resource.type=\"gce_instance\" AND metric.type=\"compute.googleapis.com/disk/bytes_used\""
      aggregation {
        alignment_period     = "3600s"
        per_series_aligner   = "ALIGN_DELTA"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.label.project_id", "resource.label.device_name"]
      }
      comparison      = "COMPARISON_GT"
      threshold_value = var.storage_growth_threshold_bytes
      duration        = "1800s"
      trigger {
        count = 1
      }
    }
  }

  conditions {
    display_name = "GKE PV growth anomaly"
    condition_threshold {
      filter = "resource.type=\"k8s_node\" AND metric.type=\"kubernetes.io/node/fs/bytes_used\""
      aggregation {
        alignment_period     = "1800s"
        per_series_aligner   = "ALIGN_PERCENT_CHANGE"
        cross_series_reducer = "REDUCE_MEAN"
        group_by_fields      = ["resource.label.cluster_name"]
      }
      comparison      = "COMPARISON_GT"
      threshold_value = var.storage_growth_percent_threshold
      duration        = "1800s"
      trigger {
        count = 1
      }
    }
  }

  severity       = "WARNING"
  documentation {
    content = <<-EOT
      Storage growth anomaly detected in ${var.environment} environment.

      Possible causes:
      - Log accumulation
      - Database growth
      - Unexplained data retention
      - Backup retention policy

      Action items:
      1. Check PV/PVC usage
      2. Review log retention policies
      3. Check database sizes
      4. Verify cleanup jobs are running

      Cost impact: Additional storage may increase monthly bill.
    EOT
  }

  dynamic "notification_channels" {
    for_each = length(local.cost_alert_channels) > 0 ? [1] : []
    content {
      notification_channels = local.cost_alert_channels
    }
  }

  alert_strategy {
    auto_close = "86400s"
  }

  labels = var.labels
}

# Alert policy: CPU waste detection
resource "google_monitoring_alert_policy" "cpu_waste_detection" {
  count        = var.enable_alerting ? 1 : 0
  project      = var.project_id
  display_name = "CRE CPU Waste Detection - ${var.environment}"
  combiner     = "OR"
  enabled      = var.alerts_enabled

  conditions {
    display_name = "Low CPU utilization with high allocation"
    condition_threshold {
      filter = "resource.type=\"k8s_container\" AND metric.type=\"kubernetes.io/container/cpu/utilization\""
      aggregation {
        alignment_period     = "1800s"
        per_series_aligner   = "ALIGN_MEAN"
        cross_series_reducer = "REDUCE_MEAN"
        group_by_fields      = ["resource.label.namespace", "resource.label.pod_name"]
      }
      comparison      = "COMPARISON_LT"
      threshold_value = var.cpu_waste_utilization_threshold
      duration        = "3600s"
      trigger {
        count = 3
      }
    }
  }

  conditions {
    display_name = "High CPU request with low usage"
    condition_threshold {
      filter = "resource.type=\"k8s_container\" AND metric.type=\"kubernetes.io/container/cpu/request_cores\""
      aggregation {
        alignment_period     = "1800s"
        per_series_aligner   = "ALIGN_MEAN"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.label.namespace"]
      }
      comparison      = "COMPARISON_GT"
      threshold_value = var.cpu_waste_request_threshold
      duration        = "3600s"
      trigger {
        count = 1
      }
    }
  }

  severity       = "INFO"
  documentation {
    content = <<-EOT
      CPU waste detected in ${var.environment} environment.

      This indicates containers are allocated more CPU than they use.

      Recommendations:
      1. Review and adjust CPU requests/limits
      2. Use Horizontal Pod Autoscaler
      3. Enable Vertical Pod Autoscaler
      4. Consider right-sizing node pools

      Potential savings: Up to ${var.cpu_waste_suggestion_percent}% of compute costs.

      Documentation: https://cloud.google.com/kubernetes-engine/docs/how-to/right-size
    EOT
  }

  dynamic "notification_channels" {
    for_each = length(local.cost_alert_channels) > 0 ? [1] : []
    content {
      notification_channels = local.cost_alert_channels
    }
  }

  alert_strategy {
    auto_close = "7200s"
  }

  labels = var.labels
}

# Alert policy: Daily cost spike detection
resource "google_monitoring_alert_policy" "daily_cost_spike" {
  count        = var.enable_alerting ? 1 : 0
  project      = var.project_id
  display_name = "CRE Daily Cost Spike - ${var.environment}"
  combiner     = "OR"
  enabled      = var.alerts_enabled

  conditions {
    display_name = "Cost spike compared to baseline"
    condition_threshold {
      filter = "resource.type=\"billing_account\" AND metric.type=\"billing.googleapis.com/cost_amount\""
      aggregation {
        alignment_period     = "86400s"
        per_series_aligner   = "ALIGN_SUM"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.label.project_id"]
      }
      comparison      = "COMPARISON_GT"
      threshold_value = var.daily_cost_spike_threshold
      duration        = "3600s"
      trigger {
        count = 1
      }
    }
  }

  conditions {
    display_name = "Unusual cost increase rate"
    condition_threshold {
      filter = "resource.type=\"billing_account\" AND metric.type=\"billing.googleapis.com/cost_amount\""
      aggregation {
        alignment_period     = "3600s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
      }
      comparison      = "COMPARISON_GT"
      threshold_value = var.cost_increase_rate_threshold
      duration        = "3600s"
      trigger {
        count = 2
      }
    }
  }

  severity       = "ERROR"
  documentation {
    content = <<-EOT
      Daily cost spike detected in ${var.environment} environment!

      Immediate actions:
      1. Review billing details: https://console.cloud.google.com/billing
      2. Check for runaway resources
      3. Verify no accidental deletion of cost controls
      4. Review recent deployments or changes

      Budget status at risk: ${var.budget_amount * 100}% of monthly budget may be exceeded.
    EOT
  }

  dynamic "notification_channels" {
    for_each = length(local.cost_alert_channels) > 0 ? [1] : []
    content {
      notification_channels = local.cost_alert_channels
    }
  }

  alert_strategy {
    # Don't auto-close cost alerts - require manual review
    auto_close = "0s"
  }

  labels = var.labels
}

# Alert policy: Memory waste detection
resource "google_monitoring_alert_policy" "memory_waste_detection" {
  count        = var.enable_alerting ? 1 : 0
  project      = var.project_id
  display_name = "CRE Memory Waste Detection - ${var.environment}"
  combiner     = "OR"
  enabled      = var.alerts_enabled

  conditions {
    display_name = "Low memory utilization with high allocation"
    condition_threshold {
      filter = "resource.type=\"k8s_container\" AND metric.type=\"kubernetes.io/container/memory/used_bytes\""
      aggregation {
        alignment_period     = "1800s"
        per_series_aligner   = "ALIGN_MEAN"
        cross_series_reducer = "REDUCE_MEAN"
        group_by_fields      = ["resource.label.namespace", "resource.label.container_name"]
      }
      comparison      = "COMPARISON_LT"
      threshold_value = var.memory_waste_utilization_threshold
      duration        = "3600s"
      trigger {
        count = 3
      }
    }
  }

  severity       = "INFO"
  documentation {
    content = <<-EOT
      Memory waste detected in ${var.environment} environment.

      Containers are using significantly less memory than allocated.

      Recommendations:
      1. Right-size memory requests/limits
      2. Use Vertical Pod Autoscaler for automatic tuning
      3. Consider switching to smaller instance types
      4. Review application memory profiles

      Potential savings: 20-40% on memory-optimized workloads.
    EOT
  }

  dynamic "notification_channels" {
    for_each = length(local.cost_alert_channels) > 0 ? [1] : []
    content {
      notification_channels = local.cost_alert_channels
    }
  }

  alert_strategy {
    auto_close = "7200s"
  }

  labels = var.labels
}

# Dashboard for cost monitoring
resource "google_monitoring_dashboard" "cost_monitoring" {
  count        = var.enable_dashboard ? 1 : 0
  project      = var.project_id
  display_name = "CRE Cost Monitoring - ${var.environment}"

  grid_layout {
    widgets {
      title       = "Estimated Cost Trend"
      x_pos       = 0
      y_pos       = 0
      width       = 6
      height      = 4

      xy_chart {
        data_sets {
          time_series_query {
            unit        = "USD"
            time_series_filter {
              filter     = "resource.type=\"billing_account\" AND metric.type=\"billing.googleapis.com/cost_amount\""
              aggregation {
                alignment_period     = "86400s"
                per_series_aligner   = "ALIGN_SUM"
              }
            }
          }
        }
        chart_options {
          mode = "COLOR"
        }
      }
    }

    widgets {
      title       = "Node Count"
      x_pos       = 6
      y_pos       = 0
      width       = 6
      height      = 4

      xy_chart {
        data_sets {
          time_series_query {
            unit        = "1"
            time_series_filter {
              filter     = "resource.type=\"gce_instance\""
              aggregation {
                alignment_period     = "300s"
                per_series_aligner   = "ALIGN_COUNT"
                cross_series_reducer = "REDUCE_COUNT"
              }
            }
          }
        }
      }
    }

    widgets {
      title       = "Storage Usage"
      x_pos       = 0
      y_pos       = 4
      width       = 6
      height      = 4

      xy_chart {
        data_sets {
          time_series_query {
            unit        = "By"
            time_series_filter {
              filter     = "resource.type=\"gce_instance\" AND metric.type=\"compute.googleapis.com/disk/bytes_used\""
              aggregation {
                alignment_period     = "3600s"
                per_series_aligner   = "ALIGN_SUM"
                cross_series_reducer = "REDUCE_SUM"
              }
            }
          }
        }
      }
    }

    widgets {
      title       = "CPU Utilization vs Request"
      x_pos       = 6
      y_pos       = 4
      width       = 6
      height      = 4

      scorecard {
        time_series_query {
          unit        = "1"
          time_series_filter {
            filter     = "resource.type=\"k8s_container\" AND metric.type=\"kubernetes.io/container/cpu/utilization\""
            aggregation {
              alignment_period     = "1800s"
              per_series_aligner   = "ALIGN_MEAN"
            }
          }
        }
        gauge_view {
          lower_bound = 0
          upper_bound = 100
        }
      }
    }

    widgets {
      title       = "Budget Status"
      x_pos       = 0
      y_pos       = 8
      width       = 12
      height      = 4

      scorecard {
        time_series_query {
          unit        = "USD"
          time_series_filter {
            filter     = "resource.type=\"billing_account\" AND metric.type=\"billing.googleapis.com/budget_amount\""
            aggregation {
              alignment_period     = "86400s"
              per_series_aligner   = "ALIGN_FRACTION_TRUE"
            }
          }
        }
        gauge_view {
          lower_bound = 0
          upper_bound = var.budget_amount
        }
      }
    }
  }

  labels = var.labels
}
