# terraform/gcp/modules/monitoring/budget.tf
# GCP Billing Budget Configuration for CRE Deployment

locals {
  # Pub/Sub topic for budget notifications
  budget_topic_name = "cre-budget-alerts"
}

# Pub/Sub topic for budget notifications
resource "google_pubsub_topic" "budget_alerts" {
  name    = local.budget_topic_name
  project = var.project_id

  labels = var.labels
}

# Pub/Sub subscription for Slack integration
resource "google_pubsub_subscription" "budget_alerts_slack" {
  count   = var.enable_slack_notifications ? 1 : 0
  name    = "${local.budget_topic_name}-slack"
  project = var.project_id
  topic   = google_pubsub_topic.budget_alerts.name

  # Push delivery to Slack webhook
  push_config {
    push_endpoint = var.slack_webhook_url
    oidc_token {
      service_account_email = var.budget_alert_service_account
    }
  }

  # 10 second message retention
  message_retention_duration = "600s"

  # At least once delivery
  ack_deadline_seconds = 60

  labels = var.labels

  depends_on = [google_pubsub_topic_iam_binding.budget_alert_subscriber]
}

# Budget notification logging sink
resource "google_logging_sink" "budget_alerts" {
  name        = "cre-budget-alerts-sink"
  description = "Sink for budget alert logs"
  destination = google_pubsub_topic.budget_alerts.id

  filter = "resource.type=\"billing_account\" AND protoPayload.serviceName=\"cloudbilling.googleapis.com\""

  unique_writer_identity = true
}

# IAM binding for billing account to publish to Pub/Sub
resource "google_pubsub_topic_iam_binding" "budget_alert_subscriber" {
  topic   = google_pubsub_topic.budget_alerts.name
  project = var.project_id
  role    = "roles/pubsub.subscriber"

  members = [
    "serviceAccount:${var.budget_alert_service_account}"
  ]
}

# IAM binding for logging sink
resource "google_pubsub_topic_iam_binding" "budget_alert_sink" {
  topic   = google_pubsub_topic.budget_alerts.name
  project = var.project_id
  role    = "roles/pubsub.publisher"

  members = [
    google_logging_sink.budget_alerts.unique_writer_identity
  ]
}

# Billing budget
resource "google_billing_budget" "cre_budget" {
  billing_account = var.billing_account_id
  display_name    = "${var.environment}-CRE Budget"

  # Budget amount
  budget_amount {
    specified_amount {
      currency_code = var.budget_currency
      units         = var.budget_amount * 1000000 # Convert to micros
    }
  }

  # Budget configuration
  all_updates_rule {
    # Pub/Sub notification
    pubsub_topic = google_pubsub_topic.budget_alerts.id

    # Disable default email notifications (using Pub/Sub instead)
    disable_default_iam_recipient_notifications = false

    # List of recipients for budget alerts
    monitoring_notification_channels = var.notification_channels
  }

  # Budget scope
  budget_filter {
    projects = ["projects/${var.project_id}"]

    # Optionally filter by environment label
    dynamic "credit_types_treatment" {
      for_each = var.include_credits ? [] : [1]
      content {
        exclude_credits = true
      }
    }
  }

  # Threshold rules for alerting
  dynamic "threshold_rule" {
    for_each = var.budget_thresholds
    content {
      threshold_percent = threshold_rule.value
      spend_basis       = "CURRENT_SPEND"
    }
  }

  # Time period for the budget
  dynamic "calendar_period" {
    for_each = var.budget_period == "custom" ? [] : [1]
    content {
      calendar_period = var.budget_period
    }
  }

  dynamic "time_period" {
    for_each = var.budget_period == "custom" && var.custom_budget_start != null && var.custom_budget_end != null ? [1] : []
    content {
      start_date = var.custom_budget_start
      end_date   = var.custom_budget_end
    }
  }

  labels = var.labels
}

# Forecasting budget (for prediction)
resource "google_billing_budget" "cre_forecast" {
  count           = var.enable_forecasting ? 1 : 0
  billing_account = var.billing_account_id
  display_name    = "${var.environment}-CRE Forecast Budget"

  budget_amount {
    specified_amount {
      currency_code = var.budget_currency
      units         = var.budget_amount * 1000000
    }
  }

  all_updates_rule {
    pubsub_topic = google_pubsub_topic.budget_alerts.id

    # Only forecast alerts
    monitoring_notification_channels = var.notification_channels
  }

  budget_filter {
    projects = ["projects/${var.project_id}"]
  }

  # Forecast threshold (typically lower to catch overages earlier)
  threshold_rule {
    threshold_percent = var.forecast_threshold_percent
    spend_basis       = "FORECASTED_SPEND"
  }

  calendar_period = var.budget_period

  labels = var.labels
}

# Cost aggregation query for per-environment tracking
resource "google_bigquery_table" "cost_tracking" {
  count               = var.enable_cost_tracking ? 1 : 0
  project             = var.project_id
  dataset_id          = var.cost_dataset_id
  table_id            = "cre_cost_tracking"
  deletion_protection = false

  schema {
    fields {
      name = "date"
      type = "DATE"
    }
    fields {
      name = "environment"
      type = "STRING"
    }
    fields {
      name = "resource_type"
      type = "STRING"
    }
    fields {
      name = "cost"
      type = "FLOAT64"
    }
    fields {
      name = "currency"
      type = "STRING"
    }
  }
}

# Scheduled query for daily cost reporting
resource "google_bigquery_data_transfer_config" "daily_cost_report" {
  count                  = var.enable_cost_tracking ? 1 : 0
  display_name           = "Daily CRE Cost Report"
  location               = var.region
  data_source_id         = "scheduled_query"
  schedule               = "every 24 hours"
  destination_dataset_id = var.cost_dataset_id

  params {
    parameters {
      name  = "query"
      value = <<-EOT
        SELECT
          CURRENT_DATE() as date,
          labels.value as environment,
          service.description as resource_type,
          SUM(cost) as cost,
          currency
        FROM `${var.project_id}.billing_dataset.gcp_billing_export_v1_*`
        WHERE _PARTITIONDATE = CURRENT_DATE()
        GROUP BY date, environment, resource_type, currency
      EOT
    }
    parameters {
      name  = "destination_table_name"
      value = "daily_cost_report"
    }
    parameters {
      name  = "write_disposition"
      value = "WRITE_TRUNCATE"
    }
  }
}
