# terraform/gcp/modules/monitoring/outputs.tf
# Output values from monitoring module

output "budget_id" {
  description = "ID of the billing budget"
  value       = google_billing_budget.cre_budget.id
}

output "budget_display_name" {
  description = "Display name of the billing budget"
  value       = google_billing_budget.cre_budget.display_name
}

output "budget_amount" {
  description = "Budget amount in the configured currency"
  value       = google_billing_budget.cre_budget.budget_amount.0.specified_amount.0.units / 1000000
}

output "budget_thresholds" {
  description = "Configured budget alert thresholds"
  value       = var.budget_thresholds
}

output "budget_pubsub_topic" {
  description = "Pub/Sub topic for budget notifications"
  value       = google_pubsub_topic.budget_alerts.id
}

output "budget_pubsub_topic_name" {
  description = "Name of the budget Pub/Sub topic"
  value       = google_pubsub_topic.budget_alerts.name
}

output "alert_policies" {
  description = "IDs of all alert policies"
  value = var.enable_alerting ? {
    node_count_anomaly     = try(google_monitoring_alert_policy.node_count_anomaly[0].id, null)
    storage_growth_anomaly = try(google_monitoring_alert_policy.storage_growth_anomaly[0].id, null)
    cpu_waste_detection    = try(google_monitoring_alert_policy.cpu_waste_detection[0].id, null)
    memory_waste_detection = try(google_monitoring_alert_policy.memory_waste_detection[0].id, null)
    daily_cost_spike       = try(google_monitoring_alert_policy.daily_cost_spike[0].id, null)
  } : {}
}

output "notification_channels" {
  description = "IDs of notification channels"
  value = {
    pubsub = try(google_monitoring_notification_channel.cost_alerts[0].id, null)
    email  = try(google_monitoring_notification_channel.email_alerts[0].id, null)
  }
}

output "dashboard_id" {
  description = "ID of the cost monitoring dashboard"
  value       = var.enable_dashboard ? try(google_monitoring_dashboard.cost_monitoring[0].id, null) : null
}

output "cost_tracking_table" {
  description = "BigQuery table for cost tracking"
  value       = var.enable_cost_tracking ? try(google_bigquery_table.cost_tracking[0].id, null) : null
}

output "logging_sink" {
  description = "Logging sink for budget alerts"
  value       = google_logging_sink.budget_alerts.id
}

output "forecast_budget_id" {
  description = "ID of the forecast budget (if enabled)"
  value       = var.enable_forecasting ? try(google_billing_budget.cre_forecast[0].id, null) : null
}

output "recommendations" {
  description = "Cost optimization recommendations"
  value = {
    over_provisioned_nodes = "Review node count if active workflows < 2 per node"
    low_cpu_utilization    = "Consider smaller instance types if CPU < 20%"
    memory_waste           = "Right-size memory requests if utilization < 40%"
    storage_cleanup        = "Review retention policies if storage grows > 50%"
    right_sizing           = "Use VPA for automatic resource tuning"
  }
}

output "daily_cost_report_config" {
  description = "BigQuery transfer configuration for daily cost reports"
  value       = var.enable_cost_tracking ? try(google_bigquery_data_transfer_config.daily_cost_report[0].id, null) : null
}
