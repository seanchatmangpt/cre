# terraform/gcp/modules/monitoring/variables.tf
# Variable specifications for monitoring module

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "billing_account_id" {
  description = "GCP billing account ID (format: 123456-7890AB-CDEF12)"
  type        = string
}

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  default     = "production"

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production."
  }
}

variable "region" {
  description = "GCP region for resources"
  type        = string
  default     = "us-central1"
}

# Budget configuration
variable "budget_amount" {
  description = "Monthly budget amount in USD"
  type        = number
  default     = 1000
}

variable "budget_currency" {
  description = "Budget currency code"
  type        = string
  default     = "USD"
}

variable "budget_period" {
  description = "Budget period: MONTH, QUARTER, YEAR, or custom"
  type        = string
  default     = "MONTH"

  validation {
    condition     = contains(["MONTH", "QUARTER", "YEAR", "custom"], var.budget_period)
    error_message = "Budget period must be MONTH, QUARTER, YEAR, or custom."
  }
}

variable "custom_budget_start" {
  description = "Custom budget start date (YYYY-MM-DD) for custom period"
  type        = string
  default     = null
}

variable "custom_budget_end" {
  description = "Custom budget end date (YYYY-MM-DD) for custom period"
  type        = string
  default     = null
}

variable "budget_thresholds" {
  description = "Budget alert thresholds as percentages (e.g., [50.0, 75.0, 90.0, 100.0])"
  type        = list(number)
  default     = [50.0, 75.0, 90.0, 100.0]

  validation {
    condition = alltrue([
      for t in var.budget_thresholds : t > 0 && t <= 100
    ])
    error_message = "All thresholds must be between 0 and 100."
  }
}

variable "include_credits" {
  description = "Whether to include credits in budget calculations"
  type        = bool
  default     = false
}

# Notifications
variable "notification_channels" {
  description = "List of Monitoring notification channel IDs for budget alerts"
  type        = list(string)
  default     = []
}

variable "enable_slack_notifications" {
  description = "Enable Slack notifications via Pub/Sub push"
  type        = bool
  default     = true
}

variable "slack_webhook_url" {
  description = "Slack webhook URL for budget alerts"
  type        = string
  sensitive   = true
  default     = ""
}

variable "alert_email_addresses" {
  description = "Email addresses to receive cost alerts"
  type        = list(string)
  default     = []
}

variable "budget_alert_service_account" {
  description = "Service account email for budget alert Pub/Sub publishing"
  type        = string
  default     = ""
}

# Alerting configuration
variable "enable_alerting" {
  description = "Enable cost anomaly alert policies"
  type        = bool
  default     = true
}

variable "alerts_enabled" {
  description = "Toggle to enable/disable all alerts"
  type        = bool
  default     = true
}

# Node count anomaly thresholds
variable "node_count_anomaly_threshold" {
  description = "Node count threshold for anomaly detection"
  type        = number
  default     = 10
}

variable "node_count_growth_threshold" {
  description = "CPU core threshold for detecting node growth (cores)"
  type        = number
  default     = 50
}

# Storage anomaly thresholds
variable "storage_growth_threshold_bytes" {
  description = "Storage growth threshold in bytes (100GB = 107374182400)"
  type        = number
  default     = 107374182400 # 100GB
}

variable "storage_growth_percent_threshold" {
  description = "Storage growth percentage threshold"
  type        = number
  default     = 50
}

# CPU waste detection
variable "cpu_waste_utilization_threshold" {
  description = "CPU utilization below this indicates waste (percentage)"
  type        = number
  default     = 20
}

variable "cpu_waste_request_threshold" {
  description = "CPU request threshold for waste detection (cores)"
  type        = number
  default     = 10
}

variable "cpu_waste_suggestion_percent" {
  description = "Suggested savings percentage for CPU waste"
  type        = number
  default     = 30
}

# Memory waste detection
variable "memory_waste_utilization_threshold" {
  description = "Memory utilization below this indicates waste (percentage)"
  type        = number
  default     = 40
}

# Cost spike detection
variable "daily_cost_spike_threshold" {
  description = "Daily cost spike threshold in USD"
  type        = number
  default     = 50
}

variable "cost_increase_rate_threshold" {
  description = "Cost increase rate threshold (USD per hour)"
  type        = number
  default     = 5
}

# Forecasting
variable "enable_forecasting" {
  description = "Enable cost forecasting budget alerts"
  type        = bool
  default     = true
}

variable "forecast_threshold_percent" {
  description = "Forecast threshold percentage"
  type        = number
  default     = 80
}

# Cost tracking
variable "enable_cost_tracking" {
  description = "Enable BigQuery cost tracking table"
  type        = bool
  default     = true
}

variable "cost_dataset_id" {
  description = "BigQuery dataset ID for cost tracking"
  type        = string
  default     = "cre_costs"
}

# Dashboard
variable "enable_dashboard" {
  description = "Enable cost monitoring dashboard"
  type        = bool
  default     = true
}

# General
variable "labels" {
  description = "Labels to apply to all resources"
  type        = map(string)
  default = {
    managed_by = "terraform"
    project    = "cre"
  }
}
