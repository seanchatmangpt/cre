# CRE GCP Monitoring Module

Cost monitoring and alerting for CRE GCP deployment.

## Features

- **Budget Alerts**: Multi-threshold budget alerts (50%, 75%, 90%, 100%)
- **Cost Anomaly Detection**: Node count, storage, and CPU waste alerts
- **Daily Cost Reporting**: BigQuery integration for cost tracking
- **Slack Integration**: Pub/Sub push notifications to Slack
- **Cost Dashboard**: Cloud Monitoring dashboard for visualization

## Usage

```hcl
module "monitoring" {
  source = "./modules/monitoring"

  project_id        = var.project_id
  billing_account_id = var.billing_account_id
  environment       = "production"

  budget_amount      = 1000  # USD per month
  budget_thresholds  = [50.0, 75.0, 90.0, 100.0]

  enable_slack_notifications = true
  slack_webhook_url          = var.slack_webhook_url
  alert_email_addresses      = ["alerts@example.com"]

  enable_alerting = true
  alerts_enabled  = true
}
```

## Cost Optimization Recommendations

The system provides automated recommendations:

1. **Over-provisioned Nodes**: Reduce nodes when active workflows < 2 per node
2. **Low CPU Utilization**: Consider smaller instance types when CPU < 20%
3. **Memory Waste**: Right-size memory requests when utilization < 40%
4. **Storage Cleanup**: Review retention policies when storage grows > 50%

## Outputs

- `budget_id`: Billing budget resource ID
- `alert_policies`: Map of created alert policy IDs
- `dashboard_id`: Cost monitoring dashboard ID
- `cost_tracking_table`: BigQuery table for cost analysis
