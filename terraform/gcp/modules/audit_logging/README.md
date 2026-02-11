# CRE Audit Logging Module

## Overview

This Terraform module implements centralized audit logging for CRE deployments on GCP. It creates a BigQuery dataset with configurable retention (default: 400 days for SOX compliance) and a Log Router sink to automatically export CRE audit logs.

## Features

- **Long-term log retention**: BigQuery dataset with 400-day default retention (SOX compliant)
- **Automatic log export**: Log Router sink captures all CRE audit logs
- **Compliance ready**: Meets SOX, HIPAA, and PCI-DSS log retention requirements
- **Cost optimized**: Excludes DEBUG logs and health check probes
- **SQL-queryable**: Audit logs stored in BigQuery for compliance reporting

## Usage

```hcl
module "audit_logging" {
  source = "./modules/audit_logging"

  name_prefix    = "cre-prod"
  project_id     = "my-cre-project"
  region         = "us-central1"

  # Optional: Customize retention (default: 400 days)
  retention_days = 400

  # Optional: Add custom labels
  common_labels = {
    environment = "production"
    managed_by  = "terraform"
  }
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| `name_prefix` | Prefix for resource names | `string` | - | Yes |
| `project_id` | GCP project ID | `string` | - | Yes |
| `region` | GCP region for BigQuery dataset | `string` | - | Yes |
| `retention_days` | Log retention period in days | `number` | `400` | No |
| `log_filter` | Cloud Logging filter for audit logs | `string` | (see defaults) | No |
| `common_labels` | Labels to apply to resources | `map(string)` | `{}` | No |

## Outputs

| Name | Description |
|------|-------------|
| `audit_logs_dataset_id` | BigQuery dataset ID |
| `audit_logs_dataset_full_name` | Full BigQuery dataset name (project:dataset) |
| `audit_logs_sink_name` | Log Router sink name |
| `audit_logs_sink_writer_identity` | Sink writer identity (service account) |
| `audit_logs_retention_days` | Configured retention period |
| `audit_logs_query_example` | Example SQL query for audit logs |

## Audit Log Sources

This module captures the following log types:

1. **Workflow Receipts** (`wf_audit_log`): Append-only receipts for workflow transitions
2. **XES Event Logs** (`xes_serial`): Process mining event logs in XES format
3. **CRE Application Logs**: Structured logs from the workflow engine

## Compliance

### SOX (Sarbanes-Oxley)
- **Requirement**: 7-year retention for financial records
- **CRE Implementation**: 400-day retention (customer extends to 7 years via BigQuery table settings)

### HIPAA
- **Requirement**: 6-year retention for PHI-related logs
- **CRE Implementation**: 400-day retention (customer extends to 6 years)

### PCI-DSS
- **Requirement**: 1-year retention (minimum)
- **CRE Implementation**: 400-day retention (exceeds requirement)

## Cost Considerations

BigQuery storage and query costs apply:
- **Storage**: ~$0.02 per GB per month
- **Query**: $5 per TB scanned (use SELECT * LIMIT to reduce cost)

For 1 GB of audit logs per day:
- 400 days × 1 GB × $0.02 = **$8/month storage cost**
- Typical query: 1 GB scanned = **$0.005 per query**

## Troubleshooting

### Logs not appearing in BigQuery

1. Check Log Router sink status:
   ```bash
   gcloud logging sinks list
   gcloud logging sinks describe cre-prod-audit-logs-sink
   ```

2. Verify sink IAM permissions:
   ```bash
   gcloud bigquery datasets get-iam-policy PROJECT_ID:cre_prod_audit_logs
   ```

3. Test log filter in Logs Explorer:
   ```bash
   gcloud logging read 'logName:"projects/PROJECT_ID/logs/cre-audit-log"' --limit 5
   ```

### High query costs

Use partitioned queries to reduce cost:
```sql
SELECT *
FROM `PROJECT_ID.cre_prod_audit_logs`
WHERE _PARTITIONTIME > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 1 DAY)
```

## References

- [BigQuery Documentation](https://cloud.google.com/bigquery/docs)
- [Log Router Documentation](https://cloud.google.com/logging/docs/routing/overview)
- [CRE Security Whitepaper](../../docs/gcp/SECURITY_WHITEPAPER.md)
