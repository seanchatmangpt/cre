# CRE Backup Infrastructure Module

Production-ready backup automation for CRE (Common Runtime Environment) on Google Cloud Platform.

## Features

### 1. Automated Mnesia Backups
- **Hourly backups**: RPO of 15 minutes
- **Daily backups**: Full Mnesia snapshots
- **Weekly backups**: Extended retention with verification
- **Backup verification**: Integrity checks after upload
- **Restoration testing**: Automated validation

### 2. Cloud Spanner Backups
- **Automated backups**: Daily snapshots via Spanner native backup
- **Point-in-time recovery**: 7-day PITR window
- **CMEK encryption**: Customer-managed encryption keys

### 3. Filestore Backups (optional)
- **Snapshot configuration**: Automated daily snapshots
- **Cross-region replication**: Geo-redundancy for critical data

### 4. Security & Encryption
- **CMEK support**: Customer-managed encryption keys
- **Soft delete**: 7-day recovery window
- **IAM permissions**: Least-privilege service accounts

### 5. Monitoring & Alerting
- **Cloud Monitoring**: Backup metrics and health checks
- **Alert policies**: Failure notifications
- **Dashboard links**: Quick access to backup status

## SLA Targets

| Metric | Target | Implementation |
|--------|--------|----------------|
| RTO (Recovery Time) | 30 minutes | Automated restoration scripts |
| RPO (Recovery Point) | 15 minutes | Hourly Mnesia backups |
| Retention | 30 days | GCS lifecycle policies |
| Cross-region replication | Enabled | Dual-region GCS buckets |

## Usage

### Basic Usage

```hcl
module "backup" {
  source = "./modules/backup"

  project_id       = var.project_id
  backup_location  = "us-central1"
  retention_days   = 30
}
```

### Full Configuration

```hcl
module "backup" {
  source = "./modules/backup"

  project_id       = var.project_id
  backup_location  = "us-central1"

  # Retention policies
  retention_days          = 30
  replica_retention_days  = 7
  soft_delete_seconds     = 604800  # 7 days

  # Cross-region replication
  enable_cross_region_replication = true
  replication_location            = "us-east1"

  # Storage optimization
  enable_storage_class_transitions = true

  # Encryption
  create_cmek = true

  # Spanner
  create_spanner_resources = true
  spanner_instance_name    = "cre-spanner"
  spanner_database_name    = "cre-db"
  spanner_config           = "regional-us-central1"
  spanner_num_nodes        = 1

  # Monitoring
  enable_alerting = true
  alert_email     = "ops@example.com"

  # Labels
  labels = {
    environment = "production"
    managed_by  = "terraform"
  }
}
```

## Backup Schedules

### Hourly Backups
- **Schedule**: Every hour at minute 0
- **Retention**: 2 days
- **Purpose**: RPO compliance (15-minute objective)

### Daily Backups
- **Schedule**: Daily at 2 AM UTC
- **Retention**: 30 days
- **Purpose**: Standard recovery point

### Weekly Backups
- **Schedule**: Sunday at 3 AM UTC
- **Retention**: 90 days
- **Purpose**: Long-term archival

## Disaster Recovery

### Restore from GCS Backup

```bash
# List available backups
gsutil ls gs://PROJECT_ID-cre-backups/mnesia/daily/

# Download latest backup
LATEST=$(gsutil ls gs://PROJECT_ID-cre-backups/mnesia/daily/ | tail -1)
gsutil cp $LATEST ./restore.tar.gz

# Extract to Mnesia directory
tar -xzf restore.tar.gz -C /opt/cre/data/

# Restart CRE nodes
kubectl rollout restart deployment cre -n cre-prod
```

### Restore Spanner Database

```bash
# List available backups
gcloud spanner backups list \
  --instance=cre-spanner \
  --database=cre-db

# Restore from backup
gcloud spanner databases restore-operations start \
  --instance=cre-spanner \
  --destination-database=cre-db-restored \
  --backup=BACKUP_ID
```

### Restore Filestore Snapshot

```bash
# List snapshots
gcloud filestore snapshots list \
  --instance=cre-filestore-backup \
  --location=us-central1

# Create new instance from snapshot
gcloud filestore instances create cre-filestore-restored \
  --location=us-central1 \
  --tier=BASIC_HDD \
  --file-share=name=cre-backups,capacity=1TB \
  --snapshot=SNAPSHOT_NAME
```

## Monitoring

### Cloud Monitoring Metrics

The following custom metrics are published:

- `custom.googleapis.com/cre/backup/complete` - Backup completion status
- `custom.googleapis.com/cre/backup/size_bytes` - Backup file size
- `custom.googleapis.com/cre/backup/duration_seconds` - Backup duration
- `custom.googleapis.com/cre/backup/health` - Backup health check result
- `custom.googleapis.com/cre/backup/age_hours` - Time since last successful backup

### Health Check Queries

```bash
# Check latest backup age
gcloud monitoring time-series query \
  --metric='custom.googleapis.com/cre/backup/age_hours' \
  --format='table(metric.type, resource.labels.project_id, points[0].value)'

# List backup sizes
gcloud monitoring time-series query \
  --metric='custom.googleapis.com/cre/backup/size_bytes' \
  --format='table(metric.type, resource.labels.project_id, points[0].value)'
```

## Testing

### Manual Backup Test

```bash
# Create a manual backup job
kubectl create job --from=cronjob/cre-mnesia-backup-daily \
  manual-backup-$(date +%Y%m%d) \
  -n cre-backup

# Monitor backup job
kubectl get job -n cre-backup -w

# View backup logs
kubectl logs job/manual-backup-$(date +%Y%m%d) -n cre-backup -f
```

### Restoration Test

```bash
# Run restoration test (included in weekly backup)
./scripts/backup.sh --type=mnesia --schedule=weekly --test-restore
```

## Cost Optimization

### Storage Class Transitions

Backups automatically transition to cheaper storage classes:
- After 30 days: NEARLINE (~50% savings)
- After 90 days: COLDLINE (~80% savings)

### Estimated Monthly Costs

| Resource | Monthly Cost (USD) |
|----------|-------------------|
| Hourly backups (2d) | ~$0.10 |
| Daily backups (30d) | ~$1.50 |
| Weekly backups (90d) | ~$2.00 |
| Spanner backups (7d) | ~$0.50 |
| Replication (East) | ~$1.80 |
| **Total** | **~$5.90/month** |

## Security

### IAM Roles

The backup service account uses the following roles:
- `roles/storage.objectAdmin` - GCS operations
- `roles/spanner.backupWriter` - Spanner backups
- `roles/cloudkms.cryptoKeyEncrypterDecrypter` - CMEK operations

### Encryption

**Default**: Google-managed encryption keys

**CMEK**: Customer-managed encryption keys with Cloud KMS
- Key rotation: Every 90 days
- Key location: Same region as backups
- Key access: Separate from service account

### Access Control

- Uniform bucket-level access: Enabled
- Public access prevention: Enforced
- Logging: Enabled to dedicated bucket
- Audit logging: Via Cloud Audit Logs

## Maintenance

### Retention Policy Adjustment

```hcl
# Increase retention to 90 days
retention_days = 90
```

### Adding New Backup Schedule

```bash
# Add monthly backups via Kubernetes CronJob
kubectl apply -f k8s/gcp/backup-cronjob-monthly.yaml
```

### Key Rotation

```bash
# Rotate CMEK key (automatic after 90 days)
gcloud kms keys versions rotate cre-backup-key \
  --keyring=cre-backup-keyring \
  --location=us-central1
```

## Troubleshooting

### Backup Failures

1. **Check CronJob status**:
   ```bash
   kubectl get cronjob -n cre-backup
   kubectl get jobs -n cre-backup
   ```

2. **View job logs**:
   ```bash
   kubectl logs job/cre-mnesia-backup-daily-XXXXXX -n cre-backup
   ```

3. **Verify GCS access**:
   ```bash
   gsutil ls gs://PROJECT_ID-cre-backups/
   ```

### Large Backup Sizes

1. **Review Mnesia tables**:
   ```bash
   kubectl exec -it cre-XXXX -n cre-prod -- erl -noshell -eval "mnesia:system_info(), init:stop()."
   ```

2. **Consider backup exclusion**:
   Add tables to exclude from backup in `backup.sh`

3. **Enable compression**:
   ```yaml
   ENABLE_COMPRESSION: "true"
   ```

### Slow Uploads

1. **Check network egress**:
   ```bash
   gcloud compute networks diagnose --project=PROJECT_ID
   ```

2. **Consider multi-part upload**:
   Modify `backup.sh` for parallel uploads

## Outputs

After deployment, use the outputs for integration:

```hcl
output "backup_bucket_name" {
  value = module.backup.backup_bucket_name
}

output "restore_commands" {
  value = module.backup.restore_commands
}
```
