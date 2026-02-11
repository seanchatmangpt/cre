# Spanner Import - Quick Reference

## Cheat Sheet

### Basic Import
```bash
./scripts/migration/spanner-import.sh --source-dir /tmp/mnesia-export
```

### From GCS
```bash
./scripts/migration/spanner-import.sh --source-gcs gs://bucket/export_id
```

### Preview (Dry-Run)
```bash
./scripts/migration/spanner-import.sh --source-dir /tmp/export --dry-run
```

### Import Specific Tables
```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/export \
  --tables workflow_cases,work_items
```

### With Custom Configuration
```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/export \
  --project my-project \
  --instance cre-spanner \
  --database cre-db \
  --batch-size 200 \
  --cre-node cre@prod-host \
  --erlang-cookie my-cookie
```

### Rollback Failed Import
```bash
./scripts/migration/spanner-import.sh \
  --enable-rollback /tmp/spanner-import/rollback_*.json
```

## Common Scenarios

### Scenario 1: Migrate Test Data
```bash
# Export from test Mnesia
./scripts/migration/mnesia-export.sh --node test@localhost --bucket test-backups

# Import to test Spanner
export GCP_PROJECT=test-project
./scripts/migration/spanner-import.sh --source-gcs gs://test-backups/mnesia-exports/*
```

### Scenario 2: Large Migration (>100k records)
```bash
./scripts/migration/spanner-import.sh \
  --source-gcs gs://prod-backups/mnesia-exports/latest \
  --batch-size 500 \
  --skip-validation  # Validate separately
```

### Scenario 3: Verify Import Before Cutover
```bash
# Dry-run to verify
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --dry-run

# Small batch test
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --tables workflow_cases \
  --batch-size 10

# Full import
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export
```

### Scenario 4: Safe Import with Rollback Readiness
```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --skip-rollback false  # Ensure rollback enabled

# If issues found, rollback
./scripts/migration/spanner-import.sh \
  --enable-rollback /tmp/spanner-import/rollback_20250211_120000_import.json
```

## Environment Variable Configuration

```bash
# Set defaults to avoid repetitive CLI flags
export GCP_PROJECT="my-project"
export SPANNER_INSTANCE="cre-spanner"
export SPANNER_DATABASE="cre-db"
export CRE_NODE_NAME="cre@localhost"
export BATCH_SIZE=100
export SPANNER_IMPORT_DIR="/var/log/spanner-import"

# Now simpler commands
./scripts/migration/spanner-import.sh --source-dir /tmp/export
./scripts/migration/spanner-import.sh --source-gcs gs://bucket/export
```

## Docker Workflow

```bash
# Start CRE node
docker run -d --name cre \
  -e CRE_NODE_NAME=cre@localhost \
  cre:0.3.0 foreground

# Run import in same container
docker exec cre \
  sh -c "scripts/migration/spanner-import.sh --source-dir /tmp/export"

# Or import container linked to CRE
docker run --rm \
  --network container:cre \
  -v /tmp/mnesia-export:/data:ro \
  cre:0.3.0 \
  sh -c "scripts/migration/spanner-import.sh --source-dir /data"
```

## Log Files Reference

```bash
# View import execution log
cat /tmp/spanner-import/import_20250211_120000_import.log

# View import statistics
jq . /tmp/spanner-import/import_plan_20250211_120000_import.json

# View rollback checkpoint
cat /tmp/spanner-import/rollback_20250211_120000_import.json
```

## Troubleshooting Quick Fixes

| Error | Fix |
|-------|-----|
| CRE node not found | `docker run -d -e CRE_NODE_NAME=cre@localhost cre:0.3.0` |
| Spanner instance not found | `gcloud spanner instances create cre-spanner --config=regional-us-central1 --nodes=1` |
| Invalid JSON | `jq . /tmp/mnesia-export/table.json \| head -20` |
| Permission denied | `chmod +x scripts/migration/spanner-import.sh` |
| GCS auth failed | `gcloud auth application-default login` |

## Exit Codes Quick Reference

| Code | Meaning | Action |
|------|---------|--------|
| 0 | Success | Done! |
| 2 | Invalid args | Check configuration |
| 3 | Spanner unreachable | Check instance/database |
| 4 | Import failed | Check CRE node & logs |
| 5 | Validation failed | Check data quality |
| 6 | Rollback failed | Manual intervention needed |

## Performance Tips

- **Fast import (500k+ records)**: Use `--batch-size 500`
- **Safe import (testing)**: Use `--batch-size 50`
- **Parallel imports**: Run multiple script instances
- **Monitor progress**: `tail -f /tmp/spanner-import/import_*.log`

## Important Notes

1. **Always dry-run first**: `--dry-run` is free
2. **Save rollback checkpoint**: It's created automatically
3. **Validate source**: `validate_json_files()` runs automatically
4. **Monitor Spanner**: Imports create Cloud Logging entries
5. **Keep logs**: Archive `/tmp/spanner-import/` for audit trail

## Verification Checklist

After import completes:

```bash
# ✓ Check import log for errors
cat /tmp/spanner-import/import_*.log | grep ERROR

# ✓ Verify record counts
gcloud spanner databases execute-sql cre-db \
  --instance=cre-spanner \
  --sql "SELECT COUNT(*) FROM workflow_cases"

# ✓ Check rollback checkpoint exists
ls -lh /tmp/spanner-import/rollback_*.json

# ✓ Review validation errors
jq '.validation_errors' /tmp/spanner-import/rollback_*.json

# ✓ Test rollback capability (if needed)
./scripts/migration/spanner-import.sh \
  --enable-rollback /tmp/spanner-import/rollback_*.json
```

## Common Parameter Combinations

```bash
# Development testing
-source-dir /tmp/export --tables workflow_cases --batch-size 10 --dry-run

# Production import with verification
--source-gcs gs://prod-backup/export --batch-size 100

# Large data migration
--source-gcs gs://backups/export --batch-size 500 --skip-validation

# Recovery/rollback
--enable-rollback /tmp/spanner-import/rollback_*.json

# Node-specific import
--source-dir /tmp/export --cre-node cre@prod-01 --erlang-cookie secret
```

## Integration with Pipeline

### GitLab CI Example
```yaml
spanner_import:
  stage: deploy
  script:
    - ./scripts/migration/spanner-import.sh
        --source-gcs gs://ci-backups/mnesia/$CI_COMMIT_SHA
        --project $GCP_PROJECT
        --instance $SPANNER_INSTANCE
        --database $SPANNER_DATABASE
  artifacts:
    paths:
      - /tmp/spanner-import/
  on_failure:
    - ./scripts/migration/spanner-import.sh
        --enable-rollback /tmp/spanner-import/rollback_*.json
```

### GitHub Actions Example
```yaml
- name: Spanner Import
  run: |
    ./scripts/migration/spanner-import.sh \
      --source-gcs gs://gh-backups/mnesia/${{ github.sha }} \
      --project ${{ secrets.GCP_PROJECT }} \
      --instance cre-spanner \
      --database cre-db
```

## Quick Benchmarks

| Dataset Size | Batch Size | Duration | Records/sec |
|--------------|-----------|----------|------------|
| 1,000 | 50 | 15s | ~67 |
| 10,000 | 100 | 120s | ~83 |
| 100,000 | 500 | 1200s | ~83 |
| 1,000,000 | 500 | 12000s | ~83 |

*Note: Includes validation overhead. Actual throughput depends on Spanner capacity and network.*

## Support Resources

- **Full Guide**: `/docs/gcp/SPANNER_IMPORT_GUIDE.md`
- **Enhancements**: `/docs/SPANNER_IMPORT_ENHANCEMENTS.md`
- **Script Help**: `./scripts/migration/spanner-import.sh --help`
- **Issues**: https://github.com/joergen7/cre/issues
