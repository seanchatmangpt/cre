# Spanner Import Guide - v2.0.0

## Overview

The enhanced `spanner-import.sh` script provides a production-ready solution for importing Mnesia JSON exports into Google Cloud Spanner. It leverages the `spanner_adapter` Erlang module for atomic transactions, includes comprehensive validation, and supports rollback on errors.

**Key Features:**
- Erlang-based import using `spanner_adapter` module
- Comprehensive JSON validation before import
- Transactional integrity with Spanner
- Automatic rollback checkpoint creation on failure
- Detailed logging and statistics
- Support for dry-run mode for planning
- Batch processing for large datasets
- Health checks and connection validation

## Architecture

### Components

1. **spanner-import.sh** - Bash orchestration script
   - Argument parsing and validation
   - Source data preparation (local or GCS)
   - JSON file validation
   - Erlang integration via RPC
   - Rollback management

2. **spanner_adapter.erl** - Erlang module
   - Cloud Spanner connection pooling
   - CRUD operations for workflow data
   - Transaction support with atomic commits
   - Connection health checks
   - Fallback to Mnesia when unavailable

3. **Import Process Flow**
   ```
   Validate Args
        ↓
   Check CRE Node
        ↓
   Verify Spanner Resources
        ↓
   Prepare Source Data
        ↓
   Validate JSON Files
        ↓
   Generate Import Plan (Erlang)
        ↓
   Execute Import (RPC to spanner_adapter)
        ↓
   Validate Imported Data
        ↓
   Create Rollback Checkpoint
   ```

## Installation

### Prerequisites

1. **Docker Environment** (recommended)
   ```bash
   docker run -it --rm \
     -v $(pwd):/work \
     -w /work \
     cre:0.3.0 \
     sh
   ```

2. **Host Requirements**
   - Bash 4.0+
   - `jq` for JSON processing
   - `gcloud` CLI with Spanner component
   - Active GCP credentials
   - Erlang/OTP 28+ (for node connectivity)

3. **CRE Node Requirements**
   - Running CRE node with `spanner_adapter` module loaded
   - GCP credentials configured for Spanner access
   - Network access from import script host

### Setup

```bash
# Ensure script is executable
chmod +x scripts/migration/spanner-import.sh

# Set up environment variables (optional)
export GCP_PROJECT="my-project"
export SPANNER_INSTANCE="cre-spanner"
export SPANNER_DATABASE="cre-db"
export CRE_NODE_NAME="cre@localhost"
```

## Usage

### Basic Import from Local Directory

```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --project my-project \
  --instance cre-spanner \
  --database cre-db
```

### Import from GCS Bucket

```bash
./scripts/migration/spanner-import.sh \
  --source-gcs gs://my-bucket/mnesia-exports/export_20250211_120000 \
  --project my-project \
  --instance cre-spanner \
  --database cre-db
```

### Import Specific Tables Only

```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --tables workflow_cases,work_items \
  --batch-size 50
```

### Dry-Run Mode (Preview Changes)

```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --dry-run
```

Output shows what would be imported without making changes:
```
[INFO] [DRY-RUN] Would check CRE node: cre@localhost
[INFO] [DRY-RUN] Would verify Spanner instance: projects/my-project/instances/cre-spanner
[INFO] [DRY-RUN] Would import data to Spanner
```

### With Authentication

```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --cre-node cre@prod-node \
  --erlang-cookie my-secret-cookie
```

### Skip Validation (For Large Imports)

```bash
./scripts/migration/spanner-import.sh \
  --source-dir /tmp/mnesia-export \
  --skip-validation \
  --batch-size 200
```

### Rollback Failed Import

If an import fails, a rollback checkpoint is automatically created:

```bash
# View checkpoint details
cat /tmp/spanner-import/rollback_20250211_120000_import.json

# Execute rollback
./scripts/migration/spanner-import.sh \
  --enable-rollback /tmp/spanner-import/rollback_20250211_120000_import.json
```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GCP_PROJECT` | (gcloud config) | GCP project ID |
| `SPANNER_INSTANCE` | `cre-spanner` | Spanner instance name |
| `SPANNER_DATABASE` | `cre-db` | Spanner database name |
| `CRE_NODE_NAME` | `cre@localhost` | CRE Erlang node name |
| `ERLANG_COOKIE` | (empty) | Erlang node cookie |
| `SPANNER_IMPORT_DIR` | `/tmp/spanner-import` | Working directory for logs/checkpoints |
| `BATCH_SIZE` | `100` | Records per batch |

### Command-Line Options

```
--project PROJECT              GCP project ID
--instance INSTANCE            Spanner instance name
--database DATABASE            Spanner database name
--source-dir DIR               Local directory with JSON exports
--source-gcs PATH              GCS path (gs://bucket/path)
--tables TABLES                Comma-separated table list (default: all)
--batch-size N                 Mutation batch size (1-1000, default: 100)
--cre-node NODE                CRE node name (default: cre@localhost)
--erlang-cookie COOKIE         Erlang authentication cookie
--skip-validation              Skip post-import validation
--skip-rollback                Don't create rollback checkpoint
--enable-rollback FILE         Execute rollback from checkpoint
--dry-run                      Preview changes without executing
--help                         Show help message
```

## Validation

### Pre-Import Validation

The script performs comprehensive validation before importing:

1. **Requirements Check**
   - Verifies presence of required tools (gcloud, jq)
   - Checks CRE node connectivity
   - Validates Erlang RPC capability

2. **Argument Validation**
   - Checks project ID, instance, database configuration
   - Validates source path/bucket accessibility
   - Confirms batch size within valid range
   - Validates CRE node name format

3. **JSON Validation**
   - Parses each exported JSON file
   - Verifies valid JSON structure
   - Counts records per table
   - Reports any malformed files

### Post-Import Validation

After import completes, validates data integrity:

1. **Record Count Verification**
   - Queries Spanner for actual record counts per table
   - Compares against import plan statistics
   - Reports any discrepancies

2. **Table Existence Check**
   - Verifies all target tables exist in Spanner
   - Checks for schema compatibility
   - Validates foreign key relationships

3. **Data Quality Checks**
   - Verifies primary key constraints
   - Checks timestamp validity
   - Reports any validation errors

## Rollback & Recovery

### Automatic Rollback Checkpoints

On any import failure, a JSON checkpoint is created:

```json
{
    "import_id": "20250211_120000_import",
    "timestamp": 1707576000,
    "project_id": "my-project",
    "instance": "cre-spanner",
    "database": "cre-db",
    "records_imported": 1500,
    "tables_imported": 3,
    "tables": ["workflow_cases", "work_items", "event_log"],
    "batch_size": 100,
    "validation_errors": 0,
    "import_log": "/tmp/spanner-import/import_20250211_120000_import.log"
}
```

### Manual Rollback Execution

```bash
# Execute rollback from checkpoint
./scripts/migration/spanner-import.sh \
  --enable-rollback /tmp/spanner-import/rollback_20250211_120000_import.json

# Rollback will:
# 1. Connect to spanner_adapter via RPC
# 2. Begin transaction
# 3. Delete imported records per table
# 4. Commit transaction
# 5. Report status
```

### Rollback Process

The rollback mechanism:

1. **Load Checkpoint** - Reads rollback metadata
2. **Verify Target** - Confirms target database matches
3. **Begin Transaction** - Starts atomic Spanner transaction
4. **Delete Records** - Removes records inserted during import
5. **Validate Rollback** - Verifies deletion succeeded
6. **Commit** - Finalizes rollback transaction
7. **Report Status** - Provides detailed rollback summary

## Logging

### Log File Locations

```
/tmp/spanner-import/import_20250211_120000_import.log    # Import execution log
/tmp/spanner-import/rollback_20250211_120000_import.json # Rollback checkpoint
```

### Log Content

Each import generates detailed logs:

```
[INFO] Starting Spanner import: spanner-import.sh v2.0.0
[INFO] Import ID: 20250211_120000_import
[INFO] Log file: /tmp/spanner-import/import_20250211_120000_import.log
[INFO] Configuration:
  Project:      my-project
  Instance:     cre-spanner
  Database:     cre-db
  Source:       /tmp/mnesia-export
  Tables:       all
  Batch Size:   100
  CRE Node:     cre@localhost
  Dry Run:      false
  Skip Valid:   false

[1/12] Checking CRE node connectivity
[SUCCESS] Connected to CRE node: cre@localhost

[2/12] Verifying Spanner instance
[SUCCESS] Spanner instance found: projects/my-project/instances/cre-spanner

[3/12] Verifying Spanner database
[SUCCESS] Spanner database found: projects/my-project/instances/cre-spanner/databases/cre-db

[4/12] Preparing source data
[SUCCESS] Found 4 data files

[5/12] Validating JSON export files
[INFO]   workflow_cases: 150 records
[INFO]   work_items: 820 records
[INFO]   event_log: 2100 records
[INFO]   checkpoints: 45 records
[SUCCESS] JSON validation: 4 valid, 0 invalid out of 4 files

[6/12] Generating import plan via Erlang
[SUCCESS] Import plan generated: /tmp/spanner-import/import_plan_20250211_120000_import.json
[INFO] Plan: 3115 records across 4 tables

[7/12] Executing Spanner import via RPC
[SUCCESS] Spanner import executed successfully

[8/12] Validating imported data in Spanner
[INFO]   workflow_cases: 150 records in Spanner
[INFO]   work_items: 820 records in Spanner
[INFO]   event_log: 2100 records in Spanner
[INFO]   checkpoints: 45 records in Spanner
[SUCCESS] Validation complete - all tables verified

[SUCCESS] Spanner import completed successfully!
[INFO] Tables imported: 4
[INFO] Records imported: 3115
[INFO] Validation errors: 0
[INFO] Rollback checkpoint: /tmp/spanner-import/rollback_20250211_120000_import.json
[INFO] Duration: 45s
```

## Exit Codes

| Code | Meaning | Action |
|------|---------|--------|
| 0 | Success | Import completed |
| 1 | General error | Check logs for details |
| 2 | Validation error | Fix configuration issues |
| 3 | Spanner connection error | Verify Spanner instance/database |
| 4 | Import failed | Check spanner_adapter and CRE node |
| 5 | Validation failed | Check data quality |
| 6 | Rollback error | Manual intervention may be needed |

## Troubleshooting

### Issue: CRE Node Not Found

```bash
# Error: CRE node not immediately available: cre@localhost

# Solution: Start CRE node first
docker run -d \
  -e CRE_NODE_NAME=cre@localhost \
  -p 9100-9200:9100-9200 \
  cre:0.3.0 foreground
```

### Issue: Spanner Instance Not Found

```bash
# Error: Spanner instance not found: projects/my-project/instances/cre-spanner

# Solution: Create instance
gcloud spanner instances create cre-spanner \
  --project=my-project \
  --config=regional-us-central1 \
  --nodes=1
```

### Issue: JSON Parse Errors

```bash
# Error: Invalid JSON in workflow_cases

# Solution: Validate export files
jq . /tmp/mnesia-export/workflow_cases.json | head -20
```

### Issue: Permission Denied

```bash
# Error: script permission denied

# Solution: Make executable
chmod +x scripts/migration/spanner-import.sh
```

### Issue: GCS Authentication Failed

```bash
# Error: GCS bucket not found or not accessible

# Solution: Authenticate with GCS
gcloud auth application-default login
gsutil ls gs://my-bucket/
```

## Performance Considerations

### Batch Size Tuning

- **Small (10-50)**: Better error recovery, slower throughput
- **Medium (100-200)**: Balanced performance/reliability (default)
- **Large (500-1000)**: Faster throughput, less recovery granularity

For typical deployments:
```bash
# High-volume import (>100k records)
--batch-size 500

# Standard import (10k-100k records)
--batch-size 100

# Small import (<10k records) or testing
--batch-size 50
```

### Memory Usage

Import memory consumption depends on batch size:
- Batch size N ≈ N × record_size × 3 (during processing)
- Typical record: ~1-2KB
- Batch of 100 records: ~300-600MB per batch

### Import Duration

Typical import rates:
- 100 records/sec (local SSD to Spanner)
- 50-100 records/sec (GCS to Spanner)
- Factor in validation: +20-30% overhead

Example: 10,000 records ≈ 100-200 seconds total

## Docker Usage (Recommended)

### Running Inside Container

```bash
# Start CRE container
docker run -d --name cre \
  -e CRE_NODE_NAME=cre@localhost \
  cre:0.3.0 foreground

# Start import container (shares network with CRE)
docker run -it --rm \
  --network container:cre \
  -v /tmp/mnesia-export:/data:ro \
  cre:0.3.0 \
  sh -c "scripts/migration/spanner-import.sh \
    --source-dir /data \
    --cre-node cre@localhost"
```

### Using Docker Compose

```yaml
version: '3.8'
services:
  cre:
    image: cre:0.3.0
    environment:
      CRE_NODE_NAME: cre@localhost
    ports:
      - "4142:4142"
      - "9100-9200:9100-9200"

  spanner-import:
    image: cre:0.3.0
    depends_on:
      - cre
    volumes:
      - /tmp/mnesia-export:/data:ro
    command: >
      scripts/migration/spanner-import.sh
      --source-dir /data
      --cre-node cre@cre
```

## Related Documentation

- [GCP Marketplace Readiness](GCP_MARKETPLACE_READINESS.md)
- [Mnesia Export Guide](../../../scripts/migration/mnesia-export.sh)
- [Spanner Adapter Reference](../../../src/db/spanner_adapter.erl)
- [Spanner Schema](../../../src/db/spanner_schema.sql)

## Support

For issues or questions:
1. Check logs in `/tmp/spanner-import/`
2. Run with `--dry-run` to preview
3. Review troubleshooting section above
4. Open issue on GitHub: https://github.com/joergen7/cre/issues
