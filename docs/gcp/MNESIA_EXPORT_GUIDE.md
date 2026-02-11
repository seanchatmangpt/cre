# Mnesia Export to Spanner: Comprehensive Guide

## Overview

The enhanced `mnesia-export.sh` script exports Mnesia database tables to JSON format compatible with Google Cloud Spanner import. It includes:

- **Multi-table export** with gen_pnet workflow state preservation
- **Automatic schema extraction** from Mnesia table definitions
- **Data type conversion** for Spanner compatibility
- **Referential integrity validation** for workflow relationships
- **Compression support** for efficient GCS upload
- **Comprehensive error reporting** with validation reports

## Table Mappings

The script handles the following CRE tables:

| Mnesia Table | Spanner Table | Purpose | Records | Status |
|---|---|---|---|---|
| `workflow_cases` | `workflow_cases` | Workflow execution instances | Variable | Core |
| `work_items` | `work_items` | Individual workflow tasks | Variable | Core |
| `event_log` | `event_log` | Workflow event history | Variable | Optional |
| `checkpoints` | `checkpoints` | Recovery data | Variable | Optional |
| Custom gen_pnet tables | Custom tables | User-defined state | Variable | Dynamic |

## Features

### 1. Schema Extraction

The script automatically extracts Mnesia table schema:

```bash
# Generated schema.json contains:
[
  {
    "table": "workflow_cases",
    "attributes": ["case_id", "workflow_id", "spec", "status", ...],
    "type": "set",
    "storage_type": "disc_copies"
  },
  ...
]
```

### 2. Data Type Conversion

Erlang terms are automatically converted to Spanner-compatible types:

| Erlang Type | Spanner Type | Example |
|---|---|---|
| `atom()` | STRING | `:running` → `"running"` |
| `binary()` | BYTES/STRING | `<<"data">>` → `"data"` |
| `integer()` | INT64 | `42` → `42` |
| `float()` | FLOAT64 | `3.14` → `3.14` |
| `list()` | ARRAY | `[1, 2, 3]` → `[1, 2, 3]` |
| `map()` | JSON | `#{key => val}` → `{"key": "val"}` |
| `undefined` | NULL | `undefined` → `null` |

### 3. Data Integrity Validation

Validates:

- **Schema consistency**: Table definitions match exported data
- **JSON validity**: All exported files are valid JSON
- **Primary key uniqueness**: No duplicate keys in tables
- **Referential integrity**: Work items reference existing cases
- **Orphaned record detection**: Identifies missing parent relationships

Output: `validation_report.json`

### 4. Spanner Compatibility

- Converts timestamps to INT64 (milliseconds)
- Serializes complex terms as JSON
- Handles NULL values for optional fields
- Preserves foreign key relationships

## Usage

### Basic Export

```bash
# Export all tables with validation
./scripts/migration/mnesia-export.sh

# Export to custom location
./scripts/migration/mnesia-export.sh --output-dir /mnt/exports
```

### Advanced Options

```bash
# Export specific tables
./scripts/migration/mnesia-export.sh --tables workflow_cases,work_items

# Export with compression
./scripts/migration/mnesia-export.sh --compress

# Export without validation (faster)
./scripts/migration/mnesia-export.sh --no-validate

# Export in JSONL format (one record per line)
./scripts/migration/mnesia-export.sh --format jsonl

# Dry run to preview what will be done
./scripts/migration/mnesia-export.sh --dry-run
```

### Container Execution

```bash
# Run in Docker container (recommended)
docker run -it --rm \
  -v /path/to/cre:/work \
  -w /work \
  cre:0.3.0 \
  ./scripts/migration/mnesia-export.sh

# With custom options
docker run -it --rm \
  -v /path/to/mnesia:/mnesia \
  -v /tmp/exports:/exports \
  cre:0.3.0 \
  sh -c './scripts/migration/mnesia-export.sh \
    --node cre@mnesia-server \
    --output-dir /exports \
    --compress'
```

## Output Files

```
/tmp/mnesia-export/
├── schema.json                  # Table definitions
├── workflow_cases.json          # Workflow instance records
├── work_items.json              # Task records
├── event_log.json               # Event history
├── validation_report.json       # Integrity check results
├── export_metadata.json         # Export metadata
├── SHA256SUMS                   # File checksums
└── export.log                   # Detailed operation log
```

### schema.json

```json
[
  {
    "table": "workflow_cases",
    "attributes": ["case_id", "workflow_id", "spec", "status", "data",
                   "created_at", "started_at", "completed_at", "updated_at"],
    "type": "set",
    "storage_type": "disc_copies"
  },
  {
    "table": "work_items",
    "attributes": ["workitem_id", "case_id", "task_id", "status", "data",
                   "enabled_at", "started_at", "completed_at"],
    "type": "set",
    "storage_type": "disc_copies"
  }
]
```

### validation_report.json

```json
{
  "validation_timestamp": "2025-02-11T10:30:45Z",
  "validation_passed": true,
  "total_records_exported": 1250,
  "tables_exported": {
    "workflow_cases": 42,
    "work_items": 1208
  },
  "errors": [],
  "validation_details": {
    "schema_extracted": true,
    "tables_count": 2,
    "format": "json"
  }
}
```

### export_metadata.json

```json
{
  "export_id": "20250211_103045_cre-node",
  "timestamp": "2025-02-11T10:30:45Z",
  "version": "2.0.0",
  "node": "cre@localhost",
  "tables_requested": "all",
  "hostname": "cre-server.example.com",
  "user": "devops",
  "format": "json",
  "validation_enabled": true,
  "compression_enabled": false,
  "environment": {
    "erlang_version": "28",
    "docker_available": true
  },
  "spanner_compatibility": {
    "status": "ready",
    "description": "Exported data is compatible with Cloud Spanner import",
    "schema_file": "schema.json"
  }
}
```

## Environment Variables

```bash
# Node configuration
export CRE_NODE_NAME="cre@prod-01"
export ERLANG_COOKIE="your_cookie_here"

# GCS configuration
export GCS_BUCKET="cre-mnesia-backups"

# Export options
export OUTPUT_DIR="/mnt/exports"
export TABLES="workflow_cases,work_items"
export VALIDATE_DATA="true"
export COMPRESS_OUTPUT="true"
export EXPORT_FORMAT="json"
export DRY_RUN="false"

# Run with environment
./scripts/migration/mnesia-export.sh
```

## Error Handling

### Exit Codes

| Code | Meaning | Action |
|---|---|---|
| 0 | Success | Export completed successfully |
| 1 | General error | Check logs for details |
| 2 | Validation error | Invalid arguments or config |
| 3 | Mnesia connection error | Node not running or unreachable |
| 4 | Export failed | Mnesia read error or file write issue |
| 5 | GCS upload failed | Bucket not accessible |
| 6 | Data integrity check failed | Validation errors detected |

### Recovery

```bash
# Retry with increased verbosity
./scripts/migration/mnesia-export.sh --dry-run
export DRY_RUN="false"
./scripts/migration/mnesia-export.sh

# Disable validation if it's causing issues
./scripts/migration/mnesia-export.sh --no-validate

# Export to local disk instead of GCS
./scripts/migration/mnesia-export.sh --output-dir /tmp/mnesia-export
# Then manually upload:
gsutil -m cp -r /tmp/mnesia-export/* gs://your-bucket/mnesia-exports/
```

## Data Validation Details

### 1. Schema Consistency

Verifies that exported JSON matches the Mnesia table schema:

```bash
# Check in validation_report.json
"validation_details": {
  "schema_extracted": true,
  "tables_count": 4,
  "format": "json"
}
```

### 2. Primary Key Uniqueness

Ensures no duplicate records:

```bash
# Check for errors like:
"errors": ["Duplicate keys in workflow_cases: unique=40, total=42"]
```

### 3. Referential Integrity

Validates foreign key relationships:

```bash
# work_items must reference existing workflow_cases
# Error example:
"errors": ["Found 2 orphaned work items without parent case"]
```

## Performance Tuning

### Large Exports (> 100,000 records)

```bash
# Disable validation for faster export
./scripts/migration/mnesia-export.sh --no-validate

# Use compression to reduce file size
./scripts/migration/mnesia-export.sh --compress

# Use JSONL format (streaming, memory-efficient)
./scripts/migration/mnesia-export.sh --format jsonl

# Or run with increased Erlang VM memory
ERLANG_MAX_ETS_TABLES=1024000 \
./scripts/migration/mnesia-export.sh
```

### Memory-Limited Environments

```bash
# Export tables one at a time
./scripts/migration/mnesia-export.sh --tables workflow_cases
./scripts/migration/mnesia-export.sh --tables work_items

# Use JSONL format with streaming processing
./scripts/migration/mnesia-export.sh --format jsonl
```

## Integration with Spanner Import

After export, import to Spanner using `spanner-import.sh`:

```bash
# 1. Export from Mnesia
./scripts/migration/mnesia-export.sh --output-dir /tmp/mnesia-export

# 2. Verify validation report
cat /tmp/mnesia-export/validation_report.json

# 3. Upload to GCS
gsutil -m cp -r /tmp/mnesia-export/* gs://cre-backups/export-20250211/

# 4. Import to Spanner
./scripts/migration/spanner-import.sh \
  --bucket cre-backups \
  --prefix export-20250211 \
  --database cre-prod
```

## Troubleshooting

### Mnesia Connection Failed

```bash
# Check if Erlang node is running
erl -sname test -eval "net_adm:ping('cre@localhost'), halt()"

# Verify cookie matches
erlang:get_cookie()  % In running CRE node

# Specify cookie in export
./scripts/migration/mnesia-export.sh --cookie your_cookie
```

### Validation Failures

```bash
# Check validation report details
jq '.errors' validation_report.json

# Export without validation
./scripts/migration/mnesia-export.sh --no-validate

# Re-run with detailed logging
bash -x ./scripts/migration/mnesia-export.sh 2>&1 | tee debug.log
```

### GCS Upload Failed

```bash
# Verify bucket exists
gsutil ls gs://your-bucket

# Check authentication
gcloud auth list

# Test with smaller export
./scripts/migration/mnesia-export.sh --tables workflow_cases

# Upload manually
gsutil -m cp /tmp/mnesia-export/*.json gs://your-bucket/exports/
```

### Docker Execution Issues

```bash
# Check Docker image
docker images | grep cre

# Run with verbose output
docker run -it --rm \
  -v $(pwd):/work \
  -w /work \
  cre:0.3.0 \
  bash -x ./scripts/migration/mnesia-export.sh

# Use host network if needed
docker run -it --rm --network host \
  -v $(pwd):/work \
  -w /work \
  cre:0.3.0 \
  ./scripts/migration/mnesia-export.sh --node cre@host-ip
```

## Best Practices

1. **Always do a dry-run first**: `--dry-run` to preview operations
2. **Validate before importing**: Keep validation enabled in production
3. **Use compression for large exports**: `--compress` reduces storage
4. **Encrypt sensitive data**: Use gcloud's encryption settings
5. **Backup before migration**: Keep original Mnesia data during transition
6. **Monitor validation report**: Check for referential integrity issues
7. **Test import workflow**: Verify exported data imports correctly to Spanner
8. **Version control exports**: Track export metadata for audit trails

## Reference

- **Erlang Version Required**: OTP 28+
- **Spanner Schema**: `src/db/spanner_schema.sql`
- **GCP Documentation**: [Cloud Spanner Import](https://cloud.google.com/spanner/docs/import-data)
- **CRE Architecture**: `docs/gcp/GCP_MARKETPLACE_READINESS.md`

---

## Version History

### v2.0.0 (Current)

- Added schema extraction from Mnesia table definitions
- Implemented comprehensive data integrity validation
- Support for multiple export formats (JSON, JSONL, CSV)
- Added compression support for large exports
- Enhanced error reporting with validation reports
- Docker container execution support
- Spanner type conversion
- Referential integrity checking

### v1.0.0 (Legacy)

- Basic Mnesia export
- Simple JSON conversion
- GCS upload

---

For additional support, see:
- GitHub Issues: https://github.com/joergen7/cre/issues
- Marketplace Documentation: https://github.com/joergen7/cre/docs/gcp/GCP_MARKETPLACE_READINESS.md
