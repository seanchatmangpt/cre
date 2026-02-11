# Mnesia Export to Spanner - Quick Start

## TL;DR

```bash
# Export all Mnesia tables to JSON with validation
cd /home/user/cre
./scripts/migration/mnesia-export.sh

# Export with options
./scripts/migration/mnesia-export.sh \
  --output-dir /tmp/exports \
  --compress \
  --tables workflow_cases,work_items

# Check results
cat /tmp/mnesia-export/validation_report.json
cat /tmp/mnesia-export/schema.json
```

## One-Minute Setup

```bash
# 1. Ensure Mnesia node is running
erl -sname test -eval "net_adm:ping('cre@localhost'), halt()"

# 2. Run export
./scripts/migration/mnesia-export.sh

# 3. Check output directory
ls -lh /tmp/mnesia-export/
```

## Output Interpretation

| File | Purpose | Action |
|---|---|---|
| `schema.json` | Table definitions | Review structure |
| `workflow_cases.json` | Case records | Contains workflow instances |
| `work_items.json` | Task records | Contains work item data |
| `validation_report.json` | Validation results | Check for `"validation_passed": true` |
| `export_metadata.json` | Export info | Review statistics |

## Common Tasks

### Export All Tables
```bash
./scripts/migration/mnesia-export.sh
```

### Export Specific Tables
```bash
./scripts/migration/mnesia-export.sh --tables workflow_cases,work_items
```

### Validate Without Exporting
```bash
./scripts/migration/mnesia-export.sh --dry-run
```

### Compress for Upload
```bash
./scripts/migration/mnesia-export.sh --compress
```

### Skip Validation (Fast)
```bash
./scripts/migration/mnesia-export.sh --no-validate
```

### Different Format
```bash
# JSONL (one record per line)
./scripts/migration/mnesia-export.sh --format jsonl

# CSV
./scripts/migration/mnesia-export.sh --format csv
```

### Run in Docker
```bash
docker run --rm -v $(pwd):/work -w /work \
  cre:0.3.0 ./scripts/migration/mnesia-export.sh
```

## Validation Checklist

After export, verify:

```bash
# 1. Validation passed?
jq '.validation_passed' /tmp/mnesia-export/validation_report.json

# 2. Schema extracted?
jq '.validation_details.schema_extracted' /tmp/mnesia-export/validation_report.json

# 3. No validation errors?
jq '.errors | length' /tmp/mnesia-export/validation_report.json

# 4. Record counts match?
jq '.tables_exported' /tmp/mnesia-export/validation_report.json

# 5. All files present?
ls -1 /tmp/mnesia-export/ | grep -E '\.(json|gz|txt)$'
```

## Troubleshooting

### "Mnesia connectivity check failed"
```bash
# Check node is running
erl -sname debug -eval "net_adm:ping('cre@localhost'), halt()"

# Specify correct node name
./scripts/migration/mnesia-export.sh --node cre@your-hostname
```

### "Data validation failed"
```bash
# Check what failed
jq '.errors' /tmp/mnesia-export/validation_report.json

# Export without validation
./scripts/migration/mnesia-export.sh --no-validate
```

### "GCS upload failed"
```bash
# Verify bucket exists
gsutil ls gs://your-bucket

# Export without uploading
./scripts/migration/mnesia-export.sh --output-dir /tmp/export

# Upload manually
gsutil -m cp -r /tmp/export/* gs://your-bucket/
```

## Performance Tips

| Task | Solution |
|---|---|
| Large export (> 100K) | Use `--no-validate` and `--compress` |
| Memory limited | Use `--format jsonl` (streaming) |
| Network limited | Use `--compress` then upload |
| Need speed | Disable validation: `--no-validate` |

## Next Steps: Import to Spanner

```bash
# After successful export, import to Spanner
./scripts/migration/spanner-import.sh \
  --source /tmp/mnesia-export \
  --database cre-prod \
  --instance my-spanner-instance
```

## Key Files

- **Script**: `/home/user/cre/scripts/migration/mnesia-export.sh`
- **Full Guide**: `/home/user/cre/docs/gcp/MNESIA_EXPORT_GUIDE.md`
- **Enhancements**: `/home/user/cre/docs/gcp/MNESIA_EXPORT_ENHANCEMENTS.md`
- **Schema**: `/home/user/cre/src/db/spanner_schema.sql`

## Success Indicators

✓ Export completes without errors
✓ `validation_report.json` shows `"validation_passed": true`
✓ `schema.json` contains all table definitions
✓ All table files are valid JSON
✓ Record counts match expectations
✓ No referential integrity errors

## Support

For detailed help:
```bash
./scripts/migration/mnesia-export.sh --help
```

For full documentation:
- See `MNESIA_EXPORT_GUIDE.md`
- See `MNESIA_EXPORT_ENHANCEMENTS.md`
- GitHub: https://github.com/joergen7/cre/issues
