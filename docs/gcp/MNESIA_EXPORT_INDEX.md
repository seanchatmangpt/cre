# Mnesia Export to Spanner - Complete Documentation Index

## Quick Navigation

This index provides a roadmap to all documentation and resources for the enhanced Mnesia export script.

## Documents in This Suite

### 1. **MNESIA_EXPORT_QUICKSTART.md** ⭐ START HERE
**Best for**: Getting started quickly
- One-minute setup guide
- Common task examples
- Validation checklist
- Troubleshooting quick links
- 200 lines, 5-minute read

**Key Sections**:
- TL;DR commands
- Output interpretation
- Common tasks
- Validation checklist
- Performance tips

**When to read**: Before running the script for the first time

### 2. **MNESIA_EXPORT_GUIDE.md** 📖 COMPREHENSIVE REFERENCE
**Best for**: Complete understanding and troubleshooting
- Full feature documentation
- Table mappings and schema details
- Environment variable reference
- Performance tuning guide
- Integration with Spanner import
- Troubleshooting section
- 450+ lines, 20-minute read

**Key Sections**:
- Overview and features (schema extraction, validation, types)
- Table mappings (Mnesia ↔ Spanner)
- Usage examples (basic to advanced)
- Output file documentation
- Environment variables
- Error handling and recovery
- Performance tuning
- Integration with spanner-import.sh
- Best practices

**When to read**: When you need detailed information or troubleshooting

### 3. **MNESIA_EXPORT_ENHANCEMENTS.md** 🔧 TECHNICAL DETAILS
**Best for**: Understanding implementation and architecture
- Feature comparison (v1.0.0 vs v2.0.0)
- Implementation details for each feature
- Code metrics and structure
- Testing checklist
- Integration points
- Future roadmap
- 400+ lines, 15-minute read

**Key Sections**:
- Overview of enhancements
- Detailed feature explanations
- Schema extraction mechanism
- Data integrity validation implementation
- Spanner type conversion system
- File structure changes
- Script metrics and complexity
- Integration with gen_pnet and GCP
- Migration strategy (phases)
- Backward compatibility

**When to read**: When understanding architectural decisions or planning extensions

### 4. **MNESIA_EXPORT_ENHANCEMENT_SUMMARY.md** 📋 PROJECT SUMMARY
**Best for**: Project overview and stakeholder communication
- High-level feature summary
- File modifications list
- Implementation details
- Usage examples
- Testing and validation status
- Quality assurance checklist
- 450+ lines, 20-minute read

**When to read**: For project status, implementation overview, or handoff documentation

## Script Reference

**Main Script**: `scripts/migration/mnesia-export.sh`

### Quick Reference Card

```bash
# Basic usage
./scripts/migration/mnesia-export.sh

# With options
./scripts/migration/mnesia-export.sh --compress --validate

# Help
./scripts/migration/mnesia-export.sh --help

# Dry run
./scripts/migration/mnesia-export.sh --dry-run

# Docker
docker run --rm -v $(pwd):/work -w /work \
  cre:0.3.0 ./scripts/migration/mnesia-export.sh
```

### Key Options

| Option | Purpose | Example |
|--------|---------|---------|
| `--node` | Erlang node name | `--node cre@prod-01` |
| `--bucket` | GCS bucket | `--bucket cre-backups` |
| `--output-dir` | Export location | `--output-dir /tmp/export` |
| `--tables` | Specific tables | `--tables workflow_cases` |
| `--validate` | Enable validation | `--validate` |
| `--no-validate` | Disable validation | `--no-validate` |
| `--compress` | Enable compression | `--compress` |
| `--format` | Export format | `--format jsonl` |
| `--dry-run` | Preview mode | `--dry-run` |
| `--help` | Show help | `--help` |

## Output Files

All files generated in `/tmp/mnesia-export/` (default):

| File | Purpose | Size | Type |
|------|---------|------|------|
| `schema.json` | Table definitions | <10KB | JSON |
| `<table>.json` | Table data | Variable | JSON Array |
| `validation_report.json` | Validation results | <5KB | JSON |
| `export_metadata.json` | Export metadata | <5KB | JSON |
| `SHA256SUMS` | File checksums | <1KB | Text |
| `export.log` | Operation log | <10KB | Text |

## Use Case Guides

### Scenario 1: First Time Setup

**Goal**: Export Mnesia tables to test Spanner import

1. Read: `MNESIA_EXPORT_QUICKSTART.md`
2. Run: `./scripts/migration/mnesia-export.sh --dry-run`
3. Review: Output files structure
4. Execute: `./scripts/migration/mnesia-export.sh`
5. Verify: `validation_report.json` shows all checks passed

### Scenario 2: Production Migration

**Goal**: Migrate production Mnesia to Spanner with full validation

1. Read: `MNESIA_EXPORT_GUIDE.md` (full guide)
2. Plan: Review performance tuning section
3. Configure: Set environment variables for prod node
4. Execute: `./scripts/migration/mnesia-export.sh --validate --compress`
5. Review: Validation report and metadata
6. Import: Use `spanner-import.sh` with exported data
7. Monitor: Track import progress via Cloud Console

### Scenario 3: Performance Optimization

**Goal**: Export large dataset (> 1M records) efficiently

1. Read: `MNESIA_EXPORT_GUIDE.md` (Performance Tuning section)
2. Options:
   - Disable validation: `--no-validate`
   - Use compression: `--compress`
   - Use JSONL format: `--format jsonl`
3. Execute: `./scripts/migration/mnesia-export.sh --no-validate --compress --format jsonl`
4. Monitor: Check throughput and disk usage

### Scenario 4: Troubleshooting Issues

**Goal**: Resolve export or validation failures

1. Check: Run `./scripts/migration/mnesia-export.sh --help`
2. Read: `MNESIA_EXPORT_GUIDE.md` (Troubleshooting section)
3. Debug: Check exit code and error messages
4. Review: `validation_report.json` for specific failures
5. Retry: With appropriate options based on error

### Scenario 5: Understanding Implementation

**Goal**: Understand how schema extraction and validation work

1. Read: `MNESIA_EXPORT_ENHANCEMENTS.md` (Feature details)
2. Review: Relevant sections:
   - Schema Extraction mechanism
   - Data Integrity Validation
   - Spanner Type Conversion
3. Examine: Enhanced Erlang export script
4. Study: Validation logic implementation

## Decision Tree

```
What do you want to do?

├─ Get started quickly
│  └─ Read: MNESIA_EXPORT_QUICKSTART.md
│
├─ Run the export
│  ├─ Basic: ./scripts/migration/mnesia-export.sh
│  └─ Advanced: See MNESIA_EXPORT_GUIDE.md for options
│
├─ Troubleshoot issues
│  └─ Read: MNESIA_EXPORT_GUIDE.md (Troubleshooting)
│
├─ Optimize performance
│  └─ Read: MNESIA_EXPORT_GUIDE.md (Performance Tuning)
│
├─ Understand implementation
│  └─ Read: MNESIA_EXPORT_ENHANCEMENTS.md
│
├─ Get project status
│  └─ Read: MNESIA_EXPORT_ENHANCEMENT_SUMMARY.md
│
└─ Integrate with Spanner import
   └─ Read: MNESIA_EXPORT_GUIDE.md (Integration section)
```

## File Structure

```
/home/user/cre/
├── scripts/migration/
│   └── mnesia-export.sh             ← Main script (enhanced v2.0.0)
├── docs/gcp/
│   ├── MNESIA_EXPORT_INDEX.md       ← This file
│   ├── MNESIA_EXPORT_QUICKSTART.md  ← Start here (quick)
│   ├── MNESIA_EXPORT_GUIDE.md       ← Full reference
│   ├── MNESIA_EXPORT_ENHANCEMENTS.md ← Technical details
│   └── GCP_MARKETPLACE_READINESS.md ← Overall GCP deployment
└── MNESIA_EXPORT_ENHANCEMENT_SUMMARY.md ← Project summary
```

## Support Resources

### Getting Help

1. **Quick Questions**: Check `MNESIA_EXPORT_QUICKSTART.md`
2. **Detailed Help**: Consult `MNESIA_EXPORT_GUIDE.md`
3. **Technical Details**: Review `MNESIA_EXPORT_ENHANCEMENTS.md`
4. **Script Help**: Run `./scripts/migration/mnesia-export.sh --help`

### Debugging

1. Enable dry-run: `--dry-run`
2. Check validation: `jq . validation_report.json`
3. Review logs: `cat export.log`
4. Check exit code: `echo $?`

### Common Issues

| Issue | Solution | Docs |
|-------|----------|------|
| Mnesia connection fails | Check node name and cookie | GUIDE: Troubleshooting |
| Validation errors | Review validation_report.json | GUIDE: Validation section |
| Slow export | Disable validation, use compression | GUIDE: Performance |
| GCS upload fails | Check credentials and bucket | GUIDE: Troubleshooting |
| Type conversion issues | Check schema.json for format | ENHANCEMENTS: Type Conversion |

## Feature Matrix

| Feature | Quick Start | Full Guide | Enhancements | Summary |
|---------|:-----------:|:----------:|:------------:|:-------:|
| Basic usage | ✓ | ✓ | - | ✓ |
| Schema extraction | ✓ | ✓ | ✓ | ✓ |
| Validation | ✓ | ✓ | ✓ | ✓ |
| Type conversion | - | ✓ | ✓ | ✓ |
| Multiple formats | ✓ | ✓ | ✓ | ✓ |
| Compression | ✓ | ✓ | ✓ | ✓ |
| Docker support | ✓ | ✓ | ✓ | ✓ |
| Error handling | ✓ | ✓ | ✓ | ✓ |
| Performance tuning | - | ✓ | - | - |
| Integration guide | - | ✓ | - | - |
| Troubleshooting | ✓ | ✓ | - | - |
| Implementation details | - | - | ✓ | - |

## Key Metrics

**Documentation**:
- Total lines: 1500+
- Number of documents: 4
- Examples provided: 20+
- Troubleshooting scenarios: 10+

**Script**:
- Total lines: 1,071
- Functions: 23
- Exit codes: 8
- Validation checks: 6
- Supported formats: 3

## Version Information

| Component | Version | Date |
|-----------|---------|------|
| Script | 2.0.0 | 2025-02-11 |
| Documentation | 1.0.0 | 2025-02-11 |
| Compatibility | OTP 28+ | Current |

## Next Steps

1. **New users**: Start with `MNESIA_EXPORT_QUICKSTART.md`
2. **Production deployers**: Read `MNESIA_EXPORT_GUIDE.md`
3. **Developers**: Review `MNESIA_EXPORT_ENHANCEMENTS.md`
4. **Stakeholders**: Check `MNESIA_EXPORT_ENHANCEMENT_SUMMARY.md`

---

**Last Updated**: 2025-02-11
**Maintained By**: CRE Team
**License**: Apache 2.0
