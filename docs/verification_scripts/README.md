# CRE Verification Scripts

Utility scripts for validating CRE (Common Runtime Environment) artifacts including BCD documents, YAML specifications, pattern coverage, receipt chains, and workflow replay.

## Overview

These scripts provide automated validation for Generative Analysis artifacts in the CRE system. Each script is self-contained (using only Python standard library) and outputs structured results in text, JSON, or TAP format.

### Available Scripts

| Script | Purpose | Input |
|--------|---------|-------|
| `validate_bcd.py` | Validate BCD document structure | BCD markdown files |
| `validate_yaml.py` | Check YAML 0.2 conformance | YAWL workflow YAML files |
| `check_pattern_coverage.py` | Verify all 43 patterns used | Source directory |
| `verify_receipts.py` | Analyze receipt chain completeness | Receipt log files |
| `replay_test.py` | Replay workflow from receipts | Receipt log + expected state |

## Installation

No external dependencies required - scripts use Python 3 standard library only.

```bash
# Make scripts executable (optional)
chmod +x docs/verification_scripts/*.py
```

## Usage

### validate_bcd.py

Parse and validate BCD (Business Context Document) structure against the CRE template.

```bash
# Validate a single BCD file
python validate_bcd.py path/to/bcd.md

# Validate all BCDs in a directory
python validate_bcd.py --directory docs/

# JSON output for CI/automation
python validate_bcd.py --json path/to/bcd.md

# Verbose output with all passing checks
python validate_bcd.py --verbose path/to/bcd.md
```

**What it checks:**
- Document header (ID, version, status, dates)
- Required sections (Executive Summary, Roles, Workflow Context, etc.)
- Role definition completeness
- Pattern context and inventory
- Traceability matrices
- Decision schemas
- Placeholder text (TODO, TBD)

**Exit codes:** 0 = pass, 1 = validation errors, 2 = file errors

### validate_yaml.py

Check YAML workflow specifications for YAWL YAML 0.2 conformance.

```bash
# Validate a single YAML file
python validate_yaml.py path/to/workflow.yaml

# Validate all YAML files in directory
python validate_yaml.py --directory src/yaws/

# JSON output
python validate_yaml.py --json path/to/workflow.yaml

# TAP output for test harnesses
python validate_yaml.py --tap path/to/workflow.yaml
```

**What it checks:**
- YAML version (must be 0.2)
- Schema version (recommends 2.1)
- Required top-level keys (yawl_yaml_version, specificationSet)
- SpecificationSet structure (uri, rootNet, nets)
- Net definitions (nodes, flows)
- Pattern instance validity (P1-P43)
- Node kind validity (task, condition, inputCondition, outputCondition)

**Exit codes:** 0 = pass, 1 = validation errors, 2 = file errors

### check_pattern_coverage.py

Verify that all 43 YAWL workflow control patterns are implemented and used.

```bash
# Check pattern coverage in source
python check_pattern_coverage.py src/

# JSON output
python check_pattern_coverage.py --json src/

# TAP output
python check_pattern_coverage.py --tap src/

# Specify pattern registry location
python check_pattern_coverage.py --pattern-module src/core/yawl_pattern_registry.erl
```

**What it checks:**
- Pattern registry entries (from yawl_pattern_registry.erl)
- Pattern module implementations (from src/patterns/)
- Pattern references across codebase
- Coverage percentage

**The 43 YAWL Patterns:**
- **P1-P11**: Basic Control Flow (Sequence, Parallel Split, Synchronization, etc.)
- **P12-P15**: Multiple Instance Patterns
- **P16-P18**: State-based Patterns
- **P19-P20**: Cancellation Patterns
- **P21-P22**: Advanced Control Flow
- **P23-P24**: Trigger Patterns
- **P25-P27**: Cancellation/Completion
- **P28-P33**: Advanced Synchronization
- **P34-P36**: Multiple Instance Partial Join
- **P37-P38**: Advanced Merge Patterns
- **P39-P40**: Concurrency Patterns
- **P41-P42**: Thread Patterns
- **P43**: Explicit Termination

**Exit codes:** 0 = all patterns present, 1 = missing patterns, 2 = file errors

### verify_receipts.py

Analyze receipt chains for completeness and consistency.

```bash
# Verify receipt log
python verify_receipts.py path/to/receipts.log

# Scan directory for log files
python verify_receipts.py --directory /var/log/cre/

# JSON output
python verify_receipts.py --json path/to/receipts.log

# TAP output
python verify_receipts.py --tap path/to/receipts.log
```

**What it checks:**
- Receipt structure (before_hash, after_hash, move, ts)
- Hash chain continuity (after_hash[i] == before_hash[i+1])
- Timestamp monotonicity
- Duplicate receipts
- Unique transitions executed

**Receipt format expected:**
```erlang
#{
    before_hash => <<...>>,
    after_hash => <<...>>,
    move => #{trsn => transition_name, mode => #{}, produce => #{}},
    ts => 1234567890
}
```

**Exit codes:** 0 = chain valid, 1 = validation errors, 2 = file errors

### replay_test.py

Replay a workflow execution from receipts and compare against expected outcome.

```bash
# Replay and compare against expected state
python replay_test.py receipts.log expected_state.json

# Generate expected state template from receipts
python replay_test.py --generate-expected receipts.log

# JSON output
python replay_test.py --json receipts.log expected_state.json

# TAP output
python replay_test.py --tap receipts.log expected_state.json
```

**Expected state format:**
```json
{
  "marking": {
    "place1": ["token1", "token2"],
    "place2": []
  },
  "terminated": true,
  "final_transition": "end_task",
  "min_transitions": 5,
  "max_transitions": 10,
  "required_places": ["final_place"],
  "forbidden_places": ["error_place"]
}
```

**What it checks:**
- Termination status match
- Final transition match
- Transition count within range
- Required places present in marking
- Forbidden places absent in marking
- Token presence in specified places

**Exit codes:** 0 = matches expected, 1 = differs, 2 = file errors

## Output Formats

### Text Output (default)

Human-readable with color-coded status indicators:
```
Summary: 10 passed, 2 warnings, 0 failed

PASSED CHECKS:
  [PASS] HDR_001: Document header delimiter found
  [PASS] HDR_002: Document ID format valid

WARNINGS:
  [WARN] QLT_001: Found 2 placeholder(s)
```

### JSON Output

Structured JSON for automation:
```json
{
  "file": "path/to/file.md",
  "summary": {
    "total": 15,
    "passed": 13,
    "failed": 0,
    "warnings": 2,
    "overall": "PASS"
  },
  "results": [...]
}
```

### TAP Output

Test Anything Protocol format for test harnesses:
```
TAP version 13
1..1
ok 1 - Receipt chain: receipts.log
# Coverage: 100.0%
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Validate CRE Artifacts

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-python@v4
        with:
          python-version: '3.x'

      - name: Validate BCDs
        run: |
          python docs/verification_scripts/validate_bcd.py \
            --directory docs/

      - name: Check Pattern Coverage
        run: |
          python docs/verification_scripts/check_pattern_coverage.py \
            --tap src/

      - name: Verify Receipts
        run: |
          python docs/verification_scripts/verify_receipts.py \
            --directory test/data/
```

### Makefile Integration

```makefile
.PHONY: validate-bcd validate-yaml check-patterns verify-receipts

validate-bcd:
	python docs/verification_scripts/validate_bcd.py --directory docs/

validate-yaml:
	python docs/verification_scripts/validate_yaml.py --directory src/yaws/

check-patterns:
	python docs/verification_scripts/check_pattern_coverage.py src/

verify-receipts:
	python docs/verification_scripts/verify_receipts.py --directory test/logs/

validate-all: validate-bcd validate-yaml check-patterns verify-receipts
```

## Error Handling

All scripts follow these conventions:

1. **Exit code 0**: All checks passed
2. **Exit code 1**: Validation errors found
3. **Exit code 2**: File access or parsing errors

Error messages are written to stderr:
```
Error: File not found: path/to/file.md
Error: Could not parse YAML: invalid syntax at line 15
```

## Development

### Adding New Checks

To add a new validation check:

1. Define a new `check_id` following the pattern:
   - `HDR_*`: Header checks
   - `SEC_*`: Section checks
   - `PAT_*`: Pattern checks
   - `QLT_*`: Quality checks

2. Add the check method:
```python
def _validate_new_thing(self):
    """Validate new thing."""
    if condition:
        self.report.add_result(
            "NEW_001",
            "Description of check",
            CheckStatus.PASS,
            "Additional details",
            "Location reference"
        )
```

3. Call from main `validate()` method

### Testing Scripts

Test scripts locally before committing:

```bash
# Run with verbose output
python validate_bcd.py --verbose docs/bcd_templates.md

# Generate JSON for inspection
python check_pattern_coverage.py --json src/ | jq .

# Use TAP output for test harness integration
python validate_yaml.py --tap src/yaws/*.yaml | prove -e cat -
```

## Troubleshooting

### "No external dependencies" requirement

Scripts must not import modules outside Python standard library. Use:
- `json` for JSON parsing
- `re` for regex
- `pathlib` for file paths
- `dataclasses` for structured data
- `enum` for enumerations

### YAML parsing without PyYAML

The `validate_yaml.py` script includes a fallback simple YAML parser when PyYAML is not available. For full YAML feature support, PyYAML is recommended but not required.

### Erlang term parsing

Receipt parsing is simplified and looks for key patterns. For production use with complex Erlang terms, consider integrating a proper Erlang term parser.

## See Also

- [CRE Documentation](../README.md)
- [BCD Templates](../bcd_templates.md)
- [YAWL Pattern Reference](../patterns/README.md)
- [Receipt Audit Log Module](../pnet/pnet_receipt.erl)
