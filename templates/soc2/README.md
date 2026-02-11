# SOC 2 Tera Templates

Deterministic Tera templates that generate stable YAML and JSON outputs for SOC 2 compliance reporting from SPARQL query results.

## Overview

These templates process SOC 2 control hierarchy data from SPARQL queries and render them as structured, deterministic outputs suitable for auditor packages and compliance documentation.

### Key Features

- **Deterministic Output**: All outputs are fully deterministic with stable key ordering
- **No Timestamps**: Generated content contains no runtime timestamps or volatile values
- **Stable Sorting**: Controls, evidence, and validators are sorted alphabetically for consistency
- **Unique Filtering**: Deduplication using Tera filters to handle SPARQL result multiplicities
- **Nested Hierarchies**: Proper nesting structure: Suite → Category → Control → Evidence + Validators

## Templates

### 1. soc2_auditor_pack.tera

**Purpose**: Generate complete SOC 2 auditor pack in JSON format

**Output Format**: JSON
**Use Case**: Auditor delivery packages, compliance evidence bundles

**Input Expectations**:
```
SPARQL result columns:
  - suiteId: String (SOC 2 suite identifier, e.g., "CC6.1")
  - suiteName: String (display name)
  - tscId: String (Trust Service Criteria category identifier)
  - tscLabel: String (category label)
  - controlId: String (control identifier)
  - controlTitle: String (control title)
  - controlDescription: String (control description)
  - validatorId: String (validator module identifier)
  - evidenceId: String (evidence artifact identifier)
  - evidencePath: String (path to evidence file)
  - evidenceFormat: String (format: "json", "yaml", "text", etc.)
  - evidenceIntegrity: String (integrity check: "sha256", "md5", etc.)
  - retentionDays: Integer (optional, retention period in days)
```

**Output Structure**:
```json
{
  "auditor_pack": {
    "version": "1.0.0",
    "format": "soc2-control-matrix",
    "suites": [
      {
        "suiteId": "CC6.1",
        "suiteName": "Logical and Physical Access Controls",
        "categories": [
          {
            "categoryId": "CC6",
            "categoryLabel": "Logical Access Controls",
            "controls": [
              {
                "controlId": "CC6.1",
                "controlTitle": "Logical and Physical Access",
                "controlDescription": "...",
                "evidence": [
                  {
                    "evidenceId": "...",
                    "evidencePath": "...",
                    "evidenceFormat": "json",
                    "evidenceIntegrity": "sha256",
                    "retentionDays": 2555
                  }
                ],
                "validators": [
                  {
                    "validatorId": "access_control_validator"
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }
}
```

### 2. soc2_control_matrix_v2.tera

**Purpose**: Generate SOC 2 control matrix in YAML format

**Output Format**: YAML
**Use Case**: Configuration files, control mappings, compliance documentation

**Output Structure**:
```yaml
soc2_control_matrix:
  format_version: "2.0.0"
  compliance_framework: "SOC 2"
  suites:
    CC6.1:
      suite_name: "Logical and Physical Access Controls"
      categories:
        CC6:
          category_label: "Logical Access Controls"
          controls:
            CC6.1:
              title: "Logical and Physical Access"
              description: "..."
              evidence:
                - evidence_id: "..."
                  format: "json"
                  integrity: "sha256"
                  path: "..."
                  retention_days: 2555
              validators:
                - "access_control_validator"
```

### 3. soc2_customer_auditor_pack.tera

**Purpose**: Generate customer-specific SOC 2 auditor pack in JSON format

**Output Format**: JSON
**Use Case**: Multi-tenant deployments, customer-specific compliance packages

**Input Expectations**: Same as soc2_auditor_pack.tera, plus:
```
  - customerId: String (customer identifier)
  - customerName: String (customer name)
```

**Output Structure**:
```json
{
  "auditor_pack": {
    "version": "1.0.0",
    "format": "soc2-customer-control-matrix",
    "customers": [
      {
        "customerId": "acme-corp",
        "customerName": "ACME Corporation",
        "suites": [
          {
            "suiteId": "CC6.1",
            "suiteName": "...",
            "categories": [...]
          }
        ]
      }
    ]
  }
}
```

### 4. soc2_customer_control_matrix.tera

**Purpose**: Generate customer-specific SOC 2 control matrix in YAML format

**Output Format**: YAML
**Use Case**: Customer onboarding, multi-tenant control mapping

**Output Structure**:
```yaml
soc2_customer_control_matrix:
  format_version: "2.0.0"
  compliance_framework: "SOC 2"
  customers:
    acme-corp:
      customer_name: "ACME Corporation"
      suites:
        CC6.1:
          suite_name: "..."
          categories:
            CC6:
              category_label: "..."
              controls:
                CC6.1:
                  title: "..."
                  description: "..."
                  evidence: [...]
                  validators: [...]
```

## Determinism Guarantees

These templates ensure deterministic, reproducible output:

1. **Stable Ordering**:
   - Suites sorted by `suiteId`
   - Categories sorted by `tscId`
   - Controls sorted by `controlId`
   - Evidence sorted by `evidencePath`
   - Validators sorted alphabetically

2. **No Timestamps**: No `datetime.now()`, `now_utc()`, or any time-based values

3. **Unique Filtering**: Uses Tera's `unique(attribute=...)` filter to handle SPARQL result multiplicities

4. **Consistent Key Names**: All keys use consistent naming (camelCase for JSON, snake_case for YAML)

## Usage with ggen

Add to `ggen.toml`:

```toml
# Generate SOC 2 auditor pack (JSON)
[[generation.rules]]
name = "soc2-auditor-pack"
query = { file = "sparql/soc2/extract_control_closure.sparql" }
template = { file = "templates/soc2/soc2_auditor_pack.tera" }
output_file = "lib/soc2/soc2.auditor_pack.json"
mode = "Overwrite"

# Generate SOC 2 control matrix (YAML)
[[generation.rules]]
name = "soc2-control-matrix"
query = { file = "sparql/soc2/extract_control_closure.sparql" }
template = { file = "templates/soc2/soc2_control_matrix_v2.tera" }
output_file = "lib/soc2/soc2.control_matrix.yaml"
mode = "Overwrite"

# Generate customer-specific auditor pack
[[generation.rules]]
name = "soc2-customer-auditor-pack"
query = { file = "sparql/soc2/extract_customer_control_closure.sparql" }
template = { file = "templates/soc2/soc2_customer_auditor_pack.tera" }
output_pattern = "lib/soc2/customers/{customerId}.auditor_pack.json"
mode = "OverwriteAll"

# Generate customer-specific control matrix
[[generation.rules]]
name = "soc2-customer-control-matrix"
query = { file = "sparql/soc2/extract_customer_control_closure.sparql" }
template = { file = "templates/soc2/soc2_customer_control_matrix.tera" }
output_pattern = "lib/soc2/customers/{customerId}.control_matrix.yaml"
mode = "OverwriteAll"
```

## Template Filters Used

- `unique(attribute="field")` - Deduplicate rows by field
- `sort(attribute="field")` - Sort rows by field alphabetically
- `where(attribute="field", value="value")` - Filter rows by exact match
- `map(attribute="field")` - Extract field from each row

## Testing Determinism

To verify templates produce deterministic output:

```bash
# Generate twice and compare
ggen generate --rule soc2-auditor-pack
cp lib/soc2/soc2.auditor_pack.json lib/soc2/soc2.auditor_pack.json.v1
ggen generate --rule soc2-auditor-pack
diff lib/soc2/soc2.auditor_pack.json lib/soc2/soc2.auditor_pack.json.v1
# Should have no differences
```

## Files

- `soc2_auditor_pack.tera` (2.7 KB) - JSON auditor pack template
- `soc2_control_matrix_v2.tera` (2.1 KB) - YAML control matrix template
- `soc2_customer_auditor_pack.tera` (3.4 KB) - JSON customer-specific auditor pack
- `soc2_customer_control_matrix.tera` (2.6 KB) - YAML customer-specific control matrix
- `README.md` (this file) - Documentation

## References

- SPARQL Queries: `sparql/soc2/extract_control_closure.sparql`, `sparql/soc2/extract_customer_control_closure.sparql`
- Tera Template Documentation: https://tera.netlify.app/
- SOC 2 Framework: https://www.aicpa.org/interestareas/informationsystems/pages/default.aspx
