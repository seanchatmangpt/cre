# SOC 2 Compliance Artifacts

This directory contains SOC 2 Trust Services Criteria compliance artifacts, automatically generated from ontology-driven specifications.

## Directory Structure

```
lib/soc2/
└── customers/
    ├── community_bank_ca.soc2.control_matrix.yaml
    ├── community_bank_ca.soc2.auditor_pack.json
    ├── credit_union_tx.soc2.control_matrix.yaml
    ├── credit_union_tx.soc2.auditor_pack.json
    ├── fintech_startup_de.soc2.control_matrix.yaml
    ├── fintech_startup_de.soc2.auditor_pack.json
    ├── megabank_ny.soc2.control_matrix.yaml
    ├── megabank_ny.soc2.auditor_pack.json
    ├── mortgage_lender_fl.soc2.control_matrix.yaml
    └── mortgage_lender_fl.soc2.auditor_pack.json
```

## Artifact Types

### Control Matrix YAML

**Files**: `{customer_id}.soc2.control_matrix.yaml`

Hierarchical YAML structure containing:
- Customer and suite metadata
- All applicable SOC 2 controls
- Grouped by Trust Services Categories (TSC)
- Each control lists:
  - Control ID and title
  - Description
  - Required validators
  - Evidence requirements with paths and retention periods

**Use Cases**:
- Audit planning and scope definition
- Control assessment documentation
- Evidence collection checklists
- Compliance documentation

**Example**:
```yaml
control_matrix:
  metadata:
    customer_id: megabank_ny
    customer_name: MegaBank (New York)
    suite_id: soc2_security_availability
    suite_name: SOC 2 Security + Availability
  suites:
  - suite_id: soc2_security_availability
    categories:
    - category_id: TSC_Security
      category_name: Security (TSC)
      controls:
      - control_id: CC6.1
        title: Authorization Controls
        description: Entity implements logical access controls...
        validators:
        - config_validator
        - zero_downtime_validator
        evidence:
        - evidence_id: build_receipt
          path: receipts/build.last.json
          format: json
          integrity: sha256_manifest
          retention_days: 2555
```

### Auditor Pack JSON

**Files**: `{customer_id}.soc2.auditor_pack.json`

Machine-readable JSON manifest containing:
- Suite evaluation scope
- Evidence manifest with file paths and retention
- Validator registry
- Control-to-evidence-validator mappings
- Receipt field locations

**Use Cases**:
- Automated evidence collection
- Validator configuration
- Evidence indexing
- Audit trail verification

**Example**:
```json
{
  "soc2_auditor_pack": {
    "version": "1.0.0",
    "customer_id": "community_bank_ca",
    "customer_name": "Community Bank (California)",
    "suites_evaluated": [
      {
        "suite_id": "soc2_security",
        "suite_name": "SOC 2 Security"
      }
    ],
    "evidence_manifest": {
      "manifest_file": "evidence/evidence.sha256",
      "integrity_mechanism": "sha256_manifest",
      "required_evidence": [
        {
          "evidence_id": "build_receipt",
          "path": "receipts/build.last.json",
          "format": "json",
          "integrity": "sha256_manifest",
          "retention_days": 2555
        }
      ]
    },
    "validator_registry": [
      "config_validator",
      "deterministic_generation_validator",
      "hot_upgrade_validator",
      "zero_downtime_validator"
    ],
    "control_evidence_mapping": [
      {
        "control_id": "CC6.1",
        "validator": "config_validator",
        "evidence": "build_receipt"
      }
    ]
  }
}
```

## Customers

| Customer ID | Customer Name | Suite | TSC Categories | Controls |
|---|---|---|---|---|
| **community_bank_ca** | Community Bank (California) | SOC 2 Security | Security | 8 |
| **credit_union_tx** | Credit Union (Texas) | SOC 2 Security + Privacy | Security, Privacy | 9 |
| **fintech_startup_de** | FinTech Startup (Delaware) | SOC 2 Security + Processing Integrity | Security, Processing Integrity | 9 |
| **megabank_ny** | MegaBank (New York) | SOC 2 Security + Availability | Security, Availability | 10 |
| **mortgage_lender_fl** | Mortgage Lender (Florida) | SOC 2 Security + Confidentiality | Security, Confidentiality | 9 |

## Generation

These artifacts are automatically generated from:

1. **SOC 2 Ontology** (`ontology/reg/soc2.ttl`)
   - Defines Trust Services Categories
   - Defines SOC 2 controls with validators and evidence
   - Defines composable suites

2. **Customer Ontology** (`ontology/customers.ttl`)
   - Defines customers with `cust:requiresSuite` property
   - Links customers to specific SOC 2 suites

### Generation Script

**Script**: `scripts/generate_customer_soc2_artifacts.py`

**Usage**:
```bash
cd examples/fortune5-fibo-lnctrl
python3 scripts/generate_customer_soc2_artifacts.py
```

**Features**:
- Parses RDF ontologies using rdflib
- Executes SPARQL queries to extract controls
- Generates deterministic artifacts (same input = same output)
- Creates both YAML and JSON formats
- Validates ontology structure

### Regenerating Artifacts

If you modify the ontologies (`customers.ttl` or `soc2.ttl`), regenerate the artifacts:

```bash
python3 scripts/generate_customer_soc2_artifacts.py
```

The script will:
1. Load both ontologies
2. Extract all customers with `requiresSuite`
3. For each customer, query all applicable controls
4. Generate fresh control matrix YAML files
5. Generate fresh auditor pack JSON files

## Integration with ggen

The `ggen.toml` configuration (Rules 20-21) can generate these artifacts using:

```toml
[[generation.rules]]
name = "soc2-customer-control-matrix"
ontology_files = ["ontology/reg/soc2.ttl", "ontology/customers.ttl"]
query = { file = "sparql/soc2/extract_customer_control_closure.sparql" }
template = { file = "templates/soc2/soc2_control_matrix.tera" }
output_pattern = "lib/soc2/customers/{customerId}.soc2.control_matrix.yaml"
```

## Key Features

### Deterministic Generation
All artifacts are generated deterministically:
- Queries ordered by IDs
- Deduplication prevents duplicates
- Same ontology always produces identical output
- Safe for version control and auditing

### Complete Control Hierarchy
Each artifact contains:
- Full control hierarchy (Suite → Category → Control)
- All validators per control
- All evidence requirements per control
- Evidence paths for automated collection
- Retention periods for archival

### Customer-Specific Scope
Each customer gets only their applicable controls:
- Based on `cust:requiresSuite` property
- Filtered to only their chosen suite(s)
- No irrelevant controls included
- Focused audit scope

## Documentation

For detailed technical information, see:

- **Implementation Guide**: `docs/SOC2_CUSTOMER_ARTIFACTS.md`
- **Data Model**: Describes the ontology structure
- **SPARQL Queries**: Details on control extraction
- **Troubleshooting**: Common issues and solutions

## Compliance Notes

These artifacts support:

- **SOC 2 Type I** Audits: Snapshot compliance at a point in time
- **SOC 2 Type II** Audits: Period-based evidence collection
- **Custom Periods**: Support for rolling audit windows
- **Multi-suite Audits**: Customers with multiple suite requirements

The control definitions in `ontology/reg/soc2.ttl` are based on AICPA's SOC 2 Trust Services Criteria framework.

## Related Files

- **Ontologies**:
  - `ontology/customers.ttl` - Customer definitions
  - `ontology/reg/soc2.ttl` - SOC 2 control library
  - `ontology/regulations.ttl` - Regulatory framework

- **Configuration**:
  - `ggen.toml` - Generation rules
  - `sparql/soc2/extract_customer_control_closure.sparql` - SPARQL query

- **Templates**:
  - `templates/soc2/soc2_control_matrix.tera` - YAML template
  - `templates/soc2/soc2_auditor_pack.tera` - JSON template

- **Scripts**:
  - `scripts/generate_customer_soc2_artifacts.py` - Generation script

---

**Last Updated**: 2025-02-11
**Version**: 1.0.0
**Status**: Active - Artifacts Generated
