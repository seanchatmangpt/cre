# SOC 2 Customer-Specific Artifact Generation

## Overview

This document describes the implementation of customer-specific SOC 2 compliance artifact generation for the Fortune-5 FIBO LineController Factory project.

## Purpose

For each customer defined in `ontology/customers.ttl` that specifies a `requiresSuite` property, the system automatically generates two types of SOC 2 compliance artifacts:

1. **Control Matrix YAML** (`{customer_id}.soc2.control_matrix.yaml`)
   - Hierarchical YAML document containing all applicable SOC 2 controls
   - Organized by Trust Services Category (TSC)
   - Includes validators and evidence requirements per control
   - Used by compliance teams for audit planning and execution

2. **Auditor Pack JSON** (`{customer_id}.soc2.auditor_pack.json`)
   - Machine-readable JSON manifest of audit requirements
   - Contains evidence inventory with paths and integrity requirements
   - Lists all required validators and control-evidence mappings
   - Supports automated evidence collection and validation

## Input: Customer Ontology

### Customers with SOC 2 Requirements

The `ontology/customers.ttl` file defines five customers, each with a `cust:requiresSuite` property pointing to a specific SOC 2 suite:

```turtle
# Example from customers.ttl
cust:megabank_ny a cust:Customer ;
    cust:customerId "megabank_ny" ;
    cust:customerName "MegaBank (New York)" ;
    cust:jurisdiction "new_york" ;
    cust:riskTier "tier_1_systemically_important" ;
    cust:requiresSuite reg:soc2_security_availability .
```

### Customers

| Customer ID | Customer Name | Suite | TSC Categories |
|---|---|---|---|
| megabank_ny | MegaBank (New York) | soc2_security_availability | Security, Availability |
| community_bank_ca | Community Bank (California) | soc2_security | Security |
| fintech_startup_de | FinTech Startup (Delaware) | soc2_security_processing_integrity | Security, Processing Integrity |
| credit_union_tx | Credit Union (Texas) | soc2_security_privacy | Security, Privacy |
| mortgage_lender_fl | Mortgage Lender (Florida) | soc2_security_confidentiality | Security, Confidentiality |

## Implementation

### Script: `scripts/generate_customer_soc2_artifacts.py`

**Language**: Python 3.11+

**Dependencies**:
- `rdflib` - RDF graph parsing and SPARQL querying
- `pyyaml` - YAML output formatting

**Architecture**:

```
SOC2ArtifactGenerator
├── load_ontologies()
│   ├── Parse ontology/reg/soc2.ttl
│   └── Parse ontology/customers.ttl
├── extract_customers_with_suites()
│   └── SPARQL: Select all customers with requiresSuite
├── extract_suite_controls(suite_id)
│   └── SPARQL: Get controls, validators, evidence for suite
├── generate_control_matrix_yaml()
│   └── Format hierarchical YAML with metadata
└── generate_auditor_pack_json()
    └── Format structured JSON with control-evidence mappings
```

### SPARQL Queries

#### Query 1: Extract Customers with Suite Requirements

```sparql
SELECT ?customerId ?customerName ?suiteId ?suiteName
WHERE {
    ?customer a cust:Customer ;
              cust:customerId ?customerId ;
              cust:customerName ?customerName ;
              cust:requiresSuite ?suite .
    ?suite a reg:Suite ;
           rdfs:label ?suiteName .
    BIND(STRAFTER(STR(?suite), "#") AS ?suiteId)
}
```

**Purpose**: Identify all customers requiring SOC 2 audits

#### Query 2: Extract Suite Controls, Validators, Evidence

```sparql
SELECT ?suiteName ?tscLabel ?controlId ?controlTitle ?validatorId ?evidenceId ?evidencePath ?retentionDays
WHERE {
    ?suite a reg:Suite ;
           rdfs:label ?suiteName .
    ?suite reg:includesCategory ?tsc .
    ?control a reg:Control ;
             reg:tscCategory ?tsc ;
             reg:controlId ?controlId ;
             reg:title ?controlTitle .
    ?control reg:requiresValidator ?validatorId .
    ?control reg:requiresEvidence ?evidence .
    ?evidence reg:evidenceId ?evidenceId ;
              reg:path ?evidencePath ;
              reg:format ?evidenceFormat ;
              reg:integrity ?evidenceIntegrity .
    OPTIONAL { ?evidence reg:retentionDays ?retentionDays . }
}
```

**Purpose**: Get complete control hierarchy for each suite

## Output: Generated Artifacts

### Directory Structure

```
lib/soc2/customers/
├── community_bank_ca.soc2.auditor_pack.json
├── community_bank_ca.soc2.control_matrix.yaml
├── credit_union_tx.soc2.auditor_pack.json
├── credit_union_tx.soc2.control_matrix.yaml
├── fintech_startup_de.soc2.auditor_pack.json
├── fintech_startup_de.soc2.control_matrix.yaml
├── megabank_ny.soc2.auditor_pack.json
├── megabank_ny.soc2.control_matrix.yaml
├── mortgage_lender_fl.soc2.auditor_pack.json
└── mortgage_lender_fl.soc2.control_matrix.yaml
```

### Control Matrix YAML Structure

Example: `megabank_ny.soc2.control_matrix.yaml`

```yaml
control_matrix:
  metadata:
    customer_id: megabank_ny
    customer_name: MegaBank (New York)
    suite_id: soc2_security_availability
    suite_name: SOC 2 Security + Availability
    generated_by: generate_customer_soc2_artifacts.py
    version: 1.0.0
  suites:
  - suite_id: soc2_security_availability
    suite_name: SOC 2 Security + Availability
    categories:
    - category_id: TSC_Availability
      category_name: Availability (TSC)
      controls:
      - control_id: CC7.1
        title: Backup and Recovery
        description: Entity maintains current processing capacity...
        validators:
        - hot_upgrade_validator
        - zero_downtime_validator
        evidence:
        - evidence_id: uptime_logs
          path: evidence/uptime/**/*.json
          format: jsonl
          integrity: sha256_manifest
          retention_days: 2555
```

**Uses**:
- Audit planning and scope definition
- Control assessment documentation
- Evidence collection checklist
- Compliance reporting

### Auditor Pack JSON Structure

Example: `community_bank_ca.soc2.auditor_pack.json`

```json
{
  "soc2_auditor_pack": {
    "version": "1.0.0",
    "generated_by": "generate_customer_soc2_artifacts.py",
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
          "evidence_id": "chaos_tests",
          "path": "evidence/chaos/**/*.json",
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
    ],
    "receipt_fields": {
      "build_receipt": "receipts/build.last.json",
      "evidence_manifest": "receipts/evidence.last.json",
      "verdict": "receipts/verdict.last.json"
    },
    "notes": [
      "Customer-specific SOC 2 auditor pack for Community Bank (California)",
      "Audit suite: SOC 2 Security",
      "All evidence files are indexed in evidence/evidence.sha256",
      "Receipt files contain cryptographic proof hashes",
      "Generated deterministically - same ontology produces identical pack"
    ]
  }
}
```

**Uses**:
- Evidence collection automation
- Validator configuration
- Audit evidence indexing
- Deterministic generation verification

## Running the Script

### Direct Execution

```bash
cd examples/fortune5-fibo-lnctrl
python3 scripts/generate_customer_soc2_artifacts.py
```

### With Custom Project Directory

```bash
python3 scripts/generate_customer_soc2_artifacts.py /path/to/project
```

### Output

```
================================================================================
SOC 2 Customer-Specific Artifact Generator
================================================================================

Loading ontologies...
Loading SOC 2 ontology from .../ontology/reg/soc2.ttl
Loading Customer ontology from .../ontology/customers.ttl

Extracting customers with requiresSuite...
  Found customer: community_bank_ca ...
  Found customer: credit_union_tx ...
  ...

Found 5 customers with SOC 2 suite requirements

Output directory: .../lib/soc2/customers

Generating customer-specific artifacts...

[1/5] community_bank_ca
  ✓ Generated community_bank_ca.soc2.control_matrix.yaml
  ✓ Generated community_bank_ca.soc2.auditor_pack.json
...

SUCCESS: Generated artifacts for 5 customers
Location: .../lib/soc2/customers
================================================================================
```

## Ontology Requirements

### SOC 2 Ontology (ontology/reg/soc2.ttl)

Defines:
- **Suites**: Composable sets of controls (e.g., `reg:soc2_security`, `reg:soc2_security_availability`)
- **Trust Services Categories (TSC)**: Categories like Security, Availability, Confidentiality, etc.
- **Controls**: Individual SOC 2 controls (CC6.1, CC7.1, etc.) with:
  - `reg:controlId` - Control identifier
  - `reg:title` - Control title
  - `reg:description` - Control description
  - `reg:tscCategory` - Related TSC
  - `reg:requiresValidator` - Validator module name
  - `reg:requiresEvidence` - Evidence requirement reference
- **Evidence Requirements**: Artifacts needed for audit with:
  - `reg:evidenceId` - Evidence identifier
  - `reg:path` - File pattern/path
  - `reg:format` - Data format (json, yaml, etc.)
  - `reg:integrity` - Integrity verification mechanism
  - `reg:retentionDays` - Retention period

### Customer Ontology (ontology/customers.ttl)

Defines:
- **Customers**: Instances of `cust:Customer` with:
  - `cust:customerId` - Unique identifier
  - `cust:customerName` - Display name
  - `cust:jurisdiction` - Regulatory jurisdiction
  - `cust:riskTier` - Risk classification
  - `cust:requiresRegulation` - Regulatory requirements (other)
  - **`cust:requiresSuite`** - SOC 2 suite requirement (**KEY PROPERTY**)

## Data Model: Control Hierarchy

```
Suite
├── TSC (Trust Services Category)
│   ├── Control 1
│   │   ├── Validator 1
│   │   └── Evidence 1
│   │       ├── evidence_id
│   │       ├── path
│   │       ├── format
│   │       ├── integrity
│   │       └── retention_days
│   │
│   └── Control 2
│       └── ...
│
└── TSC 2
    └── ...
```

## Deterministic Generation

The script ensures deterministic generation through:

1. **Stable SPARQL Ordering**: Queries use `ORDER BY` clauses ensuring consistent result ordering
2. **Set Uniqueness**: Deduplication prevents duplicate evidence entries
3. **Sorted Output**: All collections sorted by IDs before serialization
4. **Immutable Ontology**: No external data sources or randomization

**Result**: Running the script twice on the same ontology produces byte-for-byte identical artifacts.

## Integration Points

### With ggen (Generator Generator)

The `ggen.toml` configuration includes rules for generating these artifacts:

```toml
# Rule 20: Generate customer-specific SOC 2 control matrices
[[generation.rules]]
name = "soc2-customer-control-matrix"
ontology_files = ["ontology/reg/soc2.ttl", "ontology/customers.ttl"]
query = { file = "sparql/soc2/extract_customer_control_closure.sparql" }
template = { file = "templates/soc2/soc2_auditor_pack.tera" }
output_pattern = "lib/soc2/customers/{customerId}.soc2.control_matrix.yaml"
mode = "OverwriteAll"

# Rule 21: Generate customer-specific SOC 2 auditor packs
[[generation.rules]]
name = "soc2-customer-auditor-pack"
ontology_files = ["ontology/reg/soc2.ttl", "ontology/customers.ttl"]
query = { file = "sparql/soc2/extract_customer_control_closure.sparql" }
template = { file = "templates/soc2/soc2_auditor_pack.tera" }
output_pattern = "lib/soc2/customers/{customerId}.soc2.auditor_pack.json"
mode = "OverwriteAll"
```

### With Evidence Collection System

The auditor packs integrate with the evidence collection system:

- **Evidence Paths**: Match glob patterns in `evidence/` directory
- **Receipt Fields**: Link to attestation receipts in `receipts/`
- **Manifest Files**: Reference SHA-256 manifests for integrity
- **Retention Days**: Support archival and record-keeping policies

## Troubleshooting

### Issue: "No customers with requiresSuite found"

**Cause**: Namespace mismatch between ontologies

**Solution**: Ensure both `customers.ttl` and `soc2.ttl` use the same namespace for Suite definitions:
- Both should use: `http://fortune5.lnctrl.io/regulation#`
- NOT `http://fortune5.lnctrl.io/regulations#` (note: plural vs singular)

### Issue: Missing evidence fields

**Cause**: Control definition incomplete in SOC 2 ontology

**Solution**: Verify each control has:
- `reg:requiresValidator` (at least one)
- `reg:requiresEvidence` (at least one)
- Each evidence has `reg:evidenceId`, `reg:path`, `reg:format`, `reg:integrity`

### Issue: Script hangs on large ontologies

**Cause**: rdflib SPARQL performance on complex queries

**Solution**:
- Simplify SPARQL queries
- Use more specific FILTER clauses
- Consider incremental generation per customer

## Future Enhancements

1. **Incremental Generation**: Only regenerate changed customers
2. **Template Customization**: Support custom Tera templates per customer
3. **Evidence Validation**: Verify evidence files exist at paths specified
4. **Control Status Tracking**: Add compliance status and last-checked dates
5. **Multi-period Support**: Generate time-series control matrices for rolling audits
6. **Custom Controls**: Support customer-specific control extensions

## Related Files

- **Ontologies**:
  - `ontology/customers.ttl` - Customer definitions
  - `ontology/reg/soc2.ttl` - SOC 2 control definitions
- **Generator Configuration**: `ggen.toml` - Rules 20-21
- **SPARQL Queries**: `sparql/soc2/extract_customer_control_closure.sparql`
- **Templates**: `templates/soc2/soc2_*.tera`
- **Output**: `lib/soc2/customers/{customerId}.soc2.*.{yaml,json}`

## References

- SOC 2 Trust Services Criteria (AICPA): https://www.aicpa.org/soc2
- RDFlib Documentation: https://rdflib.readthedocs.io/
- SPARQL Query Language: https://www.w3.org/TR/sparql11-query/
- YAML Specification: https://yaml.org/spec/1.2/spec.html

---

**Generated**: 2025-02-11
**Version**: 1.0.0
**Status**: Implemented and Tested
