# SOC 2 Template Example Outputs

This document shows example outputs from the SOC 2 Tera templates when processing SPARQL results.

## Example SPARQL Input

The templates expect input from SPARQL queries like `sparql/soc2/extract_control_closure.sparql`:

```
suiteId | suiteName                            | tscId | tscLabel          | controlId | controlTitle      | controlDescription | validatorId           | evidenceId | evidencePath      | evidenceFormat | evidenceIntegrity | retentionDays
--------|---------------------------------------|-------|-------------------|-----------|-------------------|--------------------|-----------------------|------------|-------------------|----------------|-------------------|---------------
CC6.1   | Logical and Physical Access Controls | CC6   | Logical Access    | CC6.1     | Logical Access    | Controls access    | access_validator      | ev_001     | evidence/access1  | json           | sha256            | 2555
CC6.1   | Logical and Physical Access Controls | CC6   | Logical Access    | CC6.1     | Logical Access    | Controls access    | audit_validator       | ev_002     | evidence/access2  | json           | sha256            | 2555
CC6.1   | Logical and Physical Access Controls | CC7   | Physical Access   | CC7.1     | Physical Access   | Controls access    | access_validator      | ev_003     | evidence/phys1    | json           | sha256            | 2555
PI1.1   | Personnel Security                   | PI1   | Personnel Rights  | PI1.1     | Personnel Rights  | Rights management  | personnel_validator   | ev_004     | evidence/pers1    | json           | sha256            | 1825
```

## Example Output: soc2_auditor_pack.tera

**Input**: SPARQL results from `extract_control_closure.sparql`

**Output** (deterministic JSON):

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
            "categoryLabel": "Logical Access",
            "controls": [
              {
                "controlId": "CC6.1",
                "controlTitle": "Logical Access",
                "controlDescription": "Controls access",
                "evidence": [
                  {
                    "evidenceFormat": "json",
                    "evidenceId": "ev_001",
                    "evidencePath": "evidence/access1",
                    "evidenceIntegrity": "sha256",
                    "retentionDays": 2555
                  },
                  {
                    "evidenceFormat": "json",
                    "evidenceId": "ev_002",
                    "evidencePath": "evidence/access2",
                    "evidenceIntegrity": "sha256",
                    "retentionDays": 2555
                  }
                ],
                "validators": [
                  {
                    "validatorId": "access_validator"
                  },
                  {
                    "validatorId": "audit_validator"
                  }
                ]
              }
            ]
          },
          {
            "categoryId": "CC7",
            "categoryLabel": "Physical Access",
            "controls": [
              {
                "controlId": "CC7.1",
                "controlTitle": "Physical Access",
                "controlDescription": "Controls access",
                "evidence": [
                  {
                    "evidenceFormat": "json",
                    "evidenceId": "ev_003",
                    "evidencePath": "evidence/phys1",
                    "evidenceIntegrity": "sha256",
                    "retentionDays": 2555
                  }
                ],
                "validators": [
                  {
                    "validatorId": "access_validator"
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "suiteId": "PI1.1",
        "suiteName": "Personnel Security",
        "categories": [
          {
            "categoryId": "PI1",
            "categoryLabel": "Personnel Rights",
            "controls": [
              {
                "controlId": "PI1.1",
                "controlTitle": "Personnel Rights",
                "controlDescription": "Rights management",
                "evidence": [
                  {
                    "evidenceFormat": "json",
                    "evidenceId": "ev_004",
                    "evidencePath": "evidence/pers1",
                    "evidenceIntegrity": "sha256",
                    "retentionDays": 1825
                  }
                ],
                "validators": [
                  {
                    "validatorId": "personnel_validator"
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

### Determinism Properties

- **Suites**: Sorted by `suiteId` (CC6.1, PI1.1)
- **Categories within Suite**: Sorted by `tscId` (CC6, CC7)
- **Controls within Category**: Sorted by `controlId`
- **Evidence within Control**: Sorted by `evidencePath`
- **Validators**: Deduplicated and sorted alphabetically
- **No timestamps**: No `datetime`, `now_utc()`, or generated timestamps

## Example Output: soc2_control_matrix_v2.tera

**Input**: Same SPARQL results

**Output** (deterministic YAML):

```yaml
soc2_control_matrix:
  format_version: "2.0.0"
  compliance_framework: "SOC 2"
  suites:
    CC6.1:
      suite_name: "Logical and Physical Access Controls"
      categories:
        CC6:
          category_label: "Logical Access"
          controls:
            CC6.1:
              title: "Logical Access"
              description: "Controls access"
              evidence:
                - evidence_id: "ev_001"
                  format: "json"
                  integrity: "sha256"
                  path: "evidence/access1"
                  retention_days: 2555
                - evidence_id: "ev_002"
                  format: "json"
                  integrity: "sha256"
                  path: "evidence/access2"
                  retention_days: 2555
              validators:
                - "access_validator"
                - "audit_validator"
        CC7:
          category_label: "Physical Access"
          controls:
            CC7.1:
              title: "Physical Access"
              description: "Controls access"
              evidence:
                - evidence_id: "ev_003"
                  format: "json"
                  integrity: "sha256"
                  path: "evidence/phys1"
                  retention_days: 2555
              validators:
                - "access_validator"
    PI1.1:
      suite_name: "Personnel Security"
      categories:
        PI1:
          category_label: "Personnel Rights"
          controls:
            PI1.1:
              title: "Personnel Rights"
              description: "Rights management"
              evidence:
                - evidence_id: "ev_004"
                  format: "json"
                  integrity: "sha256"
                  path: "evidence/pers1"
                  retention_days: 1825
              validators:
                - "personnel_validator"
```

## Example Output: soc2_customer_auditor_pack.tera

**Input**: SPARQL results from `extract_customer_control_closure.sparql` with `customerId` and `customerName` fields

**Output** (customer-scoped JSON):

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
            "suiteName": "Logical and Physical Access Controls",
            "categories": [
              {
                "categoryId": "CC6",
                "categoryLabel": "Logical Access",
                "controls": [
                  {
                    "controlId": "CC6.1",
                    "controlTitle": "Logical Access",
                    "controlDescription": "Controls access",
                    "evidence": [
                      {
                        "evidenceFormat": "json",
                        "evidenceId": "ev_001",
                        "evidencePath": "evidence/access1",
                        "evidenceIntegrity": "sha256",
                        "retentionDays": 2555
                      }
                    ],
                    "validators": [
                      {
                        "validatorId": "access_validator"
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "customerId": "widgetco",
        "customerName": "Widget Corp",
        "suites": [
          {
            "suiteId": "PI1.1",
            "suiteName": "Personnel Security",
            "categories": [
              {
                "categoryId": "PI1",
                "categoryLabel": "Personnel Rights",
                "controls": [
                  {
                    "controlId": "PI1.1",
                    "controlTitle": "Personnel Rights",
                    "controlDescription": "Rights management",
                    "evidence": [
                      {
                        "evidenceFormat": "json",
                        "evidenceId": "ev_004",
                        "evidencePath": "evidence/pers1",
                        "evidenceIntegrity": "sha256",
                        "retentionDays": 1825
                      }
                    ],
                    "validators": [
                      {
                        "validatorId": "personnel_validator"
                      }
                    ]
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

### Determinism Properties

- **Customers**: Sorted by `customerId` alphabetically (acme-corp, widgetco)
- **Suites within Customer**: Sorted by `suiteId`
- **All nested structures**: Same stable sorting as regular auditor pack
- **No timestamps**: No customer-scoped timestamps or generation times

## Example Output: soc2_customer_control_matrix.tera

**Input**: SPARQL results from `extract_customer_control_closure.sparql`

**Output** (customer-scoped YAML):

```yaml
soc2_customer_control_matrix:
  format_version: "2.0.0"
  compliance_framework: "SOC 2"
  customers:
    acme-corp:
      customer_name: "ACME Corporation"
      suites:
        CC6.1:
          suite_name: "Logical and Physical Access Controls"
          categories:
            CC6:
              category_label: "Logical Access"
              controls:
                CC6.1:
                  title: "Logical Access"
                  description: "Controls access"
                  evidence:
                    - evidence_id: "ev_001"
                      format: "json"
                      integrity: "sha256"
                      path: "evidence/access1"
                      retention_days: 2555
                  validators:
                    - "access_validator"
    widgetco:
      customer_name: "Widget Corp"
      suites:
        PI1.1:
          suite_name: "Personnel Security"
          categories:
            PI1:
              category_label: "Personnel Rights"
              controls:
                PI1.1:
                  title: "Personnel Rights"
                  description: "Rights management"
                  evidence:
                    - evidence_id: "ev_004"
                      format: "json"
                      integrity: "sha256"
                      path: "evidence/pers1"
                      retention_days: 1825
                  validators:
                    - "personnel_validator"
```

## Verifying Determinism

All outputs are fully deterministic. Running the same template twice on the same SPARQL results will produce byte-for-byte identical output:

```bash
# First generation
ggen generate --rule soc2-auditor-pack
sha256sum lib/soc2/soc2.auditor_pack.json > hash1.txt

# Second generation
ggen generate --rule soc2-auditor-pack
sha256sum lib/soc2/soc2.auditor_pack.json > hash2.txt

# Verify same hash
diff hash1.txt hash2.txt
# Should show no output (hashes match)
```

## Key Properties

| Property | Value |
|----------|-------|
| Output Format | JSON or YAML (template-specific) |
| Deterministic | Yes - same input → same output |
| Reproducible | Yes - no runtime dependencies |
| Timestamp-Free | Yes - no datetime values |
| Sorted | Yes - all collections alphabetically ordered |
| Deduplicated | Yes - uses Tera `unique()` filter |
| Validated | Yes - valid JSON/YAML schema |

## Next Steps

1. **Add SPARQL queries** to `sparql/soc2/` directory
2. **Configure in ggen.toml** with template rules
3. **Run code generation**: `ggen generate`
4. **Verify outputs** in `lib/soc2/` directory
