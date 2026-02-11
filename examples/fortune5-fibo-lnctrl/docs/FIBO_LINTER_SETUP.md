# FIBO Linter Setup Guide

Block F: FIBO-first enforcement with ontology linter

## Overview

The FIBO linter enforces FIBO (Financial Industry Business Ontology) vocabulary compliance across the Fortune-5 LineController ontologies. It validates that all financial terms use standardized FIBO classes and properties.

## Installation

### 1. Application Structure

```
apps/f5_ontology_tools/
├── src/
│   ├── fibo_linter.erl           # Core linting logic
│   ├── fibo_linter.hrl           # Record definitions
│   ├── fibo_linter_cli.erl       # (Deprecated) Old CLI
│   ├── f5_ontology_tools_app.erl # OTP app module
│   └── f5_ontology_tools_sup.erl # OTP supervisor
├── ebin/                          # Compiled BEAM files
└── README.md                      # Documentation
```

### 2. Ontology Files

```
ontology/
├── f5_line_control.ttl    # Main ontology (now imports FIBO alignment)
└── fibo_alignment.ttl     # FIBO term mappings (owl:equivalentClass)
```

### 3. Scripts

```
scripts/
├── fibo_linter                    # Main CLI tool (escript)
├── run_fibo_linter.sh            # Compile + run pipeline
└── pre-commit-fibo-linter.sh     # Pre-commit hook
```

## Usage

### Quick Start

```bash
# Run linter on default ontology
bash scripts/run_fibo_linter.sh

# Lint specific file
./scripts/fibo_linter lint ontology/f5_line_control.ttl

# Check if a term is FIBO-aligned
./scripts/fibo_linter check-term Borrower
./scripts/fibo_linter check-term LoanApplication
```

### Output

The linter generates:

1. **Console output** - Summary statistics and top undefined terms
2. **Markdown report** - Detailed alignment report at `docs/FIBO_ALIGNMENT_REPORT.md`

Example console output:
```
FIBO Linter - Analyzing ontology/f5_line_control.ttl
----------------------------------------
Total terms: 261
FIBO-aligned: 10
Undefined terms: 251
Warnings: 0

✓ Report generated: docs/FIBO_ALIGNMENT_REPORT.md

⚠️  Undefined terms requiring FIBO alignment:
  - f5:CreateAccountOp → fibo-fnd:Account
  - f5:UpdateAccountOp → fibo-fnd:Account
  ... and 241 more (see report)
```

## FIBO Alignment Ontology

The `ontology/fibo_alignment.ttl` file provides explicit mappings:

```turtle
# Example: LoanApplication → FIBO LoanContract
f5:LoanApplication
    a owl:Class ;
    owl:equivalentClass fibo-loan-ln-ln:LoanContract ;
    skos:definition "A loan application is equivalent to a FIBO loan contract in the application stage" ;
    rdfs:comment "FIBO Alignment: Use fibo-loan-ln-ln:LoanContract for loan contracts" .
```

Key mappings:

| Fortune-5 Term      | FIBO Equivalent                              |
|---------------------|----------------------------------------------|
| f5:LoanApplication  | fibo-loan-ln-ln:LoanContract                 |
| f5:Borrower         | fibo-loan-ln-ln:Borrower                     |
| f5:Party            | fibo-fnd-aap-agt:AutonomousAgent             |
| f5:Agreement        | fibo-fnd-agr-ctr:WrittenContract             |
| f5:Account          | fibo-fbc-pas-fpas:Account                    |
| f5:LegalEntity      | fibo-be-le-lp:LegalPerson                    |

## Pre-commit Hook Installation

```bash
# Create git hooks directory if needed
mkdir -p .git/hooks

# Install pre-commit hook
ln -s ../../scripts/pre-commit-fibo-linter.sh .git/hooks/pre-commit

# Make executable
chmod +x scripts/pre-commit-fibo-linter.sh
```

The hook will:
- Automatically run on `git commit`
- Lint all modified `.ttl` files
- Block commits if FIBO violations are found
- Generate alignment reports for review

To bypass (emergency only):
```bash
git commit --no-verify -m "Emergency commit"
```

## FIBO Vocabulary Reference

### Recognized FIBO Namespaces

- **fibo-fnd** - Foundations
  - `fibo-fnd-aap-agt` - Agents and People / Agents
  - `fibo-fnd-aap-ppl` - Agents and People / People
  - `fibo-fnd-agr-ctr` - Agreements / Contracts
  - `fibo-fnd-acc-cur` - Accounting / Currency Amount

- **fibo-be** - Business Entities
  - `fibo-be-le-lp` - Legal Entities / Legal Persons
  - `fibo-be-le-fbo` - Legal Entities / Formal Business Organizations

- **fibo-loan** - Loans
  - `fibo-loan-ln-ln` - Loans General / Loans
  - `fibo-loan-spc-cns` - Loans Specific / Consumer Loans
  - `fibo-loan-spc-com` - Loans Specific / Commercial Loans

- **fibo-fbc** - Financial Business & Commerce
  - `fibo-fbc-pas-fpas` - Products and Services / Financial Products and Services
  - `fibo-fbc-dae-dbt` - Debt and Equities / Debt

## Validation Rules

1. **Financial domain classes SHOULD use FIBO vocabulary**
2. **Loan-related terms MUST use fibo-loan namespace**
3. **Party/entity terms MUST use fibo-fnd or fibo-be namespaces**
4. **Custom business logic (workflow, connectors) MAY use custom namespaces**

## Troubleshooting

### Compilation Issues

```bash
# Ensure modules are compiled
cd apps/f5_ontology_tools
erlc -o ebin src/fibo_linter.erl src/f5_ontology_tools_app.erl src/f5_ontology_tools_sup.erl
```

### Escript Path Issues

```bash
# The escript needs to be run from project root
cd /path/to/fortune5-fibo-lnctrl
./scripts/fibo_linter lint ontology/f5_line_control.ttl
```

### Missing BEAM Files

```bash
# Run the full pipeline which compiles first
bash scripts/run_fibo_linter.sh
```

## Integration with CI/CD

Add to your CI pipeline:

```yaml
# Example GitHub Actions
- name: FIBO Linter
  run: |
    bash scripts/run_fibo_linter.sh
    if [ $(grep -c "FIBO-Aligned: 0" docs/FIBO_ALIGNMENT_REPORT.md) -eq 1 ]; then
      echo "No FIBO-aligned terms found!"
      exit 1
    fi
```

## References

- [FIBO Specification](https://spec.edmcouncil.org/fibo/)
- [FIBO GitHub Repository](https://github.com/edmcouncil/fibo)
- [EDM Council FIBO](https://edmcouncil.org/frameworks/industry-models/fibo/)
- [Financial Industry Business Data Model](https://fib-dm.com/)

## Support

For issues or questions:
1. Check `docs/FIBO_ALIGNMENT_REPORT.md` for detailed term analysis
2. Review `ontology/fibo_alignment.ttl` for mapping examples
3. Consult FIBO specification for canonical vocabulary

## License

Apache-2.0
