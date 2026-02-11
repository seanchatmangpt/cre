# F5 Ontology Tools - FIBO Linter

FIBO-first enforcement tooling for Fortune-5 LineController ontologies.

## Overview

This application provides ontology validation and linting tools to ensure all financial terms use FIBO (Financial Industry Business Ontology) vocabulary. It helps maintain semantic interoperability with FIBO-compliant systems.

## Features

- **FIBO Term Validation**: Checks all terms in Turtle ontology files against FIBO vocabulary
- **Smart Suggestions**: Provides FIBO equivalent suggestions for custom terms
- **Detailed Reports**: Generates comprehensive alignment reports in Markdown format
- **Pre-commit Hooks**: Prevents non-FIBO terms from being committed

## Usage

### Command-Line Interface

```bash
# Lint an ontology file
./scripts/fibo_linter lint ontology/f5_line_control.ttl

# Lint with custom output file
./scripts/fibo_linter lint ontology/f5_line_control.ttl docs/my_report.md

# Check a specific term
./scripts/fibo_linter check-term LoanApplication
./scripts/fibo_linter check-term Borrower
```

### Shell Script

```bash
# Run the full linter pipeline (compile + lint)
bash scripts/run_fibo_linter.sh ontology/f5_line_control.ttl docs/FIBO_ALIGNMENT_REPORT.md
```

### Programmatic Usage

```erlang
%% Lint a file
{ok, Result} = fibo_linter:lint_file("ontology/f5_line_control.ttl"),
io:format("FIBO-aligned: ~B out of ~B terms~n",
          [Result#lint_result.fibo_aligned, Result#lint_result.total_terms]).

%% Check a single term
case fibo_linter:check_term("LoanApplication") of
    {ok, {Namespace, FiboTerm}} ->
        io:format("FIBO term: ~s:~s~n", [Namespace, FiboTerm]);
    {error, not_fibo} ->
        io:format("Not a FIBO term~n")
end.

%% Get suggestion for custom term
case fibo_linter:suggest_fibo_term("CustomLoan") of
    {ok, Suggestion} ->
        io:format("Suggested FIBO term: ~s~n", [Suggestion]);
    {error, no_suggestion} ->
        io:format("No FIBO suggestion available~n")
end.
```

## FIBO Vocabulary Coverage

The linter recognizes the following FIBO namespaces:

- **fibo-fnd**: Foundations (Party, Agreement, Account, Person, Organization, etc.)
- **fibo-be**: Business Entities (LegalEntity, Corporation, Partnership, etc.)
- **fibo-loan**: Loans (Loan, Borrower, LoanContract, Mortgage, etc.)
- **fibo-fbc**: Financial Business & Commerce (FinancialInstitution, Bank, Lender, etc.)
- **fibo-sec**: Securities
- **fibo-der**: Derivatives
- **fibo-ind**: Indices & Indicators

## Term Mapping

The linter includes built-in mappings for common financial terms:

| Custom Term         | FIBO Equivalent                |
|---------------------|--------------------------------|
| LoanApplication     | fibo-loan:LoanContract         |
| Borrower            | fibo-loan:Borrower             |
| Party               | fibo-fnd:Party                 |
| Agreement           | fibo-fnd:Agreement             |
| Account             | fibo-fnd:Account               |
| LegalEntity         | fibo-be:LegalEntity            |
| FinancialInstitution| fibo-fbc:FinancialInstitution  |

## FIBO Alignment Ontology

See `ontology/fibo_alignment.ttl` for explicit OWL mappings between custom Fortune-5 terms and FIBO vocabulary using `owl:equivalentClass`.

## Pre-commit Hook

Install the pre-commit hook to automatically validate ontology files:

```bash
# If you have a git repo
ln -s ../../scripts/pre-commit-fibo-linter.sh .git/hooks/pre-commit
```

The hook will:
- Run FIBO linter on all modified .ttl files
- Generate alignment reports
- Block commits if FIBO violations are found

To bypass (not recommended):
```bash
git commit --no-verify
```

## Generated Report

The linter generates a detailed Markdown report with:

- Summary statistics (total terms, FIBO-aligned percentage)
- List of undefined terms with line numbers
- FIBO suggestions for each term
- Namespace information
- Recommendations for alignment

Example output:
```
# FIBO Alignment Report

**File:** ontology/f5_line_control.ttl
**Generated:** 2026-02-11 14:15:12 UTC

## Summary

- **Total Terms:** 261
- **FIBO-Aligned:** 10 (3.8%)
- **Undefined Terms:** 251
- **Warnings:** 0

## Undefined Terms

### `f5:CreateAccountOp` (Line 7)
- **Namespace:** `f5`
- **Status:** ⚠️  Needs FIBO alignment
- **Recommendation:** Suggested: `fibo-fnd:Account`
...
```

## References

- [FIBO Specification](https://spec.edmcouncil.org/fibo/)
- [FIBO on GitHub](https://github.com/edmcouncil/fibo)
- [EDM Council](https://edmcouncil.org/frameworks/industry-models/fibo/)
- [FIBO Data Model](https://fib-dm.com/)

## Module Structure

- `fibo_linter.erl` - Core linting logic and FIBO vocabulary mappings
- `fibo_linter.hrl` - Shared record definitions
- `fibo_linter_cli.erl` - (Deprecated) Old CLI interface
- `f5_ontology_tools_app.erl` - OTP application module
- `f5_ontology_tools_sup.erl` - OTP supervisor

## License

Apache-2.0
