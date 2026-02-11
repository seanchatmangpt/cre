# FIBO IRI Validation Enhancement

## Overview

The `fibo_cloud_first_linter` module has been enhanced to validate that all financial domain terms in the ontology resolve to valid FIBO (Financial Industry Business Ontology) IRIs (Internationalized Resource Identifiers). This ensures semantic correctness and interoperability with FIBO-compliant systems.

## Key Enhancements

### 1. IRI Resolution Validation

The linter now validates that FIBO-aligned terms have proper IRI resolution paths:

```erlang
%% Validate that a term resolves to a valid FIBO IRI
validate_fibo_iri(<<"fibo-loan:Loan">>) -> ok

%% Resolve the full IRI path
resolve_term_iri(<<"fibo-loan">>, <<"Loan">>) ->
    {ok, <<"https://spec.edmcouncil.org/fibo/ontology/LOAN>...">>}
```

### 2. Enhanced Financial Domain Detection

Extended financial domain keyword list to cover more financial concepts:

- **Loan Domain**: loan, mortgage, borrower, lender, principal, credit, debit
- **Account Domain**: account, transaction, payment, interest
- **Entity Domain**: customer, party, agreement, contract
- **Advanced Finance**: financial, banking, asset, liability, equity, security, bond, stock, fund, portfolio, investment

### 3. Multi-File Ontology Scanning

New batch processing capability to scan entire ontology directories:

```erlang
%% Lint all .ttl files in a directory
lint_ontology_dir("ontology/") -> {ok, [Results]}

%% With options for customization
lint_ontology_dir("ontology/", #{strict => true})
```

### 4. Detailed Violation Reporting

Enhanced violations include:
- **type**: Violation type (missing_fibo_alignment, invalid_fibo_iri, missing_cloud_alignment, missing_justification)
- **term**: The problematic term
- **line**: Line number in source file
- **reason**: Detailed reason for failure
- **suggestion**: FIBO term suggestion for alignment
- **severity**: high, medium, or low

## FIBO Namespace Support

The linter recognizes the following FIBO namespaces and their IRI mappings:

| Namespace | IRI Base | Domain |
|-----------|----------|--------|
| `fibo-fnd` | `FND` | Foundations (Party, Agreement, Account) |
| `fibo-be` | `BE` | Business Entities (LegalEntity, Corporation) |
| `fibo-loan` | `LOAN` | Loans (Loan, Borrower, LoanContract) |
| `fibo-fbc` | `FBC` | Financial Business & Commerce (Bank, Lender) |
| `fibo-sec` | `SEC` | Securities |
| `fibo-der` | `DER` | Derivatives |
| `fibo-ind` | `IND` | Indices & Indicators |

Plus full namespace prefixes:
- `fibo-fnd-aap-agt` - Agents
- `fibo-fnd-aap-ppl` - People
- `fibo-fnd-agr-ctr` - Contracts
- `fibo-fnd-acc-cur` - Currency & Amounts
- `fibo-be-le-lp` - Legal Persons
- `fibo-be-le-fbo` - Formal Business Organizations
- `fibo-fbc-pas-fpas` - Financial Products & Services
- `fibo-fbc-dae-dbt` - Debt
- `fibo-loan-ln-ln` - General Loans
- `fibo-loan-spc-cns` - Consumer Loans
- `fibo-loan-spc-com` - Commercial Loans

## IRI Construction

IRIs are constructed following FIBO conventions:

```
Base: https://spec.edmcouncil.org/fibo/ontology/
Pattern: Base + {NAMESPACE_PATH} + ">"

Examples:
- fibo-loan:Loan → https://spec.edmcouncil.org/fibo/ontology/LOAN>
- fibo-fnd:Party → https://spec.edmcouncil.org/fibo/ontology/FND>
- fibo-be:LegalEntity → https://spec.edmcouncil.org/fibo/ontology/BE>
```

## API Reference

### Main Functions

#### `lint_ontology/1`
```erlang
-spec lint_ontology(string()) -> {ok, lint_result()} | {error, term()}.
```
Lint a single TTL ontology file. Returns aggregated results including IRI validation counts.

**Result Map:**
```erlang
#{
    terms_checked => integer(),      %% Total terms found
    fibo_aligned => integer(),       %% Terms using FIBO namespaces
    cloud_aligned => integer(),      %% Terms using cloud namespaces
    custom_justified => integer(),   %% Custom terms with justification
    custom_unjustified => integer(), %% Custom terms without justification
    iri_validated => integer(),      %% FIBO terms with valid IRIs
    iri_invalid => integer(),        %% FIBO terms with invalid IRIs
    violations => [map()]            %% Detailed violation list
}
```

#### `lint_ontology_dir/1,2`
```erlang
-spec lint_ontology_dir(string()) -> {ok, [lint_result()]} | {error, term()}.
-spec lint_ontology_dir(string(), map()) -> {ok, [lint_result()]} | {error, term()}.
```
Batch process all `.ttl` files in a directory.

#### `validate_fibo_iri/1`
```erlang
-spec validate_fibo_iri(binary()) -> ok | {error, term()}.
```
Validate that a term has a valid FIBO IRI. Accepts format: `<<"namespace:Term">>`.

**Errors:**
- `{error, invalid_term_format}` - Term doesn't contain colon separator
- `{error, {not_fibo_namespace, Namespace}}` - Not a recognized FIBO namespace
- `{error, {invalid_iri_format, IRI}}` - IRI construction failed

#### `resolve_term_iri/2`
```erlang
-spec resolve_term_iri(binary(), binary()) -> {ok, binary()} | {error, term()}.
```
Resolve a namespace and term to a full IRI.

**Example:**
```erlang
resolve_term_iri(<<"fibo-loan">>, <<"Loan">>)
-> {ok, <<"https://spec.edmcouncil.org/fibo/ontology/LOAN>">>}
```

#### `validate_term/2`
```erlang
-spec validate_term(binary(), binary()) -> ok | {error, term()}.
```
Validate a term in a specific domain context. Enhanced to check IRI validity.

#### `generate_proof/1`
```erlang
-spec generate_proof(lint_result()) -> map().
```
Generate cryptographic proof of FIBO/Cloud-First compliance. Returns:
```erlang
#{
    proof_type => <<"FIBO_Cloud_First_Compliance">>,
    terms_checked => integer(),
    fibo_aligned => integer(),
    cloud_aligned => integer(),
    custom_justified => integer(),
    iri_validated => integer(),     %% NEW: IRI count
    iri_invalid => integer(),       %% NEW: Invalid IRI count
    violations_count => integer(),
    violations => [map()],
    compliant => boolean(),
    hash => binary()                %% SHA256 of sorted result
}
```

## Example Usage

### Single File Validation

```erlang
%% Lint a single ontology file
{ok, Result} = fibo_cloud_first_linter:lint_ontology("ontology/fibo_alignment.ttl"),

%% Check compliance
Proof = fibo_cloud_first_linter:generate_proof(Result),

case maps:get(compliant, Proof) of
    true -> io:format("Ontology is FIBO-compliant!~n");
    false ->
        Violations = maps:get(violations, Proof),
        lists:foreach(fun(V) ->
            io:format("Violation: ~p~n", [V])
        end, Violations)
end.
```

### Batch Directory Validation

```erlang
%% Lint all TTL files in a directory
{ok, Results} = fibo_cloud_first_linter:lint_ontology_dir("ontology/"),

%% Process each file's results
lists:foreach(fun(Result) ->
    TermsChecked = maps:get(terms_checked, Result),
    FiboAligned = maps:get(fibo_aligned, Result),
    IRIValidated = maps:get(iri_validated, Result),

    io:format("File: ~p terms, ~p FIBO-aligned, ~p IRIs validated~n",
              [TermsChecked, FiboAligned, IRIValidated])
end, Results).
```

### IRI Validation

```erlang
%% Validate individual terms
case fibo_cloud_first_linter:validate_fibo_iri(<<"fibo-loan:Borrower">>) of
    ok -> io:format("IRI valid~n");
    {error, Reason} -> io:format("IRI error: ~p~n", [Reason])
end.

%% Resolve full IRI path
{ok, IRI} = fibo_cloud_first_linter:resolve_term_iri(<<"fibo-loan">>, <<"Loan">>),
io:format("Full IRI: ~p~n", [IRI]).
```

### Term Validation in Domain Context

```erlang
%% Validate term in financial domain
case fibo_cloud_first_linter:validate_term(<<"fibo-loan:Loan">>, <<"financial">>) of
    ok -> io:format("Term is properly aligned~n");
    {error, {missing_fibo_alignment, Term, Domain}} ->
        io:format("Term ~p should use FIBO in domain ~p~n", [Term, Domain]);
    {error, {invalid_fibo_iri, Term, Reason}} ->
        io:format("IRI invalid for ~p: ~p~n", [Term, Reason])
end.
```

## Ontology File Format

The linter expects standard Turtle (TTL) format with FIBO prefix declarations:

```turtle
@prefix fibo-loan: <https://spec.edmcouncil.org/fibo/ontology/LOAN/> .
@prefix fibo-fnd: <https://spec.edmcouncil.org/fibo/ontology/FND/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .

fibo-loan:Borrower a owl:Class ;
    rdfs:label "Borrower" ;
    rdfs:comment "A party that receives a loan" ;
    skos:definition "Equivalent to FIBO definition" .

fibo-fnd:Party a owl:Class ;
    rdfs:label "Party" .
```

### Justification Annotations

Custom terms must include justification using one of:
- `skos:note` - Explanation of custom term
- `rdfs:comment` - Comment annotation
- `# Justification:` - Comment line starting with "Justification:"

Example:
```turtle
custom:MyCustomTerm a owl:Class ;
    rdfs:comment "Used for application-specific workflow tracking" ;
    skos:note "No direct FIBO equivalent exists for this workflow concept" .
```

## Violation Types

### 1. `missing_fibo_alignment`
A term in the financial domain is not using FIBO namespace.

```erlang
#{
    type => missing_fibo_alignment,
    term => <<"custom:Loan">>,
    line => 42,
    suggestion => <<"fibo-loan:Loan">>,
    severity => high
}
```

**Resolution**: Use the suggested FIBO term instead.

### 2. `invalid_fibo_iri`
A term uses FIBO namespace but IRI resolution fails.

```erlang
#{
    type => invalid_fibo_iri,
    term => <<"fibo-loan:UnknownTerm">>,
    line => 42,
    reason => {invalid_iri_format, <<"https://...">>},
    severity => high
}
```

**Resolution**: Verify term name matches FIBO specification.

### 3. `missing_cloud_alignment`
A cloud infrastructure term is not using standard cloud namespace.

```erlang
#{
    type => missing_cloud_alignment,
    term => <<"custom:Deployment">>,
    line => 42,
    suggestion => <<"k8s:Deployment">>,
    severity => medium
}
```

**Resolution**: Use cloud ontology term (k8s, gcp, aws, etc.).

### 4. `missing_justification`
A custom term lacks required justification annotation.

```erlang
#{
    type => missing_justification,
    term => <<"custom:CustomTerm">>,
    line => 42,
    required => <<"skos:note or rdfs:comment explaining why custom term is needed">>,
    severity => low
}
```

**Resolution**: Add `rdfs:comment` or `skos:note` explaining the custom term.

## Testing

Run the test suite:

```bash
# In Docker container
rebar3 ct --suite=fibo_cloud_first_linter_SUITE

# Or with specific tests
rebar3 ct --suite=fibo_cloud_first_linter_SUITE --test=test_validate_fibo_iri_valid
```

### Test Coverage

The test suite (`fibo_cloud_first_linter_SUITE.erl`) includes:

- **IRI Validation Tests**
  - Valid FIBO IRI format acceptance
  - Invalid namespace rejection
  - Invalid format handling
  - Namespace-specific IRI resolution (LOAN, FND, BE)

- **Ontology Linting Tests**
  - FIBO-aligned term detection
  - Custom term violation detection
  - Batch directory processing

- **Domain Detection Tests**
  - Financial domain keyword detection
  - Financial domain term validation
  - Cloud domain alignment

- **Proof Generation Tests**
  - Violation-rich proof generation
  - Compliant ontology proof
  - IRI validation count accuracy

## Integration with CI/CD

The linter can be integrated into CI/CD pipelines:

```bash
#!/bin/bash
# ci_check_fibo_compliance.sh

if fibo_cloud_first_linter:lint_ontology("ontology/fibo_alignment.ttl"); then
    Proof=$(generate_proof Result)
    if maps:get(compliant, Proof) then
        echo "✓ FIBO compliance verified"
        exit 0
    else
        echo "✗ FIBO violations found:"
        violations=$(maps:get(violations, Proof))
        # Output violations for review
        exit 1
    fi
fi
```

## Performance Notes

- **Single file**: O(n) where n = number of lines in TTL file
- **Directory scan**: O(m*n) where m = number of TTL files, n = average lines per file
- **IRI validation**: O(1) per term - simple string matching
- **Memory**: Minimal - streaming line processing

### Optimizations for Large Ontologies

For ontologies with 10,000+ terms:
1. Process in parallel per-file
2. Cache namespace mappings
3. Use batch IRI validation
4. Stream output for large result sets

## References

- **FIBO Specification**: https://spec.edmcouncil.org/fibo/
- **FIBO on GitHub**: https://github.com/edmcouncil/fibo
- **EDM Council**: https://edmcouncil.org/frameworks/industry-models/fibo/
- **OWL 2 Specification**: https://www.w3.org/TR/owl2-overview/
- **Turtle Format**: https://www.w3.org/TR/turtle/

## Troubleshooting

### IRI Validation Failing

**Problem**: `{error, {unknown_namespace, NS}}`

**Cause**: Namespace not in `?FIBO_NS_IRI_MAP`

**Solution**: Add mapping in module defines or use full namespace prefix

### Terms Not Detected as Financial

**Problem**: Custom financial term not flagged as missing FIBO alignment

**Cause**: Keyword not in `?FINANCIAL_DOMAINS`

**Solution**: Add term to financial domains list or add custom domain detection logic

### Duplicate Violations

**Problem**: Same violation reported multiple times

**Cause**: Term appears on multiple lines

**Solution**: Expected behavior - each occurrence is validated independently

## Future Enhancements

1. **IRI HTTP Validation**: Actually resolve IRIs to FIBO specification
2. **SHACL Validation**: Validate against FIBO SHACL shapes
3. **Semantic Reasoner**: Infer missing FIBO alignments using OWL reasoning
4. **Performance Metrics**: Measure alignment quality and coverage
5. **Custom Mappings**: User-defined domain-to-FIBO mappings
6. **Multi-Language Support**: Validation in multiple semantic languages
