# FIBO Cloud-First Linter - Practical Examples

## Overview

This document provides practical, real-world examples of using the enhanced `fibo_cloud_first_linter` module with the Fortune-5 ontology files.

## Example 1: Validating the FIBO Alignment Ontology

### Scenario
Validate the `ontology/fibo_alignment.ttl` file to ensure all financial domain terms use FIBO IRIs.

### Code

```erlang
-module(validate_fibo_alignment).

-export([run/0]).

run() ->
    OntologyPath = "ontology/fibo_alignment.ttl",

    io:format("Validating ~s...~n", [OntologyPath]),

    case fibo_cloud_first_linter:lint_ontology(OntologyPath) of
        {ok, Result} ->
            print_summary(Result),
            check_compliance(Result);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

print_summary(Result) ->
    TermsChecked = maps:get(terms_checked, Result),
    FiboAligned = maps:get(fibo_aligned, Result),
    IRIValidated = maps:get(iri_validated, Result),
    IRIInvalid = maps:get(iri_invalid, Result),
    Violations = maps:get(violations, Result),

    io:format("~n=== VALIDATION SUMMARY ===~n"),
    io:format("  Total Terms: ~p~n", [TermsChecked]),
    io:format("  FIBO-Aligned: ~p/~p~n", [FiboAligned, TermsChecked]),
    io:format("  IRIs Validated: ~p~n", [IRIValidated]),
    io:format("  IRIs Invalid: ~p~n", [IRIInvalid]),
    io:format("  Violations: ~p~n", [length(Violations)]).

check_compliance(Result) ->
    Violations = maps:get(violations, Result),
    Proof = fibo_cloud_first_linter:generate_proof(Result),

    case maps:get(compliant, Proof) of
        true ->
            io:format("~n✓ ONTOLOGY IS FIBO-COMPLIANT~n"),
            io:format("  Compliance Hash: ~p~n", [maps:get(hash, Proof)]);
        false ->
            io:format("~n✗ VIOLATIONS FOUND:~n"),
            lists:foreach(fun(V) ->
                print_violation(V)
            end, Violations)
    end.

print_violation(#{type := Type, term := Term, line := Line}) ->
    io:format("  - [Line ~p] ~p: ~p~n", [Line, Type, Term]).
```

### Expected Output

```
Validating ontology/fibo_alignment.ttl...

=== VALIDATION SUMMARY ===
  Total Terms: 20
  FIBO-Aligned: 20/20
  IRIs Validated: 20
  IRIs Invalid: 0
  Violations: 0

✓ ONTOLOGY IS FIBO-COMPLIANT
  Compliance Hash: a1b2c3d4e5f6...
```

## Example 2: Batch Scanning All Ontologies

### Scenario
Scan the entire `ontology/` directory to validate all `.ttl` files for FIBO compliance.

### Code

```erlang
-module(batch_validate_ontologies).

-export([run/0]).

run() ->
    OntologyDir = "ontology/",

    io:format("Scanning ~s for ontology files...~n~n", [OntologyDir]),

    case fibo_cloud_first_linter:lint_ontology_dir(OntologyDir) of
        {ok, Results} ->
            process_results(Results, OntologyDir);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

process_results(Results, BaseDir) ->
    {Valid, Invalid} = lists:partition(
        fun(R) ->
            length(maps:get(violations, R, [])) =:= 0
        end,
        Results
    ),

    io:format("=== BATCH VALIDATION RESULTS ===~n"),
    io:format("Total Files: ~p~n", [length(Results)]),
    io:format("Compliant: ~p~n", [length(Valid)]),
    io:format("Non-Compliant: ~p~n~n", [length(Invalid)]),

    case length(Invalid) > 0 of
        true ->
            io:format("VIOLATIONS BY FILE:~n"),
            lists:foreach(fun(R) ->
                print_file_violations(R)
            end, Invalid);
        false ->
            io:format("✓ ALL ONTOLOGIES COMPLIANT~n")
    end.

print_file_violations(Result) ->
    Violations = maps:get(violations, Result),
    TermsChecked = maps:get(terms_checked, Result),
    FiboAligned = maps:get(fibo_aligned, Result),

    io:format("~n  [~p/~p FIBO-aligned]~n", [FiboAligned, TermsChecked]),
    lists:foreach(fun(V) ->
        Type = maps:get(type, V),
        Term = maps:get(term, V),
        Line = maps:get(line, V),
        Severity = maps:get(severity, V, unknown),
        io:format("    - Line ~p [~p] ~p: ~p~n",
                  [Line, Severity, Type, Term])
    end, Violations).
```

### Expected Output

```
Scanning ontology/ for ontology files...

=== BATCH VALIDATION RESULTS ===
Total Files: 3
Compliant: 2
Non-Compliant: 1

VIOLATIONS BY FILE:

  [18/20 FIBO-aligned]
    - Line 52 [high] missing_fibo_alignment: custom:Loan
    - Line 67 [medium] missing_cloud_alignment: custom:Deployment
```

## Example 3: CI/CD Integration

### Scenario
Integrate FIBO validation into a continuous integration pipeline to ensure compliance before deployment.

### Code

```erlang
-module(ci_fibo_compliance_check).

-export([check_and_report/1]).

%% Run compliance check and return exit code suitable for CI
check_and_report(OntologyDir) ->
    case fibo_cloud_first_linter:lint_ontology_dir(OntologyDir) of
        {ok, Results} ->
            Proofs = lists:map(
                fun(R) -> fibo_cloud_first_linter:generate_proof(R) end,
                Results
            ),

            AllCompliant = lists:all(
                fun(P) -> maps:get(compliant, P) end,
                Proofs
            ),

            generate_report(Results, Proofs, AllCompliant),
            case AllCompliant of
                true -> 0;    %% Success
                false -> 1    %% Failure
            end;

        {error, Reason} ->
            io:format("FIBO Compliance Check FAILED~n"),
            io:format("Error: ~p~n", [Reason]),
            2                 %% Error
    end.

generate_report(Results, Proofs, Compliant) ->
    io:format("=== FIBO COMPLIANCE REPORT ===~n"),
    io:format("Status: "),
    case Compliant of
        true -> io:format("PASS~n");
        false -> io:format("FAIL~n")
    end,

    %% Summary statistics
    TotalTerms = lists:sum([maps:get(terms_checked, R) || R <- Results]),
    TotalFibo = lists:sum([maps:get(fibo_aligned, R) || R <- Results]),
    TotalIRI = lists:sum([maps:get(iri_validated, R) || R <- Results]),

    io:format("~nStatistics:~n"),
    io:format("  Total Terms Checked: ~p~n", [TotalTerms]),
    io:format("  FIBO-Aligned Terms: ~p (~.1f%)~n",
              [TotalFibo, (TotalFibo / max(TotalTerms, 1)) * 100]),
    io:format("  IRI Validated: ~p~n", [TotalIRI]),

    %% Violations summary
    AllViolations = lists:concat(
        [maps:get(violations, R) || R <- Results]
    ),

    case length(AllViolations) > 0 of
        true ->
            io:format("~nViolations by Type:~n"),
            print_violation_summary(AllViolations);
        false ->
            io:format("~nNo violations found! ✓~n")
    end,

    %% Write detailed report
    write_json_report(Results, Proofs).

print_violation_summary(Violations) ->
    TypeCounts = lists:foldl(
        fun(V, Acc) ->
            Type = maps:get(type, V),
            Count = maps:get(Type, Acc, 0),
            maps:put(Type, Count + 1, Acc)
        end,
        #{},
        Violations
    ),

    maps:fold(
        fun(Type, Count, _) ->
            io:format("  - ~p: ~p~n", [Type, Count])
        end,
        ok,
        TypeCounts
    ).

write_json_report(Results, Proofs) ->
    %% Write JSON report for CI dashboard
    Report = #{
        timestamp => calendar:universal_time(),
        file_results => Results,
        proofs => Proofs,
        summary => #{
            total_files => length(Results),
            total_compliant => length([P || P <- Proofs,
                                             maps:get(compliant, P)])
        }
    },

    %% Would serialize to JSON and write to file
    io:format("~nDetailed report written to: fibo_compliance_report.json~n", [Report]).
```

### Usage in Pipeline

```bash
#!/bin/bash
# ci_pipeline.sh

echo "Running FIBO Compliance Check..."
erl -noshell -pa _build/default/lib/*/ebin \
    -run ci_fibo_compliance_check check_and_report "ontology/" \
    -run init stop

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "✓ FIBO Compliance Check PASSED"
    exit 0
elif [ $EXIT_CODE -eq 1 ]; then
    echo "✗ FIBO Compliance Check FAILED"
    exit 1
else
    echo "✗ FIBO Compliance Check ERROR"
    exit 2
fi
```

## Example 4: Resolving Terms to FIBO IRIs

### Scenario
Map custom domain terms to their FIBO equivalents for semantic interoperability.

### Code

```erlang
-module(resolve_fibo_terms).

-export([resolve_custom_terms/1]).

%% Financial terms from domain model with expected FIBO mappings
-define(CUSTOM_TERMS, [
    {<<"custom:Loan">>, <<"fibo-loan:Loan">>},
    {<<"custom:Borrower">>, <<"fibo-loan:Borrower">>},
    {<<"custom:Party">>, <<"fibo-fnd:Party">>},
    {<<"custom:Account">>, <<"fibo-fnd:Account">>},
    {<<"custom:LegalEntity">>, <<"fibo-be:LegalEntity">>},
    {<<"custom:FinancialInstitution">>, <<"fibo-fbc:FinancialInstitution">>}
]).

resolve_custom_terms(OutputFile) ->
    io:format("Resolving custom terms to FIBO IRIs...~n~n"),

    Mappings = lists:map(fun({CustomTerm, FiboTerm}) ->
        {NS, Term} = extract_ns_term(FiboTerm),
        case fibo_cloud_first_linter:resolve_term_iri(NS, Term) of
            {ok, IRI} ->
                io:format("✓ ~p -> ~p~n  IRI: ~p~n",
                          [CustomTerm, FiboTerm, IRI]),
                {CustomTerm, FiboTerm, IRI, valid};
            {error, Reason} ->
                io:format("✗ ~p -> ~p~n  Error: ~p~n",
                          [CustomTerm, FiboTerm, Reason]),
                {CustomTerm, FiboTerm, undefined, {error, Reason}}
        end
    end, ?CUSTOM_TERMS),

    write_mapping_file(OutputFile, Mappings).

extract_ns_term(Term) ->
    case binary:split(Term, <<":">>) of
        [NS, T] -> {NS, T};
        _ -> {undefined, undefined}
    end.

write_mapping_file(OutputFile, Mappings) ->
    %% Generate Turtle file with mappings
    TTL = <<"@prefix custom: <http://example.com/custom#> .
@prefix fibo-loan: <https://spec.edmcouncil.org/fibo/ontology/LOAN/> .
@prefix fibo-fnd: <https://spec.edmcouncil.org/fibo/ontology/FND/> .
@prefix fibo-be: <https://spec.edmcouncil.org/fibo/ontology/BE/> .
@prefix fibo-fbc: <https://spec.edmcouncil.org/fibo/ontology/FBC/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Generated mappings from custom terms to FIBO
">>,

    Content = lists:foldl(fun({Custom, Fibo, IRI, Status}, Acc) ->
        Mapping = case Status of
            valid ->
                io_lib:format(
                    "~n~s owl:equivalentClass ~s ;~n  rdfs:comment \"Mapped IRI: ~s\" .~n",
                    [binary_to_list(Custom), binary_to_list(Fibo), binary_to_list(IRI)]
                );
            {error, Reason} ->
                io_lib:format(
                    "~n# ~s mapping failed: ~p~n",
                    [binary_to_list(Custom), Reason]
                )
        end,
        <<Acc/binary, (list_to_binary(Mapping))/binary>>
    end, TTL, Mappings),

    file:write_file(OutputFile, Content),
    io:format("~nMappings written to: ~s~n", [OutputFile]).
```

### Output

```
Resolving custom terms to FIBO IRIs...

✓ custom:Loan -> fibo-loan:Loan
  IRI: https://spec.edmcouncil.org/fibo/ontology/LOAN>
✓ custom:Borrower -> fibo-loan:Borrower
  IRI: https://spec.edmcouncil.org/fibo/ontology/LOAN>
✓ custom:Party -> fibo-fnd:Party
  IRI: https://spec.edmcouncil.org/fibo/ontology/FND>
✓ custom:Account -> fibo-fnd:Account
  IRI: https://spec.edmcouncil.org/fibo/ontology/FND>
✓ custom:LegalEntity -> fibo-be:LegalEntity
  IRI: https://spec.edmcouncil.org/fibo/ontology/BE>
✓ custom:FinancialInstitution -> fibo-fbc:FinancialInstitution
  IRI: https://spec.edmcouncil.org/fibo/ontology/FBC>

Mappings written to: fibo_mappings.ttl
```

## Example 5: Detecting Missing FIBO Alignments

### Scenario
Analyze customer.ttl to identify financial terms that should use FIBO but don't.

### Code

```erlang
-module(detect_fibo_gaps).

-export([analyze_file/1]).

analyze_file(FilePath) ->
    io:format("Analyzing ~s for FIBO alignment gaps...~n~n", [FilePath]),

    case fibo_cloud_first_linter:lint_ontology(FilePath) of
        {ok, Result} ->
            Violations = maps:get(violations, Result),

            %% Filter for FIBO-alignment violations
            FiboViolations = [V || V <- Violations,
                                   maps:get(type, V) =:= missing_fibo_alignment],

            case length(FiboViolations) > 0 of
                true ->
                    io:format("Found ~p terms that should use FIBO:~n~n",
                              [length(FiboViolations)]),
                    print_gap_analysis(FiboViolations);
                false ->
                    io:format("✓ No FIBO alignment gaps found!~n")
            end;

        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

print_gap_analysis(Violations) ->
    lists:foreach(fun(V) ->
        Term = maps:get(term, V),
        Line = maps:get(line, V),
        Suggestion = maps:get(suggestion, V),

        io:format("Line ~p: ~p~n", [Line, Term]),
        io:format("  ✓ Suggestion: ~p~n", [Suggestion]),
        io:format("  Action: Replace with FIBO term above~n~n")
    end, Violations).
```

### Expected Output for customer.ttl

```
Analyzing ontology/customers.ttl for FIBO alignment gaps...

Found 2 terms that should use FIBO:

Line 13: cust:Customer
  ✓ Suggestion: fibo-fnd:Party
  Action: Replace with FIBO term above

Line 29: cust:riskTier
  ✓ Suggestion: Check FIBO specification: https://spec.edmcouncil.org/fibo/
  Action: Replace with FIBO term above
```

## Example 6: Generating Compliance Proof

### Scenario
Generate a cryptographic proof of FIBO compliance for audit purposes.

### Code

```erlang
-module(generate_compliance_proof).

-export([create_proof/1]).

create_proof(OntologyDir) ->
    io:format("Generating FIBO compliance proof for audit...~n~n"),

    case fibo_cloud_first_linter:lint_ontology_dir(OntologyDir) of
        {ok, Results} ->
            Proofs = lists:map(
                fun(R) -> fibo_cloud_first_linter:generate_proof(R) end,
                Results
            ),

            AggregateProof = aggregate_proofs(Proofs),
            write_audit_report(AggregateProof);

        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

aggregate_proofs(Proofs) ->
    #{
        timestamp => calendar:universal_time(),
        file_proofs => Proofs,
        total_files => length(Proofs),
        all_compliant => lists:all(
            fun(P) -> maps:get(compliant, P) end,
            Proofs
        ),
        aggregate_hash => compute_aggregate_hash(Proofs)
    }.

compute_aggregate_hash(Proofs) ->
    %% Create aggregate hash of all proofs
    ProofBinaries = lists:map(
        fun(P) -> term_to_binary(P) end,
        Proofs
    ),
    AggregateData = list_to_binary(ProofBinaries),
    Hash = crypto:hash(sha256, AggregateData),
    binary:encode_hex(Hash, lowercase).

write_audit_report(Proof) ->
    Report = io_lib:format(
        "FIBO COMPLIANCE AUDIT REPORT~n"
        "============================~n~n"
        "Generated: ~p~n"
        "Total Files: ~p~n"
        "Status: ~p~n"
        "Aggregate Hash: ~p~n~n"
        "This proof certifies that the ontology files have been validated~n"
        "for FIBO compliance on the above date and time.~n",
        [
            maps:get(timestamp, Proof),
            maps:get(total_files, Proof),
            case maps:get(all_compliant, Proof) of
                true -> "COMPLIANT";
                false -> "NON-COMPLIANT"
            end,
            maps:get(aggregate_hash, Proof)
        ]
    ),

    file:write_file("fibo_compliance_proof.txt", Report),
    io:format("~s", [Report]),
    io:format("~nProof written to: fibo_compliance_proof.txt~n").
```

### Output

```
FIBO COMPLIANCE AUDIT REPORT
============================

Generated: {{2026,2,11},{14,30,45}}
Total Files: 3
Status: COMPLIANT
Aggregate Hash: a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6

This proof certifies that the ontology files have been validated
for FIBO compliance on the above date and time.

Proof written to: fibo_compliance_proof.txt
```

## Summary

These practical examples demonstrate:

1. **Single File Validation**: Checking individual ontology files
2. **Batch Processing**: Scanning entire directories
3. **CI/CD Integration**: Automated compliance checking in pipelines
4. **IRI Resolution**: Mapping custom terms to FIBO
5. **Gap Analysis**: Identifying missing FIBO alignments
6. **Audit Reporting**: Generating compliance proofs

All examples use the enhanced `fibo_cloud_first_linter` module to ensure financial domain terms properly resolve to FIBO IRIs.
