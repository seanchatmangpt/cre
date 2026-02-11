%%%-------------------------------------------------------------------
%%% @doc Common Test Suite for FIBO Cloud-First Linter
%%% Tests IRI resolution validation and financial domain term checking
%%% @end
%%%-------------------------------------------------------------------
-module(fibo_cloud_first_linter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_validate_fibo_iri_valid/1,
    test_validate_fibo_iri_invalid_namespace/1,
    test_validate_fibo_iri_invalid_format/1,
    test_resolve_term_iri_loan/1,
    test_resolve_term_iri_fnd/1,
    test_resolve_term_iri_be/1,
    test_resolve_term_iri_unknown_namespace/1,
    test_lint_ontology_with_fibo_terms/1,
    test_lint_ontology_with_custom_terms/1,
    test_lint_ontology_dir/1,
    test_validate_term_financial_domain/1,
    test_validate_term_financial_domain_missing_fibo/1,
    test_generate_proof_with_violations/1,
    test_generate_proof_compliant/1,
    test_financial_domain_detection/1,
    test_iri_validation_count/1
]).

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [
        test_validate_fibo_iri_valid,
        test_validate_fibo_iri_invalid_namespace,
        test_validate_fibo_iri_invalid_format,
        test_resolve_term_iri_loan,
        test_resolve_term_iri_fnd,
        test_resolve_term_iri_be,
        test_resolve_term_iri_unknown_namespace,
        test_lint_ontology_with_fibo_terms,
        test_lint_ontology_with_custom_terms,
        test_lint_ontology_dir,
        test_validate_term_financial_domain,
        test_validate_term_financial_domain_missing_fibo,
        test_generate_proof_with_violations,
        test_generate_proof_compliant,
        test_financial_domain_detection,
        test_iri_validation_count
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases - IRI Validation
%%====================================================================

test_validate_fibo_iri_valid(Config) ->
    %% Valid FIBO IRI format
    Result = fibo_cloud_first_linter:validate_fibo_iri(<<"fibo-loan:Loan">>),
    ?assertEqual(ok, Result),
    ct:log("PASS: Valid FIBO IRI accepted", []).

test_validate_fibo_iri_invalid_namespace(Config) ->
    %% Invalid namespace (not FIBO)
    Result = fibo_cloud_first_linter:validate_fibo_iri(<<"custom:Term">>),
    ?assertMatch({error, {not_fibo_namespace, _}}, Result),
    ct:log("PASS: Non-FIBO namespace rejected", []).

test_validate_fibo_iri_invalid_format(Config) ->
    %% Invalid format (no colon separator)
    Result = fibo_cloud_first_linter:validate_fibo_iri(<<"InvalidTerm">>),
    ?assertMatch({error, invalid_term_format}, Result),
    ct:log("PASS: Invalid format rejected", []).

test_resolve_term_iri_loan(Config) ->
    %% Resolve FIBO Loan IRI
    Result = fibo_cloud_first_linter:resolve_term_iri(<<"fibo-loan">>, <<"Loan">>),
    ?assertMatch({ok, <<"https://spec.edmcouncil.org/fibo/ontology/LOAN", _/binary>>}, Result),
    ct:log("PASS: Loan IRI resolved correctly", []).

test_resolve_term_iri_fnd(Config) ->
    %% Resolve FIBO FND IRI
    Result = fibo_cloud_first_linter:resolve_term_iri(<<"fibo-fnd">>, <<"Party">>),
    ?assertMatch({ok, <<"https://spec.edmcouncil.org/fibo/ontology/FND", _/binary>>}, Result),
    ct:log("PASS: FND IRI resolved correctly", []).

test_resolve_term_iri_be(Config) ->
    %% Resolve FIBO BE IRI
    Result = fibo_cloud_first_linter:resolve_term_iri(<<"fibo-be">>, <<"LegalEntity">>),
    ?assertMatch({ok, <<"https://spec.edmcouncil.org/fibo/ontology/BE", _/binary>>}, Result),
    ct:log("PASS: BE IRI resolved correctly", []).

test_resolve_term_iri_unknown_namespace(Config) ->
    %% Unknown namespace
    Result = fibo_cloud_first_linter:resolve_term_iri(<<"unknown">>, <<"Term">>),
    ?assertMatch({error, {unknown_namespace, _}}, Result),
    ct:log("PASS: Unknown namespace error returned", []).

%%====================================================================
%% Test Cases - Ontology Linting
%%====================================================================

test_lint_ontology_with_fibo_terms(Config) ->
    %% Create test TTL file with FIBO terms
    TestFile = "/tmp/test_fibo_terms.ttl",
    Content = <<"@prefix fibo-loan: <https://spec.edmcouncil.org/fibo/ontology/LOAN/> .
@prefix fibo-fnd: <https://spec.edmcouncil.org/fibo/ontology/FND/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

fibo-loan:Borrower a owl:Class ;
    rdfs:label \"Borrower\" .

fibo-fnd:Party a owl:Class ;
    rdfs:label \"Party\" .
">>,
    file:write_file(TestFile, Content),

    Result = fibo_cloud_first_linter:lint_ontology(TestFile),

    ?assertMatch({ok, #{terms_checked := 2, fibo_aligned := 2}}, Result),

    file:delete(TestFile),
    ct:log("PASS: FIBO-aligned ontology linted correctly", []).

test_lint_ontology_with_custom_terms(Config) ->
    %% Create test TTL file with custom terms (should trigger violations)
    TestFile = "/tmp/test_custom_terms.ttl",
    Content = <<"@prefix custom: <http://example.com/custom#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

custom:Loan a owl:Class ;
    rdfs:label \"Loan\" .

custom:Payment a owl:Class ;
    rdfs:label \"Payment\" .
">>,
    file:write_file(TestFile, Content),

    Result = fibo_cloud_first_linter:lint_ontology(TestFile),

    ?assertMatch({ok, #{terms_checked := 2, violations := [_|_]}}, Result),

    file:delete(TestFile),
    ct:log("PASS: Custom terms with financial domain detected", []).

test_lint_ontology_dir(Config) ->
    %% Create test directory with multiple TTL files
    TestDir = "/tmp/test_ontology_dir",
    file:make_dir(TestDir),

    %% Create test file 1
    File1 = filename:join(TestDir, "ontology1.ttl"),
    Content1 = <<"@prefix fibo-loan: <https://spec.edmcouncil.org/fibo/ontology/LOAN/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

fibo-loan:Loan a owl:Class .
">>,
    file:write_file(File1, Content1),

    %% Create test file 2
    File2 = filename:join(TestDir, "ontology2.ttl"),
    Content2 = <<"@prefix custom: <http://example.com/custom#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

custom:CustomTerm a owl:Class .
">>,
    file:write_file(File2, Content2),

    Result = fibo_cloud_first_linter:lint_ontology_dir(TestDir),

    ?assertMatch({ok, [_|_]}, Result),

    file:delete(File1),
    file:delete(File2),
    file:del_dir(TestDir),

    ct:log("PASS: Directory linting processes multiple files", []).

%%====================================================================
%% Test Cases - Term Validation
%%====================================================================

test_validate_term_financial_domain(Config) ->
    %% Valid financial term with FIBO
    Result = fibo_cloud_first_linter:validate_term(<<"fibo-loan:Loan">>, <<"loan">>),
    ?assertEqual(ok, Result),
    ct:log("PASS: Valid financial FIBO term accepted", []).

test_validate_term_financial_domain_missing_fibo(Config) ->
    %% Financial term not using FIBO
    Result = fibo_cloud_first_linter:validate_term(<<"custom:Loan">>, <<"loan">>),
    ?assertMatch({error, {missing_fibo_alignment, _, _}}, Result),
    ct:log("PASS: Custom financial term rejected", []).

%%====================================================================
%% Test Cases - Proof Generation
%%====================================================================

test_generate_proof_with_violations(Config) ->
    LintResult = #{
        terms_checked => 10,
        fibo_aligned => 7,
        cloud_aligned => 2,
        custom_justified => 1,
        custom_unjustified => 1,
        iri_validated => 7,
        iri_invalid => 0,
        violations => [
            #{type => missing_fibo_alignment, term => <<"custom:Loan">>},
            #{type => missing_justification, term => <<"custom:CustomTerm">>}
        ]
    },

    Proof = fibo_cloud_first_linter:generate_proof(LintResult),

    ?assertEqual(false, maps:get(compliant, Proof)),
    ?assertEqual(2, maps:get(violations_count, Proof)),
    ?assertEqual(7, maps:get(iri_validated, Proof)),
    ct:log("Proof: ~p", [Proof]),
    ct:log("PASS: Proof generated with violation details", []).

test_generate_proof_compliant(Config) ->
    LintResult = #{
        terms_checked => 10,
        fibo_aligned => 10,
        cloud_aligned => 0,
        custom_justified => 0,
        custom_unjustified => 0,
        iri_validated => 10,
        iri_invalid => 0,
        violations => []
    },

    Proof = fibo_cloud_first_linter:generate_proof(LintResult),

    ?assertEqual(true, maps:get(compliant, Proof)),
    ?assertEqual(0, maps:get(violations_count, Proof)),
    ct:log("PASS: Compliant ontology proof generated", []).

%%====================================================================
%% Test Cases - Domain Detection
%%====================================================================

test_financial_domain_detection(Config) ->
    %% Test financial domain keywords
    FinancialKeywords = [
        <<"Loan">>,
        <<"Mortgage">>,
        <<"Borrower">>,
        <<"Payment">>,
        <<"Account">>,
        <<"Transaction">>
    ],

    %% Each should be detected as financial domain
    Results = lists:map(fun(Keyword) ->
        case fibo_cloud_first_linter:validate_term(<<"fibo-loan:", Keyword/binary>>, <<"financial">>) of
            ok -> {Keyword, pass};
            {error, _} -> {Keyword, fail}
        end
    end, FinancialKeywords),

    ?assertEqual(6, length([R || {_, pass} <- Results])),
    ct:log("Financial keywords detected: ~p", [Results]),
    ct:log("PASS: Financial domain detection working", []).

test_iri_validation_count(Config) ->
    %% Verify IRI validation counts are accurate
    TestFile = "/tmp/test_iri_count.ttl",
    Content = <<"@prefix fibo-loan: <https://spec.edmcouncil.org/fibo/ontology/LOAN/> .
@prefix fibo-fnd: <https://spec.edmcouncil.org/fibo/ontology/FND/> .
@prefix custom: <http://example.com/custom#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

fibo-loan:Loan a owl:Class .
fibo-loan:Borrower a owl:Class .
fibo-fnd:Party a owl:Class .
custom:CustomLoan a owl:Class .
">>,
    file:write_file(TestFile, Content),

    {ok, Result} = fibo_cloud_first_linter:lint_ontology(TestFile),

    %% Should have 3 FIBO-aligned and 3 IRIs validated
    ?assertEqual(3, maps:get(fibo_aligned, Result)),
    ?assertEqual(3, maps:get(iri_validated, Result)),

    file:delete(TestFile),
    ct:log("PASS: IRI validation counts accurate", []).
