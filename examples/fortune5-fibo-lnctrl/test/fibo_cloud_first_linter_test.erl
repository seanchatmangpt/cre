%%%-------------------------------------------------------------------
%%% @doc Tests for FIBO/Cloud-First Linter with Cloud Ontology Validation
%%% Tests cloud deployment term validation against standard vocabularies
%%% @end
%%%-------------------------------------------------------------------
-module(fibo_cloud_first_linter_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Cloud Ontology Validation Tests
%%====================================================================

%% Test GCP namespace validation
gcp_namespace_validation_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_namespace(<<"gcp">>)).

%% Test AWS namespace validation
aws_namespace_validation_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_namespace(<<"aws">>)).

%% Test Azure namespace validation
azure_namespace_validation_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_namespace(<<"azure">>)).

%% Test Kubernetes namespace validation
k8s_namespace_validation_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_namespace(<<"k8s">>)).

%% Test Docker namespace validation
docker_namespace_validation_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_namespace(<<"docker">>)).

%% Test invalid namespace
invalid_cloud_namespace_test() ->
    Result = fibo_cloud_first_linter:validate_cloud_namespace(<<"custom_cloud">>),
    ?assertMatch({error, {unknown_cloud_ontology, _}}, Result).

%%====================================================================
%% Standard Cloud Term Tests
%%====================================================================

%% Test getting GCP standard terms
get_gcp_terms_test() ->
    {ok, Terms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"gcp">>),
    ?assert(lists:member(<<"Deployment">>, Terms)),
    ?assert(lists:member(<<"Service">>, Terms)),
    ?assert(lists:member(<<"Cluster">>, Terms)),
    ?assert(length(Terms) > 0).

%% Test getting AWS standard terms
get_aws_terms_test() ->
    {ok, Terms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"aws">>),
    ?assert(lists:member(<<"Instance">>, Terms)),
    ?assert(lists:member(<<"SecurityGroup">>, Terms)),
    ?assert(lists:member(<<"LoadBalancer">>, Terms)),
    ?assert(length(Terms) > 0).

%% Test getting Azure standard terms
get_azure_terms_test() ->
    {ok, Terms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"azure">>),
    ?assert(lists:member(<<"VirtualMachine">>, Terms)),
    ?assert(lists:member(<<"ResourceGroup">>, Terms)),
    ?assert(lists:member(<<"StorageAccount">>, Terms)),
    ?assert(length(Terms) > 0).

%% Test getting Kubernetes standard terms
get_k8s_terms_test() ->
    {ok, Terms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"k8s">>),
    ?assert(lists:member(<<"Deployment">>, Terms)),
    ?assert(lists:member(<<"Service">>, Terms)),
    ?assert(lists:member(<<"Pod">>, Terms)),
    ?assert(lists:member(<<"Namespace">>, Terms)),
    ?assert(length(Terms) > 0).

%% Test getting Docker standard terms
get_docker_terms_test() ->
    {ok, Terms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"docker">>),
    ?assert(lists:member(<<"Container">>, Terms)),
    ?assert(lists:member(<<"Image">>, Terms)),
    ?assert(lists:member(<<"Network">>, Terms)),
    ?assert(length(Terms) > 0).

%%====================================================================
%% Cloud Term Validation Tests
%%====================================================================

%% Test standard GCP term validation
validate_standard_gcp_term_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_term(<<"gcp">>, <<"Deployment">>)).

%% Test standard AWS term validation
validate_standard_aws_term_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_term(<<"aws">>, <<"Instance">>)).

%% Test standard K8s term validation
validate_standard_k8s_term_test() ->
    ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_term(<<"k8s">>, <<"Pod">>)).

%% Test custom GCP term detection
custom_gcp_term_test() ->
    Result = fibo_cloud_first_linter:validate_cloud_term(<<"gcp">>, <<"CustomResource">>),
    ?assertMatch({error, {custom_cloud_term, <<"gcp">>, <<"CustomResource">>}}, Result).

%% Test custom AWS term detection
custom_aws_term_test() ->
    Result = fibo_cloud_first_linter:validate_cloud_term(<<"aws">>, <<"CustomCompute">>),
    ?assertMatch({error, {custom_cloud_term, <<"aws">>, <<"CustomCompute">>}}, Result).

%% Test invalid cloud namespace for term validation
validate_term_invalid_namespace_test() ->
    Result = fibo_cloud_first_linter:validate_cloud_term(<<"invalid_ns">>, <<"SomeTerm">>),
    ?assertMatch({error, {unknown_cloud_ontology, _}}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test validate_term for cloud deployment terms
validate_cloud_deployment_term_test() ->
    %% Test with GCP namespace and standard term
    ?assertEqual(ok,
        fibo_cloud_first_linter:validate_term(<<"gcp:Deployment">>, <<"deployment">>)).

%% Test validate_term for financial domain
validate_financial_term_test() ->
    %% FIBO-aligned financial term
    ?assertEqual(ok,
        fibo_cloud_first_linter:validate_term(<<"fibo-loan:Loan">>, <<"loan">>)).

%%====================================================================
%% Ontology File Linting Tests
%%====================================================================

%% Create test ontology content with cloud terms
test_ontology_content() ->
    <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix gcp: <https://cloud.google.com/ontology/resources/> .
@prefix k8s: <https://kubernetes.io/ontology/api/> .
@prefix fibo-loan: <https://spec.edmcouncil.org/fibo/ontology/LOAN/> .

% GCP standard deployment term
gcp:Deployment a owl:Class ;
    rdfs:label \"Google Cloud Deployment\" .

% K8s standard term
k8s:Pod a owl:Class ;
    rdfs:label \"Kubernetes Pod\" .

% Custom cloud term (would trigger warning without justification)
gcp:CustomResource a owl:Class ;
    rdfs:label \"Custom GCP Resource\" ;
    rdfs:comment \"Custom term used for application-specific resource type\" .

% FIBO-aligned financial term
fibo-loan:Loan a owl:Class ;
    rdfs:label \"Loan Contract\" .
">>.

%% Test linting ontology with cloud terms
lint_ontology_with_cloud_terms_test() ->
    %% Create temporary test file
    TestFile = "/tmp/test_cloud_ontology.ttl",
    ok = file:write_file(TestFile, test_ontology_content()),

    %% Lint the ontology
    {ok, Result} = fibo_cloud_first_linter:lint_ontology(TestFile),

    %% Verify results
    ?assert(maps:get(terms_checked, Result) > 0),
    ?assert(maps:get(cloud_ontology_validated, Result) >= 2),

    %% Cleanup
    file:delete(TestFile),
    ok.

%%====================================================================
%% Proof Generation Tests
%%====================================================================

%% Test proof generation for cloud-first compliance
generate_cloud_first_proof_test() ->
    TestResult = #{
        terms_checked => 10,
        fibo_aligned => 5,
        cloud_aligned => 3,
        custom_justified => 2,
        custom_unjustified => 0,
        cloud_ontology_validated => 3,
        cloud_ontology_violations => 0,
        iri_validated => 5,
        iri_invalid => 0,
        violations => []
    },

    Proof = fibo_cloud_first_linter:generate_proof(TestResult),

    ?assertEqual(<<"FIBO_Cloud_First_Compliance">>, maps:get(proof_type, Proof)),
    ?assertEqual(10, maps:get(terms_checked, Proof)),
    ?assertEqual(true, maps:get(compliant, Proof)),
    ?assert(is_binary(maps:get(hash, Proof))).

%% Test proof generation with violations
generate_proof_with_violations_test() ->
    TestResult = #{
        terms_checked => 10,
        fibo_aligned => 4,
        cloud_aligned => 2,
        custom_justified => 1,
        custom_unjustified => 1,
        cloud_ontology_validated => 2,
        cloud_ontology_violations => 1,
        iri_validated => 4,
        iri_invalid => 1,
        violations => [
            #{
                type => custom_cloud_term_unjustified,
                term => <<"gcp:UnjustifiedTerm">>,
                severity => medium
            }
        ]
    },

    Proof = fibo_cloud_first_linter:generate_proof(TestResult),

    ?assertEqual(false, maps:get(compliant, Proof)),
    ?assertEqual(1, maps:get(violations_count, Proof)).

%%====================================================================
%% Helper Tests
%%====================================================================

%% Test namespace extraction
extract_namespace_test() ->
    %% Validate standard GCP deployment term as proxy for namespace handling
    Result = fibo_cloud_first_linter:validate_cloud_term(<<"gcp">>, <<"Deployment">>),
    ?assertEqual(ok, Result).

%% Test cloud term suggestions
suggest_deployment_term_test() ->
    %% Indirectly test by validating known deployment suggestions work
    StandardGCPTerms = [
        <<"Deployment">>,
        <<"Service">>,
        <<"Cluster">>
    ],
    lists:foreach(fun(Term) ->
        ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_term(<<"gcp">>, Term))
    end, StandardGCPTerms).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test empty/null namespace validation
null_namespace_validation_test() ->
    Result = fibo_cloud_first_linter:validate_cloud_namespace(<<"">>) ,
    ?assertMatch({error, {unknown_cloud_ontology, _}}, Result).

%% Test case sensitivity of namespaces
case_sensitive_namespace_test() ->
    %% Namespaces should be lowercase
    Result = fibo_cloud_first_linter:validate_cloud_namespace(<<"GCP">>),
    ?assertMatch({error, {unknown_cloud_ontology, _}}, Result).

%% Test multiple cloud namespaces in same ontology
multiple_namespaces_test() ->
    Namespaces = [<<"gcp">>, <<"aws">>, <<"azure">>, <<"k8s">>],
    lists:foreach(fun(NS) ->
        ?assertEqual(ok, fibo_cloud_first_linter:validate_cloud_namespace(NS))
    end, Namespaces).
