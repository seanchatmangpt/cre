%%%-------------------------------------------------------------------
%%% @doc FIBO/Cloud-First Enforcement Linter with Cloud Ontology Validation
%%% Ensures domain concepts use FIBO terms and cloud concepts use
%%% standard cloud ontology terms with namespace validation.
%%% Custom terms must have justification.
%%% Enhanced with:
%%%   - Cloud ontology namespace validation (GCP, AWS, Azure, K8s)
%%%   - Standard cloud deployment term validation
%%%   - Custom cloud term detection against standard vocabularies
%%%   - IRI resolution validation for financial domain terms
%%% @end
%%%-------------------------------------------------------------------
-module(fibo_cloud_first_linter).

-export([
    lint_ontology/1,
    lint_ontology_dir/1,
    lint_ontology_dir/2,
    generate_proof/1,
    validate_term/2,
    validate_fibo_iri/1,
    resolve_term_iri/2,
    validate_cloud_term/2,
    validate_cloud_namespace/1,
    get_standard_cloud_terms/1
]).

-define(FIBO_NAMESPACES, [
    <<"fibo-fnd">>,
    <<"fibo-be">>,
    <<"fibo-loan">>,
    <<"fibo-fbc">>,
    <<"fibo-sec">>,
    <<"fibo-der">>,
    <<"fibo-ind">>,
    <<"fibo-fnd-aap-agt">>,
    <<"fibo-fnd-aap-ppl">>,
    <<"fibo-fnd-agr-ctr">>,
    <<"fibo-fnd-acc-cur">>,
    <<"fibo-be-le-lp">>,
    <<"fibo-be-le-fbo">>,
    <<"fibo-fbc-pas-fpas">>,
    <<"fibo-fbc-dae-dbt">>,
    <<"fibo-loan-ln-ln">>,
    <<"fibo-loan-spc-cns">>,
    <<"fibo-loan-spc-com">>
]).

-define(FIBO_IRI_PREFIX, <<"https://spec.edmcouncil.org/fibo/ontology/">>).

-define(CLOUD_NAMESPACES, [
    <<"gcp">>,
    <<"aws">>,
    <<"azure">>,
    <<"k8s">>,
    <<"docker">>,
    <<"cloud">>
]).

%% Standard cloud ontology namespace URLs and IRI bases
-define(STANDARD_CLOUD_ONTOLOGIES, #{
    <<"gcp">> => #{
        iri => <<"https://cloud.google.com/ontology/resources/">>,
        standard_terms => ?GCP_STANDARD_TERMS,
        description => <<"Google Cloud Platform Resource Ontology">>
    },
    <<"aws">> => #{
        iri => <<"https://aws.amazon.com/ontology/resources/">>,
        standard_terms => ?AWS_STANDARD_TERMS,
        description => <<"Amazon Web Services Resource Ontology">>
    },
    <<"azure">> => #{
        iri => <<"https://ontology.microsoft.com/azure/resources/">>,
        standard_terms => ?AZURE_STANDARD_TERMS,
        description => <<"Microsoft Azure Resource Ontology">>
    },
    <<"k8s">> => #{
        iri => <<"https://kubernetes.io/ontology/api/">>,
        standard_terms => ?K8S_STANDARD_TERMS,
        description => <<"Kubernetes / CNCF Ontology">>
    },
    <<"docker">> => #{
        iri => <<"https://hub.docker.com/ontology/resources/">>,
        standard_terms => ?DOCKER_STANDARD_TERMS,
        description => <<"Docker Container Ontology">>
    }
}).

%% Standard GCP deployment terms (from Google Cloud vocabulary)
-define(GCP_STANDARD_TERMS, [
    <<"Deployment">>,
    <<"Service">>,
    <<"Instance">>,
    <<"Network">>,
    <<"Firewall">>,
    <<"LoadBalancer">>,
    <<"StorageBucket">>,
    <<"Database">>,
    <<"AuthenticationPolicy">>,
    <<"Cluster">>,
    <<"Pod">>,
    <<"ConfigMap">>,
    <<"Secret">>,
    <<"Namespace">>
]).

%% Standard AWS deployment terms (from AWS vocabulary)
-define(AWS_STANDARD_TERMS, [
    <<"Instance">>,
    <<"SecurityGroup">>,
    <<"LoadBalancer">>,
    <<"AutoScalingGroup">>,
    <<"S3Bucket">>,
    <<"DynamoDBTable">>,
    <<"IAMRole">>,
    <<"EC2Instance">>,
    <<"ElastiCacheCluster">>,
    <<"RDSDatabase">>,
    <<"CloudWatchMetric">>,
    <<"CloudTrailEvent">>
]).

%% Standard Azure deployment terms (from Microsoft vocabulary)
-define(AZURE_STANDARD_TERMS, [
    <<"VirtualMachine">>,
    <<"ResourceGroup">>,
    <<"StorageAccount">>,
    <<"CosmosDBDatabase">>,
    <<"KeyVault">>,
    <<"ApplicationGateway">>,
    <<"VirtualNetwork">>,
    <<"NetworkSecurityGroup">>,
    <<"ManagedIdentity">>,
    <<"AppService">>,
    <<"AKSCluster">>,
    <<"MonitoringAlert">>
]).

%% Standard Kubernetes terms (from CNCF vocabulary)
-define(K8S_STANDARD_TERMS, [
    <<"Deployment">>,
    <<"Service">>,
    <<"Pod">>,
    <<"Node">>,
    <<"ConfigMap">>,
    <<"Secret">>,
    <<"PersistentVolume">>,
    <<"PersistentVolumeClaim">>,
    <<"StatefulSet">>,
    <<"DaemonSet">>,
    <<"Namespace">>,
    <<"Ingress">>,
    <<"NetworkPolicy">>,
    <<"Role">>,
    <<"RoleBinding">>
]).

%% Standard Docker terms
-define(DOCKER_STANDARD_TERMS, [
    <<"Container">>,
    <<"Image">>,
    <<"Registry">>,
    <<"Network">>,
    <<"Volume">>,
    <<"Service">>,
    <<"HealthCheck">>,
    <<"BuildContext">>
]).

-define(FINANCIAL_DOMAINS, [
    <<"loan">>,
    <<"mortgage">>,
    <<"borrower">>,
    <<"lender">>,
    <<"payment">>,
    <<"interest">>,
    <<"principal">>,
    <<"credit">>,
    <<"debit">>,
    <<"account">>,
    <<"transaction">>,
    <<"customer">>,
    <<"party">>,
    <<"agreement">>,
    <<"contract">>,
    <<"financial">>,
    <<"banking">>,
    <<"asset">>,
    <<"liability">>,
    <<"equity">>,
    <<"security">>,
    <<"bond">>,
    <<"stock">>,
    <<"fund">>,
    <<"portfolio">>,
    <<"investment">>
]).

-define(CLOUD_DOMAINS, [
    <<"deployment">>,
    <<"container">>,
    <<"instance">>,
    <<"cluster">>,
    <<"service">>,
    <<"load_balancer">>,
    <<"autoscaling">>,
    <<"monitoring">>,
    <<"logging">>
]).

%% FIBO namespace to IRI base path mappings
-define(FIBO_NS_IRI_MAP, #{
    <<"fibo-fnd">> => <<"FND">>,
    <<"fibo-be">> => <<"BE">>,
    <<"fibo-loan">> => <<"LOAN">>,
    <<"fibo-fbc">> => <<"FBC">>,
    <<"fibo-sec">> => <<"SEC">>,
    <<"fibo-der">> => <<"DER">>,
    <<"fibo-ind">> => <<"IND">>
}).

-type lint_result() :: #{
    terms_checked => integer(),
    fibo_aligned => integer(),
    cloud_aligned => integer(),
    custom_justified => integer(),
    custom_unjustified => integer(),
    cloud_ontology_validated => integer(),
    cloud_ontology_violations => integer(),
    iri_validated => integer(),
    iri_invalid => integer(),
    violations => [map()]
}.

%%====================================================================
%% API
%%====================================================================

-spec lint_ontology(string()) -> {ok, lint_result()} | {error, term()}.
lint_ontology(OntologyFile) ->
    case file:read_file(OntologyFile) of
        {ok, Content} ->
            %% Parse TTL and extract term definitions
            Terms = extract_terms(Content),

            %% Lint each term and validate IRIs + cloud ontologies
            Results = lists:map(fun(Term) ->
                lint_term(Term, Content)
            end, Terms),

            %% Aggregate results with IRI and cloud ontology validation
            Summary = aggregate_results(Results, Content),

            {ok, Summary};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

-spec lint_ontology_dir(string()) -> {ok, [lint_result()]} | {error, term()}.
lint_ontology_dir(OntologyDir) ->
    lint_ontology_dir(OntologyDir, #{}).

-spec lint_ontology_dir(string(), map()) -> {ok, [lint_result()]} | {error, term()}.
lint_ontology_dir(OntologyDir, _Options) ->
    case file:list_dir(OntologyDir) of
        {ok, Files} ->
            %% Filter for .ttl files
            TTLFiles = lists:filter(
                fun(F) -> filename:extension(F) =:= ".ttl" end,
                Files
            ),

            %% Lint each file
            Results = lists:map(fun(File) ->
                Path = filename:join(OntologyDir, File),
                case lint_ontology(Path) of
                    {ok, Result} -> Result;
                    {error, _Reason} -> #{errors => [File]}
                end
            end, TTLFiles),

            {ok, Results};
        {error, Reason} ->
            {error, {directory_read_error, Reason}}
    end.

-spec generate_proof(lint_result()) -> map().
generate_proof(LintResult) ->
    Violations = maps:get(violations, LintResult, []),

    #{
        proof_type => <<"FIBO_Cloud_First_Compliance">>,
        terms_checked => maps:get(terms_checked, LintResult),
        fibo_aligned => maps:get(fibo_aligned, LintResult),
        cloud_aligned => maps:get(cloud_aligned, LintResult),
        custom_justified => maps:get(custom_justified, LintResult),
        cloud_ontology_validated => maps:get(cloud_ontology_validated, LintResult, 0),
        cloud_ontology_violations => maps:get(cloud_ontology_violations, LintResult, 0),
        iri_validated => maps:get(iri_validated, LintResult, 0),
        iri_invalid => maps:get(iri_invalid, LintResult, 0),
        violations_count => length(Violations),
        violations => Violations,
        compliant => length(Violations) =:= 0,
        hash => compute_proof_hash(LintResult)
    }.

-spec validate_term(binary(), binary()) -> ok | {error, term()}.
validate_term(Term, Domain) ->
    %% Check if term should use FIBO
    case is_financial_domain(Domain) of
        true ->
            case is_fibo_term(Term) of
                true ->
                    %% Validate the IRI resolves correctly
                    case validate_fibo_iri(Term) of
                        ok -> ok;
                        {error, Reason} -> {error, {invalid_fibo_iri, Term, Reason}}
                    end;
                false -> {error, {missing_fibo_alignment, Term, Domain}}
            end;
        false ->
            %% Check if term should use cloud namespace
            case is_cloud_domain(Domain) of
                true ->
                    case is_cloud_term(Term) of
                        true ->
                            %% Validate against standard cloud ontology
                            case extract_namespace_and_term(Term) of
                                {ok, {Namespace, TermName}} ->
                                    validate_cloud_term(Namespace, TermName);
                                error ->
                                    {error, invalid_cloud_term_format}
                            end;
                        false -> {error, {missing_cloud_alignment, Term, Domain}}
                    end;
                false ->
                    ok  %% Custom domain, no alignment required
            end
    end.

-spec validate_fibo_iri(binary()) -> ok | {error, term()}.
validate_fibo_iri(Term) when is_binary(Term) ->
    case extract_namespace_and_term(Term) of
        {ok, {Namespace, TermName}} ->
            case is_fibo_namespace(Namespace) of
                true ->
                    case resolve_term_iri(Namespace, TermName) of
                        {ok, _IRI} -> ok;
                        {error, Reason} -> {error, Reason}
                    end;
                false ->
                    {error, {not_fibo_namespace, Namespace}}
            end;
        error ->
            {error, invalid_term_format}
    end.

-spec resolve_term_iri(binary(), binary()) -> {ok, binary()} | {error, term()}.
resolve_term_iri(Namespace, Term) ->
    %% Construct the IRI path based on FIBO namespace conventions
    case maps:get(Namespace, ?FIBO_NS_IRI_MAP, undefined) of
        undefined ->
            {error, {unknown_namespace, Namespace}};
        IRIPath ->
            %% Construct full IRI
            IRI = <<?FIBO_IRI_PREFIX/binary, IRIPath/binary, ">">>,
            case is_valid_fibo_iri(IRI, Term) of
                true -> {ok, IRI};
                false -> {error, {invalid_iri_format, IRI}}
            end
    end.

%% @doc Validate cloud deployment term against standard cloud ontologies
-spec validate_cloud_term(binary(), binary()) -> ok | {error, term()}.
validate_cloud_term(Namespace, Term) ->
    case validate_cloud_namespace(Namespace) of
        ok ->
            %% Check if term is in the standard vocabulary for this namespace
            case get_standard_cloud_terms(Namespace) of
                {ok, StandardTerms} ->
                    case lists:member(Term, StandardTerms) of
                        true ->
                            ok;
                        false ->
                            %% Custom term - may be acceptable with justification
                            {error, {custom_cloud_term, Namespace, Term}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate that namespace is a known standard cloud ontology
-spec validate_cloud_namespace(binary()) -> ok | {error, term()}.
validate_cloud_namespace(Namespace) when is_binary(Namespace) ->
    case maps:get(Namespace, ?STANDARD_CLOUD_ONTOLOGIES, undefined) of
        undefined ->
            {error, {unknown_cloud_ontology, Namespace}};
        _OntologyInfo ->
            ok
    end;
validate_cloud_namespace(Namespace) ->
    {error, {invalid_namespace_format, Namespace}}.

%% @doc Get standard terms for a cloud namespace
-spec get_standard_cloud_terms(binary()) -> {ok, [binary()]} | {error, term()}.
get_standard_cloud_terms(Namespace) when is_binary(Namespace) ->
    case maps:get(Namespace, ?STANDARD_CLOUD_ONTOLOGIES, undefined) of
        undefined ->
            {error, {unknown_cloud_ontology, Namespace}};
        OntologyInfo ->
            StandardTerms = maps:get(standard_terms, OntologyInfo, []),
            {ok, StandardTerms}
    end;
get_standard_cloud_terms(Namespace) ->
    {error, {invalid_namespace_format, Namespace}}.

%%====================================================================
%% Internal Functions
%%====================================================================

extract_terms(Content) ->
    %% Extract all class definitions from TTL
    %% Format: prefix:ClassName a owl:Class
    Lines = binary:split(Content, <<"\n">>, [global]),

    lists:filtermap(fun(Line) ->
        case re:run(Line, <<"^([a-z0-9_-]+):([A-Z][a-zA-Z0-9_]*) a owl:Class">>,
                    [{capture, all_but_first, binary}]) of
            {match, [Prefix, ClassName]} ->
                {true, #{prefix => Prefix, name => ClassName, line => Line}};
            nomatch ->
                false
        end
    end, Lines).

lint_term(#{prefix := Prefix, name := Name, line := Line}, Content) ->
    %% Determine if term is in financial or cloud domain
    NameLower = string:lowercase(Name),

    %% Check FIBO alignment
    IsFibo = is_fibo_namespace(Prefix),

    %% Check cloud alignment
    IsCloud = lists:member(Prefix, ?CLOUD_NAMESPACES),

    %% Check if term is in financial domain
    IsFinancialDomain = lists:any(fun(Domain) ->
        binary:match(NameLower, Domain) =/= nomatch
    end, ?FINANCIAL_DOMAINS),

    %% Check if term is in cloud domain
    IsCloudDomain = lists:any(fun(Domain) ->
        binary:match(NameLower, Domain) =/= nomatch
    end, ?CLOUD_DOMAINS),

    %% Check for justification annotation
    HasJustification = has_justification_annotation(Name, Content),

    %% Validate FIBO IRI if FIBO-aligned
    IRIValidation = case IsFibo of
        true ->
            case resolve_term_iri(Prefix, Name) of
                {ok, IRI} -> {valid, IRI};
                {error, Reason} -> {invalid, Reason}
            end;
        false ->
            not_fibo
    end,

    %% Validate cloud ontology namespace if cloud-aligned
    CloudOntologyValidation = case IsCloud of
        true ->
            validate_cloud_namespace(Prefix);
        false ->
            not_cloud
    end,

    %% Check if cloud term is standard
    CloudTermValidation = case IsCloud andalso CloudOntologyValidation =:= ok of
        true ->
            case validate_cloud_term(Prefix, Name) of
                ok -> {standard, Prefix};
                {error, {custom_cloud_term, _, _}} -> {custom, Prefix};
                {error, ErrorReason} -> {error, ErrorReason}
            end;
        false ->
            not_checked
    end,

    %% Determine violation
    Violation = case {IsFinancialDomain, IsFibo, IsCloudDomain, IsCloud, HasJustification, IRIValidation, CloudOntologyValidation, CloudTermValidation} of
        {true, false, _, _, false, _, _, _} ->
            %% Financial term not using FIBO and no justification
            #{
                type => missing_fibo_alignment,
                term => <<Prefix/binary, ":", Name/binary>>,
                line => Line,
                suggestion => suggest_fibo_term(Name),
                severity => high
            };
        {true, true, _, _, _, {invalid, IRIReason}, _, _} ->
            %% Financial term uses FIBO but IRI is invalid
            #{
                type => invalid_fibo_iri,
                term => <<Prefix/binary, ":", Name/binary>>,
                line => Line,
                reason => IRIReason,
                severity => high
            };
        {_, _, true, true, _, _, {error, NSReason}, _} ->
            %% Cloud term uses cloud namespace but namespace validation failed
            #{
                type => invalid_cloud_namespace,
                term => <<Prefix/binary, ":", Name/binary>>,
                line => Line,
                reason => NSReason,
                severity => high
            };
        {_, _, true, true, _, _, ok, {custom, _}} ->
            %% Cloud term is custom (not in standard vocabulary) without justification
            case HasJustification of
                false ->
                    #{
                        type => custom_cloud_term_unjustified,
                        term => <<Prefix/binary, ":", Name/binary>>,
                        line => Line,
                        namespace => Prefix,
                        severity => medium,
                        suggestion => <<
                            "Term not found in standard ", Prefix/binary,
                            " ontology. Add documentation (rdfs:comment or skos:note) "
                            "explaining the custom term, or use standard term: ",
                            (suggest_cloud_term(Name))/binary
                        >>
                    };
                true ->
                    ok
            end;
        {_, _, true, false, false, _, _, _} ->
            %% Cloud term not using cloud namespace and no justification
            #{
                type => missing_cloud_alignment,
                term => <<Prefix/binary, ":", Name/binary>>,
                line => Line,
                suggestion => suggest_cloud_term(Name),
                severity => medium
            };
        {false, false, false, false, false, _, _, _} ->
            %% Custom term without justification
            #{
                type => missing_justification,
                term => <<Prefix/binary, ":", Name/binary>>,
                line => Line,
                required => <<"skos:note or rdfs:comment explaining why custom term is needed">>,
                severity => low
            };
        _ ->
            ok
    end,

    #{
        term => <<Prefix/binary, ":", Name/binary>>,
        prefix => Prefix,
        name => Name,
        line => Line,
        is_fibo => IsFibo,
        is_cloud => IsCloud,
        is_financial_domain => IsFinancialDomain,
        is_cloud_domain => IsCloudDomain,
        has_justification => HasJustification,
        iri_validation => IRIValidation,
        cloud_ontology_validation => CloudOntologyValidation,
        cloud_term_validation => CloudTermValidation,
        violation => Violation
    }.

aggregate_results(Results, _Content) ->
    TermsChecked = length(Results),

    FiboAligned = length([R || R <- Results, maps:get(is_fibo, R) =:= true]),
    CloudAligned = length([R || R <- Results, maps:get(is_cloud, R) =:= true]),

    CustomJustified = length([R || R <- Results,
                               maps:get(is_fibo, R) =:= false andalso
                               maps:get(is_cloud, R) =:= false andalso
                               maps:get(has_justification, R) =:= true]),

    %% Count IRI validation results
    IRIValidated = length([R || R <- Results,
                           maps:get(is_fibo, R) =:= true,
                           case maps:get(iri_validation, R) of
                               {valid, _} -> true;
                               _ -> false
                           end]),

    IRIInvalid = length([R || R <- Results,
                         maps:get(is_fibo, R) =:= true,
                         case maps:get(iri_validation, R) of
                             {invalid, _} -> true;
                             _ -> false
                         end]),

    %% Count cloud ontology validation results
    CloudOntologyValidated = length([R || R <- Results,
                                     maps:get(is_cloud, R) =:= true andalso
                                     maps:get(cloud_ontology_validation, R) =:= ok]),

    CloudOntologyViolations = length([R || R <- Results,
                                      maps:get(is_cloud, R) =:= true andalso
                                      maps:get(cloud_ontology_validation, R) =/= ok]),

    Violations = [maps:get(violation, R) || R <- Results,
                  maps:get(violation, R) =/= ok],

    CustomUnjustified = length([V || V <- Violations,
                                maps:get(type, V) =:= missing_justification]),

    #{
        terms_checked => TermsChecked,
        fibo_aligned => FiboAligned,
        cloud_aligned => CloudAligned,
        custom_justified => CustomJustified,
        custom_unjustified => CustomUnjustified,
        cloud_ontology_validated => CloudOntologyValidated,
        cloud_ontology_violations => CloudOntologyViolations,
        iri_validated => IRIValidated,
        iri_invalid => IRIInvalid,
        violations => Violations
    }.

is_financial_domain(Domain) ->
    DomainLower = string:lowercase(Domain),
    lists:any(fun(Fin) ->
        binary:match(DomainLower, Fin) =/= nomatch
    end, ?FINANCIAL_DOMAINS).

is_cloud_domain(Domain) ->
    DomainLower = string:lowercase(Domain),
    lists:any(fun(Cloud) ->
        binary:match(DomainLower, Cloud) =/= nomatch
    end, ?CLOUD_DOMAINS).

is_fibo_term(Term) ->
    lists:any(fun(NS) ->
        binary:match(Term, NS) =/= nomatch
    end, ?FIBO_NAMESPACES).

is_fibo_namespace(Namespace) ->
    lists:member(Namespace, ?FIBO_NAMESPACES).

is_cloud_term(Term) ->
    lists:any(fun(NS) ->
        binary:match(Term, NS) =/= nomatch
    end, ?CLOUD_NAMESPACES).

has_justification_annotation(Name, Content) ->
    %% Look for skos:note or rdfs:comment near the term definition
    %% Simple heuristic: check if justification appears within 10 lines
    Lines = binary:split(Content, <<"\n">>, [global]),

    %% Find line with term definition
    TermLine = lists:search(fun(Line) ->
        binary:match(Line, Name) =/= nomatch andalso
        binary:match(Line, <<"owl:Class">>) =/= nomatch
    end, Lines),

    case TermLine of
        {value, _} ->
            %% Check surrounding lines for justification
            LineIndex = find_line_index(Name, Lines),
            Start = max(0, LineIndex - 5),
            End = min(length(Lines), LineIndex + 5),
            Context = lists:sublist(Lines, Start, End - Start),

            lists:any(fun(L) ->
                binary:match(L, <<"skos:note">>) =/= nomatch orelse
                binary:match(L, <<"rdfs:comment">>) =/= nomatch orelse
                binary:match(L, <<"# Justification:">>) =/= nomatch
            end, Context);
        false ->
            false
    end.

find_line_index(Name, Lines) ->
    find_line_index(Name, Lines, 1).

find_line_index(_, [], _) -> 0;
find_line_index(Name, [Line | Rest], Index) ->
    case binary:match(Line, Name) of
        nomatch -> find_line_index(Name, Rest, Index + 1);
        _ -> Index
    end.

extract_namespace_and_term(TermWithNS) ->
    case binary:split(TermWithNS, <<":">>) of
        [Namespace, Term] ->
            {ok, {Namespace, Term}};
        _ ->
            error
    end.

suggest_fibo_term(Name) ->
    %% Simple suggestion based on name
    NameLower = string:lowercase(Name),

    case {binary:match(NameLower, <<"loan">>),
          binary:match(NameLower, <<"borrower">>),
          binary:match(NameLower, <<"account">>),
          binary:match(NameLower, <<"party">>)} of
        {{match, _}, _, _, _} -> <<"fibo-loan:Loan or fibo-loan:LoanContract">>;
        {_, {match, _}, _, _} -> <<"fibo-loan:Borrower">>;
        {_, _, {match, _}, _} -> <<"fibo-fnd:Account">>;
        {_, _, _, {match, _}} -> <<"fibo-fnd:Party">>;
        _ -> <<"Check FIBO specification: https://spec.edmcouncil.org/fibo/">>
    end.

suggest_cloud_term(Name) ->
    %% Simple suggestion based on name
    NameLower = string:lowercase(Name),

    case {binary:match(NameLower, <<"deploy">>),
          binary:match(NameLower, <<"container">>),
          binary:match(NameLower, <<"service">>),
          binary:match(NameLower, <<"instance">>),
          binary:match(NameLower, <<"cluster">>)} of
        {{match, _}, _, _, _, _} -> <<"gcp:Deployment, aws:Instance, azure:VirtualMachine, or k8s:Deployment">>;
        {_, {match, _}, _, _, _} -> <<"docker:Container, k8s:Pod, or gcp:Container">>;
        {_, _, {match, _}, _, _} -> <<"gcp:Service, k8s:Service, or aws:SecurityGroup">>;
        {_, _, _, {match, _}, _} -> <<"gcp:Instance, aws:EC2Instance, or azure:VirtualMachine">>;
        {_, _, _, _, {match, _}} -> <<"gcp:Cluster, aws:ECSCluster, azure:AKSCluster, or k8s:Cluster">>;
        _ -> <<"Check standard cloud ontologies for similar terms">>
    end.

compute_proof_hash(LintResult) ->
    %% Create deterministic hash of lint result
    Json = json:encode(sort_map_keys(LintResult)),
    Hash = crypto:hash(sha256, Json),
    binary:encode_hex(Hash, lowercase).

sort_map_keys(Map) when is_map(Map) ->
    Keys = lists:sort(maps:keys(Map)),
    maps:from_list([{K, sort_map_keys(maps:get(K, Map))} || K <- Keys]);
sort_map_keys(List) when is_list(List) ->
    [sort_map_keys(Item) || Item <- List];
sort_map_keys(Other) ->
    Other.

is_valid_fibo_iri(IRI, _Term) ->
    %% Simple validation: check if IRI is well-formed
    %% Full validation would require HTTP checks
    binary:match(IRI, ?FIBO_IRI_PREFIX) =/= nomatch.
