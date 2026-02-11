%%%-------------------------------------------------------------------
%%% @doc FIBO/Cloud-First Enforcement Linter
%%% Ensures domain concepts use FIBO terms and cloud concepts use
%%% standard cloud ontology terms. Custom terms must have justification.
%%% @end
%%%-------------------------------------------------------------------
-module(fibo_cloud_first_linter).

-export([
    lint_ontology/1,
    generate_proof/1,
    validate_term/2
]).

-define(FIBO_NAMESPACES, [
    <<"fibo-fnd">>,
    <<"fibo-be">>,
    <<"fibo-loan">>,
    <<"fibo-fbc">>,
    <<"fibo-sec">>,
    <<"fibo-der">>,
    <<"fibo-ind">>
]).

-define(CLOUD_NAMESPACES, [
    <<"gcp">>,
    <<"aws">>,
    <<"azure">>,
    <<"k8s">>,
    <<"docker">>,
    <<"cloud">>
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
    <<"contract">>
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

-type lint_result() :: #{
    terms_checked => integer(),
    fibo_aligned => integer(),
    cloud_aligned => integer(),
    custom_justified => integer(),
    custom_unjustified => integer(),
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

            %% Lint each term
            Results = lists:map(fun(Term) ->
                lint_term(Term, Content)
            end, Terms),

            %% Aggregate results
            Summary = aggregate_results(Results),

            {ok, Summary};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
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
                true -> ok;
                false -> {error, {missing_fibo_alignment, Term, Domain}}
            end;
        false ->
            %% Check if term should use cloud namespace
            case is_cloud_domain(Domain) of
                true ->
                    case is_cloud_term(Term) of
                        true -> ok;
                        false -> {error, {missing_cloud_alignment, Term, Domain}}
                    end;
                false ->
                    ok  %% Custom domain, no alignment required
            end
    end.

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
    IsFibo = lists:member(Prefix, ?FIBO_NAMESPACES),

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

    %% Determine violation
    Violation = case {IsFinancialDomain, IsFibo, IsCloudDomain, IsCloud, HasJustification} of
        {true, false, _, _, false} ->
            %% Financial term not using FIBO and no justification
            #{
                type => missing_fibo_alignment,
                term => <<Prefix/binary, ":", Name/binary>>,
                suggestion => suggest_fibo_term(Name)
            };
        {_, _, true, false, false} ->
            %% Cloud term not using cloud namespace and no justification
            #{
                type => missing_cloud_alignment,
                term => <<Prefix/binary, ":", Name/binary>>,
                suggestion => suggest_cloud_term(Name)
            };
        {false, false, false, false, false} ->
            %% Custom term without justification
            #{
                type => missing_justification,
                term => <<Prefix/binary, ":", Name/binary>>,
                required => <<"skos:note or rdfs:comment explaining why custom term is needed">>
            };
        _ ->
            ok
    end,

    #{
        term => <<Prefix/binary, ":", Name/binary>>,
        is_fibo => IsFibo,
        is_cloud => IsCloud,
        is_financial_domain => IsFinancialDomain,
        is_cloud_domain => IsCloudDomain,
        has_justification => HasJustification,
        violation => Violation
    }.

aggregate_results(Results) ->
    TermsChecked = length(Results),

    FiboAligned = length([R || R <- Results, maps:get(is_fibo, R) =:= true]),
    CloudAligned = length([R || R <- Results, maps:get(is_cloud, R) =:= true]),

    CustomJustified = length([R || R <- Results,
                               maps:get(is_fibo, R) =:= false andalso
                               maps:get(is_cloud, R) =:= false andalso
                               maps:get(has_justification, R) =:= true]),

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
          binary:match(NameLower, <<"service">>)} of
        {{match, _}, _, _} -> <<"gcp:Deployment or k8s:Deployment">>;
        {_, {match, _}, _} -> <<"docker:Container or k8s:Pod">>;
        {_, _, {match, _}} -> <<"gcp:Service or k8s:Service">>;
        _ -> <<"Use standard cloud ontology terms">>
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
