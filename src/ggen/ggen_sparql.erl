%%% @doc SPARQL Query Engine for ggen Manufacturing Pipeline
%%% Minimal SPARQL SELECT implementation for ontology extraction
%%% Supports: SELECT, WHERE, FILTER patterns for manufacturing
%%% Version: 1.0.0 - MVP grade
-module(ggen_sparql).

-export([
    execute/2,
    parse_select/1,
    extract_bindings/2
]).

-type graph() :: [{Subject :: binary(), Predicate :: binary(), Object :: binary()}].
-type query() :: binary() | string().
-type bindings() :: #{atom() => binary()}.
-type results() :: [bindings()].

%%% ===========================================================================
%%% PUBLIC API
%%% ===========================================================================

%% @doc Execute SPARQL SELECT query against RDF graph
-spec execute(query(), graph()) -> {ok, results()} | {error, term()}.
execute(Query, Graph) when is_binary(Query) ->
    execute(binary_to_list(Query), Graph);
execute(Query, Graph) when is_list(Query) ->
    case parse_select(Query) of
        {ok, ParsedQuery} ->
            Results = match_patterns(ParsedQuery, Graph),
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Parse SPARQL SELECT query (minimal implementation)
-spec parse_select(string()) -> {ok, map()} | {error, term()}.
parse_select(Query) ->
    %% Extract SELECT variables
    Vars = extract_variables(Query),

    %% Extract WHERE patterns
    Patterns = extract_patterns(Query),

    %% Extract FILTER conditions
    Filters = extract_filters(Query),

    {ok, #{
        type => select,
        vars => Vars,
        patterns => Patterns,
        filters => Filters
    }}.

%% @doc Extract variable bindings from match results
-spec extract_bindings(atom(), results()) -> [binary()].
extract_bindings(VarName, Results) ->
    [maps:get(VarName, R, undefined) || R <- Results, maps:is_key(VarName, R)].

%%% ===========================================================================
%%% INTERNAL FUNCTIONS
%%% ===========================================================================

%% Match WHERE patterns against graph
-spec match_patterns(map(), graph()) -> results().
match_patterns(#{patterns := Patterns, filters := Filters, vars := _Vars}, Graph) ->
    %% Start with empty bindings
    InitialBindings = [#{}],

    %% Match each pattern sequentially
    MatchedBindings = lists:foldl(
        fun(Pattern, Acc) ->
            lists:flatmap(fun(B) -> match_pattern(Pattern, Graph, B) end, Acc)
        end,
        InitialBindings,
        Patterns
    ),

    %% Apply filters
    filter_results(MatchedBindings, Filters).

%% Match single triple pattern
-spec match_pattern(tuple(), graph(), bindings()) -> [bindings()].
match_pattern({S, P, O}, Graph, Bindings) ->
    lists:filtermap(
        fun({GS, GP, GO}) ->
            case match_term(S, GS, Bindings) of
                {ok, B1} ->
                    case match_term(P, GP, B1) of
                        {ok, B2} ->
                            case match_term(O, GO, B2) of
                                {ok, B3} -> {true, B3};
                                false -> false
                            end;
                        false -> false
                    end;
                false -> false
            end
        end,
        Graph
    ).

%% Match term (variable or literal)
-spec match_term(term(), binary(), bindings()) -> {ok, bindings()} | false.
match_term(Var, Value, Bindings) when is_atom(Var) ->
    %% Check if variable (starts with ?)
    VarStr = atom_to_list(Var),
    case VarStr of
        [$? | _Rest] ->
            case maps:get(Var, Bindings, undefined) of
                undefined -> {ok, maps:put(Var, Value, Bindings)};
                Value -> {ok, Bindings};  %% Variable already bound to same value
                _ -> false  %% Variable bound to different value
            end;
        "_" ->
            %% Wildcard matches anything
            {ok, Bindings};
        _ ->
            %% Literal must match exactly
            LitBin = list_to_binary(VarStr),
            case LitBin =:= Value of
                true -> {ok, Bindings};
                false -> false
            end
    end;
match_term(Lit, Value, Bindings) when is_binary(Lit) ->
    case Lit =:= Value of
        true -> {ok, Bindings};
        false -> false
    end;
match_term(_, _, _Bindings) ->
    false.

%% Apply FILTER conditions
-spec filter_results(results(), [term()]) -> results().
filter_results(Results, []) ->
    Results;
filter_results(Results, Filters) ->
    lists:filter(
        fun(Binding) ->
            lists:all(fun(F) -> eval_filter(F, Binding) end, Filters)
        end,
        Results
    ).

%% Evaluate FILTER expression
-spec eval_filter(term(), bindings()) -> boolean().
eval_filter({in, Var, Values}, Bindings) ->
    case maps:get(Var, Bindings, undefined) of
        undefined -> false;
        Val -> lists:member(Val, Values)
    end;
eval_filter({eq, Var, Value}, Bindings) ->
    maps:get(Var, Bindings, undefined) =:= Value;
eval_filter({contains, Var, Substr}, Bindings) ->
    case maps:get(Var, Bindings, undefined) of
        undefined -> false;
        Val when is_binary(Val) ->
            binary:match(Val, Substr) =/= nomatch;
        _ -> false
    end;
eval_filter(_, _) ->
    true.  %% Unknown filter passes (conservative)

%% Extract SELECT variables from query
-spec extract_variables(string()) -> [atom()].
extract_variables(Query) ->
    case re:run(Query, "SELECT\\s+(.+?)\\s+WHERE", [caseless, {capture, all_but_first, list}]) of
        {match, [VarsStr]} ->
            VarList = string:tokens(VarsStr, " \t\n"),
            [list_to_atom(V) || V <- VarList, hd(V) =:= $?];
        nomatch ->
            []
    end.

%% Extract WHERE triple patterns
-spec extract_patterns(string()) -> [tuple()].
extract_patterns(Query) ->
    case re:run(Query, "WHERE\\s*\\{(.+?)\\}", [caseless, dotall, {capture, all_but_first, list}]) of
        {match, [WhereBody]} ->
            %% Split by . or ; separators
            PatternStrs = re:split(WhereBody, "[.;]", [{return, list}, trim]),
            lists:filtermap(
                fun(PS) ->
                    case parse_triple_pattern(string:trim(PS)) of
                        {ok, Pattern} -> {true, Pattern};
                        error -> false
                    end
                end,
                PatternStrs
            );
        nomatch ->
            []
    end.

%% Parse single triple pattern
-spec parse_triple_pattern(string()) -> {ok, tuple()} | error.
parse_triple_pattern(Str) ->
    Tokens = string:tokens(Str, " \t\n"),
    case Tokens of
        [S, P, O | _] ->
            {ok, {parse_term(S), parse_term(P), parse_term(O)}};
        _ ->
            error
    end.

%% Parse term (variable, URI, or literal)
-spec parse_term(string()) -> atom() | binary().
parse_term([$? | Rest]) ->
    list_to_atom([$? | Rest]);
parse_term("_") ->
    '_';
parse_term([$< | Rest]) ->
    %% URI: remove angle brackets
    list_to_binary(string:trim(Rest, trailing, ">"));
parse_term([$" | Rest]) ->
    %% Literal: remove quotes
    list_to_binary(string:trim(Rest, trailing, "\""));
parse_term(Str) ->
    %% Assume URI without brackets
    list_to_binary(Str).

%% Extract FILTER conditions
-spec extract_filters(string()) -> [term()].
extract_filters(Query) ->
    case re:run(Query, "FILTER\\s*\\((.+?)\\)", [caseless, global, {capture, all_but_first, list}]) of
        {match, Matches} ->
            [parse_filter(F) || [F] <- Matches];
        nomatch ->
            []
    end.

%% Parse FILTER expression
-spec parse_filter(string()) -> term().
parse_filter(FilterStr) ->
    %% Simple IN filter
    case re:run(FilterStr, "\\?([a-zA-Z]+)\\s+IN\\s+\\((.+?)\\)", [{capture, all_but_first, list}]) of
        {match, [Var, ValuesStr]} ->
            Values = [list_to_binary(string:trim(V, both, "\"")) || V <- string:tokens(ValuesStr, ",")],
            {in, list_to_atom("?" ++ Var), Values};
        nomatch ->
            %% Unknown filter format
            {unknown, FilterStr}
    end.
