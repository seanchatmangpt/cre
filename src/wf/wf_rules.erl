%% -*- erlang -*-
%%
-module(wf_rules).
-moduledoc """
Erlog-backed rule evaluation for workflow decision making.

Provides a simple Prolog-like rule evaluator for determining workflow
behavior based on token presence in markings. Rules are expressed in
a Datalog-like syntax and compiled for efficient evaluation.

```erlang
> {ok, Rules} =
..   wf_rules:compile(<<"enabled(a) :- token(coin_slot, coin).">>).
> Facts = wf_rules:facts_from_marking(#{coin_slot => [coin], storage => []}).
> wf_rules:bool(Rules, {enabled, a}, Facts, #{}).
true
> wf_rules:bool(Rules, {enabled, b}, Facts, #{}).
false
```

Variables in queries use the {var, Name} form and return bindings:

```erlang
> {ok, R2} = wf_rules:compile(<<"take(X) :- token(p, X).">>).
> F2 = wf_rules:facts_from_marking(#{p => [a,b]}).
> {ok, Bs} = wf_rules:query(R2, {take, {var, x}}, F2, #{}).
> lists:sort([maps:get(x, B) || B <- Bs]).
[a,b]
```

Malformed rules return an error:

```erlang
> wf_rules:compile(<<"enabled(a) :- .">>).
{error,bad_program}
```

<h3>Rule Syntax</h3>
Rules follow a simple Horn clause syntax:

```
Head :- Body.
```

Where:
- Head is a predicate: predicate_name(arg1, arg2, ...)
- Body is a comma-separated list of predicates
- Facts (rules with empty body) are written as: predicate(args).
- The special predicate token(Place, Token) checks for token presence

<h3>Usage Pattern</h3>
1. Compile rules once with compile/1
2. Convert markings to facts with facts_from_marking/1
3. Query using bool/4 for yes/no or query/4 for bindings

<h3>Facts from Markings</h3>
The facts_from_marking/1 function converts a Petri net marking into
a flat list of {token, Place, Token} facts. Each token in each place
becomes a separate fact, allowing rules to reason about token presence
and values.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Compilation
-export([compile/1]).

%% Fact conversion
-export([facts_from_marking/1]).

%% Query operations
-export([bool/4, query/4]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A compiled set of rules.
%%
%% Internal representation of parsed and validated rules.
%% Contains clauses indexed by predicate name for efficient lookup.
%%--------------------------------------------------------------------
-type rules() :: #{
    clauses := #{predicate() => [clause()]}
}.

%%--------------------------------------------------------------------
%% @doc A predicate name (atom).
%%
%% The functor/name of a predicate or rule head.
%%--------------------------------------------------------------------
-type predicate() :: atom().

%%--------------------------------------------------------------------
%% @doc An argument to a predicate.
%%
%% Can be:
%%   - An atom (constant)
%%   - A variable represented as {var, Name}
%%   - Any other Erlang term
%%--------------------------------------------------------------------
-type arg() :: atom() | {var, atom()} | term().

%%--------------------------------------------------------------------
%% @doc A clause (rule) with head and body.
%%
%% Represents a single Horn clause: Head :- Body.
%% Facts have empty bodies.
%%--------------------------------------------------------------------
-type clause() :: #{
    head := pred_ref(),
    body := [pred_ref()]
}.

%%--------------------------------------------------------------------
%% @doc A predicate reference with arguments.
%%
%% Represents a predicate occurrence with its concrete arguments
%% or variables for pattern matching.
%%--------------------------------------------------------------------
-type pred_ref() :: {predicate(), [arg()]}.

%%--------------------------------------------------------------------
%% @doc A fact extracted from a marking.
%%
%% Facts are ground predicates with no variables.
%%--------------------------------------------------------------------
-type fact() :: {predicate(), [term()]}.

%%--------------------------------------------------------------------
%% @doc A variable binding mapping.
%%
%% Maps variable names (as atoms) to their bound values.
%%--------------------------------------------------------------------
-type binding() :: #{atom() => term()}.

%%--------------------------------------------------------------------
%% @doc A query term.
%%
%% Can be a ground predicate or a predicate with {var, Name} args.
%%--------------------------------------------------------------------
-type query() :: {predicate(), [arg()]}.

%% Export types
-export_type([rules/0, predicate/0, arg/0, clause/0, fact/0, binding/0, query/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles a rule string into an internal ruleset.
%%
%% Parses a binary containing rules in Datalog-like syntax and
%% returns a compiled ruleset for efficient querying.
%%
%% @param Program Binary string containing rule definitions
%% @return {ok, Rules} on success, {error, bad_program} on parse error
%%
%% @end
%%--------------------------------------------------------------------
-spec compile(Program :: binary()) -> {ok, rules()} | {error, bad_program}.

compile(Program) when is_binary(Program) ->
    try
        Clauses = parse_program(Program),
        RulesIndex = index_clauses(Clauses),
        {ok, #{clauses => RulesIndex}}
    catch
        throw:{parse_error, _} -> {error, bad_program};
        error:_ -> {error, bad_program}
    end.

%%--------------------------------------------------------------------
%% @doc Converts a Petri net marking to a flat list of facts.
%%
%% Each token in each place becomes a {token, [Place, Token]} fact.
%% Empty places produce no facts.
%%
%% @param Marking Map of places to token lists
%% @return List of {token, [Place, Token]} facts
%%
%% @end
%%--------------------------------------------------------------------
-spec facts_from_marking(Marking :: #{atom() => [term()]}) -> [fact()].

facts_from_marking(Marking) when is_map(Marking) ->
    maps:fold(fun
        (_Place, [], Acc) -> Acc;
        (Place, Tokens, Acc) when is_list(Tokens) ->
            Facts = [{token, [Place, Token]} || Token <- Tokens],
            Facts ++ Acc
    end, [], Marking).

%%--------------------------------------------------------------------
%% @doc Evaluates a query as a boolean against rules and facts.
%%
%% Returns true if the query can be proven from the rules and facts,
%% false otherwise. Extra facts (additional context) can be provided.
%%
%% @param Rules Compiled ruleset from compile/1
%% @param Query Query predicate to evaluate
%% @param Facts Facts derived from marking
%% @param ExtraFacts Additional facts for context
%% @return true if query succeeds, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec bool(Rules :: rules(), Query :: query(), Facts :: [fact()],
           ExtraFacts :: #{atom() => term()}) ->
          true | false | {error, Reason :: term()}.

bool(#{clauses := Clauses}, Query, Facts, ExtraFacts) when is_list(Facts), is_map(ExtraFacts) ->
    try
        NormQuery = normalize_query(Query),
        case eval_query(NormQuery, Clauses, Facts, ExtraFacts, #{}) of
            {ok, _Bindings} -> true;
            no_match -> false
        end
    catch
        throw:{error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Evaluates a query and returns all variable bindings.
%%
%% Returns a list of binding maps, one for each way the query can
%% be proven. Variables in the query should use {var, Name} format.
%%
%% @param Rules Compiled ruleset from compile/1
%% @param Query Query predicate with possible variables
%% @param Facts Facts derived from marking
%% @param ExtraFacts Additional facts for context
%% @return {ok, [BindingMap]} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec query(Rules :: rules(), Query :: query(), Facts :: [fact()],
            ExtraFacts :: #{atom() => term()}) ->
          {ok, [binding()]} | {error, Reason :: term()}.

query(#{clauses := Clauses}, Query, Facts, ExtraFacts) when is_list(Facts), is_map(ExtraFacts) ->
    try
        NormQuery = normalize_query(Query),
        case eval_query(NormQuery, Clauses, Facts, ExtraFacts, #{}) of
            {ok, _Bindings} -> {ok, collect_all_bindings(NormQuery, Clauses, Facts, ExtraFacts)};
            no_match -> {ok, []}
        end
    catch
        throw:{error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Internal Functions - Parsing
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes a query to have args as a list.
%%
%% Converts {Pred, Arg} to {Pred, [Arg]} for single-arg predicates.
%% Also normalizes variable names to lowercase.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_query(query()) -> pred_ref().

normalize_query({Pred, Args}) when is_list(Args) ->
    {Pred, normalize_args(Args)};
normalize_query({Pred, Arg}) ->
    {Pred, [normalize_arg(Arg)]};
normalize_query(Tuple) when is_tuple(Tuple), tuple_size(Tuple) > 2 ->
    [Pred | Args] = tuple_to_list(Tuple),
    {Pred, normalize_args(Args)}.

%% @private
-spec normalize_args([arg()]) -> [arg()].

normalize_args(Args) ->
    [normalize_arg(A) || A <- Args].

%%--------------------------------------------------------------------
%% @private
%% @doc Parses a program string into a list of clauses.
%%
%% Handles multiple clauses separated by periods. Returns a list
%% of parsed clause maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_program(binary()) -> [clause()].

parse_program(Program) ->
    %% Tokenize the input
    Tokens = tokenize(Program),
    %% Parse clauses
    {Clauses, _} = parse_clauses(Tokens),
    Clauses.

%%--------------------------------------------------------------------
%% @private
%% @doc Tokenizes a binary into a list of tokens.
%%
%% Tokens are: atoms, variables, strings, parens, commas, periods,
%% and operators (:-).
%%
%% @end
%%--------------------------------------------------------------------
-spec tokenize(binary()) -> [token()].

tokenize(Binary) ->
    Str = binary_to_list(Binary),
    tokenize_loop(Str, []).

%% Helper to consume whitespace
tokenize_loop([], Acc) ->
    lists:reverse(Acc);
tokenize_loop([C | Rest], Acc) when C =:= $\s; C =:= $\n; C =:= $\t; C =:= $\r ->
    tokenize_loop(Rest, Acc);
%% Comment: % to end of line
tokenize_loop([$% | Rest], Acc) ->
    tokenize_loop_comment(Rest, Acc);
%% Parentheses
tokenize_loop([$( | Rest], Acc) ->
    tokenize_loop(Rest, ['(' | Acc]);
tokenize_loop([$) | Rest], Acc) ->
    tokenize_loop(Rest, [')' | Acc]);
%% Comma
tokenize_loop([$, | Rest], Acc) ->
    tokenize_loop(Rest, [',' | Acc]);
%% Period
tokenize_loop([$. | Rest], Acc) ->
    tokenize_loop(Rest, ['.' | Acc]);
%% Neck operator :-
tokenize_loop([$:, $- | Rest], Acc) ->
    tokenize_loop(Rest, [':-' | Acc]);
%% Atom or variable starting with letter
tokenize_loop([C | _] = Input, Acc) when C >= $a, C =< $z; C >= $A, C =< $z; C =:= $_ ->
    {AtomOrVar, Rest} = scan_atom_or_var(Input),
    tokenize_loop(Rest, [AtomOrVar | Acc]);
%% Number
tokenize_loop([C | _] = Input, Acc) when C >= $0, C =< $9 ->
    {Num, Rest} = scan_number(Input),
    tokenize_loop(Rest, [Num | Acc]);
%% Single quoted string
tokenize_loop([39 | Rest], Acc) ->  % 39 is $'
    {String, Rest2} = scan_single_quoted(Rest),
    tokenize_loop(Rest2, [String | Acc]);
%% Double quoted string (binary)
tokenize_loop([34 | Rest], Acc) ->  % 34 is $"
    {String, Rest2} = scan_double_quoted(Rest),
    tokenize_loop(Rest2, [String | Acc]);
%% Unknown char - skip or error
tokenize_loop([_C | Rest], Acc) ->
    tokenize_loop(Rest, Acc).

%% Comment: consume until newline
tokenize_loop_comment([], Acc) ->
    lists:reverse(Acc);
tokenize_loop_comment([$\n | Rest], Acc) ->
    tokenize_loop(Rest, Acc);
tokenize_loop_comment([_ | Rest], Acc) ->
    tokenize_loop_comment(Rest, Acc).

%% Scan an atom or variable
scan_atom_or_var(Input) ->
    scan_atom_or_var_loop(Input, []).

scan_atom_or_var_loop([C | Rest], Acc) when C >= $a, C =< $z; C >= $A, C =< $Z;
                                           C >= $0, C =< $9; C =:= $_ ->
    scan_atom_or_var_loop(Rest, [C | Acc]);
scan_atom_or_var_loop(Rest, Acc) ->
    AtomChars = lists:reverse(Acc),
    AtomStr = case AtomChars of
        [C | _] when C >= $A, C =< $Z ->
            % Variable - lowercase the name for consistency
            VarName = list_to_atom(string:to_lower(AtomChars)),
            {var, VarName};
        _ ->
            % Atom
            list_to_atom(AtomChars)
    end,
    {AtomStr, Rest}.

%% Scan a number
scan_number(Input) ->
    scan_number_loop(Input, []).

scan_number_loop([C | Rest], Acc) when C >= $0, C =< $9 ->
    scan_number_loop(Rest, [C | Acc]);
scan_number_loop(Rest, Acc) ->
    NumStr = lists:reverse(Acc),
    Num = list_to_integer(NumStr),
    {Num, Rest}.

%% Scan single-quoted string
scan_single_quoted(Input) ->
    scan_single_quoted_loop(Input, []).

scan_single_quoted_loop([39 | Rest], Acc) ->  % closing quote
    AtomStr = lists:reverse(Acc),
    Atom = list_to_atom(AtomStr),
    {Atom, Rest};
scan_single_quoted_loop([C | Rest], Acc) ->
    scan_single_quoted_loop(Rest, [C | Acc]).

%% Scan double-quoted string
scan_double_quoted(Input) ->
    scan_double_quoted_loop(Input, []).

scan_double_quoted_loop([34 | Rest], Acc) ->  % closing quote
    BinStr = lists:reverse(Acc),
    Bin = list_to_binary(BinStr),
    {Bin, Rest};
scan_double_quoted_loop([C | Rest], Acc) ->
    scan_double_quoted_loop(Rest, [C | Acc]).

%% Token type
-type token() :: atom() | {var, atom()} | binary() | integer() | '(' | ')' | ',' | '.' | ':-'.

%%--------------------------------------------------------------------
%% @private
%% @doc Parses clauses from tokens.
%%
%% Returns {Clauses, RemainingTokens}.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_clauses([token()]) -> {[clause()], [token()]}.

parse_clauses([]) ->
    {[], []};
parse_clauses(['.' | Rest]) ->
    parse_clauses(Rest);
parse_clauses(Tokens) ->
    {Clause, Rest1} = parse_clause(Tokens),
    {Clauses, Rest2} = parse_clauses(Rest1),
    {[Clause | Clauses], Rest2}.

%%--------------------------------------------------------------------
%% @private
%% @doc Parses a single clause from tokens.
%%
%% A clause is: Head :- Body. or Head.
%% Returns {Clause, RemainingTokens}.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_clause([token()]) -> {clause(), [token()]}.

parse_clause(Tokens) ->
    {Head, Rest1} = parse_predicate(Tokens),
    case Rest1 of
        [':-' | Rest2] ->
            {Body, Rest3} = parse_body(Rest2),
            ['.' | Rest4] = Rest3,
            {#{head => Head, body => Body}, Rest4};
        ['.' | Rest2] ->
            {#{head => Head, body => []}, Rest2};
        _ ->
            throw({parse_error, expected_neck_or_period})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Parses a predicate from tokens.
%%
%% A predicate is: functor(arg1, ..., argN) or functor for 0-arity.
%% Returns {{Predicate, Args}, RemainingTokens}.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_predicate([token()]) -> {pred_ref(), [token()]}.

parse_predicate([{Functor, _} = _VarOrAtom | ['(' | Rest]]) when is_atom(Functor) ->
    {Args, [')' | Rest2]} = parse_args(Rest),
    {{Functor, Args}, Rest2};
parse_predicate([Functor | ['(' | Rest]]) when is_atom(Functor) ->
    {Args, [')' | Rest2]} = parse_args(Rest),
    {{Functor, Args}, Rest2};
parse_predicate([Functor | Rest]) when is_atom(Functor) ->
    {{Functor, []}, Rest};
parse_predicate(_) ->
    throw({parse_error, expected_predicate}).

%%--------------------------------------------------------------------
%% @private
%% @doc Parses argument list.
%%
%% Returns {Args, RemainingTokens}.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_args([token()]) -> {[arg()], [token()]}.

parse_args([]) ->
    throw({parse_error, unexpected_eof});
parse_args([')' | _] = Rest) ->
    {[], Rest};
parse_args([Arg | [',' | Rest]]) ->
    {Args, Rest2} = parse_args(Rest),
    {[normalize_arg(Arg) | Args], Rest2};
parse_args([Arg | [')' | _] = Rest]) ->
    {[normalize_arg(Arg)], Rest};
parse_args(_) ->
    throw({parse_error, expected_arg_or_paren}).

%%--------------------------------------------------------------------
%% @private
%% @doc Parses a body (comma-separated predicates).
%%
%% Returns {Predicates, RemainingTokens}.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_body([token()]) -> {[pred_ref()], [token()]}.

parse_body([]) ->
    throw({parse_error, unexpected_eof});
parse_body(['.' | _]) ->
    {[], []};
parse_body(Tokens) ->
    {Pred, Rest1} = parse_predicate(Tokens),
    case Rest1 of
        [',' | Rest2] ->
            {Predicates, Rest3} = parse_body(Rest2),
            {[Pred | Predicates], Rest3};
        _ ->
            {[Pred], Rest1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes an argument token.
%%
%% Converts {var, Name} tokens to the standard form.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_arg(token()) -> arg().

normalize_arg({var, Name} = Var) when is_atom(Name) ->
    Var;
normalize_arg(Atom) when is_atom(Atom) ->
    Atom;
normalize_arg(Int) when is_integer(Int) ->
    Int;
normalize_arg(Bin) when is_binary(Bin) ->
    Bin;
normalize_arg(Other) ->
    Other.

%%--------------------------------------------------------------------
%% @private
%% @doc Indexes clauses by predicate name for efficient lookup.
%%
%% Returns a map from predicate name to list of clauses.
%%
%% @end
%%--------------------------------------------------------------------
-spec index_clauses([clause()]) -> #{predicate() => [clause()]}.

index_clauses(Clauses) ->
    lists:foldl(fun(Clause, Acc) ->
        {PredName, _Args} = maps:get(head, Clause),
        Existing = maps:get(PredName, Acc, []),
        Acc#{PredName => [Clause | Existing]}
    end, #{}, Clauses).

%%====================================================================
%% Internal Functions - Evaluation
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Evaluates a query and returns bindings or no_match.
%%
%% Main evaluation engine: tries to prove the query using rules
%% and facts.
%%
%% @end
%%--------------------------------------------------------------------
-spec eval_query(Query :: query(), Clauses :: #{predicate() => [clause()]},
                 Facts :: [fact()], ExtraFacts :: #{atom() => term()},
                 Binding :: binding()) ->
          {ok, binding()} | no_match.

-define(MAX_RECURSION_DEPTH, 64).

eval_query({PredName, Args}, Clauses, Facts, ExtraFacts, InitialBinding) ->
    %% Try to find a proof (with depth bound for recursive rules)
    case prove({PredName, Args}, Clauses, Facts, ExtraFacts, InitialBinding, 0) of
        {ok, Binding} -> {ok, Binding};
        no_match -> no_match
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Attempts to prove a predicate goal.
%%
%% Uses facts first, then rules (clauses with bodies).
%%
%% @end
%%--------------------------------------------------------------------
-spec prove(pred_ref(), #{predicate() => [clause()]}, [fact()],
            #{atom() => term()}, binding(), non_neg_integer()) ->
          {ok, binding()} | no_match.

prove(_Goal, _Clauses, _Facts, _ExtraFacts, _Binding, Depth) when Depth >= ?MAX_RECURSION_DEPTH ->
    no_match;
prove({PredName, Args}, Clauses, Facts, ExtraFacts, Binding, Depth) ->
    %% First try facts (ground facts matching the goal)
    case match_facts({PredName, Args}, Facts, Binding) of
        {ok, NewBinding} -> {ok, NewBinding};
        no_match ->
            %% Then try rules
            case maps:get(PredName, Clauses, []) of
                [] -> no_match;
                PredClauses ->
                    prove_clauses({PredName, Args}, PredClauses, Clauses,
                                  Facts, ExtraFacts, Binding, Depth)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Tries to match goal against known facts.
%%
%% Facts are ground predicates that can bind variables.
%%
%% @end
%%--------------------------------------------------------------------
-spec match_facts(pred_ref(), [fact()], binding()) ->
          {ok, binding()} | no_match.

match_facts({PredName, Args}, Facts, Binding) ->
    match_facts_loop({PredName, Args}, Facts, Binding).

match_facts_loop(_Goal, [], _Binding) ->
    no_match;
match_facts_loop({PredName, Args}, [{PredName, FactArgs} | Rest], Binding) ->
    case unify(Args, FactArgs, Binding) of
        {ok, NewBinding} -> {ok, NewBinding};
        no_match -> match_facts_loop({PredName, Args}, Rest, Binding)
    end;
match_facts_loop(Goal, [_OtherFact | Rest], Binding) ->
    match_facts_loop(Goal, Rest, Binding).

%%--------------------------------------------------------------------
%% @private
%% @doc Tries each clause to prove the goal.
%%
%% For each clause, unify goal with head, then prove body.
%%
%% @end
%%--------------------------------------------------------------------
-spec prove_clauses(pred_ref(), [clause()], #{predicate() => [clause()]},
                     [fact()], #{atom() => term()}, binding(), non_neg_integer()) ->
          {ok, binding()} | no_match.

prove_clauses(_Goal, [], _Clauses, _Facts, _ExtraFacts, _Binding, _Depth) ->
    no_match;
prove_clauses({PredName, Args}, [Clause | Rest], Clauses, Facts, ExtraFacts, Binding, Depth) ->
    #{head := {PredName, HeadArgs}, body := Body} = Clause,
    case unify(Args, HeadArgs, Binding) of
        {ok, NewBinding} ->
            case prove_body(Body, Clauses, Facts, ExtraFacts, NewBinding, Depth + 1) of
                {ok, FinalBinding} -> {ok, FinalBinding};
                no_match ->
                    prove_clauses({PredName, Args}, Rest, Clauses, Facts, ExtraFacts, Binding, Depth)
            end;
        no_match ->
            prove_clauses({PredName, Args}, Rest, Clauses, Facts, ExtraFacts, Binding, Depth)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Proves all goals in a body.
%%
%% All body goals must succeed.
%%
%% @end
%%--------------------------------------------------------------------
-spec prove_body([pred_ref()], #{predicate() => [clause()]}, [fact()],
                 #{atom() => term()}, binding(), non_neg_integer()) ->
          {ok, binding()} | no_match.

prove_body([], _Clauses, _Facts, _ExtraFacts, Binding, _Depth) ->
    {ok, Binding};
prove_body([Goal | Rest], Clauses, Facts, ExtraFacts, Binding, Depth) ->
    case prove(Goal, Clauses, Facts, ExtraFacts, Binding, Depth) of
        {ok, NewBinding} ->
            prove_body(Rest, Clauses, Facts, ExtraFacts, NewBinding, Depth);
        no_match ->
            no_match
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Unifies two argument lists under a binding.
%%
%% Standard unification algorithm.
%%
%% @end
%%--------------------------------------------------------------------
-spec unify([arg()], [term()], binding()) -> {ok, binding()} | no_match.

unify([], [], Binding) ->
    {ok, Binding};
unify([Arg1 | Rest1], [Arg2 | Rest2], Binding) ->
    case unify_term(Arg1, Arg2, Binding) of
        {ok, NewBinding} -> unify(Rest1, Rest2, NewBinding);
        no_match -> no_match
    end;
unify(_, _, _) ->
    no_match.

%%--------------------------------------------------------------------
%% @private
%% @doc Unifies a single term with another under a binding.
%%
%% Handles variables, constants, and nested structures.
%%
%% @end
%%--------------------------------------------------------------------
-spec unify_term(arg(), term(), binding()) -> {ok, binding()} | no_match.

unify_term({var, VarName}, {var, VarName}, Binding) ->
    %% Same variable - succeeds without changing binding
    {ok, Binding};
unify_term({var, VarName1}, {var, VarName2}, Binding) ->
    %% Two different variables - unify them
    case maps:find(VarName1, Binding) of
        {ok, BoundValue} ->
            %% VarName1 is already bound - unify with that
            unify_term(BoundValue, {var, VarName2}, Binding);
        error ->
            case maps:find(VarName2, Binding) of
                {ok, BoundValue} ->
                    %% VarName2 is already bound - unify with that
                    unify_term({var, VarName1}, BoundValue, Binding);
                error ->
                    %% Neither is bound - create an alias
                    {ok, Binding#{VarName1 => {var, VarName2}}}
            end
    end;
unify_term({var, VarName}, Term, Binding) ->
    case maps:find(VarName, Binding) of
        {ok, BoundValue} ->
            case BoundValue of
                Term -> {ok, Binding};
                _ -> no_match
            end;
        error ->
            {ok, Binding#{VarName => Term}}
    end;
unify_term(Term, {var, VarName}, Binding) ->
    unify_term({var, VarName}, Term, Binding);
unify_term(Term1, Term2, Binding) when Term1 =:= Term2 ->
    {ok, Binding};
unify_term(_, _, _) ->
    no_match.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts variable names from an argument list.
%%
%% Returns a list of {var, Name} terms found.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_vars([arg()]) -> [{var, atom()}].

extract_vars(Args) ->
    lists:filter(fun
        ({var, _}) -> true;
        (_) -> false
    end, Args).

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all bindings for a query.
%%
%% Returns a list of all possible binding maps that satisfy the query.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_all_bindings(query(), #{predicate() => [clause()]}, [fact()],
                            #{atom() => term()}) -> [binding()].

collect_all_bindings({PredName, Args}, Clauses, Facts, ExtraFacts) ->
    %% Get all variable names in query
    Vars = extract_vars(Args),
    %% Find all solutions
    Solutions = find_all_solutions({PredName, Args}, Clauses, Facts, ExtraFacts, #{}),
    %% Extract bindings for query variables
    lists:map(fun(Solution) ->
        maps:with([Name || {var, Name} <- Vars], Solution)
    end, Solutions).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds all solutions to a query.
%%
%% Returns a list of all binding maps that prove the query.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_all_solutions(pred_ref(), #{predicate() => [clause()]}, [fact()],
                         #{atom() => term()}, binding()) -> [binding()].

find_all_solutions(Goal, Clauses, Facts, ExtraFacts, InitialBinding) ->
    %% First try facts
    FactSolutions = find_fact_solutions(Goal, Facts, InitialBinding, []),
    %% Then try rules (with depth bound for recursion)
    RuleSolutions = find_rule_solutions(Goal, Clauses, Facts, ExtraFacts, InitialBinding, [], 0),
    %% Deduplicate and combine
    lists:usort(FactSolutions ++ RuleSolutions).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds solutions from facts only.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_fact_solutions(pred_ref(), [fact()], binding(), [binding()]) -> [binding()].

find_fact_solutions({_PredName, Args}, Facts, Binding, Acc) ->
    lists:foldl(fun
        ({_PN, FactArgs}, SolutionsAcc) when _PN =:= _PredName ->
            case unify(Args, FactArgs, Binding) of
                {ok, NewBinding} -> [NewBinding | SolutionsAcc];
                no_match -> SolutionsAcc
            end;
        (_, SolutionsAcc) ->
            SolutionsAcc
    end, Acc, Facts).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds all solutions when proving a body goal.
%%
%% Unlike prove_body which returns the first solution, this returns
%% all possible bindings that satisfy the body.
%%
%% @end
%%--------------------------------------------------------------------
-spec prove_body_all([pred_ref()], #{predicate() => [clause()]}, [fact()],
                      #{atom() => term()}, binding(), non_neg_integer()) -> [binding()].

prove_body_all([], _Clauses, _Facts, _ExtraFacts, Binding, _Depth) ->
    [Binding];
prove_body_all(_, _Clauses, _Facts, _ExtraFacts, _Binding, Depth) when Depth >= ?MAX_RECURSION_DEPTH ->
    [];
prove_body_all([Goal | Rest], Clauses, Facts, ExtraFacts, Binding, Depth) ->
    %% Find all solutions for Goal
    GoalSolutions = find_goal_solutions(Goal, Clauses, Facts, ExtraFacts, Binding, Depth),
    %% For each solution, prove the rest of the body
    lists:foldl(fun(GoalBinding, SolAcc) ->
        case prove_body_all(Rest, Clauses, Facts, ExtraFacts, GoalBinding, Depth) of
            [] -> SolAcc;
            RestSolutions -> RestSolutions ++ SolAcc
        end
    end, [], GoalSolutions).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds all solutions for a single goal.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_goal_solutions(pred_ref(), #{predicate() => [clause()]}, [fact()],
                          #{atom() => term()}, binding(), non_neg_integer()) -> [binding()].

find_goal_solutions(Goal, Clauses, Facts, ExtraFacts, Binding, Depth) ->
    %% Try facts first
    FactSols = find_fact_solutions_for_goal(Goal, Facts, Binding, []),
    %% Try rules
    RuleSols = find_rule_solutions_for_goal(Goal, Clauses, Facts, ExtraFacts, Binding, [], Depth),
    FactSols ++ RuleSols.

%%--------------------------------------------------------------------
%% @private
%% @doc Finds fact solutions for a specific goal.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_fact_solutions_for_goal(pred_ref(), [fact()], binding(), [binding()]) -> [binding()].

find_fact_solutions_for_goal({_PredName, Args}, Facts, Binding, Acc) ->
    lists:foldl(fun
        ({_PN, FactArgs}, SolutionsAcc) when _PN =:= _PredName ->
            case unify(Args, FactArgs, Binding) of
                {ok, NewBinding} -> [NewBinding | SolutionsAcc];
                no_match -> SolutionsAcc
            end;
        (_, SolutionsAcc) ->
            SolutionsAcc
    end, Acc, Facts).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds rule solutions for a specific goal.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_rule_solutions_for_goal(pred_ref(), #{predicate() => [clause()]}, [fact()],
                                   #{atom() => term()}, binding(), [binding()], non_neg_integer()) -> [binding()].

find_rule_solutions_for_goal(_Goal, _Clauses, _Facts, _ExtraFacts, _Binding, Acc, Depth)
        when Depth >= ?MAX_RECURSION_DEPTH ->
    Acc;
find_rule_solutions_for_goal({PredName, Args}, Clauses, Facts, ExtraFacts, Binding, Acc, Depth) ->
    case maps:get(PredName, Clauses, []) of
        [] -> Acc;
        PredClauses ->
            lists:foldl(fun(Clause, SolutionsAcc) ->
                #{head := {PredName, HeadArgs}, body := Body} = Clause,
                case unify(Args, HeadArgs, Binding) of
                    {ok, NewBinding} ->
                        BodySolutions = prove_body_all(Body, Clauses, Facts, ExtraFacts, NewBinding, Depth + 1),
                        BodySolutions ++ SolutionsAcc;
                    no_match ->
                        SolutionsAcc
                end
            end, Acc, PredClauses)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Finds solutions from rules.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_rule_solutions(pred_ref(), #{predicate() => [clause()]}, [fact()],
                          #{atom() => term()}, binding(), [binding()], non_neg_integer()) -> [binding()].

find_rule_solutions(_Goal, _Clauses, _Facts, _ExtraFacts, _Binding, Acc, Depth)
        when Depth >= ?MAX_RECURSION_DEPTH ->
    Acc;
find_rule_solutions({PredName, Args}, Clauses, Facts, ExtraFacts, Binding, Acc, Depth) ->
    case maps:get(PredName, Clauses, []) of
        [] -> Acc;
        PredClauses ->
            lists:foldl(fun(Clause, SolutionsAcc) ->
                #{head := {PredName, HeadArgs}, body := Body} = Clause,
                case unify(Args, HeadArgs, Binding) of
                    {ok, NewBinding} ->
                        %% Find all solutions for the body
                        BodySolutions = prove_body_all(Body, Clauses, Facts, ExtraFacts, NewBinding, Depth + 1),
                        BodySolutions ++ SolutionsAcc;
                    no_match ->
                        SolutionsAcc
                end
            end, Acc, PredClauses)
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs doctests from the moduledoc and function documentation.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    %% Test 1: Simple enabled check
    {ok, Rules} = compile(<<"enabled(a) :- token(coin_slot, coin).">>),
    Facts = facts_from_marking(#{coin_slot => [coin], storage => []}),
    true = bool(Rules, {enabled, a}, Facts, #{}),
    false = bool(Rules, {enabled, b}, Facts, #{}),

    %% Test 2: Query with variable bindings
    {ok, R2} = compile(<<"take(X) :- token(p, X).">>),
    F2 = facts_from_marking(#{p => [a, b]}),
    {ok, Bs} = query(R2, {take, {var, x}}, F2, #{}),
    [a, b] = lists:sort([maps:get(x, B) || B <- Bs]),

    %% Test 3: Error on malformed program
    {error, bad_program} = compile(<<"enabled(a) :- .">>),

    %% Test 4: Empty marking produces no facts
    [] = facts_from_marking(#{}),

    %% Test 5: Multiple tokens in same place
    {ok, R3} = compile(<<"has(X) :- token(p, X).">>),
    F3 = facts_from_marking(#{p => [1, 2, 3]}),
    {ok, Bs3} = query(R3, {has, {var, x}}, F3, #{}),
    3 = length(Bs3),

    ok.

%% Unit tests for tokenize
tokenize_simple_atom_test() ->
    Tokens = tokenize(<<"hello">>),
    [hello] = Tokens.

tokenize_predicate_test() ->
    Tokens = tokenize(<<"token(coin_slot, coin)">>),
    [token, '(', coin_slot, ',', coin, ')'] = Tokens.

tokenize_rule_test() ->
    Tokens = tokenize(<<"enabled(a) :- token(coin_slot, coin).">>),
    [enabled, '(', a, ')', ':-', token, '(', coin_slot, ',', coin, ')', '.'] = Tokens.

tokenize_variable_test() ->
    Tokens = tokenize(<<"take(X)">>),
    [take, '(', {var, x}, ')'] = Tokens.

tokenize_uppercase_variable_test() ->
    Tokens = tokenize(<<"take(VarName)">>),
    [take, '(', {var, varname}, ')'] = Tokens.

%% Unit tests for parse
parse_fact_test() ->
    Tokens = [token, '(', coin_slot, ',', coin, ')', '.'],
    {[#{head := {token, [coin_slot, coin]}, body := []}], []} = parse_clauses(Tokens).

parse_rule_test() ->
    Tokens = [enabled, '(', a, ')', ':-', token, '(', coin_slot, ',', coin, ')', '.'],
    {[#{head := {enabled, [a]}, body := [{token, [coin_slot, coin]}]}], []} = parse_clauses(Tokens).

parse_multiple_clauses_test() ->
    Tokens = [fact, '(', a, ')', '.', fact, '(', b, ')', '.'],
    {[C1, C2], []} = parse_clauses(Tokens),
    {fact, [a]} = maps:get(head, C1),
    {fact, [b]} = maps:get(head, C2).

%% Unit tests for facts_from_marking
facts_empty_test() ->
    [] = facts_from_marking(#{}).

facts_single_place_test() ->
    Facts = facts_from_marking(#{p => [a]}),
    [{token, [p, a]}] = Facts.

facts_multiple_tokens_test() ->
    Facts = facts_from_marking(#{p => [a, b, c]}),
    3 = length(Facts),
    true = lists:member({token, [p, a]}, Facts),
    true = lists:member({token, [p, b]}, Facts),
    true = lists:member({token, [p, c]}, Facts).

facts_empty_place_test() ->
    Facts = facts_from_marking(#{p => [], q => [a]}),
    [{token, [q, a]}] = Facts.

%% Unit tests for unify
unify_atoms_test() ->
    {ok, #{}} = unify([a, b], [a, b], #{}).

unify_variable_bind_test() ->
    {ok, #{x := a}} = unify([{var, x}], [a], #{}).

unify_variable_already_bound_test() ->
    {ok, #{x := a}} = unify([{var, x}], [a], #{x => a}).

unify_variable_conflict_test() ->
    no_match = unify([{var, x}], [a], #{x => b}).

unify_multiple_vars_test() ->
    {ok, #{x := a, y := b}} = unify([{var, x}, {var, y}], [a, b], #{}).

%% Unit tests for bool
bool_simple_true_test() ->
    {ok, Rules} = compile(<<"p :- q. q.">>),
    Facts = [{q, []}],
    true = bool(Rules, {p, []}, Facts, #{}).

bool_simple_false_test() ->
    {ok, Rules} = compile(<<"p :- q.">>),
    Facts = [{r, []}],
    false = bool(Rules, {p, []}, Facts, #{}).

bool_with_token_fact_test() ->
    {ok, Rules} = compile(<<"enabled :- token(slot, coin).">>),
    Facts = [{token, [slot, coin]}],
    true = bool(Rules, {enabled, []}, Facts, #{}).

bool_no_token_fact_test() ->
    {ok, Rules} = compile(<<"enabled :- token(slot, coin).">>),
    Facts = [{token, slot, other}],
    false = bool(Rules, {enabled, []}, Facts, #{}).

bool_complex_rule_test() ->
    {ok, Rules} = compile(<<"grandparent(X, Y) :- parent(X, Z), parent(Z, Y).">>),
    Facts = [{parent, [bob, alice]}, {parent, [alice, eve]}],
    true = bool(Rules, {grandparent, [bob, eve]}, Facts, #{}).

%% Unit tests for query
query_no_vars_test() ->
    {ok, Rules} = compile(<<"p(a). p(b).">>),
    Facts = [],
    {ok, Bs} = query(Rules, {p, a}, Facts, #{}),
    1 = length(Bs).

query_single_var_test() ->
    {ok, Rules} = compile(<<"p(X) :- token(t, X).">>),
    Facts = [{token, [t, a]}, {token, [t, b]}],
    {ok, Bs} = query(Rules, {p, {var, x}}, Facts, #{}),
    2 = length(Bs),
    true = lists:any(fun(B) -> maps:get(x, B) =:= a end, Bs),
    true = lists:any(fun(B) -> maps:get(x, B) =:= b end, Bs).

query_multiple_vars_test() ->
    {ok, Rules} = compile(<<"p(X, Y) :- token(t, X, Y).">>),
    Facts = [{token, [t, a, b]}, {token, [t, c, d]}],
    {ok, Bs} = query(Rules, {p, {var, x}, {var, y}}, Facts, #{}),
    2 = length(Bs).

query_empty_result_test() ->
    {ok, Rules} = compile(<<"p(X) :- token(t, X).">>),
    Facts = [{token, other, a}],
    {ok, []} = query(Rules, {p, {var, x}}, Facts, #{}).

query_transitive_test() ->
    {ok, Rules} = compile(<<"ancestor(X, Y) :- parent(X, Y). ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).">>),
    Facts = [{parent, [a, b]}, {parent, [b, c]}, {parent, [c, d]}],
    {ok, Bs} = query(Rules, {ancestor, {var, x}, d}, Facts, #{}),
    3 = length(Bs).

%% Unit tests for compile
compile_valid_program_test() ->
    {ok, _} = compile(<<"p. q :- r.">>).

compile_empty_program_test() ->
    {ok, _} = compile(<<"">>).

compile_with_comments_test() ->
    {ok, _} = compile(<<"% comment\np.">>).

compile_bad_program_missing_period_test() ->
    {error, bad_program} = compile(<<"p(a)">>).

compile_bad_program_malformed_test() ->
    {error, bad_program} = compile(<<"p :- .">>).

compile_quoted_atoms_test() ->
    {ok, _} = compile(<<"p('atom with spaces').">>).

compile_binary_strings_test() ->
    {ok, _} = compile(<<"p(\"binary string\").">>).

compile_numbers_test() ->
    {ok, _} = compile(<<"p(42).">>).

%% Integration tests
integration_workflow_rules_test() ->
    %% Simulate workflow decision rules
    Program = <<
        "approve(User) :- token(approval_pool, User), has_manager(User)."
        "has_manager(User) :- token(staff, User)."
    >>,
    {ok, Rules} = compile(Program),
    Facts = facts_from_marking(#{
        approval_pool => [alice, bob],
        staff => [alice]
    }),
    true = bool(Rules, {approve, alice}, Facts, #{}),
    false = bool(Rules, {approve, bob}, Facts, #{}),
    ok.

integration_multi_place_facts_test() ->
    {ok, Rules} = compile(<<"ready :- token(input, data), token(config, settings).">>),
    Facts = facts_from_marking(#{
        input => [data],
        config => [settings],
        other => [ignored]
    }),
    true = bool(Rules, {ready, []}, Facts, #{}),
    ok.

-endif.
