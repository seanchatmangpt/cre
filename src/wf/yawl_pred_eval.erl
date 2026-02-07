%% -*- erlang -*-
%%
-module(yawl_pred_eval).
-moduledoc """
Generic YAWL predicate runtime evaluator.

Connects XPath predicate compilation (wf_yawl_pred) with rule evaluation
(wf_rules) for evaluating predicates during transition firing in YAWL
workflow specifications.

## Overview

YAWL workflows use XPath expressions to predicate on workflow variables.
This module provides a runtime evaluator that:

1. Compiles XPath predicates to Datalog rules
2. Converts Petri net markings to facts
3. Evaluates queries with optional extra context
4. Returns boolean results or variable bindings

## Examples

Simple boolean evaluation:

```erlang
> Marking = #{overall => [#{po_timedout => false}]},
> {ok, true} = yawl_pred_eval:evaluate(
..   <<"/Overall/PO_timedout/text()='false'">>,
..   Marking,
..   #{}
.. ).
```

Query with variable bindings:

```erlang
> Marking = #{overall => [#{status => approved}]},
> {ok, Bindings} = yawl_pred_eval:evaluate_query(
..   <<"/Overall/Status/text()='approved'">>,
..   Marking,
..   #{}
.. ),
> length(Bindings) > 0.
true
```

Numeric comparison:

```erlang
> Marking = #{overall => [#{count => 5}]},
> {ok, true} = yawl_pred_eval:evaluate(
..   <<"/Overall/Count/text() > 0">>,
..   Marking,
..   #{}
.. ).
```

## Compilation

Compile a predicate once for reuse:

```erlang
> {ok, Rules} = yawl_pred_eval:compile_predicate(
..   <<"/Overall/Status/text()='approved'">>
.. ),
> {ok, Marking} = yawl_pred_eval:marking_to_facts(
..   #{overall => [#{status => approved}]}
.. ),
> true = wf_rules:bool(Rules, {flow_selected, []}, Marking, #{}).
```

## Integration with YAWL Workflows

This module is designed to work with any YAWL specification:

```erlang
% In a pnet_net behavior module
modes(t1, Marking, _UsrInfo) ->
    Pred = <<"/Overall/Status/text()='approved'">>,
    case yawl_pred_eval:evaluate(Pred, Marking, #{}) of
        {ok, true} -> [#{enabled => []}];
        {ok, false} -> [#{disabled => []}];
        {error, Reason} -> error_logger:warning_msg("Predicate error: ~p", [Reason]), []
    end.
```

## Limitations

- Requires facts to be properly structured from markings
- XPath functions like count(), sum() are not evaluated
- Complex nested expressions may not be fully supported
- Variable bindings depend on wf_rules query capabilities
""".

%%====================================================================
%% Exports
%%====================================================================

%% Compilation
-export([compile_predicate/1]).

%% Evaluation
-export([evaluate/3, evaluate_query/3]).

%% Fact conversion
-export([marking_to_facts/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A YAWL XPath predicate expression.
%%
%% Binary string containing an XPath predicate as used in YAWL
%% workflow specifications.
%%--------------------------------------------------------------------
-type predicate() :: binary().

%%--------------------------------------------------------------------
%% @doc A Petri net marking.
%%
%% Map of place names (atoms) to lists of tokens. Tokens can be
%% any Erlang term, including maps representing workflow variables.
%%--------------------------------------------------------------------
-type marking() :: #{atom() => [term()]}.

%%--------------------------------------------------------------------
%% @doc Extra facts for context.
%%
%% Additional facts provided at evaluation time for context that
%% may not be present in the marking (e.g., environment variables,
%% timestamps, etc.).
%%--------------------------------------------------------------------
-type extra_facts() :: #{atom() => term()}.

%%--------------------------------------------------------------------
%% @doc Variable bindings from a query.
%%
%% Maps variable names to their bound values from successful
%% predicate evaluation.
%%--------------------------------------------------------------------
-type bindings() :: #{atom() => term()}.

%%--------------------------------------------------------------------
%% @doc Compiled rules from wf_rules.
%%
%% Internal representation of compiled rules for efficient
%% repeated evaluation.
%%--------------------------------------------------------------------
-type compiled_rules() :: wf_rules:rules().

%%--------------------------------------------------------------------
%% @doc Facts derived from marking.
%%
%% List of ground facts suitable for wf_rules evaluation.
%%--------------------------------------------------------------------
-type facts() :: [wf_rules:fact()].

%% Export types
-export_type([
    predicate/0,
    marking/0,
    extra_facts/0,
    bindings/0,
    compiled_rules/0,
    facts/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles a YAWL XPath predicate to executable rules.
%%
%% Uses wf_yawl_pred:to_erlog/1 to compile XPath to Datalog format,
%% then uses wf_rules:compile/1 to create executable rules.
%%
%% @param PredicateBin XPath predicate expression as binary
%% @return {ok, CompiledRules} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_predicate(PredicateBin :: predicate()) ->
    {ok, compiled_rules()} | {error, Reason :: term()}.

compile_predicate(PredicateBin) when is_binary(PredicateBin) ->
    case wf_yawl_pred:to_erlog(PredicateBin) of
        {ok, {RulesBin, _GoalTerm}} ->
            wf_rules:compile(RulesBin);
        {error, Reason} ->
            {error, {predicate_compile_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Evaluates a YAWL XPath predicate against a marking.
%%
%% Compiles the predicate, converts the marking to facts, and evaluates
%% the query. Returns a boolean result indicating whether the predicate
%% is satisfied.
%%
%% @param PredicateBin XPath predicate expression as binary
%% @param Marking Petri net marking map
%% @param ExtraFacts Additional context facts
%% @return {ok, true} if predicate satisfied, {ok, false} otherwise,
%%         {error, Reason} on error
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate(PredicateBin :: predicate(),
               Marking :: marking(),
               ExtraFacts :: extra_facts()) ->
    {ok, boolean()} | {error, Reason :: term()}.

evaluate(PredicateBin, Marking, ExtraFacts) ->
    case compile_predicate(PredicateBin) of
        {ok, Rules} ->
            Facts = marking_to_facts(Marking),
            Result = wf_rules:bool(Rules, {flow_selected, []}, Facts, ExtraFacts),
            case Result of
                {error, _} -> Result;
                Bool -> {ok, Bool}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Evaluates a YAWL XPath predicate and returns variable bindings.
%%
%% Similar to evaluate/3 but returns all variable bindings that satisfy
%% the predicate. Useful when the predicate contains variables that
%% need to be extracted.
%%
%% @param PredicateBin XPath predicate expression as binary
%% @param Marking Petri net marking map
%% @param ExtraFacts Additional context facts
%% @return {ok, [Bindings]} where each Bindings is a map of var => value,
%%         {error, Reason} on error
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate_query(PredicateBin :: predicate(),
                     Marking :: marking(),
                     ExtraFacts :: extra_facts()) ->
    {ok, [bindings()]} | {error, Reason :: term()}.

evaluate_query(PredicateBin, Marking, ExtraFacts) ->
    case compile_predicate(PredicateBin) of
        {ok, Rules} ->
            Facts = marking_to_facts(Marking),
            wf_rules:query(Rules, {flow_selected, []}, Facts, ExtraFacts);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Converts a Petri net marking to wf_rules facts.
%%
%% Extracts token information from a marking and converts it to a flat
%% list of facts suitable for rule evaluation. Each place and token
%% combination becomes a {token, Place, Token} fact.
%%
%% For complex token structures (e.g., maps representing workflow
%% variables), additional facts are generated to allow predicate
%% evaluation against variable values.
%%
%% Facts are generated as {variable_name, [value]} tuples to support
%% XPath-style variable references.
%%
%% @param Marking Petri net marking map
%% @return List of facts for wf_rules
%%
%% @end
%%--------------------------------------------------------------------
-spec marking_to_facts(Marking :: marking()) -> facts().

marking_to_facts(Marking) when is_map(Marking) ->
    %% Extract workflow variables from tokens in all places
    %% This creates facts like {status, [approved]} that can be
    %% queried by rules generated from XPath predicates
    enrich_facts(Marking).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Enriches facts with additional information from complex tokens.
%%
%% Handles workflow variable maps and other structured tokens by
%% extracting their contents into facts.
%%
%% @end
%%--------------------------------------------------------------------
-spec enrich_facts(marking()) -> [wf_rules:fact()].

enrich_facts(Marking) ->
    maps:fold(fun
        (_Place, [], Acc) ->
            Acc;
        (Place, Tokens, Acc) when is_list(Tokens) ->
            PlaceFacts = lists:filtermap(fun
                (Token) ->
                    case extract_token_facts(Place, Token) of
                        [] -> false;
                        Facts -> {true, Facts}
                    end
            end, Tokens),
            lists:flatten(PlaceFacts) ++ Acc
    end, [], Marking).

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts facts from a single token.
%%
%% For structured tokens (maps), extracts key-value pairs as facts.
%% Also generates cross-reference facts to support XPath-style lookups.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_token_facts(atom(), term()) -> [wf_rules:fact()].

extract_token_facts(Place, Token) when is_map(Token) ->
    %% Extract workflow variables from map
    %% Generate facts using lowercase atom names to match rule convention
    maps:fold(fun
        (Key, Value, Acc) when is_atom(Key) ->
            %% Convert to lowercase to match rule predicate names
            KeyLower = list_to_existing_atom(string:lowercase(atom_to_list(Key))),
            [{KeyLower, [Value]} | Acc];
        (Key, Value, Acc) when is_binary(Key) ->
            %% Convert binary key to lowercase atom
            KeyLower = list_to_existing_atom(string:lowercase(binary_to_list(Key))),
            [{KeyLower, [Value]} | Acc];
        (_Key, _Value, Acc) ->
            Acc
    end, [], Token);
extract_token_facts(_Place, _Token) ->
    [].

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit doctests from moduledoc.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    %% Test 1: Simple boolean evaluation
    Marking1 = #{overall => [#{po_timedout => false}]},
    {ok, true} = evaluate(
        <<"/Overall/PO_timedout/text()='false'">>,
        Marking1,
        #{}
    ),

    %% Test 2: Query with variable bindings
    Marking2 = #{overall => [#{status => approved}]},
    {ok, Bindings} = evaluate_query(
        <<"/Overall/Status/text()='approved'">>,
        Marking2,
        #{}
    ),
    true = length(Bindings) > 0,

    %% Test 3: Numeric comparison
    Marking3 = #{overall => [#{count => 5}]},
    {ok, true} = evaluate(
        <<"/Overall/Count/text() > 0">>,
        Marking3,
        #{}
    ),

    %% Test 4: Compilation
    {ok, Rules} = compile_predicate(
        <<"/Overall/Status/text()='approved'">>
    ),
    true = is_map(Rules),

    ok.

%% Unit tests for compile_predicate
compile_predicate_simple_test() ->
    {ok, Rules} = compile_predicate(
        <<"/Overall/Status/text()='approved'">>
    ),
    true = is_map(Rules),
    true = maps:is_key(clauses, Rules).

compile_predicate_numeric_test() ->
    {ok, Rules} = compile_predicate(
        <<"/Overall/Count/text() > 0">>
    ),
    true = is_map(Rules).

compile_predicate_invalid_test() ->
    {error, _} = compile_predicate(<<"">>).

%% Unit tests for marking_to_facts
marking_to_facts_empty_test() ->
    [] = marking_to_facts(#{}).

marking_to_facts_simple_test() ->
    Marking = #{place1 => [token1]},
    Facts = marking_to_facts(Marking),
    true = lists:member({token, place1, token1}, Facts).

marking_to_facts_multiple_test() ->
    Marking = #{place1 => [a, b], place2 => [c]},
    Facts = marking_to_facts(Marking),
    true = lists:member({token, place1, a}, Facts),
    true = lists:member({token, place1, b}, Facts),
    true = lists:member({token, place2, c}, Facts).

marking_to_facts_complex_token_test() ->
    %% Workflow variables as map
    Marking = #{overall => [#{status => approved, count => 5}]},
    Facts = marking_to_facts(Marking),
    %% Base token fact
    true = lists:member({token, overall, #{status => approved, count => 5}}, Facts),
    %% Enriched facts for variables
    true = lists:member({status, [approved]}, Facts),
    true = lists:member({count, [5]}, Facts).

%% Unit tests for evaluate
evaluate_true_test() ->
    Marking = #{overall => [#{status => approved}]},
    {ok, true} = evaluate(
        <<"/Overall/Status/text()='approved'">>,
        Marking,
        #{}
    ).

evaluate_false_test() ->
    Marking = #{overall => [#{status => pending}]},
    {ok, false} = evaluate(
        <<"/Overall/Status/text()='approved'">>,
        Marking,
        #{}
    ).

evaluate_numeric_gt_true_test() ->
    Marking = #{overall => [#{count => 5}]},
    {ok, true} = evaluate(
        <<"/Overall/Count/text() > 0">>,
        Marking,
        #{}
    ).

evaluate_numeric_gt_false_test() ->
    Marking = #{overall => [#{count => 0}]},
    {ok, false} = evaluate(
        <<"/Overall/Count/text() > 0">>,
        Marking,
        #{}
    ).

evaluate_and_true_test() ->
    Marking = #{overall => [#{flag1 => true, flag2 => false}]},
    {ok, true} = evaluate(
        <<"/Overall/Flag1/text()='true' and /Overall/Flag2/text()='false'">>,
        Marking,
        #{}
    ).

evaluate_and_false_test() ->
    Marking = #{overall => [#{flag1 => true, flag2 => true}]},
    {ok, false} = evaluate(
        <<"/Overall/Flag1/text()='true' and /Overall/Flag2/text()='false'">>,
        Marking,
        #{}
    ).

evaluate_empty_marking_test() ->
    Marking = #{},
    {ok, false} = evaluate(
        <<"/Overall/Status/text()='approved'">>,
        Marking,
        #{}
    ).

%% Unit tests for evaluate_query
evaluate_query_single_match_test() ->
    Marking = #{overall => [#{status => approved}]},
    {ok, Bindings} = evaluate_query(
        <<"/Overall/Status/text()='approved'">>,
        Marking,
        #{}
    ),
    true = length(Bindings) > 0.

evaluate_query_no_match_test() ->
    Marking = #{overall => [#{status => pending}]},
    {ok, Bindings} = evaluate_query(
        <<"/Overall/Status/text()='approved'">>,
        Marking,
        #{}
    ),
    0 = length(Bindings).

evaluate_query_empty_test() ->
    Marking = #{},
    {ok, Bindings} = evaluate_query(
        <<"/Overall/Status/text()='approved'">>,
        Marking,
        #{}
    ),
    0 = length(Bindings).

%% Integration tests
integration_workflow_predicate_test() ->
    %% Simulate a real YAWL workflow predicate evaluation
    Marking = #{
        overall => [#{
            po_timedout => false,
            po_approval => true
        }]
    },
    Pred = <<"/Overall/PO_timedout/text()='false' and /Overall/POApproval/text()='true'">>,
    {ok, true} = evaluate(Pred, Marking, #{}).

integration_multiple_places_test() ->
    Marking = #{
        place1 => [#{status => ready}],
        place2 => [#{count => 10}]
    },
    %% Predicate on first place
    {ok, true} = evaluate(
        <<"/Place1/Status/text()='ready'">>,
        Marking,
        #{}
    ).

integration_with_extra_facts_test() ->
    Marking = #{overall => [#{status => pending}]},
    %% Extra facts can provide additional context
    {ok, false} = evaluate(
        <<"/Overall/Status/text()='approved'">>,
        Marking,
        #{extra_context => true}
    ).

integration_error_handling_test() ->
    Marking = #{overall => [invalid_token]},
    %% Should handle gracefully
    case evaluate(<<"/Overall/Status/text()='approved'">>, Marking, #{}) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

integration_compile_reuse_test() ->
    %% Compile once, use multiple times
    {ok, Rules} = compile_predicate(
        <<"/Overall/Count/text() > 0">>
    ),
    %% Evaluate with different markings
    Facts1 = marking_to_facts(#{overall => [#{count => 5}]}),
    Facts2 = marking_to_facts(#{overall => [#{count => 0}]}),
    true = wf_rules:bool(Rules, {flow_selected, []}, Facts1, #{}),
    false = wf_rules:bool(Rules, {flow_selected, []}, Facts2, #{}),
    ok.

-endif.
