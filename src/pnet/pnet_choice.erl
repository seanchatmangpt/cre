%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------

-module(pnet_choice).

-moduledoc """
Deterministic choice (pure RNG state threading).

This module provides deterministic random choice operations for Petri net
mode selection. The RNG state is explicitly threaded through all operations,
ensuring reproducibility: same seed + same inputs always yield same choices.

Uses the rand module with exrop algorithm for deterministic results
across Erlang versions and platforms.

```erlang
> R0 = pnet_choice:seed(42).
> {C1, R1} = pnet_choice:pick([a,b,c], R0).
> lists:member(C1, [a,b,c]) andalso R1 =/= R0.
true
```

```erlang
> R0a = pnet_choice:seed(42),
.. R0b = pnet_choice:seed(42),
.. {X1, _} = pnet_choice:pick([a,b,c], R0a),
.. {X2, _} = pnet_choice:pick([a,b,c], R0b),
.. X1 =:= X2.
true
```

```erlang
> pnet_choice:pick([], pnet_choice:seed(1)).
{error,empty}
```

```erlang
> Rw0 = pnet_choice:seed(9).
> {W1, _Rw1} = pnet_choice:pick_weighted([{a,1},{b,3},{c,1}], Rw0).
> lists:member(W1, [a,b,c]).
true
```

```erlang
> pnet_choice:pick_weighted([], pnet_choice:seed(1)).
{error,empty}
> pnet_choice:pick_weighted([{a,0}], pnet_choice:seed(1)).
{error,bad_weights}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% RNG state management
-export([seed/1]).

%% Choice operations
-export([pick/2, pick_weighted/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc RNG state from rand module.
%%
%% Opaque state representing the random number generator state.
%% Use seed/1 to create an initial state.
%%--------------------------------------------------------------------
-type rand_state() :: rand:state().

%%--------------------------------------------------------------------
%% @doc A weighted element for weighted choice.
%%
%% Tuple of {Element, Weight} where Weight is a non-negative integer.
%% Elements with zero weight are never selected.
%%--------------------------------------------------------------------
-type weighted(Elem) :: {Elem, non_neg_integer()}.

%% Export types
-export_type([rand_state/0, weighted/1]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an RNG state from an integer seed.
%%
%% Uses the exrop algorithm for deterministic results across platforms.
%% The same seed value will always produce the same sequence of choices.
%%
%% @param Seed Integer seed value (any integer)
%% @return New RNG state
%%
%% @end
%%--------------------------------------------------------------------
-spec seed(integer()) -> rand_state().

seed(Seed) when is_integer(Seed) ->
    rand:seed_s(exrop, {Seed, 0, 0}).

%%--------------------------------------------------------------------
%% @doc Uniformly picks a random element from a non-empty list.
%%
%% Returns {Element, NewState} where Element is from the input list
%% and NewState is the updated RNG state. The same seed with the same
%% list will always return the same element.
%%
%% @param List Non-empty list of elements to choose from
%% @param RandState Current RNG state
%% @return {Element, NewState} or {error, empty}
%%
%% @end
%%--------------------------------------------------------------------
-spec pick(List :: [E], RandState :: rand_state()) ->
          {E, rand_state()} | {error, empty}.

pick([], _RandState) ->
    {error, empty};
pick(List, RandState) when is_list(List), length(List) > 0 ->
    Len = length(List),
    {Index, NewState} = rand:uniform_s(Len, RandState),
    Element = lists:nth(Index, List),
    {Element, NewState}.

%%--------------------------------------------------------------------
%% @doc Picks a random element based on weights.
%%
%% Elements are selected with probability proportional to their weight.
%% Higher weights mean higher probability of selection. Elements with
%% zero weight are never selected.
%%
%% The input is a list of {Element, Weight} tuples.
%%
%% @param WeightedList Non-empty list of {Element, Weight} tuples
%% @param RandState Current RNG state
%% @return {Element, NewState} or {error, empty | bad_weights}
%%
%% @end
%%--------------------------------------------------------------------
-spec pick_weighted(WeightedList :: [weighted(E)], RandState :: rand_state()) ->
          {E, rand_state()} | {error, empty | bad_weights}.

pick_weighted([], _RandState) ->
    {error, empty};
pick_weighted(WeightedList, RandState) when is_list(WeightedList) ->
    case validate_weights(WeightedList) of
        {ok, TotalWeight} when TotalWeight > 0 ->
            {Value, NewState} = rand:uniform_s(TotalWeight, RandState),
            Element = select_by_weight(WeightedList, Value, 0),
            {Element, NewState};
        {ok, 0} ->
            {error, bad_weights};
        {error, _Reason} = Error ->
            Error
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validates the weighted list and computes total weight.
%%
%% Returns {ok, TotalWeight} if all weights are non-negative integers,
%% or {error, bad_weights} if any weight is invalid.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_weights([weighted(_)]) ->
          {ok, non_neg_integer()} | {error, bad_weights}.

validate_weights(WeightedList) ->
    validate_weights(WeightedList, 0).

validate_weights([], Total) ->
    {ok, Total};
validate_weights([{_Elem, W} | Rest], Acc) when is_integer(W), W >= 0 ->
    validate_weights(Rest, Acc + W);
validate_weights(_Invalid, _Acc) ->
    {error, bad_weights}.

%%--------------------------------------------------------------------
%% @private
%% @doc Selects an element based on accumulated weight threshold.
%%
%% Traverses the weighted list, accumulating weights until the threshold
%% is exceeded, then returns that element.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_by_weight([weighted(E)], non_neg_integer(), non_neg_integer()) -> E
      ; ([weighted(_)], non_neg_integer(), non_neg_integer()) -> term().

select_by_weight([{Elem, W} | _Rest], Threshold, Acc) when Threshold < Acc + W ->
    Elem;
select_by_weight([{_Elem, W} | Rest], Threshold, Acc) when W =:= 0 ->
    %% Skip zero-weight elements
    select_by_weight(Rest, Threshold, Acc);
select_by_weight([{Elem, W} | _Rest], Threshold, Acc) when Threshold =:= Acc + W ->
    %% Threshold exactly at boundary - select this element
    Elem;
select_by_weight([{_Elem, W} | Rest], Threshold, Acc) ->
    select_by_weight(Rest, Threshold, Acc + W);
select_by_weight([], _Threshold, _Acc) ->
    %% Should never happen if validate_weights ensures TotalWeight > 0
    error({unexpected, empty_list}).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%% Additional unit tests for edge cases

seed_returns_state_test() ->
    R = seed(42),
    %% rand:seed_s/2 returns a valid rand state
    %% The format varies by Erlang version - just verify it works with pick
    {C, _R1} = pick([a, b], R),
    ?assert(lists:member(C, [a, b])).

pick_empty_test() ->
    R0 = seed(1),
    ?assertEqual({error, empty}, pick([], R0)).

pick_non_empty_test() ->
    R0 = seed(42),
    {C, _R1} = pick([a, b, c], R0),
    ?assert(lists:member(C, [a, b, c])).

pick_deterministic_test() ->
    R0a = seed(42),
    R0b = seed(42),
    {X1, _} = pick([a, b, c], R0a),
    {X2, _} = pick([a, b, c], R0b),
    ?assertEqual(X1, X2).

pick_advances_state_test() ->
    R0 = seed(42),
    {_, R1} = pick([a, b, c], R0),
    ?assertNotEqual(R0, R1).

pick_sequence_test() ->
    R0 = seed(123),
    {X1, R1} = pick([a, b, c], R0),
    {X2, R2} = pick([a, b, c], R1),
    {X3, _R3} = pick([a, b, c], R2),
    %% All should be valid elements
    ?assert(lists:member(X1, [a, b, c])),
    ?assert(lists:member(X2, [a, b, c])),
    ?assert(lists:member(X3, [a, b, c])),
    %% Sequence should be deterministic
    R0a = seed(123),
    {Y1, R1a} = pick([a, b, c], R0a),
    {Y2, R2a} = pick([a, b, c], R1a),
    {Y3, _R3a} = pick([a, b, c], R2a),
    ?assertEqual(X1, Y1),
    ?assertEqual(X2, Y2),
    ?assertEqual(X3, Y3).

pick_weighted_empty_test() ->
    R0 = seed(1),
    ?assertEqual({error, empty}, pick_weighted([], R0)).

pick_weighted_zero_total_test() ->
    R0 = seed(1),
    ?assertEqual({error, bad_weights}, pick_weighted([{a, 0}], R0)),
    ?assertEqual({error, bad_weights}, pick_weighted([{a, 0}, {b, 0}], R0)).

pick_weighted_invalid_weight_test() ->
    R0 = seed(1),
    ?assertEqual({error, bad_weights}, pick_weighted([{a, -1}], R0)),
    ?assertEqual({error, bad_weights}, pick_weighted([{a, 1.5}], R0)),
    ?assertEqual({error, bad_weights}, pick_weighted([{a, abc}], R0)).

pick_weighted_valid_test() ->
    R0 = seed(9),
    {W, _R1} = pick_weighted([{a, 1}, {b, 3}, {c, 1}], R0),
    ?assert(lists:member(W, [a, b, c])).

pick_weighted_deterministic_test() ->
    R0a = seed(7),
    R0b = seed(7),
    {X1, _} = pick_weighted([{a, 1}, {b, 2}, {c, 3}], R0a),
    {X2, _} = pick_weighted([{a, 1}, {b, 2}, {c, 3}], R0b),
    ?assertEqual(X1, X2).

pick_weighted_distribution_test() ->
    %% With seed 42, verify weighted selection works
    %% Not testing full distribution, just that it runs
    R0 = seed(42),
    {W, _R1} = pick_weighted([{a, 10}, {b, 1}], R0),
    ?assert(lists:member(W, [a, b])).

pick_weighted_zero_allowed_test() ->
    %% Zero-weight elements should be skipped
    R0 = seed(5),
    {W, _R1} = pick_weighted([{a, 0}, {b, 1}, {c, 0}], R0),
    ?assertEqual(b, W).

pick_weighted_single_element_test() ->
    R0 = seed(1),
    {W, _R1} = pick_weighted([{only, 100}], R0),
    ?assertEqual(only, W).

-endif.
