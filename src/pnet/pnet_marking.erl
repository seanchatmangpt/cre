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

-compile({no_auto_import,[apply/2]}).
-module(pnet_marking).

-moduledoc """
Multiset marking algebra (places -> bag of tokens).

Places map to token lists representing multisets. Operations respect
multiplicity: adding [a,b] to [a] gives [a,a,b]; taking [a] from [a,a,b]
leaves [a,b].

```erlang
> M0 = pnet_marking:new([p1, p2]).
> maps:get(p1, M0).
[]
> maps:get(p2, M0).
[]
> pnet_marking:get(M0, p1).
{ok, []}
> pnet_marking:get(M0, missing_place).
{ok, []}
```

```erlang
> M0 = pnet_marking:new([p1]).
> M1 = pnet_marking:set(M0, p1, [a,b]).
> pnet_marking:get(M1, p1).
{ok, [a,b]}
```

```erlang
> M0 = pnet_marking:new([p1, p2]).
> M1 = pnet_marking:set(M0, p1, [a,b]).
> M2 = pnet_marking:add(M1, #{p1 => [b], p2 => [c]}).
> {ok, T1} = pnet_marking:get(M2, p1).
> lists:sort(T1).
[a,b,b]
```

```erlang
> M0 = pnet_marking:new([p1, p2]).
> M1 = pnet_marking:set(M0, p1, [a,b]).
> M2 = pnet_marking:add(M1, #{p1 => [b], p2 => [c]}).
> {ok, M3} = pnet_marking:take(M2, #{p1 => [a], p2 => [c]}).
> {ok, T3} = pnet_marking:get(M3, p1).
> lists:sort(T3).
[b,b]
```

```erlang
> M0 = pnet_marking:new([p1, p2]).
> M1 = pnet_marking:set(M0, p1, [a,b]).
> M2 = pnet_marking:add(M1, #{p1 => [b], p2 => [c]}).
> pnet_marking:take(M2, #{p1 => [a,a,a]}).
{error,insufficient}
```

```erlang
> M0 = pnet_marking:new([p1, p2]).
> M1 = pnet_marking:set(M0, p1, [a,b]).
> M2 = pnet_marking:add(M1, #{p1 => [b], p2 => [c]}).
> {ok, M3} = pnet_marking:take(M2, #{p1 => [a], p2 => [c]}).
> Move = #{mode => #{p1 => [b]}, produce => #{p2 => [d]}}.
> {ok, M4} = pnet_marking:apply(M3, Move).
> {ok, T4p2} = pnet_marking:get(M4, p2).
> T4p2.
[d]
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Creation and basic operations
-export([new/1, get/2, set/3]).

%% Marking algebra operations
-export([add/2, take/2, apply/2, apply/3]).

%% Inspection and utilities
-export([snapshot/1, hash/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%
%% Places are locations where tokens reside. Represented as atoms
%% for efficient pattern matching.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net.
%%
%% Tokens can be any Erlang term, allowing flexible data flow.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% Each place atom maps to a list of tokens currently in that place.
%% Empty lists represent places with no tokens.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A consume map specifies tokens to be removed.
%%
%% Maps places to the specific list of tokens that will be consumed.
%%--------------------------------------------------------------------
-type consume_map() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A produce map specifies tokens to be added.
%%
%% Maps places to the list of tokens that will be produced.
%%--------------------------------------------------------------------
-type produce_map() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A move represents a complete transition firing.
%%
%% Contains the mode (tokens to consume) and produce map (tokens to add).
%%--------------------------------------------------------------------
-type move() :: #{mode := consume_map(), produce := produce_map()}.

%% Export types
-export_type([marking/0, place/0, token/0, consume_map/0,
              produce_map/0, move/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new empty marking with the given places.
%%
%% All places are initialized with empty token lists.
%%
%% @param Places List of place atoms to include in the marking
%% @return A new marking with all places initialized to empty
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Places :: [place()]) -> marking().

new(Places) when is_list(Places) ->
    maps:from_list([{P, []} || P <- Places]).

%%--------------------------------------------------------------------
%% @doc Gets the tokens at a specific place.
%%
%% Returns {ok, Tokens} where Tokens is the list at the place.
%% For unknown places, returns {ok, []} (total function).
%%
%% @param Marking The marking to query
%% @param Place The place to get tokens from
%% @return {ok, Tokens} - always returns ok, empty list for missing places
%%
%% @end
%%--------------------------------------------------------------------
-spec get(Marking :: marking(), Place :: place()) -> {ok, [token()]}.

get(Marking, Place) when is_map(Marking), is_atom(Place) ->
    case maps:find(Place, Marking) of
        {ok, Tokens} -> {ok, Tokens};
        error -> {ok, []}
    end.

%%--------------------------------------------------------------------
%% @doc Sets the tokens at a specific place.
%%
%% Replaces the current token list at the given place with a new list.
%% The place must exist in the marking.
%%
%% @param Marking The marking to modify
%% @param Place The place to set tokens at
%% @param Tokens The new token list for the place
%% @return Updated marking
%%
%% @end
%%--------------------------------------------------------------------
-spec set(Marking :: marking(), Place :: place(), Tokens :: [token()]) ->
          marking().

set(Marking, Place, Tokens) when is_map(Marking), is_atom(Place), is_list(Tokens) ->
    Marking#{Place => Tokens}.

%%--------------------------------------------------------------------
%% @doc Adds tokens to the marking via multiset union.
%%
%% Tokens are appended to existing tokens at each place.
%% Places are created if they don't exist.
%%
%% @param Marking The marking to add tokens to
%% @param ProduceMap Map of places to token lists to add
%% @return Updated marking
%%
%% @end
%%--------------------------------------------------------------------
-spec add(Marking :: marking(), ProduceMap :: produce_map()) -> marking().

add(Marking, ProduceMap) when is_map(Marking), is_map(ProduceMap) ->
    maps:fold(fun
        (Place, NewTokens, Acc) when is_atom(Place), is_list(NewTokens) ->
            ExistingTokens = maps:get(Place, Acc, []),
            Acc#{Place => ExistingTokens ++ NewTokens};
        (_, _, Acc) ->
            Acc
    end, Marking, ProduceMap).

%%--------------------------------------------------------------------
%% @doc Takes tokens from the marking via multiset subtraction.
%%
%% Returns {ok, UpdatedMarking} if all tokens are available with
%% sufficient multiplicity. Returns {error, insufficient} otherwise.
%%
%% @param Marking The marking to take tokens from
%% @param ConsumeMap Map of places to token lists to remove
%% @return {ok, marking()} | {error, insufficient}
%%
%% @end
%%--------------------------------------------------------------------
-spec take(Marking :: marking(), ConsumeMap :: consume_map()) ->
          {ok, marking()} | {error, insufficient}.

take(Marking, ConsumeMap) when is_map(Marking), is_map(ConsumeMap) ->
    take_fold(maps:to_list(ConsumeMap), Marking).

take_fold([], Marking) ->
    {ok, Marking};
take_fold([{Place, TokensToTake} | Rest], Marking) ->
    CurrentTokens = maps:get(Place, Marking, []),
    case multiset_subtract(CurrentTokens, TokensToTake) of
        {ok, Remaining} ->
            take_fold(Rest, Marking#{Place => Remaining});
        {error, insufficient} ->
            {error, insufficient}
    end.

%%--------------------------------------------------------------------
%% @doc Applies a move (consume then produce) atomically.
%%
%% A move contains a mode (tokens to consume) and produce map
%% (tokens to add). First consumes tokens via mode, then adds tokens
%% via produce map. Returns error if consumption fails.
%%
%% @param Marking The marking to apply the operation to
%% @param Move Map with mode and produce keys
%% @return {ok, UpdatedMarking} | {error, insufficient}
%%
%% @end
%%--------------------------------------------------------------------
-spec apply(Marking :: marking(), ConsumeMap :: consume_map(), ProduceMap :: produce_map()) ->
          {ok, marking()} | {error, insufficient}.
apply(Marking, ConsumeMap, ProduceMap) when is_map(Marking), is_map(ConsumeMap), is_map(ProduceMap) ->
    apply(Marking, #{mode => ConsumeMap, produce => ProduceMap}).

-spec apply(Marking :: marking(), Move :: move()) ->
          {ok, marking()} | {error, insufficient}.
apply(Marking, #{mode := Mode, produce := Produce}) ->
    case take(Marking, Mode) of
        {ok, Marking1} ->
            {ok, add(Marking1, Produce)};
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Creates a snapshot (copy) of the marking.
%%
%% Returns a copy equal to the input. Since Erlang data is immutable,
%% this returns the same marking but provides semantic clarity.
%%
%% @param Marking The marking to snapshot
%% @return An identical copy of the marking
%%
%% @end
%%--------------------------------------------------------------------
-spec snapshot(Marking :: marking()) -> marking().

snapshot(Marking) when is_map(Marking) ->
    Marking.

%%--------------------------------------------------------------------
%% @doc Computes a stable hash independent of insertion order.
%%
%% Uses SHA-256 on the canonical representation (sorted by place)
%% for consistent hashing regardless of map key ordering.
%%
%% @param Marking The marking to hash
%% @return Binary hash of the marking
%%
%% @end
%%--------------------------------------------------------------------
-spec hash(Marking :: marking()) -> binary().

hash(Marking) when is_map(Marking) ->
    %% Sort by place, and sort tokens for canonical representation
    Canonical = lists:sort([{Place, lists:sort(Tokens)} || {Place, Tokens} <- maps:to_list(Marking)]),
    crypto:hash(sha256, term_to_binary(Canonical)).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Multiset subtraction: removes tokens from Available.
%%
%% Returns {ok, Remaining} if all tokens in Remove are present
%% with sufficient multiplicity. Returns {error, insufficient}
%% otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec multiset_subtract(Available :: [token()], Remove :: [token()]) ->
          {ok, [token()]} | {error, insufficient}.

multiset_subtract(Available, Remove) ->
    %% Count available tokens
    AvailableCounts = count_tokens(Available),
    RemoveCounts = count_tokens(Remove),
    %% Check if we have enough of each token
    case can_consume(AvailableCounts, RemoveCounts) of
        true ->
            RemainingCounts = subtract_counts(AvailableCounts, RemoveCounts),
            {ok, expand_counts(RemainingCounts)};
        false ->
            {error, insufficient}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Counts tokens in a list (multiset frequency map).
%%
%% @end
%%--------------------------------------------------------------------
count_tokens(Tokens) ->
    lists:foldl(fun(T, Acc) ->
        maps:update_with(T, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Tokens).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if Remove counts can be satisfied by Available counts.
%%
%% @end
%%--------------------------------------------------------------------
can_consume(AvailableCounts, RemoveCounts) ->
    maps:fold(fun(Token, Count, _) ->
        case maps:get(Token, AvailableCounts, 0) of
            AvailableCount when AvailableCount >= Count -> true;
            _ -> false
        end
    end, true, RemoveCounts).

%%--------------------------------------------------------------------
%% @private
%% @doc Subtracts RemoveCounts from AvailableCounts.
%%
%% Precondition: can_consume returned true.
%% @end
%%--------------------------------------------------------------------
subtract_counts(AvailableCounts, RemoveCounts) ->
    %% Start with AvailableCounts and subtract RemoveCounts
    maps:fold(fun(Token, RemoveCount, Acc) ->
        AvailableCount = maps:get(Token, AvailableCounts, 0),
        NewCount = AvailableCount - RemoveCount,
        case NewCount of
            0 -> maps:remove(Token, Acc);
            _ -> Acc#{Token => NewCount}
        end
    end, AvailableCounts, RemoveCounts).

%%--------------------------------------------------------------------
%% @private
%% @doc Expands a frequency map back to a token list.
%%
%% @end
%%--------------------------------------------------------------------
expand_counts(Counts) ->
    maps:fold(fun(_Token, 0, Acc) -> Acc;
                 (Token, N, Acc) -> lists:duplicate(N, Token) ++ Acc
              end, [], Counts).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

new_test() ->
    M = new([p1, p2]),
    ?assertEqual([], maps:get(p1, M)),
    ?assertEqual([], maps:get(p2, M)).

get_test() ->
    M0 = new([p1]),
    ?assertEqual({ok, []}, get(M0, p1)),
    ?assertEqual({ok, []}, get(M0, missing_place)),
    M1 = set(M0, p1, [a, b]),
    ?assertEqual({ok, [a, b]}, get(M1, p1)).

set_test() ->
    M0 = new([p1, p2]),
    M1 = set(M0, p1, [a, b]),
    ?assertEqual([a, b], maps:get(p1, M1)),
    M2 = set(M1, p1, []),
    ?assertEqual([], maps:get(p1, M2)).

add_test() ->
    M0 = new([p1, p2]),
    M1 = set(M0, p1, [a, b]),
    M2 = add(M1, #{p1 => [b], p2 => [c]}),
    {ok, T1} = get(M2, p1),
    ?assertEqual([a, b, b], lists:sort(T1)),
    {ok, T2} = get(M2, p2),
    ?assertEqual([c], T2).

take_test() ->
    M0 = new([p1, p2]),
    M1 = add(M0, #{p1 => [a, b], p2 => [c]}),
    ?assertMatch({ok, _}, take(M1, #{p1 => [a], p2 => [c]})),
    {ok, M2} = take(M1, #{p1 => [a], p2 => [c]}),
    {ok, T1} = get(M2, p1),
    ?assertEqual([b], lists:sort(T1)),
    ?assertEqual({error, insufficient}, take(M1, #{p1 => [a, a, a]})).

apply_test() ->
    M0 = new([p1, p2]),
    M1 = add(M0, #{p1 => [a, b], p2 => [c]}),
    {ok, M2} = take(M1, #{p1 => [a], p2 => [c]}),
    Move = #{mode => #{p1 => [b]}, produce => #{p2 => [d]}},
    {ok, M3} = apply(M2, Move),
    {ok, Tp1} = get(M3, p1),
    {ok, Tp2} = get(M3, p2),
    ?assertEqual([], Tp1),
    ?assertEqual([d], Tp2).

hash_test() ->
    M0 = new([p]),
    Ma = set(M0, p, [a, b]),
    Mb = set(M0, p, [b, a]),
    ?assertEqual(hash(Ma), hash(Mb)).

snapshot_test() ->
    M0 = new([p1]),
    M1 = set(M0, p1, [a, b]),
    Snap = snapshot(M1),
    ?assertEqual(M1, Snap),
    ?assertMatch({ok, [a, b]}, get(Snap, p1)).

-endif.
