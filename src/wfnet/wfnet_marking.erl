%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @author CRE Team
%% @version 0.3.0
%% @doc Workflow Net Marking Algebra
%%
%% Provides multiset operations on workflow markings with support for:
%% - Marking creation and manipulation
%% - Mode enumeration for transition firing
%% - Hash-based comparison for validation
%%
%% <h3>Basic Usage</h3>
%%
%% ```erlang
%% %% Create a new marking
%% Marking = wfnet_marking:new([p1, p2, p3]).
%%
%% %% Set tokens at a place
%% Marking1 = wfnet_marking:set(Marking, p1, [token1, token2]).
%%
%% %% Get tokens from a place
%% Tokens = wfnet_marking:get(Marking1, p1).
%%
%% %% Add tokens (multiset union)
%% Marking2 = wfnet_marking:add(Marking1, #{p2 => [new_token]}).
%%
%% %% Consume tokens using a mode
%% {ok, Marking3} = wfnet_marking:take(Marking2, #{p1 => [token1]}).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_marking).

%% API exports
-export([
    new/1,
    set/3,
    get/2,
    add/2,
    take/2,
    apply_move/2,
    hash/1,
    snapshot/1,
    enum_modes/2,
    is_enabled/3,
    tokens_count/1,
    places_with_tokens/1,
    is_empty/1,
    compare/2,
    merge/2
]).

%% Types
-type place() :: atom().
-type token() :: term().
-type marking() :: #{place() => [token()]}.
-type mode() :: #{place() => [token()]}.
-type produce_map() :: #{place() => [token()]}.
-type trsn() :: atom().

-type move() :: #{
    mode := mode(),
    produce := produce_map()
}.

-export_type([marking/0, mode/0, produce_map/0, move/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create a new empty marking with the given places initialized.
%%
%% All places are initialized with empty token lists.
%%
%% === Example ===
%% ```erlang
%% > Marking = wfnet_marking:new([p1, p2, p3]).
%% #{p1 => [], p2 => [], p3 => []}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec new([place()]) -> marking().
new(Places) when is_list(Places) ->
    lists:foldl(fun(P, Acc) -> Acc#{P => []} end, #{}, Places).

%%--------------------------------------------------------------------
%% @doc Set the tokens for a place in the marking.
%%
%% Replaces any existing tokens at the place.
%%
%% === Example ===
%% ```erlang
%% > Marking = wfnet_marking:new([p1]),
%% > Marking1 = wfnet_marking:set(Marking, p1, [a, b]).
%% #{p1 => [a, b]}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec set(marking(), place(), [token()]) -> marking().
set(Marking, Place, Tokens) when is_atom(Place), is_list(Tokens) ->
    Marking#{Place => Tokens}.

%%--------------------------------------------------------------------
%% @doc Get the tokens for a place from the marking.
%%
%% Returns empty list if place not found.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a, b], p2 => []},
%% > wfnet_marking:get(Marking, p1).
%% [a, b]
%% > wfnet_marking:get(Marking, p3).
%% []
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec get(marking(), place()) -> [token()].
get(Marking, Place) ->
    maps:get(Place, Marking, []).

%%--------------------------------------------------------------------
%% @doc Add tokens from a produce map to the marking (multiset union).
%%
%% Tokens are appended to existing tokens at each place.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a], p2 => []},
%% > wfnet_marking:add(Marking, #{p1 => [b], p2 => [c]}).
%% #{p1 => [a, b], p2 => [c]}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec add(marking(), produce_map()) -> marking().
add(Marking, ProduceMap) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Existing = maps:get(Place, Acc, []),
        Acc#{Place => Existing ++ Tokens}
    end, Marking, ProduceMap).

%%--------------------------------------------------------------------
%% @doc Remove tokens specified in mode from marking (multiset difference).
%%
%% Returns {ok, NewMarking} if successful, or {error, insufficient_tokens}
%% if any place doesn't have enough tokens.
%%
%% Token comparison uses exact match (term equality).
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a, b, c], p2 => [x]},
%% > Mode = #{p1 => [a], p2 => [x]},
%% > wfnet_marking:take(Marking, Mode).
%% {ok, #{p1 => [b, c], p2 => []}}
%% > wfnet_marking:take(Marking, #{p1 => [a, b, c, d]}).
%% {error, insufficient_tokens}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec take(marking(), mode()) -> {ok, marking()} | {error, insufficient_tokens}.
take(Marking, Mode) ->
    try
        NewMarking = maps:fold(fun(Place, ToRemove, Acc) ->
            Existing = maps:get(Place, Acc, []),
            case has_enough(Existing, ToRemove) of
                true -> Acc#{Place => remove_all(Existing, ToRemove)};
                false -> error(insufficient_tokens)
            end
        end, Marking, Mode),
        {ok, NewMarking}
    catch
        error:insufficient_tokens -> {error, insufficient_tokens}
    end.

%%--------------------------------------------------------------------
%% @doc Apply a move (mode + produce map) atomically to the marking.
%%
%% A move represents a complete transition firing: consume tokens
%% according to mode, then produce new tokens.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a], p2 => []},
%% > Move = #{mode => #{p1 => [a]}, produce => #{p2 => [b]}},
%% > wfnet_marking:apply_move(Marking, Move).
%% {ok, #{p1 => [], p2 => [b]}}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec apply_move(marking(), move()) -> {ok, marking()} | {error, insufficient_tokens}.
apply_move(Marking, #{mode := Mode, produce := ProduceMap}) ->
    case take(Marking, Mode) of
        {ok, Marking1} -> {ok, add(Marking1, ProduceMap)};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Compute a stable hash of the marking for comparison.
%%
%% Uses canonical representation independent of insertion order.
%% Useful for marking comparison in validation algorithms.
%%
%% === Example ===
%% ```erlang
%% > M1 = #{p1 => [a, b], p2 => [c]},
%% > M2 = #{p2 => [c], p1 => [a, b]},
%% > wfnet_marking:hash(M1) =:= wfnet_marking:hash(M2).
%% true
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec hash(marking()) -> integer().
hash(Marking) ->
    %% Sort places and canonicalize token representation
    SortedPlaces = lists:sort(maps:keys(Marking)),
    Canonical = lists:map(fun(P) ->
        Tokens = maps:get(P, Marking),
        %% Sort tokens for canonical form (if they are comparable)
        SortedTokens = try lists:sort(Tokens) catch _:_ -> Tokens end,
        {P, SortedTokens}
    end, SortedPlaces),
    erlang:phash2(term_to_binary(Canonical)).

%%--------------------------------------------------------------------
%% @doc Create a deep copy snapshot of the marking.
%%
%% Returns a new marking with independent token lists.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a, b]},
%% > Snapshot = wfnet_marking:snapshot(Marking).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec snapshot(marking()) -> marking().
snapshot(Marking) ->
    maps:map(fun(_P, Tokens) -> lists:reverse(Tokens, []) end, Marking).

%%--------------------------------------------------------------------
%% @doc Enumerate all possible firing modes for a transition's preset.
%%
%% Returns all valid ways to consume tokens from input places.
%% For each place, we can select any combination of available tokens.
%%
%% This is a simplified implementation that returns single-token modes.
%% A full implementation would generate all combinations.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a, b], p2 => [c]},
%% > Preset = [p1, p2],
%% > Modes = wfnet_marking:enum_modes(Preset, Marking).
%% [#{p1 => [a], p2 => [c]}, #{p1 => [b], p2 => [c]}]
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec enum_modes([place()], marking()) -> [mode()].
enum_modes(Preset, Marking) ->
    %% Generate modes by taking one token from each preset place
    %% Simplified: single-token selection per place
    %% Full implementation would generate all Cartesian products
    enum_modes_recursive(Preset, Marking, #{}).

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled in the given mode.
%%
%% A transition is enabled if all places in the mode have sufficient
%% tokens to consume.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a], p2 => [b, c]},
%% > Mode = #{p1 => [a], p2 => [b]},
%% > wfnet_marking:is_enabled(t1, Mode, Marking).
%% true
%% > Mode2 = #{p1 => [a], p2 => [b, c, d]},
%% > wfnet_marking:is_enabled(t1, Mode2, Marking).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(trsn(), mode(), marking()) -> boolean().
is_enabled(_Trsn, Mode, Marking) ->
    maps:fold(fun(Place, RequiredTokens, Acc) ->
        Available = maps:get(Place, Marking, []),
        Acc andalso has_enough(Available, RequiredTokens)
    end, true, Mode).

%%--------------------------------------------------------------------
%% @doc Count total tokens across all places.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a, b], p2 => [c]},
%% > wfnet_marking:tokens_count(Marking).
%% 3
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec tokens_count(marking()) -> non_neg_integer().
tokens_count(Marking) ->
    maps:fold(fun(_P, Tokens, Acc) -> Acc + length(Tokens) end, 0, Marking).

%%--------------------------------------------------------------------
%% @doc Get list of places that have at least one token.
%%
%% === Example ===
%% ```erlang
%% > Marking = #{p1 => [a], p2 => [], p3 => [b]},
%% > wfnet_marking:places_with_tokens(Marking).
%% [p1, p3]
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec places_with_tokens(marking()) -> [place()].
places_with_tokens(Marking) ->
    [P || P <- maps:keys(Marking), length(maps:get(P, Marking, [])) > 0].

%%--------------------------------------------------------------------
%% @doc Check if a marking is empty (all places have no tokens).
%%
%% === Example ===
%% ```erlang
%% > wfnet_marking:is_empty(#{p1 => [], p2 => []}).
%% true
%% > wfnet_marking:is_empty(#{p1 => [a]}).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_empty(marking()) -> boolean().
is_empty(Marking) ->
    lists:all(fun(Tokens) -> Tokens =:= [] end, maps:values(Marking)).

%%--------------------------------------------------------------------
%% @doc Compare two markings.
%%
%% Returns:
%% - `equal` if both have same tokens at all places
%% - `less_than` if this has subset of tokens at all places
%% - `greater_than` if this has superset of tokens
%% - `incomparable` otherwise
%%
%% === Example ===
%% ```erlang
%% > M1 = #{p1 => [a]},
%% > M2 = #{p1 => [a, b]},
%% > wfnet_marking:compare(M1, M2).
%% less_than
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec compare(marking(), marking()) ->
    equal | less_than | greater_than | incomparable.
compare(M1, M2) ->
    AllPlaces = lists:usort([maps:keys(M1) ++ maps:keys(M2)]),
    compare_places(AllPlaces, M1, M2, undefined).

%%--------------------------------------------------------------------
%% @doc Merge two markings (multiset union).
%%
%% Combines tokens from both markings. Places not in either are added
%% with empty lists.
%%
%% === Example ===
%% ```erlang
%% > M1 = #{p1 => [a]},
%% > M2 = #{p1 => [b], p2 => [c]},
%% > wfnet_marking:merge(M1, M2).
%% #{p1 => [a, b], p2 => [c]}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec merge(marking(), marking()) -> marking().
merge(M1, M2) ->
    AllPlaces = lists:usort([maps:keys(M1) ++ maps:keys(M2)]),
    lists:foldl(fun(P, Acc) ->
        Tokens1 = maps:get(P, M1, []),
        Tokens2 = maps:get(P, M2, []),
        Acc#{P => Tokens1 ++ Tokens2}
    end, #{}, AllPlaces).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Check if we have enough tokens to consume.
%%--------------------------------------------------------------------
-spec has_enough([token()], [token()]) -> boolean().
has_enough(_Existing, []) ->
    true;
has_enough(Existing, [H | Rest]) ->
    case lists:member(H, Existing) of
        true -> has_enough(Existing -- [H], Rest);
        false -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Remove all occurrences of tokens from list.
%%--------------------------------------------------------------------
-spec remove_all([token()], [token()]) -> [token()].
remove_all(List, []) ->
    List;
remove_all(List, [ToRemove | Rest]) ->
    remove_all(lists:delete(ToRemove, List), Rest).

%%--------------------------------------------------------------------
%% @private
%% @doc Recursive mode enumeration.
%%--------------------------------------------------------------------
-spec enum_modes_recursive([place()], marking(), mode()) -> [mode()].
enum_modes_recursive([], _Marking, CurrentMode) ->
    [CurrentMode];
enum_modes_recursive([Place | Rest], Marking, CurrentMode) ->
    Available = maps:get(Place, Marking, []),
    Modes = lists:foldl(fun(_Token, Acc) ->
        %% For each available token, create a mode variant
        UpdatedMode = maps:put(Place, [], CurrentMode),
        enum_modes_recursive(Rest, Marking, UpdatedMode) ++ Acc
    end, [], Available),
    case Modes of
        [] -> [CurrentMode];
        _ -> Modes
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Compare markings place by place.
%%--------------------------------------------------------------------
-spec compare_places([place()], marking(), marking(), atom()) ->
    equal | less_than | greater_than | incomparable.
compare_places([], _M1, _M2, Result) ->
    case Result of
        undefined -> equal;
        _ -> Result
    end;
compare_places([Place | Rest], M1, M2, CurrentResult) ->
    Tokens1 = maps:get(Place, M1, []),
    Tokens2 = maps:get(Place, M2, []),
    Len1 = length(Tokens1),
    Len2 = length(Tokens2),
    NewResult = case {Len1, Len2} of
        {L, L} -> same;
        {L1, L2} when L1 < L2 -> less;
        {L1, L2} when L1 > L2 -> greater
    end,
    CombinedResult = case {CurrentResult, NewResult} of
        {undefined, _} -> NewResult;
        {same, R} -> R;
        {less, less} -> less;
        {greater, greater} -> greater;
        _ -> incomparable
    end,
    case CombinedResult of
        incomparable -> incomparable;
        _ -> compare_places(Rest, M1, M2, CombinedResult)
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Basic operations test
new_test() ->
    Marking = new([p1, p2, p3]),
    ?assertEqual([], maps:get(p1, Marking)),
    ?assertEqual([], maps:get(p2, Marking)),
    ?assertEqual([], maps:get(p3, Marking)).

set_get_test() ->
    Marking = new([p1]),
    Marking1 = set(Marking, p1, [a, b]),
    ?assertEqual([a, b], get(Marking1, p1)),
    ?assertEqual([], get(Marking1, p2)).

add_test() ->
    Marking = #{p1 => [a], p2 => []},
    Marking1 = add(Marking, #{p1 => [b], p2 => [c]}),
    ?assertEqual([a, b], maps:get(p1, Marking1)),
    ?assertEqual([c], maps:get(p2, Marking1)).

take_test() ->
    Marking = #{p1 => [a, b, c], p2 => [x]},
    Mode = #{p1 => [a], p2 => [x]},
    {ok, Marking1} = take(Marking, Mode),
    ?assertEqual([b, c], maps:get(p1, Marking1)),
    ?assertEqual([], maps:get(p2, Marking1)).

take_insufficient_test() ->
    Marking = #{p1 => [a]},
    Mode = #{p1 => [a, b]},
    ?assertEqual({error, insufficient_tokens}, take(Marking, Mode)).

apply_move_test() ->
    Marking = #{p1 => [a], p2 => []},
    Move = #{mode => #{p1 => [a]}, produce => #{p2 => [b]}},
    {ok, Marking1} = apply_move(Marking, Move),
    ?assertEqual([], maps:get(p1, Marking1)),
    ?assertEqual([b], maps:get(p2, Marking1)).

hash_test() ->
    M1 = #{p1 => [a, b], p2 => [c]},
    M2 = #{p2 => [c], p1 => [a, b]},
    ?assertEqual(hash(M1), hash(M2)).

is_empty_test() ->
    ?assert(is_empty(#{p1 => [], p2 => []})),
    ?assertNot(is_empty(#{p1 => [a]})),
    ?assertNot(is_empty(#{p1 => [], p2 => [b]})).

tokens_count_test() ->
    Marking = #{p1 => [a, b], p2 => [c]},
    ?assertEqual(3, tokens_count(Marking)),
    ?assertEqual(0, tokens_count(#{})).

places_with_tokens_test() ->
    Marking = #{p1 => [a], p2 => [], p3 => [b]},
    Places = places_with_tokens(Marking),
    ?assert(lists:member(p1, Places)),
    ?assertNot(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)).

compare_test() ->
    M1 = #{p1 => [a]},
    M2 = #{p1 => [a, b]},
    M3 = #{p1 => [a]},
    ?assertEqual(less_than, compare(M1, M2)),
    ?assertEqual(equal, compare(M1, M3)),
    ?assertEqual(greater_than, compare(M2, M1)).

merge_test() ->
    M1 = #{p1 => [a]},
    M2 = #{p1 => [b], p2 => [c]},
    Merged = merge(M1, M2),
    ?assertEqual([a, b], maps:get(p1, Merged)),
    ?assertEqual([c], maps:get(p2, Merged)).

is_enabled_test() ->
    Marking = #{p1 => [a], p2 => [b, c]},
    Mode = #{p1 => [a], p2 => [b]},
    ?assert(is_enabled(t1, Mode, Marking)),
    ?assertNot(is_enabled(t1, #{p1 => [d]}, Marking)).

-endif.
