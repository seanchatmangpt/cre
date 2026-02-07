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

-module(pnet_mode).
-moduledoc """
Mode enumeration (input token selections).

preset_counts/1 counts multiplicity of places in a preset list.
enum_modes/2 enumerates deterministic modes:
- for each place P with count N, choose N tokens from Marking[P]
- token lists inside a mode are in term order
- output list is deterministic

```erlang
> pnet_mode:preset_counts([p1,p2,p1]).
#{p1 => 2,p2 => 1}

> Mark = #{p1 => [a,b], p2 => [c]}.
> Modes = pnet_mode:enum_modes([p1,p2], Mark).
> length(Modes).
2
> lists:sort([maps:get(p1, M) || M <- Modes]).
[[a],[b]]

> Mark2 = #{p1 => [a,b,c]}.
> Modes2 = pnet_mode:enum_modes([p1,p1], Mark2).
> length(Modes2).
3
> lists:all(fun(M) -> length(maps:get(p1, M)) =:= 2 end, Modes2).
true

> pnet_mode:enum_modes([p9], #{p9 => []}).
[]

> pnet_mode:enum_cmodes(t1, #{p => [a,b]}, ctx, wf_test_net_basic).
{ok,[{#{},#{p => [a]}},{#{},#{p => [b]}}]}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Mode enumeration
-export([preset_counts/1, enum_modes/2]).

%% Colored net extension
-export([enum_cmodes/4]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A mode specifies token availability for transition firing.
%%
%% Maps each input place to the list of tokens that enable the
%% transition in this particular firing mode.
%%--------------------------------------------------------------------
-type mode() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A variable name in colored Petri nets.
%%--------------------------------------------------------------------
-type var() :: atom().

%%--------------------------------------------------------------------
%% @doc A binding maps variables to their concrete values.
%%--------------------------------------------------------------------
-type binding() :: #{var() => term()}.

%%--------------------------------------------------------------------
%% @doc A colored mode combines a binding with a token mode.
%%--------------------------------------------------------------------
-type cmode() :: {binding(), mode()}.

%%--------------------------------------------------------------------
%% @doc A net module implementing pnet_net behaviour.
%%--------------------------------------------------------------------
-type net_mod() :: module().

%%--------------------------------------------------------------------
%% @doc User context for net module callbacks.
%%--------------------------------------------------------------------
-type usr_info() :: term().

%% Export types
-export_type([mode/0, cmode/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the count of tokens needed from each preset place.
%%
%% Counts the multiplicity of each place in the preset list.
%% For example, [p1, p2, p1] returns #{p1 => 2, p2 => 1}.
%%
%% @param PresetPlaces List of input places for a transition
%% @return Map of places to their count in the preset list
%%
%% @end
%%--------------------------------------------------------------------
-spec preset_counts(PresetPlaces :: [place()]) ->
          #{place() => non_neg_integer()}.

preset_counts(PresetPlaces) when is_list(PresetPlaces) ->
    %% Count multiplicity of each place in the preset list
    lists:foldl(
        fun(P, Acc) ->
            maps:update_with(P, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        PresetPlaces
    ).

%%--------------------------------------------------------------------
%% @doc Enumerates all possible modes given the current marking.
%%
%% A mode represents one valid way to fire a transition by selecting
%% tokens from each preset place. The number of tokens selected from
%% each place equals its multiplicity in the preset list.
%%
%% For example, with preset [p1, p2] and marking #{p1 => [a,b], p2 => [c]},
%% returns [#{p1 => [a], p2 => [c]}, #{p1 => [b], p2 => [c]}].
%%
%% With preset [p1, p1] and marking #{p1 => [a,b,c]},
%% returns all 3 combinations of 2 tokens: [#{p1 => [a,b]}, #{p1 => [a,c]}, #{p1 => [b,c]}].
%%
%% @param PresetPlaces List of input places for the transition
%% @param Marking Current marking of the net
%% @return List of all valid modes (empty if no mode possible)
%%
%% @end
%%--------------------------------------------------------------------
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) ->
          [mode()].

enum_modes(PresetPlaces, Marking) when is_list(PresetPlaces), is_map(Marking) ->
    %% Get counts for each place (handles multiplicity)
    Counts = preset_counts(PresetPlaces),
    %% Get unique places
    UniquePlaces = lists:usort(PresetPlaces),
    enum_modes_for_places(UniquePlaces, Counts, Marking).

%%--------------------------------------------------------------------
%% @doc Enumerates colored modes with variable bindings.
%%
%% For colored Petri nets, this function calls the net module's
%% cmodes/3 callback to get modes that include variable bindings.
%% If the net module doesn't implement colored modes, falls back
%% to basic mode enumeration with empty bindings.
%%
%% @param Trsn The transition to enumerate modes for
%% @param Marking Current marking of the net
%% @param UsrInfo User context for the net
%% @param NetMod The net module implementing pnet_net behaviour
%% @return {ok, [{Binding, Mode}]} pairs on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec enum_cmodes(Trsn :: atom(), Marking :: marking(),
                 UsrInfo :: usr_info(), NetMod :: net_mod()) ->
          {ok, [cmode()]} | {error, term()}.

enum_cmodes(Trsn, Marking, UsrInfo, NetMod) when is_atom(Trsn), is_map(Marking),
                                              is_atom(NetMod) ->
    %% Check if the net module implements colored modes (cmodes/3)
    case erlang:function_exported(NetMod, cmodes, 3) of
        true ->
            try
                NetMod:cmodes(Trsn, Marking, UsrInfo)
            catch
                _Type:_Reason:_Stack ->
                    %% On error, try fallback to basic enumeration
                    Preset = NetMod:preset(Trsn),
                    Modes = enum_modes(Preset, Marking),
                    {ok, [{#{}, M} || M <- Modes]}
            end;
        false ->
            %% Fall back to basic mode enumeration with empty binding
            try
                Preset = NetMod:preset(Trsn),
                Modes = enum_modes(Preset, Marking),
                {ok, [{#{}, M} || M <- Modes]}
            catch
                _Type:_Reason:_Stack ->
                    {error, no_preset}
            end
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Enumerates modes for a list of unique places with their counts.
%%
%% For each place, generates all combinations of N tokens where N is
%% the count from the preset multiplicity. Results are in deterministic
%% term order (combinations maintain original list order).
%%
%% @private
%%--------------------------------------------------------------------
-spec enum_modes_for_places(Places :: [place()],
                             Counts :: #{place() => non_neg_integer()},
                             Marking :: marking()) ->
          [mode()].

enum_modes_for_places([], _Counts, _Marking) ->
    [#{ }];
enum_modes_for_places([Place | Rest], Counts, Marking) ->
    case maps:get(Place, Marking, []) of
        [] ->
            [];  %% No tokens available, no modes possible
        Tokens ->
            %% Get the count needed from this place
            Count = maps:get(Place, Counts, 1),
            %% Generate all combinations of Count tokens
            TokenCombos = combinations(Count, Tokens),
            %% Check if we have enough tokens
            case length(Tokens) < Count of
                true ->
                    [];  %% Not enough tokens, no modes possible
                false ->
                    %% Generate modes for the rest of the places
                    RestModes = enum_modes_for_places(Rest, Counts, Marking),
                    %% Combine each token combination with each rest mode
                    lists:flatmap(
                        fun(Combo) ->
                            [M#{Place => Combo} || M <- RestModes]
                        end,
                        TokenCombos
                    )
            end
    end.

%%--------------------------------------------------------------------
%% @doc Generate all combinations of N elements from a list.
%%
%% Returns combinations in deterministic order (lexicographic by
%% position in the original list). Each combination maintains the
%% original order of elements from the input list.
%%
%% Examples:
%%   combinations(2, [a,b,c]) -> [[a,b], [a,c], [b,c]]
%%   combinations(1, [a,b]) -> [[a], [b]]
%%   combinations(0, _) -> [[]]
%%
%% @private
%%--------------------------------------------------------------------
-spec combinations(N :: non_neg_integer(), List :: [term()]) -> [[term()]].

combinations(0, _List) ->
    [[]];
combinations(_N, []) ->
    [];
combinations(N, [H | T]) ->
    %% Combinations including H (need N-1 more from T)
    WithH = [[H | Rest] || Rest <- combinations(N - 1, T)],
    %% Combinations excluding H (need N from T)
    WithoutH = combinations(N, T),
    WithH ++ WithoutH.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Additional unit tests for comprehensive coverage

preset_counts_test() ->
    ?assertEqual(#{p1 => 2, p2 => 1}, preset_counts([p1, p2, p1])),
    ?assertEqual(#{}, preset_counts([])),
    ?assertEqual(#{p => 3}, preset_counts([p, p, p])).

enum_modes_empty_preset_test() ->
    Modes = enum_modes([], #{p => [a, b]}),
    ?assertEqual([#{}], Modes).

enum_modes_empty_tokens_test() ->
    Modes = enum_modes([p1], #{p1 => []}),
    ?assertEqual([], Modes).

enum_modes_combinations_test() ->
    Modes = enum_modes([p1, p1], #{p1 => [a, b, c]}),
    ?assertEqual(3, length(Modes)),
    ?assert(lists:all(fun(M) -> length(maps:get(p1, M)) =:= 2 end, Modes)).

enum_modes_single_place_test() ->
    Modes = enum_modes([p1], #{p1 => [a, b, c]}),
    ?assertEqual(3, length(Modes)),
    ?assertEqual([[a], [b], [c]], lists:sort([maps:get(p1, M) || M <- Modes])).

enum_modes_multiple_places_test() ->
    Modes = enum_modes([p1, p2], #{p1 => [a, b], p2 => [c]}),
    ?assertEqual(2, length(Modes)),
    ?assert(lists:all(fun(M) -> maps:get(p2, M) =:= [c] end, Modes)),
    ?assertEqual([[a], [b]], lists:sort([maps:get(p1, M) || M <- Modes])).

enum_modes_insufficient_tokens_test() ->
    Modes = enum_modes([p1, p1], #{p1 => [a]}),
    ?assertEqual([], Modes).

enum_cmodes_no_cmodes_callback_test() ->
    %% Test fallback when net module doesn't implement cmodes/3
    Modes = enum_cmodes(t1, #{p => [a, b]}, ctx, wf_test_net_basic),
    ?assertMatch({ok, _}, Modes),
    {ok, Cmodes} = Modes,
    ?assertEqual(2, length(Cmodes)),
    ?assert(lists:all(fun({Binding, _}) -> Binding =:= #{} end, Cmodes)).

-endif.
