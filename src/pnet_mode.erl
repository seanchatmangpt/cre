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
%% @doc Petri Net Mode Enumeration Module
%%
%% This module provides mode enumeration utilities for the gen_pnet
%% Petri net framework. Modes represent the different ways a transition
%% can be fired given the current marking.
%%
%% <h3>Key Concepts</h3>
%%
%% A <em>mode</em> specifies which tokens are consumed from each input
%% place when a transition fires. For uncolored Petri nets, a mode is
%% simply a selection of available tokens from each preset place.
%%
%% For colored Petri nets, a <em>cmode</em> (colored mode) extends this
%% with variable bindings that match token values against arc expressions.
%%
%% <h3>Usage Example</h3>
%% <pre><code>
%% %% Get preset counts for a transition
%% PresetPlaces = [p1, p2, p3],
%% Counts = pnet_mode:preset_counts(PresetPlaces),
%% %% => #{p1 => 1, p2 => 1, p3 => 1}
%%
%% %% Enumerate all possible modes given current marking
%% Marking = #{p1 => [a, b], p2 => [c], p3 => [d, e]},
%% Modes = pnet_mode:enum_modes(PresetPlaces, Marking),
%% %% => [#{p1 => [a], p2 => [c], p3 => [d]},
%% %%     #{p1 => [a], p2 => [c], p3 => [e]},
%% %%     #{p1 => [b], p2 => [c], p3 => [d]},
%% %%     #{p1 => [b], p2 => [c], p3 => [e]}]
%% </code></pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(pnet_mode).

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
%% For basic Petri nets, each place needs at least 1 token.
%% This function returns a map indicating the minimum requirement.
%%
%% @param PresetPlaces List of input places for a transition
%% @return Map of places to required token count (always 1 for uncolored)
%%
%% @end
%%--------------------------------------------------------------------
-spec preset_counts(PresetPlaces :: [place()]) ->
          #{place() => non_neg_integer()}.

preset_counts(PresetPlaces) when is_list(PresetPlaces) ->
    maps:from_list([{P, 1} || P <- PresetPlaces]).

%%--------------------------------------------------------------------
%% @doc Enumerates all possible modes given the current marking.
%%
%% A mode represents one valid way to fire a transition by selecting
%% tokens from each preset place. This function generates the Cartesian
%% product of available tokens across all preset places.
%%
%% For example, with preset places [p1, p2] and marking #{p1 => [a,b], p2 => [c]},
%% returns [#{p1 => [a], p2 => [c]}, #{p1 => [b], p2 => [c]}].
%%
%% @param PresetPlaces List of input places for the transition
%% @param Marking Current marking of the net
%% @return List of all valid modes
%%
%% @end
%%--------------------------------------------------------------------
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) ->
          [mode()].

enum_modes([], _Marking) ->
    [#{ }];
enum_modes([Place | Rest], Marking) when is_atom(Place), is_map(Marking) ->
    case maps:get(Place, Marking, []) of
        [] ->
            [];  %% No tokens available, no modes possible
        Tokens ->
            %% Generate modes for the rest of the places
            RestModes = enum_modes(Rest, Marking),
            %% Combine each token with each rest mode
            lists:flatmap(
                fun(Token) ->
                    [M#{Place => [Token]} || M <- RestModes]
                end,
                Tokens
            )
    end;
enum_modes(PresetPlaces, Marking) ->
    enum_modes(lists:usort(PresetPlaces), Marking).

%%--------------------------------------------------------------------
%% @doc Enumerates colored modes with variable bindings.
%%
%% For colored Petri nets, this function calls the net module's
%% cmodes callback to get modes that include variable bindings.
%% If the net module doesn't implement colored modes, falls back
%% to basic mode enumeration.
%%
%% @param Trsn The transition to enumerate modes for
%% @param Marking Current marking of the net
%% @param Ctx User context (usr_info) for the net
%% @param NetMod The net module implementing pnet_net behaviour
%% @return List of all valid colored modes
%%
%% @end
%%--------------------------------------------------------------------
-spec enum_cmodes(Trsn :: atom(), Marking :: marking(),
                 Ctx :: usr_info(), NetMod :: net_mod()) ->
          [cmode()].

enum_cmodes(Trsn, Marking, Ctx, NetMod) when is_atom(Trsn), is_map(Marking),
                                             is_atom(NetMod) ->
    %% Check if the net module implements colored modes
    case erlang:function_exported(NetMod, cmodes, 3) of
        true ->
            try
                NetMod:cmodes(Trsn, Marking, Ctx)
            catch
                _:_:_-> []
            end;
        false ->
            %% Fall back to basic mode enumeration with empty binding
            Modes = enum_modes(NetMod:preset(Trsn), Marking),
            [{#{}, M} || M <- Modes]
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% No internal functions currently - all logic is in exported functions
