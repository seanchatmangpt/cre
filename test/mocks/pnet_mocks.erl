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
%% @doc Petri Net Mock Factories
%%
%% This module provides mock factories for Petri net structures
%% for testing gen_pnet and gen_yawl workflow patterns.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Mock Petri net structures with places and transitions</li>
%%   <li>Mock markings with tokens</li>
%%   <li>Mock preset/postset maps</li>
%%   <li>Net state mock factories</li>
%%   <li>Meck-compatible for easy mocking</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% Create a mock Petri net:
%% ```erlang
%% > Net = pnet_mocks:mock_net().
%% #{places => [p1, p2], transitions => [t1], ...}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(pnet_mocks).

%%====================================================================
%% Exports
%%====================================================================

%% Petri net generators
-export([mock_net/0, mock_net/1]).
-export([simple_net/0, parallel_net/0, loop_net/0]).
-export([empty_net/0]).

%% Marking generators
-export([mock_marking/1]).
-export([initial_marking/0, initial_marking/1]).
-export([marking_with_tokens/2]).

%% Place and transition generators
-export([mock_places/0, mock_places/1]).
-export([mock_transitions/0, mock_transitions/1]).

%% Preset/postset generators
-export([mock_preset/1, mock_postset/1]).
-export([preset_map/1, postset_map/1]).

%% Net state generators
-export([mock_net_state/0, mock_net_state/1]).
-export([net_state_with_marking/1]).
-export([net_state_with_usr_info/1]).

%% Utility functions
-export([is_valid_net/1]).
-export([net_to_proplist/1]).
-export([proplist_to_net/1]).

%%====================================================================
%% Types
%%====================================================================

-type place() :: atom().
-type transition() :: atom().
-type token() :: term().
-type marking() :: #{place() => [token()]}.
-type preset() :: #{transition() => [place()]}.
-type postset() :: #{transition() => [place()]}.

-type mock_net() :: #{
    places => [place()],
    transitions => [transition()],
    preset => preset(),
    postset => postset()
}.

-type net_state() :: #{
    net_mod => atom(),
    usr_info => term(),
    marking => marking(),
    stats => term(),
    tstart => integer(),
    cnt => non_neg_integer()
}.

-export_type([place/0, transition/0, token/0, marking/0,
             preset/0, postset/0, mock_net/0, net_state/0]).

%%====================================================================
%% Petri Net Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a mock Petri net with default settings.
%%
%% Creates a simple net with 2 places and 1 transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_net() -> mock_net().

mock_net() ->
    mock_net([]).

%%--------------------------------------------------------------------
%% @doc Generates a mock Petri net with options.
%%
%% Options:
%% - `{places, [atom()]}` - Custom place list
%% - `{transitions, [atom()]}` - Custom transition list
%% - `{preset, map()}` - Custom preset map
%% - `{postset, map()}` - Custom postset map
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_net([{atom(), term()}]) -> mock_net().

mock_net(Options) ->
    Places = proplists:get_value(places, Options,
                                 [p_start, p_end]),
    Transitions = proplists:get_value(transitions, Options,
                                      [t_main]),
    Preset = proplists:get_value(preset, Options,
                                 preset_map(Transitions)),
    Postset = proplists:get_value(postset, Options,
                                  postset_map(Transitions)),

    #{
        places => Places,
        transitions => Transitions,
        preset => Preset,
        postset => Postset
    }.

%%--------------------------------------------------------------------
%% @doc Generates a simple Petri net.
%%
%% Linear structure: p_start -> t1 -> p_end
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_net() -> mock_net().

simple_net() ->
    #{
        places => [p_start, p_end],
        transitions => [t1],
        preset => #{t1 => [p_start]},
        postset => #{t1 => [p_end]}
    }.

%%--------------------------------------------------------------------
%% @doc Generates a parallel Petri net.
%%
%% Parallel split: p_start -> t_split -> {p1, p2} -> {t_join1, t_join2} -> p_end
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel_net() -> mock_net().

parallel_net() ->
    #{
        places => [p_start, p_branch1, p_branch2, p_end],
        transitions => [t_split, t_join1, t_join2],
        preset => #{
            t_split => [p_start],
            t_join1 => [p_branch1],
            t_join2 => [p_branch2]
        },
        postset => #{
            t_split => [p_branch1, p_branch2],
            t_join1 => [p_end],
            t_join2 => [p_end]
        }
    }.

%%--------------------------------------------------------------------
%% @doc Generates a loop Petri net.
%%
%% Loop structure: p_start -> t1 -> p_loop -> t2 -> p_end
%%                                           ^
%%                                           |
%%                                        t_back
%%
%% @end
%%--------------------------------------------------------------------
-spec loop_net() -> mock_net().

loop_net() ->
    #{
        places => [p_start, p_loop, p_end],
        transitions => [t_forward, t_back, t_exit],
        preset => #{
            t_forward => [p_start],
            t_back => [p_loop],
            t_exit => [p_loop]
        },
        postset => #{
            t_forward => [p_loop],
            t_back => [p_loop],
            t_exit => [p_end]
        }
    }.

%%--------------------------------------------------------------------
%% @doc Generates an empty Petri net.
%%
%% @end
%%--------------------------------------------------------------------
-spec empty_net() -> mock_net().

empty_net() ->
    #{
        places => [],
        transitions => [],
        preset => #{},
        postset => #{}
    }.

%%====================================================================
%% Marking Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a mock marking for a list of places.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_marking([place()]) -> marking().

mock_marking(Places) ->
    maps:from_list([{P, []} || P <- Places]).

%%--------------------------------------------------------------------
%% @doc Generates an initial marking (all places empty).
%%
%% @end
%%--------------------------------------------------------------------
-spec initial_marking() -> marking().

initial_marking() ->
    initial_marking([p_start, p_end]).

%%--------------------------------------------------------------------
%% @doc Generates an initial marking for specified places.
%%
%% @end
%%--------------------------------------------------------------------
-spec initial_marking([place()]) -> marking().

initial_marking(Places) ->
    maps:from_list([{P, []} || P <- Places]).

%%--------------------------------------------------------------------
%% @doc Generates a marking with tokens on specified places.
%%
%% TokensMap is a map of Place => TokenCount.
%%
%% @end
%%--------------------------------------------------------------------
-spec marking_with_tokens(Places :: [place()], TokensMap :: #{place() => pos_integer()}) ->
          marking().

marking_with_tokens(Places, TokensMap) ->
    maps:from_list(lists:map(fun(P) ->
        TokenCount = maps:get(P, TokensMap, 0),
        Tokens = if
            TokenCount > 0 -> lists:duplicate(TokenCount, token);
            true -> []
        end,
        {P, Tokens}
    end, Places)).

%%====================================================================
%% Place and Transition Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates mock places with default count.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_places() -> [place()].

mock_places() ->
    mock_places(3).

%%--------------------------------------------------------------------
%% @doc Generates mock places with specified count.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_places(pos_integer()) -> [place()].

mock_places(Count) ->
    [list_to_atom("p_" ++ integer_to_list(I)) || I <- lists:seq(1, Count)].

%%--------------------------------------------------------------------
%% @doc Generates mock transitions with default count.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_transitions() -> [transition()].

mock_transitions() ->
    mock_transitions(2).

%%--------------------------------------------------------------------
%% @doc Generates mock transitions with specified count.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_transitions(pos_integer()) -> [transition()].

mock_transitions(Count) ->
    [list_to_atom("t_" ++ integer_to_list(I)) || I <- lists:seq(1, Count)].

%%====================================================================
%% Preset/Postset Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates mock preset for a list of transitions.
%%
%% Each transition gets the first place as input.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_preset([transition()]) -> preset().

mock_preset(Transitions) ->
    preset_map(Transitions).

%%--------------------------------------------------------------------
%% @doc Generates mock postset for a list of transitions.
%%
%% Each transition gets the last place as output.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_postset([transition()]) -> postset().

mock_postset(Transitions) ->
    postset_map(Transitions).

%%--------------------------------------------------------------------
%% @doc Creates a preset map for transitions.
%%
%% @end
%%--------------------------------------------------------------------
-spec preset_map([transition()]) -> preset().

preset_map(Transitions) ->
    maps:from_list([{T, [p_start]} || T <- Transitions]).

%%--------------------------------------------------------------------
%% @doc Creates a postset map for transitions.
%%
%% @end
%%--------------------------------------------------------------------
-spec postset_map([transition()]) -> postset().

postset_map(Transitions) ->
    maps:from_list([{T, [p_end]} || T <- Transitions]).

%%====================================================================
%% Net State Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a mock net state with default values.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_net_state() -> net_state().

mock_net_state() ->
    mock_net_state([]).

%%--------------------------------------------------------------------
%% @doc Generates a mock net state with options.
%%
%% Options:
%% - `{marking, map()}` - Custom marking
%% - `{usr_info, term()}` - Custom user info
%% - `{net_mod, atom()}` - Net module name
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_net_state([{atom(), term()}]) -> net_state().

mock_net_state(Options) ->
    Marking = proplists:get_value(marking, Options,
                                  initial_marking()),
    UsrInfo = proplists:get_value(usr_info, Options, #{}),
    NetMod = proplists:get_value(net_mod, Options, mock_net_mod),

    #{
        net_mod => NetMod,
        usr_info => UsrInfo,
        marking => Marking,
        stats => undefined,
        tstart => erlang:system_time(millisecond),
        cnt => 0
    }.

%%--------------------------------------------------------------------
%% @doc Generates a net state with a specific marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec net_state_with_marking(marking()) -> net_state().

net_state_with_marking(Marking) ->
    mock_net_state([{marking, Marking}]).

%%--------------------------------------------------------------------
%% @doc Generates a net state with specific user info.
%%
%% @end
%%--------------------------------------------------------------------
-spec net_state_with_usr_info(term()) -> net_state().

net_state_with_usr_info(UsrInfo) ->
    mock_net_state([{usr_info, UsrInfo}]).

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a Petri net structure.
%%
%% Checks that all referenced places exist and structure is consistent.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_net(mock_net()) -> boolean().

is_valid_net(#{places := Places, transitions := Transitions,
                preset := Preset, postset := Postset}) ->
    %% Check all preset places exist
    PresetValid = lists:all(fun(P) ->
        lists:member(P, Places)
    end, lists:flatten(maps:values(Preset))),

    %% Check all postset places exist
    PostsetValid = lists:all(fun(P) ->
        lists:member(P, Places)
    end, lists:flatten(maps:values(Postset))),

    %% Check all preset transitions exist
    PresetTransValid = lists:all(fun(T) ->
        lists:member(T, Transitions)
    end, maps:keys(Preset)),

    %% Check all postset transitions exist
    PostsetTransValid = lists:all(fun(T) ->
        lists:member(T, Transitions)
    end, maps:keys(Postset)),

    PresetValid andalso PostsetValid andalso
        PresetTransValid andalso PostsetTransValid;
is_valid_net(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Converts a Petri net to a proplist.
%%
%% @end
%%--------------------------------------------------------------------
-spec net_to_proplist(mock_net()) -> [{atom(), term()}].

net_to_proplist(Net) ->
    maps:to_list(Net).

%%--------------------------------------------------------------------
%% @doc Converts a proplist to a Petri net.
%%
%% @end
%%--------------------------------------------------------------------
-spec proplist_to_net([{atom(), term()}]) -> mock_net().

proplist_to_net(PropList) ->
    maps:from_list(PropList).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test mock_net/0
mock_net_test() ->
    Net = mock_net(),
    ?assert(is_map(Net)),
    ?assert(lists:all(fun is_atom/1, maps:get(places, Net))),
    ?assert(lists:all(fun is_atom/1, maps:get(transitions, Net))).

%% Test mock_net/1 with options
mock_net_options_test() ->
    Net = mock_net([{places, [p1, p2, p3]}]),
    ?assertEqual([p1, p2, p3], maps:get(places, Net)).

%% Test simple_net/0
simple_net_test() ->
    Net = simple_net(),
    ?assertEqual([p_start, p_end], maps:get(places, Net)),
    ?assertEqual([t1], maps:get(transitions, Net)),
    ?assert(is_valid_net(Net)).

%% Test parallel_net/0
parallel_net_test() ->
    Net = parallel_net(),
    ?assert(length(maps:get(places, Net)) > 2),
    ?assert(length(maps:get(transitions, Net)) > 1),
    ?assert(is_valid_net(Net)).

%% Test loop_net/0
loop_net_test() ->
    Net = loop_net(),
    ?assert(is_valid_net(Net)).

%% Test empty_net/0
empty_net_test() ->
    Net = empty_net(),
    ?assertEqual([], maps:get(places, Net)),
    ?assertEqual([], maps:get(transitions, Net)).

%% Test mock_marking/1
mock_marking_test() ->
    Marking = mock_marking([p1, p2]),
    ?assertEqual([], maps:get(p1, Marking)),
    ?assertEqual([], maps:get(p2, Marking)).

%% Test initial_marking/0
initial_marking_test() ->
    Marking = initial_marking(),
    ?assert(is_map(Marking)),
    ?assertEqual([], maps:get(p_start, Marking)).

%% Test marking_with_tokens/2
marking_with_tokens_test() ->
    Marking = marking_with_tokens([p1, p2], #{p1 => 2, p2 => 0}),
    ?assertEqual([token, token], maps:get(p1, Marking)),
    ?assertEqual([], maps:get(p2, Marking)).

%% Test mock_places/0
mock_places_test() ->
    Places = mock_places(),
    ?assertEqual(3, length(Places)).

%% Test mock_places/1
mock_places_count_test() ->
    Places = mock_places(5),
    ?assertEqual(5, length(Places)),
    ?assert(lists:all(fun(P) -> is_atom(P) end, Places)).

%% Test mock_transitions/0
mock_transitions_test() ->
    Transitions = mock_transitions(),
    ?assertEqual(2, length(Transitions)).

%% Test mock_transitions/1
mock_transitions_count_test() ->
    Transitions = mock_transitions(4),
    ?assertEqual(4, length(Transitions)).

%% Test preset_map/1
preset_map_test() ->
    Preset = preset_map([t1, t2]),
    ?assertEqual([p_start], maps:get(t1, Preset)),
    ?assertEqual([p_start], maps:get(t2, Preset)).

%% Test postset_map/1
postset_map_test() ->
    Postset = postset_map([t1, t2]),
    ?assertEqual([p_end], maps:get(t1, Postset)),
    ?assertEqual([p_end], maps:get(t2, Postset)).

%% Test mock_net_state/0
mock_net_state_test() ->
    State = mock_net_state(),
    ?assert(is_map(State)),
    ?assert(is_atom(maps:get(net_mod, State))),
    ?assert(is_map(maps:get(marking, State))).

%% Test mock_net_state/1 with options
mock_net_state_options_test() ->
    State = mock_net_state([{usr_info, test_info}]),
    ?assertEqual(test_info, maps:get(usr_info, State)).

%% Test net_state_with_marking/1
net_state_with_marking_test() ->
    Marking = #{p1 => [a], p2 => []},
    State = net_state_with_marking(Marking),
    ?assertEqual(Marking, maps:get(marking, State)).

%% Test net_state_with_usr_info/1
net_state_with_usr_info_test() ->
    State = net_state_with_usr_info(#{key => val}),
    ?assertEqual(#{key => val}, maps:get(usr_info, State)).

%% Test is_valid_net/1
is_valid_net_test() ->
    ?assert(is_valid_net(simple_net())),
    ?assert(is_valid_net(parallel_net())),
    ?assert(is_valid_net(loop_net())),
    ?assertNot(is_valid_net(#{
        places => [p1], transitions => [t1],
        preset => #{t1 => [nonexistent]},
        postset => #{t1 => [p1]}
    })).

%% Test net_to_proplist/1
net_to_proplist_test() ->
    Net = simple_net(),
    PropList = net_to_proplist(Net),
    ?assert(is_list(PropList)),
    ?assert(length(PropList) >= 4).

%% Test proplist_to_net/1
proplist_to_net_test() ->
    PropList = [{places, [p1, p2]}, {transitions, [t1]}],
    Net = proplist_to_net(PropList),
    ?assertEqual([p1, p2], maps:get(places, Net)).

-endif.
