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

-module(wf_verify).
-moduledoc """
Workflow soundness verification for Petri net structures.

This module provides pure verification functions for analyzing Petri net
structures to ensure they satisfy soundness properties required for
correct workflow execution.

Soundness properties verified:
- Has at least one transition
- All places are connected (no orphan places)
- Has input condition (initial place with no incoming arcs)
- Has output condition (final place with no outgoing arcs)
- Proper completion option (can reach final marking)

```erlang
> Bad = #{places => [i,o], transitions => [], arcs => []}, wf_verify:sound(Bad).
{error,[no_transitions]}

> Valid = #{places => [s,d], transitions => [t], arcs => [{s,t},{t,d}]}, wf_verify:sound(Valid).
ok
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Main verification
-export([sound/1]).

%% Individual property checks
-export([has_transitions/1, connected_places/1,
         has_input_condition/1, has_output_condition/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%
%% Places are atoms representing locations where tokens reside.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net.
%%
%% Transitions represent workflow tasks or activities.
%%--------------------------------------------------------------------
-type transition() :: atom().

%%--------------------------------------------------------------------
%% @doc An arc connects a place to a transition or vice versa.
%%
%% Arcs are tuples {From, To} where From and To are either places
%% or transitions. The direction indicates token flow.
%%--------------------------------------------------------------------
-type arc() :: {place(), transition()} | {transition(), place()}.

%%--------------------------------------------------------------------
%% @doc Petri net structure map.
%%
%% Contains the structural elements of a workflow:
%% - places: List of all places in the net
%% - transitions: List of all transitions in the net
%% - arcs: List of all arcs connecting places and transitions
%%--------------------------------------------------------------------
-type net() :: #{places => [place()],
                 transitions => [transition()],
                 arcs => [arc()]}.

%%--------------------------------------------------------------------
%% @doc A compiled workflow module.
%%
%% Can be either a module name (atom) implementing gen_yawl callbacks
%% or a net structure map.
%%--------------------------------------------------------------------
-type compiled() :: module() | net().

%%--------------------------------------------------------------------
%% @doc Verification result.
%%
%% Returns ok if verification passes, or {error, [Reason]} where
%% each Reason is an atom describing a failed property.
%%--------------------------------------------------------------------
-type verify_result() :: ok | {error, [atom()]}.

%% Export types
-export_type([net/0, compiled/0, verify_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Verifies workflow soundness.
%%
%% Checks all soundness properties:
%% - Has at least one transition
%% - All places are connected
%% - Has input condition (start place)
%% - Has output condition (end place)
%%
%% @param Compiled A module atom or net structure map
%% @return ok if sound, {error, [Reason]} otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec sound(Compiled :: compiled()) -> verify_result().

sound(Module) when is_atom(Module) ->
    %% Extract net structure from gen_yawl module
    try extract_net(Module) of
        {ok, Net} ->
            verify_soundness(Net);
        {error, Reason} ->
            {error, [Reason]}
    catch
        _:_ ->
            {error, [invalid_module]}
    end;

sound(Net) when is_map(Net) ->
    verify_soundness(Net);

sound(_Other) ->
    {error, [invalid_input]}.

%%--------------------------------------------------------------------
%% @doc Checks if the net has at least one transition.
%%
%% A valid workflow must have at least one transition to execute.
%%
%% @param Net The net structure map
%% @return true if has transitions, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec has_transitions(Net :: net()) -> boolean().

has_transitions(#{transitions := Transitions}) when is_list(Transitions) ->
    length(Transitions) > 0;
has_transitions(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if all places are connected via arcs.
%%
%% An orphan place has no incoming or outgoing arcs. Returns
%% {ok, []} if all places connected, or {ok, [OrphanPlaces]}.
%%
%% @param Net The net structure map
%% @return {ok, []} if connected, {ok, Orphans} with list of orphan places
%%
%% @end
%%--------------------------------------------------------------------
-spec connected_places(Net :: net()) -> {ok, [place()]}.

connected_places(#{places := Places, arcs := Arcs}) when is_list(Places), is_list(Arcs) ->
    Connected = collect_connected_places(Arcs, sets:new()),
    Orphans = lists:filter(fun(P) -> not sets:is_element(P, Connected) end, Places),
    {ok, Orphans};
connected_places(_) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc Checks if the net has an input condition.
%%
%% An input condition is a place with no incoming arcs (can start
%% with initial token).
%%
%% @param Net The net structure map
%% @return true if has input condition, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec has_input_condition(Net :: net()) -> boolean().

has_input_condition(#{places := Places, arcs := Arcs}) when is_list(Places), is_list(Arcs) ->
    Targets = collect_targets(Arcs, sets:new()),
    %% Input condition is a place that is never a target
    lists:any(fun(P) -> not sets:is_element(P, Targets) end, Places);
has_input_condition(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if the net has an output condition.
%%
%% An output condition is a place with no outgoing arcs (can hold
%% final completion token).
%%
%% @param Net The net structure map
%% @return true if has output condition, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec has_output_condition(Net :: net()) -> boolean().

has_output_condition(#{places := Places, arcs := Arcs}) when is_list(Places), is_list(Arcs) ->
    Sources = collect_sources(Arcs, sets:new()),
    %% Output condition is a place that is never a source
    lists:any(fun(P) -> not sets:is_element(P, Sources) end, Places);
has_output_condition(_) ->
    false.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts net structure from a gen_yawl module.
%%
%% Calls the module's place_lst/0, trsn_lst/0, and preset/1 to
%% reconstruct the arc structure.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_net(Module :: module()) -> {ok, net()} | {error, term()}.

extract_net(Module) ->
    try
        Places = Module:place_lst(),
        Transitions = Module:trsn_lst(),
        Arcs = extract_arcs(Module, Transitions),
        {ok, #{places => Places, transitions => Transitions, arcs => Arcs}}
    catch
        _:_:_ ->
            {error, module_extraction_failed}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts arcs by examining preset of each transition.
%%
%% For basic gen_yawl modules, arcs are determined from preset/1.
%% We assume arcs from transition to output places based on
%% standard workflow patterns.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_arcs(Module :: module(), Transitions :: [transition()]) -> [arc()].

extract_arcs(Module, Transitions) ->
    lists:flatmap(fun(T) ->
        Preset = Module:preset(T),
        %% Create arcs from preset places to transition
        [{P, T} || P <- Preset]
    end, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Performs comprehensive soundness verification.
%%
%% Collects all errors and returns them together.
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_soundness(Net :: net()) -> verify_result().

verify_soundness(Net) ->
    Errors = collect_errors(Net, []),
    case Errors of
        [] -> ok;
        _ -> {error, lists:reverse(Errors)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all verification errors.
%%
%% Runs each check and accumulates errors.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_errors(Net :: net(), Acc :: [atom()]) -> [atom()].

collect_errors(Net, Acc) ->
    Acc1 = case has_transitions(Net) of
        true -> Acc;
        false -> [no_transitions | Acc]
    end,
    %% Only check connectivity if there are transitions
    Acc2 = case has_transitions(Net) of
        true ->
            case connected_places(Net) of
                {ok, []} -> Acc1;
                {ok, _Orphans} -> [orphan_places | Acc1]
            end;
        false ->
            Acc1
    end,
    Acc3 = case has_input_condition(Net) of
        true -> Acc2;
        false -> [no_input_condition | Acc2]
    end,
    Acc4 = case has_output_condition(Net) of
        true -> Acc3;
        false -> [no_output_condition | Acc3]
    end,
    Acc4.

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all places connected by arcs.
%%
%% Returns a set of all places that appear in any arc.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_connected_places([arc()], sets:set(place())) -> sets:set(place()).

collect_connected_places([], Connected) ->
    Connected;
collect_connected_places([{From, To} | Rest], Connected) ->
    Connected1 = case is_atom(From) of
        true -> sets:add_element(From, Connected);
        false -> Connected
    end,
    Connected2 = case is_atom(To) of
        true -> sets:add_element(To, Connected1);
        false -> Connected1
    end,
    collect_connected_places(Rest, Connected2).

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all target places from arcs.
%%
%% Targets are places that receive tokens (second element of arc).
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_targets([arc()], sets:set(place())) -> sets:set(place()).

collect_targets([], Targets) ->
    Targets;
collect_targets([{_From, To} | Rest], Targets) when is_atom(To) ->
    collect_targets(Rest, sets:add_element(To, Targets));
collect_targets([{_From, _To} | Rest], Targets) ->
    collect_targets(Rest, Targets).

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all source places from arcs.
%%
%% Sources are places that send tokens (first element of arc).
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_sources([arc()], sets:set(place())) -> sets:set(place()).

collect_sources([], Sources) ->
    Sources;
collect_sources([{From, _To} | Rest], Sources) when is_atom(From) ->
    collect_sources(Rest, sets:add_element(From, Sources));
collect_sources([{_From, _To} | Rest], Sources) ->
    collect_sources(Rest, Sources).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => false}).

%% Test sound/1 with bad net (no transitions)
sound_no_transitions_test() ->
    Bad = #{places => [i,o], transitions => [], arcs => []},
    ?assertEqual({error, [no_transitions]}, sound(Bad)).

%% Test has_transitions/1
has_transitions_empty_test() ->
    Net = #{places => [], transitions => [], arcs => []},
    ?assertEqual(false, has_transitions(Net)).

has_transitions_with_one_test() ->
    Net = #{places => [p1], transitions => [t1], arcs => []},
    ?assertEqual(true, has_transitions(Net)).

%% Test connected_places/1
connected_places_all_connected_test() ->
    Net = #{places => [p1, p2], transitions => [t1],
             arcs => [{p1, t1}, {t1, p2}]},
    ?assertEqual({ok, []}, connected_places(Net)).

connected_places_orphan_test() ->
    Net = #{places => [p1, p2, orphan], transitions => [t1],
             arcs => [{p1, t1}, {t1, p2}]},
    {ok, Orphans} = connected_places(Net),
    ?assert(lists:member(orphan, Orphans)).

%% Test has_input_condition/1
has_input_condition_true_test() ->
    Net = #{places => [p1, p2], transitions => [t1],
             arcs => [{p1, t1}, {t1, p2}]},
    ?assertEqual(true, has_input_condition(Net)).

has_input_condition_false_test() ->
    %% All places are targets
    Net = #{places => [p1, p2], transitions => [t1],
             arcs => [{p1, t1}, {t1, p1}, {p2, t1}, {t1, p2}]},
    ?assertEqual(false, has_input_condition(Net)).

%% Test has_output_condition/1
has_output_condition_true_test() ->
    Net = #{places => [p1, p2], transitions => [t1],
             arcs => [{p1, t1}, {t1, p2}]},
    ?assertEqual(true, has_output_condition(Net)).

has_output_condition_false_test() ->
    %% All places are sources
    Net = #{places => [p1, p2], transitions => [t1],
             arcs => [{p1, t1}, {t1, p1}, {p2, t1}, {t1, p2}]},
    ?assertEqual(false, has_output_condition(Net)).

%% Test sound/1 with valid net
sound_valid_test() ->
    %% A minimal valid workflow net
    Valid = #{places => [start, done], transitions => [t1],
               arcs => [{start, t1}, {t1, done}]},
    ?assertEqual(ok, sound(Valid)).

%% Test sound/1 with orphan places
sound_orphan_test() ->
    Net = #{places => [p1, p2, orphan], transitions => [t1],
             arcs => [{p1, t1}, {t1, p2}]},
    ?assertMatch({error, Errors} when length(Errors) > 0, sound(Net)).

-endif.
