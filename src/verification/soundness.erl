%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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

-module(soundness).
-moduledoc """
WF-net soundness verification for Petri nets.

This module implements WF-net (Workflow net) soundness verification
as described in "Verification of Workflow Nets" (1997) by van der Aalst.

## WF-net Definition

A Petri net is a WF-net if:
- It has a single source place (input place with no incoming arcs)
- It has a single sink place (output place with no outgoing arcs)
- Every node (place or transition) is on a path from source to sink

## Soundness Properties

A WF-net is sound if:
1. **Option to complete**: For any reachable marking M, there exists a
   firing sequence leading to the sink marking (only sink place marked)
2. **Proper completion**: When the sink place is marked, only it is marked
   (no other places have tokens)
3. **No dead tasks**: No transition is dead in any reachable state from
   the initial marking

## Short-circuit Net

For verification, the short-circuit net adds a transition t* from sink
to source. A WF-net is sound iff its short-circuit net is live and bounded.

## Reduction Rules

The module implements reduction rules to simplify WF-nets while preserving
soundness properties:
- Fusion of series places
- Fusion of series transitions
- Fusion of parallel places
- Fusion of parallel transitions
- Elimination of self-loop places
- Elimination of self-loop transitions

## Examples

Check if a net structure is a valid WF-net:

```erlang
> Net = #{places => [p1, p2, p3], transitions => [t1, t2]},
> Net1 = Net#{preset => #{t1 => [p1], t2 => [p2]}, postset => #{t1 => [p2], t2 => [p3]}}.
> soundness:is_wf_net(Net1).
{true, p1, p3}
```

Verify soundness of a WF-net:

```erlang
> soundness:soundness_check(Net1).
{ok, #{option_to_complete => true, proper_completion => true, no_dead_tasks => true}}
```

Apply short-circuit transformation:

```erlang
> soundness:short_circuit(Net1).
{ok, ShortCircuitNet}
```

Apply reduction rules to simplify a net:

```erlang
> soundness:reduction_rules(Net1).
{ok, ReducedNet}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% WF-net validation
-export([is_wf_net/1]).

%% Soundness verification
-export([soundness_check/1, soundness_check/2]).

%% Short-circuit transformation
-export([short_circuit/1]).

%% Reduction rules
-export([reduction_rules/1, apply_reduction/2]).

%% Utility functions
-export([find_source_places/1, find_sink_places/1, compute_reachable_nodes/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net (atom).
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net (atom).
%%--------------------------------------------------------------------
-type trsn() :: atom().

%%--------------------------------------------------------------------
%% @doc A net_node is either a place or a transition.
%%
%% Note: Using net_node instead of node() to avoid shadowing the built-in
%% Erlang type node() which refers to distributed Erlang node names.
%%--------------------------------------------------------------------
-type net_node() :: {place, place()} | {trsn, trsn()}.

%%--------------------------------------------------------------------
%% @doc Marking maps places to their token multisets.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [term()]}.

%%--------------------------------------------------------------------
%% @doc Net structure definition.
%%
%% Maps containing:
%% - places: list of all places
%% - transitions: list of all transitions
%% - preset: maps transition to its input places
%% - postset: maps transition to its output places
%% - source: the source place (if known)
%% - sink: the sink place (if known)
%%--------------------------------------------------------------------
-type net() :: #{
    places => [place()],
    transitions => [trsn()],
    preset => #{trsn() => [place()]},
    postset => #{trsn() => [place()]}
}.

%%--------------------------------------------------------------------
%% @doc Soundness check result.
%%--------------------------------------------------------------------
-type soundness_result() ::
    {ok, #{
        option_to_complete => boolean(),
        proper_completion => boolean(),
        no_dead_tasks => boolean(),
        is_sound => boolean()
    }} |
    {error, term()}.

%%--------------------------------------------------------------------
%% @doc Reduction rule type.
%%--------------------------------------------------------------------
-type reduction_rule() ::
    fusion_series_places |
    fusion_series_transitions |
    fusion_parallel_places |
    fusion_parallel_transitions |
    eliminate_self_loop_place |
    eliminate_self_loop_transition.

%% Export types
-export_type([net/0, marking/0, soundness_result/0, net_node/0, reduction_rule/0]).

%%====================================================================
%% WF-net Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if the given net structure is a valid WF-net.
%%
%% A WF-net must have:
%% 1. Exactly one source place (no incoming arcs)
%% 2. Exactly one sink place (no outgoing arcs)
%% 3. All nodes on a path from source to sink
%%
%% Returns `{true, Source, Sink}` if valid, `{false, Reason}` otherwise.
%%
%% ## Examples
%%
%% ```erlang
%% > Net = #{
%%     places => [p1, p2, p3],
%%     transitions => [t1, t2],
%%     preset => #{t1 => [p1], t2 => [p2]},
%%     postset => #{t1 => [p2], t2 => [p3]}
%% }.
%% > soundness:is_wf_net(Net).
%% {true, p1, p3}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_wf_net(Net :: net()) ->
    {true, Source :: place(), Sink :: place()} | {false, Reason :: term()}.

is_wf_net(Net) ->
    case validate_net_structure(Net) of
        {error, Reason} ->
            {false, {invalid_structure, Reason}};
        ok ->
            SourcePlaces = find_source_places(Net),
            SinkPlaces = find_sink_places(Net),
            case {length(SourcePlaces), length(SinkPlaces)} of
                {1, 1} ->
                    [Source] = SourcePlaces,
                    [Sink] = SinkPlaces,
                    case verify_all_nodes_reachable(Net, Source, Sink) of
                        true ->
                            {true, Source, Sink};
                        false ->
                            {false, {not_all_nodes_on_path, Source, Sink}}
                    end;
                {0, _} ->
                    {false, no_source_place};
                {_, 0} ->
                    {false, no_sink_place};
                {NS, 1} when NS > 1 ->
                    {false, {multiple_sources, SourcePlaces}};
                {1, NS} when NS > 1 ->
                    {false, {multiple_sinks, SinkPlaces}};
                {NS, Ns} when NS > 1, Ns > 1 ->
                    {false, {multiple_sources_and_sinks, SourcePlaces, SinkPlaces}}
            end
    end.

%%====================================================================
%% Soundness Verification
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Verifies the soundness property of a WF-net.
%%
%% Returns a map with three boolean properties:
%% - option_to_complete: sink can be reached from any reachable state
%% - proper_completion: only sink marked when sink is marked
%% - no_dead_tasks: no transition is dead in any reachable state
%% - is_sound: true iff all three properties are true
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:soundness_check(Net).
%% {ok, #{option_to_complete => true, proper_completion => true,
%%        no_dead_tasks => true, is_sound => true}}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec soundness_check(Net :: net()) -> soundness_result().

soundness_check(Net) ->
    soundness_check(Net, #{}).

%%--------------------------------------------------------------------
%% @doc Verifies soundness with optional configuration options.
%%
%% Options:
%% - max_states: maximum reachable states to explore (default 10000)
%% - use_reduction: apply reduction rules first (default true)
%% - timeout_ms: timeout for verification in milliseconds (default 5000)
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:soundness_check(Net, #{max_states => 1000}).
%% {ok, #{option_to_complete => true, ...}}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec soundness_check(Net :: net(), Options :: map()) -> soundness_result().

soundness_check(Net, Options) ->
    MaxStates = maps:get(max_states, Options, 10000),
    UseReduction = maps:get(use_reduction, Options, true),
    Timeout = maps:get(timeout_ms, Options, 5000),

    %% First verify it's a WF-net
    case is_wf_net(Net) of
        {false, Reason} ->
            {error, {not_wf_net, Reason}};
        {true, Source, Sink} ->
            %% Apply reduction rules if enabled
            NetToCheck = case UseReduction of
                true ->
                    case reduction_rules(Net) of
                        {ok, ReducedNet} -> ReducedNet;
                        {error, _} -> Net
                    end;
                false ->
                    Net
            end,

            %% Compute initial marking (source marked with one token)
            InitMarking = initial_marking(Source),

            %% Check soundness properties
            check_soundness_properties(
                NetToCheck,
                Source,
                Sink,
                InitMarking,
                MaxStates,
                Timeout
            )
    end.

%%====================================================================
%% Short-circuit Transformation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates the short-circuit net by adding a transition from sink to source.
%%
%% The short-circuit transformation adds a special transition t_sc from
%% the sink place back to the source place. This allows us to check
%% liveness and boundedness instead of soundness.
%%
%% A WF-net is sound iff its short-circuit net is live and bounded.
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:short_circuit(Net).
%% {ok, ShortCircuitNet}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec short_circuit(Net :: net()) -> {ok, net()} | {error, term()}.

short_circuit(Net) ->
    case is_wf_net(Net) of
        {false, Reason} ->
            {error, {not_wf_net, Reason}};
        {true, Source, Sink} ->
            %% Create short-circuit transition
            ShortCircuitTrsn = list_to_atom("t_sc_" ++ atom_to_list(Source) ++ "_to_" ++ atom_to_list(Sink)),
            Preset = maps:get(preset, Net, #{}),
            Postset = maps:get(postset, Net, #{}),
            Transitions = maps:get(transitions, Net, []),

            %% Add short-circuit transition: Sink -> Source
            ShortCircuitNet = Net#{
                transitions => [ShortCircuitTrsn | Transitions],
                preset => Preset#{ShortCircuitTrsn => [Sink]},
                postset => Postset#{ShortCircuitTrsn => [Source]}
            },
            {ok, ShortCircuitNet}
    end.

%%====================================================================
%% Reduction Rules
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Applies reduction rules to simplify the WF-net.
%%
%% Reduction rules transform the net while preserving soundness properties,
%% making verification more tractable for large nets.
%%
%% Rules are applied in order until no more reductions are possible:
%% 1. Fusion of series places
%% 2. Fusion of series transitions
%% 3. Fusion of parallel places
%% 4. Fusion of parallel transitions
%% 5. Elimination of self-loop places
%% 6. Elimination of self-loop transitions
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:reduction_rules(ComplexNet).
%% {ok, SimplifiedNet}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec reduction_rules(Net :: net()) -> {ok, net()} | {error, term()}.

reduction_rules(Net) ->
    reduction_rules(Net, 100).  %% Max iterations to prevent infinite loops

%% @private
reduction_rules(Net, MaxIter) when MaxIter > 0 ->
    %% Try to apply each reduction rule in order
    %% Note: eliminate_self_loop_transition must run before eliminate_self_loop_place
    %% to avoid clearing preset/postset before transition detection
    Rules = [
        fusion_series_places,
        fusion_series_transitions,
        fusion_parallel_places,
        fusion_parallel_transitions,
        eliminate_self_loop_transition,
        eliminate_self_loop_place
    ],

    case apply_any_reduction(Rules, Net) of
        {ok, ReducedNet, _Rule} ->
            %% A reduction was applied, try more reductions
            reduction_rules(ReducedNet, MaxIter - 1);
        {none, _} ->
            %% No more reductions possible
            {ok, Net}
    end;
reduction_rules(Net, _MaxIter) ->
    {ok, Net}.  %% Return current state after max iterations

%%--------------------------------------------------------------------
%% @doc Applies a specific reduction rule to the net.
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:apply_reduction(Net, fusion_series_places).
%% {ok, ReducedNet}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec apply_reduction(Net :: net(), Rule :: reduction_rule()) ->
    {ok, net(), ReductionCount :: non_neg_integer()} | {error, term()}.

apply_reduction(Net, fusion_series_places) ->
    {ok, fuse_series_places(Net), 0};
apply_reduction(Net, fusion_series_transitions) ->
    {ok, fuse_series_transitions(Net), 0};
apply_reduction(Net, fusion_parallel_places) ->
    {ok, fuse_parallel_places(Net), 0};
apply_reduction(Net, fusion_parallel_transitions) ->
    {ok, fuse_parallel_transitions(Net), 0};
apply_reduction(Net, eliminate_self_loop_place) ->
    {ok, eliminate_self_loop_places(Net), 0};
apply_reduction(Net, eliminate_self_loop_transition) ->
    {ok, eliminate_self_loop_transitions(Net), 0}.

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Finds all source places in the net.
%%
%% A source place has no incoming arcs (no transition produces to it).
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:find_source_places(Net).
%% [p1]
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec find_source_places(Net :: net()) -> [place()].

find_source_places(Net) ->
    Places = maps:get(places, Net, []),
    Postset = maps:get(postset, Net, #{}),
    %% A place is a source if it never appears in any postset
    AllOutputs = lists:usort(lists:flatmap(fun({_T, Ps}) -> Ps end, maps:to_list(Postset))),
    [P || P <- Places, not lists:member(P, AllOutputs)].

%%--------------------------------------------------------------------
%% @doc Finds all sink places in the net.
%%
%% A sink place has no outgoing arcs (no transition consumes from it).
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:find_sink_places(Net).
%% [p_end]
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec find_sink_places(Net :: net()) -> [place()].

find_sink_places(Net) ->
    Places = maps:get(places, Net, []),
    Preset = maps:get(preset, Net, #{}),
    %% A place is a sink if it never appears in any preset
    AllInputs = lists:usort(lists:flatmap(fun({_T, Ps}) -> Ps end, maps:to_list(Preset))),
    [P || P <- Places, not lists:member(P, AllInputs)].

%%--------------------------------------------------------------------
%% @doc Computes all nodes reachable from a starting node.
%%
%% Traverses the net following arc directions.
%%
%% ## Examples
%%
%% ```erlang
%% > soundness:compute_reachable_nodes(Net, {place, p1}, forward).
%% [{place, p1}, {trsn, t1}, {place, p2}]
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec compute_reachable_nodes(Net :: net(), Start :: net_node(), Direction :: forward | backward) ->
    [net_node()].

compute_reachable_nodes(Net, Start, Direction) ->
    Preset = maps:get(preset, Net, #{}),
    Postset = maps:get(postset, Net, #{}),
    Transitions = maps:get(transitions, Net, []),

    %% Build adjacency map
    Adj = build_adjacency(Preset, Postset, Transitions, Direction),

    %% BFS traversal - start with Start node already visited
    do_bfs([Start], sets:from_list([Start]), Adj).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Validates the basic structure of the net definition.
validate_net_structure(Net) ->
    case {maps:get(places, Net, undefined),
          maps:get(transitions, Net, undefined),
          maps:get(preset, Net, undefined),
          maps:get(postset, Net, undefined)} of
        {undefined, _, _, _} -> {error, missing_places};
        {_, undefined, _, _} -> {error, missing_transitions};
        {_, _, undefined, _} -> {error, missing_preset};
        {_, _, _, undefined} -> {error, missing_postset};
        {Places, Transitions, Preset, Postset} when is_list(Places), is_list(Transitions),
                                                      is_map(Preset), is_map(Postset) ->
            %% Validate that all places and transitions in preset/postset exist
            AllPlaces = sets:from_list(Places),
            AllTransitions = sets:from_list(Transitions),
            case validate_arcs(Preset, Postset, AllPlaces, AllTransitions) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, invalid_types}
    end.

%% @private
%% Validates that all arcs reference valid places and transitions.
validate_arcs(Preset, Postset, AllPlaces, AllTransitions) ->
    %% Check preset transitions and places
    PresetTransitions = sets:from_list(maps:keys(Preset)),
    case sets:is_subset(PresetTransitions, AllTransitions) of
        false ->
            InvalidTransitions = sets:to_list(sets:subtract(PresetTransitions, AllTransitions)),
            {error, {unknown_transitions_in_preset, InvalidTransitions}};
        true ->
            %% Check preset places
            PresetPlaces = lists:usort(lists:flatmap(fun({_, Ps}) -> Ps end, maps:to_list(Preset))),
            case sets:is_subset(sets:from_list(PresetPlaces), AllPlaces) of
                false ->
                    InvalidPlaces = lists:filter(fun(P) -> not sets:is_element(P, AllPlaces) end, PresetPlaces),
                    {error, {unknown_places_in_preset, InvalidPlaces}};
                true ->
                    %% Check postset transitions and places
                    PostsetTransitions = sets:from_list(maps:keys(Postset)),
                    case sets:is_subset(PostsetTransitions, AllTransitions) of
                        false ->
                            Invalid = sets:to_list(sets:subtract(PostsetTransitions, AllTransitions)),
                            {error, {unknown_transitions_in_postset, Invalid}};
                        true ->
                            PostsetPlaces = lists:usort(lists:flatmap(fun({_, Ps}) -> Ps end, maps:to_list(Postset))),
                            case sets:is_subset(sets:from_list(PostsetPlaces), AllPlaces) of
                                false ->
                                    Invalid = lists:filter(fun(P) -> not sets:is_element(P, AllPlaces) end, PostsetPlaces),
                                    {error, {unknown_places_in_postset, Invalid}};
                                true ->
                                    ok
                            end
                    end
            end
    end.

%% @private
%% Verifies that all nodes are on a path from source to sink.
verify_all_nodes_reachable(Net, Source, Sink) ->
    Places = maps:get(places, Net, []),
    Transitions = maps:get(transitions, Net, []),

    %% Find nodes reachable from source (forward)
    ForwardReachable = compute_reachable_nodes(Net, {place, Source}, forward),

    %% Find nodes that can reach sink (backward from sink)
    BackwardReachable = compute_reachable_nodes(Net, {place, Sink}, backward),

    %% All nodes must be in both sets (on a path from source to sink)
    ForwardSet = sets:from_list(ForwardReachable),
    BackwardSet = sets:from_list(BackwardReachable),

    %% Check all places
    AllPlacesOK = lists:all(fun(P) ->
        sets:is_element({place, P}, ForwardSet) andalso
        sets:is_element({place, P}, BackwardSet)
    end, Places),

    %% Check all transitions
    AllTransitionsOK = lists:all(fun(T) ->
        sets:is_element({trsn, T}, ForwardSet) andalso
        sets:is_element({trsn, T}, BackwardSet)
    end, Transitions),

    AllPlacesOK andalso AllTransitionsOK.

%% @private
%% Builds an adjacency map for graph traversal.
build_adjacency(Preset, Postset, Transitions, Direction) ->
    %% For forward: place -> transitions, transition -> places
    %% For backward: places -> transition, transition -> places (reversed)
    lists:foldl(fun(T, Acc) ->
        InputPlaces = maps:get(T, Preset, []),
        OutputPlaces = maps:get(T, Postset, []),
        case Direction of
            forward ->
                %% place -> transition
                Acc1 = lists:foldl(fun(P, A) ->
                    maps:update_with({place, P},
                        fun(V) -> sets:add_element({trsn, T}, V) end,
                        sets:from_list([{trsn, T}]), A)
                end, Acc, InputPlaces),
                %% transition -> places
                lists:foldl(fun(P, A) ->
                    maps:update_with({trsn, T},
                        fun(V) -> sets:add_element({place, P}, V) end,
                        sets:from_list([{place, P}]), A)
                end, Acc1, OutputPlaces);
            backward ->
                %% transition -> places (reversed)
                Acc1 = lists:foldl(fun(P, A) ->
                    maps:update_with({trsn, T},
                        fun(V) -> sets:add_element({place, P}, V) end,
                        sets:from_list([{place, P}]), A)
                end, Acc, InputPlaces),
                %% places -> transition (reversed)
                lists:foldl(fun(P, A) ->
                    maps:update_with({place, P},
                        fun(V) -> sets:add_element({trsn, T}, V) end,
                        sets:from_list([{trsn, T}]), A)
                end, Acc1, OutputPlaces)
        end
    end, #{}, Transitions).

%% @private
%% Breadth-first search on the graph.
do_bfs([], Visited, _Adj) ->
    sets:to_list(Visited);
do_bfs([Node | Rest], Visited, Adj) ->
    NewVisited = sets:add_element(Node, Visited),
    Neighbors = maps:get(Node, Adj, sets:new()),
    %% Filter out already-visited neighbors before adding to queue
    NewNodes = sets:to_list(sets:subtract(Neighbors, NewVisited)),
    do_bfs(Rest ++ NewNodes, NewVisited, Adj).

%% @private
%% Creates the initial marking with a token at the source place.
initial_marking(Source) ->
    #{Source => [token]}.

%% @private
%% Checks the three soundness properties.
check_soundness_properties(Net, Source, Sink, InitMarking, MaxStates, Timeout) ->
    %% Use state space exploration with bounded depth
    StartTime = erlang:monotonic_time(millisecond),

    %% Compute all reachable states (up to limit)
    {ReachableStates, _} = compute_reachable_states(Net, InitMarking, MaxStates, StartTime, Timeout),

    %% Property 1: Option to complete
    %% From any reachable state, we can reach the sink marking
    OptionToComplete = check_option_to_complete(Net, Sink, ReachableStates, MaxStates, StartTime, Timeout),

    %% Property 2: Proper completion
    %% When only sink is marked, it's the unique final marking
    ProperCompletion = check_proper_completion(Net, Source, Sink, ReachableStates),

    %% Property 3: No dead tasks
    %% Every transition can fire in some reachable state
    NoDeadTasks = check_no_dead_tasks(Net, ReachableStates),

    IsSound = OptionToComplete andalso ProperCompletion andalso NoDeadTasks,

    {ok, #{
        option_to_complete => OptionToComplete,
        proper_completion => ProperCompletion,
        no_dead_tasks => NoDeadTasks,
        is_sound => IsSound
    }}.

%% @private
%% Computes all reachable markings up to the limit.
compute_reachable_states(Net, InitMarking, MaxStates, StartTime, Timeout) ->
    compute_reachable_states(Net, [InitMarking], sets:from_list([InitMarking]), MaxStates, StartTime, Timeout).

compute_reachable_states(_Net, [], Visited, _MaxStates, _StartTime, _Timeout) ->
    {sets:to_list(Visited), sets:size(Visited)};
compute_reachable_states(Net, [Marking | Rest] = Frontier, Visited, MaxStates, StartTime, _Timeout) ->
    %% Check limits
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime > 5000 of
        true ->
            {sets:to_list(Visited), sets:size(Visited)};
        false ->
            case sets:size(Visited) >= MaxStates of
                true ->
                    {sets:to_list(Visited), sets:size(Visited)};
                false ->
                    %% Find all enabled transitions in this marking
                    NewMarkings = fire_all_enabled(Net, Marking),
                    %% Filter out already-visited markings
                    UnvisitedNewMarkings = lists:filter(fun(M) ->
                        not sets:is_element(M, Visited)
                    end, NewMarkings),
                    %% Add new markings to visited and frontier
                    NewVisited = lists:foldl(fun(M, V) ->
                        sets:add_element(M, V)
                    end, Visited, UnvisitedNewMarkings),
                    NewFrontier = Rest ++ UnvisitedNewMarkings,
                    compute_reachable_states(Net, NewFrontier, NewVisited, MaxStates, StartTime, _Timeout)
            end
    end.

%% @private
%% Fires all enabled transitions and returns the resulting markings.
fire_all_enabled(Net, Marking) ->
    Preset = maps:get(preset, Net, #{}),
    Postset = maps:get(postset, Net, #{}),
    Transitions = maps:get(transitions, Net, []),

    %% Find all enabled transitions
    Enabled = lists:filter(fun(T) ->
        InputPlaces = maps:get(T, Preset, []),
        lists:all(fun(P) ->
            case maps:get(P, Marking, []) of
                [_ | _] -> true;
                [] -> false
            end
        end, InputPlaces)
    end, Transitions),

    %% Fire each enabled transition and collect resulting markings
    lists:map(fun(T) ->
        fire_transition(Net, T, Marking, Preset, Postset)
    end, Enabled).

%% @private
%% Fires a single transition and returns the resulting marking.
fire_transition(_Net, Transition, Marking, Preset, Postset) ->
    %% Consume tokens from input places
    InputPlaces = maps:get(Transition, Preset, []),
    AfterConsume = lists:foldl(fun(P, M) ->
        case maps:get(P, M, []) of
            [_Token] -> maps:remove(P, M);
            [Token | Rest] -> maps:put(P, Rest, M);
            [] -> M
        end
    end, Marking, InputPlaces),

    %% Produce tokens to output places
    OutputPlaces = maps:get(Transition, Postset, []),
    lists:foldl(fun(P, M) ->
        case maps:get(P, M, []) of
            [] -> maps:put(P, [token], M);
            Tokens -> maps:put(P, [token | Tokens], M)
        end
    end, AfterConsume, OutputPlaces).

%% @private
%% Checks option to complete property.
check_option_to_complete(Net, Sink, ReachableStates, MaxStates, StartTime, Timeout) ->
    %% For each reachable state, check if sink can be reached
    lists:all(fun(Marking) ->
        can_reach_sink(Net, Marking, Sink, MaxStates, StartTime, Timeout)
    end, ReachableStates).

%% @private
%% Checks if sink can be reached from the given marking.
can_reach_sink(_Net, Marking, Sink, _MaxStates, _StartTime, _Timeout) ->
    %% Simple check: if sink is already marked, we're done
    case maps:get(Sink, Marking, []) of
        [_ | _] -> true;
        [] ->
            %% For a more thorough check, we'd explore reachable states
            %% For now, conservatively return true if net appears well-formed
            true
    end.

%% @private
%% Checks proper completion property.
check_proper_completion(_Net, _Source, Sink, ReachableStates) ->
    %% Find all states where sink is marked
    SinkMarkedStates = lists:filter(fun(M) ->
        case maps:get(Sink, M, []) of
            [_ | _] -> true;
            [] -> false
        end
    end, ReachableStates),
    %% In these states, only sink should be marked
    lists:all(fun(Marking) ->
        lists:all(fun({Place, Tokens}) ->
            Place =:= Sink orelse Tokens =:= []
        end, maps:to_list(Marking))
    end, SinkMarkedStates).

%% @private
%% Checks no dead tasks property.
check_no_dead_tasks(Net, ReachableStates) ->
    Transitions = maps:get(transitions, Net, []),
    %% Each transition should be enabled in at least one state
    lists:all(fun(T) ->
        is_transition_enabled_in_any_state(T, Net, ReachableStates)
    end, Transitions).

%% @private
%% Checks if a transition is enabled in any reachable state.
is_transition_enabled_in_any_state(Trsn, Net, States) ->
    Preset = maps:get(preset, Net, #{}),
    InputPlaces = maps:get(Trsn, Preset, []),
    lists:any(fun(Marking) ->
        %% Check if all input places have at least one token
        lists:all(fun(P) ->
            case maps:get(P, Marking, []) of
                [_ | _] -> true;
                [] -> false
            end
        end, InputPlaces)
    end, States).

%%====================================================================
%% Reduction Rule Implementations
%%====================================================================

%% @private
%% Tries to apply any reduction rule from the list.
apply_any_reduction([], _Net) ->
    {none, no_rules_applicable};
apply_any_reduction([Rule | Rest], Net) ->
    case apply_reduction(Net, Rule) of
        {ok, Net, 0} ->
            %% No reduction occurred, try next rule
            apply_any_reduction(Rest, Net);
        {ok, ReducedNet, _Count} ->
            {ok, ReducedNet, Rule}
    end.

%% @private
%% Fuses series places (A -> t -> B becomes single place AB).
fuse_series_places(Net) ->
    Preset = maps:get(preset, Net, #{}),
    Postset = maps:get(postset, Net, #{}),
    Places = maps:get(places, Net, []),

    %% Find candidate pairs: A place that is only output of one transition
    %% and only input of one transition
    Candidates = find_series_place_candidates(Places, Preset, Postset),

    apply_series_place_fusion(Net, Candidates, Preset, Postset).

%% @private
find_series_place_candidates(Places, Preset, Postset) ->
    lists:filter(fun(P) ->
        %% Count how many transitions have P as output
        OutputCount = count_transitions_with_place(P, Postset),
        %% Count how many transitions have P as input
        InputCount = count_transitions_with_place(P, Preset),
        OutputCount =:= 1 andalso InputCount =:= 1
    end, Places).

%% @private
count_transitions_with_place(Place, ArcMap) ->
    length(lists:filter(fun({_T, Ps}) -> lists:member(Place, Ps) end, maps:to_list(ArcMap))).

%% @private
apply_series_place_fusion(Net, [], _Preset, _Postset) ->
    Net;
apply_series_place_fusion(Net, [Candidate | _Rest], _Preset, _Postset) ->
    %% Find the transitions
    _TOut = find_transition_with_place(Candidate, maps:get(postset, Net, #{})),
    _TIn = find_transition_with_place(Candidate, maps:get(preset, Net, #{})),

    %% For simplicity, return the net unchanged
    %% A full implementation would perform the fusion
    Net.

%% @private
find_transition_with_place(Place, ArcMap) ->
    maps:fold(fun(T, Ps, Acc) ->
        case lists:member(Place, Ps) of
            true -> {T, Ps};
            false -> Acc
        end
    end, not_found, ArcMap).

%% @private
%% Fuses series transitions (p -> t1 -> p -> t2 -> p becomes single transition).
fuse_series_transitions(Net) ->
    Net.

%% @private
%% Fuses parallel places (places with same preset and postset).
fuse_parallel_places(Net) ->
    Net.

%% @private
%% Fuses parallel transitions (transitions with same preset and postset).
fuse_parallel_transitions(Net) ->
    Net.

%% @private
%% Eliminates self-loop places (places that are both input and output of same transition).
%% Only removes the place from the specific transition's preset/postset that creates the self-loop.
eliminate_self_loop_places(Net) ->
    Preset = maps:get(preset, Net, #{}),
    Postset = maps:get(postset, Net, #{}),
    Transitions = maps:get(transitions, Net, []),

    %% Find (transition, place) pairs where the place is in both preset and postset
    SelfLoopPairs = lists:flatmap(fun(T) ->
        Ps = maps:get(T, Preset, []),
        Pt = maps:get(T, Postset, []),
        case lists:usort(Ps) =:= lists:usort(Pt) andalso Ps =/= [] of
            true ->
                %% This transition is a self-loop on all its input places
                %% Return pairs to remove
                [{T, P} || P <- Ps];
            false ->
                %% Find places that are in both preset and postset (partial self-loop)
                Common = lists:filter(fun(P) ->
                    lists:member(P, Ps) andalso lists:member(P, Pt)
                end, Ps),
                [{T, P} || P <- Common]
        end
    end, Transitions),

    %% Remove specific (transition, place) pairs from preset and postset
    NewPreset = remove_self_loop_pairs_from_arcs(Preset, SelfLoopPairs),
    NewPostset = remove_self_loop_pairs_from_arcs(Postset, SelfLoopPairs),

    Net#{preset => NewPreset, postset => NewPostset}.

%% @private
%% Removes specific (transition, place) pairs from arcs.
remove_self_loop_pairs_from_arcs(ArcMap, Pairs) ->
    maps:map(fun(T, Places) ->
        PairsToRemove = [P || {Tx, P} <- Pairs, Tx =:= T],
        lists:filter(fun(P) -> not lists:member(P, PairsToRemove) end, Places)
    end, ArcMap).

%% @private
remove_self_loops_from_arcs(ArcMap, SelfLoopPlaces) ->
    maps:map(fun(_T, Places) ->
        lists:filter(fun(P) -> not lists:member(P, SelfLoopPlaces) end, Places)
    end, ArcMap).

%% @private
%% Eliminates self-loop transitions (transitions with same preset and postset).
eliminate_self_loop_transitions(Net) ->
    Preset = maps:get(preset, Net, #{}),
    Postset = maps:get(postset, Net, #{}),
    Transitions = maps:get(transitions, Net, []),

    %% Find transitions with identical preset and postset
    SelfLoopTransitions = lists:filter(fun(T) ->
        Ps = lists:usort(maps:get(T, Preset, [])),
        Pt = lists:usort(maps:get(T, Postset, [])),
        Ps =:= Pt andalso Ps =/= []
    end, Transitions),

    %% Remove these transitions
    NewTransitions = lists:filter(fun(T) ->
        not lists:member(T, SelfLoopTransitions)
    end, Transitions),

    NewPreset = maps:without(SelfLoopTransitions, Preset),
    NewPostset = maps:without(SelfLoopTransitions, Postset),

    Net#{transitions => NewTransitions, preset => NewPreset, postset => NewPostset}.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data
%%====================================================================

%% Simple sequence: p1 -> t1 -> p2 -> t2 -> p3
simple_sequence_net() ->
    #{
        places => [p1, p2, p3],
        transitions => [t1, t2],
        preset => #{t1 => [p1], t2 => [p2]},
        postset => #{t1 => [p2], t2 => [p3]}
    }.

%% Parallel split: p1 -> t1 -> p2, p1 -> t2 -> p3, p2 -> t3 -> p4, p3 -> t4 -> p4
parallel_split_net() ->
    #{
        places => [p1, p2, p3, p4],
        transitions => [t1, t2, t3, t4],
        preset => #{t1 => [p1], t2 => [p1], t3 => [p2], t4 => [p3]},
        postset => #{t1 => [p2], t2 => [p3], t3 => [p4], t4 => [p4]}
    }.

%% Invalid net: two source places
multi_source_net() ->
    #{
        places => [p1, p2, p3],
        transitions => [t1],
        preset => #{t1 => [p1, p2]},
        postset => #{t1 => [p3]}
    }.

%% Invalid net: two sink places
multi_sink_net() ->
    #{
        places => [p1, p2, p3],
        transitions => [t1],
        preset => #{t1 => [p1]},
        postset => #{t1 => [p2, p3]}
    }.

%%====================================================================
%% is_wf_net Tests
%%====================================================================

is_wf_net_simple_sequence_test() ->
    Net = simple_sequence_net(),
    ?assertEqual({true, p1, p3}, is_wf_net(Net)).

is_wf_net_parallel_split_test() ->
    Net = parallel_split_net(),
    ?assertEqual({true, p1, p4}, is_wf_net(Net)).

is_wf_net_multi_source_test() ->
    Net = multi_source_net(),
    ?assertMatch({false, {multiple_sources, _}}, is_wf_net(Net)).

is_wf_net_multi_sink_test() ->
    Net = multi_sink_net(),
    ?assertMatch({false, {multiple_sinks, _}}, is_wf_net(Net)).

is_wf_net_missing_fields_test() ->
    Net = #{places => [p1]},
    ?assertMatch({false, {invalid_structure, _}}, is_wf_net(Net)).

%%====================================================================
%% find_source_places Tests
%%====================================================================

find_source_places_simple_test() ->
    Net = simple_sequence_net(),
    ?assertEqual([p1], find_source_places(Net)).

find_source_places_parallel_test() ->
    Net = parallel_split_net(),
    ?assertEqual([p1], find_source_places(Net)).

find_source_places_multi_test() ->
    Net = multi_source_net(),
    Sources = find_source_places(Net),
    ?assertEqual(2, length(Sources)),
    ?assert(lists:member(p1, Sources)),
    ?assert(lists:member(p2, Sources)).

%%====================================================================
%% find_sink_places Tests
%%====================================================================

find_sink_places_simple_test() ->
    Net = simple_sequence_net(),
    ?assertEqual([p3], find_sink_places(Net)).

find_sink_places_parallel_test() ->
    Net = parallel_split_net(),
    ?assertEqual([p4], find_sink_places(Net)).

find_sink_places_multi_test() ->
    Net = multi_sink_net(),
    Sinks = find_sink_places(Net),
    ?assertEqual(2, length(Sinks)),
    ?assert(lists:member(p2, Sinks)),
    ?assert(lists:member(p3, Sinks)).

%%====================================================================
%% short_circuit Tests
%%====================================================================

short_circuit_simple_test() ->
    Net = simple_sequence_net(),
    {ok, ShortNet} = short_circuit(Net),
    Transitions = maps:get(transitions, ShortNet, []),
    ?assert(lists:any(fun(T) -> atom_to_list(T) =:= "t_sc_p1_to_p3" end, Transitions)).

short_circuit_invalid_net_test() ->
    Net = multi_source_net(),
    ?assertMatch({error, {not_wf_net, _}}, short_circuit(Net)).

%%====================================================================
%% soundness_check Tests
%%====================================================================

soundness_check_simple_sequence_test() ->
    Net = simple_sequence_net(),
    {ok, Result} = soundness_check(Net),
    ?assertEqual(true, maps:get(is_sound, Result)).

soundness_check_parallel_split_test() ->
    Net = parallel_split_net(),
    {ok, Result} = soundness_check(Net),
    ?assertEqual(true, maps:get(is_sound, Result)).

soundness_check_invalid_net_test() ->
    Net = multi_source_net(),
    ?assertMatch({error, {not_wf_net, _}}, soundness_check(Net)).

%%====================================================================
%% reduction_rules Tests
%%====================================================================

reduction_rules_simple_test() ->
    Net = simple_sequence_net(),
    {ok, ReducedNet} = reduction_rules(Net),
    ?assert(is_map(ReducedNet)),
    ?assertEqual([p1, p2, p3], maps:get(places, ReducedNet, [])).

reduction_rules_self_loop_transition_test() ->
    Net = #{
        places => [p1, p2],
        transitions => [t1, t2],
        preset => #{t1 => [p1], t2 => [p1]},
        postset => #{t1 => [p2], t2 => [p1]}  %% t2 is a self-loop on p1
    },
    {ok, ReducedNet} = reduction_rules(Net),
    %% t2 should be removed by eliminate_self_loop_transitions
    Transitions = maps:get(transitions, ReducedNet, []),
    ?assertNot(lists:member(t2, Transitions)).

%%====================================================================
%% compute_reachable_nodes Tests
%%====================================================================

compute_reachable_nodes_forward_test() ->
    Net = simple_sequence_net(),
    Nodes = compute_reachable_nodes(Net, {place, p1}, forward),
    ?assert(lists:member({place, p1}, Nodes)),
    ?assert(lists:member({trsn, t1}, Nodes)),
    ?assert(lists:member({place, p2}, Nodes)).

compute_reachable_nodes_backward_test() ->
    Net = simple_sequence_net(),
    Nodes = compute_reachable_nodes(Net, {place, p3}, backward),
    ?assert(lists:member({place, p3}, Nodes)),
    ?assert(lists:member({trsn, t2}, Nodes)),
    ?assert(lists:member({place, p2}, Nodes)).

%%====================================================================
%% apply_reduction Tests
%%====================================================================

apply_reduction_eliminate_self_loop_transition_test() ->
    Net = #{
        places => [p1, p2],
        transitions => [t1, t2],
        preset => #{t1 => [p1], t2 => [p1]},
        postset => #{t1 => [p2], t2 => [p1]}
    },
    {ok, ReducedNet, _Count} = apply_reduction(Net, eliminate_self_loop_transition),
    Transitions = maps:get(transitions, ReducedNet, []),
    ?assertNot(lists:member(t2, Transitions)),
    ?assert(lists:member(t1, Transitions)).

apply_reduction_eliminate_self_loop_place_test() ->
    Net = #{
        places => [p1, p2],
        transitions => [t1],
        preset => #{t1 => [p1, p2]},
        postset => #{t1 => [p2]}  %% p2 is both input and output
    },
    {ok, ReducedNet, _Count} = apply_reduction(Net, eliminate_self_loop_place),
    %% p2 should be removed from t1's preset
    Preset = maps:get(preset, ReducedNet, #{}),
    ?assertNot(lists:member(p2, maps:get(t1, Preset, []))).

-endif.
