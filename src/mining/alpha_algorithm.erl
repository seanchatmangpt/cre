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
%% @doc Alpha Algorithm for Process Mining
%%
%% This module implements the Alpha algorithm for discovering workflow
%% nets (WF-nets) from event logs, as described in:
%%
%% "Workflow Mining: Discovering Process Models from Event Logs"
%% by W.M.P. van der Aalst, et al. (2001)
%%
%% <h3>Algorithm Overview</h3>
%%
%% The Alpha algorithm constructs a Petri net (WF-net) from an event log
%% by analyzing ordering relations between activities:
%%
%% <ul>
%%   <li><b>Direct succession (a > b):</b> Activity a is immediately
%%       followed by activity b in some trace</li>
%%   <li><b>Causality (a -> b):</b> a > b and not b > a</li>
%%   <li><b>Parallel (a || b):</b> a > b and b > a</li>
%%   <li><b>Unrelated (a # b):</b> Neither a > b nor b > a</li>
%% </ul>
%%
%% <h3>Algorithm Steps</h3>
%%
%% <ol>
%%   <li>Extract TL (set of transitions appearing in log)</li>
%%   <li>Extract direct succession relations (>) from log</li>
%%   <li>Extract causal relations (->) using the footprint matrix</li>
%%   <li>Extract parallel relations (||) from footprint matrix</li>
%%   <li>Construct places based on causal relations</li>
%%   <li>Connect transitions and places to form WF-net</li>
%% </ol>
%%
%% <h3>Event Log Format</h3>
%%
%% Event logs are lists of traces, where each trace is a list of
%% activities (atoms) representing a case execution:
%%
%% ```erlang
%% > Log = [[a, b, c, d], [a, c, b, d], [a, b, c, e, d]].
%% _
%% > alpha_algorithm:mine_workflow_net(Log).
%% #{places => [...], transitions => [a,b,c,d,e], initial => ..., final => ...}
%% '''
%%
%% <h3>WF-net Structure</h3>
%%
%% The output is a map containing:
%% <ul>
%%   <li><b>places:</b> List of place atoms</li>
%%   <li><b>transitions:</b> List of transition atoms (activities)</li>
%%   <li><b>arcs:</b> List of {source, target} tuples</li>
%%   <li><b>initial_place:</b> The source place (i_source)</li>
%%   <li><b>final_place:</b> The sink place (o_sink)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(alpha_algorithm).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([mine_workflow_net/1]).
-export([extract_ordering_relations/1]).
-export([event_log_to_relations/1]).

%% Utility exports for testing
-export([direct_succession/1]).
-export([causality/1]).
-export([parallel/1]).
-export([unrelated/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc An activity in the event log.
%%
%% Activities are represented as atoms for efficient pattern matching.
%%--------------------------------------------------------------------
-type activity() :: atom().

%%--------------------------------------------------------------------
%% @doc A trace is a sequence of activities.
%%
%% Each trace represents one complete case execution through the
%% discovered process model.
%%--------------------------------------------------------------------
-type trace() :: [activity()].

%%--------------------------------------------------------------------
%% @doc An event log is a list of traces.
%%
%% The log may contain multiple traces of varying lengths, representing
%% different execution paths through the process.
%%--------------------------------------------------------------------
-type event_log() :: [trace()].

%%--------------------------------------------------------------------
%% @doc Direct succession relation a > b.
%%
%% A set of {A, B} tuples indicating activity A is immediately
%% followed by activity B in at least one trace.
%%--------------------------------------------------------------------
-type direct_succession() :: sets:set({activity(), activity()}).

%%--------------------------------------------------------------------
%% @doc Causal relation a -> b.
%%
%% A set of {A, B} tuples indicating activity A causally precedes
%% activity B (A > B and not B > A).
%%--------------------------------------------------------------------
-type causality() :: sets:set({activity(), activity()}).

%%--------------------------------------------------------------------
%% @doc Parallel relation a || b.
%%
%% A set of {A, B} tuples indicating activities A and B can execute
%% in parallel (A > B and B > A).
%%--------------------------------------------------------------------
-type parallel() :: sets:set({activity(), activity()}).

%%--------------------------------------------------------------------
%% @doc Unrelated relation a # b.
%%
%% A set of {A, B} tuples indicating no ordering relationship between
%% activities A and B.
%%--------------------------------------------------------------------
-type unrelated() :: sets:set({activity(), activity()}).

%%--------------------------------------------------------------------
%% @doc Ordering relations extracted from an event log.
%%
%% Contains all four relation types from the Alpha algorithm's
%% footprint matrix analysis.
%%--------------------------------------------------------------------
-type ordering_relations() :: #{
    direct_succession => direct_succession(),
    causality => causality(),
    parallel => parallel(),
    unrelated => unrelated(),
    activities => sets:set(activity())
}.

%%--------------------------------------------------------------------
%% @doc A place in the WF-net.
%%
%% Places are atoms representing Petri net places where tokens reside.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the WF-net.
%%
%% Transitions correspond to activities from the event log.
%%--------------------------------------------------------------------
-type transition() :: atom().

%%--------------------------------------------------------------------
%% @doc An arc connecting two nodes in the WF-net.
%%
%% Arcs can be place->transition, transition->place, or
%% place->place (for initial/final connections).
%%--------------------------------------------------------------------
-type arc() :: {place(), transition()} | {transition(), place()}.

%%--------------------------------------------------------------------
%% @doc A workflow net (WF-net) discovered from an event log.
%%
%% Contains places, transitions, arcs, and the designated source/sink
%% places required for WF-net soundness properties.
%%--------------------------------------------------------------------
-type wf_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place()
}.

%% Export types
-export_type([
    activity/0,
    trace/0,
    event_log/0,
    ordering_relations/0,
    wf_net/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Mines a workflow net from an event log using the Alpha algorithm.
%%
%% This is the main entry point that implements the complete Alpha
%% algorithm:
%%
%% 1. Extract all activities from the log
%% 2. Compute direct succession relations
%% 3. Extract causal relations from footprint matrix
%% 4. Identify parallel relations
%% 5. Construct places based on causal relations
%% 6. Build complete WF-net structure
%%
%% Returns a map containing the places, transitions, arcs, and the
%% initial/final places of the discovered workflow net.
%%
%% ## Examples
%%
%% Basic sequential process:
%% ```erlang
%% > Log = [[a, b, c]],
%% > alpha_algorithm:mine_workflow_net(Log).
%% #{places => [i_source,'p_a_b','p_b_c',o_sink],
%%   transitions => [a,b,c],
%%   arcs => [{i_source,a},{a,'p_a_b'},{'p_a_b',b},{b,'p_b_c'},
%%            {'p_b_c',c},{c,o_sink}],
%%   initial_place => i_source,
%%   final_place => o_sink}
%% '''
%%
%% Process with parallelism:
%% ```erlang
%% > Log = [[a, b, c, d], [a, c, b, d]],
%% > Net = alpha_algorithm:mine_workflow_net(Log).
%% _
%% > maps:get(places, Net).
%% [i_source,p_i_a,p_i_b,p_i_c,p_o_b,p_o_c,p_o_d,o_sink]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec mine_workflow_net(Log :: event_log()) -> wf_net().

mine_workflow_net(Log) when is_list(Log) ->
    %% Step 1: Extract TL (set of all transitions/activities)
    Activities = extract_activities(Log),

    %% Step 2: Extract direct succession relations (>)
    DirectSucc = direct_succession(Log),

    %% Step 3: Extract causal relations (->)
    Causal = causality(DirectSucc),

    %% Step 4: Extract parallel relations (||)
    _Parallel = parallel(DirectSucc),

    %% Step 5: Extract unrelated relations (#)
    _Unrelated = unrelated(DirectSucc, Activities),

    %% Step 6: Identify initial activities (TI_L)
    InitialActivities = find_initial_activities(DirectSucc, Activities),

    %% Step 7: Identify final activities (TO_L)
    FinalActivities = find_final_activities(DirectSucc, Activities),

    %% Step 8: Construct places based on causal relations
    Places = construct_places(InitialActivities, FinalActivities, Causal),

    %% Step 9: Build complete WF-net structure
    build_wf_net(Activities, Places, InitialActivities, FinalActivities,
                 Causal, DirectSucc).

%%--------------------------------------------------------------------
%% @doc Extracts all ordering relations from an event log.
%%
%% Returns a map containing:
%% <ul>
%%   <li><b>direct_succession:</b> Set of {A, B} where A > B</li>
%%   <li><b>causality:</b> Set of {A, B} where A -> B</li>
%%   <li><b>parallel:</b> Set of {A, B} where A || B</li>
%%   <li><b>unrelated:</b> Set of {A, B} where A # B</li>
%%   <li><b>activities:</b> Set of all activities in log</li>
%% </ul>
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c], [a, c, b]],
%% > Relations = alpha_algorithm:extract_ordering_relations(Log),
%% _
%% > sets:to_list(maps:get(causality, Relations)).
%% [{a,b},{a,c},{b,d},{c,d}]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_ordering_relations(Log :: event_log()) -> ordering_relations().

extract_ordering_relations(Log) when is_list(Log) ->
    Activities = extract_activities(Log),
    DirectSucc = direct_succession(Log),
    Causal = causality(DirectSucc),
    Parallel = parallel(DirectSucc),
    Unrel = unrelated(DirectSucc, Activities),

    #{
        direct_succession => DirectSucc,
        causality => Causal,
        parallel => Parallel,
        unrelated => Unrel,
        activities => Activities
    }.

%%--------------------------------------------------------------------
%% @doc Parses an event log and extracts ordering relations.
%%
%% This is an alias for extract_ordering_relations/1 that follows the
%% naming convention suggested in the requirements.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c, d], [a, c, b, d]],
%% > alpha_algorithm:event_log_to_relations(Log).
%% #{direct_succession => {...}, causality => {...}, ...}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec event_log_to_relations(Log :: event_log()) -> ordering_relations().

event_log_to_relations(Log) ->
    extract_ordering_relations(Log).

%%====================================================================
%% Relation Extraction Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extracts direct succession relations from an event log.
%%
%% Direct succession (a > b) exists if activity a is immediately
%% followed by activity b in at least one trace.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c, d], [a, c, b, d]],
%% > DS = alpha_algorithm:direct_succession(Log),
%% _
%% > sets:to_list(DS).
%% [{a,b},{b,c},{c,d},{a,c},{c,b},{b,d}]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec direct_succession(Log :: event_log()) -> direct_succession().

direct_succession(Log) when is_list(Log) ->
    lists:foldl(fun(Trace, Acc) ->
        extract_succession_from_trace(Trace, Acc)
    end, sets:new(), Log).

%% @private
%% @doc Extracts direct succession pairs from a single trace.
-spec extract_succession_from_trace(trace(), direct_succession()) ->
    direct_succession().

extract_succession_from_trace([], Acc) ->
    Acc;
extract_succession_from_trace([_], Acc) ->
    Acc;
extract_succession_from_trace([A, B | Rest], Acc) ->
    NewAcc = sets:add_element({A, B}, Acc),
    extract_succession_from_trace([B | Rest], NewAcc).

%%--------------------------------------------------------------------
%% @doc Extracts causal relations from direct succession relations.
%%
%% Causality (a -> b) exists when a > b and NOT b > a.
%% This represents a true causal dependency where a must precede b.
%%
%% ## Examples
%%
%% ```erlang
%% > DS = sets:from_list([{a,b},{b,a},{b,c},{c,d}]),
%% > Causal = alpha_algorithm:causality(DS),
%% _
%% > sets:to_list(Causal).
%% [{b,c},{c,d}]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec causality(DirectSucc :: direct_succession()) -> causality().

causality(DirectSucc) ->
    DirectList = sets:to_list(DirectSucc),
    lists:foldl(fun({A, B}, Acc) ->
        %% A -> B if A > B and NOT B > A
        case sets:is_element({B, A}, DirectSucc) of
            true -> Acc;  %% Parallel, not causal
            false -> sets:add_element({A, B}, Acc)
        end
    end, sets:new(), DirectList).

%%--------------------------------------------------------------------
%% @doc Extracts parallel relations from direct succession relations.
%%
%% Parallel (a || b) exists when both a > b AND b > a.
%% This indicates activities can execute in parallel.
%%
%% ## Examples
%%
%% ```erlang
%% > DS = sets:from_list([{a,b},{b,a},{b,c}]),
%% > Par = alpha_algorithm:parallel(DS),
%% _
%% > sets:to_list(Par).
%% [{a,b}]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel(DirectSucc :: direct_succession()) -> parallel().

parallel(DirectSucc) ->
    DirectList = sets:to_list(DirectSucc),
    lists:foldl(fun({A, B}, Acc) ->
        %% A || B if A > B AND B > A
        %% Only add one direction (A < B) to avoid duplicates
        case sets:is_element({B, A}, DirectSucc) of
            true when A < B -> sets:add_element({A, B}, Acc);
            true -> Acc;
            false -> Acc
        end
    end, sets:new(), DirectList).

%%--------------------------------------------------------------------
%% @doc Extracts unrelated relations from direct succession and activities.
%%
%% Unrelated (a # b) exists when NEITHER a > b NOR b > a.
%%
%% ## Examples
%%
%% ```erlang
%% > Activities = sets:from_list([a, b, c, d]),
%% > DS = sets:from_list([{a,b},{b,c}]),
%% > Unrel = alpha_algorithm:unrelated(DS, Activities),
%% _
%% > lists:sort(sets:to_list(Unrel)).
%% [{a,c},{a,d},{b,d},{c,a},{c,d},{d,a},{d,b},{d,c}]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec unrelated(DirectSucc :: direct_succession(),
                Activities :: sets:set(activity())) -> unrelated().

unrelated(DirectSucc, Activities) ->
    ActivityList = sets:to_list(Activities),
    AllPairs = [{A, B} || A <- ActivityList, B <- ActivityList, A =/= B],

    lists:foldl(fun({A, B}, Acc) ->
        HasAB = sets:is_element({A, B}, DirectSucc),
        HasBA = sets:is_element({B, A}, DirectSucc),
        case HasAB orelse HasBA of
            true -> Acc;
            false -> sets:add_element({A, B}, Acc)
        end
    end, sets:new(), AllPairs).

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @private
%% @doc Extracts all unique activities from an event log.
-spec extract_activities(event_log()) -> sets:set(activity()).

extract_activities(Log) ->
    lists:foldl(fun(Trace, Acc) ->
        lists:foldl(fun(Activity, Set) ->
            sets:add_element(Activity, Set)
        end, Acc, Trace)
    end, sets:new(), Log).

%% @private
%% @doc Finds initial activities (TI_L in the paper).
%%
%% Initial activities are those with no incoming causality.
-spec find_initial_activities(direct_succession(), sets:set(activity())) ->
    [activity()].

find_initial_activities(DirectSucc, Activities) ->
    ActivityList = sets:to_list(Activities),
    lists:filter(fun(A) ->
        %% A is initial if no activity B exists such that B > A
        %% i.e., A never appears as the second element
        not lists:any(fun({_B, X}) -> X =:= A end, sets:to_list(DirectSucc))
    end, ActivityList).

%% @private
%% @doc Finds final activities (TO_L in the paper).
%%
%% Final activities are those with no outgoing causality.
-spec find_final_activities(direct_succession(), sets:set(activity())) ->
    [activity()].

find_final_activities(DirectSucc, Activities) ->
    ActivityList = sets:to_list(Activities),
    lists:filter(fun(A) ->
        %% A is final if no activity B exists such that A > B
        %% i.e., A never appears as the first element
        not lists:any(fun({X, _B}) -> X =:= A end, sets:to_list(DirectSucc))
    end, ActivityList).

%% @private
%% @doc Constructs places based on Alpha algorithm rules.
%%
%% For each pair (A, B) in causality, creates a place p_A_B
%% representing the causal relationship A -> B.
-spec construct_places([activity()], [activity()], causality()) -> [place()].

construct_places(_InitialActivities, _FinalActivities, Causal) ->
    %% For each causal pair A -> B, create a place
    CausalList = sets:to_list(Causal),
    [make_place_name(A, B) || {A, B} <- CausalList].

%% @private
%% @doc Creates a unique place name from two activities.
-spec make_place_name(activity(), activity()) -> place().

make_place_name(A, B) ->
    list_to_atom("p_" ++ atom_to_list(A) ++ "_" ++ atom_to_list(B)).

%% @private
%% @doc Builds the complete WF-net structure.
-spec build_wf_net(sets:set(activity()), [place()],
                   [activity()], [activity()],
                   causality(), direct_succession()) -> wf_net().

build_wf_net(Activities, Places, InitialActivities, FinalActivities,
             Causal, DirectSucc) ->
    TransitionList = sets:to_list(Activities),
    PlaceList = lists:usort(Places),

    %% Create initial place (source)
    InitialPlace = 'i_source',

    %% Create final place (sink)
    FinalPlace = 'o_sink',

    %% Build arcs
    Arcs = build_arcs(TransitionList, PlaceList, InitialActivities,
                      FinalActivities, Causal, DirectSucc,
                      InitialPlace, FinalPlace),

    %% Complete place list includes source and sink
    AllPlaces = lists:usort([InitialPlace, FinalPlace | PlaceList]),

    #{
        places => AllPlaces,
        transitions => TransitionList,
        arcs => Arcs,
        initial_place => InitialPlace,
        final_place => FinalPlace
    }.

%% @private
%% @doc Builds all arcs for the WF-net.
-spec build_arcs([activity()], [place()], [activity()], [activity()],
                 causality(), direct_succession(),
                 place(), place()) -> [arc()].

build_arcs(_Transitions, _Places, InitialActivities, FinalActivities,
           Causal, DirectSucc, InitialPlace, FinalPlace) ->
    %% Arcs from initial place to initial activities
    InitialArcs = [{InitialPlace, A} || A <- InitialActivities],

    %% Arcs from final activities to final place
    FinalArcs = [{A, FinalPlace} || A <- FinalActivities],

    %% Arcs based on causal relations (through places)
    CausalArcs = build_causal_arcs(Causal, DirectSucc),

    lists:usort(InitialArcs ++ FinalArcs ++ CausalArcs).

%% @private
%% @doc Builds arcs based on causal relations.
-spec build_causal_arcs(causality(), direct_succession()) -> [arc()].

build_causal_arcs(Causal, _DirectSucc) ->
    CausalList = sets:to_list(Causal),

    lists:foldl(fun({A, B}, ArcAcc) ->
        %% Create arcs A -> Place -> B
        Place = make_place_name(A, B),
        lists:usort([{A, Place}, {Place, B} | ArcAcc])
    end, [], CausalList).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data from the paper
%%--------------------------------------------------------------------

paper_log_example() ->
    [
        [a, b, c, d],
        [a, c, b, d],
        [a, b, c, e, d],
        [a, b, c, e, f, d]
    ].

%%--------------------------------------------------------------------
%% Direct succession tests
%%--------------------------------------------------------------------

direct_succession_empty_test() ->
    Log = [],
    DS = direct_succession(Log),
    ?assertEqual(0, sets:size(DS)).

direct_succession_single_trace_test() ->
    Log = [[a, b, c]],
    DS = direct_succession(Log),
    ?assert(sets:is_element({a, b}, DS)),
    ?assert(sets:is_element({b, c}, DS)),
    ?assertNot(sets:is_element({a, c}, DS)),
    ?assertEqual(2, sets:size(DS)).

direct_succession_multiple_traces_test() ->
    Log = [[a, b, c], [a, c, b]],
    DS = direct_succession(Log),
    ?assert(sets:is_element({a, b}, DS)),
    ?assert(sets:is_element({b, c}, DS)),
    ?assert(sets:is_element({a, c}, DS)),
    ?assert(sets:is_element({c, b}, DS)),
    ?assertEqual(4, sets:size(DS)).

%%--------------------------------------------------------------------
%% Causality tests
%%--------------------------------------------------------------------

causality_no_parallel_test() ->
    DS = sets:from_list([{a, b}, {b, c}, {c, d}]),
    Causal = causality(DS),
    ?assertEqual(3, sets:size(Causal)),
    ?assert(sets:is_element({a, b}, Causal)),
    ?assert(sets:is_element({b, c}, Causal)),
    ?assert(sets:is_element({c, d}, Causal)).

causality_with_parallel_test() ->
    DS = sets:from_list([{a, b}, {b, a}, {b, c}]),
    Causal = causality(DS),
    ?assertEqual(1, sets:size(Causal)),
    ?assertNot(sets:is_element({a, b}, Causal)),
    ?assertNot(sets:is_element({b, a}, Causal)),
    ?assert(sets:is_element({b, c}, Causal)).

%%--------------------------------------------------------------------
%% Parallel tests
%%--------------------------------------------------------------------

parallel_no_parallel_test() ->
    DS = sets:from_list([{a, b}, {b, c}]),
    Par = parallel(DS),
    ?assertEqual(0, sets:size(Par)).

parallel_with_parallel_test() ->
    DS = sets:from_list([{a, b}, {b, a}, {b, c}]),
    Par = parallel(DS),
    ?assertEqual(1, sets:size(Par)),
    ?assert(sets:is_element({a, b}, Par)).

%%--------------------------------------------------------------------
%% Unrelated tests
%%--------------------------------------------------------------------

unrelated_empty_test() ->
    DS = sets:new(),
    Activities = sets:from_list([a, b]),
    Unrel = unrelated(DS, Activities),
    ?assertEqual(2, sets:size(Unrel)),
    ?assert(sets:is_element({a, b}, Unrel)),
    ?assert(sets:is_element({b, a}, Unrel)).

unrelated_partial_test() ->
    DS = sets:from_list([{a, b}]),
    Activities = sets:from_list([a, b, c]),
    Unrel = unrelated(DS, Activities),
    ?assert(sets:is_element({a, c}, Unrel)),
    ?assert(sets:is_element({c, a}, Unrel)),
    ?assert(sets:is_element({b, c}, Unrel)),
    ?assert(sets:is_element({c, b}, Unrel)),
    ?assertNot(sets:is_element({a, b}, Unrel)).

%%--------------------------------------------------------------------
%% Extract ordering relations tests
%%--------------------------------------------------------------------

extract_ordering_relations_basic_test() ->
    Log = [[a, b, c]],
    Relations = extract_ordering_relations(Log),
    ?assert(sets:is_element(a, maps:get(activities, Relations))),
    ?assert(sets:is_element(b, maps:get(activities, Relations))),
    ?assert(sets:is_element(c, maps:get(activities, Relations))).

extract_ordering_relations_parallel_test() ->
    Log = [[a, b, c], [a, c, b]],
    Relations = extract_ordering_relations(Log),
    Parallel = maps:get(parallel, Relations),
    ?assert(sets:is_element({b, c}, Parallel)).

%%--------------------------------------------------------------------
%% Mine workflow net tests
%%--------------------------------------------------------------------

mine_workflow_net_sequential_test() ->
    Log = [[a, b, c]],
    Net = mine_workflow_net(Log),
    ?assert(is_list(maps:get(places, Net))),
    ?assert(is_list(maps:get(transitions, Net))),
    ?assertEqual(i_source, maps:get(initial_place, Net)),
    ?assertEqual(o_sink, maps:get(final_place, Net)).

mine_workflow_net_parallel_test() ->
    Log = [[a, b, c, d], [a, c, b, d]],
    Net = mine_workflow_net(Log),
    Transitions = maps:get(transitions, Net),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),
    ?assert(lists:member(d, Transitions)).

mine_workflow_net_paper_example_test() ->
    Log = paper_log_example(),
    Net = mine_workflow_net(Log),
    Transitions = maps:get(transitions, Net),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),
    ?assert(lists:member(d, Transitions)),
    ?assert(lists:member(e, Transitions)),
    ?assert(lists:member(f, Transitions)).

%%--------------------------------------------------------------------
%% WF-net structure validation
%%--------------------------------------------------------------------

wf_net_has_source_test() ->
    Log = [[a, b]],
    Net = mine_workflow_net(Log),
    Places = maps:get(places, Net),
    ?assert(lists:member(i_source, Places)),
    ?assertEqual(i_source, maps:get(initial_place, Net)).

wf_net_has_sink_test() ->
    Log = [[a, b]],
    Net = mine_workflow_net(Log),
    Places = maps:get(places, Net),
    ?assert(lists:member(o_sink, Places)),
    ?assertEqual(o_sink, maps:get(final_place, Net)).

wf_net_has_arcs_test() ->
    Log = [[a, b, c]],
    Net = mine_workflow_net(Log),
    Arcs = maps:get(arcs, Net),
    ?assert(is_list(Arcs)),
    ?assert(length(Arcs) > 0).

%%--------------------------------------------------------------------
%% Doctest
%%--------------------------------------------------------------------

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

-endif.
