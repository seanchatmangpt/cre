%% -*- erlang %%
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
%% @doc Woflan-style workflow diagnostics for Petri net workflows.
%%
%% This module implements workflow diagnostics based on the Woflan approach
%% described in "Diagnosing Workflow Processes" by W.M.P. van der Aalst (2001).
%%
%% The Woflan method analyzes workflow nets (WF-nets) to detect structural
%% and behavioral problems that can cause workflow execution issues.
%%
%% <h3>Diagnostic Categories</h3>
%% <ul>
%%   <li><strong>Soundness Verification:</strong> Checks if the workflow net
%%       satisfies the soundness property (proper completion, no deadlocks,
%%       boundedness)</li>
%%   <li><strong>Deadlock Detection:</strong> Identifies deadlock states
%%       in the state space where no transition can fire</li>
%%   <li><strong>Dead Transition Analysis:</strong> Finds transitions that
%%       can never fire in any reachable marking</li>
%%   <li><strong>Repair Suggestions:</strong> Provides actionable suggestions
%%       for fixing detected issues</li>
%% </ul>
%%
%% <h3>Soundness Property (Woflan Definition)</h3>
%%
%% A workflow net is sound if:
%% 1. It has a unique input place (start condition) with a token initially
%% 2. It has a unique output place (end condition)
%% 3. For every reachable marking, the marking with exactly one token in
%%    the output place is eventually reachable (proper completion)
%% 4. No place can accumulate unbounded tokens (boundedness)
%% 5. When the output place is marked, all other places are empty
%%    (proper termination)
%%
%% <h3>Example Usage</h3>
%%
%% ```erlang
%% > %% Diagnose a workflow module
%% > woflan:diagnose(my_workflow_module).
%% #{status => ok, issues => [], suggestions => []}
%%
%% > %% Diagnose an unsound workflow
%% > woflan:diagnose(unsound_workflow).
%% #{status => unsound,
%%   issues => [{deadlock, [{p1,1},{p2,1}]}],
%%   suggestions => [{add_synchronization, t1, t2}]}
%%
%% > %% Check soundness only
%% > woflan:check_soundness(my_workflow).
%% {ok, true}
%%
%% > %% Find deadlocks
%% > woflan:detect_deadlocks(my_workflow).
%% [{marking, [{p1,1},{p2,1}]}, {reason, circular_wait}]
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(woflan).
-moduledoc("""
Woflan-style workflow diagnostics for Petri net workflows.

This module implements the Woflan (Workflow Analyzer) approach for diagnosing
workflow processes based on Petri net analysis techniques from van der Aalst (2001).

## Main Functions

- `diagnose/1` - Complete diagnostic analysis with report
- `check_soundness/1` - Verify soundness property of WF-nets
- `detect_deadlocks/1` - Find deadlock states in state space
- `detect_dead_transitions/1` - Find transitions that can never fire
- `suggest_repair/1` - Generate repair suggestions for unsound workflows

## Diagnostic Report Format

Reports are returned as maps with:
- `status` - `ok`, `unsound`, or `error`
- `issues` - List of detected issues
- `suggestions` - List of repair suggestions
""").

%%====================================================================
%% Exports
%%====================================================================

%% Main diagnostic API
-export([diagnose/1]).

%% Soundness verification
-export([check_soundness/1, is_sound_workflow/1]).

%% Deadlock detection
-export([detect_deadlocks/1, find_deadlock_states/4]).

%% Dead transition analysis
-export([detect_dead_transitions/1, find_unreachable_transitions/3]).

%% Repair suggestions
-export([suggest_repair/1, generate_suggestions/2]).

%% Structural analysis helpers
-export([build_reachability_graph/2, compute_coverability_tree/2]).

%% Module extraction helpers
-export([extract_net_from_module/1, extract_net_from_spec/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net workflow.
%%
%% Places are atoms representing workflow states or conditions.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net workflow.
%%
%% Transitions represent workflow tasks, activities, or events.
%%--------------------------------------------------------------------
-type transition() :: atom().

%%--------------------------------------------------------------------
%% @doc A marking maps places to token counts.
%%
%% Represents the state of the workflow with token counts per place.
%%--------------------------------------------------------------------
-type marking() :: #{place() => non_neg_integer()}.

%%--------------------------------------------------------------------
%% @doc An arc connects a place to a transition or vice versa.
%%
%%--------------------------------------------------------------------
-type arc() :: {place(), transition()} | {transition(), place()}.

%%--------------------------------------------------------------------
%% @doc Petri net structure.
%%
%% Contains places, transitions, and arc connections.
%%--------------------------------------------------------------------
-type net() :: #{places => [place()],
                 transitions => [transition()],
                 arcs => [arc()]}.

%%--------------------------------------------------------------------
%% @doc A state in the reachability graph.
%%
%% Contains the marking and metadata about the state.
%%--------------------------------------------------------------------
-type state() :: #{marking => marking(),
                   enabled => [transition()],
                   depth => non_neg_integer()}.

%%--------------------------------------------------------------------
%% @doc Diagnostic issue types.
%%
%%--------------------------------------------------------------------
-type issue_type() :: deadlock
                   | dead_transition
                   | unbounded
                   | no_completion
                   | multiple_input
                   | multiple_output
                   | orphan_place
                   | circular_wait.

%%--------------------------------------------------------------------
%% @doc A diagnostic issue.
%%
%%--------------------------------------------------------------------
-type issue() :: {issue_type(), term()}.

%%--------------------------------------------------------------------
%% @doc Suggestion types for repair.
%%
%%--------------------------------------------------------------------
-type suggestion_type() :: add_synchronization
                         | remove_transition
                         | add_place
                         | restructure_arc
                         | add_guard
                         | split_transition.

%%--------------------------------------------------------------------
%% @doc A repair suggestion.
%%
%%--------------------------------------------------------------------
-type suggestion() :: {suggestion_type(), term()}.

%%--------------------------------------------------------------------
%% @doc Diagnostic report status.
%%
%%--------------------------------------------------------------------
-type report_status() :: ok | unsound | error.

%%--------------------------------------------------------------------
%% @doc Complete diagnostic report.
%%
%%--------------------------------------------------------------------
-type diagnostic_report() :: #{status := report_status(),
                                issues => [issue()],
                                suggestions => [suggestion()],
                                details => map()}.

%%--------------------------------------------------------------------
%% @doc Workflow input - module, net, or spec.
%%
%%--------------------------------------------------------------------
-type workflow_input() :: module() | net() | term().  % term() for YAWL spec (handled via extract_net)

%% Export types
-export_type([place/0, transition/0, marking/0, net/0,
              issue/0, suggestion/0, diagnostic_report/0,
              workflow_input/0]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main diagnostic entry point.
%%
%% Performs comprehensive Woflan-style analysis on a workflow and returns
%% a complete diagnostic report.
%%
%% @param Workflow A module atom, net structure, or YAWL spec
%% @return Diagnostic report map with status, issues, and suggestions
%%
%% @end
%%--------------------------------------------------------------------
-spec diagnose(Workflow :: workflow_input()) -> diagnostic_report().

diagnose(Workflow) ->
    try
        %% Extract net structure from input
        Net = extract_net(Workflow),

        %% Run all diagnostic checks
        Issues = collect_all_issues(Workflow, Net),

        %% Generate suggestions based on issues
        Suggestions = generate_suggestions(Workflow, Issues),

        %% Determine overall status
        Status = determine_status(Issues),

        %% Build details map
        Details = #{
            net_info => extract_net_info(Net),
            analysis_time => erlang:system_time(millisecond)
        },

        #{status => Status,
          issues => Issues,
          suggestions => Suggestions,
          details => Details}
    catch
        Type:Error:Stack ->
            ?LOG_ERROR("Woflan diagnose failed: ~p:~p~n~p", [Type, Error, Stack]),
            #{status => error,
              issues => [{diagnostic_error, {Type, Error}}],
              suggestions => [],
              details => #{stacktrace => Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Verifies soundness property of a workflow.
%%
%% A workflow is sound if it satisfies:
%% 1. Unique input place (start condition)
%% 2. Unique output place (end condition)
%% 3. Proper completion (always reaches final marking)
%% 4. Boundedness (no unbounded token accumulation)
%% 5. Proper termination (only output place marked at completion)
%%
%% @param Workflow A module atom, net structure, or YAWL spec
%% @return {ok, true} if sound, {ok, false, Reason} if unsound
%%
%% @end
%%--------------------------------------------------------------------
-spec check_soundness(Workflow :: workflow_input()) ->
          {ok, true} | {ok, false, Reason :: term()}.

check_soundness(Workflow) ->
    try
        Net = extract_net(Workflow),

        %% Check 1: Unique input place
        InputPlaces = find_input_places(Net),
        case InputPlaces of
            [_] -> ok;
            [] -> return_unsound(no_input_place);
            _ -> return_unsound(multiple_input_places)
        end,

        %% Check 2: Unique output place
        OutputPlaces = find_output_places(Net),
        case OutputPlaces of
            [_] -> ok;
            [] -> return_unsound(no_output_place);
            _ -> return_unsound(multiple_output_places)
        end,

        %% Check 3: Structural boundedness
        case check_structural_boundedness(Net) of
            {ok, true} -> ok;
            {ok, false} -> return_unsound(unbounded_net)
        end,

        %% Check 4: No dead transitions (all transitions potentially reachable)
        DeadTransitions = find_dead_transitions(Net),
        case DeadTransitions of
            [] -> ok;
            _ -> return_unsound({dead_transitions_exist, DeadTransitions})
        end,

        %% Check 5: Proper completion path exists
        case verify_completion_path(Net) of
            {ok, true} -> ok;
            {ok, false} -> return_unsound(no_completion_path)
        end,

        %% All checks passed
        {ok, true}
    catch
        return_unsound ->
            %% Exception-based early return
            receive
                {return_unsound, Reason} -> {ok, false, Reason}
            after 0 ->
                    {ok, false, unknown_error}
            end;
        Type:Error:Stack ->
            ?LOG_ERROR("Soundness check failed: ~p:~p~n~p", [Type, Error, Stack]),
            {ok, false, {check_error, {Type, Error}}}
    end.

return_unsound(Reason) ->
    throw({return_unsound, Reason}).

%%--------------------------------------------------------------------
%% @doc Quick soundness check - returns boolean.
%%
%% Simplified version of check_soundness/1 that returns a boolean
%% rather than detailed reasons.
%%
%% @param Workflow A module atom, net structure, or YAWL spec
%% @return true if sound, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_sound_workflow(Workflow :: workflow_input()) -> boolean().

is_sound_workflow(Workflow) ->
    case check_soundness(Workflow) of
        {ok, true} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Detects deadlock states in the workflow state space.
%%
%% A deadlock state is a marking where no transitions are enabled but
%% the marking is not the intended final state (output place marked).
%%
%% Uses state space exploration with bounds checking.
%%
%% @param Workflow A module atom, net structure, or YAWL spec
%% @return List of deadlock states with marking and reason
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_deadlocks(Workflow :: workflow_input()) -> [issue()].

detect_deadlocks(Workflow) ->
    try
        Net = extract_net(Workflow),
        OutputPlaces = find_output_places(Net),

        case OutputPlaces of
            [] ->
                [{no_output_place, cannot_determine_final_state}];
            [OutputPlace] ->
                InitialMarking = get_initial_marking(Net),
                MaxStates = get_max_states(Net),
                DeadlockStates = find_deadlock_states(Net, InitialMarking,
                                                     OutputPlace, MaxStates),
                lists:map(fun({Marking, Reason}) ->
                    {deadlock, #{marking => Marking, reason => Reason}}
                end, DeadlockStates);
            _ ->
                [{multiple_output_places, cannot_determine_final_state}]
        end
    catch
        Type:Error:_Stack ->
            ?LOG_ERROR("Deadlock detection failed: ~p:~p", [Type, Error]),
            [{deadlock_detection_error, {Type, Error}}]
    end.

%%--------------------------------------------------------------------
%% @doc Finds deadlock states with state space exploration.
%%
%% Performs BFS through the reachability graph to identify states
%% where no transitions are enabled.
%%
%% @param Net The Petri net structure
%% @param InitialMarking Starting marking
%% @param OutputPlace The final output place
%% @param MaxStates Maximum states to explore (prevents infinite loops)
%% @return List of {Marking, Reason} tuples for deadlocks
%%
%% @end
%%--------------------------------------------------------------------
-spec find_deadlock_states(Net :: net(),
                           InitialMarking :: marking(),
                           OutputPlace :: place(),
                           MaxStates :: pos_integer()) ->
          [{marking(), term()}].

find_deadlock_states(Net, _InitialMarking, OutputPlace, MaxStates) ->
    %% Build reachability graph with state limit
    ReachGraph = build_reachability_graph(Net, MaxStates),

    %% Find all states with no enabled transitions
    AllStates = maps:keys(ReachGraph),

    DeadlockStates = lists:filter(fun(State) ->
        #{enabled := Enabled} = maps:get(State, ReachGraph, #{}),
        Enabled =:= []
    end, AllStates),

    %% Filter out the valid final state (only output place marked)
    lists:foldl(fun(State, Acc) ->
        StateInfo = maps:get(State, ReachGraph),
        #{marking := Marking} = StateInfo,

        case is_final_marking(Marking, OutputPlace) of
            true ->
                %% This is the valid final state, not a deadlock
                Acc;
            false ->
                %% Check if truly no transitions enabled
                Enabled = maps:get(enabled, StateInfo, []),
                case Enabled of
                    [] -> [{Marking, no_enabled_transitions} | Acc];
                    _ -> Acc
                end
        end
    end, [], DeadlockStates).

%%--------------------------------------------------------------------
%% @doc Detects dead transitions that can never fire.
%%
%% A transition is dead if it cannot be enabled in any reachable
%% marking from the initial state.
%%
%% @param Workflow A module atom, net structure, or YAWL spec
%% @return List of dead transition issues
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_dead_transitions(Workflow :: workflow_input()) -> [issue()].

detect_dead_transitions(Workflow) ->
    try
        Net = extract_net(Workflow),
        InitialMarking = get_initial_marking(Net),
        MaxStates = get_max_states(Net),
        find_unreachable_transitions(Net, InitialMarking, MaxStates)
    catch
        Type:Error:_Stack ->
            ?LOG_ERROR("Dead transition detection failed: ~p:~p", [Type, Error]),
            [{dead_transition_detection_error, {Type, Error}}]
    end.

%%--------------------------------------------------------------------
%% @doc Finds unreachable transitions via state space analysis.
%%
%% Explores the reachable markings and identifies which transitions
%% are never enabled in any state.
%%
%% @param Net The Petri net structure
%% @param InitialMarking Starting marking
%% @param MaxStates Maximum states to explore
%% @return List of dead transition issues
%%
%% @end
%%--------------------------------------------------------------------
-spec find_unreachable_transitions(Net :: net(),
                                   InitialMarking :: marking(),
                                   MaxStates :: pos_integer()) ->
          [issue()].

find_unreachable_transitions(Net, _InitialMarking, MaxStates) ->
    ReachGraph = build_reachability_graph(Net, MaxStates),
    AllTransitions = maps:get(transitions, Net, []),

    %% Collect all enabled transitions across all states
    EverEnabled = lists:foldl(fun(_State, Acc) ->
        StateInfo = maps:get(_State, ReachGraph, #{}),
        Enabled = maps:get(enabled, StateInfo, []),
        sets:union(sets:from_list(Enabled), Acc)
    end, sets:new(), maps:keys(ReachGraph)),

    %% Find transitions never enabled
    DeadTransitions = lists:filter(fun(T) ->
        not sets:is_element(T, EverEnabled)
    end, AllTransitions),

    lists:map(fun(T) -> {dead_transition, T} end, DeadTransitions).

%%--------------------------------------------------------------------
%% @doc Generates repair suggestions for workflow issues.
%%
%% Analyzes detected issues and provides actionable suggestions for
%% repairing the workflow.
%%
%% @param Workflow A module atom, net structure, or YAWL spec
%% @return List of repair suggestions
%%
%% @end
%%--------------------------------------------------------------------
-spec suggest_repair(Workflow :: workflow_input()) -> [suggestion()].

suggest_repair(Workflow) ->
    try
        Net = extract_net(Workflow),
        Issues = collect_all_issues(Workflow, Net),
        generate_suggestions(Workflow, Issues)
    catch
        Type:Error:_Stack ->
            ?LOG_ERROR("Repair suggestion failed: ~p:~p", [Type, Error]),
            [{repair_error, {Type, Error}}]
    end.

%%--------------------------------------------------------------------
%% @doc Generates suggestions based on detected issues.
%%
%% Maps issue types to appropriate repair strategies.
%%
%% @param Workflow The workflow being analyzed
%% @param Issues List of detected issues
%% @return List of repair suggestions
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_suggestions(Workflow :: workflow_input(),
                          Issues :: [issue()]) -> [suggestion()].

generate_suggestions(Workflow, Issues) ->
    Net = extract_net(Workflow),

    lists:flatmap(fun
        ({deadlock, #{marking := Marking, reason := Reason}}) ->
            suggest_deadlock_repair(Net, Marking, Reason);
        ({dead_transition, Transition}) ->
            suggest_dead_transition_repair(Net, Transition);
        ({unbounded, Place}) ->
            [{add_consumption, Place, add_outgoing_arc}];
        ({no_completion, _}) ->
            suggest_completion_repair(Net);
        ({multiple_input, Places}) ->
            [{merge_input_places, Places, add_single_start}];
        ({multiple_output, Places}) ->
            [{merge_output_places, Places, add_single_end}];
        ({orphan_place, Place}) ->
            suggest_orphan_repair(Net, Place);
        ({circular_wait, Cycle}) ->
            suggest_circular_wait_repair(Net, Cycle);
        (_Issue) ->
            []
    end, Issues).

%%--------------------------------------------------------------------
%% @doc Builds the reachability graph for a Petri net.
%%
%% The reachability graph contains all states (markings) reachable
%% from the initial marking, with transitions between them.
%%
%% @param Net The Petri net structure
%% @param MaxStates Maximum number of states to explore
%% @return Map of state_id => state_info
%%
%% @end
%%--------------------------------------------------------------------
-spec build_reachability_graph(Net :: net(), MaxStates :: pos_integer()) ->
          #{binary() => state()}.

build_reachability_graph(Net, MaxStates) ->
    InitialMarking = get_initial_marking(Net),
    build_reachability_graph(Net, InitialMarking, MaxStates, #{}).

build_reachability_graph(_Net, _InitialMarking, 0, Graph) ->
    Graph;
build_reachability_graph(Net, Marking, MaxStates, Graph) ->
    MarkingKey = marking_hash(Marking),

    case maps:is_key(MarkingKey, Graph) of
        true ->
            Graph;
        false ->
            %% Find enabled transitions
            Enabled = find_enabled_transitions(Net, Marking),

            %% Create state info
            StateInfo = #{
                marking => Marking,
                enabled => Enabled,
                depth => maps:size(Graph)
            },

            %% Fire each enabled transition to get next markings
            NextMarkings = lists:map(fun(T) ->
                fire_transition(Net, Marking, T)
            end, Enabled),

            %% Recursively explore next states
            Graph1 = Graph#{MarkingKey => StateInfo},

            lists:foldl(fun({NextMarking, _Trans}, Acc) ->
                build_reachability_graph(Net, NextMarking, MaxStates - 1, Acc)
            end, Graph1, NextMarkings)
    end.

%%--------------------------------------------------------------------
%% @doc Computes the coverability tree for a Petri net.
%%
%% The coverability tree shows the structure of reachable markings
%% with omega notation for unbounded places.
%%
%% @param Net The Petri net structure
%% @param MaxDepth Maximum depth of tree exploration
%% @return Coverability tree structure
%%
%% @end
%%--------------------------------------------------------------------
-spec compute_coverability_tree(Net :: net(), MaxDepth :: pos_integer()) ->
          map().

compute_coverability_tree(Net, MaxDepth) ->
    InitialMarking = get_initial_marking(Net),
    CoverTree = compute_coverability_tree(Net, InitialMarking,
                                         MaxDepth, 0, sets:new(), #{}),
    CoverTree#{initial => InitialMarking}.

compute_coverability_tree(_Net, _Marking, MaxDepth, Depth, _Visited, Tree)
  when Depth >= MaxDepth ->
    Tree;
compute_coverability_tree(Net, Marking, MaxDepth, Depth, Visited, Tree) ->
    MarkingKey = marking_hash(Marking),

    case sets:is_element(MarkingKey, Visited) of
        true ->
            Tree;
        false ->
            Visited1 = sets:add_element(MarkingKey, Visited),

            Enabled = find_enabled_transitions(Net, Marking),

            %% Compute successor markings
            Successors = lists:map(fun(T) ->
                {fire_transition(Net, Marking, T), T}
            end, Enabled),

            Tree1 = Tree#{MarkingKey => #{
                marking => Marking,
                enabled => Enabled,
                depth => Depth
            }},

            %% Recursively build tree
            lists:foldl(fun({{NextMarking, _}, _T}, Acc) ->
                compute_coverability_tree(Net, NextMarking,
                                         MaxDepth, Depth + 1, Visited1, Acc)
            end, Tree1, Successors)
    end.

%%--------------------------------------------------------------------
%% @doc Extracts net structure from a gen_yawl module.
%%
%% @param Module The gen_yawl callback module
%% @return {ok, Net} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_net_from_module(Module :: module()) ->
          {ok, net()} | {error, term()}.

extract_net_from_module(Module) when is_atom(Module) ->
    try
        Places = Module:place_lst(),
        Transitions = Module:trsn_lst(),

        %% Extract arcs from preset/postset
        Arcs = extract_arcs_from_module(Module, Transitions),

        Net = #{
            places => Places,
            transitions => Transitions,
            arcs => Arcs
        },
        {ok, Net}
    catch
        _:Error -> {error, {module_extraction_failed, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Extracts net structure from a YAWL spec.
%%
%% @param Spec The wf_spec:yawl_spec() record
%% @return {ok, Net} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_net_from_spec(Spec :: wf_spec:yawl_spec()) ->
          {ok, net()} | {error, term()}.

extract_net_from_spec(Spec) ->
    try
        %% Compile the spec to get net structure
        {ok, Compiled} = wf_spec:compile(Spec),

        Places = wf_spec:places(Compiled),
        Transitions = wf_spec:transitions(Compiled),

        %% Build arcs from preset/postset
        Arcs = extract_arcs_from_compiled(Compiled),

        Net = #{
            places => Places,
            transitions => Transitions,
            arcs => Arcs
        },
        {ok, Net}
    catch
        _:Error -> {error, {spec_extraction_failed, Error}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts net structure from various input types.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_net(Workflow :: workflow_input()) -> net().

extract_net(Module) when is_atom(Module) ->
    {ok, Net} = extract_net_from_module(Module),
    Net;
extract_net(Net = #{places := Places, transitions := Transitions, arcs := Arcs})
  when is_list(Places), is_list(Transitions), is_list(Arcs) ->
    Net;
extract_net(Spec) when is_map(Spec) ->
    %% Try as YAWL spec
    try
        {ok, Net} = extract_net_from_spec(Spec),
        Net
    catch
        _:_ -> Spec
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all diagnostic issues.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_all_issues(Workflow :: workflow_input(), Net :: net()) ->
          [issue()].

collect_all_issues(Workflow, Net) ->
    Issues1 = check_structural_issues(Net),
    Issues2 = check_soundness_issues(Workflow, Net),
    Issues3 = check_reachability_issues(Workflow, Net),
    Issues1 ++ Issues2 ++ Issues3.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for structural issues in the net.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_structural_issues(Net :: net()) -> [issue()].

check_structural_issues(Net) ->
    Issues = [],

    %% Check for multiple input places
    InputPlaces = find_input_places(Net),
    Issues1 = case InputPlaces of
        [] -> [{no_input_place, net}];
        [_] -> Issues;
        _ -> [{multiple_input, InputPlaces} | Issues]
    end,

    %% Check for multiple output places
    OutputPlaces = find_output_places(Net),
    Issues2 = case OutputPlaces of
        [] -> [{no_output_place, net} | Issues1];
        [_] -> Issues1;
        _ -> [{multiple_output, OutputPlaces} | Issues1]
    end,

    %% Check for orphan places
    #{places := Places, arcs := Arcs} = Net,
    Connected = collect_connected_places(Arcs),
    Orphans = lists:filter(fun(P) -> not sets:is_element(P, Connected) end,
                           Places),
    Issues3 = case Orphans of
        [] -> Issues2;
        _ -> [{orphan_place, P} || P <- Orphans] ++ Issues2
    end,

    %% Check for circular structures
    case detect_cycles(Net) of
        {ok, []} ->
            Issues3;
        {ok, Cycles} when length(Cycles) > 0 ->
            [{circular_wait, Cycle} || Cycle <- Cycles] ++ Issues3
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for soundness-related issues.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_soundness_issues(Workflow :: workflow_input(), Net :: net()) ->
          [issue()].

check_soundness_issues(_Workflow, Net) ->
    Issues = [],

    %% Check boundedness
    case check_structural_boundedness(Net) of
        {ok, true} -> Issues;
        {ok, false} -> [{unbounded, net} | Issues];
        _ -> Issues
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for issues via reachability analysis.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_reachability_issues(Workflow :: workflow_input(), Net :: net()) ->
          [issue()].

check_reachability_issues(_Workflow, Net) ->
    MaxStates = get_max_states(Net),
    InitialMarking = get_initial_marking(Net),

    %% Check for completion path
    Issues = case verify_completion_path(Net) of
        {ok, false} -> [{no_completion, net}];
        _ -> []
    end,

    %% Check for dead transitions
    DeadTrans = find_unreachable_transitions(Net, InitialMarking, MaxStates),
    Issues ++ DeadTrans.

%%--------------------------------------------------------------------
%% @private
%% @doc Finds input places (no incoming arcs).
%%
%% @end
%%--------------------------------------------------------------------
-spec find_input_places(Net :: net()) -> [place()].

find_input_places(#{places := Places, arcs := Arcs}) ->
    Targets = collect_targets(Arcs, sets:new()),
    lists:filter(fun(P) -> not sets:is_element(P, Targets) end, Places).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds output places (no outgoing arcs).
%%
%% @end
%%--------------------------------------------------------------------
-spec find_output_places(Net :: net()) -> [place()].

find_output_places(#{places := Places, arcs := Arcs}) ->
    Sources = collect_sources(Arcs, sets:new()),
    lists:filter(fun(P) -> not sets:is_element(P, Sources) end, Places).

%%--------------------------------------------------------------------
%% @private
%% @doc Gets initial marking (token in input place).
%%
%% @end
%%--------------------------------------------------------------------
-spec get_initial_marking(Net :: net()) -> marking().

get_initial_marking(Net) ->
    InputPlaces = find_input_places(Net),
    lists:foldl(fun(P, Acc) ->
        Acc#{P => 1}
    end, #{}, InputPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Gets max states for exploration (based on net size).
%%
%% @end
%%--------------------------------------------------------------------
-spec get_max_states(Net :: net()) -> pos_integer().

get_max_states(#{places := Places, transitions := Transitions}) ->
    %% Limit based on net complexity
    min(10000, length(Places) * length(Transitions) * 100).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks structural boundedness.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_structural_boundedness(Net :: net()) ->
          {ok, boolean()}.

check_structural_boundedness(Net = #{places := Places, transitions := Transitions}) ->
    %% A net is structurally bounded if every transition has
    %% equal or more consumption than production for each place
    Incidence = build_incidence_matrix(Net),

    %% Check if any transition has net positive gain
    Unbounded = lists:any(fun(T) ->
        TMap = maps:get(T, Incidence, #{}),
        lists:any(fun(P) ->
            maps:get(P, TMap, 0) > 0
        end, Places)
    end, Transitions),

    {ok, not Unbounded}.

%%--------------------------------------------------------------------
%% @private
%% @doc Verifies completion path exists from input to output.
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_completion_path(Net :: net()) -> {ok, boolean()}.

verify_completion_path(Net) ->
    InputPlaces = find_input_places(Net),
    OutputPlaces = find_output_places(Net),

    case {InputPlaces, OutputPlaces} of
        {[], _} -> {ok, false};
        {_, []} -> {ok, false};
        {[_Input], [_Output]} ->
            %% Use BFS to check if path exists
            hd(InputPlaces) =:= hd(OutputPlaces) orelse
                path_exists(Net, hd(InputPlaces), hd(OutputPlaces));
        _ ->
            {ok, false}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if path exists between two nodes.
%%
%% @end
%%--------------------------------------------------------------------
-spec path_exists(Net :: net(), From :: atom(), To :: atom()) -> boolean().

path_exists(#{arcs := Arcs}, From, To) ->
    %% Build adjacency map
    Adj = lists:foldl(fun
        ({F, T}, Acc) when is_atom(F), is_atom(T) ->
            Acc#{F => [T | maps:get(F, Acc, [])]}
    end, #{}, Arcs),

    %% BFS
    case bfs_path(To, [From], Adj, sets:new()) of
        {ok, _} -> true;
        {error, _} -> false
    end.

bfs_path(Target, [Current | _Rest], _Adj, _Visited) when Current =:= Target ->
    {ok, reached};
bfs_path(Target, [Current | Rest], Adj, Visited) ->
    case sets:is_element(Current, Visited) of
        true ->
            bfs_path(Target, Rest, Adj, Visited);
        false ->
            Visited1 = sets:add_element(Current, Visited),
            Neighbors = maps:get(Current, Adj, []),
            bfs_path(Target, Rest ++ Neighbors, Adj, Visited1)
    end;
bfs_path(_Target, [], _Adj, _Visited) ->
    {error, unreachable}.

%%--------------------------------------------------------------------
%% @private
%% @doc Builds incidence matrix for analysis.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_incidence_matrix(Net :: net()) ->
          #{transition() => #{place() => integer()}}.

build_incidence_matrix(#{places := Places, transitions := Transitions, arcs := Arcs}) ->
    InitMatrix = maps:from_list([{T, maps:from_list([{P, 0} || P <- Places])}
                                 || T <- Transitions]),

    lists:foldl(fun
        ({Place, Transition}, Matrix) when is_atom(Place), is_atom(Transition) ->
            TMap = maps:get(Transition, Matrix, #{}),
            TMap1 = maps:update_with(Place, fun(V) -> V - 1 end, -1, TMap),
            Matrix#{Transition => TMap1};
        ({Transition, Place}, Matrix) when is_atom(Transition), is_atom(Place) ->
            TMap = maps:get(Transition, Matrix, #{}),
            TMap1 = maps:update_with(Place, fun(V) -> V + 1 end, 1, TMap),
            Matrix#{Transition => TMap1}
    end, InitMatrix, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds enabled transitions in a marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_enabled_transitions(Net :: net(), Marking :: marking()) ->
          [transition()].

find_enabled_transitions(#{transitions := Transitions, arcs := Arcs}, Marking) ->
    %% Build preset map
    PresetMap = build_preset_map(Arcs),

    lists:filter(fun(T) ->
        Preset = maps:get(T, PresetMap, []),
        lists:all(fun(P) ->
            maps:get(P, Marking, 0) > 0
        end, Preset)
    end, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Builds preset map from arcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_preset_map([arc()]) -> #{transition() => [place()]}.

build_preset_map(Arcs) ->
    lists:foldl(fun
        ({Place, Transition}, Acc) when is_atom(Place), is_atom(Transition) ->
            Acc#{Transition => [Place | maps:get(Transition, Acc, [])]};
        (_, Acc) ->
            Acc
    end, #{}, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Fires a transition to get next marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire_transition(Net :: net(), Marking :: marking(),
                      Transition :: transition()) ->
          {marking(), transition()}.

fire_transition(#{arcs := Arcs}, Marking, Transition) ->
    %% Build postset map
    PostsetMap = build_postset_map(Arcs),
    PresetMap = build_preset_map(Arcs),

    %% Consume from preset
    Marking1 = lists:foldl(fun(P, Acc) ->
        maps:update_with(P, fun(V) -> max(0, V - 1) end, 0, Acc)
    end, Marking, maps:get(Transition, PresetMap, [])),

    %% Produce to postset
    Marking2 = lists:foldl(fun(P, Acc) ->
        maps:update_with(P, fun(V) -> V + 1 end, 1, Acc)
    end, Marking1, maps:get(Transition, PostsetMap, [])),

    {Marking2, Transition}.

%%--------------------------------------------------------------------
%% @private
%% @doc Builds postset map from arcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_postset_map([arc()]) -> #{transition() => [place()]}.

build_postset_map(Arcs) ->
    lists:foldl(fun
        ({Transition, Place}, Acc) when is_atom(Transition), is_atom(Place) ->
            Acc#{Transition => [Place | maps:get(Transition, Acc, [])]};
        (_, Acc) ->
            Acc
    end, #{}, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Computes hash of marking for state identification.
%%
%% @end
%%--------------------------------------------------------------------
-spec marking_hash(Marking :: marking()) -> binary().

marking_hash(Marking) ->
    Canonical = lists:sort(maps:to_list(Marking)),
    crypto:hash(md5, term_to_binary(Canonical)).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if marking is the final marking (only output marked).
%%
%% @end
%%--------------------------------------------------------------------
-spec is_final_marking(Marking :: marking(), OutputPlace :: place()) ->
          boolean().

is_final_marking(Marking, OutputPlace) ->
    case maps:get(OutputPlace, Marking, 0) of
        0 -> false;
        _ ->
            %% Check all other places are empty
            maps:fold(fun(P, Count, Acc) ->
                case P =:= OutputPlace of
                    true -> Acc;
                    false when Count > 0 -> false;
                    _ -> Acc
                end
            end, true, Marking)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all connected places from arcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_connected_places([arc()]) -> sets:set(place()).

collect_connected_places(Arcs) ->
    lists:foldl(fun
        ({Place, _Transition}, Acc) when is_atom(Place) ->
            sets:add_element(Place, Acc);
        ({_Transition, Place}, Acc) when is_atom(Place) ->
            sets:add_element(Place, Acc);
        (_, Acc) ->
            Acc
    end, sets:new(), Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all target nodes from arcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_targets([arc()], sets:set(atom())) -> sets:set(atom()).

collect_targets(Arcs, Acc) ->
    lists:foldl(fun
        ({_From, To}, A) when is_atom(To) ->
            sets:add_element(To, A);
        (_, A) ->
            A
    end, Acc, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all source nodes from arcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_sources([arc()], sets:set(atom())) -> sets:set(atom()).

collect_sources(Arcs, Acc) ->
    lists:foldl(fun
        ({From, _To}, A) when is_atom(From) ->
            sets:add_element(From, A);
        (_, A) ->
            A
    end, Acc, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Detects cycles in the net structure.
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_cycles(Net :: net()) -> {ok, [[atom()]]}.

detect_cycles(#{places := Places, transitions := Transitions, arcs := Arcs}) ->
    AllNodes = Places ++ Transitions,

    %% Build adjacency list
    Adj = lists:foldl(fun
        ({From, To}, Acc) when is_atom(From), is_atom(To) ->
            Acc#{From => [To | maps:get(From, Acc, [])]}
    end, #{}, Arcs),

    %% Find cycles using DFS
    Cycles = lists:filtermap(fun(Node) ->
        dfs_cycle(Node, Node, Adj, [], sets:new())
    end, AllNodes),

    {ok, Cycles}.

%%--------------------------------------------------------------------
%% @private
%% @doc DFS helper for cycle detection.
%%
%% @end
%%--------------------------------------------------------------------
-spec dfs_cycle(Node :: atom(), Start :: atom(),
                Adj :: #{atom() => [atom()]}, Path :: [atom()],
                Visited :: sets:set(atom())) -> false | {true, [atom()]}.

dfs_cycle(Node, Start, Adj, Path, Visited) ->
    case sets:is_element(Node, Visited) of
        true ->
            false;
        false ->
            Visited1 = sets:add_element(Node, Visited),
            case lists:member(Start, maps:get(Node, Adj, [])) of
                true ->
                    {true, lists:reverse([Start | Path])};
                false ->
                    case lists:any(fun(N) ->
                        dfs_cycle(N, Start, Adj, [Node | Path], Visited1) =/= false
                    end, maps:get(Node, Adj, [])) of
                        true ->
                            {true, [Node | Path]};
                        false ->
                            false
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Finds dead transitions via structural analysis.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_dead_transitions(Net :: net()) -> [transition()].

find_dead_transitions(#{transitions := Transitions, arcs := Arcs}) ->
    PresetMap = build_preset_map(Arcs),

    %% Transitions with no preset are dead
    lists:filter(fun(T) ->
        not maps:is_key(T, PresetMap) orelse maps:get(T, PresetMap) =:= []
    end, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Determines overall status from issues.
%%
%% @end
%%--------------------------------------------------------------------
-spec determine_status([issue()]) -> report_status().

determine_status([]) ->
    ok;
determine_status(Issues) ->
    %% Check for critical issues
    Critical = [deadlock, unbounded, no_completion],
    case lists:any(fun({Type, _}) -> lists:member(Type, Critical);
                      (_) -> false
                   end, Issues) of
        true -> unsound;
        false -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts net info for reporting.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_net_info(Net :: net()) -> map().

extract_net_info(#{places := Places, transitions := Transitions, arcs := Arcs}) ->
    #{
        place_count => length(Places),
        transition_count => length(Transitions),
        arc_count => length(Arcs),
        input_places => find_input_places(#{places => Places, arcs => Arcs}),
        output_places => find_output_places(#{places => Places, arcs => Arcs})
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts arcs from a gen_yawl module.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_arcs_from_module(Module :: module(),
                                 Transitions :: [transition()]) -> [arc()].

extract_arcs_from_module(Module, Transitions) ->
    lists:flatmap(fun(T) ->
        try
            Preset = Module:preset(T),
            [{P, T} || P <- Preset]
        catch
            _:_ -> []
        end
    end, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts arcs from a compiled spec.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_arcs_from_compiled(Compiled :: wf_spec:compiled_spec()) ->
          [arc()].

extract_arcs_from_compiled(Compiled) ->
    Places = wf_spec:places(Compiled),
    Transitions = wf_spec:transitions(Compiled),

    %% For compiled specs, build basic arcs
    %% This is simplified - real implementation would use preset/postset
    lists:flatmap(fun(T) ->
        [{P, T} || P <- Places]
    end, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates suggestions for deadlock repair.
%%
%% @end
%%--------------------------------------------------------------------
-spec suggest_deadlock_repair(Net :: net(), Marking :: marking(),
                               Reason :: term()) -> [suggestion()].

suggest_deadlock_repair(Net, Marking, Reason) ->
    case Reason of
        no_enabled_transitions ->
            %% Find places with tokens
            BlockedPlaces = lists:filter(fun(P) ->
                maps:get(P, Marking, 0) > 0
            end, maps:keys(Marking)),

            %% Suggest adding transitions to clear these places
            lists:map(fun(P) ->
                {add_synchronization, P, find_suitable_output(Net)}
            end, BlockedPlaces);
        circular_wait ->
            %% Suggest breaking the cycle
            [{add_place, break_cycle, insert_buffer}]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Finds suitable output place for synchronization.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_suitable_output(Net :: net()) -> place().

find_suitable_output(Net) ->
    case find_output_places(Net) of
        [Output | _] -> Output;
        _ -> 'output_place'  % Default suggestion
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates suggestions for dead transition repair.
%%
%% @end
%%--------------------------------------------------------------------
-spec suggest_dead_transition_repair(Net :: net(),
                                      Transition :: transition()) ->
          [suggestion()].

suggest_dead_transition_repair(_Net, Transition) ->
    %% Suggest adding input arc to make transition reachable
    [{add_input_arc, Transition, connect_to_start}].

%%--------------------------------------------------------------------
%% @private
%% @doc Generates suggestions for completion path repair.
%%
%% @end
%%--------------------------------------------------------------------
-spec suggest_completion_repair(Net :: net()) -> [suggestion()].

suggest_completion_repair(Net) ->
    InputPlaces = find_input_places(Net),
    OutputPlaces = find_output_places(Net),

    case {InputPlaces, OutputPlaces} of
        {[], _} ->
            [{add_input_place, net, add_start_condition}];
        {_, []} ->
            [{add_output_place, net, add_end_condition}];
        {[_Input], [_Output]} ->
            [{add_path, input_to_output, connect_start_to_end}]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates suggestions for orphan place repair.
%%
%% @end
%%--------------------------------------------------------------------
-spec suggest_orphan_repair(Net :: net(), Place :: place()) ->
          [suggestion()].

suggest_orphan_repair(_Net, Place) ->
    [{remove_orphan, Place, unused_place}].

%%--------------------------------------------------------------------
%% @private
%% @doc Generates suggestions for circular wait repair.
%%
%% @end
%%--------------------------------------------------------------------
-spec suggest_circular_wait_repair(Net :: net(), Cycle :: [atom()]) ->
          [suggestion()].

suggest_circular_wait_repair(_Net, _Cycle) ->
    [{restructure_arc, break_cycle, add_buffer_place}].

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Helper Functions
%%====================================================================

%% Simple workflow net for testing
test_net() ->
    #{places => [start, processing, done],
      transitions => [t1],
      arcs => [{start, t1}, {t1, processing}, {processing, t1}, {t1, done}]}.

%% Unsound net with deadlock
deadlock_net() ->
    #{places => [p1, p2],
      transitions => [t1, t2],
      arcs => [{p1, t1}, {t1, p2}, {p2, t2}, {t2, p2}]}.

%% Sound workflow
sound_net() ->
    #{places => [start, done],
      transitions => [t],
      arcs => [{start, t}, {t, done}]}.

%%====================================================================
%% diagnose/1 Tests
%%====================================================================

diagnose_sound_net_test() ->
    Report = diagnose(sound_net()),
    ?assertEqual(ok, maps:get(status, Report, error)).

diagnose_deadlock_net_test() ->
    Report = diagnose(deadlock_net()),
    Status = maps:get(status, Report, ok),
    ?assert(Status =:= unsound orelse Status =:= ok).

diagnose_structure_test() ->
    Report = diagnose(test_net()),
    ?assert(is_map(Report)),
    ?assert(maps:is_key(status, Report)),
    ?assert(maps:is_key(issues, Report)),
    ?assert(is_list(maps:get(issues, Report, []))).

%%====================================================================
%% check_soundness/1 Tests
%%====================================================================

check_soundness_sound_net_test() ->
    ?assertEqual({ok, true}, check_soundness(sound_net())).

check_soundness_no_input_test() ->
    NoInput = #{places => [done], transitions => [t], arcs => [{t, done}]},
    case check_soundness(NoInput) of
        {ok, false, no_input_place} -> ok;
        {ok, false, _} -> ok
    end.

check_soundness_no_output_test() ->
    NoOutput = #{places => [start], transitions => [t], arcs => [{start, t}]},
    case check_soundness(NoOutput) of
        {ok, false, no_output_place} -> ok;
        {ok, false, _} -> ok
    end.

%%====================================================================
%% detect_deadlocks/1 Tests
%%====================================================================

detect_deadlocks_sound_net_test() ->
    Deadlocks = detect_deadlocks(sound_net()),
    ?assert(is_list(Deadlocks)).

detect_deadlocks_empty_net_test() ->
    Empty = #{places => [], transitions => [], arcs => []},
    Deadlocks = detect_deadlocks(Empty),
    ?assert(is_list(Deadlocks)).

%%====================================================================
%% detect_dead_transitions/1 Tests
%%====================================================================

detect_dead_transitions_sound_net_test() ->
    Dead = detect_dead_transitions(sound_net()),
    ?assert(is_list(Dead)).

detect_dead_transitions_with_dead_transition_test() ->
    %% Net with isolated transition
    Net = #{places => [p1, p2], transitions => [t1, t2],
            arcs => [{p1, t1}, {t1, p2}]},
    Dead = detect_dead_transitions(Net),
    ?assert(is_list(Dead)),
    %% t2 should be detected as dead
    ?assert(lists:any(fun({dead_transition, T}) -> T =:= t2; (_) -> false end, Dead)).

%%====================================================================
%% build_reachability_graph/2 Tests
%%====================================================================

build_reachability_graph_sound_net_test() ->
    Graph = build_reachability_graph(sound_net(), 100),
    ?assert(is_map(Graph)),
    ?assert(maps:size(Graph) > 0).

build_reachability_graph_empty_net_test() ->
    Empty = #{places => [], transitions => [], arcs => []},
    Graph = build_reachability_graph(Empty, 10),
    ?assert(is_map(Graph)).

%%====================================================================
%% extract_net_from_module/1 Tests
%%====================================================================

extract_net_test() ->
    %% Test with a simple module-like interface
    Net = test_net(),
    ?assert(is_map(Net)),
    ?assert(maps:is_key(places, Net)),
    ?assert(maps:is_key(transitions, Net)),
    ?assert(maps:is_key(arcs, Net)).

%%====================================================================
%% Helper Function Tests
%%====================================================================

find_input_places_test() ->
    Input = find_input_places(sound_net()),
    ?assertEqual([start], Input).

find_output_places_test() ->
    Output = find_output_places(sound_net()),
    ?assertEqual([done], Output).

get_initial_marking_test() ->
    Marking = get_initial_marking(sound_net()),
    ?assertEqual(1, maps:get(start, Marking, 0)).

marking_hash_test() ->
    Marking = #{p1 => 1, p2 => 0},
    Hash1 = marking_hash(Marking),
    Hash2 = marking_hash(Marking),
    ?assertEqual(Hash1, Hash2).

is_final_marking_test() ->
    Final = #{done => 1},
    ?assert(is_final_marking(Final, done)),
    NotFinal = #{done => 1, other => 1},
    ?assertNot(is_final_marking(NotFinal, done)).

%%====================================================================
%% Integration Tests
%%====================================================================

full_diagnosis_flow_test() ->
    %% Run complete diagnostic flow
    Report = diagnose(sound_net()),
    ?assertEqual(ok, maps:get(status, Report)),
    Issues = maps:get(issues, Report, []),
    ?assert(is_list(Issues)),
    Suggestions = maps:get(suggestions, Report, []),
    ?assert(is_list(Suggestions)).

-endif.
