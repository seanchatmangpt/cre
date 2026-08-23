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
%% @doc Workflow Net Composition Validator
%%
%% This module provides comprehensive validation for composed workflow
%% patterns. It ensures that when multiple workflow patterns are
%% composed together, the resulting workflow is valid and maintains
%% all WF-Net properties (soundness, liveness, boundedness).
%%
%% <h3>Composition Validation</h3>
%%
%% <ul>
%%   <li><b>validate_composition/2:</b> Validates pattern composition with options</li>
%%   <li><b>check_soundness/1:</b> Verifies composed workflow soundness</li>
%%   <li><b>check_connectedness/1:</b> Verifies single connected component</li>
%%   <li><b>find_conflicts/2:</b> Finds naming and conflict issues</li>
%%   <li><b>generate_validation_report/1:</b> Produces detailed report</li>
%% </ul>
%%
%% <h3>Composition Rules Checked</h3>
%%
%% The validator enforces these composition rules:
%% <ol>
%%   <li>Unique names: No duplicate place or transition names after composition</li>
%%   <li>Proper connections: All preset/postset references must be valid</li>
%%   <li>Single start/end: Composed workflow has exactly one start and end place</li>
%%   <li>Soundness preservation: Composition maintains soundness properties</li>
%%   <li>Connectedness: All components are reachable from the start</li>
%%   <li>No deadlock: No unreachable states where no transition can fire</li>
%% </ol>
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create workflow specs
%% Spec1 = wfnet_compose:task(validate_order),
%% Spec2 = wfnet_compose:task(process_payment),
%% Spec3 = wfnet_compose:task(ship_order),
%%
%% %% Compose workflows
%% Sequential = wfnet_compose:sequence(Spec1, Spec2),
%% Composed = wfnet_compose:sequence(Sequential, Spec3),
%%
%% %% Validate composition
%% Result = wfnet_compose_validator:validate_composition(Composed, #{}),
%% case Result of
%%     {ok, []} -> io:format("Valid composition~n");
%%     {ok, Warnings} -> io:format("Valid with warnings: ~p~n", [Warnings]);
%%     {error, Errors} -> io:format("Invalid: ~p~n", [Errors])
%% end.
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_compose_validator).

%%====================================================================
%% Exports
%%====================================================================

%% Main validation API
-export([
    validate_composition/2,
    validate_composition/1
]).

%% Soundness and structural checks
-export([
    check_soundness/1,
    check_connectedness/1,
    find_conflicts/2,
    generate_validation_report/1
]).

%% Internal validation helpers
-export([
    check_unique_names/1,
    check_connection_validity/1,
    check_start_end_validity/1,
    check_boundedness/1,
    check_liveness/1,
    detect_cycles/1
]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the workflow net.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the workflow net.
%%--------------------------------------------------------------------
-type trsn() :: atom().

%%--------------------------------------------------------------------
%% @doc Workflow specification map.
%%--------------------------------------------------------------------
-type workflow_spec() :: #{
    places := [place()],
    transitions := [trsn()],
    start_place := place(),
    end_place := place(),
    preset => #{trsn() => [place()]},
    postset => #{trsn() => [place()]},
    optional => map()
}.

%%--------------------------------------------------------------------
%% @doc Validation options.
%%
%% Options for controlling validation behavior:
%% - check_soundness: boolean() - Whether to perform soundness analysis (default: true)
%% - check_boundedness: boolean() - Whether to check boundedness (default: true)
%% - max_depth: pos_integer() - Maximum depth for reachability analysis (default: 1000)
%% - strict_mode: boolean() - Enable stricter validation rules (default: false)
%%--------------------------------------------------------------------
-type validation_options() :: #{
    check_soundness => boolean(),
    check_boundedness => boolean(),
    max_depth => pos_integer(),
    strict_mode => boolean()
}.

%%--------------------------------------------------------------------
%% @doc Validation error category.
%%--------------------------------------------------------------------
-type error_category() ::
    naming_conflict        %% Duplicate names in composition
    | invalid_reference    %% References to non-existent places/transitions
    | soundness_violation  %% Workflow soundness issues
    | connectedness_error  %% Disconnected components
    | boundedness_error    %% Unbounded places
    | liveness_error       %% Dead transitions or deadlocks
    | structural_error.    %% General structural issues

%%--------------------------------------------------------------------
%% @doc A validation error.
%%--------------------------------------------------------------------
-type validation_error() :: #{
    category := error_category(),
    severity := error,
    message := binary(),
    location => place() | trsn(),
    code := atom(),
    details => map()
}.

%%--------------------------------------------------------------------
%% @doc A validation warning.
%%--------------------------------------------------------------------
-type validation_warning() :: #{
    category := atom(),
    severity := warning,
    message := binary(),
    location => place() | trsn(),
    code := atom()
}.

%%--------------------------------------------------------------------
%% @doc Validation result.
%%--------------------------------------------------------------------
-type validation_result() :: {ok, [validation_warning()]} | {error, [validation_error()]}.

%%--------------------------------------------------------------------
%% @doc Conflict information from find_conflicts/2.
%%--------------------------------------------------------------------
-type conflict_info() :: #{
    type := atom(),
    conflicts := [atom()],
    description := binary()
}.

%%--------------------------------------------------------------------
%% @doc Validation report from generate_validation_report/1.
%%--------------------------------------------------------------------
-type validation_report() :: #{
    valid := boolean(),
    errors := [validation_error()],
    warnings := [validation_warning()],
    summary := binary(),
    details => map()
}.

%% Export types
-export_type([
    workflow_spec/0,
    validation_options/0,
    validation_result/0,
    validation_error/0,
    validation_warning/0,
    error_category/0,
    conflict_info/0,
    validation_report/0
]).

%%====================================================================
%% Main Validation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a composed workflow specification with options.
%%
%% Performs comprehensive validation including:
%% - Unique name checking
%% - Connection validity
%% - Soundness verification
%% - Connectedness analysis
%% - Liveness checking
%% - Boundedness verification
%%
%% === Example ===
%% ```erlang
%% Options => #{
%%     check_soundness => true,
%%     check_boundedness => true,
%%     max_depth => 500,
%%     strict_mode => false
%% },
%% Result = wfnet_compose_validator:validate_composition(Spec, Options).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_composition(workflow_spec(), validation_options()) -> validation_result().

validate_composition(Spec, Options) when is_map(Spec), is_map(Options) ->
    %% Collect all validation results
    AllErrors = lists:flatten([
        check_required_fields(Spec),
        check_unique_names(Spec),
        check_connection_validity(Spec),
        check_start_end_validity(Spec)
    ]),

    %% Separate errors and warnings from initial checks
    {Errors1, Warnings1} = lists:partition(
        fun(#{severity := Sev}) -> Sev =:= error end,
        AllErrors
    ),

    %% Only run deeper analysis if no structural errors found
    {Errors2, Warnings2} = case Errors1 of
        [] ->
            SoundnessErrors = case maps:get(check_soundness, Options, true) of
                true -> check_soundness(Spec);
                false -> []
            end,

            BoundednessErrors = case maps:get(check_boundedness, Options, true) of
                true -> check_boundedness(Spec);
                false -> []
            end,

            LivenessErrors = check_liveness(Spec),
            ConnectednessErrors = check_connectedness(Spec),

            AllDeepErrors = SoundnessErrors ++ BoundednessErrors ++ LivenessErrors ++ ConnectednessErrors,
            lists:partition(fun(#{severity := Sev}) -> Sev =:= error end, AllDeepErrors);
        _ ->
            {Errors1, Warnings1}
    end,

    %% Combine all errors and warnings
    AllErrorsFinal = Errors2,
    AllWarningsFinal = lists:usort(Warnings2),

    case AllErrorsFinal of
        [] -> {ok, AllWarningsFinal};
        _ -> {error, AllErrorsFinal ++ AllWarningsFinal}
    end;

validate_composition(_Spec, _Options) ->
    {error, [#{category => structural_error,
               severity => error,
               message => <<"Invalid specification: must be a map">>,
               code => invalid_spec_type}]}.

%%--------------------------------------------------------------------
%% @doc Validates a composed workflow specification with default options.
%%
%% Uses default validation options:
%% - check_soundness: true
%% - check_boundedness: true
%% - max_depth: 1000
%% - strict_mode: false
%%
%% === Example ===
%% ```erlang
%% Result = wfnet_compose_validator:validate_composition(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_composition(workflow_spec()) -> validation_result().

validate_composition(Spec) ->
    DefaultOptions = #{
        check_soundness => true,
        check_boundedness => true,
        max_depth => 1000,
        strict_mode => false
    },
    validate_composition(Spec, DefaultOptions).

%%====================================================================
%% Soundness Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if the composed workflow is sound.
%%
%% Soundness means:
%% 1. Option to complete: The end state is reachable from the start state
%% 2. Proper completion: When the end place is marked, no other places are marked
%% 3. No dead transitions: Every transition can fire from some reachable state
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_compose_validator:check_soundness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_soundness(workflow_spec()) -> [validation_error() | validation_warning()].

check_soundness(#{start_place := Start, end_place := End,
                  places := Places, transitions := Transitions} = Spec) ->
    Errors = [],

    %% Check 1: Start and end must be different places
    Errors1 = case Start =:= End of
        true ->
            [#{category => soundness_violation,
              severity => error,
              message => iolist_to_binary([<<"Start and end place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"' must be different for sound workflows">>]),
              location => Start,
              code => start_end_same} | Errors];
        false ->
            Errors
    end,

    %% Check 2: Start and end must be in places list
    Errors2 = case {lists:member(Start, Places), lists:member(End, Places)} of
        {false, _} ->
            [#{category => invalid_reference,
              severity => error,
              message => iolist_to_binary([<<"Start place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"' not in places list">>]),
              location => Start,
              code => start_not_found} | Errors1];
        {_, false} ->
            [#{category => invalid_reference,
              severity => error,
              message => iolist_to_binary([<<"End place '">>,
                                          atom_to_binary(End, utf8),
                                          <<"' not in places list">>]),
              location => End,
              code => end_not_found} | Errors1];
        {true, true} ->
            Errors1
    end,

    %% Check 3: Option to complete - end must be reachable from start
    Errors3 = case is_reachable(Start, End, Spec) of
        true ->
            Errors2;
        false ->
            [#{category => soundness_violation,
              severity => error,
              message => iolist_to_binary([<<"End place '">>,
                                          atom_to_binary(End, utf8),
                                          <<"' is not reachable from start place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"'">>]),
              location => Start,
              code => end_not_reachable} | Errors2]
    end,

    %% Check 4: Proper completion - verify no transitions consume from end place
    Preset = maps:get(preset, Spec, #{}),
    EndConsumers = [T || T <- Transitions,
                       lists:member(End, maps:get(T, Preset, []))],
    Errors4 = case EndConsumers of
        [] ->
            Errors3;
        _ ->
            [#{category => soundness_violation,
              severity => error,
              message => iolist_to_binary([<<"Transitions consume from end place '">>,
                                          atom_to_binary(End, utf8),
                                          <<"', violating proper completion">>]),
              location => End,
              code => end_place_consumed,
              details => #{consumers => EndConsumers}} | Errors3]
    end,

    %% Check 5: No dead transitions (simplified check)
    Errors5 = check_dead_transitions(Spec),

    Errors4 ++ Errors5;

check_soundness(_Spec) ->
    [#{category => structural_error,
       severity => error,
       message => <<"Cannot check soundness: missing required fields">>,
       code => missing_fields_for_soundness}].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if target place is reachable from source place.
%%--------------------------------------------------------------------
-spec is_reachable(place(), place(), workflow_spec()) -> boolean().

is_reachable(Source, Target, #{places := Places, preset := Preset, postset := Postset}) ->
    %% Build reverse adjacency for backward search
    %% We want to find a path from Source to Target
    %% In Petri nets: place -> transition -> place -> transition ...
    Visited = sets:new(),
    ToVisit = queue:from_list([Source]),
    is_reachable_bfs(Target, ToVisit, Visited, Places, Preset, Postset).

%%--------------------------------------------------------------------
%% @private
%% @doc BFS helper for reachability checking.
%%--------------------------------------------------------------------
-spec is_reachable_bfs(place(), queue:queue(place()), sets:set(place()),
                       [place()], #{trsn() => [place()]}, #{trsn() => [place()]}) -> boolean().

is_reachable_bfs(Target, ToVisit, Visited, Places, Preset, Postset) ->
    case queue:out(ToVisit) of
        {empty, _} ->
            false;
        {{value, Current}, _RestQueue} when Current =:= Target ->
            true;
        {{value, Current}, RestQueue} ->
            case sets:is_element(Current, Visited) of
                true ->
                    is_reachable_bfs(Target, RestQueue, Visited, Places, Preset, Postset);
                false ->
                    Visited1 = sets:add_element(Current, Visited),
                    %% Find all transitions that can be reached from Current place
                    %% (transitions where Current is in preset)
                    NextTransitions = [T || T <- maps:keys(Postset),
                                          lists:member(Current, maps:get(T, Preset, []))],
                    %% Find all places reachable from those transitions
                    NextPlaces = lists:filter(fun(P) ->
                        lists:member(P, Places) andalso not sets:is_element(P, Visited1)
                    end, lists:flatten([maps:get(T, Postset, []) || T <- NextTransitions])),
                    ToVisit1 = lists:foldl(fun(P, Q) -> queue:in(P, Q) end, RestQueue, NextPlaces),
                    is_reachable_bfs(Target, ToVisit1, Visited1, Places, Preset, Postset)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for dead transitions that can never fire.
%%--------------------------------------------------------------------
-spec check_dead_transitions(workflow_spec()) -> [validation_error()].

check_dead_transitions(#{transitions := Transitions, preset := Preset}) ->
    %% A transition is potentially dead if its preset is empty (source only)
    %% or if its preset places cannot be marked
    lists:foldl(fun(T, Acc) ->
        case maps:get(T, Preset, []) of
            [] ->
                %% Source transition - not dead, it's the entry point
                Acc;
            PresetPlaces ->
                %% Check if any preset place can be marked
                %% This is a simplified check - a full implementation would
                %% trace the complete reachability graph
                case has_incoming_arcs(PresetPlaces, Preset) of
                    true ->
                        Acc;
                    false ->
                        [#{category => liveness_error,
                          severity => warning,
                          message => iolist_to_binary([<<"Transition '">>,
                                                      atom_to_binary(T, utf8),
                                                      <<"' may be dead (preset places have no incoming arcs)">>]),
                          location => T,
                          code => potentially_dead_transition} | Acc]
                end
        end
    end, [], Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if any of the places have incoming arcs.
%%--------------------------------------------------------------------
-spec has_incoming_arcs([place()], #{trsn() => [place()]}) -> boolean().

has_incoming_arcs(Places, Postset) ->
    %% Check if any transition produces to these places
    lists:any(fun(P) ->
        lists:any(fun(_, PostsetPlaces) ->
            lists:member(P, PostsetPlaces)
        end, maps:to_list(Postset))
    end, Places).

%%====================================================================
%% Connectedness Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Verifies the workflow is a single connected component.
%%
%% A valid workflow should have all nodes reachable from the start place.
%% Disconnected components indicate structural errors where parts of
%% the workflow cannot be reached during execution.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_compose_validator:check_connectedness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_connectedness(workflow_spec()) -> [validation_error() | validation_warning()].

check_connectedness(#{places := Places, transitions := Transitions,
                      start_place := Start} = Spec) ->
    %% Build full bipartite graph
    AllNodes = Places ++ Transitions,

    case AllNodes of
        [] ->
            [];
        _ ->
            %% Build adjacency list
            Adj = build_full_adjacency(Spec),

            %% Find connected component from start
            Visited = bfs_from(Start, Adj, sets:new()),

            %% Check for unvisited nodes
            Unvisited = [N || N <- AllNodes, not sets:is_element(N, Visited)],

            case Unvisited of
                [] ->
                    [];
                _ ->
                    %% Categorize unvisited nodes
                    UnvisitedPlaces = [P || P <- Unvisited, lists:member(P, Places)],
                    UnvisitedTransitions = [T || T <- Unvisited, lists:member(T, Transitions)],

                    lists:flatten([
                        case UnvisitedPlaces of
                            [] -> [];
                            _ ->
                                [#{category => connectedness_error,
                                  severity => error,
                                  message => iolist_to_binary([integer_to_binary(length(UnvisitedPlaces)),
                                                                <<" disconnected place(s): '">>,
                                                                format_list(UnvisitedPlaces),
                                                                <<"'">>]),
                                  code => disconnected_places,
                                  details => #{unvisited_places => UnvisitedPlaces}}]
                        end,
                        case UnvisitedTransitions of
                            [] -> [];
                            _ ->
                                [#{category => connectedness_error,
                                  severity => warning,
                                  message => iolist_to_binary([integer_to_binary(length(UnvisitedTransitions)),
                                                                <<" disconnected transition(s): '">>,
                                                                format_list(UnvisitedTransitions),
                                                                <<"'">>]),
                                  code => disconnected_transitions,
                                  details => #{unvisited_transitions => UnvisitedTransitions}}]
                        end
                    ])
            end
    end;

check_connectedness(_Spec) ->
    [#{category => structural_error,
       severity => error,
       message => <<"Cannot check connectedness: missing required fields">>,
       code => missing_fields_for_connectedness}].

%%--------------------------------------------------------------------
%% @private
%% @doc Build full bipartite adjacency list for the workflow.
%%--------------------------------------------------------------------
-spec build_full_adjacency(workflow_spec()) -> #{atom() => [atom()]}.

build_full_adjacency(#{places := _Places, transitions := Transitions,
                        preset := Preset, postset := Postset}) ->
    %% Build bipartite graph: places <-> transitions
    %% Edges from preset places to transition
    Adj1 = lists:foldl(fun(T, Acc) ->
        PresetPlaces = maps:get(T, Preset, []),
        lists:foldl(fun(P, AccIn) ->
            AccIn#{P => [T | maps:get(P, AccIn, [])]}
        end, Acc, PresetPlaces)
    end, #{}, Transitions),

    %% Edges from transition to postset places
    lists:foldl(fun(T, Acc) ->
        PostsetPlaces = maps:get(T, Postset, []),
        lists:foldl(fun(P, AccIn) ->
            AccIn#{T => [P | maps:get(T, AccIn, [])]}
        end, Acc, PostsetPlaces)
    end, Adj1, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc BFS traversal from a starting node.
%%--------------------------------------------------------------------
-spec bfs_from(atom(), #{atom() => [atom()]}, sets:set(atom())) -> sets:set(atom()).

bfs_from(Start, Adj, Visited) ->
    case sets:is_element(Start, Visited) of
        true -> Visited;
        false ->
            Visited1 = sets:add_element(Start, Visited),
            Neighbors = maps:get(Start, Adj, []),
            lists:foldl(fun(N, Acc) -> bfs_from(N, Adj, Acc) end, Visited1, Neighbors)
    end.

%%====================================================================
%% Conflict Detection
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Finds naming conflicts and other issues in composed workflows.
%%
%% Checks for:
%% - Duplicate place names
%% - Duplicate transition names
%% - Places and transitions with the same name
%% - Invalid name formats
%%
%% === Example ===
%% ```erlang
%% Conflicts = wfnet_compose_validator:find_conflicts(Spec1, Spec2).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec find_conflicts(workflow_spec(), workflow_spec() | undefined) -> [conflict_info()].

find_conflicts(Spec, undefined) ->
    %% Check single spec for internal consistency
    find_internal_conflicts(Spec);

find_conflicts(Spec1, Spec2) ->
    %% Check for conflicts between two specs that will be composed
    Conflicts1 = find_internal_conflicts(Spec1),
    Conflicts2 = find_internal_conflicts(Spec2),
    CrossConflicts = find_cross_conflicts(Spec1, Spec2),
    Conflicts1 ++ Conflicts2 ++ CrossConflicts.

%%--------------------------------------------------------------------
%% @private
%% @doc Find internal conflicts within a single spec.
%%--------------------------------------------------------------------
-spec find_internal_conflicts(workflow_spec()) -> [conflict_info()].

find_internal_conflicts(#{places := Places, transitions := Transitions}) ->
    Conflicts = [],

    %% Check for duplicate places
    DuplicatePlaces = find_duplicates(Places),
    Conflicts1 = case DuplicatePlaces of
        [] -> Conflicts;
        _ ->
            [#{type => duplicate_places,
               conflicts => DuplicatePlaces,
               description => iolist_to_binary([<<"Duplicate place names: '">>,
                                                format_list(DuplicatePlaces),
                                                <<"'">>])} | Conflicts]
    end,

    %% Check for duplicate transitions
    DuplicateTransitions = find_duplicates(Transitions),
    Conflicts2 = case DuplicateTransitions of
        [] -> Conflicts1;
        _ ->
            [#{type => duplicate_transitions,
               conflicts => DuplicateTransitions,
               description => iolist_to_binary([<<"Duplicate transition names: '">>,
                                                format_list(DuplicateTransitions),
                                                <<"'">>])} | Conflicts1]
    end,

    %% Check for name collisions between places and transitions
    NameCollisions = lists:filter(fun(N) ->
        lists:member(N, Places) andalso lists:member(N, Transitions)
    end, lists:usort(Places ++ Transitions)),
    Conflicts3 = case NameCollisions of
        [] -> Conflicts2;
        _ ->
            [#{type => place_transition_collision,
               conflicts => NameCollisions,
               description => iolist_to_binary([<<"Name collision (both place and transition): '">>,
                                                format_list(NameCollisions),
                                                <<"'">>])} | Conflicts2]
    end,

    Conflicts3.

%%--------------------------------------------------------------------
%% @private
%% @doc Find conflicts between two specs that will be composed.
%%--------------------------------------------------------------------
-spec find_cross_conflicts(workflow_spec(), workflow_spec()) -> [conflict_info()].

find_cross_conflicts(#{places := Places1, transitions := Transitions1},
                     #{places := Places2, transitions := Transitions2}) ->
    Conflicts = [],

    %% Check for overlapping place names
    CommonPlaces = lists:filter(fun(P) -> lists:member(P, Places2) end, Places1),
    Conflicts1 = case CommonPlaces of
        [] -> Conflicts;
        _ ->
            [#{type => overlapping_places,
               conflicts => CommonPlaces,
               description => iolist_to_binary([<<"Overlapping place names between specs: '">>,
                                                format_list(CommonPlaces),
                                                <<"' (use rename to avoid conflicts)">>])} | Conflicts]
    end,

    %% Check for overlapping transition names
    CommonTransitions = lists:filter(fun(T) -> lists:member(T, Transitions2) end, Transitions1),
    Conflicts2 = case CommonTransitions of
        [] -> Conflicts1;
        _ ->
            [#{type => overlapping_transitions,
               conflicts => CommonTransitions,
               description => iolist_to_binary([<<"Overlapping transition names between specs: '">>,
                                                format_list(CommonTransitions),
                                                <<"' (use rename to avoid conflicts)">>])} | Conflicts1]
    end,

    Conflicts2.

%%--------------------------------------------------------------------
%% @private
%% @doc Find duplicate items in a list.
%%--------------------------------------------------------------------
-spec find_duplicates([term()]) -> [term()].

find_duplicates(List) ->
    Counts = lists:foldl(fun(E, Acc) ->
        Acc#{E => maps:get(E, Acc, 0) + 1}
    end, #{}, List),
    [E || E <- maps:keys(Counts), maps:get(E, Counts) > 1].

%%====================================================================
%% Validation Report Generation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a detailed validation report.
%%
%% Creates a comprehensive report including:
%% - Overall validity status
%% - List of errors with details
%% - List of warnings
%% - Summary statistics
%% - Detailed analysis per category
%%
%% === Example ===
%% ```erlang
%% Report = wfnet_compose_validator:generate_validation_report(Spec).
%% io:format("~s~n", [maps:get(summary, Report)]).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_validation_report(workflow_spec() | {error, term()} | validation_result()) ->
    validation_report().

generate_validation_report({ok, Warnings}) ->
    #{
        valid => true,
        errors => [],
        warnings => Warnings,
        summary => build_summary(0, length(Warnings)),
        details => #{
            warning_count => length(Warnings),
            categories => categorize_issues(Warnings)
        }
    };

generate_validation_report({error, Errors}) ->
    {ActualErrors, Warnings} = lists:partition(
        fun(#{severity := Sev}) -> Sev =:= error end,
        Errors
    ),
    #{
        valid => false,
        errors => ActualErrors,
        warnings => Warnings,
        summary => build_summary(length(ActualErrors), length(Warnings)),
        details => #{
            error_count => length(ActualErrors),
            warning_count => length(Warnings),
            categories => categorize_issues(ActualErrors ++ Warnings),
            suggested_fixes => suggest_fixes(ActualErrors)
        }
    };

generate_validation_report(Spec) when is_map(Spec) ->
    %% Validate the spec and generate report
    case validate_composition(Spec) of
        {ok, Warnings} -> generate_validation_report({ok, Warnings});
        {error, Errors} -> generate_validation_report({error, Errors})
    end;

generate_validation_report({error, Reason}) ->
    ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
    #{
        valid => false,
        errors => [#{category => structural_error,
                     severity => error,
                     message => iolist_to_binary([<<"Validation failed: ">>, ReasonBinary]),
                     code => validation_failed}],
        warnings => [],
        summary => <<"Validation failed with exception">>,
        details => #{reason => Reason}
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Build summary string from error and warning counts.
%%--------------------------------------------------------------------
-spec build_summary(non_neg_integer(), non_neg_integer()) -> binary().

build_summary(0, 0) ->
    <<"Validation passed: No errors or warnings found">>;
build_summary(0, Warnings) ->
    iolist_to_binary([<<"Validation passed with ">>,
                      integer_to_binary(Warnings),
                      <<" warning(s)">>]);
build_summary(Errors, 0) ->
    iolist_to_binary([<<"Validation failed with ">>,
                      integer_to_binary(Errors),
                      <<" error(s)">>]);
build_summary(Errors, Warnings) ->
    iolist_to_binary([<<"Validation failed with ">>,
                      integer_to_binary(Errors),
                      <<" error(s) and ">>,
                      integer_to_binary(Warnings),
                      <<" warning(s)">>]).

%%--------------------------------------------------------------------
%% @private
%% @doc Categorize issues by type.
%%--------------------------------------------------------------------
-spec categorize_issues([validation_error() | validation_warning()]) -> #{atom() => non_neg_integer()}.

categorize_issues(Issues) ->
    lists:foldl(fun(Issue, Acc) ->
        Category = maps:get(category, Issue, unknown),
        Acc#{Category => maps:get(Category, Acc, 0) + 1}
    end, #{}, Issues).

%%--------------------------------------------------------------------
%% @private
%% @doc Suggest fixes for common errors.
%%--------------------------------------------------------------------
-spec suggest_fixes([validation_error()]) -> [binary()].

suggest_fixes(Errors) ->
    UniqueCodes = lists:usort([maps:get(code, E) || E <- Errors]),
    lists:map(fun(Code) ->
        suggest_fix(Code)
    end, UniqueCodes).

%%--------------------------------------------------------------------
%% @private
%% @doc Suggest fix for a specific error code.
%%--------------------------------------------------------------------
-spec suggest_fix(atom()) -> binary().

suggest_fix(start_end_same) ->
    <<"Ensure start_place and end_place refer to different places in the workflow">>;
suggest_fix(start_not_found) ->
    <<"Add the start_place to the places list or correct the reference">>;
suggest_fix(end_not_found) ->
    <<"Add the end_place to the places list or correct the reference">>;
suggest_fix(end_not_reachable) ->
    <<"Add transitions to create a path from start to end place">>;
suggest_fix(duplicate_places) ->
    <<"Rename places to ensure all place names are unique">>;
suggest_fix(duplicate_transitions) ->
    <<"Rename transitions to ensure all transition names are unique">>;
suggest_fix(disconnected_places) ->
    <<"Connect disconnected places to the main workflow or remove them">>;
suggest_fix(Code) ->
    iolist_to_binary([<<"Review issue: ">>, atom_to_binary(Code, utf8)]).

%%====================================================================
%% Structural Validation Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks that all required fields are present in the spec.
%%--------------------------------------------------------------------
-spec check_required_fields(workflow_spec()) -> [validation_error()].

check_required_fields(Spec) when is_map(Spec) ->
    Required = [places, transitions, start_place, end_place],
    Missing = [F || F <- Required, not maps:is_key(F, Spec)],
    case Missing of
        [] -> [];
        _ ->
            lists:map(fun(F) ->
                #{category => structural_error,
                  severity => error,
                  message => iolist_to_binary([<<"Missing required field: '">>,
                                              atom_to_binary(F, utf8),
                                              <<"'">>]),
                  code => missing_required_field}
            end, Missing)
    end;

check_required_fields(_) ->
    [#{category => structural_error,
       severity => error,
       message => <<"Specification must be a map">>,
       code => invalid_spec_type}].

%%--------------------------------------------------------------------
%% @doc Checks for unique names in the workflow specification.
%%
%% Ensures that:
%% - All place names are unique
%% - All transition names are unique
%% - No place has the same name as a transition
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_compose_validator:check_unique_names(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_unique_names(workflow_spec()) -> [validation_error() | validation_warning()].

check_unique_names(#{places := Places, transitions := Transitions}) ->
    Errors = [],

    %% Check for duplicate places
    PlaceCounts = count_occurrences(Places),
    DuplicatePlaces = [P || P <- maps:keys(PlaceCounts), maps:get(P, PlaceCounts) > 1],
    Errors1 = case DuplicatePlaces of
        [] -> Errors;
        _ ->
            [#{category => naming_conflict,
              severity => error,
              message => iolist_to_binary([<<"Duplicate place names detected: '">>,
                                          format_list(DuplicatePlaces),
                                          <<"'">>]),
              code => duplicate_places,
              details => #{duplicates => DuplicatePlaces}} | Errors]
    end,

    %% Check for duplicate transitions
    TransCounts = count_occurrences(Transitions),
    DuplicateTrans = [T || T <- maps:keys(TransCounts), maps:get(T, TransCounts) > 1],
    Errors2 = case DuplicateTrans of
        [] -> Errors1;
        _ ->
            [#{category => naming_conflict,
              severity => error,
              message => iolist_to_binary([<<"Duplicate transition names detected: '">>,
                                          format_list(DuplicateTrans),
                                          <<"'">>]),
              code => duplicate_transitions,
              details => #{duplicates => DuplicateTrans}} | Errors1]
    end,

    %% Check for name collision between places and transitions
    CommonNames = sets:to_list(sets:intersection(
        sets:from_list(Places),
        sets:from_list(Transitions)
    )),
    Errors3 = case CommonNames of
        [] -> Errors2;
        _ ->
            [#{category => naming_conflict,
              severity => warning,
              message => iolist_to_binary([<<"Name collision between places and transitions: '">>,
                                          format_list(CommonNames),
                                          <<"' (ambiguous references)">>]),
              code => place_transition_name_collision,
              details => #{collisions => CommonNames}} | Errors2]
    end,

    %% Check for reserved name conflicts
    ReservedNames = [start, 'end', init, undefined],
    ReservedConflicts = lists:filter(fun(N) ->
        lists:member(N, Places) orelse lists:member(N, Transitions)
    end, ReservedNames),
    Errors4 = case ReservedConflicts of
        [] -> Errors3;
        _ ->
            [#{category => naming_conflict,
              severity => warning,
              message => iolist_to_binary([<<"Using reserved names: '">>,
                                          format_list(ReservedConflicts),
                                          <<"' (may cause issues)">>]),
              code => reserved_name_usage,
              details => #{reserved => ReservedConflicts}} | Errors3]
    end,

    Errors4;

check_unique_names(_) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Count occurrences of items in a list.
%%--------------------------------------------------------------------
-spec count_occurrences([term()]) -> #{term() => pos_integer()}.

count_occurrences(List) ->
    lists:foldl(fun(E, Acc) ->
        Acc#{E => maps:get(E, Acc, 0) + 1}
    end, #{}, List).

%%--------------------------------------------------------------------
%% @doc Checks that all connections (preset/postset) are valid.
%%
%% Verifies:
%% - All places referenced in preset/postset exist
%% - All transitions with preset/postset are in the transitions list
%% - No circular self-loops (unless intentional)
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_compose_validator:check_connection_validity(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_connection_validity(workflow_spec()) -> [validation_error() | validation_warning()].

check_connection_validity(#{places := Places, transitions := Transitions,
                            preset := Preset, postset := Postset} = _Spec) ->
    PlaceSet = sets:from_list(Places),
    TransSet = sets:from_list(Transitions),

    %% Check preset validity
    PresetErrors = maps:fold(fun(T, PresetPlaces, Acc) ->
        %% Check if transition is valid
        Acc1 = case sets:is_element(T, TransSet) of
            true -> Acc;
            false ->
                [#{category => invalid_reference,
                  severity => error,
                  message => iolist_to_binary([<<"Preset refers to unknown transition '">>,
                                              atom_to_binary(T, utf8),
                                              <<"'">>]),
                  location => T,
                  code => transition_not_in_list} | Acc]
        end,

        %% Check if all preset places exist
        lists:foldl(fun(P, AccIn) ->
            case sets:is_element(P, PlaceSet) of
                true -> AccIn;
                false ->
                    [#{category => invalid_reference,
                      severity => error,
                      message => iolist_to_binary([<<"Preset place '">>,
                                                  atom_to_binary(P, utf8),
                                                  <<"' for transition '">>,
                                                  atom_to_binary(T, utf8),
                                                  <<"' not found">>]),
                      location => T,
                      code => preset_place_not_found} | AccIn]
            end
        end, Acc1, PresetPlaces)
    end, [], Preset),

    %% Check postset validity
    PostsetErrors = maps:fold(fun(T, PostsetPlaces, Acc) ->
        %% Check if transition is valid
        Acc1 = case sets:is_element(T, TransSet) of
            true -> Acc;
            false ->
                [#{category => invalid_reference,
                  severity => error,
                  message => iolist_to_binary([<<"Postset refers to unknown transition '">>,
                                              atom_to_binary(T, utf8),
                                              <<"'">>]),
                  location => T,
                  code => transition_not_in_list} | Acc]
        end,

        %% Check if all postset places exist
        lists:foldl(fun(P, AccIn) ->
            case sets:is_element(P, PlaceSet) of
                true -> AccIn;
                false ->
                    [#{category => invalid_reference,
                      severity => error,
                      message => iolist_to_binary([<<"Postset place '">>,
                                                  atom_to_binary(P, utf8),
                                                  <<"' for transition '">>,
                                                  atom_to_binary(T, utf8),
                                                  <<"' not found">>]),
                      location => T,
                      code => postset_place_not_found} | AccIn]
            end
        end, Acc1, PostsetPlaces)
    end, [], Postset),

    %% Check for self-loops
    SelfLoopErrors = maps:fold(fun(T, PresetPlaces, Acc) ->
        PostsetPlaces = maps:get(T, Postset, []),
        LoopPlaces = sets:to_list(sets:intersection(
            sets:from_list(PresetPlaces),
            sets:from_list(PostsetPlaces)
        )),
        lists:foldl(fun(P, AccIn) ->
            [#{category => structural_error,
              severity => warning,
              message => iolist_to_binary([<<"Self-loop detected: transition '">>,
                                          atom_to_binary(T, utf8),
                                          <<"' consumes from and produces to place '">>,
                                          atom_to_binary(P, utf8),
                                          <<"'">>]),
              location => T,
              code => self_loop} | AccIn]
        end, Acc, LoopPlaces)
    end, [], Preset),

    PresetErrors ++ PostsetErrors ++ SelfLoopErrors;

check_connection_validity(_) ->
    [].

%%--------------------------------------------------------------------
%% @doc Checks validity of start and end places.
%%
%% Verifies:
%% - Start and end places are in the places list
%% - Start and end are different
%% - Start place has no incoming transitions (optional)
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_compose_validator:check_start_end_validity(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_start_end_validity(workflow_spec()) -> [validation_error() | validation_warning()].

check_start_end_validity(#{start_place := Start, end_place := End,
                           places := Places, preset := Preset}) ->
    Errors = [],

    %% Check start place is in places
    Errors1 = case lists:member(Start, Places) of
        true -> Errors;
        false ->
            [#{category => invalid_reference,
              severity => error,
              message => iolist_to_binary([<<"Start place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"' not in places list">>]),
              location => Start,
              code => start_place_not_found} | Errors]
    end,

    %% Check end place is in places
    Errors2 = case lists:member(End, Places) of
        true -> Errors1;
        false ->
            [#{category => invalid_reference,
              severity => error,
              message => iolist_to_binary([<<"End place '">>,
                                          atom_to_binary(End, utf8),
                                          <<"' not in places list">>]),
              location => End,
              code => end_place_not_found} | Errors1]
    end,

    %% Check start and end are different
    Errors3 = case Start =:= End of
        true ->
            [#{category => structural_error,
              severity => error,
              message => <<"Start and end places must be different">>,
              location => Start,
              code => start_end_same} | Errors2];
        false ->
            Errors2
    end,

    %% Check for transitions producing to start (warning)
    ToStart = [T || T <- maps:keys(Preset),
                   lists:member(Start, maps:get(T, Preset, []))],
    Errors4 = case ToStart of
        [] -> Errors3;
        _ ->
            [#{category => structural_error,
              severity => warning,
              message => iolist_to_binary([<<"Transitions produce to start place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"' (may cause re-execution)">>]),
              location => Start,
              code => transitions_to_start} | Errors3]
    end,

    Errors4;

check_start_end_validity(_) ->
    [].

%%====================================================================
%% Boundedness Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks boundedness of the workflow.
%%
%% A workflow is bounded if no place can accumulate unlimited tokens.
%% Unbounded workflows may cause memory issues during execution.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_compose_validator:check_boundedness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_boundedness(workflow_spec()) -> [validation_error() | validation_warning()].

check_boundedness(#{places := Places} = Spec) ->
    %% Analyze the workflow structure for potential unboundedness
    %% Common causes:
    %% 1. Cycles without consuming transitions
    %% 2. Places that only receive tokens but never consume
    %% 3. Multiple producers to a single place with limited consumption

    %% Find sink places (only produce, never consume)
    Preset = maps:get(preset, Spec, #{}),
    Postset = maps:get(postset, Spec, #{}),

    %% Build sets of places that are consumed and produced
    ConsumedPlaces = sets:from_list(lists:flatten(maps:values(Preset))),
    ProducedPlaces = sets:from_list(lists:flatten(maps:values(Postset))),

    %% Sink places: produced but never consumed (except end place which is OK)
    #{end_place := EndPlace} = Spec,
    SinkPlaces = lists:filter(fun(P) ->
        P =/= EndPlace andalso
        sets:is_element(P, ProducedPlaces) andalso
        not sets:is_element(P, ConsumedPlaces)
    end, Places),

    SinkErrors = case SinkPlaces of
        [] -> [];
        _ ->
            [#{category => boundedness_error,
              severity => warning,
              message => iolist_to_binary([<<"Potential unbounded place(s): '">>,
                                          format_list(SinkPlaces),
                                          <<"' (tokens produced but never consumed)">>]),
              code => potential_unbounded_place,
              details => #{sink_places => SinkPlaces}}]
    end,

    %% Check for cycles that could cause unboundedness
    CycleErrors = case detect_cycles(Spec) of
        [] -> [];
        Cycles ->
            [#{category => boundedness_error,
              severity => warning,
              message => iolist_to_binary([<<"Cycle(s) detected that may cause unboundedness: '">>,
                                          format_cycles(Cycles),
                                          <<"'">>]),
              code => cycle_detected,
              details => #{cycles => Cycles}}]
    end,

    SinkErrors ++ CycleErrors.

%%--------------------------------------------------------------------
%% @doc Detects cycles in the workflow graph.
%%
%% Returns a list of cycles (each cycle is a list of nodes).
%% Cycles can cause unbounded token accumulation if not properly
%% synchronized.
%%
%% === Example ===
%% ```erlang
%% Cycles = wfnet_compose_validator:detect_cycles(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_cycles(workflow_spec()) -> [[atom()]].

detect_cycles(#{places := Places, transitions := Transitions} = Spec) ->
    %% Build directed graph (places -> transitions -> places)
    %% Detect cycles using DFS
    Adj = build_full_adjacency(Spec),
    AllNodes = Places ++ Transitions,

    %% Find cycles using DFS with coloring
    {_, Cycles} = lists:foldl(fun(Node, {Visited, Acc}) ->
        case sets:is_element(Node, Visited) of
            true -> {Visited, Acc};
            false ->
                {Visited1, NodeCycles} = dfs_cycles(Node, Adj, sets:new(), Visited, []),
                {Visited1, Acc ++ NodeCycles}
        end
    end, {sets:new(), []}, AllNodes),

    Cycles.

%%--------------------------------------------------------------------
%% @private
%% @doc DFS cycle detection helper.
%%--------------------------------------------------------------------
-spec dfs_cycles(atom(), #{atom() => [atom()]}, sets:set(atom()),
                 sets:set(atom()), [atom()]) ->
    {sets:set(atom()), [[atom()]]}.

dfs_cycles(Node, Adj, RecStack, Visited, Path) ->
    Visited1 = sets:add_element(Node, Visited),

    %% Check if node is in recursion stack (cycle found)
    case sets:is_element(Node, RecStack) of
        true ->
            %% Extract the cycle from the path
            Cycle = extract_cycle(Path ++ [Node], Node),
            {Visited1, [Cycle]};
        false ->
            RecStack1 = sets:add_element(Node, RecStack),
            Neighbors = maps:get(Node, Adj, []),
            lists:foldl(fun(N, {V, Acc}) ->
                case sets:is_element(N, V) of
                    true -> {V, Acc};
                    false ->
                        dfs_cycles(N, Adj, RecStack1, V, Path ++ [Node])
                end
            end, {Visited1, []}, Neighbors)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Extract cycle from path when a back-edge is found.
%%--------------------------------------------------------------------
-spec extract_cycle([atom()], atom()) -> [atom()].

extract_cycle(Path, Node) ->
    %% Find the node in the path and extract from there to end
    case lists:reverse(Path) of
        [Node | Rest] ->
            lists:reverse([Node | Rest]);
        RevPath ->
            case lists:dropwhile(fun(N) -> N =/= Node end, RevPath) of
                [Node | _] = Cycle -> lists:reverse(Cycle);
                _ -> [Node]
            end
    end.

%%====================================================================
%% Liveness Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks liveness properties of the workflow.
%%
%% Liveness means:
%% 1. No dead transitions (every transition can fire from some state)
%% 2. No deadlocks (except at the terminal state)
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_compose_validator:check_liveness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_liveness(workflow_spec()) -> [validation_error() | validation_warning()].

check_liveness(#{transitions := Transitions, places := _Places,
                 preset := Preset, postset := Postset,
                 start_place := Start} = Spec) ->

    %% Check for source transitions (no preset) - should only be the initial transition
    SourceTransitions = [T || T <- Transitions,
                             maps:get(T, Preset, []) =:= []],
    Errors1 = case SourceTransitions of
        [] ->
            %% No source transition - workflow may not start
            [#{category => liveness_error,
              severity => error,
              message => <<"No source transition found (workflow cannot start)">>,
              code => no_source_transition}];
        _ when length(SourceTransitions) > 1 ->
            [#{category => liveness_error,
              severity => warning,
              message => iolist_to_binary([<<"Multiple source transitions: '">>,
                                          format_list(SourceTransitions),
                                          <<"' (workflow may have ambiguous entry points)">>]),
              code => multiple_source_transitions,
              details => #{sources => SourceTransitions}}];
        _ ->
            []
    end,

    %% Check for sink transitions (no postset) - should only be the final transition
    SinkTransitions = [T || T <- Transitions,
                           maps:get(T, Postset, []) =:= []],
    Errors2 = case SinkTransitions of
        [] ->
            %% No sink transition - workflow may not terminate properly
            [#{category => liveness_error,
              severity => warning,
              message => <<"No sink transition found (workflow may not terminate cleanly)">>,
              code => no_sink_transition} | Errors1];
        _ when length(SinkTransitions) > 1 ->
            [#{category => liveness_error,
              severity => warning,
              message => iolist_to_binary([<<"Multiple sink transitions: '">>,
                                          format_list(SinkTransitions),
                                          <<"'">>]),
              code => multiple_sink_transitions,
              details => #{sinks => SinkTransitions}} | Errors1];
        _ ->
            Errors1
    end,

    %% Check for isolated transitions (no preset or postset)
    IsolatedTransitions = [T || T <- Transitions,
                               maps:get(T, Preset, []) =:= [],
                               maps:get(T, Postset, []) =:= []],
    Errors3 = case IsolatedTransitions of
        [] -> Errors2;
        _ ->
            [#{category => liveness_error,
              severity => error,
              message => iolist_to_binary([<<"Isolated transitions found: '">>,
                                          format_list(IsolatedTransitions),
                                          <<"' (no preset or postset)">>]),
              code => isolated_transitions,
              details => #{isolated => IsolatedTransitions}} | Errors2]
    end,

    %% Check for potentially unreachable transitions
    %% (transitions whose preset places are not reachable from start)
    Unreachable = lists:filter(fun(T) ->
        PresetPlaces = maps:get(T, Preset, []),
        case PresetPlaces of
            [] -> false;  %% Source transition
            _ ->
                %% Check if any preset place is reachable from start
                not lists:any(fun(P) -> is_reachable(Start, P, Spec) end, PresetPlaces)
        end
    end, Transitions),
    Errors4 = case Unreachable of
        [] -> Errors3;
        _ ->
            [#{category => liveness_error,
              severity => warning,
              message => iolist_to_binary([<<"Potentially unreachable transitions: '">>,
                                          format_list(Unreachable),
                                          <<"'">>]),
              code => unreachable_transitions,
              details => #{unreachable => Unreachable}} | Errors3]
    end,

    Errors4.

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Format a list of atoms as a comma-separated binary.
%%--------------------------------------------------------------------
-spec format_list([atom()]) -> binary().

format_list([]) ->
    <<>>;
format_list([Item]) ->
    atom_to_binary(Item, utf8);
format_list([Item | Rest]) ->
    iolist_to_binary([atom_to_binary(Item, utf8),
                      <<", ">>,
                      format_list(Rest)]).

%%--------------------------------------------------------------------
%% @private
%% @doc Format cycles for display.
%%--------------------------------------------------------------------
-spec format_cycles([[atom()]]) -> binary().

format_cycles([]) ->
    <<>>;
format_cycles([Cycle]) ->
    iolist_to_binary(["[", format_list(Cycle), "]"]);
format_cycles([Cycle | Rest]) ->
    iolist_to_binary(["[", format_list(Cycle), "], ", format_cycles(Rest)]).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test: validate_composition with valid spec
%%--------------------------------------------------------------------
validate_composition_valid_test() ->
    Spec = #{
        places => [p1, p2, p3],
        transitions => [t1, t2],
        start_place => p1,
        end_place => p3,
        preset => #{t1 => [p1], t2 => [p2]},
        postset => #{t1 => [p2], t2 => [p3]}
    },
    ?assertMatch({ok, _}, validate_composition(Spec)).

%%--------------------------------------------------------------------
%% Test: validate_composition with duplicate places
%%--------------------------------------------------------------------
validate_composition_duplicate_test() ->
    Spec = #{
        places => [p1, p2, p1],  % Duplicate p1
        transitions => [t1],
        start_place => p1,
        end_place => p2,
        preset => #{t1 => [p1]},
        postset => #{t1 => [p2]}
    },
    ?assertMatch({error, _}, validate_composition(Spec)),
    {error, Errors} = validate_composition(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= duplicate_places end, Errors)).

%%--------------------------------------------------------------------
%% Test: check_soundness with same start/end
%%--------------------------------------------------------------------
check_soundness_same_start_end_test() ->
    Spec = #{
        places => [p1],
        transitions => [t1],
        start_place => p1,
        end_place => p1,
        preset => #{t1 => [p1]},
        postset => #{t1 => [p1]}
    },
    Errors = check_soundness(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= start_end_same end, Errors)).

%%--------------------------------------------------------------------
%% Test: check_connectedness with disconnected component
%%--------------------------------------------------------------------
check_connectedness_disconnected_test() ->
    Spec = #{
        places => [p1, p2, p3],  % p3 is disconnected
        transitions => [t1],
        start_place => p1,
        end_place => p2,
        preset => #{t1 => [p1]},
        postset => #{t1 => [p2]}
    },
    Errors = check_connectedness(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= disconnected_places end, Errors)).

%%--------------------------------------------------------------------
%% Test: find_conflicts between specs
%%--------------------------------------------------------------------
find_conflicts_test() ->
    Spec1 = #{
        places => [p1, p2],
        transitions => [t1],
        start_place => p1,
        end_place => p2
    },
    Spec2 = #{
        places => [p2, p3],  % Overlapping p2
        transitions => [t1],  % Overlapping t1
        start_place => p2,
        end_place => p3
    },
    Conflicts = find_conflicts(Spec1, Spec2),
    ?assert(length(Conflicts) > 0),
    ?assert(lists:any(fun(#{type := T}) -> T =:= overlapping_places end, Conflicts)),
    ?assert(lists:any(fun(#{type := T}) -> T =:= overlapping_transitions end, Conflicts)).

%%--------------------------------------------------------------------
%% Test: generate_validation_report
%%--------------------------------------------------------------------
generate_validation_report_test() ->
    Spec = #{
        places => [p1, p2],
        transitions => [t1],
        start_place => p1,
        end_place => p2,
        preset => #{t1 => [p1]},
        postset => #{t1 => [p2]}
    },
    Report = generate_validation_report(Spec),
    ?assert(maps:get(valid, Report)),
    ?assertEqual(0, maps:get(error_count, maps:get(details, Report))).

%%--------------------------------------------------------------------
%% Test: check_unique_names
%%--------------------------------------------------------------------
check_unique_names_test() ->
    Spec = #{
        places => [p1, p2, p1],
        transitions => [t1, t1],
        start_place => p1,
        end_place => p2
    },
    Errors = check_unique_names(Spec),
    ?assert(length(Errors) >= 2).

%%--------------------------------------------------------------------
%% Test: detect_cycles
%%--------------------------------------------------------------------
detect_cycles_test() ->
    Spec = #{
        places => [p1, p2],
        transitions => [t1, t2],
        start_place => p1,
        end_place => p2,
        preset => #{t1 => [p1], t2 => [p2]},
        postset => #{t1 => [p2], t2 => [p1]}  % Creates cycle
    },
    Cycles = detect_cycles(Spec),
    ?assert(length(Cycles) > 0).

%%--------------------------------------------------------------------
%% Test: check_connection_validity
%%--------------------------------------------------------------------
check_connection_validity_test() ->
    Spec = #{
        places => [p1, p2],
        transitions => [t1],
        start_place => p1,
        end_place => p2,
        preset => #{t1 => [p1, p3]},  % p3 doesn't exist
        postset => #{t1 => [p2]}
    },
    Errors = check_connection_validity(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= preset_place_not_found end, Errors)).

%%--------------------------------------------------------------------
%% Test: format_list utility
%%--------------------------------------------------------------------
format_list_test() ->
    ?assertEqual(<<"a">>, format_list([a])),
    ?assertEqual(<<"a, b, c">>, format_list([a, b, c])),
    ?assertEqual(<<>>, format_list([])).

%%--------------------------------------------------------------------
%% Test: is_reachable
%%--------------------------------------------------------------------
is_reachable_test() ->
    Spec = #{
        places => [p1, p2, p3],
        transitions => [t1, t2],
        preset => #{t1 => [p1], t2 => [p2]},
        postset => #{t1 => [p2], t2 => [p3]}
    },
    ?assert(is_reachable(p1, p3, Spec)),
    ?assertNot(is_reachable(p3, p1, Spec)).

%%--------------------------------------------------------------------
%% Test: find_duplicates
%%--------------------------------------------------------------------
find_duplicates_test() ->
    ?assertEqual([], find_duplicates([a, b, c])),
    ?assertEqual([a], find_duplicates([a, b, a, c])),
    ?assertEqual([a, b], lists:sort(find_duplicates([a, b, a, b]))).

%%--------------------------------------------------------------------
%% Test: generate_validation_report with errors
%%--------------------------------------------------------------------
generate_validation_report_errors_test() ->
    Result = {error, [
        #{category => structural_error, severity => error, message => <<"Error 1">>, code => err1},
        #{category => soundness_violation, severity => warning, message => <<"Warning 1">>, code => warn1}
    ]},
    Report = generate_validation_report(Result),
    ?assertNot(maps:get(valid, Report)),
    ?assertEqual(1, maps:get(error_count, maps:get(details, Report))),
    ?assertEqual(1, maps:get(warning_count, maps:get(details, Report))).

%%--------------------------------------------------------------------
%% Test: check_liveness with source transitions
%%--------------------------------------------------------------------
check_liveness_source_test() ->
    Spec = #{
        places => [p1, p2],
        transitions => [t1, t2],  % Both source
        start_place => p1,
        end_place => p2,
        preset => #{t1 => [], t2 => []},
        postset => #{t1 => [p2], t2 => [p1]}
    },
    Errors = check_liveness(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= multiple_source_transitions end, Errors)).

%%--------------------------------------------------------------------
%% Test: check_boundedness with sink places
%%--------------------------------------------------------------------
check_boundedness_sink_test() ->
    Spec = #{
        places => [p1, p2, p3],
        transitions => [t1, t2],
        start_place => p1,
        end_place => p3,
        preset => #{t1 => [p1], t2 => [p2]},
        postset => #{t1 => [p2, p3], t2 => [p3]}  % Only produces to p3
    },
    Errors = check_boundedness(Spec),
    %% p3 is the end place so it's OK
    ?assertEqual([], [E || E <- Errors, maps:get(code, E) =:= potential_unbounded_place]).

-endif.
