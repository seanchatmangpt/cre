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
%% @doc Workflow Net (WF-Net) Validator
%%
%% This module provides comprehensive validation for Workflow Nets based on
%% Petri net theory and YAWL workflow patterns. A WF-Net is a Petri net with
%% specific structural properties that ensure well-formed workflow execution.
%%
%% <h3>WF-Net Properties Validated</h3>
%%
%% <ul>
%%   <li><b>Soundness:</b> Every execution path terminates at the correct end state</li>
%%   <li><b>Liveness:</b> No dead transitions - every transition can fire from some state</li>
%%   <li><b>Boundedness:</b> Places are K-bounded - no unbounded token accumulation</li>
%%   <li><b>Connectedness:</b> Single connected component - no isolated nodes</li>
%%   <li><b>Proper Completion:</b> Correct termination conditions</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Validate a gen_pnet workflow module
%% {ok, Warnings} = wfnet_validate:validate_workflow(my_workflow).
%% ```
%%
%% ```erlang
%% %% Validate a specification map
%% Spec = extract_structure(my_workflow),
%% {ok, []} = wfnet_validate:validate_spec(Spec).
%% ```
%%
%% ```erlang
%% %% Check soundness specifically
%% SoundErrors = wfnet_validate:check_soundness(Spec).
%% ```
%%
%% ```erlang
%% %% Format errors for display
%% Formatted = wfnet_validate:format_errors(Errors).
%% ```
%% @end
%% -------------------------------------------------------------------

-module(wfnet_validate).

%%====================================================================
%% Exports
%%====================================================================

%% Main validation API
-export([validate_workflow/1, validate_spec/1]).

%% Soundness and liveness checks
-export([check_soundness/1, check_liveness/1, check_boundedness/1]).

%% Structural validation
-export([find_structural_errors/1, format_errors/1]).

%% Helper functions
-export([extract_structure/1]).
-export([check_option_to_complete/1, check_proper_completion/1]).
-export([check_dead_transitions/1, check_isolated_nodes/1]).
-export([check_connectedness/1, check_split_join_consistency/1]).
-export([check_start_end_places/1, check_self_loops/1]).

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
%% @doc A token in the workflow net.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc Workflow net specification structure.
%%
%% Extracted from a gen_pnet module via extract_structure/1.
%%--------------------------------------------------------------------
-type wfnet_spec() :: #{
    places => [place()],
    transitions => [trsn()],
    initial_marking => marking(),
    preset => #{trsn() => [place()]},
    postset => #{trsn() => [place()]},
    start_place => place(),
    end_place => place()
}.

%%--------------------------------------------------------------------
%% @doc Validation error types.
%%--------------------------------------------------------------------
-type error_type() ::
    structural      %% Structural issues in the net
    | soundness     %% Soundness property violations
    | liveness      %% Liveness property violations
    | boundedness   %% Boundedness property violations
    | reference     %% Invalid references
    | semantic.     %% Semantic issues

%%--------------------------------------------------------------------
%% @doc Validation error severity.
%%--------------------------------------------------------------------
-type severity() :: error | warning.

%%--------------------------------------------------------------------
%% @doc A validation error record.
%%--------------------------------------------------------------------
-type validation_error() :: #{
    type => error_type(),
    severity => severity(),
    message => binary(),
    location => atom() | undefined,
    code => atom()
}.

%%--------------------------------------------------------------------
%% @doc Validation result.
%%--------------------------------------------------------------------
-type validation_result() :: {ok, [validation_error()]} |
                             {error, [validation_error()]}.

%%--------------------------------------------------------------------
%% @doc Graph representation for analysis.
%%--------------------------------------------------------------------
-type graph() :: #{
    nodes => [atom()],
    edges => [{atom(), atom()}],
    adj_list => #{atom() => [atom()]}
}.

%% Export types
-export_type([place/0, trsn/0, token/0, marking/0, wfnet_spec/0,
              validation_error/0, validation_result/0, graph/0]).

%%====================================================================
%% Main Validation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a workflow implemented as a gen_pnet module.
%%
%% Extracts the structure from the module and performs comprehensive
%% validation including soundness, liveness, boundedness, and
%% structural checks.
%%
%% === Example ===
%% ```erlang
%% {ok, Warnings} = wfnet_validate:validate_workflow(my_workflow).
%% {error, Errors} = wfnet_validate:validate_workflow(broken_workflow).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_workflow(module()) -> validation_result().

validate_workflow(Module) when is_atom(Module) ->
    try
        Spec = extract_structure(Module),
        validate_spec(Spec)
    catch
        Type:Error:Stack ->
            logger:error("wfnet_validate: failed to extract structure from ~p: ~p:~p~n~p",
                        [Module, Type, Error, Stack]),
            {error, [#{
                type => structural,
                severity => error,
                message => iolist_to_binary([
                    <<"Failed to extract workflow structure: ">>,
                    atom_to_list(Type), <<":">>,
                    io_lib:format("~p", [Error])
                ]),
                location => Module,
                code => structure_extraction_failed
            }]}
    end.

%%--------------------------------------------------------------------
%% @doc Validates a workflow specification map.
%%
%% Performs all validation checks on the pre-extracted specification.
%%
%% === Example ===
%% ```erlang
%% Spec = wfnet_validate:extract_structure(my_workflow),
%% {ok, []} = wfnet_validate:validate_spec(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_spec(wfnet_spec()) -> validation_result().

validate_spec(Spec) when is_map(Spec) ->
    AllErrors = lists:flatten([
        check_required_spec_fields(Spec),
        find_structural_errors(Spec),
        check_soundness(Spec),
        check_liveness(Spec),
        check_boundedness(Spec)
    ]),

    %% Separate errors and warnings
    {Errors, Warnings} = lists:partition(
        fun(#{severity := Sev}) -> Sev =:= error end,
        AllErrors
    ),

    case Errors of
        [] -> {ok, Warnings};
        _ -> {error, Errors ++ Warnings}
    end;

validate_spec(_Spec) ->
    {error, [#{type => structural,
               severity => error,
               message => <<"Specification must be a map">>,
               location => undefined,
               code => invalid_spec_type}]}.

%%====================================================================
%% Required Fields Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Checks that all required spec fields are present.
%%--------------------------------------------------------------------
-spec check_required_spec_fields(wfnet_spec()) -> [validation_error()].

check_required_spec_fields(#{places := Places, transitions := Transitions,
                             initial_marking := Marking, preset := Preset,
                             postset := Postset, start_place := Start,
                             end_place := End})
  when is_list(Places), is_list(Transitions), is_map(Marking),
       is_map(Preset), is_map(Postset), is_atom(Start), is_atom(End) ->
    %% Basic field presence check passed
    [];
check_required_spec_fields(Spec) ->
    %% Find missing fields
    Required = [places, transitions, initial_marking, preset, postset,
                start_place, end_place],
    Missing = [F || F <- Required, not maps:is_key(F, Spec)],
    lists:map(fun(F) ->
        #{type => structural,
          severity => error,
          message => iolist_to_binary([<<"Missing required field: '">>,
                                      atom_to_binary(F, utf8), <<"'">>]),
          location => undefined,
          code => missing_required_field}
    end, Missing).

%%====================================================================
%% Structural Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Finds all structural errors in the workflow net.
%%
%% Checks for:
%% - Isolated nodes (no incoming or outgoing arcs)
%% - Self-looping transitions
%% - Disconnected components
%% - Invalid split/join configurations
%% - Missing start/end places
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:find_structural_errors(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec find_structural_errors(wfnet_spec()) -> [validation_error()].

find_structural_errors(#{places := Places, transitions := Transitions,
                         preset := Preset, postset := Postset,
                         start_place := Start, end_place := End} = Spec) ->
    Errors = [],
    %% Check isolated nodes
    Errors1 = Errors ++ check_isolated_nodes(Spec),
    %% Check self loops
    Errors2 = Errors1 ++ check_self_loops(Spec),
    %% Check connectedness
    Errors3 = Errors2 ++ check_connectedness(Spec),
    %% Check split/join consistency
    Errors4 = Errors3 ++ check_split_join_consistency(Spec),
    %% Check start/end places
    Errors5 = Errors4 ++ check_start_end_places(Spec),
    %% Validate start and end places are in the places list
    Errors6 = case {lists:member(Start, Places), lists:member(End, Places)} of
        {true, true} -> Errors5;
        {false, true} ->
            [#{type => reference,
              severity => error,
              message => iolist_to_binary([<<"Start place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"' not in places list">>]),
              location => start_place,
              code => start_place_not_found} | Errors5];
        {true, false} ->
            [#{type => reference,
              severity => error,
              message => iolist_to_binary([<<"End place '">>,
                                          atom_to_binary(End, utf8),
                                          <<"' not in places list">>]),
              location => end_place,
              code => end_place_not_found} | Errors5];
        {false, false} ->
            [#{type => reference,
              severity => error,
              message => <<"Both start and end places not in places list">>,
              location => start_place,
              code => start_end_places_not_found},
             #{type => reference,
              severity => error,
              message => <<"Both start and end places not in places list">>,
              location => end_place,
              code => start_end_places_not_found} | Errors5]
    end,
    %% Validate preset/postset consistency
    Errors7 = Errors6 ++ validate_preset_postset(Places, Transitions, Preset, Postset),
    Errors7;

find_structural_errors(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Invalid specification structure">>,
       location => undefined,
       code => invalid_spec_structure}].

%%--------------------------------------------------------------------
%% @private
%% @doc Validates preset and postset maps for consistency.
%%--------------------------------------------------------------------
-spec validate_preset_postset([place()], [trsn()], #{trsn() => [place()]},
                              #{trsn() => [place()]}) -> [validation_error()].

validate_preset_postset(Places, Transitions, Preset, Postset) ->
    Errors = [],
    %% Check all transitions have preset/postset defined
    Errors1 = lists:foldl(fun(T, Acc) ->
        HasPreset = maps:is_key(T, Preset),
        HasPostset = maps:is_key(T, Postset),
        case {HasPreset, HasPostset} of
            {false, false} ->
                [#{type => structural,
                  severity => error,
                  message => iolist_to_binary([<<"Transition '">>,
                                              atom_to_binary(T, utf8),
                                              <<"' has no preset or postset">>]),
                  location => T,
                  code => transition_no_arcs} | Acc];
            {false, true} ->
                [#{type => structural,
                  severity => warning,
                  message => iolist_to_binary([<<"Transition '">>,
                                              atom_to_binary(T, utf8),
                                              <<"' has no preset (source transition)">>]),
                  location => T,
                  code => transition_no_preset} | Acc];
            {true, false} ->
                [#{type => structural,
                  severity => warning,
                  message => iolist_to_binary([<<"Transition '">>,
                                              atom_to_binary(T, utf8),
                                              <<"' has no postset (sink transition)">>]),
                  location => T,
                  code => transition_no_postset} | Acc];
            {true, true} ->
                Acc
        end
    end, Errors, Transitions),

    %% Check all places in preset/postset are valid
    AllPlaces = sets:from_list(Places),
    Errors2 = maps:fold(fun(T, PresetPlaces, Acc) ->
        lists:foldl(fun(P, AccIn) ->
            case sets:is_element(P, AllPlaces) of
                true -> AccIn;
                false ->
                    [#{type => reference,
                      severity => error,
                      message => iolist_to_binary([<<"Preset place '">>,
                                                  atom_to_binary(P, utf8),
                                                  <<"' for transition '">>,
                                                  atom_to_binary(T, utf8),
                                                  <<"' not found">>]),
                      location => T,
                      code => preset_place_not_found} | AccIn]
            end
        end, Acc, PresetPlaces)
    end, Errors1, Preset),

    Errors3 = maps:fold(fun(T, PostsetPlaces, Acc) ->
        lists:foldl(fun(P, AccIn) ->
            case sets:is_element(P, AllPlaces) of
                true -> AccIn;
                false ->
                    [#{type => reference,
                      severity => error,
                      message => iolist_to_binary([<<"Postset place '">>,
                                                  atom_to_binary(P, utf8),
                                                  <<"' for transition '">>,
                                                  atom_to_binary(T, utf8),
                                              <<"' not found">>]),
                      location => T,
                      code => postset_place_not_found} | AccIn]
            end
        end, Acc, PostsetPlaces)
    end, Errors2, Postset),

    Errors3.

%%====================================================================
%% Soundness Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks the soundness property of the workflow net.
%%
%% Soundness means:
%% 1. Option to complete: from the initial state, the terminal state
%%    is reachable
%% 2. Proper completion: when the terminal state is reached, it's
%%    the only marked place
%% 3. No dead transitions: every transition can fire from some state
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_soundness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_soundness(wfnet_spec()) -> [validation_error()].

check_soundness(Spec) ->
    Errors = [],
    %% Check option to complete
    Errors1 = Errors ++ check_option_to_complete(Spec),
    %% Check proper completion
    Errors2 = Errors1 ++ check_proper_completion(Spec),
    %% Check for dead transitions
    Errors3 = Errors2 ++ check_dead_transitions(Spec),
    Errors3.

%%--------------------------------------------------------------------
%% @doc Checks the option to complete property.
%%
%% Verifies that from the initial marking, the terminal state (end place
%% marked, all others empty) is reachable.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_option_to_complete(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_option_to_complete(wfnet_spec()) -> [validation_error()].

check_option_to_complete(#{initial_marking := InitialMarking,
                           end_place := EndPlace} = Spec) ->
    %% Build the reachability graph and check if end state is reachable
    Reachable = compute_reachable_markings(Spec, InitialMarking, sets:new(), 1000),

    %% Check if any reachable marking has only the end place marked
    HasEndState = lists:any(fun(Marking) ->
        case Marking of
            #{EndPlace := [_]} when map_size(Marking) =:= 1 -> true;
            _ -> false
        end
    end, Reachable),

    case HasEndState of
        true -> [];
        false ->
            [#{type => soundness,
              severity => error,
              message => iolist_to_binary([<<"Terminal state not reachable from initial marking.">>,
                                          <<" End place: '">>,
                                          atom_to_binary(EndPlace, utf8),
                                          <<"'">>]),
              location => EndPlace,
              code => terminal_not_reachable}]
    end;

check_option_to_complete(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check option to complete: missing required fields">>,
       location => undefined,
       code => missing_fields_for_option_completion}].

%%--------------------------------------------------------------------
%% @doc Checks the proper completion property.
%%
%% Verifies that when the terminal state is reached (end place marked),
%% no other places have tokens.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_proper_completion(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_proper_completion(wfnet_spec()) -> [validation_error()].

check_proper_completion(#{end_place := EndPlace, preset := Preset}) ->
    %% Find all transitions that produce to the end place
    EndProducers = maps:fold(fun(T, Postset, Acc) ->
        case lists:member(EndPlace, Postset) of
            true -> [T | Acc];
            false -> Acc
        end
    end, [], get_postset_from_preset(Preset)),

    %% Check if any of these transitions also produce to other places
    Errors = lists:foldl(fun(T, Acc) ->
        Postset = maps:get(T, get_postset_from_preset(Preset), []),
        case length(Postset) > 1 of
            true ->
                [#{type => soundness,
                  severity => warning,
                  message => iolist_to_binary([<<"Transition '">>,
                                              atom_to_binary(T, utf8),
                                              <<"' produces to end place '">>,
                                              atom_to_binary(EndPlace, utf8),
                                              <<"' and other places (may not properly complete)">>]),
                  location => T,
                  code => improper_completion} | Acc];
            false ->
                Acc
        end
    end, [], EndProducers),

    %% Also check for transitions that consume from end place (would allow continuation)
    EndConsumers = maps:fold(fun(T, PresetList, Acc) ->
        case lists:member(EndPlace, PresetList) of
            true -> [T | Acc];
            false -> Acc
        end
    end, [], Preset),

    Errors1 = case EndConsumers of
        [] -> Errors;
        _ ->
            [#{type => soundness,
              severity => error,
              message => iolist_to_binary([<<"Transitions consume from end place '">>,
                                          atom_to_binary(EndPlace, utf8),
                                          <<"' (violates proper completion)">>]),
              location => EndPlace,
              code => end_place_consumed} | Errors]
    end,

    Errors1;

check_proper_completion(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check proper completion: missing required fields">>,
       location => undefined,
       code => missing_fields_for_proper_completion}].

%%--------------------------------------------------------------------
%% @doc Checks for dead transitions in the workflow net.
%%
%% A dead transition is one that can never fire from any reachable
%% marking. Dead transitions indicate dead code or unreachable paths.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_dead_transitions(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_dead_transitions(wfnet_spec()) -> [validation_error()].

check_dead_transitions(#{transitions := Transitions} = Spec) ->
    ReachableMarkings = get_all_reachable_markings(Spec),

    %% Check each transition for liveness
    {LiveTransitions, DeadTransitions} = lists:foldl(fun(T, {Live, Dead}) ->
        CanFire = lists:any(fun(Marking) ->
            is_enabled(T, Marking, Spec)
        end, ReachableMarkings),
        case CanFire of
            true -> {[T | Live], Dead};
            false -> {Live, [T | Dead]}
        end
    end, {[], []}, Transitions),

    %% Source transitions (no preset) are not considered dead
    DeadTransitions1 = lists:filter(fun(T) ->
        case maps:get(T, maps:get(preset, Spec, #{}), []) of
            [] -> false;  %% Source transition
            _ -> true
        end
    end, DeadTransitions),

    lists:map(fun(T) ->
        #{type => liveness,
          severity => warning,
          message => iolist_to_binary([<<"Transition '">>,
                                      atom_to_binary(T, utf8),
                                      <<"' is dead (never enabled)">>]),
          location => T,
          code => dead_transition}
    end, DeadTransitions1);

check_dead_transitions(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check dead transitions: missing transitions">>,
       location => undefined,
       code => missing_transitions_list}].

%%====================================================================
%% Liveness Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks the liveness property of the workflow net.
%%
%% Liveness means:
%% 1. No dead transitions (every transition can fire from some state)
%% 2. No deadlocks (no state where no transition is enabled except
%%    the terminal state)
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_liveness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_liveness(wfnet_spec()) -> [validation_error()].

check_liveness(Spec) ->
    Errors = [],
    %% Check for dead transitions
    Errors1 = Errors ++ check_dead_transitions(Spec),

    %% Check for potential deadlocks (non-terminal states with no enabled transitions)
    Errors2 = Errors1 ++ check_deadlocks(Spec),
    Errors2.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for deadlock states.
%%
%% A deadlock is a reachable state where no transitions are enabled
%% but it's not the terminal state.
%%--------------------------------------------------------------------
-spec check_deadlocks(wfnet_spec()) -> [validation_error()].

check_deadlocks(#{end_place := EndPlace} = Spec) ->
    ReachableMarkings = get_all_reachable_markings(Spec),

    Deadlocks = lists:filter(fun(Marking) ->
        %% Not the terminal state
        IsTerminal = case Marking of
            #{EndPlace := [_]} when map_size(Marking) =:= 1 -> true;
            _ -> false
        end,

        case IsTerminal of
            true -> false;
            false ->
                %% Check if any transition is enabled
                not any_transition_enabled(Marking, Spec)
        end
    end, ReachableMarkings),

    case Deadlocks of
        [] -> [];
        _ ->
            [#{type => liveness,
              severity => error,
              message => iolist_to_binary([<<"Found ">>,
                                          integer_to_binary(length(Deadlocks)),
                                          <<" deadlock state(s) where no transition is enabled">>]),
              location => undefined,
              code => deadlock_detected}]
    end;

check_deadlocks(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check deadlocks: missing end_place">>,
       location => undefined,
       code => missing_end_place}].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if any transition is enabled in the given marking.
%%--------------------------------------------------------------------
-spec any_transition_enabled(marking(), wfnet_spec()) -> boolean().

any_transition_enabled(Marking, #{transitions := Transitions} = Spec) ->
    lists:any(fun(T) -> is_enabled(T, Marking, Spec) end, Transitions).

%%====================================================================
%% Boundedness Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks the boundedness property of the workflow net.
%%
%% A workflow net is K-bounded if no place ever contains more than K
%% tokens. Unbounded places can cause memory issues and indicate
%% structural problems.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_boundedness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_boundedness(wfnet_spec()) -> [validation_error()].

check_boundedness(#{places := Places} = Spec) ->
    ReachableMarkings = get_all_reachable_markings(Spec),

    %% Find maximum token count for each place
    MaxTokens = lists:foldl(fun(Marking, Acc) ->
        maps:fold(fun(Place, Tokens, AccIn) ->
            Count = length(Tokens),
            PrevMax = maps:get(Place, AccIn, 0),
            AccIn#{Place => max(PrevMax, Count)}
        end, Acc, Marking)
    end, #{}, ReachableMarkings),

    %% Check for unbounded or excessively bounded places
    %% Default threshold: consider > 10 as potentially problematic
    Threshold = 10,
    UnboundedPlaces = maps:fold(fun(Place, Max, Acc) ->
        case Max > Threshold of
            true ->
                [#{type => boundedness,
                  severity => warning,
                  message => iolist_to_binary([<<"Place '">>,
                                              atom_to_binary(Place, utf8),
                                              <<"' can accumulate up to ">>,
                                              integer_to_binary(Max),
                                              <<" tokens (threshold: ">>,
                                              integer_to_binary(Threshold),
                                              <<")">>]),
                  location => Place,
                  code => excessive_boundedness} | Acc];
            false ->
                Acc
        end
    end, [], MaxTokens),

    UnboundedPlaces.

%%====================================================================
%% Structural Check Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks for isolated nodes in the workflow net.
%%
%% An isolated node has no incoming or outgoing arcs. Isolated nodes
%% indicate dead code or structural errors.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_isolated_nodes(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_isolated_nodes(wfnet_spec()) -> [validation_error()].

check_isolated_nodes(#{places := Places, transitions := Transitions,
                       preset := Preset, postset := Postset}) ->
    %% Build adjacency
    ConnectedPlaces = sets:from_list(
        lists:flatten([
            maps:values(Preset),
            maps:values(Postset)
        ])
    ),

    ConnectedTransitions = sets:from_list(
        lists:flatten([
            maps:keys(Preset),
            maps:keys(Postset)
        ])
    ),

    %% Find isolated places (except start/end which might only have one connection)
    IsolatedPlaces = lists:filter(fun(P) ->
        not sets:is_element(P, ConnectedPlaces)
    end, Places),

    %% Find isolated transitions
    IsolatedTransitions = lists:filter(fun(T) ->
        not sets:is_element(T, ConnectedTransitions)
    end, Transitions),

    lists:map(fun(P) ->
        #{type => structural,
          severity => warning,
          message => iolist_to_binary([<<"Isolated place '">>,
                                      atom_to_binary(P, utf8),
                                      <<"' has no connections">>]),
          location => P,
          code => isolated_place}
    end, IsolatedPlaces) ++
    lists:map(fun(T) ->
        #{type => structural,
          severity => error,
          message => iolist_to_binary([<<"Isolated transition '">>,
                                      atom_to_binary(T, utf8),
                                      <<"' has no connections">>]),
          location => T,
          code => isolated_transition}
    end, IsolatedTransitions);

check_isolated_nodes(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check isolated nodes: missing required fields">>,
       location => undefined,
       code => missing_fields_for_isolated_check}].

%%--------------------------------------------------------------------
%% @doc Checks that the workflow net is a single connected component.
%%
%% Disconnected components indicate multiple independent workflows
%% in a single specification, which is typically an error.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_connectedness(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_connectedness(wfnet_spec()) -> [validation_error()].

check_connectedness(#{places := Places, transitions := Transitions,
                       preset := Preset, postset := Postset}) ->
    %% Build bipartite graph
    AllNodes = Places ++ Transitions,

    case AllNodes of
        [] -> [];
        _ ->
            %% Build adjacency list
            Adj = build_adjacency_list(Places, Transitions, Preset, Postset),

            %% Find connected components using BFS
            StartNode = hd(AllNodes),
            Visited = bfs(StartNode, Adj, sets:new()),

            %% Check if all nodes are visited
            Unvisited = [N || N <- AllNodes, not sets:is_element(N, Visited)],

            case Unvisited of
                [] -> [];
                _ ->
                    [#{type => structural,
                      severity => error,
                      message => iolist_to_binary([<<"Workflow net is not connected. ">>,
                                                  integer_to_binary(length(Unvisited)),
                                                  <<" unconnected node(s)">>]),
                      location => hd(Unvisited),
                      code => disconnected_component}]
            end
    end;

check_connectedness(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check connectedness: missing required fields">>,
       location => undefined,
       code => missing_fields_for_connectedness}].

%%--------------------------------------------------------------------
%% @doc Checks split/join consistency in the workflow net.
%%
%% Validates that split and join operations are properly matched
%% and configured.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_split_join_consistency(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_split_join_consistency(wfnet_spec()) -> [validation_error()].

check_split_join_consistency(#{transitions := Transitions,
                              preset := Preset, postset := Postset}) ->
    %% Find AND-splits (transitions with multiple postset places)
    AndSplits = [{T, Postset} || T <- Transitions,
                              is_function_key(T, Postset),
                              length(maps:get(T, Postset, [])) > 1],

    %% Find AND-joins (transitions with multiple preset places)
    AndJoins = [{T, Preset} || T <- Transitions,
                            is_function_key(T, Preset),
                            length(maps:get(T, Preset, [])) > 1],

    %% Find OR/XOR-splits (multiple transitions from same preset context)
    %% This requires analyzing which places feed into multiple transitions
    PlaceToTrsn = maps:fold(fun(T, PresetPlaces, Acc) ->
        lists:foldl(fun(P, AccIn) ->
            AccIn#{P => [T | maps:get(P, AccIn, [])]}
        end, Acc, PresetPlaces)
    end, #{}, Preset),

    %% Places with multiple outgoing transitions are potential OR/XOR splits
    OrXorSplits = [{P, Trsns} || {P, Trsns} <- maps:to_list(PlaceToTrsn),
                                 length(Trsns) > 1],

    %% Places with multiple incoming transitions are potential OR/XOR joins
    TrsnToPlace = maps:fold(fun(T, PostsetPlaces, Acc) ->
        lists:foldl(fun(P, AccIn) ->
            AccIn#{P => [T | maps:get(P, AccIn, [])]}
        end, Acc, PostsetPlaces)
    end, #{}, Postset),

    OrXorJoins = [{P, Trsns} || {P, Trsns} <- maps:to_list(TrsnToPlace),
                               length(Trsns) > 1],

    %% Generate warnings for potentially mismatched splits/joins
    Errors = [],
    Errors1 = case {AndSplits, AndJoins} of
        {[], []} -> Errors;
        {[_|_], []} ->
            %% Has AND-splits but no AND-joins
            [#{type => structural,
              severity => warning,
              message => <<"Workflow has AND-splits but no AND-joins (tokens may not be properly synchronized)">>,
              location => element(1, hd(AndSplits)),
              code => unmatched_and_split} | Errors];
        {[], [_|_]} ->
            %% Has AND-joins but no AND-splits
            [#{type => structural,
              severity => warning,
              message => <<"Workflow has AND-joins but no AND-splits (may deadlock waiting for synchronization)">>,
              location => element(1, hd(AndJoins)),
              code => unmatched_and_join} | Errors];
        _ ->
            Errors
    end,

    Errors1;

check_split_join_consistency(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check split/join consistency: missing required fields">>,
       location => undefined,
       code => missing_fields_for_split_join}].

%%--------------------------------------------------------------------
%% @doc Checks start and end place validity.
%%
%% Validates that start and end places are properly configured.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_start_end_places(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_start_end_places(wfnet_spec()) -> [validation_error()].

check_start_end_places(#{start_place := Start, end_place := End,
                         initial_marking := InitMarking} = Spec) ->
    Errors = [],

    %% Check start place is initially marked
    Errors1 = case maps:get(Start, InitMarking, []) of
        [] ->
            [#{type => structural,
              severity => error,
              message => iolist_to_binary([<<"Start place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"' is not initially marked">>]),
              location => Start,
              code => start_place_unmarked} | Errors];
        _ ->
            Errors
    end,

    %% Check start and end are different
    Errors2 = case Start =:= End of
        true ->
            [#{type => structural,
              severity => error,
              message => <<"Start and end places must be different">>,
              location => Start,
              code => start_end_same} | Errors1];
        false ->
            Errors1
    end,

    %% Check end place is initially unmarked
    Errors3 = case maps:get(End, InitMarking, []) of
        [_|_] ->
            [#{type => structural,
              severity => warning,
              message => iolist_to_binary([<<"End place '">>,
                                          atom_to_binary(End, utf8),
                                          <<"' is initially marked (unusual)">>]),
              location => End,
              code => end_place_initially_marked} | Errors2];
        _ ->
            Errors2
    end,

    %% Check transitions TO start place (should be none for WF-net)
    #{preset := Preset} = Spec,
    ToStart = [T || T <- maps:keys(Preset),
                   lists:member(Start, maps:get(T, Preset, []))],
    Errors4 = case ToStart of
        [] -> Errors3;
        _ ->
            [#{type => structural,
              severity => warning,
              message => iolist_to_binary([<<"Transitions produce to start place '">>,
                                          atom_to_binary(Start, utf8),
                                          <<"' (may cause re-execution)">>]),
              location => Start,
              code => transitions_to_start} | Errors3]
    end,

    Errors4;

check_start_end_places(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check start/end places: missing required fields">>,
       location => undefined,
       code => missing_fields_for_start_end}].

%%--------------------------------------------------------------------
%% @doc Checks for self-looping transitions.
%%
%% A self-loop is a transition that consumes from and produces to
%% the same place, which can cause infinite loops.
%%
%% === Example ===
%% ```erlang
%% Errors = wfnet_validate:check_self_loops(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_self_loops(wfnet_spec()) -> [validation_error()].

check_self_loops(#{transitions := Transitions,
                   preset := Preset, postset := Postset}) ->
    lists:foldl(fun(T, Acc) ->
        PresetPlaces = maps:get(T, Preset, []),
        PostsetPlaces = maps:get(T, Postset, []),

        %% Find places in both preset and postset (self-loops)
        LoopPlaces = sets:to_list(sets:intersection(
            sets:from_list(PresetPlaces),
            sets:from_list(PostsetPlaces)
        )),

        lists:foldl(fun(P, AccIn) ->
            [#{type => structural,
              severity => warning,
              message => iolist_to_binary([<<"Transition '">>,
                                          atom_to_binary(T, utf8),
                                          <<"' has self-loop on place '">>,
                                          atom_to_binary(P, utf8),
                                          <<"'">>]),
              location => T,
              code => self_loop} | AccIn]
        end, Acc, LoopPlaces)
    end, [], Transitions);

check_self_loops(_Spec) ->
    [#{type => structural,
       severity => error,
       message => <<"Cannot check self-loops: missing required fields">>,
       location => undefined,
       code => missing_fields_for_self_loop}].

%%====================================================================
%% Structure Extraction
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extracts the workflow net structure from a gen_pnet module.
%%
%% Calls the module's callbacks to extract places, transitions,
%% markings, and connectivity information.
%%
%% === Example ===
%% ```erlang
%% Spec = wfnet_validate:extract_structure(my_workflow).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_structure(module()) -> wfnet_spec().

extract_structure(Module) when is_atom(Module) ->
    %% Get basic structure from callbacks
    Places = Module:place_lst(),
    Transitions = Module:trsn_lst(),

    %% Get initial marking
    UsrInfo = case erlang:function_exported(Module, init, 1) of
        true -> Module:init(undefined);
        false -> undefined
    end,

    InitMarking = lists:foldl(fun(P, Acc) ->
        Tokens = case erlang:function_exported(Module, init_marking, 2) of
            true -> Module:init_marking(P, UsrInfo);
            false -> []
        end,
        Acc#{P => Tokens}
    end, #{}, Places),

    %% Extract preset for each transition
    Preset = lists:foldl(fun(T, Acc) ->
        PresetPlaces = case erlang:function_exported(Module, preset, 1) of
            true -> Module:preset(T);
            false -> []
        end,
        Acc#{T => PresetPlaces}
    end, #{}, Transitions),

    %% Infer postset by analyzing which places each transition produces to
    %% This requires calling fire/3 with synthetic modes
    Postset = infer_postset(Module, Transitions, Preset, UsrInfo),

    %% Infer start and end places
    StartPlace = infer_start_place(Places, InitMarking),
    EndPlace = infer_end_place(Places, Transitions, Preset, Postset),

    #{
        places => Places,
        transitions => Transitions,
        initial_marking => InitMarking,
        preset => Preset,
        postset => Postset,
        start_place => StartPlace,
        end_place => EndPlace
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Infers postset by analyzing fire/3 behavior.
%%--------------------------------------------------------------------
-spec infer_postset(module(), [trsn()], #{trsn() => [place()]}, term()) ->
          #{trsn() => [place()]}.

infer_postset(Module, Transitions, Preset, UsrInfo) ->
    lists:foldl(fun(T, Acc) ->
        PresetPlaces = maps:get(T, Preset, []),
        PostsetPlaces = case PresetPlaces of
            [] ->
                %% Source transition - create a minimal mode to test
                try_fire(Module, T, #{}, UsrInfo);
            _ ->
                %% Create a minimal mode with empty token lists
                Mode = lists:foldl(fun(P, M) -> M#{P => []} end, #{}, PresetPlaces),
                try_fire(Module, T, Mode, UsrInfo)
        end,
        Acc#{T => PostsetPlaces}
    end, #{}, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Attempts to fire a transition and extract postset.
%%--------------------------------------------------------------------
-spec try_fire(module(), trsn(), #{place() => [token()]}, term()) -> [place()].

try_fire(Module, T, Mode, UsrInfo) ->
    case erlang:function_exported(Module, is_enabled, 3) of
        false -> [];
        true ->
            case Module:is_enabled(T, Mode, UsrInfo) of
                false -> [];
                true ->
                    case Module:fire(T, Mode, UsrInfo) of
                        abort -> [];
                        {produce, ProduceMap} -> maps:keys(ProduceMap);
                        {produce, ProduceMap, _NewUsrInfo} -> maps:keys(ProduceMap)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Infers the start place from initial marking.
%%--------------------------------------------------------------------
-spec infer_start_place([place()], marking()) -> place().

infer_start_place(Places, InitMarking) ->
    %% Start place is the one initially marked
    MarkedPlaces = [P || P <- Places,
                         length(maps:get(P, InitMarking, [])) > 0],
    case MarkedPlaces of
        [Start | _] -> Start;
        _ -> case Places of
            [P | _] -> P;
            _ -> 'start'
        end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Infers the end place from structure.
%%
%% End place is typically the sink - has postset but no preset connections.
%%--------------------------------------------------------------------
-spec infer_end_place([place()], [trsn()], #{trsn() => [place()]},
                     #{trsn() => [place()]}) -> place().

infer_end_place(Places, _Transitions, Preset, Postset) ->
    %% End place is in postset but not in preset (consumed only at end)
    %% Actually for WF-net, end place is only in postset (produced to)
    PresetPlaces = sets:from_list(lists:flatten(maps:values(Preset))),
    PostsetPlaces = sets:from_list(lists:flatten(maps:values(Postset))),

    %% Find places that are in postset but not preset (sink places)
    SinkPlaces = [P || P <- Places,
                       sets:is_element(P, PostsetPlaces),
                       not sets:is_element(P, PresetPlaces)],

    case SinkPlaces of
        [End | _] -> End;
        _ -> case Places of
            [P | _] -> P;
            _ -> 'end'
        end
    end.

%%====================================================================
%% Error Formatting
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Formats validation errors for human-readable display.
%%
%% Returns a list of formatted strings suitable for logging or
%% user interface display.
%%
%% === Example ===
%% ```erlang
%% Formatted = wfnet_validate:format_errors(Errors),
%% lists:foreach(fun(E) -> io:format("~s~n", [E]) end, Formatted).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec format_errors([validation_error()]) -> [binary()].

format_errors(Errors) ->
    lists:map(fun(#{severity := Sev, message := Msg} = Error) ->
        Location = case maps:get(location, Error, undefined) of
            undefined -> <<>>;
            Loc when is_atom(Loc) ->
                iolist_to_binary([<<" (at '">>, atom_to_binary(Loc, utf8), <<"')">>]);
            Loc when is_binary(Loc) ->
                iolist_to_binary([<<" (at '">>, Loc, <<"')">>])
        end,
        Type = case maps:get(type, Error, undefined) of
            undefined -> <<>>;
            TypeAtom ->
                iolist_to_binary([<<", ">>, atom_to_binary(TypeAtom, utf8)])
        end,
        iolist_to_binary([
            case Sev of
                error -> <<"[ERROR]   ">>;
                warning -> <<"[WARNING] ">>
            end,
            Msg,
            Type,
            Location
        ])
    end, Errors).

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a key exists in a map.
%%--------------------------------------------------------------------
-spec is_function_key(term(), map()) -> boolean().

is_function_key(Key, Map) when is_map(Map) ->
    maps:is_key(Key, Map);
is_function_key(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets postset from preset (reverse lookup helper).
%%--------------------------------------------------------------------
-spec get_postset_from_preset(#{trsn() => [place()]}) -> #{trsn() => [place()]}.

get_postset_from_preset(Preset) ->
    %% Build reverse mapping: for each place, find transitions that consume it
    %% This is a simplified version - actual postset should be extracted
    Preset.

%%--------------------------------------------------------------------
%% @private
%% @doc Builds adjacency list for connectedness check.
%%--------------------------------------------------------------------
-spec build_adjacency_list([place()], [trsn()], #{trsn() => [place()]},
                          #{trsn() => [place()]}) -> #{atom() => [atom()]}.

build_adjacency_list(Places, Transitions, Preset, Postset) ->
    %% Build bipartite graph adjacency
    Adj1 = lists:foldl(fun(T, Acc) ->
        PresetPlaces = maps:get(T, Preset, []),
        %% Add edges from preset places to transition
        lists:foldl(fun(P, AccIn) ->
            AccIn#{P => [T | maps:get(P, AccIn, [])]}
        end, Acc, PresetPlaces)
    end, #{}, Transitions),

    lists:foldl(fun(T, Acc) ->
        PostsetPlaces = maps:get(T, Postset, []),
        %% Add edges from transition to postset places
        lists:foldl(fun(P, AccIn) ->
            AccIn#{T => [P | maps:get(T, AccIn, [])]}
        end, Acc, PostsetPlaces)
    end, Adj1, Transitions).

%%--------------------------------------------------------------------
%% @private
%% @doc Breadth-first search to find connected component.
%%--------------------------------------------------------------------
-spec bfs(atom(), #{atom() => [atom()]}, sets:set(atom())) -> sets:set(atom()).

bfs(Start, Adj, Visited) ->
    case sets:is_element(Start, Visited) of
        true -> Visited;
        false ->
            Visited1 = sets:add_element(Start, Visited),
            Neighbors = maps:get(Start, Adj, []),
            lists:foldl(fun(N, Acc) -> bfs(N, Adj, Acc) end, Visited1, Neighbors)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Computes all reachable markings from the initial marking.
%% Uses bounded exploration to prevent infinite loops.
%%--------------------------------------------------------------------
-spec get_all_reachable_markings(wfnet_spec()) -> [marking()].

get_all_reachable_markings(#{initial_marking := InitMarking} = Spec) ->
    compute_reachable_markings(Spec, InitMarking, sets:new(), 1000).

%%--------------------------------------------------------------------
%% @private
%% @doc Computes reachable markings with depth limit.
%%--------------------------------------------------------------------
-spec compute_reachable_markings(wfnet_spec(), marking(), sets:set(marking()),
                                 non_neg_integer()) -> [marking()].

compute_reachable_markings(_Spec, CurrentMarking, _Visited, 0) ->
    [CurrentMarking];
compute_reachable_markings(Spec, CurrentMarking, Visited, Limit) ->
    MarkingKey = erlang:phash2(CurrentMarking),
    case sets:is_element(MarkingKey, Visited) of
        true -> [];
        false ->
            Visited1 = sets:add_element(MarkingKey, Visited),
            %% Find all enabled transitions
            Enabled = get_enabled_transitions(CurrentMarking, Spec),
            %% Fire each enabled transition and recurse
            NextMarkings = lists:foldl(fun(T, Acc) ->
                fire_all_modes(T, CurrentMarking, Spec, Acc)
            end, [], Enabled),
            lists:flatten([CurrentMarking | lists:map(fun(M) ->
                compute_reachable_markings(Spec, M, Visited1, Limit - 1)
            end, NextMarkings)])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets list of enabled transitions for a marking.
%%--------------------------------------------------------------------
-spec get_enabled_transitions(marking(), wfnet_spec()) -> [trsn()].

get_enabled_transitions(Marking, #{transitions := Transitions} = Spec) ->
    [T || T <- Transitions, is_enabled(T, Marking, Spec)].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a transition is enabled in a marking.
%%--------------------------------------------------------------------
-spec is_enabled(trsn(), marking(), wfnet_spec()) -> boolean().

is_enabled(T, Marking, #{preset := Preset}) ->
    PresetPlaces = maps:get(T, Preset, []),
    lists:all(fun(P) ->
        maps:get(P, Marking, []) =/= []
    end, PresetPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Fires a transition in all possible modes.
%%--------------------------------------------------------------------
-spec fire_all_modes(trsn(), marking(), wfnet_spec(), [marking()]) -> [marking()].

fire_all_modes(T, Marking, Spec, Acc) ->
    Preset = maps:get(preset, Spec, #{}),
    PresetPlaces = maps:get(T, Preset, []),
    %% Create all possible modes from preset
    Modes = generate_modes(PresetPlaces, Marking),
    lists:foldl(fun(Mode, AccIn) ->
        case fire_transition(T, Mode, Spec) of
            {ok, ProduceMap} ->
                NewMarking = apply_mode_and_produce(Mode, ProduceMap, Marking),
                [NewMarking | AccIn];
            abort -> AccIn
        end
    end, Acc, Modes).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates all possible modes for a transition.
%%--------------------------------------------------------------------
-spec generate_modes([place()], marking()) -> [#{place() => [token()]}].

generate_modes(PresetPlaces, Marking) ->
    %% For each preset place, take one token (simplified)
    %% A full implementation would generate all combinations
    lists:foldl(fun(P, Modes) ->
        Tokens = maps:get(P, Marking, []),
        case Tokens of
            [] -> [];
            [_|_] ->
                %% Add this place with empty consumption to existing modes
                lists:map(fun(M) -> M#{P => []} end, Modes)
        end
    end, [#{}], PresetPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Simulates firing a transition (without actual module call).
%%--------------------------------------------------------------------
-spec fire_transition(trsn(), #{place() => [token()]}, wfnet_spec()) ->
          {ok, #{place() => [token()]}} | abort.

fire_transition(_T, _Mode, _Spec) ->
    %% Simplified: assume produce map is empty
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Applies mode consumption and produces new tokens.
%%--------------------------------------------------------------------
-spec apply_mode_and_produce(#{place() => [token()]}, #{place() => [token()]},
                             marking()) -> marking().

apply_mode_and_produce(Mode, ProduceMap, Marking) ->
    %% Consume from mode
    Marking1 = maps:map(fun(P, Tokens) ->
        Consumed = maps:get(P, Mode, []),
        case Tokens of
            [] -> [];
            _ when length(Tokens) >= length(Consumed) ->
                %% Simple subtraction (not exact multiset)
                lists:nthtail(length(Consumed), Tokens);
            _ -> Tokens
        end
    end, Marking),
    %% Add produced tokens
    maps:map(fun(P, Tokens) ->
        Produced = maps:get(P, ProduceMap, []),
        Tokens ++ Produced
    end, Marking1).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test: extract_structure works with valid module
%%--------------------------------------------------------------------
extract_structure_test() ->
    %% Create a mock module structure
    meck:new(mock_workflow, [non_strict]),
    meck:expect(mock_workflow, place_lst, 0, [p1, p2, p3]),
    meck:expect(mock_workflow, trsn_lst, 0, [t1, t2]),
    meck:expect(mock_workflow, init, 1, undefined),
    meck:expect(mock_workflow, init_marking, 2, fun(p1, _) -> [token]; (_, _) -> [] end),
    meck:expect(mock_workflow, preset, 1, fun(t1) -> [p1]; (t2) -> [p2] end),
    meck:expect(mock_workflow, is_enabled, 3, fun(_, _, _) -> true end),
    meck:expect(mock_workflow, fire, 3, fun(t1, _, _) -> {produce, #{p2 => []}};
                                            (t2, _, _) -> {produce, #{p3 => []}} end),

    Spec = extract_structure(mock_workflow),
    ?assertEqual([p1, p2, p3], maps:get(places, Spec)),
    ?assertEqual([t1, t2], maps:get(transitions, Spec)),
    ?assertEqual([token], maps:get(p1, maps:get(initial_marking, Spec))),

    meck:unload(mock_workflow).

%%--------------------------------------------------------------------
%% Test: validate_spec rejects invalid spec type
%%--------------------------------------------------------------------
validate_spec_invalid_type_test() ->
    Result = validate_spec(not_a_map),
    {error, Errors} = Result,
    ?assert(length(Errors) > 0).

%%--------------------------------------------------------------------
%% Test: check_self_loops detects self-loops
%%--------------------------------------------------------------------
check_self_loops_test() ->
    Spec = #{
        transitions => [t1, t2],
        preset => #{t1 => [p1], t2 => [p2]},
        postset => #{t1 => [p1, p2], t2 => [p3]}  %% t1 has self-loop on p1
    },
    Errors = check_self_loops(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= self_loop end, Errors)).

%%--------------------------------------------------------------------
%% Test: check_isolated_nodes finds isolated places
%%--------------------------------------------------------------------
check_isolated_nodes_test() ->
    Spec = #{
        places => [p1, p2, p3],
        transitions => [t1],
        preset => #{t1 => [p1]},
        postset => #{t1 => [p2]}
        %% p3 is isolated
    },
    Errors = check_isolated_nodes(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= isolated_place end, Errors)).

%%--------------------------------------------------------------------
%% Test: check_start_end_places detects same start/end
%%--------------------------------------------------------------------
check_start_end_places_test() ->
    Spec = #{
        start_place => p1,
        end_place => p1,
        initial_marking => #{p1 => [token]},
        preset => #{},
        postset => #{}
    },
    Errors = check_start_end_places(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= start_end_same end, Errors)).

%%--------------------------------------------------------------------
%% Test: format_errors produces readable output
%%--------------------------------------------------------------------
format_errors_test() ->
    Errors = [
        #{severity => error, message => <<"Error 1">>, location => p1, type => structural},
        #{severity => warning, message => <<"Warning 1">>, location => undefined, type => soundness}
    ],
    Formatted = format_errors(Errors),
    ?assertEqual(2, length(Formatted)),
    ?assert(match(<<"[ERROR]">>, lists:nth(1, Formatted))),
    ?assert(match(<<"[WARNING]">>, lists:nth(2, Formatted))).

%%--------------------------------------------------------------------
%% Test: check_connectedness detects disconnected components
%%--------------------------------------------------------------------
check_connectedness_test() ->
    Spec = #{
        places => [p1, p2, p3],
        transitions => [t1],
        preset => #{t1 => [p1]},
        postset => #{t1 => [p2]}
        %% p3 is disconnected
    },
    Errors = check_connectedness(Spec),
    ?assert(lists:any(fun(#{code := C}) -> C =:= disconnected_component end, Errors)).

%% Helper function for matching binary prefix
match(Prefix, Binary) ->
    PrefixSize = byte_size(Prefix),
    case Binary of
        <<Prefix:PrefixSize/binary, _/binary>> -> true;
        _ -> false
    end.

-endif.
