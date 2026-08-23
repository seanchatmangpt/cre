%% -*- erlang -*-
%%%% @doc Workflow Net Type Definitions
%%
%% Comprehensive type system for gen_wfnet including:
%% - Petri net structure types
%% - Workflow specification types
%% - Validation result types
%% - Execution state types
%%
%% This module provides type definitions and validators for workflow
%% nets built on gen_pnet. Types are designed for workflow analysis,
%% validation, and execution monitoring.
%%
%% <h3>Type Categories</h3>
%% <ul>
%%   <li><strong>Basic Types:</strong> place, trsn, token</li>
%%   <li><strong>State Types:</strong> marking, mode, produce_map</li>
%%   <li><strong>Specification Types:</strong> workflow_spec, net_structure</li>
%%   <li><strong>Execution Types:</strong> wfnet_status, case_id, case_status</li>
%%   <li><strong>Validation Types:</strong> validation_result, validation_error, soundness_result</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_types).
-author("CRE Team").
-moduledoc """
Type definitions and validators for workflow nets.

This module provides a comprehensive type system for workflow nets
built on gen_pnet, including validation result types, soundness
analysis types, and execution state types.

All validators are total: they return true/false and never crash.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Basic Types
-export([is_place/1,
         is_trsn/1,
         is_token/1]).

%% State Types
-export([is_marking/1,
         is_mode/1,
         is_produce_map/1]).

%% Specification Types
-export([is_workflow_spec/1,
         is_net_structure/1]).

%% Execution Types
-export([is_wfnet_status/1,
         is_case_id/1,
         is_case_status/1]).

%% Validation Types
-export([is_validation_result/1,
         is_validation_error/1,
         is_validation_warning/1,
         is_v_error_category/1,
         is_soundness_result/1,
         is_boundedness/1,
         is_k_bound/1,
         is_soundness_violation/1]).

%% Type Conversion Helpers
-export([status_to_binary/1,
         status_from_binary/1,
         case_status_to_binary/1,
         case_status_from_binary/1]).

%%====================================================================
%% Basic Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the workflow net.
%%
%% Places hold tokens and represent workflow states. Places are atoms
%% for efficient pattern matching.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the workflow net.
%%
%% Transitions consume and produce tokens when fired. Transitions are
%% atoms representing workflow steps.
%%--------------------------------------------------------------------
-type trsn() :: atom().

%%--------------------------------------------------------------------
%% @doc A token on a place.
%%
%% Tokens can be any Erlang term carrying workflow data. This allows
%% flexible representation of workflow items, tasks, and data.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token lists.
%%
%% Represents the current state of the workflow. Each place atom maps
%% to a list of tokens currently in that place.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A mode selects specific tokens from places for consumption.
%%
%% Used during transition firing to specify which tokens will be
%% consumed from each input place.
%%--------------------------------------------------------------------
-type mode() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A produce map specifies tokens to add to places.
%%
%% Returned by fire/3 callbacks to indicate which tokens should be
%% added to which output places after transition firing.
%%--------------------------------------------------------------------
-type produce_map() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc Status of a workflow net instance.
%%
%% Represents the lifecycle state of a workflow net process.
%%--------------------------------------------------------------------
-type wfnet_status() :: idle | running | paused | completed | failed | cancelled.

%%--------------------------------------------------------------------
%% @doc Unique identifier for a workflow case instance.
%%
%% Binary identifier for workflow instances, typically UUID v4 format.
%%--------------------------------------------------------------------
-type case_id() :: binary().

%%--------------------------------------------------------------------
%% @doc Status of a workflow case.
%%
%% Represents the execution state of a workflow case instance.
%%--------------------------------------------------------------------
-type case_status() :: created | running | suspended | completed | failed | cancelled.

%%--------------------------------------------------------------------
%% @doc Categories of validation errors.
%%
%% Classifies errors detected during workflow validation.
%%--------------------------------------------------------------------
-type v_error_category() ::
    soundness_error          %% Workflow cannot complete properly
    | deadlock_error         %% Detected deadlock
    | unbounded_error        %% Unbounded token accumulation
    | structural_error       %% Invalid graph structure
    | reference_error        %% Invalid references
    | type_error.            %% Type mismatches

%%--------------------------------------------------------------------
%% @doc A validation error with category and details.
%%
%% Represents a critical issue found during workflow validation that
%% must be resolved before the workflow can execute correctly.
%%--------------------------------------------------------------------
-type validation_error() :: #{
    category := v_error_category(),
    severity := error,
    message := binary(),
    location => binary() | atom(),
    code := atom(),
    details => map()
}.

%%--------------------------------------------------------------------
%% @doc A validation warning for non-critical issues.
%%
%% Represents a non-critical issue found during workflow validation
%% that should be reviewed but does not prevent execution.
%%--------------------------------------------------------------------
-type validation_warning() :: #{
    category := atom(),
    severity := warning,
    message := binary(),
    location => binary() | atom(),
    code := atom()
}.

%%--------------------------------------------------------------------
%% @doc Result of workflow validation.
%%
%% Returns success with optional warnings, or failure with a list of
%% errors that must be addressed.
%%--------------------------------------------------------------------
-type validation_result() :: {ok, [validation_warning()]} | {error, [validation_error()]}.

%%--------------------------------------------------------------------
%% @doc Boundedness analysis result.
%%
%% Indicates whether all places in the workflow are bounded (have a
%% maximum token count).
%%--------------------------------------------------------------------
-type boundedness() ::
    bounded                    %% All places bounded
    | {bounded, k_bound()}     %% Bounded with k tokens per place
    | unbounded.               %% At least one place unbounded

%%--------------------------------------------------------------------
%% @doc Maximum tokens for a place (k-bounded).
%%
%% Maps each place to its maximum token count, or 'unlimited' if the
%% place has no upper bound.
%%--------------------------------------------------------------------
-type k_bound() :: #{place() => pos_integer() | unlimited}.

%%--------------------------------------------------------------------
%% @doc Soundness violations detected during validation.
%%
%% Represents specific violations of workflow soundness properties.
%%--------------------------------------------------------------------
-type soundness_violation() ::
    {dead_transitions, [trsn()]}                 %% Transitions that can never fire
    | {deadlock, [place()]}                      %% Potential deadlock states
    | {livelock, [trsn()]}                       %% Potential livelock cycles
    | {unbounded_place, [place()]}.              %% Places with unbounded tokens

%%--------------------------------------------------------------------
%% @doc Result of soundness analysis.
%%
%% Returns 'sound' with boundedness info, or 'unsound' with a list
%% of detected violations.
%%--------------------------------------------------------------------
-type soundness_result() ::
    {sound, boundedness()}                       %% Fully sound
    | {unsound, [soundness_violation()]}.        %% Violations found

%%--------------------------------------------------------------------
%% @doc A workflow specification defines the net structure.
%%
%% Contains all information needed to define a workflow net structure.
%%--------------------------------------------------------------------
-type workflow_spec() :: #{
    places := [place()],
    transitions := [trsn()],
    start_place := place(),
    end_place := place(),
    preset := #{trsn() => [place()]},
    postset := #{trsn() => [place()]},
    optional => map()
}.

%%--------------------------------------------------------------------
%% @doc Internal representation of net structure.
%%
%% Used internally for validation and analysis.
%%--------------------------------------------------------------------
-type net_structure() :: #{
    places := #{place() => true},
    transitions := #{trsn() => true},
    start_place := place(),
    end_place := place(),
    preset := #{trsn() => [place()]},
    postset := #{trsn() => [place()]},
    reverse_preset := #{place() => [trsn()]},
    reverse_postset := #{place() => [trsn()]}
}.

%% Export all types
-export_type([
    place/0,
    trsn/0,
    token/0,
    marking/0,
    mode/0,
    produce_map/0,
    workflow_spec/0,
    net_structure/0,
    wfnet_status/0,
    case_id/0,
    case_status/0,
    validation_result/0,
    validation_error/0,
    validation_warning/0,
    v_error_category/0,
    soundness_result/0,
    boundedness/0,
    k_bound/0,
    soundness_violation/0
]).

%%====================================================================
%% Basic Type Validators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid place.
%%
%% A valid place is an atom. The function never crashes.
%%
%% ```erlang
%% > wfnet_types:is_place(p1).
%% true
%% > wfnet_types:is_place("p1").
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_place(term()) -> boolean().

is_place(undefined) -> false;
is_place(Term) when is_atom(Term) -> true;
is_place(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid transition.
%%
%% A valid transition is an atom. The function never crashes.
%%
%% ```erlang
%% > wfnet_types:is_trsn(t1).
%% true
%% > wfnet_types:is_trsn(123).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_trsn(term()) -> boolean().

is_trsn(undefined) -> false;
is_trsn(Term) when is_atom(Term) -> true;
is_trsn(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid token.
%%
%% Tokens can be any Erlang term. All terms are valid tokens.
%% The function never crashes.
%%
%% ```erlang
%% > wfnet_types:is_token({any, term}).
%% true
%% > wfnet_types:is_token(undefined).
%% true
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_token(term()) -> boolean().

is_token(_Term) -> true.

%%====================================================================
%% State Type Validators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid marking.
%%
%% A valid marking is a map where all keys are atoms (places) and
%% all values are lists (of tokens). The function never crashes.
%%--------------------------------------------------------------------
-spec is_marking(term()) -> boolean().

is_marking(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        _:_ -> false
    end;
is_marking(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid mode.
%%
%% A valid mode is a map where all keys are atoms (places) and
%% all values are lists (of tokens). The function never crashes.
%%--------------------------------------------------------------------
-spec is_mode(term()) -> boolean().

is_mode(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        _:_ -> false
    end;
is_mode(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid produce_map.
%%
%% A valid produce_map is a map where all keys are atoms (places)
%% and all values are lists (of tokens to produce). The function
%% never crashes.
%%--------------------------------------------------------------------
-spec is_produce_map(term()) -> boolean().

is_produce_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        _:_ -> false
    end;
is_produce_map(_) ->
    false.

%%====================================================================
%% Specification Type Validators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid workflow_spec.
%%
%% A valid workflow_spec is a map with required keys: places (list of atoms),
%% transitions (list of atoms), start_place (atom), end_place (atom),
%% preset (map of atoms to lists), and postset (map of atoms to lists).
%% The function never crashes.
%%--------------------------------------------------------------------
-spec is_workflow_spec(term()) -> boolean().

is_workflow_spec(Term) when is_map(Term) ->
    case Term of
        #{places := Places, transitions := Transitions, start_place := Start,
          end_place := End, preset := Preset, postset := Postset} ->
            is_list(Places) andalso
            lists:all(fun is_place/1, Places) andalso
            is_list(Transitions) andalso
            lists:all(fun is_trsn/1, Transitions) andalso
            is_place(Start) andalso
            is_place(End) andalso
            is_preset_map(Preset) andalso
            is_postset_map(Postset);
        _ ->
            false
    end;
is_workflow_spec(_) ->
    false.

%% @private
-spec is_preset_map(term()) -> boolean().
is_preset_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) ->
                lists:all(fun is_place/1, V);
            (_, _, _) ->
                throw(error)
        end, true, Term)
    catch
        _:_ -> false
    end;
is_preset_map(_) ->
    false.

%% @private
-spec is_postset_map(term()) -> boolean().
is_postset_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) ->
                lists:all(fun is_place/1, V);
            (_, _, _) ->
                throw(error)
        end, true, Term)
    catch
        _:_ -> false
    end;
is_postset_map(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid net_structure.
%%
%% A valid net_structure is an internal representation with places,
%% transitions, and connectivity information. The function never crashes.
%%--------------------------------------------------------------------
-spec is_net_structure(term()) -> boolean().

is_net_structure(Term) when is_map(Term) ->
    case Term of
        #{places := Places, transitions := Transitions, start_place := Start,
          end_place := End, preset := Preset, postset := Postset,
          reverse_preset := RevPreset, reverse_postset := RevPostset} ->
            is_place_set(Places) andalso
            is_trsn_set(Transitions) andalso
            is_place(Start) andalso
            is_place(End) andalso
            is_preset_map(Preset) andalso
            is_preset_map(Postset) andalso
            is_reverse_preset_map(RevPreset) andalso
            is_reverse_postset_map(RevPostset);
        _ ->
            false
    end;
is_net_structure(_) ->
    false.

%% @private
-spec is_place_set(term()) -> boolean().
is_place_set(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, true, _) when is_atom(K) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        _:_ -> false
    end;
is_place_set(_) ->
    false.

%% @private
-spec is_trsn_set(term()) -> boolean().
is_trsn_set(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, true, _) when is_atom(K) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        _:_ -> false
    end;
is_trsn_set(_) ->
    false.

%% @private
-spec is_reverse_preset_map(term()) -> boolean().
is_reverse_preset_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) ->
                lists:all(fun is_trsn/1, V);
            (_, _, _) ->
                throw(error)
        end, true, Term)
    catch
        _:_ -> false
    end;
is_reverse_preset_map(_) ->
    false.

%% @private
-spec is_reverse_postset_map(term()) -> boolean().
is_reverse_postset_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) ->
                lists:all(fun is_trsn/1, V);
            (_, _, _) ->
                throw(error)
        end, true, Term)
    catch
        _:_ -> false
    end;
is_reverse_postset_map(_) ->
    false.

%%====================================================================
%% Execution Type Validators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid wfnet_status.
%%
%% Valid statuses are: idle, running, paused, completed, failed, cancelled.
%% The function never crashes.
%%--------------------------------------------------------------------
-spec is_wfnet_status(term()) -> boolean().

is_wfnet_status(idle) -> true;
is_wfnet_status(running) -> true;
is_wfnet_status(paused) -> true;
is_wfnet_status(completed) -> true;
is_wfnet_status(failed) -> true;
is_wfnet_status(cancelled) -> true;
is_wfnet_status(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid case_id.
%%
%% A valid case_id is a non-empty binary. The function never crashes.
%%--------------------------------------------------------------------
-spec is_case_id(term()) -> boolean().

is_case_id(Term) when is_binary(Term) -> byte_size(Term) > 0;
is_case_id(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid case_status.
%%
%% Valid case statuses are: created, running, suspended, completed,
%% failed, cancelled. The function never crashes.
%%--------------------------------------------------------------------
-spec is_case_status(term()) -> boolean().

is_case_status(created) -> true;
is_case_status(running) -> true;
is_case_status(suspended) -> true;
is_case_status(completed) -> true;
is_case_status(failed) -> true;
is_case_status(cancelled) -> true;
is_case_status(_) -> false.

%%====================================================================
%% Validation Type Validators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid v_error_category.
%%
%% Valid categories are: soundness_error, deadlock_error, unbounded_error,
%% structural_error, reference_error, type_error.
%%--------------------------------------------------------------------
-spec is_v_error_category(term()) -> boolean().

is_v_error_category(soundness_error) -> true;
is_v_error_category(deadlock_error) -> true;
is_v_error_category(unbounded_error) -> true;
is_v_error_category(structural_error) -> true;
is_v_error_category(reference_error) -> true;
is_v_error_category(type_error) -> true;
is_v_error_category(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid validation_error.
%%
%% A validation_error must be a map with required fields: category
%% (v_error_category), severity (must be 'error'), message (binary),
%% and code (atom). Optional fields: location, details.
%%--------------------------------------------------------------------
-spec is_validation_error(term()) -> boolean().

is_validation_error(Term) when is_map(Term) ->
    case Term of
        #{category := Category, severity := error, message := Message, code := Code} ->
            is_v_error_category(Category) andalso
            is_binary(Message) andalso
            is_atom(Code) andalso
            is_optional_location(maps:get(location, Term, undefined)) andalso
            is_optional_details(maps:get(details, Term, undefined));
        _ ->
            false
    end;
is_validation_error(_) ->
    false.

%% @private
-spec is_optional_location(term()) -> boolean().
is_optional_location(Term) when is_binary(Term); is_atom(Term) -> true;
is_optional_location(undefined) -> true;
is_optional_location(_) -> false.

%% @private
-spec is_optional_details(term()) -> boolean().
is_optional_details(Term) when is_map(Term) -> true;
is_optional_details(undefined) -> true;
is_optional_details(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid validation_warning.
%%
%% A validation_warning must be a map with required fields: category
%% (atom), severity (must be 'warning'), message (binary), and code (atom).
%% Optional field: location.
%%--------------------------------------------------------------------
-spec is_validation_warning(term()) -> boolean().

is_validation_warning(Term) when is_map(Term) ->
    case Term of
        #{category := Category, severity := warning, message := Message, code := Code} ->
            is_atom(Category) andalso
            is_binary(Message) andalso
            is_atom(Code) andalso
            is_optional_location(maps:get(location, Term, undefined));
        _ ->
            false
    end;
is_validation_warning(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid validation_result.
%%
%% A validation_result is either {ok, [validation_warning()]} or
%% {error, [validation_error()]}. The function never crashes.
%%--------------------------------------------------------------------
-spec is_validation_result(term()) -> boolean().

is_validation_result({ok, Warnings}) when is_list(Warnings) ->
    lists:all(fun is_validation_warning/1, Warnings);
is_validation_result({error, Errors}) when is_list(Errors) ->
    lists:all(fun is_validation_error/1, Errors);
is_validation_result(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid boundedness.
%%
%% Valid boundedness values are: bounded, {bounded, k_bound()}, unbounded.
%%--------------------------------------------------------------------
-spec is_boundedness(term()) -> boolean().

is_boundedness(bounded) -> true;
is_boundedness({bounded, Bound}) -> is_k_bound(Bound);
is_boundedness(unbounded) -> true;
is_boundedness(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid k_bound.
%%
%% A k_bound is a map where keys are places (atoms) and values are
%% either positive integers or the atom 'unlimited'.
%%--------------------------------------------------------------------
-spec is_k_bound(term()) -> boolean().

is_k_bound(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_integer(V), V > 0 -> ok;
            (K, unlimited, _) when is_atom(K) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        _:_ -> false
    end;
is_k_bound(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid soundness_violation.
%%
%% Valid violations are: {dead_transitions, [trsn()]}, {deadlock, [place()]},
%% {livelock, [trsn()]}, {unbounded_place, [place()]}.
%%--------------------------------------------------------------------
-spec is_soundness_violation(term()) -> boolean().

is_soundness_violation({dead_transitions, Transitions}) when is_list(Transitions) ->
    lists:all(fun is_trsn/1, Transitions);
is_soundness_violation({deadlock, Places}) when is_list(Places) ->
    lists:all(fun is_place/1, Places);
is_soundness_violation({livelock, Transitions}) when is_list(Transitions) ->
    lists:all(fun is_trsn/1, Transitions);
is_soundness_violation({unbounded_place, Places}) when is_list(Places) ->
    lists:all(fun is_place/1, Places);
is_soundness_violation(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid soundness_result.
%%
%% Valid soundness_result values are: {sound, boundedness()} or
%% {unsound, [soundness_violation()]}.
%%--------------------------------------------------------------------
-spec is_soundness_result(term()) -> boolean().

is_soundness_result({sound, Boundedness}) ->
    is_boundedness(Boundedness);
is_soundness_result({unsound, Violations}) when is_list(Violations) ->
    lists:all(fun is_soundness_violation/1, Violations);
is_soundness_result(_) ->
    false.

%%====================================================================
%% Type Conversion Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Convert wfnet_status atom to binary string.
%%
%% ```erlang
%% > wfnet_types:status_to_binary(running).
%% <<"running">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec status_to_binary(wfnet_status()) -> binary().

status_to_binary(idle) -> <<"idle">>;
status_to_binary(running) -> <<"running">>;
status_to_binary(paused) -> <<"paused">>;
status_to_binary(completed) -> <<"completed">>;
status_to_binary(failed) -> <<"failed">>;
status_to_binary(cancelled) -> <<"cancelled">>.

%%--------------------------------------------------------------------
%% @doc Convert binary string to wfnet_status atom.
%%
%% Returns undefined for invalid status strings.
%%
%% ```erlang
%% > wfnet_types:status_from_binary(<<"running">>).
%% running
%% > wfnet_types:status_from_binary(<<"invalid">>).
%% undefined
%% ```
%% @end
%%--------------------------------------------------------------------
-spec status_from_binary(binary()) -> wfnet_status() | undefined.

status_from_binary(<<"idle">>) -> idle;
status_from_binary(<<"running">>) -> running;
status_from_binary(<<"paused">>) -> paused;
status_from_binary(<<"completed">>) -> completed;
status_from_binary(<<"failed">>) -> failed;
status_from_binary(<<"cancelled">>) -> cancelled;
status_from_binary(_) -> undefined.

%%--------------------------------------------------------------------
%% @doc Convert case_status atom to binary string.
%%
%% ```erlang
%% > wfnet_types:case_status_to_binary(running).
%% <<"running">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec case_status_to_binary(case_status()) -> binary().

case_status_to_binary(created) -> <<"created">>;
case_status_to_binary(running) -> <<"running">>;
case_status_to_binary(suspended) -> <<"suspended">>;
case_status_to_binary(completed) -> <<"completed">>;
case_status_to_binary(failed) -> <<"failed">>;
case_status_to_binary(cancelled) -> <<"cancelled">>.

%%--------------------------------------------------------------------
%% @doc Convert binary string to case_status atom.
%%
%% Returns undefined for invalid status strings.
%%
%% ```erlang
%% > wfnet_types:case_status_from_binary(<<"running">>).
%% running
%% > wfnet_types:case_status_from_binary(<<"invalid">>).
%% undefined
%% ```
%% @end
%%--------------------------------------------------------------------
-spec case_status_from_binary(binary()) -> case_status() | undefined.

case_status_from_binary(<<"created">>) -> created;
case_status_from_binary(<<"running">>) -> running;
case_status_from_binary(<<"suspended">>) -> suspended;
case_status_from_binary(<<"completed">>) -> completed;
case_status_from_binary(<<"failed">>) -> failed;
case_status_from_binary(<<"cancelled">>) -> cancelled;
case_status_from_binary(_) -> undefined.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Basic type tests
is_place_test() ->
    ?assert(is_place(p1)),
    ?assert(is_place('Place-1')),
    ?assertNot(is_place("p1")),
    ?assertNot(is_place(123)),
    ?assertNot(is_place(undefined)).

is_trsn_test() ->
    ?assert(is_trsn(t1)),
    ?assert(is_trsn('Transition-A')),
    ?assertNot(is_trsn("t1")),
    ?assertNot(is_trsn(123)).

is_token_test() ->
    ?assert(is_token({any, term})),
    ?assert(is_token(undefined)),
    ?assert(is_token([])),
    ?assert(is_token(#{a => b})).

%% State type tests
is_marking_test() ->
    ?assert(is_marking(#{p1 => [a,b], p2 => []})),
    ?assert(is_marking(#{})),
    ?assertNot(is_marking(#{p1 => a})),
    ?assertNot(is_marking([])),
    ?assertNot(is_marking(#{1 => [a]})).

is_mode_test() ->
    ?assert(is_mode(#{p1 => [a], p2 => [b]})),
    ?assert(is_mode(#{})),
    ?assertNot(is_mode(#{p1 => a})),
    ?assertNot(is_mode([])).

is_produce_map_test() ->
    ?assert(is_produce_map(#{p1 => [a], p2 => []})),
    ?assert(is_produce_map(#{})),
    ?assertNot(is_produce_map(#{p1 => a})).

%% Status tests
is_wfnet_status_test() ->
    ?assert(is_wfnet_status(idle)),
    ?assert(is_wfnet_status(running)),
    ?assert(is_wfnet_status(paused)),
    ?assert(is_wfnet_status(completed)),
    ?assert(is_wfnet_status(failed)),
    ?assert(is_wfnet_status(cancelled)),
    ?assertNot(is_wfnet_status(unknown)),
    ?assertNot(is_wfnet_status("running")).

is_case_status_test() ->
    ?assert(is_case_status(created)),
    ?assert(is_case_status(running)),
    ?assert(is_case_status(suspended)),
    ?assert(is_case_status(completed)),
    ?assert(is_case_status(failed)),
    ?assert(is_case_status(cancelled)),
    ?assertNot(is_case_status(unknown)).

is_case_id_test() ->
    ?assert(is_case_id(<<"case-123">>)),
    ?assert(is_case_id(<<1,2,3>>)),
    ?assertNot(is_case_id(<<>>)),
    ?assertNot(is_case_id("case-123")),
    ?assertNot(is_case_id(undefined)).

%% Validation type tests
is_v_error_category_test() ->
    ?assert(is_v_error_category(soundness_error)),
    ?assert(is_v_error_category(deadlock_error)),
    ?assert(is_v_error_category(unbounded_error)),
    ?assert(is_v_error_category(structural_error)),
    ?assert(is_v_error_category(reference_error)),
    ?assert(is_v_error_category(type_error)),
    ?assertNot(is_v_error_category(unknown)).

is_validation_error_test() ->
    Error1 = #{category => soundness_error,
               severity => error,
               message => <<"Dead transition">>,
               code => dead_transition},
    ?assert(is_validation_error(Error1)),
    Error2 = Error1#{location => module},
    ?assert(is_validation_error(Error2)),
    Error3 = Error1#{details => #{transition => t1}},
    ?assert(is_validation_error(Error3)),
    ?assertNot(is_validation_error(#{category => soundness_error,
                                      severity => warning,
                                      message => <<"Not an error">>,
                                      code => test})),
    ?assertNot(is_validation_error(#{})).

is_validation_warning_test() ->
    Warning = #{category => style,
                severity => warning,
                message => <<"Naming convention">>,
                code => naming},
    ?assert(is_validation_warning(Warning)),
    ?assertNot(is_validation_warning(#{category => style,
                                       severity => error,
                                       message => <<"Not a warning">>,
                                       code => test})).

is_validation_result_test() ->
    ?assert(is_validation_result({ok, []})),
    ?assert(is_validation_result({ok, [#{category => test,
                                         severity => warning,
                                         message => <<"test">>,
                                         code => test}]})),
    ?assert(is_validation_result({error, [#{category => soundness_error,
                                            severity => error,
                                            message => <<"error">>,
                                            code => test}]})),
    ?assertNot(is_validation_result(ok)),
    ?assertNot(is_validation_result({ok, #{}})).

is_boundedness_test() ->
    ?assert(is_boundedness(bounded)),
    ?assert(is_boundedness(unbounded)),
    ?assert(is_boundedness({bounded, #{p1 => 1}})),
    ?assert(is_boundedness({bounded, #{p1 => unlimited}})),
    ?assertNot(is_boundedness({bounded, #{p1 => -1}})),
    ?assertNot(is_boundedness(unknown)).

is_k_bound_test() ->
    ?assert(is_k_bound(#{p1 => 1, p2 => 5})),
    ?assert(is_k_bound(#{p1 => unlimited})),
    ?assert(is_k_bound(#{})),
    ?assertNot(is_k_bound(#{p1 => -1})),
    ?assertNot(is_k_bound(#{p1 => 0})),
    ?assertNot(is_k_bound(#{1 => 1})).

is_soundness_violation_test() ->
    ?assert(is_soundness_violation({dead_transitions, [t1, t2]})),
    ?assert(is_soundness_violation({deadlock, [p1, p2]})),
    ?assert(is_soundness_violation({livelock, [t1]})),
    ?assert(is_soundness_violation({unbounded_place, [p1]})),
    ?assertNot(is_soundness_violation({unknown, []})).

is_soundness_result_test() ->
    ?assert(is_soundness_result({sound, bounded})),
    ?assert(is_soundness_result({sound, {bounded, #{p1 => 1}}})),
    ?assert(is_soundness_result({sound, unbounded})),
    ?assert(is_soundness_result({unsound, [{dead_transitions, [t1]}]})),
    ?assertNot(is_soundness_result({sound, unknown})).

%% Conversion tests
status_to_binary_test() ->
    ?assertEqual(<<"idle">>, status_to_binary(idle)),
    ?assertEqual(<<"running">>, status_to_binary(running)),
    ?assertEqual(<<"completed">>, status_to_binary(completed)).

status_from_binary_test() ->
    ?assertEqual(running, status_from_binary(<<"running">>)),
    ?assertEqual(undefined, status_from_binary(<<"unknown">>)).

case_status_to_binary_test() ->
    ?assertEqual(<<"created">>, case_status_to_binary(created)),
    ?assertEqual(<<"running">>, case_status_to_binary(running)),
    ?assertEqual(<<"failed">>, case_status_to_binary(failed)).

case_status_from_binary_test() ->
    ?assertEqual(running, case_status_from_binary(<<"running">>)),
    ?assertEqual(undefined, case_status_from_binary(<<"unknown">>)).

-endif.
