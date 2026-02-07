%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc Comprehensive validation module for YAWL patterns, workflows,
%% and data structures. Provides input validation, bounds checking,
%% and detailed error reporting.
%%
%% <h3>Validation Categories</h3>
%% <ul>
%%   <li><code>pattern_validation</code> - Validate workflow patterns</li>
%%   <li><code>data_validation</code> - Validate data structures</li>
%%   <li><code>bounds_validation</code> - Bounds and limit checking</li>
%%   <li><code>type_validation</code> - Type checking</li>
%%   <li><code>reference_validation</code> - Reference and ID validation</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_validation).

%%====================================================================
%% Exports
%%====================================================================

%% Pattern validation
-export([validate_task_id/1,
         validate_task_list/1,
         validate_workflow_id/1,
         validate_pattern_structure/1]).

%% Data validation
-export([validate_binary_field/2,
         validate_atom_field/2,
         validate_list_field/2,
         validate_map_field/2,
         validate_function_field/2]).

%% Bounds validation
-export([validate_max_length/3,
         validate_min_length/3,
         validate_range/3,
         validate_positive_integer/1,
         validate_non_negative_integer/1]).

%% Reference validation
-export([validate_task_reference/2,
         validate_activity_reference/2,
         validate_condition_reference/2]).

%% Comprehensive validation
-export([validate_with_context/3,
         validate_all/2,
         collect_validation_errors/2]).

%%====================================================================
%% Types
%%====================================================================

-type validation_error() :: {error, Reason :: term()}.
-type validation_result() :: ok | validation_error().
-type validation_context() :: map().

-export_type([validation_error/0, validation_result/0, validation_context/0]).

%%====================================================================
%% Pattern Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a task ID.
%%
%% A valid task ID must be:
%% - A binary
%% - Non-empty
%% - Less than 256 characters
%% @end
%%--------------------------------------------------------------------
-spec validate_task_id(TaskId :: term()) -> validation_result().

validate_task_id(TaskId) when is_binary(TaskId) ->
    case byte_size(TaskId) of
        0 -> {error, {invalid_task_id, empty_id}};
        Size when Size > 256 -> {error, {invalid_task_id, id_too_long}};
        _ -> ok
    end;
validate_task_id(TaskId) ->
    {error, {invalid_task_id, {not_binary, TaskId}}}.

%%--------------------------------------------------------------------
%% @doc Validates a list of task IDs.
%% @end
%%--------------------------------------------------------------------
-spec validate_task_list(TaskIds :: term()) -> validation_result().

validate_task_list(TaskIds) when is_list(TaskIds) ->
    case length(TaskIds) of
        0 -> {error, {invalid_task_list, empty_list}};
        _ ->
            validate_task_list_items(TaskIds)
    end;
validate_task_list(TaskIds) ->
    {error, {invalid_task_list, {not_a_list, TaskIds}}}.

-spec validate_task_list_items(TaskIds :: list()) -> validation_result().

validate_task_list_items([]) -> ok;
validate_task_list_items([TaskId | Rest]) ->
    case validate_task_id(TaskId) of
        ok -> validate_task_list_items(Rest);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Validates a workflow ID.
%%
%% A valid workflow ID must be:
%% - A binary
%% - Non-empty
%% - Less than 512 characters
%% @end
%%--------------------------------------------------------------------
-spec validate_workflow_id(WorkflowId :: term()) -> validation_result().

validate_workflow_id(WorkflowId) when is_binary(WorkflowId) ->
    case byte_size(WorkflowId) of
        0 -> {error, {invalid_workflow_id, empty_id}};
        Size when Size > 512 -> {error, {invalid_workflow_id, id_too_long}};
        _ -> ok
    end;
validate_workflow_id(WorkflowId) ->
    {error, {invalid_workflow_id, {not_binary, WorkflowId}}}.

%%--------------------------------------------------------------------
%% @doc Validates the structure of a pattern.
%%
%% Ensures that a pattern has the required fields and proper types.
%% @end
%%--------------------------------------------------------------------
-spec validate_pattern_structure(Pattern :: term()) -> validation_result().

validate_pattern_structure(Pattern) when is_record(Pattern, task) ->
    case check_task_structure(Pattern) of
        [] -> ok;
        Errors -> {error, {invalid_pattern_structure, Errors}}
    end;
validate_pattern_structure(Pattern) when is_record(Pattern, workflow) ->
    case check_workflow_structure(Pattern) of
        [] -> ok;
        Errors -> {error, {invalid_pattern_structure, Errors}}
    end;
validate_pattern_structure(_) ->
    {error, {invalid_pattern_structure, unrecognized_pattern_type}}.

-spec check_task_structure(Task :: term()) -> list().

check_task_structure(Task) ->
    Errors = [],
    TaskId = element(2, Task),
    case is_binary(TaskId) of
        false -> [task_id_not_binary | Errors];
        true -> Errors
    end.

-spec check_workflow_structure(Workflow :: term()) -> list().

check_workflow_structure(Workflow) ->
    Errors = [],
    WorkflowId = element(2, Workflow),
    case is_binary(WorkflowId) of
        false -> [workflow_id_not_binary | Errors];
        true -> Errors
    end.

%%====================================================================
%% Data Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates that a field contains a binary.
%% @end
%%--------------------------------------------------------------------
-spec validate_binary_field(FieldValue :: term(), FieldName :: atom()) -> validation_result().

validate_binary_field(Value, FieldName) when is_binary(Value) ->
    ok;
validate_binary_field(Value, FieldName) ->
    {error, {invalid_field, FieldName, {not_binary, Value}}}.

%%--------------------------------------------------------------------
%% @doc Validates that a field contains an atom.
%% @end
%%--------------------------------------------------------------------
-spec validate_atom_field(FieldValue :: term(), FieldName :: atom()) -> validation_result().

validate_atom_field(Value, FieldName) when is_atom(Value) ->
    ok;
validate_atom_field(Value, FieldName) ->
    {error, {invalid_field, FieldName, {not_atom, Value}}}.

%%--------------------------------------------------------------------
%% @doc Validates that a field contains a list.
%% @end
%%--------------------------------------------------------------------
-spec validate_list_field(FieldValue :: term(), FieldName :: atom()) -> validation_result().

validate_list_field(Value, FieldName) when is_list(Value) ->
    ok;
validate_list_field(Value, FieldName) ->
    {error, {invalid_field, FieldName, {not_list, Value}}}.

%%--------------------------------------------------------------------
%% @doc Validates that a field contains a map.
%% @end
%%--------------------------------------------------------------------
-spec validate_map_field(FieldValue :: term(), FieldName :: atom()) -> validation_result().

validate_map_field(Value, FieldName) when is_map(Value) ->
    ok;
validate_map_field(Value, FieldName) ->
    {error, {invalid_field, FieldName, {not_map, Value}}}.

%%--------------------------------------------------------------------
%% @doc Validates that a field contains a function.
%% @end
%%--------------------------------------------------------------------
-spec validate_function_field(FieldValue :: term(), FieldName :: atom()) -> validation_result().

validate_function_field(Value, FieldName) when is_function(Value) ->
    ok;
validate_function_field(Value, FieldName) ->
    {error, {invalid_field, FieldName, {not_function, Value}}}.

%%====================================================================
%% Bounds Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates that a value doesn't exceed maximum length.
%%
%% Works with binaries, lists, and strings.
%% @end
%%--------------------------------------------------------------------
-spec validate_max_length(Value :: term(), MaxLength :: non_neg_integer(),
                          FieldName :: atom()) -> validation_result().

validate_max_length(Value, MaxLength, FieldName) when is_binary(Value) ->
    case byte_size(Value) of
        Size when Size > MaxLength ->
            {error, {exceeds_max_length, FieldName, Size, MaxLength}};
        _ -> ok
    end;
validate_max_length(Value, MaxLength, FieldName) when is_list(Value) ->
    case length(Value) of
        Size when Size > MaxLength ->
            {error, {exceeds_max_length, FieldName, Size, MaxLength}};
        _ -> ok
    end;
validate_max_length(Value, _MaxLength, FieldName) ->
    {error, {invalid_value_type, FieldName, Value}}.

%%--------------------------------------------------------------------
%% @doc Validates that a value meets minimum length requirement.
%% @end
%%--------------------------------------------------------------------
-spec validate_min_length(Value :: term(), MinLength :: non_neg_integer(),
                          FieldName :: atom()) -> validation_result().

validate_min_length(Value, MinLength, FieldName) when is_binary(Value) ->
    case byte_size(Value) of
        Size when Size < MinLength ->
            {error, {below_min_length, FieldName, Size, MinLength}};
        _ -> ok
    end;
validate_min_length(Value, MinLength, FieldName) when is_list(Value) ->
    case length(Value) of
        Size when Size < MinLength ->
            {error, {below_min_length, FieldName, Size, MinLength}};
        _ -> ok
    end;
validate_min_length(Value, _MinLength, FieldName) ->
    {error, {invalid_value_type, FieldName, Value}}.

%%--------------------------------------------------------------------
%% @doc Validates that a numeric value is within a range.
%% @end
%%--------------------------------------------------------------------
-spec validate_range(Value :: number(), {Min :: number(), Max :: number()},
                     FieldName :: atom()) -> validation_result().

validate_range(Value, {Min, Max}, FieldName) when is_number(Value) ->
    case Value >= Min andalso Value =< Max of
        true -> ok;
        false -> {error, {out_of_range, FieldName, Value, Min, Max}}
    end;
validate_range(Value, _Range, FieldName) ->
    {error, {not_a_number, FieldName, Value}}.

%%--------------------------------------------------------------------
%% @doc Validates that a value is a positive integer.
%% @end
%%--------------------------------------------------------------------
-spec validate_positive_integer(Value :: term()) -> validation_result().

validate_positive_integer(Value) when is_integer(Value), Value > 0 ->
    ok;
validate_positive_integer(Value) ->
    {error, {not_positive_integer, Value}}.

%%--------------------------------------------------------------------
%% @doc Validates that a value is a non-negative integer.
%% @end
%%--------------------------------------------------------------------
-spec validate_non_negative_integer(Value :: term()) -> validation_result().

validate_non_negative_integer(Value) when is_integer(Value), Value >= 0 ->
    ok;
validate_non_negative_integer(Value) ->
    {error, {not_non_negative_integer, Value}}.

%%====================================================================
%% Reference Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates that a task reference exists in a workflow.
%%
%% Context should contain 'workflow' key with the workflow record.
%% @end
%%--------------------------------------------------------------------
-spec validate_task_reference(TaskId :: binary(), Context :: validation_context()) -> validation_result().

validate_task_reference(TaskId, Context) when is_binary(TaskId) ->
    case maps:get(workflow, Context, undefined) of
        undefined ->
            {error, {missing_workflow_context, TaskId}};
        Workflow when is_record(Workflow, workflow) ->
            Tasks = element(4, Workflow),
            case maps:is_key(TaskId, Tasks) of
                true -> ok;
                false -> {error, {task_not_found, TaskId}}
            end;
        _ ->
            {error, {invalid_workflow_context, TaskId}}
    end;
validate_task_reference(TaskId, _Context) ->
    {error, {invalid_task_id, TaskId}}.

%%--------------------------------------------------------------------
%% @doc Validates that an activity reference exists.
%%
%% Context should contain 'activities' key with list of activity IDs.
%% @end
%%--------------------------------------------------------------------
-spec validate_activity_reference(ActivityId :: binary(), Context :: validation_context()) -> validation_result().

validate_activity_reference(ActivityId, Context) when is_binary(ActivityId) ->
    case maps:get(activities, Context, undefined) of
        undefined ->
            {error, {missing_activities_context, ActivityId}};
        Activities when is_list(Activities) ->
            case lists:member(ActivityId, Activities) of
                true -> ok;
                false -> {error, {activity_not_found, ActivityId}}
            end;
        _ ->
            {error, {invalid_activities_context, ActivityId}}
    end;
validate_activity_reference(ActivityId, _Context) ->
    {error, {invalid_activity_id, ActivityId}}.

%%--------------------------------------------------------------------
%% @doc Validates that a condition reference exists.
%%
%% Context should contain 'conditions' key with the conditions map.
%% @end
%%--------------------------------------------------------------------
-spec validate_condition_reference(ConditionId :: binary(), Context :: validation_context()) -> validation_result().

validate_condition_reference(ConditionId, Context) when is_binary(ConditionId) ->
    case maps:get(conditions, Context, undefined) of
        undefined ->
            {error, {missing_conditions_context, ConditionId}};
        Conditions when is_map(Conditions) ->
            case maps:is_key(ConditionId, Conditions) of
                true -> ok;
                false -> {error, {condition_not_found, ConditionId}}
            end;
        _ ->
            {error, {invalid_conditions_context, ConditionId}}
    end;
validate_condition_reference(ConditionId, _Context) ->
    {error, {invalid_condition_id, ConditionId}}.

%%====================================================================
%% Comprehensive Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a value using a custom validation function with context.
%%
%% ValidateFun should accept (Value, Context) and return ok or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec validate_with_context(Value :: term(), ValidateFun :: function(),
                            Context :: validation_context()) -> validation_result().

validate_with_context(Value, ValidateFun, Context) when is_function(ValidateFun) ->
    try
        ValidateFun(Value, Context)
    catch
        _:Error ->
            {error, {validation_exception, Error}}
    end;
validate_with_context(_Value, _ValidateFun, _Context) ->
    {error, validation_function_not_callable}.

%%--------------------------------------------------------------------
%% @doc Validates multiple values against a list of validators.
%%
%% Returns ok if all validations pass, collects all errors otherwise.
%% Validators should be {Value, ValidateFun} tuples or {Value, ValidateFun, Context}.
%% @end
%%--------------------------------------------------------------------
-spec validate_all(Validators :: list(), Context :: validation_context()) ->
    ok | {error, [validation_error()]}.

validate_all(Validators, Context) when is_list(Validators) ->
    Errors = collect_validation_errors(Validators, Context),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end;
validate_all(_Validators, _Context) ->
    {error, invalid_validators}.

%%--------------------------------------------------------------------
%% @doc Collects validation errors from multiple validators.
%%
%% Returns a list of validation errors that occurred.
%% @end
%%--------------------------------------------------------------------
-spec collect_validation_errors(Validators :: list(),
                                Context :: validation_context()) -> [validation_error()].

collect_validation_errors(Validators, Context) ->
    collect_validation_errors(Validators, Context, []).

-spec collect_validation_errors(Validators :: list(),
                                Context :: validation_context(),
                                Accumulator :: list()) -> [validation_error()].

collect_validation_errors([], _Context, Accumulator) ->
    lists:reverse(Accumulator);
collect_validation_errors([{Value, ValidateFun} | Rest], Context, Accumulator) ->
    case validate_with_context(Value, ValidateFun, Context) of
        ok ->
            collect_validation_errors(Rest, Context, Accumulator);
        Error ->
            collect_validation_errors(Rest, Context, [Error | Accumulator])
    end;
collect_validation_errors([{Value, ValidateFun, LocalContext} | Rest], Context, Accumulator) ->
    MergedContext = maps:merge(Context, LocalContext),
    case validate_with_context(Value, ValidateFun, MergedContext) of
        ok ->
            collect_validation_errors(Rest, Context, Accumulator);
        Error ->
            collect_validation_errors(Rest, Context, [Error | Accumulator])
    end;
collect_validation_errors([Invalid | Rest], Context, Accumulator) ->
    Error = {error, {invalid_validator_format, Invalid}},
    collect_validation_errors(Rest, Context, [Error | Accumulator]).
