%% @doc YAWL Workflow Data Handling Module
%%
%% This module provides generic workflow variable management for ANY YAWL specification.
%% It handles data extraction, validation, merging, and retrieval operations with
%% schema validation support for data integrity.
%%
%% ## Data Flow
%%
%% 1. Extract variables from YAWL spec
%% 2. Validate against schema
%% 3. Merge with existing workflow data
%% 4. Store as tokens in workflow marking
%% 5. Retrieve/update during execution
%%
%% ## Usage Example
%%
%% ```
%% %% Define schema for order data
%% Schema = #{
%%     <<"order_id">> => {type, required, [{min_length, 10}]},
%%     <<"customer_id">> => {type, required, []},
%%     <<"amount">> => {type, required, [{min, 0}]}
%% },
%%
%% %% Extract from YAWL spec
%% {ok, DataMap} = yawl_data:extract_data(Spec, NetId, TaskId),
%%
%% %% Validate data
%% {ok, Validated} = yawl_data:validate_data(DataMap, Schema),
%%
%% %% Get/set variables
%% Value = yawl_data:get_variable(Validated, <<"amount">>),
%% Updated = yawl_data:set_variable(Validated, <<"discount">>, 0.0),
%%
%% %% Convert to tokens
%% Tokens = yawl_data:to_marking_data(Updated)
%% '''
%%
%% @since 1.0.0
-module(yawl_data).

-export([extract_data/3, validate_data/2, merge_data/2,
         get_variable/2, set_variable/3, to_marking_data/1,
         check_constraint/3, check_basic_type/2]).

-export_type([data/0, schema/0, type_def/0]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

%% @type data() :: #{binary() => term()}.
%% Map of variable names to their values.
-type data() :: #{binary() => term()}.

%% @type schema() :: #{binary() => type_def()}.
%% Schema defining data types and constraints for variables.
-type schema() :: #{binary() => type_def()}.

%% @type type_def() :: {type, required, constraints()}.
%% Type definition with requirement flag and constraints.
-type type_def() :: {type, required | optional, constraints()}.

%% @type constraints() :: [constraint()].
%% List of validation constraints.
-type constraints() :: [constraint()].

%% @type constraint() :: {min, number()} | {max, number()} |
%%                      {min_length, integer()} | {max_length, integer()} |
%%                      {allowed, [term()]} | {pattern, binary()}.
%% Validation constraints for data types.
-type constraint() :: {min, number()} | {max, number()} |
                     {min_length, integer()} | {max_length, integer()} |
                     {allowed, [term()]} | {pattern, binary()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Extracts data variable definitions from YAWL spec.
%%
%% @param Spec YAWL specification term
%% @param NetId Network identifier
%% @param TaskId Task identifier
%% @return {ok, DataMap} | {error, Reason}
-spec extract_data(term(), binary(), binary()) -> {ok, data()} | {error, term()}.
extract_data(Spec, NetId, TaskId) ->
    try
        %% Use wf_spec to get task parameters
        {ok, Params} = wf_spec:task_params(Spec, {NetId, TaskId}),

        %% Convert parameters to data map
        DataMap = lists:foldl(fun({Key, Value}, Acc) ->
            Acc#{Key => Value}
        end, #{}, Params),

        {ok, DataMap}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to extract data for task ~ts/~ts: ~p",
                      [NetId, TaskId, {Error, Reason}]),
            {error, {data_extraction_failed, Error, Reason}}
    end.

%% @doc Validates data against schema type definitions.
%%
%% @param Data Data map to validate
%% @param Schema Schema definition
%% @return {ok, ValidatedData} | {error, Reasons}
-spec validate_data(data(), schema()) -> {ok, data()} | {error, [term()]}.
validate_data(Data, Schema) ->
    try
        %% Collect validation errors
        Errors = lists:foldl(fun({VarName, TypeDef}, Acc) ->
            case maps:find(VarName, Data) of
                {ok, Value} ->
                    case validate_variable(VarName, Value, Schema) of
                        {error, ReasonList} when is_list(ReasonList) -> ReasonList ++ Acc;
                        {error, Reason} -> [Reason | Acc];
                        ok -> Acc
                    end;
                error ->
                    %% Required field missing
                    case TypeDef of
                        {type, required, _} ->
                            [{undefined_required_variable, VarName} | Acc];
                        {type, optional, _} ->
                            Acc
                    end
            end
        end, [], maps:to_list(Schema)),

        case Errors of
            [] -> {ok, Data};
            _ -> {error, lists:reverse(Errors)}
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Validation error: ~p", [{Error, Reason}]),
            {error, {validation_failed, Error, Reason, Data, Schema}}
    end.

%% @doc Merges new data into current workflow data.
%%
%% @param CurrentData Existing workflow data
%% @param NewData New data to merge
%% @return MergedData
-spec merge_data(data(), data()) -> data().
merge_data(CurrentData, NewData) ->
    %% Merge maps, with NewData values taking precedence
    maps:merge(CurrentData, NewData).

%% @doc Retrieves variable value from data map.
%%
%% @param Data Data map
%% @param VarName Variable name
%% @return {ok, Value} | {error, undefined}
-spec get_variable(data(), binary()) -> {ok, term()} | {error, undefined}.
get_variable(Data, VarName) ->
    case maps:find(VarName, Data) of
        {ok, Value} -> {ok, Value};
        error -> {error, undefined}
    end.

%% @doc Sets variable value in data map.
%%
%% @param Data Data map
%% @param VarName Variable name
%% @param Value Value to set
%% @return UpdatedData
-spec set_variable(data(), binary(), term()) -> data().
set_variable(Data, VarName, Value) ->
    Data#{VarName => Value}.

%% @doc Converts data map to tokens for marking.
%%
%% Each variable becomes a {data, VarName, Value} token
%% that can be injected into workflow places.
%%
%% @param Data Data map
%% @return MarkingTokens
-spec to_marking_data(data()) -> [{data, binary(), term()}].
to_marking_data(Data) ->
    maps:fold(fun(VarName, Value, Acc) ->
        [{data, VarName, Value} | Acc]
    end, [], Data).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validates a single variable against schema.
%% Returns ok or {error, [Reasons]} where Reasons is a list of error reasons.
-spec validate_variable(binary(), term(), schema()) -> ok | {error, [term()]}.
validate_variable(VarName, Value, Schema) ->
    case maps:find(VarName, Schema) of
        {ok, TypeDef} ->
            case TypeDef of
                {type, Required, Constraints} ->
                    case Required of
                        required when Value =:= undefined ->
                            {error, [{undefined_required_variable, VarName}]};
                        required ->
                            validate_value_type(VarName, Value, Constraints);
                        optional ->
                            %% Optional variables can be undefined
                            case Value =:= undefined of
                                true -> ok;
                                false -> validate_value_type(VarName, Value, Constraints)
                            end
                    end;
                _ ->
                    %% Unknown type format - allow as free-form data
                    ok
            end;
        error ->
            %% Variable not in schema - allow as free-form data
            ok
    end.

%% @doc Validates value type and constraints.
%% Returns {error, [Reasons]} with a list of reasons, or ok.
-spec validate_value_type(binary(), term(), constraints()) -> ok | {error, [term()]}.
validate_value_type(VarName, Value, Constraints) ->
    try
        %% Basic type validation
        case check_basic_type(Value, Constraints) of
            {error, Reason} -> {error, [Reason]};
            ok ->
                %% Check additional constraints
                check_constraints(VarName, Value, Constraints)
        end
    catch
        Error:Reason1 ->
            {error, [{validation_error, Error, Reason1, VarName, Value, Constraints}]}
    end.

%% @doc Checks basic type against constraints.
-spec check_basic_type(term(), constraints()) -> ok | {error, term()}.
check_basic_type(Value, Constraints) ->
    try
        %% Find type constraint - look for {type, Type} in the constraints list
        Type = case lists:foldl(fun(Constraint, Acc) ->
            case Constraint of
                {type, T} -> {found, T};
                _ -> Acc
            end
        end, not_found, Constraints) of
            {found, T} -> T;
            not_found -> undefined
        end,

        case Type of
            undefined ->
                %% No type constraint - allow any value
                ok;
            _ ->
                check_type_match(Value, Type)
        end
    catch
        Error:Reason ->
            {error, {check_basic_type_error, Error, Reason, Value, Constraints}}
    end.

%% @doc Checks if value matches expected type.
-spec check_type_match(term(), atom()) -> ok | {error, term()}.
check_type_match(Value, Type) ->
    case Type of
        binary when is_binary(Value) -> ok;
        binary when is_list(Value) -> ok;  % Allow list conversion
        integer when is_integer(Value) -> ok;
        float when is_float(Value) -> ok;
        float when is_integer(Value) -> ok;  % Allow integer to float
        boolean when is_boolean(Value) -> ok;
        map when is_map(Value) -> ok;
        list when is_list(Value) -> ok;
        _ -> {error, {type_mismatch, {expected, Type}, {actual, Value}}}
    end.

%% @doc Checks additional value constraints.
-spec check_constraints(binary(), term(), constraints()) -> ok | {error, term()}.
check_constraints(VarName, Value, Constraints) ->
    Errors = lists:foldl(fun(Constraint, Acc) ->
        case check_constraint(VarName, Value, Constraint) of
            ok -> Acc;
            {error, Reason} -> [Reason | Acc]
        end
    end, [], Constraints),
    case Errors of
        [] -> ok;
        _ -> {error, lists:reverse(Errors)}
    end.

%% @doc Checks a single constraint.
-spec check_constraint(binary(), term(), constraint()) -> ok | {error, term()}.
check_constraint(VarName, Value, {min, Min}) when is_number(Value) ->
    if Value >= Min -> ok;
       true -> {error, {below_min, VarName, Value, Min}}
    end;
check_constraint(VarName, Value, {max, Max}) when is_number(Value) ->
    if Value =< Max -> ok;
       true -> {error, {above_max, VarName, Value, Max}}
    end;
check_constraint(VarName, Value, {min_length, Min}) when is_binary(Value) ->
    if byte_size(Value) >= Min -> ok;
       true -> {error, {too_short, VarName, byte_size(Value), Min}}
    end;
check_constraint(VarName, Value, {min_length, Min}) when is_list(Value) ->
    if length(Value) >= Min -> ok;
       true -> {error, {too_short, VarName, length(Value), Min}}
    end;
check_constraint(VarName, Value, {max_length, Max}) when is_binary(Value) ->
    if byte_size(Value) =< Max -> ok;
       true -> {error, {too_long, VarName, byte_size(Value), Max}}
    end;
check_constraint(VarName, Value, {max_length, Max}) when is_list(Value) ->
    if length(Value) =< Max -> ok;
       true -> {error, {too_long, VarName, length(Value), Max}}
    end;
check_constraint(VarName, Value, {allowed, Allowed}) ->
    case lists:member(Value, Allowed) of
        true -> ok;
        false -> {error, {not_allowed, VarName, Value, Allowed}}
    end;
check_constraint(VarName, Value, {pattern, Pattern}) when is_binary(Value) ->
    try
        case re:run(Value, Pattern) of
            {match, _} -> ok;
            nomatch -> {error, {pattern_mismatch, VarName, Value, Pattern}}
        end
    catch
        _:_ -> {error, {invalid_pattern, VarName, Pattern}}
    end;
check_constraint(_, _, _) -> ok.