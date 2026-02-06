%% -*- erlang -*-
%% Persistence wrapper functions for cre_yawl
%% This file contains the patch to be added to cre_yawl.erl

%% Add this line after line 89 (after execute_capability_allocate/3 export):
%% -export([save_state/1, load_state/1, delete_state/1, list_states/0]).

%% Add these functions at the end of the file (after line 1740):

%%====================================================================
%% Persistence API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Saves a workflow state to persistent storage.
%%
%% Converts the workflow record to a case map and delegates to
%% yawl_persistence:save_case/1. Returns {ok, CaseId} on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_state(Workflow :: #workflow{}) -> {ok, binary()} | {error, term()}.
save_state(#workflow{id = WorkflowId} = Workflow) ->
    case ensure_mnesia_running() of
        ok ->
            try
                CaseMap = workflow_to_case_map(Workflow),
                yawl_persistence:save_case(CaseMap)
            catch
                Kind:Reason:Stack ->
                    logger:error("Failed to save workflow state: ~p:~p~n~p", [Kind, Reason, Stack]),
                    {error, {Kind, Reason}}
            end;
        {error, Reason} ->
            {error, {mnesia_not_available, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Loads a workflow state from persistent storage.
%%
%% Delegates to yawl_persistence:load_case/1 and converts the
%% case map back to a workflow record.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_state(CaseId :: binary()) -> {ok, #workflow{}} | {error, not_found | term()}.
load_state(CaseId) when is_binary(CaseId) ->
    case ensure_mnesia_running() of
        ok ->
            case yawl_persistence:load_case(CaseId) of
                {ok, CaseMap} ->
                    try case_map_to_workflow(CaseMap)
                    catch
                        Kind:Reason:Stack ->
                            logger:error("Failed to restore workflow from case ~p: ~p:~p~n~p",
                                        [CaseId, Kind, Reason, Stack]),
                            {error, {invalid_case_format, {Kind, Reason}}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {mnesia_not_available, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a saved workflow state.
%%
%% Delegates to yawl_persistence:delete_case/1. Returns ok on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_state(CaseId :: binary()) -> ok | {error, term()}.
delete_state(CaseId) when is_binary(CaseId) ->
    case ensure_mnesia_running() of
        ok ->
            yawl_persistence:delete_case(CaseId);
        {error, Reason} ->
            {error, {mnesia_not_available, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all active workflow states.
%%
%% Delegates to yawl_persistence:list_active_cases/0.
%% Returns a list of case IDs for running/suspended workflows.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_states() -> {ok, [binary()]} | {error, term()}.
list_states() ->
    case ensure_mnesia_running() of
        ok ->
            case yawl_persistence:list_active_cases() of
                {ok, Cases} ->
                    CaseIds = [maps:get(case_id, Case) || Case <- Cases],
                    {ok, CaseIds};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {mnesia_not_available, Reason}}
    end.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures Mnesia is running. Attempts to start it if not.
%%
%% Returns ok if Mnesia is available, {error, Reason} otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_mnesia_running() -> ok | {error, term()}.
ensure_mnesia_running() ->
    case whereis(mnesia) of
        undefined ->
            %% Mnesia not started, try to initialize
            case yawl_persistence:init_schema() of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _Pid ->
            %% Mnesia is running
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a workflow record to a case map for persistence.
%%
%% The case map contains:
%% - case_id: The workflow ID (unique identifier)
%% - workflow_id: The workflow ID (same as case_id for top-level)
%% - spec: The workflow specification (tasks, conditions, connections)
%% - status: running (default for saved workflows)
%% - data: Workflow metadata
%% - Timestamps for tracking
%%
%% @end
%%--------------------------------------------------------------------
-spec workflow_to_case_map(#workflow{}) -> map().
workflow_to_case_map(#workflow{
        id = Id,
        name = Name,
        tasks = Tasks,
        conditions = Conditions,
        connections = Connections,
        start_task_id = StartTaskId,
        end_task_ids = EndTaskIds
    }) ->
    Timestamp = erlang:system_time(millisecond),
    #{
        case_id => Id,
        workflow_id => Id,
        spec => #{
            name => Name,
            tasks => encode_tasks(Tasks),
            conditions => encode_conditions(Conditions),
            connections => encode_connections(Connections),
            start_task_id => StartTaskId,
            end_task_ids => EndTaskIds
        },
        status => running,
        data => #{},
        created_at => Timestamp,
        started_at => Timestamp,
        completed_at => undefined
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a case map back to a workflow record.
%%
%% Reconstructs the workflow record from the persisted case map.
%% Returns {ok, #workflow{}} on success, throws on error.
%%
%% @end
%%--------------------------------------------------------------------
-spec case_map_to_workflow(map()) -> #workflow{}.
case_map_to_workflow(CaseMap) ->
    Spec = maps:get(spec, CaseMap, #{}),
    #workflow{
        id = maps:get(case_id, CaseMap),
        name = maps_get(name, Spec, <<"Restored Workflow">>),
        tasks = decode_tasks(maps_get(tasks, Spec, #{})),
        conditions = decode_conditions(maps_get(conditions, Spec, #{})),
        connections = decode_connections(maps_get(connections, Spec, [])),
        start_task_id = maps_get(start_task_id, Spec, undefined),
        end_task_ids = maps_get(end_task_ids, Spec, [])
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes task records to maps for persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_tasks(#{element_id() => #task{}}) -> #{element_id() => map()}.
encode_tasks(Tasks) ->
    maps:map(fun(_TaskId, #task{
                id = Id,
                name = Name,
                type = Type,
                split_type = SplitType,
                join_type = JoinType,
                metadata = Metadata
            }) ->
        #{
            id => Id,
            name => Name,
            type => Type,
            split_type => SplitType,
            join_type => JoinType,
            metadata => Metadata
        }
    end, Tasks).

%%--------------------------------------------------------------------
%% @private
%% @doc Decodes task maps back to task records.
%%
%% @end
%%--------------------------------------------------------------------
-spec decode_tasks(#{element_id() => map()}) -> #{element_id() => #task{}}.
decode_tasks(TaskMaps) ->
    maps:map(fun(_TaskId, TaskMap) ->
        #task{
            id = maps_get(id, TaskMap, <<>>),
            name = maps_get(name, TaskMap, <<"Unnamed Task">>),
            type = maps_get(type, TaskMap, atomic),
            split_type = maps_get(split_type, TaskMap, undefined),
            join_type = maps_get(join_type, TaskMap, undefined),
            metadata = maps_get(metadata, TaskMap, #{})
        }
    end, TaskMaps).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes condition records to maps for persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_conditions(#{element_id() => #yawl_condition{}}) -> #{element_id() => map()}.
encode_conditions(Conditions) ->
    maps:map(fun(_CondId, #yawl_condition{
                id = Id,
                expression = Expression,
                description = Description
            }) ->
        #{
            id => Id,
            expression => encode_expression(Expression),
            description => Description
        }
    end, Conditions).

%%--------------------------------------------------------------------
%% @private
%% @doc Decodes condition maps back to condition records.
%%
%% @end
%%--------------------------------------------------------------------
-spec decode_conditions(#{element_id() => map()}) -> #{element_id() => #yawl_condition{}}.
decode_conditions(ConditionMaps) ->
    maps:map(fun(_CondId, CondMap) ->
        #yawl_condition{
            id = maps_get(id, CondMap, <<>>),
            expression = decode_expression(maps_get(expression, CondMap, true)),
            description = maps_get(description, CondMap, undefined)
        }
    end, ConditionMaps).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes connection records to maps for persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_connections([#connection{}]) -> [map()].
encode_connections(Connections) ->
    [begin
        #{
            from_id => Conn#connection.from_id,
            to_id => Conn#connection.to_id,
            condition_id => Conn#connection.condition_id
        }
    end || Conn <- Connections].

%%--------------------------------------------------------------------
%% @private
%% @doc Decodes connection maps back to connection records.
%%
%% @end
%%--------------------------------------------------------------------
-spec decode_connections([map()]) -> [#connection{}].
decode_connections(ConnMaps) when is_list(ConnMaps) ->
    [begin
        #connection{
            from_id = maps_get(from_id, ConnMap, <<>>),
            to_id = maps_get(to_id, ConnMap, <<>>),
            condition_id = maps_get(condition_id, ConnMap, undefined)
        }
    end || ConnMap <- ConnMaps];
decode_connections(_) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes condition expressions for persistence.
%%
%% Handles binary, tuple, and function expressions.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_expression(condition()) -> binary() | tuple().
encode_expression(Expr) when is_binary(Expr); is_tuple(Expr) ->
    Expr;
encode_expression(Expr) when is_function(Expr) ->
    %% Store functions as erlang term reference
    %% Note: Anonymous functions cannot be serialized across restarts
    {erlang_fun, erlang:pid_to_list(self()), erlang:system_time(millisecond)}.

%%--------------------------------------------------------------------
%% @private
%% @doc Decodes condition expressions from persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec decode_expression(binary() | tuple()) -> condition().
decode_expression(Expr) when is_binary(Expr); is_tuple(Expr) ->
    Expr;
decode_expression({erlang_fun, _Pid, _Time}) ->
    %% Functions cannot be restored - return a default true condition
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc Safe maps:get with default value.
%%
%% @end
%%--------------------------------------------------------------------
-spec maps_get(atom(), map(), term()) -> term().
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
