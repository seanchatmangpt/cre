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
%% @doc YAWL Workflow Persistence Wrapper Module
%%
%% This module provides persistence wrapper functions for YAWL workflows,
%% delegating to yawl_persistence for Mnesia-based storage. It enables
%% workflow state to survive system restarts and supports recovery of
%% long-running business processes.
%%
%% <h3>Overview</h3>
%%
%% The persistence layer provides:
%% - Atomic state snapshots for checkpoint/recovery
%% - Mnesia-based distributed storage
%% - Workflow specification preservation
%% - Active workflow tracking and listing
%%
%% <h3>Functions</h3>
%% <ul>
%%   <li><b>save_state/1</b> - Save workflow state to persistent storage</li>
%%   <li><b>load_state/1</b> - Load workflow state from persistent storage</li>
%%   <li><b>delete_state/1</b> - Delete a saved workflow state</li>
%%   <li><b>list_states/0</b> - List all active workflow states</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% ```erlang
%% %% Create a simple workflow for persistence testing
%% %% Note: These doctests assume Mnesia is initialized
%% 1> Workflow = cre_yawl:new_workflow(<<"test_wf">>),
%% 2> Task1 = cre_yawl:add_task(Workflow, <<"step1">>, [{type, atomic}]),
%% 3> {ok, <<"test_wf">>} = cre_yawl_persistence:save_state(Task1).
%% ok
%%
%% %% Load the workflow back
%% 4> {ok, LoadedWF} = cre_yawl_persistence:load_state(<<"test_wf">>).
%% {ok, {workflow, <<"test_wf">>, _, _, _, _, _, _}}
%%
%% %% List active states includes our workflow
%% 5> {ok, States} = cre_yawl_persistence:list_states(),
%% 6> true = lists:member(<<"test_wf">>, States).
%% true
%%
%% %% Clean up - delete the state
%% 7> ok = cre_yawl_persistence:delete_state(<<"test_wf">>).
%% ok
%%
%% %% Verify deletion
%% 8> {error, not_found} = cre_yawl_persistence:load_state(<<"test_wf">>).
%% {error, not_found}
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_yawl_persistence).

%%====================================================================
%% Exports
%%====================================================================

-export([save_state/1, load_state/1, delete_state/1, list_states/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Saves a workflow state to persistent storage.
%%
%% Converts the workflow record to a case map and delegates to
%% yawl_persistence:save_case/1. Returns {ok, CaseId} on success.
%%
%% The saved state includes:
%% - Workflow specification (tasks, conditions, connections)
%% - Workflow metadata (name, IDs)
%% - Timestamps for tracking
%%
%% ```erlang
%% 1> Workflow = cre_yawl:new_workflow(<<"my_wf">>),
%% 2> Workflow1 = cre_yawl:add_task(Workflow, <<"task1">>, [{type, atomic}]),
%% 3> {ok, <<"my_wf">>} = cre_yawl_persistence:save_state(Workflow1).
%% {ok, <<"my_wf">>}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Saves a workflow state to persistent storage.

Converts the workflow record to a case map and delegates to
yawl_persistence:save_case/1.

### Example

```erlang
Workflow = cre_yawl:new_workflow(<<"my_wf">>),
Workflow1 = cre_yawl:add_task(Workflow, <<"task1">>, [{type, atomic}]),
{ok, <<"my_wf">>} = cre_yawl_persistence:save_state(Workflow1).
```
""".
-spec save_state(Workflow :: cre_yawl:workflow()) -> {ok, binary()} | {error, term()}.
save_state(Workflow) when element(1, Workflow) =:= workflow ->
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
%% ```erlang
%% 1> {ok, Workflow} = cre_yawl_persistence:load_state(<<"my_wf">>).
%% {ok, {workflow, <<"my_wf">>, _, _, _, _, _, _}}
%%
%% %% Non-existent workflow returns error
%% 2> {error, not_found} = cre_yawl_persistence:load_state(<<"nonexistent">>).
%% {error, not_found}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Loads a workflow state from persistent storage.

Delegates to yawl_persistence:load_case/1 and converts the
case map back to a workflow record.

### Example

```erlang
{ok, Workflow} = cre_yawl_persistence:load_state(<<"my_wf">>).
```
""".
-spec load_state(CaseId :: binary()) -> {ok, cre_yawl:workflow()} | {error, not_found | term()}.
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
%% ```erlang
%% 1> ok = cre_yawl_persistence:delete_state(<<"my_wf">>).
%% ok
%%
%% %% Subsequent load fails
%% 2> {error, not_found} = cre_yawl_persistence:load_state(<<"my_wf">>).
%% {error, not_found}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Deletes a saved workflow state.

Delegates to yawl_persistence:delete_case/1.

### Example

```erlang
ok = cre_yawl_persistence:delete_state(<<"my_wf">>).
```
""".
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
%% ```erlang
%% 1> {ok, States} = cre_yawl_persistence:list_states().
%% {ok, [<<"wf1">>, <<"wf2">>, <<"wf3">>]}
%%
%% %% Empty when no active workflows
%% 2> {ok, []} = cre_yawl_persistence:list_states().
%% {ok, []}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Lists all active workflow states.

Returns a list of case IDs for running/suspended workflows.

### Example

```erlang
{ok, States} = cre_yawl_persistence:list_states().
```
""".
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
-spec workflow_to_case_map(cre_yawl:workflow()) -> map().
workflow_to_case_map(Workflow) ->
    Id = element(2, Workflow),  %% workflow id
    Name = element(3, Workflow),  %% workflow name
    Tasks = element(4, Workflow),  %% tasks map
    Conditions = element(5, Workflow),  %% conditions map
    Connections = element(6, Workflow),  %% connections list
    StartTaskId = element(7, Workflow),  %% start task id
    EndTaskIds = element(8, Workflow),  %% end task ids

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
-spec case_map_to_workflow(map()) -> cre_yawl:workflow().
case_map_to_workflow(CaseMap) ->
    Spec = maps:get(spec, CaseMap, #{}),
    Id = maps:get(case_id, CaseMap),
    Name = maps_get(name, Spec, <<"Restored Workflow">>),
    Tasks = decode_tasks(maps_get(tasks, Spec, #{})),
    Conditions = decode_conditions(maps_get(conditions, Spec, #{})),
    Connections = decode_connections(maps_get(connections, Spec, [])),
    StartTaskId = maps_get(start_task_id, Spec, undefined),
    EndTaskIds = maps_get(end_task_ids, Spec, []),

    %% Build workflow record using record definition from cre_yawl
    {workflow, Id, Name, Tasks, Conditions, Connections, StartTaskId, EndTaskIds}.

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes task records to maps for persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_tasks(map()) -> map().
encode_tasks(Tasks) when is_map(Tasks) ->
    maps:map(fun(_TaskId, Task) when element(1, Task) =:= task ->
        Id = element(2, Task),
        Name = element(3, Task),
        Type = element(4, Task),
        SplitType = element(5, Task),
        JoinType = element(6, Task),
        Metadata = element(7, Task),
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
-spec decode_tasks(map()) -> map().
decode_tasks(TaskMaps) when is_map(TaskMaps) ->
    maps:map(fun(_TaskId, TaskMap) ->
        Id = maps_get(id, TaskMap, <<>>),
        Name = maps_get(name, TaskMap, <<"Unnamed Task">>),
        Type = maps_get(type, TaskMap, atomic),
        SplitType = maps_get(split_type, TaskMap, undefined),
        JoinType = maps_get(join_type, TaskMap, undefined),
        Metadata = maps_get(metadata, TaskMap, #{}),
        {task, Id, Name, Type, SplitType, JoinType, Metadata}
    end, TaskMaps).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes condition records to maps for persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_conditions(map()) -> map().
encode_conditions(Conditions) when is_map(Conditions) ->
    maps:map(fun(_CondId, Condition) when element(1, Condition) =:= yawl_condition ->
        Id = element(2, Condition),
        Expression = element(3, Condition),
        Description = element(4, Condition),
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
-spec decode_conditions(map()) -> map().
decode_conditions(ConditionMaps) when is_map(ConditionMaps) ->
    maps:map(fun(_CondId, CondMap) ->
        Id = maps_get(id, CondMap, <<>>),
        Expression = decode_expression(maps_get(expression, CondMap, true)),
        Description = maps_get(description, CondMap, undefined),
        {yawl_condition, Id, Expression, Description}
    end, ConditionMaps).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes connection records to maps for persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_connections(list()) -> list().
encode_connections(Connections) when is_list(Connections) ->
    [begin
        FromId = element(2, Conn),
        ToId = element(3, Conn),
        ConditionId = element(4, Conn),
        #{
            from_id => FromId,
            to_id => ToId,
            condition_id => ConditionId
        }
    end || Conn <- Connections].

%%--------------------------------------------------------------------
%% @private
%% @doc Decodes connection maps back to connection records.
%%
%% @end
%%--------------------------------------------------------------------
-spec decode_connections(list()) -> list().
decode_connections(ConnMaps) when is_list(ConnMaps) ->
    [begin
        FromId = maps_get(from_id, ConnMap, <<>>),
        ToId = maps_get(to_id, ConnMap, <<>>),
        ConditionId = maps_get(condition_id, ConnMap, undefined),
        {connection, FromId, ToId, ConditionId}
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
-spec encode_expression(term()) -> term().
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
-spec decode_expression(term()) -> term().
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
