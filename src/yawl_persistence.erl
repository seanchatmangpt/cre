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
%% @doc YAWL Persistence Module
%%
%% This module provides database state management for YAWL workflow
%% execution using Mnesia. It handles persistent storage of workflow
%% cases and work items with disk-backed tables.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Mnesia table schema initialization with disc_copies</li>
%%   <li>Workflow case persistence (create, read, update, delete)</li>
%%   <li>Work item persistence linked to cases</li>
%%   <li>Active case listing for recovery</li>
%%   <li>Automatic cleanup of expired completed cases</li>
%%   <li>Case statistics and counting</li>
%% </ul>
%%
%% <h3>Mnesia Tables</h3>
%% <ul>
%%   <li><b>persistent_case:</b> Workflow case records</li>
%%   <li><b>persistent_workitem:</b> Work item records with case reference</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_persistence).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([init_schema/0,
         save_case/1,
         load_case/1,
         delete_case/1,
         save_workitem/1,
         load_workitems/1,
         list_active_cases/0,
         cleanup_expired_cases/0,
         get_case_count/0]).

%%====================================================================
%% Records
%%====================================================================

-record(persistent_case, {
    case_id :: binary(),
    workflow_id :: binary(),
    spec :: term(),
    status :: running | suspended | completed | cancelled | failed,
    data :: map(),
    created_at :: integer(),
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined
}).

-record(persistent_workitem, {
    workitem_id :: binary(),
    case_id :: binary(),
    task_id :: binary(),
    status :: enabled | started | completed | failed | cancelled,
    data :: map(),
    enabled_at :: integer() | undefined,
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined
}).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary().
-type workflow_id() :: binary().
-type workitem_id() :: binary().
-type task_id() :: binary().
-type case_status() :: running | suspended | completed | cancelled | failed.
-type workitem_status() :: enabled | started | completed | failed | cancelled.

-export_type([case_id/0, workflow_id/0, workitem_id/0, task_id/0,
              case_status/0, workitem_status/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the Mnesia schema with persistent tables.
%%
%% Creates the following tables with disc_copies on the current node:
%% - persistent_case: Stores workflow case records
%% - persistent_workitem: Stores work item records
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_schema() -> ok | {error, term()}.

init_schema() ->
    Node = node(),

    %% Create Mnesia schema if it doesn't exist
    _ = case mnesia:create_schema([Node]) of
        ok ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, create_schema},
                                     {node, Node}]);
        {error, {already_exists, Node}} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, schema_exists},
                                     {node, Node}]);
        {error, SchemaReason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, create_schema},
                                      {error, SchemaReason}])
    end,

    %% Start Mnesia
    _ = case mnesia:start() of
        ok ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, mnesia_started}]);
        {error, {already_started, _}} ->
            ok;
        {error, StartReason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, mnesia_start},
                                      {error, StartReason}])
    end,

    %% Create persistent_case table
    CaseAttrs = record_info(fields, persistent_case),
    CaseDef = [{attributes, CaseAttrs},
               {disc_copies, [Node]},
               {type, set}],
    CaseResult = case mnesia:create_table(persistent_case, CaseDef) of
        {atomic, ok} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, create_table},
                                     {table, persistent_case}]),
            ok;
        {aborted, {already_exists, persistent_case}} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, table_exists},
                                     {table, persistent_case}]),
            ok;
        {aborted, CaseReason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, create_table},
                                      {table, persistent_case},
                                      {error, CaseReason}]),
            {error, CaseReason}
    end,

    %% Create persistent_workitem table
    WorkitemAttrs = record_info(fields, persistent_workitem),
    WorkitemDef = [{attributes, WorkitemAttrs},
                   {disc_copies, [Node]},
                   {type, set},
                   {index, [case_id]}],
    WorkitemResult = case mnesia:create_table(persistent_workitem, WorkitemDef) of
        {atomic, ok} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, create_table},
                                     {table, persistent_workitem}]),
            ok;
        {aborted, {already_exists, persistent_workitem}} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, table_exists},
                                     {table, persistent_workitem}]),
            ok;
        {aborted, WorkitemReason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, create_table},
                                      {table, persistent_workitem},
                                      {error, WorkitemReason}]),
            {error, WorkitemReason}
    end,

    %% Wait for tables to be ready
    WaitResult = case mnesia:wait_for_tables([persistent_case, persistent_workitem], 5000) of
        ok ->
            ok;
        {timeout, Tables} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, wait_for_tables},
                                      {error, timeout},
                                      {tables, Tables}]),
            {error, {timeout, Tables}};
        {error, WaitReason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, wait_for_tables},
                                      {error, WaitReason}]),
            {error, WaitReason}
    end,

    %% Return combined result
    case {CaseResult, WorkitemResult, WaitResult} of
        {ok, ok, ok} -> ok;
        {{error, R}, _, _} -> {error, R};
        {_, {error, R}, _} -> {error, R};
        {_, _, {error, R}} -> {error, R}
    end.

%%--------------------------------------------------------------------
%% @doc Saves or updates a workflow case.
%%
%% The Case parameter can be a map or a record with case fields.
%% If a case with the same case_id exists, it will be updated.
%%
%% Returns {ok, case_id()} on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_case(Case :: map() | tuple()) ->
          {ok, case_id()} | {error, term()}.

save_case(CaseMap) when is_map(CaseMap) ->
    CaseId = maps:get(case_id, CaseMap),
    PersistentCase = #persistent_case{
        case_id = CaseId,
        workflow_id = maps:get(workflow_id, CaseMap, <<>>),
        spec = maps:get(spec, CaseMap, #{}),
        status = maps:get(status, CaseMap, running),
        data = maps:get(data, CaseMap, #{}),
        created_at = maps:get(created_at, CaseMap, erlang:system_time(millisecond)),
        started_at = maps:get(started_at, CaseMap, undefined),
        completed_at = maps:get(completed_at, CaseMap, undefined)
    },
    save_case_record(PersistentCase);

save_case(CaseRecord) when is_tuple(CaseRecord), tuple_size(CaseRecord) >= 1 ->
    try
        %% Extract fields from the record based on record_info
        case element(1, CaseRecord) of
            workflow_case ->
                %% Convert from yawl_engine workflow_case record
                CaseId = element(2, CaseRecord),
                PersistentCase = #persistent_case{
                    case_id = CaseId,
                    workflow_id = element(3, CaseRecord),
                    spec = element(4, CaseRecord),
                    status = element(5, CaseRecord),
                    data = element(7, CaseRecord),
                    created_at = element(8, CaseRecord),
                    started_at = element(9, CaseRecord),
                    completed_at = element(10, CaseRecord)
                },
                save_case_record(PersistentCase);
            persistent_case ->
                %% Already a persistent_case record
                save_case_record(CaseRecord);
            _ ->
                {error, {unknown_record_type, element(1, CaseRecord)}}
        end
    catch
        Kind:Reason:Stack ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, save_case},
                                      {error, Kind},
                                      {reason, Reason},
                                      {stacktrace, Stack}]),
            {error, {Kind, Reason}}
    end.

%% @private
save_case_record(Case) ->
    Transaction = fun() ->
        mnesia:write(Case)
    end,
    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, save_case},
                                     {case_id, Case#persistent_case.case_id}]),
            {ok, Case#persistent_case.case_id};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, save_case},
                                      {error, Reason}]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Loads a case by its case_id.
%%
%% Returns {ok, CaseMap} on success where CaseMap contains:
%% - case_id, workflow_id, spec, status, data
%% - created_at, started_at, completed_at
%%
%% Returns {error, not_found} if the case does not exist.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_case(case_id()) -> {ok, map()} | {error, not_found | term()}.

load_case(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        case mnesia:read(persistent_case, CaseId) of
            [#persistent_case{} = Case] ->
                {ok, case_to_map(Case)};
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Transaction) of
        {atomic, {ok, CaseMap}} ->
            {ok, CaseMap};
        {atomic, {error, not_found}} ->
            {error, not_found};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, load_case},
                                      {case_id, CaseId},
                                      {error, Reason}]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a case and all its associated work items.
%%
%% This is a cascading delete that removes all work items
%% associated with the case before removing the case itself.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_case(case_id()) -> ok | {error, term()}.

delete_case(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        %% Delete all work items for this case
        Workitems = mnesia:index_read(persistent_workitem, CaseId, #persistent_workitem.case_id),
        lists:foreach(fun(Workitem) ->
            mnesia:delete_object(Workitem)
        end, Workitems),

        %% Delete the case
        case mnesia:read(persistent_case, CaseId) of
            [_Case] ->
                mnesia:delete(persistent_case, CaseId, write),
                ok;
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, delete_case},
                                     {case_id, CaseId}]),
            ok;
        {atomic, {error, not_found}} ->
            {error, not_found};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, delete_case},
                                      {case_id, CaseId},
                                      {error, Reason}]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Saves or updates a work item.
%%
%% The Workitem parameter can be a map or a record with workitem fields.
%%
%% Returns {ok, workitem_id()} on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_workitem(Workitem :: map() | tuple()) ->
          {ok, workitem_id()} | {error, term()}.

save_workitem(WorkitemMap) when is_map(WorkitemMap) ->
    %% Support both 'id' and 'workitem_id' keys for flexibility
    WorkitemId = case maps:get(id, WorkitemMap, undefined) of
                     undefined -> maps:get(workitem_id, WorkitemMap);
                     Id -> Id
                 end,
    PersistentWorkitem = #persistent_workitem{
        workitem_id = WorkitemId,
        case_id = maps:get(case_id, WorkitemMap),
        task_id = maps:get(task_id, WorkitemMap, <<>>),
        status = maps:get(status, WorkitemMap, enabled),
        data = maps:get(data, WorkitemMap, #{}),
        enabled_at = maps:get(enabled_at, WorkitemMap, undefined),
        started_at = maps:get(started_at, WorkitemMap, undefined),
        completed_at = maps:get(completed_at, WorkitemMap, undefined)
    },
    save_workitem_record(PersistentWorkitem);

save_workitem(WorkitemRecord) when is_tuple(WorkitemRecord), tuple_size(WorkitemRecord) >= 1 ->
    try
        case element(1, WorkitemRecord) of
            workitem ->
                %% Convert from yawl_engine workitem record
                WorkitemId = element(2, WorkitemRecord),
                PersistentWorkitem = #persistent_workitem{
                    workitem_id = WorkitemId,
                    case_id = element(3, WorkitemRecord),
                    task_id = element(4, WorkitemRecord),
                    status = element(5, WorkitemRecord),
                    data = element(6, WorkitemRecord),
                    enabled_at = element(7, WorkitemRecord),
                    started_at = element(8, WorkitemRecord),
                    completed_at = element(9, WorkitemRecord)
                },
                save_workitem_record(PersistentWorkitem);
            persistent_workitem ->
                %% Already a persistent_workitem record
                save_workitem_record(WorkitemRecord);
            _ ->
                {error, {unknown_record_type, element(1, WorkitemRecord)}}
        end
    catch
        Kind:Reason:Stack ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, save_workitem},
                                      {error, Kind},
                                      {reason, Reason},
                                      {stacktrace, Stack}]),
            {error, {Kind, Reason}}
    end.

%% @private
save_workitem_record(Workitem) ->
    Transaction = fun() ->
        mnesia:write(Workitem)
    end,
    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            error_logger:info_report([{module, ?MODULE},
                                     {action, save_workitem},
                                     {workitem_id, Workitem#persistent_workitem.workitem_id}]),
            {ok, Workitem#persistent_workitem.workitem_id};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, save_workitem},
                                      {error, Reason}]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Loads all work items for a given case_id.
%%
%% Returns {ok, [WorkitemMap]} where each WorkitemMap contains:
%% - workitem_id, case_id, task_id, status, data
%% - enabled_at, started_at, completed_at
%%
%% Returns {ok, []} if no work items exist for the case.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_workitems(case_id()) -> {ok, [map()]} | {error, term()}.

load_workitems(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        mnesia:index_read(persistent_workitem, CaseId, #persistent_workitem.case_id)
    end,
    case mnesia:transaction(Transaction) of
        {atomic, Workitems} ->
            Maps = [workitem_to_map(W) || W <- Workitems],
            {ok, Maps};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, load_workitems},
                                      {case_id, CaseId},
                                      {error, Reason}]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all active cases (running or suspended).
%%
%% Returns a list of case maps with status running or suspended.
%% Useful for recovery after system restart.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_active_cases() -> {ok, [map()]}.

list_active_cases() ->
    Transaction = fun() ->
        %% Select all cases with running or suspended status
        Running = mnesia:match_object(#persistent_case{status = running, _ = '_'}),
        Suspended = mnesia:match_object(#persistent_case{status = suspended, _ = '_'}),
        {Running, Suspended}
    end,
    case mnesia:transaction(Transaction) of
        {atomic, {Running, Suspended}} ->
            ActiveCases = lists:map(fun case_to_map/1, Running ++ Suspended),
            {ok, ActiveCases};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, list_active_cases},
                                      {error, Reason}]),
            {ok, []}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes completed cases older than 24 hours.
%%
%% Also deletes all associated work items for each case.
%% Returns {ok, Count} with the number of cases deleted.
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_expired_cases() -> {ok, non_neg_integer()} | {error, term()}.

cleanup_expired_cases() ->
    ExpirationTime = erlang:system_time(millisecond) - (24 * 60 * 60 * 1000),  %% 24 hours ago

    Transaction = fun() ->
        %% Find all completed cases
        AllCompleted = mnesia:match_object(#persistent_case{status = completed, _ = '_'}),

        %% Filter cases older than expiration time
        ExpiredCases = lists:filter(fun(#persistent_case{completed_at = CompletedAt}) ->
            CompletedAt =/= undefined andalso CompletedAt < ExpirationTime
        end, AllCompleted),

        %% Delete each expired case and its work items
        DeletedCount = lists:foldl(fun(#persistent_case{case_id = CaseId}, Count) ->
            %% Delete associated work items
            Workitems = mnesia:index_read(persistent_workitem, CaseId, #persistent_workitem.case_id),
            lists:foreach(fun(W) -> mnesia:delete_object(W) end, Workitems),

            %% Delete the case
            mnesia:delete(persistent_case, CaseId, write),
            Count + 1
        end, 0, ExpiredCases),

        {ok, DeletedCount, ExpiredCases}
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {ok, Count, ExpiredCases}} ->
            CaseIds = [C#persistent_case.case_id || C <- ExpiredCases],
            error_logger:info_report([{module, ?MODULE},
                                     {action, cleanup_expired_cases},
                                     {count, Count},
                                     {case_ids, CaseIds}]),
            {ok, Count};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, cleanup_expired_cases},
                                      {error, Reason}]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Returns the total count of cases in the database.
%%
%% Returns {ok, Count} where Count is the total number of cases
%% across all statuses.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_count() -> {ok, non_neg_integer()} | {error, term()}.

get_case_count() ->
    Transaction = fun() ->
        mnesia:table_info(persistent_case, size)
    end,
    case mnesia:transaction(Transaction) of
        {atomic, Count} when is_integer(Count) ->
            {ok, Count};
        {aborted, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                      {action, get_case_count},
                                      {error, Reason}]),
            {error, Reason}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Converts a persistent_case record to a map.
-spec case_to_map(#persistent_case{}) -> map().

case_to_map(#persistent_case{} = Case) ->
    #{
        case_id => Case#persistent_case.case_id,
        workflow_id => Case#persistent_case.workflow_id,
        spec => Case#persistent_case.spec,
        status => Case#persistent_case.status,
        data => Case#persistent_case.data,
        created_at => Case#persistent_case.created_at,
        started_at => Case#persistent_case.started_at,
        completed_at => Case#persistent_case.completed_at
    }.

%% @private
%% @doc Converts a persistent_workitem record to a map.
-spec workitem_to_map(#persistent_workitem{}) -> map().

workitem_to_map(#persistent_workitem{} = Workitem) ->
    #{
        workitem_id => Workitem#persistent_workitem.workitem_id,
        case_id => Workitem#persistent_workitem.case_id,
        task_id => Workitem#persistent_workitem.task_id,
        status => Workitem#persistent_workitem.status,
        data => Workitem#persistent_workitem.data,
        enabled_at => Workitem#persistent_workitem.enabled_at,
        started_at => Workitem#persistent_workitem.started_at,
        completed_at => Workitem#persistent_workitem.completed_at
    }.
