%% -*- erlang -*-
%%
%% @doc Query running workflows - list, state, and filter operations.
%%
%% This module provides query capabilities for active workflows managed by
%% wf_engine. It supports listing workflows, retrieving workflow state,
%% and filtering by status, task, or pattern.
%%
%% == Examples ==
%%
%% List all active workflows:
%% ```erlang
%% > {ok, Engine} = wf_engine:start_link(#{spec => Spec, seed => 1, now => 0}).
%% _
%% > {ok, CaseId} = wf_engine:start_case(Engine, #{data => #{}}, 0).
%% _
%% > Summaries = wf_query:list_workflows(Engine).
%% _
%% > lists:any(fun(S) -> S#wf_workflow_summary.case_id =:= CaseId end, Summaries).
%% true
%% ```
%%
%% Get workflow state:
%% ```erlang
%% > {ok, Info} = wf_query:get_workflow(Engine, CaseId).
%% _
%% > Info#wf_workflow_info.status.
%% running
%% ```
%%
%% Filter by status:
%% ```erlang
%% > Running = wf_query:filter_by_status(Engine, running).
%% _
%% > length(Running) > 0.
%% true
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_query).

%%====================================================================
%% Exports
%%====================================================================

%% Query API
-export([list_workflows/1]).
-export([get_workflow/2]).
-export([filter_by_status/2]).
-export([filter_by_task/2]).
-export([list_work_items/2]).
-export([count_by_status/1]).
-export([get_enabled_tasks/2]).

%% gen_server integration
-export([handle_request/2]).

%%====================================================================
%% Records and Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Brief summary of a workflow.
%%--------------------------------------------------------------------
-record(wf_workflow_summary, {
    case_id :: wf_engine:case_id(),
    status :: wf_engine:case_status(),
    work_items_count :: non_neg_integer(),
    created_at :: integer() | undefined
}).

-type workflow_summary() :: #wf_workflow_summary{}.

%%--------------------------------------------------------------------
%% @doc Detailed workflow information.
%%--------------------------------------------------------------------
-record(wf_workflow_info, {
    case_id :: wf_engine:case_id(),
    status :: wf_engine:case_status(),
    data :: map(),
    work_items :: [wf_engine:work_item()],
    enabled_tasks :: [atom()],
    timestamps :: map()
}).

-type workflow_info() :: #wf_workflow_info{}.

%%--------------------------------------------------------------------
%% @doc Workflow status filter.
%%--------------------------------------------------------------------
-type status_filter() :: wf_engine:case_status().

%%--------------------------------------------------------------------
%% @doc Task name filter.
%%--------------------------------------------------------------------
-type task_filter() :: atom().

%% Export types
-export_type([workflow_summary/0, workflow_info/0, status_filter/0, task_filter/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Lists all workflows with basic summary information.
%%
%% Returns a list of workflow summaries containing case ID, status,
%% and work item count for each active workflow.
%%
%% @param Engine Engine pid or name
%% @return [workflow_summary()] List of workflow summaries
%%
%% @end
%%--------------------------------------------------------------------
-spec list_workflows(Engine :: pid() | atom()) -> [workflow_summary()].

list_workflows(Engine) ->
    gen_server:call(Engine, {wf_query, list_workflows}).

%%--------------------------------------------------------------------
%% @doc Gets detailed information for a specific workflow.
%%
%% Returns full workflow state including data, work items, and
%% enabled tasks. Returns error if workflow not found.
%%
%% @param Engine Engine pid or name
%% @param CaseId Case identifier
%% @return {ok, workflow_info()} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workflow(Engine :: pid() | atom(), CaseId :: wf_engine:case_id()) ->
    {ok, workflow_info()} | {error, not_found}.

get_workflow(Engine, CaseId) ->
    gen_server:call(Engine, {wf_query, get_workflow, CaseId}).

%%--------------------------------------------------------------------
%% @doc Filters workflows by status.
%%
%% Returns summaries for all workflows matching the given status.
%% Status can be: pending, running, suspended, cancelled, completed, failed, scheduled.
%%
%% @param Engine Engine pid or name
%% @param Status Workflow status to filter by
%% @return [workflow_summary()]
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_by_status(Engine :: pid() | atom(), Status :: status_filter()) ->
    [workflow_summary()].

filter_by_status(Engine, Status) ->
    gen_server:call(Engine, {wf_query, filter_by_status, Status}).

%%--------------------------------------------------------------------
%% @doc Filters workflows by pending task.
%%
%% Returns summaries for all workflows that have a work item
%% offering the specified task.
%%
%% @param Engine Engine pid or name
%% @param Task Task name to filter by
%% @return [workflow_summary()]
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_by_task(Engine :: pid() | atom(), Task :: task_filter()) ->
    [workflow_summary()].

filter_by_task(Engine, Task) ->
    gen_server:call(Engine, {wf_query, filter_by_task, Task}).

%%--------------------------------------------------------------------
%% @doc Lists all work items for a workflow.
%%
%% Returns detailed work item records for the given case,
%% or error if case not found.
%%
%% @param Engine Engine pid or name
%% @param CaseId Case identifier
%% @return {ok, [wf_engine:work_item()]} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec list_work_items(Engine :: pid() | atom(), CaseId :: wf_engine:case_id()) ->
    {ok, [wf_engine:work_item()]} | {error, not_found}.

list_work_items(Engine, CaseId) ->
    gen_server:call(Engine, {wf_query, list_work_items, CaseId}).

%%--------------------------------------------------------------------
%% @doc Counts workflows by status.
%%
%% Returns a map with status as key and count as value for each status
%% that has at least one workflow.
%%
%% @param Engine Engine pid or name
%% @return #{status_filter() => non_neg_integer()}
%%
%% @end
%%--------------------------------------------------------------------
-spec count_by_status(Engine :: pid() | atom()) ->
    #{wf_engine:case_status() => non_neg_integer()}.

count_by_status(Engine) ->
    gen_server:call(Engine, {wf_query, count_by_status}).

%%--------------------------------------------------------------------
%% @doc Gets enabled (executable) tasks for a workflow.
%%
%% Returns list of tasks that can be executed next in the workflow.
%%
%% @param Engine Engine pid or name
%% @param CaseId Case identifier
%% @return {ok, [atom()]} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_enabled_tasks(Engine :: pid() | atom(), CaseId :: wf_engine:case_id()) ->
    {ok, [atom()]} | {error, not_found}.

get_enabled_tasks(Engine, CaseId) ->
    gen_server:call(Engine, {wf_query, get_enabled_tasks, CaseId}).

%%====================================================================
%% Internal Functions (for gen_server integration)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Handle query requests from gen_server.
%%
%% This function is called from wf_engine:handle_call/3 to process
%% wf_query requests. It should be invoked like:
%%
%% handle_call({wf_query, ...} = Request, From, State) ->
%%     Response = wf_query:handle_request(Request, State),
%%     {reply, Response, State}
%%
%% @param Request Query request tuple
%% @param State Engine state from wf_engine
%% @return Query result
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_request(Request :: term(), State :: term()) -> term().

handle_request({wf_query, list_workflows}, State) ->
    handle_list_workflows(State);

handle_request({wf_query, get_workflow, CaseId}, State) ->
    handle_get_workflow(State, CaseId);

handle_request({wf_query, filter_by_status, Status}, State) ->
    handle_filter_by_status(State, Status);

handle_request({wf_query, filter_by_task, Task}, State) ->
    handle_filter_by_task(State, Task);

handle_request({wf_query, list_work_items, CaseId}, State) ->
    handle_list_work_items(State, CaseId);

handle_request({wf_query, count_by_status}, State) ->
    handle_count_by_status(State);

handle_request({wf_query, get_enabled_tasks, CaseId}, State) ->
    handle_get_enabled_tasks(State, CaseId);

handle_request(_Request, _State) ->
    {error, unknown_query_request}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles list_workflows query.
%%--------------------------------------------------------------------
-spec handle_list_workflows(State :: term()) -> [workflow_summary()].

handle_list_workflows(State) ->
    Cases = maps:get(cases, State, #{}),
    lists:map(
        fun({_CaseId, Case}) ->
            case_to_summary(Case)
        end,
        maps:to_list(Cases)
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Handles get_workflow query.
%%--------------------------------------------------------------------
-spec handle_get_workflow(State :: term(), CaseId :: term()) ->
    {ok, workflow_info()} | {error, not_found}.

handle_get_workflow(State, CaseId) ->
    Cases = maps:get(cases, State, #{}),
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            {error, not_found};
        Case ->
            {ok, case_to_info(Case)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles filter_by_status query.
%%--------------------------------------------------------------------
-spec handle_filter_by_status(State :: term(), Status :: term()) ->
    [workflow_summary()].

handle_filter_by_status(State, Status) ->
    Cases = maps:get(cases, State, #{}),
    lists:filtermap(
        fun({_CaseId, Case}) ->
            CaseStatus = maps:get(status, Case, running),
            case CaseStatus =:= Status of
                true ->
                    {true, case_to_summary(Case)};
                false ->
                    false
            end
        end,
        maps:to_list(Cases)
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Handles filter_by_task query.
%%--------------------------------------------------------------------
-spec handle_filter_by_task(State :: term(), Task :: term()) ->
    [workflow_summary()].

handle_filter_by_task(State, Task) ->
    Cases = maps:get(cases, State, #{}),
    lists:filtermap(
        fun({_CaseId, Case}) ->
            WorkItems = maps:get(work_items, Case, #{}),
            HasTask = lists:any(
                fun(WI) ->
                    WITask = maps:get(task, WI, undefined),
                    WIStatus = maps:get(status, WI, undefined),
                    WITask =:= Task andalso WIStatus =:= offered
                end,
                maps:values(WorkItems)
            ),
            case HasTask of
                true -> {true, case_to_summary(Case)};
                false -> false
            end
        end,
        maps:to_list(Cases)
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Handles list_work_items query.
%%--------------------------------------------------------------------
-spec handle_list_work_items(State :: term(), CaseId :: term()) ->
    {ok, [term()]} | {error, not_found}.

handle_list_work_items(State, CaseId) ->
    Cases = maps:get(cases, State, #{}),
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            {error, not_found};
        Case ->
            WorkItems = maps:get(work_items, Case, #{}),
            {ok, maps:values(WorkItems)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles count_by_status query.
%%--------------------------------------------------------------------
-spec handle_count_by_status(State :: term()) ->
    #{term() => non_neg_integer()}.

handle_count_by_status(State) ->
    Cases = maps:get(cases, State, #{}),
    lists:foldl(
        fun({_CaseId, Case}, Acc) ->
            Status = maps:get(status, Case, running),
            Count = maps:get(Status, Acc, 0),
            maps:put(Status, Count + 1, Acc)
        end,
        #{},
        maps:to_list(Cases)
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Handles get_enabled_tasks query.
%%--------------------------------------------------------------------
-spec handle_get_enabled_tasks(State :: term(), CaseId :: term()) ->
    {ok, [term()]} | {error, not_found}.

handle_get_enabled_tasks(State, CaseId) ->
    Cases = maps:get(cases, State, #{}),
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            {error, not_found};
        Case ->
            WorkItems = maps:get(work_items, Case, #{}),
            EnabledTasks = lists:filtermap(
                fun(WI) ->
                    WIStatus = maps:get(status, WI, undefined),
                    case WIStatus of
                        offered ->
                            Task = maps:get(task, WI, undefined),
                            {true, Task};
                        _ ->
                            false
                    end
                end,
                maps:values(WorkItems)
            ),
            {ok, lists:usort(EnabledTasks)}
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a case record to a workflow summary.
%%--------------------------------------------------------------------
-spec case_to_summary(Case :: term()) -> workflow_summary().

case_to_summary(Case) ->
    CaseId = maps:get(case_id, Case, undefined),
    Status = maps:get(status, Case, running),
    WorkItems = maps:get(work_items, Case, #{}),
    WorkItemsCount = maps:size(WorkItems),
    Timestamps = maps:get(timestamps, Case, #{}),
    CreatedAt = maps:get(created_at, Timestamps, undefined),
    #wf_workflow_summary{
        case_id = CaseId,
        status = Status,
        work_items_count = WorkItemsCount,
        created_at = CreatedAt
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a case record to detailed workflow info.
%%--------------------------------------------------------------------
-spec case_to_info(Case :: term()) -> workflow_info().

case_to_info(Case) ->
    CaseId = maps:get(case_id, Case, undefined),
    Status = maps:get(status, Case, running),
    Data = maps:get(data, Case, #{}),
    WorkItems = maps:get(work_items, Case, #{}),
    Timestamps = maps:get(timestamps, Case, #{}),

    %% Extract enabled (offered) tasks
    EnabledTasks = lists:filtermap(
        fun(WI) ->
            WIStatus = maps:get(status, WI, undefined),
            case WIStatus of
                offered ->
                    Task = maps:get(task, WI, undefined),
                    {true, Task};
                _ ->
                    false
            end
        end,
        maps:values(WorkItems)
    ),

    #wf_workflow_info{
        case_id = CaseId,
        status = Status,
        data = Data,
        work_items = maps:values(WorkItems),
        enabled_tasks = lists:usort(EnabledTasks),
        timestamps = Timestamps
    }.
