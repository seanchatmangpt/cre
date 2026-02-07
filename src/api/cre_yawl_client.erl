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
%% @author YAWL Client Module
%%
%% -------------------------------------------------------------------

-module(cre_yawl_client).

-moduledoc """
YAWL Client Implementation for CRE.

This module implements the `cre_client` behavior for executing YAWL workflows
within the CRE distributed runtime environment. It bridges workflow patterns
defined in `cre_yawl` with the execution capabilities of the CRE system.

<h3>Architecture</h3>
The client maintains workflow state and orchestrates task execution through:
<ul>
  <li><b>Workflow State:</b> Tracks active tasks, completed work, and pending operations</li>
  <li><b>Task Queue:</b> Manages tasks ready for CRE execution</li>
  <li><b>Result Accumulation:</b> Collects results from completed worker executions</li>
  <li><b>Pattern Composition:</b> Supports combining multiple patterns</li>
</ul>

<h3>Examples</h3>

Pattern composition:

```erlang
> cre_yawl_client:compose_patterns([pattern1, pattern2], #{mode => sequence}).
{sequence,[pattern1,pattern2]}

> cre_yawl_client:compose_patterns([pattern1, pattern2], #{mode => parallel}).
{parallel_split,[pattern1,pattern2]}
```
""".

%%====================================================================
%% Includes
%%====================================================================

-include("cre.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% cre_client callback exports
-export([init/1, is_value/2, step/2, recv/3, load/2, unload/2]).

%% Client API exports
-export([start_link/2, start_link/3, execute_workflow/2, execute_pattern/3,
         compose_patterns/2, get_workflow_state/1, get_workflow_results/1,
         terminate_workflow/1]).

%% gen_server exports (when using cre_client gen_server wrapper)
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

%%====================================================================
%% Records
%%====================================================================

%% Import records from cre_yawl for local use
-define(workflow, cre_yawl:workflow).
-define(task, cre_yawl:task).
-define(sequence, cre_yawl:sequence).
-define(parallel_split, cre_yawl:parallel_split).
-define(synchronization, cre_yawl:synchronization).
-define(exclusive_choice, cre_yawl:exclusive_choice).
-define(simple_merge, cre_yawl:simple_merge).
-define(multi_choice, cre_yawl:multi_choice).
-define(synchronizing_merge, cre_yawl:synchronizing_merge).
-define(multi_merge, cre_yawl:multi_merge).
-define(discriminator, cre_yawl:discriminator).
-define(arbitration, cre_yawl:arbitration).

-record(yawl_client_state, {
          workflow :: undefined | term(),
          pattern :: undefined | term(),
          execution_state :: running | completed | failed | terminated,
          active_tasks = [] :: list(),
          completed_tasks = [] :: list(),
          pending_tasks = [] :: list(),
          results = #{} :: map(),
          errors = [] :: list(),
          start_time :: undefined | erlang:timestamp(),
          end_time :: undefined | erlang:timestamp(),
          metadata = #{} :: map()
         }).

%%====================================================================
%% Type Definitions
%%====================================================================

-type workflow_expr() :: term().
-type task_spec() :: {binary(), module(), atom(), list()}.
-type execution_state() :: running | completed | failed | terminated.
-type workflow_result() :: #{atom() => term()}.
-type client_state() :: #yawl_client_state{}.
-type usr_info() :: client_state().

-export_type([client_state/0, execution_state/0, workflow_result/0]).

%%====================================================================
%% cre_client Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initialize the YAWL client with a workflow or pattern.
%%
%% This callback is invoked by cre_client when the client starts.
%% It validates the workflow and initializes the execution state.
%%
%% @param WorkflowExpr A workflow record or pattern to execute.
%% @return Initial user info state for the client.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(WorkflowExpr :: workflow_expr()) -> usr_info().

init(WorkflowExpr) when element(1, WorkflowExpr) =:= workflow ->
    case cre_yawl:validate(WorkflowExpr) of
        ok ->
            WorkflowId = element(2, WorkflowExpr),
            #yawl_client_state{
              workflow = WorkflowExpr,
              pattern = undefined,
              execution_state = running,
              start_time = erlang:timestamp(),
              metadata = #{workflow_id => WorkflowId}
             };
        {error, Errors} ->
            error({workflow_validation_failed, Errors})
    end;

init(Pattern) when is_tuple(Pattern), tuple_size(Pattern) >= 1 ->
    PatternType = element(1, Pattern),
    case is_pattern_record(PatternType) of
        true ->
            #yawl_client_state{
              workflow = undefined,
              pattern = Pattern,
              execution_state = running,
              start_time = erlang:timestamp(),
              metadata = #{pattern_type => PatternType}
             };
        false ->
            error({invalid_workflow_expression, Pattern})
    end;

init(WorkflowExpr) ->
    error({invalid_workflow_expression, WorkflowExpr}).

%%--------------------------------------------------------------------
%% @doc Check if the workflow has reached a terminal state.
%%
%% A workflow is considered complete when:
%% <ul>
%%   <li>All tasks have been executed</li>
%%   <li>No pending tasks remain</li>
%%   <li>No active tasks are running</li>
%% </ul>
%%
%% @param _E The expression (unused for YAWL client).
%% @param UsrInfo The client state.
%% @return true if workflow is complete, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_value(_E :: term(), UsrInfo :: usr_info()) -> boolean().

is_value(_E, #yawl_client_state{execution_state = State,
                                 active_tasks = Active,
                                 pending_tasks = Pending}) ->
    case State of
        completed -> true;
        failed -> true;
        terminated -> true;
        running when Active =:= [], Pending =:= [] -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Execute one step of the workflow.
%%
%% This function determines the next tasks to execute and returns them
%% as CRE tasks to be sent to workers.
%%
%% @param _E The expression (unused for YAWL client).
%% @param UsrInfo The client state.
%% @return {ok, NewUsrInfo, TaskList} where TaskList contains tasks for CRE.
%%
%% @end
%%--------------------------------------------------------------------
-spec step(_E :: term(), UsrInfo :: usr_info()) ->
          {ok, usr_info(), [term()]}.

step(_E, #yawl_client_state{execution_state = terminated} = UsrInfo) ->
    {ok, UsrInfo, []};

step(_E, #yawl_client_state{workflow = undefined, pattern = Pattern} = UsrInfo) ->
    %% Execute pattern-based workflow
    PatternType = get_pattern_type(Pattern),
    execute_pattern_by_type(PatternType, UsrInfo);

step(_E, #yawl_client_state{workflow = Workflow} = UsrInfo) ->
    %% Execute workflow-based execution
    TaskMap = element(6, Workflow),  % tasks field
    Conns = element(5, Workflow),    % connections field

    %% Find ready tasks (tasks with no pending prerequisites)
    ReadyTasks = find_ready_tasks(Workflow, UsrInfo),

    %% Convert ready tasks to CRE task format
    CreTasks = tasks_to_cre(ReadyTasks, TaskMap, Conns),

    %% Update state with active tasks
    NewUsrInfo = UsrInfo#yawl_client_state{
                    active_tasks = UsrInfo#yawl_client_state.active_tasks ++ ReadyTasks,
                    pending_tasks = UsrInfo#yawl_client_state.pending_tasks -- ReadyTasks
                   },

    {ok, NewUsrInfo, CreTasks}.

%%--------------------------------------------------------------------
%% @doc Receive and process results from worker executions.
%%
%% This callback handles replies from CRE workers, updating the workflow
%% state with results and potentially making new tasks ready.
%%
%% @param _E The expression (unused).
%% @param ReplyLst List of {A, Delta} result tuples from workers.
%% @param UsrInfo The client state.
%% @return Updated client state with results incorporated.
%%
%% @end
%%--------------------------------------------------------------------
-spec recv(_E :: term(), ReplyLst :: [{term(), term()}], UsrInfo :: usr_info()) ->
          usr_info().

recv(_E, [], UsrInfo) ->
    UsrInfo;

recv(_E, [{TaskId, Result} | Rest],
       #yawl_client_state{results = Results,
                          completed_tasks = Completed,
                          active_tasks = Active,
                          workflow = Workflow} = UsrInfo) ->
    %% Remove completed task from active
    NewActive = lists:filter(fun(T) -> element(1, T) =/= TaskId end, Active),

    %% Store result
    NewResults = Results#{TaskId => Result},

    %% Check if workflow is complete
    NewState = case is_workflow_complete(Workflow, NewActive,
                                         UsrInfo#yawl_client_state.pending_tasks) of
                   true -> completed;
                   false -> UsrInfo#yawl_client_state.execution_state
               end,

    NewUsrInfo = UsrInfo#yawl_client_state{
                   active_tasks = NewActive,
                   completed_tasks = [TaskId | Completed],
                   results = NewResults,
                   execution_state = NewState
                  },

    recv(_E, Rest, NewUsrInfo).

%%--------------------------------------------------------------------
%% @doc Load a workflow expression into the client state.
%%
%% @param E The workflow expression to load.
%% @param UsrInfo The current client state.
%% @return Updated client state with the workflow loaded.
%%
%% @end
%%--------------------------------------------------------------------
-spec load(E :: workflow_expr(), UsrInfo :: usr_info()) -> usr_info().

load(WorkflowExpr, _UsrInfo) when element(1, WorkflowExpr) =:= workflow ->
    init(WorkflowExpr);

load(Pattern, _UsrInfo) when is_tuple(Pattern), tuple_size(Pattern) >= 1 ->
    PatternType = element(1, Pattern),
    case is_pattern_record(PatternType) of
        true -> init(Pattern);
        false -> error({invalid_workflow_expression, Pattern})
    end;

load(ComposedPatterns, _UsrInfo) when is_list(ComposedPatterns) ->
    %% Handle composed patterns
    #yawl_client_state{
      workflow = undefined,
      pattern = {sequence, ComposedPatterns},
      execution_state = running,
      start_time = erlang:timestamp(),
      metadata = #{composed => true}
     }.

%%--------------------------------------------------------------------
%% @doc Unload and finalize workflow execution.
%%
%% Extracts final results and cleans up the workflow state.
%%
%% @param _E The expression (unused).
%% @param UsrInfo The client state.
%% @return Map containing workflow results and metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec unload(_E :: term(), UsrInfo :: usr_info()) -> workflow_result().

unload(_E, #yawl_client_state{results = Results,
                              execution_state = State,
                              errors = Errors,
                              start_time = StartTime,
                              end_time = EndTime}) ->
    FinalEndTime = case EndTime of
                       undefined -> erlang:timestamp();
                       _ -> EndTime
                   end,

    Duration = case StartTime of
                   undefined -> 0;
                   _ -> timer:now_diff(FinalEndTime, StartTime) / 1000
               end,

    Results#{
      status => State,
      errors => Errors,
      duration_ms => Duration,
      completed_at => calendar:now_to_datetime(FinalEndTime)
     }.

%%====================================================================
%% Client API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a YAWL client linked to the calling process.
%%
%% @param CreName Name or PID of the CRE master process.
%% @param WorkflowExpr The workflow or pattern to execute.
%% @return {ok, ClientPid} or {error, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(CreName :: atom() | pid(), WorkflowExpr :: workflow_expr()) ->
          {ok, pid()} | {error, term()}.

start_link(CreName, WorkflowExpr) ->
    cre_client:start_link(CreName, ?MODULE, WorkflowExpr).

%%--------------------------------------------------------------------
%% @doc Start a named YAWL client linked to the calling process.
%%
%% @param ClientName Name to register the client process.
%% @param CreName Name or PID of the CRE master process.
%% @param WorkflowExpr The workflow or pattern to execute.
%% @return {ok, ClientPid} or {error, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(ClientName :: atom(), CreName :: atom() | pid(),
                 WorkflowExpr :: workflow_expr()) ->
          {ok, pid()} | {error, term()}.

start_link(ClientName, CreName, WorkflowExpr) ->
    cre_client:start_link(ClientName, CreName, ?MODULE, WorkflowExpr).

%%--------------------------------------------------------------------
%% @doc Execute a complete workflow synchronously.
%%
%% This function evaluates the workflow and waits for completion,
%% returning the final results.
%%
%% @param ClientPid The client process PID.
%% @param InitialData Initial data to pass to the workflow.
%% @return {ok, Results} or {error, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_workflow(ClientPid :: pid(), InitialData :: map()) ->
          {ok, workflow_result()} | {error, term()}.

execute_workflow(ClientPid, InitialData) ->
    try
        %% Create a workflow expression from initial data
        WorkflowExpr = {workflow, InitialData},
        Result = cre_client:eval(ClientPid, WorkflowExpr),
        {ok, Result}
    catch
        exit:{normal, _} -> {ok, #{}};
        exit:Reason -> {error, Reason};
        throw:Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Execute a single YAWL pattern.
%%
%% @param ClientPid The client process PID.
%% @param Pattern The pattern to execute.
%% @param InputData Input data for the pattern.
%% @return {ok, Results} or {error, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_pattern(ClientPid :: pid(),
                     Pattern :: term(),
                     InputData :: term()) ->
          {ok, workflow_result()} | {error, term()}.

execute_pattern(ClientPid, Pattern, InputData) ->
    try
        %% Initialize pattern state with input data
        PatternState = cre_yawl_patterns:implicit_termination(Pattern),
        Result = cre_client:eval(ClientPid, {pattern, PatternState, InputData}),
        {ok, Result}
    catch
        exit:{normal, _} -> {ok, #{}};
        exit:Reason -> {error, Reason};
        throw:Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Compose multiple patterns into a single workflow.
%%
%% This function combines patterns sequentially, where the output of
%% one pattern becomes the input of the next.
%%
%% @param Patterns List of patterns to compose.
%% @param Options Composition options (map with keys like mode, error_handling).
%% @return Composed workflow expression.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_patterns(Patterns :: list(), Options :: map()) ->
          workflow_expr().

compose_patterns(Patterns, Options) when is_list(Patterns) ->
    Mode = maps:get(mode, Options, sequence),
    ErrorHandling = maps:get(error_handling, Options, continue),

    case Mode of
        sequence ->
            compose_sequence(Patterns, ErrorHandling);
        parallel ->
            compose_parallel(Patterns, ErrorHandling);
        conditional ->
            compose_conditional(Patterns, Options);
        _ ->
            error({invalid_composition_mode, Mode})
    end.

%%--------------------------------------------------------------------
%% @doc Get the current workflow state.
%%
%% @param ClientPid The client process PID.
%% @return {ok, StateMap} or {error, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_state(ClientPid :: pid()) ->
          {ok, map()} | {error, term()}.

get_workflow_state(ClientPid) ->
    try
        State = sys:get_state(ClientPid),
        ClientState = extract_client_state(State),
        {ok, #{
          execution_state => ClientState#yawl_client_state.execution_state,
          active_tasks => length(ClientState#yawl_client_state.active_tasks),
          completed_tasks => length(ClientState#yawl_client_state.completed_tasks),
          pending_tasks => length(ClientState#yawl_client_state.pending_tasks),
          metadata => ClientState#yawl_client_state.metadata
         }}
    catch
        _:_ -> {error, client_not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Get workflow execution results.
%%
%% @param ClientPid The client process PID.
%% @return {ok, Results} or {error, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_results(ClientPid :: pid()) ->
          {ok, workflow_result()} | {error, term()}.

get_workflow_results(ClientPid) ->
    try
        State = sys:get_state(ClientPid),
        ClientState = extract_client_state(State),
        {ok, ClientState#yawl_client_state.results}
    catch
        _:_ -> {error, client_not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Terminate a running workflow.
%%
%% @param ClientPid The client process PID.
%% @return ok.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate_workflow(ClientPid :: pid()) -> ok.

terminate_workflow(ClientPid) ->
    cre_client:stop(ClientPid),
    ok.

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%% @private
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Check if a tuple is a valid pattern record type.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_pattern_record(atom()) -> boolean().

is_pattern_record(sequence) -> true;
is_pattern_record(parallel_split) -> true;
is_pattern_record(synchronization) -> true;
is_pattern_record(exclusive_choice) -> true;
is_pattern_record(simple_merge) -> true;
is_pattern_record(multi_choice) -> true;
is_pattern_record(synchronizing_merge) -> true;
is_pattern_record(multi_merge) -> true;
is_pattern_record(discriminator) -> true;
is_pattern_record(arbitration) -> true;
is_pattern_record(param_pass) -> true;
is_pattern_record(data_transform) -> true;
is_pattern_record(data_distribute) -> true;
is_pattern_record(data_accumulate) -> true;
is_pattern_record(data_visibility) -> true;
is_pattern_record(resource_create) -> true;
is_pattern_record(role_allocate) -> true;
is_pattern_record(resource_start) -> true;
is_pattern_record(role_distribute) -> true;
is_pattern_record(capability_allocate) -> true;
is_pattern_record(_) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Get the pattern type from a pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pattern_type(term()) -> atom().

get_pattern_type(Pattern) when is_tuple(Pattern), tuple_size(Pattern) >= 1 ->
    element(1, Pattern);
get_pattern_type(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a pattern based on its type.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_pattern_by_type(atom(), usr_info()) ->
          {ok, usr_info(), [term()]}.

execute_pattern_by_type(sequence, UsrInfo) ->
    execute_sequence(UsrInfo);
execute_pattern_by_type(parallel_split, UsrInfo) ->
    execute_parallel_split(UsrInfo);
execute_pattern_by_type(synchronization, UsrInfo) ->
    execute_synchronization(UsrInfo);
execute_pattern_by_type(exclusive_choice, UsrInfo) ->
    execute_exclusive_choice(UsrInfo);
execute_pattern_by_type(simple_merge, UsrInfo) ->
    execute_simple_merge(UsrInfo);
execute_pattern_by_type(multi_choice, UsrInfo) ->
    execute_multi_choice(UsrInfo);
execute_pattern_by_type(synchronizing_merge, UsrInfo) ->
    execute_synchronizing_merge(UsrInfo);
execute_pattern_by_type(multi_merge, UsrInfo) ->
    execute_multi_merge(UsrInfo);
execute_pattern_by_type(discriminator, UsrInfo) ->
    execute_discriminator(UsrInfo);
execute_pattern_by_type(arbitration, UsrInfo) ->
    execute_arbitration(UsrInfo);
execute_pattern_by_type(_, UsrInfo) ->
    {ok, UsrInfo, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a sequence pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_sequence(usr_info()) -> {ok, usr_info(), [term()]}.

execute_sequence(#yawl_client_state{pattern = Pattern} = UsrInfo) ->
    TaskIds = get_pattern_field(task_ids, Pattern, []),
    case TaskIds of
        [] ->
            {ok, UsrInfo, []};
        [Next | Rest] ->
            CreTask = {sequence_task, Next, self()},
            NewPattern = set_pattern_field(task_ids, Rest, Pattern),
            NewUsrInfo = UsrInfo#yawl_client_state{pattern = NewPattern},
            {ok, NewUsrInfo, [CreTask]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a parallel split pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_parallel_split(usr_info()) -> {ok, usr_info(), [term()]}.

execute_parallel_split(#yawl_client_state{pattern = Pattern} = UsrInfo) ->
    Branches = get_pattern_field(branch_task_ids, Pattern, []),
    CreTasks = [{parallel_task, BranchId, self()} || BranchId <- Branches],
    {ok, UsrInfo, CreTasks}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a synchronization pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_synchronization(usr_info()) -> {ok, usr_info(), [term()]}.

execute_synchronization(UsrInfo) ->
    {ok, UsrInfo, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute an exclusive choice pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_exclusive_choice(usr_info()) -> {ok, usr_info(), [term()]}.

execute_exclusive_choice(#yawl_client_state{pattern = Pattern} = UsrInfo) ->
    Branches = get_pattern_field(branches, Pattern, []),
    SelectedBranch = select_branch(Branches),
    case SelectedBranch of
        {TaskId, _Condition} ->
            CreTask = {choice_task, TaskId, self()},
            {ok, UsrInfo, [CreTask]};
        none ->
            {ok, UsrInfo, []}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a simple merge pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_simple_merge(usr_info()) -> {ok, usr_info(), [term()]}.

execute_simple_merge(UsrInfo) ->
    {ok, UsrInfo, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a multi-choice pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multi_choice(usr_info()) -> {ok, usr_info(), [term()]}.

execute_multi_choice(#yawl_client_state{pattern = Pattern} = UsrInfo) ->
    Branches = get_pattern_field(branches, Pattern, []),
    SelectedBranches = select_branches(Branches, []),
    CreTasks = [{multi_choice_task, TaskId, self()} || {TaskId, _Cond} <- SelectedBranches],
    {ok, UsrInfo, CreTasks}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a synchronizing merge pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_synchronizing_merge(usr_info()) -> {ok, usr_info(), [term()]}.

execute_synchronizing_merge(UsrInfo) ->
    {ok, UsrInfo, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a multi-merge pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multi_merge(usr_info()) -> {ok, usr_info(), [term()]}.

execute_multi_merge(UsrInfo) ->
    {ok, UsrInfo, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a discriminator pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_discriminator(usr_info()) -> {ok, usr_info(), [term()]}.

execute_discriminator(UsrInfo) ->
    {ok, UsrInfo, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute an arbitration pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_arbitration(usr_info()) -> {ok, usr_info(), [term()]}.

execute_arbitration(UsrInfo) ->
    {ok, UsrInfo, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Find tasks ready for execution based on workflow state.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_ready_tasks(term(), usr_info()) -> [task_spec()].

find_ready_tasks(Workflow, #yawl_client_state{completed_tasks = Completed,
                                               pending_tasks = Pending}) ->
    StartTaskId = get_workflow_field(start_task_id, Workflow, undefined),
    Tasks = get_workflow_field(tasks, Workflow, #{}),

    case {Completed, Pending} of
        {[], []} when StartTaskId =/= undefined ->
            %% Initial state - return start task
            Task = maps:get(StartTaskId, Tasks, {StartTaskId, undefined}),
            [{StartTaskId, Task}];
        _ ->
            %% Find tasks whose prerequisites are complete
            find_dependent_tasks(Tasks, Completed, Pending)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Find tasks whose dependencies are satisfied.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_dependent_tasks(map(), list(), list()) -> [task_spec()].

find_dependent_tasks(Tasks, Completed, Pending) ->
    %% Filter pending tasks to find those with all prerequisites complete
    lists:filter(
        fun({TaskId, _Task}) ->
            Prereqs = get_task_prerequisites(TaskId, Tasks),
            lists:all(fun(P) -> lists:member(P, Completed) end, Prereqs)
        end,
        Pending).

%%--------------------------------------------------------------------
%% @private
%% @doc Get prerequisite tasks for a given task.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_task_prerequisites(binary(), map()) -> list().

get_task_prerequisites(_TaskId, _Tasks) ->
    %% In a full implementation, this would traverse connections
    %% to find incoming tasks
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Convert workflow tasks to CRE task format.
%%
%% @end
%%--------------------------------------------------------------------
-spec tasks_to_cre(list(), map(), list()) -> list().

tasks_to_cre(TaskSpecs, _TaskMap, _Conns) ->
    [{yawl_task, TaskId, self()} || {TaskId, _Task} <- TaskSpecs].

%%--------------------------------------------------------------------
%% @private
%% @doc Check if workflow execution is complete.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_workflow_complete(term() | undefined, list(), list()) -> boolean().

is_workflow_complete(undefined, _Active, Pending) ->
    Pending =:= [];
is_workflow_complete(_Workflow, Active, Pending) ->
    Active =:= [] andalso Pending =:= [].

%%--------------------------------------------------------------------
%% @private
%% @doc Compose patterns sequentially.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_sequence(list(), atom()) -> workflow_expr().

compose_sequence(Patterns, _ErrorHandling) ->
    {sequence, Patterns}.

%%--------------------------------------------------------------------
%% @private
%% @doc Compose patterns in parallel.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_parallel(list(), atom()) -> workflow_expr().

compose_parallel(Patterns, _ErrorHandling) ->
    {parallel_split, Patterns}.

%%--------------------------------------------------------------------
%% @private
%% @doc Compose patterns conditionally.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_conditional(list(), map()) -> workflow_expr().

compose_conditional(Patterns, _Options) ->
    Branches = [{Pattern, true} || Pattern <- Patterns],
    {exclusive_choice, Branches}.

%%--------------------------------------------------------------------
%% @private
%% @doc Select the first true branch from a list.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_branch(list()) -> {term(), term()} | none.

select_branch([]) -> none;
select_branch([{TaskId, Condition} | Rest]) ->
    case evaluate_condition(Condition) of
        true -> {TaskId, Condition};
        false -> select_branch(Rest)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Select all branches with true conditions.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_branches(list(), list()) -> list().

select_branches([], Acc) ->
    lists:reverse(Acc);
select_branches([{TaskId, Condition} | Rest], Acc) ->
    case evaluate_condition(Condition) of
        true -> select_branches(Rest, [{TaskId, Condition} | Acc]);
        false -> select_branches(Rest, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Evaluate a condition expression.
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate_condition(term()) -> boolean().

evaluate_condition(true) -> true;
evaluate_condition(false) -> false;
evaluate_condition(Cond) when is_function(Cond, 0) ->
    Cond();
evaluate_condition(_Cond) ->
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc Extract client state from gen_server state.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_client_state(term()) -> client_state().

extract_client_state(State) when element(1, State) =:= client_state ->
    %% The cre_client gen_server wraps our state
    element(4, State);
extract_client_state(State) when element(1, State) =:= yawl_client_state ->
    State;
extract_client_state(State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc Get a field from a workflow record by position.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_field(atom(), term(), term()) -> term().

get_workflow_field(tasks, Workflow, Default) ->
    case tuple_size(Workflow) of
        7 -> element(6, Workflow);  % tasks is 6th field (1-indexed)
        _ -> Default
    end;
get_workflow_field(start_task_id, Workflow, Default) ->
    case tuple_size(Workflow) of
        7 -> element(7, Workflow);  % start_task_id is 7th field
        _ -> Default
    end;
get_workflow_field(connections, Workflow, Default) ->
    case tuple_size(Workflow) of
        7 -> element(5, Workflow);  % connections is 5th field
        _ -> Default
    end;
get_workflow_field(_, _Workflow, Default) ->
    Default.

%%--------------------------------------------------------------------
%% @private
%% @doc Get a field from a pattern record.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pattern_field(atom(), term(), term()) -> term().

get_pattern_field(task_ids, Pattern, Default) ->
    case tuple_size(Pattern) of
        2 -> element(2, Pattern);
        _ -> Default
    end;
get_pattern_field(branch_task_ids, Pattern, Default) ->
    case tuple_size(Pattern) of
        3 -> element(3, Pattern);
        _ -> Default
    end;
get_pattern_field(branches, Pattern, Default) ->
    case tuple_size(Pattern) of
        3 -> element(3, Pattern);
        _ -> Default
    end;
get_pattern_field(_, _Pattern, Default) ->
    Default.

%%--------------------------------------------------------------------
%% @private
%% @doc Set a field in a pattern record.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_pattern_field(atom(), term(), term()) -> term().

set_pattern_field(task_ids, Value, Pattern) ->
    setelement(2, Pattern, Value);
set_pattern_field(_Field, _Value, Pattern) ->
    Pattern.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
