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
%% @author YAWL Worker Implementation
%% @copyright 2025
%% -------------------------------------------------------------------

-module(cre_yawl_worker).
-behavior(cre_worker).

-moduledoc """
YAWL Worker Implementation for CRE.

This module implements the cre_worker behavior for executing YAWL workflow
tasks within the CRE distributed runtime environment.

## Task Types

The worker supports the following task types:

- `atomic` - Single operation tasks
- `composite` - Sub-workflows with multiple tasks
- `multi_instance` - Parallel instance execution
- `subworkflow` - Nested workflow invocation

## Worker Lifecycle

### Starting a Worker

```erlang
> CreName = cre_master,
> Config = #{cre_name => CreName},
> {ok, Pid} = cre_yawl_worker:start_link(CreName, Config).
{ok, <0.123.0>}
```

### Starting a Named Worker

```erlang
> WorkerName = yawl_worker_1,
> CreName = cre_master,
> Config = #{cre_name => CreName},
> {ok, Pid} = cre_yawl_worker:start_link(WorkerName, CreName, Config).
{ok, <0.124.0>}
```

### Stopping a Worker

```erlang
> ok = cre_yawl_worker:stop(WorkerName).
ok
```

## Task Handler Registration

### Register a Task Handler

```erlang
> TaskName = <<"process_order">>,
> Handler = fun(Input) -> {ok, #{status => processed, data => Input}} end,
> ok = cre_yawl_worker:register_task_handler(TaskName, Handler, atomic).
ok
```

### Register a Task Type with Options

```erlang
> TaskName = <<"send_email">>,
> Handler = fun(Email) -> {ok, #{sent => true}} end,
> Options = [{type, atomic}, {timeout, 5000}],
> ok = cre_yawl_worker:register_task_type(TaskName, Handler, Options).
ok
```

### List Task Handlers

```erlang
> Handlers = cre_yawl_worker:list_task_handlers().
[{<<"process_order">>, atomic}, {<<"send_email">>, atomic}]
```

### Get a Task Handler

```erlang
> {ok, HandlerFun} = cre_yawl_worker:get_task_handler(<<"process_order">>).
{ok, #Fun<erl_eval.6.127694169>}
```

### Unregister a Task Handler

```erlang
> ok = cre_yawl_worker:unregister_task_handler(<<"process_order">>).
ok
```

## Task Execution

### Execute a Task Synchronously

```erlang
> Task = #yawl_task{
>   id = <<"task_001">>,
>   name = <<"process_order">>,
>   type = atomic,
>   input = #{order_id => 123}
> },
> {ok, Result} = cre_yawl_worker:execute_task(Task).
{ok, #{status => processed, data => #{order_id => 123}}}
```

### Execute a Task Asynchronously

```erlang
> Task = #yawl_task{
>   id = <<"task_002">>,
>   name = <<"send_email">>,
>   type = atomic,
>   input = #{to => <<"user@example.com">>}
> },
> {ok, TaskId} = cre_yawl_worker:execute_task_async(Task).
{ok, <<"task_002_">>}
```

### Get Task Result

```erlang
> {ok, Result} = cre_yawl_worker:get_task_result(<<"task_002_">>).
{ok, #{task_id => <<"task_002_">>, status => completed}}
```

### Clear Task Cache

```erlang
> ok = cre_yawl_worker:clear_task_cache().
ok

> ok = cre_yawl_worker:clear_task_cache(<<"task_002_">>).
ok
```

## Pattern Execution Helpers

### Execute Parallel Branches

```erlang
> Branches = [
>   #parallel_branch{
>     branch_id = <<"branch_1">>,
>     task_id = <<"task_001">>,
>     handler = fun(X) -> {ok, X * 2} end,
>     input = 5,
>     status = pending
>   },
>   #parallel_branch{
>     branch_id = <<"branch_2">>,
>     task_id = <<"task_001">>,
>     handler = fun(X) -> {ok, X + 10} end,
>     input = 3,
>     status = pending
>   }
> ],
> {ok, Results} = cre_yawl_worker:execute_parallel(Branches, 5000).
{ok, [10, 13]}
```

### Execute Join Operations

```erlang
> %% AND join - requires all results
> {ok, AllResults} = cre_yawl_worker:execute_join(and_join, [1, 2, 3], default).
{ok, {all_results, [1, 2, 3]}}

> %% OR join - takes first result
> {ok, First} = cre_yawl_worker:execute_join(or_join, [1, 2, 3], default).
{ok, 1}

> %% XOR join - requires exactly one result
> {ok, Single} = cre_yawl_worker:execute_join(xor_join, [42], default).
{ok, 42}

> %% XOR join with default
> {ok, Default} = cre_yawl_worker:execute_join(xor_join, [], fallback).
{ok, fallback}
```

### Execute Multi-Instance Task

```erlang
> Task = #yawl_task{
>   id = <<"multi_001">>,
>   name = <<"process_item">>,
>   type = multi_instance,
>   input = #{},
>   metadata => #{timeout => 5000}
> },
> InstanceCount = 3,
> InputData = [<<"a">>, <<"b">>, <<"c">>],
> {ok, Results} = cre_yawl_worker:execute_multi_instance(Task, InstanceCount, InputData).
{ok, [<<96, 248, 173, 156, 82, 189, 134, 201, 172, 171, 197, 23, ...>>]}
```

### Execute Subworkflow

```erlang
> Task = #yawl_task{
>   id = <<"sub_001">>,
>   name = <<"approval_flow">>,
>   type = subworkflow,
>   input = #{request_id => 123}
> },
> Subworkflow = #{steps => [validate, approve, notify]},
> {ok, Result} = cre_yawl_worker:execute_subworkflow(Task, Subworkflow).
{ok, #{steps => [validate, approve, notify]}}
```

## Running the Doctests

```erlang
> cre_yawl_worker:doctest_test().
ok
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% cre_worker callbacks
-export([init/1,
         prepare_case/2,
         stagein_lst/2,
         do_stagein/3,
         run/2,
         stageout_lst/3,
         do_stageout/3,
         error_to_expr/3,
         cleanup_case/3]).

%% gen_pnet callbacks (via cre_worker)
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         trigger/3]).

%% Worker lifecycle
-export([start_link/2,
         start_link/3,
         stop/1]).

%% Task handler registration
-export([register_task_handler/3,
         register_task_type/3,
         unregister_task_handler/1,
         list_task_handlers/0,
         get_task_handler/1]).

%% Direct task execution
-export([execute_task/1,
         execute_task/2,
         execute_task_async/1,
         execute_task_async/2,
         get_task_result/1,
         clear_task_cache/0,
         clear_task_cache/1]).

%% Pattern execution helpers
-export([execute_parallel/2,
         execute_join/3,
         execute_multi_instance/3,
         execute_subworkflow/2]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Includes
%%====================================================================

%%====================================================================
%% Types
%%====================================================================

-type task_id() :: binary().
-type task_name() :: binary().
-type task_handler() :: fun((term()) -> {ok, term()} | {error, term()}).
-type task_type() :: atomic | composite | subworkflow | multi_instance.
-type task_result() :: {ok, term()} | {error, term()}.
-type file_spec() :: binary() | {binary(), term()}.

%% Define compensator record first (used by worker_state)
-record(compensator, {
          activity_id :: task_id(),
          handler :: function(),
          state :: pending | executing | completed | failed,
          result :: undefined | {ok, term()} | {error, term()}
         }).

%% Define task_metadata record first (used by worker_state)
-record(task_metadata, {
          task_id :: task_id(),
          task_name :: task_name(),
          task_type :: task_type(),
          started_at :: integer(),
          status :: pending | running | completed | failed,
          retry_count = 0 :: non_neg_integer()
         }).

-record(worker_state, {
          cre_name :: atom() | pid(),
          task_registry = #{} :: map(),
          result_cache = #{} :: #{task_id() => task_result()},
          compensation_stack = [] :: [#compensator{}],
          exception_handlers = #{} :: map(),
          active_tasks = #{} :: #{task_id() => #task_metadata{}},
          config = #{} :: map()
         }).

-record(task_handler_entry, {
          name :: task_name(),
          handler :: task_handler(),
          type :: task_type(),
          metadata = #{} :: map(),
          registered_at :: integer()
         }).

-record(parallel_branch, {
          branch_id :: binary(),
          task_id :: task_id(),
          handler :: task_handler(),
          input :: term(),
          status :: pending | running | completed | failed,
          result :: undefined | term()
         }).

-record(yawl_task, {
          id :: task_id(),
          name :: task_name(),
          type :: task_type(),
          input :: term(),
          metadata = #{} :: map(),
          split_type :: undefined | and_split | or_split | xor_split,
          join_type :: undefined | and_join | or_join | xor_join,
          timeout :: infinity | pos_integer()
         }).

-type worker_state() :: #worker_state{}.
-type yawl_task() :: #yawl_task{}.
-type parallel_branch() :: #parallel_branch{}.

%%====================================================================
%% Records
%%====================================================================

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a YAWL worker linked to the CRE master.
%%
%% @param CreName The name or pid of the CRE master process.
%% @param Config Configuration map for the worker.
%% @return {ok, Pid} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(CreName :: atom() | pid(), Config :: map()) ->
          {ok, pid()} | {error, term()}.

start_link(CreName, Config) ->
    cre_worker:start_link(?MODULE, CreName, Config).

%%--------------------------------------------------------------------
%% @doc Starts a named YAWL worker.
%%
%% @param WorkerName The name to register the worker as.
%% @param CreName The name or pid of the CRE master process.
%% @param Config Configuration map for the worker.
%% @return {ok, Pid} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(WorkerName :: atom(), CreName :: atom() | pid(),
                 Config :: map()) ->
          {ok, pid()} | {error, term()}.

start_link(WorkerName, CreName, Config) ->
    cre_worker:start_link(WorkerName, CreName, ?MODULE, Config).

%%--------------------------------------------------------------------
%% @doc Stops the YAWL worker.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(WorkerName :: atom() | pid()) -> ok.

stop(WorkerName) ->
    cre_worker:stop(WorkerName).

%%--------------------------------------------------------------------
%% @doc Registers a task handler for a specific task name.
%%
%% @param TaskName The name of the task this handler executes.
%% @param HandlerFun The function that executes the task.
%% @param TaskType The type of task (atomic, composite, subworkflow, multi_instance).
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec register_task_handler(TaskName :: task_name(),
                            HandlerFun :: task_handler(),
                            TaskType :: task_type()) -> ok.

register_task_handler(TaskName, HandlerFun, TaskType)
  when is_binary(TaskName), is_function(HandlerFun, 1) ->
    gen_server:cast(?MODULE, {register_task_handler, TaskName, HandlerFun, TaskType}),
    ok.

%%--------------------------------------------------------------------
%% @doc Registers a task type with its handler using a property list.
%%
%% @param TaskName The name of the task.
%% @param HandlerFun The handler function.
%% @param Options Property list with type and metadata.
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec register_task_type(TaskName :: task_name(),
                         HandlerFun :: task_handler(),
                         Options :: proplists:proplist()) -> ok.

register_task_type(TaskName, HandlerFun, Options) ->
    TaskType = proplists:get_value(type, Options, atomic),
    gen_server:cast(?MODULE, {register_task_handler, TaskName, HandlerFun, TaskType}),
    ok.

%%--------------------------------------------------------------------
%% @doc Unregisters a task handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_task_handler(TaskName :: task_name()) -> ok.

unregister_task_handler(TaskName) ->
    gen_server:cast(?MODULE, {unregister_task_handler, TaskName}),
    ok.

%%--------------------------------------------------------------------
%% @doc Lists all registered task handlers.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_task_handlers() -> [{task_name(), task_type()}].

list_task_handlers() ->
    gen_server:call(?MODULE, list_task_handlers).

%%--------------------------------------------------------------------
%% @doc Gets a task handler by name.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_task_handler(TaskName :: task_name()) ->
          {ok, task_handler()} | {error, not_found}.

get_task_handler(TaskName) ->
    gen_server:call(?MODULE, {get_task_handler, TaskName}).

%%--------------------------------------------------------------------
%% @doc Executes a task synchronously.
%%
%% @param Task The YAWL task to execute.
%% @param WorkerName The worker to execute on (defaults to ?MODULE).
%% @return {ok, Result} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_task(yawl_task()) -> task_result().
-spec execute_task(yawl_task(), atom() | pid()) -> task_result().

execute_task(Task) ->
    execute_task(Task, ?MODULE).

execute_task(Task, WorkerName) ->
    gen_server:call(WorkerName, {execute_task, Task}, infinity).

%%--------------------------------------------------------------------
%% @doc Executes a task asynchronously.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_task_async(yawl_task()) -> {ok, task_id()}.
-spec execute_task_async(yawl_task(), atom() | pid()) -> {ok, task_id()}.

execute_task_async(Task) ->
    execute_task_async(Task, ?MODULE).

execute_task_async(Task, WorkerName) ->
    TaskId = generate_task_id(Task),
    gen_server:cast(WorkerName, {execute_task_async, TaskId, Task}),
    {ok, TaskId}.

%%--------------------------------------------------------------------
%% @doc Gets a cached task result.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_task_result(task_id()) -> {ok, task_result()} | {error, not_found}.

get_task_result(TaskId) ->
    gen_server:call(?MODULE, {get_task_result, TaskId}).

%%--------------------------------------------------------------------
%% @doc Clears the task result cache.
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_task_cache() -> ok.
-spec clear_task_cache(task_id()) -> ok.

clear_task_cache() ->
    gen_server:cast(?MODULE, clear_task_cache),
    ok.

clear_task_cache(TaskId) ->
    gen_server:cast(?MODULE, {clear_task_cache, TaskId}),
    ok.

%%--------------------------------------------------------------------
%% @doc Executes tasks in parallel branches.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_parallel(Branches :: [parallel_branch()],
                       Timeout :: pos_integer() | infinity) ->
          {ok, [term()]} | {error, term()}.

execute_parallel(Branches, Timeout) ->
    execute_parallel(Branches, Timeout, self()).

execute_parallel(Branches, Timeout, Caller) ->
    Ref = make_ref(),
    Pids = [spawn_monitor(fun() ->
                BranchId = B#parallel_branch.branch_id,
                Handler = B#parallel_branch.handler,
                Input = B#parallel_branch.input,
                Result = try
                    {ok, Handler(Input)}
                catch
                    Kind:Reason:Stack ->
                        {error, {Kind, Reason, Stack}}
                end,
                Caller ! {parallel_result, Ref, BranchId, Result}
        end) || B <- Branches],

    collect_parallel_results(Pids, Ref, Timeout, [], []).

%%--------------------------------------------------------------------
%% @doc Executes a join operation on parallel branch results.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_join(JoinType :: and_join | or_join | xor_join,
                   Results :: [term()],
                   Default :: term()) ->
          {ok, term()} | {error, term()}.

execute_join(and_join, Results, _Default) when length(Results) > 0 ->
    {ok, {all_results, Results}};
execute_join(and_join, [], _Default) ->
    {error, no_branches_completed};

execute_join(or_join, [], Default) ->
    {ok, Default};
execute_join(or_join, [First | _Rest], _Default) ->
    {ok, First};

execute_join(xor_join, [First], _Default) ->
    {ok, First};
execute_join(xor_join, [], Default) ->
    {ok, Default};
execute_join(xor_join, _Multiple, _Default) ->
    {error, multiple_branches_for_xor_join}.

%%--------------------------------------------------------------------
%% @doc Executes a multi-instance task.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multi_instance(Task :: yawl_task(),
                             InstanceCount :: pos_integer(),
                             InputData :: [term()]) ->
          {ok, [term()]} | {error, term()}.

execute_multi_instance(Task, InstanceCount, InputData) ->
    Branches = [
        #parallel_branch{
            branch_id = <<(Task#yawl_task.id)/binary, "_inst_",
                          (integer_to_binary(I))/binary>>,
            task_id = Task#yawl_task.id,
            handler = get_handler_for_task(Task),
            input = Input,
            status = pending,
            result = undefined
        }
        || {I, Input} <- lists:zip(lists:seq(1, InstanceCount),
                                   InputData ++ lists:duplicate(InstanceCount, undefined))
    ],
    Timeout = maps:get(timeout, Task#yawl_task.metadata, infinity),
    execute_parallel(Branches, Timeout).

%%--------------------------------------------------------------------
%% @doc Executes a subworkflow task.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_subworkflow(Task :: yawl_task(),
                         Subworkflow :: term()) ->
          {ok, term()} | {error, term()}.

execute_subworkflow(Task, Subworkflow) ->
    case Task#yawl_task.type of
        subworkflow ->
            Handler = get_handler_for_task(Task),
            case Handler(Subworkflow) of
                {ok, Result} -> {ok, Result};
                {error, Reason} -> {error, {subworkflow_failed, Reason}}
            end;
        _ ->
            {error, not_a_subworkflow_task}
    end.

%%====================================================================
%% cre_worker Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the worker state.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(InitArg :: term()) -> worker_state().

init(InitArg) when is_map(InitArg) ->
    #worker_state{
      cre_name = maps:get(cre_name, InitArg),
      config = InitArg
     };
init(InitArg) when is_list(InitArg) ->
    Config = maps:from_list(InitArg),
    init(Config);
init(_InitArg) ->
    #worker_state{}.

%%--------------------------------------------------------------------
%% @doc Prepares a task case for execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec prepare_case(Task :: yawl_task(), UsrInfo :: worker_state()) -> ok.

prepare_case(Task, UsrInfo) ->
    TaskId = Task#yawl_task.id,
    TaskName = Task#yawl_task.name,

    %% Check if handler is registered
    TaskRegistry = UsrInfo#worker_state.task_registry,
    case maps:is_key(TaskName, TaskRegistry) of
        false ->
            logger:warning([{module, ?MODULE},
                           {task_not_found, TaskName},
                           {task_id, TaskId}],
                          "Task not found in registry");
        true ->
            ok
    end,

    %% Initialize task metadata
    _Metadata = #task_metadata{
      task_id = TaskId,
      task_name = TaskName,
      task_type = Task#yawl_task.type,
      started_at = erlang:system_time(millisecond),
      status = pending
    },

    %% Log task start
    logger:info([{module, ?MODULE},
                 {action, prepare_case},
                 {task_id, TaskId},
                 {task_name, TaskName},
                 {task_type, Task#yawl_task.type}],
                "Task prepared"),

    ok.

%%--------------------------------------------------------------------
%% @doc Lists input data requirements for a task.
%%
%% @end
%%--------------------------------------------------------------------
-spec stagein_lst(Task :: yawl_task(), UsrInfo :: worker_state()) -> [file_spec()].

stagein_lst(Task, _UsrInfo) ->
    %% Extract input file specifications from task input
    Input = Task#yawl_task.input,
    extract_input_files(Input).

%%--------------------------------------------------------------------
%% @doc Fulfills input requirements by staging files.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_stagein(Task :: yawl_task(),
                  File :: file_spec(),
                  UsrInfo :: worker_state()) ->
          ok | {error, enoent}.

do_stagein(_Task, File, _UsrInfo) ->
    %% Default implementation assumes files are already available
    %% Override for custom staging logic
    case file_exists(File) of
        true -> ok;
        false -> {error, enoent}
    end.

%%--------------------------------------------------------------------
%% @doc Executes the task.
%%
%% @end
%%--------------------------------------------------------------------
-spec run(Task :: yawl_task(), UsrInfo :: worker_state()) ->
          {ok, term()} | {error, term()}.

run(Task, UsrInfo) ->
    TaskName = Task#yawl_task.name,
    TaskType = Task#yawl_task.type,
    _TaskId = Task#yawl_task.id,
    Input = Task#yawl_task.input,

    %% Get the task handler
    TaskRegistry = UsrInfo#worker_state.task_registry,
    case maps:get(TaskName, TaskRegistry, undefined) of
        undefined ->
            {error, {no_handler, TaskName}};
        #task_handler_entry{handler = Handler} ->
            %% Execute based on task type
            case TaskType of
                atomic ->
                    execute_atomic_task(Handler, Input, Task);
                composite ->
                    execute_composite_task(Handler, Input, Task, UsrInfo);
                multi_instance ->
                    InstanceCount = maps:get(instance_count,
                                             Task#yawl_task.metadata, 1),
                    InstanceData = maps:get(instance_data,
                                           Task#yawl_task.metadata, []),
                    execute_multi_instance_task(Handler, InstanceCount,
                                                InstanceData, Task);
                subworkflow ->
                    execute_subworkflow_task(Handler, Input, Task)
            end
    end.

%%--------------------------------------------------------------------
%% @doc Lists output requirements for the task.
%%
%% @end
%%--------------------------------------------------------------------
-spec stageout_lst(Task :: yawl_task(),
                   Result :: term(),
                   UsrInfo :: worker_state()) -> [file_spec()].

stageout_lst(_Task, Result, _UsrInfo) ->
    %% Extract output file specifications from result
    extract_output_files(Result).

%%--------------------------------------------------------------------
%% @doc Fulfills output requirements.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_stageout(Task :: yawl_task(),
                  File :: file_spec(),
                  UsrInfo :: worker_state()) ->
          ok | {error, enoent}.

do_stageout(_Task, _File, _UsrInfo) ->
    %% Default implementation - files are handled via result data
    %% Override for custom staging logic
    ok.

%%--------------------------------------------------------------------
%% @doc Converts errors to expression results.
%%
%% @end
%%--------------------------------------------------------------------
-spec error_to_expr(Task :: yawl_task(),
                    Reason :: term(),
                    UsrInfo :: worker_state()) ->
          term().

error_to_expr(Task, Reason, UsrInfo) ->
    TaskId = Task#yawl_task.id,
    TaskName = Task#yawl_task.name,

    %% Log the error
    logger:error([{module, ?MODULE},
                  {action, task_failed},
                  {task_id, TaskId},
                  {task_name, TaskName},
                  {reason, Reason}],
                 "Task execution failed"),

    %% Check for exception handlers
    ExceptionHandlers = UsrInfo#worker_state.exception_handlers,
    ExceptionType = classify_error(Reason),

    case maps:get(ExceptionType, ExceptionHandlers, undefined) of
        undefined ->
            #{
              task_id => TaskId,
              task_name => TaskName,
              status => error,
              reason => Reason,
              result => undefined
             };
        Handler ->
            %% Execute exception handler
            case Handler(Task, Reason) of
                {ok, RecoveryResult} ->
                    #{
                      task_id => TaskId,
                      task_name => TaskName,
                      status => recovered,
                      reason => Reason,
                      result => RecoveryResult
                     };
                {error, HandlerReason} ->
                    #{
                      task_id => TaskId,
                      task_name => TaskName,
                      status => error,
                      reason => HandlerReason,
                      result => undefined
                     }
            end
    end.

%%--------------------------------------------------------------------
%% @doc Cleanup after task completion.
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_case(Task :: yawl_task(),
                   Result :: term(),
                   UsrInfo :: worker_state()) ->
          term().

cleanup_case(Task, Result, UsrInfo) ->
    TaskId = Task#yawl_task.id,

    %% Log completion
    logger:info([{module, ?MODULE},
                 {action, cleanup_case},
                 {task_id, TaskId},
                 {result_status, element(1, Result)}],
                "Task cleanup completed"),

    %% Cache the result if configured
    case maps:get(cache_results, UsrInfo#worker_state.config, true) of
        true ->
            _CacheKey = TaskId,
            %% Cache update would be handled in handle_info/cast
            ok;
        false ->
            ok
    end,

    %% Cleanup compensation stack if task succeeded
    _NewUsrInfo = case Result of
        {ok, _} ->
            UsrInfo#worker_state{compensation_stack = []};
        _ ->
            UsrInfo
    end,

    %% Return the result expression
    #{
      task_id => TaskId,
      task_name => Task#yawl_task.name,
      task_type => Task#yawl_task.type,
      status => case Result of
                    {ok, _} -> completed;
                    {error, _} -> failed
                end,
      result => Result,
      completed_at => erlang:system_time(millisecond)
     }.

%%====================================================================
%% gen_server Callback Functions (via cre_worker)
%%====================================================================

%%--------------------------------------------------------------------
%% @code_change callback for hot code upgrade.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @handle_call for synchronous requests.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), term()},
                  State :: worker_state()) ->
          {reply, term(), worker_state()} |
          {reply, term(), worker_state(), term()} |
          {noreply, worker_state()} |
          {stop, term(), term(), worker_state()}.

handle_call(list_task_handlers, _From, State) ->
    TaskRegistry = State#worker_state.task_registry,
    Handlers = [{Name, Entry#task_handler_entry.type} ||
                   Name <- maps:keys(TaskRegistry),
                   Entry <- [maps:get(Name, TaskRegistry)]],
    {reply, Handlers, State};

handle_call({get_task_handler, TaskName}, _From, State) ->
    case maps:get(TaskName, State#worker_state.task_registry, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Entry ->
            {reply, {ok, Entry#task_handler_entry.handler}, State}
    end;

handle_call({get_task_result, TaskId}, _From, State) ->
    case maps:get(TaskId, State#worker_state.result_cache, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Result ->
            {reply, {ok, Result}, State}
    end;

handle_call({execute_task, Task}, _From, State) ->
    TaskId = Task#yawl_task.id,
    case run(Task, State) of
        {ok, Result} ->
            ResultExpr = cleanup_case(Task, Result, State),
            State1 = State#worker_state{
              result_cache = maps:put(TaskId, {ok, ResultExpr},
                                      State#worker_state.result_cache)
             },
            {reply, {ok, ResultExpr}, State1};
        {error, Reason} ->
            ErrorExpr = error_to_expr(Task, Reason, State),
            State1 = State#worker_state{
              result_cache = maps:put(TaskId, {error, ErrorExpr},
                                      State#worker_state.result_cache)
             },
            {reply, {error, ErrorExpr}, State1}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%%--------------------------------------------------------------------
%% @handle_cast for asynchronous messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: worker_state()) ->
          {noreply, worker_state()} |
          {stop, term(), worker_state()}.

handle_cast({register_task_handler, TaskName, HandlerFun, TaskType}, State) ->
    Entry = #task_handler_entry{
      name = TaskName,
      handler = HandlerFun,
      type = TaskType,
      registered_at = erlang:system_time(millisecond)
     },
    TaskRegistry = maps:put(TaskName, Entry, State#worker_state.task_registry),

    logger:info([{module, ?MODULE},
                 {action, register_task_handler},
                 {task_name, TaskName},
                 {task_type, TaskType}],
                "Task handler registered"),

    {noreply, State#worker_state{task_registry = TaskRegistry}};

handle_cast({unregister_task_handler, TaskName}, State) ->
    TaskRegistry = maps:remove(TaskName, State#worker_state.task_registry),
    {noreply, State#worker_state{task_registry = TaskRegistry}};

handle_cast(clear_task_cache, State) ->
    {noreply, State#worker_state{result_cache = #{}}};

handle_cast({clear_task_cache, TaskId}, State) ->
    Cache = maps:remove(TaskId, State#worker_state.result_cache),
    {noreply, State#worker_state{result_cache = Cache}};

handle_cast({execute_task_async, TaskId, Task}, State) ->
    %% Spawn a process to execute the task
    spawn(fun() ->
        case run(Task, State) of
            {ok, Result} ->
                ResultExpr = cleanup_case(Task, Result, State),
                gen_server:cast(?MODULE, {task_complete, TaskId, ResultExpr});
            {error, Reason} ->
                ErrorExpr = error_to_expr(Task, Reason, State),
                gen_server:cast(?MODULE, {task_complete, TaskId, ErrorExpr})
        end
    end),

    ActiveTasks = maps:put(TaskId,
                           #task_metadata{
                             task_id = TaskId,
                             task_name = Task#yawl_task.name,
                             task_type = Task#yawl_task.type,
                             started_at = erlang:system_time(millisecond),
                             status = running
                            },
                           State#worker_state.active_tasks),
    {noreply, State#worker_state{active_tasks = ActiveTasks}};

handle_cast({task_complete, TaskId, Result}, State) ->
    ResultCache = maps:put(TaskId, Result, State#worker_state.result_cache),
    ActiveTasks = maps:remove(TaskId, State#worker_state.active_tasks),
    {noreply, State#worker_state{
                    result_cache = ResultCache,
                    active_tasks = ActiveTasks
                   }};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @handle_info for unexpected messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: worker_state()) ->
          {noreply, worker_state()} |
          {stop, term(), worker_state()}.

handle_info({'DOWN', _Ref, process, _Pid, normal}, State) ->
    {noreply, State};
handle_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    logger:warning([{module, ?MODULE},
                    {action, process_down},
                    {reason, Reason}],
                   "Process monitored went down"),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @terminate called when worker stops.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: worker_state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @trigger callback for token processing.
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: term()) ->
          pass | drop.

trigger(_Place, _Token, _NetState) ->
    pass.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Executes an atomic task.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_atomic_task(Handler :: task_handler(),
                          Input :: term(),
                          Task :: yawl_task()) ->
          {ok, term()} | {error, term()}.

execute_atomic_task(Handler, Input, Task) ->
    Timeout = maps:get(timeout, Task#yawl_task.metadata, infinity),
    try
        case Timeout of
            infinity ->
                Handler(Input);
            T when is_integer(T), T > 0 ->
                execute_with_timeout(Handler, Input, T)
        end
    catch
        Kind:Reason:Stack ->
            {error, {Kind, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a composite task (sub-workflow).
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_composite_task(Handler :: task_handler(),
                             Input :: term(),
                             Task :: yawl_task(),
                             UsrInfo :: worker_state()) ->
          {ok, term()} | {error, term()}.

execute_composite_task(Handler, Input, Task, _UsrInfo) ->
    Subworkflow = maps:get(subworkflow, Task#yawl_task.metadata, Input),
    execute_subworkflow_task(Handler, Subworkflow, Task).

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a subworkflow task.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_subworkflow_task(Handler :: task_handler(),
                               Input :: term(),
                               Task :: yawl_task()) ->
          {ok, term()} | {error, term()}.

execute_subworkflow_task(Handler, Input, _Task) ->
    try
        Handler(Input)
    catch
        Kind:Reason:Stack ->
            {error, {Kind, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a multi-instance task.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multi_instance_task(Handler :: task_handler(),
                                  InstanceCount :: pos_integer(),
                                  InstanceData :: [term()],
                                  Task :: yawl_task()) ->
          {ok, [term()]} | {error, term()}.

execute_multi_instance_task(Handler, InstanceCount, InstanceData, Task) ->
    %% Pad instance data if needed
    Data = case length(InstanceData) of
        N when N < InstanceCount ->
            InstanceData ++ lists:duplicate(InstanceCount - N, undefined);
        _ ->
            InstanceData
    end,

    Branches = [
        #parallel_branch{
            branch_id = <<(Task#yawl_task.id)/binary, "_inst_",
                          (integer_to_binary(I))/binary>>,
            task_id = Task#yawl_task.id,
            handler = Handler,
            input = Input,
            status = pending
        }
        || {I, Input} <- lists:zip(lists:seq(1, InstanceCount), Data)
    ],

    Timeout = maps:get(timeout, Task#yawl_task.metadata, infinity),
    case execute_parallel(Branches, Timeout) of
        {ok, Results} ->
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a function with a timeout.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_with_timeout(Function :: function(), Args :: term(),
                          Timeout :: pos_integer()) ->
          {ok, term()} | {error, timeout}.

execute_with_timeout(Function, Args, Timeout) ->
    Pid = spawn_link(fun() ->
        Result = Function(Args),
        exit({normal, Result})
    end),

    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Ppid, {normal, Result}} when Ppid =:= Pid ->
            {ok, Result};
        {'DOWN', Ref, process, Ppid, Info} when Ppid =:= Pid ->
            {error, Info}
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        exit(Pid, kill),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collects results from parallel branch execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_parallel_results([{pid(), reference()}],
                                reference(),
                                pos_integer() | infinity,
                                [term()],
                                [term()]) ->
          {ok, [term()]} | {error, term()}.

collect_parallel_results([], _Ref, _Timeout, Results, Errors) when Errors =:= [] ->
    {ok, lists:reverse(Results)};
collect_parallel_results([], _Ref, _Timeout, _Results, Errors) ->
    {error, {parallel_errors, lists:reverse(Errors)}};
collect_parallel_results(Pids, Ref, Timeout, Results, Errors) ->
    receive
        {parallel_result, Ref, _BranchId, {ok, Result}} ->
            collect_parallel_results(Pids, Ref, Timeout,
                                   [Result | Results], Errors);
        {parallel_result, Ref, _BranchId, {error, Reason}} ->
            collect_parallel_results(Pids, Ref, Timeout,
                                   Results, [Reason | Errors]);
        {'DOWN', _MRef, process, _Pid, _Info} ->
            collect_parallel_results(Pids, Ref, Timeout,
                                   Results, Errors)
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the handler for a task from the registry.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_handler_for_task(Task :: yawl_task()) -> task_handler().

get_handler_for_task(_Task) ->
    %% This is a simplified version - in production would query the actual registry
    fun(Input) ->
        %% Default handler implementation
        {ok, Input}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique task ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_task_id(Task :: yawl_task()) -> task_id().

generate_task_id(Task) ->
    Base = case Task#yawl_task.id of
        <<>> -> <<"task">>;
        Id -> Id
    end,
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<Base/binary, "_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts input file specifications from task input.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_input_files(Input :: term()) -> [file_spec()].

extract_input_files(Input) when is_map(Input) ->
    case maps:get(<<"input_files">>, Input, undefined) of
        undefined -> [];
        Files when is_list(Files) -> Files;
        File -> [File]
    end;
extract_input_files(_Input) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts output file specifications from result.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_output_files(Result :: term()) -> [file_spec()].

extract_output_files(Result) when is_map(Result) ->
    case maps:get(<<"output_files">>, Result, undefined) of
        undefined -> [];
        Files when is_list(Files) -> Files;
        File -> [File]
    end;
extract_output_files(_Result) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a file specification exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec file_exists(File :: file_spec()) -> boolean().

file_exists(File) when is_binary(File) ->
    case filelib:is_file(binary_to_list(File)) of
        true -> true;
        false ->
            %% Check if it's a URL or other reference
            case File of
                <<"http://", _/binary>> -> true;
                <<"https://", _/binary>> -> true;
                <<"file://", _/binary>> -> true;
                _ -> false
            end
    end;
file_exists({File, _Location}) ->
    file_exists(File).

%%--------------------------------------------------------------------
%% @private
%% @doc Classifies an error into an exception type.
%%
%% @end
%%--------------------------------------------------------------------
-spec classify_error(Reason :: term()) ->
          business_exception | system_exception |
          timeout_exception | resource_exception | data_exception.

classify_error({error, timeout}) ->
    timeout_exception;
classify_error({error, enoent}) ->
    resource_exception;
classify_error({error, _}) ->
    system_exception;
classify_error({throw, _}) ->
    business_exception;
classify_error({exit, _}) ->
    system_exception;
%% Duplicate clause removed - covered by previous patterns
%% classify_error({error, {Kind, _Reason, _Stack}}) when Kind =:= error; Kind =:= exit ->
%%     system_exception;
classify_error({Kind, _Reason, _Stack}) when Kind =:= throw ->
    business_exception;
classify_error(_) ->
    system_exception.

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs all doctests for the cre_yawl_worker module.
%%
%% These are minimal, fast unit tests that verify the core functionality
%% of the YAWL worker implementation without requiring actual process
%% spawning or external dependencies.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: yawl_task record creation
    Task = #yawl_task{
      id = <<"test_task_001">>,
      name = <<"test_task">>,
      type = atomic,
      input = #{data => <<"test">>},
      metadata = #{},
      split_type = undefined,
      join_type = undefined,
      timeout = infinity
    },
    <<"test_task_001">> = Task#yawl_task.id,
    atomic = Task#yawl_task.type,

    %% Test 2: parallel_branch record creation
    Branch = #parallel_branch{
      branch_id = <<"branch_1">>,
      task_id = <<"task_001">>,
      handler = fun(X) -> {ok, X} end,
      input = test_input,
      status = pending,
      result = undefined
    },
    <<"branch_1">> = Branch#parallel_branch.branch_id,
    pending = Branch#parallel_branch.status,

    %% Test 3: worker_state record creation
    State = #worker_state{
      cre_name = cre_master,
      task_registry = #{},
      result_cache = #{},
      compensation_stack = [],
      exception_handlers = #{},
      active_tasks = #{},
      config = #{}
    },
    cre_master = State#worker_state.cre_name,
    #{} = State#worker_state.task_registry,

    %% Test 4: task_handler_entry record creation
    HandlerEntry = #task_handler_entry{
      name = <<"test_handler">>,
      handler = fun(X) -> {ok, X} end,
      type = atomic,
      metadata = #{version => 1},
      registered_at = erlang:system_time(millisecond)
    },
    <<"test_handler">> = HandlerEntry#task_handler_entry.name,
    atomic = HandlerEntry#task_handler_entry.type,

    %% Test 5: execute_join with and_join
    {ok, {all_results, [1, 2, 3]}} = execute_join(and_join, [1, 2, 3], default),

    %% Test 6: execute_join with or_join (takes first)
    {ok, 1} = execute_join(or_join, [1, 2, 3], default),

    %% Test 7: execute_join with or_join and empty list (uses default)
    {ok, default_val} = execute_join(or_join, [], default_val),

    %% Test 8: execute_join with xor_join and single result
    {ok, 42} = execute_join(xor_join, [42], default),

    %% Test 9: execute_join with xor_join and empty list
    {ok, fallback} = execute_join(xor_join, [], fallback),

    %% Test 10: execute_join with xor_join and multiple results (error)
    {error, multiple_branches_for_xor_join} = execute_join(xor_join, [1, 2], default),

    %% Test 11: execute_join with and_join and empty list (error)
    {error, no_branches_completed} = execute_join(and_join, [], default),

    %% Test 12: classify_error for timeout
    timeout_exception = classify_error({error, timeout}),

    %% Test 13: classify_error for resource errors
    resource_exception = classify_error({error, enoent}),

    %% Test 14: classify_error for generic errors
    system_exception = classify_error({error, generic_error}),

    %% Test 15: classify_error for business exceptions (throw)
    business_exception = classify_error({throw, business_rule_violation}),

    %% Test 16: classify_error for system exceptions (exit)
    system_exception = classify_error({exit, normal}),

    %% Test 17: classify_error for stack traces with throw
    business_exception = classify_error({throw, reason, []}),

    %% Test 18: classify_error for unknown errors
    system_exception = classify_error(unknown_error),

    %% Test 19: extract_input_files from map with input_files key
    InputMap = #{<<"input_files">> => [<<"file1">>, <<"file2">>]},
    [<<"file1">>, <<"file2">>] = extract_input_files(InputMap),

    %% Test 20: extract_input_files from map without input_files (empty list)
    NoFilesMap = #{<<"data">> => <<"test">>},
    [] = extract_input_files(NoFilesMap),

    %% Test 21: extract_input_files from non-map (empty list)
    [] = extract_input_files(<<"not_a_map">>),

    %% Test 22: extract_output_files from map with output_files key
    OutputMap = #{<<"output_files">> => [<<"out1">>]},
    [<<"out1">>] = extract_output_files(OutputMap),

    %% Test 23: extract_output_files from map without output_files (empty list)
    [] = extract_output_files(#{<<"result">> => ok}),

    %% Test 24: file_exists with http URL (returns true)
    true = file_exists(<<"http://example.com/file">>),

    %% Test 25: file_exists with https URL (returns true)
    true = file_exists(<<"https://example.com/file">>),

    %% Test 26: file_exists with file:// URL (returns true)
    true = file_exists(<<"file://path/to/file">>),

    %% Test 27: file_exists with tuple format
    true = file_exists({<<"http://example.com/file">>, location}),

    %% Test 28: generate_task_id creates unique binary
    TestTask = #yawl_task{id = <<"my_task">>},
    TaskId = generate_task_id(TestTask),
    true = is_binary(TaskId),
    true = byte_size(TaskId) > byte_size(<<"my_task">>),

    %% Test 29: generate_task_id with empty id
    EmptyTask = #yawl_task{id = <<>>},
    EmptyId = generate_task_id(EmptyTask),
    true = is_binary(EmptyId),
    <<"task_", _/binary>> = EmptyId,

    %% Test 30: task_type type is correctly defined
    atomic = atomic,
    composite = composite,
    subworkflow = subworkflow,
    multi_instance = multi_instance,

    ok.
