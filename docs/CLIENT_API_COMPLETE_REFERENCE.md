# CRE Client API Complete Reference

## Overview

This comprehensive reference document provides complete documentation for the CRE client API modules, including all public functions, types, callbacks, usage patterns, integration examples, and best practices. The document covers both the generic `cre_client` module and the YAWL-specific `cre_yawl_client` module.

## Table of Contents

1. [cre_client Module](#cre_client-module)
   - [Public API Functions](#public-api-functions)
   - [Callback Interface](#callback-interface)
   - [Client State Management](#client-state-management)
   - [Doctest Examples](#doctest-examples-cre_client)

2. [cre_yawl_client Module](#cre_yawl_client-module)
   - [Public API Functions](#public-api-functions-1)
   - [Callback Interface](#callback-interface-1)
   - [Pattern Support](#pattern-support)
   - [Doctest Examples](#doctest-examples-cre_yawl_client)

3. [Integration Patterns](#integration-patterns)
   - [Client-Master Integration](#client-master-integration)
   - [Error Handling Integration](#error-handling-integration)
   - [Performance Integration](#performance-integration)

4. [Best Practices](#best-practices)
   - [Client Lifecycle Management](#client-lifecycle-management)
   - [Error Recovery Strategies](#error-recovery-strategies)
   - [Performance Optimization](#performance-optimization)

5. [Complete Examples](#complete-examples)
   - [Simple Workflow](#simple-workflow)
   - [Complex Multi-Pattern Workflow](#complex-multi-pattern-workflow)
   - [Human-in-the-Loop Workflow](#human-in-the-loop-workflow)

## cre_client Module

### Public API Functions

#### `start_link/3` - Start Anonymous Client

```erlang
%% @doc Starts an anonymous CRE client gen_server.

Links the client to the CRE master process identified by `CreName`.
The `ClientMod` module must implement the `cre_client` callback interface
to customize workflow evaluation behavior.

## Parameters

- `CreName`: Name or pid of the CRE master process
- `ClientMod`: Module implementing the client callbacks
- `ClientArg`: Argument passed to `ClientMod:init/1`

## Returns

`{ok, Pid}` on success, where `Pid` is the client process identifier

## Example

```erlang
1> {ok, Pid} = cre_client:start_link(my_cre, my_client_mod, []).
{ok,<0.123.0>}
```
```

#### `start_link/4` - Start Named Client

```erlang
%% @doc Starts a named CRE client gen_server.

Registers the client under `ClientName` (an atom) and links it to the
CRE master process identified by `CreName`. The client uses `ClientMod`
for callbacks during workflow evaluation.

## Parameters

- `ClientName`: Atom name to register the client process
- `CreName`: Name or pid of the CRE master process
- `ClientMod`: Module implementing the client callbacks
- `ClientArg`: Argument passed to `ClientMod:init/1`

## Returns

`{ok, Pid}` on success, where `Pid` is the client process identifier

## Example

```erlang
1> {ok, Pid} = cre_client:start_link(my_client, my_cre, my_client_mod, []).
{ok,<0.456.0>}
2> whereis(my_client).
<0.456.0>
```
```

#### `eval/2` - Evaluate Workflow Expression

```erlang
%% @doc Evaluates a workflow expression.

Submits the expression `E` for evaluation through the connected CRE master.
This is a blocking call that only returns when the workflow completes or fails.

The evaluation process:
1. Loads the expression via `ClientMod:load/2`
2. Iteratively evaluates via `ClientMod:step/2`
3. Collects worker replies
4. Returns the final result via `ClientMod:unload/2`

## Parameters

- `ClientName`: Name or pid of the client process
- `E`: Workflow expression to evaluate

## Returns

The result from `ClientMod:unload/2` when evaluation completes

## Example

```erlang
1> {ok, Pid} = cre_client:start_link(my_cre, my_client_mod, []).
1> Result = cre_client:eval(Pid, my_workflow_expr).
{ok, #{result => "completed"}}
```
```

#### `cre_reply/4` - Handle CRE Master Reply

```erlang
%% @doc Handles a reply from the CRE master.

Asynchronously delivers a worker reply to the client. Called by the CRE
master when a worker completes a task. The reply is queued and processed
during the next evaluation step.

## Parameters

- `ClientName`: Name or pid of the client process
- `I`: Request identifier (typically the gen_server `From` tuple)
- `A`: The task argument that was sent to the worker
- `Delta`: The result value returned by the worker

## Returns

`ok` (cast operation, no response)

## Example

```erlang
1> cre_client:cre_reply(my_client, From, task_arg, result_value).
ok
```
```

#### `stop/1` - Stop Client Process

```erlang
%% @doc Stops the client process.

Terminates the client gen_server gracefully, cleaning up all pending
requests and state maps.

## Parameters

- `ClientName`: Name or pid of the client process

## Returns

`ok`

## Example

```erlang
1> cre_client:stop(my_client).
ok
```
```

#### `doctest_test/0` - Run Doctests

```erlang
%% @doc Runs doctests for the cre_client module.

This function verifies the client gen_server implementation including:
- Client state record initialization
- Request/reply map management
- State map transitions
- Timer spawning for polling

## Returns

`ok` if all tests pass

## Example

```erlang
1> cre_client:doctest_test().
ok
```
```

### Callback Interface

The `cre_client` module defines a callback interface that client modules must implement to customize workflow evaluation behavior.

#### `init/1` - Initialize Client

```erlang
-callback init(InitArg :: _) -> UsrInfo :: _.
```

Called when the client starts. Should return user-specific information
that will be passed to other callback functions.

#### `is_value/2` - Check for Terminal State

```erlang
-callback is_value(E :: _, UsrInfo :: _) -> boolean().
```

Called to check if the workflow expression has reached a terminal state.
Returns `true` if evaluation is complete, `false` otherwise.

#### `step/2` - Execute Evaluation Step

```erlang
-callback step(E :: _, UsrInfo :: _) -> {ok, _, [_]}.
```

Executes one step of workflow evaluation. Returns:
- `{ok, NewE, ALst}` where `NewE` is the updated expression and `ALst`
  contains arguments to send to workers.

#### `recv/3` - Receive Worker Replies

```erlang
-callback recv(E :: _, ReplyLst :: [{_, _}], UsrInfo :: _) -> _.
```

Processes replies from workers. Called with list of `{A, Delta}` tuples
where `A` is the task argument and `Delta` is the result.

#### `load/2` - Load Workflow Expression

```erlang
-callback load(_, UserInfo :: _) -> _.
```

Loads a workflow expression into the client state.

#### `unload/2` - Unload and Finalize

```erlang
-callback unload(_, UserInfo :: _) -> _.
```

Called when workflow completes. Should return final results and clean up
state.

### Client State Management

The client state is managed through the `#client_state{}` record:

```erlang
-record(client_state, {
          cre_name,           % Target CRE instance
          client_mod,         % Client callback module
          usr_info,           % User-specific info
          request_map = #{},  % Pending requests
          reply_map = #{},    % Received replies
          state_map = #{}     % Client state
         }).
```

#### State Transitions

1. **Initialization**: Client starts with `request_map`, `reply_map`, and `state_map` empty
2. **Request Submission**: `eval/2` adds request to `request_map` and starts evaluation loop
3. **Task Execution**: `step/2` generates tasks sent to CRE master
4. **Reply Handling**: `cre_reply/4` accumulates replies in `reply_map`
5. **Completion**: `is_value/2` detects completion, returns result via `unload/2`

#### Poll Interval

The client uses a configurable poll interval (default 250ms) for checking
replies, stored in persistent_term for O(1) access.

### Doctest Examples - cre_client

#### Client State Record Initialization

```erlang
%% Test 1: Client state record initialization
State = #client_state{
    cre_name = test_cre,
    client_mod = test_client_mod,
    usr_info = test_usr_info,
    request_map = #{},
    reply_map = #{},
    state_map = #{}
},
test_cre = State#client_state.cre_name,
test_client_mod = State#client_state.client_mod,
test_usr_info = State#client_state.usr_info,
#{} = State#client_state.request_map,
#{} = State#client_state.reply_map,
#{} = State#client_state.state_map.
```

#### Request Map Updates

```erlang
%% Test 2: Request map updates add new entries
From = {pid, ref},
P = test_program,
RequestMap1 = #{From => P},
State1 = State#client_state{request_map = RequestMap1},
#{From := P} = State1#client_state.request_map.
```

#### Reply Map Accumulation

```erlang
%% Test 3: Reply map accumulates replies
ReplyMap1 = #{From => [{task1, result1}]},
State2 = State1#client_state{reply_map = ReplyMap1},
[{task1, result1}] = maps:get(From, State2#client_state.reply_map).
```

#### State Map Tracking

```erlang
%% Test 4: State map tracks request state transitions
StateMap1 = #{From => idle},
State3 = State2#client_state{state_map = StateMap1},
idle = maps:get(From, State3#client_state.state_map).
```

#### State Transitions

```erlang
%% Test 5: State transitions from idle to primed on reply
StateMap2 = #{From => primed},
State4 = State3#client_state{state_map = StateMap2},
primed = maps:get(From, State4#client_state.state_map).

%% Test 6: Reply map prepends new replies
ReplyMap2 = #{From => [{task2, result2}, {task1, result1}]},
State5 = State4#client_state{reply_map = ReplyMap2},
[{task2, result2}, {task1, result1}] = maps:get(From, State5#client_state.reply_map).
```

## cre_yawl_client Module

### Public API Functions

#### `start_link/2` - Start Anonymous YAWL Client

```erlang
-spec start_link(CreName :: atom() | pid(), WorkflowExpr :: workflow_expr()) ->
          {ok, pid()} | {error, term()}.
```

Starts a YAWL client linked to the CRE master process. Automatically
initializes with the provided workflow expression or pattern.

#### `start_link/3` - Start Named YAWL Client

```erlang
-spec start_link(ClientName :: atom(), CreName :: atom() | pid(),
                 WorkflowExpr :: workflow_expr()) ->
          {ok, pid()} | {error, term()}.
```

Starts a named YAWL client registered under `ClientName`.

#### `execute_workflow/2` - Execute Complete Workflow

```erlang
-spec execute_workflow(ClientPid :: pid(), InitialData :: map()) ->
          {ok, workflow_result()} | {error, term()}.
```

Executes a complete workflow synchronously and returns final results.

#### `execute_pattern/3` - Execute Single Pattern

```erlang
-spec execute_pattern(ClientPid :: pid(),
                     Pattern :: term(),
                     InputData :: term()) ->
          {ok, workflow_result()} | {error, term()}.
```

Executes a single YAWL pattern with input data.

#### `compose_patterns/2` - Compose Multiple Patterns

```erlang
-spec compose_patterns(Patterns :: list(), Options :: map()) ->
          workflow_expr().
```

Composes multiple patterns into a single workflow expression.

#### `get_workflow_state/1` - Get Current Workflow State

```erlang
-spec get_workflow_state(ClientPid :: pid()) ->
          {ok, map()} | {error, term()}.
```

Returns the current execution state of the workflow.

#### `get_workflow_results/1` - Get Workflow Results

```erlang
-spec get_workflow_results(ClientPid :: pid()) ->
          {ok, workflow_result()} | {error, term()}.
```

Returns the accumulated results from workflow execution.

#### `terminate_workflow/1` - Terminate Running Workflow

```erlang
-spec terminate_workflow(ClientPid :: pid()) -> ok.
```

Gracefully terminates a running workflow.

### Callback Interface

The `cre_yawl_client` module implements the `cre_client` callback interface for YAWL workflows.

#### `init/1` - Initialize YAWL Client

```erlang
-spec init(WorkflowExpr :: workflow_expr()) -> usr_info().
```

Initializes the client with a workflow expression or pattern. Validates
YAWL workflows and sets up execution state.

#### `is_value/2` - Check for Workflow Completion

```erlang
-spec is_value(_E :: term(), UsrInfo :: usr_info()) -> boolean().
```

Returns `true` if workflow is complete (all tasks executed, no active tasks).

#### `step/2` - Execute Workflow Step

```erlang
-spec step(_E :: term(), UsrInfo :: usr_info()) ->
          {ok, usr_info(), [term()]}.
```

Determines next tasks to execute and returns them as CRE tasks.

#### `recv/3` - Process Worker Results

```erlang
-spec recv(_E :: term(), ReplyLst :: [{term(), term()}], UsrInfo :: usr_info()) ->
          usr_info().
```

Processes results from completed workers and updates workflow state.

#### `load/2` - Load Workflow Expression

```erlang
-spec load(E :: workflow_expr(), UsrInfo :: usr_info()) -> usr_info().
```

Loads a workflow expression or pattern into the client state.

#### `unload/2` - Finalize Workflow Execution

```erlang
-spec unload(_E :: term(), UsrInfo :: usr_info()) -> workflow_result().
```

Returns final workflow results including status, errors, and metadata.

### Pattern Support

The client supports all YAWL workflow patterns through the `cre_yawl` module:

#### Control Flow Patterns
- `sequence` - Sequential task execution
- `parallel_split` - Execute tasks in parallel
- `synchronization` - Wait for all tasks to complete
- `exclusive_choice` - Select one branch conditionally
- `simple_merge` - Merge from single source
- `multi_choice` - Select multiple branches
- `synchronizing_merge` - Wait for multiple sources
- `multi_merge` - Merge from multiple sources
- `discriminator` - Select based on condition
- `arbitration` - Select one of multiple options

#### Data Flow Patterns
- `param_pass` - Pass parameters between tasks
- `data_transform` - Transform data between tasks
- `data_distribute` - Distribute data to multiple tasks
- `data_accumulate` - Accumulate data from multiple tasks
- `data_visibility` - Control data visibility scope

#### Resource Patterns
- `resource_create` - Create new workflow resource
- `role_allocate` - Allocate based on role
- `resource_start` - Start resource execution
- `role_distribute` - Distribute based on roles
- `capability_allocate` - Allocate by capability

### Doctest Examples - cre_yawl_client

#### Pattern Record Validation

```erlang
%% Check if tuple is a valid pattern record type
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
is_pattern_record(_) -> false.
```

#### Pattern Type Extraction

```erlang
%% Get pattern type from pattern tuple
get_pattern_type(Pattern) when is_tuple(Pattern), tuple_size(Pattern) >= 1 ->
    element(1, Pattern);
get_pattern_type(_) ->
    undefined.
```

#### Sequence Pattern Execution

```erlang
%% Execute sequence pattern one step at a time
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
```

#### Parallel Split Execution

```erlang
%% Execute all branches in parallel
execute_parallel_split(#yawl_client_state{pattern = Pattern} = UsrInfo) ->
    Branches = get_pattern_field(branch_task_ids, Pattern, []),
    CreTasks = [{parallel_task, BranchId, self()} || BranchId <- Branches],
    {ok, UsrInfo, CreTasks}.
```

#### Exclusive Choice Execution

```erlang
%% Execute first branch with true condition
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
```

## Integration Patterns

### Client-Master Integration

#### Basic Connection Pattern

```erlang
%% Start CRE master
{ok, MasterPid} = cre_master:start_link(my_cre),

%% Start client connected to master
{ok, ClientPid} = cre_client:start_link(my_cre, my_workflow_mod, []),

%% Execute workflow
Result = cre_client:eval(ClientPid, workflow_expression),

%% Cleanup
cre_client:stop(ClientPid),
cre_master:stop(MasterPid).
```

#### Named Client Pattern

```erlang
%% Start named client for easier reference
{ok, ClientPid} = cre_client:start_link(my_client, my_cre, my_workflow_mod, []),

%% Execute workflow through named client
Result = cre_client:eval(my_client, workflow_expression),

%% Check status via name
Status = cre_client:get_status(my_client).
```

### Error Handling Integration

#### Workflow Validation Errors

```erlang
try
    {ok, ClientPid} = cre_yawl_client:start_link(my_cre, InvalidWorkflow),
    Result = cre_yawl_client:execute_workflow(ClientPid, #{})
catch
    error:{workflow_validation_failed, Errors} ->
        io:format("Validation failed: ~p~n", [Errors]),
        handle_validation_error(Errors);
    throw:{invalid_workflow_expression, Expr} ->
        io:format("Invalid expression: ~p~n", [Expr]),
        handle_expression_error(Expr)
end.
```

#### Worker Execution Errors

```erlang
%% In callback module
step(E, UserInfo) ->
    try
        {ok, NewE, TaskList} = do_workflow_step(E, UserInfo),
        {ok, NewE, TaskList}
    catch
        Error:Reason ->
            %% Mark workflow as failed
            UserInfo1 = UserInfo#yawl_client_state{
                          execution_state = failed,
                          errors = [{Error, Reason}]
                         },
            {ok, UserInfo1, []}
    end.
```

#### Timeout Handling

```erlang
%% Set custom timeout for long-running workflows
Result = cre_client:eval(ClientPid, workflow_expr, infinity).

%% Handle timeout gracefully
try
    Result = cre_client:eval(ClientPid, workflow_expr)
catch
    exit:timeout ->
        handle_timeout(),
        terminate_workflow(ClientPid)
end.
```

### Performance Integration

#### Batch Processing

```erlang
%% Process multiple workflows in parallel
process_workflows(Workflows) ->
    Clients = [begin
                 {ok, Pid} = cre_yawl_client:start_link(my_cre, W),
                 Pid
             end || W <- Workflows],

    Results = [cre_yawl_client:execute_workflow(Pid, #{}) || Pid <- Clients],

    %% Cleanup
    [cre_yawl_client:terminate_workflow(Pid) || Pid <- Clients],

    Results.
```

#### State Monitoring

```erlang
%% Monitor workflow progress
monitor_workflow_progress(ClientPid) ->
    {ok, State} = cre_yawl_client:get_workflow_state(ClientPid),
    #{execution_state := State,
      active_tasks := ActiveCount,
      completed_tasks := CompletedCount,
      pending_tasks := PendingCount} = State,

    io:format("Progress: ~p (~p active, ~p completed, ~p pending)~n",
              [State, ActiveCount, CompletedCount, PendingCount]).
```

#### Result Collection

```erlang
%% Collect and aggregate workflow results
collect_results(ClientPids) ->
    Results = [cre_yawl_client:get_workflow_results(Pid) || Pid <- ClientPids],

    Aggregated = lists:foldl(fun(Result, Acc) ->
                                   merge_results(Result, Acc)
                               end, #{}, Results),

    Aggregated.
```

## Best Practices

### Client Lifecycle Management

#### Proper Initialization

```erlang
%% Always validate workflow before starting client
validate_and_start(CreName, WorkflowExpr) ->
    case cre_yawl:validate(WorkflowExpr) of
        ok ->
            cre_yawl_client:start_link(CreName, WorkflowExpr);
        {error, Errors} ->
            {error, {validation_failed, Errors}}
    end.
```

#### Resource Cleanup

```erlang
%% Use supervisor or ensure_proper_cleanup
process_with_cleanup(Fun) ->
    {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow),
    try
        Fun(ClientPid)
    after
        cre_yawl_client:terminate_workflow(ClientPid)
    end.
```

#### Connection Management

```erlang
%% Monitor CRE master connection
monitor_master_connection(CreName) ->
    process_flag(trap_exit, true),
    receive
        {'DOWN', _, process, CreName, _} ->
            %% Handle master failure
            handle_master_failure()
    end.
```

### Error Recovery Strategies

#### Retry Mechanism

```erlang
%% Implement retry for transient failures
execute_with_retry(ClientPid, Expr, MaxRetries) ->
    execute_with_retry(ClientPid, Expr, MaxRetries, 0).

execute_with_retry(_ClientPid, _Expr, MaxRetries, Retries) when Retries >= MaxRetries ->
    {error, max_retries_exceeded};

execute_with_retry(ClientPid, Expr, MaxRetries, Retries) ->
    try
        cre_yawl_client:execute_workflow(ClientPid, Expr)
    catch
        throw:transient_error ->
            %% Wait before retry
            timer:sleep(1000 * (Retries + 1)),
            execute_with_retry(ClientPid, Expr, MaxRetries, Retries + 1)
    end.
```

#### Fallback Patterns

```erlang
%% Implement fallback workflow
execute_with_fallback(PrimaryExpr, FallbackExpr) ->
    {ok, ClientPid} = cre_yawl_client:start_link(my_cre, PrimaryExpr),
    try
        {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, PrimaryExpr),
        Result
    catch
        Error:Reason ->
            %% Switch to fallback
            io:format("Primary failed: ~p, using fallback~n", [{Error, Reason}]),
            {ok, FallbackPid} = cre_yawl_client:start_link(my_cre, FallbackExpr),
            cre_yawl_client:execute_workflow(FallbackPid, FallbackExpr)
    end.
```

### Performance Optimization

#### Connection Pooling

```erlang
%% Maintain pool of reusable clients
start_client_pool(Size) ->
    [begin
         {ok, Pid} = cre_yawl_client:start_link(my_cre, idle_workflow),
         Pid
     end || _ <- lists:seq(1, Size)].

get_client_from_pool(Pool) ->
    case Pool of
        [Pid | Rest] ->
            {Pid, Rest};
        [] ->
            {new_client(), []}
    end.
```

#### Batch Processing Optimization

```erlang
%% Process similar workflows together
process_similar_workflows(Workflows) ->
    Grouped = group_by_similarity(Workflows),

    Results = lists:map(fun({Pattern, Instances}) ->
                             process_pattern_instances(Pattern, Instances)
                         end, Grouped),

    lists:flatten(Results).
```

#### State Caching

```erlang
%% Cache workflow templates for reuse
workflow_cache() ->
    case get(workflow_cache) of
        undefined ->
            Cache = initialize_workflow_cache(),
            put(workflow_cache, Cache),
            Cache;
        Cache ->
            Cache
    end.
```

## Complete Examples

### Simple Workflow

```erlang
%% Define a simple sequence workflow
simple_workflow() ->
    %% Create client
    {ok, ClientPid} = cre_yawl_client:start_link(my_cre,
        cre_yawl:sequence([
            {task1, <<"First task">>, []},
            {task2, <<"Second task">>, []},
            {task3, <<"Third task">>, []}
        ])),

    %% Execute workflow
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Display results
    io:format("Workflow completed: ~p~n", [Result]),

    %% Cleanup
    cre_yawl_client:terminate_workflow(ClientPid).
```

### Complex Multi-Pattern Workflow

```erlang
%% Create a complex workflow with multiple patterns
complex_workflow() ->
    %% Define workflow with parallel processing and conditional logic
    Workflow = cre_yawl:compose_patterns([
        %% First, process data in parallel
        cre_yawl:parallel_split([
            {data_task1, <<"Data processing 1">>, []},
            {data_task2, <<"Data processing 2">>, []}
        ]),

        %% Then, conditionally process results
        cre_yawl:exclusive_choice([
            {condition_task1, fun() -> check_condition() end},
            {condition_task2, fun() -> true end}
        ]),

        %% Finally, synchronize and collect results
        cre_yawl:synchronization()
    ], #{mode => sequence}),

    %% Execute workflow
    {ok, ClientPid} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Monitor progress during execution
    monitor_workflow_progress(ClientPid),

    %% Cleanup
    cre_yawl_client:terminate_workflow(ClientPid).
```

### Human-in-the-Loop Workflow

```erlang
%% Define a workflow requiring human approval
human_approval_workflow() ->
    %% Define workflow with approval step
    Workflow = cre_yawl:sequence([
        {auto_task1, <<"Automatic processing">>, []},
        {approval_task, <<"Human approval required">>, []},
        {auto_task2, <<"Final processing">>, []}
    ]),

    %% Start workflow
    {ok, ClientPid} = cre_yawl_client:start_link(my_cre, Workflow),

    %% Monitor for approval task
    monitor_for_approval_task(ClientPid),

    %% Simulate human approval
    simulate_human_approval(ClientPid),

    %% Complete workflow
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Cleanup
    cre_yawl_client:terminate_workflow(ClientPid).
```

### Custom Client Implementation

```erlang
%% Implement custom client with specialized workflow logic
my_custom_client() ->

    %% Define callback module
    -module(my_client).
    -behaviour(cre_client).

    %% Callback implementations
    init(_Arg) ->
        #{user => "test_user", session => start_session()}.

    is_value(E, _UserInfo) ->
        %% Custom completion logic
        is_workflow_complete(E).

    step(E, UserInfo) ->
        %% Custom workflow step logic
        {ok, NewE, Tasks} = process_workflow_step(E, UserInfo),
        {ok, NewE, Tasks}.

    recv(E, Replies, UserInfo) ->
        %% Process worker replies
        process_worker_replies(E, Replies, UserInfo).

    load(E, _UserInfo) ->
        %% Load workflow
        prepare_workflow(E).

    unload(_E, UserInfo) ->
        %% Finalize and return results
        finalize_workflow(UserInfo).

    %% Client usage
    start_client() ->
        {ok, ClientPid} = cre_client:start_link(my_cre, my_client, []),
        Result = cre_client:eval(ClientPid, workflow_expr),
        cre_client:stop(ClientPid),
        Result.
```

### Error Handling Example

```erlang
%% Comprehensive error handling example
robust_workflow_execution() ->
    try
        %% Validate workflow
        case cre_yawl:validate(Workflow) of
            ok ->
                start_and_execute_workflow(Workflow);
            {error, Errors} ->
                handle_validation_errors(Errors)
        end
    catch
        Error:Reason ->
            io:format("Error: ~p~n", [{Error, Reason}]),
            %% Log error and attempt recovery
            log_error(Error, Reason),
            attempt_recovery(Error, Reason)
    end.
```

This comprehensive reference provides everything needed to effectively use the CRE client API modules, from basic usage to advanced integration patterns and best practices.