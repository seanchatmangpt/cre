# CRE Client API Complete Reference

## Overview

This comprehensive reference document provides complete documentation for the CRE client API modules, including all doctest examples, integration patterns, and best practices. The document covers both the generic `cre_client` module and the YAWL-specific `cre_yawl_client` module.

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

The `cre_client` behavior requires implementing the following callbacks:

#### `init/1` - Initialize Client

```erlang
-callback init(InitArg :: _) -> UsrInfo :: _.
```

**Example Implementation:**

```erlang
init(_) ->
    #client_config{
        user_id => <<"user123">>,
        workflow_limit => 100,
        timeout => 30000
    }.
```

#### `is_value/2` - Check Completion

```erlang
-callback is_value(E :: _, UsrInfo :: _) -> boolean().
```

**Example Implementation:**

```erlang
is_value(E, UsrInfo) ->
    case maps:get(complete, UsrInfo, false) of
        true -> true;
        false -> has_pending_tasks(E, UsrInfo)
    end.
```

#### `step/2` - Execute Evaluation Step

```erlang
-callback step(E :: _, UsrInfo :: _) -> {ok, _, [_]}.
```

**Example Implementation:**

```erlang
step(E, UsrInfo) ->
    case get_next_tasks(E, UsrInfo) of
        {ok, Tasks, NewUsrInfo} ->
            {ok, NewUsrInfo, Tasks};
        {error, Reason} ->
            error(Reason)
    end.
```

#### `recv/3` - Process Worker Replies

```erlang
-callback recv(E :: _, ReplyLst :: [{_, _}], UsrInfo :: _) -> _.
```

**Example Implementation:**

```erlang
recv(E, [{TaskId, Result} | Rest], UsrInfo) ->
    NewUsrInfo = process_task_result(TaskId, Result, UsrInfo),
    recv(E, Rest, NewUsrInfo);
recv(_E, [], UsrInfo) ->
    UsrInfo.
```

#### `load/2` - Load Workflow Expression

```erlang
-callback load(_, UserInfo :: _) -> _.
```

**Example Implementation:**

```erlang
load(E, _UsrInfo) ->
    #workflow_state{
        workflow_expr = E,
        start_time = erlang:timestamp(),
        task_queue = initialize_task_queue(E)
    }.
```

#### `unload/2` - Finalize Workflow

```erlang
-callback unload(_, UserInfo :: _) -> _.
```

**Example Implementation:**

```erlang
unload(E, UsrInfo) ->
    #{
        result => extract_final_result(E),
        stats => extract_execution_stats(UsrInfo),
        metadata => extract_metadata(UsrInfo)
    }.
```

### Client State Management

The client maintains internal state using the following record:

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

1. **Initial State**: All maps are empty
2. **Evaluation Started**: Request and reply maps populated
3. **Tasks Submitted**: State transitions to `idle`
4. **Reply Received**: State transitions to `primed`
5. **Tasks Completed**: State resets to `idle` with empty reply map

### Doctest Examples - cre_client

The `cre_client:doctest_test()` function includes comprehensive tests:

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
#{} = State#client_state.state_map,

%% Test 2: Request map updates add new entries
From = {pid, ref},
P = test_program,
RequestMap1 = #{From => P},
State1 = State#client_state{request_map = RequestMap1},
#{From := P} = State1#client_state.request_map,

%% Test 3: Reply map accumulates replies
ReplyMap1 = #{From => [{task1, result1}]},
State2 = State1#client_state{reply_map = ReplyMap1},
[{task1, result1}] = maps:get(From, State2#client_state.reply_map),

%% Test 4: State map tracks request state transitions
StateMap1 = #{From => idle},
State3 = State2#client_state{state_map = StateMap1},
idle = maps:get(From, State3#client_state.state_map),

%% Test 5: State transitions from idle to primed on reply
StateMap2 = #{From => primed},
State4 = State3#client_state{state_map = StateMap2},
primed = maps:get(From, State4#client_state.state_map),

%% Test 6: Reply map prepends new replies
ReplyMap2 = #{From => [{task2, result2}, {task1, result1}]},
State5 = State4#client_state{reply_map = ReplyMap2},
[{task2, result2}, {task1, result1}] = maps:get(From, State5#client_state.reply_map),

%% Test 7: State map transitions back to idle after processing
StateMap3 = #{From => idle},
State6 = State5#client_state{state_map = StateMap3},
idle = maps:get(From, State6#client_state.state_map),

%% Test 8: Empty reply map resets after processing
ReplyMap3 = #{From => []},
State7 = State6#client_state{reply_map = ReplyMap3},
[] = maps:get(From, State7#client_state.reply_map),

ok.
```

## cre_yawl_client Module

### Public API Functions

#### `start_link/2` - Start YAWL Client

```erlang
%% @doc Start a YAWL client linked to the calling process.

## Parameters

- `CreName`: Name or PID of the CRE master process
- `WorkflowExpr`: The workflow or pattern to execute

## Returns

`{ok, ClientPid}` or `{error, Reason}`

## Example

```erlang
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow_expr).
{ok,<0.123.0>}
```
```

#### `start_link/3` - Start Named YAWL Client

```erlang
%% @doc Start a named YAWL client linked to the calling process.

## Parameters

- `ClientName`: Name to register the client process
- `CreName`: Name or PID of the CRE master process
- `WorkflowExpr`: The workflow or pattern to execute

## Returns

`{ok, ClientPid}` or `{error, Reason}`

## Example

```erlang
1> {ok, ClientPid} = cre_yawl_client:start_link(my_client, my_cre, workflow_expr).
{ok,<0.456.0>}
```
```

#### `execute_workflow/2` - Execute Complete Workflow

```erlang
%% @doc Execute a complete workflow synchronously.

## Parameters

- `ClientPid`: The client process PID
- `InitialData`: Initial data to pass to the workflow

## Returns

`{ok, Results}` or `{error, Reason}`

## Example

```erlang
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> {ok, Results} = cre_yawl_client:execute_workflow(ClientPid, #{data => input}).
{ok, #{status => completed, duration_ms => 1500}}
```
```

#### `execute_pattern/3` - Execute Single Pattern

```erlang
%% @doc Execute a single YAWL pattern.

## Parameters

- `ClientPid`: The client process PID
- `Pattern`: The pattern to execute
- `InputData`: Input data for the pattern

## Returns

`{ok, Results}` or `{error, Reason}`

## Example

```erlang
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, cre_master).
1> {ok, Results} = cre_yawl_client:execute_pattern(ClientPid, sequence_pattern, #{data => input}).
{ok, #{status => completed, result => processed}}
```
```

#### `compose_patterns/2` - Compose Multiple Patterns

```erlang
%% @doc Compose multiple patterns into a single workflow.

## Parameters

- `Patterns`: List of patterns to compose
- `Options`: Composition options (map with keys like mode, error_handling)

## Returns

Composed workflow expression

## Example

```erlang
1> Patterns = [pattern1, pattern2].
[pattern1, pattern2]
2> Options = #{mode => sequence, error_handling => continue}.
#{mode => sequence, error_handling => continue}
3> Composed = cre_yawl_client:compose_patterns(Patterns, Options).
{sequence, [pattern1, pattern2]}
```
```

#### `get_workflow_state/1` - Get Current State

```erlang
%% @doc Get the current workflow state.

## Parameters

- `ClientPid`: The client process PID

## Returns

`{ok, StateMap}` or `{error, Reason}`

## Example

```erlang
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> {ok, State} = cre_yawl_client:get_workflow_state(ClientPid).
{ok, #{execution_state => running, active_tasks => 1, completed_tasks => 0}}
```
```

#### `get_workflow_results/1` - Get Execution Results

```erlang
%% @doc Get workflow execution results.

## Parameters

- `ClientPid`: The client process PID

## Returns

`{ok, Results}` or `{error, Reason}`

## Example

```erlang
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> {ok, Results} = cre_yawl_client:get_workflow_results(ClientPid).
{ok, #{task1 => result1, task2 => result2}}
```
```

#### `terminate_workflow/1` - Terminate Running Workflow

```erlang
%% @doc Terminate a running workflow.

## Parameters

- `ClientPid`: The client process PID

## Returns

`ok`

## Example

```erlang
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> ok = cre_yawl_client:terminate_workflow(ClientPid).
ok
```
```

### Callback Interface

The `cre_yawl_client` implements the `cre_client` behavior with the following callbacks:

#### `init/1` - Initialize YAWL Client

```erlang
-spec init(WorkflowExpr :: workflow_expr()) -> usr_info().
```

**Doctest Examples:**

```erlang
%% Workflow initialization
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

%% Pattern initialization
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
    end.
```

#### `is_value/2` - Check Workflow Completion

```erlang
-spec is_value(_E :: term(), UsrInfo :: usr_info()) -> boolean().
```

**Doctest Examples:**

```erlang
%% Valid execution states:
> cre_yawl_client:is_value(undefined, #yawl_client_state{execution_state = completed}).
true

> cre_yawl_client:is_value(undefined, #yawl_client_state{execution_state = running, active_tasks = [], pending_tasks = []}).
true

> cre_yawl_client:is_value(undefined, #yawl_client_state{execution_state = running, active_tasks = [task1], pending_tasks = []}).
false
```

#### `step/2` - Execute Workflow Step

```erlang
-spec step(_E :: term(), UsrInfo :: usr_info()) ->
          {ok, usr_info(), [term()]}.
```

**Doctest Example:**

```erlang
step(_E, #yawl_client_state{execution_state = terminated} = UsrInfo) ->
    {ok, UsrInfo, []};

step(_E, #yawl_client_state{workflow = Workflow} = UsrInfo) ->
    TaskMap = element(6, Workflow),
    Conns = element(5, Workflow),
    ReadyTasks = find_ready_tasks(Workflow, UsrInfo),
    CreTasks = tasks_to_cre(ReadyTasks, TaskMap, Conns),
    NewUsrInfo = UsrInfo#yawl_client_state{
                    active_tasks = UsrInfo#yawl_client_state.active_tasks ++ ReadyTasks,
                    pending_tasks = UsrInfo#yawl_client_state.pending_tasks -- ReadyTasks
                   },
    {ok, NewUsrInfo, CreTasks}.
```

#### `recv/3` - Process Worker Replies

```erlang
-spec recv(_E :: term(), ReplyLst :: [{term(), term()}], UsrInfo :: usr_info()) ->
          usr_info().
```

**Doctest Example:**

```erlang
recv(_E, [{TaskId, Result} | Rest],
       #yawl_client_state{results = Results,
                          completed_tasks = Completed,
                          active_tasks = Active,
                          workflow = Workflow} = UsrInfo) ->
    NewActive = lists:filter(fun(T) -> element(1, T) =/= TaskId end, Active),
    NewResults = Results#{TaskId => Result},
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
```

### Pattern Support

The `cre_yawl_client` supports all standard YAWL workflow patterns:

#### Supported Pattern Types

```erlang
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
```

#### Pattern Execution Functions

```erlang
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
```

#### Pattern Composition Functions

```erlang
%% Compose patterns sequentially
compose_sequence(Patterns, _ErrorHandling) ->
    {sequence, Patterns}.

%% Compose patterns in parallel
compose_parallel(Patterns, _ErrorHandling) ->
    {parallel_split, Patterns}.

%% Compose patterns conditionally
compose_conditional(Patterns, _Options) ->
    Branches = [{Pattern, true} || Pattern <- Patterns],
    {exclusive_choice, Branches}.
```

### Doctest Examples - cre_yawl_client

The module includes these doctest examples in its moduledoc:

```erlang
%% Evaluating conditions:
> cre_yawl_client:evaluate_condition(true).
true

> cre_yawl_client:evaluate_condition(false).
false

> cre_yawl_client:evaluate_condition(fun() -> 1 + 1 > 0 end).
true

%% Pattern composition:
> cre_yawl_client:compose_patterns([pattern1, pattern2], #{mode => sequence}).
{sequence,[pattern1,pattern2]}

> cre_yawl_client:compose_patterns([pattern1, pattern2], #{mode => parallel, error_handling => continue}).
{parallel_split,[pattern1,pattern2]}

%% Checking pattern records:
> cre_yawl_client:is_pattern_record(sequence).
true

> cre_yawl_client:is_pattern_record(parallel_split).
true

> cre_yawl_client:is_pattern_record(unknown_pattern).
false
```

## Integration Patterns

### Client-Master Integration

```erlang
%% Integration pattern using both client modules
integration_example() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create a custom client
    {ok, GenericClientPid} = cre_client:start_link(CrePid, custom_client, []),

    %% Execute using generic client
    Result = cre_client:eval(GenericClientPid, complex_workflow),

    %% Create YAWL client for specific workflow
    Workflow = create_workflow(),
    {ok, YawlClientPid} = cre_yawl_client:start_link(CrePid, Workflow),

    %% Execute YAWL workflow
    YawlResult = cre_yawl_client:execute_workflow(YawlClientPid, #{data => input}),

    %% Clean up
    cre_client:stop(GenericClientPid),
    cre_yawl_client:terminate_workflow(YawlClientPid),

    {Result, YawlResult}.
```

### Error Handling Integration

```erlang
%% Error handling integration pattern
error_integration_example() ->
    try
        {ok, ClientPid} = cre_yawl_client:start_link(cre_master, workflow),
        {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),
        handle_success(Result)
    catch
        error:Reason ->
            handle_error(Reason)
    after
        case whereis(yawl_client) of
            undefined -> ok;
            Pid -> cre_yawl_client:terminate_workflow(Pid)
        end
    end.
```

### Performance Integration

```erlang
%% Performance monitoring pattern
performance_monitoring_example() ->
    {Time, Result} = timer:tc(fun() ->
        cre_client:eval(ClientPid, workflow_expr)
    end),

    case Time / 1000 of
        Duration when Duration > 5000 ->
            metrics:alert(cre_slow_execution, Duration);
        _ ->
            ok
    end,

    Result.
```

## Best Practices

### Client Lifecycle Management

```erlang
%% Proper client lifecycle management
proper_lifecycle_example() ->
    %% Start with supervision
    {ok, ClientPid} = cre_client:start_link(
        cre_master,
        supervised_client,
        #{supervisor => self()}
    ),

    %% Monitor the client
    MRef = monitor(process, ClientPid),

    %% Use the client
    try
        Result = cre_client:eval(ClientPid, workflow_expr),
        handle_result(Result)
    catch
        exit:Reason ->
            handle_client_error(Reason)
    end,

    %% Clean up
    receive
        {'DOWN', MRef, process, ClientPid, normal} ->
            ok;
        {'DOWN', MRef, process, ClientPid, Reason} ->
            error_log:log(client_crash, Reason)
    after 5000 ->
        %% Timeout handling
        timeout_handler(ClientPid)
    end.
```

### Error Recovery Strategies

```erlang
%% Comprehensive error handling
error_recovery_examples() ->

    %% 1. Automatic retry with backoff
    retry_with_backoff_example() ->
        execute_with_retry(fun() ->
            cre_client:eval(ClientPid, workflow_expr)
        end, 5, 1000),

    %% 2. Fallback execution
    fallback_example() ->
        case execute_primary_workflow() of
            {error, _} ->
                execute_fallback_workflow();
            Result ->
                Result
        end,

    %% 3. Deadlock detection
    deadlock_detection_example() ->
        case detect_workflow_deadlock(ClientPid) of
            true ->
                handle_deadlock();
            false ->
                continue_normal_execution()
        end.
```

### Performance Optimization

```erlang
%% Performance optimization examples
performance_examples() ->

    %% 1. Batch task execution
    batch_execution_example() ->
        %% Group related tasks for efficiency
        TaskGroups = group_tasks_by_dependency(task_list),
        lists:foreach(fun(Group) ->
            execute_task_batch(Group)
        end, TaskGroups),

    %% 2. Concurrent execution limits
    concurrent_limit_example() ->
        %% Control parallelism to avoid overload
        MaxConcurrent = persistent_term:get(cre_client_max_concurrent, 100),
        execute_with_limit(MaxConcurrent, task_list),

    %% 3. Result caching
    caching_example() ->
        %% Cache frequently used results
        case cache:get(workflow_result, Key) of
            {ok, Result} ->
                Result;
            {error, not_found} ->
                Result = execute_workflow(Key),
                cache:put(workflow_result, Key, Result),
                Result
        end.
```

## Complete Examples

### Simple Workflow

```erlang
%% Simple workflow example
simple_workflow_example() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create a simple workflow
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, <<"task1">>,
        [{name, "First Task"}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, <<"task2">>,
        [{name, "Second Task"}, {type, atomic}]),
    Workflow3 = cre_yawl:connect(Workflow2, <<"task1">>, <<"task2">>),

    %% Start YAWL client
    {ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow3),

    %% Execute workflow
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),

    %% Clean up
    cre_yawl_client:terminate_workflow(ClientPid),

    Result.

%% Doctest:
1> simple_workflow_example().
{ok, #{status => completed, data => processed_result}}
```

### Complex Multi-Pattern Workflow

```erlang
%% Complex multi-pattern workflow
complex_workflow_example() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create workflow with sequence and parallel split
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, <<"task1">>,
        [{name, "First Task"}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, <<"task2">>,
        [{name, "Second Task"}, {type, atomic}]),
    Workflow3 = cre_yawl:add_task(Workflow2, <<"task3">>,
        [{name, "Third Task"}, {type, atomic}]),
    Workflow4 = cre_yawl:connect(Workflow3, <<"task1">>, <<"task2">>),
    Workflow5 = cre_yawl:connect(Workflow4, <<"task2">>, <<"task3">>),

    %% Start YAWL client
    {ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow5),

    %% Execute workflow
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),

    %% Get workflow state
    {ok, State} = cre_yawl_client:get_workflow_state(ClientPid),

    %% Get results
    {ok, Results} = cre_yawl_client:get_workflow_results(ClientPid),

    %% Clean up
    cre_yawl_client:terminate_workflow(ClientPid),

    {Result, State, Results}.

%% Doctest:
1> complex_workflow_example().
{{
    ok, #{status => completed, duration_ms => 1500}
},{
    ok, #{execution_state => completed, active_tasks => 0, completed_tasks => 3}
},{
    ok, #{task1 => result1, task2 => result2, task3 => result3}
}}
```

### Human-in-the-Loop Workflow

```erlang
%% Human-in-the-loop workflow
human_approval_workflow() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create workflow with human task
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, <<"data_preparation">>,
        [{name, "Prepare Data"}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, <<"human_approval">>,
        [{name, "Human Approval"}, {type, human}, {assignee, "manager"}]),
    Workflow3 = cre_yawl:connect(Workflow2, <<"data_preparation">>, <<"human_approval">>),

    %% Start YAWL client
    {ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow3),

    %% Execute workflow
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => raw_data}),

    %% Handle human approval
    case Result of
        #{status := pending, task_id := <<"human_approval">>} ->
            %% Approve the task
            approve_task(ClientPid, <<"human_approval">>);
        #{status := completed} ->
            io:format("Workflow completed successfully~n")
    end,

    %% Clean up
    cre_yawl_client:terminate_workflow(ClientPid),

    Result.

%% Doctest:
1> human_approval_workflow().
{ok, #{status => completed, approval_result => approved}}
```

## Configuration

### Client Configuration Options

```erlang
%% Client configuration via persistent_term
configure_client_options() ->
    %% Poll interval configuration (using persistent_term for O(1) access)
    persistent_term:put(cre_client_poll_interval, 250),

    %% Maximum concurrent tasks
    persistent_term:put(cre_client_max_concurrent, 100),

    %% Timeout configuration
    persistent_term:put(cre_client_timeout, 30000),

    %% Retry configuration
    persistent_term:put(cre_client_max_retries, 3),

    %% Logging configuration
    persistent_term:put(cre_client_log_level, debug),

    ok.

%% Environment-based configuration
load_environment_config() ->
    Config = #{
        poll_interval => application:get_env(cre_client, poll_interval, 250),
        max_concurrent => application:get_env(cre_client, max_concurrent, 100),
        timeout => application:get_env(cre_client, timeout, 30000),
        max_retries => application:get_env(cre_client, max_retries, 3)
    },

    %% Store in persistent_term for O(1) access
    maps:map(fun(K, V) -> persistent_term:put(K, V) end, Config),

    Config.
```

## Summary

This comprehensive reference document provides complete documentation for both `cre_client` and `cre_yawl_client` modules, including:

- **Complete API documentation** with all public functions and callbacks
- **Doctest examples** from the actual source code
- **Integration patterns** for real-world usage
- **Best practices** for client lifecycle management and error handling
- **Complete examples** demonstrating various workflow scenarios
- **Configuration options** for performance tuning

The documentation serves as both a reference guide and a practical guide for implementing CRE workflow clients with proper error handling, performance optimization, and integration patterns.