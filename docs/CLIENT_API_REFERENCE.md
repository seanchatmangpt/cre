# CRE Client API Reference

## Overview

This reference provides comprehensive documentation for the CRE client API modules, including all doctest examples from the source code.

## Module: cre_client

### Public API Functions

#### `start_link/3`

```erlang
%% @doc Starts an anonymous CRE client gen_server.

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

#### `start_link/4`

```erlang
%% @doc Starts a named CRE client gen_server.

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

#### `eval/2`

```erlang
%% @doc Evaluates a workflow expression.

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

#### `cre_reply/4`

```erlang
%% @doc Handles a reply from the CRE master.

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

#### `stop/1`

```erlang
%% @doc Stops the client process.

## Parameters
- `ClientName`: Name or pid of the client process

## Returns
`ok`

## Example
```erlang
1> cre_client:stop(my_client).
ok
```

#### `doctest_test/0`

```erlang
%% @doc Runs doctests for the cre_client module.

## Returns
`ok` if all tests pass

## Example
```erlang
1> cre_client:doctest_test().
ok
```

### Client State Record

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

### Callback Functions

#### `init/1`

```erlang
-callback init(InitArg :: _) -> UsrInfo :: _.
```

**Doctest Example:**
```erlang
init(_) ->
    #client_config{
        user_id => <<"user123">>,
        workflow_limit => 100,
        timeout => 30000
    }.
```

#### `is_value/2`

```erlang
-callback is_value(E :: _, UsrInfo :: _) -> boolean().
```

**Doctest Example:**
```erlang
is_value(_E, #client_state{complete = true}) -> true;
is_value(_E, _UsrInfo) -> false.
```

#### `step/2`

```erlang
-callback step(E :: _, UsrInfo :: _) -> {ok, _, [_]}.
```

**Doctest Example:**
```erlang
step(E, UsrInfo) ->
    case get_next_tasks(E, UsrInfo) of
        {ok, Tasks, NewUsrInfo} ->
            {ok, NewUsrInfo, Tasks};
        {error, Reason} ->
            error(Reason)
    end.
```

#### `recv/3`

```erlang
-callback recv(E :: _, ReplyLst :: [{_, _}], UsrInfo :: _) -> _.
```

**Doctest Example:**
```erlang
recv(E, [{TaskId, Result} | Rest], UsrInfo) ->
    NewUsrInfo = process_task_result(TaskId, Result, UsrInfo),
    recv(E, Rest, NewUsrInfo);
recv(_E, [], UsrInfo) ->
    UsrInfo.
```

#### `load/2`

```erlang
-callback load(_, UserInfo :: _) -> _.
```

**Doctest Example:**
```erlang
load(E, _UsrInfo) ->
    #workflow_state{
        workflow_expr = E,
        start_time = erlang:timestamp(),
        task_queue = initialize_task_queue(E)
    }.
```

#### `unload/2`

```erlang
-callback unload(_, UserInfo :: _) -> _.
```

**Doctest Example:**
```erlang
unload(_E, UsrInfo) ->
    #{
        result => extract_final_result(UsrInfo),
        stats => extract_execution_stats(UsrInfo),
        metadata => extract_metadata(UsrInfo)
    }.
```

### Complete Doctest Example

```erlang
%% Run doctests for the cre_client module
1> cre_client:doctest_test().
ok

%% The doctest includes:
%% - Client state record initialization
%% - Request map updates
%% - Reply map accumulation
%% - State map transitions
%% - Reply prepending
%% - State transition from idle to primed
%% - Empty reply map reset

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

%% Test 2: Request map updates
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

## Module: cre_yawl_client

### Public API Functions

#### `start_link/2`

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

#### `start_link/3`

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

#### `execute_workflow/2`

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

#### `execute_pattern/3`

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

#### `compose_patterns/2`

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

#### `get_workflow_state/1`

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

#### `get_workflow_results/1`

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

#### `terminate_workflow/1`

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

### Client State Record

```erlang
-record(yawl_client_state, {
          workflow :: undefined | term(),       % Workflow record
          pattern :: undefined | term(),        % Pattern record
          execution_state :: running | completed | failed | terminated,
          active_tasks = [] :: list(),          % Currently executing tasks
          completed_tasks = [] :: list(),       % Completed task IDs
          pending_tasks = [] :: list(),         % Tasks waiting to execute
          results = #{} :: map(),               % Task results
          errors = [] :: list(),               % Error list
          start_time :: undefined | erlang:timestamp(),
          end_time :: undefined | erlang:timestamp(),
          metadata = #{} :: map()               % Additional metadata
         }).
```

### Type Definitions

```erlang
-type workflow_expr() :: term().
-type task_spec() :: {binary(), module(), atom(), list()}.
-type execution_state() :: running | completed | failed | terminated.
-type workflow_result() :: #{atom() => term()}.
-type client_state() :: #yawl_client_state{}.
-type usr_info() :: client_state().

-export_type([client_state/0, execution_state/0, workflow_result/0]).
```

### Callback Functions

#### `init/1`

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

#### `is_value/2`

```erlang
-spec is_value(_E :: term(), UsrInfo :: usr_info()) -> boolean().
```

**Doctest Example:**
```erlang
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
```

#### `step/2`

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

#### `recv/3`

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

#### `load/2`

```erlang
-spec load(E :: workflow_expr(), UsrInfo :: usr_info()) -> usr_info().
```

**Doctest Example:**
```erlang
load(WorkflowExpr, _UsrInfo) when element(1, WorkflowExpr) =:= workflow ->
    init(WorkflowExpr);

load(Pattern, _UsrInfo) when is_tuple(Pattern), tuple_size(Pattern) >= 1 ->
    PatternType = element(1, Pattern),
    case is_pattern_record(PatternType) of
        true -> init(Pattern);
        false -> error({invalid_workflow_expression, Pattern})
    end;

load(ComposedPatterns, _UsrInfo) when is_list(ComposedPatterns) ->
    #yawl_client_state{
      workflow = undefined,
      pattern = {sequence, ComposedPatterns},
      execution_state = running,
      start_time = erlang:timestamp(),
      metadata = #{composed => true}
     }.
```

#### `unload/2`

```erlang
-spec unload(_E :: term(), UsrInfo :: usr_info()) -> workflow_result().
```

**Doctest Example:**
```erlang
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
```

### Pattern Functions

#### Pattern Type Recognition

```erlang
%% Recognize all supported pattern types
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
%% Execute pattern based on type
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

### Composition Functions

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

### Complete Workflow Example

```erlang
%% Create and execute a YAWL workflow
create_and_execute_workflow() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create a workflow with sequence and parallel split
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
1> create_and_execute_workflow().
{{
    ok, #{status => completed, duration_ms => 1500}
},{
    ok, #{execution_state => completed, active_tasks => 0, completed_tasks => 3}
},{
    ok, #{task1 => result1, task2 => result2, task3 => result3}
}}.
```

## Integration Patterns

### Client Integration with CRE Master

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

### Error Integration Pattern

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

## Configuration and Environment

### Client Configuration

```erlang
%% Configuration via persistent_term
configure_client() ->
    %% Poll interval configuration (O(1) access)
    persistent_term:put(cre_client_poll_interval, 250),

    %% Maximum concurrent tasks
    persistent_term:put(cre_client_max_concurrent, 100),

    %% Timeout configuration
    persistent_term:put(cre_client_timeout, 30000),

    %% Retry configuration
    persistent_term:put(cre_client_max_retries, 3),

    ok.
```

### Environment Variables

```erlang
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

## Performance Considerations

### Client Performance

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

### Batch Processing Pattern

```erlang
%% Batch processing for better performance
batch_processing_example() ->
    Workflows = [workflow1, workflow2, workflow3, workflow4, workflow5],

    %% Process in batches to avoid overload
    Batches = lists:chunk(Workflows, 3),

    lists:foreach(fun(Batch) ->
        lists:foreach(fun(Workflow) ->
            process_workflow_batch(Workflow)
        end, Batch),
        %% Small delay between batches
        timer:sleep(100)
    end, Batches).
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

### Resource Management

```erlang
%% Resource management pattern
resource_management_example() ->
    %% Track client resources
    ResourceTracker = #{
        active_clients => sets:new(),
        max_concurrent => 100,
        total_resources => 1000
    },

    %% Add client with resource check
    add_client(ClientPid, ResourceTracker) ->
        case maps:get(active_clients, ResourceTracker) < maps:get(max_concurrent, ResourceTracker) of
            true ->
                NewTracker = ResourceTracker#{
                    active_clients => sets:add_element(ClientPid, ResourceTracker#active_clients)
                },
                {ok, NewTracker};
            false ->
                {error, resource_limit_exceeded}
        end.
```

This comprehensive reference includes all doctest examples from both client modules, showing proper usage patterns, integration examples, and best practices for working with the CRE client APIs.