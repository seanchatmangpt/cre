# CRE Client API Tutorial

## Table of Contents

1. [Getting Started](#getting-started)
2. [Basic Usage](#basic-usage)
3. [Workflow Patterns](#workflow-patterns)
4. [Advanced Topics](#advanced-toptopics)
5. [Integration Examples](#integration-examples)
6. [Best Practices](#best-practices)

## Getting Started

### Prerequisites

Before using the CRE client APIs, ensure you have:

1. **CRE System Running** - The CRE master process must be started
2. **Client Modules Available** - Both `cre_client` and `cre_yawl_client` modules
3. **Required Callbacks Implemented** - For custom client behavior

### Initial Setup

```erlang
%% Step 1: Start the CRE system
cre:start().

%% Step 2: Get the CRE master PID
{ok, CrePid} = cre:pid(node()).

%% Step 3: Configure client options
persistent_term:put(cre_client_poll_interval, 250).
persistent_term:put(cre_client_timeout, 30000).
```

## Basic Usage

### Using cre_client (Generic Client)

The `cre_client` module provides a generic interface for workflow execution.

#### Example: Simple Workflow Execution

```erlang
%% Define a simple client module
-module(simple_client).
-behaviour(cre_client).

init(_Args) ->
    #workflow_state{tasks = []}.

is_value(_E, #workflow_state{complete = true}) -> true;
is_value(_E, _UsrInfo) -> false.

step(_E, UsrInfo) ->
    case get_next_task(UsrInfo) of
        {ok, Task, NewUsrInfo} ->
            {ok, NewUsrInfo, [Task]};
        {error, complete} ->
            {ok, UsrInfo, []}
    end.

recv(_E, [{TaskId, Result} | Rest], UsrInfo) ->
    NewUsrInfo = UsrInfo#workflow_state{
        results = UsrInfo#workflow_state.results#{TaskId => Result},
        complete = is_workflow_complete(NewUsrInfo)
    },
    recv(_E, Rest, NewUsrInfo);
recv(_E, [], UsrInfo) ->
    UsrInfo.

load(_E, _UsrInfo) ->
    #workflow_state{started = erlang:timestamp()}.

unload(_E, UsrInfo) ->
    UsrInfo#workflow_state.results.
```

#### Complete Workflow Example

```erlang
%% Complete workflow using cre_client
demo_basic_workflow() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create and start client
    {ok, ClientPid} = cre_client:start_link(CrePid, simple_client, []),

    %% Submit workflow
    Workflow = #{tasks => [task1, task2, task3], data => initial_data},
    Result = cre_client:eval(ClientPid, Workflow),

    %% Clean up
    cre_client:stop(ClientPid),

    Result.

%% Doctest:
1> demo_basic_workflow().
{ok, #{task1 => result1, task2 => result2, task3 => result3}}
```

### Using cre_yawl_client (YAWL-Specific Client)

The `cre_yawl_client` module provides specialized support for YAWL workflows.

#### Example: Simple YAWL Workflow

```erlang
%% Create a YAWL workflow
create_simple_workflow() ->
    Workflow = cre_yawl:new_workflow(),

    %% Add tasks
    Workflow1 = cre_yawl:add_task(Workflow, <<"task1">>,
        [{name, "First Task"}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, <<"task2">>,
        [{name, "Second Task"}, {type, atomic}]),

    %% Connect tasks
    Workflow3 = cre_yawl:connect(Workflow2, <<"task1">>, <<"task2">>),

    Workflow3.

%% Execute the workflow
demo_yawl_workflow() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create workflow
    Workflow = create_simple_workflow(),

    %% Start YAWL client
    {ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow),

    %% Execute workflow
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),

    %% Clean up
    cre_yawl_client:terminate_workflow(ClientPid),

    Result.

%% Doctest:
1> demo_yawl_workflow().
{ok, #{status => completed, task1 => result1, task2 => result2}}
```

## Workflow Patterns

### Pattern 1: Sequential Execution

```erlang
%% Sequential workflow using YAWL patterns
sequential_workflow_example() ->
    %% Create sequence pattern
    Pattern = cre_yawl:sequence([
        cre_yawl:task(<<"task1">>, task_mod, task_function, [arg1]),
        cre_yawl:task(<<"task2">>, task_mod, task_function, [arg2]),
        cre_yawl:task(<<"task3">>, task_mod, task_function, [arg3])
    ]),

    %% Execute pattern
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Pattern),
    {ok, Results} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),

    Results.

%% Doctest:
1> sequential_workflow_example().
{ok, #{status => completed, task1 => result1, task2 => result2, task3 => result3}}
```

### Pattern 2: Parallel Execution

```erlang
%% Parallel workflow using YAWL patterns
parallel_workflow_example() ->
    %% Create parallel split pattern
    Pattern = cre_yawl:parallel_split([
        cre_yawl:task(<<"task1">>, task_mod, task_function, []),
        cre_yawl:task(<<"task2">>, task_mod, task_function, []),
        cre_yawl:task(<<"task3">>, task_mod, task_function, [])
    ]),

    %% Execute pattern
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Pattern),
    {ok, Results} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),

    Results.

%% Doctest:
1> parallel_workflow_example().
{ok, #{status => completed, task1 => result1, task2 => result2, task3 => result3}}
```

### Pattern 3: Conditional Execution

```erlang
%% Conditional workflow using exclusive choice
conditional_workflow_example() ->
    %% Create exclusive choice pattern
    Pattern = cre_yawl:exclusive_choice([
        {cre_yawl:task(<<"task1">>, task_mod, task_function, []),
         fun() -> condition1() end},
        {cre_yawl:task(<<"task2">>, task_mod, task_function, []),
         fun() -> condition2() end}
    ]),

    %% Execute pattern
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Pattern),
    {ok, Results} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),

    Results.

%% Doctest:
1> conditional_workflow_example().
{ok, #{status => completed, selected_task => task1}}
```

### Pattern 4: Complex Workflow Composition

```erlang
%% Complex workflow with multiple patterns
complex_workflow_example() ->
    %% Create individual patterns
    PreProcess = cre_yawl:sequence([
        cre_yawl:task(<<"data_validation">>, data_mod, validate, []),
        cre_yawl:task(<<"data_transform">>, data_mod, transform, [])
    ]),

    ParallelProcessing = cre_yawl:parallel_split([
        cre_yawl:task(<<"analysis1">>, analysis_mod, analyze1, []),
        cre_yawl:task(<<"analysis2">>, analysis_mod, analyze2, []),
        cre_yawl:task(<<"analysis3">>, analysis_mod, analyze3, [])
    ]),

    PostProcess = cre_yawl:sequence([
        cre_yawl:task(<<"result_aggregation">>, result_mod, aggregate, []),
        cre_yawl:task(<<"report_generation">>, report_mod, generate, [])
    ]),

    %% Compose patterns sequentially
    Patterns = [PreProcess, ParallelProcessing, PostProcess],
    Options = #{mode => sequence, error_handling => continue},
    ComposedWorkflow = cre_yawl_client:compose_patterns(Patterns, Options),

    %% Execute composed workflow
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, ComposedWorkflow),
    {ok, Results} = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),

    Results.

%% Doctest:
1> complex_workflow_example().
{ok, #{status => completed,
       data_validation => ok,
       data_transform => ok,
       analysis1 => result1,
       analysis2 => result2,
       analysis3 => result3,
       result_aggregation => aggregated,
       report_generation => report.pdf}}
```

## Advanced Topics

### 1. Error Handling and Recovery

```erlang
%% Robust workflow with error handling
error_handling_example() ->
    %% Create workflow with error handling
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, risky_task,
        [{name, "Risky Task"}, {retry_count, 3}]),
    Workflow2 = cre_yawl:add_task(Workflow1, fallback_task,
        [{name, "Fallback Task"}, {type, atomic}]),
    Workflow3 = cre_yawl:connect(Workflow2, risky_task, fallback_task),

    %% Execute with error handling
    try
        {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Workflow3),
        Result = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),

        case Result of
            #{status := completed} ->
                handle_success(Result);
            #{status := failed, errors := Errors} ->
                handle_failure(Errors)
        end
    catch
        error:Reason ->
            handle_critical_error(Reason)
    end.

%% Retry logic implementation
execute_with_retry(Fun, MaxRetries) ->
    execute_with_retry(Fun, MaxRetries, 1000).

execute_with_retry(Fun, 0, _) -> Fun();
execute_with_retry(Fun, Retries, Delay) ->
    case Fun() of
        {ok, Result} -> Result;
        {error, _} ->
            timer:sleep(Delay),
            execute_with_retry(Fun, Retries - 1, Delay * 2)
    end.
```

### 2. Monitoring and Telemetry

```erlang
%% Workflow monitoring with telemetry
monitoring_example() ->
    %% Create workflow with monitoring
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, processing_task,
        [{name, "Processing"}, {monitor, true}]),
    Workflow2 = cre_yawl:add_task(Workflow1, validation_task,
        [{name, "Validation"}, {monitor, true}]),

    %% Execute with telemetry
    Telemetry = #{
        trace_id => generate_trace_id(),
        start_time => erlang:timestamp(),
        metadata => #{user_id => "user123", workflow_type => "processing"}
    },

    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Workflow2),

    %% Start monitoring
    spawn_link(fun() -> monitor_workflow_progress(ClientPid, Telemetry) end),

    %% Execute workflow
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),

    %% Record final metrics
    record_workflow_telemetry(Telemetry, Result),

    Result.

%% Progress monitoring function
monitor_workflow_progress(ClientPid, Telemetry) ->
    {ok, State} = cre_yawl_client:get_workflow_state(ClientPid),

    case State of
        #{execution_state := running} ->
            telemetry:increment(cre_workflow_progress, 1),
            timer:sleep(1000),
            monitor_workflow_progress(ClientPid, Telemetry);
        #{execution_state := completed} ->
            telemetry:increment(cre_workflow_completed, 1);
        #{execution_state := failed} ->
            telemetry:increment(cre_workflow_failed, 1)
    end.
```

### 3. State Management

```erlang
%% Advanced state management
state_management_example() ->
    %% Create workflow with state tracking
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, initialize_task,
        [{name, "Initialize"}, {stateful, true}]),
    Workflow2 = cre_yawl:add_task(Workflow1, process_task,
        [{name, "Process"}, {stateful, true}]),
    Workflow3 = cre_yawl:add_task(Workflow2, finalize_task,
        [{name, "Finalize"}, {stateful, true}]),

    %% Connect tasks
    Workflow4 = cre_yawl:connect(Workflow3, initialize_task, process_task),
    Workflow5 = cre_yawl:connect(Workflow4, process_task, finalize_task),

    %% Execute workflow with state tracking
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Workflow5),

    %% Initialize state
    InitialState = #{user_id => "user123", session_id => generate_session_id()},

    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, InitialState),

    %% Retrieve and log final state
    {ok, FinalState} = cre_yawl_client:get_workflow_state(ClientPid),

    log_workflow_state(InitialState, FinalState, Result),

    Result.

%% State persistence example
persist_workflow_state(ClientPid, StateData) ->
    {ok, CurrentState} = cre_yawl_client:get_workflow_state(ClientPid),

    %% Merge new state with existing
    UpdatedState = maps:merge(CurrentState, StateData),

    %% Update workflow results
    case cre_yawl_client:get_workflow_results(ClientPid) of
        {ok, Results} ->
            UpdatedResults = maps:merge(Results, UpdatedState),
            %% Note: This is for demonstration - actual state persistence
            %% would require custom implementation
            UpdatedResults;
        {error, _} ->
            UpdatedState
    end.
```

## Integration Examples

### 1. Human-in-the-Loop Workflow

```erlang
%% Human approval workflow
human_approval_workflow() ->
    %% Create workflow with human task
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, data_preparation,
        [{name, "Prepare Data"}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, human_approval,
        [{name, "Human Approval"}, {type, human}, {assignee, "manager"}]),
    Workflow3 = cre_yawl:add_task(Workflow2, notification,
        [{name, "Send Notification"}, {type, atomic}]),

    %% Connect tasks
    Workflow4 = cre_yawl:connect(Workflow3, data_preparation, human_approval),
    Workflow5 = cre_yawl:connect(Workflow4, human_approval, notification),

    %% Execute workflow
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Workflow5),
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => raw_data}),

    %% Handle human approval
    case Result of
        #{status := pending, task_id := human_approval} ->
            %% Human approval needed
            notify_human_approval_needed(),
            await_human_approval(ClientPid);
        #{status := completed} ->
            io:format("Workflow completed successfully~n")
    end,

    Result.

%% Human approval handling
await_human_approval(ClientPid) ->
    io:format("Waiting for human approval...~n"),

    %% Wait for approval (with timeout)
    receive
        {approve, human_approval} ->
            approve_task(ClientPid, human_approval);
        {reject, human_approval, Reason} ->
            reject_task(ClientPid, human_approval, Reason)
    after 300000 ->  % 5 minute timeout
        timeout_approval(ClientPid)
    end.

%% Human approval implementation
approve_task(ClientPid, TaskId) ->
    %% This would integrate with your human workflow system
    %% For demonstration, we'll simulate approval
    ApprovedResult = #{status => approved, decision => "Approved"},

    %% Continue workflow
    {ok, Results} = cre_yawl_client:execute_workflow(ClientPid, ApprovedResult),

    io:format("Task ~p approved~n", [TaskId]),
    Results.
```

### 2. Multi-Client Coordination

```erlang
%% Coordinated workflow with multiple clients
multi_client_workflow() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create specialized clients
    {ok, PreparationClient} = cre_client:start_link(CrePid, preparation_client, []),
    {ok, AnalysisClient} = cre_client:start_link(CrePid, analysis_client, []),
    {ok, ReportingClient} = cre_client:start_link(CrePid, reporting_client, []),

    %% Execute workflow with coordination
    PreparationResult = cre_client:eval(PreparationClient, prepare_data_workflow),

    case PreparationResult of
        {ok, Data} ->
            AnalysisResult = cre_client:eval(AnalysisClient, analyze_data_workflow),
            case AnalysisResult of
                {ok, Analysis} ->
                    ReportingResult = cre_client:eval(ReportingClient,
                        generate_report_workflow#{data => Data, analysis => Analysis}),
                    ReportingResult;
                {error, Error} ->
                    handle_analysis_error(Error)
            end;
        {error, Error} ->
            handle_preparation_error(Error)
    end.

%% Coordinated cleanup
multi_client_cleanup(Clients) ->
    lists:foreach(fun(Client) ->
        cre_client:stop(Client)
    end, Clients).
```

### 3. External System Integration

```erlang
%% Integration with external systems
external_integration_workflow() ->
    %% Create workflow with external tasks
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, api_call_task,
        [{name, "External API Call"}, {external, true},
         {endpoint, "https://api.example.com/data"}]),
    Workflow2 = cre_yawl:add_task(Workflow1, database_store_task,
        [{name, "Store in Database"}, {external, true}]),
    Workflow3 = cre_yawl:add_task(Workflow2, webhook_notify_task,
        [{name, "Notify Webhook"}, {external, true}]),

    %% Execute workflow
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Workflow3),

    %% Prepare external data
    ExternalData = #{
        api_key => get_api_key(),
        database_config => get_database_config(),
        webhook_url => get_webhook_url()
    },

    Result = cre_yawl_client:execute_workflow(ClientPid, ExternalData),

    Result.

%% External task handler
handle_external_task(TaskType, TaskData) ->
    case TaskType of
        api_call ->
            handle_api_call(TaskData);
        database_store ->
            handle_database_store(TaskData);
        webhook_notify ->
            handle_webhook_notify(TaskData);
        _ ->
            {error, unknown_task_type}
    end.
```

## Best Practices

### 1. Client Lifecycle Management

```erlang
%% Proper client lifecycle management
proper_lifecycle_management() ->
    %% Start client with supervision
    {ok, ClientPid} = cre_client:start_link(
        cre_master,
        supervised_client,
        #{supervisor => self()}
    ),

    %% Monitor client process
    MRef = monitor(process, ClientPid),

    %% Use client for workflow execution
    Result = try
        cre_client:eval(ClientPid, workflow_expr)
    catch
        exit:Reason ->
            handle_client_error(Reason)
    end,

    %% Clean up properly
    receive
        {'DOWN', MRef, process, ClientPid, normal} ->
            io:format("Client shutdown normally~n");
        {'DOWN', MRef, process, ClientPid, Reason} ->
            io:format("Client crashed: ~p~n", [Reason])
    after 5000 ->
        io:format("Client shutdown timeout~n")
    end,

    Result.
```

### 2. Resource Management

```erlang
%% Resource management for multiple clients
resource_management_example() ->
    %% Define resource limits
    ResourceConfig = #{
        max_concurrent_clients => 10,
        memory_limit => 1000000000, % 1GB
        cpu_limit => 80, % 80% CPU
        timeout => 300000 % 5 minutes
    },

    %% Start client with resource check
    start_client_with_resources(CrePid, ClientMod, ClientArgs, ResourceConfig) ->
        case check_resource_availability(ResourceConfig) of
            {ok, NewConfig} ->
                {ok, Pid} = cre_client:start_link(CrePid, ClientMod, ClientArgs),
                update_resource_usage(Pid, NewConfig),
                {ok, Pid};
            {error, Reason} ->
                {error, Reason}
        end.

%% Resource monitoring
monitor_resources() ->
    spawn_link(fun() ->
        resource_monitor_loop(#{
            max_concurrent => 10,
            current_usage => 0,
            last_check => erlang:timestamp()
        })
    end).

resource_monitor_loop(State) ->
    timer:sleep(5000), % Check every 5 seconds
    NewState = check_and_adjust_resources(State),
    resource_monitor_loop(NewState).
```

### 3. Error Recovery Strategies

```erlang
%% Comprehensive error recovery
error_recovery_strategies() ->
    %% Define recovery strategies
    RecoveryConfig = #{
        max_retries => 3,
        retry_delay => 1000,
        fallback_enabled => true,
        circuit_breaker_enabled => true
    },

    %% Execute with error recovery
    execute_with_recovery(Fun, RecoveryConfig) ->
        execute_with_retry(Fun, maps:get(max_retries, RecoveryConfig),
            maps:get(retry_delay, RecoveryConfig)).

%% Circuit breaker pattern
circuit_breaker(Fun, Module, Function) ->
    case is_circuit_closed(Module, Function) of
        true ->
            try
                Result = Fun(),
                record_success(Module, Function),
                Result
            catch
                _:Error ->
                    record_failure(Module, Function),
                    handle_circuit_breaker_error(Error)
            end;
        false ->
            {error, circuit_breaker_open}
    end.
```

### 4. Performance Optimization

```erlang
%% Performance optimization patterns
performance_optimization_example() ->
    %% Batch task execution
    batch_execution_example() ->
        TaskGroups = group_tasks_by_dependency(task_list),
        Results = lists:map(fun(Group) ->
            execute_task_batch(Group)
        end, TaskGroups),
        merge_results(Results).

%% Concurrency control
concurrency_control_example() ->
    MaxConcurrent = persistent_term:get(cre_client_max_concurrent, 100),

    %% Use pool to manage concurrent execution
    Pool = start_task_pool(MaxConcurrent),

    Tasks = [task1, task2, task3, task4, task5],
    Results = execute_with_pool(Pool, Tasks),

    stop_task_pool(Pool),
    Results.

## Conclusion

This tutorial has covered the complete usage of both CRE client APIs:

1. **cre_client** - Generic client for custom workflow implementations
2. **cre_yawl_client** - Specialized client for YAWL workflow patterns

Key takeaways:
- Start with the basic setup and ensure CRE is running
- Use cre_client for custom workflow logic
- Use cre_yawl_client for standard YAWL patterns
- Implement proper error handling and recovery
- Monitor performance and resources
- Follow best practices for client lifecycle management

The examples provided can be adapted for specific use cases and scaled according to your workflow requirements.