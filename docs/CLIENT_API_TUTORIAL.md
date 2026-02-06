# CRE Client API Tutorial

## Table of Contents

1. [Getting Started](#getting-started)
2. [Basic Usage](#basic-usage)
3. [Workflow Patterns](#workflow-patterns)
4. [Advanced Topics](#advanced-topics)
5. [Integration Examples](#integration-examples)
6. [Best Practices](#best-practices)

## Getting Started

### Prerequisites

Before using the CRE client API, ensure you have:

1. **Erlang/OTP** installed (version 25+ recommended)
2. **CRE project** compiled and available
3. **Basic understanding** of Erlang processes and OTP

### Setting Up Your Environment

```bash
# Clone and compile the CRE project
git clone <repository-url>
cd cre
rebar3 compile

# Start the Erlang shell with the project loaded
rebar3 shell
```

### First Steps with CRE Client

Let's start with a simple "Hello World" example to understand the basic workflow:

```erlang
%% Start the CRE master process
{ok, MasterPid} = cre_master:start_link(my_cre),

%% Create a simple workflow expression
Workflow = cre_yawl:sequence([
    {hello_task, "Print Hello", []},
    {world_task, "Print World", []}
]),

%% Start a YAWL client
{ok, ClientPid} = cre_yawl_client:start_link(my_cre, Workflow),

%% Execute the workflow
{ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

%% Display the result
io:format("Workflow result: ~p~n", [Result]),

%% Clean up
cre_yawl_client:terminate_workflow(ClientPid),
cre_master:stop(MasterPid).
```

## Basic Usage

### Understanding the Client-Master Architecture

The CRE system uses a client-server architecture:

1. **CRE Master** (`cre_master`): Central coordinator that manages worker pools
2. **CRE Client** (`cre_client`): Client process that connects to the master
3. **YAWL Client** (`cre_yawl_client`): Specialized client for YAWL workflows
4. **Workers**: Processes that execute individual tasks

### Creating Your First Workflow

Let's create a simple data processing workflow:

```erlang
%% Define a data processing workflow
data_processing_workflow() ->
    %% Start CRE master
    {ok, MasterPid} = cre_master:start_link(data_processor),

    %% Define workflow steps
    Workflow = cre_yawl:sequence([
        {read_data, "Read input data", [{file, "input.txt"}]},
        {process_data, "Process data", [{algorithm, "standard"}]},
        {save_result, "Save result", [{file, "output.txt"}]}
    ]),

    %% Start client and execute workflow
    {ok, ClientPid} = cre_yawl_client:start_link(data_processor, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Check results
    io:format("Processing completed: ~p~n", [Result]),

    %% Cleanup
    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid).
```

### Working with Custom Clients

Sometimes you need custom behavior for your workflows. Here's how to create a custom client:

```erlang
%% Define a custom client module
-module(my_custom_client).
-behaviour(cre_client).

%% Initialize the client
init(_Arg) ->
    #{user => "test_user", session => start_session()}.

%% Check if workflow is complete
is_value(E, UserInfo) ->
    %% Custom logic to determine completion
    case E of
        {completed, _} -> true;
        _ -> false
    end.

%% Execute one step of the workflow
step(E, UserInfo) ->
    case E of
        {start, _} ->
            %% First step - create some tasks
            NewE = {processing, step1},
            {ok, NewE, [{task1, "do_work"}]};
        {processing, step1} ->
            %% Second step - create more tasks
            NewE = {completed, final},
            {ok, NewE, [{task2, "finalize"}]};
        {completed, _} ->
            %% Workflow complete
            {ok, E, []}
    end.

%% Process worker replies
recv(E, Replies, UserInfo) ->
    %% Process the replies from workers
    io:format("Received replies: ~p~n", [Replies]),
    UserInfo.

%% Load workflow expression
load(E, _UserInfo) ->
    io:format("Loading workflow: ~p~n", [E]),
    E.

%% Finalize and return results
unload(E, UserInfo) ->
    io:format("Finalizing workflow: ~p~n", [E]),
    #{result => completed, user_info => UserInfo}.

%% Usage example
use_custom_client() ->
    {ok, MasterPid} = cre_master:start_link(my_cre),
    {ok, ClientPid} = cre_client:start_link(my_cre, my_custom_client, []),

    Result = cre_client:eval(ClientPid, {start, initial}),

    io:format("Custom client result: ~p~n", [Result]),

    cre_client:stop(ClientPid),
    cre_master:stop(MasterPid).
```

## Workflow Patterns

### Basic Control Flow Patterns

#### 1. Sequential Workflow

Execute tasks one after another:

```erlang
%% Step-by-step execution
sequential_workflow() ->
    Workflow = cre_yawl:sequence([
        {task1, "First step", []},
        {task2, "Second step", []},
        {task3, "Third step", []}
    ]),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

#### 2. Parallel Workflow

Execute multiple tasks simultaneously:

```erlang
%% Parallel execution
parallel_workflow() ->
    Workflow = cre_yawl:parallel_split([
        {process_a, "Process data A", []},
        {process_b, "Process data B", []},
        {process_c, "Process data C", []}
    ]),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

#### 3. Conditional Workflow

Execute different branches based on conditions:

```erlang
%% Conditional execution
conditional_workflow() ->
    Workflow = cre_yawl:exclusive_choice([
        {happy_path, fun() -> data_is_valid() end},
        {error_path, fun() -> true end}
    ]),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

#### 4. Synchronization

Wait for multiple parallel tasks to complete:

```erlang
%% Wait for all tasks to finish
synchronization_workflow() ->
    %% First process in parallel
    Parallel = cre_yawl:parallel_split([
        {task1, "Task 1", []},
        {task2, "Task 2", []}
    ]),

    %% Then synchronize
    Workflow = cre_yawl:sequence([
        Parallel,
        {merge_task, "Merge results", []}
    ]),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

### Advanced Workflow Patterns

#### 1. Multi-Choice Pattern

Execute multiple branches conditionally:

```erlang
%% Multiple conditional branches
multi_choice_workflow() ->
    Workflow = cre_yawl:multi_choice([
        {branch_a, fun() -> condition_a() end},
        {branch_b, fun() -> condition_b() end},
        {branch_c, fun() -> condition_c() end}
    ]),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

#### 2. Data Flow Patterns

Pass data between tasks:

```erlang
%% Data transformation workflow
data_flow_workflow() ->
    Workflow = cre_yawl:sequence([
        %% Transform data
        cre_yawl:data_transform(source_task, target_task, fun(Data) ->
            transform_data(Data)
        end),
        %% Accumulate results
        cre_yawl:data_accumulate([task1, task2], result_task)
    ]),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

#### 3. Resource Management Patterns

Allocate and manage resources:

```erlang
%% Resource allocation workflow
resource_workflow() ->
    Workflow = cre_yawl:sequence([
        %% Create resource
        cre_yawl:resource_create(resource_def),
        %% Allocate role
        cre_yawl:role_allocate(task, role),
        %% Start resource
        cre_yawl:resource_start(resource)
    ]),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

### Composing Complex Workflows

Combine multiple patterns into complex workflows:

```erlang
%% Complex multi-pattern workflow
complex_workflow() ->
    %% First, process data in parallel
    DataProcessing = cre_yawl:parallel_split([
        {preprocess_a, "Preprocess A", []},
        {preprocess_b, "Preprocess B", []}
    ]),

    %% Then, validate each result
    Validation = cre_yawl:exclusive_choice([
        {validation_passed, fun() -> all_valid() end},
        {validation_failed, fun() -> true end}
    ]),

    %% Finally, aggregate results
    Aggregation = cre_yawl:data_accumulate([preprocess_a, preprocess_b], final_result),

    %% Compose everything
    Workflow = cre_yawl:compose_patterns([
        DataProcessing,
        Validation,
        Aggregation
    ], #{mode => sequence}),

    {ok, Client} = cre_yawl_client:start_link(my_cre, Workflow),
    {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),

    cre_yawl_client:terminate_workflow(Client),
    Result.
```

## Advanced Topics

### Error Handling

#### 1. Validation Errors

Handle workflow validation failures:

```erlang
handle_validation_errors() ->
    try
        InvalidWorkflow = cre_yawl:sequence([
            {task1, "Valid task", []},
            {invalid_task, "", []}  %% Missing required fields
        ]),

        case cre_yawl:validate(InvalidWorkflow) of
            ok ->
                {ok, Client} = cre_yawl_client:start_link(my_cre, InvalidWorkflow),
                {ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),
                Result;
            {error, Errors} ->
                io:format("Validation errors: ~p~n", [Errors]),
                handle_validation_error(Errors)
        end
    catch
        Error:Reason ->
            io:format("Error: ~p~n", [{Error, Reason}]),
            error
    end.
```

#### 2. Runtime Errors

Handle errors during workflow execution:

```erlang
%% In your custom client module
step(E, UserInfo) ->
    try
        {ok, NewE, Tasks} = do_workflow_step(E, UserInfo),
        {ok, NewE, Tasks}
    catch
        {error, task_failed} ->
            %% Handle task failure gracefully
            NewE = {error, task_failed},
            {ok, NewE, []};
        Error:Reason ->
            %% Mark workflow as failed
            UserInfo1 = UserInfo#yawl_client_state{
                          execution_state = failed,
                          errors = [{Error, Reason}]
                         },
            {ok, UserInfo1, []}
    end.
```

#### 3. Retry Logic

Implement retry mechanisms for transient failures:

```erlang
execute_with_retry(ClientPid, Expr, MaxRetries) ->
    execute_with_retry(ClientPid, Expr, MaxRetries, 0).

execute_with_retry(_ClientPid, _Expr, MaxRetries, Retries) when Retries >= MaxRetries ->
    {error, max_retries_exceeded};

execute_with_retry(ClientPid, Expr, MaxRetries, Retries) ->
    try
        {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, Expr),
        Result
    catch
        throw:transient_error ->
            %% Wait before retry
            timer:sleep(1000 * (Retries + 1)),
            execute_with_retry(ClientPid, Expr, MaxRetries, Retries + 1)
    end.
```

### Monitoring and Debugging

#### 1. Workflow Monitoring

Monitor workflow progress in real-time:

```erlang
monitor_workflow(ClientPid) ->
    receive
        after 1000 ->  % Check every second
            {ok, State} = cre_yawl_client:get_workflow_state(ClientPid),
            #{execution_state := ExecState,
              active_tasks := ActiveCount,
              completed_tasks := CompletedCount,
              pending_tasks := PendingCount} = State,

            io:format("Progress: ~p (~p active, ~p completed, ~p pending)~n",
                      [ExecState, ActiveCount, CompletedCount, PendingCount]),

            case ExecState of
                completed -> ok;
                failed -> io:format("Workflow failed!~n");
                _ -> monitor_workflow(ClientPid)
            end
    end.
```

#### 2. Debug Mode

Enable detailed debugging:

```erlang
debug_workflow(Workflow) ->
    %% Start with debug logging enabled
    {ok, MasterPid} = cre_master:start_link(debug_cre),

    %% Create client with debug output
    {ok, ClientPid} = cre_yawl_client:start_link(debug_cre, Workflow),

    %% Monitor progress
    monitor_workflow(ClientPid),

    %% Execute workflow
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Display final result
    io:format("Final result: ~p~n", [Result]),

    %% Cleanup
    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid).
```

### Performance Optimization

#### 1. Batch Processing

Process multiple workflows efficiently:

```erlang
%% Process multiple workflows in parallel
process_batch_workflows(Workflows) ->
    Start = erlang:timestamp(),

    %% Start all clients
    Clients = [begin
                 {ok, Pid} = cre_yawl_client:start_link(my_cre, W),
                 Pid
             end || W <- Workflows],

    %% Execute all workflows
    Results = [cre_yawl_client:execute_workflow(Pid, #{}) || Pid <- Clients],

    %% Cleanup
    [cre_yawl_client:terminate_workflow(Pid) || Pid <- Clients],

    End = erlang:timestamp(),
    Duration = timer:now_diff(End, Start) / 1000,

    io:format("Processed ~p workflows in ~p ms~n", [length(Workflows), Duration]),

    Results.
```

#### 2. Connection Pooling

Reuse client connections for better performance:

```erlang
%% Client pool implementation
start_client_pool(Size) ->
    [begin
         {ok, Pid} = cre_yawl_client:start_link(my_cre, idle),
         Pid
     end || _ <- lists:seq(1, Size)].

get_client_from_pool(Pool) ->
    case Pool of
        [Pid | Rest] ->
            {Pid, Rest};
        [] ->
            %% Pool empty, create new client
            {new_client(), []}
    end.

return_client_to_pool(Pool, Pid) ->
    [Pid | Pool].

new_client() ->
    {ok, Pid} = cre_yawl_client:start_link(my_cre, idle),
    Pid.
```

#### 3. Workflow Caching

Cache workflow definitions for reuse:

```erlang
workflow_cache() ->
    case get(workflow_cache) of
        undefined ->
            Cache = initialize_cache(),
            put(workflow_cache, Cache),
            Cache;
        Cache ->
            Cache
    end.

initialize_cache() ->
    %% Create commonly used workflows
    StandardWorkflow = cre_yawl:sequence([
        {validate, "Validate", []},
        {process, "Process", []},
        {save, "Save", []}
    ]),

    Cache = #{
        standard => StandardWorkflow,
        parallel => cre_yawl:parallel_split([
            {process_a, "Process A", []},
            {process_b, "Process B", []}
        ])
    },
    Cache.

get_cached_workflow(WorkflowName) ->
    Cache = workflow_cache(),
    maps:get(WorkflowName, Cache, undefined).
```

## Integration Examples

### 1. File Processing Pipeline

Create a complete file processing workflow:

```erlang
file_processing_pipeline(InputFile, OutputFile) ->
    %% Define the workflow
    Workflow = cre_yawl:sequence([
        %% Read file
        {read_file, "Read input", [{file, InputFile}]},
        %% Process content
        {process_content, "Process data", [{output_file, OutputFile}]},
        %% Save results
        {save_results, "Save output", [{format, "json"}]}
    ]),

    %% Start CRE system
    {ok, MasterPid} = cre_master:start_link(file_processor),
    {ok, ClientPid} = cre_yawl_client:start_link(file_processor, Workflow),

    %% Execute workflow
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Check results
    case Result of
        #{status := completed} ->
            io:format("File processing completed successfully~n");
        #{status := failed, errors := Errors} ->
            io:format("File processing failed: ~p~n", [Errors])
    end,

    %% Cleanup
    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid).
```

### 2. Data Validation Workflow

Create a comprehensive data validation system:

```erlang
data_validation_workflow(Data) ->
    %% Define validation steps
    Workflow = cre_yawl:sequence([
        %% Initial validation
        {validate_format, "Validate format", []},
        %% Business rules validation
        cre_yawl:exclusive_choice([
            {valid_business_rules, fun() -> validate_business_rules(Data) end},
            {invalid_business_rules, fun() -> true end}
        ]),
        %% Final validation
        {validate_completeness, "Validate completeness", []}
    ]),

    {ok, MasterPid} = cre_master:start_link(data_validator),
    {ok, ClientPid} = cre_yawl_client:start_link(data_validator, Workflow),

    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{data => Data}),

    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid),

    Result.
```

### 3. Human-in-the-Loop Workflow

Create workflows requiring human approval:

```erlang
human_approval_workflow(TaskDescription) ->
    %% Define workflow with approval step
    Workflow = cre_yawl:sequence([
        {auto_task1, "Automatic processing 1", []},
        {approval_task, "Human approval required", [
            {description, TaskDescription},
            {requires_approval, true}
        ]},
        {auto_task2, "Automatic processing 2", []},
        {notification_task, "Send notification", []}
    ]),

    {ok, MasterPid} = cre_master:start_link(approval_workflow),
    {ok, ClientPid} = cre_yawl_client:start_link(approval_workflow, Workflow),

    %% Start workflow
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Wait for approval
    wait_for_approval(ClientPid),

    %% Continue workflow after approval
    {ok, FinalResult} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid),

    FinalResult.

wait_for_approval(ClientPid) ->
    receive
        after 5000 ->  % Check every 5 seconds
            {ok, State} = cre_yawl_client:get_workflow_state(ClientPid),
            case State of
                #{execution_state := running, pending_tasks := Pending} ->
                    case lists:member({approval_task, _}, Pending) of
                        true ->
                            io:format("Waiting for approval...~n"),
                            wait_for_approval(ClientPid);
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end
    end.
```

### 4. Distributed Workflow Processing

Create workflows that span multiple systems:

```erlang
distributed_workflow(LocalTasks, RemoteTasks) ->
    %% Process local tasks
    LocalWorkflow = cre_yawl:sequence(LocalTasks),

    %% Process remote tasks
    RemoteWorkflow = cre_yawl:sequence(RemoteTasks),

    %% Combine workflows
    Workflow = cre_yawl:sequence([
        LocalWorkflow,
        cre_yawl:synchronization(),
        RemoteWorkflow
    ]),

    {ok, MasterPid} = cre_master:start_link(distributed_workflow),
    {ok, ClientPid} = cre_yawl_client:start_link(distributed_workflow, Workflow),

    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid),

    Result.
```

## Best Practices

### 1. Client Lifecycle Management

#### Proper Initialization

```erlang
%% Always validate workflows before starting clients
start_validated_client(CreName, Workflow) ->
    case cre_yawl:validate(Workflow) of
        ok ->
            cre_yawl_client:start_link(CreName, Workflow);
        {error, Errors} ->
            {error, {validation_failed, Errors}}
    end.
```

#### Resource Cleanup

```erlang
%% Use try-catch or supervisor for proper cleanup
process_with_workflow(Fun, Workflow) ->
    {ok, MasterPid} = cre_master:start_link(temp_cre),
    {ok, ClientPid} = cre_yawl_client:start_link(temp_cre, Workflow),

    try
        Fun(ClientPid)
    after
        cre_yawl_client:terminate_workflow(ClientPid),
        cre_master:stop(MasterPid)
    end.
```

### 2. Error Handling Strategies

#### Comprehensive Error Handling

```erlang
%% Implement robust error handling
robust_workflow_execution(Workflow) ->
    try
        case cre_yawl:validate(Workflow) of
            ok ->
                {ok, ClientPid} = cre_yawl_client:start_link(my_cre, Workflow),
                try
                    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),
                    handle_success(Result)
                catch
                    Error:Reason ->
                        handle_workflow_error(Error, Reason, ClientPid)
                finally
                    cre_yawl_client:terminate_workflow(ClientPid)
                end;
            {error, Errors} ->
                handle_validation_errors(Errors)
        end
    catch
        Error:Reason ->
            handle_critical_error(Error, Reason)
    end.
```

#### Fallback Mechanisms

```erlang
%% Implement fallback workflows
execute_with_fallback(PrimaryWorkflow, FallbackWorkflow) ->
    {ok, ClientPid} = cre_yawl_client:start_link(my_cre, PrimaryWorkflow),
    try
        {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),
        {success, Result}
    catch
        Error:Reason ->
            io:format("Primary workflow failed: ~p, using fallback~n", [{Error, Reason}]),
            cre_yawl_client:terminate_workflow(ClientPid),

            {ok, FallbackPid} = cre_yawl_client:start_link(my_cre, FallbackWorkflow),
            {ok, FallbackResult} = cre_yawl_client:execute_workflow(FallbackPid, #{}),
            {fallback, FallbackResult}
    end.
```

### 3. Performance Optimization

#### Batch Processing

```erlang
%% Process similar workflows together
process_workflow_batch(Workflows) ->
    Grouped = group_similar_workflows(Workflows),

    Results = lists:map(fun({Pattern, InstanceWorkflows}) ->
        process_workflow_instances(Pattern, InstanceWorkflows)
    end, Grouped),

    lists:flatten(Results).

group_similar_workflows(Workflows) ->
    %% Group workflows by similarity
    lists:foldl(fun(Workflow, Acc) ->
        Pattern = extract_workflow_pattern(Workflow),
        maps:update_with(Pattern, fun(L) -> [Workflow | L] end, [Workflow], Acc)
    end, #{}, Workflows).
```

#### Memory Management

```erlang
%% Monitor memory usage
monitor_workflow_memory(ClientPid) ->
    process_flag(trap_exit, true),

    spawn_link(fun() ->
        memory_monitor_loop(ClientPid)
    end).

memory_monitor_loop(ClientPid) ->
    receive
        after 30000 ->  % Check every 30 seconds
            case process_info(self(), memory) of
                {memory, Memory} when Memory > 100000000 ->  % 100MB
                    io:format("Memory usage high: ~p bytes~n", [Memory]),
                    %% Potentially clean up or restart
                    ok;
                _ ->
                    ok
            end,
            memory_monitor_loop(ClientPid)
    end.
```

### 4. Testing Strategies

#### Unit Testing Workflows

```erlang
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_workflow_test() ->
    Workflow = cre_yawl:sequence([
        {task1, "Task 1", []},
        {task2, "Task 2", []}
    ]),

    {ok, MasterPid} = cre_master:start_link(test_cre),
    {ok, ClientPid} = cre_yawl_client:start_link(test_cre, Workflow),

    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    ?assertEqual(completed, maps:get(status, Result)),

    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid),
    ok.

error_handling_test() ->
    InvalidWorkflow = cre_yawl:sequence([
        {valid_task, "Valid", []},
        {invalid_task, "", []}
    ]),

    {error, {validation_failed, _}} = start_validated_client(test_cre, InvalidWorkflow),
    ok.
-endif.
```

#### Integration Testing

```erlang
%% Integration test for complex workflows
integration_test() ->
    Workflow = create_complex_workflow(),

    {ok, MasterPid} = cre_master:start_link(integration_test),
    {ok, ClientPid} = cre_yawl_client:start_link(integration_test, Workflow),

    %% Execute workflow and monitor
    spawn(fun() ->
        monitor_workflow_progress(ClientPid)
    end),

    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

    %% Validate results
    ?assertEqual(completed, maps:get(status, Result)),
    ?assertMatch(#{results := _}, Result),

    cre_yawl_client:terminate_workflow(ClientPid),
    cre_master:stop(MasterPid),
    ok.
```

### 5. Documentation and Maintenance

#### Document Workflow Definitions

```erlang
%% Document your workflows with metadata
documented_workflow() ->
    #{workflow := Workflow,
      description := "Data processing pipeline",
      author := "Your Name",
      version => "1.0",
      created => calendar:local_time(),
      parameters => [input_file, output_file],
      expected_duration => 30000  % 30 seconds
     }.

%% Use documentation for validation and testing
validate_workflow_documentation(DocWorkflow) ->
    RequiredFields = [workflow, description, author],
    lists:foreach(fun(Field) ->
        case maps:get(Field, DocWorkflow, undefined) of
            undefined -> error(missing_field, Field);
            _ -> ok
        end
    end, RequiredFields).
```

This tutorial should give you a comprehensive understanding of how to use the CRE client API effectively. From basic setup to advanced patterns and best practices, you now have the knowledge to create sophisticated workflow applications using CRE.