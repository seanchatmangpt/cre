# CRE Client API Guide

## Overview

This guide documents the client interfaces to the CRE (Common Runtime Environment) workflow engine. The CRE system provides two main client modules:

1. **`cre_client`** - Generic client gen_server implementation for CRE workflow execution
2. **`cre_yawl_client`** - YAWL-specific client implementation with workflow pattern support

## Table of Contents

1. [cre_client Module](#cre_client-module)
   - [Basic Usage](#basic-usage)
   - [API Functions](#api-functions)
   - [Callback Interface](#callback-interface)
   - [Request/Response Patterns](#requestresponse-patterns)
   - [Error Handling](#error-handling)
   - [Configuration](#configuration)

2. [cre_yawl_client Module](#cre_yawl_client-module)
   - [Workflow Execution](#workflow-execution)
   - [Pattern Support](#pattern-support)
   - [Composition](#composition)
   - [State Management](#state-management)
   - [Integration Patterns](#integration-patterns)

3. [Best Practices](#best-practices)
   - [Client Lifecycle Management](#client-lifecycle-management)
   - [Performance Considerations](#performance-considerations)
   - [Error Recovery](#error-recovery)
   - [Testing Strategies](#testing-strategies)

## cre_client Module

### Basic Usage

The `cre_client` module provides a generic gen_server implementation that acts as a client for communicating with the CRE master process.

```erlang
%% Start an anonymous client
{ok, ClientPid} = cre_client:start_link(my_cre, my_client_mod, []).

%% Submit a workflow for evaluation
Result = cre_client:eval(ClientPid, my_workflow_expr).

%% Stop the client
ok = cre_client:stop(ClientPid).
```

### API Functions

#### `start_link/3` - Start Anonymous Client

```erlang
%% @doc Starts an anonymous CRE client gen_server.
%%
%% Links the client to the CRE master process identified by `CreName`.
%% The `ClientMod` module must implement the `cre_client` callback interface
%% to customize workflow evaluation behavior.
%%
%% ## Parameters
%% - `CreName`: Name or pid of the CRE master process
%% - `ClientMod`: Module implementing the client callbacks
%% - `ClientArg`: Argument passed to `ClientMod:init/1`
%%
%% ## Returns
%% `{ok, Pid}` on success, where `Pid` is the client process identifier
%%
%% ## Example
cre_client:start_link(my_cre, my_client_mod, []).

%% Doctest:
1> {ok, Pid} = cre_client:start_link(my_cre, my_client_mod, []).
{ok,<0.123.0>}
```

#### `start_link/4` - Start Named Client

```erlang
%% @doc Starts a named CRE client gen_server.
%%
%% Registers the client under `ClientName` (an atom) and links it to the
%% CRE master process identified by `CreName`. The client uses `ClientMod`
%% for callbacks during workflow evaluation.
%%
%% ## Parameters
%% - `ClientName`: Atom name to register the client process
%% - `CreName`: Name or pid of the CRE master process
%% - `ClientMod`: Module implementing the client callbacks
%% - `ClientArg`: Argument passed to `ClientMod:init/1`
%%
%% ## Returns
%% `{ok, Pid}` on success, where `Pid` is the client process identifier
%%
%% ## Example
cre_client:start_link(my_client, my_cre, my_client_mod, []).

%% Doctest:
2> {ok, Pid} = cre_client:start_link(my_client, my_cre, my_client_mod, []).
{ok,<0.456.0>}
3> whereis(my_client).
<0.456.0>
```

#### `eval/2` - Evaluate Workflow Expression

```erlang
%% @doc Evaluates a workflow expression.
%%
%% Submits the expression `E` for evaluation through the connected CRE master.
%% This is a blocking call that only returns when the workflow completes or fails.
%%
%% The evaluation process:
%% 1. Loads the expression via `ClientMod:load/2`
%% 2. Iteratively evaluates via `ClientMod:step/2`
%% 3. Collects worker replies
%% 4. Returns the final result via `ClientMod:unload/2`
%%
%% ## Parameters
%% - `ClientName`: Name or pid of the client process
%% - `E`: Workflow expression to evaluate
%%
%% ## Returns
%% The result from `ClientMod:unload/2` when evaluation completes
%%
%% ## Example
Result = cre_client:eval(ClientPid, my_workflow_expr).

%% Doctest:
1> {ok, Pid} = cre_client:start_link(my_cre, my_client_mod, []).
1> Result = cre_client:eval(Pid, my_workflow_expr).
{ok, #{result => "completed"}}
```

#### `cre_reply/4` - Handle CRE Master Reply

```erlang
%% @doc Handles a reply from the CRE master.
%%
%% Asynchronously delivers a worker reply to the client. Called by the CRE
%% master when a worker completes a task. The reply is queued and processed
%% during the next evaluation step.
%%
%% ## Parameters
%% - `ClientName`: Name or pid of the client process
%% - `I`: Request identifier (typically the gen_server `From` tuple)
%% - `A`: The task argument that was sent to the worker
%% - `Delta`: The result value returned by the worker
%%
%% ## Returns
%% `ok` (cast operation, no response)
%%
%% ## Example
cre_client:cre_reply(my_client, From, task_arg, result_value).

%% Doctest:
1> cre_client:cre_reply(my_client, From, task_arg, result_value).
ok
```

#### `stop/1` - Stop Client Process

```erlang
%% @doc Stops the client process.
%%
%% Terminates the client gen_server gracefully, cleaning up all pending
%% requests and state maps.
%%
%% ## Parameters
%% - `ClientName`: Name or pid of the client process
%%
%% ## Returns
%% `ok`
%%
%% ## Example
cre_client:stop(my_client).

%% Doctest:
1> cre_client:stop(my_client).
ok
```

### Callback Interface

The `cre_client` behavior requires implementing the following callbacks:

#### `init/1` - Initialize Client

```erlang
%% @doc Initialize the client with custom user information.
%%
%% Called when the client starts, this callback should set up any
%% necessary state and configuration for the client module.
%%
%% ## Parameters
%% - `InitArg`: Initial argument passed to `start_link`
%%
%% ## Returns
%% UsrInfo - User information state maintained by the client
%%
%% ## Example
init(_) ->
    #client_config{
        user_id => <<"user123">>,
        workflow_limit => 100,
        timeout => 30000
    }.
```

#### `is_value/2` - Check Completion

```erlang
%% @doc Check if the workflow has reached a terminal state.
%%
## ## Parameters
%% - `E`: Current workflow expression
%% - `UsrInfo`: User information state
%%
%% ## Returns
%% `true` if workflow is complete, `false` otherwise
%%
%% ## Example
is_value(E, UsrInfo) ->
    case maps:get(complete, UsrInfo, false) of
        true -> true;
        false -> has_pending_tasks(E, UsrInfo)
    end.
```

#### `step/2` - Execute Evaluation Step

```erlang
%% @doc Execute one step of workflow evaluation.
%%
## ## Parameters
%% - `E`: Current workflow expression
%% - `UsrInfo`: User information state
%%
%% ## Returns
%% `{ok, NewUsrInfo, TaskList}` where TaskList contains tasks for CRE
%%
%% ## Example
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
%% @doc Process replies from worker executions.
%%
## ## Parameters
%% - `E`: Current workflow expression
%% - `ReplyLst`: List of {A, Delta} result tuples
%% - `UsrInfo`: User information state
%%
%% ## Returns
%% Updated user information state with replies processed
%%
%% ## Example
recv(E, [{TaskId, Result} | Rest], UsrInfo) ->
    NewUsrInfo = process_task_result(TaskId, Result, UsrInfo),
    recv(E, Rest, NewUsrInfo);
recv(_E, [], UsrInfo) ->
    UsrInfo.
```

#### `load/2` - Load Workflow Expression

```erlang
%% @doc Load a workflow expression into the client state.
%%
## ## Parameters
%% - `E`: Workflow expression to load
%% - `UsrInfo`: Current user information state
%%
%% ## Returns
%% Updated user information state with workflow loaded
%%
%% ## Example
load(E, _UsrInfo) ->
    #workflow_state{
        workflow_expr = E,
        start_time = erlang:timestamp(),
        task_queue = initialize_task_queue(E)
    }.
```

#### `unload/2` - Finalize Workflow

```erlang
%% @doc Unload and finalize workflow execution.
##
## ## Parameters
%% - `E`: Final workflow expression
%% - `UsrInfo`: User information state
##
## ## Returns
%% Final result from workflow execution
##
## Example
unload(E, UsrInfo) ->
    #{
        result => extract_final_result(E),
        stats => extract_execution_stats(UsrInfo),
        metadata => extract_metadata(UsrInfo)
    }.
```

### Request/Response Patterns

#### Asynchronous Workflow Evaluation

```erlang
%% Client callback implementation for asynchronous workflows
-module(async_workflow_client).
-behaviour(cre_client).

init(_) ->
    #async_state{pending = []}.

is_value(_E, #async_state{complete = true}) -> true;
is_value(_E, _UsrInfo) -> false.

step(E, UsrInfo) ->
    case fetch_next_tasks(E) of
        {ok, Tasks} ->
            {ok, UsrInfo, Tasks};
        {error, Reason} ->
            error(Reason)
    end.

recv(E, [{TaskId, Result} | Rest], UsrInfo) ->
    NewUsrInfo = UsrInfo#async_state{
        results = UsrInfo#async_state.results#{TaskId => Result}
    },
    case is_workflow_complete(E, NewUsrInfo) of
        true -> NewUsrInfo#async_state{complete = true};
        false -> recv(E, Rest, NewUsrInfo)
    end.

recv(_E, [], UsrInfo) ->
    UsrInfo.

load(E, _UsrInfo) ->
    #async_state{workflow = E, started = erlang:timestamp()}.

unload(_E, #async_state{results = Results}) ->
    Results.

%% Usage example
{ok, ClientPid} = cre_client:start_link(cre_master, async_workflow_client, []),
Result = cre_client:eval(ClientPid, workflow_expr).
```

#### Parallel Task Execution

```erlang
%% Client callback for parallel task processing
-module(parallel_client).
-behaviour(cre_client).

init(_Args) ->
    #parallel_state{workers = []}.

is_value(_E, #parallel_state{pending = []}) -> true;
is_value(_E, _UsrInfo) -> false.

step(E, #parallel_state{pending = Pending} = UsrInfo) when length(Pending) < 10 ->
    %% Limit concurrent tasks to 10
    NewTasks = get_tasks_for_parallel_execution(E),
    {ok, UsrInfo#parallel_state{pending = Pending ++ NewTasks}, NewTasks};
step(_E, UsrInfo) ->
    %% Max concurrent tasks reached, wait for replies
    {ok, UsrInfo, []}.

recv(E, [{TaskId, Result} | Rest], UsrInfo) ->
    case process_parallel_result(TaskId, Result, UsrInfo) of
        {ok, NewUsrInfo} ->
            recv(E, Rest, NewUsrInfo);
        {error, Error, NewUsrInfo} ->
            %% Handle error and continue
            recv(E, Rest, NewUsrInfo)
    end.

load(E, _UsrInfo) ->
    #parallel_state{
        workflow = E,
        active_tasks = maps:new(),
        completed_results = maps:new()
    }.

unload(_E, #parallel_state{completed_results = Results}) ->
    Results.
```

### Error Handling

#### Client Error Handling Examples

```erlang
%% Error handling in client callbacks
-module(resilient_client).
-behaviour(cre_client).

init(_Args) ->
    #resilient_state{retries = 0}.

is_value(_E, #resilient_state{error = Error}) when Error =/= undefined ->
    true;  // Treat error state as terminal
is_value(_E, #resilient_state{pending = []}) ->
    true;
is_value(_E, _UsrInfo) ->
    false.

step(E, #resilient_state{retries = Retries} = UsrInfo) when Retries < 3 ->
    try
        {ok, Tasks, NewUsrInfo} = get_tasks_with_retry(E, UsrInfo),
        {ok, NewUsrInfo, Tasks}
    catch
        error:Reason ->
            %% Retry on errors
            NewUsrInfo = UsrInfo#resilient_state{retries = Retries + 1},
            {ok, NewUsrInfo, []}
    end;
step(_E, UsrInfo) ->
    %% Max retries exceeded
    {ok, UsrInfo#resilient_state{error = max_retries_exceeded}, []}.

recv(E, [{TaskId, {error, Error}} | Rest], UsrInfo) ->
    %% Handle task errors
    case should_retry_task(Error) of
        true ->
            NewUsrInfo = UsrInfo#resilient_state{
                retries = UsrInfo#resilient_state.retries + 1
            },
            %% Add task back to pending queue
            recv(E, [{TaskId, retry} | Rest], NewUsrInfo);
        false ->
            recv(E, Rest, UsrInfo#resilient_state{
                errors = [Error | UsrInfo#resilient_state.errors]
            })
    end;
recv(E, [{TaskId, Result} | Rest], UsrInfo) ->
    %% Handle successful results
    NewUsrInfo = UsrInfo#resilient_state{
        results = UsrInfo#resilient_state.results#{TaskId => Result}
    },
    recv(E, Rest, NewUsrInfo).

load(E, _UsrInfo) ->
    #resilient_state{
        workflow = E,
        results = maps:new(),
        errors = [],
        retries = 0
    }.

unload(_E, #resilient_state{results = Results, errors = Errors}) ->
    #{
        results => Results,
        errors => Errors,
        success => case Errors of
                      [] -> true;
                      _ -> false
                  end
    }.
```

#### Error Handling Strategies

```erlang
%% Common error handling patterns
error_handling_examples() ->

    %% 1. Timeout handling
    handle_timeout(_E, _UsrInfo) ->
        case get_timeout_state() of
            expired ->
                {error, workflow_timeout};
            active ->
                {ok, continue_execution}
        end,

    %% 2. Resource error handling
    handle_resource_error(_E, UsrInfo) ->
        case check_resource_availability() of
            available ->
                {ok, UsrInfo};
            limited ->
                {error, resource_unavailable}
        end,

    %% 3. Data validation error handling
    handle_validation_error(_E, UsrInfo) ->
        case validate_input_data(UsrInfo) of
            valid ->
                {ok, UsrInfo};
            {invalid, Reasons} ->
                {error, {validation_failed, Reasons}}
        end.
```

### Configuration

#### Client Configuration Options

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

## cre_yawl_client Module

### Workflow Execution

The `cre_yawl_client` module provides specialized support for YAWL (Yet Another Workflow Language) workflows.

```erlang
%% Create and execute a YAWL workflow
-module(yawl_workflow_example).

-export([start_workflow/0]).

start_workflow() ->
    %% Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% Create a simple workflow
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, <<"task1">>,
        [{name, <<"First Task">>}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, <<"task2">>,
        [{name, <<"Second Task">>}, {type, atomic}]),
    Workflow3 = cre_yawl:connect(Workflow2, <<"task1">>, <<"task2">>),

    %% Start YAWL client
    {ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow3),

    %% Execute workflow
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),
    Result.

%% Doctest:
1> cre_yawl_workflow_example:start_workflow().
{ok, #{status => completed, data => processed_result}}
```

#### `start_link/2` - Start YAWL Client

```erlang
%% @doc Start a YAWL client linked to the calling process.
%%
## ## Parameters
%% - `CreName`: Name or PID of the CRE master process
%% - `WorkflowExpr`: The workflow or pattern to execute
%%
## ## Returns
%% `{ok, ClientPid}` or `{error, Reason}`
%%
## ## Example
{ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow).

%% Doctest:
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow_expr).
{ok,<0.123.0>}
```

#### `execute_workflow/2` - Execute Complete Workflow

```erlang
%% @doc Execute a complete workflow synchronously.
%%
## ## Parameters
%% - `ClientPid`: The client process PID
%% - `InitialData`: Initial data to pass to the workflow
##
## ## Returns
%% `{ok, Results}` or `{error, Reason}`
##
## ## Example
{ok, Results} = cre_yawl_client:execute_workflow(ClientPid, #{data => input}).

%% Doctest:
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> {ok, Results} = cre_yawl_client:execute_workflow(ClientPid, #{data => input}).
{ok, #{status => completed, duration_ms => 1500}}
```

#### `execute_pattern/3` - Execute Single Pattern

```erlang
%% @doc Execute a single YAWL pattern.
%%
## ## Parameters
%% - `ClientPid`: The client process PID
%% - `Pattern`: The pattern to execute
%% - `InputData`: Input data for the pattern
##
## ## Returns
%% `{ok, Results}` or `{error, Reason}`
##
## ## Example
{ok, Results} = cre_yawl_client:execute_pattern(ClientPid, sequence_pattern, InputData).

%% Doctest:
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, cre_master).
1> {ok, Results} = cre_yawl_client:execute_pattern(ClientPid, sequence_pattern, #{data => input}).
{ok, #{status => completed, result => processed}}
```

#### `get_workflow_state/1` - Get Current State

```erlang
%% @doc Get the current workflow state.
##
## ## Parameters
%% - `ClientPid`: The client process PID
##
## ## Returns
%% `{ok, StateMap}` or `{error, Reason}`
##
## ## Example
{ok, State} = cre_yawl_client:get_workflow_state(ClientPid).

%% Doctest:
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> {ok, State} = cre_yawl_client:get_workflow_state(ClientPid).
{ok, #{execution_state => running, active_tasks => 1, completed_tasks => 0}}
```

#### `get_workflow_results/1` - Get Execution Results

```erlang
%% @doc Get workflow execution results.
##
## ## Parameters
%% - `ClientPid`: The client process PID
##
## ## Returns
%% `{ok, Results}` or `{error, Reason}`
##
## ## Example
{ok, Results} = cre_yawl_client:get_workflow_results(ClientPid).

%% Doctest:
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> {ok, Results} = cre_yawl_client:get_workflow_results(ClientPid).
{ok, #{task1 => result1, task2 => result2}}
```

#### `terminate_workflow/1` - Terminate Running Workflow

```erlan%% @doc Terminate a running workflow.
##
## ## Parameters
%% - `ClientPid`: The client process PID
##
## ## Returns
%% `ok`
##
## ## Example
ok = cre_yawl_client:terminate_workflow(ClientPid).

%% Doctest:
1> {ok, ClientPid} = cre_yawl_client:start_link(my_cre, workflow).
1> ok = cre_yawl_client:terminate_workflow(ClientPid).
ok
```

### Pattern Support

The `cre_yawl_client` supports all standard YAWL workflow patterns:

#### Sequence Pattern

```erlang
%% Execute a sequence pattern
sequence_example() ->
    %% Create sequence pattern
    Pattern = cre_yawl:sequence([
        cre_yawl:task(<<"task1">>, task_mod, task_function, [arg1]),
        cre_yawl:task(<<"task2">>, task_mod, task_function, [arg2])
    ]),

    %% Execute pattern
    {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),
    Result.

%% Doctest:
1> Pattern = cre_yawl:sequence([task1, task2]).
{sequence, [task1, task2]}
1> {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, input).
{ok, #{task1 => result1, task2 => result2}}
```

#### Parallel Split Pattern

```erlang
%% Execute a parallel split pattern
parallel_split_example() ->
    %% Create parallel split
    Pattern = cre_yawl:parallel_split([
        cre_yawl:task(<<"task1">>, task_mod, task_function, []),
        cre_yawl:task(<<"task2">>, task_mod, task_function, []),
        cre_yawl:task(<<"task3">>, task_mod, task_function, [])
    ]),

    %% Execute pattern
    {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),
    Result.

%% Doctest:
1> Pattern = cre_yawl:parallel_split([task1, task2, task3]).
{parallel_split, [task1, task2, task3]}
1> {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, input).
{ok, #{task1 => result1, task2 => result2, task3 => result3}}
```

#### Synchronization Pattern

```erlang
%% Execute a synchronization pattern
synchronization_example() ->
    %% Create synchronization (wait for all tasks to complete)
    Pattern = cre_yawl:synchronization([
        cre_yawl:task(<<"task1">>, task_mod, task_function, []),
        cre_yawl:task(<<"task2">>, task_mod, task_function, [])
    ]),

    %% Execute pattern
    {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),
    Result.

%% Doctest:
1> Pattern = cre_yawl:synchronization([task1, task2]).
{synchronization, [task1, task2]}
1> {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, input).
{ok, #{synchronized => true, results => [result1, result2]}}
```

#### Exclusive Choice Pattern

```erlang
%% Execute an exclusive choice pattern
exclusive_choice_example() ->
    %% Create exclusive choice with condition
    Pattern = cre_yawl:exclusive_choice([
        {cre_yawl:task(<<"task1">>, task_mod, task_function, []), fun() -> true end},
        {cre_yawl:task(<<"task2">>, task_mod, task_function, []), fun() -> false end}
    ]),

    %% Execute pattern
    {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),
    Result.

%% Doctest:
1> Pattern = cre_yawl:exclusive_choice([{task1, true_condition}, {task2, false_condition}]).
{exclusive_choice, [{task1, true_condition}, {task2, false_condition}]}
1> {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, input).
{ok, #{selected_task => task1}}
```

#### Multi-Choice Pattern

```erlang
%% Execute a multi-choice pattern
multi_choice_example() ->
    %% Create multi-choice (multiple branches can execute)
    Pattern = cre_yawl:multi_choice([
        {cre_yawl:task(<<"task1">>, task_mod, task_function, []), fun() -> true end},
        {cre_yawl:task(<<"task2">>, task_mod, task_function, []), fun() -> true end},
        {cre_yawl:task(<<"task3">>, task_mod, task_function, []), fun() -> false end}
    ]),

    %% Execute pattern
    {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, #{data => input}),
    Result.

%% Doctest:
1> Pattern = cre_yawl:multi_choice([{task1, true}, {task2, true}, {task3, false}]).
{multi_choice, [{task1, true}, {task2, true}, {task3, false}]}
1> {ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, input).
{ok, #{selected_tasks => [task1, task2]}}
```

### Composition

#### `compose_patterns/2` - Compose Multiple Patterns

```erlang
%% @doc Compose multiple patterns into a single workflow.
##
## ## Parameters
%% - `Patterns`: List of patterns to compose
%% - `Options`: Composition options (map with keys like mode, error_handling)
##
## ## Returns
%% Composed workflow expression
##
## ## Example
Patterns = [sequence_pattern, parallel_split_pattern],
Options = #{mode => sequence, error_handling => continue},
Composed = cre_yawl_client:compose_patterns(Patterns, Options).

%% Doctest:
1> Patterns = [pattern1, pattern2].
[pattern1, pattern2]
1> Options = #{mode => sequence, error_handling => continue}.
#{mode => sequence, error_handling => continue}
1> Composed = cre_yawl_client:compose_patterns(Patterns, Options).
{sequence, [pattern1, pattern2]}
```

#### Composition Modes

```erlang
%% Sequential composition
sequential_composition_example() ->
    Patterns = [pattern1, pattern2, pattern3],
    Options = #{mode => sequence},
    Composed = cre_yawl_client:compose_patterns(Patterns, Options),
    Composed.

%% Parallel composition
parallel_composition_example() ->
    Patterns = [pattern1, pattern2, pattern3],
    Options = #{mode => parallel},
    Composed = cre_yawl_client:compose_patterns(Patterns, Options),
    Composed.

%% Conditional composition
conditional_composition_example() ->
    Patterns = [pattern1, pattern2],
    Options = #{mode => conditional, condition => fun() -> true end},
    Composed = cre_yawl_client:compose_patterns(Patterns, Options),
    Composed.

%% Doctest examples:
1> sequential_composition_example().
{sequence, [pattern1, pattern2, pattern3]}
2> parallel_composition_example().
{parallel_split, [pattern1, pattern2, pattern3]}
3> conditional_composition_example().
{exclusive_choice, [{pattern1, true}, {pattern2, true}]}
```

### State Management

#### Workflow State Transitions

```erlang
%% Monitor workflow state changes
monitor_workflow_state(ClientPid) ->
    {ok, State} = cre_yawl_client:get_workflow_state(ClientPid),
    case State of
        #{execution_state := running} ->
            io:format("Workflow is running~n");
        #{execution_state := completed} ->
            io:format("Workflow completed~n");
        #{execution_state := failed} ->
            io:format("Workflow failed~n");
        #{execution_state := terminated} ->
            io:format("Workflow terminated~n")
    end.

%% State transitions example
state_transition_example() ->
    InitialState = #{
        execution_state => running,
        active_tasks => 0,
        completed_tasks => 0
    },

    %% Add active tasks
    State1 = maps:put(active_tasks, 3, InitialState),

    %% Complete a task
    State2 = maps:update(completed_tasks, 1, State1),

    %% Check completion
    IsComplete = cre_yawl_client:is_workflow_complete(State2),
    IsComplete.
```

#### Workflow Result Extraction

```erlang
%% Extract specific results from workflow
extract_workflow_results(ClientPid) ->
    {ok, Results} = cre_yawl_client:get_workflow_results(ClientPid),

    Extracted = #{
        task_results => maps:with([<<"task1">>, <<"task2">>], Results),
        status => maps:get(status, Results, unknown),
        duration => maps:get(duration_ms, Results, 0)
    },

    Extracted.

%% Results aggregation example
aggregate_results(ClientPids) ->
    lists:foldl(fun(Pid, Acc) ->
        case cre_yawl_client:get_workflow_results(Pid) of
            {ok, Results} ->
                Acc#{Pid => Results};
            {error, Error} ->
                Acc#{Pid => {error, Error}}
        end
    end, #{}, ClientPids).
```

### Integration Patterns

#### Human-in-the-Loop Integration

```erlang
%% Integrate human approval workflow
human_approval_workflow() ->
    %% Create workflow with human task
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, <<"data_preparation">>,
        [{name, "Prepare Data"}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, <<"human_approval">>,
        [{name, "Human Approval"}, {type, human}, {assignee, "manager"}]),
    Workflow3 = cre_yawl:connect(Workflow2, <<"data_preparation">>, <<"human_approval">>),

    %% Execute workflow
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Workflow3),
    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => raw_data}),

    %% Handle human approval
    case Result of
        #{status := pending, task_id := <<"human_approval">>} ->
            %% Approve the task
            approve_task(ClientPid, <<"human_approval">>);
        #{status := completed} ->
            io:format("Workflow completed successfully~n")
    end.

%% Doctest:
1> human_approval_workflow().
{ok, #{status => completed, approval_result => approved}}
```

#### Error Recovery Integration

```erlang
%% Error recovery pattern with retry
error_recovery_workflow() ->
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, risky_task,
        [{name, "Risky Task"}, {retry_count, 3}]),
    Workflow2 = cre_yawl:add_task(Workflow1, fallback_task,
        [{name, "Fallback Task"}, {type, atomic}]),
    Workflow3 = cre_yawl:connect(Workflow2, risky_task, fallback_task),

    %% Execute with error handling
    try
        {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, Workflow3),
        Result
    catch
        error:Error ->
            handle_workflow_error(Error)
    end.

%% Circuit breaker pattern
circuit_breaker_workflow() ->
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:exclusive_choice([
        {cre_yawl:task(primary_service, task_mod, task_function, []),
            fun() -> is_service_available(primary) end},
        {cre_yawl:task(fallback_service, task_mod, task_function, []),
            fun() -> true end}
    ]),

    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, Workflow1),
    Result.
```

#### Monitoring and Telemetry Integration

```erlang
%% Add monitoring to workflow execution
monitored_workflow() ->
    %% Create workflow with monitoring
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, processing_task,
        [{name, "Processing"}, {monitor, true}, {metric, processing_time}]),
    Workflow2 = cre_yawl:add_task(Workflow1, validation_task,
        [{name, "Validation"}, {monitor, true}]),

    %% Execute with telemetry
    {ok, ClientPid} = cre_yawl_client:start_link(cre_master, Workflow2),
    Telemetry = #{
        trace_id => generate_trace_id(),
        start_time => erlang:timestamp()
    },

    Result = cre_yawl_client:execute_workflow(ClientPid, #{data => input}),

    %% Record metrics
    record_workflow_metrics(Telemetry, Result),
    Result.
```

## Best Practices

### Client Lifecycle Management

```erlang
%% Proper client lifecycle management
lifecycle_management_example() ->

    %% 1. Start with proper supervision
    {ok, ClientPid} = cre_client:start_link(
        cre_master,
        supervised_client,
        #{supervisor => self()}
    ),

    %% 2. Monitor client process
    MRef = monitor(process, ClientPid),

    %% 3. Use client for workflow execution
    try
        Result = cre_client:eval(ClientPid, workflow_expr),
        handle_result(Result)
    catch
        exit:{Reason, _} ->
            handle_client_error(Reason)
    end,

    %% 4. Clean up
    receive
        {'DOWN', MRef, process, ClientPid, normal} ->
            io:format("Client shutdown normally~n");
        {'DOWN', MRef, process, ClientPid, Reason} ->
            io:format("Client crashed: ~p~n", [Reason])
    after 5000 ->
        %% Timeout handling
        io:format("Client shutdown timeout~n")
    end.
```

### Performance Considerations

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

### Error Recovery

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

### Testing Strategies

```erlang
%% Client testing strategies
testing_examples() ->

    %% 1. Unit testing client callbacks
    unit_test_example() ->
        %% Test callback functions in isolation
        UsrInfo = #client_state{...},
        Result = my_client:step(test_workflow, UsrInfo),
        Expected = {ok, UpdatedState, [task1, task2]},
        assert_equal(Expected, Result),

    %% 2. Integration testing
    integration_test_example() ->
        %% Test full workflow execution
        {ok, ClientPid} = cre_client:start_link(test_cre, test_client, []),
        {ok, Result} = cre_client:eval(ClientPid, test_workflow),
        assert_workflow_completed(Result),

    %% 3. Performance testing
    performance_test_example() ->
        %% Measure execution metrics
        {Time, Result} = timer:tc(fun() ->
            cre_client:eval(ClientPid, performance_test_workflow)
        end),
        io:format("Execution time: ~p ms~n", [Time/1000]),
        assert_performance_acceptable(Time, 5000),

    %% 4. Error testing
    error_test_example() ->
        %% Test error conditions
        {error, Reason} = cre_client:eval(ClientPid, error_workflow),
        assert_error_type(Reason, expected_error_type).
```

## Complete Example: Multi-Process Workflow

Here's a comprehensive example demonstrating both client modules working together:

```erlang
%% Complete workflow example
workflow_example() ->

    %% 1. Start CRE system
    cre:start(),
    {ok, CrePid} = cre:pid(node()),

    %% 2. Create a complex workflow
    Workflow = cre_yawl:new_workflow(),
    Workflow1 = cre_yawl:add_task(Workflow, <<"data_collection">>,
        [{name, "Collect Data"}, {type, atomic}]),
    Workflow2 = cre_yawl:add_task(Workflow1, <<"validation">>,
        [{name, "Validate Data"}, {type, atomic}]),
    Workflow3 = cre_yawl:add_task(Workflow2, <<"human_approval">>,
        [{name, "Human Approval"}, {type, human}, {assignee, "manager"}]),
    Workflow4 = cre_yawl:add_task(Workflow3, <<"notification">>,
        [{name, "Send Notification"}, {type, atomic}]),

    %% Connect tasks in sequence
    Workflow5 = cre_yawl:connect(Workflow4, <<"data_collection">>, <<"validation">>),
    Workflow6 = cre_yawl:connect(Workflow5, <<"validation">>, <<"human_approval">>),
    Workflow7 = cre_yawl:connect(Workflow6, <<"human_approval">>, <<"notification">>),

    %% 3. Start YAWL client
    {ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow7),

    %% 4. Execute workflow
    {ok, Results} = cre_yawl_client:execute_workflow(ClientPid, #{data => raw_data}),

    %% 5. Monitor workflow state
    {ok, State} = cre_yawl_client:get_workflow_state(ClientPid),

    %% 6. Handle results
    case Results of
        #{status := completed} ->
            io:format("Workflow completed: ~p~n", [Results]);
        #{status := failed, errors := Errors} ->
            io:format("Workflow failed: ~p~n", [Errors]);
        #{status := pending} ->
            io:format("Workflow pending approval~n")
    end,

    %% 7. Clean up
    cre_yawl_client:terminate_workflow(ClientPid),

    Results.

%% Doctest:
1> workflow_example().
{ok, #{status => completed, data => processed_data, notification_sent => true}}
```

This comprehensive guide provides detailed documentation for both client API modules, complete with doctest examples that demonstrate proper usage of the CRE API. The examples cover basic usage, advanced patterns, error handling, configuration, and integration scenarios.