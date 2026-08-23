# Error Handling Guide

This guide describes error handling patterns and strategies in the CRE (Common Runtime Environment) for YAWL workflows.

## Table of Contents

1. [Error Types](#error-types)
2. [Recovery Strategies](#recovery-strategies)
3. [Supervision Trees](#supervision-trees)
4. [Circuit Breaker Usage](#circuit-breaker-usage)
5. [Timeout Handling](#timeout-handling)
6. [Logging Errors](#logging-errors)
7. [Code Examples](#code-examples)

## Error Types

### Exception Types

The CRE defines several exception types in `cre_yawl_exception`:

| Exception Type | Description | Severity |
|----------------|-------------|----------|
| `business_exception` | Expected business rule violations | medium |
| `system_exception` | System-level failures | high |
| `timeout_exception` | Operation timeout | high |
| `resource_exception` | Resource unavailability | medium |
| `data_exception` | Data validation failures | medium |
| `communication_exception` | Network/communication failures | high |
| `validation_exception` | Input validation errors | low |
| `security_exception` | Security-related errors | critical |
| `workflow_exception` | Workflow execution errors | high |
| `compensation_exception` | Compensation execution failures | high |

### Error Tuples

Standard error return values used throughout CRE:

```erlang
% Standard error tuple
{error, Reason}

% Error with type and reason
{error, {Type, Reason}}

% Circuit breaker open
{error, circuit_open}

% Timeout error
{error, timeout}

% Not found
{error, not_found}

% Bad request/message
{error, bad_msg}

% Bad place (Petri net)
{error, #bad_place{name = Place}}
```

### Exception Records

The `yawl_exception` record encapsulates exception information:

```erlang
-record(yawl_exception, {
    id              :: exception_id(),
    type            :: exception_type(),
    severity        :: exception_severity(),
    message         :: binary(),
    context         :: map(),
    timestamp       :: integer(),
    stacktrace      :: list(),
    workflow_id     :: undefined | binary(),
    activity_id     :: undefined | binary(),
    compensation_attempts = 0 :: non_neg_integer(),
    retry_attempts = 0 :: non_neg_integer(),
    handled_by      :: undefined | handler_id(),
    resolved = false :: boolean()
}).
```

## Recovery Strategies

### Retry with Backoff

CRE supports multiple retry strategies via `exception_patterns`:

```erlang
% Retry strategies
-type retry_strategy() :: exponential | linear | constant | fibonacci.

% Retry configuration
-type retry_config() :: #{
    max_attempts => non_neg_integer(),
    strategy => retry_strategy(),
    base_delay => non_neg_integer(),
    max_delay => non_neg_integer(),
    multiplier => float()
}.
```

#### Retry Strategies

1. **Exponential Backoff** (default): Delay increases exponentially
   - Formula: `min(base_delay * multiplier^(attempt-1), max_delay)`
   - Example: 1000ms, 2000ms, 4000ms, 8000ms...

2. **Linear Backoff**: Delay increases linearly
   - Formula: `min(base_delay * attempt, max_delay)`
   - Example: 1000ms, 2000ms, 3000ms, 4000ms...

3. **Constant**: Fixed delay between retries
   - Formula: `base_delay`
   - Example: 1000ms, 1000ms, 1000ms...

4. **Fibonacci**: Delay follows Fibonacci sequence
   - Uses Fibonacci numbers: 1, 1, 2, 3, 5, 8, 13...

#### Retry Example

```erlang
% Configure retry policy
RetryConfig = #{
    max_attempts => 5,
    strategy => exponential,
    base_delay => 1000,
    max_delay => 60000,
    multiplier => 2.0
},

% Retry activity with backoff
exception_patterns:retry_with_backoff(Marking, Activity, RetryConfig).
```

### Compensation

Compensation undoes effects of completed activities when workflow fails.

```erlang
% Create a compensator
Compensator = exception_patterns:new_compensator(
    <<"activity_1">>,
    fun(Data) -> undo_activity(Data) end,
    immediate  % compensation_strategy()
),

% Execute compensation
{ok, Compensated} = exception_patterns:compensate(Compensator, InputData).
```

#### Compensation Strategies

| Strategy | Description |
|----------|-------------|
| `immediate` | Execute immediately when triggered |
| `deferred` | Execute after workflow reaches stable state |
| `chained` | Execute in dependency order |
| `parallel` | Execute concurrently (with dependencies) |

### Cancellation Patterns

```erlang
% Cancel a specific activity
Marking1 = exception_patterns:cancel_activity(Marking, task1),

% Cancel entire workflow case
Marking2 = exception_patterns:cancel_case(Marking),

% Cancel a region (set of activities)
Marking3 = exception_patterns:cancel_region(Marking, [task1, task2, task3]).
```

### Exception Escalation

```erlang
% Escalate exception to higher-level handler
Exception = #{
    type => system_exception,
    reason => database_unavailable,
    source => task1
},
Marking1 = exception_patterns:escalate_exception(Marking, Exception).

% Propagate to parent workflow
Marking2 = exception_patterns:propagate_exception(Marking, Exception).
```

## Supervision Trees

### Top-Level Supervisor

The `cre_sup` module manages core CRE processes:

```erlang
SupFlags = #{
    strategy => one_for_one,
    intensity => 0,
    period => 5
},
```

**Key Settings:**
- `strategy: one_for_one` - Only terminated child is restarted
- `intensity: 0` - No automatic restarts (manual recovery)
- `period: 5` - Time window in seconds

### Child Specifications

| Child | Restart Type | Shutdown Timeout | Type |
|-------|--------------|------------------|------|
| `cre_master` | temporary | 5000ms | worker |
| `yawl_timeout` | permanent | 5000ms | worker |
| `yawl_xes` | permanent | 5000ms | worker |
| `yawl_approval` | permanent | 5000ms | worker |
| `yawl_workflow_supervisor` | permanent | infinity | supervisor |
| `yawl_worklist` | permanent | 5000ms | worker |
| `yawl_registry` | permanent | 5000ms | worker |

### Restart Strategies

```erlang
% temporary - Do not restart (except on abnormal termination)
{restart, temporary}

% permanent - Always restart (even on normal termination)
{restart, permanent}

% transient - Restart only on abnormal termination
{restart, transient}
```

### Shutdown Timeouts

```erlang
% Workers: milliseconds
{shutdown, 5000}  % 5 seconds

% Supervisors: infinity (waits for children)
{shutdown, infinity}
```

## Circuit Breaker Usage

### Circuit Breaker Module

The `circuit_breaker` module implements the Circuit Breaker pattern to prevent cascading failures.

### States

| State | Description |
|-------|-------------|
| `closed` | Normal operation, requests pass through |
| `open` | Failed threshold reached, requests rejected |
| `half_open` | Testing if service has recovered |

### Starting a Circuit Breaker

```erlang
% With default config (threshold: 5, timeout: 60s)
{ok, Pid} = circuit_breaker:start_link(<<"my_service">>, fun() -> do_work() end).

% With custom config
Config = [
    {failure_threshold, 10},
    {timeout_ms, 30000},
    {success_threshold, 3},
    {call_timeout, 10000}
],
{ok, Pid} = circuit_breaker:start_link(<<"my_service">>, fun() -> do_work() end, Config).
```

### Using Circuit Breaker

```erlang
% Execute function through circuit breaker
case circuit_breaker:execute(<<"my_service">>, fun sensitive_call/0) of
    {ok, Result} ->
        handle_success(Result);
    {error, circuit_open} ->
        logger:warning("Circuit open, using fallback"),
        handle_fallback();
    {error, {Type, Reason}} ->
        logger:error("Call failed: ~p:~p", [Type, Reason]),
        handle_error(Type, Reason)
end.
```

### Circuit Breaker in Error Handlers

```erlang
% Register handler with circuit breaker
Handler = cre_yawl_exception:new_error_handler(
    <<"handler_1">>,
    [system_exception, communication_exception],
    fun(Exception) -> handle_error(Exception) end
),

Handlers1 = cre_yawl_exception:register_handler_with_circuit_breaker(
    Handlers,
    Handler,
    5,   % failure threshold
    60000 % timeout in ms
).
```

### Checking Circuit Breaker State

```erlang
% Get current state
{ok, #circuit_state{
    state = State,
    failures = Failures,
    successes = Successes,
    last_failure_time = LastFailure
}} = circuit_breaker:get_state(<<"my_service">>).

% Reset to closed state
ok = circuit_breaker:reset(<<"my_service">>).
```

## Timeout Handling

### gen_server Call Timeouts

```erlang
% Default 5-second timeout
Reply = gen_pnet:call(Pid, get_status).

% Custom timeout (milliseconds)
Reply = gen_pnet:call(Pid, get_status, 10000).

% Infinite timeout (use sparingly)
Reply = gen_pnet:call(Pid, get_status, infinity).
```

### gen_yawl Timeouts

```erlang
% Fire timeout for transition firing
{ok, Pid} = gen_yawl:start_link(
    my_workflow,
    InitArg,
    [{fire_timeout, 10000}]  % 10 seconds
),

% Progress timeout for workflow execution
{ok, Pid} = gen_yawl:start_link(
    my_workflow,
    InitArg,
    [{progress_timeout, 60000}]  % 60 seconds
).
```

### Handling Timeouts

```erlang
% Safe call with timeout
safe_call(Pid, Request) ->
    try gen_server:call(Pid, Request, 5000) of
        Response -> {ok, Response}
    catch
        exit:{timeout, _} ->
            {error, timeout};
        exit:{noproc, _} ->
            {error, no_process}
    end.
```

### Timeout Configuration in Retry Policy

```erlang
RetryPolicy = cre_yawl_exception:new_retry_policy(#{
    max_attempts => 3,
    timeout => 30000,  % 30 second timeout per attempt
    backoff => exponential
}).
```

## Logging Errors

### Logger Usage

CRE uses the `logger` module for structured logging:

```erlang
% Error logging
logger:error("Database connection failed: ~p", [Reason]).

% Warning logging
logger:warning("Retry attempt ~p failed", [Attempt]).

% Info logging
logger:info("Workflow ~p started", [WorkflowId]).

% Debug logging
logger:debug("Processing token: ~p", [Token]).

% With metadata
logger:error("Task failed",
    #{task_id => TaskId, reason => Reason, retry_count => Count}).
```

### Error Logging Patterns

```erlang
% Log exception with context
log_exception(Exception) ->
    logger:error("Exception: ~s",
        [cre_yawl_exception:exception_message(Exception)],
        #{type => cre_yawl_exception:exception_type(Exception),
          severity => cre_yawl_exception:exception_severity(Exception),
          workflow_id => cre_yawl_exception:exception_workflow_id(Exception),
          activity_id => cre_yawl_exception:exception_activity_id(Exception)}).
```

### Monitor Logging

The `yawl_monitor` module provides metric recording:

```erlang
% Record error metric
yawl_monitor:record_metric(
    <<"workflow_error">>,
    1,
    #{
        error_type => <<"timeout">>,
        workflow_id => WorkflowId,
        task_id => TaskId
    }
).
```

## Code Examples

### Complete Error Handling Pattern

```erlang
-module(my_workflow_handler).
-behaviour(gen_pnet).

%% Callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Includes
-include_lib("kernel/include/logger.hrl").

place_lst() -> [input, processing, output, error, retry].

trsn_lst() -> [process, handle_error, retry, compensate].

init_marking(input, _UsrInfo) -> [ready];
init_marking(_Place, _UsrInfo) -> [].

preset(process) -> [input];
preset(handle_error) -> [error];
preset(retry) -> [retry];
preset(compensate) -> [output].

is_enabled(process, #{input := [_]}, _UsrInfo) -> true;
is_enabled(handle_error, #{error := [_]}, _UsrInfo) -> true;
is_enabled(retry, #{retry := [_, Count]}, _UsrInfo) when Count < 3 -> true;
is_enabled(compensate, #{output := [Data]}, _UsrInfo) -> true;
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

%% Process transition - wrap in try-catch
fire(process, #{input := [Data]}, _UsrInfo) ->
    try
        Result = risky_operation(Data),
        {produce, #{output => [Result], input => []}}
    catch
        Type:Reason:Stack ->
            ?LOG_ERROR("Process failed: ~p:~p", [Type, Reason]),
            ?LOG_DEBUG("Stack: ~p", [Stack]),
            Exception = cre_yawl_exception:new_exception(
                system_exception,
                <<"Processing failed">>,
                #{data => Data},
                __STACKTRACE__
            ),
            {produce, #{error => [Exception], input => []}}
    end;

%% Handle error transition - with retry
fire(handle_error, #{error := [Exception]}, _UsrInfo) ->
    Type = cre_yawl_exception:exception_type(Exception),
    case Type of
        system_exception ->
            % Retry with backoff
            RetryCount = cre_yawl_exception:exception_retry_attempts(Exception),
            case RetryCount < 3 of
                true ->
                    {produce, #{retry => [{Exception, RetryCount + 1}], error => []}};
                false ->
                    % Max retries exceeded
                    ?LOG_ERROR("Max retries exceeded for ~p", [Type]),
                    {produce, #{output => [failed]}}
            end;
        _ ->
            % Non-retryable error
            {produce, #{output => [failed]}}
    end;

%% Retry transition
fire(retry, #{retry := [{Exception, Count}]}, _UsrInfo) ->
    ?LOG_INFO("Retrying operation, attempt ~p", [Count]),
    Exception1 = Exception#yawl_exception{retry_attempts = Count},
    {produce, #{input => [Exception1], retry => []}}.

init(Arg) -> Arg.

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast(_Request, State) ->
    noreply.

handle_info(_Info, State) ->
    noreply.

terminate(_Reason, _State) ->
    ok.
```

### Using Circuit Breaker for External Service

```erlang
-module(service_client).
-behaviour(gen_server).

-export([start_link/0, call_service/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Start circuit breaker for external service
    {ok, _BreakerPid} = circuit_breaker:start_link(
        <<"external_api">>,
        fun() => external_api_call() end,
        [
            {failure_threshold, 5},
            {timeout_ms, 30000},
            {success_threshold, 2}
        ]
    ),
    {ok, #{}}.

call_service(Request) ->
    gen_server:call(?MODULE, {call, Request}, 60000).

handle_call({call, Request}, _From, State) ->
    case circuit_breaker:execute(<<"external_api">>, fun() -> external_api_call(Request) end) of
        {ok, Response} ->
            {reply, {ok, Response}, State};
        {error, circuit_open} ->
            ?LOG_WARNING("Circuit breaker open, using fallback"),
            {reply, {ok, fallback_response()}, State};
        {error, Reason} ->
            ?LOG_ERROR("Service call failed: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

external_api_call() ->
    % Actual external API call
    ok.

external_api_call(_Request) ->
    external_api_call().

fallback_response() ->
    #{status => fallback, data => #{}}.
```

### Supervisor with Error Handling

```erlang
-module(my_workflow_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(WorkflowArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorkflowArgs).

init(WorkflowArgs) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    WorkerSpec = #{
        id => workflow_worker,
        start => {my_workflow, start_link, [WorkflowArgs]},
        restart => permanent,
        shutdown => 10000,
        type => worker,
        modules => [my_workflow]
    },

    {ok, {SupFlags, [WorkerSpec]}}.
```

## Best Practices

1. **Always use typed errors**: Return `{error, Reason}` tuples consistently
2. **Log with context**: Include relevant metadata in error logs
3. **Set appropriate timeouts**: Avoid infinite timeouts in production
4. **Use circuit breakers**: Prevent cascading failures for external services
5. **Implement compensation**: Design compensation handlers for critical activities
6. **Monitor retry counts**: Log and alert when retries are exhausted
7. **Handle all exceptions**: Use try-catch for external calls
8. **Document error scenarios**: Document possible errors and recovery strategies
9. **Use supervision trees**: Structure processes for fault isolation
10. **Test error paths**: Verify error handling logic with unit tests

## References

- `circuit_breaker.erl` - Circuit breaker pattern implementation
- `exception_patterns.erl` - Workflow exception handling patterns
- `cre_yawl_exception.erl` - Exception records and retry policies
- `gen_yawl.erl` - YAWL workflow wrapper with timeout handling
- `yawl_monitor.erl` - Workflow monitoring and metrics
- `cre_sup.erl` - Top-level supervisor configuration
