# YAWL Telemetry Integration Guide

## Overview

The `yawl_telemetry` module provides comprehensive OpenTelemetry integration for YAWL workflow execution, enabling distributed tracing, metrics collection, and observability for workflow patterns.

## Architecture

### Design Principles

1. **Graceful Degradation**: Works with or without OpenTelemetry installed
2. **Zero Overhead When Disabled**: Silent mode disables all telemetry emission
3. **Generic Design**: Works with ANY YAWL specification
4. **Async Operations**: Non-blocking event emission for high-throughput workflows
5. **Comprehensive Metrics**: Tracks execution, failures, cancellations, and timing

### Key Components

- **Span Management**: Create and manage distributed tracing spans
- **Event Emission**: Lightweight events for workflow lifecycle tracking
- **Metrics Collection**: Aggregated metrics per workflow instance
- **Verbosity Control**: Configurable logging levels (silent, minimal, normal, verbose, debug)

## Installation

The telemetry module is included in the CRE workflow system. No additional dependencies required for basic functionality.

### Optional: OpenTelemetry Integration

To enable full OpenTelemetry support, add to your `rebar.config`:

```erlang
{deps, [
    {opentelemetry_api, "1.2.0"},
    {opentelemetry, "1.2.0"},
    {opentelemetry_exporter_jaeger, "1.2.0"}
]}.
```

## API Reference

### Starting the Telemetry Server

```erlang
% Start with default settings
{ok, Pid} = yawl_telemetry:start_link().
```

### Span Management

#### `start_span/4`

Starts a new OpenTelemetry span for workflow activity.

```erlang
{ok, SpanCtx} = yawl_telemetry:start_span(
    SpecId,      % Workflow specification ID
    CaseId,      % Workflow case instance ID
    TaskId,      % Task ID (optional, use undefined for workflow-level)
    SpanName     % Human-readable span name
).
```

**Example**:

```erlang
% Start workflow-level span
{ok, WfSpan} = yawl_telemetry:start_span(
    <<"order_fulfillment">>,
    <<"order-123">>,
    undefined,
    <<"Order Fulfillment Workflow">
).

% Start task-level span
{ok, TaskSpan} = yawl_telemetry:start_span(
    <<"order_fulfillment">>,
    <<"order-123">>,
    <<"payment_task">>,
    <<"Execute Payment">
).
```

#### `end_span/2`

Ends a span with completion status.

```erlang
ok = yawl_telemetry:end_span(SpanCtx, Status).

% Status options: success, failure, timeout, cancelled
```

**Example**:

```erlang
yawl_telemetry:end_span(TaskSpan, success).
yawl_telemetry:end_span(WfSpan, failure).
```

### Event Emission

#### `emit_event/4`

Emits a workflow event without creating a span.

```erlang
ok = yawl_telemetry:emit_event(
    SpecId,
    CaseId,
    EventType,
    Attributes
).
```

**Event Types**:

- `task_start`: Task execution started
- `task_complete`: Task completed successfully
- `task_fail`: Task failed with error
- `task_cancel`: Task was cancelled
- `workflow_start`: Workflow instance started
- `workflow_complete`: Workflow completed successfully
- `workflow_fail`: Workflow failed
- `checkpoint_created`: Checkpoint was created
- `checkpoint_restored`: Workflow was restored from checkpoint

**Example**:

```erlang
% Emit task start event
yawl_telemetry:emit_event(
    <<"order_fulfillment">>,
    <<"order-123">>,
    task_start,
    #{
        task_id => <<"payment">>,
        worker_id => <<"worker-1">>,
        start_time => erlang:system_time(millisecond)
    }
).

% Emit task complete event
yawl_telemetry:emit_event(
    <<"order_fulfillment">>,
    <<"order-123">>,
    task_complete,
    #{
        task_id => <<"payment">>,
        duration_ms => 250,
        result => <<"payment_processed">>
    }
).
```

### Task Tracking

#### `track_task/4`

Tracks task execution status with metrics.

```erlang
ok = yawl_telemetry:track_task(
    SpecId,
    CaseId,
    TaskId,
    Status
).
```

**Example**:

```erlang
yawl_telemetry:track_task(
    <<"order_fulfillment">>,
    <<"order-123">>,
    <<"payment_task">>,
    success
).
```

### Metrics Retrieval

#### `get_metrics/2`

Returns aggregated metrics for workflow instance.

```erlang
{ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId).
```

**Metrics Structure**:

```erlang
#{
    spec_id => SpecId,
    case_id => CaseId,
    total_tasks => TotalTasks,
    completed_tasks => CompletedTasks,
    failed_tasks => FailedTasks,
    cancelled_tasks => CancelledTasks,
    duration_ms => DurationMs,
    start_time => StartTime,
    end_time => EndTime
}
```

**Example**:

```erlang
{ok, Metrics} = yawl_telemetry:get_metrics(
    <<"order_fulfillment">>,
    <<"order-123">
),

io:format("Completed: ~p/~p~n", [
    maps:get(completed_tasks, Metrics),
    maps:get(total_tasks, Metrics)
]).
```

### Configuration

#### `set_verbosity/1`

Sets the telemetry verbosity level.

```erlang
ok = yawl_telemetry:set_verbosity(Verbosity).

% Verbosity levels:
% - silent:      No telemetry emitted
% - minimal:     Only critical errors
% - normal:      Errors and important events (default)
% - verbose:     All events and warnings
% - debug:       Everything including internal operations
```

**Example**:

```erlang
% Enable debug logging during development
yawl_telemetry:set_verbosity(debug).

% Reduce to minimal for production
yawl_telemetry:set_verbosity(minimal).
```

#### `flush/0`

Forces immediate emission of queued events.

```erlang
ok = yawl_telemetry:flush().
```

**Use Case**: Ensure all telemetry is sent before shutdown.

#### `get_span_context/1`

Extracts span context ID from span context map.

```erlang
SpanId = yawl_telemetry:get_span_context(SpanCtx).
```

## Usage Patterns

### Pattern 1: Complete Workflow Lifecycle

```erlang
% Start workflow
SpecId = <<"order_fulfillment">>,
CaseId = <<"order-123">>,

yawl_telemetry:emit_event(SpecId, CaseId, workflow_start, #{}),
{ok, WfSpan} = yawl_telemetry:start_span(
    SpecId, CaseId, undefined, <<"Order Fulfillment">
),

% Execute tasks with telemetry
Tasks = [
    {<<"validate">>, fun validate_order/1},
    {<<"payment">>, fun process_payment/1},
    {<<"inventory">>, fun check_inventory/1},
    {<<"shipping">>, fun arrange_shipping/1}
],

lists:foreach(
    fun({TaskId, TaskFn}) ->
        % Start task span
        {ok, TaskSpan} = yawl_telemetry:start_span(
            SpecId, CaseId, TaskId, <<"Execute ", TaskId/binary>>
        ),

        % Execute task
        Result = try TaskFn(CaseId) of
            {ok, _} ->
                yawl_telemetry:track_task(SpecId, CaseId, TaskId, success),
                yawl_telemetry:end_span(TaskSpan, success),
                success;
            {error, Reason} ->
                yawl_telemetry:track_task(SpecId, CaseId, TaskId, failure),
                yawl_telemetry:end_span(TaskSpan, failure),
                failure
        catch
            _:Exception ->
                yawl_telemetry:track_task(SpecId, CaseId, TaskId, failure),
                yawl_telemetry:end_span(TaskSpan, failure),
                failure
        end,

        % Emit event
        case Result of
            success ->
                yawl_telemetry:emit_event(SpecId, CaseId, task_complete, #{});
            failure ->
                yawl_telemetry:emit_event(SpecId, CaseId, task_fail, #{})
        end
    end,
    Tasks
),

% Get final metrics
{ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId),

% End workflow span
yawl_telemetry:end_span(WfSpan, success),
yawl_telemetry:emit_event(SpecId, CaseId, workflow_complete, #{}).
```

### Pattern 2: Parallel Task Execution

```erlang
% Start tasks in parallel with individual spans
ParentSpanCtx = ..., % from workflow

TaskIds = [<<"task1">>, <<"task2">>, <<"task3">>],

Spans = lists:map(
    fun(TaskId) ->
        {ok, Span} = yawl_telemetry:start_span(
            SpecId, CaseId, TaskId, <<"Parallel Task: ", TaskId/binary>>
        ),
        {TaskId, Span}
    end,
    TaskIds
),

% Execute in parallel
Pids = lists:map(
    fun({TaskId, Span}) ->
        spawn_monitor(fun() ->
            Result = execute_task(TaskId),
            yawl_telemetry:track_task(SpecId, CaseId, TaskId, Result),
            yawl_telemetry:end_span(Span, Result)
        end)
    end,
    Spans
),
```

### Pattern 3: Error Handling and Recovery

```erlang
% Track task with timeout handling
{ok, TaskSpan} = yawl_telemetry:start_span(
    SpecId, CaseId, TaskId, <<"Task with Timeout">
),

Result = try
    case execute_task_with_timeout(TaskId, 5000) of
        {ok, _} ->
            yawl_telemetry:track_task(SpecId, CaseId, TaskId, success),
            yawl_telemetry:end_span(TaskSpan, success);
        {error, timeout} ->
            yawl_telemetry:track_task(SpecId, CaseId, TaskId, timeout),
            yawl_telemetry:end_span(TaskSpan, timeout),
            retry_task(TaskId);
        {error, Reason} ->
            yawl_telemetry:track_task(SpecId, CaseId, TaskId, failure),
            yawl_telemetry:end_span(TaskSpan, failure),
            handle_error(TaskId, Reason)
    end
catch
    _:Exception ->
        yawl_telemetry:track_task(SpecId, CaseId, TaskId, failure),
        yawl_telemetry:end_span(TaskSpan, failure),
        error
end.
```

### Pattern 4: Checkpoint/Resume with Telemetry

```erlang
% Create checkpoint
ok = yawl_telemetry:emit_event(
    SpecId, CaseId, checkpoint_created, #{
        checkpoint_id => CheckpointId,
        tasks_completed => maps:get(completed_tasks, Metrics)
    }
),

% Later, restore from checkpoint
ok = yawl_telemetry:emit_event(
    SpecId, CaseId, checkpoint_restored, #{
        checkpoint_id => CheckpointId,
        tasks_remaining => maps:get(total_tasks, Metrics) - maps:get(completed_tasks, Metrics)
    }
).
```

## Verbosity Levels

### Silent

```erlang
yawl_telemetry:set_verbosity(silent).
```

**Effect**: No telemetry emitted. Useful for performance-critical sections or testing.

**Emits**: Nothing

### Minimal

```erlang
yawl_telemetry:set_verbosity(minimal).
```

**Effect**: Only critical errors.

**Emits**:
- `task_fail`
- `workflow_fail`

### Normal (Default)

```erlang
yawl_telemetry:set_verbosity(normal).
```

**Effect**: Errors and important events.

**Emits**:
- All minimal events
- `task_complete`
- `task_cancel`
- `workflow_complete`
- `workflow_start`

### Verbose

```erlang
yawl_telemetry:set_verbosity(verbose).
```

**Effect**: All events and warnings.

**Emits**:
- All normal events
- `task_start`
- Detailed error messages

### Debug

```erlang
yawl_telemetry:set_verbosity(debug).
```

**Effect**: Everything including internal operations.

**Emits**:
- All verbose events
- Span lifecycle events
- Internal diagnostic information

## Integration with Workflow Patterns

### Sequence Pattern

```erlang
sequence_pattern_telemetry(SpecId, CaseId) ->
    {ok, WfSpan} = yawl_telemetry:start_span(
        SpecId, CaseId, undefined, <<"Sequence Pattern">
    ),

    Steps = [step1, step2, step3],
    lists:foreach(
        fun(Step) ->
            {ok, StepSpan} = yawl_telemetry:start_span(
                SpecId, CaseId, Step, atom_to_binary(Step)>
            ),
            execute_step(Step),
            yawl_telemetry:end_span(StepSpan, success)
        end,
        Steps
    ),

    yawl_telemetry:end_span(WfSpan, success).
```

### Parallel Split Pattern

```erlang
parallel_split_telemetry(SpecId, CaseId) ->
    {ok, WfSpan} = yawl_telemetry:start_span(
        SpecId, CaseId, undefined, <<"Parallel Split">
    ),

    % Start all tasks
    Tasks = [task1, task2, task3],
    Spans = lists:map(
        fun(Task) ->
            {ok, Span} = yawl_telemetry:start_span(
                SpecId, CaseId, Task, atom_to_binary(Task)>
            ),
            {Task, Span}
        end,
        Tasks
    ),

    % Execute in parallel and track
    lists:foreach(
        fun({Task, Span}) ->
            spawn(fun() ->
                Result = execute_task(Task),
                yawl_telemetry:track_task(SpecId, CaseId, Task, Result),
                yawl_telemetry:end_span(Span, Result)
            end)
        end,
        Spans
    ),

    yawl_telemetry:end_span(WfSpan, success).
```

### Exclusive Choice Pattern

```erlang
exclusive_choice_telemetry(SpecId, CaseId) ->
    {ok, WfSpan} = yawl_telemetry:start_span(
        SpecId, CaseId, undefined, <<"Exclusive Choice">
    ),

    Condition = evaluate_condition(),

    SelectedBranch = case Condition of
        true -> branch_a;
        false -> branch_b
    end,

    {ok, BranchSpan} = yawl_telemetry:start_span(
        SpecId, CaseId, SelectedBranch, <<"Branch: ", SelectedBranch/binary>>
    ),

    execute_branch(SelectedBranch),
    yawl_telemetry:end_span(BranchSpan, success),
    yawl_telemetry:end_span(WfSpan, success).
```

## Performance Considerations

### Span Creation Overhead

Span creation is lightweight (< 100 microseconds per span).

**Benchmark Results**:
- 1000 spans: < 1 second
- 1000 track operations: < 500ms

### Memory Usage

Each span context is approximately 200 bytes.

**Recommendation**: For long-running workflows with many tasks, periodically call `flush/0` to emit queued events and free memory.

### Verbosity Impact

- **Silent**: Zero overhead
- **Minimal**: < 1% overhead
- **Normal**: < 2% overhead
- **Verbose**: < 5% overhead
- **Debug**: 5-10% overhead (use only in development)

## Testing

### Unit Tests

```erlang
% Run telemetry API tests
rebar3 eunit --module=yawl_telemetry_api_test.
```

### Manual Testing

```erlang
% Start telemetry
{ok, Pid} = yawl_telemetry:start_link(),

% Test span lifecycle
{ok, Span} = yawl_telemetry:start_span(
    <<"test_spec">>, <<"test_case">>, undefined, <<"Test Span">
),
yawl_telemetry:end_span(Span, success),

% Test metrics
yawl_telemetry:track_task(<<"spec">>, <<"case">>, <<"task1">>, success),
{ok, Metrics} = yawl_telemetry:get_metrics(<<"spec">>, <<"case">>),
io:format("Metrics: ~p~n", [Metrics]),

% Cleanup
gen_server:stop(Pid).
```

## Troubleshooting

### Telemetry Not Appearing

1. Check verbosity level: `yawl_telemetry:set_verbosity(debug)`
2. Verify server is running: `erlang:is_process_alive(whereis(yawl_telemetry))`
3. Check for errors in logs

### High Memory Usage

1. Reduce verbosity level
2. Call `flush/0` more frequently
3. Limit concurrent spans

### Missing Events

1. Ensure `emit_event/4` is called with correct parameters
2. Check event type matches verbosity level
3. Verify OpenTelemetry exporter configuration (if using OTEL)

## Best Practices

1. **Always close spans**: Ensure every `start_span/4` has matching `end_span/2`
2. **Use appropriate verbosity**: Normal for production, debug for development
3. **Track task outcomes**: Use `track_task/4` for accurate metrics
4. **Set meaningful span names**: Use descriptive names for debugging
5. **Include context**: Add relevant attributes to events for troubleshooting
6. **Handle errors gracefully**: Wrap telemetry calls in try-catch if needed
7. **Flush before shutdown**: Call `flush/0` before terminating workflow

## Example: Complete Workflow with Telemetry

```erlang
-module(order_fulfillment_telemetry).
-export([execute/1]).

execute(OrderId) ->
    SpecId = <<"order_fulfillment">>,
    CaseId = OrderId,

    % Start workflow
    yawl_telemetry:set_verbosity(normal),
    yawl_telemetry:emit_event(SpecId, CaseId, workflow_start, #{
        order_id => OrderId,
        timestamp => erlang:system_time(millisecond)
    }),

    {ok, WfSpan} = yawl_telemetry:start_span(
        SpecId, CaseId, undefined, <<"Order Fulfillment: ", OrderId/binary>>
    ),

    % Execute with telemetry
    Result = try
        % Step 1: Validate
        {ok, ValidateSpan} = yawl_telemetry:start_span(
            SpecId, CaseId, <<"validate">>, <<"Validate Order">
        ),
        {ok, Order} = validate_order(OrderId),
        yawl_telemetry:track_task(SpecId, CaseId, <<"validate">>, success),
        yawl_telemetry:end_span(ValidateSpan, success),

        % Step 2: Payment
        {ok, PaymentSpan} = yawl_telemetry:start_span(
            SpecId, CaseId, <<"payment">>, <<"Process Payment">
        ),
        {ok, Payment} = process_payment(Order),
        yawl_telemetry:track_task(SpecId, CaseId, <<"payment">>, success),
        yawl_telemetry:end_span(PaymentSpan, success),

        % Step 3: Inventory
        {ok, InventorySpan} = yawl_telemetry:start_span(
            SpecId, CaseId, <<"inventory">>, <<"Check Inventory">
        ),
        {ok, Shipment} = check_inventory(Order),
        yawl_telemetry:track_task(SpecId, CaseId, <<"inventory">>, success),
        yawl_telemetry:end_span(InventorySpan, success),

        % Success
        yawl_telemetry:end_span(WfSpan, success),
        yawl_telemetry:emit_event(SpecId, CaseId, workflow_complete, #{
            order_id => OrderId,
            shipment_id => Shipment
        }),
        {ok, Order}

    catch
        _:Error ->
            yawl_telemetry:end_span(WfSpan, failure),
            yawl_telemetry:emit_event(SpecId, CaseId, workflow_fail, #{
                order_id => OrderId,
                error => Error
            }),
            {error, Error}
    end,

    % Get final metrics
    {ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId),
    io:format("Workflow metrics: ~p~n", [Metrics]),

    Result.
```

## References

- [Complete API Reference](COMPLETE_API_REFERENCE.md)
- [YAWL Patterns Reference](YAWL_PATTERNS_REFERENCE_CARD.md)
- [Diataxis Architecture](DIATAXIS_ARCHITECTURE.md)
