# YAWL Telemetry and Observability Module

## Overview

The `yawl_telemetry` module provides comprehensive monitoring, observability, and distributed tracing capabilities for YAWL workflow pattern execution in the CRE runtime environment.

## Features

### 1. Telemetry Management

- **Service Lifecycle**: Start and stop telemetry service with custom configuration
- **Dynamic Configuration**: Update configuration at runtime
- **Configurable Retention**: Set retention periods for metrics and audit logs

### 2. OpenTelemetry-Style Span Management

```erlang
% Start a span for pattern execution
{ok, SpanId} = yawl_telemetry:start_span(multiple_instances_no_sync, <<"case_123">>),

% Add attributes to the span
ok = yawl_telemetry:span_attribute(SpanId, instance_count, 5),
ok = yawl_telemetry:span_attribute(SpanId, metadata, #{key => value}),

% Add events to the span
ok = yawl_telemetry:span_event(SpanId, task_started),

% Set span status
ok = yawl_telemetry:span_status(SpanId, running),

% End the span with result
ok = yawl_telemetry:end_span(SpanId, {ok, completed}, ok),

% Get span information
{ok, SpanInfo} = yawl_telemetry:get_span_info(SpanId),
```

### 3. Metrics Collection

```erlang
% Record pattern execution start
ok = yawl_telemetry:record_execution_start(implicit_termination, <<"case_456">>),

% Record pattern execution completion with duration (ms)
ok = yawl_telemetry:record_execution_complete(implicit_termination, <<"case_456">>, 1250),

% Record execution error
ok = yawl_telemetry:record_execution_error(implicit_termination, <<"case_456">>, timeout),

% Record timing for specific stage
ok = yawl_telemetry:record_timing(implicit_termination, validation, 45),

% Increment counter with tags
ok = yawl_telemetry:increment_counter(workitems_completed, #{workflow => <<"order_process">>}),

% Get metrics for a pattern type
Metrics = yawl_telemetry:get_metrics(implicit_termination),

% Get metrics summary
Summary = yawl_telemetry:get_metrics_summary(),
```

### 4. Distributed Tracing

```erlang
% Generate new trace context
TraceId = yawl_telemetry:generate_trace_id(),  % 128-bit hex
SpanId = yawl_telemetry:generate_span_id(),    % 64-bit hex

% Set trace context in process dictionary
ok = yawl_telemetry:set_trace_context(#{trace_id => TraceId, span_id => SpanId}),

% Get current trace context
Context = yawl_telemetry:get_trace_context(),

% Inject trace context into headers
Headers = yawl_telemetry:inject_trace_context(#{<<"x-request-id">> => <<"12345">>}),

% Extract trace context from incoming headers
Context2 = yawl_telemetry:extract_trace_context(IncomingHeaders),
```

### 5. Health Checks

```erlang
% Check specific pattern health
{ok, healthy, Info} = yawl_telemetry:check_pattern_health(<<"case_789">>),

% Get overall system health
{ok, SystemInfo} = yawl_telemetry:system_health(),

% Get component status
Status = yawl_telemetry:component_status(),

% Register custom health check
CheckFun = fun() -> {ok, healthy, #{status => ok}} end,
{ok, Ref} = yawl_telemetry:register_health_check(<<"database">>, CheckFun),

% Unregister health check
ok = yawl_telemetry:unregister_health_check(Ref),
```

### 6. Execution Visualization

```erlang
% Generate execution graph
{ok, Graph} = yawl_telemetry:execution_graph(<<"case_123">>),

% Export to DOT format (for Graphviz)
{ok, DotData} = yawl_telemetry:export_dot(<<"case_123">>),

% Get execution timeline
{ok, Timeline} = yawl_telemetry:execution_timeline(<<"case_123">>),

% Get execution tree
{ok, Tree} = yawl_telemetry:get_execution_tree(<<"case_123">>),

% Visualize pattern structure
{ok, PatternDot} = yawl_telemetry:visualize_pattern(implicit_termination, <<"case_123">>),
```

### 7. Alerting

```erlang
% Add alert rule
Condition = fun() -> length(yawl_telemetry:get_active_spans()) > 100 end,
Action = fun() -> logger:warning("Too many active spans") end,
{ok, AlertId} = yawl_telemetry:add_alert_rule(Condition, Action),

% Check and trigger alerts
Results = yawl_telemetry:check_alerts(),

% List alert rules
Rules = yawl_telemetry:list_alert_rules(),

% Manually trigger alert
ok = yawl_telemetry:trigger_alert(AlertId),

% Remove alert rule
ok = yawl_telemetry:remove_alert_rule(AlertId),
```

### 8. Audit Logging

```erlang
% Log event
ok = yawl_telemetry:log_event(workitem_completed, <<"case_123">>, #{workitem => <<"wi_1">>}),

% Log state change
ok = yawl_telemetry:log_state_change(<<"case_123">>, pending, running),

% Query audit log with filters
Filter = #{pattern_id => <<"case_123">>, from => 1640000000000},
{ok, AuditEntries} = yawl_telemetry:query_audit(Filter),

% Get audit log for pattern
{ok, Log} = yawl_telemetry:get_audit_log(<<"case_123">>),

% Export audit log to file
{ok, Count} = yawl_telemetry:export_audit_log("/tmp/audit.json", Filter),

% Clear audit log
ok = yawl_telemetry:clear_audit_log(),
ok = yawl_telemetry:clear_audit_log(<<"case_123">>),
```

### 9. Prometheus Export

```erlang
% Export all metrics
PrometheusText = yawl_telemetry:export_prometheus(),

% Export specific pattern type metrics
PrometheusText = yawl_telemetry:export_prometheus(implicit_termination),
```

Output format:
```
# HELP pattern_execution_complete YAWL pattern metric
# TYPE pattern_execution_complete gauge
pattern_execution_complete{pattern_type="implicit_termination"} 1250 1640000000000
```

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `metrics_retention_ms` | integer | 86400000 | Time to keep metrics (24 hours) |
| `audit_retention_ms` | integer | 604800000 | Time to keep audit logs (7 days) |
| `enable_opentelemetry` | boolean | false | Enable OpenTelemetry integration |
| `prometheus_port` | integer | 9091 | Port for Prometheus metrics endpoint |
| `max_spans` | integer | 10000 | Maximum active spans |

## Pattern Type Visualization

The module provides DOT visualization for all YAWL pattern types:

- **implicit_termination**: Start -> Active -> Work -> Terminate
- **multiple_instances_no_sync**: Pool -> Ready -> Done
- **multiple_instances_static**: Pool -> Ready -> Running -> Complete (All)
- **multiple_instances_runtime**: Eval -> Pool -> Running -> Complete
- **multiple_instances_dynamic**: Source -> Running -> Done
- **deferred_choice**: Start -> Options -> Selected
- **interleaved_routing**: Pool -> Next -> Executing -> Return
- **structured_loop**: Init -> Body -> Condition -> Exit
- **critical_section**: Request -> Lock -> Active -> Release

## Integration with YAWL Engine

To integrate telemetry with the YAWL engine:

```erlang
% In your workflow execution
{ok, SpanId} = yawl_telemetry:start_span(PatternType, CaseId),

% Record execution
ok = yawl_telemetry:record_execution_start(PatternType, CaseId),

% ... workflow execution ...

% Complete span
ok = yawl_telemetry:end_span(SpanId, {ok, Result}, ok),
ok = yawl_telemetry:record_execution_complete(PatternType, CaseId, Duration),
```

## Health Status Values

- **healthy**: All checks passing, normal operation
- **degraded**: Some issues but functioning (e.g., slow execution)
- **unhealthy**: Critical issues (e.g., errors, timeouts)
- **not_found**: Pattern doesn't exist or has no data

## W3C Trace Context Format

The module uses W3C traceparent format for distributed tracing:
```
00-traceId-spanId-flags
```

- `00`: Version
- `traceId`: 128-bit trace identifier (32 hex chars)
- `spanId`: 64-bit span identifier (16 hex chars)
- `flags`: 8-bit trace flags

## Testing

Run the test suite:
```bash
erlc -I ../include -o ../src ../test/yawl_telemetry_test.erl
erl -noshell -pa ../src -s yawl_telemetry_test run_tests -s init stop
```

## License

Apache 2.0
