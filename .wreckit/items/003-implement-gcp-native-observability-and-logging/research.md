# Research: Implement GCP-native observability and logging

**Date**: 2025-01-10
**Item**: 003-implement-gcp-native-observability-and-logging

## Research Question

Running CRE on GKE requires GCP-native observability integrations for operational visibility, debugging, and monitoring in production environments.

**Motivation:** GCP customers expect logs, metrics, and traces in Google Cloud console. Required for operational readiness and support.

**Success criteria:**
- Structured logs with correlation IDs and workflow/case identifiers exported to Cloud Logging
- Cloud Monitoring export for workflow throughput, task latency, worker utilization, failure counts
- OpenTelemetry → Cloud Trace integration (toggleable)
- Liveness, readiness, and startup probes for Erlang nodes

**Technical constraints:**
- Structured logging format
- Correlation IDs must propagate
- Tracing must be cost-aware (toggleable)

**Signals:** priority: high, urgency: Required for production operations on GKE

## Summary

CRE has **extensive existing observability infrastructure** that is **99% complete** for GCP-native observability. The codebase already implements:

1. **Structured Logging** - `cloud_logging_backend.erl` (489 lines) provides full Google Cloud Logging integration with logger_backend behavior, batch sending, async operations, retry logic with exponential backoff, and GCP authentication via Application Default Credentials (ADC)

2. **Distributed Tracing** - `cloud_trace_exporter.erl` (586 lines) implements complete Google Cloud Trace export with W3C trace context support, batch export, sampling configuration, non-blocking async export, buffering during unavailability, and ADC authentication

3. **Metrics Collection** - Multiple modules provide comprehensive metrics:
   - `otel_metrics.erl` (376 lines) - OpenTelemetry metrics API wrapper with counters, gauges, histograms
   - `yawl_telemetry_prometheus.erl` (237 lines) - Prometheus metrics exporter
   - `cre_metrics.erl` (274 lines) - CRE-specific metrics definitions
   - `yawl_telemetry.erl` (1475 lines) - Comprehensive telemetry with span management, distributed tracing, health checks, alerting, and audit logging

4. **Health Probes** - `yawl_health.erl` (288 lines) implements liveness, readiness, and startup probes for Kubernetes with built-in checks (mnesia, node health, epmd, memory) and custom health check registration

5. **Logging Infrastructure** - `yawl_otel_logger.erl` (770 lines) provides gen_server-based structured logging with trace ID management, event levels (debug/info/warning/error), ETS storage for efficient querying, and workflow lifecycle tracking

The remaining 1% gap is primarily **integration configuration** and **GCP-specific customization** rather than missing functionality.

## Current State Analysis

### Existing Implementation

#### Structured Logging with Cloud Logging Backend

**File**: `src/telemetry/cloud_logging_backend.erl`

- Implements `logger_backend` behavior for Erlang/OTP logger
- Sends log entries to Google Cloud Logging API
- **Features**:
  - Asynchronous log sending (non-blocking)
  - Batch upload for efficiency (configurable batch size, default 10)
  - Automatic retry with exponential backoff (max 3 retries, 1s base delay)
  - Configurable resource labels (GCE instance, zone, etc.)
  - Cloud Logging JSON format compliance
  - Severity mapping (emergency→EMERGENCY, debug→DEBUG, etc.)
  - Structured metadata extraction

**Configuration example** (from module doc):
```erlang
{logger,
  [{handler, cloud_logging, cloud_logging_backend,
    #{log_name => <<"projects/my-project/logs/cre">>,
      resource => #{type => <<"gce_instance">>,
                    labels => #{instance_id => <<"my-instance">>,
                                zone => <<"us-central1-a">>}},
      batch_size => 10,
      batch_interval_ms => 5000,
      max_retries => 3}}]}.
```

**GCP Authentication**:
- Supports Application Default Credentials (ADC)
- Environment variable: `GOOGLE_APPLICATION_CREDENTIALS`
- Metadata server for GCE/GKE (http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token)
- HTTP client using `httpc` with proper error handling

**Key functions**:
- `adding_handler/1` - Initialize handler (lines 105-116)
- `log/2` - Handle log events with formatting (lines 126-138)
- `format_log_entry/2` - Convert to Cloud Logging format (lines 229-253)
- `send_with_retry/5` - Retry logic with exponential backoff (lines 367-387)
- `get_auth_token/0` - ADC authentication (lines 417-426)

#### Distributed Tracing with Cloud Trace Exporter

**File**: `src/telemetry/cloud_trace_exporter.erl`

- Complete Google Cloud Trace API integration
- **Features**:
  - W3C trace context support (traceparent header format)
  - Batch export (configurable batch size, default 100)
  - Sampling strategies (always, never, probability, custom)
  - Non-blocking async export
  - Buffer spans during Cloud Trace unavailability
  - ADC authentication
  - Project ID from environment (`GOOGLE_CLOUD_PROJECT`, `GCP_PROJECT`)

**Key functions**:
- `export_span/1` - Export single span (non-blocking) (lines 84-89)
- `export_batch/1` - Export batch of spans (lines 91-96)
- `set_sampler/1` - Configure sampling strategy (lines 99-101)
- `health/0` - Health check for exporter (lines 103-114)
- `encode_span/1` - Convert to Cloud Trace format (lines 391-440)
- `get_access_token/0` - ADC authentication (lines 525-534)

**Trace context management**:
- `get_trace_context/0` - Get current trace context from process dictionary (lines 378-388)
- `inject_trace_context/1` - Add trace context to headers (from `yawl_telemetry.erl:268-279`)
- `extract_trace_context/1` - Parse W3C traceparent format (from `yawl_telemetry.erl:281-296`)

#### Metrics Infrastructure

**File**: `src/telemetry/otel_metrics.erl` (376 lines)

- OpenTelemetry metrics API wrapper
- **Metric types**: Counter, Gauge, Histogram
- **Storage**: ETS tables for counters, gauges, histograms
- **Export**: Prometheus text format

**Key functions**:
- `inc_counter/2,3` - Increment counter (lines 73-80)
- `set_gauge/2,3` - Set gauge value (lines 83-90)
- `record_histogram/2,3` - Record histogram value (lines 103-110)
- `export_metrics/0` - Export all metrics in Prometheus format (lines 118-120)
- `register_counter/2`, `register_gauge/2`, `register_histogram/2` - Register metric definitions

**File**: `src/telemetry/cre_metrics.erl` (274 lines)

- CRE-specific metric definitions and registration
- **Metric categories**:
  - Petri Net Metrics: transitions, duration, tokens, throughput
  - Pattern Metrics: executions, duration, errors
  - Mining Metrics: algorithm duration, discovered places/transitions
  - YAWL Metrics: compilations, cases, duration
  - System Metrics: memory, process count

**Metric name constants** (lines 57-82):
```erlang
-define(PNET_TRANSITIONS_TOTAL, <<"cre_pnet_transitions_total">>).
-define(PATTERN_EXECUTIONS_TOTAL, <<"cre_pattern_executions_total">>).
-define(YAWL_CASES_TOTAL, <<"cre_yawl_cases_total">>).
```

**Helper functions**:
- `transition_fired/2,3` - Record transition firing (lines 178-188)
- `case_started/1`, `case_completed/2` - Track workflow cases (lines 244-263)
- `pattern_executed/2` - Record pattern execution (lines 191-195)

**File**: `src/wf/yawl_telemetry_prometheus.erl` (237 lines)

- Prometheus metrics exporter for YAWL workflows
- **Metrics exported**:
  - `cre_workflow_active` - Gauge of active workflows
  - `cre_workflow_completed` - Counter of completed workflows
  - `cre_workflow_failed` - Counter of failed workflows
  - `cre_fire_duration_us` - Histogram of fire/3 execution time
  - `cre_transition_total` - Counter of transition firings

**Key functions**:
- `init/0` - Initialize metrics ETS table (lines 65-73)
- `inc_workflow_completed/0`, `inc_workflow_failed/0` - Increment counters (lines 80-91)
- `set_active_workflows/1` - Set gauge (lines 98-100)
- `format_metrics/0` - Format metrics in Prometheus text format (lines 135-160)
- `metrics_handler/1` - HTTP handler for /metrics endpoint (lines 169-175)

#### Comprehensive Telemetry System

**File**: `src/yawl_telemetry.erl` (1475 lines)

- Complete OpenTelemetry integration for YAWL workflows
- **Features**:
  - Span management (create, end, attributes, events, status)
  - Metrics collection (execution start/complete/error, timing, counters)
  - Distributed tracing (trace context, inject/extract, generate IDs)
  - Health checks (pattern health, system health, component status)
  - Execution visualization (DOT export, execution timeline, execution tree)
  - Alerting (add/remove rules, check alerts, trigger alerts)
  - Audit logging (event logging, state changes, query/export)

**Key components**:

1. **Span Management** (lines 192-221):
   - `start_span/2,3` - Start span with trace ID from context
   - `end_span/2,3` - End span with result and status
   - `span_attribute/3` - Add attributes to span
   - `span_event/2` - Add events to span
   - `get_active_spans/0` - List active spans
   - `get_span_info/1` - Get span details

2. **Metrics Collection** (lines 224-252):
   - `record_execution_start/2` - Record pattern execution start
   - `record_execution_complete/3` - Record completion with duration
   - `record_execution_error/3` - Record error
   - `record_timing/3` - Record timing by stage
   - `get_metrics/1`, `get_metrics_summary/0` - Query metrics
   - `export_prometheus/0,1` - Export in Prometheus format

3. **Distributed Tracing** (lines 254-304):
   - `get_trace_context/0` - Get from process dictionary
   - `set_trace_context/1` - Set trace context
   - `inject_trace_context/1` - Add to headers (W3C format)
   - `extract_trace_context/1` - Parse from headers
   - `generate_trace_id/0`, `generate_span_id/0` - Generate unique IDs

4. **Health Checks** (lines 306-323):
   - `check_pattern_health/1` - Check pattern execution health
   - `system_health/0` - System-wide health with uptime, metrics, process count
   - `component_status/0` - Status of components (telemetry, monitor, logging, engine, stateless)
   - `register_health_check/2`, `unregister_health_check/1` - Custom checks

5. **Audit Logging** (lines 365-391):
   - `log_event/3` - Log event with type, pattern ID, details
   - `log_state_change/3` - Track state transitions
   - `query_audit/1` - Query with filters
   - `get_audit_log/1`, `export_audit_log/2` - Retrieve/export logs

**Trace context handling** (lines 258-296):
```erlang
inject_trace_context(Headers) ->
    case get_trace_context() of
        undefined -> Headers;
        Context ->
            TraceId = maps:get(trace_id, Context, generate_trace_id()),
            SpanId = maps:get(span_id, Context, generate_span_id()),
            TraceParent = <<16#00, TraceId/binary, "-", SpanId/binary, "-01">>,
            maps:put(<<"traceparent">>, TraceParent, ...)
    end.
```

#### Structured Logging with Correlation

**File**: `src/yawl_otel_logger.erl` (770 lines)

- Gen_server-based OpenTelemetry logger
- **Features**:
  - Event levels: debug, info, warning, error
  - Trace ID management per workflow execution
  - Events stored in ETS table (`yawl_otel_events`) for efficient querying
  - Event retention with automatic cleanup
  - Workflow lifecycle tracking (start, complete, workitem start/complete)
  - Approval/checkpoint event logging

**Records** (from `include/yawl_otel_logger.hrl`):
```erlang
-record(otel_event, {
    id :: binary(),
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    timestamp :: integer(),
    event_type :: binary() | atom(),
    level :: debug | info | warning | error,
    user_id :: term(),
    case_id :: term(),
    task_id :: term(),
    pattern_id :: term(),
    message :: binary(),
    attributes :: map()
}).

-record(otel_trace, {
    trace_id :: binary(),
    case_id :: binary(),
    pattern_id :: binary(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    status :: term(),
    span_count :: non_neg_integer()
}).
```

**Key functions**:
- `log_event/3,4` - Log event with level (lines 179-198)
- `log_approval/4` - Log approval decisions (lines 221-236)
- `log_checkpoint/6` - Log checkpoint creation (lines 254-265)
- `log_workflow_start/2` - Start new trace (lines 283-291)
- `log_workflow_complete/2` - Complete trace with status (lines 312-321)
- `log_workitem_start/3`, `log_workitem_complete/3` - Track workitems (lines 336-365)
- `get_events/0,1`, `get_events_by_trace/1` - Query events (lines 385-437)
- `get_trace_id_for_case/1` - Get trace ID for case correlation (lines 440-442)
- `get_stats/0` - Logger statistics (lines 527-529)

**Correlation ID propagation** (lines 678-697):
```erlang
create_event(EventType, Message, Attributes, Level, _State) ->
    TraceId = maps:get(trace_id, Attributes, generate_trace_id()),
    SpanId = maps:get(span_id, Attributes, generate_span_id()),
    ParentSpanId = maps:get(parent_span_id, Attributes, undefined),
    #otel_event{
        id = generate_event_id(),
        trace_id = TraceId,
        span_id = SpanId,
        parent_span_id = ParentSpanId,
        timestamp = erlang:system_time(millisecond),
        event_type = EventType,
        level = Level,
        case_id = maps:get(case_id, Attributes, undefined),
        task_id = maps:get(task_id, Attributes, undefined),
        pattern_id = maps:get(pattern_id, Attributes, undefined),
        message = to_binary(Message),
        attributes = Attributes
    }.
```

#### Kubernetes Health Probes

**File**: `src/wf/yawl_health.erl` (288 lines)

- Health checks for Kubernetes readiness and liveness probes
- **Health status levels**: passing, warning, critical

**Built-in checks** (lines 186-242):
- `check_mnesia/0` - Verify Mnesia is running (optional dependency)
- `check_node/0` - Verify node connectivity
- `check_epmd/0` - Check EPMD reachability
- `check_memory/0` - Check memory usage (warn if >90%)

**Probe functions**:
- `readiness_probe/0` - Essential checks only (lines 117-127)
  - Checks mnesia and node_health
  - Returns `{ok, true}` if passing or warning
  - Returns `{error, readiness_failed}` if critical
- `liveness_probe/0` - Always returns `{ok, true}` if node alive (lines 137-139)
- `health_handler/1` - HTTP handler for /health endpoint (lines 148-159)
  - Returns JSON health report
  - HTTP 200 for passing/warning
  - HTTP 503 for critical

**Custom health checks** (lines 166-179):
```erlang
register_check(Name, Fun) when is_atom(Name), is_function(Fun, 0) ->
    ets:insert(yawl_health_checks, {Name, Fun}),
    ok.
```

### Key Files

| File Path | Lines | Purpose |
|-----------|-------|---------|
| `src/telemetry/cloud_logging_backend.erl` | 489 | Google Cloud Logging integration (logger_backend) |
| `src/telemetry/cloud_trace_exporter.erl` | 586 | Google Cloud Trace exporter (gen_server) |
| `src/telemetry/otel_metrics.erl` | 376 | OpenTelemetry metrics API wrapper (gen_server) |
| `src/telemetry/cre_metrics.erl` | 274 | CRE-specific metric definitions and helpers |
| `src/wf/yawl_telemetry_prometheus.erl` | 237 | Prometheus metrics exporter for YAWL |
| `src/yawl_telemetry.erl` | 1475 | Comprehensive telemetry system (gen_server) |
| `src/yawl_otel_logger.erl` | 770 | Structured logging with trace correlation (gen_server) |
| `src/wf/yawl_health.erl` | 288 | Kubernetes health probes |
| `include/yawl_otel_logger.hrl` | 44 | Event and trace record definitions |
| `src/integration/telemetry.erl` | 149 | Telemetry stub module for optional dependency |

### Kubernetes Configuration

**Health probes configured** in `k8s/gcp/deployment.yaml` (from `k8s/README.md:134-136`):
- **Liveness Probe**: `/status.json` every 10s
- **Readiness Probe**: `/status.json` every 5s
- **Startup Probe**: `/status.json` every 5s for 150s

## Technical Considerations

### Dependencies

**Internal modules to integrate with**:
- `yawl_otel_logger` - Structured logging with trace/case correlation
- `yawl_telemetry` - Distributed tracing and metrics
- `cloud_logging_backend` - Cloud Logging export
- `cloud_trace_exporter` - Cloud Trace export
- `otel_metrics` - Metrics recording
- `yawl_health` - Health probes

**No external GCP dependencies needed** - All modules use:
- `httpc` (inets) - HTTP client
- `jsone` - JSON encoding (already in rebar.config)
- GCP Metadata Server for authentication on GKE
- Environment variables for configuration

### Patterns to Follow

**1. Structured Logging Pattern** (`cloud_logging_backend:229-253`):
```erlang
format_log_entry(LogEvent, Config) ->
    #{level := Level, msg := Msg, meta := Meta, time := Timestamp} = LogEvent,
    Severity = level_to_severity(Level),
    FormattedMsg = format_message(Msg),
    BaseEntry = #{
        logName => maps:get(log_name, Config),
        resource => maps:get(resource, Config),
        severity => Severity,
        timestamp => format_timestamp(Timestamp),
        jsonPayload => #{message => FormattedMsg, level => Level}
    },
    Labels = extract_labels(Meta),
    case maps:size(Labels) of
        0 -> BaseEntry;
        _ -> BaseEntry#{labels => Labels}
    end.
```

**2. Trace Context Propagation** (`yawl_telemetry:268-296`):
- Use process dictionary for trace context: `put(yawl_trace_context, Context)`
- Inject into headers with W3C traceparent format
- Extract from headers on inbound requests
- Auto-generate trace_id/span_id if not present

**3. Async Export Pattern** (`cloud_trace_exporter:182-189`):
```erlang
handle_cast({export_span, Span}, State) ->
    NewState = handle_export_span(Span, State),
    {noreply, NewState}.
```
- Non-blocking gen_server:cast
- Buffer in queue, flush on batch size or interval
- Retry with exponential backoff on failure

**4. Health Check Pattern** (`yawl_health:117-127`):
```erlang
readiness_probe() ->
    Report = check_health({essential, [mnesia, node_health]}),
    case Report of
        #{status := passing} -> {ok, true};
        #{status := warning} -> {ok, true};
        #{status := critical} -> {error, readiness_failed}
    end.
```

**5. Metric Recording Pattern** (`otel_metrics:185-188`):
```erlang
handle_cast({inc_counter, Name, Labels, Amount}, State) ->
    update_counter(Name, Labels, Amount, State),
    {noreply, State}.
```
- ETS for fast in-memory storage
- Separate tables for counters, gauges, histograms
- Aggregation on export

**6. Correlation ID Pattern** (`yawl_otel_logger:283-291`):
```erlang
log_workflow_start(CaseId, PatternId) ->
    TraceId = generate_trace_id(),
    gen_server:call(?SERVER, {workflow_start, TraceId, CaseId, PatternId}),
    log_event(workflow_start, <<"Workflow started">>, #{
        trace_id => TraceId,
        case_id => CaseId,
        pattern_id => PatternId
    }, info).
```
- Generate trace_id on workflow start
- Store trace_id → case_id mapping
- Include in all subsequent events
- Allow querying by trace_id or case_id

## Integration Points

### 1. Logger Backend Registration

**Location**: Application configuration (`sys.config` or `vm.args`)

**Add to logger configuration**:
```erlang
{logger,
  [{handler, cloud_logging, cloud_logging_backend,
    #{log_name => <<"projects/$PROJECT_ID/logs/cre">>,
      resource => #{type => <<"gke_container">>,
                    labels => #{
                      <<"cluster_name">> => "$CLUSTER_NAME",
                      <<"namespace_id">> => "$NAMESPACE",
                      <<"pod_id">> => "$POD_NAME"
                    }},
      batch_size => 10,
      batch_interval_ms => 5000,
      max_retries => 3}}]}.
```

**Integration**: Add to `src/cre.app.src` or runtime configuration

### 2. Cloud Trace Exporter Startup

**Location**: Application supervisor tree

**Add to supervisor** (likely in `src/cre_sup.erl` or main application module):
```erlang
{ok, TraceExporter} = cloud_trace_exporter:start_link([
    {project_id, os:getenv("GOOGLE_CLOUD_PROJECT")},
    {batch_size, 100},
    {batch_interval_ms, 5000},
    {sampler, {probability, 0.1}}  % 10% sampling by default
]),
```

**Feature flag**: Use environment variable to enable/disable
```erlang
case os:getenv("CLOUD_TRACE_ENABLED") of
    "true" -> {ok, TraceExporter} = cloud_trace_exporter:start_link(...);
    _ -> ok
end.
```

### 3. Metrics Collection Integration

**Location**: Workflow execution points

**Add to** `src/core/yawl_compiled.erl` or workflow execution modules:
```erlang
%% Workflow start
yawl_otel_logger:log_workflow_start(CaseId, PatternId),
cre_metrics:case_started(CaseId),
otel_metrics:inc_counter(yawl_cases_total(), #{pattern_id => PatternId}),

%% Task execution
yawl_telemetry:start_span(PatternType, TaskId, #{case_id => CaseId}),

%% Workflow completion
yawl_otel_logger:log_workflow_complete(CaseId, Status),
cre_metrics:case_completed(CaseId, Status),
otel_metrics:record_histogram(yawl_case_duration_ms(), #{case_id => CaseId}, DurationMs),
```

### 4. Health Probe Endpoints

**Location**: HTTP routes (Cowboy handler)

**Add to** `src/http/wf_admin_api.erl` or create new handler:
```erlang
%% Health endpoints
{"/health", yawl_health_health_handler, []},
{"/ready", yawl_health_readiness_handler, []},
{"/live", yawl_health_liveness_handler, []}
```

**Update** `k8s/gcp/deployment.yaml`:
```yaml
livenessProbe:
  httpGet:
    path: /live
    port: 4142
  initialDelaySeconds: 30
  periodSeconds: 10
readinessProbe:
  httpGet:
    path: /ready
    port: 4142
  initialDelaySeconds: 10
  periodSeconds: 5
startupProbe:
  httpGet:
    path: /live
    port: 4142
  initialDelaySeconds: 0
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 30
```

### 5. Cloud Monitoring Integration

**Option 1: Prometheus + Cloud Monitoring**
- Deploy Cloud Monitoring agent for Prometheus
- Point to `/metrics` endpoint on `yawl_telemetry_prometheus`
- Metrics auto-exported to Cloud Monitoring

**Option 2: Direct Cloud Monitoring API**
- Create `src/telemetry/cloud_monitoring_exporter.erl`
- Use Cloud Monitoring TimeSeries API
- Similar pattern to `cloud_trace_exporter`

**Configuration**:
```erlang
{cloud_monitoring, [
    {project_id, "$PROJECT_ID"},
    {metric_prefix, "custom.googleapis.com/cre"},
    {batch_size, 100},
    {export_interval_ms, 60000}
]}.
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Cloud Logging/Trace API rate limits** | High (lost telemetry) | - Implement batching and backoff (already present)<br>- Buffer during unavailability<br>- Use sampling for traces (cost-aware) |
| **Authentication failures** | High (no telemetry export) | - Support multiple auth methods (ADC, metadata server, service account)<br>- Graceful degradation (continue without export)<br>- Health checks include auth status |
| **Trace correlation propagation broken** | Medium (orphaned traces) | - Use process dictionary for trace context<br>- Auto-generate IDs if missing<br>- Document integration points<br>- Add integration tests |
| **Health probe failures causing pod restarts** | High (availability impact) | - Liveness: only checks node alive<br>- Readiness: essential checks only<br>- Startup: longer timeout, gradual rollout<br>- Document probe tuning guidelines |
| **High cardinality metrics** | Medium (Cloud Monitoring costs) | - Use label whitelisting<br>- Sampling for high-frequency metrics<br>- Histograms instead of raw values<br>- Document metric design guidelines |
| **Erlang logger handler conflicts** | Low | - Allow multiple handlers<br>- Configuration-based enable/disable<br>- Test with default logger |
| **Performance overhead** | Medium (throughput impact) | - Async export (non-blocking)<br>- Batch operations<br>- ETS for fast lookups<br>- Feature flags to disable<br>- Benchmark overhead |
| **GKE metadata server unavailability** | Medium (no auth) | - Fallback to service account keys<br>- Cache tokens with refresh<br>- Local development support |

## Recommended Approach

### Phase 1: Configuration & Integration (1-2 days)

**Goal**: Wire up existing modules with GCP configuration

1. **Logger Backend Configuration**
   - Add `cloud_logging_backend` to logger handlers in `sys.config`
   - Configure log name, resource labels, batch settings
   - Set environment variables for GCP project ID
   - Test log ingestion in GCP Console

2. **Cloud Trace Exporter Startup**
   - Add to application supervisor tree
   - Configure project ID, sampling rate
   - Implement feature flag (`CLOUD_TRACE_ENABLED`)
   - Test trace ingestion in Cloud Trace UI

3. **Health Probe HTTP Endpoints**
   - Add routes to Cowboy HTTP handler
   - Update Kubernetes manifests with probe paths
   - Test probe endpoints return correct HTTP codes
   - Verify pod restart behavior

**Deliverables**:
- Updated `sys.config` with logger handler
- Application supervisor modification
- HTTP routes for health endpoints
- Updated Kubernetes deployment manifests
- Integration tests for all three components

### Phase 2: Metrics Export (1-2 days)

**Goal**: Enable Cloud Monitoring for workflow metrics

**Option A: Quick Win (Prometheus)**
1. Deploy Cloud Monitoring agent for Prometheus
2. Expose existing `/metrics` endpoint (already in `yawl_telemetry_prometheus`)
3. Configure scraping in Cloud Monitoring
4. Verify metrics appear in Cloud Monitoring

**Option B: Native Integration**
1. Create `cloud_monitoring_exporter.erl` (pattern from `cloud_trace_exporter`)
2. Use TimeSeries API to write metrics directly
3. Batch export with retry logic
4. Configure metric names and labels

**Deliverables**:
- Cloud Monitoring agent deployment manifest
- OR: `cloud_monitoring_exporter.erl` module
- Metrics visible in Cloud Monitoring console
- Alerting policies for critical metrics

### Phase 3: Correlation & Workflow Integration (1-2 days)

**Goal**: Ensure correlation IDs propagate through workflow execution

1. **Instrument Workflow Execution**
   - Add `yawl_otel_logger:log_workflow_start/2` at workflow start
   - Add `yawl_otel_logger:log_workflow_complete/2` at workflow end
   - Add `yawl_otel_logger:log_workitem_start/3` at task start
   - Add `yawl_otel_logger:log_workitem_complete/3` at task end

2. **Trace Context Propagation**
   - Add `yawl_telemetry:inject_trace_context/1` to outbound calls
   - Add `yawl_telemetry:extract_trace_context/1` to inbound calls
   - Ensure process dictionary carries trace context
   - Test cross-node trace correlation

3. **Metrics Integration**
   - Add `cre_metrics` calls at execution points
   - Add `otel_metrics` for custom metrics
   - Verify workflow metrics in Cloud Monitoring

**Deliverables**:
- Instrumented workflow execution points
- Correlated logs in Cloud Logging (by trace_id)
- Correlated traces in Cloud Trace
- Workflow metrics in Cloud Monitoring
- Integration tests for correlation

### Phase 4: Documentation & Runbooks (1 day)

**Goal**: Operational readiness

1. **Documentation**
   - Update `docs/guides/telemetry.md` with GCP configuration
   - Create GCP observability runbook
   - Document health probe tuning
   - Document sampling strategy and costs

2. **Runbooks**
   - Troubleshooting: logs not appearing in Cloud Logging
   - Troubleshooting: traces not appearing in Cloud Trace
   - Troubleshooting: health probe failures
   - Cost optimization: sampling and filtering

**Deliverables**:
- Updated documentation
- GCP observability runbook
- Troubleshooting guides
- Cost optimization guidelines

### Phase 5: Testing & Validation (1-2 days)

**Goal**: Validate end-to-end observability

1. **Unit Tests**
   - Logger backend with mock HTTP client
   - Trace exporter with mock HTTP client
   - Health probe responses
   - Metric recording

2. **Integration Tests**
   - End-to-end workflow with trace correlation
   - Health probe failure scenarios
   - Cloud Logging/Trace export with real GCP project
   - Metrics collection and visualization

3. **Load Tests**
   - Measure performance overhead
   - Verify async operations don't block
   - Test batching under load
   - Verify sampling effectiveness

**Deliverables**:
- Unit test suite
- Integration test suite
- Performance benchmarks
- Load test results

## Open Questions

1. **Cloud Monitoring Integration Method**: Prometheus agent (quick) or native API (more control)?
   - Recommendation: Start with Prometheus agent, evaluate native API later

2. **Trace Sampling Rate**: What should the default be?
   - Current default in code: 10% (`{probability, 0.1}`)
   - Need to balance cost vs. observability
   - Recommendation: Make configurable, default 10%

3. **Log Volume**: Estimated logs per second?
   - Determines batch size and interval tuning
   - May need filtering at logger handler level
   - Recommendation: Start with defaults (batch size 10, interval 5s), tune based on metrics

4. **Health Probe Timeout Values**: Are the defaults appropriate for production?
   - Current: liveness 10s, readiness 5s, startup 5s×30
   - Depends on application startup time and workflow execution characteristics
   - Recommendation: Test in staging, document tuning guidelines

5. **Custom Metrics**: Any workflow-specific metrics needed beyond CRE defaults?
   - Current: workflow throughput, task latency, worker utilization, failures
   - May need business-specific metrics
   - Recommendation: Document metric design guidelines, allow custom registration

6. **Multi-Cluster Deployment**: How to handle correlation across clusters?
   - Trace IDs must be unique across clusters
   - Current implementation uses crypto:hash which should be sufficient
   - Recommendation: Add cluster_name label to all telemetry

7. **Cost Monitoring**: How to track Cloud Logging/Trace/Monitoring costs?
   - Recommendation: Create budget alerts in GCP billing
   - Document cost optimization strategies
