# CRE Monitoring Implementation Guide

**Target:** Implement missing metrics to enable full monitoring dashboard functionality
**Status:** Specification ready for implementation
**Priority:** HIGH - Required for GCP Marketplace submission

---

## Overview

The monitoring dashboards and alert policies reference 33 unique metrics. Currently:
- ✅ 7 metrics available (Kubernetes native metrics from GKE)
- ✅ 2 metrics available (from autoscaling_metrics.erl)
- ⚠️ 2 metrics partially available (format/unit conversion needed)
- ❌ 16 metrics not implemented (critical gaps)
- ❌ 6 metrics not yet designed (need specification)

This guide specifies the missing metrics and provides implementation guidance.

---

## Missing Metrics by Category

### Category 1: Workflow Execution Metrics (4 metrics)

**Module:** `src/telemetry/cre_metrics.erl` (existing module)
**Priority:** CRITICAL - Required by 5+ dashboards and alert policies

#### 1.1 Workflow Completed Counter

**Name (Prometheus):** `cre_workflow_completed_total`
**Name (GCP):** `workload.googleapis.com/cre/workflow/completed_total`
**Type:** Counter (incremental)
**Unit:** Count
**Labels:**
- `status` (optional) - "success" | "failed" | "timeout"
- `workflow_type` (optional) - type of workflow

**Description:** Total number of completed workflows (success and failure combined)

**Collection Points:**
- gen_yawl: when workflow transitions to terminal state
- yawl_case: case completion handler

**Implementation:**
```erlang
-define(WORKFLOW_COMPLETED_TOTAL, <<"cre_workflow_completed_total">>).

workflow_completed(Status) ->
    Labels = #{status => Status},
    otel_metrics:inc_counter(workflow_completed_total(), Labels).

workflow_completed_total() -> ?WORKFLOW_COMPLETED_TOTAL.
```

**Used By:**
- Workflow Throughput widget (scorecard)
- Workflow Completion Rate widget (xyChart)
- Workflow Success Rate widget (xyChart)
- Low Throughput alert
- Workflow Success Rate alert

---

#### 1.2 Workflow Failed Counter

**Name (Prometheus):** `cre_workflow_failed_total`
**Name (GCP):** `workload.googleapis.com/cre/workflow/failed_total`
**Type:** Counter (incremental)
**Unit:** Count
**Labels:**
- `failure_reason` (optional) - "timeout" | "error" | "cancelled"
- `workflow_type` (optional)

**Description:** Total number of failed/terminated workflows

**Collection Points:**
- gen_yawl: on workflow failure
- Error handlers in YAWL execution engine

**Implementation:**
```erlang
-define(WORKFLOW_FAILED_TOTAL, <<"cre_workflow_failed_total">>).

workflow_failed(FailureReason) ->
    Labels = #{failure_reason => FailureReason},
    otel_metrics:inc_counter(workflow_failed_total(), Labels).

workflow_failed_total() -> ?WORKFLOW_FAILED_TOTAL.
```

**Used By:**
- Workflow Completion Rate widget
- Workflow Success Rate widget
- Workflow Success Rate alert

---

#### 1.3 Workflow Error Rate Gauge

**Name (Prometheus):** `cre_workflow_error_rate`
**Name (GCP):** `workload.googleapis.com/cre/workflow/error_rate`
**Type:** Gauge (current value)
**Unit:** Fraction (0.0-1.0, representing percentage)
**Labels:** None

**Description:** Current error rate as fraction of failed/(completed + failed)

**Collection Points:**
- Calculated periodically from completed and failed counters
- Update every 60 seconds

**Implementation:**
```erlang
-define(WORKFLOW_ERROR_RATE, <<"cre_workflow_error_rate">>).

update_workflow_error_rate() ->
    case otel_metrics:get_metric(workflow_completed_total(), #{}) of
        {ok, Completed} ->
            case otel_metrics:get_metric(workflow_failed_total(), #{}) of
                {ok, Failed} ->
                    Total = Completed + Failed,
                    ErrorRate = case Total of
                        0 -> 0.0;
                        _ -> Failed / Total
                    end,
                    otel_metrics:set_gauge(workflow_error_rate(), #{}, ErrorRate);
                _ -> ok
            end;
        _ -> ok
    end.
```

**Alert Thresholds:**
- WARNING: > 5% (0.05)
- CRITICAL: > 15% (0.15)

---

#### 1.4 Workflow Latency Histogram

**Name (Prometheus):** `cre_workflow_latency_ms`
**Name (GCP):** `workload.googleapis.com/cre/workflow/latency`
**Type:** Histogram (distribution)
**Unit:** Seconds (dashboard expects seconds, convert from ms)
**Labels:**
- `workflow_type` (optional)
- `status` (optional) - "success" | "failed"

**Description:** Workflow execution latency from start to completion

**Collection Points:**
- gen_yawl: record start time when workflow begins
- gen_yawl: record completion when workflow terminates
- Calculate duration on completion

**Implementation:**
```erlang
-define(WORKFLOW_LATENCY_MS, <<"cre_workflow_latency_ms">>).

workflow_started(CaseId, WorkflowType) ->
    put({workflow_start, CaseId}, erlang:monotonic_time(millisecond)),
    put({workflow_type, CaseId}, WorkflowType).

workflow_completed(CaseId, Status) ->
    case get({workflow_start, CaseId}) of
        undefined -> ok;
        StartTime ->
            DurationMs = erlang:monotonic_time(millisecond) - StartTime,
            WorkflowType = get({workflow_type, CaseId}),
            Labels = #{
                workflow_type => WorkflowType,
                status => Status
            },
            otel_metrics:record_histogram(workflow_latency_ms(), Labels, DurationMs),
            erase({workflow_start, CaseId}),
            erase({workflow_type, CaseId})
    end.
```

**Alert Thresholds:**
- WARNING: p95 > 30 seconds
- CRITICAL: n/a (monitoring only)

---

### Category 2: Mining Algorithm Metrics (2 metrics)

**Module:** `src/telemetry/cre_metrics.erl` (extend existing module)
**Priority:** HIGH - Used by Mining Throughput dashboard

#### 2.1 Mining Events Processed Counter

**Name (Prometheus):** `cre_mining_events_processed_total`
**Name (GCP):** `workload.googleapis.com/cre/mining/events_processed_total`
**Type:** Counter (incremental)
**Unit:** Count
**Labels:**
- `algorithm` - name of mining algorithm (e.g., "alpha", "inductive")
- `status` (optional) - "success" | "error"

**Description:** Total number of events processed by mining algorithms

**Collection Points:**
- mining module: for each event processed
- Update with batch size to reduce overhead

**Implementation:**
```erlang
mining_events_processed(Algorithm, Count) ->
    Labels = #{algorithm => Algorithm},
    otel_metrics:inc_counter(mining_events_processed_total(), Labels, Count).
```

**Used By:**
- Mining Throughput widget
- Models Discovered widget (indirectly - as completion metric)

---

#### 2.2 Mining Models Discovered Counter

**Name (Prometheus):** `cre_mining_models_discovered_total`
**Name (GCP):** `workload.googleapis.com/cre/mining/models_discovered_total`
**Type:** Counter (incremental)
**Unit:** Count
**Labels:**
- `algorithm` - mining algorithm used

**Description:** Total number of distinct process models discovered by mining

**Collection Points:**
- mining completion handler
- Update when new model is discovered

**Implementation:**
```erlang
mining_model_discovered(Algorithm) ->
    Labels = #{algorithm => Algorithm},
    otel_metrics:inc_counter(mining_models_discovered_total(), Labels).
```

**Used By:**
- Models Discovered scorecard widget
- Mining algorithm evaluation dashboard

---

### Category 3: Health Check Metric (1 metric)

**Module:** `src/api/cre_health.erl` (new export)
**Priority:** HIGH - Required for Service Down alert

#### 3.1 Health Check Status Gauge

**Name (Prometheus):** `cre_health_check`
**Name (GCP):** `workload.googleapis.com/cre/health/check`
**Type:** Gauge
**Unit:** 0 (unhealthy) or 1 (healthy)
**Labels:** None

**Description:** Current health status of CRE service (1=healthy, 0=unhealthy)

**Collection Points:**
- Health check endpoint: `/health`
- Update every 30 seconds
- Aggregate results from multiple checks

**Implementation:**
```erlang
% In cre_health.erl module
-define(HEALTH_CHECK, <<"cre_health_check">>).

update_health_status() ->
    Status = case perform_health_check() of
        {healthy, _Details} -> 1;
        {unhealthy, _Reason} -> 0
    end,
    otel_metrics:set_gauge(health_check(), #{}, Status).

health_check() -> ?HEALTH_CHECK.

% Health check should verify:
% - Mnesia is reachable and responding
% - gen_yawl supervisor is running
% - Message queues are not overflowing
% - Memory usage is within limits
```

**Alert Configuration:**
- CRITICAL: health_check < 1.0 (unhealthy) for 2 minutes

**Used By:**
- Service Down alert
- No Metrics Received alert (as secondary check)

---

### Category 4: Erlang VM Metrics (9 metrics)

**Module:** New module `src/telemetry/erlang_vm_metrics.erl`
**Priority:** HIGH - Required by Erlang VM Dashboard

#### 4.1 Memory Breakdown Metrics

These metrics provide insight into memory allocation across different components.

**Implementation Pattern:**
```erlang
-module(erlang_vm_metrics).
-behaviour(gen_server).

% Metric names
-define(MEMORY_ATOM, <<"erlang/memory/atom">>).
-define(MEMORY_BINARY, <<"erlang/memory/binary">>).
-define(MEMORY_CODE, <<"erlang/memory/code">>).
-define(MEMORY_ETS, <<"erlang/memory/ets">>).
-define(MEMORY_PROCESSES, <<"erlang/memory/processes">>).
-define(MEMORY_SYSTEM, <<"erlang/memory/system">>).

% Periodic collection (every 30 seconds)
update_memory_metrics() ->
    case erlang:memory() of
        MemoryData when is_list(MemoryData) ->
            Atom = proplists:get_value(atom, MemoryData, 0),
            Binary = proplists:get_value(binary, MemoryData, 0),
            Code = proplists:get_value(code, MemoryData, 0),
            Ets = proplists:get_value(ets, MemoryData, 0),
            Processes = proplists:get_value(processes, MemoryData, 0),
            System = proplists:get_value(system, MemoryData, 0),

            otel_metrics:set_gauge(memory_atom(), #{}, Atom),
            otel_metrics:set_gauge(memory_binary(), #{}, Binary),
            otel_metrics:set_gauge(memory_code(), #{}, Code),
            otel_metrics:set_gauge(memory_ets(), #{}, Ets),
            otel_metrics:set_gauge(memory_processes(), #{}, Processes),
            otel_metrics:set_gauge(memory_system(), #{}, System);
        _ ->
            ok
    end.
```

**Metrics (6 total):**

| Name | Type | Unit | Description |
|------|------|------|-------------|
| `cre_erlang_memory_atom` | Gauge | Bytes | Atom table memory |
| `cre_erlang_memory_binary` | Gauge | Bytes | Binary data memory |
| `cre_erlang_memory_code` | Gauge | Bytes | Code memory (loaded modules) |
| `cre_erlang_memory_ets` | Gauge | Bytes | ETS table memory |
| `cre_erlang_memory_processes` | Gauge | Bytes | Process heap memory |
| `cre_erlang_memory_system` | Gauge | Bytes | System allocator memory |

**Used By:**
- Memory Breakdown widget (stacked area chart showing all 6)

---

#### 4.2 Garbage Collection Metrics

**Implementation:**
```erlang
update_gc_metrics() ->
    case erlang:statistics(garbage_collection) of
        {GCCount, WordsReclaimed, _0} ->
            otel_metrics:set_gauge(gc_count(), #{}, GCCount),
            otel_metrics:set_gauge(gc_words_reclaimed(), #{}, WordsReclaimed);
        _ ->
            ok
    end.

% GC pause time collected from scheduler wall time
update_gc_pause_time() ->
    case erlang:statistics(wall_clock) of
        {Total, Delta} ->
            % Estimate pause time from wall clock delta
            % This is approximate; exact pause time requires instrumentation
            otel_metrics:record_histogram(gc_pause_seconds(), #{}, Delta / 1000);
        _ ->
            ok
    end.
```

**Metrics (3 total):**

| Name | Type | Unit | Description |
|------|------|------|-------------|
| `cre_erlang_gc_count` | Gauge | Count | Total GC collections |
| `cre_erlang_gc_words_reclaimed` | Gauge | Words | Memory words reclaimed by GC |
| `cre_erlang_gc_pause_seconds` | Histogram | Seconds | GC pause duration |

**Alert Thresholds:**
- WARNING: gc_count > 100 collections/sec (indicates memory pressure)

---

#### 4.3 Scheduler Metrics

**Implementation:**
```erlang
update_scheduler_metrics() ->
    case erlang:statistics(scheduler_wall_time) of
        Stats when is_list(Stats) ->
            % Each scheduler: {Id, ActiveTime, TotalTime}
            lists:foreach(fun({Id, Active, Total}) ->
                case Total of
                    0 -> Utilization = 0.0;
                    _ -> Utilization = (Active / Total) * 100
                end,
                Labels = #{scheduler_id => Id},
                otel_metrics:set_gauge(scheduler_utilization(), Labels, Utilization)
            end, Stats);
        _ ->
            ok
    end.
```

**Metrics (1 total):**

| Name | Type | Unit | Labels | Description |
|------|------|------|--------|-------------|
| `cre_erlang_scheduler_utilization` | Gauge | Percentage | scheduler_id | Per-scheduler CPU utilization |

**Example Labels:**
- `{scheduler_id => 1}` - Scheduler 1 utilization
- `{scheduler_id => 2}` - Scheduler 2 utilization
- etc.

---

### Category 5: Mnesia Database Metrics (5 metrics)

**Module:** New module `src/telemetry/mnesia_metrics.erl`
**Priority:** HIGH - Required for Mnesia monitoring

#### 5.1 Mnesia Table Size

**Implementation:**
```erlang
-define(MNESIA_TABLE_SIZE, <<"erlang/mnesia/table_size">>).

update_mnesia_table_sizes() ->
    case application:which_applications() of
        Apps when is_list(Apps) ->
            case lists:keyfind(mnesia, 1, Apps) of
                {mnesia, _, _} ->
                    Tables = mnesia:system_info(tables),
                    lists:foreach(fun(Table) ->
                        case mnesia:table_info(Table, size) of
                            Size when is_integer(Size) ->
                                Labels = #{table_name => atom_to_binary(Table, utf8)},
                                otel_metrics:set_gauge(mnesia_table_size(), Labels, Size);
                            _ ->
                                ok
                        end
                    end, Tables);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.
```

**Metric:**
- Name: `cre_erlang_mnesia_table_size`
- Type: Gauge
- Unit: Count (number of records)
- Labels: `table_name` (e.g., "yawl_case", "yawl_task")

---

#### 5.2 Mnesia Transaction Metrics

**Implementation:**
```erlang
-define(MNESIA_TRANSACTIONS_COMMITTED, <<"erlang/mnesia/transactions_committed">>).
-define(MNESIA_TRANSACTIONS_FAILED, <<"erlang/mnesia/transactions_failed">>).
-define(MNESIA_TRANSACTIONS_ABORTED, <<"erlang/mnesia/transactions_aborted">>).

update_mnesia_transaction_metrics() ->
    case mnesia:table_info(schema, access_module) of
        mnesia_tm ->
            % mnesia_tm tracks transaction statistics
            CommitCount = mnesia_tm:commits(),
            FailCount = mnesia_tm:failures(),
            AbortCount = mnesia_tm:aborts(),

            otel_metrics:set_gauge(
                mnesia_transactions_committed(), #{}, CommitCount
            ),
            otel_metrics:set_gauge(
                mnesia_transactions_failed(), #{}, FailCount
            ),
            otel_metrics:set_gauge(
                mnesia_transactions_aborted(), #{}, AbortCount
            );
        _ ->
            ok
    end.
```

**Metrics (3 total):**

| Name | Type | Unit | Description |
|------|------|------|-------------|
| `cre_erlang_mnesia_transactions_committed` | Gauge | Count | Successful transactions |
| `cre_erlang_mnesia_transactions_failed` | Gauge | Count | Failed transactions |
| `cre_erlang_mnesia_transactions_aborted` | Gauge | Count | Aborted transactions |

**Alert Thresholds:**
- WARNING: (failed + aborted) / (committed + failed + aborted) > 1% (0.01)

---

#### 5.3 Mnesia Partition Detection

**Implementation:**
```erlang
-define(MNESIA_PARTITIONED_NODES, <<"erlang/mnesia/partitioned_nodes">>).

update_mnesia_partition_status() ->
    case mnesia:system_info(running_db_nodes) of
        Nodes when is_list(Nodes) ->
            % Check for network partitions
            PartitionedCount = count_partitioned_nodes(Nodes),
            otel_metrics:set_gauge(
                mnesia_partitioned_nodes(), #{}, PartitionedCount
            );
        _ ->
            ok
    end.

count_partitioned_nodes(Nodes) ->
    % Implementation: check connectivity between nodes
    % Mnesia detects partitions via failed synchronization
    lists:foldl(fun(Node, Acc) ->
        case mnesia:table_info(schema, subscribers) of
            Subscribers when is_list(Subscribers) ->
                case lists:member(Node, Subscribers) of
                    true -> Acc;
                    false -> Acc + 1  % Node is partitioned
                end;
            _ ->
                Acc
        end
    end, 0, Nodes).
```

**Metric:**
- Name: `cre_erlang_mnesia_partitioned_nodes`
- Type: Gauge
- Unit: Count (number of partitioned nodes)
- Labels: None

**Alert Configuration:**
- CRITICAL: partitioned_nodes > 0 for 60 seconds

---

## Implementation Schedule

### Phase 1: Workflow Metrics (Priority 1)
- **Effort:** 4 hours
- **Impact:** Enables Workflow Execution Dashboard (3 widgets, 2 alerts)
- **Files:**
  - Edit: `src/telemetry/cre_metrics.erl`
  - Edit: `src/core/gen_yawl.erl` (collection points)
  - New: `test/telemetry/workflow_metrics_test.erl`

### Phase 2: Mining & Health Metrics (Priority 2)
- **Effort:** 3 hours
- **Impact:** Enables Mining Dashboard (2 widgets), Health Alerts
- **Files:**
  - Edit: `src/telemetry/cre_metrics.erl`
  - Edit: `src/api/cre_health.erl`
  - Edit: `src/yawl/yawl_mining.erl` (collection points)
  - New: `test/telemetry/health_metrics_test.erl`

### Phase 3: Erlang VM Metrics (Priority 3)
- **Effort:** 6 hours
- **Impact:** Enables Erlang VM Dashboard (10 widgets)
- **Files:**
  - New: `src/telemetry/erlang_vm_metrics.erl`
  - New: `src/telemetry/mnesia_metrics.erl`
  - Edit: `src/cre_app.erl` (supervision)
  - New: `test/telemetry/erlang_vm_metrics_test.erl`

### Phase 4: GCP Metrics Adapter (Priority 4)
- **Effort:** 8 hours
- **Impact:** Enables deployment to GCP, Marketplace submission
- **Files:**
  - New: `src/telemetry/gcp_metrics_adapter.erl`
  - Edit: `src/cre_app.erl` (supervision)
  - New: `test/telemetry/gcp_metrics_adapter_test.erl`

**Total Effort:** ~21 hours
**Total Metrics Implemented:** 22 new metrics + conversions

---

## Testing Strategy

### Unit Tests
Each metric should have corresponding unit tests:

```erlang
% test/telemetry/workflow_metrics_test.erl
workflow_completed_test() ->
    cre_metrics:workflow_completed(success),
    cre_metrics:workflow_completed(failed),
    {ok, Value} = otel_metrics:get_metric(
        cre_metrics:workflow_completed_total(),
        #{status => success}
    ),
    ?assertEqual(1, Value).
```

### Integration Tests
Test metric collection and export:

```erlang
% test/telemetry/metrics_export_test.erl
test_prometheus_export() ->
    Export = otel_metrics:export_metrics(),
    ?assertMatch("cre_workflow_completed_total", Export),
    ?assertMatch("cre_erlang_memory_atom", Export).
```

### System Tests
Test metrics on live system:

```bash
# Build and run in Docker
docker run -it cre:0.3.0 sh
# Inside container:
erl -noshell -eval "
    cre_metrics:init(),
    cre_metrics:workflow_completed(success),
    io:format('~p~n', [otel_metrics:export_metrics()])
"
```

---

## Deployment Checklist

- [ ] All unit tests pass: `rebar3 eunit`
- [ ] All metrics export correctly: `rebar3 eunit --module=metrics_export_test`
- [ ] No performance regression: `rebar3 ct --suite=performance_test`
- [ ] YAML validation: `python3 -m yaml alert-policies.yaml`
- [ ] JSON validation: `python3 -m json.tool *.json`
- [ ] GCP dashboard creation test
- [ ] GCP alert policy creation test
- [ ] Metrics visible in Cloud Monitoring console within 5 minutes
- [ ] Alert policies triggered correctly with test data

---

## Notes

1. **Metric Collection Overhead:** Keep metric collection efficient to avoid performance impact
   - Batch updates where possible
   - Cache expensive calculations (e.g., scheduler stats)
   - Collect on 30-60 second intervals for gauges

2. **Unit Conversions:** Dashboards expect specific units
   - Milliseconds → Seconds: divide by 1000
   - Bytes → Gigabytes: divide by 10^9
   - Fractions: as 0.0-1.0 (not percentages)

3. **Label Cardinality:** Be careful with high-cardinality labels
   - Good: `algorithm` (few values), `table_name` (bounded)
   - Bad: `case_id` (millions of values), `timestamp` (unbounded)

4. **GCP Metric Limits:**
   - Max 500 custom metrics per workspace
   - Max 100 labels per metric (total)
   - Max 10,000 active time series per metric

---

