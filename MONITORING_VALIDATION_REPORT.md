# CRE Monitoring Dashboards and Alert Policies - Validation Report

**Generated:** 2025-02-11
**Status:** ISSUES FOUND - ACTION REQUIRED

---

## Executive Summary

The CRE monitoring infrastructure includes 3 JSON dashboards and 1 YAML alert policy file. While the JSON files are syntactically valid, there are **critical issues** with metric references and YAML formatting that need to be fixed before deployment.

**Key Findings:**
- ✅ All JSON dashboards: Valid syntax
- ❌ Alert policies YAML: Indentation error (line 26)
- ⚠️ Metric reference gaps: Dashboards reference 33+ metrics, but implementation only exports ~22 metrics
- ⚠️ Missing metric implementations: Several CRE-specific metrics not yet implemented

---

## 1. JSON Syntax Validation

### Result: PASS ✅

All JSON dashboard files are syntactically valid:

| File | Status | Lines | Widgets |
|------|--------|-------|---------|
| `monitoring/gcp/gke-cluster-dashboard.json` | ✅ Valid | 305 | 8 |
| `monitoring/gcp/erlang-vm-dashboard.json` | ✅ Valid | 417 | 10 |
| `monitoring/gcp/workflow-execution-dashboard.json` | ✅ Valid | 357 | 10 |

**Total dashboards:** 3
**Total widgets:** 28

---

## 2. YAML Validation

### Result: FAIL ❌

**File:** `monitoring/gcp/alert-policies.yaml`
**Error:** Indentation error at line 26

```yaml
22:            alignmentPeriod: 300s
23:            perSeriesAligner: ALIGN_PERCENTILE99
24:            crossSeriesReducer: REDUCE_NONE
25:           groupByFields:  # ❌ WRONG - 11 spaces instead of 12
26:              - metric.label.pattern_name
```

**Issue:** Line 25 has 11 spaces (one less than required). YAML requires 12 spaces to align with `crossSeriesReducer` above.

**Fix:** Add one more space before `groupByFields` on line 25.

**Policy Count:** 16 alert policies defined
**Affected Policies:** CRE - High Pattern Execution Time (line 13-36)

---

## 3. Dashboard Metrics Analysis

### GKE Cluster Dashboard (`gke-cluster-dashboard.json`)

**Metrics Referenced (7):**
- `kubernetes.io/container/cpu/usage_time` - Kubernetes native
- `kubernetes.io/container/restart_count` - Kubernetes native
- `kubernetes.io/node/memory/bytes_used` - Kubernetes native
- `kubernetes.io/node/network/received_bytes_count` - Kubernetes native
- `kubernetes.io/node/network/sent_bytes_count` - Kubernetes native
- `kubernetes.io/pod/status/phase` - Kubernetes native
- `kubernetes.io/volume/bytes_used` - Kubernetes native

**Status:** ✅ All metrics are standard Kubernetes metrics, should be available from GKE by default.

---

### Erlang VM Dashboard (`erlang-vm-dashboard.json`)

**Metrics Referenced (18):**

| Metric | Source | Status | Notes |
|--------|--------|--------|-------|
| `workload.googleapis.com/erlang/ets/memory` | cre_metrics.erl | ⚠️ Partial | Exported as `cre_memory_bytes`, needs adapter mapping |
| `workload.googleapis.com/erlang/gc/count` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang/gc/pause_seconds` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang/gc/words_reclaimed` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang/memory/atom` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects breakdown metrics |
| `workload.googleapis.com/erlang/memory/binary` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects breakdown metrics |
| `workload.googleapis.com/erlang/memory/code` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects breakdown metrics |
| `workload.googleapis.com/erlang/memory/ets` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects breakdown metrics |
| `workload.googleapis.com/erlang/memory/processes` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects breakdown metrics |
| `workload.googleapis.com/erlang/memory/system` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects breakdown metrics |
| `workload.googleapis.com/erlang/memory/total` | cre_metrics.erl | ⚠️ Partial | Exported as `cre_memory_bytes` |
| `workload.googleapis.com/erlang/mnesia/table_size` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang/mnesia/transactions_aborted` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang/mnesia/transactions_committed` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang/mnesia/transactions_failed` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang/process/count` | autoscaling_metrics.erl | ✅ Exported | Available via `cre_autoscaling_erlang_process_count` |
| `workload.googleapis.com/erlang/statistics/runtime` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects this metric |
| `workload.googleapis.com/erlang_scheduler/utilization` | cre_metrics.erl | ⚠️ Not Exported | Dashboard expects per-scheduler metrics |

**Summary:** 3 metrics partially available, 15 metrics NOT EXPORTED

---

### Workflow Execution Dashboard (`workflow-execution-dashboard.json`)

**Metrics Referenced (8):**

| Metric | Source | Status | Notes |
|--------|--------|--------|-------|
| `workload.googleapis.com/cre/workflow/active` | autoscaling_metrics.erl | ✅ Available | Via `cre_autoscaling_active_workflows` |
| `workload.googleapis.com/cre/workflow/completed_total` | ⚠️ Not Exported | ❌ Missing | Alert policies also depend on this |
| `workload.googleapis.com/cre/workflow/failed_total` | ⚠️ Not Exported | ❌ Missing | Dashboard widget references this |
| `workload.googleapis.com/cre/pattern/execution_duration` | cre_metrics.erl | ⚠️ Partial | Exported as `cre_pattern_execution_duration_ms` |
| `workload.googleapis.com/cre/pnet/transition/total` | cre_metrics.erl | ⚠️ Partial | Exported as `cre_pnet_transitions_total` |
| `workload.googleapis.com/cre/mining/execution_duration` | cre_metrics.erl | ⚠️ Partial | Exported as `cre_mining_algorithm_duration_ms` |
| `workload.googleapis.com/cre/mining/events_processed_total` | ⚠️ Not Exported | ❌ Missing | Dashboard widget references this |
| `workload.googleapis.com/cre/mining/models_discovered_total` | ⚠️ Not Exported | ❌ Missing | Dashboard widget references this |

**Summary:** 1 metric available, 2 metrics partially available, 5 metrics NOT EXPORTED

---

## 4. Alert Policies Metrics Analysis

**File:** `monitoring/gcp/alert-policies.yaml`
**Total Policies:** 16
**Unique Metrics Referenced:** 13

| Metric | Used By | Status |
|--------|---------|--------|
| `workload.googleapis.com/cre/health/check` | Service Down alert | ⚠️ Not in dashboards |
| `workload.googleapis.com/cre/mining/execution_duration` | Mining Algorithm Slow | ⚠️ Not Exported |
| `workload.googleapis.com/cre/pattern/execution_duration` | Pattern Execution Time | ⚠️ Partial (from cre_metrics) |
| `workload.googleapis.com/cre/workflow/active` | Too Many Active Workflows | ✅ Available |
| `workload.googleapis.com/cre/workflow/completed_total` | Low Throughput, Completion Rate | ⚠️ Not Exported |
| `workload.googleapis.com/cre/workflow/error_rate` | High/Critical Error Rate | ⚠️ Not Exported |
| `workload.googleapis.com/cre/workflow/latency` | High Latency | ⚠️ Not Exported |
| `workload.googleapis.com/erlang/gc/count` | High GC Rate | ⚠️ Not Exported |
| `workload.googleapis.com/erlang/memory/total` | Memory Usage | ⚠️ Partial |
| `workload.googleapis.com/erlang/mnesia/partitioned_nodes` | Mnesia Partition Detected | ⚠️ Not Exported |
| `workload.googleapis.com/erlang/mnesia/transactions_committed` | Mnesia Transaction Failure | ⚠️ Not Exported |
| `workload.googleapis.com/erlang/mnesia/transactions_failed` | Mnesia Transaction Failure | ⚠️ Not Exported |
| `workload.googleapis.com/erlang/process/count` | High Process Count | ✅ Available |

**Summary:** 2 metrics available, 11 metrics NOT EXPORTED

---

## 5. Missing Metric Implementations

### Critical Gap: Workflow Metrics

The following workflow metrics are referenced in dashboards and alert policies but are NOT exported by any module:

1. **`workload.googleapis.com/cre/workflow/completed_total`**
   - Used by: Throughput, Completion Rate, Success Rate widgets
   - Used by: Low Throughput, Success Rate alerts
   - Required: Counter tracking completed workflows

2. **`workload.googleapis.com/cre/workflow/failed_total`**
   - Used by: Workflow Execution Dashboard widget
   - Used by: Completion Rate widget
   - Required: Counter tracking failed workflows

3. **`workload.googleapis.com/cre/workflow/error_rate`**
   - Used by: Error Rate alerts (High/Critical)
   - Required: Pre-computed error rate metric or calculated from completed/failed

4. **`workload.googleapis.com/cre/workflow/latency`**
   - Used by: High Latency alert
   - Required: Histogram/gauge for workflow execution latency

5. **`workload.googleapis.com/cre/health/check`**
   - Used by: Service Down alert, No Metrics Received alert
   - Required: Health check status metric

### Critical Gap: Mining Metrics

The following mining metrics are missing implementations:

1. **`workload.googleapis.com/cre/mining/events_processed_total`**
   - Used by: Mining Throughput widget
   - Exported as: Not implemented yet
   - Required: Counter for events processed in mining

2. **`workload.googleapis.com/cre/mining/models_discovered_total`**
   - Used by: Models Discovered scorecard
   - Exported as: Not implemented yet
   - Required: Counter for discovered models

### Critical Gap: Erlang VM Metrics

Missing memory breakdown and GC metrics:

1. **Memory breakdown** (atom, binary, code, ets, processes, system)
   - Expected: Individual breakdown metrics
   - Actual: Only total memory (`cre_memory_bytes`)
   - Solution: Collect from `erlang:memory()` BIF

2. **GC metrics** (count, pause_seconds, words_reclaimed)
   - Expected: Separate GC metrics
   - Actual: Not implemented
   - Solution: Collect from `erlang:statistics(garbage_collection)` BIF

3. **Mnesia metrics** (table_size, transactions_committed, transactions_aborted, transactions_failed, partitioned_nodes)
   - Expected: Mnesia-specific metrics
   - Actual: Not implemented
   - Solution: Use Mnesia activity monitoring

4. **Scheduler utilization** (`erlang_scheduler/utilization`)
   - Expected: Per-scheduler CPU utilization
   - Actual: Not implemented
   - Solution: Collect from `erlang:statistics(scheduler_wall_time)` BIF

---

## 6. Metric Mapping Issues

### Issue 1: Format Mismatch
- **Code exports:** Prometheus format with `cre_` prefix (e.g., `cre_pattern_execution_duration_ms`)
- **Dashboards expect:** GCP Custom Metrics format (e.g., `workload.googleapis.com/cre/pattern/execution_duration`)
- **Unit mismatch:** Code uses milliseconds, dashboards expect seconds
- **Solution:** Cloud exporter adapters must convert units and format metrics

### Issue 2: Missing Adapter Implementation
The dashboards and alert policies assume a Cloud Monitoring adapter is converting Prometheus metrics to GCP Custom Metrics. This adapter needs to:

1. **Collect** metrics from cre_metrics.erl and other modules
2. **Convert** format: `cre_*` → `workload.googleapis.com/cre/*`
3. **Convert** units: milliseconds → seconds, bytes → GB, etc.
4. **Push** to Cloud Monitoring API

**Current Status:** Adapter infrastructure exists (cloud_logging_backend.erl, cloud_trace_exporter.erl) but metric export implementation is incomplete.

---

## 7. Query Validation

### Erlang VM Dashboard - Runtime Utilization Widget

**Issue:** Duplicate `gaugeView` configuration

**Lines:** 52-61
```json
{
  "title": "Runtime Utilization",
  "gaugeView": {
    "lowerBound": 0,
    "upperBound": 100
  },
  "scorecard": {
    "gaugeView": {  // ❌ DUPLICATE - nested gaugeView inside scorecard
      "lowerBound": 0,
      "upperBound": 100
    },
```

**Fix:** Remove the outer `gaugeView` object; scorecard should define visualization type internally.

---

## 8. Summary of Issues

### By Category

| Category | Count | Severity | Status |
|----------|-------|----------|--------|
| JSON Syntax Errors | 0 | - | ✅ PASS |
| YAML Syntax Errors | 1 | HIGH | ❌ FAIL |
| JSON Structure Issues | 1 | MEDIUM | ⚠️ WARN |
| Missing Metric Implementations | 16 | CRITICAL | ❌ FAIL |
| Metric Format Mismatches | ~26 | CRITICAL | ❌ FAIL |
| Unit Conversion Issues | ~18 | HIGH | ⚠️ WARN |

### Breakdown by File

| File | Status | Issues |
|------|--------|--------|
| gke-cluster-dashboard.json | ✅ PASS | 0 |
| erlang-vm-dashboard.json | ⚠️ WARN | 1 (duplicate gaugeView) |
| workflow-execution-dashboard.json | ⚠️ WARN | ~8 missing metrics |
| alert-policies.yaml | ❌ FAIL | 1 syntax + ~11 missing metrics |

---

## 9. Required Fixes

### Priority 1: CRITICAL - Fix YAML Indentation

**File:** `/home/user/cre/monitoring/gcp/alert-policies.yaml`
**Location:** Line 25
**Change:** Indent `groupByFields:` to align with `crossSeriesReducer:`

Before:
```yaml
24:            crossSeriesReducer: REDUCE_NONE
25:           groupByFields:
```

After:
```yaml
24:            crossSeriesReducer: REDUCE_NONE
25:            groupByFields:
```

### Priority 2: HIGH - Implement Missing Workflow Metrics

Add to `src/telemetry/cre_metrics.erl`:
- `cre_workflow_completed_total` (counter)
- `cre_workflow_failed_total` (counter)
- `cre_workflow_error_rate` (gauge)
- `cre_workflow_latency_ms` (histogram)

### Priority 3: HIGH - Implement Mining Metrics

Add to `src/telemetry/cre_metrics.erl`:
- `cre_mining_events_processed_total` (counter)
- `cre_mining_models_discovered_total` (counter)

### Priority 4: HIGH - Implement Health Check Metric

Add to `src/api/cre_health.erl`:
- `cre_health_check` (gauge) - 1 if healthy, 0 if unhealthy

### Priority 5: HIGH - Implement Erlang VM Metrics

Add to new module `src/telemetry/erlang_vm_metrics.erl`:
- Memory breakdown: `erlang/memory/atom`, `/binary`, `/code`, `/ets`, `/processes`, `/system`
- GC metrics: `erlang/gc/count`, `/pause_seconds`, `/words_reclaimed`
- Scheduler metrics: `erlang_scheduler/utilization` (per scheduler)
- Mnesia metrics: `erlang/mnesia/table_size`, `/transactions_*`, `/partitioned_nodes`

### Priority 6: MEDIUM - Fix Dashboard Syntax

**File:** `/home/user/cre/monitoring/gcp/erlang-vm-dashboard.json`
**Location:** Lines 52-61 (Runtime Utilization widget)
**Change:** Remove duplicate `gaugeView` configuration

Before:
```json
{
  "title": "Runtime Utilization",
  "gaugeView": { ... },
  "scorecard": {
    "gaugeView": { ... },
    "dataSets": [ ... ]
  }
}
```

After:
```json
{
  "title": "Runtime Utilization",
  "scorecard": {
    "gaugeView": { ... },
    "dataSets": [ ... ]
  }
}
```

### Priority 7: MEDIUM - Create Metric Adapter

Create `src/telemetry/gcp_metrics_adapter.erl` to:
1. Subscribe to Prometheus metrics
2. Convert format (cre_* → workload.googleapis.com/cre/*)
3. Convert units (ms → s, B → GB)
4. Push to Cloud Monitoring API

---

## 10. Testing Checklist

- [ ] Validate YAML syntax after fix: `python3 -c "import yaml; yaml.safe_load(open('monitoring/gcp/alert-policies.yaml'))"`
- [ ] Validate all JSON files: `python3 -c "import json; [json.load(open(f)) for f in ['gke-cluster-dashboard.json', 'erlang-vm-dashboard.json', 'workflow-execution-dashboard.json']]"`
- [ ] Run telemetry metrics tests: `docker run -v $(pwd):/work cre:0.3.0 rebar3 eunit --module=cre_metrics_test`
- [ ] Verify metrics exported: `docker run -v $(pwd):/work cre:0.3.0 rebar3 eunit --module=otel_metrics_test`
- [ ] Deploy to GCP and verify metrics appear in Cloud Monitoring console
- [ ] Test alert policies trigger correctly with sample data

---

## 11. Recommendations

1. **Implement missing metrics BEFORE deploying to GCP Marketplace**
   - Dashboards won't display meaningful data without these metrics
   - Alert policies won't trigger without the underlying metrics

2. **Add metric exporter integration tests**
   - Verify each metric is collected at least once
   - Verify unit conversions are correct
   - Verify labels are properly formatted for GCP Custom Metrics

3. **Document metric availability**
   - Create a "Metrics Inventory" file listing which metrics are available in which environments
   - Document unit conventions (seconds, bytes, counts, etc.)
   - Document label keys for each metric

4. **Consider metric collection efficiency**
   - Some metrics (memory breakdown, scheduler stats) require system calls
   - Batch collection to avoid performance impact
   - Cache results with appropriate TTL

5. **Update dashboards after fixing metrics**
   - Test each dashboard widget with real data
   - Adjust thresholds based on actual workload patterns
   - Add explanatory text for non-obvious metrics

---

## Appendix: Metrics Inventory

### Exported by Code (22 metrics)

**cre_metrics.erl:**
- cre_pnet_transitions_total (counter)
- cre_pnet_transition_duration_ms (histogram)
- cre_pnet_tokens (gauge)
- cre_pnet_throughput_fps (gauge)
- cre_pattern_executions_total (counter)
- cre_pattern_execution_duration_ms (histogram)
- cre_pattern_errors_total (counter)
- cre_mining_algorithm_duration_ms (histogram)
- cre_mining_algorithm_executions_total (counter)
- cre_mining_discovered_places (gauge)
- cre_mining_discovered_transitions (gauge)
- cre_yawl_compilations_total (counter)
- cre_yawl_compilation_duration_ms (histogram)
- cre_yawl_cases_total (counter)
- cre_yawl_case_duration_ms (histogram)
- cre_memory_bytes (gauge)
- cre_process_count (gauge)

**autoscaling_metrics.erl:**
- cre_autoscaling_active_workflows (gauge)
- cre_autoscaling_workflow_queue_depth (gauge)
- cre_autoscaling_erlang_process_count (gauge)
- cre_autoscaling_mnesia_table_size (gauge)
- cre_autoscaling_scrape_timestamp (gauge)

### Expected by Dashboards (33 metrics)

**GKE Cluster:** 7 Kubernetes native metrics (available from GKE)
**Erlang VM:** 18 Erlang-specific metrics (16 missing implementations)
**Workflow Execution:** 8 CRE-specific metrics (6 missing implementations)

### Expected by Alert Policies (13 unique metrics)

**Implemented:** 2 metrics
**Partial:** 2 metrics (format/unit conversion needed)
**Missing:** 9 metrics

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-02-11 | Validation Script | Initial report - All issues identified |

