# CRE Monitoring Fixes Applied

**Date:** 2025-02-11
**Status:** ✅ FIXES COMPLETED

---

## Summary

Two critical issues in the monitoring configuration have been identified and fixed:

1. **YAML Indentation Error** - `alert-policies.yaml` line 25
2. **Duplicate JSON Property** - `erlang-vm-dashboard.json` Runtime Utilization widget

Both files now pass validation.

---

## Fix #1: YAML Indentation Error

**File:** `/home/user/cre/monitoring/gcp/alert-policies.yaml`
**Location:** Line 25-26
**Policy:** "CRE - High Pattern Execution Time"

### Problem
The `groupByFields` property was indented with 11 spaces instead of 12, breaking YAML block structure:

```yaml
22:            alignmentPeriod: 300s
23:            perSeriesAligner: ALIGN_PERCENTILE99
24:            crossSeriesReducer: REDUCE_NONE
25:           groupByFields:              # ❌ 11 spaces (wrong)
26:              - metric.label.pattern_name
```

### Solution
Added one space to align `groupByFields` with surrounding properties:

```yaml
22:            alignmentPeriod: 300s
23:            perSeriesAligner: ALIGN_PERCENTILE99
24:            crossSeriesReducer: REDUCE_NONE
25:            groupByFields:              # ✅ 12 spaces (correct)
26:              - metric.label.pattern_name
```

### Verification
```bash
python3 -c "import yaml; yaml.safe_load(open('monitoring/gcp/alert-policies.yaml'))"
# Result: ✅ YAML parses successfully, 16 policies loaded
```

---

## Fix #2: Duplicate JSON Property

**File:** `/home/user/cre/monitoring/gcp/erlang-vm-dashboard.json`
**Location:** Lines 52-61
**Widget:** "Runtime Utilization"

### Problem
The Runtime Utilization widget had a redundant `gaugeView` property at the top level, plus another inside the `scorecard`:

```json
{
  "title": "Runtime Utilization",
  "gaugeView": {                    // ❌ Incorrect - top level
    "lowerBound": 0,
    "upperBound": 100
  },
  "scorecard": {
    "gaugeView": {                  // ✅ Correct - nested in scorecard
      "lowerBound": 0,
      "upperBound": 100
    },
    "dataSets": [ ... ]
  }
}
```

This violates the GCP Dashboard API schema where visualization properties must be nested within the widget type (scorecard, xyChart, etc.).

### Solution
Removed the outer `gaugeView` property, keeping only the one inside `scorecard`:

```json
{
  "title": "Runtime Utilization",
  "scorecard": {
    "gaugeView": {                  // ✅ Only nested version
      "lowerBound": 0,
      "upperBound": 100
    },
    "dataSets": [ ... ]
  }
}
```

### Verification
```bash
python3 -c "import json; json.load(open('monitoring/gcp/erlang-vm-dashboard.json'))"
# Result: ✅ JSON is valid
```

---

## Validation Results

### Files Checked
- ✅ `monitoring/gcp/gke-cluster-dashboard.json` - Valid JSON (8 widgets)
- ✅ `monitoring/gcp/erlang-vm-dashboard.json` - Fixed + Valid JSON (10 widgets)
- ✅ `monitoring/gcp/workflow-execution-dashboard.json` - Valid JSON (10 widgets)
- ✅ `monitoring/gcp/alert-policies.yaml` - Fixed + Valid YAML (16 policies)

### Total Monitoring Components
- **Dashboards:** 3
- **Widgets:** 28
- **Alert Policies:** 16
- **Status:** All syntactically valid ✅

---

## What's NOT Fixed (Requires Code Changes)

The following issues require implementation in the telemetry modules and are documented in `MONITORING_VALIDATION_REPORT.md`:

### 1. Missing Metric Implementations (16 metrics)

**Erlang VM Metrics** (Need new module: `src/telemetry/erlang_vm_metrics.erl`):
- Memory breakdown: atom, binary, code, ets, processes, system (6 metrics)
- GC metrics: count, pause_seconds, words_reclaimed (3 metrics)
- Scheduler metrics: per-scheduler utilization (1 metric)
- Mnesia metrics: table_size, transactions_committed/aborted/failed, partitioned_nodes (5 metrics)

**Workflow Metrics** (Add to `src/telemetry/cre_metrics.erl`):
- workflow_completed_total
- workflow_failed_total
- workflow_error_rate
- workflow_latency

**Mining Metrics** (Add to `src/telemetry/cre_metrics.erl`):
- mining_events_processed_total
- mining_models_discovered_total

**Health Metrics** (Add to `src/api/cre_health.erl`):
- health_check

### 2. Metric Format/Unit Conversion Issues

Dashboard metrics use GCP Custom Metrics format and units:
- Format: `workload.googleapis.com/cre/*` (not `cre_*`)
- Units: seconds (not milliseconds), GB (not bytes)

**Solution:** Implement `src/telemetry/gcp_metrics_adapter.erl` to convert and export metrics.

---

## Next Steps

1. **Deploy Configuration Changes (Ready Now)**
   ```bash
   # Files are ready for deployment
   git add monitoring/gcp/alert-policies.yaml
   git add monitoring/gcp/erlang-vm-dashboard.json
   ```

2. **Implement Missing Metrics** (See MONITORING_VALIDATION_REPORT.md Priority 2-7)
   - Create new telemetry modules
   - Update cre_metrics.erl with new metrics
   - Run tests: `rebar3 eunit`

3. **Test Configuration**
   ```bash
   # Validate after each change
   python3 -c "import yaml; yaml.safe_load(open('monitoring/gcp/alert-policies.yaml'))"
   python3 -c "import json; [json.load(open(f)) for f in glob.glob('monitoring/gcp/*.json')]"
   ```

4. **Deploy to GCP**
   ```bash
   # Apply dashboards
   gcloud monitoring dashboards create --config-from-file=monitoring/gcp/gke-cluster-dashboard.json

   # Apply alert policies
   gcloud alpha monitoring policies create --policy-from-file=monitoring/gcp/alert-policies.yaml
   ```

---

## Files Modified

| File | Changes | Lines |
|------|---------|-------|
| `/home/user/cre/monitoring/gcp/alert-policies.yaml` | Fixed indentation on line 25 | 1 space added |
| `/home/user/cre/monitoring/gcp/erlang-vm-dashboard.json` | Removed duplicate gaugeView property | 4 lines deleted |

---

## Validation Checklist

- [x] YAML syntax validation
- [x] JSON syntax validation
- [x] JSON structure validation
- [x] YAML parsing test
- [x] Dashboard widget structure
- [x] Alert policy structure
- [ ] Metric availability test (requires implementation)
- [ ] GCP dashboard deployment test
- [ ] GCP alert policy deployment test

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-02-11 | Initial fixes applied and validated |

