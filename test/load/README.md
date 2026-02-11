# CRE Load Testing Suite

Comprehensive load testing framework for validating the scalability and performance of the CRE workflow engine.

## Overview

This directory contains load testing scenarios designed to validate:

- **Concurrent workflow execution** (10 to 1000+ workflows)
- **Sustained load behavior** (long-running tests)
- **Various load patterns** (ramp-up, burst, spike, oscillating)
- **Resource usage** (memory, CPU, schedulers, processes)
- **Failure resilience** (crash recovery, cascading failures)
- **Performance degradation** (throughput and latency under load)

## Files

### Test Suites

- **`load_test_SUITE.erl`** - Main Common Test suite for load testing
  - 24 comprehensive test cases across 6 categories
  - Automated result collection and reporting
  - Resource monitoring and profiling

- **`load_test_scenarios.erl`** - Reusable load testing scenarios
  - Complex workflow patterns under load
  - Real-world simulation scenarios
  - Stress testing utilities
  - Performance profiling helpers

### Documentation

- **`LOAD_TEST_REPORT_TEMPLATE.md`** - Template for load test reports
  - Structured format for documenting results
  - Performance metrics and analysis sections
  - Scalability recommendations

- **`README.md`** - This file

## Running Load Tests

### Prerequisites

All tests MUST be run inside Docker containers per project requirements:

```bash
# Build Docker image
docker buildx bake --load

# Start development container
docker run -it --rm -v $(pwd):/work -w /work cre:0.3.0 sh
```

### Running the Full Test Suite

Inside the Docker container:

```bash
# Compile all modules
rebar3 compile

# Run complete load test suite
rebar3 ct --suite=test/load/load_test_SUITE

# View results
cat _build/test/logs/*/load_test_SUITE.log
```

### Running Individual Test Groups

```bash
# Run only concurrent execution tests
rebar3 ct --suite=test/load/load_test_SUITE --group=concurrent_execution

# Run only sustained load tests
rebar3 ct --suite=test/load/load_test_SUITE --group=sustained_load

# Run only load pattern tests
rebar3 ct --suite=test/load/load_test_SUITE --group=load_patterns

# Run only resource monitoring tests
rebar3 ct --suite=test/load/load_test_SUITE --group=resource_monitoring

# Run only failure scenario tests
rebar3 ct --suite=test/load/load_test_SUITE --group=failure_scenarios

# Run only degradation analysis
rebar3 ct --suite=test/load/load_test_SUITE --group=degradation_analysis
```

### Running Individual Test Cases

```bash
# Run specific test case
rebar3 ct --suite=test/load/load_test_SUITE --case=concurrent_workflows_100

# Run multiple specific tests
rebar3 ct --suite=test/load/load_test_SUITE \
  --case=concurrent_workflows_100,sustained_load_medium
```

### Running Scenario Tests

From Erlang shell inside container:

```erlang
% Start the application
application:ensure_all_started(cre).

% Compile scenario module
c("test/load/load_test_scenarios.erl").

% Run a scenario
load_test_scenarios:run_scenario(mixed_pattern_load).
load_test_scenarios:run_scenario(order_fulfillment_simulation).
load_test_scenarios:run_scenario(extreme_concurrency_test).

% Run scenario with custom options
load_test_scenarios:run_scenario(mixed_pattern_load, #{
    workflow_count => 200,
    duration_seconds => 120,
    concurrency_level => 50,
    enable_profiling => true
}).

% Profile specific workflow pattern
load_test_scenarios:profile_workflow_execution(sequence, 1000).

% Detect memory leaks
load_test_scenarios:detect_memory_leaks(parallel_split, 500).

% Measure GC impact
load_test_scenarios:measure_gc_impact(1000).
```

## Test Categories

### 1. Concurrent Execution Tests

Validates system behavior with increasing numbers of parallel workflows.

**Tests:**
- `concurrent_workflows_10` - Baseline (10 workflows)
- `concurrent_workflows_50` - Light load
- `concurrent_workflows_100` - Moderate load
- `concurrent_workflows_500` - Stress test
- `concurrent_workflows_1000` - Extreme stress

**Metrics:**
- Throughput (workflows/second)
- Latency (average, P50, P95, P99)
- Success/failure rates
- Resource utilization

### 2. Sustained Load Tests

Long-running tests to validate stability over extended periods.

**Tests:**
- `sustained_load_light` - 10 wf/s for 60 seconds
- `sustained_load_medium` - 50 wf/s for 60 seconds
- `sustained_load_heavy` - 100 wf/s for 60 seconds

**Focus Areas:**
- Memory leak detection
- Resource stability
- Consistent performance
- Error accumulation

### 3. Load Pattern Tests

Tests various realistic load patterns.

**Tests:**
- `ramp_up_load` - Gradual load increase
- `steady_load` - Constant load
- `burst_load` - Periodic bursts
- `spike_load` - Sudden spikes
- `oscillating_load` - Fluctuating load

**Purpose:**
- Validate elasticity
- Test autoscaling triggers
- Measure recovery time
- Identify breaking points

### 4. Resource Monitoring Tests

Monitor system resources under load.

**Tests:**
- `memory_usage_under_load` - Track memory growth
- `scheduler_utilization` - CPU scheduler efficiency
- `ets_table_growth` - ETS table proliferation
- `process_count_stability` - Process lifecycle management

**Thresholds:**
- Memory growth: < 50 MB acceptable
- ETS table growth: < 50 tables
- Process count: Stable (low std deviation)

### 5. Failure Scenario Tests

Validate resilience and recovery.

**Tests:**
- `process_crash_during_load` - Random process crashes
- `cascading_failure_recovery` - Multiple failures
- `partial_system_failure` - Subsystem failures
- `supervisor_restart_under_load` - Supervisor behavior

**Validation:**
- Graceful degradation
- Recovery time
- Data integrity
- Error isolation

### 6. Degradation Analysis

Measure performance degradation under increasing load.

**Tests:**
- `throughput_degradation` - Throughput vs load
- `latency_degradation` - Latency vs load
- `recovery_time_measurement` - Post-stress recovery

**Thresholds:**
- Latency degradation: < 50% acceptable
- Throughput: Should scale linearly in safe range

## Available Scenarios

### Complex Workflow Scenarios

- `mixed_pattern_load` - Multiple pattern types concurrently
- `nested_workflow_load` - Workflows with sub-workflows
- `long_running_workflow_load` - Extended execution time
- `rapid_short_workflow_load` - Many quick workflows

### Real-World Simulations

- `order_fulfillment_simulation` - E-commerce workflow
- `approval_workflow_simulation` - Multi-level approval
- `data_pipeline_simulation` - ETL pipeline pattern

### Stress Tests

- `extreme_concurrency_test` - 2000 concurrent workflows
- `memory_pressure_test` - High memory utilization
- `scheduler_saturation_test` - Max out schedulers

## Performance Profiling

### Built-in Profiling Tools

```erlang
% Start profiling
ProfilingState = load_test_scenarios:start_profiling().

% ... run tests ...

% Get profiling results
Results = load_test_scenarios:stop_profiling(ProfilingState).
```

### Memory Leak Detection

```erlang
% Run 500 iterations and check for leaks
HasLeak = load_test_scenarios:detect_memory_leaks(sequence, 500).
% Returns: true if leak detected (> 10 MB growth)
```

### GC Impact Analysis

```erlang
% Measure garbage collection during 1000 workflow executions
GcStats = load_test_scenarios:measure_gc_impact(1000).
% Returns: #{gc_count, words_reclaimed, mb_reclaimed}
```

## Report Generation

After running the test suite, a comprehensive report is automatically generated:

**Locations:**
- Text report: `_build/test/logs/ct_run.*/load_test_results/load_test_report.txt`
- CSV data: `_build/test/logs/ct_run.*/load_test_results/load_test_results.csv`
- CT logs: `_build/test/logs/ct_run.*/load_test_SUITE.*.html`

**Manual Report Generation:**

1. Run the full test suite
2. Fill in the `LOAD_TEST_REPORT_TEMPLATE.md` with results
3. Include graphs and visualizations as needed
4. Archive for future comparison

## Interpreting Results

### Success Criteria

**Throughput:**
- Should scale linearly up to 100 concurrent workflows
- Acceptable degradation: < 20% at 500 workflows

**Latency:**
- P95 latency should remain < 500ms under normal load
- P99 latency should remain < 1000ms under normal load
- Degradation should be < 50% at high load

**Memory:**
- No sustained growth over time (< 50 MB per hour)
- GC should reclaim memory effectively
- No ETS table leaks

**Stability:**
- Zero crashes under normal load
- Graceful degradation under stress
- Full recovery within 30 seconds after load removal

### Warning Signs

🚨 **Critical Issues:**
- Consistent workflow failures (> 5%)
- Unbounded memory growth
- System unresponsiveness
- Crash cascades

⚠️ **Performance Issues:**
- Latency > 2x baseline at moderate load
- Throughput degradation > 50%
- Memory growth > 100 MB
- High scheduler imbalance

ℹ️ **Optimization Opportunities:**
- Throughput plateaus before expected saturation
- High GC frequency
- Uneven scheduler utilization
- ETS table fragmentation

## Troubleshooting

### Tests Failing to Start

```bash
# Check application starts correctly
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 erl -eval \
  "application:ensure_all_started(cre), erlang:halt()."

# Verify compilation
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile
```

### Tests Timing Out

Increase timeouts in `load_test_SUITE.erl` or run fewer concurrent workflows:

```bash
# Run with reduced load
rebar3 ct --suite=test/load/load_test_SUITE \
  --case=concurrent_workflows_50
```

### Memory Issues

If tests crash due to memory:

```bash
# Run container with more memory
docker run --memory=4g --rm -v $(pwd):/work -w /work cre:0.3.0 sh

# Inside container, increase BEAM memory
erl +MBscs 4096 -pa _build/test/lib/*/ebin ...
```

### Docker Not Available

Per project rules, all work must be done in Docker. If Docker is unavailable, the SessionStart.sh hook will bootstrap OTP 28 in gVisor environments (like Claude Code on Web).

## Integration with CI/CD

### GitHub Actions

```yaml
- name: Run Load Tests
  run: |
    docker buildx bake --load
    docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
      rebar3 ct --suite=test/load/load_test_SUITE --group=concurrent_execution
```

### GCP Cloud Build

```yaml
- name: 'gcr.io/cloud-builders/docker'
  args: ['buildx', 'bake', '--load']

- name: 'cre:0.3.0'
  args: ['rebar3', 'ct', '--suite=test/load/load_test_SUITE']
```

## Performance Baselines

**Recommended Baselines (to be established):**

| Metric | Target | Stretch Goal |
|--------|--------|--------------|
| Max Concurrent Workflows | 500 | 1000 |
| Throughput (wf/s) | 50 | 100 |
| P95 Latency (ms) | < 500 | < 250 |
| Memory Growth (MB/hour) | < 50 | < 10 |
| Recovery Time (s) | < 30 | < 10 |

These baselines should be updated after initial test runs.

## Contributing

When adding new load tests:

1. Follow existing test patterns in `load_test_SUITE.erl`
2. Add reusable scenarios to `load_test_scenarios.erl`
3. Update this README with new test descriptions
4. Run full test suite to verify no regressions
5. Update report template if new metrics are added

## References

- **Main Documentation:** `/home/user/cre/docs/`
- **GCP Deployment:** `/home/user/cre/docs/gcp/GCP_MARKETPLACE_READINESS.md`
- **Testing Rules:** `/home/user/cre/.claude/rules/testing.md`
- **Erlang Conventions:** `/home/user/cre/.claude/rules/erlang.md`

## Support

For issues or questions:
- Open an issue: https://github.com/joergen7/cre/issues
- Review existing tests in `/home/user/cre/test/`
- Check CI/CD pipeline logs

---

**Version:** 0.3.0
**Last Updated:** 2025-02-11
**Status:** Ready for initial load testing
