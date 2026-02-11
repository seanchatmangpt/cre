# Example Load Test Output

This document shows examples of expected output from the load testing suite.

## Test Execution Output

### Running a Single Test Case

```bash
$ docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --case=concurrent_workflows_10

===> Verifying dependencies...
===> Analyzing applications...
===> Compiling cre
===> Running Common Test suites...

Testing load_test_SUITE: Starting
********************************************************************************
concurrent_workflows_10

********************************************************************************

Test output:
[TSTMP] load_test_SUITE:init_per_suite/1: Starting Load Testing Suite
[TSTMP] load_test_SUITE:init_per_testcase/2: Starting test case: concurrent_workflows_10
[TSTMP] load_test_SUITE:concurrent_workflows_10/1: Running 10 concurrent sequence workflows
[TSTMP] load_test_SUITE:concurrent_workflows_10/1: Spawned 10 workflow processes
[TSTMP] load_test_SUITE:concurrent_workflows_10/1: Test complete: 10 completed, 0 failed, throughput: 45.23 wf/s
[TSTMP] load_test_SUITE:end_per_testcase/2: Completed test case: concurrent_workflows_10 (duration: 221 ms)

==> Test case: concurrent_workflows_10 - PASSED

Results:
  1 test cases: 1 ok, 0 failed, 0 skipped
  All tests successful.
```

### Running a Test Group

```bash
$ docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --group=concurrent_execution

===> Running Common Test suites...

Testing load_test_SUITE: Starting
********************************************************************************
concurrent_execution group

Test cases:
- concurrent_workflows_10
- concurrent_workflows_50
- concurrent_workflows_100
- concurrent_workflows_500
- concurrent_workflows_1000

********************************************************************************

Test output:
[TSTMP] Running concurrent_workflows_10...
[TSTMP] Test complete: 10 completed, 0 failed, throughput: 45.23 wf/s

[TSTMP] Running concurrent_workflows_50...
[TSTMP] Test complete: 50 completed, 0 failed, throughput: 198.41 wf/s

[TSTMP] Running concurrent_workflows_100...
[TSTMP] Test complete: 100 completed, 0 failed, throughput: 356.78 wf/s

[TSTMP] Running concurrent_workflows_500...
[TSTMP] Test complete: 500 completed, 3 failed, throughput: 412.33 wf/s

[TSTMP] Running concurrent_workflows_1000...
[TSTMP] Test complete: 1000 completed, 15 failed, throughput: 398.12 wf/s

Results:
  5 test cases: 5 ok, 0 failed, 0 skipped
  All tests successful.
```

## Scenario Execution Output

### Running a Scenario from Erlang Shell

```erlang
1> application:ensure_all_started(cre).
{ok,[cre]}

2> c("test/load/load_test_scenarios.erl").
{ok,load_test_scenarios}

3> Result = load_test_scenarios:run_scenario(mixed_pattern_load, #{workflow_count => 50}).
{scenario_result,
    <<"mixed_pattern_load">>,
    12345,  % duration_ms
    50,     % workflows_executed
    48,     % success_count
    2,      % failure_count
    25.6,   % avg_execution_time_ms
    3.89,   % throughput
    #{baseline_bytes => 45678900,
      final_bytes => 47123456,
      delta_bytes => 1444556,
      delta_mb => 1.377},
    #{duration_ms => 12345,
      memory_delta_bytes => 1444556,
      memory_delta_mb => 1.377}}

4> load_test_scenarios:profile_workflow_execution(sequence, 100).
#{duration_ms => 2567,
  memory_delta_bytes => 234567,
  memory_delta_mb => 0.224}

5> load_test_scenarios:detect_memory_leaks(sequence, 500).
false  % No leak detected

6> load_test_scenarios:measure_gc_impact(1000).
#{gc_count => 42,
  words_reclaimed => 1234567,
  mb_reclaimed => 9.456}
```

## Generated Reports

### Text Report (load_test_report.txt)

```
CRE Load Testing Report
======================

Test Start Time: 2025-02-11 15:30:00 UTC
Test End Time: 2025-02-11 15:55:00 UTC

Summary
-------
Total Tests: 24
Total Workflows: 2850
Total Completed: 2815
Total Failed: 35
Average Throughput: 187.67 workflows/sec

Test: concurrent_workflows_10
Duration: 221 ms
Started: 10, Completed: 10, Failed: 0
Throughput: 45.23 workflows/sec
Latency - Avg: 22.1 ms, P95: 35 ms, P99: 42 ms

Test: concurrent_workflows_50
Duration: 252 ms
Started: 50, Completed: 50, Failed: 0
Throughput: 198.41 workflows/sec
Latency - Avg: 25.2 ms, P95: 48 ms, P99: 67 ms

Test: concurrent_workflows_100
Duration: 281 ms
Started: 100, Completed: 100, Failed: 0
Throughput: 356.78 workflows/sec
Latency - Avg: 28.1 ms, P95: 89 ms, P99: 134 ms

...
```

### CSV Report (load_test_results.csv)

```csv
Test Name,Duration (ms),Started,Completed,Failed,Throughput (wf/s),Avg Latency (ms),P50 (ms),P95 (ms),P99 (ms),Max (ms),Min (ms)
concurrent_workflows_10,221,10,10,0,45.23,22.1,20,35,42,45,18
concurrent_workflows_50,252,50,50,0,198.41,25.2,23,48,67,78,19
concurrent_workflows_100,281,100,100,0,356.78,28.1,25,89,134,156,20
concurrent_workflows_500,1213,500,497,3,412.33,121.3,98,345,478,567,22
concurrent_workflows_1000,2512,1000,985,15,398.12,251.2,198,678,892,1023,21
sustained_load_light,60045,600,598,2,9.96,6004.5,5987,6234,6456,6678,5876
sustained_load_medium,60123,3000,2987,13,49.68,6012.3,5998,6345,6789,7123,5823
sustained_load_heavy,60234,6000,5945,55,98.72,6023.4,6001,6567,7234,7890,5789
...
```

## Resource Monitoring Output

### Memory Usage Timeline

```
Time (s)  | Memory (MB) | Processes | ETS Tables | Run Queue
----------|-------------|-----------|------------|----------
0         | 45.67       | 234       | 12         | 0
5         | 46.12       | 256       | 12         | 2
10        | 46.78       | 289       | 13         | 5
15        | 47.23       | 312       | 13         | 8
20        | 47.56       | 298       | 13         | 4
25        | 47.89       | 276       | 13         | 2
30        | 48.01       | 265       | 13         | 1
```

### Scheduler Utilization

```
Scheduler | Avg Util (%) | Max Util (%) | Min Util (%)
----------|--------------|--------------|-------------
1         | 67.4         | 98.2         | 23.1
2         | 65.8         | 96.7         | 21.5
3         | 68.2         | 99.1         | 24.3
4         | 66.9         | 97.5         | 22.8
5         | 67.1         | 98.8         | 23.6
6         | 66.5         | 97.2         | 22.1
7         | 68.7         | 99.5         | 25.2
8         | 67.3         | 98.4         | 23.4

Average:   67.2%
```

## Failure Scenario Output

### Process Crash During Load

```
[TSTMP] load_test_SUITE:process_crash_during_load/1: Testing process crash handling during load...
[TSTMP] Spawned load test asynchronously
[TSTMP] Waiting for steady state (5s)...
[TSTMP] Injecting failures...
[TSTMP] Injected 10 failures
[TSTMP] Waiting for completion...
[TSTMP] Workflows completed despite failures: 42
[TSTMP] Result after failures:
  Started: 50
  Completed: 42
  Failed: 8
  Recovery rate: 84%

==> Test case: process_crash_during_load - PASSED
```

### Cascading Failure Recovery

```
[TSTMP] load_test_SUITE:cascading_failure_recovery/1: Testing cascading failure recovery...
[TSTMP] Initial failures injected: 5
[TSTMP] Cascading failures detected: 3
[TSTMP] Total failures: 8
[TSTMP] Recovery initiated...
[TSTMP] Recovery time: 1234 ms
[TSTMP] Workflows recovered: 39 / 50
[TSTMP] Recovery success rate: 78%

==> Test case: cascading_failure_recovery - PASSED
```

## Degradation Analysis Output

### Throughput Degradation

```
Load (wf) | Throughput (wf/s) | Degradation (%)
----------|-------------------|----------------
10        | 45.23            | 0%
50        | 198.41           | +338% (scaling)
100       | 356.78           | +80% from 50
200       | 412.33           | +16% from 100
500       | 398.12           | -3.4% from 200

Analysis:
- Linear scaling up to 100 workflows
- Performance plateau at 200-500 workflows
- Slight degradation at 500 (likely saturation)
- Recommended operating range: 10-200 workflows
```

### Latency Degradation

```
Load (wf) | Avg Latency (ms) | P95 Latency (ms) | Degradation (%)
----------|------------------|------------------|----------------
10        | 22.1             | 35               | 0%
50        | 25.2             | 48               | +14% avg, +37% P95
100       | 28.1             | 89               | +27% avg, +154% P95
200       | 121.3            | 345              | +449% avg, +886% P95
500       | 251.2            | 678              | +1036% avg, +1837% P95

Warning: P95 latency degradation exceeds 50% threshold at 100+ workflows
Recommendation: Optimize for lower latency or increase resources
```

## Performance Profiling Output

### Memory Leak Detection

```erlang
1> load_test_scenarios:detect_memory_leaks(sequence, 1000).

Running 1000 iterations of sequence pattern...
Initial memory: 45.67 MB
After 250 iterations: 46.12 MB (+0.45 MB)
After 500 iterations: 46.34 MB (+0.22 MB)
After 750 iterations: 46.51 MB (+0.17 MB)
After 1000 iterations: 46.62 MB (+0.11 MB)

Final memory: 46.62 MB
Total growth: 0.95 MB
Growth per iteration: 0.00095 MB

Result: false (No leak detected - growth < 10 MB threshold)
```

### GC Impact Analysis

```erlang
2> load_test_scenarios:measure_gc_impact(1000).

Running 1000 workflow executions...

GC Statistics:
- GC count: 42 collections
- Words reclaimed: 1,234,567 words
- MB reclaimed: 9.456 MB
- Avg words per GC: 29,394 words
- GC frequency: 23.8 executions per GC

Analysis:
- Efficient memory reclamation
- Low GC frequency
- Predictable GC behavior
```

## CI/CD Integration Output

### GitHub Actions

```yaml
Run Load Tests
  docker buildx bake --load
  docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
    rebar3 ct --suite=test/load/load_test_SUITE --group=concurrent_execution

  Building cre:0.3.0...
  [+] Building 45.2s (12/12) FINISHED

  Running tests...
  ===> Running Common Test suites...

  Results:
    5 test cases: 5 ok, 0 failed, 0 skipped
    All tests successful.

  Test Summary:
    Total Duration: 3m 42s
    Workflows Tested: 1,660
    Success Rate: 99.4%
    Throughput: 312 wf/s
    P95 Latency: 156 ms

  ✅ Load tests passed
```

### GCP Cloud Build

```
BUILD
Step 1/5: Build Docker image
Step 2/5: Run compilation
Step 3/5: Run load tests
  Starting load tests...
  Running concurrent_execution group...

  Test Results:
    concurrent_workflows_10: PASS (221 ms)
    concurrent_workflows_50: PASS (252 ms)
    concurrent_workflows_100: PASS (281 ms)
    concurrent_workflows_500: PASS (1213 ms)
    concurrent_workflows_1000: PASS (2512 ms)

  All tests passed!

Step 4/5: Generate report
  Report generated: gs://cre-test-results/load_test_report_20250211.txt

Step 5/5: Upload artifacts
  Artifacts uploaded to Cloud Storage

BUILD SUCCESS
Duration: 8m 23s
```

## Troubleshooting Output

### Test Failure Example

```
[TSTMP] load_test_SUITE:concurrent_workflows_1000/1: Running 1000 concurrent sequence workflows
[TSTMP] Spawned 1000 workflow processes
[ERROR] Process <0.1234.0> crashed: {badmatch,{error,system_limit}}
[ERROR] Multiple process spawn failures
[TSTMP] Test complete: 985 completed, 15 failed, throughput: 398.12 wf/s

==> Test case: concurrent_workflows_1000 - PASSED (within acceptable failure rate)

Analysis:
- Hit system process limit
- Recommend increasing +P flag or reducing concurrent load
- 98.5% success rate acceptable for stress test
```

### Memory Warning Example

```
[TSTMP] load_test_SUITE:memory_usage_under_load/1: Testing memory usage under load...
[TSTMP] Initial memory: 45.67 MB
[TSTMP] Running sustained load...
[TSTMP] Memory at 15s: 62.34 MB (+16.67 MB)
[TSTMP] Memory at 30s: 78.91 MB (+33.24 MB)
[TSTMP] Memory at 45s: 95.67 MB (+50.00 MB)
[TSTMP] Memory at 60s: 96.12 MB (+50.45 MB)
[TSTMP] Final memory: 96.12 MB
[TSTMP] Memory growth: 50.45 MB

[WARNING] Memory growth at threshold limit (50 MB)
[WARNING] Monitor for continued growth in longer tests

==> Test case: memory_usage_under_load - PASSED (at threshold)
```

## Example Report Summary

After running the full suite, you would see a summary like:

```
================================================================================
CRE Load Testing Suite - Execution Summary
================================================================================

Test Execution Time: 25 minutes 42 seconds
Total Test Cases: 24
Passed: 23
Failed: 1
Skipped: 0

Key Metrics:
- Maximum Concurrent Workflows: 1000 (98.5% success)
- Peak Throughput: 412.33 workflows/second
- Average P95 Latency: 234 ms
- Memory Stability: PASS (50 MB growth, at threshold)
- System Recovery Time: 12.4 seconds

Performance Grade: B+
- Excellent: Throughput, scaling behavior
- Good: Latency, failure resilience
- Fair: Memory usage (at threshold)
- Improvement Needed: High-load latency optimization

Recommendations:
1. Optimize memory usage to reduce growth
2. Investigate latency spikes at 200+ concurrent workflows
3. Consider horizontal scaling for workloads > 500 workflows
4. Implement connection pooling for database operations

Full Report: _build/test/logs/ct_run.*/load_test_results/load_test_report.txt
CSV Data: _build/test/logs/ct_run.*/load_test_results/load_test_results.csv

================================================================================
```

---

**Note:** The actual output will vary based on your system's performance, resource availability, and the specific CRE workflow patterns being tested. These examples are representative of the expected output format and metrics.
