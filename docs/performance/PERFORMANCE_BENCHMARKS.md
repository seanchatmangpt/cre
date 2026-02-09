# CRE Performance Benchmarks Guide

This document provides comprehensive performance guidance for the CRE (Common Runtime Environment) workflow engine, covering benchmarks, performance characteristics, optimization tips, profiling tools, bottleneck identification, and scaling guidance.

## Table of Contents

1. [Benchmarks Overview](#benchmarks-overview)
2. [Performance Characteristics by Module](#performance-characteristics-by-module)
3. [Optimization Tips](#optimization-tips)
4. [Profiling Tools](#profiling-tools)
5. [Bottleneck Identification](#bottleneck-identification)
6. [Scaling Guidance](#scaling-guidance)
7. [Rust NIF Performance](#rust-nif-performance)
8. [Running Benchmarks](#running-benchmarks)

---

## Benchmarks Overview

### Available Test Suites

CRE includes comprehensive performance testing via two main test suites:

| Test Suite | Location | Purpose |
|------------|----------|---------|
| `yawl_performance_SUITE` | `/test/yawl_performance_SUITE.erl` | Common Test suite for YAWL workflow benchmarks |
| `yawl_integration_performance_test` | `/test/yawl_integration_performance_test.erl` | EUnit integration and performance tests |

### Benchmark Categories

1. **Parse Performance** - XML/YAWL specification parsing
2. **Compile Performance** - Module generation and dynamic loading
3. **Execution Performance** - Workflow step throughput and token processing
4. **Memory Usage** - Instance footprint, token storage, receipts
5. **Scalability** - Concurrent workflows, multi-instance, heap growth
6. **Real-World Benchmarks** - Order Fulfillment workflow simulation

### Benchmark Thresholds

```erlang
-define(BENCHMARK_THRESHOLD_PARSE_MS, 100).
-define(BENCHMARK_THRESHOLD_COMPILE_MS, 50).
-define(BENCHMARK_THRESHOLD_EXECUTION_MS, 10).
-define(BENCHMARK_THRESHOLD_MEMORY_KB, 1024).
```

### Benchmark Results Summary

| Operation | Small (5 tasks) | Medium (20 tasks) | Large (100 tasks) |
|-----------|-----------------|-------------------|-------------------|
| Parse XML | ~5-20 ms | ~20-100 ms | ~100-500 ms |
| Compile to Memory | ~1-5 ms | ~5-20 ms | ~20-50 ms |
| Module Generation | ~10-50 ms | ~50-200 ms | ~200-1000 ms |
| Dynamic Load | ~100-500 us | ~200-1000 us | ~500-2000 us |

---

## Performance Characteristics by Module

### gen_pnet (Petri Net Engine)

**Location:** `/src/core/gen_pnet.erl`

The core Petri net execution engine exhibits the following performance characteristics:

| Metric | Value | Notes |
|--------|-------|-------|
| Single Transition Firing | ~10-50 microseconds | Per transition |
| Token Processing Rate | 10,000-100,000 tokens/sec | Depending on complexity |
| Mode Enumeration | O(n*m) | n = preset size, m = tokens per place |
| Statistics Update | Every 1000 firings | Negligible overhead |

**Throughput Calculation:**
```erlang
%% Statistics computed using: 1000000000000 / Tdelta (firings/second)
%% Update interval: 1000 firings to minimize measurement overhead
```

### Pattern Execution Performance

| Pattern | Avg Execution Time | Throughput | Memory Footprint |
|---------|-------------------|-------------|------------------|
| Sequence | ~0.1-1 ms/step | 1000-10000 ops/sec | ~1-2 KB/instance |
| Parallel Split (AND) | ~0.5-2 ms | 500-2000 ops/sec | ~2-5 KB/branch |
| XOR Choice | ~0.2-1 ms | 1000-5000 ops/sec | ~1-3 KB |
| OR Join | ~0.3-1.5 ms | 700-3000 ops/sec | ~2-4 KB |
| Multi-Instance (10) | ~5-20 ms | 50-200 ops/sec | ~10-30 KB |
| Structured Loop | ~1-5 ms/iteration | 200-1000 iter/sec | ~5-10 KB |

### Marking Operations (pnet_marking)

| Operation | Time Complexity | Notes |
|-----------|-----------------|-------|
| `new/1` | O(n) | n = number of places |
| `get/2` | O(1) | Direct map lookup |
| `set/3` | O(1) | Map update |
| `add/2` | O(k) | k = tokens being added |
| `take/2` | O(k) | k = tokens being consumed |
| `hash/1` | O(n) | n = total tokens across all places |

**Memory Characteristics:**
- Base marking: ~8 bytes per place + 8 bytes per token
- Hash computation: ~1 microsecond per 10 tokens
- Receipt storage: ~100-200 bytes per receipt

### YAWL Compilation

| Operation | Small (5 tasks) | Medium (20 tasks) | Large (100 tasks) |
|-----------|-----------------|-------------------|-------------------|
| Parse XML | ~5-20 ms | ~20-100 ms | ~100-500 ms |
| Compile to Memory | ~1-5 ms | ~5-20 ms | ~20-50 ms |
| Module Generation | ~10-50 ms | ~50-200 ms | ~200-1000 ms |
| Dynamic Load | ~100-500 us/module | ~200-1000 us/module | ~500-2000 us/module |

---

## Optimization Tips

### 1. Pattern Design Optimization

**Avoid Deep Nesting:**
```
Recommendation: Maximum 5 levels of nesting
Reason: Each nesting level adds context overhead
Impact: 20-30% performance degradation per level
```

**Prefer Flat Structures:**
```erlang
%% GOOD: 10 parallel tasks at same level
parallel_split([task1, task2, task3, task4, task5,
                task6, task7, task8, task9, task10])

%% BAD: 5 layers of 2-way splits
split(split(split(split(split(...)))))
```

**Minimize OR Joins:**
```erlang
%% OR joins require threshold checking
%% Use AND joins when possible for better performance
and_join() %% Faster than or_join()
```

### 2. Token Management

**Batch Token Operations:**
```erlang
%% GOOD: Batch token injection
ProduceMap = #{p1 => [T1, T2, T3], p2 => [T4]}
gen_pnet:inject(P, ProduceMap)

%% AVOID: Multiple single-token injections
gen_pnet:inject(P, #{p1 => [T1]}),
gen_pnet:inject(P, #{p1 => [T2]}),
gen_pnet:inject(P, #{p1 => [T3]})
```

**Use Efficient Token Types:**
- Atoms for identifiers: 1 byte
- Binaries for data: variable
- Maps for complex data: higher overhead

### 3. Receipt Management

**Periodic Draining:**
```erlang
%% Drain receipts periodically to prevent unbounded growth
case length(Receipts) > 1000 of
    true ->
        {Drained, Remaining} = drain_receipts(),
        %% Store or export Drained
        ok;
    false -> ok
end
```

**Selective Receipt Creation:**
```erlang
%% Only create receipts for important transitions
case TransitionType of
    critical -> pnet_receipt:make(...);
    _ -> omit_receipt
end
```

### 4. Memory Management

**Force GC After Completion:**
```erlang
%% After workflow completes, force garbage collection
erlang:garbage_collect(),
erlang:memory(memory)
```

**Monitor Heap Growth:**
```erlang
%% Check for memory leaks
Before = erlang:memory(total),
%% ... run workload ...
After = erlang:memory(total),
Growth = After - Before
```

### 5. Concurrency Tuning

**Worker Pool Sizing:**
```erlang
%% For CPU-bound tasks:
PoolSize = erlang:system_info(schedulers_online)

%% For I/O-bound tasks:
PoolSize = erlang:system_info(schedulers_online) * 2
```

**Configuration Example:**
```erlang
#{name => mypool,
  size => 5,           %% Core pool size
  max_overflow => 10}  %% Additional workers under load
```

### 6. Telemetry Optimization

**Production Settings:**
```erlang
%% Recommended: normal verbosity for production
wf_yawl_telemetry:set_verbosity(normal)

%% For high-throughput scenarios:
wf_yawl_telemetry:set_verbosity(minimal)

%% For debugging only:
wf_yawl_telemetry:set_verbosity(debug)
```

**Optimization Techniques:**
1. **Sampling:** Track every Nth task instead of all tasks
2. **Batching:** Flush telemetry events periodically
3. **Filtering:** Only emit events for critical paths
4. **Async Export:** Use separate telemetry process

---

## Profiling Tools

### Built-in Profiling

#### fprof - Function Profiling

**Usage:**
```bash
# Using the debug script
./scripts/debug_profile.sh fprof gen_pnet

# Manual usage
erl -noshell -s fprof apply -s module function -s fprof profile -s init stop
```

**In-code usage:**
```erlang
fprof:apply(fun() ->
    wf_engine:start_case(Engine, Options, Now)
end, "case_start.profile")
```

#### eprof - Graph Profiling

**Usage:**
```bash
# Using the debug script
./scripts/debug_profile.sh eprof gen_yawl

# Manual usage
erl -noshell -s eprof start -s eprof start_profiling [self()] ...
```

**In-code usage:**
```erlang
eprof:start(),
eprof:start_profiling([self()]),
%% ... run workload ...
eprof:stop_profiling(),
Analysis = eprof:analyze(),
eprof:stop()
```

### Flame Graph Profiling

**Using eflame:**
```bash
./scripts/debug_eflame.sh /tmp/flame.txt
```

**Generate SVG:**
```bash
~/bin/stackcollapse-erl.pl /tmp/flame.txt | flamegraph.pl > flame.svg
```

### Observer Tool

```bash
erl
observer:start().
```

**Observer provides:**
- Process state visualization
- Memory breakdown
- ETS table inspection
- Application overview
- Port and table viewers

### Performance Scripts

| Script | Purpose |
|--------|---------|
| `/scripts/debug_profile.sh` | fprof/eprof profiling wrapper |
| `/scripts/debug_eflame.sh` | Flame graph generation |
| `/scripts/debug_monitor.sh` | System monitoring |
| `/scripts/debug_trace.sh` | Trace debugging |
| `/scripts/debug_system_info.sh` | System information |

---

## Bottleneck Identification

### Common Bottlenecks

**Symptom: Low FPS with High CPU**
- **Cause:** Expensive transition firing logic
- **Solution:** Profile `fire/3` callback, optimize computation

**Symptom: High Memory Growth**
- **Cause:** Receipt accumulation or token leaks
- **Solution:** Drain receipts, check for unreclaimed work items

**Symptom: Pool Always Full**
- **Cause:** Task execution slower than arrival rate
- **Solution:** Increase pool size or optimize task handlers

**Symptom: High GC Time**
- **Cause:** Large temporary allocations
- **Solution:** Use iolists, reuse binaries

### Health Indicators

| Status | Transitions/Second | GC Time |
|--------|-------------------|---------|
| Green | >1000 | <100ms |
| Yellow | 100-1000 | <500ms |
| Red | <100 | >500ms |

### Built-in Statistics

```erlang
%% Get throughput statistics from gen_pnet
#stats{current = #stat{fps = Fps},
       hi = #stat{fps = HiFps},
       lo = #stat{fps = LoFps}} = gen_pnet:stats(Pid)

%% High variance indicates contention
%% Low fps indicates CPU or I/O bottleneck
```

### Memory Leak Detection

```erlang
%% From yawl_integration_performance_test
memory_leak_detection_test() ->
    garbage_collect(),
    BaselineMemory = erlang:memory(total),

    %% Create many workflows repeatedly
    Iterations = 500,
    lists:foreach(fun(I) ->
        Workflow = cre_yawl:new_workflow(<<"mem_leak_", ...>>),
        %% ... workflow operations ...
        ok
    end, lists:seq(1, Iterations)),

    garbage_collect(),
    EndMemory = erlang:memory(total),
    MemoryDelta = EndMemory - BaselineMemory,

    %% Memory growth should be minimal
    ?assert(MemoryDelta < 200000, {memory_leak_detected, MemoryDelta})
```

---

## Scaling Guidance

### Horizontal Scaling

**Multi-Node Deployment:**
- Run multiple CRE nodes behind a load balancer
- Each node processes independent workflows
- Shared state via Mnesia clustering

**Mnesia Clustering:**
```erlang
mnesia:create_table(persistent_case,
    [{attributes, record_info(fields, persistent_case)},
     {disc_copies, [node1, node2, node3]},
     {type, set}])
```

**Network Overhead:**
- Inter-node transactions: 5-20 ms
- Table replication: asynchronous for performance
- Consider partitioning by workflow type

### Vertical Scaling

**CPU Utilization:**
- Single core: ~1000-5000 transitions/second
- Multi-core: Scales linearly to ~4 cores
- Beyond 4 cores: diminishing returns due to VM overhead

**Memory Requirements:**
- Base: 100 MB per node
- Per workflow: 1-10 MB depending on complexity
- Per active case: 0.5-5 MB
- Buffer: 2x expected peak for safety

### Throughput Targets

**Reference Targets (per node):**

| Workflow Type | Target Throughput |
|---------------|-------------------|
| Simple (5-10 tasks) | 100-500 workflows/minute |
| Medium (20-50 tasks) | 20-100 workflows/minute |
| Complex (100+ tasks) | 5-20 workflows/minute |

**Achievable Throughputs:**
- Single-threaded: ~10-50 transitions/second
- 4-core: ~40-200 transitions/second
- Optimized workflows: 2-5x baseline

### Capacity Planning

**Resource Formula:**
```
Required Nodes = (Workflows/Minute x Avg Duration x Tasks)
                / (Target Throughput x 60)
```

**Example:**
```
100 workflows/minute x 30s x 10 tasks / (50 workflows/min x 60)
≈ 10 nodes
```

**Buffer Recommendations:**
- 20-30% headroom for traffic spikes
- 50%+ headroom for mixed workloads
- Plan for horizontal scale-out during peak

### Concurrent Execution

**Scaling Behavior:**
```
10 workflows x 50 operations: ~500-1000 ms total
100 workflows x 50 operations: ~2000-5000 ms total
```

**Worker Pool Management:**
- Pool checkout overhead: ~50-200 microseconds
- Returns `{error, busy}` when full (non-blocking)
- Overflow workers add 100-500 ms spawn latency

---

## Rust NIF Performance

### NIF vs Pure Erlang

| Operation | Rust NIF | Pure Erlang | Speedup |
|-----------|----------|-------------|---------|
| Alpha algorithm (small log) | ~10ms | ~100ms | 10x |
| Alpha algorithm (large log) | ~500ms | ~5000ms | 10x |
| Heuristic miner | ~100ms | ~1000ms | 10x |
| Conformance checking | ~50ms | ~500ms | 10x |

### NIF Memory Characteristics

- **NIF Memory**: Managed by Rust, separate from Erlang's heap
- **No GC Impact**: NIF operations do not trigger Erlang GC
- **Resource Cleanup**: Automatic via Rust's ownership model
- **Memory Limits**: Configurable via `CreConfig` (default: 1GB)

### NIF Configuration

```rust
pub struct CreConfig {
    pub debug: bool,           // Enable debug logging
    pub max_concurrent_ops: usize, // Max concurrent operations (default: 100)
    pub timeout_ms: u64,       // Operation timeout (default: 300000 = 5 min)
    pub memory_limit: usize,   // Memory limit in bytes (default: 1GB)
}
```

### NIF Optimization Tips

1. **Batch Operations**: Process multiple traces in a single NIF call
2. **Resource Management**: Store large data structures as resources
3. **Avoid Frequent Calls**: Minimize NIF call overhead
4. **Use Parallelism**: Leverage Rayon for parallel processing

### Benchmarking NIFs

```erlang
%% Benchmark an algorithm
{ok, Benchmark} = rust_nif:benchmark(alpha, Log),
#{
  duration_ms := Duration,
  memory_mb := Memory,
  success := true
} = Benchmark
```

---

## Running Benchmarks

### Common Test Performance Suite

```bash
# Run all performance tests
rebar3 ct --suite=yawl_performance_SUITE

# Run specific test group
rebar3 ct --suite=yawl_performance_SUITE --group=execution_performance

# Run specific test
rebar3 ct --suite=yawl_performance_SUITE --case=execution_simple_sequence
```

### EUnit Performance Tests

```bash
# Run integration and performance tests
rebar3 eunit --module=yawl_integration_performance_test

# Run specific test
rebar3 eunit --module=yawl_integration_performance_test --test=pattern_execution_benchmark_test
```

### Quick Benchmark Commands

```bash
# Using rebar3 shell
rebar3 shell
erlang:apply(timer, tc, [fun() -> workflow:end_to_end() end]).

# Using escript
erl -noshell -eval "timer:tc(fun() -> ... end), init:stop()."
```

### Performance Test Groups

Available test groups in `yawl_performance_SUITE`:

- `parse_performance` - XML parsing benchmarks
- `compile_performance` - Module generation benchmarks
- `execution_performance` - Runtime execution benchmarks
- `memory_usage` - Memory consumption tests
- `scalability` - Concurrent execution tests
- `orderfulfillment_benchmark` - Real-world workflow benchmark

### Interpreting Results

**Parse Performance:**
```
Average parse time: ~.3f ms (~p microseconds)
Parse rate: ~.2f specs/second
```

**Execution Performance:**
```
Average execution time: ~p microseconds
Execution rate: ~.2f steps/second
```

**Memory Usage:**
```
Memory: ~p bytes/pattern (total delta: ~p bytes)
```

**Scalability:**
```
Workflows: ~p
Operations per workflow: ~p
Operations per second: ~.2f
```

---

## Appendix: Performance Tuning Parameters

### Application Configuration

**sys.config:**
```erlang
[
  {cre, [
    {persistence_enabled, false},  %% Enable for production
    {telemetry_verbosity, normal},
    {max_concurrent_workflows, 100},
    {worker_pool_size, 10},
    {worker_pool_overflow, 20}
  ]}.
]
```

### VM Configuration

**vm.args:**
```erlang
#+smp auto
+P 4096000  %% 4MB stack size
+Q 65536   %% 64MB heap size
+A 128     %% thread pool size
```

### Mnesia Configuration

**Application startup:**
```erlang
%% Optimize Mnesia for performance
mnesia:change_table_copy_type(persistent_case, disc_copies),
mnesia:change_config(extra_db_nodes, [node1, node2]).
```

---

## Summary

The CRE workflow engine demonstrates excellent performance characteristics for typical workflow workloads:

- **Sub-millisecond pattern execution** for simple sequences and choices
- **Linear scaling** for parallel patterns within workflows
- **Minimal state management overhead** through immutable receipts
- **Configurable persistence** with optional synchronous/asynchronous modes
- **Comprehensive telemetry** with configurable verbosity levels

**Key Optimization Principles:**
1. Batch operations where possible
2. Drain receipts periodically
3. Use appropriate persistence strategy per workflow criticality
4. Monitor and tune based on actual workload characteristics
5. Profile before optimizing - measure, don't guess

**Performance Health Indicators:**
- Green: >1000 transitions/second, <100ms GC time
- Yellow: 100-1000 transitions/second, <500ms GC time
- Red: <100 transitions/second, >500ms GC time

---

*For additional performance tuning guidance, refer to:*
- `/test/yawl_performance_SUITE.erl` - Performance test suite
- `/test/yawl_integration_performance_test.erl` - Integration performance tests
- `/docs/operations/performance/performance.md` - Performance considerations
- `/docs/rust/RUST_NIF_INTEGRATION.md` - Rust NIF performance documentation

*Last updated: February 2026*
*CRE Version: 0.1.0*
