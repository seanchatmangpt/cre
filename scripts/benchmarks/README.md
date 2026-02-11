# CRE Performance Benchmarks

Comprehensive performance benchmarking suite for the CRE workflow engine.

## Overview

This benchmark suite measures:

- **Workflow Throughput**: Workflows executed per second at different concurrency levels
- **Task Execution Latency**: Response time distribution (p50, p95, p99 percentiles)
- **Petri Net Operations**: Performance of core primitives (marking hash, choice selection, etc.)
- **Memory Usage**: Memory consumption under varying load
- **Scalability**: System behavior from 1 to 1000+ concurrent workflows

## Quick Start

### Using Docker (Recommended)

```bash
# Build the CRE image
docker buildx bake --load

# Run benchmarks in container
docker run -it --rm \
  -v $(pwd):/work \
  -w /work \
  cre:0.3.0 \
  /work/scripts/benchmarks/run_benchmarks.sh
```

### Running Individual Benchmarks

```bash
# Run all benchmarks
./scripts/benchmarks/cre_benchmark.erl all

# Run specific benchmarks
./scripts/benchmarks/cre_benchmark.erl throughput
./scripts/benchmarks/cre_benchmark.erl latency
./scripts/benchmarks/cre_benchmark.erl memory
./scripts/benchmarks/cre_benchmark.erl pnet
./scripts/benchmarks/cre_benchmark.erl scalability
```

## Benchmark Components

### 1. Workflow Throughput Benchmark

Measures workflows per second at different concurrency levels:

- 1 concurrent workflow
- 10 concurrent workflows
- 100 concurrent workflows
- 1,000 concurrent workflows

**Metrics:**
- Total throughput (operations/second)
- Workflows per second
- Per-workflow efficiency
- Execution time

### 2. Task Execution Latency Benchmark

Collects 10,000 latency samples and calculates:

- Minimum latency
- Mean latency
- Median (p50)
- 95th percentile (p95)
- 99th percentile (p99)
- 99.9th percentile (p999)
- Maximum latency

### 3. Petri Net Operations Benchmark

Micro-benchmarks for core Petri net primitives:

- **Marking Hash**: Cryptographic hash calculation for markings
- **Marking Merge**: Combining two markings
- **Choice Selection**: Selecting from multiple enabled transitions

Tested at different scales (10, 50, 100, 500 places/choices).

### 4. Memory Usage Benchmark

Measures memory consumption:

- Baseline memory
- Single workflow instance footprint
- Multiple instance scaling (100 instances)
- Memory growth under sustained load (1,000 operations)

### 5. Scalability Benchmark

Tests system scalability:

- Workflow startup time at different scales
- Concurrent execution behavior
- Resource utilization patterns

## Output

### Console Output

Benchmarks print detailed results to console:

```
╔════════════════════════════════════════════════════════════════╗
║           CRE Performance Benchmark Suite v0.3.0              ║
╚════════════════════════════════════════════════════════════════╝

System Information:
  Timestamp:        2026-02-11 14:30:00
  Erlang/OTP:       28
  ERTS Version:     15.0
  Schedulers:       8
  System Memory:    16.00 GB

...
```

### Result Files

- `benchmark_results/benchmark_TIMESTAMP.txt` - Full text results
- `benchmark_results/csv_TIMESTAMP/*.csv` - CSV data for graphing
- `benchmark_graphs/*.png` - Performance graphs (if matplotlib available)

## Generating Graphs

```bash
# Generate graphs from sample data
./scripts/benchmarks/generate_graphs.py --sample-data

# Generate graphs from benchmark results
./scripts/benchmarks/generate_graphs.py \
  --input benchmark_results/benchmark_20260211_143000.txt \
  --output benchmark_graphs
```

### Graph Types

1. **Throughput Scaling** (`throughput_scaling.png`)
   - Throughput vs concurrency
   - Per-workflow efficiency

2. **Latency Distribution** (`latency_distribution.png`)
   - Percentile chart
   - Histogram distribution

3. **Memory Scaling** (`memory_scaling.png`)
   - Memory vs workflow instances
   - Linear fit analysis

4. **Petri Net Operations** (`pnet_operations.png`)
   - Operations per second for each primitive

## Benchmark Configuration

### Quick Benchmark

```bash
./scripts/benchmarks/run_benchmarks.sh --quick
```

Reduces iterations for faster results (useful for CI/CD).

### Full Benchmark

```bash
./scripts/benchmarks/run_benchmarks.sh --full
```

Full iteration counts for accurate measurements (default).

## Performance Targets

Expected performance on modern hardware (8-core, 16GB RAM):

| Metric | Target | Good | Excellent |
|--------|--------|------|-----------|
| Single workflow throughput | > 100 ops/sec | > 500 | > 1000 |
| 100 concurrent workflows | > 1000 ops/sec | > 5000 | > 10000 |
| Median latency (p50) | < 500 μs | < 200 μs | < 100 μs |
| 99th percentile (p99) | < 2000 μs | < 1000 μs | < 500 μs |
| Memory per instance | < 1 MB | < 500 KB | < 100 KB |
| Marking hash (100 places) | > 50,000 ops/sec | > 100,000 | > 200,000 |

## Interpreting Results

### Throughput

- **Linear scaling**: Ideal case, throughput increases proportionally with concurrency
- **Sub-linear scaling**: Normal for concurrent systems due to coordination overhead
- **Saturation**: Throughput plateaus when system resources are fully utilized

### Latency

- **Low p50**: Most operations complete quickly
- **Low p99**: Few outliers, consistent performance
- **High p99**: May indicate GC pauses, scheduling delays, or resource contention

### Memory

- **Linear growth**: Expected pattern, memory increases with workflow count
- **Constant overhead**: Each workflow has fixed memory footprint
- **Memory leaks**: Watch for growth during sustained operations

## Troubleshooting

### Out of Memory

Reduce concurrency levels in benchmark configuration:

```erlang
ConcurrencyLevels = [1, 10, 100]  % Remove 1000
```

### Timeouts

Increase timeout values in `cre_benchmark.erl`:

```erlang
receive
    {done, Worker} -> ok
after 120000 ->  % Increase from 60000
    ...
end
```

### Module Not Found Errors

Ensure project is compiled:

```bash
rebar3 compile
```

## Advanced Usage

### Custom Benchmarks

Add custom benchmark functions to `cre_benchmark.erl`:

```erlang
benchmark_my_feature() ->
    print_section("My Custom Benchmark"),
    %% Your benchmark code here
    ok.
```

### Profiling

Use Erlang profiling tools during benchmarks:

```bash
# Use fprof for profiling
erl -pa _build/default/lib/*/ebin
> fprof:apply(cre_benchmark, benchmark_throughput, []).
> fprof:profile().
> fprof:analyse().
```

## Integration with CI/CD

### GitHub Actions

```yaml
- name: Run Performance Benchmarks
  run: |
    docker buildx bake --load
    docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
      /work/scripts/benchmarks/run_benchmarks.sh --quick
```

### Performance Regression Detection

Compare benchmark results over time:

```bash
# Save baseline
cp benchmark_results/benchmark_latest.txt baseline.txt

# Run new benchmark
./scripts/benchmarks/run_benchmarks.sh

# Compare (manual or automated)
diff baseline.txt benchmark_results/benchmark_latest.txt
```

## Dependencies

### Required

- Erlang/OTP 28+
- rebar3
- CRE compiled (`rebar3 compile`)

### Optional

- Python 3.7+ (for graph generation)
- matplotlib (for graphs)
- numpy (for statistical analysis)

Install optional dependencies:

```bash
pip install matplotlib numpy
```

## Performance Optimization Tips

Based on benchmark results:

1. **High Latency**: Check for blocking operations in callbacks
2. **Low Throughput**: Consider increasing scheduler count
3. **Memory Growth**: Review token/marking cleanup
4. **Scalability Issues**: Examine inter-process messaging patterns

## License

Apache-2.0 (same as CRE project)

## References

- [CRE Documentation](../../docs/)
- [YAWL Patterns](../../docs/patterns/)
- [Erlang Performance Guide](https://www.erlang.org/doc/efficiency_guide/users_guide.html)
