# CRE Benchmark Quick Start

## 30-Second Quick Test

```bash
# Run simple benchmark
./scripts/benchmarks/simple_bench.escript
```

Output:
```
Process Creation:  404,924 processes/sec
Message Passing:   4,757,374 msgs/sec
ETS Insert:        2,680,390 ops/sec
ETS Lookup:        6,268,021 ops/sec
Map Get:           9,908,839 ops/sec
Map Put:           70,077,085 ops/sec
```

## 5-Minute Full Benchmark

```bash
# Run complete benchmark suite
./scripts/benchmarks/cre_benchmark.erl all
```

Tests:
- ✓ Workflow throughput (1, 10, 100, 1000 concurrent)
- ✓ Task latency (p50, p95, p99)
- ✓ Memory usage
- ✓ Petri net operations
- ✓ Scalability

## Docker Workflow (Recommended)

```bash
# 1. Build image
docker buildx bake --load

# 2. Run benchmarks
docker run -it --rm \
  -v $(pwd):/work \
  -w /work \
  cre:0.3.0 \
  /work/scripts/benchmarks/run_benchmarks.sh

# 3. View results
cat benchmark_results/benchmark_*.txt
```

## Generate Graphs

```bash
# Install dependencies
pip install matplotlib numpy

# Generate graphs
./scripts/benchmarks/generate_graphs.py --sample-data

# View graphs
ls benchmark_graphs/
```

## What to Expect

### Good Performance
- Throughput: > 1,000 ops/sec
- Latency p50: < 200 μs
- Memory per workflow: < 500 KB

### Excellent Performance
- Throughput: > 10,000 ops/sec
- Latency p50: < 100 μs
- Memory per workflow: < 100 KB

## Troubleshooting

### "rebar3: command not found"
```bash
# Use Docker instead
docker run -it --rm -v $(pwd):/work -w /work cre:0.3.0 sh
```

### "Module not found"
```bash
# Compile project first
rebar3 compile
# Or in Docker
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile
```

### Out of memory
```bash
# Reduce concurrency in cre_benchmark.erl
# Change: ConcurrencyLevels = [1, 10, 100, 1000]
# To:     ConcurrencyLevels = [1, 10, 100]
```

## Next Steps

- Read [README.md](README.md) for detailed documentation
- Review [BENCHMARK_RESULTS.md](BENCHMARK_RESULTS.md) for analysis
- Check existing performance tests in `test/yawl_performance_SUITE.erl`

## Common Commands

```bash
# Run specific benchmark
./scripts/benchmarks/cre_benchmark.erl throughput
./scripts/benchmarks/cre_benchmark.erl latency
./scripts/benchmarks/cre_benchmark.erl memory

# Quick benchmark
./scripts/benchmarks/simple_bench.escript

# Full benchmark with graphs
./scripts/benchmarks/run_benchmarks.sh --full

# Save results
./scripts/benchmarks/cre_benchmark.erl all > results.txt 2>&1
```

## Performance Goals

Target for GCP Marketplace deployment:

| Metric | Target |
|--------|--------|
| Workflow throughput | > 5,000 wf/sec |
| Task latency (p99) | < 1 ms |
| Memory per instance | < 500 KB |
| Startup time | < 100 ms |
| Scalability | Linear to 100 concurrent |

Run benchmarks to verify!
