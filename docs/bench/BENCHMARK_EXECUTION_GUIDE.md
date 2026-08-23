# CRE Benchmark Execution Guide

This guide explains how to run and interpret the comprehensive benchmark suite for CRE.

## Overview

The CRE benchmark suite measures performance across four key areas:

1. **Pattern Benchmarks** (`pattern_benchmarks.erl`)
   - All 43 YAWL workflow control patterns
   - Execution time, memory usage, throughput

2. **Mining Benchmarks** (`mining_benchmarks.erl`)
   - Alpha, Heuristic, Inductive, and other algorithms
   - Scalability across different log sizes

3. **Compilation Benchmarks** (`compilation_benchmarks.erl`)
   - YAWL to Petri net compilation performance
   - Parsing, compilation, code generation times

4. **NIF Benchmarks** (`nif_benchmarks.erl`)
   - Rust NIF vs Pure Erlang comparison
   - Speedup and overhead measurements

## Running Benchmarks

### Prerequisites

```bash
# Ensure dependencies are installed
rebar3 compile

# Ensure jsone is available for JSON output
rebar3 deps
```

### Individual Benchmark Suites

```bash
# Start Erlang shell with benchmark modules loaded
rebar3 shell

# Compile benchmarks
c(test/bench/pattern_benchmarks).
c(test/bench/mining_benchmarks).
c(test/bench/compilation_benchmarks).
c(test/bench/nif_benchmarks).
```

#### Pattern Benchmarks

```erlang
% Start the benchmark server
{ok, Pid} = pattern_benchmarks:start_link().

% Run all pattern benchmarks
{ok, Results} = pattern_benchmarks:run_all_benchmarks().

% Run a specific pattern
{ok, Result} = pattern_benchmarks:run_pattern_benchmark(sequence).

% Run a pattern group
{ok, Results} = pattern_benchmarks:run_pattern_group(basic_control_flow).

% Compare to baseline
{ok, Comparison} = pattern_benchmarks:compare_to_baseline().

% Export results
ok = pattern_benchmarks:export_results("test/bench/results/pattern_results.json").
```

#### Mining Benchmarks

```erlang
% Start the benchmark server
{ok, Pid} = mining_benchmarks:start_link().

% Run all mining benchmarks
{ok, Results} = mining_benchmarks:run_all_benchmarks().

% Run specific algorithm
{ok, Results} = mining_benchmarks:run_algorithm_benchmark(alpha).

% Run specific log size
{ok, Result} = mining_benchmarks:run_size_benchmark(alpha, medium).

% Export results
ok = mining_benchmarks:export_results("test/bench/results/mining_results.json").
```

#### Compilation Benchmarks

```erlang
% Start the benchmark server
{ok, Pid} = compilation_benchmarks:start_link().

% Run all compilation benchmarks
{ok, Results} = compilation_benchmarks:run_all_benchmarks().

% Run specific spec size
{ok, Result} = compilation_benchmarks:run_spec_benchmark(medium).

% Benchmark actual YAWL file
{ok, Result} = compilation_benchmarks:benchmark_yawl_file("test/fixtures/orderfulfillment_2_1.yawl").

% Export results
ok = compilation_benchmarks:export_results("test/bench/results/compilation_results.json").
```

#### NIF Benchmarks

```erlang
% Start the benchmark server
{ok, Pid} = nif_benchmarks:start_link().

% Check if NIF is available
Available = nif_benchmarks:check_nif_available().

% Run all NIF benchmarks
{ok, Results} = nif_benchmarks:run_all_benchmarks().

% Compare NIF vs pure Erlang
{ok, Result} = nif_benchmarks:compare_nif_vs_pure(alpha_discovery).

% Export results
ok = nif_benchmarks:export_results("test/bench/results/nif_results.json").
```

## Automated CI Execution

### Common Test Suite

Create `test/bench/benchmark_SUITE.erl`:

```erlang
-module(benchmark_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    run_pattern_benchmarks/1,
    run_mining_benchmarks/1,
    run_compilation_benchmarks/1,
    run_nif_benchmarks/1
]).

all() ->
    [run_pattern_benchmarks, run_mining_benchmarks,
     run_compilation_benchmarks, run_nif_benchmarks].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cre),
    Config.

end_per_suite(_Config) ->
    ok.

run_pattern_benchmarks(_Config) ->
    {ok, Pid} = pattern_benchmarks:start_link(),
    {ok, _} = pattern_benchmarks:run_all_benchmarks(),
    gen_server:stop(Pid).

run_mining_benchmarks(_Config) ->
    {ok, Pid} = mining_benchmarks:start_link(),
    {ok, _} = mining_benchmarks:run_all_benchmarks(),
    gen_server:stop(Pid).

run_compilation_benchmarks(_Config) ->
    {ok, Pid} = compilation_benchmarks:start_link(),
    {ok, _} = compilation_benchmarks:run_all_benchmarks(),
    gen_server:stop(Pid).

run_nif_benchmarks(_Config) ->
    {ok, Pid} = nif_benchmarks:start_link(),
    {ok, _} = nif_benchmarks:run_all_benchmarks(),
    gen_server:stop(Pid).
```

Run with:

```bash
rebar3 ct --suite benchmark_SUITE
```

## Interpreting Results

### Baseline Comparison

The `compare_to_baseline()` function returns:

```erlang
#{
    status => compared,
    baseline_created => <<"2026-02-09T00:00:00Z">>,
    comparisons => #{
        sequence => #{
            status => ok | warning | regression | improvement,
            current_us => 52.0,
            baseline_us => 50.0,
            diff_percent => 4.0
        },
        ...
    },
    regressions => []
}
```

### Status Values

- **ok**: Within 10% of baseline
- **warning**: 10-20% slower than baseline
- **regression**: >20% slower than baseline
- **improvement**: >10% faster than baseline

### Key Metrics

| Metric | Description | Good Range |
|--------|-------------|------------|
| `avg_time_us` | Average execution time | Lower is better |
| `throughput_per_sec` | Operations per second | Higher is better |
| `memory_bytes` | Memory per operation | Lower is better |
| `speedup` | NIF vs Pure Erlang ratio | >1.0 means NIF is faster |

## Updating Baselines

After significant performance improvements:

```erlang
% Run benchmarks and save as new baseline
{ok, _} = pattern_benchmarks:start_link(),
{ok, _} = pattern_benchmarks:run_all_benchmarks(),
pattern_benchmarks:save_baseline().
```

## Regression Detection in CI

Add to GitHub Actions `.github/workflows/benchmark.yml`:

```yaml
name: Benchmarks

on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
      - name: Compile
        run: rebar3 compile
      - name: Run benchmarks
        run: rebar3 ct --suite benchmark_SUITE
      - name: Check regressions
        run: |
          rebar3 shell -eval "
            {ok, Pid} = pattern_benchmarks:start_link(),
            {ok, C} = pattern_benchmarks:compare_to_baseline(),
            case maps:get(regressions, C, []) of
              [] -> init:stop(0);
              Regressions ->
                io:format("Regressions detected: ~p~n", [Regressions]),
                init:stop(1)
            end
          "
```

## File Structure

```
test/bench/
├── pattern_benchmarks.erl      # Pattern performance tests
├── mining_benchmarks.erl        # Algorithm performance tests
├── compilation_benchmarks.erl   # YAWL compilation tests
├── nif_benchmarks.erl           # NIF vs Pure Erlang tests
├── baseline.json                # Reference metrics
└── benchmark_SUITE.erl          # Common Test wrapper

docs/bench/
├── BENCHMARK_EXECUTION_GUIDE.md # This file
└── results/                     # Generated benchmark reports
    ├── pattern_results.json
    ├── mining_results.json
    ├── compilation_results.json
    └── nif_results.json
```

## Troubleshooting

### Benchmarks Fail to Load

```erlang
% Ensure modules are compiled
rebar3 compile test/bench/*.erl

% Check ebin directory
ls _build/default/lib/cre/test/bench/
```

### NIF Not Available

```erlang
% Check NIF status
nif_benchmarks:check_nif_available().
% => false (NIF not compiled or loaded)

% Compile Rust NIFs
cd src/rust_nifs && make build

% Verify NIF loads
erl -pa _build/default/lib/cre/priv -s rust_nif
```

### Memory Measurements Inaccurate

Run garbage collection before measuring:

```erlang
garbage_collect(),
erlang:memory(total).
```

### Inconsistent Results

Ensure:
1. System is idle (no heavy background processes)
2. Consistent CPU frequency (disable turbo boost)
3. Multiple iterations for averaging
4. Warmup iterations executed
