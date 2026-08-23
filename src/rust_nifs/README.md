# CRE Rust NIF Bindings

Erlang NIF (Native Implemented Functions) bindings for the CRE process mining engine, providing high-performance Rust implementations of core algorithms.

## Overview

This library uses [rustler](https://github.com/rusterlium/rustler) to create safe Erlang NIF bindings for Rust-based process mining algorithms, including:

- **Alpha Algorithm**: Classic process discovery from event logs
- **Heuristic Miner**: Noise-tolerant process discovery
- **Conformance Checking**: Fitness and precision analysis
- **Object-Centric Process Mining**: Multi-dimensional process analysis

## Building

### Prerequisites

- Rust 1.70+ (install from https://rustup.rs/)
- Erlang/OTP 25+
- cargo-make (optional, for advanced build workflows)

### Build Commands

```bash
# Build the NIF library
make build

# Debug build
make dev

# Release build (fully optimized)
make release

# Run tests
make test

# Run linting
make lint

# Format code
make format
```

### Build Output

The compiled shared library is placed in `priv/libcre_rust_nif.{so,dylib,dll}` depending on platform.

## Usage

### Loading the NIF in Erlang

```erlang
%% Load the NIF library
-module(rust_nif).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", "priv"])) of
                true ->
                    filename:join(["..", "priv", "libcre_rust_nif"]);
                _ ->
                    filename:join(["priv", "libcre_rust_nif"])
            end;
        Dir ->
            filename:join(Dir, "libcre_rust_nif")
    end,
    erlang:load_nif(SoName, 0).
```

### Example: Alpha Algorithm

```erlang
%% Load an event log from JSON
{ok, Log} = rust_nif:load_json_log(JsonLog),

%% Discover a process model using Alpha algorithm
{ok, Result} = rust_nif:alpha_discover(Log),

%% Get fitness score
#{fitness := Fitness} = Result.
```

### Example: Heuristic Miner

```erlang
%% Discover with custom parameters
Params = #{
    dependency_threshold => 0.9,
    enable_noise_reduction => true
},
{ok, Result} = rust_nif:heuristic_discover_with_params(Log, Params).
```

### Example: Conformance Checking

```erlang
%% Check model fitness against log
{ok, Fitness} = rust_nif:conformance_fitness(Log, Model),

%% Check model precision
{ok, Precision} = rust_nif:conformance_precision(Log, Model),

%% Full conformance check
{ok, Result} = rust_nif:conformance_check(Log, Model).
```

## API Reference

### Alpha Algorithm Functions

| Function | Description |
|----------|-------------|
| `alpha_discover/1` | Discover process model with default parameters |
| `alpha_discover_with_params/2` | Discover with custom parameters |
| `alpha_extract_relations/1` | Extract ordering relations from log |

### Heuristic Miner Functions

| Function | Description |
|----------|-------------|
| `heuristic_discover/1` | Discover with default parameters |
| `heuristic_discover_with_params/2` | Discover with custom parameters |
| `heuristic_get_dependencies/1` | Get dependency matrix |

### Conformance Functions

| Function | Description |
|----------|-------------|
| `conformance_check/2` | Full conformance analysis |
| `conformance_fitness/2` | Calculate fitness score |
| `conformance_precision/2` | Calculate precision score |
| `conformance_align/2` | Calculate trace alignments |

### Object-Centric Functions

| Function | Description |
|----------|-------------|
| `object_centric_discover/1` | Discover OCEL models |
| `object_centric_ocel_deserialize/1` | Parse OCEL 2.0 JSON |

### Utility Functions

| Function | Description |
|----------|-------------|
| `version/0` | Get NIF version |
| `algorithm_list/0` | List available algorithms |
| `benchmark/2` | Benchmark an algorithm |

## Memory Safety

The NIF implementation uses rustler's safe abstractions to ensure:

- **No memory leaks**: Rust ownership model prevents resource leaks
- **No data races**: Rust type system prevents concurrent access issues
- **Safe FFI**: rustler handles all Erlang term conversions safely

## Performance

The Rust NIF implementations provide significant performance benefits over pure Erlang:

- **10-100x faster** for CPU-intensive algorithms
- **Lower memory footprint** through efficient data structures
- **No GC pauses** for NIF-executed code

## Development

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_alpha

# Run with output
cargo test -- --nocapture
```

### Documentation

```bash
# Generate and open documentation
cargo doc --open
```

### Benchmarking

```bash
# Run benchmarks
cargo bench
```

## License

Apache-2.0

## References

- [rustler User Guide](https://rusterlium.github.io/rustler/)
- [Erlang NIF Documentation](https://www.erlang.org/doc/man/erl_nif.html)
- [Process Mining Handbook](https://www.processmining.org/book/)
