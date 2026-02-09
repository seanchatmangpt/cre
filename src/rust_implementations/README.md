# CRE Rust NIF Bindings

This directory contains Rust Native Implemented Functions (NIF) for integrating high-performance Rust process mining algorithms with the Erlang CRE runtime.

## Overview

The Rust NIF provides significant performance improvements over pure Erlang implementations:

| Algorithm | Speedup | Use Case |
|-----------|---------|----------|
| Alpha Algorithm | 10-100x | Process discovery from event logs |
| Heuristic Miner | 5-50x | Noise-tolerant process discovery |
| Conformance Checking | 20-200x | Model validation and fitness analysis |
| Object-Centric Mining | 10-50x | Multi-dimensional process analysis |

## Directory Structure

```
rust_implementations/
├── Cargo.toml           # Rust project configuration
├── Makefile             # Build system
├── lib.rs               # NIF entry point and initialization
├── types.rs             # Erlang-Rust type conversions
├── alpha.rs             # Alpha algorithm NIF bindings
├── heuristic.rs         # Heuristic miner NIF bindings
├── conformance.rs       # Conformance checking NIF bindings
├── object_centric.rs    # Object-centric mining NIF bindings
├── resource.rs          # Resource management for long-lived objects
├── error.rs             # Error handling and conversion
└── paper_algorithms/    # Pure Rust algorithm implementations
    ├── Cargo.toml
    ├── lib.rs
    ├── common/
    ├── algorithms/
    │   ├── alpha/
    │   ├── heuristic_miner/
    │   ├── conformance_checking/
    │   └── object_centric/
    └── ...
```

## Building

### Prerequisites

1. **Rust toolchain**: Install from https://rustup.rs/
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. **Erlang/OTP 25+**: Required for NIF 2.15+ support

3. **rustler**: Rust framework for Erlang NIFs
   ```bash
   cargo install rustler
   ```

### Build Commands

```bash
# Build the NIF library
make build

# Development build (debug symbols)
make dev

# Optimized release build
make release

# Run tests
make test

# Clean build artifacts
make clean
```

## Usage in Erlang

### Basic Example

```erlang
%% Load event log (list of traces)
Log = [[a, b, c, d], [a, c, b, d], [a, b, c, e, d]],

%% Discover a model using the Alpha algorithm
{ok, Result} = rust_nif:alpha_discover(Log),

%% Extract metrics
Fitness = maps:get(fitness, Result),
Precision = maps:get(precision, Result).
```

### With Parameters

```erlang
%% Alpha algorithm with custom parameters
Params => #{
    alpha_threshold => 0.05,
    enable_pruning => true,
    max_model_size => 1000
},
{ok, Result} = rust_nif:alpha_discover(Log, Params).
```

### Heuristic Miner

```erlang
%% Noise-tolerant discovery
Params => #{
    dependency_threshold => 0.8,
    enable_noise_reduction => true
},
{ok, Result} = rust_nif:heuristic_discover(Log, Params).

%% Get dependency information
{ok, Deps} = rust_nif:heuristic_get_dependencies(Log).
```

### Conformance Checking

```erlang
%% First discover a model
{ok, Model} = rust_nif:alpha_discover(Log),

%% Check conformance
{ok, Conformance} = rust_nif:conformance_check(Log, Model),
Fitness = maps:get(fitness, Conformance),
Precision = maps:get(precision, Conformance).

%% Get detailed alignments
{ok, Alignments} = rust_nif:conformance_align(Log, Model).
```

## API Reference

### Alpha Algorithm

```erlang
% Discover process model
rust_nif:alpha_discover(Log) -> {ok, Result} | {error, Reason}
rust_nif:alpha_discover(Log, Params) -> {ok, Result} | {error, Reason}

% Extract relations without building model
rust_nif:alpha_extract_relations(Log) -> {ok, Relations} | {error, Reason}
```

### Heuristic Miner

```erlang
% Noise-tolerant discovery
rust_nif:heuristic_discover(Log) -> {ok, Result} | {error, Reason}
rust_nif:heuristic_discover(Log, Params) -> {ok, Result} | {error, Reason}

% Get dependencies
rust_nif:heuristic_get_dependencies(Log) -> {ok, Deps} | {error, Reason}
```

### Conformance Checking

```erlang
% Full conformance analysis
rust_nif:conformance_check(Log, Model) -> {ok, Result} | {error, Reason}

% Individual metrics
rust_nif:conformance_fitness(Log, Model) -> {ok, Fitness} | {error, Reason}
rust_nif:conformance_precision(Log, Model) -> {ok, Precision} | {error, Reason}

% Alignments
rust_nif:conformance_align(Log, Model) -> {ok, Alignments} | {error, Reason}
```

### Object-Centric Mining

```erlang
% Discover from OCEL 2.0 JSON
rust_nif:object_centric_discover(OcelJson) -> {ok, Models} | {error, Reason}

% Deserialize OCEL
rust_nif:object_centric_ocel_deserialize(Json) -> {ok, Log} | {error, Reason}
```

## Memory Management

The NIF uses Rust's ownership system for automatic memory management:

- **Short-lived objects**: Automatically freed when the NIF returns
- **Long-lived objects**: Managed through the resource system

```erlang
% Create a resource for a long-lived object
{ok, ResourceId} = rust_nif:resource_create(Log),

% Use the resource later
{ok, Data} = rust_nif:resource_get(ResourceId),

% Clean up when done
ok = rust_nif:resource_delete(ResourceId).
```

## Error Handling

All NIF functions return either:

- `{ok, Result}` on success
- `{error, Reason}` on failure

Common error reasons:

- `nif_not_loaded` - NIF library failed to load
- `badarg` - Invalid argument type
- `invalid_event_log` - Malformed event log data
- `computation_error` - Algorithm execution error

## Performance Tips

1. **Use native data formats**: Pass logs as lists of traces for best performance
2. **Batch operations**: Process multiple traces in a single NIF call
3. **Reuse resources**: Use resources for long-lived objects
4. **Enable optimizations**: Use `make release` for production builds

## Integration with rebar3

Add to `rebar.config`:

```erlang
{plugins, [rustler_prv]}.

{provider_links, [
    {rustler_prv, [
        {<<"0.1.0">>, {git, "https://github.com/rusterlium/rustler.git", {tag, "0.1.0"}}}
    ]}
]}.

{rustler_plt, [
    {<<"0.1.0">>, {git, "https://github.com/rusterlium/rustler.git", {tag, "0.1.0"}}}
]}.
```

## Testing

```bash
# Run Rust tests
make test

# Run with coverage
cargo tarpaulin --out Html

# Run benchmarks
make bench
```

## Troubleshooting

### NIF fails to load

1. Check that the library was built:
   ```bash
   ls -la priv/libcre_rust_nif.so
   ```

2. Check platform compatibility:
   ```bash
   uname -m  # Should match Rust target
   ```

3. Verify Rust installation:
   ```bash
   rustc --version
   cargo --version
   ```

### Performance issues

1. Build in release mode: `make release`
2. Ensure event logs are in efficient format
3. Check for unnecessary data conversions

## Contributing

When adding new algorithms:

1. Implement in `paper_algorithms/`
2. Add NIF bindings in appropriate module
3. Update `lib.rs` to export new functions
4. Add Erlang wrapper in `src/rust_nif.erl`
5. Update this README

## References

- [Rustler Documentation](https://github.com/rusterlium/rustler)
- [Erlang NIF Documentation](https://www.erlang.org/doc/man/erl_nif.html)
- [Process Mining Handbook](https://fluxicon.com/book/)

## License

Apache-2.0 - See LICENSE file for details
