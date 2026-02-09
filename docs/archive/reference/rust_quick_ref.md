# Rust Modules Quick Reference

**CRE Version:** 0.3.0
**Last Updated:** 2026-02-08

---

## Quick Start

### Build

```bash
cd /Users/sac/cre/src/rust_implementations/paper_algorithms
cargo build --release
```

### Test

```bash
cargo test --all
```

### Benchmark

```bash
cargo bench --all
```

---

## Algorithm Quick Reference

### Alpha Algorithm

```rust
use paper_implementations::alpha::*;

let log = EventLog::new("log".to_string());
let params = AlphaParameters::default();
let mut alpha = AlphaAlgorithm::new(log, params);
let result = alpha.run()?;
```

**Parameters**:
- `alpha_threshold`: 0.05 (default)
- `fitness_threshold`: 0.8
- `precision_threshold`: 0.7
- `enable_pruning`: true

### Heuristic Miner

```rust
use paper_implementations::heuristic_miner::*;

let log = EventLog::new("log".to_string());
let params = HeuristicParameters::default();
let mut miner = HeuristicMiner::new(log, params);
let result = miner.run()?;
```

**Parameters**:
- `dependency_threshold`: 0.8
- `AND_threshold`: 0.6
- `OR_threshold`: 0.6
- `enable_noise_reduction`: true

### Conformance Checking

```rust
use paper_implementations::conformance_checking::*;

let checker = ConformanceChecker::new(model, log);
let result = checker.check_alignment()?;
```

---

## Common Data Types

### Event

```rust
pub struct Event {
    pub id: String,
    pub activity: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub case_id: String,
    pub resource: Option<String>,
    // ... XES 2.0 attributes
}
```

### Case

```rust
pub struct Case {
    pub id: String,
    pub events: Vec<Event>,
    pub attributes: HashMap<String, serde_json::Value>,
    // ... derived attributes
}
```

### EventLog

```rust
pub struct EventLog {
    pub cases: HashMap<String, Case>,
    pub activities: HashSet<String>,
    pub num_cases: usize,
    pub num_events: usize,
    // ... statistics
}
```

### ProcessModel

```rust
pub struct ProcessModel {
    pub id: String,
    pub model_type: ModelType,
    pub graph: UnGraph<ProcessNode, ProcessEdge>,
    pub activities: HashSet<String>,
    // ... quality metrics
}
```

---

## Error Handling

### Error Types

```rust
pub enum ProcessMiningError {
    InvalidEventLog(String),
    CaseNotFound(String),
    InvalidModel(String),
    ComputationError(String),
    TimeoutError(String),
    // ... more types
}

pub type ProcessMiningResult<T> = Result<T, ProcessMiningError>;
```

### Error Recovery

```rust
use paper_implementations::errors::recovery::*;

// Retry with backoff
let result = retry_with_backoff(
    || algorithm.run(),
    max_retries,
    base_delay_ms
)?;

// Fallback strategy
let result = fallback(
    primary_algorithm,
    fallback_algorithm
)?;

// Circuit breaker
let mut breaker = CircuitBreaker::new(max_failures, timeout);
let result = breaker.execute(|| algorithm.run())?;
```

---

## Configuration

### Builder Pattern

```rust
use paper_implementations::config::*;

let config = ConfigBuilder::new()
    .algorithm_name("alpha".to_string())
    .with_alpha_threshold(0.05)
    .with_parallel_workers(4)
    .with_memory_limit(2048)
    .build();
```

### Presets

```rust
ConfigPresets::high_performance()
ConfigPresets::memory_efficient()
ConfigPresets::batch_processing()
ConfigPresets::real_time_processing()
ConfigPresets::development()
ConfigPresets::production()
```

---

## Performance Monitoring

```rust
use paper_implementations::metrics::*;

let mut monitor = PerformanceMonitor::new();
monitor.snapshot();
// ... run algorithm
monitor.snapshot();

println!("{}", monitor.report());
println!("Peak Memory: {} MB", monitor.get_memory_peak_mb());
```

---

## Cargo Commands Reference

| Command | Description |
|---------|-------------|
| `cargo build` | Debug build |
| `cargo build --release` | Optimized build |
| `cargo test` | Run tests |
| `cargo bench` | Run benchmarks |
| `cargo doc --open` | Generate and open docs |
| `cargo fmt` | Format code |
| `cargo clippy` | Run linter |
| `cargo clean` | Clean build artifacts |

---

## File Locations

| Item | Path |
|------|------|
| Source | `/Users/sac/cre/src/rust_implementations/paper_algorithms/` |
| Tests | `src/rust_implementations/paper_algorithms/tests/` |
| Benches | `src/rust_implementations/paper_algorithms/benches/` |
| Docs | `/Users/sac/cre/docs/RUST_MODULES_IMPLEMENTATION_GUIDE.md` |

---

## Dependencies

Core dependencies for process mining:

```toml
itertools = "0.13.0"    # Iterator tools
rayon = "1.8.0"          # Parallelization
petgraph = "0.6"         # Graph algorithms
ndarray = "0.15"         # N-dimensional arrays
serde = "1.0"            # Serialization
polars = "0.39"          # Dataframes
candle-core = "0.3"      # ML (LLM support)
```

---

## Module Export Map

```
paper_implementations
├── alpha                  - Alpha Algorithm
├── heuristic_miner        - Heuristic Miner
├── conformance_checking   - Conformance Checking
├── object_centric         - Object-Centric Mining
├── uncertain_event_data   - Uncertain Event Mining
├── llm_process_mining     - LLM-based Mining
├── local_process_mining   - Local Process Models
├── differential_privacy   - Privacy-Preserving Mining
├── petri_nets             - Petri Net Operations
└── performance_analysis   - Performance Metrics
```

---

## Troubleshooting

### Build Fails

```bash
# Update dependencies
cargo update

# Clean build
cargo clean
cargo build --release
```

### Tests Fail

```bash
# Run with output
cargo test -- --nocapture

# Run single test
cargo test test_name
```

### Memory Issues

```bash
# Use memory-efficient preset
let config = ConfigPresets::memory_efficient();

# Reduce parallel workers
config.performance.parallel_workers = 1;
```

---

**Version:** 0.1.0
