# CRE CLI - dot Command

Line Controller Factory command-line interface for quality gates and evidence collection.

## Commands

### `./bin/dot validate <module_or_file> [options]`

Run validation checks on workflow specifications.

**Options:**
- `--help, -h` - Show help message
- `--verbose, -v` - Enable verbose output
- `--format, -f <fmt>` - Output format (text|json)
- `--warnings-as-errors` - Treat warnings as errors

**Exit codes:**
- 0 - PASS (validation successful)
- 1 - ERROR (command error)
- 2 - FAIL (validation failed)

### `./bin/dot sync [options]`

Synchronize evidence with remote storage.

**Options:**
- `--help, -h` - Show help message
- `--push, -p` - Push local evidence to remote
- `--pull, -l` - Pull remote evidence to local
- `--both, -b` - Bidirectional sync (default)
- `--remote, -r <url>` - Remote storage URL
- `--force, -f` - Force overwrite on conflicts

### `./bin/dot evidence [options]`

Collect evidence pack for audit.

**Options:**
- `--help, -h` - Show help message
- `--output, -o <dir>` - Output directory (default: ./evidence-pack)
- `--format, -f <fmt>` - Output format (tar|zip|directory)
- `--include, -i <typ>` - Include specific types (receipts,andon,benchmarks,proofs,config)
- `--since, -s <date>` - Only include evidence since date

### `./bin/dot bench [options]`

Run benchmarks with regression detection.

**Options:**
- `--help, -h` - Show help message
- `--iterations, -i <n>` - Number of iterations (default: 1000)
- `--warmup, -w <n>` - Warmup iterations (default: 10)
- `--baseline, -b <file>` - Baseline file for comparison
- `--threshold, -t <pct>` - Regression threshold % (default: 5)
- `--module, -m <mod>` - Benchmark module to run
- `--save, -s <file>` - Save results to file

### `./bin/dot prove <module_or_spec> [options]`

Run proof verification on workflow specifications.

**Options:**
- `--help, -h` - Show help message
- `--verbose, -v` - Enable verbose output
- `--format, -f <fmt>` - Output format (text|json)
- `--soundness, -s` - Verify soundness properties
- `--liveness, -l` - Verify liveness properties
- `--boundedness, -b` - Verify boundedness properties
- `--structural, -t` - Verify structural properties

### `./bin/dot andon [options]`

Display andon gate status (PASS/FAIL).

**Andon gate rules:**
- FAIL if compile warnings > 0
- FAIL if any proof fails
- FAIL if benchmark regression > threshold
- FAIL if any critical error in receipt log

**Options:**
- `--help, -h` - Show help message
- `--compile-check, -c` - Check compile warnings
- `--proof-check, -p` - Run proof verification
- `--bench-check, -b` - Run benchmark regression check
- `--log-check, -l` - Check receipt log for errors
- `--threshold, -t <pct>` - Benchmark regression threshold % (default: 5)

**Exit codes:**
- 0 - PASS (all gates passed)
- 1 - ERROR (command error)
- 2 - FAIL (quality gate failed)

## Shortcuts

- `./bin/bench` - Shortcut for `./bin/dot bench`
- `./bin/prove` - Shortcut for `./bin/dot prove`

## Examples

```bash
# Run andon gate check (for CI/CD)
./bin/dot andon

# Run benchmarks with regression detection
./bin/dot bench --iterations=10000 --save=evidence/benchmarks.json

# Verify workflow soundness
./bin/dot prove my_workflow --soundness --liveness

# Collect evidence pack for audit
./bin/dot evidence --format=tar --output=/tmp/audit-pack

# Sync evidence to remote storage
./bin/dot sync --push --remote=gcs://bucket/evidence
```

## Module Structure

- `dot.escript` - Main entry point escript
- `dot_validate.erl` - Validation command
- `dot_sync.erl` - Sync command
- `dot_evidence.erl` - Evidence collection command
- `dot_bench.erl` - Benchmark command
- `dot_prove.erl` - Proof verification command
- `dot_andon.erl` - Andon gate command
