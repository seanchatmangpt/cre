# Load Testing Quick Start Guide

Fast-track guide to running CRE load tests.

## Quick Docker Setup

```bash
# Build the image
docker buildx bake --load

# Run tests in container
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 sh -c "rebar3 compile && rebar3 ct --suite=test/load/load_test_SUITE"
```

## Run Specific Test Groups

```bash
# Concurrent execution tests (5-10 minutes)
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --group=concurrent_execution

# Sustained load tests (3-5 minutes)
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --group=sustained_load

# Quick test - just 10 concurrent workflows
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --case=concurrent_workflows_10
```

## Interactive Testing

```bash
# Start interactive shell
docker run -it --rm -v $(pwd):/work -w /work cre:0.3.0 sh

# Inside container:
rebar3 compile
rebar3 shell

# In Erlang shell:
application:ensure_all_started(cre).
c("test/load/load_test_scenarios.erl").

% Run a quick scenario
load_test_scenarios:run_scenario(mixed_pattern_load, #{workflow_count => 20}).

% Profile memory usage
load_test_scenarios:profile_workflow_execution(sequence, 100).

% Check for memory leaks
load_test_scenarios:detect_memory_leaks(sequence, 100).
```

## View Results

```bash
# After running tests, results are in:
cat _build/test/logs/ct_run.*/load_test_results/load_test_report.txt

# CSV for analysis:
cat _build/test/logs/ct_run.*/load_test_results/load_test_results.csv

# Open HTML report in browser:
open _build/test/logs/index.html
```

## Recommended Test Sequence

**1. Baseline (2 minutes)**
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --case=concurrent_workflows_10
```

**2. Moderate Load (5 minutes)**
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --case=concurrent_workflows_100
```

**3. Sustained Test (1 minute)**
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE --case=sustained_load_light
```

**4. Full Suite (20-30 minutes)**
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 ct --suite=test/load/load_test_SUITE
```

## Common Issues

### "Docker not found"
- All tests must run in Docker per project requirements
- Build the image first: `docker buildx bake --load`

### "Tests timing out"
- Start with smaller test cases
- Increase memory: `docker run --memory=4g ...`

### "Module not found"
- Run `rebar3 compile` before tests
- Check you're in project root: `/work` inside container

## Performance Targets

| Metric | Minimum | Target | Stretch |
|--------|---------|--------|---------|
| 10 workflows | Pass | < 200ms P95 | < 100ms P95 |
| 100 workflows | Pass | < 500ms P95 | < 300ms P95 |
| Memory growth | < 100 MB | < 50 MB | < 10 MB |
| Recovery time | < 60s | < 30s | < 10s |

## Next Steps

1. Run baseline test to establish performance metrics
2. Run full suite to identify bottlenecks
3. Fill out report template with results
4. Compare with previous runs for regressions
5. Optimize and re-test

See `README.md` for detailed documentation.
