# CRE Monitoring Documentation

This document describes the monitoring setup for CRE, including Prometheus configuration, Grafana dashboards, and alerting rules.

## Overview

CRE uses OpenTelemetry for metrics collection, Prometheus for metrics storage, and Grafana for visualization. The monitoring stack provides:

- **Performance monitoring**: Pattern execution timing, mining algorithm performance, throughput metrics
- **Resource monitoring**: CPU usage, memory consumption, garbage collection, Mnesia table sizes
- **Regression detection**: Benchmark comparison over time, performance trend analysis, baseline comparison
- **Alerting**: Pre-configured alerts for critical conditions

## Architecture

```
+-------------------+     +-------------------+     +-------------------+
|   CRE Application | --> |  OTEL Prometheus  | --> |    Prometheus     |
|   (Erlang/OTP)    |     |     Exporter      |     |   (TSDB)          |
|                   |     |   (port 9091)     |     |   (port 9090)     |
+-------------------+     +-------------------+     +-------------------+
                                                           |
                                                           v
                                                    +-------------------+
                                                    |     Grafana       |
                                                    |   (port 3000)     |
                                                    +-------------------+
```

## Quick Start

### 1. Start Prometheus

```bash
# Navigate to monitoring directory
cd /Users/sac/cre/monitoring/prometheus

# Start Prometheus (requires prometheus binary)
prometheus --config.file=prometheus.yml \
           --storage.tsdb.path=./data \
           --web.listen-address=:9090
```

Or using Docker:

```bash
docker run -d \
  --name prometheus \
  -p 9090:9090 \
  -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml \
  -v $(pwd)/alerts:/etc/prometheus/alerts \
  prom/prometheus
```

### 2. Start Grafana

```bash
docker run -d \
  --name grafana \
  -p 3000:3000 \
  -v $(pwd)/grafana/provisioning:/etc/grafana/provisioning \
  -v $(pwd)/grafana/dashboards:/var/lib/grafana/dashboards/cre \
  -e "GF_SECURITY_ADMIN_PASSWORD=admin" \
  -e "GF_INSTALL_PLUGINS=grafana-piechart-panel" \
  grafana/grafana
```

### 3. Start the CRE OTEL Exporter

The OTEL exporter is started as part of the CRE application:

```erlang
%% Start the exporter
{ok, Pid} = prometheus_exporter:start_link([{port, 9091}]).

%% Check health
prometheus_exporter:health().
%% => up
```

### 4. Access Dashboards

- **Grafana**: http://localhost:3000 (admin/admin)
- **Prometheus**: http://localhost:9090
- **Metrics endpoint**: http://localhost:9091/metrics

## Available Dashboards

### 1. CRE Performance Dashboard (`/d/cre-performance`)

**Purpose**: Monitor pattern execution timing and throughput

**Panels**:
- Pattern Execution Time (p99) - Histogram quantiles for all patterns
- Average Pattern Execution Time - Time series by pattern name
- Pattern Execution Percentiles - p50, p95, p99 comparison
- Mining Algorithm Duration - Performance by algorithm
- Total Models Discovered - Counter for discovered models
- Mining Throughput - Events processed per second
- Workflow Throughput - Completed workflows per second
- Transition Rate - Transition firings per second
- Active Workflows - Current gauge value
- Workflow Success vs Failure Rate - Comparison

**Variables**:
- `datasource`: Prometheus datasource selection
- `pattern_name`: Filter by pattern name (e.g., sequence, parallel_split)
- `algorithm`: Filter by mining algorithm (e.g., alpha, heuristic, inductive)

### 2. CRE Resources Dashboard (`/d/cre-resources`)

**Purpose**: Monitor system resource consumption

**Panels**:
- Scheduler CPU Utilization - Per-scheduler CPU usage
- Runtime Utilization - Gauge for overall runtime usage
- Memory Breakdown - Total, processes, system, atom, binary, code, ETS
- ETS Memory by Table - Memory usage per ETS table
- Process Count - Total process count over time
- Garbage Collection Rate - GC operations and words reclaimed
- GC Pause Time - Time spent in garbage collection
- Mnesia Table Size - Row count per table
- Mnesia Transaction Rate - Committed, aborted, failed transactions

**Variables**:
- `datasource`: Prometheus datasource selection
- `ets_table`: Filter by ETS table name
- `mnesia_table`: Filter by Mnesia table name
- `scheduler`: Filter by scheduler ID

### 3. CRE Regression Dashboard (`/d/cre-regression`)

**Purpose**: Track performance trends and detect regressions

**Panels**:
- Performance Deviation from Baseline (%) - Difference from baseline
- Active Regression Alerts - Current alert count
- 7-Day Performance Trend - Weekly trend analysis
- Benchmark Duration Over Time - Historical benchmark results
- Benchmark Throughput - Throughput comparison
- Alert Threshold Configuration - Current alert settings

**Variables**:
- `datasource`: Prometheus datasource selection
- `pattern_name`: Filter by pattern name
- `benchmark`: Filter by benchmark type

### 4. CRE Home Dashboard (`/d/cre-home`)

**Purpose**: Overview dashboard with quick navigation

**Panels**:
- System Health Score - Overall health (0-100%)
- Active Workflows - Current count
- Process Count - Total processes
- Total Memory - Memory usage
- Quick Links - Navigation to other dashboards
- Recent Activity - Recent workflow and transition rates

## Metrics Reference

### Workflow Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cre_workflow_active` | gauge | - | Number of currently active workflows |
| `cre_workflow_completed_total` | counter | - | Total completed workflows |
| `cre_workflow_failed_total` | counter | - | Total failed workflows |

### Pattern Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cre_pattern_execution_duration` | histogram | pattern_name | Pattern execution time in seconds |
| `cre_pattern_execution_count` | counter | pattern_name | Pattern execution count |

### Mining Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cre_mining_duration` | histogram | algorithm | Mining algorithm duration |
| `cre_mining_events_processed_total` | counter | algorithm | Total events processed |
| `cre_mining_models_discovered_total` | counter | algorithm | Total models discovered |

### Transition Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cre_transition_total` | counter | transition | Total transition firings |
| `cre_fire_duration_us` | histogram | - | Fire/3 execution time |

### Erlang VM Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `erlang_memory_total` | gauge | - | Total memory in bytes |
| `erlang_memory_processes` | gauge | - | Process memory |
| `erlang_memory_system` | gauge | - | System memory |
| `erlang_memory_ets` | gauge | - | ETS memory |
| `erlang_process_count` | gauge | - | Total process count |
| `erlang_gc_count` | gauge | - | Garbage collection count |
| `erlang_gc_pause_seconds` | gauge | - | GC pause time |

## Alert Thresholds

### Performance Alerts

| Alert | Threshold | Duration | Severity |
|-------|-----------|----------|----------|
| CREPatternExecutionHigh | p99 > 5s | 5m | warning |
| CREPatternExecutionCritical | p99 > 10s | 2m | critical |
| CREPerformanceRegression | >20% vs baseline | 10m | warning |

### Workflow Alerts

| Alert | Threshold | Duration | Severity |
|-------|-----------|----------|----------|
| CREWorkflowFailureRateHigh | >5% | 5m | warning |
| CREWorkflowFailureRateCritical | >15% | 2m | critical |
| CREActiveWorkflowsHigh | >500 | 10m | warning |

### Resource Alerts

| Alert | Threshold | Duration | Severity |
|-------|-----------|----------|----------|
| CREMemoryHigh | >2GB | 10m | warning |
| CREMemoryCritical | >4GB | 5m | critical |
| CREProcessCountHigh | >50,000 | 10m | warning |
| CREGarbageCollectionHigh | >100/sec | 10m | warning |

## Integration with CI/CD

### Baseline Management

```erlang
%% Save current performance as baseline
{ok, Pid} = pattern_benchmarks:start_link(),
{ok, _} = pattern_benchmarks:run_all_benchmarks(),
pattern_benchmarks:save_baseline().
```

### Regression Detection in CI

Add to `.github/workflows/benchmark.yml`:

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
                io:format(\"Regressions detected: ~p~n\", [Regressions]),
                init:stop(1)
            end
          "
```

## File Structure

```
monitoring/
├── grafana/
│   ├── provisioning/
│   │   ├── datasources/
│   │   │   └── prometheus.yml      # Grafana datasource config
│   │   └── dashboards/
│   │       └── cre-dashboards.yml  # Dashboard provisioning
│   └── dashboards/
│       ├── cre-performance.json    # Performance dashboard
│       ├── cre-resources.json      # Resources dashboard
│       ├── cre-regression.json     # Regression dashboard
│       └── home.json               # Overview dashboard
├── prometheus/
│   ├── prometheus.yml              # Prometheus configuration
│   └── alerts/
│       └── cre-alerts.yml          # Alerting rules
└── docker-compose.yml              # Local development stack
```

## Docker Compose Setup

For local development, use docker-compose:

```yaml
version: '3.8'

services:
  prometheus:
    image: prom/prometheus
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus/prometheus.yml:/etc/prometheus/prometheus.yml
      - ./prometheus/alerts:/etc/prometheus/alerts
      - prometheus-data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'

  grafana:
    image: grafana/grafana
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
    volumes:
      - ./grafana/provisioning:/etc/grafana/provisioning
      - ./grafana/dashboards:/var/lib/grafana/dashboards/cre
      - grafana-data:/var/lib/grafana
    depends_on:
      - prometheus

volumes:
  prometheus-data:
  grafana-data:
```

## Troubleshooting

### No metrics appearing

1. Check if the exporter is running:
   ```bash
   curl http://localhost:9091/health
   ```

2. Check if Prometheus is scraping:
   - Go to http://localhost:9090/targets
   - Verify the `cre-otel-exporter` target is "UP"

3. Check metrics endpoint:
   ```bash
   curl http://localhost:9091/metrics
   ```

### Dashboard not loading

1. Check Grafana datasource configuration:
   - Navigate to Configuration > Data Sources
   - Verify Prometheus is accessible at http://localhost:9090

2. Check dashboard provisioning logs:
   ```bash
   docker logs grafana
   ```

### Alerts not firing

1. Check alert rules in Prometheus:
   - Go to http://localhost:9090/alerts
   - Verify rules are loaded and active

2. Test alert expression:
   - Use the Prometheus query UI to validate expressions

## Additional Resources

- [Prometheus Documentation](https://prometheus.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)
- [OpenTelemetry Erlang](https://github.com/open-telemetry/opentelemetry-erlang)
- [BENCHMARK_EXECUTION_GUIDE.md](/docs/bench/BENCHMARK_EXECUTION_GUIDE.md)
