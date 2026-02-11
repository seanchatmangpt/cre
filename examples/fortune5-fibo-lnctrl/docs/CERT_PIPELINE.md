# Certification Pipeline Orchestrator

## Overview

The certification pipeline orchestrator (`f5_cert_runner`) coordinates all evidence collection activities for nine-nines (99.999%) availability certification. It manages continuous operation logging over 90 days, load testing, and chaos engineering experiments.

## Architecture

### Evidence Collection Modules

All evidence collection modules follow a standardized API:

- `start/0` - Initialize the collector
- `stop/0` - Shutdown the collector
- `collect/0` - Collect evidence and write to `evidence/` directory
- `verify/0` - Verify evidence integrity using hash chains

#### f5_uptime_logger

- **Purpose**: Continuous uptime monitoring for 90-day certification trial
- **API**: Standard evidence collector API
- **Evidence**: `evidence/uptime/continuous_operation.json`
- **Data Collected**:
  - Uptime statistics (seconds, days, percentage)
  - Event count
  - Unplanned restart count
  - Supervisor events

#### f5_load_tester

- **Purpose**: Load testing with 10K+ concurrent workflows
- **API**: Standard evidence collector API
- **Evidence**: `evidence/load_tests/10k_concurrent_test.json`
- **Data Collected**:
  - Latency percentiles (P50, P95, P99, P99.9, P99.99)
  - Throughput (workflows/second)
  - Resource usage (memory, process count)
  - Total workflows executed

#### chaos_controller

- **Purpose**: Chaos engineering for resilience testing
- **API**: Standard evidence collector API
- **Evidence**: `evidence/chaos/resilience_test.json`
- **Data Collected**:
  - Kill random processes test results
  - Network partition simulation results
  - Memory exhaustion test results
  - System recovery status

### Orchestrator Components

#### f5_cert_runner (gen_server)

Main orchestrator that coordinates evidence collection.

**API**:
- `start_collection/0` - Trigger evidence collection from all modules
- `generate_report/0` - Generate comprehensive certification report
- `get_status/0` - Get current status (collection count, errors)

**Supervisor Tree**:
```
f5_cert_runner_sup
├── f5_cert_runner (orchestrator)
└── f5_cert_scheduler (scheduler)
```

#### f5_cert_scheduler (gen_server)

Schedules periodic evidence collection for continuous operation.

**API**:
- `pause/0` - Pause scheduled collections
- `resume/0` - Resume scheduled collections
- `get_schedule_info/0` - Get runtime progress (days, percentage complete)

**Configuration**:
- `collection_interval` - Default: 3600000 ms (1 hour)
- `continuous_operation_days` - Default: 90 days

#### generate_certification_report

Aggregates all evidence into a comprehensive certification report.

**API**:
- `generate/0` - Generate certification report
- `generate/1` - Generate with options

**Report Contents**:
- Evidence summary from all collectors
- Scheduler status (runtime, progress)
- Runner status (collection count, errors)
- Certification criteria assessment
- Certification status (certified/in_progress)

### Verdict Integration

The `verdict_builder` module has been enhanced to include evidence summary:

```erlang
Verdict = #{
    apps_generated := [binary()],
    chain := #{prev_hash := binary() | null, this_hash := binary()},
    environment_fingerprint := map(),
    evidence_summary := map(),  % NEW: Evidence files and hashes
    failing_tests := [#{test_id := binary(), reason := binary()}],
    generator_version := binary(),
    ontology_hash := binary(),
    proofs_summary := map(),
    suite := binary(),
    tests_passed := boolean(),
    timestamp := binary()
}
```

The `evidence_summary` includes:
- Evidence file paths
- Evidence hashes for integrity verification
- Collection status (ok, not_yet_collected, error)

## Usage

### Start Certification Pipeline

```erlang
%% Start the cert_runner application
application:ensure_all_started(f5_cert_runner).

%% Trigger evidence collection
{ok, Results} = f5_cert_runner:start_collection().

%% Generate certification report
{ok, Report} = f5_cert_runner:generate_report().

%% Get status
Status = f5_cert_runner:get_status().
SchedulerInfo = f5_cert_scheduler:get_schedule_info().
```

### Manual Evidence Collection

```erlang
%% Start individual collectors
ok = f5_uptime_logger:start().
ok = f5_load_tester:start().
ok = chaos_controller:start().

%% Collect evidence
{ok, UptimeEvidence} = f5_uptime_logger:collect().
{ok, LoadTestEvidence} = f5_load_tester:collect().
{ok, ChaosEvidence} = chaos_controller:collect().

%% Verify evidence
ok = f5_uptime_logger:verify().
ok = f5_load_tester:verify().
ok = chaos_controller:verify().
```

### Build Verdict with Evidence

```erlang
Verdict = verdict_builder:build_verdict(#{
    apps_generated => [<<"f5_app_02">>],
    ontology_hash => <<"abc123">>,
    proofs_summary => #{},
    failing_tests => []
}).

%% Verify verdict
ok = verdict_builder:verify_verdict(Verdict).
```

## Evidence Files

All evidence is written to the `evidence/` directory:

```
evidence/
├── uptime/
│   └── continuous_operation.json
├── load_tests/
│   └── 10k_concurrent_test.json
├── chaos/
│   └── resilience_test.json
└── reports/
    ├── certification_latest.json
    └── certification_2026-02-11T14:30:00Z.json
```

Each evidence file includes:
- `module` - Evidence collector module name
- `type` - Evidence type (uptime_monitoring, load_testing, chaos_engineering)
- `timestamp` - ISO8601 timestamp
- `data` - Evidence-specific data
- `evidence_file` - File path
- `evidence_hash` - SHA256 hash for integrity verification

## Certification Criteria

The system assesses certification based on:

1. **Uptime Criterion**: ≥99.999% uptime
2. **Load Test Criterion**: ≥10,000 concurrent workflows
3. **Chaos Criterion**: System recovery after chaos experiments
4. **Continuous Operation Criterion**: ≥90 days of operation

When all criteria are met, `certification_status` is set to `certified`.

## Testing

### Unit Tests

```bash
# Run EUnit tests
rebar3 eunit

# Run specific test module
rebar3 eunit --module=f5_cert_pipeline_test
```

### Integration Test

```bash
# Run full pipeline test
./scripts/test_cert_pipeline.sh
```

## Configuration

Application environment variables in `f5_cert_runner.app.src`:

```erlang
{env, [
    {collection_interval, 3600000},  % 1 hour in milliseconds
    {continuous_operation_days, 90}  % 90-day certification trial
]}
```

## Receipt Chain Integration

All evidence files are integrated with the receipt chain:

1. Evidence is collected and written to files
2. SHA256 hash is computed over canonical JSON
3. Hash is included in evidence file
4. Evidence summary is included in verdict
5. Verdict hash chains to previous verdicts

This creates an immutable audit trail of all certification activities.

## Future Enhancements

- [ ] Real-time evidence streaming to external audit systems
- [ ] Evidence compression for long-term storage
- [ ] Evidence encryption for sensitive data
- [ ] Evidence replication to multiple locations
- [ ] Evidence expiration and archival policies
- [ ] Evidence search and query API
- [ ] Evidence visualization dashboard
- [ ] Evidence export to industry-standard formats (SPDX, CycloneDX)

## References

- `apps/f5_cert_runner/` - Orchestrator application
- `apps/f5_evidence/` - Evidence collection modules
- `apps/f5_receipts/` - Receipt and verdict builders
- `test/f5_cert_pipeline_test.erl` - End-to-end tests
- `scripts/test_cert_pipeline.sh` - Integration test script
