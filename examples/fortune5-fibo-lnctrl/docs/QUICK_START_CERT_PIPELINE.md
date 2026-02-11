# Quick Start: Certification Pipeline

## TL;DR

```erlang
%% Start the certification pipeline
application:ensure_all_started(f5_cert_runner).

%% Trigger evidence collection
{ok, Results} = f5_cert_runner:start_collection().

%% Generate certification report
{ok, Report} = f5_cert_runner:generate_report().
```

## What Does It Do?

The certification pipeline orchestrates evidence collection for nine-nines (99.999%) availability certification:

1. **Uptime Monitoring** - Tracks system uptime over 90 days
2. **Load Testing** - Tests 10K+ concurrent workflows
3. **Chaos Engineering** - Verifies resilience under failure conditions
4. **Report Generation** - Aggregates evidence into certification reports
5. **Verdict Integration** - Links evidence to hash-chained verdicts

## Architecture

```
f5_cert_runner (Orchestrator)
├── f5_cert_scheduler (Schedules hourly collections)
└── Evidence Collectors:
    ├── f5_uptime_logger (Continuous uptime)
    ├── f5_load_tester (Load testing)
    └── chaos_controller (Chaos engineering)
```

## Evidence Collection

All evidence collectors follow the same API:

```erlang
%% Start collector
ok = f5_uptime_logger:start().

%% Collect evidence
{ok, Evidence} = f5_uptime_logger:collect().
% Evidence written to: evidence/uptime/continuous_operation.json

%% Verify evidence integrity
ok = f5_uptime_logger:verify().

%% Stop collector
ok = f5_uptime_logger:stop().
```

## Scheduler

The scheduler runs evidence collection every hour for 90 days:

```erlang
%% Get progress
Info = f5_cert_scheduler:get_schedule_info().
% #{
%   runtime_days => 45.3,
%   progress_percent => 50.3,
%   target_days => 90,
%   collections => 1087
% }

%% Pause/resume
f5_cert_scheduler:pause().
f5_cert_scheduler:resume().
```

## Certification Report

Generate a comprehensive report:

```erlang
Report = generate_certification_report:generate().
% #{
%   report_type => certification_report,
%   evidence => #{
%     uptime_monitoring => ...,
%     load_testing => ...,
%     chaos_engineering => ...
%   },
%   assessment => #{
%     uptime_criterion => true,
%     load_test_criterion => true,
%     chaos_criterion => true,
%     continuous_operation_criterion => false,  % Need 90 days
%     all_criteria_met => false,
%     certification_status => in_progress
%   }
% }
```

## Verdict Integration

Verdicts now include evidence summary:

```erlang
Verdict = verdict_builder:build_verdict(#{
    apps_generated => [<<"f5_app_02">>],
    ontology_hash => <<"abc123">>,
    proofs_summary => #{},
    failing_tests => []
}).

%% Evidence summary included
EvidenceSummary = maps:get(evidence_summary, Verdict).
% #{
%   uptime => #{file => <<"evidence/uptime/...">>, hash => <<"...">>, status => ok},
%   load_test => #{...},
%   chaos => #{...},
%   certification_report => #{...}
% }
```

## Evidence Files

All evidence is written to the `evidence/` directory:

```
evidence/
├── uptime/continuous_operation.json        # Uptime statistics
├── load_tests/10k_concurrent_test.json     # Load test results
├── chaos/resilience_test.json              # Chaos test results
└── reports/certification_latest.json       # Latest report
```

Each file includes a SHA256 hash for integrity verification.

## Testing

```bash
# Run unit tests
rebar3 eunit --module=f5_cert_pipeline_test

# Run integration test
./scripts/test_cert_pipeline.sh
```

## Configuration

In `apps/f5_cert_runner/src/f5_cert_runner.app.src`:

```erlang
{env, [
    {collection_interval, 3600000},     % 1 hour (in milliseconds)
    {continuous_operation_days, 90}     % 90-day trial
]}
```

## 90-Day Certification Process

1. **Day 0**: Start the cert_runner application
   ```erlang
   application:ensure_all_started(f5_cert_runner).
   ```

2. **Day 1-89**: Scheduler collects evidence hourly
   - Check progress: `f5_cert_scheduler:get_schedule_info()`
   - Generate interim reports: `f5_cert_runner:generate_report()`

3. **Day 90**: Final certification
   ```erlang
   {ok, FinalReport} = f5_cert_runner:generate_report().
   % #{assessment => #{certification_status => certified, ...}}
   ```

4. **Submit**: Use final report for certification submission

## Certification Criteria

| Criterion | Target | Evidence Source |
|-----------|--------|-----------------|
| Uptime | ≥99.999% | f5_uptime_logger |
| Concurrent Workflows | ≥10,000 | f5_load_tester |
| Chaos Resilience | System recovery | chaos_controller |
| Continuous Operation | ≥90 days | f5_cert_scheduler |

## Troubleshooting

### Scheduler not running
```erlang
%% Check if running
whereis(f5_cert_scheduler).

%% Restart
application:stop(f5_cert_runner),
application:start(f5_cert_runner).
```

### Evidence collection failed
```erlang
%% Check status
Status = f5_cert_runner:get_status().
% #{errors => [...]}

%% Manual collection
{ok, Evidence} = f5_uptime_logger:collect().
```

### Missing evidence files
```erlang
%% Check if collectors are started
whereis(f5_uptime_logger).

%% Start manually
f5_uptime_logger:start().
```

## API Reference

### f5_cert_runner
- `start_collection/0` - Trigger evidence collection
- `generate_report/0` - Generate certification report
- `get_status/0` - Get orchestrator status

### f5_cert_scheduler
- `pause/0` - Pause scheduled collections
- `resume/0` - Resume scheduled collections
- `get_schedule_info/0` - Get runtime progress

### Evidence Collectors (all have same API)
- `start/0` - Initialize collector
- `stop/0` - Shutdown collector
- `collect/0` - Collect evidence
- `verify/0` - Verify evidence integrity

### generate_certification_report
- `generate/0` - Generate certification report
- `generate/1` - Generate with options

### verdict_builder
- `build_verdict/1` - Build verdict with evidence summary
- `verify_verdict/1` - Verify verdict integrity

## Documentation

- `docs/CERT_PIPELINE.md` - Detailed architecture and usage
- `docs/BLOCK_D_SUMMARY.md` - Implementation summary
- `test/f5_cert_pipeline_test.erl` - Code examples

## Support

For issues or questions:
- Check logs: `logger:info/1,2`
- Run tests: `rebar3 eunit`
- Review evidence files in `evidence/`
- Check scheduler status: `f5_cert_scheduler:get_schedule_info()`
