# SOC 2 Validation Quick Start Guide

This guide covers practical examples for starting, monitoring, and auditing SOC 2 compliance validation in CRE.

## Overview

CRE's SOC 2 validation system is built on Joe Armstrong's principles:
- **Observable**: Every validation produces a receipt
- **Fault-tolerant**: Validators fail independently
- **Provable**: Receipts form a cryptographic Merkle tree
- **Continuous**: Validators run perpetually
- **Traceable**: All evidence is manifest-linked

## 1. Start the Validation Supervision Tree

The validation system runs as a supervised Erlang process tree. Start it in your application initialization.

### Example 1.1: Start from Application Code

```erlang
%% In your application startup (e.g., cre.erl)
{ok, Pid} = soc2_validation_sup:start_link(),
io:format("SOC 2 validation supervisor started: ~p~n", [Pid]).
```

### Example 1.2: Start from Erlang Shell

```erlang
%% Interactive shell
(cre@localhost)1> {ok, Pid} = soc2_validation_sup:start_link().
{ok, <0.123.0>}

(cre@localhost)2> whereis(soc2_validation_sup).
<0.123.0>
```

**What starts:**
- `soc2_control_executor_sup` - 7 control validators (CC6.1, CC7.1, CC8.1, CC9.1, PI1.1, C1.1, P1.1)
- `soc2_evidence_stream_sup` - 4 evidence generators (uptime, load_test, chaos, build)
- `soc2_receipt_chain` - Merkle tree accumulator
- `soc2_meta_validator` - Validator of validators

## 2. Check Validation Status

Monitor real-time validation status for all controls.

### Example 2.1: Get Status for All Controls

```erlang
%% All control status
Status = soc2_validation_sup:get_validation_status(),
maps:foreach(
    fun(ControlId, StatusMap) ->
        io:format("~s: ~w (last validated: ~w)~n",
                  [ControlId,
                   maps:get(status, StatusMap),
                   maps:get(last_validated, StatusMap)])
    end,
    Status).
```

**Output:**
```
<<"CC6.1">>: pass (last validated: {{2025, 2, 11}, {14, 30, 45}})
<<"CC7.1">>: pass (last validated: {{2025, 2, 11}, {14, 30, 42}})
<<"CC8.1">>: fail (last validated: {{2025, 2, 11}, {14, 29, 50}})
<<"CC9.1">>: pass (last validated: {{2025, 2, 11}, {14, 30, 38}})
<<"PI1.1">>: pass (last validated: {{2025, 2, 11}, {14, 30, 35}})
<<"C1.1">>: pass (last validated: {{2025, 2, 11}, {14, 30, 40}})
<<"P1.1">>: pass (last validated: {{2025, 2, 11}, {14, 30, 43}})
```

### Example 2.2: Get Status for a Specific Control

```erlang
%% Single control
ControlStatus = soc2_control_executor:get_status(<<"CC6.1">>),
io:format("CC6.1 Status: ~p~n", [ControlStatus]).
```

**Output:**
```
CC6.1 Status: #{status => pass,
                last_validated => {{2025, 2, 11}, {14, 30, 45}},
                validation_count => 12}
```

### Example 2.3: Poll Status in a Loop

```erlang
%% Monitor validation every 30 seconds
monitor_validation() ->
    loop_monitor(0).

loop_monitor(N) ->
    case soc2_validation_sup:get_validation_status() of
        Status ->
            {Pass, Fail} = count_status(Status),
            io:format("[~w] Pass: ~w | Fail: ~w~n", [N, Pass, Fail])
    end,
    timer:sleep(30000),
    loop_monitor(N + 1).

count_status(Status) ->
    maps:fold(
        fun(_ControlId, #{status := pass}, {P, F}) -> {P + 1, F};
           (_ControlId, #{status := fail}, {P, F}) -> {P, F + 1};
           (_ControlId, _, Acc) -> Acc
        end,
        {0, 0},
        Status).
```

## 3. View Receipts

Validation receipts form a cryptographic chain. Each receipt proves a validation occurred and passed/failed.

### Example 3.1: Get the Complete Receipt Chain

```erlang
%% Fetch the receipt chain
Chain = soc2_receipt_chain:get_chain(),
RootHash = maps:get(root_hash, Chain),
ReceiptCount = maps:get(receipt_count, Chain),
Receipts = maps:get(receipts, Chain),

io:format("Merkle Root: ~s~n", [base64:encode(RootHash)]),
io:format("Total Receipts: ~w~n", [ReceiptCount]),
io:format("Latest 3 Receipts:~n", []),
lists:foreach(
    fun(Receipt) ->
        ControlId = maps:get(control_id, Receipt),
        Status = maps:get(status, Receipt),
        Timestamp = maps:get(timestamp, Receipt),
        io:format("  ~s: ~w @ ~w~n", [ControlId, Status, Timestamp])
    end,
    lists:sublist(Receipts, 1, 3)).
```

**Output:**
```
Merkle Root: 0xabcd1234...
Total Receipts: 47
Latest 3 Receipts:
  <<"CC6.1">>: pass @ {{2025, 2, 11}, {14, 30, 45}}
  <<"CC7.1">>: pass @ {{2025, 2, 11}, {14, 30, 42}}
  <<"P1.1">>: pass @ {{2025, 2, 11}, {14, 30, 40}}
```

### Example 3.2: Trigger Immediate Validation

```erlang
%% Force validation now instead of waiting for interval (default 5 minutes)
{ok, Receipt} = soc2_validation_sup:validate_control(<<"CC6.1">>),
io:format("Validation Receipt:~n"),
io:format("  Control: ~s~n", [maps:get(control_id, Receipt)]),
io:format("  Status: ~w~n", [maps:get(status, Receipt)]),
io:format("  Timestamp: ~w~n", [maps:get(timestamp, Receipt)]),
io:format("  Validators: ~p~n", [maps:get(validator_results, Receipt)]),
io:format("  Evidence: ~p~n", [maps:get(evidence_results, Receipt)]).
```

**Output:**
```
Validation Receipt:
  Control: <<"CC6.1">>
  Status: pass
  Timestamp: {{2025, 2, 11}, {14, 35, 12}}
  Validators: [#{validator_id => <<"access_control_validator">>,
                  status => pass,
                  timestamp => {{2025, 2, 11}, {14, 35, 12}}}]
  Evidence: [#{evidence_path => <<"receipts/build.last.json">>,
               status => pass,
               exists => true}]
```

### Example 3.3: Export Receipts to JSON

```erlang
%% Export receipt chain as JSON for auditors
Chain = soc2_receipt_chain:get_chain(),
Json = jsx:encode(Chain, [{space, 2}]),
file:write_file("soc2_receipts_export.json", Json),
io:format("Receipt chain exported to soc2_receipts_export.json~n").
```

### Example 3.4: Verify a Receipt

```erlang
%% Cryptographically verify a receipt exists in the chain
Receipt = #{
    control_id => <<"CC6.1">>,
    status => pass,
    timestamp => {{2025, 2, 11}, {14, 35, 12}}
},

case soc2_receipt_chain:verify_receipt(Receipt) of
    {ok, verified} ->
        io:format("Receipt is valid and in the chain~n");
    {error, not_found} ->
        io:format("Receipt not found in chain~n")
end.
```

## 4. Run Meta-Validation

The meta-validator is the "watcher of watchers" - it validates the validation system itself.

### Example 4.1: Run Meta-Validation Manually

```erlang
%% Execute meta-validation (normal interval is 10 minutes)
Health = soc2_meta_validator:run_meta_validation(),
io:format("System Health Report:~n"),
io:format("  Overall Status: ~w~n", [maps:get(status, Health)]),
io:format("  Timestamp: ~w~n", [maps:get(timestamp, Health)]),

Checks = maps:get(checks, Health),
format_checks(Checks).

format_checks(Checks) ->
    io:format("~n  Control Executors:~n"),
    format_check(maps:get(control_executors, Checks)),
    io:format("~n  Evidence Generators:~n"),
    format_check(maps:get(evidence_generators, Checks)),
    io:format("~n  Receipt Chain:~n"),
    format_check(maps:get(receipt_chain, Checks)),
    io:format("~n  Coverage:~n"),
    format_check(maps:get(coverage, Checks)).

format_check(Check) ->
    io:format("    Status: ~w~n", [maps:get(status, Check)]),
    maps:foreach(
        fun(Key, Value) ->
            io:format("    ~w: ~w~n", [Key, Value])
        end,
        maps:remove(status, Check)).
```

**Output:**
```
System Health Report:
  Overall Status: pass
  Timestamp: {{2025, 2, 11}, {14, 40, 22}}

  Control Executors:
    Status: pass
    expected_count: 7
    running_count: 7
    missing: []

  Evidence Generators:
    Status: pass
    expected_count: 4
    running_count: 4
    missing: []

  Receipt Chain:
    Status: pass
    receipt_count: 47
    root_hash: <<...>>

  Coverage:
    Status: pass
    required_count: 4
    missing_count: 0
    missing: []
```

### Example 4.2: Get Current System Health

```erlang
%% Check last known health without re-running validation
Health = soc2_meta_validator:get_system_health(),
case maps:get(status, Health) of
    pass ->
        io:format("System is healthy~n");
    fail ->
        io:format("System has issues:~n"),
        Checks = maps:get(checks, Health),
        lists:foreach(
            fun({CheckName, CheckResult}) ->
                case maps:get(status, CheckResult) of
                    fail ->
                        io:format("  - ~w: ~p~n", [CheckName, CheckResult]);
                    _ -> ok
                end
            end,
            maps:to_list(Checks))
end.
```

### Example 4.3: Create Health Monitoring Loop

```erlang
%% Monitor system health and alert if unhealthy
start_health_monitor() ->
    spawn(fun health_monitor_loop/0).

health_monitor_loop() ->
    Health = soc2_meta_validator:get_system_health(),
    case maps:get(status, Health) of
        fail ->
            log_alert(Health);
        _ ->
            ok
    end,
    timer:sleep(300000),  %% Check every 5 minutes
    health_monitor_loop().

log_alert(Health) ->
    logger:alert(#{
        what => soc2_system_unhealthy,
        health_report => Health
    }).
```

## 5. Generate Auditor Pack

Create a complete compliance package for auditors containing evidence, receipts, and metadata.

### Example 5.1: Generate Basic Auditor Pack

```erlang
%% Create a timestamped auditor package
generate_auditor_pack() ->
    Timestamp = calendar:universal_time(),
    {{Y, M, D}, {H, Min, S}} = Timestamp,
    PackageName = io_lib:format(
        "soc2_audit_~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B",
        [Y, M, D, H, Min, S]
    ),
    PackageDir = filename:join("audit_packages", PackageName),
    filelib:ensure_dir(filename:join(PackageDir, "dummy")),

    io:format("Generating auditor package: ~s~n", [PackageName]),

    %% 1. Export receipt chain
    export_receipts(PackageDir),

    %% 2. Export system health
    export_health_report(PackageDir),

    %% 3. Export evidence files
    export_evidence_files(PackageDir),

    %% 4. Create manifest
    create_manifest(PackageDir, Timestamp),

    io:format("Auditor package complete: ~s~n", [PackageDir]),
    {ok, PackageDir}.

export_receipts(PackageDir) ->
    Chain = soc2_receipt_chain:get_chain(),
    Json = jsx:encode(Chain, [{space, 2}]),
    FilePath = filename:join(PackageDir, "receipt_chain.json"),
    ok = file:write_file(FilePath, Json),
    io:format("  - receipt_chain.json (~w bytes)~n", [byte_size(Json)]).

export_health_report(PackageDir) ->
    Health = soc2_meta_validator:run_meta_validation(),
    Json = jsx:encode(Health, [{space, 2}]),
    FilePath = filename:join(PackageDir, "system_health.json"),
    ok = file:write_file(FilePath, Json),
    io:format("  - system_health.json (~w bytes)~n", [byte_size(Json)]).

export_evidence_files(PackageDir) ->
    EvidenceDir = "evidence",
    TargetDir = filename:join(PackageDir, "evidence"),
    filelib:ensure_dir(filename:join(TargetDir, "dummy")),
    case file:list_dir(EvidenceDir) of
        {ok, Files} ->
            lists:foreach(
                fun(File) ->
                    copy_file(filename:join(EvidenceDir, File),
                             filename:join(TargetDir, File))
                end,
                Files),
            io:format("  - copied ~w evidence files~n", [length(Files)]);
        _ ->
            io:format("  - no evidence files found~n", [])
    end.

copy_file(Src, Dst) ->
    case file:copy(Src, Dst) of
        {ok, _} -> ok;
        {error, R} ->
            logger:warning(#{what => copy_failed, src => Src, reason => R})
    end.

create_manifest(PackageDir, Timestamp) ->
    Manifest = #{
        package_generated => format_timestamp(Timestamp),
        cre_version => <<"0.3.0">>,
        contents => [
            "receipt_chain.json - Cryptographically signed validation receipts",
            "system_health.json - Meta-validator health report",
            "evidence/ - Raw evidence files (logs, metrics, etc.)"
        ],
        merkle_root => base64:encode(soc2_receipt_chain:get_merkle_root())
    },
    Json = jsx:encode(Manifest, [{space, 2}]),
    FilePath = filename:join(PackageDir, "MANIFEST.json"),
    ok = file:write_file(FilePath, Json),
    io:format("  - MANIFEST.json~n", []).

format_timestamp({{Y, M, D}, {H, Min, S}}) ->
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Y, M, D, H, Min, S])
    ).
```

**Output:**
```
Generating auditor package: soc2_audit_20250211_143012
  - receipt_chain.json (12847 bytes)
  - system_health.json (1203 bytes)
  - copied 87 evidence files
  - MANIFEST.json
Auditor package complete: audit_packages/soc2_audit_20250211_143012
```

### Example 5.2: List Available Auditor Packages

```erlang
%% Show all audit packages
list_audit_packages() ->
    case file:list_dir("audit_packages") of
        {ok, Packages} ->
            io:format("Available audit packages:~n"),
            lists:foreach(
                fun(Package) ->
                    PackagePath = filename:join("audit_packages", Package),
                    case file:list_dir(PackagePath) of
                        {ok, Files} ->
                            io:format("  ~s (~w files)~n", [Package, length(Files)]);
                        _ ->
                            ok
                    end
                end,
                lists:sort(Packages));
        {error, _} ->
            io:format("No audit packages directory~n")
    end.
```

### Example 5.3: Generate Compliance Summary

```erlang
%% Create a human-readable compliance summary for auditors
generate_compliance_summary() ->
    Status = soc2_validation_sup:get_validation_status(),
    Health = soc2_meta_validator:get_system_health(),
    Chain = soc2_receipt_chain:get_chain(),

    Summary = format_summary(Status, Health, Chain),
    file:write_file("compliance_summary.txt", Summary),
    io:format("Compliance summary written to compliance_summary.txt~n").

format_summary(Status, Health, Chain) ->
    PassCount = count_pass_controls(Status),
    TotalCount = map_size(Status),
    ReceiptCount = maps:get(receipt_count, Chain),
    RootHash = base64:encode(maps:get(root_hash, Chain)),
    HealthStatus = maps:get(status, Health),

    io:lib:format(
        "SOC 2 Compliance Status Report~n"
        "==============================~n"
        "Generated: ~s~n"
        "~n"
        "Overall Status: ~w~n"
        "Controls Passing: ~w/~w~n"
        "Total Validation Receipts: ~w~n"
        "Merkle Root Hash: ~s~n"
        "~n"
        "System Health: ~w~n"
        "~n"
        "Control Details:~n",
        [format_timestamp(calendar:universal_time()),
         HealthStatus,
         PassCount,
         TotalCount,
         ReceiptCount,
         RootHash,
         HealthStatus]
    ) ++ format_control_details(Status).

count_pass_controls(Status) ->
    maps:fold(
        fun(_ControlId, #{status := pass}, Count) -> Count + 1;
           (_ControlId, _, Count) -> Count
        end,
        0,
        Status).

format_control_details(Status) ->
    maps:fold(
        fun(ControlId, #{status := Status, last_validated := Time}, Acc) ->
            Acc ++ io_lib:format("  ~s: ~w (last: ~w)~n", [ControlId, Status, Time])
        end,
        "",
        Status).
```

## Common Tasks

### Restart a Failed Control Validator

```erlang
%% If a control executor crashes and doesn't auto-recover
ControlId = <<"CC8.1">>,
case soc2_control_executor:get_status(ControlId) of
    #{status := fail} ->
        logger:info(#{what => restarting_control, control_id => ControlId}),
        supervisor:restart_child(soc2_control_executor_sup,
                                binary_to_atom(<<"soc2_control_executor_", (string:lowercase(ControlId))/binary>>));
    _ ->
        ok
end.
```

### Export Receipts for a Specific Time Window

```erlang
%% Get receipts from the last N hours
get_recent_receipts(HoursBack) ->
    Chain = soc2_receipt_chain:get_chain(),
    Receipts = maps:get(receipts, Chain),
    CutoffTime = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - (HoursBack * 3600)
    ),

    RecentReceipts = lists:filter(
        fun(Receipt) ->
            Time = maps:get(timestamp, Receipt),
            Time >= CutoffTime
        end,
        Receipts
    ),

    io:format("Found ~w receipts from last ~w hours~n", [length(RecentReceipts), HoursBack]),
    RecentReceipts.
```

### Check Evidence File Integrity

```erlang
%% Verify all required evidence files exist
check_evidence_integrity() ->
    RequiredEvidence = [
        <<"receipts/build.last.json">>,
        <<"receipts/evidence.last.json">>,
        <<"receipts/verdict.last.json">>,
        <<"evidence/evidence.sha256">>
    ],

    MissingFiles = lists:filter(
        fun(Path) -> not filelib:is_regular(Path) end,
        RequiredEvidence
    ),

    case MissingFiles of
        [] ->
            io:format("All required evidence files are present~n");
        Missing ->
            io:format("Missing evidence files:~n"),
            lists:foreach(fun(F) -> io:format("  - ~s~n", [F]) end, Missing)
    end.
```

## Troubleshooting

### Control Validation Failing

Check what evidence is missing:
```erlang
{ok, Receipt} = soc2_validation_sup:validate_control(<<"CC6.1">>),
EvidenceResults = maps:get(evidence_results, Receipt),
lists:foreach(
    fun(#{evidence_path := Path, status := Status}) ->
        io:format("~s: ~w~n", [Path, Status])
    end,
    EvidenceResults).
```

### Receipt Chain Not Growing

Verify evidence generators are running:
```erlang
Health = soc2_meta_validator:get_system_health(),
EvidenceCheck = maps:get(evidence_generators, maps:get(checks, Health)),
io:format("Evidence Generators:~n"),
io:format("  Running: ~w/~w~n",
          [maps:get(running_count, EvidenceCheck),
           maps:get(expected_count, EvidenceCheck)]),
io:format("  Missing: ~p~n", [maps:get(missing, EvidenceCheck)]).
```

### System Health Failing

Run a detailed meta-validation to see which components are down:
```erlang
Health = soc2_meta_validator:run_meta_validation(),
maps:foreach(
    fun(CheckName, CheckResult) ->
        case maps:get(status, CheckResult) of
            fail -> io:format("FAILED: ~w - ~p~n", [CheckName, CheckResult]);
            pass -> io:format("OK: ~w~n", [CheckName])
        end
    end,
    maps:get(checks, Health)).
```

## References

- **Module Documentation**:
  - `soc2_validation_sup` - Supervision tree root
  - `soc2_control_executor` - Individual control validators
  - `soc2_evidence_gen` - Evidence generation
  - `soc2_receipt_chain` - Merkle tree accumulator
  - `soc2_meta_validator` - System health validator

- **Test Examples**: See `/home/user/cre/test/soc2_validation_SUITE.erl` for integration test patterns

- **Architecture**: See `/home/user/cre/docs/gcp/GCP_MARKETPLACE_READINESS.md` for deployment context
