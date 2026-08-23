# Workflow Case Supervision

## Overview

The `wf_case_sup` module provides unified supervision for workflow case execution across multiple workflow engines. It implements OTP supervisor best practices with fault tolerance, clean lifecycle management, and process registry integration.

## Features

- **Unified Supervision**: Single supervisor for multiple workflow engine types (gen_yawl, wf_engine, custom)
- **OTP Compliant**: Implements `simple_one_for_one` strategy for dynamic child creation
- **Fault Tolerance**: Isolated case failures don't affect other cases or the supervisor
- **Clean Shutdown**: Proper termination and cleanup of case resources
- **Process Registry**: Integration with gproc for case lookup and metadata
- **Query Interface**: List active cases, find by ID, get status information

## Architecture

```
cre_sup (top-level supervisor)
  └── wf_case_sup (simple_one_for_one supervisor)
        ├── wf_case_runner (case_abc123) → gen_yawl process
        ├── wf_case_runner (case_def456) → gen_yawl process
        └── wf_case_runner (case_ghi789) → custom workflow
```

Each case is wrapped in a `wf_case_runner` gen_server that:
- Delegates to the appropriate workflow engine
- Tracks case metadata (case_id, spec_id, status, timestamps)
- Registers with gproc for lookup
- Provides uniform query interface
- Handles graceful shutdown

## Basic Usage

### Starting a Case

```erlang
% Start a gen_yawl workflow case
{ok, CasePid} = wf_case_sup:start_case(
    my_workflow_module,  % SpecId (net module for gen_yawl)
    #{data => #{amount => 100}},  % Initial data
    #{type => gen_yawl}  % Options
).
```

### Listing Active Cases

```erlang
Cases = wf_case_sup:list_cases().
% Returns: [#{
%   case_id => <<"case_abc123">>,
%   spec_id => my_workflow_module,
%   type => gen_yawl,
%   status => running,
%   started_at => 1706901234567
% }]
```

### Finding a Case

```erlang
case wf_case_sup:find_case(CaseId) of
    {ok, Pid} ->
        % Case found, Pid is the wf_case_runner process
        ok;
    {error, not_found} ->
        % Case doesn't exist or has completed
        not_found
end.
```

### Getting Case Status

```erlang
{ok, Info} = wf_case_sup:get_case_status(CaseId),
% Info contains: case_id, spec_id, type, status, started_at,
%               completed_at, workflow_pid
```

### Stopping a Case

```erlang
% Stop with default timeout (5000ms)
ok = wf_case_sup:stop_case(CaseId).

% Stop with custom timeout
ok = wf_case_sup:stop_case(CaseId, 10000).
```

### Counting Active Cases

```erlang
Count = wf_case_sup:case_count().
% Returns: number of currently supervised cases
```

## API Reference

### `start_link() -> {ok, Pid} | {error, Reason}`

Starts the workflow case supervisor. Usually called automatically by `cre_sup`.

### `start_case(SpecId, InitialData, Options) -> {ok, Pid} | {error, Reason}`

Starts a new workflow case under supervision.

**Parameters:**
- `SpecId`: Workflow specification identifier (module name for gen_yawl)
- `InitialData`: Initial case data map
- `Options`: Case options map
  - `type`: Workflow type (`gen_yawl`, `wf_engine`, `custom`)
  - `timeout`: Maximum execution time (default: infinity)
  - `auto_continue`: Auto-continue workflow (default: true)

**Returns:**
- `{ok, Pid}`: Case started successfully, Pid is the wf_case_runner process
- `{error, Reason}`: Failed to start case

### `stop_case(CaseId) -> ok | {error, Reason}`

Stops a workflow case with default timeout (5000ms).

### `stop_case(CaseId, Timeout) -> ok | {error, Reason}`

Stops a workflow case with custom timeout.

### `list_cases() -> [CaseInfo]`

Returns list of all active cases with their metadata.

### `find_case(CaseId) -> {ok, Pid} | {error, not_found}`

Finds a case by its ID.

### `get_case_status(CaseId) -> {ok, CaseInfo} | {error, not_found}`

Gets detailed status information for a case.

### `case_count() -> non_neg_integer()`

Returns the number of currently active cases.

## Workflow Engine Support

### gen_yawl

Full support for gen_yawl-based workflows:

```erlang
{ok, _Pid} = wf_case_sup:start_case(
    my_yawl_workflow,
    #{input => data},
    #{type => gen_yawl}
).
```

### wf_engine

Currently not supported at process level. wf_engine cases remain as data within the engine process. This may be enhanced in future versions.

### Custom Implementations

Support for custom workflow implementations:

```erlang
{ok, _Pid} = wf_case_sup:start_case(
    custom_spec,
    #{input => data},
    #{
        type => custom,
        start_module => my_custom_engine,
        start_function => start_workflow
    }
).
```

## Comparison with Existing Supervisors

### yawl_workflow_supervisor

- **Scope**: gen_yawl workflows only
- **Pattern**: Direct supervision of gen_yawl processes
- **Use when**: You only use gen_yawl and want minimal overhead

### yawl_supervisor

- **Scope**: gen_yawl workflows with advanced features
- **Pattern**: Feature-rich supervisor with gproc metadata
- **Features**: Pause/resume, comprehensive registry
- **Use when**: You need advanced workflow management features

### wf_case_sup

- **Scope**: Multiple workflow engine types
- **Pattern**: Case runner wrapper with unified interface
- **Features**: Mixed engine types, clean lifecycle
- **Use when**: You use multiple workflow engines or want unified supervision

## Migration Notes

**No migration required** - This is additive functionality. Existing code using `yawl_workflow_supervisor` or `yawl_supervisor` continues to work unchanged.

### Adoption Path

1. **New code**: Use `wf_case_sup` directly for all new workflow cases
2. **Existing code**: Continue using existing supervisors
3. **Incremental migration**: Gradually migrate existing code if desired
4. **Coexistence**: All three supervisors can coexist indefinitely

### Migration Example

**Before (using yawl_workflow_supervisor):**

```erlang
{ok, Pid} = yawl_workflow_supervisor:start_workflow(
    my_workflow_module,
    #{},
    []
).
```

**After (using wf_case_sup):**

```erlang
{ok, Pid} = wf_case_sup:start_case(
    my_workflow_module,
    #{},
    #{type => gen_yawl}
).
```

## Error Handling

### Case Startup Failures

If a case fails to start:
- The supervisor logs the error
- No child process is created
- Other cases are unaffected
- Returns `{error, Reason}` to caller

### Case Runtime Failures

If a case crashes during execution:
- The case runner traps exits and updates status
- Workflow process is terminated
- Case is marked as `failed`
- Other cases are unaffected
- No automatic restart (temporary restart strategy)

### Supervisor Failures

If the supervisor crashes:
- All cases are terminated
- Supervisor is restarted by cre_sup (permanent restart)
- Cases are not automatically restarted (by design)

## Monitoring and Telemetry

The supervisor integrates with existing CRE monitoring:

- **Process registry**: gproc metadata for all cases
- **Case lifecycle**: Start, completion, cancellation events
- **Status tracking**: Real-time case status via `get_case_status/1`
- **Metrics**: Active case count via `case_count/0`

## Best Practices

1. **Always clean up completed cases**: Use `stop_case/1` to release resources
2. **Monitor case count**: Use `case_count/0` to detect runaway case creation
3. **Handle errors**: Check return values from `start_case/3`
4. **Use appropriate timeouts**: Set reasonable timeouts in options
5. **Query before acting**: Use `find_case/1` before operating on cases

## Future Enhancements

Planned improvements for future versions:

- Effect worker supervision (per-case or global)
- Per-case metrics and telemetry
- Distributed case execution across nodes
- wf_engine process-level supervision
- Case data persistence integration
- Hot code upgrade support

## See Also

- [wf_engine documentation](../../src/wf/wf_engine.erl)
- [gen_yawl documentation](../../src/core/gen_yawl.erl)
- [yawl_workflow_supervisor](../../src/app/yawl_workflow_supervisor.erl)
- [yawl_supervisor](../../src/yawl/yawl_supervisor.erl)
- [CRE supervisor](../../src/app/cre_sup.erl)
