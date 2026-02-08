# Debugging Quick Reference

Quick reference for CRE debugging tools and techniques.

## Scripts

| Script | Purpose | Usage |
|--------|----------|-------|
| `debug_trace.sh` | Trace function calls | `./scripts/debug_trace.sh gen_yawl step 1` |
| `debug_profile.sh` | Profile execution | `./scripts/debug_profile.sh eprof gen_yawl` |
| `debug_monitor.sh` | Real-time monitoring | `./scripts/debug_monitor.sh 1000` |
| `debug_recon.sh` | Recon utilities | `./scripts/debug_recon.sh top_memory` |
| `debug_redbug.sh` | Safe tracing | `./scripts/debug_redbug.sh gen_yawl step 10 5000` |
| `debug_observer.sh` | Observer GUI | `./scripts/debug_observer.sh` |
| `debug_system_info.sh` | System info | `./scripts/debug_system_info.sh` |
| `debug_process_tree.sh` | Process tree | `./scripts/debug_process_tree.sh yawl_engine` |
| `debug_crash_dump.sh` | Crash analysis | `./scripts/debug_crash_dump.sh erl_crash.dump` |
| `debug_eflame.sh` | Flame graphs | `./scripts/debug_eflame.sh /tmp/flame.txt` |

## Modules

### cre_debug_advanced

```erlang
% Inspect process
cre_debug_advanced:inspect_process(yawl_engine).

% System stats
cre_debug_advanced:system_stats().

% Memory summary
cre_debug_advanced:memory_summary().

% Top memory processes
cre_debug_advanced:top_memory_processes(10).

% Detect deadlocks
cre_debug_advanced:detect_deadlocks().

% Find bottlenecks
cre_debug_advanced:find_bottlenecks().

% Debug workflow
cre_debug_advanced:debug_workflow(WorkflowPid).
```

### cre_trace

```erlang
% Trace module
cre_trace:trace_module(gen_yawl, #{}).

% Trace function
cre_trace:trace_function(gen_yawl, step, 1).

% Trace workflow
cre_trace:trace_workflow(WorkflowPid, #{level => transitions}).

% Safe trace (redbug)
cre_trace:safe_trace(gen_yawl, step, 1).
```

### cre_profiler

```erlang
% Profile workflow
cre_profiler:profile_workflow(WorkflowPid, #{type => eprof}).

% Profile function
{Result, Trace} = cre_profiler:fprof_profile(fun() -> ... end).

% Profile transition
cre_profiler:profile_transition(WorkflowPid, t_GoNoGo, #{}).
```

### cre_monitor

```erlang
% Start monitoring
cre_monitor:start_link(),
cre_monitor:start_monitoring(#{interval => 1000}).

% Monitor process
cre_monitor:monitor_process(WorkflowPid).

% Set threshold
cre_monitor:set_alert_threshold(memory, 100 * 1024 * 1024).

% Get alerts
cre_monitor:get_alerts().
```

## Common Tasks

### Inspect a Process

```erlang
Info = cre_debug_advanced:inspect_process(Pid),
io:format("Memory: ~p bytes~n", [maps:get(memory, Info)]),
io:format("Queue: ~p messages~n", [maps:get(message_queue_len, Info)]).
```

### Trace Function Calls

```bash
./scripts/debug_trace.sh gen_yawl step 1
```

### Profile Performance

```erlang
Profile = cre_profiler:profile_workflow(WorkflowPid, #{
    type => eprof,
    max_steps => 100
}).
```

### Monitor System

```bash
./scripts/debug_monitor.sh 1000
```

### Analyze Memory

```erlang
Mem = cre_debug_advanced:memory_summary(),
Top = cre_debug_advanced:top_memory_processes(10).
```

### Detect Deadlocks

```erlang
Deadlocks = cre_debug_advanced:detect_deadlocks().
```

### Analyze Crash Dump

```bash
./scripts/debug_crash_dump.sh erl_crash.dump
```

## Observer GUI

```bash
# Start Observer
./scripts/debug_observer.sh

# Or from Erlang shell
observer:start().
```

## Optional Dependencies

To enable advanced features, add to `rebar.config`:

```erlang
{profiles, [
    {debug, [{deps, [
        {recon, {git, "https://github.com/ferd/recon.git"}}},
        {redbug, {git, "https://github.com/massemanet/redbug.git"}}},
        {eflame, {git, "https://github.com/proger/eflame.git"}}
    ]}]}
]}.
```

Then install:

```bash
rebar3 as debug deps
```

## See Also

- [Full Debugging Guide](DEBUGGING.md)
- [Troubleshooting Guide](TROUBLESHOOTING.md)
