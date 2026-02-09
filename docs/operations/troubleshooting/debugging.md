# Advanced Erlang Debugging Guide

This guide covers the comprehensive debugging tools and techniques available in CRE for diagnosing workflow issues, performance problems, and system behavior.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Debugging Modules](#debugging-modules)
3. [Tracing](#tracing)
4. [Profiling](#profiling)
5. [Monitoring](#monitoring)
6. [Process Inspection](#process-inspection)
7. [Memory Analysis](#memory-analysis)
8. [Crash Dump Analysis](#crash-dump-analysis)
9. [Workflow-Specific Debugging](#workflow-specific-debugging)
10. [Scripts Reference](#scripts-reference)
11. [Best Practices](#best-practices)

## Quick Start

### Basic Process Inspection

```erlang
% Inspect a process
cre_debug_advanced:inspect_process(gen_yawl).

% Get system statistics
cre_debug_advanced:system_stats().

% Check memory usage
cre_debug_advanced:memory_summary().
```

### Trace a Function

```bash
# Using dbg
./scripts/debug_trace.sh gen_yawl step 1

# Using redbug (safer, limited)
./scripts/debug_redbug.sh gen_yawl step 10 5000
```

### Profile Workflow Execution

```bash
# Using eprof
./scripts/debug_profile.sh eprof gen_yawl

# Using fprof
./scripts/debug_profile.sh fprof gen_pnet
```

### Start Observer GUI

```bash
./scripts/debug_observer.sh
```

## Debugging Modules

### cre_debug_advanced

Comprehensive debugging utilities for process inspection, memory analysis, and system diagnostics.

**Key Functions:**

- `inspect_process/1` - Detailed process inspection
- `inspect_process_tree/1` - Process tree visualization
- `memory_summary/0` - System memory overview
- `top_memory_processes/1` - Find memory-intensive processes
- `detect_deadlocks/0` - Detect circular dependencies
- `find_bottlenecks/0` - Identify performance bottlenecks

**Example:**

```erlang
% Inspect a workflow process
Info = cre_debug_advanced:inspect_process(yawl_engine),
io:format("Memory: ~p bytes~n", [maps:get(memory, Info)]).

% Find top 10 memory consumers
TopMem = cre_debug_advanced:top_memory_processes(10),
[io:format("~p: ~p bytes~n", [P, M]) || {P, M, _} <- TopMem].

% Detect deadlocks
Deadlocks = cre_debug_advanced:detect_deadlocks(),
case Deadlocks of
    [] -> io:format("No deadlocks detected~n");
    _ -> [io:format("Deadlock: ~p <-> ~p~n", [P1, P2]) || {P1, P2, _} <- Deadlocks]
end.
```

### cre_trace

Advanced tracing utilities with support for dbg and redbug.

**Key Functions:**

- `trace_module/2` - Trace all functions in a module
- `trace_function/3` - Trace a specific function
- `trace_process/2` - Trace a process
- `trace_messages/1` - Trace messages
- `trace_workflow/2` - Workflow-specific tracing

**Example:**

```erlang
% Trace all gen_yawl functions
cre_trace:trace_module(gen_yawl, #{}).

% Trace specific function
cre_trace:trace_function(gen_yawl, step, 1).

% Trace workflow execution
cre_trace:trace_workflow(WorkflowPid, #{level => transitions}).

% Safe tracing with redbug (if available)
cre_trace:safe_trace(gen_yawl, step, 1).
```

### cre_profiler

Profiling utilities using fprof, eprof, eflame, and cprof.

**Key Functions:**

- `fprof_profile/1` - Profile with fprof
- `eprof_profile/1` - Profile with eprof
- `profile_workflow/2` - Profile workflow execution
- `profile_transition/3` - Profile transition firing

**Example:**

```erlang
% Profile a function
{Result, TraceFile} = cre_profiler:fprof_profile(fun() ->
    gen_yawl:step(WorkflowPid)
end),
cre_profiler:fprof_analyze(TraceFile).

% Profile workflow
Profile = cre_profiler:profile_workflow(WorkflowPid, #{
    type => eprof,
    max_steps => 100
}).
```

### cre_monitor

Real-time system monitoring with alerting.

**Key Functions:**

- `start_monitoring/1` - Start monitoring
- `monitor_process/1` - Monitor a process
- `set_alert_threshold/2` - Set alert thresholds
- `get_alerts/0` - Get current alerts

**Example:**

```erlang
% Start monitoring
cre_monitor:start_link(),
cre_monitor:start_monitoring(#{interval => 1000}),

% Monitor a process
cre_monitor:monitor_process(WorkflowPid),

% Set memory threshold
cre_monitor:set_alert_threshold(memory, 100 * 1024 * 1024),

% Check alerts
Alerts = cre_monitor:get_alerts().
```

## Tracing

### Using dbg

The built-in dbg tracer provides powerful but potentially intrusive tracing.

```erlang
% Start tracer
dbg:tracer().

% Trace all calls
dbg:p(all, [c, timestamp]).

% Trace specific module/function
dbg:tpl(gen_yawl, step, 1, x).

% Stop tracing
dbg:stop_clear().
```

### Using redbug (Recommended)

Redbug provides safe tracing with automatic limits.

**Installation:**

```erlang
% Add to rebar.config
{deps, [{redbug, {git, "https://github.com/massemanet/redbug.git"}}]}.
```

**Usage:**

```erlang
% Trace with limits (10 calls, 5 seconds)
redbug:start(#{time => 5000, msgs => 10}),
redbug:tp(gen_yawl, step, 1, []),
timer:sleep(6000),
redbug:stop().
```

### Workflow Tracing

```erlang
% Trace transitions only
cre_trace:trace_transitions(WorkflowPid).

% Trace marking changes
cre_trace:trace_marking_changes(WorkflowPid).

% Full workflow trace
cre_trace:trace_workflow(WorkflowPid, #{level => all}).
```

## Profiling

### fprof - Function Call Profiling

fprof provides detailed function call graphs and timing information.

```erlang
% Profile a function
{Result, TraceFile} = cre_profiler:fprof_profile(fun() ->
    gen_yawl:drain(WorkflowPid, 1000)
end),

% Analyze results
cre_profiler:fprof_analyze(TraceFile).
```

**Output:** Detailed call graph showing:
- Total time per function
- Self time per function
- Call counts
- Call hierarchy

### eprof - Execution Time Profiling

eprof provides execution time statistics.

```erlang
% Profile execution
{Result, Analysis} = cre_profiler:eprof_profile(fun() ->
    gen_yawl:step(WorkflowPid)
end),

% View analysis
io:format("~p~n", [Analysis]).
```

### eflame - Flame Graphs

eflame generates flame graphs for visual performance analysis.

**Installation:**

```erlang
% Add to rebar.config
{deps, [{eflame, {git, "https://github.com/proger/eflame.git"}}]}.
```

**Usage:**

```bash
# Generate flame graph
./scripts/debug_eflame.sh /tmp/flame.txt

# Convert to SVG (requires flamegraph.pl)
~/bin/stackcollapse-erl.pl /tmp/flame.txt | flamegraph.pl > flame.svg
```

### cprof - Count Profiling

cprof counts function calls without timing overhead.

```erlang
% Start profiling
cre_profiler:cprof_start([gen_yawl, gen_pnet]),

% Run code
gen_yawl:step(WorkflowPid),

% Analyze
Analysis = cre_profiler:cprof_analyze().
```

## Monitoring

### Real-Time Monitoring

```erlang
% Start monitor
cre_monitor:start_link(),
cre_monitor:start_monitoring(#{interval => 1000}),

% Monitor specific processes
cre_monitor:monitor_process(WorkflowPid),

% Set thresholds
cre_monitor:set_alert_threshold(memory, 100 * 1024 * 1024),
cre_monitor:set_alert_threshold(message_queue_len, 1000),

% Get statistics
Stats = cre_monitor:get_statistics().
```

### System Statistics

```erlang
% Get comprehensive stats
Stats = cre_debug_advanced:system_stats(),
io:format("Processes: ~p~n", [maps:get(processes, Stats)]),
io:format("Memory: ~p bytes~n", [maps:get(memory, Stats)]).

% Scheduler stats
Sched = cre_debug_advanced:scheduler_stats(),
io:format("Schedulers: ~p~n", [maps:get(schedulers, Sched)]).

% GC stats
GC = cre_debug_advanced:gc_stats(),
io:format("GCs: ~p~n", [maps:get(number_of_gcs, GC)]).
```

## Process Inspection

### Inspect Individual Process

```erlang
% Inspect by PID
Info = cre_debug_advanced:inspect_process(Pid),
io:format("Status: ~p~n", [maps:get(status, Info)]),
io:format("Memory: ~p bytes~n", [maps:get(memory, Info)]),
io:format("Queue: ~p messages~n", [maps:get(message_queue_len, Info)]).

% Inspect by name
Info = cre_debug_advanced:inspect_process(yawl_engine).
```

### Process Tree

```erlang
% Get process tree
Tree = cre_debug_advanced:inspect_process_tree(WorkflowPid),

% Visualize
print_tree(Tree).
```

### Find Processes

```erlang
% Find by criteria
HighMemory = cre_debug_advanced:find_processes(fun(Info) ->
    maps:get(memory, Info, 0) > 10 * 1024 * 1024
end).

% Find registered processes
YawlProcs = cre_debug_advanced:find_registered("yawl").
```

## Memory Analysis

### Memory Summary

```erlang
% System-wide memory
Mem = cre_debug_advanced:memory_summary(),
io:format("Total: ~p MB~n", [maps:get(total, Mem) div 1024 div 1024]),
io:format("Processes: ~p MB~n", [maps:get(processes, Mem) div 1024 div 1024]),
io:format("ETS: ~p MB~n", [maps:get(ets, Mem) div 1024 div 1024]).
```

### Memory by Module

```erlang
% Code memory usage
ModMem = cre_debug_advanced:memory_by_module(),
Sorted = lists:reverse(lists:sort(ModMem)),
[io:format("~p: ~p bytes~n", [M, S]) || {M, S} <- lists:sublist(Sorted, 10)].
```

### Memory Leak Detection

```erlang
% Check for potential leaks
Leaks = cre_debug_advanced:memory_leak_check(),
case Leaks of
    [] -> io:format("No leaks detected~n");
    _ -> [io:format("Potential leak: ~p (~p bytes)~n", [P, M]) || {P, M, _} <- Leaks]
end.
```

### Top Memory Processes

```erlang
% Top 10 memory consumers
Top = cre_debug_advanced:top_memory_processes(10),
[io:format("~p: ~p MB~n", [P, M div 1024 div 1024]) || {P, M, _} <- Top].
```

## Crash Dump Analysis

### Analyze Crash Dump

```bash
# Analyze crash dump
./scripts/debug_crash_dump.sh erl_crash.dump
```

```erlang
% Programmatic analysis
Analysis = cre_debug_advanced:analyze_crash_dump("erl_crash.dump"),
io:format("Processes: ~p~n", [maps:get(processes, Analysis)]),
io:format("ETS tables: ~p~n", [maps:get(ets_tables, Analysis)]).
```

### Extract Process Info

```erlang
% Extract specific process info
Info = cre_debug_advanced:extract_process_info("erl_crash.dump", yawl_engine),
io:format("~p~n", [Info]).
```

## Workflow-Specific Debugging

### Debug Workflow State

```erlang
% Comprehensive workflow debugging
Debug = cre_debug_advanced:debug_workflow(WorkflowPid),
io:format("Process: ~p~n", [maps:get(process, Debug)]),
io:format("State: ~p~n", [maps:get(workflow_state, Debug)]).

% Workflow state only
State = cre_debug_advanced:debug_workflow_state(WorkflowPid),
io:format("Marking: ~p~n", [maps:get(marking, State)]),
io:format("Enabled: ~p~n", [maps:get(enabled_transitions, State)]).
```

### Profile Workflow

```erlang
% Profile workflow execution
Profile = cre_profiler:profile_workflow(WorkflowPid, #{
    type => eprof,
    max_steps => 100
}).

% Profile specific transition
Profile = cre_profiler:profile_transition(WorkflowPid, t_GoNoGo, #{}).
```

### Trace Workflow

```erlang
% Trace with custom options
cre_trace:trace_workflow(WorkflowPid, #{
    level => transitions,
    file => "/tmp/workflow.trace"
}).
```

## Scripts Reference

### debug_trace.sh

Trace function calls using dbg.

```bash
# Trace gen_yawl:step/1
./scripts/debug_trace.sh gen_yawl step 1

# Trace gen_pnet:fire/3
./scripts/debug_trace.sh gen_pnet fire 3
```

### debug_profile.sh

Profile execution using eprof or fprof.

```bash
# Profile with eprof
./scripts/debug_profile.sh eprof gen_yawl

# Profile with fprof
./scripts/debug_profile.sh fprof gen_pnet
```

### debug_monitor.sh

Start real-time monitoring.

```bash
# Monitor with 1 second interval
./scripts/debug_monitor.sh 1000
```

### debug_recon.sh

Use recon library (if available).

```bash
# Top memory processes
./scripts/debug_recon.sh top_memory

# Top message queues
./scripts/debug_recon.sh top_queue

# Binary leak analysis
./scripts/debug_recon.sh bin_leak
```

### debug_redbug.sh

Safe tracing with redbug.

```bash
# Trace 10 calls over 5 seconds
./scripts/debug_redbug.sh gen_yawl step 10 5000
```

### debug_observer.sh

Start Observer GUI.

```bash
# Local observer
./scripts/debug_observer.sh

# Connect to remote node
./scripts/debug_observer.sh node@hostname
```

### debug_system_info.sh

Display system information.

```bash
./scripts/debug_system_info.sh
```

### debug_process_tree.sh

Visualize process tree.

```bash
# Tree from init
./scripts/debug_process_tree.sh init

# Tree from specific process
./scripts/debug_process_tree.sh yawl_engine
```

### debug_crash_dump.sh

Analyze crash dump.

```bash
./scripts/debug_crash_dump.sh erl_crash.dump
```

### debug_eflame.sh

Generate flame graph.

```bash
./scripts/debug_eflame.sh /tmp/flame.txt
```

## Best Practices

### 1. Start with Non-Intrusive Tools

- Use `cre_debug_advanced:inspect_process/1` before tracing
- Use Observer GUI for visual inspection
- Use `cre_monitor` for continuous monitoring

### 2. Use Safe Tracing

- Prefer `redbug` over `dbg` for production systems
- Set reasonable limits (time, message count)
- Use `cre_trace:safe_trace/3` when available

### 3. Profile Before Optimizing

- Use `cre_profiler:profile_workflow/2` to identify bottlenecks
- Generate flame graphs for visual analysis
- Compare before/after profiles

### 4. Monitor Continuously

- Set up `cre_monitor` with appropriate thresholds
- Monitor memory, message queues, and process counts
- Review alerts regularly

### 5. Analyze Crash Dumps

- Always analyze crash dumps after crashes
- Use `cre_debug_advanced:analyze_crash_dump/1`
- Extract process-specific information

### 6. Workflow-Specific Debugging

- Use `cre_debug_advanced:debug_workflow/1` for comprehensive analysis
- Trace transitions and marking changes separately
- Profile specific operations

### 7. Memory Management

- Regularly check for memory leaks
- Monitor top memory consumers
- Analyze ETS table sizes

### 8. Performance Analysis

- Use `cre_debug_advanced:find_bottlenecks/0`
- Profile hot paths with eprof/fprof
- Generate flame graphs for complex workflows

## Troubleshooting

### Tracing Not Working

- Ensure `debug_info` is enabled in `rebar.config`
- Check that modules are loaded: `code:ensure_loaded(Module)`
- Verify process is alive: `erlang:is_process_alive(Pid)`

### Profiling Overhead

- fprof has significant overhead, use sparingly
- eprof has moderate overhead
- cprof has minimal overhead

### Observer Not Starting

- Ensure GUI environment (X11/Wayland) is available
- Check Erlang/OTP version compatibility
- Try connecting to remote node instead

### Memory Analysis Issues

- Some functions require root privileges
- Large systems may take time to analyze
- Consider sampling instead of full analysis

## Additional Resources

- [Erlang/OTP Debugging](https://www.erlang.org/doc/apps/debugger/debugger_chapter.html)
- [recon Documentation](https://github.com/ferd/recon)
- [redbug Documentation](https://github.com/massemanet/redbug)
- [eflame Documentation](https://github.com/proger/eflame)
- [Observer User's Guide](https://www.erlang.org/doc/apps/observer/observer_ug.html)
