# CRE Troubleshooting Guide

**Comprehensive troubleshooting documentation for the CRE YAWL workflow engine.**

This guide provides systematic approaches to diagnosing and resolving common issues encountered during development, testing, and production use of CRE.

---

## Table of Contents

1. [Quick Diagnostics](#quick-diagnostics)
2. [Common Issues](#common-issues)
3. [Debugging Tools](#debugging-tools)
4. [Log Analysis](#log-analysis)
5. [State Inspection](#state-inspection)
6. [Performance Issues](#performance-issues)
7. [Deadlock Diagnosis](#deadlock-diagnosis)
8. [Memory Issues](#memory-issues)
9. [Test Failures](#test-failures)
10. [Getting Help](#getting-help)

---

## Quick Diagnostics

### First Steps

When encountering an issue, start with these diagnostic commands:

```bash
# Check compilation
rebar3 compile

# Run tests with verbose output
rebar3 eunit -v
rebar3 ct -v

# Check for type issues
rebar3 dialyzer

# Check cross-references
rebar3 xref

# Check formatting
rebar3 efmt -c
```

### Gather System Information

```erlang
% From Erlang shell
erlang:system_info(otp_release).        % OTP version
erlang:system_info(system_architecture). % Architecture
erlang:memory(total).                    % Total memory
erlang:system_info(process_count).       % Process count
erlang:system_info(port_count).          % Port count
application:which_applications().        % Loaded applications
```

---

## Common Issues

### Compilation Errors

#### `undefined type` or `undefined record`

**Symptoms:**
```
src/my_module.erl:42: type net_element() is undefined
src/my_module.erl:55: record pattern_state is undefined
```

**Causes:**
- Missing include directive for header files
- Header file not in the include path
- Circular dependency between modules

**Solutions:**

1. Add missing include directives:
```erlang
-include("gen_pnet.hrl").
-include_lib("kernel/include/logger.hrl").
```

2. Verify include paths in `rebar.config`:
```erlang
{erl_opts, [
    {i, "include"},
    {i, "src"}
]}.
```

3. For circular dependencies, use `-define()` constants in a shared header file.

---

#### `unused variable` Warnings

**Symptoms:**
```
Warning: variable 'From' is unused
Warning: variable 'Ref' is shadowed
```

**Solutions:**

Prefix unused variables with underscore:
```erlang
% Before:
handle_info(Msg, State) ->
    {noreply, State}.

% After:
handle_info(_Msg, State) ->
    {noreply, State}.
```

---

### OTP Version Compatibility

**Symptoms:**
```
unbound type variable error in cowlib
function_clause in logger module
deprecated function erlang:get_stacktrace/0
```

**Solution:**

Ensure OTP version 25+ is installed:
```bash
erl -version  # Should show 25 or higher
```

For OTP 28+, add cowlib overrides to `rebar.config`:
```erlang
{overrides, [
    {override, cowboy, [
        {deps, [
            {cowlib, {git, "https://github.com/ninenines/cowlib.git", {tag, "2.16.0"}}},
            {ranch, {git, "https://github.com/ninenines/ranch.git", {tag, "2.1.0"}}}
        ]}
    ]}
]}.
```

Clean and rebuild:
```bash
rm -rf _build/
rebar3 compile
```

---

### Pattern Match Failures

#### `badmatch` Errors

**Symptoms:**
```
{badmatch, {error, something}}
```

**Causes:**
- Value does not match expected pattern
- Function returned unexpected type

**Solutions:**

1. Use explicit pattern matching:
```erlang
case some_function() of
    {ok, Result} -> process(Result);
    {error, Reason} -> handle_error(Reason)
end
```

2. Add type guards:
```erlang
is_valid_result({ok, _}) -> true;
is_valid_result({error, _}) -> true;
is_valid_result(_) -> false.
```

---

#### `case_clause` Errors

**Symptoms:**
```
{case_clause, unexpected_value}
```

**Solution:**

Always include catch-all clause:
```erlang
case Value of
    expected -> handle_expected();
    another_expected -> handle_another();
    Unexpected -> logger:warning("Unexpected value: ~p", [Unexpected])
end
```

---

### Runtime Errors

#### `noproc` - Process Not Found

**Symptoms:**
```
{noproc, {gen_server, call, [undefined, request]}}
```

**Causes:**
- Process not started
- Process crashed
- Process registered under different name

**Solutions:**

1. Check if process is registered:
```erlang
case whereis(process_name) of
    undefined -> {error, not_found};
    Pid -> {ok, Pid}
end.
```

2. Verify process is alive:
```erlang
erlang:is_process_alive(Pid).
```

3. Check process info:
```erlang
erlang:process_info(Pid, [current_function, message_queue_len]).
```

---

#### `timeout` Errors

**Symptoms:**
```
{timeout, {gen_server, call, [Pid, Request, 5000]}}
```

**Causes:**
- Gen_server not processing calls
- Long-running operation
- Deadlock

**Solutions:**

1. Increase timeout:
```erlang
gen_server:call(Pid, Request, 10000).
```

2. Check message queue:
```erlang
{message_queue_len, Len} = erlang:process_info(Pid, message_queue_len).
```

3. Use cast for fire-and-forget:
```erlang
gen_server:cast(Pid, Request).
```

---

## Debugging Tools

### Observer

The Erlang Observer provides a GUI for system inspection.

```bash
# Start Observer GUI
erl -s observer start

# Or from running node
observer:start().
```

**Key Features:**
- Process viewer (inspect message queues, memory, stack traces)
- ETS table viewer
- Application viewer
- Load charts
- Trace tool

---

### Debugger

Interactive command-line debugger:

```erlang
% Attach to a module
debugger:start().
int:interpret(my_module).

% Set breakpoints
int:break(my_module, function_name, 2).

% Step through code
int:next().
int:continue().
```

---

### Recon (Recommended)

Production-safe debugging library.

**Installation:**
```erlang
% Add to rebar.config
{deps, [{recon, {git, "https://github.com/ferd/recon.git"}}]}.
```

**Key Functions:**

```erlang
% Process info
recon:info(Pid).
recon:trace({module, function, arity}, 10).

% Memory analysis
recon:bin_leak(100).
recon:port_types/0.

% Top consumers
recon:top(10, [memory, reductions, message_queue_len]).
```

---

### dbg (Built-in Tracer)

```erlang
% Start tracer
dbg:tracer().
dbg:p(all, c).

% Trace module functions
dbg:tpl(gen_yawl, step, 1, x).

% Trace specific process
dbg:p(Pid, [c, timestamp]).

% Stop tracing
dbg:stop_clear().
```

---

### Advanced Debugging Modules

#### cre_debug_advanced

```erlang
% Inspect process
cre_debug_advanced:inspect_process(yawl_engine).

% System stats
cre_debug_advanced:system_stats().

% Memory summary
cre_debug_advanced:memory_summary().

% Top memory consumers
cre_debug_advanced:top_memory_processes(10).

% Detect deadlocks
cre_debug_advanced:detect_deadlocks().

% Find bottlenecks
cre_debug_advanced:find_bottlenecks().
```

#### cre_trace

```erlang
% Trace module
cre_trace:trace_module(gen_yawl, #{}).

% Trace function
cre_trace:trace_function(gen_yawl, step, 1).

% Trace workflow
cre_trace:trace_workflow(WorkflowPid, #{level => transitions}).

% Safe tracing (redbug)
cre_trace:safe_trace(gen_yawl, step, 1).
```

---

## Log Analysis

### Structured Logging

Configure logger with metadata:

```erlang
% Set log level
logger:set_primary_config(level, all).

% Configure formatter
logger:set_handler_config(default, formatter,
    {logger_formatter, #{template => [time, " ", level, " [",
        {module, [{module, undefined, "???"}], ":", line, "] ",
        message, "\n"]}}).

% Log with metadata
logger:info("Workflow started",
    #{workflow_id => Id, case_id => CaseId}).
```

---

### Filtering Logs

```erlang
% Filter by metadata
logger:filter_module(my_filter, fun
    (#{level := Level, meta := #{workflow_id := Id}}) when Id =:= <<"debug_wf">> ->
        Level =:= debug;
    (_) -> true
end).
```

---

### OpenTelemetry Trace Analysis

```erlang
% Start a span
otel_telemetry:start_span(<<"workflow.execute">>, #{
    workflow_id => Id,
    case_id => CaseId
}).

% Add events
otel_telemetry:add_event(<<"task.completed">>, #{
    task_id => TaskId,
    duration => DurationMs
}).

% End span
otel_telemetry:end_span().
```

View traces in Jaeger or compatible backend.

---

### XES Event Log Analysis

XES logs capture workflow execution for process mining:

```erlang
% Export XES log
yawl_xes:export_log(LogId, "workflow_trace.xes").
```

---

## State Inspection

### gen_server State Inspection

```erlang
% Get state via sys
sys:get_state(Pid).

% Get state with more detail
sys:get_status(Pid).
```

### gen_pnet State Inspection

```erlang
% Get current marking
{ok, Marking} = gen_pnet:marking(NetPid).

% Check statistics
{ok, Stats} = gen_pnet:stats(NetPid).

% Query specific place
case gen_pnet:ls(NetPid, [place_name]) of
    {ok, [{place_name, Tokens}]} ->
        io:format("Tokens: ~p~n", [Tokens]);
    {error, #bad_place{name = Place}} ->
        io:format("Place ~p does not exist~n", [Place])
end.
```

### gen_yawl Workflow State

```erlang
% Get workflow state
{ok, WorkflowState} = gen_yawl:get_state(NetPid).

% Get marking
{ok, Marking} = gen_yawl:marking(NetPid).

% Get receipts
Receipts = pnet_receipt:extract_receipts(Marking).
```

### Process Dictionary Inspection

```erlang
% Get process dictionary
erlang:get().
```

---

## Performance Issues

### Slow Workflow Execution

**Symptoms:**
- Workflow takes longer than expected
- Step latency increases over time

**Diagnosis:**

```erlang
% Profile workflow execution
{ok, Profile} = cre_profiler:profile_workflow(WorkflowPid, #{
    type => eprof,
    max_steps => 100
}).

% Check for bottlenecks
Bottlenecks = cre_debug_advanced:find_bottlenecks().
```

**Solutions:**

1. Enable mode caching:
```erlang
modes(Transition, _Marking, _UsrInfo) ->
    [{}].  % Single empty mode for simple transitions
```

2. Batch state updates:
```erlang
UpdateMap = #{p1 => [a], p2 => [b]},
NewMarking = pnet_marking:add(Marking, UpdateMap).
```

3. Clean up timers:
```erlang
TimerQ = wf_timerq:poll(TimerQ, CurrentTime),
TimerQ = wf_timerq:disarm(TimerQ, ExpiredKeys).
```

---

### High Memory Usage

**Symptoms:**
- Memory grows continuously
- GC runs frequently

**Diagnosis:**

```erlang
% Memory summary
Mem = cre_debug_advanced:memory_summary().

% Top memory processes
Top = cre_debug_advanced:top_memory_processes(10).

% Check for leaks
Leaks = cre_debug_advanced:memory_leak_check().
```

**Solutions:**

1. Use snapshots instead of full copies:
```erlang
Snapshot = pnet_marking:snapshot(Marking).
```

2. Clean up ETS tables:
```erlang
ets:delete(Table),
ets:delete_all_objects(Table).
```

3. Use binary references:
```erlang
% Instead of copying binaries
<<Part1:100/binary, Rest/binary>> = LargeBinary.
```

---

### Message Queue Backlog

**Symptoms:**
- Slow response times
- Processes with large message queues

**Diagnosis:**

```erlang
% Check queue length
{message_queue_len, Len} = erlang:process_info(Pid, message_queue_len).

% Find processes with large queues
recon:top(10, [message_queue_len]).
```

**Solutions:**

1. Process messages in batches:
```erlang
handle_info({'$gen_call', From, batch_request}, State) ->
    {reply, process_batch(Requests), State}.
```

2. Use selective receives:
```erlang
receive
    {high_priority, Msg} -> handle_high(Msg)
after 0 ->
    receive
        {normal_priority, Msg} -> handle_normal(Msg)
    end
end.
```

---

## Deadlock Diagnosis

### Symptoms
- Processes waiting indefinitely
- No progress in workflow
- All processes appear idle

### Detection

```erlang
% Detect potential deadlocks
Deadlocks = cre_debug_advanced:detect_deadlocks().

% Check for circular dependencies
case Deadlocks of
    [] -> io:format("No deadlocks detected~n");
    _ -> [io:format("Deadlock: ~p <-> ~p~n", [P1, P2]) || {P1, P2, _} <- Deadlocks]
end.
```

### Common Causes

1. **Synchronous call chain:** A calls B, B calls A
```erlang
% BAD: circular dependency
% A -> gen_server:call(B, ...)
% B -> gen_server:call(A, ...)

% GOOD: use cast or async
% A -> gen_server:cast(B, ...)
% B -> gen_server:cast(A, reply)
```

2. **Resource contention:** Multiple processes waiting for same resource
```erlang
% Use timeout to avoid infinite wait
gen_server:call(ResourcePid, Request, 5000).
```

3. **Mnesia transaction deadlock:**
```erlang
% Keep transactions short
mnesia:transaction(fun() ->
    mnesia:write(Record)
end).

% Access tables in consistent order
mnesia:transaction(fun() ->
    [mnesia:read({Table1, Key}) || Key <- Keys1],
    [mnesia:read({Table2, Key}) || Key <- Keys2]
end).
```

---

## Memory Issues

### Memory Leaks

**Diagnosis:**

```erlang
% Check for binary leaks
recon:bin_leak(100).

% Check for port leaks
recon:port_types().

% Memory by module
ModMem = cre_debug_advanced:memory_by_module().
```

**Solutions:**

1. Clear unused binaries:
```erlang
% Force GC
erlang:garbage_collect(Pid).
```

2. Close unused ports:
```erlang
port_close(Port).
```

3. Monitor ETS tables:
```erlang
ets:info(Table, size).
ets:info(Table, memory).
```

---

### Large Message Queues

**Diagnosis:**

```erlang
% Find processes with large queues
recon:top(10, [message_queue_len]).
```

**Solutions:**

1. Flush message queue:
```erlang
flush_mailbox(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, 0} -> ok;
        {message_queue_len, N} ->
            erlang:garbage_collect(Pid),
            timer:sleep(100),
            flush_mailbox(Pid)
    end.
```

2. Use async patterns:
```erlang
gen_server:cast(Pid, Message).
```

---

## Test Failures

### `already_started` Errors

**Symptoms:**
```
{error,{already_started,<0.123.0>}}
```

**Solutions:**

1. Add explicit cleanup:
```erlang
teardown(_) ->
    case whereis(process_name) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.
```

2. Use unique names:
```erlang
{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Or use via tuples
{ok, Pid} = gen_server:start_link({via, gproc, {n, l, {test, ?LINE}}}, ?MODULE, [], []).
```

---

### EUnit Process Dictionary Issues

**Symptoms:**
```
Test timeout waiting for message from handler
```

**Solutions:**

1. Capture test process pid:
```erlang
my_test(_) ->
    TestPid = self(),
    HandlerPid = spawn(fun() -> handler_loop(TestPid) end),
    receive
        {HandlerPid, Result} -> Result
    end.
```

2. Avoid creating handlers in setup:
```erlang
% Create handlers in test, not setup
```

---

### Cover `no_abstract_code` Warnings

**Symptoms:**
```
Warning: {no_abstract_code,"..._build/.../lib/...beam"}
```

**Causes:** Precompiled beam files lack debug information

**Solution:**

```erlang
% Compile with debug_info
{erl_opts, [debug_info, ...]}.
```

---

### Mnesia Schema Issues

**Symptoms:**
```
{error, {already_exists, node()}}
{aborted, {no_exists, Table}}
```

**Solutions:**

1. Check schema:
```erlang
mnesia:schema().
```

2. Delete and recreate (development):
```bash
rm -rf Mnesia.*
erl -s mnesia create_schema [node()] -s init stop
```

3. Use temporary tables for tests:
```erlang
mnesia:create_table(Tab, [{ram_copies, [node()]}, {type, set}]).
```

---

## Docker and Colima

### exec format error / input/output error

**Symptoms:**
```
exec /usr/local/bin/docker-entrypoint.sh: input/output error
exec /bin/sh: exec format error
```

**Causes:**
- **Wrong build target:** Use `--target runtime` if the Dockerfile has multiple stages.
- **Architecture mismatch:** Running amd64 image on Apple Silicon (arm64) without emulation.
- **Colima virtiofs:** Mount or I/O issues with default virtiofs on some macOS setups.

**Solutions:**

1. **Ensure correct build target (fixed in docker-bake.hcl):**
   ```bash
   # docker-bake.hcl now has target = "runtime" for cre target
   docker buildx bake --load arm64   # Apple Silicon
   docker buildx bake --load amd64   # Intel/AMD
   ```

2. **Plain docker build (explicit target):**
   ```bash
   docker build --platform linux/arm64 --target runtime -t cre:0.3.0 -f Dockerfile .
   ```

3. **Colima: switch to 9p mounts** (if virtiofs causes I/O errors):
   ```bash
   colima stop
   colima delete
   colima start --mount-type 9p
   ```

4. **Verify image architecture:**
   ```bash
   docker image inspect cre:0.3.0 --format '{{.Architecture}}'
   # Should show arm64 on Apple Silicon
   ```

5. **Test without volume mount** (isolate mount issues):
   ```bash
   docker run --rm cre:0.3.0 echo hello
   ```

---

## Getting Help

### Before Asking

1. **Search existing resources:**
   ```bash
   # Search GitHub issues
   gh issue list --search "keyword"

   # Search documentation
   grep -r "keyword" docs/
   ```

2. **Create minimal reproducible example:**
   - Isolate the failing code
   - Remove dependencies where possible
   - Test with latest CRE version

3. **Gather diagnostic information:**
   ```erlang
   % Version info
   erlang:system_info(otp_release).
   application:which_applications().

   % Process info
   erlang:process_info(Pid, [current_function, message_queue_len]).
   ```

---

### Information to Include

When asking for help, include:

1. **Version information:**
   ```bash
   erl -version
   grep "{vsn," src/cre.app.src
   ```

2. **Full error message and stack trace**

3. **Minimal reproduction code**

4. **Relevant configuration:**
   - `rebar.config`
   - Any sys.config files

5. **What you have already tried**

---

### Where to Ask

- **GitHub Issues:** https://github.com/seanchatmangpt/cre/issues
- **Documentation:** Check `docs/` directory for related guides
- **Source Code:** Look at test files for usage examples

---

## Common Error Reference

| Error | Cause | Quick Fix |
|-------|-------|-----------|
| `badmatch` | Pattern match failed | Check value types on LHS of match |
| `badarg` | Wrong argument type/arity | Verify function argument types |
| `noproc` | Process not found | Check process registration and alive status |
| `timeout` | Call timed out | Increase timeout or check for blocking operations |
| `case_clause` | No matching case clause | Add catch-all clause with proper error handling |
| `function_clause` | No matching function clause | Add missing pattern or check input types |
| `undef` | Function not defined | Ensure function is exported and module loaded |
| `badarith` | Bad arithmetic argument | Check numeric types before math operations |
| `badfun` | Bad function reference | Ensure fun syntax is correct |
| `system_limit` | Resource limit hit | Increase memory/process limits |
| `bad_record` | Record field missing | Verify record definition and include |
| `badarg` | Invalid argument | Check argument types and ranges |
| `bad_return` | Unexpected return value | Match on expected return values |
| `bad_return_from` | Callback returned unexpected value | Check callback return type |

---

## Quick Diagnostic Commands

```bash
# Compile and check warnings
rebar3 compile

# Run tests with verbose output
rebar3 eunit -v
rebar3 ct -v

# Check dialyzer warnings
rebar3 dialyzer

# Run xref for cross-reference checks
rebar3 xref

# Generate coverage report
rebar3 cover

# Check for common anti-patterns
rebar3 lint
```

---

## Related Documentation

- [Quick Reference Card](/Users/sac/cre/docs/QUICK_REFERENCE_CARD.md) - Essential commands and patterns
- [Debugging Guide](/Users/sac/cre/docs/operations/troubleshooting/debugging.md) - Advanced debugging techniques
- [Architecture Guide](/Users/sac/cre/docs/ARCHITECTURE.md) - System architecture
- [API Reference](/Users/sac/cre/docs/MINING_API_REFERENCE.md) - API documentation
- [Testing Guide](/Users/sac/cre/.claude/rules/testing.md) - Testing conventions

---

*Last Updated: 2026-02-09*
*For CRE version 0.2.1+*
