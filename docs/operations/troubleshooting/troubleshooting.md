# CRE YAWL - Troubleshooting Guide

**Comprehensive troubleshooting documentation for the CRE YAWL workflow engine.**

This guide covers common issues encountered during development, testing, and production use of CRE. Each issue includes symptoms, diagnosis steps, and resolution strategies.

---

## Table of Contents

1. [Compilation Errors](#compilation-errors)
2. [Test Failures](#test-failures)
3. [Mnesia Schema Issues](#mnesia-schema-issues)
4. [gen_pnet Callback Problems](#gen_pnet-callback-problems)
5. [Pattern Execution Issues](#pattern-execution-issues)
6. [State Recovery Problems](#state-recovery-problems)
7. [Debugging Techniques](#debugging-techniques)
8. [Log Analysis](#log-analysis)
9. [When to Ask for Help](#when-to-ask-for-help)

---

## Compilation Errors

### `undefined type` or `undefined record` Errors

**Symptoms:**
```
src/my_module.erl:42: type net_element() is undefined
src/my_module.erl:55: record pattern_state is undefined
```

**Diagnosis:**
- Missing include directive for header files
- Header file not in the include path
- Circular dependency between modules

**Resolution:**
1. Check for missing `-include()` or `-include_lib()` directives:
   ```erlang
   -include("gen_pnet.hrl").
   -include("cre_yawl.hrl").
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

### OTP Version Compatibility Issues

**Symptoms:**
```
unbound type variable error in cowlib
function_clause in logger module
deprecated function erlang:get_stacktrace/0
```

**Diagnosis:**
- Using OTP version below 25.0 (minimum supported)
- Cowboy/cowlib version mismatch with OTP version

**Resolution:**
1. Check your OTP version:
   ```bash
   erl -version
   ```

2. Ensure `rebar.config` has the correct overrides for OTP 28:
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

3. Clean and rebuild:
   ```bash
   rm -rf _build/
   rebar3 compile
   ```

---

### `unused variable` Warnings

**Symptoms:**
```
Warning: variable 'From' is unused
Warning: variable 'Ref' is shadowed
```

**Diagnosis:**
- Variables declared but not used in the function body
- Pattern matching capturing unused values

**Resolution:**
1. Prefix unused variables with underscore:
   ```erlang
   % Before:
   handle_info(Msg, State) ->
       {noreply, State}.

   % After:
   handle_info(_Msg, State) ->
       {noreply, State}.
   ```

2. Use `_` for truly ignored values:
   ```erlang
   {ok, _Pid} = start_worker().
   ```

---

## Test Failures

### `already_started` Errors

**Symptoms:**
```
yawl_auth_test:...failure...{error,{already_started,_}}
```

**Diagnosis:**
- Test isolation issue when starting gen_server processes
- Previous test left a process running

**Resolution:**
1. Add explicit cleanup in test teardown:
   ```erlang
   teardown(_) ->
       case whereis(yawl_auth) of
           undefined -> ok;
           Pid -> gen_server:stop(Pid)
       end.
   ```

2. Use `meck` to mock dependencies that have single-process constraints.

3. Check for process registration conflicts:
   ```erlang
   ?assertEqual(undefined, whereis(process_name)).
   ```

---

### EUnit Process Dictionary Issues

**Symptoms:**
```
Test timeout waiting for message from handler
{timeout_error, ...}
```

**Diagnosis:**
- Handler processes created during test setup use wrong process reference
- Messages sent to wrong process due to `self()` being called in setup context

**Resolution:**
1. Always capture `self()` at the start of the test:
   ```erlang
   my_test(_) ->
       TestPid = self(),  % Capture test process pid
       HandlerPid = spawn(fun() -> handler_loop(TestPid) end),
       receive
           {HandlerPid, Result} -> Result
       end.
   ```

2. Avoid creating handlers in `setup()` functions - create them in the test itself.

---

### Integration Test Pattern Mismatches

**Symptoms:**
```
Failure/Error: {error,{badmatch,{workflow,...}}}
in function yawl_integration_test:set_workflow_boundaries/3
```

**Diagnosis:**
- Workflow parsing expects specific record structure
- Missing API exports for workflow introspection

**Resolution:**
1. Verify required API exports exist:
   ```erlang
   -export([get_tasks/1, set_workflow_boundaries/3]).
   ```

2. Check workflow record structure:
   ```erlang
   -record(workflow, {
       id,
       tasks,
       connections,
       data
   }).
   ```

3. Run tests individually to isolate the failing module:
   ```bash
   rebar3 eunit --module=yawl_integration_test
   ```

---

### Cover `no_abstract_code` Warnings

**Symptoms:**
```
Warning: {no_abstract_code,"..._build/.../lib/...beam"}
```

**Diagnosis:**
- Precompiled beam files lack debug information
- Cover tool cannot analyze modules without abstract code

**Resolution:**
1. This is a known Rebar3/Erlang toolchain limitation
2. Coverage reports will show 0% for affected modules
3. To fix, compile with `debug_info`:
   ```erlang
   {erl_opts, [debug_info, ...]}.
   ```
4. For dependencies, compile from source instead of using hex packages.

---

## Mnesia Schema Issues

### Schema Creation Failures

**Symptoms:**
```
{error, {already_exists, node()}}
{aborted, {no_exists, Table}}
```

**Diagnosis:**
- Mnesia schema already exists from previous run
- Running multiple nodes with conflicting schemas

**Resolution:**
1. Check if schema exists:
   ```erlang
   mnesia:schema().
   ```

2. Delete and recreate schema (development only):
   ```bash
   rm -rf Mnesia.*
   erl -s mnesia create_schema [node()] -s init stop
   ```

3. For production, use `mnesia:change_table_copy_type/3`:
   ```erlang
   mnesia:change_table_copy_type(Tab, node(), disc_copies).
   ```

---

### Table Access Errors

**Symptoms:**
```
{aborted, {no_exists, persistent_case}}
{aborted, {bad_type, persistent_case, disc_copies}}
```

**Diagnosis:**
- Table not initialized
- Wrong table type (ram_copies vs disc_copies)

**Resolution:**
1. Initialize schema before running tests:
   ```erlang
   init_per_testcase(_) ->
       {ok, _} = yawl_persistence:init_schema(),
       ok.
   ```

2. Check table info:
   ```erlang
   mnesia:table_info(Tab, where_to_read).
   mnesia:table_info(Tab, storage_type).
   ```

3. Use temporary in-memory tables for tests:
   ```erlang
   mnesia:create_table(Tab, [{ram_copies, [node()]}, {type, set}]).
   ```

---

### Transaction Deadlocks

**Symptoms:**
```
{aborted, {deadlock, [...]}}
{atomic, ...} never returns
```

**Diagnosis:**
- Multiple transactions waiting on each other
- Nested transactions with conflicting locks

**Resolution:**
1. Keep transactions short:
   ```erlang
   %% BAD - long transaction
   mnesia:transaction(fun() ->
       %% expensive computation here
       Result = long_running_function(),
       mnesia:write(Record)
   end).

   %% GOOD - short transaction
   Result = long_running_function(),
   mnesia:transaction(fun() ->
       mnesia:write(Record)
   end).
   ```

2. Use `mnesia:async_dirty/2` for non-critical writes.

3. Order table access consistently across all transactions.

---

## gen_pnet Callback Problems

### Missing Required Callbacks

**Symptoms:**
```
Warning: undefined callback function place_lst/0
Warning: undefined callback function trigger/3
```

**Diagnosis:**
- gen_pnet behavior requires 13 callbacks
- Missing structure or interface callbacks

**Resolution:**
1. Ensure all 13 gen_pnet callbacks are implemented:

   **Structure Callbacks (6):**
   - `place_lst/0` - List of all places
   - `trsn_lst/0` - List of all transitions
   - `init_marking/2` - Initial tokens for each place
   - `preset/1` - Input places for each transition
   - `is_enabled/3` - Enable condition for transitions
   - `fire/3` - Tokens produced when transition fires

   **Interface Callbacks (7):**
   - `init/1` - Initialize net instance
   - `handle_call/3` - Synchronous calls
   - `handle_cast/2` - Asynchronous casts
   - `handle_info/2` - Unformatted messages
   - `code_change/3` - Hot code reload
   - `terminate/2` - Cleanup
   - `trigger/3` - Side effects when tokens produced

2. Use `-compile({nowarn_unused_function, ...})` for intentionally unused callbacks.

---

### Trigger Callback Returning Wrong Type

**Symptoms:**
```
{badmatch, {error, {bad_trigger, ...}}}
```

**Diagnosis:**
- `trigger/3` must return `{ok, NetState}` or other accepted values
- Returning wrong structure causes gen_pnet to fail

**Resolution:**
1. Always return a valid net state:
   ```erlang
   trigger(_Place, _Token, NetState) ->
       %% Side effects here
       {ok, NetState}.
   ```

2. For filtering tokens, return `{discard, NetState}`:
   ```erlang
   trigger(Place, Token, NetState) ->
       case validate_token(Token) of
           true -> {ok, NetState};
           false -> {discard, NetState}
       end.
   ```

---

### Infinite Transition Loops

**Symptoms:**
```
Process never terminates
100% CPU usage
gen_pnet stats show massive fire counts
```

**Diagnosis:**
- Transition always enabled and produces tokens that keep it enabled
- Missing guard condition in `is_enabled/3`

**Resolution:**
1. Add termination condition to `is_enabled/3`:
   ```erlang
   is_enabled(Trsn, Mode, _UsrInfo) ->
       Preset = preset(Trsn),
       HasTokens = has_all_tokens(Preset, Mode),
       IsComplete = check_completion(Mode),
       HasTokens andalso IsComplete.
   ```

2. Use `gen_pnet:stats/1` to check fire counts:
   ```erlang
   {ok, Stats} = gen_pnet:stats(NetPid),
   io:format("Fires: ~p~n", [Stats#stats.current#stat.fps]).
   ```

3. Set a maximum iteration count for testing.

---

## Pattern Execution Issues

### Workflow State Stuck in `running`

**Symptoms:**
```
Workflow status shows "running" indefinitely
No tokens moving through places
```

**Diagnosis:**
- No enabled transitions in current marking
- Missing connection between places
- Predicate condition never satisfied

**Resolution:**
1. Check current marking:
   ```erlang
   {ok, Marking} = gen_pnet:marking(NetPid),
   io:format("Marking: ~p~n", [Marking]).
   ```

2. Verify all transitions are reachable:
   ```erlang
   Preset = Module:preset(TransitionName),
   %% Ensure all places in Preset have tokens
   ```

3. Check predicate conditions:
   ```erlang
   %% In is_enabled/3:
   is_enabled(Trsn, Mode, _UsrInfo) ->
       case maps:get(Trsn, Mode, []) of
           [] -> false;  % No tokens, not enabled
           _ -> true
       end.
   ```

---

### Choice Pattern Not Selecting

**Symptoms:**
```
Exclusive choice never fires
Deferred choice stuck waiting
```

**Diagnosis:**
- Condition function returns `false` for all options
- Condition function not properly evaluating predicates

**Resolution:**
1. Verify condition function covers all cases:
   ```erlang
   ConditionFun = fun
       (#{data := #{status := <<"approved">>}}) -> true;
       (#{data := #{status := <<"rejected">>}}) -> true;
       (_) -> false  % Default case
   end.
   ```

2. Check data is available to the condition:
   ```erlang
   %% In fire/3, log the input
   fire(Trsn, Mode, UsrInfo) ->
       logger:debug("Firing ~p with mode ~p", [Trsn, Mode]),
       %% ...
   ```

---

### Synchronization Never Completes

**Symptoms:**
```
OR join waiting for tokens that never arrive
N-out-of-M threshold never reached
```

**Diagnosis:**
- Wrong threshold value
- Missing tokens from a parallel branch
- Dead branch not producing expected tokens

**Resolution:**
1. Check threshold configuration:
   ```erlang
   %% For N-out-of-M:
   is_enabled(Trsn, Mode, _UsrInfo) ->
       Threshold = 2,  % Minimum tokens needed
       Preset = preset(Trsn),
       TokenCount = count_tokens(Preset, Mode),
       TokenCount >= Threshold.
   ```

2. Use timeout for synchronization:
   ```erlang
   %% Add timeout transition
   {timeout_sync, 5000}  % 5 second timeout
   ```

3. Verify all parallel branches produce tokens:
   ```erlang
   %% In each branch's fire clause, ensure output tokens
   fire(BranchTrsn, _Mode, _UsrInfo) ->
       #{sync_place => [completion_token]}.
   ```

---

## State Recovery Problems

### Checkpoint Restore Failures

**Symptoms:**
```
{error, {checkpoint_corrupted, ...}}
{error, {version_mismatch, ...}}
```

**Diagnosis:**
- Checkpoint format changed between versions
- Checkpoint file corrupted
- Incompatible state structure

**Resolution:**
1. Version your checkpoint format:
   ```erlang
   -record(checkpoint, {
       version = 1,
       timestamp,
       state
   }).
   ```

2. Add migration logic for old checkpoints:
   ```erlang
   load_checkpoint(File) ->
       case file:read_file(File) of
           {ok, Binary} ->
               CP = binary_to_term(Binary),
               migrate_checkpoint(CP);
           {error, Reason} ->
               {error, Reason}
       end.

   migrate_checkpoint(#checkpoint{version = 1, state = State}) ->
       %% Convert v1 to v2 format
       State2 = upgrade_state(State),
       #checkpoint{version = 2, state = State2}.
   ```

3. Validate checkpoint before use:
   ```erlang
   validate_checkpoint(#checkpoint{state = State}) ->
       case validate_state_structure(State) of
           ok -> {ok, State};
           Error -> Error
       end.
   ```

---

### Worker Registration After Restart

**Symptoms:**
```
{error, {worker_not_found, WorkerId}}
Tasks not scheduled after node restart
```

**Diagnosis:**
- Workers not re-registering after restart
- Worker registry not persisted

**Resolution:**
1. Implement worker auto-registration:
   ```erlang
   init([_]) ->
       {ok, Pid} = SomeWorker:start_link(),
       ok = cre_worker:register(Pid, self()),
       {ok, #{worker_pid => Pid}}.

   handle_info({register_worker, WorkerPid}, State) ->
       ok = cre_worker:register(WorkerPid, self()),
       {noreply, State}.
   ```

2. Persist worker registry:
   ```erlang
   save_worker_registry() ->
       Workers = cre_worker:list(),
       mnesia:transaction(fun() ->
           [mnesia:write(#worker_registry{id=Id, pid=Pid})
            || {Id, Pid} <- Workers]
       end).
   ```

---

## Debugging Techniques

### Enabling Debug Output

**Add to your module:**
```erlang
-compile([debug_info]).
-define(DEBUG(Format, Args),
    logger:debug("[~p:~p] " ++ Format,
        [?MODULE, ?LINE | Args])).

my_function(Arg) ->
    ?DEBUG("Called with: ~p", [Arg]),
    %% ...
```

**Enable tracing:**
```bash
# Start with dbg
erl -eval "dbg:tracer(), dbg:p(all, c), dbg:tpl(yawl_engine, cx)."

# Or use recon_trace
recon_trace:calls({yawl_engine, '_', '_'}, 10).
```

---

### Inspecting gen_pnet State

```erlang
%% Get current marking
{ok, Marking} = gen_pnet:marking(NetPid).

%% Check statistics
{ok, Stats} = gen_pnet:stats(NetPid).

%% Query specific place
case gen_pnet:ls(NetPid, [place_name]) of
    {ok, [{place_name, Tokens}]} ->
        io:format("Tokens: ~p~n", [Tokens]);
    {error, #bad_place{name = Place}} ->
        io:format("Place ~p does not exist~n", [Place])
end.
```

---

### Using the Observer

```bash
# Start observer with visual tools
erl -s observer start

# Or start from running node
erl -name observer@localhost -setcookie mycookie -remsh target@hostname
# Then run: observer:start().
```

Key things to check:
- Process message queue length
- Process heap size
- Registered processes
- ETS tables

---

### Crash Dump Analysis

When a process crashes, Erlang can write an erl_crash.dump file:

```bash
# Analyze with webtool
erl -eval "webtool:start()" -s crashdump_viewer view_file "erl_crash.dump"
```

Look for:
- `exit_reason` - Why the process crashed
- `message_queue_len` - Was it overloaded?
- `last_calls` - Stack trace at crash
- `dictionary` - Process dictionary state

---

## Log Analysis

### Structured Logging with Logger

```erlang
%% Configure metadata
logger:set_primary_config(level, all).
logger:set_handler_config(default, formatter,
    {logger_formatter, #{
        template => [time, " ", level, " [",
            {module, [{module, undefined, "???"}], ":", line, "] ",
            message, "\n"]
    }}).

%% Add metadata for filtering
logger:info("Workflow started",
    #{workflow_id => Id, case_id => CaseId}).

%% Filter logs
logger:filter_module(my_filter, fun
    (#{level := Level, meta := #{workflow_id := Id}}) when Id =:= <<"debug_wf">> ->
        Level =:= debug;
    (_) -> true
end).
```

---

### OpenTelemetry Trace Analysis

```erlang
%% Start a span
otel_telemetry:start_span(<<"workflow.execute">>, #{
    workflow_id => Id,
    case_id => CaseId
}).

%% Add events to span
otel_telemetry:add_event(<<"task.completed">>, #{
    task_id => TaskId,
    duration => DurationMs
}).

%% End span
otel_telemetry:end_span().
```

View traces in Jaeger or compatible backend.

---

### XES Event Log Analysis

XES logs capture workflow execution for process mining:

```erlang
%% Export XES log
yawl_xes:export_log(LogId, "workflow_trace.xes").

%% Analyze with ProM or other process mining tools
```

---

## When to Ask for Help

### Before Asking

1. **Search existing issues:**
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
   %% Version info
   erlang:system_info(otp_release).
   application:which_applications().
   cre:version().

   %% Process info
   erlang:process_info(Pid, [current_function, message_queue_len]).
   ```

---

### Information to Include

When asking for help, include:

1. **CRE version and OTP version:**
   ```bash
   erl -version
   grep "{vsn," src/cre.app.src
   ```

2. **Full error message and stack trace**

3. **Minimal reproduction code**

4. **Relevant configuration:**
   - `rebar.config`
   - Any sys.config files

5. **What you've already tried**

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

---

## Quick Diagnostic Commands

```bash
# Compile and check warnings
rebar3 compile

# Run tests with verbose output
rebar3 eunit -v

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

*Last Updated: 2026-02-06*
*For CRE version 0.2.1+*
