# Structured Tracing and Replay Logging Implementation Plan

## Implementation Plan Title
Extend `ln_trace` with Replay Support, OTP Logger Integration, and Reduction Step Instrumentation

## Overview
This implementation extends the existing `ln_trace` module to support:
1. **Deterministic replay** - Save and replay execution traces to verify exact behavior
2. **OTP logger integration** - Emit trace events to standard OTP logger for centralized logging
3. **Reduction step instrumentation** - Trace every Petri net reduction step via `gen_pnet:step/1`
4. **Performance-optimized tracing** - Zero-overhead when disabled, minimal overhead at 'min' level

The existing `ln_trace` module (`src/ln_trace.erl:1-138`) provides a solid foundation with:
- Configurable trace levels (`none`, `min`, `full`)
- Sequential event numbering for range queries
- Event format: `#{timestamp => integer(), type => event_type(), data => map(), seq => non_neg_integer()}`
- In-memory buffering with configurable max_events

We will extend this with persistence, replay verification, and integration points.

## Current State
The codebase has **multiple overlapping tracing systems**:
- **`ln_trace`** (138 lines) - Lightweight structured event tracing with levels and range queries
- **`yawl_telemetry`** (1475+ lines) - Full OpenTelemetry-style gen_server with spans, metrics, health checks
- **`yawl_logging`** (1168+ lines) - YAWL-specific audit logging with OpenXES export
- **`pnet_receipt`** - Immutable audit records with before/after hashes for state transitions

**What's Missing:**
- No trace persistence (save/load to disk)
- No replay functionality (execute and verify exact trace reproduction)
- No OTP logger integration (traces only go to in-memory buffer)
- No instrumentation of reduction steps in `gen_pnet:step/1`
- No trace comparison utilities for debugging

**Key Constraints:**
- Minimal performance impact at 'min' level (already enforced via level check at `ln_trace:82-83`)
- Must support trace queries (range retrieval already implemented at `ln_trace:96-102`)
- Structured event format (already using maps at `ln_trace:38-42`)
- Integration with OTP logging (logger.hrl already used in 50+ modules)

## Desired End State

### Core Functionality
1. **Trace Persistence**: Save traces to file (JSON format) and load for replay
2. **Replay Execution**: Execute a workflow and compare against saved trace
3. **OTP Logger Integration**: All trace events emitted to OTP logger with appropriate metadata
4. **Reduction Step Tracing**: Every call to `gen_pnet:step/1` emits a trace event
5. **Trace Verification**: Compare two traces for exact match or identify differences

### Verification
- **Automated**: Unit tests pass for trace save/load, replay verification, and instrumentation
- **Manual**: Execute workflow, save trace, replay and verify exact match
- **Performance**: Benchmarks show <1% overhead at 'min' level, zero overhead at 'none' level

### Key Discoveries:
- **`ln_trace` is functional API** - State passed in/out, not a gen_server (verified at `ln_trace:78-81`)
- **Receipt format supports deterministic replay** - Hash-based verification at `pnet_receipt:77-80`
- **Multiple export formats already exist** - JSON, list, map at `ln_trace:105-112`
- **OTP logger already integrated** - Used in 50+ modules via `-include_lib("kernel/include/logger.hrl")`
- **`gen_pnet:step/1` is the reduction point** - Executes single transition at `gen_pnet:675-681`
- **Multiple telemetry systems exist** - Need to avoid creating yet another system

## What We're NOT Doing
- ❌ Replacing `yawl_telemetry` or `yawl_logging` - creating adapters instead
- ❌ Implementing full OpenTelemetry distributed tracing - already exists in `yawl_telemetry`
- ❌ Creating a gen_server for tracing - keeping `ln_trace` as functional API
- ❌ Implementing real-time trace streaming - out of scope, can be added later
- ❌ Building a trace visualization UI - out of scope, use existing DOT export from `yawl_telemetry`
- ❌ Implementing trace compression - out of scope, can be added later
- ❌ Building a trace query language - range queries sufficient for now
- ❌ Implementing distributed replay - single-node replay only
- ❌ Changing the event format - extending existing format only
- ❌ Modifying the receipt format - using existing `pnet_receipt` as-is

## Implementation Approach

### High-Level Strategy
1. **Extend `ln_trace` with persistence and OTP logger integration** - Keep it as a functional API
2. **Create `ln_trace_replay` module** - Replay execution and verification
3. **Create `ln_trace_adapter` module** - Adapters for `yawl_telemetry` and `yawl_logging`
4. **Instrument `gen_pnet:step/1`** - Emit trace events for each reduction step
5. **Instrument `wf_engine` lifecycle** - Emit trace events for case start/complete/fail
6. **Add comprehensive tests** - Unit tests for each module, integration tests for replay

### Design Principles
- **Zero overhead when disabled** - Use compile-time macros for 'none' level
- **Minimal overhead at 'min' level** - Only capture essential events (case lifecycle, errors)
- **Full tracing when enabled** - Capture all reduction steps and intermediate states
- **Backwards compatible** - Existing code continues to work without changes
- **Testable** - Each module independently testable
- **Observable** - All trace events visible via OTP logger

---

## Phases

### Phase 1: Extend `ln_trace` with Persistence and OTP Logger Integration

#### Overview
Add trace persistence (save/load to file) and OTP logger integration to `ln_trace` module.

#### Changes Required:

##### 1. Extend `src/ln_trace.erl`
**File**: `src/ln_trace.erl`
**Changes**: Add OTP logger integration and persistence API

**Add exports:**
```erlang
-export([save/2, load/1]).
-export([set_logger_level/1]).
```

**Add OTP logger integration in emit/2:**
```erlang
emit(#{
    timestamp := _} = Event, #trace_state{events = Events, max_events = Max, seq = Seq} = State) ->
    NewEvents = [Event#{seq => Seq} | Events],
    Trimmed = trim_events(NewEvents, Max),
    %% Log to OTP logger
    logger:info("Trace event ~p: ~p", [Seq, Event]),
    State#trace_state{events = Trimmed, seq = Seq + 1};
```

**Add save/2 function:**
```erlang
%% @doc Save trace to file in JSON format.
-spec save(state(), file:name()) -> ok | {error, term()}.
save(#trace_state{events = Events}, Filename) ->
    JSON = jsx:encode(lists:reverse(Events)),
    file:write_file(Filename, JSON).
```

**Add load/1 function:**
```erlang
%% @doc Load trace from file.
-spec load(file:name()) -> {ok, state()} | {error, term()}.
load(Filename) ->
    case file:read_file(Filename) of
        {ok, JSON} ->
            Events = jsx:decode(JSON, [return_maps]),
            {ok, #trace_state{
                events = Events,
                max_events = infinity,
                level = full,
                seq = length(Events)
            }};
        {error, Reason} ->
            {error, Reason}
    end.
```

**Add logger level control:**
```erlang
%% @doc Set the logger level for trace events.
-spec set_logger_level(logger:level()) -> ok.
set_logger_level(Level) ->
    logger:set_application_level(cre, Level).
```

#### Success Criteria:

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_trace`
- [ ] Type checking passes: `dialyzer -r ebin`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Create trace buffer, emit events, save to file
- [ ] Load trace from file, verify events match
- [ ] Check OTP logger contains trace events
- [ ] Verify no regressions in existing ln_trace functionality

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Create `ln_trace_replay` Module

#### Overview
Create a new module for replay execution and trace verification.

#### Changes Required:

##### 1. Create new file `src/ln_trace_replay.erl`
**File**: `src/ln_trace_reerl` (NEW)
**Changes**: New module for replay execution

**Module header:**
```erlang
%%%-------------------------------------------------------------------
%%% @doc ln_trace_replay - Trace replay and verification.
%%%
%%% Provides functionality to replay workflow execution and verify
%%% that the trace matches exactly.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_trace_replay).

%% API
-export([replay/3]).
-export([verify/2]).
-export([diff/2]).

%% Types
-export_type([replay_result/0, diff_result/0]).
```

**Type definitions:**
```erlang
-type replay_result() :: {ok, state()} | {error, mismatch, diff_result()}.
-type diff_result() :: #{missing := [event()], extra := [event()], different := [event()]}.
```

**Replay function:**
```erlang
%% @doc Replay workflow execution and verify trace matches.
-spec replay(module(), atom(), [term()], state()) -> replay_result().
replay(Module, Function, Args, ExpectedTrace) ->
    %% Create new trace buffer
    TraceState = ln_trace:new(#{level => full, max_events => infinity}),

    %% Execute function with tracing enabled
    %% (Implementation depends on how we enable tracing for execution)
    Result = apply(Module, Function, Args),

    %% Compare traces
    case verify(TraceState, ExpectedTrace) of
        ok ->
            {ok, TraceState};
        {error, Diff} ->
            {error, mismatch, Diff}
    end.
```

**Verify function:**
```erlang
%% @doc Verify two traces match exactly.
-spec verify(state(), state()) -> ok | {error, diff_result()}.
verify(#trace_state{events = Events1}, #trace_state{events = Events2}) ->
    case Events1 =:= Events2 of
        true ->
            ok;
        false ->
            {error, diff(Events1, Events2)}
    end.
```

**Diff function:**
```erlang
%% @doc Compute differences between two traces.
-spec diff(state(), state()) -> diff_result().
diff(#trace_state{events = Events1}, #trace_state{events = Events2}) ->
    %% Compute missing, extra, and different events
    %% (Implementation details)
    #{
        missing => get_missing(Events1, Events2),
        extra => get_extra(Events1, Events2),
        different => get_different(Events1, Events2)
    }.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_trace_replay`
- [ ] Type checking passes: `dialyzer -r ebin`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Execute workflow, save trace
- [ ] Replay same workflow, verify traces match
- [ ] Modify workflow, replay, verify diff is detected
- [ ] Test with missing events, extra events, different data

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Create `ln_trace_adapter` Module

#### Overview
Create adapters for integrating `ln_trace` with existing telemetry systems.

#### Changes Required:

##### 1. Create new file `src/ln_trace_adapter.erl`
**File**: `src/ln_trace_adapter.erl` (NEW)
**Changes**: New module for telemetry adapters

**Module header:**
```erlang
%%%-------------------------------------------------------------------
%%% @doc ln_trace_adapter - Adapters for telemetry integration.
%%%
%%% Provides adapters to convert ln_trace events to formats used
%%% by yawl_telemetry and yawl_logging.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_trace_adapter).

%% API
-export([to_telemetry_span/1]).
-export([to_yawl_log/1]).
-export([export_to_telemetry/2]).
-export([export_to_yawl_logging/2]).
```

**Adapter functions:**
```erlang
%% @doc Convert ln_trace event to yawl_telemetry span format.
-spec to_telemetry_span(ln_trace:event()) -> map().
to_telemetry_span(#{timestamp := TS, type := Type, seq := Seq, data := Data}) ->
    #{
        trace_id => generate_trace_id(),
        span_id => generate_span_id(Seq),
        parent_id => undefined,
        name => atom_to_list(Type),
        start_time => TS,
        end_time => TS,
        attributes => Data
    }.

%% @doc Convert ln_trace event to yawl_logging format.
-spec to_yawl_log(ln_trace:event()) -> yawl_logging:log_entry().
to_yawl_log(#{timestamp := TS, type := Type, seq := Seq, data := Data}) ->
    #{
        id => integer_to_binary(Seq),
        timestamp => TS,
        level => info,
        type => Type,
        case_id => maps:get(case_id, Data, undefined),
        workitem_id => maps:get(workitem_id, Data, undefined),
        message => format_message(Type, Data),
        data => Data
    }.

%% @doc Export ln_trace to yawl_telemetry.
-spec export_to_telemetry(state(), pid()) -> ok.
export_to_telemetry(#trace_state{events = Events}, TelemetryPid) ->
    lists:foreach(fun(Event) ->
        Span = to_telemetry_span(Event),
        gen_server:cast(TelemetryPid, {record_span, Span})
    end, Events),
    ok.

%% @doc Export ln_trace to yawl_logging.
-spec export_to_yawl_logging(state(), pid()) -> ok.
export_to_yawl_logging(#trace_state{events = Events}, LoggingPid) ->
    lists:foreach(fun(Event) ->
        LogEntry = to_yawl_log(Event),
        gen_server:cast(LoggingPid, {log, LogEntry})
    end, Events),
    ok.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_trace_adapter`
- [ ] Type checking passes: `dialyzer -r ebin`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Create trace, export to yawl_telemetry, verify spans created
- [ ] Create trace, export to yawl_logging, verify log entries created
- [ ] Verify data preservation through conversion
- [ ] Test with all event types

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Instrument `gen_pnet:step/1`

#### Overview
Add trace event emission to `gen_pnet:step/1` for each reduction step.

#### Changes Required:

##### 1. Modify `src/core/gen_pnet.erl`
**File**: `src/core/gen_pnet.erl`
**Changes**: Add trace event emission in handle_call for step

**Add include:**
```erlang
-include_lib("kernel/include/logger.hrl").
```

**Modify handle_call for step (around line 675):**
```erlang
handle_call(step, _From, NetState) ->
    %% Emit trace event before step
    ?LOG_INFO("Reduction step starting", []),
    case fire_transition(NetState) of
        abort ->
            %% Emit trace event for abort
            ?LOG_INFO("Reduction step aborted", []),
            {reply, abort, NetState};
        {ok, Receipt, NetState1} ->
            %% Emit trace event for successful step
            #{before_hash := Before, after_hash := After, move := Move} = Receipt,
            ?LOG_INFO("Reduction step completed", #{
                before_hash => base64:encode(Before),
                after_hash => base64:encode(After),
                transition => maps:get(trsn, Move)
            }),
            {reply, {ok, Receipt}, NetState1}
    end;
```

**Note**: This uses OTP logger directly. Integration with `ln_trace` would require passing trace state through gen_pnet, which is a larger refactoring. The OTP logger integration ensures observability while avoiding architectural changes.

#### Success Criteria:

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=gen_pnet`
- [ ] Type checking passes: `dialyzer -r ebin`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Execute workflow, verify reduction steps appear in logs
- [ ] Check that abort cases are logged
- [ ] Verify hash values are logged correctly
- [ ] Confirm no performance regression in workflow execution

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Instrument `wf_engine` Lifecycle

#### Overview
Add trace event emission to `wf_engine` for case lifecycle events.

#### Changes Required:

##### 1. Modify `src/wf/wf_engine.erl`
**File**: `src/wf/wf_engine.erl`
**Changes**: Add trace event emission for case lifecycle

**Add trace event emissions in key functions:**

**In start_case/3 (around line 200-250):**
```erlang
start_case(Engine, Data, Timestamp) ->
    gen_server:call(Engine, {start_case, Data, Timestamp}).

%% In handle_call:
handle_call({start_case, Data, Timestamp}, _From, State) ->
    CaseId = generate_case_id(),
    ?LOG_INFO("Case started", #{case_id => CaseId, data => Data, timestamp => Timestamp}),
    %% ... existing implementation ...
    {reply, {ok, CaseId}, NewState}.
```

**In complete/5 (around line 300-350):**
```erlang
complete(Engine, WiId, User, Result, Timestamp) ->
    gen_server:call(Engine, {complete, WiId, User, Result, Timestamp}).

%% In handle_call:
handle_call({complete, WiId, User, Result, Timestamp}, _From, State) ->
    ?LOG_INFO("Work item completed", #{
        wi_id => WiId,
        user => User,
        result => Result,
        timestamp => Timestamp
    }),
    %% ... existing implementation ...
    {reply, ok, NewState}.
```

**In case failure/cancellation paths:**
```erlang
?LOG_INFO("Case failed", #{case_id => CaseId, reason => Reason}).
?LOG_INFO("Case cancelled", #{case_id => CaseId, reason => Reason}).
```

#### Success Criteria:

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=wf_engine`
- [ ] Type checking passes: `dialyzer -r ebin`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Start workflow case, verify case_started event in logs
- [ ] Complete work item, verify workitem_completed event in logs
- [ ] Fail/cancel case, verify failure/cancellation events in logs
- [ ] Verify case_id and other metadata are logged correctly

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 6: Add Configuration for Trace Level

#### Overview
Add application configuration for trace level control.

#### Changes Required:

##### 1. Modify `src/cre.app.src`
**File**: `src/cre.app.src`
**Changes**: Add trace_level to env

**Modify env section:**
```erlang
{env, [
  {trace_level, none},  %% none | min | full
  {trace_max_events, 10000}
]},
```

##### 2. Modify `src/ln_trace.erl`
**File**: `src/ln_trace.erl`
**Changes**: Add function to get application config

**Add function:**
```erlang
%% @doc Create a new trace buffer from application config.
-spec from_config() -> state().
from_config() ->
    Level = application:get_env(cre, trace_level, full),
    MaxEvents = application:get_env(cre, trace_max_events, infinity),
    new(#{level => Level, max_events => MaxEvents}).
```

#### Success Criteria:

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Set trace_level to none, verify no trace events
- [ ] Set trace_level to min, verify minimal events
- [ ] Set trace_level to full, verify all events
- [ ] Modify trace_max_events, verify buffer trimming works

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

## Testing Strategy

### Unit Tests:
- **`ln_trace`**: Test save/load, OTP logger integration, level filtering
- **`ln_trace_replay`**: Test replay execution, trace verification, diff computation
- **`ln_trace_adapter`**: Test conversion to telemetry and logging formats
- **`gen_pnet`**: Test that step logging works correctly
- **`wf_engine`**: Test that lifecycle events are logged

### Integration Tests:
- Execute workflow, save trace, replay and verify exact match
- Export trace to yawl_telemetry, verify spans created
- Export trace to yawl_logging, verify log entries created
- Test with different trace levels (none, min, full)
- Test trace buffer trimming with max_events limit

### Performance Tests:
- Benchmark overhead at 'none' level (should be ~0%)
- Benchmark overhead at 'min' level (target <1%)
- Benchmark overhead at 'full' level (document impact)
- Memory usage with max_events limit

### Manual Testing Steps:
1. Start workflow engine with trace_level = full
2. Execute a sample workflow
3. Save trace to file
4. Replay workflow and verify exact match
5. Change trace_level to min, repeat
6. Change trace_level to none, verify no traces
7. Export trace to yawl_telemetry, verify in telemetry UI
8. Export trace to yawl_logging, verify in logs

## Migration Notes

### For Existing Code:
- **No changes required** - tracing is additive only
- Existing workflows continue to work without modification
- Tracing is disabled by default (trace_level = none)

### For New Code:
- Use `ln_trace:from_config/0` to create trace buffers from config
- Use `?LOG_INFO`, `?LOG_ERROR` macros for structured logging
- Use `ln_trace_adapter` to export traces to other telemetry systems

### Configuration:
- Add `trace_level` and `trace_max_events` to sys.config
- Default: `trace_level = none` (no overhead)
- Production: `trace_level = min` (minimal overhead)
- Debug: `trace_level = full` (full observability)

## References
- Research: `/Users/sac/cre/.wreckit/items/017-structured-tracing-and-replay-logging/research.md`
- Core tracing: `src/ln_trace.erl:1-138`
- Petri net engine: `src/core/gen_pnet.erl:675-681` (step function)
- Receipt format: `src/pnet/pnet_receipt.erl:77-80`
- Workflow engine: `src/wf/wf_engine.erl:122-128` (receipt type)
- Telemetry: `src/yawl_telemetry.erl:1-150` (span management)
- Logging: `src/yawl_logging.erl:1-150` (YAWL audit logging)
- OTP logger: Used in 50+ modules via `-include_lib("kernel/include/logger.hrl")`
