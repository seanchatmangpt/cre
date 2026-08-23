# Research: Structured tracing and replay logging

**Date**: 2025-01-11
**Item**: 017-structured-tracing-and-replay-logging

## Research Question

Debugging complex workflows requires detailed execution traces. Need structured logging that supports replay analysis, performance debugging, and operational monitoring without excessive overhead.

**Motivation:** Enables post-mortem debugging, supports verification of deterministic replay, provides operational observability, facilitates performance analysis and optimization.

**Success criteria:**
- Trace event per reduction step
- Replay reproduces exact trace
- Configurable trace levels
- Integration with OTP logging

**Technical constraints:**
- Structured event format
- Configurable verbosity
- Minimal performance impact at 'min' level
- Supports trace queries (range retrieval)

**Signals:** priority: high, urgency: Essential for debugging and observability

## Summary

The CRE (YAWL Workflow Engine) codebase already has a **rich foundation** for structured tracing and replay logging. Three primary tracing subsystems exist:

1. **`ln_trace`** (`src/ln_trace.erl`) - A lightweight structured event tracing buffer with configurable levels (none/min/full), sequential numbering, and range queries. This appears to be the most aligned with the requirements.

2. **`yawl_telemetry`** (`src/yawl_telemetry.erl`) - A comprehensive OpenTelemetry-style gen_server with span management, metrics collection, distributed tracing support, and audit logging. This is feature-rich but may be heavier than needed.

3. **`yawl_logging`** (`src/yawl_logging.erl`) - A YAWL-specific audit logger with event types for cases/work items, OpenXES export, and configurable log levels. This is domain-specific but could be integrated.

The recommendation is to **extend and standardize on `ln_trace`** as the core structured tracing primitive, while integrating with existing OTP logger and creating adapters for YAWL-specific telemetry. The replay functionality can be built by serializing trace events and implementing a deterministic replay verifier.

## Current State Analysis

### Existing Implementation

The codebase has **multiple overlapping tracing systems**:

1. **`ln_trace`** (lines 1-138) - Core structured event tracing
   - Event types: `case_started`, `step_started`, `step_completed`, `branch_chosen`, `join_waiting`, `effect_requested`, `effect_completed`, `scope_cancelled`, `case_completed`, `case_failed`, `case_cancelled`
   - Configurable levels: `none`, `min`, `full`
   - Sequential event numbering for range queries
   - Event format: `#{timestamp => integer(), type => event_type(), data => map(), seq => non_neg_integer()}`
   - Export formats: map, list, json

2. **`yawl_telemetry`** (lines 1-1475) - Full telemetry system
   - Span management with trace context propagation
   - Metrics collection with Prometheus export
   - Health checks and alerting
   - Audit logging with time-range queries
   - DOT export for visualization
   - gen_server-based with persistent state

3. **`yawl_logging`** (lines 1-1168) - YAWL audit logging
   - Event types for cases, work items, engine events
   - OpenXES XML export for process mining
   - CSV and JSON export
   - Configurable log levels: `debug`, `info`, `warning`, `error`, `critical`
   - gen_server-based with in-memory event buffer

4. **`tracing`** (lines 1-300) - OpenTelemetry distributed tracing
   - W3C trace context support
   - Span creation/management
   - Attribute and event annotation
   - Trace context injection/extraction for distributed systems

5. **`wf_audit_log`** (lines 1-493) - Persistent audit trail
   - disk_log-based append-only storage
   - Cursor-based pagination
   - Receipt format with before/after hashes
   - Cloud logging export (fire-and-forget)
   - Temporary file support for testing

### Key Files

- **`src/ln_trace.erl:1-138`** - Core structured event tracing buffer with configurable levels and range queries. This is the closest match to the requirements.

- **`src/yawl_telemetry.erl:1-1475`** - Comprehensive telemetry gen_server with span management, metrics, health checks, and audit logging. Feature-rich but heavier weight.

- **`src/yawl_logging.erl:1-1168`** - YAWL-specific audit logger with OpenXES export, configurable levels, and gen_server-based event buffering.

- **`src/telemetry/tracing.erl:1-300`** - OpenTelemetry distributed tracing with W3C trace context support, span management, and trace context propagation.

- **`src/wf/wf_audit_log.erl:1-493`** - Persistent append-only audit log using disk_log with cursor-based pagination and cloud logging export.

- **`src/wf/cre_trace.erl:1-360`** - Advanced tracing utilities using dbg/redbug for function call, message, and process lifecycle tracing.

- **`src/ln_introspect.erl:96-101`** - Trace entry format with step numbering, transition tracking, and timestamp.

- **`src/core/gen_pnet.erl:59-60`** - Petri net step API (`step/1`) that represents reduction steps in workflow execution.

- **`src/wf/wf_engine.erl:122-128`** - Receipt format for audit trail with before/after hashes, move record, and timestamp.

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- **`ln_trace`** - Core tracing buffer (extend this)
- **`logger`** (OTP) - Standard OTP logging (already integrated via `include_lib("kernel/include/logger.hrl")`)
- **`gen_pnet`** - Petri net execution engine (reduction steps happen here via `step/1`)
- **`wf_engine`** - Workflow engine (receipt generation)
- **`yawl_telemetry`** - Existing telemetry system (can integrate/adapt)
- **`yawl_logging`** - YAWL audit logging (can use as export adapter)

**External dependencies:**
- **OTP logger** - Standard logging framework (already in applications list)
- **disk_log** - Persistent log storage (already used in `wf_audit_log`)
- **crypto** - For hash generation in receipts (already in applications list)

### Patterns to Follow

1. **Event format from `ln_trace`** (`src/ln_trace.erl:38-42`):
   ```erlang
   -type event() :: #{
       timestamp := integer(),
       type := event_type(),
       data => map()
   }.
   ```
   This map-based format is flexible and extensible.

2. **Trace levels from `ln_trace`** (`src/ln_trace.erl:44`):
   ```erlang
   -type trace_level() :: none | min | full.
   ```
   Simple three-level system for controlling verbosity.

3. **Sequential numbering from `ln_trace`** (`src/ln_trace.erl:79`):
   ```erlang
   emit(#{
       timestamp := _} = Event, #trace_state{events = Events, max_events = Max, seq = Seq} = State) ->
       NewEvents = [Event#{seq => Seq} | Events],
       ```
   Enables range-based queries and replay ordering.

4. **gen_server pattern from `yawl_telemetry`** (`src/yawl_telemetry.erl:397-439`):
   Standard OTP gen_server with state management for trace buffering.

5. **Export format adapters** (`src/ln_trace.erl:105-112`):
   Multiple export formats (map, list, json) for different consumers.

6. **Receipt format from `wf_engine`** (`src/wf/wf_engine.erl:123-128`):
   ```erlang
   -type receipt() :: #{
       before_hash := binary(),
       after_hash := binary(),
       move := pnet_receipt:move(),
       ts := integer()
   }.
   ```
   Hash-based format supports deterministic replay verification.

7. **Cursor-based pagination from `wf_audit_log`** (`src/wf/wf_audit_log.erl:235-259`):
   Efficient range retrieval without loading entire log.

8. **Integration with OTP logger** (`src/yawl_logging.erl:461-468`):
   ```erlang
   case whereis(?MODULE) of
       undefined ->
           error_logger:info_report([...]);
       Pid ->
           gen_server:cast(Pid, {log, Entry})
   end
   ```
   Fallback to error_logger if gen_server not available.

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Performance overhead at 'min' level** | High | Use compile-time macros to completely disable tracing at 'none' level; only add minimal overhead (timestamp counter increment) at 'min' level |
| **Memory growth from unbounded trace buffers** | Medium | Implement configurable max_events with automatic trimming (already in `ln_trace:69-74`); add periodic cleanup |
| **Non-deterministic replay due to timing/concurrency** | High | Record sufficient context (inputs, RNG state, timestamps) to enable deterministic replay; use hash-based verification from receipts |
| **Integration complexity with multiple existing tracing systems** | Medium | Choose one primary system (`ln_trace`) and create adapters for others; avoid creating yet another tracing subsystem |
| **Hot code loading compatibility** | Low | Use gen_server code_change callback (already pattern in `yawl_telemetry:1036`) |
| **Disk space exhaustion from persistent logs** | Medium | Implement log rotation and retention policies (already pattern in `yawl_telemetry:988-999`) |

## Recommended Approach

### High-Level Strategy

1. **Standardize on `ln_trace` as the core tracing primitive**
   - It's lightweight, has the right feature set (levels, seq numbers, range queries)
   - Extend it to add replay support and OTP logger integration
   - Keep it as a functional API (state in), not a gen_server

2. **Create a `trace_replay` module** for replay functionality
   - Serialize trace events to disk (JSON or binary format)
   - Implement deterministic replay verification using hash-based receipts
   - Add trace comparison utilities for debugging

3. **Integrate with existing systems via adapters**
   - `yawl_telemetry` adapter for OpenTelemetry span export
   - `yawl_logging` adapter for YAWL audit log export
   - OTP logger handler for standard logging integration

4. **Add trace points to reduction steps**
   - Instrument `gen_pnet:step/1` to emit trace events
   - Instrument `wf_engine` to capture workflow lifecycle events
   - Use trace level to control granularity

### Implementation Plan

**Phase 1: Extend `ln_trace`**
1. Add OTP logger integration (emit to logger in addition to buffer)
2. Add persistence API (save/load trace to file)
3. Add trace level macros for zero-overhead when disabled
4. Add more event types for workflow execution

**Phase 2: Create `trace_replay` module**
1. Implement trace serialization/deserialization
2. Implement deterministic replay execution
3. Implement trace comparison utilities
4. Add hash-based verification

**Phase 3: Integration**
1. Add trace points to `gen_pnet:step/1`
2. Add trace points to `wf_engine` lifecycle
3. Create adapters for `yawl_telemetry` and `yawl_logging`
4. Add configuration for trace level

**Phase 4: Testing and Documentation**
1. Add unit tests for trace capture
2. Add integration tests for replay
3. Add performance benchmarks
4. Document API and usage

### Module Structure

```
src/telemetry/
  ├── ln_trace.erl          # Core tracing (extend)
  ├── trace_replay.erl      # NEW: Replay execution
  ├── trace_persist.erl     # NEW: Trace serialization
  └── trace_logger.erl      # NEW: OTP logger integration
src/core/
  └── gen_pnet.erl          # Add trace points to step/1
src/wf/
  └── wf_engine.erl         # Add trace points to lifecycle
```

## Open Questions

1. **Trace granularity**: Should we trace at the Petri net transition level (fine-grained) or workflow task level (coarse-grained)? Both levels may be needed with configurable verbosity.

2. **Replay determinism**: How do we handle non-deterministic elements (RNG, timestamps, external dependencies)? Need to record RNG state and potentially mock external services.

3. **Performance budget**: What is the acceptable overhead percentage at 'min' trace level? This will guide how much instrumentation we can add.

4. **Retention policy**: How long should traces be retained? Need configurable retention with automatic cleanup (already pattern in `yawl_telemetry:988-999`).

5. **Integration scope**: Should we replace existing tracing systems or create adapters? Recommendation is adapters to avoid breaking changes.

6. **Cloud export**: Should traces be exported to cloud logging (like `wf_audit_log_cloud:append/1`)? This adds observability but also latency.

7. **Trace format**: Should we use JSON (human-readable) or binary (efficient) for persistence? May need both for different use cases.

8. **Concurrent tracing**: How do we handle multiple workflow cases executing concurrently? Each case should have its own trace context.

9. **Backwards compatibility**: How do we ensure existing code continues to work when we add tracing? Use feature flags and gradual rollout.

10. **Testing strategy**: How do we verify that replay produces exactly the same trace? Need automated comparison tools.
