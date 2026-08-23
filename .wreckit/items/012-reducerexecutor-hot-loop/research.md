# Research: Reducer/executor hot loop

**Date**: 2025-01-14
**Item**: 012-reducerexecutor-hot-loop

## Research Question
Need an execution engine that applies pattern semantics as direct operations without repeatedly walking and dispatching over AST nodes, while supporting tracing, effects, and cancellation.

**Motivation:** Provides the core execution engine with bounded overhead per step, enables deterministic scheduling, supports cancellation propagation, produces structured traces for debugging and replay.

**Technical constraints:**
- Tight loop executing opcodes or stepping frames
- Explicit exec_state mutation
- Configurable quanta (N reductions per tick) then yield/hibernate
- Produce trace event per reduction
- Handle effect yields and resumes
- Support cancellation signals

**Signals:** priority: critical, urgency: Core runtime component - required for all pattern execution

## Summary

The CRE (Common Runtime Environment) codebase currently has **multiple execution engines** operating at different layers:

1. **Petri Net-based execution** (`gen_pnet`, `wf_engine`) - Lower-level, structural execution
2. **YAWL workflow execution** (`yawl_executor`, `yawl_compile`) - High-level workflow orchestration
3. **Plan-based bytecode execution** (`ln_compile`) - Experimental compiler to bytecode
4. **Workflow net execution** (`wfnet_engine`) - Step-based event-driven runtime

The research reveals that **item 012's requirements align closely with the existing `ln_compile` bytecode approach**, but this appears to be incomplete/experimental. The system needs a **unified, production-ready reducer/executor** that combines the best aspects of these approaches.

**Key Finding**: The codebase has a **bytecode compiler** (`ln_compile`) that compiles plan terms to opcodes, but **no corresponding bytecode executor/vm**. This is the gap that item 012 needs to fill.

## Current State Analysis

### Existing Implementation

#### 1. Bytecode Compilation (`ln_compile.erl`)

**Location**: `/Users/sac/cre/src/ln_compile.erl:1-241`

The system has a **plan-to-bytecode compiler** that generates opcodes:

```erlang
-type opcode() :: {op_task_start, atom()}
                | {op_task_complete, term()}
                | {op_fork, [label()], label()}
                | {op_join_wait, join_id(), label()}
                | {op_xor_branch, [label()], label()}
                | {op_scope_enter, scope_id()}
                | {op_scope_exit, scope_id()}
                | {op_defer_start, [label()], label()}
                | {op_loop_check, label(), label()}
                | {op_loop_iter, label()}
                | {op_wait_check, term(), label()}
                | {op_halt, term()}.
```

**Key features**:
- Compiles plan terms to labeled bytecode program
- Generates join table for parallel fork/join coordination
- Generates scope table for cancellation regions
- Produces structured program with labels for jumps

**Missing**: No bytecode executor/VM exists to run this bytecode.

#### 2. Petri Net Execution (`gen_pnet`, `wf_engine`)

**Location**: `/Users/sac/cre/src/wf/wf_engine.erl:1-1580`

The current production execution engine uses **Petri net semantics**:

```erlang
-record(wf_case, {
    case_id :: case_id(),
    status :: case_status(),
    work_items = #{} :: #{wi_id() => work_item()},
    data = #{} :: map(),
    receipts = [] :: [receipt()],
    events = [] :: [term()],
    log = [] :: [term()],
    marking :: pnet_types:marking(),
    rng_state :: pnet_choice:rng_state(),
    scheduled_at :: integer() | undefined,
    timestamps :: map()
}).
```

**Execution model** (`wf_engine.erl:1299-1324`):
- Step-based transition firing
- Receipt generation for audit trail
- Event emission for observability
- Work item lifecycle management
- Marking-based state representation

**Limitations**:
- Repeated AST walking for enabled transition checks
- No explicit bytecode/opcode execution
- Higher overhead per reduction
- No configurable quanta or yielding

#### 3. Workflow Net Engine (`wfnet_engine`)

**Location**: `/Users/sac/cre/src/wfnet/wfnet_engine.erl:1-1075`

A **step-based, event-driven** execution engine:

```erlang
-record(case_state, {
    case_id :: wfnet_types:case_id(),
    status :: wfnet_types:case_status(),
    marking :: wfnet_types:marking(),
    usr_info :: map(),
    receipts = [] :: [term()],
    created_at :: integer(),
    updated_at :: integer(),
    parent_case :: wfnet_types:case_id() | undefined,
    context :: map()
}).
```

**Features** (`wfnet_engine.erl:287-328`):
- `execute_step/1-2` - Single transition execution
- `execute_steps/2-3` - Batch execution with count
- `run_to_completion/1-2` - Execute until terminal
- Event subscription and emission
- Suspend/resume support

**Architecture**:
- gen_server-based execution
- Explicit step counting
- Receipt-based audit trail
- Event buffering and filtering

**Limitations**:
- Still uses marking-based execution (not bytecode)
- No quanta-based yielding
- No cancellation signal handling in hot loop

#### 4. YAWL Compilation (`yawl_compile.erl`)

**Location**: `/Users/sac/cre/src/core/yawl_compile.erl:1-1301`

Compiles YAWL specifications to **gen_pnet modules**:

- Generates Erlang module source code
- Each net becomes a gen_pnet behavior module
- Implements `place_lst/0`, `trsn_lst/0`, `init_marking/2`, `preset/1`, `is_enabled/3`, `fire/3`
- Supports pattern expansion and variable initialization

**Output** (`yawl_compile.erl:544-639`):
```erlang
-module(yawl_NetId).
-behaviour(gen_pnet).
%% Auto-generated callbacks...
```

**Limitations**:
- Generates code, not bytecode
- No execution loop control
- No tracing/effects integration

### Key Files

#### Core Execution Files

1. **`/Users/sac/cre/src/ln_compile.erl:1-241`**
   - **Purpose**: Plan-to-bytecode compiler
   - **Key exports**: `compile/1`, `opcode_name/1`
   - **Opcodes defined**: `op_task_start`, `op_task_complete`, `op_fork`, `op_join_wait`, `op_xor_branch`, `op_scope_enter`, `op_scope_exit`, `op_defer_start`, `op_defer_wait`, `op_loop_check`, `op_loop_iter`, `op_wait_check`, `op_halt`
   - **Status**: **Compiler exists, no executor**

2. **`/Users/sac/cre/src/wf/wf_engine.erl:1-1580`**
   - **Purpose**: Production workflow engine (Petri net based)
   - **Key functions**: `start_case/3`, `complete/5`, `tick/2`, `enabled/2`
   - **Execution model**: Step-based transition firing with marking
   - **Records**: `#wf_case{}`, `#work_item{}`, `#engine_state{}`
   - **Limitation**: No bytecode, no quanta yielding

3. **`/Users/sac/cre/src/wfnet/wfnet_engine.erl:1-1075`**
   - **Purpose**: Step-based workflow net execution
   - **Key functions**: `execute_step/1-2`, `execute_steps/2-3`, `run_to_completion/1-2`
   - **Features**: Event emission, receipts, suspend/resume
   - **Records**: `#case_state{}`, `#engine_state{}`
   - **Limitation**: Marking-based, not bytecode

4. **`/Users/sac/cre/src/yawl_executor.erl:1-1723`**
   - **Purpose**: Unified YAWL pattern executor
   - **Categories**: basic, multiple_instances, state_based, extended_control, data_flow, resource, exception_handling
   - **Key exports**: `execute_pattern/2-3`, `execute_with_timeout/3-4`
   - **Features**: Statistics tracking, state persistence, timeout support
   - **Limitation**: Pattern-based dispatch, not bytecode execution

#### Supporting Files

5. **`/Users/sac/cre/src/core/yawl_compile.erl:1-1301`**
   - **Purpose**: YAWL to gen_pnet code generator
   - **Key exports**: `compile/2`, `compile_to_file/3`, `generate_module/2`
   - **Output**: Erlang source code for workflow nets
   - **Status**: Working but generates code, not bytecode

6. **`/Users/sac/cre/src/core/yawl_compiled.erl:1-664`**
   - **Purpose**: Access compiled YAWL specifications
   - **Key exports**: `net/2`, `tasks/2`, `places/2`, `transitions/2`, `flows/3`
   - **Features**: Pure accessor functions, validation
   - **Type**: `compiled_spec()`, `net_info()`

7. **`/Users/sac/cre/src/wf/cre_trace.erl:1-360`**
   - **Purpose**: Advanced tracing utilities
   - **Key exports**: `trace_workflow/2`, `trace_transitions/1`, `trace_marking_changes/1`
   - **Features**: Module/function tracing, message tracing, workflow-specific tracing
   - **Integration**: Uses `dbg` and `redbug` for tracing

8. **`/Users/sac/cre/src/ln_cancel.erl`** (cancellation handling)
9. **`/Users/sac/cre/src/ln_effect.erl`** (effect handling)
10. **`/Users/sac/cre/src/ln_trace.erl`** (tracing for ln system)
11. **`/Users/sac/cre/src/ln_budget.erl`** (budget/quanta management)

#### Related Pattern Files

12. **`/Users/sac/cre/src/patterns/cancellation.erl`** - Cancellation pattern implementation
13. **`/Users/sac/cre/src/patterns/cancel_region.erl`** - Cancel region pattern
14. **`/Users/sac/cre/src/wf/wf_cancel.erl:1-1`** - Cancellation operations
15. **`/Users/sac/cre/src/wf/yawl_cancel_runtime.erl`** - Cancellation runtime support

## Technical Considerations

### Dependencies

**Internal modules to integrate with**:
- `ln_compile` - Bytecode compiler (already exists)
- `ln_cancel` - Cancellation signal handling
- `ln_effect` - Effect yield/resume handling
- `ln_trace` - Tracing integration
- `ln_budget` - Quanta/budget management
- `pnet_marking` - Marking operations (for state)
- `pnet_receipt` - Receipt generation
- `wfnet_events` - Event emission
- `cre_trace` - Existing tracing infrastructure

**External dependencies**:
- Erlang/OTP gen_server for executor process
- ETS tables for state storage
- `dbg` or `redbug` for tracing

### Patterns to Follow

**Existing patterns in the codebase**:

1. **Step-based execution** (`wfnet_engine`):
   ```erlang
   execute_steps(Engine, CaseId, Count) ->
       gen_server:call(Engine, {execute_steps, CaseId, Count}, infinity).
   ```
   - Batch execution with count parameter
   - Return results for each step
   - Use gen_server for concurrency

2. **Receipt generation** (`wf_engine:1246-1253`):
   ```erlang
   Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
   Case1 = Case#wf_case{receipts = [Receipt | Case#wf_case.receipts]},
   ```
   - Hash before/after states
   - Store receipts in case state
   - Use `pnet_receipt` module

3. **Event emission** (`wfnet_engine:994-1004`):
   ```erlang
   emit_event(Type, CaseId, Data, #engine_state{event_subscribers = Subs}) ->
       Event = wfnet_events:emit_event(Type, CaseId, Data),
       lists:foreach(fun({Sub, Filter}) ->
           case wfnet_events:event_filter_match(Event, Filter) of
               true -> Sub ! {wfnet_event, Event};
               false -> ok
           end
       end, Subs),
   ```
   - Filter events before sending
   - Send to subscribed processes
   - Use event records

4. **Marking-based state** (`wf_engine:1226-1244`):
   ```erlang
   BeforeHash = pnet_marking:hash(Marking),
   Marking1 = consume_preset(Marking, Preset),
   Marking2 = apply_produce_map(Marking1, ProduceMap),
   AfterHash = pnet_marking:hash(Marking2),
   ```
   - Hash states for receipts
   - Apply produce maps
   - Use `pnet_marking` module

5. **Gen_server state management**:
   - Use records for state
   - Handle call/cast/info messages
   - Return updated state in responses

### Architecture Recommendations

**Proposed executor structure**:

```erlang
-record(exec_state, {
    bytecode :: ln_compile:bytecode(),
    pc :: non_neg_integer(),           % Program counter
    stack :: [term()],                 % Call stack
    env :: map(),                      % Environment (variables)
    joins :: ln_compile:join_table(),  % Join state
    scopes :: ln_compile:scope_table(),% Scope state
    receipts :: [term()],              % Execution receipts
    events :: [term()],                % Trace events
    quanta_remaining :: non_neg_integer(),% Remaining reductions
    cancel_flag :: boolean(),          % Cancellation signal
    effect_handler :: pid() | undefined% Effect handler process
}).

-record(executor, {
    exec_state :: exec_state(),
    trace_enabled :: boolean(),
    max_quanta :: pos_integer(),
    subscribers :: [pid()]
}).
```

**Key functions to implement**:
- `run_bytecode/3` - Main execution loop
- `execute_opcode/3` - Individual opcode execution
- `check_cancellation/1` - Check cancellation flag
- `yield_quanta/2` - Yield after quanta exhausted
- `emit_trace_event/3` - Emit trace event
- `handle_effect/2` - Handle effect yields

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **No existing bytecode VM** | High | Must design and implement new executor architecture |
| **Complexity of cancellation** | High | Use existing `ln_cancel` patterns, integrate with scope table |
| **Effect handling coordination** | Medium | Design effect protocol with resume capabilities |
| **Performance of per-reduction tracing** | Medium | Use configurable trace levels, sampling |
| **Quanta management fairness** | Medium | Implement adaptive quanta based on workload |
| **Integration with existing engines** | Medium | Keep existing engines during migration, use feature flags |
| **State persistence** | Low | Reuse existing receipt/event infrastructure |
| **Testing complexity** | Medium | Use property-based testing for executor correctness |

## Recommended Approach

### Phase 1: Bytecode VM Foundation (Core)

**Create `ln_executor` module** with basic bytecode execution:

1. **Executor process** (`ln_executor.erl`):
   - gen_server-based executor
   - State: `#exec_state{}` with PC, stack, environment
   - API: `start_link/1`, `execute/2`, `suspend/1`, `resume/1`, `cancel/1`

2. **Execution loop** (`run_bytecode/3`):
   ```erlang
   run_bytecode(Bytecode, Options, State) ->
       Quanta = maps:get(quanta, Options, 100),
       run_loop(Bytecode, State#exec_state{quanta_remaining = Quota}).

   run_loop(Bytecode, #exec_state{pc = PC, quanta_remaining = 0} = State) ->
       {yield, State};  % Yield control
   run_loop(Bytecode, #exec_state{pc = PC} = State) ->
       Opcode = fetch_opcode(Bytecode, PC),
       case execute_opcode(Opcode, State) of
           {continue, NewState} -> run_loop(Bytecode, NewState);
           {effect, Effect, NewState} -> handle_effect(Effect, NewState);
           {halt, Result, NewState} -> {complete, Result, NewState}
       end.
   ```

3. **Opcode handlers** - Implement each opcode from `ln_compile`:
   - `op_task_start` - Create task work item
   - `op_task_complete` - Complete task, produce token
   - `op_fork` - Spawn parallel branches
   - `op_join_wait` - Wait for branch completion
   - `op_xor_branch` - Non-deterministic choice
   - `op_scope_enter/exit` - Cancellation region management
   - `op_defer_start/wait` - External choice
   - `op_loop_check/iter` - Loop control
   - `op_wait_check` - Message matching
   - `op_halt` - Termination

### Phase 2: Tracing & Effects (Observability)

**Integrate tracing and effect handling**:

4. **Per-reduction tracing**:
   ```erlang
   execute_opcode(Opcode, #exec_state{trace_enabled = true} = State) ->
       Before = hash_state(State),
       Result = execute_opcode_impl(Opcode, State),
       After = hash_state(Result),
       TraceEvent = #trace_event{
           opcode = Opcode,
           before_hash = Before,
           after_hash = After,
           timestamp = erlang:monotonic_time(microsecond)
       },
       emit_trace_event(TraceEvent, State),
       Result;
   execute_opcode(Opcode, State) ->
       execute_opcode_impl(Opcode, State).
   ```

5. **Effect handling**:
   - Design effect protocol: `{effect, Type, Data, Continuation}`
   - Implement effect handler process
   - Support: async service calls, message waits, timers
   - Resume with `execute_with_result/2`

6. **Receipt generation**:
   - Reuse `pnet_receipt:make/3`
   - Generate receipt per reduction
   - Store in `exec_state.receipts`

### Phase 3: Cancellation & Quanta (Control)

**Add cancellation and quanta management**:

7. **Cancellation signals**:
   ```erlang
   check_cancellation(#exec_state{cancel_flag = true}) ->
       {cancelled, State};
   check_cancellation(State) ->
       continue.

   cancel_execution(ExecutorPid) ->
       gen_server:cast(ExecutorPid, cancel).
   ```

8. **Quanta-based yielding**:
   - Count down `quanta_remaining` per reduction
   - Yield when zero
   - Configurable quanta per execution
   - Adaptive quanta based on execution time

9. **Scope-based cancellation**:
   - Use `ln_compile` scope table
   - Track active scopes in stack
   - Cancel all work in scope on signal

### Phase 4: Integration (Production)

**Integrate with existing systems**:

10. **Bridge to existing engines**:
    - Adapter layer to compile YAWL specs → bytecode
    - Fallback to `wf_engine` for unsupported features
    - Migration path for existing workflows

11. **Monitoring integration**:
    - Emit metrics to `cre_metrics`
    - Trace events to `cre_trace`
    - OpenTelemetry integration

12. **Testing infrastructure**:
    - Property-based tests for executor correctness
    - Performance benchmarks vs existing engines
    - Cancellation stress tests

## Open Questions

1. **Execution model**: Should the executor be a single long-running process or spawned per execution?
   - **Recommendation**: Spawn per execution (like `wfnet_engine`) for isolation

2. **State representation**: Should we use marking-based state or a new exec_state representation?
   - **Recommendation**: New `exec_state` with bytecode-specific fields, can bridge to marking if needed

3. **Effect handling semantics**: How do effects integrate with the hot loop?
   - **Question**: Should effects suspend the executor or use async messaging?
   - **Recommendation**: Suspend with continuation, allow effect handler to resume

4. **Tracing overhead**: Per-reduction tracing adds overhead - how to make it optional?
   - **Recommendation**: Configurable trace levels (none, minimal, detailed, debug), sampling for production

5. **Backwards compatibility**: How do we migrate existing workflows?
   - **Recommendation**: Feature flag to choose executor, compile YAWL → bytecode automatically

6. **Cancellation propagation**: How do cancellation signals propagate through parallel branches?
   - **Recommendation**: Use scope table from `ln_compile`, track active scopes in executor state

7. **Quanta tuning**: What's the default quanta? How do we adapt?
   - **Recommendation**: Start with 100 reductions, measure and adapt based on wall-clock time

8. **Error handling**: What happens when opcode execution fails?
   - **Recommendation**: Generate error receipt, emit error event, continue or halt based on policy
