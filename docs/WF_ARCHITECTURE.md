# WF Substrate Architecture

## Overview

The WF Substrate is a pure-Erlang control substrate that compiles workflow patterns into executable form. Rather than interpreting workflow graphs at runtime (the "engine interpreting data" antipattern), patterns are compiled to **bytecode** that is executed by a tight reducer loop, ensuring minimal latency and deterministic behavior.

## Key Design Principles

1. **Patterns as Native Primitives**: Users author only using pattern constructors; patterns compile to bytecode at creation time.
2. **No Hot-Path AST Dispatch**: Runtime execution never interprets node types in a loop; all dispatch happens at compile time.
3. **Atomic State Model**: Case state is held in a single store with explicit effect boundaries; no shadow state across engines.
4. **Observable and Replayable**: Every reduction step produces structured trace events; executions are deterministically replayable.
5. **Pure Erlang/OTP**: No NIFs, ports, or external processes; uses only stdlib + OTP behaviors.

## Module Dependency Graph

```
wf_substrate (public API)
  ├── wf_case_runner (gen_server per case)
  │   ├── wf_exec (hot-loop reducer)
  │   ├── wf_sched (scheduler policy)
  │   └── wf_state (atomic state store)
  │
  ├── wf_term (AST constructors)
  │   └── wf_core (kernel patterns + smart constructors)
  │
  ├── wf_compile (term → bytecode)
  │   ├── wf_vm (opcode definitions)
  │   └── wf_validate (structure checks)
  │
  ├── wf_cancel (cancellation semantics)
  ├── wf_mi (multiple instances)
  ├── wf_effect (effect boundary + tool calls)
  ├── wf_receipt (causal tracking + idempotence)
  ├── wf_trace (structured events + replay)
  │
  └── wf_substrate_sup (supervision tree)
      └── wf_case_sup (case runner supervisor)

Test modules:
  ├── wf_test_primitives (kernel semantics)
  ├── wf_test_join_policies (synchronization)
  ├── wf_test_cancel (cancellation)
  ├── wf_test_mi (multiple instances)
  ├── wf_test_determinism (replay validation)
  └── wf_bench (performance harness)
```

## Runtime Strategy: Bytecode VM (S1)

### Compilation Pipeline

```
wf_term (AST)
  ↓
wf_compile:compile/1
  ↓
wf_bc (bytecode list)
  ↓
Stored in exec_state{program=BC}
```

### Opcodes

**Kernel opcodes**:
- `seq_enter / seq_exit`: Sequence boundaries
- `par_fork / par_join`: Parallel split and synchronization
- `xor_choose`: Exclusive choice selector
- `xor_merge / join_wait(Policy)`: Join variants (all, xor_merge, sync_merge, first_n, n_of_m)
- `loop_back(Condition)`: Loop entry and conditional back-edge
- `defer_race`: Deferred choice (race on external signals)
- `cancel_scope_enter / cancel_scope_exit`: Cancellation region boundaries
- `mi_spawn(Policy) / mi_join(Policy)`: Multiple instance fork and join
- `task_enter / task_call / task_exit`: Task invocation with effect handling
- `effect_yield / effect_resume`: Effect suspension and resumption
- `halt / error`: Termination opcodes

**Stack-based execution**:
- Explicit stack `[{Opcode, PC, Locals} | ...]` tracks scope nesting
- Join counters indexed by join point ID
- Cancellation flags propagated to scoped sections

### Execution Model

```
exec_state = {
  program = [Opcode],         % compiled bytecode
  pc = integer(),             % program counter
  stack = [StackFrame],       % scope/frame stack
  ctx = context(),            % user data + tokens
  joins = #{JoinId => Counter},
  cancel = #{ScopeId => bool},
  trace = [TraceEvent]
}
```

**Reducer step**:
1. Fetch opcode at PC
2. Match opcode type (seq, par, xor, etc.)
3. Update state based on opcode semantics
4. Advance PC (or jump, or push/pop stack)
5. Emit trace event
6. Return {continue, NewExecState} | {yield, EffectSpec} | {halt, FinalCtx}

**Quanta-based scheduling**:
- Execute N steps per timer tick
- Yield to allow other cases to progress
- Prevent starvation and long-latency pauses

## State Management

### Case State (`wf_state.erl`)

```erlang
case_state() = {
  case_id = CaseId,
  exec = exec_state(),            % execution machine state
  user_ctx = context(),           % user-provided context
  tokens = #{TokenId => Value},   % per-activity tokens
  effects = #{EffectId => {Status, Result}},
  committed = integer()           % last committed step
}
```

**Atomic Commit Protocol**:
- All state mutations staged in exec_state
- On effect yield, state checkpoint taken
- On effect result, state rolled back or committed
- User context (tokens, data) updated only at commit boundaries

### Context (User Data)

```erlang
context() = #{
  data => any(),           % user-provided data store
  signals => [Signal],     % inbound signals (from external world)
  results => map()         % effect results
}
```

## Pattern Algebra

### Kernel Constructors (in wf_term.erl)

```erlang
-type wf_term() ::
      {task, Name, Fun}
    | {seq, wf_term(), wf_term()}
    | {par, [wf_term()]}
    | {xor, [wf_term()]}
    | {join, join_policy(), [wf_term()]}
    | {loop, loop_policy(), wf_term()}
    | {defer, [wf_term()]}
    | {cancel, scope_spec(), wf_term()}
    | {mi, mi_policy(), wf_term()}.

-type join_policy() :: all | xor_merge | sync_merge | first_n(N) | n_of_m(N,M).
-type loop_policy() :: {max_iter, N} | {while, Fun} | {until, Fun}.
-type mi_policy() :: {fixed, N} | {dynamic, CollectorFun}.
-type scope_spec() :: {region, RegionId} | {activity, ActivityId} | {case, CaseId}.
```

### Smart Constructors (in wf_core.erl)

Derived patterns built from kernels:
- `simple_merge(Procs)` = `join(xor_merge, Procs)`
- `synchronizing_merge(Procs)` = `join(sync_merge, Procs)`
- `discriminator(Procs)` = `{first_n, 1, Procs}` + cancel remaining
- `n_out_of_m(N, Procs)` = `join({n_of_m,N,length(Procs)}, Procs)`
- etc. (all 43 patterns expressible as combinations)

## Semantics Outline

Small-step reduction rules defined in docs/WF_SEMANTICS.md. Key invariants:
- **Deadlock freedom** (bounded): validator checks for unreachable join points
- **Proper completion** (bounded): at least one accepting path exists
- **Cancellation soundness**: cancelled scope's tokens never fire transitions
- **No token loss**: all tokens accounted for (generated, in flight, consumed)

## Observable Execution

### Trace Events

```erlang
trace_event() = {
  seq = integer(),       % global sequence number
  type = atom(),         % 'step_exec', 'task_enter', 'task_exit', 'effect_yield', ...
  opcode = atom(),       % the opcode executed
  ctx = context(),       % context after this step
  timestamp = integer(), % microseconds
  scope = [ScopeId],     % nesting path
  cancel_signal = bool   % was this step cancelled?
}.
```

**Replay capability**: Deterministic scheduler + trace log → identical sequence of exec states.

## Failure & Cancellation

### Task Failure
```
task returns {error, Reason}
  ↓
Case enters error state
  ↓
Optional recovery callback invoked
  ↓
Case terminates with error receipt
```

### Activity Cancellation
```
cancel_activity(ActivityId)
  ↓
Scheduler sends cancel signal to activity
  ↓
Activity's opcode detects cancel flag
  ↓
Activity halts; no further steps in that activity
```

### Region Cancellation
```
cancel_region(RegionId)
  ↓
All activities in region set cancel flag
  ↓
Cascades through nested scopes
  ↓
Region's join cleanup triggered
```

### Case Cancellation
```
cancel(CaseId)
  ↓
set cancel flag on root scope
  ↓
all reduction steps detect cancel
  ↓
case halts with cancelled receipt
```

## OTP Integration

### Supervision Tree
```
wf_substrate_sup
  ├── wf_case_sup (simple_one_for_one)
  │   └── wf_case_runner (gen_server, spawned per case)
  └── wf_effect_sup (optional, for async effect execution)
```

### Case Runner (`wf_case_runner.erl`, gen_server)

- **State**: `case_state()` (includes exec_state)
- **Messages**:
  - `{reduce, NumSteps}`: execute N reduction steps
  - `{signal, Msg}`: inbound signal from external world
  - `{effect_result, EffectId, Result}`: result from effect execution
  - `{cancel}`: cancel the case
  - `{get_status}`: return `{status, #{state, steps, current_activity, ...}}`
- **Timers**: periodic reductions + effect timeout handling

### Effect Execution

Effects are yielded to a separate effect executor (could be inline or async):
```erlang
{effect_yield, EffectSpec, ContCtx}
  ↓
wf_effect_sup worker handles EffectSpec
  ↓
Result sent back to case runner
  ↓
case runner resumes reducer with result
```

## Testing Strategy

1. **Unit Tests** (wf_test_primitives): Each opcode semantics in isolation
2. **Property Tests**: Random pattern generation + determinism assertion
3. **Bounded Model Checks**: Validator explores state space up to depth D, token bound K
4. **Determinism & Replay**: Run case twice, compare traces bit-for-bit
5. **Performance**: Microbench for sequence, par, xor, cancel, MI patterns

## Performance Targets

- **Pure sequence**: < 1 μs per step (10k steps in < 10ms)
- **Parallel join**: < 10 μs per join decision (100 branches in < 1ms)
- **Cancellation**: < 100 μs for cancel_region (50 activities in < 5ms)
- **Effect yield/resume**: < 50 μs round-trip (excluding external effect time)
- **Memory**: < 1MB per case context (excluding large user data)

## Integration with CRE

WF Substrate modules integrate into the existing CRE app:
- Code lives in `src/wf/` directory
- Tests in `test/wf_*` suite
- Examples in `examples/wf_*.erl`
- Documentation in `docs/WF_*.md`

---

**Document Version**: 1.0
**Status**: Architecture finalized, implementation beginning
