# Research: Deterministic scheduling with replay support

**Date**: 2025-02-11
**Item**: 013-deterministic-scheduling-with-replay-support

## Research Question
Testing and debugging workflows requires deterministic execution. Need scheduler that eliminates nondeterminism from the Erlang VM while still supporting exploration and exact replay.

**Motivation:** Enables property-based testing, reproducible debugging, trace validation, and verification that implementations are correct by comparing deterministic runs against recorded nondeterministic runs.

**Success criteria:**
- Deterministic policy produces identical traces across runs
- Replay mode reproduces nondeterministic run exactly
- Choices are recorded for effect ordering and branch selection

**Technical constraints:**
- Stable ordering of enabled actions in deterministic mode
- Record all nondeterministic choices
- Replay mode must not deviate from recorded choices

**Signals:** priority: high, urgency: Required for testing infrastructure and debugging

## Summary

The CRE codebase **already has a partial implementation** of deterministic scheduling with replay support in the `/Users/sac/cre/src/ln_sched.erl` module. However, this implementation is **not integrated** with the main workflow execution engines (`gen_pnet`, `gen_yawl`, `wf_engine`, `wfnet_engine`).

The research reveals a **dual architecture**:
1. **New bytecode-based execution system** (`ln_*` modules) - Has scheduling and tracing but incomplete executor
2. **Existing Petri net-based execution** (`gen_pnet`, `gen_yawl`) - Production-ready but lacks deterministic scheduling

The key finding is that **item 013 needs to bridge these two systems** by:
- Integrating `ln_sched` with existing execution engines
- Adding choice recording to transition firing
- Implementing replay mode for workflows
- Ensuring stable ordering in deterministic mode

## Current State Analysis

### Existing Implementation

#### 1. Scheduler Module (`ln_sched.erl`)

**Location**: `/Users/sac/cre/src/ln_sched.erl:1-163`

The system **already has a complete scheduler implementation** with three modes:

```erlang
-type mode() :: deterministic | nondeterministic | replay.

-record(sched_state, {
    mode :: mode(),
    seed :: undefined | rand:state(),
    choices :: choice_log(),
    position :: non_neg_integer()
}).
```

**Key features already implemented**:
- **Deterministic mode** (line 95-99): Selects first item by term ordering using `lists:keysort(1, Candidates)`
- **Nondeterministic mode** (line 100-110): Random selection with choice logging
- **Replay mode** (line 111-125): Consumes choices from recorded log
- **Choice recording** (line 128-138): Records type, value, metadata, timestamp
- **Choice verification** (line 151-162): Validates choice matches expected type

**Choice types supported** (line 27-30):
- `xor_selection` - Exclusive choice branches
- `defer_race` - Deferred choice races
- `task_selection` - Task scheduling
- `join_order` - Parallel join ordering

**Critical gap**: This scheduler is **only used by `ln_ctrl`** (experimental bytecode executor), not by production engines.

#### 2. Tracing Module (`ln_trace.erl`)

**Location**: `/Users/sac/cre/src/ln_trace.erl:1-138`

**Structured event tracing** is already implemented:

```erlang
-type event_type() :: case_started
                     | step_started
                     | step_completed
                     | branch_chosen
                     | join_waiting
                     | effect_requested
                     | effect_completed
                     | scope_cancelled
                     | case_completed
                     | case_failed
                     | case_cancelled.
```

**Features**:
- Event buffering with sequence numbers (line 78-81)
- Configurable trace levels: `none | min | full` (line 44)
- Event range queries (line 96-102)
- Export in multiple formats: `map | list | json` (line 104-112)

**Gap**: Tracing is integrated with `ln_ctrl` but not with `gen_yawl` or `gen_pnet`.

#### 3. Control Behavior (`ln_ctrl.erl`)

**Location**: `/Users/sac/cre/src/ln_ctrl.erl:1-329`

**OTP behavior for reliable choreography** that combines:
- Scheduler integration (line 165): `Sched = ln_sched:init(SchedulerMode)`
- Tracing integration (line 166): `Trace = ln_trace:new(...)`
- Effect handling (line 169): `Effect = ln_effect:init(EffectHandler)`
- Cancellation support (line 168): `Cancel = ln_cancel:init()`

**Options supported** (line 54-61):
- `{scheduler, Mode}` - Set scheduler mode
- `{step_quanta, Quanta}` - Reductions per tick
- `{trace, Level}` - Tracing verbosity
- `{budget, Budget}` - Budget management
- `{effect_handler, Module}` - Effect handler

**Key limitation**: This is a **new experimental system** that doesn't integrate with existing YAWL workflows.

#### 4. Existing Execution Engines

**gen_yawl** (`/Users/sac/cre/src/core/gen_yawl.erl:1-1556`):
- Production wrapper around `gen_pnet`
- Supports 3-tuple fire/3 returns: `{produce, Map, NewUsrInfo}`
- Has checkpoint integration (line 894-900)
- **No scheduler integration** - uses Erlang's native scheduling
- Continue loop at line 928-1048

**gen_pnet** (`/Users/sac/cre/src/core/gen_pnet.erl:1-1556`):
- Base Petri net behavior
- 2-tuple fire/3 returns: `{produce, Map}`
- **No deterministic scheduling**
- Continue loop at line 706-721

**wf_engine** (`/Users/sac/cre/src/wf/wf_engine.erl:1-1580`):
- Step-based workflow execution
- Receipt generation for audit trail
- **No choice recording for nondeterminism**

**wfnet_engine** (`/Users/sac/cre/src/wfnet/wfnet_engine.erl:1-1075`):
- Event-driven execution
- Step counting and batch execution
- **No scheduler integration**

### Key Files

**Scheduler and tracing (already implemented):**
- `/Users/sac/cre/src/ln_sched.erl:1-163` - Complete scheduler with 3 modes
- `/Users/sac/cre/src/ln_trace.erl:1-138` - Structured event tracing
- `/Users/sac/cre/src/ln_ctrl.erl:1-329` - Control behavior using scheduler

**Execution engines (need integration):**
- `/Users/sac/cre/src/core/gen_yawl.erl:1-1556` - Production YAWL wrapper, line 928-1048 continue loop
- `/Users/sac/cre/src/core/gen_pnet.erl:1-1556` - Base Petri net behavior, line 706-721 continue loop
- `/Users/sac/cre/src/wf/wf_engine.erl:1-1580` - Workflow engine, line 1299-1324 execution model
- `/Users/sac/cre/src/wfnet/wfnet_engine.erl:1-1075` - Step-based engine, line 287-328 execute_steps

**Supporting modules:**
- `/Users/sac/cre/src/ln_effect.erl` - Effect handling for external interactions
- `/Users/sac/cre/src/ln_cancel.erl` - Cancellation support
- `/Users/sac/cre/src/ln_compile.erl:1-241` - Plan-to-bytecode compiler
- `/Users/sac/cre/src/wf/wf_cancel.erl:1-435` - Cancellation token handling

**Pattern modules (need deterministic ordering):**
- `/Users/sac/cre/src/patterns/exclusive_choice.erl` - XOR choices
- `/Users/sac/cre/src/patterns/multiple_choice.erl` - Multi-branch choices
- `/Users/sac/cre/src/patterns/deferred_choice.erl` - Deferred choices
- `/Users/sac/cre/src/patterns/parallel_split.erl` - Parallel branches
- `/Users/sac/cre/src/patterns/discriminator.erl` - N-out-of-M patterns

## Technical Considerations

### Dependencies

**Internal modules to integrate with**:
- `ln_sched` - Scheduler (already exists, needs integration)
- `ln_trace` - Tracing (already exists, needs integration)
- `gen_yawl` - Primary execution engine
- `gen_pnet` - Base Petri net semantics
- `wf_engine` - Workflow execution
- `pnet_receipt` - Receipt generation
- `yawl_recovery` - Checkpoint and recovery

**External dependencies**:
- `rand` - Erlang random number generator (for seeding)
- `erlang:monotonic_time` - Timestamps for choices
- Mnesia (for checkpoint storage)
- Logger (for telemetry)

### Patterns to Follow

**1. Scheduler integration pattern** (from `ln_ctrl.erl:165`):
```erlang
Sched = ln_sched:init(SchedulerMode),
```

**2. Choice recording pattern** (from `ln_sched.erl:128-138`):
```erlang
record_choice(Type, Value, #sched_state{mode = nondeterministic} = State) ->
    Choice = #choice{
        type = Type,
        value = Value,
        metadata = #{},
        timestamp = erlang:monotonic_time(millisecond)
    },
    State#sched_state{choices = [Choice | State#sched_state.choices]}.
```

**3. Deterministic selection pattern** (from `ln_sched.erl:95-99`):
```erlang
choose(Candidates, #sched_state{mode = deterministic} = State) ->
    Sorted = lists:keysort(1, Candidates),
    {Selected, _} = lists:split(1, Sorted),
    {hd(Selected), State}.
```

**4. Tracing integration pattern** (from `ln_ctrl.erl:197`):
```erlang
ln_trace:emit({case_started, #{}}, Trace),
```

**5. Continue loop pattern** (from `gen_yawl.erl:928-1048`):
- Needs to be modified to call scheduler for enabled transition selection
- Currently selects first enabled transition implicitly
- Should record choices in nondeterministic mode

### Sources of Nondeterminism in Current System

**1. Enabled transition selection**:
- Multiple transitions may be enabled simultaneously
- Current implementation: undefined order (depends on list iteration)
- Needs: Stable ordering in deterministic mode

**2. Parallel branch execution order**:
- `parallel_split` enables multiple output places
- Join ordering is nondeterministic
- Needs: Record join order for replay

**3. Token selection from places**:
- When multiple tokens in a place, which one is consumed?
- Current: undefined (list head)
- Needs: Stable selection order

**4. Race condition handling**:
- `deferred_choice` waits for first of multiple messages
- Current: depends on message arrival order
- Needs: Record winner for replay

**5. External effect ordering**:
- Multiple effects may complete in different orders
- Current: depends on external services
- Needs: Record completion order

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Breaking existing workflows** | High - Deterministic ordering may change execution semantics | Add opt-in flag for deterministic mode; default to current behavior |
| **Performance overhead** | Medium - Choice logging and term sorting add overhead | Benchmark overhead; use sampling for production traces |
| **Incomplete choice logging** | High - Missed choices cause replay divergence | Add runtime assertions; verify replay produces same trace |
| **State space explosion** | Medium - All possible choice combinations may be huge | Use property-based testing with limited exploration |
| **Integration complexity** | High - Multiple execution engines need modification | Phase integration: start with gen_yawl, then extend |
| **Replay validation** | Critical - Replay must produce identical final state | Add replay tests to CI/CD; compare state hashes |
| **Checkpoint compatibility** | Medium - Existing checkpoints may not have choice logs | Version checkpoint format; provide migration tool |
| **Hidden nondeterminism** | High - Unknown sources may cause replay divergence | Add tracing for all system interactions; fuzz testing |

## Recommended Approach

### Phase 1: gen_yawl Integration (High priority, Medium risk)

**1. Modify gen_yawl continue loop to use scheduler**:

**Location**: `/Users/sac/cre/src/core/gen_yawl.erl:928-1048`

Current code (line ~950):
```erlang
case progress(NetState0, FireTimeout) of
    abort -> ...;
    {delta, Mode, Pm, NewUsrInfo} -> ...
```

**Modification**: Add scheduler state to `#wrapper_state{}`:
```erlang
-record(wrapper_state, {
    net_state :: #net_state{},
    callback :: module(),
    ...
    scheduler :: ln_sched:sched_state(),  % NEW
    trace :: ln_trace:state()             % NEW
}).
```

**2. Record choice when selecting enabled transition**:

In `progress/2`, when multiple transitions are enabled:
```erlang
get_enabled_transition(Enabled, #wrapper_state{scheduler = Sched0} = Wrapper) ->
    Candidates = [{T, T} || T <- Enabled],
    {Selected, Sched1} = ln_sched:choose(Candidates, Sched0),
    {Selected, Wrapper#wrapper_state{scheduler = Sched1}}.
```

**3. Add scheduler mode to gen_yawl options**:

In `init/1`:
```erlang
SchedulerMode = proplists:get_value(scheduler_mode, NetArg, nondeterministic),
Sched = ln_sched:init(SchedulerMode),
```

**4. Export choice log on completion**:

Add callback or API function:
```erlang
get_choice_log(Pid) ->
    gen_server:call(Pid, get_choice_log).

handle_call(get_choice_log, _From, #wrapper_state{scheduler = Sched}) ->
    {reply, ln_sched:get_log(Sched), WrapperState}.
```

### Phase 2: Pattern Module Updates (Medium priority, Low risk)

**5. Ensure stable ordering in pattern modules**:

**Exclusive choice** (`/Users/sac/cre/src/patterns/exclusive_choice.erl`):
- Ensure branch candidates are sorted by atom name
- Record choice in nondeterministic mode

**Multiple choice** (`/Users/sac/cre/src/patterns/multiple_choice.erl`):
- Same as exclusive choice
- Record which branches were selected

**Parallel split** (`/Users/sac/cre/src/patterns/parallel_split.erl`):
- Record order of branch creation
- Ensure deterministic token distribution

**Discriminator** (`/Users/sac/cre/src/patterns/discriminator.erl`):
- Record which branch completes first
- Record N-out-of-M selection

### Phase 3: Tracing Integration (Medium priority, Low risk)

**6. Integrate ln_trace with gen_yawl**:

Add trace state to wrapper state (see step 1).

Emit trace events:
```erlang
ln_trace:emit({step_started, #{transition => Trsn}}, Trace),
%% ... execute transition ...
ln_trace:emit({step_completed, #{transition => Trsn, result => Result}}, Trace).
```

**7. Add trace export API**:

```erlang
get_trace(Pid) ->
    gen_server:call(Pid, get_trace).

handle_call(get_trace, _From, #wrapper_state{trace = Trace}) ->
    {reply, ln_trace:get_all(Trace), WrapperState}.
```

### Phase 4: Testing and Verification (Critical)

**8. Add property-based tests**:

Use PropEr to verify:
- Deterministic mode produces identical traces across runs
- Replay reproduces original execution exactly
- Choice logs are complete (no hidden nondeterminism)

Example test:
```erlang
prop_deterministic_trace() ->
    ?FORALL({Workflow, Seed}, {workflow_gen(), seed_gen()},
        begin
            Sched1 = ln_sched:init(deterministic),
            {ok, Trace1} = execute_workflow(Workflow, Sched1),
            Sched2 = ln_sched:init(deterministic),
            {ok, Trace2} = execute_workflow(Workflow, Sched2),
            equals(Trace1, Trace2)
        end).
```

**9. Add replay tests**:

```erlang
prop_replay_matches_original() ->
    ?FORALL({Workflow, Seed}, {workflow_gen(), seed_gen()},
        begin
            Sched1 = ln_sched:init(nondeterministic, Seed),
            {ok, Trace1, Choices} = execute_and_record(Workflow, Sched1),
            Sched2 = ln_sched:init(replay, Choices),
            {ok, Trace2} = execute_workflow(Workflow, Sched2),
            equals(Trace1, Trace2)
        end).
```

**10. Add CI/CD integration**:

- Run determinism tests on every PR
- Fail if replay diverges from original
- Track flakiness metrics

### Phase 5: Documentation and Migration (Low priority, Low risk)

**11. Document scheduler modes**:

- When to use deterministic mode (testing, debugging)
- When to use nondeterministic mode (production, exploration)
- How to record and replay choices

**12. Add migration guide**:

- How to enable deterministic mode for existing workflows
- How to interpret choice logs
- How to debug nondeterminism

## Open Questions

1. **Scheduler scope**: Should scheduler be per-workflow-instance or global?
   - **Recommendation**: Per-instance to allow different modes for different workflows

2. **Choice log format**: Should choice logs be persisted or only in-memory?
   - **Recommendation**: Both - in-memory for active replay, persisted for post-mortem debugging

3. **Determinism vs. performance**: How much overhead is acceptable for deterministic mode?
   - **Question**: Should we use term ordering (slow) or hash-based ordering (faster but less stable)?
   - **Recommendation**: Use term ordering for correctness; optimize if needed

4. **Effect ordering**: How do we record ordering of external effects?
   - **Question**: Effects complete asynchronously - when do we record ordering?
   - **Recommendation**: Record when effect is selected from wait queue, not when it completes

5. **Checkpoint format**: Do we include choice logs in checkpoints?
   - **Recommendation**: Yes - checkpoints should capture complete execution state including choices

6. **Backwards compatibility**: Can existing workflows opt-in to deterministic mode?
   - **Recommendation**: Yes - add `scheduler_mode` option to gen_yawl start args

7. **Partial replay**: Can we replay from a specific point in execution?
   - **Recommendation**: Not in v1 - requires full trace reconstruction; consider for future

8. **Distributed execution**: How does scheduler work with distributed Erlang?
   - **Question**: Each node has its own scheduler - how do we coordinate?
   - **Recommendation**: Single scheduler per workflow instance, even if distributed

9. **Testing coverage**: How do we verify we've eliminated all nondeterminism?
   - **Recommendation**: Property-based testing + differential fuzzing (compare deterministic vs nondeterministic)

10. **Integration with existing engines**: Should we also add scheduling to gen_pnet directly?
    - **Recommendation**: No - gen_pnet is low-level; keep scheduling at gen_yawl layer
