# Research: Centralize gen_yawl as single workflow execution authority

**Date**: 2025-01-21
**Item**: 001-centralize-genyawl-as-single-workflow-execution-au

## Research Question
Multiple entry points to gen_pnet create split-brain execution engine with hidden state mutation paths, duplicate logic, and semantic drift. Without centralization, the codebase accumulates accidental complexity and bypasses that make state non-replayable.

**Motivation:** Makes CRE semantics enforceable, human-in-the-loop state correct, and GCP/enterprise readiness straightforward. Prevents future bugs where state updates happen outside observable execution paths.

**Success criteria:**
- All fire/3 implementations return {produce, Map, NewUsrInfo} 3-tuple explicitly
- No direct gen_pnet calls outside gen_yawl (only internal dependency)
- All external usr_info mutation moved into fire/3 or trigger/3 callbacks
- Every state mutation representable as (Marking_before, Fire, Marking_after, UsrInfo_after)
- No module spins its own continue loop or calls fire/3 directly
- All execution passes through gen_yawl:step/1, gen_yawl:drain/2, or continue(self())

**Technical constraints:**
- gen_pnet becomes internal dependency, not public API
- usr_info is transition-scoped, not process-scoped
- Checkpoints must capture marking + usr_info
- Replay must re-enter via gen_yawl:init/1 + marking injection

**In scope:**
- Inventory and containment of gen_pnet entry points
- Refactor all YAWL pattern modules to support 3-tuple fire/3
- Eliminate external usr_info mutations
- Enforce single progress loop authority
- API surface cleanup to emphasize gen_yawl over gen_pnet
- Recovery and checkpoint coherence
- Mechanical guard rails and assertions
**Out of scope:**
- Changing the fundamental gen_pnet implementation
- Removing gen_pnet (mark as internal instead)
- Modifying core Petri net semantics

**Signals:** priority: high, urgency: This is a critical architectural spine - refactor until no meaningful workflow logic can execute without passing through gen_yawl

## Summary

The CRE codebase currently has a **split-brain execution model** where both `gen_pnet` and `gen_yawl` are being used as entry points for workflow execution. This creates multiple problems:

1. **Hidden state mutation paths**: Modules using `gen_pnet` directly can mutate state without going through the centralized `gen_yawl` wrapper
2. **Inconsistent usr_info handling**: Some patterns use 3-tuple returns `{produce, Map, UsrInfo}`, others use 2-tuple `{produce, Map}`
3. **Multiple execution loops**: Both `gen_pnet` and `gen_yawl` have their own `continue(self())` loops, creating potential race conditions
4. **Non-replayable state**: Direct `gen_pnet` calls bypass the checkpoint/trace infrastructure in `gen_yawl`

The solution is to **centralize all workflow execution through `gen_yawl`**, making `gen_pnet` an internal dependency only. This ensures:
- All state mutations flow through the 3-tuple fire/3 return pattern
- Checkpoints capture both marking and usr_info consistently
- Every transition is observable and traceable
- Recovery and replay work correctly

## Current State Analysis

### Existing Implementation

#### 1. Dual Behavior System

The codebase has **two separate behaviors**:

**gen_pnet** (`/Users/sac/cre/src/core/gen_pnet.erl`):
- Base Petri net behavior (1556 lines)
- Implements standard 2-tuple fire/3: `abort | {produce, ProduceMap}`
- Has its own progress loop in `handle_cast(continue, ...)` at line 706-721
- Directly calls module callbacks for fire/3, is_enabled/3, etc.

**gen_yawl** (`/Users/sac/cre/src/core/gen_yawl.erl`):
- Wrapper around gen_pnet (1556 lines)
- Extends fire/3 to support 3-tuple: `abort | {produce, ProduceMap} | {produce, ProduceMap, NewUsrInfo}`
- Has its own progress loop in `handle_cast(continue, ...)` at line 928-1048
- Wraps module callbacks and handles usr_info updates automatically
- Adds cycle detection, checkpoint integration, and telemetry

**Key finding**: `gen_yawl` internally creates a `#net_state{}` record (from `gen_pnet`) and wraps it in `#wrapper_state{}`. The wrapper intercepts all execution and delegates to gen_pnet semantics.

#### 2. Pattern Module Behavior Inconsistency

**51 pattern modules** use `-behaviour(gen_yawl).` and already return 3-tuples:
- `/Users/sac/cre/src/patterns/sequence.erl` - Returns `{produce, #{p_task1 => [token]}, UsrInfo}`
- `/Users/sac/cre/src/patterns/parallel_split.erl` - Returns 3-tuples consistently
- `/Users/sac/cre/src/patterns/structured_loop.erl` - Complex state management with 3-tuples
- All patterns in `/Users/sac/cre/src/patterns/` directory (except `cancel_region.erl`)

**But at least 7 workflow modules** still use `-behaviour(gen_pnet).`:
- `/Users/sac/cre/src/order_fulfillment.erl:40` - Order fulfillment orchestrator
- `/Users/sac/cre/src/freight_in_transit.erl:34` - Freight tracking workflow
- `/Users/sac/cre/src/cre_worker.erl` - Generic worker process
- `/Users/sac/cre/src/cre_yawl_exception.erl:41` - Exception handling
- Test modules: `wf_test_net_resume.erl`, `wf_test_net_receipt.erl`, `wf_test_stub_net.erl`, `wf_test_net_trigger_drop.erl`

**Critical finding**: These modules are **bypassing gen_yawl entirely** and using gen_pnet directly, which means:
- No automatic usr_info updates
- No checkpoint integration
- No cycle detection
- No telemetry integration
- State mutations are not observable through gen_yawl's tracing

#### 3. Direct gen_pnet API Usage Outside gen_yawl

Found **11 modules** that call `gen_pnet` APIs directly:

**Workflow execution helpers** (legitimate use cases, should use gen_yawl instead):
- `/Users/sac/cre/src/wf/yawl_execution.erl:148` - `gen_pnet:start_link(NetMod, NetArg, [])`
- `/Users/sac/cre/src/wf/yawl_recovery.erl:44` - Resume example uses `gen_pnet:start_link`

**Production workflows** (bypassing gen_yawl):
- `/Users/sac/cre/src/order_fulfillment.erl:153` - Starts workflows with `gen_pnet:start_link`
- `/Users/sac/cre/src/freight_in_transit.erl:142` - Starts workflows with `gen_pnet:start_link`
- `/Users/sac/cre/src/cre_worker.erl:229,249` - Worker processes using gen_pnet directly

**Test infrastructure** (acceptable for unit testing):
- `wf_test_stub_net.erl`, `wf_test_net_receipt.erl` - Test doubles
- `yawl_timer_runtime.erl` - Timeout testing

**Key finding**: The `yawl_execution` module is meant to be a **high-level API**, but it's calling `gen_pnet` instead of `gen_yawl`. This is wrong because:
- Line 147-148: `start_link/2` calls `gen_pnet:start_link/3`
- Line 174-175: `start_link/4` calls `gen_pnet:start_link/4`
- Should be calling `gen_yawl` instead to get the 3-tuple support

#### 4. usr_info Mutation Patterns

**Current state**: The codebase has **inconsistent usr_info handling**:

**3-tuple pattern (already using gen_yawl correctly)**:
```erlang
% From sequence.erl:45-52
fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_task1 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_task2 => [token]}, UsrInfo};
```

**State-mutating pattern (updates usr_info in 3-tuple)**:
```erlang
% From structured_loop.erl:434-443
fire('t_execute_body', #{'p_body_active' := [{state, CurrentState}]},
     #loop_state{body_fun = BodyFun} = State) ->
    NewState = try
        BodyFun(CurrentState)
    catch
        _:_ -> CurrentState
    end,
    {produce, #{
        'p_body_active' => [],
        'p_body_done' => [{state, NewState}]
    }, State#loop_state{current_state = NewState}}.
```

**2-tuple pattern (gen_pnet modules, no usr_info update)**:
```erlang
% From order_fulfillment.erl (gen_pnet behavior)
% Cannot return updated usr_info in fire/3
% Must use handle_call/handle_cast to mutate state
```

**Critical finding**: Modules using `gen_pnet` have **no mechanism to update usr_info in fire/3**. They must:
1. Use `handle_call/3` or `handle_cast/2` to mutate state
2. Call `gen_pnet:inject/2` to add tokens
3. Manually manage state consistency

This creates **hidden state mutation paths** that are not observable as transitions.

#### 5. Execution Flow and Continue Loops

**Both gen_pnet and gen_yawl have separate continue loops**:

**gen_pnet continue loop** (`/Users/sac/cre/src/core/gen_pnet.erl:706-721`):
```erlang
handle_cast(continue, NetState = #net_state{...}) ->
    case progress(NetState) of
        abort -> {noreply, NetState};
        {delta, Mode, Pm} ->
            NetState1 = cns(Mode, NetState),
            NetState2 = handle_trigger(Pm, NetState1),
            continue(self()),  % <-- Recursive continue
            NetState3 = update_stats(NetState2, ...),
            {noreply, NetState3}
    end.
```

**gen_yawl continue loop** (`/Users/sac/cre/src/core/gen_yawl.erl:928-1048`):
```erlang
handle_cast(continue, WrapperState = #wrapper_state{...}) ->
    case progress(NetState0, FireTimeout) of
        abort -> {noreply, WrapperState};
        {delta, Mode, Pm, NewUsrInfo} ->
            NetState1 = cns(Mode, NetState0),
            NetState2 = case NewUsrInfo of
                undefined -> NetState1;
                _ -> NetState1#net_state{usr_info = NewUsrInfo}
            end,
            NetState3 = handle_trigger(Pm, NetState2, NetMod),
            continue(self()),  % <-- Recursive continue
            {noreply, WrapperState{...}}
    end.
```

**Key findings**:
1. Only **gen_yawl** handles the 3-tuple return with `NewUsrInfo`
2. **gen_pnet** ignores the third element, losing state updates
3. Both modules independently call `continue(self())`, creating dual progress loops
4. No coordination between the two loops

**Critical problem**: If a module using `gen_pnet` returns a 3-tuple, the third element (usr_info update) is **silently ignored** by gen_pnet's progress loop.

#### 6. Checkpoint and Recovery Integration

**gen_yawl has checkpoint integration** (`/Users/sac/cre/src/core/gen_yawl.erl:894-900`):
```erlang
case yawl_recovery:maybe_checkpoint(StepCount, CheckpointInterval,
        NetArg, NetState3#net_state.marking, NetState3#net_state.usr_info) of
    {do_checkpoint, SpecId, CaseId, Marking, Data} ->
        _ = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),
        ok;
    ok -> ok
end
```

**Key finding**: gen_yawl captures **both marking and usr_info** in checkpoints.

**gen_pnet has NO checkpoint integration** - it only knows about marking, not usr_info. This means:
- Workflows using gen_pnet directly cannot be properly checkpointed
- Recovery is incomplete (usr_info is lost)
- Replay is impossible

#### 7. API Surface Confusion

The codebase has **conflicting APIs**:

**gen_pnet API** (should be internal only):
- `gen_pnet:start_link/3,4`
- `gen_pnet:step/1`
- `gen_pnet:drain/2`
- `gen_pnet:inject/2`
- `gen_pnet:marking/1`
- `gen_pnet:usr_info/1`

**gen_yawl API** (should be the public API):
- `gen_yawl:start_link/3,4`
- `gen_yawl:step/1`
- `gen_yawl:drain/2`
- `gen_yawl:inject/2`
- `gen_yawl:marking/1`
- `gen_yawl:usr_info/1`

**yawl_execution API** (high-level workflow API):
- `yawl_execution:start_workflow/2,3`
- `yawl_execution:inject_input/2`
- `yawl_execution:execute_step/1`
- `yawl_execution:drain_workflow/2`

**Problem**: `yawl_execution` calls `gen_pnet` directly (line 148, 175), but should call `gen_yawl` to get:
- 3-tuple fire/3 support
- Checkpoint integration
- Cycle detection
- Telemetry

### Key Files

**Core execution modules:**
- `/Users/sac/cre/src/core/gen_yawl.erl` - 1556 lines, wrapper around gen_pnet with 3-tuple support
- `/Users/sac/cre/src/core/gen_pnet.erl` - 1556 lines, base Petri net behavior

**Pattern modules (already using gen_yawl correctly):**
- `/Users/sac/cre/src/patterns/sequence.erl:45-54` - Example of correct 3-tuple fire/3
- `/Users/sac/cre/src/patterns/parallel_split.erl:433-503` - Complex pattern with state updates
- `/Users/sac/cre/src/patterns/structured_loop.erl:390-472` - Loop pattern with usr_info mutations

**Workflow modules (need migration to gen_yawl):**
- `/Users/sac/cre/src/order_fulfillment.erl:40,153` - Uses gen_pnet directly
- `/Users/sac/cre/src/freight_in_transit.erl:34,142` - Uses gen_pnet directly
- `/Users/sac/cre/src/cre_worker.erl:229,249` - Uses gen_pnet directly
- `/Users/sac/cre/src/cre_yawl_exception.erl:41` - Uses gen_pnet directly

**Execution API layer (needs fixing):**
- `/Users/sac/cre/src/wf/yawl_execution.erl:147-175` - Calls gen_pnet instead of gen_yawl
- `/Users/sac/cre/src/wf/yawl_recovery.erl:44` - Documentation shows gen_pnet usage

**Headers and types:**
- `/Users/sac/cre/include/gen_yawl.hrl` - Includes gen_pnet.hrl
- `/Users/sac/cre/include/gen_pnet.hrl` - Defines #net_state{}, #stats{}, #bad_place{}

**Checkpoint and recovery:**
- `/Users/sac/cre/src/wf/yawl_recovery.erl:223-260` - `maybe_checkpoint/5` integrates with gen_yawl's continue loop

## Technical Considerations

### Dependencies

**Internal dependencies:**
- `gen_yawl` depends on `gen_pnet` (creates #net_state{} internally)
- `yawl_recovery` depends on both (checkpoint format)
- `yawl_execution` depends on both (but should only use gen_yawl)

**External dependencies:**
- Mnesia (for checkpoint storage via yawl_recovery)
- Logger (for telemetry)
- YAWL XES logging (yawl_xes)

### Patterns to Follow

**1. 3-tuple fire/3 pattern:**
All pattern modules should follow this pattern:
```erlang
-fire(Transition, Mode, UsrInfo) ->
+fire(Transition, Mode, UsrInfo) ->
     case do_work(UsrInfo) of
-        {ok, Result} -> {produce, #{p_out => [Result]}}
+        {ok, Result, NewUsrInfo} -> {produce, #{p_out => [Result]}, NewUsrInfo}
     end.
```

**2. usr_info mutation in fire/3:**
State updates should happen in fire/3, not in handle_call/handle_cast:
```erlang
% WRONG: State mutation in handle_cast
-handle_cast({update_state, NewState}, NetState) ->
-    {noreply, NetState#net_state{usr_info = NewState}}.

% CORRECT: State mutation in fire/3
-fire(t_update, _Mode, #state{count = C} = State) ->
-    {produce, #{p_next => [token]}, State#state{count = C + 1}}.
```

**3. Centralized execution through gen_yawl:**
All workflow starts should use gen_yawl:
```erlang
% WRONG: Direct gen_pnet start
-gen_pnet:start_link(my_workflow, InitArg, []).

% CORRECT: Start through gen_yawl
-gen_yawl:start_link(my_workflow, InitArg, []).
```

**4. Checkpoint integration:**
Workflows should pass checkpoint_interval in NetArg:
```erlang
gen_yawl:start_link(MyWorkflow, #{spec_id => <<"order">>,
                                 case_id => <<"123">>,
                                 checkpoint_interval => 100}, []).
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Breaking change for gen_pnet modules** | High - 7 workflow modules need migration | Create migration guide; provide shim layer for backwards compatibility |
| **Performance regression from gen_yawl overhead** | Medium - gen_yawl adds cycle detection and checkpoint checks | Benchmark before/after; add option to disable cycle detection for trusted workflows |
| **usr_info scope confusion** | High - usr_info currently process-scoped, needs to be transition-scoped | Document that usr_info is per-transition state, not process state; add type checks |
| **Checkpoint format incompatibility** | Medium - Existing checkpoints may not load after refactor | Version checkpoint format; provide migration tool |
| **Test suite breakage** | High - Many tests use gen_pnet directly | Update test infrastructure; allow gen_pnet usage in test modules only with @doc tag |
| **Hidden state mutations in handle_call/handle_cast** | High - Modules may mutate state outside fire/3 | Add dialyzer warnings; add runtime assertions to detect state changes |
| **Replay correctness** | Critical - State must be exactly reproducible | Add replay tests; verify checkpoint → resume produces same final state |

## Recommended Approach

Based on research findings, here's the recommended refactoring strategy:

### Phase 1: API Surface Cleanup (Low risk)
1. **Fix yawl_execution to call gen_yawl instead of gen_pnet**
   - Update `/Users/sac/cre/src/wf/yawl_execution.erl:148` to call `gen_yawl:start_link`
   - Update line 175 similarly
   - Benefit: All high-level API users get 3-tuple support automatically

2. **Add deprecation notices to gen_pnet public API**
   - Add @doc tags: "Use gen_yawl instead (gen_pnet will become internal)"
   - Add compiler warnings with -warn_deprecated
   - Benefit: Clear signal to developers to migrate

### Phase 2: Pattern Module Migration (Medium risk)
3. **Migrate 7 workflow modules from gen_pnet to gen_yawl**
   - `order_fulfillment.erl`
   - `freight_in_transit.erl`
   - `cre_worker.erl`
   - `cre_yawl_exception.erl`
   - Test modules can stay on gen_pnet with explicit comments

4. **Ensure all fire/3 implementations return 3-tuples**
   - Audit all 51 pattern modules for 2-tuple returns
   - Convert to 3-tuple: `{produce, Map, UsrInfo}` (identity for no state change)
   - Add dialyzer type specs

### Phase 3: State Mutation Centralization (High risk)
5. **Eliminate usr_info mutations in handle_call/handle_cast**
   - Audit all modules for `NetState#net_state{usr_info = ...}` patterns
   - Move state mutations into fire/3 or trigger/3 callbacks
   - Add runtime assertion: usr_info should only change in fire/3

6. **Add guard rails and assertions**
   - Add check in gen_yawl: `if NetState#net_state.usr_info =/= NewUsrInfo -> ok end`
   - Add telemetry event for every usr_info change
   - Add audit log of state mutations

### Phase 4: Internalization (Critical path)
7. **Mark gen_pnet as internal**
   - Move to `src/core/internal/` or rename to `gen_pnet_internal`
   - Remove from public API documentation
   - Update all references to say "internal use only"

8. **Enforce single execution loop**
   - Remove continue loop from gen_pnet (delegating to gen_yawl)
   - Add assertion: only gen_yawl can call continue()
   - Benefit: Single authority on execution flow

### Phase 5: Verification (Critical)
9. **Add checkpoint/replay tests**
   - Test: checkpoint → resume → execution produces same final state as direct execution
   - Test: replay from checkpoint is deterministic
   - Add to CI/CD pipeline

10. **Add integration tests**
    - Test: all 43 workflow patterns can be checkpointed and resumed
    - Test: state mutations are observable in telemetry
    - Test: cycle detection works correctly

## Open Questions

1. **Backwards compatibility strategy**: Should we provide a shim layer that allows old gen_pnet-based workflows to continue working, or force a hard migration?

2. **Performance impact**: gen_yawl adds cycle detection, checkpoint checks, and telemetry. What's the performance overhead? Can it be disabled for trusted workflows?

3. **usr_info scope semantics**: The spec says "usr_info is transition-scoped, not process-scoped". Does this mean:
   - usr_info is immutable between transitions?
   - usr_info can only be updated in fire/3?
   - How does this interact with long-running workflows (hours/days)?

4. **Checkpoint granularity**: The current implementation checkpoints every N steps. Is this sufficient for enterprise use, or do we need event-triggered checkpoints (e.g., after critical transitions)?

5. **Testing strategy**: How do we verify that "every state mutation is representable as (Marking_before, Fire, Marking_after, UsrInfo_after)"? Do we need a formal verification tool?

6. **Documentation updates**: What documentation needs to be updated? API references? Tutorials? Examples?

7. **Rollback plan**: If this refactor causes issues in production, what's the rollback strategy? Can we deploy gen_yawl and gen_pnet side-by-side?
