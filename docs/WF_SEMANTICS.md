# WF Substrate Formal Semantics

## Abstract

This document defines the small-step operational semantics of the WF Substrate bytecode VM. The semantics are presented as reduction rules in the style of Plotkin structural operational semantics (SOS).

## Notation

**Execution State**:
```
S = (π, pc, σ, κ, τ)

where:
  π ∈ Program        = bytecode (opcode list)
  pc ∈ ℕ            = program counter
  σ ∈ State          = machine state (stack, joins, cancellation flags, context)
  κ ∈ Context        = user context (data, tokens, signals)
  τ ∈ TraceLog       = event sequence
```

**Opcodes**:
```
OP ∈ {SEQ_ENTER, SEQ_EXIT, PAR_FORK, PAR_JOIN,
       XOR_CHOOSE, JOIN_WAIT(policy),
       TASK_ENTER(name,f), TASK_EXIT,
       LOOP_BACK(cond), DEFER_RACE,
       CANCEL_SCOPE_ENTER(id), CANCEL_SCOPE_EXIT,
       MI_SPAWN(policy), MI_JOIN(policy),
       EFFECT_YIELD, EFFECT_RESUME,
       HALT, ERROR}
```

**Stack Frame**:
```
Frame = {scope_type, scope_id, locals}

scope_type ∈ {seq, par, xor, join, loop, defer, cancel, mi}
```

---

## Reduction Rules

### 1. Sequence (SEQ_ENTER, SEQ_EXIT)

```
SEQ_ENTER(name)
─────────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒ (π, pc+1, [seq_frame(name)|σ], κ, τ ++ [step(pc, seq_enter)])
```

**Semantics**: Push a sequence frame onto the stack; used to track scope nesting.

```
SEQ_EXIT
─────────────────────────────────────────────────────
(π, pc, [seq_frame(_)|σ], κ, τ) ⇒ (π, pc+1, σ, κ, τ ++ [step(pc, seq_exit)])
```

**Semantics**: Pop the sequence frame.

### 2. Parallel Split (PAR_FORK)

```
PAR_FORK(N)
─────────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒ (π, pc+1, [par_frame(N, 0)|σ], κ', τ ++ [step(pc, par_fork)])

where:
  κ' = fork_tokens(κ, N)   [create N token copies for branches]

[If join counter for this join_id not yet initialized]
```

**Semantics**: Split into N concurrent branches. Fork all tokens. Initialize join counter to 0.

### 3. Synchronization (PAR_JOIN, JOIN_WAIT)

#### 3.1 Join with Policy = ALL

```
JOIN_WAIT(all)
─────────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  if join_counter[pc] < N then
    (π, pc, σ, κ, τ ++ [step(pc, join_wait_stall)])
  else
    (π, pc+1, [par_frame(N,N)|σ], κ, τ ++ [step(pc, par_join_all)])
```

**Semantics**: Wait until all N branches have completed. Completion signals increment join counter. When counter reaches N, proceed.

#### 3.2 Merge (XOR variant)

```
JOIN_WAIT(xor_merge)
─────────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc+1, σ, κ_merged, τ ++ [step(pc, join_xor_merge)])

where:
  κ_merged = merge_tokens_xor(κ)  [take first non-empty token; discard others]
```

**Semantics**: Merge takes the first available branch output. Discards other branches' tokens.

#### 3.3 Synchronizing Merge

```
JOIN_WAIT(sync_merge)
─────────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc+1, σ, κ_synced, τ ++ [step(pc, join_sync_merge)])

where:
  κ_synced = synchronized_merge(κ, policy_variant)
  [coordinates with explicit synchronization signal or internal ordering]
```

**Semantics**: Synchronizing merge waits for branches to signal readiness, then merges in deterministic order.

#### 3.4 First-N Join

```
JOIN_WAIT({first_n, N})
─────────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  if count_completed_branches() >= N then
    (π, pc+1, σ, κ_first_n, τ ++ [step(pc, join_first_n)])
  else
    (π, pc, σ, κ, τ ++ [step(pc, join_first_n_stall)])

where:
  κ_first_n = take_first_n_tokens(κ, N)
```

**Semantics**: Proceed as soon as N branches complete; cancel remaining.

### 4. Exclusive Choice (XOR_CHOOSE)

```
XOR_CHOOSE(branches)
─────────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc_branch_i, [xor_frame(branches, i)|σ], κ, τ ++ [step(pc, xor_choose(i))])

where:
  i = deterministic_select(branches, κ)  [under det. scheduler]
  pc_branch_i = program_offset(branch_i)
```

**Semantics**: Select one branch deterministically (e.g., first enabled). Cancel alternatives (or never spawn them). Push xor_frame to mark cancellation scope.

### 5. Loop (LOOP_BACK)

```
LOOP_BACK(condition)
───────────────────────────────────────────────────
(π, pc, [loop_frame(iter)|σ], κ, τ) ⇒
  if condition(κ, iter) then
    (π, pc_loop_start, [loop_frame(iter+1)|σ], κ, τ ++ [step(pc, loop_back(iter))])
  else
    (π, pc+1, σ, κ, τ ++ [step(pc, loop_exit)])

where:
  pc_loop_start = loop_body_start  [jump back to loop body]
```

**Semantics**: Test loop condition. If true, increment iteration counter and jump back. If false, exit loop.

### 6. Deferred Choice (DEFER_RACE)

```
DEFER_RACE(branches)
───────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc', [defer_frame(branches, pending)|σ], κ, τ ++ [step(pc, defer_race_start)])

where:
  pc' = next instruction after DEFER_RACE  [suspend execution]
  branches are held in pending state waiting for first external event or internal signal
```

**Semantics**: Wait for external signal or first branch to enable. Once an event occurs, proceed with that branch; cancel alternatives.

### 7. Task Invocation (TASK_ENTER, TASK_CALL, TASK_EXIT)

```
TASK_ENTER(name, f)
───────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  if ctx_has_signal(κ, name) then
    (π, pc+2, σ, κ', τ ++ [step(pc, task_enter_signal)])
  else
    (π, pc+1, [task_frame(name, f)|σ], κ, τ ++ [step(pc, task_enter)])
```

**Semantics**: Check for external signal matching task name. If found, skip to TASK_EXIT. Otherwise, set up task frame and invoke function on next step.

```
TASK_CALL(f)
───────────────────────────────────────────────────
(π, pc, [task_frame(name, f)|σ], κ, τ) ⇒
  match f(κ) with:
    | {ok, κ'} ⇒
        (π, pc+1, σ, κ', τ ++ [step(pc, task_ok)])
    | {error, reason} ⇒
        (π, ERROR, σ, κ, τ ++ [step(pc, task_error(reason))])
    | {effect, spec, cont_ctx} ⇒
        (π, pc+1, σ, κ_yield, τ ++ [step(pc, effect_yield(spec))])
        where κ_yield = make_effect_yield(cont_ctx)
```

**Semantics**: Invoke the task function. On success, update context and advance. On error, jump to ERROR. On effect, yield and suspend.

```
TASK_EXIT
───────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc+1, σ, κ, τ ++ [step(pc, task_exit)])
```

**Semantics**: Cleanup after task (pop task frame, etc.).

### 8. Effect Handling (EFFECT_YIELD, EFFECT_RESUME)

```
EFFECT_YIELD(spec)
───────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  {yield, effect_spec(spec), (π, pc, σ, κ, τ)}
```

**Semantics**: Suspend execution and request effect handling externally. Return the execution state for resumption.

```
EFFECT_RESUME(result)
───────────────────────────────────────────────────
(π, pc_after_effect, σ, κ, τ) ⇒
  (π, pc_after_effect + 1, σ, κ_with_result, τ ++ [step(pc_after_effect, effect_resume(result))])
```

**Semantics**: Resume execution after effect completes. Update context with result and continue.

### 9. Cancellation (CANCEL_SCOPE_ENTER, CANCEL_SCOPE_EXIT)

```
CANCEL_SCOPE_ENTER(scope_id, sub_pc)
───────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc+1, [cancel_frame(scope_id, sub_pc)|σ], κ, τ ++ [step(pc, cancel_enter)])
```

**Semantics**: Enter a cancellation scope. Push a cancel frame that marks the region and exit PC.

```
CANCEL_SCOPE_EXIT(scope_id)  with cancel_flag(scope_id) = true
───────────────────────────────────────────────────
(π, pc, [cancel_frame(scope_id, exit_pc)|σ], κ, τ) ⇒
  (π, exit_pc, σ, κ_cancel, τ ++ [step(pc, cancel_exit)])

where:
  κ_cancel = apply_cancellation_semantics(κ, scope_id)
```

**Semantics**: If the scope was cancelled, jump to the exit PC. Otherwise proceed normally.

### 10. Multiple Instances (MI_SPAWN, MI_JOIN)

```
MI_SPAWN(policy = {fixed, N})
───────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc+1, [mi_frame({fixed, N}, 0)|σ], κ', τ ++ [step(pc, mi_spawn_fixed(N))])

where:
  κ' = spawn_instances(κ, N)  [fork user context N times]
```

**Semantics**: Spawn N instances of the following sub-process.

```
MI_SPAWN(policy = {dynamic, collector_fn})
───────────────────────────────────────────────────
(π, pc, σ, κ, τ) ⇒
  (π, pc+1, [mi_frame({dynamic, collector_fn}, 0)|σ], κ', τ ++ [step(pc, mi_spawn_dynamic)])

where:
  κ' = spawn_dynamic_instances(κ, collector_fn)  [spawn until collector_fn returns done]
```

**Semantics**: Dynamically spawn instances as long as collector function returns {next, data}.

```
MI_JOIN(policy = all)
───────────────────────────────────────────────────
(π, pc, [mi_frame(policy, count)|σ], κ, τ) ⇒
  if count == N then
    (π, pc+1, σ, κ_joined, τ ++ [step(pc, mi_join_all)])
  else
    (π, pc, [mi_frame(policy, count+1)|σ], κ, τ)
```

**Semantics**: Wait for all instances to complete.

---

## Invariants

### I1: Token Conservation

For any reachable state S, let T(S) = {tokens in σ.ctx ∪ in-flight in effects}.
Then |T(S)| = |T(S₀)| for all reachable S from initial state S₀.

**Proof sketch**: Tokens are created only at PAR_FORK (incremented join counter), consumed at JOIN_WAIT (decremented counter). All paths preserve conservation.

### I2: No Invalid State Transitions

The reduction relation is total on valid states. Invalid states (e.g., malformed stack, negative join counters) are unreachable given correct compilation.

### I3: Determinism Under Deterministic Scheduler

Given fixed scheduler policy (e.g., always pick first enabled branch in XOR_CHOOSE), two executions of the same case with the same initial context will produce identical trace sequences.

### I4: Cancellation Propagation

If cancel_flag(scope_id) = true is set during execution, no further instruction within that scope (until CANCEL_SCOPE_EXIT) will execute; control jumps to exit PC.

### I5: Effect Idempotence

If an effect yields multiple times with the same receipt_id, applying the same effect twice produces the same result. Effects are resumable exactly once.

---

## Determinism & Scheduling

### Deterministic Scheduler

Under deterministic policy, non-deterministic choices (XOR_CHOOSE, DEFER_RACE) are made in a fixed order:
- **XOR_CHOOSE**: Always pick the first branch in the list that is enabled
- **DEFER_RACE**: Always pick the first signal in the inbound queue

**Replay**: Given a trace log of choices, the same execution can be replayed deterministically.

### Nondeterministic Scheduler

Allows any enabled branch to be chosen. Used for testing and exploration. Choices are logged; replay uses the log to reproduce.

---

## Soundness Properties

### Property S1: No Deadlock in Bounded Execution

**Theorem**: For any acyclic pattern term, the bytecode either:
1. Reaches HALT (successful completion), or
2. Reaches ERROR (task failure), or
3. Never deadlocks (all pending join points have a completion path)

**Validation**: Bounded model checker verifies up to depth D and token bound K.

### Property S2: Proper Completion

**Theorem**: For any pattern term with no explicit cycles, if execution reaches HALT, all sub-processes have been joined (no orphaned branches).

**Validation**: Track all tokens; verify final state has empty join counters.

### Property S3: Cancellation Terminates

**Theorem**: If cancel_flag is set on a scope, within O(scope_size) reduction steps, all activities in that scope will have halted.

**Validation**: Measure reduction steps between cancel and final exit PC.

---

## Examples

### Example 1: Sequence of Two Tasks

```
Pattern: seq(task(a, f), task(b, g))

Bytecode:
  0: SEQ_ENTER(a)
  1: TASK_ENTER(a, f)
  2: TASK_CALL(f)
  3: TASK_EXIT
  4: SEQ_EXIT
  5: SEQ_ENTER(b)
  6: TASK_ENTER(b, g)
  7: TASK_CALL(g)
  8: TASK_EXIT
  9: SEQ_EXIT
 10: HALT

Trace (successful):
  step(0, seq_enter) -> step(1, task_enter) -> step(2, task_ok)
    -> step(3, task_exit) -> step(4, seq_exit)
    -> step(5, seq_enter) -> step(6, task_enter) -> step(7, task_ok)
    -> step(8, task_exit) -> step(9, seq_exit) -> step(10, halt)
```

### Example 2: Parallel with All Join

```
Pattern: par([task(a, f), task(b, g)])

Bytecode:
  0: PAR_FORK(2)
  1: TASK_ENTER(a, f)       [branch 0]
  2: TASK_EXIT
  3: BRANCH_END
  4: TASK_ENTER(b, g)       [branch 1, offset from 0]
  5: TASK_EXIT
  6: BRANCH_END
  7: JOIN_WAIT(all)
  8: HALT

Trace:
  step(0, par_fork)
    -> step(1, task_enter) -> step(2, task_ok) -> step(3, branch_end)
    -> step(4, task_enter) -> step(5, task_ok) -> step(6, branch_end)
    -> step(7, join_wait_stall)  [join counter goes 1/2]
    -> step(7, join_wait_stall)  [join counter goes 2/2]
    -> step(8, join_all) -> step(9, halt)
```

---

**Document Version**: 1.0
**Status**: Semantics finalized, ready for implementation
