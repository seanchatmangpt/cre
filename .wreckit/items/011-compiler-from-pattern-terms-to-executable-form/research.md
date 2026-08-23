# Research: Compiler from pattern terms to executable form

**Date**: 2025-01-21
**Item**: 011-compiler-from-pattern-terms-to-executable-form

## Research Question
Pattern terms are convenient for authoring but inefficient for runtime execution. Need compilation strategy that produces efficient executable forms without interpretation overhead.

**Motivation:** Achieves performance goals by eliminating runtime dispatch overhead, enables optimization opportunities, produces forms suitable for deterministic replay and efficient cancellation.

**Technical constraints:**
- No runtime 'case NodeType of' dispatch in hot loops
- Must support Strategy S1 (bytecode VM) or S2 (continuation network)
- Opcodes: SEQ_ENTER, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_BACK, CANCEL_SCOPE, MI_SPAWN, EFFECT_YIELD
- Or continuation network of closures/frames
- Must support efficient cancellation propagation
- Must support join policies without graph scanning

**Signals:** priority: critical, urgency: Required before any pattern execution is possible

## Summary

The CRE codebase currently uses **Petri net interpretation** as its execution model through `gen_pnet` and `gen_yawl` behaviors. Pattern terms are authored as Erlang modules that implement callback functions (`place_lst/0`, `trsn_lst/0`, `fire/3`, etc.), which are then **interpreted at runtime** through a generic progress loop that performs dynamic dispatch on transition types.

**Current architecture (interpretive):**
- Each pattern is a separate gen_yawl behavior module
- Runtime progress loop enumerates enabled transitions
- For each transition: calls `is_enabled/3`, then `fire/3` with dynamic dispatch
- Token manipulation through marking algebra (multiset operations)
- No pre-compilation to optimized executable form

**The compiler needed (Item 011):**
A transformation layer that converts pattern term ASTs (from Item 010) into efficient executable representations that eliminate runtime dispatch. Two strategies are specified:

**Strategy S1 (Bytecode VM):** Compile patterns to linear bytecode sequences with opcodes like SEQ_ENTER, PAR_FORK, JOIN_WAIT, etc. A tight VM loop executes these opcodes without node type dispatch.

**Strategy S2 (Continuation Network):** Compile patterns to a network of closures/frames that directly encode execution flow as function calls, eliminating the interpreter loop entirely.

This compiler is **blocked on Item 010** (Pattern term algebra and AST), which provides the source representation. The compiler is also a dependency for Item 012 (Reducer/executor hot loop), Item 013 (Deterministic scheduling), and Item 021 (Core pattern implementations).

## Current State Analysis

### Existing Implementation

#### 1. Petri Net Interpretation Model

**Core execution behaviors:**

**gen_pnet** (`/Users/sac/cre/src/core/gen_pnet.erl`, 1556 lines):
- Base Petri net OTP behavior
- Implements generic progress loop with dynamic dispatch
- Callback-based: modules define places, transitions, enablement, firing
- Progress loop (lines 706-721):
  ```erlang
  handle_cast(continue, NetState = #net_state{...}) ->
      case progress(NetState) of
          abort -> {noreply, NetState};
          {delta, Mode, Pm} ->
              NetState1 = cns(Mode, NetState),
              NetState2 = handle_trigger(Pm, NetState1),
              continue(self()),  % Recursive continue
              {noreply, NetState2}
      end.
  ```
- Runtime dispatch: for each enabled transition, calls module's `fire/3` callback
- No pre-compilation or optimization

**gen_yawl** (`/Users/sac/cre/src/core/gen_yawl.erl`):
- Wrapper around gen_pnet with enhanced fire/3 support
- Allows 3-tuple returns: `{produce, Map, NewUsrInfo}`
- Adds cycle detection, checkpoint integration, telemetry
- Still interpretive - no compilation to bytecode or continuations

#### 2. Pattern Module Structure

**All 43 workflow patterns** in `/Users/sac/cre/src/patterns/` follow the same structure:

Example: `sequence.erl` (lines 1-67):
```erlang
-module(sequence).
-behaviour(gen_yawl).

place_lst() -> [p_start, p_task1, p_task2, p_end].
trsn_lst() -> [t_start, t_complete1, t_complete2, t_finish].
init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_complete1) -> [p_task1];
preset(t_complete2) -> [p_task2];
preset(t_finish) -> [p_end];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_task1 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_task2 => [token]}, UsrInfo};
%% ... more fire clauses
```

**Key observation:** Each pattern is a **hand-written module**, not a compiled term. The "pattern term algebra" (Item 010) doesn't exist yet.

#### 3. YAWL Compiler (Existing but Different Purpose)

**yawl_compile** (`/Users/sac/cre/src/core/yawl_compile.erl`):
- Compiles YAWL XML specifications to gen_pnet modules
- Generates Erlang source code for Petri net structures
- **Not a bytecode compiler** - generates callback-based modules
- Generates standard gen_pnet behavior implementations

Example compilation flow:
1. Parse YAWL XML with `wf_spec:from_xml/1`
2. Extract tasks, flows, conditions
3. Generate places for tasks and conditions
4. Generate transitions with presets
5. Generate fire/3 clauses for each transition
6. Output: Erlang module source code

**This is source-to-source compilation**, not to bytecode or continuations.

#### 4. Pattern Registry

**yawl_pattern_registry** (`/Users/sac/cre/src/core/yawl_pattern_registry.erl`):
- Maps pattern macro names to module names
- Example: `<<"P1_Sequence">> -> sequence`
- Used for pattern lookup, not compilation
- 43 patterns registered (WCP-01 through WCP-43)

**No compilation or transformation occurs** - just static name mapping.

### Key Files

**Core execution (interpretive model):**
- `/Users/sac/cre/src/core/gen_pnet.erl` - Base Petri net behavior, progress loop with runtime dispatch
- `/Users/sac/cre/src/core/gen_yawl.erl` - Wrapper with 3-tuple fire/3 support, checkpoint integration
- `/Users/sac/cre/src/wf/yawl_execution.erl` - High-level workflow execution API

**Pattern implementations (gen_yawl behaviors):**
- `/Users/sac/cre/src/patterns/sequence.erl:1-67` - Simple sequence pattern
- `/Users/sac/cre/src/patterns/parallel_split.erl:1-500+` - Parallel split with complex join logic
- `/Users/sac/cre/src/patterns/structured_loop.erl:390-472` - Loop pattern with state updates in fire/3
- All 43 patterns in `/Users/sac/cre/src/patterns/` directory

**Compilation infrastructure (existing but different purpose):**
- `/Users/sac/cre/src/core/yawl_compile.erl:1-1000+` - YAWL XML to gen_pnet module compiler (source-to-source)
- `/Users/sac/cre/src/core/yawl_pattern_registry.erl:1-100` - Pattern name to module mapping

**Architecture documentation:**
- `/Users/sac/cre/docs/ARCHITECTURE.md:1-300+` - Joe Armstrong design philosophy, component layers

**Related items (dependencies/consumers):**
- `/Users/sac/cre/.wreckit/items/010-pattern-term-algebra-and-ast/item.json` - Pattern term algebra (BLOCKER)
- `/Users/sac/cre/.wreckit/items/012-reducerexecutor-hot-loop/item.json` - Hot loop execution engine
- `/Users/sac/cre/.wreckit/items/013-deterministic-scheduling-with-replay-support/item.json` - Deterministic scheduling
- `/Users/sac/cre/.wreckit/items/021-core-control-flow-pattern-implementations/item.json` - Pattern implementations

## Technical Considerations

### Dependencies

**Internal dependencies (blocks this item):**
- **Item 010: Pattern term algebra and AST** - CRITICAL BLOCKER
  - Need formal pattern term representation before compilation
  - Need AST structure for compiler to transform
  - Status: "idea" state, error: "Agent failed with exit code 1"
  - Must define closed algebra of pattern constructors
  - Must distinguish kernel basis patterns from derived patterns

**Internal dependencies (blocked by this item):**
- **Item 012: Reducer/executor hot loop** - Needs compiled bytecode or continuations to execute
- **Item 013: Deterministic scheduling with replay support** - Needs deterministic execution form
- **Item 021: Core control flow pattern implementations** - Needs executable pattern semantics

**Existing codebase dependencies:**
- `gen_pnet` and `gen_yawl` behaviors - current execution model
- `pnet_marking`, `pnet_mode`, `pnet_receipt` - marking algebra utilities
- `yawl_compile` - compilation infrastructure (can reuse patterns)
- Pattern registry (`yawl_pattern_registry`) - pattern metadata

### Patterns to Follow

**1. Pure functional compilation (Joe Armstrong design):**
From `yawl_compile.erl:4-8`:
```erlang
%% **Joe Armstrong Design: Pure Helper Module (Stateless)**
%% This module provides pure functional code generation. No state is maintained -
%% all compilation functions are stateless transformations.
```

The compiler should be a **pure functional transformation**:
- Input: Pattern term AST (from Item 010)
- Output: Bytecode sequence OR continuation network
- No side effects during compilation
- Deterministic compilation

**2. Separation of compilation and execution:**
Current pattern: `yawl_compile` generates source, `gen_pnet` executes it.
New pattern needed: Compiler generates executable form, Item 012 executes it.

**3. OTP conventions:**
- Use standard Erlang/OTP patterns
- gen_server for compilation service (if needed)
- Type specs with -spec attributes
- Proper error handling with `{ok, Result} | {error, Reason}` tuples

**4. Pattern module structure:**
Current patterns follow gen_yawl behavior. After compilation, patterns should be executable bytecode or continuations, but the **authoring surface** remains pattern terms.

**5. Checkpoint and replay integration:**
From research on Item 001:
- Checkpoints capture marking + usr_info
- Replay requires deterministic execution
- Compiled form must support checkpoint restoration

### Existing Conventions

**Naming conventions:**
- Pattern modules: lowercase atoms (`sequence`, `parallel_split`)
- Transition atoms: `t_<name>` (e.g., `t_start`, `t_complete1`)
- Place atoms: `p_<name>` (e.g., `p_start`, `p_task1`)
- Opcode names: uppercase with underscores (from spec: `SEQ_ENTER`, `PAR_FORK`)

**Error handling:**
- Compilation errors: `{error, Reason}`
- Runtime errors: `abort` in fire/3, gen_server crashes
- Validation errors: `{error, {validation, Reason}}`

**Type specifications:**
- Use `-spec` attributes for all public functions
- Use `-type` for exported types
- Use `-opaque` for hidden implementation types

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Item 010 (pattern algebra) not complete** | Critical - No source representation to compile | Parallel development: Define minimal pattern term structure first, implement compiler incrementally |
| **Strategy choice uncertainty (S1 vs S2)** | High - Wrong choice requires complete rewrite | Implement both strategies with abstraction layer; benchmark to choose; support both via compilation flag |
| **Performance of bytecode VM vs continuations** | High - May not meet performance goals | Prototype both strategies early; microbenchmark with representative patterns; choose based on actual measurements |
| **Cancellation propagation in compiled form** | High - Cancellation must efficiently unwind | Design cancellation into bytecode opcodes (CANCEL_SCOPE) or continuation traps; test with deep nesting |
| **Join policy implementation without graph scanning** | High - Current model scans marking for join conditions | Compile join conditions into explicit opcodes; use reference counting or state machines for complex joins |
| **Deterministic replay correctness** | Critical - Compiled form must replay exactly | Record nondeterministic choices in trace; ensure bytecode VM is deterministic; add replay validation tests |
| **Integration with existing gen_pnet/gen_yawl** | Medium - May break existing workflows | Provide migration path; support both interpreted and compiled workflows; add feature flag to switch |
| **Checkpoint format compatibility** | Medium - Existing checkpoints must load | Version checkpoint format; provide migration tool; maintain marking+usr_info structure |
| **Testing complexity** | High - Need to verify compilation correctness | Add compiler verification tests; property-based testing with QuickCheck; compare interpreted vs compiled results |
| **Compilation performance** | Medium - Large workflows may compile slowly | Lazy compilation; caching; parallel compilation for independent patterns |

## Recommended Approach

Based on research findings, here's the recommended implementation strategy:

### Phase 0: Unblock on Item 010 (Pattern Term Algebra)

**Critical path** - Cannot proceed without pattern term representation:

1. **Define minimal pattern term AST:**
   ```erlang
   %% Kernel basis patterns (primitives)
   -type pattern_term() ::
       {sequence, [pattern_term()]} |
       {parallel_split, [pattern_term()]} |
       {exclusive_choice, [{condition(), pattern_term()}]} |
       {structured_loop, pattern_term(), condition_fun()} |
       %% ... other kernel patterns
       {task, task_spec()}.
   ```

2. **Implement smart constructors** with invariant checking
3. **Add derived pattern macros** as library functions
4. **Create pattern parser** from YAML/JSON to terms

**Estimated effort:** 2-3 weeks (highly coupled with Item 010)

### Phase 1: Compiler Design and Strategy Selection

**Objective:** Design compiler architecture and choose execution strategy.

1. **Prototype Strategy S1 (Bytecode VM):**
   - Define opcode set: `SEQ_ENTER`, `PAR_FORK`, `JOIN_WAIT`, `XOR_CHOOSE`, `LOOP_BACK`, `CANCEL_SCOPE`, `MI_SPAWN`, `EFFECT_YIELD`
   - Design bytecode format: linear instruction sequence with operands
   - Implement prototype VM loop (tight loop with pattern matching on opcodes)
   - Compile simple patterns (sequence, parallel) to bytecode
   - Benchmark against current gen_pnet interpretation

2. **Prototype Strategy S2 (Continuation Network):**
   - Design continuation frame structure
   - Compile patterns to closure networks
   - Implement trampoline executor for stackless execution
   - Benchmark against Strategy S1

3. **Decision point:** Choose S1 or S2 based on:
   - Performance (microbenchmarks)
   - Code clarity
   - Cancellation support
   - Debuggability
   - Deterministic replay support

**Estimated effort:** 3-4 weeks

### Phase 2: Core Compiler Implementation

**Objective:** Implement compiler from pattern terms to chosen executable form.

1. **Compilation pipeline:**
   ```
   Pattern Term AST
       ↓
   Validation (well-formedness checks)
       ↓
   Optimization (constant folding, dead code elimination)
       ↓
   Code Generation (bytecode OR continuations)
       ↓
   Executable Form
   ```

2. **Key modules to create:**
   - `pattern_compile` - Main compilation API
   - `pattern_validate` - AST validation (well-formedness)
   - `pattern_optimize` - Compiler optimizations
   - `pattern_codegen` - Code generation (strategy-specific)
   - `pattern_bytecode` - Bytecode utilities (if S1 chosen)
   - `pattern_continuation` - Continuation utilities (if S2 chosen)

3. **Compilation API:**
   ```erlang
   %% Compile pattern term to executable form
   -spec compile(pattern_term()) ->
       {ok, executable()} | {error, compile_error()}.

   %% Compile with options
   -spec compile(pattern_term(), compile_options()) ->
       {ok, executable()} | {error, compile_error()}.

   -type compile_options() :: #{
       strategy => bytecode | continuation,
       optimize => boolean(),
       debug_info => boolean()
   }.
   ```

**Estimated effort:** 4-6 weeks

### Phase 3: Executor Integration (with Item 012)

**Objective:** Integrate compiled form with executor hot loop.

1. **Define executor interface:**
   - Item 012 provides the hot loop (tight VM loop OR continuation trampoline)
   - Compiler generates executable form compatible with executor
   - Support quanta-based execution (N reductions per tick)

2. **Cancellation support:**
   - Compile `CANCEL_SCOPE` opcodes into bytecode
   - OR: Compile continuation traps for cancellation
   - Ensure efficient cancellation propagation (no graph scanning)

3. **Effect integration:**
   - `EFFECT_YIELD` opcode for effect handler calls
   - Resumption from effect handlers
   - Integration with Item 016 (Effect system)

4. **Join policies:**
   - Compile join conditions into explicit opcodes
   - Use reference counting or state machines
   - Avoid runtime marking scans

**Estimated effort:** 3-4 weeks (parallel with Item 012)

### Phase 4: Deterministic Replay Support (with Item 013)

**Objective:** Ensure compiled form supports deterministic replay.

1. **Record nondeterministic choices:**
   - Branch selection in XOR_CHOOSE
   - Effect ordering in MI_SPAWN
   - Transition selection in concurrent sections

2. **Replay mode:**
   - Load recorded choices
   - Execute deterministically without deviation
   - Verify replay matches original execution

3. **Trace integration:**
   - Emit trace events per reduction
   - Include opcode/continuation in trace
   - Support trace validation

**Estimated effort:** 2-3 weeks (parallel with Item 013)

### Phase 5: Pattern Implementation Migration (with Item 021)

**Objective:** Implement kernel patterns using compiled execution.

1. **Implement kernel patterns as pattern terms:**
   - Sequence, parallel split, exclusive choice, etc.
   - Use smart constructors from Item 010

2. **Compile patterns to executable form:**
   - Test compilation correctness
   - Verify semantic equivalence with hand-written modules

3. **Benchmark:**
   - Compare performance: compiled vs interpreted
   - Measure overhead reduction
   - Validate performance goals

**Estimated effort:** 4-6 weeks (parallel with Item 021)

### Phase 6: Integration and Testing

**Objective:** Full integration with existing CRE infrastructure.

1. **Migration path:**
   - Support both interpreted (gen_yawl) and compiled workflows
   - Feature flag to switch between modes
   - Documentation for migration

2. **Checkpoint compatibility:**
   - Ensure compiled workflows produce same checkpoint format
   - Test checkpoint restore from interpreted to compiled
   - Provide migration tool if needed

3. **Comprehensive testing:**
   - Unit tests for compiler (each transformation)
   - Property-based tests (QuickCheck)
   - Integration tests (end-to-end workflows)
   - Performance benchmarks (before/after)

4. **Documentation:**
   - Compilation guide
   - Opcode reference (if S1)
   - Continuation semantics (if S2)
   - Migration guide from gen_yawl to compiled

**Estimated effort:** 3-4 weeks

## Open Questions

1. **Pattern term algebra availability:** Item 010 is in "idea" state with previous agent failure. What is the timeline for completing the pattern term algebra? Can we proceed with a minimal subset?

2. **Strategy selection (S1 vs S2):** What are the criteria for choosing bytecode VM vs continuation network? Should we implement both and benchmark, or decide based on theoretical analysis?

3. **Performance requirements:** What are the specific performance goals? What is the acceptable overhead reduction? How much faster must compiled execution be compared to interpretation?

4. **Compilation granularity:** Should we compile entire workflows to a single bytecode sequence, or compile individual patterns and link them? How do we handle dynamic pattern composition?

5. **Debugging and introspection:** How do we debug compiled workflows? Can we map bytecode locations back to source pattern terms? What debug info should be compiled in?

6. **Hot code reload:** How does hot code reload work with compiled workflows? Can we reload compiled code without restarting workflows?

7. **Interop with existing workflows:** How do compiled workflows interact with existing gen_yawl workflows? Can we mix interpreted and compiled patterns in the same workflow?

8. **Cancellation semantics:** What are the exact cancellation semantics? How do we ensure cancellation propagates correctly in compiled form without scanning the workflow graph?

9. **Join policy implementation:** How do we compile complex join policies (e.g., n-out-of-m, generalized AND join) without runtime marking scans? What compilation techniques apply?

10. **Effect system integration:** How do compiled patterns interact with effect handlers (Item 016)? What is the compilation strategy for EFFECT_YIELD and effect resumption?

11. **Determinism guarantees:** How do we ensure the bytecode VM or continuation executor is deterministic? What sources of nondeterminism must be controlled?

12. **Testing strategy:** How do we verify compilation correctness? Do we need formal verification, or is testing sufficient? What properties should we test with QuickCheck?

## Recommendations

1. **Immediate action:** Coordinate with Item 010 team to define minimal pattern term AST. This is the critical blocker.

2. **Prototype early:** Build minimal prototypes of both S1 (bytecode VM) and S2 (continuation network) with 2-3 simple patterns. Benchmark to make data-driven strategy decision.

3. **Incremental compilation:** Start with kernel basis patterns only, add derived patterns later. Allows faster validation of compiler design.

4. **Maintain compatibility:** Keep gen_yawl interpretation as fallback. Don't break existing workflows. Use feature flags to enable compilation.

5. **Design for debugging:** Include source mapping in compiled form. Emit detailed trace events. Support single-stepping through bytecode/continuations.

6. **Test extensively:** Use property-based testing to verify compiled ≡ interpreted. Add replay tests to verify determinism. Benchmark continuously.

7. **Document everything:** Pattern term syntax, compilation process, opcode semantics (if S1), continuation semantics (if S2), migration guide.

8. **Plan for migration:** Existing workflows need migration path. Provide tools to compile gen_yawl modules to pattern terms (reverse compilation).

9. **Coordinate with dependent items:** Items 012 (executor), 013 (scheduling), 021 (patterns) depend on this compiler. Ensure API contracts are clear.

10. **Consider hybrid approach:** If S1 and S2 both have merits, design abstraction layer that supports both. Strategy selection per-workflow or per-pattern.
