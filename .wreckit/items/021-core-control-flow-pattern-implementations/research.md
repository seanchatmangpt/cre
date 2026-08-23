# Research: Core control-flow pattern implementations

**Date**: 2025-01-11
**Item**: 021-core-control-flow-pattern-implementations

## Research Question
Need complete implementation of fundamental workflow control-flow patterns with correct semantics, efficient execution, and comprehensive test coverage.

**Motivation:** Provides essential workflow building blocks, ensures pattern coverage from workflow patterns canon, enables users to express common workflow structures, validates that the substrate works correctly.

**Success criteria:**
- All listed patterns implemented and tested
- Each pattern has executable semantics
- Tests verify correctness
- Documentation mapping patterns to terms

**Technical constraints:**
- Implement as kernel or derived patterns
- Include property-based tests where applicable
- Document mapping in docs/PATTERNS.md
- Scaffolding for remaining patterns with TODO stubs

**Signals:** priority: critical, urgency: Primary deliverable - defines substrate capabilities

## Summary

The CRE codebase already has **comprehensive implementation** of all 43 YAWL workflow patterns as `gen_yawl` behavior modules. The patterns are well-structured, tested, and documented. However, this item appears to be asking for a **fundamental re-architecture** from the current Petri-net-based implementation to a **pattern-term algebra with compiled executable forms**, as outlined in `IDEAS.md`.

**Key Finding:** There are TWO different architectural visions:
1. **Current Implementation** (Completed): All 43 patterns implemented as `gen_yawl` behaviors compiling to Petri nets
2. **Requested Implementation** (From IDEAS.md): Pattern-term algebra with direct compilation to bytecode/continuations, avoiding "workflow as data interpreted by engine"

The current system is production-ready but follows the "Petri net compilation" approach that `IDEAS.md` explicitly argues against. This item likely requires implementing the new architecture described in `IDEAS.md` sections 0-2.

## Current State Analysis

### Existing Implementation

**All 43 YAWL patterns are implemented** in `/Users/sac/cre/src/patterns/` as `gen_yawl` behavior modules:

- **Basic Control Flow (WCP 1-6):**
  - `sequence.erl` (WCP-01) - Sequential task execution
  - `parallel_split.erl` (WCP-02) - AND-split for parallel branches
  - `synchronization.erl` (WCP-03) - AND-join waiting for all branches
  - `exclusive_choice.erl` (WCP-04) - XOR-split with branch selection
  - `simple_merge.erl` (WCP-05) - XOR-merge for exclusive branches
  - `multiple_choice.erl` (WCP-06) - OR-split for multiple branches

- **Advanced Synchronization (WCP 7-10):**
  - `structured_sync_merge.erl` (WCP-07) - Structured OR-join
  - `multiple_merge.erl` (WCP-08) - Unstructured OR-join
  - `discriminator.erl` (WCP-09) - First-completion trigger
  - `arbitrary_cycles.erl` (WCP-10) - Loop/iteration support

- **Multiple Instances (WCP 11-17):**
  - `implicit_termination.erl` (WCP-11)
  - `multiple_instances_sync.erl` (WCP 12-15) - Static/dynamic instances
  - `deferred_choice.erl` (WCP-16)
  - `interleaved_routing.erl` (WCP-17)

- **State-Based Patterns (WCP 18-20):**
  - `milestone.erl` (WCP-18)
  - `cancel_activity.erl` (WCP-19)
  - `cancel_case.erl` (WCP-20)

- **Extended Control Flow (WCP 21-28):**
  - `structured_loop.erl` (WCP-21)
  - `recursion.erl` (WCP-22)
  - `transient_trigger.erl` (WCP-23)
  - `persistent_trigger.erl` (WCP-24)
  - `cancel_region.erl` (WCP-25)
  - Additional cancel/partial join patterns (WCP 26-28)

- **Data Flow Patterns (WDP 1-5):**
  - `param_pass.erl`, `data_transform.erl`, `data_distribute.erl`
  - `data_accumulate.erl`, `data_visibility.erl`

- **Resource Patterns (WRP 1-5):**
  - `direct_resource_creation.erl`, `resource_allocation.erl`
  - `role_based_allocation.erl`, etc.

- **Exception Handling (WHP 1-5):**
  - Integrated into pattern implementations

**Pattern Registry**: `/Users/sac/cre/src/core/yawl_pattern_registry.erl` maps all 43 pattern IDs to their module implementations.

### Key Files

**Core Runtime:**
- `src/core/gen_yawl.erl:24-194` - OTP behavior wrapper around gen_pnet with enhanced fire/3 supporting 3-tuple returns for automatic usr_info updates
- `src/core/gen_pnet.erl` - Core Petri net runtime (mentioned but not explored)

**Pattern Implementations:**
- `src/patterns/sequence.erl:1-67` - Minimal gen_yawl implementation with place_lst/0, trsn_lst/0, preset/1, fire/3 callbacks
- `src/patterns/parallel_split.erl:1-787` - Comprehensive implementation with XES logging, state management, branch distribution
- `src/patterns/exclusive_choice.erl:1-596` - Full-featured with lib_combin:pick_from for nondeterministic branch selection, logging
- `src/patterns/synchronization.erl` - AND-join pattern
- `src/patterns/multiple_choice.erl` - OR-split pattern
- All other pattern modules follow consistent structure

**Documentation:**
- `docs/43_PATTERNS_COMPLETE.md:1-190` - Claims all 43 patterns implemented and tested
- `docs/YAWL_PATTERNS_REFERENCE.md:1-200+` - Comprehensive pattern reference with examples
- `docs/patterns/PATTERN_IMPLEMENTATION_GUIDE.md:1-200+` - Detailed guide for implementing patterns
- `docs/yawl_patterns/README.md:1-81` - Overview of 43 YAWL patterns with DOT diagram references

**Tests:**
- `test/yawl_patterns_test.erl:1-948` - Comprehensive EUnit test suite covering all 43 patterns
- `test/yawl_patterns_execution_test.erl:1-200+` - Execution tests for pattern state transitions
- Pattern modules include doctests (e.g., `exclusive_choice.erl:544-594`)

### Current Architecture

**Pattern Module Structure:**
```erlang
-module(pattern_name).
-behaviour(gen_yawl).

%% Required callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2, trigger/3]).

%% Pattern-specific API
-export([new/1, start/1, run/1, execute/2, get_state/1]).
```

**Petri Net Semantics:**
- Each pattern defines places (tokens) and transitions (actions)
- `fire/3` callback can return: `abort | {produce, MarkingMap} | {produce, MarkingMap, NewUsrInfo}`
- XES logging integrated for process mining compliance
- OpenTelemetry instrumentation support

**Integration Points:**
- Pattern registry: `yawl_pattern_registry:pattern_module/1` maps pattern IDs to modules
- YAML spec parsing: `wf_yaml_spec` supports pattern instances in YAML 0.2 format
- Compiler integration: `yawl_compile.erl` generates gen_pnet-compatible code

## Technical Considerations

### Dependencies

**Internal:**
- `gen_yawl` behavior (wrapper around `gen_pnet`)
- `yawl_xes` for XES event logging
- `lib_combin` for nondeterministic choice (e.g., `exclusive_choice.erl:224`)
- `yawl_pattern_registry` for pattern lookups
- `wf_yaml_spec` for YAML workflow definitions

**External:**
- OTP 25.0-28.x (documented in `YAWL_PATTERNS_REFERENCE.md:10`)
- EUnit for testing
- `yamerl` for YAML parsing

### Patterns to Follow

**Consistent Pattern Module Structure:**
1. Module header with license and `-moduledoc`
2. `-behaviour(gen_yawl)` declaration
3. State record with type exports
4. API functions: `new/N`, `start/1`, `run/N`, `execute/2`, `get_state/1`
5. `gen_yawl` callbacks: structure (places, transitions, marking) + interface (init, handle_call, etc.)
6. Internal helpers with `log_event/N`, `generate_log_id/0`
7. Doctests under `-ifdef(TEST)`

**Logging Pattern:**
```erlang
log_event(State, Concept, Lifecycle, Data) when State#pattern_state.log_id =/= undefined ->
    yawl_xes:log_event(State#pattern_state.log_id, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
```

**State Management Pattern:**
```erlang
init(PatternState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"PatternName">>}) of
        {ok, LogId} ->
            State1 = PatternState#pattern_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, PatternState}
    end.
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Architecture Mismatch** | High | Current implementation uses Petri net compilation approach; IDEAS.md explicitly rejects this approach. Need to clarify if this item requires re-architecture or enhancement of existing patterns. |
| **Test Coverage Gaps** | Medium | EUnit tests exist but property-based tests (PropEr/Proper) not found in initial search. Need to verify property-based test requirements. |
| **Documentation Outdated** | Medium | `docs/43_PATTERNS_COMPLETE.md` claims completion but may not reflect IDEAS.md requirements. Cross-check needed. |
| **Pattern Semantics Correctness** | High | Need formal verification of pattern semantics against YAWL pattern canon. Bounded model checking mentioned in item 018. |
| **Performance** | Medium | Current Petri net compilation may have indirection overhead; IDEAS.md demands "tight latency" with "minimal indirection." |

## Recommended Approach

### Option 1: Enhance Existing Petri Net Implementation (Conservative)

If the goal is to complete/verify the **current implementation**:

1. **Verify Pattern Coverage:**
   - Audit all 43 patterns against YAWL canon
   - Ensure each pattern has correct Petri net structure
   - Verify soundness properties (option to complete, proper completion, no dead transitions)

2. **Add Property-Based Tests:**
   - Implement PropEr tests for each pattern
   - Test invariants: "one branch selected" for exclusive choice, "all branches complete" for parallel split
   - Test concurrency properties with race condition detection

3. **Complete Documentation:**
   - Create `docs/PATTERNS.md` mapping patterns → terms → compiled forms
   - Document pattern algebra if exists
   - Add usage examples for each pattern

4. **Performance Optimization:**
   - Profile Petri net execution overhead
   - Optimize hot paths in `fire/3` callbacks
   - Consider JIT compilation for frequently-used patterns

### Option 2: Implement Pattern-Term Algebra (RADICAL - per IDEAS.md)

If the goal is to implement the **new architecture** from `IDEAS.md`:

1. **Implement Pattern Term Algebra:**
   - Create `wf_term.erl` with AST nodes for each pattern
   - Define kernel pattern basis (sequence, parallel-split, xor-choice, etc.)
   - Implement smart constructors for derived patterns

2. **Build Compiler:**
   - Implement `wf_compile.erl` compiling `wf_term()` to bytecode or continuation network
   - Avoid "workflow as data" interpretation - compile to direct operations
   - Target representation: compiled continuation network or custom bytecode

3. **Implement Reducer/Executor:**
   - Create `wf_exec.erl` with hot-loop reduction semantics
   - Implement small-step operational semantics
   - Support deterministic scheduling (Λ policy)

4. **Implement State Management:**
   - Create `wf_state.erl` for per-case state store
   - Implement atomic commit protocol
   - Integrate with existing effect system (`wf_effect.erl`)

5. **Add Validation Backend:**
   - Keep Petri net compilation for validation only (`wf_validate.erl`)
   - Use bounded model checking (item 018) for soundness verification
   - Do NOT use Petri nets at runtime

### Recommended: Hybrid Approach

Given the investment in current implementation:

1. **Phase 1: Complete Current System**
   - Add property-based tests to existing patterns
   - Create `docs/PATTERNS.md` documenting current architecture
   - Verify pattern semantics correctness

2. **Phase 2: Experimental New Architecture**
   - Implement `wf_term` and `wf_compile` in parallel module namespace
   - Build prototype compiler to bytecode
   - Benchmark against current implementation

3. **Phase 3: Migration Path**
   - If new architecture proves significantly faster, plan migration
   - Provide compatibility layer for existing pattern modules
   - Deprecate Petri net runtime gradually

## Open Questions

1. **Architecture Direction:** Does this item require (a) completion of current Petri net implementation, or (b) implementation of new pattern-term algebra architecture from `IDEAS.md`? The two approaches are fundamentally different.

2. **Pattern-Term Algebra:** Is there already a `wf_term.erl` module defining pattern AST? If not, is it within scope to create it, or does it belong to item 010 (pattern-term algebra)?

3. **Property-Based Tests:** Are there existing PropEr tests that weren't found in initial search? What specific properties should be tested for each pattern?

4. **Compilation Target:** If implementing new architecture, should compilation target be:
   - Custom bytecode VM (`wf_vm.erl`)
   - Compiled continuation network (Erlang closures)
   - Direct inlined operations

5. **Dependency on Other Items:** This item likely depends on:
   - Item 010: Pattern-term algebra and AST
   - Item 011: Compiler from pattern terms to executable form
   - Item 012: Reducer/executor hot loop
   - Item 018: Validation backend with bounded model checking

6. **Runtime Model:** Should patterns maintain `gen_yawl` behavior interface for compatibility, or is a new runtime API acceptable?

7. **Testing Infrastructure:** Item 024 covers "comprehensive testing infrastructure." Should this item wait for it, or implement pattern-specific tests independently?

8. **Documentation Format:** `docs/PATTERNS.md` is referenced in both `item.json:25` and `IDEAS.md:71`. What's the expected format? Mapping table? Algebra specification?

9. **Backwards Compatibility:** If implementing new architecture, must it maintain compatibility with existing YAML workflow specifications and AGI Symposium Ω simulation?

10. **Performance Requirements:** What are the latency targets? "Minimal indirection" is vague - need concrete metrics.
