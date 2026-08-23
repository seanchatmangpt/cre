# Research: Multiple instance pattern implementations

**Date**: 2025-01-11
**Item**: 023-multiple-instance-pattern-implementations

## Research Question
Parallel execution of same task with data variations is a common workflow requirement. Need patterns that handle multiple instances with well-defined semantics and synchronization.

**Motivation:** Supports fan-out/fan-in patterns, enables data parallelism, covers important workflow patterns, provides flexible synchronization options.

**Success criteria:**
- Fixed MI spawns exact count
- Dynamic MI spawns based on runtime data
- All sync variants work correctly
- Join policies enforced

**Signals:** priority: high, urgency: Required for pattern coverage

## Summary

The CRE codebase has **partial implementation** of multiple instance patterns with significant gaps in synchronization variants, dynamic spawning, and unified semantics. The current system supports basic multi-instance functionality through multiple disconnected modules (`wf_mi`, `multi_instance`, `multiple_instances_sync`, `n_out_of_m`, static/dynamic partial join patterns) but lacks a **comprehensive, unified framework** that handles all synchronization variants with consistent semantics.

**Key Findings:**

1. **Existing multi-instance support is fragmented:**
   - `wf_mi.erl` - Pure utility functions for MI detection and token creation
   - `multi_instance.erl` - Basic WCP-12/13/14 implementations (process-spawning approach, not gen_yawl)
   - `multiple_instances_sync.erl` - WCP-12 synchronization pattern (gen_yawl)
   - `n_out_of_m.erl` - WCP-22 partial join (quorum-based, fully implemented gen_yawl)
   - `static_partial_join_mi.erl` / `dynamic_partial_join_mi.erl` - Basic implementations
   - `blocking_discriminator.erl` - WCP-09 discriminator (standalone, not integrated)

2. **Critical gaps identified:**
   - No unified API covering all synchronization variants (no sync, all sync, n-of-m, discriminator, first-n)
   - Dynamic spawning (WCP-15) is not truly dynamic - instances created at start, not data-driven
   - Missing first-N join policy (proceed after N complete, wait for all M only)
   - Cancellation integration with item 014 is incomplete
   - Result aggregation strategies are limited
   - No pattern-term algebra support (IDEAS.md requirement for compilation)

3. **Architectural context (from IDEAS.md):**
   - The long-term vision requires pattern-term algebra with compilation to bytecode/continuations
   - Current implementation uses Petri net interpretation (gen_pnet/gen_yawl)
   - Item 023 must align with Items 010 (pattern algebra), 011 (compiler), 021 (core patterns)
   - Goal: "Multiple instance support (fixed and dynamic)" is explicitly listed in IDEAS.md:59

**Recommendation:** Implement a unified `mi` (multiple instance) pattern wrapper in the new wf_term algebra (Item 010) that compiles to efficient bytecode/continuations (Item 011), while maintaining backward compatibility with existing gen_yawl patterns. This bridges the current Petri net implementation with the future compiled execution model.

## Current State Analysis

### Existing Implementation

#### 1. Multi-Instance Utility Module (`wf_mi`)

**File:** `/Users/sac/cre/src/wf/wf_mi.erl:1-400+`

Pure functional utilities for MI task handling:
- `is_mi_task/3` - Detect if task has multi-instance configuration
- `evaluate_mi/2` - Calculate instance count from data
- `create_instance_tokens/2` - Generate indexed tokens for each instance
- `instance_count/2` - Get total instance count
- `instance_threshold/2` - Check if continuation threshold met

**Status:** ✅ Functional but limited
- Supports parallel/sequential modes
- N-of-M threshold semantics
- No synchronization logic (just utilities)
- No gen_yawl behavior (pure module)

#### 2. Basic Multi-Instance Pattern (`multi_instance`)

**File:** `/Users/sac/cre/src/patterns/multi_instance.erl:1-500+`

Implements WCP-12, WCP-13, WCP-14 using process spawning:
- `multiple_instances_no_sync/2` - Spawn N instances, continue immediately
- `multiple_instances_design_time/3` - Fixed N instances with synchronization
- `multiple_instances_runtime/3` - Runtime-determined N with synchronization
- `create_instances/3` - Spawn instance tokens
- `collect_instances/2` - Gather results
- `instance_counter/1` - Track active instances

**Status:** ⚠️ Partial implementation
- Uses direct process spawning (erlang:spawn), NOT gen_yawl behavior
- Limited synchronization variants (only "all" and "none")
- No N-of-M or discriminator join policies
- Not integrated with Petri net semantics
- Outdated approach - doesn't follow gen_yawl convention

#### 3. Synchronization Pattern (`multiple_instances_sync`)

**File:** `/Users/sac/cre/src/patterns/multiple_instances_sync.erl:1-300+`

Implements WCP-12 (Multiple Instances with Synchronization) as gen_yawl behavior:
- Petri net structure with p_start, p_instance_pool, p_active_* places
- Synchronization barrier (p_sync_barrier) waiting for all instances
- Hardcoded for 4 instances (p_active_1 through p_active_4)
- XES logging integration

**Status:** ⚠️ Basic gen_yawl implementation
- Follows correct pattern (gen_yawl behavior)
- Only supports "all" join policy (wait for all instances)
- Hardcoded instance count (not dynamic)
- No N-of-M, first-N, or discriminator variants
- Reference for gen_yawl structure but incomplete

#### 4. N-out-of-M Pattern (`n_out_of_m`)

**File:** `/Users/sac/cre/src/patterns/n_out_of_m.erl:1-657`

Fully implemented WCP-22 partial join pattern:
- N-of-M quorum join policy (proceed after N out of M complete)
- Complete gen_yawl behavior with Petri net semantics
- XES logging for process mining
- Pure functional design
- Comprehensive documentation and doctests

**Status:** ✅ **Production-ready model implementation**
- This is the reference for how MI patterns should be implemented
- Demonstrates proper gen_yawl structure
- Shows correct fire/3 callback with 3-tuple returns
- Has result collection and quorum detection

**Gap:** Only implements one join policy (n_of_m), not a unified framework

#### 5. Static/Dynamic Partial Join Patterns

**Files:**
- `/Users/sac/cre/src/patterns/static_partial_join_mi.erl:1-87`
- `/Users/sac/cre/src/patterns/dynamic_partial_join_mi.erl:1-108`

Basic implementations of WCP-13 and WCP-14:
- Static: Fixed M instances known at design time
- Dynamic: Runtime threshold computation via expression evaluation

**Status:** ⚠️ Simplistic implementations
- Minimal Petri net structures
- No result aggregation
- Dynamic version doesn't truly spawn instances dynamically (just threshold)
- No integration with cancellation scopes

#### 6. Discriminator Pattern

**File:** `/Users/sac/cre/src/patterns/blocking_discriminator.erl:1-83`

Implements WCP-09 (first completion triggers):
- Blocks until first instance completes
- Triggers downstream flow immediately
- Cancels remaining branches

**Status:** ⚠️ Standalone pattern
- Not integrated with multi-instance framework
- Shows discriminator semantics but not reusable
- No parameterization (hardcoded for specific use case)

### Key Files

| File | Lines | Purpose | Status |
|------|-------|---------|--------|
| `/Users/sac/cre/src/wf/wf_mi.erl` | 1-400+ | MI utilities (detection, evaluation, tokens) | ⚠️ Utilities only |
| `/Users/sac/cre/src/patterns/multi_instance.erl` | 1-500+ | Basic WCP-12/13/14 (process spawning) | ⚠️ Outdated approach |
| `/Users/sac/cre/src/patterns/multiple_instances_sync.erl` | 1-300+ | WCP-12 gen_yawl (sync barrier) | ⚠️ Limited to "all" join |
| `/Users/sac/cre/src/patterns/n_out_of_m.erl` | 1-657 | WCP-22 quorum join | ✅ Model implementation |
| `/Users/sac/cre/src/patterns/static_partial_join_mi.erl` | 1-87 | WCP-13 static instances | ⚠️ Basic |
| `/Users/sac/cre/src/patterns/dynamic_partial_join_mi.erl` | 1-108 | WCP-14 runtime instances | ⚠️ Partial |
| `/Users/sac/cre/src/patterns/blocking_discriminator.erl` | 1-83 | WCP-09 discriminator | ⚠️ Standalone |
| `/Users/sac/cre/src/wf/wf_multi_instance.erl` | 1-418 | gen_server registry | ⚠️ Not gen_yawl |
| `/Users/sac/cre/test/yawl_multiple_instances_test.erl` | 1-2324 | Comprehensive test suite | ✅ Tests exist |

### Existing Patterns and Conventions

**1. gen_yawl Behavior Implementation**

All workflow patterns implement `gen_yawl` behavior (from `n_out_of_m.erl:21-111`):

```erlang
-module(pattern_name).
-behaviour(gen_yawl).

%% Required callbacks
-export([
    place_lst/0,         % List of Petri net places
    trsn_lst/0,          % List of transitions
    init_marking/2,      % Initial token distribution
    preset/1,            % Transition wiring (inputs)
    is_enabled/3,        % Guard conditions
    fire/3,              % Token production/consumption
    trigger/3            % Token filtering (cancellation)
]).

%% State record
-record(state, {
    m :: pos_integer(),
    n :: pos_integer(),
    completed = [] :: [term()],
    results = [] :: [term()],
    log_id :: binary() | undefined
}).
```

**Implication:** Any new multi-instance pattern must be a gen_yawl module

**2. Pure Functional Design**

Core logic is pure - only gen_yawl callbacks have side effects (from `n_out_of_m.erl:366-373`):

```erlang
fire('t_split', #{'p_start' := [start]}, #state{m = M, branch_funs = Funs}) ->
    %% Create branch tokens - pure function
    BranchTokens = [{{branch, I}, Fun} || {I, Fun} <- lists:zip(lists:seq(1, M), Funs)],
    {produce, #{
        'p_start' => [],
        'p_branch_pool' => BranchTokens
    }};
```

**Implication:** Instance spawning and join logic must be pure

**3. Token-Based Communication**

State changes flow through token production/consumption:
- Instance tokens: `{{instance, Id}, Data}`
- Completion tokens: `{instance_complete, Id}`
- Cancellation tokens: `{cancel, {instance, InstanceId}}`

**4. XES Logging Integration**

Log workflow events for process mining (from `n_out_of_m.erl:635-644`):

```erlang
log_event(#state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
```

**Implication:** Multi-instance events should be logged (spawn, complete, quorum)

**5. 3-Tuple Fire Returns (gen_yawl extension)**

Extended `fire/3` can update `usr_info`:

```erlang
%% Standard 2-tuple (gen_pnet compatible)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap}

%% Enhanced 3-tuple (gen_yawl extension)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap, NewUsrInfo}
```

**Implication:** Can track instance completion state in usr_info

## Technical Considerations

### Dependencies

#### Internal Modules to Integrate

1. **gen_yawl** (Core behavior)
   - **Purpose:** Single OTP runner maintaining Petri net state
   - **Usage:** All multi-instance patterns must implement gen_yawl callbacks
   - **Key insight:** Follow `n_out_of_m.erl` as the reference implementation
   - **File:** `/Users/sac/cre/src/core/gen_yawl.erl:1-200+`

2. **wf_mi** (Utilities)
   - **File:** `/Users/sac/cre/src/wf/wf_mi.erl:1-400+`
   - **Purpose:** Detection, evaluation, token creation utilities
   - **Integration:** Use for instance ID generation and token creation
   - **Functions:** `create_instance_tokens/2`, `evaluate_mi/2`

3. **wf_scope** (Scope boundaries)
   - **File:** `/Users/sac/cre/src/wf/wf_scope.erl:1-319`
   - **Purpose:** Maps parent-child place relationships for subflows
   - **Integration:** Define instance-level scopes for cancellation (item 014)
   - **Key functions:** `enter/3`, `leave/3`, `bindings/2`

4. **wf_cancel** (Cancellation tokens)
   - **File:** `/Users/sac/cre/src/wf/wf_cancel.erl:1-689`
   - **Purpose:** Create and apply cancellation tokens
   - **Integration:** Add instance-level cancellation support
   - **Need:** Extend to support per-instance cancellation scopes

5. **wf_term** (Pattern algebra - from IDEAS.md)
   - **File:** `/Users/sac/cre/src/wf/wf_term.erl` (exists, needs inspection)
   - **Purpose:** Pattern-term AST constructors
   - **Integration:** Should define `mi(Policy, P)` constructor (IDEAS.md:119)
   - **Status:** Blocked by Item 010

6. **wf_compile** (Compiler - from IDEAS.md)
   - **File:** `/Users/sac/cre/src/wf/wf_compile.erl` (exists)
   - **Purpose:** Compile pattern terms to bytecode/continuations
   - **Integration:** Should handle MI_SPAWN opcode (IDEAS.md:166)
   - **Status:** Blocked by Item 011

#### External Dependencies

- **lib_combin** (from joergen7/lib_combin)
  - Used for deterministic nondeterminism in transition selection
  - Automatically used by gen_yawl for enabled transition selection
  - No direct integration needed

- **yawl_xes** (XES logging)
  - Process mining event logging
  - Integrated via gen_yawl pattern modules
  - Use `log_event/4` for MI events

### Patterns to Follow

#### 1. Model Implementation: n_out_of_m.erl

**File:** `/Users/sac/cre/src/patterns/n_out_of_m.erl:1-657`

This is the production-ready reference for multi-instance patterns:
- Complete gen_yawl callbacks (place_lst, trsn_lst, fire/3, trigger/3)
- State record with quorum tracking
- XES logging for process mining
- Pure functional design
- Comprehensive doctests

**Key structure:**
```erlang
-record(n_out_of_m_state, {
    m :: pos_integer(),          % Total instances
    n :: pos_integer(),          % Quorum required
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = [] :: [{pos_integer(), term()}],
    quorum_met = false :: boolean(),
    wait_for_all = false :: boolean(),
    log_id :: binary() | undefined
}).
```

#### 2. Pattern-Term Algebra (from IDEAS.md)

**Section 2.2 (lines 107-127):**

Required primitives:
- `mi(Policy, P)` - Multiple instances wrapper
- `join(Policy, ListOfP)` - Generalized join with policy

Join policies (from IDEAS.md:115):
- `all` - Wait for all instances
- `first_n` - Proceed after N complete
- `n_of_m` - Quorum-based (N out of M)
- `sync_merge` - Synchronizing merge
- `discriminator` - First completion triggers

**Implication:** Item 023 should implement these as pattern terms that compile to bytecode

#### 3. Bytecode Opcodes (from IDEAS.md)

**Section 3 Strategy S1 (lines 163-167):**

Opcodes for multiple instances:
- `MI_SPAWN` - Spawn instances (fixed or dynamic)
- `JOIN_WAIT` - Wait for join policy satisfaction
- `CANCEL_SCOPE` - Cancel remaining instances after quorum

**Implication:** Compiler (Item 011) needs to generate these opcodes for MI patterns

#### 4. Petri Net to Compilation Migration

**Current state:** All MI patterns use Petri net interpretation (gen_yawl)
**Desired state (IDEAS.md:24-28):** Patterns compile to bytecode/continuations

**Migration path:**
1. Keep existing gen_yawl patterns for backward compatibility
2. Implement new `wf_term:mi/2` constructor (Item 010)
3. Extend `wf_compile` to handle MI patterns (Item 011)
4. Generate MI_SPAWN/JOIN_WAIT opcodes
5. Executor (Item 012) runs compiled bytecode

### Integration Points

#### 1. Workflow Specification Integration

**Purpose:** Define multi-instance tasks in YAWL XML

**Current state:** `wf_spec` module parses YAWL XML but MI support unclear

**Proposed XML format:**
```xml
<task id="review">
  <name>Document Review</name>
  <multiInstance>
    <instanceCount>3</instanceCount>
    <quorum>2</quorum>
    <joinPolicy>n_of_m</joinPolicy>
  </multiInstance>
</task>
```

#### 2. Cancellation Integration (Item 014)

**Purpose:** Cancel individual instances or entire multi-instance activity

**Scope types:**
```erlang
-type mi_cancel_scope() :: {instance, binary()} |  %% Single instance
                          {activity, atom()} |       %% All instances of task
                          {quorum, pos_integer()}.    %% Cancel after N complete
```

**Integration with wf_cancel:**
```erlang
%% Cancel specific instance
wf_cancel:create_activity_cancel({instance, InstanceId}),

%% Cancel entire activity
wf_cancel:create_activity_cancel({activity, review_task}),

%% Cancel remaining after quorum
wf_cancel:create_activity_cancel({quorum, 2}),
```

#### 3. Result Aggregation Strategies

**Purpose:** Collect and merge results from completed instances

**Proposed strategies:**
```erlang
-type result_strategy() :: collect_all |      %% Return all results
                          collect_quorum |    %% Return first Q results
                          merge |             %% Merge results using function
                          broadcast |         %% Send results to multiple places
                          discard.            %% Discard results

-record(mi_config, {
    result_strategy :: result_strategy(),
    merge_fun :: fun(([term()]) -> term()) | undefined,
    broadcast_to :: [atom()] | undefined
}).
```

#### 4. Deterministic Scheduling (Item 013)

**Purpose:** Ensure reproducible MI execution order

**Requirements:**
- Record instance spawn order in trace
- Record nondeterministic completion order
- Replay mode must reproduce exact execution
- Choice logging for branch selection

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Fragmented API** - Multiple disconnected MI modules confuse users | High | Create unified `mi` pattern wrapper in wf_term that dispatches to appropriate implementation |
| **Architecture mismatch** - Current Petri net interpretation vs. IDEAS.md compilation vision | Critical | Implement hybrid approach: keep gen_yawl patterns, add wf_term:mi/2 that compiles to bytecode |
| **Dynamic spawning unboundedness** - WCP-15 could spawn infinite instances | High | Add max_instances limit; implement backpressure via token pool; require data source exhaustion condition |
| **Join policy complexity** - Multiple join policies may have conflicting semantics | Medium | Define clear precedence (discriminator > first_n > n_of_m > all); validate policy at compile time |
| **Cancellation race conditions** - Instance may complete while cancellation in flight | Medium | Use gen_yawl trigger/3 callback to filter cancellation tokens atomically; follow item 014 structured cancellation |
| **Test gap** - Tests reference `cre_yawl_patterns` which doesn't exist | Medium | Update tests to call new `mi` pattern wrapper or individual gen_yawl patterns directly |
| **Dependency on Items 010/011** - Pattern algebra and compiler not complete | Critical | Implement Item 023 in two phases: (1) gen_yawl patterns now, (2) wf_term compilation when Items 010/011 ready |
| **Performance of interpretation** - Petri net dispatch may be slow for thousands of instances | Medium | Benchmark current implementation; optimize hot paths; plan migration to compiled bytecode |
| **Result ordering inconsistency** - Parallel instances complete in non-deterministic order | Low | Define result ordering semantics (completion order vs. instance ID); provide sorting functions |
| **Integration with existing workflows** - May break existing YAWL specifications | Medium | Maintain backward compatibility; support both old and new MI patterns; provide migration guide |

## Recommended Approach

### High-Level Strategy

Based on research findings and IDEAS.md requirements, implement **unified multiple instance support** in two parallel tracks:

#### Track A: Short-Term (gen_yawl Patterns - Item 023 Scope)

**Goal:** Complete missing synchronization variants using existing gen_yawl infrastructure

**Phases:**

1. **Phase 1: Implement Missing Join Policies**
   - Create `mi_n_of_m_pattern.erl` (extend existing n_out_of_m)
   - Create `mi_first_n_pattern.erl` (new - proceed after N complete)
   - Create `mi_discriminator_pattern.erl` (integrate blocking_discriminator)
   - Create `mi_all_pattern.erl` (extend multiple_instances_sync)
   - All as gen_yawl behaviors following n_out_of_m model

2. **Phase 2: Implement Dynamic Spawning (WCP-15)**
   - Create `mi_dynamic_pattern.erl` with data-driven spawning
   - Add max_instances limit for safety
   - Implement backpressure via token pool
   - Data source exhaustion detection

3. **Phase 3: Unified Facade Module**
   - Create `mi_pattern.erl` that dispatches to appropriate implementation
   - API: `mi_pattern:execute(Spec, JoinPolicy, InstanceFuns)`
   - Validation: `mi_pattern:validate_join_policy/2`
   - Backward compatibility with existing patterns

4. **Phase 4: Cancellation Integration**
   - Add per-instance cancellation scopes (item 014)
   - Implement trigger/3 for cancellation token filtering
   - Add cancel-on-quorum transition

5. **Phase 5: Test Suite Updates**
   - Fix `yawl_multiple_instances_test.erl` to use new facade
   - Add property-based tests for invariants
   - Performance benchmarks for large instance counts

#### Track B: Long-Term (Pattern Algebra - Items 010/011)

**Goal:** Implement MI as pattern-term constructor that compiles to bytecode

**Phases:**

1. **Phase 1: Pattern-Term Constructor (Item 010)**
   - Add `mi(Policy, P)` to `wf_term.erl`
   - Define join policy types: `all`, `{first_n, N}`, `{n_of_m, N, M}`, `discriminator`
   - Add instance spec types: `{fixed, M}`, `{runtime, Fun}`, `{dynamic, DataFun, Max}`

2. **Phase 2: Compiler Support (Item 011)**
   - Extend `wf_compile.erl` to handle `mi()` terms
   - Generate MI_SPAWN opcode for instance creation
   - Generate JOIN_WAIT opcode for synchronization
   - Compile join policies to efficient checks (no graph scanning)

3. **Phase 3: Executor Integration (Item 012)**
   - Add MI_SPAWN handler in `wf_exec.erl`
   - Implement instance spawning in hot loop
   - Add quorum detection without marking scans
   - Support cancellation propagation

4. **Phase 4: Migration Path**
   - Provide compatibility layer: gen_yawl patterns call wf_term constructors
   - Benchmark: compiled bytecode vs. Petri net interpretation
   - Deprecation plan for old patterns

### Implementation Architecture (Track A)

```
┌─────────────────────────────────────────────────────────────┐
│                    Unified MI Facade                         │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  mi_pattern.erl (Unified API)                          │ │
│  │  - execute/3 (Spec, JoinPolicy, Funs)                  │ │
│  │  - validate_join_policy/2                              │ │
│  │  - validate_instance_spec/1                            │ │
│  │  - Dispatches to appropriate pattern module            │ │
│  └────────────────────────────────────────────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         ▼               ▼               ▼
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│ mi_all_      │ │ mi_n_of_m_   │ │ mi_first_n_  │
│ pattern.erl  │ │ pattern.erl  │ │ pattern.erl  │
│              │ │              │ │              │
│ Wait all M   │ │ Quorum N/M   │ │ First N      │
│ (WCP-13)     │ │ (WCP-22)     │ │ (NEW)        │
└──────────────┘ └──────────────┘ └──────────────┘
         │               │               │
         └───────────────┼───────────────┘
                         ▼
                  ┌──────────────┐
                  │ mi_          │
                  │ discriminator│
                  │ _pattern.erl │
                  │              │
                  │ First wins   │
                  │ (WCP-09)     │
                  └──────────────┘
                         │
                         ▼
                  ┌──────────────┐
                  │ mi_dynamic_  │
                  │ pattern.erl  │
                  │              │
                  │ Data-driven  │
                  │ (WCP-15)     │
                  └──────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                    gen_yawl Behavior                         │
│  - All patterns implement gen_yawl callbacks                │
│  - Petri net structure with places/transitions              │
│  - XES logging integration                                  │
│  - trigger/3 for cancellation token filtering               │
└─────────────────────────────────────────────────────────────┘
```

### Example Usage (Track A)

```erlang
%% Fixed instances with quorum join
Spec = {fixed, 5},
JoinPolicy = {n_of_m, 3, 5},  %% 3 of 5 quorum
InstanceFuns = [fun() -> work1() end, fun() -> work2() end, ...],
{ok, Results} = mi_pattern:execute(Spec, JoinPolicy, InstanceFuns).

%% Dynamic instances with first-N join
DataFun = fun() ->
    case get_next_data() of
        {ok, Data} -> {more, Data};
        eof -> done
    end
end,
Spec2 = {dynamic, DataFun, 100},  %% Max 100 instances
JoinPolicy2 = {first_n, 10},  %% Proceed after first 10
{ok, Results2} = mi_pattern:execute(Spec2, JoinPolicy2, []).

%% Discriminator pattern
Spec3 = {fixed, 3},
JoinPolicy3 = discriminator,  %% First completion wins
{ok, FirstResult} = mi_pattern:execute(Spec3, JoinPolicy3, Funs).
```

### Implementation Architecture (Track B)

```erlang
%% Pattern-term constructor (wf_term.erl)
mi({n_of_m, N, M}, Task) ->
    #{type => mi, policy => {n_of_m, N, M}, body => Task}.

%% Compiler generates bytecode (wf_compile.erl)
compile(#{type => mi, policy => Policy, body := Task}) ->
    TaskCode = compile(Task),
    [
        {mi_spawn, get_instance_spec(Task), M},
        {execute_instances, TaskCode},
        {join_wait, Policy},
        {collect_results}
    ].

%% Executor runs bytecode (wf_exec.erl)
exec({mi_spawn, Spec, M}, State) ->
    InstanceTokens = create_instance_tokens(Spec, M),
    State#{instances => InstanceTokens};
exec({join_wait, {n_of_m, N, M}}, State#{completed := Completed}) ->
    case length(Completed) >= N of
        true -> proceed;
        false -> wait
    end.
```

## Open Questions

1. **Architecture Decision:** Should Item 023 implement (a) complete gen_yawl patterns now, or (b) wait for Items 010/011 and implement pattern-term algebra directly?
   - **Recommendation:** Both tracks in parallel - gen_yawl for immediate use, wf_term for long-term vision

2. **Pattern Granularity:** Should each join policy be a separate gen_yawl module, or one unified `mi_pattern` module with policy parameter?
   - **Recommendation:** Separate modules for clarity (follow n_out_of_m model), unified facade for API

3. **Dynamic Spawning Semantics:** How does true dynamic spawning (WCP-15) work in Petri net model? Can instances be created after workflow starts?
   - **Recommendation:** Use self-loop transition that creates instances until data source exhausted or max_instances reached

4. **Instance State Persistence:** Should instance state be persisted to disk for long-running workflows?
   - **Recommendation:** Start with in-memory only, add optional ETS persistence later

5. **Result Ordering:** Should result order be deterministic (instance ID) or natural (completion order)?
   - **Recommendation:** Support both with configurable ordering strategy

6. **Test Suite Fix:** Tests reference `cre_yawl_patterns` which doesn't exist - should we create this facade or fix tests?
   - **Recommendation:** Create `mi_pattern` facade, update tests to use it

7. **Cancellation Timing:** What happens when instance completes as cancellation arrives?
   - **Recommendation:** Completion wins (respect successful work), filter via trigger/3

8. **Performance Targets:** What are the latency targets for MI patterns? How many instances can we spawn?
   - **Recommendation:** Benchmark with existing patterns, set targets based on measurements

9. **Integration with Item 015:** Item 015 (Multiple Instance Semantics) research exists - is it blocking or complementary?
   - **Recommendation:** Complementary - Item 015 defines semantics, Item 023 implements patterns

10. **Backward Compatibility:** Must new patterns maintain compatibility with existing YAWL XML specifications?
    - **Recommendation:** Yes - support both gen_yawl and wf_term approaches with feature flag

## Next Steps

1. **Clarify scope:** Confirm Item 023 should implement gen_yawl patterns (Track A) vs. pattern-term algebra (Track B)
2. **Coordinate with Items 010/011:** If implementing Track B, ensure pattern algebra and compiler are ready
3. **Choose architecture:** Decide between separate modules vs. unified module with policy parameter
4. **Design dynamic spawning:** Define how WCP-15 works in Petri net model
5. **Plan testing strategy:** Fix test suite, add property-based tests, performance benchmarks
6. **Document migration path:** How users migrate from old patterns to new unified API
7. **Implement Phase 1:** Create missing join policy patterns (first_n, discriminator integration)
8. **Implement Phase 2:** Dynamic spawning with backpressure
9. **Implement Phase 3:** Unified facade module
10. **Integrate cancellation:** Per-instance scopes (item 014)
