# Research: Multiple instance semantics

**Date**: 2025-01-11
**Item**: 015-multiple-instance-semantics

## Research Question
Many real-world workflows require parallel execution of same task with data variations. Need well-defined semantics for creating, tracking, and joining multiple instances.

**Motivation:** Enables parallel processing patterns, supports fan-out/fan-in workflows, provides data parallelism capabilities, covers important workflow patterns from the 43 patterns canon.

**Success criteria:**
- Fixed instances spawn exact count
- Dynamic instances spawn based on runtime data
- Join policies: all, first_n, n_of_m, discriminator
- Instance state tracked independently

**Technical constraints:**
- Support both fixed and dynamic instance counts
- Multiple join policies
- Integration with cancellation scope per instance

**Signals:** priority: high, urgency: Required for pattern coverage completeness

## Summary

CRE (Common Runtime Environment) is a YAWL workflow engine built on Erlang/OTP with **Petri nets as its formal foundation** using the `gen_yawl` behavior. The system has **partial implementation** of multiple instance patterns but lacks a **unified, comprehensive multi-instance semantics framework**.

**Key Finding:** CRE has **basic multi-instance support** through `wf_multi_instance` module and several YAWL pattern implementations (WCP-12 through WCP-15), but lacks:
1. **Unified semantics** across fixed, runtime, and dynamic instance spawning
2. **Complete join policy implementation** (only n-of-m is fully implemented)
3. **Integration with structured cancellation** (item 014)
4. **Instance-level state tracking** with proper lifecycle management

**What exists:**
- ✅ Basic multi-instance registry (`wf_multi_instance`) - gen_server with M/Q (N-out-of-M) semantics
- ✅ Static multi-instance pattern (WCP-13) - design-time knowledge
- ✅ Runtime multi-instance pattern (WCP-14) - runtime knowledge
- ✅ N-out-of-M pattern (WCP-22 partial join variant) - full implementation
- ✅ Discriminator pattern (WCP-09) - first-completion triggers
- ✅ Comprehensive test suite (2324 lines in `yawl_multiple_instances_test.erl`)

**What needs to be added:**
1. **Dynamic multi-instance pattern** (WCP-15) - no prior knowledge, unbounded spawning
2. **First-N join policy** - proceed after first N complete
3. **Join policy framework** - unify all join policies under one API
4. **Instance-level cancellation scope** - integrate with item 014
5. **Result aggregation strategies** - collect, merge, broadcast, discard

## Current State Analysis

### Existing Implementation

#### 1. Multi-Instance Registry (`wf_multi_instance`)

**File:** `/Users/sac/cre/src/wf/wf_multi_instance.erl:1-418`

Provides a gen_server-based registry for tracking multi-instance tasks:

```erlang
%% State record (lines 48-60)
-record(mi_task, {
    mi_id :: binary(),
    task :: atom(),
    case_id :: binary(),
    m :: pos_integer(),          %% Total instances
    q :: pos_integer(),          %% Quorum needed
    mode :: mi_mode(),           %% parallel | sequential
    instance_ids = [] :: [binary()],
    completed = [] :: [binary()],
    results = [] :: [term()],
    created_at :: integer(),
    status :: mi_status()        %% running | quorum_met | completed | cancelled
}).

%% Start multi-instance (lines 138-144)
-spec start_multi_instance(Engine :: pid() | atom(), CaseId :: binary(),
                           Task :: atom(), M :: pos_integer(), Q :: pos_integer(),
                           Mode :: mi_mode(), Now :: integer()) ->
          {ok, binary(), [binary()]} | {error, term()}.
```

**Status:** ⚠️ Functional but limited
- Supports N-out-of-M semantics (quorum-based)
- Tracks instances independently with unique IDs
- Provides state queries via `get_state/2`
- Limited to fixed M (total instances) known at spawn time
- No dynamic instance creation
- No integration with Petri net semantics

**Limitations:**
- Not a gen_yawl behavior - standalone gen_server
- No Petri net places/transitions for workflow integration
- Fixed mode only (parallel/sequential) - no adaptive spawning
- Results stored in memory - no persistence
- No integration with cancellation scopes

#### 2. N-out-of-M Pattern (WCP-22 Partial Join)

**File:** `/Users/sac/cre/src/patterns/n_out_of_m.erl:1-657`

**Full gen_yawl implementation** with complete Petri net semantics:

```erlang
%% State record (lines 127-136)
-record(n_out_of_m_state, {
    m :: pos_integer(),  %% Total number of branches
    n :: pos_integer(),  %% Quorum required
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = [] :: [{pos_integer(), term()}],
    quorum_met = false :: boolean(),
    wait_for_all = false :: boolean(),
    log_id :: binary() | undefined
}).

%% Places (lines 284-293)
place_lst() ->
    ['p_start', 'p_branch_pool', 'p_running', 'p_completed',
     'p_quorum_met', 'p_remaining', 'p_output'].

%% Transitions (lines 300-309)
trsn_lst() ->
    ['t_split', 't_execute', 't_complete', 't_check_quorum',
     't_proceed', 't_complete_all'].
```

**Key features:**
- ✅ Complete gen_yawl behavior implementation
- ✅ Petri net structure with places/transitions
- ✅ Quorum detection (N out of M)
- ✅ XES logging for process mining
- ✅ Pure functional design (except gen_yawl callbacks)
- ✅ Comprehensive documentation and doctests

**Status:** ✅ **Production-ready** - This is the model to follow

#### 3. Static Multi-Instance Pattern (WCP-13)

**File:** `/Users/sac/cre/src/patterns/static_partial_join_mi.erl:1-87`

Implements static multi-instance with partial join:

```erlang
-record(state, {
    total_instances :: pos_integer(),
    threshold :: pos_integer(),
    completed = 0 :: non_neg_integer()
}).

place_lst() ->
    [p_start, p_instances, p_threshold_met, p_end].

trsn_lst() ->
    [t_create_instances, t_complete_instance, t_threshold, t_finish].
```

**Status:** ⚠️ Basic implementation
- Fixed instance count known at design time
- Threshold-based partial join
- Simple structure but not fully featured

#### 4. Dynamic Multi-Instance Pattern (WCP-14)

**File:** `/Users/sac/cre/src/patterns/dynamic_partial_join_mi.erl:1-108`

Implements runtime-determined instance count:

```erlang
-record(state, {
    threshold_expr :: binary(),
    threshold :: pos_integer() | undefined,
    completed = 0 :: non_neg_integer()
}).

trsn_lst() ->
    [t_create_instances, t_complete_instance, t_compute_threshold,
     t_threshold, t_finish].
```

**Status:** ⚠️ Partial implementation
- Runtime threshold computation
- Example expression: `<<"ceil(attendance_estimate*0.08)">>`
- Lacks dynamic spawning (instances still created at start)
- No data-driven instance creation

#### 5. Blocking Discriminator (WCP-09)

**File:** `/Users/sac/cre/src/patterns/blocking_discriminator.erl:1-83`

Implements first-completion trigger pattern:

```erlang
-record(state, {
    trigger :: atom(),
    blocks_until :: [atom()],
    triggered = false :: boolean(),
    blocked = [] :: [atom()]
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_triggered,
     p_blocked, p_cleared, p_end].
```

**Status:** ⚠️ Pattern exists but not integrated
- Standalone pattern
- No integration with multi-instance framework
- Blocking semantics implemented but not reusable

#### 6. Test Suite

**File:** `/Users/sac/cre/test/yawl_multiple_instances_test.erl:1-2324`

**Comprehensive test coverage** for WCP-11 through WCP-17:

```erlang
%% WCP-11: Implicit Termination (lines 177-332)
implicit_termination_test_() -> ...

%% WCP-12: Multiple Instances without Synchronization (lines 346-552)
multiple_instances_no_sync_test_() -> ...

%% WCP-13: Multiple Instances with Design Time Knowledge (lines 566-747)
multiple_instances_static_test_() -> ...

%% WCP-14: Multiple Instances with Runtime Knowledge (lines 761-933)
multiple_instances_runtime_test_() -> ...

%% WCP-15: Multiple Instances without Prior Knowledge (lines 947-1207)
multiple_instances_dynamic_test_() -> ...
```

**Status:** ✅ **Excellent test coverage**
- 2324 lines of comprehensive tests
- Normal execution, stress tests, failure scenarios, state validation
- Performance benchmarks included
- Tests all patterns WCP-11 through WCP-17

**Gap:** Tests exist but **reference `cre_yawl_patterns` module** which doesn't exist - tests are mocking expected behavior

### Key Files

| File | Lines | Purpose | Status |
|------|-------|---------|--------|
| `/Users/sac/cre/src/wf/wf_multi_instance.erl` | 1-418 | Multi-instance registry | ⚠️ Limited |
| `/Users/sac/cre/src/patterns/n_out_of_m.erl` | 1-657 | N-out-of-M pattern (WCP-22) | ✅ Complete |
| `/Users/sac/cre/src/patterns/static_partial_join_mi.erl` | 1-87 | Static MI (WCP-13) | ⚠️ Basic |
| `/Users/sac/cre/src/patterns/dynamic_partial_join_mi.erl` | 1-108 | Runtime MI (WCP-14) | ⚠️ Partial |
| `/Users/sac/cre/src/patterns/blocking_discriminator.erl` | 1-83 | Discriminator (WCP-09) | ⚠️ Standalone |
| `/Users/sac/cre/test/yawl_multiple_instances_test.erl` | 1-2324 | Test suite | ✅ Comprehensive |
| `/Users/sac/cre/src/wf/wf_scope.erl` | 1-319 | Scope boundary mapping | ✅ Complete |
| `/Users/sac/cre/src/patterns/cancel_mi_activity.erl` | 1-65 | Cancel MI pattern | ⚠️ Basic |

## Technical Considerations

### Dependencies

#### Internal Modules to Integrate

1. **gen_yawl** (Core behavior)
   - **Purpose:** Single OTP runner maintaining Petri net state
   - **Usage:** All multi-instance patterns must implement gen_yawl callbacks
   - **Key insight:** Follow `n_out_of_m.erl` as the reference implementation
   - **Callbacks:** `place_lst/0`, `trsn_lst/0`, `init_marking/2`, `preset/1`, `is_enabled/3`, `fire/3`, `trigger/3`

2. **wf_multi_instance** (Registry)
   - **File:** `/Users/sac/cre/src/wf/wf_multi_instance.erl:1-418`
   - **Purpose:** Track multi-instance tasks across workflows
   - **Integration:** Use for instance ID generation and state queries
   - **Limitation:** Currently gen_server - consider making it pure functional

3. **wf_scope** (Scope boundaries)
   - **File:** `/Users/sac/cre/src/wf/wf_scope.erl:1-319`
   - **Purpose:** Maps parent-child place relationships for subflows
   - **Integration:** Define instance-level scopes for cancellation (item 014)
   - **Key functions:** `enter/3`, `leave/3`, `bindings/2`

4. **wf_cancel** (Cancellation tokens)
   - **File:** Referenced in item 014 research
   - **Purpose:** Create and apply cancellation tokens
   - **Integration:** Add instance-level cancellation support
   - **Need:** Extend to support per-instance cancellation scopes

5. **yawl_state** (Workflow state management)
   - **Purpose:** Track workflow case status and work items
   - **Integration:** Store instance completion status in case state
   - **Status:** Has cancellation status - needs instance tracking

#### External Dependencies

- **lib_combin** (from joergen7/lib_combin)
  - Used for deterministic nondeterminism in transition selection
  - Automatically used by gen_yawl for enabled transition selection
  - No direct integration needed

### Patterns to Follow

#### 1. gen_yawl Behavior Implementation

**Convention:** All workflow patterns implement `gen_yawl` behavior

**Example from `n_out_of_m.erl:21-111`:**
```erlang
-module(n_out_of_m).
-behaviour(gen_yawl).

%% gen_pnet callbacks
-export([
    place_lst/0,         %% Returns list of places
    trsn_lst/0,          %% Returns list of transitions
    init_marking/2,      %% Initial token distribution
    preset/1,            %% Transition wiring
    is_enabled/3,        %% Guard conditions
    fire/3,              %% Token production
    trigger/3            %% Token filtering
]).
```

**Implication:** All multi-instance patterns must be gen_yawl modules

#### 2. Pure Functional Design

**Convention:** Core logic is pure - only gen_yawl callbacks have side effects

**Example from `n_out_of_m.erl:366-373`:**
```erlang
fire('t_split', #{'p_start' := [start]}, #n_out_of_m_state{m = M, branch_funs = Funs}) ->
    %% Create branch tokens - pure function
    BranchTokens = [{{branch, I}, Fun} || {I, Fun} <- lists:zip(lists:seq(1, M), Funs)],
    log_event(State, <<"NOutOfM">>, <<"Split">>, #{<<"m">> => M}),
    {produce, #{
        'p_start' => [],
        'p_branch_pool' => BranchTokens
    }};
```

**Implication:** Instance spawning and quorum logic must be pure

#### 3. Token-Based Communication

**Convention:** State changes flow through token production/consumption

**Example pattern:**
```erlang
%% Instance tokens carry both ID and function
{{branch, Index}, Fun}

%% Completion tokens carry index
{branch_complete, Index}
```

**Implication:** Instances must be represented as tokens in the Petri net

#### 4. XES Logging Integration

**Convention:** Log workflow events for process mining

**Example from `n_out_of_m.erl:635-644`:**
```erlang
log_event(#n_out_of_m_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
```

**Implication:** Multi-instance events should be logged (spawn, complete, quorum)

#### 5. 3-Tuple Fire Returns (gen_yawl extension)

**Convention:** Extended `fire/3` can update `usr_info`

```erlang
%% Standard 2-tuple (gen_pnet compatible)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap}

%% Enhanced 3-tuple (gen_yawl extension)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap, NewUsrInfo}
```

**Implication:** Can track instance completion state in usr_info

### Integration Points

#### 1. Workflow Specification Integration

**Purpose:** Define multi-instance tasks in YAWL XML

**Current state:** `wf_spec` module parses YAWL XML but multi-instance support unclear

**Proposed approach:**
```xml
<!-- YAWL XML with multi-instance task -->
<task id="review">
  <name>Document Review</name>
  <multiInstance>
    <instanceCount>3</instanceCount>
    <quorum>2</quorum>
    <joinPolicy>first_n</joinPolicy>
  </multiInstance>
</task>
```

#### 2. Cancellation Scope Integration (Item 014)

**Purpose:** Cancel individual instances or entire multi-instance activity

**Proposed scope types:**
```erlang
-type mi_cancel_scope() :: {instance, binary()} |  %% Single instance
                          {activity, atom()} |       %% All instances of task
                          {quorum, pos_integer()}.    %% Cancel after N complete
```

**Integration with wf_cancel:**
```erlang
%% Cancel specific instance
wf_cancel:create_scope_cancel({instance, InstanceId}),

%% Cancel entire activity
wf_cancel:create_scope_cancel({activity, review_task}),

%% Cancel remaining after quorum met
wf_cancel:create_scope_cancel({quorum, 2}),
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

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Instance state explosion** - Tracking thousands of instances may exhaust memory | High | Implement **lazy instance creation** - spawn only when resources available; use **ets** for large instance counts; provide pagination for state queries |
| **Quorum deadlock** - Instances may hang waiting for quorum that can never be reached | High | Implement **timeout semantics** per instance; add **force cancel** if quorum unreachable; provide **degraded quorum** option (proceed with fewer than Q) |
| **Dynamic spawning unboundedness** - WCP-15 could spawn infinite instances | High | Add **max_instances limit**; implement **backpressure** via token pool; require **data source exhaustion** condition |
| **Join policy complexity** - Multiple join policies may have conflicting semantics | Medium | Define **clear precedence** (first_n → n_of_m → all); validate policy at workflow compile time; provide **policy validation** function |
| **Cancellation race conditions** - Instance may complete while cancellation in flight | Medium | Use **gen_yawl trigger/3** callback to filter cancellation tokens atomically; implement **cancellation tokens** as special token type; follow item 014's structured cancellation semantics |
| **Result ordering inconsistency** - Parallel instances complete in non-deterministic order | Low | Define **result ordering semantics** (completion order vs. instance ID); provide **sorting functions**; document as non-deterministic |
| **Testing complexity** - Concurrent instances make testing difficult | Medium | Use **property-based testing** (PropEr) for invariants; add **determinism flags** for testing; leverage existing test suite structure |
| **Integration with existing patterns** - May conflict with other YAWL patterns | Medium | Define **pattern composition rules**; validate at workflow compile time; provide **pattern compatibility matrix** |

## Recommended Approach

### High-Level Strategy

Based on the research, implement **unified multiple instance semantics** in four phases:

#### Phase 1: Instance Spawning Framework

**Goal:** Unified API for fixed, runtime, and dynamic instance creation

1. **Define instance configuration type**
   ```erlang
   -record(mi_config, {
       task :: atom(),
       case_id :: binary(),
       instance_spec :: instance_spec(),
       join_policy :: join_policy(),
       result_strategy :: result_strategy(),
       cancellation_scope :: cancel_scope()
   }).

   -type instance_spec() :: {fixed, pos_integer()} |
                           {runtime, fun(() -> pos_integer())} |
                           {dynamic, fun(() -> {more, term()} | done}, pos_integer()}.

   -type join_policy() :: all |                       %% Wait for all instances
                          {first_n, pos_integer()} |  %% Proceed after N complete
                          {n_of_m, pos_integer(), pos_integer()} | %% N out of M quorum
                          discriminator.              %% First to complete triggers
   ```

2. **Create `multi_instance` pattern module**
   - Follow `n_out_of_m.erl` as reference
   - Implement gen_yawl behavior
   - Support all three instance specs (fixed, runtime, dynamic)
   - Pure functional design with XES logging

3. **Instance spawning transitions**
   ```erlang
   %% Fixed spawning (design time knowledge)
   fire('t_spawn_fixed', #{'p_start' := [start]}, #state{m = M}) ->
       InstanceTokens = [{{instance, I}, data} || I <- lists:seq(1, M)],
       {produce, #{'p_start' => [], 'p_instances' => InstanceTokens}};

   %% Runtime spawning (runtime knowledge)
   fire('t_spawn_runtime', #{'p_start' := [start]}, #state{count_fun = Fun}) ->
       M = Fun(),
       InstanceTokens = [{{instance, I}, data} || I <- lists:seq(1, M)],
       {produce, #{'p_start' => [], 'p_instances' => InstanceTokens}};

   %% Dynamic spawning (no prior knowledge)
   fire('t_spawn_dynamic', #{'p_data' := [Data]}, #state{data_fun = DataFun}) ->
       case DataFun() of
           {more, InstanceData} ->
               {produce, #{'p_instances' => [{{instance, ref()}, InstanceData}]}};
           done ->
               {produce, #{}}
       end.
   ```

#### Phase 2: Join Policy Implementation

**Goal:** Comprehensive join policies with configurable semantics

1. **Implement join transitions**
   ```erlang
   %% All join (wait for all instances)
   fire('t_join_all', #{'p_completed' := Completed, 'p_total' := [M]},
         #state{join_policy = all}) when length(Completed) =:= M ->
       {produce, #{'p_output' => [all_complete]}};

   %% First N join
   fire('t_join_first_n', #{'p_completed' := Completed},
         #state{join_policy = {first_n, N}}) when length(Completed) >= N ->
       {produce, #{'p_output' => [first_n_complete]}};

   %% N of M join (quorum)
   fire('t_join_quorum', #{'p_completed' := Completed},
         #state{join_policy = {n_of_m, N, M}}) when length(Completed) >= N ->
       {produce, #{'p_output' => [{quorum_met, Completed}]}};

   %% Discriminator (first completion triggers)
   fire('t_discriminator', #{'p_first' := [First]}, #state{join_policy = discriminator}) ->
       {produce, #{'p_output' => [First], 'p_cancel' => [cancel_others]}}.
   ```

2. **Join policy validation**
   ```erlang
   -spec validate_join_policy(join_policy(), pos_integer()) ->
           ok | {error, term()}.
   validate_join_policy(all, M) when M > 0 -> ok;
   validate_join_policy({first_n, N}, M) when N =< M, N > 0 -> ok;
   validate_join_policy({n_of_m, N, M}, M) when N =< M, N > 0 -> ok;
   validate_join_policy(discriminator, M) when M > 0 -> ok;
   validate_join_policy(Policy, _M) -> {error, {invalid_policy, Policy}}.
   ```

#### Phase 3: Cancellation Integration

**Goal:** Per-instance and per-activity cancellation

1. **Integrate with item 014 cancellation scopes**
   ```erlang
   %% Instance-level cancellation
   trigger(_Place, {cancel, {instance, InstanceId}}, NetState) ->
       %% Cancel specific instance
       NewNetState = cancel_instance(NetState, InstanceId),
       {pass, [], NewNetState};  %% Drop cancel token, clean state

   %% Activity-level cancellation
   trigger(_Place, {cancel, {activity, TaskId}}, NetState) ->
       %% Cancel all instances of task
       NewNetState = cancel_activity_instances(NetState, TaskId),
       {pass, [], NewNetState}.

   %% Cancel remaining after quorum
   fire('t_cancel_remaining', #{'p_completed' := Completed},
         #state{join_policy = {n_of_m, N, M}, cancel_on_quorum = true})
       when length(Completed) >= N ->
       Remaining = M - length(Completed),
       {produce, #{
           'p_output' => [{quorum_met, Completed}],
           'p_cancel' => [{cancel, {instances, lists:seq(N+1, M)}}]
       }}.
   ```

2. **Scope-aware cancellation**
   - Use `wf_scope` binding table to define instance scopes
   - Each instance gets unique scope ID: `{instance, InstanceId}`
   - Activity scope includes all instances: `{activity, TaskId}`
   - Integrate with `wf_cancel:create_scope_cancel/2`

#### Phase 4: Result Aggregation

**Goal:** Flexible result collection and merging

1. **Implement result strategies**
   ```erlang
   %% Collect all results
   fire('t_collect_all', #{'p_results' := Results},
         #state{result_strategy = collect_all}) ->
       {produce, #{'p_output' => [{all_results, Results}]}};

   %% Collect quorum results
   fire('t_collect_quorum', #{'p_results' := Results},
         #state{result_strategy = collect_quorum, n = N}) ->
       {QuorumResults, _Rest} = lists:split(N, Results),
       {produce, #{'p_output' => [{quorum_results, QuorumResults}]}};

   %% Merge results
   fire('t_merge', #{'p_results' := Results},
         #state{result_strategy = merge, merge_fun = MergeFun}) ->
       Merged = MergeFun(Results),
       {produce, #{'p_output' => [{merged, Merged}]}};

   %% Broadcast results
   fire('t_broadcast', #{'p_results' := Results},
         #state{result_strategy = broadcast, broadcast_to = Places}) ->
       Broadcast = [{Place, Results} || Place <- Places],
       {produce, maps:from_list(Broadcast)}.
   ```

2. **Result ordering**
   ```erlang
   -type result_order() :: completion_order | instance_id | custom_sort.

   -spec order_results([term()], result_order()) -> [term()].
   order_results(Results, completion_order) -> Results;
   order_results(Results, instance_id) ->
       lists:sort(fun({I1, _}, {I2, _}) -> I1 =< I2 end, Results);
   order_results(Results, {custom_sort, SortFun}) ->
       lists:sort(SortFun, Results).
   ```

### Implementation Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Multi-Instance Framework                  │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Instance Spawning (Phase 1)                           │ │
│  │  - Fixed M instances (design time knowledge)           │ │
│  │  - Runtime M instances (runtime knowledge)             │ │
│  │  - Dynamic unbounded (no prior knowledge)              │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Join Policies (Phase 2)                               │ │
│  │  - All (wait for M)                                    │ │
│  │  - First N (proceed after N)                           │ │
│  │  - N of M (quorum)                                     │ │
│  │  - Discriminator (first triggers)                      │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Cancellation (Phase 3)                                │ │
│  │  - Per-instance scope                                  │ │
│  │  - Per-activity scope                                  │ │
│  │  - Cancel-on-quorum                                    │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Result Aggregation (Phase 4)                          │ │
│  │  - Collect all / quorum                                │ │
│  │  - Merge / Broadcast / Discard                         │ │
│  └────────────────────────────────────────────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                    gen_yawl Multi-Instance Pattern           │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Petri Net Structure                                   │ │
│  │  Places: p_start, p_instances, p_running, p_completed,  │ │
│  │          p_quorum_met, p_output, p_cancel              │ │
│  │  Transitions: t_spawn_*, t_execute, t_complete,        │ │
│  │              t_join_*, t_collect                        │ │
│  └────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  usr_info State                                        │ │
│  │  - instance_spec                                        │ │
│  │  - join_policy                                         │ │
│  │  - result_strategy                                     │ │
│  │  - completed_instances                                 │ │
│  │  - instance_results                                    │ │
│  └────────────────────────────────────────────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                    gen_pnet Progress Loop                     │
│  1. Fire transition (fire/3)                                │
│     └─ Returns {produce, Tokens} or {produce, Tokens, UsrInfo} │
│  2. Call trigger/3 for each token                           │
│     └─ Filter cancellation tokens, update state             │
│  3. Add passed tokens to marking                            │
└─────────────────────────────────────────────────────────────┘
```

### Example Usage

```erlang
%% Fixed instances with quorum join
Config = #mi_config{
    task = review_task,
    case_id = <<"case123">>,
    instance_spec = {fixed, 5},
    join_policy = {n_of_m, 3, 5},  %% 3 of 5 quorum
    result_strategy = collect_quorum,
    cancellation_scope = {activity, review_task}
},
{ok, Pid} = gen_yawl:start_link(multi_instance, Config, []),
{ok, Results} = multi_instance:run(Pid).

%% Dynamic instances with first_N join
Config2 = #mi_config{
    task = process_item,
    instance_spec = {dynamic,
        fun() -> receive {item, I} -> {more, I} after 0 -> done end end,
        1000  %% Max 1000 instances
    },
    join_policy = {first_n, 10},  %% Proceed after first 10
    result_strategy = merge,
    merge_fun = fun(Lists) -> lists:flatten(Lists) end
}.

%% Discriminator pattern
Config3 = #mi_config{
    task = parallel_check,
    instance_spec = {fixed, 3},
    join_policy = discriminator,  %% First completion wins
    result_strategy = broadcast,
    broadcast_to = [p_notifier, p_logger]
}.
```

## Open Questions

1. **Instance state persistence**
   - **Question:** Should instance state be persisted to disk?
   - **Impact:** Affects recovery and long-running workflows
   - **Options:**
     - In-memory only (simpler, faster)
     - Persistent ETS tables (survives crashes)
     - Database-backed (distributed, durable)
   - **Recommendation:** Start with in-memory, add optional persistence

2. **Dynamic instance backpressure**
   - **Question:** How to prevent unbounded dynamic spawning?
   - **Impact:** Resource exhaustion in WCP-15 pattern
   - **Options:**
     - Hard limit (max_instances parameter)
     - Soft limit with backpressure (token pool)
     - Adaptive limit based on system load
   - **Recommendation:** Hard limit + backpressure token pool

3. **Result aggregation ordering guarantees**
   - **Question:** Should result order be deterministic?
   - **Impact:** Reproducibility and testing
   - **Options:**
     - Completion order (non-deterministic, natural)
     - Instance ID order (deterministic, may wait)
     - Custom sort function (flexible)
   - **Recommendation:** Support all three, default to completion order

4. **Cancellation timing edge cases**
   - **Question:** What happens when instance completes as cancellation arrives?
   - **Impact:** Race conditions in concurrent execution
   - **Options:**
     - Cancellation wins (drop result)
     - Completion wins (keep result)
     - Timestamp-based (later wins)
   - **Recommendation:** Completion wins (respect successful work)

5. **Integration with existing wf_multi_instance**
   - **Question:** Should we extend or replace the gen_server-based registry?
   - **Impact:** Backward compatibility and migration
   - **Options:**
     - Extend existing module (backward compatible)
     - Replace with gen_yawl version (cleaner, breaking change)
     - Hybrid approach (registry for queries, gen_yawl for execution)
   - **Recommendation:** Hybrid - keep registry for queries, use gen_yawl for patterns

6. **Test gap resolution**
   - **Question:** Tests reference `cre_yawl_patterns` which doesn't exist - how to fix?
   - **Impact:** Cannot run comprehensive test suite
   - **Options:**
     - Create `cre_yawl_patterns` module that aggregates all patterns
     - Rewrite tests to call pattern modules directly
     - Mock expected behavior (current approach)
   - **Recommendation:** Create `cre_yawl_patterns` as pattern facade

## Next Steps

1. **Clarify persistence strategy** (Open Question #1)
2. **Design backpressure mechanism** (Open Question #2)
3. **Create `cre_yawl_patterns` facade module** (Open Question #6)
4. **Implement Phase 1** (Instance Spawning Framework)
5. **Add comprehensive property-based tests** using PropEr
6. **Document pattern composition rules** for integration with other YAWL patterns
