# YAWL Pattern Comparison: Java vs Erlang

**CRE - Common Runtime Environment**
*Detailed comparison of all 43 YAWL workflow control patterns*

**Date:** 2026-02-07
**Status:** Complete

---

## Overview

This document provides a pattern-by-pattern comparison between YAWL v5.2 Java implementation and CRE Erlang implementation for all 43 YAWL workflow control patterns.

### Pattern Categories

- **Basic Control Flow (WCP 1-11)**: 11 patterns
- **Advanced Branching & Synchronization**: 8 patterns
- **Multiple Instance Patterns (WCP 12-15)**: 4 patterns
- **State-Based Patterns**: 2 patterns
- **Cancellation Patterns (WHP)**: 5 patterns
- **Extended Control Patterns**: 13 patterns

---

## Quick Reference Table

| Pattern ID | Pattern Name | YAWL Java | CRE Erlang | Implementation Match |
|------------|--------------|-----------|------------|----------------------|
| P1 | Sequence | Flow order | `sequence.erl` | ✅ Equivalent |
| P2 | Parallel Split | AND split | `parallel_split.erl` | ✅ Equivalent |
| P3 | Synchronization | AND join | `synchronization.erl` | ✅ Equivalent |
| P4 | Exclusive Choice | XOR split + predicates | `exclusive_choice.erl` | ✅ Equivalent |
| P5 | Simple Merge | XOR join | `simple_merge.erl` | ✅ Equivalent |
| P6 | Multiple Choice | OR split + predicates | `multiple_choice.erl` | ✅ Equivalent |
| P7 | Structured Sync Merge | N-way AND join | `structured_sync_merge.erl` | ✅ Equivalent |
| P8 | Multiple Merge | OR join | `multiple_merge.erl` | ✅ Equivalent |
| P9 | Discriminator | First-completion tracking | `discriminator.erl` | ✅ Equivalent |
| P10 | Arbitrary Cycles | Flow cycles | `arbitrary_cycles.erl` | ✅ Equivalent |
| P11 | Implicit Termination | No tokens = done | `implicit_termination.erl` | ✅ Equivalent |
| P12 | MI: No Sync | MI attributes | `multiple_instances_sync` | ✅ Equivalent |
| P13 | MI: Static | MI attributes | `multiple_instances_sync` | ✅ Equivalent |
| P14 | MI: Runtime Known | MI attributes | `multiple_instances_sync` | ✅ Equivalent |
| P15 | MI: Runtime Unknown | MI attributes | `multiple_instances_sync` | ✅ Equivalent |
| P16 | Deferred Choice | Runtime choice | `deferred_choice.erl` | ✅ Equivalent |
| P17 | Interleaved Routing | Serialization | `interleaved_routing.erl` | ✅ Equivalent |
| P18 | Milestone | State guard | `milestone.erl` | ✅ Equivalent |
| P19 | Cancel Activity | Remove set | `cancel_activity.erl` | ✅ Equivalent |
| P20 | Cancel Case | Case cancellation | `cancel_case.erl` | ✅ Equivalent |
| P21 | Structured Loop | While/until | `structured_loop.erl` | ✅ Equivalent |
| P22 | Recursion | Subnet call | `recursion.erl` | ✅ Equivalent |
| P23 | Transient Trigger | One-shot trigger | `transient_trigger.erl` | ⚠️ Partial |
| P24 | Persistent Trigger | Multi-shot trigger | `persistent_trigger.erl` | ⚠️ Partial |
| P25 | Cancel Region | Remove set | `cancel_region.erl` | ✅ Equivalent |
| P26 | Cancel MI Activity | MI cancel | `cancel_mi_activity.erl` | ✅ Equivalent |
| P27 | Complete MI Activity | MI complete | `complete_mi_activity.erl` | ✅ Equivalent |
| P28 | Blocking Discriminator | First + wait reset | `blocking_discriminator.erl` | ✅ CRE Enhancement |
| P29 | Cancelling Discriminator | First + cancel rest | `cancelling_discriminator.erl` | ✅ CRE Enhancement |
| P30 | Structured Partial Join | K of N join | `structured_partial_join.erl` | ✅ CRE Enhancement |
| P31 | Blocking Partial Join | K of N + wait | `blocking_partial_join.erl` | ✅ CRE Enhancement |
| P32 | Cancelling Partial Join | K of N + cancel | `cancelling_partial_join.erl` | ✅ CRE Enhancement |
| P33 | Generalized AND Join | Threshold join | `generalized_and_join.erl` | ✅ CRE Enhancement |
| P34 | Static Partial Join MI | MI partial join | `static_partial_join_mi.erl` | ✅ CRE Enhancement |
| P35 | Cancelling Partial Join MI | MI partial + cancel | `cancelling_partial_join_mi.erl` | ✅ CRE Enhancement |
| P36 | Dynamic Partial Join MI | MI dynamic partial | `dynamic_partial_join_mi.erl` | ✅ CRE Enhancement |
| P37 | Local Sync Merge | Scoped sync | `local_sync_merge.erl` | ✅ CRE Enhancement |
| P38 | General Sync Merge | Configurable sync | `general_sync_merge.erl` | ✅ CRE Enhancement |
| P39 | Critical Section | Mutex pattern | `critical_section.erl` | ✅ Equivalent |
| P40 | Interleaved Routing | Sequential parallel | `interleaved_routing.erl` | ✅ Equivalent |
| P41 | Thread Merge | Thread join | `thread_merge.erl` | ✅ CRE Enhancement |
| P42 | Thread Split | Thread fork | `thread_split.erl` | ✅ CRE Enhancement |
| P43 | Explicit Termination | Hard stop | `explicit_termination.erl` | ✅ Equivalent |

---

## Detailed Pattern Comparisons

### P1: Sequence

**YAWL Java Implementation:**
```java
// Sequence is implicit in flow ordering
YInputCondition input = net.getInputCondition();
YTask task1 = net.getNetTask("task1");
YTask task2 = net.getNetTask("task2");
YOutputCondition output = net.getOutputCondition();

// Flows define sequence
new YFlow(input, task1);
new YFlow(task1, task2);
new YFlow(task2, output);
```

**CRE Erlang Implementation:**
```erlang
%% Explicit Petri net structure
place_lst() -> [p_start, p_task1, p_task2, p_end].
trsn_lst() -> [t_start, t_complete1, t_complete2, t_finish].

preset(t_start) -> [p_start];
preset(t_complete1) -> [p_task1];
preset(t_complete2) -> [p_task2];
preset(t_finish) -> [p_end].

fire(t_start, _, _) ->
    {produce, #{p_task1 => [token]}};
fire(t_complete1, _, _) ->
    {produce, #{p_task2 => [token]}};
fire(t_finish, _, _) ->
    {produce, #{p_end => [done]}}.
```

**Key Difference:** YAWL uses implicit flow ordering; CRE uses explicit Petri net structure.

---

### P2: Parallel Split / P3: Synchronization

**YAWL Java Implementation:**
```java
// Configure split type
task.setSplitType(YTask._AND);  // 95

// Configure corresponding join
joinTask.setJoinType(YTask._AND);

// Flow definition creates parallel paths
new YFlow(splitTask, branch1);
new YFlow(splitTask, branch2);
new YFlow(splitTask, branch3);
// ... all branches flow into joinTask
```

**CRE Erlang Implementation:**
```erlang
%% P2: Parallel Split
place_lst() -> [p_start, p_branch1, p_branch2, p_branch3, p_join_ready, p_all_done, p_end].

fire(t_split, _, _) ->
    {produce, #{
        p_branch1 => [token],
        p_branch2 => [token],
        p_branch3 => [token]
    }}.

%% P3: Synchronization
is_enabled(t_join, Mode, _) ->
    maps:keys(Mode) == [p_branch1, p_branch2, p_branch3].
```

**Key Difference:** YAWL uses type codes (AND/OR/XOR); CRE uses explicit enablement checks.

---

### P4: Exclusive Choice / P5: Simple Merge

**YAWL Java Implementation:**
```java
// XOR split with predicates
task.setSplitType(YTask._XOR);  // 126

YFlow flow1 = new YFlow(choice, branchA);
flow1.setXpathPredicate("//amount > 1000");

YFlow flow2 = new YFlow(choice, branchB);
flow2.setXpathPredicate("//amount <= 1000");

// XOR merge accepts any input
mergeTask.setJoinType(YTask._XOR);
```

**CRE Erlang Implementation:**
```erlang
%% P4: Exclusive Choice
is_enabled(t_select_a, #{p_start := [start]}, State) ->
    State#state.selected =:= undefined;
is_enabled(t_select_b, #{p_start := [start]}, State) ->
    State#state.selected =:= undefined;
is_enabled(_, _, _) ->
    false.

fire(t_select_a, _, State) ->
    {produce, #{p_selected => [a]}, State#state{selected = a}}.

%% P5: Simple Merge
%% Multiple transitions flow to same place
preset(t_merge_a) -> [p_branch_a];
preset(t_merge_b) -> [p_branch_b];

fire(t_merge_a, _, _) -> {produce, #{p_merged => [token]}};
fire(t_merge_b, _, _) -> {produce, #{p_merged => [token]}}.
```

**Key Difference:** YAWL uses XPath predicates; CRE uses `is_enabled` guards.

---

### P9: Discriminator

**YAWL Java Implementation:**
```java
// Discriminator is built into YNetRunner
// When first branch completes, trigger output
// Subsequent completions are consumed without re-triggering

// Internal tracking in YNetRunner
private Set<YTask> _busyTasks;
private Set<YTask> _enabledTasks;

// Logic in YNetRunner.kick()
// When output condition gets token first time, propagate
// Subsequent tokens are absorbed
```

**CRE Erlang Implementation:**
```erlang
%% Explicit discriminator state machine
place_lst() -> [
    p_input, p_branch_pool, p_trigger_ready,
    p_triggered, p_consume, p_reset, p_output
].

%% First completion triggers
fire('t_complete_branch', #{p_branch_pool := [Token|Rest]},
      #discriminator_state{triggered_by = undefined}) ->
    {produce, #{
        p_branch_pool => Rest,
        p_triggered => [first_complete]
    }};

%% Subsequent completions consumed
fire('t_complete_branch', #{p_branch_pool := [Token|Rest]},
      #discriminator_state{triggered_by = _Already}) ->
    {produce, #{
        p_branch_pool => Rest,
        p_consume => [consumed]
    }}.
```

**Key Difference:** CRE provides explicit, inspectable discriminator state via places.

---

### P13: Static Multiple Instances

**YAWL Java Implementation:**
```java
// Configure multi-instance attributes
task.setUpMultipleInstanceAttributes(
    "/definition/query",      // min instances
    "/definition/query",      // max instances
    null,                      // threshold
    "static"                   // creation mode
);

// Runtime instance creation
int instanceCount = queryMaxInstances(caseID);
for (int i = 0; i < instanceCount; i++) {
    YIdentifier childID = _caseIDForNet.createNewChild();
    // Launch each instance
}
```

**CRE Erlang Implementation:**
```erlang
-record(mi_state, {
    mi_count :: integer(),          % Static instance count
    instances_completed = 0 :: integer(),
    outputs = [] :: [term()]
}).

%% Spawn all instances at split
fire(t_split, _, #mi_state{mi_count = N}) ->
    Produce = lists:foldl(fun(I, Acc) ->
        Place = list_to_atom("p_active_" ++ integer_to_list(I)),
        maps:put(Place, [token], Acc)
    end, #{}, lists:seq(1, N)),
    {produce, Produce}.

%% Join when all complete
is_enabled(t_sync, #mi_state{instances_completed = N, mi_count = N}) ->
    true;
is_enabled(t_sync, _) ->
    false.
```

**Key Difference:** CRE uses explicit Petri net places per instance; YAWL uses dynamic identifier creation.

---

### P19: Cancel Activity

**YAWL Java Implementation:**
```java
// Remove set specifies what to cancel
YTask canceller = new YAtomicTask("cancel_task", net);
Set<YExternalNetElement> removeSet = new HashSet<>();
removeSet.add(taskToCancel);
removeSet.add(associatedCondition);
canceller.addRemovesTokensFrom(removeSet);

// When canceller fires, all removed elements lose tokens
```

**CRE Erlang Implementation:**
```erlang
%% Cancellation as explicit API
cancel_activity(Pid, ActivityID) ->
    gen_server:call(Pid, {cancel_activity, ActivityID}).

handle_call({cancel_activity, ActivityID}, _, State) ->
    %% Withdraw token from activity place
    NewMarking = maps:update_with(ActivityID,
        fun([_|T]) -> T end,
        State#wrapper_state.marking),
    {reply, ok, State#wrapper_state{marking = NewMarking}}.
```

**Key Difference:** YAWL uses declarative remove sets; CRE uses imperative cancellation API.

---

### P25: Cancel Region

**YAWL Java Implementation:**
```java
// Region defined by remove set
private Set<YExternalNetElement> _removeSet;

// When transition with remove set fires:
for (YExternalNetElement e : _removeSet) {
    e.remove(pmgr, _caseIDForNet);  // Remove all tokens
}

// Can also cancel from external trigger
netRunner.cancel([cancelledElement]);
```

**CRE Erlang Implementation:**
```erlang
%% Region cancellation via API
cancel_region(Pid, RegionID) ->
    gen_server:call(Pid, {cancel_region, RegionID}).

%% Regions defined in wrapper_state
-record(wrapper_state, {
    regions = #{} :: #{binary() => [atom()]}
}).

handle_call({cancel_region, RegionID}, _, State) ->
    Places = maps:get(RegionID, State#wrapper_state.regions, []),
    NewMarking = lists:foldl(fun(P, M) ->
        maps:update_with(P, fun([_|T]) -> T end, [], M)
    end, State#wrapper_state.marking, Places),
    {reply, ok, State#wrapper_state{marking = NewMarking}}.
```

**Key Difference:** CRE provides runtime region definitions; YAWL regions are fixed at specification time.

---

### P39: Critical Section

**YAWL Java Implementation:**
```java
// Critical section via resource constraints
// Uses YResourcing framework with mutex-like behavior

YResource资源 = new YResource("lock_id");
task.setResourcingID("lock_id");
资源.setConstraint(new MutualExclusionConstraint());
```

**CRE Erlang Implementation:**
```erlang
%% Critical section using ETS-based locking
place_lst() -> [
    p_start, p_lock_request, p_lock_wait,
    p_lock_acquired, p_critical, p_critical_done,
    p_lock_release, p_complete
].

%% Acquire lock (via ETS)
is_enabled(t_acquire_lock, #{p_lock_wait := [Token | Rest]}, _) ->
    try ets:lookup(critical_section_locks, LockID) of
        [] -> true;  % Lock available
        _  -> false  % Lock held
    catch
        _:_ -> false
    end.

fire(t_acquire_lock, _, _) ->
    ets:insert(critical_section_locks, {LockID, self()}),
    {produce, #{p_lock_wait => [], p_lock_acquired => [token]}}.
```

**Key Difference:** CRE uses ETS for distributed locking; YAWL uses resource constraint framework.

---

## Advanced Pattern Comparisons

### P28: Blocking Discriminator

**Enhancement in CRE (not in YAWL base):**

```erlang
%% Blocks new triggers until reset complete
%% Prevents overlap between discrimination cycles

place_lst() -> [
    p_input, p_block_trigger, p_trigger_ready, p_triggered,
    p_consume, p_resetting, p_ready
].

%% Cannot trigger while resetting
is_enabled(t_trigger, #{p_trigger_ready := [_]}, State) ->
    State#bdisc_state.is_resetting =:= false.
```

### P33: Generalized AND Join

**Enhancement in CRE:**

```erlang
%% N-way join with threshold K (K of N must complete)
-record(gaj_state, {
    threshold :: integer(),  % K
    total :: integer(),      % N
    completed = [] :: [integer()]
}).

is_enabled(t_join, #gaj_state{completed = C, threshold = K})
  when length(C) >= K ->
    true;
is_enabled(t_join, _) ->
    false.
```

### P41/P42: Thread Merge/Split

**Enhancement in CRE:**

```erlang
%% P42: Thread Split - independent threads
%% P41: Thread Merge - collect results

%% Thread split creates independent execution paths
%% No synchronization between them
%% Each produces its own completion token
```

---

## State Machine Comparison

### YAWL Java YNetRunner State Machine

```
[CREATED] -> [STARTING] -> [RUNNING] -> [COMPLETED]
               |            |
               v            v
          [CANCELLED]    [SUSPENDED]
                              |
                              v
                         [RESUMING] -> [RUNNING]
```

### CRE gen_yawl State Machine

```
[INITIALIZED] -> [RUNNING] -> [QUIESCENT] -> [RUNNING] --+
                   |            |            |         |
                   v            v            v         |
              [CANCELLED]   [COMPLETED]   [ERROR]   [TIMEOUT]
                   |            |
                   +------------+
```

### YAWL YWorkItem State Machine

```
[ENABLED] -> [FIRED] -> [EXECUTING] -> [COMPLETE] -> [PARENT]
    |           |           |
    v           v           v
[CANCELLED] [CANCELLED] [FAILED]
```

### CRE Token State Machine (Implicit)

```
[CREATED] -> [p_pending_tasks] -> [p_busy_workers] -> [p_completed]
                                          |
                                          v
                                    [p_failed]
```

---

## Multi-Instance Pattern Comparison Table

| Pattern | Instance Count | When Known | Sync | YAWL Implementation | CRE Implementation |
|---------|---------------|------------|------|-------------------|-------------------|
| P12 | Fixed | Design | No | `MIContinuation` | `multiple_instances_no_sync` |
| P13 | Fixed | Design | Yes | `MIContinuation` | `multiple_instances_sync` |
| P14 | Fixed | Runtime | Yes | Query at launch | `multiple_instances_sync` |
| P15 | Dynamic | Runtime | Yes | Query repeatedly | `multiple_instances_sync` |

---

## Code Structure Comparison

### YAWL Java Package Structure

```
org.yawlfoundation.yawl.elements/
├── YTask.java              # Base task class
├── YAtomicTask.java        # Leaf task
├── YCompositeTask.java     # Subnet call
├── YCondition.java         # Place
├── YFlow.java              # Arc
└── state/
    ├── YMarking.java       # Token multiset
    └── YIdentifier.java    # Case ID
```

### CRE Erlang Module Structure

```
src/patterns/
├── sequence.erl            # P1
├── parallel_split.erl      # P2
├── synchronization.erl     # P3
├── ... (43 pattern modules)
└── explicit_termination.erl # P43

src/core/
├── gen_yawl.erl            # Runtime wrapper
├── gen_pnet.erl            # Petri net behavior
├── yawl_pattern_registry.erl
└── yawl_pattern_expander.erl

src/pnet/
├── pnet_marking.erl        # Marking algebra
├── pnet_mode.erl           # Mode enumeration
└── pnet_choice.erl         # Deterministic choice
```

---

## Performance Characteristics

| Operation | YAWL Java | CRE Erlang |
|-----------|-----------|------------|
| **Process Spawn** | Thread creation (heavy) | Process spawn (lightweight) |
| **State Copy** | Object cloning | Immutable copy (GC friendly) |
| **Synchronization** | Locks/monitors | Message passing (no locks) |
| **Serialization** | Manual (Serializable) | Automatic (term format) |
| **Distribution** | Manual (RMI/custom) | Native (Erlang distribution) |

---

## Migration Guide

### From YAWL Java to CRE Erlang

For each pattern:

1. **Identify the YAWL pattern type** (split/join combination)
2. **Find the corresponding CRE pattern module**
3. **Translate flow logic** to Petri net structure
4. **Map data transformations** to token payloads
5. **Replace exception handling** with let-it-crash + supervisors

### Example Translation

**YAWL Java:**
```java
YTask choice = new YAtomicTask("choice", net);
choice.setSplitType(YTask._XOR);

YFlow flowA = new YFlow(choice, branchA);
flowA.setXpathPredicate("$amount > 1000");

YFlow flowB = new YFlow(choice, branchB);
flowB.setXpathPredicate("$amount <= 1000");
```

**CRE Erlang:**
```erlang
%% Pattern module
-module(exclusive_choice_example).
-behaviour(gen_yawl).

place_lst() -> [p_start, p_choice, p_branch_a, p_branch_b, p_end].

trsn_lst() -> [t_select_a, t_select_b, t_finish].

is_enabled(t_select_a, #{p_start := [{amount, Amount}]}, _) ->
    Amount > 1000;
is_enabled(t_select_b, #{p_start := [{amount, Amount}]}, _) ->
    Amount =< 1000;
is_enabled(_, _, _) ->
    false.

fire(t_select_a, _, _) ->
    {produce, #{p_branch_a => [{selected, a}]}};
fire(t_select_b, _, _) ->
    {produce, #{p_branch_b => [{selected, b}]}}.
```

---

## Verification Checklist

Use this checklist when verifying pattern equivalence:

- [ ] **Soundness**: Option to complete
- [ ] **Proper Completion**: Exactly one completion token
- [ ] **No Dead Transitions**: All transitions can fire
- [ ] **Liveness**: No deadlock in reachable states
- [ ] **Boundedness**: All places have bounded tokens
- [ ] **Reversibility**: Can undo (if required)

---

## References

1. YAWL Source Code: `vendors/yawl/`
2. CRE Source Code: `/Users/sac/cre/src/`
3. Pattern Reference: `/Users/sac/cre/docs/pattern_reference_card.md`
4. Generative Analysis: `/Users/sac/cre/docs/generative_analysis_diagrams.md`

---

**Document Version:** 1.0.0
**Generated:** 2026-02-07
**Author:** CRE Team
