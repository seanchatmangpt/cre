# Advanced YAWL Patterns Reference

## Overview

This document provides detailed documentation for the 14 advanced YAWL workflow control patterns. These patterns handle complex workflow scenarios including arbitrary cycles, advanced partial joins, multi-instance cancellation, trigger-based execution, and generalized synchronization.

## Version Information

- **CRE Version**: v0.3.0
- **Patterns Covered**: 14 advanced patterns (P23-P36, P10)
- **Last Updated**: February 2026

---

## Pattern Categories

| Category | Patterns | Description |
|----------|----------|-------------|
| **Trigger Patterns** | P23, P24 | Event-driven activation based on transient or persistent events |
| **Multi-Instance Control** | P26, P27 | Cancellation and early completion of multiple instance activities |
| **Advanced Discriminators** | P28, P29 | Blocking and cancelling variants of the discriminator pattern |
| **Partial Joins** | P30-P36 | Seven variants of partial synchronization |
| **Advanced Synchronization** | P33 | Generalized AND-join for active branches |
| **Advanced Flow Control** | P10 | Arbitrary cycles with unrestricted backward flow |

---

## 1. Trigger Patterns

### P23: Transient Trigger Pattern

**Module**: `transient_trigger`

**Pattern ID**: WCP-28 / P23

**Purpose**: Enable an activity only when a specific event occurs WHILE a designated task is enabled. The event is "transient" - it only matters during the enabled window and is lost if not consumed immediately.

**Use Case**: Time-sensitive notifications that must be acted upon during a specific window, such as limited-time offers or approval windows.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_enabled, p_event, p_triggered, p_end].

% Transition definitions
trsn_lst() -> [t_enable, t_event, t_trigger, t_complete].

% Initialize with the task that defines the enabled window
init(#{enabled_only_in := Task}) -> #state{enabled_only_in = Task};
init(_) -> #state{enabled_only_in = undefined}.

% Initial marking (empty by default)
init_marking(_Place, _UsrInfo) -> [].
```

**Petri Net Structure**:
```
p_start ──► t_enable ──► p_enabled
                     └───► t_trigger ──► p_triggered ──► t_complete ──► p_end
                                ▲
p_event ──► t_event ────────────┘
```

**Key Behavior**:
- The trigger transition `t_trigger` is only enabled when BOTH `p_enabled` AND `p_event` have tokens
- If the event arrives before the task is enabled, it waits in `p_event`
- If the task becomes enabled after the event, both must be present simultaneously
- Once triggered, the event is consumed (transient - not persistent)

**Usage Example**:
```erlang
% Create a transient trigger for approval window
% Approval must be given WHILE the manager task is enabled
State = transient_trigger:init(#{enabled_only_in => manager_task}),

% The workflow enables the manager task
% Manager must approve (event) while enabled
% If approval comes too late (task disabled), it's rejected
```

**Best Practices**:
- Use for strict time-sensitive operations requiring immediate response
- Document the enabled window clearly for users
- Implement proper timeout handling for the enabled task
- Consider adding fallback behavior for missed events

**Common Pitfalls**:
- Events arriving before the enabled window are lost
- Race conditions between task enablement and event arrival
- No recovery if the trigger is missed
- Difficult to test timing-dependent behavior

---

### P24: Persistent Trigger Pattern

**Module**: `persistent_trigger`

**Pattern ID**: WCP-28 / P24

**Purpose**: Event persists in a pool until consumed by a waiting task. Unlike the transient trigger, the event is not lost if the task is not yet ready.

**Use Case**: Asynchronous notifications that can be processed when the workflow is ready, such as queued work items or buffered messages.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_event_pool, p_consume_ready, p_consumed, p_end].

% Transition definitions
trsn_lst() -> [t_event_arrives, t_consume, t_complete].

% Initialize with the task that will consume the event
init(#{consumed_in := Task}) -> #state{consumed_in = Task};
init(_) -> #state{consumed_in = undefined}.

% Initial marking (empty by default)
init_marking(_Place, _UsrInfo) -> [].
```

**Petri Net Structure**:
```
p_start ─────────────────────┐
                               │
p_event_pool ◄── t_event_arrives
     │
     └───► t_consume ──► p_consumed ──► t_complete ──► p_end
                ▲
                │
      p_consume_ready
```

**Key Behavior**:
- Events accumulate in `p_event_pool` via `t_event_arrives`
- The consume transition requires both an event in the pool AND readiness in `p_consume_ready`
- Events persist until explicitly consumed
- Multiple events can queue in the pool

**Usage Example**:
```erlang
% Create a persistent trigger for buffered work
State = persistent_trigger:init(#{consumed_in => processor_task}),

% Multiple work items can arrive and queue
% The processor consumes them when ready
% No work is lost - everything is processed eventually
```

**Best Practices**:
- Use for buffering work items when processing rate varies
- Monitor pool size to prevent unbounded growth
- Implement backpressure for event producers
- Consider ordering guarantees if needed

**Common Pitfalls**:
- Unbounded event pool can cause memory issues
- No event priority without additional logic
- Ordering may be lost with concurrent event arrival
- Potential for event starvation if consume rate is too slow

---

## 2. Multi-Instance Control Patterns

### P26: Cancel MI Activity Pattern

**Module**: `cancel_mi_activity`

**Pattern ID**: P26

**Purpose**: Cancel all running instances of a multiple instance activity upon receiving a cancellation event.

**Use Case**: Emergency shutdown of parallel processing, such as cancelling all pending approval requests when a document is withdrawn.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_instances, p_cancel_event, p_cancelled, p_end].

% Transition definitions
trsn_lst() -> [t_create_instances, t_cancel, t_complete].

% State record
-record(state, {
    mi_task :: atom(),           % The multiple instance task
    cancel_event :: atom(),      % The cancellation event
    instances = [] :: [term()]   % Running instances
}).

% Initialize with task and cancellation event
init(#{mi_task := Task, cancel_event := Event}) ->
    #state{mi_task = Task, cancel_event = Event}.
```

**Petri Net Structure**:
```
p_start ──► t_create_instances ──► p_instances ──┐
                                                  │
p_cancel_event ───────────► t_cancel ────────────┤
                                                  │
                                                  └──► p_cancelled ──► t_complete ──► p_end
```

**Key Behavior**:
- Creates multiple instances in `p_instances` via `t_create_instances`
- The `t_cancel` transition requires both instances AND a cancel event
- Once cancelled, all instances are terminated
- The cancellation is irreversible

**Usage Example**:
```erlang
% Create a cancellable MI activity for parallel approvals
State = cancel_mi_activity:init(#{
    mi_task => parallel_approval,
    cancel_event => document_withdrawn
}),

% Creates 3 approval instances
% If document withdrawn, all approvals cancelled
```

**Best Practices**:
- Implement proper cleanup for each cancelled instance
- Log cancellation for audit purposes
- Consider partial completion tracking
- Provide notification to cancelled instances

**Common Pitfalls**:
- Instances may be in inconsistent state after cancellation
- No mechanism for partial completion before cancellation
- Resource leaks if cleanup is incomplete
- Difficult to resume after cancellation

---

### P27: Complete MI Activity Pattern

**Module**: `complete_mi_activity`

**Pattern ID**: P27

**Purpose**: Complete all remaining instances of a multiple instance activity early when a completion condition is met, without waiting for all instances to finish naturally.

**Use Case**: Early termination when sufficient results are obtained, such as stopping data collection when confidence threshold is reached.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_instances, p_condition_met, p_completed, p_end].

% Transition definitions
trsn_lst() -> [t_create_instances, t_check_condition, t_complete, t_finish].

% State record
-record(state, {
    mi_task :: atom(),              % The multiple instance task
    complete_condition :: binary(),  % Condition expression
    instances = [] :: [term()],      % Running instances
    completed = false :: boolean()   % Completion flag
}).

% Initialize with task and completion condition
init(#{mi_task := Task, complete_condition := Cond}) ->
    #state{mi_task = Task, complete_condition = Cond}.
```

**Petri Net Structure**:
```
p_start ──► t_create_instances ──► p_instances ──┬──► t_check_condition ──► p_condition_met ──┐
                                                    │                                      │
                                                    └──────────────────────────────────────┼──► t_complete ──► p_completed ──► t_finish ──► p_end
```

**Key Behavior**:
- The `t_complete` transition requires both instances AND condition met
- Early completion terminates remaining instances gracefully
- Unlike cancellation, this is a successful completion

**Usage Example**:
```erlang
% Create an early-completing MI activity for data collection
State = complete_mi_activity:init(#{
    mi_task => data_collector,
    complete_condition => <<"confidence >= 0.95">>
}),

% Creates multiple data collectors
% Stops early when confidence threshold reached
```

**Best Practices**:
- Define clear, measurable completion conditions
- Handle partial results gracefully
- Consider resource cleanup for early-terminated instances
- Log early completion events

**Common Pitfalls**:
- Complex conditions difficult to evaluate correctly
- Premature completion with insufficient data
- Inconsistent state between completed and terminated instances
- No way to resume if condition was met incorrectly

---

## 3. Advanced Discriminator Patterns

### P28: Blocking Discriminator Pattern

**Module**: `blocking_discriminator`

**Pattern ID**: P28

**Purpose**: The first branch to complete triggers the output and BLOCKS all other branches until explicitly cleared. Other branches cannot proceed until the block is released.

**Use Case**: First-response wins scenarios where other responses must be held, such as accepting the first bid in an auction and holding others for potential backup.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_branch1, p_branch2, p_branch3, p_triggered, p_blocked, p_cleared, p_end].

% Transition definitions
trsn_lst() -> [t_split, t_complete1, t_complete2, t_complete3, t_trigger, t_clear, t_finish].

% State record
-record(state, {
    trigger :: atom(),              % Which branch triggered
    blocks_until :: [atom()],        % What conditions clear the block
    triggered = false :: boolean(),  % Trigger state
    blocked = [] :: [atom()]         % Blocked branches
}).

% Initialize with trigger and blocking conditions
init(#{trigger := T, blocks_until := Blocks}) ->
    #state{trigger = T, blocks_until = Blocks}.
```

**Petri Net Structure**:
```
p_start ──► t_split ──►►► p_branch1 ──► t_complete1 ──┐
                      ├─► p_branch2 ──► t_complete2 ──┼──► t_trigger ──► p_triggered ──► t_clear ──► p_cleared ──► t_finish ──► p_end
                      └─► p_branch3 ──► t_complete3 ──┘
                                   │
                                   └──► p_blocked (other branches held here)
```

**Key Behavior**:
- First completion triggers output via `t_trigger`
- Other branches are held in `p_blocked`
- The `t_clear` transition must fire to release blocked branches
- Provides a mechanism to hold backup options

**Usage Example**:
```erlang
% Create a blocking discriminator for auction bidding
State = blocking_discriminator:init(#{
    trigger => winning_bid,
    blocks_until => [auction_closed, winner_declared]
}),

% First bid wins, others held as backup
% Cleared when auction closes
```

**Best Practices**:
- Use when backup options are valuable
- Implement clear timeout for block release
- Document blocking conditions clearly
- Consider resource usage of blocked branches

**Common Pitfalls**:
- Blocked branches consume resources while waiting
- No automatic timeout mechanism built-in
- Complex coordination for block release
- Potential deadlock if clear never fires

---

### P29: Cancelling Discriminator Pattern

**Module**: `cancelling_discriminator`

**Pattern ID**: P29

**Purpose**: The first branch to complete triggers the output and CANCELS all other branches immediately. Other branches are terminated, not just blocked.

**Use Case**: Race-to-compute scenarios where only the first result matters, such as trying multiple APIs and using the first response.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_race1, p_race2, p_race3, p_winner, p_cancelled, p_end].

% Transition definitions
trsn_lst() -> [t_start_race, t_win1, t_win2, t_win3, t_cancel_others, t_finish].

% State record
-record(state, {
    race :: [atom()],             % Competing branches
    cancel_rest = true :: boolean(), % Cancel remaining
    winner :: atom() | undefined   % Winning branch
}).

% Initialize with race participants
init(#{race := Race, cancel_rest := Cancel}) ->
    #state{race = Race, cancel_rest = Cancel}.
```

**Petri Net Structure**:
```
p_start ──► t_start_race ──►►► p_race1 ──► t_win1 ──┐
                           ├─► p_race2 ──► t_win2 ──┼──► p_winner ──► t_cancel_others ──► p_cancelled ──► t_finish ──► p_end
                           └─► p_race3 ──► t_win3 ──┘
```

**Key Behavior**:
- All branches start simultaneously via `t_start_race`
- First completion wins via respective `t_winN` transition
- Winner determined immediately, others cancelled via `t_cancel_others`
- No resources wasted on losing branches

**Usage Example**:
```erlang
% Create a cancelling discriminator for API redundancy
State = cancelling_discriminator:init(#{
    race => [api_primary, api_backup, api_fallback],
    cancel_rest => true
}),

% Try all APIs simultaneously
% First response wins, others cancelled
```

**Best Practices**:
- Use for redundant operations with identical results
- Ensure cancellation is clean and resource-efficient
- Consider adding timeout for maximum wait time
- Log which branch won for analysis

**Common Pitfalls**:
- Cancellation overhead may exceed just letting all finish
- No guarantee all branches produce equivalent results
- Difficult to debug race conditions
- Potential resource leaks if cancellation is incomplete

---

## 4. Partial Join Patterns

### P30: Structured Partial Join (N-of-M) Pattern

**Module**: `structured_partial_join`

**Pattern ID**: P30

**Purpose**: Wait for N out of M branches to complete before proceeding. A quorum-based join where not all branches need to finish.

**Use Case**: Voting systems, consensus with fault tolerance, or any scenario where a threshold is sufficient.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_branch1, p_branch2, p_branch3, p_partial_ready, p_end].

% Transition definitions
trsn_lst() -> [t_split, t_complete1, t_complete2, t_complete3, t_partial_join, t_finish].

% State record
-record(state, {
    m :: pos_integer(),    % Total branches (M)
    n :: pos_integer(),    % Required for quorum (N)
    completed = 0 :: non_neg_integer()  % Completion counter
}).

% Initialize with M and N
init(#{m := M, n := N}) ->
    #{m => M, n => N, completed => 0}.
```

**Petri Net Structure**:
```
p_start ──► t_split ──►►► p_branch1 ──► t_complete1 ──┐
                      ├─► p_branch2 ──► t_complete2 ──┼──► t_partial_join ──► p_partial_ready ──► t_finish ──► p_end
                      └─► p_branch3 ──► t_complete3 ──┘
                                 (enabled when N of M complete)
```

**Key Behavior**:
- Spawns M branches via `t_split`
- `t_partial_join` enabled when N branches have completed
- Proceeds without waiting for all branches
- Remaining branches may continue or be cancelled separately

**Usage Example**:
```erlang
% Create a 2-of-3 partial join for consensus
State = structured_partial_join:init(#{
    m => 3,  % 3 total branches
    n => 2   % Need 2 to proceed
}),

% Proceeds when any 2 branches complete
% Third branch can finish or be cancelled
```

**Best Practices**:
- Choose N based on fault tolerance requirements (N > M/2 for majority)
- Consider what to do with remaining branches
- Document quorum rules clearly
- Handle partial results aggregation

**Common Pitfalls**:
- Unclear handling of remaining branches
- Results may be inconsistent if branches disagree
- Quorum too low may accept poor results
- Quorum too high reduces fault tolerance benefit

---

### P31: Blocking Partial Join Pattern

**Module**: `blocking_partial_join`

**Pattern ID**: P31

**Purpose**: Produces a PARTIAL output after N branches complete, then blocks until ALL M branches complete for the final output. Two-stage output: partial and final.

**Use Case**: Progressive results where early partial results are useful but final results require all data, such as phased report generation.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_branch1, p_branch2, p_branch3, p_partial_out, p_final_out, p_end].

% Transition definitions
trsn_lst() -> [t_split, t_complete1, t_complete2, t_complete3, t_partial, t_final, t_finish].

% State record
-record(state, {
    m :: pos_integer(),         % Total branches (M)
    n :: pos_integer(),         % Threshold for partial (N)
    partial_out :: atom(),      % Partial output place
    final_out :: atom(),        % Final output place
    completed = 0 :: non_neg_integer()
}).

% Initialize with M, N, and output places
init(#{m := M, n := N, partial_out := Partial, final_out := Final}) ->
    #{m => M, n => N, partial_out => Partial, final_out => Final, completed => 0}.
```

**Petri Net Structure**:
```
p_start ──► t_split ──►►► p_branch1 ──► t_complete1 ──┐
                      ├─► p_branch2 ──► t_complete2 ──┼──► t_partial (N of M) ──► p_partial_out
                      └─► p_branch3 ──► t_complete3 ──┘
                                 │
                                 └──► t_final (all M) ──► p_final_out ──► t_finish ──► p_end
```

**Key Behavior**:
- First output via `t_partial` when N branches complete (N < M)
- Continues blocking until ALL M branches complete
- Final output via `t_final` when all branches done
- Provides both progressive and complete results

**Usage Example**:
```erlang
% Create a blocking partial join for phased reporting
State = blocking_partial_join:init(#{
    m => 5,       % 5 total data sources
    n => 3,       % Preliminary report after 3
    partial_out => preliminary_report,
    final_out => complete_report
}),

% Preliminary report after 3 sources
% Final report after all 5 sources
```

**Best Practices**:
- Use when early partial results provide value
- Clearly document the difference between partial and final outputs
- Consider whether partial results should be updateable
- Handle cases where final results contradict partial results

**Common Pitfalls**:
- Users may confuse partial with final results
- Two-stage output adds complexity to consumers
- Partial results may change when final arrives
- Blocking after partial may waste time

---

### P32: Cancelling Partial Join Pattern

**Module**: `cancelling_partial_join`

**Pattern ID**: P32

**Purpose**: Proceeds when N branches complete and CANCELS all remaining M-N branches. Unlike the blocking partial join, this does not wait for all branches.

**Use Case**: Early exit scenarios where remaining work is unnecessary, such as finding sufficient valid results and ignoring the rest.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_branch1, p_branch2, p_branch3, p_threshold_met, p_cancelled, p_end].

% Transition definitions
trsn_lst() -> [t_split, t_complete1, t_complete2, t_complete3, t_threshold, t_cancel, t_finish].

% State record
-record(state, {
    m :: pos_integer(),                % Total branches (M)
    n :: pos_integer(),                % Cancellation threshold (N)
    cancel_remaining = true :: boolean(), % Cancel flag
    completed = 0 :: non_neg_integer()
}).

% Initialize with M and N
init(#{m := M, n := N, cancel_remaining := Cancel}) ->
    #{m => M, n => N, cancel_remaining => Cancel, completed => 0}.
```

**Petri Net Structure**:
```
p_start ──► t_split ──►►► p_branch1 ──► t_complete1 ──┐
                      ├─► p_branch2 ──► t_complete2 ──┼──► t_threshold (N of M) ──► p_threshold_met ──► t_cancel ──► p_cancelled ──► t_finish ──► p_end
                      └─► p_branch3 ──► t_complete3 ──┘
                                 (remaining branches cancelled)
```

**Key Behavior**:
- Proceeds when N branches complete via `t_threshold`
- Immediately cancels remaining M-N branches via `t_cancel`
- Single output point (not two-stage like blocking partial join)
- Efficient use of resources

**Usage Example**:
```erlang
% Create a cancelling partial join for redundant validation
State = cancelling_partial_join:init(#{
    m => 5,  % 5 validators
    n => 3,  % Need 3 to agree
    cancel_remaining => true
}),

% Once 3 validators agree, proceed
% Cancel remaining 2 validations
```

**Best Practices**:
- Use when remaining work is purely redundant
- Ensure cancellation is clean and efficient
- Consider logging cancelled branches for audit
- Verify that N is sufficient for correctness

**Common Pitfalls**:
- May cancel branches that could find errors
- No opportunity for remaining branches to contribute
- Threshold choice is critical for correctness
- Difficult to recover if threshold was too low

---

## 5. Multi-Instance Partial Join Patterns

### P34: Static Partial Join for MI Pattern

**Module**: `static_partial_join_mi`

**Pattern ID**: P34

**Purpose**: For a multiple instance activity with a fixed pool of M instances, proceed when N instances complete. The pool size is known at design time.

**Use Case**: Parallel processing with fixed worker pool where early exit is acceptable, such as processing a batch with multiple workers.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_instances, p_threshold_met, p_end].

% Transition definitions
trsn_lst() -> [t_create_instances, t_complete_instance, t_threshold, t_finish].

% State record
-record(state, {
    total_instances :: pos_integer(),  % M: Total instances
    threshold :: pos_integer(),         % N: Completion threshold
    completed = 0 :: non_neg_integer()
}).

% Initialize with total instances and threshold
init(#{total_instances := Total, threshold := Threshold}) ->
    #{total_instances => Total, threshold => Threshold, completed => 0}.
```

**Petri Net Structure**:
```
p_start ──► t_create_instances ──► p_instances ──► t_complete_instance
                                          │
                                          └──► t_threshold (N of M complete) ──► p_threshold_met ──► t_finish ──► p_end
```

**Key Behavior**:
- Creates fixed M instances via `t_create_instances`
- Instances complete via `t_complete_instance`
- Proceeds when N instances have completed
- Remaining instances continue running unless externally cancelled

**Usage Example**:
```erlang
% Create a static partial join for batch processing
State = static_partial_join_mi:init(#{
    total_instances => 10,  % 10 workers
    threshold => 7          % Proceed when 7 done
}),

% 10 workers process batch items
% Continue when 7 complete
```

**Best Practices**:
- Use when parallelism is fixed at design time
- Consider what to do with remaining instances
- Monitor for slow instances that may delay final cleanup
- Set threshold based on performance requirements

**Common Pitfalls**:
- No automatic cancellation of remaining instances
- Fixed pool size may not match runtime needs
- Threshold too high defeats the purpose
- Threshold too low may produce incomplete results

---

### P35: Cancelling Partial Join for MI Pattern

**Module**: `cancelling_partial_join_mi`

**Pattern ID**: P35

**Purpose**: For a multiple instance activity, proceed when N instances complete and CANCEL all remaining M-N instances. Similar to P32 but specifically for MI activities.

**Use Case**: Parallel search or validation where successful results make remaining work unnecessary.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_instances, p_threshold_met, p_cancelled, p_end].

% Transition definitions
trsn_lst() -> [t_create_instances, t_complete_instance, t_threshold, t_cancel, t_finish].

% State record
-record(state, {
    total_instances :: pos_integer(),  % M: Total instances
    threshold :: pos_integer(),         % N: Cancellation threshold
    completed = 0 :: non_neg_integer(),
    cancelled = false :: boolean()
}).

% Initialize with total instances and threshold
init(#{total_instances := Total, threshold := Threshold}) ->
    #{total_instances => Total, threshold => Threshold,
      completed => 0, cancelled => false}.
```

**Petri Net Structure**:
```
p_start ──► t_create_instances ──► p_instances ──┬──► t_complete_instance
                                                  │
                                                  └──► t_threshold (N of M) ──► p_threshold_met ──► t_cancel ──► p_cancelled ──► t_finish ──► p_end
```

**Key Behavior**:
- Creates M instances via `t_create_instances`
- When N complete, triggers cancellation via `t_threshold` and `t_cancel`
- Remaining instances are explicitly cancelled
- Efficient resource cleanup

**Usage Example**:
```erlang
% Create a cancelling partial join for parallel search
State = cancelling_partial_join_mi:init(#{
    total_instances => 20,
    threshold => 5  % Need 5 successful results
}),

% Search with 20 parallel workers
% Cancel remaining 15 when 5 succeed
```

**Best Practices**:
- Use when early success makes remaining work redundant
- Implement clean cancellation for each instance
- Consider threshold carefully based on success probability
- Log cancellations for analysis

**Common Pitfalls**:
- May cancel instances that would succeed later
- Cancellation overhead may exceed just letting finish
- No mechanism to restart if cancelled instances were needed
- Difficult to choose optimal threshold

---

### P36: Dynamic Partial Join for MI Pattern

**Module**: `dynamic_partial_join_mi`

**Pattern ID**: P36

**Purpose**: Similar to P34, but the threshold N is computed DYNAMICALLY at runtime based on an expression, not fixed at design time. Allows adaptive behavior based on runtime conditions.

**Use Case**: Adaptive parallelism where the completion threshold depends on data characteristics, such as confidence-based data collection.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_instances, p_threshold_met, p_end].

% Transition definitions
trsn_lst() -> [t_create_instances, t_complete_instance, t_compute_threshold, t_threshold, t_finish].

% State record
-record(state, {
    threshold_expr :: binary(),     % Expression to compute threshold
    threshold :: pos_integer() | undefined,
    completed = 0 :: non_neg_integer()
}).

% Initialize with threshold expression
init(#{threshold_expr := Expr}) ->
    #{threshold_expr => Expr, threshold => undefined, completed => 0}.

% Default threshold computation
compute_threshold(State) ->
    case State#state.threshold_expr of
        <<"ceil(attendance_estimate*0.08)">> ->
            %% Example: 800 * 0.08 = 64
            64;
        _ ->
            3
    end.
```

**Petri Net Structure**:
```
p_start ──► t_create_instances ──► p_instances ──┬──► t_complete_instance
                                                  │
                                                  └──► t_compute_threshold ──► p_threshold_met ──► t_threshold ──► t_finish ──► p_end
```

**Key Behavior**:
- Creates instances via `t_create_instances`
- Computes threshold dynamically via `t_compute_threshold`
- Threshold expression evaluated at runtime
- Proceeds when completed count >= computed threshold

**Usage Example**:
```erlang
% Create a dynamic partial join for adaptive data collection
State = dynamic_partial_join_mi:init(#{
    threshold_expr => <<"ceil(attendance_estimate * 0.08)">>
}),

% For 800 expected attendees, need 64 responses
% Threshold computed at runtime based on estimates
```

**Best Practices**:
- Use simple, well-tested threshold expressions
- Provide fallback for expression evaluation failures
- Document the expression syntax and variables
- Consider caching threshold if recomputation is expensive

**Common Pitfalls**:
- Complex expressions difficult to debug
- Expression evaluation may fail at runtime
- Threshold may be inappropriate for actual conditions
- No validation of expression before execution

---

## 6. Advanced Synchronization Patterns

### P33: Generalized AND-Join Pattern

**Module**: `generalized_and_join`

**Pattern ID**: P33

**Purpose**: Wait for all ACTIVE branches to complete, not necessarily all defined branches. Branches can be dynamically enabled or disabled at runtime.

**Use Case**: Conditional parallelism where some branches may not execute based on runtime conditions.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_branch1, p_branch2, p_branch3, p_join_ready, p_joined, p_end].

% Transition definitions
trsn_lst() -> [t_split, t_complete1, t_complete2, t_complete3, t_join, t_finish].

% State record
-record(state, {
    active_branches = [] :: [atom()],  % Dynamically determined active branches
    joined = false :: boolean()
}).

% Initialize with active branches
init(#{active_branches := Active}) ->
    #{active_branches => Active, joined => false}.
```

**Petri Net Structure**:
```
p_start ──► t_split ──►►► p_branch1 (if active) ──► t_complete1 ──┐
                      ├─► p_branch2 (if active) ──► t_complete2 ──┼──► t_join ──► p_joined ──► t_finish ──► p_end
                      └─► p_branch3 (if active) ──► t_complete3 ──┘
                                 (only waits for ACTIVE branches)
```

**Key Behavior**:
- `t_split` only creates tokens for active branches
- `t_join` waits for all active branches to complete
- Inactive branches are ignored entirely
- Active branch set determined at runtime

**Usage Example**:
```erlang
% Create a generalized AND-join with dynamic branches
State = generalized_and_join:init(#{
    active_branches => [p_branch1, p_branch3]  % branch2 is inactive
}),

% Only branches 1 and 3 execute
% Join waits only for active branches
```

**Best Practices**:
- Use when branch execution is conditionally determined
- Clearly document how branches are activated/deactivated
- Consider adding validation for active branch consistency
- Handle case where no branches are active

**Common Pitfalls**:
- Empty active branch set causes immediate join
- Difficult to track which branches should be active
- No validation that inactive branches are safe to skip
- Complex state management with dynamic activation

---

## 7. Advanced Flow Control Patterns

### P10: Arbitrary Cycles Pattern

**Module**: `arbitrary_cycles`

**Pattern ID**: WCP-31 / P10

**Purpose**: Allows arbitrary cycles in the workflow, enabling tokens to be sent back to ANY previous node, not just the immediately preceding one. Enables complex iterative and recursive workflows.

**Use Case**: Complex workflows requiring non-linear backward flow, such as retry mechanisms with multiple fallback points or state machines with arbitrary transitions.

**API Functions**:

```erlang
% Place definitions
place_lst() -> [p_start, p_cycle_pool, p_end].

% Transition definitions
trsn_lst() -> [t_start, t_cycle, t_exit].

% State record
-record(state, {
    nodes :: [atom()],           % All nodes in the workflow
    cycles :: #{atom() => [atom()]}  % Allowed cycle targets per node
}).

% Initialize with nodes and cycle definitions
init(NetArg) when is_map(NetArg) ->
    NetArg.
```

**Petri Net Structure**:
```
p_start ──► t_start ──► p_cycle_pool ──┬──► t_cycle (back to pool or any node)
                                        │
                                        └──► t_exit ──► p_end
```

**Key Behavior**:
- `t_start` populates the cycle pool
- `t_cycle` can send tokens back to any previous node
- `t_exit` allows leaving the cycle structure
- Enables arbitrary workflow graphs

**Usage Example**:
```erlang
% Create an arbitrary cycle pattern for retry logic
State = arbitrary_cycles:init(#{
    nodes => [step1, step2, step3, validation],
    cycles => #{
        validation => [step1, step2, step3],  % Can retry from any step
        step3 => [step2]                       % step3 can go back to step2
    }
}),

% Validation failure can retry from any step
% Complex retry logic possible
```

**Best Practices**:
- Use for complex state machines and retry logic
- Clearly document allowed cycle transitions
- Implement safeguards to prevent infinite loops
- Consider adding maximum cycle depth limits

**Common Pitfalls**:
- Easy to create infinite loops
- Difficult to reason about workflow behavior
- No built-in cycle detection or prevention
- Complex visualization and debugging

---

## Pattern Comparison Matrix

| Pattern | Use Case | Key Feature | Complexity |
|---------|----------|-------------|------------|
| **P23 Transient Trigger** | Time-sensitive events | Event lost if not consumed during window | Medium |
| **P24 Persistent Trigger** | Buffered events | Event persists until consumed | Low |
| **P26 Cancel MI** | Emergency shutdown | Cancels all MI instances | Low |
| **P27 Complete MI** | Early completion | Completes MI when condition met | Medium |
| **P28 Blocking Discriminator** | Hold backup options | Blocks losing branches | Medium |
| **P29 Cancelling Discriminator** | Race-to-compute | Cancels losing branches | Medium |
| **P30 Structured Partial Join** | Quorum-based | N-of-M proceed | Low |
| **P31 Blocking Partial Join** | Progressive results | Partial then final output | Medium |
| **P32 Cancelling Partial Join** | Early exit | N-of-M, cancel rest | Low |
| **P33 Generalized AND-Join** | Conditional branches | Join active branches only | Medium |
| **P34 Static Partial Join MI** | Fixed pool MI | N-of-M fixed instances | Low |
| **P35 Cancelling Partial Join MI** | Early exit MI | N-of-M MI, cancel rest | Medium |
| **P36 Dynamic Partial Join MI** | Adaptive MI | Threshold computed at runtime | High |
| **P10 Arbitrary Cycles** | Complex flow | Cycles to any previous node | High |

---

## Selection Guide

### Choose Trigger Patterns When:
- Event timing is critical: **P23 Transient Trigger**
- Events can be buffered: **P24 Persistent Trigger**

### Choose MI Control When:
- Need emergency shutdown: **P26 Cancel MI**
- Need early completion: **P27 Complete MI**

### Choose Discriminator When:
- Need backup options: **P28 Blocking Discriminator**
- Only first result matters: **P29 Cancelling Discriminator**

### Choose Partial Join When:
- Simple quorum needed: **P30 Structured Partial Join**
- Progressive results valuable: **P31 Blocking Partial Join**
- Early exit desired: **P32 Cancelling Partial Join**

### Choose MI Partial Join When:
- Fixed pool with early exit: **P34 Static Partial Join MI**
- Adaptive threshold needed: **P36 Dynamic Partial Join MI**
- Early exit with cancellation: **P35 Cancelling Partial Join MI**

### Choose Synchronization When:
- Branches conditionally active: **P33 Generalized AND-Join**

### Choose Flow Control When:
- Complex backward flow needed: **P10 Arbitrary Cycles**

---

## Implementation Notes

### Common State Fields

All patterns use a common state record structure:
- Configuration parameters (m, n, threshold, etc.)
- Completion counters
- Status flags (triggered, cancelled, completed)

### Initialization Patterns

All patterns support two initialization modes:
1. **Explicit**: `init(#{param := Value})` - Full configuration
2. **Default**: `init(_)`) - Uses sensible defaults

### Best Practices

1. **Always explicitly configure critical parameters** in production
2. **Use defaults only for prototyping**
3. **Add logging for pattern transitions** in production
4. **Monitor pattern state** for long-running workflows
5. **Test edge cases** (empty sets, single items, max values)

---

## References

- YAWL Patterns: https://www.workflowpatterns.com
- Pattern Reference: `docs/YAWL_PATTERNS_REFERENCE.md`
- 43 Patterns Status: `docs/43_PATTERNS_COMPLETE.md`
- Architecture: `docs/ARCHITECTURE.md`
