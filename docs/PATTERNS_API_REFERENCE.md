# CRE Pattern Modules API Reference

This document provides comprehensive API documentation for all 43+ workflow patterns implemented in CRE (Common Runtime Environment), along with Reinforcement Learning strategies and agent APIs.

## Table of Contents

- [Overview](#overview)
- [Control Flow Patterns](#control-flow-patterns)
- [Branching Patterns](#branching-patterns)
- [Synchronization Patterns](#synchronization-patterns)
- [Cancellation Patterns](#cancellation-patterns)
- [Data Patterns](#data-patterns)
- [Resource Patterns](#resource-patterns)
- [Exception Patterns](#exception-patterns)
- [RL Agent API](#rl-agent-api)
- [RL Strategies API](#rl-strategies-api)
- [Infrastructure Patterns](#infrastructure-patterns)

---

## Overview

CRE implements workflow patterns using the `gen_yawl` behavior, which extends the Petri net algebra (`gen_pnet`). Each pattern module defines:

- **Places**: Locations where tokens reside
- **Transitions**: Actions that consume/produce tokens
- **Markings**: Token distribution across places
- **Presets**: Input places for each transition

### Core Behavior Callbacks

All pattern modules implement these `gen_yawl` callbacks:

```erlang
%% Returns list of places
-spec place_lst() -> [atom()].

%% Returns list of transitions
-spec trsn_lst() -> [atom()].

%% Returns initial marking for a place
-spec init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].

%% Returns input places for a transition
-spec preset(Trsn :: atom()) -> [atom()].

%% Checks if transition is enabled
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) -> boolean().

%% Fires a transition, consuming and producing tokens
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    {produce, map()} | {produce, map(), term()} | abort.
```

### State Records

Most patterns maintain state in a record following this convention:

```erlang
-record(pattern_state, {
    %% Pattern-specific fields
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

---

## Control Flow Patterns

### sequence (WCP-01)

**Module**: `sequence.erl`

**Description**: The most basic control flow pattern - executes tasks sequentially.

**State Record**:
```erlang
-record(state, {
    tasks = [] :: [atom()],
    current = 0 :: non_neg_integer()
}).
```

**Petri Net Structure**:
- **Places**: `[p_start, p_task1, p_task2, p_end]`
- **Transitions**: `[t_start, t_complete1, t_complete2, t_finish]`

**Example**:
```erlang
%% Execute a sequence of tasks
State = #state{tasks = [task_a, task_b, task_c], current = 0},
gen_yawl:start_link(sequence, State, []).
```

---

### parallel_split (WCP-02)

**Module**: `parallel_split.erl`

**Description**: Splits workflow into multiple parallel branches that execute concurrently.

**State Record**:
```erlang
-record(parallel_split_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec execute(BranchFuns :: [function()], InputData :: term()) ->
    {ok, #{pos_integer() => term()}} | {error, term()}.
```

**Petri Net Structure**:
- **Places**: `[p_start, p_branch1, ..., p_branchN, p_sync, p_end]`
- **Transitions**: `[t_split, t_complete1, ..., t_completeN, t_sync, t_finish]`

**Example**:
```erlang
%% Execute three branches in parallel
BranchFuns = [
    fun(Data) -> process_a(Data) end,
    fun(Data) -> process_b(Data) end,
    fun(Data) -> process_c(Data) end
],
{ok, Results} = parallel_split:execute(BranchFuns, Input).
```

---

### synchronization (WCP-03)

**Module**: `synchronization.erl`

**Description**: AND-join pattern - waits for all parallel branches to complete before proceeding.

**Petri Net Structure**:
- **Places**: `[p_start, p_branch1, ..., p_branchN, p_sync, p_end]`
- **Transitions**: `[t_split, t_complete1, ..., t_completeN, t_sync, t_finish]`

**Soundness Properties**:
- Option to complete: Yes
- Proper completion: Yes (all branches must complete)
- No dead transitions: Yes

---

### structured_loop (WCP-22/23)

**Module**: `structured_loop.erl`

**Description**: Implements while and until loop constructs for repetitive workflow execution.

**State Record**:
```erlang
-record(loop_state, {
    body_fun :: function(),
    loop_type :: while | until,
    condition_fun :: function(),
    current_state :: term(),
    iteration_count = 0 :: non_neg_integer(),
    max_iterations = 1000 :: pos_integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec execute(BodyFun :: function(), LoopType :: while | until,
             ConditionFun :: function(), InitialState :: term()) ->
    {ok, term()} | {error, term()}.
```

**Example**:
```erlang
%% While loop - execute while condition is true
{ok, Result} = structured_loop:execute(
    fun(State) -> process(State) end,
    while,
    fun(State) -> State < 10 end,
    0
).

%% Until loop - execute until condition becomes true
{ok, Result} = structured_loop:execute(
    fun(State) -> process(State) end,
    until,
    fun(State) -> State >= 10 end,
    0
).
```

---

### recursion (WCP-24)

**Module**: `recursion.erl`

**Description**: Allows a workflow to call itself recursively with new data.

**Example**:
```erlang
%% Recursive factorial pattern
State = #recursion_state{
    recursion_fun = fun(N) when N > 0 -> N * recurse(N - 1); (0) -> 1 end,
    base_case = 0
}.
```

---

### implicit_termination (WCP-11)

**Module**: `implicit_termination.erl`

**Description**: Automatically terminates the workflow when no more work is available.

**State Record**:
```erlang
-record(implicit_termination_state, {
    subprocess :: function(),
    input_data :: term() | undefined,
    work_remaining = true :: boolean(),
    inputs_satisfied = true :: boolean(),
    result :: undefined | term(),
    log_id :: binary() | undefined
}).
```

---

### explicit_termination (WCP-10)

**Module**: `explicit_termination.erl`

**Description**: Terminates the workflow when a specific condition is met.

---

### arbitrary_cycles (WCP-21)

**Module**: `arbitrary_cycles.erl`

**Description**: Allows for arbitrary cyclic execution patterns in workflows.

---

## Branching Patterns

### exclusive_choice (WCP-04)

**Module**: `exclusive_choice.erl`

**Description**: XOR branch selection - exactly one branch is selected and executed.

**State Record**:
```erlang
-record(exclusive_choice_state, {
    branches :: map(),
    selected :: undefined | atom(),
    branch_count :: pos_integer(),
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec select_branch(Branches :: map(), SelectionStrategy :: atom()) ->
    {ok, atom()} | {error, term()}.
```

**Petri Net Structure**:
- **Places**: `[p_start, p_choice, p_branch1, ..., p_branchN, p_end]`
- **Transitions**: `[t_select, t_branch1, ..., t_branchN, t_finish]`

**Example**:
```erlang
Branches = #{
    fast_path => fun(Data) -> fast_process(Data) end,
    slow_path => fun(Data) -> slow_process(Data) end
},
{ok, Selected} = exclusive_choice:select_branch(Branches, random).
```

---

### simple_merge (WCP-05)

**Module**: `simple_merge.erl`

**Description**: XOR merge - merges multiple exclusive branches back together.

**Petri Net Structure**:
- **Places**: `[p_branch1, ..., p_branchN, p_merge, p_end]`
- **Transitions**: `[t_complete1, ..., t_completeN, t_merge, t_finish]`

---

### multiple_choice (WCP-06)

**Module**: `multiple_choice.erl`

**Description**: Selects and executes multiple branches simultaneously (OR-split).

**State Record**:
```erlang
-record(multiple_choice_state, {
    branches :: #{atom() => {function(), function()}},
    input_data :: term() | undefined,
    selected = [] :: [atom()],
    completed = [] :: [atom()],
    results = #{} :: #{atom() => term()},
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec select_multiple(Branches :: map(), Selector :: function()) ->
    {ok, [atom()]} | {error, term()}.
```

---

### multiple_merge (WCP-07)

**Module**: `multiple_merge.erl`

**Description**: Merges multiple paths that may have executed concurrently.

**State Record**:
```erlang
-record(multiple_merge_state, {
    path_count :: pos_integer(),
    path_funs :: [function()],
    completed = [] :: [pos_integer()],
    output_count = 0 :: non_neg_integer(),
    log_id :: binary() | undefined
}).
```

---

### discriminator (WCP-09)

**Module**: `discriminator.erl`

**Description**: Triggers on the first branch completion, ignores others until reset.

**State Record**:
```erlang
-record(discriminator_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    triggered_by :: undefined | pos_integer(),
    cycle_count = 0 :: non_neg_integer(),
    log_id :: binary() | undefined,
    threshold :: undefined | pos_integer(),
    counter = 0 :: non_neg_integer(),
    triggered = false :: boolean()
}).
```

**API Functions**:
```erlang
-spec discriminator_wait(Marking :: marking(), CounterKey :: term()) ->
    {ok, discriminator_state()}.
-spec discriminator_trigger(Marking :: marking(), CounterKey :: term()) ->
    {ok, discriminator_state()}.
-spec discriminator_reset(CounterKey :: term()) -> ok.
```

---

### n_out_of_m (WCP-22 variant)

**Module**: `n_out_of_m.erl`

**Description**: Quorum-based synchronization - waits for N of M branches to complete.

**State Record**:
```erlang
-record(n_out_of_m_state, {
    m :: pos_integer(),
    n :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = [] :: [{pos_integer(), term()}],
    quorum_met = false :: boolean(),
    wait_for_all = false :: boolean(),
    log_id :: binary() | undefined
}).
```

**Example**:
```erlang
%% Wait for 3 of 5 branches (quorum)
BranchFuns = [fun() -> task1() end, fun() -> task2() end, ...],
State = #n_out_of_m_state{m = 5, n = 3, branch_funs = BranchFuns}.
```

---

### deferred_choice (WCP-16)

**Module**: `deferred_choice.erl`

**Description**: defers branch selection until runtime - selects the first available option.

**State Record**:
```erlang
-record(deferred_choice_state, {
    options :: map(),
    selected :: undefined | atom(),
    discarded = [] :: [atom()],
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec deferred_choice_trigger(Options :: map(), Context :: map()) ->
    {ok, atom()} | {error, term()}.
-spec enabled_branches(Options :: map(), Context :: map()) -> [atom()].
-spec select_branch(Branches :: [atom()], SelectionFun :: function()) ->
    {ok, atom()} | {error, term()}.
```

**Strategy Integration**:
The deferred_choice pattern integrates with RL strategies:
- `strategy_q_learning`: Q-learning with epsilon-greedy exploration
- `strategy_ucb`: Upper Confidence Bound
- `strategy_contextual`: Context-aware bandit
- `strategy_thompson_sampling`: Thompson sampling
- `strategy_first_n`: First-N completion strategy
- `strategy_fastest_n`: Fastest-N completion strategy

---

### interleaved_routing (WCP-17 variant)

**Module**: `interleaved_routing.erl`

**Description**: Routes work items through parallel paths in an interleaved fashion.

---

### interleaved_parallel (WCP-17)

**Module**: `interleaved_parallel.erl`

**Description**: Round-robin mutex pattern - fair interleaved execution of parallel branches.

**State Record**:
```erlang
-record(interleave_state, {
    active_branch :: undefined | pos_integer(),
    pending_branches :: [pos_integer()],
    completed_branches :: [pos_integer()],
    total_branches :: pos_integer(),
    cycle_count = 0 :: non_neg_integer()
}).
```

**API Functions**:
```erlang
-spec interleave_start(BranchCount :: pos_integer()) -> interleave_state().
-spec interleave_next(State :: interleave_state()) -> pos_integer().
-spec interleave_complete(State :: interleave_state(), Branch :: pos_integer()) ->
    interleave_state().
-spec interleave_sync(State :: interleave_state()) -> {completed | pending, interleave_state()}.
```

---

## Synchronization Patterns

### or_join (WCP-09 variant)

**Module**: `or_join.erl`

**Description**: OR-join synchronization - the most complex sync pattern. Waits for all active threads that CAN reach the join, but proceeds if a thread is stuck elsewhere.

**State Record**:
```erlang
-record(or_join_state, {
    branch_count = 3 :: pos_integer(),
    branch_funs = [] :: [function()],
    completed = [] :: [pos_integer()],
    triggered_by :: undefined | pos_integer(),
    net_structure = #{} :: map(),
    active_branches = [] :: [pos_integer()],
    cycle_count = 0 :: non_neg_integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
%% Analysis functions
-spec or_join_trigger(JoinTransition :: atom(), Marking :: marking()) -> boolean().
-spec active_threads(Marking :: marking(), NetStructure :: net_structure(),
                     PotentialThreads :: [atom()]) -> [atom()].
-spec can_reach_join(Source :: atom(), NetStructure :: net_structure()) -> boolean().
-spec or_join_semantics(Marking :: marking()) ->
    {ok, marking()} | {error, term()}.

%% Execution functions
-spec new(BranchFuns :: [function()], BranchCount :: pos_integer()) -> or_join_state().
-spec start(BranchFuns :: [function()]) -> {ok, pid()} | {error, term()}.
-spec run(BranchFuns :: [function()]) ->
    {ok, {pos_integer(), term()}} | {error, term()}.
-spec execute(BranchFuns :: [function()], InputData :: term()) ->
    {ok, {pos_integer(), term()}} | {error, term()}.
-spec get_state(Pid :: pid()) -> {ok, or_join_state()} | {error, term()}.
-spec reset(Pid :: pid()) -> ok | {error, term()}.
```

**OR-Join Semantics**:
1. Wait for all active threads that CAN reach the join
2. Proceed if a thread cannot reach the join (structurally stuck)
3. Trigger on first arrival after waiting set is determined

**Petri Net Structure**:
- **Places**: `[p_start, p_branch1, p_branch2, p_branch3, p_arrived, p_joined, p_end]`
- **Transitions**: `[t_split, t_complete1, t_complete2, t_complete3, t_join, t_finish]`

---

### general_sync_merge (WCP-??)

**Module**: `general_sync_merge.erl`

**Description**: Generalized synchronization merge for complex sync scenarios.

---

### local_sync_merge (WCP-??)

**Module**: `local_sync_merge.erl`

**Description**: Local synchronization merge within a region.

---

### structured_sync_merge (WCP-??)

**Module**: `structured_sync_merge.erl`

**Description**: Structured merge with explicit synchronization points.

---

### blocking_discriminator

**Module**: `blocking_discriminator.erl`

**Description**: Discriminator that blocks after first trigger until reset.

---

### blocking_partial_join

**Module**: `blocking_partial_join.erl`

**Description**: Partial join with blocking semantics.

---

### cancelling_discriminator

**Module**: `cancelling_discriminator.erl`

**Description**: Discriminator that cancels remaining branches on trigger.

---

### cancelling_partial_join

**Module**: `cancelling_partial_join.erl`

**Description**: Partial join that cancels incomplete branches.

---

### cancelling_partial_join_mi

**Module**: `cancelling_partial_join_mi.erl`

**Description**: Cancelling partial join for multiple instances.

---

### static_partial_join_mi

**Module**: `static_partial_join_mi.erl`

**Description**: Static partial join for multiple instances.

---

### dynamic_partial_join_mi

**Module**: `dynamic_partial_join_mi.erl`

**Description**: Dynamic partial join for multiple instances.

---

## Cancellation Patterns

### cancel_activity (P19)

**Module**: `cancel_activity.erl`

**Description**: Cancel a single activity within the workflow.

**State Record**:
```erlang
-record(state, {
    target :: atom(),
    cancel_event :: atom(),
    cancelled = false :: boolean()
}).
```

**Petri Net Structure**:
- **Places**: `[p_start, p_active, p_cancel_event, p_cancelled, p_end]`
- **Transitions**: `[t_start, t_cancel, t_finish]`

---

### cancel_case (P20)

**Module**: `cancel_case.erl`

**Description**: Cancel the entire workflow case.

**State Record**:
```erlang
-record(state, {
    cancel_event :: atom(),
    cancelled = false :: boolean()
}).
```

---

### cancel_region (P25)

**Module**: `cancel_region.erl`

**Description**: Cancel all activities within a specific region.

**State Record**:
```erlang
-record(region, {
    id :: binary(),
    parent_id :: undefined | binary(),
    places :: [atom()],
    transitions :: [atom()],
    child_regions :: sets:set(binary()),
    status :: active | cancelled
}).
```

**API Functions**:
```erlang
-spec define_region(RegionId :: binary(), Places :: [atom()]) -> ok.
-spec define_region(RegionId :: binary(), ParentId :: binary() | undefined,
                    Places :: [atom()]) -> ok.
-spec cancel_region(RegionId :: binary()) -> ok.
-spec cancel_case(CaseId :: binary()) -> ok.
-spec get_active_regions(CaseId :: binary()) -> [region_id()].
-spec register_activity(ActivityId :: binary(), RegionId :: binary(),
                        Place :: atom()) -> ok.
-spec register_activity(ActivityId :: binary(), RegionId :: binary(),
                        Place :: atom(), Pid :: pid()) -> ok.
-spec unregister_activity(ActivityId :: binary()) -> ok.
```

---

### cancel_mi_activity

**Module**: `cancel_mi_activity.erl`

**Description**: Cancel activity in a multiple instance context.

---

### cancellation (Utilities)

**Module**: `cancellation.erl`

**Description**: Comprehensive cancellation utilities for workflow exception handling.

**Type Definitions**:
```erlang
-type region() :: #{
    name => atom(),
    places => [place()],
    transitions => [transition()],
    parent => atom() | undefined
}.
-type marking() :: #{place() => [term()]}.
```

**API Functions**:
```erlang
-spec define_region(Name :: atom(), Places :: [place()], Transitions :: [transition()]) ->
    region().
-spec cancel_activity_region(Marking :: marking(), TriggerPlace :: place(), Region :: region()) ->
    marking().
-spec cancel_case_region(Marking :: marking(), Region :: region()) -> marking().
```

---

### exception_patterns

**Module**: `exception_patterns.erl`

**Description**: Workflow exception handling patterns (WHP-01 through WHP-05).

**Type Definitions**:
```erlang
-type marking() :: #{atom() => [term()]}.
-type control_state() :: #{
    cancelled_activities => [atom()],
    exception_state => exception_state() | undefined,
    retry_counts => #{atom() => non_neg_integer()},
    compensation_stack => [compensation()]
}.
-type exception_state() :: #{
    type => atom(),
    reason => term(),
    source => atom() | undefined,
    timestamp => integer(),
    handled => boolean(),
    escalated => boolean()
}.
-type compensation() :: #{
    activity_id => atom(),
    handler => function(),
    state => pending | executing | completed | failed,
    data => map()
}.
-type retry_strategy() :: exponential | linear | constant | fibonacci.
-type retry_config() :: #{
    max_attempts => non_neg_integer(),
    strategy => retry_strategy(),
    base_delay => non_neg_integer(),
    max_delay => non_neg_integer(),
    multiplier => float()
}.
```

**API Functions**:
```erlang
%% Cancellation
-spec cancel_activity(Marking :: marking(), Activity :: atom()) -> marking().
-spec cancel_case(Marking :: marking()) -> marking().
-spec cancel_region(Marking :: marking(), Region :: [atom()]) -> marking().

%% Compensation
-spec compensation_handler(Marking :: marking(), Handler :: function()) ->
    pattern_result().
-spec trigger_compensation(Marking :: marking(), Activity :: atom()) ->
    pattern_result().
-spec consecutive_compensation(Marking :: marking()) -> pattern_result().

%% Retry
-spec retry_activity(Activity :: atom(), Config :: retry_config()) ->
    pattern_result().
-spec retry_with_backoff(Activity :: atom(), Attempt :: non_neg_integer(),
                         Config :: retry_config()) -> {ok, non_neg_integer()}.

%% Escalation
-spec escalate_exception(Marking :: marking(), Exception :: exception_state()) ->
    pattern_result().
-spec propagate_exception(Marking :: marking(), Exception :: exception_state()) ->
    pattern_result().

%% Utilities
-spec is_cancelled(Marking :: marking()) -> boolean().
-spec mark_cancelled(Marking :: marking(), Activity :: atom()) -> marking().
-spec get_cancelled_activities(Marking :: marking()) -> [atom()].
-spec clear_cancelled(Marking :: marking()) -> marking().
```

---

## Data Patterns

### data_distribute (WDP-03)

**Module**: `data_distribute.erl`

**Description**: Distributes data to multiple recipients (one-to-many distribution).

**State Record**:
```erlang
-record(data_distribute_state, {
    data :: term(),
    targets :: [term()],
    distributed_count = 0 :: non_neg_integer(),
    start_time :: integer()
}).
```

**API Functions**:
```erlang
-spec new(Data :: term(), Targets :: [term()], TargetCount :: pos_integer()) ->
    data_distribute_state().
-spec start(Data :: term(), Targets :: [term()]) -> {ok, pid()} | {error, term()}.
-spec get_state(Pid :: pid()) -> {ok, data_distribute_state()} | {error, term()}.
-spec distribute(Data :: term(), Targets :: [term()],
                DistributionFun :: function()) -> {ok, [term()]}.
```

**Petri Net Structure**:
- **Places**: `[p_start, p_targets, p_distributed, p_end]`
- **Transitions**: `[t_distribute, t_finish]`

**Example**:
```erlang
%% Distribute data to multiple targets
Fun = fun(Data, Target) -> send_to(Data, Target) end,
{ok, Results} = data_distribute:distribute(Data, [t1, t2, t3], Fun).
```

---

### data_accumulate (WDP-04)

**Module**: `data_accumulate.erl`

**Description**: Accumulates data from multiple sources (many-to-one aggregation).

**State Record**:
```erlang
-record(data_accumulate_state, {
    sources :: [term()],
    accumulator_fun :: function(),
    collected_data = [] :: [term()],
    source_count :: pos_integer(),
    start_time :: integer()
}).
```

**API Functions**:
```erlang
-spec new(Sources :: [term()], AccumulatorFun :: function(),
         SourceCount :: pos_integer()) -> data_accumulate_state().
-spec start(Sources :: [term()], AccumulatorFun :: function()) ->
    {ok, pid()} | {error, term()}.
-spec get_state(Pid :: pid()) -> {ok, data_accumulate_state()} | {error, term()}.
-spec accumulate(Sources :: [term()], AccumulatorFun :: function(),
                Initial :: term()) -> {ok, term()}.
```

**Petri Net Structure**:
- **Places**: `[p_start, p_collecting, p_accumulated, p_end]`
- **Transitions**: `[t_accumulate, t_finish]`

**Example**:
```erlang
%% Sum all sources
Fun = fun(Acc, X) -> Acc + X end,
{ok, Result} = data_accumulate:accumulate([1, 2, 3, 4], Fun, 0).
```

---

### data_visibility (WDP-05)

**Module**: `data_visibility.erl`

**Description**: Controls data visibility and access within the workflow.

**State Record**:
```erlang
-record(data_visibility_state, {
    data :: term(),
    scope :: term(),
    access_check_fun :: function(),
    access_granted = false :: boolean(),
    start_time :: integer()
}).
```

**API Functions**:
```erlang
-spec new(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
    data_visibility_state().
-spec start(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
    {ok, pid()} | {error, term()}.
-spec get_state(Pid :: pid()) -> {ok, data_visibility_state()} | {error, term()}.
-spec check_visibility(Data :: term(), Scope :: term(),
                      AccessCheckFun :: function()) -> {ok, boolean()}.
```

---

### data_transform

**Module**: `data_transform.erl`

**Description**: Transforms data between workflow steps (WDP-02).

---

### param_pass

**Module**: `param_pass.erl`

**Description**: Parameter passing between workflow activities.

---

## Resource Patterns

### resource_allocation (WRP-04)

**Module**: `resource_allocation.erl`

**Description**: Manages allocation of resources to tasks.

**State Record**:
```erlang
-record(resource_allocation_state, {
    resources :: [term()],
    available :: [term()],
    allocated :: undefined | term(),
    task_id :: term(),
    status = pending :: pending | requesting | allocated | busy | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec new(Resources :: [term()], TaskId :: term()) -> resource_allocation_state().
-spec start(Resources :: [term()], TaskId :: term()) -> {ok, pid()} | {error, term()}.
-spec get_state(Pid :: pid()) -> {ok, resource_allocation_state()} | {error, term()}.
```

**Petri Net Structure**:
- **Places**: `[p_start, p_available, p_allocating, p_allocated, p_busy, p_end]`
- **Transitions**: `[t_request, t_allocate, t_release, t_finish]`

---

### resource_deallocation

**Module**: `resource_deallocation.erl`

**Description**: Handles deallocation and release of resources.

---

### resource_initialization (WRP-03)

**Module**: `resource_initialization.erl`

**Description**: Handles initialization phase of resources before use.

**State Record**:
```erlang
-record(resource_initialization_state, {
    init_fun :: function(),
    resource :: term(),
    status = pending :: pending | initializing | validated | ready | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec new(InitFun :: function(), Resource :: term()) -> resource_initialization_state().
-spec start(InitFun :: function(), Resource :: term()) -> {ok, pid()} | {error, term()}.
-spec get_state(Pid :: pid()) -> {ok, resource_initialization_state()} | {error, term()}.
```

---

### role_based_allocation (WRP-02)

**Module**: `role_based_allocation.erl`

**Description**: Allocates resources to tasks based on role definitions.

**State Record**:
```erlang
-record(role_based_allocation_state, {
    required_role :: atom(),
    role_map :: #{atom() => [term()]},
    allocated :: undefined | term(),
    status = pending :: pending | checking | allocated | failed | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec new(RequiredRole :: atom(), RoleMap :: map()) -> role_based_allocation_state().
-spec start(RequiredRole :: atom(), RoleMap :: map()) -> {ok, pid()} | {error, term()}.
-spec get_state(Pid :: pid()) -> {ok, role_based_allocation_state()} | {error, term()}.
```

---

### direct_resource_creation

**Module**: `direct_resource_creation.erl`

**Description**: Creates resources directly within the workflow.

---

### critical_section

**Module**: `critical_section.erl`

**Description**: Manages critical sections for mutually exclusive resource access.

---

## Multiple Instance Patterns

### multi_instance

**Module**: `multi_instance.erl`

**Description**: Multiple instance patterns (WCP-12/13/14).

**API Functions**:
```erlang
%% WCP-12: No synchronization
-spec multiple_instances_no_sync(Subprocess :: function(), Count :: pos_integer()) ->
    {ok, [pid()]} | {error, term()}.

%% WCP-13: Design time knowledge
-spec multiple_instances_design_time(Subprocess :: function(), InputData :: list(),
                                     InstanceCount :: pos_integer()) ->
    {ok, [term()]} | {error, term()}.

%% WCP-14: Runtime knowledge
-spec multiple_instances_runtime(Subprocess :: function(), InputData :: list(),
                                 CountFun :: function()) ->
    {ok, [term()]} | {error, term()}.
```

---

### multiple_instances_sync

**Module**: `multiple_instances_sync.erl`

**Description**: Synchronized multiple instance execution.

---

### complete_mi_activity

**Module**: `complete_mi_activity.erl`

**Description**: Complete activity in multiple instance context.

---

## Thread Patterns

### thread_split (P42)

**Module**: `thread_split.erl`

**Description**: Split into multiple independent thread execution paths.

**State Record**:
```erlang
-record(state, {
    branches = [] :: [atom()],
    split = false :: boolean()
}).
```

**Petri Net Structure**:
- **Places**: `[p_start, p_thread1, p_thread2, p_thread3, p_thread4, p_end]`
- **Transitions**: `[t_split, t_finish1, t_finish2, t_finish3, t_finish4]`

---

### thread_merge (P41)

**Module**: `thread_merge.erl`

**Description**: Merge multiple thread execution paths.

**State Record**:
```erlang
-record(state, {
    threads = [] :: [atom()],
    merged = false :: boolean()
}).
```

**Petri Net Structure**:
- **Places**: `[p_start, p_thread1, p_thread2, p_thread3, p_thread4, p_merged, p_end]`
- **Transitions**: `[t_split, t_complete1, t_complete2, t_complete3, t_complete4, t_merge, t_finish]`

---

### thread_merge

**Module**: `thread_merge.erl`

**Description**: Merges parallel thread executions.

---

## Milestone Pattern

### milestone (WCP-18)

**Module**: `milestone.erl`

**Description**: Enables activities based on workflow state milestones.

**State Record**:
```erlang
-record(milestone_state, {
    activity_fun :: function(),
    milestone_fun :: function(),
    milestone_reached = false :: boolean(),
    activity_result :: undefined | term(),
    activity_executed = false :: boolean(),
    log_id :: binary() | undefined
}).
```

**API Functions**:
```erlang
-spec milestone_reached(WorkflowState :: term(), Milestone :: atom()) -> boolean().
-spec enable_on_milestone(Activity :: function(), Milestone :: atom(),
                         WorkflowState :: term()) -> {ok, term()} | {pending, term()}.
-spec milestone_check(MilestoneFun :: function(), CurrentState :: term()) ->
    {reached, term()} | {not_reached, term()}.
```

---

## Infrastructure Patterns

### circuit_breaker

**Module**: `circuit_breaker.erl`

**Description**: Circuit Breaker pattern for preventing cascading failures.

**State Record**:
```erlang
-record(circuit_state, {
    state :: closed | open | half_open,
    failures = 0 :: non_neg_integer(),
    successes = 0 :: non_neg_integer(),
    last_failure_time :: undefined | integer(),
    last_state_change :: integer()
}).

-record(breaker_config, {
    failure_threshold = 5 :: pos_integer(),
    timeout_ms = 60000 :: pos_integer(),
    success_threshold = 2 :: pos_integer(),
    call_timeout = 5000 :: pos_integer()
}).
```

**API Functions**:
```erlang
-spec start_link(Name :: binary(), Fun :: function()) -> {ok, pid()} | {error, term()}.
-spec start_link(Name :: binary(), Fun :: function(), Options :: proplists:proplist()) ->
    {ok, pid()} | {error, term()}.
-spec execute(Name :: binary(), Fun :: function()) -> breaker_result().
-spec execute(Name :: binary(), Fun :: function(), Timeout :: pos_integer()) ->
    breaker_result().
-spec reset(Name :: binary()) -> ok | {error, not_found}.
-spec get_state(Name :: binary()) -> {ok, circuit_state()} | {error, not_found}.
-spec stop(Name :: binary()) -> ok.
```

**Circuit States**:
- **Closed**: Normal operation, requests pass through
- **Open**: Failure threshold reached, requests are rejected
- **Half-Open**: Testing if service has recovered

**Example**:
```erlang
%% Start a circuit breaker
{ok, _Pid} = circuit_breaker:start_link(<<"my_service">>(), fun external_api/0).

%% Execute through the breaker
case circuit_breaker:execute(<<"my_service">>(), fun() -> api_call() end, 5000) of
    {ok, Result} -> handle_result(Result);
    {error, circuit_open} -> handle_circuit_open()
end.
```

---

### pattern_learning

**Module**: `pattern_learning.erl`

**Description**: Learning capabilities for pattern selection and optimization.

---

---

## RL Agent API

### rl_agent

**Module**: `rl_agent.erl`

**Description**: Reinforcement Learning agent for workflow intervention using gen_statem.

**State Record**:
```erlang
-record(rl_agent_state, {
    agent_id :: binary(),
    pattern_id :: binary(),
    workflow_id :: binary(),
    state_space :: map(),
    action_space :: map(),
    q_table :: ets:tid(),
    policy :: policy_type(),
    learning_rate :: float(),
    discount_factor :: float(),
    exploration_rate :: float(),
    exploration_decay :: float(),
    circuit_breaker :: pid() | undefined,
    intervention_count :: non_neg_integer(),
    max_interventions :: pos_integer(),
    total_reward :: float(),
    episode_count :: non_neg_integer(),
    last_state :: term() | undefined,
    last_action :: atom() | undefined,
    last_reward :: float() | undefined
}).

-record(rl_action, {
    action_type :: reroute | skip | prioritize | parallelize | no_action,
    target :: binary(),
    parameters :: map()
}).

-type policy_type() :: tabular_q | deep_q.
-type rl_action() :: #rl_action{}.
```

**Agent States** (gen_statem):
- **initializing**: Agent is being initialized
- **observing**: Observing workflow state
- **selecting_action**: Choosing an action
- **intervening**: Executing the chosen action
- **learning**: Updating Q-values based on reward
- **paused**: Agent is paused

**API Functions**:
```erlang
%% Lifecycle
-spec start_link(AgentId :: binary(), Options :: map()) ->
    {ok, pid()} | {error, term()}.
-spec stop(AgentId :: binary()) -> ok.

%% Action Selection
-spec recommend_action(AgentId :: binary(), StateFeatures :: map()) ->
    {ok, #rl_action{}} | {error, term()}.

%% Learning
-spec record_reward(AgentId :: binary(), Reward :: float()) -> ok.
-spec observe_next_state(AgentId :: binary(), NextState :: map()) -> ok.

%% Configuration
-spec get_policy(AgentId :: binary()) -> {ok, map()}.
-spec set_learning_rate(AgentId :: binary(), Rate :: float()) ->
    ok | {error, term()}.
-spec pause(AgentId :: binary()) -> ok.
-spec resume(AgentId :: binary()) -> ok.

%% Statistics
-spec get_statistics(AgentId :: binary()) -> {ok, map()}.
```

**Example**:
```erlang
%% Start an RL agent
{ok, Pid} = rl_agent:start_link(<<"agent1">>, #{
    pattern_id => <<"deferred_choice">>,
    workflow_id => <<"order_processing">>,
    learning_rate => 0.1,
    discount_factor => 0.95,
    exploration_rate => 1.0,
    max_interventions => 100
}).

%% Get action recommendation
{ok, Action} = rl_agent:recommend_action(<<"agent1">>, #{
    queue_depth => 5,
    processing_time => 120,
    error_rate => 0.05
}).

%% Record reward and observe next state
rl_agent:record_reward(<<"agent1">>, 1.0),
rl_agent:observe_next_state(<<"agent1">>, #{queue_depth => 3}).

%% Get statistics
{ok, Stats} = rl_agent:get_statistics(<<"agent1">>).
```

---

## RL Strategies API

### strategy_q_learning

**Module**: `strategy_q_learning.erl`

**Description**: Tabular Q-learning with epsilon-greedy exploration.

**State Record**:
```erlang
-record(q_learning_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    q_table :: ets:tid(),
    state_encoder :: function(),
    learning_rate = 0.1 :: float(),
    discount_factor = 0.95 :: float(),
    epsilon = 1.0 :: float(),
    epsilon_min = 0.01 :: float(),
    epsilon_decay = 0.995 :: float()
}).
```

**API Functions**:
```erlang
%% Lifecycle
-spec start_link(N :: pos_integer(), Options :: map()) -> {ok, pid()} | {error, term()}.
-spec stop(Pid :: pid()) -> ok.

%% Action Selection
-spec select_action(Pid :: pid(), State :: term()) -> {ok, pos_integer()}.

%% Learning
-spec update_q_value(Pid :: pid(), State :: term(), Action :: pos_integer(),
                     Reward :: float(), NextState :: term()) -> ok.

%% Configuration
-spec get_q_table(Pid :: pid()) -> map().
-spec set_epsilon(Pid :: pid(), Epsilon :: float()) -> ok.
-spec set_learning_rate(Pid :: pid(), Rate :: float()) -> ok.
```

**Update Rule**:
```
Q(s,a) += alpha * (reward + gamma * max(Q(s',a')) - Q(s,a))
```

**Example**:
```erlang
{ok, Pid} = strategy_q_learning:start_link(5, #{
    learning_rate => 0.1,
    discount_factor => 0.95,
    epsilon => 0.9
}),

{ok, Action} = strategy_q_learning:select_action(Pid, CurrentState),
%% ... execute action, observe reward ...
ok = strategy_q_learning:update_q_value(Pid, CurrentState, Action, Reward, NextState).
```

---

### strategy_ucb

**Module**: `strategy_ucb.erl`

**Description**: Upper Confidence Bound (UCB1) algorithm for multi-armed bandits.

**State Record**:
```erlang
-record(ucb_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    arms :: [ucb_arm()],
    total_pulls = 0 :: non_neg_integer(),
    c = 1.41 :: float()
}).

-record(ucb_arm, {
    id :: pos_integer(),
    pulls = 0 :: non_neg_integer(),
    total_reward = 0.0 :: float(),
    avg_reward = 0.0 :: float()
}).
```

**API Functions**:
```erlang
-spec new(N :: pos_integer(), Options :: map()) -> ucb_state().
-spec select_arm(ucb_state()) -> {pos_integer(), ucb_state()}.
-spec record_result(ucb_state(), ArmId :: pos_integer(), Reward :: float()) ->
    ucb_state().
-spec get_stats(ucb_state()) -> map().
```

**UCB Formula**:
```
UCB = avg_reward + c * sqrt(log(total) / pulls)
```

**Example**:
```erlang
State = strategy_ucb:new(5, #{c => 1.41}),
{ArmId, State1} = strategy_ucb:select_arm(State),
%% ... execute arm, observe reward ...
State2 = strategy_ucb:record_result(State1, ArmId, Reward).
```

---

### strategy_contextual

**Module**: `strategy_contextual.erl`

**Description**: Context-aware branch selection using feature-based models (linear model).

**State Record**:
```erlang
-record(linear_model, {
    weights :: [float()],
    bias :: float(),
    samples :: pos_integer()
}).

-record(contextual_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    feature_extractor :: function(),
    model :: #linear_model{} | undefined,
    min_samples = 10 :: pos_integer(),
    fallback_strategy :: atom()
}).
```

**API Functions**:
```erlang
-spec start_link(N :: pos_integer(), Options :: map()) -> {ok, pid()} | {error, term()}.
-spec stop(Pid :: pid()) -> ok.
-spec predict_branch(Pid :: pid(), Context :: map()) -> {ok, pos_integer()}.
-spec update_model(Pid :: pid(), Context :: map(), Branch :: pos_integer(),
                  Reward :: float()) -> ok.
-spec set_feature_extractor(Pid :: pid(), Fun :: function()) -> ok.
```

**Example**:
```erlang
{ok, Pid} = strategy_contextual:start_link(5, #{
    feature_extractor => fun(Ctx) ->
        [
            maps:get(priority, Ctx, 0.0),
            maps:get(queue_depth, Ctx, 0.0)
        ]
    end
}),

{ok, Branch} = strategy_contextual:predict_branch(Pid, #{
    priority => 1,
    queue_depth => 5
}).
```

---

### strategy_thompson_sampling

**Module**: `strategy_thompson_sampling.erl`

**Description**: Thompson Sampling for multi-armed bandits using Beta distributions.

**State Record**:
```erlang
-record(bandit_arm, {
    branch_id :: pos_integer(),
    alpha :: pos_integer(),
    beta :: pos_integer(),
    success_count :: non_neg_integer(),
    failure_count :: non_neg_integer()
}).

-record(thompson_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    arms :: [#bandit_arm{}],
    completed :: [pos_integer()],
    results :: map()
}).
```

**API Functions**:
```erlang
-spec init(N :: pos_integer(), M :: pos_integer()) -> {ok, thompson_state()}.
-spec should_complete(thompson_state(), map()) -> boolean().
-spec on_branch_complete(thompson_state(), {pos_integer(), term()}) ->
    thompson_state().
-spec get_result(thompson_state()) -> {ok, map()}.
-spec select_branch(thompson_state()) -> pos_integer().
-spec record_outcome(thompson_state(), BranchId :: pos_integer(),
                    Outcome :: success | failure) -> thompson_state().
```

**Algorithm**:
```
alpha = 1 + success_count
beta = 1 + failure_count
Sample from Beta(alpha, beta) for each arm
Select arm with highest sample
```

---

### strategy_first_n

**Module**: `strategy_first_n.erl`

**Description**: First-N completion strategy - selects based on which branches complete first.

---

### strategy_fastest_n

**Module**: `strategy_fastest_n.erl`

**Description**: Fastest-N strategy - selects branches based on execution speed.

---

### strategy_quality

**Module**: `strategy_quality.erl`

**Description**: Quality-based strategy selection.

---

## Pattern Classification

### By Workflow Control Patterns (WCP)

| Pattern # | Module | Category | Description |
|-----------|--------|----------|-------------|
| WCP-01 | sequence | Control | Sequential execution |
| WCP-02 | parallel_split | Control | Parallel split |
| WCP-03 | synchronization | Sync | AND-join |
| WCP-04 | exclusive_choice | Branch | XOR choice |
| WCP-05 | simple_merge | Branch | XOR merge |
| WCP-06 | multiple_choice | Branch | OR choice |
| WCP-07 | multiple_merge | Sync | Multiple merge |
| WCP-09 | discriminator, or_join | Sync | Discriminator/OR-join |
| WCP-10 | explicit_termination | Control | Explicit termination |
| WCP-11 | implicit_termination | Control | Implicit termination |
| WCP-12 | multi_instance | Control | MI no sync |
| WCP-13 | multi_instance | Control | MI design time |
| WCP-14 | multi_instance | Control | MI runtime |
| WCP-16 | deferred_choice | Branch | Deferred choice |
| WCP-17 | interleaved_parallel | Sync | Interleaved parallel |
| WCP-18 | milestone | Control | Milestone |
| WCP-19 | cancel_activity | Cancel | Cancel activity |
| WCP-20 | cancel_case | Cancel | Cancel case |
| WCP-21 | arbitrary_cycles | Control | Arbitrary cycles |
| WCP-22 | structured_loop | Control | While loop |
| WCP-23 | structured_loop | Control | Until loop |
| WCP-24 | recursion | Control | Recursion |
| WCP-25 | cancel_region | Cancel | Cancel region |
| WCP-41 | thread_merge | Thread | Thread merge |
| WCP-42 | thread_split | Thread | Thread split |

### By Workflow Data Patterns (WDP)

| Pattern # | Module | Description |
|-----------|--------|-------------|
| WDP-02 | data_transform | Data transformation |
| WDP-03 | data_distribute | Data distribution |
| WDP-04 | data_accumulate | Data accumulation |
| WDP-05 | data_visibility | Data visibility |

### By Workflow Resource Patterns (WRP)

| Pattern # | Module | Description |
|-----------|--------|-------------|
| WRP-02 | role_based_allocation | Role-based allocation |
| WRP-03 | resource_initialization | Resource initialization |
| WRP-04 | resource_allocation | Resource allocation |
| WRP-05 | resource_deallocation | Resource deallocation |

### By Exception Handling Patterns (WHP)

| Pattern # | Module | Description |
|-----------|--------|-------------|
| WHP-01 | exception_patterns | Escalation |
| WHP-02 | exception_patterns | Retry |
| WHP-03 | exception_patterns | Compensation |
| WHP-P19 | cancel_activity | Cancel activity |
| WHP-P20 | cancel_case | Cancel case |
| WHP-P25 | cancel_region | Cancel region |

---

## Soundness Properties

All patterns support verification of the following soundness properties:

1. **Option to Complete**: From any reachable marking, there exists a firing sequence that leads to the final marking.

2. **Proper Completion**: When the final marking is reached, the workflow has completed exactly one instance.

3. **No Dead Transitions**: There are no transitions that can never be fired in any execution.

4. **No Dead Tasks**: All tasks can potentially be executed in some workflow execution.

---

## XES Logging Integration

Most patterns support XES (eXtensible Event Stream) logging for process mining:

```erlang
%% Each pattern state includes:
log_id :: binary() | undefined

%% Events logged:
- Pattern start
- Activity execution
- Branch selection
- Synchronization points
- Pattern completion
```

---

## Type Specifications

All pattern modules export their state and configuration types:

```erlang
-type marking() :: #{atom() => [term()]}.
-type net_structure() :: #{places => [atom()], transitions => [atom()], arcs => [{atom(), atom()}]}.
-type pattern_result() :: {ok, term()} | {error, term()}.
```

---

## Best Practices

1. **Always use `gen_yawl:start_link/3`** to start pattern instances for proper supervision.

2. **Provide unique identifiers** for patterns to avoid naming conflicts in the registry.

3. **Handle `{error, timeout}`** results appropriately - patterns may time out on synchronization.

4. **Use proper supervisors** for pattern instances to handle failures gracefully.

5. **Clean up ETS tables** in terminate/2 callbacks to prevent memory leaks.

6. **Log pattern events** using `yawl_xes` for process mining analysis.

7. **Consider soundness** when designing custom patterns - verify properties before deployment.

---

## References

- van der Aalst, W. M. P., ter Hofstede, A. H. M., Kiepuszewski, B., & Barros, A. P. (2003). Workflow patterns.
- Russell, N., ter Hofstede, A. H. M., van der Aalst, W. M. P., & Mulyar, N. (2006). Workflow control-flow patterns.
- Russell, N., van der Aalst, W. M. P., & ter Hofstede, A. H. M. (2006). Workflow data patterns.
- Russell, N., van der Aalst, W. M. P., ter Hofstede, A. H. M., & Edmond, D. (2005). Workflow resource patterns.

---

*Generated for CRE - Common Runtime Environment*
*Erlang/OTP YAWL Workflow Engine with Petri Net Patterns*
