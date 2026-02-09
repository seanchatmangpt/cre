# CRE Pattern Modules API Documentation

This document provides comprehensive API documentation for all workflow pattern modules in the CRE (Common Runtime Environment) system. These modules implement the Workflow Patterns Initiative catalog using Petri Net semantics through the `gen_yawl` behavior.

## Table of Contents

- [Overview](#overview)
- [Control Flow Patterns](#control-flow-patterns)
- [Advanced Branching Patterns](#advanced-branching-patterns)
- [Cancellation Patterns](#cancellation-patterns)
- [Data Patterns](#data-patterns)
- [Resource Patterns](#resource-patterns)
- [Multiple Instance Patterns](#multiple-instance-patterns)
- [RL Strategy Patterns](#rl-strategy-patterns)
- [Utility Patterns](#utility-patterns)
- [Pattern Classification](#pattern-classification)

---

## Overview

All pattern modules implement the `gen_yawl` behavior and share common callbacks:

### Common Callbacks

```erlang
%% Returns list of places in the Petri net
-spec place_lst() -> [atom()].

%% Returns list of transitions in the Petri net
-spec trsn_lst() -> [atom()].

%% Returns initial marking for a place
-spec init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].

%% Returns preset (input places) for a transition
-spec preset(Trsn :: atom()) -> [atom()].

%% Checks if a transition is enabled
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) -> boolean().

%% Fires a transition, consuming and producing tokens
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    {produce, map()} | {produce, map(), UsrInfo} | abort.

%% gen_yawl behavior callbacks
-spec init(UsrInfo :: term()) -> {ok, UsrInfo}.
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
    {reply, term(), term()}.
-spec handle_cast(Request :: term(), NetState :: term()) -> {noreply, term()}.
-spec handle_info(Request :: term(), NetState :: term()) -> {noreply, term()}.
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) -> {ok, term()}.
-spec terminate(Reason :: term(), NetState :: term()) -> ok.
-spec trigger(Place :: atom(), Token :: term(), NetState :: term()) -> pass | {consume, [term()]}.
```

---

## Control Flow Patterns

### Sequence Pattern (P1)
**Module**: `sequence`

Implements sequential execution of tasks where one task must complete before the next begins.

#### State Record
```erlang
-record(state, {
    tasks = [] :: [atom()],
    current = 0 :: non_neg_integer()
}).
```

#### API Functions
```erlang
%% Initialize with from/to tasks
init(#{from := From, to := To}) -> #{tasks => [From, To], current => 0}
```

#### Petri Net Structure
- **Places**: `p_start`, `p_task1`, `p_task2`, `p_end`
- **Transitions**: `t_start`, `t_complete1`, `t_complete2`, `t_finish`

#### Usage Example
```erlang
%% Create a sequence from task_a to task_b
State = sequence:init(#{from => task_a, to => task_b}),
{ok, Pid} = gen_yawl:start_link(sequence, State, []).
```

---

### Parallel Split Pattern (WCP-02)
**Module**: `parallel_split`

Splits a single thread of execution into multiple concurrent branches that execute in parallel. All branches are activated simultaneously from the same starting point.

#### State Record
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

#### API Functions
```erlang
%% Create a new parallel split state
-spec new(BranchFuns :: [function()], BranchCount :: pos_integer()) ->
    parallel_split_state().

%% Start the workflow as a gen_yawl process
-spec start(BranchFuns :: [function()]) -> {ok, pid()} | {error, term()}.

%% Run synchronously with input data
-spec run(BranchFuns :: [function()], InputData :: term()) ->
    {ok, #{pos_integer() => term()}} | {error, term()}.

%% Get current state
-spec get_state(Pid :: pid()) -> {ok, parallel_split_state()} | {error, term()}.

%% Execute function synchronously
-spec execute(BranchFuns :: [function()], InputData :: term()) ->
    {ok, #{pos_integer() => term()}} | {error, term()}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_branch1`, `p_branch2`, `p_branch3`, `p_branch4`, `p_join_ready`, `p_all_done`, `p_end`
- **Transitions**: `t_split`, `t_join_branch1`, `t_join_branch2`, `t_join_branch3`, `t_join_branch4`, `t_finish`

#### Usage Example
```erlang
%% Execute 2 branches in parallel
Fun1 = fun(X) -> X + 1 end,
Fun2 = fun(X) -> X * 2 end,
{ok, Results} = parallel_split:execute([Fun1, Fun2], 10).
%% Results: #{1 => 11, 2 => 20}
```

---

### Synchronization Pattern (WCP-03)
**Module**: `synchronization`

Implements AND-join waiting for all parallel branches to complete before continuing.

#### State Record
```erlang
-record(state, {
    waits_for = [] :: [atom()],
    joined = false :: boolean()
}).
```

#### API Functions
```erlang
%% Initialize with branches to wait for
init(#{waits_for := WaitsFor}) -> #{waits_for => WaitsFor, joined => false}
```

#### Petri Net Structure
- **Places**: `p_start`, `p_branch1`, `p_branch2`, `p_branch3`, `p_joined`, `p_end`
- **Transitions**: `t_split`, `t_complete1`, `t_complete2`, `t_complete3`, `t_join`, `t_finish`

#### Usage Example
```erlang
%% Wait for 3 branches
State = synchronization:init(#{waits_for => [p_branch1, p_branch2, p_branch3]}),
{ok, Pid} = gen_yawl:start_link(synchronization, State, []).
```

---

### Exclusive Choice Pattern (WCP-04)
**Module**: `exclusive_choice`

Represents a divergence where exactly one of multiple alternative branches is selected based on conditions or data available at runtime.

#### State Record
```erlang
-record(exclusive_choice_state, {
    branches :: map(),  %% Map of branch_id => function()
    selected :: undefined | atom(),
    branch_count :: pos_integer(),
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

#### API Functions
```erlang
%% Create new exclusive choice state
-spec new(Branches :: map(), BranchCount :: pos_integer()) ->
    exclusive_choice_state().

%% Start the workflow
-spec start(Branches :: map()) -> {ok, pid()} | {error, term()}.

%% Run synchronously
-spec run(Branches :: map()) -> {ok, {atom(), term()}} | {error, term()}.

%% Get current state
-spec get_state(Pid :: pid()) -> {ok, exclusive_choice_state()} | {error, term()}.

%% Execute with input data
-spec execute(Branches :: map(), InputData :: term()) ->
    {ok, {atom(), term()}} | {error, term()}.

%% Manually select a branch
-spec select_branch(Pid :: pid(), BranchId :: atom()) -> ok | {error, term()}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_choice`, `p_selected`, `p_end`
- **Transitions**: `t_select_a`, `t_select_b`, `t_finish`

#### Usage Example
```erlang
%% Create choice between two branches
Branches = #{a => fun(X) -> X + 1 end, b => fun(X) -> X * 2 end},
{ok, {Selected, Result}} = exclusive_choice:execute(Branches, 10).
%% Selected is either 'a' or 'b'
```

---

### Simple Merge Pattern (WCP-05)
**Module**: `simple_merge`

Merges multiple exclusive branches into a single flow without synchronization.

---

### Multiple Choice Pattern (WCP-06)
**Module**: `multiple_choice`

Selects multiple branches to execute in parallel based on runtime conditions.

---

### Multiple Merge Pattern (WCP-07)
**Module**: `multiple_merge`

Merges multiple parallel branches without waiting for all to complete.

---

### Synchronization Merge Pattern (WCP-08)
**Module**: `synchronization`

Waits for all incoming branches before proceeding (AND-join).

---

### N-out-of-M Pattern (WCP-09)
**Module**: `n_out_of_m`

Proceeds when N of M parallel branches complete (partial synchronization).

---

## Advanced Branching Patterns

### Structured Partial Join Pattern (P30)
**Module**: `structured_partial_join`

Proceeds after N of M branches complete (structured partial join).

#### State Record
```erlang
-record(state, {
    m :: pos_integer(),  %% Total branches
    n :: pos_integer(),  %% Required to proceed
    completed = 0 :: non_neg_integer()
}).
```

#### API Functions
```erlang
init(#{m := M, n := N}) -> #{m => M, n => N, completed => 0}
```

#### Petri Net Structure
- **Places**: `p_start`, `p_branch1`, `p_branch2`, `p_branch3`, `p_partial_ready`, `p_end`
- **Transitions**: `t_split`, `t_complete1`, `t_complete2`, `t_complete3`, `t_partial_join`, `t_finish`

---

### Generalized AND-Join Pattern (P33)
**Module**: `generalized_and_join`

Joins across only active branches (dynamically determined at runtime).

#### State Record
```erlang
-record(state, {
    active_branches = [] :: [atom()],
    joined = false :: boolean()
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_branch1`, `p_branch2`, `p_branch3`, `p_join_ready`, `p_joined`, `p_end`
- **Transitions**: `t_split`, `t_complete1`, `t_complete2`, `t_complete3`, `t_join`, `t_finish`

---

### Local Sync Merge Pattern (WCP-17)
**Module**: `local_sync_merge`

Merges parallel branches with local synchronization semantics.

---

### Thread Split Pattern (WCP-20)
**Module**: `thread_split`

Splits execution into independent threads of control.

---

### Thread Merge Pattern (WCP-21)
**Module**: `thread_merge`

Merges independent threads back together.

---

## Cancellation Patterns

### Cancel Activity Pattern (P19)
**Module**: `cancel_activity`

Cancels a single activity during execution.

#### State Record
```erlang
-record(state, {
    target :: atom(),
    cancel_event :: atom(),
    cancelled = false :: boolean()
}).
```

#### API Functions
```erlang
init(#{target := Target, cancel_event := Event}) ->
    #{target => Target, cancel_event => Event, cancelled => false}
```

#### Petri Net Structure
- **Places**: `p_start`, `p_active`, `p_cancel_event`, `p_cancelled`, `p_end`
- **Transitions**: `t_start`, `t_cancel`, `t_finish`

---

### Cancel Case Pattern (P20)
**Module**: `cancel_case`

Cancels the entire workflow case (all activities).

#### State Record
```erlang
-record(state, {
    cancel_event :: atom(),
    cancelled = false :: boolean()
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_active`, `p_cancel_event`, `p_cancelled`, `p_end`
- **Transitions**: `t_start`, `t_cancel`, `t_finish`

---

### Cancel Region Pattern
**Module**: `cancel_region`

Cancels a region (set of activities) within the workflow.

---

### Cancel MI Activity Pattern (P26)
**Module**: `cancel_mi_activity`

Cancels all instances of a multiple instance activity.

#### State Record
```erlang
-record(state, {
    mi_task :: atom(),
    cancel_event :: atom(),
    instances = [] :: [term()]
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_instances`, `p_cancel_event`, `p_cancelled`, `p_end`
- **Transitions**: `t_create_instances`, `t_cancel`, `t_complete`

---

### Complete MI Activity Pattern (P27)
**Module**: `complete_mi_activity`

Completes all MI instances early when a condition is met.

#### State Record
```erlang
-record(state, {
    mi_task :: atom(),
    complete_condition :: binary(),
    instances = [] :: [term()],
    completed = false :: boolean()
}).
```

---

### Cancelling Discriminator Pattern (P29)
**Module**: `cancelling_discriminator`

First completion wins, cancels remaining branches.

#### State Record
```erlang
-record(state, {
    race :: [atom()],
    cancel_rest = true :: boolean(),
    winner :: atom() | undefined
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_race1`, `p_race2`, `p_race3`, `p_winner`, `p_cancelled`, `p_end`
- **Transitions**: `t_start_race`, `t_win1`, `t_win2`, `t_win3`, `t_cancel_others`, `t_finish`

---

### Blocking Discriminator Pattern (P28)
**Module**: `blocking_discriminator`

First completion triggers, blocks others until cleared.

#### State Record
```erlang
-record(state, {
    trigger :: atom(),
    blocks_until :: [atom()],
    triggered = false :: boolean(),
    blocked = [] :: [atom()]
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_branch1`, `p_branch2`, `p_branch3`, `p_triggered`, `p_blocked`, `p_cleared`, `p_end`
- **Transitions**: `t_split`, `t_complete1`, `t_complete2`, `t_complete3`, `t_trigger`, `t_clear`, `t_finish`

---

### Blocking Partial Join Pattern (P31)
**Module**: `blocking_partial_join`

Partial output after N, final output after all M.

#### State Record
```erlang
-record(state, {
    m :: pos_integer(),
    n :: pos_integer(),
    partial_out :: atom(),
    final_out :: atom(),
    completed = 0 :: non_neg_integer()
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_branch1`, `p_branch2`, `p_branch3`, `p_partial_out`, `p_final_out`, `p_end`
- **Transitions**: `t_split`, `t_complete1`, `t_complete2`, `t_complete3`, `t_partial`, `t_final`, `t_finish`

---

### Cancelling Partial Join Pattern (P32)
**Module**: `cancelling_partial_join`

Cancel remaining after N of M complete.

#### State Record
```erlang
-record(state, {
    m :: pos_integer(),
    n :: pos_integer(),
    cancel_remaining = true :: boolean(),
    completed = 0 :: non_neg_integer()
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_branch1`, `p_branch2`, `p_branch3`, `p_threshold_met`, `p_cancelled`, `p_end`
- **Transitions**: `t_split`, `t_complete1`, `t_complete2`, `t_complete3`, `t_threshold`, `t_cancel`, `t_finish`

---

### Static Partial Join for MI Pattern (P34)
**Module**: `static_partial_join_mi`

Proceed after N of M instances complete (fixed pool).

#### State Record
```erlang
-record(state, {
    total_instances :: pos_integer(),
    threshold :: pos_integer(),
    completed = 0 :: non_neg_integer()
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_instances`, `p_threshold_met`, `p_end`
- **Transitions**: `t_create_instances`, `t_complete_instance`, `t_threshold`, `t_finish`

---

### Cancelling Partial Join for MI Pattern (P35)
**Module**: `cancelling_partial_join_mi`

Cancel remaining instances after threshold met.

#### State Record
```erlang
-record(state, {
    total_instances :: pos_integer(),
    threshold :: pos_integer(),
    completed = 0 :: non_neg_integer(),
    cancelled = false :: boolean()
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_instances`, `p_threshold_met`, `p_cancelled`, `p_end`
- **Transitions**: `t_create_instances`, `t_complete_instance`, `t_threshold`, `t_cancel`, `t_finish`

---

### Dynamic Partial Join for MI Pattern (P36)
**Module**: `dynamic_partial_join_mi`

Threshold computed dynamically at runtime.

#### State Record
```erlang
-record(state, {
    threshold_expr :: binary(),
    threshold :: pos_integer() | undefined,
    completed = 0 :: non_neg_integer()
}).
```

---

### Explicit Termination Pattern (P43)
**Module**: `explicit_termination`

Hard-stop that cancels all activities.

#### State Record
```erlang
-record(state, {
    terminator :: atom(),
    cancels_all = true :: boolean(),
    terminated = false :: boolean()
}).
```

#### Petri Net Structure
- **Places**: `p_start`, `p_active`, `p_terminate_event`, `p_terminated`, `p_cancelled`, `p_end`
- **Transitions**: `t_start`, `t_terminate`, `t_cancel_all`, `t_finish`

---

## Data Patterns

### Data Transformation Pattern (WDP-02)
**Module**: `data_transform`

Transforms data between formats as it flows through the workflow.

#### State Record
```erlang
-record(data_transform_state, {
    transform_fun :: function(),
    input_data :: term(),
    output_data :: undefined | term(),
    start_time :: integer()
}).
```

#### API Functions
```erlang
%% Create new transformation state
-spec new(TransformFun :: function(), InputData :: term()) ->
    data_transform_state().

%% Start workflow
-spec start(TransformFun :: function(), InputData :: term()) ->
    {ok, pid()} | {error, term()}.

%% Get state
-spec get_state(Pid :: pid()) -> {ok, data_transform_state()} | {error, term()}.

%% Transform synchronously
-spec transform(TransformFun :: function(), InputData :: term()) ->
    {ok, term()}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_transforming`, `p_end`
- **Transitions**: `t_transform`, `t_finish`

#### Usage Example
```erlang
%% Transform data (e.g., encode to JSON)
Fun = fun(Data) -> json:encode(Data) end,
{ok, Json} = data_transform:transform(Fun, #{key => value}).
```

---

### Data Distribution Pattern (WDP-03)
**Module**: `data_distribute`

Distributes data to multiple recipients in the workflow.

#### State Record
```erlang
-record(data_distribute_state, {
    data :: term(),
    targets :: [term()],
    distributed_count = 0 :: non_neg_integer(),
    start_time :: integer()
}).
```

#### API Functions
```erlang
%% Create new distribution state
-spec new(Data :: term(), Targets :: [term()], TargetCount :: pos_integer()) ->
    data_distribute_state().

%% Start workflow
-spec start(Data :: term(), Targets :: [term()]) -> {ok, pid()} | {error, term()}.

%% Get state
-spec get_state(Pid :: pid()) -> {ok, data_distribute_state()} | {error, term()}.

%% Distribute synchronously
-spec distribute(Data :: term(), Targets :: [term()], DistributionFun :: function()) ->
    {ok, [term()]}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_targets`, `p_distributed`, `p_end`
- **Transitions**: `t_distribute`, `t_finish`

---

### Data Accumulation Pattern (WDP-04)
**Module**: `data_accumulate`

Collects and aggregates data from multiple sources.

#### State Record
```erlang
-record(data_accumulate_state, {
    sources :: [term()],
    accumulator_fun :: function(),
    collected_data = [] :: [term()],
    source_count :: pos_integer(),
    start_time :: integer()
}).
```

#### API Functions
```erlang
%% Create new accumulation state
-spec new(Sources :: [term()], AccumulatorFun :: function(), SourceCount :: pos_integer()) ->
    data_accumulate_state().

%% Start workflow
-spec start(Sources :: [term()], AccumulatorFun :: function()) ->
    {ok, pid()} | {error, term()}.

%% Get state
-spec get_state(Pid :: pid()) -> {ok, data_accumulate_state()} | {error, term()}.

%% Accumulate synchronously
-spec accumulate(Sources :: [term()], AccumulatorFun :: function(), Initial :: term()) ->
    {ok, term()}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_collecting`, `p_accumulated`, `p_end`
- **Transitions**: `t_accumulate`, `t_finish`

---

### Data Visibility Pattern (WDP-05)
**Module**: `data_visibility`

Controls data visibility and access within the workflow.

#### State Record
```erlang
-record(data_visibility_state, {
    data :: term(),
    scope :: term(),
    access_check_fun :: function(),
    access_granted = false :: boolean(),
    start_time :: integer()
}).
```

#### API Functions
```erlang
%% Create new visibility state
-spec new(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
    data_visibility_state().

%% Start workflow
-spec start(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
    {ok, pid()} | {error, term()}.

%% Get state
-spec get_state(Pid :: pid()) -> {ok, data_visibility_state()} | {error, term()}.

%% Check visibility
-spec check_visibility(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
    {ok, boolean()}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_check_scope`, `p_granted`, `p_denied`, `p_end`
- **Transitions**: `t_check`, `t_grant`, `t_deny`, `t_finish`

---

## Resource Patterns

### Direct Resource Creation Pattern (WRP-01)
**Module**: `direct_resource_creation`

Creates and initializes resources on-demand within a workflow.

#### State Record
```erlang
-record(direct_resource_creation_state, {
    create_fun :: function(),
    resource :: undefined | term(),
    status = pending :: pending | creating | ready | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

#### API Functions
```erlang
%% Create new resource creation state
-spec new(CreateFun :: function()) -> direct_resource_creation_state().

%% Start workflow
-spec start(CreateFun :: function()) -> {ok, pid()} | {error, term()}.

%% Get state
-spec get_state(Pid :: pid()) -> {ok, direct_resource_creation_state()} | {error, term()}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_creating`, `p_ready`, `p_end`
- **Transitions**: `t_create`, `t_finish`

---

### Resource Allocation Pattern
**Module**: `resource_allocation`

Allocates resources to activities based on availability.

---

### Resource Deallocation Pattern
**Module**: `resource_deallocation`

Releases resources after activity completion.

---

### Resource Initialization Pattern
**Module**: `resource_initialization`

Initializes resources before first use.

---

### Role-Based Allocation Pattern
**Module**: `role_based_allocation`

Allocates resources based on role assignments.

---

## Multiple Instance Patterns

### Multiple Instance Module
**Module**: `multi_instance`

Implements WCP12-WCP14: Multiple Instance patterns with and without synchronization.

#### Types
```erlang
-type instance_id() :: pos_integer().
-type instance_token() :: {instance, instance_id(), term()}.
-type instance_result() :: {instance_id(), term()}.
-type counter_state() :: #{
    total => pos_integer(),
    active => non_neg_integer(),
    completed => non_neg_integer()
}.
-type sync_state() :: #{
    expected => pos_integer(),
    results => [instance_result()],
    pids => [pid()]
}.
```

#### API Functions
```erlang
%% Multiple Instances without Synchronization (WCP12)
-spec multiple_instances_no_sync(Subprocess :: function(), Count :: pos_integer()) ->
    {ok, [pid()]} | {error, term()}.

%% Multiple Instances with Design Time Knowledge (WCP13)
-spec multiple_instances_design_time(
    Subprocess :: function(),
    InputData :: list(),
    InstanceCount :: pos_integer()
) -> {ok, [term()]} | {error, term()}.

%% Multiple Instances with Runtime Knowledge (WCP14)
-spec multiple_instances_runtime(
    Subprocess :: function(),
    InputData :: list(),
    CountFun :: function()
) -> {ok, [term()]} | {error, term()}.

%% Create instance tokens
-spec create_instances(InputData :: list(), Count :: pos_integer(), TransformFun :: function()) ->
    [instance_token()].

%% Collect results when all instances complete
-spec collect_instances(Ref :: reference(), ExpectedCount :: pos_integer()) ->
    {ok, [term()]} | {error, term()}.

%% Track active instance count
-spec instance_counter(Input :: new | {spawn, pos_integer(), counter_state()} |
                                   {complete, pos_integer(), counter_state()}) ->
    counter_state().
```

#### Usage Examples
```erlang
%% No synchronization - spawn and continue
Fun = fun(X) -> X * 2 end,
{ok, Pids} = multi_instance:multiple_instances_no_sync(Fun, 5).

%% Design time knowledge - fixed count with sync
Data = [1,2,3,4],
{ok, Results} = multi_instance:multiple_instances_design_time(Fun, Data, 4).
%% Results: [2,4,6,8]

%% Runtime knowledge - dynamic count with sync
CountFun = fun(L) -> length(L) end,
{ok, Results} = multi_instance:multiple_instances_runtime(Fun, Data, CountFun).
```

---

### Multiple Instances with Sync
**Module**: `multiple_instances_sync`

Synchronizes after all instances complete.

---

## RL Strategy Patterns

### RL Agent
**Module**: `rl_agent`

Reinforcement Learning agent for workflow intervention using gen_statem.

#### State Record
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
    episode_count :: non_neg_integer()
}).
```

#### Types
```erlang
-type policy_type() :: tabular_q | deep_q.
-type rl_action() :: #rl_action{}.
```

#### API Functions
```erlang
%% Start agent
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.

%% Stop agent
-spec stop(binary()) -> ok.

%% Recommend action based on current state
-spec recommend_action(binary(), map()) -> {ok, #rl_action{}} | {error, term()}.

%% Record reward for last action
-spec record_reward(binary(), float()) -> ok.

%% Observe next state after action
-spec observe_next_state(binary(), map()) -> ok.

%% Get current policy
-spec get_policy(binary()) -> {ok, map()}.

%% Set learning rate
-spec set_learning_rate(binary(), float()) -> ok | {error, term()}.
```

#### States
- `initializing` - Agent starting up
- `observing` - Observing environment, not acting
- `selecting_action` - Choosing action to take
- `intervening` - Executing intervention
- `learning` - Updating policy from experience
- `paused` - Agent paused

---

### Q-Learning Strategy
**Module**: `strategy_q_learning`

Tabular Q-learning with epsilon-greedy exploration for N-of-M branch selection.

#### State Record
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

#### API Functions
```erlang
%% Start Q-learning strategy
-spec start_link(pos_integer(), map()) -> {ok, pid()} | {error, term()}.

%% Stop strategy
-spec stop(pid()) -> ok.

%% Select action using epsilon-greedy
-spec select_action(pid(), term()) -> {ok, pos_integer()}.

%% Update Q-value
-spec update_q_value(pid(), term(), pos_integer(), float(), term()) -> ok.

%% Get Q-table snapshot
-spec get_q_table(pid()) -> map().

%% Set exploration rate
-spec set_epsilon(pid(), float()) -> ok.

%% Set learning rate
-spec set_learning_rate(pid(), float()) -> ok.
```

---

### UCB Strategy
**Module**: `strategy_ucb`

Upper Confidence Bound (UCB1) algorithm for multi-armed bandit branch selection.

#### State Record
```erlang
-record(ucb_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    arms :: [ucb_arm()],
    total_pulls = 0 :: non_neg_integer(),
    c = 1.41 :: float()
}).
```

#### API Functions
```erlang
%% Create new UCB1 strategy state
-spec new(pos_integer(), map()) -> ucb_state().

%% Select arm using UCB1 algorithm
-spec select_arm(ucb_state()) -> {pos_integer(), ucb_state()}.

%% Record result of arm pull
-spec record_result(ucb_state(), pos_integer(), float()) -> ucb_state().

%% Get current statistics
-spec get_stats(ucb_state()) -> map().
```

---

### Thompson Sampling Strategy
**Module**: `strategy_thompson_sampling`

Bayesian approach using Beta distributions for branch selection.

#### State Record
```erlang
-record(thompson_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    arms :: [#bandit_arm{}],
    completed :: [pos_integer()],
    results :: map()
}).
```

#### API Functions
```erlang
%% Initialize Thompson sampling strategy
-spec init(pos_integer(), pos_integer()) -> {ok, thompson_state()}.

%% Check if pattern should complete
-spec should_complete(thompson_state(), map()) -> boolean().

%% Called when branch completes
-spec on_branch_complete(thompson_state(), {pos_integer(), term()}) ->
    thompson_state().

%% Get final result
-spec get_result(thompson_state()) -> {ok, map()}.

%% Select branch using Thompson sampling
-spec select_branch(thompson_state()) -> pos_integer().

%% Record outcome for a branch
-spec record_outcome(thompson_state(), pos_integer(), success | failure) ->
    thompson_state().
```

---

### Contextual Strategy
**Module**: `strategy_contextual`

Context-aware branch selection using feature-based linear models.

#### State Record
```erlang
-record(contextual_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    feature_extractor :: function(),
    model :: #linear_model{} | undefined,
    min_samples = 10 :: pos_integer(),
    fallback_strategy :: atom()
}).
```

#### API Functions
```erlang
%% Start contextual strategy
-spec start_link(pos_integer(), map()) -> {ok, pid()} | {error, term()}.

%% Stop strategy
-spec stop(pid()) -> ok.

%% Predict best branch using contextual model
-spec predict_branch(pid(), map()) -> {ok, pos_integer()}.

%% Update model with observation
-spec update_model(pid(), map(), pos_integer(), float()) -> ok.

%% Set custom feature extractor
-spec set_feature_extractor(pid(), function()) -> ok.
```

---

## Utility Patterns

### Critical Section Pattern (WCP-26)
**Module**: `critical_section`

Provides mutual exclusion for shared resources using lock-based synchronization.

#### State Record
```erlang
-record(critical_section_state, {
    critical_fun :: function(),
    lock_id :: term(),
    input_data :: term() | undefined,
    result :: undefined | term(),
    lock_acquired = false :: boolean(),
    log_id :: binary() | undefined
}).
```

#### API Functions
```erlang
%% Create new critical section state
-spec new(CriticalFun :: function(), LockId :: term()) ->
    critical_section_state().

%% Start critical section workflow
-spec start(CriticalFun :: function(), LockId :: term()) ->
    {ok, pid()} | {error, term()}.

%% Run synchronously
-spec run(CriticalFun :: function(), InputData :: term()) ->
    {ok, term()} | {error, term()}.

%% Get state
-spec get_state(Pid :: pid()) -> {ok, critical_section_state()} | {error, term()}.

%% Execute within critical section
-spec execute(CriticalFun :: function(), LockId :: term(), InputData :: term()) ->
    {ok, term()} | {error, term()}.
```

#### Petri Net Structure
- **Places**: `p_start`, `p_lock_request`, `p_lock_wait`, `p_lock_acquired`, `p_critical`, `p_critical_done`, `p_lock_release`, `p_complete`
- **Transitions**: `t_request_lock`, `t_acquire_lock`, `t_enter_critical`, `t_execute`, `t_exit_critical`, `t_release_lock`, `t_complete`

#### Usage Example
```erlang
%% Execute function within critical section
Fun = fun(X) -> X * 2 end,
{ok, Result} = critical_section:execute(Fun, my_lock, 5).
%% Result: 10
```

---

### Milestone Pattern (WCP-18)
**Module**: `milestone`

Enables an activity only when a specific milestone state has been reached.

#### State Record
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

#### API Functions
```erlang
%% Create new milestone state
-spec new(ActivityFun :: function(), MilestoneFun :: function()) ->
    milestone_state().

%% Start milestone workflow
-spec start(ActivityFun :: function(), MilestoneFun :: function()) ->
    {ok, pid()} | {error, term()}.

%% Run synchronously
-spec run(ActivityFun :: function(), InitialState :: term()) ->
    {ok, term()} | {error, term()}.

%% Get state
-spec get_state(Pid :: pid()) -> {ok, milestone_state()} | {error, term()}.

%% Execute with milestone guard
-spec execute(ActivityFun :: function(), MilestoneFun :: function(), InitialState :: term()) ->
    {ok, term()} | {error, term()}.

%% Manually set milestone as reached
-spec set_milestone(Pid :: pid()) -> ok | {error, term()}.

%% Check if milestone has been reached in marking
-spec milestone_reached(pnet_marking:marking(), MilestonePlace :: atom()) ->
    boolean().

%% Enable activity only after milestone reached
-spec enable_on_milestone(pnet_marking:marking(), MilestonePlace :: atom(),
                         ActivityPlace :: atom()) -> boolean().

%% Disable activity after milestone reached
-spec disable_on_milestone(pnet_marking:marking(), MilestonePlace :: atom(),
                          ActivityPlace :: atom()) -> boolean().

%% Verify milestone constraints
-spec milestone_check(pnet_marking:marking(), MilestoneConfig :: map()) ->
    {ok, true} | {error, term()}.

%% Check if milestone has been passed
-spec milestone_passed(pnet_marking:marking(),
                      MilestoneSpec :: atom() | {atom(), [atom()]}) -> boolean().
```

#### Petri Net Structure
- **Places**: `p_start`, `p_milestone_guard`, `p_milestone_ready`, `p_milestone_reached`, `p_activity_pending`, `p_activity_active`, `p_activity_done`, `p_complete`
- **Transitions**: `t_start`, `t_check_milestone`, `t_reach_milestone`, `t_enable_activity`, `t_execute`, `t_complete`

#### Usage Example
```erlang
%% Activity executes only after milestone reached
Activity = fun() -> io:format("Activity executed!~n") end,
MilestoneCheck = fun(State) -> State =:= reached end,
{ok, Result} = milestone:execute(Activity, MilestoneCheck, initial).
```

---

### Circuit Breaker Pattern
**Module**: `circuit_breaker`

Prevents cascading failures by stopping execution after a threshold of failures is reached.

#### State Records
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

#### Types
```erlang
-type breaker_name() :: binary().
-type breaker_result() :: {ok, term()} | {error, circuit_open | timeout | term()}.
-type breaker_config() :: #breaker_config{}.
-type circuit_state() :: #circuit_state{}.
```

#### API Functions
```erlang
%% Start circuit breaker with default config
-spec start_link(breaker_name(), fun()) -> {ok, pid()} | {error, term()}.

%% Start with custom config
-spec start_link(breaker_name(), fun(), proplists:proplist()) ->
    {ok, pid()} | {error, term()}.

%% Execute function through circuit breaker
-spec execute(breaker_name(), fun()) -> breaker_result().

%% Execute with timeout
-spec execute(breaker_name(), fun(), pos_integer()) -> breaker_result().

%% Reset circuit breaker to closed state
-spec reset(breaker_name()) -> ok | {error, not_found}.

%% Get current circuit state
-spec get_state(breaker_name()) -> {ok, circuit_state()} | {error, not_found}.

%% Stop circuit breaker
-spec stop(breaker_name()) -> ok.
```

#### States
- **Closed** - Normal operation, requests pass through
- **Open** - Failed threshold reached, requests are rejected
- **Half-Open** - Testing if service has recovered

#### Usage Example
```erlang
%% Create circuit breaker for external service
ServiceFun = fun() -> http:get("https://api.example.com") end,
{ok, Pid} = circuit_breaker:start_link(<<"my_api">>, ServiceFun, [
    {failure_threshold, 3},
    {timeout_ms, 30000}
]),

%% Execute through circuit breaker
case circuit_breaker:execute(<<"my_api">>, ServiceFun) of
    {ok, Result} -> handle_success(Result);
    {error, circuit_open} -> handle_circuit_open()
end.
```

---

### Arbitrary Cycles Pattern (P10)
**Module**: `arbitrary_cycles`

Allows cycles with arbitrary entry/exit points in the workflow.

#### State Record
```erlang
-record(state, {
    nodes :: [atom()],
    cycles :: #{atom() => [atom()]}
}).
```

---

### Structured Loop Pattern
**Module**: `structured_loop`

Implements structured cycle with single entry/exit point.

---

### Recursion Pattern
**Module**: `recursion`

Implements recursive workflow patterns.

---

### Discriminator Pattern
**Module**: `discriminator`

Waits for first branch to complete, ignores others.

---

### OR-Join Pattern
**Module**: `or_join`

Merges when any incoming branch completes.

---

### Interleaved Parallel Pattern
**Module**: `interleaved_parallel`

Interleaves execution of parallel branches.

---

### Interleaved Routing Pattern
**Module**: `interleaved_routing`

Routes tasks in interleaved fashion.

---

### Deferred Choice Pattern
**Module**: `deferred_choice`

Defers branch selection until execution time.

---

### Transient Trigger Pattern (P23)
**Module**: `transient_trigger`

Event only matters while specific task is enabled.

#### State Record
```erlang
-record(state, {
    enabled_only_in :: atom(),
    event_received = false :: boolean()
}).
```

---

### Persistent Trigger Pattern (P24)
**Module**: `persistent_trigger`

Event persists until consumed.

#### State Record
```erlang
-record(state, {
    consumed_in :: atom(),
    event_persistent = false :: boolean()
}).
```

---

### Pattern Learning Module
**Module**: `pattern_learning`

Learns optimal pattern configurations from execution data.

---

### Exception Patterns Module
**Module**: `exception_patterns`

Handles workflow exceptions and recovery.

---

### Cancellation Module
**Module**: `cancellation`

Core cancellation functionality.

---

## Pattern Classification

### By Workflow Patterns Initiative (WCP) Number

| Pattern | WCP # | Module |
|---------|-------|--------|
| Sequence | WCP-01 | `sequence` |
| Parallel Split | WCP-02 | `parallel_split` |
| Synchronization | WCP-03 | `synchronization` |
| Exclusive Choice | WCP-04 | `exclusive_choice` |
| Simple Merge | WCP-05 | `simple_merge` |
| Multiple Choice | WCP-06 | `multiple_choice` |
| Multiple Merge | WCP-07 | `multiple_merge` |
| Synchronization Merge | WCP-08 | `synchronization` |
| N-out-of-M | WCP-09 | `n_out_of_m` |
| Arbitrary Cycles | WCP-10 | `arbitrary_cycles` |
| Implicit Termination | WCP-11 | `implicit_termination` |
| Multiple Instances (No Sync) | WCP-12 | `multi_instance` |
| Multiple Instances (Design Time) | WCP-13 | `multi_instance` |
| Multiple Instances (Runtime) | WCP-14 | `multi_instance` |
| Milestone | WCP-18 | `milestone` |
| Critical Section | WCP-26 | `critical_section` |
| Cancel Activity | P19 | `cancel_activity` |
| Cancel Case | P20 | `cancel_case` |
| Transient Trigger | P23 | `transient_trigger` |
| Persistent Trigger | P24 | `persistent_trigger` |
| Cancel MI Activity | P26 | `cancel_mi_activity` |
| Complete MI Activity | P27 | `complete_mi_activity` |
| Blocking Discriminator | P28 | `blocking_discriminator` |
| Cancelling Discriminator | P29 | `cancelling_discriminator` |
| Structured Partial Join | P30 | `structured_partial_join` |
| Blocking Partial Join | P31 | `blocking_partial_join` |
| Cancelling Partial Join | P32 | `cancelling_partial_join` |
| Generalized AND-Join | P33 | `generalized_and_join` |
| Static Partial Join for MI | P34 | `static_partial_join_mi` |
| Cancelling Partial Join for MI | P35 | `cancelling_partial_join_mi` |
| Dynamic Partial Join for MI | P36 | `dynamic_partial_join_mi` |
| Explicit Termination | P43 | `explicit_termination` |

### By Category

**Control Flow**: sequence, parallel_split, synchronization, exclusive_choice, simple_merge, multiple_choice, multiple_merge, arbitrary_cycles, structured_loop, recursion, discriminator, or_join, deferred_choice, implicit_termination, explicit_termination

**Advanced Branching**: structured_partial_join, generalized_and_join, blocking_partial_join, cancelling_partial_join, static_partial_join_mi, cancelling_partial_join_mi, dynamic_partial_join_mi, local_sync_merge, general_sync_merge, thread_split, thread_merge, interleaved_parallel, interleaved_routing

**Cancellation**: cancel_activity, cancel_case, cancel_region, cancel_mi_activity, complete_mi_activity, cancelling_discriminator, blocking_discriminator

**Data Operations**: data_transform, data_distribute, data_accumulate, data_visibility, param_pass

**Resource Management**: direct_resource_creation, resource_allocation, resource_deallocation, resource_initialization, role_based_allocation

**Multiple Instances**: multi_instance, multiple_instances_sync, static_partial_join_mi, cancelling_partial_join_mi, dynamic_partial_join_mi

**Triggers**: transient_trigger, persistent_trigger, milestone

**Fault Tolerance**: circuit_breaker, critical_section

**Machine Learning**: rl_agent, strategy_q_learning, strategy_ucb, strategy_thompson_sampling, strategy_contextual, pattern_learning

---

## Compilation

After modifying any pattern module, run:

```bash
rebar3 compile
```

This ensures all changes are correctly compiled and type-checked.

---

## Testing

Pattern modules use EUnit for unit tests and Common Test for integration tests:

```bash
# Run all pattern tests
rebar3 eunit

# Run specific pattern module test
rebar3 eunit --module=parallel_split

# Run integration tests
rebar3 ct
```

---

**Last Updated:** 2026-02-09
