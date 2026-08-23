# RL Strategy Patterns for Branch Selection

This document details the Reinforcement Learning (RL) strategy modules for intelligent branch selection in the CRE workflow system. These modules implement various multi-armed bandit and reinforcement learning algorithms for optimizing N-of-M pattern decisions.

## Overview

The RL strategy modules provide adaptive branch selection algorithms that learn from experience to optimize workflow performance. They are designed for use with the N-out-of-M (WCP-09) pattern and Multiple Instance patterns where selecting optimal execution paths improves throughput and reduces latency.

## Table of Contents

- [Available Strategies](#available-strategies)
- [Common Types and Records](#common-types-and-records)
- [Strategy Comparison](#strategy-comparison)
- [Usage Examples](#usage-examples)
- [Strategy Modules](#strategy-modules)

---

## Available Strategies

| Strategy Module | Algorithm | Use Case | Exploration |
|----------------|-----------|----------|-------------|
| `strategy_q_learning` | Tabular Q-Learning | Static environments, discrete states | Epsilon-greedy |
| `strategy_ucb` | Upper Confidence Bound (UCB1) | Optimistic exploration | Deterministic |
| `strategy_thompson_sampling` | Thompson Sampling | Bayesian optimization | Probabilistic |
| `strategy_contextual` | Linear Contextual Bandits | Feature-aware decisions | Feature-dependent |
| `strategy_first_n` | First-N Selection | Simple heuristics | None |
| `strategy_fastest_n` | Fastest-N Selection | Performance-based | Performance-based |
| `strategy_quality` | Quality-Based Selection | Outcome-based | Outcome-based |

---

## Common Types and Records

### Strategy State
```erlang
-record(strategy_state, {
    n :: pos_integer(),          %% Total number of branches
    m :: pos_integer(),          %% Number to select (for N-of-M)
    selected :: [pos_integer()], %% Currently selected branches
    history :: map(),            %% Execution history
    config :: map()              %% Strategy-specific config
}).
```

### Branch Outcome
```erlang
-type branch_outcome() :: success | failure | timeout | error.
-type branch_result() :: {ok, term()} | {error, term()}.
```

### Action Record
```erlang
-record(action, {
    branch :: pos_integer(),
    timestamp :: integer(),
    outcome :: branch_outcome(),
    duration_ms :: non_neg_integer(),
    reward :: float()
}).
```

---

## Strategy Comparison

### Q-Learning (strategy_q_learning)

**Algorithm**: Tabular Q-learning with epsilon-greedy exploration

**Pros**:
- Simple to implement and understand
- Guaranteed convergence with sufficient exploration
- Works well in small state spaces

**Cons**:
- Requires discrete state space
- Slow convergence in large state spaces
- Requires tuning of learning rate and epsilon

**Best For**: Static environments with moderate state spaces

**Key Parameters**:
```erlang
#{learning_rate => 0.1,      %% Alpha: how fast to learn
  discount_factor => 0.95,   %% Gamma: future reward discount
  epsilon => 1.0,            %% Initial exploration rate
  epsilon_min => 0.01,       %% Minimum exploration
  epsilon_decay => 0.995}    %% Exploration decay
```

### UCB1 (strategy_ucb)

**Algorithm**: Upper Confidence Bound

**Pros**:
- No hyperparameter tuning required (except C)
- Optimistic exploration naturally balances exploration/exploitation
- Theoretical regret bounds

**Cons**:
- Requires storing pull counts
- Can be slow to adapt to non-stationary environments
- Assumes bounded rewards

**Best For**: Stationary environments where you want automatic exploration

**Key Parameters**:
```erlang
#{c => 1.41}  %% Exploration parameter (sqrt(2) is optimal)
```

**UCB Formula**:
```
UCB(a) = avg_reward(a) + C * sqrt(ln(total_pulls) / pulls(a))
```

### Thompson Sampling (strategy_thompson_sampling)

**Algorithm**: Bayesian approach with Beta distributions

**Pros**:
- Natural exploration through posterior sampling
- Handles non-stationary environments well
- Computationally efficient

**Cons**:
- Requires conjugate prior (Beta-Bernoulli for binary outcomes)
- Less interpretable than UCB

**Best For**: Binary outcome scenarios with potentially changing branch characteristics

**Key Parameters**:
```erlang
%% Beta distribution parameters (updated automatically)
-record(bandit_arm, {
    branch_id :: pos_integer(),
    alpha :: pos_integer(),      %% Success + 1
    beta :: pos_integer(),        %% Failure + 1
    success_count :: non_neg_integer(),
    failure_count :: non_neg_integer()
}).
```

### Contextual Bandit (strategy_contextual)

**Algorithm**: Linear model with feature extraction

**Pros**:
- Can use context features for better decisions
- Learns from similar past situations
- Adapts to different workflow patterns

**Cons**:
- Requires meaningful features
- More complex to implement
- Needs feature engineering

**Best For**: Environments with rich contextual information

**Key Parameters**:
```erlang
#{feature_extractor => fun(Context) -> [float()] end,
  min_samples => 10,           %% Samples before using model
  fallback_strategy => ucb}    %% What to use when model unavailable
```

---

## Usage Examples

### Example 1: Simple Q-Learning Branch Selection

```erlang
%% Start Q-learning strategy for 5 branches
{ok, Pid} = strategy_q_learning:start_link(5, #{
    learning_rate => 0.1,
    epsilon => 0.2,  %% 20% exploration
    m => 2            %% Select top 2 branches
}),

%% Select branch for current state
StateFeatures = encode_workflow_state(CurrentState),
{ok, BranchId} = strategy_q_learning:select_action(Pid, StateFeatures),

%% Execute branch and record result
Result = execute_branch(BranchId, InputData),
Reward = calculate_reward(Result),
strategy_q_learning:update_q_value(Pid, StateFeatures, BranchId, Reward, NextState).
```

### Example 2: UCB1 for Parallel Branch Selection

```erlang
%% Initialize UCB1 for 3 branches, select 1
State = strategy_ucb:new(3, #{c => 1.41, m => 1}),

%% Select arm
{BranchId, NewState} = strategy_ucb:select_arm(State),

%% Record outcome (success = 1.0, failure = 0.0)
Reward = case execute_branch(BranchId) of
    {ok, _} -> 1.0;
    {error, _} -> 0.0
end,
UpdatedState = strategy_ucb:record_result(NewState, BranchId, Reward),

%% Get statistics
Stats = strategy_ucb:get_stats(UpdatedState).
```

### Example 3: Thompson Sampling for Racing Branches

```erlang
%% Initialize Thompson sampling (N=2 of M=5)
{ok, State} = strategy_thompson_sampling:init(2, 5),

%% Select branch
BranchId = strategy_thompson_sampling:select_branch(State),

%% Record outcome
State1 = strategy_thompson_sampling:record_outcome(
    State, BranchId, success
),

%% Check if we should complete (N branches selected)
case strategy_thompson_sampling:should_complete(State1, #{}) of
    true ->
        {ok, Results} = strategy_thompson_sampling:get_result(State1);
    false ->
        %% Select another branch
        NextBranch = strategy_thompson_sampling:select_branch(State1)
end.
```

### Example 4: Contextual Strategy with Features

```erlang
%% Define feature extractor
FeatureFun = fun(Context) ->
    [
        maps:get(priority, Context, 0) / 10.0,
        case maps:get(type, Context) of
            urgent -> 1.0;
            normal -> 0.0
        end,
        maps:get(queue_depth, Context, 0) / 100.0
    ]
end,

%% Start contextual strategy
{ok, Pid} = strategy_contextual:start_link(4, #{
    feature_extractor => FeatureFun,
    fallback_strategy => ucb
}),

%% Predict best branch for current context
Context = #{priority => 5, type => urgent, queue_depth => 10},
{ok, BranchId} = strategy_contextual:predict_branch(Pid, Context),

%% Update model after execution
strategy_contextual:update_model(Pid, Context, BranchId, 1.0).
```

---

## Strategy Modules

### strategy_q_learning

Tabular Q-Learning with epsilon-greedy exploration for branch selection.

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
%% Start Q-learning strategy server
-spec start_link(pos_integer(), map()) -> {ok, pid()} | {error, term()}.

%% Stop the server
-spec stop(pid()) -> ok.

%% Select action using epsilon-greedy
-spec select_action(pid(), term()) -> {ok, pos_integer()}.

%% Update Q-value after observing reward
-spec update_q_value(pid(), term(), pos_integer(), float(), term()) -> ok.

%% Get Q-table snapshot
-spec get_q_table(pid()) -> map().

%% Set exploration rate
-spec set_epsilon(pid(), float()) -> ok.

%% Set learning rate
-spec set_learning_rate(pid(), float()) -> ok.
```

#### Q-Learning Update Formula

```
Q(s,a) += alpha * (reward + gamma * max(Q(s',a')) - Q(s,a))
```

Where:
- `alpha` = learning rate
- `gamma` = discount factor
- `s` = current state
- `a` = action taken
- `s'` = next state

#### Usage Example

```erlang
%% Initialize with custom parameters
{ok, Pid} = strategy_q_learning:start_link(5, #{
    learning_rate => 0.2,
    discount_factor => 0.9,
    epsilon => 0.3,
    m => 2
}),

%% State encoder (convert context to state key)
Encoder = fun(Ctx) ->
    {maps:get(type, Ctx), maps:get(priority, Ctx)}
end,

%% Select branch for state
{ok, Branch} = strategy_q_learning:select_action(Pid, Encoder(Context)),

%% Update with reward
strategy_q_learning:update_q_value(
    Pid, Encoder(Context), Branch, 0.8, Encoder(NextContext)
).
```

---

### strategy_ucb

Upper Confidence Bound (UCB1) algorithm for multi-armed bandits.

#### State Record
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

#### UCB1 Formula

```
UCB(a) = avg_reward(a) + c * sqrt(ln(total_pulls + 1) / pulls(a))
```

Special handling for arms never pulled: returns infinity to force initial exploration.

#### Usage Example

```erlang
%% Create UCB1 for 4 arms
State = strategy_ucb:new(4, #{c => 1.5, m => 1}),

%% Selection loop
{Branch1, State1} = strategy_ucb:select_arm(State),
Reward1 = execute_and_score(Branch1),
State2 = strategy_ucb:record_result(State1, Branch1, Reward1),

%% Get arm statistics
Stats = strategy_ucb:get_stats(State2),
%% Stats = #{arms => #{1 => #{pulls => 1, avg_reward => 0.8}, ...},
%%         total_pulls => 1}
```

---

### strategy_thompson_sampling

Bayesian approach using Beta distributions for probabilistic branch selection.

#### State Record
```erlang
-record(thompson_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    arms :: [#bandit_arm{}],
    completed :: [pos_integer()],
    results :: map()
}).

-record(bandit_arm, {
    branch_id :: pos_integer(),
    alpha :: pos_integer(),
    beta :: pos_integer(),
    success_count :: non_neg_integer(),
    failure_count :: non_neg_integer()
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

#### Beta Distribution Sampling

Thompson sampling uses the Beta distribution:
```
X ~ Beta(alpha, beta)
Y ~ Beta(alpha', beta')
return X / (X + Y)
```

Where:
- `alpha = 1 + success_count`
- `beta = 1 + failure_count`

#### Usage Example

```erlang
%% Initialize (N=2, M=4)
{ok, State} = strategy_thompson_sampling:init(2, 4),

%% Select branch
Branch = strategy_thompson_sampling:select_branch(State),

%% Execute and record outcome
case execute_branch(Branch) of
    {ok, _} ->
        State1 = strategy_thompson_sampling:record_outcome(State, Branch, success);
    {error, _} ->
        State1 = strategy_thompson_sampling:record_outcome(State, Branch, failure)
end,

%% Mark branch as complete
State2 = strategy_thompson_sampling:on_branch_complete(State1, {Branch, result}),

%% Check if done
case strategy_thompson_sampling:should_complete(State2, #{}) of
    true -> {ok, Results} = strategy_thompson_sampling:get_result(State2);
    false -> continue_selection(State2)
end.
```

---

### strategy_contextual

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

-record(linear_model, {
    weights :: [float()],
    bias :: float(),
    samples :: pos_integer()
}).
```

#### API Functions

```erlang
%% Start contextual strategy server
-spec start_link(pos_integer(), map()) -> {ok, pid()} | {error, term()}.

%% Stop the server
-spec stop(pid()) -> ok.

%% Predict best branch using contextual model
-spec predict_branch(pid(), map()) -> {ok, pos_integer()}.

%% Update model with observation
-spec update_model(pid(), map(), pos_integer(), float()) -> ok.

%% Set custom feature extractor
-spec set_feature_extractor(pid(), function()) -> ok.
```

#### Default Features

The default feature extractor extracts:
```erlang
[
    maps:get(case_type, Context, 0.0),
    maps:get(priority, Context, 0.0),
    maps:get(value, Context, 0.0),
    maps:get(hour_of_day, Context, 0.0) / 24.0,
    maps:get(queue_depth, Context, 0.0)
]
```

#### Model Update

Uses online gradient descent:
```
error = prediction - target
gradient = error * features
weights = weights - learning_rate * gradient
bias = bias - learning_rate * error
```

#### Usage Example

```erlang
%% Custom feature extractor
FeatureFun = fun(Context) ->
    Priority = maps:get(priority, Context, normal),
    TypeScore = case maps:get(type, Context) of
        urgent -> 1.0;
        high -> 0.7;
        normal -> 0.3;
        low -> 0.0
    end,
    QueueLen = maps:get(queue_length, Context, 0) / 50.0,
    [Priority, TypeScore, QueueLen]
end,

%% Start contextual strategy
{ok, Pid} = strategy_contextual:start_link(4, #{
    feature_extractor => FeatureFun,
    fallback_strategy => ucb
}),

%% Make prediction
Context = #{
    priority => high,
    type => urgent,
    queue_length => 10
},
{ok, Branch} = strategy_contextual:predict_branch(Pid, Context),

%% Update model
strategy_contextual:update_model(Pid, Context, Branch, 1.0).
```

---

### strategy_first_n

Simple heuristic: always select the first N branches in order.

#### Use Case
- When branches are pre-ordered by priority
- Deterministic selection is required
- Testing and baseline comparison

#### Usage Example

```erlang
%% Always select branches 1 and 2
Branches = strategy_first_n:select(5, 2).
%% Branches = [1, 2]
```

---

### strategy_fastest_n

Select the N branches with best historical performance (lowest latency).

#### Use Case
- Performance-based routing
- Latency-sensitive workflows
- When execution time varies significantly

#### Usage Example

```erlang
%% Select 2 fastest branches from history
Branches = strategy_fastest_n:select(History, 5, 2).
```

---

### strategy_quality

Select branches based on historical success rates.

#### Use Case
- Reliability-critical workflows
- When failure rates vary significantly
- Quality-of-service prioritization

#### Usage Example

```erlang
%% Select 2 most reliable branches
Branches = strategy_quality:select(History, 5, 2).
```

---

## Integration with N-of-M Pattern

The strategy modules integrate with the N-out-of-M pattern (WCP-09) to intelligently select which branches to execute:

```erlang
%% Initialize strategy
{ok, StrategyPid} = strategy_q_learning:start_link(5, #{m => 2}),

%% In N-of-M pattern, select branches
Branches = select_n_of_m(StrategyPid, 5, 2),
%% Branches = [2, 4]

%% Execute selected branches
Results = execute_branches(Branches, InputData),

%% Update strategy with outcomes
lists:foreach(fun({Branch, Result}) ->
    Reward = calculate_reward(Result),
    strategy_q_learning:update_q_value(
        StrategyPid, State, Branch, Reward, NextState
    )
end, Results).
```

---

## Performance Considerations

### Q-Learning
- **Memory**: O(S x A) where S is state space size, A is number of actions
- **Time**: O(1) for selection, O(1) for update
- **Convergence**: Requires O(S x A x (1/(1-gamma))) episodes

### UCB1
- **Memory**: O(A) for arm statistics
- **Time**: O(A) for selection (find max UCB)
- **Regret**: O(sqrt(A x T)) where T is time steps

### Thompson Sampling
- **Memory**: O(A) for Beta parameters
- **Time**: O(A) for selection + sampling overhead
- **Regret**: O(sqrt(A x T)) similar to UCB1

### Contextual Bandit
- **Memory**: O(F) for model weights, F is feature dimension
- **Time**: O(F x A) for scoring all branches
- **Convergence**: Depends on feature quality

---

## Reward Function Design

Choosing the right reward function is crucial for strategy performance:

### Latency-Based Reward
```erlang
%% Lower latency = higher reward
calculate_reward(DurationMs) ->
    MaxDuration = 5000,  %% 5 seconds
    case DurationMs of
        D when D < 100 -> 1.0;        %% Fast
        D when D < 500 -> 0.7;        %% Normal
        D when D < 2000 -> 0.3;       %% Slow
        _ -> 0.1                        %% Very slow
    end.
```

### Success-Based Reward
```erlang
%% Binary outcome
calculate_reward({ok, _}) -> 1.0;
calculate_reward({error, _}) -> 0.0.
```

### Combined Reward
```erlang
%% Consider both success and latency
calculate_reward({ok, _}, DurationMs) when DurationMs < 1000 -> 1.0;
calculate_reward({ok, _}, DurationMs) when DurationMs < 5000 -> 0.7;
calculate_reward({ok, _}, _) -> 0.3;
calculate_reward({error, _}, _) -> -0.5.
```

---

## Testing Strategies

### Unit Testing Individual Strategies

```erlang
strategy_ucb_test_() ->
    [fun() ->
        State = strategy_ucb:new(3, #{}),
        ?assertEqual(3, State#ucb_state.n)
    end,
    fun() ->
        State = strategy_ucb:new(3, #{}),
        {Branch, _} = strategy_ucb:select_arm(State),
        ?assert(Branch >= 1 andalso Branch =< 3)
    end].
```

### Comparing Strategies

```erlang
%% Run comparison simulation
simulate(Strategies, NumArms, NumPulls) ->
    lists:map(fun(Strategy) ->
        {Strategy, run_simulation(Strategy, NumArms, NumPulls)}
    end, Strategies).
```

---

## Best Practices

1. **Start Simple**: Begin with UCB1 or Q-learning before trying more complex strategies
2. **Monitor Exploration**: Track epsilon/selection diversity to ensure adequate exploration
3. **Handle Non-Stationarity**: Use Thompson Sampling or adaptive epsilon for changing environments
4. **Feature Engineering**: Invest time in good features for contextual bandits
5. **Reward Scaling**: Keep rewards in [-1, 1] range for stable learning
6. **Warm-Up Period**: Allow initial exploration before making critical decisions
7. **A/B Testing**: Compare strategies against baselines in production

---

## Compilation

After modifying any strategy module:

```bash
rebar3 compile
```

---

## Further Reading

- Sutton & Barto, "Reinforcement Learning: An Introduction"
- Lattimore & Szepesvari, "Bandit Algorithms"
- Workflow Patterns Initiative: https://www.workflowpatterns.com/
