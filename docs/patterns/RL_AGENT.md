# RL Agent Module

## Module Overview

The `rl_agent` module implements a Reinforcement Learning (RL) agent for workflow intervention in process mining scenarios. It uses the `gen_statem` OTP behavior to manage the agent's lifecycle and state transitions during learning and decision-making.

The RL agent is designed to:
- Observe workflow states and recommend interventions
- Learn from rewards using Q-learning
- Support multiple exploration strategies
- Provide circuit breaker protection for cascading failures
- Track intervention statistics and performance metrics

## Architecture

### gen_statem Behavior

The RL agent is implemented as an `gen_statem` with the following state machine:

```
     +--------------+      pause      +--------+
     |  observing   | -------------> | paused |
     +--------------+                 +--------+
           ^    |                            |
           |    | record_reward              | resume
           |    v                            |
     +--------------+                        |
     | intervening | ------------------------+
     +--------------+
           |
           | observe_next_state
           v
     +--------------+
     |   learning   |
     +--------------+
           |
           | (update complete)
           v
     +--------------+
     |  observing   |
     +--------------+
```

### State Descriptions

| State | Purpose |
|-------|---------|
| `initializing` | Initial setup state before agent is ready |
| `observing` | Default state where agent accepts action recommendations |
| `selecting_action` | Internal state for action selection |
| `intervening` | Agent has recommended an action and is executing |
| `learning` | Q-learning update phase after observing next state |
| `paused` | Agent is suspended, not accepting new requests |

## State Management

### rl_agent_state Record

```erlang
-record(rl_agent_state, {
    agent_id :: binary(),              % Unique agent identifier
    pattern_id :: binary(),            % Associated workflow pattern
    workflow_id :: binary(),           % Workflow being monitored
    state_space :: map(),              % State feature definitions
    action_space :: map(),             % Available actions
    q_table :: ets:tid(),              % Q-value storage (ETS table)
    policy :: policy_type(),           % tabular_q | deep_q
    learning_rate :: float(),          % Alpha: 0.0-1.0 (default: 0.1)
    discount_factor :: float(),        % Gamma: 0.0-1.0 (default: 0.95)
    exploration_rate :: float(),       % Epsilon: 0.0-1.0 (default: 1.0)
    exploration_decay :: float(),      % Decay factor (default: 0.995)
    circuit_breaker :: pid() | undefined, % Optional circuit breaker
    intervention_count :: non_neg_integer(), % Total interventions
    max_interventions :: pos_integer(), % Intervention limit
    total_reward :: float(),           % Cumulative reward
    episode_count :: non_neg_integer(), % Learning episodes completed
    last_state :: term() | undefined,  % Previous state for learning
    last_action :: atom() | undefined, % Previous action for learning
    last_reward :: float() | undefined % Previous reward for learning
}).
```

### Q-Table Storage

The Q-table is stored in an ETS table with the following structure:
- **Key**: `{StateKey, Action}` tuple
- **Value**: Float Q-value
- **Access**: O(1) lookup and update

### Action Types

```erlang
-record(rl_action, {
    action_type :: reroute | skip | prioritize | parallelize | no_action,
    target :: binary(),          % Target workflow element
    parameters :: map()          % Action-specific parameters
}).
```

| Action Type | Description |
|-------------|-------------|
| `reroute` | Redirect workflow to alternative path |
| `skip` | Skip current workflow step |
| `prioritize` | Increase priority of current task |
| `parallelize` | Split task into parallel execution |
| `no_action` | Do not intervene |

## Strategy Plugins

The RL agent integrates with multiple strategy modules for different selection algorithms:

### Thompson Sampling (`strategy_thompson_sampling`)

Multi-armed bandit strategy using Bayesian optimization:
- Maintains Beta distribution parameters (alpha, beta) for each branch
- Samples from distributions and selects highest value
- Balances exploration and exploitation naturally

**API:**
```erlang
strategy_thompson_sampling:init(N, M) -> {ok, State}
strategy_thompson_sampling:select_branch(State) -> BranchId
strategy_thompson_sampling:record_outcome(State, BranchId, Outcome) -> NewState
```

### UCB1 (`strategy_ucb`)

Upper Confidence Bound algorithm:
- Uses confidence intervals for exploration
- Formula: `UCB = avg_reward + c * sqrt(ln(total_pulls) / pulls)`
- Default `c = 1.41` (sqrt(2))

**API:**
```erlang
strategy_ucb:new(N, Options) -> State
strategy_ucb:select_arm(State) -> {ArmId, NewState}
strategy_ucb:record_result(State, ArmId, Reward) -> NewState
```

### Q-Learning (`strategy_q_learning`)

Tabular Q-learning with epsilon-greedy exploration:
- gen_server implementation for persistent learning
- Supports online learning with state-action pairs
- Adjustable epsilon, learning rate, discount factor

**API:**
```erlang
strategy_q_learning:start_link(N, Options) -> {ok, Pid}
strategy_q_learning:select_action(Pid, State) -> {ok, Action}
strategy_q_learning:update_q_value(Pid, State, Action, Reward, NextState) -> ok
```

### Contextual Bandit (`strategy_contextual`)

Feature-based branch selection using linear models:
- Custom feature extractor function
- Online gradient descent for model updates
- Fallback strategy when model unavailable

**API:**
```erlang
strategy_contextual:start_link(N, Options) -> {ok, Pid}
strategy_contextual:predict_branch(Pid, Context) -> {ok, Branch}
strategy_contextual:update_model(Pid, Context, Branch, Reward) -> ok
```

### Other Strategies

| Module | Description |
|--------|-------------|
| `strategy_first_n` | Select first N branches (baseline) |
| `strategy_fastest_n` | Select based on historical completion times |
| `strategy_quality` | Select based on quality metrics |

## Circuit Breaker Integration

The RL agent supports circuit breaker integration for preventing cascading failures. The circuit breaker has three states:

### Circuit States

| State | Behavior |
|-------|----------|
| `closed` | Normal operation, requests pass through |
| `open` | Threshold reached, requests are rejected |
| `half_open` | Testing if service has recovered |

### Circuit Breaker API

```erlang
% Start a circuit breaker
circuit_breaker:start_link(Name, Fun) -> {ok, Pid}
circuit_breaker:start_link(Name, Fun, Options) -> {ok, Pid}

% Execute through the breaker
circuit_breaker:execute(Name, Fun) -> {ok, Result} | {error, Reason}
circuit_breaker:execute(Name, Fun, Timeout) -> {ok, Result} | {error, Reason}

% Query and control
circuit_breaker:get_state(Name) -> {ok, CircuitState}
circuit_breaker:reset(Name) -> ok
circuit_breaker:stop(Name) -> ok
```

### Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `failure_threshold` | pos_integer() | 5 | Failures before opening |
| `timeout_ms` | pos_integer() | 60000 | ms before half-open |
| `success_threshold` | pos_integer() | 2 | Successes to close |
| `call_timeout` | pos_integer() | 5000 | Execution timeout |

## API Reference

### Starting and Stopping

#### `start_link/2`

```erlang
-spec start_link(AgentId :: binary(), Options :: map()) ->
    {ok, pid()} | {error, term()}.
```

Starts an RL agent with the given ID and options.

**Options:**
- `pattern_id` (`binary()`) - Pattern identifier (default: `<<"default_pattern">>`)
- `workflow_id` (`binary()`) - Workflow identifier (default: `<<"default_workflow">>`)
- `state_space` (`map()`) - State feature definitions (default: `#{}`)
- `action_space` (`map()`) - Available actions (default: `#{}`)
- `policy` (`tabular_q | deep_q`) - Policy type (default: `tabular_q`)
- `learning_rate` (`float()`) - Alpha value 0.0-1.0 (default: 0.1)
- `discount_factor` (`float()`) - Gamma value 0.0-1.0 (default: 0.95)
- `exploration_rate` (`float()`) - Epsilon value 0.0-1.0 (default: 1.0)
- `exploration_decay` (`float()`) - Decay factor (default: 0.995)
- `max_interventions` (`pos_integer()`) - Intervention limit (default: 100)

#### `stop/1`

```erlang
-spec stop(AgentId :: binary()) -> ok.
```

Stops the RL agent.

### Action Recommendation

#### `recommend_action/2`

```erlang
-spec recommend_action(AgentId :: binary(), StateFeatures :: map()) ->
    {ok, #rl_action{}} | {error, term()}.
```

Recommends an action based on current state features using epsilon-greedy policy.

**State Features Example:**
```erlang
#{
    <<"queue_depth">> => 5,
    <<"avg_wait_time">> => 120.5,
    <<"worker_utilization">> => 0.85,
    <<"priority">> => 1
}
```

### Learning API

#### `record_reward/2`

```erlang
-spec record_reward(AgentId :: binary(), Reward :: float()) -> ok.
```

Records the reward for the last action. Rewards should be normalized between -1.0 and 1.0.

**Reward Guidelines:**
- Positive rewards for desirable outcomes
- Negative rewards for undesirable outcomes
- Scale based on magnitude of outcome

#### `observe_next_state/2`

```erlang
-spec observe_next_state(AgentId :: binary(), NextState :: map()) -> ok.
```

Observes the next state after an action, triggering Q-learning update.

### Policy Management

#### `get_policy/1`

```erlang
-spec get_policy(AgentId :: binary()) -> {ok, map()}.
```

Returns current policy parameters:
```erlang
#{
    learning_rate => 0.1,
    exploration_rate => 0.5,
    policy_type => tabular_q
}
```

#### `set_learning_rate/2`

```erlang
-spec set_learning_rate(AgentId :: binary(), Rate :: float()) ->
    ok | {error, invalid_rate}.
```

Sets the learning rate (alpha). Must be between 0.0 and 1.0.

### Control API

#### `pause/1`

```erlang
-spec pause(AgentId :: binary()) -> ok.
```

Pauses the agent, preventing new recommendations.

#### `resume/1`

```erlang
-spec resume(AgentId :: binary()) -> ok.
```

Resumes the agent from paused state.

### Statistics

#### `get_statistics/1`

```erlang
-spec get_statistics(AgentId :: binary()) -> {ok, map()}.
```

Returns comprehensive statistics:
```erlang
#{
    agent_id => <<"agent_1">>,
    pattern_id => <<"n_of_m">>,
    workflow_id => <<"wf_123">>,
    intervention_count => 42,
    total_reward => 15.3,
    episode_count => 42,
    exploration_rate => 0.6,
    learning_rate => 0.1
}
```

## Usage Examples

### Basic Agent Lifecycle

```erlang
% Start an agent
{ok, _Pid} = rl_agent:start_link(<<"agent_1">>, #{
    pattern_id => <<"n_of_m">>,
    workflow_id => <<"wf_123">>,
    learning_rate => 0.1,
    exploration_rate => 1.0
}).

% Get action recommendation
State = #{queue_depth => 5, avg_wait_time => 120.0},
{ok, Action} = rl_agent:recommend_action(<<"agent_1">>, State).

% Record reward (positive for good outcome)
rl_agent:record_reward(<<"agent_1">>, 0.8).

% Observe next state
NextState = #{queue_depth => 3, avg_wait_time => 80.0},
rl_agent:observe_next_state(<<"agent_1">>, NextState).

% Get statistics
{ok, Stats} = rl_agent:get_statistics(<<"agent_1">>).

% Stop agent
rl_agent:stop(<<"agent_1">>).
```

### Complete Episode

```erlang
episode(AgentId, InitialState) ->
    % Get action
    {ok, Action} = rl_agent:recommend_action(AgentId, InitialState),

    % Execute action (user code)
    Result = execute_action(Action),

    % Calculate reward based on result
    Reward = calculate_reward(Result),

    % Record reward
    rl_agent:record_reward(AgentId, Reward),

    % Observe new state
    NextState = observe_state(Result),
    rl_agent:observe_next_state(AgentId, NextState).
```

### Integration with Circuit Breaker

```erlang
% Start circuit breaker
{ok, _CBPid} = circuit_breaker:start_link(<<"wf_breaker">>, fun() ->
    % Protected workflow execution
    ok
end, [
    {failure_threshold, 3},
    {timeout_ms, 30000}
]).

% Execute through breaker
case circuit_breaker:execute(<<"wf_breaker">>, fun protected_workflow/0) of
    {ok, Result} -> handle_success(Result);
    {error, circuit_open} -> handle_circuit_open();
    {error, Reason} -> handle_error(Reason)
end.
```

### Custom Reward Function

```erlang
calculate_reward(WorkflowResult) ->
    BaseReward = case WorkflowResult of
        #{success := true, duration := D} when D < 1000 -> 1.0;
        #{success := true, duration := D} when D < 5000 -> 0.5;
        #{success := true} -> 0.1;
        #{success := false} -> -1.0
    end,
    % Adjust for resource usage
    ResourceBonus = case WorkflowResult of
        #{resource_usage := RU} when RU < 0.5 -> 0.2;
        _ -> 0.0
    end,
    BaseReward + ResourceBonus.
```

## Configuration Options

### Learning Parameters

| Parameter | Range | Default | Effect |
|-----------|-------|---------|--------|
| `learning_rate` | 0.0-1.0 | 0.1 | Higher = faster learning, less stable |
| `discount_factor` | 0.0-1.0 | 0.95 | Higher = values future rewards more |
| `exploration_rate` | 0.0-1.0 | 1.0 | Higher = more random exploration |
| `exploration_decay` | 0.0-1.0 | 0.995 | Lower = faster exploitation shift |

### Recommended Settings

| Scenario | Learning Rate | Discount Factor | Exploration Rate |
|----------|---------------|----------------|------------------|
| Stable environment | 0.1 | 0.95 | 0.1 |
| Changing environment | 0.3 | 0.8 | 0.3 |
| Fast learning | 0.5 | 0.9 | 0.5 |
| Conservative | 0.05 | 0.99 | 0.05 |

## State Encoding

States are encoded as sorted tuples for ETS key storage:

```erlang
encode_state(#{b => 2, a => 1, c => 3}) -> {1, 2, 3}
```

This ensures consistent keys regardless of map key order.

## Q-Learning Update

The agent implements standard Q-learning:

```
Q(s,a) = Q(s,a) + alpha * (reward + gamma * max(Q(s',a')) - Q(s,a))
```

Where:
- `Q(s,a)` - Current Q-value for state-action pair
- `alpha` - Learning rate
- `reward` - Observed reward
- `gamma` - Discount factor
- `max(Q(s',a'))` - Maximum Q-value for next state

## Thread Safety

The agent uses `gen_statem` which provides message serialization. All state updates are atomic. The ETS Q-table is owned by the agent process and accessed only from that process, ensuring consistency.

## Performance Considerations

- Q-table lookups are O(1) via ETS
- State encoding is O(n log n) due to sorting
- Action selection is O(m) where m is action space size
- Q-learning update is O(m) for max calculation

## See Also

- `circuit_breaker` - Circuit breaker pattern implementation
- `strategy_thompson_sampling` - Thompson sampling strategy
- `strategy_ucb` - UCB1 strategy
- `strategy_q_learning` - Q-learning strategy
- `strategy_contextual` - Contextual bandit strategy
- `gen_statem` - OTP behavior documentation
