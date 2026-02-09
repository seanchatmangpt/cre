%% -*- erlang -*-
%% @doc Reinforcement Learning Agent for Process Mining
%%
%% gen_statem RL agent for workflow intervention.
%%
%% @end

-module(rl_agent).
-behaviour(gen_statem).

%% API
-export([start_link/2, stop/1]).
-export([recommend_action/2, record_reward/2, observe_next_state/2]).
-export([get_policy/1, set_learning_rate/2, pause/1, resume/1]).
-export([get_statistics/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([initializing/3, observing/3, selecting_action/3, intervening/3, learning/3, paused/3, handle_event/3]).

%% Records
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

-type rl_action() :: #rl_action{}.
-type policy_type() :: tabular_q | deep_q.

%%====================================================================
%% API
%%====================================================================

-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(AgentId, Options) when is_binary(AgentId), is_map(Options) ->
    gen_statem:start_link({via, registry, {local, AgentId}}, ?MODULE, [AgentId, Options], []).

-spec stop(binary()) -> ok.
stop(AgentId) ->
    gen_statem:stop(AgentId).

%% @doc Recommend action based on current state.
-spec recommend_action(binary(), map()) -> {ok, #rl_action{}} | {error, term()}.
recommend_action(AgentId, StateFeatures) ->
    gen_statem:call(AgentId, {recommend_action, StateFeatures}).

%% @doc Record reward for last action.
-spec record_reward(binary(), float()) -> ok.
record_reward(AgentId, Reward) when is_float(Reward) ->
    gen_statem:cast(AgentId, {record_reward, Reward}).

%% @doc Observe next state after action.
-spec observe_next_state(binary(), map()) -> ok.
observe_next_state(AgentId, NextState) when is_map(NextState) ->
    gen_statem:cast(AgentId, {observe_next_state, NextState}).

%% @doc Get current policy.
-spec get_policy(binary()) -> {ok, map()}.
get_policy(AgentId) ->
    gen_statem:call(AgentId, get_policy).

%% @doc Set learning rate.
-spec set_learning_rate(binary(), float()) -> ok | {error, term()}.
set_learning_rate(AgentId, Rate) when is_float(Rate), Rate > 0, Rate =< 1 ->
    gen_statem:call(AgentId, {set_learning_rate, Rate});
set_learning_rate(_AgentId, _Rate) ->
    {error, invalid_rate}.

%% @doc Pause the agent.
-spec pause(binary()) -> ok.
pause(AgentId) ->
    gen_statem:call(AgentId, pause).

%% @doc Resume the agent.
-spec resume(binary()) -> ok.
resume(AgentId) ->
    gen_statem:cast(AgentId, resume).

%% @doc Get agent statistics.
-spec get_statistics(binary()) -> {ok, map()}.
get_statistics(AgentId) ->
    gen_statem:call(AgentId, get_statistics).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

init([AgentId, Options]) ->
    PatternId = maps:get(pattern_id, Options, <<"default_pattern">>),
    WorkflowId = maps:get(workflow_id, Options, <<"default_workflow">>),
    QTable = ets:new(q_table, [set, private]),

    StateData = #rl_agent_state{
        agent_id = AgentId,
        pattern_id = PatternId,
        workflow_id = WorkflowId,
        state_space = maps:get(state_space, Options, #{}),
        action_space = maps:get(action_space, Options, #{}),
        q_table = QTable,
        policy = maps:get(policy, Options, tabular_q),
        learning_rate = maps:get(learning_rate, Options, 0.1),
        discount_factor = maps:get(discount_factor, Options, 0.95),
        exploration_rate = maps:get(exploration_rate, Options, 1.0),
        exploration_decay = maps:get(exploration_decay, Options, 0.995),
        max_interventions = maps:get(max_interventions, Options, 100),
        intervention_count = 0,
        total_reward = 0.0,
        episode_count = 0,
        last_state = undefined,
        last_action = undefined,
        last_reward = undefined
    },

    {ok, observing, StateData}.

callback_mode() ->
    state_functions.

%%====================================================================
%% State Functions
%%====================================================================

initializing(call, {recommend_action, _StateFeatures}, State) ->
    {keep_state, State, [{reply, {error, not_ready}}]};

initializing(cast, _Event, State) ->
    {keep_state, State};

initializing(info, _Info, State) ->
    {keep_state, State};

initializing(state_timeout, _, State) ->
    {next_state, observing, State};

initializing(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, State).

observing(call, {recommend_action, StateFeatures}, State) ->
    Action = select_action_egreedy(StateFeatures, State),
    %% Store state and action for learning
    StateKey = encode_state(StateFeatures),
    NewState = State#rl_agent_state{
        last_state = StateKey,
        last_action = Action#rl_action.action_type,
        last_reward = undefined
    },
    {keep_state, NewState, [{reply, {ok, Action}}]};

observing(call, get_policy, State) ->
    Policy = #{
        learning_rate => State#rl_agent_state.learning_rate,
        exploration_rate => State#rl_agent_state.exploration_rate,
        policy_type => State#rl_agent_state.policy
    },
    {keep_state, State, [{reply, {ok, Policy}}]};

observing(call, {set_learning_rate, Rate}, State) when is_float(Rate), Rate > 0, Rate =< 1 ->
    {keep_state, State#rl_agent_state{learning_rate = Rate}, [{reply, ok}]};

observing(call, pause, State) ->
    {next_state, paused, State, [{reply, ok}]};

observing(call, get_statistics, State) ->
    Stats = #{
        agent_id => State#rl_agent_state.agent_id,
        pattern_id => State#rl_agent_state.pattern_id,
        workflow_id => State#rl_agent_state.workflow_id,
        intervention_count => State#rl_agent_state.intervention_count,
        total_reward => State#rl_agent_state.total_reward,
        episode_count => State#rl_agent_state.episode_count,
        exploration_rate => State#rl_agent_state.exploration_rate,
        learning_rate => State#rl_agent_state.learning_rate
    },
    {keep_state, State, [{reply, {ok, Stats}}]};

observing(cast, _Event, State) ->
    {keep_state, State};

observing(info, _Info, State) ->
    {keep_state, State};

observing(state_timeout, _, State) ->
    {keep_state, State};

observing(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, State).

selecting_action(call, get_policy, State) ->
    Policy = #{
        learning_rate => State#rl_agent_state.learning_rate,
        exploration_rate => State#rl_agent_state.exploration_rate,
        policy_type => State#rl_agent_state.policy
    },
    {keep_state, State, [{reply, {ok, Policy}}]};

selecting_action(call, {set_learning_rate, Rate}, State) ->
    {keep_state, State#rl_agent_state{learning_rate = Rate}, [{reply, ok}]};

selecting_action(call, pause, State) ->
    {next_state, paused, State, [{reply, ok}]};

selecting_action(call, get_statistics, State) ->
    Stats = #{
        agent_id => State#rl_agent_state.agent_id,
        pattern_id => State#rl_agent_state.pattern_id,
        workflow_id => State#rl_agent_state.workflow_id,
        intervention_count => State#rl_agent_state.intervention_count,
        total_reward => State#rl_agent_state.total_reward,
        episode_count => State#rl_agent_state.episode_count,
        exploration_rate => State#rl_agent_state.exploration_rate,
        learning_rate => State#rl_agent_state.learning_rate
    },
    {keep_state, State, [{reply, {ok, Stats}}]};

selecting_action(cast, {execute_action, _Action}, State) ->
    {next_state, intervening, State};

selecting_action(cast, _Event, State) ->
    {keep_state, State};

selecting_action(info, _Info, State) ->
    {keep_state, State};

selecting_action(state_timeout, _, State) ->
    {next_state, observing, State};

selecting_action(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, State).

intervening(call, get_policy, State) ->
    Policy = #{
        learning_rate => State#rl_agent_state.learning_rate,
        exploration_rate => State#rl_agent_state.exploration_rate,
        policy_type => State#rl_agent_state.policy
    },
    {keep_state, State, [{reply, {ok, Policy}}]};

intervening(call, {set_learning_rate, Rate}, State) ->
    {keep_state, State#rl_agent_state{learning_rate = Rate}, [{reply, ok}]};

intervening(call, pause, State) ->
    {next_state, paused, State, [{reply, ok}]};

intervening(call, get_statistics, State) ->
    Stats = #{
        agent_id => State#rl_agent_state.agent_id,
        pattern_id => State#rl_agent_state.pattern_id,
        workflow_id => State#rl_agent_state.workflow_id,
        intervention_count => State#rl_agent_state.intervention_count,
        total_reward => State#rl_agent_state.total_reward,
        episode_count => State#rl_agent_state.episode_count,
        exploration_rate => State#rl_agent_state.exploration_rate,
        learning_rate => State#rl_agent_state.learning_rate
    },
    {keep_state, State, [{reply, {ok, Stats}}]};

intervening(cast, {record_reward, Reward}, State) when is_float(Reward) ->
    NewTotal = State#rl_agent_state.total_reward + Reward,
    NewCount = State#rl_agent_state.intervention_count + 1,
    %% Store reward for learning step
    UpdatedState = State#rl_agent_state{
        total_reward = NewTotal,
        intervention_count = NewCount,
        last_reward = Reward
    },
    {next_state, learning, UpdatedState};

intervening(cast, _Event, State) ->
    {keep_state, State};

intervening(info, _Info, State) ->
    {keep_state, State};

intervening(state_timeout, _, State) ->
    {next_state, observing, State};

intervening(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, State).

learning(call, get_policy, State) ->
    Policy = #{
        learning_rate => State#rl_agent_state.learning_rate,
        exploration_rate => State#rl_agent_state.exploration_rate,
        policy_type => State#rl_agent_state.policy
    },
    {keep_state, State, [{reply, {ok, Policy}}]};

learning(call, {set_learning_rate, Rate}, State) ->
    {keep_state, State#rl_agent_state{learning_rate = Rate}, [{reply, ok}]};

learning(call, pause, State) ->
    {next_state, paused, State, [{reply, ok}]};

learning(call, get_statistics, State) ->
    Stats = #{
        agent_id => State#rl_agent_state.agent_id,
        pattern_id => State#rl_agent_state.pattern_id,
        workflow_id => State#rl_agent_state.workflow_id,
        intervention_count => State#rl_agent_state.intervention_count,
        total_reward => State#rl_agent_state.total_reward,
        episode_count => State#rl_agent_state.episode_count,
        exploration_rate => State#rl_agent_state.exploration_rate,
        learning_rate => State#rl_agent_state.learning_rate
    },
    {keep_state, State, [{reply, {ok, Stats}}]};

learning(cast, {observe_next_state, NextState}, #rl_agent_state{
    q_table = QTable,
    last_state = LastState,
    last_action = LastAction,
    last_reward = Reward,
    learning_rate = Alpha,
    discount_factor = Gamma,
    exploration_rate = Epsilon,
    exploration_decay = Decay
} = State) when LastState =/= undefined, LastAction =/= undefined, Reward =/= undefined ->
    %% Perform Q-learning update
    NextStateKey = encode_state(NextState),
    update_q_value(QTable, LastState, LastAction, Reward, NextStateKey, Alpha, Gamma),
    %% Decay exploration rate
    NewEpsilon = max(0.01, Epsilon * Decay),
    NewEpisodeCount = State#rl_agent_state.episode_count + 1,
    %% Return to observing state, reset learning fields
    {next_state, observing, State#rl_agent_state{
        exploration_rate = NewEpsilon,
        episode_count = NewEpisodeCount,
        last_state = undefined,
        last_action = undefined,
        last_reward = undefined
    }};

learning(cast, {observe_next_state, _NextState}, State) ->
    %% No previous state/action to learn from
    {next_state, observing, State};

learning(cast, _Event, State) ->
    {keep_state, State};

learning(info, _Info, State) ->
    {keep_state, State};

learning(state_timeout, _, State) ->
    {next_state, observing, State};

learning(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, State).

paused(cast, resume, State) ->
    {next_state, observing, State};

paused(call, pause, State) ->
    {keep_state, State, [{reply, ok}]};

paused(call, get_statistics, State) ->
    Stats = #{
        agent_id => State#rl_agent_state.agent_id,
        pattern_id => State#rl_agent_state.pattern_id,
        workflow_id => State#rl_agent_state.workflow_id,
        intervention_count => State#rl_agent_state.intervention_count,
        total_reward => State#rl_agent_state.total_reward,
        episode_count => State#rl_agent_state.episode_count,
        exploration_rate => State#rl_agent_state.exploration_rate,
        learning_rate => State#rl_agent_state.learning_rate
    },
    {keep_state, State, [{reply, {ok, Stats}}]};

paused(cast, _Event, State) ->
    {keep_state, State};

paused(info, _Info, State) ->
    {keep_state, State};

paused(state_timeout, _, State) ->
    {keep_state, State};

paused(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, State).

%% @private
-spec handle_event(gen_statem:event_type(), term(), #rl_agent_state{}) ->
    gen_statem:state_enter_result(#rl_agent_state{}).
handle_event(EventType, EventContent, State) ->
    logger:warning("Unknown event in rl_agent: ~p ~p", [EventType, EventContent]),
    {keep_state, State}.

%% @private
-spec handle_common_event(gen_statem:event_type(), term(), #rl_agent_state{}) ->
    gen_statem:state_enter_result(#rl_agent_state{}) | gen_statem:handle_event_result(#rl_agent_state{}).
handle_common_event(call, Request, State) ->
    logger:warning("Unhandled call in rl_agent: ~p", [Request]),
    {keep_state, State, [{reply, {error, unknown_request}}]};

handle_common_event(cast, Event, State) ->
    logger:debug("Unhandled cast in rl_agent: ~p", [Event]),
    {keep_state, State};

handle_common_event(info, Info, State) ->
    logger:debug("Unhandled info in rl_agent: ~p", [Info]),
    {keep_state, State};

handle_common_event(state_timeout, _, State) ->
    {keep_state, State}.

%%====================================================================
%% gen_statem Callbacks
%%====================================================================

%% @private
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                StateName :: atom(), StateData :: #rl_agent_state{}) -> term().
terminate(_Reason, _StateName, #rl_agent_state{q_table = QTable}) ->
    %% Clean up ETS table
    catch ets:delete(QTable),
    ok.

%% @private
-spec code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
                  StateData :: #rl_agent_state{}, Extra :: term()) ->
    {ok, StateName :: atom(), StateData :: #rl_agent_state{}}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec select_action_egreedy(map(), #rl_agent_state{}) -> rl_action().
select_action_egreedy(StateFeatures, #rl_agent_state{
    q_table = QTable,
    exploration_rate = Epsilon,
    action_space = ActionSpace
}) ->
    Actions = available_actions(ActionSpace),
    case rand:uniform() of
        X when X < Epsilon ->
            %% Explore: random action
            select_random_action(Actions);
        _ ->
            %% Exploit: best action from Q-table
            StateKey = encode_state(StateFeatures),
            select_best_action(QTable, StateKey, Actions)
    end.

%% @private
-spec available_actions(map()) -> [reroute | skip | prioritize | parallelize | no_action].
available_actions(_ActionSpace) ->
    [reroute, skip, prioritize, parallelize, no_action].

%% @private
-spec select_random_action([atom()]) -> rl_action().
select_random_action([]) ->
    #rl_action{action_type = no_action, target = <<>>, parameters = #{}};
select_random_action(Actions) ->
    ActionType = lists:nth(rand:uniform(length(Actions)), Actions),
    #rl_action{action_type = ActionType, target = <<>>, parameters = #{}}.

%% @private
-spec select_best_action(ets:tid(), term(), [atom()]) -> rl_action().
select_best_action(QTable, StateKey, Actions) ->
    QValues = [{A, get_q_value(QTable, StateKey, A)} || A <- Actions],
    {BestAction, _QValue} = max_by_q(QValues),
    #rl_action{action_type = BestAction, target = <<>>, parameters = #{}}.

%% @private
-spec get_q_value(ets:tid(), term(), atom()) -> float().
get_q_value(QTable, StateKey, Action) ->
    case ets:lookup(QTable, {StateKey, Action}) of
        [{_, QVal}] -> QVal;
        [] -> 0.0
    end.

%% @private
-spec encode_state(map()) -> term().
encode_state(StateFeatures) ->
    %% Simple state encoding: sort keys and create tuple
    SortedKeys = lists:sort(maps:keys(StateFeatures)),
    list_to_tuple([maps:get(K, StateFeatures) || K <- SortedKeys]).

%% @private
-spec max_by_q([{atom(), float()}]) -> {atom(), float()}.
max_by_q([{Action, Q}]) -> {Action, Q};
max_by_q([{Action1, Q1}, {_Action2, Q2} | Rest]) when Q1 >= Q2 ->
    max_by_q([{Action1, Q1} | Rest]);
max_by_q([{_Action1, _Q1}, {Action2, Q2} | Rest]) ->
    max_by_q([{Action2, Q2} | Rest]).

%% @private
-spec update_q_value(ets:tid(), term(), atom(), float(), term(), float(), float()) -> ok.
update_q_value(QTable, StateKey, Action, Reward, NextStateKey, Alpha, Gamma) ->
    CurrentQ = get_q_value(QTable, StateKey, Action),
    %% Q-learning: Q(s,a) = Q(s,a) + alpha * (reward + gamma * max(Q(s',a')) - Q(s,a))
    MaxNextQ = max_q_for_state(QTable, NextStateKey),
    NewQ = CurrentQ + Alpha * (Reward + Gamma * MaxNextQ - CurrentQ),
    ets:insert(QTable, {{StateKey, Action}, NewQ}),
    ok.

%% @private
-spec max_q_for_state(ets:tid(), term()) -> float().
max_q_for_state(QTable, StateKey) ->
    Actions = [reroute, skip, prioritize, parallelize, no_action],
    QValues = [get_q_value(QTable, StateKey, A) || A <- Actions],
    case QValues of
        [] -> 0.0;
        _ -> lists:max(QValues)
    end.
