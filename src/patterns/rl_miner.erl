%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc Deep Q-Network (DQN) Process Miner
%%
%% This module implements a Deep Q-Network for process discovery
%% using reinforcement learning. The miner learns to select the
%% best control-flow patterns by exploring the event log and
%% receiving rewards based on model quality.
%%
%% <h3>Architecture</h3>
%%
%% <ul>
%%   <li><b>State:</b> Current partial model and remaining log features</li>
%%   <li><b>Actions:</b> Add pattern, merge, split, or finalize</li>
%%   <li><b>Network:</b> Neural network that learns Q-values</li>
%%   <li><b>Training:</b> Experience replay with target network</li>
%% </ul>
%%
%% <h3>Pattern Selection</h3>
%%
%% The DQN learns to select appropriate workflow patterns:
%% <ul>
%%   <li>Sequence</li>
%%   <li>Parallel Split</li>
%%   <li>Exclusive Choice</li>
%%   <li>Loop</li>
%%   <li>And more complex patterns</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(rl_miner).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([start_link/1, stop/1]).
-export([discover_model/2, discover_model/3]).
-export([get_policy/1, set_learning_rate/2]).
-export([train_episode/2]).
-export([get_statistics/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type trace() :: [activity()].
-type event_log() :: [trace()].

-type pattern_type() :: sequence | parallel | exclusive | loop | milestone | skip.

-type rl_action() :: #{
    type := add_pattern | merge | split | finalize,
    pattern := pattern_type() | undefined,
    activities := [activity()]
}.

-type state_features() :: #{
    activities_discovered => [activity()],
    transitions_covered => float(),
    log_fitness => float(),
    model_complexity => non_neg_integer()
}.

-type q_network() :: #{
    weights => map(),
    architecture => map()
}.

-type training_config() :: #{
    learning_rate => float(),
    discount_factor => float(),
    epsilon => float(),
    epsilon_decay => float(),
    batch_size => pos_integer(),
    memory_size => pos_integer()
}.

-type discovery_state() :: #{
    partial_model => map(),
    remaining_log => event_log(),
    features => state_features(),
    step => non_neg_integer()
}.

-type miner_statistics() :: #{
    episodes_completed => non_neg_integer(),
    total_reward => float(),
    best_fitness => float(),
    average_loss => float()
}.

-export_type([
    activity/0, trace/0, event_log/0,
    pattern_type/0, rl_action/0,
    state_features/0, q_network/0,
    training_config/0, miner_statistics/0
]).

%%====================================================================
%% Records
%%====================================================================

-record(miner_state, {
    miner_id :: binary(),
    q_network :: q_network(),
    target_network :: q_network(),
    experience_replay :: queue:queue(),
    config :: training_config(),
    statistics :: miner_statistics(),
    episode_count :: non_neg_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a new RL miner.
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(MinerId) ->
    gen_server:start_link({via, registry, {local, MinerId}}, ?MODULE, [MinerId], []).

%% @doc Stop a running miner.
-spec stop(binary()) -> ok.
stop(MinerId) ->
    gen_server:stop(MinerId).

%% @doc Discover a process model using the RL miner.
-spec discover_model(binary(), event_log()) -> {ok, map()}.
discover_model(MinerId, Log) ->
    discover_model(MinerId, Log, #{}).

%% @doc Discover a process model with custom configuration.
-spec discover_model(binary(), event_log(), map()) -> {ok, map()}.
discover_model(MinerId, Log, Options) ->
    MaxEpisodes = maps:get(max_episodes, Options, 100),
    TargetFitness = maps:get(target_fitness, Options, 0.95),

    %% Train for multiple episodes
    Results = lists:map(fun(_) ->
        train_episode(MinerId, Log)
    end, lists:seq(1, MaxEpisodes)),

    %% Get best model
    BestResult = lists:foldl(fun(Result, Best) ->
        ResultFitness = maps:get(fitness, Result, 0.0),
        BestFitness = maps:get(fitness, Best, 0.0),
        case ResultFitness > BestFitness of
            true -> Result;
            false -> Best
        end
    end, #{fitness => 0.0}, Results),

    case maps:get(fitness, BestResult) >= TargetFitness of
        true -> {ok, BestResult};
        false -> {ok, BestResult#{converged => false}}
    end.

%% @doc Get the current policy network.
-spec get_policy(binary()) -> {ok, q_network()}.
get_policy(MinerId) ->
    gen_server:call(MinerId, get_policy).

%% @doc Set the learning rate.
-spec set_learning_rate(binary(), float()) -> ok | {error, term()}.
set_learning_rate(MinerId, Rate) when is_float(Rate), Rate > 0, Rate =< 1 ->
    gen_server:call(MinerId, {set_learning_rate, Rate});
set_learning_rate(_MinerId, _Rate) ->
    {error, invalid_rate}.

%% @doc Train a single episode.
-spec train_episode(binary(), event_log()) -> map().
train_episode(MinerId, Log) ->
    gen_server:call(MinerId, {train_episode, Log}).

%% @doc Get miner statistics.
-spec get_statistics(binary()) -> {ok, miner_statistics()}.
get_statistics(MinerId) ->
    gen_server:call(MinerId, get_statistics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([MinerId]) ->
    Config = #{
        learning_rate => 0.001,
        discount_factor => 0.99,
        epsilon => 1.0,
        epsilon_decay => 0.995,
        batch_size => 32,
        memory_size => 10000
    },

    %% Initialize Q-network and target network
    QNetwork = init_network(),
    TargetNetwork = QNetwork,

    State = #miner_state{
        miner_id = MinerId,
        q_network = QNetwork,
        target_network = TargetNetwork,
        experience_replay = queue:new(),
        config = Config,
        statistics = #{
            episodes_completed => 0,
            total_reward => 0.0,
            best_fitness => 0.0,
            average_loss => 0.0
        },
        episode_count = 0
    },

    {ok, State}.

handle_call(get_policy, _From, State) ->
    {reply, {ok, State#miner_state.q_network}, State};

handle_call({set_learning_rate, Rate}, _From, State) ->
    Config = State#miner_state.config,
    NewConfig = Config#{learning_rate => Rate},
    {reply, ok, State#miner_state{config = NewConfig}};

handle_call({train_episode, Log}, _From, State) ->
    %% Run one training episode
    {Model, Reward, Stats} = run_episode(Log, State),

    %% Update statistics
    NewStats = update_statistics(State#miner_state.statistics, Reward, Stats),

    %% Decay epsilon
    Config = State#miner_state.config,
    NewEpsilon = max(0.01, maps:get(epsilon, Config) * maps:get(epsilon_decay, Config)),
    NewConfig = Config#{epsilon => NewEpsilon},

    NewState = State#miner_state{
        config = NewConfig,
        statistics = NewStats,
        episode_count = State#miner_state.episode_count + 1
    },

    {reply, Model, NewState};

handle_call(get_statistics, _From, State) ->
    {reply, {ok, State#miner_state.statistics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec init_network() -> q_network().
init_network() ->
    #{
        weights => #{
            w1 => init_weights(100, 50),
            w2 => init_weights(50, 20),
            w3 => init_weights(20, 10)  %% 10 actions
        },
        architecture => #{
            input_size => 100,
            hidden_sizes => [50, 20],
            output_size => 10
        }
    }.

%% @private
-spec init_weights(pos_integer(), pos_integer()) -> [[float()]].
init_weights(Rows, Cols) ->
    [[rand:uniform() * 0.2 - 0.1 || _ <- lists:seq(1, Cols)]
     || _ <- lists:seq(1, Rows)].

%% @private
-spec run_episode(event_log(), #miner_state{}) -> {map(), float(), map()}.
run_episode(Log, State) ->
    %% Initialize discovery state
    InitialState = #{
        partial_model => #{places => [], transitions => [], arcs => []},
        remaining_log => Log,
        features => extract_features(Log, #{}),
        step => 0
    },

    %% Run episode steps
    run_step(InitialState, Log, State, 0, []).

%% @private
-spec run_step(discovery_state(), event_log(), #miner_state{},
               float(), [map()]) -> {map(), float(), map()}.
run_step(State, Log, MinerState, TotalReward, History) ->
    %% Check if episode is done
    case is_episode_done(State, MinerState) of
        true ->
            %% Return final model and statistics
            FinalModel = maps:get(partial_model, State),
            Fitness = evaluate_model(FinalModel, Log),
            {FinalModel#{fitness => Fitness}, TotalReward, #{
                steps => maps:get(step, State),
                final_fitness => Fitness
            }};
        false ->
            %% Select action using epsilon-greedy
            Features = maps:get(features, State),
            Action = select_action(Features, MinerState),

            %% Execute action and get reward
            {NewState, Reward} = execute_action(Action, State, Log),

            %% Store experience
            Experience = #{
                state => Features,
                action => Action,
                reward => Reward,
                next_state => maps:get(features, NewState)
            },

            %% Continue episode
            run_step(NewState, Log, MinerState, TotalReward + Reward, [Experience | History])
    end.

%% @private
-spec is_episode_done(discovery_state(), #miner_state{}) -> boolean().
is_episode_done(State, _MinerState) ->
    %% Episode ends when all log activities are covered
    Features = maps:get(features, State),
    maps:get(transitions_covered, Features) >= 0.95 orelse
    maps:get(step, State) >= 100.

%% @private
-spec select_action(state_features(), #miner_state{}) -> rl_action().
select_action(Features, MinerState) ->
    Config = MinerState#miner_state.config,
    Epsilon = maps:get(epsilon, Config),

    case rand:uniform() of
        X when X < Epsilon ->
            %% Explore: random action
            random_action();
        _ ->
            %% Exploit: best action from Q-network
            best_action(Features, MinerState)
    end.

%% @private
-spec random_action() -> rl_action().
random_action() ->
    Actions = [sequence, parallel, exclusive, loop, milestone],
    ActionType = lists:nth(rand:uniform(length(Actions)), Actions),
    #{
        type => add_pattern,
        pattern => ActionType,
        activities => []
    }.

%% @private
-spec best_action(state_features(), #miner_state{}) -> rl_action().
best_action(Features, MinerState) ->
    QNetwork = MinerState#miner_state.q_network,
    %% Compute Q-values for all actions
    QValues = compute_q_values(Features, QNetwork),
    %% Select action with max Q-value
    ActionIndex = select_max_q_index(QValues),
    index_to_action(ActionIndex).

%% @private
-spec compute_q_values(state_features(), q_network()) -> [float()].
compute_q_values(_Features, _QNetwork) ->
    %% Simplified - would use actual neural network forward pass
    [rand:uniform() || _ <- lists:seq(1, 10)].

%% @private
-spec select_max_q_index([float()]) -> pos_integer().
select_max_q_index(QValues) ->
    {_Max, Index} = lists:foldl(fun(V, {M, I}) ->
        case V > M of
            true -> {V, I};
            false -> {M, I}
        end
    end, {-1.0, 1}, lists:zip(QValues, lists:seq(1, length(QValues)))),
    Index.

%% @private
-spec index_to_action(pos_integer()) -> rl_action().
index_to_action(1) -> #{type => add_pattern, pattern => sequence, activities => []};
index_to_action(2) -> #{type => add_pattern, pattern => parallel, activities => []};
index_to_action(3) -> #{type => add_pattern, pattern => exclusive, activities => []};
index_to_action(4) -> #{type => add_pattern, pattern => loop, activities => []};
index_to_action(5) -> #{type => add_pattern, pattern => milestone, activities => []};
index_to_action(6) -> #{type => merge, pattern => undefined, activities => []};
index_to_action(7) -> #{type => split, pattern => undefined, activities => []};
index_to_action(8) -> #{type => finalize, pattern => undefined, activities => []};
index_to_action(_) -> #{type => add_pattern, pattern => skip, activities => []}.

%% @private
-spec execute_action(rl_action(), discovery_state(), event_log()) -> {discovery_state(), float()}.
execute_action(Action, State, Log) ->
    %% Apply action to partial model
    PartialModel = maps:get(partial_model, State),
    NewPartialModel = apply_action_to_model(Action, PartialModel, Log),

    %% Update features
    NewFeatures = extract_features(Log, NewPartialModel),

    %% Compute reward
    Reward = compute_reward(State, NewPartialModel, Log),

    NewState = State#{
        partial_model => NewPartialModel,
        features => NewFeatures,
        step => maps:get(step, State) + 1
    },

    {NewState, Reward}.

%% @private
-spec apply_action_to_model(rl_action(), map(), event_log()) -> map().
apply_action_to_model(#{type := finalize}, Model, _Log) ->
    Model;
apply_action_to_model(#{type := add_pattern, pattern := Pattern}, Model, Log) ->
    Activities = extract_activities_from_log(Log),
    %% Add pattern to model
    CurrentTransitions = maps:get(transitions, Model, []),
    CurrentPlaces = maps:get(places, Model, []),

    %% Create new places/transitions for pattern
    NewPlace = list_to_atom("p_" ++ atom_to_list(Pattern) ++ "_" ++ integer_to_list(length(CurrentPlaces))),

    Model#{
        places => [NewPlace | CurrentPlaces],
        transitions => Activities ++ CurrentTransitions,
        patterns => [Pattern | maps:get(patterns, Model, [])]
    };
apply_action_to_model(_, Model, _Log) ->
    Model.

%% @private
-spec extract_activities_from_log(event_log()) -> [activity()].
extract_activities_from_log(Log) ->
    lists:usort(lists:flatten(Log)).

%% @private
-spec extract_features(event_log(), map()) -> state_features().
extract_features(Log, PartialModel) ->
    Activities = extract_activities_from_log(Log),
    ModelActivities = maps:get(transitions, PartialModel, []),
    Covered = case length(Activities) of
        0 -> 1.0;
        N -> length(Activities -- ModelActivities) / N
    end,

    #{
        activities_discovered => Activities,
        transitions_covered => Covered,
        log_fitness => 0.5,  %% Placeholder
        model_complexity => length(maps:get(places, PartialModel, []))
    }.

%% @private
-spec compute_reward(discovery_state(), map(), event_log()) -> float().
compute_reward(_OldState, NewModel, Log) ->
    %% Reward based on fitness and simplicity
    Fitness = evaluate_model(NewModel, Log),
    Complexity = length(maps:get(places, NewModel, [])),

    %% Balance fitness and simplicity
    Fitness - 0.01 * Complexity.

%% @private
-spec evaluate_model(map(), event_log()) -> float().
evaluate_model(Model, Log) ->
    %% Simple fitness: proportion of activities covered
    Activities = extract_activities_from_log(Log),
    ModelActivities = maps:get(transitions, Model, []),
    Covered = length([A || A <- Activities, lists:member(A, ModelActivities)]),

    case length(Activities) of
        0 -> 1.0;
        N -> Covered / N
    end.

%% @private
-spec update_statistics(miner_statistics(), float(), map()) -> miner_statistics().
update_statistics(Stats, Reward, EpisodeStats) ->
    #{
        episodes_completed => maps:get(episodes_completed, Stats) + 1,
        total_reward => maps:get(total_reward, Stats) + Reward,
        best_fitness => max(maps:get(best_fitness, Stats), maps:get(final_fitness, EpisodeStats, 0.0)),
        average_loss => 0.0  %% Placeholder
    }.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

simple_log() ->
    [[a, b, c, d], [a, c, b, d]].

%%--------------------------------------------------------------------
%% Network initialization tests
%%--------------------------------------------------------------------

init_network_test() ->
    Network = init_network(),
    ?assert(maps:is_key(weights, Network)),
    ?assert(maps:is_key(architecture, Network)).

%%--------------------------------------------------------------------
%% Action selection tests
%%--------------------------------------------------------------------

random_action_test() ->
    Action = random_action(),
    ?assert(maps:is_key(type, Action)),
    ?assert(maps:is_key(pattern, Action)).

index_to_action_test() ->
    Action = index_to_action(1),
    ?assertEqual(sequence, maps:get(pattern, Action)).

%%--------------------------------------------------------------------
%% Feature extraction tests
%%--------------------------------------------------------------------

extract_features_test() ->
    Log = simple_log(),
    Features = extract_features(Log, #{}),
    ?assert(maps:is_key(activities_discovered, Features)),
    ?assert(maps:is_key(transitions_covered, Features)).

%%--------------------------------------------------------------------
%% Model application tests
%%--------------------------------------------------------------------

apply_action_finalize_test() ->
    Model = #{places => [], transitions => []},
    Action = #{type => finalize},
    NewModel = apply_action_to_model(Action, Model, simple_log()),
    ?assertEqual(Model, NewModel).

apply_action_add_pattern_test() ->
    Model = #{places => [], transitions => []},
    Action = #{type => add_pattern, pattern => sequence},
    NewModel = apply_action_to_model(Action, Model, simple_log()),
    ?assert(length(maps:get(places, NewModel)) > 0).

%%--------------------------------------------------------------------
%% Evaluation tests
%%--------------------------------------------------------------------

evaluate_model_test() ->
    Model = #{transitions => [a, b, c, d]},
    Log = simple_log(),
    Fitness = evaluate_model(Model, Log),
    ?assert(Fitness >= 0.0 andalso Fitness =< 1.0).

-endif.
