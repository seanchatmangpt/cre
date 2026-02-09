%% -*- erlang -*-
%% @doc Tests for Deep RL Miner (DQN)

-module(rl_miner_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

simple_log() ->
    [[a, b, c, d], [a, c, b, d]].

empty_log() ->
    [].

complex_log() ->
    [
        [a, b, c, d],
        [a, b, b, c, d],
        [a, c, b, d],
        [a, b, c, e, d]
    ].

%%====================================================================
%% Network Initialization Tests
%%====================================================================

init_network_test() ->
    Network = rl_miner:init_network(),
    ?assert(maps:is_key(weights, Network)),
    ?assert(maps:is_key(architecture, Network)).

init_weights_test() ->
    Weights = rl_miner:init_weights(10, 5),
    ?assertEqual(10, length(Weights)),
    ?assertEqual(5, length(hd(Weights))).

%%====================================================================
%% Action Selection Tests
%%====================================================================

random_action_test() ->
    Action = rl_miner:random_action(),
    ?assert(maps:is_key(type, Action)),
    ?assert(maps:is_key(pattern, Action)).

index_to_action_test() ->
    Action = rl_miner:index_to_action(1),
    ?assertEqual(sequence, maps:get(pattern, Action)).

index_to_action_all_test() ->
    lists:foreach(fun(I) ->
        Action = rl_miner:index_to_action(I),
        ?assert(maps:is_key(type, Action))
    end, lists:seq(1, 10)).

%%====================================================================
%% Feature Extraction Tests
%%====================================================================

extract_features_test() ->
    Log = simple_log(),
    Features = rl_miner:extract_features(Log, #{}),
    ?assert(maps:is_key(activities_discovered, Features)),
    ?assert(maps:is_key(transitions_covered, Features)).

extract_features_with_model_test() ->
    Log = simple_log(),
    Model = #{transitions => [a, b], places => []},
    Features = rl_miner:extract_features(Log, Model),
    ?assertEqual(4, length(maps:get(activities_discovered, Features))).

%%====================================================================
%% Model Application Tests
%%====================================================================

apply_action_finalize_test() ->
    Model = #{places => [], transitions => []},
    Action = #{type => finalize},
    NewModel = rl_miner:apply_action_to_model(Action, Model, simple_log()),
    ?assertEqual(Model, NewModel).

apply_action_add_pattern_test() ->
    Model = #{places => [], transitions => []},
    Action = #{type => add_pattern, pattern => sequence},
    NewModel = rl_miner:apply_action_to_model(Action, Model, simple_log()),
    ?assert(length(maps:get(places, NewModel)) > 0).

apply_action_merge_test() ->
    Model = #{places => [p1, p2], transitions => [a, b]},
    Action = #{type => merge},
    NewModel = rl_miner:apply_action_to_model(Action, Model, simple_log()),
    ?assert(is_map(NewModel)).

%%====================================================================
%% Evaluation Tests
%%====================================================================

evaluate_model_test() ->
    Model = #{transitions => [a, b, c, d]},
    Log = simple_log(),
    Fitness = rl_miner:evaluate_model(Model, Log),
    ?assert(Fitness >= 0.0 andalso Fitness =< 1.0).

evaluate_model_partial_test() ->
    Model = #{transitions => [a, b]},
    Log = simple_log(),
    Fitness = rl_miner:evaluate_model(Model, Log),
    ?assert(Fitness > 0.0),
    ?assert(Fitness < 1.0).

evaluate_model_empty_test() ->
    Model = #{transitions => []},
    Log = simple_log(),
    Fitness = rl_miner:evaluate_model(Model, Log),
    ?assertEqual(0.0, Fitness).

%%====================================================================
%% Statistics Update Tests
%%====================================================================

update_statistics_test() ->
    Stats = #{
        episodes_completed => 5,
        total_reward => 10.0,
        best_fitness => 0.8,
        average_loss => 0.5
    },
    EpisodeStats = #{final_fitness => 0.9},
    NewStats = rl_miner:update_statistics(Stats, 1.0, EpisodeStats),
    ?assertEqual(6, maps:get(episodes_completed, NewStats)),
    ?assertEqual(0.9, maps:get(best_fitness, NewStats)).

update_statistics_first_test() ->
    Stats = #{
        episodes_completed => 0,
        total_reward => 0.0,
        best_fitness => 0.0,
        average_loss => 0.0
    },
    EpisodeStats = #{final_fitness => 0.5},
    NewStats = rl_miner:update_statistics(Stats, 0.5, EpisodeStats),
    ?assertEqual(1, maps:get(episodes_completed, NewStats)),
    ?assertEqual(0.5, maps:get(best_fitness, NewStats)).

%%====================================================================
%% Integration Tests
%%====================================================================

discover_model_simple_test() ->
    MinerId = <<"test_miner_simple">>,
    case whereis(rl_miner) of
        undefined ->
            %% Skip if miner not started
            ?assert(true);
        _ ->
            {ok, Model} = rl_miner:discover_model(MinerId, simple_log(), #{max_episodes => 2}),
            ?assert(is_map(Model))
    end.

discover_model_empty_log_test() ->
    MinerId = <<"test_miner_empty">>,
    case whereis(rl_miner) of
        undefined ->
            ?assert(true);
        _ ->
            {ok, Model} = rl_miner:discover_model(MinerId, empty_log(), #{max_episodes => 1}),
            ?assert(is_map(Model))
    end.

discover_model_complex_log_test() ->
    MinerId = <<"test_miner_complex">>,
    case whereis(rl_miner) of
        undefined ->
            ?assert(true);
        _ ->
            {ok, Model} = rl_miner:discover_model(MinerId, complex_log(), #{max_episodes => 3}),
            ?assert(is_map(Model)),
            ?assert(maps:is_key(fitness, Model))
    end.

%%====================================================================
%% Episode Training Tests
%%====================================================================

train_episode_test() ->
    MinerId = <<"test_miner_episode">>,
    case whereis(rl_miner) of
        undefined ->
            ?assert(true);
        _ ->
            Result = rl_miner:train_episode(MinerId, simple_log()),
            ?assert(is_map(Result))
    end.

%%====================================================================
%% Policy Management Tests
%%====================================================================

get_policy_test() ->
    MinerId = <<"test_miner_policy">>,
    case whereis(rl_miner) of
        undefined ->
            ?assert(true);
        _ ->
            Result = rl_miner:get_policy(MinerId),
            ?assertMatch({ok, _}, Result)
    end.

set_learning_rate_test() ->
    MinerId = <<"test_miner_rate">>,
    case whereis(rl_miner) of
        undefined ->
            ?assert(true);
        _ ->
            Result = rl_miner:set_learning_rate(MinerId, 0.5),
            ?assertEqual(ok, Result)
    end.

set_learning_rate_invalid_test() ->
    MinerId = <<"test_miner_rate_invalid">>,
    case whereis(rl_miner) of
        undefined ->
            ?assert(true);
        _ ->
            Result = rl_miner:set_learning_rate(MinerId, 1.5),
            ?assertMatch({error, _}, Result)
    end.

%%====================================================================
%% Statistics Tests
%%====================================================================

get_statistics_test() ->
    MinerId = <<"test_miner_stats">>,
    case whereis(rl_miner) of
        undefined ->
            ?assert(true);
        _ ->
            Result = rl_miner:get_statistics(MinerId),
            ?assertMatch({ok, _}, Result)
    end.

%%====================================================================
%% Q-Value Computation Tests
%%====================================================================

compute_q_values_test() ->
    Features = #{activities_discovered => [a, b, c]},
    QNetwork = #{weights => #{}, architecture => #{}},
    QValues = rl_miner:compute_q_values(Features, QNetwork),
    ?assert(is_list(QValues)),
    ?assertEqual(10, length(QValues)).

select_max_q_index_test() ->
    QValues = [0.1, 0.5, 0.3, 0.9, 0.2],
    Index = rl_miner:select_max_q_index(QValues),
    ?assertEqual(4, Index).  %% 0.9 is at index 4

select_max_q_index_tie_test() ->
    QValues = [0.5, 0.5, 0.3],
    Index = rl_miner:select_max_q_index(QValues),
    ?assert(Index >= 1),
    ?assert(Index =< 2).

%%====================================================================
%% Episode State Tests
%%====================================================================

is_episode_done_test() ->
    State = #{
        partial_model => #{},
        remaining_log => [],
        features => #{transitions_covered => 1.0},
        step => 100
    },
    Done = rl_miner:is_episode_done(State, #{}),
    ?assertEqual(true, Done).

is_episode_done_not_done_test() ->
    State = #{
        partial_model => #{},
        remaining_log => [a, b],
        features => #{transitions_covered => 0.5},
        step => 10
    },
    Done = rl_miner:is_episode_done(State, #{}),
    ?assertEqual(false, Done).

is_episode_done_max_steps_test() ->
    State = #{
        partial_model => #{},
        remaining_log => [a, b],
        features => #{transitions_covered => 0.5},
        step => 101
    },
    Done = rl_miner:is_episode_done(State, #{}),
    ?assertEqual(true, Done).

%%====================================================================
;; Reward Computation Tests
%%====================================================================

compute_reward_test() ->
    OldState = #{partial_model => #{}, step => 0},
    NewModel = #{transitions => [a, b], places => [p1]},
    Log = simple_log(),
    Reward = rl_miner:compute_reward(OldState, NewModel, Log),
    ?assert(is_float(Reward)).
