%% -*- erlang -*-
%% @doc Test suite for reinforcement learning strategy modules

-module(rl_strategy_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
     ucb_new_select_record,
     ucb_first_pull_bias,
     ucb_exploration_decay,
     ucb_statistics_tracking,
     q_learning_initialization,
     q_learning_epsilon_greedy,
     q_learning_q_value_update,
     q_learning_table_get,
     contextual_initialization,
     contextual_prediction_no_model,
     contextual_model_update,
     thompson_init,
     thompson_select_branch,
     thompson_record_outcome,
     thompson_beta_convergence,
     first_n_init,
     first_n_completion_logic,
     first_n_result_collection,
     fastest_n_timing,
     fastest_n_result_sorting,
     quality_init_with_threshold,
     quality_min_filtering,
     quality_best_n_selection
    ].

init_per_suite(Config) ->
    %% Seed random for reproducible tests
    rand:seed(exs1024, {123, 456, 789}),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% UCB Strategy Tests
%%====================================================================

ucb_new_select_record(_Config) ->
    %% Test basic UCB1 state creation and arm selection
    State = strategy_ucb:new(5, #{}),
    %% Select first arm (should be valid)
    {ArmId1, _State1} = strategy_ucb:select_arm(State),
    ?assert(ArmId1 >= 1 andalso ArmId1 =< 5),

    %% Record results
    State1 = strategy_ucb:record_result(State, 1, 1.0),
    State2 = strategy_ucb:record_result(State1, 2, 0.5),

    %% Check statistics
    Stats = strategy_ucb:get_stats(State2),
    ?assertEqual(2, maps:get(total_pulls, Stats)),

    ok.

ucb_first_pull_bias(_Config) ->
    %% Test that unpulled arms get infinite UCB
    State = strategy_ucb:new(3, #{}),
    %% Pull arm 1 once
    State1 = strategy_ucb:record_result(State, 1, 1.0),
    %% Arm 2 should be selected (infinite UCB for unpulled arms)
    {2, _State2} = strategy_ucb:select_arm(State1),
    ok.

ucb_exploration_decay(_Config) ->
    %% Test UCB confidence bound decreases with more pulls
    State0 = strategy_ucb:new(2, #{c => 2.0}),
    %% Give many pulls to arm 1
    State1 = lists:foldl(fun(_, Acc) ->
        strategy_ucb:record_result(Acc, 1, 1.0)
    end, State0, lists:seq(1, 50)),

    %% Arm 2 should now have higher UCB due to exploration bonus
    {2, _State2} = strategy_ucb:select_arm(State1),
    ok.

ucb_statistics_tracking(_Config) ->
    %% Test statistics accumulation
    State0 = strategy_ucb:new(3, #{}),
    State1 = strategy_ucb:record_result(State0, 1, 1.0),
    State2 = strategy_ucb:record_result(State1, 1, 0.0),
    State3 = strategy_ucb:record_result(State2, 2, 1.0),

    Stats = strategy_ucb:get_stats(State3),
    ?assertEqual(3, maps:get(total_pulls, Stats)),
    Arm1Stats = maps:get(1, maps:get(arms, Stats)),
    ?assertEqual(2, maps:get(pulls, Arm1Stats)),
    ?assertEqual(0.5, maps:get(avg_reward, Arm1Stats)),
    ok.

%%====================================================================
%% Q-Learning Strategy Tests
%%====================================================================

q_learning_initialization(_Config) ->
    %% Test Q-learning strategy setup
    {ok, Pid} = strategy_q_learning:start_link(3, #{}),
    %% Get initial Q-table (empty)
    QTable = strategy_q_learning:get_q_table(Pid),
    ?assertEqual(0, map_size(QTable)),
    gen_server:stop(Pid),
    ok.

q_learning_epsilon_greedy(_Config) ->
    %% Test epsilon-greedy action selection
    {ok, Pid} = strategy_q_learning:start_link(3, #{epsilon => 0.0}),
    %% With epsilon=0, should always select argmax
    {ok, Action1} = strategy_q_learning:select_action(Pid, state1),
    %% All Q-values are 0 initially, should pick 1
    ?assertEqual(1, Action1),

    %% Set some Q-values
    ok = strategy_q_learning:update_q_value(Pid, state1, 2, 1.0, state1),
    {ok, Action2} = strategy_q_learning:select_action(Pid, state1),
    ?assertEqual(2, Action2),

    gen_server:stop(Pid),
    ok.

q_learning_q_value_update(_Config) ->
    %% Test Q-value update rule
    {ok, Pid} = strategy_q_learning:start_link(3, #{learning_rate => 0.5, discount_factor => 0.9}),
    State = state1,
    Action = 1,
    Reward = 1.0,
    NextState = state2,

    ok = strategy_q_learning:update_q_value(Pid, State, Action, Reward, NextState),

    QTable = strategy_q_learning:get_q_table(Pid),
    QValue = maps:get({State, Action}, QTable),
    ?assert(QValue > 0),

    gen_server:stop(Pid),
    ok.

q_learning_table_get(_Config) ->
    %% Test getting Q-table snapshot
    {ok, Pid} = strategy_q_learning:start_link(2, #{}),
    ok = strategy_q_learning:update_q_value(Pid, s1, 1, 0.5, s2),

    QTable = strategy_q_learning:get_q_table(Pid),
    ?assert(maps:is_key({s1, 1}, QTable)),

    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Contextual Strategy Tests
%%====================================================================

contextual_initialization(_Config) ->
    %% Test contextual bandit initialization
    {ok, Pid} = strategy_contextual:start_link(3, #{}),
    gen_server:stop(Pid),
    ok.

contextual_prediction_no_model(_Config) ->
    %% Test prediction when no model exists (uses fallback)
    {ok, Pid} = strategy_contextual:start_link(3, #{fallback_strategy => first_n}),
    Context = #{priority => 1.0},
    {ok, Branch} = strategy_contextual:predict_branch(Pid, Context),
    ?assert(Branch >= 1 andalso Branch =< 3),
    gen_server:stop(Pid),
    ok.

contextual_model_update(_Config) ->
    %% Test model update after prediction
    {ok, Pid} = strategy_contextual:start_link(3, #{}),
    Context = #{value => 1.0},

    %% First prediction creates model
    {ok, Branch1} = strategy_contextual:predict_branch(Pid, Context),
    %% Update model
    ok = strategy_contextual:update_model(Pid, Context, Branch1, 1.0),

    %% Subsequent prediction uses model
    {ok, Branch2} = strategy_contextual:predict_branch(Pid, Context),
    ?assert(Branch2 >= 1 andalso Branch2 =< 3),

    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Thompson Sampling Strategy Tests
%%====================================================================

thompson_init(_Config) ->
    %% Test Thompson sampling initialization
    {ok, State} = strategy_thompson_sampling:init(2, 5),
    ok.

thompson_select_branch(_Config) ->
    %% Test branch selection with Thompson sampling
    {ok, State} = strategy_thompson_sampling:init(1, 3),
    BranchId = strategy_thompson_sampling:select_branch(State),
    ?assert(BranchId >= 1 andalso BranchId =< 3),
    ok.

thompson_record_outcome(_Config) ->
    %% Test recording outcomes
    {ok, State} = strategy_thompson_sampling:init(1, 3),
    State1 = strategy_thompson_sampling:record_outcome(State, 1, success),
    State2 = strategy_thompson_sampling:record_outcome(State1, 2, failure),
    State3 = strategy_thompson_sampling:record_outcome(State2, 1, success),
    ok.

thompson_beta_convergence(_Config) ->
    %% Test that Beta distribution parameters update correctly
    {ok, State} = strategy_thompson_sampling:init(1, 2),

    %% Record many successes for arm 1
    State1 = lists:foldl(fun(_, Acc) ->
        strategy_thompson_sampling:record_outcome(Acc, 1, success)
    end, State, lists:seq(1, 20)),

    %% Record failures for arm 2
    State2 = lists:foldl(fun(_, Acc) ->
        strategy_thompson_sampling:record_outcome(Acc, 2, failure)
    end, State1, lists:seq(1, 20)),
    ok.

%%====================================================================
%% First N Strategy Tests
%%====================================================================

first_n_init(_Config) ->
    %% Test first-n strategy initialization
    {ok, State} = strategy_first_n:init(2, 5),
    ok.

first_n_completion_logic(_Config) ->
    %% Test completion condition
    {ok, State} = strategy_first_n:init(3, 5),
    ?assertEqual(false, strategy_first_n:should_complete(State, #{})),

    State1 = strategy_first_n:on_branch_complete(State, {1, result1}),
    State2 = strategy_first_n:on_branch_complete(State1, {2, result2}),
    ?assertEqual(false, strategy_first_n:should_complete(State2, #{})),

    State3 = strategy_first_n:on_branch_complete(State2, {3, result3}),
    ?assertEqual(true, strategy_first_n:should_complete(State3, #{})),
    ok.

first_n_result_collection(_Config) ->
    %% Test result collection
    {ok, State} = strategy_first_n:init(2, 3),
    State1 = strategy_first_n:on_branch_complete(State, {1, a}),
    State2 = strategy_first_n:on_branch_complete(State1, {2, b}),
    State3 = strategy_first_n:on_branch_complete(State2, {3, c}),

    {ok, Results} = strategy_first_n:get_result(State3),
    ?assertEqual(3, map_size(Results)),
    ?assertEqual(a, maps:get(1, Results)),
    ?assertEqual(b, maps:get(2, Results)),
    ?assertEqual(c, maps:get(3, Results)),
    ok.

%%====================================================================
%% Fastest N Strategy Tests
%%====================================================================

fastest_n_timing(_Config) ->
    %% Test timing tracking
    {ok, State} = strategy_fastest_n:init(2, 3),

    %% Simulate branch completions
    State1 = strategy_fastest_n:on_branch_complete(State, {1, slow}),
    timer:sleep(10),
    State2 = strategy_fastest_n:on_branch_complete(State1, {2, fast}),

    ok.

fastest_n_result_sorting(_Config) ->
    %% Test result sorting by completion time
    {ok, State} = strategy_fastest_n:init(2, 3),

    State1 = strategy_fastest_n:on_branch_complete(State, {1, first}),
    timer:sleep(10),
    State2 = strategy_fastest_n:on_branch_complete(State1, {2, second}),
    timer:sleep(10),
    State3 = strategy_fastest_n:on_branch_complete(State2, {3, third}),

    {ok, Results} = strategy_fastest_n:get_result(State3),
    ?assertEqual(2, length(Results)),
    %% First completed should be first in results
    {1, first} = lists:nth(1, Results),
    ok.

%%====================================================================
%% Quality Strategy Tests
%%====================================================================

quality_init_with_threshold(_Config) ->
    %% Test initialization with minimum quality
    {ok, State} = strategy_quality:init(2, 3, [{min_quality, 0.7}]),
    ok.

quality_min_filtering(_Config) ->
    %% Test minimum quality filtering
    {ok, State} = strategy_quality:init(2, 3, [{min_quality, 0.5}]),
    State1 = strategy_quality:on_branch_complete(State, {1, {0.3, low_quality}}),
    State2 = strategy_quality:on_branch_complete(State1, {2, {0.8, high_quality}}),

    %% Should not complete yet (only 1 acceptable result)
    ?assertEqual(false, strategy_quality:should_complete(State2, #{})),

    State3 = strategy_quality:on_branch_complete(State2, {3, {0.9, best_quality}}),
    %% Now should complete (2 acceptable results)
    ?assertEqual(true, strategy_quality:should_complete(State3, #{})),
    ok.

quality_best_n_selection(_Config) ->
    %% Test selecting best N results
    {ok, State} = strategy_quality:init(2, 4),
    State1 = strategy_quality:on_branch_complete(State, {1, {0.5, a}}),
    State2 = strategy_quality:on_branch_complete(State1, {2, {0.9, b}}),
    State3 = strategy_quality:on_branch_complete(State2, {3, {0.3, c}}),
    State4 = strategy_quality:on_branch_complete(State3, {4, {0.8, d}}),

    {ok, Results} = strategy_quality:get_result(State4),
    ?assertEqual(2, length(Results)),
    %% Should return highest quality results (b and d)
    ResultIds = [Id || {Id, _} <- Results],
    ?assert(lists:member(2, ResultIds)),
    ?assert(lists:member(4, ResultIds)),
    ok.
