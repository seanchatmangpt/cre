%% -*- erlang -*-
%%%% @doc strategy_ucb_tests - EUnit tests for UCB1 strategy.
%%
%% Tests for:
%% - Initialization with custom parameters
%% - Arm selection using UCB1 algorithm
%% - Result recording and statistics updates
%% - Exploration vs exploitation behavior
%% - Edge cases (uninitialized arms, equal values)
%%
%% @end

-module(strategy_ucb_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

-record(ucb_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    arms :: [tuple()],
    total_pulls = 0 :: non_neg_integer(),
    c = 1.41 :: float()
}).

%%====================================================================
%% Initialization Tests
%%====================================================================

strategy_ucb_new_default_test() ->
    State = strategy_ucb:new(5, #{}),
    ?assertEqual(5, State#ucb_state.n),
    ?assertEqual(1, State#ucb_state.m),
    ?assertEqual(0, State#ucb_state.total_pulls),
    ?assertEqual(1.41, State#ucb_state.c),
    ?assertEqual(5, length(State#ucb_state.arms)).

strategy_ucb_new_custom_c_test() ->
    State = strategy_ucb:new(5, #{c => 2.0}),
    ?assertEqual(2.0, State#ucb_state.c).

strategy_ucb_new_custom_m_test() ->
    State = strategy_ucb:new(5, #{m => 3}),
    ?assertEqual(3, State#ucb_state.m).

strategy_ucb_new_multiple_options_test() ->
    State = strategy_ucb:new(10, #{c => 1.5, m => 2}),
    ?assertEqual(10, State#ucb_state.n),
    ?assertEqual(2, State#ucb_state.m),
    ?assertEqual(1.5, State#ucb_state.c).

strategy_ucb_arm_initialization_test() ->
    State = strategy_ucb:new(3, #{}),
    #{
        arms := ArmsMap,
        total_pulls := TotalPulls
    } = strategy_ucb:get_stats(State),
    ?assertEqual(0, TotalPulls),
    ?assertEqual(3, map_size(ArmsMap)),
    %% All arms should have 0 pulls initially
    lists:foreach(fun(I) ->
        Arm = maps:get(I, ArmsMap),
        ?assertEqual(0, maps:get(pulls, Arm)),
        ?assertEqual(0.0, maps:get(avg_reward, Arm))
    end, lists:seq(1, 3)).

%%====================================================================
%% Select Arm Tests
%%====================================================================

strategy_ucb_select_arm_first_selection_test() ->
    State0 = strategy_ucb:new(5, #{}),
    %% First selection should explore (all arms have 0 pulls, UCB = infinity)
    {ArmId, _State1} = strategy_ucb:select_arm(State0),
    ?assert(ArmId >= 1 andalso ArmId =< 5).

strategy_ucb_select_arm_all_arms_test() ->
    State0 = strategy_ucb:new(3, #{}),
    %% Select multiple times - should eventually select all arms
    ArmIds = lists:map(fun(_) ->
        {ArmId, State} = strategy_ucb:select_arm(State0),
        State0 = State,
        ArmId
    end, lists:seq(1, 50)),
    %% All arms should be selected at least once due to exploration
    UniqueArms = lists:usort(ArmIds),
    ?assertEqual(3, length(UniqueArms)).

strategy_ucb_select_arm_with_rewards_test() ->
    State0 = strategy_ucb:new(3, #{}),
    %% Give arm 1 high rewards
    State1 = lists:foldl(fun(_, Acc) ->
        {_, S1} = strategy_ucb:select_arm(Acc),
        strategy_ucb:record_result(S1, 1, 1.0)
    end, State0, lists:seq(1, 10)),

    %% Give arm 2 low rewards
    State2 = lists:foldl(fun(_, Acc) ->
        {_, S1} = strategy_ucb:select_arm(Acc),
        strategy_ucb:record_result(S1, 2, 0.1)
    end, State1, lists:seq(1, 10)),

    %% Now selection should prefer arm 1 (exploitation)
    {ArmId, _} = strategy_ucb:select_arm(State2),
    ?assertEqual(1, ArmId).

%%====================================================================
%% Record Result Tests
%%====================================================================

strategy_ucb_record_result_first_pull_test() ->
    State0 = strategy_ucb:new(3, #{}),
    State1 = strategy_ucb:record_result(State0, 1, 0.5),
    Stats = strategy_ucb:get_stats(State1),
    ?assertEqual(1, maps:get(total_pulls, Stats)),

    Arm1Stats = maps:get(1, maps:get(arms, Stats)),
    ?assertEqual(1, maps:get(pulls, Arm1Stats)),
    ?assertEqual(0.5, maps:get(avg_reward, Arm1Stats)).

strategy_ucb_record_result_multiple_pulls_same_arm_test() ->
    State0 = strategy_ucb:new(3, #{}),
    State1 = lists:foldl(fun(_, Acc) ->
        strategy_ucb:record_result(Acc, 1, 1.0)
    end, State0, lists:seq(1, 5)),

    Arm1Stats = maps:get(1, maps:get(arms, strategy_ucb:get_stats(State1))),
    ?assertEqual(5, maps:get(pulls, Arm1Stats)),
    ?assertEqual(1.0, maps:get(avg_reward, Arm1Stats)).

strategy_ucb_record_result_different_rewards_test() ->
    State0 = strategy_ucb:new(3, #{}),
    State1 = strategy_ucb:record_result(State0, 1, 0.0),
    State2 = strategy_ucb:record_result(State1, 1, 1.0),
    State3 = strategy_ucb:record_result(State2, 1, 0.5),

    Arm1Stats = maps:get(1, maps:get(arms, strategy_ucb:get_stats(State3))),
    ?assertEqual(3, maps:get(pulls, Arm1Stats)),
    ?assertEqual(0.5, maps:get(avg_reward, Arm1Stats)).  %% (0+1+0.5)/3

strategy_ucb_record_result_different_arms_test() ->
    State0 = strategy_ucb:new(3, #{}),
    State1 = strategy_ucb:record_result(State0, 1, 1.0),
    State2 = strategy_ucb:record_result(State1, 2, 0.5),
    State3 = strategy_ucb:record_result(State2, 3, 0.0),

    Stats = strategy_ucb:get_stats(State3),
    ?assertEqual(3, maps:get(total_pulls, Stats)),
    ?assertEqual(1.0, maps:get(avg_reward, maps:get(1, maps:get(arms, Stats)))),
    ?assertEqual(0.5, maps:get(avg_reward, maps:get(2, maps:get(arms, Stats)))),
    ?assertEqual(0.0, maps:get(avg_reward, maps:get(3, maps:get(arms, Stats)))).

strategy_ucb_record_result_total_pulls_increment_test() ->
    State0 = strategy_ucb:new(5, #{}),
    State1 = lists:foldl(fun(I, Acc) ->
        strategy_ucb:record_result(Acc, I, 0.5)
    end, State0, lists:seq(1, 10)),

    Stats = strategy_ucb:get_stats(State1),
    ?assertEqual(10, maps:get(total_pulls, Stats)).

%%====================================================================
%% Get Stats Tests
%%====================================================================

strategy_ucb_get_stats_structure_test() ->
    State = strategy_ucb:new(3, #{}),
    Stats = strategy_ucb:get_stats(State),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(arms, Stats)),
    ?assert(maps:is_key(total_pulls, Stats)).

strategy_ucb_get_stats_after_updates_test() ->
    State0 = strategy_ucb:new(3, #{}),
    State1 = strategy_ucb:record_result(State0, 1, 0.8),
    State2 = strategy_ucb:record_result(State1, 1, 0.6),
    State3 = strategy_ucb:record_result(State2, 2, 0.4),

    Stats = strategy_ucb:get_stats(State3),
    ?assertEqual(3, maps:get(total_pulls, Stats)),

    ArmsMap = maps:get(arms, Stats),
    ?assertEqual(2, maps:get(pulls, maps:get(1, ArmsMap))),
    ?assertEqual(0.7, maps:get(avg_reward, maps:get(1, ArmsMap))),
    ?assertEqual(1, maps:get(pulls, maps:get(2, ArmsMap))),
    ?assertEqual(0.4, maps:get(avg_reward, maps:get(2, ArmsMap))).

%%====================================================================
%% Integration Tests
%%====================================================================

strategy_ucb_convergence_to_best_arm_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              %% Simulate a 3-armed bandit where arm 1 is best
              State0 = strategy_ucb:new(3, #{c => 1.0}),

              %% Training phase: give rewards based on arm quality
              State1 = train_ucb(State0, 100, fun(Arm) ->
                  case Arm of
                      1 -> 0.9;  %% Best arm
                      2 -> 0.5;  %% Medium arm
                      3 -> 0.1   %% Poor arm
                  end
              end),

              %% Test phase: select arms and verify preference for arm 1
              Selections = lists:map(fun(_) ->
                  {Arm, _S} = strategy_ucb:select_arm(State1),
                  Arm
              end, lists:seq(1, 20)),

              %% Count selections
              Counts = lists:foldl(fun(Arm, Acc) ->
                  maps:update_with(Arm, fun(V) -> V + 1 end, 1, Acc)
              end, #{}, Selections),

              %% Arm 1 should be selected most often
              Arm1Count = maps:get(1, Counts, 0),
              ?assert(Arm1Count > 10)  %% At least half of selections
           end)
         ]
     end}.

strategy_ucb_exploration_behavior_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              %% Test that UCB explores before exploiting
              State0 = strategy_ucb:new(5, #{c => 2.0}),  %% High exploration

              %% Make some selections with no prior knowledge
              {Arm1, State1} = strategy_ucb:select_arm(State0),
              State2 = strategy_ucb:record_result(State1, Arm1, 0.5),

              {Arm2, State3} = strategy_ucb:select_arm(State2),
              State4 = strategy_ucb:record_result(State3, Arm2, 0.5),

              {Arm3, _State5} = strategy_ucb:select_arm(State4),

              %% With high c, should explore different arms
              ?assertNotEqual(Arm1, Arm2)
           end)
         ]
     end}.

strategy_ucb_state_immutability_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              State0 = strategy_ucb:new(3, #{}),
              OriginalStats = strategy_ucb:get_stats(State0),

              State1 = strategy_ucb:record_result(State0, 1, 0.5),
              UpdatedStats = strategy_ucb:get_stats(State1),

              %% Original stats should be unchanged (immutability)
              ?assertEqual(0, maps:get(total_pulls, OriginalStats)),
              ?assertEqual(1, maps:get(total_pulls, UpdatedStats))
           end)
         ]
     end}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

strategy_ucb_extreme_rewards_test() ->
    State0 = strategy_ucb:new(2, #{}),
    State1 = strategy_ucb:record_result(State0, 1, 0.0),
    State2 = strategy_ucb:record_result(State1, 1, 1.0),
    State3 = strategy_ucb:record_result(State2, 2, 100.0),

    Stats = strategy_ucb:get_stats(State3),
    ?assertEqual(0.5, maps:get(avg_reward, maps:get(1, maps:get(arms, Stats)))),
    ?assertEqual(100.0, maps:get(avg_reward, maps:get(2, maps:get(arms, Stats)))).

strategy_ucb_negative_rewards_handling_test() ->
    State0 = strategy_ucb:new(2, #{}),
    %% UCB can handle negative rewards (though typically rewards are 0-1)
    State1 = strategy_ucb:record_result(State0, 1, -0.5),
    State2 = strategy_ucb:record_result(State1, 1, 0.5),

    Arm1Stats = maps:get(1, maps:get(arms, strategy_ucb:get_stats(State2))),
    ?assertEqual(0.0, maps:get(avg_reward, Arm1Stats)).

strategy_ucb_single_arm_test() ->
    State0 = strategy_ucb:new(1, #{}),
    {Arm, State1} = strategy_ucb:select_arm(State0),
    ?assertEqual(1, Arm),

    State2 = strategy_ucb:record_result(State1, 1, 0.7),
    {Arm2, _} = strategy_ucb:select_arm(State2),
    ?assertEqual(1, Arm2).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper to train UCB by simulating pulls and rewards
train_ucb(State, Iterations, RewardFunc) ->
    lists:foldl(fun(_, Acc) ->
        {Arm, S1} = strategy_ucb:select_arm(Acc),
        Reward = RewardFunc(Arm),
        strategy_ucb:record_result(S1, Arm, Reward)
    end, State, lists:seq(1, Iterations)).
