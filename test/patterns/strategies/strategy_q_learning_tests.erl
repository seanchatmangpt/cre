%% -*- erlang -*-
%%%% @doc strategy_q_learning_tests - EUnit tests for Q-learning strategy.
%%
%% Tests for:
%% - gen_server lifecycle (start_link, stop)
%% - Action selection with epsilon-greedy
%% - Q-value updates and learning
%% - Parameter adjustment (epsilon, learning rate)
%% - Edge cases (empty Q-table, convergence)
%%
%% @end

-module(strategy_q_learning_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

-define(TEST_SERVER, q_learning_test_server).

%%====================================================================
%% gen_server Lifecycle Tests
%%====================================================================

strategy_q_learning_start_link_test() ->
    {ok, Pid} = strategy_q_learning:start_link(5, #{}),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, strategy_q_learning:stop(Pid)),
    ?assertNot(is_process_alive(Pid)).

strategy_q_learning_start_link_with_options_test() ->
    {ok, Pid} = strategy_q_learning:start_link(5, #{
        learning_rate => 0.2,
        discount_factor => 0.9,
        epsilon => 0.5
    }),
    ?assert(is_pid(Pid)),
    strategy_q_learning:stop(Pid).

strategy_q_learning_start_link_invalid_n_test() ->
    ?assertExit({badarg, _}, strategy_q_learning:start_link(0, #{})).

%%====================================================================
%% Action Selection Tests
%%====================================================================

strategy_q_learning_select_action_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{epsilon => 1.0}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              {ok, Action} = strategy_q_learning:select_action(Pid, state1),
              ?assert(Action >= 1 andalso Action =< 3)
           end)
         ]
     end}.

strategy_q_learning_select_action_exploitation_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{epsilon => 0.0}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Train to prefer action 2 in state1
              ok = strategy_q_learning:update_q_value(Pid, state1, 2, 1.0, state1),
              ok = strategy_q_learning:update_q_value(Pid, state1, 1, 0.0, state1),
              ok = strategy_q_learning:update_q_value(Pid, state1, 3, 0.0, state1),

              %% With epsilon=0, should exploit (choose action 2)
              {ok, Action} = strategy_q_learning:select_action(Pid, state1),
              ?assertEqual(2, Action)
           end)
         ]
     end}.

strategy_q_learning_select_action_exploration_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(5, #{epsilon => 1.0}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% With epsilon=1.0, should always explore (random)
              Actions = lists:map(fun(_) ->
                  {ok, A} = strategy_q_learning:select_action(Pid, state1),
                  A
              end, lists:seq(1, 50)),

              %% Should see variety in actions
              UniqueActions = lists:usort(Actions),
              ?assert(length(UniqueActions) > 1)
           end)
         ]
     end}.

%%====================================================================
%% Q-Value Update Tests
%%====================================================================

strategy_q_learning_update_q_value_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{learning_rate => 0.5}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Initial Q-value should be 0 (not in table)
              ok = strategy_q_learning:update_q_value(Pid, state1, 1, 1.0, state2),

              QTable = strategy_q_learning:get_q_table(Pid),
              ?assert(maps:is_key({state1, 1}, QTable)),

              QValue = maps:get({state1, 1}, QTable),
              ?assert(QValue > 0.0)  %% Should have learned something
           end)
         ]
     end}.

strategy_q_learning_update_multiple_transitions_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{
             learning_rate => 0.1,
             discount_factor => 0.9
         }),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Train multiple transitions
              ok = strategy_q_learning:update_q_value(Pid, state1, 1, 1.0, state2),
              ok = strategy_q_learning:update_q_value(Pid, state2, 2, 1.0, state3),
              ok = strategy_q_learning:update_q_value(Pid, state3, 3, 10.0, terminal),

              QTable = strategy_q_learning:get_q_table(Pid),

              ?assert(maps:is_key({state1, 1}, QTable)),
              ?assert(maps:is_key({state2, 2}, QTable)),
              ?assert(maps:is_key({state3, 3}, QTable))
           end)
         ]
     end}.

strategy_q_learning_learning_convergence_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(2, #{
             learning_rate => 0.5,
             discount_factor => 0.9,
             epsilon => 0.0
         }),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Simple 2-state, 2-action MDP
              %% state1 -> action1 -> state2 -> action1 -> reward 10
              lists:foreach(fun(_) ->
                  ok = strategy_q_learning:update_q_value(Pid, state2, 1, 10.0, terminal),
                  ok = strategy_q_learning:update_q_value(Pid, state1, 1, 0.0, state2)
              end, lists:seq(1, 20)),

              QTable = strategy_q_learning:get_q_table(Pid),
              Q1 = maps:get({state1, 1}, QTable),
              Q2 = maps:get({state2, 1}, QTable),

              %% Q(state1, 1) should converge toward Q(state2, 1) * gamma
              %% Q(state2, 1) should converge toward 10
              ?assert(Q2 > 5.0),  %% Learned high value
              ?assert(Q1 > 0.0)   %% Learned value propagates back
           end)
         ]
     end}.

%%====================================================================
%% Get Q-Table Tests
%%====================================================================

strategy_q_learning_get_q_table_empty_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              QTable = strategy_q_learning:get_q_table(Pid),
              ?assertEqual(#{}, QTable)
           end)
         ]
     end}.

strategy_q_learning_get_q_table_after_updates_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              ok = strategy_q_learning:update_q_value(Pid, state1, 1, 1.0, state2),
              ok = strategy_q_learning:update_q_value(Pid, state1, 2, -1.0, state2),

              QTable = strategy_q_learning:get_q_table(Pid),
              ?assertEqual(2, map_size(QTable))
           end)
         ]
     end}.

strategy_q_learning_get_q_table_structure_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              ok = strategy_q_learning:update_q_value(Pid, {complex, state}, 1, 0.5, next_state),

              QTable = strategy_q_learning:get_q_table(Pid),
              ?assert(is_map(QTable)),
              ?assert(maps:is_key({{complex, state}, 1}, QTable))
           end)
         ]
     end}.

%%====================================================================
%% Parameter Adjustment Tests
%%====================================================================

strategy_q_learning_set_epsilon_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{epsilon => 0.5}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              ?assertEqual(ok, strategy_q_learning:set_epsilon(Pid, 0.1)),
              ?assertEqual(ok, strategy_q_learning:set_epsilon(Pid, 0.0)),
              ?assertEqual(ok, strategy_q_learning:set_epsilon(Pid, 1.0))
           end),
          ?_test(begin
              %% Invalid values should fail
              ?assertError(_, strategy_q_learning:set_epsilon(Pid, -0.1)),
              ?assertError(_, strategy_q_learning:set_epsilon(Pid, 1.5))
           end)
         ]
     end}.

strategy_q_learning_set_learning_rate_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{learning_rate => 0.1}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              ?assertEqual(ok, strategy_q_learning:set_learning_rate(Pid, 0.5)),
              ?assertEqual(ok, strategy_q_learning:set_learning_rate(Pid, 0.01)),
              ?assertEqual(ok, strategy_q_learning:set_learning_rate(Pid, 1.0))
           end),
          ?_test(begin
              %% Invalid values should fail
              ?assertError(_, strategy_q_learning:set_learning_rate(Pid, 0.0)),
              ?assertError(_, strategy_q_learning:set_learning_rate(Pid, 1.5)),
              ?assertError(_, strategy_q_learning:set_learning_rate(Pid, -0.1))
           end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

strategy_q_learning_full_training_cycle_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(3, #{
             learning_rate => 0.1,
             discount_factor => 0.95,
             epsilon => 0.3
         }),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Simulate a simple episodic task
              %% Episode 1
              {ok, A1} = strategy_q_learning:select_action(Pid, start),
              ok = strategy_q_learning:update_q_value(Pid, start, A1, 0.0, middle),
              {ok, A2} = strategy_q_learning:select_action(Pid, middle),
              ok = strategy_q_learning:update_q_value(Pid, middle, A2, 1.0, terminal),

              %% Episode 2
              ok = strategy_q_learning:set_epsilon(Pid, 0.1),
              {ok, A3} = strategy_q_learning:select_action(Pid, start),
              ok = strategy_q_learning:update_q_value(Pid, start, A3, 0.0, middle),
              {ok, A4} = strategy_q_learning:select_action(Pid, middle),
              ok = strategy_q_learning:update_q_value(Pid, middle, A4, 1.0, terminal),

              %% Check that Q-values were updated
              QTable = strategy_q_learning:get_q_table(Pid),
              ?assert(map_size(QTable) > 0)
           end)
         ]
     end}.

strategy_q_learning_multiple_states_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(2, #{
             learning_rate => 0.1,
             discount_factor => 0.9,
             epsilon => 0.0
         }),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Train different policies for different states
              %% State A: action 1 is good
              lists:foreach(fun(_) ->
                  ok = strategy_q_learning:update_q_value(Pid, state_a, 1, 10.0, state_a),
                  ok = strategy_q_learning:update_q_value(Pid, state_a, 2, -10.0, state_a)
              end, lists:seq(1, 10)),

              %% State B: action 2 is good
              lists:foreach(fun(_) ->
                  ok = strategy_q_learning:update_q_value(Pid, state_b, 1, -10.0, state_b),
                  ok = strategy_q_learning:update_q_value(Pid, state_b, 2, 10.0, state_b)
              end, lists:seq(1, 10)),

              %% Check learned policy
              {ok, ActionA} = strategy_q_learning:select_action(Pid, state_a),
              {ok, ActionB} = strategy_q_learning:select_action(Pid, state_b),

              ?assertEqual(1, ActionA),
              ?assertEqual(2, ActionB)
           end)
         ]
     end}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

strategy_q_learning_different_state_types_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(2, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
    fun(Pid) ->
        [
         ?_test(begin
             %% Test with different state representations
             States = [
                 atom_state,
                 {tuple, state},
                 #{map => state},
                 <<"binary_state">>,
                 42
             ],

             lists:foreach(fun(State) ->
                 ok = strategy_q_learning:update_q_value(Pid, State, 1, 1.0, State),
                 {ok, Action} = strategy_q_learning:select_action(Pid, State),
                 ?assert(Action >= 1 andalso Action =< 2)
             end, States),

             QTable = strategy_q_learning:get_q_table(Pid),
             ?assertEqual(length(States), map_size(QTable))
          end)
        ]
    end}.

strategy_q_learning_negative_rewards_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_q_learning:start_link(2, #{
             learning_rate => 0.5,
             discount_factor => 0.9,
             epsilon => 0.0
         }),
         Pid
     end,
     fun(Pid) ->
         strategy_q_learning:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Action 1 gives negative reward, action 2 gives positive
              ok = strategy_q_learning:update_q_value(Pid, state, 1, -1.0, state),
              ok = strategy_q_learning:update_q_value(Pid, state, 2, 1.0, state),

              %% Should prefer action 2
              {ok, Action} = strategy_q_learning:select_action(Pid, state),
              ?assertEqual(2, Action)
           end)
         ]
     end}.
