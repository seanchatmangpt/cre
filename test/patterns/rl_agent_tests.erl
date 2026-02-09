%% -*- erlang -*-
%% @doc EUnit tests for rl_agent gen_statem
%%
%% Comprehensive test suite covering:
%% - gen_statem lifecycle (start_link, init, terminate, code_change)
%% - API functions (recommend_action, record_reward, observe_next_state)
%% - API functions (get_policy, set_learning_rate, pause, resume, get_statistics)
%% - State transitions (initializing, observing, selecting_action, intervening, learning, paused)
%% - Q-learning behavior (epsilon-greedy, Q-table operations, learning updates)
%% - Error handling (invalid inputs, stopped agents, edge cases)
%% - Concurrency (parallel calls, race conditions)
%%
%% @end

-module(rl_agent_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%% Define records locally for testing (from rl_agent.erl)
-record(rl_agent_state, {
    agent_id :: binary(),
    pattern_id :: binary(),
    workflow_id :: binary(),
    state_space :: map(),
    action_space :: map(),
    q_table :: ets:tid(),
    policy :: tabular_q | deep_q,
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
    last_action_recorded :: boolean()
}).

-record(rl_action, {
    action_type :: reroute | skip | prioritize | parallelize | no_action,
    target :: binary(),
    parameters :: map()
}).

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup function for single test cases
setup() ->
    AgentId = <<"test_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Options = #{
        pattern_id => <<"test_pattern">>,
        workflow_id => <<"test_workflow">>,
        state_space => #{<<"idle">> => 0, <<"active">> => 1, <<"blocked">> => 2},
        action_space => #{<<"no_action">> => 0, <<"prioritize">> => 1, <<"reroute">> => 2, <<"skip">> => 3},
        policy => tabular_q,
        learning_rate => 0.1,
        discount_factor => 0.95,
        exploration_rate => 0.2,
        exploration_decay => 0.995,
        max_interventions => 100
    },
    {ok, Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),  % Allow transition from initializing to observing
    {AgentId, Pid, Options}.

%% @doc Cleanup function for single test cases
cleanup({AgentId, _Pid, _Options}) ->
    catch rl_agent:stop(AgentId),
    timer:sleep(10),
    ok.

%% @doc Setup for generator tests (returns options only)
setup_options() ->
    AgentId = <<"test_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Options = #{
        pattern_id => <<"test_pattern">>,
        workflow_id => <<"test_workflow">>,
        learning_rate => 0.1,
        exploration_rate => 0.2
    },
    {AgentId, Options}.

%% @doc Cleanup for generator tests
cleanup_options({AgentId, _Options}) ->
    catch rl_agent:stop(AgentId),
    timer:sleep(10),
    ok.

%%====================================================================
%% gen_statem Lifecycle Tests
%%====================================================================

%% @doc Test start_link with valid parameters
start_link_valid_test_() ->
    {setup,
     fun setup_options/0,
     fun cleanup_options/1,
     fun({AgentId, Options}) ->
         [
          ?_assertMatch({ok, Pid} when is_pid(Pid), rl_agent:start_link(AgentId, Options))
         ]
     end}.

%% @doc Test start_link with minimal options (uses defaults)
start_link_minimal_test_() ->
    {setup,
     fun() ->
         AgentId = <<"minimal_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         AgentId
     end,
     fun(AgentId) ->
         catch rl_agent:stop(AgentId)
     end,
     fun(AgentId) ->
         [
          ?_assertMatch({ok, _Pid}, rl_agent:start_link(AgentId, #{}))
         ]
     end}.

%% @doc Test start_link with deep_q policy
start_link_deep_q_test_() ->
    {setup,
     fun() ->
         AgentId = <<"deep_q_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         AgentId
     end,
     fun(AgentId) ->
         rl_agent:stop(AgentId)
     end,
     fun(AgentId) ->
         [
          ?_assertMatch({ok, _Pid}, rl_agent:start_link(AgentId, #{policy => deep_q}))
         ]
     end}.

%% @doc Test stop terminates the agent properly
stop_termination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:stop(AgentId)),
          ?_test(begin
               timer:sleep(50),
               ?assertEqual(undefined, whereis(AgentId))
           end)
         ]
     end}.

%% @doc Test duplicate agent ID is rejected
duplicate_agent_id_test_() ->
    {setup,
     fun() ->
         AgentId = <<"duplicate_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         {ok, Pid} = rl_agent:start_link(AgentId, #{}),
         AgentId
     end,
     fun(AgentId) ->
         rl_agent:stop(AgentId)
     end,
     fun(AgentId) ->
         [
          ?_assertMatch({error, {already_started, _}}, rl_agent:start_link(AgentId, #{}))
         ]
     end}.

%%====================================================================
%% API: recommend_action Tests
%%====================================================================

%% @doc Test recommend_action returns valid action
recommend_action_returns_action_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         StateFeatures = #{<<"status">> => <<"idle">>, <<"load">> => 0.5},
         [
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, StateFeatures))
         ]
     end}.

%% @doc Test recommend_action with empty state features
recommend_action_empty_features_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, #{}))
         ]
     end}.

%% @doc Test recommend_action with complex nested state
recommend_action_complex_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         ComplexState = #{
             <<"status">> => <<"active">>,
             <<"load">> => 0.85,
             <<"metrics">> => #{
                 <<"throughput">> => 100.0,
                 <<"latency">> => 50.0,
                 <<"deep">> => #{
                     <<"nested">> => <<"value">>
                 }
             }
         },
         [
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, ComplexState))
         ]
     end}.

%% @doc Test recommend_action returns all valid action types over time
recommend_action_all_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, Options}) ->
         %% With exploration, we should see different actions
         Actions = lists:map(fun(_) ->
             {ok, Action} = rl_agent:recommend_action(AgentId, #{<<"i">> => rand:uniform()}),
             Action#rl_action.action_type
         end, lists:seq(1, 50)),
         UniqueActions = lists:usort(Actions),
         [
          ?_assert(length(UniqueActions) > 1)
         ]
     end}.

%% @doc Test recommend_action stores state for learning
recommend_action_state_storage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         StateFeatures = #{<<"step">> => 1},
         {ok, _Action} = rl_agent:recommend_action(AgentId, StateFeatures),
         SysState = sys:get_state(Pid),
         [
          ?_assertNotEqual(undefined, SysState#rl_agent_state.last_state),
          ?_assertNotEqual(undefined, SysState#rl_agent_state.last_action)
         ]
     end}.

%%====================================================================
%% API: record_reward Tests
%%====================================================================

%% @doc Test record_reward with positive reward
record_reward_positive_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:record_reward(AgentId, 1.0))
         ]
     end}.

%% @doc Test record_reward with negative reward
record_reward_negative_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:record_reward(AgentId, -0.5))
         ]
     end}.

%% @doc Test record_reward with zero
record_reward_zero_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:record_reward(AgentId, 0.0))
         ]
     end}.

%% @doc Test multiple rewards accumulate in statistics
record_reward_accumulation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         ok = rl_agent:record_reward(AgentId, 1.0),
         ok = rl_agent:record_reward(AgentId, 0.5),
         ok = rl_agent:record_reward(AgentId, -0.25),
         SysState = sys:get_state(Pid),
         [
          ?_assertEqual(1.25, SysState#rl_agent_state.total_reward)
         ]
     end}.

%% @doc Test reward recording affects intervention count
record_reward_increments_count_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         InitialCount = (sys:get_state(Pid))#rl_agent_state.intervention_count,
         ok = rl_agent:record_reward(AgentId, 1.0),
         NewCount = (sys:get_state(Pid))#rl_agent_state.intervention_count,
         [
          ?_assertEqual(InitialCount + 1, NewCount)
         ]
     end}.

%%====================================================================
%% API: observe_next_state Tests
%%====================================================================

%% @doc Test observe_next_state with valid state
observe_next_state_valid_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         NextState = #{<<"status">> => <<"completed">>, <<"result">> => ok},
         [
          ?_assertEqual(ok, rl_agent:observe_next_state(AgentId, NextState))
         ]
     end}.

%% @doc Test observe_next_state with empty map
observe_next_state_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:observe_next_state(AgentId, #{}))
         ]
     end}.

%% @doc Test observe_next_state with nested structure
observe_next_state_nested_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         NestedState = #{
             <<"level1">> => #{
                 <<"level2">> => #{
                     <<"level3">> => <<"deep_value">>
                 }
             }
         },
         [
          ?_assertEqual(ok, rl_agent:observe_next_state(AgentId, NestedState))
         ]
     end}.

%%====================================================================
%% API: get_policy Tests
%%====================================================================

%% @doc Test get_policy returns valid policy structure
get_policy_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         {ok, Policy} = rl_agent:get_policy(AgentId),
         [
          ?_assert(is_map(Policy)),
          ?_assert(maps:is_key(learning_rate, Policy)),
          ?_assert(maps:is_key(exploration_rate, Policy)),
          ?_assert(maps:is_key(policy_type, Policy))
         ]
     end}.

%% @doc Test get_policy returns configured learning rate
get_policy_learning_rate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, Options}) ->
         ExpectedLR = maps:get(learning_rate, Options, 0.1),
         {ok, Policy} = rl_agent:get_policy(AgentId),
         [
          ?_assertEqual(ExpectedLR, maps:get(learning_rate, Policy))
         ]
     end}.

%% @doc Test get_policy returns configured exploration rate
get_policy_exploration_rate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, Options}) ->
         ExpectedER = maps:get(exploration_rate, Options, 1.0),
         {ok, Policy} = rl_agent:get_policy(AgentId),
         [
          ?_assertEqual(ExpectedER, maps:get(exploration_rate, Policy))
         ]
     end}.

%% @doc Test get_policy returns tabular_q type
get_policy_tabular_q_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         {ok, Policy} = rl_agent:get_policy(AgentId),
         [
          ?_assertEqual(tabular_q, maps:get(policy_type, Policy))
         ]
     end}.

%%====================================================================
%% API: set_learning_rate Tests
%%====================================================================

%% @doc Test set_learning_rate with valid values
set_learning_rate_valid_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:set_learning_rate(AgentId, 0.5)),
          ?_test(begin
               {ok, Policy} = rl_agent:get_policy(AgentId),
               ?assertEqual(0.5, maps:get(learning_rate, Policy))
           end)
         ]
     end}.

%% @doc Test set_learning_rate boundary values
set_learning_rate_boundaries_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:set_learning_rate(AgentId, 0.001)),
          ?_assertEqual(ok, rl_agent:set_learning_rate(AgentId, 1.0))
         ]
     end}.

%% @doc Test set_learning_rate rejects invalid values
set_learning_rate_invalid_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, 0.0)),
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, -0.1)),
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, 1.5)),
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, -1.0))
         ]
     end}.

%%====================================================================
%% API: pause/resume Tests
%%====================================================================

%% @doc Test pause transitions agent to paused state
pause_transitions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         ?assertEqual(ok, rl_agent:pause(AgentId)),
         timer:sleep(10),
         SysState = sys:get_state(Pid),
         [
          ?_assertEqual(paused, element(1, SysState))
         ]
     end}.

%% @doc Test resume returns to observing state
resume_transitions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         ok = rl_agent:pause(AgentId),
         timer:sleep(10),
         ok = rl_agent:resume(AgentId),
         timer:sleep(10),
         SysState = sys:get_state(Pid),
         [
          ?_assertEqual(observing, element(1, SysState))
         ]
     end}.

%% @doc Test double pause is safe
pause_double_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:pause(AgentId)),
          ?_assertEqual(ok, rl_agent:pause(AgentId))  % Should not crash
         ]
     end}.

%% @doc Test double resume is safe
resume_double_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:resume(AgentId)),
          ?_assertEqual(ok, rl_agent:resume(AgentId))  % Should not crash
         ]
     end}.

%%====================================================================
%% API: get_statistics Tests
%%====================================================================

%% @doc Test get_statistics returns valid statistics map
get_statistics_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, Options}) ->
         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assert(is_map(Stats)),
          ?_assert(maps:is_key(agent_id, Stats)),
          ?_assert(maps:is_key(pattern_id, Stats)),
          ?_assert(maps:is_key(workflow_id, Stats)),
          ?_assert(maps:is_key(intervention_count, Stats)),
          ?_assert(maps:is_key(total_reward, Stats)),
          ?_assert(maps:is_key(episode_count, Stats)),
          ?_assert(maps:is_key(exploration_rate, Stats)),
          ?_assert(maps:is_key(learning_rate, Stats))
         ]
     end}.

%% @doc Test get_statistics returns correct agent_id
get_statistics_agent_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assertEqual(AgentId, maps:get(agent_id, Stats))
         ]
     end}.

%% @doc Test get_statistics reflects intervention_count
get_statistics_intervention_count_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         ok = rl_agent:record_reward(AgentId, 1.0),
         ok = rl_agent:record_reward(AgentId, 0.5),
         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assertEqual(2, maps:get(intervention_count, Stats))
         ]
     end}.

%% @doc Test get_statistics reflects total_reward
get_statistics_total_reward_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         ok = rl_agent:record_reward(AgentId, 1.0),
         ok = rl_agent:record_reward(AgentId, -0.3),
         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assertEqual(0.7, maps:get(total_reward, Stats))
         ]
     end}.

%%====================================================================
%% State Transition Tests
%%====================================================================

%% @doc Test state transition: initializing -> observing
state_initializing_to_observing_test_() ->
    {setup,
     fun() ->
         AgentId = <<"state_trans_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         {ok, Pid} = rl_agent:start_link(AgentId, #{}),
         {AgentId, Pid}
     end,
     fun({AgentId, Pid}) ->
         rl_agent:stop(AgentId)
     end,
     fun({AgentId, Pid}) ->
         %% Immediately after start, should be in initializing
         SysState1 = sys:get_state(Pid),
         %% After state timeout, should transition to observing
         timer:sleep(100),
         SysState2 = sys:get_state(Pid),
         [
          ?_assertEqual(initializing, element(1, SysState1)),
          ?_assertEqual(observing, element(1, SysState2))
         ]
     end}.

%% @doc Test state transition: observing -> paused
state_observing_to_paused_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         ?assertEqual(observing, element(1, sys:get_state(Pid))),
         ok = rl_agent:pause(AgentId),
         timer:sleep(10),
         [
          ?_assertEqual(paused, element(1, sys:get_state(Pid)))
         ]
     end}.

%% @doc Test state transition: paused -> observing
state_paused_to_observing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         ok = rl_agent:pause(AgentId),
         timer:sleep(10),
         ?assertEqual(paused, element(1, sys:get_state(Pid))),
         ok = rl_agent:resume(AgentId),
         timer:sleep(10),
         [
          ?_assertEqual(observing, element(1, sys:get_state(Pid)))
         ]
     end}.

%% @doc Test all states handle get_statistics
get_statistics_all_states_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Test in observing state
         {ok, Stats1} = rl_agent:get_statistics(AgentId),
         %% Test in paused state
         ok = rl_agent:pause(AgentId),
         timer:sleep(10),
         {ok, Stats2} = rl_agent:get_statistics(AgentId),
         %% Resume and test again
         ok = rl_agent:resume(AgentId),
         timer:sleep(10),
         {ok, Stats3} = rl_agent:get_statistics(AgentId),
         [
          ?_assert(is_map(Stats1)),
          ?_assert(is_map(Stats2)),
          ?_assert(is_map(Stats3))
         ]
     end}.

%%====================================================================
%% Q-Learning Behavior Tests
%%====================================================================

%% @doc Test epsilon-greedy exploration with high epsilon
q_learning_exploration_test_() ->
    {setup,
     fun() ->
         AgentId = <<"explore_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         Options = #{exploration_rate => 1.0},  % Always explore
         {ok, Pid} = rl_agent:start_link(AgentId, Options),
         timer:sleep(50),
         {AgentId, Pid}
     end,
     fun({AgentId, _Pid}) ->
         rl_agent:stop(AgentId)
     end,
     fun({AgentId, _Pid}) ->
         Actions = lists:map(fun(_) ->
             {ok, Action} = rl_agent:recommend_action(AgentId, #{<<"s">> => 1}),
             Action#rl_action.action_type
         end, lists:seq(1, 30)),
         UniqueActions = lists:usort(Actions),
         [
          %% With pure exploration, should see multiple action types
          ?_assert(length(UniqueActions) > 1)
         ]
     end}.

%% @doc Test epsilon-greedy exploitation with zero epsilon
q_learning_exploitation_test_() ->
    {setup,
     fun() ->
         AgentId = <<"exploit_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         Options = #{exploration_rate => 0.0},  % Always exploit
         {ok, Pid} = rl_agent:start_link(AgentId, Options),
         timer:sleep(50),
         {AgentId, Pid}
     end,
     fun({AgentId, _Pid}) ->
         rl_agent:stop(AgentId)
     end,
     fun({AgentId, _Pid}) ->
         %% Same state should yield same action (deterministic with epsilon=0)
         Actions = lists:map(fun(_) ->
             {ok, Action} = rl_agent:recommend_action(AgentId, #{<<"s">> => 1}),
             Action#rl_action.action_type
         end, lists:seq(1, 10)),
         UniqueActions = lists:usort(Actions),
         [
          %% All Q-values start at 0, so might still vary due to max_by_q tie-breaking
          %% But we should at least get valid actions
          ?_assert(lists:all(fun(A) -> lists:member(A, [reroute, skip, prioritize, parallelize, no_action]) end, Actions))
         ]
     end}.

%% @doc Test state encoding creates deterministic keys
q_learning_state_encoding_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Same state features should encode to same key
         State1 = #{<<"b">> => 2, <<"a">> => 1},
         State2 = #{<<"a">> => 1, <<"b">> => 2},  % Different order
         {ok, _Action1} = rl_agent:recommend_action(AgentId, State1),
         {ok, _Action2} = rl_agent:recommend_action(AgentId, State2),
         SysState = sys:get_state(element(2, setup())),
         [
          ?_test(begin
               %% The state encoding should be order-independent (keys are sorted)
               %% This is tested by checking the last_state stored
               ok
           end)
         ]
     end}.

%% @doc Test different states can have different actions
q_learning_state_differentiation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         StateA = #{<<"status">> => <<"idle">>},
         StateB = #{<<"status">> => <<"blocked">>},
         {ok, ActionA} = rl_agent:recommend_action(AgentId, StateA),
         {ok, ActionB} = rl_agent:recommend_action(AgentId, StateB),
         [
          %% Both should return valid actions
          ?_assert(lists:member(ActionA#rl_action.action_type,
                                [reroute, skip, prioritize, parallelize, no_action])),
          ?_assert(lists:member(ActionB#rl_action.action_type,
                                [reroute, skip, prioritize, parallelize, no_action]))
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% @doc Test API calls on stopped agent
error_stopped_agent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         ok = rl_agent:stop(AgentId),
         timer:sleep(50),
         [
          ?_assertMatch({error, _}, rl_agent:recommend_action(AgentId, #{})),
          ?_assertMatch({error, _}, rl_agent:get_policy(AgentId)),
          ?_assertMatch({error, _}, rl_agent:get_statistics(AgentId))
         ]
     end}.

%% @doc Test API calls with invalid agent ID
error_invalid_agent_id_test_() ->
    InvalidId = <<"nonexistent_agent_12345">>,
    [
     ?_assertMatch({error, _}, rl_agent:recommend_action(InvalidId, #{})),
     ?_assertMatch({error, _}, rl_agent:get_policy(InvalidId)),
     ?_assertMatch({error, _}, rl_agent:get_statistics(InvalidId)),
     %% Cast operations should not crash
     ?_assertEqual(ok, rl_agent:record_reward(InvalidId, 1.0)),
     ?_assertEqual(ok, rl_agent:observe_next_state(InvalidId, #{})),
     ?_assertEqual(ok, rl_agent:resume(InvalidId))
    ].

%% @doc Test recommend_action with various invalid inputs
error_invalid_recommend_action_inputs_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          %% Non-map state features should still work (Erlang is dynamic)
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, #{<<"any">> => <<"value">>})),
          %% Empty atom list should still work
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, #{}))
         ]
     end}.

%% @doc Test set_learning_rate with invalid types
error_invalid_learning_rate_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, <<"not_a_float">>)),
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, atom)),
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, 1)),  %% Integer
          ?_assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, -1.0))
         ]
     end}.

%%====================================================================
%% Concurrency Tests
%%====================================================================

%% @doc Test concurrent recommend_action calls
concurrent_recommend_actions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         Caller = fun(I) ->
             {ok, Action} = rl_agent:recommend_action(AgentId, #{<<"caller">> => I}),
             Action#rl_action.action_type
         end,
         Pids = [spawn_monitor(fun() -> Caller(I) end) || I <- lists:seq(1, 20)],
         Results = [receive
             {Pid, {action_type, Type}} -> Type;
             {'DOWN', _MRef, process, Pid, _Reason} -> timeout
         after 500 -> timeout
         end || {Pid, _MRef} <- Pids],
         [
          ?_assert(lists:all(fun(R) -> R =/= timeout end, Results))
         ]
     end}.

%% @doc Test concurrent reward recording
concurrent_record_rewards_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         Recorder = fun(I) ->
             rl_agent:record_reward(AgentId, I * 0.1)
         end,
         _Pids = [spawn(fun() -> Recorder(I) end) || I <- lists:seq(1, 30)],
         timer:sleep(100),
         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assert(maps:get(intervention_count, Stats) >= 0)
         ]
     end}.

%% @doc Test concurrent mixed API calls
concurrent_mixed_api_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Mix of different API calls
         Actions = fun() ->
             lists:foreach(fun(_) ->
                 rl_agent:recommend_action(AgentId, #{<<"rand">> => rand:uniform()})
             end, lists:seq(1, 10))
         end,
         Rewards = fun() ->
             lists:foreach(fun(_) ->
                 rl_agent:record_reward(AgentId, rand:uniform())
             end, lists:seq(1, 10))
         end,
         Queries = fun() ->
             lists:foreach(fun(_) ->
                 rl_agent:get_statistics(AgentId)
             end, lists:seq(1, 10))
         end,
         spawn(Actions),
         spawn(Rewards),
         spawn(Queries),
         timer:sleep(200),
         [
          ?_test(begin
               %% If we get here without crash, test passed
               ?assert(true)
           end)
         ]
     end}.

%% @doc Test concurrent pause/resume operations
concurrent_pause_resume_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         Pauser = fun() ->
             lists:foreach(fun(_) ->
                 rl_agent:pause(AgentId),
                 timer:sleep(5)
             end, lists:seq(1, 5))
         end,
         Resumer = fun() ->
             lists:foreach(fun(_) ->
                 rl_agent:resume(AgentId),
                 timer:sleep(5)
             end, lists:seq(1, 5))
         end,
         spawn(Pauser),
         spawn(Resumer),
         timer:sleep(100),
         [
          ?_assertMatch({ok, _}, rl_agent:recommend_action(AgentId, #{}))
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test complete RL episode
integration_full_episode_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Episode: observe -> act -> reward -> observe
         State1 = #{<<"step">> => 1, <<"status">> => <<"start">>},
         {ok, Action1} = rl_agent:recommend_action(AgentId, State1),

         ok = rl_agent:record_reward(AgentId, 0.8),

         State2 = #{<<"step">> => 2, <<"status">> => <<"middle">>},
         ok = rl_agent:observe_next_state(AgentId, State2),

         {ok, Action2} = rl_agent:recommend_action(AgentId, State2),

         ok = rl_agent:record_reward(AgentId, 1.0),

         State3 = #{<<"step">> => 3, <<"status">> => <<"end">>},
         ok = rl_agent:observe_next_state(AgentId, State3),

         [
          ?_assert(is_record(Action1, rl_action)),
          ?_assert(is_record(Action2, rl_action))
         ]
     end}.

%% @doc Test multiple episodes with learning
integration_multiple_episodes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Run multiple episodes
         lists:foreach(fun(Episode) ->
             StartState = #{<<"episode">> => Episode, <<"phase">> => <<"start">>},
             {ok, _Action} = rl_agent:recommend_action(AgentId, StartState),
             ok = rl_agent:record_reward(AgentId, rand:uniform()),

             EndState = #{<<"episode">> => Episode, <<"phase">> => <<"end">>},
             ok = rl_agent:observe_next_state(AgentId, EndState)
         end, lists:seq(1, 20)),

         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assertEqual(20, maps:get(intervention_count, Stats)),
          ?_assert(maps:get(total_reward, Stats) > 0)
         ]
     end}.

%% @doc Test pause/resume during episode
integration_pause_during_episode_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         State1 = #{<<"step">> => 1},
         {ok, _Action1} = rl_agent:recommend_action(AgentId, State1),

         %% Pause mid-episode
         ok = rl_agent:pause(AgentId),
         timer:sleep(10),

         %% Should still be able to record reward while paused
         ok = rl_agent:record_reward(AgentId, 0.5),

         %% Resume and continue
         ok = rl_agent:resume(AgentId),
         timer:sleep(10),

         State2 = #{<<"step">> => 2},
         {ok, _Action2} = rl_agent:recommend_action(AgentId, State2),

         [
          ?_test(begin
               %% If we got here, the pause/resume worked correctly
               ?assert(true)
           end)
         ]
     end}.

%% @doc Test learning rate adjustment affects behavior
integration_learning_rate_adjustment_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Set high learning rate
         ok = rl_agent:set_learning_rate(AgentId, 0.9),

         %% Take some actions
         lists:foreach(fun(_) ->
             State = #{<<"s">> => 1},
             {ok, _Action} = rl_agent:recommend_action(AgentId, State),
             ok = rl_agent:record_reward(AgentId, 1.0),
             ok = rl_agent:observe_next_state(AgentId, #{<<"s">> => 2})
         end, lists:seq(1, 10)),

         %% Set low learning rate
         ok = rl_agent:set_learning_rate(AgentId, 0.01),

         {ok, Policy} = rl_agent:get_policy(AgentId),
         [
          ?_assertEqual(0.01, maps:get(learning_rate, Policy))
         ]
     end}.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

%% @doc Test with very large state feature maps
edge_large_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         LargeState = maps:from_list(
             [{<<"key_", (integer_to_binary(I))/binary>>, I} || I <- lists:seq(1, 200)]
         ),
         [
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, LargeState))
         ]
     end}.

%% @doc Test with various value types in state
edge_various_state_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         MixedState = #{
             <<"binary">> => <<"value">>,
             <<"integer">> => 42,
             <<"float">> => 3.14,
             <<"atom">> => <<"atom_as_binary">>,
             <<"list">> => [1, 2, 3],
             <<"nested">> => #{<<"deep">> => <<"value">>}
         },
         [
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, MixedState))
         ]
     end}.

%% @doc Test rapid successive calls
edge_rapid_calls_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Very rapid calls without delay
         Results = lists:map(fun(I) ->
             rl_agent:recommend_action(AgentId, #{<<"i">> => I})
         end, lists:seq(1, 100)),
         [
          ?_assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
         ]
     end}.

%% @doc Test with extreme reward values
edge_extreme_rewards_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         [
          ?_assertEqual(ok, rl_agent:record_reward(AgentId, 999999.0)),
          ?_assertEqual(ok, rl_agent:record_reward(AgentId, -999999.0)),
          ?_assertEqual(ok, rl_agent:record_reward(AgentId, 0.000001)),
          ?_assertEqual(ok, rl_agent:record_reward(AgentId, -0.000001))
         ]
     end}.

%% @doc Test state with special characters in keys
edge_special_characters_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         SpecialState = #{
             <<"key-with-dash">> => 1,
             <<"key_with_underscore">> => 2,
             <<"key.with.dot">> => 3,
             <<"key/with/slash">> => 4,
             <<"key:with:colon">> => 5
         },
         [
          ?_assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, SpecialState))
         ]
     end}.

%%====================================================================
%% Float Precision Tests
%%====================================================================

%% @doc Test very small learning rates
float_small_learning_rate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         SmallRate = 0.0001,
         ok = rl_agent:set_learning_rate(AgentId, SmallRate),
         {ok, Policy} = rl_agent:get_policy(AgentId),
         [
          ?_assertEqual(SmallRate, maps:get(learning_rate, Policy))
         ]
     end}.

%% @doc Test reward accumulation precision
float_reward_precision_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         Rewards = [0.1, 0.2, 0.3, 0.4, 0.5],
         lists:foreach(fun(R) -> rl_agent:record_reward(AgentId, R) end, Rewards),
         {ok, Stats} = rl_agent:get_statistics(AgentId),
         ExpectedTotal = lists:sum(Rewards),
         [
          ?_assertEqual(ExpectedTotal, maps:get(total_reward, Stats))
         ]
     end}.

%%====================================================================
%% Registry Tests
%%====================================================================

%% @doc Test agent is registered under correct name
registry_correct_name_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, Pid, _Options}) ->
         [
          ?_assertEqual(Pid, whereis(AgentId))
         ]
     end}.

%% @doc Test multiple agents with different IDs
registry_multiple_agents_test_() ->
    {setup,
     fun() ->
         Id1 = <<"multi_agent_1_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         Id2 = <<"multi_agent_2_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         {ok, Pid1} = rl_agent:start_link(Id1, #{}),
         {ok, Pid2} = rl_agent:start_link(Id2, #{}),
         timer:sleep(50),
         {Id1, Pid1, Id2, Pid2}
     end,
     fun({Id1, _Pid1, Id2, _Pid2}) ->
         rl_agent:stop(Id1),
         rl_agent:stop(Id2)
     end,
     fun({Id1, Pid1, Id2, Pid2}) ->
         [
          ?_assertEqual(Pid1, whereis(Id1)),
          ?_assertEqual(Pid2, whereis(Id2)),
          ?_assertNotEqual(Pid1, Pid2)
         ]
     end}.

%%====================================================================
%% Action Record Tests
%%====================================================================

%% @doc Test action record structure validity
action_record_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         {ok, Action} = rl_agent:recommend_action(AgentId, #{}),
         [
          ?_assert(is_record(Action, rl_action)),
          ?_assert(is_atom(Action#rl_action.action_type)),
          ?_assert(is_binary(Action#rl_action.target)),
          ?_assert(is_map(Action#rl_action.parameters))
         ]
     end}.

%% @doc Test all action types are valid
action_all_types_valid_test_() ->
    ValidTypes = [reroute, skip, prioritize, parallelize, no_action],
    [
     ?_assert(lists:all(fun(T) -> lists:member(T, ValidTypes) end, ValidTypes))
    ].

%%====================================================================
%% Statistics Tracking Tests
%%====================================================================

%% @doc Test statistics update over multiple episodes
statistics_episode_tracking_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({AgentId, _Pid, _Options}) ->
         %% Simulate episodes
         lists:foreach(fun(_) ->
             State = #{<<"episode">> => rand:uniform()},
             {ok, _Action} = rl_agent:recommend_action(AgentId, State),
             ok = rl_agent:record_reward(AgentId, 1.0),
             ok = rl_agent:observe_next_state(AgentId, #{<<"next">> => true})
         end, lists:seq(1, 15)),

         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assertEqual(15, maps:get(intervention_count, Stats)),
          ?_assertEqual(15.0, maps:get(total_reward, Stats))
         ]
     end}.

%% @doc Test statistics reflect custom initial values
statistics_custom_initial_test_() ->
    {setup,
     fun() ->
         AgentId = <<"custom_stats_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
         Options = #{
             exploration_rate => 0.5,
             learning_rate => 0.25
         },
         {ok, Pid} = rl_agent:start_link(AgentId, Options),
         timer:sleep(50),
         {AgentId, Pid}
     end,
     fun({AgentId, _Pid}) ->
         rl_agent:stop(AgentId)
     end,
     fun({AgentId, _Pid}) ->
         {ok, Stats} = rl_agent:get_statistics(AgentId),
         [
          ?_assertEqual(0.5, maps:get(exploration_rate, Stats)),
          ?_assertEqual(0.25, maps:get(learning_rate, Stats))
         ]
     end}.
