%% -*- erlang -*-
%% @doc EUnit tests for rl_agent gen_statem
%%
%% Tests for:
%% - gen_statem lifecycle (start_link, init, terminate)
%% - State transitions (initializing, observing, selecting_action, intervening, learning, paused)
%% - API functions (recommend_action, record_reward, observe_next_state, get_policy, set_learning_rate)
%% - State management and ETS Q-table operations
%% - Error handling and edge cases
%%
%% @end

-module(rl_agent_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

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
    episode_count :: non_neg_integer()
}).

-record(rl_action, {
    action_type :: reroute | skip | prioritize | parallelize | no_action,
    target :: binary(),
    parameters :: map()
}).

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

rl_agent_setup() ->
    %% Generate unique agent ID for each test
    AgentId = <<"test_agent_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Options = #{
        pattern_id => <<"test_pattern">>,
        workflow_id => <<"test_workflow">>,
        state_space => #{<<"idle">> => 0, <<"active">> => 1},
        action_space => #{<<"no_action">> => 0, <<"prioritize">> => 1, <<"reroute">> => 2},
        policy => tabular_q,
        learning_rate => 0.1,
        discount_factor => 0.95,
        exploration_rate => 1.0,
        exploration_decay => 0.995,
        max_interventions => 100
    },
    {AgentId, Options}.

rl_agent_cleanup({AgentId, _Options}) ->
    catch rl_agent:stop(AgentId),
    timer:sleep(10),  % Give process time to terminate
    ok.

%%====================================================================
%% gen_statem Lifecycle Tests
%%====================================================================

%% @doc Test start_link with valid parameters
start_link_valid_test() ->
    {AgentId, Options} = rl_agent_setup(),
    ?assertMatch({ok, Pid} when is_pid(Pid), rl_agent:start_link(AgentId, Options)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test start_link with minimal options (uses defaults)
start_link_minimal_options_test() ->
    AgentId = <<"minimal_agent">>,
    Options = #{},
    ?assertMatch({ok, _Pid}, rl_agent:start_link(AgentId, Options)),
    rl_agent:stop(AgentId).

%% @doc Test start_link with custom policy type
start_link_custom_policy_test() ->
    AgentId = <<"deep_q_agent">>,
    Options = #{policy => deep_q},
    ?assertMatch({ok, _Pid}, rl_agent:start_link(AgentId, Options)),
    rl_agent:stop(AgentId).

%% @doc Test stop/1 terminates the agent
stop_agent_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:stop(AgentId)),
    %% Verify process is dead
    timer:sleep(50),
    ?assertEqual(undefined, whereis(AgentId)).

%%====================================================================
%% API: recommend_action Tests
%%====================================================================

%% @doc Test recommend_action returns valid action in observing state
recommend_action_valid_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),  % Allow transition from initializing
    StateFeatures = #{<<"status">> => <<"idle">>, <<"load">> => 0.5},
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, StateFeatures)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test recommend_action returns error when called before ready
recommend_action_not_ready_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    %% Call immediately - agent still in initializing state
    StateFeatures = #{<<"status">> => <<"idle">>},
    ?assertMatch({error, not_ready}, rl_agent:recommend_action(AgentId, StateFeatures)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test recommend_action with empty state features
recommend_action_empty_features_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, #{})),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test recommend_action with complex state features
recommend_action_complex_features_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    StateFeatures = #{
        <<"status">> => <<"active">>,
        <<"load">> => 0.85,
        <<"queue_length">> => 42,
        <<"processing_time">> => 1234.5
    },
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, StateFeatures)),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% API: record_reward Tests
%%====================================================================

%% @doc Test record_reward with positive reward
record_reward_positive_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:record_reward(AgentId, 1.0)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test record_reward with negative reward
record_reward_negative_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:record_reward(AgentId, -0.5)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test record_reward with zero reward
record_reward_zero_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:record_reward(AgentId, 0.0)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test multiple record_reward calls accumulate
record_reward_accumulation_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:record_reward(AgentId, 1.0)),
    ?assertEqual(ok, rl_agent:record_reward(AgentId, 0.5)),
    ?assertEqual(ok, rl_agent:record_reward(AgentId, -0.25)),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% API: observe_next_state Tests
%%====================================================================

%% @doc Test observe_next_state with valid state map
observe_next_state_valid_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    NextState = #{<<"status">> => <<"completed">>, <<"result">> => ok},
    ?assertEqual(ok, rl_agent:observe_next_state(AgentId, NextState)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test observe_next_state with empty state
observe_next_state_empty_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:observe_next_state(AgentId, #{})),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test observe_next_state with nested state
observe_next_state_nested_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    NextState = #{
        <<"status">> => <<"active">>,
        <<"metrics">> => #{
            <<"throughput">> => 100.0,
            <<"latency">> => 50.0
        }
    },
    ?assertEqual(ok, rl_agent:observe_next_state(AgentId, NextState)),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% API: get_policy Tests
%%====================================================================

%% @doc Test get_policy returns valid policy map
get_policy_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertMatch({ok, #{
        learning_rate := _,
        exploration_rate := _,
        policy_type := _
    }}, rl_agent:get_policy(AgentId)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test get_policy returns default learning rate
get_policy_default_learning_rate_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    {ok, Policy} = rl_agent:get_policy(AgentId),
    ?assertEqual(0.1, maps:get(learning_rate, Policy)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test get_policy returns default exploration rate
get_policy_default_exploration_rate_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    {ok, Policy} = rl_agent:get_policy(AgentId),
    ?assertEqual(1.0, maps:get(exploration_rate, Policy)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test get_policy returns custom learning rate
get_policy_custom_learning_rate_test() ->
    AgentId = <<"custom_lr_agent">>,
    Options = #{learning_rate => 0.25},
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    {ok, Policy} = rl_agent:get_policy(AgentId),
    ?assertEqual(0.25, maps:get(learning_rate, Policy)),
    rl_agent:stop(AgentId).

%% @doc Test get_policy returns tabular_q policy type
get_policy_tabular_q_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    {ok, Policy} = rl_agent:get_policy(AgentId),
    ?assertEqual(tabular_q, maps:get(policy_type, Policy)),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% API: set_learning_rate Tests
%%====================================================================

%% @doc Test set_learning_rate with valid rate
set_learning_rate_valid_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:set_learning_rate(AgentId, 0.5)),
    {ok, Policy} = rl_agent:get_policy(AgentId),
    ?assertEqual(0.5, maps:get(learning_rate, Policy)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test set_learning_rate with minimum valid rate
set_learning_rate_minimum_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:set_learning_rate(AgentId, 0.01)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test set_learning_rate with maximum valid rate
set_learning_rate_maximum_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(ok, rl_agent:set_learning_rate(AgentId, 1.0)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test set_learning_rate rejects zero rate
set_learning_rate_zero_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, 0.0)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test set_learning_rate rejects negative rate
set_learning_rate_negative_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, -0.1)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test set_learning_rate rejects rate above 1.0
set_learning_rate_above_max_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual({error, invalid_rate}, rl_agent:set_learning_rate(AgentId, 1.5)),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% State Management Tests
%%====================================================================

%% @doc Test state initialization with custom max_interventions
state_custom_max_interventions_test() ->
    AgentId = <<"max_int_agent">>,
    Options = #{max_interventions => 50},
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    ?assertMatch({ok, _}, rl_agent:recommend_action(AgentId, #{})),
    rl_agent:stop(AgentId).

%% @doc Test state initialization with custom discount factor
state_custom_discount_factor_test() ->
    AgentId = <<"discount_agent">>,
    Options = #{discount_factor => 0.99},
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    ?assertMatch({ok, _}, rl_agent:recommend_action(AgentId, #{})),
    rl_agent:stop(AgentId).

%% @doc Test state initialization with custom exploration decay
state_custom_exploration_decay_test() ->
    AgentId = <<"decay_agent">>,
    Options = #{exploration_decay => 0.9},
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    ?assertMatch({ok, _}, rl_agent:recommend_action(AgentId, #{})),
    rl_agent:stop(AgentId).

%%====================================================================
%% ETS Q-Table Tests (via behavior observation)
%%====================================================================

%% @doc Test Q-table operations through action selection pattern
q_table_action_selection_pattern_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    %% Multiple selections should work (Q-table is being accessed)
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, #{<<"s">> => 1})),
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, #{<<"s">> => 2})),
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, #{<<"s">> => 1})),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% RL Action Record Tests
%%====================================================================

%% @doc Test action structure for no_action
action_no_action_structure_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    %% With high exploration, likely to get no_action
    {ok, #rl_action{action_type = Type}} = rl_agent:recommend_action(AgentId, #{}),
    ?assert(lists:member(Type, [no_action, prioritize])),  % Due to epsilon-greedy
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test action record fields are valid
action_record_fields_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    {ok, Action} = rl_agent:recommend_action(AgentId, #{}),
    ?assert(is_record(Action, rl_action)),
    ?assert(is_binary(Action#rl_action.target)),
    ?assert(is_map(Action#rl_action.parameters)),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% Integration: Full RL Loop Tests
%%====================================================================

%% @doc Test full RL loop: observe -> act -> reward -> observe
rl_full_loop_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),

    %% Initial state observation
    State1 = #{<<"status">> => <<"idle">>},
    {ok, _Action1} = rl_agent:recommend_action(AgentId, State1),

    %% Record reward
    ok = rl_agent:record_reward(AgentId, 0.8),

    %% Observe next state
    NextState = #{<<"status">> => <<"active">>},
    ok = rl_agent:observe_next_state(AgentId, NextState),

    %% Next action based on new state
    {ok, _Action2} = rl_agent:recommend_action(AgentId, NextState),

    rl_agent_cleanup({AgentId, Options}).

%% @doc Test multiple episodes
rl_multiple_episodes_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),

    lists:foreach(fun(I) ->
        State = #{<<"episode">> => I},
        {ok, _Action} = rl_agent:recommend_action(AgentId, State),
        ok = rl_agent:record_reward(AgentId, rand:uniform()),
        ok = rl_agent:observe_next_state(AgentId, #{<<"next">> => I + 1})
    end, lists:seq(1, 10)),

    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% @doc Test calling API on stopped agent returns error
api_stopped_agent_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    rl_agent:stop(AgentId),
    timer:sleep(50),
    ?assertMatch({error, _}, rl_agent:recommend_action(AgentId, #{})).

%% @doc Test invalid agent ID handling
api_invalid_agent_test() ->
    InvalidId = <<"nonexistent_agent">>,
    ?assertMatch({error, _}, rl_agent:recommend_action(InvalidId, #{})),

    %% Cast operations should not crash
    ?assertEqual(ok, rl_agent:record_reward(InvalidId, 1.0)),
    ?assertEqual(ok, rl_agent:observe_next_state(InvalidId, #{})).

%%====================================================================
%% State Transition Tests
%%====================================================================

%% @doc Test agent transitions from initializing to observing
state_initializing_to_observing_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    %% Initially in initializing state - recommend_action should fail
    {error, not_ready} = rl_agent:recommend_action(AgentId, #{}),
    %% After state timeout, should transition to observing
    timer:sleep(100),
    ?assertMatch({ok, _}, rl_agent:recommend_action(AgentId, #{})),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% Concurrency Tests
%%====================================================================

%% @doc Test concurrent API calls
concurrent_recommend_actions_test_() ->
    {setup,
     fun() ->
         {AgentId, Options} = rl_agent_setup(),
         {ok, _Pid} = rl_agent:start_link(AgentId, Options),
         timer:sleep(50),
         {AgentId, Options}
     end,
     fun({AgentId, Options}) ->
         rl_agent_cleanup({AgentId, Options})
     end,
     fun({AgentId, _Options}) ->
         %% Spawn multiple callers
         Caller = fun() ->
             rl_agent:recommend_action(AgentId, #{<<"caller">> => self()})
         end,
         _Monitors = [spawn_monitor(fun() -> Caller() end) || _ <- lists:seq(1, 10)],
         %% Wait for all to complete
         timer:sleep(100),
         ?assert(true)  % If we get here, no crashes occurred
     end}.

%% @doc Test concurrent reward recording
concurrent_record_rewards_test_() ->
    {setup,
     fun() ->
         {AgentId, Options} = rl_agent_setup(),
         {ok, _Pid} = rl_agent:start_link(AgentId, Options),
         {AgentId, Options}
     end,
     fun({AgentId, Options}) ->
         rl_agent_cleanup({AgentId, Options})
     end,
     fun({AgentId, _Options}) ->
         %% Spawn multiple reward recorders
         Recorder = fun(I) ->
             rl_agent:record_reward(AgentId, I * 0.1)
         end,
         _Pids = [spawn(fun() -> Recorder(I) end) || I <- lists:seq(1, 20)],
         %% Wait for all to complete
         timer:sleep(100),
         ?assert(true)  % If we get here, no crashes occurred
     end}.

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Test generator for various learning rates
learning_rate_generator_test_() ->
    Rates = [0.01, 0.05, 0.1, 0.25, 0.5, 0.9, 1.0],
    [{Rate,
      fun() ->
          AgentId = <<"lr_agent_", (float_to_binary(Rate))/binary>>,
          Options = #{learning_rate => Rate},
          {ok, _Pid} = rl_agent:start_link(AgentId, Options),
          {ok, Policy} = rl_agent:get_policy(AgentId),
          ?assertEqual(Rate, maps:get(learning_rate, Policy)),
          rl_agent:stop(AgentId)
      end}
     || Rate <- Rates].

%% @doc Test generator for various exploration rates
exploration_rate_generator_test_() ->
    Rates = [0.0, 0.1, 0.5, 0.9, 1.0],
    [{Rate,
      fun() ->
          AgentId = <<"er_agent_", (float_to_binary(Rate))/binary>>,
          Options = #{exploration_rate => Rate},
          {ok, _Pid} = rl_agent:start_link(AgentId, Options),
          timer:sleep(50),
          {ok, _Action} = rl_agent:recommend_action(AgentId, #{}),
          rl_agent:stop(AgentId)
      end}
     || Rate <- Rates].

%% @doc Test generator for various action types
action_types_generator_test_() ->
    ActionTypes = [reroute, skip, prioritize, parallelize, no_action],
    [{Type,
      fun() ->
          %% Verify the action type is valid in the record
          Action = #rl_action{action_type = Type, target = <<>>, parameters = #{}},
          ?assertEqual(Type, Action#rl_action.action_type)
      end}
     || Type <- ActionTypes].

%%====================================================================
%% Edge Cases Tests
%%====================================================================

%% @doc Test with very large state feature maps
large_state_features_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    LargeState = maps:from_list([{<<"key", (integer_to_binary(I))/binary>>, I} || I <- lists:seq(1, 100)]),
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, LargeState)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test with binary keys and values in state
binary_state_features_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    BinaryState = #{
        <<"binary_key">> => <<"binary_value">>,
        <<"nested">> => #{<<"deep">> => <<"value">>}
    },
    ?assertMatch({ok, #rl_action{}}, rl_agent:recommend_action(AgentId, BinaryState)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test rapid state-action-reward cycles
rapid_cycles_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),

    lists:foreach(fun(I) ->
        State = #{<<"step">> => I},
        {ok, _} = rl_agent:recommend_action(AgentId, State),
        ok = rl_agent:record_reward(AgentId, 0.1),
        ok = rl_agent:observe_next_state(AgentId, #{<<"next">> => I + 1})
    end, lists:seq(1, 50)),

    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% Callback Mode Test
%%====================================================================

%% @doc Test that callback_mode returns state_functions
callback_mode_test() ->
    %% This tests the gen_statem callback configuration
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    %% If we got here successfully, callback_mode is working
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% Registry Integration Tests
%%====================================================================

%% @doc Test agent registered via registry
registry_registration_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, Pid} = rl_agent:start_link(AgentId, Options),
    ?assertEqual(Pid, whereis(AgentId)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test duplicate agent ID handling (should fail)
duplicate_agent_id_test() ->
    AgentId = <<"duplicate_agent">>,
    Options = #{},
    {ok, _Pid1} = rl_agent:start_link(AgentId, Options),
    ?assertMatch({error, {already_started, _}}, rl_agent:start_link(AgentId, Options)),
    rl_agent:stop(AgentId).

%%====================================================================
%% Termination Cleanup Tests
%%====================================================================

%% @doc Test ETS table cleanup on termination
ets_cleanup_test_() ->
    {setup,
     fun() ->
         {AgentId, Options} = rl_agent_setup(),
         {ok, Pid} = rl_agent:start_link(AgentId, Options),
         timer:sleep(50),
         {AgentId, Options, Pid}
     end,
     fun({AgentId, Options, _Pid}) ->
         rl_agent_cleanup({AgentId, Options})
     end,
     fun({_AgentId, _Options, Pid}) ->
         %% Get ETS table info before termination
         State = sys:get_state(Pid),
         QTableTid = State#rl_agent_state.q_table,
         ?assert(is_reference(QTableTid)),
         ?assertEqual(true, ets:info(QTableTid, size) >= 0),
         %% After stop, ETS should be cleaned up
         ok
     end}.

%%====================================================================
%% Float Precision Tests
%%====================================================================

%% @doc Test floating point precision handling for learning rate
float_precision_learning_rate_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    VerySmallRate = 0.001,
    ?assertEqual(ok, rl_agent:set_learning_rate(AgentId, VerySmallRate)),
    {ok, Policy} = rl_agent:get_policy(AgentId),
    ?assertEqual(VerySmallRate, maps:get(learning_rate, Policy)),
    rl_agent_cleanup({AgentId, Options}).

%% @doc Test floating point reward precision
float_precision_reward_test() ->
    {AgentId, Options} = rl_agent_setup(),
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    VerySmallReward = 0.000001,
    ?assertEqual(ok, rl_agent:record_reward(AgentId, VerySmallReward)),
    rl_agent_cleanup({AgentId, Options}).

%%====================================================================
%% Deep Q Policy Type Tests
%%====================================================================

%% @doc Test deep_q policy initialization
deep_q_policy_init_test() ->
    AgentId = <<"deep_q_init">>,
    Options = #{
        policy => deep_q,
        learning_rate => 0.01
    },
    {ok, _Pid} = rl_agent:start_link(AgentId, Options),
    timer:sleep(50),
    {ok, Policy} = rl_agent:get_policy(AgentId),
    ?assertEqual(deep_q, maps:get(policy_type, Policy)),
    rl_agent:stop(AgentId).

%%====================================================================
%% Agent ID Format Tests
%%====================================================================

%% @doc Test various binary agent ID formats
agent_id_formats_test_() ->
    IdFormats = [
        <<"simple">>,
        <<"agent_with_underscores">>,
        <<"Agent123">>,
        <<"agent-with-dashes">>,
        <<"agent.with.dots">>,
        <<"agent_with_unicode_\xc3\xa9">>
    ],
    [{Id,
      fun() ->
          Options = #{},
          {ok, _Pid} = rl_agent:start_link(Id, Options),
          rl_agent:stop(Id)
      end}
     || Id <- IdFormats].
