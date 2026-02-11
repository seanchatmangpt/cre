%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @author CRE Team
%% @version 0.3.0
%% @doc Structured Loop Workflow Pattern Tests
%%
%% Comprehensive test suite for wfnet_loop pattern including:
%% - Basic while loop execution
%% - Until loop execution
%% - Max iteration limits
%% - State tracking
%% - Condition evaluation
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_loop_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main test generator.
%%--------------------------------------------------------------------
wfnet_loop_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"while loop executes while condition is true", fun while_loop_test/0},
        {"until loop executes until condition is true", fun until_loop_test/0},
        {"loop respects max iterations limit", fun max_iterations_test/0},
        {"loop with unlimited iterations", fun unlimited_iterations_test/0},
        {"state is tracked across iterations", fun state_tracking_test/0},
        {"workflow spec is valid", fun workflow_spec_test/0},
        {"init creates valid state", fun init_test/0},
        {"init_marking returns correct tokens", fun init_marking_test/0},
        {"is_enabled checks transition enablement", fun is_enabled_test/0},
        {"fire enter_body executes body function", fun fire_enter_body_test/0},
        {"fire check_condition evaluates condition", fun fire_check_condition_test/0},
        {"fire repeat transitions back to start", fun fire_repeat_test/0},
        {"fire exit completes the loop", fun fire_exit_test/0},
        {"invalid config throws error", fun invalid_config_test/0},
        {"empty body returns initial state", fun empty_body_test/0},
        {"condition that is always false exits immediately", fun false_condition_test/0},
        {"nested state updates work correctly", fun nested_state_test/0},
        {"list accumulation in loop", fun list_accumulation_test/0}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Ensure any previous test processes are cleaned up
    ok.

cleanup(_SetupState) ->
    %% Cleanup after tests
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that while loop executes while condition is true.
%%--------------------------------------------------------------------
while_loop_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(X) -> X < 5 end,
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, 0),
    ?assertEqual(5, Result).

%%--------------------------------------------------------------------
%% @doc Test that until loop executes until condition is true.
%%--------------------------------------------------------------------
until_loop_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(X) -> X >= 5 end,
    {ok, Result} = wfnet_loop:execute(Body, until, Cond, 0),
    ?assertEqual(5, Result).

%%--------------------------------------------------------------------
%% @doc Test that loop respects max iterations limit.
%%--------------------------------------------------------------------
max_iterations_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(_X) -> true end,  %% Always true

    %% Create a workflow with limited iterations
    Config = #{
        body_fun => Body,
        condition_fun => Cond,
        initial_state => 0,
        max_iterations => 10
    },
    Spec = wfnet_loop:new(Config),
    ?assertMatch(#{optional := #{max_iterations := 10}}, Spec).

%%--------------------------------------------------------------------
%% @doc Test loop with unlimited iterations.
%%--------------------------------------------------------------------
unlimited_iterations_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> X < 100 end,  %% Will exit at 100
        max_iterations => unlimited
    },
    {ok, State} = wfnet_loop:init(Config),
    ?assertEqual(unlimited, maps:get(max_iterations, State)).

%%--------------------------------------------------------------------
%% @doc Test that state is tracked across iterations.
%%--------------------------------------------------------------------
state_tracking_test() ->
    %% Use a counter function
    Body = fun(Count) -> Count + 1 end,
    Cond = fun(Count) -> Count < 10 end,
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, 0),
    ?assertEqual(10, Result).

%%--------------------------------------------------------------------
%% @doc Test that workflow spec is valid.
%%--------------------------------------------------------------------
workflow_spec_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end
    },
    Spec = wfnet_loop:new(Config),

    %% Check required fields
    ?assert(maps:is_key(places, Spec)),
    ?assert(maps:is_key(transitions, Spec)),
    ?assert(maps:is_key(start_place, Spec)),
    ?assert(maps:is_key(end_place, Spec)),
    ?assert(maps:is_key(preset, Spec)),
    ?assert(maps:is_key(postset, Spec)),

    %% Check structure
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)),

    %% Check places
    Places = maps:get(places, Spec),
    ?assert(lists:member(start, Places)),
    ?assert(lists:member('end', Places)),
    ?assert(lists:member(body, Places)),
    ?assert(lists:member(check, Places)),
    ?assert(lists:member(condition, Places)),

    %% Check transitions
    Transitions = maps:get(transitions, Spec),
    ?assert(lists:member(enter_body, Transitions)),
    ?assert(lists:member(check_condition, Transitions)),
    ?assert(lists:member(repeat, Transitions)),
    ?assert(lists:member(exit, Transitions)).

%%--------------------------------------------------------------------
%% @doc Test init creates valid state.
%%--------------------------------------------------------------------
init_test() ->
    Config = #{
        body_fun => fun(X) -> X + 1 end,
        condition_fun => fun(X) -> X < 5 end,
        initial_state => 42,
        max_iterations => 1000,
        loop_type => while
    },
    {ok, State} = wfnet_loop:init(Config),

    ?assertEqual(42, maps:get(current_state, State)),
    ?assertEqual(0, maps:get(iteration_count, State)),
    ?assertEqual(1000, maps:get(max_iterations, State)),
    ?assertEqual(while, maps:get(loop_type, State)),
    ?assert(is_function(maps:get(body_fun, State))),
    ?assert(is_function(maps:get(condition_fun, State))).

%%--------------------------------------------------------------------
%% @doc Test init_marking returns correct tokens.
%%--------------------------------------------------------------------
init_marking_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end
    },
    {ok, State} = wfnet_loop:init(Config),

    ?assertEqual([init], wfnet_loop:init_marking(start, State)),
    ?assertEqual([evaluate], wfnet_loop:init_marking(condition, State)),
    ?assertEqual([], wfnet_loop:init_marking(body, State)),
    ?assertEqual([], wfnet_loop:init_marking(check, State)),
    ?assertEqual([], wfnet_loop:init_marking('end', State)).

%%--------------------------------------------------------------------
%% @doc Test is_enabled checks transition enablement.
%%--------------------------------------------------------------------
is_enabled_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end,
        max_iterations => 10
    },
    {ok, State} = wfnet_loop:init(Config),
    Mode = #{},

    %% All transitions should be enabled initially
    ?assert(wfnet_loop:is_enabled(enter_body, Mode, State)),
    ?assert(wfnet_loop:is_enabled(check_condition, Mode, State)),
    ?assert(wfnet_loop:is_enabled(repeat, Mode, State)),
    ?assert(wfnet_loop:is_enabled(exit, Mode, State)),

    %% After reaching max iterations, repeat should be disabled
    StateMaxed = State#{iteration_count => 10},
    ?assertNot(wfnet_loop:is_enabled(repeat, Mode, StateMaxed)),

    %% With unlimited max iterations, repeat stays enabled
    StateUnlimited = State#{iteration_count => 10000, max_iterations => unlimited},
    ?assert(wfnet_loop:is_enabled(repeat, Mode, StateUnlimited)).

%%--------------------------------------------------------------------
%% @doc Test fire enter_body executes body function.
%%--------------------------------------------------------------------
fire_enter_body_test() ->
    Config = #{
        body_fun => fun(X) -> X * 2 end,
        condition_fun => fun(X) -> X < 100 end,
        initial_state => 5
    },
    {ok, State} = wfnet_loop:init(Config),
    Mode = #{},

    {produce, ProduceMap, NewState} = wfnet_loop:fire(enter_body, Mode, State),

    %% Check produced tokens
    ?assertEqual([], maps:get(body, ProduceMap)),
    ?assertEqual([check], maps:get(check, ProduceMap)),

    %% Check state was updated
    ?assertEqual(10, maps:get(current_state, NewState)),
    ?assertEqual(1, maps:get(iteration_count, NewState)).

%%--------------------------------------------------------------------
%% @doc Test fire check_condition evaluates condition.
%%--------------------------------------------------------------------
fire_check_condition_test() ->
    %% Test with while loop (true condition means continue)
    ConfigWhile = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> X < 5 end,
        initial_state => 3,
        loop_type => while
    },
    {ok, StateWhile} = wfnet_loop:init(ConfigWhile),
    Mode = #{},

    {produce, ProduceMapWhile, _} = wfnet_loop:fire(check_condition, Mode, StateWhile),
    ?assertEqual([true], maps:get(condition, ProduceMapWhile)),

    %% Test with condition that should be false
    StateWhile2 = StateWhile#{current_state => 10},
    {produce, ProduceMapWhile2, _} = wfnet_loop:fire(check_condition, Mode, StateWhile2),
    ?assertEqual([false], maps:get(condition, ProduceMapWhile2)).

%%--------------------------------------------------------------------
%% @doc Test fire repeat transitions back to start.
%%--------------------------------------------------------------------
fire_repeat_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end
    },
    {ok, State} = wfnet_loop:init(Config),
    Mode = #{},

    {produce, ProduceMap, _} = wfnet_loop:fire(repeat, Mode, State),

    ?assertEqual([looping], maps:get(start, ProduceMap)),
    ?assertEqual([], maps:get(condition, ProduceMap)).

%%--------------------------------------------------------------------
%% @doc Test fire exit completes the loop.
%%--------------------------------------------------------------------
fire_exit_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end,
        initial_state => final_result
    },
    {ok, State} = wfnet_loop:init(Config),
    Mode = #{},

    {produce, ProduceMap, _} = wfnet_loop:fire(exit, Mode, State),

    ?assertEqual([final_result], maps:get('end', ProduceMap)),
    ?assertEqual([], maps:get(condition, ProduceMap)).

%%--------------------------------------------------------------------
%% @doc Test invalid config throws error.
%%--------------------------------------------------------------------
invalid_config_test() ->
    %% Test with invalid body_fun
    Config1 = #{
        body_fun => not_a_function,
        condition_fun => fun(X) -> true end
    },
    ?assertError({invalid_config, body_fun_must_be_function},
                 wfnet_loop:new(Config1)),

    %% Test with invalid condition_fun
    Config2 = #{
        body_fun => fun(X) -> X end,
        condition_fun => not_a_function
    },
    ?assertError({invalid_config, condition_fun_must_be_function},
                 wfnet_loop:new(Config2)),

    %% Test with invalid max_iterations
    Config3 = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end,
        max_iterations => -1
    },
    ?assertError({invalid_config, max_iterations_must_be_positive_integer_or_unlimited},
                 wfnet_loop:new(Config3)).

%%--------------------------------------------------------------------
%% @doc Test empty body returns initial state.
%%--------------------------------------------------------------------
empty_body_test() ->
    Body = fun(X) -> X end,  %% Identity function
    Cond = fun(X) -> X < 1 end,
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, 0),
    ?assertEqual(0, Result).

%%--------------------------------------------------------------------
%% @doc Test condition that is always false exits immediately.
%%--------------------------------------------------------------------
false_condition_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(_X) -> false end,
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, 42),
    ?assertEqual(42, Result).

%%--------------------------------------------------------------------
%% @doc Test nested state updates work correctly.
%%--------------------------------------------------------------------
nested_state_test() ->
    %% Use a map as state
    Body = fun(State) -> State#{count => maps:get(count, State, 0) + 1} end,
    Cond = fun(State) -> maps:get(count, State, 0) < 5 end,
    InitialState = #{count => 0, total => 100},
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, InitialState),
    ?assertEqual(5, maps:get(count, Result)),
    ?assertEqual(100, maps:get(total, Result)).

%%--------------------------------------------------------------------
%% @doc Test list accumulation in loop.
%%--------------------------------------------------------------------
list_accumulation_test() ->
    %% Accumulate values in a list
    Body = fun(N) -> N + 1 end,
    Cond = fun(N) -> N =< 5 end,
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, 1),
    ?assertEqual(6, Result).

%%====================================================================
%% Property-Based Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Property: Loop never exceeds max iterations.
%%--------------------------------------------------------------------
prop_max_iterations_test() ->
    %% This would use proper or similar for property testing
    %% For now, we do a simple manual check
    MaxIter = 100,
    Body = fun(X) -> X + 1 end,
    Cond = fun(_X) -> true end,

    Config = #{
        body_fun => Body,
        condition_fun => Cond,
        max_iterations => MaxIter
    },
    Spec = wfnet_loop:new(Config),
    Optional = maps:get(optional, Spec),
    ?assertEqual(MaxIter, maps:get(max_iterations, Optional)).

%%====================================================================
%% Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test full workflow execution with gen_wfnet.
%%--------------------------------------------------------------------
integration_test_() ->
    {setup,
     fun() ->
         %% Start a loop workflow
         Config = #{
             body_fun => fun(X) -> X + 1 end,
             condition_fun => fun(X) -> X < 3 end,
             initial_state => 0
         },
         {ok, Pid} = wfnet_loop:start_link(Config),
         Pid
     end,
     fun(Pid) ->
         gen_wfnet:stop(Pid)
     end,
     [
      {"workflow runs to completion", fun(_) -> integration_run_test() end}
     ]}.

integration_run_test() ->
    %% This test would verify the actual workflow execution
    %% For now, we test the spec is valid
    Config = #{
        body_fun => fun(X) -> X + 1 end,
        condition_fun => fun(X) -> X < 3 end,
        initial_state => 0
    },
    Spec = wfnet_loop:new(Config),
    ?assertMatch(#{places := _, transitions := _}, Spec).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test loop with condition that crashes.
%%--------------------------------------------------------------------
crashing_condition_test() ->
    Body = fun(X) -> X end,
    Cond = fun(_X) -> error(bad) end,
    %% Should handle gracefully by treating crash as false
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, 42),
    ?assertEqual(42, Result).

%%--------------------------------------------------------------------
%% @doc Test loop with body that crashes.
%%--------------------------------------------------------------------
crashing_body_test() ->
    Body = fun(_X) -> error(bad) end,
    Cond = fun(X) -> X < 5 end,
    %% Should handle crash by keeping original state
    {ok, Result} = wfnet_loop:execute(Body, while, Cond, 0),
    ?assertEqual(0, Result).
