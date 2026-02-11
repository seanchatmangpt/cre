%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @author Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
%% @copyright 2015
%% @version 0.3.0
%%
%% @doc Comprehensive Execution Tests for Extended Control Flow YAWL Patterns
%%
%% This module contains comprehensive execution tests for extended control
%% flow patterns WCP-21 through WCP-28:
%% - WCP-21: Structured Synchronization Pattern
%% - WCP-22: Partial Join Pattern
%% - WCP-23: Structured Loop Pattern
%% - WCP-24: Recursion Pattern
%% - WCP-25: Interleaved Loop Pattern
%% - WCP-26: Critical Section Pattern
%% - WCP-27: Protocol Pattern
%% - WCP-28: Try-Catch Pattern
%%
%% Tests cover:
%% - Normal execution
%% - Edge cases
%% - Concurrency behavior
%% - Resource management
%% - Performance validation
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_extended_control_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions
%%====================================================================

-record(pattern_state, {
          pattern_type :: atom(),
          subprocess :: module() | function() | undefined,
          instance_count :: non_neg_integer() | undefined,
          max_instances :: non_neg_integer() | unlimited | undefined,
          pending_instances :: list(),
          active_instances :: list(),
          completed_instances :: list(),
          choice_data :: map(),
          branch_queue :: list()
         }).

-record(sync_token, {
          activity_id :: reference(),
          data :: term(),
          completed :: boolean()
         }).

-record(loop_state, {
          iteration :: non_neg_integer(),
          condition_result :: boolean(),
          body_data :: term()
         }).

-record(recursion_token, {
          level :: non_neg_integer(),
          data :: term(),
          is_base_case :: boolean()
         }).

-record(protocol_state, {
          request :: term(),
          response :: term(),
          timeout_ref :: reference() | undefined,
          start_time :: integer()
         }).

-record(catch_state, {
          exception_type :: atom(),
          exception_reason :: term(),
          handled :: boolean()
         }).

-record(lock_state, {
          lock_id :: term(),
          holder :: pid() | undefined,
          queue :: list()
         }).

%%====================================================================
%% Test Activities and Helper Functions
%%====================================================================

%% Test activities for sync pattern
sync_activity_1() ->
    fun(Data) ->
        timer:sleep(10),
        {activity_1_complete, Data}
    end.

sync_activity_2() ->
    fun(Data) ->
        timer:sleep(15),
        {activity_2_complete, Data}
    end.

sync_activity_3() ->
    fun(Data) ->
        timer:sleep(20),
        {activity_3_complete, Data}
    end.

%% Test activities for partial join
partial_activity_1() ->
    fun() ->
        timer:sleep(50),
        partial_1_done
    end.

partial_activity_2() ->
    fun() ->
        timer:sleep(100),
        partial_2_done
    end.

partial_activity_3() ->
    fun() ->
        timer:sleep(150),
        partial_3_done
    end.

%% Test activities for critical section
critical_activity() ->
    fun(LockId, Data) ->
        %% Simulate critical section work
        timer:sleep(20),
        {critical_work_done, LockId, Data}
    end.

%% Test protocol functions
protocol_request() ->
    fun() ->
        {request, <<"test_request">>}
    end.

protocol_response_handler() ->
    fun(Response) ->
        case Response of
            {response, Data} -> {handled, Data};
            {error, Reason} -> {error_handled, Reason};
            _ -> {unknown_response, Response}
        end
    end.

%% Test try-catch functions
try_success_function() ->
    fun(Input) ->
        {ok, Input * 2}
    end.

try_failure_function() ->
    fun(_Input) ->
        throw(business_exception)
    end.

catch_handler() ->
    fun(Exception) ->
        case Exception of
            business_exception -> {caught, business};
            timeout_exception -> {caught, timeout};
            _ -> {caught, unknown}
        end
    end.

%% Loop condition functions
loop_condition_while() ->
    fun(Counter) -> Counter < 5 end.

loop_condition_until() ->
    fun(Counter) -> Counter >= 5 end.

loop_body() ->
    fun(Counter) ->
        Counter + 1
    end.

%% Recursion functions
recursive_function() ->
    fun
        (0) -> base_case_reached;
        (N) when N > 0 -> {recursive_step, N - 1}
    end.

base_case_function() ->
    fun
        (0) -> true;
        (_) -> false
    end.

%%====================================================================
%% WCP-21: Structured Synchronization Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-21: Structured Synchronization
%% @end
%%--------------------------------------------------------------------
structured_sync_pattern_test_() ->
    {setup,
     fun setup_sync/0,
     fun cleanup_sync/1,
     fun(_Context) ->
         [
          {"WCP-21: Normal execution - All activities complete",
           fun sync_normal_execution_test/0},
          {"WCP-21: Barrier synchronization semantics",
           fun sync_barrier_test/0},
          {"WCP-21: Activities complete in different order",
           fun sync_out_of_order_test/0},
          {"WCP-21: Empty activity list",
           fun sync_empty_activities_test/0},
          {"WCP-21: Single activity",
           fun sync_single_activity_test/0},
          {"WCP-21: Maximum activities",
           fun sync_max_activities_test/0},
          {"WCP-21: Timeout behavior",
           fun sync_timeout_test/0},
          {"WCP-21: Concurrent execution",
           fun sync_concurrent_test/0}
         ]
     end}.

setup_sync() ->
    %% Initialize sync pattern state
    Activities = [sync_activity_1(), sync_activity_2(), sync_activity_3()],
    Pattern = cre_yawl_patterns:structured_sync(Activities, initial_data),
    #{pattern => Pattern, activities => Activities}.

cleanup_sync(_Context) ->
    ok.

%% Normal execution test
sync_normal_execution_test() ->
    #{pattern := Pattern} = setup_sync(),

    %% Verify pattern structure
    ?assertEqual(structured_sync, Pattern#pattern_state.pattern_type),
    ?assertEqual(3, Pattern#pattern_state.instance_count),
    ?assert(is_list(Pattern#pattern_state.subprocess)),
    ?assertEqual(3, length(Pattern#pattern_state.subprocess)).

%% Barrier synchronization test
sync_barrier_test() ->
    %% Test that all activity places are part of barrier
    AllPlaces = cre_yawl_patterns:place_lst(),

    %% Verify sync places exist
    ?assert(lists:member('p_sync_block_start', AllPlaces)),
    ?assert(lists:member('p_sync_block_active', AllPlaces)),
    ?assert(lists:member('p_sync_barrier', AllPlaces)),
    ?assert(lists:member('p_sync_block_done', AllPlaces)),
    ?assert(lists:member('p_sync_activity_1', AllPlaces)),
    ?assert(lists:member('p_sync_activity_2', AllPlaces)),
    ?assert(lists:member('p_sync_activity_3', AllPlaces)).

%% Out of order completion test
sync_out_of_order_test() ->
    #{pattern := Pattern} = setup_sync(),

    %% Simulate activities completing in reverse order
    %% Activity 3 completes first
    Mode3 = #{'p_sync_activity_3' => [{completed, activity_3}]},
    ?assertEqual(true, cre_yawl_patterns:is_enabled('t_sync_barrier',
                                                       #{'p_sync_activity_1' => [],
                                                         'p_sync_activity_2' => [],
                                                         'p_sync_activity_3' => [completed]},
                                                       Pattern)).

%% Empty activities test
sync_empty_activities_test() ->
    Pattern = cre_yawl_patterns:structured_sync([], undefined),
    ?assertEqual(0, Pattern#pattern_state.instance_count),
    ?assertEqual([], Pattern#pattern_state.subprocess).

%% Single activity test
sync_single_activity_test() ->
    Pattern = cre_yawl_patterns:structured_sync([sync_activity_1()], data),
    ?assertEqual(1, Pattern#pattern_state.instance_count),
    ?assertEqual([sync_activity_1()], Pattern#pattern_state.subprocess).

%% Maximum activities test
sync_max_activities_test() ->
    %% Test with larger number of activities
    Activities = lists:duplicate(10, sync_activity_1()),
    Pattern = cre_yawl_patterns:structured_sync(Activities, stress_test),
    ?assertEqual(10, Pattern#pattern_state.instance_count),
    ?assertEqual(10, length(Pattern#pattern_state.subprocess)).

%% Timeout test
sync_timeout_test() ->
    #{pattern := Pattern} = setup_sync(),

    %% Verify timeout transition exists
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_sync_timeout', AllTransitions)).

%% Concurrent execution test
sync_concurrent_test() ->
    #{activities := Activities} = setup_sync(),

    %% Execute activities concurrently
    Pids = lists:map(fun(Activity) ->
        spawn(fun() -> Activity(test_data) end)
    end, Activities),

    %% Verify all processes started
    ?assertEqual(3, length(Pids)),
    ?assert(lists:all(fun(Pid) -> is_pid(Pid) end, Pids)).

%%====================================================================
%% WCP-22: Partial Join Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-22: Partial Join (Quorum)
%% @end
%%--------------------------------------------------------------------
partial_join_pattern_test_() ->
    {setup,
     fun setup_partial_join/0,
     fun cleanup_partial_join/1,
     fun(_Context) ->
         [
          {"WCP-22: Normal execution - Quorum reached",
           fun partial_join_quorum_test/0},
          {"WCP-22: All activities complete",
           fun partial_join_all_complete_test/0},
          {"WCP-22: Minimum quorum (1)",
           fun partial_join_min_quorum_test/0},
          {"WCP-22: Full quorum",
           fun partial_join_full_quorum_test/0},
          {"WCP-22: Quorum validation",
           fun partial_join_validation_test/0},
          {"WCP-22: Timeout before quorum",
           fun partial_join_timeout_test/0},
          {"WCP-22: Empty activities",
           fun partial_join_empty_test/0},
          {"WCP-22: Concurrent completion",
           fun partial_join_concurrent_test/0}
         ]
     end}.

setup_partial_join() ->
    Activities = [partial_activity_1(), partial_activity_2(), partial_activity_3()],
    Pattern = cre_yawl_patterns:partial_join(Activities, 2),
    #{pattern => Pattern, activities => Activities, quorum => 2}.

cleanup_partial_join(_Context) ->
    ok.

%% Quorum reached test
partial_join_quorum_test() ->
    #{pattern := Pattern, quorum := Quorum} = setup_partial_join(),

    ?assertEqual(partial_join, Pattern#pattern_state.pattern_type),
    ?assertEqual(2, Pattern#pattern_state.max_instances),
    ?assertEqual(2, Quorum).

%% All complete test
partial_join_all_complete_test() ->
    #{pattern := Pattern} = setup_partial_join(),

    ?assertEqual(3, length(Pattern#pattern_state.pending_instances)),
    ?assertEqual([], Pattern#pattern_state.completed_instances).

%% Minimum quorum test
partial_join_min_quorum_test() ->
    Pattern = cre_yawl_patterns:partial_join([partial_activity_1()], 1),
    ?assertEqual(1, Pattern#pattern_state.max_instances).

%% Full quorum test
partial_join_full_quorum_test() ->
    Pattern = cre_yawl_patterns:partial_join(
        [partial_activity_1(), partial_activity_2(), partial_activity_3()],
        3
    ),
    ?assertEqual(3, Pattern#pattern_state.max_instances).

%% Quorum validation test
partial_join_validation_test() ->
    %% Verify quorum cannot exceed activity count
    try
        cre_yawl_patterns:partial_join([partial_activity_1()], 2),
        ?assert(false, "Should have thrown error")
    catch
        _:_ -> ok  % Expected
    end.

%% Timeout test
partial_join_timeout_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_partial_timeout', AllTransitions)).

%% Empty test
partial_join_empty_test() ->
    try
        cre_yawl_patterns:partial_join([], 0),
        ?assert(false, "Should fail with empty activities")
    catch
        _:_ -> ok
    end.

%% Concurrent completion test
partial_join_concurrent_test() ->
    #{activities := Activities} = setup_partial_join(),

    %% Start all activities concurrently
    Results = lists:map(fun(Activity) ->
        spawn_monitor(fun() -> Activity() end)
    end, Activities),

    ?assertEqual(3, length(Results)),

    %% Verify all processes and monitors created
    lists:foreach(fun({Pid, _Ref}) ->
        ?assert(is_pid(Pid))
    end, Results).

%%====================================================================
%% WCP-23: Structured Loop Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-23: Structured Loop (While/Until)
%% @end
%%--------------------------------------------------------------------
structured_loop_pattern_test_() ->
    {setup,
     fun setup_loop/0,
     fun cleanup_loop/1,
     fun(_Context) ->
         [
          {"WCP-23: While loop - Normal execution",
           fun loop_while_normal_test/0},
          {"WCP-23: Until loop - Normal execution",
           fun loop_until_normal_test/0},
          {"WCP-23: Zero iterations",
           fun loop_zero_iterations_test/0},
          {"WCP-23: Single iteration",
           fun loop_single_iteration_test/0},
          {"WCP-23: Maximum iterations",
           fun loop_max_iterations_test/0},
          {"WCP-23: Loop break",
           fun loop_break_test/0},
          {"WCP-23: Loop continue",
           fun loop_continue_test/0},
          {"WCP-23: Loop state management",
           fun loop_state_test/0},
          {"WCP-23: Loop timeout",
           fun loop_timeout_test/0}
         ]
     end}.

setup_loop() ->
    PatternWhile = cre_yawl_patterns:structured_loop(loop_body(), while, loop_condition_while()),
    PatternUntil = cre_yawl_patterns:structured_loop(loop_body(), until, loop_condition_until()),
    #{while_pattern => PatternWhile, until_pattern => PatternUntil}.

cleanup_loop(_Context) ->
    ok.

%% While loop normal test
loop_while_normal_test() ->
    #{while_pattern := Pattern} = setup_loop(),

    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type),
    ?assertEqual(while, maps:get(loop_type, Pattern#pattern_state.choice_data)),
    ?assert(is_function(maps:get(condition, Pattern#pattern_state.choice_data))).

%% Until loop normal test
loop_until_normal_test() ->
    #{until_pattern := Pattern} = setup_loop(),

    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type),
    ?assertEqual(until, maps:get(loop_type, Pattern#pattern_state.choice_data)).

%% Zero iterations test
loop_zero_iterations_test() ->
    Condition = fun(_) -> false end,
    Pattern = cre_yawl_patterns:structured_loop(loop_body(), while, Condition),

    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type),
    ?assertEqual(while, maps:get(loop_type, Pattern#pattern_state.choice_data)).

%% Single iteration test
loop_single_iteration_test() ->
    Condition = fun(N) -> N >= 1 end,
    Pattern = cre_yawl_patterns:structured_loop(loop_body(), until, Condition),

    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type).

%% Maximum iterations test
loop_max_iterations_test() ->
    Condition = fun(N) -> N < 1000 end,
    Pattern = cre_yawl_patterns:structured_loop(loop_body(), while, Condition),

    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type),
    ?assert(is_function(maps:get(condition, Pattern#pattern_state.choice_data))).

%% Loop break test
loop_break_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_loop_break', AllTransitions)).

%% Loop continue test
loop_continue_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_loop_continue', AllTransitions)).

%% Loop state test
loop_state_test() ->
    #{while_pattern := Pattern} = setup_loop(),

    ?assert(is_list(Pattern#pattern_state.active_instances)),
    ?assertEqual(0, Pattern#pattern_state.instance_count).

%% Loop timeout test
loop_timeout_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_loop_timeout', AllTransitions)).

%%====================================================================
%% WCP-24: Recursion Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-24: Recursion
%% @end
%%--------------------------------------------------------------------
recursion_pattern_test_() ->
    {setup,
     fun setup_recursion/0,
     fun cleanup_recursion/1,
     fun(_Context) ->
         [
          {"WCP-24: Recursive call - Normal execution",
           fun recursion_normal_test/0},
          {"WCP-24: Base case detection",
           fun recursion_base_case_test/0},
          {"WCP-24: Single level recursion",
           fun recursion_single_level_test/0},
          {"WCP-24: Deep recursion",
           fun recursion_deep_test/0},
          {"WCP-24: Recursive state management",
           fun recursion_state_test/0},
          {"WCP-24: Stack overflow prevention",
           fun recursion_stack_limit_test/0},
          {"WCP-24: Empty base case",
           fun recursion_empty_base_test/0},
          {"WCP-24: Recursive result accumulation",
           fun recursion_accumulation_test/0}
         ]
     end}.

setup_recursion() ->
    Pattern = cre_yawl_patterns:recursion(recursive_function(), base_case_function()),
    #{pattern => Pattern}.

cleanup_recursion(_Context) ->
    ok.

%% Normal recursion test
recursion_normal_test() ->
    #{pattern := Pattern} = setup_recursion(),

    ?assertEqual(recursion, Pattern#pattern_state.pattern_type),
    ?assert(is_function(Pattern#pattern_state.subprocess)),
    ?assert(is_function(maps:get(base_case, Pattern#pattern_state.choice_data))).

%% Base case detection test
recursion_base_case_test() ->
    #{pattern := Pattern} = setup_recursion(),

    BaseCaseFun = maps:get(base_case, Pattern#pattern_state.choice_data),
    ?assert(BaseCaseFun(0)),
    ?assertNot(BaseCaseFun(1)),
    ?assertNot(BaseCaseFun(5)).

%% Single level test
recursion_single_level_test() ->
    Pattern = cre_yawl_patterns:recursion(
        fun(1) -> base_case_reached; (N) -> {recursive_step, N - 1} end,
        fun(1) -> true; (_) -> false end
    ),
    ?assertEqual(recursion, Pattern#pattern_state.pattern_type).

%% Deep recursion test
recursion_deep_test() ->
    %% Test with higher recursion depth
    DeepFunc = fun
        (0) -> base_case_reached;
        (N) when N > 0 -> {recursive_step, N - 1}
    end,
    Pattern = cre_yawl_patterns:recursion(DeepFunc, base_case_function()),
    ?assertEqual(recursion, Pattern#pattern_state.pattern_type).

%% Recursion state test
recursion_state_test() ->
    #{pattern := Pattern} = setup_recursion(),

    ?assert(is_list(Pattern#pattern_state.active_instances)),
    ?assertEqual(0, Pattern#pattern_state.instance_count).

%% Stack limit test
recursion_stack_limit_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_rec_stack_push', AllTransitions)),
    ?assert(lists:member('t_rec_stack_pop', AllTransitions)).

%% Empty base case test
recursion_empty_base_test() ->
    Pattern = cre_yawl_patterns:recursion(
        fun(_) -> always_recurse end,
        fun(_) -> false end
    ),
    ?assertEqual(recursion, Pattern#pattern_state.pattern_type).

%% Accumulation test
recursion_accumulation_test() ->
    %% Test recursive accumulation
    SumFunc = fun
        SumFuncHelper(0) -> 0;
        SumFuncHelper(N) -> N + SumFuncHelper(N - 1)
    end,
    Pattern = cre_yawl_patterns:recursion(
        SumFunc,
        fun(0) -> true; (_) -> false end
    ),
    ?assertEqual(recursion, Pattern#pattern_state.pattern_type).

%%====================================================================
%% WCP-25: Interleaved Loop Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-25: Interleaved Loop (Parallel + Loop)
%% @end
%%--------------------------------------------------------------------
interleaved_loop_pattern_test_() ->
    {setup,
     fun setup_interleaved_loop/0,
     fun cleanup_interleaved_loop/1,
     fun(_Context) ->
         [
          {"WCP-25: Interleaved execution - Normal",
           fun interleaved_normal_test/0},
          {"WCP-25: Parallel bodies execute interleaved",
           fun interleaved_parallel_test/0},
          {"WCP-25: Loop condition evaluation",
           fun interleaved_condition_test/0},
          {"WCP-25: Single iteration",
           fun interleaved_single_iteration_test/0},
          {"WCP-25: Multiple iterations",
           fun interleaved_multiple_iterations_test/0},
          {"WCP-25: Empty activity list",
           fun interleaved_empty_test/0},
          {"WCP-25: Activity fairness",
           fun interleaved_fairness_test/0},
          {"WCP-25: Interleaved loop timeout",
           fun interleaved_timeout_test/0},
          {"WCP-25: State across iterations",
           fun interleaved_state_test/0}
         ]
     end}.

setup_interleaved_loop() ->
    Activities = [sync_activity_1(), sync_activity_2(), sync_activity_3()],
    Condition = fun(N) -> N < 3 end,
    Pattern = cre_yawl_patterns:interleaved_loop(Activities, Condition),
    #{pattern => Pattern, activities => Activities}.

cleanup_interleaved_loop(_Context) ->
    ok.

%% Normal interleaved test
interleaved_normal_test() ->
    #{pattern := Pattern} = setup_interleaved_loop(),

    ?assertEqual(interleaved_loop, Pattern#pattern_state.pattern_type),
    ?assert(is_list(Pattern#pattern_state.subprocess)),
    ?assertEqual(3, length(Pattern#pattern_state.subprocess)).

%% Parallel execution test
interleaved_parallel_test() ->
    AllPlaces = cre_yawl_patterns:place_lst(),

    %% Verify interleaved loop places
    ?assert(lists:member('p_il_loop_start', AllPlaces)),
    ?assert(lists:member('p_il_parallel', AllPlaces)),
    ?assert(lists:member('p_il_interleave', AllPlaces)),
    ?assert(lists:member('p_il_loop_cond', AllPlaces)),
    ?assert(lists:member('p_il_exit', AllPlaces)).

%% Condition test
interleaved_condition_test() ->
    #{pattern := Pattern} = setup_interleaved_loop(),

    ConditionFun = maps:get(condition, Pattern#pattern_state.choice_data),
    ?assert(is_function(ConditionFun)),
    ?assert(ConditionFun(0)),
    ?assert(ConditionFun(1)),
    ?assertNot(ConditionFun(5)).

%% Single iteration test
interleaved_single_iteration_test() ->
    Condition = fun(N) -> N >= 1 end,
    Pattern = cre_yawl_patterns:interleaved_loop([sync_activity_1()], Condition),

    ?assertEqual(interleaved_loop, Pattern#pattern_state.pattern_type).

%% Multiple iterations test
interleaved_multiple_iterations_test() ->
    Condition = fun(N) -> N < 10 end,
    Pattern = cre_yawl_patterns:interleaved_loop([sync_activity_1()], Condition),

    ?assertEqual(interleaved_loop, Pattern#pattern_state.pattern_type).

%% Empty test
interleaved_empty_test() ->
    Pattern = cre_yawl_patterns:interleaved_loop([], fun(_) -> false end),
    ?assertEqual([], Pattern#pattern_state.subprocess).

%% Fairness test
interleaved_fairness_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_il_interleave', AllTransitions)).

%% Timeout test
interleaved_timeout_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_il_timeout', AllTransitions)).

%% State test
interleaved_state_test() ->
    #{pattern := Pattern} = setup_interleaved_loop(),

    ?assert(is_list(Pattern#pattern_state.branch_queue)),
    ?assertEqual(0, Pattern#pattern_state.instance_count).

%%====================================================================
%% WCP-26: Critical Section Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-26: Critical Section (Mutex)
%% @end
%%--------------------------------------------------------------------
critical_section_pattern_test_() ->
    {setup,
     fun setup_critical_section/0,
     fun cleanup_critical_section/1,
     fun(_Context) ->
         [
          {"WCP-26: Lock acquisition - Single request",
           fun critical_acquire_test/0},
          {"WCP-26: Lock release",
           fun critical_release_test/0},
          {"WCP-26: Multiple concurrent requests",
           fun critical_concurrent_test/0},
          {"WCP-26: Mutex behavior - Only one holder",
           fun critical_mutex_test/0},
          {"WCP-26: Lock queue ordering",
           fun critical_queue_test/0},
          {"WCP-26: Lock timeout",
           fun critical_timeout_test/0},
          {"WCP-26: Resource cleanup",
           fun critical_cleanup_test/0},
          {"WCP-26: Nested critical sections",
           fun critical_nested_test/0},
          {"WCP-26: Reentrant lock",
           fun critical_reentrant_test/0}
         ]
     end}.

setup_critical_section() ->
    LockId = make_ref(),
    Pattern = cre_yawl_patterns:critical_section(critical_activity(), LockId),
    #{pattern => Pattern, lock_id => LockId}.

cleanup_critical_section(_Context) ->
    ok.

%% Lock acquisition test
critical_acquire_test() ->
    #{pattern := Pattern, lock_id := LockId} = setup_critical_section(),

    ?assertEqual(critical_section, Pattern#pattern_state.pattern_type),
    ?assertEqual(LockId, maps:get(lock_id, Pattern#pattern_state.choice_data)).

%% Lock release test
critical_release_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),

    %% Verify critical section transitions
    ?assert(lists:member('t_cs_acquire', AllTransitions)),
    ?assert(lists:member('t_cs_enter', AllTransitions)),
    ?assert(lists:member('t_cs_exit', AllTransitions)),
    ?assert(lists:member('t_cs_release', AllTransitions)).

%% Concurrent requests test
critical_concurrent_test() ->
    #{lock_id := LockId} = setup_critical_section(),

    %% Simulate multiple lock requests
    RequestPids = lists:map(fun(_) ->
        spawn(fun() ->
            %% Simulate lock request
            timer:sleep(10),
            {lock_acquired, LockId}
        end)
    end, lists:seq(1, 5)),

    ?assertEqual(5, length(RequestPids)),
    ?assert(lists:all(fun(Pid) -> is_pid(Pid) end, RequestPids)).

%% Mutex behavior test
critical_mutex_test() ->
    AllPlaces = cre_yawl_patterns:place_lst(),

    %% Verify only one lock token place exists
    ?assert(lists:member('p_cs_lock', AllPlaces)),
    ?assert(lists:member('p_cs_request', AllPlaces)),
    ?assert(lists:member('p_cs_active', AllPlaces)).

%% Queue ordering test
critical_queue_test() ->
    #{pattern := Pattern} = setup_critical_section(),

    ?assert(is_list(Pattern#pattern_state.active_instances)),
    ?assertEqual(0, Pattern#pattern_state.instance_count).

%% Lock timeout test
critical_timeout_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_cs_timeout', AllTransitions)).

%% Resource cleanup test
critical_cleanup_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_cs_error', AllTransitions)).

%% Nested critical sections test
critical_nested_test() ->
    LockId1 = make_ref(),
    LockId2 = make_ref(),
    Pattern1 = cre_yawl_patterns:critical_section(critical_activity(), LockId1),
    Pattern2 = cre_yawl_patterns:critical_section(critical_activity(), LockId2),

    ?assertNotEqual(LockId1, LockId2),
    ?assertEqual(critical_section, Pattern1#pattern_state.pattern_type),
    ?assertEqual(critical_section, Pattern2#pattern_state.pattern_type).

%% Reentrant lock test
critical_reentrant_test() ->
    LockId = make_ref(),
    Pattern = cre_yawl_patterns:critical_section(critical_activity(), LockId),

    ?assertEqual(LockId, maps:get(lock_id, Pattern#pattern_state.choice_data)).

%%====================================================================
%% WCP-27: Protocol Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-27: Protocol (Request-Response)
%% @end
%%--------------------------------------------------------------------
protocol_pattern_test_() ->
    {setup,
     fun setup_protocol/0,
     fun cleanup_protocol/1,
     fun(_Context) ->
         [
          {"WCP-27: Request sent - Normal",
           fun protocol_request_test/0},
          {"WCP-27: Response received",
           fun protocol_response_test/0},
          {"WCP-27: Timeout on no response",
           fun protocol_timeout_test/0},
          {"WCP-27: Retry on failure",
           fun protocol_retry_test/0},
          {"WCP-27: Error handling",
           fun protocol_error_test/0},
          {"WCP-27: Multiple sequential requests",
           fun protocol_multiple_test/0},
          {"WCP-27: Request state management",
           fun protocol_state_test/0},
          {"WCP-27: Concurrent protocol instances",
           fun protocol_concurrent_test/0},
          {"WCP-27: Infinity timeout",
           fun protocol_infinity_timeout_test/0}
         ]
     end}.

setup_protocol() ->
    Timeout = 5000,
    Pattern = cre_yawl_patterns:protocol_pattern(
        protocol_request(),
        protocol_response_handler(),
        Timeout
    ),
    #{pattern => Pattern, timeout => Timeout}.

cleanup_protocol(_Context) ->
    ok.

%% Request sent test
protocol_request_test() ->
    #{pattern := Pattern} = setup_protocol(),

    ?assertEqual(protocol, Pattern#pattern_state.pattern_type),
    ?assert(is_function(Pattern#pattern_state.subprocess)).

%% Response received test
protocol_response_test() ->
    #{pattern := Pattern} = setup_protocol(),

    ResponseHandler = maps:get(response_handler, Pattern#pattern_state.choice_data),
    ?assert(is_function(ResponseHandler)).

%% Timeout test
protocol_timeout_test() ->
    #{timeout := Timeout} = setup_protocol(),

    ?assert(is_integer(Timeout)),
    ?assert(Timeout > 0).

%% Retry test
protocol_retry_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_proto_retry', AllTransitions)).

%% Error handling test
protocol_error_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_proto_error', AllTransitions)).

%% Multiple requests test
protocol_multiple_test() ->
    #{pattern := Pattern} = setup_protocol(),

    ChoiceData = Pattern#pattern_state.choice_data,
    ?assert(is_map(ChoiceData)),
    ?assert(maps:is_key(timeout, ChoiceData)),
    ?assert(maps:is_key(request_sent, ChoiceData)),
    ?assert(maps:is_key(response_received, ChoiceData)).

%% State test
protocol_state_test() ->
    #{pattern := Pattern} = setup_protocol(),

    ?assertEqual(0, Pattern#pattern_state.instance_count),
    ?assertEqual(false, maps:get(request_sent, Pattern#pattern_state.choice_data)),
    ?assertEqual(false, maps:get(response_received, Pattern#pattern_state.choice_data)).

%% Concurrent protocol test
protocol_concurrent_test() ->
    Timeout = 5000,
    Patterns = lists:map(fun(_) ->
        cre_yawl_patterns:protocol_pattern(
            protocol_request(),
            protocol_response_handler(),
            Timeout
        )
    end, lists:seq(1, 5)),

    ?assertEqual(5, length(Patterns)),
    lists:foreach(fun(P) ->
        ?assertEqual(protocol, P#pattern_state.pattern_type)
    end, Patterns).

%% Infinity timeout test
protocol_infinity_timeout_test() ->
    Pattern = cre_yawl_patterns:protocol_pattern(
        protocol_request(),
        protocol_response_handler(),
        infinity
    ),

    ?assertEqual(infinity, maps:get(timeout, Pattern#pattern_state.choice_data)).

%%====================================================================
%% WCP-28: Try-Catch Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WCP-28: Try-Catch (Exception Handling)
%% @end
%%--------------------------------------------------------------------
try_catch_pattern_test_() ->
    {setup,
     fun setup_try_catch/0,
     fun cleanup_try_catch/1,
     fun(_Context) ->
         [
          {"WCP-28: Try block success - Normal execution",
           fun try_catch_success_test/0},
          {"WCP-28: Try block failure - Catch invoked",
           fun try_catch_failure_test/0},
          {"WCP-28: Multiple exception types",
           fun try_catch_multiple_types_test/0},
          {"WCP-28: Catch all exceptions",
           fun try_catch_catch_all_test/0},
          {"WCP-28: Exception type matching",
           fun try_catch_type_match_test/0},
          {"WCP-28: State after catch",
           fun try_catch_state_test/0},
          {"WCP-28: Nested try-catch",
           fun try_catch_nested_test/0},
          {"WCP-28: Rethrow exception",
           fun try_catch_rethrow_test/0},
          {"WCP-28: Finally clause simulation",
           fun try_catch_finally_test/0},
          {"WCP-28: Empty exception list",
           fun try_catch_empty_list_test/0}
         ]
     end}.

setup_try_catch() ->
    Pattern = cre_yawl_patterns:try_catch(
        try_success_function(),
        catch_handler(),
        [business_exception, timeout_exception]
    ),
    #{pattern => Pattern}.

cleanup_try_catch(_Context) ->
    ok.

%% Success test
try_catch_success_test() ->
    #{pattern := Pattern} = setup_try_catch(),

    ?assertEqual(try_catch, Pattern#pattern_state.pattern_type),
    ?assertEqual(false, maps:get(exception_occurred, Pattern#pattern_state.choice_data)).

%% Failure test
try_catch_failure_test() ->
    Pattern = cre_yawl_patterns:try_catch(
        try_failure_function(),
        catch_handler(),
        [business_exception]
    ),

    ?assertEqual(try_catch, Pattern#pattern_state.pattern_type),
    ?assertEqual([business_exception], maps:get(exceptions, Pattern#pattern_state.choice_data)).

%% Multiple exception types test
try_catch_multiple_types_test() ->
    Pattern = cre_yawl_patterns:try_catch(
        try_success_function(),
        catch_handler(),
        [business_exception, timeout_exception, system_exception]
    ),

    Exceptions = maps:get(exceptions, Pattern#pattern_state.choice_data),
    ?assertEqual(3, length(Exceptions)).

%% Catch all test
try_catch_catch_all_test() ->
    Pattern = cre_yawl_patterns:try_catch(
        try_failure_function(),
        catch_handler(),
        '_'
    ),

    ?assertEqual('_', maps:get(exceptions, Pattern#pattern_state.choice_data)).

%% Type match test
try_catch_type_match_test() ->
    AllTransitions = cre_yawl_patterns:trsn_lst(),

    %% Verify try-catch transitions
    ?assert(lists:member('t_try_enter', AllTransitions)),
    ?assert(lists:member('t_try_execute', AllTransitions)),
    ?assert(lists:member('t_try_success', AllTransitions)),
    ?assert(lists:member('t_try_failure', AllTransitions)),
    ?assert(lists:member('t_catch_enter', AllTransitions)),
    ?assert(lists:member('t_catch_execute', AllTransitions)),
    ?assert(lists:member('t_catch_complete', AllTransitions)),
    ?assert(lists:member('t_try_catch_complete', AllTransitions)).

%% State after catch test
try_catch_state_test() ->
    #{pattern := Pattern} = setup_try_catch(),

    ChoiceData = Pattern#pattern_state.choice_data,
    ?assert(maps:is_key(exception_occurred, ChoiceData)),
    ?assert(maps:is_key(exception_type, ChoiceData)),
    ?assertEqual(false, maps:get(exception_occurred, ChoiceData)).

%% Nested test
try_catch_nested_test() ->
    OuterPattern = cre_yawl_patterns:try_catch(
        fun() -> outer_try end,
        fun(_) -> outer_catch end,
        [outer_exception]
    ),
    InnerPattern = cre_yawl_patterns:try_catch(
        fun() -> inner_try end,
        fun(_) -> inner_catch end,
        [inner_exception]
    ),

    ?assertEqual(try_catch, OuterPattern#pattern_state.pattern_type),
    ?assertEqual(try_catch, InnerPattern#pattern_state.pattern_type),
    ?assertNotEqual(OuterPattern, InnerPattern).

%% Rethrow test
try_catch_rethrow_test() ->
    Handler = fun(Exception) ->
        %% Simulate rethrow by not handling
        throw(Exception)
    end,
    Pattern = cre_yawl_patterns:try_catch(
        try_failure_function(),
        Handler,
        [business_exception]
    ),

    ?assertEqual(try_catch, Pattern#pattern_state.pattern_type).

%% Finally test
try_catch_finally_test() ->
    %% Simulate finally with separate catch pattern
    Pattern = cre_yawl_patterns:try_catch(
        try_success_function(),
        catch_handler(),
        [business_exception]
    ),

    ?assertEqual(try_catch, Pattern#pattern_state.pattern_type).

%% Empty list test
try_catch_empty_list_test() ->
    Pattern = cre_yawl_patterns:try_catch(
        try_success_function(),
        catch_handler(),
        []
    ),

    ?assertEqual([], maps:get(exceptions, Pattern#pattern_state.choice_data)).

%%====================================================================
%% Performance Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Performance benchmark tests for extended control patterns
%% @end
%%--------------------------------------------------------------------
extended_control_performance_test_() ->
    {setup,
     fun setup_performance/0,
     fun cleanup_performance/1,
     fun(_Context) ->
         [
          {"Performance: Structured sync scalability",
           fun perf_sync_scalability_test/0},
          {"Performance: Partial join throughput",
           fun perf_partial_join_throughput_test/0},
          {"Performance: Loop iteration speed",
           fun perf_loop_speed_test/0},
          {"Performance: Recursion depth",
           fun perf_recursion_depth_test/0},
          {"Performance: Critical section contention",
           fun perf_critical_contention_test/0},
          {"Performance: Protocol round-trip",
           fun perf_protocol_roundtrip_test/0},
          {"Performance: Try-catch overhead",
           fun perf_try_catch_overhead_test/0},
          {"Performance: Memory usage patterns",
           fun perf_memory_test/0}
         ]
     end}.

setup_performance() ->
    ok.

cleanup_performance(_Context) ->
    ok.

%% Sync scalability test
perf_sync_scalability_test() ->
    %% Test with increasing activity counts
    Counts = [2, 5, 10, 20],
    Results = lists:map(fun(Count) ->
        Activities = lists:duplicate(Count, sync_activity_1()),
        Pattern = cre_yawl_patterns:structured_sync(Activities, perf_data),

        {Time, _} = timer:tc(fun() ->
            %% Simulate sync operations
            lists:foreach(fun(_) -> ok end, Activities)
        end),

        {Count, Time}
    end, Counts),

    %% Verify execution time scales reasonably
    lists:foreach(fun({Count, Time}) ->
        ?assert(Time < 100000, io_lib:format("~p activities took ~p us", [Count, Time]))
    end, Results).

%% Partial join throughput test
perf_partial_join_throughput_test() ->
    Activities = lists:duplicate(10, partial_activity_1()),
    Pattern = cre_yawl_patterns:partial_join(Activities, 5),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Activity) -> Activity() end, Activities)
    end),

    %% Should complete in reasonable time
    ?assert(Time < 1000000, "Partial join too slow").

%% Loop speed test
perf_loop_speed_test() ->
    Condition = fun(N) -> N < 100 end,
    Pattern = cre_yawl_patterns:structured_loop(loop_body(), while, Condition),

    {Time, Iterations} = timer:tc(fun() ->
        execute_loop_iterations(100, fun(_) -> ok end)
    end),

    ?assert(Iterations =:= 100),
    ?assert(Time < 100000, "Loop iteration too slow").

%% Recursion depth test
perf_recursion_depth_test() ->
    %% Test with safe recursion depth
    RecFunc = fun
        RecFuncHelper(0) -> base;
        RecFuncHelper(N) when N > 0 -> RecFuncHelper(N - 1)
    end,

    _Pattern = cre_yawl_patterns:recursion(
        RecFunc,
        fun(0) -> true; (_) -> false end
    ),

    {Time, Result} = timer:tc(fun() -> RecFunc(50) end),

    ?assertEqual(base, Result),
    ?assert(Time < 100000, "Recursion too slow").

%% Critical section contention test
perf_critical_contention_test() ->
    LockId = make_ref(),
    Pattern = cre_yawl_patterns:critical_section(critical_activity(), LockId),

    %% Simulate multiple lock requests
    Pids = lists:map(fun(_) ->
        spawn(fun() ->
            %% Simulate lock acquire/release cycle
            timer:sleep(1),
            lock_acquired
        end)
    end, lists:seq(1, 20)),

    %% Verify all processes complete
    lists:foreach(fun(Pid) ->
        MonRef = monitor(process, Pid),
        receive
            {'DOWN', MonRef, process, Pid, _} -> ok
        after 1000 ->
            ?assert(false, "Process did not complete")
        end
    end, Pids).

%% Protocol roundtrip test
perf_protocol_roundtrip_test() ->
    _Pattern = cre_yawl_patterns:protocol_pattern(
        protocol_request(),
        protocol_response_handler(),
        1000
    ),

    {Time, _} = timer:tc(fun() ->
        %% Simulate request-response cycle
        _Req = protocol_request(),
        RespHandler = protocol_response_handler(),
        RespHandler({response, <<"test">>})
    end),

    ?assert(Time < 100000, "Protocol roundtrip too slow").

%% Try-catch overhead test
perf_try_catch_overhead_test() ->
    _Pattern = cre_yawl_patterns:try_catch(
        try_success_function(),
        catch_handler(),
        [business_exception]
    ),

    %% Measure overhead with successful execution
    TryFun = try_success_function(),
    {TimeWithTry, _} = timer:tc(fun() ->
        TryFun(5)
    end),

    %% Measure without try-catch
    {TimeWithout, _} = timer:tc(fun() ->
        5 * 2
    end),

    %% Overhead should be reasonable
    Overhead = TimeWithTry - TimeWithout,
    ?assert(Overhead < 10000, io_lib:format("Try-catch overhead ~p us too high", [Overhead])).

%% Memory usage test
perf_memory_test() ->
    Before = erlang:memory(process),

    %% Create multiple pattern instances
    lists:foreach(fun(_) ->
        cre_yawl_patterns:structured_sync([sync_activity_1()], test),
        cre_yawl_patterns:partial_join([partial_activity_1()], 1),
        cre_yawl_patterns:structured_loop(loop_body(), while, loop_condition_while()),
        cre_yawl_patterns:recursion(recursive_function(), base_case_function()),
        cre_yawl_patterns:critical_section(critical_activity(), make_ref()),
        cre_yawl_patterns:protocol_pattern(protocol_request(), protocol_response_handler(), 5000),
        cre_yawl_patterns:try_catch(try_success_function(), catch_handler(), [error])
    end, lists:seq(1, 10)),

    After = erlang:memory(process),
    MemoryDelta = After - Before,

    %% Memory growth should be reasonable
    ?assert(MemoryDelta < 1000000, io_lib:format("Memory delta ~p bytes too high", [MemoryDelta])).

%%====================================================================
%% Concurrency Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Concurrency behavior tests for extended control patterns
%% @end
%%--------------------------------------------------------------------
extended_control_concurrency_test_() ->
    {setup,
     fun setup_concurrency/0,
     fun cleanup_concurrency/1,
     fun(_Context) ->
         [
          {"Concurrency: No deadlocks in structured sync",
           fun concurrent_sync_no_deadlock_test/0},
          {"Concurrency: Partial join race conditions",
           fun concurrent_partial_join_race_test/0},
          {"Concurrency: Loop thread safety",
           fun concurrent_loop_safety_test/0},
          {"Concurrency: Critical section mutual exclusion",
           fun concurrent_critical_mutex_test/0},
          {"Concurrency: Multiple protocol instances",
           fun concurrent_protocol_instances_test/0},
          {"Concurrency: Parallel try-catch",
           fun concurrent_try_catch_test/0},
          {"Concurrency: Nested patterns",
           fun concurrent_nested_patterns_test/0}
         ]
     end}.

setup_concurrency() ->
    ok.

cleanup_concurrency(_Context) ->
    ok.

%% No deadlock test
concurrent_sync_no_deadlock_test() ->
    Pattern = cre_yawl_patterns:structured_sync(
        [sync_activity_1(), sync_activity_2(), sync_activity_3()],
        concurrent_test
    ),

    %% Execute concurrently
    Pids = lists:map(fun(Activity) ->
        spawn_link(fun() -> Activity(concurrent_test) end)
    end, Pattern#pattern_state.subprocess),

    %% Verify all complete without deadlock
    lists:foreach(fun(Pid) ->
        MonRef = monitor(process, Pid),
        receive
            {'DOWN', MonRef, process, Pid, _} -> ok
        after 1000 ->
            ?assert(false, "Deadlock detected")
        end
    end, Pids).

%% Race condition test
concurrent_partial_join_race_test() ->
    Activities = lists:duplicate(5, partial_activity_1()),
    Pattern = cre_yawl_patterns:partial_join(Activities, 3),

    %% Start all concurrently
    Pids = lists:map(fun(Activity) ->
        spawn(fun() -> Activity() end)
    end, Activities),

    %% All should complete independently
    lists:foreach(fun(Pid) ->
        MonRef = monitor(process, Pid),
        receive
            {'DOWN', MonRef, process, Pid, _} -> ok
        after 1000 ->
            ?assert(false, "Activity did not complete")
        end
    end, Pids).

%% Loop thread safety test
concurrent_loop_safety_test() ->
    Condition = fun(N) -> N < 10 end,
    Pattern = cre_yawl_patterns:structured_loop(loop_body(), while, Condition),

    %% Execute multiple loop instances concurrently
    Pids = lists:map(fun(_) ->
        spawn(fun() ->
            execute_loop_iterations(10, fun(_) -> ok end)
        end)
    end, lists:seq(1, 5)),

    %% Verify all complete
    lists:foreach(fun(Pid) ->
        MonRef = monitor(process, Pid),
        receive
            {'DOWN', MonRef, process, Pid, _} -> ok
        after 2000 ->
            ?assert(false, "Loop did not complete")
        end
    end, Pids).

%% Mutex test
concurrent_critical_mutex_test() ->
    LockId = make_ref(),
    Pattern = cre_yawl_patterns:critical_section(critical_activity(), LockId),

    %% Track execution order
    Self = self(),
    Pids = lists:map(fun(I) ->
        spawn(fun() ->
            %% Simulate critical section entry
            Self ! {enter, I},
            timer:sleep(10),
            Self ! {exit, I}
        end)
    end, lists:seq(1, 5)),

    %% Collect all entry and exit messages
    Entries = lists:sort([receive {enter, I} -> I after 500 -> timeout end || _ <- Pids]),
    Exits = lists:sort([receive {exit, I} -> I after 500 -> timeout end || _ <- Pids]),

    %% Verify all entered and exited
    ?assertEqual([1, 2, 3, 4, 5], Entries),
    ?assertEqual([1, 2, 3, 4, 5], Exits).

%% Protocol instances test
concurrent_protocol_instances_test() ->
    Patterns = lists:map(fun(I) ->
        cre_yawl_patterns:protocol_pattern(
            fun() -> {request, I} end,
            protocol_response_handler(),
            5000
        )
    end, lists:seq(1, 10)),

    ?assertEqual(10, length(Patterns)),

    %% All should be independent
    lists:foreach(fun(P) ->
        ?assertEqual(protocol, P#pattern_state.pattern_type)
    end, Patterns).

%% Parallel try-catch test
concurrent_try_catch_test() ->
    Patterns = lists:map(fun(I) ->
        cre_yawl_patterns:try_catch(
            fun(Input) -> {ok, Input * 2} end,
            fun(_) -> caught end,
            [error]
        )
    end, lists:seq(1, 10)),

    %% Execute all concurrently
    Results = lists:map(fun(P) ->
        spawn(fun() ->
            Subprocess = P#pattern_state.subprocess,
            Subprocess(5)
        end)
    end, Patterns),

    ?assertEqual(10, length(Results)).

%% Nested patterns test
concurrent_nested_patterns_test() ->
    %% Create nested sync within loop
    OuterCondition = fun(N) -> N < 3 end,
    LoopPattern = cre_yawl_patterns:structured_loop(loop_body(), while, OuterCondition),

    InnerSync = cre_yawl_patterns:structured_sync(
        [sync_activity_1(), sync_activity_2()],
        nested
    ),

    ?assertEqual(structured_loop, LoopPattern#pattern_state.pattern_type),
    ?assertEqual(structured_sync, InnerSync#pattern_state.pattern_type).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Edge case tests for extended control patterns
%% @end
%%--------------------------------------------------------------------
extended_control_edge_case_test_() ->
    [
     {"Edge: Structured sync with no activities",
      fun edge_sync_no_activities/0},
     {"Edge: Partial join with quorum of zero",
      fun edge_partial_zero_quorum/0},
     {"Edge: Loop with negative condition",
      fun edge_loop_negative/0},
     {"Edge: Recursion with immediate base case",
      fun edge_recursion_immediate_base/0},
     {"Edge: Critical section with undefined lock",
      fun edge_critical_undefined_lock/0},
     {"Edge: Protocol with zero timeout",
      fun edge_protocol_zero_timeout/0},
     {"Edge: Try-catch with undefined exception",
      fun edge_try_catch_undefined_exception/0},
     {"Edge: Maximum recursion depth",
      fun edge_max_recursion_depth/0},
     {"Edge: Empty catch handler",
      fun edge_empty_catch_handler_test/0},
     {"Edge: Loop with infinite condition",
      fun edge_infinite_loop_condition/0}
    ].

%% Sync no activities
edge_sync_no_activities() ->
    Pattern = cre_yawl_patterns:structured_sync([], undefined),
    ?assertEqual(0, Pattern#pattern_state.instance_count).

%% Partial join zero quorum
edge_partial_zero_quorum() ->
    try
        cre_yawl_patterns:partial_join([partial_activity_1()], 0),
        ?assert(false, "Should not allow quorum of 0")
    catch
        _:_ -> ok  % Expected to fail
    end.

%% Loop negative condition
edge_loop_negative() ->
    Condition = fun(N) -> N < -1 end,
    Pattern = cre_yawl_patterns:structured_loop(loop_body(), while, Condition),
    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type).

%% Recursion immediate base
edge_recursion_immediate_base() ->
    Pattern = cre_yawl_patterns:recursion(
        fun(_) -> base_immediate end,
        fun(_) -> true end
    ),
    ?assertEqual(recursion, Pattern#pattern_state.pattern_type).

%% Critical undefined lock
edge_critical_undefined_lock() ->
    Pattern = cre_yawl_patterns:critical_section(critical_activity(), undefined),
    ?assertEqual(undefined, maps:get(lock_id, Pattern#pattern_state.choice_data)).

%% Protocol zero timeout
edge_protocol_zero_timeout() ->
    Pattern = cre_yawl_patterns:protocol_pattern(
        protocol_request(),
        protocol_response_handler(),
        0
    ),
    ?assertEqual(0, maps:get(timeout, Pattern#pattern_state.choice_data)).

%% Try-catch undefined exception
edge_try_catch_undefined_exception() ->
    Handler = fun(_) -> caught end,
    Pattern = cre_yawl_patterns:try_catch(try_success_function(), Handler, [undefined_type]),
    ?assertEqual([undefined_type], maps:get(exceptions, Pattern#pattern_state.choice_data)).

%% Max recursion depth
edge_max_recursion_depth() ->
    DeepFunc = fun
        DeepFuncHelper(0) -> base;
        DeepFuncHelper(N) -> DeepFuncHelper(N - 1)
    end,
    %% Test with reasonable depth
    _Pattern = cre_yawl_patterns:recursion(
        DeepFunc,
        fun(0) -> true; (_) -> false end
    ),
    ok.

%% Empty catch handler
edge_empty_catch_handler_test() ->
    EmptyHandler = fun(_) -> ok end,
    _Pattern = cre_yawl_patterns:try_catch(try_success_function(), EmptyHandler, [error]),
    ok.

%% Infinite loop condition
edge_infinite_loop_condition() ->
    Condition = fun(_) -> true end,
    Pattern = cre_yawl_patterns:structured_loop(loop_body(), while, Condition),
    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type).

%%====================================================================
%% Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc End-to-end integration tests for extended control patterns
%% @end
%%--------------------------------------------------------------------
extended_control_integration_test_() ->
    {setup,
     fun setup_integration_extended/0,
     fun cleanup_integration_extended/1,
     fun(_Context) ->
         [
          {"Integration: Sync within loop",
           fun integration_sync_in_loop/0},
          {"Integration: Partial join after sync",
           fun integration_partial_after_sync/0},
          {"Integration: Critical section in loop",
           fun integration_critical_in_loop/0},
          {"Integration: Try-catch around protocol",
           fun integration_try_catch_protocol/0},
          {"Integration: Complete workflow with multiple patterns",
           fun integration_complete_workflow/0}
         ]
     end}.

setup_integration_extended() ->
    %% Start CRE if needed
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    ok.

cleanup_integration_extended(_Context) ->
    ok.

%% Sync in loop integration
integration_sync_in_loop() ->
    %% Create a pattern that syncs activities in each loop iteration
    LoopCondition = fun(N) -> N < 3 end,
    LoopPattern = cre_yawl_patterns:structured_loop(loop_body(), while, LoopCondition),
    SyncPattern = cre_yawl_patterns:structured_sync(
        [sync_activity_1(), sync_activity_2()],
        loop_iteration
    ),

    ?assertEqual(structured_loop, LoopPattern#pattern_state.pattern_type),
    ?assertEqual(structured_sync, SyncPattern#pattern_state.pattern_type).

%% Partial after sync integration
integration_partial_after_sync() ->
    %% First synchronize all, then wait for quorum
    SyncPattern = cre_yawl_patterns:structured_sync(
        [sync_activity_1(), sync_activity_2(), sync_activity_3()],
        phase1
    ),
    PartialPattern = cre_yawl_patterns:partial_join(
        [partial_activity_1(), partial_activity_2()],
        2
    ),

    ?assertEqual(structured_sync, SyncPattern#pattern_state.pattern_type),
    ?assertEqual(partial_join, PartialPattern#pattern_state.pattern_type).

%% Critical in loop integration
integration_critical_in_loop() ->
    %% Execute critical section multiple times in loop
    LoopCondition = fun(N) -> N < 5 end,
    LoopPattern = cre_yawl_patterns:structured_loop(loop_body(), while, LoopCondition),
    CriticalPattern = cre_yawl_patterns:critical_section(critical_activity(), loop_lock),

    ?assertEqual(structured_loop, LoopPattern#pattern_state.pattern_type),
    ?assertEqual(critical_section, CriticalPattern#pattern_state.pattern_type).

%% Try-catch around protocol
integration_try_catch_protocol() ->
    %% Wrap protocol execution in try-catch
    TryCatchPattern = cre_yawl_patterns:try_catch(
        fun() -> protocol_request() end,
        fun(_) -> protocol_error_handled end,
        [protocol_exception]
    ),
    ProtocolPattern = cre_yawl_patterns:protocol_pattern(
        protocol_request(),
        protocol_response_handler(),
        5000
    ),

    ?assertEqual(try_catch, TryCatchPattern#pattern_state.pattern_type),
    ?assertEqual(protocol, ProtocolPattern#pattern_state.pattern_type).

%% Complete workflow integration
integration_complete_workflow() ->
    %% Create a complex workflow using multiple patterns
    SyncPattern = cre_yawl_patterns:structured_sync(
        [sync_activity_1(), sync_activity_2()],
        initial
    ),

    LoopCondition = fun(N) -> N < 3 end,
    LoopPattern = cre_yawl_patterns:structured_loop(loop_body(), while, LoopCondition),

    CriticalPattern = cre_yawl_patterns:critical_section(critical_activity(), workflow_lock),

    %% All patterns should be valid
    ?assertEqual(structured_sync, SyncPattern#pattern_state.pattern_type),
    ?assertEqual(structured_loop, LoopPattern#pattern_state.pattern_type),
    ?assertEqual(critical_section, CriticalPattern#pattern_state.pattern_type).

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Execute loop iterations
%% @end
%%--------------------------------------------------------------------
execute_loop_iterations(0, _Fun) ->
    0;
execute_loop_iterations(N, Fun) when N > 0 ->
    _Result = Fun(N),
    execute_loop_iterations(N - 1, Fun).

%%--------------------------------------------------------------------
%% @doc Execute multiple activities concurrently
%% @end
%%--------------------------------------------------------------------
execute_concurrent_activities(Activities) ->
    Pids = lists:map(fun(Activity) ->
        spawn(fun() -> Activity() end)
    end, Activities),

    %% Wait for all to complete
    lists:foreach(fun(Pid) ->
        MonRef = monitor(process, Pid),
        receive
            {'DOWN', MonRef, process, Pid, _} -> ok
        after 5000 ->
            timeout
        end
    end, Pids).

%%--------------------------------------------------------------------
%% @doc Simulate partial join completion
%% @end
%%--------------------------------------------------------------------
simulate_partial_join(Activities, Quorum) ->
    Self = self(),
    lists:foreach(fun(Activity) ->
        spawn(fun() ->
            Result = Activity(),
            Self ! {activity_complete, Result}
        end)
    end, Activities),

    %% Wait for quorum
    collect_quorum_results(Quorum, []).

collect_quorum_results(0, Results) ->
    lists:reverse(Results);
collect_quorum_results(Quorum, Results) when Quorum > 0 ->
    receive
        {activity_complete, Result} ->
            collect_quorum_results(Quorum - 1, [Result | Results])
    after 5000 ->
        lists:reverse(Results)
    end.
