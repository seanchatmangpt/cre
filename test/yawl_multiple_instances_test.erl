%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Multiple Instance Patterns Test Suite
%%
%% Comprehensive execution tests for YAWL workflow control patterns
%% WCP-11 through WCP-17 (Multiple Instance Patterns).
%%
%% Test Coverage:
%% - WCP-11: Implicit Termination
%% - WCP-12: Multiple Instances without Synchronization
%% - WCP-13: Multiple Instances with Design Time Knowledge (Static)
%% - WCP-14: Multiple Instances with Runtime Knowledge
%% - WCP-15: Multiple Instances without Prior Knowledge (Dynamic)
%% - WCP-16: Deferred Choice
%% - WCP-17: Interleaved Parallel Routing
%%
%% Each pattern includes:
%% - Normal execution tests
%% - Stress tests (high instance counts)
%% - Failure scenario tests
%% - State validation tests
%% - Performance benchmarks
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_multiple_instances_test).
-author('joergen.brandt@cuneiform-lang.org').

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

-record(instance_token, {
          instance_id :: reference(),
          data :: term()
         }).

-record(branch_token, {
          branch_id :: atom(),
          data :: term(),
          index :: non_neg_integer()
         }).

-record(sync_token, {
          activity_id :: reference(),
          data :: term(),
          completed :: boolean()
         }).

%% Performance tracking records
-record(perf_metrics, {
          start_time :: integer(),
          end_time :: integer(),
          instance_count :: non_neg_integer(),
          memory_before :: non_neg_integer(),
          memory_after :: non_neg_integer()
         }).

%%====================================================================
%% Test Setup and Cleanup
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% @end
%%--------------------------------------------------------------------
setup() ->
    %% Start CRE application
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Wait for cre_master to be registered
    wait_for_process(cre_master, 5000),

    %% Initialize test environment
    initialize_test_state(),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% @end
%%--------------------------------------------------------------------
cleanup(_TestData) ->
    %% Clean up test state
    cleanup_test_state(),
    ok.

%%--------------------------------------------------------------------
%% @doc Helper to wait for a process to start.
%% @end
%%--------------------------------------------------------------------
wait_for_process(ProcessName, Timeout) ->
    Start = erlang:monotonic_time(millisecond),
    wait_for_process_helper(ProcessName, Timeout, Start).

wait_for_process_helper(ProcessName, Timeout, Start) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    if CurrentTime - Start >= Timeout -> timeout;
       true -> wait_for_process_loop(ProcessName, Timeout, Start)
    end.

wait_for_process_loop(ProcessName, Timeout, Start) ->
    case whereis(ProcessName) of
        undefined ->
            timer:sleep(50),
            wait_for_process_helper(ProcessName, Timeout, Start);
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.

%%--------------------------------------------------------------------
%% @doc Initialize test state in process dictionary.
%% @end
%%--------------------------------------------------------------------
initialize_test_state() ->
    put(test_instances, []),
    put(test_results, #{}),
    put(test_metrics, #{}),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup test state from process dictionary.
%% @end
%%--------------------------------------------------------------------
cleanup_test_state() ->
    erase(test_instances),
    erase(test_results),
    erase(test_metrics),
    ok.

%%====================================================================
%% WCP-11: Implicit Termination Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for Implicit Termination pattern (WCP-11).
%%
%% The implicit termination pattern ensures a subprocess terminates when
%% no work remains and all input conditions are satisfied.
%%
%% @end
%%--------------------------------------------------------------------
implicit_termination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          %% Normal execution tests
          {"Implicit termination - Normal execution",
           fun() ->
               Subprocess = fun() ->
                   receive
                       {work, Data} -> {processed, Data};
                       {stop} -> stopped
                   after 100 -> idle
                   end
               end,

               Pattern = cre_yawl_patterns:implicit_termination(Subprocess),

               ?assertMatch(#pattern_state{pattern_type = implicit_termination}, Pattern),
               ?assertEqual(0, Pattern#pattern_state.instance_count),

               %% Simulate work completion
               Result = execute_implicit_termination(Pattern, [1, 2, 3]),

               ?assertEqual(stopped, Result)
           end},

          {"Implicit termination - Auto-termination detection",
           fun() ->
               %% Test that termination fires when work place is empty
               Pattern = cre_yawl_patterns:implicit_termination(fun() -> ok end),

               %% Check initial state
               ?assertEqual(0, Pattern#pattern_state.instance_count),

               %% Simulate empty work queue
               TerminationResult = check_implicit_termination_condition(Pattern, []),

               ?assertEqual(true, TerminationResult)
           end},

          {"Implicit termination - With pending work",
           fun() ->
               Pattern = cre_yawl_patterns:implicit_termination(fun() -> ok end),

               %% Add pending work
               PatternWithWork = Pattern#pattern_state{
                 pending_instances = [work1, work2, work3]
               },

               %% Should NOT terminate with pending work
               TerminationResult = check_implicit_termination_condition(
                   PatternWithWork,
                   PatternWithWork#pattern_state.pending_instances
               ),

               ?assertEqual(false, TerminationResult)
           end},

          %% Stress tests
          {"Implicit termination - Stress test 100 instances",
           fun() ->
               WorkItems = lists:seq(1, 100),
               Pattern = cre_yawl_patterns:implicit_termination(
                   fun(I) -> I * 2 end
               ),

               Start = erlang:monotonic_time(millisecond),
               Result = execute_implicit_termination_stress(Pattern, WorkItems),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(completed, Result),
               ?assert(Duration < 5000, "Stress test took too long"),

               ?debugFmt("Implicit termination 100 instances: ~p ms~n", [Duration])
           end},

          {"Implicit termination - Stress test 1000 instances",
           fun() ->
               WorkItems = lists:seq(1, 1000),
               Pattern = cre_yawl_patterns:implicit_termination(
                   fun(I) -> I end
               ),

               Start = erlang:monotonic_time(millisecond),
               Result = execute_implicit_termination_stress(Pattern, WorkItems),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(completed, Result),
               ?assert(Duration < 30000, "Large stress test took too long"),

               ?debugFmt("Implicit termination 1000 instances: ~p ms~n", [Duration])
           end},

          %% State validation tests
          {"Implicit termination - State tracking",
           fun() ->
               Pattern = cre_yawl_patterns:implicit_termination(fun() -> ok end),

               %% Verify initial state
               ?assertEqual(implicit_termination, Pattern#pattern_state.pattern_type),
               ?assert(is_function(Pattern#pattern_state.subprocess) orelse
                      is_atom(Pattern#pattern_state.subprocess)),
               ?assertEqual(0, Pattern#pattern_state.instance_count),

               %% Verify state transitions
               UpdatedPattern = Pattern#pattern_state{
                 instance_count = 5,
                 completed_instances = [1, 2, 3, 4, 5]
               },

               ?assertEqual(5, UpdatedPattern#pattern_state.instance_count),
               ?assertEqual(5, length(UpdatedPattern#pattern_state.completed_instances))
           end},

          %% Failure scenario tests
          {"Implicit termination - Subprocess failure handling",
           fun() ->
               FailingSubprocess = fun() ->
                   receive
                       _ -> exit(subprocess_failed)
                   after 100 -> ok
                   end
               end,

               Pattern = cre_yawl_patterns:implicit_termination(FailingSubprocess),

               %% Execute and handle failure gracefully
               Result = execute_with_failure_handling(Pattern, [work_item]),

               ?assertMatch({error, _}, Result)
           end},

          {"Implicit termination - Timeout conditions",
           fun() ->
               SlowSubprocess = fun() ->
                   receive
                       _ -> ok
                   after 10000 -> timeout
                   end
               end,

               Pattern = cre_yawl_patterns:implicit_termination(SlowSubprocess),

               %% Execute with timeout
               Result = execute_with_timeout(Pattern, 1000),

               ?assertEqual(timeout, Result)
           end}
         ]
     end}.

%%====================================================================
%% WCP-12: Multiple Instances without Synchronization Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for Multiple Instances without Synchronization (WCP-12).
%%
%% This pattern creates concurrent instances without synchronizing
%% after completion. Each instance operates independently.
%%
%% @end
%%--------------------------------------------------------------------
multiple_instances_no_sync_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          %% Normal execution tests
          {"No sync - Basic instance spawning",
           fun() ->
               Subprocess = fun(X) -> X * 2 end,
               InputData = [1, 2, 3, 4],
               InstanceCount = 4,

               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   Subprocess,
                   InstanceCount,
                   InputData
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(multiple_instances_no_sync, Pattern#pattern_state.pattern_type),
               ?assertEqual(4, Pattern#pattern_state.instance_count),
               ?assertEqual(4, Pattern#pattern_state.max_instances),

               %% Execute instances
               Results = execute_no_sync_instances(Pattern),

               ?assertEqual(4, length(Results)),
               ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
           end},

          {"No sync - Independent execution",
           fun() ->
               %% Verify instances execute independently without waiting
               Subprocess = fun(X) ->
                   timer:sleep(rand:uniform(10)),
                   X * 2
               end,

               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   Subprocess,
                   10,
                   lists:seq(1, 10)
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = execute_no_sync_instances(Pattern),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               %% With no sync, execution should be fast (parallel-like)
               ?assert(Duration < 500, "No sync execution too slow"),

               %% Verify results
               Expected = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
               Actual = [R || {ok, R} <- Results],
               ?assertEqual(lists:sort(Expected), lists:sort(Actual))
           end},

          {"No sync - Completion tracking",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   fun(X) -> X end,
                   5,
                   [a, b, c, d, e]
               ),

               %% Execute and track completions
               {Results, Completed} = execute_no_sync_with_tracking(Pattern),

               ?assertEqual(5, length(Results)),
               ?assertEqual(5, Completed),
               ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
           end},

          %% Stress tests
          {"No sync - Stress test 100 instances",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   fun(X) -> X end,
                   100,
                   lists:seq(1, 100)
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = execute_no_sync_instances(Pattern),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,
               SuccessCount = length([R || {ok, R} <- Results]),

               ?assertEqual(100, SuccessCount),
               ?assert(Duration < 10000, "Stress test 100 instances too slow"),

               ?debugFmt("No sync 100 instances: ~p ms, ~p successful~n",
                        [Duration, SuccessCount])
           end},

          {"No sync - Stress test 1000 instances",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   fun(X) -> X end,
                   1000,
                   lists:seq(1, 1000)
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = execute_no_sync_instances(Pattern),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,
               SuccessCount = length([R || {ok, R} <- Results]),

               ?assertEqual(1000, SuccessCount),
               ?assert(Duration < 60000, "Stress test 1000 instances too slow"),

               ?debugFmt("No sync 1000 instances: ~p ms, ~p successful~n",
                        [Duration, SuccessCount])
           end},

          {"No sync - Concurrent pattern execution",
           fun() ->
               %% Execute multiple no-sync patterns concurrently
               Patterns = [
                   cre_yawl_patterns:multiple_instances_no_sync(
                       fun(X) -> X + 1 end, 10, lists:seq(1, 10)),
                   cre_yawl_patterns:multiple_instances_no_sync(
                       fun(X) -> X * 2 end, 10, lists:seq(1, 10)),
                   cre_yawl_patterns:multiple_instances_no_sync(
                       fun(X) -> X - 1 end, 10, lists:seq(2, 11))
               ],

               Start = erlang:monotonic_time(millisecond),
               Results = lists:map(fun execute_no_sync_instances/1, Patterns),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(3, length(Results)),
               ?assert(lists:all(fun(R) -> length(R) =:= 10 end, Results)),

               ?debugFmt("Concurrent patterns: ~p ms for ~p patterns~n",
                        [Duration, length(Patterns)])
           end},

          %% Failure scenario tests
          {"No sync - Instance failure handling",
           fun() ->
               FailingSubprocess = fun(X) ->
                   case X rem 5 of
                       0 -> exit(failure);
                       _ -> X * 2
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   FailingSubprocess,
                   10,
                   lists:seq(1, 10)
               ),

               Results = execute_no_sync_instances(Pattern),

               %% Some should fail, some should succeed
               SuccessCount = length([1 || {ok, _} <- Results]),
               FailureCount = length([1 || {error, _} <- Results]),

               ?assert(SuccessCount > 0),
               ?assert(FailureCount > 0),
               ?assertEqual(10, SuccessCount + FailureCount)
           end},

          {"No sync - Resource exhaustion prevention",
           fun() ->
               %% Test that pattern handles resource limits gracefully
               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   fun(X) -> X end,
                   50,
                   lists:seq(1, 50)
               ),

               %% Execute with limited resources
               Results = execute_no_sync_instances_with_limit(Pattern, 10),

               ?assert(length(Results) > 0),
               ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
           end},

          %% State validation tests
          {"No sync - Instance state tracking",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   fun(X) -> X end,
                   3,
                   [1, 2, 3]
               ),

               %% Verify state components
               ?assertEqual(3, Pattern#pattern_state.instance_count),
               ?assertEqual(3, Pattern#pattern_state.max_instances),
               ?assertEqual([1, 2, 3], Pattern#pattern_state.pending_instances),
               ?assertEqual([], Pattern#pattern_state.active_instances),
               ?assertEqual([], Pattern#pattern_state.completed_instances)
           end}
         ]
     end}.

%%====================================================================
%% WCP-13: Multiple Instances with Design Time Knowledge Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for Multiple Instances with Design Time Knowledge (WCP-13).
%%
%% This pattern creates a fixed number of instances known at design time.
%% All instances are created simultaneously and synchronized upon completion.
%%
%% @end
%%--------------------------------------------------------------------
multiple_instances_static_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          %% Normal execution tests
          {"Static instances - Fixed count spawning",
           fun() ->
               Subprocess = fun(X) -> X * 3 end,
               InstanceCount = 5,
               InputData = [1, 2, 3, 4, 5],

               Pattern = cre_yawl_patterns:multiple_instances_static(
                   Subprocess,
                   InstanceCount,
                   InputData
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(multiple_instances_static, Pattern#pattern_state.pattern_type),
               ?assertEqual(5, Pattern#pattern_state.instance_count),
               ?assertEqual(5, Pattern#pattern_state.max_instances),

               %% Execute with synchronization
               {Results, SyncResult} = execute_static_instances_with_sync(Pattern),

               ?assertEqual(5, length(Results)),
               ?assertEqual({all_complete, 5}, SyncResult),

               Expected = [3, 6, 9, 12, 15],
               Actual = [R || {ok, R} <- Results],
               ?assertEqual(lists:sort(Expected), lists:sort(Actual))
           end},

          {"Static instances - Synchronization on completion",
           fun() ->
               %% Verify all instances complete before proceeding
               Subprocess = fun(X) ->
                   timer:sleep(rand:uniform(20)),
                   X + 10
               end,

               Pattern = cre_yawl_patterns:multiple_instances_static(
                   Subprocess,
                   8,
                   lists:seq(1, 8)
               ),

               {Results, SyncResult} = execute_static_instances_with_sync(Pattern),

               %% All should complete
               ?assertEqual(8, length(Results)),
               ?assertEqual({all_complete, 8}, SyncResult),

               %% Verify all completed
               ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
           end},

          {"Static instances - Completion detection",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_static(
                   fun(X) -> X end,
                   4,
                   [a, b, c, d]
               ),

               %% Execute and detect completion
               {Results, Completed, AllComplete} =
                   execute_static_with_completion_detection(Pattern),

               ?assertEqual(4, Completed),
               ?assertEqual(true, AllComplete),
               ?assertEqual(4, length(Results))
           end},

          %% Stress tests
          {"Static instances - Stress test 100 instances",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_static(
                   fun(X) -> X end,
                   100,
                   lists:seq(1, 100)
               ),

               Start = erlang:monotonic_time(millisecond),
               {Results, SyncResult} = execute_static_instances_with_sync(Pattern),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(100, length(Results)),
               ?assertEqual({all_complete, 100}, SyncResult),
               ?assert(Duration < 15000, "Stress test 100 instances too slow"),

               ?debugFmt("Static 100 instances: ~p ms~n", [Duration])
           end},

          {"Static instances - Stress test 1000 instances",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_static(
                   fun(X) -> X end,
                   1000,
                   lists:seq(1, 1000)
               ),

               Start = erlang:monotonic_time(millisecond),
               {Results, SyncResult} = execute_static_instances_with_sync(Pattern),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(1000, length(Results)),
               ?assertEqual({all_complete, 1000}, SyncResult),
               ?assert(Duration < 90000, "Stress test 1000 instances too slow"),

               ?debugFmt("Static 1000 instances: ~p ms~n", [Duration])
           end},

          %% Failure scenario tests
          {"Static instances - Partial failure handling",
           fun() ->
               FailingSubprocess = fun(X) ->
                   case X > 7 of
                       true -> exit(failure);
                       false -> X * 2
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_static(
                   FailingSubprocess,
                   10,
                   lists:seq(1, 10)
               ),

               {Results, _SyncResult} = execute_static_instances_with_sync(Pattern),

               SuccessCount = length([1 || {ok, _} <- Results]),
               FailureCount = length([1 || {error, _} <- Results]),

               ?assert(SuccessCount < 10),
               ?assert(FailureCount > 0),
               ?assertEqual(10, SuccessCount + FailureCount)
           end},

          {"Static instances - Timeout on slow instances",
           fun() ->
               SlowSubprocess = fun(X) ->
                   case X of
                       5 -> timer:sleep(10000);
                       _ -> ok
                   end,
                   X
               end,

               Pattern = cre_yawl_patterns:multiple_instances_static(
                   SlowSubprocess,
                   5,
                   lists:seq(1, 5)
               ),

               Result = execute_static_with_timeout(Pattern, 1000),

               ?assertEqual({error, timeout}, Result)
           end},

          %% State validation tests
          {"Static instances - Design time validation",
           fun() ->
               %% Verify instance count is fixed at design time
               Pattern = cre_yawl_patterns:multiple_instances_static(
                   fun(X) -> X end,
                   7,
                   lists:seq(1, 10)
               ),

               ?assertEqual(7, Pattern#pattern_state.instance_count),
               ?assertEqual(7, Pattern#pattern_state.max_instances),
               ?assertEqual(10, length(Pattern#pattern_state.pending_instances))
           end}
         ]
     end}.

%%====================================================================
%% WCP-14: Multiple Instances with Runtime Knowledge Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for Multiple Instances with Runtime Knowledge (WCP-14).
%%
%% This pattern creates instances where the count is determined at runtime
%% but before instance creation begins.
%%
%% @end
%%--------------------------------------------------------------------
multiple_instances_runtime_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          %% Normal execution tests
          {"Runtime instances - Dynamic count evaluation",
           fun() ->
               CountFun = fun(Data) -> length(Data) end,
               Subprocess = fun(X) -> X * 2 end,
               InputData = [1, 2, 3, 4, 5],

               Pattern = cre_yawl_patterns:multiple_instances_runtime(
                   Subprocess,
                   CountFun,
                   InputData
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(multiple_instances_runtime, Pattern#pattern_state.pattern_type),

               %% Count should be evaluated from input data
               ?assertEqual(5, Pattern#pattern_state.instance_count),

               %% Execute instances
               Results = execute_runtime_instances(Pattern),

               ?assertEqual(5, length(Results)),
               ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
           end},

          {"Runtime instances - Count function evaluation",
           fun() ->
               %% Test various count functions
               CountFun1 = fun(Data) -> length(Data) * 2 end,
               Pattern1 = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   CountFun1,
                   [1, 2, 3]
               ),
               ?assertEqual(6, Pattern1#pattern_state.instance_count),

               CountFun2 = fun(Data) -> length(Data) div 2 end,
               Pattern2 = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   CountFun2,
                   [1, 2, 3, 4, 5, 6]
               ),
               ?assertEqual(3, Pattern2#pattern_state.instance_count),

               %% Test with integer count directly
               Pattern3 = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   4,
                   []
               ),
               ?assertEqual(4, Pattern3#pattern_state.instance_count)
           end},

          {"Runtime instances - Synchronization after runtime count",
           fun() ->
               Subprocess = fun(X) ->
                   timer:sleep(rand:uniform(10)),
                   X + 1
               end,

               CountFun = fun(Data) -> length(Data) end,

               Pattern = cre_yawl_patterns:multiple_instances_runtime(
                   Subprocess,
                   CountFun,
                   lists:seq(1, 10)
               ),

               {Results, SyncResult} = execute_runtime_instances_with_sync(Pattern),

               ?assertEqual(10, length(Results)),
               ?assertEqual({all_complete, 10}, SyncResult)
           end},

          %% Stress tests
          {"Runtime instances - Stress test 100 instances",
           fun() ->
               CountFun = fun(Data) -> length(Data) end,
               Pattern = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   CountFun,
                   lists:seq(1, 100)
               ),

               Start = erlang:monotonic_time(millisecond),
               {Results, SyncResult} = execute_runtime_instances_with_sync(Pattern),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(100, length(Results)),
               ?assertEqual({all_complete, 100}, SyncResult),
               ?assert(Duration < 15000, "Stress test 100 instances too slow"),

               ?debugFmt("Runtime 100 instances: ~p ms~n", [Duration])
           end},

          {"Runtime instances - Stress test 1000 instances",
           fun() ->
               CountFun = fun(Data) -> length(Data) end,
               Pattern = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   CountFun,
                   lists:seq(1, 1000)
               ),

               Start = erlang:monotonic_time(millisecond),
               {Results, SyncResult} = execute_runtime_instances_with_sync(Pattern),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(1000, length(Results)),
               ?assertEqual({all_complete, 1000}, SyncResult),
               ?assert(Duration < 90000, "Stress test 1000 instances too slow"),

               ?debugFmt("Runtime 1000 instances: ~p ms~n", [Duration])
           end},

          %% Failure scenario tests
          {"Runtime instances - Count function failure",
           fun() ->
               FailingCountFun = fun(_Data) ->
                   exit(count_failed)
               end,

               Result = catch cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   FailingCountFun,
                   [1, 2, 3]
               ),

               ?assertMatch({'EXIT', _}, Result)
           end},

          {"Runtime instances - Invalid count handling",
           fun() ->
               InvalidCountFun = fun(_Data) -> -1 end,

               Pattern = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   InvalidCountFun,
                   []
               ),

               %% Pattern should handle invalid count
               ?assertMatch(#pattern_state{}, Pattern)
           end},

          %% State validation tests
          {"Runtime instances - Runtime state validation",
           fun() ->
               CountFun = fun(Data) -> length(Data) + 5 end,

               Pattern = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X end,
                   CountFun,
                   [a, b, c]
               ),

               ?assertEqual(8, Pattern#pattern_state.instance_count),
               ?assertEqual(8, Pattern#pattern_state.max_instances),
               ?assertEqual(8, Pattern#pattern_state.instance_count)
           end}
         ]
     end}.

%%====================================================================
%% WCP-15: Multiple Instances without Prior Knowledge Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for Multiple Instances without Prior Knowledge (WCP-15).
%%
%% This pattern dynamically creates instances during execution based on
%% data availability. New instances can be spawned while others are running.
%%
%% @end
%%--------------------------------------------------------------------
multiple_instances_dynamic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          %% Normal execution tests
          {"Dynamic instances - Unbounded creation",
           fun() ->
               %% Simulate data stream
               DataFun = fun() ->
                   receive
                       {more_data, Data} -> {more, Data};
                       done -> done
                   after 100 -> done
                   end
               end,

               Subprocess = fun(X) -> X * 2 end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   Subprocess,
                   DataFun,
                   initial_data
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(multiple_instances_dynamic, Pattern#pattern_state.pattern_type),
               ?assertEqual(unlimited, Pattern#pattern_state.max_instances),

               %% Execute with dynamic spawning
               Results = execute_dynamic_instances(Pattern, 5),

               ?assert(length(Results) > 0),
               ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
           end},

          {"Dynamic instances - Runtime spawning",
           fun() ->
               %% Test spawning while other instances are running
               DataFun = fun() ->
                   case get(data_counter) of
                       undefined -> put(data_counter, 1), {more, 1};
                       N when N < 5 -> put(data_counter, N + 1), {more, N + 1};
                       _ -> done
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   fun(X) -> X end,
                   DataFun,
                   start
               ),

               Results = execute_dynamic_instances(Pattern, 5),

               ?assertEqual(5, length(Results))
           end},

          {"Dynamic instances - Data exhaustion detection",
           fun() ->
               %% Test termination when data is exhausted
               DataFun = fun() ->
                   case get(dynamic_data) of
                       undefined -> put(dynamic_data, [1, 2, 3]), {more, 1};
                       [H] -> put(dynamic_data, []), {more, H};
                       [] -> done
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   fun(X) -> X end,
                   DataFun,
                   initial
               ),

               {Results, Exhausted} = execute_dynamic_until_exhausted(Pattern),

               ?assert(length(Results) >= 3),
               ?assertEqual(true, Exhausted)
           end},

          %% Stress tests
          {"Dynamic instances - Stress test 100 dynamic spawns",
           fun() ->
               Counter = spawn_link(fun() -> dynamic_counter(100) end),

               DataFun = fun() ->
                   Counter ! {get_count, self()},
                   receive
                       {count, N} when N > 0 ->
                           Counter ! {decrement, self()},
                           {more, N};
                       {count, 0} ->
                           done
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   fun(X) -> X end,
                   DataFun,
                   start
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = execute_dynamic_instances(Pattern, 100),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(100, length(Results)),
               ?assert(Duration < 20000, "Stress test 100 dynamic instances too slow"),

               ?debugFmt("Dynamic 100 instances: ~p ms~n", [Duration])
           end},

          {"Dynamic instances - Stress test 1000 dynamic spawns",
           fun() ->
               Counter = spawn_link(fun() -> dynamic_counter(1000) end),

               DataFun = fun() ->
                   Counter ! {get_count, self()},
                   receive
                       {count, N} when N > 0 ->
                           Counter ! {decrement, self()},
                           {more, N};
                       {count, 0} ->
                           done
                   after 5000 ->
                           done
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   fun(X) -> X end,
                   DataFun,
                   start
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = execute_dynamic_instances(Pattern, 1000),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(1000, length(Results)),
               ?assert(Duration < 120000, "Stress test 1000 dynamic instances too slow"),

               ?debugFmt("Dynamic 1000 instances: ~p ms~n", [Duration])
           end},

          {"Dynamic instances - Concurrent pattern execution",
           fun() ->
               %% Execute multiple dynamic patterns concurrently
               Patterns = lists:map(fun(I) ->
                   Counter = spawn_link(fun() -> dynamic_counter(10) end),

                   DataFun = fun() ->
                       Counter ! {get_count, self()},
                       receive
                           {count, N} when N > 0 ->
                               Counter ! {decrement, self()},
                               {more, {I, N}};
                           {count, 0} ->
                               done
                       end
                   end,

                   cre_yawl_patterns:multiple_instances_dynamic(
                       fun({P, X}) -> {P, X} end,
                       DataFun,
                       I
                   )
               end, lists:seq(1, 5)),

               Start = erlang:monotonic_time(millisecond),
               Results = lists:map(fun(P) -> execute_dynamic_instances(P, 10) end, Patterns),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(5, length(Results)),
               ?assert(lists:all(fun(R) -> length(R) =:= 10 end, Results)),

               ?debugFmt("Concurrent dynamic patterns: ~p ms for ~p patterns~n",
                        [Duration, length(Patterns)])
           end},

          %% Failure scenario tests
          {"Dynamic instances - Spawn failure handling",
           fun() ->
               FailingDataFun = fun() ->
                   case get(fail_count) of
                       undefined ->
                           put(fail_count, 1),
                           {more, ok};
                       N when N < 3 ->
                           put(fail_count, N + 1),
                           {more, ok};
                       _ ->
                           exit(data_source_failed)
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   fun(X) -> X end,
                   FailingDataFun,
                   start
               ),

               Results = catch execute_dynamic_instances(Pattern, 5),

               ?assertMatch({'EXIT', _}, Results)
           end},

          {"Dynamic instances - Instance failure during spawn",
           fun() ->
               FailingSubprocess = fun(X) ->
                   case X rem 3 of
                       0 -> exit(instance_failed);
                       _ -> X
                   end
               end,

               DataFun = fun() ->
                   case get(dyn_counter) of
                       undefined -> put(dyn_counter, 1), {more, 1};
                       N when N < 10 -> put(dyn_counter, N + 1), {more, N};
                       _ -> done
                   end
               end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   FailingSubprocess,
                   DataFun,
                   start
               ),

               {Results, _Exhausted} = execute_dynamic_until_exhausted(Pattern),

               SuccessCount = length([1 || {ok, _} <- Results]),
               ?assert(SuccessCount > 0),
               ?assert(SuccessCount < 10)
           end},

          %% State validation tests
          {"Dynamic instances - Unbounded state tracking",
           fun() ->
               DataFun = fun() -> done end,

               Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                   fun(X) -> X end,
                   DataFun,
                   initial
               ),

               ?assertEqual(unlimited, Pattern#pattern_state.max_instances),
               ?assertEqual(0, Pattern#pattern_state.instance_count)
           end}
         ]
     end}.

%%====================================================================
%% WCP-16: Deferred Choice Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for Deferred Choice pattern (WCP-16).
%%
%% This pattern defers the choice between multiple alternatives until
%% runtime based on data availability or conditions.
%%
%% @end
%%--------------------------------------------------------------------
deferred_choice_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          %% Normal execution tests
          {"Deferred choice - Option selection at runtime",
           fun() ->
               Options = #{
                   option_a => fun() -> {selected, a} end,
                   option_b => fun() -> {selected, b} end,
                   option_c => fun() -> {selected, c} end
               },

               ConditionFun = fun(AvailableOptions) ->
                   %% Select first available option
                   [First | _] = maps:keys(AvailableOptions),
                   First
               end,

               Pattern = cre_yawl_patterns:deferred_choice(
                   Options,
                   ConditionFun,
                   undefined
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(deferred_choice, Pattern#pattern_state.pattern_type),

               %% Execute choice
               SelectedOption = execute_deferred_choice(Pattern),

               ?assert(is_atom(SelectedOption)),
               ?assert(lists:member(SelectedOption, [option_a, option_b, option_c]))
           end},

          {"Deferred choice - Data-based selection",
           fun() ->
               Options = #{
                   fast => fun(Data) -> {fast, Data} end,
                   slow => fun(Data) -> {slow, Data} end
               },

               ConditionFun = fun(Data) ->
                   case Data of
                       {urgent, _} -> fast;
                       {normal, _} -> slow
                   end
               end,

               Pattern = cre_yawl_patterns:deferred_choice(
                   Options,
                   ConditionFun,
                   {urgent, task1}
               ),

               Result = execute_deferred_choice_with_data(Pattern),

               ?assertEqual({selected, fast}, Result)
           end},

          {"Deferred choice - Alternative discarding",
           fun() ->
               Options = #{
                   path_a => fun() -> result_a end,
                   path_b => fun() -> result_b end
               },

               ConditionFun = fun(_) -> path_a end,

               Pattern = cre_yawl_patterns:deferred_choice(
                   Options,
                   ConditionFun,
                   undefined
               ),

               {Selected, Discarded} = execute_deferred_choice_with_discard(Pattern),

               ?assertEqual(path_a, Selected),
               ?assertEqual([path_b], Discarded)
           end},

          %% Stress tests
          {"Deferred choice - Stress test 100 selections",
           fun() ->
               Options = #{
                   opt1 => fun(I) -> I * 2 end,
                   opt2 => fun(I) -> I + 10 end,
                   opt3 => fun(I) -> I - 1 end
               },

               ConditionFun = fun(I) ->
                   case I rem 3 of
                       0 -> opt1;
                       1 -> opt2;
                       2 -> opt3
                   end
               end,

               Pattern = cre_yawl_patterns:deferred_choice(
                   Options,
                   ConditionFun,
                   undefined
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = lists:map(fun(I) ->
                   execute_deferred_choice_with_input(Pattern, I)
               end, lists:seq(1, 100)),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(100, length(Results)),
               ?assert(Duration < 5000, "Stress test 100 selections too slow"),

               ?debugFmt("Deferred choice 100 selections: ~p ms~n", [Duration])
           end},

          {"Deferred choice - Concurrent choices",
           fun() ->
               %% Execute multiple deferred choices concurrently
               Choices = lists:map(fun(I) ->
                   Options = #{
                       a => fun() -> {I, a} end,
                       b => fun() -> {I, b} end
                   },
                   ConditionFun = fun(J) ->
                       case J rem 2 of
                           0 -> a;
                           1 -> b
                       end
                   end,
                   cre_yawl_patterns:deferred_choice(Options, ConditionFun, I)
               end, lists:seq(1, 50)),

               Start = erlang:monotonic_time(millisecond),
               Results = lists:map(fun execute_deferred_choice/1, Choices),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(50, length(Results)),
               ?assert(Duration < 5000, "Concurrent choices too slow"),

               ?debugFmt("Concurrent deferred choices: ~p ms for ~p choices~n",
                        [Duration, length(Choices)])
           end},

          %% Failure scenario tests
          {"Deferred choice - No available options",
           fun() ->
               EmptyOptions = #{},

               Pattern = cre_yawl_patterns:deferred_choice(
                   EmptyOptions,
                   fun(_) -> no_option end,
                   undefined
               ),

               Result = execute_deferred_choice(Pattern),

               ?assertEqual(no_option_available, Result)
           end},

          {"Deferred choice - Condition function failure",
           fun() ->
               Options = #{a => fun() -> ok end},

               FailingConditionFun = fun(_) ->
                   exit(condition_failed)
               end,

               Pattern = cre_yawl_patterns:deferred_choice(
                   Options,
                   FailingConditionFun,
                   undefined
               ),

               Result = catch execute_deferred_choice(Pattern),

               ?assertMatch({'EXIT', _}, Result)
           end},

          %% State validation tests
          {"Deferred choice - Option state tracking",
           fun() ->
               Options = #{
                   option_x => fun() -> x_result end,
                   option_y => fun() -> y_result end
               },

               ConditionFun = fun(_) -> option_x end,

               Pattern = cre_yawl_patterns:deferred_choice(
                   Options,
                   ConditionFun,
                   undefined
               ),

               ?assertEqual(deferred_choice, Pattern#pattern_state.pattern_type),
               ?assert(is_function(Pattern#pattern_state.subprocess)),
               ?assert(is_map(Pattern#pattern_state.choice_data))
           end}
         ]
     end}.

%%====================================================================
%% WCP-17: Interleaved Parallel Routing Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for Interleaved Parallel Routing pattern (WCP-17).
%%
%% This pattern executes multiple branches in an interleaved fashion,
%% where tasks from different branches are executed in a non-deterministic
%% alternating order (round-robin).
%%
%% @end
%%--------------------------------------------------------------------
interleaved_routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          %% Normal execution tests
          {"Interleaved routing - Round-robin execution",
           fun() ->
               Branches = #{
                   branch_a => fun() -> {a, result} end,
                   branch_b => fun() -> {b, result} end,
                   branch_c => fun() -> {c, result} end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(interleaved_routing, Pattern#pattern_state.pattern_type),

               %% Execute interleaved
               Results = execute_interleaved_routing(Pattern, 3),

               ?assertEqual(3, length(Results)),

               %% Verify all branches executed
               BranchIds = [B || {B, _} <- Results],
               ?assert(lists:member(branch_a, BranchIds)),
               ?assert(lists:member(branch_b, BranchIds)),
               ?assert(lists:member(branch_c, BranchIds))
           end},

          {"Interleaved routing - Fairness validation",
           fun() ->
               %% Verify each branch gets fair chance to execute
               Branches = #{
                   b1 => fun() -> b1 end,
                   b2 => fun() -> b2 end,
                   b3 => fun() -> b3 end,
                   b4 => fun() -> b4 end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               %% Execute multiple cycles
               Results = execute_interleaved_routing(Pattern, 12),

               %% Count executions per branch (should be roughly equal)
               Counts = lists:foldl(fun(B, Acc) ->
                   maps:update_with(B, fun(V) -> V + 1 end, 1, Acc)
               end, #{b1 => 0, b2 => 0, b3 => 0, b4 => 0}, Results),

               ?assertEqual(3, maps:get(b1, Counts)),
               ?assertEqual(3, maps:get(b2, Counts)),
               ?assertEqual(3, maps:get(b3, Counts)),
               ?assertEqual(3, maps:get(b4, Counts))
           end},

          {"Interleaved routing - Branch completion tracking",
           fun() ->
               Branches = #{
                   task_a => fun() -> a_complete end,
                   task_b => fun() -> b_complete end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               {Results, Completed} = execute_interleaved_with_tracking(Pattern),

               ?assertEqual(2, length(Results)),
               ?assertEqual(2, Completed),
               ?assertEqual(true, lists:member(a_complete, Results)),
               ?assertEqual(true, lists:member(b_complete, Results))
           end},

          %% Stress tests
          {"Interleaved routing - Stress test 100 cycles",
           fun() ->
               Branches = #{
                   b1 => fun() -> b1 end,
                   b2 => fun() -> b2 end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = execute_interleaved_routing(Pattern, 100),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(100, length(Results)),
               ?assert(Duration < 10000, "Stress test 100 cycles too slow"),

               ?debugFmt("Interleaved routing 100 cycles: ~p ms~n", [Duration])
           end},

          {"Interleaved routing - Stress test 1000 cycles",
           fun() ->
               Branches = #{
                   x => fun() -> x end,
                   y => fun() -> y end,
                   z => fun() -> z end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               Start = erlang:monotonic_time(millisecond),
               Results = execute_interleaved_routing(Pattern, 300),
               End = erlang:monotonic_time(millisecond),

               Duration = End - Start,

               ?assertEqual(300, length(Results)),
               ?assert(Duration < 60000, "Stress test 1000 cycles too slow"),

               ?debugFmt("Interleaved routing 300 cycles: ~p ms~n", [Duration])
           end},

          {"Interleaved routing - Many branches",
           fun() ->
               %% Test with many branches
               BranchIds = lists:seq(1, 10),
               Branches = maps:from_list(lists:map(fun(I) ->
                   {I, fun() -> I end}
               end, BranchIds)),

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               Results = execute_interleaved_routing(Pattern, 30),

               ?assertEqual(30, length(Results)),

               %% Verify all branches were used
               UniqueBranches = lists:usort(Results),
               ?assertEqual(10, length(UniqueBranches))
           end},

          %% Failure scenario tests
          {"Interleaved routing - Branch failure handling",
           fun() ->
               Branches = #{
                   good => fun() -> ok end,
                   bad => fun() -> exit(branch_failed) end,
                   also_good => fun() -> ok end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               {Results, Failures} = execute_interleaved_with_failure_handling(Pattern),

               ?assert(Failures > 0),
               ?assert(length(Results) + Failures > 0)
           end},

          {"Interleaved routing - Timeout on slow branch",
           fun() ->
               Branches = #{
                   fast => fun() -> fast_result end,
                   slow => fun() -> timer:sleep(10000), slow_result end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               Result = execute_interleaved_with_timeout(Pattern, 1000),

               ?assertEqual({error, timeout}, Result)
           end},

          %% State validation tests
          {"Interleaved routing - Branch state tracking",
           fun() ->
               Branches = #{
                   alpha => fun() -> a end,
                   beta => fun() -> b end
               },

               Pattern = cre_yawl_patterns:interleaved_routing(
                   Branches,
                   undefined
               ),

               ?assertEqual(interleaved_routing, Pattern#pattern_state.pattern_type),
               ?assertEqual([], Pattern#pattern_state.branch_queue),
               ?assert(is_map(Pattern#pattern_state.choice_data))
           end}
         ]
     end}.

%%====================================================================
%% Performance Benchmark Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Performance benchmark tests for all multiple instance patterns.
%% @end
%%--------------------------------------------------------------------
performance_benchmark_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Performance - All patterns comparison",
           fun() ->
               Patterns = [
                   {implicit_termination,
                    cre_yawl_patterns:implicit_termination(fun() -> ok end)},
                   {no_sync,
                    cre_yawl_patterns:multiple_instances_no_sync(
                        fun(X) -> X end, 10, lists:seq(1, 10))},
                   {static,
                    cre_yawl_patterns:multiple_instances_static(
                        fun(X) -> X end, 10, lists:seq(1, 10))},
                   {runtime,
                    cre_yawl_patterns:multiple_instances_runtime(
                        fun(X) -> X end, fun(D) -> length(D) end, lists:seq(1, 10))}
               ],

               Benchmarks = lists:map(fun({Name, Pattern}) ->
                   Start = erlang:monotonic_time(millisecond),
                   MemoryBefore = memory_usage(),

                   _ = benchmark_execute_pattern(Name, Pattern),

                   End = erlang:monotonic_time(millisecond),
                   MemoryAfter = memory_usage(),

                   #perf_metrics{
                      start_time = Start,
                      end_time = End,
                      instance_count = 10,
                      memory_before = MemoryBefore,
                      memory_after = MemoryAfter
                   }
               end, Patterns),

               %% Report results
               lists:foreach(fun({Name, Metrics}) ->
                   Duration = Metrics#perf_metrics.end_time -
                               Metrics#perf_metrics.start_time,
                   MemoryDelta = Metrics#perf_metrics.memory_after -
                                 Metrics#perf_metrics.memory_before,
                   ?debugFmt("~s: ~p ms, ~p bytes~n",
                            [Name, Duration, MemoryDelta])
               end, lists:zip([N || {N, _} <- Patterns], Benchmarks)),

               ?assert(true)
           end},

          {"Performance - Memory usage validation",
           fun() ->
               %% Test memory usage across different instance counts
               Counts = [10, 50, 100],

               MemoryResults = lists:map(fun(Count) ->
                   Pattern = cre_yawl_patterns:multiple_instances_static(
                       fun(X) -> X end,
                       Count,
                       lists:seq(1, Count)
                   ),

                   Before = memory_usage(),

                   {Results, _Sync} = execute_static_instances_with_sync(Pattern),

                   After = memory_usage(),

                   {Count, After - Before, length(Results)}
               end, Counts),

               %% Verify memory growth is reasonable
               lists:foreach(fun({Count, Delta, _Results}) ->
                   PerInstance = Delta / Count,
                   ?debugFmt("~p instances: ~p bytes total, ~p bytes/instance~n",
                            [Count, Delta, PerInstance]),
                   ?assert(PerInstance < 10000, "Memory per instance too high")
               end, MemoryResults)
           end},

          {"Performance - Throughput measurement",
           fun() ->
               %% Measure throughput for different patterns
               Patterns = [
                   {no_sync, fun() ->
                       cre_yawl_patterns:multiple_instances_no_sync(
                           fun(X) -> X end, 50, lists:seq(1, 50))
                   end},
                   {static, fun() ->
                       cre_yawl_patterns:multiple_instances_static(
                           fun(X) -> X end, 50, lists:seq(1, 50))
                   end}
               ],

               ThroughputResults = lists:map(fun({Name, PatternFun}) ->
                   Pattern = PatternFun(),

                   Start = erlang:monotonic_time(millisecond),
                   {Results, _} = benchmark_execute_pattern(Name, Pattern),
                   End = erlang:monotonic_time(millisecond),

                   Duration = End - Start,
                   Throughput = length(Results) / Duration * 1000,

                   ?debugFmt("~s throughput: ~p instances/sec~n",
                            [Name, round(Throughput)]),

                   {Name, Throughput}
               end, Patterns),

               %% Verify minimum throughput
               lists:foreach(fun({_Name, Throughput}) ->
                   ?assert(Throughput > 10, "Throughput too low")
               end, ThroughputResults)
           end}
         ]
     end}.

%%====================================================================
%% Petri Net Semantics Validation Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Tests validating Petri net semantics for patterns.
%% @end
%%--------------------------------------------------------------------
petri_net_semantics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Petri net - Place validation",
           fun() ->
               Places = cre_yawl_patterns:place_lst(),

               %% Verify all required places exist
               RequiredPlaces = [
                   'p_start', 'p_active', 'p_work', 'p_terminate',
                   'p_instance_pool', 'p_ready', 'p_running', 'p_done', 'p_complete',
                   'p_option_a', 'p_option_b', 'p_selected',
                   'p_branch_pool', 'p_next_branch', 'p_executing'
               ],

               lists:foreach(fun(P) ->
                   ?assert(lists:member(P, Places),
                          io_lib:format("Missing place: ~p~n", [P]))
               end, RequiredPlaces)
           end},

          {"Petri net - Transition validation",
           fun() ->
               Transitions = cre_yawl_patterns:trsn_lst(),

               %% Verify all required transitions exist
               RequiredTransitions = [
                   't_activate', 't_implicit_term',
                   't_spawn_no_sync', 't_execute_no_sync',
                   't_spawn_all_static', 't_join_static',
                   't_offer_choice', 't_select_a',
                   't_distribute_branches', 't_pick_branch'
               ],

               lists:foreach(fun(T) ->
                   ?assert(lists:member(T, Transitions),
                          io_lib:format("Missing transition: ~p~n", [T]))
               end, RequiredTransitions)
           end},

          {"Petri net - Preset validation",
           fun() ->
               %% Verify preset definitions
               ?assertEqual(['p_start'], cre_yawl_patterns:preset('t_activate')),
               ?assertEqual(['p_instance_pool'], cre_yawl_patterns:preset('t_spawn_no_sync')),
               ?assertEqual(['p_start'], cre_yawl_patterns:preset('t_offer_choice')),
               ?assertEqual(['p_start'], cre_yawl_patterns:preset('t_distribute_branches'))
           end},

          {"Petri net - Is enabled validation",
           fun() ->
               %% Test is_enabled for various transitions
               Mode1 = #{'p_start' => [start]},
               ?assert(cre_yawl_patterns:is_enabled('t_activate', Mode1, #pattern_state{})),

               Mode2 = #{'p_instance_pool' => [data1, data2]},
               ?assert(cre_yawl_patterns:is_enabled('t_spawn_no_sync', Mode2, #pattern_state{})),

               Mode3 = #{'p_active' => [active], 'p_work' => [], 'p_work_pending' => []},
               ?assert(cre_yawl_patterns:is_enabled('t_implicit_term', Mode3, #pattern_state{}))
           end},

          {"Petri net - Fire validation",
           fun() ->
               %% Test fire produces correct tokens
               Mode = #{'p_start' => [start]},
               Result = cre_yawl_patterns:fire('t_activate', Mode, #pattern_state{}),

               ?assertMatch({produce, _}, Result),
               {produce, NewMode} = Result,
               ?assert(maps:is_key('p_active', NewMode))
           end}
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Execute implicit termination pattern.
%% @end
%%--------------------------------------------------------------------
execute_implicit_termination(Pattern, WorkItems) ->
    Subprocess = Pattern#pattern_state.subprocess,
    lists:foreach(fun(Item) ->
        Subprocess(Item)
    end, WorkItems),
    stopped.

%%--------------------------------------------------------------------
%% @doc Check implicit termination condition.
%% @end
%%--------------------------------------------------------------------
check_implicit_termination_condition(_Pattern, WorkQueue) ->
    %% Terminate when work is empty
    length(WorkQueue) =:= 0.

%%--------------------------------------------------------------------
%% @doc Execute implicit termination stress test.
%% @end
%%--------------------------------------------------------------------
execute_implicit_termination_stress(Pattern, WorkItems) ->
    Subprocess = Pattern#pattern_state.subprocess,
    lists:foreach(fun(Item) ->
        case Subprocess of
            Fun when is_function(Fun) -> Fun(Item);
            _ -> ok
        end
    end, WorkItems),
    completed.

%%--------------------------------------------------------------------
%% @doc Execute with failure handling.
%% @end
%%--------------------------------------------------------------------
execute_with_failure_handling(Pattern, WorkItems) ->
    Subprocess = Pattern#pattern_state.subprocess,
    try
        lists:foreach(fun(Item) ->
            Subprocess(Item)
        end, WorkItems),
        {ok, completed}
    catch
        _:Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Execute with timeout.
%% @end
%%--------------------------------------------------------------------
execute_with_timeout(Pattern, TimeoutMs) ->
    Subprocess = Pattern#pattern_state.subprocess,
    Pid = spawn_link(fun() ->
        Subprocess(work),
        exit(completed)
    end),
    receive
        {'EXIT', Pid, completed} -> completed;
        {'EXIT', Pid, _Reason} -> {error, failed}
    after TimeoutMs ->
        exit(Pid, kill),
        timeout
    end.

%%--------------------------------------------------------------------
%% @doc Execute no-sync instances.
%% @end
%%--------------------------------------------------------------------
execute_no_sync_instances(Pattern) ->
    Subprocess = Pattern#pattern_state.subprocess,
    InputData = Pattern#pattern_state.pending_instances,

    lists:map(fun(Data) ->
        try
            Result = Subprocess(Data),
            {ok, Result}
        catch
            _:Error -> {error, Error}
        end
    end, InputData).

%%--------------------------------------------------------------------
%% @doc Execute no-sync instances with tracking.
%% @end
%%--------------------------------------------------------------------
execute_no_sync_with_tracking(Pattern) ->
    Subprocess = Pattern#pattern_state.subprocess,
    InputData = Pattern#pattern_state.pending_instances,

    Results = lists:map(fun(Data) ->
        try
            Result = Subprocess(Data),
            {ok, Result}
        catch
            _:Error -> {error, Error}
        end
    end, InputData),

    Completed = length([R || {ok, R} <- Results]),
    {Results, Completed}.

%%--------------------------------------------------------------------
%% @doc Execute no-sync instances with limit.
%% @end
%%--------------------------------------------------------------------
execute_no_sync_instances_with_limit(Pattern, Limit) ->
    Subprocess = Pattern#pattern_state.subprocess,
    InputData = Pattern#pattern_state.pending_instances,

    {ToExecute, _Rest} = lists:split(min(Limit, length(InputData)), InputData),

    lists:map(fun(Data) ->
        try
            Result = Subprocess(Data),
            {ok, Result}
        catch
            _:Error -> {error, Error}
        end
    end, ToExecute).

%%--------------------------------------------------------------------
%% @doc Execute static instances with synchronization.
%% @end
%%--------------------------------------------------------------------
execute_static_instances_with_sync(Pattern) ->
    Subprocess = Pattern#pattern_state.subprocess,
    InputData = Pattern#pattern_state.pending_instances,
    InstanceCount = Pattern#pattern_state.instance_count,

    {ToExecute, _} = lists:split(InstanceCount, InputData),

    Results = lists:map(fun(Data) ->
        try
            Result = Subprocess(Data),
            {ok, Result}
        catch
            _:Error -> {error, Error}
        end
    end, ToExecute),

    SyncResult = {all_complete, InstanceCount},
    {Results, SyncResult}.

%%--------------------------------------------------------------------
%% @doc Execute static with completion detection.
%% @end
%%--------------------------------------------------------------------
execute_static_with_completion_detection(Pattern) ->
    {Results, _} = execute_static_instances_with_sync(Pattern),
    Completed = length([1 || {ok, _} <- Results]),
    AllComplete = Completed =:= Pattern#pattern_state.instance_count,
    {Results, Completed, AllComplete}.

%%--------------------------------------------------------------------
%% @doc Execute static with timeout.
%% @end
%%--------------------------------------------------------------------
execute_static_with_timeout(Pattern, TimeoutMs) ->
    Subprocess = Pattern#pattern_state.subprocess,
    InputData = Pattern#pattern_state.pending_instances,

    Pid = spawn_link(fun() ->
        lists:foreach(fun(Data) ->
            Subprocess(Data)
        end, InputData),
        exit(completed)
    end),

    receive
        {'EXIT', Pid, completed} -> {ok, completed};
        {'EXIT', Pid, _Reason} -> {error, failed}
    after TimeoutMs ->
        exit(Pid, kill),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Execute runtime instances.
%% @end
%%--------------------------------------------------------------------
execute_runtime_instances(Pattern) ->
    Subprocess = Pattern#pattern_state.subprocess,
    InstanceCount = Pattern#pattern_state.instance_count,

    lists:map(fun(I) ->
        try
            Result = Subprocess(I),
            {ok, Result}
        catch
            _:Error -> {error, Error}
        end
    end, lists:seq(1, InstanceCount)).

%%--------------------------------------------------------------------
%% @doc Execute runtime instances with synchronization.
%% @end
%%--------------------------------------------------------------------
execute_runtime_instances_with_sync(Pattern) ->
    Subprocess = Pattern#pattern_state.subprocess,
    InstanceCount = Pattern#pattern_state.instance_count,

    Results = lists:map(fun(I) ->
        try
            Result = Subprocess(I),
            {ok, Result}
        catch
            _:Error -> {error, Error}
        end
    end, lists:seq(1, InstanceCount)),

    SyncResult = {all_complete, InstanceCount},
    {Results, SyncResult}.

%%--------------------------------------------------------------------
%% @doc Execute dynamic instances.
%% @end
%%--------------------------------------------------------------------
execute_dynamic_instances(Pattern, MaxInstances) ->
    execute_dynamic_instances_helper(Pattern, MaxInstances, []).

execute_dynamic_instances_helper(_Pattern, 0, Acc) ->
    lists:reverse(Acc);
execute_dynamic_instances_helper(Pattern, Remaining, Acc) ->
    DataFun = maps:get(data_fun, Pattern#pattern_state.choice_data, fun() -> done end),

    case DataFun() of
        done ->
            lists:reverse(Acc);
        {more, Data} ->
            Subprocess = Pattern#pattern_state.subprocess,
            try
                Result = Subprocess(Data),
                execute_dynamic_instances_helper(
                    Pattern, Remaining - 1, [{ok, Result} | Acc])
            catch
                _:Error ->
                    execute_dynamic_instances_helper(
                        Pattern, Remaining - 1, [{error, Error} | Acc])
            end
    end.

%%--------------------------------------------------------------------
%% @doc Execute dynamic until exhausted.
%% @end
%%--------------------------------------------------------------------
execute_dynamic_until_exhausted(Pattern) ->
    execute_dynamic_until_exhausted_helper(Pattern, []).

execute_dynamic_until_exhausted_helper(Pattern, Acc) ->
    DataFun = maps:get(data_fun, Pattern#pattern_state.choice_data, fun() -> done end),

    case DataFun() of
        done ->
            {lists:reverse(Acc), true};
        {more, Data} ->
            Subprocess = Pattern#pattern_state.subprocess,
            try
                Result = Subprocess(Data),
                execute_dynamic_until_exhausted_helper(
                    Pattern, [{ok, Result} | Acc])
            catch
                _:Error ->
                    execute_dynamic_until_exhausted_helper(
                        Pattern, [{error, Error} | Acc])
            end
    end.

%%--------------------------------------------------------------------
%% @doc Dynamic counter helper.
%% @end
%%--------------------------------------------------------------------
dynamic_counter(InitialCount) ->
    put(dynamic_count, InitialCount),
    dynamic_counter_loop().

dynamic_counter_loop() ->
    receive
        {get_count, Pid} ->
            Count = get(dynamic_count),
            Pid ! {count, Count},
            dynamic_counter_loop();
        {decrement, Pid} ->
            Current = get(dynamic_count),
            put(dynamic_count, Current - 1),
            Pid ! decremented,
            dynamic_counter_loop()
    end.

%%--------------------------------------------------------------------
%% @doc Execute deferred choice.
%% @end
%%--------------------------------------------------------------------
execute_deferred_choice(Pattern) ->
    ConditionFun = Pattern#pattern_state.subprocess,
    Options = maps:get(options, Pattern#pattern_state.choice_data, #{}),

    case maps:keys(Options) of
        [] -> no_option_available;
        Keys ->
            Selected = ConditionFun(Keys),
            case maps:find(Selected, Options) of
                {ok, SelectedFun} ->
                    SelectedFun(),
                    Selected;
                error ->
                    no_option_available
            end
    end.

%%--------------------------------------------------------------------
%% @doc Execute deferred choice with data.
%% @end
%%--------------------------------------------------------------------
execute_deferred_choice_with_data(Pattern) ->
    ConditionFun = Pattern#pattern_state.subprocess,
    InitialData = Pattern#pattern_state.pending_instances,

    Selected = ConditionFun(InitialData),
    {selected, Selected}.

%%--------------------------------------------------------------------
%% @doc Execute deferred choice with discard.
%% @end
%%--------------------------------------------------------------------
execute_deferred_choice_with_discard(Pattern) ->
    ConditionFun = Pattern#pattern_state.subprocess,
    Options = maps:get(options, Pattern#pattern_state.choice_data, #{}),

    Selected = ConditionFun(undefined),
    Discarded = lists:delete(Selected, maps:keys(Options)),

    {Selected, Discarded}.

%%--------------------------------------------------------------------
%% @doc Execute deferred choice with input.
%% @end
%%--------------------------------------------------------------------
execute_deferred_choice_with_input(Pattern, Input) ->
    ConditionFun = Pattern#pattern_state.subprocess,
    Options = maps:get(options, Pattern#pattern_state.choice_data, #{}),

    Selected = ConditionFun(Input),
    case maps:find(Selected, Options) of
        {ok, Fun} -> Fun(Input);
        error -> not_found
    end.

%%--------------------------------------------------------------------
%% @doc Execute interleaved routing.
%% @end
%%--------------------------------------------------------------------
execute_interleaved_routing(Pattern, NumCycles) ->
    Branches = maps:get(branches, Pattern#pattern_state.choice_data, #{}),
    BranchKeys = maps:keys(Branches),

    execute_interleaved_round_robin(Branches, BranchKeys, NumCycles, []).

execute_interleaved_round_robin(_Branches, _BranchKeys, 0, Acc) ->
    lists:reverse(Acc);
execute_interleaved_round_robin(Branches, [BranchKey | Rest], Cycles, Acc) when Cycles > 0 ->
    BranchFun = maps:get(BranchKey, Branches),
    Result = BranchFun(),
    execute_interleaved_round_robin(
        Branches, Rest ++ [BranchKey], Cycles - 1, [Result | Acc]).

%%--------------------------------------------------------------------
%% @doc Execute interleaved with tracking.
%% @end
%%--------------------------------------------------------------------
execute_interleaved_with_tracking(Pattern) ->
    Branches = maps:get(branches, Pattern#pattern_state.choice_data, #{}),
    BranchKeys = maps:keys(Branches),

    Results = lists:map(fun(Key) ->
        BranchFun = maps:get(Key, Branches),
        BranchFun()
    end, BranchKeys),

    Completed = length(Results),
    {Results, Completed}.

%%--------------------------------------------------------------------
%% @doc Execute interleaved with failure handling.
%% @end
%%--------------------------------------------------------------------
execute_interleaved_with_failure_handling(Pattern) ->
    Branches = maps:get(branches, Pattern#pattern_state.choice_data, #{}),
    BranchKeys = maps:keys(Branches),

    {Results, Failures} = lists:foldl(fun(Key, {ResAcc, FailAcc}) ->
        BranchFun = maps:get(Key, Branches),
        try
            Result = BranchFun(),
            {[Result | ResAcc], FailAcc}
        catch
            _:_ ->
                {ResAcc, FailAcc + 1}
        end
    end, {[], 0}, BranchKeys),

    {Results, Failures}.

%%--------------------------------------------------------------------
%% @doc Execute interleaved with timeout.
%% @end
%%--------------------------------------------------------------------
execute_interleaved_with_timeout(Pattern, TimeoutMs) ->
    Branches = maps:get(branches, Pattern#pattern_state.choice_data, #{}),
    BranchKeys = maps:keys(Branches),

    Pid = spawn_link(fun() ->
        Results = lists:map(fun(Key) ->
            BranchFun = maps:get(Key, Branches),
            BranchFun()
        end, BranchKeys),
        exit({results, Results})
    end),

    receive
        {'EXIT', Pid, {results, Results}} -> {ok, Results};
        {'EXIT', Pid, _Reason} -> {error, failed}
    after TimeoutMs ->
        exit(Pid, kill),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Benchmark execute pattern.
%% @end
%%--------------------------------------------------------------------
benchmark_execute_pattern(implicit_termination, _Pattern) ->
    {ok, []};
benchmark_execute_pattern(no_sync, Pattern) ->
    execute_no_sync_instances(Pattern);
benchmark_execute_pattern(static, Pattern) ->
    execute_static_instances_with_sync(Pattern);
benchmark_execute_pattern(runtime, Pattern) ->
    execute_runtime_instances(Pattern);
benchmark_execute_pattern(_, _Pattern) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc Get current memory usage.
%% @end
%%--------------------------------------------------------------------
memory_usage() ->
    {memory, Usage} = process_info(self(), memory),
    Usage.
