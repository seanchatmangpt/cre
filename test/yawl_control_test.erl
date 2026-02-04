%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Control Panel Test Suite
%%
%% Comprehensive test suite for YAWL control panel functionality including
%% case management, monitoring, statistics, and configuration.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_control_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%% Explicitly export test generators for EUnit discovery
-export([
    test_start_control_test_/0,
    test_register_case_test_/0,
    test_get_running_cases_test_/0,
    test_cancel_case_test_/0,
    test_suspend_resume_case_test_/0,
    test_get_case_statistics_test_/0,
    test_get_engine_status_test_/0,
    test_set_engine_parameter_test_/0,
    test_list_all_cases_test_/0,
    test_case_status_transitions_test_/0,
    test_get_case_status_test_/0,
    test_unregister_case_test_/0,
    setup/0,
    cleanup/1,
    get_status/1,
    test/0
]).

-compile(export_all).

%%====================================================================
%% Record Definitions (imported from yawl_control)
%%====================================================================

-record(case_info, {
    case_id :: binary(),
    spec_id :: binary(),
    status :: running | suspended | cancelled | completed | failed,
    start_time :: erlang:timestamp(),
    end_time :: erlang:timestamp() | undefined,
    current_task :: binary() | undefined,
    tasks_completed = 0 :: non_neg_integer(),
    tasks_total = 0 :: non_neg_integer(),
    data = #{} :: map()
}).

-record(engine_status, {
    start_time :: erlang:timestamp(),
    cases_completed = 0 :: non_neg_integer(),
    cases_cancelled = 0 :: non_neg_integer(),
    cases_failed = 0 :: non_neg_integer(),
    parameters = #{} :: map()
}).

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts the yawl_control gen_server.
%% @end
%%--------------------------------------------------------------------
setup() ->
    case whereis(yawl_control) of
        undefined ->
            {ok, _Pid} = yawl_control:start_control(),
            timer:sleep(100),  % Allow server to initialize
            ok;
        _Pid ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the yawl_control gen_server.
%% @end
%%--------------------------------------------------------------------
cleanup(_State) ->
    yawl_control:stop(),
    timer:sleep(50),  % Allow server to stop
    ok.

%%====================================================================
%% Test: Start Control Panel
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test starting the control panel with default name.
%% @end
%%--------------------------------------------------------------------
test_start_control_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     ?assert(is_pid(whereis(yawl_control)))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Register Case
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test registering a new workflow case.
%% @end
%%--------------------------------------------------------------------
test_register_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     CaseId = <<"case_001">>,
                     SpecId = <<"order_processing">>,
                     ?assertEqual(ok, yawl_control:register_case(CaseId, SpecId))
                 end),
          ?_test(begin
                     % First unregister any existing case from previous test
                     yawl_control:unregister_case(<<"case_001">>),
                     % Register multiple cases
                     lists:foreach(
                         fun(N) ->
                             Id = list_to_binary("case_" ++ integer_to_list(N)),
                             Spec = list_to_binary("spec_" ++ integer_to_list(N)),
                             ok = yawl_control:register_case(Id, Spec)
                         end,
                         lists:seq(1, 5)
                     ),
                     ?assertEqual(5, length(yawl_control:get_running_cases()))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Running Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test retrieving all currently running cases.
%% @end
%%--------------------------------------------------------------------
test_get_running_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Initially no running cases
                     ?assertEqual([], yawl_control:get_running_cases())
                 end),
          ?_test(begin
                     % After registering cases
                     yawl_control:register_case(<<"case_001">>, <<"spec_a">>),
                     yawl_control:register_case(<<"case_002">>, <<"spec_b">>),
                     yawl_control:register_case(<<"case_003">>, <<"spec_c">>),
                     Running = yawl_control:get_running_cases(),
                     ?assertEqual(3, length(Running)),
                     % Verify case IDs are present
                     CaseIds = [maps:get(case_id, C) || C <- Running],
                     ?assert(lists:member(<<"case_001">>, CaseIds)),
                     ?assert(lists:member(<<"case_002">>, CaseIds)),
                     ?assert(lists:member(<<"case_003">>, CaseIds))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Cancel Case
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test canceling a running workflow case.
%% @end
%%--------------------------------------------------------------------
test_cancel_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Cancel non-existent case
                     Result = yawl_control:cancel_case(<<"nonexistent">>, <<"test">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Register and cancel a case
                     yawl_control:register_case(<<"case_to_cancel">>, <<"spec_x">>),
                     ?assertEqual(ok, yawl_control:cancel_case(<<"case_to_cancel">>, <<"user requested">>)),
                     % Verify case is no longer running
                     Running = yawl_control:get_running_cases(),
                     ?assertNot(lists:any(
                         fun(C) -> maps:get(case_id, C) =:= <<"case_to_cancel">> end,
                         Running
                     ))
                 end),
          ?_test(begin
                     % Double cancel should fail
                     yawl_control:register_case(<<"case_double_cancel">>, <<"spec_y">>),
                     ok = yawl_control:cancel_case(<<"case_double_cancel">>, <<"reason1">>),
                     Result = yawl_control:cancel_case(<<"case_double_cancel">>, <<"reason2">>),
                     ?assertEqual({error, already_cancelled}, Result)
                 end),
          ?_test(begin
                     % Verify cancelled case appears in statistics
                     yawl_control:register_case(<<"case_stats_check">>, <<"spec_z">>),
                     yawl_control:cancel_case(<<"case_stats_check">>, <<"cancelled">>),
                     Stats = yawl_control:get_case_statistics(),
                     ?assert(maps:get(cancelled, Stats) > 0)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Suspend and Resume Case
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suspending and resuming a workflow case.
%% @end
%%--------------------------------------------------------------------
test_suspend_resume_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Suspend non-existent case
                     Result = yawl_control:suspend_case(<<"nonexistent">>, <<"test">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Suspend a running case
                     yawl_control:register_case(<<"case_suspend">>, <<"spec_s">>),
                     ?assertEqual(ok, yawl_control:suspend_case(<<"case_suspend">>, <<"maintenance">>)),
                     % Verify case is no longer in running cases
                     Running = yawl_control:get_running_cases(),
                     ?assertNot(lists:any(
                         fun(C) -> maps:get(case_id, C) =:= <<"case_suspend">> end,
                         Running
                     ))
                 end),
          ?_test(begin
                     % Resume a suspended case
                     yawl_control:register_case(<<"case_resume">>, <<"spec_r">>),
                     yawl_control:suspend_case(<<"case_resume">>, <<"pause">>),
                     ?assertEqual(ok, yawl_control:resume_case(<<"case_resume">>, <<"maintenance complete">>)),
                     % Verify case is running again
                     Running = yawl_control:get_running_cases(),
                     ?assert(lists:any(
                         fun(C) -> maps:get(case_id, C) =:= <<"case_resume">> end,
                         Running
                     ))
                 end),
          ?_test(begin
                     % Double suspend should fail
                     yawl_control:register_case(<<"case_double_suspend">>, <<"spec_ds">>),
                     ok = yawl_control:suspend_case(<<"case_double_suspend">>, <<"pause1">>),
                     Result = yawl_control:suspend_case(<<"case_double_suspend">>, <<"pause2">>),
                     ?assertEqual({error, already_suspended}, Result)
                 end),
          ?_test(begin
                     % Resume without suspend should fail
                     yawl_control:register_case(<<"case_resume_fail">>, <<"spec_rf">>),
                     Result = yawl_control:resume_case(<<"case_resume_fail">>, <<"test">>),
                     ?assertEqual({error, not_suspended}, Result)
                 end),
          ?_test(begin
                     % Verify suspended case appears in statistics
                     yawl_control:register_case(<<"case_suspended_stats">>, <<"spec_ss">>),
                     yawl_control:suspend_case(<<"case_suspended_stats">>, <<"test">>),
                     Stats = yawl_control:get_case_statistics(),
                     ?assert(maps:get(suspended, Stats) > 0)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Case Statistics
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test retrieving execution statistics.
%% @end
%%--------------------------------------------------------------------
test_get_case_statistics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Initial statistics
                     Stats = yawl_control:get_case_statistics(),
                     ?assert(maps:is_key(running, Stats)),
                     ?assert(maps:is_key(suspended, Stats)),
                     ?assert(maps:is_key(completed, Stats)),
                     ?assert(maps:is_key(cancelled, Stats)),
                     ?assert(maps:is_key(failed, Stats)),
                     ?assert(maps:is_key(total, Stats)),
                     ?assert(maps:is_key(avg_completion_time_ms, Stats))
                 end),
          ?_test(begin
                     % Statistics after registering cases
                     yawl_control:register_case(<<"case_001">>, <<"spec_a">>),
                     yawl_control:register_case(<<"case_002">>, <<"spec_b">>),
                     yawl_control:register_case(<<"case_003">>, <<"spec_c">>),
                     Stats = yawl_control:get_case_statistics(),
                     ?assertEqual(3, maps:get(running, Stats)),
                     ?assertEqual(3, maps:get(total, Stats))
                 end),
          ?_test(begin
                     % Statistics after cancel
                     yawl_control:register_case(<<"case_cancel_stats">>, <<"spec_cs">>),
                     yawl_control:cancel_case(<<"case_cancel_stats">>, <<"cancelled">>),
                     Stats = yawl_control:get_case_statistics(),
                     ?assert(maps:get(cancelled, Stats) > 0)
                 end),
          ?_test(begin
                     % Statistics after suspend
                     yawl_control:register_case(<<"case_suspend_stats">>, <<"spec_sus">>),
                     yawl_control:suspend_case(<<"case_suspend_stats">>, <<"paused">>),
                     Stats = yawl_control:get_case_statistics(),
                     ?assert(maps:get(suspended, Stats) > 0)
                 end),
          ?_test(begin
                     % Statistics after marking completed
                     yawl_control:register_case(<<"case_complete_stats">>, <<"spec_comp">>),
                     yawl_control:update_case_status(<<"case_complete_stats">>, completed),
                     Stats = yawl_control:get_case_statistics(),
                     ?assert(maps:get(completed, Stats) > 0)
                 end),
          ?_test(begin
                     % Statistics after marking failed
                     yawl_control:register_case(<<"case_fail_stats">>, <<"spec_fail">>),
                     yawl_control:update_case_status(<<"case_fail_stats">>, failed),
                     Stats = yawl_control:get_case_statistics(),
                     ?assert(maps:get(failed, Stats) > 0)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Engine Status
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test retrieving engine status information.
%% @end
%%--------------------------------------------------------------------
test_get_engine_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     Status = yawl_control:get_engine_status(),
                     ?assert(maps:is_key(start_time, Status)),
                     ?assert(maps:is_key(uptime_ms, Status)),
                     ?assert(maps:is_key(cases_completed, Status)),
                     ?assert(maps:is_key(cases_cancelled, Status)),
                     ?assert(maps:is_key(cases_failed, Status)),
                     ?assert(maps:is_key(parameters, Status)),
                     % Verify uptime is positive
                     ?assert(maps:get(uptime_ms, Status) >= 0)
                 end),
          ?_test(begin
                     % Verify default parameters
                     Status = yawl_control:get_engine_status(),
                     Params = maps:get(parameters, Status),
                     ?assert(maps:is_key(max_concurrent_cases, Params)),
                     ?assert(maps:is_key(case_timeout, Params)),
                     ?assert(maps:is_key(enable_auto_cleanup, Params)),
                     ?assert(maps:is_key(cleanup_interval, Params))
                 end),
          ?_test(begin
                     % Verify counters increment
                     yawl_control:register_case(<<"case_001">>, <<"spec_a">>),
                     yawl_control:update_case_status(<<"case_001">>, completed),
                     yawl_control:register_case(<<"case_002">>, <<"spec_b">>),
                     yawl_control:cancel_case(<<"case_002">>, <<"test">>),
                     yawl_control:register_case(<<"case_003">>, <<"spec_c">>),
                     yawl_control:update_case_status(<<"case_003">>, failed),
                     Status = yawl_control:get_engine_status(),
                     ?assert(maps:get(cases_completed, Status) > 0),
                     ?assert(maps:get(cases_cancelled, Status) > 0),
                     ?assert(maps:get(cases_failed, Status) > 0)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Set Engine Parameter
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test setting engine runtime parameters.
%% @end
%%--------------------------------------------------------------------
test_set_engine_parameter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Set valid parameter
                     ?assertEqual(ok, yawl_control:set_engine_parameter(max_concurrent_cases, 200)),
                     Status = yawl_control:get_engine_status(),
                     Params = maps:get(parameters, Status),
                     ?assertEqual(200, maps:get(max_concurrent_cases, Params))
                 end),
          ?_test(begin
                     % Set case_timeout
                     ?assertEqual(ok, yawl_control:set_engine_parameter(case_timeout, 7200000)),
                     Status = yawl_control:get_engine_status(),
                     Params = maps:get(parameters, Status),
                     ?assertEqual(7200000, maps:get(case_timeout, Params))
                 end),
          ?_test(begin
                     % Set enable_auto_cleanup
                     ?assertEqual(ok, yawl_control:set_engine_parameter(enable_auto_cleanup, false)),
                     Status = yawl_control:get_engine_status(),
                     Params = maps:get(parameters, Status),
                     ?assertEqual(false, maps:get(enable_auto_cleanup, Params))
                 end),
          ?_test(begin
                     % Set custom parameter
                     ?assertEqual(ok, yawl_control:set_engine_parameter(custom_param, <<"custom_value">>)),
                     Status = yawl_control:get_engine_status(),
                     Params = maps:get(parameters, Status),
                     ?assertEqual(<<"custom_value">>, maps:get(custom_param, Params))
                 end),
          ?_test(begin
                     % Set multiple parameters
                     lists:foreach(
                         fun({N, V}) -> yawl_control:set_engine_parameter(N, V) end,
                         [
                             {param1, 100},
                             {param2, <<"value">>},
                             {param3, true},
                             {param4, [1, 2, 3]}
                         ]
                     ),
                     Status = yawl_control:get_engine_status(),
                     Params = maps:get(parameters, Status),
                     ?assertEqual(100, maps:get(param1, Params)),
                     ?assertEqual(<<"value">>, maps:get(param2, Params)),
                     ?assertEqual(true, maps:get(param3, Params)),
                     ?assertEqual([1, 2, 3], maps:get(param4, Params))
                 end)
         ]
     end}.

%%====================================================================
%% Test: List All Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test listing all cases (running and completed).
%% @end
%%--------------------------------------------------------------------
test_list_all_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Initially empty
                     ?assertEqual([], yawl_control:list_all_cases())
                 end),
          ?_test(begin
                     % After registering cases
                     yawl_control:register_case(<<"case_001">>, <<"spec_a">>),
                     yawl_control:register_case(<<"case_002">>, <<"spec_b">>),
                     AllCases = yawl_control:list_all_cases(),
                     ?assertEqual(2, length(AllCases)),
                     % Verify all are running
                     lists:foreach(
                         fun(C) -> ?assertEqual(running, maps:get(status, C)) end,
                         AllCases
                     )
                 end),
          ?_test(begin
                     % Include completed cases
                     yawl_control:register_case(<<"case_running">>, <<"spec_r">>),
                     yawl_control:register_case(<<"case_completed">>, <<"spec_c">>),
                     yawl_control:update_case_status(<<"case_completed">>, completed),
                     yawl_control:register_case(<<"case_cancelled">>, <<"spec_x">>),
                     yawl_control:cancel_case(<<"case_cancelled">>, <<"test">>),
                     AllCases = yawl_control:list_all_cases(),
                     ?assert(length(AllCases) >= 3),
                     % Verify different statuses
                     Statuses = [maps:get(status, C) || C <- AllCases],
                     ?assert(lists:member(running, Statuses)),
                     ?assert(lists:member(completed, Statuses) orelse lists:member(cancelled, Statuses))
                 end),
          ?_test(begin
                     % Verify case info structure
                     yawl_control:register_case(<<"case_info_test">>, <<"spec_i">>),
                     [Case | _] = yawl_control:list_all_cases(),
                     ?assert(maps:is_key(case_id, Case)),
                     ?assert(maps:is_key(spec_id, Case)),
                     ?assert(maps:is_key(status, Case)),
                     ?assert(maps:is_key(start_time, Case)),
                     ?assert(maps:is_key(progress, Case)),
                     ?assert(maps:is_key(tasks_completed, Case)),
                     ?assert(maps:is_key(tasks_total, Case))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Case Status Transitions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test all valid and invalid state transitions.
%% @end
%%--------------------------------------------------------------------
test_case_status_transitions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Valid: running -> suspended
                     yawl_control:register_case(<<"case_r_to_s">>, <<"spec_a">>),
                     ?assertEqual(ok, yawl_control:suspend_case(<<"case_r_to_s">>, <<"pause">>)),
                     ?assertEqual({error, already_suspended}, yawl_control:suspend_case(<<"case_r_to_s">>, <<"pause">>))
                 end),
          ?_test(begin
                     % Valid: suspended -> running (resume)
                     yawl_control:register_case(<<"case_s_to_r">>, <<"spec_b">>),
                     yawl_control:suspend_case(<<"case_s_to_r">>, <<"pause">>),
                     ?assertEqual(ok, yawl_control:resume_case(<<"case_s_to_r">>, <<"resume">>)),
                     ?assertEqual({error, not_suspended}, yawl_control:resume_case(<<"case_s_to_r">>, <<"resume">>))
                 end),
          ?_test(begin
                     % Valid: running -> cancelled
                     yawl_control:register_case(<<"case_r_to_c">>, <<"spec_c">>),
                     ?assertEqual(ok, yawl_control:cancel_case(<<"case_r_to_c">>, <<"cancel">>)),
                     ?assertEqual({error, already_cancelled}, yawl_control:cancel_case(<<"case_r_to_c">>, <<"cancel">>))
                 end),
          ?_test(begin
                     % Valid: running -> completed
                     yawl_control:register_case(<<"case_r_to_comp">>, <<"spec_d">>),
                     ?assertEqual(ok, yawl_control:update_case_status(<<"case_r_to_comp">>, completed)),
                     % Get case status to verify
                     Status = yawl_control:get_case_status(<<"case_r_to_comp">>),
                     ?assertEqual(completed, maps:get(status, Status))
                 end),
          ?_test(begin
                     % Valid: running -> failed
                     yawl_control:register_case(<<"case_r_to_f">>, <<"spec_e">>),
                     ?assertEqual(ok, yawl_control:update_case_status(<<"case_r_to_f">>, failed)),
                     Status = yawl_control:get_case_status(<<"case_r_to_f">>),
                     ?assertEqual(failed, maps:get(status, Status))
                 end),
          ?_test(begin
                     % Invalid: suspended -> cancelled (direct)
                     yawl_control:register_case(<<"case_s_to_c">>, <<"spec_f">>),
                     yawl_control:suspend_case(<<"case_s_to_c">>, <<"pause">>),
                     % Cancel from suspended should work (force cancel)
                     ?assertEqual(ok, yawl_control:cancel_case(<<"case_s_to_c">>, <<"force cancel">>))
                 end),
          ?_test(begin
                     % Invalid: completed -> cancelled
                     yawl_control:register_case(<<"case_comp_to_c">>, <<"spec_g">>),
                     yawl_control:update_case_status(<<"case_comp_to_c">>, completed),
                     Result = yawl_control:cancel_case(<<"case_comp_to_c">>, <<"cancel">>),
                     ?assertEqual({error, already_completed}, Result)
                 end),
          ?_test(begin
                     % Invalid: cancelled -> suspended
                     yawl_control:register_case(<<"case_c_to_s">>, <<"spec_h">>),
                     yawl_control:cancel_case(<<"case_c_to_s">>, <<"cancel">>),
                     Result = yawl_control:suspend_case(<<"case_c_to_s">>, <<"pause">>),
                     ?assertEqual({error, not_running}, Result)
                 end),
          ?_test(begin
                     % Invalid: cancelled -> resume
                     yawl_control:register_case(<<"case_c_to_r">>, <<"spec_i">>),
                     yawl_control:cancel_case(<<"case_c_to_r">>, <<"cancel">>),
                     Result = yawl_control:resume_case(<<"case_c_to_r">>, <<"resume">>),
                     ?assertEqual({error, invalid_state}, Result)
                 end),
          ?_test(begin
                     % Full lifecycle: running -> suspended -> running -> completed
                     yawl_control:register_case(<<"case_lifecycle">>, <<"spec_j">>),
                     ?assertEqual(running, get_status(<<"case_lifecycle">>)),
                     ?assertEqual(ok, yawl_control:suspend_case(<<"case_lifecycle">>, <<"pause">>)),
                     ?assertEqual(suspended, get_status(<<"case_lifecycle">>)),
                     ?assertEqual(ok, yawl_control:resume_case(<<"case_lifecycle">>, <<"resume">>)),
                     ?assertEqual(running, get_status(<<"case_lifecycle">>)),
                     ?assertEqual(ok, yawl_control:update_case_status(<<"case_lifecycle">>, completed)),
                     ?assertEqual(completed, get_status(<<"case_lifecycle">>))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Case Status
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test retrieving status of a specific case.
%% @end
%%--------------------------------------------------------------------
test_get_case_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Get non-existent case
                     Result = yawl_control:get_case_status(<<"nonexistent">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Get existing case
                     yawl_control:register_case(<<"case_status">>, <<"spec_s">>),
                     StatusMap = yawl_control:get_case_status(<<"case_status">>),
                     ?assertEqual(<<"case_status">>, maps:get(case_id, StatusMap)),
                     ?assertEqual(<<"spec_s">>, maps:get(spec_id, StatusMap)),
                     ?assertEqual(running, maps:get(status, StatusMap)),
                     ?assert(maps:is_key(start_time, StatusMap)),
                     ?assert(maps:is_key(progress, StatusMap))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Unregister Case
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test unregistering a workflow case.
%% @end
%%--------------------------------------------------------------------
test_unregister_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Unregister existing case
                     yawl_control:register_case(<<"case_unregister">>, <<"spec_u">>),
                     ?assertEqual(ok, yawl_control:unregister_case(<<"case_unregister">>)),
                     % Case should no longer exist
                     Result = yawl_control:get_case_status(<<"case_unregister">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Unregister non-existent case (should not error)
                     ?assertEqual(ok, yawl_control:unregister_case(<<"nonexistent">>))
                 end)
         ]
     end}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Helper to get the status of a case.
%% @end
%%--------------------------------------------------------------------
-spec get_status(binary()) -> running | suspended | cancelled | completed | failed.

get_status(CaseId) ->
    case yawl_control:get_case_status(CaseId) of
        {error, _} -> undefined;
        StatusMap -> maps:get(status, StatusMap)
    end.

%%====================================================================
%% EUnit Test Export
%%====================================================================
%% @doc Returns all test generators for EUnit discovery.
%% @end
%%--------------------------------------------------------------------
test() ->
    [
        test_start_control_test_(),
        test_register_case_test_(),
        test_get_running_cases_test_(),
        test_cancel_case_test_(),
        test_suspend_resume_case_test_(),
        test_get_case_statistics_test_(),
        test_get_engine_status_test_(),
        test_set_engine_parameter_test_(),
        test_list_all_cases_test_(),
        test_case_status_transitions_test_(),
        test_get_case_status_test_(),
        test_unregister_case_test_()
    ].
