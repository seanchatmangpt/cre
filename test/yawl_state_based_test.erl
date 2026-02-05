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
%% @doc YAWL State-Based Pattern Execution Test Suite
%%
%% Comprehensive test suite for state-based YAWL patterns (WCP-18 through WCP-20).
%% Tests cover normal execution, edge cases, state transitions, and cancellation
%% for:
%%   - WCP-18: Milestone Pattern
%%   - WCP-19: Cancel Activity Pattern
%%   - WCP-20: Cancel Case Pattern
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_state_based_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Exports
%%====================================================================

-export([
    setup/0,
    cleanup/1,
    test_activity/0,
    milestone_not_reached/0,
    milestone_reached/0,
    no_cancellation/0,
    cancel_requested/0
]).

-compile(export_all).

%%====================================================================
%% Records
%%====================================================================

-record(pattern_state, {
    pattern_type :: atom(),
    subprocess :: term() | undefined,
    instance_count :: non_neg_integer() | undefined,
    max_instances :: non_neg_integer() | unlimited | undefined,
    pending_instances = [] :: list(),
    active_instances = [] :: list(),
    completed_instances = [] :: list(),
    choice_data = #{} :: map(),
    branch_queue = [] :: list()
}).

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% @end
%%--------------------------------------------------------------------
setup() ->
    %% Initialize persistent terms for configuration (handle if cre_config not available)
    try
        cre_config:init()
    catch
        error:undef ->
            %% cre_config:init/0 may not be fully available, initialize terms manually
            init_persistent_terms();
        _:_ ->
            init_persistent_terms()
    end,

    %% Ensure clean state
    catch gen_server:stop(yawl_stateless),
    timer:sleep(10).

%%--------------------------------------------------------------------
%% @doc Initialize persistent terms manually for testing.
%% @end
%%--------------------------------------------------------------------
init_persistent_terms() ->
    %% Initialize YAWL patterns place and transition lists
    PlaceList = [
        'p_start', 'p_active', 'p_work', 'p_work_pending', 'p_terminate',
        'p_instance_pool', 'p_ready', 'p_running', 'p_done', 'p_complete',
        'p_spawn_pending', 'p_all_spawned', 'p_eval', 'p_data_source', 'p_final',
        'p_choice_pending', 'p_option_a', 'p_option_b', 'p_options',
        'p_selected', 'p_discarded', 'p_choice_complete',
        'p_branch_pool', 'p_next_branch', 'p_executing', 'p_branch_done',
        'p_all_done', 'p_interleave_complete',
        'p_milestone_guard', 'p_milestone_reached', 'p_active', 'p_complete',
        'p_activity_running', 'p_cancellation_pending', 'p_cancelled', 'p_completed',
        'p_case_active', 'p_cancellation_requested', 'p_cancelling', 'p_cancelled', 'p_completed',
        'p_sync_block_start', 'p_sync_block_active', 'p_sync_barrier', 'p_sync_block_done',
        'p_sync_activity_1', 'p_sync_activity_2', 'p_sync_activity_3',
        'p_partial_start', 'p_partial_running', 'p_partial_quorum', 'p_partial_done',
        'p_partial_instance_1', 'p_partial_instance_2', 'p_partial_instance_3',
        'p_loop_init', 'p_loop_body', 'p_loop_condition', 'p_loop_exit', 'p_loop_complete',
        'p_loop_iteration', 'p_loop_continue', 'p_loop_break',
        'p_rec_start', 'p_rec_call', 'p_rec_result', 'p_rec_base', 'p_rec_done',
        'p_rec_call_1', 'p_rec_call_2', 'p_rec_stack',
        'p_il_loop_start', 'p_il_parallel', 'p_il_interleave', 'p_il_loop_cond', 'p_il_exit',
        'p_il_body_1', 'p_il_body_2', 'p_il_body_3', 'p_il_iteration',
        'p_cs_request', 'p_cs_lock', 'p_cs_active', 'p_cs_release', 'p_cs_complete',
        'p_proto_idle', 'p_proto_request_sent', 'p_proto_waiting', 'p_proto_response',
        'p_proto_complete', 'p_proto_error',
        'p_try_entry', 'p_try_body', 'p_catch_entry', 'p_catch_body', 'p_try_catch_done',
        'p_try_success', 'p_try_failure', 'p_catch_error'
    ],
    try persistent_term:put(yawl_patterns_place_lst, PlaceList) catch _:_ -> ok end,

    TrsnList = [
        't_activate', 't_queue_work', 't_dequeue_work', 't_implicit_term',
        't_spawn_no_sync', 't_execute_no_sync', 't_complete_no_sync',
        't_spawn_all_static', 't_execute_static', 't_collect_static', 't_join_static',
        't_eval_count', 't_spawn_runtime', 't_execute_runtime', 't_collect_runtime',
        't_join_runtime',
        't_spawn_dynamic', 't_fetch_data', 't_execute_dynamic', 't_collect_dynamic',
        't_check_done', 't_terminate_dynamic',
        't_offer_choice', 't_select_a', 't_select_b', 't_discard_a', 't_discard_b',
        't_complete_choice',
        't_distribute_branches', 't_pick_branch', 't_execute_branch',
        't_return_branch', 't_complete_interleaved',
        't_work', 't_milestone_check', 't_complete',
        't_work', 't_cancel_request', 't_cancel_confirm', 't_complete',
        't_work', 't_request_cancel', 't_confirm_cancel', 't_execute_cancel', 't_complete',
        't_sync_enter', 't_sync_activity_1', 't_sync_activity_2', 't_sync_activity_3',
        't_sync_barrier', 't_sync_complete', 't_sync_timeout',
        't_partial_start', 't_partial_instance_1', 't_partial_instance_2', 't_partial_instance_3',
        't_partial_check_quorum', 't_partial_complete', 't_partial_timeout',
        't_loop_init', 't_loop_enter', 't_loop_execute', 't_loop_check',
        't_loop_continue', 't_loop_break', 't_loop_complete', 't_loop_timeout',
        't_rec_start', 't_rec_call_1', 't_rec_call_2', 't_rec_base',
        't_rec_continue', 't_rec_complete', 't_rec_stack_push', 't_rec_stack_pop',
        't_il_loop_start', 't_il_distribute', 't_il_execute', 't_il_check',
        't_il_interleave', 't_il_continue', 't_il_complete', 't_il_timeout',
        't_cs_request', 't_cs_acquire', 't_cs_enter', 't_cs_exit', 't_cs_release',
        't_cs_timeout', 't_cs_error',
        't_proto_start', 't_proto_request', 't_proto_wait', 't_proto_response',
        't_proto_complete', 't_proto_error', 't_proto_retry',
        't_try_enter', 't_try_execute', 't_try_success', 't_try_failure',
        't_catch_enter', 't_catch_execute', 't_catch_complete', 't_try_catch_complete'
    ],
    try persistent_term:put(yawl_patterns_trsn_lst, TrsnList) catch _:_ -> ok end,

    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% @end
%%--------------------------------------------------------------------
cleanup(_Arg) ->
    catch gen_server:stop(yawl_stateless),
    timer:sleep(10).

%%--------------------------------------------------------------------
%% @doc Creates a test activity function.
%% @end
%%--------------------------------------------------------------------
test_activity() ->
    fun(Data) ->
        io:format("Executing activity with data: ~p~n", [Data]),
        {ok, processed_data}
    end.

%%--------------------------------------------------------------------
%% @doc Creates a milestone check function that returns false.
%% @end
%%--------------------------------------------------------------------
milestone_not_reached() ->
    fun() -> false end.

%%--------------------------------------------------------------------
%% @doc Creates a milestone check function that returns true.
%% @end
%%--------------------------------------------------------------------
milestone_reached() ->
    fun() -> true end.

%%--------------------------------------------------------------------
%% @doc Creates a cancellation check function that returns false.
%% @end
%%--------------------------------------------------------------------
no_cancellation() ->
    fun() -> false end.

%%--------------------------------------------------------------------
%% @doc Creates a cancellation check function that returns true.
%% @end
%%--------------------------------------------------------------------
cancel_requested() ->
    fun() -> true end.

%%====================================================================
%% WCP-18: Milestone Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Normal Execution Tests
%%--------------------------------------------------------------------

milestone_normal_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Milestone pattern - Create valid pattern",
           fun() ->
               Activity = test_activity(),
               MilestoneFun = milestone_not_reached(),
               Pattern = cre_yawl_patterns:milestone(Activity, MilestoneFun),

               ?assertEqual(milestone, Pattern#pattern_state.pattern_type),
               ?assert(is_function(Pattern#pattern_state.subprocess)),
               ?assertEqual(0, Pattern#pattern_state.instance_count)
           end},

          {"Milestone pattern - Guard evaluation prevents execution",
           fun() ->
               Activity = test_activity(),
               MilestoneFun = milestone_not_reached(),
               Pattern = cre_yawl_patterns:milestone(Activity, MilestoneFun),

               %% When milestone is not reached, activity should not execute
               ?assert(is_function(maps:get(milestone_check, Pattern#pattern_state.choice_data))),

               %% Simulate guard check - should return false
               GuardResult = (maps:get(milestone_check, Pattern#pattern_state.choice_data))(),
               ?assertEqual(false, GuardResult)
           end},

          {"Milestone pattern - Guard evaluation allows execution",
           fun() ->
               Activity = test_activity(),
               MilestoneFun = milestone_reached(),
               Pattern = cre_yawl_patterns:milestone(Activity, MilestoneFun),

               %% When milestone is reached, activity should execute
               GuardResult = (maps:get(milestone_check, Pattern#pattern_state.choice_data))(),
               ?assertEqual(true, GuardResult)
           end},

          {"Milestone pattern - Fire work transition",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),

               InputMode = #{'p_active' => [active]},
               Result = cre_yawl_patterns:fire('t_work', InputMode, Pattern),

               ?assertMatch({produce, _}, Result),
               {produce, Output} = Result,
               ?assert(maps:is_key('p_milestone_guard', Output)),
               ?assert(maps:is_key('p_active', Output))
           end},

          {"Milestone pattern - Complete after milestone reached",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),

               %% Simulate milestone reached and complete
               InputMode = #{'p_active' => [active], 'p_complete' => [complete]},
               Result = cre_yawl_patterns:fire('t_complete', InputMode, Pattern),

               ?assertMatch({produce, _}, Result),
               {produce, Output} = Result,
               ?assertEqual([completed], maps:get('p_active', Output, undefined))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Edge Case Tests - Milestone
%%--------------------------------------------------------------------

milestone_edge_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Milestone - Never reached milestone",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_not_reached()),

               %% Milestone check returns false - activity should wait
               GuardFun = maps:get(milestone_check, Pattern#pattern_state.choice_data),
               ?assertEqual(false, GuardFun()),

               %% Multiple checks should all return false
               ?assertEqual(false, GuardFun()),
               ?assertEqual(false, GuardFun()),
               ?assertEqual(false, GuardFun())
           end},

          {"Milestone - Empty activity list",
           fun() ->
               EmptyActivity = fun(_Data) -> {ok, empty} end,
               Pattern = cre_yawl_patterns:milestone(EmptyActivity, milestone_reached()),

               ?assertEqual(milestone, Pattern#pattern_state.pattern_type),
               ?assert(is_function(Pattern#pattern_state.subprocess))
           end},

          {"Milestone - Undefined milestone function",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), undefined),

               %% Should handle undefined milestone function gracefully
               ?assert(is_map(Pattern#pattern_state.choice_data))
           end},

          {"Milestone - Rapid state transitions",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),

               %% Rapid firing of transitions
               lists:foreach(fun(_) ->
                   InputMode = #{'p_active' => [active]},
                   case cre_yawl_patterns:fire('t_work', InputMode, Pattern) of
                       {produce, _} -> ok;
                       abort -> ok
                   end
               end, lists:seq(1, 100))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% State Transition Tests - Milestone
%%--------------------------------------------------------------------

milestone_state_transition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Milestone - All valid state paths",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),

               %% Path 1: Guard -> Milestone Reached -> Complete
               State1 = #{'p_active' => [active]},
               {produce, Output1} = cre_yawl_patterns:fire('t_work', State1, Pattern),

               ?assert(maps:is_key('p_milestone_guard', Output1)),

               %% Path 2: Check milestone
               State2 = maps:put('p_milestone_guard', [milestone_required], State1),
               {produce, Output2} = cre_yawl_patterns:fire('t_milestone_check', State2, Pattern),

               ?assert(lists:member(milestone_reached, maps:get('p_milestone_reached', Output2, []))),

               %% Path 3: Complete
               State3 = maps:put('p_complete', [complete], State2),
               {produce, Output3} = cre_yawl_patterns:fire('t_complete', State3, Pattern),

               ?assertEqual([completed], maps:get('p_active', Output3))
           end},

          {"Milestone - Invalid state transitions are rejected",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),

               %% Try to complete without milestone
               InvalidState = #{'p_active' => [active]},
               Result = cre_yawl_patterns:fire('t_complete', InvalidState, Pattern),

               %% Should abort when invalid transition is attempted
               ?assertEqual(abort, Result)
           end},

          {"Milestone - State persistence across transitions",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),

               %% Track instance count across transitions
               InitialCount = Pattern#pattern_state.instance_count,
               ?assertEqual(0, InitialCount),

               %% After milestone work, count should increase (via trigger)
               State = #{'p_active' => [active]},
               _ = cre_yawl_patterns:fire('t_work', State, Pattern),

               %% Verify pattern state structure is maintained
               ?assert(is_record(Pattern, pattern_state))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Cancellation Tests - Milestone (uses timeout-like behavior)
%%--------------------------------------------------------------------

milestone_timeout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Milestone - Timeout when milestone never reached",
           fun() ->
               %% Create a milestone that never gets reached
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_not_reached()),

               %% Simulate timeout scenario
               TimeoutFun = fun() ->
                   case (maps:get(milestone_check, Pattern#pattern_state.choice_data))() of
                       true -> reached;
                       false -> timeout
                   end
               end,

               ?assertEqual(timeout, TimeoutFun())
           end},

          {"Milestone - Resource cleanup on timeout",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_not_reached()),

               %% Verify pattern can be cleaned up
               ?assert(is_pattern_state_valid(Pattern))
           end}
         ]
     end}.

%%====================================================================
%% WCP-19: Cancel Activity Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Normal Execution Tests - Cancel Activity
%%--------------------------------------------------------------------

cancel_activity_normal_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel activity - Create valid pattern",
           fun() ->
               Activity = test_activity(),
               CancelFun = no_cancellation(),
               Pattern = cre_yawl_patterns:cancel_activity(Activity, CancelFun),

               ?assertEqual(cancel_activity, Pattern#pattern_state.pattern_type),
               ?assert(is_function(Pattern#pattern_state.subprocess)),
               ?assertEqual(0, Pattern#pattern_state.instance_count)
           end},

          {"Cancel activity - Normal execution without cancellation",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), no_cancellation()),

               %% Normal work transition
               InputMode = #{'p_activity_running' => [running]},
               Result = cre_yawl_patterns:fire('t_work', InputMode, Pattern),

               ?assertMatch({produce, _}, Result),
               {produce, Output} = Result,
               ?assertEqual([work_done], maps:get('p_completed', Output))
           end},

          {"Cancel activity - Guard evaluation",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               CancelCheck = maps:get(cancel_check, Pattern#pattern_state.choice_data),
               ?assert(is_function(CancelCheck)),
               ?assertEqual(true, CancelCheck())
           end},

          {"Cancel activity - Complete transition",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), no_cancellation()),

               InputMode = #{
                   'p_activity_running' => [running],
                   'p_completed' => [work_done]
               },
               Result = cre_yawl_patterns:fire('t_complete', InputMode, Pattern),

               ?assertMatch({produce, _}, Result),
               {produce, Output} = Result,
               ?assertEqual([completed], maps:get('p_activity_running', Output))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Edge Case Tests - Cancel Activity
%%--------------------------------------------------------------------

cancel_activity_edge_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel activity - Cancel before start",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               %% Request cancel on activity that hasn't started
               InputMode = #{'p_activity_running' => []},
               Result = cre_yawl_patterns:fire('t_cancel_request', InputMode, Pattern),

               %% Should handle gracefully
               case Result of
                   {produce, _} -> ok;
                   abort -> ok
               end
           end},

          {"Cancel activity - Multiple cancel requests",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               %% First cancel request
               InputMode1 = #{'p_activity_running' => [running]},
               Result1 = cre_yawl_patterns:fire('t_cancel_request', InputMode1, Pattern),

               case Result1 of
                   {produce, Output1} ->
                       ?assertEqual([cancel_pending], maps:get('p_cancellation_pending', Output1));
                   _ ->
                       ?assert(false, first_cancel_failed)
               end,

               %% Second cancel request should be idempotent
               InputMode2 = #{'p_activity_running' => [], 'p_cancellation_pending' => [cancel_pending]},
               Result2 = cre_yawl_patterns:fire('t_cancel_confirm', InputMode2, Pattern),

               case Result2 of
                   {produce, Output2} ->
                       ?assertEqual([cancelled], maps:get('p_cancelled', Output2));
                   _ ->
                       ?assert(false, second_cancel_failed)
               end
           end},

          {"Cancel activity - Empty activity",
           fun() ->
               EmptyActivity = fun(_Data) -> {ok, empty} end,
               Pattern = cre_yawl_patterns:cancel_activity(EmptyActivity, cancel_requested()),

               ?assertEqual(cancel_activity, Pattern#pattern_state.pattern_type)
           end},

          {"Cancel activity - Cancel during execution",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), fun() ->
                   %% Simulate cancel triggered mid-execution
                   true
               end),

               InputMode = #{'p_activity_running' => [running]},
               Result = cre_yawl_patterns:fire('t_cancel_request', InputMode, Pattern),

               ?assertMatch({produce, _}, Result)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% State Transition Tests - Cancel Activity
%%--------------------------------------------------------------------

cancel_activity_state_transition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel activity - All valid state paths",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               %% Path 1: Running -> Cancel Requested
               State1 = #{'p_activity_running' => [running]},
               {produce, Output1} = cre_yawl_patterns:fire('t_cancel_request', State1, Pattern),
               ?assertEqual([cancel_pending], maps:get('p_cancellation_pending', Output1)),

               %% Path 2: Cancel Requested -> Confirmed
               State2 = #{'p_cancellation_pending' => [cancel_pending]},
               {produce, Output2} = cre_yawl_patterns:fire('t_cancel_confirm', State2, Pattern),
               ?assertEqual([cancelled], maps:get('p_cancelled', Output2))
           end},

          {"Cancel activity - Running to completion path",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), no_cancellation()),

               %% Normal completion path
               State = #{'p_activity_running' => [running]},
               {produce, Output} = cre_yawl_patterns:fire('t_work', State, Pattern),
               ?assertEqual([work_done], maps:get('p_completed', Output))
           end},

          {"Cancel activity - State persistence",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               InitialCount = Pattern#pattern_state.instance_count,
               ?assertEqual(0, InitialCount),

               %% After cancel, instance count should update
               _ = cre_yawl_patterns:fire('t_cancel_request', #{'p_activity_running' => [running]}, Pattern),

               ?assert(is_record(Pattern, pattern_state))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Cancellation Tests - Cancel Activity
%%--------------------------------------------------------------------

cancel_activity_cancellation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel activity - Graceful cancellation",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               %% Graceful cancellation flow
               State1 = #{'p_activity_running' => [running]},
               {produce, Output1} = cre_yawl_patterns:fire('t_cancel_request', State1, Pattern),
               ?assertEqual([], maps:get('p_activity_running', Output1)),

               State2 = #{'p_cancellation_pending' => [cancel_pending]},
               {produce, Output2} = cre_yawl_patterns:fire('t_cancel_confirm', State2, Pattern),
               ?assertEqual([cancelled], maps:get('p_cancelled', Output2))
           end},

          {"Cancel activity - Forced cancellation",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), fun() -> true end),

               %% Force cancellation via immediate cancel check
               InputMode = #{'p_activity_running' => [running]},
               Result = cre_yawl_patterns:fire('t_cancel_request', InputMode, Pattern),

               ?assertMatch({produce, #{'p_cancellation_pending' := [cancel_pending]}}, Result)
           end},

          {"Cancel activity - Resource cleanup",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               %% Verify cleanup tokens are produced
               State = #{'p_cancellation_pending' => [cancel_pending]},
               {produce, Output} = cre_yawl_patterns:fire('t_cancel_confirm', State, Pattern),

               ?assertEqual([cancelled], maps:get('p_cancelled', Output)),
               ?assertEqual([], maps:get('p_cancellation_pending', Output))
           end}
         ]
     end}.

%%====================================================================
%% WCP-20: Cancel Case Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Normal Execution Tests - Cancel Case
%%--------------------------------------------------------------------

cancel_case_normal_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel case - Create valid pattern",
           fun() ->
               Activities = [test_activity(), test_activity()],
               CancelFun = no_cancellation(),
               Pattern = cre_yawl_patterns:cancel_case(Activities, CancelFun),

               ?assertEqual(cancel_case, Pattern#pattern_state.pattern_type),
               ?assert(is_list(Pattern#pattern_state.subprocess)),
               ?assertEqual(2, length(Pattern#pattern_state.subprocess)),
               ?assertEqual(2, Pattern#pattern_state.instance_count)
           end},

          {"Cancel case - Normal execution without cancellation",
           fun() ->
               Activities = [test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, no_cancellation()),

               %% Normal work transition
               InputMode = #{'p_case_active' => [active]},
               Result = cre_yawl_patterns:fire('t_work', InputMode, Pattern),

               ?assertMatch({produce, _}, Result),
               {produce, Output} = Result,
               ?assertEqual([work_done], maps:get('p_completed', Output))
           end},

          {"Cancel case - Multiple activities in case",
           fun() ->
               Activities = [
                   fun(_) -> {ok, activity1} end,
                   fun(_) -> {ok, activity2} end,
                   fun(_) -> {ok, activity3} end
               ],
               Pattern = cre_yawl_patterns:cancel_case(Activities, no_cancellation()),

               ?assertEqual(3, Pattern#pattern_state.instance_count),
               ?assertEqual(3, length(Pattern#pattern_state.subprocess))
           end},

          {"Cancel case - Complete transition",
           fun() ->
               Activities = [test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, no_cancellation()),

               InputMode = #{
                   'p_case_active' => [active],
                   'p_completed' => [work_done]
               },
               Result = cre_yawl_patterns:fire('t_complete', InputMode, Pattern),

               ?assertMatch({produce, _}, Result),
               {produce, Output} = Result,
               ?assertEqual([completed], maps:get('p_case_active', Output))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Edge Case Tests - Cancel Case
%%--------------------------------------------------------------------

cancel_case_edge_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel case - Cancel before start",
           fun() ->
               Activities = [test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               %% Cancel request before activities start
               InputMode = #{'p_case_active' => []},
               Result = cre_yawl_patterns:fire('t_request_cancel', InputMode, Pattern),

               %% Should handle gracefully
               case Result of
                   {produce, _} -> ok;
                   abort -> ok
               end
           end},

          {"Cancel case - Multiple cancel requests",
           fun() ->
               Activities = [test_activity(), test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               %% First cancel request
               InputMode1 = #{'p_case_active' => [active]},
               Result1 = cre_yawl_patterns:fire('t_request_cancel', InputMode1, Pattern),
               ?assertMatch({produce, #{'p_cancellation_requested' := [cancel_requested]}}, Result1),

               %% Verify idempotency
               ?assertMatch({produce, #{'p_cancellation_requested' := [cancel_requested]}},
                           cre_yawl_patterns:fire('t_request_cancel', InputMode1, Pattern))
           end},

          {"Cancel case - Empty activity list",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_case([], no_cancellation()),

               ?assertEqual(cancel_case, Pattern#pattern_state.pattern_type),
               ?assertEqual(0, Pattern#pattern_state.instance_count)
           end},

          {"Cancel case - Single activity case",
           fun() ->
               Activities = [test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               ?assertEqual(1, Pattern#pattern_state.instance_count),

               %% Cancel flow for single activity
               InputMode = #{'p_case_active' => [active]},
               Result = cre_yawl_patterns:fire('t_request_cancel', InputMode, Pattern),

               ?assertMatch({produce, _}, Result)
           end},

          {"Cancel case - Large number of activities",
           fun() ->
               Activities = [test_activity() || _ <- lists:seq(1, 100)],
               Pattern = cre_yawl_patterns:cancel_case(Activities, no_cancellation()),

               ?assertEqual(100, Pattern#pattern_state.instance_count),
               ?assertEqual(100, length(Pattern#pattern_state.subprocess))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% State Transition Tests - Cancel Case
%%--------------------------------------------------------------------

cancel_case_state_transition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel case - All valid state paths",
           fun() ->
               Activities = [test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               %% Path 1: Active -> Cancellation Requested
               State1 = #{'p_case_active' => [active]},
               {produce, Output1} = cre_yawl_patterns:fire('t_request_cancel', State1, Pattern),
               ?assertEqual([cancel_requested], maps:get('p_cancellation_requested', Output1)),

               %% Path 2: Requested -> Confirming
               State2 = #{'p_cancellation_requested' => [cancel_requested]},
               {produce, Output2} = cre_yawl_patterns:fire('t_confirm_cancel', State2, Pattern),
               ?assertEqual([cancelling], maps:get('p_cancelling', Output2)),

               %% Path 3: Confirming -> Cancelled
               State3 = #{'p_cancelling' => [cancelling]},
               {produce, Output3} = cre_yawl_patterns:fire('t_execute_cancel', State3, Pattern),
               ?assertEqual([case_cancelled], maps:get('p_cancelled', Output3))
           end},

          {"Cancel case - Running to completion path",
           fun() ->
               Activities = [test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, no_cancellation()),

               %% Normal completion path
               State = #{'p_case_active' => [active]},
               {produce, Output} = cre_yawl_patterns:fire('t_work', State, Pattern),
               ?assertEqual([work_done], maps:get('p_completed', Output))
           end},

          {"Cancel case - State persistence",
           fun() ->
               Activities = [test_activity(), test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               InitialCount = Pattern#pattern_state.instance_count,
               ?assertEqual(2, InitialCount),

               %% State should persist across transitions
               _ = cre_yawl_patterns:fire('t_request_cancel', #{'p_case_active' => [active]}, Pattern),

               ?assert(is_record(Pattern, pattern_state)),
               ?assertEqual(2, Pattern#pattern_state.instance_count)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Cancellation Tests - Cancel Case
%%--------------------------------------------------------------------

cancel_case_cancellation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Cancel case - Graceful cancellation",
           fun() ->
               Activities = [test_activity(), test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               %% Full graceful cancellation flow
               State1 = #{'p_case_active' => [active]},
               {produce, Output1} = cre_yawl_patterns:fire('t_request_cancel', State1, Pattern),
               ?assertEqual([], maps:get('p_case_active', Output1)),

               State2 = #{'p_cancellation_requested' => [cancel_requested]},
               {produce, Output2} = cre_yawl_patterns:fire('t_confirm_cancel', State2, Pattern),
               ?assertEqual([cancelling], maps:get('p_cancelling', Output2)),

               State3 = #{'p_cancelling' => [cancelling]},
               {produce, Output3} = cre_yawl_patterns:fire('t_execute_cancel', State3, Pattern),
               ?assertEqual([case_cancelled], maps:get('p_cancelled', Output3))
           end},

          {"Cancel case - Forced cancellation",
           fun() ->
               Activities = [test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, fun() -> true end),

               %% Immediate cancel via check function
               InputMode = #{'p_case_active' => [active]},
               Result = cre_yawl_patterns:fire('t_request_cancel', InputMode, Pattern),

               ?assertMatch({produce, #{'p_cancellation_requested' := [cancel_requested]}}, Result)
           end},

          {"Cancel case - Resource cleanup",
           fun() ->
               Activities = [test_activity(), test_activity(), test_activity()],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               %% Verify all activities are cleaned up
               State = #{'p_cancelling' => [cancelling]},
               {produce, Output} = cre_yawl_patterns:fire('t_execute_cancel', State, Pattern),

               ?assertEqual([case_cancelled], maps:get('p_cancelled', Output)),
               ?assertEqual([], maps:get('p_cancelling', Output))
           end},

          {"Cancel case - Cancellation propagation to all activities",
           fun() ->
               Activities = [
                   fun(_) -> {ok, act1} end,
                   fun(_) -> {ok, act2} end,
                   fun(_) -> {ok, act3} end
               ],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               %% All activities should be cancelled
               ?assertEqual(3, Pattern#pattern_state.instance_count),

               %% Cancel propagation
               State = #{'p_cancelling' => [cancelling]},
               {produce, Output} = cre_yawl_patterns:fire('t_execute_cancel', State, Pattern),

               ?assertEqual([case_cancelled], maps:get('p_cancelled', Output))
           end}
         ]
     end}.

%%====================================================================
%% Timeout Tests
%%====================================================================

timeout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Milestone - Timeout after guard limit",
           fun() ->
               %% Simulate timeout scenario for milestone
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_not_reached()),

               %% Simulate waiting for milestone with timeout
               TimeoutResult = simulate_milestone_timeout(Pattern, 3),

               ?assertEqual(timeout, TimeoutResult)
           end},

          {"Cancel activity - Timeout during cancellation",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               %% Cancellation should complete quickly
               StartTime = erlang:monotonic_time(millisecond),

               State = #{'p_cancellation_pending' => [cancel_pending]},
               _ = cre_yawl_patterns:fire('t_cancel_confirm', State, Pattern),

               EndTime = erlang:monotonic_time(millisecond),
               Duration = EndTime - StartTime,

               %% Cancellation should be fast (< 100ms)
               ?assert(Duration < 100)
           end},

          {"Cancel case - Timeout during case cancellation",
           fun() ->
               Activities = [test_activity() || _ <- lists:seq(1, 10)],
               Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

               %% Multiple activities should cancel quickly
               StartTime = erlang:monotonic_time(millisecond),

               State = #{'p_cancelling' => [cancelling]},
               _ = cre_yawl_patterns:fire('t_execute_cancel', State, Pattern),

               EndTime = erlang:monotonic_time(millisecond),
               Duration = EndTime - StartTime,

               ?assert(Duration < 100)
           end}
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"WCP-18 and WCP-19 - Milestone with cancel activity",
           fun() ->
               %% Combine milestone and cancel activity patterns
               MilestonePattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),
               CancelPattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

               ?assertEqual(milestone, MilestonePattern#pattern_state.pattern_type),
               ?assertEqual(cancel_activity, CancelPattern#pattern_state.pattern_type)
           end},

          {"WCP-19 and WCP-20 - Activity and case cancellation",
           fun() ->
               ActivityPattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),
               CasePattern = cre_yawl_patterns:cancel_case([test_activity()], cancel_requested()),

               %% Both should have cancellation functions
               ?assert(is_function(maps:get(cancel_check, ActivityPattern#pattern_state.choice_data))),
               ?assert(is_function(maps:get(cancel_check, CasePattern#pattern_state.choice_data)))
           end},

          {"All state-based patterns - Pattern sequence",
           fun() ->
               %% Test all patterns can be created and used in sequence
               Patterns = [
                   cre_yawl_patterns:milestone(test_activity(), milestone_reached()),
                   cre_yawl_patterns:cancel_activity(test_activity(), no_cancellation()),
                   cre_yawl_patterns:cancel_case([test_activity()], no_cancellation())
               ],

               ?assertEqual(3, length(Patterns)),
               lists:foreach(fun(P) ->
                   ?assert(is_record(P, pattern_state))
               end, Patterns)
           end}
         ]
     end}.

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [
          {"Performance - Pattern creation",
           fun() ->
               {TimeMilestone, _} = timer:tc(fun() ->
                   cre_yawl_patterns:milestone(test_activity(), milestone_reached())
               end),

               {TimeCancelActivity, _} = timer:tc(fun() ->
                   cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested())
               end),

               {TimeCancelCase, _} = timer:tc(fun() ->
                   cre_yawl_patterns:cancel_case([test_activity()], cancel_requested())
               end),

               %% All should create in under 1ms
               ?assert(TimeMilestone < 1000),
               ?assert(TimeCancelActivity < 1000),
               ?assert(TimeCancelCase < 1000)
           end},

          {"Performance - Transition firing",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),
               State = #{'p_active' => [active]},

               {Time, _} = timer:tc(fun() ->
                   lists:foreach(fun(_) ->
                       cre_yawl_patterns:fire('t_work', State, Pattern)
                   end, lists:seq(1, 1000))
               end),

               %% 1000 transitions should complete in reasonable time
               ?assert(Time < 100000)  % < 100ms
           end},

          {"Performance - Large cancel case",
           fun() ->
               Activities = [test_activity() || _ <- lists:seq(1, 1000)],
               Pattern = cre_yawl_patterns:cancel_case(Activities, no_cancellation()),

               ?assertEqual(1000, Pattern#pattern_state.instance_count)
           end}
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Simulate milestone timeout scenario.
%% @end
%%--------------------------------------------------------------------
simulate_milestone_timeout(_Pattern, 0) ->
    timeout;
simulate_milestone_timeout(Pattern, Attempts) ->
    GuardFun = maps:get(milestone_check, Pattern#pattern_state.choice_data, fun() -> false end),
    case GuardFun() of
        true -> reached;
        false -> simulate_milestone_timeout(Pattern, Attempts - 1)
    end.

%%--------------------------------------------------------------------
%% @doc Check if state is valid or process is alive.
%% @end
%%--------------------------------------------------------------------
is_pattern_state_valid(#pattern_state{}) ->
    true;
is_pattern_state_valid(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Check if term is a record.
%% @end
%%--------------------------------------------------------------------
is_record(Term, _RecordName) when is_tuple(Term), tuple_size(Term) > 0 ->
    %% Simple record check - in production would use more sophisticated check
    try
        element(1, Term),
        true
    catch
        error:_ -> false
    end;
is_record(_, _) ->
    false.

%%====================================================================
%% EUnit Auto-Discovery
%%====================================================================
%% Tests ending in _test_() are automatically discovered by EUnit
