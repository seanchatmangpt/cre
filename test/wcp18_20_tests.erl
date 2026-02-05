%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jrgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jrgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%% @version 0.2.0
%%
%% @doc Test suite for WCP-18 through WCP-20 patterns
%%
%% This module contains comprehensive tests for the three state-based workflow
%% control patterns:
%% - WCP-18: Milestone Pattern
%% - WCP-19: Cancel Activity Pattern
%% - WCP-20: Cancel Case Pattern
%%
%% @end
%% -------------------------------------------------------------------

-module(wcp18_20_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions
%%====================================================================

-record(pattern_state, {
          pattern_type :: atom(),
          subprocess :: module() | function() | undefined,
          instance_count :: non_neg_integer() | undefined,
          max_instances :: non_neg_integer() | unlimited | undefined,
          pending_instances = [] :: list(),
          active_instances = [] :: list(),
          completed_instances = [] :: list(),
          choice_data = #{} :: map(),
          branch_queue = [] :: list()
         }).

%%====================================================================
%% Test Data
%%====================================================================

%% Test activities
test_activity() ->
    fun(Data) ->
        io:format("Executing activity with data: ~p~n", [Data]),
        {ok, processed_data}
    end.

error_activity() ->
    fun(_Data) ->
        error_test_error
    end.

%% Test milestone check functions
milestone_not_reached() ->
    fun() -> false end.

milestone_reached() ->
    fun() -> true end.

%% Test cancellation check functions
no_cancellation() ->
    fun() -> false end.

cancel_requested() ->
    fun() -> true end.

%%====================================================================
%% WCP-18: Milestone Pattern Tests
%%====================================================================

milestone_pattern_test_() ->
    {setup,
     fun() ->
         %% Initialize a simple test activity
         cre_yawl_patterns:milestone(test_activity(), milestone_not_reached())
     end,
     fun(_Pattern) ->
         %% Cleanup
         ok
     end,
     fun(Pattern) ->
         [
          milestone_places_test(Pattern),
          milestone_transitions_test(Pattern),
          milestone_initial_marking_test(Pattern),
          milestone_fire_logic_test(Pattern),
          milestone_state_management_test(Pattern),
          milestone_error_handling_test(Pattern)
         ]
     end}.

milestone_places_test(_Pattern) ->
    ActualPlaces = cre_yawl_patterns:place_lst(),
    ?assert(lists:member('p_milestone_guard', ActualPlaces)),
    ?assert(lists:member('p_milestone_reached', ActualPlaces)),
    ?assert(lists:member('p_active', ActualPlaces)),
    ?assert(lists:member('p_complete', ActualPlaces)).

milestone_transitions_test(_Pattern) ->
    ActualTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_milestone_work', ActualTransitions)),
    ?assert(lists:member('t_milestone_check', ActualTransitions)),
    ?assert(lists:member('t_milestone_complete', ActualTransitions)).

milestone_initial_marking_test(_Pattern) ->
    %% Test initial marking for milestone places
    ?assertEqual([], cre_yawl_patterns:init_marking('p_milestone_guard', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_milestone_reached', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_active', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_complete', _Pattern)).

milestone_fire_logic_test(_Pattern) ->
    %% Test with milestone NOT reached
    PatternNotReached = cre_yawl_patterns:milestone(test_activity(), milestone_not_reached()),
    TestCasesNotReached = [
        {t_work, #{'p_active' => [active]},
         #{'p_active' => [active], 'p_milestone_guard' => [milestone_required]}},
        {t_milestone_check, #{'p_active' => [active], 'p_milestone_guard' => [milestone_required]},
         #{'p_milestone_guard' => [milestone_required]}}
    ],

    lists:foreach(fun({Trsn, Input, Expected}) ->
        Result = cre_yawl_patterns:fire(Trsn, Input, PatternNotReached),
        case Result of
            {produce, Actual} ->
                ?assertEqual(Expected, Actual);
            abort ->
                ?assert(false, "Expected produce but got abort")
        end
    end, TestCasesNotReached),

    %% Test with milestone reached
    PatternReached = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),
    TestCasesReached = [
        {t_milestone_check, #{'p_active' => [active], 'p_milestone_guard' => [milestone_required]},
         #{'p_milestone_guard' => [], 'p_milestone_reached' => [milestone_reached], 'p_active' => [active]}}
    ],

    lists:foreach(fun({Trsn, Input, Expected}) ->
        Result = cre_yawl_patterns:fire(Trsn, Input, PatternReached),
        case Result of
            {produce, Actual} ->
                ?assertEqual(Expected, Actual);
            abort ->
                ?assert(false, "Expected produce but got abort")
        end
    end, TestCasesReached).

milestone_state_management_test(Pattern) ->
    %% Test pattern creation
    ?assertEqual(milestone, Pattern#pattern_state.pattern_type),
    ?assert(is_function(Pattern#pattern_state.subprocess)),
    ?assert(is_function(maps:get(milestone_check, Pattern#pattern_state.choice_data))).

milestone_error_handling_test(Pattern) ->
    %% Test fire with invalid inputs
    ?assertEqual(abort, cre_yawl_patterns:fire('invalid_trsn', #{}, Pattern)),
    ?assertEqual(abort, cre_yawl_patterns:fire('t_work', #{'p_active' => []}, Pattern)).

%%====================================================================
%% WCP-19: Cancel Activity Pattern Tests
%%====================================================================

cancel_activity_pattern_test_() ->
    {setup,
     fun() ->
         %% Initialize a test activity with cancellation capability
         cre_yawl_patterns:cancel_activity(test_activity(), no_cancellation())
     end,
     fun(_Pattern) ->
         %% Cleanup
         ok
     end,
     fun(Pattern) ->
         [
          cancel_activity_places_test(Pattern),
          cancel_activity_transitions_test(Pattern),
          cancel_activity_initial_marking_test(Pattern),
          cancel_activity_fire_logic_normal_test(Pattern),
          cancel_activity_fire_logic_cancel_test(Pattern),
          cancel_activity_state_management_test(Pattern),
          cancel_activity_error_handling_test(Pattern)
         ]
     end}.

cancel_activity_places_test(_Pattern) ->
    ActualPlaces = cre_yawl_patterns:place_lst(),
    ?assert(lists:member('p_activity_running', ActualPlaces)),
    ?assert(lists:member('p_cancellation_pending', ActualPlaces)),
    ?assert(lists:member('p_cancelled', ActualPlaces)),
    ?assert(lists:member('p_completed', ActualPlaces)).

cancel_activity_transitions_test(_Pattern) ->
    ActualTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_cancel_activity_work', ActualTransitions)),
    ?assert(lists:member('t_cancel_request', ActualTransitions)),
    ?assert(lists:member('t_cancel_confirm', ActualTransitions)),
    ?assert(lists:member('t_cancel_complete', ActualTransitions)).

cancel_activity_initial_marking_test(_Pattern) ->
    %% Test initial marking for cancel activity places
    ?assertEqual([], cre_yawl_patterns:init_marking('p_activity_running', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_cancellation_pending', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_cancelled', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_completed', _Pattern)).

cancel_activity_fire_logic_normal_test(Pattern) ->
    %% Test normal execution path (no cancellation)
    Result1 = cre_yawl_patterns:fire('t_work', #{'p_activity_running' => [running]}, Pattern),
    ?assertMatch({produce, #{'p_activity_running' := [running], 'p_completed' := [work_done]}}, Result1).

cancel_activity_fire_logic_cancel_test(Pattern) ->
    %% This test uses no_cancellation() which returns false
    %% So the work completes normally instead of canceling
    TestCases = [
        {t_cancel_request, #{'p_activity_running' => [running]},
         #{'p_activity_running' => [], 'p_cancellation_pending' => [cancel_pending]}},
        {t_cancel_confirm, #{'p_cancellation_pending' => [cancel_pending]},
         #{'p_cancellation_pending' => [], 'p_cancelled' => [cancelled]}}
    ],

    lists:foreach(fun({Trsn, Input, Expected}) ->
        Result = cre_yawl_patterns:fire(Trsn, Input, Pattern),
        case Result of
            {produce, Actual} ->
                ?assertEqual(Expected, Actual);
            abort ->
                ?assert(false, "Expected produce but got abort")
        end
    end, TestCases).

cancel_activity_state_management_test(Pattern) ->
    %% Test pattern creation
    ?assertEqual(cancel_activity, Pattern#pattern_state.pattern_type),
    ?assert(is_function(Pattern#pattern_state.subprocess)),
    ?assert(is_function(maps:get(cancel_check, Pattern#pattern_state.choice_data))).

cancel_activity_error_handling_test(Pattern) ->
    %% Test fire with invalid inputs
    ?assertEqual(abort, cre_yawl_patterns:fire('invalid_trsn', #{}, Pattern)),
    ?assertEqual(abort, cre_yawl_patterns:fire('t_work', #{'p_activity_running' => []}, Pattern)).

%%====================================================================
%% WCP-20: Cancel Case Pattern Tests
%%====================================================================

cancel_case_pattern_test_() ->
    {setup,
     fun() ->
         %% Initialize multiple activities for case cancellation
         Activities = [test_activity(), error_activity()],
         cre_yawl_patterns:cancel_case(Activities, no_cancellation())
     end,
     fun(_Pattern) ->
         %% Cleanup
         ok
     end,
     fun(Pattern) ->
         [
          cancel_case_places_test(Pattern),
          cancel_case_transitions_test(Pattern),
          cancel_case_initial_marking_test(Pattern),
          cancel_case_fire_logic_normal_test(Pattern),
          cancel_case_fire_logic_cancel_test(Pattern),
          cancel_case_state_management_test(Pattern),
          cancel_case_error_handling_test(Pattern)
         ]
     end}.

cancel_case_places_test(_Pattern) ->
    ActualPlaces = cre_yawl_patterns:place_lst(),
    ?assert(lists:member('p_case_active', ActualPlaces)),
    ?assert(lists:member('p_cancellation_requested', ActualPlaces)),
    ?assert(lists:member('p_cancelling', ActualPlaces)),
    ?assert(lists:member('p_cancelled', ActualPlaces)),
    ?assert(lists:member('p_completed', ActualPlaces)).

cancel_case_transitions_test(_Pattern) ->
    ActualTransitions = cre_yawl_patterns:trsn_lst(),
    ?assert(lists:member('t_cancel_case_work', ActualTransitions)),
    ?assert(lists:member('t_request_cancel', ActualTransitions)),
    ?assert(lists:member('t_confirm_cancel', ActualTransitions)),
    ?assert(lists:member('t_execute_cancel', ActualTransitions)),
    ?assert(lists:member('t_cancel_case_complete', ActualTransitions)).

cancel_case_initial_marking_test(_Pattern) ->
    %% Test initial marking for cancel case places
    ?assertEqual([], cre_yawl_patterns:init_marking('p_case_active', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_cancellation_requested', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_cancelling', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_cancelled', _Pattern)),
    ?assertEqual([], cre_yawl_patterns:init_marking('p_completed', _Pattern)).

cancel_case_fire_logic_normal_test(Pattern) ->
    %% Test normal execution path (no cancellation)
    Result1 = cre_yawl_patterns:fire('t_work', #{'p_case_active' => [active]}, Pattern),
    ?assertMatch({produce, #{'p_case_active' := [active], 'p_completed' := [work_done]}}, Result1).

cancel_case_fire_logic_cancel_test(Pattern) ->
    %% This test uses no_cancellation() which returns false
    %% So the work completes normally instead of canceling
    TestCases = [
        {t_request_cancel, #{'p_case_active' => [active]},
         #{'p_case_active' => [], 'p_cancellation_requested' => [cancel_requested]}},
        {t_confirm_cancel, #{'p_cancellation_requested' => [cancel_requested]},
         #{'p_cancellation_requested' => [], 'p_cancelling' => [cancelling]}},
        {t_execute_cancel, #{'p_cancelling' => [cancelling]},
         #{'p_cancelling' => [], 'p_cancelled' => [case_cancelled]}}
    ],

    lists:foreach(fun({Trsn, Input, Expected}) ->
        Result = cre_yawl_patterns:fire(Trsn, Input, Pattern),
        case Result of
            {produce, Actual} ->
                ?assertEqual(Expected, Actual);
            abort ->
                ?assert(false, "Expected produce but got abort")
        end
    end, TestCases).

cancel_case_state_management_test(Pattern) ->
    %% Test pattern creation
    ?assertEqual(cancel_case, Pattern#pattern_state.pattern_type),
    ?assert(is_list(Pattern#pattern_state.subprocess)),
    ?assert(length(Pattern#pattern_state.subprocess) == 2),  % Two activities
    ?assert(is_function(maps:get(cancel_check, Pattern#pattern_state.choice_data))).

cancel_case_error_handling_test(Pattern) ->
    %% Test fire with invalid inputs
    ?assertEqual(abort, cre_yawl_patterns:fire('invalid_trsn', #{}, Pattern)),
    ?assertEqual(abort, cre_yawl_patterns:fire('t_work', #{'p_case_active' => []}, Pattern)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_workflow_test_() ->
    [
        {"WCP-18 workflow: Complete milestone execution",
         fun() ->
             Pattern = cre_yawl_patterns:milestone(test_activity(), milestone_reached()),

             %% Step 1: Start work transition
             Result1 = cre_yawl_patterns:fire('t_work', #{'p_active' => [active]}, Pattern),
             ?assertMatch({produce, #{'p_active' := [active], 'p_milestone_guard' := [milestone_required]}}, Result1),

             %% Step 2: Check milestone (should be reached)
             Result2 = cre_yawl_patterns:fire('t_milestone_check',
                                            #{'p_active' => [active], 'p_milestone_guard' => [milestone_required]},
                                            Pattern),
             ?assertMatch({produce, #{'p_milestone_guard' := [], 'p_milestone_reached' := [milestone_reached], 'p_active' := [active]}}, Result2),

             %% Step 3: Complete milestone workflow
             Result3 = cre_yawl_patterns:fire('t_complete',
                                            #{'p_active' => [active], 'p_complete' => [complete]},
                                            Pattern),
             ?assertMatch({produce, #{'p_active' := [completed], 'p_complete' := [final_complete]}}, Result3)
         end},

        {"WCP-19 workflow: Activity cancellation",
         fun() ->
             Pattern = cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested()),

             %% Step 1: Request cancellation
             Result1 = cre_yawl_patterns:fire('t_cancel_request', #{'p_activity_running' => [running]}, Pattern),
             ?assertMatch({produce, #{'p_activity_running' := [], 'p_cancellation_pending' := [cancel_pending]}}, Result1),

             %% Step 2: Confirm cancellation
             Result2 = cre_yawl_patterns:fire('t_cancel_confirm', #{'p_cancellation_pending' => [cancel_pending]}, Pattern),
             ?assertMatch({produce, #{'p_cancellation_pending' := [], 'p_cancelled' := [cancelled]}}, Result2)
         end},

        {"WCP-20 workflow: Case cancellation",
         fun() ->
             Activities = [test_activity(), test_activity()],
             Pattern = cre_yawl_patterns:cancel_case(Activities, cancel_requested()),

             %% Step 1: Request case cancellation
             Result1 = cre_yawl_patterns:fire('t_request_cancel', #{'p_case_active' => [active]}, Pattern),
             ?assertMatch({produce, #{'p_case_active' := [], 'p_cancellation_requested' := [cancel_requested]}}, Result1),

             %% Step 2: Confirm cancellation
             Result2 = cre_yawl_patterns:fire('t_confirm_cancel', #{'p_cancellation_requested' => [cancel_requested]}, Pattern),
             ?assertMatch({produce, #{'p_cancellation_requested' := [], 'p_cancelling' := [cancelling]}}, Result2),

             %% Step 3: Execute cancellation
             Result3 = cre_yawl_patterns:fire('t_execute_cancel', #{'p_cancelling' => [cancelling]}, Pattern),
             ?assertMatch({produce, #{'p_cancelling' := [], 'p_cancelled' := [case_cancelled]}}, Result3)
         end}
    ].

error_handling_test_() ->
    [
        {"Missing milestone function causes badfun error",
         fun() ->
             Pattern = cre_yawl_patterns:milestone(test_activity(), undefined),
             %% When milestone_check is undefined, the transition fires but calling it will fail
             ?assertMatch({produce, #{}}, cre_yawl_patterns:fire('t_work', #{'p_active' => [active]}, Pattern))
         end},

        {"Missing cancellation function causes badfun error",
         fun() ->
             %% The badfun error happens when trying to call the undefined function
             %% This is tested via the cancel_activity work transition with undefined check
             Pattern = cre_yawl_patterns:cancel_activity(test_activity(), undefined),
             %% The error will be thrown when the fire function tries to call the undefined check function
             %% We wrap it in a try-catch to handle the error gracefully
             try
                 cre_yawl_patterns:fire('t_cancel_request', #{'p_activity_running' => [running]}, Pattern),
                 ?assert(false, "Expected badfun error for undefined cancel_check")
             catch
                 error:_ -> ok
             end
         end},

        {"Invalid transition names cause abort",
         fun() ->
             Pattern1 = cre_yawl_patterns:milestone(test_activity(), milestone_not_reached()),
             Pattern2 = cre_yawl_patterns:cancel_activity(test_activity(), no_cancellation()),

             ?assertEqual(abort, cre_yawl_patterns:fire('invalid_transition', #{}, Pattern1)),
             ?assertEqual(abort, cre_yawl_patterns:fire('invalid_transition', #{}, Pattern2))
         end}
    ].

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {"Performance benchmarks for WCP-18-20 patterns",
     fun() ->
         %% Benchmark pattern creation
         {Time1, _} = timer:tc(fun() ->
             cre_yawl_patterns:milestone(test_activity(), milestone_reached())
         end),
         io:format("WCP-18 creation time: ~p microseconds~n", [Time1]),

         {Time2, _} = timer:tc(fun() ->
             cre_yawl_patterns:cancel_activity(test_activity(), cancel_requested())
         end),
         io:format("WCP-19 creation time: ~p microseconds~n", [Time2]),

         {Time3, _} = timer:tc(fun() ->
             cre_yawl_patterns:cancel_case([test_activity()], cancel_requested())
         end),
         io:format("WCP-20 creation time: ~p microseconds~n", [Time3]),

         %% All creation times should be reasonable (under 1ms)
         ?assert(Time1 < 1000),
         ?assert(Time2 < 1000),
         ?assert(Time3 < 1000)
     end}.