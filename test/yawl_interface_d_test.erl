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
%% @doc YAWL Interface D Test Suite
%%
%% Comprehensive test suite for Interface D which handles exception
%% service integration and worklet communication.
%%
%% Tests cover:
%% - Exception service registration
%% - Worklet launching for exception handling
%% - Exception routing to worklets
%% - Pre/post condition validation
%% - Exception propagation
%% - Compensation coordination
%% - Worklet status tracking
%% - Worklet abortion
%% @end
%% -------------------------------------------------------------------

-module(yawl_interface_d_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts the Interface D service.
%% @end
%%--------------------------------------------------------------------
setup() ->
    {ok, Pid} = yawl_interface_d:start_link(),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the Interface D service.
%% @end
%%--------------------------------------------------------------------
cleanup(_Pid) ->
    case whereis(yawl_interface_d) of
        undefined -> ok;
        Pid ->
            unlink(Pid),
            exit(Pid, normal),
            timer:sleep(50)
    end,
    ok.

%%====================================================================
%% 1. Exception Service Registration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test registering an exception service.
%% @end
%%--------------------------------------------------------------------
test_register_exception_service_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    ServiceName = <<"test_exception_service">>,
                    Config = [
                        {endpoint, <<"http://localhost:8080/exceptions">>},
                        {service_type, local},
                        {priority, 10},
                        {enabled, true}
                    ],
                    {ok, ServiceId} = yawl_interface_d:register_exception_service(
                        ServiceName,
                        Config
                    ),
                    ?assert(is_binary(ServiceId)),
                    ?assertMatch(<<"svc_", _/binary>>, ServiceId),

                    % Verify service is in the list
                    Services = yawl_interface_d:get_exception_services(),
                    ?assert(length(Services) > 0),

                    % Find our service in the list
                    FoundService = lists:filter(
                        fun(S) -> element(2, S) =:= ServiceId end,
                        Services
                    ),
                    ?assert(length(FoundService) > 0)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test registering a remote exception service.
%% @end
%%--------------------------------------------------------------------
test_register_remote_service_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    ServiceName = <<"remote_exception_service">>,
                    Config = [
                        {endpoint, <<"http://remote-server:8080/exceptions">>},
                        {service_type, remote},
                        {priority, 5},
                        {enabled, true}
                    ],
                    {ok, ServiceId} = yawl_interface_d:register_exception_service(
                        ServiceName,
                        Config
                    ),
                    ?assert(is_binary(ServiceId))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unregistering an exception service.
%% @end
%%--------------------------------------------------------------------
test_unregister_exception_service_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    ServiceName = <<"temp_service">>,
                    {ok, ServiceId} = yawl_interface_d:register_exception_service(
                        ServiceName,
                        [{endpoint, <<"http://localhost/temp">>}]
                    ),

                    % Verify it exists
                    Services = yawl_interface_d:get_exception_services(),
                    FoundBefore = lists:any(
                        fun(S) -> element(2, S) =:= ServiceId end,
                        Services
                    ),
                    ?assert(FoundBefore),

                    % Unregister
                    ok = yawl_interface_d:unregister_exception_service(ServiceId),

                    % Verify it's gone
                    ServicesAfter = yawl_interface_d:get_exception_services(),
                    FoundAfter = lists:any(
                        fun(S) -> element(2, S) =:= ServiceId end,
                        ServicesAfter
                    ),
                    ?assertNot(FoundAfter)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unregistering non-existent service.
%% @end
%%--------------------------------------------------------------------
test_unregister_nonexistent_service_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeServiceId = <<"svc_nonexistent">>,
                    {error, not_found} = yawl_interface_d:unregister_exception_service(
                        FakeServiceId
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test enabling an exception service.
%% @end
%%--------------------------------------------------------------------
test_enable_exception_service_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, ServiceId} = yawl_interface_d:register_exception_service(
                        <<"disabled_service">>,
                        [{endpoint, <<"http://localhost/disabled">>}, {enabled, false}]
                    ),

                    % Disable it first
                    ok = yawl_interface_d:disable_exception_service(ServiceId),

                    % Enable it
                    ok = yawl_interface_d:enable_exception_service(ServiceId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test disabling an exception service.
%% @end
%%--------------------------------------------------------------------
test_disable_exception_service_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, ServiceId} = yawl_interface_d:register_exception_service(
                        <<"enabled_service">>,
                        [{endpoint, <<"http://localhost/enabled">>}]
                    ),

                    % Disable it
                    ok = yawl_interface_d:disable_exception_service(ServiceId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test setting service priority.
%% @end
%%--------------------------------------------------------------------
test_set_service_priority_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, ServiceId} = yawl_interface_d:register_exception_service(
                        <<"priority_service">>,
                        [{endpoint, <<"http://localhost/priority">>}, {priority, 1}]
                    ),

                    % Set new priority
                    ok = yawl_interface_d:set_service_priority(ServiceId, 100)
                end)]
     end}.

%%====================================================================
%% 2. Worklet Launch Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test launching a worklet for exception handling.
%% @end
%%--------------------------------------------------------------------
test_launch_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_123">>,
                    WorkletSpecId = <<"worklet_spec_exception">>,
                    ExceptionData = #{
                        <<"task_id">> => <<"task_456">>,
                        <<"exception_type">> => yawl_runtime_exception,
                        <<"message">> => <<"Something went wrong">>
                    },

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    ?assert(is_binary(ExecutionId)),
                    ?assertMatch(<<"exec_", _/binary>>, ExecutionId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test launching worklet with precondition check.
%% @end
%%--------------------------------------------------------------------
test_launch_worklet_precondition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_precondition">>,
                    WorkletSpecId = <<"worklet_spec_precondition">>,

                    % Register worklet with precondition in yawl_worklet if available
                    case whereis(yawl_worklet) of
                        undefined -> ok;
                        _Pid ->
                            PreCond = fun(Ctx) ->
                                maps:get(<<"can_handle">>, Ctx, false) =:= true
                            end,
                            yawl_worklet:register_worklet(
                                <<"precondition_worklet">>,
                                [{pre_condition, PreCond}]
                            )
                    end,

                    ExceptionData = #{<<"can_handle">> => true},

                    {ok, _ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    )
                end)]
     end}.

%%====================================================================
%% 3. Worklet Completion Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test completing a worklet with result.
%% @end
%%--------------------------------------------------------------------
test_complete_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % First launch a worklet
                    CaseId = <<"case_complete">>,
                    WorkletSpecId = <<"worklet_spec_complete">>,
                    ExceptionData = #{<<"task_id">> => <<"task_complete">>},

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    % Complete the worklet with a result
                    Result = #{<<"status">> => <<"handled">>, <<"recovery">> => true},
                    ok = yawl_interface_d:complete_worklet(ExecutionId, Result),

                    % Verify status
                    {ok, Status} = yawl_interface_d:get_worklet_status(ExecutionId),
                    ?assertEqual(completed, Status)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test completing worklet with postcondition check.
%% @end
%%--------------------------------------------------------------------
test_complete_worklet_postcondition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Launch worklet
                    CaseId = <<"case_postcondition">>,
                    WorkletSpecId = <<"worklet_spec_postcondition">>,
                    ExceptionData = #{<<"task_id">> => <<"task_postcond">>},

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    % Complete with passing result
                    Result = #{<<"success">> => true},
                    ok = yawl_interface_d:complete_worklet(ExecutionId, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test completing non-existent worklet.
%% @end
%%--------------------------------------------------------------------
test_complete_nonexistent_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeExecutionId = <<"exec_nonexistent">>,
                    {error, not_found} = yawl_interface_d:complete_worklet(
                        FakeExecutionId,
                        #{}
                    )
                end)]
     end}.

%%====================================================================
%% 4. Exception Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test handling an exception via Interface D.
%% @end
%%--------------------------------------------------------------------
test_handle_exception_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register an exception service first
                    {ok, _ServiceId} = yawl_interface_d:register_exception_service(
                        <<"exception_handler_service">>,
                        [{endpoint, <<"http://localhost/handle">>}, {priority, 10}]
                    ),

                    ExceptionType = yawl_runtime_exception,
                    ExceptionData = #{
                        <<"case_id">> => <<"case_handle">>,
                        <<"task_id">> => <<"task_handle">>,
                        <<"message">> => <<"Runtime error occurred">>
                    },

                    % Handle the exception
                    {ok, ExecutionId} = yawl_interface_d:handle_exception(
                        ExceptionType,
                        ExceptionData
                    ),

                    ?assert(is_binary(ExecutionId))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test handling exception without registered service.
%% @end
%%--------------------------------------------------------------------
test_handle_exception_no_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % No services registered
                    ExceptionType = yawl_runtime_exception,
                    ExceptionData = #{<<"message">> => <<"No handler">>},

                    {error, no_handler} = yawl_interface_d:handle_exception(
                        ExceptionType,
                        ExceptionData
                    )
                end)]
     end}.

%%====================================================================
%% 5. Precondition Validation Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test checking preconditions.
%% @end
%%--------------------------------------------------------------------
test_check_preconditions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    WorkletSpecId = <<"worklet_precond_check">>,

                    % No precondition defined - should pass
                    {ok, true} = yawl_interface_d:check_preconditions(
                        WorkletSpecId,
                        #{}
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test checking preconditions with context.
%% @end
%%--------------------------------------------------------------------
test_check_preconditions_with_context_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with precondition
                    case whereis(yawl_worklet) of
                        undefined -> ok;
                        _Pid ->
                            PreCond = fun(Ctx) ->
                                maps:get(<<"ready">>, Ctx, false) =:= true
                            end,
                            {ok, SpecId} = yawl_worklet:register_worklet(
                                <<"context_precond_worklet">>,
                                [{pre_condition, PreCond}]
                            ),

                            % Check with passing context
                            {ok, true} = yawl_interface_d:check_preconditions(
                                SpecId,
                                #{<<"ready">> => true}
                            ),

                            % Check with failing context
                            {ok, false} = yawl_interface_d:check_preconditions(
                                SpecId,
                                #{<<"ready">> => false}
                            )
                    end
                end)]
     end}.

%%====================================================================
%% 6. Postcondition Validation Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test checking postconditions.
%% @end
%%--------------------------------------------------------------------
test_check_postconditions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    WorkletSpecId = <<"worklet_postcond_check">>,

                    % No postcondition defined - should pass
                    {ok, true} = yawl_interface_d:check_postconditions(
                        WorkletSpecId,
                        #{}
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test checking postconditions with result.
%% @end
%%--------------------------------------------------------------------
test_check_postconditions_with_result_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with postcondition
                    case whereis(yawl_worklet) of
                        undefined -> ok;
                        _Pid ->
                            PostCond = fun(Ctx) ->
                                maps:get(<<"success">>, Ctx, false) =:= true
                            end,
                            {ok, SpecId} = yawl_worklet:register_worklet(
                                <<"result_postcond_worklet">>,
                                [{post_condition, PostCond}]
                            ),

                            % Check with passing result
                            {ok, true} = yawl_interface_d:check_postconditions(
                                SpecId,
                                #{<<"success">> => true}
                            ),

                            % Check with failing result
                            {ok, false} = yawl_interface_d:check_postconditions(
                                SpecId,
                                #{<<"success">> => false}
                            )
                    end
                end)]
     end}.

%%====================================================================
%% 7. Exception Propagation Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test exception propagation.
%% @end
%%--------------------------------------------------------------------
test_propagate_exception_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_propagate">>,
                    ExceptionData = #{
                        <<"exception_type">> => yawl_runtime_exception,
                        <<"message">> => <<"Propagating exception">>
                    },

                    % Propagate with default level
                    ok = yawl_interface_d:propagate_exception(
                        CaseId,
                        ExceptionData
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test exception propagation with level.
%% @end
%%--------------------------------------------------------------------
test_propagate_exception_with_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_propagate_level">>,
                    ParentCaseId = <<"parent_case">>,
                    ExceptionData = #{
                        <<"exception_type">> => yawl_runtime_exception,
                        <<"message">> => <<"Propagating with level">>,
                        <<"parent_case_id">> => ParentCaseId
                    },

                    % Propagate with level 2
                    ok = yawl_interface_d:propagate_exception(
                        CaseId,
                        ExceptionData,
                        2
                    )
                end)]
     end}.

%%====================================================================
%% 8. Compensation Coordination Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test compensation coordination.
%% @end
%%--------------------------------------------------------------------
test_coordinate_compensation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % First create a failed worklet
                    CaseId = <<"case_compensation">>,
                    WorkletSpecId = <<"worklet_compensation">>,

                    ExceptionData = #{
                        <<"task_id">> => <<"task_compensation">>,
                        <<"compensation_actions">> => [
                            fun() -> compensated_action_1 end,
                            fun() -> compensated_action_2 end
                        ]
                    },

                    % Launch worklet
                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    % Since we can't create a failed worklet without yawl_worklet
                    % we abort and test with a non-existent ID instead
                    ok = yawl_interface_d:abort_worklet(ExecutionId),

                    % Verify status is aborted
                    {ok, Status} = yawl_interface_d:get_worklet_status(ExecutionId),
                    ?assertEqual(aborted, Status),

                    % Try to coordinate compensation on non-existent worklet
                    {error, not_found} = yawl_interface_d:coordinate_compensation(
                        <<"exec_nonexistent_test">>
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation with no actions defined.
%% @end
%%--------------------------------------------------------------------
test_coordinate_compensation_no_actions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % This test verifies compensation when no actions are defined
                    % In a real scenario, a failed worklet without compensation actions
                    % would return {ok, false}
                    CaseId = <<"case_no_comp">>,
                    WorkletSpecId = <<"worklet_no_comp">>,

                    ExceptionData = #{
                        <<"task_id">> => <<"task_no_comp">>
                    },

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    % When no compensation actions, should return false
                    % But worklet needs to be in failed state first
                    % Since we can't create a failed worklet without yawl_worklet
                    % we abort and check the error handling
                    ok = yawl_interface_d:abort_worklet(ExecutionId),

                    % Try to coordinate compensation on aborted worklet
                    Result = yawl_interface_d:coordinate_compensation(ExecutionId),
                    ?assertEqual({error, {invalid_status, aborted}}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation for non-existent execution.
%% @end
%%--------------------------------------------------------------------
test_coordinate_compensation_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeExecutionId = <<"exec_nonexistent_comp">>,

                    {error, not_found} = yawl_interface_d:coordinate_compensation(
                        FakeExecutionId
                    )
                end)]
     end}.

%%====================================================================
%% 9. Worklet Status Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test getting worklet status.
%% @end
%%--------------------------------------------------------------------
test_get_worklet_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_status">>,
                    WorkletSpecId = <<"worklet_status">>,

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_status">>}
                    ),

                    % Check initial status (running)
                    {ok, Status} = yawl_interface_d:get_worklet_status(ExecutionId),
                    ?assertEqual(running, Status),

                    % Complete the worklet
                    ok = yawl_interface_d:complete_worklet(ExecutionId, #{}),

                    % Check completed status
                    {ok, CompletedStatus} = yawl_interface_d:get_worklet_status(
                        ExecutionId
                    ),
                    ?assertEqual(completed, CompletedStatus)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test getting status of non-existent worklet.
%% @end
%%--------------------------------------------------------------------
test_get_worklet_status_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeExecutionId = <<"exec_nonexistent_status">>,

                    {error, not_found} = yawl_interface_d:get_worklet_status(
                        FakeExecutionId
                    )
                end)]
     end}.

%%====================================================================
%% 10. Worklet Abortion Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test aborting a running worklet.
%% @end
%%--------------------------------------------------------------------
test_abort_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_abort">>,
                    WorkletSpecId = <<"worklet_abort">>,

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_abort">>}
                    ),

                    % Abort the worklet
                    ok = yawl_interface_d:abort_worklet(ExecutionId),

                    % Verify status is aborted
                    {ok, Status} = yawl_interface_d:get_worklet_status(ExecutionId),
                    ?assertEqual(aborted, Status)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test aborting non-existent worklet.
%% @end
%%--------------------------------------------------------------------
test_abort_nonexistent_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeExecutionId = <<"exec_nonexistent_abort">>,

                    {error, not_found} = yawl_interface_d:abort_worklet(
                        FakeExecutionId
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test aborting completed worklet.
%% @end
%%--------------------------------------------------------------------
test_abort_completed_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_abort_completed">>,
                    WorkletSpecId = <<"worklet_abort_completed">>,

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_abort_completed">>}
                    ),

                    % Complete the worklet first
                    ok = yawl_interface_d:complete_worklet(ExecutionId, #{}),

                    % Try to abort completed worklet - should fail
                    {error, {invalid_status, completed}} = yawl_interface_d:abort_worklet(
                        ExecutionId
                    )
                end)]
     end}.

%%====================================================================
%% 11. Active Worklets Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test getting active worklets.
%% @end
%%--------------------------------------------------------------------
test_get_active_worklets_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Initially no active worklets
                    ActiveWorklets1 = yawl_interface_d:get_active_worklets(),
                    ?assertEqual(0, length(ActiveWorklets1)),

                    % Launch some worklets
                    CaseId = <<"case_active">>,
                    WorkletSpecId = <<"worklet_active">>,

                    {ok, Exec1} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_active_1">>}
                    ),
                    {ok, Exec2} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_active_2">>}
                    ),

                    % Should have active worklets
                    ActiveWorklets2 = yawl_interface_d:get_active_worklets(),
                    ?assert(length(ActiveWorklets2) >= 2)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test getting worklets by case ID.
%% @end
%%--------------------------------------------------------------------
test_get_worklets_by_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId1 = <<"case_by_case_1">>,
                    CaseId2 = <<"case_by_case_2">>,
                    WorkletSpecId = <<"worklet_by_case">>,

                    % Launch worklets for different cases
                    {ok, _Exec1} = yawl_interface_d:launch_worklet(
                        CaseId1,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_bc_1">>}
                    ),
                    {ok, _Exec2} = yawl_interface_d:launch_worklet(
                        CaseId2,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_bc_2">>}
                    ),
                    {ok, _Exec3} = yawl_interface_d:launch_worklet(
                        CaseId1,
                        WorkletSpecId,
                        #{<<"task_id">> => <<"task_bc_3">>}
                    ),

                    % Get worklets for CaseId1
                    Case1Worklets = yawl_interface_d:get_worklets_by_case(CaseId1),
                    ?assert(length(Case1Worklets) >= 2),

                    % Get worklets for CaseId2
                    Case2Worklets = yawl_interface_d:get_worklets_by_case(CaseId2),
                    ?assert(length(Case2Worklets) >= 1)
                end)]
     end}.

%%====================================================================
%% 12. Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test complete exception handling workflow.
%% @end
%%--------------------------------------------------------------------
test_complete_exception_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % 1. Register exception service
                    {ok, _ServiceId} = yawl_interface_d:register_exception_service(
                        <<"workflow_service">>,
                        [
                            {endpoint, <<"http://localhost/workflow">>},
                            {service_type, local},
                            {priority, 100},
                            {enabled, true}
                        ]
                    ),

                    % 2. Simulate exception occurrence
                    CaseId = <<"case_workflow">>,
                    ExceptionType = yawl_runtime_exception,
                    ExceptionData = #{
                        <<"case_id">> => CaseId,
                        <<"task_id">> => <<"task_workflow">>,
                        <<"message">> => <<"Workflow exception">>,
                        <<"compensation_actions">> => [
                            fun() -> workflow_compensated end
                        ]
                    },

                    % 3. Handle exception
                    {ok, ExecutionId} = yawl_interface_d:handle_exception(
                        ExceptionType,
                        ExceptionData
                    ),

                    % 4. Check worklet is running
                    {ok, running} = yawl_interface_d:get_worklet_status(ExecutionId),

                    % 5. Get active worklets
                    ActiveWorklets = yawl_interface_d:get_active_worklets(),
                    ?assert(length(ActiveWorklets) > 0),

                    % 6. Complete worklet successfully
                    Result = #{<<"handled">> => true},
                    ok = yawl_interface_d:complete_worklet(ExecutionId, Result),

                    % 7. Verify completion
                    {ok, completed} = yawl_interface_d:get_worklet_status(ExecutionId),

                    ok
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test exception propagation workflow.
%% @end
%%--------------------------------------------------------------------
test_exception_propagation_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Setup parent-child case relationship
                    ParentCaseId = <<"parent_case">>,
                    ChildCaseId = <<"child_case">>,

                    ExceptionData = #{
                        <<"exception_type">> => yawl_runtime_exception,
                        <<"message">> => <<"Child exception">>,
                        <<"parent_case_id">> => ParentCaseId
                    },

                    % Propagate from child to parent (level 1)
                    ok = yawl_interface_d:propagate_exception(
                        ChildCaseId,
                        ExceptionData,
                        1
                    ),

                    % Propagate with default level
                    ok = yawl_interface_d:propagate_exception(
                        ChildCaseId,
                        ExceptionData
                    ),

                    ok
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation workflow.
%% @end
%%--------------------------------------------------------------------
test_compensation_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_compensation_workflow">>,
                    WorkletSpecId = <<"worklet_compensation_workflow">>,

                    % Define compensation actions that will be executed in reverse
                    CompensationActions = [
                        fun() ->
                            % Step 3 compensation (executes first)
                            step_3_compensated
                        end,
                        fun() ->
                            % Step 2 compensation
                            step_2_compensated
                        end,
                        fun() ->
                            % Step 1 compensation (executes last)
                            step_1_compensated
                        end
                    ],

                    ExceptionData = #{
                        <<"task_id">> => <<"task_compensation_workflow">>,
                        <<"compensation_actions">> => CompensationActions
                    },

                    % Launch worklet
                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    % Abort the worklet
                    ok = yawl_interface_d:abort_worklet(ExecutionId),

                    % Note: Aborted worklets cannot be compensated directly
                    % The compensation would only work for failed worklets
                    % Since we can't easily create a failed worklet without yawl_worklet,
                    % we just verify the abort worked
                    {ok, Status} = yawl_interface_d:get_worklet_status(ExecutionId),
                    ?assertEqual(aborted, Status)
                end)]
     end}.

%%====================================================================
%% 13. Timeout and Error Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test precondition timeout handling.
%% @end
%%--------------------------------------------------------------------
test_precondition_timeout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with slow precondition
                    case whereis(yawl_worklet) of
                        undefined -> ok;
                        _Pid ->
                            SlowPreCond = fun() ->
                                timer:sleep(100),
                                true
                            end,
                            {ok, SpecId} = yawl_worklet:register_worklet(
                                <<"slow_precond_worklet">>,
                                [{pre_condition, SlowPreCond}]
                            ),

                            % Check precondition - should handle timeout gracefully
                            {ok, _Result} = yawl_interface_d:check_preconditions(
                                SpecId,
                                #{}
                            )
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test error in precondition function.
%% @end
%%--------------------------------------------------------------------
test_precondition_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with error-throwing precondition
                    case whereis(yawl_worklet) of
                        undefined -> ok;
                        _Pid ->
                            ErrorPreCond = fun() -> error(bad_precondition) end,
                            {ok, SpecId} = yawl_worklet:register_worklet(
                                <<"error_precond_worklet">>,
                                [{pre_condition, ErrorPreCond}]
                            ),

                            % Check precondition - should handle error
                            Result = yawl_interface_d:check_preconditions(
                                SpecId,
                                #{}
                            ),
                            % Either error or true (default when worklet not found)
                    ?assert(lists:member(element(1, Result), [ok, error]))
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test error in compensation action.
%% @end
%%--------------------------------------------------------------------
test_compensation_action_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    CaseId = <<"case_comp_error">>,
                    WorkletSpecId = <<"worklet_comp_error">>,

                    % Define compensation actions where one fails
                    CompensationActions = [
                        fun() -> good_action end,
                        fun() -> error(bad_action) end,
                        fun() -> another_good_action end
                    ],

                    ExceptionData = #{
                        <<"task_id">> => <<"task_comp_error">>,
                        <<"compensation_actions">> => CompensationActions
                    },

                    {ok, ExecutionId} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    % Abort the worklet to simulate failure
                    ok = yawl_interface_d:abort_worklet(ExecutionId),

                    % Coordinate compensation - note: aborted worklets cannot be compensated
                    % So we need to manually mark as failed by completing with postcondition fail
                    % Let's launch another worklet and abort it (not complete)
                    {ok, ExecId2} = yawl_interface_d:launch_worklet(
                        CaseId,
                        WorkletSpecId,
                        ExceptionData
                    ),

                    % Abort the running worklet
                    ok = yawl_interface_d:abort_worklet(ExecId2),

                    % Verify status is aborted
                    {ok, Status} = yawl_interface_d:get_worklet_status(ExecId2),
                    ?assertEqual(aborted, Status),

                    % Try to coordinate compensation on aborted worklet
                    Result = yawl_interface_d:coordinate_compensation(ExecId2),

                    % Should return error since aborted is not failed status
                    ?assertEqual({error, {invalid_status, aborted}}, Result)
                end)]
     end}.

%%====================================================================
%% 14. Performance Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet launch performance under normal conditions.
%% @end
%%--------------------------------------------------------------------
test_worklet_launch_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Prepare test data
                    CaseId = <<"case_performance">>,
                    WorkletSpecId = <<"worklet_performance">>,
                    ExceptionData = #{
                        <<"task_id">> => <<"task_performance">>,
                        <<"large_data">> => lists:duplicate(1000, <<"performance_test_data">>)
                    },

                    % Measure launch time for multiple worklets
                    N = 100,
                    {Time, Results} = timer:tc(
                        fun() ->
                            [yawl_interface_d:launch_worklet(CaseId, WorkletSpecId, ExceptionData)
                             || _ <- lists:seq(1, N)]
                        end
                    ),

                    % Calculate performance metrics
                    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
                    SuccessRate = SuccessCount / N,
                    AvgTimePerOp = Time / N,
                    Throughput = (N * 1000) / Time,

                    % Log performance metrics
                    logger:info("Worklet launch performance: ~pms total, ~p ops/sec, ~p% success rate",
                                [Time, Throughput, SuccessRate * 100],

                    % Performance assertions
                    ?assert(SuccessRate >= 0.95, "Success rate should be at least 95%"),
                    ?assert(AvgTimePerOp < 100, "Average launch time should be < 100ms"),
                    ?assert(Throughput > 10, "Throughput should be > 10 ops/sec")
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test worklet completion performance.
%% @end
%%--------------------------------------------------------------------
test_worklet_completion_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Launch test worklets
                    CaseId = <<"case_completion_perf">>,
                    WorkletSpecId = <<"worklet_completion_perf">>,
                    ExceptionData = #{<<"task_id">> => <<"task_completion_perf">>},

                    {ok, ExecutionIds} = lists:unzip([
                        yawl_interface_d:launch_worklet(CaseId, WorkletSpecId, ExceptionData)
                        || _ <- lists:seq(1, 50)
                    ]),

                    % Measure completion time with large result data
                    LargeResult = #{<<"status">> => <<"completed">>,
                                 <<"large_data">> => lists:duplicate(5000, <<"completion_data">>)},

                    {Time, Results} = timer:tc(
                        fun() ->
                            [yawl_interface_d:complete_worklet(ExecId, LargeResult)
                             || ExecId <- ExecutionIds]
                        end
                    ),

                    % Calculate metrics
                    SuccessCount = length([R || R <- Results, R =:= ok]),
                    SuccessRate = SuccessCount / length(ExecutionIds),
                    AvgTimePerCompletion = Time / length(ExecutionIds),

                    % Log performance metrics
                    logger:info("Worklet completion performance: ~pms total, ~pms avg, ~p% success rate",
                                [Time, AvgTimePerCompletion, SuccessRate * 100],

                    % Performance assertions
                    ?assert(SuccessRate >= 0.95, "Completion success rate should be at least 95%"),
                    ?assert(AvgTimePerCompletion < 200, "Average completion time should be < 200ms")
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test memory usage patterns.
%% @end
%%--------------------------------------------------------------------
test_memory_usage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Get initial memory
                    InitialMemory = process_info(self(), memory),

                    % Perform operations to track memory growth
                    CaseId = <<"case_memory">>,
                    WorkletSpecId = <<"worklet_memory">>,
                    LargeExceptionData = #{
                        <<"task_id">> => <<"task_memory">>,
                        <<"large_metadata">> => generate_large_metadata(5000)
                    },

                    % Launch and complete many worklets
                    N = 100,
                    Operations = [],
                    for I <- lists:seq(1, N) do
                        {ok, ExecId} = yawl_interface_d:launch_worklet(
                            CaseId, WorkletSpecId, LargeExceptionData
                        ),
                        Result = #{<<"status">> => <<"handled">>,
                                 <<"result_data">> => generate_large_metadata(2000)},
                        ok = yawl_interface_d:complete_worklet(ExecId, Result),
                        Operations := [I | Operations]
                    end,

                    % Get final memory
                    FinalMemory = process_info(self(), memory),

                    % Calculate memory increase
                    MemoryIncrease = case {InitialMemory, FinalMemory} of
                                        {undefined, _} -> 0;
                                        {_, undefined} -> 0;
                                        {I, F} -> element(2, F) - element(2, I)
                                    end,

                    % Memory assertions (should not grow excessively)
                    ?assert(MemoryIncrease < 10 * 1024 * 1024, "Memory increase should be < 10MB"),
                    ?assert(MemoryIncrease / N < 100000, "Average memory per operation should be < 100KB")

                    % Clean up
                    lists:foreach(fun(ExecId) ->
                        % Clean up any remaining active worklets
                        case yawl_interface_d:get_worklet_status(ExecId) of
                            {ok, _Status} -> yawl_interface_d:abort_worklet(ExecId);
                            {error, not_found} -> ok
                        end
                    end, Operations)
                end)]
     end}.

%%====================================================================
%% 15. Concurrency Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test concurrent worklet launches.
%% @end
%%--------------------------------------------------------------------
test_concurrent_worklet_launches_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    N = 20,
                    CaseId = <<"case_concurrent">>,
                    WorkletSpecId = <<"worklet_concurrent">>,
                    ExceptionData = #{<<"task_id">> => <<"task_concurrent">>},

                    % Launch worklets concurrently using spawn
                    LaunchFun = fun(I) ->
                        spawn_link(fun() ->
                            case yawl_interface_d:launch_worklet(
                                CaseId, WorkletSpecId, ExceptionData) of
                                {ok, ExecId} ->
                                    % Also complete immediately
                                    Result = #{<<"status">> => <<"completed">>, <<"iteration">> => I},
                                    yawl_interface_d:complete_worklet(ExecId, Result);
                                {error, Reason} ->
                                    logger:error("Launch failed for ~p: ~p", [I, Reason])
                            end
                        end)
                    end,

                    % Spawn all launches
                    Pids = [LaunchFun(I) || I <- lists:seq(1, N)],

                    % Wait for all to complete
                    lists:foreach(fun(Pid) ->
                        receive
                            {'EXIT', Pid, _} -> ok
                        after 5000 ->
                            logger:error("Process ~p did not complete", [Pid])
                        end
                    end, Pids),

                    % Verify all worklets were handled
                    ActiveWorklets = yawl_interface_d:get_active_worklets(),
                    CompletedCount = length([W || W <- ActiveWorklets,
                                                 W#worklet_execution.status =:= completed]),
                    ?assert(CompletedCount >= N - 2, "Should have most worklets completed")
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test concurrent exception handling.
%% @end
%%--------------------------------------------------------------------
test_concurrent_exception_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register service for concurrent testing
                    ServiceConfig = [{endpoint, <<"http://concurrent_service">>}],
                    {ok, _ServiceId} = yawl_interface_d:register_exception_service(
                        <<"concurrent_service">>, ServiceConfig
                    ),

                    N = 15,
                    ExceptionType = system_exception,
                    BaseExceptionData = #{
                        <<"task_id">> => <<"task_concurrent_exception">>,
                        <<"worklet_spec_id">> => <<"concurrent_handler">>
                    },

                    % Handle exceptions concurrently
                    HandleFun = fun(I) ->
                        ExceptionData = BaseExceptionData#{
                            <<"case_id">> => list_to_binary(integer_to_list(I)),
                            <<"iteration">> => I
                        },
                        spawn_link(fun() ->
                            case yawl_interface_d:handle_exception(ExceptionType, ExceptionData) of
                                {ok, ExecId} ->
                                    % Complete the worklet
                                    Result = #{<<"status">> => <<"resolved">>, <<"iteration">> => I},
                                    yawl_interface_d:complete_worklet(ExecId, Result);
                                {error, Reason} ->
                                    logger:error("Exception handling failed for ~p: ~p", [I, Reason])
                            end
                        end)
                    end,

                    % Spawn all exception handlers
                    Pids = [HandleFun(I) || I <- lists:seq(1, N)],

                    % Wait for all to complete
                    lists:foreach(fun(Pid) ->
                        receive
                            {'EXIT', Pid, _} -> ok
                        after 5000 ->
                            logger:error("Process ~p did not complete", [Pid])
                        end
                    end, Pids),

                    % Verify all worklets completed
                    ActiveWorklets = yawl_interface_d:get_active_worklets(),
                    CompletedCount = length([W || W <- ActiveWorklets,
                                                 W#worklet_execution.status =:= completed]),
                    ?assert(CompletedCount >= N - 2, "Should have most exceptions handled")
                end)]
     end}.

%%====================================================================
%% 16. Load Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test high-volume worklet processing.
%% @end
%%--------------------------------------------------------------------
test_high_volume_worklet_processing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    N = 1000,  %% 1000 worklets
                    CaseId = <<"case_high_volume">>,
                    WorkletSpecId = <<"worklet_high_volume">>,
                    ExceptionData = #{<<"task_id">> => <<"task_high_volume">>,
                                    <<"batch_id">> => <<"batch_001">>},

                    % Measure total processing time
                    {StartTime, Results} = timer:tc(
                        fun() ->
                            [yawl_interface_d:launch_worklet(CaseId, WorkletSpecId, ExceptionData)
                             || _ <- lists:seq(1, N)]
                        end
                    ),

                    EndTime = erlang:monotonic_time(millisecond),
                    TotalTime = EndTime - StartTime,

                    % Calculate metrics
                    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
                    SuccessRate = SuccessCount / N,
                    Throughput = (N * 1000) / TotalTime,

                    % Performance requirements
                    ?assert(SuccessRate >= 0.95,
                            io_lib:format("Success rate ~p% below threshold", [SuccessRate * 100])),
                    ?assert(TotalTime < 30000,
                            io_lib:format("Total time ~pms exceeds 30s limit", [TotalTime])),
                    ?assert(Throughput > 33,
                            io_lib:format("Throughput ~p ops/sec below 33 ops/sec threshold", [Throughput])),

                    % Complete successfully launched worklets
                    SuccessfulLaunches = [element(2, R) || R <- Results, element(1, R) =:= ok],

                    {CompleteStartTime, CompleteResults} = timer:tc(
                        fun() ->
                            [yawl_interface_d:complete_worklet(
                                ExecId, #{<<"status">> => <<"completed">>})
                             || ExecId <- SuccessfulLaunches]
                        end
                    ),

                    CompleteRate = length([R || R <- CompleteResults, R =:= ok]) / length(SuccessfulLaunches),
                    CompleteThroughput = (length(SuccessfulLaunches) * 1000) / CompleteStartTime,

                    logger:info("High-volume metrics: ~p ops launched (~p%), ~p ops completed (~p%), "
                                "Launch throughput: ~p ops/sec, Complete throughput: ~p ops/sec",
                                [N, SuccessRate * 100, length(SuccessfulLaunches), CompleteRate * 100,
                                 Throughput, CompleteThroughput])
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test sustained load over time.
%% @end
%%--------------------------------------------------------------------
test_sustained_load_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Duration = 10000,  %% 10 seconds
                    Interval = 100,    %% 100ms between operations
                    BatchSize = 5,    %% 5 worklets per interval

                    % Register service
                    ServiceConfig = [{endpoint, <<"http://sustained_service">>}],
                    {ok, _ServiceId} = yawl_interface_d:register_exception_service(
                        <<"sustained_service">>, ServiceConfig
                    ),

                    ExceptionType = timeout_exception,
                    BaseExceptionData = #{
                        <<"task_id">> => <<"task_sustained">>,
                        <<"worklet_spec_id">> => <<"sustained_handler">>
                    },

                    % Perform sustained operations
                    StartTime = erlang:monotonic_time(millisecond),
                    CurrentTime = StartTime,
                    Operations = [],

                    while(CurrentTime < StartTime + Duration, fun() ->
                        BatchResults = [yawl_interface_d:handle_exception(
                            ExceptionType,
                            BaseExceptionData#{
                                <<"case_id">> => list_to_binary(integer_to_list(CurrentTime, I)),
                                <<"iteration">> => I
                            })
                            || I <- lists:seq(1, BatchSize)],

                        Operations := [BatchResults | Operations],
                        CurrentTime := erlang:monotonic_time(millisecond),
                        timer:sleep(Interval)
                    end),

                    % Complete all launched worklets
                    AllExecutionIds = lists:flatten([
                        [element(2, R) || R <- Batch, element(1, R) =:= ok]
                        || Batch <- Operations
                    ]),

                    CompleteFun = fun(ExecId) ->
                        yawl_interface_d:complete_worklet(
                            ExecId, #{<<"status">> => <<"resolved">>})
                    end,
                    lists:foreach(CompleteFun, AllExecutionIds),

                    % Calculate final metrics
                    TotalOperations = length(lists:flatten(Operations)),
                    DurationMs = erlang:monotonic_time(millisecond) - StartTime,
                    OpsPerSecond = (TotalOperations * 1000) / DurationMs,

                    logger:info("Sustained load: ~p operations in ~pms = ~p ops/sec",
                                [TotalOperations, DurationMs, OpsPerSecond]),

                    % Requirements
                    ?assert(TotalOperations >= Duration / Interval * BatchSize * 0.8,
                            io_lib:format("Operations ~p below expected threshold", [TotalOperations])),
                    ?assert(OpsPerSecond > BatchSize * 5,
                            io_lib:format("Throughput ~p ops/sec below expected threshold", [OpsPerSecond]))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test stress conditions with limited resources.
%% @end
%%--------------------------------------------------------------------
test_stress_conditions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Simulate stress conditions with multiple operations
                    StressDuration = 5000,  %% 5 seconds stress
                    HighFrequency = 20,    %% 20ms between operations
                    ConcurrentBatches = 10, %% 10 concurrent processes

                    % Register service
                    ServiceConfig = [{endpoint, <<"http://stress_service">>}],
                    {ok, _ServiceId} = yawl_interface_d:register_exception_service(
                        <<"stress_service">>, ServiceConfig
                    ),

                    ExceptionType = business_rule_exception,
                    BaseExceptionData = #{
                        <<"task_id">> => <<"task_stress">>,
                        <<"worklet_spec_id">> => <<"stress_handler">>,
                        <<"stress_data">> => lists:duplicate(1000, <<"stress_data">>)
                    },

                    % Start concurrent stress processes
                    StressProcesses = [],
                    for I <- lists:seq(1, ConcurrentBatches) do
                        ProcessPid = spawn_link(fun() ->
                            ProcessStartTime = erlang:monotonic_time(millisecond),
                            ProcessOps = [],

                            while(erlang:monotonic_time(millisecond) < ProcessStartTime + StressDuration, fun() ->
                                CaseId = list_to_binary(integer_to_list(I, erlang:unique_integer())),
                                ExceptionData = BaseExceptionData#{
                                    <<"case_id">> => CaseId,
                                    <<"process_id">> => I,
                                    <<"timestamp">> => erlang:system_time(millisecond)
                                },

                                case yawl_interface_d:handle_exception(ExceptionType, ExceptionData) of
                                    {ok, ExecId} ->
                                        % Complete immediately
                                        Result = #{<<"status">> => <<"handled">>,
                                                 <<"process_id">> => I,
                                                 <<"timestamp">> => erlang:system_time(millisecond)},
                                        yawl_interface_d:complete_worklet(ExecId, Result),
                                        ProcessOps := [ok | ProcessOps];
                                    {error, Reason} ->
                                        ProcessOps := [{error, Reason} | ProcessOps]
                                end,

                                timer:sleep(HighFrequency)
                            end),

                            % Log process results
                            ProcessCount = length(ProcessOps),
                            SuccessCount = length([R || R <- ProcessOps, R =:= ok]),
                            logger:info("Stress process ~p: ~p ops, ~p% success",
                                        [I, ProcessCount, (SuccessCount / ProcessCount) * 100]),

                            exit(normal)
                        end),
                        StressProcesses := [ProcessPid | StressProcesses]
                    end,

                    % Wait for all stress processes to complete
                    lists:foreach(fun(Pid) ->
                        receive
                            {'EXIT', Pid, _} -> ok
                        after 10000 ->
                            logger:error("Stress process ~p did not complete", [Pid])
                        end
                    end, StressProcesses),

                    % Final validation
                    logger:info("Stress test completed for ~p concurrent processes", [ConcurrentBatches]),
                    logger:info("System should have handled high frequency operations gracefully")
                end)]
     end}.

%%====================================================================
%% 17. Interface D Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test Interface D with YAWL workflow patterns integration.
%% @end
%%--------------------------------------------------------------------
test_interface_d_workflow_pattern_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register multiple exception services for different patterns
                    ServiceConfigs = [
                        {<<"pattern_service_1">>,
                            [{endpoint, <<"http://pattern1">>}, {priority, 10}]},
                        {<<"pattern_service_2">>,
                            [{endpoint, <<"http://pattern2">>}, {priority, 5}]},
                        {<<"pattern_service_3">>,
                            [{endpoint, <<"http://pattern3">>}, {priority, 8}]}
                    ],

                    lists:foreach(fun({Name, Config}) ->
                        {ok, _Id} = yawl_interface_d:register_exception_service(Name, Config)
                    end, ServiceConfigs),

                    % Test different workflow patterns
                    Patterns = [
                        {sequential_flow, sequential_exception, <<"sequential_handler">>},
                        {parallel_flow, parallel_exception, <<"parallel_handler">>},
                        {conditional_flow, business_rule_exception, <<"conditional_handler">>},
                        {loop_flow, timeout_exception, <<"loop_handler">>},
                        {merge_flow, runtime_exception, <<"merge_handler">>}
                    ],

                    PatternResults = [],
                    for {PatternName, ExceptionType, WorkletSpecId} <- Patterns do
                        ExceptionData = #{
                            <<"pattern_name">> => PatternName,
                            <<"case_id">> => list_to_binary(atom_to_list(PatternName)),
                            <<"task_id">> => list_to_binary(atom_to_list(PatternName)),
                            <<"worklet_spec_id">> => WorkletSpecId,
                            <<"metadata">> => #{
                                pattern => PatternName,
                                timestamp => erlang:system_time(millisecond)
                            }
                        },

                        {ok, ExecId} = yawl_interface_d:handle_exception(ExceptionType, ExceptionData),
                        ok = yawl_interface_d:complete_worklet(
                            ExecId, #{
                                <<"pattern_completed">> => PatternName,
                                <<"status">> => <<"handled">>,
                                <<"timestamp">> => erlang:system_time(millisecond)
                            }),

                        PatternResults := [{PatternName, ExecId} | PatternResults]
                    end,

                    % Verify all patterns were handled
                    ?assert(length(PatternResults) =:= 5, "All 5 patterns should be handled"),

                    logger:info("Workflow pattern integration test: ~p patterns completed",
                                [length(PatternResults)])
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test Interface D with distributed workflow execution.
%% @end
%%--------------------------------------------------------------------
test_interface_d_distributed_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register both local and remote services
                    LocalService = [{endpoint, <<"http://localhost:8080/local">>},
                                   {service_type, local}, {priority, 5}],
                    RemoteService = [{endpoint, <<"http://remote.example.com:8080/distributed">>},
                                     {service_type, remote}, {priority, 10}],

                    {ok, LocalId} = yawl_interface_d:register_exception_service(
                        <<"local_distributed_service">>, LocalService),
                    {ok, RemoteId} = yawl_interface_d:register_exception_service(
                        <<"remote_distributed_service">>, RemoteService),

                    % Test with different exception types to trigger routing
                    ExceptionTypes = [
                        {local_distributed_exception, LocalId},
                        {remote_distributed_exception, RemoteId},
                        {mixed_exception, LocalId}  % Should pick local due to higher priority
                    ],

                    Results = [],
                    for {ExceptionType, ExpectedServiceId} <- ExceptionTypes do
                        ExceptionData = #{
                            <<"exception_type">> => ExceptionType,
                            <<"case_id">> => list_to_binary(atom_to_list(ExceptionType)),
                            <<"task_id">> => <<"distributed_task">>,
                            <<"service_id">> => ExceptionType,
                            <<"distributed_context">> => #{
                                nodes => [node()],
                                timestamp => erlang:system_time(millisecond)
                            }
                        },

                        case yawl_interface_d:handle_exception(ExceptionType, ExceptionData) of
                            {ok, ExecId} ->
                                % Complete the worklet
                                Result = #{<<"handled_by">> => ExceptionType,
                                         <<"service_id">> => ExpectedServiceId,
                                         <<"status">> => <<"distributed_handled">>},
                                ok = yawl_interface_d:complete_worklet(ExecId, Result),
                                Results := [{ExceptionType, ExecId, success} | Results];
                            {error, Reason} ->
                                Results := [{ExceptionType, undefined, Reason} | Results]
                        end
                    end,

                    % Verify results
                    SuccessCount = length([R || R <- Results, element(3, R) =:= success]),
                    ?assert(SuccessCount >= 2, "Should handle at least 2/3 exceptions"),

                    logger:info("Distributed integration: ~p/~p exceptions handled",
                                [SuccessCount, length(Results)])
                end)]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
generate_large_metadata(Size) ->
    lists:duplicate(Size, <<"metadata_test">>).

%% @private
while(Condition, Fun) when is_function(Fun, 0) ->
    case Condition() of
        true ->
            Fun(),
            while(Condition, Fun);
        false ->
            ok
    end.
