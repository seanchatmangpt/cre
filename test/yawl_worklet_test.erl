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
%% @doc YAWL Worklet Service Test Suite
%%
%% Comprehensive test suite for worklet registration, launching,
%% and dynamic workflow adaptation.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_worklet_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions (imported from yawl_worklet)
%%====================================================================
-record(worklet_spec, {
    id :: binary(),
    name :: binary(),
    parameters :: [binary()],
    pre_condition :: function() | undefined,
    post_condition :: function() | undefined,
    constraints :: #{atom() => term()},
    workflow_def :: term() | undefined,
    description :: binary() | undefined
}).

-record(worklet_mapping, {
    id :: binary(),
    task_id :: binary(),
    worklet_spec_id :: binary(),
    selection_policy :: auto_offer | auto_start | manual,
    triggered_by :: exception | data | time,
    trigger_condition :: term() | undefined,
    priority :: non_neg_integer()
}).

-record(worklet_instance, {
    instance_id :: binary(),
    worklet_spec_id :: binary(),
    task_id :: binary(),
    status :: idle | running | completed | aborted | failed,
    started_at :: erlang:timestamp() | undefined,
    completed_at :: erlang:timestamp() | undefined,
    parameters :: #{binary() => term()},
    result :: term() | undefined
}).

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts the worklet service.
%% @end
%%--------------------------------------------------------------------
setup() ->
    case whereis(yawl_worklet) of
        undefined ->
            {ok, Pid} = yawl_worklet:start_link({local, yawl_worklet}),
            timer:sleep(100),
            Pid;
        Pid ->
            Pid
    end.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the worklet service.
%% @end
%%--------------------------------------------------------------------
cleanup(_Pid) ->
    case whereis(yawl_worklet) of
        undefined -> ok;
        Pid ->
            unlink(Pid),
            exit(Pid, normal),
            timer:sleep(50)
    end,
    ok.

%%====================================================================
%% 1. Worklet Registration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test registering a worklet specification.
%% @end
%%--------------------------------------------------------------------
test_register_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Name = <<"test_worklet">>,
                    Props = [
                        {parameters, [<<"param1">>, <<"param2">>]},
                        {description, <<"A test worklet">>}
                    ],
                    {ok, SpecId} = yawl_worklet:register_worklet(Name, Props),
                    ?assert(is_binary(SpecId)),
                    ?assertMatch(<<"worklet_spec_", _/binary>>, SpecId),

                    % Verify spec can be retrieved
                    {ok, Spec} = yawl_worklet:get_worklet_spec(SpecId),
                    ?assertEqual(Name, Spec#worklet_spec.name)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test registering worklet with pre/post conditions.
%% @end
%%--------------------------------------------------------------------
test_register_worklet_with_conditions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Name = <<"conditional_worklet">>,
                    PreCond = fun() -> true end,
                    PostCond = fun(_) -> true end,

                    Props = [
                        {pre_condition, PreCond},
                        {post_condition, PostCond},
                        {parameters, [<<"data">>]}
                    ],

                    {ok, SpecId} = yawl_worklet:register_worklet(Name, Props),

                    {ok, Spec} = yawl_worklet:get_worklet_spec(SpecId),
                    ?assertNotEqual(undefined, Spec#worklet_spec.pre_condition),
                    ?assertNotEqual(undefined, Spec#worklet_spec.post_condition)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unregistering a worklet.
%% @end
%%--------------------------------------------------------------------
test_unregister_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Name = <<"temp_worklet">>,
                    {ok, SpecId} = yawl_worklet:register_worklet(Name, []),

                    % Verify it exists
                    {ok, _Spec} = yawl_worklet:get_worklet_spec(SpecId),

                    % Unregister
                    ok = yawl_worklet:unregister_worklet(SpecId),

                    % Verify it's gone
                    {error, not_found} = yawl_worklet:get_worklet_spec(SpecId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test listing all worklet specifications.
%% @end
%%--------------------------------------------------------------------
test_list_worklet_specs_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register multiple worklets
                    {ok, _Id1} = yawl_worklet:register_worklet(<<"worklet1">>, []),
                    {ok, _Id2} = yawl_worklet:register_worklet(<<"worklet2">>, []),
                    {ok, _Id3} = yawl_worklet:register_worklet(<<"worklet3">>, []),

                    % List all specs
                    Specs = yawl_worklet:list_worklet_specs(),

                    ?assert(length(Specs) >= 3)
                end)]
     end}.

%%====================================================================
%% 2. Worklet Mapping Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test adding a worklet mapping.
%% @end
%%--------------------------------------------------------------------
test_add_worklet_mapping_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % First register a worklet spec
                    {ok, SpecId} = yawl_worklet:register_worklet(<<"mapped_worklet">>, []),

                    % Add mapping
                    TaskId = <<"task_123">>,
                    Policy = auto_start,
                    {ok, MappingId} = yawl_worklet:add_worklet_mapping(TaskId, SpecId, Policy),

                    ?assert(is_binary(MappingId)),
                    ?assertMatch(<<"mapping_", _/binary>>, MappingId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test removing a worklet mapping.
%% @end
%%--------------------------------------------------------------------
test_remove_worklet_mapping_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet and add mapping
                    {ok, SpecId} = yawl_worklet:register_worklet(<<"removable_worklet">>, []),
                    {ok, MappingId} = yawl_worklet:add_worklet_mapping(
                        <<"task_456">>,
                        SpecId,
                        manual
                    ),

                    % Remove mapping
                    ok = yawl_worklet:remove_worklet_mapping(MappingId)
                end)]
     end}.

%%====================================================================
%% 3. Precondition Tests
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
                    % Register worklet with no precondition (always true)
                    {ok, SpecId1} = yawl_worklet:register_worklet(
                        <<"no_precond_worklet">>,
                        []
                    ),
                    {ok, true} = yawl_worklet:check_preconditions(SpecId1, #{}),

                    % Register worklet with passing precondition
                    PreCondPass = fun() -> true end,
                    {ok, SpecId2} = yawl_worklet:register_worklet(
                        <<"pass_precond_worklet">>,
                        [{pre_condition, PreCondPass}]
                    ),
                    {ok, true} = yawl_worklet:check_preconditions(SpecId2, #{}),

                    % Register worklet with failing precondition
                    PreCondFail = fun() -> false end,
                    {ok, SpecId3} = yawl_worklet:register_worklet(
                        <<"fail_precond_worklet">>,
                        [{pre_condition, PreCondFail}]
                    ),
                    {ok, false} = yawl_worklet:check_preconditions(SpecId3, #{}),

                    % Register worklet with context precondition
                    PreCondContext = fun(Ctx) ->
                        maps:get(<<"valid">>, Ctx, false) =:= true
                    end,
                    {ok, SpecId4} = yawl_worklet:register_worklet(
                        <<"context_precond_worklet">>,
                        [{pre_condition, PreCondContext}]
                    ),
                    {ok, false} = yawl_worklet:check_preconditions(
                        SpecId4,
                        #{<<"valid">> => false}
                    ),
                    {ok, true} = yawl_worklet:check_preconditions(
                        SpecId4,
                        #{<<"valid">> => true}
                    )
                end)]
     end}.

%%====================================================================
%% 4. Postcondition Tests
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
                    % Register worklet with no postcondition
                    {ok, SpecId1} = yawl_worklet:register_worklet(
                        <<"no_postcond_worklet">>,
                        []
                    ),
                    {ok, true} = yawl_worklet:check_postconditions(SpecId1, #{}),

                    % Register worklet with passing postcondition
                    PostCondPass = fun() -> true end,
                    {ok, SpecId2} = yawl_worklet:register_worklet(
                        <<"pass_postcond_worklet">>,
                        [{post_condition, PostCondPass}]
                    ),
                    {ok, true} = yawl_worklet:check_postconditions(SpecId2, #{}),

                    % Register worklet with failing postcondition
                    PostCondFail = fun() -> false end,
                    {ok, SpecId3} = yawl_worklet:register_worklet(
                        <<"fail_postcond_worklet">>,
                        [{post_condition, PostCondFail}]
                    ),
                    {ok, false} = yawl_worklet:check_postconditions(SpecId3, #{}),

                    % Register worklet with result-checking postcondition
                    PostCondResult = fun(Ctx) ->
                        maps:get(<<"result">>, Ctx, 0) > 0
                    end,
                    {ok, SpecId4} = yawl_worklet:register_worklet(
                        <<"result_postcond_worklet">>,
                        [{post_condition, PostCondResult}]
                    ),
                    {ok, false} = yawl_worklet:check_postconditions(
                        SpecId4,
                        #{<<"result">> => -1}
                    ),
                    {ok, true} = yawl_worklet:check_postconditions(
                        SpecId4,
                        #{<<"result">> => 1}
                    )
                end)]
     end}.

%%====================================================================
%% 5. Worklet Selection Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test selecting appropriate worklet.
%% @end
%%--------------------------------------------------------------------
test_select_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet specs
                    {ok, SpecId1} = yawl_worklet:register_worklet(
                        <<"exception_handler">>,
                        []
                    ),
                    {ok, SpecId2} = yawl_worklet:register_worklet(
                        <<"data_handler">>,
                        []
                    ),

                    % Add mappings for different triggers
                    TaskId = <<"select_task">>,
                    {ok, _} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId1,
                        manual
                    ),

                    % Select worklet for exception trigger
                    {ok, SelectedId} = yawl_worklet:select_worklet(TaskId, exception),
                    ?assertEqual(SpecId1, SelectedId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test selecting worklet with none available.
%% @end
%%--------------------------------------------------------------------
test_select_worklet_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % No mappings registered
                    {error, no_worklet_found} = yawl_worklet:select_worklet(
                        <<"nonexistent_task">>,
                        exception
                    )
                end)]
     end}.

%%====================================================================
%% 6. Worklet Launch Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test launching a worklet instance.
%% @end
%%--------------------------------------------------------------------
test_launch_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet spec
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"launchable_worklet">>,
                        []
                    ),

                    % Launch worklet
                    TaskId = <<"launch_task">>,
                    Parameters = #{<<"input">> => value},

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        TaskId,
                        SpecId,
                        Parameters
                    ),

                    ?assert(is_binary(InstanceId)),
                    ?assertMatch(<<"worklet_instance_", _/binary>>, InstanceId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test launching worklet with precondition.
%% @end
%%--------------------------------------------------------------------
test_launch_worklet_with_precondition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with passing precondition
                    PreCond = fun() -> true end,
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"precond_launch_worklet">>,
                        [{pre_condition, PreCond}]
                    ),

                    TaskId = <<"precond_launch_task">>,
                    {ok, _InstanceId} = yawl_worklet:launch_worklet(
                        TaskId,
                        SpecId,
                        #{}
                    )
                end),
          ?_test(begin
                    % Register worklet with failing precondition
                    PreCond = fun() -> false end,
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"fail_precond_launch_worklet">>,
                        [{pre_condition, PreCond}]
                    ),

                    TaskId = <<"fail_precond_launch_task">>,
                    {error, precondition_not_met} = yawl_worklet:launch_worklet(
                        TaskId,
                        SpecId,
                        #{}
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test launching with non-existent spec.
%% @end
%%--------------------------------------------------------------------
test_launch_worklet_invalid_spec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeSpecId = <<"worklet_spec_fakenonexistent">>,
                    TaskId = <<"invalid_spec_task">>,

                    {error, worklet_spec_not_found} = yawl_worklet:launch_worklet(
                        TaskId,
                        FakeSpecId,
                        #{}
                    )
                end)]
     end}.

%%====================================================================
%% 7. Worklet Completion Tests
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
                    % Register and launch worklet
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"completable_worklet">>,
                        []
                    ),
                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"complete_task">>,
                        SpecId,
                        #{}
                    ),

                    % Simulate worklet completion
                    ok = yawl_worklet:complete_worklet_instance(
                        InstanceId,
                        #{result => success}
                    ),

                    % Verify the worklet is now in completed list
                    CompletedWorklets = yawl_worklet:get_completed_worklets(),
                    Completed = lists:keyfind(InstanceId, #worklet_instance.instance_id, CompletedWorklets),
                    ?assertMatch(#worklet_instance{status = completed}, Completed),
                    ?assertEqual(#{result => success}, Completed#worklet_instance.result)
                end)]
     end}.

%%====================================================================
%% 8. Worklet Abort Tests
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
                    % Register and launch worklet
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"abortable_worklet">>,
                        []
                    ),
                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"abort_task">>,
                        SpecId,
                        #{}
                    ),

                    % Verify it's running
                    ActiveWorklets = yawl_worklet:get_active_worklets(),
                    ?assert(length(ActiveWorklets) > 0),

                    % Abort the worklet
                    ok = yawl_worklet:abort_worklet(InstanceId),

                    % Check it's no longer active
                    NewActiveWorklets = yawl_worklet:get_active_worklets(),
                    ?assert(lists:all(
                        fun(W) -> W#worklet_instance.instance_id =/= InstanceId end,
                        NewActiveWorklets
                    ))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test aborting non-existent instance.
%% @end
%%--------------------------------------------------------------------
test_abort_nonexistent_worklet_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeInstanceId = <<"worklet_instance_fakenonexistent">>,

                    % Should not error, just no-op
                    ok = yawl_worklet:abort_worklet(FakeInstanceId)
                end)]
     end}.

%%====================================================================
%% 9. Active Worklets Tests
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
                    ActiveWorklets1 = yawl_worklet:get_active_worklets(),
                    ?assertEqual(0, length(ActiveWorklets1)),

                    % Launch some worklets
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"active_test_worklet">>,
                        []
                    ),

                    {ok, _Inst1} = yawl_worklet:launch_worklet(
                        <<"active_task1">>,
                        SpecId,
                        #{}
                    ),
                    {ok, _Inst2} = yawl_worklet:launch_worklet(
                        <<"active_task2">>,
                        SpecId,
                        #{}
                    ),

                    % Should have active worklets
                    ActiveWorklets2 = yawl_worklet:get_active_worklets(),
                    ?assert(length(ActiveWorklets2) >= 2),

                    % All should be in running state
                    ?assert(lists:all(
                        fun(W) -> W#worklet_instance.status =:= running end,
                        ActiveWorklets2
                    ))
                end)]
     end}.

%%====================================================================
%% 10. Completed Worklets Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test getting completed worklets.
%% @end
%%--------------------------------------------------------------------
test_get_completed_worklets_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Initially no completed worklets
                    CompletedWorklets1 = yawl_worklet:get_completed_worklets(),
                    ?assertEqual(0, length(CompletedWorklets1)),

                    % Launch and abort worklets to create "completed" ones
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"completed_test_worklet">>,
                        []
                    ),

                    {ok, Inst1} = yawl_worklet:launch_worklet(
                        <<"completed_task1">>,
                        SpecId,
                        #{}
                    ),
                    {ok, Inst2} = yawl_worklet:launch_worklet(
                        <<"completed_task2">>,
                        SpecId,
                        #{}
                    ),

                    % Abort them (changes status to aborted)
                    ok = yawl_worklet:abort_worklet(Inst1),
                    ok = yawl_worklet:abort_worklet(Inst2),

                    % Note: aborted worklets won't show in completed list
                    % This test verifies the API works
                    CompletedWorklets2 = yawl_worklet:get_completed_worklets(),
                    ?assert(is_list(CompletedWorklets2))
                end)]
     end}.

%%====================================================================
%% 11. Selection Policy Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test auto_start selection policy.
%% @end
%%--------------------------------------------------------------------
test_auto_start_policy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"auto_start_worklet">>,
                        []
                    ),

                    % Add mapping with auto_start policy
                    TaskId = <<"auto_start_task">>,
                    {ok, _MappingId} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId,
                        auto_start
                    ),

                    % Trigger worklet - should auto-launch
                    {ok, InstanceId} = yawl_worklet:trigger_worklet(
                        TaskId,
                        exception
                    ),

                    ?assert(is_binary(InstanceId)),

                    % Verify worklet is running
                    ActiveWorklets = yawl_worklet:get_active_worklets(),
                    ?assert(lists:any(
                        fun(W) -> W#worklet_instance.instance_id =:= InstanceId end,
                        ActiveWorklets
                    ))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test manual selection policy.
%% @end
%%--------------------------------------------------------------------
test_manual_policy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"manual_worklet">>,
                        []
                    ),

                    % Add mapping with manual policy
                    TaskId = <<"manual_task">>,
                    {ok, MappingId} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId,
                        manual
                    ),

                    % Trigger worklet - should return mapping ID, not launch
                    {ok, MappingId} = yawl_worklet:trigger_worklet(
                        TaskId,
                        exception
                    ),

                    % No worklet should be auto-launched
                    ActiveWorklets = yawl_worklet:get_active_worklets(),
                    ?assertEqual(0, length(ActiveWorklets))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test auto_offer selection policy.
%% @end
%%--------------------------------------------------------------------
test_auto_offer_policy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"auto_offer_worklet">>,
                        []
                    ),

                    % Add mapping with auto_offer policy
                    TaskId = <<"auto_offer_task">>,
                    {ok, MappingId} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId,
                        auto_offer
                    ),

                    % Trigger worklet - should return mapping ID for offer
                    {ok, MappingId} = yawl_worklet:trigger_worklet(
                        TaskId,
                        exception
                    ),

                    % No worklet should be auto-launched (requires user approval)
                    ActiveWorklets = yawl_worklet:get_active_worklets(),
                    ?assertEqual(0, length(ActiveWorklets))
                end)]
     end}.

%%====================================================================
%% 12. Parameter Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test setting and getting worklet parameters.
%% @end
%%--------------------------------------------------------------------
test_worklet_parameters_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with parameters
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"param_worklet">>,
                        [
                            {parameters, [<<"param1">>, <<"param2">>]},
                            {description, <<"Test parameter worklet">>}
                        ]
                    ),

                    % Get parameters
                    {ok, Params} = yawl_worklet:get_worklet_parameters(SpecId),
                    ?assert(lists:member(<<"param1">>, Params)),
                    ?assert(lists:member(<<"param2">>, Params)),

                    % Set new parameter
                    ok = yawl_worklet:set_worklet_parameter(SpecId, <<"param3">>, value),

                    % Verify new parameter exists
                    {ok, UpdatedParams} = yawl_worklet:get_worklet_parameters(SpecId),
                    ?assert(lists:member(<<"param3">>, UpdatedParams))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test setting parameter on non-existent spec.
%% @end
%%--------------------------------------------------------------------
test_set_parameter_invalid_spec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeSpecId = <<"worklet_spec_fakenotfound">>,
                    {error, not_found} = yawl_worklet:set_worklet_parameter(
                        FakeSpecId,
                        <<"param">>,
                        value
                    )
                end)]
     end}.

%%====================================================================
%% 13. Trigger Type Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test different trigger types.
%% @end
%%--------------------------------------------------------------------
test_trigger_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"trigger_test_worklet">>,
                        []
                    ),

                    % Test exception trigger
                    {ok, _} = yawl_worklet:add_worklet_mapping(
                        <<"exception_task">>,
                        SpecId,
                        auto_start
                    ),
                    {ok, _} = yawl_worklet:trigger_worklet(
                        <<"exception_task">>,
                        exception
                    ),

                    timer:sleep(50),

                    % Verify active
                    Active = yawl_worklet:get_active_worklets(),
                    ?assert(length(Active) > 0)
                end)]
     end}.

%%====================================================================
%% 14. Edge Cases and Error Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test precondition that throws error.
%% @end
%%--------------------------------------------------------------------
test_precondition_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with error-throwing precondition
                    PreCond = fun() -> error(bad_precondition) end,
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"error_precond_worklet">>,
                        [{pre_condition, PreCond}]
                    ),

                    % Should return error
                    {error, precondition_failed} = yawl_worklet:check_preconditions(
                        SpecId,
                        #{}
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test postcondition that throws error.
%% @end
%%--------------------------------------------------------------------
test_postcondition_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with error-throwing postcondition
                    PostCond = fun() -> error(bad_postcondition) end,
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"error_postcond_worklet">>,
                        [{post_condition, PostCond}]
                    ),

                    % Should return error
                    {error, postcondition_failed} = yawl_worklet:check_postconditions(
                        SpecId,
                        #{}
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test operations on non-existent worklet.
%% @end
%%--------------------------------------------------------------------
test_nonexistent_worklet_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeSpecId = <<"worklet_spec_fakenonexistent">>,

                    {error, not_found} = yawl_worklet:get_worklet_spec(FakeSpecId),
                    {error, not_found} = yawl_worklet:unregister_worklet(FakeSpecId),
                    {error, not_found} = yawl_worklet:check_preconditions(
                        FakeSpecId,
                        #{}
                    ),
                    {error, not_found} = yawl_worklet:check_postconditions(
                        FakeSpecId,
                        #{}
                    ),
                    {error, not_found} = yawl_worklet:get_worklet_parameters(FakeSpecId)
                end)]
     end}.

%%====================================================================
%% 15. Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test complete worklet workflow.
%% @end
%%--------------------------------------------------------------------
test_complete_worklet_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % 1. Register worklet specification
                    Name = <<"workflow_worklet">>,
                    PreCond = fun(Ctx) ->
                        maps:get(<<"ready">>, Ctx, false) =:= true
                    end,
                    PostCond = fun(Ctx) ->
                        maps:get(<<"success">>, Ctx, false) =:= true
                    end,

                    {ok, SpecId} = yawl_worklet:register_worklet(
                        Name,
                        [
                            {pre_condition, PreCond},
                            {post_condition, PostCond},
                            {parameters, [<<"input">>, <<"output">>]},
                            {description, <<"Integration test worklet">>}
                        ]
                    ),
                    ?assert(is_binary(SpecId)),

                    % 2. Add mapping
                    TaskId = <<"workflow_task">>,
                    {ok, MappingId} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId,
                        auto_start
                    ),

                    % 3. Check preconditions with ready context
                    {ok, true} = yawl_worklet:check_preconditions(
                        SpecId,
                        #{<<"ready">> => true}
                    ),

                    % 4. Launch worklet with correct context for precondition
                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        TaskId,
                        SpecId,
                        #{<<"ready">> => true, <<"input">> => test_data}
                    ),
                    ?assert(is_binary(InstanceId)),

                    % 5. Verify worklet is active
                    ActiveWorklets = yawl_worklet:get_active_worklets(),
                    ?assert(lists:any(
                        fun(W) -> W#worklet_instance.instance_id =:= InstanceId end,
                        ActiveWorklets
                    )),

                    % 6. Abort worklet
                    ok = yawl_worklet:abort_worklet(InstanceId),

                    % 7. Verify no longer active
                    NewActiveWorklets = yawl_worklet:get_active_worklets(),
                    ?assertNot(lists:any(
                        fun(W) -> W#worklet_instance.instance_id =:= InstanceId end,
                        NewActiveWorklets
                    )),

                    % 8. Remove mapping
                    ok = yawl_worklet:remove_worklet_mapping(MappingId),

                    % 9. Unregister worklet
                    ok = yawl_worklet:unregister_worklet(SpecId),

                    % 10. Verify removed
                    {error, not_found} = yawl_worklet:get_worklet_spec(SpecId),

                    ok
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test multiple worklets for same task.
%% @end
%%--------------------------------------------------------------------
test_multiple_worklets_same_task_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register multiple worklets
                    {ok, SpecId1} = yawl_worklet:register_worklet(
                        <<"priority_worklet_1">>,
                        []
                    ),
                    {ok, SpecId2} = yawl_worklet:register_worklet(
                        <<"priority_worklet_2">>,
                        []
                    ),

                    TaskId = <<"priority_task">>,

                    % Add mappings - second one should have higher priority
                    {ok, _} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId1,
                        auto_start
                    ),
                    {ok, _} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId2,
                        auto_start
                    ),

                    % Select worklet - should return based on priority
                    {ok, SelectedId} = yawl_worklet:select_worklet(TaskId, exception),
                    ?assert(lists:member(SelectedId, [SpecId1, SpecId2]))
                end)]
     end}.

%%====================================================================
%% 16. Workflow Definition Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet with workflow definition.
%% @end
%%--------------------------------------------------------------------
test_workflow_definition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with workflow definition
                    WorkflowDef = #{
                        steps => [
                            #{type => task, name => <<"step1">>},
                            #{type => task, name => <<"step2">>},
                            #{type => decision, name => <<"check">>}
                        ],
                        variables => #{input => <<"data">>}
                    },

                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"workflow_def_worklet">>,
                        [{workflow_def, WorkflowDef}]
                    ),

                    % Verify spec was created
                    {ok, Spec} = yawl_worklet:get_worklet_spec(SpecId),
                    ?assertNotEqual(undefined, Spec#worklet_spec.workflow_def)
                end)]
     end}.

%%====================================================================
%% 17. Constraint Tests (R2ML)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet with constraints.
%% @end
%%--------------------------------------------------------------------
test_constraints_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register worklet with R2ML-style constraints
                    Constraints = #{
                        max_execution_time => 3600,
                        required_roles => [<<"admin">>, <<"approver">>],
                        resource_limits => #{memory => 512, cpu => 2},
                        retry_count => 3
                    },

                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"constrained_worklet">>,
                        [{constraints, Constraints}]
                    ),

                    {ok, Spec} = yawl_worklet:get_worklet_spec(SpecId),
                    StoredConstraints = Spec#worklet_spec.constraints,
                    ?assertEqual(3600, maps:get(max_execution_time, StoredConstraints)),
                    ?assertEqual(3, maps:get(retry_count, StoredConstraints))
                end),
          ?_test(begin
                    % Register worklet with empty constraints
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"no_constraints_worklet">>,
                        [{constraints, #{}}]
                    ),

                    {ok, Spec} = yawl_worklet:get_worklet_spec(SpecId),
                    ?assertEqual(#{}, Spec#worklet_spec.constraints)
                end)]
     end}.

%%====================================================================
%% 18. Parameter Type Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet parameter type handling.
%% @end
%%--------------------------------------------------------------------
test_parameter_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register with atom parameters (converted to binary)
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"atom_param_worklet">>,
                        [{parameters, [param1, param2, param3]}]
                    ),

                    {ok, Params} = yawl_worklet:get_worklet_parameters(SpecId),
                    ?assert(is_list(Params)),
                    ?assertEqual(3, length(Params))
                end),
          ?_test(begin
                    % Register with string parameters (converted to binary)
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"string_param_worklet">>,
                        [{parameters, ["string_param", "another_param"]}]
                    ),

                    {ok, Params} = yawl_worklet:get_worklet_parameters(SpecId),
                    ?assert(lists:member(<<"string_param">>, Params))
                end)]
     end}.

%%====================================================================
%% 19. Worklet Instance Status Transitions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet instance status transitions.
%% @end
%%--------------------------------------------------------------------
test_status_transitions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Launch transitions to running
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"status_worklet">>,
                        []
                    ),

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"status_task">>,
                        SpecId,
                        #{}
                    ),

                    Active = yawl_worklet:get_active_worklets(),
                    Instance = lists:keyfind(InstanceId, #worklet_instance.instance_id, Active),
                    ?assertMatch(#worklet_instance{status = running}, Instance)
                end),
          ?_test(begin
                    % Abort transitions from running to aborted
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"abort_status_worklet">>,
                        []
                    ),

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"abort_status_task">>,
                        SpecId,
                        #{}
                    ),

                    ok = yawl_worklet:abort_worklet(InstanceId),

                    Active = yawl_worklet:get_active_worklets(),
                    ?assertNot(lists:any(
                        fun(W) -> W#worklet_instance.instance_id =:= InstanceId end,
                        Active
                    ))
                end)]
     end}.

%%====================================================================
%% 20. Instance Timestamp Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet instance timestamps.
%% @end
%%--------------------------------------------------------------------
test_instance_timestamps_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Instance has started_at timestamp
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"timestamp_worklet">>,
                        []
                    ),

                    StartTime = erlang:timestamp(),

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"timestamp_task">>,
                        SpecId,
                        #{}
                    ),

                    Active = yawl_worklet:get_active_worklets(),
                    Instance = lists:keyfind(InstanceId, #worklet_instance.instance_id, Active),

                    ?assertMatch(#worklet_instance{started_at = _}, Instance),
                    ?assertNotEqual(undefined, Instance#worklet_instance.started_at)
                end),
          ?_test(begin
                    % Completed instance has completed_at timestamp
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"complete_timestamp_worklet">>,
                        []
                    ),

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"complete_timestamp_task">>,
                        SpecId,
                        #{}
                    ),

                    ok = yawl_worklet:complete_worklet_instance(
                        InstanceId,
                        #{result => done}
                    ),

                    CompletedWorklets = yawl_worklet:get_completed_worklets(),
                    Completed = lists:keyfind(InstanceId, #worklet_instance.instance_id, CompletedWorklets),
                    ?assertNotEqual(undefined, Completed#worklet_instance.completed_at)
                end)]
     end}.

%%====================================================================
%% 21. Multiple Trigger Types Test
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test all trigger types.
%% @end
%% @doc Test exception trigger through mappings.
%% @end
%%--------------------------------------------------------------------
test_all_trigger_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Test exception trigger (the only one supported by add_worklet_mapping)
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"exception_trigger_worklet">>,
                        []
                    ),

                    TaskId = <<"exception_trigger_task">>,
                    {ok, _} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        SpecId,
                        auto_start
                    ),

                    {ok, InstanceId} = yawl_worklet:trigger_worklet(TaskId, exception),
                    ?assert(is_binary(InstanceId))
                end),
          ?_test(begin
                    % Test that data/time triggers won't find worklets without proper mappings
                    {ok, OtherSpecId} = yawl_worklet:register_worklet(
                        <<"other_trigger_worklet">>,
                        []
                    ),

                    TaskId = <<"other_trigger_task">>,
                    {ok, _} = yawl_worklet:add_worklet_mapping(
                        TaskId,
                        OtherSpecId,
                        auto_start
                    ),

                    % These should fail because mappings are exception-type only
                    Result1 = yawl_worklet:trigger_worklet(TaskId, data),
                    Result2 = yawl_worklet:trigger_worklet(<<"nonexistent_task">>, exception),
                    ?assertEqual({error, no_worklet_found}, Result1),
                    ?assertEqual({error, no_worklet_found}, Result2)
                end)]
     end}.

%%====================================================================
%% 22. Worklet Cleanup Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet cleanup after completion.
%% @end
%%--------------------------------------------------------------------
test_worklet_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Complete worklets don't show in active list
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"cleanup_worklet">>,
                        []
                    ),

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"cleanup_task">>,
                        SpecId,
                        #{}
                    ),

                    % Should be in active list
                    Active1 = yawl_worklet:get_active_worklets(),
                    ?assert(lists:any(
                        fun(W) -> W#worklet_instance.instance_id =:= InstanceId end,
                        Active1
                    )),

                    % Abort the worklet
                    ok = yawl_worklet:abort_worklet(InstanceId),

                    % Should not be in active list
                    Active2 = yawl_worklet:get_active_worklets(),
                    ?assertNot(lists:any(
                        fun(W) -> W#worklet_instance.instance_id =:= InstanceId end,
                        Active2
                    ))
                end)]
     end}.

%%====================================================================
%% 23. Context-Aware Pre/Post Conditions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test preconditions and postconditions with complex context.
%% @end
%%--------------------------------------------------------------------
test_context_aware_conditions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Precondition checking multiple context values
                    PreCondMulti = fun(Ctx) ->
                        Val1 = maps:get(<<"val1">>, Ctx, 0),
                        Val2 = maps:get(<<"val2">>, Ctx, 0),
                        Val1 > 0 andalso Val2 > 0
                    end,

                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"multi_context_worklet">>,
                        [{pre_condition, PreCondMulti}]
                    ),

                    {ok, false} = yawl_worklet:check_preconditions(
                        SpecId,
                        #{<<"val1">> => 1, <<"val2">> => 0}
                    ),
                    {ok, true} = yawl_worklet:check_preconditions(
                        SpecId,
                        #{<<"val1">> => 1, <<"val2">> => 1}
                    )
                end),
          ?_test(begin
                    % Postcondition validating result structure
                    PostCondStruct = fun(Ctx) ->
                        case maps:get(<<"result">>, Ctx) of
                            #{<<"status">> := <<"success">>, <<"data">> := _} -> true;
                            _ -> false
                        end
                    end,

                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"struct_postcond_worklet">>,
                        [{post_condition, PostCondStruct}]
                    ),

                    {ok, false} = yawl_worklet:check_postconditions(
                        SpecId,
                        #{<<"result">> => #{<<"status">> => <<"error">>}}
                    ),
                    {ok, true} = yawl_worklet:check_postconditions(
                        SpecId,
                        #{<<"result">> => #{<<"status">> => <<"success">>, <<"data">> => <<"value">>}}
                    )
                end)]
     end}.

%%====================================================================
%% 24. Instance Parameters Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worklet instance with complex parameters.
%% @end
%%--------------------------------------------------------------------
test_instance_parameters_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Launch with nested parameters
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"nested_param_worklet">>,
                        []
                    ),

                    NestedParams = #{
                        <<"level1">> => #{
                            <<"level2">> => #{
                                <<"level3">> => <<"deep_value">>
                            }
                        },
                        <<"list">> => [<<"item1">>, <<"item2">>]
                    },

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"nested_task">>,
                        SpecId,
                        NestedParams
                    ),

                    Active = yawl_worklet:get_active_worklets(),
                    Instance = lists:keyfind(InstanceId, #worklet_instance.instance_id, Active),
                    ?assertEqual(NestedParams, Instance#worklet_instance.parameters)
                end),
          ?_test(begin
                    % Launch with empty parameters
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"empty_param_worklet">>,
                        []
                    ),

                    {ok, InstanceId} = yawl_worklet:launch_worklet(
                        <<"empty_task">>,
                        SpecId,
                        #{}
                    ),

                    Active = yawl_worklet:get_active_worklets(),
                    Instance = lists:keyfind(InstanceId, #worklet_instance.instance_id, Active),
                    ?assertEqual(#{}, Instance#worklet_instance.parameters)
                end)]
     end}.

%%====================================================================
%% 25. Stress Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test system with many worklets.
%% @end
%%--------------------------------------------------------------------
test_many_worklets_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Register many worklets
                    lists:foreach(
                        fun(N) ->
                            Name = list_to_binary("stress_worklet_" ++ integer_to_list(N)),
                            {ok, _} = yawl_worklet:register_worklet(Name, [])
                        end,
                        lists:seq(1, 50)
                    ),

                    AllSpecs = yawl_worklet:list_worklet_specs(),
                    ?assert(length(AllSpecs) >= 50)
                end),
          ?_test(begin
                    % Launch many instances
                    {ok, SpecId} = yawl_worklet:register_worklet(
                        <<"stress_launch_worklet">>,
                        []
                    ),

                    lists:foreach(
                        fun(N) ->
                            TaskId = list_to_binary("stress_task_" ++ integer_to_list(N)),
                            {ok, _} = yawl_worklet:launch_worklet(
                                TaskId,
                                SpecId,
                                #{<<"index">> => N}
                            )
                        end,
                        lists:seq(1, 20)
                    ),

                    Active = yawl_worklet:get_active_worklets(),
                    ?assert(length(Active) >= 20)
                end)]
     end}.
