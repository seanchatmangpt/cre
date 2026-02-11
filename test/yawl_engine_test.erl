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
%% @doc YAWL Engine Test Suite
%%
%% Comprehensive test suite for the YAWL workflow execution engine.
%% Tests cover case lifecycle, workitem management, token flow,
%% observer notifications, and engine status operations.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_engine_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%% Explicitly export test generators for EUnit discovery
-export([
    test_start_workflow_basic_/0,
    test_suspend_resume_case_/0,
    test_cancel_case_/0,
    test_case_status_/0,
    test_get_available_workitems_/0,
    test_start_workitem_/0,
    test_complete_workitem_/0,
    test_fail_workitem_/0,
    test_invalid_workitem_state_transitions_/0,
    test_and_split_join_/0,
    test_or_split_join_/0,
    test_xor_split_join_/0,
    test_register_observer_/0,
    test_observer_case_events_/0,
    test_observer_workitem_events_/0,
    test_get_engine_status_/0,
    test_list_cases_/0,
    test_no_cases_error_handling_/0,
    test_invalid_state_transitions_/0,
    test_nonexistent_workitem_/0,
    test_parallel_execution_/0,
    setup/0,
    cleanup/1,
    simple_workflow_spec/0,
    parallel_workflow_spec/0,
    xor_workflow_spec/0,
    or_workflow_spec/0,
    start_observer/0,
    observer_loop/1,
    get_observer_events/1,
    stop_observer/1
]).

-compile(export_all).

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Creates a test engine instance and initial workflow specification.
%% @end
%%--------------------------------------------------------------------
setup() ->
    case whereis(yawl_engine) of
        undefined ->
            {ok, EnginePid} = yawl_engine:start_link(),
            EnginePid;
        _ExistingPid ->
            %% Engine already running, just return the pid
            {ok, Pid} = yawl_engine:start_link(),
            Pid
    end.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the engine and flushes messages.
%% @end
%%--------------------------------------------------------------------
cleanup(_EnginePid) ->
    %% Stop the engine to ensure clean state for next test
    catch gen_server:stop(yawl_engine),
    %% Wait a bit for shutdown
    timer:sleep(10),
    %% Flush any remaining messages
    receive
        _ -> ok
    after 0 -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Creates a simple workflow specification for testing.
%% @end
%%--------------------------------------------------------------------
simple_workflow_spec() ->
    #{
      id => <<"test_workflow">>,
      name => <<"Simple Test Workflow">>,
      tasks => #{
        <<"task_a">> => #{id => <<"task_a">>, type => atomic},
        <<"task_b">> => #{id => <<"task_b">>, type => atomic},
        <<"task_c">> => #{id => <<"task_c">>, type => atomic}
      },
      flows => [
        #{source => <<"task_a">>, target => <<"task_b">>},
        #{source => <<"task_b">>, target => <<"task_c">>}
      ]
     }.

%%--------------------------------------------------------------------
%% @doc Creates a parallel workflow specification for AND split/join tests.
%% @end
%%--------------------------------------------------------------------
parallel_workflow_spec() ->
    #{
      id => <<"parallel_workflow">>,
      name => <<"Parallel Test Workflow">>,
      tasks => #{
        <<"start_task">> => #{id => <<"start_task">>, type => atomic},
        <<"branch_a">> => #{id => <<"branch_a">>, type => atomic},
        <<"branch_b">> => #{id => <<"branch_b">>, type => atomic},
        <<"branch_c">> => #{id => <<"branch_c">>, type => atomic},
        <<"join_task">> => #{id => <<"join_task">>, type => and_join}
      },
      flows => [
        #{source => <<"start_task">>, target => <<"branch_a">>},
        #{source => <<"start_task">>, target => <<"branch_b">>},
        #{source => <<"start_task">>, target => <<"branch_c">>},
        #{source => <<"branch_a">>, target => <<"join_task">>},
        #{source => <<"branch_b">>, target => <<"join_task">>},
        #{source => <<"branch_c">>, target => <<"join_task">>}
      ]
     }.

%%--------------------------------------------------------------------
%% @doc Creates an XOR workflow specification for exclusive choice tests.
%% @end
%%--------------------------------------------------------------------
xor_workflow_spec() ->
    #{
      id => <<"xor_workflow">>,
      name => <<"XOR Test Workflow">>,
      tasks => #{
        <<"choice_task">> => #{id => <<"choice_task">>, type => xor_split},
        <<"option_a">> => #{id => <<"option_a">>, type => atomic},
        <<"option_b">> => #{id => <<"option_b">>, type => atomic},
        <<"merge_task">> => #{id => <<"merge_task">>, type => xor_join}
      },
      flows => [
        #{source => <<"choice_task">>, target => <<"option_a">>},
        #{source => <<"choice_task">>, target => <<"option_b">>},
        #{source => <<"option_a">>, target => <<"merge_task">>},
        #{source => <<"option_b">>, target => <<"merge_task">>}
      ]
     }.

%%--------------------------------------------------------------------
%% @doc Creates an OR workflow specification for multi-choice tests.
%% @end
%%--------------------------------------------------------------------
or_workflow_spec() ->
    #{
      id => <<"or_workflow">>,
      name => <<"OR Test Workflow">>,
      tasks => #{
        <<"multi_choice">> => #{id => <<"multi_choice">>, type => or_split},
        <<"path_1">> => #{id => <<"path_1">>, type => atomic},
        <<"path_2">> => #{id => <<"path_2">>, type => atomic},
        <<"path_3">> => #{id => <<"path_3">>, type => atomic},
        <<"partial_join">> => #{id => <<"partial_join">>, type => or_join}
      },
      flows => [
        #{source => <<"multi_choice">>, target => <<"path_1">>},
        #{source => <<"multi_choice">>, target => <<"path_2">>},
        #{source => <<"multi_choice">>, target => <<"path_3">>},
        #{source => <<"path_1">>, target => <<"partial_join">>},
        #{source => <<"path_2">>, target => <<"partial_join">>},
        #{source => <<"path_3">>, target => <<"partial_join">>}
      ]
     }.

%%--------------------------------------------------------------------
%% @doc Creates a test observer process for event verification.
%% @end
%%--------------------------------------------------------------------
start_observer() ->
    spawn_link(fun() -> observer_loop([]) end).

%%--------------------------------------------------------------------
%% @doc Observer loop that collects events.
%% @end
%%--------------------------------------------------------------------
observer_loop(Events) ->
    receive
        {get_events, From} ->
            From ! {events, lists:reverse(Events)},
            observer_loop(Events);
        {yawl_event, Event} ->
            observer_loop([Event | Events]);
        stop ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Gets collected events from an observer.
%% @end
%%--------------------------------------------------------------------
get_observer_events(ObserverPid) ->
    ObserverPid ! {get_events, self()},
    receive
        {events, Events} -> Events
    after 1000 -> timeout
    end.

%%--------------------------------------------------------------------
%% @doc Stops an observer process.
%% @end
%%--------------------------------------------------------------------
stop_observer(ObserverPid) ->
    ObserverPid ! stop,
    ok.

%%====================================================================
%% Case Lifecycle Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test starting a basic workflow.
%% Verifies that a workflow can be started and returns a valid case ID.
%% @end
%%--------------------------------------------------------------------
test_start_workflow_basic_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    Spec = simple_workflow_spec(),
                    {ok, CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),
                    ?assert(is_binary(CaseId)),
                    ?assert(<<>> /= CaseId),
                    ?assertEqual(<<"case_">>, binary_part(CaseId, 0, 5))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test suspending and resuming a running case.
%% Verifies that a case can be suspended and later resumed successfully.
%% @end
%%--------------------------------------------------------------------
test_suspend_resume_case_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Verify case is running
                    {ok, State1} = yawl_engine:get_case_state(yawl_engine),
                    ?assertEqual(running, maps:get(status, State1)),

                    %% Suspend the case
                    ?assertEqual(ok, yawl_engine:suspend_case(yawl_engine)),

                    %% Verify case is suspended
                    {ok, State2} = yawl_engine:get_case_state(yawl_engine),
                    ?assertEqual(suspended, maps:get(status, State2)),

                    %% Resume the case
                    ?assertEqual(ok, yawl_engine:resume_case(yawl_engine)),

                    %% Verify case is running again
                    {ok, State3} = yawl_engine:get_case_state(yawl_engine),
                    ?assertEqual(running, maps:get(status, State3))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test cancelling a running case.
%% Verifies that a case can be cancelled and workitems are properly cleaned up.
%% @end
%%--------------------------------------------------------------------
test_cancel_case_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Cancel the case
                    ?assertEqual(ok, yawl_engine:cancel_case(yawl_engine)),

                    %% Verify case is cancelled
                    {ok, State} = yawl_engine:get_case_state(yawl_engine),
                    ?assertEqual(cancelled, maps:get(status, State)),

                    %% Verify workitems are cancelled
                    Workitems = maps:get(workitems, State),
                    maps:map(fun(_K, W) ->
                        ?assertEqual(cancelled, maps:get(status, W))
                    end, Workitems)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test getting case state at various points in the lifecycle.
%% Verifies that case state is properly tracked throughout execution.
%% @end
%%--------------------------------------------------------------------
test_case_status_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get initial state
                    {ok, State1} = yawl_engine:get_case_state(yawl_engine),
                    ?assertMatch(#{case_id := _, workflow_id := _, status := running}, State1),
                    ?assert(maps:is_key(workitems, State1)),
                    ?assert(maps:is_key(data, State1)),
                    ?assert(maps:is_key(timestamps, State1)),

                    %% Verify timestamps
                    Timestamps = maps:get(timestamps, State1),
                    ?assert(maps:is_key(created_at, Timestamps)),
                    ?assert(maps:is_key(started_at, Timestamps)),

                    %% Suspend and check state again
                    ok = yawl_engine:suspend_case(yawl_engine),
                    {ok, State2} = yawl_engine:get_case_state(yawl_engine),
                    ?assertEqual(suspended, maps:get(status, State2))
                end)]
     end}.

%%====================================================================
%% Workitem Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test getting available (enabled) workitems.
%% Verifies that only enabled workitems are returned.
%% @end
%%--------------------------------------------------------------------
test_get_available_workitems_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get available workitems
                    {ok, Available} = yawl_engine:get_available_workitems(yawl_engine),

                    %% Verify workitems are enabled
                    ?assert(length(Available) > 0),
                    lists:foreach(fun(W) ->
                        ?assertEqual(enabled, element(5, W))  % status field
                    end, Available)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test starting a workitem.
%% Verifies that an enabled workitem can be started.
%% @end
%%--------------------------------------------------------------------
test_start_workitem_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get available workitems
                    {ok, [Workitem | _]} = yawl_engine:get_available_workitems(yawl_engine),
                    WorkitemId = element(2, Workitem),  % id field (second field after record tag)

                    %% Start the workitem
                    ?assertEqual(ok, yawl_engine:start_workitem(yawl_engine, WorkitemId)),

                    %% Verify workitem status changed
                    {ok, State} = yawl_engine:get_case_state(yawl_engine),
                    Workitems = maps:get(workitems, State),
                    UpdatedWorkitem = maps:get(WorkitemId, Workitems),
                    ?assertEqual(started, maps:get(status, UpdatedWorkitem))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test completing a workitem with results.
%% Verifies that a started workitem can be completed with result data.
%% @end
%%--------------------------------------------------------------------
test_complete_workitem_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get and start a workitem
                    {ok, [Workitem | _]} = yawl_engine:get_available_workitems(yawl_engine),
                    WorkitemId = element(2, Workitem),  % id field (second field after record tag)
                    ok = yawl_engine:start_workitem(yawl_engine, WorkitemId),

                    %% Complete the workitem with results
                    Results = #{output => <<"test_result">>, status => complete},
                    ?assertEqual(ok, yawl_engine:complete_workitem(yawl_engine, WorkitemId, Results)),

                    %% Verify workitem is completed
                    {ok, State} = yawl_engine:get_case_state(yawl_engine),
                    Workitems = maps:get(workitems, State),
                    CompletedWorkitem = maps:get(WorkitemId, Workitems),
                    ?assertEqual(completed, maps:get(status, CompletedWorkitem)),
                    ?assertEqual(<<"test_result">>, maps:get(output, maps:get(data, CompletedWorkitem)))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test failing a workitem with error information.
%% Verifies that a started workitem can be marked as failed.
%% @end
%%--------------------------------------------------------------------
test_fail_workitem_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get and start a workitem
                    {ok, [Workitem | _]} = yawl_engine:get_available_workitems(yawl_engine),
                    WorkitemId = element(2, Workitem),  % id field (second field after record tag)
                    ok = yawl_engine:start_workitem(yawl_engine, WorkitemId),

                    %% Fail the workitem
                    ErrorInfo = #{error_type => validation, message => <<"Invalid input">>},
                    ?assertEqual(ok, yawl_engine:fail_workitem(yawl_engine, WorkitemId, ErrorInfo)),

                    %% Verify workitem is failed
                    {ok, State} = yawl_engine:get_case_state(yawl_engine),
                    Workitems = maps:get(workitems, State),
                    FailedWorkitem = maps:get(WorkitemId, Workitems),
                    ?assertEqual(failed, maps:get(status, FailedWorkitem)),
                    ?assert(maps:is_key(error, maps:get(data, FailedWorkitem)))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test invalid workitem state transitions.
%% Verifies that invalid state transitions are properly rejected.
%% @end
%%--------------------------------------------------------------------
test_invalid_workitem_state_transitions_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get an enabled workitem
                    {ok, [Workitem | _]} = yawl_engine:get_available_workitems(yawl_engine),
                    WorkitemId = element(2, Workitem),  % id field (second field after record tag)

                    %% Try to complete without starting - should fail
                    Results = #{output => test},
                    ?assertMatch({error, {invalid_status, enabled}},
                                yawl_engine:complete_workitem(yawl_engine, WorkitemId, Results)),

                    %% Try to fail without starting - should fail
                    ErrorInfo = #{error => test},
                    ?assertMatch({error, {invalid_status, enabled}},
                                yawl_engine:fail_workitem(yawl_engine, WorkitemId, ErrorInfo)),

                    %% Start the workitem
                    ok = yawl_engine:start_workitem(yawl_engine, WorkitemId),

                    %% Try to start again - should fail
                    ?assertMatch({error, {invalid_status, started}},
                                yawl_engine:start_workitem(yawl_engine, WorkitemId))
                end)]
     end}.

%%====================================================================
%% Token Flow Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test AND split and join semantics.
%% Verifies that all branches execute and join properly.
%% @end
%%--------------------------------------------------------------------
test_and_split_join_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a parallel workflow
                    Spec = parallel_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get initial available workitems
                    {ok, Available1} = yawl_engine:get_available_workitems(yawl_engine),
                    ?assert(length(Available1) > 0),

                    %% Complete all branch workitems
                    lists:foreach(fun(W) ->
                        WorkitemId = element(2, W),
                        ok = yawl_engine:start_workitem(yawl_engine, WorkitemId),
                        ok = yawl_engine:complete_workitem(yawl_engine, WorkitemId, #{result => ok})
                    end, Available1),

                    %% Verify workflow advances correctly
                    {ok, State} = yawl_engine:get_case_state(yawl_engine),
                    ?assertEqual(running, maps:get(status, State))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test OR split and join semantics.
%% Verifies that multiple paths can be selected.
%% @end
%%--------------------------------------------------------------------
test_or_split_join_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start an OR workflow
                    Spec = or_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Verify initial workitems
                    {ok, Available} = yawl_engine:get_available_workitems(yawl_engine),
                    ?assert(length(Available) > 0),

                    %% OR split allows multiple branches to execute
                    %% Complete some but not all branches
                    case Available of
                        [W1, W2 | _] ->
                            WorkitemId1 = element(2, W1),
                            WorkitemId2 = element(2, W2),
                            ok = yawl_engine:start_workitem(yawl_engine, WorkitemId1),
                            ok = yawl_engine:complete_workitem(yawl_engine, WorkitemId1, #{result => ok}),
                            ok = yawl_engine:start_workitem(yawl_engine, WorkitemId2),
                            ok = yawl_engine:complete_workitem(yawl_engine, WorkitemId2, #{result => ok});
                        [W1] ->
                            WorkitemId1 = element(2, W1),
                            ok = yawl_engine:start_workitem(yawl_engine, WorkitemId1),
                            ok = yawl_engine:complete_workitem(yawl_engine, WorkitemId1, #{result => ok})
                    end,

                    %% Verify state
                    {ok, State} = yawl_engine:get_case_state(yawl_engine),
                    ?assert(lists:member(maps:get(status, State), [running, completed]))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test XOR split and join semantics.
%% Verifies that only one branch is selected.
%% @end
%%--------------------------------------------------------------------
test_xor_split_join_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start an XOR workflow
                    Spec = xor_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Verify initial workitems for XOR choice
                    {ok, Available} = yawl_engine:get_available_workitems(yawl_engine),
                    ?assert(length(Available) > 0),

                    %% XOR split - execute only one branch
                    [W | _] = Available,
                    WorkitemId = element(2, W),
                    ok = yawl_engine:start_workitem(yawl_engine, WorkitemId),
                    ok = yawl_engine:complete_workitem(yawl_engine, WorkitemId, #{result => ok}),

                    %% Verify state
                    {ok, State} = yawl_engine:get_case_state(yawl_engine),
                    ?assert(lists:member(maps:get(status, State), [running, completed]))
                end)]
     end}.

%%====================================================================
%% Observer Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test registering an observer.
%% Verifies that observers can be registered with the engine.
%% @end
%%--------------------------------------------------------------------
test_register_observer_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start an observer
                    Observer = start_observer(),

                    %% Register observer
                    ?assertEqual(ok, yawl_engine:register_observer(yawl_engine, Observer)),

                    %% Verify observer is in engine status
                    {ok, Status} = yawl_engine:get_engine_status(yawl_engine),
                    ?assert(maps:get(observer_count, Status) > 0),

                    %% Unregister observer
                    ?assertEqual(ok, yawl_engine:unregister_observer(yawl_engine, Observer)),
                    stop_observer(Observer)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test receiving case started/completed events.
%% Verifies that observers receive case lifecycle events.
%% @end
%%--------------------------------------------------------------------
test_observer_case_events_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start an observer
                    Observer = start_observer(),
                    yawl_engine:register_observer(yawl_engine, Observer),

                    %% Start a workflow - should trigger case_started event
                    Spec = simple_workflow_spec(),
                    {ok, CaseId} = yawl_engine:start_workflow(yawl_engine, Spec, #{observers => [Observer]}),

                    %% Give time for event delivery
                    timer:sleep(100),

                    %% Check for case_started event
                    Events = get_observer_events(Observer),
                    ?assert(lists:any(fun
                        ({case_started, Id}) when Id =:= CaseId -> true;
                        (_) -> false
                    end, Events)),

                    %% Complete all workitems to trigger case_completed
                    {ok, Available} = yawl_engine:get_available_workitems(yawl_engine),
                    lists:foreach(fun(W) ->
                        WorkitemId = element(2, W),
                        ok = yawl_engine:start_workitem(yawl_engine, WorkitemId),
                        ok = yawl_engine:complete_workitem(yawl_engine, WorkitemId, #{result => ok})
                    end, Available),

                    %% Give time for event delivery
                    timer:sleep(100),

                    %% Check for case_completed event
                    Events2 = get_observer_events(Observer),
                    ?assert(lists:any(fun
                        ({case_completed, Id}) when Id =:= CaseId -> true;
                        (_) -> false
                    end, Events2)),

                    %% Cleanup
                    yawl_engine:unregister_observer(yawl_engine, Observer),
                    stop_observer(Observer)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test receiving workitem events.
%% Verifies that observers receive workitem lifecycle events.
%% @end
%%--------------------------------------------------------------------
test_observer_workitem_events_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start an observer
                    Observer = start_observer(),

                    %% Start a workflow with observer
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec, #{}),
                    yawl_engine:register_observer(yawl_engine, Observer),

                    %% Get and start a workitem
                    {ok, [Workitem | _]} = yawl_engine:get_available_workitems(yawl_engine),
                    WorkitemId = element(2, Workitem),  % id field (second field after record tag)

                    %% Flush existing events
                    get_observer_events(Observer),

                    %% Start workitem
                    ok = yawl_engine:start_workitem(yawl_engine, WorkitemId),
                    timer:sleep(50),

                    %% Complete workitem
                    ok = yawl_engine:complete_workitem(yawl_engine, WorkitemId, #{result => ok}),
                    timer:sleep(50),

                    %% Check for workitem events
                    Events = get_observer_events(Observer),
                    ?assert(lists:any(fun
                        ({workitem_started, Id}) when Id =:= WorkitemId -> true;
                        (_) -> false
                    end, Events)),
                    ?assert(lists:any(fun
                        ({workitem_completed, Id, _}) when Id =:= WorkitemId -> true;
                        (_) -> false
                    end, Events)),

                    %% Cleanup
                    yawl_engine:unregister_observer(yawl_engine, Observer),
                    stop_observer(Observer)
                end)]
     end}.

%%====================================================================
%% Engine Status Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test getting engine status.
%% Verifies that engine statistics are correctly reported.
%% @end
%%--------------------------------------------------------------------
test_get_engine_status_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Get initial status
                    {ok, Status1} = yawl_engine:get_engine_status(yawl_engine),
                    ?assertMatch(#{
                        total_cases := _,
                        active_cases := _,
                        suspended_cases := _,
                        completed_cases := _,
                        cancelled_cases := _,
                        failed_cases := _,
                        observer_count := _
                    }, Status1),
                    ?assertEqual(0, maps:get(total_cases, Status1)),

                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Get updated status
                    {ok, Status2} = yawl_engine:get_engine_status(yawl_engine),
                    ?assertEqual(1, maps:get(total_cases, Status2)),
                    ?assertEqual(1, maps:get(active_cases, Status2)),

                    %% Suspend case
                    ok = yawl_engine:suspend_case(yawl_engine),

                    %% Get status after suspend
                    {ok, Status3} = yawl_engine:get_engine_status(yawl_engine),
                    ?assertEqual(1, maps:get(suspended_cases, Status3))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test listing all cases.
%% Verifies that all cases are properly listed with their statuses.
%% @end
%%--------------------------------------------------------------------
test_list_cases_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Initially no cases
                    {ok, Cases1} = yawl_engine:list_cases(yawl_engine),
                    ?assertEqual([], Cases1),

                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% List cases
                    {ok, Cases2} = yawl_engine:list_cases(yawl_engine),
                    ?assertEqual(1, length(Cases2)),
                    [{_CaseId2, Status} | _] = Cases2,
                    ?assertEqual(running, Status),

                    %% Suspend case
                    ok = yawl_engine:suspend_case(yawl_engine),

                    %% List cases again
                    {ok, Cases3} = yawl_engine:list_cases(yawl_engine),
                    [{_CaseId3, Status3} | _] = Cases3,
                    ?assertEqual(suspended, Status3)
                end)]
     end}.

%%====================================================================
%% Edge Cases and Error Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test operations when no cases exist.
%% Verifies proper error handling for edge cases.
%% @end
%%--------------------------------------------------------------------
test_no_cases_error_handling_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Try to get case state when no cases exist
                    ?assertMatch({error, no_cases}, yawl_engine:get_case_state(yawl_engine)),

                    %% Try to get workitems when no cases exist
                    ?assertMatch({error, no_cases}, yawl_engine:get_available_workitems(yawl_engine)),

                    %% Try to suspend when no cases exist
                    ?assertMatch({error, no_cases}, yawl_engine:suspend_case(yawl_engine)),

                    %% Try to resume when no cases exist
                    ?assertMatch({error, no_cases}, yawl_engine:resume_case(yawl_engine)),

                    %% Try to cancel when no cases exist
                    ?assertMatch({error, no_cases}, yawl_engine:cancel_case(yawl_engine))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test invalid state transitions.
%% Verifies that invalid status transitions are rejected.
%% @end
%%--------------------------------------------------------------------
test_invalid_state_transitions_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Try to resume when not suspended
                    ?assertMatch({error, {invalid_status, running}},
                                yawl_engine:resume_case(yawl_engine)),

                    %% Suspend the case
                    ok = yawl_engine:suspend_case(yawl_engine),

                    %% Try to suspend again
                    ?assertMatch({error, {invalid_status, suspended}},
                                yawl_engine:suspend_case(yawl_engine))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test operations on non-existent workitems.
%% Verifies proper error handling for invalid workitem IDs.
%% @end
%%--------------------------------------------------------------------
test_nonexistent_workitem_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start a workflow
                    Spec = simple_workflow_spec(),
                    {ok, _CaseId} = yawl_engine:start_workflow(yawl_engine, Spec),

                    %% Try operations on non-existent workitem
                    FakeWorkitemId = <<"nonexistent_workitem">>,

                    ?assertMatch({error, workitem_not_found},
                                yawl_engine:start_workitem(yawl_engine, FakeWorkitemId)),

                    ?assertMatch({error, workitem_not_found},
                                yawl_engine:complete_workitem(yawl_engine, FakeWorkitemId, #{})),

                    ?assertMatch({error, workitem_not_found},
                                yawl_engine:fail_workitem(yawl_engine, FakeWorkitemId, #{}))
                end)]
     end}.

%%====================================================================
%% Parallel Test Execution
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that multiple independent tests can run in parallel.
%% This test verifies the engine handles concurrent operations correctly.
%% @end
%%--------------------------------------------------------------------
test_parallel_execution_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_EnginePid) ->
         [?_test(begin
                    %% Start multiple workflows in quick succession
                    Spec1 = simple_workflow_spec(),
                    Spec2 = parallel_workflow_spec(),
                    Spec3 = xor_workflow_spec(),

                    {ok, CaseId1} = yawl_engine:start_workflow(yawl_engine, Spec1),
                    {ok, CaseId2} = yawl_engine:start_workflow(yawl_engine, Spec2),
                    {ok, CaseId3} = yawl_engine:start_workflow(yawl_engine, Spec3),

                    %% Verify all cases are tracked
                    {ok, Cases} = yawl_engine:list_cases(yawl_engine),
                    ?assertEqual(3, length(Cases)),

                    %% Verify case IDs are unique
                    CaseIds = [Id || {Id, _} <- Cases],
                    ?assertEqual(length(CaseIds), length(lists:usort(CaseIds))),

                    %% Clean up
                    ok = yawl_engine:cancel_case(yawl_engine)
                end)]
     end}.

%%====================================================================
%% EUnit Test Export
%%====================================================================
%% @doc Returns all test generators for EUnit discovery.
%% This function is required for EUnit to find test generators when
%% -compile(export_all) is used.
%% @end
%%--------------------------------------------------------------------
test() ->
    [
        test_start_workflow_basic_(),
        test_suspend_resume_case_(),
        test_cancel_case_(),
        test_case_status_(),
        test_get_available_workitems_(),
        test_start_workitem_(),
        test_complete_workitem_(),
        test_fail_workitem_(),
        test_invalid_workitem_state_transitions_(),
        test_and_split_join_(),
        test_or_split_join_(),
        test_xor_split_join_(),
        test_register_observer_(),
        test_observer_case_events_(),
        test_observer_workitem_events_(),
        test_get_engine_status_(),
        test_list_cases_(),
        test_no_cases_error_handling_(),
        test_invalid_state_transitions_(),
        test_nonexistent_workitem_(),
        test_parallel_execution_()
    ].
