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
%% @doc Test Suite for Mock Utilities
%%
%% This module contains integration tests for all mock utilities,
%% ensuring they work correctly together and produce valid test data.
%%
%% @end
%% -------------------------------------------------------------------

-module(mocks_tests).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Exports
%%====================================================================

-export([run_all_tests/0]).
-export([test_event_log_mocks/0]).
-export([test_pnet_mocks/0]).
-export([test_workflow_mocks/0]).
-export([test_mnesia_mocks/0]).
-export([test_time_mocks/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs all mock utility tests.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_all_tests() -> ok.

run_all_tests() ->
    io:format("Testing Event Log Mocks...~n"),
    test_event_log_mocks(),
    io:format("Testing Petri Net Mocks...~n"),
    test_pnet_mocks(),
    io:format("Testing Workflow Mocks...~n"),
    test_workflow_mocks(),
    io:format("Testing Mnesia Mocks...~n"),
    test_mnesia_mocks(),
    io:format("Testing Time Mocks...~n"),
    test_time_mocks(),
    io:format("All mock tests passed!~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Tests event_log_mocks module.
%%
%% @end
%%--------------------------------------------------------------------
-spec test_event_log_mocks() -> ok.

test_event_log_mocks() ->
    %% Test simple log
    SimpleLog = event_log_mocks:simple_log(),
    ?assert(length(SimpleLog) > 0),

    %% Test validate_log
    {ok, _} = event_log_mocks:validate_log(SimpleLog),

    %% Test count_events
    CreatedCount = event_log_mocks:count_events(SimpleLog, case_created),
    ?assert(CreatedCount > 0),

    %% Test filter_by_case
    FirstCase = maps:get(case_id, hd(SimpleLog)),
    Filtered = event_log_mocks:filter_by_case(SimpleLog, FirstCase),
    ?assert(length(Filtered) > 0),

    %% Test noisy log
    NoisyLog = event_log_mocks:noisy_log(),
    ?assert(length(NoisyLog) > 0),

    %% Test large log
    LargeLog = event_log_mocks:large_log(50),
    ?assertEqual(50, length(LargeLog)),

    %% Test trace log
    TraceLog = event_log_mocks:trace_log(),
    ?assert(length(TraceLog) > 0),

    ok.

%%--------------------------------------------------------------------
%% @doc Tests pnet_mocks module.
%%
%% @end
%%--------------------------------------------------------------------
-spec test_pnet_mocks() -> ok.

test_pnet_mocks() ->
    %% Test simple net
    SimpleNet = pnet_mocks:simple_net(),
    ?assert(pnet_mocks:is_valid_net(SimpleNet)),

    %% Test mock net state
    State = pnet_mocks:mock_net_state(),
    ?assert(is_map(State)),
    ?assert(is_atom(maps:get(net_mod, State))),

    %% Test marking with tokens
    Marking = pnet_mocks:marking_with_tokens([p1, p2], #{p1 => 2, p2 => 0}),
    ?assertEqual(2, length(maps:get(p1, Marking))),
    ?assertEqual([], maps:get(p2, Marking)),

    %% Test parallel net
    ParallelNet = pnet_mocks:parallel_net(),
    ?assert(pnet_mocks:is_valid_net(ParallelNet)),
    ?assert(length(maps:get(places, ParallelNet)) > 2),

    %% Test loop net
    LoopNet = pnet_mocks:loop_net(),
    ?assert(pnet_mocks:is_valid_net(LoopNet)),

    %% Test places and transitions
    Places = pnet_mocks:mock_places(5),
    ?assertEqual(5, length(Places)),

    Transitions = pnet_mocks:mock_transitions(3),
    ?assertEqual(3, length(Transitions)),

    ok.

%%--------------------------------------------------------------------
%% @doc Tests workflow_mocks module.
%%
%% @end
%%--------------------------------------------------------------------
-spec test_workflow_mocks() -> ok.

test_workflow_mocks() ->
    %% Test simple workflow
    SimpleWF = workflow_mocks:simple_workflow(),
    ?assert(workflow_mocks:is_valid_workflow(SimpleWF)),

    %% Test task operations
    TaskIds = workflow_mocks:task_ids(maps:get(tasks, SimpleWF)),
    ?assert(length(TaskIds) >= 2),

    FirstTaskId = hd(TaskIds),
    Task = workflow_mocks:get_task(SimpleWF, FirstTaskId),
    ?assertNotEqual(undefined, Task),

    %% Test complex workflow
    ComplexWF = workflow_mocks:complex_workflow(),
    ?assert(workflow_mocks:is_valid_workflow(ComplexWF)),
    ?assert(length(maps:get(flows, ComplexWF)) > 0),

    %% Test approval workflow
    ApprovalWF = workflow_mocks:approval_workflow(),
    ?assertEqual(<<"approval_wf">>, maps:get(id, ApprovalWF)),

    %% Test parallel workflow
    ParallelWF = workflow_mocks:parallel_workflow(),
    ?assert(length(maps:get(tasks, ParallelWF)) >= 5),

    %% Test loop workflow
    LoopWF = workflow_mocks:loop_workflow(),
    ?assert(length(maps:get(flows, LoopWF)) >= 3),

    %% Test task generators
    AtomicTask = workflow_mocks:atomic_task(<<"atomic1">>),
    ?assertEqual(atomic, maps:get(type, AtomicTask)),

    CompositeTask = workflow_mocks:composite_task(<<"comp1">>),
    ?assertEqual(composite, maps:get(type, CompositeTask)),

    MITask = workflow_mocks:multi_instance_task(<<"mi1">>, {2, 5}),
    ?assertEqual(multi_instance, maps:get(type, MITask)),

    %% Test YAWL spec
    YawlSpec = workflow_mocks:yawl_spec(),
    ?assert(is_binary(maps:get(id, YawlSpec))),

    ok.

%%--------------------------------------------------------------------
%% @doc Tests mnesia_mocks module.
%%
%% @end
%%--------------------------------------------------------------------
-spec test_mnesia_mocks() -> ok.

test_mnesia_mocks() ->
    %% Setup database
    ?assertEqual(ok, mnesia_mocks:setup_db()),

    try
        %% Create tables
        TableDefs = [
            {test_users, [id, name, email]},
            {test_posts, [id, user_id, content]}
        ],
        ?assertEqual(ok, mnesia_mocks:init_tables(TableDefs)),

        %% Test write and read
        ?assertEqual(ok, mnesia_mocks:mock_write(test_users,
            {test_users, 1, <<"Alice">>, <<"alice@example.com">>})),

        {ok, User} = mnesia_mocks:mock_read(test_users, 1),
        ?assertEqual(1, element(2, User)),
        ?assertEqual(<<"Alice">>, element(3, User)),

        %% Test dirty operations
        ?assertEqual(ok, mnesia_mocks:dirty_write(test_users,
            {test_users, 2, <<"Bob">>, <<"bob@example.com">>})),

        {ok, Bob} = mnesia_mocks:dirty_read(test_users, 2),
        ?assertEqual(<<"Bob">>, element(3, Bob)),

        %% Test fold
        ?assertEqual(ok, mnesia_mocks:mock_write(test_users,
            {test_users, 3, <<"Charlie">>, <<"charlie@example.com">>})),

        {ok, Count} = mnesia_mocks:mock_fold(test_users,
            fun(_Rec, Acc) -> Acc + 1 end, 0),
        ?assertEqual(3, Count),

        %% Test table info
        {ok, Attrs} = mnesia_mocks:table_info(test_users, attributes),
        ?assertEqual([id, name, email], Attrs),

        %% Test all keys
        {ok, Keys} = mnesia_mocks:all_keys(test_users),
        ?assertEqual(3, length(Keys)),

        %% Test select
        {ok, Results} = mnesia_mocks:select(test_users, {test_users, '_', '_', '_'}),
        ?assertEqual(3, length(Results))

    after
        %% Teardown
        ?assertEqual(ok, mnesia_mocks:teardown_db())
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc Tests time_mocks module.
%%
%% @end
%%--------------------------------------------------------------------
-spec test_time_mocks() -> ok.

test_time_mocks() ->
    %% Test freeze/unfreeze
    ?assertEqual(false, time_mocks:is_frozen()),
    ?assertEqual(ok, time_mocks:freeze_time(10000)),
    ?assertEqual(true, time_mocks:is_frozen()),
    ?assertEqual(10000, time_mocks:mock_timestamp()),

    %% Test advance
    ?assertEqual(ok, time_mocks:advance_time(500)),
    ?assertEqual(10500, time_mocks:mock_timestamp()),

    %% Test different time units
    ?assertEqual(ok, time_mocks:advance_time(second, 1)),
    ?assertEqual(11500, time_mocks:mock_timestamp()),

    ?assertEqual(ok, time_mocks:advance_time(minute, 1)),
    ?assertEqual(71500, time_mocks:mock_timestamp()),

    %% Test unfreeze
    ?assertEqual(ok, time_mocks:unfreeze_time()),
    ?assertEqual(false, time_mocks:is_frozen()),

    %% Test datetime conversion
    ?assertEqual({{1970, 1, 1}, {0, 0, 0}},
                 time_mocks:millis_to_datetime(0)),

    Result1 = time_mocks:datetime_to_millis({{1970, 1, 1}, {0, 0, 0}}),
    ?assertEqual(0, Result1),

    %% Test wait_until
    ?assertEqual(true, time_mocks:wait_until(fun() -> true end, 100)),
    ?assertEqual(timeout, time_mocks:wait_until(fun() -> false end, 100)),

    %% Test sleep_mock
    ?assertEqual(ok, time_mocks:freeze_time(5000)),
    ?assertEqual(ok, time_mocks:sleep_mock(250)),
    ?assertEqual(5250, time_mocks:mock_timestamp()),
    ?assertEqual(ok, time_mocks:unfreeze_time()),

    ok.

%%====================================================================
%% EUnit Test Generator
%%====================================================================

mocks_integration_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
         {"Event log mocks integration", fun() ->
             Log = event_log_mocks:simple_log(),
             ?assert(length(Log) > 0)
         end},
         {"Petri net mocks integration", fun() ->
             Net = pnet_mocks:simple_net(),
             ?assert(pnet_mocks:is_valid_net(Net))
         end},
         {"Workflow mocks integration", fun() ->
             WF = workflow_mocks:simple_workflow(),
             ?assert(workflow_mocks:is_valid_workflow(WF))
         end},
         {"Time mocks integration", fun() ->
             ?assertEqual(ok, time_mocks:freeze_time(1000)),
             ?assertEqual(1000, time_mocks:mock_timestamp()),
             ?assertEqual(ok, time_mocks:unfreeze_time())
         end}
     ]
    }.
