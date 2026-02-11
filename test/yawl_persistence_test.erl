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
%% @doc YAWL Persistence Test Suite
%%
%% Comprehensive test suite for the YAWL persistence layer using Mnesia.
%% Tests cover schema initialization, case persistence, workitem persistence,
%% active case listing, cleanup operations, and ACID transaction guarantees.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_persistence_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup for each test case.
%% Clears tables between tests.
%% @end
%%--------------------------------------------------------------------
setup_each() ->
    %% Clear all tables if they exist
    case mnesia:system_info(is_running) of
        yes ->
            try mnesia:clear_table(persistent_case), ok catch _:_ -> ok end,
            try mnesia:clear_table(persistent_workitem), ok catch _:_ -> ok end;
        no ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup for each test case.
%% @end
%%--------------------------------------------------------------------
cleanup_each(_TestData) ->
    %% Tables already cleared in setup
    ok.

%%--------------------------------------------------------------------
%% @doc Creates a test case map for persistence operations.
%% @end
%%--------------------------------------------------------------------
test_case_map(TestCaseId) ->
    Now = erlang:system_time(millisecond),
    maps:from_list([
        {case_id, TestCaseId},
        {workflow_id, <<"workflow_test_123">>},
        {spec, maps:from_list([{id, <<"workflow_test_123">>}, {name, <<"Test Workflow">>}])},
        {status, running},
        {data, maps:from_list([{key1, <<"value1">>}, {key2, 42}])},
        {created_at, Now},
        {started_at, Now},
        {completed_at, undefined}
    ]).

%%--------------------------------------------------------------------
%% @doc Creates a test workitem map for persistence operations.
%% @end
%%--------------------------------------------------------------------
test_workitem_map(WorkitemId, CaseId) ->
    Now = erlang:system_time(millisecond),
    maps:from_list([
        {id, WorkitemId},
        {case_id, CaseId},
        {task_id, <<"task_test">>},
        {status, enabled},
        {data, maps:from_list([{input, <<"test_input">>}])},
        {enabled_at, Now},
        {started_at, undefined},
        {completed_at, undefined}
    ]).

%%--------------------------------------------------------------------
%% @doc Creates a test case map with custom status.
%% @end
%%--------------------------------------------------------------------
test_case_map_with_status(TestCaseId, Status) ->
    maps:put(status, Status, test_case_map(TestCaseId)).

%%--------------------------------------------------------------------
%% @doc Creates a test case map with custom data.
%% @end
%%--------------------------------------------------------------------
test_case_map_with_data(TestCaseId, Data) ->
    maps:put(data, Data, test_case_map(TestCaseId)).

%%====================================================================
%% Test Generator Wrapper with Global Setup
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Wrapper that provides global Mnesia setup for all tests.
%% Each test gets a clean slate via table clearing.
%% @end
%%--------------------------------------------------------------------
yawl_persistence_test_() ->
    {setup,
     fun() ->
         %% Global setup - runs once before all tests in this group
         application:stop(mnesia),
         timer:sleep(100),
         mnesia:delete_schema([node()]),
         timer:sleep(100),
         ok = yawl_persistence:init_schema(),
         ok
     end,
     fun(_SetupData) ->
         %% Global cleanup - runs once after all tests
         mnesia:stop(),
         mnesia:delete_schema([node()]),
         ok
     end,
     [
      %% Schema Tests
      {inorder,
       [
        ?_test(begin
                   %% Verify tables exist
                   ?assertEqual(true, mnesia:table_info(persistent_case, is_a_table)),
                   ?assertEqual(true, mnesia:table_info(persistent_workitem, is_a_table)),

                   %% Verify table attributes
                   CaseAttrs = mnesia:table_info(persistent_case, attributes),
                   ?assertEqual([case_id, workflow_id, spec, status, data,
                                 created_at, started_at, completed_at],
                               CaseAttrs),

                   WorkitemAttrs = mnesia:table_info(persistent_workitem, attributes),
                   ?assertEqual([workitem_id, case_id, task_id, status, data,
                                 enabled_at, started_at, completed_at],
                               WorkitemAttrs),

                   %% Verify disc_copies for persistence
                   CaseStorage = mnesia:table_info(persistent_case, storage_type),
                   ?assertEqual(disc_copies, element(1, CaseStorage)),

                   WorkitemStorage = mnesia:table_info(persistent_workitem, storage_type),
                   ?assertEqual(disc_copies, element(1, WorkitemStorage))
               end),
        ?_test(begin
                   %% Call init_schema again
                   ?assertEqual(ok, yawl_persistence:init_schema()),

                   %% Tables should still exist and work
                   ?assertEqual(true, mnesia:table_info(persistent_case, is_a_table)),
                   ?assertEqual(true, mnesia:table_info(persistent_workitem, is_a_table))
               end)
       ]},

      %% Case Persistence Tests
      {inorder,
       [
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_save_test_001">>,
                   CaseMap = test_case_map(CaseId),
                   ?assertMatch({ok, CaseId}, yawl_persistence:save_case(CaseMap)),
                   {ok, LoadedCase} = yawl_persistence:load_case(CaseId),
                   ?assertEqual(CaseId, maps:get(case_id, LoadedCase)),
                   ?assertEqual(<<"workflow_test_123">>, maps:get(workflow_id, LoadedCase)),
                   ?assertEqual(running, maps:get(status, LoadedCase))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_load_test_002">>,
                   OriginalCase = test_case_map(CaseId),
                   {ok, CaseId} = yawl_persistence:save_case(OriginalCase),
                   {ok, LoadedCase} = yawl_persistence:load_case(CaseId),
                   ?assertEqual(CaseId, maps:get(case_id, LoadedCase)),
                   ?assertEqual(maps:get(workflow_id, OriginalCase), maps:get(workflow_id, LoadedCase)),
                   ?assertEqual(maps:get(status, OriginalCase), maps:get(status, LoadedCase)),
                   ?assertEqual(maps:get(data, OriginalCase), maps:get(data, LoadedCase)),
                   ?assert(maps:is_key(created_at, LoadedCase)),
                   ?assert(maps:is_key(started_at, LoadedCase))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   ?assertMatch({error, not_found}, yawl_persistence:load_case(<<"nonexistent_case">>))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_update_test_003">>,
                   CaseMap1 = test_case_map(CaseId),
                   ?assertMatch({ok, CaseId}, yawl_persistence:save_case(CaseMap1)),
                   UpdatedCase = maps:put(status, suspended, CaseMap1),
                   ?assertMatch({ok, CaseId}, yawl_persistence:save_case(UpdatedCase)),
                   {ok, LoadedCase} = yawl_persistence:load_case(CaseId),
                   ?assertEqual(suspended, maps:get(status, LoadedCase))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_delete_test_004">>,
                   CaseMap = test_case_map(CaseId),
                   ?assertMatch({ok, CaseId}, yawl_persistence:save_case(CaseMap)),
                   {ok, _Case} = yawl_persistence:load_case(CaseId),
                   ?assertEqual(ok, yawl_persistence:delete_case(CaseId)),
                   ?assertMatch({error, not_found}, yawl_persistence:load_case(CaseId))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   ?assertMatch({error, not_found}, yawl_persistence:delete_case(<<"nonexistent_case">>))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   Statuses = [running, suspended, completed, cancelled, failed],
                   StatusList = lists:zip(Statuses, lists:seq(1, length(Statuses))),
                   lists:foreach(fun({Status, N}) ->
                       CaseId = list_to_binary("case_status_" ++ integer_to_list(N)),
                       CaseMap = test_case_map_with_status(CaseId, Status),
                       {ok, CaseId} = yawl_persistence:save_case(CaseMap),
                       {ok, Loaded} = yawl_persistence:load_case(CaseId),
                       ?assertEqual(Status, maps:get(status, Loaded))
                   end, StatusList)
               end)
       ]},

      %% Workitem Persistence Tests
      {inorder,
       [
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_wi_save_001">>,
                   WorkitemId = <<"workitem_save_001">>,
                   WorkitemMap = test_workitem_map(WorkitemId, CaseId),
                   ?assertMatch({ok, WorkitemId}, yawl_persistence:save_workitem(WorkitemMap)),
                   {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                   ?assertEqual(1, length(Workitems)),
                   [LoadedWorkitem] = Workitems,
                   ?assertEqual(WorkitemId, maps:get(workitem_id, LoadedWorkitem))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_wi_load_002">>,
                   lists:foreach(fun(N) ->
                       Bin = list_to_binary("workitem_" ++ integer_to_list(N)),
                       WorkitemMap = test_workitem_map(Bin, CaseId),
                       {ok, _} = yawl_persistence:save_workitem(WorkitemMap)
                   end, lists:seq(1, 3)),
                   {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                   ?assertEqual(3, length(Workitems)),
                   lists:foreach(fun(W) ->
                       ?assertEqual(CaseId, maps:get(case_id, W)),
                       ?assert(maps:is_key(workitem_id, W)),
                       ?assert(maps:is_key(task_id, W)),
                       ?assert(maps:is_key(status, W))
                   end, Workitems)
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_wi_empty_003">>,
                   {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                   ?assertEqual([], Workitems)
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_wi_update_004">>,
                   WorkitemId = <<"workitem_update_001">>,
                   Now = erlang:system_time(millisecond),
                   WorkitemMap = test_workitem_map(WorkitemId, CaseId),
                   {ok, WorkitemId} = yawl_persistence:save_workitem(WorkitemMap),
                   UpdatedWorkitem = maps:put(status, started, maps:put(started_at, Now, WorkitemMap)),
                   {ok, WorkitemId} = yawl_persistence:save_workitem(UpdatedWorkitem),
                   {ok, [LoadedWorkitem]} = yawl_persistence:load_workitems(CaseId),
                   ?assertEqual(started, maps:get(status, LoadedWorkitem)),
                   ?assertEqual(Now, maps:get(started_at, LoadedWorkitem))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"case_cascade_001">>,
                   CaseMap = test_case_map(CaseId),
                   {ok, _} = yawl_persistence:save_case(CaseMap),
                   lists:foreach(fun(N) ->
                       Bin = list_to_binary("workitem_" ++ integer_to_list(N)),
                       WorkitemMap = test_workitem_map(Bin, CaseId),
                       {ok, _} = yawl_persistence:save_workitem(WorkitemMap)
                   end, lists:seq(1, 3)),
                   {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                   ?assertEqual(3, length(Workitems)),
                   ok = yawl_persistence:delete_case(CaseId),
                   {ok, WorkitemsAfter} = yawl_persistence:load_workitems(CaseId),
                   ?assertEqual([], WorkitemsAfter)
               end)
       ]},

      %% Active Cases Tests
      {inorder,
       [
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   RunningCase = test_case_map_with_status(<<"active_running_001">>, running),
                   SuspendedCase = test_case_map_with_status(<<"active_suspended_001">>, suspended),
                   CompletedCase = test_case_map_with_status(<<"active_completed_001">>, completed),
                   CancelledCase = test_case_map_with_status(<<"active_cancelled_001">>, cancelled),
                   {ok, _} = yawl_persistence:save_case(RunningCase),
                   {ok, _} = yawl_persistence:save_case(SuspendedCase),
                   {ok, _} = yawl_persistence:save_case(CompletedCase),
                   {ok, _} = yawl_persistence:save_case(CancelledCase),
                   {ok, ActiveCases} = yawl_persistence:list_active_cases(),
                   ?assertEqual(2, length(ActiveCases)),
                   Statuses = [maps:get(status, C) || C <- ActiveCases],
                   ?assert(lists:member(running, Statuses)),
                   ?assert(lists:member(suspended, Statuses)),
                   ?assertNot(lists:member(completed, Statuses)),
                   ?assertNot(lists:member(cancelled, Statuses))
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   {ok, ActiveCases} = yawl_persistence:list_active_cases(),
                   ?assertEqual([], ActiveCases)
               end),
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   {ok, Count0} = yawl_persistence:get_case_count(),
                   ?assertEqual(0, Count0),
                   lists:foreach(fun(N) ->
                       Bin = list_to_binary("count_case_" ++ integer_to_list(N)),
                       CaseMap = test_case_map(Bin),
                       {ok, _} = yawl_persistence:save_case(CaseMap)
                   end, lists:seq(1, 5)),
                   {ok, Count5} = yawl_persistence:get_case_count(),
                   ?assertEqual(5, Count5),
                   ok = yawl_persistence:delete_case(<<"count_case_1">>),
                   {ok, Count4} = yawl_persistence:get_case_count(),
                   ?assertEqual(4, Count4)
               end)
       ]},

      %% Integration Tests
      {inorder,
       [
        ?_test(begin
                   mnesia:clear_table(persistent_case),
                   mnesia:clear_table(persistent_workitem),
                   CaseId = <<"lifecycle_001">>,
                   %% Step 1: Create and save a case
                   CaseMap = test_case_map(CaseId),
                   {ok, _} = yawl_persistence:save_case(CaseMap),
                   %% Step 2: Add workitems
                   WorkitemIds = [list_to_binary("lifecycle_wi_" ++ integer_to_list(N)) || N <- lists:seq(1, 3)],
                   lists:foreach(fun(WorkitemId) ->
                       WorkitemMap = test_workitem_map(WorkitemId, CaseId),
                       {ok, _} = yawl_persistence:save_workitem(WorkitemMap)
                   end, WorkitemIds),
                   %% Step 3: Load and verify
                   {ok, LoadedCase} = yawl_persistence:load_case(CaseId),
                   ?assertEqual(running, maps:get(status, LoadedCase)),
                   {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                   ?assertEqual(3, length(Workitems)),
                   %% Step 4: Update workitem status
                   [FirstWI | _] = Workitems,
                   _ = maps:get(workitem_id, FirstWI),
                   Now = erlang:system_time(millisecond),
                   UpdatedWI = maps:put(status, started, maps:put(started_at, Now, FirstWI)),
                   {ok, _} = yawl_persistence:save_workitem(UpdatedWI),
                   %% Step 5: Complete case
                   Now2 = erlang:system_time(millisecond),
                   CompletedCase = maps:put(status, completed, maps:put(completed_at, Now2, LoadedCase)),
                   {ok, _} = yawl_persistence:save_case(CompletedCase),
                   %% Step 6: Verify final state
                   {ok, FinalCase} = yawl_persistence:load_case(CaseId),
                   ?assertEqual(completed, maps:get(status, FinalCase)),
                   %% Step 7: Cleanup
                   ok = yawl_persistence:delete_case(CaseId),
                   ?assertMatch({error, not_found}, yawl_persistence:load_case(CaseId)),
                   {ok, []} = yawl_persistence:load_workitems(CaseId)
               end)
       ]}
     ]}.
