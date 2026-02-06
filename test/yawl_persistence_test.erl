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
%% Global Setup - runs once before all tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Global setup that runs once before all tests.
%% Initializes Mnesia and creates schema.
%% @end
%%--------------------------------------------------------------------
setup_global() ->
    %% Clean setup
    application:stop(mnesia),
    timer:sleep(100),
    mnesia:delete_schema([node()]),
    timer:sleep(100),
    %% Initialize schema
    ok = yawl_persistence:init_schema(),
    %% Verify Mnesia is running
    yes = mnesia:system_info(is_running),
    ok.

%%====================================================================
%% Schema Initialization Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test Mnesia schema initialization.
%% Verifies that tables are created with correct attributes.
%% @end
%%--------------------------------------------------------------------
test_init_schema_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
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
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test calling init_schema when already initialized.
%% Verifies idempotency of schema initialization.
%% @end
%%--------------------------------------------------------------------
test_init_schema_idempotent_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    %% Call init_schema again
                    ?assertEqual(ok, yawl_persistence:init_schema()),

                    %% Tables should still exist and work
                    ?assertEqual(true, mnesia:table_info(persistent_case, is_a_table)),
                    ?assertEqual(true, mnesia:table_info(persistent_workitem, is_a_table))
                end)]}.

%%====================================================================
%% Case Persistence Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test saving a case with map input.
%% Verifies that a case map can be persisted to Mnesia.
%% @end
%%--------------------------------------------------------------------
test_save_case_map_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_save_test_001">>,
                    CaseMap = test_case_map(CaseId),

                    %% Save the case
                    ?assertMatch({ok, CaseId}, yawl_persistence:save_case(CaseMap)),

                    %% Verify case was saved
                    {ok, LoadedCase} = yawl_persistence:load_case(CaseId),
                    ?assertEqual(CaseId, maps:get(case_id, LoadedCase)),
                    ?assertEqual(<<"workflow_test_123">>, maps:get(workflow_id, LoadedCase)),
                    ?assertEqual(running, maps:get(status, LoadedCase))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test loading a saved case.
%% Verifies that a case can be retrieved with all fields intact.
%% @end
%%--------------------------------------------------------------------
test_load_case_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_load_test_002">>,
                    OriginalCase = test_case_map(CaseId),

                    %% Save the case
                    {ok, CaseId} = yawl_persistence:save_case(OriginalCase),

                    %% Load the case
                    {ok, LoadedCase} = yawl_persistence:load_case(CaseId),

                    %% Verify all fields
                    ?assertEqual(CaseId, maps:get(case_id, LoadedCase)),
                    ?assertEqual(maps:get(workflow_id, OriginalCase), maps:get(workflow_id, LoadedCase)),
                    ?assertEqual(maps:get(status, OriginalCase), maps:get(status, LoadedCase)),
                    ?assertEqual(maps:get(data, OriginalCase), maps:get(data, LoadedCase)),
                    ?assert(maps:is_key(created_at, LoadedCase)),
                    ?assert(maps:is_key(started_at, LoadedCase))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test loading a non-existent case.
%% Verifies proper error handling for missing cases.
%% @end
%%--------------------------------------------------------------------
test_load_case_not_found_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    %% Try to load non-existent case
                    ?assertMatch({error, not_found},
                                yawl_persistence:load_case(<<"nonexistent_case">>))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test updating an existing case.
%% Verifies that case updates work correctly (upsert semantics).
%% @end
%%--------------------------------------------------------------------
test_update_case_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_update_test_003">>,
                    CaseMap1 = test_case_map(CaseId),

                    %% Save initial case
                    ?assertMatch({ok, CaseId}, yawl_persistence:save_case(CaseMap1)),

                    %% Update case status
                    UpdatedCase = maps:put(status, suspended, CaseMap1),
                    ?assertMatch({ok, CaseId}, yawl_persistence:save_case(UpdatedCase)),

                    %% Verify update
                    {ok, LoadedCase} = yawl_persistence:load_case(CaseId),
                    ?assertEqual(suspended, maps:get(status, LoadedCase))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test deleting a case.
%% Verifies that a case can be deleted with cascade to workitems.
%% @end
%%--------------------------------------------------------------------
test_delete_case_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_delete_test_004">>,
                    CaseMap = test_case_map(CaseId),

                    %% Save the case
                    ?assertMatch({ok, CaseId}, yawl_persistence:save_case(CaseMap)),

                    %% Verify case exists
                    {ok, _Case} = yawl_persistence:load_case(CaseId),

                    %% Delete the case
                    ?assertEqual(ok, yawl_persistence:delete_case(CaseId)),

                    %% Verify case is gone
                    ?assertMatch({error, not_found}, yawl_persistence:load_case(CaseId))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test deleting a non-existent case.
%% Verifies proper error handling.
%% @end
%%--------------------------------------------------------------------
test_delete_case_not_found_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    ?assertMatch({error, not_found},
                                yawl_persistence:delete_case(<<"nonexistent_case">>))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test different case statuses.
%% Verifies all valid statuses can be stored and retrieved.
%% @end
%%--------------------------------------------------------------------
test_case_statuses_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    Statuses = [running, suspended, completed, cancelled, failed],
                    %% Create a list of {Status, Index} tuples
                    StatusList = lists:zip(Statuses, lists:seq(1, length(Statuses))),
                    lists:foreach(fun({Status, N}) ->
                        CaseId = list_to_binary("case_status_" ++ integer_to_list(N)),
                        CaseMap = test_case_map_with_status(CaseId, Status),
                        {ok, CaseId} = yawl_persistence:save_case(CaseMap),
                        {ok, Loaded} = yawl_persistence:load_case(CaseId),
                        ?assertEqual(Status, maps:get(status, Loaded))
                    end, StatusList)
                end)]}.

%%====================================================================
%% Workitem Persistence Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test saving a workitem with map input.
%% Verifies that a workitem map can be persisted.
%% @end
%%--------------------------------------------------------------------
test_save_workitem_map_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_wi_save_001">>,
                    WorkitemId = <<"workitem_save_001">>,
                    WorkitemMap = test_workitem_map(WorkitemId, CaseId),

                    %% Save the workitem
                    ?assertMatch({ok, WorkitemId}, yawl_persistence:save_workitem(WorkitemMap)),

                    %% Verify it was saved
                    {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                    ?assertEqual(1, length(Workitems)),
                    [LoadedWorkitem] = Workitems,
                    ?assertEqual(WorkitemId, maps:get(workitem_id, LoadedWorkitem))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test loading workitems for a case.
%% Verifies all workitems for a case are retrieved.
%% @end
%%--------------------------------------------------------------------
test_load_workitems_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_wi_load_002">>,

                    %% Save multiple workitems
                    lists:foreach(fun(N) ->
                        Bin = list_to_binary("workitem_" ++ integer_to_list(N)),
                        WorkitemMap = test_workitem_map(Bin, CaseId),
                        {ok, _} = yawl_persistence:save_workitem(WorkitemMap)
                    end, lists:seq(1, 3)),

                    %% Load all workitems for the case
                    {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                    ?assertEqual(3, length(Workitems)),

                    %% Verify workitem fields
                    lists:foreach(fun(W) ->
                        ?assertEqual(CaseId, maps:get(case_id, W)),
                        ?assert(maps:is_key(workitem_id, W)),
                        ?assert(maps:is_key(task_id, W)),
                        ?assert(maps:is_key(status, W))
                    end, Workitems)
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test loading workitems for a case with no workitems.
%% Verifies empty list is returned.
%% @end
%%--------------------------------------------------------------------
test_load_workitems_empty_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_wi_empty_003">>,

                    %% Load workitems for case with none
                    {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                    ?assertEqual([], Workitems)
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test updating a workitem.
%% Verifies workitem updates work correctly.
%% @end
%%--------------------------------------------------------------------
test_update_workitem_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_wi_update_004">>,
                    WorkitemId = <<"workitem_update_001">>,
                    Now = erlang:system_time(millisecond),
                    WorkitemMap = test_workitem_map(WorkitemId, CaseId),

                    %% Save initial workitem
                    {ok, WorkitemId} = yawl_persistence:save_workitem(WorkitemMap),

                    %% Update workitem status
                    UpdatedWorkitem = maps:put(status, started, maps:put(started_at, Now, WorkitemMap)),
                    {ok, WorkitemId} = yawl_persistence:save_workitem(UpdatedWorkitem),

                    %% Verify update
                    {ok, [LoadedWorkitem]} = yawl_persistence:load_workitems(CaseId),
                    ?assertEqual(started, maps:get(status, LoadedWorkitem)),
                    ?assertEqual(Now, maps:get(started_at, LoadedWorkitem))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test cascade delete of workitems when deleting case.
%% Verifies that deleting a case also removes its workitems.
%% @end
%%--------------------------------------------------------------------
test_delete_case_cascade_workitems_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    CaseId = <<"case_cascade_001">>,

                    %% Save a case
                    CaseMap = test_case_map(CaseId),
                    {ok, _} = yawl_persistence:save_case(CaseMap),

                    %% Save workitems for the case
                    lists:foreach(fun(N) ->
                        Bin = list_to_binary("workitem_" ++ integer_to_list(N)),
                        WorkitemMap = test_workitem_map(Bin, CaseId),
                        {ok, _} = yawl_persistence:save_workitem(WorkitemMap)
                    end, lists:seq(1, 3)),

                    %% Verify workitems exist
                    {ok, Workitems} = yawl_persistence:load_workitems(CaseId),
                    ?assertEqual(3, length(Workitems)),

                    %% Delete the case
                    ok = yawl_persistence:delete_case(CaseId),

                    %% Verify workitems are also deleted
                    {ok, WorkitemsAfter} = yawl_persistence:load_workitems(CaseId),
                    ?assertEqual([], WorkitemsAfter)
                end)]}.

%%====================================================================
%% Active Cases Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test listing active cases.
%% Verifies only running and suspended cases are returned.
%% @end
%%--------------------------------------------------------------------
test_list_active_cases_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    %% Create cases with different statuses
                    RunningCase = test_case_map_with_status(<<"active_running_001">>, running),
                    SuspendedCase = test_case_map_with_status(<<"active_suspended_001">>, suspended),
                    CompletedCase = test_case_map_with_status(<<"active_completed_001">>, completed),
                    CancelledCase = test_case_map_with_status(<<"active_cancelled_001">>, cancelled),

                    %% Save all cases
                    {ok, _} = yawl_persistence:save_case(RunningCase),
                    {ok, _} = yawl_persistence:save_case(SuspendedCase),
                    {ok, _} = yawl_persistence:save_case(CompletedCase),
                    {ok, _} = yawl_persistence:save_case(CancelledCase),

                    %% List active cases
                    {ok, ActiveCases} = yawl_persistence:list_active_cases(),

                    %% Should only include running and suspended
                    ?assertEqual(2, length(ActiveCases)),

                    Statuses = [maps:get(status, C) || C <- ActiveCases],
                    ?assert(lists:member(running, Statuses)),
                    ?assert(lists:member(suspended, Statuses)),
                    ?assertNot(lists:member(completed, Statuses)),
                    ?assertNot(lists:member(cancelled, Statuses))
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test listing active cases when none exist.
%% Verifies empty list is returned.
%% @end
%%--------------------------------------------------------------------
test_list_active_cases_empty_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    {ok, ActiveCases} = yawl_persistence:list_active_cases(),
                    ?assertEqual([], ActiveCases)
                end)]}.

%%--------------------------------------------------------------------
%% @doc Test case count function.
%% Verifies accurate case counting.
%% @end
%%--------------------------------------------------------------------
test_get_case_count_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
                    %% Initial count
                    {ok, Count0} = yawl_persistence:get_case_count(),
                    ?assertEqual(0, Count0),

                    %% Add some cases
                    lists:foreach(fun(N) ->
                        Bin = list_to_binary("count_case_" ++ integer_to_list(N)),
                        CaseMap = test_case_map(Bin),
                        {ok, _} = yawl_persistence:save_case(CaseMap)
                    end, lists:seq(1, 5)),

                    %% Check count
                    {ok, Count5} = yawl_persistence:get_case_count(),
                    ?assertEqual(5, Count5),

                    %% Delete a case
                    ok = yawl_persistence:delete_case(<<"count_case_1">>),

                    %% Check count again
                    {ok, Count4} = yawl_persistence:get_case_count(),
                    ?assertEqual(4, Count4)
                end)]}.

%%====================================================================
%% Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test full case lifecycle with workitems.
%% Verifies complete workflow persistence scenario.
%% @end
%%--------------------------------------------------------------------
test_full_lifecycle_test_() ->
    {foreach,
     fun setup_each/0,
     fun cleanup_each/1,
     [?_test(begin
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
                    FirstWIId = maps:get(workitem_id, FirstWI),
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
                end)]}.

%%====================================================================
%% Main Test Generator with Global Setup
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main test generator that sets up Mnesia once for all tests.
%% All individual tests use setup_each/cleanup_each to clear tables.
%% @end
%%--------------------------------------------------------------------
main_test_() ->
    {setup,
     fun setup_global/0,
     fun(_TestData) ->
         mnesia:stop(),
         mnesia:delete_schema([node()]),
         ok
     end,
     fun(_TestData) ->
         [
          %% Schema Tests
          test_init_schema_test_(),
          test_init_schema_idempotent_test_(),

          %% Case Persistence Tests
          test_save_case_map_test_(),
          test_load_case_test_(),
          test_load_case_not_found_test_(),
          test_update_case_test_(),
          test_delete_case_test_(),
          test_delete_case_not_found_test_(),
          test_case_statuses_test_(),

          %% Workitem Persistence Tests
          test_save_workitem_map_test_(),
          test_load_workitems_test_(),
          test_load_workitems_empty_test_(),
          test_update_workitem_test_(),
          test_delete_case_cascade_workitems_test_(),

          %% Active Cases Tests
          test_list_active_cases_test_(),
          test_list_active_cases_empty_test_(),
          test_get_case_count_test_(),

          %% Integration Tests
          test_full_lifecycle_test_()
         ]
     end}.
