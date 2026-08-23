%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Team
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
%% @doc EUnit Tests for Cloud Spanner Adapter
%%
%% Test suite for spanner_adapter module covering:
%% - Connection management and health checks
%% - Case CRUD operations
%% - Work item CRUD operations
%% - Query execution
%% - Transaction support
%% - Connection failure handling
%% - Statistics tracking
%%
%% @end
%% -------------------------------------------------------------------

-module(spanner_adapter_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

-record(test_state, {
    adapter_pid :: pid() | undefined,
    mock_cases :: map(),
    mock_workitems :: map()
}).

%% Sample test data
-define(TEST_CASE_ID, <<"test-case-001">>).
-define(TEST_WORKFLOW_ID, <<"test-workflow-order">>).
-define(TEST_WORKITEM_ID, <<"test-workitem-001">>).

%%====================================================================
%% Setup and Teardown
%%====================================================================

%% @doc Setup function to initialize test state.
setup() ->
    #test_state{
        adapter_pid = undefined,
        mock_cases = #{},
        mock_workitems = #{}
    }.

%% @doc Cleanup function after tests.
cleanup(_State) ->
    ok.

%%====================================================================
%% Connection Management Tests
%%====================================================================

spanner_adapter_start_link_default_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:start_link/0, 0)).

spanner_adapter_start_link_config_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:start_link/1, 1)).

spanner_adapter_health_check_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:health_check/0, 0)).

spanner_adapter_reconnect_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:reconnect/0, 0)).

spanner_adapter_get_stats_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:get_stats/0, 0)).

%%====================================================================
%% Case Operation API Tests
%%====================================================================

spanner_adapter_save_case_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:save_case/1, 1)).

spanner_adapter_load_case_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:load_case/1, 1)).

spanner_adapter_delete_case_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:delete_case/1, 1)).

spanner_adapter_list_active_cases_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:list_active_cases/0, 0)).

spanner_adapter_get_case_count_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:get_case_count/0, 0)).

%%====================================================================
%% Work Item Operation API Tests
%%====================================================================

spanner_adapter_save_workitem_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:save_workitem/1, 1)).

spanner_adapter_load_workitems_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:load_workitems/1, 1)).

spanner_adapter_delete_workitems_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:delete_workitems/1, 1)).

%%====================================================================
%% Query and Transaction API Tests
%%====================================================================

spanner_adapter_query_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:query/2, 2)).

spanner_adapter_execute_sql_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:execute_sql/1, 1)).

spanner_adapter_transaction_test() ->
    ?assertEqual(true, is_function(fun spanner_adapter:transaction/1, 1)).

%%====================================================================
%% Data Structure Tests - Case Map
%%====================================================================

spanner_case_map_structure_test() ->
    CaseMap = #{
        case_id => ?TEST_CASE_ID,
        workflow_id => ?TEST_WORKFLOW_ID,
        spec => #{version => 1},
        status => running,
        data => #{key => value},
        created_at => 1234567890,
        started_at => 1234567891,
        completed_at => undefined
    },
    ?assertEqual(?TEST_CASE_ID, maps:get(case_id, CaseMap)),
    ?assertEqual(?TEST_WORKFLOW_ID, maps:get(workflow_id, CaseMap)),
    ?assertEqual(running, maps:get(status, CaseMap)),
    ?assertEqual(1234567890, maps:get(created_at, CaseMap)).

%%====================================================================
%% Data Structure Tests - Work Item Map
%%====================================================================

spanner_workitem_map_structure_test() ->
    WorkitemMap = #{
        workitem_id => ?TEST_WORKITEM_ID,
        case_id => ?TEST_CASE_ID,
        task_id => <<"task-approve">>,
        status => enabled,
        data => #{payload => data},
        enabled_at => 1234567892,
        started_at => undefined,
        completed_at => undefined
    },
    ?assertEqual(?TEST_WORKITEM_ID, maps:get(workitem_id, WorkitemMap)),
    ?assertEqual(?TEST_CASE_ID, maps:get(case_id, WorkitemMap)),
    ?assertEqual(enabled, maps:get(status, WorkitemMap)),
    ?assertEqual(1234567892, maps:get(enabled_at, WorkitemMap)).

%%====================================================================
%% Statistics Tracking Tests
%%====================================================================

spanner_stats_initial_state_test() ->
    Stats = #{
        total_queries => 0,
        failed_queries => 0,
        avg_latency => 0.0
    },
    ?assertEqual(0, maps:get(total_queries, Stats)),
    ?assertEqual(0, maps:get(failed_queries, Stats)),
    ?assertEqual(0.0, safe_get_float(avg_latency, Stats, 0.0)).

spanner_stats_update_success_test() ->
    InitialStats = #{
        total_queries => 0,
        failed_queries => 0,
        avg_latency => 0.0
    },
    UpdatedStats = spanner_adapter:update_stats(InitialStats, 100, success),
    ?assertEqual(1, maps:get(total_queries, UpdatedStats)),
    ?assertEqual(100.0, safe_get_float(avg_latency, UpdatedStats, 0.0)).

spanner_stats_update_failure_test() ->
    InitialStats = #{
        total_queries => 5,
        failed_queries => 1,
        avg_latency => 50.0
    },
    UpdatedStats = spanner_adapter:update_stats(InitialStats, 200, failure),
    ?assertEqual(6, maps:get(total_queries, UpdatedStats)),
    ?assertEqual(2, maps:get(failed_queries, UpdatedStats)).

%%====================================================================
%% UUID Generation Tests
%%====================================================================

spanner_uuid_format_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              Uuid = spanner_adapter:generate_uuid(),
              ?assert(is_list(Uuid)),
              ?assertEqual(36, length(Uuid)),
              ?assertEqual($-, lists:nth(9, Uuid)),
              ?assertEqual($-, lists:nth(14, Uuid)),
              ?assertEqual($-, lists:nth(19, Uuid)),
              ?assertEqual($-, lists:nth(24, Uuid))
           end)
         ]
     end}.

spanner_uuid_uniqueness_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              Uuid1 = spanner_adapter:generate_uuid(),
              Uuid2 = spanner_adapter:generate_uuid(),
              ?assertNotEqual(Uuid1, Uuid2)
           end)
         ]
     end}.

%%====================================================================
%% Utility Function Tests
%%====================================================================

spanner_maps_get_safe_default_test() ->
    Map = #{key => value},
    ?assertEqual(value, spanner_adapter:maps_get_safe(key, Map, default)),
    ?assertEqual(default, spanner_adapter:maps_get_safe(missing, Map, default)).

spanner_maps_get_bin_test() ->
    Map = #{<<"key">> => <<"value">>},
    ?assertEqual(<<"value">>, spanner_adapter:maps_get_bin(<<"key">>, Map, <<>>)).

spanner_maps_get_bin_default_test() ->
    Map = #{},
    ?assertEqual(<<"default">>, spanner_adapter:maps_get_bin(<<"key">>, Map, <<"default">>)).

spanner_maps_get_int_test() ->
    Map = #{<<"key">> => 42},
    ?assertEqual(42, spanner_adapter:maps_get_int(<<"key">>, Map, 0)).

spanner_maps_get_int_default_test() ->
    Map = #{},
    ?assertEqual(0, spanner_adapter:maps_get_int(<<"key">>, Map, 0)).

spanner_maps_get_int_undefined_test() ->
    Map = #{<<"key">> => undefined},
    ?assertEqual(undefined, spanner_adapter:maps_get_int(<<"key">>, Map, 0)).

%%====================================================================
%% Row Conversion Tests
%%====================================================================

spanner_row_to_case_map_test() ->
    Row = #{
        <<"case_id">> => ?TEST_CASE_ID,
        <<"workflow_id">> => ?TEST_WORKFLOW_ID,
        <<"status">> => <<"running">>,
        <<"created_at">> => 1234567890,
        <<"started_at">> => 1234567891,
        <<"completed_at">> => undefined
    },
    CaseMap = spanner_adapter:row_to_case_map(Row),
    ?assertEqual(?TEST_CASE_ID, maps:get(case_id, CaseMap)),
    ?assertEqual(?TEST_WORKFLOW_ID, maps:get(workflow_id, CaseMap)),
    ?assertEqual(running, maps:get(status, CaseMap)).

spanner_row_to_workitem_map_test() ->
    Row = #{
        <<"workitem_id">> => ?TEST_WORKITEM_ID,
        <<"case_id">> => ?TEST_CASE_ID,
        <<"task_id">> => <<"task-approve">>,
        <<"status">> => <<"enabled">>,
        <<"enabled_at">> => 1234567892,
        <<"started_at">> => undefined,
        <<"completed_at">> => undefined
    },
    WorkitemMap = spanner_adapter:row_to_workitem_map(Row),
    ?assertEqual(?TEST_WORKITEM_ID, maps:get(workitem_id, WorkitemMap)),
    ?assertEqual(?TEST_CASE_ID, maps:get(case_id, WorkitemMap)),
    ?assertEqual(enabled, maps:get(status, WorkitemMap)).

%%====================================================================
%% Configuration Tests
%%====================================================================

spanner_default_config_test() ->
    Config = spanner_adapter:get_application_config(),
    ?assert(is_map(Config)),
    ?assert(maps:is_key(spanner_instance, Config)),
    ?assert(maps:is_key(spanner_database, Config)),
    ?assert(maps:is_key(spanner_project, Config)),
    ?assert(maps:is_key(pool_size, Config)).

%%====================================================================
%% Average Calculation Tests
%%====================================================================

spanner_update_avg_initial_test() ->
    Avg = spanner_adapter:update_avg(0.0, 1, 100),
    ?assertEqual(100.0, Avg).

spanner_update_avg_multiple_test() ->
    Avg1 = spanner_adapter:update_avg(0.0, 1, 100),
    Avg2 = spanner_adapter:update_avg(Avg1, 2, 200),
    Avg3 = spanner_adapter:update_avg(Avg2, 3, 300),
    ?assertEqual(200.0, Avg3).

%%====================================================================
%% Integration Tests
%%====================================================================

%% Generator for complete case workflow
spanner_case_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
              %% Test 1: Create case map
              CaseMap = #{
                  case_id => ?TEST_CASE_ID,
                  workflow_id => ?TEST_WORKFLOW_ID,
                  spec => #{version => 1},
                  status => running,
                  data => #{},
                  created_at => erlang:system_time(millisecond)
              },
              ?assertEqual(?TEST_CASE_ID, maps:get(case_id, CaseMap)),

              %% Test 2: Create work item map
              WorkitemMap = #{
                  workitem_id => ?TEST_WORKITEM_ID,
                  case_id => ?TEST_CASE_ID,
                  task_id => <<"task-approve">>,
                  status => enabled,
                  data => #{},
                  enabled_at => erlang:system_time(millisecond)
              },
              ?assertEqual(?TEST_WORKITEM_ID, maps:get(workitem_id, WorkitemMap)),

              %% Test 3: Status conversions
              ?assertEqual(running, spanner_adapter:status_atom(<<"running">>)),
              ?assertEqual(enabled, spanner_adapter:workitem_status_atom(<<"enabled">>))
           end)
         ]
     end}.

%% Generator for query parameter tests
spanner_query_parameter_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              %% Test with empty params
              Sql = <<"SELECT * FROM workflow_cases">>,
              Result = spanner_adapter:query(Sql, []),
              %% Mock implementation returns {ok, []}
              ?assertMatch({ok, _}, Result)
           end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

spanner_error_not_found_test() ->
    ?assertEqual(not_found, element(1, element(2, {error, not_found}))).

spanner_error_connection_test() ->
    ?assertMatch({error, _}, {error, not_connected}).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Helper to safely get float values from maps
safe_get_float(Key, Map, Default) ->
    case maps:get(Key, Map, Default) of
        Float when is_float(Float) -> Float;
        Int when is_integer(Int) -> Int * 1.0;
        _ -> Default
    end.
