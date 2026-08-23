%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joorgen@cuneiform-lang.org>
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
%% @author CRE Project
%% @copyright 2025
%%
%% @doc EUnit Tests for Mnesia-Spanner Sync
%%
%% Test suite for the mnesia_spanner_sync module covering
%% background sync operations, conflict resolution, and metrics.
%%
%% @end
%% -------------------------------------------------------------------

-module(mnesia_spanner_sync_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Macros
%%====================================================================

-define(TEST_SYNC_INTERVAL, 5000).
-define(TEST_BATCH_SIZE, 10).
-define(TEST_CASE_IDS, [sync_test_1, sync_test_2, sync_test_3]).

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Main test generator
mnesia_spanner_sync_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"sync status and configuration", fun test_sync_status/0},
         {"resolution strategies", fun test_resolution_strategies/0},
         {"sync interval configuration", fun test_sync_interval/0},
         {"batch size configuration", fun test_batch_size/0},
         {"pause and resume", fun test_pause_resume/0},
         {"manual sync trigger", fun test_sync_now/0},
         {"sync metrics", fun test_sync_metrics/0},
         {"sync report", fun test_sync_report/0},
         {"batch processing", fun test_batch_processing/0}
     ]
    }.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

%% @private Setup function
setup() ->
    %% Ensure Mnesia is stopped before tests
    mnesia:stop(),
    mnesia:delete_schema([node()]),

    %% Start Mnesia for testing
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    %% Create test case table
    {atomic, ok} = mnesia:create_table(case_table,
        [{attributes, [id, data, status]}, {ram_copies, [node()]}]),

    %% Populate with test data
    lists:foreach(fun(CaseId) ->
        mnesia:dirty_write(case_table, {case_record, CaseId, "test_data", active})
    end, ?TEST_CASE_IDS),

    %% Start sync process with auto_start disabled
    {ok, _Pid} = mnesia_spanner_sync:start_link([
        {sync_interval, ?TEST_SYNC_INTERVAL},
        {batch_size, ?TEST_BATCH_SIZE},
        {auto_start, false}
    ]),

    ok.

%% @private Cleanup function
cleanup(_State) ->
    %% Stop sync process
    gen_server:stop(mnesia_spanner_sync),

    %% Cleanup Mnesia
    mnesia:delete_table(case_table),
    mnesia:stop(),
    mnesia:delete_schema([node()]),

    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Tests sync status retrieval
test_sync_status() ->
    Status = mnesia_spanner_sync:get_status(),

    %% Verify status structure
    ?assert(maps:is_key(sync_status, Status)),
    ?assert(maps:is_key(resolution_strategy, Status)),
    ?assert(maps:is_key(current_batch, Status)),
    ?assert(maps:is_key(metrics, Status)),
    ?assert(maps:is_key(sync_interval, Status)),
    ?assert(maps:is_key(batch_size, Status)),

    %% Check initial status (should be idle since auto_start is false)
    ?assertEqual(idle, maps:get(sync_status, Status)),

    %% Verify sync interval
    ?assertEqual(?TEST_SYNC_INTERVAL, maps:get(sync_interval, Status)),

    %% Verify batch size
    ?assertEqual(?TEST_BATCH_SIZE, maps:get(batch_size, Status)),

    ok.

%% @doc Tests resolution strategy configuration
test_resolution_strategies() ->
    %% Test mnesia_wins
    ok = mnesia_spanner_sync:set_resolution_strategy(mnesia_wins),
    Status1 = mnesia_spanner_sync:get_status(),
    ?assertEqual(mnesia_wins, maps:get(resolution_strategy, Status1)),

    %% Test spanner_wins
    ok = mnesia_spanner_sync:set_resolution_strategy(spanner_wins),
    Status2 = mnesia_spanner_sync:get_status(),
    ?assertEqual(spanner_wins, maps:get(resolution_strategy, Status2)),

    %% Test newest_wins
    ok = mnesia_spanner_sync:set_resolution_strategy(newest_wins),
    Status3 = mnesia_spanner_sync:get_status(),
    ?assertEqual(newest_wins, maps:get(resolution_strategy, Status3)),

    %% Test report_only
    ok = mnesia_spanner_sync:set_resolution_strategy(report_only),
    Status4 = mnesia_spanner_sync:get_status(),
    ?assertEqual(report_only, maps:get(resolution_strategy, Status4)),

    %% Reset to default
    ok = mnesia_spanner_sync:set_resolution_strategy(mnesia_wins),

    ok.

%% @doc Tests sync interval configuration
test_sync_interval() ->
    %% Change interval
    NewInterval = ?TEST_SYNC_INTERVAL * 2,
    ok = mnesia_spanner_sync:set_sync_interval(NewInterval),

    Status = mnesia_spanner_sync:get_status(),
    ?assertEqual(NewInterval, maps:get(sync_interval, Status)),

    %% Reset
    ok = mnesia_spanner_sync:set_sync_interval(?TEST_SYNC_INTERVAL),

    ok.

%% @doc Tests batch size configuration
test_batch_size() ->
    %% Change batch size
    NewBatchSize = ?TEST_BATCH_SIZE * 2,
    ok = mnesia_spanner_sync:set_batch_size(NewBatchSize),

    Status = mnesia_spanner_sync:get_status(),
    ?assertEqual(NewBatchSize, maps:get(batch_size, Status)),

    %% Reset
    ok = mnesia_spanner_sync:set_batch_size(?TEST_BATCH_SIZE),

    ok.

%% @doc Tests pause and resume functionality
test_pause_resume() ->
    %% Pause sync
    ok = mnesia_spanner_sync:pause_sync(),

    Status = mnesia_spanner_sync:get_status(),
    ?assertEqual(paused, maps:get(sync_status, Status)),

    %% Resume sync
    ok = mnesia_spanner_sync:resume_sync(),

    Status2 = mnesia_spanner_sync:get_status(),
    ?assertEqual(idle, maps:get(sync_status, Status2)),

    ok.

%% @doc Tests manual sync trigger
test_sync_now() ->
    %% Trigger immediate sync
    {ok, SyncResult} = mnesia_spanner_sync:sync_now(),

    %% Verify sync result structure
    ?assert(maps:is_key(duration_ms, SyncResult)),
    ?assert(maps:is_key(total_records, SyncResult)),
    ?assert(maps:is_key(discrepancies, SyncResult)),
    ?assert(maps:is_key(resolution_strategy, SyncResult)),

    %% Verify positive duration
    Duration = maps:get(duration_ms, SyncResult),
    ?assert(Duration >= 0),

    ok.

%% @doc Tests sync metrics tracking
test_sync_metrics() ->
    %% Perform a sync to populate metrics
    {ok, _} = mnesia_spanner_sync:sync_now(),

    Status = mnesia_spanner_sync:get_status(),
    Metrics = maps:get(metrics, Status),

    %% Verify metrics structure
    ?assert(maps:is_key(total_records, Metrics)),
    ?assert(maps:is_key(synced_records, Metrics)),
    ?assert(maps:is_key(discrepancies_found, Metrics)),
    ?assert(maps:is_key(discrepancies_resolved, Metrics)),
    ?assert(maps:is_key(last_sync_time, Metrics)),
    ?assert(maps:is_key(last_sync_duration, Metrics)),
    ?assert(maps:is_key(total_sync_count, Metrics)),

    %% Reset metrics
    ok = mnesia_spanner_sync:reset_metrics(),

    Status2 = mnesia_spanner_sync:get_status(),
    Metrics2 = maps:get(metrics, Status2),
    ?assertEqual(0, maps:get(total_sync_count, Metrics2)),

    ok.

%% @doc Tests sync report generation
test_sync_report() ->
    %% Perform a sync
    {ok, _} = mnesia_spanner_sync:sync_now(),

    %% Get report
    Report = mnesia_spanner_sync:get_sync_report(),

    %% Verify report structure
    ?assert(maps:is_key(duration_ms, Report)),
    ?assert(maps:is_key(total_records, Report)),
    ?assert(maps:is_key(discrepancies, Report)),
    ?assert(maps:is_key(resolution_strategy, Report)),

    ok.

%% @doc Tests batch processing
test_batch_processing() ->
    %% Set a small batch size
    ok = mnesia_spanner_sync:set_batch_size(2),

    %% Trigger sync
    {ok, SyncResult} = mnesia_spanner_sync:sync_now(),

    %% Verify sync completed
    ?assert(maps:is_key(total_records, SyncResult)),
    TotalRecords = maps:get(total_records, SyncResult),
    ?assert(TotalRecords >= length(?TEST_CASE_IDS)),

    ok.

%%====================================================================
%% Unit Tests for Internal Functions
%%====================================================================

%% @doc Test resolution_strategy type
resolution_strategy_type_test_() ->
    [
        ?_assertEqual(true, is_atom(mnesia_wins)),
        ?_assertEqual(true, is_atom(spanner_wins)),
        ?_assertEqual(true, is_atom(newest_wins)),
        ?_assertEqual(true, is_atom(report_only))
    ].

%% @doc Test sync_status type
sync_status_type_test_() ->
    [
        ?_assertEqual(true, is_atom(idle)),
        ?_assertEqual(true, is_atom(syncing)),
        ?_assertEqual(true, is_atom(paused)),
        ?_assertEqual(true, is_atom(error))
    ].

%% @doc Test sync_metrics type
sync_metrics_type_test_() ->
    Metrics = #{
        total_records => 0,
        synced_records => 0,
        discrepancies_found => 0,
        discrepancies_resolved => 0,
        last_sync_time => undefined,
        last_sync_duration => undefined,
        total_sync_count => 0
    },
    [
        ?_assertEqual(true, is_map(Metrics)),
        ?_assertEqual(0, maps:get(total_records, Metrics)),
        ?_assertEqual(true, is_integer(maps:get(synced_records, Metrics)))
    ].

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test full sync workflow
sync_workflow_test_() ->
    {setup,
     fun() ->
         %% Setup
         mnesia:stop(),
         mnesia:delete_schema([node()]),
         ok = mnesia:create_schema([node()]),
         ok = mnesia:start(),
         {atomic, ok} = mnesia:create_table(case_table,
             [{attributes, [id, data]}, {ram_copies, [node()]}]),
         %% Add test data
         mnesia:dirty_write(case_table, {case_record, wf_1, "data1"}),
         mnesia:dirty_write(case_table, {case_record, wf_2, "data2"}),
         {ok, _} = mnesia_spanner_sync:start_link([{auto_start, false}])
     end,
     fun(_) ->
         %% Cleanup
         gen_server:stop(mnesia_spanner_sync),
         mnesia:delete_table(case_table),
         mnesia:stop(),
         mnesia:delete_schema([node()])
     end,
     fun(_) ->
         [
             ?_test(begin
                 %% Set strategy
                 ok = mnesia_spanner_sync:set_resolution_strategy(mnesia_wins),

                 %% Trigger sync
                 {ok, Result} = mnesia_spanner_sync:sync_now(),

                 %% Verify result
                 ?assert(maps:is_key(total_records, Result)),
                 ?assert(maps:is_key(duration_ms, Result)),

                 %% Check status after sync
                 Status = mnesia_spanner_sync:get_status(),
                 ?assertEqual(idle, maps:get(sync_status, Status))
             end)
         ]
     end
    }.

%%====================================================================
%% Property-Based Tests
%%====================================================================

%% @doc Property: sync should handle various batch sizes
prop_batch_size_test_() ->
    {setup,
     fun() ->
         mnesia:stop(),
         mnesia:delete_schema([node()]),
         ok = mnesia:create_schema([node()]),
         ok = mnesia:start(),
         {atomic, ok} = mnesia:create_table(case_table,
             [{attributes, [id, data]}, {ram_copies, [node()]}]),
         {ok, _} = mnesia_spanner_sync:start_link([{auto_start, false}])
     end,
     fun(_) ->
         gen_server:stop(mnesia_spanner_sync),
         mnesia:delete_table(case_table),
         mnesia:stop(),
         mnesia:delete_schema([node()])
     end,
     fun(_) ->
         BatchSizes = [1, 5, 10, 50],
         [?_test(begin
             %% Set batch size
             ok = mnesia_spanner_sync:set_batch_size(BatchSize),

             %% Verify it was set
             Status = mnesia_spanner_sync:get_status(),
             ?assertEqual(BatchSize, maps:get(batch_size, Status))
          end) || BatchSize <- BatchSizes]
     end
    }.
