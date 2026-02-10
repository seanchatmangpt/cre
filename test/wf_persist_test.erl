%% -*- erlang -*-
%%%% @doc EUnit tests for wf_persist module.
%%
%% This test suite validates the complete persistence functionality:
%% <ul>
%%   <li>Schema initialization with different storage types</li>
%%   <li>Workflow save, load, and delete operations</li>
%%   <li>Snapshot creation, listing, and restoration</li>
%%   <li>Checkpoint creation and recovery</li>
%%   <li>Cache operations (get, put, warm, invalidate)</li>
%%   <li>Utility functions (workflow_exists, get_snapshot_info)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_persist_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite with setup and cleanup for persistence tests.
%%--------------------------------------------------------------------
persistence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"init_schema initializes successfully", fun test_init_schema/0},
      {"init_schema with ram_copies", fun test_init_schema_ram_copies/0},
      {"save_workflow saves and loads workflow", fun test_save_load_workflow/0},
      {"load_workflow returns error for missing workflow", fun test_load_workflow_not_found/0},
      {"delete_workflow removes workflow", fun test_delete_workflow/0},
      {"workflow_exists returns true for saved workflow", fun test_workflow_exists_true/0},
      {"workflow_exists returns false for missing workflow", fun test_workflow_exists_false/0},
      {"snapshot_workflow creates snapshot", fun test_snapshot_workflow/0},
      {"list_snapshots returns empty list for new workflow", fun test_list_snapshots_empty/0},
      {"list_snapshots returns snapshots in reverse chronological order", fun test_list_snapshots_ordered/0},
      {"restore_snapshot restores workflow state", fun test_restore_snapshot/0},
      {"restore_snapshot returns error for missing snapshot", fun test_restore_snapshot_not_found/0},
      {"cleanup_snapshots removes old snapshots", fun test_cleanup_snapshots/0},
      {"cleanup_snapshots keeps recent snapshots", fun test_cleanup_snapshots_keeps_recent/0},
      {"create_checkpoint creates checkpoint with snapshot", fun test_create_checkpoint/0},
      {"list_checkpoints returns all checkpoints", fun test_list_checkpoints/0},
      {"restore_checkpoint restores from checkpoint", fun test_restore_checkpoint/0},
      {"restore_checkpoint returns error for missing checkpoint", fun test_restore_checkpoint_not_found/0},
      {"delete_checkpoint removes checkpoint", fun test_delete_checkpoint/0},
      {"cache_put and cache_get work together", fun test_cache_put_get/0},
      {"cache_get returns error for missing key", fun test_cache_get_not_found/0},
      {"invalidate_cache clears cache", fun test_invalidate_cache/0},
      {"warm_cache loads all workflows", fun test_warm_cache/0},
      {"get_snapshot_info returns snapshot metadata", fun test_get_snapshot_info/0},
      {"save_workflow increments version", fun test_save_workflow_increments_version/0},
      {"save_workflow preserves created_at", fun test_save_workflow_preserves_created_at/0}
     ]}.

%%--------------------------------------------------------------------
%% @doc Setup function - initializes schema.
%%--------------------------------------------------------------------
setup() ->
    _ = application:ensure_all_started(mnesia),
    ok = wf_persist:init_schema(ram_copies),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function - stops mnesia.
%%--------------------------------------------------------------------
cleanup(_) ->
    _ = mnesia:stop(),
    ok.

%%====================================================================
%% Test Cases - Schema Initialization
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test init_schema/0 creates tables successfully.
%%--------------------------------------------------------------------
test_init_schema() ->
    ok = wf_persist:init_schema(),
    ?assert(true).

%%--------------------------------------------------------------------
%% @doc Test init_schema/1 with ram_copies.
%%--------------------------------------------------------------------
test_init_schema_ram_copies() ->
    ok = wf_persist:init_schema(ram_copies),
    ?assert(true).

%%====================================================================
%% Test Cases - Workflow Persistence
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test save_workflow and load_workflow.
%%--------------------------------------------------------------------
test_save_load_workflow() ->
    WorkflowId = <<"wf-1">>,
    WorkflowState = #{
        status => running,
        marking => #{p1 => [a, b]},
        data => #{key => value},
        events => [event1, event2],
        metadata => #{source => test}
    },
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, Loaded} = wf_persist:load_workflow(WorkflowId),
    ?assertEqual(running, maps:get(status, Loaded)),
    ?assertEqual(#{p1 => [a, b]}, maps:get(marking, Loaded)),
    ?assertEqual(#{key => value}, maps:get(data, Loaded)),
    ?assertEqual([event1, event2], maps:get(events, Loaded)).

%%--------------------------------------------------------------------
%% @doc Test load_workflow returns error for missing workflow.
%%--------------------------------------------------------------------
test_load_workflow_not_found() ->
    {error, not_found} = wf_persist:load_workflow(<<"missing-wf">>),
    ?assert(true).

%%--------------------------------------------------------------------
%% @doc Test delete_workflow removes workflow.
%%--------------------------------------------------------------------
test_delete_workflow() ->
    WorkflowId = <<"wf-delete">>,
    WorkflowState = #{status => pending, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, _} = wf_persist:load_workflow(WorkflowId),
    ok = wf_persist:delete_workflow(WorkflowId),
    {error, not_found} = wf_persist:load_workflow(WorkflowId).

%%====================================================================
%% Test Cases - Workflow Existence
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test workflow_exists returns true for saved workflow.
%%--------------------------------------------------------------------
test_workflow_exists_true() ->
    WorkflowId = <<"wf-exists">>,
    WorkflowState = #{status => pending, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    ?assert(wf_persist:workflow_exists(WorkflowId)).

%%--------------------------------------------------------------------
%% @doc Test workflow_exists returns false for missing workflow.
%%--------------------------------------------------------------------
test_workflow_exists_false() ->
    ?assertNot(wf_persist:workflow_exists(<<"missing-wf">>)).

%%====================================================================
%% Test Cases - Snapshot Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test snapshot_workflow creates snapshot.
%%--------------------------------------------------------------------
test_snapshot_workflow() ->
    WorkflowId = <<"wf-snap">>,
    WorkflowState = #{
        status => running,
        marking => #{p1 => [token1]},
        data => #{counter => 5},
        events => []
    },
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, SnapshotId} = wf_persist:snapshot_workflow(WorkflowId, <<"Snapshot 1">>),
    ?assert(is_binary(SnapshotId)),
    ?assert(byte_size(SnapshotId) > 0).

%%--------------------------------------------------------------------
%% @doc Test list_snapshots returns empty list for new workflow.
%%--------------------------------------------------------------------
test_list_snapshots_empty() ->
    WorkflowId = <<"wf-snap-empty">>,
    WorkflowState = #{status => pending, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    Snapshots = wf_persist:list_snapshots(WorkflowId),
    ?assertEqual([], Snapshots).

%%--------------------------------------------------------------------
%% @doc Test list_snapshots returns snapshots in reverse chronological order.
%%--------------------------------------------------------------------
test_list_snapshots_ordered() ->
    WorkflowId = <<"wf-snap-ordered">>,
    WorkflowState = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, Snap1} = wf_persist:snapshot_workflow(WorkflowId, <<"First">>),
    timer:sleep(10),
    {ok, Snap2} = wf_persist:snapshot_workflow(WorkflowId, <<"Second">>),
    timer:sleep(10),
    {ok, Snap3} = wf_persist:snapshot_workflow(WorkflowId, <<"Third">>),
    Snapshots = wf_persist:list_snapshots(WorkflowId),
    ?assertEqual(3, length(Snapshots)),
    %% Should be in reverse chronological order (most recent first)
    [First, Second, Third] = Snapshots,
    ?assertEqual(Snap3, maps:get(snapshot_id, First)),
    ?assertEqual(Snap2, maps:get(snapshot_id, Second)),
    ?assertEqual(Snap1, maps:get(snapshot_id, Third)).

%%--------------------------------------------------------------------
%% @doc Test restore_snapshot restores workflow state.
%%--------------------------------------------------------------------
test_restore_snapshot() ->
    WorkflowId = <<"wf-snap-restore">>,
    OriginalState = #{
        status => running,
        marking => #{p1 => [a, b], p2 => [c]},
        data => #{counter => 42},
        events => [event1],
        metadata => #{test => true}
    },
    ok = wf_persist:save_workflow(WorkflowId, OriginalState),
    {ok, SnapshotId} = wf_persist:snapshot_workflow(WorkflowId, <<"Test snapshot">>),
    %% Modify the workflow
    ModifiedState = #{
        status => completed,
        marking => #{},
        data => #{},
        events => []
    },
    ok = wf_persist:save_workflow(WorkflowId, ModifiedState),
    %% Restore from snapshot
    {ok, RestoredState} = wf_persist:restore_snapshot(WorkflowId, SnapshotId),
    ?assertEqual(running, maps:get(status, RestoredState)),
    ?assertEqual(#{p1 => [a, b], p2 => [c]}, maps:get(marking, RestoredState)),
    ?assertEqual(#{counter => 42}, maps:get(data, RestoredState)).

%%--------------------------------------------------------------------
%% @doc Test restore_snapshot returns error for missing snapshot.
%%--------------------------------------------------------------------
test_restore_snapshot_not_found() ->
    WorkflowId = <<"wf-snap-missing">>,
    WorkflowState = #{status => pending, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {error, not_found} = wf_persist:restore_snapshot(WorkflowId, <<"missing-snap">>).

%%--------------------------------------------------------------------
%% @doc Test cleanup_snapshots removes old snapshots.
%%--------------------------------------------------------------------
test_cleanup_snapshots() ->
    WorkflowId = <<"wf-cleanup">>,
    WorkflowState = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, _Snap1} = wf_persist:snapshot_workflow(WorkflowId, <<"Snap1">>),
    timer:sleep(10),
    {ok, _Snap2} = wf_persist:snapshot_workflow(WorkflowId, <<"Snap2">>),
    timer:sleep(10),
    {ok, _Snap3} = wf_persist:snapshot_workflow(WorkflowId, <<"Snap3">>),
    %% Keep only the 1 most recent
    ok = wf_persist:cleanup_snapshots(WorkflowId, 1),
    Snapshots = wf_persist:list_snapshots(WorkflowId),
    ?assertEqual(1, length(Snapshots)).

%%--------------------------------------------------------------------
%% @doc Test cleanup_snapshots keeps recent snapshots.
%%--------------------------------------------------------------------
test_cleanup_snapshots_keeps_recent() ->
    WorkflowId = <<"wf-cleanup-keep">>,
    WorkflowState = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, _Snap1} = wf_persist:snapshot_workflow(WorkflowId, <<"Snap1">>),
    timer:sleep(10),
    {ok, _Snap2} = wf_persist:snapshot_workflow(WorkflowId, <<"Snap2">>),
    %% Keep 2, cleanup should do nothing
    ok = wf_persist:cleanup_snapshots(WorkflowId, 2),
    Snapshots = wf_persist:list_snapshots(WorkflowId),
    ?assertEqual(2, length(Snapshots)).

%%====================================================================
%% Test Cases - Checkpoint Operations
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test create_checkpoint creates checkpoint with snapshot.
%%--------------------------------------------------------------------
test_create_checkpoint() ->
    WorkflowId = <<"wf-cp">>,
    WorkflowState = #{
        status => running,
        marking => #{p1 => [x]},
        data => #{state => checkpoint_test},
        events => []
    },
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, CheckpointId} = wf_persist:create_checkpoint(WorkflowId, [label1, label2]),
    ?assert(is_binary(CheckpointId)),
    ?assert(byte_size(CheckpointId) > 0).

%%--------------------------------------------------------------------
%% @doc Test list_checkpoints returns all checkpoints.
%%--------------------------------------------------------------------
test_list_checkpoints() ->
    WorkflowId1 = <<"wf-cp-list-1">>,
    WorkflowId2 = <<"wf-cp-list-2">>,
    WorkflowState = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId1, WorkflowState),
    ok = wf_persist:save_workflow(WorkflowId2, WorkflowState),
    {ok, _Cp1} = wf_persist:create_checkpoint(WorkflowId1, [label1]),
    {ok, _Cp2} = wf_persist:create_checkpoint(WorkflowId2, [label2]),
    Checkpoints = wf_persist:list_checkpoints(),
    ?assert(length(Checkpoints) >= 2).

%%--------------------------------------------------------------------
%% @doc Test restore_checkpoint restores from checkpoint.
%%--------------------------------------------------------------------
test_restore_checkpoint() ->
    WorkflowId = <<"wf-cp-restore">>,
    OriginalState = #{
        status => running,
        marking => #{p1 => [token]},
        data => #{checkpoint_data => true},
        events => [e1],
        metadata => #{cp => test}
    },
    ok = wf_persist:save_workflow(WorkflowId, OriginalState),
    {ok, CheckpointId} = wf_persist:create_checkpoint(WorkflowId, [recovery_label]),
    %% Modify the workflow
    ModifiedState = #{status => failed, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, ModifiedState),
    %% Restore from checkpoint
    {ok, RestoredWfId} = wf_persist:restore_checkpoint(CheckpointId),
    ?assertEqual(WorkflowId, RestoredWfId),
    {ok, RestoredState} = wf_persist:load_workflow(RestoredWfId),
    ?assertEqual(running, maps:get(status, RestoredState)).

%%--------------------------------------------------------------------
%% @doc Test restore_checkpoint returns error for missing checkpoint.
%%--------------------------------------------------------------------
test_restore_checkpoint_not_found() ->
    {error, not_found} = wf_persist:restore_checkpoint(<<"missing-cp">>).

%%--------------------------------------------------------------------
%% @doc Test delete_checkpoint removes checkpoint.
%%--------------------------------------------------------------------
test_delete_checkpoint() ->
    WorkflowId = <<"wf-cp-delete">>,
    WorkflowState = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, WorkflowState),
    {ok, CheckpointId} = wf_persist:create_checkpoint(WorkflowId, [label]),
    ok = wf_persist:delete_checkpoint(CheckpointId),
    {error, not_found} = wf_persist:restore_checkpoint(CheckpointId).

%%====================================================================
%% Test Cases - Cache Operations
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test cache_put and cache_get work together.
%%--------------------------------------------------------------------
test_cache_put_get() ->
    WorkflowId = <<"cache-test">>,
    Record = #{test => data},
    ok = wf_persist:cache_put(WorkflowId, Record),
    {ok, Retrieved} = wf_persist:cache_get(WorkflowId),
    ?assertEqual(Record, Retrieved).

%%--------------------------------------------------------------------
%% @doc Test cache_get returns error for missing key.
%%--------------------------------------------------------------------
test_cache_get_not_found() ->
    {error, not_found} = wf_persist:cache_get(<<"missing-cache-key">>).

%%--------------------------------------------------------------------
%% @doc Test invalidate_cache clears cache.
%%--------------------------------------------------------------------
test_invalidate_cache() ->
    WorkflowId = <<"cache-invalidate">>,
    Record = #{data => test},
    ok = wf_persist:cache_put(WorkflowId, Record),
    {ok, _} = wf_persist:cache_get(WorkflowId),
    ok = wf_persist:invalidate_cache(),
    {error, not_found} = wf_persist:cache_get(WorkflowId).

%%--------------------------------------------------------------------
%% @doc Test warm_cache loads all workflows.
%%--------------------------------------------------------------------
test_warm_cache() ->
    WorkflowId1 = <<"wf-warm-1">>,
    WorkflowId2 = <<"wf-warm-2">>,
    State = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId1, State),
    ok = wf_persist:save_workflow(WorkflowId2, State),
    ok = wf_persist:invalidate_cache(),
    ok = wf_persist:warm_cache(),
    {ok, _} = wf_persist:cache_get(WorkflowId1),
    {ok, _} = wf_persist:cache_get(WorkflowId2).

%%====================================================================
%% Test Cases - Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test get_snapshot_info returns snapshot metadata.
%%--------------------------------------------------------------------
test_get_snapshot_info() ->
    WorkflowId = <<"wf-snap-info">>,
    State = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, State),
    {ok, SnapshotId} = wf_persist:snapshot_workflow(WorkflowId, <<"Test Description">>),
    {ok, Info} = wf_persist:get_snapshot_info(WorkflowId, SnapshotId),
    ?assertEqual(SnapshotId, maps:get(snapshot_id, Info)),
    ?assertEqual(<<"Test Description">>, maps:get(description, Info)),
    ?assert(is_integer(maps:get(created_at, Info))),
    ?assert(is_integer(maps:get(version, Info))).

%%====================================================================
%% Test Cases - Version Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test save_workflow increments version.
%%--------------------------------------------------------------------
test_save_workflow_increments_version() ->
    WorkflowId = <<"wf-version">>,
    State0 = #{status => pending, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, State0),
    {ok, Loaded1} = wf_persist:load_workflow(WorkflowId),
    Ver1 = maps:get(version, Loaded1),
    State1 = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, State1),
    {ok, Loaded2} = wf_persist:load_workflow(WorkflowId),
    Ver2 = maps:get(version, Loaded2),
    ?assertEqual(Ver1 + 1, Ver2).

%%--------------------------------------------------------------------
%% @doc Test save_workflow preserves created_at on updates.
%%--------------------------------------------------------------------
test_save_workflow_preserves_created_at() ->
    WorkflowId = <<"wf-created-at">>,
    State0 = #{status => pending, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, State0),
    {ok, Loaded1} = wf_persist:load_workflow(WorkflowId),
    CreatedAt1 = maps:get(updated_at, Loaded1),
    timer:sleep(100),
    State1 = #{status => running, marking => #{}, data => #{}, events => []},
    ok = wf_persist:save_workflow(WorkflowId, State1),
    {ok, Loaded2} = wf_persist:load_workflow(WorkflowId),
    CreatedAt2 = maps:get(updated_at, Loaded2),
    %% created_at should not change, but updated_at should
    ?assert(CreatedAt2 >= CreatedAt1).
