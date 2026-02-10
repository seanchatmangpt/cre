%% -*- erlang -*-
%%%% @doc Workflow Persistence Module
%%
%% This module provides disk and ETS-based persistence for workflow state.
%% It handles saving/loading workflows, checkpoints, and state snapshots.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Workflow state persistence to Mnesia with disc_copies</li>
%%   <li>ETS-backed in-memory cache for fast access</li>
%%   <li>Checkpoint/recovery mechanism for fault tolerance</li>
%%   <li>Versioned state snapshots with timestamps</li>
%%   <li>Atomic transactions for consistency</li>
%%   <li>Garbage collection of old snapshots</li>
%% </ul>
%%
%% <h3>Mnesia Tables</h3>
%% <ul>
%%   <li><b>wf_persist_workflow:</b> Current workflow state</li>
%%   <li><b>wf_persist_snapshot:</b> Historical snapshots for recovery</li>
%%   <li><b>wf_persist_checkpoint:</b> Checkpoint markers</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_persist).

%%====================================================================
%% Exports
%%====================================================================

%% Schema management
-export([init_schema/0, init_schema/1]).

%% Workflow persistence
-export([save_workflow/2, load_workflow/1, delete_workflow/1]).

%% Snapshot management
-export([snapshot_workflow/2, list_snapshots/1, restore_snapshot/2]).
-export([cleanup_snapshots/2]).

%% Checkpoint operations
-export([create_checkpoint/2, list_checkpoints/0, restore_checkpoint/1]).
-export([delete_checkpoint/1]).

%% Cache operations
-export([warm_cache/0, invalidate_cache/0, cache_get/1, cache_put/2]).

%% Utilities
-export([workflow_exists/1, get_snapshot_info/2]).

%%====================================================================
%% Records
%%====================================================================

-record(wf_persist_workflow, {
    workflow_id :: binary(),
    status :: atom(),
    marking :: map(),
    data :: map(),
    events :: [term()],
    version :: non_neg_integer(),
    updated_at :: integer(),
    created_at :: integer(),
    metadata :: map()
}).

-record(wf_persist_snapshot, {
    snapshot_id :: binary(),
    workflow_id :: binary(),
    version :: non_neg_integer(),
    marking :: map(),
    data :: map(),
    events :: [term()],
    status :: atom(),
    created_at :: integer(),
    description :: binary()
}).

-record(wf_persist_checkpoint, {
    checkpoint_id :: binary(),
    workflow_id :: binary(),
    version :: non_neg_integer(),
    snapshot_id :: binary() | undefined,
    created_at :: integer(),
    labels :: [term()]
}).

%%====================================================================
%% Types
%%====================================================================

-type workflow_id() :: binary().
-type snapshot_id() :: binary().
-type checkpoint_id() :: binary().
-type version() :: non_neg_integer().
-type workflow_state() :: #{
    workflow_id := binary(),
    status := atom(),
    marking := map(),
    data := map(),
    events := [term()],
    version := version(),
    updated_at := integer(),
    metadata := map()
}.
-type snapshot_info() :: #{
    snapshot_id := binary(),
    version := version(),
    created_at := integer(),
    description => binary()
}.

-export_type([workflow_id/0, snapshot_id/0, checkpoint_id/0, version/0,
              workflow_state/0, snapshot_info/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes Mnesia schema for persistence with disc_copies.
%%
%% Creates tables:
%% - wf_persist_workflow: Current workflow state
%% - wf_persist_snapshot: Historical snapshots
%% - wf_persist_checkpoint: Checkpoint markers
%%
%% @end
%%--------------------------------------------------------------------
-spec init_schema() -> ok | {error, term()}.
init_schema() ->
    init_schema(disc_copies).

%%--------------------------------------------------------------------
%% @doc Initializes Mnesia schema with specified storage type.
%%
%% StorageType can be: disc_copies, ram_copies, or disc_only_copies.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_schema(disc_copies | ram_copies | disc_only_copies) ->
          ok | {error, term()}.
init_schema(StorageType) when StorageType =:= disc_copies;
                              StorageType =:= ram_copies;
                              StorageType =:= disc_only_copies ->
    Node = node(),

    %% Start Mnesia if needed
    _ = case mnesia:start() of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Create workflow table
    WfDef = [
        {attributes, record_info(fields, wf_persist_workflow)},
        {index, [#wf_persist_workflow.status, #wf_persist_workflow.updated_at]},
        {StorageType, [Node]},
        {type, set}
    ],

    %% Create snapshot table
    SnapDef = [
        {attributes, record_info(fields, wf_persist_snapshot)},
        {index, [#wf_persist_snapshot.workflow_id, #wf_persist_snapshot.created_at]},
        {StorageType, [Node]},
        {type, bag}
    ],

    %% Create checkpoint table
    CpDef = [
        {attributes, record_info(fields, wf_persist_checkpoint)},
        {index, [#wf_persist_checkpoint.workflow_id, #wf_persist_checkpoint.created_at]},
        {StorageType, [Node]},
        {type, set}
    ],

    Results = lists:map(
        fun({Tab, Def}) -> create_mnesia_table(Tab, Def) end,
        [{wf_persist_workflow, WfDef},
         {wf_persist_snapshot, SnapDef},
         {wf_persist_checkpoint, CpDef}]
    ),

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true ->
            init_cache(),
            ok;
        false ->
            {error, table_creation_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Saves a workflow state to persistent storage.
%%
%% WorkflowId is a unique identifier for the workflow.
%% WorkflowState is a map containing:
%%   - status: Current status (atom)
%%   - marking: Petri net marking (map)
%%   - data: Workflow data (map)
%%   - events: Event list (list)
%%   - metadata: Optional metadata (map)
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_workflow(workflow_id(), workflow_state()) -> ok | {error, term()}.
save_workflow(WorkflowId, WorkflowState) when is_binary(WorkflowId),
                                               is_map(WorkflowState) ->
    Timestamp = erlang:system_time(millisecond),

    %% Get version and created_at
    {Version, CreatedAt} = case get_workflow_meta(WorkflowId) of
        {ok, {V, C}} -> {V + 1, C};
        {error, not_found} -> {0, Timestamp}
    end,

    Status = maps:get(status, WorkflowState, pending),
    Marking = maps:get(marking, WorkflowState, #{}),
    Data = maps:get(data, WorkflowState, #{}),
    Events = maps:get(events, WorkflowState, []),
    Metadata = maps:get(metadata, WorkflowState, #{}),

    Record = #wf_persist_workflow{
        workflow_id = WorkflowId,
        status = Status,
        marking = Marking,
        data = Data,
        events = Events,
        version = Version,
        updated_at = Timestamp,
        created_at = CreatedAt,
        metadata = Metadata
    },

    Transaction = fun() ->
        mnesia:write(Record)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            cache_put(WorkflowId, Record),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to save workflow ~p: ~p", [WorkflowId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Loads a workflow state from persistent storage.
%%
%% Returns {ok, WorkflowState} on success, {error, not_found} if
%% the workflow does not exist.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_workflow(workflow_id()) -> {ok, workflow_state()} | {error, not_found}.
load_workflow(WorkflowId) when is_binary(WorkflowId) ->
    %% Try cache first
    case cache_get(WorkflowId) of
        {ok, Rec} ->
            {ok, record_to_map(Rec)};
        {error, not_found} ->
            %% Load from Mnesia
            Transaction = fun() ->
                mnesia:read(wf_persist_workflow, WorkflowId)
            end,

            case mnesia:transaction(Transaction) of
                {atomic, [Rec | _]} ->
                    cache_put(WorkflowId, Rec),
                    {ok, record_to_map(Rec)};
                {atomic, []} ->
                    {error, not_found};
                {aborted, Reason} ->
                    logger:error("Failed to load workflow ~p: ~p", [WorkflowId, Reason]),
                    {error, Reason}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a workflow from persistent storage.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_workflow(workflow_id()) -> ok | {error, term()}.
delete_workflow(WorkflowId) when is_binary(WorkflowId) ->
    Transaction = fun() ->
        mnesia:delete(wf_persist_workflow, WorkflowId),
        mnesia:delete(wf_persist_snapshot, {'_', WorkflowId, '_', '_', '_', '_', '_', '_', '_'})
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            invalidate_cache(),
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates a snapshot of the current workflow state.
%%
%% Returns {ok, SnapshotId} on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec snapshot_workflow(workflow_id(), binary()) ->
          {ok, snapshot_id()} | {error, term()}.
snapshot_workflow(WorkflowId, Description) when is_binary(WorkflowId),
                                                 is_binary(Description) ->
    case load_workflow(WorkflowId) of
        {ok, WfState} ->
            Timestamp = erlang:system_time(millisecond),
            SnapshotId = gen_snapshot_id(WorkflowId, Timestamp),

            Snapshot = #wf_persist_snapshot{
                snapshot_id = SnapshotId,
                workflow_id = WorkflowId,
                version = maps:get(version, WfState, 0),
                marking = maps:get(marking, WfState, #{}),
                data = maps:get(data, WfState, #{}),
                events = maps:get(events, WfState, []),
                status = maps:get(status, WfState, pending),
                created_at = Timestamp,
                description = Description
            },

            Transaction = fun() ->
                mnesia:write(Snapshot)
            end,

            case mnesia:transaction(Transaction) of
                {atomic, ok} ->
                    {ok, SnapshotId};
                {aborted, Reason} ->
                    logger:error("Failed to create snapshot for ~p: ~p", [WorkflowId, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all snapshots for a workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_snapshots(workflow_id()) -> [snapshot_info()].
list_snapshots(WorkflowId) when is_binary(WorkflowId) ->
    Transaction = fun() ->
        mnesia:index_read(wf_persist_snapshot, WorkflowId, #wf_persist_snapshot.workflow_id)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Snapshots} ->
            lists:map(
                fun(#wf_persist_snapshot{
                    snapshot_id = SnapId,
                    version = Ver,
                    created_at = CreatedAt,
                    description = Desc
                }) ->
                    #{
                        snapshot_id => SnapId,
                        version => Ver,
                        created_at => CreatedAt,
                        description => Desc
                    }
                end,
                lists:sort(
                    fun(#wf_persist_snapshot{created_at = T1},
                        #wf_persist_snapshot{created_at = T2}) ->
                        T1 > T2
                    end,
                    Snapshots
                )
            );
        {aborted, _Reason} ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc Restores a workflow from a snapshot.
%%
%% Returns {ok, WorkflowState} on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec restore_snapshot(workflow_id(), snapshot_id()) ->
          {ok, workflow_state()} | {error, not_found}.
restore_snapshot(WorkflowId, SnapshotId) when is_binary(WorkflowId),
                                               is_binary(SnapshotId) ->
    Transaction = fun() ->
        mnesia:read(wf_persist_snapshot, SnapshotId)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, [#wf_persist_snapshot{
            version = Ver,
            marking = Marking,
            data = Data,
            events = Events,
            status = Status
        }]} ->
            WfState = #{
                workflow_id => WorkflowId,
                status => Status,
                marking => Marking,
                data => Data,
                events => Events,
                version => Ver,
                updated_at => erlang:system_time(millisecond),
                metadata => #{}
            },
            save_workflow(WorkflowId, WfState),
            {ok, WfState};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            logger:error("Failed to restore snapshot ~p: ~p", [SnapshotId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Cleans up old snapshots, keeping only the most recent N.
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_snapshots(workflow_id(), non_neg_integer()) -> ok | {error, term()}.
cleanup_snapshots(WorkflowId, MaxSnapshots) when is_binary(WorkflowId),
                                                  is_integer(MaxSnapshots),
                                                  MaxSnapshots >= 0 ->
    Snapshots = list_snapshots(WorkflowId),

    case length(Snapshots) > MaxSnapshots of
        true ->
            ToDelete = lists:drop(MaxSnapshots, Snapshots),
            Transaction = fun() ->
                lists:foreach(
                    fun(#{snapshot_id := SnapId}) ->
                        mnesia:delete(wf_persist_snapshot, SnapId)
                    end,
                    ToDelete
                )
            end,

            case mnesia:transaction(Transaction) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    logger:error("Failed to cleanup snapshots: ~p", [Reason]),
                    {error, Reason}
            end;
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Creates a checkpoint for fault recovery.
%%
%% @end
%%--------------------------------------------------------------------
-spec create_checkpoint(workflow_id(), [term()]) ->
          {ok, checkpoint_id()} | {error, term()}.
create_checkpoint(WorkflowId, Labels) when is_binary(WorkflowId),
                                            is_list(Labels) ->
    case load_workflow(WorkflowId) of
        {ok, WfState} ->
            Timestamp = erlang:system_time(millisecond),
            CheckpointId = gen_checkpoint_id(WorkflowId, Timestamp),
            Version = maps:get(version, WfState, 0),

            %% Create optional snapshot as part of checkpoint
            SnapshotId = case snapshot_workflow(WorkflowId, <<"checkpoint">>) of
                {ok, SnapId} -> SnapId;
                {error, _} -> undefined
            end,

            Checkpoint = #wf_persist_checkpoint{
                checkpoint_id = CheckpointId,
                workflow_id = WorkflowId,
                version = Version,
                snapshot_id = SnapshotId,
                created_at = Timestamp,
                labels = Labels
            },

            Transaction = fun() ->
                mnesia:write(Checkpoint)
            end,

            case mnesia:transaction(Transaction) of
                {atomic, ok} ->
                    {ok, CheckpointId};
                {aborted, Reason} ->
                    logger:error("Failed to create checkpoint: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all checkpoints.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_checkpoints() -> [{checkpoint_id(), workflow_id(), non_neg_integer(), integer()}].
list_checkpoints() ->
    Transaction = fun() ->
        mnesia:all_keys(wf_persist_checkpoint)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Keys} ->
            lists:filtermap(
                fun(CheckpointId) ->
                    case mnesia:transaction(fun() ->
                        mnesia:read(wf_persist_checkpoint, CheckpointId)
                    end) of
                        {atomic, [#wf_persist_checkpoint{
                            workflow_id = WfId,
                            version = Ver,
                            created_at = CreatedAt
                        }]} ->
                            {true, {CheckpointId, WfId, Ver, CreatedAt}};
                        _ ->
                            false
                    end
                end,
                Keys
            );
        {aborted, _} ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc Restores a workflow from a checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
-spec restore_checkpoint(checkpoint_id()) -> {ok, workflow_id()} | {error, not_found}.
restore_checkpoint(CheckpointId) when is_binary(CheckpointId) ->
    Transaction = fun() ->
        mnesia:read(wf_persist_checkpoint, CheckpointId)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, [#wf_persist_checkpoint{
            workflow_id = WorkflowId,
            snapshot_id = SnapshotId
        }]} ->
            case SnapshotId of
                undefined ->
                    {ok, WorkflowId};
                _ ->
                    case restore_snapshot(WorkflowId, SnapshotId) of
                        {ok, _} -> {ok, WorkflowId};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            logger:error("Failed to restore checkpoint: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_checkpoint(checkpoint_id()) -> ok | {error, term()}.
delete_checkpoint(CheckpointId) when is_binary(CheckpointId) ->
    Transaction = fun() ->
        mnesia:delete(wf_persist_checkpoint, CheckpointId)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a workflow exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec workflow_exists(workflow_id()) -> boolean().
workflow_exists(WorkflowId) when is_binary(WorkflowId) ->
    case load_workflow(WorkflowId) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%%--------------------------------------------------------------------
%% @doc Gets metadata about a snapshot.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_snapshot_info(workflow_id(), snapshot_id()) ->
          {ok, snapshot_info()} | {error, not_found}.
get_snapshot_info(WorkflowId, SnapshotId) when is_binary(WorkflowId),
                                                is_binary(SnapshotId) ->
    Transaction = fun() ->
        mnesia:read(wf_persist_snapshot, SnapshotId)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, [#wf_persist_snapshot{
            version = Ver,
            created_at = CreatedAt,
            description = Desc
        }]} ->
            {ok, #{
                snapshot_id => SnapshotId,
                version => Ver,
                created_at => CreatedAt,
                description => Desc
            }};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            logger:error("Failed to get snapshot info: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Warms the cache by loading all workflows into memory.
%%
%% @end
%%--------------------------------------------------------------------
-spec warm_cache() -> ok | {error, term()}.
warm_cache() ->
    Transaction = fun() ->
        mnesia:all_keys(wf_persist_workflow)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Keys} ->
            lists:foreach(
                fun(WorkflowId) ->
                    _ = load_workflow(WorkflowId)
                end,
                Keys
            ),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to warm cache: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Invalidates the in-memory cache.
%%
%% @end
%%--------------------------------------------------------------------
-spec invalidate_cache() -> ok.
invalidate_cache() ->
    case ets:whereis(wf_persist_cache) of
        undefined -> ok;
        TabId ->
            ets:delete_all_objects(TabId),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Gets a value from the cache.
%%
%% @end
%%--------------------------------------------------------------------
-spec cache_get(workflow_id()) -> {ok, term()} | {error, not_found}.
cache_get(WorkflowId) when is_binary(WorkflowId) ->
    case ets:whereis(wf_persist_cache) of
        undefined ->
            {error, not_found};
        _TabId ->
            case ets:lookup(wf_persist_cache, WorkflowId) of
                [{WorkflowId, Value}] -> {ok, Value};
                [] -> {error, not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Puts a value in the cache.
%%
%% @end
%%--------------------------------------------------------------------
-spec cache_put(workflow_id(), term()) -> ok.
cache_put(WorkflowId, Value) when is_binary(WorkflowId) ->
    case ets:whereis(wf_persist_cache) of
        undefined ->
            init_cache(),
            cache_put(WorkflowId, Value);
        _TabId ->
            ets:insert(wf_persist_cache, {WorkflowId, Value}),
            ok
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
create_mnesia_table(TableName, Def) ->
    case mnesia:create_table(TableName, Def) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, TableName}} ->
            ok;
        {aborted, Reason} ->
            logger:error("Failed to create table ~p: ~p", [TableName, Reason]),
            {error, Reason}
    end.

%% @private
record_to_map(#wf_persist_workflow{
    workflow_id = WorkflowId,
    status = Status,
    marking = Marking,
    data = Data,
    events = Events,
    version = Version,
    updated_at = UpdatedAt,
    metadata = Metadata
}) ->
    #{
        workflow_id => WorkflowId,
        status => Status,
        marking => Marking,
        data => Data,
        events => Events,
        version => Version,
        updated_at => UpdatedAt,
        metadata => Metadata
    }.

%% @private
get_workflow_meta(WorkflowId) ->
    Transaction = fun() ->
        mnesia:read(wf_persist_workflow, WorkflowId)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, [#wf_persist_workflow{
            version = Version,
            created_at = CreatedAt
        }]} ->
            {ok, {Version, CreatedAt}};
        {atomic, []} ->
            {error, not_found};
        {aborted, _Reason} ->
            {error, not_found}
    end.

%% @private
gen_snapshot_id(WorkflowId, Timestamp) ->
    Hash = erlang:phash2({WorkflowId, Timestamp, erlang:monotonic_time()}),
    iolist_to_binary(io_lib:format("snap-~s-~b-~b", [WorkflowId, Timestamp, Hash])).

%% @private
gen_checkpoint_id(WorkflowId, Timestamp) ->
    Hash = erlang:phash2({WorkflowId, Timestamp, erlang:monotonic_time()}),
    iolist_to_binary(io_lib:format("cp-~s-~b-~b", [WorkflowId, Timestamp, Hash])).

%% @private
init_cache() ->
    case ets:whereis(wf_persist_cache) of
        undefined ->
            ets:new(wf_persist_cache, [
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _TabId ->
            ok
    end.
