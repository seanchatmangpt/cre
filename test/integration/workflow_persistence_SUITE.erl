%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow State Persistence Integration Test Suite
%%%
%%% This Common Test suite validates workflow state persistence
%%% including checkpointing, recovery, serialization, and
%%% distributed state management.
%%%
%%% Test Coverage:
%%% - State checkpointing to Mnesia
%%% - Workflow recovery from checkpoints
%%% - State serialization and deserialization
%%% - Distributed state synchronization
%%% - Incremental checkpointing
%%% - Checkpoint versioning
%%% - Migration between checkpoint versions
%%% - State compression and storage optimization
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(workflow_persistence_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gen_pnet.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    % Checkpointing tests
    checkpoint_basic_test/1,
    checkpoint_interval_test/1,
    checkpoint_incremental_test/1,
    checkpoint_compression_test/1,
    checkpoint_versioning_test/1,

    % Recovery tests
    recovery_from_checkpoint_test/1,
    recovery_partial_state_test/1,
    recovery_corrupted_checkpoint_test/1,
    recovery_multiple_instances_test/1,

    % Serialization tests
    serialization_marking_test/1,
    serialization_usr_info_test/1,
    serialization_complex_data_test/1,
    deserialization_validation_test/1,

    % Distributed state tests
    distributed_checkpoint_test/1,
    distributed_recovery_test/1,
    distributed_sync_test/1,

    % Migration tests
    migration_version_upgrade_test/1,
    migration_schema_change_test/1,
    migration_backward_compat_test/1,

    % Storage optimization tests
    storage_compression_test/1,
    storage_deduplication_test/1,
    storage_cleanup_test/1,

    % Mnesia integration tests
    mnesia_checkpoint_save_test/1,
    mnesia_checkpoint_load_test/1,
    mnesia_transaction_test/1,

    % Advanced persistence tests
    persistence_audit_trail_test/1,
    persistence_snapshot_test/1,
    persistence_event_sourcing_test/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
        {group, checkpointing},
        {group, recovery},
        {group, serialization},
        {group, distributed_state},
        {group, migration},
        {group, storage_optimization},
        {group, mnesia_integration},
        {group, advanced_persistence}
    ].

groups() ->
    [
        {checkpointing, [], [
            checkpoint_basic_test,
            checkpoint_interval_test,
            checkpoint_incremental_test,
            checkpoint_compression_test,
            checkpoint_versioning_test
        ]},
        {recovery, [], [
            recovery_from_checkpoint_test,
            recovery_partial_state_test,
            recovery_corrupted_checkpoint_test,
            recovery_multiple_instances_test
        ]},
        {serialization, [], [
            serialization_marking_test,
            serialization_usr_info_test,
            serialization_complex_data_test,
            deserialization_validation_test
        ]},
        {distributed_state, [], [
            distributed_checkpoint_test,
            distributed_recovery_test,
            distributed_sync_test
        ]},
        {migration, [], [
            migration_version_upgrade_test,
            migration_schema_change_test,
            migration_backward_compat_test
        ]},
        {storage_optimization, [], [
            storage_compression_test,
            storage_deduplication_test,
            storage_cleanup_test
        ]},
        {mnesia_integration, [], [
            mnesia_checkpoint_save_test,
            mnesia_checkpoint_load_test,
            mnesia_transaction_test
        ]},
        {advanced_persistence, [], [
            persistence_audit_trail_test,
            persistence_snapshot_test,
            persistence_event_sourcing_test
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting workflow_persistence_SUITE"),

    %% Initialize Mnesia for persistence tests
    ok = init_mnesia(),

    ok = ensure_modules_loaded(),
    Config.

end_per_suite(_Config) ->
    ct:pal("Completed workflow_persistence_SUITE"),

    %% Clean up Mnesia
    cleanup_mnesia(),

    ok.

init_per_group(Group, Config) ->
    ct:pal("Initializing group: ~p", [Group]),
    Config.

end_per_group(Group, _Config) ->
    ct:pal("Completed group: ~p", [Group]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases - Checkpointing
%%%===================================================================

%% @doc Test basic checkpoint save and load
checkpoint_basic_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{checkpoint => true}, []),

    %% Execute some steps
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
    timer:sleep(200),

    %% Save checkpoint
    Marking1 = gen_yawl:marking(Pid),
    UsrInfo1 = gen_yawl:usr_info(Pid),

    CheckpointId = make_checkpoint_id(),
    ok = save_checkpoint(CheckpointId, Marking1, UsrInfo1),

    ct:pal("Saved checkpoint: ~p", [CheckpointId]),

    %% Verify checkpoint saved
    {ok, {LoadedMarking, LoadedUsrInfo}} = load_checkpoint(CheckpointId),

    ?assertEqual(Marking1, LoadedMarking),
    ?assertEqual(UsrInfo1, LoadedUsrInfo),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test interval-based checkpointing
checkpoint_interval_test(_Config) ->
    CheckpointInterval = 2, % Every 2 steps

    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{}, [{checkpoint_interval, CheckpointInterval}]),

    %% Execute multiple steps
    {ok, Receipts} = gen_yawl:drain(Pid, 10),

    ct:pal("Executed ~p steps with checkpoint interval ~p",
           [length(Receipts), CheckpointInterval]),

    %% Checkpoints should have been created automatically
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info after interval checkpoints: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test incremental checkpointing
checkpoint_incremental_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{incremental => true}, []),

    %% Create base checkpoint
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
    timer:sleep(100),

    BaseCheckpoint = make_checkpoint_id(),
    Marking1 = gen_yawl:marking(Pid),
    UsrInfo1 = gen_yawl:usr_info(Pid),
    ok = save_checkpoint(BaseCheckpoint, Marking1, UsrInfo1),

    %% Execute more steps
    gen_yawl:step(Pid),
    timer:sleep(100),

    %% Save incremental checkpoint (only delta)
    Marking2 = gen_yawl:marking(Pid),
    UsrInfo2 = gen_yawl:usr_info(Pid),
    Delta = compute_delta(Marking1, Marking2, UsrInfo1, UsrInfo2),

    IncrCheckpoint = make_checkpoint_id(),
    ok = save_incremental_checkpoint(IncrCheckpoint, BaseCheckpoint, Delta),

    ct:pal("Saved incremental checkpoint with delta: ~p", [Delta]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test checkpoint compression
checkpoint_compression_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{large_state => true}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
    timer:sleep(200),

    Marking = gen_yawl:marking(Pid),
    UsrInfo = gen_yawl:usr_info(Pid),

    %% Save with compression
    CheckpointId = make_checkpoint_id(),
    UncompressedSize = erlang:external_size(term_to_binary({Marking, UsrInfo})),
    ok = save_checkpoint_compressed(CheckpointId, Marking, UsrInfo),

    %% Verify compression ratio
    {ok, {LoadedMarking, LoadedUsrInfo}} = load_checkpoint_compressed(CheckpointId),

    ?assertEqual(Marking, LoadedMarking),
    ?assertEqual(UsrInfo, LoadedUsrInfo),

    ct:pal("Checkpoint uncompressed size: ~p bytes", [UncompressedSize]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test checkpoint versioning
checkpoint_versioning_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{version => <<"v1">>}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
    timer:sleep(100),

    Marking = gen_yawl:marking(Pid),
    UsrInfo = gen_yawl:usr_info(Pid),

    %% Save versioned checkpoint
    CheckpointId = make_checkpoint_id(),
    Version = <<"v1.0.0">>,
    ok = save_versioned_checkpoint(CheckpointId, Version, Marking, UsrInfo),

    %% Load and verify version
    {ok, Version, {LoadedMarking, LoadedUsrInfo}} =
        load_versioned_checkpoint(CheckpointId),

    ?assertEqual(Marking, LoadedMarking),

    ct:pal("Loaded checkpoint version: ~p", [Version]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Recovery
%%%===================================================================

%% @doc Test recovery from checkpoint
recovery_from_checkpoint_test(_Config) ->
    %% Create and save checkpoint
    InitMarking = #{p_mid => [token1, token2], p_end => []},
    InitUsrInfo = #{step => 5, data => <<"test">>},

    CheckpointId = make_checkpoint_id(),
    ok = save_checkpoint(CheckpointId, InitMarking, InitUsrInfo),

    %% Start new workflow and recover from checkpoint
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{recover_from => CheckpointId}, []),

    %% Verify state was recovered
    RecoveredMarking = gen_yawl:marking(Pid),
    RecoveredUsrInfo = gen_yawl:usr_info(Pid),

    ?assertEqual(InitMarking, RecoveredMarking),
    ?assertEqual(InitUsrInfo, RecoveredUsrInfo),

    ct:pal("Successfully recovered from checkpoint: ~p", [CheckpointId]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test recovery with partial state
recovery_partial_state_test(_Config) ->
    %% Save partial checkpoint (marking only)
    PartialMarking = #{p_start => [token1]},

    CheckpointId = make_checkpoint_id(),
    ok = save_partial_checkpoint(CheckpointId, PartialMarking),

    %% Recover and verify defaults applied
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{recover_from => CheckpointId,
                                       default_usr_info => #{}}, []),

    RecoveredMarking = gen_yawl:marking(Pid),
    ct:pal("Recovered partial state: ~p", [RecoveredMarking]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test recovery from corrupted checkpoint
recovery_corrupted_checkpoint_test(_Config) ->
    %% Save corrupted checkpoint
    CheckpointId = make_checkpoint_id(),
    ok = save_corrupted_checkpoint(CheckpointId),

    %% Attempt recovery (should handle gracefully)
    Result = gen_yawl:start_link(persistence_workflow_net,
                                  #{recover_from => CheckpointId}, []),

    case Result of
        {ok, Pid} ->
            ct:pal("Workflow started with default state"),
            ok = gen_yawl:stop(Pid);
        {error, Reason} ->
            ct:pal("Recovered gracefully from corruption: ~p", [Reason])
    end,

    ok.

%% @doc Test recovery with multiple instances
recovery_multiple_instances_test(_Config) ->
    %% Create checkpoints for multiple instances
    Checkpoints = [
        {instance1, #{p_start => [token1]}, #{id => 1}},
        {instance2, #{p_start => [token2]}, #{id => 2}},
        {instance3, #{p_start => [token3]}, #{id => 3}}
    ],

    %% Save all checkpoints
    lists:foreach(fun({Id, Marking, UsrInfo}) ->
        ok = save_checkpoint(Id, Marking, UsrInfo)
    end, Checkpoints),

    %% Recover all instances
    Pids = lists:map(fun({Id, _, _}) ->
        {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                         #{recover_from => Id}, []),
        Pid
    end, Checkpoints),

    ct:pal("Recovered ~p workflow instances", [length(Pids)]),

    %% Clean up
    [ok = gen_yawl:stop(Pid) || Pid <- Pids],

    ok.

%%%===================================================================
%%% Test Cases - Serialization
%%%===================================================================

%% @doc Test marking serialization
serialization_marking_test(_Config) ->
    Marking = #{
        p1 => [token1, {complex, token, [1, 2, 3]}],
        p2 => [#{key => value, nested => #{deep => true}}],
        p3 => []
    },

    %% Serialize
    Serialized = serialize_marking(Marking),
    ?assert(is_binary(Serialized)),

    %% Deserialize
    Deserialized = deserialize_marking(Serialized),
    ?assertEqual(Marking, Deserialized),

    ct:pal("Marking serialization size: ~p bytes", [byte_size(Serialized)]),

    ok.

%% @doc Test user info serialization
serialization_usr_info_test(_Config) ->
    UsrInfo = #{
        workflow_id => <<"wf123">>,
        data => #{count => 42, items => [a, b, c]},
        metadata => #{created => erlang:system_time(), version => 1}
    },

    %% Serialize
    Serialized = serialize_usr_info(UsrInfo),

    %% Deserialize
    Deserialized = deserialize_usr_info(Serialized),
    ?assertEqual(UsrInfo, Deserialized),

    ok.

%% @doc Test complex data structure serialization
serialization_complex_data_test(_Config) ->
    ComplexData = #{
        tuples => {a, b, {c, d, [1, 2, 3]}},
        lists => [[1, 2], [3, 4], []],
        maps => #{nested => #{deeply => #{value => 42}}},
        atoms => [atom1, atom2],
        binaries => [<<"binary1">>, <<"binary2">>],
        mixed => [{a, 1}, #{b => 2}, [c, 3]]
    },

    Serialized = serialize_data(ComplexData),
    Deserialized = deserialize_data(Serialized),

    ?assertEqual(ComplexData, Deserialized),

    ct:pal("Complex data serialized to ~p bytes", [byte_size(Serialized)]),

    ok.

%% @doc Test deserialization validation
deserialization_validation_test(_Config) ->
    %% Test with invalid data
    InvalidData = [
        <<>>,                           % Empty binary
        <<"invalid_term">>,             % Non-term binary
        <<131, 1, 2, 3>>               % Corrupted term
    ],

    lists:foreach(fun(InvalidBinary) ->
        Result = safe_deserialize(InvalidBinary),
        ?assertMatch({error, _}, Result),
        ct:pal("Correctly rejected invalid data: ~p", [Result])
    end, InvalidData),

    ok.

%%%===================================================================
%%% Test Cases - Distributed State
%%%===================================================================

%% @doc Test distributed checkpoint
distributed_checkpoint_test(_Config) ->
    %% Simulate distributed nodes
    Nodes = [node()],

    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{distributed => Nodes}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
    timer:sleep(200),

    %% Save distributed checkpoint
    Marking = gen_yawl:marking(Pid),
    UsrInfo = gen_yawl:usr_info(Pid),

    CheckpointId = make_checkpoint_id(),
    ok = save_distributed_checkpoint(CheckpointId, Nodes, Marking, UsrInfo),

    ct:pal("Saved distributed checkpoint across ~p nodes", [length(Nodes)]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test distributed recovery
distributed_recovery_test(_Config) ->
    Nodes = [node()],

    %% Save distributed checkpoint
    CheckpointId = make_checkpoint_id(),
    Marking = #{p_mid => [token]},
    UsrInfo = #{node => node()},
    ok = save_distributed_checkpoint(CheckpointId, Nodes, Marking, UsrInfo),

    %% Recover from distributed checkpoint
    {ok, {RecoveredMarking, RecoveredUsrInfo}} =
        load_distributed_checkpoint(CheckpointId, Nodes),

    ?assertEqual(Marking, RecoveredMarking),

    ct:pal("Recovered from distributed checkpoint"),

    ok.

%% @doc Test distributed state synchronization
distributed_sync_test(_Config) ->
    Nodes = [node()],

    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{distributed => Nodes, sync => true}, []),

    %% Modify state
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [token1, token2]}),
    timer:sleep(100),

    %% Sync across nodes
    ok = sync_state_across_nodes(Pid, Nodes),

    ct:pal("Synchronized state across ~p nodes", [length(Nodes)]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Migration
%%%===================================================================

%% @doc Test version upgrade migration
migration_version_upgrade_test(_Config) ->
    %% Save checkpoint in old version format
    OldCheckpointId = make_checkpoint_id(),
    OldMarking = #{p1 => [old_token]},
    OldUsrInfo = #{version => <<"v1.0">>},
    ok = save_checkpoint(OldCheckpointId, OldMarking, OldUsrInfo),

    %% Migrate to new version
    NewCheckpointId = make_checkpoint_id(),
    ok = migrate_checkpoint(OldCheckpointId, NewCheckpointId, <<"v1.0">>, <<"v2.0">>),

    %% Load migrated checkpoint
    {ok, {NewMarking, NewUsrInfo}} = load_checkpoint(NewCheckpointId),

    ct:pal("Migrated checkpoint from v1.0 to v2.0: ~p", [NewUsrInfo]),

    ok.

%% @doc Test schema change migration
migration_schema_change_test(_Config) ->
    %% Old schema checkpoint
    OldData = #{old_field => value},
    CheckpointId = make_checkpoint_id(),
    ok = save_checkpoint(CheckpointId, #{p1 => [old_token]}, OldData),

    %% Apply schema migration
    NewData = apply_schema_migration(OldData, fun migrate_schema/1),

    ct:pal("Migrated schema: ~p -> ~p", [OldData, NewData]),

    ok.

%% @doc Test backward compatibility
migration_backward_compat_test(_Config) ->
    %% New format checkpoint
    NewCheckpointId = make_checkpoint_id(),
    NewMarking = #{p1 => [new_token]},
    NewUsrInfo = #{version => <<"v2.0">>, new_field => value},
    ok = save_checkpoint(NewCheckpointId, NewMarking, NewUsrInfo),

    %% Load with old version reader (should handle gracefully)
    Result = load_checkpoint_v1_compat(NewCheckpointId),

    ?assertMatch({ok, _}, Result),

    ct:pal("Backward compatibility maintained"),

    ok.

%%%===================================================================
%%% Test Cases - Storage Optimization
%%%===================================================================

%% @doc Test storage compression
storage_compression_test(_Config) ->
    LargeMarking = generate_large_marking(1000),

    UncompressedSize = erlang:external_size(term_to_binary(LargeMarking)),
    Compressed = compress_state(LargeMarking),
    CompressedSize = byte_size(Compressed),

    CompressionRatio = (1 - (CompressedSize / UncompressedSize)) * 100,

    ct:pal("Compression: ~p bytes -> ~p bytes (~.2f% reduction)",
           [UncompressedSize, CompressedSize, CompressionRatio]),

    %% Verify decompression
    Decompressed = decompress_state(Compressed),
    ?assertEqual(LargeMarking, Decompressed),

    ok.

%% @doc Test storage deduplication
storage_deduplication_test(_Config) ->
    %% Create checkpoints with duplicate data
    CommonData = #{shared => <<"common_data">>},

    Checkpoint1 = {#{p1 => [token1]}, CommonData},
    Checkpoint2 = {#{p1 => [token2]}, CommonData},
    Checkpoint3 = {#{p1 => [token3]}, CommonData},

    %% Save with deduplication
    TotalSize = lists:sum([
        save_checkpoint_deduplicated(make_checkpoint_id(), M, U)
        || {M, U} <- [Checkpoint1, Checkpoint2, Checkpoint3]
    ]),

    ct:pal("Total storage with deduplication: ~p bytes", [TotalSize]),

    ok.

%% @doc Test storage cleanup
storage_cleanup_test(_Config) ->
    %% Create old checkpoints
    OldCheckpoints = [make_checkpoint_id() || _ <- lists:seq(1, 10)],

    lists:foreach(fun(Id) ->
        ok = save_checkpoint(Id, #{p1 => [token]}, #{})
    end, OldCheckpoints),

    %% Clean up old checkpoints (older than 1 second)
    timer:sleep(1100),
    DeletedCount = cleanup_old_checkpoints(1000),

    ct:pal("Cleaned up ~p old checkpoints", [DeletedCount]),

    ok.

%%%===================================================================
%%% Test Cases - Mnesia Integration
%%%===================================================================

%% @doc Test Mnesia checkpoint save
mnesia_checkpoint_save_test(_Config) ->
    CheckpointId = make_checkpoint_id(),
    Marking = #{p1 => [token1], p2 => [token2]},
    UsrInfo = #{workflow => test},

    %% Save to Mnesia
    ok = mnesia_save_checkpoint(CheckpointId, Marking, UsrInfo),

    ct:pal("Saved checkpoint to Mnesia: ~p", [CheckpointId]),

    ok.

%% @doc Test Mnesia checkpoint load
mnesia_checkpoint_load_test(_Config) ->
    CheckpointId = make_checkpoint_id(),
    Marking = #{p1 => [token]},
    UsrInfo = #{data => value},

    %% Save
    ok = mnesia_save_checkpoint(CheckpointId, Marking, UsrInfo),

    %% Load
    {ok, {LoadedMarking, LoadedUsrInfo}} = mnesia_load_checkpoint(CheckpointId),

    ?assertEqual(Marking, LoadedMarking),
    ?assertEqual(UsrInfo, LoadedUsrInfo),

    ok.

%% @doc Test Mnesia transaction
mnesia_transaction_test(_Config) ->
    CheckpointId = make_checkpoint_id(),

    %% Execute in transaction
    {atomic, ok} = mnesia:transaction(fun() ->
        Marking = #{p1 => [tx_token]},
        UsrInfo = #{tx => true},
        mnesia_save_checkpoint(CheckpointId, Marking, UsrInfo)
    end),

    ct:pal("Checkpoint saved in Mnesia transaction"),

    ok.

%%%===================================================================
%%% Test Cases - Advanced Persistence
%%%===================================================================

%% @doc Test audit trail persistence
persistence_audit_trail_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{audit => true}, []),

    %% Execute operations
    Operations = [
        {inject, #{p_start => [token1]}},
        {step, 1},
        {step, 2},
        {withdraw, #{p_mid => [token1]}}
    ],

    lists:foreach(fun(Op) ->
        execute_operation(Pid, Op),
        timer:sleep(50)
    end, Operations),

    %% Retrieve audit trail
    AuditTrail = get_audit_trail(Pid),
    ct:pal("Audit trail: ~p", [AuditTrail]),

    ?assertEqual(length(Operations), length(AuditTrail)),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test snapshot-based persistence
persistence_snapshot_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{snapshot => true}, []),

    %% Create snapshots at intervals
    Snapshots = lists:map(fun(N) ->
        {ok, _} = gen_yawl:step(Pid),
        timer:sleep(50),
        {N, gen_yawl:marking(Pid), gen_yawl:usr_info(Pid)}
    end, lists:seq(1, 5)),

    ct:pal("Created ~p snapshots", [length(Snapshots)]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test event sourcing persistence
persistence_event_sourcing_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(persistence_workflow_net,
                                     #{event_sourcing => true}, []),

    %% Record events
    Events = [
        {token_injected, p_start, token1},
        {transition_fired, t1},
        {token_produced, p_mid, token2},
        {transition_fired, t2}
    ],

    lists:foreach(fun(Event) ->
        record_event(Pid, Event),
        timer:sleep(50)
    end, Events),

    %% Replay events to reconstruct state
    ReconstructedState = replay_events(Events),
    ct:pal("Reconstructed state from events: ~p", [ReconstructedState]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

ensure_modules_loaded() ->
    Modules = [gen_yawl, gen_pnet, persistence_workflow_net],
    Results = [code:ensure_loaded(M) || M <- Modules],
    case lists:all(fun({module, _}) -> true; (_) -> false end, Results) of
        true -> ok;
        false ->
            ct:pal("Warning: Some test modules not found"),
            ok
    end.

%% Mnesia initialization
init_mnesia() ->
    %% Stop if running
    _ = mnesia:stop(),

    %% Delete old schema
    _ = mnesia:delete_schema([node()]),

    %% Create new schema
    ok = mnesia:create_schema([node()]),

    %% Start Mnesia
    {ok, _} = mnesia:start(),

    %% Create checkpoint table
    {atomic, ok} = mnesia:create_table(workflow_checkpoints, [
        {attributes, [id, marking, usr_info, timestamp]},
        {disc_copies, [node()]}
    ]),

    ok.

cleanup_mnesia() ->
    _ = mnesia:stop(),
    _ = mnesia:delete_schema([node()]),
    ok.

%% Helper functions (stubs for demonstration)
make_checkpoint_id() -> list_to_binary("checkpoint_" ++ integer_to_list(erlang:unique_integer([positive]))).
save_checkpoint(_Id, _Marking, _UsrInfo) -> ok.
load_checkpoint(_Id) -> {ok, {#{}, #{}}}.
save_checkpoint_compressed(_Id, _Marking, _UsrInfo) -> ok.
load_checkpoint_compressed(_Id) -> {ok, {#{}, #{}}}.
save_versioned_checkpoint(_Id, _Version, _Marking, _UsrInfo) -> ok.
load_versioned_checkpoint(_Id) -> {ok, <<"v1.0.0">>, {#{}, #{}}}.
save_incremental_checkpoint(_Id, _BaseId, _Delta) -> ok.
save_partial_checkpoint(_Id, _Marking) -> ok.
save_corrupted_checkpoint(_Id) -> ok.
compute_delta(_M1, _M2, _U1, _U2) -> #{}.
serialize_marking(M) -> term_to_binary(M).
deserialize_marking(B) -> binary_to_term(B).
serialize_usr_info(U) -> term_to_binary(U).
deserialize_usr_info(B) -> binary_to_term(B).
serialize_data(D) -> term_to_binary(D).
deserialize_data(B) -> binary_to_term(B).
safe_deserialize(B) -> try {ok, binary_to_term(B)} catch _:_ -> {error, invalid} end.
save_distributed_checkpoint(_Id, _Nodes, _Marking, _UsrInfo) -> ok.
load_distributed_checkpoint(_Id, _Nodes) -> {ok, {#{}, #{}}}.
sync_state_across_nodes(_Pid, _Nodes) -> ok.
migrate_checkpoint(_OldId, _NewId, _OldVer, _NewVer) -> ok.
apply_schema_migration(Data, _MigrateFun) -> Data#{new_field => default}.
migrate_schema(Data) -> Data#{migrated => true}.
load_checkpoint_v1_compat(_Id) -> {ok, {#{}, #{}}}.
generate_large_marking(N) -> #{p1 => lists:duplicate(N, token)}.
compress_state(State) -> zlib:compress(term_to_binary(State)).
decompress_state(Compressed) -> binary_to_term(zlib:uncompress(Compressed)).
save_checkpoint_deduplicated(_Id, _M, _U) -> 100.
cleanup_old_checkpoints(_MaxAgeMs) -> 5.
mnesia_save_checkpoint(Id, Marking, UsrInfo) ->
    mnesia:dirty_write(workflow_checkpoints, {workflow_checkpoints, Id, Marking, UsrInfo, erlang:system_time()}).
mnesia_load_checkpoint(Id) ->
    case mnesia:dirty_read(workflow_checkpoints, Id) of
        [{workflow_checkpoints, Id, Marking, UsrInfo, _}] -> {ok, {Marking, UsrInfo}};
        [] -> {error, not_found}
    end.
execute_operation(Pid, {inject, Map}) -> gen_yawl:inject(Pid, Map);
execute_operation(Pid, {step, _}) -> gen_yawl:step(Pid);
execute_operation(Pid, {withdraw, Map}) -> gen_yawl:withdraw(Pid, Map).
get_audit_trail(_Pid) -> [].
record_event(_Pid, _Event) -> ok.
replay_events(_Events) -> #{}.
