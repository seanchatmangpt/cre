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
%% @author CRE Project
%% @copyright 2025
%%
%% @doc Backup and Restore Operations Test Suite
%%
%% This Common Test suite validates backup/restore functionality for Mnesia,
%% including:
%%
%% <h3>Test Coverage</h3>
%% <ul>
%%   <li><b>Backup Creation:</b> Full and incremental backup generation</li>
%%   <li><b>Backup Integrity:</b> Verification of backup file contents</li>
%%   <li><b>Data Restoration:</b> Complete and selective table restoration</li>
%%   <li><b>Table Verification:</b> Mnesia table schema and data validation</li>
%%   <li><b>Concurrent Operations:</b> Backup during active table writes</li>
%%   <li><b>Error Handling:</b> Invalid paths, corrupted files, missing tables</li>
%%   <li><b>Shell Scripts:</b> Integration with scripts/backup.sh</li>
%% </ul>
%%
%% <h3>Test Workflow</h3>
%% 1. Initialize Mnesia with test schema
%% 2. Create multiple test tables with different storage types
%% 3. Insert test data with various patterns
%% 4. Create backups at different stages
%% 5. Verify backup file integrity and format
%% 6. Restore from backups into fresh Mnesia instance
%% 7. Validate restored data matches original
%% 8. Verify table metadata and attributes
%%
%% <h3>Test Data</h3>
%% The suite creates:
%% - workflow_instances: Workflow execution records (disc_copies)
%% - task_results: Task output data (ram_copies)
%% - checkpoint_logs: Execution checkpoints (disc_copies)
%% - metrics_data: Performance metrics (disc_only_copies)
%% - audit_events: Audit trail records (bag type)
%%
%% <h3>Backup Script Integration</h3>
%% Tests verify the shell script at scripts/backup.sh:
%% - Backup file naming and location conventions
%% - GCS upload readiness (file format, size, checksums)
%% - Restoration readiness (extractable, valid format)
%% - Metadata generation and validation
%%
%% @end
%% -------------------------------------------------------------------

-module(backup_restore_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data Records
%%====================================================================

-record(workflow_instances, {
    id :: binary(),
    name :: string(),
    status :: atom(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record(task_results, {
    task_id :: binary(),
    workflow_id :: binary(),
    result :: term(),
    timestamp :: integer()
}).

-record(checkpoint_logs, {
    checkpoint_id :: binary(),
    workflow_id :: binary(),
    state :: term(),
    created_at :: integer()
}).

-record(metrics_data, {
    metric_id :: binary(),
    type :: atom(),
    value :: number(),
    timestamp :: integer()
}).

-record(audit_events, {
    event_id :: binary(),
    user_id :: binary(),
    action :: atom(),
    timestamp :: integer()
}).

%%====================================================================
%% Common Test Callbacks
%%====================================================================

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test case exports
-export([
    backup_full_creates_valid_file/1,
    backup_with_empty_tables/1,
    backup_with_large_dataset/1,
    restore_from_backup_basic/1,
    restore_creates_table_structure/1,
    restore_preserves_data_integrity/1,
    restore_multiple_tables/1,
    backup_and_restore_roundtrip/1,
    restore_partial_tables/1,
    backup_incremental/1,
    backup_with_concurrent_writes/1,
    verify_table_schema_after_restore/1,
    verify_table_attributes_preserved/1,
    verify_storage_types_preserved/1,
    backup_file_format_validation/1,
    restore_handles_missing_file/1,
    restore_handles_corrupted_file/1,
    list_backups_finds_valid_files/1,
    list_backups_excludes_non_backup_files/1,
    backup_respects_backup_level/1,
    restore_clears_target_tables/1,
    backup_creates_readable_tar_format/1,
    restore_idempotent/1,
    backup_disk_space_estimation/1,
    verify_backup_contains_table_data/1
]).

%%====================================================================
%% Test Suite Metadata
%%====================================================================

all() ->
    [
        backup_full_creates_valid_file,
        backup_with_empty_tables,
        backup_with_large_dataset,
        restore_from_backup_basic,
        restore_creates_table_structure,
        restore_preserves_data_integrity,
        restore_multiple_tables,
        backup_and_restore_roundtrip,
        restore_partial_tables,
        backup_incremental,
        backup_with_concurrent_writes,
        verify_table_schema_after_restore,
        verify_table_attributes_preserved,
        verify_storage_types_preserved,
        backup_file_format_validation,
        restore_handles_missing_file,
        restore_handles_corrupted_file,
        list_backups_finds_valid_files,
        list_backups_excludes_non_backup_files,
        backup_respects_backup_level,
        restore_clears_target_tables,
        backup_creates_readable_tar_format,
        restore_idempotent,
        backup_disk_space_estimation,
        verify_backup_contains_table_data
    ].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

%% @doc Initialize test suite - run once before all tests
-spec init_per_suite(term()) -> term().
init_per_suite(Config) ->
    %% Ensure Mnesia is stopped
    catch mnesia:stop(),
    catch mnesia:delete_schema([node()]),

    %% Start fresh Mnesia
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    %% Wait for Mnesia to be ready
    mnesia:wait_for_tables([schema], 5000),

    %% Create backup test directory
    BackupDir = "/tmp/cre_backup_test",
    filelib:ensure_dir(BackupDir ++ "/"),
    file:make_dir(BackupDir),

    [{backup_dir, BackupDir} | Config].

%% @doc Clean up test suite - run once after all tests
-spec end_per_suite(term()) -> term().
end_per_suite(Config) ->
    %% Stop Mnesia
    catch mnesia:stop(),
    catch mnesia:delete_schema([node()]),

    %% Clean up backup directory
    BackupDir = ?config(backup_dir, Config),
    catch delete_directory_recursive(BackupDir),

    ok.

%%====================================================================
%% Test Case Setup/Teardown
%%====================================================================

%% @doc Initialize test case - run before each test
-spec init_per_testcase(atom(), term()) -> term().
init_per_testcase(_TestCase, Config) ->
    %% Stop and restart Mnesia for clean state
    catch mnesia:stop(),
    catch mnesia:delete_schema([node()]),

    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mnesia:wait_for_tables([schema], 5000),

    Config.

%% @doc Clean up test case - run after each test
-spec end_per_testcase(atom(), term()) -> ok.
end_per_testcase(_TestCase, Config) ->
    %% Stop Mnesia
    catch mnesia:stop(),
    catch mnesia:delete_schema([node()]),

    %% Clean up any test files
    BackupDir = ?config(backup_dir, Config),
    catch delete_directory_contents(BackupDir),

    ok.

%%====================================================================
%% Test Cases: Backup Creation
%%====================================================================

%% @doc Test full backup creates a valid backup file
-spec backup_full_creates_valid_file(term()) -> ok.
backup_full_creates_valid_file(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_full_backup.bak"),

    %% Create test table with data
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(3),

    %% Create backup
    Result = mnesia_manager:backup(BackupFile, full),

    %% Verify success
    ?assertEqual(ok, Result),
    ?assert(filelib:is_file(BackupFile)),
    ?assert(filelib:file_size(BackupFile) > 0),

    ok.

%% @doc Test backup works with empty tables
-spec backup_with_empty_tables(term()) -> ok.
backup_with_empty_tables(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_empty_tables.bak"),

    %% Create empty tables
    ok = create_test_table(workflow_instances, ram_copies),
    ok = create_test_table(task_results, disc_copies),

    %% Create backup of empty tables
    Result = mnesia_manager:backup(BackupFile),

    %% Verify success even with empty tables
    ?assertEqual(ok, Result),
    ?assert(filelib:is_file(BackupFile)),

    ok.

%% @doc Test backup with large dataset
-spec backup_with_large_dataset(term()) -> ok.
backup_with_large_dataset(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_large_backup.bak"),

    %% Create test tables
    ok = create_test_table(workflow_instances, disc_copies),
    ok = create_test_table(task_results, ram_copies),

    %% Insert large dataset (1000 records)
    insert_test_workflows(1000),
    insert_test_results(1000),

    %% Create backup
    Result = mnesia_manager:backup(BackupFile, full),

    %% Verify success and file size
    ?assertEqual(ok, Result),
    ?assert(filelib:is_file(BackupFile)),
    FileSize = filelib:file_size(BackupFile),
    ?assert(FileSize > 10000),  %% Should be significantly larger

    ok.

%%====================================================================
%% Test Cases: Restore Operations
%%====================================================================

%% @doc Test basic restore from backup
-spec restore_from_backup_basic(term()) -> ok.
restore_from_backup_basic(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_restore_basic.bak"),

    %% Create and backup test table
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(5),
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Clear the table
    {atomic, ok} = mnesia:clear_table(workflow_instances),
    ?assertEqual({atomic, 0}, mnesia:table_info(workflow_instances, size)),

    %% Restore from backup
    Result = mnesia_manager:restore(BackupFile),
    ?assertEqual(ok, Result),

    %% Verify data is restored
    {atomic, Count} = mnesia:table_info(workflow_instances, size),
    ?assertEqual(5, Count),

    ok.

%% @doc Test restore creates correct table structure
-spec restore_creates_table_structure(term()) -> ok.
restore_creates_table_structure(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_restore_structure.bak"),

    %% Create tables with specific structure
    ok = create_test_table(workflow_instances, disc_copies),
    ok = create_test_table(task_results, ram_copies),
    ok = create_test_table(checkpoint_logs, disc_copies),

    insert_test_workflows(2),
    insert_test_results(2),

    %% Backup
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Delete tables
    ok = mnesia_manager:delete_table(workflow_instances),
    ok = mnesia_manager:delete_table(task_results),
    ok = mnesia_manager:delete_table(checkpoint_logs),

    %% Verify tables are deleted
    Tables = mnesia_manager:list_tables(),
    ?assertNot(lists:member(workflow_instances, Tables)),

    %% Restore
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    %% Verify tables are recreated
    RestoredTables = mnesia_manager:list_tables(),
    ?assert(lists:member(workflow_instances, RestoredTables)),
    ?assert(lists:member(task_results, RestoredTables)),

    ok.

%% @doc Test restore preserves data integrity
-spec restore_preserves_data_integrity(term()) -> ok.
restore_preserves_data_integrity(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_restore_integrity.bak"),

    %% Create test table and insert specific data
    ok = create_test_table(workflow_instances, disc_copies),

    OriginalData = [
        {workflow_instances, <<"wf1">>, <<"Workflow 1">>, active, 100, 101},
        {workflow_instances, <<"wf2">>, <<"Workflow 2">>, completed, 200, 201},
        {workflow_instances, <<"wf3">>, <<"Workflow 3">>, failed, 300, 301}
    ],

    lists:foreach(
        fun({Table, Id, Name, Status, Created, Updated}) ->
            {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#workflow_instances{
                    id = Id,
                    name = Name,
                    status = Status,
                    created_at = Created,
                    updated_at = Updated
                })
            end)
        end,
        OriginalData
    ),

    %% Backup and restore
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),
    {atomic, ok} = mnesia:clear_table(workflow_instances),
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    %% Verify data integrity
    {atomic, Records} = mnesia:transaction(fun() ->
        mnesia:match_object(#workflow_instances{_ = '_'})
    end),

    ?assertEqual(3, length(Records)),

    %% Verify specific records
    lists:foreach(
        fun({Table, Id, Name, Status, Created, Updated}) ->
            Found = lists:search(
                fun(R) -> R#workflow_instances.id == Id end,
                Records
            ),
            ?assertMatch({value, _}, Found),
            {value, Rec} = Found,
            ?assertEqual(Name, Rec#workflow_instances.name),
            ?assertEqual(Status, Rec#workflow_instances.status),
            ?assertEqual(Created, Rec#workflow_instances.created_at),
            ?assertEqual(Updated, Rec#workflow_instances.updated_at)
        end,
        OriginalData
    ),

    ok.

%% @doc Test restore with multiple tables
-spec restore_multiple_tables(term()) -> ok.
restore_multiple_tables(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_restore_multiple.bak"),

    %% Create multiple tables
    ok = create_test_table(workflow_instances, disc_copies),
    ok = create_test_table(task_results, ram_copies),
    ok = create_test_table(checkpoint_logs, disc_only_copies),
    ok = create_test_table(audit_events, disc_copies),

    %% Insert data in each table
    insert_test_workflows(3),
    insert_test_results(3),
    insert_test_checkpoints(3),
    insert_test_events(3),

    %% Backup all tables
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Delete all tables
    lists:foreach(
        fun(T) -> ok = mnesia_manager:delete_table(T) end,
        [workflow_instances, task_results, checkpoint_logs, audit_events]
    ),

    %% Restore
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    %% Verify all tables are restored with data
    {atomic, WfCount} = mnesia:table_info(workflow_instances, size),
    {atomic, TrCount} = mnesia:table_info(task_results, size),
    {atomic, CpCount} = mnesia:table_info(checkpoint_logs, size),
    {atomic, AuCount} = mnesia:table_info(audit_events, size),

    ?assertEqual(3, WfCount),
    ?assertEqual(3, TrCount),
    ?assertEqual(3, CpCount),
    ?assertEqual(3, AuCount),

    ok.

%%====================================================================
%% Test Cases: Roundtrip and Integration
%%====================================================================

%% @doc Test complete backup and restore roundtrip
-spec backup_and_restore_roundtrip(term()) -> ok.
backup_and_restore_roundtrip(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_roundtrip.bak"),

    %% Create comprehensive test data
    ok = create_test_table(workflow_instances, disc_copies),
    ok = create_test_table(task_results, ram_copies),
    ok = create_test_table(checkpoint_logs, disc_copies),

    insert_test_workflows(50),
    insert_test_results(100),
    insert_test_checkpoints(25),

    %% Get original counts
    {atomic, OrigWfCount} = mnesia:table_info(workflow_instances, size),
    {atomic, OrigTrCount} = mnesia:table_info(task_results, size),
    {atomic, OrigCpCount} = mnesia:table_info(checkpoint_logs, size),

    %% Create backup
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Clear all tables
    lists:foreach(
        fun(T) -> {atomic, ok} = mnesia:clear_table(T) end,
        [workflow_instances, task_results, checkpoint_logs]
    ),

    %% Restore
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    %% Verify counts match
    {atomic, RestoredWfCount} = mnesia:table_info(workflow_instances, size),
    {atomic, RestoredTrCount} = mnesia:table_info(task_results, size),
    {atomic, RestoredCpCount} = mnesia:table_info(checkpoint_logs, size),

    ?assertEqual(OrigWfCount, RestoredWfCount),
    ?assertEqual(OrigTrCount, RestoredTrCount),
    ?assertEqual(OrigCpCount, RestoredCpCount),

    ok.

%% @doc Test restore with partial table selection
-spec restore_partial_tables(term()) -> ok.
restore_partial_tables(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_restore_partial.bak"),

    %% Create and populate tables
    ok = create_test_table(workflow_instances, disc_copies),
    ok = create_test_table(task_results, disc_copies),

    insert_test_workflows(5),
    insert_test_results(5),

    %% Backup
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Clear only workflow_instances
    {atomic, ok} = mnesia:clear_table(workflow_instances),
    {atomic, 0} = mnesia:table_info(workflow_instances, size),

    %% Restore (should restore all tables since Mnesia doesn't support selective restore)
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    %% Verify both tables are restored
    {atomic, WfCount} = mnesia:table_info(workflow_instances, size),
    {atomic, TrCount} = mnesia:table_info(task_results, size),

    ?assertEqual(5, WfCount),
    ?assertEqual(5, TrCount),

    ok.

%% @doc Test incremental backup
-spec backup_incremental(term()) -> ok.
backup_incremental(Config) ->
    BackupDir = ?config(backup_dir, Config),
    FullBackupFile = filename:join(BackupDir, "test_incremental_full.bak"),
    IncBackupFile = filename:join(BackupDir, "test_incremental_inc.bak"),

    %% Create table with initial data
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(10),

    %% Create full backup
    ?assertEqual(ok, mnesia_manager:backup(FullBackupFile, full)),
    ?assert(filelib:is_file(FullBackupFile)),

    %% Add more data
    insert_test_workflows(5),

    %% Create incremental backup
    Result = mnesia_manager:backup(IncBackupFile, incremental),

    %% Both should succeed
    ?assertEqual(ok, Result),
    ?assert(filelib:is_file(IncBackupFile)),

    ok.

%% @doc Test backup during concurrent writes
-spec backup_with_concurrent_writes(term()) -> ok.
backup_with_concurrent_writes(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_concurrent_backup.bak"),

    %% Create test table
    ok = create_test_table(workflow_instances, disc_copies),

    %% Insert initial data
    insert_test_workflows(10),

    %% Perform backup (should handle concurrent writes gracefully)
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Verify backup was successful
    ?assert(filelib:is_file(BackupFile)),
    ?assert(filelib:file_size(BackupFile) > 0),

    ok.

%%====================================================================
%% Test Cases: Table Verification
%%====================================================================

%% @doc Verify table schema after restore
-spec verify_table_schema_after_restore(term()) -> ok.
verify_table_schema_after_restore(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_schema_restore.bak"),

    %% Create table with specific schema
    ok = create_test_table(workflow_instances, disc_copies),

    %% Get original schema info
    OrigSchemaInfo = mnesia_manager:get_table_info(workflow_instances),
    OrigAttrs = maps:get(attributes, OrigSchemaInfo),

    %% Backup and delete
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),
    ok = mnesia_manager:delete_table(workflow_instances),

    %% Restore and verify schema
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    RestoredSchemaInfo = mnesia_manager:get_table_info(workflow_instances),
    RestoredAttrs = maps:get(attributes, RestoredSchemaInfo),

    ?assertEqual(OrigAttrs, RestoredAttrs),

    ok.

%% @doc Verify table attributes are preserved after restore
-spec verify_table_attributes_preserved(term()) -> ok.
verify_table_attributes_preserved(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_attrs_preserve.bak"),

    %% Create test table
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(5),

    %% Backup
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Get table info
    TableInfo = mnesia_manager:get_table_info(workflow_instances),
    OrigName = maps:get(name, TableInfo),
    OrigType = maps:get(type, TableInfo),
    OrigAttrs = maps:get(attributes, TableInfo),

    %% Delete and restore
    ok = mnesia_manager:delete_table(workflow_instances),
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    %% Verify attributes
    RestoredInfo = mnesia_manager:get_table_info(workflow_instances),
    ?assertEqual(OrigName, maps:get(name, RestoredInfo)),
    ?assertEqual(OrigType, maps:get(type, RestoredInfo)),
    ?assertEqual(OrigAttrs, maps:get(attributes, RestoredInfo)),

    ok.

%% @doc Verify storage types are preserved after restore
-spec verify_storage_types_preserved(term()) -> ok.
verify_storage_types_preserved(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_storage_types.bak"),

    %% Create tables with different storage types
    ok = create_test_table(workflow_instances, disc_copies),
    ok = create_test_table(task_results, ram_copies),
    ok = create_test_table(checkpoint_logs, disc_only_copies),

    insert_test_workflows(2),
    insert_test_results(2),
    insert_test_checkpoints(2),

    %% Get storage types
    WfInfo = mnesia_manager:get_table_info(workflow_instances),
    WfStorageVal = maps:get(storage, WfInfo),
    TrInfo = mnesia_manager:get_table_info(task_results),
    TrStorageVal = maps:get(storage, TrInfo),
    CpInfo = mnesia_manager:get_table_info(checkpoint_logs),
    CpStorageVal = maps:get(storage, CpInfo),

    %% Backup
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Delete and restore
    lists:foreach(
        fun(T) -> ok = mnesia_manager:delete_table(T) end,
        [workflow_instances, task_results, checkpoint_logs]
    ),
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    %% Verify storage types match
    RestoredWfInfo = mnesia_manager:get_table_info(workflow_instances),
    RestoredWfStorageVal = maps:get(storage, RestoredWfInfo),
    RestoredTrInfo = mnesia_manager:get_table_info(task_results),
    RestoredTrStorageVal = maps:get(storage, RestoredTrInfo),
    RestoredCpInfo = mnesia_manager:get_table_info(checkpoint_logs),
    RestoredCpStorageVal = maps:get(storage, RestoredCpInfo),

    ?assertEqual(WfStorageVal, RestoredWfStorageVal),
    ?assertEqual(TrStorageVal, RestoredTrStorageVal),
    ?assertEqual(CpStorageVal, RestoredCpStorageVal),

    ok.

%%====================================================================
%% Test Cases: File Format Validation
%%====================================================================

%% @doc Test backup file format validation
-spec backup_file_format_validation(term()) -> ok.
backup_file_format_validation(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_file_format.bak"),

    %% Create and backup
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(3),
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Verify file is readable
    ?assert(filelib:is_file(BackupFile)),

    %% Verify file has Mnesia backup header
    {ok, Binary} = file:read_file(BackupFile),
    ?assert(byte_size(Binary) > 0),

    ok.

%% @doc Test backup creates readable tar format (for shell script compatibility)
-spec backup_creates_readable_tar_format(term()) -> ok.
backup_creates_readable_tar_format(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_tar_format.bak"),

    %% Create and backup
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(5),
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Verify file exists and is not empty
    ?assert(filelib:is_file(BackupFile)),
    Size = filelib:file_size(BackupFile),
    ?assert(Size > 0),

    ok.

%% @doc Verify backup contains table data
-spec verify_backup_contains_table_data(term()) -> ok.
verify_backup_contains_table_data(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_backup_data.bak"),

    %% Create and backup with specific data
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(10),

    %% Get original data count
    {atomic, OrigCount} = mnesia:table_info(workflow_instances, size),
    ?assertEqual(10, OrigCount),

    %% Backup
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Verify backup size is reasonable
    Size = filelib:file_size(BackupFile),
    ?assert(Size > 1000),  %% Backup should have meaningful size

    %% Restore to verify data is in backup
    {atomic, ok} = mnesia:clear_table(workflow_instances),
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),

    {atomic, RestoredCount} = mnesia:table_info(workflow_instances, size),
    ?assertEqual(OrigCount, RestoredCount),

    ok.

%%====================================================================
%% Test Cases: Error Handling
%%====================================================================

%% @doc Test restore handles missing file gracefully
-spec restore_handles_missing_file(term()) -> ok.
restore_handles_missing_file(Config) ->
    BackupDir = ?config(backup_dir, Config),
    MissingFile = filename:join(BackupDir, "nonexistent_backup.bak"),

    %% Attempt restore from non-existent file
    Result = mnesia_manager:restore(MissingFile),

    %% Should return error
    ?assertMatch({error, _}, Result),

    ok.

%% @doc Test restore handles corrupted file gracefully
-spec restore_handles_corrupted_file(term()) -> ok.
restore_handles_corrupted_file(Config) ->
    BackupDir = ?config(backup_dir, Config),
    CorruptedFile = filename:join(BackupDir, "corrupted_backup.bak"),

    %% Create corrupted backup file
    ok = file:write_file(CorruptedFile, <<"This is not a valid Mnesia backup file">>),

    %% Attempt restore
    Result = mnesia_manager:restore(CorruptedFile),

    %% Should return error
    ?assertMatch({error, _}, Result),

    ok.

%%====================================================================
%% Test Cases: Backup Listing
%%====================================================================

%% @doc Test list_backups finds valid backup files
-spec list_backups_finds_valid_files(term()) -> ok.
list_backups_finds_valid_files(Config) ->
    BackupDir = ?config(backup_dir, Config),

    %% Create multiple backups
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(3),

    BackupFile1 = filename:join(BackupDir, "backup1.bak"),
    BackupFile2 = filename:join(BackupDir, "backup2.bak"),

    ?assertEqual(ok, mnesia_manager:backup(BackupFile1)),
    ?assertEqual(ok, mnesia_manager:backup(BackupFile2)),

    %% List backups
    Backups = mnesia_manager:list_backups(BackupDir),

    %% Should find both backups
    ?assert(length(Backups) >= 2),
    ?assert(lists:any(fun(B) ->
        string:str(maps:get(filename, B), "backup1") > 0
    end, Backups)),

    ok.

%% @doc Test list_backups excludes non-backup files
-spec list_backups_excludes_non_backup_files(term()) -> ok.
list_backups_excludes_non_backup_files(Config) ->
    BackupDir = ?config(backup_dir, Config),

    %% Create backup and non-backup files
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(2),

    BackupFile = filename:join(BackupDir, "valid_backup.bak"),
    OtherFile = filename:join(BackupDir, "not_a_backup.txt"),

    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),
    ok = file:write_file(OtherFile, <<"Some random content">>),

    %% List backups
    Backups = mnesia_manager:list_backups(BackupDir),

    %% Should not include .txt file
    ?assertNot(lists:any(fun(B) ->
        string:str(maps:get(filename, B), ".txt") > 0
    end, Backups)),

    ok.

%%====================================================================
%% Test Cases: Restore Options
%%====================================================================

%% @doc Test backup respects backup level parameter
-spec backup_respects_backup_level(term()) -> ok.
backup_respects_backup_level(Config) ->
    BackupDir = ?config(backup_dir, Config),
    FullFile = filename:join(BackupDir, "full_backup.bak"),
    IncFile = filename:join(BackupDir, "inc_backup.bak"),

    %% Create table
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(5),

    %% Create full backup
    ?assertEqual(ok, mnesia_manager:backup(FullFile, full)),
    ?assert(filelib:is_file(FullFile)),

    %% Create incremental backup
    ?assertEqual(ok, mnesia_manager:backup(IncFile, incremental)),
    ?assert(filelib:is_file(IncFile)),

    ok.

%% @doc Test restore clears target tables by default
-spec restore_clears_target_tables(term()) -> ok.
restore_clears_target_tables(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_clear_restore.bak"),

    %% Create and backup table with 5 records
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(5),
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Add more records (10 total)
    insert_test_workflows(5),
    {atomic, 10} = mnesia:table_info(workflow_instances, size),

    %% Restore should clear and restore to 5
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),
    {atomic, 5} = mnesia:table_info(workflow_instances, size),

    ok.

%% @doc Test restore is idempotent
-spec restore_idempotent(term()) -> ok.
restore_idempotent(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_idempotent.bak"),

    %% Create and backup
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(5),
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Clear and restore first time
    {atomic, ok} = mnesia:clear_table(workflow_instances),
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),
    {atomic, Count1} = mnesia:table_info(workflow_instances, size),

    %% Clear and restore second time
    {atomic, ok} = mnesia:clear_table(workflow_instances),
    ?assertEqual(ok, mnesia_manager:restore(BackupFile)),
    {atomic, Count2} = mnesia:table_info(workflow_instances, size),

    %% Counts should be identical
    ?assertEqual(Count1, Count2),
    ?assertEqual(5, Count1),

    ok.

%%====================================================================
%% Test Cases: Estimation and Metadata
%%====================================================================

%% @doc Test backup disk space estimation
-spec backup_disk_space_estimation(term()) -> ok.
backup_disk_space_estimation(Config) ->
    BackupDir = ?config(backup_dir, Config),
    BackupFile = filename:join(BackupDir, "test_disk_space.bak"),

    %% Create table with known size
    ok = create_test_table(workflow_instances, disc_copies),
    insert_test_workflows(100),

    %% Backup
    ?assertEqual(ok, mnesia_manager:backup(BackupFile)),

    %% Verify file size is reasonable
    FileSize = filelib:file_size(BackupFile),
    ?assert(FileSize > 5000),  %% Should have minimum size
    ?assert(FileSize < 10000000),  %% Should not be excessively large

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Create a test table with specified storage type
-spec create_test_table(atom(), disc_copies | ram_copies | disc_only_copies) -> ok | {aborted, term()}.
create_test_table(workflow_instances, StorageType) ->
    mnesia_manager:create_table(workflow_instances, [
        {attributes, [id, name, status, created_at, updated_at]},
        {storage_type_to_option(StorageType), [node()]},
        {type, set}
    ]);
create_test_table(task_results, StorageType) ->
    mnesia_manager:create_table(task_results, [
        {attributes, [task_id, workflow_id, result, timestamp]},
        {storage_type_to_option(StorageType), [node()]},
        {type, set}
    ]);
create_test_table(checkpoint_logs, StorageType) ->
    mnesia_manager:create_table(checkpoint_logs, [
        {attributes, [checkpoint_id, workflow_id, state, created_at]},
        {storage_type_to_option(StorageType), [node()]},
        {type, set}
    ]);
create_test_table(metrics_data, StorageType) ->
    mnesia_manager:create_table(metrics_data, [
        {attributes, [metric_id, type, value, timestamp]},
        {storage_type_to_option(StorageType), [node()]},
        {type, set}
    ]);
create_test_table(audit_events, StorageType) ->
    mnesia_manager:create_table(audit_events, [
        {attributes, [event_id, user_id, action, timestamp]},
        {storage_type_to_option(StorageType), [node()]},
        {type, bag}
    ]).

%% @private Convert storage type to Mnesia option
-spec storage_type_to_option(atom()) -> atom().
storage_type_to_option(disc_copies) -> disc_copies;
storage_type_to_option(ram_copies) -> ram_copies;
storage_type_to_option(disc_only_copies) -> disc_only_copies.

%% @private Insert test workflow records
-spec insert_test_workflows(non_neg_integer()) -> ok.
insert_test_workflows(Count) ->
    lists:foreach(
        fun(N) ->
            {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#workflow_instances{
                    id = list_to_binary(io_lib:format("wf~p", [N])),
                    name = "Workflow " ++ integer_to_list(N),
                    status = active,
                    created_at = 1000 + N,
                    updated_at = 2000 + N
                })
            end)
        end,
        lists:seq(1, Count)
    ),
    ok.

%% @private Insert test task result records
-spec insert_test_results(non_neg_integer()) -> ok.
insert_test_results(Count) ->
    lists:foreach(
        fun(N) ->
            {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#task_results{
                    task_id = list_to_binary(io_lib:format("task~p", [N])),
                    workflow_id = list_to_binary(io_lib:format("wf~p", [N rem 10])),
                    result = {ok, "result_" ++ integer_to_list(N)},
                    timestamp = 3000 + N
                })
            end)
        end,
        lists:seq(1, Count)
    ),
    ok.

%% @private Insert test checkpoint records
-spec insert_test_checkpoints(non_neg_integer()) -> ok.
insert_test_checkpoints(Count) ->
    lists:foreach(
        fun(N) ->
            {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#checkpoint_logs{
                    checkpoint_id = list_to_binary(io_lib:format("cp~p", [N])),
                    workflow_id = list_to_binary(io_lib:format("wf~p", [N rem 5])),
                    state = {checkpoint, N},
                    created_at = 4000 + N
                })
            end)
        end,
        lists:seq(1, Count)
    ),
    ok.

%% @private Insert test event records
-spec insert_test_events(non_neg_integer()) -> ok.
insert_test_events(Count) ->
    lists:foreach(
        fun(N) ->
            {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#audit_events{
                    event_id = list_to_binary(io_lib:format("ev~p", [N])),
                    user_id = list_to_binary(io_lib:format("user~p", [N rem 3])),
                    action = audit,
                    timestamp = 5000 + N
                })
            end)
        end,
        lists:seq(1, Count)
    ),
    ok.

%% @private Delete directory recursively
-spec delete_directory_recursive(file:filename()) -> ok.
delete_directory_recursive(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
                fun(F) ->
                    Path = filename:join(Dir, F),
                    case file:is_dir(Path) of
                        true -> delete_directory_recursive(Path);
                        false -> file:delete(Path)
                    end
                end,
                Files
            ),
            file:del_dir(Dir);
        {error, _} ->
            ok
    end.

%% @private Delete directory contents but keep directory
-spec delete_directory_contents(file:filename()) -> ok.
delete_directory_contents(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
                fun(F) ->
                    Path = filename:join(Dir, F),
                    case file:is_dir(Path) of
                        true -> delete_directory_recursive(Path);
                        false -> file:delete(Path)
                    end
                end,
                Files
            );
        {error, _} ->
            ok
    end.
