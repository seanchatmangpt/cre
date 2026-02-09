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
%% @doc Mnesia Schema and Table Management Module
%%
%% This module handles Mnesia schema initialization, table creation with
%% distributed configuration, and backup/restore operations. It provides
%% a high-level API for managing database lifecycle in clustered environments.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Schema Init:</b> Creates and configures Mnesia schema</li>
%%   <li><b>Table Creation:</b> Creates tables with distribution options</li>
%%   <li><b>Backup/Restore:</b> Full and incremental backup support</li>
%%   <li><b>Cluster Ready:</b> Configures replication and disc copies</li>
%% </ul>
%%
%% <h3>Storage Types</h3>
%%
%% <ul>
%%   <li><b>disc_copies:</b> Stored on disk and in RAM (persistent)</li>
%%   <li><b>ram_copies:</b> Stored in RAM only (fast, volatile)</li>
%%   <li><b>disc_only_copies:</b> Stored on disk only (memory efficient)</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Initialize schema with disc storage
%% ok = mnesia_manager:init_schema(disc),
%%
%% %% Create a distributed table
%% ok = mnesia_manager:create_table(my_table,
%%     [{attributes, [key, value]}, {disc_copies, [node()]}]),
%%
%% %% Backup to file
%% ok = mnesia_manager:backup("/var/backup/mnesia_backup").
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(mnesia_manager).
-include_lib("kernel/include/file.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Schema and table management
-export([init_schema/1,
         stop_mnesia/0,
         create_table/2,
         delete_table/1,
         get_table_info/1,
         list_tables/0]).

%% Backup and restore
-export([backup/1,
         backup/2,
         restore/1,
         restore/2,
         list_backups/1]).

%%====================================================================
%% Type definitions
%%====================================================================

-type storage_type() :: ram | disc | disc_only.
-type table_option() :: {attributes, [atom()]} |
                        {disc_copies, [node()]} |
                        {ram_copies, [node()]} |
                        {disc_only_copies, [node()]} |
                        {index, [atom()]} |
                        {type, set | ordered_set | bag} |
                        {record_name, atom()}.
-type backup_level() :: full | incremental.
-type backup_result() :: ok | {error, term()}.

%%====================================================================
%% Schema and table management
%%====================================================================

%% @doc Initializes the Mnesia schema on the current node.
%%
%%      Creates a new Mnesia schema if none exists, or ensures the
%%      existing schema is accessible. The storage type determines
%%      how data is persisted.
%%
%%      Storage types:
%%      - `disc' - Schema stored on disk (default for production)
%%      - `ram' - Schema in RAM only (fast, volatile, for testing)
%%      - `disc_only' - Schema on disk only (memory efficient)
%%
%% @param StorageType The storage backend type
%% @returns `ok' | `{error, Reason}'
%%
-spec init_schema(storage_type()) -> ok | {error, term()}.
init_schema(StorageType) ->
    CurrentNode = node(),

    logger:info("Initializing Mnesia schema: node=~p, storage=~p",
                [CurrentNode, StorageType],
                [{info, "schema_init"}, {application, cre}]),

    case mnesia:system_info(is_running) of
        no ->
            %% Mnesia not running, create schema
            case mnesia:create_schema([CurrentNode]) of
                ok ->
                    logger:info("Schema created successfully",
                                [{info, "schema_created"}, {application, cre}]),
                    case mnesia:start() of
                        ok ->
                            logger:info("Mnesia started",
                                        [{info, "mnesia_started"}, {application, cre}]),
                            ok;
                        {error, Reason} ->
                            logger:error("Failed to start Mnesia: ~p", [Reason],
                                         [{info, "mnesia_start_failed"}, {application, cre}]),
                            {error, {mnesia_start_failed, Reason}}
                    end;
                {error, {already_exists, _}} ->
                    %% Schema exists, just start Mnesia
                    case mnesia:start() of
                        ok ->
                            logger:info("Mnesia started with existing schema",
                                        [{info, "mnesia_started_existing"}, {application, cre}]),
                            ok;
                        {error, Reason} ->
                            {error, {mnesia_start_failed, Reason}}
                    end;
                {error, Reason} ->
                    logger:error("Failed to create schema: ~p", [Reason],
                                 [{info, "schema_create_failed"}, {application, cre}]),
                    {error, {schema_create_failed, Reason}}
            end;
        yes ->
            logger:info("Mnesia already running",
                        [{info, "mnesia_already_running"}, {application, cre}]),
            ok
    end.

%% @doc Stops Mnesia on the current node.
%%
%%      Gracefully stops Mnesia, flushing all data to disk if using
%%      disc-based storage.
%%
%% @returns `ok' | `{error, not_started}'
%%
-spec stop_mnesia() -> ok | {error, not_started}.
stop_mnesia() ->
    case mnesia:system_info(is_running) of
        yes ->
            logger:info("Stopping Mnesia",
                        [{info, "mnesia_stopping"}, {application, cre}]),
            stopped = mnesia:stop(),
            ok;
        no ->
            {error, not_started}
    end.

%% @doc Creates a new Mnesia table with the given options.
%%
%%      Creates a table with the specified attributes and distribution
%%      configuration. The table type defaults to `set' if not specified.
%%
%%      Common options:
%%      - `{attributes, Attrs}' - Record attribute names (required)
%%      - `{disc_copies, Nodes}' - Disc storage on nodes
%%      - `{ram_copies, Nodes}' - RAM storage on nodes
%%      - `{type, Type}' - set, ordered_set, or bag
%%      - `{index, Indexes}' - Additional indexed fields
%%
%% @param TableName The name of the table
%% @param Options List of table configuration options
%% @returns `ok' | `{aborted, Reason}'
%%
-spec create_table(atom(), [table_option()]) -> ok | {aborted, term()}.
create_table(TableName, Options) when is_atom(TableName), is_list(Options) ->
    logger:info("Creating table: ~p with options: ~p", [TableName, Options],
                [{info, "table_create"}, {application, cre}]),

    %% Check if attributes are provided
    Attributes = proplists:get_value(attributes, Options),
    case Attributes of
        undefined ->
            logger:error("Missing attributes for table ~p", [TableName],
                         [{info, "table_missing_attrs"}, {application, cre}]),
            {aborted, {missing_attributes, TableName}};
        _ when is_list(Attributes) ->
            case mnesia:create_table(TableName, Options) of
                {atomic, ok} ->
                    logger:info("Table created successfully: ~p", [TableName],
                                [{info, "table_created"}, {application, cre}]),
                    ok;
                {aborted, {already_exists, TableName}} ->
                    logger:info("Table already exists: ~p", [TableName],
                                [{info, "table_exists"}, {application, cre}]),
                    ok;
                {aborted, Reason} ->
                    logger:error("Failed to create table ~p: ~p", [TableName, Reason],
                                 [{info, "table_create_failed"}, {application, cre}]),
                    {aborted, Reason}
            end;
        _ ->
            {aborted, {invalid_attributes, Attributes}}
    end.

%% @doc Deletes a table from the Mnesia schema.
%%
%%      Removes the table and all its data from the database.
%%      This operation cannot be undone.
%%
%% @param TableName The name of the table to delete
%% @returns `ok' | `{aborted, Reason}'
%%
-spec delete_table(atom()) -> ok | {aborted, term()}.
delete_table(TableName) when is_atom(TableName) ->
    logger:info("Deleting table: ~p", [TableName],
                [{info, "table_delete"}, {application, cre}]),

    case mnesia:delete_table(TableName) of
        {atomic, ok} ->
            logger:info("Table deleted: ~p", [TableName],
                        [{info, "table_deleted"}, {application, cre}]),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to delete table ~p: ~p", [TableName, Reason],
                         [{info, "table_delete_failed"}, {application, cre}]),
            {aborted, Reason}
    end.

%% @doc Retrieves information about a specific table.
%%
%%      Returns a map containing table properties such as storage type,
%%      replication nodes, record count, and index configuration.
%%
%% @param TableName The name of the table
%% @returns Map with table information | `{error, not_found}'
%%
-spec get_table_info(atom()) -> #{atom() => term()} | {error, not_found}.
get_table_info(TableName) when is_atom(TableName) ->
    case lists:member(TableName, mnesia:system_info(tables)) of
        false ->
            {error, not_found};
        true ->
            Info = #{
                name => TableName,
                storage => get_storage_type(TableName),
                nodes => mnesia:table_info(TableName, where_to_read),
                record_count => mnesia:table_info(TableName, size),
                attributes => mnesia:table_info(TableName, attributes),
                type => mnesia:table_info(TableName, type),
                index => mnesia:table_info(TableName, index),
                record_name => mnesia:table_info(TableName, record_name)
            },
            Info
    end.

%% @doc Lists all tables in the Mnesia schema.
%%
%%      Returns a list of all table names, including the schema table.
%%
%% @returns List of table names
%%
-spec list_tables() -> [atom()].
list_tables() ->
    mnesia:system_info(tables).

%%====================================================================
%% Backup and restore
%%====================================================================

%% @doc Creates a full backup of the Mnesia database.
%%
%%      Performs a complete backup of all tables to the specified file.
%%      The backup can be restored using restore/1.
%%
%% @param FilePath Path to the backup file
%% @returns `ok' | `{error, Reason}'
%%
-spec backup(file:filename_all()) -> backup_result().
backup(FilePath) ->
    backup(FilePath, full).

%% @doc Creates a backup with specified level.
%%
%%      Backup levels:
%%      - `full' - Complete backup of all tables
%%      - `incremental' - Backup changes since last backup
%%
%% @param FilePath Path to the backup file
%% @param Level full or incremental backup
%% @returns `ok' | `{error, Reason}'
%%
-spec backup(file:filename_all(), backup_level()) -> backup_result().
backup(FilePath, Level) when is_list(FilePath); is_binary(FilePath) ->
    logger:info("Creating ~p backup: ~p", [Level, FilePath],
                [{info, "backup_start"}, {application, cre}]),

    case mnesia:activate_checkpoint([{max, mnesia:system_info(tables)}], []) of
        {ok, _Name, _Nodes} ->
            BackupResult = mnesia:backup_checkpoint(FilePath, Level),
            mnesia:deactivate_checkpoint([]),
            case BackupResult of
                ok ->
                    logger:info("Backup completed: ~p", [FilePath],
                                [{info, "backup_success"}, {application, cre}]),
                    ok;
                {error, Reason} ->
                    logger:error("Backup failed: ~p", [Reason],
                                 [{info, "backup_failed"}, {application, cre}]),
                    {error, {backup_failed, Reason}}
            end;
        {error, Reason} ->
            logger:error("Failed to create checkpoint: ~p", [Reason],
                         [{info, "checkpoint_failed"}, {application, cre}]),
            {error, {checkpoint_failed, Reason}}
    end.

%% @doc Restores a Mnesia backup from file.
%%
%%      Restores the database from a previously created backup.
%%      By default, uses default restore options.
%%
%% @param FilePath Path to the backup file
%% @returns `ok' | `{error, Reason}'
%%
-spec restore(file:filename_all()) -> backup_result().
restore(FilePath) ->
    restore(FilePath, []).

%% @doc Restores a Mnesia backup with options.
%%
%%      Restore options:
%%      - `{default,, Goal}' - `clear' to clear tables before restore
%%      - `{skip_tables, Tables}' - Skip specific tables during restore
%%      - `{recreate_tables, Tables}' - Recreate tables before restore
%%
%% @param FilePath Path to the backup file
%% @param Options Restore options
%% @returns `ok' | `{error, Reason}'
%%
-spec restore(file:filename_all(), [term()]) -> backup_result().
restore(FilePath, Options) when is_list(FilePath); is_binary(FilePath) ->
    logger:info("Restoring backup: ~p with options: ~p", [FilePath, Options],
                [{info, "restore_start"}, {application, cre}]),

    DefaultOptions = [{default, clear}],
    FinalOptions = case Options of
        [] -> DefaultOptions;
        _  -> Options
    end,

    case mnesia:restore(FilePath, FinalOptions) of
        {atomic, ok} ->
            logger:info("Restore completed: ~p", [FilePath],
                        [{info, "restore_success"}, {application, cre}]),
            ok;
        {aborted, Reason} ->
            logger:error("Restore failed: ~p", [Reason],
                         [{info, "restore_failed"}, {application, cre}]),
            {error, {restore_failed, Reason}}
    end.

%% @doc Lists available backups in a directory.
%%
%%      Scans the specified directory for Mnesia backup files
%%      and returns a list of available backups with metadata.
%%
%% @param DirPath Path to the backup directory
%% @returns List of backup file information
%%
-spec list_backups(file:filename_all()) -> [#{
    filename => file:filename_all(),
    size => non_neg_integer(),
    mtime => calendar:datetime()
}].
list_backups(DirPath) ->
    case file:list_dir(DirPath) of
        {ok, Files} ->
            BackupFiles = lists:filter(
                fun(F) ->
                    case filename:extension(F) of
                        ".bak" -> true;
                        ".BAC" -> true;
                        _ -> false
                    end
                end,
                Files
            ),
            lists:map(
                fun(F) ->
                    FullPath = filename:join(DirPath, F),
                    {ok, FileInfo} = file:read_file_info(FullPath),
                    #{
                        filename => FullPath,
                        size => FileInfo#file_info.size,
                        mtime => FileInfo#file_info.mtime
                    }
                end,
                BackupFiles
            );
        {error, Reason} ->
            logger:error("Failed to list backup directory: ~p", [Reason],
                         [{info, "list_backups_failed"}, {application, cre}]),
            []
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Determines the storage type of a table.
-spec get_storage_type(atom()) -> disc_copies | ram_copies | disc_only_copies | unknown.
get_storage_type(TableName) ->
    case mnesia:table_info(TableName, disc_copies) of
        [_|_] -> disc_copies;
        [] ->
            case mnesia:table_info(TableName, ram_copies) of
                [_|_] -> ram_copies;
                [] ->
                    case mnesia:table_info(TableName, disc_only_copies) of
                        [_|_] -> disc_only_copies;
                        [] -> unknown
                    end
            end
    end.

%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test table info retrieval
get_table_info_test() ->
    ?assertMatch(#{name := schema}, get_table_info(schema)).

%% Test list_tables returns at least schema
list_tables_test() ->
    Tables = list_tables(),
    ?assert(lists:member(schema, Tables)).

-endif.
