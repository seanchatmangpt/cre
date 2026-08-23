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
%% @doc Mnesia Mock Factories
%%
%% This module provides mock utilities for Mnesia database operations
%% in testing. It creates in-memory test databases and provides
%% helper functions for setup and teardown.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>In-memory test database setup</li>
%%   <li>Table creation helpers</li>
%%   <li>Mock Mnesia operations without real database</li>
%%   <li>Clean teardown helpers</li>
%%   <li>Transaction mock factories</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% Setup a test database:
%% ```erlang
%% > ok = mnesia_mocks:setup_db(),
%% > ok = mnesia_mocks:init_tables([my_table]),
%% > ... run tests ...
%% > ok = mnesia_mocks:teardown_db().
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(mnesia_mocks).

%%====================================================================
%% Exports
%%====================================================================

%% Database setup/teardown
-export([setup_db/0, setup_db/1]).
-export([teardown_db/0]).
-export([init_tables/1, init_tables/2]).
-export([clear_tables/1]).

%% Table helpers
-export([create_table/2, create_table/3]).
-export([wait_for_tables/1, wait_for_tables/2]).

%% Mock operations (in-memory only)
-export([mock_write/2]).
-export([mock_read/2]).
-export([mock_delete/2]).
-export([mock_fold/3]).

%% Transaction helpers
-export([transaction/1]).
-export([dirty_write/2, dirty_read/2, dirty_delete/2]).

%% Utility functions
-export([is_table/1]).
-export([table_info/2]).
-export([all_keys/1]).
-export([select/2]).

%%====================================================================
%% Types
%%====================================================================

-type table() :: atom().
%% -type table_def() :: {table(), [tuple()]}.  % Unused, commented out
-type key() :: term().
-type record() :: tuple().
-type match_pattern() :: tuple().

%%====================================================================
%% Database Setup/Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Sets up an in-memory Mnesia database for testing.
%%
%% Stops any existing Mnesia and creates a new in-memory schema.
%%
%% @end
%%--------------------------------------------------------------------
-spec setup_db() -> ok | {error, term()}.

setup_db() ->
    setup_db([]).

%%--------------------------------------------------------------------
%% @doc Sets up an in-memory Mnesia database with options.
%%
%% Options:
%% - `{dir, file:filename()}` - Custom directory (for disc_copies tables)
%% - `{extra_nodes, [node()]}` - Additional nodes to include
%%
%% @end
%%--------------------------------------------------------------------
-spec setup_db([{atom(), term()}]) -> ok | {error, term()}.

setup_db(Options) ->
    %% Stop any existing Mnesia application
    application:stop(mnesia),

    %% Delete any existing schema
    mnesia:delete_schema([node()]),

    %% Create new schema with options
    Dir = proplists:get_value(dir, Options, undefined),
    ExtraNodes = proplists:get_value(extra_nodes, Options, []),

    SchemaArgs = [node() | ExtraNodes],

    CreateResult = case Dir of
        undefined ->
            mnesia:create_schema(SchemaArgs);
        _ ->
            mnesia:create_schema(SchemaArgs, [{dir, Dir}])
    end,

    case CreateResult of
        ok ->
            %% Start Mnesia
            case application:start(mnesia) of
                ok -> ok;
                {error, {already_started, mnesia}} -> ok;
                Error -> Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Tears down the test database.
%%
%% Stops Mnesia and cleans up.
%%
%% @end
%%--------------------------------------------------------------------
-spec teardown_db() -> ok.

teardown_db() ->
    %% Stop Mnesia
    application:stop(mnesia),

    %% Delete schema
    mnesia:delete_schema([node()]),

    ok.

%%====================================================================
%% Table Initialization
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes tables from a list of table definitions.
%%
%% TableDef is `{TableName, Attributes}` where Attributes is a list
%% of atoms defining the record fields.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_tables([{table(), [atom()]}]) -> ok | {error, term()}.

init_tables(TableDefs) ->
    init_tables(TableDefs, []).

%%--------------------------------------------------------------------
%% @doc Initializes tables with options.
%%
%% Options:
%% - `{type, set | ordered_set | bag_type}` - Table type (default: set)
%% - `{disc_copies, boolean()}` - Use disc_copies instead of ram_copies
%%
%% @end
%%--------------------------------------------------------------------
-spec init_tables([{table(), [atom()]}], [{atom(), term()}]) ->
          ok | {error, term()}.

init_tables(TableDefs, Options) ->
    Type = proplists:get_value(type, Options, set),
    UseDisc = proplists:get_value(disc_copies, Options, false),

    %% Create tables directly without wrapping in transaction
    Results = lists:map(fun({Table, Attributes}) ->
        case is_table(Table) of
            true ->
                {atomic, ok};
            false ->
                TableType = case Type of
                    bag_type -> bag;
                    ordered_set -> ordered_set;
                    _ -> set
                end,
                CopyType = case UseDisc of
                    true -> disc_copies;
                    false -> ram_copies
                end,
                mnesia:create_table(Table, [
                    {attributes, Attributes},
                    {type, TableType},
                    {CopyType, [node()]}
                ])
        end
    end, TableDefs),

    case lists:all(fun(R) -> R =:= ok orelse R =:= {atomic, ok} orelse
                             R =:= {atomic, already_exists} end, Results) of
        true -> ok;
        false -> {error, {create_failed, Results}}
    end.

%%--------------------------------------------------------------------
%% @doc Clears all data from specified tables.
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_tables([table()]) -> ok | {error, term()}.

clear_tables(Tables) ->
    ClearFuns = lists:map(fun(Table) ->
        fun() -> mnesia:clear_table(Table) end
    end, Tables),

    Results = [mnesia:transaction(F) || F <- ClearFuns],

    case lists:all(fun(R) -> R =:= ok orelse R =:= {atomic, ok} end, Results) of
        true -> ok;
        false -> {error, {clear_failed, Results}}
    end.

%%====================================================================
%% Table Creation Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a table with attributes (transaction-wrapped).
%%
%% @end
%%--------------------------------------------------------------------
-spec create_table(table(), [atom()]) -> ok | {error, term()}.

create_table(Table, Attributes) ->
    create_table(Table, Attributes, []).

%%--------------------------------------------------------------------
%% @doc Creates a table with attributes and options.
%%
%% Options:
%% - `{type, set | ordered_set | bag_type}` - Table type
%% - `{disc_copies, boolean()}` - Use disc_copies
%%
%% @end
%%--------------------------------------------------------------------
-spec create_table(table(), [atom()], [{atom(), term()}]) ->
          ok | {error, term()}.

create_table(Table, Attributes, Options) ->
    Type = proplists:get_value(type, Options, set),
    UseDisc = proplists:get_value(disc_copies, Options, false),

    TableType = case Type of
        bag_type -> bag;
        ordered_set -> ordered_set;
        _ -> set
    end,

    CopyType = case UseDisc of
        true -> disc_copies;
        false -> ram_copies
    end,

    Result = mnesia:create_table(Table, [
        {attributes, Attributes},
        {type, TableType},
        {CopyType, [node()]}
    ]),

    case Result of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Waits for tables to be ready.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_tables([table()]) -> ok | {error, term()}.

wait_for_tables(Tables) ->
    wait_for_tables(Tables, 5000).

%%--------------------------------------------------------------------
%% @doc Waits for tables with timeout.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_tables([table()], timeout()) -> ok | {error, term()}.

wait_for_tables(Tables, Timeout) ->
    case mnesia:wait_for_tables(Tables, Timeout) of
        ok -> ok;
        {timeout, BadTables} -> {error, {timeout, BadTables}};
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Mock Operations (In-Memory)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Writes a record to a table within a transaction.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_write(table(), record()) -> ok | {error, term()}.

mock_write(Table, Record) ->
    case mnesia:transaction(fun() -> mnesia:write(Table, Record, write) end) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Reads a record from a table by key within a transaction.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_read(table(), key()) -> {ok, record()} | {error, not_found}.

mock_read(Table, Key) ->
    case mnesia:transaction(fun() -> mnesia:read(Table, Key) end) of
        {atomic, []} -> {error, not_found};
        {atomic, [Record]} -> {ok, Record};
        {atomic, Records} when is_list(Records) -> {ok, hd(Records)};
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a record from a table by key within a transaction.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_delete(table(), key()) -> ok | {error, term()}.

mock_delete(Table, Key) ->
    case mnesia:transaction(fun() -> mnesia:delete({Table, Key}) end) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Folds over all records in a table.
%%
%% Fun receives `(Record, Acc)` and returns `NewAcc`.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_fold(table(), fun((record(), term()) -> term()), term()) ->
          {ok, term()} | {error, term()}.

mock_fold(Table, Fun, Acc0) ->
    case mnesia:transaction(fun() ->
        mnesia:foldl(Fun, Acc0, Table)
    end) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Transaction Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a fun within a Mnesia transaction.
%%
%% @end
%%--------------------------------------------------------------------
-spec transaction(fun(() -> term())) -> {ok, term()} | {error, term()}.

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Writes a record without transaction (dirty operation).
%%
%% @end
%%--------------------------------------------------------------------
-spec dirty_write(table(), record()) -> ok.

dirty_write(Table, Record) ->
    mnesia:dirty_write(Table, Record),
    ok.

%%--------------------------------------------------------------------
%% @doc Reads a record without transaction (dirty operation).
%%
%% @end
%%--------------------------------------------------------------------
-spec dirty_read(table(), key()) -> {ok, record()} | {error, not_found}.

dirty_read(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [] -> {error, not_found};
        [Record] -> {ok, Record};
        Records when is_list(Records) -> {ok, hd(Records)}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a record without transaction (dirty operation).
%%
%% @end
%%--------------------------------------------------------------------
-spec dirty_delete(table(), key()) -> ok.

dirty_delete(Table, Key) ->
    mnesia:dirty_delete({Table, Key}),
    ok.

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a table exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_table(table()) -> boolean().

is_table(Table) ->
    lists:member(Table, mnesia:system_info(tables)).

%%--------------------------------------------------------------------
%% @doc Gets table information.
%%
%% Returns `{ok, Info}` or `{error, not_found}`.
%%
%% @end
%%--------------------------------------------------------------------
-spec table_info(table(), atom()) -> {ok, term()} | {error, not_found}.

table_info(Table, Item) ->
    try
        {ok, mnesia:table_info(Table, Item)}
    catch
        error:{aborted, _} -> {error, not_found};
        _:_ -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Gets all keys from a table.
%%
%% @end
%%--------------------------------------------------------------------
-spec all_keys(table()) -> {ok, [key()]} | {error, term()}.

all_keys(Table) ->
    try
        Keys = mnesia:dirty_all_keys(Table),
        {ok, Keys}
    catch
        _:_ -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Selects records matching a pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec select(table(), match_pattern()) -> {ok, [record()]} | {error, term()}.

select(Table, Pattern) ->
    try
        Result = mnesia:dirty_select(Table, [{Pattern, [], ['$_']}]),
        {ok, Result}
    catch
        _:_ -> {error, not_found}
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Setup/teardown for Mnesia tests
mnesia_setup() ->
    case setup_db() of
        ok -> ok;
        {error, {already_exists, _}} ->
            %% Schema already exists from a previous test, clean it first
            teardown_db(),
            setup_db()
    end.

mnesia_cleanup(_) ->
    teardown_db().

%% Test setup_db/0
setup_db_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_assertEqual(ok, teardown_db())]
     end}.

%% Test init_tables/1
init_tables_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         TableDefs = [
             {test_table1, [id, name, value]},
             {test_table2, [key, data]}
         ],
         [?_assertEqual(ok, init_tables(TableDefs)),
          ?_assert(is_table(test_table1)),
          ?_assert(is_table(test_table2))]
     end}.

%% Test create_table/2
create_table_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_assertEqual(ok, create_table(my_table, [id, data])),
          ?_assert(is_table(my_table))]
     end}.

%% Test wait_for_tables/1
wait_for_tables_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_assertEqual(ok, create_table(t1, [id])),
          ?_assertEqual(ok, wait_for_tables([t1]))]
     end}.

%% Test mock_write/2 and mock_read/2
mock_write_read_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(wr_table, [id, value])),
              Record = {wr_table, 1, <<"test">>},
              ?assertEqual(ok, mock_write(wr_table, Record)),
              ?assertEqual({ok, Record}, mock_read(wr_table, 1)),
              ?assertEqual({error, not_found}, mock_read(wr_table, 999))
           end)]
     end}.

%% Test mock_delete/2
mock_delete_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(del_table, [id, value])),
              Record = {del_table, 1, <<"test">>},
              ?assertEqual(ok, mock_write(del_table, Record)),
              ?assertEqual({ok, Record}, mock_read(del_table, 1)),
              ?assertEqual(ok, mock_delete(del_table, 1)),
              ?assertEqual({error, not_found}, mock_read(del_table, 1))
           end)]
     end}.

%% Test mock_fold/3
mock_fold_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(fold_table, [id, value])),
              ?assertEqual(ok, mock_write(fold_table, {fold_table, 1, <<"a">>})),
              ?assertEqual(ok, mock_write(fold_table, {fold_table, 2, <<"b">>})),
              ?assertEqual(ok, mock_write(fold_table, {fold_table, 3, <<"c">>})),
              {ok, Sum} = mock_fold(fold_table, fun({_T, _K, V}, Acc) ->
                  case V of
                      <<"a">> -> Acc + 1;
                      <<"b">> -> Acc + 2;
                      <<"c">> -> Acc + 3
                  end
              end, 0),
              ?assertEqual(6, Sum)
           end)]
     end}.

%% Test transaction/1
transaction_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(tx_table, [id, value])),
              ?assertEqual({ok, ok}, transaction(fun() ->
                  mnesia:write({tx_table, 1, <<"tx_test">>})
              end)),
              ?assertEqual({ok, {tx_table, 1, <<"tx_test">>}},
                           transaction(fun() -> mnesia:read({tx_table, 1}) end))
           end)]
     end}.

%% Test dirty_write/2 and dirty_read/2
dirty_ops_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(dirty_table, [id, value])),
              Record = {dirty_table, 1, <<"dirty">>},
              ?assertEqual(ok, dirty_write(dirty_table, Record)),
              ?assertEqual({ok, Record}, dirty_read(dirty_table, 1)),
              ?assertEqual({error, not_found}, dirty_read(dirty_table, 999)),
              ?assertEqual(ok, dirty_delete(dirty_table, 1)),
              ?assertEqual({error, not_found}, dirty_read(dirty_table, 1))
           end)]
     end}.

%% Test table_info/2
table_info_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(info_table, [id, value])),
              ?assertEqual({ok, [id, value]}, table_info(info_table, attributes)),
              ?assertEqual({error, not_found}, table_info(nonexistent, attributes))
           end)]
     end}.

%% Test all_keys/1
all_keys_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(keys_table, [id, value])),
              ?assertEqual(ok, mock_write(keys_table, {keys_table, 1, <<"a">>})),
              ?assertEqual(ok, mock_write(keys_table, {keys_table, 2, <<"b">>})),
              {ok, Keys} = all_keys(keys_table),
              ?assertEqual(2, length(Keys))
           end)]
     end}.

%% Test select/2
select_test_() ->
    {setup,
     fun mnesia_setup/0,
     fun mnesia_cleanup/1,
     fun(_) ->
         [?_test(begin
              ?assertEqual(ok, create_table(sel_table, [id, value])),
              ?assertEqual(ok, mock_write(sel_table, {sel_table, 1, <<"match">>})),
              ?assertEqual(ok, mock_write(sel_table, {sel_table, 2, <<"other">>})),
              {ok, Results} = select(sel_table, {sel_table, '_', '_'}),
              ?assertEqual(2, length(Results))
           end)]
     end}.

-endif.
