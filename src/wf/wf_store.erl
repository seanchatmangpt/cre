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
%% @doc Workflow Store Module
%%
%% This module provides Mnesia-backed persistence for workflow instances
%% and work items. It supports both disk-backed tables (disc_copies) for
%% production and in-memory tables (ram_copies) for testing with temporary
%% directories.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Mnesia table schema initialization with disc_copies or ram_copies</li>
%%   <li>Workflow instance persistence (create, read, update)</li>
%%   <li>Work item persistence with query support</li>
%%   <li>Transaction support for atomic operations</li>
%%   <li>Temporary in-memory stores for testing</li>
%% </ul>
%%
%% <h3>Mnesia Tables</h3>
%% <ul>
%%   <li><b>wf_store_instance:</b> Workflow instance records</li>
%%   <li><b>wf_store_workitem:</b> Work item records</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Opening a store and storing instances:
%%
%% ```erlang
%% 1> {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}).
%% 2> Id = <<"wf-1">>.
%% 3> ok = wf_store:put_instance(Store, Id, #{version => 0, status => running}, undefined).
%% ok
%% 4> {ok, I0} = wf_store:get_instance(Store, Id).
%% 5> maps:get(version, I0).
%% 0
%% 6> wf_store:get_instance(Store, <<"missing">>).
%% not_found
%% '''
%%
%% Using transactions:
%%
%% ```erlang
%% 1> {atomic, ok} =
%% 1> wf_store:tx(Store, fun() ->
%% 1>     {ok, I1} = wf_store:get_instance(Store, Id),
%% 1>     ok = wf_store:put_instance(Store, Id, I1#{version => 1}, undefined),
%% 1>     ok
%% 1> end).
%% _
%% 2> {ok, I2} = wf_store:get_instance(Store, Id).
%% 3> maps:get(version, I2).
%% 1
%% '''
%%
%% Work item queries:
%%
%% ```erlang
%% 1> ok = wf_store:put_work_item(Store, #{id => <<"w1">>, assignee => <<"joe">>, status => enabled}).
%% ok
%% 2> ok = wf_store:put_work_item(Store, #{id => <<"w2">>, assignee => <<"jane">>, status => enabled}).
%% ok
%% 3> {ok, L} = wf_store:query_work_items(Store, #{assignee => <<"joe">>}).
%% 4> lists:sort([maps:get(id, X) || X <- L]).
%% [<<"w1">>]
%% '''
%%
%% Claiming work items:
%%
%% ```erlang
%% 1> ok = wf_store:put_work_item(Store, #{id => <<"w3">>, assignee => <<"bob">>, status => enabled}).
%% ok
%% 2> {atomic, ok} = wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w3">>, <<"bob">>) end).
%% {atomic,ok}
%% 3> {atomic, {error, already_claimed}} = wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w3">>, <<"bob">>) end).
%% {atomic,{error,already_claimed}}
%% 4> {ok, [Claimed]} = wf_store:query_work_items(Store, #{id => <<"w3">>}).
%% 5> maps:get(status, Claimed).
%% claimed
%% '''
%%
%% Closing the store:
%%
%% ```erlang
%% 1> ok = wf_store:close(Store).
%% ok
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_store).

%%====================================================================
%% Exports
%%====================================================================

%% Store management
-export([open/1, close/1]).

%% Instance operations
-export([put_instance/4, get_instance/2]).

%% Work item operations
-export([put_work_item/2, query_work_items/2, claim_work_item/3]).

%% Transaction support
-export([tx/2]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Records
%%====================================================================

-record(wf_store_instance, {
    id :: binary(),
    version :: non_neg_integer(),
    status :: term(),
    data :: map(),
    updated_at :: integer()
}).

-record(wf_store_workitem, {
    id :: binary(),
    assignee :: binary() | undefined,
    status :: term(),
    data :: map(),
    created_at :: integer()
}).

-record(store, {
    backend :: mnesia,
    node :: node(),
    is_tmp :: boolean(),
    tables :: [atom()]
}).

%%====================================================================
%% Types
%%====================================================================

-type store() :: #store{}.
-type instance_id() :: binary().
-type instance_map() :: map().
-type workitem_map() :: map().
-type tx_fun() :: fun(() -> term()).

-type config() :: #{
    backend := mnesia,
    dir := atom() | binary()
}.

-export_type([store/0, config/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Opens a store with the given configuration.
%%
%% Configuration is a map with:
%% - backend: mnesia (required)
%% - dir: tmp for in-memory tables, or a path for disc_copies (required)
%%
%% Returns {ok, Store} on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec open(config()) -> {ok, store()} | {error, term()}.

open(#{backend := mnesia, dir := tmp}) ->
    %% Create in-memory store for testing
    Node = node(),
    IsTmp = true,

    %% Start Mnesia if not running
    _ = case mnesia:start() of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Create tables with ram_copies for in-memory operation
    Tables = [wf_store_instance, wf_store_workitem],
    Result = lists:map(
        fun(Table) -> create_table(Table, ram_copies, Node) end,
        Tables
    ),

    case lists:all(fun(R) -> R =:= ok end, Result) of
        true ->
            {ok, #store{backend = mnesia, node = Node, is_tmp = IsTmp, tables = Tables}};
        false ->
            {error, table_creation_failed}
    end;

open(#{backend := mnesia, dir := _Dir}) ->
    %% Create disc-backed store
    Node = node(),
    IsTmp = false,

    %% Create Mnesia schema if needed
    _ = case mnesia:create_schema([Node]) of
        ok -> ok;
        {error, {already_exists, Node}} -> ok
    end,

    %% Start Mnesia
    _ = case mnesia:start() of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Create tables with disc_copies for persistence
    Tables = [wf_store_instance, wf_store_workitem],
    Result = lists:map(
        fun(Table) -> create_table(Table, disc_copies, Node) end,
        Tables
    ),

    case lists:all(fun(R) -> R =:= ok end, Result) of
        true ->
            {ok, #store{backend = mnesia, node = Node, is_tmp = IsTmp, tables = Tables}};
        false ->
            {error, table_creation_failed}
    end;

open(_Config) ->
    {error, invalid_config}.

%% @private
create_table(TableName, CopyType, Node) ->
    Attributes = case TableName of
        wf_store_instance -> record_info(fields, wf_store_instance);
        wf_store_workitem -> record_info(fields, wf_store_workitem)
    end,

    Def = [
        {attributes, Attributes},
        {CopyType, [Node]},
        {type, set}
    ],

    case mnesia:create_table(TableName, Def) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, TableName}} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Stores or updates a workflow instance.
%%
%% Instance is stored as a map under the given Id.
%% The Prev parameter is for optimistic locking (unused in simple impl).
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec put_instance(store(), instance_id(), instance_map(), term() | undefined) ->
          ok | {error, term()}.

put_instance(#store{}, Id, InstanceMap, _Prev) when is_binary(Id), is_map(InstanceMap) ->
    Version = maps:get(version, InstanceMap, 0),
    Status = maps:get(status, InstanceMap, undefined),
    Data = maps:get(data, InstanceMap, #{}),
    UpdatedAt = erlang:system_time(millisecond),

    Record = #wf_store_instance{
        id = Id,
        version = Version,
        status = Status,
        data = Data,
        updated_at = UpdatedAt
    },

    Transaction = fun() -> mnesia:write(Record) end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Retrieves a workflow instance by ID.
%%
%% Returns {ok, InstanceMap} on success with the instance data,
%% not_found if the instance does not exist.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_instance(store(), instance_id()) -> {ok, instance_map()} | not_found.

get_instance(#store{}, Id) when is_binary(Id) ->
    Transaction = fun() ->
        case mnesia:read(wf_store_instance, Id) of
            [#wf_store_instance{} = Record] ->
                {ok, instance_to_map(Record)};
            [] ->
                not_found
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {ok, Map}} -> {ok, Map};
        {atomic, not_found} -> not_found;
        {aborted, _Reason} -> not_found
    end.

%%--------------------------------------------------------------------
%% @doc Executes a function within a Mnesia transaction.
%%
%% The function can call get_instance/2 and put_instance/4 which will
%% participate in the transaction. Returns {atomic, Val} or {aborted, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec tx(store(), tx_fun()) -> {atomic, term()} | {aborted, term()}.

tx(#store{}, Fun) when is_function(Fun, 0) ->
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @doc Stores a work item.
%%
%% Work item is stored as a map with at least 'id' required.
%% Other typical fields: assignee, status, data.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec put_work_item(store(), workitem_map()) -> ok | {error, term()}.

put_work_item(#store{}, WorkItemMap) when is_map(WorkItemMap) ->
    Id = maps:get(id, WorkItemMap),
    Assignee = maps:get(assignee, WorkItemMap, undefined),
    Status = maps:get(status, WorkItemMap, undefined),
    Data = maps:get(data, WorkItemMap, #{}),
    CreatedAt = maps:get(created_at, WorkItemMap, erlang:system_time(millisecond)),

    Record = #wf_store_workitem{
        id = Id,
        assignee = Assignee,
        status = Status,
        data = Data,
        created_at = CreatedAt
    },

    Transaction = fun() -> mnesia:write(Record) end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Queries work items by matching against the provided criteria.
%%
%% Criteria is a map with field names as keys and expected values.
%% Supported fields: id, assignee, status.
%%
%% Returns {ok, [WorkItemMap]} with matching work items.
%%
%% @end
%%--------------------------------------------------------------------
-spec query_work_items(store(), map()) -> {ok, [workitem_map()]}.

query_work_items(#store{}, Criteria) when is_map(Criteria) ->
    Transaction = fun() ->
        %% Read all work items and filter in memory
        AllRecords = mnesia:match_object(#wf_store_workitem{_ = '_'}),
        filter_workitems(AllRecords, Criteria)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Results} -> {ok, Results};
        {aborted, _Reason} -> {ok, []}
    end.

%% @private
filter_workitems(Records, Criteria) ->
    lists:filtermap(
        fun(Record) ->
            Map = workitem_to_map(Record),
            case matches_criteria(Map, Criteria) of
                true -> {true, Map};
                false -> false
            end
        end,
        Records
    ).

%% @private
matches_criteria(_Map, empty) ->
    true;
matches_criteria(Map, Criteria) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            Acc andalso maps:get(Key, Map, undefined) =:= Value
        end,
        true,
        Criteria
    ).

%%--------------------------------------------------------------------
%% @doc Claims a work item by marking it as claimed.
%%
%% This function must be called within a transaction using tx/2.
%% It checks if the work item is already claimed (status = claimed or assigned)
%% and returns {error, already_claimed} if so. Otherwise, updates the status
%% to "claimed" and returns ok.
%%
%% Returns ok on success, {error, already_claimed} if already claimed.
%%
%% @end
%%--------------------------------------------------------------------
-spec claim_work_item(store(), binary(), binary() | undefined) -> ok | {error, already_claimed}.

claim_work_item(#store{}, WorkItemId, Claimant) when is_binary(WorkItemId) ->
    case mnesia:read(wf_store_workitem, WorkItemId) of
        [#wf_store_workitem{status = Status}] when Status =:= claimed; Status =:= assigned ->
            {error, already_claimed};
        [#wf_store_workitem{} = Record] ->
            UpdatedRecord = Record#wf_store_workitem{
                status = claimed,
                assignee = case Claimant of
                    undefined -> Record#wf_store_workitem.assignee;
                    _ -> Claimant
                end
            },
            ok = mnesia:write(UpdatedRecord),
            ok;
        [] ->
            {error, already_claimed}
    end.

%%--------------------------------------------------------------------
%% @doc Closes the store and cleans up resources.
%%
%% For tmp stores, deletes all tables.
%% For persistent stores, keeps the data on disk.
%%
%% Returns ok on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec close(store()) -> ok.

close(#store{is_tmp = true, tables = Tables}) ->
    %% Delete tables for tmp stores
    lists:foreach(
        fun(Table) ->
            mnesia:delete_table(Table)
        end,
        Tables
    ),
    ok;

close(#store{is_tmp = false}) ->
    %% For persistent stores, just stop Mnesia (optional)
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Converts a wf_store_instance record to a map.
-spec instance_to_map(#wf_store_instance{}) -> instance_map().

instance_to_map(#wf_store_instance{} = Record) ->
    #{
        version => Record#wf_store_instance.version,
        status => Record#wf_store_instance.status,
        data => Record#wf_store_instance.data,
        updated_at => Record#wf_store_instance.updated_at
    }.

%% @private
%% @doc Converts a wf_store_workitem record to a map.
-spec workitem_to_map(#wf_store_workitem{}) -> workitem_map().

workitem_to_map(#wf_store_workitem{} = Record) ->
    #{
        id => Record#wf_store_workitem.id,
        assignee => Record#wf_store_workitem.assignee,
        status => Record#wf_store_workitem.status,
        data => Record#wf_store_workitem.data,
        created_at => Record#wf_store_workitem.created_at
    }.

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the wf_store module.
%%
%% Tests all the examples from the module documentation.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Open tmp store
    {ok, Store} = open(#{backend => mnesia, dir => tmp}),

    %% Test 2: Put and get instance
    Id = <<"wf-1">>,
    ok = put_instance(Store, Id, #{version => 0, status => running}, undefined),
    {ok, I0} = get_instance(Store, Id),
    0 = maps:get(version, I0),
    running = maps:get(status, I0),
    not_found = get_instance(Store, <<"missing">>),

    %% Test 3: Transaction with update
    {atomic, ok} = tx(Store, fun() ->
        {ok, I1} = get_instance(Store, Id),
        ok = put_instance(Store, Id, I1#{version => 1}, undefined),
        ok
    end),
    {ok, I2} = get_instance(Store, Id),
    1 = maps:get(version, I2),

    %% Test 4: Work item operations
    ok = put_work_item(Store, #{id => <<"w1">>, assignee => <<"joe">>, status => enabled}),
    ok = put_work_item(Store, #{id => <<"w2">>, assignee => <<"jane">>, status => enabled}),
    {ok, L} = query_work_items(Store, #{assignee => <<"joe">>}),
    [<<"w1">>] = lists:sort([maps:get(id, X) || X <- L]),

    %% Test 5: Query by status
    {ok, L2} = query_work_items(Store, #{status => enabled}),
    true = length(L2) >= 2,

    %% Test 6: Empty query returns all
    {ok, L3} = query_work_items(Store, #{}),
    true = length(L3) >= 2,

    %% Test 7: Claim work item - first claim succeeds
    ok = put_work_item(Store, #{id => <<"w3">>, assignee => <<"bob">>, status => enabled}),
    {atomic, ok} = tx(Store, fun() -> claim_work_item(Store, <<"w3">>, <<"bob">>) end),

    %% Test 8: Second claim fails with already_claimed
    {atomic, {error, already_claimed}} = tx(Store, fun() ->
        claim_work_item(Store, <<"w3">>, <<"bob">>)
    end),

    %% Test 9: Verify work item status changed to claimed
    {ok, [Claimed]} = query_work_items(Store, #{id => <<"w3">>}),
    claimed = maps:get(status, Claimed),

    %% Test 10: Claiming non-existent work item fails
    {atomic, {error, already_claimed}} = tx(Store, fun() ->
        claim_work_item(Store, <<"nonexistent">>, <<"nobody">>)
    end),

    %% Test 11: Close store
    ok = close(Store),

    ok.
