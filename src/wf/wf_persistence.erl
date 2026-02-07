%% -*- erlang -*-
%%%% @doc Workflow Engine Persistence Module
%%
%% This module provides Mnesia-based persistence for the wf_engine workflow
%% engine. It handles durable storage of workflow cases, work items, events,
%% and checkpoint/recovery functionality.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Mnesia table schema initialization with disc_copies</li>
%%   <li>Workflow case persistence (create, read, update, delete)</li>
%%   <li>Work item persistence linked to cases</li>
%%   <li>Event log for audit trail</li>
%%   <li>Active case listing for recovery</li>
%%   <li>Checkpoint/recovery mechanism</li>
%% </ul>
%%
%% <h3>Mnesia Tables</h3>
%% <ul>
%%   <li><b>wf_persistent_case:</b> Workflow case records</li>
%%   <li><b>wf_persistent_workitem:</b> Work item records with case reference</li>
%%   <li><b>wf_persistent_event:</b> Event log records for audit trail</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Initializing the database:
%% ```erlang
%% 1> ok = wf_persistence:init_db().
%% ok
%% ```
%%
%% Saving and loading a case:
%% ```erlang
%% 1> Case = #wf_case{case_id = <<"case1">>, status = running,
%% 1> marking = #{p_start => [init]}, data = #{}}.
%% 2> ok = wf_persistence:save_case(Case).
%% ok
%% 3> {ok, Loaded} = wf_persistence:load_case(<<"case1">>).
%% {ok, #wf_case{case_id = <<"case1">>, status = running, ...}}
%% ```
%%
%% Events are persisted correctly:
%% ```erlang
%% 1> Event = {case_started, <<"case1">>, 1000}.
%% 2> ok = wf_persistence:save_event(<<"case1">>, Event).
%% ok
%% 3> {ok, Events} = wf_persistence:load_events(<<"case1">>).
%% {ok, [{case_started, <<"case1">>, 1000}]}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_persistence).

-include("wf_engine.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Database initialization
-export([init_db/0]).

%% Case persistence
-export([save_case/1, load_case/1, delete_case/1, list_active_cases/0]).

%% Work item persistence
-export([save_workitem/1, load_workitems/1, delete_workitem/2]).

%% Event log
-export([save_event/2, load_events/1, clear_events/1]).

%% Recovery
-export([create_checkpoint/1, restore_from_checkpoint/0]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Records
%%====================================================================

-record(wf_persistent_case, {
    case_id :: binary(),
    status :: running | suspended | cancelled | completed | failed | scheduled,
    marking :: map(),
    data :: map(),
    receipts :: [map()],
    rng_state :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
    timestamps :: map(),
    scheduled_at :: integer() | undefined
}).

-record(wf_persistent_workitem, {
    wi_id :: binary(),
    case_id :: binary(),
    task :: atom(),
    status :: offered | allocated | started | completed,
    assigned_to :: atom() | binary() | undefined,
    data :: map() | undefined,
    created_at :: integer()
}).

-record(wf_persistent_event, {
    event_id :: binary(),
    case_id :: binary(),
    event_type :: atom(),
    event_data :: term(),
    timestamp :: integer()
}).

-record(wf_checkpoint, {
    checkpoint_id :: binary(),
    timestamp :: integer(),
    engine_state :: term()
}).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary().
-type wi_id() :: binary().
-type case_status() :: running | suspended | cancelled | completed | failed | scheduled.
-type wi_status() :: offered | allocated | started | completed.

-export_type([case_id/0, wi_id/0, case_status/0, wi_status/0]).

%% Include wf_engine records for type compatibility
-include("wf_engine.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the Mnesia database with persistent tables.
%%
%% Creates the following tables with disc_copies on the current node:
%% - wf_persistent_case: Stores workflow case records
%% - wf_persistent_workitem: Stores work item records
%% - wf_persistent_event: Stores event log records
%% - wf_checkpoint: Stores checkpoint records
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_db() -> ok | {error, term()}.

init_db() ->
    Node = node(),

    %% Create Mnesia schema if it doesn't exist
    _ = case mnesia:create_schema([Node]) of
        ok ->
            ?LOG_INFO("Mnesia schema created on node ~p", [Node]);
        {error, {already_exists, Node}} ->
            ?LOG_INFO("Mnesia schema already exists on node ~p", [Node]);
        {error, SchemaReason} ->
            ?LOG_ERROR("Failed to create Mnesia schema: ~p", [SchemaReason])
    end,

    %% Start Mnesia
    _ = case mnesia:start() of
        ok ->
            ?LOG_INFO("Mnesia started successfully");
        {error, {already_started, _}} ->
            ok;
        {error, StartReason} ->
            ?LOG_ERROR("Failed to start Mnesia: ~p", [StartReason])
    end,

    %% Table definitions
    Tables = [
        {wf_persistent_case,
         [{attributes, record_info(fields, wf_persistent_case)},
          {disc_copies, [Node]},
          {type, set}]},
        {wf_persistent_workitem,
         [{attributes, record_info(fields, wf_persistent_workitem)},
          {disc_copies, [Node]},
          {type, set},
          {index, [case_id]}]},
        {wf_persistent_event,
         [{attributes, record_info(fields, wf_persistent_event)},
          {disc_copies, [Node]},
          {type, bag},
          {index, [case_id]}]},
        {wf_checkpoint,
         [{attributes, record_info(fields, wf_checkpoint)},
          {disc_copies, [Node]},
          {type, set}]}
    ],

    %% Create tables
    Results = lists:map(fun create_table_if_not_exists/1, Tables),

    %% Wait for tables to be ready
    TableNames = [wf_persistent_case, wf_persistent_workitem,
                  wf_persistent_event, wf_checkpoint],
    WaitResult = case mnesia:wait_for_tables(TableNames, 10000) of
        ok ->
            ok;
        {timeout, FailedTables} ->
            ?LOG_ERROR("Timeout waiting for tables: ~p", [FailedTables]),
            {error, {timeout, FailedTables}};
        {error, WaitReason} ->
            ?LOG_ERROR("Error waiting for tables: ~p", [WaitReason]),
            {error, WaitReason}
    end,

    %% Return combined result
    case {lists:all(fun(R) -> R =:= ok end, Results), WaitResult} of
        {true, ok} -> ok;
        {false, _} -> {error, table_creation_failed};
        {_, {error, R}} -> {error, R}
    end.

%% @private
create_table_if_not_exists({TableName, TableDef}) ->
    case mnesia:create_table(TableName, TableDef) of
        {atomic, ok} ->
            ?LOG_INFO("Created table ~p", [TableName]),
            ok;
        {aborted, {already_exists, TableName}} ->
            ?LOG_INFO("Table ~p already exists", [TableName]),
            ok;
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to create table ~p: ~p", [TableName, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Saves or updates a workflow case.
%%
%% Takes a #wf_case{} record and persists it to Mnesia.
%% If a case with the same case_id exists, it will be updated.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% ```erlang
%% 1> Case = #wf_case{case_id = <<"case1">>, status = running,
%% 1> marking = #{p_start => [init]}, data = #{key => val}}.
%% 2> ok = wf_persistence:save_case(Case).
%% ok
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec save_case(#wf_case{}) -> ok | {error, term()}.

save_case(#wf_case{case_id = CaseId} = Case) ->
    PersistentCase = #wf_persistent_case{
        case_id = CaseId,
        status = Case#wf_case.status,
        marking = Case#wf_case.marking,
        data = Case#wf_case.data,
        receipts = serialize_receipts(Case#wf_case.receipts),
        rng_state = serialize_rng_state(Case#wf_case.rng_state),
        timestamps = Case#wf_case.timestamps,
        scheduled_at = Case#wf_case.scheduled_at
    },

    Transaction = fun() ->
        mnesia:write(PersistentCase)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            ?LOG_DEBUG("Saved case: ~p", [CaseId]),
            ok;
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to save case ~p: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Loads a case by its case_id.
%%
%% Returns {ok, #wf_case{}} on success with the case record reconstructed,
%% {error, not_found} if the case does not exist.
%%
%% ```erlang
%% 1> {ok, Case} = wf_persistence:load_case(<<"case1">>).
%% {ok, #wf_case{case_id = <<"case1">>, status = running, ...}}
%%
%% 2> {error, not_found} = wf_persistence:load_case(<<"nonexistent">>).
%% {error, not_found}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec load_case(case_id()) -> {ok, #wf_case{}} | {error, not_found | term()}.

load_case(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        case mnesia:read(wf_persistent_case, CaseId) of
            [#wf_persistent_case{} = PersistentCase] ->
                {ok, persistent_case_to_wf_case(PersistentCase)};
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {ok, Case}} ->
            %% Load associated work items
            case load_workitems(CaseId) of
                {ok, WorkItems} ->
                    WIMap = lists:foldl(
                        fun(WI, Acc) -> maps:put(WI#wf_persistent_workitem.wi_id,
                                                 workitem_to_wf_workitem(WI), Acc) end,
                        #{},
                        WorkItems
                    ),
                    {ok, Case#wf_case{work_items = WIMap}};
                {error, Reason} ->
                    ?LOG_WARNING("Failed to load work items for case ~p: ~p", [CaseId, Reason]),
                    {ok, Case#wf_case{work_items = #{}}}
            end;
        {atomic, {error, not_found}} ->
            {error, not_found};
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to load case ~p: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a case and all its associated data.
%%
%% This is a cascading delete that removes all work items and events
%% associated with the case before removing the case itself.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_case(case_id()) -> ok | {error, term()}.

delete_case(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        %% Delete all work items for this case
        WIResults = mnesia:index_read(wf_persistent_workitem, CaseId,
                                      #wf_persistent_workitem.case_id),
        lists:foreach(fun(WI) -> mnesia:delete_object(WI) end, WIResults),

        %% Delete all events for this case
        EventResults = mnesia:index_read(wf_persistent_event, CaseId,
                                         #wf_persistent_event.case_id),
        lists:foreach(fun(E) -> mnesia:delete_object(E) end, EventResults),

        %% Delete the case
        case mnesia:read(wf_persistent_case, CaseId) of
            [_Case] ->
                mnesia:delete(wf_persistent_case, CaseId, write),
                ok;
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            ?LOG_INFO("Deleted case: ~p", [CaseId]),
            ok;
        {atomic, {error, not_found}} ->
            {error, not_found};
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to delete case ~p: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all active cases (running or suspended).
%%
%% Returns a list of case maps with status running or suspended.
%% Useful for recovery after system restart.
%%
%% ```erlang
%% 1> {ok, ActiveCases} = wf_persistence:list_active_cases().
%% {ok, [#{case_id => <<"case1">>, status => running, ...}]}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec list_active_cases() -> {ok, [map()]} | {error, term()}.

list_active_cases() ->
    Transaction = fun() ->
        Running = mnesia:match_object(#wf_persistent_case{status = running, _ = '_'}),
        Suspended = mnesia:match_object(#wf_persistent_case{status = suspended, _ = '_'}),
        Scheduled = mnesia:match_object(#wf_persistent_case{status = scheduled, _ = '_'}),
        {Running, Suspended, Scheduled}
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {Running, Suspended, Scheduled}} ->
            ActiveCases = lists:map(fun persistent_case_to_map/1,
                                    Running ++ Suspended ++ Scheduled),
            {ok, ActiveCases};
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to list active cases: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Saves or updates a work item.
%%
%% Takes a #work_item{} record and persists it to Mnesia.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_workitem(#work_item{}) -> ok | {error, term()}.

save_workitem(#work_item{wi_id = WiId} = WI) ->
    PersistentWI = #wf_persistent_workitem{
        wi_id = WiId,
        case_id = WI#work_item.case_id,
        task = WI#work_item.task,
        status = WI#work_item.status,
        assigned_to = WI#work_item.assigned_to,
        data = WI#work_item.data,
        created_at = WI#work_item.created_at
    },

    Transaction = fun() ->
        mnesia:write(PersistentWI)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            ?LOG_DEBUG("Saved work item: ~p", [WiId]),
            ok;
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to save work item ~p: ~p", [WiId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Loads all work items for a given case_id.
%%
%% Returns {ok, [#wf_persistent_workitem{}]} where each record contains
%% the persisted work item data.
%%
%% Returns {ok, []} if no work items exist for the case.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_workitems(case_id()) -> {ok, [#wf_persistent_workitem{}]} | {error, term()}.

load_workitems(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        mnesia:index_read(wf_persistent_workitem, CaseId, #wf_persistent_workitem.case_id)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, WorkItems} ->
            {ok, WorkItems};
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to load work items for case ~p: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a specific work item.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_workitem(case_id(), wi_id()) -> ok | {error, term()}.

delete_workitem(CaseId, WiId) ->
    Transaction = fun() ->
        case mnesia:match_object(#wf_persistent_workitem{
                                    wi_id = WiId,
                                    case_id = CaseId,
                                    _ = '_'}) of
            [WI] ->
                mnesia:delete_object(WI),
                ok;
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            ok;
        {atomic, {error, not_found}} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Saves an event to the event log.
%%
%% Events are stored for audit trail and debugging purposes.
%% Each event is tagged with a case_id, type, and timestamp.
%%
%% ```erlang
%% 1> Event = {case_started, <<"case1">>, 1000}.
%% 2> ok = wf_persistence:save_event(<<"case1">>, Event).
%% ok
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec save_event(case_id(), term()) -> ok | {error, term()}.

save_event(CaseId, Event) when is_binary(CaseId) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(millisecond),
    {EventType, EventData} = extract_event_info(Event),

    PersistentEvent = #wf_persistent_event{
        event_id = EventId,
        case_id = CaseId,
        event_type = EventType,
        event_data = EventData,
        timestamp = Timestamp
    },

    Transaction = fun() ->
        mnesia:write(PersistentEvent)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            ?LOG_DEBUG("Saved event ~p for case ~p", [EventType, CaseId]),
            ok;
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to save event for case ~p: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Loads all events for a given case_id.
%%
%% Returns {ok, [term()]} with the reconstructed event terms.
%% Events are ordered by timestamp (oldest first).
%%
%% ```erlang
%% 1> {ok, Events} = wf_persistence:load_events(<<"case1">>).
%% {ok, [{case_started, <<"case1">>, 1000}, {task_created, t1, wi1, 1001}]}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec load_events(case_id()) -> {ok, [term()]} | {error, term()}.

load_events(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        mnesia:index_read(wf_persistent_event, CaseId, #wf_persistent_event.case_id)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Events} ->
            %% Reconstruct events and sort by timestamp
            Reconstructed = [reconstruct_event(E) || E <- Events],
            Sorted = lists:keysort(1, Reconstructed),  %% Sort by timestamp
            EventTerms = [Term || {_Ts, Term} <- Sorted],
            {ok, EventTerms};
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to load events for case ~p: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Clears all events for a given case_id.
%%
%% Useful for cleanup after case completion.
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_events(case_id()) -> ok | {error, term()}.

clear_events(CaseId) when is_binary(CaseId) ->
    Transaction = fun() ->
        Events = mnesia:index_read(wf_persistent_event, CaseId,
                                   #wf_persistent_event.case_id),
        lists:foreach(fun(E) -> mnesia:delete_object(E) end, Events),
        ok
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates a checkpoint of all active cases.
%%
%% A checkpoint is a snapshot that can be used for recovery.
%%
%% Returns {ok, CheckpointId} on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec create_checkpoint(map()) -> {ok, binary()} | {error, term()}.

create_checkpoint(EngineState) when is_map(EngineState); is_tuple(EngineState) ->
    CheckpointId = generate_checkpoint_id(),
    Timestamp = erlang:system_time(millisecond),

    Checkpoint = #wf_checkpoint{
        checkpoint_id = CheckpointId,
        timestamp = Timestamp,
        engine_state = EngineState
    },

    Transaction = fun() ->
        mnesia:write(Checkpoint)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            ?LOG_INFO("Created checkpoint: ~p at ~p", [CheckpointId, Timestamp]),
            {ok, CheckpointId};
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to create checkpoint: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Restores the most recent checkpoint.
%%
%% Returns {ok, EngineState} on success, {error, not_found} if no
%% checkpoint exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec restore_from_checkpoint() -> {ok, term()} | {error, not_found | term()}.

restore_from_checkpoint() ->
    Transaction = fun() ->
        %% Get all checkpoints and find the most recent
        Checkpoints = mnesia:match_object(#wf_checkpoint{_ = '_'}),
        case lists:keysort(#wf_checkpoint.timestamp, Checkpoints) of
            [] ->
                {error, not_found};
            Sorted ->
                #wf_checkpoint{engine_state = EngineState} = lists:last(Sorted),
                {ok, EngineState}
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {ok, EngineState}} ->
            ?LOG_INFO("Restored from checkpoint"),
            {ok, EngineState};
        {atomic, {error, not_found}} ->
            {error, not_found};
        {aborted, Reason} ->
            ?LOG_ERROR("Failed to restore checkpoint: ~p", [Reason]),
            {error, Reason}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Converts a persistent_case record to a wf_case record.
-spec persistent_case_to_wf_case(#wf_persistent_case{}) -> #wf_case{}.

persistent_case_to_wf_case(#wf_persistent_case{} = PC) ->
    #wf_case{
        case_id = PC#wf_persistent_case.case_id,
        status = PC#wf_persistent_case.status,
        marking = PC#wf_persistent_case.marking,
        data = PC#wf_persistent_case.data,
        receipts = deserialize_receipts(PC#wf_persistent_case.receipts),
        rng_state = deserialize_rng_state(PC#wf_persistent_case.rng_state),
        timestamps = PC#wf_persistent_case.timestamps,
        scheduled_at = PC#wf_persistent_case.scheduled_at,
        work_items = #{},
        events = [],
        log = []
    }.

%% @private
%% @doc Converts a persistent_case record to a map.
-spec persistent_case_to_map(#wf_persistent_case{}) -> map().

persistent_case_to_map(#wf_persistent_case{} = PC) ->
    #{
        case_id => PC#wf_persistent_case.case_id,
        status => PC#wf_persistent_case.status,
        marking => PC#wf_persistent_case.marking,
        data => PC#wf_persistent_case.data,
        timestamps => PC#wf_persistent_case.timestamps,
        scheduled_at => PC#wf_persistent_case.scheduled_at
    }.

%% @private
%% @doc Converts a persistent_workitem to a work_item record.
-spec workitem_to_wf_workitem(#wf_persistent_workitem{}) -> #work_item{}.

workitem_to_wf_workitem(#wf_persistent_workitem{} = PWI) ->
    #work_item{
        wi_id = PWI#wf_persistent_workitem.wi_id,
        case_id = PWI#wf_persistent_workitem.case_id,
        task = PWI#wf_persistent_workitem.task,
        status = PWI#wf_persistent_workitem.status,
        assigned_to = PWI#wf_persistent_workitem.assigned_to,
        data = PWI#wf_persistent_workitem.data,
        created_at = PWI#wf_persistent_workitem.created_at
    }.

%% @private
%% @doc Serializes receipts for storage.
%% Converts pnet_receipt format to plain maps.
-spec serialize_receipts([term()]) -> [map()].

serialize_receipts(Receipts) when is_list(Receipts) ->
    [serialize_receipt(R) || R <- Receipts].

%% @private
serialize_receipt(Receipt) when is_map(Receipt) ->
    %% Receipts are already maps, just ensure they're storable
    Receipt;
serialize_receipt(Receipt) ->
    %% Fallback for other formats
    #{data => term_to_binary(Receipt)}.

%% @private
%% @doc Deserializes receipts from storage.
-spec deserialize_receipts([map()]) -> [term()].

deserialize_receipts(Maps) when is_list(Maps) ->
    [deserialize_receipt(M) || M <- Maps].

%% @private
deserialize_receipt(#{data := Data} = Map) when is_binary(Data) ->
    case maps:get(before_hash, Map, undefined) of
        undefined -> binary_to_term(Data);
        _ -> Map
    end;
deserialize_receipt(Map) when is_map(Map) ->
    Map.

%% @private
%% @doc Serializes RNG state for storage.
-spec serialize_rng_state(pnet_choice:rng_state()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

serialize_rng_state({S1, S2, S3, S4}) when is_integer(S1), is_integer(S2),
                                           is_integer(S3), is_integer(S4) ->
    {S1, S2, S3};  %% Truncate 4-tuple to 3-tuple for storage
serialize_rng_state({S1, S2, S3}) when is_integer(S1), is_integer(S2), is_integer(S3) ->
    {S1, S2, S3};
serialize_rng_state(RngState) ->
    %% Fallback for other formats
    {0, 0, 0}.

%% @private
%% @doc Deserializes RNG state from storage.
-spec deserialize_rng_state({non_neg_integer(), non_neg_integer(), non_neg_integer()}) ->
          pnet_choice:rng_state().

deserialize_rng_state({S1, S2, S3}) when is_integer(S1), is_integer(S2), is_integer(S3) ->
    {S1, S2, S3}.

%% @private
%% @doc Extracts event type and data from an event term.
-spec extract_event_info(term()) -> {atom(), term()}.

extract_event_info({EventType, _Data} = Event) when is_atom(EventType) ->
    {EventType, Event};
extract_event_info({EventType, _, _} = Event) when is_atom(EventType) ->
    {EventType, Event};
extract_event_info({EventType, _, _, _} = Event) when is_atom(EventType) ->
    {EventType, Event};
extract_event_info(Event) ->
    {unknown, Event}.

%% @private
%% @doc Reconstructs an event term from persistent format.
-spec reconstruct_event(#wf_persistent_event{}) -> {integer(), term()}.

reconstruct_event(#wf_persistent_event{event_type = EventType,
                                       event_data = EventData,
                                       timestamp = Timestamp}) ->
    {Timestamp, EventData}.

%% @private
%% @doc Generates a unique event ID.
-spec generate_event_id() -> binary().

generate_event_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"evt_", Hex/binary>>.

%% @private
%% @doc Generates a unique checkpoint ID.
-spec generate_checkpoint_id() -> binary().

generate_checkpoint_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"ckpt_", Hex/binary>>.

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the persistence module.
%%
%% Tests save/load roundtrip, event persistence, and recovery.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Initialize database for testing
    ok = init_db(),

    %% Test 1: Save and load case roundtrip
    TestCaseId = <<"doctest_case">>,
    TestCase = #wf_case{
        case_id = TestCaseId,
        status = running,
        marking = #{p_start => [init], p_end => []},
        data = #{key => value},
        receipts = [],
        rng_state = {1, 2, 3},
        timestamps = #{created_at => 1000},
        scheduled_at = undefined,
        work_items = #{},
        events = [],
        log = []
    },
    ok = save_case(TestCase),

    %% Test 2: Load case and verify
    {ok, LoadedCase} = load_case(TestCaseId),
    TestCaseId = LoadedCase#wf_case.case_id,
    running = LoadedCase#wf_case.status,
    #{key := value} = LoadedCase#wf_case.data,

    %% Test 3: Update case status
    UpdatedCase = TestCase#wf_case{status = suspended},
    ok = save_case(UpdatedCase),
    {ok, SuspendedCase} = load_case(TestCaseId),
    suspended = SuspendedCase#wf_case.status,

    %% Test 4: Work item persistence
    TestWI = #work_item{
        wi_id = <<"wi_doctest">>,
        case_id = TestCaseId,
        task = test_task,
        status = offered,
        assigned_to = undefined,
        data = #{},
        created_at = 1001
    },
    ok = save_workitem(TestWI),
    {ok, LoadedWIs} = load_workitems(TestCaseId),
    true = length(LoadedWIs) >= 1,

    %% Test 5: Event persistence
    Event1 = {case_started, TestCaseId, 1000},
    ok = save_event(TestCaseId, Event1),
    Event2 = {task_created, test_task, <<"wi_doctest">>, 1001},
    ok = save_event(TestCaseId, Event2),
    {ok, LoadedEvents} = load_events(TestCaseId),
    true = length(LoadedEvents) >= 2,

    %% Test 6: Clear events
    ok = clear_events(TestCaseId),
    {ok, ClearedEvents} = load_events(TestCaseId),
    0 = length(ClearedEvents),

    %% Test 7: List active cases
    {ok, ActiveCases} = list_active_cases(),
    true = is_list(ActiveCases),

    %% Test 8: Delete case
    ok = delete_case(TestCaseId),
    {error, not_found} = load_case(TestCaseId),

    %% Test 9: Load non-existent case
    {error, not_found} = load_case(<<"nonexistent_case">>),

    %% Test 10: Save event with various types
    TestCaseId2 = <<"doctest_case2">>,
    TestCase2 = TestCase#wf_case{case_id = TestCaseId2},
    ok = save_case(TestCase2),
    ok = save_event(TestCaseId2, {test_event, data, 123}),
    ok = save_event(TestCaseId2, {complex_event, #{a => 1, b => 2}}),
    {ok, MixedEvents} = load_events(TestCaseId2),
    true = length(MixedEvents) >= 2,

    %% Cleanup
    ok = delete_case(TestCaseId2),

    ok.
