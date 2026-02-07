%% -*- erlang -*-
%%%% @doc YAWL Workflow Checkpoint Layer using Mnesia
%%
%% This module provides checkpoint storage for YAWL workflow instances
%% using Mnesia. It supports checkpointing workflow state for recovery
%% after crashes or restarts.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_checkpoint).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0, start_link/1]).
-export([init_schema/0, init_schema/1]).
-export([checkpoint_save/2, checkpoint_restore/1]).
-export([checkpoint_delete/1, checkpoint_list/0]).
-export([get_checkpoint_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary().
-type timestamp() :: integer().

-record(yawl_instance, {
    case_id :: case_id(),
    net_mod :: atom(),
    marking :: map(),
    usr_info :: term(),
    timestamp :: timestamp(),
    status :: running | completed | failed | suspended
}).

-type checkpoint() :: #yawl_instance{}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the checkpoint server with default options.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc Starts the checkpoint server with options.
%%
%% Options:
%% - `{ram_copies, boolean()}` - Use ram_copies instead of disc_copies
%% - `{auto_save, boolean()}` - Auto-save on interval (default: true)
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Options :: [{atom(), term()}]) -> gen_server:start_ret().
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Initializes the Mnesia schema for workflow instances.
%%
%% Creates the yawl_instance table with appropriate indexes.
%% Uses disc_copies by default for persistence.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_schema() -> ok | {error, term()}.
init_schema() ->
    init_schema(disc_copies).

%%--------------------------------------------------------------------
%% @doc Initializes the Mnesia schema with specified storage type.
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
    case mnesia:create_table(yawl_instance,
            [{attributes, record_info(fields, yawl_instance)},
             {index, [#yawl_instance.net_mod, #yawl_instance.timestamp]},
             {StorageType, [node()]},
             {type, set}]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, yawl_instance}} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Saves a checkpoint for a workflow instance.
%%
%% Takes the Pid of the gen_yawl process and the current net_state.
%% Generates a case_id from the pid and stores the checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint_save(pid(), map()) -> ok | {error, term()}.
checkpoint_save(Pid, NetState) when is_pid(Pid), is_map(NetState) ->
    %% Generate case_id from pid
    CaseId = list_to_binary(io_lib:format("~p", [Pid])),

    %% Extract fields from net_state
    NetMod = maps:get(net_mod, NetState, undefined),
    Marking = maps:get(marking, NetState, #{}),
    UsrInfo = maps:get(usr_info, NetState, undefined),
    Timestamp = erlang:system_time(millisecond),
    Status = running,

    %% Create checkpoint record
    Checkpoint = #yawl_instance{
        case_id = CaseId,
        net_mod = NetMod,
        marking = Marking,
        usr_info = UsrInfo,
        timestamp = Timestamp,
        status = Status
    },

    %% Write to Mnesia
    case mnesia:transaction(fun() -> mnesia:write(Checkpoint) end) of
        {atomic, ok} ->
            logger:debug("Saved checkpoint for case ~p", [CaseId]),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to save checkpoint: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Restores a workflow instance from checkpoint.
%%
%% Takes a case_id and returns the checkpoint data if found.
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint_restore(binary()) -> {ok, map()} | {error, not_found}.
checkpoint_restore(CaseId) when is_binary(CaseId) ->
    case mnesia:transaction(fun() -> mnesia:read(yawl_instance, CaseId) end) of
        {atomic, [#yawl_instance{marking = Marking,
                                 usr_info = UsrInfo,
                                 net_mod = NetMod}]} ->
            {ok, #{
                <<"net_mod">> => NetMod,
                <<"marking">> => Marking,
                <<"usr_info">> => UsrInfo
            }};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint_delete(binary()) -> ok | {error, term()}.
checkpoint_delete(CaseId) when is_binary(CaseId) ->
    case mnesia:transaction(fun() -> mnesia:delete(yawl_instance, CaseId) end) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all checkpoints.
%%
%% Returns a list of case_ids for all stored checkpoints.
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint_list() -> [binary()].
checkpoint_list() ->
    case mnesia:transaction(fun() -> mnesia:all_keys(yawl_instance) end) of
        {atomic, Keys} ->
            Keys;
        {aborted, _} ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc Gets detailed information about a checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_checkpoint_info(binary()) ->
          {ok, #{binary() => term()}} | {error, not_found}.
get_checkpoint_info(CaseId) when is_binary(CaseId) ->
    case mnesia:transaction(fun() -> mnesia:read(yawl_instance, CaseId) end) of
        {atomic, [#yawl_instance{net_mod = NetMod,
                                 timestamp = Timestamp,
                                 status = Status}]} ->
            {ok, #{
                <<"case_id">> => CaseId,
                <<"net_mod">> => NetMod,
                <<"timestamp">> => Timestamp,
                <<"status">> => Status,
                <<"age_ms">> => erlang:system_time(millisecond) - Timestamp
            }};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(_Options) ->
    %% Ensure schema is initialized
    case init_schema() of
        ok ->
            logger:info("yawl_checkpoint initialized"),
            {ok, #{}};
        {error, Reason} ->
            logger:error("yawl_checkpoint init failed: ~p", [Reason]),
            {stop, Reason}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.
