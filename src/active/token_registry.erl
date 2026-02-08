%% -*- erlang -*-
%%%% @doc token_registry - Token process registry using gproc.
%%
%% This module provides a centralized registry for active token processes.
%% It uses gproc for efficient process registration and lookup.
%%
%% <h3>Registration Keys</h3>
%%
%% Tokens are registered under multiple keys for different lookup patterns:
%% <ul>
%%   <li><b>{n, g, {token, TokenId}}:</b> Direct token ID lookup</li>
%%   <li><b>{n, g, {token, Place, TokenId}}:</b> Place-scoped lookup</li>
%%   <li><b>{n, g, {tokens, Place}}:</b> All tokens in a place (counter)</li>
%%   <li><b>{n, g, {token, parent, ParentId}}:</b> Parent-child relationship</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(token_registry).
-author("CRE Team").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Registry API
-export([start_link/0]).
-export([register/3]).
-export([unregister/2]).
-export([lookup/1]).
-export([lookup_by_place/2]).
-export([lookup_by_parent/1]).
-export([list_tokens/1]).
-export([count_tokens/1]).
-export([whereis/1]).
-export([get_token_info/1]).

%% Token lifecycle hooks
-export([token_created/3]).
-export([token_terminated/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% Records
%%====================================================================

-record(token_info, {
    token_id :: binary(),
    pid :: pid(),
    place :: atom(),
    parent_id :: binary() | undefined,
    created_at :: integer(),
    metadata = #{} :: map()
}).

-record(registry_state, {
    token_count = 0 :: non_neg_integer(),
    place_counts = #{} :: #{atom() => non_neg_integer()},
    stats = #{} :: map()
}).

%%====================================================================
%% Types
%%====================================================================

-type token_info() :: #token_info{}.
-type registry_state() :: #registry_state{}.

-export_type([token_info/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the token registry.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Registers a token with the registry.
%%
%% @param TokenId Unique token identifier
%% @param Pid Token process
%% @param Place Initial place
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec register(binary(), pid(), atom()) -> ok | {error, term()}.

register(TokenId, Pid, Place) when is_binary(TokenId), is_pid(Pid), is_atom(Place) ->
    gen_server:call(?MODULE, {register, TokenId, Pid, Place}).

%%--------------------------------------------------------------------
%% @doc Unregisters a token from the registry.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister(binary(), atom()) -> ok.

unregister(TokenId, Place) when is_binary(TokenId), is_atom(Place) ->
    gen_server:call(?MODULE, {unregister, TokenId, Place}).

%%--------------------------------------------------------------------
%% @doc Looks up a token by ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup(binary()) -> {ok, pid()} | {error, not_found}.

lookup(TokenId) when is_binary(TokenId) ->
    case gproc:lookup_local_name({token, TokenId}) of
        Pid when is_pid(Pid) -> {ok, Pid};
        undefined -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Looks up a token by place and ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_by_place(atom(), binary()) -> {ok, pid()} | {error, not_found}.

lookup_by_place(Place, TokenId) when is_atom(Place), is_binary(TokenId) ->
    case gproc:lookup_local_name({token, Place, TokenId}) of
        Pid when is_pid(Pid) -> {ok, Pid};
        undefined -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Looks up tokens by parent ID.
%%
%% Returns all tokens spawned from the given parent.
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_by_parent(binary()) -> [pid()].

lookup_by_parent(ParentId) when is_binary(ParentId) ->
    %% gproc doesn't support reverse lookup well, so we scan
    %% In production, maintain an ets table for parent index
    Pattern = [{{{token, '$1', '_'}, '_', '_', '_', '_', '_', '_}},
               [{'=:=', '$1', ParentId}], ['$_']]],
    case ets:select(token_registry_table, Pattern, 100) of
        '$end_of_table' -> [];
        {Results, _} -> [Info#token_info.pid || Info <- Results]
    end.

%%--------------------------------------------------------------------
%% @doc Lists all tokens in a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_tokens(atom()) -> [pid()].

list_tokens(Place) when is_atom(Place) ->
    Pattern = [{ Place, '_', '_', '_', '_', '_', '_', '_' }],
    case ets:match(token_registry_table, Pattern, 100) of
        '$end_of_table' -> [];
        {Results, _} -> [Pid || [Pid] <- Results]
    end.

%%--------------------------------------------------------------------
%% @doc Counts tokens in a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec count_tokens(atom()) -> non_neg_integer().

count_tokens(Place) when is_atom(Place) ->
    gen_server:call(?MODULE, {count_tokens, Place}).

%%--------------------------------------------------------------------
%% @doc Finds the process for a token ID.
%%
%% Similar to lookup/1 but returns pid() directly.
%%
%% @end
%%--------------------------------------------------------------------
-spec whereis(binary()) -> pid() | undefined.

whereis(TokenId) when is_binary(TokenId) ->
    gproc:whereis_name_local({token, TokenId}).

%%--------------------------------------------------------------------
%% @doc Gets detailed token information.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_token_info(binary()) -> {ok, token_info()} | {error, not_found}.

get_token_info(TokenId) when is_binary(TokenId) ->
    case ets:lookup(token_registry_table, TokenId) of
        [Info] -> {ok, Info};
        [] -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Hook called when a token is created.
%%
%% @end
%%--------------------------------------------------------------------
-spec token_created(binary(), pid(), atom()) -> ok.

token_created(TokenId, Pid, Place) ->
    register(TokenId, Pid, Place),
    logger:debug("Token ~s created at ~s", [TokenId, Place]),
    ok.

%%--------------------------------------------------------------------
%% @doc Hook called when a token terminates.
%%
%% @end
%%--------------------------------------------------------------------
-spec token_terminated(binary(), atom()) -> ok.

token_terminated(TokenId, Place) ->
    unregister(TokenId, Place),
    logger:debug("Token ~s terminated at ~s", [TokenId, Place]),
    ok.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, registry_state()}.

init([]) ->
    %% Create ETS table for token metadata
    ets:new(token_registry_table, [
        named_table,
        set,
        public,
        {keypos, #token_info.token_id},
        {read_concurrency, true}
    ]),
    {ok, #registry_state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, registry_state()) ->
          {reply, term(), registry_state()}.

handle_call({register, TokenId, Pid, Place}, _From, State) ->
    case gproc:register_name({token, TokenId}, Pid) of
        true ->
            %% Register place-scoped name too
            gproc:register_name({token, Place, TokenId}, Pid),

            %% Store token info
            TokenInfo = #token_info{
                token_id = TokenId,
                pid = Pid,
                place = Place,
                created_at = erlang:system_time(millisecond)
            },
            ets:insert(token_registry_table, TokenInfo),

            %% Monitor the process
            erlang:monitor(process, Pid),

            %% Update counts
            Count = State#registry_state.token_count + 1,
            PlaceCounts = maps:update_with(Place, fun(C) -> C + 1 end, 1,
                                            State#registry_state.place_counts),

            {reply, ok, State#registry_state{
                token_count = Count,
                place_counts = PlaceCounts
            }};
        false ->
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister, TokenId, Place}, _From, State) ->
    %% Unregister from gproc
    gproc:unregister_name({token, TokenId}),
    gproc:unregister_name({token, Place, TokenId}),

    %% Remove from ETS
    ets:delete(token_registry_table, TokenId),

    %% Update counts
    Count = State#registry_state.token_count - 1,
    PlaceCounts = maps:update_with(Place, fun(C) -> C - 1 end,
                                    State#registry_state.place_counts),

    {reply, ok, State#registry_state{
        token_count = Count,
        place_counts = PlaceCounts
    }};

handle_call({count_tokens, Place}, _From, State) ->
    Count = maps:get(Place, State#registry_state.place_counts, 0),
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), registry_state()) -> {noreply, registry_state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), registry_state()) -> {noreply, registry_state()}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Process died - clean up registry entries
    case ets:match_object(token_registry_table, #token_info{pid = Pid, _ = '_'}) of
        [TokenInfo] ->
            token_terminated(TokenInfo#token_info.token_id, TokenInfo#token_info.place);
        [] ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), registry_state(), term()) -> {ok, registry_state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), registry_state()) -> ok.

terminate(_Reason, _State) ->
    ok.
