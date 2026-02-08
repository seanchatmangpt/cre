%% -*- erlang -*-
%%%% @doc place_coordinator - Place-level coordination for active tokens.
%%
%% This module coordinates tokens at the place level. Each place has a
%% coordinator that manages token lifecycle within that place, handles
%% transitions, and enables token-to-token communication.
%%
%% <h3>Place Coordinator Responsibilities</h3>
%%
%% <ul>
%%   <li><b>Token Registration:</b> Track all tokens in the place</li>
%%   <li><b>Transition Enablement:</b> Determine which transitions are enabled</li>
%%   <li><b>Token Synchronization:</b> Coordinate concurrent token operations</li>
%%   <li><b>Communication:</b> Route messages between tokens in the place</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(place_coordinator).
-author("CRE Team").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Place Coordinator API
-export([start_link/2]).
-export([register_token/3]).
-export([unregister_token/2]).
-export([get_tokens/1]).
-export([enable_transition/3]).
-export([notify_tokens/2]).
-export([get_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").
-include("token_registry.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(place_coordinator, {
    place :: atom(),
    net_mod :: atom(),
    tokens = #{} :: #{binary() => pid()},
    pending_transitions = [] :: [tuple()],
    metadata = #{} :: map()
}).

%%====================================================================
%% Types
%%====================================================================

-opaque place_coordinator() :: #place_coordinator{}.

-export_type([place_coordinator/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a place coordinator for a given place.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), atom()) -> {ok, pid()} | {error, term()}.

start_link(Place, NetMod) when is_atom(Place), is_atom(NetMod) ->
    CoordinatorName = place_coordinator_name(Place, NetMod),
    gen_server:start_link({local, CoordinatorName}, ?MODULE, {Place, NetMod}, []).

%%--------------------------------------------------------------------
%% @doc Registers a token with the place coordinator.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_token(atom(), atom(), {binary(), pid()}) -> ok | {error, term()}.

register_token(NetMod, Place, {TokenId, Pid}) when is_atom(NetMod), is_atom(Place) ->
    CoordinatorName = place_coordinator_name(Place, NetMod),
    gen_server:call(CoordinatorName, {register_token, TokenId, Pid}).

%%--------------------------------------------------------------------
%% @doc Unregisters a token from the place coordinator.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_token(atom(), atom()) -> ok.

unregister_token(NetMod, TokenId) when is_atom(NetMod), is_binary(TokenId) ->
    %% Find which place the token is in
    case token_registry:get_token_info(TokenId) of
        {ok, #token_info{place = Place}} ->
            CoordinatorName = place_coordinator_name(Place, NetMod),
            gen_server:call(CoordinatorName, {unregister_token, TokenId});
        {error, not_found} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Gets all tokens in a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_tokens(atom()) -> [binary()].

get_tokens(Place) when is_atom(Place) ->
    %% For the default net_mod
    get_tokens(undefined, Place).

%%--------------------------------------------------------------------
%% @doc Gets all tokens in a place for a specific network module.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_tokens(atom() | undefined, atom()) -> [binary()].

get_tokens(NetMod, Place) ->
    CoordinatorName = place_coordinator_name(Place, NetMod),
    gen_server:call(CoordinatorName, get_tokens).

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled for given tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec enable_transition(atom(), atom(), [binary()]) ->
          {ok, [binary()]} | {error, term()}.

enable_transition(NetMod, Transition, TokenIds) ->
    %% Find the place from the transition's preset
    %% This is a simplified implementation
    {ok, TokenIds}.

%%--------------------------------------------------------------------
%% @doc Notifies all tokens in a place of an event.
%%
%% @end
%%--------------------------------------------------------------------
-spec notify_tokens(atom(), term()) -> ok.

notify_tokens(Place, Message) when is_atom(Place) ->
    notify_tokens(undefined, Place, Message).

%%--------------------------------------------------------------------
%% @doc Notifies all tokens in a place for a specific network module.
%%
%% @end
%%--------------------------------------------------------------------
-spec notify_tokens(atom() | undefined, atom(), term()) -> ok.

notify_tokens(NetMod, Place, Message) ->
    CoordinatorName = place_coordinator_name(Place, NetMod),
    gen_server:cast(CoordinatorName, {notify_tokens, Message}).

%%--------------------------------------------------------------------
%% @doc Gets the state of a place coordinator.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(atom()) -> {ok, place_coordinator()} | {error, term()}.

get_state(Place) when is_atom(Place) ->
    get_state(undefined, Place).

%%--------------------------------------------------------------------
%% @doc Gets the state for a specific network module.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(atom() | undefined, atom()) -> {ok, place_coordinator()} | {error, term()}.

get_state(NetMod, Place) ->
    CoordinatorName = place_coordinator_name(Place, NetMod),
    gen_server:call(CoordinatorName, get_state).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init({atom(), atom()}) -> {ok, #place_coordinator{}}.

init({Place, NetMod}) ->
    logger:info("Place coordinator started for ~s in net ~p", [Place, NetMod]),
    {ok, #place_coordinator{
        place = Place,
        net_mod = NetMod
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #place_coordinator{}) ->
          {reply, term(), #place_coordinator{}}.

handle_call({register_token, TokenId, Pid}, _From,
            State = #place_coordinator{tokens = Tokens}) ->
    case maps:is_key(TokenId, Tokens) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            %% Monitor the token process
            erlang:monitor(process, Pid),
            State1 = State#place_coordinator{tokens = maps:put(TokenId, Pid, Tokens)},
            {reply, ok, State1}
    end;

handle_call({unregister_token, TokenId}, _From,
            State = #place_coordinator{tokens = Tokens}) ->
    State1 = State#place_coordinator{tokens = maps:remove(TokenId, Tokens)},
    {reply, ok, State1};

handle_call(get_tokens, _From, State = #place_coordinator{tokens = Tokens}) ->
    TokenIds = maps:keys(Tokens),
    {reply, TokenIds, State};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #place_coordinator{}) -> {noreply, #place_coordinator{}}.

handle_cast({notify_tokens, Message}, State = #place_coordinator{tokens = Tokens}) ->
    %% Send message to all tokens
    maps:foreach(
        fun(_TokenId, Pid) ->
            gen_server:cast(Pid, Message)
        end,
        Tokens
    ),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #place_coordinator{}) -> {noreply, #place_coordinator{}}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State = #place_coordinator{tokens = Tokens}) ->
    %% Remove the token from our registry
    Tokens1 = maps:filter(fun(_K, V) -> V =/= Pid end, Tokens),
    {noreply, State#place_coordinator{tokens = Tokens1}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), #place_coordinator{}, term()) -> {ok, #place_coordinator{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), #place_coordinator{}) -> ok.

terminate(_Reason, #place_coordinator{place = Place}) ->
    logger:info("Place coordinator stopping for ~s", [Place]),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec place_coordinator_name(atom(), atom() | undefined) -> atom().

place_coordinator_name(Place, undefined) ->
    list_to_existing_atom(atom_to_list(Place) ++ "_coordinator");
place_coordinator_name(Place, NetMod) when is_atom(NetMod) ->
    list_to_existing_atom(
        atom_to_list(NetMod) ++ "_" ++
        atom_to_list(Place) ++ "_coordinator"
    ).
