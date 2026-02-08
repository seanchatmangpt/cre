%% -*- erlang -*-
%%%% @doc gen_active_token - Active Token behavior specification.
%%
%% This module defines the behavior for active tokens in the Active Petri Net
%% architecture. Active tokens are autonomous actors with location, history,
%% and communication capabilities, extending the passive token model of
%% traditional Petri nets.
%%
%% <h3>Token Lifecycle States</h3>
%%
%% <ul>
%%   <li><b>unborn:</b> Token not yet created</li>
%%   <li><b>initializing:</b> Token being set up</li>
%%   <li><b>idle:</b> Token created but not registered</li>
%%   <li><b>place_registered:</b> Token registered with a place</li>
%%   <li><b>participating:</b> Token actively participating in transitions</li>
%%   <li><b>migrating:</b> Token moving between places</li>
%%   <li><b>communicating:</b> Token engaged in communication</li>
%%   <li><b>expiring:</b> Token being cleaned up</li>
%%   <li><b>terminated:</b> Token stopped gracefully</li>
%%   <li><b>dead:</b> Token cannot be revived</li>
%% </ul>
%%
%% <h3>Recovery States</h3>
%%
%% <ul>
%%   <li><b>failed:</b> Token encountered error, awaiting recovery</li>
%%   <li><b>recovering:</b> Token is recovering from failure</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_active_token).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Token lifecycle API
-export([start_link/4]).
-export([spawn/4]).
-export([migrate/2]).
-export([communicate/2]).
-export([expire/1]).
-export([terminate/1]).
-export([get_state/1]).
-export([get_history/1]).
-export([add_metadata/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Active Token State Record
%%
%% Tokens are autonomous actors with:
%% - Unique ID (UUID)
%% - Process ID for communication
%% - Current location (place)
%% - Payload data
%% - History of events
%% - Current state
%% - Creation timestamp
%% - Parent token ID (for spawned tokens)
%% - Metadata map
%%--------------------------------------------------------------------
-record(active_token, {
    id :: binary(),                           %% UUID
    pid :: pid() | undefined,                 %% Token process
    place :: atom(),                          %% Current location
    payload :: term(),                        %% Token data
    history = [] :: [token_event()],          %% Path history
    state = unborn :: token_state(),           %% Current state
    created_at :: integer(),                  %% Creation timestamp
    parent_id :: binary() | undefined,        %% Parent for spawned tokens
    metadata = #{} :: map()                   %% Extended metadata
}).

%%--------------------------------------------------------------------
%% @doc Token Event for History Tracking
%%--------------------------------------------------------------------
-record(token_event, {
    timestamp :: integer(),
    event_type :: atom(),
    from_place :: atom() | undefined,
    to_place :: atom() | undefined,
    metadata = #{} :: map()
}).

%%--------------------------------------------------------------------
%% @doc Token Message Types
%%--------------------------------------------------------------------
-record(token_msg, {
    from :: binary(),
    type :: atom(),
    payload :: term()
}).

%%====================================================================
%% Types
%%====================================================================

-type active_token() :: #active_token{}.
-type token_event() :: #token_event{}.
-type token_msg() :: #token_msg{}.
-type token_state() ::
    unborn |
    initializing |
    idle |
    place_registered |
    participating |
    migrating |
    communicating |
    expiring |
    terminated |
    dead |
    failed |
    recovering.

-type token_option() ::
    {parent_id, binary()} |
    {metadata, map()} |
    {history, [token_event()]}.

-export_type([
    active_token/0,
    token_event/0,
    token_msg/0,
    token_state/0,
    token_option/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts an active token as a gen_server.
%%
%% @param Place Initial place for the token
%% @param Payload Token data payload
%% @param Options Token options
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), term(), [token_option()], [gen_server:option()]) ->
          {ok, pid()} | {error, term()}.

start_link(Place, Payload, Options, GenServerOpts) when is_atom(Place), is_list(Options) ->
    TokenId = generate_token_id(),
    gen_server:start_link(?MODULE, {TokenId, Place, Payload, Options}, GenServerOpts).

%%--------------------------------------------------------------------
%% @doc Spawns a new token with optional parent.
%%
%% Similar to start_link but registers with the place coordinator.
%%
%% @end
%%--------------------------------------------------------------------
-spec spawn(atom(), term(), [token_option()], [gen_server:option()]) ->
          {ok, pid(), binary()} | {error, term()}.

spawn(Place, Payload, Options, GenServerOpts) ->
    TokenId = generate_token_id(),
    ParentId = proplists:get_value(parent_id, Options),
    Token = #active_token{
        id = TokenId,
        place = Place,
        payload = Payload,
        created_at = erlang:system_time(millisecond),
        parent_id = ParentId,
        state = initializing,
        metadata = proplists:get_value(metadata, Options, #{})
    },
    case gen_server:start(?MODULE, {Token, []}, GenServerOpts) of
        {ok, Pid} ->
            %% Register with place coordinator
            case register_with_place(Pid, Place, TokenId) of
                ok ->
                    {ok, Pid, TokenId};
                {error, Reason} ->
                    gen_server:stop(Pid),
                    {error, {place_registration_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Migrates a token to a new place.
%%
%% @param TokenPid Token process
%% @param NewPlace Target place
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate(pid(), atom()) -> ok | {error, term()}.

migrate(TokenPid, NewPlace) when is_pid(TokenPid), is_atom(NewPlace) ->
    gen_server:call(TokenPid, {migrate, NewPlace}, 5000).

%%--------------------------------------------------------------------
%% @doc Sends a message to another token.
%%
%% @param TokenPid Source token process
%% @param Message Message to send
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec communicate(pid(), token_msg()) -> ok | {error, term()}.

communicate(TokenPid, Message) when is_pid(TokenPid), is_record(Message, token_msg) ->
    gen_server:call(TokenPid, {communicate, Message}, 5000).

%%--------------------------------------------------------------------
%% @doc Expires a token, starting graceful shutdown.
%%
%% @end
%%--------------------------------------------------------------------
-spec expire(pid()) -> ok | {error, term()}.

expire(TokenPid) when is_pid(TokenPid) ->
    gen_server:call(TokenPid, expire, 5000).

%%--------------------------------------------------------------------
%% @doc Terminates a token immediately.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(pid()) -> ok.

terminate(TokenPid) when is_pid(TokenPid) ->
    gen_server:stop(TokenPid, normal, 5000).

%%--------------------------------------------------------------------
%% @doc Gets the current state of a token.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(pid()) -> {ok, active_token()} | {error, term()}.

get_state(TokenPid) when is_pid(TokenPid) ->
    gen_server:call(TokenPid, get_state, 5000).

%%--------------------------------------------------------------------
%% @doc Gets the history of a token.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_history(pid()) -> {ok, [token_event()]} | {error, term()}.

get_history(TokenPid) when is_pid(TokenPid) ->
    gen_server:call(TokenPid, get_history, 5000).

%%--------------------------------------------------------------------
%% @doc Adds metadata to a token.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_metadata(pid(), map()) -> ok | {error, term()}.

add_metadata(TokenPid, Metadata) when is_pid(TokenPid), is_map(Metadata) ->
    gen_server:call(TokenPid, {add_metadata, Metadata}, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init({binary(), #active_token{}} | {binary(), atom(), term(), [token_option()]}) ->
          {ok, #active_token{}}.

init({TokenId, Place, Payload, Options}) ->
    %% Initialize with parameters
    ParentId = proplists:get_value(parent_id, Options),
    Metadata = proplists:get_value(metadata, Options, #{}),
    History = proplists:get_value(history, Options, []),

    Token = #active_token{
        id = TokenId,
        pid = self(),
        place = Place,
        payload = Payload,
        history = History,
        state = initializing,
        created_at = erlang:system_time(millisecond),
        parent_id = ParentId,
        metadata = Metadata
    },
    {ok, transition_state(Token, place_registered)};

init({Token, []}) when is_record(Token, active_token) ->
    {ok, Token#active_token{state = place_registered, pid = self()}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #active_token{}) ->
          {reply, term(), #active_token{}} | {noreply, #active_token{}} |
          {stop, term(), term(), #active_token{}}.

handle_call({migrate, NewPlace}, _From, Token = #active_token{place = CurrentPlace, history = History}) ->
    %% Record migration event
    Event = #token_event{
        timestamp = erlang:system_time(millisecond),
        event_type = migrate,
        from_place = CurrentPlace,
        to_place = NewPlace
    },
    Token1 = Token#active_token{
        place = NewPlace,
        history = [Event | History],
        state = participating
    },
    {reply, ok, Token1};

handle_call({communicate, #token_msg{from = From, type = Type, payload = Payload}}, _From,
            Token = #active_token{history = History, metadata = Meta}) ->
    %% Record communication event
    Event = #token_event{
        timestamp = erlang:system_time(millisecond),
        event_type = {communicate, Type},
        from_place = From,
        metadata = #{payload => Payload}
    },
    Token1 = Token#active_token{
        history = [Event | History],
        metadata = maps:put(<<"last_communication">>, erlang:system_time(millisecond), Meta)
    },
    {reply, ok, Token1};

handle_call(expire, _From, Token = #active_token{state = State}) when State =/= dead ->
    Token1 = transition_state(Token, expiring),
    {reply, ok, Token1};

handle_call(expire, _From, Token) ->
    {reply, {error, already_dead}, Token};

handle_call(get_state, _From, Token) ->
    {reply, {ok, Token}, Token};

handle_call(get_history, _From, Token = #active_token{history = History}) ->
    {reply, {ok, lists:reverse(History)}, Token};

handle_call({add_metadata, NewMetadata}, _From, Token = #active_token{metadata = Meta}) ->
    Token1 = Token#active_token{metadata = maps:merge(Meta, NewMetadata)},
    {reply, ok, Token1};

handle_call(_Request, _From, Token) ->
    {reply, {error, unknown_request}, Token}.

%% @private
-spec handle_cast(term(), #active_token{}) ->
          {noreply, #active_token{}} | {stop, term(), #active_token{}}.

handle_cast({transition, NewState}, Token) ->
    {noreply, transition_state(Token, NewState)};

handle_cast({record_event, Event}, Token = #active_token{history = History}) ->
    {noreply, Token#active_token{history = [Event | History]}};

handle_cast(_Msg, Token) ->
    {noreply, Token}.

%% @private
-spec handle_info(term(), #active_token{}) ->
          {noreply, #active_token{}} | {stop, term(), #active_token{}}.

handle_info(timeout, Token = #active_token{state = migrating}) ->
    %% Migration timeout - transition to failed
    {noreply, transition_state(Token, failed)};

handle_info({'EXIT', _Pid, Reason}, Token = #active_token{state = State}) ->
    case State of
        participating ->
            %% Unexpected death during participation
            {noreply, transition_state(Token, failed)};
        _ ->
            {stop, Reason, Token}
    end;

handle_info(_Info, Token) ->
    {noreply, Token}.

%% @private
-spec code_change(term(), #active_token{}, term()) ->
          {ok, #active_token{}}.

code_change(_OldVsn, Token, _Extra) ->
    {ok, Token}.

%% @private
-spec terminate(term(), #active_token{}) -> ok.

terminate(_Reason, #active_token{id = TokenId, place = Place}) ->
    %% Unregister from place coordinator
    _ = unregister_from_place(self(), Place, TokenId),
    logger:debug("Active token ~s terminated", [TokenId]),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec transition_state(#active_token{}, token_state()) -> #active_token{}.

transition_state(Token = #active_token{history = History}, NewState) ->
    Event = #token_event{
        timestamp = erlang:system_time(millisecond),
        event_type = state_change,
        metadata = #{
            from => Token#active_token.state,
            to => NewState
        }
    },
    Token#active_token{
        state = NewState,
        history = [Event | History]
    }.

%% @private
-spec generate_token_id() -> binary().

generate_token_id() ->
    Time = erlang:system_time(microsecond),
    Unique = erlang:unique_integer([positive]),
    <<Time:64, Unique:64>>.

%% @private
-spec register_with_place(pid(), atom(), binary()) -> ok | {error, term()}.

register_with_place(_Pid, _Place, _TokenId) ->
    %% TODO: Integrate with place_coordinator
    ok.

%% @private
-spec unregister_from_place(pid(), atom(), binary()) -> ok.

unregister_from_place(_Pid, _Place, _TokenId) ->
    %% TODO: Integrate with place_coordinator
    ok.
