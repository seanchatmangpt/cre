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
%% @doc Circuit Breaker for Callback Failures
%%
%% This module implements a circuit breaker pattern to prevent
%% cascading failures when callback modules fail repeatedly.
%%
%% States:
%% - closed: Normal operation, requests pass through
%% - open: Circuit is tripped, requests fail immediately
%% - half_open: Testing if service has recovered
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_breaker).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([new/0, new/1]).
-export([check_breaker/1, check_breaker/2]).
-export([record_failure/1, record_failure/2]).
-export([record_success/1, record_success/2]).
-export([reset/1, reset/2]).
-export([get_state/1]).
-export([start_link/0, start_link/1]).
-export([stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Types
%%====================================================================

-type breaker_key() :: atom() | {atom(), term()}.
-type breaker_state() :: closed | open | half_open.

-record(breaker, {
    key :: breaker_key(),
    state :: breaker_state(),
    failure_count :: non_neg_integer(),
    success_count :: non_neg_integer(),
    last_failure_time :: integer() | undefined,
    last_state_change :: integer()
}).

-type breaker() :: #breaker{}.

-record(breaker_opts, {
    threshold :: pos_integer(),
    timeout :: pos_integer(),
    half_open_attempts :: pos_integer()
}).

-type opts() :: #breaker_opts{}.

-record(server_state, {
    breakers :: #{breaker_key() => breaker()},
    opts :: opts()
}).

-export_type([breaker/0, breaker_state/0, breaker_key/0, opts/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new circuit breaker with default options.
%%
%% Default options:
%% - threshold: 5 failures before opening
%% - timeout: 60000 ms (1 minute) before half-open
%% - half_open_attempts: 3 successes to close
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> breaker().
new() ->
    new(default).

%%--------------------------------------------------------------------
%% @doc Creates a new circuit breaker for the given key.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(breaker_key()) -> breaker().
new(Key) when is_atom(Key); is_tuple(Key) ->
    #breaker{
        key = Key,
        state = closed,
        failure_count = 0,
        success_count = 0,
        last_failure_time = undefined,
        last_state_change = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Checks if a request should be allowed through the breaker.
%%
%% Returns `allow' if requests should proceed, or `deny' if the
%% circuit is open and requests should fail fast.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_breaker(breaker_key()) -> allow | deny.
check_breaker(Key) ->
    check_breaker(Key, #breaker_opts{}).

%%--------------------------------------------------------------------
%% @doc Checks breaker with custom options.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_breaker(breaker_key(), opts() | undefined) -> allow | deny.
check_breaker(Key, Opts) when is_atom(Key); is_tuple(Key) ->
    case get_breaker_state(Key) of
        {ok, Breaker} ->
            check_breaker_state(Breaker, Opts);
        {error, not_found} ->
            %% No breaker exists, allow through
            allow
    end.

%%--------------------------------------------------------------------
%% @doc Records a failure for the given breaker key.
%%
%% May transition the breaker from closed -> open if threshold reached.
%%
%% @end
%%--------------------------------------------------------------------
-spec record_failure(breaker_key()) -> ok.
record_failure(Key) ->
    record_failure(Key, #breaker_opts{}).

%%--------------------------------------------------------------------
%% @doc Records a failure with custom options.
%%
%% @end
%%--------------------------------------------------------------------
-spec record_failure(breaker_key(), opts() | undefined) -> ok.
record_failure(Key, Opts) ->
    Threshold = case Opts of
        #breaker_opts{threshold = T} when T > 0 -> T;
        _ -> 5
    end,

    case get_breaker_state(Key) of
        {ok, Breaker = #breaker{failure_count = Count, state = closed}} when Count + 1 >= Threshold ->
            %% Transition to open
            NewBreaker = Breaker#breaker{
                state = open,
                failure_count = Count + 1,
                last_failure_time = erlang:system_time(millisecond),
                last_state_change = erlang:system_time(millisecond)
            },
            store_breaker(NewBreaker),
            logger:warning("Circuit breaker opened for ~p after ~p failures",
                          [Key, Count + 1]),
            ok;
        {ok, Breaker = #breaker{state = closed}} ->
            %% Increment failure count but stay closed
            NewBreaker = Breaker#breaker{
                failure_count = Breaker#breaker.failure_count + 1,
                last_failure_time = erlang:system_time(millisecond)
            },
            store_breaker(NewBreaker),
            ok;
        {ok, Breaker = #breaker{state = half_open}} ->
            %% Half-open failure, go back to open
            NewBreaker = Breaker#breaker{
                state = open,
                failure_count = Breaker#breaker.failure_count + 1,
                last_failure_time = erlang:system_time(millisecond),
                last_state_change = erlang:system_time(millisecond)
            },
            store_breaker(NewBreaker),
            logger:warning("Circuit breaker ~p failed in half-open, returning to open", [Key]),
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Records a success for the given breaker key.
%%
%% May transition the breaker from half_open -> closed.
%%
%% @end
%%--------------------------------------------------------------------
-spec record_success(breaker_key()) -> ok.
record_success(Key) ->
    record_success(Key, #breaker_opts{}).

%%--------------------------------------------------------------------
%% @doc Records a success with custom options.
%%
%% @end
%%--------------------------------------------------------------------
-spec record_success(breaker_key(), opts() | undefined) -> ok.
record_success(Key, Opts) ->
    HalfOpenAttempts = case Opts of
        #breaker_opts{half_open_attempts = A} when A > 0 -> A;
        _ -> 3
    end,

    case get_breaker_state(Key) of
        {ok, Breaker = #breaker{state = closed}} ->
            %% Reset failure count on success in closed state
            NewBreaker = Breaker#breaker{
                failure_count = 0,
                success_count = Breaker#breaker.success_count + 1
            },
            store_breaker(NewBreaker),
            ok;
        {ok, Breaker = #breaker{state = half_open, success_count = Count}}
                when Count + 1 >= HalfOpenAttempts ->
            %% Transition to closed
            NewBreaker = Breaker#breaker{
                state = closed,
                success_count = 0,
                failure_count = 0,
                last_state_change = erlang:system_time(millisecond)
            },
            store_breaker(NewBreaker),
            logger:info("Circuit breaker closed for ~p after ~p successful attempts",
                       [Key, Count + 1]),
            ok;
        {ok, Breaker = #breaker{state = half_open}} ->
            %% Increment success count but stay half-open
            NewBreaker = Breaker#breaker{
                success_count = Breaker#breaker.success_count + 1
            },
            store_breaker(NewBreaker),
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Resets a breaker to closed state.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(breaker_key()) -> ok.
reset(Key) ->
    reset(Key, undefined).

%%--------------------------------------------------------------------
%% @doc Resets a breaker with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(breaker_key(), opts() | undefined) -> ok.
reset(Key, _Opts) ->
    NewBreaker = new(Key),
    store_breaker(NewBreaker),
    logger:info("Circuit breaker reset for ~p", [Key]),
    ok.

%%--------------------------------------------------------------------
%% @doc Gets the current state of a breaker.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(breaker_key()) -> {ok, breaker_state()} | {error, not_found}.
get_state(Key) ->
    case get_breaker_state(Key) of
        {ok, Breaker} ->
            {ok, Breaker#breaker.state};
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Starts the circuit breaker server.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc Starts the circuit breaker server with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link([term()]) -> gen_server:start_ret().
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Stops the circuit breaker server.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(_Options) ->
    %% Use ETS for storing breaker state
    ets:new(yawl_breakers, [named_table, set, public]),
    {ok, #server_state{
        breakers = #{},
        opts = #breaker_opts{threshold = 5, timeout = 60000, half_open_attempts = 3}
    }}.

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
    ets:delete(yawl_breakers),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec get_breaker_state(breaker_key()) -> {ok, breaker()} | {error, not_found}.
get_breaker_state(Key) ->
    case ets:lookup(yawl_breakers, Key) of
        [{Key, Breaker}] ->
            {ok, Breaker};
        [] ->
            {error, not_found}
    end.

%% @private
-spec store_breaker(breaker()) -> true.
store_breaker(Breaker = #breaker{key = Key}) ->
    ets:insert(yawl_breakers, {Key, Breaker}).

%% @private
-spec check_breaker_state(breaker(), opts() | undefined) -> allow | deny.
check_breaker_state(#breaker{state = closed}, _Opts) ->
    allow;
check_breaker_state(#breaker{state = half_open}, _Opts) ->
    allow;
check_breaker_state(#breaker{state = open, last_state_change = LastChange}, Opts) ->
    Timeout = case Opts of
        #breaker_opts{timeout = T} when T > 0 -> T;
        _ -> 60000
    end,
    Now = erlang:system_time(millisecond),
    case Now - LastChange >= Timeout of
        true ->
            %% Transition to half-open
            allow;
        false ->
            deny
    end.
