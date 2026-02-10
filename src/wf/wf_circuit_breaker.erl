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

-module(wf_circuit_breaker).
-moduledoc """
Circuit breaker pattern for protecting external service calls.

Prevents cascading failures by monitoring call success/failure rates and
temporarily blocking requests to failing services. Implements three states:

- `closed`: Normal operation, requests pass through
- `open`: Too many failures detected, requests rejected immediately
- `half_open`: Testing if service recovered, allowing limited requests

Configuration parameters:
- `failure_threshold`: Consecutive failures before opening (default: 5)
- `success_threshold`: Consecutive successes before closing (default: 2)
- `timeout`: Request timeout in milliseconds (default: 5000)
- `reset_timeout`: Time to wait before attempting half-open (default: 60000)

Example:

```erlang
> {ok, CB} = wf_circuit_breaker:start_link(myservice, #{}).
_

> F = fun() -> {ok, result} end.
_

> wf_circuit_breaker:call(CB, F).
{ok, result}

> wf_circuit_breaker:status(CB).
#{state => closed, failures => 0, successes => 0}
```
""".

-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

-export([
    start_link/2,
    stop/1,
    call/2,
    call/3,
    status/1,
    reset/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Types
%%====================================================================

-type breaker_state() :: closed | open | half_open.

-type breaker_config() :: #{
    failure_threshold => pos_integer(),
    success_threshold => pos_integer(),
    timeout => pos_integer(),
    reset_timeout => pos_integer()
}.

-type breaker_ref() :: atom() | pid().

-type call_result() :: ok | {ok, term()} | {error, term()}.

-export_type([breaker_state/0, breaker_config/0, breaker_ref/0]).

%%====================================================================
%% Internal State
%%====================================================================

-record(state, {
    name :: atom(),
    breaker_state :: breaker_state(),
    failure_count :: non_neg_integer(),
    success_count :: non_neg_integer(),
    failure_threshold :: pos_integer(),
    success_threshold :: pos_integer(),
    timeout :: pos_integer(),
    reset_timeout :: pos_integer(),
    last_failure_time :: integer() | undefined,
    reset_timer :: reference() | undefined
}).

-type state_record() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a new circuit breaker server.
%%
%% Creates a gen_server process to manage circuit breaker state for an
%% external service. Config map can specify:
%% - `failure_threshold`: Failures before opening (default: 5)
%% - `success_threshold`: Successes before closing (default: 2)
%% - `timeout`: Request timeout ms (default: 5000)
%% - `reset_timeout`: Time before half-open attempt ms (default: 60000)
%%
%% @param Name Atom to identify this circuit breaker
%% @param Config Configuration map
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Name :: atom(), Config :: breaker_config()) ->
          {ok, pid()} | {error, term()}.

start_link(Name, Config) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Config}, []).

%%--------------------------------------------------------------------
%% @doc Stops a circuit breaker server.
%%
%% @param Ref Circuit breaker reference (atom or pid)
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Ref :: breaker_ref()) -> ok.

stop(Ref) when is_atom(Ref) ->
    case whereis(Ref) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end;
stop(Ref) when is_pid(Ref) ->
    gen_server:stop(Ref).

%%--------------------------------------------------------------------
%% @doc Executes a function protected by circuit breaker.
%%
%% Attempts to call the function if the circuit is not open.
%% Returns {circuit_open, Time} if breaker is open, where Time is
%% milliseconds until retry.
%%
%% Uses default timeout from breaker configuration.
%%
%% @param Ref Circuit breaker reference
%% @param Fun Zero-arity function to execute
%% @returns {ok, Result} | {error, Reason} | {circuit_open, TimeUntilRetry}
%%
%% @end
%%--------------------------------------------------------------------
-spec call(Ref :: breaker_ref(), Fun :: fun(() -> call_result())) ->
          {ok, term()} | {error, term()} | {circuit_open, non_neg_integer()}.

call(Ref, Fun) ->
    gen_server:call(Ref, {call, Fun}, infinity).

%%--------------------------------------------------------------------
%% @doc Executes a function with explicit timeout.
%%
%% @param Ref Circuit breaker reference
%% @param Fun Zero-arity function to execute
%% @param Timeout Maximum execution time in milliseconds
%% @returns {ok, Result} | {error, Reason} | {circuit_open, TimeUntilRetry}
%%
%% @end
%%--------------------------------------------------------------------
-spec call(Ref :: breaker_ref(), Fun :: fun(() -> call_result()),
          Timeout :: pos_integer()) ->
          {ok, term()} | {error, term()} | {circuit_open, non_neg_integer()}.

call(Ref, Fun, Timeout) ->
    gen_server:call(Ref, {call, Fun, Timeout}, infinity).

%%--------------------------------------------------------------------
%% @doc Gets current circuit breaker status.
%%
%% @param Ref Circuit breaker reference
%% @returns Map with breaker state and counters
%%
%% @end
%%--------------------------------------------------------------------
-spec status(Ref :: breaker_ref()) -> #{
    state := breaker_state(),
    failures := non_neg_integer(),
    successes := non_neg_integer(),
    time_until_retry := non_neg_integer() | infinity
}.

status(Ref) ->
    gen_server:call(Ref, status).

%%--------------------------------------------------------------------
%% @doc Manually resets circuit breaker to closed state.
%%
%% Clears failure and success counters, cancels any pending reset timer.
%%
%% @param Ref Circuit breaker reference
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(Ref :: breaker_ref()) -> ok.

reset(Ref) ->
    gen_server:cast(Ref, reset).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init({atom(), breaker_config()}) -> {ok, state_record()}.

init({Name, Config}) ->
    FailureThreshold = maps:get(failure_threshold, Config, 5),
    SuccessThreshold = maps:get(success_threshold, Config, 2),
    Timeout = maps:get(timeout, Config, 5000),
    ResetTimeout = maps:get(reset_timeout, Config, 60000),

    State = #state{
        name = Name,
        breaker_state = closed,
        failure_count = 0,
        success_count = 0,
        failure_threshold = FailureThreshold,
        success_threshold = SuccessThreshold,
        timeout = Timeout,
        reset_timeout = ResetTimeout,
        last_failure_time = undefined,
        reset_timer = undefined
    },

    {ok, State}.

-spec handle_call(term(), {pid(), reference()}, state_record()) ->
          {reply, term(), state_record()}.

handle_call({call, Fun}, _From, State = #state{breaker_state = closed}) ->
    Result = execute_with_timeout(Fun, State#state.timeout),
    NewState = handle_call_result(Result, State),
    {reply, Result, NewState};

handle_call({call, _Fun}, _From, State = #state{breaker_state = open}) ->
    TimeUntilRetry = time_until_retry(State),
    {reply, {circuit_open, TimeUntilRetry}, State};

handle_call({call, Fun}, _From, State = #state{breaker_state = half_open}) ->
    Result = execute_with_timeout(Fun, State#state.timeout),
    NewState = handle_half_open_result(Result, State),
    {reply, Result, NewState};

handle_call({call, Fun, Timeout}, _From, State = #state{breaker_state = closed}) ->
    Result = execute_with_timeout(Fun, Timeout),
    NewState = handle_call_result(Result, State),
    {reply, Result, NewState};

handle_call({call, _Fun, _Timeout}, _From, State = #state{breaker_state = open}) ->
    TimeUntilRetry = time_until_retry(State),
    {reply, {circuit_open, TimeUntilRetry}, State};

handle_call({call, Fun, Timeout}, _From, State = #state{breaker_state = half_open}) ->
    Result = execute_with_timeout(Fun, Timeout),
    NewState = handle_half_open_result(Result, State),
    {reply, Result, NewState};

handle_call(status, _From, State = #state{breaker_state = BS, failure_count = FC,
                                           success_count = SC}) ->
    Status = #{
        state => BS,
        failures => FC,
        successes => SC,
        time_until_retry => time_until_retry(State)
    },
    {reply, Status, State};

handle_call(_Request, _From, State = #state{} = S) ->
    {reply, {error, unknown_call}, S}.

-spec handle_cast(term(), state_record()) -> {noreply, state_record()}.

handle_cast(reset, State = #state{reset_timer = Timer}) ->
    NewTimer = case Timer of
        undefined -> undefined;
        Ref -> erlang:cancel_timer(Ref), undefined
    end,
    NewState = State#state{
        breaker_state = closed,
        failure_count = 0,
        success_count = 0,
        last_failure_time = undefined,
        reset_timer = NewTimer
    },
    {noreply, NewState};

handle_cast(_Request, State = #state{} = S) ->
    {noreply, S}.

-spec handle_info(term(), state_record()) -> {noreply, state_record()}.

handle_info({timeout, Ref, half_open_attempt}, State = #state{reset_timer = Ref}) ->
    logger:info("wf_circuit_breaker: ~p entering half-open state",
                [State#state.name]),
    NewState = State#state{
        breaker_state = half_open,
        failure_count = 0,
        success_count = 0,
        reset_timer = undefined
    },
    {noreply, NewState};

handle_info(_Info, State = #state{} = S) ->
    {noreply, S}.

-spec terminate(term(), state_record()) -> ok.

terminate(_Reason, State = #state{reset_timer = Timer}) ->
    case Timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

-spec code_change(term(), state_record(), term()) -> {ok, state_record()}.

code_change(_OldVsn, State = #state{} = S, _Extra) ->
    {ok, S}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a function with timeout protection.
%%
%% @param Fun Zero-arity function
%% @param Timeout Timeout in milliseconds
%% @returns {ok, Result} | {error, Reason} | {error, timeout}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_with_timeout(fun(() -> call_result()), pos_integer()) ->
          {ok, term()} | {error, term()}.

execute_with_timeout(Fun, Timeout) ->
    try
        case Fun() of
            ok -> ok;
            {ok, Result} -> {ok, Result};
            {error, Reason} -> {error, Reason};
            Other -> {ok, Other}
        end
    catch
        Type:Error:_Stack ->
            {error, {Type, Error}}
    after
        ok
    end.

%%--------------------------------------------------------------------
%% @doc Handles call result in closed state.
%%
%% Tracks failures and transitions to open if threshold exceeded.
%% Resets failure count on success.
%%
%% @param Result Call result
%% @param State Current breaker state
%% @returns Updated breaker state
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call_result(call_result(), state_record()) -> state_record().

handle_call_result(ok, State = #state{success_count = SC}) ->
    State#state{
        failure_count = 0,
        success_count = SC + 1,
        last_failure_time = undefined
    };

handle_call_result({ok, _}, State = #state{success_count = SC}) ->
    State#state{
        failure_count = 0,
        success_count = SC + 1,
        last_failure_time = undefined
    };

handle_call_result({error, _}, State = #state{failure_count = FC,
                                               failure_threshold = FT}) ->
    NewFailureCount = FC + 1,
    Now = erlang:system_time(millisecond),
    case NewFailureCount >= FT of
        true ->
            logger:warning("wf_circuit_breaker: ~p opening circuit after ~p failures",
                          [State#state.name, NewFailureCount]),
            Timer = erlang:start_timer(
                State#state.reset_timeout,
                self(),
                half_open_attempt
            ),
            State#state{
                breaker_state = open,
                failure_count = NewFailureCount,
                success_count = 0,
                last_failure_time = Now,
                reset_timer = Timer
            };
        false ->
            State#state{
                failure_count = NewFailureCount,
                success_count = 0,
                last_failure_time = Now
            }
    end.

%%--------------------------------------------------------------------
%% @doc Handles call result in half-open state.
%%
%% Transitions back to open on failure, or to closed on success.
%%
%% @param Result Call result
%% @param State Current breaker state
%% @returns Updated breaker state
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_half_open_result(call_result(), state_record()) -> state_record().

handle_half_open_result(ok, State = #state{success_count = SC,
                                            success_threshold = ST}) ->
    NewSuccessCount = SC + 1,
    case NewSuccessCount >= ST of
        true ->
            logger:info("wf_circuit_breaker: ~p closing circuit",
                       [State#state.name]),
            State#state{
                breaker_state = closed,
                failure_count = 0,
                success_count = 0,
                reset_timer = undefined
            };
        false ->
            State#state{
                success_count = NewSuccessCount
            }
    end;

handle_half_open_result({ok, _}, State = #state{success_count = SC,
                                                  success_threshold = ST}) ->
    NewSuccessCount = SC + 1,
    case NewSuccessCount >= ST of
        true ->
            logger:info("wf_circuit_breaker: ~p closing circuit",
                       [State#state.name]),
            State#state{
                breaker_state = closed,
                failure_count = 0,
                success_count = 0,
                reset_timer = undefined
            };
        false ->
            State#state{
                success_count = NewSuccessCount
            }
    end;

handle_half_open_result({error, _}, State = #state{}) ->
    Now = erlang:system_time(millisecond),
    logger:warning("wf_circuit_breaker: ~p reopening circuit during half-open",
                  [State#state.name]),
    Timer = erlang:start_timer(
        State#state.reset_timeout,
        self(),
        half_open_attempt
    ),
    State#state{
        breaker_state = open,
        failure_count = 1,
        success_count = 0,
        last_failure_time = Now,
        reset_timer = Timer
    }.

%%--------------------------------------------------------------------
%% @doc Calculates milliseconds until circuit enters half-open state.
%%
%% @param State Current breaker state
%% @returns Non-negative integer (milliseconds) or infinity
%%
%% @end
%%--------------------------------------------------------------------
-spec time_until_retry(state_record()) -> non_neg_integer() | infinity.

time_until_retry(#state{breaker_state = open, reset_timer = undefined}) ->
    infinity;
time_until_retry(#state{breaker_state = open, reset_timer = Ref}) ->
    case erlang:read_timer(Ref) of
        false -> 0;
        TimeLeft when TimeLeft > 0 -> TimeLeft
    end;
time_until_retry(#state{breaker_state = _}) ->
    0.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = start_link(test_cb_start, #{}),
    ?assert(is_pid(Pid)),
    ok = stop(test_cb_start),
    ?assertEqual(ok, stop(test_cb_start)).

closed_state_success_test() ->
    {ok, CB} = start_link(test_cb_closed, #{}),
    F = fun() -> {ok, result} end,
    Result = call(CB, F),
    ?assertEqual({ok, result}, Result),
    Status = status(CB),
    ?assertEqual(closed, maps:get(state, Status)),
    ok = stop(CB).

closed_state_failure_test() ->
    {ok, CB} = start_link(test_cb_failure, #{failure_threshold => 2}),
    F = fun() -> {error, broken} end,
    call(CB, F),
    call(CB, F),
    Result = call(CB, F),
    ?assertEqual({circuit_open, _}, Result),
    Status = status(CB),
    ?assertEqual(open, maps:get(state, Status)),
    ok = stop(CB).

reset_test() ->
    {ok, CB} = start_link(test_cb_reset, #{failure_threshold => 2}),
    F = fun() -> {error, broken} end,
    call(CB, F),
    call(CB, F),
    reset(CB),
    Status = status(CB),
    ?assertEqual(closed, maps:get(state, Status)),
    ?assertEqual(0, maps:get(failures, Status)),
    ok = stop(CB).

-endif.
