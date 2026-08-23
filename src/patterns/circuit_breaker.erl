%% -*- erlang -*-
%%%% @doc circuit_breaker - Circuit Breaker pattern for preventing cascading failures.
%%
%% This module implements the Circuit Breaker pattern which prevents
%% cascading failures by stopping execution after a threshold of failures
%% is reached. The circuit breaker has three states:
%%
%% <ul>
%%   <li><b>Closed:</b> Normal operation, requests pass through</li>
%%   <li><b>Open:</b> Failed threshold reached, requests are rejected</li>
%%   <li><b>Half-Open:</b> Testing if service has recovered</li>
%% </ul>
%%
%% <h3>Configuration</h3>
%%
%% <ul>
%%   <li><b>failure_threshold:</b> Number of failures before opening (default: 5)</li>
%%   <li><b>timeout:</b> Milliseconds before transitioning to half-open (default: 60000)</li>
%%   <li><b>success_threshold:</b> Successes needed to close circuit (default: 2)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(circuit_breaker).
-author("CRE Team").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/2]).
-export([start_link/3]).
-export([execute/2]).
-export([execute/3]).
-export([reset/1]).
-export([get_state/1]).
-export([stop/1]).

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

-record(circuit_state, {
    state :: closed | open | half_open,
    failures = 0 :: non_neg_integer(),
    successes = 0 :: non_neg_integer(),
    last_failure_time :: undefined | integer(),
    last_state_change :: integer()
}).

-record(breaker_state, {
    name :: binary(),
    circuit :: #circuit_state{},
    config :: breaker_config(),
    monitored_pids :: sets:set(pid())
}).

-record(breaker_config, {
    failure_threshold = 5 :: pos_integer(),
    timeout_ms = 60000 :: pos_integer(),
    success_threshold = 2 :: pos_integer(),
    call_timeout = 5000 :: pos_integer()
}).

-type breaker_name() :: binary().
-type breaker_result() :: {ok, term()} | {error, circuit_open | timeout | term()}.
-type breaker_config() :: #breaker_config{}.
-type circuit_state() :: #circuit_state{}.

-export_type([breaker_name/0, breaker_result/0, breaker_config/0, circuit_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts a circuit breaker with default config.
-spec start_link(breaker_name(), fun()) -> {ok, pid()} | {error, term()}.

start_link(Name, Fun) when is_binary(Name), is_function(Fun) ->
    start_link(Name, Fun, []).  %% Pass empty proplist for default config

%% @doc Starts a circuit breaker with custom config.
-spec start_link(breaker_name(), fun(), proplists:proplist()) ->
          {ok, pid()} | {error, term()}.

start_link(Name, Fun, Options) when is_binary(Name), is_function(Fun) ->
    Config = parse_options(Options),
    gen_server:start_link({local, reg_name(Name)}, ?MODULE, {Name, Fun, Config}, []).

%% @doc Executes a function through the circuit breaker.
-spec execute(breaker_name(), fun()) -> breaker_result().

execute(Name, Fun) ->
    execute(Name, Fun, 5000).

%% @doc Executes a function with timeout through the circuit breaker.
-spec execute(breaker_name(), fun(), pos_integer()) -> breaker_result().

execute(Name, Fun, Timeout) ->
    case whereis(reg_name(Name)) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, {execute, Fun, Timeout}, infinity)
    end.

%% @doc Resets the circuit breaker to closed state.
-spec reset(breaker_name()) -> ok | {error, not_found}.

reset(Name) ->
    case whereis(reg_name(Name)) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, reset)
    end.

%% @doc Gets the current circuit state.
-spec get_state(breaker_name()) -> {ok, circuit_state()} | {error, not_found}.

get_state(Name) ->
    case whereis(reg_name(Name)) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, get_state)
    end.

%% @doc Stops the circuit breaker.
-spec stop(breaker_name()) -> ok.

stop(Name) ->
    case whereis(reg_name(Name)) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init({Name, _Fun, Config}) ->
    Circuit = #circuit_state{
        state = closed,
        failures = 0,
        successes = 0,
        last_state_change = erlang:system_time(millisecond)
    },
    {ok, #breaker_state{
        name = Name,
        circuit = Circuit,
        config = Config,
        monitored_pids = sets:new()
    }}.

%% @private
handle_call({execute, Fun, Timeout}, _From, State) ->
    #breaker_state{circuit = Circuit, config = Config} = State,
    case Circuit#circuit_state.state of
        closed ->
            execute_call(Fun, Timeout, State);
        open ->
            Now = erlang:system_time(millisecond),
            TimeSinceFailure = Now - Circuit#circuit_state.last_failure_time,
            TimeoutMs = Config#breaker_config.timeout_ms,
            if
                TimeSinceFailure >= TimeoutMs ->
                    %% Transition to half-open
                    NewCircuit = Circuit#circuit_state{
                        state = half_open,
                        successes = 0,
                        last_state_change = Now
                    },
                    execute_call(Fun, Timeout, State#breaker_state{circuit = NewCircuit});
                true ->
                    {reply, {error, circuit_open}, State}
            end;
        half_open ->
            execute_call(Fun, Timeout, State)
    end;

handle_call(reset, _From, State) ->
    NewCircuit = #circuit_state{
        state = closed,
        failures = 0,
        successes = 0,
        last_state_change = erlang:system_time(millisecond)
    },
    {reply, ok, State#breaker_state{circuit = NewCircuit}};

handle_call(get_state, _From, State) ->
    {reply, {ok, State#breaker_state.circuit}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
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

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
execute_call(Fun, Timeout, State) ->
    #breaker_state{circuit = Circuit, config = Config} = State,
    FailThreshold = Config#breaker_config.failure_threshold,
    SuccessThreshold = Config#breaker_config.success_threshold,

    try apply_with_timeout(Fun, Timeout) of
        {ok, Result} ->
            %% Success path
            SuccessCircuit = case Circuit#circuit_state.state of
                half_open ->
                    NewSuccesses = Circuit#circuit_state.successes + 1,
                    if
                        NewSuccesses >= SuccessThreshold ->
                            %% Close the circuit
                            ?LOG_INFO("Circuit breaker ~s closed after ~p successes",
                                      [State#breaker_state.name, NewSuccesses]),
                            Circuit#circuit_state{
                                state = closed,
                                failures = 0,
                                successes = 0,
                                last_state_change = erlang:system_time(millisecond)
                            };
                        true ->
                            Circuit#circuit_state{successes = NewSuccesses}
                    end;
                closed ->
                    Circuit#circuit_state{failures = 0};
                open ->
                    Circuit
            end,
            {reply, {ok, Result}, State#breaker_state{circuit = SuccessCircuit}}
    catch
        Type:Error:Stack ->
            %% Failure path
            ?LOG_WARNING("Circuit breaker ~s execution failed: ~p:~p",
                         [State#breaker_state.name, Type, Error]),
            ?LOG_DEBUG("Stack: ~p", [Stack]),
            NewFailures = Circuit#circuit_state.failures + 1,
            FailureCircuit = if
                NewFailures >= FailThreshold ->
                    %% Open the circuit
                    ?LOG_WARNING("Circuit breaker ~s opened after ~p failures",
                                  [State#breaker_state.name, NewFailures]),
                    Circuit#circuit_state{
                        state = open,
                        failures = NewFailures,
                        successes = 0,
                        last_failure_time = erlang:system_time(millisecond),
                        last_state_change = erlang:system_time(millisecond)
                    };
                true ->
                    Circuit#circuit_state{
                        failures = NewFailures,
                        last_failure_time = erlang:system_time(millisecond)
                    }
            end,
            {reply, {error, {Type, Error}}, State#breaker_state{circuit = FailureCircuit}}
    end.

%% @private
apply_with_timeout(Fun, Timeout) ->
    Pid = self(),
    Ref = make_ref(),
    SpawnFun = fun() ->
        try
            Result = Fun(),
            Pid ! {Ref, {ok, Result}}
        catch
            Type:Error:Stack ->
                Pid ! {Ref, {error, {Type, Error, Stack}}}
        end
    end,
    spawn_monitor(SpawnFun),
    receive
        {Ref, {ok, Result}} ->
            {ok, Result};
        {Ref, {error, {Type, Error, _Stack}}} ->
            erlang:raise(Type, Error);
        {'DOWN', _MRef, process, _Pid, Reason} ->
            erlang:error(Reason)
    after Timeout ->
        erlang:error(timeout)
    end.

%% @private
reg_name(Name) ->
    binary_to_atom(<<"circuit_breaker_", Name/binary>>, utf8).

%% @private
parse_options(Options) ->
    #breaker_config{
        failure_threshold = proplists:get_value(failure_threshold, Options, 5),
        timeout_ms = proplists:get_value(timeout_ms, Options, 60000),
        success_threshold = proplists:get_value(success_threshold, Options, 2),
        call_timeout = proplists:get_value(call_timeout, Options, 5000)
    }.
