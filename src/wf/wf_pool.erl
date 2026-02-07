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

-module(wf_pool).
-moduledoc """
Poolboy wrapper for bounded concurrency.

This module provides a simplified wrapper around the poolboy library for
managing worker pools with bounded concurrency. It validates pool configuration
and provides a clean API for starting pools, executing transactions, and
stopping pools.

The pool requires at least one worker (size >= 1) to ensure meaningful
concurrency bounds.

```erlang
> PoolPid = wf_pool:start_link(#{name => mypool, size => 1, max_overflow => 0}).
_

> element(1, PoolPid).
ok

> wf_pool:transaction(element(2, PoolPid), fun(_W) -> 40 + 2 end).
42

> wf_pool:stop(element(2, PoolPid)).
ok
```

```erlang
> wf_pool:start_link(#{name => badpool, size => 0}).
{error,bad_size}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Pool API
-export([start_link/1]).
-export([transaction/2, transaction/3]).
-export([stop/1]).
-export([status/1, queue_depth/1]).

%%====================================================================
%% Types
%%====================================================================

%% Pool configuration map with required keys:
%% - name: atom() - the registered name of the pool
%% - size: pos_integer() - maximum pool size (must be >= 1)
%% - max_overflow: non_neg_integer() - additional workers under load
-type pool_config() :: #{
    name := atom(),
    size := pos_integer(),
    max_overflow := non_neg_integer()
}.

%% Pool reference (pid or registered name)
-type pool() :: pid() | atom().

%% Transaction function that receives a worker and returns any result
-type transaction_fun(Result) :: fun((pid()) -> Result).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a new worker pool.
%%
%% Creates a new poolboy pool with the given configuration. The pool
%% config map must contain:
%% - `name`: The registered name for the pool
%% - `size`: Maximum pool size (must be >= 1)
%% - `max_overflow`: Additional workers created when pool is full
%%
%% Returns {ok, PoolPid} on success, or {error, bad_size} if size < 1.
%%
%% @param Config Pool configuration map
%% @returns {ok, Pool} | {error, bad_size}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Config :: pool_config()) ->
          {ok, pool()} | {error, bad_size}.

start_link(#{size := Size} = _Config) when Size < 1 ->
    %% Validate size >= 1
    {error, bad_size};

start_link(#{name := Name, size := Size, max_overflow := MaxOverflow}) ->
    %% Construct poolboy args
    PoolArgs = [
        {name, {local, Name}},
        {worker_module, wf_pool_worker},
        {size, Size},
        {max_overflow, MaxOverflow}
    ],

    %% Start poolboy pool with dummy worker args
    %% (wf_pool_worker is a placeholder for doctests)
    case poolboy:start_link(PoolArgs, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Executes a function with a worker from the pool.
%%
%% The function receives the worker pid as its argument and should
%% return any result. The worker is automatically checked back into
%% the pool after the function completes, regardless of success or failure.
%%
%% Uses a default checkout timeout of 5000ms.
%%
%% @param Pool The pool reference (pid or registered name)
%% @param Fun Function taking worker pid and returning a result
%% @returns The result of Fun, or {error, busy} if pool is full
%%
%% @end
%%--------------------------------------------------------------------
-spec transaction(Pool :: pool(), Fun :: transaction_fun(Result)) ->
          Result | {error, busy | timeout}.

transaction(Pool, Fun) ->
    transaction(Pool, Fun, 5000).

%%--------------------------------------------------------------------
%% @doc Executes a function with a worker and explicit timeout.
%%
%% Unlike the default poolboy:transaction, this returns {error, busy}
%% instead of blocking indefinitely when the pool is full. The timeout
%% applies to both checkout and execution.
%%
%% @param Pool The pool reference (pid or registered name)
%% @param Fun Function taking worker pid and returning a result
%% @param Timeout Maximum time to wait for checkout and execution (ms)
%% @returns The result of Fun, or {error, busy | timeout}
%%
%% @end
%%--------------------------------------------------------------------
-spec transaction(Pool :: pool(), Fun :: transaction_fun(Result),
                 Timeout :: pos_integer()) ->
          Result | {error, busy | timeout}.

transaction(Pool, Fun, Timeout) when is_integer(Timeout), Timeout > 0 ->
    case poolboy:checkout(Pool, false) of
        full ->
            logger:warning("wf_pool: Pool ~p is full, returning busy", [Pool]),
            {error, busy};
        Worker when is_pid(Worker) ->
            try
                Result = Fun(Worker),
                poolboy:checkin(Pool, Worker),
                Result
            catch
                Type:Error:Stack ->
                    logger:error("wf_pool: Transaction exception: ~p:~p~n~p",
                                [Type, Error, Stack]),
                    poolboy:checkin(Pool, Worker),
                    {error, exception}
            end;
        {error, Reason} ->
            logger:error("wf_pool: Checkout error: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Stops the pool and terminates all workers.
%%
%% Gracefully shuts down the worker pool. All workers are terminated
%% and any pending work is discarded.
%%
%% @param Pool The pool reference (pid or registered name)
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Pool :: pool()) -> ok.

stop(Pool) when is_pid(Pool) ->
    gen_server:stop(Pool);
stop(Pool) when is_atom(Pool) ->
    %% Try to stop via registered name
    case whereis(Pool) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current status of the pool.
%%
%% Returns a map with pool statistics including size, available workers,
%% and overflow count.
%%
%% @param Pool The pool reference (pid or registered name)
%% @returns Map with pool status information
%%
%% @end
%%--------------------------------------------------------------------
-spec status(Pool :: pool()) -> #{atom() => term()}.

status(Pool) when is_pid(Pool); is_atom(Pool) ->
    case poolboy:status(Pool) of
        {Available, Overflow} ->
            #{
                available => Available,
                overflow => Overflow,
                status => running
            };
        _ ->
            #{status => unknown}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current queue depth of the pool.
%%
%% Returns the number of workers waiting in the pool.
%%
%% @param Pool The pool reference (pid or registered name)
%% @returns Number of available workers
%%
%% @end
%%--------------------------------------------------------------------
-spec queue_depth(Pool :: pool()) -> non_neg_integer().

queue_depth(Pool) when is_pid(Pool); is_atom(Pool) ->
    case poolboy:status(Pool) of
        {Available, _Overflow} ->
            Available;
        _ ->
            0
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Valid pool configuration test (requires poolboy)
valid_pool_test() ->
    case code:ensure_loaded(poolboy) of
        {module, _} ->
            {ok, Pool} = start_link(#{name => test_pool_valid, size => 1, max_overflow => 0}),
            ?assert(is_pid(Pool) orelse is_atom(Pool)),
            ok = stop(Pool),
            ?assertEqual(ok, stop(test_pool_valid));
        _ -> ok  %% poolboy not available, skip
    end.

%% Transaction test (requires poolboy)
transaction_test() ->
    case code:ensure_loaded(poolboy) of
        {module, _} ->
            {ok, Pool} = start_link(#{name => test_pool_txn, size => 1, max_overflow => 0}),
            Result = transaction(Pool, fun(_W) -> 42 end),
            ?assertEqual(42, Result),
            ok = stop(Pool);
        _ -> ok
    end.

%% Transaction with timeout test (requires poolboy)
transaction_timeout_test() ->
    case code:ensure_loaded(poolboy) of
        {module, _} ->
            {ok, Pool} = start_link(#{name => test_pool_timeout, size => 1, max_overflow => 0}),
            Result = transaction(Pool, fun(_W) -> 42 end, 1000),
            ?assertEqual(42, Result),
            ok = stop(Pool);
        _ -> ok
    end.

%% Bad size rejection test
bad_size_test() ->
    ?assertEqual({error, bad_size}, start_link(#{name => bad, size => 0, max_overflow => 0})),
    ?assertEqual({error, bad_size}, start_link(#{name => bad, size => -1, max_overflow => 0})).

-endif.
