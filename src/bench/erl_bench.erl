%% -*- erlang -*-
%% @doc Erlang Benchmark Execution Harness
%%
%% Microbenchmarking framework for pattern execution with:
%% - Multiple iteration timing
%% - Warmup runs for JIT compilation
%% - Statistical output (min, max, mean, median, percentiles)
%%
%% @end

-module(erl_bench).
-author("CRE Team").

%% API
-export([bench/2, bench/3]).
-export([bench_sync/2, bench_sync/3]).
-export([run_many/2]).

%% Internal exports for MFA calls
-export([identity_function/1]).

%% Types
-type time_value() :: number().
-type benchmark_result() :: #{
    min => time_value(),
    max => time_value(),
    mean => time_value(),
    median => time_value(),
    p95 => time_value(),
    p99 => time_value(),
    p999 => time_value(),
    std_dev => time_value(),
    iterations => pos_integer(),
    total_time => time_value()
}.
-type bench_fun() :: fun(() -> term()) | {module(), atom(), list()}.
-type bench_options() :: #{
    warmup => non_neg_integer(),
    gc => boolean(),
    scheduler => boolean()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Benchmark a function with N iterations.
%% Uses default warmup of 3 iterations.
-spec bench(bench_fun(), pos_integer()) -> benchmark_result().
bench(Fun, N) when is_function(Fun, 0), is_integer(N), N > 0 ->
    bench(Fun, N, 3);
bench({M, F, A}, N) when is_atom(M), is_atom(F), is_list(A), is_integer(N), N > 0 ->
    bench({M, F, A}, N, 3).

%% @doc Benchmark a function with N iterations and W warmup runs.
-spec bench(bench_fun(), pos_integer(), pos_integer() | bench_options()) -> benchmark_result().
bench(Fun, N, Warmup) when is_function(Fun, 0), is_integer(N), N > 0,
                         is_integer(Warmup), Warmup >= 0 ->
    do_bench(Fun, N, Warmup, #{});
bench({M, F, A}, N, Warmup) when is_atom(M), is_atom(F), is_list(A),
                                 is_integer(N), N > 0, is_integer(Warmup), Warmup >= 0 ->
    WrappedFun = fun() -> apply(M, F, A) end,
    do_bench(WrappedFun, N, Warmup, #{});
bench(Fun, N, Options) when is_function(Fun, 0), is_map(Options) ->
    Warmup = maps:get(warmup, Options, 3),
    do_bench(Fun, N, Warmup, Options);
bench({M, F, A}, N, Options) when is_atom(M), is_atom(F), is_list(A), is_map(Options) ->
    WrappedFun = fun() -> apply(M, F, A) end,
    Warmup = maps:get(warmup, Options, 3),
    do_bench(WrappedFun, N, Warmup, Options).

%% @doc Synchronous benchmark, runs on current process.
%% Single iteration timing with N repeats for reliability.
-spec bench_sync(bench_fun(), pos_integer()) -> benchmark_result().
bench_sync(Fun, N) ->
    bench_sync(Fun, N, #{}).

%% @doc Synchronous benchmark with options.
-spec bench_sync(bench_fun(), pos_integer(), bench_options()) -> benchmark_result().
bench_sync(Fun, N, Options) ->
    Warmup = maps:get(warmup, Options, 0),
    do_bench(Fun, N, Warmup, Options#{sync => true}).

%% @doc Run a function multiple times and return results.
-spec run_many(bench_fun(), pos_integer()) -> list().
run_many(Fun, N) when is_function(Fun, 0), is_integer(N), N > 0 ->
    [Fun() || _ <- lists:seq(1, N)];
run_many({M, F, A}, N) when is_atom(M), is_atom(F), is_list(A), is_integer(N), N > 0 ->
    [apply(M, F, A) || _ <- lists:seq(1, N)].

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Core benchmark implementation.
do_bench(Fun, N, Warmup, Options) ->
    case Warmup > 0 of
        true ->
            %% Perform warmup runs
            lists:foreach(fun(_) -> run_single(Fun, Options) end, lists:seq(1, Warmup)),
            %% Force GC after warmup
            garbage_collect(),
            do_bench(Fun, N, 0, Options);
        false ->
            %% Run actual benchmark iterations
            Timings = [run_single(Fun, Options) || _ <- lists:seq(1, N)],
            %% Force GC after all runs
            garbage_collect(),
            %% Calculate statistics
            calculate_stats(Timings, N, Options)
    end.

%% @private Run a single iteration and return time in microseconds.
run_single(Fun, Options) ->
    %% Force GC if requested
    case maps:get(gc, Options, false) of
        true -> garbage_collect();
        false -> ok
    end,

    %% High-precision timing
    T0 = os:system_time(microsecond),
    Result = Fun(),
    T1 = os:system_time(microsecond),

    %% Prevent optimization of unused result
    suppress_unused(Result),
    T1 - T0.

%% @private Calculate statistics from timings.
calculate_stats(Timings, N, _Options) ->
    Sorted = lists:sort(Timings),
    Sum = lists:sum(Timings),
    Mean = Sum / N,
    Min = erlang:float(hd(Sorted)),
    Max = erlang:float(lists:last(Sorted)),
    Median = percentile(Sorted, 50),
    P95 = percentile(Sorted, 95),
    P99 = percentile(Sorted, 99),
    P999 = percentile(Sorted, 99.9),
    StdDev = std_dev(Timings, Mean, N),

    #{
        min => Min,
        max => Max,
        mean => Mean,
        median => Median,
        p95 => P95,
        p99 => P99,
        p999 => P999,
        std_dev => StdDev,
        iterations => N,
        total_time => Sum
    }.

%% @private Calculate percentile from sorted list.
percentile(Sorted, P) when is_list(Sorted), is_number(P), P > 0, P =< 100 ->
    N = length(Sorted),
    K = (P / 100) * (N - 1) + 1,
    Floor = trunc(K),
    Ceil = Floor + 1,
    Fraction = K - Floor,

    if
        Ceil > N ->
            lists:nth(N, Sorted);
        Floor < 1 ->
            lists:nth(1, Sorted);
        Fraction == 0 ->
            lists:nth(Floor, Sorted);
        true ->
            Lower = lists:nth(Floor, Sorted),
            UpperVal = if
                Ceil > N -> lists:nth(N, Sorted);
                true -> lists:nth(Ceil, Sorted)
            end,
            Lower + Fraction * (UpperVal - Lower)
    end.

%% @private Calculate standard deviation.
std_dev(_Timings, _Mean, 0) ->
    0.0;
std_dev(Timings, Mean, N) ->
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Timings]) / N,
    math:sqrt(Variance).

%% @private Suppress unused result warning.
suppress_unused(_Result) ->
    ok.

%% @private Helper function for MFA benchmarking.
identity_function(Input) ->
    Input.
