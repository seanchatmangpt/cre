%% -*- erlang -*-
%% @doc Throughput Benchmarking for Workflow Execution
%%
%% Measures throughput metrics for workflow case execution:
%% - Cases per second: Number of workflow cases completed
%% - Effects per second: Number of effect receipts generated
%% - Reductions per second: VM reduction operations
%%
%% Uses erlang:statistics(reductions) for accurate reduction counting.
%%
%% @end

-module(f5_bench_throughput).
-author("CRE Team").

%% API
-export([measure_throughput/2]).
-export([benchmark_cases/1]).
-export([benchmark_effects/1]).
-export([benchmark_reductions/1]).
-export([compare_baseline/2]).
-export([run_workload/2]).
-export([format_result/1]).

%% Types
-type workload_fun() :: fun(() -> term()).
-type throughput_result() :: #{
    cases_per_sec => float(),
    effects_per_sec => float(),
    reductions_per_sec => float(),
    duration_ms => non_neg_integer(),
    total_cases => non_neg_integer(),
    total_effects => non_neg_integer(),
    total_reductions => non_neg_integer()
}.
-type baseline() :: #{
    cases_per_sec => float(),
    effects_per_sec => float(),
    reductions_per_sec => float()
}.
-type comparison() :: #{
    status => improved | regressed | no_change,
    cases_diff => float(),
    effects_diff => float(),
    reductions_diff => float()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Measure throughput for a workload function.
%% Runs the workload repeatedly for Duration ms and returns throughput metrics.
-spec measure_throughput(workload_fun(), non_neg_integer()) -> throughput_result().
measure_throughput(Workload, Duration) when is_function(Workload, 0), is_integer(Duration), Duration > 0 ->
    %% Get initial reduction count
    {InitialReductions, _} = erlang:statistics(reductions),

    %% Start timing
    StartTime = os:system_time(microsecond),

    %% Run workload loop for duration
    {Cases, Effects} = run_workload_loop(Workload, StartTime, Duration, 0, 0),

    %% Get final reduction count
    {FinalReductions, _} = erlang:statistics(reductions),
    TotalReductions = FinalReductions - InitialReductions,

    %% Calculate actual duration
    EndTime = os:system_time(microsecond),
    ActualDurationMs = (EndTime - StartTime) div 1000,

    %% Calculate per-second rates (handle very short durations)
    Secs = ActualDurationMs / 1000.0,
    CasesPerSec = case Secs > 0 of
        true -> Cases / Secs;
        false -> Cases
    end,
    EffectsPerSec = case Secs > 0 of
        true -> Effects / Secs;
        false -> Effects
    end,
    ReductionsPerSec = case Secs > 0 of
        true -> TotalReductions / Secs;
        false -> TotalReductions
    end,

    #{
        cases_per_sec => CasesPerSec,
        effects_per_sec => EffectsPerSec,
        reductions_per_sec => ReductionsPerSec,
        duration_ms => ActualDurationMs,
        total_cases => Cases,
        total_effects => Effects,
        total_reductions => TotalReductions
    }.

%% @doc Benchmark workflow cases per second.
%% Uses a simple no-op workflow for baseline measurement.
-spec benchmark_cases(pos_integer()) -> throughput_result().
benchmark_cases(Duration) ->
    Workload = fun() -> run_single_case() end,
    measure_throughput(Workload, Duration).

%% @doc Benchmark effect receipts per second.
%% Simulates effects that generate receipts.
-spec benchmark_effects(pos_integer()) -> throughput_result().
benchmark_effects(Duration) ->
    Workload = fun() -> run_effect_case() end,
    measure_throughput(Workload, Duration).

%% @doc Benchmark VM reductions per second.
%% Measures raw reduction throughput without workflow overhead.
-spec benchmark_reductions(pos_integer()) -> throughput_result().
benchmark_reductions(Duration) ->
    Workload = fun() -> reduction_workload() end,
    measure_throughput(Workload, Duration).

%% @doc Compare throughput result against a baseline.
%% Returns comparison status and percentage differences.
-spec compare_baseline(throughput_result(), baseline()) -> comparison().
compare_baseline(Result, Baseline) ->
    CasesDiff = calculate_diff(maps:get(cases_per_sec, Baseline, 0),
                               maps:get(cases_per_sec, Result)),
    EffectsDiff = calculate_diff(maps:get(effects_per_sec, Baseline, 0),
                                 maps:get(effects_per_sec, Result)),
    ReductionsDiff = calculate_diff(maps:get(reductions_per_sec, Baseline, 0),
                                    maps:get(reductions_per_sec, Result)),

    %% Determine overall status
    Status = case {CasesDiff, EffectsDiff, ReductionsDiff} of
        {neg, _, _} -> regressed;
        {_, neg, _} -> regressed;
        {_, _, neg} -> regressed;
        {pos, _, _} -> improved;
        {_, pos, _} -> improved;
        {_, _, pos} -> improved;
        _ -> no_change
    end,

    #{
        status => Status,
        cases_diff => CasesDiff,
        effects_diff => EffectsDiff,
        reductions_diff => ReductionsDiff
    }.

%% @doc Run a workload N times and return case/effect counts.
%% Useful for controlled microbenchmarks.
-spec run_workload(workload_fun(), pos_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
run_workload(_Workload, 0) ->
    {0, 0};
run_workload(Workload, N) when N > 0 ->
    lists:foldl(
        fun(_, {CasesAcc, EffectsAcc}) ->
            Effects = Workload(),
            {CasesAcc + 1, EffectsAcc + Effects}
        end,
        {0, 0},
        lists:seq(1, N)
    ).

%% @doc Format throughput result for display.
-spec format_result(throughput_result()) -> iolist().
format_result(Result) ->
    [
        "Throughput Results:~n",
        io_lib:format("  Duration: ~.2f sec~n", [maps:get(duration_ms, Result) / 1000.0]),
        io_lib:format("  Cases/sec: ~.2f~n", [maps:get(cases_per_sec, Result)]),
        io_lib:format("  Effects/sec: ~.2f~n", [maps:get(effects_per_sec, Result)]),
        io_lib:format("  Reductions/sec: ~.2f~n", [maps:get(reductions_per_sec, Result)]),
        io_lib:format("  Total cases: ~p~n", [maps:get(total_cases, Result)]),
        io_lib:format("  Total effects: ~p~n", [maps:get(total_effects, Result)]),
        io_lib:format("  Total reductions: ~p~n", [maps:get(total_reductions, Result)])
    ].

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Run workload loop until duration expires.
run_workload_loop(Workload, StartTime, DurationMs, CasesAcc, EffectsAcc) ->
    CurrentTime = os:system_time(microsecond),
    ElapsedMs = (CurrentTime - StartTime) div 1000,

    case ElapsedMs >= DurationMs of
        true ->
            {CasesAcc, EffectsAcc};
        false ->
            Effects = Workload(),
            run_workload_loop(Workload, StartTime, DurationMs,
                             CasesAcc + 1, EffectsAcc + Effects)
    end.

%% @private Simulate a single workflow case execution.
%% Returns 1 effect per case by default.
run_single_case() ->
    %% Simulate minimal case work
    _ = lists:sum(lists:seq(1, 10)),
    1.  %% Return 1 effect

%% @private Simulate a case that generates effects.
run_effect_case() ->
    %% Simulate effect generation (3 effects per case)
    _ = [lists:sum(lists:seq(1, 5)) || _ <- lists:seq(1, 3)],
    3.  %% Return 3 effects

%% @private Pure reduction workload for measuring VM throughput.
reduction_workload() ->
    %% Compute-intensive work that generates reductions
    lists:foldl(
        fun(X, Acc) -> X + Acc end,
        0,
        lists:seq(1, 100)
    ),
    0.  %% No effects for pure reduction benchmark

%% @private Calculate difference between baseline and actual.
%% Returns pos for improvement, neg for regression, zero for no change.
calculate_diff(0, _Actual) ->
    zero;
calculate_diff(Baseline, Actual) ->
    DiffPct = ((Actual - Baseline) / Baseline) * 100,
    if
        DiffPct > 5.0 -> pos;
        DiffPct < -5.0 -> neg;
        true -> zero
    end.
