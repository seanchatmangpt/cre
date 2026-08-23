%% -*- erlang -*-
%% @doc Statistical Analysis for Benchmarks
%%
%% Statistical calculations for benchmark analysis including:
%% - Mean, median, variance, standard deviation
%% - Percentiles calculation
%% - Comparison and regression detection
%%
%% @end

-module(stat).
-author("CRE Team").

%% API
-export([summarize/1]).
-export([compare/2]).
-export([mean/1, median/1, variance/1, std_dev/1]).
-export([percentile/2]).
-export([regression_detected/2, regression_detected/3]).
-export([format_result/1]).

%% Types
-type sample() :: [number()].
-type stats_summary() :: #{
    n => non_neg_integer(),
    min => number(),
    max => number(),
    mean => float(),
    median => float(),
    variance => float(),
    std_dev => float(),
    p95 => float(),
    p99 => float()
}.
-type comparison_result() :: #{
    status => improved | regressed | no_change | inconclusive,
    mean_diff => float(),
    mean_diff_pct => float(),
    ratio => float(),
    significance => float()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Generate a comprehensive statistical summary from samples.
-spec summarize(sample()) -> stats_summary().
summarize([]) ->
    error(empty_sample);
summarize(Samples) when is_list(Samples) ->
    Sorted = lists:sort(Samples),
    N = length(Sorted),
    #{
        n => N,
        min => hd(Sorted),
        max => lists:last(Sorted),
        mean => mean(Sorted),
        median => median(Sorted),
        variance => variance(Sorted),
        std_dev => std_dev(Sorted),
        p95 => percentile(Sorted, 95),
        p99 => percentile(Sorted, 99)
    }.

%% @doc Compare two sample sets and determine if regression occurred.
%% Uses relative change threshold of 5% for regression detection.
-spec compare(sample(), sample()) -> comparison_result().
compare(Before, After) ->
    compare(Before, After, 0.05).

%% @doc Compare two samples with custom significance threshold.
-spec compare(sample(), sample(), float()) -> comparison_result().
compare([], _After, _Threshold) ->
    error(empty_before_sample);
compare(_Before, [], _Threshold) ->
    error(empty_after_sample);
compare(Before, After, Threshold) when is_float(Threshold), Threshold > 0 ->
    BeforeMean = mean(Before),
    AfterMean = mean(After),
    BeforeStdDev = std_dev(Before),
    AfterStdDev = std_dev(After),

    MeanDiff = AfterMean - BeforeMean,
    Ratio = AfterMean / BeforeMean,
    MeanDiffPct = (MeanDiff / BeforeMean) * 100,

    %% Calculate significance (z-score approximation)
    PooledStdDev = math:sqrt((BeforeStdDev * BeforeStdDev + AfterStdDev * AfterStdDev) / 2),
    Significance = case PooledStdDev of
        0.0 -> 0.0;
        _ -> abs(MeanDiff) / PooledStdDev
    end,

    %% If std dev is 0 (consistent samples), use the mean difference directly
    Status = case PooledStdDev =:= 0.0 of
        true ->
            %% No variance, so trust the mean difference
            case MeanDiffPct > (Threshold * 100) of
                true -> regressed;
                false when MeanDiffPct < -(Threshold * 100) -> improved;
                false -> no_change
            end;
        false ->
            %% Use significance threshold
            case Significance > 2 of
                true ->
                    case MeanDiffPct > (Threshold * 100) of
                        true -> regressed;
                        false when MeanDiffPct < -(Threshold * 100) -> improved;
                        false -> no_change
                    end;
                false ->
                    no_change
            end
    end,

    #{
        status => Status,
        mean_diff => MeanDiff,
        mean_diff_pct => MeanDiffPct,
        ratio => Ratio,
        significance => Significance
    }.

%% @doc Calculate arithmetic mean.
-spec mean(sample()) -> float().
mean([]) ->
    error(empty_sample);
mean(Samples) when is_list(Samples) ->
    lists:sum(Samples) / length(Samples).

%% @doc Calculate median.
-spec median(sample()) -> float().
median([]) ->
    error(empty_sample);
median(Samples) ->
    Sorted = lists:sort(Samples),
    N = length(Sorted),
    Mid = N div 2,
    case N rem 2 of
        0 ->
            (lists:nth(Mid, Sorted) + lists:nth(Mid + 1, Sorted)) / 2.0;
        1 ->
            erlang:float(lists:nth(Mid + 1, Sorted))
    end.

%% @doc Calculate variance (population).
-spec variance(sample()) -> float().
variance([]) ->
    error(empty_sample);
variance([_]) ->
    0.0;
variance(Samples) ->
    Mean = mean(Samples),
    N = length(Samples),
    SumSq = lists:sum([(X - Mean) * (X - Mean) || X <- Samples]),
    SumSq / N.

%% @doc Calculate standard deviation.
-spec std_dev(sample()) -> float().
std_dev([]) ->
    error(empty_sample);
std_dev([_]) ->
    0.0;
std_dev(Samples) ->
    math:sqrt(variance(Samples)).

%% @doc Calculate percentile from samples.
-spec percentile(sample(), number()) -> float().
percentile([], _P) ->
    error(empty_sample);
percentile(Samples, P) when is_number(P), P >= 0, P =< 100 ->
    Sorted = lists:sort(Samples),
    N = length(Sorted),
    K = (P / 100) * (N - 1) + 1,
    Floor = trunc(K),
    Ceil = Floor + 1,
    Fraction = K - Floor,

    Lower = lists:nth(max(1, Floor), Sorted),
    Upper = if
        Ceil > N -> lists:nth(N, Sorted);
        true -> lists:nth(Ceil, Sorted)
    end,
    Lower + Fraction * (Upper - Lower).

%% @doc Detect if regression occurred (true if regressed).
-spec regression_detected(sample(), sample()) -> boolean().
regression_detected(Before, After) ->
    regression_detected(Before, After, 0.05).

%% @doc Detect regression with custom threshold.
-spec regression_detected(sample(), sample(), float()) -> boolean().
regression_detected(Before, After, Threshold) ->
    case compare(Before, After, Threshold) of
        #{status := regressed} -> true;
        _ -> false
    end.

%% @doc Format a benchmark result for display.
-spec format_result(map()) -> iolist().
format_result(Result) when is_map(Result) ->
    Format = fun(Key, Label) ->
        Value = maps:get(Key, Result, 0.0),
        io_lib:format("  ~-12s: ~.2f~n", [Label, Value])
    end,

    [
        "Benchmark Results:~n",
        Format(min, "Min (us)"),
        Format(max, "Max (us)"),
        Format(mean, "Mean (us)"),
        Format(median, "Median (us)"),
        Format(p95, "P95 (us)"),
        Format(p99, "P99 (us)"),
        Format(std_dev, "StdDev"),
        io_lib:format("  ~-12s: ~p~n", ["Iterations", maps:get(iterations, Result, 0)])
    ].
