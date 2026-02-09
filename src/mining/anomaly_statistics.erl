%% -*- erlang -*-
%% @doc Anomaly Detection Statistics Module
%%
%% Pure functional statistical calculations for anomaly detection.
%% All functions are total (no crashes) and operate on lists.
%%
%% @end

-module(anomaly_statistics).

%% Basic statistics
-export([mean/1, median/1, stddev/1, variance/1, percentile/2]).
-export([zscore/2, iqr_outliers/2, moving_average/2]).
-export([correlation/2, covariance/2]).
-export([trend_analysis/1, detect_trend/2]).
-export([outlier_score/2, anomaly_probability/2]).

%% Types
-type numeric() :: number().
-export_type([numeric/0]).

%%--------------------------------------------------------------------
%% @doc Calculates the arithmetic mean of a list of numbers.
%% Returns 0.0 for empty lists.
%% @end
%%--------------------------------------------------------------------
-spec mean([numeric()]) -> float().
mean([]) -> 0.0;
mean(Values) -> lists:sum(Values) / length(Values).

%%--------------------------------------------------------------------
%% @doc Calculates the median (50th percentile).
%% @end
%%--------------------------------------------------------------------
-spec median([numeric()]) -> float().
median([]) -> 0.0;
median(Values) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    Mid = Len div 2,
    case Len rem 2 of
        0 -> (lists:nth(Mid, Sorted) + lists:nth(Mid + 1, Sorted)) / 2;
        1 -> lists:nth(Mid + 1, Sorted)
    end.

%%--------------------------------------------------------------------
%% @doc Calculates the Nth percentile of values (0-100).
%% @end
%%--------------------------------------------------------------------
-spec percentile([numeric()], integer()) -> float().
percentile([], _Percentile) -> 0.0;
percentile(Sorted, Percentile) when is_list(Sorted), is_integer(Percentile), Percentile >= 0, Percentile =< 100 ->
    N = length(Sorted),
    case N of
        0 -> 0.0;
        _ ->
            Pos = max(1, (Percentile * N) div 100),
            lists:nth(min(Pos, N), Sorted)
    end;
percentile(Values, Percentile) ->
    percentile(lists:sort(Values), Percentile).

%%--------------------------------------------------------------------
%% @doc Calculates population standard deviation.
%% @end
%%--------------------------------------------------------------------
-spec stddev([numeric()]) -> float().
stddev([]) -> 0.0;
stddev([_Single]) -> 0.0;
stddev(Values) ->
    Mean = mean(Values),
    math:sqrt(variance(Values, Mean)).

%%--------------------------------------------------------------------
%% @doc Calculates population variance.
%% @end
%%--------------------------------------------------------------------
-spec variance([numeric()]) -> float().
variance([]) -> 0.0;
variance([_Single]) -> 0.0;
variance(Values) ->
    Mean = mean(Values),
    lists:sum([math:pow(V - Mean, 2) || V <- Values]) / length(Values).

%% @private
-spec variance([numeric()], float()) -> float().
variance([], _Mean) -> 0.0;
variance([_Single], _Mean) -> 0.0;
variance(Values, Mean) ->
    lists:sum([math:pow(V - Mean, 2) || V <- Values]) / length(Values).

%%--------------------------------------------------------------------
%% @doc Calculates Z-score for a value given population statistics.
%% @end
%%--------------------------------------------------------------------
-spec zscore(numeric(), [numeric()]) -> float().
zscore(_Value, []) -> 0.0;
zscore(_Value, [_Single]) -> 0.0;
zscore(Value, Population) ->
    Mean = mean(Population),
    StdDev = stddev(Population),
    case StdDev of
        +0.0 -> 0.0;
        _ -> (Value - Mean) / StdDev
    end.

%%--------------------------------------------------------------------
%% @doc Identifies outliers using IQR method.
%% Returns {ok, Outliers} where Outliers is list of {Value, Index}.
%% @end
%%--------------------------------------------------------------------
-spec iqr_outliers([numeric()], float()) -> {ok, [{numeric(), pos_integer()}]}.
iqr_outliers([], _Multiplier) -> {ok, []};
iqr_outliers(Values, Multiplier) ->
    Sorted = lists:sort(Values),
    Q1 = percentile(Sorted, 25),
    Q3 = percentile(Sorted, 75),
    IQR = Q3 - Q1,
    Lower = Q1 - Multiplier * IQR,
    Upper = Q3 + Multiplier * IQR,
    Outliers = lists:filtermap(fun(V) ->
        case V < Lower orelse V > Upper of
            true -> {true, {V, index_of(V, Sorted, 1)}};
            false -> false
        end
    end, Sorted),
    {ok, Outliers}.

%% @private
index_of(_Value, [], _Index) -> 1;
index_of(Value, [H|_T], Index) when H =:= Value -> Index;
index_of(Value, [_H|T], Index) -> index_of(Value, T, Index + 1).

%%--------------------------------------------------------------------
%% @doc Calculates moving average over specified window size.
%% @end
%%--------------------------------------------------------------------
-spec moving_average([numeric()], pos_integer()) -> [float()].
moving_average([], _WindowSize) -> [];
moving_average(Values, WindowSize) when is_integer(WindowSize), WindowSize > 0 ->
    moving_average(Values, WindowSize, [], 0, 0.0, []).

%% @private
moving_average([], _WindowSize, _Window, _Count, _Sum, Acc) ->
    lists:reverse(Acc);
moving_average([V|Rest], WindowSize, Window, Count, Sum, Acc) when Count >= WindowSize - 1 ->
    NewSum = Sum + V,
    Avg = NewSum / WindowSize,
    %% Slide window: remove oldest, add new
    [Oldest|_] = lists:reverse(Window),
    NewWindow = lists:reverse([V|lists:reverse(Window)] -- [Oldest]),
    moving_average(Rest, WindowSize, NewWindow, Count + 1, NewSum - Oldest, [Avg|Acc]);
moving_average([V|Rest], WindowSize, Window, Count, Sum, Acc) ->
    moving_average(Rest, WindowSize, [V|Window], Count + 1, Sum + V, Acc).

%%--------------------------------------------------------------------
%% @doc Calculates correlation coefficient between two lists.
%% @end
%%--------------------------------------------------------------------
-spec correlation([numeric()], [numeric()]) -> float() | undefined.
correlation([], _) -> undefined;
correlation(_, []) -> undefined;
correlation(X, Y) when length(X) =/= length(Y) -> undefined;
correlation(X, Y) ->
    MeanX = mean(X),
    MeanY = mean(Y),
    Cov = covariance(X, Y, MeanX, MeanY),
    StdX = stddev(X),
    StdY = stddev(Y),
    case StdX * StdY of
        +0.0 -> undefined;
        _ -> Cov / (StdX * StdY)
    end.

%%--------------------------------------------------------------------
%% @doc Calculates covariance between two lists.
%% @end
%%--------------------------------------------------------------------
-spec covariance([numeric()], [numeric()]) -> float() | undefined.
covariance([], _) -> undefined;
covariance(_, []) -> undefined;
covariance(X, Y) when length(X) =/= length(Y) -> undefined;
covariance(X, Y) ->
    MeanX = mean(X),
    MeanY = mean(Y),
    covariance(X, Y, MeanX, MeanY).

%% @private
-spec covariance([numeric()], [numeric()], float(), float()) -> float().
covariance(X, Y, MeanX, MeanY) ->
    N = length(X),
    SumProd = lists:sum(lists:zipwith(
        fun(Xv, Yv) -> (Xv - MeanX) * (Yv - MeanY) end,
        X, Y
    )),
    SumProd / N.

%%--------------------------------------------------------------------
%% @doc Analyzes trend in time series data.
%% @end
%%--------------------------------------------------------------------
-spec trend_analysis([numeric()]) -> map().
trend_analysis([]) -> #{trend => stable, slope => 0.0, r_squared => 1.0};
trend_analysis([_Single]) -> #{trend => stable, slope => 0.0, r_squared => 1.0};
trend_analysis(Values) ->
    N = length(Values),
    X = lists:seq(1, N),
    {Slope, _Intercept, R2} = linear_regression(X, Values),
    Trend = if
        Slope > 0.01 -> rising;
        Slope < -0.01 -> falling;
        true -> stable
    end,
    #{
        trend => Trend,
        slope => Slope,
        r_squared => R2
    }.

%%--------------------------------------------------------------------
%% @doc Detects trend direction (simplified).
%% @end
%%--------------------------------------------------------------------
-spec detect_trend([numeric()], float()) -> rising | falling | stable.
detect_trend([], _Threshold) -> stable;
detect_trend([_], _Threshold) -> stable;
detect_trend(Values, Threshold) ->
    #{slope := Slope} = trend_analysis(Values),
    if
        Slope > Threshold -> rising;
        Slope < -Threshold -> falling;
        true -> stable
    end.

%% @private
-spec linear_regression([integer()], [numeric()]) -> {float(), float(), float()}.
linear_regression(X, Y) ->
    N = length(X),
    SumX = lists:sum(X),
    SumY = lists:sum(Y),
    SumXX = lists:sum([Xx * Xx || Xx <- X]),
    SumXY = lists:sum(lists:zipwith(fun(Xx, Yy) -> Xx * Yy end, X, Y)),

    Slope = case (N * SumXX - SumX * SumX) of
        +0.0 -> 0.0;
        Denom -> (N * SumXY - SumX * SumY) / Denom
    end,

    Intercept = case N of
        0 -> 0.0;
        _ -> (SumY - Slope * SumX) / N
    end,

    %% Calculate R-squared
    YMean = mean(Y),
    SST = lists:sum([(Yv - YMean) * (Yv - YMean) || Yv <- Y]),
    SSR = case SST of
        +0.0 -> 0.0;
        _ -> lists:sum([math:pow(Yi - (Intercept + Slope * Xi), 2) || Yi <- Y, Xi <- X])
    end,

    R2 = case SST of
        +0.0 -> 1.0;
        _ -> 1.0 - (SSR / SST)
    end,

    {Slope, Intercept, R2}.

%%--------------------------------------------------------------------
%% @doc Calculates composite outlier score (0-1, higher = more anomalous).
%% @end
%%--------------------------------------------------------------------
-spec outlier_score(numeric(), [numeric()]) -> float().
outlier_score(_Value, []) -> 0.0;
outlier_score(_Value, Population) when length(Population) < 3 ->
    0.0;
outlier_score(Value, Population) ->
    ZScoreAbs = abs(zscore(Value, Population)),

    {ok, IQRRef} = iqr_outliers(Population, 1.5),
    IQRScore = case IQRRef of
        [] -> 0.0;
        _ -> 1.0
    end,

    Sorted = lists:sort(Population),
    PercentileRank = case lists:member(Value, Sorted) of
        true ->
            Position = length([V || V <- Sorted, V =< Value]),
            Position / length(Sorted);
        false ->
            case Value > lists:last(Sorted) of
                true -> 1.0;
                false -> 0.0
            end
    end,

    %% Combine scores with weighted average
    Score = 0.5 * min(ZScoreAbs / 3.0, 1.0) +
            0.3 * IQRScore +
            0.2 * abs(PercentileRank - 0.5) * 2,
    min(1.0, max(0.0, Score)).

%%--------------------------------------------------------------------
%% @doc Calculates probability that value belongs to population distribution.
%% @end
%%--------------------------------------------------------------------
-spec anomaly_probability(numeric(), [numeric()]) -> float().
anomaly_probability(_Value, []) -> 0.5;
anomaly_probability(_Value, Population) when length(Population) < 3 ->
    0.5;
anomaly_probability(Value, Population) ->
    Mean = mean(Population),
    StdDev = stddev(Population),
    case StdDev of
        +0.0 ->
            case Value =:= Mean of
                true -> 1.0;
                false -> 0.0
            end;
        _ ->
            Z = zscore(Value, Population),
            PDF = math:exp(-Z * Z / 2) / math:sqrt(2 * math:pi()),
            min(1.0, max(0.0, PDF))
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

mean_test() ->
    ?assertEqual(0.0, mean([])),
    ?assertEqual(3.0, mean([1,2,3,4,5])),
    ?assertEqual(-2.0, mean([-2, -4, -6])),
    ?assertEqual(2.5, mean([1,2,3,4])).

median_test() ->
    ?assertEqual(0.0, median([])),
    ?assertEqual(3.0, median([1,2,3,4,5])),
    ?assertEqual(3.5, median([1,2,3,4,5,6])),
    ?assertEqual(2.5, median([1,2,3,4])).

stddev_test() ->
    ?assertEqual(0.0, stddev([42])),
    ?assertEqual(0.0, stddev([1,1,1,1])),
    ?assert(abs(stddev([0,2,4]) - 1.632) < 0.01).

percentile_test() ->
    Sorted = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    ?assertEqual(1, percentile(Sorted, 10)),
    ?assertEqual(5, percentile(Sorted, 50)),
    ?assertEqual(10, percentile(Sorted, 90)).

zscore_test() ->
    ?assertEqual(0.0, zscore(5, [5,5,5])),
    ?assertEqual(0.0, zscore(5, [3,4,5,6,7])),
    ?assert(abs(zscore(7, [3,4,5,6,7]) - 1.0) < 0.01).

iqr_outliers_test() ->
    ?assertEqual({ok, []}, iqr_outliers([], 1.5)),
    ?assertEqual({ok, []}, iqr_outliers([1,1,1,1], 1.5)),
    {ok, Outliers} = iqr_outliers([1,1,1,1,100], 1.5),
    ?assert(length(Outliers) > 0).

moving_average_test() ->
    ?assertEqual([], moving_average([], 3)),
    ?assertEqual([2.0, 3.0], moving_average([1,2,3,4], 2)),
    ?assertEqual([2.0], moving_average([1,2,3], 3)).

correlation_test() ->
    ?assertEqual(undefined, correlation([], [1,2])),
    ?assert(abs(correlation([1,2,3], [1,2,3]) - 1.0) < 0.01),
    ?assert(abs(correlation([1,2,3], [3,2,1]) + 1.0) < 0.01).

trend_analysis_test() ->
    ?assertMatch(#{trend := stable}, trend_analysis([5,5,5,5])),
    ?assertMatch(#{trend := rising}, trend_analysis([1,2,3,4,5])),
    ?assertMatch(#{trend := falling}, trend_analysis([5,4,3,2,1])).

outlier_score_test() ->
    Score = outlier_score(100, [1,1,1,1,1]),
    ?assert(Score >= 0.0 andalso Score =< 1.0).

anomaly_probability_test() ->
    ?assertEqual(1.0, anomaly_probability(5, [5,5,5,5])),
    ?assert(anomaly_probability(100, [1,1,1,1,1]) < 0.5).

-endif.
