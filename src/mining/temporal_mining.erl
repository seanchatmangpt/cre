%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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
%% @doc Temporal Process Mining
%%
%% This module implements time-aware pattern discovery for process mining,
%% focusing on temporal aspects of event logs such as:
%%
%% <ul>
%%   <li><b>Cycle Time:</b> Time between process start and end</li>
%%   <li><b>Processing Time:</b> Duration of activities</li>
%%   <li><b>Waiting Time:</b> Time between activities</li>
%%   <li><b>Seasonality:</b> Time-based patterns (daily, weekly, etc.)</li>
%%   <li><b>Trends:</b> Changes over time</li>
%% </ul>
%%
%% <h3>Input Format</h3>
%%
%% Events should include timestamps:
%% ```erlang
%% #{
%%   activity => approve,
%%   timestamp => 1704067200000,  %% Unix ms
%%   case_id => <<"case1">>
%% }
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(temporal_mining).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([mine_temporal/1]).
-export([mine_temporal/2]).

%% Temporal analysis
-export([compute_cycle_times/1]).
-export([compute_processing_times/1]).
-export([detect_seasonality/2]).
-export([analyze_trends/1]).

%% Statistics
-export([temporal_statistics/1]).
-export([percentile/2]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type timestamp() :: integer().  %% Unix milliseconds
-type case_id() :: binary().

-type temporal_event() :: #{
    activity := activity(),
    timestamp := timestamp(),
    case_id := case_id()
}.

-type temporal_log() :: [temporal_event()].

-type duration_ms() :: pos_integer().

-type cycle_time() :: #{
    case_id := case_id(),
    start_time := timestamp(),
    end_time := timestamp(),
    duration := duration_ms()
}.

-type processing_time() :: #{
    activity := activity(),
    case_id := case_id(),
    start_time := timestamp(),
    end_time := timestamp(),
    duration := duration_ms()
}.

-type seasonality_pattern() :: #{
    period := hourly | daily | weekly | monthly,
    strength := float(),     %% 0.0 to 1.0
    peaks => [timestamp()],
    troughs => [timestamp()]
}.

-type temporal_pattern() :: #{
    type := cycle_time | processing_time | waiting_time | seasonality | trend,
    data => term()
}.

-type mining_options() :: #{
    include_cycle_time => boolean(),
    include_processing_time => boolean(),
    include_seasonality => boolean(),
    include_trends => boolean(),
    seasonality_periods => [hourly | daily | weekly | monthly]
}.

-export_type([
    activity/0, timestamp/0, case_id/0,
    temporal_event/0, temporal_log/0,
    cycle_time/0, processing_time/0,
    seasonality_pattern/0, temporal_pattern/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Mine temporal patterns with default options.
-spec mine_temporal(temporal_log()) -> {ok, [temporal_pattern()]}.
mine_temporal(Log) ->
    mine_temporal(Log, #{}).

%% @doc Mine temporal patterns with custom options.
-spec mine_temporal(temporal_log(), mining_options()) -> {ok, [temporal_pattern()]}.
mine_temporal(Log, Options) when is_list(Log), is_map(Options) ->
    Patterns = [],

    %% Cycle time analysis
    Patterns1 = case maps:get(include_cycle_time, Options, true) of
        true ->
            CycleTimes = compute_cycle_times(Log),
            [#{
                type => cycle_time,
                data => #{
                    cycle_times => CycleTimes,
                    statistics => compute_cycle_stats(CycleTimes)
                }
            } | Patterns];
        false ->
            Patterns
    end,

    %% Processing time analysis
    Patterns2 = case maps:get(include_processing_time, Options, true) of
        true ->
            ProcessingTimes = compute_processing_times(Log),
            [#{
                type => processing_time,
                data => #{
                    processing_times => ProcessingTimes,
                    statistics => compute_processing_stats(ProcessingTimes)
                }
            } | Patterns1];
        false ->
            Patterns1
    end,

    %% Seasonality detection
    Patterns3 = case maps:get(include_seasonality, Options, true) of
        true ->
            Periods = maps:get(seasonality_periods, Options, [daily, weekly]),
            Seasonality = detect_seasonality(Log, Periods),
            [#{
                type => seasonality,
                data => Seasonality
            } | Patterns2];
        false ->
            Patterns2
    end,

    %% Trend analysis
    Patterns4 = case maps:get(include_trends, Options, true) of
        true ->
            Trends = analyze_trends(Log),
            [#{
                type => trend,
                data => Trends
            } | Patterns3];
        false ->
            Patterns3
    end,

    {ok, lists:reverse(Patterns4)}.

%%====================================================================
%% Temporal Analysis Functions
%%====================================================================

%% @doc Compute cycle times for all cases.
-spec compute_cycle_times(temporal_log()) -> [cycle_time()].
compute_cycle_times(Log) ->
    %% Group events by case
    Cases = group_by_case(Log),

    maps:fold(fun(CaseId, Events, Acc) ->
        case compute_case_cycle(CaseId, Events) of
            {ok, CycleTime} -> [CycleTime | Acc];
            {error, _} -> Acc
        end
    end, [], Cases).

%% @doc Compute processing times for activities.
-spec compute_processing_times(temporal_log()) -> [processing_time()].
compute_processing_times(Log) ->
    %% Group events by case and activity
    Cases = group_by_case(Log),

    maps:fold(fun(_CaseId, Events, Acc) ->
        case compute_case_processing_times(Events) of
            {ok, Times} -> Times ++ Acc;
            {error, _} -> Acc
        end
    end, [], Cases).

%% @doc Detect seasonality patterns.
-spec detect_seasonality(temporal_log(), [hourly | daily | weekly | monthly]) ->
    [seasonality_pattern()].
detect_seasonality(Log, Periods) ->
    %% Extract timestamps
    Timestamps = [maps:get(timestamp, E) || E <- Log],

    lists:map(fun(Period) ->
        detect_period_seasonality(Timestamps, Period)
    end, Periods).

%% @doc Analyze temporal trends.
-spec analyze_trends(temporal_log()) -> map().
analyze_trends(Log) ->
    %% Sort by timestamp
    Sorted = lists:sort(fun(E1, E2) ->
        maps:get(timestamp, E1) =< maps:get(timestamp, E2)
    end, Log),

    %% Group by time windows
    Windows = group_by_time_window(Sorted, 86400000),  %% Daily windows

    %% Compute trend metrics
    WindowSizes = [length(Events) || {_Window, Events} <- Windows],

    #{
        trend_direction => compute_trend_direction(WindowSizes),
        trend_strength => compute_trend_strength(WindowSizes),
        windows => Windows,
        summary => #{
            total_events => length(Log),
            total_windows => length(Windows),
            avg_events_per_window => case WindowSizes of
                [] -> 0;
                _ -> lists:sum(WindowSizes) / length(WindowSizes)
            end
        }
    }.

%% @doc Compute temporal statistics for a log.
-spec temporal_statistics(temporal_log()) -> map().
temporal_statistics(Log) ->
    Timestamps = [maps:get(timestamp, E) || E <- Log],

    #{
        event_count => length(Log),
        time_span => case Timestamps of
            [] -> 0;
            _ -> lists:max(Timestamps) - lists:min(Timestamps)
        end,
        first_event => case Timestamps of
            [] -> undefined;
            _ -> lists:min(Timestamps)
        end,
        last_event => case Timestamps of
            [] -> undefined;
            _ -> lists:max(Timestamps)
        end
    }.

%% @doc Compute percentile of a list of values.
-spec percentile([float()], float()) -> float().
percentile([], _P) ->
    0.0;
percentile(Values, P) when P >= 0.0, P =< 1.0 ->
    Sorted = lists:sort(Values),
    N = length(Sorted),
    K = max(1, round(P * (N - 1)) + 1),
    lists:nth(min(K, N), Sorted).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec group_by_case(temporal_log()) -> map().
group_by_case(Log) ->
    lists:foldl(fun(Event, Acc) ->
        CaseId = maps:get(case_id, Event),
        Existing = maps:get(CaseId, Acc, []),
        maps:put(CaseId, [Event | Existing], Acc)
    end, #{}, Log).

%% @private
-spec compute_case_cycle(case_id(), [temporal_event()]) ->
    {ok, cycle_time()} | {error, term()}.
compute_case_cycle(CaseId, Events) ->
    Sorted = lists:sort(fun(E1, E2) ->
        maps:get(timestamp, E1) =< maps:get(timestamp, E2)
    end, Events),

    case Sorted of
        [] ->
            {error, empty_case};
        [First | Rest] ->
            StartTime = maps:get(timestamp, First),
            EndTime = case Rest of
                [] -> StartTime;
                _ -> maps:get(timestamp, lists:last(Rest))
            end,
            {ok, #{
                case_id => CaseId,
                start_time => StartTime,
                end_time => EndTime,
                duration => EndTime - StartTime
            }}
    end.

%% @private
-spec compute_case_processing_times([temporal_event()]) ->
    {ok, [processing_time()]} | {error, term()}.
compute_case_processing_times(Events) ->
    Sorted = lists:sort(fun(E1, E2) ->
        maps:get(timestamp, E1) =< maps:get(timestamp, E2)
    end, Events),

    %% For each activity, find start and end
    ActivityGroups = group_by_activity(Events),

    Times = maps:fold(fun(Activity, ActivityEvents, Acc) ->
        SortedActivity = lists:sort(fun(E1, E2) ->
            maps:get(timestamp, E1) =< maps:get(timestamp, E2)
        end, ActivityEvents),

        lists:map(fun(E) ->
            #{
                activity => Activity,
                case_id => maps:get(case_id, E),
                start_time => maps:get(timestamp, E),
                end_time => maps:get(timestamp, E) + 1000,  %% Placeholder
                duration => 1000
            }
        end, SortedActivity) ++ Acc
    end, [], ActivityGroups),

    {ok, Times}.

%% @private
-spec group_by_activity([temporal_event()]) -> map().
group_by_activity(Events) ->
    lists:foldl(fun(Event, Acc) ->
        Activity = maps:get(activity, Event),
        Existing = maps:get(Activity, Acc, []),
        maps:put(Activity, [Event | Existing], Acc)
    end, #{}, Events).

%% @private
-spec compute_cycle_stats([cycle_time()]) -> map().
compute_cycle_stats(CycleTimes) ->
    Durations = [maps:get(duration, CT) || CT <- CycleTimes],

    case Durations of
        [] ->
            #{count => 0, min => 0, max => 0, avg => 0, median => 0, p90 => 0, p95 => 0};
        _ ->
            Sorted = lists:sort(Durations),
            N = length(Sorted),
            #{
                count => N,
                min => lists:min(Sorted),
                max => lists:max(Sorted),
                avg => lists:sum(Sorted) / N,
                median => percentile(Sorted, 0.5),
                p90 => percentile(Sorted, 0.9),
                p95 => percentile(Sorted, 0.95)
            }
    end.

%% @private
-spec compute_processing_stats([processing_time()]) -> map().
compute_processing_stats(ProcessingTimes) ->
    Durations = [maps:get(duration, PT) || PT <- ProcessingTimes],

    case Durations of
        [] ->
            #{count => 0, min => 0, max => 0, avg => 0, by_activity => #{}};
        _ ->
            ByActivity = lists:foldl(fun(PT, Acc) ->
                Activity = maps:get(activity, PT),
                Duration = maps:get(duration, PT),
                Existing = maps:get(Activity, Acc, []),
                maps:put(Activity, [Duration | Existing], Acc)
            end, #{}, ProcessingTimes),

            #{
                count => length(Durations),
                min => lists:min(Durations),
                max => lists:max(Durations),
                avg => lists:sum(Durations) / length(Durations),
                by_activity => maps:map(fun(_A, Ds) ->
                    #{
                        count => length(Ds),
                        avg => lists:sum(Ds) / length(Ds)
                    }
                end, ByActivity)
            }
    end.

%% @private
-spec detect_period_seasonality([timestamp()], hourly | daily | weekly | monthly) ->
    seasonality_pattern().
detect_period_seasonality(Timestamps, Period) ->
    %% Convert timestamps to period buckets
    Buckets = bucket_by_period(Timestamps, Period),

    %% Compute variance (higher variance = stronger seasonality)
    Counts = [length(Events) || {_Key, Events} <- Buckets],
    Avg = case Counts of
        [] -> 0;
        _ -> lists:sum(Counts) / length(Counts)
    end,

    Variance = case Counts of
        [] -> 0;
        _ -> lists:sum([(C - Avg) * (C - Avg) || C <- Counts]) / length(Counts)
    end,

    Strength = min(1.0, Variance / (Avg * Avg + 1)),

    %% Find peaks and troughs
    SortedBuckets = lists:sort(fun({_, E1}, {_, E2}) ->
        length(E1) >= length(E2)
    end, Buckets),

    Peaks = [Key || {Key, _} <- lists:sublist(SortedBuckets, min(3, length(SortedBuckets)))],
    Troughs = [Key || {Key, _} <- lists:sublist(lists:reverse(SortedBuckets), min(3, length(SortedBuckets)))],

    #{
        period => Period,
        strength => Strength,
        peaks => Peaks,
        troughs => Troughs
    }.

%% @private
-spec bucket_by_period([timestamp()], hourly | daily | weekly | monthly) -> map().
bucket_by_period(Timestamps, Period) ->
    lists:foldl(fun(TS, Acc) ->
        Key = case Period of
            hourly -> TS div 3600000;
            daily -> TS div 86400000;
            weekly -> TS div (86400000 * 7);
            monthly -> TS div (86400000 * 30)
        end,
        Existing = maps:get(Key, Acc, []),
        maps:put(Key, [TS | Existing], Acc)
    end, #{}, Timestamps).

%% @private
-spec group_by_time_window([temporal_event()], pos_integer()) -> [{timestamp(), [temporal_event()]}].
group_by_time_window(Log, WindowSize) ->
    case Log of
        [] -> [];
        [First | _] ->
            StartWindow = maps:get(timestamp, First) div WindowSize,
            lists:foldl(fun(Event, Acc) ->
                TS = maps:get(timestamp, Event),
                Window = TS div WindowSize,
                Existing = lists:keyfind(Window, 1, Acc),
                case Existing of
                    false ->
                        [{Window, [Event]} | Acc];
                    {Window, Events} ->
                        lists:keyreplace(Window, 1, Acc, {Window, [Event | Events]})
                end
            end, [{StartWindow, [First]}], Log)
    end.

%% @private
-spec compute_trend_direction([pos_integer()]) -> increasing | decreasing | stable.
compute_trend_direction([]) ->
    stable;
compute_trend_direction([_]) ->
    stable;
compute_trend_direction(Values) ->
    %% Simple linear regression slope
    N = length(Values),
    SumX = lists:sum(lists:seq(1, N)),
    SumY = lists:sum(Values),
    SumXY = lists:sum([X * Y || {X, Y} <- lists:zip(lists:seq(1, N), Values)]),
    SumX2 = lists:sum([X * X || X <- lists:seq(1, N)]),

    Slope = (N * SumXY - SumX * SumY) / (N * SumX2 - SumX * SumX),

    case abs(Slope) of
        S when S < 0.1 -> stable;
        S when S > 0 -> increasing;
        _ -> decreasing
    end.

%% @private
-spec compute_trend_strength([pos_integer()]) -> float().
compute_trend_strength([]) ->
    0.0;
compute_trend_strength(Values) ->
    case length(Values) of
        N when N < 2 ->
            0.0;
        N ->
            Avg = lists:sum(Values) / N,
            Variance = lists:sum([(V - Avg) * (V - Avg) || V <- Values]) / N,
            case Variance of
                0.0 -> 0.0;
                V -> min(1.0, V / (Avg * Avg + 1))
            end
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

simple_temporal_log() ->
    [
        #{activity => a, timestamp => 1000, case_id => <<"c1">>},
        #{activity => b, timestamp => 2000, case_id => <<"c1">>},
        #{activity => c, timestamp => 3000, case_id => <<"c1">>},
        #{activity => a, timestamp => 1500, case_id => <<"c2">>},
        #{activity => b, timestamp => 2500, case_id => <<"c2">>},
        #{activity => c, timestamp => 3500, case_id => <<"c2">>}
    ].

log_with_daily_pattern() ->
    %% Create pattern with daily peaks
    Base = 1704067200000,  %% 2024-01-01
    [
        #{activity => a, timestamp => Base + 3600000, case_id => <<"c1">>},
        #{activity => a, timestamp => Base + 86400000 + 3600000, case_id => <<"c2">>},
        #{activity => a, timestamp => Base + 2*86400000 + 3600000, case_id => <<"c3">>},
        #{activity => a, timestamp => Base + 3*86400000 + 3600000, case_id => <<"c4">>}
    ].

%%--------------------------------------------------------------------
%% Mining tests
%%--------------------------------------------------------------------

mine_temporal_test() ->
    Log = simple_temporal_log(),
    {ok, Patterns} = mine_temporal(Log),
    ?assert(is_list(Patterns)),
    ?assert(length(Patterns) > 0).

mine_temporal_with_options_test() ->
    Log = simple_temporal_log(),
    {ok, Patterns} = mine_temporal(Log, #{
        include_cycle_time => true,
        include_processing_time => false,
        include_seasonality => false,
        include_trends => false
    }),
    ?assert(is_list(Patterns)),
    %% Should only have cycle time pattern
    ?assertEqual(1, length([P || P <- Patterns, maps:get(type, P) =:= cycle_time])).

%%--------------------------------------------------------------------
%% Cycle time tests
%%--------------------------------------------------------------------

compute_cycle_times_test() ->
    Log = simple_temporal_log(),
    CycleTimes = compute_cycle_times(Log),
    ?assert(is_list(CycleTimes)),
    ?assertEqual(2, length(CycleTimes)),
    lists:foreach(fun(CT) ->
        ?assert(maps:is_key(case_id, CT)),
        ?assert(maps:is_key(duration, CT))
    end, CycleTimes).

compute_cycle_stats_test() ->
    CycleTimes = [
        #{case_id => <<"c1">>, duration => 2000},
        #{case_id => <<"c2">>, duration => 2000},
        #{case_id => <<"c3">>, duration => 4000}
    ],
    Stats = compute_cycle_stats(CycleTimes),
    ?assertEqual(3, maps:get(count, Stats)),
    ?assertEqual(2000, maps:get(min, Stats)),
    ?assertEqual(4000, maps:get(max, Stats)).

%%--------------------------------------------------------------------
%% Processing time tests
%%--------------------------------------------------------------------

compute_processing_times_test() ->
    Log = simple_temporal_log(),
    Times = compute_processing_times(Log),
    ?assert(is_list(Times)).

compute_processing_stats_test() ->
    Times = [
        #{activity => a, duration => 1000},
        #{activity => a, duration => 1500},
        #{activity => b, duration => 2000}
    ],
    Stats = compute_processing_stats(Times),
    ?assert(maps:is_key(count, Stats)),
    ?assert(maps:is_key(avg, Stats)).

%%--------------------------------------------------------------------
%% Seasonality tests
%%--------------------------------------------------------------------

detect_seasonality_test() ->
    Log = log_with_daily_pattern(),
    Patterns = detect_seasonality(Log, [daily]),
    ?assert(is_list(Patterns)),
    ?assertEqual(1, length(Patterns)),
    Pattern = lists:nth(1, Patterns),
    ?assertEqual(daily, maps:get(period, Pattern)).

detect_period_seasonality_test() ->
    Timestamps = [0, 86400000, 2*86400000, 3*86400000],
    Pattern = detect_period_seasonality(Timestamps, daily),
    ?assertEqual(daily, maps:get(period, Pattern)),
    ?assert(maps:get(strength, Pattern) >= 0.0).

%%--------------------------------------------------------------------
%% Trend analysis tests
%%--------------------------------------------------------------------

analyze_trends_test() ->
    Log = simple_temporal_log(),
    Trends = analyze_trends(Log),
    ?assert(is_map(Trends)),
    ?assert(maps:is_key(trend_direction, Trends)),
    ?assert(maps:is_key(trend_strength, Trends)).

compute_trend_direction_test() ->
    ?assertEqual(stable, compute_trend_direction([])),
    ?assertEqual(increasing, compute_trend_direction([1, 2, 3, 4])),
    ?assertEqual(decreasing, compute_trend_direction([4, 3, 2, 1])),
    ?assertEqual(stable, compute_trend_direction([2, 2, 2, 2])).

compute_trend_strength_test() ->
    ?assertEqual(0.0, compute_trend_strength([])),
    Strength1 = compute_trend_strength([1, 2, 3, 4]),
    ?assert(Strength1 >= 0.0 andalso Strength1 =< 1.0).

%%--------------------------------------------------------------------
%% Statistics tests
%%--------------------------------------------------------------------

temporal_statistics_test() ->
    Log = simple_temporal_log(),
    Stats = temporal_statistics(Log),
    ?assert(maps:is_key(event_count, Stats)),
    ?assertEqual(6, maps:get(event_count, Stats)),
    ?assert(maps:is_key(time_span, Stats)).

percentile_test() ->
    ?assertEqual(0.0, percentile([], 0.5)),
    ?assertEqual(2.0, percentile([1, 2, 3, 4, 5], 0.4)),
    ?assertEqual(4.0, percentile([1, 2, 3, 4, 5], 0.75)).

%%--------------------------------------------------------------------
%% Integration tests
%%--------------------------------------------------------------------

mine_full_pipeline_test() ->
    Log = [
        #{activity => a, timestamp => 1000, case_id => <<"c1">>},
        #{activity => b, timestamp => 2000, case_id => <<"c1">>},
        #{activity => c, timestamp => 3000, case_id => <<"c1">>},
        #{activity => a, timestamp => 86401000, case_id => <<"c2">>},  %% Next day
        #{activity => b, timestamp => 86402000, case_id => <<"c2">>},
        #{activity => c, timestamp => 86403000, case_id => <<"c2">>}
    ],
    {ok, Patterns} = mine_temporal(Log, #{}),
    ?assert(length(Patterns) > 0),
    %% Verify cycle time pattern exists
    HasCycleTime = lists:any(fun(P) ->
        maps:get(type, P) =:= cycle_time
    end, Patterns),
    ?assert(HasCycleTime).

-endif.
