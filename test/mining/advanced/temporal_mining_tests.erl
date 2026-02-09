%% -*- erlang -*-
%% @doc Tests for Temporal Process Mining

-module(temporal_mining_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
;; Test Fixtures
;;====================================================================

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
    Base = 1704067200000,
    [
        #{activity => a, timestamp => Base + 3600000, case_id => <<"c1">>},
        #{activity => a, timestamp => Base + 86400000 + 3600000, case_id => <<"c2">>},
        #{activity => a, timestamp => Base + 2*86400000 + 3600000, case_id => <<"c3">>},
        #{activity => a, timestamp => Base + 3*86400000 + 3600000, case_id => <<"c4">>}
    ].

log_with_cycle_times() ->
    [
        #{activity => a, timestamp => 1000, case_id => <<"c1">>},
        #{activity => b, timestamp => 2000, case_id => <<"c1">>},
        #{activity => c, timestamp => 5000, case_id => <<"c1">>},
        #{activity => a, timestamp => 1000, case_id => <<"c2">>},
        #{activity => b, timestamp => 1500, case_id => <<"c2">>},
        #{activity => c, timestamp => 3000, case_id => <<"c2">>}
    ].

empty_log() ->
    [].

%%====================================================================
;; Mining Tests
;;====================================================================

mine_temporal_test() ->
    Log = simple_temporal_log(),
    {ok, Patterns} = temporal_mining:mine_temporal(Log),
    ?assert(is_list(Patterns)),
    ?assert(length(Patterns) > 0).

mine_temporal_with_options_test() ->
    Log = simple_temporal_log(),
    {ok, Patterns} = temporal_mining:mine_temporal(Log, #{
        include_cycle_time => true,
        include_processing_time => false,
        include_seasonality => false,
        include_trends => false
    }),
    ?assert(is_list(Patterns)),
    ?assertEqual(1, length([P || P <- Patterns, maps:get(type, P) =:= cycle_time])).

mine_temporal_all_options_test() ->
    Log = simple_temporal_log(),
    {ok, Patterns} = temporal_mining:mine_temporal(Log, #{
        include_cycle_time => true,
        include_processing_time => true,
        include_seasonality => true,
        include_trends => true
    }),
    ?assert(length(Patterns) >= 4).

mine_temporal_empty_log_test() ->
    Log = empty_log(),
    {ok, Patterns} = temporal_mining:mine_temporal(Log),
    ?assert(is_list(Patterns)).

%%====================================================================
;; Cycle Time Tests
;;====================================================================

compute_cycle_times_test() ->
    Log = simple_temporal_log(),
    CycleTimes = temporal_mining:compute_cycle_times(Log),
    ?assert(is_list(CycleTimes)),
    ?assertEqual(2, length(CycleTimes)),
    lists:foreach(fun(CT) ->
        ?assert(maps:is_key(case_id, CT)),
        ?assert(maps:is_key(duration, CT))
    end, CycleTimes).

compute_cycle_times_different_durations_test() ->
    Log = log_with_cycle_times(),
    CycleTimes = temporal_mining:compute_cycle_times(Log),
    ?assertEqual(2, length(CycleTimes)),
    Durations = [maps:get(duration, CT) || CT <- CycleTimes],
    %% Should have different durations
    ?assert(lists:min(Durations) < lists:max(Durations)).

compute_cycle_stats_test() ->
    CycleTimes = [
        #{case_id => <<"c1">>, duration => 2000},
        #{case_id => <<"c2">>, duration => 2000},
        #{case_id => <<"c3">>, duration => 4000}
    ],
    Stats = temporal_mining:compute_cycle_stats(CycleTimes),
    ?assertEqual(3, maps:get(count, Stats)),
    ?assertEqual(2000, maps:get(min, Stats)),
    ?assertEqual(4000, maps:get(max, Stats)).

compute_cycle_stats_empty_test() ->
    Stats = temporal_mining:compute_cycle_stats([]),
    ?assertEqual(0, maps:get(count, Stats)),
    ?assertEqual(0, maps:get(min, Stats)),
    ?assertEqual(0, maps:get(max, Stats)).

compute_cycle_stats_single_test() ->
    CycleTimes = [#{case_id => <<"c1">>, duration => 1000}],
    Stats = temporal_mining:compute_cycle_stats(CycleTimes),
    ?assertEqual(1, maps:get(count, Stats)),
    ?assertEqual(1000, maps:get(min, Stats)),
    ?assertEqual(1000, maps:get(max, Stats)).

%%====================================================================
;; Processing Time Tests
;;====================================================================

compute_processing_times_test() ->
    Log = simple_temporal_log(),
    Times = temporal_mining:compute_processing_times(Log),
    ?assert(is_list(Times)).

compute_processing_stats_test() ->
    Times = [
        #{activity => a, duration => 1000},
        #{activity => a, duration => 1500},
        #{activity => b, duration => 2000}
    ],
    Stats = temporal_mining:compute_processing_stats(Times),
    ?assert(maps:is_key(count, Stats)),
    ?assert(maps:is_key(avg, Stats)).

compute_processing_stats_empty_test() ->
    Stats = temporal_mining:compute_processing_stats([]),
    ?assertEqual(0, maps:get(count, Stats)),
    ?assertEqual(0, maps:get(avg, Stats)).

compute_processing_stats_by_activity_test() ->
    Times = [
        #{activity => a, duration => 1000},
        #{activity => a, duration => 2000},
        #{activity => b, duration => 1500}
    ],
    Stats = temporal_mining:compute_processing_stats(Times),
    ByActivity = maps:get(by_activity, Stats),
    ?assert(maps:is_key(a, ByActivity)),
    ?assert(maps:is_key(b, ByActivity)).

%%====================================================================
;; Seasonality Tests
;;====================================================================

detect_seasonality_test() ->
    Log = log_with_daily_pattern(),
    Patterns = temporal_mining:detect_seasonality(Log, [daily]),
    ?assert(is_list(Patterns)),
    ?assertEqual(1, length(Patterns)),
    Pattern = lists:nth(1, Patterns),
    ?assertEqual(daily, maps:get(period, Pattern)).

detect_seasonality_multiple_periods_test() ->
    Log = log_with_daily_pattern(),
    Patterns = temporal_mining:detect_seasonality(Log, [daily, weekly]),
    ?assertEqual(2, length(Patterns)).

detect_period_seasonality_test() ->
    Timestamps = [0, 86400000, 2*86400000, 3*86400000],
    Pattern = temporal_mining:detect_period_seasonality(Timestamps, daily),
    ?assertEqual(daily, maps:get(period, Pattern)),
    ?assert(maps:get(strength, Pattern) >= 0.0).

detect_period_seasonality_weekly_test() ->
    Base = 1704067200000,
    Timestamps = [Base, Base + 7*86400000, Base + 14*86400000],
    Pattern = temporal_mining:detect_period_seasonality(Timestamps, weekly),
    ?assertEqual(weekly, maps:get(period, Pattern)).

detect_period_seasonality_hourly_test() ->
    Timestamps = [0, 3600000, 2*3600000, 3*3600000],
    Pattern = temporal_mining:detect_period_seasonality(Timestamps, hourly),
    ?assertEqual(hourly, maps:get(period, Pattern)).

detect_period_seasonality_empty_test() ->
    Pattern = temporal_mining:detect_period_seasonality([], daily),
    ?assert(maps:is_key(strength, Pattern)).

%%====================================================================
;; Trend Analysis Tests
;;====================================================================

analyze_trends_test() ->
    Log = simple_temporal_log(),
    Trends = temporal_mining:analyze_trends(Log),
    ?assert(is_map(Trends)),
    ?assert(maps:is_key(trend_direction, Trends)),
    ?assert(maps:is_key(trend_strength, Trends)).

analyze_trends_empty_log_test() ->
    Trends = temporal_mining:analyze_trends([]),
    ?assert(is_map(Trends)),
    ?assert(maps:is_key(trend_direction, Trends)).

analyze_trends_increasing_test() ->
    Log = [
        #{activity => a, timestamp => 1000, case_id => <<"c1">>},
        #{activity => a, timestamp => 2000, case_id => <<"c2">>},
        #{activity => a, timestamp => 3000, case_id => <<"c3">>}
    ],
    Trends = temporal_mining:analyze_trends(Log),
    ?assert(maps:is_key(trend_direction, Trends)).

compute_trend_direction_test() ->
    ?assertEqual(stable, temporal_mining:compute_trend_direction([])),
    ?assertEqual(increasing, temporal_mining:compute_trend_direction([1, 2, 3, 4])),
    ?assertEqual(decreasing, temporal_mining:compute_trend_direction([4, 3, 2, 1])),
    ?assertEqual(stable, temporal_mining:compute_trend_direction([2, 2, 2, 2])),
    ?assertEqual(stable, temporal_mining:compute_trend_direction([1])).

compute_trend_strength_test() ->
    ?assertEqual(0.0, temporal_mining:compute_trend_strength([])),
    Strength1 = temporal_mining:compute_trend_strength([1, 2, 3, 4]),
    ?assert(Strength1 >= 0.0 andalso Strength1 =< 1.0),
    Strength2 = temporal_mining:compute_trend_strength([2, 2, 2, 2]),
    ?assertEqual(0.0, Strength2).

%%====================================================================
;; Statistics Tests
;;====================================================================

temporal_statistics_test() ->
    Log = simple_temporal_log(),
    Stats = temporal_mining:temporal_statistics(Log),
    ?assert(maps:is_key(event_count, Stats)),
    ?assertEqual(6, maps:get(event_count, Stats)),
    ?assert(maps:is_key(time_span, Stats)).

temporal_statistics_empty_test() ->
    Stats = temporal_mining:temporal_statistics([]),
    ?assertEqual(0, maps:get(event_count, Stats)),
    ?assertEqual(0, maps:get(time_span, Stats)).

temporal_statistics_single_event_test() ->
    Log = [#{activity => a, timestamp => 1000, case_id => <<"c1">>}],
    Stats = temporal_mining:temporal_statistics(Log),
    ?assertEqual(1, maps:get(event_count, Stats)),
    ?assertEqual(0, maps:get(time_span, Stats)).

percentile_test() ->
    ?assertEqual(0.0, temporal_mining:percentile([], 0.5)),
    ?assertEqual(2.0, temporal_mining:percentile([1, 2, 3, 4, 5], 0.4)),
    ?assertEqual(4.0, temporal_mining:percentile([1, 2, 3, 4, 5], 0.75)),
    ?assertEqual(3.0, temporal_mining:percentile([1, 2, 3, 4, 5], 0.5)).

percentile_edge_cases_test() ->
    ?assertEqual(1.0, temporal_mining:percentile([1, 2, 3], 0.0)),
    ?assertEqual(3.0, temporal_mining:percentile([1, 2, 3], 1.0)),
    ?assertEqual(1.0, temporal_mining:percentile([1], 0.5)).

%%====================================================================
;; Integration Tests
;;====================================================================

mine_full_pipeline_test() ->
    Log = [
        #{activity => a, timestamp => 1000, case_id => <<"c1">>},
        #{activity => b, timestamp => 2000, case_id => <<"c1">>},
        #{activity => c, timestamp => 3000, case_id => <<"c1">>},
        #{activity => a, timestamp => 86401000, case_id => <<"c2">>},
        #{activity => b, timestamp => 86402000, case_id => <<"c2">>},
        #{activity => c, timestamp => 86403000, case_id => <<"c2">>}
    ],
    {ok, Patterns} = temporal_mining:mine_temporal(Log, #{}),
    ?assert(length(Patterns) > 0),
    %% Verify cycle time pattern exists
    HasCycleTime = lists:any(fun(P) ->
        maps:get(type, P) =:= cycle_time
    end, Patterns),
    ?assert(HasCycleTime).

mine_and_analyze_test() ->
    Log = simple_temporal_log(),
    {ok, Patterns} = temporal_mining:mine_temporal(Log),

    %% Check each pattern has required fields
    lists:foreach(fun(P) ->
        Type = maps:get(type, P),
        ?assert(maps:is_key(data, P)),
        Data = maps:get(data, P),

        case Type of
            cycle_time ->
                ?assert(maps:is_key(cycle_times, Data)),
                ?assert(maps:is_key(statistics, Data));
            seasonality ->
                ?assert(maps:is_key(period, Data));
            trend ->
                ?assert(maps:is_key(trend_direction, Data));
            _ ->
                ok
        end
    end, Patterns).

temporal_statistics_with_patterns_test() ->
    Log = simple_temporal_log(),
    Stats = temporal_mining:temporal_statistics(Log),
    {ok, Patterns} = temporal_mining:mine_temporal(Log),

    %% Verify consistency
    EventCount = maps:get(event_count, Stats),
    ?assertEqual(6, EventCount),
    ?assert(length(Patterns) > 0).

%%====================================================================
;; Time Window Tests
;;====================================================================

group_by_time_window_test() ->
    Log = [
        #{activity => a, timestamp => 1000, case_id => <<"c1">>},
        #{activity => a, timestamp => 2000, case_id => <<"c2">>},
        #{activity => a, timestamp => 90000000, case_id => <<"c3">>}
    ],
    Windows = temporal_mining:group_by_time_window(Log, 86400000),
    ?assert(is_list(Windows)),
    ?assert(length(Windows) >= 1).

bucket_by_period_test() ->
    Timestamps = [0, 3600000, 7200000, 86400000],
    Buckets = temporal_mining:bucket_by_period(Timestamps, hourly),
    ?assert(is_map(Buckets)),
    ?assert(maps:size(Buckets) > 0).

bucket_by_period_daily_test() ->
    Timestamps = [0, 86400000, 2*86400000],
    Buckets = temporal_mining:bucket_by_period(Timestamps, daily),
    ?assertEqual(3, maps:size(Buckets)).
