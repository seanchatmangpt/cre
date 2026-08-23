%%%-------------------------------------------------------------------
%%% @doc f5_bench_pressure test suite
%%%
%%% Tests for memory and scheduler pressure benchmarking:
%%% - Memory snapshot and profiling
%%% - Scheduler utilization measurement
%%% - Pressure point detection
%%% - Report formatting
%%% @end
%%%-------------------------------------------------------------------
-module(f5_bench_pressure_test).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Data
%%%-------------------------------------------------------------------

%% Simple workload that allocates memory
memory_workload() ->
    %% Allocate some binary data
    _ = [<<X:32>> || X <- lists:seq(1, 10000)],
    ok.

%% CPU-bound workload
cpu_workload() ->
    _ = [X * X || X <- lists:seq(1, 100000)],
    ok.

%% Spawning workload
spawn_workload() ->
    Pids = [spawn(fun() -> timer:sleep(100) end) || _ <- lists:seq(1, 10)],
    timer:sleep(50),
    _ = [exit(Pid, kill) || Pid <- Pids],
    ok.

%% Create a memory snapshot for testing
test_memory_snapshot(Total, Processes, Atom, Binary, ETS) ->
    #{
        timestamp => erlang:monotonic_time(millisecond),
        total => Total,
        processes => Processes,
        processes_used => Processes div 2,
        system => Total - Processes,
        atom => Atom,
        binary => Binary,
        ets => ETS,
        code => 1000000
    }.

%% Create a scheduler snapshot for testing
test_scheduler_snapshot(Utilization, MaxQueue, AvgQueue) ->
    #{
        timestamp => erlang:monotonic_time(millisecond),
        scheduler_count => 4,
        scheduler_utilization => [{1, Utilization}, {2, Utilization},
                                  {3, Utilization}, {4, Utilization}],
        total_utilization => Utilization,
        io_utilization => Utilization * 0.1,
        cpu_utilization => Utilization * 0.9,
        run_queue_lengths => [{1, MaxQueue}, {2, MaxQueue div 2},
                              {3, MaxQueue div 3}, {4, 0}],
        max_run_queue => MaxQueue,
        avg_run_queue => AvgQueue
    }.

%%%-------------------------------------------------------------------
%%% Memory Pressure Tests
%%%-------------------------------------------------------------------

%% @doc Test memory_profile returns valid snapshot
memory_profile_returns_snapshot_test() ->
    Snapshot = f5_bench_pressure:memory_profile(all),

    ?assert(is_map(Snapshot)),
    ?assert(maps:is_key(timestamp, Snapshot)),
    ?assert(maps:is_key(total, Snapshot)),
    ?assert(maps:is_key(processes, Snapshot)),
    ?assert(maps:is_key(atom, Snapshot)),
    ?assert(maps:is_key(binary, Snapshot)),
    ?assert(maps:is_key(ets, Snapshot)).

%% @doc Test memory_profile for specific pid
memory_profile_pid_test() ->
    Snapshot = f5_bench_pressure:memory_profile(self()),

    ?assert(is_map(Snapshot)),
    ?assert(maps:is_key(total, Snapshot)).

%% @doc Test measure_memory_pressure with simple workload
measure_memory_pressure_simple_test() ->
    Result = f5_bench_pressure:measure_memory_pressure(fun() -> memory_workload() end),

    ?assertMatch(#{workload_name := <<"memory_pressure">>}, Result),
    ?assert(maps:is_key(memory, Result)),
    ?assert(maps:is_key(duration_ms, Result)).

%% @doc Test measure_memory_pressure captures before/after
measure_memory_pressure_before_after_test() ->
    Result = f5_bench_pressure:measure_memory_pressure(fun memory_workload/0),

    MemMap = maps:get(memory, Result),
    ?assert(maps:is_key(before, MemMap)),
    ?assert(maps:is_key('after', MemMap)),
    ?assert(maps:is_key(diff, MemMap)),

    Before = maps:get(before, MemMap),
    After = maps:get('after', MemMap),
    Diff = maps:get(diff, MemMap),

    ?assert(maps:is_key(total, Before)),
    ?assert(maps:is_key(total, After)),
    ?assert(maps:is_key(total, Diff)).

%% @doc Test measure_memory_pressure with options
measure_memory_pressure_with_options_test() ->
    Options = #{
        sample_interval => 50,
        gc_before => true,
        gc_after => true
    },
    Result = f5_bench_pressure:measure_memory_pressure(fun memory_workload/0, Options),

    ?assertMatch(#{duration_ms := _}, Result),
    ?assert(maps:is_key(memory, Result)).

%% @doc Test memory_diff calculates correct differences
memory_diff_calculates_differences_test() ->
    Before = test_memory_snapshot(1000000, 500000, 100000, 200000, 50000),
    After = test_memory_snapshot(1200000, 600000, 110000, 250000, 60000),

    Diff = f5_bench_pressure:memory_diff(Before, After),

    ?assertEqual(200000, maps:get(total, Diff)),
    ?assertEqual(100000, maps:get(processes, Diff)),
    ?assertEqual(10000, maps:get(atom, Diff)),
    ?assertEqual(50000, maps:get(binary, Diff)),
    ?assertEqual(10000, maps:get(ets, Diff)).

%% @doc Test memory_diff with negative growth (memory freed)
memory_diff_negative_growth_test() ->
    Before = test_memory_snapshot(2000000, 1000000, 200000, 400000, 100000),
    After = test_memory_snapshot(1500000, 750000, 180000, 300000, 80000),

    Diff = f5_bench_pressure:memory_diff(Before, After),

    ?assertEqual(-500000, maps:get(total, Diff)),
    ?assertEqual(-250000, maps:get(processes, Diff)),
    ?assertEqual(-20000, maps:get(atom, Diff)),
    ?assertEqual(-100000, maps:get(binary, Diff)).

%%%-------------------------------------------------------------------
%%% Scheduler Pressure Tests
%%%-------------------------------------------------------------------

%% @doc Test scheduler_profile returns valid snapshot
scheduler_profile_returns_snapshot_test() ->
    %% Enable scheduler wall time for accurate measurement
    erlang:system_flag(scheduler_wall_time, true),
    Snapshot = f5_bench_pressure:scheduler_profile(),
    erlang:system_flag(scheduler_wall_time, false),

    ?assert(is_map(Snapshot)),
    ?assert(maps:is_key(timestamp, Snapshot)),
    ?assert(maps:is_key(scheduler_count, Snapshot)),
    ?assert(maps:is_key(total_utilization, Snapshot)),
    ?assert(maps:is_key(max_run_queue, Snapshot)),
    ?assert(maps:is_key(avg_run_queue, Snapshot)).

%% @doc Test scheduler_profile has scheduler_count
scheduler_profile_scheduler_count_test() ->
    erlang:system_flag(scheduler_wall_time, true),
    Snapshot = f5_bench_pressure:scheduler_profile(),
    erlang:system_flag(scheduler_wall_time, false),

    SchedulerCount = maps:get(scheduler_count, Snapshot),
    ?assert(SchedulerCount > 0),
    ?assert(is_integer(SchedulerCount)).

%% @doc Test measure_scheduler_pressure with workload
measure_scheduler_pressure_simple_test() ->
    Result = f5_bench_pressure:measure_scheduler_pressure(fun cpu_workload/0),

    ?assertMatch(#{workload_name := <<"scheduler_pressure">>}, Result),
    ?assert(maps:is_key(scheduler, Result)),
    ?assert(maps:is_key(duration_ms, Result)).

%% @doc Test measure_scheduler_pressure captures utilization
measure_scheduler_pressure_utilization_test() ->
    Result = f5_bench_pressure:measure_scheduler_pressure(fun cpu_workload/0),

    SchedMap = maps:get(scheduler, Result),
    ?assert(maps:is_key(before, SchedMap)),
    ?assert(maps:is_key('after', SchedMap)),
    ?assert(maps:is_key(peak_utilization, SchedMap)),
    ?assert(maps:is_key(avg_utilization, SchedMap)),
    ?assert(maps:is_key(utilization_samples, SchedMap)).

%% @doc Test scheduler_diff calculates differences
scheduler_diff_calculates_differences_test() ->
    Before = test_scheduler_snapshot(50.0, 10, 5.0),
    After = test_scheduler_snapshot(75.0, 20, 10.0),

    Diff = f5_bench_pressure:scheduler_diff(Before, After),

    ?assertEqual(25.0, maps:get(total_utilization, Diff)),
    ?assertEqual(10, maps:get(max_run_queue, Diff)),
    ?assertEqual(5.0, maps:get(avg_run_queue, Diff)).

%% @doc Test measure_scheduler_pressure with options
measure_scheduler_pressure_with_options_test() ->
    Options = #{
        sample_interval => 25,
        scheduler_wall_time => true
    },
    Result = f5_bench_pressure:measure_scheduler_pressure(fun cpu_workload/0, Options),

    ?assertMatch(#{samples_taken := N}, Result),
    Samples = maps:get(samples_taken, Result),
    ?assert(Samples >= 0).

%%%-------------------------------------------------------------------
%%% Pressure Point Detection Tests
%%%-------------------------------------------------------------------

%% @doc Test detect_pressure_points with normal result
detect_pressure_points_normal_test() ->
    Before = test_memory_snapshot(1000000, 500000, 50000, 50000, 25000),
    After = test_memory_snapshot(1100000, 550000, 55000, 55000, 27500),
    Diff = f5_bench_pressure:memory_diff(Before, After),

    SchedBefore = test_scheduler_snapshot(30.0, 5, 2.0),
    SchedAfter = test_scheduler_snapshot(40.0, 8, 3.0),

    PressureResult = #{
        workload_name => <<"test">>,
        duration_ms => 100,
        samples_taken => 5,
        memory => #{
            before => Before,
            'after' => After,
            peak => After,
            diff => Diff
        },
        scheduler => #{
            before => SchedBefore,
            'after' => SchedAfter,
            peak_utilization => 40.0,
            avg_utilization => 35.0,
            utilization_samples => [30.0, 35.0, 40.0, 35.0, 30.0]
        }
    },

    Points = f5_bench_pressure:detect_pressure_points(PressureResult),

    ?assert(is_list(Points)).

%% @doc Test detect_pressure_points detects high binary memory
detect_pressure_points_high_binary_test() ->
    %% High binary memory: 150 MB (above 100 MB threshold)
    Before = test_memory_snapshot(50000000, 25000000, 5000000, 50000000, 10000000),
    After = test_memory_snapshot(200000000, 75000000, 10000000, 150000000, 20000000),
    Diff = f5_bench_pressure:memory_diff(Before, After),

    SchedBefore = test_scheduler_snapshot(30.0, 5, 2.0),
    SchedAfter = test_scheduler_snapshot(40.0, 8, 3.0),

    PressureResult = #{
        workload_name => <<"test">>,
        duration_ms => 100,
        samples_taken => 1,
        memory => #{
            before => Before,
            'after' => After,
            peak => After,
            diff => Diff
        },
        scheduler => #{
            before => SchedBefore,
            'after' => SchedAfter,
            peak_utilization => 40.0,
            avg_utilization => 35.0,
            utilization_samples => []
        }
    },

    Points = f5_bench_pressure:detect_pressure_points(PressureResult),

    ?assert(length(Points) > 0),
    %% Should have a binary_leak point
    HasBinaryPoint = lists:any(fun(P) ->
        maps:get(type, P, undefined) =:= binary_leak
    end, Points),
    ?assert(HasBinaryPoint).

%% @doc Test detect_pressure_points detects critical scheduler
detect_pressure_points_critical_scheduler_test() ->
    %% Normal memory
    Before = test_memory_snapshot(1000000, 500000, 50000, 50000, 25000),
    After = test_memory_snapshot(1100000, 550000, 55000, 55000, 27500),
    Diff = f5_bench_pressure:memory_diff(Before, After),

    %% Critical scheduler utilization (97%)
    SchedBefore = test_scheduler_snapshot(30.0, 5, 2.0),
    SchedAfter = test_scheduler_snapshot(97.0, 8, 3.0),

    PressureResult = #{
        workload_name => <<"test">>,
        duration_ms => 100,
        samples_taken => 1,
        memory => #{
            before => Before,
            'after' => After,
            peak => After,
            diff => Diff
        },
        scheduler => #{
            before => SchedBefore,
            'after' => SchedAfter,
            peak_utilization => 97.0,
            avg_utilization => 80.0,
            utilization_samples => []
        }
    },

    Points = f5_bench_pressure:detect_pressure_points(PressureResult),

    ?assert(length(Points) > 0),
    %% Should have a critical scheduler point
    HasCritical = lists:any(fun(P) ->
        maps:get(severity, P, undefined) =:= critical
    end, Points),
    ?assert(HasCritical).

%% @doc Test detect_pressure_points detects high run queue
detect_pressure_points_high_run_queue_test() ->
    Before = test_memory_snapshot(1000000, 500000, 50000, 50000, 25000),
    After = test_memory_snapshot(1100000, 550000, 55000, 55000, 27500),
    Diff = f5_bench_pressure:memory_diff(Before, After),

    %% High run queue: 150 (above 100 threshold)
    SchedBefore = test_scheduler_snapshot(30.0, 5, 2.0),
    SchedAfter = test_scheduler_snapshot(50.0, 150, 75.0),

    PressureResult = #{
        workload_name => <<"test">>,
        duration_ms => 100,
        samples_taken => 1,
        memory => #{
            before => Before,
            'after' => After,
            peak => After,
            diff => Diff
        },
        scheduler => #{
            before => SchedBefore,
            'after' => SchedAfter,
            peak_utilization => 50.0,
            avg_utilization => 40.0,
            utilization_samples => []
        }
    },

    Points = f5_bench_pressure:detect_pressure_points(PressureResult),

    ?assert(length(Points) > 0),
    %% Should have a run_queue point
    HasRunQueue = lists:any(fun(P) ->
        maps:get(type, P, undefined) =:= run_queue
    end, Points),
    ?assert(HasRunQueue).

%% @doc Test detect_pressure_points detects memory growth (leak)
detect_pressure_points_memory_growth_test() ->
    %% Significant growth: 20 MB (above 10 MB threshold)
    Before = test_memory_snapshot(50000000, 25000000, 5000000, 10000000, 5000000),
    After = test_memory_snapshot(75000000, 35000000, 6000000, 30000000, 7000000),
    Diff = f5_bench_pressure:memory_diff(Before, After),

    SchedBefore = test_scheduler_snapshot(30.0, 5, 2.0),
    SchedAfter = test_scheduler_snapshot(40.0, 8, 3.0),

    PressureResult = #{
        workload_name => <<"test">>,
        duration_ms => 100,
        samples_taken => 1,
        memory => #{
            before => Before,
            'after' => After,
            peak => After,
            diff => Diff
        },
        scheduler => #{
            before => SchedBefore,
            'after' => SchedAfter,
            peak_utilization => 40.0,
            avg_utilization => 35.0,
            utilization_samples => []
        }
    },

    Points = f5_bench_pressure:detect_pressure_points(PressureResult),

    ?assert(length(Points) > 0),
    %% Should detect growth
    HasGrowth = lists:any(fun(P) ->
        case maps:get(metric, P, undefined) of
            <<"total_growth">> -> true;
            _ -> false
        end
    end, Points),
    ?assert(HasGrowth).

%%%-------------------------------------------------------------------
%%% Report Formatting Tests
%%%-------------------------------------------------------------------

%% @doc Test format_pressure_report with pressure result
format_pressure_report_result_test() ->
    Before = test_memory_snapshot(1000000, 500000, 50000, 50000, 25000),
    After = test_memory_snapshot(1100000, 550000, 55000, 55000, 27500),
    Diff = f5_bench_pressure:memory_diff(Before, After),

    SchedBefore = test_scheduler_snapshot(30.0, 5, 2.0),
    SchedAfter = test_scheduler_snapshot(50.0, 10, 4.0),

    PressureResult = #{
        workload_name => <<"test">>,
        duration_ms => 100,
        samples_taken => 5,
        memory => #{
            before => Before,
            'after' => After,
            peak => After,
            diff => Diff
        },
        scheduler => #{
            before => SchedBefore,
            'after' => SchedAfter,
            peak_utilization => 50.0,
            avg_utilization => 40.0,
            utilization_samples => []
        }
    },

    Report = f5_bench_pressure:format_pressure_report(PressureResult),

    ?assert(is_list(Report)),
    ?assertNotEqual([], Report).

%% @doc Test format_pressure_report with points list
format_pressure_report_points_test() ->
    Points = [
        #{
            type => memory,
            severity => high,
            description => <<"High memory usage">>,
            metric => <<"total">>,
            value => 200000000,
            threshold => 100000000
        },
        #{
            type => scheduler,
            severity => critical,
            description => <<"Scheduler overload">>,
            metric => <<"peak_utilization">>,
            value => 98.0,
            threshold => 95.0
        }
    ],

    Report = f5_bench_pressure:format_pressure_report(Points),

    ?assert(is_list(Report)),
    ?assertNotEqual([], Report).

%% @doc Test format_pressure_report with empty points
format_pressure_report_empty_test() ->
    Report = f5_bench_pressure:format_pressure_report([]),

    ?assert(is_list(Report)),
    ?assertNotEqual([], Report).

%%%-------------------------------------------------------------------
%%% Integration Tests
%%%-------------------------------------------------------------------

%% @doc Test full pressure measurement workflow
full_pressure_measurement_workflow_test() ->
    %% Create a realistic workload
    Workload = fun() ->
        %% Spawn some processes
        Pids = [spawn(fun() ->
            %% Allocate memory
            _ = [<<X:64>> || X <- lists:seq(1, 1000)],
            timer:sleep(50)
        end) || _ <- lists:seq(1, 5)],
        timer:sleep(100),
        _ = [exit(Pid, kill) || Pid <- Pids],
        ok
    end,

    Result = f5_bench_pressure:measure_memory_pressure(Workload),

    ?assertMatch(#{workload_name := <<"memory_pressure">>}, Result),
    ?assertMatch(#{duration_ms := Duration}, Result),
    Duration = maps:get(duration_ms, Result, 0),
    ?assert(Duration > 0),
    ?assertMatch(#{memory := #{before := _, 'after' := _}}, Result).

%% @doc Test combined memory and scheduler measurement
combined_measurement_test() ->
    SchedResult = f5_bench_pressure:measure_scheduler_pressure(fun cpu_workload/0),

    PeakUtil = maps:get(peak_utilization, maps:get(scheduler, SchedResult, #{}), 0.0),
    ?assert(is_float(PeakUtil)),
    ?assertMatch(#{samples_taken := N} when N >= 0, SchedResult).

%% @doc Test pressure detection with actual workload
actual_workload_pressure_detection_test() ->
    Workload = fun() ->
        %% Simulate binary allocation
        _ = [crypto:strong_rand_bytes(1024) || _ <- lists:seq(1, 100)],
        timer:sleep(50),
        ok
    end,

    Result = f5_bench_pressure:measure_memory_pressure(Workload),
    Points = f5_bench_pressure:detect_pressure_points(Result),

    ?assert(is_list(Points)).
