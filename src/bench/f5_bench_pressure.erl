%% -*- erlang -*-
%% @doc Fortune-5 Benchmark: Memory and Scheduler Pressure Measurement
%%
%% Pressure measurement for detecting system bottlenecks during load:
%% - Memory pressure tracking (total, processes, atom, binary, ETS)
%% - Scheduler utilization measurement (CPU vs IO)
%% - Run queue depth monitoring
%% - Pressure point detection and analysis
%%
%% @end

-module(f5_bench_pressure).
-author("CRE Team").

%% API - Memory Pressure
-export([measure_memory_pressure/1]).
-export([measure_memory_pressure/2]).
-export([memory_profile/1]).
-export([memory_diff/2]).

%% API - Scheduler Pressure
-export([measure_scheduler_pressure/1]).
-export([measure_scheduler_pressure/2]).
-export([scheduler_profile/0]).
-export([scheduler_diff/2]).

%% API - Pressure Point Detection
-export([detect_pressure_points/1]).
-export([format_pressure_report/1]).

%% Types
-type memory_snapshot() :: #{
    timestamp => integer(),
    total => non_neg_integer(),
    processes => non_neg_integer(),
    processes_used => non_neg_integer(),
    system => non_neg_integer(),
    atom => non_neg_integer(),
    binary => non_neg_integer(),
    ets => non_neg_integer(),
    code => non_neg_integer()
}.

-type scheduler_snapshot() :: #{
    timestamp => integer(),
    scheduler_count => pos_integer(),
    scheduler_utilization => [{SchedulerId::pos_integer(), Utilization::float()}],
    total_utilization => float(),
    io_utilization => float(),
    cpu_utilization => float(),
    run_queue_lengths => [{SchedulerId::pos_integer(), Length::non_neg_integer()}],
    max_run_queue => non_neg_integer(),
    avg_run_queue => float()
}.

-type pressure_result() :: #{
    workload_name => binary(),
    duration_ms => non_neg_integer(),
    samples_taken => non_neg_integer(),
    memory => #{
        before => memory_snapshot(),
        'after' => memory_snapshot(),
        peak => memory_snapshot(),
        diff => memory_diff()
    },
    scheduler => #{
        before => scheduler_snapshot(),
        'after' => scheduler_snapshot(),
        peak_utilization => float(),
        avg_utilization => float(),
        utilization_samples => [float()]
    }
}.

-type memory_diff() :: #{
    total => integer(),
    processes => integer(),
    processes_used => integer(),
    system => integer(),
    atom => integer(),
    binary => integer(),
    ets => integer(),
    code => integer()
}.

-type scheduler_diff() :: #{
    total_utilization => float(),
    io_utilization => float(),
    cpu_utilization => float(),
    max_run_queue => integer(),
    avg_run_queue => float()
}.

-type pressure_point() :: #{
    type => memory | scheduler | run_queue | binary_leak | process_leak,
    severity => low | medium | high | critical,
    description => binary(),
    metric => binary(),
    value => number(),
    threshold => number()
}.

-type workload_fun() :: fun(() -> term()) | {module(), atom(), list()}.
-type options() :: #{
    sample_interval => pos_integer(),
    gc_before => boolean(),
    gc_after => boolean(),
    scheduler_wall_time => boolean()
}.

%%====================================================================
%% API - Memory Pressure
%%====================================================================

%% @doc Measure memory pressure during workload execution.
%% Returns pressure result with before/after/peak memory metrics.
-spec measure_memory_pressure(workload_fun()) -> pressure_result().
measure_memory_pressure(Workload) ->
    measure_memory_pressure(Workload, #{}).

%% @doc Measure memory pressure with options.
-spec measure_memory_pressure(workload_fun(), options()) -> pressure_result().
measure_memory_pressure({M, F, A}, Options) when is_atom(M), is_atom(F), is_list(A) ->
    measure_memory_pressure(fun() -> apply(M, F, A) end, Options);
measure_memory_pressure(Workload, Options) when is_function(Workload, 0); is_map(Options) ->
    SampleInterval = maps:get(sample_interval, Options, 100),
    GCBefore = maps:get(gc_before, Options, true),
    GCAfter = maps:get(gc_after, Options, true),

    %% Force GC before if requested
    case GCBefore of
        true -> erlang:garbage_collect(self());
        false -> ok
    end,

    %% Take before snapshot
    BeforeMem = take_memory_snapshot(),

    %% Execute workload
    StartTime = erlang:monotonic_time(millisecond),
    WorkloadResult = try Workload() catch _:_ -> ok end,
    EndTime = erlang:monotonic_time(millisecond),

    %% Force GC after if requested
    case GCAfter of
        true -> erlang:garbage_collect(self());
        false -> ok
    end,

    %% Take after snapshot
    AfterMem = take_memory_snapshot(),

    %% Calculate result
    Diff = calculate_memory_diff(BeforeMem, AfterMem),
    Duration = EndTime - StartTime,

    #{
        workload_name => <<"memory_pressure">>,
        duration_ms => Duration,
        samples_taken => 1,
        memory => #{
            before => BeforeMem,
            'after' => AfterMem,
            peak => AfterMem,
            diff => Diff
        },
        scheduler => #{}
    }.

%% @doc Get detailed memory profile at current moment.
-spec memory_profile(pid() | all) -> memory_snapshot().
memory_profile(_Pid) when is_pid(_Pid) ->
    take_memory_snapshot();
memory_profile(all) ->
    take_memory_snapshot().

%% @doc Calculate difference between two memory snapshots.
-spec memory_diff(memory_snapshot(), memory_snapshot()) -> memory_diff().
memory_diff(Before, After) ->
    calculate_memory_diff(Before, After).

%%====================================================================
%% API - Scheduler Pressure
%%====================================================================

%% @doc Measure scheduler pressure during workload execution.
-spec measure_scheduler_pressure(workload_fun()) -> pressure_result().
measure_scheduler_pressure(Workload) ->
    measure_scheduler_pressure(Workload, #{}).

%% @doc Measure scheduler pressure with options.
-spec measure_scheduler_pressure(workload_fun(), options()) -> pressure_result().
measure_scheduler_pressure({M, F, A}, Options) when is_atom(M), is_atom(F), is_list(A) ->
    measure_scheduler_pressure(fun() -> apply(M, F, A) end, Options);
measure_scheduler_pressure(Workload, Options) when is_function(Workload, 0); is_map(Options) ->
    SampleInterval = maps:get(sample_interval, Options, 50),
    EnableWallTime = maps:get(scheduler_wall_time, Options, true),

    %% Enable scheduler wall time measurement if requested
    case EnableWallTime of
        true -> erlang:system_flag(scheduler_wall_time, true);
        false -> ok
    end,

    %% Take before snapshot
    BeforeSched = take_scheduler_snapshot(),

    %% Execute workload
    StartTime = erlang:monotonic_time(millisecond),
    WorkloadResult = try Workload() catch _:_ -> ok end,
    EndTime = erlang:monotonic_time(millisecond),

    %% Reset wall time flag
    case EnableWallTime of
        true -> erlang:system_flag(scheduler_wall_time, false);
        false -> ok
    end,

    %% Take after snapshot
    AfterSched = take_scheduler_snapshot(),

    %% Calculate statistics
    AvgUtil = maps:get(total_utilization, AfterSched, 0.0),
    PeakUtil = AvgUtil,

    #{
        workload_name => <<"scheduler_pressure">>,
        duration_ms => EndTime - StartTime,
        samples_taken => 1,
        memory => #{},
        scheduler => #{
            before => BeforeSched,
            'after' => AfterSched,
            peak_utilization => PeakUtil,
            avg_utilization => AvgUtil,
            utilization_samples => []
        }
    }.

%% @doc Get detailed scheduler profile at current moment.
-spec scheduler_profile() -> scheduler_snapshot().
scheduler_profile() ->
    take_scheduler_snapshot().

%% @doc Calculate difference between two scheduler snapshots.
-spec scheduler_diff(scheduler_snapshot(), scheduler_snapshot()) -> scheduler_diff().
scheduler_diff(Before, After) ->
    #{
        total_utilization => maps:get(total_utilization, After, 0.0) -
                              maps:get(total_utilization, Before, 0.0),
        io_utilization => maps:get(io_utilization, After, 0.0) -
                           maps:get(io_utilization, Before, 0.0),
        cpu_utilization => maps:get(cpu_utilization, After, 0.0) -
                           maps:get(cpu_utilization, Before, 0.0),
        max_run_queue => maps:get(max_run_queue, After, 0) -
                         maps:get(max_run_queue, Before, 0),
        avg_run_queue => maps:get(avg_run_queue, After, 0.0) -
                         maps:get(avg_run_queue, Before, 0.0)
    }.

%%====================================================================
%% API - Pressure Point Detection
%%====================================================================

%% @doc Detect pressure points from pressure result.
-spec detect_pressure_points(pressure_result()) -> [pressure_point()].
detect_pressure_points(PressureResult) ->
    Points = [],
    %% Check memory pressure points
    Points1 = check_memory_pressure(PressureResult, Points),
    %% Check scheduler pressure points
    Points2 = check_scheduler_pressure(PressureResult, Points1),
    %% Check run queue pressure
    Points3 = check_run_queue_pressure(PressureResult, Points2),
    %% Check for memory leaks
    check_memory_leaks(PressureResult, Points3).

%% @doc Format pressure report for human reading.
-spec format_pressure_report(pressure_result() | [pressure_point()]) -> iolist().
format_pressure_report(#{memory := Mem, scheduler := Sched} = Pressure) ->
    [
        "=== Pressure Report ===~n",
        format_memory_report(Mem),
        "~n",
        format_scheduler_report(Sched),
        "~n",
        format_pressure_points(detect_pressure_points(Pressure))
    ];
format_pressure_report(Points) when is_list(Points) ->
    format_pressure_points(Points).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Take a memory snapshot.
take_memory_snapshot() ->
    Memory0 = erlang:memory(),
    %% Convert proplist to map if needed
    Memory = case is_list(Memory0) of
        true -> maps:from_list(Memory0);
        false -> Memory0
    end,
    #{
        timestamp => erlang:monotonic_time(millisecond),
        total => maps:get(total, Memory, 0),
        processes => maps:get(processes, Memory, 0),
        processes_used => maps:get(processes_used, Memory, 0),
        system => maps:get(system, Memory, 0),
        atom => maps:get(atom, Memory, 0),
        binary => maps:get(binary, Memory, 0),
        ets => maps:get(ets, Memory, 0),
        code => maps:get(code, Memory, 0)
    }.

%% @private Take a scheduler snapshot.
take_scheduler_snapshot() ->
    SchedulerCount = erlang:system_info(schedulers),
    SchedulerWallTime = case erlang:statistics(scheduler_wall_time) of
        undefined -> [];
        Data -> Data
    end,

    %% Calculate per-scheduler utilization
    Utilization = calculate_scheduler_utilization(SchedulerWallTime, SchedulerCount),

    %% Get run queue lengths - OTP 28 returns a list directly
    AllQueueLengths = erlang:statistics(run_queue_lengths),
    %% Take only SchedulerCount elements or pad if needed
    QueueLengths = case length(AllQueueLengths) of
        N when N >= SchedulerCount ->
            lists:sublist(AllQueueLengths, SchedulerCount);
        N ->
            AllQueueLengths ++ lists:duplicate(SchedulerCount - N, 0)
    end,
    RunQueues = lists:zip(lists:seq(1, SchedulerCount), QueueLengths),
    MaxQueue = case QueueLengths of
        [] -> 0;
        _ -> lists:max(QueueLengths)
    end,
    AvgQueue = case QueueLengths of
        [] -> 0.0;
        _ -> lists:sum(QueueLengths) / length(QueueLengths)
    end,

    %% Calculate IO vs CPU breakdown (approximate)
    {IOUtil, CPUUtil} = case SchedulerWallTime of
        [] -> {0.0, 0.0};
        WallTimes ->
            TotalActive = lists:sum([Active || {_, Active, _} <- WallTimes]),
            TotalIO = lists:sum([IO || {_, _, IO} <- WallTimes]),
            Total = TotalActive + TotalIO,
            case Total > 0 of
                true -> {TotalIO / Total * 100, TotalActive / Total * 100};
                false -> {0.0, 0.0}
            end
    end,

    #{
        timestamp => erlang:monotonic_time(millisecond),
        scheduler_count => SchedulerCount,
        scheduler_utilization => Utilization,
        total_utilization => calculate_total_utilization(Utilization),
        io_utilization => IOUtil,
        cpu_utilization => CPUUtil,
        run_queue_lengths => RunQueues,
        max_run_queue => MaxQueue,
        avg_run_queue => AvgQueue
    }.

%% @private Calculate scheduler utilization from wall time data.
calculate_scheduler_utilization([], _Count) -> [];
calculate_scheduler_utilization(WallTime, Count) ->
    %% Sum all active and IO time
    TotalActive = lists:sum([A || {_, A, _} <- WallTime]),
    TotalIO = lists:sum([IO || {_, _, IO} <- WallTime]),
    TotalAll = TotalActive + TotalIO,

    %% Per-scheduler utilization
    case TotalAll > 0 of
        true ->
            [{Id, (Active + IO) / TotalAll * 100} || {Id, Active, IO} <- WallTime];
        false ->
            [{Id, 0.0} || Id <- lists:seq(1, Count)]
    end.

%% @private Calculate total utilization from per-scheduler list.
calculate_total_utilization([]) -> 0.0;
calculate_total_utilization(UtilList) when is_list(UtilList) ->
    case UtilList of
        [] -> 0.0;
        _ -> lists:sum([U || {_, U} <- UtilList]) / length(UtilList)
    end;
calculate_total_utilization(#{total_utilization := Util}) -> Util;
calculate_total_utilization(_) -> 0.0.

%% @private Calculate memory difference.
calculate_memory_diff(Before, After) ->
    maps:map(fun(_K, VAfter) ->
        VBefore = maps:get(_K, Before, 0),
        VAfter - VBefore
    end, After).

%% @private Check for memory pressure points.
check_memory_pressure(#{memory := #{'after' := Mem}}, Acc) when is_map(Mem), map_size(Mem) > 0 ->
    Total = maps:get(total, Mem, 0),
    Binary = maps:get(binary, Mem, 0),
    ETS = maps:get(ets, Mem, 0),

    %% Thresholds (in bytes)
    BinaryThreshold = 100 * 1024 * 1024,  %% 100 MB
    ETSThreshold = 50 * 1024 * 1024,       %% 50 MB
    TotalThreshold = 1024 * 1024 * 1024,   %% 1 GB

    lists:flatten([
        case Binary > BinaryThreshold of
            true ->
                [#{
                    type => binary_leak,
                    severity => severity(Binary, BinaryThreshold, 3),
                    description => <<"High binary memory usage">>,
                    metric => <<"binary">>,
                    value => Binary,
                    threshold => BinaryThreshold
                }];
            false -> []
        end,
        case ETS > ETSThreshold of
            true ->
                [#{
                    type => memory,
                    severity => severity(ETS, ETSThreshold, 2),
                    description => <<"High ETS memory usage">>,
                    metric => <<"ets">>,
                    value => ETS,
                    threshold => ETSThreshold
                }];
            false -> []
        end,
        case Total > TotalThreshold of
            true ->
                [#{
                    type => memory,
                    severity => severity(Total, TotalThreshold, 2),
                    description => <<"High total memory usage">>,
                    metric => <<"total">>,
                    value => Total,
                    threshold => TotalThreshold
                }];
            false -> []
        end,
        Acc
    ]);
check_memory_pressure(_, Acc) ->
    Acc.

%% @private Check for scheduler pressure points.
check_scheduler_pressure(#{scheduler := #{peak_utilization := Peak}}, Acc) ->
    HighThreshold = 80.0,
    CriticalThreshold = 95.0,

    lists:flatten([
        case Peak >= CriticalThreshold of
            true ->
                [#{
                    type => scheduler,
                    severity => critical,
                    description => <<"Scheduler at critical utilization">>,
                    metric => <<"peak_utilization">>,
                    value => Peak,
                    threshold => CriticalThreshold
                }];
            false -> []
        end,
        case Peak >= HighThreshold andalso Peak < CriticalThreshold of
            true ->
                [#{
                    type => scheduler,
                    severity => high,
                    description => <<"Scheduler at high utilization">>,
                    metric => <<"peak_utilization">>,
                    value => Peak,
                    threshold => HighThreshold
                }];
            false -> []
        end,
        Acc
    ]);
check_scheduler_pressure(_, Acc) ->
    Acc.

%% @private Check for run queue pressure.
check_run_queue_pressure(#{scheduler := #{'after' := Sched}}, Acc) when is_map(Sched), map_size(Sched) > 0 ->
    MaxQueue = maps:get(max_run_queue, Sched, 0),
    AvgQueue = maps:get(avg_run_queue, Sched, 0.0),

    MaxQueueThreshold = 100,
    AvgQueueThreshold = 20,

    lists:flatten([
        case MaxQueue > MaxQueueThreshold of
            true ->
                [#{
                    type => run_queue,
                    severity => severity(MaxQueue, MaxQueueThreshold, 2),
                    description => <<"Excessive run queue buildup">>,
                    metric => <<"max_run_queue">>,
                    value => MaxQueue,
                    threshold => MaxQueueThreshold
                }];
            false -> []
        end,
        case AvgQueue > AvgQueueThreshold of
            true ->
                [#{
                    type => run_queue,
                    severity => medium,
                    description => <<"High average run queue">>,
                    metric => <<"avg_run_queue">>,
                    value => AvgQueue,
                    threshold => AvgQueueThreshold
                }];
            false -> []
        end,
        Acc
    ]);
check_run_queue_pressure(_, Acc) ->
    Acc.

%% @private Check for memory leaks (rapid growth).
check_memory_leaks(#{memory := #{diff := Diff}}, Acc) ->
    TotalDiff = maps:get(total, Diff, 0),
    BinaryDiff = maps:get(binary, Diff, 0),

    %% Thresholds for significant growth (bytes)
    GrowthThreshold = 10 * 1024 * 1024,  %% 10 MB growth

    lists:flatten([
        case TotalDiff > GrowthThreshold of
            true ->
                [#{
                    type => process_leak,
                    severity => medium,
                    description => <<"Significant memory growth detected">>,
                    metric => <<"total_growth">>,
                    value => TotalDiff,
                    threshold => GrowthThreshold
                }];
            false -> []
        end,
        case BinaryDiff > GrowthThreshold of
            true ->
                [#{
                    type => binary_leak,
                    severity => high,
                    description => <<"Potential binary memory leak">>,
                    metric => <<"binary_growth">>,
                    value => BinaryDiff,
                    threshold => GrowthThreshold
                }];
            false -> []
        end,
        Acc
    ]);
check_memory_leaks(_, Acc) -> Acc.

%% @private Calculate severity based on value vs threshold.
severity(Value, Threshold, Multiplier) ->
    Ratio = Value / Threshold,
    if
        Ratio >= Multiplier -> critical;
        Ratio >= Multiplier * 0.75 -> high;
        Ratio >= Multiplier * 0.5 -> medium;
        true -> low
    end.

%% @private Format memory report.
format_memory_report(#{before := Before, 'after' := After, diff := Diff}) when is_map(Before), is_map(After), is_map(Diff) ->
    [
        "Memory Report:~n",
        "  Before: ", format_bytes(maps:get(total, Before, 0)), "~n",
        "  After:  ", format_bytes(maps:get(total, After, 0)), "~n",
        "  Growth: ", format_bytes(maps:get(total, Diff, 0)), "~n",
        "  Binary: ", format_bytes(maps:get(binary, Diff, 0)), " growth~n",
        "  ETS:    ", format_bytes(maps:get(ets, Diff, 0)), " growth~n"
    ];
format_memory_report(_) ->
    [
        "Memory Report:~n",
        "  No data available.~n"
    ].

%% @private Format scheduler report.
format_scheduler_report(#{'after' := After,
                          peak_utilization := Peak, avg_utilization := Avg}) when is_map(After) ->
    [
        "Scheduler Report:~n",
        "  Peak utilization: ", fmt_float(Peak), "%~n",
        "  Avg utilization:  ", fmt_float(Avg), "%~n",
        "  Max run queue:    ", integer_to_list(maps:get(max_run_queue, After, 0)), "~n",
        "  Avg run queue:    ", fmt_float(maps:get(avg_run_queue, After, 0.0)), "~n"
    ];
format_scheduler_report(_) ->
    [
        "Scheduler Report:~n",
        "  No data available.~n"
    ].

%% @private Format pressure points list.
format_pressure_points([]) ->
    ["No pressure points detected.~n"];
format_pressure_points(Points) ->
    [
        "Pressure Points Detected:~n",
        [format_pressure_point(P) || P <- lists:sort(
            fun(A, B) -> severity_rank(maps:get(severity, A, low)) >=
                         severity_rank(maps:get(severity, B, low)) end, Points)]
    ].

%% @private Format single pressure point.
format_pressure_point(#{severity := Severity,
                        description := Desc, metric := Metric,
                        value := Value, threshold := Threshold}) ->
    [
        "  [", atom_to_list(Severity), "] ",
        binary_to_list(Desc), "~n",
        "    Metric: ", binary_to_list(Metric), "~n",
        "    Value: ", format_value(Value), " (threshold: ", format_value(Threshold), ")~n"
    ].

%% @private Format bytes.
format_bytes(Bytes) when Bytes >= 1024 * 1024 * 1024 ->
    io_lib:format("~.2f GB", [Bytes / (1024 * 1024 * 1024)]);
format_bytes(Bytes) when Bytes >= 1024 * 1024 ->
    io_lib:format("~.2f MB", [Bytes / (1024 * 1024)]);
format_bytes(Bytes) when Bytes >= 1024 ->
    io_lib:format("~.2f KB", [Bytes / 1024]);
format_bytes(Bytes) ->
    integer_to_list(Bytes) ++ " B".

%% @private Format value (bytes or float).
format_value(V) when is_integer(V) -> format_bytes(V);
format_value(V) when is_float(V) -> io_lib:format("~.2f", [V]);
format_value(V) -> io_lib:format("~p", [V]).

%% @private Format float with precision.
fmt_float(Float) when is_float(Float) ->
    io_lib:format("~.2f", [Float]);
fmt_float(Int) when is_integer(Int) ->
    integer_to_list(Int).

%% @private Get severity rank for sorting.
severity_rank(critical) -> 4;
severity_rank(high) -> 3;
severity_rank(medium) -> 2;
severity_rank(low) -> 1.
