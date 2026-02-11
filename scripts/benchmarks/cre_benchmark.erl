#!/usr/bin/env escript
%%! -pa ../../_build/default/lib/*/ebin
%%
%% CRE Performance Benchmark Suite
%%
%% Comprehensive benchmarks for:
%%   - Workflow execution throughput
%%   - Task execution latency (p50, p95, p99)
%%   - Petri net operations
%%   - Memory usage under load
%%   - Scalability testing
%%

-mode(compile).

-define(WARMUP_ROUNDS, 5).
-define(LATENCY_SAMPLES, 10000).
-define(THROUGHPUT_OPS, 100).

%%%===================================================================
%%% Main Entry Point
%%%===================================================================

main(Args) ->
    io:format("~n"),
    io:format("╔════════════════════════════════════════════════════════════════╗~n"),
    io:format("║           CRE Performance Benchmark Suite v0.3.0              ║~n"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),

    %% Parse arguments
    BenchType = case Args of
        ["all"] -> all;
        ["throughput"] -> throughput;
        ["latency"] -> latency;
        ["memory"] -> memory;
        ["pnet"] -> pnet;
        ["scalability"] -> scalability;
        [] -> all;
        _ ->
            io:format("Usage: cre_benchmark.erl [all|throughput|latency|memory|pnet|scalability]~n"),
            halt(1)
    end,

    %% Ensure required modules are loaded
    ensure_modules_loaded(),

    %% Print system information
    print_system_info(),

    %% Run requested benchmarks
    case BenchType of
        all ->
            run_all_benchmarks();
        throughput ->
            benchmark_throughput();
        latency ->
            benchmark_latency();
        memory ->
            benchmark_memory();
        pnet ->
            benchmark_pnet_operations();
        scalability ->
            benchmark_scalability()
    end,

    io:format("~n"),
    io:format("Benchmark complete!~n"),
    io:format("~n"),

    halt(0).

%%%===================================================================
%%% Module Loading
%%%===================================================================

ensure_modules_loaded() ->
    Modules = [
        gen_pnet,
        wf_test_net_basic,
        wf_test_net_choice,
        wf_test_net_task_gate,
        pnet_marking,
        pnet_choice,
        pnet_types,
        pnet_receipt
    ],

    io:format("Loading required modules...~n"),
    lists:foreach(fun(Mod) ->
        case code:ensure_loaded(Mod) of
            {module, _} ->
                io:format("  ✓ ~p~n", [Mod]);
            {error, Reason} ->
                io:format("  ✗ ~p (error: ~p)~n", [Mod, Reason]),
                io:format("~nERROR: Failed to load required module: ~p~n", [Mod]),
                io:format("Please run: rebar3 compile~n~n"),
                halt(1)
        end
    end, Modules),
    io:format("~n").

%%%===================================================================
%%% System Information
%%%===================================================================

print_system_info() ->
    io:format("╔════════════════════════════════════════════════════════════════╗~n"),
    io:format("║ System Information                                             ║~n"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),

    io:format("Timestamp:        ~s~n", [format_timestamp()]),
    io:format("Erlang/OTP:       ~s~n", [erlang:system_info(otp_release)]),
    io:format("ERTS Version:     ~s~n", [erlang:system_info(version)]),
    io:format("Schedulers:       ~p~n", [erlang:system_info(schedulers)]),
    io:format("Logical CPUs:     ~p~n", [erlang:system_info(logical_processors)]),

    {TotalMem, AllocMem, _} = memsup:get_memory_data(),
    io:format("System Memory:    ~.2f GB total, ~.2f GB used~n",
              [TotalMem / 1073741824, AllocMem / 1073741824]),

    io:format("~n").

format_timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Y, M, D, H, Min, S]).

%%%===================================================================
%%% Run All Benchmarks
%%%===================================================================

run_all_benchmarks() ->
    benchmark_throughput(),
    benchmark_latency(),
    benchmark_memory(),
    benchmark_pnet_operations(),
    benchmark_scalability().

%%%===================================================================
%%% Throughput Benchmark
%%%===================================================================

benchmark_throughput() ->
    print_section("Workflow Throughput Benchmark"),

    io:format("Measuring workflows per second at different concurrency levels~n~n"),

    ConcurrencyLevels = [1, 10, 100, 1000],

    Results = lists:map(fun(N) ->
        bench_concurrent_workflows(N, ?THROUGHPUT_OPS)
    end, ConcurrencyLevels),

    io:format("~n"),
    io:format("Summary:~n"),
    io:format("~-12s ~-10s ~-15s ~-15s~n",
              ["Concurrency", "Time (s)", "Throughput", "Workflows/s"]),
    io:format("~s~n", [lists:duplicate(60, $-)]),

    lists:foreach(fun({N, Time, Throughput, WfPerSec}) ->
        io:format("~-12w ~-10.3f ~-15.2f ~-15.2f~n",
                  [N, Time, Throughput, WfPerSec])
    end, Results),

    io:format("~n").

bench_concurrent_workflows(NumWorkflows, OpsPerWorkflow) ->
    io:format("Testing ~p concurrent workflows (~p ops each)...~n",
              [NumWorkflows, OpsPerWorkflow]),

    %% Start workflows
    StartTime = erlang:monotonic_time(microsecond),

    Pids = lists:filtermap(fun(N) ->
        case gen_pnet:start_link(wf_test_net_basic, #{seed => N}, []) of
            {ok, Pid} -> {true, Pid};
            _ -> false
        end
    end, lists:seq(1, NumWorkflows)),

    NumStarted = length(Pids),

    if
        NumStarted == 0 ->
            io:format("  ERROR: Could not start any workflows~n"),
            {NumWorkflows, 0.0, 0.0, 0.0};
        true ->
            io:format("  Started ~p workflows~n", [NumStarted]),

            %% Execute operations in parallel
            Parent = self(),
            Workers = lists:map(fun(Pid) ->
                spawn_link(fun() ->
                    lists:foreach(fun(_) ->
                        try
                            gen_pnet:inject(Pid, #{p => [token]}),
                            gen_pnet:drain(Pid, 10)
                        catch
                            _:_ -> ok
                        end
                    end, lists:seq(1, OpsPerWorkflow)),
                    Parent ! {done, self()}
                end)
            end, Pids),

            %% Wait for all workers to complete
            lists:foreach(fun(Worker) ->
                receive
                    {done, Worker} -> ok
                after 60000 ->
                    io:format("  WARNING: Worker timeout~n")
                end
            end, Workers),

            EndTime = erlang:monotonic_time(microsecond),
            TotalTimeUs = EndTime - StartTime,
            TotalTimeSec = TotalTimeUs / 1000000,

            TotalOps = NumStarted * OpsPerWorkflow,
            Throughput = TotalOps / TotalTimeSec,
            WfPerSec = NumStarted / TotalTimeSec,

            io:format("  Time: ~.3f seconds~n", [TotalTimeSec]),
            io:format("  Throughput: ~.2f ops/sec~n", [Throughput]),
            io:format("  Workflows/sec: ~.2f~n~n", [WfPerSec]),

            %% Cleanup
            lists:foreach(fun(Pid) ->
                try gen_pnet:stop(Pid) catch _:_ -> ok end
            end, Pids),

            {NumWorkflows, TotalTimeSec, Throughput, WfPerSec}
    end.

%%%===================================================================
%%% Latency Benchmark
%%%===================================================================

benchmark_latency() ->
    print_section("Task Execution Latency Benchmark"),

    io:format("Collecting ~p latency samples...~n~n", [?LATENCY_SAMPLES]),

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    %% Warmup
    io:format("Warming up (~p rounds)...~n", [?WARMUP_ROUNDS]),
    lists:foreach(fun(_) ->
        try
            gen_pnet:inject(P, #{p => [token]}),
            gen_pnet:drain(P, 10)
        catch
            _:_ -> ok
        end
    end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Collect samples
    io:format("Collecting samples...~n"),
    Latencies = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),
        try
            gen_pnet:inject(P, #{p => [token]}),
            gen_pnet:drain(P, 10)
        catch
            _:_ -> ok
        end,
        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, ?LATENCY_SAMPLES)),

    gen_pnet:stop(P),

    %% Calculate statistics
    Sorted = lists:sort(Latencies),
    Stats = calculate_percentiles(Sorted),

    %% Print results
    io:format("~n"),
    io:format("Latency Statistics (microseconds):~n"),
    io:format("~s~n", [lists:duplicate(60, $-)]),
    io:format("  Min:    ~10w μs~n", [maps:get(min, Stats)]),
    io:format("  Mean:   ~10.2f μs~n", [maps:get(mean, Stats)]),
    io:format("  Median: ~10w μs~n", [maps:get(p50, Stats)]),
    io:format("  p95:    ~10w μs~n", [maps:get(p95, Stats)]),
    io:format("  p99:    ~10w μs~n", [maps:get(p99, Stats)]),
    io:format("  p999:   ~10w μs~n", [maps:get(p999, Stats)]),
    io:format("  Max:    ~10w μs~n", [maps:get(max, Stats)]),
    io:format("~n"),
    io:format("  Throughput: ~.2f ops/sec (based on mean)~n",
              [1000000 / maps:get(mean, Stats)]),
    io:format("~n").

calculate_percentiles(Sorted) ->
    N = length(Sorted),

    P50_idx = max(1, round(N * 0.50)),
    P95_idx = max(1, round(N * 0.95)),
    P99_idx = max(1, round(N * 0.99)),
    P999_idx = max(1, round(N * 0.999)),

    #{
        min => lists:min(Sorted),
        max => lists:max(Sorted),
        mean => lists:sum(Sorted) / N,
        p50 => lists:nth(P50_idx, Sorted),
        p95 => lists:nth(P95_idx, Sorted),
        p99 => lists:nth(P99_idx, Sorted),
        p999 => lists:nth(P999_idx, Sorted)
    }.

%%%===================================================================
%%% Memory Benchmark
%%%===================================================================

benchmark_memory() ->
    print_section("Memory Usage Benchmark"),

    io:format("Measuring memory consumption patterns~n~n"),

    %% Baseline
    garbage_collect(),
    timer:sleep(100),
    Baseline = erlang:memory(total),
    io:format("Baseline memory: ~.2f MB~n", [Baseline / 1048576]),

    %% Single workflow instance
    {ok, P1} = gen_pnet:start_link(wf_test_net_basic, #{}, []),
    timer:sleep(100),
    SingleInstance = erlang:memory(total) - Baseline,
    io:format("Single instance: ~.2f KB~n", [SingleInstance / 1024]),
    gen_pnet:stop(P1),

    garbage_collect(),
    timer:sleep(100),

    %% Multiple instances
    NumInstances = 100,
    io:format("~nStarting ~p workflow instances...~n", [NumInstances]),

    BeforeMulti = erlang:memory(total),

    Pids = lists:filtermap(fun(N) ->
        case gen_pnet:start_link(wf_test_net_basic, #{seed => N}, []) of
            {ok, Pid} -> {true, Pid};
            _ -> false
        end
    end, lists:seq(1, NumInstances)),

    timer:sleep(200),
    AfterMulti = erlang:memory(total),
    MultiMemory = AfterMulti - BeforeMulti,
    PerInstance = if
        length(Pids) > 0 -> MultiMemory / length(Pids);
        true -> 0
    end,

    io:format("Memory for ~p instances: ~.2f MB~n",
              [length(Pids), MultiMemory / 1048576]),
    io:format("Memory per instance: ~.2f KB~n", [PerInstance / 1024]),

    %% Cleanup
    lists:foreach(fun(Pid) ->
        try gen_pnet:stop(Pid) catch _:_ -> ok end
    end, Pids),

    garbage_collect(),
    timer:sleep(100),

    %% Memory under load
    io:format("~nMemory under load test (1000 operations)...~n"),
    {ok, P2} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    BeforeLoad = erlang:memory(total),

    lists:foreach(fun(_) ->
        try
            gen_pnet:inject(P2, #{p => [token]}),
            gen_pnet:drain(P2, 10)
        catch
            _:_ -> ok
        end
    end, lists:seq(1, 1000)),

    AfterLoad = erlang:memory(total),
    LoadMemory = AfterLoad - BeforeLoad,

    io:format("Memory growth: ~.2f KB~n", [LoadMemory / 1024]),
    io:format("Per-operation overhead: ~.2f bytes~n", [LoadMemory / 1000]),

    gen_pnet:stop(P2),

    io:format("~n").

%%%===================================================================
%%% Petri Net Operations Benchmark
%%%===================================================================

benchmark_pnet_operations() ->
    print_section("Petri Net Operations Benchmark"),

    io:format("Benchmarking core Petri net primitives~n~n"),

    %% Marking operations
    bench_marking_hash(),
    bench_marking_merge(),

    %% Choice operations
    bench_choice_select(),

    io:format("~n").

bench_marking_hash() ->
    io:format("Marking Hash Operations:~n"),

    %% Create test markings of different sizes
    Sizes = [10, 50, 100, 500],

    lists:foreach(fun(NumPlaces) ->
        Marking = maps:from_list([
            {list_to_atom("p" ++ integer_to_list(N)), lists:seq(1, 10)}
            || N <- lists:seq(1, NumPlaces)
        ]),

        Iterations = 10000,
        StartTime = erlang:monotonic_time(microsecond),

        lists:foreach(fun(_) ->
            pnet_marking:hash(Marking)
        end, lists:seq(1, Iterations)),

        EndTime = erlang:monotonic_time(microsecond),
        TimePerOp = (EndTime - StartTime) / Iterations,

        io:format("  ~4w places: ~.2f μs/op (~.2f ops/sec)~n",
                  [NumPlaces, TimePerOp, 1000000 / TimePerOp])
    end, Sizes),

    io:format("~n").

bench_marking_merge() ->
    io:format("Marking Merge Operations:~n"),

    M1 = #{p1 => [a, b], p2 => [c]},
    M2 = #{p2 => [d], p3 => [e, f]},

    Iterations = 100000,
    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
        pnet_marking:merge(M1, M2)
    end, lists:seq(1, Iterations)),

    EndTime = erlang:monotonic_time(microsecond),
    TimePerOp = (EndTime - StartTime) / Iterations,

    io:format("  Merge: ~.2f μs/op (~.2f ops/sec)~n",
              [TimePerOp, 1000000 / TimePerOp]),

    io:format("~n").

bench_choice_select() ->
    io:format("Choice Selection Operations:~n"),

    ChoiceSizes = [1, 5, 10, 50],

    lists:foreach(fun(NumChoices) ->
        Choices = [#{mode => #{p1 => [a]}, produce => #{p2 => [b]}}
                   || _ <- lists:seq(1, NumChoices)],

        Iterations = 100000,
        StartTime = erlang:monotonic_time(microsecond),

        lists:foreach(fun(_) ->
            pnet_choice:select(Choices, 42)
        end, lists:seq(1, Iterations)),

        EndTime = erlang:monotonic_time(microsecond),
        TimePerOp = (EndTime - StartTime) / Iterations,

        io:format("  ~4w choices: ~.2f μs/op (~.2f ops/sec)~n",
                  [NumChoices, TimePerOp, 1000000 / TimePerOp])
    end, ChoiceSizes),

    io:format("~n").

%%%===================================================================
%%% Scalability Benchmark
%%%===================================================================

benchmark_scalability() ->
    print_section("Scalability Benchmark"),

    io:format("Testing scalability with increasing load~n~n"),

    %% Test workflow scalability
    io:format("Workflow Scalability:~n"),
    WorkflowCounts = [1, 10, 100, 1000],

    lists:foreach(fun(N) ->
        io:format("  Testing ~p concurrent workflows...~n", [N]),

        StartTime = erlang:monotonic_time(microsecond),

        Pids = lists:filtermap(fun(I) ->
            case gen_pnet:start_link(wf_test_net_basic, #{seed => I}, []) of
                {ok, Pid} -> {true, Pid};
                _ -> false
            end
        end, lists:seq(1, N)),

        EndTime = erlang:monotonic_time(microsecond),
        StartupTime = (EndTime - StartTime) / 1000,

        io:format("    Started ~p workflows in ~.2f ms~n",
                  [length(Pids), StartupTime]),
        io:format("    Startup rate: ~.2f workflows/sec~n",
                  [length(Pids) / (StartupTime / 1000)]),

        %% Cleanup
        lists:foreach(fun(Pid) ->
            try gen_pnet:stop(Pid) catch _:_ -> ok end
        end, Pids),

        timer:sleep(100)
    end, WorkflowCounts),

    io:format("~n").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

print_section(Title) ->
    io:format("~n"),
    io:format("╔════════════════════════════════════════════════════════════════╗~n"),
    TitlePadded = string:centre(Title, 62),
    io:format("║ ~s ║~n", [TitlePadded]),
    io:format("╚════════════════════════════════════════════════════════════════╝~n"),
    io:format("~n").
