%% Load Test Harness
%% Tests 10K+ concurrent workflows for certification
-module(f5_load_tester).
-export([run_test/1, generate_report/1]).

-record(load_test_config, {
    concurrent_workflows = 10000 :: integer(),
    duration_seconds = 86400 :: integer(),  %% 24 hours
    ramp_up_rate = 1000 :: integer(),  %% workflows/minute
    workflow_types :: [atom()]
}).

run_test(Config) ->
    io:format("~n=== LOAD TEST STARTING ===~n"),
    io:format("Target: ~p concurrent workflows~n", [Config#load_test_config.concurrent_workflows]),
    io:format("Duration: ~p seconds (~.1f hours)~n",
              [Config#load_test_config.duration_seconds,
               Config#load_test_config.duration_seconds / 3600]),

    %% Create results directory
    filelib:ensure_dir("evidence/load_tests/"),

    %% Start collectors
    {ok, LatencyCollector} = start_latency_collector(),
    {ok, ThroughputCollector} = start_throughput_collector(),
    {ok, ResourceCollector} = start_resource_collector(),

    %% Ramp up to target concurrency
    ramp_up(Config),

    %% Sustain load
    sustain_load(Config),

    %% Collect results
    Results = collect_results(LatencyCollector, ThroughputCollector, ResourceCollector),

    %% Generate report
    generate_report(Results),

    io:format("~n=== LOAD TEST COMPLETE ===~n"),
    {ok, Results}.

ramp_up(Config) ->
    Target = Config#load_test_config.concurrent_workflows,
    Rate = Config#load_test_config.ramp_up_rate,

    io:format("Ramping up at ~p workflows/minute...~n", [Rate]),

    NumSteps = Target div Rate,
    lists:foreach(fun(Step) ->
        spawn_workflows(Rate, Config#load_test_config.workflow_types),
        timer:sleep(60000),  %% 1 minute
        Current = Step * Rate,
        io:format("  Progress: ~p/~p workflows (~.1f%)~n",
                  [Current, Target, (Current/Target)*100])
    end, lists:seq(1, NumSteps)).

sustain_load(Config) ->
    Duration = Config#load_test_config.duration_seconds,
    io:format("Sustaining load for ~.1f hours...~n", [Duration / 3600]),

    %% Keep workflows running by spawning new ones as old ones complete
    Interval = 1000,  %% Check every second
    NumIntervals = Duration div (Interval div 1000),

    lists:foreach(fun(N) ->
        timer:sleep(Interval),
        if N rem 3600 == 0 ->
            Hours = N div 3600,
            io:format("  Sustained for ~p hours...~n", [Hours]);
           true -> ok
        end,

        %% Replace completed workflows
        replace_completed_workflows(Config)
    end, lists:seq(1, NumIntervals)).

spawn_workflows(Count, WorkflowTypes) ->
    lists:foreach(fun(_) ->
        WorkflowType = lists:nth(rand:uniform(length(WorkflowTypes)), WorkflowTypes),
        spawn(fun() -> execute_workflow(WorkflowType) end)
    end, lists:seq(1, Count)).

execute_workflow(WorkflowType) ->
    %% Simulate workflow execution
    StartTime = erlang:monotonic_time(microsecond),

    %% Do actual work here
    Result = case WorkflowType of
        crm_operation -> simulate_crm_workflow();
        kyc_operation -> simulate_kyc_workflow();
        _ -> simulate_generic_workflow()
    end,

    EndTime = erlang:monotonic_time(microsecond),
    Latency = EndTime - StartTime,

    %% Report to collectors
    f5_latency_collector:record(Latency),
    f5_throughput_collector:increment(),

    Result.

simulate_crm_workflow() ->
    %% Simulate CRM operations
    timer:sleep(rand:uniform(100)),
    {ok, completed}.

simulate_kyc_workflow() ->
    %% Simulate KYC operations
    timer:sleep(rand:uniform(200)),
    {ok, completed}.

simulate_generic_workflow() ->
    timer:sleep(rand:uniform(50)),
    {ok, completed}.

replace_completed_workflows(Config) ->
    %% Check how many workflows are still running
    CurrentCount = length(erlang:processes()) - 100,  %% Subtract system processes
    Target = Config#load_test_config.concurrent_workflows,

    if CurrentCount < Target ->
        Deficit = Target - CurrentCount,
        spawn_workflows(Deficit, Config#load_test_config.workflow_types);
       true -> ok
    end.

start_latency_collector() ->
    %% Start process that collects latency measurements
    {ok, spawn(fun latency_collector_loop/0)}.

latency_collector_loop() ->
    latency_collector_loop([]).

latency_collector_loop(Latencies) ->
    receive
        {record, Latency} ->
            latency_collector_loop([Latency | Latencies]);
        {get_results, From} ->
            From ! {latencies, Latencies},
            latency_collector_loop(Latencies)
    end.

start_throughput_collector() ->
    {ok, spawn(fun throughput_collector_loop/0)}.

throughput_collector_loop() ->
    throughput_collector_loop(0).

throughput_collector_loop(Count) ->
    receive
        increment ->
            throughput_collector_loop(Count + 1);
        {get_results, From} ->
            From ! {throughput, Count},
            throughput_collector_loop(Count)
    end.

start_resource_collector() ->
    {ok, spawn(fun resource_collector_loop/0)}.

resource_collector_loop() ->
    %% Collect CPU, memory, etc every second
    timer:sleep(1000),
    Sample = #{
        timestamp => erlang:system_time(second),
        memory => erlang:memory(),
        process_count => erlang:system_info(process_count),
        schedulers_online => erlang:system_info(schedulers_online)
    },
    resource_collector_loop([Sample]).

resource_collector_loop(Samples) ->
    receive
        {get_results, From} ->
            From ! {resources, Samples},
            resource_collector_loop(Samples)
    after 1000 ->
        Sample = #{
            timestamp => erlang:system_time(second),
            memory => erlang:memory(),
            process_count => erlang:system_info(process_count)
        },
        resource_collector_loop([Sample | Samples])
    end.

collect_results(LatencyCollector, ThroughputCollector, ResourceCollector) ->
    LatencyCollector ! {get_results, self()},
    ThroughputCollector ! {get_results, self()},
    ResourceCollector ! {get_results, self()},

    Latencies = receive {latencies, L} -> L end,
    Throughput = receive {throughput, T} -> T end,
    Resources = receive {resources, R} -> R end,

    #{
        latencies => Latencies,
        throughput => Throughput,
        resources => Resources
    }.

generate_report(Results) ->
    Latencies = maps:get(latencies, Results),
    Sorted = lists:sort(Latencies),

    P50 = percentile(Sorted, 50),
    P95 = percentile(Sorted, 95),
    P99 = percentile(Sorted, 99),
    P99_9 = percentile(Sorted, 99.9),
    P99_99 = percentile(Sorted, 99.99),

    Report = #{
        timestamp => calendar:system_time_to_rfc3339(erlang:system_time(second)),
        total_workflows => length(Latencies),
        throughput_per_second => maps:get(throughput, Results) / 86400,
        latency_percentiles => #{
            p50 => P50,
            p95 => P95,
            p99 => P99,
            p99_9 => P99_9,
            p99_99 => P99_99
        },
        resource_usage => analyze_resources(maps:get(resources, Results))
    },

    %% Write report
    ReportJson = jsx:encode(Report),
    file:write_file("evidence/load_tests/10k_concurrent_test.json", ReportJson),

    io:format("~n=== LOAD TEST REPORT ===~n"),
    io:format("Total workflows: ~p~n", [length(Latencies)]),
    io:format("Throughput: ~.2f workflows/second~n", [maps:get(throughput, Results) / 86400]),
    io:format("Latency P50: ~.2f ms~n", [P50 / 1000]),
    io:format("Latency P95: ~.2f ms~n", [P95 / 1000]),
    io:format("Latency P99: ~.2f ms~n", [P99 / 1000]),
    io:format("Latency P99.9: ~.2f ms~n", [P99_9 / 1000]),
    io:format("Latency P99.99: ~.2f ms~n", [P99_99 / 1000]),

    {ok, Report}.

percentile([], _P) -> 0;
percentile(SortedList, P) ->
    Index = round((P / 100) * length(SortedList)),
    lists:nth(max(1, Index), SortedList).

analyze_resources(ResourceSamples) ->
    #{
        avg_memory => average([maps:get(memory, S) || S <- ResourceSamples]),
        avg_process_count => average([maps:get(process_count, S) || S <- ResourceSamples])
    }.

average([]) -> 0;
average(List) -> lists:sum(List) / length(List).
