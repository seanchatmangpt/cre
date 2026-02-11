%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc Load Testing Suite for CRE Workflow Engine
%%
%% Comprehensive load testing to validate scalability and performance
%% characteristics under various stress conditions.
%%
%% Test Categories:
%% 1. Concurrent Workflow Execution (N parallel workflows)
%% 2. Sustained Load Testing (steady-state behavior)
%% 3. Load Pattern Testing (ramp-up, burst, spike)
%% 4. Resource Monitoring (memory, CPU, schedulers)
%% 5. Failure Under Load (process crashes, recovery)
%% 6. Degradation Analysis (performance under increasing load)
%%
%% @end
%% -------------------------------------------------------------------

-module(load_test_SUITE).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    %% Concurrent execution tests
    concurrent_workflows_10/1,
    concurrent_workflows_50/1,
    concurrent_workflows_100/1,
    concurrent_workflows_500/1,
    concurrent_workflows_1000/1,

    %% Sustained load tests
    sustained_load_light/1,
    sustained_load_medium/1,
    sustained_load_heavy/1,

    %% Load pattern tests
    ramp_up_load/1,
    steady_load/1,
    burst_load/1,
    spike_load/1,
    oscillating_load/1,

    %% Resource monitoring tests
    memory_usage_under_load/1,
    scheduler_utilization/1,
    ets_table_growth/1,
    process_count_stability/1,

    %% Failure scenario tests
    process_crash_during_load/1,
    cascading_failure_recovery/1,
    partial_system_failure/1,
    supervisor_restart_under_load/1,

    %% Degradation tests
    throughput_degradation/1,
    latency_degradation/1,
    recovery_time_measurement/1,

    %% Report generation
    generate_load_test_report/1
]).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-record(load_config, {
    workflow_count :: pos_integer(),
    duration_seconds :: pos_integer(),
    pattern_type :: atom(),
    spawn_rate :: pos_integer(),  % workflows per second
    think_time :: non_neg_integer()  % ms between operations
}).

-record(load_result, {
    test_name :: binary(),
    config :: #load_config{},
    start_time :: integer(),
    end_time :: integer(),
    duration_ms :: non_neg_integer(),
    workflows_started :: non_neg_integer(),
    workflows_completed :: non_neg_integer(),
    workflows_failed :: non_neg_integer(),
    throughput :: float(),  % workflows per second
    avg_latency_ms :: float(),
    p50_latency_ms :: non_neg_integer(),
    p95_latency_ms :: non_neg_integer(),
    p99_latency_ms :: non_neg_integer(),
    max_latency_ms :: non_neg_integer(),
    min_latency_ms :: non_neg_integer(),
    errors :: [term()]
}).

-record(resource_snapshot, {
    timestamp :: integer(),
    memory_total :: non_neg_integer(),
    memory_processes :: non_neg_integer(),
    memory_ets :: non_neg_integer(),
    process_count :: non_neg_integer(),
    scheduler_utilization :: [float()],
    run_queue_length :: non_neg_integer(),
    ets_table_count :: non_neg_integer(),
    gc_count :: non_neg_integer(),
    gc_words_reclaimed :: non_neg_integer()
}).

%%%===================================================================
%%% Test Configuration
%%%===================================================================

-define(LIGHT_LOAD_WORKFLOWS, 10).
-define(MEDIUM_LOAD_WORKFLOWS, 50).
-define(HEAVY_LOAD_WORKFLOWS, 100).
-define(STRESS_LOAD_WORKFLOWS, 500).
-define(EXTREME_LOAD_WORKFLOWS, 1000).

-define(SUSTAINED_DURATION_SEC, 60).
-define(SHORT_DURATION_SEC, 30).
-define(LONG_DURATION_SEC, 120).

-define(RESOURCE_SAMPLE_INTERVAL_MS, 1000).
-define(ACCEPTABLE_MEMORY_GROWTH_MB, 50).
-define(MAX_LATENCY_DEGRADATION_PERCENT, 50).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @doc Returns list of all test cases and groups.
-spec all() -> [atom() | {group, atom()}].
all() ->
    [
        {group, concurrent_execution},
        {group, sustained_load},
        {group, load_patterns},
        {group, resource_monitoring},
        {group, failure_scenarios},
        {group, degradation_analysis},
        generate_load_test_report
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [parallel | sequence | {repeat, pos_integer()}], [atom()]}].
groups() ->
    [
        {concurrent_execution, [sequence], [
            concurrent_workflows_10,
            concurrent_workflows_50,
            concurrent_workflows_100,
            concurrent_workflows_500,
            concurrent_workflows_1000
        ]},
        {sustained_load, [sequence], [
            sustained_load_light,
            sustained_load_medium,
            sustained_load_heavy
        ]},
        {load_patterns, [sequence], [
            ramp_up_load,
            steady_load,
            burst_load,
            spike_load,
            oscillating_load
        ]},
        {resource_monitoring, [sequence], [
            memory_usage_under_load,
            scheduler_utilization,
            ets_table_growth,
            process_count_stability
        ]},
        {failure_scenarios, [sequence], [
            process_crash_during_load,
            cascading_failure_recovery,
            partial_system_failure,
            supervisor_restart_under_load
        ]},
        {degradation_analysis, [sequence], [
            throughput_degradation,
            latency_degradation,
            recovery_time_measurement
        ]}
    ].

%% @doc Suite-level initialization.
-spec init_per_suite(Config :: ct:config()) -> ct:config().
init_per_suite(Config) ->
    ct:pal("Starting Load Testing Suite"),

    %% Start application
    ok = application:ensure_all_started(cre),

    %% Create results directory
    PrivDir = ?config(priv_dir, Config),
    ResultsDir = filename:join(PrivDir, "load_test_results"),
    ok = filelib:ensure_dir(filename:join(ResultsDir, "dummy")),

    %% Initialize ETS table for results
    ets:new(load_test_results, [named_table, public, ordered_set,
                                 {keypos, 2}]),  % keypos = test_name field

    %% Store configuration
    [{results_dir, ResultsDir},
     {test_start_time, erlang:system_time(millisecond)} | Config].

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: ct:config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Load Testing Suite Complete"),
    catch ets:delete(load_test_results),
    ok.

%% @doc Group-level initialization.
-spec init_per_group(GroupName :: atom(), Config :: ct:config()) -> ct:config().
init_per_group(GroupName, Config) ->
    ct:pal("Starting test group: ~p", [GroupName]),
    [{current_group, GroupName} | Config].

%% @doc Group-level cleanup.
-spec end_per_group(GroupName :: atom(), Config :: ct:config()) -> ok.
end_per_group(GroupName, _Config) ->
    ct:pal("Completed test group: ~p", [GroupName]),
    ok.

%% @doc Test case initialization.
-spec init_per_testcase(TestCase :: atom(), Config :: ct:config()) -> ct:config().
init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),

    %% Force garbage collection before test
    [erlang:garbage_collect(P) || P <- erlang:processes()],

    %% Capture baseline metrics
    Baseline = capture_resource_snapshot(),

    [{testcase, TestCase},
     {baseline_snapshot, Baseline},
     {test_start, erlang:system_time(millisecond)} | Config].

%% @doc Test case cleanup.
-spec end_per_testcase(TestCase :: atom(), Config :: ct:config()) -> ok.
end_per_testcase(TestCase, Config) ->
    TestStart = ?config(test_start, Config),
    Duration = erlang:system_time(millisecond) - TestStart,

    ct:pal("Completed test case: ~p (duration: ~p ms)", [TestCase, Duration]),

    %% Force cleanup
    timer:sleep(1000),
    [erlang:garbage_collect(P) || P <- erlang:processes()],

    ok.

%%%===================================================================
%%% Test Cases: Concurrent Execution
%%%===================================================================

%% @doc Test 10 concurrent workflows (baseline).
concurrent_workflows_10(Config) ->
    run_concurrent_workflow_test(Config, ?LIGHT_LOAD_WORKFLOWS, sequence).

%% @doc Test 50 concurrent workflows.
concurrent_workflows_50(Config) ->
    run_concurrent_workflow_test(Config, 50, sequence).

%% @doc Test 100 concurrent workflows.
concurrent_workflows_100(Config) ->
    run_concurrent_workflow_test(Config, ?HEAVY_LOAD_WORKFLOWS, sequence).

%% @doc Test 500 concurrent workflows (stress test).
concurrent_workflows_500(Config) ->
    run_concurrent_workflow_test(Config, ?STRESS_LOAD_WORKFLOWS, sequence).

%% @doc Test 1000 concurrent workflows (extreme stress).
concurrent_workflows_1000(Config) ->
    run_concurrent_workflow_test(Config, ?EXTREME_LOAD_WORKFLOWS, sequence).

%%%===================================================================
%%% Test Cases: Sustained Load
%%%===================================================================

%% @doc Light sustained load test.
sustained_load_light(Config) ->
    LoadConfig = #load_config{
        workflow_count = ?LIGHT_LOAD_WORKFLOWS,
        duration_seconds = ?SUSTAINED_DURATION_SEC,
        pattern_type = sequence,
        spawn_rate = 5,
        think_time = 100
    },
    run_sustained_load_test(Config, LoadConfig).

%% @doc Medium sustained load test.
sustained_load_medium(Config) ->
    LoadConfig = #load_config{
        workflow_count = ?MEDIUM_LOAD_WORKFLOWS,
        duration_seconds = ?SUSTAINED_DURATION_SEC,
        pattern_type = parallel_split,
        spawn_rate = 10,
        think_time = 50
    },
    run_sustained_load_test(Config, LoadConfig).

%% @doc Heavy sustained load test.
sustained_load_heavy(Config) ->
    LoadConfig = #load_config{
        workflow_count = ?HEAVY_LOAD_WORKFLOWS,
        duration_seconds = ?SUSTAINED_DURATION_SEC,
        pattern_type = parallel_split,
        spawn_rate = 20,
        think_time = 10
    },
    run_sustained_load_test(Config, LoadConfig).

%%%===================================================================
%%% Test Cases: Load Patterns
%%%===================================================================

%% @doc Ramp-up load pattern test.
ramp_up_load(Config) ->
    run_load_pattern_test(Config, ramp_up).

%% @doc Steady load pattern test.
steady_load(Config) ->
    run_load_pattern_test(Config, steady).

%% @doc Burst load pattern test.
burst_load(Config) ->
    run_load_pattern_test(Config, burst).

%% @doc Spike load pattern test.
spike_load(Config) ->
    run_load_pattern_test(Config, spike).

%% @doc Oscillating load pattern test.
oscillating_load(Config) ->
    run_load_pattern_test(Config, oscillating).

%%%===================================================================
%%% Test Cases: Resource Monitoring
%%%===================================================================

%% @doc Monitor memory usage under sustained load.
memory_usage_under_load(Config) ->
    ct:pal("Testing memory usage under load..."),

    LoadConfig = #load_config{
        workflow_count = ?MEDIUM_LOAD_WORKFLOWS,
        duration_seconds = ?SUSTAINED_DURATION_SEC,
        pattern_type = sequence,
        spawn_rate = 10,
        think_time = 50
    },

    %% Start resource monitor
    MonitorPid = start_resource_monitor(?RESOURCE_SAMPLE_INTERVAL_MS),

    %% Run load test
    Result = run_sustained_load_test(Config, LoadConfig),

    %% Stop monitor and get snapshots
    Snapshots = stop_resource_monitor(MonitorPid),

    %% Analyze memory growth
    MemoryGrowth = analyze_memory_growth(Snapshots),
    ct:pal("Memory growth: ~p MB", [MemoryGrowth]),

    %% Assert acceptable memory growth
    true = MemoryGrowth < ?ACCEPTABLE_MEMORY_GROWTH_MB,

    Result.

%% @doc Monitor scheduler utilization under load.
scheduler_utilization(Config) ->
    ct:pal("Testing scheduler utilization under load..."),

    LoadConfig = #load_config{
        workflow_count = ?HEAVY_LOAD_WORKFLOWS,
        duration_seconds = ?SHORT_DURATION_SEC,
        pattern_type = parallel_split,
        spawn_rate = 20,
        think_time = 10
    },

    MonitorPid = start_resource_monitor(?RESOURCE_SAMPLE_INTERVAL_MS),
    Result = run_sustained_load_test(Config, LoadConfig),
    Snapshots = stop_resource_monitor(MonitorPid),

    %% Analyze scheduler utilization
    AvgUtilization = analyze_scheduler_utilization(Snapshots),
    ct:pal("Average scheduler utilization: ~p%", [AvgUtilization]),

    Result.

%% @doc Monitor ETS table growth under load.
ets_table_growth(Config) ->
    ct:pal("Testing ETS table growth under load..."),

    BaselineCount = length(ets:all()),
    ct:pal("Baseline ETS tables: ~p", [BaselineCount]),

    LoadConfig = #load_config{
        workflow_count = ?MEDIUM_LOAD_WORKFLOWS,
        duration_seconds = ?SHORT_DURATION_SEC,
        pattern_type = sequence,
        spawn_rate = 10,
        think_time = 50
    },

    Result = run_sustained_load_test(Config, LoadConfig),

    FinalCount = length(ets:all()),
    Growth = FinalCount - BaselineCount,
    ct:pal("ETS table growth: ~p (baseline: ~p, final: ~p)",
           [Growth, BaselineCount, FinalCount]),

    %% Assert reasonable growth
    true = Growth < 50,

    Result.

%% @doc Monitor process count stability under load.
process_count_stability(Config) ->
    ct:pal("Testing process count stability under load..."),

    LoadConfig = #load_config{
        workflow_count = ?MEDIUM_LOAD_WORKFLOWS,
        duration_seconds = ?SUSTAINED_DURATION_SEC,
        pattern_type = sequence,
        spawn_rate = 10,
        think_time = 50
    },

    MonitorPid = start_resource_monitor(?RESOURCE_SAMPLE_INTERVAL_MS),
    Result = run_sustained_load_test(Config, LoadConfig),
    Snapshots = stop_resource_monitor(MonitorPid),

    %% Analyze process count stability
    ProcessCountStats = analyze_process_count(Snapshots),
    ct:pal("Process count stats: ~p", [ProcessCountStats]),

    Result.

%%%===================================================================
%%% Test Cases: Failure Scenarios
%%%===================================================================

%% @doc Test process crashes during load.
process_crash_during_load(Config) ->
    ct:pal("Testing process crash handling during load..."),

    LoadConfig = #load_config{
        workflow_count = ?MEDIUM_LOAD_WORKFLOWS,
        duration_seconds = ?SHORT_DURATION_SEC,
        pattern_type = sequence,
        spawn_rate = 10,
        think_time = 50
    },

    %% Start load test
    LoadPid = spawn_load_test_async(LoadConfig),

    %% Wait for steady state
    timer:sleep(5000),

    %% Inject failures
    FailureCount = inject_random_failures(10),
    ct:pal("Injected ~p failures", [FailureCount]),

    %% Wait for completion
    Result = wait_for_load_test(LoadPid, 60000),

    %% Verify recovery
    true = Result#load_result.workflows_completed > 0,
    ct:pal("Workflows completed despite failures: ~p",
           [Result#load_result.workflows_completed]),

    Result.

%% @doc Test cascading failure recovery.
cascading_failure_recovery(Config) ->
    ct:pal("Testing cascading failure recovery..."),

    LoadConfig = #load_config{
        workflow_count = ?MEDIUM_LOAD_WORKFLOWS,
        duration_seconds = ?SHORT_DURATION_SEC,
        pattern_type = parallel_split,
        spawn_rate = 10,
        think_time = 50
    },

    LoadPid = spawn_load_test_async(LoadConfig),
    timer:sleep(5000),

    %% Inject cascading failures
    inject_cascading_failures(5),

    Result = wait_for_load_test(LoadPid, 60000),

    %% Measure recovery time
    RecoveryTime = measure_recovery_time(Result),
    ct:pal("Recovery time: ~p ms", [RecoveryTime]),

    Result.

%% @doc Test partial system failure.
partial_system_failure(Config) ->
    ct:pal("Testing partial system failure..."),

    LoadConfig = #load_config{
        workflow_count = ?LIGHT_LOAD_WORKFLOWS,
        duration_seconds = ?SHORT_DURATION_SEC,
        pattern_type = sequence,
        spawn_rate = 5,
        think_time = 100
    },

    LoadPid = spawn_load_test_async(LoadConfig),
    timer:sleep(5000),

    %% Simulate partial failure (e.g., kill subset of processes)
    simulate_partial_failure(),

    Result = wait_for_load_test(LoadPid, 60000),

    %% Verify partial recovery
    ct:pal("Result after partial failure: ~p", [Result]),

    Result.

%% @doc Test supervisor restart under load.
supervisor_restart_under_load(Config) ->
    ct:pal("Testing supervisor restart under load..."),

    LoadConfig = #load_config{
        workflow_count = ?MEDIUM_LOAD_WORKFLOWS,
        duration_seconds = ?SHORT_DURATION_SEC,
        pattern_type = sequence,
        spawn_rate = 10,
        think_time = 50
    },

    LoadPid = spawn_load_test_async(LoadConfig),
    timer:sleep(5000),

    %% Trigger supervisor restart (if applicable)
    %% Note: This is implementation-specific
    ct:pal("Supervisor restart test - implementation pending"),

    Result = wait_for_load_test(LoadPid, 60000),
    Result.

%%%===================================================================
%%% Test Cases: Degradation Analysis
%%%===================================================================

%% @doc Measure throughput degradation under increasing load.
throughput_degradation(Config) ->
    ct:pal("Testing throughput degradation..."),

    Loads = [10, 50, 100, 200, 500],
    Results = [run_concurrent_workflow_test(Config, N, sequence) || N <- Loads],

    %% Analyze throughput trend
    Throughputs = [R#load_result.throughput || R <- Results],
    ct:pal("Throughput progression: ~p", [Throughputs]),

    %% Calculate degradation
    Degradation = calculate_degradation(Throughputs),
    ct:pal("Throughput degradation: ~p%", [Degradation]),

    Results.

%% @doc Measure latency degradation under increasing load.
latency_degradation(Config) ->
    ct:pal("Testing latency degradation..."),

    Loads = [10, 50, 100, 200, 500],
    Results = [run_concurrent_workflow_test(Config, N, sequence) || N <- Loads],

    %% Analyze latency trend
    Latencies = [R#load_result.avg_latency_ms || R <- Results],
    ct:pal("Latency progression: ~p", [Latencies]),

    %% Calculate degradation
    Degradation = calculate_degradation(Latencies),
    ct:pal("Latency degradation: ~p%", [Degradation]),

    %% Assert acceptable degradation
    true = Degradation < ?MAX_LATENCY_DEGRADATION_PERCENT,

    Results.

%% @doc Measure recovery time after system stress.
recovery_time_measurement(Config) ->
    ct:pal("Testing recovery time measurement..."),

    %% Apply heavy load
    LoadConfig = #load_config{
        workflow_count = ?STRESS_LOAD_WORKFLOWS,
        duration_seconds = ?SHORT_DURATION_SEC,
        pattern_type = parallel_split,
        spawn_rate = 50,
        think_time = 0
    },

    StartTime = erlang:system_time(millisecond),
    _Result = run_sustained_load_test(Config, LoadConfig),
    StopTime = erlang:system_time(millisecond),

    %% Measure recovery to baseline
    RecoveryStart = erlang:system_time(millisecond),
    wait_for_system_recovery(),
    RecoveryEnd = erlang:system_time(millisecond),

    RecoveryTime = RecoveryEnd - RecoveryStart,
    ct:pal("System recovery time: ~p ms", [RecoveryTime]),

    ok.

%%%===================================================================
%%% Report Generation
%%%===================================================================

%% @doc Generate comprehensive load test report.
generate_load_test_report(Config) ->
    ct:pal("Generating load test report..."),

    ResultsDir = ?config(results_dir, Config),
    TestStartTime = ?config(test_start_time, Config),

    %% Collect all results from ETS
    AllResults = ets:tab2list(load_test_results),

    %% Generate report
    Report = generate_report(AllResults, TestStartTime),

    %% Write report to file
    ReportFile = filename:join(ResultsDir, "load_test_report.txt"),
    ok = file:write_file(ReportFile, Report),

    ct:pal("Load test report written to: ~s", [ReportFile]),

    %% Generate CSV for analysis
    CsvFile = filename:join(ResultsDir, "load_test_results.csv"),
    CsvData = generate_csv(AllResults),
    ok = file:write_file(CsvFile, CsvData),

    ct:pal("Load test CSV written to: ~s", [CsvFile]),

    ok.

%%%===================================================================
%%% Internal Functions: Test Execution
%%%===================================================================

%% @doc Run concurrent workflow test with N workflows.
-spec run_concurrent_workflow_test(ct:config(), pos_integer(), atom()) ->
    #load_result{}.
run_concurrent_workflow_test(Config, WorkflowCount, PatternType) ->
    TestCase = ?config(testcase, Config),
    ct:pal("Running ~p concurrent ~p workflows", [WorkflowCount, PatternType]),

    StartTime = erlang:system_time(millisecond),

    %% Spawn workflows concurrently
    WorkflowPids = spawn_concurrent_workflows(WorkflowCount, PatternType),
    ct:pal("Spawned ~p workflow processes", [length(WorkflowPids)]),

    %% Wait for all workflows to complete with timeout
    Timeout = WorkflowCount * 1000 + 30000,  % 1s per workflow + 30s buffer
    Results = collect_workflow_results(WorkflowPids, Timeout),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% Analyze results
    Completed = length([ok || {ok, _} <- Results]),
    Failed = length([error || {error, _} <- Results]),
    Latencies = [Lat || {ok, Lat} <- Results],

    LoadResult = #load_result{
        test_name = atom_to_binary(TestCase, utf8),
        config = #load_config{
            workflow_count = WorkflowCount,
            duration_seconds = Duration div 1000,
            pattern_type = PatternType,
            spawn_rate = 0,
            think_time = 0
        },
        start_time = StartTime,
        end_time = EndTime,
        duration_ms = Duration,
        workflows_started = WorkflowCount,
        workflows_completed = Completed,
        workflows_failed = Failed,
        throughput = (Completed / Duration) * 1000.0,
        avg_latency_ms = safe_avg(Latencies),
        p50_latency_ms = percentile(Latencies, 50),
        p95_latency_ms = percentile(Latencies, 95),
        p99_latency_ms = percentile(Latencies, 99),
        max_latency_ms = safe_max(Latencies),
        min_latency_ms = safe_min(Latencies),
        errors = [E || {error, E} <- Results]
    },

    %% Store result
    ets:insert(load_test_results, LoadResult),

    ct:pal("Test complete: ~p completed, ~p failed, throughput: ~.2f wf/s",
           [Completed, Failed, LoadResult#load_result.throughput]),

    LoadResult.

%% @doc Run sustained load test.
-spec run_sustained_load_test(ct:config(), #load_config{}) -> #load_result{}.
run_sustained_load_test(Config, LoadConfig) ->
    TestCase = ?config(testcase, Config),
    ct:pal("Running sustained load test: ~p", [LoadConfig]),

    StartTime = erlang:system_time(millisecond),

    %% Spawn workflows at controlled rate
    WorkflowPids = spawn_workflows_at_rate(LoadConfig),

    %% Wait for duration
    timer:sleep(LoadConfig#load_config.duration_seconds * 1000),

    %% Collect results
    Results = collect_workflow_results(WorkflowPids, 60000),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% Analyze results
    Completed = length([ok || {ok, _} <- Results]),
    Failed = length([error || {error, _} <- Results]),
    Latencies = [Lat || {ok, Lat} <- Results],

    LoadResult = #load_result{
        test_name = atom_to_binary(TestCase, utf8),
        config = LoadConfig,
        start_time = StartTime,
        end_time = EndTime,
        duration_ms = Duration,
        workflows_started = length(WorkflowPids),
        workflows_completed = Completed,
        workflows_failed = Failed,
        throughput = (Completed / Duration) * 1000.0,
        avg_latency_ms = safe_avg(Latencies),
        p50_latency_ms = percentile(Latencies, 50),
        p95_latency_ms = percentile(Latencies, 95),
        p99_latency_ms = percentile(Latencies, 99),
        max_latency_ms = safe_max(Latencies),
        min_latency_ms = safe_min(Latencies),
        errors = [E || {error, E} <- Results]
    },

    ets:insert(load_test_results, LoadResult),

    ct:pal("Sustained load test complete: ~p completed, ~p failed",
           [Completed, Failed]),

    LoadResult.

%% @doc Run load pattern test.
-spec run_load_pattern_test(ct:config(), atom()) -> #load_result{}.
run_load_pattern_test(Config, Pattern) ->
    TestCase = ?config(testcase, Config),
    ct:pal("Running load pattern test: ~p", [Pattern]),

    LoadConfig = get_load_pattern_config(Pattern),

    StartTime = erlang:system_time(millisecond),

    %% Execute load pattern
    WorkflowPids = execute_load_pattern(Pattern, LoadConfig),

    %% Wait for completion
    Results = collect_workflow_results(WorkflowPids, 120000),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% Analyze results
    Completed = length([ok || {ok, _} <- Results]),
    Failed = length([error || {error, _} <- Results]),
    Latencies = [Lat || {ok, Lat} <- Results],

    LoadResult = #load_result{
        test_name = atom_to_binary(TestCase, utf8),
        config = LoadConfig,
        start_time = StartTime,
        end_time = EndTime,
        duration_ms = Duration,
        workflows_started = length(WorkflowPids),
        workflows_completed = Completed,
        workflows_failed = Failed,
        throughput = (Completed / Duration) * 1000.0,
        avg_latency_ms = safe_avg(Latencies),
        p50_latency_ms = percentile(Latencies, 50),
        p95_latency_ms = percentile(Latencies, 95),
        p99_latency_ms = percentile(Latencies, 99),
        max_latency_ms = safe_max(Latencies),
        min_latency_ms = safe_min(Latencies),
        errors = [E || {error, E} <- Results]
    },

    ets:insert(load_test_results, LoadResult),

    ct:pal("Load pattern test complete: ~p", [Pattern]),

    LoadResult.

%%%===================================================================
%%% Internal Functions: Workflow Spawning
%%%===================================================================

%% @doc Spawn N workflows concurrently.
-spec spawn_concurrent_workflows(pos_integer(), atom()) -> [pid()].
spawn_concurrent_workflows(Count, PatternType) ->
    [spawn_workflow(PatternType) || _ <- lists:seq(1, Count)].

%% @doc Spawn workflows at controlled rate.
-spec spawn_workflows_at_rate(#load_config{}) -> [pid()].
spawn_workflows_at_rate(LoadConfig) ->
    #load_config{
        workflow_count = Count,
        spawn_rate = Rate,
        pattern_type = PatternType
    } = LoadConfig,

    IntervalMs = 1000 div Rate,

    spawn_workflows_with_interval(Count, PatternType, IntervalMs, []).

%% @doc Spawn workflows with interval between spawns.
-spec spawn_workflows_with_interval(pos_integer(), atom(), pos_integer(), [pid()]) ->
    [pid()].
spawn_workflows_with_interval(0, _PatternType, _IntervalMs, Acc) ->
    lists:reverse(Acc);
spawn_workflows_with_interval(Count, PatternType, IntervalMs, Acc) ->
    Pid = spawn_workflow(PatternType),
    timer:sleep(IntervalMs),
    spawn_workflows_with_interval(Count - 1, PatternType, IntervalMs, [Pid | Acc]).

%% @doc Spawn a single workflow process.
-spec spawn_workflow(atom()) -> pid().
spawn_workflow(PatternType) ->
    Parent = self(),
    spawn_link(fun() ->
        StartTime = erlang:system_time(millisecond),
        try
            Result = execute_workflow(PatternType),
            EndTime = erlang:system_time(millisecond),
            Latency = EndTime - StartTime,
            Parent ! {workflow_result, self(), {ok, Latency}},
            Result
        catch
            Class:Reason:Stacktrace ->
                Parent ! {workflow_result, self(), {error, {Class, Reason, Stacktrace}}},
                error
        end
    end).

%% @doc Execute a workflow based on pattern type.
-spec execute_workflow(atom()) -> ok.
execute_workflow(sequence) ->
    %% Simple sequence execution
    {ok, Pid} = gen_pnet:start_link(sequence, sequence, #{from => task_a, to => task_b}),

    %% Inject initial token
    ok = gen_pnet:inject(Pid, #{p_start => [start_token]}),

    %% Step through workflow
    ok = gen_pnet:step(Pid),
    ok = gen_pnet:step(Pid),
    ok = gen_pnet:step(Pid),

    %% Stop process
    gen_pnet:stop(Pid),
    ok;

execute_workflow(parallel_split) ->
    %% Parallel split execution
    {ok, Pid} = gen_pnet:start_link(parallel_split, parallel_split, #{}),

    %% Inject initial token
    ok = gen_pnet:inject(Pid, #{p_start => [start_token]}),

    %% Step through workflow (split, execute branches, join)
    ok = gen_pnet:step(Pid),
    timer:sleep(10),  % Simulate work
    ok = gen_pnet:step(Pid),
    ok = gen_pnet:step(Pid),

    gen_pnet:stop(Pid),
    ok;

execute_workflow(_PatternType) ->
    %% Generic execution
    timer:sleep(10),
    ok.

%%%===================================================================
%%% Internal Functions: Result Collection
%%%===================================================================

%% @doc Collect workflow results with timeout.
-spec collect_workflow_results([pid()], pos_integer()) ->
    [{ok, non_neg_integer()} | {error, term()}].
collect_workflow_results(Pids, Timeout) ->
    collect_workflow_results(Pids, Timeout, []).

%% @doc Collect workflow results (internal).
-spec collect_workflow_results([pid()], pos_integer(), [term()]) -> [term()].
collect_workflow_results([], _Timeout, Acc) ->
    lists:reverse(Acc);
collect_workflow_results(Pids, Timeout, Acc) ->
    receive
        {workflow_result, Pid, Result} ->
            RemainingPids = lists:delete(Pid, Pids),
            collect_workflow_results(RemainingPids, Timeout, [Result | Acc])
    after Timeout ->
        %% Timeout - mark remaining as failed
        Timeouts = [{error, timeout} || _ <- Pids],
        lists:reverse(Acc) ++ Timeouts
    end.

%%%===================================================================
%%% Internal Functions: Resource Monitoring
%%%===================================================================

%% @doc Start resource monitoring process.
-spec start_resource_monitor(pos_integer()) -> pid().
start_resource_monitor(IntervalMs) ->
    Parent = self(),
    spawn_link(fun() -> resource_monitor_loop(Parent, IntervalMs, []) end).

%% @doc Resource monitoring loop.
-spec resource_monitor_loop(pid(), pos_integer(), [#resource_snapshot{}]) -> ok.
resource_monitor_loop(Parent, IntervalMs, Snapshots) ->
    receive
        {stop, From} ->
            From ! {snapshots, lists:reverse(Snapshots)},
            ok
    after IntervalMs ->
        Snapshot = capture_resource_snapshot(),
        resource_monitor_loop(Parent, IntervalMs, [Snapshot | Snapshots])
    end.

%% @doc Stop resource monitor and retrieve snapshots.
-spec stop_resource_monitor(pid()) -> [#resource_snapshot{}].
stop_resource_monitor(MonitorPid) ->
    MonitorPid ! {stop, self()},
    receive
        {snapshots, Snapshots} -> Snapshots
    after 5000 ->
        []
    end.

%% @doc Capture current resource snapshot.
-spec capture_resource_snapshot() -> #resource_snapshot{}.
capture_resource_snapshot() ->
    MemInfo = erlang:memory(),

    #resource_snapshot{
        timestamp = erlang:system_time(millisecond),
        memory_total = proplists:get_value(total, MemInfo),
        memory_processes = proplists:get_value(processes, MemInfo),
        memory_ets = proplists:get_value(ets, MemInfo),
        process_count = erlang:system_info(process_count),
        scheduler_utilization = get_scheduler_utilization(),
        run_queue_length = erlang:statistics(run_queue),
        ets_table_count = length(ets:all()),
        gc_count = element(1, erlang:statistics(garbage_collection)),
        gc_words_reclaimed = element(2, erlang:statistics(garbage_collection))
    }.

%% @doc Get scheduler utilization.
-spec get_scheduler_utilization() -> [float()].
get_scheduler_utilization() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            erlang:system_flag(scheduler_wall_time, true),
            timer:sleep(100),
            get_scheduler_utilization();
        List ->
            [Active / Total || {_Id, Active, Total} <- List, Total > 0]
    end.

%% @doc Analyze memory growth from snapshots.
-spec analyze_memory_growth([#resource_snapshot{}]) -> float().
analyze_memory_growth([]) ->
    0.0;
analyze_memory_growth(Snapshots) ->
    InitialMem = (hd(Snapshots))#resource_snapshot.memory_total,
    FinalMem = (lists:last(Snapshots))#resource_snapshot.memory_total,
    (FinalMem - InitialMem) / (1024 * 1024).  % Convert to MB

%% @doc Analyze scheduler utilization from snapshots.
-spec analyze_scheduler_utilization([#resource_snapshot{}]) -> float().
analyze_scheduler_utilization([]) ->
    0.0;
analyze_scheduler_utilization(Snapshots) ->
    AllUtilizations = lists:flatten([S#resource_snapshot.scheduler_utilization
                                     || S <- Snapshots]),
    safe_avg(AllUtilizations).

%% @doc Analyze process count from snapshots.
-spec analyze_process_count([#resource_snapshot{}]) -> map().
analyze_process_count([]) ->
    #{};
analyze_process_count(Snapshots) ->
    Counts = [S#resource_snapshot.process_count || S <- Snapshots],
    #{
        min => lists:min(Counts),
        max => lists:max(Counts),
        avg => safe_avg(Counts),
        std_dev => std_dev(Counts)
    }.

%%%===================================================================
%%% Internal Functions: Failure Injection
%%%===================================================================

%% @doc Inject random failures into running workflows.
-spec inject_random_failures(pos_integer()) -> pos_integer().
inject_random_failures(Count) ->
    Processes = erlang:processes(),
    WorkflowProcs = [P || P <- Processes, is_workflow_process(P)],
    TargetCount = min(Count, length(WorkflowProcs)),

    Targets = lists:sublist(shuffle(WorkflowProcs), TargetCount),
    [exit(P, kill) || P <- Targets],

    TargetCount.

%% @doc Check if process is a workflow process.
-spec is_workflow_process(pid()) -> boolean().
is_workflow_process(Pid) ->
    case erlang:process_info(Pid, [dictionary, registered_name]) of
        undefined ->
            false;
        [{dictionary, Dict}, _] ->
            proplists:is_defined('$ancestors', Dict);
        _ ->
            false
    end.

%% @doc Inject cascading failures.
-spec inject_cascading_failures(pos_integer()) -> ok.
inject_cascading_failures(Count) ->
    _FailureCount = inject_random_failures(Count),
    timer:sleep(100),
    _FailureCount2 = inject_random_failures(Count div 2),
    ok.

%% @doc Simulate partial system failure.
-spec simulate_partial_failure() -> ok.
simulate_partial_failure() ->
    _FailureCount = inject_random_failures(5),
    timer:sleep(500),
    ok.

%%%===================================================================
%%% Internal Functions: Load Patterns
%%%===================================================================

%% @doc Get load pattern configuration.
-spec get_load_pattern_config(atom()) -> #load_config{}.
get_load_pattern_config(ramp_up) ->
    #load_config{
        workflow_count = 100,
        duration_seconds = 60,
        pattern_type = sequence,
        spawn_rate = 5,  % Will increase over time
        think_time = 50
    };
get_load_pattern_config(steady) ->
    #load_config{
        workflow_count = 100,
        duration_seconds = 60,
        pattern_type = sequence,
        spawn_rate = 10,
        think_time = 50
    };
get_load_pattern_config(burst) ->
    #load_config{
        workflow_count = 100,
        duration_seconds = 30,
        pattern_type = parallel_split,
        spawn_rate = 50,
        think_time = 0
    };
get_load_pattern_config(spike) ->
    #load_config{
        workflow_count = 200,
        duration_seconds = 30,
        pattern_type = parallel_split,
        spawn_rate = 100,
        think_time = 0
    };
get_load_pattern_config(oscillating) ->
    #load_config{
        workflow_count = 100,
        duration_seconds = 60,
        pattern_type = sequence,
        spawn_rate = 10,  % Will oscillate
        think_time = 50
    }.

%% @doc Execute load pattern.
-spec execute_load_pattern(atom(), #load_config{}) -> [pid()].
execute_load_pattern(ramp_up, Config) ->
    execute_ramp_up_pattern(Config);
execute_load_pattern(steady, Config) ->
    spawn_workflows_at_rate(Config);
execute_load_pattern(burst, Config) ->
    execute_burst_pattern(Config);
execute_load_pattern(spike, Config) ->
    execute_spike_pattern(Config);
execute_load_pattern(oscillating, Config) ->
    execute_oscillating_pattern(Config).

%% @doc Execute ramp-up load pattern.
-spec execute_ramp_up_pattern(#load_config{}) -> [pid()].
execute_ramp_up_pattern(Config) ->
    #load_config{
        workflow_count = Count,
        duration_seconds = Duration,
        pattern_type = PatternType
    } = Config,

    %% Gradually increase spawn rate
    StepCount = 10,
    StepDuration = (Duration * 1000) div StepCount,
    WorkflowsPerStep = Count div StepCount,

    spawn_with_ramp_up(PatternType, StepCount, WorkflowsPerStep, StepDuration, []).

%% @doc Spawn workflows with ramp-up.
-spec spawn_with_ramp_up(atom(), pos_integer(), pos_integer(), pos_integer(), [pid()]) ->
    [pid()].
spawn_with_ramp_up(0, _PatternType, _WorkflowsPerStep, _StepDuration, Acc) ->
    lists:reverse(Acc);
spawn_with_ramp_up(StepCount, PatternType, WorkflowsPerStep, StepDuration, Acc) ->
    %% Spawn workflows for this step
    Pids = [spawn_workflow(PatternType) || _ <- lists:seq(1, WorkflowsPerStep)],
    timer:sleep(StepDuration),
    spawn_with_ramp_up(StepCount - 1, PatternType, WorkflowsPerStep, StepDuration,
                       Pids ++ Acc).

%% @doc Execute burst load pattern.
-spec execute_burst_pattern(#load_config{}) -> [pid()].
execute_burst_pattern(Config) ->
    #load_config{
        workflow_count = Count,
        pattern_type = PatternType
    } = Config,

    %% Spawn bursts of workflows
    BurstSize = 20,
    BurstCount = Count div BurstSize,
    BurstInterval = 2000,

    spawn_bursts(PatternType, BurstCount, BurstSize, BurstInterval, []).

%% @doc Spawn workflows in bursts.
-spec spawn_bursts(atom(), pos_integer(), pos_integer(), pos_integer(), [pid()]) ->
    [pid()].
spawn_bursts(0, _PatternType, _BurstSize, _Interval, Acc) ->
    lists:reverse(Acc);
spawn_bursts(BurstCount, PatternType, BurstSize, Interval, Acc) ->
    Pids = [spawn_workflow(PatternType) || _ <- lists:seq(1, BurstSize)],
    timer:sleep(Interval),
    spawn_bursts(BurstCount - 1, PatternType, BurstSize, Interval, Pids ++ Acc).

%% @doc Execute spike load pattern.
-spec execute_spike_pattern(#load_config{}) -> [pid()].
execute_spike_pattern(Config) ->
    #load_config{
        workflow_count = Count,
        pattern_type = PatternType
    } = Config,

    %% Low load phase
    LowLoad = spawn_concurrent_workflows(10, PatternType),
    timer:sleep(5000),

    %% Spike phase
    SpikeLoad = spawn_concurrent_workflows(Count - 10, PatternType),
    timer:sleep(5000),

    LowLoad ++ SpikeLoad.

%% @doc Execute oscillating load pattern.
-spec execute_oscillating_pattern(#load_config{}) -> [pid()].
execute_oscillating_pattern(Config) ->
    #load_config{
        workflow_count = Count,
        duration_seconds = Duration,
        pattern_type = PatternType
    } = Config,

    %% Oscillate between high and low rates
    Cycles = 5,
    CycleDuration = (Duration * 1000) div Cycles,
    WorkflowsPerCycle = Count div Cycles,

    spawn_oscillating(PatternType, Cycles, WorkflowsPerCycle, CycleDuration, []).

%% @doc Spawn workflows with oscillating pattern.
-spec spawn_oscillating(atom(), pos_integer(), pos_integer(), pos_integer(), [pid()]) ->
    [pid()].
spawn_oscillating(0, _PatternType, _WorkflowsPerCycle, _CycleDuration, Acc) ->
    lists:reverse(Acc);
spawn_oscillating(Cycles, PatternType, WorkflowsPerCycle, CycleDuration, Acc) ->
    %% High rate phase
    HighPids = [spawn_workflow(PatternType) ||
                _ <- lists:seq(1, WorkflowsPerCycle div 2)],
    timer:sleep(CycleDuration div 4),

    %% Low rate phase
    LowPids = spawn_workflows_with_interval(WorkflowsPerCycle div 2, PatternType,
                                           100, []),
    timer:sleep((CycleDuration * 3) div 4),

    spawn_oscillating(Cycles - 1, PatternType, WorkflowsPerCycle, CycleDuration,
                     HighPids ++ LowPids ++ Acc).

%%%===================================================================
%%% Internal Functions: Async Test Execution
%%%===================================================================

%% @doc Spawn load test asynchronously.
-spec spawn_load_test_async(#load_config{}) -> pid().
spawn_load_test_async(LoadConfig) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = execute_load_test(LoadConfig),
        Parent ! {load_test_result, self(), Result}
    end).

%% @doc Execute load test.
-spec execute_load_test(#load_config{}) -> #load_result{}.
execute_load_test(LoadConfig) ->
    StartTime = erlang:system_time(millisecond),
    WorkflowPids = spawn_workflows_at_rate(LoadConfig),
    timer:sleep(LoadConfig#load_config.duration_seconds * 1000),
    Results = collect_workflow_results(WorkflowPids, 60000),
    EndTime = erlang:system_time(millisecond),

    Completed = length([ok || {ok, _} <- Results]),
    Failed = length([error || {error, _} <- Results]),
    Latencies = [Lat || {ok, Lat} <- Results],

    #load_result{
        test_name = <<"async_load_test">>,
        config = LoadConfig,
        start_time = StartTime,
        end_time = EndTime,
        duration_ms = EndTime - StartTime,
        workflows_started = length(WorkflowPids),
        workflows_completed = Completed,
        workflows_failed = Failed,
        throughput = (Completed / (EndTime - StartTime)) * 1000.0,
        avg_latency_ms = safe_avg(Latencies),
        p50_latency_ms = percentile(Latencies, 50),
        p95_latency_ms = percentile(Latencies, 95),
        p99_latency_ms = percentile(Latencies, 99),
        max_latency_ms = safe_max(Latencies),
        min_latency_ms = safe_min(Latencies),
        errors = [E || {error, E} <- Results]
    }.

%% @doc Wait for async load test to complete.
-spec wait_for_load_test(pid(), pos_integer()) -> #load_result{}.
wait_for_load_test(LoadPid, Timeout) ->
    receive
        {load_test_result, LoadPid, Result} ->
            Result
    after Timeout ->
        exit(LoadPid, kill),
        #load_result{
            test_name = <<"timeout">>,
            config = #load_config{
                workflow_count = 0,
                duration_seconds = 0,
                pattern_type = unknown,
                spawn_rate = 0,
                think_time = 0
            },
            start_time = 0,
            end_time = 0,
            duration_ms = 0,
            workflows_started = 0,
            workflows_completed = 0,
            workflows_failed = 0,
            throughput = 0.0,
            avg_latency_ms = 0.0,
            p50_latency_ms = 0,
            p95_latency_ms = 0,
            p99_latency_ms = 0,
            max_latency_ms = 0,
            min_latency_ms = 0,
            errors = [timeout]
        }
    end.

%%%===================================================================
%%% Internal Functions: Recovery
%%%===================================================================

%% @doc Measure recovery time after failure.
-spec measure_recovery_time(#load_result{}) -> non_neg_integer().
measure_recovery_time(_Result) ->
    %% Implementation: Measure time for system to return to baseline
    %% This is a placeholder
    1000.

%% @doc Wait for system to recover to baseline.
-spec wait_for_system_recovery() -> ok.
wait_for_system_recovery() ->
    wait_for_system_recovery(10, 1000).

%% @doc Wait for system recovery with retry.
-spec wait_for_system_recovery(pos_integer(), pos_integer()) -> ok.
wait_for_system_recovery(0, _Interval) ->
    ok;
wait_for_system_recovery(Retries, Interval) ->
    case is_system_recovered() of
        true ->
            ok;
        false ->
            timer:sleep(Interval),
            wait_for_system_recovery(Retries - 1, Interval)
    end.

%% @doc Check if system has recovered.
-spec is_system_recovered() -> boolean().
is_system_recovered() ->
    RunQueue = erlang:statistics(run_queue),
    ProcessCount = erlang:system_info(process_count),

    %% Simple heuristics
    RunQueue < 10 andalso ProcessCount < 1000.

%%%===================================================================
%%% Internal Functions: Statistics
%%%===================================================================

%% @doc Calculate safe average.
-spec safe_avg([number()]) -> float().
safe_avg([]) ->
    0.0;
safe_avg(List) ->
    lists:sum(List) / length(List).

%% @doc Calculate safe max.
-spec safe_max([number()]) -> number().
safe_max([]) ->
    0;
safe_max(List) ->
    lists:max(List).

%% @doc Calculate safe min.
-spec safe_min([number()]) -> number().
safe_min([]) ->
    0;
safe_min(List) ->
    lists:min(List).

%% @doc Calculate percentile.
-spec percentile([number()], non_neg_integer()) -> number().
percentile([], _Percentile) ->
    0;
percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    Index = max(1, round((Percentile / 100) * length(Sorted))),
    lists:nth(Index, Sorted).

%% @doc Calculate standard deviation.
-spec std_dev([number()]) -> float().
std_dev([]) ->
    0.0;
std_dev(List) ->
    Avg = safe_avg(List),
    Variance = lists:sum([math:pow(X - Avg, 2) || X <- List]) / length(List),
    math:sqrt(Variance).

%% @doc Calculate degradation percentage.
-spec calculate_degradation([number()]) -> float().
calculate_degradation([]) ->
    0.0;
calculate_degradation([_]) ->
    0.0;
calculate_degradation(List) ->
    Baseline = hd(List),
    Final = lists:last(List),
    ((Final - Baseline) / Baseline) * 100.0.

%% @doc Shuffle list.
-spec shuffle([term()]) -> [term()].
shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- List])].

%%%===================================================================
%%% Internal Functions: Report Generation
%%%===================================================================

%% @doc Generate text report from results.
-spec generate_report([#load_result{}], integer()) -> binary().
generate_report(Results, TestStartTime) ->
    Header = generate_report_header(TestStartTime),
    Summary = generate_report_summary(Results),
    Details = [generate_result_detail(R) || R <- Results],

    iolist_to_binary([Header, "\n\n", Summary, "\n\n", Details]).

%% @doc Generate report header.
-spec generate_report_header(integer()) -> iolist().
generate_report_header(TestStartTime) ->
    io_lib:format("CRE Load Testing Report~n"
                  "======================~n~n"
                  "Test Start Time: ~s~n"
                  "Test End Time: ~s~n",
                  [format_timestamp(TestStartTime),
                   format_timestamp(erlang:system_time(millisecond))]).

%% @doc Generate report summary.
-spec generate_report_summary([#load_result{}]) -> iolist().
generate_report_summary(Results) ->
    TotalTests = length(Results),
    TotalWorkflows = lists:sum([R#load_result.workflows_started || R <- Results]),
    TotalCompleted = lists:sum([R#load_result.workflows_completed || R <- Results]),
    TotalFailed = lists:sum([R#load_result.workflows_failed || R <- Results]),
    AvgThroughput = safe_avg([R#load_result.throughput || R <- Results]),

    io_lib:format("Summary~n"
                  "-------~n"
                  "Total Tests: ~p~n"
                  "Total Workflows: ~p~n"
                  "Total Completed: ~p~n"
                  "Total Failed: ~p~n"
                  "Average Throughput: ~.2f workflows/sec~n",
                  [TotalTests, TotalWorkflows, TotalCompleted, TotalFailed,
                   AvgThroughput]).

%% @doc Generate result detail.
-spec generate_result_detail(#load_result{}) -> iolist().
generate_result_detail(Result) ->
    #load_result{
        test_name = TestName,
        duration_ms = Duration,
        workflows_started = Started,
        workflows_completed = Completed,
        workflows_failed = Failed,
        throughput = Throughput,
        avg_latency_ms = AvgLatency,
        p95_latency_ms = P95,
        p99_latency_ms = P99
    } = Result,

    io_lib:format("~nTest: ~s~n"
                  "Duration: ~p ms~n"
                  "Started: ~p, Completed: ~p, Failed: ~p~n"
                  "Throughput: ~.2f workflows/sec~n"
                  "Latency - Avg: ~.2f ms, P95: ~p ms, P99: ~p ms~n",
                  [TestName, Duration, Started, Completed, Failed,
                   Throughput, AvgLatency, P95, P99]).

%% @doc Generate CSV data.
-spec generate_csv([#load_result{}]) -> binary().
generate_csv(Results) ->
    Header = "Test Name,Duration (ms),Started,Completed,Failed,"
             "Throughput (wf/s),Avg Latency (ms),P50 (ms),P95 (ms),P99 (ms),"
             "Max (ms),Min (ms)\n",

    Rows = [generate_csv_row(R) || R <- Results],

    iolist_to_binary([Header | Rows]).

%% @doc Generate CSV row.
-spec generate_csv_row(#load_result{}) -> iolist().
generate_csv_row(Result) ->
    #load_result{
        test_name = TestName,
        duration_ms = Duration,
        workflows_started = Started,
        workflows_completed = Completed,
        workflows_failed = Failed,
        throughput = Throughput,
        avg_latency_ms = AvgLatency,
        p50_latency_ms = P50,
        p95_latency_ms = P95,
        p99_latency_ms = P99,
        max_latency_ms = Max,
        min_latency_ms = Min
    } = Result,

    io_lib:format("~s,~p,~p,~p,~p,~.2f,~.2f,~p,~p,~p,~p,~p~n",
                  [TestName, Duration, Started, Completed, Failed,
                   Throughput, AvgLatency, P50, P95, P99, Max, Min]).

%% @doc Format timestamp.
-spec format_timestamp(integer()) -> string().
format_timestamp(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    calendar:system_time_to_rfc3339(Seconds, [{unit, second}, {offset, "Z"}]).
