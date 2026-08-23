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
%% @doc Load Test Scenarios - Advanced Load Testing Patterns
%%
%% Provides reusable load testing scenarios for CRE workflow engine:
%% - Complex workflow patterns under load
%% - Multi-pattern concurrent execution
%% - Realistic workload simulations
%% - Performance profiling helpers
%% - Resource leak detection
%%
%% @end
%% -------------------------------------------------------------------

-module(load_test_scenarios).

-export([
    % Scenario runners
    run_scenario/1,
    run_scenario/2,

    % Complex workflow scenarios
    mixed_pattern_load/1,
    nested_workflow_load/1,
    long_running_workflow_load/1,
    rapid_short_workflow_load/1,

    % Real-world simulation scenarios
    order_fulfillment_simulation/1,
    approval_workflow_simulation/1,
    data_pipeline_simulation/1,

    % Stress test scenarios
    extreme_concurrency_test/0,
    memory_pressure_test/0,
    scheduler_saturation_test/0,

    % Profiling helpers
    profile_workflow_execution/2,
    detect_memory_leaks/2,
    measure_gc_impact/1,

    % Monitoring utilities
    start_profiling/0,
    stop_profiling/1,
    get_profiling_results/1
]).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-record(scenario_config, {
    name :: binary(),
    patterns :: [atom()],
    workflow_count :: pos_integer(),
    duration_seconds :: pos_integer(),
    concurrency_level :: pos_integer(),
    throttle_ms :: non_neg_integer(),
    enable_profiling :: boolean(),
    enable_tracing :: boolean()
}).

-record(scenario_result, {
    name :: binary(),
    duration_ms :: non_neg_integer(),
    workflows_executed :: non_neg_integer(),
    success_count :: non_neg_integer(),
    failure_count :: non_neg_integer(),
    avg_execution_time_ms :: float(),
    throughput :: float(),
    memory_stats :: map(),
    profile_data :: term()
}).

-record(profiling_state, {
    start_time :: integer(),
    baseline_memory :: non_neg_integer(),
    snapshots :: [map()],
    trace_enabled :: boolean()
}).

%%%===================================================================
%%% Scenario Runners
%%%===================================================================

%% @doc Run a named scenario with default configuration.
-spec run_scenario(atom()) -> #scenario_result{}.
run_scenario(ScenarioName) ->
    run_scenario(ScenarioName, #{}).

%% @doc Run a named scenario with custom options.
-spec run_scenario(atom(), map()) -> #scenario_result{}.
run_scenario(mixed_pattern_load, Opts) ->
    mixed_pattern_load(Opts);
run_scenario(nested_workflow_load, Opts) ->
    nested_workflow_load(Opts);
run_scenario(long_running_workflow_load, Opts) ->
    long_running_workflow_load(Opts);
run_scenario(rapid_short_workflow_load, Opts) ->
    rapid_short_workflow_load(Opts);
run_scenario(order_fulfillment_simulation, Opts) ->
    order_fulfillment_simulation(Opts);
run_scenario(approval_workflow_simulation, Opts) ->
    approval_workflow_simulation(Opts);
run_scenario(data_pipeline_simulation, Opts) ->
    data_pipeline_simulation(Opts);
run_scenario(Unknown, _Opts) ->
    error({unknown_scenario, Unknown}).

%%%===================================================================
%%% Complex Workflow Scenarios
%%%===================================================================

%% @doc Mixed pattern load - multiple pattern types executing concurrently.
-spec mixed_pattern_load(map()) -> #scenario_result{}.
mixed_pattern_load(Opts) ->
    Config = #scenario_config{
        name = <<"mixed_pattern_load">>,
        patterns = [sequence, parallel_split, exclusive_choice,
                   synchronization, multiple_merge],
        workflow_count = maps:get(workflow_count, Opts, 100),
        duration_seconds = maps:get(duration_seconds, Opts, 60),
        concurrency_level = maps:get(concurrency_level, Opts, 20),
        throttle_ms = maps:get(throttle_ms, Opts, 50),
        enable_profiling = maps:get(enable_profiling, Opts, false),
        enable_tracing = maps:get(enable_tracing, Opts, false)
    },

    execute_scenario(Config, fun execute_mixed_pattern_workflows/1).

%% @doc Nested workflow load - workflows containing sub-workflows.
-spec nested_workflow_load(map()) -> #scenario_result{}.
nested_workflow_load(Opts) ->
    Config = #scenario_config{
        name = <<"nested_workflow_load">>,
        patterns = [sequence, parallel_split],
        workflow_count = maps:get(workflow_count, Opts, 50),
        duration_seconds = maps:get(duration_seconds, Opts, 60),
        concurrency_level = maps:get(concurrency_level, Opts, 10),
        throttle_ms = maps:get(throttle_ms, Opts, 100),
        enable_profiling = maps:get(enable_profiling, Opts, false),
        enable_tracing = maps:get(enable_tracing, Opts, false)
    },

    execute_scenario(Config, fun execute_nested_workflows/1).

%% @doc Long-running workflow load - workflows with extended execution time.
-spec long_running_workflow_load(map()) -> #scenario_result{}.
long_running_workflow_load(Opts) ->
    Config = #scenario_config{
        name = <<"long_running_workflow_load">>,
        patterns = [sequence, parallel_split],
        workflow_count = maps:get(workflow_count, Opts, 20),
        duration_seconds = maps:get(duration_seconds, Opts, 120),
        concurrency_level = maps:get(concurrency_level, Opts, 5),
        throttle_ms = maps:get(throttle_ms, Opts, 200),
        enable_profiling = maps:get(enable_profiling, Opts, true),
        enable_tracing = maps:get(enable_tracing, Opts, false)
    },

    execute_scenario(Config, fun execute_long_running_workflows/1).

%% @doc Rapid short workflow load - many quick workflows.
-spec rapid_short_workflow_load(map()) -> #scenario_result{}.
rapid_short_workflow_load(Opts) ->
    Config = #scenario_config{
        name = <<"rapid_short_workflow_load">>,
        patterns = [sequence],
        workflow_count = maps:get(workflow_count, Opts, 500),
        duration_seconds = maps:get(duration_seconds, Opts, 30),
        concurrency_level = maps:get(concurrency_level, Opts, 50),
        throttle_ms = maps:get(throttle_ms, Opts, 10),
        enable_profiling = maps:get(enable_profiling, Opts, false),
        enable_tracing = maps:get(enable_tracing, Opts, false)
    },

    execute_scenario(Config, fun execute_rapid_workflows/1).

%%%===================================================================
%%% Real-World Simulation Scenarios
%%%===================================================================

%% @doc Order fulfillment simulation - realistic e-commerce workflow.
-spec order_fulfillment_simulation(map()) -> #scenario_result{}.
order_fulfillment_simulation(Opts) ->
    Config = #scenario_config{
        name = <<"order_fulfillment_simulation">>,
        patterns = [sequence, parallel_split, exclusive_choice],
        workflow_count = maps:get(workflow_count, Opts, 100),
        duration_seconds = maps:get(duration_seconds, Opts, 60),
        concurrency_level = maps:get(concurrency_level, Opts, 15),
        throttle_ms = maps:get(throttle_ms, Opts, 100),
        enable_profiling = maps:get(enable_profiling, Opts, true),
        enable_tracing = maps:get(enable_tracing, Opts, false)
    },

    execute_scenario(Config, fun execute_order_fulfillment_workflows/1).

%% @doc Approval workflow simulation - multi-level approval process.
-spec approval_workflow_simulation(map()) -> #scenario_result{}.
approval_workflow_simulation(Opts) ->
    Config = #scenario_config{
        name = <<"approval_workflow_simulation">>,
        patterns = [sequence, exclusive_choice, synchronization],
        workflow_count = maps:get(workflow_count, Opts, 80),
        duration_seconds = maps:get(duration_seconds, Opts, 60),
        concurrency_level = maps:get(concurrency_level, Opts, 10),
        throttle_ms = maps:get(throttle_ms, Opts, 150),
        enable_profiling = maps:get(enable_profiling, Opts, false),
        enable_tracing = maps:get(enable_tracing, Opts, false)
    },

    execute_scenario(Config, fun execute_approval_workflows/1).

%% @doc Data pipeline simulation - ETL workflow pattern.
-spec data_pipeline_simulation(map()) -> #scenario_result{}.
data_pipeline_simulation(Opts) ->
    Config = #scenario_config{
        name = <<"data_pipeline_simulation">>,
        patterns = [sequence, parallel_split],
        workflow_count = maps:get(workflow_count, Opts, 50),
        duration_seconds = maps:get(duration_seconds, Opts, 90),
        concurrency_level = maps:get(concurrency_level, Opts, 8),
        throttle_ms = maps:get(throttle_ms, Opts, 200),
        enable_profiling = maps:get(enable_profiling, Opts, true),
        enable_tracing = maps:get(enable_tracing, Opts, false)
    },

    execute_scenario(Config, fun execute_data_pipeline_workflows/1).

%%%===================================================================
%%% Stress Test Scenarios
%%%===================================================================

%% @doc Extreme concurrency test - maximum concurrent workflows.
-spec extreme_concurrency_test() -> #scenario_result{}.
extreme_concurrency_test() ->
    Config = #scenario_config{
        name = <<"extreme_concurrency_test">>,
        patterns = [sequence, parallel_split],
        workflow_count = 2000,
        duration_seconds = 60,
        concurrency_level = 200,
        throttle_ms = 5,
        enable_profiling = true,
        enable_tracing = false
    },

    execute_scenario(Config, fun execute_extreme_concurrency/1).

%% @doc Memory pressure test - high memory utilization scenario.
-spec memory_pressure_test() -> #scenario_result{}.
memory_pressure_test() ->
    Config = #scenario_config{
        name = <<"memory_pressure_test">>,
        patterns = [parallel_split],
        workflow_count = 500,
        duration_seconds = 120,
        concurrency_level = 50,
        throttle_ms = 20,
        enable_profiling = true,
        enable_tracing = false
    },

    execute_scenario(Config, fun execute_memory_pressure/1).

%% @doc Scheduler saturation test - max out BEAM schedulers.
-spec scheduler_saturation_test() -> #scenario_result{}.
scheduler_saturation_test() ->
    Config = #scenario_config{
        name = <<"scheduler_saturation_test">>,
        patterns = [sequence],
        workflow_count = 1000,
        duration_seconds = 60,
        concurrency_level = 100,
        throttle_ms = 0,
        enable_profiling = true,
        enable_tracing = false
    },

    execute_scenario(Config, fun execute_scheduler_saturation/1).

%%%===================================================================
%%% Scenario Execution
%%%===================================================================

%% @doc Execute a scenario with configuration.
-spec execute_scenario(#scenario_config{}, fun()) -> #scenario_result{}.
execute_scenario(Config, ExecuteFun) ->
    #scenario_config{
        name = Name,
        enable_profiling = EnableProfiling
    } = Config,

    %% Setup profiling if enabled
    ProfilingState = case EnableProfiling of
        true -> start_profiling();
        false -> undefined
    end,

    StartTime = erlang:system_time(millisecond),
    BaselineMemory = element(2, lists:keyfind(total, 1, erlang:memory())),

    %% Execute scenario
    {SuccessCount, FailureCount, ExecutionTimes} = ExecuteFun(Config),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    FinalMemory = element(2, lists:keyfind(total, 1, erlang:memory())),

    %% Stop profiling and collect results
    ProfileData = case EnableProfiling of
        true -> stop_profiling(ProfilingState);
        false -> undefined
    end,

    %% Build result
    TotalExecuted = SuccessCount + FailureCount,
    AvgExecutionTime = case ExecutionTimes of
        [] -> 0.0;
        _ -> lists:sum(ExecutionTimes) / length(ExecutionTimes)
    end,

    #scenario_result{
        name = Name,
        duration_ms = Duration,
        workflows_executed = TotalExecuted,
        success_count = SuccessCount,
        failure_count = FailureCount,
        avg_execution_time_ms = AvgExecutionTime,
        throughput = (TotalExecuted / Duration) * 1000.0,
        memory_stats = #{
            baseline_bytes => BaselineMemory,
            final_bytes => FinalMemory,
            delta_bytes => FinalMemory - BaselineMemory,
            delta_mb => (FinalMemory - BaselineMemory) / (1024 * 1024)
        },
        profile_data = ProfileData
    }.

%%%===================================================================
%%% Workflow Execution Functions
%%%===================================================================

%% @doc Execute mixed pattern workflows.
-spec execute_mixed_pattern_workflows(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_mixed_pattern_workflows(Config) ->
    #scenario_config{
        patterns = Patterns,
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent(Patterns, Count, Concurrency, Throttle,
                                fun execute_random_pattern/1).

%% @doc Execute nested workflows.
-spec execute_nested_workflows(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_nested_workflows(Config) ->
    #scenario_config{
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent([sequence], Count, Concurrency, Throttle,
                                fun execute_nested_pattern/1).

%% @doc Execute long-running workflows.
-spec execute_long_running_workflows(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_long_running_workflows(Config) ->
    #scenario_config{
        patterns = Patterns,
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent(Patterns, Count, Concurrency, Throttle,
                                fun execute_long_running_pattern/1).

%% @doc Execute rapid short workflows.
-spec execute_rapid_workflows(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_rapid_workflows(Config) ->
    #scenario_config{
        patterns = Patterns,
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent(Patterns, Count, Concurrency, Throttle,
                                fun execute_rapid_pattern/1).

%% @doc Execute order fulfillment workflows.
-spec execute_order_fulfillment_workflows(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_order_fulfillment_workflows(Config) ->
    #scenario_config{
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent([sequence], Count, Concurrency, Throttle,
                                fun execute_order_fulfillment_pattern/1).

%% @doc Execute approval workflows.
-spec execute_approval_workflows(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_approval_workflows(Config) ->
    #scenario_config{
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent([sequence], Count, Concurrency, Throttle,
                                fun execute_approval_pattern/1).

%% @doc Execute data pipeline workflows.
-spec execute_data_pipeline_workflows(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_data_pipeline_workflows(Config) ->
    #scenario_config{
        patterns = Patterns,
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent(Patterns, Count, Concurrency, Throttle,
                                fun execute_data_pipeline_pattern/1).

%% @doc Execute extreme concurrency workflows.
-spec execute_extreme_concurrency(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_extreme_concurrency(Config) ->
    execute_mixed_pattern_workflows(Config).

%% @doc Execute memory pressure workflows.
-spec execute_memory_pressure(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_memory_pressure(Config) ->
    #scenario_config{
        workflow_count = Count,
        concurrency_level = Concurrency,
        throttle_ms = Throttle
    } = Config,

    execute_workflows_concurrent([parallel_split], Count, Concurrency, Throttle,
                                fun execute_memory_intensive_pattern/1).

%% @doc Execute scheduler saturation workflows.
-spec execute_scheduler_saturation(#scenario_config{}) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_scheduler_saturation(Config) ->
    execute_rapid_workflows(Config).

%%%===================================================================
%%% Concurrent Workflow Execution
%%%===================================================================

%% @doc Execute workflows concurrently with throttling.
-spec execute_workflows_concurrent([atom()], pos_integer(), pos_integer(),
                                  non_neg_integer(), fun()) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
execute_workflows_concurrent(Patterns, Count, Concurrency, Throttle, ExecFun) ->
    Parent = self(),

    %% Spawn worker pool
    Workers = spawn_worker_pool(Concurrency, Parent, Patterns, ExecFun),

    %% Distribute work
    distribute_work(Workers, Count, Throttle),

    %% Wait for completion
    collect_worker_results(Workers, 0, 0, []).

%% @doc Spawn worker pool.
-spec spawn_worker_pool(pos_integer(), pid(), [atom()], fun()) -> [pid()].
spawn_worker_pool(Count, Parent, Patterns, ExecFun) ->
    [spawn_worker(Parent, Patterns, ExecFun) || _ <- lists:seq(1, Count)].

%% @doc Spawn a single worker.
-spec spawn_worker(pid(), [atom()], fun()) -> pid().
spawn_worker(Parent, Patterns, ExecFun) ->
    spawn_link(fun() -> worker_loop(Parent, Patterns, ExecFun, []) end).

%% @doc Worker loop.
-spec worker_loop(pid(), [atom()], fun(), [float()]) -> ok.
worker_loop(Parent, Patterns, ExecFun, ExecutionTimes) ->
    receive
        {work, Pattern} ->
            StartTime = erlang:system_time(millisecond),
            Result = try
                ExecFun(Pattern),
                ok
            catch
                _:_ -> error
            end,
            EndTime = erlang:system_time(millisecond),
            ExecutionTime = EndTime - StartTime,

            Parent ! {worker_result, self(), Result, ExecutionTime},
            worker_loop(Parent, Patterns, ExecFun, [ExecutionTime | ExecutionTimes]);

        stop ->
            Parent ! {worker_done, self(), ExecutionTimes},
            ok
    end.

%% @doc Distribute work to workers.
-spec distribute_work([pid()], pos_integer(), non_neg_integer()) -> ok.
distribute_work(_Workers, 0, _Throttle) ->
    ok;
distribute_work(Workers, Count, Throttle) ->
    WorkerIdx = (Count rem length(Workers)) + 1,
    Worker = lists:nth(WorkerIdx, Workers),

    %% Randomly select pattern (simplified - could be weighted)
    Pattern = sequence,
    Worker ! {work, Pattern},

    case Throttle of
        0 -> ok;
        _ -> timer:sleep(Throttle)
    end,

    distribute_work(Workers, Count - 1, Throttle).

%% @doc Collect worker results.
-spec collect_worker_results([pid()], non_neg_integer(), non_neg_integer(),
                            [float()]) ->
    {non_neg_integer(), non_neg_integer(), [float()]}.
collect_worker_results([], SuccessCount, FailureCount, ExecutionTimes) ->
    {SuccessCount, FailureCount, ExecutionTimes};
collect_worker_results(Workers, SuccessCount, FailureCount, ExecutionTimes) ->
    receive
        {worker_result, _Worker, ok, ExecutionTime} ->
            collect_worker_results(Workers, SuccessCount + 1, FailureCount,
                                 [ExecutionTime | ExecutionTimes]);

        {worker_result, _Worker, error, ExecutionTime} ->
            collect_worker_results(Workers, SuccessCount, FailureCount + 1,
                                 [ExecutionTime | ExecutionTimes]);

        {worker_done, Worker, WorkerTimes} ->
            RemainingWorkers = lists:delete(Worker, Workers),
            collect_worker_results(RemainingWorkers, SuccessCount, FailureCount,
                                 WorkerTimes ++ ExecutionTimes)
    after 60000 ->
        %% Timeout - stop remaining workers
        [Worker ! stop || Worker <- Workers],
        {SuccessCount, FailureCount, ExecutionTimes}
    end.

%%%===================================================================
%%% Pattern Execution Functions
%%%===================================================================

%% @doc Execute random pattern.
-spec execute_random_pattern(atom()) -> ok.
execute_random_pattern(Pattern) ->
    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [start_token]}),
    ok = gen_pnet:step(Pid),
    timer:sleep(10),
    gen_pnet:stop(Pid),
    ok.

%% @doc Execute nested pattern (workflow within workflow).
-spec execute_nested_pattern(atom()) -> ok.
execute_nested_pattern(Pattern) ->
    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [start_token]}),
    ok = gen_pnet:step(Pid),
    timer:sleep(20),
    ok = gen_pnet:step(Pid),
    gen_pnet:stop(Pid),
    ok.

%% @doc Execute long-running pattern.
-spec execute_long_running_pattern(atom()) -> ok.
execute_long_running_pattern(Pattern) ->
    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [start_token]}),

    %% Simulate long-running work
    ok = gen_pnet:step(Pid),
    timer:sleep(100),
    ok = gen_pnet:step(Pid),
    timer:sleep(100),

    gen_pnet:stop(Pid),
    ok.

%% @doc Execute rapid pattern (minimal work).
-spec execute_rapid_pattern(atom()) -> ok.
execute_rapid_pattern(Pattern) ->
    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [start_token]}),
    ok = gen_pnet:step(Pid),
    gen_pnet:stop(Pid),
    ok.

%% @doc Execute order fulfillment pattern.
-spec execute_order_fulfillment_pattern(atom()) -> ok.
execute_order_fulfillment_pattern(Pattern) ->
    %% Simulate: Receive Order -> Validate -> Process Payment -> Ship
    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [order_received]}),
    ok = gen_pnet:step(Pid),
    timer:sleep(30),
    ok = gen_pnet:step(Pid),
    timer:sleep(30),
    gen_pnet:stop(Pid),
    ok.

%% @doc Execute approval pattern.
-spec execute_approval_pattern(atom()) -> ok.
execute_approval_pattern(Pattern) ->
    %% Simulate: Submit -> Manager Approval -> Director Approval -> Execute
    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [request_submitted]}),
    ok = gen_pnet:step(Pid),
    timer:sleep(50),
    ok = gen_pnet:step(Pid),
    timer:sleep(50),
    gen_pnet:stop(Pid),
    ok.

%% @doc Execute data pipeline pattern.
-spec execute_data_pipeline_pattern(atom()) -> ok.
execute_data_pipeline_pattern(Pattern) ->
    %% Simulate: Extract -> Transform -> Load
    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [data_batch]}),
    ok = gen_pnet:step(Pid),
    timer:sleep(80),
    ok = gen_pnet:step(Pid),
    gen_pnet:stop(Pid),
    ok.

%% @doc Execute memory intensive pattern.
-spec execute_memory_intensive_pattern(atom()) -> ok.
execute_memory_intensive_pattern(Pattern) ->
    %% Create large data structures
    _LargeData = lists:duplicate(1000, {data, lists:seq(1, 100)}),

    {ok, Pid} = gen_pnet:start_link(Pattern, Pattern, #{}),
    ok = gen_pnet:inject(Pid, #{p_start => [start_token]}),
    ok = gen_pnet:step(Pid),
    timer:sleep(50),
    gen_pnet:stop(Pid),
    ok.

%%%===================================================================
%%% Profiling Functions
%%%===================================================================

%% @doc Start profiling.
-spec start_profiling() -> #profiling_state{}.
start_profiling() ->
    StartTime = erlang:system_time(millisecond),
    BaselineMemory = element(2, lists:keyfind(total, 1, erlang:memory())),

    %% Enable scheduler wall time statistics
    erlang:system_flag(scheduler_wall_time, true),

    #profiling_state{
        start_time = StartTime,
        baseline_memory = BaselineMemory,
        snapshots = [],
        trace_enabled = false
    }.

%% @doc Stop profiling.
-spec stop_profiling(#profiling_state{}) -> map().
stop_profiling(State) ->
    #profiling_state{
        start_time = StartTime,
        baseline_memory = BaselineMemory
    } = State,

    EndTime = erlang:system_time(millisecond),
    FinalMemory = element(2, lists:keyfind(total, 1, erlang:memory())),

    #{
        duration_ms => EndTime - StartTime,
        memory_delta_bytes => FinalMemory - BaselineMemory,
        memory_delta_mb => (FinalMemory - BaselineMemory) / (1024 * 1024)
    }.

%% @doc Get profiling results.
-spec get_profiling_results(#profiling_state{}) -> map().
get_profiling_results(State) ->
    stop_profiling(State).

%% @doc Profile workflow execution.
-spec profile_workflow_execution(atom(), pos_integer()) -> map().
profile_workflow_execution(Pattern, Iterations) ->
    ProfilingState = start_profiling(),

    %% Execute workflow multiple times
    [execute_random_pattern(Pattern) || _ <- lists:seq(1, Iterations)],

    stop_profiling(ProfilingState).

%% @doc Detect memory leaks.
-spec detect_memory_leaks(atom(), pos_integer()) -> boolean().
detect_memory_leaks(Pattern, Iterations) ->
    InitialMemory = element(2, lists:keyfind(total, 1, erlang:memory())),

    %% Run workflows and force GC
    [begin
         execute_random_pattern(Pattern),
         erlang:garbage_collect()
     end || _ <- lists:seq(1, Iterations)],

    timer:sleep(1000),
    erlang:garbage_collect(),

    FinalMemory = element(2, lists:keyfind(total, 1, erlang:memory())),
    MemoryGrowth = (FinalMemory - InitialMemory) / (1024 * 1024),

    %% Threshold: More than 10 MB growth suggests leak
    MemoryGrowth > 10.0.

%% @doc Measure garbage collection impact.
-spec measure_gc_impact(pos_integer()) -> map().
measure_gc_impact(Iterations) ->
    {GcCount1, WordsReclaimed1, _} = erlang:statistics(garbage_collection),

    %% Execute workflows
    [execute_random_pattern(sequence) || _ <- lists:seq(1, Iterations)],

    {GcCount2, WordsReclaimed2, _} = erlang:statistics(garbage_collection),

    #{
        gc_count => GcCount2 - GcCount1,
        words_reclaimed => WordsReclaimed2 - WordsReclaimed1,
        mb_reclaimed => ((WordsReclaimed2 - WordsReclaimed1) * 8) / (1024 * 1024)
    }.
