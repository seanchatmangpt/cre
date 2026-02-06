%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author YAWL Simulation Implementation
%% @copyright 2025
%%
%% @doc YAWL Workflow Simulation Module for CRE
%%
%% This module implements workflow simulation capabilities including
%% Monte Carlo analysis, what-if scenarios, and bottleneck detection.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Simulation:</b> Run workflow simulations with case data</li>
%%   <li><b>Monte Carlo:</b> Probabilistic simulation for risk analysis</li>
%%   <li><b>What-If Analysis:</b> Compare different scenarios</li>
%%   <li><b>Bottleneck Detection:</b> Identify workflow constraints</li>
%%   <li><b>Resource Utilization:</b> Analyze resource usage patterns</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% <pre>
%% %% Run a basic simulation
%% Config = #simulation_config{iterations = 100, ...},
%% {ok, Result} = yawl_simulation:run_simulation(Workflow, Config).
%%
%% %% Run Monte Carlo simulation
%% {ok, MCResult} = yawl_simulation:monte_carlo_simulation(Workflow, 1000, Config).
%%
%% %% Analyze bottlenecks
%% Bottlenecks = yawl_simulation:bottleneck_analysis(Workflow).
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_simulation).

%%====================================================================
%% Exports
%%====================================================================

%% Simulation API
-export([run_simulation/2,
         run_simulation/3,
         monte_carlo_simulation/3,
         what_if_analysis/2,
         bottleneck_analysis/1,
         compare_scenarios/2,
         resource_utilization/1,
         predict_completion_time/2]).

%% Result analysis
-export([get_simulation_stats/1,
         get_confidence_interval/2,
         get_percentile/2,
         simulate_approval_delay/1,
         get_approval_delay_stats/1]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").
-include("yawl_simulation.hrl").

%%====================================================================
%% gen_server callbacks
%%====================================================================

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type workflow() :: term().
-type case_data() :: map().
-type resource_id() :: binary().
-type task_id() :: binary().

-type simulation_config() :: #simulation_config{}.
-type simulation_result() :: #simulation_result{}.
-type monte_carlo_result() :: #monte_carlo_result{}.
-type scenario() :: #scenario{}.
-type bottleneck() :: #bottleneck{}.

-export_type([
    workflow/0,
    case_data/0,
    resource_id/0,
    task_id/0,
    simulation_config/0,
    simulation_result/0,
    monte_carlo_result/0,
    scenario/0,
    bottleneck/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs a workflow simulation with default configuration.
%%
%% @param Workflow The workflow specification to simulate.
%% @param Config The simulation configuration.
%% @return {ok, SimulationResult}
%%
%% @end
%%--------------------------------------------------------------------
-spec run_simulation(Workflow :: workflow(),
                    Config :: simulation_config()) ->
          {ok, simulation_result()}.

run_simulation(Workflow, Config) ->
    run_simulation(Workflow, Config, #{}).

%%--------------------------------------------------------------------
%% @doc Runs a workflow simulation with additional options.
%%
%% @param Workflow The workflow specification.
%% @param Config The simulation configuration.
%% @param Options Additional simulation options.
%% @return {ok, SimulationResult}
%%
%% @end
%%--------------------------------------------------------------------
-spec run_simulation(Workflow :: workflow(),
                    Config :: simulation_config(),
                    Options :: map()) ->
          {ok, simulation_result()}.

run_simulation(Workflow, #simulation_config{iterations = N} = Config, Options) ->
    %% Set random seed if specified
    maybe_set_random_seed(Config#simulation_config.random_seed),

    %% Run N simulation iterations
    Results = run_iterations(Workflow, Config, N, [], Options),

    %% Calculate statistics
    CycleTimes = [R || {cycle_time, R} <- Results],
    CompletedCount = length([ok || {status, ok} <- Results]),
    FailedCount = length([error || {status, error} <- Results]),

    AvgCycleTime = case CycleTimes of
        [] -> 0;
        _ -> lists:sum(CycleTimes) / length(CycleTimes)
    end,

    %% Calculate approval delay statistics
    ApprovalDelays = calculate_approval_delays(Results),
    TotalApprovalWaitTime = calculate_total_approval_wait_time(ApprovalDelays),

    #{
        total_cases => N,
        completed_cases => CompletedCount,
        failed_cases => FailedCount,
        average_cycle_time => AvgCycleTime,
        min_cycle_time => case CycleTimes of [] -> 0; _ -> lists:min(CycleTimes) end,
        max_cycle_time => case CycleTimes of [] -> 0; _ -> lists:max(CycleTimes) end,
        cycle_time_stddev => calculate_stddev(CycleTimes, AvgCycleTime),
        bottleneck_tasks => identify_bottlenecks(Results),
        resource_utilization => calculate_utilization(Results),
        task_completion_rates => calculate_completion_rates(Results),
        timestamps => [erlang:system_time(millisecond) || _ <- lists:seq(1, N)],
        approval_delays => ApprovalDelays,
        total_approval_wait_time => TotalApprovalWaitTime
    }.

%%--------------------------------------------------------------------
%% @doc Runs Monte Carlo simulation with probabilistic analysis.
%%
%% @param Workflow The workflow specification.
%% @param Iterations Number of simulation iterations.
%% @param Config The simulation configuration.
%% @return {ok, MonteCarloResult}
%%
%% @end
%%--------------------------------------------------------------------
-spec monte_carlo_simulation(Workflow :: workflow(),
                            Iterations :: pos_integer(),
                            Config :: simulation_config()) ->
          {ok, monte_carlo_result()}.

monte_carlo_simulation(Workflow, Iterations, Config) ->
    %% Run simulations
    Results = run_iterations(Workflow, Config, Iterations, [], #{}),

    %% Extract cycle times
    CycleTimes = [R || {cycle_time, R} <- Results],

    %% Calculate statistics
    SortedTimes = lists:sort(CycleTimes),
    Count = length(SortedTimes),
    Mean = case Count of
        0 -> 0;
        _ -> lists:sum(SortedTimes) / Count
    end,

    Median = percentile(SortedTimes, 50),

    %% Calculate percentiles
    P50 = percentile(SortedTimes, 50),
    P90 = percentile(SortedTimes, 90),
    P95 = percentile(SortedTimes, 95),
    P99 = percentile(SortedTimes, 99),

    %% Calculate 95% confidence interval
    StdErr = case Count of
        0 -> 0;
        _ -> calculate_stddev(CycleTimes, Mean) / math:sqrt(Count)
    end,
    CI95 = {Mean - 1.96 * StdErr, Mean + 1.96 * StdErr},

    %% Build probability distribution
    Distribution = build_distribution(CycleTimes),

    %% Calculate risk factors
    RiskFactors = calculate_risk_factors(CycleTimes, Results),

    {ok, #monte_carlo_result{
        iterations = Iterations,
        mean_cycle_time = Mean,
        median_cycle_time = Median,
        percentile_50 = P50,
        percentile_90 = P90,
        percentile_95 = P95,
        percentile_99 = P99,
        confidence_interval_95 = CI95,
        probability_distribution = Distribution,
        risk_factors = RiskFactors
    }}.

%%--------------------------------------------------------------------
%% @doc Performs what-if analysis comparing scenarios.
%%
%% @param Scenarios List of scenarios to compare.
%% @param Metric The metric to compare (cycle_time, cost, etc.).
%% @return Comparison results.
%%
%% @end
%%--------------------------------------------------------------------
-spec what_if_analysis(Scenarios :: [scenario()], Metric :: atom()) ->
          #{binary() => number()}.

what_if_analysis(Scenarios, Metric) ->
    %% Simulate each scenario
    Results = lists:map(
        fun(#scenario{name = Name, workflow = Workflow, config = Config}) ->
            Result = run_simulation(Workflow, Config),
            Value = case Metric of
                cycle_time -> maps:get(average_cycle_time, Result, 0);
                completion_rate ->
                    Total = maps:get(total_cases, Result, 1),
                    Completed = maps:get(completed_cases, Result, 0),
                    case Total of
                        0 -> 0;
                        _ -> Completed / Total
                    end;
                cost -> estimate_simulation_cost(Result);
                _ -> 0
            end,
            {Name, Value}
        end,
        Scenarios
    ),

    maps:from_list(Results).

%%--------------------------------------------------------------------
%% @doc Analyzes workflow for potential bottlenecks.
%%
%% @param Workflow The workflow specification.
%% @return List of identified bottlenecks.
%%
%% @end
%%--------------------------------------------------------------------
-spec bottleneck_analysis(Workflow :: workflow()) -> [bottleneck()].

bottleneck_analysis(Workflow) ->
    %% Extract task information from workflow
    Tasks = extract_tasks(Workflow),

    %% Analyze each task for bottlenecks
    lists:filtermap(
        fun(TaskId) ->
            case analyze_task_bottleneck(TaskId, Tasks) of
                #bottleneck{severity = critical} = B ->
                    {true, B};
                #bottleneck{severity = high} = B ->
                    {true, B};
                #bottleneck{severity = medium} = B ->
                    {true, B};
                #bottleneck{severity = low} ->
                    false
            end
        end,
        maps:keys(Tasks)
    ).

%%--------------------------------------------------------------------
%% @doc Compares multiple scenarios.
%%
%% @param Scenarios List of scenarios to compare.
%% @param Configs List of configurations for each scenario.
%% @return Comparison results.
%%
%% @end
%%--------------------------------------------------------------------
-spec compare_scenarios(Scenarios :: [{binary(), workflow()}],
                       Configs :: [simulation_config()]) ->
          #{binary() => simulation_result()}.

compare_scenarios(Scenarios, Configs) when length(Scenarios) =:= length(Configs) ->
    lists:foldl(
        fun({{Name, Workflow}, Config}, Acc) ->
            {ok, Result} = run_simulation(Workflow, Config),
            maps:put(Name, Result, Acc)
        end,
        #{},
        lists:zip(Scenarios, Configs)
    ).

%%--------------------------------------------------------------------
%% @doc Calculates resource utilization for a workflow.
%%
%% @param Workflow The workflow specification.
%% @return Resource utilization map.
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_utilization(Workflow :: workflow()) ->
          #{resource_id() => number()}.

resource_utilization(Workflow) ->
    Tasks = extract_tasks(Workflow),
    Resources = extract_resources(Tasks),

    %% Calculate utilization for each resource
    maps:map(
        fun(_ResourceId, _ResourceInfo) ->
            %% Default utilization - would be calculated from actual
            %% workflow execution data
            rand:uniform() * 100
        end,
        Resources
    ).

%%--------------------------------------------------------------------
%% @doc Predicts completion time for a workflow with given case data.
%%
%% @param Workflow The workflow specification.
%% @param CaseData The case data for prediction.
%% @return Predicted completion time in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec predict_completion_time(Workflow :: workflow(), CaseData :: case_data()) ->
          {ok, number()} | {error, term()}.

predict_completion_time(Workflow, CaseData) ->
    %% Extract task durations from case data
    TaskDurations = maps:get(task_durations, CaseData, #{}),

    %% Calculate base prediction
    BaseTime = maps:fold(
        fun(_TaskId, Duration, Acc) ->
            Acc + Duration
        end,
        0,
        TaskDurations
    ),

    %% Add overhead for parallel/split tasks
    Overhead = calculate_overhead(Workflow),

    {ok, BaseTime * (1 + Overhead)}.

%%--------------------------------------------------------------------
%% @doc Gets statistics from simulation results.
%%
%% @param Result The simulation result.
%% @return Statistics map.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_simulation_stats(simulation_result()) -> map().

get_simulation_stats(Result) ->
    #{
        total_cases => Result#simulation_result.total_cases,
        completion_rate => Result#simulation_result.completed_cases /
                           Result#simulation_result.total_cases,
        average_cycle_time => Result#simulation_result.average_cycle_time,
        min_cycle_time => Result#simulation_result.min_cycle_time,
        max_cycle_time => Result#simulation_result.max_cycle_time,
        cycle_time_stddev => Result#simulation_result.cycle_time_stddev,
        bottleneck_count => length(Result#simulation_result.bottleneck_tasks)
    }.

%%--------------------------------------------------------------------
%% @doc Gets confidence interval for simulation results.
%%
%% @param Results List of cycle times from simulations.
%% @param ConfidenceLevel The confidence level (e.g., 95 for 95%).
%% @return {LowerBound, UpperBound}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_confidence_interval([number()], float()) -> {number(), number()}.

get_confidence_interval([], _ConfidenceLevel) ->
    {0.0, 0.0};
get_confidence_interval(Results, ConfidenceLevel) ->
    N = length(Results),
    Mean = lists:sum(Results) / N,
    StdDev = calculate_stddev(Results, Mean),
    StdErr = StdDev / math:sqrt(N),

    %% Z-score for confidence level
    ZScore = case ConfidenceLevel of
        90.0 -> 1.645;
        95.0 -> 1.96;
        99.0 -> 2.576;
        _ -> 1.96
    end,

    {Mean - ZScore * StdErr, Mean + ZScore * StdErr}.

%%--------------------------------------------------------------------
%% @doc Gets percentile value from simulation results.
%%
%% @param Results List of cycle times.
%% @param Percentile The percentile to calculate (0-100).
%% @return The percentile value.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_percentile([number()], float()) -> number().

get_percentile(Results, Percentile) when length(Results) > 0 ->
    percentile(lists:sort(Results), Percentile);
get_percentile([], _Percentile) ->
    0.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Runs multiple simulation iterations.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_iterations(workflow(), simulation_config(), pos_integer(),
                    [term()], map()) -> [term()].

run_iterations(_Workflow, _Config, 0, Acc, _Options) ->
    lists:reverse(Acc);
run_iterations(Workflow, Config, N, Acc, Options) ->
    %% Simulate a single case execution
    Result = simulate_case(Workflow, Config, Options),

    %% Record metrics to monitor if available
    try
        CaseId = generate_case_id(),
        record_metrics(Result, CaseId)
    catch
        _:_ -> ok
    end,

    run_iterations(Workflow, Config, N - 1, [Result | Acc], Options).

%%--------------------------------------------------------------------
%% @private
%% @doc Simulates a single case execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec simulate_case(workflow(), simulation_config(), map()) -> map().

simulate_case(Workflow, Config, Options) ->
    Start = erlang:system_time(millisecond),

    %% Simulate workflow execution
    TaskResults = simulate_workflow_tasks(Workflow, Config, Options),

    End = erlang:system_time(millisecond),
    CycleTime = End - Start,

    %% Check if any tasks failed
    Status = case lists:member(failed, [maps:get(status, T, ok) || T <- TaskResults]) of
        true -> error;
        false -> ok
    end,

    #{
        status => Status,
        cycle_time => CycleTime,
        task_results => TaskResults,
        timestamps => #{
            start => Start,
            'end' => End
        }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Simulates execution of workflow tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec simulate_workflow_tasks(workflow(), simulation_config(), map()) -> [map()].

simulate_workflow_tasks(Workflow, Config, Options) ->
    Tasks = extract_tasks(Workflow),

    lists:map(
        fun({TaskId, TaskInfo}) ->
            simulate_task(TaskId, TaskInfo, Config, Options)
        end,
        maps:to_list(Tasks)
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Simulates execution of a single task.
%%
%% @end
%%--------------------------------------------------------------------
-spec simulate_task(task_id(), map(), simulation_config(), map()) -> map().

simulate_task(TaskId, TaskInfo, Config, _Options) ->
    %% Get task duration (with random variation)
    BaseDuration = maps:get(duration, TaskInfo, 100),
    Variation = maps:get(duration_variation, TaskInfo, 0.2),

    Duration = BaseDuration * (1 + (rand:uniform() - 0.5) * 2 * Variation),

    %% Check if task requires approval
    RequiresApproval = requires_approval(TaskId, TaskInfo, Config),
    ApprovalDelay = case RequiresApproval of
        true -> simulate_approval_delay(Config);
        false -> 0
    end,

    %% Simulate task execution (including approval delay)
    TaskSleepTime = max(0, trunc(Duration / 10)), %% Scale down for simulation
    ApprovalSleepTime = max(0, trunc(ApprovalDelay / 100)), %% Scale down for simulation
    timer:sleep(TaskSleepTime + ApprovalSleepTime),

    %% Check resource constraints
    ResourceConstrained = check_resource_constraints(TaskId, Config),

    #{
        task_id => TaskId,
        status => case ResourceConstrained of
            true -> failed;
            false -> ok
        end,
        duration => Duration,
        approval_delay => ApprovalDelay,
        required_approval => RequiresApproval,
        timestamp => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts tasks from workflow specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_tasks(workflow()) -> #{task_id() => map()}.

extract_tasks(Workflow) when is_map(Workflow) ->
    case maps:get(<<"tasks">>, Workflow, undefined) of
        undefined ->
            case maps:get(tasks, Workflow, undefined) of
                undefined -> #{};
                Tasks -> Tasks
            end;
        Tasks -> Tasks
    end;
extract_tasks(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts resources from task map.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_resources(#{task_id() => map()}) -> #{resource_id() => map()}.

extract_resources(Tasks) ->
    lists:foldl(
        fun({_TaskId, TaskInfo}, Acc) ->
            case maps:get(<<"resource_id">>, TaskInfo,
                         maps:get(resource_id, TaskInfo, undefined)) of
                undefined -> Acc;
                ResourceId ->
                    maps:put(ResourceId, #{}, Acc)
            end
        end,
        #{},
        maps:to_list(Tasks)
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if task is resource constrained.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_resource_constraints(task_id(), simulation_config()) -> boolean().

check_resource_constraints(TaskId, Config) ->
    Constraints = Config#simulation_config.resource_constraints,
    ResourceId = case is_atom(TaskId) of
        true -> list_to_binary([atom_to_list(TaskId), "_resource"]);
        false when is_binary(TaskId) -> <<TaskId/binary, "_resource">>
    end,

    case maps:get(ResourceId, Constraints, undefined) of
        undefined -> false;
        MaxCapacity ->
            %% Simulate resource usage
            rand:uniform() > MaxCapacity
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates standard deviation.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_stddev([number()], number()) -> number().

calculate_stddev([], _Mean) -> 0;
calculate_stddev(Values, Mean) ->
    Variance = lists:foldl(
        fun(V, Acc) ->
            Diff = V - Mean,
            Acc + Diff * Diff
        end,
        0,
        Values
    ) / length(Values),
    math:sqrt(Variance).

%%--------------------------------------------------------------------
%% @private
%% @doc Identifies bottlenecks from simulation results.
%%
%% @end
%%--------------------------------------------------------------------
-spec identify_bottlenecks([term()]) -> [task_id()].

identify_bottlenecks(Results) ->
    %% Aggregate task durations
    TaskDurations = lists:foldl(
        fun(Result, Acc) ->
            TaskResults = maps:get(task_results, Result, []),
            lists:foldl(
                fun(Task, TaskAcc) ->
                    TaskId = maps:get(task_id, Task, <<>>),
                    Duration = maps:get(duration, Task, 0),
                    maps:update_with(TaskId,
                        fun(Existing) -> [Duration | Existing] end,
                        [Duration],
                        TaskAcc)
                end,
                Acc,
                TaskResults)
        end,
        #{},
        Results
    ),

    %% Find tasks with high average duration
    maps:fold(
        fun(TaskId, Durations, Acc) ->
            AvgDuration = lists:sum(Durations) / length(Durations),
            case AvgDuration > 1000 of %% Threshold: 1 second
                true -> [TaskId | Acc];
                false -> Acc
            end
        end,
        [],
        TaskDurations
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates resource utilization from results.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_utilization([term()]) -> #{resource_id() => number()}.

calculate_utilization(_Results) ->
    %% Simplified utilization calculation
    #{
        <<"cpu">> => rand:uniform() * 100,
        <<"memory">> => rand:uniform() * 100,
        <<"io">> => rand:uniform() * 100
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates task completion rates.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_completion_rates([term()]) -> #{task_id() => float()}.

calculate_completion_rates(Results) ->
    TaskStats = lists:foldl(
        fun(Result, Acc) ->
            TaskResults = maps:get(task_results, Result, []),
            lists:foldl(
                fun(Task, TaskAcc) ->
                    TaskId = maps:get(task_id, Task, <<>>),
                    Status = maps:get(status, Task, ok),
                    maps:update_with(TaskId,
                        fun({Total, Completed}) ->
                            {Total + 1, case Status of ok -> Completed + 1; _ -> Completed end}
                        end,
                        {1, case Status of ok -> 1; _ -> 0 end},
                        TaskAcc)
                end,
                Acc,
                TaskResults)
        end,
        #{},
        Results
    ),

    maps:map(
        fun(_TaskId, {Total, Completed}) ->
            Completed / Total
        end,
        TaskStats
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates percentile from sorted list.
%%
%% @end
%%--------------------------------------------------------------------
-spec percentile([number()], float()) -> number().

percentile([], _P) -> 0;
percentile(Sorted, P) ->
    N = length(Sorted),
    Index = max(1, min(N, trunc((P / 100) * N))),
    lists:nth(Index, Sorted).

%%--------------------------------------------------------------------
%% @private
%% @doc Builds probability distribution from values.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_distribution([number()]) -> [{number(), number()}].

build_distribution([]) -> [];
build_distribution(Values) ->
    %% Create histogram with 10 bins
    Min = lists:min(Values),
    Max = lists:max(Values),
    BinSize = (Max - Min) / 10,

    Bins = lists:map(
        fun(I) ->
            BinStart = Min + I * BinSize,
            BinEnd = Min + (I + 1) * BinSize,
            Count = length([V || V <- Values, V >= BinStart, V < BinEnd]),
            Probability = Count / length(Values),
            {BinStart + BinSize / 2, Probability}
        end,
        lists:seq(0, 9)
    ),

    Bins.

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates risk factors from simulation results.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_risk_factors([number()], [term()]) -> #{atom() => number()}.

calculate_risk_factors(CycleTimes, Results) ->
    Mean = case CycleTimes of
        [] -> 0;
        _ -> lists:sum(CycleTimes) / length(CycleTimes)
    end,

    FailureCount = length([R || R <- Results, maps:get(status, R, ok) =:= error]),
    FailureRate = case Results of
        [] -> 0;
        _ -> FailureCount / length(Results)
    end,

    %% Coefficient of variation as risk indicator
    StdDev = calculate_stddev(CycleTimes, Mean),
    CV = case Mean of
        0 -> 0;
        _ -> StdDev / Mean
    end,

    %% lists:count/2 was removed in OTP 26; use length(lists:filter/2) instead
    OutlierCount = length(lists:filter(fun(T) -> T > Mean * 2 end, CycleTimes)),

    #{
        failure_rate => FailureRate,
        variability_risk => min(1.0, CV),
        time_risk => case Mean of
            0 -> 0;
            _ -> OutlierCount / length(CycleTimes)
        end
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Analyzes a specific task for bottlenecks.
%%
%% @end
%%--------------------------------------------------------------------
-spec analyze_task_bottleneck(task_id(), #{task_id() => map()}) -> bottleneck().

analyze_task_bottleneck(TaskId, Tasks) ->
    TaskInfo = maps:get(TaskId, Tasks, #{}),

    %% Calculate simulated metrics
    AvgProcessingTime = maps:get(avg_processing_time, TaskInfo, 100),
    AvgWaitTime = maps:get(avg_wait_time, TaskInfo, 10),
    Utilization = maps:get(utilization, TaskInfo, 0.5),

    %% Determine severity
    Severity = case {Utilization, AvgProcessingTime} of
        {U, _} when U > 0.9 -> critical;
        {U, _} when U > 0.8 -> high;
        {_, T} when T > 5000 -> high;
        {U, T} when U > 0.6 orelse T > 2000 -> medium;
        _ -> low
    end,

    %% Generate recommendations
    Recommendations = generate_recommendations(TaskId, Severity, Utilization),

    #bottleneck{
        task_id = TaskId,
        severity = Severity,
        avg_wait_time = AvgWaitTime,
        avg_processing_time = AvgProcessingTime,
        utilization = Utilization,
        recommendations = Recommendations
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates recommendations for bottleneck resolution.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_recommendations(task_id(), atom(), number()) -> [binary()].

generate_recommendations(TaskId, Severity, Utilization) ->
    BaseRecs = case Severity of
        critical -> [
            <<"URGENT: Task requires immediate optimization">>,
            <<"Consider increasing resource capacity">>,
            <<"Implement task parallelization">>
        ];
        high -> [
            <<"High utilization detected">>,
            <<"Review resource allocation">>,
            <<"Consider workload distribution">>
        ];
        medium -> [
            <<"Moderate utilization - monitor closely">>,
            <<"Consider optimization opportunities">>
        ];
        low -> []
    end,

    UtilRec = if
        Utilization > 0.85 -> [<<"Resource capacity may be insufficient">>];
        Utilization > 0.70 -> [<<"Consider adding standby resources">>];
        true -> []
    end,

    [<<TaskId/binary, ": ", R/binary>> || R <- BaseRecs ++ UtilRec].

%%--------------------------------------------------------------------
%% @private
%% @doc Estimates cost from simulation result.
%%
%% @end
%%--------------------------------------------------------------------
-spec estimate_simulation_cost(simulation_result()) -> number().

estimate_simulation_cost(Result) ->
    %% Simple cost estimation based on cycle time
    Result#simulation_result.average_cycle_time * 0.001.

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates overhead for workflow structure.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_overhead(workflow()) -> float().

calculate_overhead(Workflow) ->
    Tasks = extract_tasks(Workflow),
    TaskCount = maps:size(Tasks),

    %% Overhead factors
    BaseOverhead = 0.1,
    ParallelOverhead = case TaskCount of
        N when N > 10 -> 0.2;
        N when N > 5 -> 0.1;
        _ -> 0
    end,

    BaseOverhead + ParallelOverhead.

%%--------------------------------------------------------------------
%% @private
%% @doc Sets random seed if specified.
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_random_seed(undefined | {integer(), integer(), integer()}) -> ok.

maybe_set_random_seed(undefined) ->
    ok;
maybe_set_random_seed(Seed) ->
    rand:seed(exs1024, Seed),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Records simulation metrics to monitor.
%%
%% @end
%%--------------------------------------------------------------------
-spec record_metrics(term(), binary()) -> ok.

record_metrics(Result, CaseId) ->
    try
        CycleTime = maps:get(cycle_time, Result, 0),
        Status = maps:get(status, Result, ok),

        yawl_monitor:record_metric(
            <<"simulation_cycle_time">>,
            CycleTime,
            #{<<"case_id">> => CaseId, <<"status">> => Status}
        ),

        yawl_monitor:record_metric(
            <<"simulation_completed">>,
            1,
            #{<<"case_id">> => CaseId}
        )
    catch
        _:_ -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique case ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().

generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"sim_case_", Hex/binary>>.

%%====================================================================
%% Approval Delay Simulation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Simulates approval delay based on configuration.
%%
%% @param Config The simulation configuration containing approval delay settings.
%% @return Simulated delay in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec simulate_approval_delay(simulation_config()) -> number().

simulate_approval_delay(Config) ->
    ApprovalDelayConfig = Config#simulation_config.approval_delay_config,

    case ApprovalDelayConfig of
        undefined ->
            %% Default configuration if not specified
            simulate_approval_delay_default();
        #approval_delay_config{enabled = false} ->
            0;
        #approval_delay_config{enabled = true,
                              mean_delay = Mean,
                              stddev_delay = StdDev,
                              min_delay = MinDelay,
                              max_delay = MaxDelay,
                              approval_probability = Prob} ->
            %% Only simulate approval delay if probability check passes
            case rand:uniform() < Prob of
                false ->
                    0;
                true ->
                    %% Generate delay using normal distribution approximation
                    %% Box-Muller transform for normal distribution
                    U1 = rand:uniform(),
                    U2 = rand:uniform(),
                    Z = math:sqrt(-2.0 * math:log(U1)) * math:cos(2.0 * math:pi() * U2),
                    Delay = Mean + Z * StdDev,

                    %% Clamp to min/max bounds
                    ClampedDelay = max(MinDelay, min(MaxDelay, Delay)),
                    ClampedDelay
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Simulates approval delay with default configuration.
%%
%% @return Simulated delay in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec simulate_approval_delay_default() -> number().

simulate_approval_delay_default() ->
    %% Default: mean 5 minutes, stddev 2 minutes, 30% probability
    Mean = 300000,   %% 5 minutes in milliseconds
    StdDev = 120000, %% 2 minutes
    MinDelay = 60000, %% 1 minute minimum
    MaxDelay = 3600000, %% 1 hour maximum
    Prob = 0.3,     %% 30% probability of requiring approval

    case rand:uniform() < Prob of
        false ->
            0;
        true ->
            U1 = rand:uniform(),
            U2 = rand:uniform(),
            Z = math:sqrt(-2.0 * math:log(U1)) * math:cos(2.0 * math:pi() * U2),
            Delay = Mean + Z * StdDev,
            max(MinDelay, min(MaxDelay, Delay))
    end.

%%--------------------------------------------------------------------
%% @doc Gets approval delay statistics from simulation results.
%%
%% @param Results The simulation results map.
%% @return Approval delay statistics map.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_approval_delay_stats(map()) -> map().

get_approval_delay_stats(Results) when is_map(Results) ->
    ApprovalDelays = maps:get(approval_delays, Results, #{}),

    Stats = maps:fold(
        fun(TaskId, {AvgDelay, MaxDelay}, Acc) ->
            Acc#{
                TaskId => #{
                    avg_delay => AvgDelay,
                    max_delay => MaxDelay,
                    avg_delay_seconds => AvgDelay / 1000,
                    max_delay_seconds => MaxDelay / 1000
                }
            }
        end,
        #{},
        ApprovalDelays
    ),

    TotalWaitTime = maps:get(total_approval_wait_time, Results, 0),

    Stats#{
        total_wait_time => TotalWaitTime,
        total_wait_time_seconds => TotalWaitTime / 1000,
        avg_wait_time_per_case => case maps:size(ApprovalDelays) of
            0 -> 0;
            N -> TotalWaitTime / N
        end
    };

get_approval_delay_stats(_) ->
    #{}.

%%====================================================================
%% Internal Functions - Approval Support
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a task requires approval based on config and task info.
%%
%% @end
%%--------------------------------------------------------------------
-spec requires_approval(task_id(), map(), simulation_config()) -> boolean().

requires_approval(_TaskId, TaskInfo, Config) ->
    %% Check if task has explicit approval requirement
    case maps:get(<<"requires_approval">>, TaskInfo,
                 maps:get(requires_approval, TaskInfo, undefined)) of
        true -> true;
        false -> false;
        undefined ->
            %% Check approval delay config
            ApprovalDelayConfig = Config#simulation_config.approval_delay_config,
            case ApprovalDelayConfig of
                undefined -> false;
                #approval_delay_config{enabled = false} -> false;
                #approval_delay_config{approval_probability = Prob} ->
                    rand:uniform() < Prob
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates approval delay statistics from simulation results.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_approval_delays([term()]) -> #{task_id() => {number(), number()}}.

calculate_approval_delays(Results) ->
    %% Aggregate approval delays by task
    %% First pass: collect all delays and counts
    TaskDelayLists = lists:foldl(
        fun(Result, Acc) ->
            TaskResults = maps:get(task_results, Result, []),
            lists:foldl(
                fun(Task, TaskAcc) ->
                    TaskId = maps:get(task_id, Task, <<>>),
                    ApprovalDelay = maps:get(approval_delay, Task, 0),
                    RequiredApproval = maps:get(required_approval, Task, false),

                    case RequiredApproval andalso ApprovalDelay > 0 of
                        true ->
                            maps:update_with(TaskId,
                                fun({Delays, Max}) ->
                                    {[ApprovalDelay | Delays], max(Max, ApprovalDelay)}
                                end,
                                {[ApprovalDelay], ApprovalDelay},
                                TaskAcc);
                        false ->
                            TaskAcc
                    end
                end,
                Acc,
                TaskResults)
        end,
        #{},
        Results
    ),

    %% Second pass: calculate averages
    maps:map(
        fun(_TaskId, {DelayList, MaxDelay}) ->
            AvgDelay = lists:sum(DelayList) / length(DelayList),
            {AvgDelay, MaxDelay}
        end,
        TaskDelayLists
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates total approval wait time from delay statistics.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_total_approval_wait_time(#{task_id() => {number(), number()}}) -> number().

calculate_total_approval_wait_time(ApprovalDelays) ->
    maps:fold(
        fun(_TaskId, {AvgDelay, _MaxDelay}, Acc) ->
            Acc + AvgDelay
        end,
        0,
        ApprovalDelays
    ).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
    {ok, #{}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
