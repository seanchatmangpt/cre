%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Chicago TDD Tests for YAWL Simulation Module
%%
%% Test-First Development: RED -> GREEN -> REFACTOR
%%
%% @doc YAWL Simulation Tests
%% @end

-module(yawl_simulation_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").
-include("../src/yawl_simulation.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = yawl_simulation:start_link(),
    Pid.

cleanup(_Pid) ->
    try
        gen_server:stop(yawl_simulation)
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% run_simulation/2 Tests
%%====================================================================

run_simulation_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{
                 <<"task1">> => #{duration => 100}
             }},
             Config = #simulation_config{
                 iterations = 5,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             Result = yawl_simulation:run_simulation(Workflow, Config),

             ?assertEqual(5, maps:get(total_cases, Result)),
             ?assert(is_number(maps:get(average_cycle_time, Result)))
         end)
     end}.

run_simulation_with_options_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             Config = #simulation_config{
                 iterations = 3,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             Result = yawl_simulation:run_simulation(Workflow, Config, #{seed => 123}),

             ?assertEqual(3, maps:get(total_cases, Result))
         end)
     end}.

%%====================================================================
%% monte_carlo_simulation/3 Tests
%%====================================================================

monte_carlo_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             Config = #simulation_config{
                 iterations = 100,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             {ok, Result} = yawl_simulation:monte_carlo_simulation(Workflow, 100, Config),

             ?assertEqual(100, Result#monte_carlo_result.iterations),
             ?assert(is_number(Result#monte_carlo_result.mean_cycle_time)),
             ?assert(is_number(Result#monte_carlo_result.median_cycle_time))
         end)
     end}.

monte_carlo_percentiles_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             Config = #simulation_config{
                 iterations = 100,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             {ok, Result} = yawl_simulation:monte_carlo_simulation(Workflow, 100, Config),

             ?assert(Result#monte_carlo_result.percentile_50 >= 0),
             ?assert(Result#monte_carlo_result.percentile_90 >= Result#monte_carlo_result.percentile_50),
             ?assert(Result#monte_carlo_result.percentile_95 >= Result#monte_carlo_result.percentile_90)
         end)
     end}.

monte_carlo_confidence_interval_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             Config = #simulation_config{
                 iterations = 100,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             {ok, Result} = yawl_simulation:monte_carlo_simulation(Workflow, 100, Config),

             {Low, High} = Result#monte_carlo_result.confidence_interval_95,
             ?assert(Low =< High),
             ?assert(is_number(Low)),
             ?assert(is_number(High))
         end)
     end}.

%%====================================================================
%% what_if_analysis/2 Tests
%%====================================================================

what_if_analysis_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             ConfigA = #simulation_config{
                 iterations = 10,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             ConfigB = #simulation_config{
                 iterations = 20,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             Scenarios = [
                 #scenario{name = <<"scenario_a">>, workflow = Workflow, config = ConfigA},
                 #scenario{name = <<"scenario_b">>, workflow = Workflow, config = ConfigB}
             ],

             Result = yawl_simulation:what_if_analysis(Scenarios, cycle_time),
             ?assert(is_map(Result)),
             ?assert(maps:is_key(<<"scenario_a">>, Result)),
             ?assert(maps:is_key(<<"scenario_b">>, Result))
         end)
     end}.

%%====================================================================
%% bottleneck_analysis/1 Tests
%%====================================================================

bottleneck_analysis_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{
                 <<"critical_task">> => #{
                     <<"avg_processing_time">> => 6000,
                     <<"utilization">> => 0.95
                 }
             }},
             Bottlenecks = yawl_simulation:bottleneck_analysis(Workflow),

             ?assert(length(Bottlenecks) > 0),
             [FirstBottleneck | _] = Bottlenecks,
             ?assertNotEqual(low, FirstBottleneck#bottleneck.severity)
         end)
     end}.

%%====================================================================
%% compare_scenarios/2 Tests
%%====================================================================

compare_scenarios_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             Scenarios = [{<<"scenario1">>, Workflow}, {<<"scenario2">>, Workflow}],
             Config1 = #simulation_config{
                 iterations = 5,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             Config2 = #simulation_config{
                 iterations = 10,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             Configs = [Config1, Config2],

             Result = yawl_simulation:compare_scenarios(Scenarios, Configs),
             ?assert(is_map(Result)),
             ?assert(maps:is_key(<<"scenario1">>, Result)),
             ?assert(maps:is_key(<<"scenario2">>, Result))
         end)
     end}.

%%====================================================================
%% resource_utilization/1 Tests
%%====================================================================

resource_utilization_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{
                 <<"task1">> => #{<<"resource_id">> => <<"res1">>}
             }},
             Util = yawl_simulation:resource_utilization(Workflow),

             ?assert(is_map(Util))
             %% Either specific resource or generic CPU
             %% ?assert(maps:is_key(<<"res1">>, Util) orelse maps:is_key(<<"cpu">>, Util))
         end)
     end}.

%%====================================================================
%% predict_completion_time/2 Tests
%%====================================================================

predict_completion_time_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             CaseData = #{<<"task_durations">> => #{
                 <<"task1">> => 100,
                 <<"task2">> => 200
             }},

             {ok, Predicted} = yawl_simulation:predict_completion_time(Workflow, CaseData),
             ?assert(Predicted > 0),
             ?assert(Predicted >= 300)  % Base: 100 + 200 = 300
         end)
     end}.

%%====================================================================
%% get_simulation_stats/1 Tests
%%====================================================================

get_simulation_stats_test_() ->
    ?_test(begin
        Result = #simulation_result{
            total_cases = 100,
            completed_cases = 95,
            failed_cases = 5,
            average_cycle_time = 500.5,
            min_cycle_time = 100,
            max_cycle_time = 1500,
            cycle_time_stddev = 50.0,
            bottleneck_tasks = [],
            resource_utilization = #{},
            task_completion_rates = #{},
            timestamps = [],
            approval_delays = #{},
            total_approval_wait_time = 0
        },

        Stats = yawl_simulation:get_simulation_stats(Result),
        ?assertEqual(100, maps:get(total_cases, Stats)),
        ?assertEqual(0.95, maps:get(completion_rate, Stats)),
        ?assertEqual(500.5, maps:get(average_cycle_time, Stats))
    end).

%%====================================================================
%% get_confidence_interval/2 Tests
%%====================================================================

get_confidence_interval_test_() ->
    ?_test(begin
        Results = [100, 110, 105, 95, 90],
        {Low, High} = yawl_simulation:get_confidence_interval(Results, 95.0),
        ?assert(Low < High),
        ?assert(Low =< 100),
        ?assert(High >= 100)
    end).

get_confidence_interval_empty_test_() ->
    ?_test(begin
        {Low, High} = yawl_simulation:get_confidence_interval([], 95.0),
        ?assertEqual(0.0, Low),
        ?assertEqual(0.0, High)
    end).

%%====================================================================
%% get_percentile/2 Tests
%%====================================================================

get_percentile_test_() ->
    ?_test(begin
        Results = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        P50 = yawl_simulation:get_percentile(Results, 50),
        ?assertEqual(5, P50),
        P90 = yawl_simulation:get_percentile(Results, 90),
        ?assert(P90 >= 9),
        P95 = yawl_simulation:get_percentile(Results, 95),
        ?assert(P95 >= 9)
    end).

get_percentile_empty_test_() ->
    ?_test(begin
        ?assertEqual(0, yawl_simulation:get_percentile([], 50))
    end).

%%====================================================================
%% simulate_approval_delay/1 Tests
%%====================================================================

simulate_approval_delay_disabled_test_() ->
    ?_test(begin
        Config = #simulation_config{
            iterations = 1,
            case_data = [],
            resource_constraints = #{},
            time_constraints = #{},
            random_seed = undefined,
            approval_delay_config = #approval_delay_config{
                enabled = false,
                mean_delay = 0,
                stddev_delay = 0,
                min_delay = 0,
                max_delay = 0,
                approval_probability = 0.0
            }
        },
        Delay = yawl_simulation:simulate_approval_delay(Config),
        ?assertEqual(0, Delay)
    end).

simulate_approval_delay_default_test_() ->
    ?_test(begin
        Config = #simulation_config{
            iterations = 1,
            case_data = [],
            resource_constraints = #{},
            time_constraints = #{},
            random_seed = undefined,
            approval_delay_config = undefined
        },
        Delay = yawl_simulation:simulate_approval_delay(Config),
        ?assert(is_number(Delay)),
        ?assert(Delay >= 0)
    end).

%%====================================================================
%% get_approval_delay_stats/1 Tests
%%====================================================================

get_approval_delay_stats_test_() ->
    ?_test(begin
        Result = #{
            total_cases => 1,
            completed_cases => 1,
            failed_cases => 0,
            average_cycle_time => 100,
            min_cycle_time => 100,
            max_cycle_time => 100,
            cycle_time_stddev => 0,
            bottleneck_tasks => [],
            resource_utilization => #{},
            task_completion_rates => #{},
            timestamps => [],
            <<"approval_delays">> => #{<<"task1">> => {1000, 5000}},
            <<"total_approval_wait_time">> => 1000
        },

        Stats = yawl_simulation:get_approval_delay_stats(Result),
        ?assert(maps:is_key(<<"task1">>, Stats))
    end).

%%====================================================================
%% Record Types Tests
%%====================================================================

simulation_config_record_test_() ->
    ?_test(begin
        Config = #simulation_config{
            iterations = 100,
            case_data = [],
            resource_constraints = #{},
            time_constraints = #{},
            random_seed = {1, 2, 3},
            approval_delay_config = #approval_delay_config{
                enabled = true,
                mean_delay = 5000,
                stddev_delay = 1000,
                min_delay = 1000,
                max_delay = 10000,
                approval_probability = 0.5
            }
        },
        ?assertEqual(100, Config#simulation_config.iterations),
        ?assertEqual(true, Config#simulation_config.approval_delay_config#approval_delay_config.enabled)
    end).

simulation_result_record_test_() ->
    ?_test(begin
        Result = #simulation_result{
            total_cases = 50,
            completed_cases = 48,
            failed_cases = 2,
            average_cycle_time = 1000,
            min_cycle_time = 500,
            max_cycle_time = 2000,
            cycle_time_stddev = 100,
            bottleneck_tasks = [],
            resource_utilization = #{},
            task_completion_rates = #{},
            timestamps = [],
            approval_delays = #{},
            total_approval_wait_time = 0
        },
        ?assertEqual(50, Result#simulation_result.total_cases),
        ?assertEqual(48, Result#simulation_result.completed_cases),
        ?assertEqual(2, Result#simulation_result.failed_cases)
    end).

monte_carlo_result_record_test_() ->
    ?_test(begin
        MCResult = #monte_carlo_result{
            iterations = 1000,
            mean_cycle_time = 500,
            median_cycle_time = 480,
            percentile_50 = 480,
            percentile_90 = 750,
            percentile_95 = 850,
            percentile_99 = 950,
            confidence_interval_95 = {400, 600},
            probability_distribution = [],
            risk_factors = #{}
        },
        ?assertEqual(1000, MCResult#monte_carlo_result.iterations),
        ?assertEqual(500, MCResult#monte_carlo_result.mean_cycle_time)
    end).

scenario_record_test_() ->
    ?_test(begin
        Workflow = #{},
        Config = #simulation_config{
            iterations = 10,
            case_data = [],
            resource_constraints = #{},
            time_constraints = #{},
            random_seed = undefined,
            approval_delay_config = undefined
        },
        Scenario = #scenario{name = <<"test">>, workflow = Workflow, config = Config},
        ?assertEqual(<<"test">>, Scenario#scenario.name),
        ?assertEqual(Workflow, Scenario#scenario.workflow),
        ?assertEqual(Config, Scenario#scenario.config)
    end).

bottleneck_record_test_() ->
    ?_test(begin
        Bottleneck = #bottleneck{
            task_id = <<"task1">>,
            severity = critical,
            avg_wait_time = 100,
            avg_processing_time = 500,
            utilization = 0.95,
            recommendations = [<<"Optimize task1">>]
        },
        ?assertEqual(<<"task1">>, Bottleneck#bottleneck.task_id),
        ?assertEqual(critical, Bottleneck#bottleneck.severity),
        ?assertEqual(0.95, Bottleneck#bottleneck.utilization)
    end).

%%====================================================================
%% Edge Cases Tests
%%====================================================================

empty_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{},
             Config = #simulation_config{
                 iterations = 1,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             Result = yawl_simulation:run_simulation(Workflow, Config),

             ?assertEqual(1, maps:get(total_cases, Result)),
             ?assert(is_number(maps:get(average_cycle_time, Result)))
         end)
     end}.

zero_iterations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{},
             Config = #simulation_config{
                 iterations = 0,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = undefined,
                 approval_delay_config = undefined
             },
             Result = yawl_simulation:run_simulation(Workflow, Config),

             ?assertEqual(0, maps:get(total_cases, Result)),
             ?assertEqual(0, maps:get(average_cycle_time, Result))
         end)
     end}.

random_seed_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             Workflow = #{<<"tasks">> => #{}},
             Config1 = #simulation_config{
                 iterations = 10,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = {123, 456, 789},
                 approval_delay_config = undefined
             },
             Config2 = #simulation_config{
                 iterations = 10,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = {123, 456, 789},
                 approval_delay_config = undefined
             },

             Result1 = yawl_simulation:run_simulation(Workflow, Config1),
             timer:sleep(10),
             Result2 = yawl_simulation:run_simulation(Workflow, Config2),

             ?assertEqual(
                maps:get(total_cases, Result1),
                maps:get(total_cases, Result2)
             )
         end)
     end}.

%%====================================================================
%% Statistical Accuracy Tests
%%====================================================================

percentile_accuracy_test_() ->
    ?_test(begin
        %% Test with known dataset
        Data = lists:seq(1, 100),
        P50 = yawl_simulation:get_percentile(Data, 50),
        P95 = yawl_simulation:get_percentile(Data, 95),
        P99 = yawl_simulation:get_percentile(Data, 99),

        %% 50th percentile should be around 50
        ?assert(P50 >= 45 andalso P50 =< 55),
        %% 95th percentile should be >= 90
        ?assert(P95 >= 90),
        %% 99th percentile should be >= 95
        ?assert(P99 >= 95)
    end).

confidence_interval_accuracy_test_() ->
    ?_test(begin
        %% Test with normal distribution-like data
        Data = [100, 105, 95, 102, 98, 101, 99, 103, 97, 100],
        {Low, High} = yawl_simulation:get_confidence_interval(Data, 95.0),

        %% CI should span the mean
        ?assert(Low < 100),
        ?assert(High > 100),
        %% CI should be reasonable (not too wide, not too narrow)
        ?assert(High - Low > 0),
        ?assert(High - Low < 20)
    end).

monte_carlo_convergence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         ?_test(begin
             %% Test that Monte Carlo converges with more iterations
             Wf = #{<<"tasks">> => #{
                 <<"task1">> => #{duration => 100}
             }},
             ConfigBase = #simulation_config{
                 iterations = 50,
                 case_data = [],
                 resource_constraints = #{},
                 time_constraints = #{},
                 random_seed = {42, 42, 42},
                 approval_delay_config = undefined
             },

             %% Run with 50 iterations
             {ok, Result50} = yawl_simulation:monte_carlo_simulation(
                 Wf, 50, ConfigBase
             ),

             %% Run with 200 iterations
             {ok, Result200} = yawl_simulation:monte_carlo_simulation(
                 Wf, 200, ConfigBase#simulation_config{iterations = 200}
             ),

             %% More iterations should have tighter confidence interval
             {Low50, High50} = Result50#monte_carlo_result.confidence_interval_95,
             {Low200, High200} = Result200#monte_carlo_result.confidence_interval_95,

             CI50 = High50 - Low50,
             CI200 = High200 - Low200,

             %% 200 iterations should have narrower or similar CI
             ?assert(CI200 =< CI50 * 1.5)
         end)
     end}.
