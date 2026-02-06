%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Record Definitions for YAWL Simulation Module
%%

%%====================================================================
%% Records
%%====================================================================

-record(approval_delay_config, {
    enabled :: boolean(),
    mean_delay :: number(),          % Mean delay in milliseconds
    stddev_delay :: number(),        % Standard deviation of delay
    min_delay :: number(),           % Minimum delay in milliseconds
    max_delay :: number(),           % Maximum delay in milliseconds
    approval_probability :: float()  % Probability of requiring approval (0.0-1.0)
}).

-record(simulation_config, {
    iterations :: pos_integer(),
    case_data :: [map()],
    resource_constraints :: #{binary() => number()},
    time_constraints :: #{binary() => number()},
    random_seed :: undefined | {integer(), integer(), integer()},
    approval_delay_config :: undefined | #approval_delay_config{}
}).

-record(simulation_result, {
    total_cases :: pos_integer(),
    completed_cases :: pos_integer(),
    failed_cases :: non_neg_integer(),
    average_cycle_time :: number(),
    min_cycle_time :: number(),
    max_cycle_time :: number(),
    cycle_time_stddev :: number(),
    bottleneck_tasks :: [binary()],
    resource_utilization :: #{binary() => number()},
    task_completion_rates :: #{binary() => float()},
    timestamps :: [integer()],
    approval_delays :: #{binary() => {number(), number()}},  % {avg_delay, max_delay}
    total_approval_wait_time :: number()
}).

-record(monte_carlo_result, {
    iterations :: pos_integer(),
    mean_cycle_time :: number(),
    median_cycle_time :: number(),
    percentile_50 :: number(),
    percentile_90 :: number(),
    percentile_95 :: number(),
    percentile_99 :: number(),
    confidence_interval_95 :: {number(), number()},
    probability_distribution :: [{number(), number()}],
    risk_factors :: #{atom() => number()}
}).

-record(scenario, {
    name :: binary(),
    workflow :: term(),
    config :: #simulation_config{}
}).

-record(bottleneck, {
    task_id :: binary(),
    severity :: low | medium | high | critical,
    avg_wait_time :: number(),
    avg_processing_time :: number(),
    utilization :: number(),
    recommendations :: [binary()]
}).
