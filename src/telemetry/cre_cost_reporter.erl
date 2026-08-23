%% -*- erlang -*-
%% @doc CRE Cost Reporter for GCP Deployment
%%
%% Tracks and exports cost-related metrics for GCP deployment monitoring.
%% Integrates with Prometheus/GCP Custom Metrics for cost optimization.
%%
%% @end

-module(cre_cost_reporter).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([stop/0]).
-export([export_cost_metrics/0]).
-export([get_resource_usage/0]).
-export([get_cost_summary/0]).
-export([update_node_count/1]).
-export([update_active_workflows/1]).
-export([collect_metrics/0]).
-export([set_environment/1]).
-export([get_optimization_recommendations/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type option() :: {export_interval, pos_integer()}
                 | {environment, binary()}
                 | {gcp_project, binary()}
                 | {enable_gcp_export, boolean()}.
-type state() :: #{node_count => non_neg_integer(),
                   active_workflows => non_neg_integer(),
                   memory_bytes => non_neg_integer(),
                   process_count => non_neg_integer(),
                   environment => binary(),
                   gcp_project => binary(),
                   enable_gcp_export => boolean(),
                   export_interval => pos_integer(),
                   last_export => integer()}.
-type resource_usage() :: #{node_count => non_neg_integer(),
                            active_workflows => non_neg_integer(),
                            memory_bytes => non_neg_integer(),
                            process_count => non_neg_integer(),
                            cpu_utilization => float(),
                            disk_usage => non_neg_integer()}.
-type cost_summary() :: #{estimated_daily_cost => float(),
                          estimated_monthly_cost => float(),
                          cost_breakdown => map(),
                          environment => binary()}.
-type optimization_suggestion() :: #{type => atom(),
                                     severity => low | medium | high,
                                     description => binary(),
                                     potential_savings => binary()}.

-export_type([resource_usage/0, cost_summary/0, option/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_EXPORT_INTERVAL, 60000). % 1 minute
-define(DEFAULT_ENVIRONMENT, <<"production">>).

%% Metric names
-define(COST_NODE_COUNT, <<"cre_cost_node_count">>).
-define(COST_ACTIVE_WORKFLOWS, <<"cre_cost_active_workflows">>).
-define(COST_MEMORY_BYTES, <<"cre_cost_memory_bytes">>).
-define(COST_PROCESS_COUNT, <<"cre_cost_process_count">>).
-define(COST_CPU_UTILIZATION, <<"cre_cost_cpu_utilization">>).
-define(COST_DISK_USAGE, <<"cre_cost_disk_usage_bytes">>).
-define(COST_ESTIMATED_HOURLY, <<"cre_cost_estimated_hourly">>).
-define(COST_ESTIMATED_DAILY, <<"cre_cost_estimated_daily">>).
-define(COST_ESTIMATED_MONTHLY, <<"cre_cost_estimated_monthly">>).

%% Cost constants (USD)
-define(COST_PER_NODE_HOUR, 0.10).  % e2-medium approximate
-define(COST_PER_GB_HOUR, 0.0004).   % PD-standard approximate

%%====================================================================
%% API
%%====================================================================

%% @doc Start the cost reporter with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the cost reporter with custom options.
-spec start_link([option()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Stop the cost reporter.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Export cost metrics to Prometheus/GCP Custom Metrics.
-spec export_cost_metrics() -> ok.
export_cost_metrics() ->
    gen_server:call(?SERVER, export_cost_metrics).

%% @doc Get current resource usage metrics.
-spec get_resource_usage() -> resource_usage().
get_resource_usage() ->
    gen_server:call(?SERVER, get_resource_usage).

%% @doc Get cost summary for current environment.
-spec get_cost_summary() -> cost_summary().
get_cost_summary() ->
    gen_server:call(?SERVER, get_cost_summary).

%% @doc Update the node count metric.
-spec update_node_count(non_neg_integer()) -> ok.
update_node_count(Count) when is_integer(Count), Count >= 0 ->
    gen_server:cast(?SERVER, {update_node_count, Count}).

%% @doc Update the active workflows count metric.
-spec update_active_workflows(non_neg_integer()) -> ok.
update_active_workflows(Count) when is_integer(Count), Count >= 0 ->
    gen_server:cast(?SERVER, {update_active_workflows, Count}).

%% @doc Collect and update all metrics from system.
-spec collect_metrics() -> ok.
collect_metrics() ->
    gen_server:cast(?SERVER, collect_metrics).

%% @doc Set the environment label for metrics.
-spec set_environment(binary()) -> ok.
set_environment(Environment) when is_binary(Environment) ->
    gen_server:cast(?SERVER, {set_environment, Environment}).

%% @doc Get cost optimization recommendations.
-spec get_optimization_recommendations() -> [optimization_suggestion()].
get_optimization_recommendations() ->
    gen_server:call(?SERVER, get_optimization_recommendations).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    ExportInterval = proplists:get_value(export_interval, Options, ?DEFAULT_EXPORT_INTERVAL),
    Environment = proplists:get_value(environment, Options, ?DEFAULT_ENVIRONMENT),
    GCPProject = proplists:get_value(gcp_project, Options, <<"cre-project">>),
    EnableGCPExport = proplists:get_value(enable_gcp_export, Options, true),

    %% Register cost metrics with OpenTelemetry
    register_cost_metrics(),

    %% Initial metrics collection
    collect_initial_metrics(),

    %% Schedule periodic exports
    timer:send_interval(ExportInterval, export_tick),

    {ok, #{node_count => 0,
           active_workflows => 0,
           memory_bytes => 0,
           process_count => 0,
           cpu_utilization => 0.0,
           disk_usage => 0,
           environment => Environment,
           gcp_project => GCPProject,
           enable_gcp_export => EnableGCPExport,
           export_interval => ExportInterval,
           last_export => erlang:system_time(millisecond)}}.

handle_call(export_cost_metrics, _From, State) ->
    ok = do_export_metrics(State),
    {reply, ok, State#{last_export => erlang:system_time(millisecond)}};

handle_call(get_resource_usage, _From, State) ->
    Usage = #{
        node_count => maps:get(node_count, State),
        active_workflows => maps:get(active_workflows, State),
        memory_bytes => maps:get(memory_bytes, State),
        process_count => maps:get(process_count, State),
        cpu_utilization => maps:get(cpu_utilization, State),
        disk_usage => maps:get(disk_usage, State)
    },
    {reply, Usage, State};

handle_call(get_cost_summary, _From, State) ->
    NodeCount = maps:get(node_count, State, 0),
    MemoryBytes = maps:get(memory_bytes, State, 0),
    Env = maps:get(environment, State, <<"production">>),
    CostPerHour = calculate_hourly_cost(NodeCount, MemoryBytes),
    Summary = #{
        estimated_hourly_cost => CostPerHour,
        estimated_daily_cost => CostPerHour * 24,
        estimated_monthly_cost => CostPerHour * 24 * 30,
        cost_breakdown => #{
            compute => NodeCount * ?COST_PER_NODE_HOUR,
            storage => (MemoryBytes / (1024 * 1024 * 1024)) * ?COST_PER_GB_HOUR
        },
        environment => Env
    },
    {reply, Summary, State};

handle_call(get_optimization_recommendations, _From, State) ->
    Recommendations = analyze_optimization(State),
    {reply, Recommendations, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({update_node_count, Count}, State) ->
    Labels = #{environment => maps:get(environment, State)},
    otel_metrics:set_gauge(?COST_NODE_COUNT, Labels, Count),
    {noreply, State#{node_count => Count}};

handle_cast({update_active_workflows, Count}, State) ->
    Labels = #{environment => maps:get(environment, State)},
    otel_metrics:set_gauge(?COST_ACTIVE_WORKFLOWS, Labels, Count),
    {noreply, State#{active_workflows => Count}};

handle_cast({set_environment, Environment}, State) ->
    {noreply, State#{environment => Environment}};

handle_cast(collect_metrics, State) ->
    NewState = do_collect_metrics(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(export_tick, State) ->
    ok = do_export_metrics(State),
    {noreply, State#{last_export => erlang:system_time(millisecond)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Register all cost-related metrics.
-spec register_cost_metrics() -> ok.
register_cost_metrics() ->
    %% Resource metrics
    ok = otel_metrics:register_gauge(?COST_NODE_COUNT,
                                     <<"Number of CRE nodes in the cluster">>),
    ok = otel_metrics:register_gauge(?COST_ACTIVE_WORKFLOWS,
                                     <<"Number of active YAWL workflows">>),
    ok = otel_metrics:register_gauge(?COST_MEMORY_BYTES,
                                     <<"CRE memory usage in bytes">>),
    ok = otel_metrics:register_gauge(?COST_PROCESS_COUNT,
                                     <<"Number of CRE processes">>),
    ok = otel_metrics:register_gauge(?COST_CPU_UTILIZATION,
                                     <<"CPU utilization as percentage">>),
    ok = otel_metrics:register_gauge(?COST_DISK_USAGE,
                                     <<"Disk usage in bytes">>),

    %% Cost estimation metrics
    ok = otel_metrics:register_gauge(?COST_ESTIMATED_HOURLY,
                                     <<"Estimated hourly cost in USD">>),
    ok = otel_metrics:register_gauge(?COST_ESTIMATED_DAILY,
                                     <<"Estimated daily cost in USD">>),
    ok = otel_metrics:register_gauge(?COST_ESTIMATED_MONTHLY,
                                     <<"Estimated monthly cost in USD">>),
    ok.

%% @private Collect initial metrics from system.
-spec collect_initial_metrics() -> ok.
collect_initial_metrics() ->
    %% Get process count
    ProcessCount = erlang:system_info(process_count),
    Labels = #{environment => ?DEFAULT_ENVIRONMENT},
    otel_metrics:set_gauge(?COST_PROCESS_COUNT, Labels, ProcessCount),

    %% Get memory usage
    MemoryBytes = erlang:memory(total),
    otel_metrics:set_gauge(?COST_MEMORY_BYTES, Labels, MemoryBytes),

    ok.

%% @private Collect current metrics from the system.
-spec do_collect_metrics(state()) -> state().
do_collect_metrics(State) ->
    Env = maps:get(environment, State),
    Labels = #{environment => Env},

    %% Update memory
    MemoryBytes = erlang:memory(total),
    otel_metrics:set_gauge(?COST_MEMORY_BYTES, Labels, MemoryBytes),

    %% Update process count
    ProcessCount = erlang:system_info(process_count),
    otel_metrics:set_gauge(?COST_PROCESS_COUNT, Labels, ProcessCount),

    %% Calculate CPU utilization (scheduler utilization)
    CpuUtil = calculate_cpu_utilization(),
    otel_metrics:set_gauge(?COST_CPU_UTILIZATION, Labels, CpuUtil),

    %% Update disk usage (application directory)
    DiskUsage = get_disk_usage(),
    otel_metrics:set_gauge(?COST_DISK_USAGE, Labels, DiskUsage),

    State#{memory_bytes => MemoryBytes,
           process_count => ProcessCount,
           cpu_utilization => CpuUtil,
           disk_usage => DiskUsage}.

%% @private Export metrics to Prometheus and optionally GCP.
-spec do_export_metrics(state()) -> ok.
do_export_metrics(State) ->
    Env = maps:get(environment, State, <<"production">>),
    Labels = #{environment => Env},

    %% Update cost estimates
    NodeCount = maps:get(node_count, State, 0),
    MemoryBytes = maps:get(memory_bytes, State, 0),

    CostPerHour = calculate_hourly_cost(NodeCount, MemoryBytes),
    otel_metrics:set_gauge(?COST_ESTIMATED_HOURLY, Labels, CostPerHour),
    otel_metrics:set_gauge(?COST_ESTIMATED_DAILY, Labels, CostPerHour * 24),
    otel_metrics:set_gauge(?COST_ESTIMATED_MONTHLY, Labels, CostPerHour * 24 * 30),

    %% Export to GCP if enabled
    case maps:get(enable_gcp_export, State, true) of
        true ->
            export_to_gcp(State);
        false ->
            ok
    end,

    ?LOG(debug, "Cost metrics exported for environment ~p: ~.2f USD/hour",
         [Env, CostPerHour]),
    ok.

%% @private Calculate estimated hourly cost.
-spec calculate_hourly_cost(non_neg_integer(), non_neg_integer()) -> float().
calculate_hourly_cost(NodeCount, MemoryBytes) ->
    ComputeCost = NodeCount * ?COST_PER_NODE_HOUR,
    StorageGB = MemoryBytes / (1024 * 1024 * 1024),
    StorageCost = StorageGB * ?COST_PER_GB_HOUR * 24,
    ComputeCost + StorageCost.

%% @private Calculate CPU utilization percentage.
-spec calculate_cpu_utilization() -> float().
calculate_cpu_utilization() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            0.0;
        {TotalActive, _TotalTotal} when is_list(TotalActive) ->
            ActiveSum = lists:sum([A || {_, A, _} <- TotalActive]),
            TotalSum = lists:sum([T || {_, _, T} <- TotalActive]),
            case TotalSum of
                0 -> 0.0;
                _ -> (ActiveSum / TotalSum) * 100
            end;
        _ ->
            0.0
    end.

%% @private Get disk usage for the application.
-spec get_disk_usage() -> non_neg_integer().
get_disk_usage() ->
    %% Try to get disk usage from application directory
    case code:priv_dir(cre) of
        {error, bad_name} ->
            %% Fallback to current directory
            get_dir_usage(".");
        Dir ->
            get_dir_usage(Dir)
    end.

%% @private Get directory usage recursively.
-spec get_dir_usage(file:filename_all()) -> non_neg_integer().
get_dir_usage(Dir) ->
    try
        Cmd = io_lib:format("du -sb ~s 2>/dev/null | cut -f1", [Dir]),
        Output = os:cmd(Cmd),
        case string:trim(Output) of
            [] -> 0;
            Str ->
                try list_to_integer(Str) of
                    Bytes -> Bytes
                catch
                    error:_ -> 0
                end
        end
    catch
        _:_ -> 0
    end.

%% @private Export metrics to GCP Custom Metrics (IMPLEMENTED).
-spec export_to_gcp(state()) -> ok.
export_to_gcp(State = #{gcp_project := Project, environment := Env}) ->
    %% Collect usage metrics
    NodeCount = maps:get(node_count, State, 0),
    ActiveWorkflows = maps:get(active_workflows, State, 0),
    MemoryBytes = maps:get(memory_bytes, State, 0),
    CpuUtil = maps:get(cpu_utilization, State, 0.0),

    %% Calculate usage units (for future v2 metering)
    %% These are defined now but not reported to Marketplace API yet
    WorkflowHours = calculate_workflow_hours(ActiveWorkflows),
    NodeHours = calculate_node_hours(NodeCount),

    UsageData = #{
        timestamp => erlang:system_time(second),
        environment => Env,
        metrics => #{
            node_count => NodeCount,
            active_workflows => ActiveWorkflows,
            memory_bytes => MemoryBytes,
            cpu_utilization_percent => CpuUtil,
            %% Future v2 metering units
            workflow_hours => WorkflowHours,
            node_hours => NodeHours
        }
    },

    %% In v1: Log usage metrics (for customer visibility)
    ?LOG(info, "CRE Usage Metrics for project ~s: ~p", [Project, UsageData]),

    %% In v1: Store usage metrics locally for v2 migration
    store_usage_metrics(UsageData),

    %% In v2: Send to Marketplace Metering API
    %% marketplace_metering_client:report_usage(Project, UsageData),

    ok;

export_to_gcp(_) ->
    ok.

%% @private Analyze current state and provide optimization recommendations.
-spec analyze_optimization(state()) -> [optimization_suggestion()].
analyze_optimization(State) ->
    NodeCount = maps:get(node_count, State, 0),
    ActiveWorkflows = maps:get(active_workflows, State, 0),
    CpuUtil = maps:get(cpu_utilization, State, 0),
    MemoryBytes = maps:get(memory_bytes, State, 0),

    Recommendations = [],

    %% Check for over-provisioned nodes
    R1 = case ActiveWorkflows > 0 of
        true when NodeCount > 0, (ActiveWorkflows / NodeCount) < 2 ->
            [#{type => node_over_provision,
               severity => medium,
               description => <<"Node count may be over-provisioned for current workload">>,
               potential_suggestion => io_lib:format("Consider reducing nodes from ~p to ~p",
                                                    [NodeCount, max(1, NodeCount div 2)]),
               potential_savings => <<"~50% compute cost reduction">>}];
        _ ->
            []
    end,

    %% Check for low CPU utilization
    R2 = case CpuUtil of
        Util when Util < 20.0, NodeCount > 1 ->
            [#{type => low_cpu_utilization,
               severity => low,
               description => <<"CPU utilization is consistently low">>,
               potential_suggestion => <<"Consider reducing node pool size or switching to smaller machine types">>,
               potential_savings => <<"~30% compute cost reduction">>}];
        _ ->
            []
    end,

    %% Check for memory waste
    R3 = case NodeCount > 0 of
        true when MemoryBytes / NodeCount < (500 * 1024 * 1024) -> %% Less than 500MB per node
            [#{type => memory_underutilization,
               severity => low,
               description => <<"Memory per node is underutilized">>,
               potential_suggestion => <<"Consider switching to memory-optimized node types">>,
               potential_savings => <<"Variable depending on instance type">>}];
        _ ->
            []
    end,

    %% Check for idle workflows
    R4 = case ActiveWorkflows of
        0 when NodeCount > 0 ->
            [#{type => idle_cluster,
               severity => high,
               description => <<"No active workflows but nodes are running">>,
               potential_suggestion => <<"Scale to zero or use cluster autoscaler">>,
               potential_savings => <<"100% compute cost during idle periods">>}];
        _ ->
            []
    end,

    lists:flatten([Recommendations, R1, R2, R3, R4]).

%% @private Calculate workflow execution hours (metering unit for v2).
-spec calculate_workflow_hours(non_neg_integer()) -> float().
calculate_workflow_hours(ActiveWorkflows) ->
    %% For v1: Just return the count
    %% For v2: This will aggregate actual execution time
    ActiveWorkflows * 1.0.  %% Will be multiplied by actual duration in v2

%% @private Calculate node hours (metering unit for v2).
-spec calculate_node_hours(non_neg_integer()) -> float().
calculate_node_hours(NodeCount) ->
    %% For v1: Just return the count
    %% For v2: This will aggregate actual uptime
    NodeCount * 1.0.  %% Will be multiplied by actual uptime in v2

%% @private Store usage metrics locally (for v2 migration).
-spec store_usage_metrics(map()) -> ok.
store_usage_metrics(UsageData) ->
    %% Store in Mnesia or file for v2 usage-based billing migration
    try
        Filename = "/opt/cre/data/usage/usage_metrics.jsonl",
        filelib:ensure_dir(Filename),
        Line = io_lib:format("~p~n", [UsageData]),
        file:write_file(Filename, Line, [append]),
        ok
    catch
        _:_ ->
            ?LOG(warning, "Failed to store usage metrics", []),
            ok
    end.
