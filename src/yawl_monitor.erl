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
%% @author YAWL Monitor Implementation
%% @copyright 2025
%%
%% @doc YAWL Workflow Monitoring Module for CRE
%%
%% This module implements real-time workflow monitoring with metrics
%% collection, aggregation, and export capabilities compatible with
%% Java YAWL's monitoring and reporting features.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Real-time Metrics:</b> Record and query workflow metrics</li>
%%   <li><b>Label Support:</b> Tag metrics with custom labels</li>
%%   <li><b>Prometheus Export:</b> Export metrics in Prometheus format</li>
%%   <li><b>Subscriptions:</b> Subscribe to metric updates</li>
%%   <li><b>Dashboards:</b> Create and manage metric dashboards</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% <pre>
%% %% Start the monitor
%% yawl_monitor:start_monitor().
%%
%% %% Record a metric
%% yawl_monitor:record_metric(<<"task_duration">>, 1250, #{task => <<"approve">>}).
%%
%% %% Get metrics for a case
%% yawl_monitor:get_case_metrics(<<"case_123">>).
%%
%% %% Export to Prometheus format
%% yawl_monitor:export_prometheus().
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_monitor).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_monitor/0,
         start_monitor/1,
         stop_monitor/0,
         record_metric/3,
         record_metric/4,
         get_metrics/2,
         get_case_metrics/1,
         get_engine_metrics/0,
         subscribe_to_metrics/2,
         unsubscribe_from_metrics/1,
         create_dashboard/1,
         delete_dashboard/1,
         get_dashboard/1,
         list_dashboards/0,
         export_prometheus/0,
         export_prometheus/1,
         get_metric_summary/0,
         reset_metrics/0,
         binary_to_existing_atom/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% Includes
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

-define(DAY, 86400000).  %% 24 hours in milliseconds
-define(HOUR, 3600000). %% 1 hour in milliseconds
-define(MINUTE, 60000). %% 1 minute in milliseconds

%%====================================================================
%% Types
%%====================================================================

-type metric_name() :: binary().
-type metric_value() :: number().
-type label_key() :: binary() | atom().
-type label_value() :: binary() | atom() | number().
-type labels() :: #{label_key() => label_value()}.
-type timestamp() :: integer().

-record(metric, {
    name :: metric_name(),
    value :: metric_value(),
    timestamp :: timestamp(),
    labels :: labels()
}).

-type metric() :: #metric{}.
-type metric_query() :: #{name => metric_name(),
                          label_filter => labels(),
                          time_range => {timestamp(), timestamp()}}.

-record(subscription, {
    id :: reference(),
    subscriber :: pid(),
    filter :: metric_query() | all
}).

-type subscription() :: #subscription{}.

-record(dashboard, {
    id :: binary(),
    name :: binary(),
    metric_names :: [metric_name()],
    refresh_interval :: pos_integer() | undefined,
    created_at :: timestamp()
}).

-type dashboard() :: #dashboard{}.

-record(monitor_state, {
    metrics :: [metric()],
    subscriptions :: [subscription()],
    dashboards :: #{binary() => dashboard()},
    metric_index :: #{metric_name() => [metric()]},
    case_index :: #{binary() => [metric()]},
    start_time :: timestamp()
}).

-type monitor_state() :: #monitor_state{}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the workflow monitor with default configuration.
%%
%% @return {ok, Pid} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_monitor() -> {ok, pid()} | {error, term()}.

start_monitor() ->
    start_monitor(#{}).

%%--------------------------------------------------------------------
%% @doc Starts the workflow monitor with configuration.
%%
%% @param Config Configuration map for the monitor.
%% @return {ok, Pid} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_monitor(Config :: map()) -> {ok, pid()} | {error, term()}.

start_monitor(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc Stops the workflow monitor.
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_monitor() -> ok.

stop_monitor() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Records a metric with labels.
%%
%% @param Name The metric name.
%% @param Value The metric value.
%% @param Labels Optional labels for the metric.
%% @param Timestamp Optional timestamp (defaults to now).
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec record_metric(Name :: metric_name(),
                    Value :: metric_value(),
                    Labels :: labels(),
                    Timestamp :: timestamp() | undefined) -> ok.

record_metric(Name, Value, Labels, Timestamp) ->
    gen_server:cast(?MODULE, {record_metric, Name, Value, Labels, Timestamp}).

%%--------------------------------------------------------------------
%% @doc Records a metric with current timestamp.
%%
%% @end
%%--------------------------------------------------------------------
%% @doc Records a metric with labels (3-arity version).
%% Exported for backward compatibility and external use.
%% @end
%%--------------------------------------------------------------------
-spec record_metric(Name :: metric_name(),
                    Value :: metric_value(),
                    Labels :: labels()) -> ok.

record_metric(Name, Value, Labels) ->
    record_metric(Name, Value, Labels, undefined).

%%--------------------------------------------------------------------
%% @doc Queries metrics based on criteria.
%%
%% @param Query The query criteria.
%% @param Limit Maximum number of results.
%% @return List of matching metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_metrics(Query :: metric_query(), Limit :: non_neg_integer()) -> [metric()].

get_metrics(Query, Limit) ->
    gen_server:call(?MODULE, {get_metrics, Query, Limit}).

%%--------------------------------------------------------------------
%% @doc Gets all metrics for a specific case.
%%
%% @param CaseId The case identifier.
%% @return List of metrics for the case.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_metrics(CaseId :: binary()) -> [metric()].

get_case_metrics(CaseId) ->
    gen_server:call(?MODULE, {get_case_metrics, CaseId}).

%%--------------------------------------------------------------------
%% @doc Gets engine-level aggregated metrics.
%%
%% @return Map of engine metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_engine_metrics() -> #{atom() => number()}.

get_engine_metrics() ->
    gen_server:call(?MODULE, get_engine_metrics).

%%--------------------------------------------------------------------
%% @doc Subscribes to metric updates.
%%
%% @param Filter The filter criteria for subscriptions.
%% @param SubscriberPid The pid to receive updates.
%% @return {ok, SubscriptionId}
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_to_metrics(Filter :: metric_query() | all,
                           SubscriberPid :: pid()) -> {ok, reference()}.

subscribe_to_metrics(Filter, SubscriberPid) ->
    gen_server:call(?MODULE, {subscribe, Filter, SubscriberPid}).

%%--------------------------------------------------------------------
%% @doc Unsubscribes from metric updates.
%%
%% @param SubscriptionId The subscription reference.
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe_from_metrics(SubscriptionId :: reference()) -> ok.

unsubscribe_from_metrics(SubscriptionId) ->
    gen_server:cast(?MODULE, {unsubscribe, SubscriptionId}).

%%--------------------------------------------------------------------
%% @doc Creates a new metric dashboard.
%%
%% @param Config Dashboard configuration map.
%% @return {ok, DashboardId}
%%
%% @end
%%--------------------------------------------------------------------
-spec create_dashboard(Config :: map()) -> {ok, binary()}.

create_dashboard(Config) ->
    gen_server:call(?MODULE, {create_dashboard, Config}).

%%--------------------------------------------------------------------
%% @doc Deletes a dashboard.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_dashboard(DashboardId :: binary()) -> ok.

delete_dashboard(DashboardId) ->
    gen_server:cast(?MODULE, {delete_dashboard, DashboardId}).

%%--------------------------------------------------------------------
%% @doc Gets dashboard configuration.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_dashboard(DashboardId :: binary()) -> {ok, dashboard()} | {error, not_found}.

get_dashboard(DashboardId) ->
    gen_server:call(?MODULE, {get_dashboard, DashboardId}).

%%--------------------------------------------------------------------
%% @doc Lists all dashboards.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_dashboards() -> [dashboard()].

list_dashboards() ->
    gen_server:call(?MODULE, list_dashboards).

%%--------------------------------------------------------------------
%% @doc Exports all metrics in Prometheus text format.
%%
%% @return Prometheus formatted binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_prometheus() -> binary().

export_prometheus() ->
    export_prometheus(all).

%%--------------------------------------------------------------------
%% @doc Exports metrics in Prometheus text format.
%%
%% @param Scope all, case, or specific metric name.
%% @return Prometheus formatted binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_prometheus(Scope :: all | {'case', binary()} | metric_name()) -> binary().

export_prometheus(Scope) ->
    gen_server:call(?MODULE, {export_prometheus, Scope}).

%%--------------------------------------------------------------------
%% @doc Gets a summary of all metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_metric_summary() -> #{metric_name() => #{count => non_neg_integer(),
                                                   min => metric_value(),
                                                   max => metric_value(),
                                                   avg => metric_value()}}.

get_metric_summary() ->
    gen_server:call(?MODULE, get_metric_summary).

%%--------------------------------------------------------------------
%% @doc Resets all stored metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_metrics() -> ok.

reset_metrics() ->
    gen_server:cast(?MODULE, reset_metrics).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the monitor server.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Config :: map()) -> {ok, monitor_state()}.

init(Config) ->
    MaxMetrics = maps:get(max_metrics, Config, 10000),
    RetentionMs = maps:get(retention_ms, Config, ?DAY),

    %% Set up retention timer
    schedule_retention_cleanup(RetentionMs),

    logger:info("Monitor initialized: max_metrics=~p retention=~p ms",
                [MaxMetrics, RetentionMs],
                [{module, ?MODULE}, {action, init}]),

    {ok, #monitor_state{
        metrics = [],
        subscriptions = [],
        dashboards = #{},
        metric_index = #{},
        case_index = #{},
        start_time = erlang:system_time(millisecond)
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: term(), State :: monitor_state()) ->
          {reply, term(), monitor_state()}.

handle_call({get_metrics, Query, Limit}, _From, State) ->
    Metrics = filter_metrics(State#monitor_state.metrics, Query),
    Result = lists:sublist(Metrics, Limit),
    {reply, Result, State};

handle_call({get_case_metrics, CaseId}, _From, State) ->
    Result = maps:get(CaseId, State#monitor_state.case_index, []),
    {reply, Result, State};

handle_call(get_engine_metrics, _From, State) ->
    Metrics = State#monitor_state.metrics,
    Result = calculate_engine_metrics(Metrics, State#monitor_state.start_time),
    {reply, Result, State};

handle_call({subscribe, Filter, SubscriberPid}, _From, State) ->
    SubId = make_ref(),
    Subscription = #subscription{
        id = SubId,
        subscriber = SubscriberPid,
        filter = Filter
    },
    State1 = State#monitor_state{
        subscriptions = [Subscription | State#monitor_state.subscriptions]
    },
    %% Monitor the subscriber process
    erlang:monitor(process, SubscriberPid),
    {reply, {ok, SubId}, State1};

handle_call({create_dashboard, Config}, _From, State) ->
    Name = maps:get(name, Config, <<"Untitled Dashboard">>),
    MetricNames = maps:get(metric_names, Config, []),
    RefreshInterval = maps:get(refresh_interval, Config, undefined),

    DashboardId = generate_dashboard_id(),
    Dashboard = #dashboard{
        id = DashboardId,
        name = Name,
        metric_names = MetricNames,
        refresh_interval = RefreshInterval,
        created_at = erlang:system_time(millisecond)
    },

    State1 = State#monitor_state{
        dashboards = maps:put(DashboardId, Dashboard, State#monitor_state.dashboards)
    },

    {reply, {ok, DashboardId}, State1};

handle_call({get_dashboard, DashboardId}, _From, State) ->
    case maps:get(DashboardId, State#monitor_state.dashboards, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Dashboard -> {reply, {ok, Dashboard}, State}
    end;

handle_call(list_dashboards, _From, State) ->
    Dashboards = maps:values(State#monitor_state.dashboards),
    {reply, Dashboards, State};

handle_call({export_prometheus, Scope}, _From, State) ->
    Metrics = select_metrics_by_scope(Scope, State),
    Result = format_prometheus(Metrics),
    {reply, Result, State};

handle_call(get_metric_summary, _From, State) ->
    Result = summarize_metrics(State#monitor_state.metrics),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles asynchronous casts.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: monitor_state()) ->
          {noreply, monitor_state()}.

handle_cast({record_metric, Name, Value, Labels, Timestamp}, State) ->
    TS = case Timestamp of
        undefined -> erlang:system_time(millisecond);
        _ -> Timestamp
    end,

    Metric = #metric{
        name = Name,
        value = Value,
        timestamp = TS,
        labels = Labels
    },

    %% Update metrics list
    Metrics = [Metric | State#monitor_state.metrics],

    %% Update metric index
    MetricIndex = maps:update_with(Name,
        fun(Existing) -> [Metric | Existing] end,
        [Metric],
        State#monitor_state.metric_index),

    %% Update case index if case_id label exists
    CaseIndex = case maps:get(<<"case_id">>, Labels) of
        undefined ->
            case maps:get(case_id, Labels, undefined) of
                undefined -> State#monitor_state.case_index;
                CaseId ->
                    maps:update_with(CaseId,
                        fun(Existing) -> [Metric | Existing] end,
                        [Metric],
                        State#monitor_state.case_index)
            end;
        CaseId ->
            maps:update_with(CaseId,
                fun(Existing) -> [Metric | Existing] end,
                [Metric],
                State#monitor_state.case_index)
    end,

    %% Notify subscribers
    notify_subscribers(Metric, State#monitor_state.subscriptions),

    %% Emit telemetry event if available
    emit_telemetry(Metric),

    {noreply, State#monitor_state{
        metrics = Metrics,
        metric_index = MetricIndex,
        case_index = CaseIndex
    }};

handle_cast({unsubscribe, SubId}, State) ->
    Subscriptions = lists:filter(
        fun(S) -> S#subscription.id =/= SubId end,
        State#monitor_state.subscriptions
    ),
    {noreply, State#monitor_state{subscriptions = Subscriptions}};

handle_cast({delete_dashboard, DashboardId}, State) ->
    Dashboards = maps:remove(DashboardId, State#monitor_state.dashboards),
    {noreply, State#monitor_state{dashboards = Dashboards}};

handle_cast(reset_metrics, State) ->
    logger:info("Metrics reset", [{module, ?MODULE}, {action, reset_metrics}]),
    {noreply, State#monitor_state{
        metrics = [],
        metric_index = #{},
        case_index = #{}
    }};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles info messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: monitor_state()) ->
          {noreply, monitor_state()}.

handle_info(retention_cleanup, State) ->
    State1 = cleanup_old_metrics(State),
    schedule_retention_cleanup(?DAY),
    {noreply, State1};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove subscriptions for dead processes
    Subscriptions = lists:filter(
        fun(S) -> S#subscription.subscriber =/= Pid end,
        State#monitor_state.subscriptions
    ),
    {noreply, State#monitor_state{subscriptions = Subscriptions}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminates the monitor server.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: monitor_state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles code changes.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: monitor_state(), Extra :: term()) ->
          {ok, monitor_state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Filters metrics based on query criteria.
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_metrics([metric()], metric_query() | all) -> [metric()].

filter_metrics(Metrics, all) ->
    Metrics;
filter_metrics(Metrics, Query) ->
    Filtered = lists:filter(
        fun(M) ->
            NameMatch = case maps:get(name, Query, undefined) of
                undefined -> true;
                QueryName -> M#metric.name =:= QueryName
            end,
            LabelMatch = case maps:get(label_filter, Query, undefined) of
                undefined -> true;
                FilterLabels ->
                    lists:all(
                        fun({K, V}) ->
                            maps:get(K, M#metric.labels, undefined) =:= V
                        end,
                        maps:to_list(FilterLabels)
                    )
            end,
            TimeMatch = case maps:get(time_range, Query, undefined) of
                undefined -> true;
                {Start, End} ->
                    M#metric.timestamp >= Start andalso M#metric.timestamp =< End
            end,
            NameMatch andalso LabelMatch andalso TimeMatch
        end,
        Metrics
    ),
    lists:reverse(Filtered).

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates engine-level aggregated metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_engine_metrics([metric()], timestamp()) ->
          #{atom() => number()}.

calculate_engine_metrics(Metrics, StartTime) ->
    Now = erlang:system_time(millisecond),
    UptimeMs = Now - StartTime,

    TotalCases = length(
        lists:usort([maps:get(<<"case_id">>, M#metric.labels, <<"unknown">>)
            || M <- Metrics])
    ),

    TotalTasks = length([M || M <- Metrics, M#metric.name =:= <<"task_completed">>]),

    AvgCycleTime = case TotalCases of
        0 -> 0;
        _ ->
            CycleTimes = [M#metric.value || M <- Metrics,
                M#metric.name =:= <<"case_cycle_time">>],
            lists:sum(CycleTimes) / length(CycleTimes)
    end,

    ActiveTasks = length([M || M <- Metrics,
        M#metric.name =:= <<"task_started">>,
        Now - M#metric.timestamp < 300000]), %% Active in last 5 minutes

    #{
        uptime_ms => UptimeMs,
        total_cases => TotalCases,
        total_tasks => TotalTasks,
        avg_cycle_time_ms => AvgCycleTime,
        active_tasks => ActiveTasks,
        metrics_recorded => length(Metrics)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Notifies subscribers of new metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec notify_subscribers(metric(), [subscription()]) -> ok.

notify_subscribers(_Metric, []) ->
    ok;
notify_subscribers(Metric, [Sub | Rest]) ->
    case should_notify(Metric, Sub#subscription.filter) of
        true ->
            Sub#subscription.subscriber ! {metric_update, Metric};
        false ->
            ok
    end,
    notify_subscribers(Metric, Rest).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a subscription should be notified.
%%
%% @end
%%--------------------------------------------------------------------
-spec should_notify(metric(), metric_query() | all) -> boolean().

should_notify(_Metric, all) ->
    true;
should_notify(Metric, Query) ->
    case maps:get(name, Query, undefined) of
        undefined -> true;
        QueryName -> Metric#metric.name =:= QueryName
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Emits telemetry event if telemetry is available.
%%
%% @end
%%--------------------------------------------------------------------
-spec emit_telemetry(metric()) -> ok.

emit_telemetry(Metric) ->
    case code:is_loaded(telemetry) of
        false ->
            ok;
        {file, _} ->
            try
                EventName = [yawl, metric, erlang:binary_to_existing_atom(Metric#metric.name, utf8)],
                Measurements = #{value => Metric#metric.value},
                Metadata = maps:put(timestamp, Metric#metric.timestamp, Metric#metric.labels),
                % Use apply/3 to avoid xref warnings for optional dependencies
                % telemetry:execute(EventName, Measurements) is the correct signature
                apply(telemetry, execute, [EventName, Measurements#{metadata => Metadata}])
            catch
                _:_ -> ok
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Selects metrics based on export scope.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_metrics_by_scope(all | {'case', binary()} | metric_name(),
                              monitor_state()) -> [metric()].

select_metrics_by_scope(all, State) ->
    State#monitor_state.metrics;
select_metrics_by_scope({'case', CaseId}, State) ->
    maps:get(CaseId, State#monitor_state.case_index, []);
select_metrics_by_scope(MetricName, State) when is_binary(MetricName) ->
    maps:get(MetricName, State#monitor_state.metric_index, []).

%%--------------------------------------------------------------------
%% @private
%% @doc Formats metrics in Prometheus text format.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_prometheus([metric()]) -> binary().

format_prometheus(Metrics) ->
    %% Group metrics by name
    Grouped = lists:foldl(
        fun(M, Acc) ->
            Name = M#metric.name,
            maps:update_with(Name, fun(Existing) -> [M | Existing] end, [M], Acc)
        end,
        #{},
        Metrics
    ),

    %% Format each metric group
    Lines = maps:fold(
        fun(Name, MetricsList, Acc) ->
            HelpLine = <<"# HELP ", Name/binary, " YAWL workflow metric">>,
            TypeLine = <<"# TYPE ", Name/binary, " gauge">>,
            MetricLines = format_metric_group(Name, MetricsList, []),
            [HelpLine, TypeLine | MetricLines] ++ Acc
        end,
        [],
        Grouped
    ),

    iolist_to_binary(lists:join(<<$\n>>, lists:reverse(Lines))).

%%--------------------------------------------------------------------
%% @private
%% @doc Formats a group of metrics with the same name.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_metric_group(metric_name(), [metric()], [binary()]) -> [binary()].

format_metric_group(_Name, [], Acc) ->
    lists:reverse(Acc);
format_metric_group(Name, [Metric | Rest], Acc) ->
    %% Format labels
    LabelStr = format_labels(Metric#metric.labels),
    Line = io_lib:format("~s~s ~w ~w",
        [Name, LabelStr, Metric#metric.value, Metric#metric.timestamp]),
    format_metric_group(Name, Rest, [list_to_binary(Line) | Acc]).

%%--------------------------------------------------------------------
%% @private
%% @doc Formats metric labels for Prometheus format.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_labels(labels()) -> iolist().

format_labels(Labels) when map_size(Labels) =:= 0 ->
    "";
format_labels(Labels) ->
    Pairs = maps:to_list(Labels),
    Formatted = [format_label_pair(K, V) || {K, V} <- Pairs],
    [${, lists:join($,, Formatted), $}].

%%--------------------------------------------------------------------
%% @private
%% @doc Formats a single label key-value pair.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_label_pair(label_key(), label_value()) -> iolist().

format_label_pair(Key, Value) when is_atom(Key); is_atom(Value) ->
    [atom_to_list(Key), "=\"", atom_to_list(Value), $"];
format_label_pair(Key, Value) when is_integer(Value) ->
    [to_string(Key), "=\"", integer_to_list(Value), $"];
format_label_pair(Key, Value) when is_float(Value) ->
    [to_string(Key), "=\"", float_to_list(Value, [{decimals, 4}]), $"];
format_label_pair(Key, Value) when is_binary(Value) ->
    [to_string(Key), "=\"", Value, $"];
format_label_pair(Key, Value) ->
    [to_string(Key), "=\"", to_string(Value), $"].

%%--------------------------------------------------------------------
%% @private
%% @doc Converts various types to string.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_string(binary() | atom() | list()) -> iolist().

to_string(V) when is_binary(V) -> V;
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_list(V) -> V.

%%--------------------------------------------------------------------
%% @private
%% @doc Summarizes metrics by name.
%%
%% @end
%%--------------------------------------------------------------------
-spec summarize_metrics([metric()]) ->
          #{metric_name() => #{count => non_neg_integer(),
                               min => metric_value(),
                               max => metric_value(),
                               avg => metric_value()}}.

summarize_metrics(Metrics) ->
    Grouped = lists:foldl(
        fun(M, Acc) ->
            Name = M#metric.name,
            maps:update_with(Name, fun(Existing) -> [M#metric.value | Existing] end,
                [M#metric.value], Acc)
        end,
        #{},
        Metrics
    ),

    maps:map(
        fun(_Name, Values) ->
            Count = length(Values),
            #{
                count => Count,
                min => lists:min(Values),
                max => lists:max(Values),
                avg => lists:sum(Values) / Count
            }
        end,
        Grouped
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Cleans up old metrics based on retention policy.
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_old_metrics(monitor_state()) -> monitor_state().

cleanup_old_metrics(State) ->
    CutoffTime = erlang:system_time(millisecond) - ?DAY,

    %% Filter out old metrics
    {KeptMetrics, RemovedMetrics} = lists:partition(
        fun(M) -> M#metric.timestamp >= CutoffTime end,
        State#monitor_state.metrics
    ),

    %% Rebuild indexes
    MetricIndex = rebuild_metric_index(KeptMetrics),
    CaseIndex = rebuild_case_index(KeptMetrics),

    logger:info("Old metrics cleaned: removed=~p kept=~p",
                [length(RemovedMetrics), length(KeptMetrics)],
                [{module, ?MODULE}, {action, cleanup_old_metrics}]),

    State#monitor_state{
        metrics = KeptMetrics,
        metric_index = MetricIndex,
        case_index = CaseIndex
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Rebuilds the metric name index.
%%
%% @end
%%--------------------------------------------------------------------
-spec rebuild_metric_index([metric()]) -> #{metric_name() => [metric()]}.

rebuild_metric_index(Metrics) ->
    lists:foldl(
        fun(M, Acc) ->
            maps:update_with(M#metric.name,
                fun(Existing) -> [M | Existing] end,
                [M],
                Acc)
        end,
        #{},
        Metrics
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Rebuilds the case index.
%%
%% @end
%%--------------------------------------------------------------------
-spec rebuild_case_index([metric()]) -> #{binary() => [metric()]}.

rebuild_case_index(Metrics) ->
    lists:foldl(
        fun(M, Acc) ->
            case maps:get(<<"case_id">>, M#metric.labels,
                         maps:get(case_id, M#metric.labels, undefined)) of
                undefined -> Acc;
                CaseId ->
                    maps:update_with(CaseId,
                        fun(Existing) -> [M | Existing] end,
                        [M],
                        Acc)
            end
        end,
        #{},
        Metrics
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Schedules the next retention cleanup.
%%
%% @end
%%--------------------------------------------------------------------
-spec schedule_retention_cleanup(non_neg_integer()) -> ok.

schedule_retention_cleanup(Interval) ->
    erlang:send_after(Interval, self(), retention_cleanup),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique dashboard ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_dashboard_id() -> binary().

generate_dashboard_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"dashboard_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts binary to existing atom, or creates new if needed.
%%
%% @end
%%--------------------------------------------------------------------
-spec binary_to_existing_atom(binary(), _Encoding) -> atom().

binary_to_existing_atom(Bin, Encoding) ->
    try
        erlang:binary_to_existing_atom(Bin, Encoding)
    catch
        error:badarg ->
            %% Fallback for metrics without existing atoms
            list_to_existing_atom(binary_to_list(Bin))
    end.
