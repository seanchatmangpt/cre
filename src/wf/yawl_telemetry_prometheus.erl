%% -*- erlang -*-
%%%% @doc Prometheus Metrics Exporter for CRE YAWL
%%
%% This module exports workflow metrics in Prometheus format for
%% scraping by monitoring systems like Prometheus itself.
%%
%% Metrics exported:
%% - cre_workflow_active: Gauge of currently active workflows
%% - cre_workflow_completed: Counter of completed workflows
%% - cre_workflow_failed: Counter of failed workflows
%% - cre_fire_duration_us: Histogram of fire/3 execution time in microseconds
%% - cre_transition_total: Counter of total transition firings
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_telemetry_prometheus).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([init/0]).
-export([inc_workflow_completed/0, inc_workflow_failed/0]).
-export([set_active_workflows/1]).
-export([record_fire_duration/1]).
-export([inc_transition_count/1]).
-export([format_metrics/0]).
-export([metrics_handler/1]).
-export([get_metrics/0]).

%%====================================================================
%% Types
%%====================================================================

-type metric_name() :: atom().
-type metric_value() :: number().
-type labels() :: [{atom(), term()}].
-type metric() :: {metric_name(), metric_value(), labels()}.

-record(metrics, {
    workflow_active = 0 :: non_neg_integer(),
    workflow_completed = 0 :: non_neg_integer(),
    workflow_failed = 0 :: non_neg_integer(),
    fire_duration_sum = 0 :: non_neg_integer(),
    fire_duration_count = 0 :: non_neg_integer(),
    transition_counts = #{} :: #{atom() => non_neg_integer()}
}).

-type metrics() :: #metrics{}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the metrics system.
%%
%% Creates the ETS table for storing metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    case ets:whereis(yawl_prometheus_metrics) of
        undefined ->
            ets:new(yawl_prometheus_metrics, [named_table, public, set]),
            ets:insert(yawl_prometheus_metrics, #metrics{}),
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Increments the workflow completed counter.
%%
%% @end
%%--------------------------------------------------------------------
-spec inc_workflow_completed() -> ok.
inc_workflow_completed() ->
    update_metrics(fun(M) -> M#metrics{workflow_completed = M#metrics.workflow_completed + 1} end).

%%--------------------------------------------------------------------
%% @doc Increments the workflow failed counter.
%%
%% @end
%%--------------------------------------------------------------------
-spec inc_workflow_failed() -> ok.
inc_workflow_failed() ->
    update_metrics(fun(M) -> M#metrics{workflow_failed = M#metrics.workflow_failed + 1} end).

%%--------------------------------------------------------------------
%% @doc Sets the gauge of active workflows.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_active_workflows(non_neg_integer()) -> ok.
set_active_workflows(Count) when is_integer(Count), Count >= 0 ->
    update_metrics(fun(M) -> M#metrics{workflow_active = Count} end).

%%--------------------------------------------------------------------
%% @doc Records a fire/3 execution duration.
%%
%% @end
%%--------------------------------------------------------------------
-spec record_fire_duration(non_neg_integer()) -> ok.
record_fire_duration(DurationUs) when is_integer(DurationUs), DurationUs >= 0 ->
    update_metrics(fun(M = #metrics{fire_duration_sum = Sum, fire_duration_count = Count}) ->
        M#metrics{
            fire_duration_sum = Sum + DurationUs,
            fire_duration_count = Count + 1
        }
    end).

%%--------------------------------------------------------------------
%% @doc Increments the counter for a specific transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec inc_transition_count(atom()) -> ok.
inc_transition_count(Transition) when is_atom(Transition) ->
    update_metrics(fun(M = #metrics{transition_counts = Counts}) ->
        NewCount = maps:get(Transition, Counts, 0) + 1,
        M#metrics{transition_counts = Counts#{Transition => NewCount}}
    end).

%%--------------------------------------------------------------------
%% @doc Formats all metrics in Prometheus text format.
%%
%% Returns the metrics as an iolist suitable for HTTP response.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_metrics() -> iolist().
format_metrics() ->
    Metrics = get_metrics(),

    %% Gauge metrics
    [
        "# HELP cre_workflow_active Number of currently active workflows\n",
        "# TYPE cre_workflow_active gauge\n",
        io_lib:format("cre_workflow_active ~p\n", [Metrics#metrics.workflow_active]),

        "# HELP cre_workflow_completed Total number of completed workflows\n",
        "# TYPE cre_workflow_completed counter\n",
        io_lib:format("cre_workflow_completed ~p\n", [Metrics#metrics.workflow_completed]),

        "# HELP cre_workflow_failed Total number of failed workflows\n",
        "# TYPE cre_workflow_failed counter\n",
        io_lib:format("cre_workflow_failed ~p\n", [Metrics#metrics.workflow_failed]),

        "# HELP cre_fire_duration_us Fire/3 execution time in microseconds\n",
        "# TYPE cre_fire_duration_us histogram\n",
        format_histogram(Metrics),

        "# HELP cre_transition_total Total number of transition firings\n",
        "# TYPE cre_transition_total counter\n",
        format_transitions(Metrics#metrics.transition_counts)
    ].

%%--------------------------------------------------------------------
%% @doc HTTP handler for /metrics endpoint.
%%
%% Returns a Cowboy response with metrics in Prometheus format.
%%
%% @end
%%--------------------------------------------------------------------
-spec metrics_handler(req()) -> req().
metrics_handler(Req) ->
    MetricsBody = format_metrics(),
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain; version=0.0.4">>
    }, MetricsBody, Req),
    Req2.

%%--------------------------------------------------------------------
%% @doc Gets the current metrics state.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_metrics() -> metrics().
get_metrics() ->
    case ets:lookup(yawl_prometheus_metrics, metrics) of
        [{metrics, M}] -> M;
        [] -> #metrics{}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec update_metrics(fun((metrics()) -> metrics())) -> ok.
update_metrics(UpdateFun) ->
    case ets:lookup(yawl_prometheus_metrics, metrics) of
        [{metrics, Metrics}] ->
            NewMetrics = UpdateFun(Metrics),
            ets:insert(yawl_prometheus_metrics, {metrics, NewMetrics}),
            ok;
        [] ->
            %% Not initialized, initialize and retry
            init(),
            update_metrics(UpdateFun)
    end.

%% @private
-spec format_histogram(metrics()) -> iolist().
format_histogram(#metrics{fire_duration_sum = Sum, fire_duration_count = Count}) when Count > 0 ->
    Avg = Sum div Count,
    [
        io_lib:format("cre_fire_duration_us_sum ~p\n", [Sum]),
        io_lib:format("cre_fire_duration_us_count ~p\n", [Count]),
        io_lib:format("cre_fire_duration_us_avg ~p\n", [Avg]),
        io_lib:format("cre_fire_duration_us_bucket{le=\"+Inf\"} ~p\n", [Count])
    ];
format_histogram(_) ->
    [
        "cre_fire_duration_us_sum 0\n",
        "cre_fire_duration_us_count 0\n",
        "cre_fire_duration_us_avg 0\n",
        "cre_fire_duration_us_bucket{le=\"+Inf\"} 0\n"
    ].

%% @private
-spec format_transitions(#{atom() => non_neg_integer()}) -> iolist().
format_transitions(Counts) ->
    maps:fold(fun(Transition, Count, Acc) ->
        [Acc, io_lib:format("cre_transition_total{transition=\"~p\"} ~p\n", [Transition, Count])]
    end, [], Counts).

%%====================================================================
%% Dummy Types for Cowboy
%%====================================================================

-type req() :: term().
