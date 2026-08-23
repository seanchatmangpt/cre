%% -*- erlang -*-
%% @doc OpenTelemetry Metrics API Wrapper
%%
%% Provides a simplified interface for OpenTelemetry metrics in CRE.
%% Supports counters, gauges, and histograms with Prometheus export.
%%
%% @end

-module(otel_metrics).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([inc_counter/2, inc_counter/3]).
-export([set_gauge/2, set_gauge/3, inc_gauge/2, dec_gauge/2]).
-export([record_histogram/2, record_histogram/3]).
-export([get_metric/2]).
-export([export_metrics/0]).
-export([register_counter/2, register_gauge/2, register_histogram/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type metric_name() :: atom() | binary().
-type metric_value() :: number().
-type labels() :: #{atom() => term()}.
-type metric_type() :: counter | gauge | histogram.
-type metric_spec() :: #{name => metric_name(),
                         type => metric_type(),
                         help => binary(),
                         labels => [atom()]}.

-type counter() :: #{name := metric_name(),
                     value := metric_value(),
                     labels := labels()}.
-type gauge() :: #{name := metric_name(),
                   value := metric_value(),
                   labels := labels()}.
-type histogram() :: #{name := metric_name(),
                       count := non_neg_integer(),
                       sum := metric_value(),
                       buckets => [{metric_value(), non_neg_integer()}]}.

-export_type([metric_name/0, metric_value/0, labels/0, metric_spec/0]).

-define(SERVER, ?MODULE).
-define(METRICS_TABLE, otel_metrics).

-record(state, {
    counters :: atom(),
    gauges :: atom(),
    histograms :: atom(),
    metric_specs :: map()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Increment a counter by 1.
-spec inc_counter(metric_name(), labels()) -> ok.
inc_counter(Name, Labels) ->
    inc_counter(Name, Labels, 1).

%% @doc Increment a counter by a specific amount.
-spec inc_counter(metric_name(), labels(), number()) -> ok.
inc_counter(Name, Labels, Amount) when is_number(Amount), Amount > 0 ->
    gen_server:cast(?SERVER, {inc_counter, Name, Labels, Amount}).

%% @doc Set a gauge to a specific value.
-spec set_gauge(metric_name(), labels()) -> ok.
set_gauge(Name, Labels) ->
    set_gauge(Name, Labels, 1).

%% @doc Set a gauge to a specific value.
-spec set_gauge(metric_name(), labels(), metric_value()) -> ok.
set_gauge(Name, Labels, Value) when is_number(Value) ->
    gen_server:cast(?SERVER, {set_gauge, Name, Labels, Value}).

%% @doc Increment a gauge by a specific amount.
-spec inc_gauge(metric_name(), metric_value()) -> ok.
inc_gauge(Name, Amount) when is_number(Amount) ->
    gen_server:cast(?SERVER, {inc_gauge, Name, #{}, Amount}).

%% @doc Decrement a gauge by a specific amount.
-spec dec_gauge(metric_name(), metric_value()) -> ok.
dec_gauge(Name, Amount) when is_number(Amount) ->
    gen_server:cast(?SERVER, {dec_gauge, Name, #{}, Amount}).

%% @doc Record a value in a histogram.
-spec record_histogram(metric_name(), metric_value()) -> ok.
record_histogram(Name, Value) ->
    record_histogram(Name, #{}, Value).

%% @doc Record a value in a histogram with labels.
-spec record_histogram(metric_name(), labels(), metric_value()) -> ok.
record_histogram(Name, Labels, Value) when is_number(Value) ->
    gen_server:cast(?SERVER, {record_histogram, Name, Labels, Value}).

%% @doc Get the current value of a metric.
-spec get_metric(metric_name(), labels()) -> {ok, metric_value()} | {error, not_found}.
get_metric(Name, Labels) ->
    gen_server:call(?SERVER, {get_metric, Name, Labels}).

%% @doc Export all metrics in Prometheus text format.
-spec export_metrics() -> iolist().
export_metrics() ->
    gen_server:call(?SERVER, export_metrics).

%% @doc Register a counter metric.
-spec register_counter(metric_name(), binary()) -> ok.
register_counter(Name, Help) ->
    gen_server:call(?SERVER, {register, Name, counter, Help, []}).

%% @doc Register a gauge metric.
-spec register_gauge(metric_name(), binary()) -> ok.
register_gauge(Name, Help) ->
    gen_server:call(?SERVER, {register, Name, gauge, Help, []}).

%% @doc Register a histogram metric.
-spec register_histogram(metric_name(), binary()) -> ok.
register_histogram(Name, Help) ->
    gen_server:call(?SERVER, {register, Name, histogram, Help,
                              [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Counters = counter_table,
    Gauges = gauge_table,
    Histograms = histogram_table,

    %% Create tables if they don't exist
    case ets:whereis(counter_table) of
        undefined -> ets:new(counter_table, [named_table, set, public]);
        _ -> ok
    end,
    case ets:whereis(gauge_table) of
        undefined -> ets:new(gauge_table, [named_table, set, public]);
        _ -> ok
    end,
    case ets:whereis(histogram_table) of
        undefined -> ets:new(histogram_table, [named_table, set, public]);
        _ -> ok
    end,

    {ok, #state{
        counters = Counters,
        gauges = Gauges,
        histograms = Histograms,
        metric_specs = #{}
    }}.

handle_call({get_metric, Name, Labels}, _From, State) ->
    Result = do_get_metric(Name, Labels, State),
    {reply, Result, State};

handle_call(export_metrics, _From, State = #state{metric_specs = SpecMap}) ->
    Export = do_export_metrics(State),
    SpecHeader = format_specs(SpecMap),
    {reply, [SpecHeader, Export], State};

handle_call({register, Name, Type, Help, LabelNames}, _From, State) ->
    Spec = #{name => Name, type => Type, help => Help, labels => LabelNames},
    NewSpecs = maps:put(Name, Spec, State#state.metric_specs),
    {reply, ok, State#state{metric_specs = NewSpecs}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({inc_counter, Name, Labels, Amount}, State) ->
    update_counter(Name, Labels, Amount, State),
    {noreply, State};

handle_cast({set_gauge, Name, Labels, Value}, State) ->
    set_gauge_value(Name, Labels, Value, State),
    {noreply, State};

handle_cast({inc_gauge, Name, Labels, Amount}, State) ->
    update_gauge(Name, Labels, Amount, State),
    {noreply, State};

handle_cast({dec_gauge, Name, Labels, Amount}, State) ->
    update_gauge(Name, Labels, -Amount, State),
    {noreply, State};

handle_cast({record_histogram, Name, Labels, Value}, State) ->
    update_histogram(Name, Labels, Value, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec do_get_metric(metric_name(), labels(), #state{}) ->
          {ok, metric_value()} | {error, not_found}.
do_get_metric(Name, Labels, #state{counters = Counters, gauges = Gauges}) ->
    Key = {Name, Labels},
    case ets:lookup(Counters, Key) of
        [{_, Value}] -> {ok, Value};
        [] ->
            case ets:lookup(Gauges, Key) of
                [{_, Value}] -> {ok, Value};
                [] -> {error, not_found}
            end
    end.

-spec update_counter(metric_name(), labels(), number(), #state{}) -> ok.
update_counter(Name, Labels, Amount, #state{counters = Counters}) ->
    Key = {Name, Labels},
    ets:update_counter(Counters, Key, {2, Amount}, {Key, 0}),
    ok.

-spec set_gauge_value(metric_name(), labels(), number(), #state{}) -> ok.
set_gauge_value(Name, Labels, Value, #state{gauges = Gauges}) ->
    Key = {Name, Labels},
    ets:insert(Gauges, {Key, Value}),
    ok.

-spec update_gauge(metric_name(), labels(), number(), #state{}) -> ok.
update_gauge(Name, Labels, Amount, #state{gauges = Gauges}) ->
    Key = {Name, Labels},
    try
        ets:update_counter(Gauges, Key, {2, Amount})
    catch
        error:badarg ->
            ets:insert(Gauges, {Key, Amount})
    end,
    ok.

-spec update_histogram(metric_name(), labels(), number(), #state{}) -> ok.
update_histogram(Name, Labels, Value, #state{histograms = Histograms}) ->
    Key = {Name, Labels},
    case ets:lookup(Histograms, Key) of
        [{_, Count, Sum, Buckets}] ->
            NewCount = Count + 1,
            NewSum = Sum + Value,
            NewBuckets = update_buckets(Value, Buckets),
            ets:insert(Histograms, {Key, NewCount, NewSum, NewBuckets});
        [] ->
            %% Initialize histogram with default buckets
            Buckets = [{0.005, 0}, {0.01, 0}, {0.025, 0}, {0.05, 0},
                       {0.1, 0}, {0.25, 0}, {0.5, 0}, {1, 0},
                       {2.5, 0}, {5, 0}, {10, 0}, {inf, 0}],
            ets:insert(Histograms, {Key, 1, Value, update_buckets(Value, Buckets)})
    end,
    ok.

-spec update_buckets(number(), [{number(), non_neg_integer()}]) ->
          [{number(), non_neg_integer()}].
update_buckets(Value, Buckets) ->
    lists:map(fun({Bound, Count}) ->
        case Value =< Bound of
            true -> {Bound, Count + 1};
            false -> {Bound, Count}
        end
    end, Buckets).

-spec do_export_metrics(#state{}) -> iolist().
do_export_metrics(#state{counters = Counters, gauges = Gauges, histograms = Histograms}) ->
    CountersOut = export_table(Counters, counter),
    GaugesOut = export_table(Gauges, gauge),
    HistogramsOut = export_histogram_table(Histograms),
    io_lib:format("~s~s~s", [CountersOut, GaugesOut, HistogramsOut]).

-spec export_table(atom(), metric_type()) -> iolist().
export_table(TableName, Type) ->
    case ets:tab2list(TableName) of
        [] -> "";
        List ->
            Lines = lists:map(fun({{Name, Labels}, Value}) ->
                LabelStr = format_labels(Labels),
                {Suffix, NameSuffix} = case Type of
                    counter -> {"_total", "_total"};
                    gauge -> {"", ""}
                end,
                io_lib:format("~s~s~s ~p~n", [Name, NameSuffix, LabelStr, Value])
            end, List),
            Lines
    end.

-spec export_histogram_table(atom()) -> iolist().
export_histogram_table(TableName) ->
    case ets:tab2list(TableName) of
        [] -> "";
        List ->
            lists:map(fun({{Name, Labels}, Count, Sum, Buckets}) ->
                LabelStr = format_labels(Labels),
                BucketLines = format_buckets(Name, LabelStr, Buckets),
                CountLine = io_lib:format("~s~s_count ~p~n", [Name, LabelStr, Count]),
                SumLine = io_lib:format("~s~s_sum ~p~n", [Name, LabelStr, Sum]),
                [BucketLines, CountLine, SumLine, "\n"]
            end, List)
    end.

-spec format_buckets(metric_name(), string(), [{number(), non_neg_integer()}]) -> iolist().
format_buckets(Name, LabelStr, Buckets) ->
    lists:map(fun({Bound, Count}) ->
        BoundStr = format_bound(Bound),
        %% Prometheus format: metric_name_bucket{le="bound",labels} value
        case LabelStr of
            "" ->
                io_lib:format("~s_bucket{le=\"~s\"} ~p~n", [Name, BoundStr, Count]);
            _ ->
                io_lib:format("~s_bucket{le=\"~s\",~s} ~p~n",
                              [Name, BoundStr, string:sub_string(LabelStr, 2, length(LabelStr) - 1), Count])
        end
    end, Buckets).

-spec format_bound(number()) -> string().
format_bound(inf) -> "+Inf";
format_bound(Bound) when is_float(Bound) ->
    io_lib:format("~.3f", [Bound]);
format_bound(Bound) when is_integer(Bound) ->
    io_lib:format("~p", [Bound]).

-spec format_labels(labels()) -> string().
format_labels(Labels) when map_size(Labels) =:= 0 ->
    "";
format_labels(Labels) ->
    Pairs = maps:to_list(Labels),
    LabelStrs = [io_lib:format("~p=\"~s\"", [K, format_value(V)]) || {K, V} <- Pairs],
    ["{" | string:join(LabelStrs, ",")] ++ "}".

-spec format_value(term()) -> string().
format_value(V) when is_atom(V) -> atom_to_list(V);
format_value(V) when is_binary(V) -> binary_to_list(V);
format_value(V) when is_list(V) -> V;
format_value(V) -> io_lib:format("~p", [V]).

-spec suffix(metric_type()) -> string().
suffix(counter) -> "_total";
suffix(gauge) -> "";
suffix(histogram) -> "".

-spec format_specs(map()) -> iolist().
format_specs(SpecMap) ->
    maps:fold(fun(_Name, Spec, Acc) ->
        TypeStr = format_type(maps:get(type, Spec)),
        HelpStr = maps:get(help, Spec),
        [io_lib:format("# HELP ~s ~s~n# TYPE ~s ~s~n",
                      [maps:get(name, Spec), HelpStr,
                       maps:get(name, Spec), TypeStr]) | Acc]
    end, [], SpecMap).

-spec format_type(metric_type()) -> string().
format_type(counter) -> "counter";
format_type(gauge) -> "gauge";
format_type(histogram) -> "histogram".
