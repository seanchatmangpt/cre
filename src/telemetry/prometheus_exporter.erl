%% -*- erlang -*-
%% @doc Prometheus Exporter for OpenTelemetry Metrics
%%
%% HTTP endpoint for serving metrics in Prometheus text format.
%% Implements gen_server for managing the metrics export endpoint.
%%
%% @end

-module(prometheus_exporter).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([get_metrics/0]).
-export([set_port/1]).
-export([health/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Cowboy REST handlers
-export([init/2]).
-export([content_types_provided/2]).
-export([to_health/2, to_metrics/2, to_json_metrics/2]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type port_number() :: 1..65535.
-type option() :: {port, port_number()} | {ip, inet:ip_address()}.
-type state() :: #{port := port_number(),
                   listener := ranch:ref(),
                   ip => inet:ip_address()}.

-export_type([port_number/0, option/0, state/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 9091).
-define(METRICS_ROUTE, "/metrics").
-define(HEALTH_ROUTE, "/health").

%%====================================================================
%% API
%%====================================================================

%% @doc Start the Prometheus exporter.
-spec start_link([option()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Get all metrics in Prometheus text format.
-spec get_metrics() -> iolist().
get_metrics() ->
    otel_metrics:export_metrics().

%% @doc Set the port for the metrics endpoint (requires restart).
-spec set_port(port_number()) -> ok.
set_port(Port) ->
    gen_server:call(?SERVER, {set_port, Port}).

%% @doc Health check for the exporter.
-spec health() -> up | down.
health() ->
    case whereis(?SERVER) of
        undefined -> down;
        _Pid ->
            try gen_server:call(?SERVER, health) of
                ok -> up
            catch
                _:_ -> down
            end
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    IP = proplists:get_value(ip, Options, {0, 0, 0, 0}),

    Dispatch = cowboy_router:compile([
        {'_', [
            {?HEALTH_ROUTE, ?MODULE, health_handler},
            {?METRICS_ROUTE, ?MODULE, metrics_handler}
        ]}
    ]),

    case cowboy:start_clear(http_listener,
                            [{ip, IP}, {port, Port}],
                            #{env => #{dispatch => Dispatch}}) of
        {ok, _} ->
            ?LOG(info, "Prometheus exporter listening on ~p:~p", [IP, Port]),
            {ok, #{port => Port, listener => http_listener, ip => IP}};
        {error, Reason} ->
            ?LOG(error, "Failed to start Prometheus exporter: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call({set_port, _NewPort}, _From, State) ->
    {reply, {error, port_change_requires_restart}, State};

handle_call(health, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{listener := Listener}) ->
    cowboy:stop_listener(Listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% HTTP Handlers
%%====================================================================

%% Health handler
init(Req, State) ->
    Path = cowboy_req:path(Req),
    handle_request(Path, Req, State).

handle_request(<<"/health">>, Req, State) ->
    {cowboy_rest, Req, #{handler => health}};
handle_request(<<"/metrics">>, Req, State) ->
    {cowboy_rest, Req, #{handler => metrics}};
handle_request(_Path, Req, State) ->
    {cowboy_rest, Req, #{handler => unknown}}.

content_types_provided(Req, State = #{handler := health}) ->
    {[
        {{<<"text">>, <<"plain">>, []}, to_health}
    ], Req, State};
content_types_provided(Req, State = #{handler := metrics}) ->
    {[
        {{<<"text">>, <<"plain">>, [{<<"version">>, <<"0.0.4">>}]}, to_metrics},
        {{<<"application">>, <<"json">>, []}, to_json_metrics}
    ], Req, State}.

to_health(Req, State) ->
    Body = <<"OK\n">>,
    {Body, Req, State}.

to_metrics(Req, State) ->
    Metrics = otel_metrics:export_metrics(),
    Body = iolist_to_binary(Metrics),
    Headers = #{<<"content-type">> => <<"text/plain; version=0.0.4; charset=utf-8">>},
    {Body, Req, State, Headers}.

to_json_metrics(Req, State) ->
    Metrics = otel_metrics:export_metrics(),
    %% Convert Prometheus text to JSON format
    Json = prometheus_to_json(Metrics),
    Body = jsone:encode(Json),
    Headers = #{<<"content-type">> => <<"application/json">>},
    {Body, Req, State, Headers}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Convert Prometheus text format to JSON.
-spec prometheus_to_json(iolist()) -> map().
prometheus_to_json(Text) ->
    Lines = binary:split(iolist_to_binary(Text), <<"\n">>, [global]),
    {Metrics, _} = lists:foldl(fun parse_metric_line/2, {[], []}, Lines),
    #{metrics => lists:reverse(Metrics)}.

%% @private Parse a single Prometheus metric line.
-spec parse_metric_line(binary(), {[map()], [binary()]}) -> {[map()], [binary()]}.
parse_metric_line(<<>>, Acc) ->
    Acc;
parse_metric_line(<<$#, _Rest/binary>>, Acc) ->
    %% Skip help and type comments
    Acc;
parse_metric_line(Line, {Metrics, Acc}) ->
    case binary:split(Line, <<" ">>) of
        [NameWithLabels, ValueBin] ->
            Value = binary_to_number(ValueBin),
            {Name, Labels} = parse_name_labels(NameWithLabels),
            {[#{name => Name, labels => Labels, value => Value} | Metrics], Acc};
        _ ->
            {Metrics, Acc}
    end.

%% @private Parse metric name and labels from a metric line.
-spec parse_name_labels(binary()) -> {binary(), map()}.
parse_name_labels(NameWithLabels) ->
    case binary:split(NameWithLabels, <<"{">>) of
        [Name, <<>>] ->
            {Name, #{}};
        [Name, Rest] ->
            case binary:split(Rest, <<"}">>) of
                [LabelsBin, <<>>] ->
                    Labels = parse_labels(LabelsBin),
                    {Name, Labels};
                _ ->
                    {NameWithLabels, #{}}
            end;
        [Name] ->
            {Name, #{}}
    end.

%% @private Parse labels from a label string.
-spec parse_labels(binary()) -> map().
parse_labels(LabelsBin) ->
    Pairs = binary:split(LabelsBin, <<",">>, [global]),
    lists:foldl(fun(Pair, Acc) ->
        case binary:split(Pair, <<"=">>) of
            [Key, Value] ->
                CleanValue = strip_quotes(Value),
                maps:put(Key, CleanValue, Acc);
            _ ->
                Acc
        end
    end, #{}, Pairs).

%% @private Strip quotes from a value.
-spec strip_quotes(binary()) -> binary().
strip_quotes(<<$", Rest/binary>>) ->
    Size = byte_size(Rest) - 1,
    case Rest of
        <<Value:Size/binary, $">> -> Value;
        _ -> Rest
    end;
strip_quotes(Value) ->
    Value.

%% @private Convert binary to number.
-spec binary_to_number(binary()) -> number().
binary_to_number(Bin) ->
    try binary_to_integer(Bin)
    catch
        error:_ ->
            try binary_to_float(Bin)
            catch
                error:_ -> 0
            end
    end.
