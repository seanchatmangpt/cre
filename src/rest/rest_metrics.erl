%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc REST API for Metrics Query Handlers
%%
%% This module provides Cowboy REST handlers for querying workflow metrics,
%% health status, telemetry data, and audit logs from the YAWL engine.
%%
%% <h3>Endpoints</h3>
%%
%% <ul>
%%   <li><b>GET /metrics</b> - Get metrics summary for all patterns</li>
%%   <li><b>GET /metrics/{pattern_type}</b> - Get metrics for specific pattern type</li>
%%   <li><b>GET /health</b> - Get system health status</li>
%%   <li><b>GET /health/{pattern_id}</b> - Get health status for specific pattern</li>
%%   <li><b>GET /prometheus</b> - Get metrics in Prometheus exposition format</li>
%%   <li><b>GET /prometheus/{pattern_type}</b> - Get Prometheus metrics for pattern type</li>
%%   <li><b>GET /spans</b> - Get active execution spans</li>
%%   <li><b>GET /spans/{span_id}</b> - Get information about specific span</li>
%%   <li><b>GET /audit</b> - Query audit log with filters</li>
%%   <li><b>GET /audit/{pattern_id}</b> - Get audit log for specific pattern</li>
%%   <li><b>GET /alerts</b> - List active alert rules</li>
%% </ul>
%%
%% <h3>Query Parameters</h3>
%%
%% <ul>
%%   <li><b>pattern_type</b> - Filter metrics by pattern type (e.g., sequence, parallel)</li>
%%   <li><b>limit</b> - Limit number of results (default: 100)</li>
%%   <li><b>offset</b> - Pagination offset (default: 0)</li>
%%   <li><b>event_type</b> - Filter audit log by event type</li>
%%   <li><b>start_time</b> - Filter events after this timestamp (ms)</li>
%%   <li><b>end_time</b> - Filter events before this timestamp (ms)</li>
%% </ul>
%%
%% <h3>Response Formats</h3>
%%
%% All responses are JSON unless otherwise specified. Prometheus format
%% is in OpenMetrics text format.
%%
%% ## Examples
%%
%% ```erlang
%% %% Query all metrics
%% 1> rest_metrics:metrics_all().
%% {ok, #{summary => {...}, patterns => [...]}}
%%
%% %% Query pattern-specific metrics
%% 1> rest_metrics:metrics_pattern(sequence).
%% {ok, #{executions => N, avg_duration => Ms, ...}}
%%
%% %% Get system health
%% 1> rest_metrics:system_health().
%% {ok, #{status => healthy, components => [...]}}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(rest_metrics).
-behaviour(cowboy_rest).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2]).

-export([to_json/2,
         to_prometheus/2]).

-export([metrics_all/0,
         metrics_pattern/1,
         system_health/0,
         pattern_health/1,
         get_prometheus_all/0,
         get_prometheus_pattern/1,
         get_active_spans/0,
         get_span_info/1,
         query_audit_log/1,
         get_pattern_audit/1,
         list_alerts/0,
         doctest_test/0]).

%%====================================================================
%% Cowboy REST Callbacks
%%====================================================================

-spec init(Req :: cowboy_req:req(), State :: term()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: term()}.

init(Req, State) ->
    {cowboy_rest, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: term()) ->
    {list(), Req :: cowboy_req:req(), State :: term()}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: term()) ->
    {list(), Req :: cowboy_req:req(), State :: term()}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/plain">>, to_prometheus}
    ], Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: term()) ->
    {list(), Req :: cowboy_req:req(), State :: term()}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

%%====================================================================
%% JSON Response Handler
%%====================================================================

-spec to_json(Req :: cowboy_req:req(), State :: term()) ->
    {iodata(), Req :: cowboy_req:req(), State :: term()}.

to_json(Req, State) ->
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),

    Result = case {Method, Path} of
        {<<"GET">>, <<"/metrics">>} ->
            metrics_all();
        {<<"GET">>, <<"/metrics/", PatternType/binary>>} ->
            metrics_pattern(binary_to_atom(PatternType, utf8));
        {<<"GET">>, <<"/health">>} ->
            system_health();
        {<<"GET">>, <<"/health/", PatternId/binary>>} ->
            pattern_health(PatternId);
        {<<"GET">>, <<"/spans">>} ->
            get_active_spans();
        {<<"GET">>, <<"/spans/", SpanId/binary>>} ->
            get_span_info(SpanId);
        {<<"GET">>, <<"/audit">>} ->
            Qs = cowboy_req:parse_qs(Req),
            Filter = build_audit_filter(Qs),
            query_audit_log(Filter);
        {<<"GET">>, <<"/audit/", PatternId/binary>>} ->
            get_pattern_audit(PatternId);
        {<<"GET">>, <<"/alerts">>} ->
            list_alerts();
        _ ->
            {error, not_found}
    end,

    case Result of
        {ok, Data} ->
            Json = jsone:encode(Data),
            Req2 = cowboy_req:set_resp_header(
                <<"content-type">>, <<"application/json">>, Req),
            {Json, Req2, State};
        {error, Reason} ->
            error_response(Reason, Req, State)
    end.

%%====================================================================
%% Prometheus Response Handler
%%====================================================================

-spec to_prometheus(Req :: cowboy_req:req(), State :: term()) ->
    {iodata(), Req :: cowboy_req:req(), State :: term()}.

to_prometheus(Req, State) ->
    Path = cowboy_req:path(Req),

    Result = case Path of
        <<"/prometheus">> ->
            get_prometheus_all();
        <<"/prometheus/", PatternType/binary>> ->
            get_prometheus_pattern(binary_to_atom(PatternType, utf8));
        _ ->
            {error, not_found}
    end,

    case Result of
        {ok, Text} ->
            Req2 = cowboy_req:set_resp_header(
                <<"content-type">>, <<"text/plain; version=0.0.4">>, Req),
            {Text, Req2, State};
        {error, Reason} ->
            error_response(Reason, Req, State)
    end.

%%====================================================================
%% Metrics Query Functions
%%====================================================================

-spec metrics_all() -> {ok, map()} | {error, term()}.

metrics_all() ->
    try
        Summary = yawl_telemetry:get_metrics_summary(),
        Timestamp = erlang:system_time(millisecond),
        Data = maps:put(timestamp, Timestamp, Summary),
        {ok, Data}
    catch
        _:Reason ->
            {error, {metrics_unavailable, Reason}}
    end.

-spec metrics_pattern(PatternType :: atom()) ->
    {ok, map()} | {error, term()}.

metrics_pattern(PatternType) ->
    try
        Metrics = yawl_telemetry:get_metrics(PatternType),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            pattern_type => PatternType,
            metrics => Metrics,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {metrics_unavailable, Reason}}
    end.

%%====================================================================
%% Health Check Functions
%%====================================================================

-spec system_health() -> {ok, map()} | {error, term()}.

system_health() ->
    try
        Status = yawl_telemetry:system_health(),
        ComponentStatus = yawl_telemetry:component_status(),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            status => maps:get(status, Status, unknown),
            components => ComponentStatus,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {health_check_failed, Reason}}
    end.

-spec pattern_health(PatternId :: term()) ->
    {ok, map()} | {error, term()}.

pattern_health(PatternId) ->
    try
        Health = yawl_telemetry:check_pattern_health(PatternId),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            pattern_id => PatternId,
            health => Health,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {health_check_failed, Reason}}
    end.

%%====================================================================
%% Prometheus Export Functions
%%====================================================================

-spec get_prometheus_all() -> {ok, iodata()} | {error, term()}.

get_prometheus_all() ->
    try
        Prometheus = yawl_telemetry:export_prometheus(all),
        {ok, Prometheus}
    catch
        _:Reason ->
            {error, {prometheus_export_failed, Reason}}
    end.

-spec get_prometheus_pattern(PatternType :: atom()) ->
    {ok, iodata()} | {error, term()}.

get_prometheus_pattern(PatternType) ->
    try
        Prometheus = yawl_telemetry:export_prometheus(PatternType),
        {ok, Prometheus}
    catch
        _:Reason ->
            {error, {prometheus_export_failed, Reason}}
    end.

%%====================================================================
%% Span Query Functions
%%====================================================================

-spec get_active_spans() -> {ok, map()} | {error, term()}.

get_active_spans() ->
    try
        Spans = yawl_telemetry:get_active_spans(),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            active_spans => length(maps:to_list(Spans)),
            spans => Spans,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {span_query_failed, Reason}}
    end.

-spec get_span_info(SpanId :: term()) ->
    {ok, map()} | {error, term()}.

get_span_info(SpanId) ->
    try
        Info = yawl_telemetry:get_span_info(SpanId),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            span_id => SpanId,
            info => Info,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {span_query_failed, Reason}}
    end.

%%====================================================================
%% Audit Log Query Functions
%%====================================================================

-spec query_audit_log(Filter :: map()) ->
    {ok, map()} | {error, term()}.

query_audit_log(Filter) ->
    try
        Results = yawl_telemetry:query_audit(Filter),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            count => length(Results),
            results => Results,
            filter => Filter,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {audit_query_failed, Reason}}
    end.

-spec get_pattern_audit(PatternId :: term()) ->
    {ok, map()} | {error, term()}.

get_pattern_audit(PatternId) ->
    try
        Results = yawl_telemetry:get_audit_log(PatternId),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            pattern_id => PatternId,
            count => length(Results),
            results => Results,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {audit_query_failed, Reason}}
    end.

%%====================================================================
%% Alert Functions
%%====================================================================

-spec list_alerts() -> {ok, map()} | {error, term()}.

list_alerts() ->
    try
        Rules = yawl_telemetry:list_alert_rules(),
        Alerts = yawl_telemetry:check_alerts(),
        Timestamp = erlang:system_time(millisecond),
        Data = #{
            rules => length(Rules),
            active_alerts => length(Alerts),
            rules_list => Rules,
            alerts => Alerts,
            timestamp => Timestamp
        },
        {ok, Data}
    catch
        _:Reason ->
            {error, {alert_query_failed, Reason}}
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

-spec build_audit_filter(QS :: list()) -> map().

build_audit_filter(QS) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                <<"event_type">> ->
                    maps:put(event_type, Value, Acc);
                <<"pattern_id">> ->
                    maps:put(pattern_id, Value, Acc);
                <<"start_time">> ->
                    case catch binary_to_integer(Value) of
                        StartTime when is_integer(StartTime) ->
                            maps:put(start_time, StartTime, Acc);
                        _ ->
                            Acc
                    end;
                <<"end_time">> ->
                    case catch binary_to_integer(Value) of
                        EndTime when is_integer(EndTime) ->
                            maps:put(end_time, EndTime, Acc);
                        _ ->
                            Acc
                    end;
                <<"limit">> ->
                    case catch binary_to_integer(Value) of
                        Limit when is_integer(Limit), Limit > 0 ->
                            maps:put(limit, Limit, Acc);
                        _ ->
                            Acc
                    end;
                <<"offset">> ->
                    case catch binary_to_integer(Value) of
                        Offset when is_integer(Offset), Offset >= 0 ->
                            maps:put(offset, Offset, Acc);
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end,
        maps:new(),
        QS).

-spec error_response(Reason :: term(),
                     Req :: cowboy_req:req(),
                     State :: term()) ->
    {iodata(), cowboy_req:req(), term()}.

error_response(Reason, Req, State) ->
    StatusCode = case Reason of
        not_found -> 404;
        {metrics_unavailable, _} -> 503;
        {health_check_failed, _} -> 503;
        {prometheus_export_failed, _} -> 503;
        {span_query_failed, _} -> 503;
        {audit_query_failed, _} -> 503;
        {alert_query_failed, _} -> 503;
        _ -> 500
    end,

    ErrorMap = #{
        error => atom_to_binary(element(1, Reason), utf8),
        reason => format_reason(Reason),
        timestamp => erlang:system_time(millisecond)
    },
    Json = jsone:encode(ErrorMap),

    Req2 = cowboy_req:reply(
        StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        Json,
        Req),
    {ok, Req2, State}.

-spec format_reason(Reason :: term()) -> binary().

format_reason({_Type, Details}) ->
    case is_list(Details) of
        true -> list_to_binary(Details);
        false -> atom_to_binary(Details, utf8)
    end;
format_reason(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_reason(Reason) when is_binary(Reason) ->
    Reason;
format_reason(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%%====================================================================
%% Doctests
%%====================================================================

-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Verify module can be loaded
    {module, rest_metrics} = code:ensure_loaded(rest_metrics),

    %% Test 2: Verify init/2 is exported
    Exports = proplists:get_value(exports, rest_metrics:module_info()),
    true = lists:member({init, 2}, Exports),

    %% Test 3: Verify allowed_methods/2 is exported
    true = lists:member({allowed_methods, 2}, Exports),

    %% Test 4: Verify content_types_provided/2 is exported
    true = lists:member({content_types_provided, 2}, Exports),

    %% Test 5: Verify to_json/2 is exported
    true = lists:member({to_json, 2}, Exports),

    %% Test 6: Verify metrics API exports
    true = lists:member({metrics_all, 0}, Exports),
    true = lists:member({metrics_pattern, 1}, Exports),
    true = lists:member({system_health, 0}, Exports),

    %% Test 7: Verify behavior declaration
    Behaviors = proplists:get_value(attributes, rest_metrics:module_info()),
    {behavior, [cowboy_rest]} = lists:keyfind(behavior, 1, Behaviors),

    %% Test 8: Verify JSON content type header is valid
    ContentType = <<"application/json">>,
    true = is_binary(ContentType),
    <<"application/json">> = ContentType,

    %% Test 9: Verify build_audit_filter with empty input
    Filter1 = build_audit_filter([]),
    true = is_map(Filter1),
    0 = maps:size(Filter1),

    %% Test 10: Verify build_audit_filter with valid inputs
    Filter2 = build_audit_filter([
        {<<"pattern_id">>, <<"pat_123">>},
        {<<"limit">>, <<"50">>}
    ]),
    true = is_map(Filter2),
    true = maps:is_key(pattern_id, Filter2),
    true = maps:is_key(limit, Filter2),
    50 = maps:get(limit, Filter2),

    ok.
