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
%% @doc YAWL Telemetry and Observability Module
%%
%% This module provides comprehensive monitoring, observability, and
%% distributed tracing capabilities for YAWL workflow pattern execution.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>OpenTelemetry Integration:</b> Span creation and management for pattern execution</li>
%%   <li><b>Metrics Collection:</b> Execution metrics, timing, counters with tags</li>
%%   <li><b>Distributed Tracing:</b> Trace context propagation across distributed nodes</li>
%%   <li><b>Health Checks:</b> Pattern-level and system-wide health monitoring</li>
%%   <li><b>Execution Visualization:</b> DOT export and execution timeline</li>
%%   <li><b>Alerting:</b> Configurable alert rules with actions</li>
%%   <li><b>Audit Logging:</b> Event logging and state change tracking</li>
%%   <li><b>Prometheus Export:</b> Metrics in Prometheus format</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Generate trace IDs for distributed tracing:
%% ```
%% 1> TraceId = yawl_telemetry:generate_trace_id().
%% <<16,256,...>>
%% 2> byte_size(TraceId).
%% 32
%% ```
%%
%% Generate span IDs:
%% ```
%% 1> SpanId = yawl_telemetry:generate_span_id().
%% <<16,256,...>>
%% 2> byte_size(SpanId).
%% 16
%% ```
%%
%% Trace context injection and extraction:
%% ```
%% 1> Ctx = #{trace_id => <<"0123456789abcdef0123456789abcdef">>, span_id => <<"0123456789abcdef">>}.
%% #{span_id => <<"0123456789abcdef">>,trace_id => <<"0123456789abcdef0123456789abcdef">>}
%% 2> Headers = yawl_telemetry:inject_trace_context(Ctx, #{}).
%% #{<<"traceparent">> => <<"00-0123456789abcdef0123456789abcdef-0123456789abcdef-01">>,...}
%% 3> Extracted = yawl_telemetry:extract_trace_context(Headers).
%% #{span_id => <<"0123456789abcdef">>,trace_id => <<"0123456789abcdef0123456789abcdef">>}
%% ```
%%
%% List to map conversion helper:
%% ```
%% 1> yawl_telemetry:list_to_map([{a, 1}, {b, 2}]).
%% #{a => 1,b => 2}
%% 2> yawl_telemetry:list_to_map([]).
%% #{}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_telemetry).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Telemetry Management
-export([start_telemetry/0, start_telemetry/1, stop_telemetry/0]).
-export([get_telemetry_config/0, update_config/1]).

%% Span Management (OpenTelemetry-style)
-export([start_span/2, start_span/3, end_span/2, end_span/3]).
-export([span_attribute/3, span_event/2, span_status/2]).
-export([get_active_spans/0, get_span_info/1]).

%% Metrics Collection
-export([record_execution_start/2, record_execution_complete/3]).
-export([record_execution_error/3, record_timing/3, increment_counter/2]).
-export([get_metrics/1, get_metrics_summary/0]).
-export([export_prometheus/0, export_prometheus/1]).

%% Distributed Tracing
-export([get_trace_context/0, set_trace_context/1]).
-export([inject_trace_context/1, extract_trace_context/1]).
-export([generate_trace_id/0, generate_span_id/0]).

%% Health Checks
-export([check_pattern_health/1, system_health/0, component_status/0]).
-export([register_health_check/2, unregister_health_check/1]).

%% Execution Visualization
-export([execution_graph/1, export_dot/1, execution_timeline/1]).
-export([get_execution_tree/1, visualize_pattern/2]).

%% Alerting
-export([add_alert_rule/2, remove_alert_rule/1, check_alerts/0]).
-export([list_alert_rules/0, trigger_alert/1]).

%% Audit Logging
-export([log_event/3, log_state_change/3, query_audit/1]).
-export([get_audit_log/1, export_audit_log/2]).
-export([clear_audit_log/0, clear_audit_log/1]).

%% Doctests and Helper Functions
-export([doctest_test/0, list_to_map/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_METRICS_RETENTION, 86400000). %% 24 hours
-define(DEFAULT_AUDIT_RETENTION, 604800000). %% 7 days
-define(MAX_ACTIVE_SPANS, 10000).

%%====================================================================
%% Records
%%====================================================================

-record(span, {
    id,
    trace_id,
    parent_id,
    pattern_type,
    pattern_id,
    start_time,
    end_time,
    status,
    attributes,
    events
}).

-record(metric, {
    name,
    value,
    timestamp,
    tags
}).

-record(alert_rule, {
    id,
    name,
    condition,
    action,
    enabled,
    last_triggered,
    trigger_count
}).

-record(audit_entry, {
    id,
    timestamp,
    event_type,
    pattern_id,
    details
}).

-record(health_check, {
    id,
    name,
    check_fn,
    last_result,
    last_check
}).

-record(telemetry_state, {
    active_spans,
    completed_spans,
    metrics,
    alert_rules,
    audit_log,
    health_checks,
    trace_context,
    config,
    start_time
}).

%%====================================================================
%% Types
%%====================================================================

-type span_id() :: reference().
-type trace_id() :: binary().
-type pattern_type() :: atom().
-type pattern_id() :: term().
-type health_status() :: healthy | degraded | unhealthy.
-type alert_id() :: reference().

-export_type([span_id/0, trace_id/0, pattern_type/0, health_status/0]).

%%====================================================================
%% Telemetry Management
%%====================================================================

start_telemetry() ->
    start_telemetry([]).

start_telemetry(Config) when is_list(Config) ->
    start_telemetry(list_to_map(Config));
start_telemetry(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop_telemetry() ->
    gen_server:stop(?MODULE).

get_telemetry_config() ->
    gen_server:call(?MODULE, get_config).

update_config(Config) ->
    gen_server:cast(?MODULE, {update_config, Config}).

%%====================================================================
%% Span Management (OpenTelemetry-style)
%%====================================================================

%% @doc Start a new span with default empty attributes.
%%
%% Creates a new telemetry span for tracking pattern execution.
%% Returns {ok, SpanId} where SpanId is a unique reference.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> {ok, SpanId} = yawl_telemetry:start_span(sequence, workflow_1).
%% {ok, #Ref<0.0.0.123>}
%% 2> is_reference(SpanId).
%% true
%% ```
-spec start_span(pattern_type(), pattern_id()) -> {ok, span_id()} | {error, term()}.
start_span(PatternType, PatternId) ->
    start_span(PatternType, PatternId, maps:new()).

%% @doc Start a new span with custom attributes.
%%
%% Creates a new telemetry span with initial attributes for
%% tracking pattern execution with metadata.
%%
%% Example:
%% ```
%% 1> Attrs = #{priority => high, source => api}.
%% 2> {ok, SpanId} = yawl_telemetry:start_split(and_join, workflow_2, Attrs).
%% {ok, #Ref<0.0.0.456>}
%% ```
-spec start_span(pattern_type(), pattern_id(), map()) -> {ok, span_id()} | {error, term()}.
start_span(PatternType, PatternId, Attributes) ->
    gen_server:call(?MODULE, {start_span, PatternType, PatternId, Attributes}).

%% @doc End a span with result and default `ok` status.
%%
%% Marks a span as complete with the given result value.
%% Equivalent to `end_span(SpanId, Result, ok)`.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:end_span(SpanId, {completed, success}).
%% ok
%% ```
-spec end_span(span_id(), term()) -> ok.
end_span(SpanId, Result) ->
    end_span(SpanId, Result, ok).

%% @doc End a span with result and custom status.
%%
%% Marks a span as complete with the specified result and status.
%% Status can be `ok`, `error`, or any other term indicating outcome.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:end_span(SpanId, {data, Result}, ok).
%% ok
%% 2> yawl_telemetry:end_span(ErrorSpanId, {failed, timeout}, error).
%% ok
%% ```
-spec end_span(span_id(), term(), atom()) -> ok.
end_span(SpanId, Result, Status) ->
    gen_server:cast(?MODULE, {end_span, SpanId, Result, Status}).

%% @doc Add a key-value attribute to an active span.
%%
%% Sets or updates an attribute on the span for additional context.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:span_attribute(SpanId, user_id, 12345).
%% ok
%% 2> yawl_telemetry:span_attribute(SpanId, tags, [<<"important">>]).
%% ok
%% ```
-spec span_attribute(span_id(), term(), term()) -> ok.
span_attribute(SpanId, Key, Value) ->
    gen_server:cast(?MODULE, {span_attribute, SpanId, Key, Value}).

%% @doc Record an event on an active span.
%%
%% Adds an event name to the span's event timeline for debugging.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:span_event(SpanId, task_started).
%% ok
%% 2> yawl_telemetry:span_event(SpanId, checkpoint_reached).
%% ok
%% ```
-spec span_event(span_id(), atom()) -> ok.
span_event(SpanId, EventName) ->
    gen_server:cast(?MODULE, {span_event, SpanId, EventName}).

%% @doc Set the status of an active span.
%%
%% Updates the span status to indicate the current state.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:span_status(SpanId, ok).
%% ok
%% 2> yawl_telemetry:span_status(SpanId, error).
%% ok
%% ```
-spec span_status(span_id(), atom()) -> ok.
span_status(SpanId, Status) ->
    gen_server:cast(?MODULE, {span_status, SpanId, Status}).

%% @doc Get list of all active spans.
%%
%% Returns a list of tuples containing {SpanId, PatternType, PatternId}
%% for all currently active (non-completed) spans.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> yawl_telemetry:get_active_spans().
%% [{#Ref<0.0.0.123>, sequence, workflow_1}]
%% ```
-spec get_active_spans() -> [{span_id(), pattern_type(), pattern_id()}].
get_active_spans() ->
    gen_server:call(?MODULE, get_active_spans).

%% @doc Get detailed information about a specific span.
%%
%% Returns a map with span details including id, trace_id, pattern_type,
%% pattern_id, start_time, duration, status, attributes, and events.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> {ok, Info} = yawl_telemetry:get_span_info(SpanId).
%% {ok, #{id => #Ref<0.0.0.123>, pattern_type => sequence, ...}}
%% 2> maps:get(duration, Info).
%% 1250
%% ```
-spec get_span_info(span_id()) -> {ok, map()} | {error, term()}.
get_span_info(SpanId) ->
    gen_server:call(?MODULE, {get_span_info, SpanId}).

%%====================================================================
%% Metrics Collection
%%====================================================================

%% @doc Record the start of a pattern execution.
%%
%% Creates a metric entry marking when a pattern began execution.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> yawl_telemetry:record_execution_start(sequence, workflow_1).
%% ok
%% ```
-spec record_execution_start(pattern_type(), pattern_id()) -> ok.
record_execution_start(PatternType, PatternId) ->
    gen_server:cast(?MODULE, {record_execution_start, PatternType, PatternId}).

record_execution_complete(PatternType, PatternId, Duration) ->
    gen_server:cast(?MODULE, {record_execution_complete, PatternType, PatternId, Duration}).

%% @doc Record a pattern execution error.
%%
%% Creates a metric entry for failed pattern executions with the reason.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> yawl_telemetry:record_execution_error(sequence, workflow_1, timeout).
%% ok
%% ```
-spec record_execution_error(pattern_type(), pattern_id(), term()) -> ok.
record_execution_error(PatternType, PatternId, Reason) ->
    gen_server:cast(?MODULE, {record_execution_error, PatternType, PatternId, Reason}).

record_timing(PatternType, Stage, Duration) ->
    gen_server:cast(?MODULE, {record_timing, PatternType, Stage, Duration}).

%% @doc Increment a named counter metric with tags.
%%
%% Records a counter increment with optional tags for categorization.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> yawl_telemetry:increment_counter(workflow_started, #{type => sequential}).
%% ok
%% ```
-spec increment_counter(atom(), map()) -> ok.
increment_counter(MetricName, Tags) ->
    gen_server:cast(?MODULE, {increment_counter, MetricName, Tags}).

get_metrics(PatternType) ->
    gen_server:call(?MODULE, {get_metrics, PatternType}).

%% @doc Get a summary of all metrics.
%%
%% Returns aggregated metrics including total_executions, completions,
%% errors, avg_duration_ms, and counts.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> {ok, Summary} = yawl_telemetry:get_metrics_summary().
%% {ok, #{total_executions => 10, completions => 8, errors => 1, ...}}
%% ```
-spec get_metrics_summary() -> {ok, map()}.
get_metrics_summary() ->
    gen_server:call(?MODULE, get_metrics_summary).

export_prometheus() ->
    export_prometheus(all).

%% @doc Export metrics in Prometheus text format.
%%
%% Returns a binary containing metrics formatted for Prometheus scraping.
%% Can filter by pattern type or use 'all' for complete export.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> {ok, PrometheusText} = yawl_telemetry:export_prometheus(sequence).
%% {ok, <<"# HELP pattern_execution_start YAWL pattern metric\n# TYPE pattern_execution_start gauge\n...">>}
%% 2> {ok, AllMetrics} = yawl_telemetry:export_prometheus().
%% {ok, <<"# HELP ...">>}
%% ```
-spec export_prometheus(all | pattern_type()) -> {ok, binary()}.
export_prometheus(PatternType) ->
    gen_server:call(?MODULE, {export_prometheus, PatternType}).

%%====================================================================
%% Distributed Tracing
%%====================================================================

%% @doc Get the current trace context from the process dictionary.
%%
%% Returns the trace context map containing `trace_id` and `span_id`
%% if previously set, otherwise returns `undefined`.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:get_trace_context().
%% undefined
%% 2> yawl_telemetry:set_trace_context(#{trace_id => <<"abc123">>}).
%% ok
%% 3> yawl_telemetry:get_trace_context().
%% #{trace_id => <<"abc123">>}
%% ```
-spec get_trace_context() -> map() | undefined.
get_trace_context() ->
    case get(yawl_trace_context) of
        undefined -> undefined;
        Context -> Context
    end.

%% @doc Set the trace context in the process dictionary.
%%
%% Stores a trace context map for distributed tracing propagation.
%% The map should contain `trace_id` and optionally `span_id`.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:set_trace_context(#{trace_id => <<"mytrace">>, span_id => <<"myspan">>}).
%% ok
%% ```
-spec set_trace_context(map()) -> ok.
set_trace_context(Context) when is_map(Context) ->
    put(yawl_trace_context, Context),
    ok.

%% @doc Inject trace context into HTTP headers map.
%%
%% Adds W3C traceparent format headers and YAWL-specific trace headers
%% to the provided headers map. If no trace context exists, returns
%% the headers unchanged.
%%
%% Example:
%% ```
%% 1> Ctx = #{trace_id => <<"0123456789abcdef0123456789abcdef">>, span_id => <<"0123456789abcdef">>}.
%% 2> yawl_telemetry:set_trace_context(Ctx).
%% ok
%% 3> yawl_telemetry:inject_trace_context(#{<<"custom">> => <<"value">>}).
%% #{<<"custom">> => <<"value">>,
%%   <<"traceparent">> => <<"00-0123456789abcdef0123456789abcdef-0123456789abcdef-01">>,
%%   <<"x-yawl-span-id">> => <<"0123456789abcdef">>,
%%   <<"x-yawl-trace-id">> => <<"0123456789abcdef0123456789abcdef">>}
%% ```
-spec inject_trace_context(map()) -> map().
inject_trace_context(Headers) ->
    case get_trace_context() of
        undefined ->
            Headers;
        Context ->
            TraceId = maps:get(trace_id, Context, generate_trace_id()),
            SpanId = maps:get(span_id, Context, generate_span_id()),
            TraceParent = <<16#00, TraceId/binary, "-", SpanId/binary, "-01">>,
            maps:put(<<"traceparent">>, TraceParent,
                maps:put(<<"x-yawl-trace-id">>, TraceId,
                    maps:put(<<"x-yawl-span-id">>, SpanId, Headers)))
    end.

%% @doc Extract trace context from HTTP headers map.
%%
%% Parses W3C traceparent format or YAWL-specific headers to extract
%% trace and span IDs. Returns a map with `trace_id` and `span_id`.
%%
%% Example:
%% ```
%% 1> Headers = #{<<"traceparent">> => <<"00-0123456789abcdef0123456789abcdef-0123456789abcdef-01">>}.
%% 2> yawl_telemetry:extract_trace_context(Headers).
%% #{span_id => <<"0123456789abcdef">>,trace_id => <<"0123456789abcdef0123456789abcdef">>}
%% 3> yawl_telemetry:extract_trace_context(#{}).
%% #{}
%% ```
-spec extract_trace_context(map()) -> map().
extract_trace_context(Headers) ->
    TraceParent = maps:get(<<"traceparent">>, Headers,
                  maps:get(<<"x-yawl-trace-id">>, Headers, undefined)),
    case TraceParent of
        undefined ->
            maps:new();
        Bin when is_binary(Bin) ->
            %% Try to parse W3C traceparent format: 00-traceId-spanId-flags
            Parts = binary:split(Bin, <<"-">>, [global]),
            case Parts of
                [<<0>>, TraceId, SpanId, _Flags] ->
                    maps:put(trace_id, TraceId, maps:put(span_id, SpanId, maps:new()));
                _ ->
                    maps:put(trace_id, Bin, maps:new())
            end
    end.

%% @doc Generate a unique 128-bit trace ID as hexadecimal string.
%%
%% Creates a cryptographically random 32-character hexadecimal string
%% suitable for distributed tracing following W3C traceparent format.
%%
%% Example:
%% ```
%% 1> TraceId = yawl_telemetry:generate_trace_id().
%% <<16,256,...>>
%% 2> byte_size(TraceId).
%% 32
%% 3> is_binary(TraceId).
%% true
%% ```
-spec generate_trace_id() -> trace_id().
generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    binary:encode_hex(Id).

%% @doc Generate a unique 64-bit span ID as hexadecimal string.
%%
%% Creates a cryptographically random 16-character hexadecimal string
%% for identifying individual spans within a trace.
%%
%% Example:
%% ```
%% 1> SpanId = yawl_telemetry:generate_span_id().
%% <<16,256,...>>
%% 2> byte_size(SpanId).
%% 16
%% 3> is_binary(SpanId).
%% true
%% ```
-spec generate_span_id() -> binary().
generate_span_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    binary:encode_hex(Id).

%%====================================================================
%% Health Checks
%%====================================================================

%% @doc Check the health status of a specific pattern.
%%
%% Returns {Status, Info} where Status is healthy, degraded, or unhealthy.
%% Info map contains details like active_spans, healthy_count, degraded_count.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> {Status, Info} = yawl_telemetry:check_pattern_health(workflow_1).
%% {healthy, #{active_spans => 1, healthy_count => 1}}
%% 2> Status.
%% healthy
%% ```
-spec check_pattern_health(pattern_id()) -> {health_status(), map()} | {{not_found, map()}, term()}.
check_pattern_health(PatternId) ->
    gen_server:call(?MODULE, {check_pattern_health, PatternId}).

system_health() ->
    gen_server:call(?MODULE, system_health).

%% @doc Get status of all YAWL components.
%%
%% Returns a map with component names as keys and their status
%% (running or not_running) as values.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> Status = yawl_telemetry:component_status().
%% #{telemetry => running, monitor => running, engine => running, ...}
%% 2> maps:get(telemetry, Status).
%% running
%% ```
-spec component_status() -> map().
component_status() ->
    gen_server:call(?MODULE, component_status).

%% @doc Register a custom health check function.
%%
%% Registers a zero-arity function that will be called during system health checks.
%% Returns {ok, CheckRef} where CheckRef can be used to unregister the check.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> CheckFn = fun() -> case file:read_file_info("/tmp") of {ok, _} -> ok; _ -> error end end.
%% 2> {ok, CheckRef} = yawl_telemetry:register_health_check(tmp_writable, CheckFn).
%% {ok, #Ref<0.0.0.789>}
%% ```
-spec register_health_check(term(), function()) -> {ok, reference()}.
register_health_check(Name, CheckFn) when is_function(CheckFn, 0) ->
    gen_server:call(?MODULE, {register_health_check, Name, CheckFn}).

%% @doc Unregister a previously registered health check.
%%
%% Removes the health check function associated with the given reference.
%%
%% Example (requires telemetry server running):
%% ```
%% 1> {ok, CheckRef} = yawl_telemetry:register_health_check(my_check, fun() -> ok end).
%% {ok, #Ref<0.0.0.789>}
%% 2> yawl_telemetry:unregister_health_check(CheckRef).
%% ok
%% ```
-spec unregister_health_check(reference()) -> ok.
unregister_health_check(CheckRef) ->
    gen_server:cast(?MODULE, {unregister_health_check, CheckRef}).

%%====================================================================
%% Execution Visualization
%%====================================================================

execution_graph(PatternId) ->
    gen_server:call(?MODULE, {execution_graph, PatternId}).

export_dot(PatternId) ->
    gen_server:call(?MODULE, {export_dot, PatternId}).

execution_timeline(PatternId) ->
    gen_server:call(?MODULE, {execution_timeline, PatternId}).

get_execution_tree(PatternId) ->
    gen_server:call(?MODULE, {get_execution_tree, PatternId}).

visualize_pattern(PatternType, PatternId) ->
    gen_server:call(?MODULE, {visualize_pattern, PatternType, PatternId}).

%%====================================================================
%% Alerting
%%====================================================================

add_alert_rule(Condition, Action) when is_function(Condition, 0),
                                        is_function(Action, 0) ->
    gen_server:call(?MODULE, {add_alert_rule, Condition, Action}).

remove_alert_rule(AlertId) ->
    gen_server:cast(?MODULE, {remove_alert_rule, AlertId}).

check_alerts() ->
    gen_server:call(?MODULE, check_alerts).

list_alert_rules() ->
    gen_server:call(?MODULE, list_alert_rules).

trigger_alert(AlertId) ->
    gen_server:cast(?MODULE, {trigger_alert, AlertId}).

%%====================================================================
%% Audit Logging
%%====================================================================

log_event(EventType, PatternId, Details) ->
    gen_server:cast(?MODULE, {log_event, EventType, PatternId, Details}).

log_state_change(PatternId, OldState, NewState) ->
    Details = maps:new(),
    Details2 = maps:put(old_state, OldState, Details),
    Details3 = maps:put(new_state, NewState, Details2),
    gen_server:cast(?MODULE, {log_event, state_change, PatternId, Details3}).

query_audit(Filter) ->
    gen_server:call(?MODULE, {query_audit, Filter}).

get_audit_log(PatternId) ->
    Filter = maps:put(pattern_id, PatternId, maps:new()),
    query_audit(Filter).

export_audit_log(Filename, Filter) ->
    gen_server:call(?MODULE, {export_audit_log, Filename, Filter}).

clear_audit_log() ->
    gen_server:cast(?MODULE, clear_audit_log).

clear_audit_log(PatternId) ->
    gen_server:cast(?MODULE, {clear_audit_log, PatternId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    MetricsRetention = maps:get(metrics_retention_ms, Config, ?DEFAULT_METRICS_RETENTION),
    AuditRetention = maps:get(audit_retention_ms, Config, ?DEFAULT_AUDIT_RETENTION),
    EnableOTel = maps:get(enable_opentelemetry, Config, false),

    %% Schedule periodic cleanup
    schedule_cleanup(),

    %% Initialize trace context if not present
    case get_trace_context() of
        undefined ->
            TraceId = generate_trace_id(),
            SpanId = generate_span_id(),
            CTX = maps:put(trace_id, TraceId, maps:new()),
            CTX2 = maps:put(span_id, SpanId, CTX),
            set_trace_context(CTX2);
        _ ->
            ok
    end,

    ConfigMap = maps:put(metrics_retention_ms, MetricsRetention,
        maps:put(audit_retention_ms, AuditRetention,
            maps:put(enable_opentelemetry, EnableOTel, maps:new()))),

    State = #telemetry_state{
        active_spans = maps:new(),
        completed_spans = [],
        metrics = [],
        alert_rules = [],
        audit_log = [],
        health_checks = [],
        trace_context = undefined,
        config = ConfigMap,
        start_time = erlang:system_time(millisecond)
    },

    logger:info("YAWL Telemetry started", [
        {module, ?MODULE},
        {enable_opentelemetry, EnableOTel},
        {metrics_retention, MetricsRetention}
    ]),

    {ok, State}.

handle_call({start_span, PatternType, PatternId, Attributes}, _From, State) ->
    TraceId = case get_trace_context() of
        undefined -> generate_trace_id();
        CTX -> maps:get(trace_id, CTX, generate_trace_id())
    end,
    SpanId = make_ref(),
    Now = erlang:system_time(millisecond),

    Span = #span{
        id = SpanId,
        trace_id = TraceId,
        parent_id = undefined,
        pattern_type = PatternType,
        pattern_id = PatternId,
        start_time = Now,
        end_time = undefined,
        status = undefined,
        attributes = Attributes,
        events = []
    },

    ActiveSpans = maps:put(SpanId, Span, State#telemetry_state.active_spans),

    {reply, {ok, SpanId}, State#telemetry_state{active_spans = ActiveSpans}};

handle_call(get_active_spans, _From, State) ->
    Spans = maps:fold(
        fun(_SpanId, Span, Acc) ->
            [{Span#span.id, Span#span.pattern_type, Span#span.pattern_id} | Acc]
        end,
        [],
        State#telemetry_state.active_spans
    ),
    {reply, lists:reverse(Spans), State};

handle_call({get_span_info, SpanId}, _From, State) ->
    case maps:get(SpanId, State#telemetry_state.active_spans, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Span ->
            Now = erlang:system_time(millisecond),
            Duration = case Span#span.end_time of
                undefined -> Now - Span#span.start_time;
                EndTime -> EndTime - Span#span.start_time
            end,
            InfoMap = maps:new(),
            Info2 = maps:put(id, Span#span.id, InfoMap),
            Info3 = maps:put(trace_id, Span#span.trace_id, Info2),
            Info4 = maps:put(pattern_type, Span#span.pattern_type, Info3),
            Info5 = maps:put(pattern_id, Span#span.pattern_id, Info4),
            Info6 = maps:put(start_time, Span#span.start_time, Info5),
            Info7 = maps:put(duration, Duration, Info6),
            Info8 = maps:put(status, Span#span.status, Info7),
            Info9 = maps:put(attributes, Span#span.attributes, Info8),
            Info10 = maps:put(events, Span#span.events, Info9),
            {reply, {ok, Info10}, State}
    end;

handle_call(get_config, _From, State) ->
    {reply, State#telemetry_state.config, State};

handle_call({get_metrics, PatternType}, _From, State) ->
    Metrics = lists:filter(
        fun(M) ->
            case maps:get(pattern_type, M#metric.tags, undefined) of
                PatternType -> true;
                _ -> false
            end
        end,
        State#telemetry_state.metrics
    ),
    MetricMaps = lists:map(fun metric_to_map/1, Metrics),
    {reply, MetricMaps, State};

handle_call(get_metrics_summary, _From, State) ->
    Summary = calculate_metrics_summary(State#telemetry_state.metrics),
    {reply, Summary, State};

handle_call({export_prometheus, PatternType}, _From, State) ->
    Metrics = case PatternType of
        all -> State#telemetry_state.metrics;
        Type -> lists:filter(
            fun(M) ->
                case maps:get(pattern_type, M#metric.tags, undefined) of
                    Type -> true;
                    _ -> false
                end
            end,
            State#telemetry_state.metrics
        )
    end,
    Result = format_prometheus_metrics(Metrics),
    {reply, Result, State};

handle_call({check_pattern_health, PatternId}, _From, State) ->
    %% Find active spans for this pattern
    PatternSpans = lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        maps:values(State#telemetry_state.active_spans)
    ),

    Status = case PatternSpans of
        [] ->
            %% No active spans - check completed spans
            Completed = lists:filter(
                fun(S) -> S#span.pattern_id =:= PatternId end,
                State#telemetry_state.completed_spans
            ),
            case Completed of
                [] -> {not_found, maps:put(reason, no_such_pattern, maps:new())};
                _ ->
                    Info = maps:put(completed_spans, length(Completed), maps:new()),
                    {healthy, Info}
            end;
        _ ->
            %% Check span status and duration
            Now = erlang:system_time(millisecond),
            {Healthy, Degraded, Unhealthy} = lists:foldl(
                fun(S, {H, D, U}) ->
                    Duration = Now - S#span.start_time,
                    case {S#span.status, Duration} of
                        {undefined, Dur} when Dur > 300000 ->
                            {H, D + 1, U};
                        {undefined, _} ->
                            {H + 1, D, U};
                        {ok, _} ->
                            {H + 1, D, U};
                        {error, _} ->
                            {H, D, U + 1};
                        _ ->
                            {H, D + 1, U}
                    end
                end,
                {0, 0, 0},
                PatternSpans
            ),
            case Unhealthy of
                N when N > 0 ->
                    Info = maps:put(active_spans, length(PatternSpans),
                        maps:put(unhealthy_count, Unhealthy,
                            maps:put(degraded_count, Degraded,
                                maps:put(healthy_count, Healthy, maps:new())))),
                    {unhealthy, Info};
                _ when Degraded > 0 ->
                    Info = maps:put(active_spans, length(PatternSpans),
                        maps:put(degraded_count, Degraded,
                            maps:put(healthy_count, Healthy, maps:new()))),
                    {degraded, Info};
                _ ->
                    Info = maps:put(active_spans, length(PatternSpans),
                        maps:put(healthy_count, Healthy, maps:new())),
                    {healthy, Info}
            end
    end,
    {reply, Status, State};

handle_call(system_health, _From, State) ->
    Now = erlang:system_time(millisecond),
    Uptime = Now - State#telemetry_state.start_time,

    %% Calculate system health metrics
    TotalMetrics = length(State#telemetry_state.metrics),
    ActiveSpans = maps:size(State#telemetry_state.active_spans),
    CompletedSpans = length(State#telemetry_state.completed_spans),
    AuditEntries = length(State#telemetry_state.audit_log),
    AlertRules = length(State#telemetry_state.alert_rules),

    %% Run custom health checks
    HealthCheckResults = run_health_checks(State#telemetry_state.health_checks),

    UptimeFormatted = format_uptime(Uptime),

    SystemInfo = maps:put(uptime_ms, Uptime,
        maps:put(uptime_formatted, UptimeFormatted,
            maps:put(active_spans, ActiveSpans,
                maps:put(completed_spans, CompletedSpans,
                    maps:put(total_metrics, TotalMetrics,
                        maps:put(audit_entries, AuditEntries,
                            maps:put(alert_rules, AlertRules,
                                maps:put(memory, erlang:memory(),
                                    maps:put(process_count, erlang:system_info(process_count),
                                        maps:put(custom_checks, HealthCheckResults, maps:new())))))))))),

    {reply, {ok, SystemInfo}, State};

handle_call(component_status, _From, State) ->
    %% Check status of various components
    Components = maps:put(telemetry, running,
        maps:put(monitor, check_component_running(yawl_monitor),
            maps:put(logging, check_component_running(yawl_logging),
                maps:put(engine, check_component_running(yawl_engine),
                    maps:put(stateless, check_component_running(yawl_stateless), maps:new()))))),
    {reply, Components, State};

handle_call({register_health_check, Name, CheckFn}, _From, State) ->
    CheckId = make_ref(),
    HealthCheck = #health_check{
        id = CheckId,
        name = Name,
        check_fn = CheckFn,
        last_result = undefined,
        last_check = undefined
    },
    {reply, {ok, CheckId}, State#telemetry_state{
        health_checks = [HealthCheck | State#telemetry_state.health_checks]
    }};

handle_call({execution_graph, PatternId}, _From, State) ->
    %% Build execution graph from completed spans
    PatternSpans = lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        State#telemetry_state.completed_spans
    ) ++ lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        maps:values(State#telemetry_state.active_spans)
    ),

    case PatternSpans of
        [] ->
            {reply, {error, no_spans_found}, State};
        _ ->
            Graph = digraph:new(),
            try
                %% Create nodes and edges
                lists:foreach(
                    fun(Span) ->
                        NodeId = {span, Span#span.id},
                        VertexInfo = maps:put(type, Span#span.pattern_type,
                            maps:put(start_time, Span#span.start_time,
                                maps:put(end_time, Span#span.end_time,
                                    maps:put(status, Span#span.status, maps:new())))),
                        digraph:add_vertex(Graph, NodeId, VertexInfo),
                        case Span#span.parent_id of
                            undefined ->
                                ok;
                            ParentId ->
                                ParentNode = {span, ParentId},
                                digraph:add_vertex(Graph, ParentNode, maps:new()),
                                digraph:add_edge(Graph, ParentNode, NodeId)
                        end
                    end,
                    PatternSpans
                ),
                {reply, {ok, Graph}, State}
            catch
                _:Error ->
                    {reply, {error, Error}, State}
            end
    end;

handle_call({export_dot, PatternId}, _From, State) ->
    PatternSpans = lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        State#telemetry_state.completed_spans
    ) ++ lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        maps:values(State#telemetry_state.active_spans)
    ),

    DotData = format_spans_as_dot(PatternId, PatternSpans),
    {reply, {ok, DotData}, State};

handle_call({execution_timeline, PatternId}, _From, State) ->
    PatternSpans = lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        State#telemetry_state.completed_spans
    ) ++ lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        maps:values(State#telemetry_state.active_spans)
    ),

    Timeline = lists:flatmap(
        fun(Span) ->
            Events = [{event, Span#span.pattern_type, start, Span#span.start_time}],
            EndEvents = case Span#span.end_time of
                undefined -> [];
                EndTimeVal -> [{event, Span#span.pattern_type, finish, EndTimeVal}]
            end,
            EventEvents = [{event, Span#span.pattern_type, E, Span#span.start_time}
                          || E <- Span#span.events],
            Events ++ EndEvents ++ EventEvents
        end,
        PatternSpans
    ),

    SortedTimeline = lists:sort(
        fun({event, _, _, T1}, {event, _, _, T2}) -> T1 =< T2 end,
        Timeline
    ),

    {reply, {ok, SortedTimeline}, State};

handle_call({get_execution_tree, PatternId}, _From, State) ->
    PatternSpans = lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        State#telemetry_state.completed_spans
    ) ++ lists:filter(
        fun(S) -> S#span.pattern_id =:= PatternId end,
        maps:values(State#telemetry_state.active_spans)
    ),

    Tree = build_execution_tree(PatternSpans),
    {reply, {ok, Tree}, State};

handle_call({visualize_pattern, PatternType, PatternId}, _From, State) ->
    %% Generate DOT visualization for pattern structure
    DotData = visualize_pattern_structure(PatternType, PatternId),
    {reply, {ok, DotData}, State};

handle_call({add_alert_rule, Condition, Action}, _From, State) ->
    RuleId = make_ref(),
    Hash = binary:encode_hex(crypto:hash(md5, term_to_binary(RuleId))),
    Name = <<"alert_", Hash/binary>>,
    Rule = #alert_rule{
        id = RuleId,
        name = Name,
        condition = Condition,
        action = Action,
        enabled = true,
        last_triggered = undefined,
        trigger_count = 0
    },
    {reply, {ok, RuleId}, State#telemetry_state{
        alert_rules = [Rule | State#telemetry_state.alert_rules]
    }};

handle_call(check_alerts, _From, State) ->
    {Results, NewState} = check_and_execute_alerts(State),
    {reply, Results, NewState};

handle_call(list_alert_rules, _From, State) ->
    Rules = lists:map(fun alert_rule_to_map/1, State#telemetry_state.alert_rules),
    {reply, Rules, State};

handle_call({query_audit, Filter}, _From, State) ->
    Filtered = filter_audit_log(State#telemetry_state.audit_log, Filter),
    Result = lists:map(fun audit_entry_to_map/1, Filtered),
    {reply, {ok, Result}, State};

handle_call({export_audit_log, Filename, Filter}, _From, State) ->
    Filtered = filter_audit_log(State#telemetry_state.audit_log, Filter),
    Result = try
        Data = [io_lib:format("~p~n", [audit_entry_to_map(E)]) || E <- Filtered],
        file:write_file(Filename, Data),
        {ok, length(Filtered)}
    catch
        _:Error -> {error, Error}
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast({end_span, SpanId, Result, Status}, State) ->
    case maps:get(SpanId, State#telemetry_state.active_spans, undefined) of
        undefined ->
            {noreply, State};
        Span ->
            Now = erlang:system_time(millisecond),
            Duration = Now - Span#span.start_time,
            UpdatedSpan = Span#span{
                end_time = Now,
                status = Status,
                events = [{completed, Result} | Span#span.events]
            },
            ActiveSpans = maps:remove(SpanId, State#telemetry_state.active_spans),
            CompletedSpans = [UpdatedSpan | State#telemetry_state.completed_spans],

            %% Record completion metric
            Tags = maps:put(pattern_type, Span#span.pattern_type,
                maps:put(pattern_id, Span#span.pattern_id,
                    maps:put(status, Status, maps:new()))),
            Metric = #metric{
                name = pattern_execution_complete,
                value = Duration,
                timestamp = Now,
                tags = Tags
            },

            logger:debug("Span ended: ~p ~p duration=~pms status=~p",
                [Span#span.pattern_type, Span#span.pattern_id, Duration, Status]),

            {noreply, State#telemetry_state{
                active_spans = ActiveSpans,
                completed_spans = CompletedSpans,
                metrics = [Metric | State#telemetry_state.metrics]
            }}
    end;

handle_cast({span_attribute, SpanId, Key, Value}, State) ->
    case maps:get(SpanId, State#telemetry_state.active_spans, undefined) of
        undefined ->
            {noreply, State};
        Span ->
            UpdatedSpan = Span#span{
                attributes = maps:put(Key, Value, Span#span.attributes)
            },
            ActiveSpans = maps:put(SpanId, UpdatedSpan, State#telemetry_state.active_spans),
            {noreply, State#telemetry_state{active_spans = ActiveSpans}}
    end;

handle_cast({span_event, SpanId, EventName}, State) ->
    case maps:get(SpanId, State#telemetry_state.active_spans, undefined) of
        undefined ->
            {noreply, State};
        Span ->
            UpdatedSpan = Span#span{
                events = [EventName | Span#span.events]
            },
            ActiveSpans = maps:put(SpanId, UpdatedSpan, State#telemetry_state.active_spans),
            {noreply, State#telemetry_state{active_spans = ActiveSpans}}
    end;

handle_cast({span_status, SpanId, Status}, State) ->
    case maps:get(SpanId, State#telemetry_state.active_spans, undefined) of
        undefined ->
            {noreply, State};
        Span ->
            UpdatedSpan = Span#span{status = Status},
            ActiveSpans = maps:put(SpanId, UpdatedSpan, State#telemetry_state.active_spans),
            {noreply, State#telemetry_state{active_spans = ActiveSpans}}
    end;

handle_cast({record_execution_start, PatternType, PatternId}, State) ->
    Now = erlang:system_time(millisecond),
    Tags = maps:put(pattern_type, PatternType,
        maps:put(pattern_id, PatternId, maps:new())),
    Metric = #metric{
        name = pattern_execution_start,
        value = 1,
        timestamp = Now,
        tags = Tags
    },
    {noreply, State#telemetry_state{
        metrics = [Metric | State#telemetry_state.metrics]
    }};

handle_cast({record_execution_complete, PatternType, PatternId, Duration}, State) ->
    Now = erlang:system_time(millisecond),
    Tags = maps:put(pattern_type, PatternType,
        maps:put(pattern_id, PatternId, maps:new())),
    Metric = #metric{
        name = pattern_execution_complete,
        value = Duration,
        timestamp = Now,
        tags = Tags
    },
    {noreply, State#telemetry_state{
        metrics = [Metric | State#telemetry_state.metrics]
    }};

handle_cast({record_execution_error, PatternType, PatternId, Reason}, State) ->
    Now = erlang:system_time(millisecond),
    Tags = maps:put(pattern_type, PatternType,
        maps:put(pattern_id, PatternId,
            maps:put(reason, Reason, maps:new()))),
    Metric = #metric{
        name = pattern_execution_error,
        value = 1,
        timestamp = Now,
        tags = Tags
    },
    {noreply, State#telemetry_state{
        metrics = [Metric | State#telemetry_state.metrics]
    }};

handle_cast({record_timing, PatternType, Stage, Duration}, State) ->
    Now = erlang:system_time(millisecond),
    Tags = maps:put(pattern_type, PatternType,
        maps:put(stage, Stage, maps:new())),
    Metric = #metric{
        name = pattern_timing,
        value = Duration,
        timestamp = Now,
        tags = Tags
    },
    {noreply, State#telemetry_state{
        metrics = [Metric | State#telemetry_state.metrics]
    }};

handle_cast({increment_counter, MetricName, Tags}, State) ->
    Now = erlang:system_time(millisecond),
    Metric = #metric{
        name = MetricName,
        value = 1,
        timestamp = Now,
        tags = Tags
    },
    {noreply, State#telemetry_state{
        metrics = [Metric | State#telemetry_state.metrics]
    }};

handle_cast({update_config, Config}, State) ->
    CurrentConfig = State#telemetry_state.config,
    MergedConfig = maps:merge(CurrentConfig, Config),
    {noreply, State#telemetry_state{config = MergedConfig}};

handle_cast({unregister_health_check, CheckRef}, State) ->
    HealthChecks = lists:filter(
        fun(C) -> C#health_check.id =/= CheckRef end,
        State#telemetry_state.health_checks
    ),
    {noreply, State#telemetry_state{health_checks = HealthChecks}};

handle_cast({remove_alert_rule, AlertId}, State) ->
    AlertRules = lists:filter(
        fun(R) -> R#alert_rule.id =/= AlertId end,
        State#telemetry_state.alert_rules
    ),
    {noreply, State#telemetry_state{alert_rules = AlertRules}};

handle_cast({trigger_alert, AlertId}, State) ->
    NewState = execute_alert_action(AlertId, State),
    {noreply, NewState};

handle_cast({log_event, EventType, PatternId, Details}, State) ->
    Entry = #audit_entry{
        id = make_ref(),
        timestamp = erlang:system_time(millisecond),
        event_type = EventType,
        pattern_id = PatternId,
        details = Details
    },
    {noreply, State#telemetry_state{
        audit_log = [Entry | State#telemetry_state.audit_log]
    }};

handle_cast(clear_audit_log, State) ->
    {noreply, State#telemetry_state{audit_log = []}};

handle_cast({clear_audit_log, PatternId}, State) ->
    AuditLog = lists:filter(
        fun(E) -> E#audit_entry.pattern_id =/= PatternId end,
        State#telemetry_state.audit_log
    ),
    {noreply, State#telemetry_state{audit_log = AuditLog}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Perform periodic cleanup
    Config = State#telemetry_state.config,
    RetentionMs = maps:get(metrics_retention_ms, Config, ?DEFAULT_METRICS_RETENTION),
    CutoffTime = erlang:system_time(millisecond) - RetentionMs,

    %% Clean up old metrics
    Metrics = lists:filter(
        fun(M) -> M#metric.timestamp >= CutoffTime end,
        State#telemetry_state.metrics
    ),

    %% Clean up old audit entries
    AuditRetention = maps:get(audit_retention_ms, Config, ?DEFAULT_AUDIT_RETENTION),
    AuditCutoff = erlang:system_time(millisecond) - AuditRetention,
    AuditLog = lists:filter(
        fun(E) -> E#audit_entry.timestamp >= AuditCutoff end,
        State#telemetry_state.audit_log
    ),

    %% Clean up old completed spans (keep last 1000)
    SortedSpans = lists:sort(
        fun(S1, S2) ->
            End1 = case S1#span.end_time of
                undefined -> S1#span.start_time;
                E1 -> E1
            end,
            End2 = case S2#span.end_time of
                undefined -> S2#span.start_time;
                E2 -> E2
            end,
            End1 >= End2
        end,
        State#telemetry_state.completed_spans
    ),
    CompletedSpans = lists:sublist(SortedSpans, 1000),

    schedule_cleanup(),

    logger:debug("Telemetry cleanup: metrics=~p audit=~p spans=~p",
        [length(Metrics), length(AuditLog), length(CompletedSpans)]),

    {noreply, State#telemetry_state{
        metrics = Metrics,
        audit_log = AuditLog,
        completed_spans = CompletedSpans
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

schedule_cleanup() ->
    erlang:send_after(300000, self(), cleanup),  %% 5 minutes
    ok.

calculate_metrics_summary(Metrics) ->
    Now = erlang:system_time(millisecond),
    HourAgo = Now - 3600000,

    RecentMetrics = lists:filter(
        fun(M) -> M#metric.timestamp >= HourAgo end,
        Metrics
    ),

    TotalExecutions = lists:foldl(
        fun(M, Acc) ->
            case M#metric.name of
                pattern_execution_start -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        RecentMetrics
    ),

    CompleteMetrics = lists:filter(
        fun(M) -> M#metric.name =:= pattern_execution_complete end,
        RecentMetrics
    ),
    AvgDuration = case CompleteMetrics of
        [] -> 0;
        _ -> lists:sum([M#metric.value || M <- CompleteMetrics]) / length(CompleteMetrics)
    end,

    ErrorCount = lists:foldl(
        fun(M, Acc) ->
            case M#metric.name of
                pattern_execution_error -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        RecentMetrics
    ),

    maps:put(total_executions, TotalExecutions,
        maps:put(completions, length(CompleteMetrics),
            maps:put(errors, ErrorCount,
                maps:put(avg_duration_ms, round(AvgDuration * 100) / 100,
                    maps:put(metrics_count, length(Metrics),
                        maps:put(recent_metrics_count, length(RecentMetrics), maps:new())))))).

metric_to_map(Metric) ->
    maps:put(name, Metric#metric.name,
        maps:put(value, Metric#metric.value,
            maps:put(timestamp, Metric#metric.timestamp,
                maps:put(tags, Metric#metric.tags, maps:new())))).

alert_rule_to_map(Rule) ->
    maps:put(id, Rule#alert_rule.id,
        maps:put(name, Rule#alert_rule.name,
            maps:put(enabled, Rule#alert_rule.enabled,
                maps:put(last_triggered, Rule#alert_rule.last_triggered,
                    maps:put(trigger_count, Rule#alert_rule.trigger_count, maps:new()))))).

audit_entry_to_map(Entry) ->
    maps:put(id, Entry#audit_entry.id,
        maps:put(timestamp, Entry#audit_entry.timestamp,
            maps:put(event_type, Entry#audit_entry.event_type,
                maps:put(pattern_id, Entry#audit_entry.pattern_id,
                    maps:put(details, Entry#audit_entry.details, maps:new()))))).

format_prometheus_metrics(Metrics) ->
    Grouped = lists:foldl(
        fun(M, Acc) ->
            Name = atom_to_binary(M#metric.name),
            maps:update_with(Name, fun(Existing) -> [M | Existing] end, [M], Acc)
        end,
        maps:new(),
        Metrics
    ),

    Lines = maps:fold(
        fun(Name, MetricsList, Acc) ->
            HelpLine = <<"# HELP ", Name/binary, " YAWL pattern metric">>,
            TypeLine = <<"# TYPE ", Name/binary, " gauge">>,
            MetricLines = format_prometheus_metric_group(Name, MetricsList, []),
            [HelpLine, TypeLine | MetricLines] ++ Acc
        end,
        [],
        Grouped
    ),

    iolist_to_binary(lists:join(<<$\n>>, lists:reverse(Lines))).

format_prometheus_metric_group(_Name, [], Acc) ->
    lists:reverse(Acc);
format_prometheus_metric_group(Name, [Metric | Rest], Acc) ->
    %% Format tags as labels
    TagStr = format_prometheus_labels(Metric#metric.tags),
    Line = io_lib:format("~s ~s ~w ~w",
        [Name, TagStr, Metric#metric.value, Metric#metric.timestamp]),
    format_prometheus_metric_group(Name, Rest, [list_to_binary(Line) | Acc]).

format_prometheus_labels(Labels) ->
    case maps:size(Labels) of
        0 -> <<>>;
        _ ->
            Pairs = maps:to_list(Labels),
            Formatted = [format_prometheus_label_pair(K, V) || {K, V} <- Pairs],
            iolist_to_binary([${, lists:join($,, Formatted), $}])
    end.

format_prometheus_label_pair(Key, Value) ->
    KeyStr = format_label_value(Key),
    ValStr = format_label_value(Value),
    <<KeyStr/binary, "=\"", ValStr/binary, $">>.

format_label_value(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
format_label_value(V) when is_integer(V) ->
    integer_to_binary(V);
format_label_value(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 4}]);
format_label_value(V) when is_binary(V) ->
    V;
format_label_value(V) when is_list(V) ->
    list_to_binary(V);
format_label_value(V) ->
    list_to_binary(io_lib:format("~p", [V])).

run_health_checks(HealthChecks) ->
    lists:foldl(
        fun(Check, Acc) ->
            Fn = Check#health_check.check_fn,
            Result = try
                Fn()
            catch
                _:_ -> {error, health_check_failed}
            end,
            maps:put(Check#health_check.name, Result, Acc)
        end,
        maps:new(),
        HealthChecks
    ).

check_and_execute_alerts(State) ->
    {Results, UpdatedRules} = lists:mapfoldl(
        fun(Rule, AccRules) ->
            case Rule#alert_rule.enabled of
                false ->
                    {skipped, [Rule | AccRules]};
                true ->
                    CondFn = Rule#alert_rule.condition,
                    Triggered = try
                        CondFn()
                    catch
                        _:_ -> false
                    end,

                    case Triggered of
                        true ->
                            ActionFn = Rule#alert_rule.action,
                            ActionResults = try
                                ActionFn()
                            catch
                                _:Error -> {error, Error}
                            end,
                            UpdatedRule = Rule#alert_rule{
                                last_triggered = erlang:system_time(millisecond),
                                trigger_count = Rule#alert_rule.trigger_count + 1
                            },
                            {{triggered, ActionResults}, [UpdatedRule | AccRules]};
                        false ->
                            {not_triggered, [Rule | AccRules]}
                    end
            end
        end,
        [],
        State#telemetry_state.alert_rules
    ),
    {Results, State#telemetry_state{alert_rules = lists:reverse(UpdatedRules)}}.

execute_alert_action(AlertId, State) ->
    AlertRules = lists:map(
        fun(Rule) ->
            case Rule#alert_rule.id of
                AlertId ->
                    ActionFn = Rule#alert_rule.action,
                    try ActionFn() catch _:_ -> ok end,
                    Rule#alert_rule{
                        last_triggered = erlang:system_time(millisecond),
                        trigger_count = Rule#alert_rule.trigger_count + 1
                    };
                _ ->
                    Rule
            end
        end,
        State#telemetry_state.alert_rules
    ),
    State#telemetry_state{alert_rules = AlertRules}.

filter_audit_log(AuditLog, Filter) ->
    lists:filter(
        fun(Entry) ->
            PatternMatch = case maps:get(pattern_id, Filter, undefined) of
                undefined -> true;
                Id -> Entry#audit_entry.pattern_id =:= Id
            end,
            TypeMatch = case maps:get(event_type, Filter, undefined) of
                undefined -> true;
                Type -> Entry#audit_entry.event_type =:= Type
            end,
            TimeMatch = case {maps:get(from, Filter, undefined),
                            maps:get(to, Filter, undefined)} of
                {undefined, undefined} -> true;
                {From, undefined} -> Entry#audit_entry.timestamp >= From;
                {undefined, To} -> Entry#audit_entry.timestamp =< To;
                {From, To} -> Entry#audit_entry.timestamp >= From andalso
                             Entry#audit_entry.timestamp =< To
            end,
            PatternMatch andalso TypeMatch andalso TimeMatch
        end,
        AuditLog
    ).

check_component_running(ComponentName) ->
    case whereis(ComponentName) of
        undefined -> not_running;
        _Pid -> running
    end.

format_spans_as_dot(PatternId, Spans) ->
    Header = [io_lib:format("digraph \"~p\" {~n", [PatternId]),
              "  rankdir=LR;~n",
              "  node [shape=box];~n~n"],

    Nodes = lists:map(
        fun(Span) ->
            StatusLabel = case Span#span.status of
                undefined -> "running";
                S -> atom_to_list(S)
            end,
            Label = io_lib:format("~p ~n~s",
                [Span#span.pattern_type, StatusLabel]),
            Duration = case Span#span.end_time of
                undefined -> "active";
                T -> io_lib:format("~pms", [T - Span#span.start_time])
            end,
            io_lib:format("  \"~p\" [label=\"~s (~s)\"];~n",
                [Span#span.id, Label, Duration])
        end,
        Spans
    ),

    Edges = lists:flatmap(
        fun(Span) ->
            case Span#span.parent_id of
                undefined -> [];
                ParentId ->
                    [io_lib:format("  \"~p\" -> \"~p\";~n", [ParentId, Span#span.id])]
            end
        end,
        Spans
    ),

    Footer = ["}~n"],

    [Header, Nodes, Edges, Footer].

build_execution_tree(Spans) ->
    %% Group by pattern type
    ByType = lists:foldl(
        fun(Span, Acc) ->
            Type = Span#span.pattern_type,
            maps:update_with(Type,
                fun(Existing) -> [Span | Existing] end,
                [Span],
                Acc)
        end,
        maps:new(),
        Spans
    ),

    maps:map(
        fun(_Type, TypeSpans) ->
            lists:map(
                fun(Span) ->
                    Duration = case Span#span.end_time of
                        undefined -> undefined;
                        End -> End - Span#span.start_time
                    end,
                    maps:put(id, Span#span.id,
                        maps:put(pattern_id, Span#span.pattern_id,
                            maps:put(start_time, Span#span.start_time,
                                maps:put(end_time, Span#span.end_time,
                                    maps:put(duration, Duration,
                                        maps:put(status, Span#span.status,
                                            maps:put(attributes, Span#span.attributes, maps:new())))))))
                end,
                TypeSpans)
        end,
        ByType
    ).

visualize_pattern_structure(PatternType, PatternId) ->
    %% Create pattern-specific DOT visualization
    PatternName = atom_to_list(PatternType),

    %% Define pattern structure based on type
    {Nodes, Edges} = case PatternType of
        implicit_termination ->
            {["p_start [label=\"Start\", shape=ellipse];",
              "p_active [label=\"Active\", shape=box];",
              "p_work [label=\"Work\", shape=box];",
              "p_terminate [label=\"Terminate\", shape=doublecircle];"],
             ["p_start -> p_active;",
              "p_active -> p_work;",
              "p_work -> p_active;",
              "p_active -> p_terminate [label=\"implicit\"];"]};
        multiple_instances_no_sync ->
            {["p_pool [label=\"Instance Pool\", shape=parallelogram];",
              "p_ready [label=\"Ready\", shape=box];",
              "p_done [label=\"Done\", shape=box];"],
             ["p_pool -> p_ready;",
              "p_ready -> p_done;"]};
        multiple_instances_static ->
            {["p_pool [label=\"Static Pool\", shape=parallelogram];",
              "p_ready [label=\"Ready\", shape=box];",
              "p_running [label=\"Running\", shape=box];",
              "p_complete [label=\"Complete\", shape=folder];"],
             ["p_pool -> p_ready;",
              "p_ready -> p_running;",
              "p_running -> p_complete;",
              "p_complete [label=\"Complete (All)\"];"]};
        multiple_instances_runtime ->
            {["p_eval [label=\"Eval Count\", shape=diamond];",
              "p_pool [label=\"Runtime Pool\", shape=parallelogram];",
              "p_running [label=\"Running\", shape=box];",
              "p_complete [label=\"Complete\", shape=folder];"],
             ["p_eval -> p_pool;",
              "p_pool -> p_running;",
              "p_running -> p_complete;"]};
        multiple_instances_dynamic ->
            {["p_source [label=\"Data Source\", shape=cylinder];",
              "p_running [label=\"Dynamic Running\", shape=box];",
              "p_done [label=\"Done\", shape=folder];"],
             ["p_source -> p_running;",
              "p_running -> p_done;",
              "p_source -> p_running [style=dashed, label=\"fetch\"];"]};
        deferred_choice ->
            {["p_start [label=\"Start\", shape=ellipse];",
              "p_option_a [label=\"Option A\", shape=hexagon];",
              "p_option_b [label=\"Option B\", shape=hexagon];",
              "p_selected [label=\"Selected\", shape=box];"],
             ["p_start -> p_option_a [style=dotted];",
              "p_start -> p_option_b [style=dotted];",
              "p_option_a -> p_selected;",
              "p_option_b -> p_selected;"]};
        interleaved_routing ->
            {["p_branch_pool [label=\"Branch Pool\", shape=parallelogram];",
              "p_next [label=\"Next Branch\", shape=diamond];",
              "p_executing [label=\"Executing\", shape=box];"],
             ["p_branch_pool -> p_next;",
              "p_next -> p_executing;",
              "p_executing -> p_branch_pool [label=\"return\"];"]};
        structured_loop ->
            {["p_init [label=\"Init\", shape=ellipse];",
              "p_body [label=\"Loop Body\", shape=box];",
              "p_condition [label=\"Condition\", shape=diamond];",
              "p_exit [label=\"Exit\", shape=doublecircle];"],
             ["p_init -> p_body;",
              "p_body -> p_condition;",
              "p_condition -> p_body [label=\"true\"];",
              "p_condition -> p_exit [label=\"false\"];"]};
        critical_section ->
            {["p_request [label=\"Request\", shape=hexagon];",
              "p_lock [label=\"Lock\", shape=parallelogram];",
              "p_active [label=\"Critical Section\", shape=box];"],
             ["p_request -> p_lock;",
              "p_lock -> p_active;",
              "p_active -> p_lock [label=\"release\"];"]};
        _ ->
            {["p_start [label=\"Start\", shape=ellipse];",
              "p_process [label=\"Process\", shape=box];",
              "p_end [label=\"End\", shape=doublecircle];"],
             ["p_start -> p_process;",
              "p_process -> p_end;"]}
    end,

    Header = [io_lib:format("digraph \"~s_~p\" {~n", [PatternName, PatternId]),
              "  rankdir=TB;~n",
              "  node [fontname=\"Arial\"];~n~n"],

    NodeList = [["  ", N, "~n"] || N <- Nodes],
    EdgeList = [["  ", E, "~n"] || E <- Edges],

    Footer = ["}~n"],

    [Header, NodeList, EdgeList, Footer].

format_uptime(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    Days = Seconds div 86400,
    Hours = (Seconds rem 86400) div 3600,
    Minutes = (Seconds rem 3600) div 60,
    Secs = Seconds rem 60,

    Parts = case Days of
        0 -> [];
        N -> [integer_to_list(N), "d"]
    end ++ case Hours of
        0 -> [];
        N -> [integer_to_list(N), "h"]
    end ++ case Minutes of
        0 -> [];
        N -> [integer_to_list(N), "m"]
    end ++ case Secs of
        0 -> [];
        N -> [integer_to_list(N), "s"]
    end,

    case Parts of
        [] -> <<"0s">>;
        _ -> iolist_to_binary(lists:join(" ", Parts))
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Convert a proplist list to a map.
%%
%% Converts a list of {Key, Value} tuples into a map.
%% Useful for converting option lists to map format.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:list_to_map([{a, 1}, {b, 2}, {c, 3}]).
%% #{a => 1,b => 2,c => 3}
%% 2> yawl_telemetry:list_to_map([]).
%% #{}
%% 3> yawl_telemetry:list_to_map([{key, <<"value">>}]).
%% #{key => <<"value">>}
%% ```
-spec list_to_map([{term(), term()}]) -> map().
list_to_map(List) ->
    lists:foldl(fun({K, V}, Acc) -> maps:put(K, V, Acc) end, maps:new(), List).

%%-------------------------------------------------------------------
%% @doc Run doctests for the yawl_telemetry module.
%%
%% Executes all doctest examples to verify documentation correctness.
%%
%% Example:
%% ```
%% 1> yawl_telemetry:doctest_test().
%% ok
%% ```
%%-------------------------------------------------------------------
-spec doctest_test() -> ok.
doctest_test() ->
    %% Test trace ID generation
    TraceId = generate_trace_id(),
    32 = byte_size(TraceId),
    true = is_binary(TraceId),

    %% Test span ID generation
    SpanId = generate_span_id(),
    16 = byte_size(SpanId),
    true = is_binary(SpanId),

    %% Test list_to_map conversion
    Map1 = list_to_map([{a, 1}, {b, 2}]),
    #{a := 1, b := 2} = Map1,

    Map2 = list_to_map([]),
    #{} = Map2,

    %% Test trace context round-trip
    Ctx = #{trace_id => TraceId, span_id => SpanId},
    ok = set_trace_context(Ctx),
    Ctx = get_trace_context(),

    %% Test trace context injection/extraction
    %% inject_trace_context/1 uses process dictionary context
    Headers = inject_trace_context(#{}),
    true = maps:is_key(<<"traceparent">>, Headers),
    true = maps:is_key(<<"x-yawl-trace-id">>, Headers),

    Extracted = extract_trace_context(Headers),
    TraceId = maps:get(trace_id, Extracted),

    %% Test with empty headers
    EmptyResult = extract_trace_context(#{}),
    #{} = EmptyResult,

    %% Test inject with no trace context set
    %% Clear trace context and verify headers unchanged
    erase(yawl_trace_context),
    HeadersNoCtx = inject_trace_context(#{<<"existing">> => <<"header">>}),
    #{<<"existing">> := <<"header">>} = HeadersNoCtx,

    ok.
