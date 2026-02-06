%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Chicago TDD Tests for YAWL Telemetry Module
%%
%% Test-First Development: RED -> GREEN -> REFACTOR
%%
%% @doc YAWL Telemetry Tests
%% @end

-module(yawl_telemetry_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = yawl_telemetry:start_telemetry(),
    Pid.

cleanup(_Pid) ->
    yawl_telemetry:stop_telemetry(),
    ok.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

start_link_default_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Starts telemetry with default config"),
          ?assert(is_pid(whereis(yawl_telemetry)))
         ]
     end}.

start_link_with_config_test_() ->
    {setup,
     fun() ->
         cleanup(whereis(yawl_telemetry)),
         {ok, Pid} = yawl_telemetry:start_telemetry(#{metrics_retention_ms => 1000}),
         Pid
     end,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Starts telemetry with custom config"),
          {ok, Config} = yawl_telemetry:get_telemetry_config(),
          ?assertEqual(1000, maps:get(metrics_retention_ms, Config))
         ]
     end}.

stop_telemetry_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) ->
         ok = yawl_telemetry:stop_telemetry(),
         ?assertEqual(undefined, whereis(yawl_telemetry))
     end}.

get_telemetry_config_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets telemetry config"),
          {ok, Config} = yawl_telemetry:get_telemetry_config(),
          ?assert(is_map(Config)),
          ?assert(maps:is_key(metrics_retention_ms, Config)),
          ?assert(maps:is_key(audit_retention_ms, Config))
         ]
     end}.

update_config_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Updates telemetry config"),
          ok = yawl_telemetry:update_config(#{metrics_retention_ms => 5000}),
          {ok, Config} = yawl_telemetry:get_telemetry_config(),
          ?assertEqual(5000, maps:get(metrics_retention_ms, Config))
         ]
     end}.

%%====================================================================
%% Span Management Tests
%%====================================================================

start_span_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Creates a new span"),
          {ok, SpanId} = yawl_telemetry:start_span(sequence, <<"pattern-1">>),
          ?assert(is_reference(SpanId)),

          ActiveSpans = yawl_telemetry:get_active_spans(),
          ?assertEqual(1, length(ActiveSpans))
         ]
     end}.

start_span_with_attributes_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Creates span with attributes"),
          Attrs = #{key => value},
          {ok, SpanId} = yawl_telemetry:start_span(parallel_split, <<"pattern-2">>, Attrs),
          {ok, SpanInfo} = yawl_telemetry:get_span_info(SpanId),
          ?assertEqual(value, maps:get(key, maps:get(attributes, SpanInfo)))
         ]
     end}.

end_span_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Ends a span"),
          {ok, SpanId} = yawl_telemetry:start_span(sequence, <<"pattern-3">>),
          ok = yawl_telemetry:end_span(SpanId, {ok, result}, ok),

          ActiveSpans = yawl_telemetry:get_active_spans(),
          ?assertEqual(0, length(ActiveSpans))
         ]
     end}.

span_attribute_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Adds attribute to span"),
          {ok, SpanId} = yawl_telemetry:start_span(sequence, <<"pattern-4">>),
          ok = yawl_telemetry:span_attribute(SpanId, custom_attr, 42),
          {ok, SpanInfo} = yawl_telemetry:get_span_info(SpanId),
          ?assertEqual(42, maps:get(custom_attr, maps:get(attributes, SpanInfo)))
         ]
     end}.

span_event_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Adds event to span"),
          {ok, SpanId} = yawl_telemetry:start_span(sequence, <<"pattern-5">>),
          ok = yawl_telemetry:span_event(SpanId, milestone_reached)
         ]
     end}.

span_status_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Sets span status"),
          {ok, SpanId} = yawl_telemetry:start_span(sequence, <<"pattern-6">>),
          ok = yawl_telemetry:span_status(SpanId, running),
          {ok, SpanInfo} = yawl_telemetry:get_span_info(SpanId),
          ?assertEqual(running, maps:get(status, SpanInfo))
         ]
     end}.

get_active_spans_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Lists all active spans"),
          {ok, _S1} = yawl_telemetry:start_span(sequence, <<"p1">>),
          {ok, _S2} = yawl_telemetry:start_span(parallel_split, <<"p2">>),
          {ok, _S3} = yawl_telemetry:start_span(exclusive_choice, <<"p3">>),

          ActiveSpans = yawl_telemetry:get_active_spans(),
          ?assertEqual(3, length(ActiveSpans))
         ]
     end}.

get_span_info_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets span info"),
          {ok, SpanId} = yawl_telemetry:start_span(sequence, <<"pattern-7">>),
          {ok, SpanInfo} = yawl_telemetry:get_span_info(SpanId),

          ?assert(maps:is_key(id, SpanInfo)),
          ?assert(maps:is_key(trace_id, SpanInfo)),
          ?assert(maps:is_key(pattern_type, SpanInfo)),
          ?assert(maps:is_key(start_time, SpanInfo)),
          ?assert(maps:is_key(duration, SpanInfo))
         ]
     end}.

get_span_info_not_found_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Returns error for non-existent span"),
          ?assertEqual({error, not_found}, yawl_telemetry:get_span_info(make_ref()))
         ]
     end}.

%%====================================================================
%% Metrics Collection Tests
%%====================================================================

record_execution_start_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Records execution start"),
          ok = yawl_telemetry:record_execution_start(sequence, <<"case-1">>),
          Summary = yawl_telemetry:get_metrics_summary(),
          ?assert(maps:get(total_executions, Summary) > 0)
         ]
     end}.

record_execution_complete_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Records execution complete"),
          ok = yawl_telemetry:record_execution_complete(sequence, <<"case-2">>, 500),
          Metrics = yawl_telemetry:get_metrics(sequence),
          ?assert(length(Metrics) > 0)
         ]
     end}.

record_execution_error_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Records execution error"),
          ok = yawl_telemetry:record_execution_error(sequence, <<"case-3">>, timeout),
          Summary = yawl_telemetry:get_metrics_summary(),
          ?assert(maps:get(errors, Summary) > 0)
         ]
     end}.

record_timing_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Records timing metric"),
          ok = yawl_telemetry:record_timing(sequence, validation, 100),
          ok = yawl_telemetry:record_timing(sequence, execution, 200),
          Metrics = yawl_telemetry:get_metrics(sequence),
          ?assert(length(Metrics) >= 2)
         ]
     end}.

increment_counter_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Increments counter"),
          Tags = #{component => test},
          ok = yawl_telemetry:increment_counter(custom_counter, Tags),
          ok = yawl_telemetry:increment_counter(custom_counter, Tags),
          ok = yawl_telemetry:increment_counter(custom_counter, Tags),

          Metrics = yawl_telemetry:get_metrics(custom_counter),
          ?assert(length(Metrics) >= 3)
         ]
     end}.

get_metrics_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets metrics for pattern type"),
          ok = yawl_telemetry:record_execution_start(sequence, <<"case-metrics">>),
          Metrics = yawl_telemetry:get_metrics(sequence),
          ?assert(is_list(Metrics)),
          ?assert(length(Metrics) > 0)
         ]
     end}.

get_metrics_summary_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets metrics summary"),
          ok = yawl_telemetry:record_execution_start(sequence, <<"case-summary">>),
          ok = yawl_telemetry:record_execution_complete(sequence, <<"case-summary">>, 100),

          Summary = yawl_telemetry:get_metrics_summary(),
          ?assert(maps:is_key(total_executions, Summary)),
          ?assert(maps:is_key(completions, Summary)),
          ?assert(maps:is_key(errors, Summary)),
          ?assert(maps:is_key(avg_duration_ms, Summary))
         ]
     end}.

export_prometheus_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Exports Prometheus format"),
          ok = yawl_telemetry:record_execution_start(sequence, <<"case-prom">>),
          Prometheus = yawl_telemetry:export_prometheus(),
          ?assert(is_binary(Prometheus)),
          ?assert(string:str(<<"# HELP">>, Prometheus)),
          ?assert(string:str(<<"# TYPE">>, Prometheus))
         ]
     end}.

export_prometheus_pattern_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Exports Prometheus for specific pattern"),
          ok = yawl_telemetry:record_execution_complete(parallel_split, <<"case-pp">>, 250),
          Prometheus = yawl_telemetry:export_prometheus(parallel_split),
          ?assert(is_binary(Prometheus)),
          ?assert(string:str(<<"pattern_execution_complete">>, Prometheus))
         ]
     end}.

%%====================================================================
%% Distributed Tracing Tests
%%====================================================================

get_trace_context_test_() ->
    [
     ?_test("Gets trace context from process dictionary"),
     yawl_telemetry:set_trace_context(#{trace_id => <<"trace-123">>}),
     ?assertEqual(#{trace_id => <<"trace-123">>}, yawl_telemetry:get_trace_context())
    ].

set_trace_context_test_() ->
    [
     ?_test("Sets trace context"),
     Context = #{trace_id => <<"trace-456">>, span_id => <<"span-789">>},
     ?assertEqual(ok, yawl_telemetry:set_trace_context(Context)),
     ?assertEqual(Context, yawl_telemetry:get_trace_context())
    ].

inject_trace_context_test_() ->
    [
     ?_test("Injects trace context into headers"),
     Context = #{trace_id => <<"trace-inject">>, span_id => <<"span-inject">>},
     ok = yawl_telemetry:set_trace_context(Context),

     Headers = #{existing => <<"header">>},
     Injected = yawl_telemetry:inject_trace_context(Headers),

     ?assert(maps:is_key(<<"traceparent">>, Injected)),
     ?assert(maps:is_key(<<"x-yawl-trace-id">>, Injected))
    ].

extract_trace_context_test_() ->
    [
     ?_test("Extracts trace context from headers"),
     Headers = #{<<"traceparent">> => <<"00-trace123-span456-01">>},
     Context = yawl_telemetry:extract_trace_context(Headers),
     ?assert(maps:is_key(trace_id, Context)),
     ?assert(maps:is_key(span_id, Context))
    ].

generate_trace_id_test_() ->
    [
     ?_test("Generates trace ID"),
     TraceId = yawl_telemetry:generate_trace_id(),
     ?assert(is_binary(TraceId)),
     ?assertEqual(32, byte_size(TraceId))
    ].

generate_span_id_test_() ->
    [
     ?_test("Generates span ID"),
     SpanId = yawl_telemetry:generate_span_id(),
     ?assert(is_binary(SpanId)),
     ?assertEqual(16, byte_size(SpanId))
    ].

%%====================================================================
%% Health Checks Tests
%%====================================================================

check_pattern_health_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Checks pattern health - non-existent"),
          Health = yawl_telemetry:check_pattern_health(<<"non-existent">>),
          ?assertMatch({not_found, _}, Health)
         ]
     end}.

check_pattern_health_with_span_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test(begin
                    {ok, _SpanId} = yawl_telemetry:start_span(sequence, <<"health-pattern">>),
                    Health = yawl_telemetry:check_pattern_health(<<"health-pattern">>),
                    ?assert(case Health of
                        {healthy, _} -> true;
                        {degraded, _} -> true;
                        _ -> false
                    end)
               end)
         ]
     end}.

system_health_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets system health"),
          {ok, Health} = yawl_telemetry:system_health(),
          ?assert(is_map(Health)),
          ?assert(maps:is_key(uptime_ms, Health)),
          ?assert(maps:is_key(active_spans, Health)),
          ?assert(maps:is_key(total_metrics, Health)),
          ?assert(maps:is_key(memory, Health))
         ]
     end}.

component_status_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets component status"),
          Status = yawl_telemetry:component_status(),
          ?assert(is_map(Status)),
          ?assert(maps:is_key(telemetry, Status))
         ]
     end}.

register_health_check_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Registers custom health check"),
          CheckFn = fun() -> {ok, healthy} end,
          {ok, CheckRef} = yawl_telemetry:register_health_check(<<"custom_check">>, CheckFn),
          ?assert(is_reference(CheckRef))
         ]
     end}.

unregister_health_check_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Unregisters health check"),
          CheckFn = fun() -> {ok, healthy} end,
          {ok, CheckRef} = yawl_telemetry:register_health_check(<<"temp_check">>, CheckFn),
          ok = yawl_telemetry:unregister_health_check(CheckRef)
         ]
     end}.

%%====================================================================
%% Execution Visualization Tests
%%====================================================================

execution_graph_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets execution graph"),
          {ok, _SpanId} = yawl_telemetry:start_span(sequence, <<"graph-pattern">>),
          ok = yawl_telemetry:end_span(_SpanId, ok, ok),
          Result = yawl_telemetry:execution_graph(<<"graph-pattern">>),
          ?assertMatch({ok, _Graph}, Result)
         ]
     end}.

export_dot_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Exports DOT format"),
          {ok, _SpanId} = yawl_telemetry:start_span(sequence, <<"dot-pattern">>),
          ok = yawl_telemetry:end_span(_SpanId, ok, ok),
          {ok, Dot} = yawl_telemetry:export_dot(<<"dot-pattern">>),
          ?assert(is_binary(Dot)),
          ?assert(string:str(<<"digraph">>, Dot))
         ]
     end}.

execution_timeline_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets execution timeline"),
          {ok, _SpanId} = yawl_telemetry:start_span(sequence, <<"timeline-pattern">>),
          ok = yawl_telemetry:end_span(_SpanId, ok, ok),
          {ok, Timeline} = yawl_telemetry:execution_timeline(<<"timeline-pattern">>),
          ?assert(is_list(Timeline)),
          ?assert(length(Timeline) >= 2)
         ]
     end}.

get_execution_tree_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets execution tree"),
          {ok, _SpanId} = yawl_telemetry:start_span(sequence, <<"tree-pattern">>),
          ok = yawl_telemetry:end_span(_SpanId, ok, ok),
          {ok, Tree} = yawl_telemetry:get_execution_tree(<<"tree-pattern">>),
          ?assert(is_map(Tree))
         ]
     end}.

visualize_pattern_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Visualizes pattern"),
          {ok, Dot} = yawl_telemetry:visualize_pattern(sequence, <<"viz-pattern">>),
          ?assert(is_binary(Dot)),
          ?assert(string:str(<<"digraph">>, Dot))
         ]
     end}.

%%====================================================================
%% Alerting Tests
%%====================================================================

add_alert_rule_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Adds alert rule"),
          Condition = fun() -> false end,
          Action = fun() -> alerted end,
          {ok, AlertId} = yawl_telemetry:add_alert_rule(Condition, Action),
          ?assert(is_reference(AlertId))
         ]
     end}.

remove_alert_rule_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Removes alert rule"),
          Condition = fun() -> false end,
          Action = fun() -> ok end,
          {ok, AlertId} = yawl_telemetry:add_alert_rule(Condition, Action),
          ok = yawl_telemetry:remove_alert_rule(AlertId)
         ]
     end}.

check_alerts_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Checks alerts"),
          Condition = fun() -> false end,
          Action = fun() -> ok end,
          {ok, _AlertId} = yawl_telemetry:add_alert_rule(Condition, Action),
          Results = yawl_telemetry:check_alerts(),
          ?assert(is_list(Results))
         ]
     end}.

list_alert_rules_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Lists alert rules"),
          Condition = fun() -> false end,
          Action = fun() -> ok end,
          {ok, _AlertId} = yawl_telemetry:add_alert_rule(Condition, Action),
          Rules = yawl_telemetry:list_alert_rules(),
          ?assert(is_list(Rules)),
          ?assert(length(Rules) > 0)
         ]
     end}.

trigger_alert_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Triggers alert"),
          Condition = fun() -> false end,
          Action = fun() -> {triggered, ok} end,
          {ok, AlertId} = yawl_telemetry:add_alert_rule(Condition, Action),
          ok = yawl_telemetry:trigger_alert(AlertId)
         ]
     end}.

%%====================================================================
%% Audit Logging Tests
%%====================================================================

log_event_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs audit event"),
          ok = yawl_telemetry:log_event(test_event, <<"pattern-audit">>, #{key => value}),
          Filter = #{pattern_id => <<"pattern-audit">>},
          {ok, Results} = yawl_telemetry:query_audit(Filter),
          ?assert(length(Results) > 0)
         ]
     end}.

log_state_change_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs state change"),
          ok = yawl_telemetry:log_state_change(<<"pattern-state">>, pending, running),
          {ok, Log} = yawl_telemetry:get_audit_log(<<"pattern-state">>),
          ?assert(length(Log) > 0)
         ]
     end}.

query_audit_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Queries audit log"),
          ok = yawl_telemetry:log_event(event1, <<"pattern-query">>, #{}),
          ok = yawl_telemetry:log_event(event2, <<"pattern-query">>, #{}),

          Filter = #{pattern_id => <<"pattern-query">>},
          {ok, Results} = yawl_telemetry:query_audit(Filter),
          ?assert(length(Results) >= 2)
         ]
     end}.

get_audit_log_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets audit log for pattern"),
          ok = yawl_telemetry:log_event(test_event, <<"pattern-get-log">>, #{}),
          {ok, Log} = yawl_telemetry:get_audit_log(<<"pattern-get-log">>),
          ?assert(is_list(Log)),
          ?assert(length(Log) > 0)
         ]
     end}.

clear_audit_log_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Clears audit log"),
          ok = yawl_telemetry:log_event(event1, <<"pattern-clear">>, #{}),
          ok = yawl_telemetry:log_event(event2, <<"pattern-clear">>, #{}),

          ok = yawl_telemetry:clear_audit_log(<<"pattern-clear">>),
          {ok, Log} = yawl_telemetry:get_audit_log(<<"pattern-clear">>),
          ?assertEqual([], Log)
         ]
     end}.

clear_audit_log_all_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Clears all audit logs"),
          ok = yawl_telemetry:log_event(event1, <<"pattern-clear-all">>, #{}),
          ok = yawl_telemetry:clear_audit_log(),

          {ok, Summary} = yawl_telemetry:system_health(),
          AuditEntries = maps:get(audit_entries, Summary),
          ?assertEqual(0, AuditEntries)
         ]
     end}.

export_audit_log_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Exports audit log to file"),
          ok = yawl_telemetry:log_event(export_test, <<"pattern-export">>, #{}),
          Filename = "/tmp/yawl_audit_test.export",
          {ok, Count} = yawl_telemetry:export_audit_log(Filename, #{pattern_id => <<"pattern-export">>}),
          ?assert(Count > 0),
          ?assert(filelib:is_file(Filename)),

          file:delete(Filename)
         ]
     end}.
