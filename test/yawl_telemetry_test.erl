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
%% @doc YAWL Telemetry Test Module
%%
%% Test suite for YAWL telemetry and observability features.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_telemetry_test).
-export([run_tests/0, test_span_lifecycle/0, test_metrics/0, test_health_checks/0]).

run_tests() ->
    io:format("Starting YAWL Telemetry Tests...~n"),
    test_span_lifecycle(),
    test_metrics(),
    test_health_checks(),
    io:format("All tests completed!~n").

test_span_lifecycle() ->
    io:format("~n=== Test: Span Lifecycle ===~n"),
    {ok, Pid} = yawl_telemetry:start_telemetry(),
    io:format("Telemetry started: ~p~n", [Pid]),

    %% Test span creation
    PatternType = implicit_termination,
    PatternId = <<"test_case_001">>,
    {ok, SpanId} = yawl_telemetry:start_span(PatternType, PatternId),
    io:format("Span created: ~p~n", [SpanId]),

    %% Test span attributes
    ok = yawl_telemetry:span_attribute(SpanId, test_attribute, 42),
    ok = yawl_telemetry:span_attribute(SpanId, description, <<"Test span">>),
    io:format("Attributes added to span~n"),

    %% Test span events
    ok = yawl_telemetry:span_event(SpanId, processing_started),
    ok = yawl_telemetry:span_event(SpanId, milestone_reached),
    io:format("Events added to span~n"),

    %% Test span status
    ok = yawl_telemetry:span_status(SpanId, running),
    io:format("Span status set to running~n"),

    %% Get span info
    {ok, SpanInfo} = yawl_telemetry:get_span_info(SpanId),
    io:format("Span info: ~p~n", [SpanInfo]),

    %% Test span completion
    ok = yawl_telemetry:end_span(SpanId, {ok, success}, ok),
    io:format("Span completed~n"),

    %% Get active spans (should be empty now)
    ActiveSpans = yawl_telemetry:get_active_spans(),
    io:format("Active spans: ~p~n", [ActiveSpans]),

    yawl_telemetry:stop_telemetry(),
    io:format("Telemetry stopped~n").

test_metrics() ->
    io:format("~n=== Test: Metrics Collection ===~n"),
    {ok, _Pid} = yawl_telemetry:start_telemetry(),

    %% Test execution start metric
    ok = yawl_telemetry:record_execution_start(multiple_instances_no_sync, <<"case_002">>),
    io:format("Recorded execution start~n"),

    %% Test timing metric
    ok = yawl_telemetry:record_timing(multiple_instances_no_sync, allocation, 150),
    io:format("Recorded timing metric~n"),

    %% Test counter increment
    Tags = maps:put(component, test, maps:new()),
    ok = yawl_telemetry:increment_counter(test_counter, Tags),
    io:format("Incremented counter~n"),

    %% Test execution complete
    ok = yawl_telemetry:record_execution_complete(multiple_instances_no_sync, <<"case_002">>, 1250),
    io:format("Recorded execution complete~n"),

    %% Get metrics summary
    Summary = yawl_telemetry:get_metrics_summary(),
    io:format("Metrics summary: ~p~n", [Summary]),

    %% Export Prometheus metrics
    PrometheusOutput = yawl_telemetry:export_prometheus(),
    io:format("Prometheus output:~n~s~n", [PrometheusOutput]),

    yawl_telemetry:stop_telemetry().

test_health_checks() ->
    io:format("~n=== Test: Health Checks ===~n"),
    {ok, _Pid} = yawl_telemetry:start_telemetry(),

    %% Test system health
    {ok, SystemHealth} = yawl_telemetry:system_health(),
    io:format("System health: ~p~n", [SystemHealth]),

    %% Test component status
    ComponentStatus = yawl_telemetry:component_status(),
    io:format("Component status: ~p~n", [ComponentStatus]),

    %% Test pattern health (non-existent pattern)
    PatternHealth = yawl_telemetry:check_pattern_health(<<"non_existent">>),
    io:format("Pattern health (non-existent): ~p~n", [PatternHealth]),

    %% Test custom health check registration
    CheckFun = fun() -> {ok, healthy, maps:put(message, <<"Custom check OK">>, maps:new())} end,
    {ok, CheckRef} = yawl_telemetry:register_health_check(<<"custom_check">>, CheckFun),
    io:format("Registered custom health check: ~p~n", [CheckRef]),

    %% Get system health again (should include custom check)
    {ok, SystemHealth2} = yawl_telemetry:system_health(),
    CustomChecks = maps:get(custom_checks, SystemHealth2, maps:new()),
    io:format("System health with custom check: ~p~n", [CustomChecks]),

    %% Unregister health check
    ok = yawl_telemetry:unregister_health_check(CheckRef),
    io:format("Unregistered custom health check~n"),

    yawl_telemetry:stop_telemetry().
