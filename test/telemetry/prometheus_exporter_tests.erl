%% -*- erlang -*-
%% @doc Unit tests for prometheus_exporter

-module(prometheus_exporter_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, MetricsPid} = otel_metrics:start_link(),
    {ok, ExporterPid} = prometheus_exporter:start_link([{port, 19091}]),
    {MetricsPid, ExporterPid}.

cleanup({MetricsPid, ExporterPid}) ->
    prometheus_exporter:stop(),
    otel_metrics:stop(),
    timer:sleep(100).

%%====================================================================
%% HTTP Endpoint Tests
%%====================================================================

health_endpoint_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    ?assertEqual(up, prometheus_exporter:health())
                end)
         ]
     end}.

metrics_endpoint_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Register and record some metrics
                    otel_metrics:register_counter(test_metric, <<"Test metric">>),
                    otel_metrics:inc_counter(test_metric, #{}, 1),

                    %% Get metrics via the exporter
                    Metrics = prometheus_exporter:get_metrics(),
                    MetricsBin = iolist_to_binary(Metrics),

                    %% Verify the metric is present
                    ?assertMatch(true, binary:match(MetricsBin, <<"test_metric_total">>) =/= nomatch)
                end)
         ]
     end}.

%%====================================================================
%% Metrics Format Tests
%%====================================================================

prometheus_format_compliance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_counter(compliance_counter, <<"Compliance test">>),
                    otel_metrics:inc_counter(compliance_counter, #{label => value}, 1),

                    Metrics = iolist_to_binary(prometheus_exporter:get_metrics()),

                    %% Check for required Prometheus elements
                    ?assertMatch(true, binary:match(Metrics, <<"# HELP">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"# TYPE">>) =/= nomatch),

                    %% Check metric format: name labels value
                    ?assertMatch(true, binary:match(Metrics, <<"compliance_counter_total">>) =/= nomatch)
                end)
         ]
     end}.

histogram_format_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_histogram(test_histogram, <<"Histogram format test">>),
                    otel_metrics:record_histogram(test_histogram, #{}, 0.5),

                    Metrics = iolist_to_binary(prometheus_exporter:get_metrics()),

                    %% Histogram should have _bucket, _count, and _sum suffixes
                    ?assertMatch(true, binary:match(Metrics, <<"test_histogram_bucket">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"test_histogram_count">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"test_histogram_sum">>) =/= nomatch)
                end)
         ]
     end}.

label_format_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_gauge(label_gauge, <<"Label format test">>),
                    otel_metrics:set_gauge(label_gauge, #{key1 => val1, key2 => val2}, 42),

                    Metrics = iolist_to_binary(prometheus_exporter:get_metrics()),

                    %% Labels should be formatted as {key1="val1",key2="val2"}
                    ?assertMatch(true, binary:match(Metrics, <<"key1=\"val1\"">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"key2=\"val2\"">>) =/= nomatch)
                end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

exporter_unavailable_test() ->
    %% When exporter is not running, health should return down
    ?assertEqual(down, prometheus_exporter:health()).

metrics_without_exporter_test() ->
    %% get_metrics/0 should still work even if exporter is not running
    %% because it delegates to otel_metrics
    {ok, _} = otel_metrics:start_link(),
    try
        Metrics = prometheus_exporter:get_metrics(),
        ?assert(is_list(Metrics))
    after
        otel_metrics:stop()
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

full_export_cycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Register various metric types
                    otel_metrics:register_counter(full_counter, <<"Full cycle counter">>),
                    otel_metrics:register_gauge(full_gauge, <<"Full cycle gauge">>),
                    otel_metrics:register_histogram(full_histogram, <<"Full cycle histogram">>),

                    %% Record values
                    otel_metrics:inc_counter(full_counter, #{type => a}, 10),
                    otel_metrics:inc_counter(full_counter, #{type => b}, 5),
                    otel_metrics:set_gauge(full_gauge, #{}, 100),
                    otel_metrics:record_histogram(full_histogram, #{}, 1.5),
                    otel_metrics:record_histogram(full_histogram, #{}, 5.0),

                    %% Export and verify
                    Metrics = iolist_to_binary(prometheus_exporter:get_metrics()),

                    %% All metrics should be present
                    ?assertMatch(true, binary:match(Metrics, <<"full_counter_total">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"full_gauge">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"full_histogram">>) =/= nomatch),

                    %% Verify labels are present
                    ?assertMatch(true, binary:match(Metrics, <<"type=\"a\"">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"type=\"b\"">>) =/= nomatch)
                end)
         ]
     end}.

%%====================================================================
%% Mock Tests
%%====================================================================

mocked_export_test_() ->
    {setup,
     fun() ->
             %% Start with mock configuration
             {ok, Pid} = otel_metrics:start_link(),
             Pid
     end,
     fun(Pid) ->
             otel_metrics:stop()
     end,
     fun(_) ->
         [
          ?_test(begin
                    %% Verify metrics are stored without needing exporter
                    otel_metrics:register_counter(mock_counter, <<"Mock test">>),
                    otel_metrics:inc_counter(mock_counter, #{}, 42),
                    ?assertEqual({ok, 42}, otel_metrics:get_metric(mock_counter, #{}))
                end)
         ]
     end}.

%%====================================================================
%% Type Validation Tests
%%====================================================================

type_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Verify metric types are correctly formatted
                    otel_metrics:register_counter(type_counter, <<"Type counter">>),
                    otel_metrics:register_gauge(type_gauge, <<"Type gauge">>),
                    otel_metrics:register_histogram(type_histogram, <<"Type histogram">>),

                    Metrics = iolist_to_binary(prometheus_exporter:get_metrics()),

                    %% Check TYPE declarations
                    ?assertMatch(true, binary:match(Metrics, <<"# TYPE type_counter_total counter">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"# TYPE type_gauge gauge">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Metrics, <<"# TYPE type_histogram histogram">>) =/= nomatch)
                end)
         ]
     end}.
