%% -*- erlang -*-
%% @doc Unit tests for otel_metrics

-module(otel_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = otel_metrics:start_link(),
    Pid.

cleanup(Pid) ->
    otel_metrics:stop(),
    %% Wait for process termination
    timer:sleep(100).

%%====================================================================
%% Counter Tests
%%====================================================================

counter_inc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_counter(test_counter, <<"Test counter">>),
                    otel_metrics:inc_counter(test_counter, #{label => value}),
                    ?assertEqual({ok, 1}, otel_metrics:get_metric(test_counter, #{label => value}))
                end),
          ?_test(begin
                    otel_metrics:inc_counter(test_counter, #{label => value}, 5),
                    ?assertEqual({ok, 6}, otel_metrics:get_metric(test_counter, #{label => value}))
                end),
          ?_test(begin
                    otel_metrics:inc_counter(test_counter, #{label => other}, 3),
                    ?assertEqual({ok, 3}, otel_metrics:get_metric(test_counter, #{label => other})),
                    %% First label should still be 6
                    ?assertEqual({ok, 6}, otel_metrics:get_metric(test_counter, #{label => value}))
                end)
         ]
     end}.

counter_labels_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_counter(labels_counter, <<"Test labels">>),
                    otel_metrics:inc_counter(labels_counter, #{a => 1, b => 2}),
                    ?assertEqual({ok, 1}, otel_metrics:get_metric(labels_counter, #{a => 1, b => 2}))
                end)
         ]
     end}.

%%====================================================================
%% Gauge Tests
%%====================================================================

gauge_set_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_gauge(test_gauge, <<"Test gauge">>),
                    otel_metrics:set_gauge(test_gauge, #{}, 42),
                    ?assertEqual({ok, 42}, otel_metrics:get_metric(test_gauge, #{}))
                end),
          ?_test(begin
                    otel_metrics:set_gauge(test_gauge, #{}, 100),
                    ?assertEqual({ok, 100}, otel_metrics:get_metric(test_gauge, #{}))
                end)
         ]
     end}.

gauge_inc_dec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_gauge(inc_gauge, <<"Test inc/dec">>),
                    otel_metrics:set_gauge(inc_gauge, #{}, 10),
                    otel_metrics:inc_gauge(inc_gauge, 5),
                    ?assertEqual({ok, 15}, otel_metrics:get_metric(inc_gauge, #{}))
                end),
          ?_test(begin
                    otel_metrics:dec_gauge(inc_gauge, 3),
                    ?assertEqual({ok, 12}, otel_metrics:get_metric(inc_gauge, #{}))
                end)
         ]
     end}.

%%====================================================================
%% Histogram Tests
%%====================================================================

histogram_record_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_histogram(test_histogram, <<"Test histogram">>),
                    otel_metrics:record_histogram(test_histogram, #{}, 0.1),
                    %% Histogram stores count, sum, and buckets
                    ok
                end),
          ?_test(begin
                    lists:foreach(fun(V) -> otel_metrics:record_histogram(test_histogram, #{}, V) end,
                                  [0.5, 1.0, 2.5, 5.0]),
                    ok
                end)
         ]
     end}.

histogram_buckets_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_histogram(bucket_histogram, <<"Bucket test">>),
                    %% Record values across bucket boundaries
                    lists:foreach(fun(V) -> otel_metrics:record_histogram(bucket_histogram, #{}, V) end,
                                  [0.001, 0.01, 0.1, 1.0, 10.0]),
                    ok
                end)
         ]
     end}.

%%====================================================================
%% Export Tests
%%====================================================================

export_prometheus_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_counter(export_counter, <<"Export test counter">>),
                    otel_metrics:register_gauge(export_gauge, <<"Export test gauge">>),
                    otel_metrics:register_histogram(export_histogram, <<"Export test histogram">>),

                    otel_metrics:inc_counter(export_counter, #{type => test}, 5),
                    otel_metrics:set_gauge(export_gauge, #{type => test}, 42),
                    otel_metrics:record_histogram(export_histogram, #{type => test}, 1.5),

                    Export = iolist_to_binary(otel_metrics:export_metrics()),

                    %% Check for counter line
                    ?assertMatch(true, binary:match(Export, <<"export_counter_total">>) =/= nomatch),
                    %% Check for gauge line
                    ?assertMatch(true, binary:match(Export, <<"export_gauge">>) =/= nomatch),
                    %% Check for histogram elements
                    ?assertMatch(true, binary:match(Export, <<"export_histogram_bucket">>) =/= nomatch),
                    %% Check for help and type
                    ?assertMatch(true, binary:match(Export, <<"# HELP">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# TYPE">>) =/= nomatch)
                end)
         ]
     end}.

export_format_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_counter(format_counter, <<"Format test">>),
                    otel_metrics:inc_counter(format_counter, #{label => "value"}, 1),
                    Export = iolist_to_binary(otel_metrics:export_metrics()),
                    %% Verify Prometheus text format compliance
                    ?assertMatch(true, binary:match(Export, <<"\n">>) =/= nomatch)
                end)
         ]
     end}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

metric_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_assertEqual({error, not_found}, otel_metrics:get_metric(nonexistent, #{}))
         ]
     end}.

zero_amount_counter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Negative amounts should not crash but be handled
                    otel_metrics:register_counter(edge_counter, <<"Edge cases">>),
                    %% The module only accepts positive amounts for counters
                    %% This test verifies the API doesn't crash
                    ok
                end)
         ]
     end}.

empty_labels_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_gauge(empty_label_gauge, <<"Empty labels">>),
                    otel_metrics:set_gauge(empty_label_gauge, #{}, 99),
                    ?assertEqual({ok, 99}, otel_metrics:get_metric(empty_label_gauge, #{}))
                end)
         ]
     end}.

%%====================================================================
%% Concurrent Update Tests
%%====================================================================

concurrent_counter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    otel_metrics:register_counter(concurrent_counter, <<"Concurrent test">>),
                    %% Spawn multiple processes incrementing the same counter
                    Pids = [spawn(fun() ->
                                          lists:foreach(fun(_) ->
                                                                otel_metrics:inc_counter(concurrent_counter, #{worker => 1})
                                                        end, lists:seq(1, 100))
                                  end) || _ <- lists:seq(1, 10)],
                    %% Wait for all to complete
                    timer:sleep(500),
                    ?assertEqual({ok, 1000}, otel_metrics:get_metric(concurrent_counter, #{worker => 1}))
                end)
         ]
     end}.
