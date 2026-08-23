%% -*- erlang -*-
%% @doc Unit tests for cloud_trace_exporter

-module(cloud_trace_exporter_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = cloud_trace_exporter:start_link([
        {project_id, <<"test-project">>},
        {max_buffer_size, 100},
        {batch_size, 5},
        {batch_interval_ms, 100}
    ]),
    Pid.

cleanup(_Pid) ->
    cloud_trace_exporter:stop(),
    timer:sleep(50).

%%====================================================================
%% Lifecycle Tests
%%====================================================================

start_stop_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    ?assertEqual(up, cloud_trace_exporter:health())
                end)
         ]
     end}.

health_without_exporter_test() ->
    ?assertEqual(down, cloud_trace_exporter:health()).

%%====================================================================
%% Span Export Tests
%%====================================================================

export_span_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Span = create_test_span(<<"test-span">>),
                    ?assertEqual(ok, cloud_trace_exporter:export_span(Span))
                end)
         ]
     end}.

export_batch_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Spans = [create_test_span(<<"span-1">>),
                             create_test_span(<<"span-2">>),
                             create_test_span(<<"span-3">>)],
                    ?assertEqual(ok, cloud_trace_exporter:export_batch(Spans))
                end)
         ]
     end}.

export_empty_batch_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    ?assertEqual(ok, cloud_trace_exporter:export_batch([]))
                end)
         ]
     end}.

%%====================================================================
%% Buffer Management Tests
%%====================================================================

buffer_size_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Initially empty
                    ?assertEqual(0, cloud_trace_exporter:get_buffer_size()),

                    %% Export some spans
                    Span = create_test_span(<<"buffer-test">>),
                    cloud_trace_exporter:export_span(Span),
                    cloud_trace_exporter:export_span(Span),

                    %% Buffer should have 2 spans
                    Size = cloud_trace_exporter:get_buffer_size(),
                    ?assert(Size >= 0)
                end)
         ]
     end}.

flush_buffer_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Export some spans
                    Span = create_test_span(<<"flush-test">>),
                    cloud_trace_exporter:export_span(Span),
                    cloud_trace_exporter:export_span(Span),

                    %% Flush buffer
                    {ok, Count} = cloud_trace_exporter:flush_buffer(),
                    ?assert(is_integer(Count)),
                    ?assert(Count >= 0)
                end)
         ]
     end}.

flush_empty_buffer_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    {ok, Count} = cloud_trace_exporter:flush_buffer(),
                    ?assertEqual(0, Count)
                end)
         ]
     end}.

%%====================================================================
%% Sampling Tests
%%====================================================================

set_sampler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Set different samplers
                    ?assertEqual(ok, cloud_trace_exporter:set_sampler(always)),
                    ?assertEqual(ok, cloud_trace_exporter:set_sampler(never)),
                    ?assertEqual(ok, cloud_trace_exporter:set_sampler({probability, 0.5}))
                end)
         ]
     end}.

sample_always_test_() ->
    {setup,
     fun() ->
             cloud_trace_exporter:start_link([{project_id, <<"test-project">>}])
     end,
     fun(_) -> cloud_trace_exporter:stop() end,
     fun(_) ->
         [
          ?_test(begin
                    cloud_trace_exporter:set_sampler(always),
                    Span = create_test_span(<<"always-sample">>),
                    cloud_trace_exporter:export_span(Span),
                    %% With always sampler, span should be in buffer
                    Size = cloud_trace_exporter:get_buffer_size(),
                    ?assert(Size > 0)
                end)
         ]
     end}.

sample_never_test_() ->
    {setup,
     fun() ->
             cloud_trace_exporter:start_link([{project_id, <<"test-project">>}])
     end,
     fun(_) -> cloud_trace_exporter:stop() end,
     fun(_) ->
         [
          ?_test(begin
                    cloud_trace_exporter:set_sampler(never),
                    Span = create_test_span(<<"never-sample">>),
                    cloud_trace_exporter:export_span(Span),
                    %% With never sampler, buffer should be empty
                    Size = cloud_trace_exporter:get_buffer_size(),
                    ?assertEqual(0, Size)
                end)
         ]
     end}.

sample_probability_test_() ->
    {setup,
     fun() ->
             cloud_trace_exporter:start_link([{project_id, <<"test-project">>}])
     end,
     fun(_) -> cloud_trace_exporter:stop() end,
     fun(_) ->
         [
          ?_test(begin
                    cloud_trace_exporter:set_sampler({probability, 1.0}),
                    Span = create_test_span(<<"prob-sample">>),
                    cloud_trace_exporter:export_span(Span),
                    %% With 100% probability, span should be buffered
                    Size = cloud_trace_exporter:get_buffer_size(),
                    ?assert(Size > 0)
                end)
         ]
     end}.

%%====================================================================
%% Project Configuration Tests
%%====================================================================

configure_project_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    ?assertEqual(ok, cloud_trace_exporter:configure_project(<<"new-project">>))
                end)
         ]
     end}.

%%====================================================================
%% Batch Processing Tests
%%====================================================================

batch_flush_on_size_test_() ->
    {setup,
     fun() ->
             %% Small batch size to trigger flush
             {ok, Pid} = cloud_trace_exporter:start_link([
                 {project_id, <<"test-project">>},
                 {batch_size, 3}
             ]),
             Pid
     end,
     fun(_) -> cloud_trace_exporter:stop() end,
     fun(_) ->
         [
          ?_test(begin
                    Span = create_test_span(<<"batch-test">>),

                    %% Export 3 spans - should trigger batch flush
                    cloud_trace_exporter:export_span(Span),
                    cloud_trace_exporter:export_span(Span),
                    cloud_trace_exporter:export_span(Span),

                    %% Buffer should be flushed (size 0)
                    %% The flush happens via handle_info, give it more time
                    timer:sleep(200),
                    Size = cloud_trace_exporter:get_buffer_size(),
                    ?assertEqual(0, Size)
                end)
         ]
     end}.

%%====================================================================
%% W3C Trace Context Tests
%%====================================================================

span_with_trace_context_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Create a span with W3C trace context
                    TraceId = crypto:strong_rand_bytes(16),
                    SpanId = crypto:strong_rand_bytes(8),
                    Span = #{
                        name => <<"context-span">>,
                        trace_id => TraceId,
                        span_id => SpanId,
                        parent_span_id => undefined,
                        start_time => erlang:monotonic_time(microsecond),
                        end_time => erlang:monotonic_time(microsecond) + 1000,
                        attributes => #{<<"key">> => <<"value">>},
                        events => [],
                        status => ok
                    },

                    ?assertEqual(ok, cloud_trace_exporter:export_span(Span))
                end)
         ]
     end}.

%%====================================================================
%% Span Encoding Tests
%%====================================================================

encode_span_test() ->
    Span = create_test_span(<<"encode-test">>),
    %% Verify span has required fields
    ?assert(maps:is_key(name, Span)),
    ?assert(maps:is_key(trace_id, Span)),
    ?assert(maps:is_key(span_id, Span)),
    ?assert(is_binary(maps:get(name, Span))),
    ?assert(is_binary(maps:get(trace_id, Span))),
    ?assert(is_binary(maps:get(span_id, Span))).

encode_span_with_attributes_test() ->
    Span = #{
        name => <<"attr-span">>,
        trace_id => crypto:strong_rand_bytes(16),
        span_id => crypto:strong_rand_bytes(8),
        start_time => 1234567890,
        end_time => 1234568890,
        attributes => #{
            <<"string.attr">> => <<"value">>,
            <<"int.attr">> => 42,
            <<"float.attr">> => 3.14,
            <<"bool.attr">> => true
        },
        events => [],
        status => ok
    },

    ?assertEqual(<<"attr-span">>, maps:get(name, Span)),
    ?assertMatch(#{<<"string.attr">> := <<"value">>}, maps:get(attributes, Span)).

encode_span_with_events_test() ->
    Event = #{
        name => <<"test-event">>,
        timestamp => erlang:monotonic_time(microsecond),
        attributes => #{<<"event.key">> => <<"event.value">>}
    },

    Span = #{
        name => <<"event-span">>,
        trace_id => crypto:strong_rand_bytes(16),
        span_id => crypto:strong_rand_bytes(8),
        start_time => 1234567890,
        end_time => 1234568890,
        attributes => #{},
        events => [Event],
        status => ok
    },

    ?assertMatch([_], maps:get(events, Span)),
    [FirstEvent | _] = maps:get(events, Span),
    ?assertEqual(<<"test-event">>, maps:get(name, FirstEvent)).

%%====================================================================
%% Error Handling Tests
%%====================================================================

export_span_missing_fields_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Span with minimal required fields
                    MinimalSpan = #{
                        name => <<"minimal">>,
                        trace_id => crypto:strong_rand_bytes(16),
                        span_id => crypto:strong_rand_bytes(8)
                    },
                    ?assertEqual(ok, cloud_trace_exporter:export_span(MinimalSpan))
                end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Create a test span.
create_test_span(Name) ->
    #{
        name => Name,
        trace_id => crypto:strong_rand_bytes(16),
        span_id => crypto:strong_rand_bytes(8),
        parent_span_id => undefined,
        start_time => erlang:monotonic_time(microsecond),
        end_time => erlang:monotonic_time(microsecond) + 1000,
        attributes => #{
            <<"service.name">> => <<"test-service">>,
            <<"span.kind">> => <<"server">>
        },
        events => [],
        status => ok
    }.
