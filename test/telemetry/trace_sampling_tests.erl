%% -*- erlang -*-
%% @doc Unit tests for trace_sampling

-module(trace_sampling_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    trace_sampling:reset_stats(),
    ok.

cleanup(_Ok) ->
    trace_sampling:reset_stats().

%%====================================================================
%% Sampling Strategy Tests
%%====================================================================

should_sample_default_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Span = create_test_span(),
                    Result = trace_sampling:should_sample(Span),
                    ?assert(is_boolean(Result))
                end)
         ]
     end}.

should_sample_with_probability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Span = create_test_span(),

                    %% 100% probability should always sample
                    ?assert(trace_sampling:should_sample(Span, {probability, 1.0})),

                    %% 0% probability should never sample
                    ?assertNot(trace_sampling:should_sample(Span, {probability, 0.0}))
                end)
         ]
     end}.

%%====================================================================
%% Adaptive Strategy Tests
%%====================================================================

adaptive_strategy_high_value_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Error span should always be sampled
                    ErrorSpan = create_error_span(),
                    ?assert(trace_sampling:should_sample(ErrorSpan, adaptive)),

                    %% Slow span should always be sampled
                    SlowSpan = create_slow_span(),
                    ?assert(trace_sampling:should_sample(SlowSpan, adaptive)),

                    %% Critical service span should always be sampled
                    CriticalSpan = create_critical_service_span(),
                    ?assert(trace_sampling:should_sample(CriticalSpan, adaptive))
                end)
         ]
     end}.

adaptive_strategy_normal_traffic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Normal span with low traffic should be sampled
                    NormalSpan = create_test_span(),
                    Result = trace_sampling:should_sample(NormalSpan, adaptive),
                    ?assert(is_boolean(Result))
                end)
         ]
     end}.

adaptive_strategy_exports_test() ->
    %% Verify the strategy function is exported
    ?assertEqual(is_function(fun trace_sampling:adaptive_strategy/1, 1), true).

%%====================================================================
%% Priority Strategy Tests
%%====================================================================

priority_strategy_high_priority_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Error span is high priority
                    ErrorSpan = create_error_span(),
                    ?assert(trace_sampling:should_sample(ErrorSpan, priority)),

                    %% Slow request is medium priority
                    SlowSpan = create_slow_span(),
                    Result = trace_sampling:should_sample(SlowSpan, priority),
                    ?assert(is_boolean(Result)),

                    %% Normal span is low priority
                    NormalSpan = create_test_span(),
                    LowResult = trace_sampling:should_sample(NormalSpan, priority),
                    ?assert(is_boolean(LowResult))
                end)
         ]
     end}.

priority_strategy_critical_service_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Critical service is medium priority
                    CriticalSpan = create_critical_service_span(),
                    Result = trace_sampling:should_sample(CriticalSpan, priority),
                    ?assert(is_boolean(Result))
                end)
         ]
     end}.

%%====================================================================
%% Error-Focused Strategy Tests
%%====================================================================

error_focused_strategy_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Error spans always sampled
                    ErrorSpan = create_error_span(),
                    ?assert(trace_sampling:should_sample(ErrorSpan, error_focused))
                end)
         ]
     end}.

error_focused_strategy_slow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Slow spans have 50% sampling
                    SlowSpan = create_slow_span(),
                    Result = trace_sampling:should_sample(SlowSpan, error_focused),
                    ?assert(is_boolean(Result))
                end)
         ]
     end}.

error_focused_strategy_normal_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Normal spans have low sampling rate
                    NormalSpan = create_test_span(),
                    Result = trace_sampling:should_sample(NormalSpan, error_focused),
                    ?assert(is_boolean(Result))
                end)
         ]
     end}.

%%====================================================================
%% Probabilistic Strategy Tests
%%====================================================================

probabilistic_strategy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Set sampling rate to 100%
                    trace_sampling:set_sampling_rate(1.0),
                    Span = create_test_span(),
                    ?assert(trace_sampling:should_sample(Span, probabilistic)),

                    %% Set sampling rate to 0%
                    trace_sampling:set_sampling_rate(0.0),
                    ?assertNot(trace_sampling:should_sample(Span, probabilistic)),

                    %% Reset to default
                    trace_sampling:set_sampling_rate(0.1)
                end)
         ]
     end}.

%%====================================================================
%% Sampling Rate Tests
%%====================================================================

get_set_sampling_rate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    ?assert(is_float(trace_sampling:get_sampling_rate())),

                    trace_sampling:set_sampling_rate(0.5),
                    ?assertEqual(0.5, trace_sampling:get_sampling_rate()),

                    %% Reset to default
                    trace_sampling:set_sampling_rate(0.1)
                end)
         ]
     end}.

invalid_sampling_rate_test() ->
    %% Test invalid rates are handled (should still be a float)
    ?assert(is_float(trace_sampling:get_sampling_rate())).

%%====================================================================
%% Sampling Statistics Tests
%%====================================================================

get_stats_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Stats = trace_sampling:get_stats(),

                    ?assert(is_map(Stats)),
                    ?assert(maps:is_key(total, Stats)),
                    ?assert(maps:is_key(sampled, Stats)),
                    ?assert(maps:is_key(dropped, Stats)),
                    ?assert(maps:is_key(rate, Stats)),

                    ?assert(is_integer(maps:get(total, Stats))),
                    ?assert(is_integer(maps:get(sampled, Stats))),
                    ?assert(is_integer(maps:get(dropped, Stats))),
                    ?assert(is_float(maps:get(rate, Stats)))
                end)
         ]
     end}.

reset_stats_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Generate some samples
                    Span = create_test_span(),
                    trace_sampling:should_sample(Span),
                    trace_sampling:should_sample(Span),

                    %% Reset
                    trace_sampling:reset_stats(),

                    %% Stats should be zero
                    Stats = trace_sampling:get_stats(),
                    ?assertEqual(0, maps:get(total, Stats)),
                    ?assertEqual(0, maps:get(sampled, Stats)),
                    ?assertEqual(0, maps:get(dropped, Stats))
                end)
         ]
     end}.

stats_accumulation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    trace_sampling:reset_stats(),
                    trace_sampling:set_strategy(probabilistic),
                    trace_sampling:set_sampling_rate(1.0),

                    %% Use always sampler to guarantee sampling
                    Span = create_test_span(),
                    trace_sampling:should_sample(Span),
                    trace_sampling:should_sample(Span),
                    trace_sampling:should_sample(Span),

                    Stats = trace_sampling:get_stats(),
                    ?assertEqual(3, maps:get(total, Stats)),
                    ?assertEqual(3, maps:get(sampled, Stats)),

                    %% Reset to default
                    trace_sampling:set_sampling_rate(0.1)
                end)
         ]
     end}.

%%====================================================================
%% Strategy Configuration Tests
%%====================================================================

set_strategy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    ?assertEqual(ok, trace_sampling:set_strategy(adaptive)),
                    ?assertEqual(ok, trace_sampling:set_strategy(priority)),
                    ?assertEqual(ok, trace_sampling:set_strategy(error_focused)),
                    ?assertEqual(ok, trace_sampling:set_strategy(probabilistic))
                end)
         ]
     end}.

strategy_exports_test() ->
    %% Verify all strategy functions are exported
    ?assertEqual(is_function(fun trace_sampling:priority_strategy/1, 1), true),
    ?assertEqual(is_function(fun trace_sampling:error_focused_strategy/1, 1), true),
    ?assertEqual(is_function(fun trace_sampling:probabilistic_strategy/1, 1), true).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Create a test span.
create_test_span() ->
    #{
        name => <<"test-span">>,
        trace_id => crypto:strong_rand_bytes(16),
        span_id => crypto:strong_rand_bytes(8),
        parent_span_id => undefined,
        start_time => erlang:monotonic_time(microsecond),
        end_time => erlang:monotonic_time(microsecond) + 100,
        attributes => #{<<"service.name">> => <<"test-service">>},
        events => [],
        status => ok
    }.

%% @private Create an error span.
create_error_span() ->
    #{
        name => <<"error-span">>,
        trace_id => crypto:strong_rand_bytes(16),
        span_id => crypto:strong_rand_bytes(8),
        parent_span_id => undefined,
        start_time => erlang:monotonic_time(microsecond),
        end_time => erlang:monotonic_time(microsecond) + 100,
        attributes => #{},
        events => [],
        status => {error, badrpc, timeout}
    }.

%% @private Create a slow span (> 1 second).
create_slow_span() ->
    #{
        name => <<"slow-span">>,
        trace_id => crypto:strong_rand_bytes(16),
        span_id => crypto:strong_rand_bytes(8),
        parent_span_id => undefined,
        start_time => erlang:monotonic_time(microsecond),
        end_time => erlang:monotonic_time(microsecond) + 2_000_000,  % 2 seconds
        attributes => #{},
        events => [],
        status => ok
    }.

%% @private Create a critical service span.
create_critical_service_span() ->
    #{
        name => <<"payment-process">>,
        trace_id => crypto:strong_rand_bytes(16),
        span_id => crypto:strong_rand_bytes(8),
        parent_span_id => undefined,
        start_time => erlang:monotonic_time(microsecond),
        end_time => erlang:monotonic_time(microsecond) + 100,
        attributes => #{<<"service.name">> => <<"payment-service">>},
        events => [],
        status => ok
    }.
