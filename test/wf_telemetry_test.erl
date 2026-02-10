%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for wf_telemetry gen_server.
%%%
%%% Tests span lifecycle, metrics recording and retrieval,
%%% place and transition tracking, pattern statistics,
%%% configuration, auto-instrumentation, and cleanup.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_telemetry_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    case whereis(wf_telemetry) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    timer:sleep(10).

cleanup(_) ->
    case whereis(wf_telemetry) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%%%===================================================================
%%% Server Lifecycle Tests
%%%===================================================================

start_stop_default_test() ->
    setup(),
    {ok, Pid} = wf_telemetry:start_link(),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(wf_telemetry)),
    ok = wf_telemetry:stop(),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(wf_telemetry)),
    cleanup(ok).

start_with_config_test() ->
    setup(),
    Config = #{enabled => false, auto_instrument => true},
    {ok, Pid} = wf_telemetry:start_link(Config),
    ?assert(is_pid(Pid)),
    ok = wf_telemetry:stop(),
    cleanup(ok).

start_registered_name_test() ->
    setup(),
    {ok, Pid} = wf_telemetry:start_link(),
    ?assertEqual(Pid, whereis(wf_telemetry)),
    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Span Lifecycle Tests
%%%===================================================================

span_transition_start_creates_span_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, SpanId} = wf_telemetry:span_transition_start(test_net, t1, #{p1 => [a, b]}),
    ?assert(is_reference(SpanId)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

span_transition_disabled_returns_undefined_test() ->
    setup(),
    Config = #{enabled => false},
    {ok, _Pid} = wf_telemetry:start_link(Config),

    {ok, SpanId} = wf_telemetry:span_transition_start(test_net, t1, #{p1 => []}),
    ?assertEqual(undefined, SpanId),

    ok = wf_telemetry:stop(),
    cleanup(ok).

span_transition_end_updates_metrics_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, SpanId} = wf_telemetry:span_transition_start(test_net, t1, #{p1 => [a]}),
    ?assert(is_reference(SpanId)),

    ok = wf_telemetry:span_transition_end(SpanId, {produce, #{p2 => [b]}}),
    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(test_net),
    ?assert(length(Metrics) > 0),

    [Metric | _] = Metrics,
    ?assertEqual(t1, maps:get(transition, Metric)),
    ?assertEqual(1, maps:get(fire_count, Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

span_transition_abort_tracked_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, SpanId} = wf_telemetry:span_transition_start(test_net, t1, #{}),
    ok = wf_telemetry:span_transition_end(SpanId, abort),
    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(test_net),
    [Metric | _] = Metrics,

    ?assertEqual(1, maps:get(abort_count, Metric)),
    ?assertEqual(0, maps:get(fire_count, Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

span_pattern_execution_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    Attrs = #{sequence_id => 42},
    {ok, SpanId} = wf_telemetry:span_pattern_execution(test_net, seq_pattern, Attrs),
    ?assert(is_reference(SpanId)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

span_end_with_invalid_id_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    InvalidId = make_ref(),
    ok = wf_telemetry:span_transition_end(InvalidId, {produce, #{}}),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Transition Metrics Tests
%%%===================================================================

transition_metrics_empty_initially_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(nonexistent_net),
    ?assertEqual([], Metrics),

    ok = wf_telemetry:stop(),
    cleanup(ok).

transition_metrics_aggregation_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    %% Fire t1 twice successfully
    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{p1 => [a]}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{p2 => [b]}}),

    {ok, S2} = wf_telemetry:span_transition_start(net1, t1, #{p1 => [c]}),
    ok = wf_telemetry:span_transition_end(S2, {produce, #{p2 => [d]}}),

    %% Fire t1 once with abort
    {ok, S3} = wf_telemetry:span_transition_start(net1, t1, #{p1 => [e]}),
    ok = wf_telemetry:span_transition_end(S3, abort),

    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(net1),
    ?assertEqual(1, length(Metrics)),

    [Metric] = Metrics,
    ?assertEqual(2, maps:get(fire_count, Metric)),
    ?assertEqual(1, maps:get(abort_count, Metric)),
    ?assert(maps:get(total_duration_us, Metric) > 0),
    ?assert(maps:get(avg_duration_us, Metric) > 0),

    ok = wf_telemetry:stop(),
    cleanup(ok).

transition_metrics_min_max_duration_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    timer:sleep(5),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    {ok, S2} = wf_telemetry:span_transition_start(net1, t1, #{}),
    timer:sleep(15),
    ok = wf_telemetry:span_transition_end(S2, {produce, #{}}),

    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(net1),
    [Metric] = Metrics,

    MinDuration = maps:get(min_duration_us, Metric),
    MaxDuration = maps:get(max_duration_us, Metric),

    ?assert(MinDuration > 0),
    ?assert(MaxDuration > MinDuration),

    ok = wf_telemetry:stop(),
    cleanup(ok).

transition_metrics_multiple_transitions_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    {ok, S2} = wf_telemetry:span_transition_start(net1, t2, #{}),
    ok = wf_telemetry:span_transition_end(S2, {produce, #{}}),

    {ok, S3} = wf_telemetry:span_transition_start(net1, t3, #{}),
    ok = wf_telemetry:span_transition_end(S3, {produce, #{}}),

    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(net1),
    ?assertEqual(3, length(Metrics)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Place Metrics Tests
%%%===================================================================

instrument_marking_creates_place_metric_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    Marking = [{token, 1}, {token, 2}, {token, 3}],
    ok = wf_telemetry:instrument_marking(net1, p1, Marking),
    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_place_metrics(net1),
    ?assertEqual(1, length(Metrics)),

    [Metric] = Metrics,
    ?assertEqual(p1, maps:get(place, Metric)),
    ?assertEqual(3, maps:get(token_count, Metric)),
    ?assertEqual(3, maps:get(high_water_mark, Metric)),
    ?assertEqual(3, maps:get(low_water_mark, Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

record_place_change_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    Added = [{token, 1}, {token, 2}],
    Removed = [{token, 3}],
    ok = wf_telemetry:record_place_change(net1, p1, Added, Removed),
    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_place_metrics(net1),
    [Metric] = Metrics,

    ?assertEqual(1, maps:get(token_count, Metric)),  %% 2 - 1 = 1
    ?assertEqual(2, maps:get(total_additions, Metric)),
    ?assertEqual(1, maps:get(total_removals, Metric)),
    ?assertEqual(2, maps:get(high_water_mark, Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

place_metrics_accumulation_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    %% Record multiple changes on same place
    ok = wf_telemetry:record_place_change(net1, p1, [{t1}, {t2}], []),
    ok = wf_telemetry:record_place_change(net1, p1, [], [{t1}]),
    ok = wf_telemetry:record_place_change(net1, p1, [{t3}], [{t2}]),

    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_place_metrics(net1),
    [Metric] = Metrics,

    ?assertEqual(1, maps:get(token_count, Metric)),  %% 2 - 1 + 1 = 2, then 2 - 1 = 1
    ?assertEqual(3, maps:get(total_additions, Metric)),
    ?assertEqual(2, maps:get(total_removals, Metric)),
    ?assertEqual(2, maps:get(high_water_mark, Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

place_metrics_multiple_places_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    ok = wf_telemetry:instrument_marking(net1, p1, [{t1}, {t2}]),
    ok = wf_telemetry:instrument_marking(net1, p2, [{t3}]),
    ok = wf_telemetry:instrument_marking(net1, p3, []),

    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_place_metrics(net1),
    ?assertEqual(3, length(Metrics)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Pattern Stats Tests
%%%===================================================================

pattern_stats_not_found_initially_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {error, not_found} = wf_telemetry:get_pattern_stats(nonexistent_net),

    ok = wf_telemetry:stop(),
    cleanup(ok).

enable_auto_instrumentation_creates_stat_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    ok = wf_telemetry:enable_auto_instrumentation(net1),

    {ok, Stat} = wf_telemetry:get_pattern_stats(net1),
    ?assertEqual(net1, maps:get(net_mod, Stat)),
    ?assertEqual(0, maps:get(total_transitions, Stat)),
    ?assert(is_integer(maps:get(start_time, Stat))),
    ?assert(is_integer(maps:get(last_activity, Stat))),

    ok = wf_telemetry:stop(),
    cleanup(ok).

pattern_stats_tracking_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    ok = wf_telemetry:enable_auto_instrumentation(net1),

    %% Simulate transition firings
    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    {ok, S2} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S2, {produce, #{}}),

    timer:sleep(10),

    {ok, Stat} = wf_telemetry:get_pattern_stats(net1),
    ?assertEqual(2, maps:get(total_transitions, Stat)),
    ?assert(maps:get(throughput, Stat) > 0.0),

    ok = wf_telemetry:stop(),
    cleanup(ok).

auto_instrumentation_disabled_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    ok = wf_telemetry:enable_auto_instrumentation(net1),
    ok = wf_telemetry:disable_auto_instrumentation(),

    {error, not_found} = wf_telemetry:get_pattern_stats(net1),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Cast Operations Tests
%%%===================================================================

instrument_transition_cast_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(#{auto_instrument => false}),

    ok = wf_telemetry:instrument_transition(net1, t1, #{p1 => [a]}, #{p2 => [b]}),
    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(net1),
    ?assertEqual([], Metrics),  %% Not recorded since auto_instrument is false

    ok = wf_telemetry:stop(),
    cleanup(ok).

instrument_transition_with_auto_enabled_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(#{auto_instrument => true}),

    ok = wf_telemetry:enable_auto_instrumentation(net1),
    ok = wf_telemetry:instrument_transition(net1, t1, #{p1 => [a]}, #{p2 => [b]}),
    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(net1),
    ?assertEqual(1, length(Metrics)),

    [Metric] = Metrics,
    ?assertEqual(1, maps:get(fire_count, Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

track_token_flow_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    Tokens = [{token, 1}, {token, 2}],
    ok = wf_telemetry:track_token_flow(net1, p1, p2, Tokens),
    timer:sleep(10),

    ok = wf_telemetry:stop(),
    cleanup(ok).

emit_firing_event_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    ok = wf_telemetry:emit_firing_event(net1, t1, #{p1 => [a]}),
    timer:sleep(10),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Reset and Export Tests
%%%===================================================================

reset_metrics_clears_all_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    %% Record some metrics
    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    ok = wf_telemetry:instrument_marking(net1, p1, [{t}]),

    timer:sleep(10),

    {ok, TMetrics1} = wf_telemetry:get_transition_metrics(net1),
    {ok, PMetrics1} = wf_telemetry:get_place_metrics(net1),

    ?assertEqual(1, length(TMetrics1)),
    ?assertEqual(1, length(PMetrics1)),

    %% Reset
    ok = wf_telemetry:reset_metrics(),
    timer:sleep(10),

    {ok, TMetrics2} = wf_telemetry:get_transition_metrics(net1),
    {ok, PMetrics2} = wf_telemetry:get_place_metrics(net1),

    ?assertEqual([], TMetrics2),
    ?assertEqual([], PMetrics2),

    ok = wf_telemetry:stop(),
    cleanup(ok).

export_otel_spans_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{p1 => [a]}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{p2 => [b]}}),

    timer:sleep(10),

    {ok, Spans} = wf_telemetry:export_otel_spans(),
    ?assert(is_list(Spans)),
    ?assert(length(Spans) > 0),

    [Span | _] = Spans,
    ?assert(is_map(Span)),
    ?assert(maps:is_key(span_id, Span)),
    ?assert(maps:is_key(trace_id, Span)),
    ?assert(maps:is_key(name, Span)),
    ?assert(maps:is_key(status, Span)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

export_otel_spans_includes_active_and_completed_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    {ok, S2} = wf_telemetry:span_transition_start(net1, t2, #{}),

    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    timer:sleep(10),

    {ok, Spans} = wf_telemetry:export_otel_spans(),

    ?assert(length(Spans) >= 2),  %% At least S1 (completed) and S2 (active)

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Callback Wrapper Tests
%%%===================================================================

wrap_gen_pnet_callbacks_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, Wrapper} = wf_telemetry:wrap_gen_pnet_callbacks(test_net),

    ?assert(is_map(Wrapper)),
    ?assert(maps:is_key(wrapper_module, Wrapper)),
    ?assert(maps:is_key(fire_wrapper, Wrapper)),
    ?assert(maps:is_key(original_module, Wrapper)),

    ?assertEqual(test_net, maps:get(original_module, Wrapper)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

config_enabled_false_disables_operations_test() ->
    setup(),
    Config = #{enabled => false},
    {ok, _Pid} = wf_telemetry:start_link(Config),

    {ok, SpanId} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ?assertEqual(undefined, SpanId),

    ok = wf_telemetry:stop(),
    cleanup(ok).

config_auto_instrument_test() ->
    setup(),
    Config = #{auto_instrument => true},
    {ok, _Pid} = wf_telemetry:start_link(Config),

    {ok, SpanId} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ?assert(is_reference(SpanId)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Multiple Net Tests
%%%===================================================================

multiple_nets_isolated_metrics_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    {ok, S2} = wf_telemetry:span_transition_start(net2, t2, #{}),
    ok = wf_telemetry:span_transition_end(S2, {produce, #{}}),

    timer:sleep(10),

    {ok, Net1Metrics} = wf_telemetry:get_transition_metrics(net1),
    {ok, Net2Metrics} = wf_telemetry:get_transition_metrics(net2),

    ?assertEqual(1, length(Net1Metrics)),
    ?assertEqual(1, length(Net2Metrics)),

    [Net1Metric] = Net1Metrics,
    [Net2Metric] = Net2Metrics,

    ?assertEqual(t1, maps:get(transition, Net1Metric)),
    ?assertEqual(t2, maps:get(transition, Net2Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

multiple_nets_independent_place_metrics_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    ok = wf_telemetry:instrument_marking(net1, p1, [{a}]),
    ok = wf_telemetry:instrument_marking(net2, p1, [{b}, {c}]),

    timer:sleep(10),

    {ok, Net1PMetrics} = wf_telemetry:get_place_metrics(net1),
    {ok, Net2PMetrics} = wf_telemetry:get_place_metrics(net2),

    ?assertEqual(1, length(Net1PMetrics)),
    ?assertEqual(1, length(Net2PMetrics)),

    [Net1PM] = Net1PMetrics,
    [Net2PM] = Net2PMetrics,

    ?assertEqual(1, maps:get(token_count, Net1PM)),
    ?assertEqual(2, maps:get(token_count, Net2PM)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Span Attributes Tests
%%%===================================================================

span_attributes_in_metrics_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    Mode = #{p1 => [a, b], p2 => [c]},
    {ok, SpanId} = wf_telemetry:span_transition_start(net1, t1, Mode),
    ok = wf_telemetry:span_transition_end(SpanId, {produce, #{}}),

    timer:sleep(10),

    {ok, Spans} = wf_telemetry:export_otel_spans(),
    [Span | _] = Spans,

    Attributes = maps:get(attributes, Span),
    ?assert(is_map(Attributes)),
    ?assert(maps:is_key('net.mod', Attributes)),
    ?assert(maps:is_key('transition.name', Attributes)),

    ok = wf_telemetry:stop(),
    cleanup(ok).

pattern_execution_span_attributes_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    Attrs = #{sequence_id => 42, user => <<"test">>},
    {ok, SpanId} = wf_telemetry:span_pattern_execution(net1, seq_pattern, Attrs),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Duration Calculation Tests
%%%===================================================================

span_duration_recorded_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, SpanId} = wf_telemetry:span_transition_start(net1, t1, #{}),
    timer:sleep(10),
    ok = wf_telemetry:span_transition_end(SpanId, {produce, #{}}),

    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(net1),
    [Metric] = Metrics,

    Duration = maps:get(avg_duration_us, Metric),
    ?assert(Duration >= 10000),  %% At least 10ms in microseconds

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

span_end_non_existent_span_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    InvalidRef = make_ref(),
    ok = wf_telemetry:span_transition_end(InvalidRef, {produce, #{}}),

    ok = wf_telemetry:stop(),
    cleanup(ok).

get_metrics_empty_net_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, TMetrics} = wf_telemetry:get_transition_metrics(unknown_net),
    {ok, PMetrics} = wf_telemetry:get_place_metrics(unknown_net),

    ?assertEqual([], TMetrics),
    ?assertEqual([], PMetrics),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Span Status Tests
%%%===================================================================

span_success_status_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    timer:sleep(10),

    {ok, Spans} = wf_telemetry:export_otel_spans(),
    [Span | _] = Spans,

    Status = maps:get(status, Span),
    StatusCode = maps:get(code, Status),
    ?assertEqual(<<"OK">>, StatusCode),

    ok = wf_telemetry:stop(),
    cleanup(ok).

span_abort_status_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, abort),

    timer:sleep(10),

    {ok, Spans} = wf_telemetry:export_otel_spans(),
    [Span | _] = Spans,

    Status = maps:get(status, Span),
    StatusCode = maps:get(code, Status),
    ?assertEqual(<<"CANCELLED">>, StatusCode),

    ok = wf_telemetry:stop(),
    cleanup(ok).

span_unknown_result_status_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, unknown_result),

    timer:sleep(10),

    {ok, Spans} = wf_telemetry:export_otel_spans(),
    [Span | _] = Spans,

    Status = maps:get(status, Span),
    StatusCode = maps:get(code, Status),
    ?assertEqual(<<"ERROR">>, StatusCode),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Trace Context Tests
%%%===================================================================

trace_context_in_spans_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    {ok, S1} = wf_telemetry:span_transition_start(net1, t1, #{}),
    ok = wf_telemetry:span_transition_end(S1, {produce, #{}}),

    timer:sleep(10),

    {ok, Spans} = wf_telemetry:export_otel_spans(),
    [Span | _] = Spans,

    TraceId = maps:get(trace_id, Span),
    SpanId = maps:get(span_id, Span),

    ?assert(TraceId =/= undefined),
    ?assert(SpanId =/= undefined),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Concurrent Operations Tests
%%%===================================================================

concurrent_span_operations_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    Parent = self(),

    Worker = fun(TransitionId) ->
        {ok, SpanId} = wf_telemetry:span_transition_start(net1, TransitionId, #{}),
        timer:sleep(5),
        ok = wf_telemetry:span_transition_end(SpanId, {produce, #{}}),
        Parent ! {done, TransitionId}
    end,

    spawn(fun() -> Worker(t1) end),
    spawn(fun() -> Worker(t2) end),
    spawn(fun() -> Worker(t3) end),

    receive {done, t1} -> ok end,
    receive {done, t2} -> ok end,
    receive {done, t3} -> ok end,

    timer:sleep(20),

    {ok, Metrics} = wf_telemetry:get_transition_metrics(net1),
    ?assert(length(Metrics) >= 1),

    ok = wf_telemetry:stop(),
    cleanup(ok).

%%%===================================================================
%%% Place High/Low Water Mark Tests
%%%===================================================================

place_water_marks_test() ->
    setup(),
    {ok, _Pid} = wf_telemetry:start_link(),

    ok = wf_telemetry:instrument_marking(net1, p1, [t1]),  %% count = 1
    ok = wf_telemetry:instrument_marking(net1, p1, [t1, t2, t3]),  %% count = 3
    ok = wf_telemetry:instrument_marking(net1, p1, [t1]),  %% count = 1

    timer:sleep(10),

    {ok, Metrics} = wf_telemetry:get_place_metrics(net1),
    [Metric] = Metrics,

    ?assertEqual(3, maps:get(high_water_mark, Metric)),
    ?assertEqual(1, maps:get(low_water_mark, Metric)),

    ok = wf_telemetry:stop(),
    cleanup(ok).
