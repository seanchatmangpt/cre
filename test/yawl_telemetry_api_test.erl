-module(yawl_telemetry_api_test).
-author('claude@anthropic.com').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

telemetry_setup() ->
    {ok, Pid} = yawl_telemetry:start_link(),
    Pid.

telemetry_cleanup(_Pid) ->
    gen_server:stop(yawl_telemetry).

%%====================================================================
%% Test Generators
%%====================================================================

yawl_telemetry_test_() ->
    {foreach,
     fun telemetry_setup/0,
     fun telemetry_cleanup/1,
     [
         fun test_start_span_basic/0,
         fun test_start_span_with_task_id/0,
         fun test_end_span_success/0,
         fun test_end_span_failure/0,
         fun test_emit_event_task_start/0,
         fun test_emit_event_task_complete/0,
         fun test_emit_event_task_fail/0,
         fun test_emit_event_workflow_complete/0,
         fun test_track_task_success/0,
         fun test_track_task_failure/0,
         fun test_track_task_cancelled/0,
         fun test_get_metrics_not_found/0,
         fun test_get_metrics_after_tracking/0,
         fun test_verbosity_levels/0,
         fun test_flush/0,
         fun test_get_span_context/0,
         fun test_multiple_concurrent_spans/0,
         fun test_metrics_aggregation/0,
         fun test_graceful_degradation/0
     ]
    }.

%%====================================================================
%% Individual Test Cases
%%====================================================================

test_start_span_basic() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    TaskId = undefined,
    SpanName = <<"Test Span">>,

    ?assertMatch(
        {ok, SpanCtx} when is_map(SpanCtx),
        yawl_telemetry:start_span(SpecId, CaseId, TaskId, SpanName)
    ),

    {ok, SpanCtx} = yawl_telemetry:start_span(SpecId, CaseId, TaskId, SpanName),
    ?assertEqual(SpecId, maps:get(spec_id, SpanCtx)),
    ?assertEqual(CaseId, maps:get(case_id, SpanCtx)),
    ?assertEqual(SpanName, maps:get(name, SpanCtx)),
    ?assert(is_binary(maps:get(id, SpanCtx))),
    ?assert(is_integer(maps:get(start_time, SpanCtx))).

test_start_span_with_task_id() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    TaskId = <<"test_task">>,
    SpanName = <<"Execute Task">>,

    {ok, SpanCtx} = yawl_telemetry:start_span(SpecId, CaseId, TaskId, SpanName),
    ?assertEqual(TaskId, maps:get(task_id, SpanCtx)),

    %% Check attributes
    Attrs = maps:get(attributes, SpanCtx),
    ?assertEqual(TaskId, maps:get(task_id, Attrs)).

test_end_span_success() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    TaskId = <<"test_task">>,
    SpanName = <<"Test Span">>,

    {ok, SpanCtx} = yawl_telemetry:start_span(SpecId, CaseId, TaskId, SpanName),
    ?assertEqual(ok, yawl_telemetry:end_span(SpanCtx, success)).

test_end_span_failure() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    TaskId = <<"test_task">>,
    SpanName = <<"Test Span">>,

    {ok, SpanCtx} = yawl_telemetry:start_span(SpecId, CaseId, TaskId, SpanName),
    ?assertEqual(ok, yawl_telemetry:end_span(SpanCtx, failure)).

test_emit_event_task_start() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    EventType = task_start,
    Attributes = #{task_id => <<"task1">>, worker_id => <<"worker-1">>},

    ?assertEqual(
        ok,
        yawl_telemetry:emit_event(SpecId, CaseId, EventType, Attributes)
    ).

test_emit_event_task_complete() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    EventType = task_complete,
    Attributes = #{task_id => <<"task1">>, duration_ms => 100},

    ?assertEqual(
        ok,
        yawl_telemetry:emit_event(SpecId, CaseId, EventType, Attributes)
    ).

test_emit_event_task_fail() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    EventType = task_fail,
    Attributes = #{task_id => <<"task1">>, error => <<"timeout">>},

    ?assertEqual(
        ok,
        yawl_telemetry:emit_event(SpecId, CaseId, EventType, Attributes)
    ).

test_emit_event_workflow_complete() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    EventType = workflow_complete,
    Attributes = #{duration_ms => 1000},

    ?assertEqual(
        ok,
        yawl_telemetry:emit_event(SpecId, CaseId, EventType, Attributes)
    ).

test_track_task_success() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    TaskId = <<"task1">>,

    ?assertEqual(
        ok,
        yawl_telemetry:track_task(SpecId, CaseId, TaskId, success)
    ).

test_track_task_failure() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    TaskId = <<"task1">>,

    ?assertEqual(
        ok,
        yawl_telemetry:track_task(SpecId, CaseId, TaskId, failure)
    ).

test_track_task_cancelled() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    TaskId = <<"task1">>,

    ?assertEqual(
        ok,
        yawl_telemetry:track_task(SpecId, CaseId, TaskId, cancelled)
    ).

test_get_metrics_not_found() ->
    SpecId = <<"nonexistent_spec">>,
    CaseId = <<"nonexistent_case">>,

    ?assertEqual(
        {error, not_found},
        yawl_telemetry:get_metrics(SpecId, CaseId)
    ).

test_get_metrics_after_tracking() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,

    %% Track some tasks
    yawl_telemetry:track_task(SpecId, CaseId, <<"task1">>, success),
    yawl_telemetry:track_task(SpecId, CaseId, <<"task2">>, success),
    yawl_telemetry:track_task(SpecId, CaseId, <<"task3">>, failure),
    yawl_telemetry:track_task(SpecId, CaseId, <<"task4">>, cancelled),

    {ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId),

    ?assertEqual(4, maps:get(total_tasks, Metrics)),
    ?assertEqual(2, maps:get(completed_tasks, Metrics)),
    ?assertEqual(1, maps:get(failed_tasks, Metrics)),
    ?assertEqual(1, maps:get(cancelled_tasks, Metrics)),
    ?assertEqual(SpecId, maps:get(spec_id, Metrics)),
    ?assertEqual(CaseId, maps:get(case_id, Metrics)).

test_verbosity_levels() ->
    ?assertEqual(ok, yawl_telemetry:set_verbosity(silent)),
    ?assertEqual(ok, yawl_telemetry:set_verbosity(minimal)),
    ?assertEqual(ok, yawl_telemetry:set_verbosity(normal)),
    ?assertEqual(ok, yawl_telemetry:set_verbosity(verbose)),
    ?assertEqual(ok, yawl_telemetry:set_verbosity(debug)).

test_flush() ->
    ?assertEqual(ok, yawl_telemetry:flush()).

test_get_span_context() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    SpanName = <<"Test Span">>,

    {ok, SpanCtx} = yawl_telemetry:start_span(SpecId, CaseId, undefined, SpanName),
    SpanId = yawl_telemetry:get_span_context(SpanCtx),
    ?assert(is_binary(SpanId)),
    ?assertEqual(maps:get(id, SpanCtx), SpanId),

    %% Test with empty map
    ?assertEqual(<<>>, yawl_telemetry:get_span_context(#{})).

test_multiple_concurrent_spans() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,

    %% Start multiple spans
    {ok, Span1} = yawl_telemetry:start_span(SpecId, CaseId, <<"task1">>, <<"Task 1">>),
    {ok, Span2} = yawl_telemetry:start_span(SpecId, CaseId, <<"task2">>, <<"Task 2">>),
    {ok, Span3} = yawl_telemetry:start_span(SpecId, CaseId, <<"task3">>, <<"Task 3">>),

    %% Verify they have unique IDs
    Id1 = maps:get(id, Span1),
    Id2 = maps:get(id, Span2),
    Id3 = maps:get(id, Span3),

    ?assertNotEqual(Id1, Id2),
    ?assertNotEqual(Id2, Id3),
    ?assertNotEqual(Id1, Id3),

    %% End all spans
    ?assertEqual(ok, yawl_telemetry:end_span(Span1, success)),
    ?assertEqual(ok, yawl_telemetry:end_span(Span2, success)),
    ?assertEqual(ok, yawl_telemetry:end_span(Span3, failure)).

test_metrics_aggregation() ->
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,

    %% Track many tasks
    lists:foreach(
        fun(I) ->
            TaskId = list_to_binary(["task", integer_to_list(I)]),
            Status = case I rem 3 of
                0 -> success;
                1 -> failure;
                2 -> cancelled
            end,
            yawl_telemetry:track_task(SpecId, CaseId, TaskId, Status)
        end,
        lists:seq(1, 100)
    ),

    {ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId),

    ?assertEqual(100, maps:get(total_tasks, Metrics)),
    ?assertEqual(34, maps:get(completed_tasks, Metrics)),
    ?assertEqual(33, maps:get(failed_tasks, Metrics)),
    ?assertEqual(33, maps:get(cancelled_tasks, Metrics)).

test_graceful_degradation() ->
    %% Test that the system works even without OpenTelemetry
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,

    %% These should not fail even if OTEL is not available
    {ok, _Span} = yawl_telemetry:start_span(SpecId, CaseId, <<"task1">>, <<"Task">>),
    ?assertEqual(ok, yawl_telemetry:emit_event(SpecId, CaseId, task_start, #{})),
    ?assertEqual(ok, yawl_telemetry:track_task(SpecId, CaseId, <<"task1">>, success)).

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_span_id_unique_test() ->
    %% Generate multiple span IDs and verify uniqueness
    {ok, Pid} = yawl_telemetry:start_link(),

    SpanIds = [begin
        {ok, Span} = yawl_telemetry:start_span(
            <<"spec">>, <<"case">>, undefined, <<"Span">>
        ),
        maps:get(id, Span)
    end || _ <- lists:seq(1, 100)],

    %% All IDs should be unique
    UniqueIds = lists:usort(SpanIds),
    ?assertEqual(100, length(UniqueIds)),

    gen_server:stop(Pid).

prop_metrics_consistency_test() ->
    %% Verify metrics remain consistent across multiple operations
    {ok, Pid} = yawl_telemetry:start_link(),

    SpecId = <<"spec">>,
    CaseId = <<"case">>,

    %% Track tasks in various states
    Tasks = [
        {<<"t1">>, success},
        {<<"t2">>, success},
        {<<"t3">>, failure},
        {<<"t4">>, cancelled},
        {<<"t5">>, success}
    ],

    lists:foreach(
        fun({TaskId, Status}) ->
            yawl_telemetry:track_task(SpecId, CaseId, TaskId, Status)
        end,
        Tasks
    ),

    {ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId),

    %% Verify totals
    Total = maps:get(total_tasks, Metrics),
    Completed = maps:get(completed_tasks, Metrics),
    Failed = maps:get(failed_tasks, Metrics),
    Cancelled = maps:get(cancelled_tasks, Metrics),

    ?assertEqual(5, Total),
    ?assertEqual(3, Completed),
    ?assertEqual(1, Failed),
    ?assertEqual(1, Cancelled),
    ?assertEqual(Total, Completed + Failed + Cancelled),

    gen_server:stop(Pid).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_workflow_lifecycle_test() ->
    %% Tests complete workflow lifecycle with telemetry
    {ok, Pid} = yawl_telemetry:start_link(),

    SpecId = <<"order_fulfillment">>,
    CaseId = <<"order-123">>,

    %% Start workflow
    yawl_telemetry:emit_event(SpecId, CaseId, workflow_start, #{}),

    %% Track multiple tasks
    Tasks = [
        {<<"validate">>, success, 50},
        {<<"payment">>, success, 200},
        {<<"inventory">>, failure, 100},
        {<<"shipping">>, cancelled, 0}
    ],

    lists:foreach(
        fun({TaskId, Status, _Duration}) ->
            {ok, Span} = yawl_telemetry:start_span(
                SpecId, CaseId, TaskId, <<"Execute ", TaskId/binary>>
            ),
            timer:sleep(1), %% Simulate work
            yawl_telemetry:end_span(Span, Status),
            yawl_telemetry:track_task(SpecId, CaseId, TaskId, Status)
        end,
        Tasks
    ),

    %% Check metrics
    {ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId),
    ?assertEqual(4, maps:get(total_tasks, Metrics)),
    ?assertEqual(2, maps:get(completed_tasks, Metrics)),
    ?assertEqual(1, maps:get(failed_tasks, Metrics)),
    ?assertEqual(1, maps:get(cancelled_tasks, Metrics)),

    gen_server:stop(Pid).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_span_creation_test() ->
    %% Test span creation performance
    {ok, Pid} = yawl_telemetry:start_link(),

    {Time, _Spans} = timer:tc(fun() ->
        [begin
            {ok, Span} = yawl_telemetry:start_span(
                <<"spec">>, <<"case">>, <<"task">>, <<"Task">>
            ),
            yawl_telemetry:end_span(Span, success)
        end || _ <- lists:seq(1, 1000)]
    end),

    %% Should complete 1000 spans in reasonable time (< 1 second)
    ?assert(Time < 1_000_000),

    gen_server:stop(Pid).

performance_metrics_tracking_test() ->
    %% Test metrics tracking performance
    {ok, Pid} = yawl_telemetry:start_link(),

    {Time, _} = timer:tc(fun() ->
        [begin
            yawl_telemetry:track_task(
                <<"spec">>,
                <<"case">>,
                list_to_binary(["task", integer_to_list(I)]),
                success
            )
        end || I <- lists:seq(1, 1000)]
    end),

    %% Should complete 1000 track operations in reasonable time
    ?assert(Time < 500_000),

    gen_server:stop(Pid).
