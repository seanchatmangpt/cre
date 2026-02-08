%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Chicago TDD Tests for YAWL OpenTelemetry Logger
%%
%% Test-First Development: RED -> GREEN -> REFACTOR
%%
%% @doc YAWL OpenTelemetry Logger Tests
%% @end

-module(yawl_otel_logger_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").
-include("../src/yawl_otel_logger.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = yawl_otel_logger:start_link(#{}),
    Pid.

cleanup(_Pid) ->
    yawl_otel_logger:clear_events(),
    gen_server:stop(yawl_otel_logger),
    ok.

%%====================================================================
%% start_link/0 Tests
%%====================================================================

start_link_default_returns_ok_test() ->
    % Cleanup any existing instance
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        _Pid -> gen_server:stop(yawl_otel_logger, normal, 1000), timer:sleep(100)
    end,
    {ok, Pid} = yawl_otel_logger:start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(yawl_otel_logger).

%%====================================================================
%% start_link/1 Tests
%%====================================================================

start_link_with_custom_max_events_test() ->
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        P -> gen_server:stop(yawl_otel_logger, normal, 1000)
    end,
    {ok, Pid} = yawl_otel_logger:start_link(#{max_events => 100}),
    #{max_events := MaxEvents} = yawl_otel_logger:get_stats(),
    ?assertEqual(100, MaxEvents),
    gen_server:stop(Pid).

start_link_with_custom_retention_test() ->
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        P -> gen_server:stop(yawl_otel_logger, normal, 1000)
    end,
    {ok, Pid} = yawl_otel_logger:start_link(#{retention_ms => 60000}),
    #{retention_ms := Retention} = yawl_otel_logger:get_stats(),
    ?assertEqual(60000, Retention),
    gen_server:stop(Pid).

%%====================================================================
%% log_event/3 Tests
%%====================================================================

log_event_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"test_type">>, <<"Test message">>, #{}),
                    Events = yawl_otel_logger:get_events(),
                    ?assert(length(Events) > 0),
                    [Event | _] = Events,
                    ?assertEqual(<<"test_type">>, Event#otel_event.event_type),
                    ?assertEqual(<<"Test message">>, Event#otel_event.message),
                    ?assertEqual(info, Event#otel_event.level)
                end)
         ]
     end}.

log_event_with_binary_type_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"binary_type">>, <<"Binary test">>, #{}),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(<<"binary_type">>, Event#otel_event.event_type)
                end)
         ]
     end}.

log_event_with_atom_type_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(atom_type, <<"Atom test">>, #{}),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(atom_type, Event#otel_event.event_type)
                end)
         ]
     end}.

log_event_with_attributes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    Attrs = #{key1 => <<"value1">>, key2 => 123},
                    yawl_otel_logger:log_event(<<"test">>, <<"Test">>, Attrs),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(Attrs, Event#otel_event.attributes)
                end)
         ]
     end}.

%%====================================================================
%% log_event/4 Tests
%%====================================================================

log_event_with_debug_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"debug_test">>, <<"Debug">>, #{}, debug),
                    [E1 | _] = yawl_otel_logger:get_events(),
                    ?assertEqual(debug, E1#otel_event.level)
                end)
         ]
     end}.

log_event_with_warning_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"warn_test">>, <<"Warning">>, #{}, warning),
                    [E2 | _] = yawl_otel_logger:get_events(),
                    ?assertEqual(warning, E2#otel_event.level)
                end)
         ]
     end}.

log_event_with_error_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"error_test">>, <<"Error">>, #{}, error),
                    [E3 | _] = yawl_otel_logger:get_events(),
                    ?assertEqual(error, E3#otel_event.level)
                end)
         ]
     end}.

%%====================================================================
%% log_approval/4 Tests
%%====================================================================

log_approval_approve_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_approval(<<"cp-001">>, <<"admin">>, true, #{note => <<"OK">>}),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(checkpoint_approve, Event#otel_event.event_type),
                    ?assertMatch(#{checkpoint_id := <<"cp-001">>, approver := <<"admin">>, approved := true},
                               Event#otel_event.attributes)
                end)
         ]
     end}.

log_approval_deny_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_approval(<<"cp-002">>, <<"manager">>, false, #{}),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(checkpoint_deny, Event#otel_event.event_type),
                    ?assertMatch(#{approved := false}, Event#otel_event.attributes)
                end)
         ]
     end}.

%%====================================================================
%% log_checkpoint/6 Tests
%%====================================================================

log_checkpoint_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_checkpoint(
                      <<"cp-123">>,
                      <<"pattern-1">>,
                      step_approval,
                      <<"supervisor">>,
                      #{context => <<"data">>},
                      #{meta => <<"info">>}
                    ),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(checkpoint_create, Event#otel_event.event_type),
                    ?assertMatch(#{checkpoint_id := <<"cp-123">>, pattern_id := <<"pattern-1">>},
                               Event#otel_event.attributes)
                end)
         ]
     end}.

%%====================================================================
%% log_workflow_start/2 Tests
%%====================================================================

log_workflow_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_workflow_start(<<"case-001">>, <<"workflow-a">>),
                    Events = yawl_otel_logger:get_events(),
                    StartEvents = [E || E <- Events, E#otel_event.event_type =:= workflow_start],
                    ?assert(length(StartEvents) > 0),
                    [StartEvent | _] = StartEvents,
                    ?assertMatch(#{case_id := <<"case-001">>, pattern_id := <<"workflow-a">>},
                               StartEvent#otel_event.attributes)
                end)
         ]
     end}.

log_workflow_start_creates_trace_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_workflow_start(<<"case-trace">>, <<"wf-trace">>),
                    Traces = yawl_otel_logger:get_traces(),
                    ?assert(length(Traces) > 0),
                    [Trace | _] = Traces,
                    ?assertEqual(running, Trace#otel_trace.status),
                    ?assertEqual(<<"case-trace">>, Trace#otel_trace.case_id)
                end)
         ]
     end}.

%%====================================================================
%% log_workflow_complete/2 Tests
%%====================================================================

log_workflow_complete_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_workflow_start(<<"case-complete">>, <<"wf-c">>),
                    yawl_otel_logger:log_workflow_complete(<<"case-complete">>, completed),
                    Events = yawl_otel_logger:get_events(),
                    CompleteEvents = [E || E <- Events, E#otel_event.event_type =:= workflow_complete],
                    ?assert(length(CompleteEvents) > 0),
                    [CompleteEvent | _] = CompleteEvents,
                    ?assertMatch(#{status := completed}, CompleteEvent#otel_event.attributes)
                end)
         ]
     end}.

log_workflow_complete_updates_trace_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_workflow_start(<<"case-update">>, <<"wf-u">>),
                    yawl_otel_logger:log_workflow_complete(<<"case-update">>, completed),
                    Traces = yawl_otel_logger:get_traces(),
                    [Trace | _] = Traces,
                    ?assertEqual(completed, Trace#otel_trace.status),
                    ?assertNotEqual(undefined, Trace#otel_trace.end_time)
                end)
         ]
     end}.

%%====================================================================
%% log_workitem_start/3 Tests
%%====================================================================

log_workitem_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_workitem_start(<<"case-wi">>, <<"task-1">>, <<"My Task">>),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(workitem_start, Event#otel_event.event_type),
                    ?assertMatch(#{task_id := <<"task-1">>, task_name := <<"My Task">>},
                               Event#otel_event.attributes)
                end)
         ]
     end}.

%%====================================================================
%% log_workitem_complete/3 Tests
%%====================================================================

log_workitem_complete_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_workitem_complete(<<"case-wic">>, <<"task-2">>, <<"success">>),
                    Events = yawl_otel_logger:get_events(),
                    [Event | _] = Events,
                    ?assertEqual(workitem_complete, Event#otel_event.event_type),
                    ?assertMatch(#{result := <<"success">>}, Event#otel_event.attributes)
                end)
         ]
     end}.

%%====================================================================
%% get_events/0 Tests
%%====================================================================

get_events_all_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"e1">>, <<"First">>, #{}),
                    timer:sleep(10),
                    yawl_otel_logger:log_event(<<"e2">>, <<"Second">>, #{}),
                    Events = yawl_otel_logger:get_events(),
                    ?assert(length(Events) >= 2),
                    [E1, E2 | _] = Events,
                    ?assert(E1#otel_event.timestamp >= E2#otel_event.timestamp)
                end)
         ]
     end}.

get_events_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    Events = yawl_otel_logger:get_events(),
                    ?assertEqual([], Events)
                end)
         ]
     end}.

%%====================================================================
%% get_events/1 Tests
%%====================================================================

get_events_by_type_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"type_a">>, <<"Msg A">>, #{}),
                    yawl_otel_logger:log_event(<<"type_b">>, <<"Msg B">>, #{}),
                    yawl_otel_logger:log_event(<<"type_a">>, <<"Msg A2">>, #{}),
                    TypeAEvents = yawl_otel_logger:get_events(<<"type_a">>),
                    ?assertEqual(2, length(TypeAEvents)),
                    TypeBEvents = yawl_otel_logger:get_events(<<"type_b">>),
                    ?assertEqual(1, length(TypeBEvents))
                end)
         ]
     end}.

get_events_by_atom_type_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(atom_type, <<"Msg">>, #{}),
                    Events = yawl_otel_logger:get_events(atom_type),
                    ?assertEqual(1, length(Events))
                end)
         ]
     end}.

%%====================================================================
%% get_events_by_trace/1 Tests
%%====================================================================

get_events_by_trace_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(
                      <<"trace_test">>,
                      <<"Msg">>,
                      #{trace_id => <<"trace-123">>}
                    ),
                    yawl_otel_logger:log_event(
                      <<"other_trace">>,
                      <<"Msg2">>,
                      #{trace_id => <<"trace-456">>}
                    ),
                    Trace123Events = yawl_otel_logger:get_events_by_trace(<<"trace-123">>),
                    ?assertEqual(1, length(Trace123Events)),
                    Trace456Events = yawl_otel_logger:get_events_by_trace(<<"trace-456">>),
                    ?assertEqual(1, length(Trace456Events))
                end)
         ]
     end}.

%%====================================================================
%% get_recent_events/2 Tests
%%====================================================================

get_recent_events_limit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    lists:foreach(fun(I) ->
                        yawl_otel_logger:log_event(<<"e">>, list_to_binary(["Msg", integer_to_list(I)]), #{})
                    end, lists:seq(1, 10)),
                    Recent5 = yawl_otel_logger:get_recent_events(5, all),
                    ?assertEqual(5, length(Recent5))
                end)
         ]
     end}.

get_recent_events_level_filter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"e1">>, <<"Msg">>, #{}, debug),
                    yawl_otel_logger:log_event(<<"e2">>, <<"Msg">>, #{}, info),
                    yawl_otel_logger:log_event(<<"e3">>, <<"Msg">>, #{}, error),
                    InfoEvents = yawl_otel_logger:get_recent_events(10, info),
                    ?assertEqual(1, length(InfoEvents)),
                    AllEvents = yawl_otel_logger:get_recent_events(10, all),
                    ?assertEqual(3, length(AllEvents))
                end)
         ]
     end}.

get_recent_events_all_levels_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"e1">>, <<"Msg">>, #{}, warning),
                    yawl_otel_logger:log_event(<<"e2">>, <<"Msg">>, #{}, debug),
                    AllEvents = yawl_otel_logger:get_recent_events(10, all),
                    ?assertEqual(2, length(AllEvents))
                end)
         ]
     end}.

%%====================================================================
%% get_traces/0 Tests
%%====================================================================

get_traces_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_workflow_start(<<"case-1">>, <<"wf-1">>),
                    yawl_otel_logger:log_workflow_complete(<<"case-1">>, completed),
                    Traces = yawl_otel_logger:get_traces(),
                    ?assert(length(Traces) > 0),
                    [Trace | _] = Traces,
                    ?assert(is_record(Trace, otel_trace))
                end)
         ]
     end}.

get_traces_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    Traces = yawl_otel_logger:get_traces(),
                    ?assertEqual([], Traces)
                end)
         ]
     end}.

%%====================================================================
%% clear_events/0 Tests
%%====================================================================

clear_events_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"e1">>, <<"Msg1">>, #{}),
                    yawl_otel_logger:log_event(<<"e2">>, <<"Msg2">>, #{}),
                    ?assertEqual(2, length(yawl_otel_logger:get_events())),
                    ok = yawl_otel_logger:clear_events(),
                    ?assertEqual([], yawl_otel_logger:get_events()),
                    ?assertEqual([], yawl_otel_logger:get_traces())
                end)
         ]
     end}.

%%====================================================================
%% get_stats/0 Tests
%%====================================================================

get_stats_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    Stats = yawl_otel_logger:get_stats(),
                    ?assert(is_map(Stats)),
                    ?assert(maps:is_key(event_count, Stats)),
                    ?assert(maps:is_key(trace_count, Stats)),
                    ?assert(maps:is_key(max_events, Stats)),
                    ?assert(maps:is_key(retention_ms, Stats))
                end)
         ]
     end}.

get_stats_counts_events_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"e1">>, <<"Msg1">>, #{}),
                    yawl_otel_logger:log_event(<<"e2">>, <<"Msg2">>, #{}),
                    Stats = yawl_otel_logger:get_stats(),
                    ?assertEqual(2, maps:get(event_count, Stats))
                end)
         ]
     end}.

%%====================================================================
%% ETS Table Tests
%%====================================================================

ets_table_persistence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"ets_test">>, <<"Persist">>, #{}),
                    % Give the gen_server cast time to complete
                    timer:sleep(10),
                    % Check that the ETS table exists and has our events
                    TableExists = ets:info(yawl_otel_events) =/= undefined,
                    ?assert(TableExists),
                    TableEvents = ets:tab2list(yawl_otel_events),
                    ?assert(length(TableEvents) > 0),
                    [Event | _] = TableEvents,
                    ?assert(is_record(Event, otel_event))
                end)
         ]
     end}.

%%====================================================================
%% Event Cleanup Tests
%%====================================================================

event_cleanup_max_events_test() ->
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        P -> gen_server:stop(yawl_otel_logger, normal, 1000)
    end,
    {ok, Pid} = yawl_otel_logger:start_link(#{max_events => 5}),
    lists:foreach(fun(I) ->
        yawl_otel_logger:log_event(<<"e">>, list_to_binary(["Event", integer_to_list(I)]), #{})
    end, lists:seq(1, 10)),
    Stats = yawl_otel_logger:get_stats(),
    ?assertEqual(5, maps:get(event_count, Stats)),
    gen_server:stop(Pid).

%%====================================================================
%% Trace Lifecycle Tests
%%====================================================================

trace_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    CaseId = <<"case-lifecycle">>,
                    PatternId = <<"wf-lifecycle">>,
                    yawl_otel_logger:log_workflow_start(CaseId, PatternId),
                    [Trace] = yawl_otel_logger:get_traces(),
                    ?assertEqual(running, Trace#otel_trace.status),
                    ?assertEqual(CaseId, Trace#otel_trace.case_id),
                    yawl_otel_logger:log_workflow_complete(CaseId, completed),
                    [UpdatedTrace] = yawl_otel_logger:get_traces(),
                    ?assertEqual(completed, UpdatedTrace#otel_trace.status),
                    ?assertNotEqual(undefined, UpdatedTrace#otel_trace.end_time)
                end)
         ]
     end}.

%%====================================================================
%% Timestamp Tests
%%====================================================================

timestamp_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    Before = erlang:system_time(millisecond),
                    yawl_otel_logger:log_event(<<"ts_test">>, <<"Timestamp">>, #{}),
                    %% Small delay to ensure event is processed before capturing After
                    timer:sleep(5),
                    After = erlang:system_time(millisecond),
                    [Event | _] = yawl_otel_logger:get_events(),
                    ?assert(Before =< Event#otel_event.timestamp),
                    %% Allow 50ms tolerance for async processing / clock resolution
                    ?assert(Event#otel_event.timestamp =< After + 50)
                end)
         ]
     end}.

%%====================================================================
%% ID Generation Tests
%%====================================================================

id_generation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"e1">>, <<"Msg1">>, #{}),
                    yawl_otel_logger:log_event(<<"e2">>, <<"Msg2">>, #{}),
                    Events = yawl_otel_logger:get_events(),
                    [E1, E2 | _] = Events,
                    ?assertNotEqual(E1#otel_event.id, E2#otel_event.id),
                    % Check ID prefix
                    ?assertEqual(<<"event_">>, binary:part(E1#otel_event.id, {0, 6}))
                end)
         ]
     end}.

%%====================================================================
%% Attribute Preservation Tests
%%====================================================================

attribute_preservation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    Attrs = #{
                        user_id => <<"user1">>,
                        case_id => <<"case1">>,
                        task_id => <<"task1">>,
                        pattern_id => <<"pattern1">>,
                        custom_key => <<"custom_value">>
                    },
                    yawl_otel_logger:log_event(<<"attr_test">>, <<"Attr test">>, Attrs),
                    [Event | _] = yawl_otel_logger:get_events(),
                    ?assertEqual(<<"user1">>, Event#otel_event.user_id),
                    ?assertEqual(<<"case1">>, Event#otel_event.case_id),
                    ?assertEqual(<<"task1">>, Event#otel_event.task_id),
                    ?assertEqual(<<"pattern1">>, Event#otel_event.pattern_id)
                end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

undefined_attributes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    yawl_otel_logger:log_event(<<"undef_test">>, <<"Msg">>, #{}),
                    [Event | _] = yawl_otel_logger:get_events(),
                    ?assertEqual(undefined, Event#otel_event.user_id),
                    ?assertEqual(undefined, Event#otel_event.case_id),
                    ?assertEqual(undefined, Event#otel_event.task_id),
                    % span_id is auto-generated for OpenTelemetry compliance
                    ?assertNotEqual(undefined, Event#otel_event.span_id),
                    ?assert(is_binary(Event#otel_event.span_id)),
                    ?assertEqual(undefined, Event#otel_event.parent_span_id)
                end)
         ]
     end}.

%%====================================================================
%% Concurrent Logging Tests
%%====================================================================

concurrent_logging_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    Pids = [spawn(fun() ->
                        yawl_otel_logger:log_event(
                            list_to_binary(["concurrent", integer_to_list(I)]),
                            list_to_binary(["Msg", integer_to_list(I)]),
                            #{}
                        )
                    end) || I <- lists:seq(1, 20)],
                    lists:foreach(fun(Pid) ->
                        Ref = monitor(process, Pid),
                        receive {'DOWN', Ref, process, _, _} -> ok end
                    end, Pids),
                    timer:sleep(100),
                    Events = yawl_otel_logger:get_events(),
                    ?assertEqual(20, length(Events))
                end)
         ]
     end}.
