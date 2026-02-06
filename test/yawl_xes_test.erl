%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Chicago TDD Tests for YAWL XES Logging Module
%%
%% Test-First Development: RED -> GREEN -> REFACTOR
%%
%% @doc YAWL XES Logging Tests
%% @end

-module(yawl_xes_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = yawl_xes:start_link(),
    Pid.

cleanup(_Pid) ->
    yawl_xes:stop(),
    ok.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

start_link_default_test_() ->
    {foreach,
     fun() ->
         ?assertCmd(cleanup),
         ?assertCmd(start_default_works)
     end}.

cleanup() ->
    try yawl_xes:stop() catch _:_ -> ok end,
    timer:sleep(100).

start_default_works() ->
    ?assertCmd(cleanup),
    {ok, Pid} = yawl_xes:start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(yawl_xes).

start_link_named_test_() ->
    ?assertCmd(cleanup),
    {ok, Pid} = yawl_xes:start_link(yawl_xes_test),
    ?assert(is_pid(Pid)),
    gen_server:stop(yawl_xes_test).

stop_test_() ->
    {setup,
     fun setup/0,
     fun() ->
         ok = yawl_xes:stop(),
         ?assertEqual(undefined, whereis(yawl_xes))
     end}.

%%====================================================================
%% Log Creation Tests
%%====================================================================

new_log_default_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Creates new log with default metadata"),
          {ok, LogId} = yawl_xes:new_log(),
          ?assert(is_binary(LogId)),
          ?assert(string:prefix(<<"log_">>, LogId)),

          {ok, Log} = yawl_xes:get_log(LogId),
          ?assertEqual(LogId, Log#xes_log.log_id),
          ?assertNotEqual(undefined, Log#xes_log.trace_id)
         ]
     end}.

new_log_with_metadata_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Creates new log with metadata"),
          Meta = #{version => <<"1.0">>, author => <<"test">>},
          {ok, LogId} = yawl_xes:new_log(Meta),

          {ok, Log} = yawl_xes:get_log(LogId),
          ?assertEqual(Meta, Log#xes_log.metadata)
         ]
     end}.

list_logs_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Lists all logs"),
          {ok, _Log1} = yawl_xes:new_log(#{name => <<"log1">>}),
          {ok, _Log2} = yawl_xes:new_log(#{name => <<"log2">>}),

          Logs = yawl_xes:list_logs(),
          ?assertEqual(2, length(Logs))
         ]
     end}.

%%====================================================================
%% Event Logging Tests
%%====================================================================

log_pattern_start_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs pattern start event"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_pattern_start(LogId, <<"approval">>, <<"pattern-1">>),

          {ok, Log} = yawl_xes:get_log(LogId),
          ?assert(length(Log#xes_log.events) > 0),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(<<"approval">>, maps:get(<<"concept:name">>, Event#xes_event.concept)),
          ?assertEqual(<<"start">>, maps:get(<<"lifecycle:transition">>, Event#xes_event.lifecycle))
         ]
     end}.

log_pattern_complete_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs pattern complete event"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_pattern_complete(LogId, <<"approval">>, <<"pattern-1">>, ok),

          {ok, Log} = yawl_xes:get_log(LogId),
          [_, SecondEvent | _] = Log#xes_log.events,
          ?assertEqual(<<"complete">>, maps:get(<<"lifecycle:transition">>, SecondEvent#xes_event.lifecycle))
         ]
     end}.

log_token_move_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs token move event"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_token_move(LogId, <<"place1">>, <<"from">>, <<"to">>),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(<<"TokenMove">>, maps:get(<<"concept:name">>, Event#xes_event.concept))
         ]
     end}.

log_transition_fire_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs transition fire event"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_transition_fire(LogId, <<"t1">>, <<"in1">>, [<<"out1">>]),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(<<"t1">>, maps:get(<<"concept:name">>, Event#xes_event.concept))
         ]
     end}.

log_case_start_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs case start event"),
          {ok, LogId} = yawl_xes:new_log(),
          CaseId = <<"case-001">>,
          ok = yawl_xes:log_case_start(LogId, CaseId),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(CaseId, Event#xes_event.case_id)
         ]
     end}.

log_case_complete_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs case complete event"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_case_complete(LogId, <<"case-001">>, #{duration => 100}),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(<<"CaseComplete">>, maps:get(<<"concept:name">>, Event#xes_event.concept))
         ]
     end}.

log_workitem_start_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs workitem start event"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_workitem_start(LogId, <<"wi-001">>, <<"task-1">>),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(<<"Workitem">>, maps:get(<<"concept:name">>, Event#xes_event.concept))
         ]
     end}.

log_workitem_complete_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs workitem complete event"),
          {ok, LogId} = yawl:xes:new_log(),
          ok = yawl_xes:log_workitem_complete(LogId, <<"wi-001">>, <<"task-1">>, success),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(success, maps:get(<<"result">>, Event#xes_event.data))
         ]
     end}.

log_event_generic_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs generic event"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_event(LogId, <<"CustomEvent">>, <<"complete">>, #{key => <<"value">>}),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(<<"CustomEvent">>, maps:get(<<"concept:name">>, Event#xes_event.concept))
         ]
     end}.

log_event_with_case_id_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Logs event with case ID"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_event(LogId, <<"E">>, <<"complete">>, #{}, <<"case-123">>),

          {ok, Log} = yawl_xes:get_log(LogId),
          [Event | _] = Log#xes_log.events,
          ?assertEqual(<<"case-123">>, Event#xes_event.case_id)
         ]
     end}.

%%====================================================================
%% Export Tests
%%====================================================================

export_xes_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Exports log to XES format"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_pattern_start(LogId, <<"test">>, <<"p1">>),

          {ok, XESContent} = yawl_xes:export_xes(LogId),
          ?assert(is_binary(XESContent)),
          ?assert(string:str(<<"<?xml">>, XESContent)),
          ?assert(string:str(<<"<log">>, XESContent))
         ]
     end}.

export_xes_to_file_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Exports XES to file"),
          {ok, LogId} = yawl_xes:new_log(),
          ok = yawl_xes:log_pattern_start(LogId, <<"test">>, <<"p1">>),

          {ok, _Content} = yawl_xes:export_xes(LogId, "/tmp/xes_test"),
          ?assert(filelib:is_file("/tmp/xes_test/" ++ binary_to_list(LogId) ++ ".xes")),

          file:delete("/tmp/xes_test/" ++ binary_to_list(LogId) ++ ".xes")
         ]
     end}.

%%====================================================================
%% get_log Tests
%%====================================================================

get_log_existing_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Gets existing log"),
          {ok, LogId} = yawl_xes:new_log(),
          {ok, Log} = yawl_xes:get_log(LogId),
          ?assertEqual(LogId, Log#xes_log.log_id)
         ]
     end}.

get_log_not_found_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Returns error for non-existent log"),
          ?assertEqual({error, not_found}, yawl_xes:get_log(<<"nonexistent">>))
         ]
     end}.

%%====================================================================
%% Helper Function Tests
%%====================================================================

format_timestamp_test_() ->
    [
     ?_test("Formats timestamp correctly"),
     Millis = 1609459200000,  % 2021-01-01 00:00:00 UTC in millis
     Formatted = yawl_xes:format_timestamp(Millis),
     ?assert(is_binary(Formatted)),
     ?assert(string:str(<<"2021-01-01T">>, Formatted))
    ].

format_map_test_() ->
    [
     ?_test("Formats map as XML string"),
     Map = #{<<"key1">> => <<"value1">>, <<"key2">> => 123},
     Result = yawl_xes:format_map(<<"test">>, Map),
     ?assert(is_binary(Result)),
     ?assert(string:str(<<"key1">>, Result)),
     ?assert(string:str(<<"value1">>, Result))
    ].

format_value_test_() ->
    [
     ?_test("Formats various value types"),
     ?assertEqual(<<"bin">>, yawl_xes:format_value(<<"bin">>)),
     ?assertEqual(<<"123">>, yawl_xes:format_value(123)),
     ?assertEqual(<<"atom">>, yawl_xes:format_value(atom)),
     ?assertEqual(<<"[1,2,3]">>, yawl_xes:format_value([1, 2, 3]))
    ].

%%====================================================================
%% Record Type Tests
%%====================================================================

xes_log_record_test_() ->
    Log = #xes_log{
        log_id = <<"log-1">>,
        trace_id = <<"trace-1">>,
        started_at = 1000,
        events = [],
        metadata = #{}
    },
    ?assertEqual(<<"log-1">>, Log#xes_log.log_id),
    ?assertEqual(<<"trace-1">>, Log#xes_log.trace_id).

xes_event_record_test_() ->
    Event = #xes_event{
        event_id = <<"event-1">>,
        timestamp = 2000,
        case_id = <<"case-1">>,
        concept = #{},
        lifecycle = #{},
        data = #{}
    },
    ?assertEqual(<<"event-1">>, Event#xes_event.event_id),
    ?assertEqual(<<"case-1">>, Event#xes_event.case_id).

%%====================================================================
%% Edge Cases Tests
%%====================================================================

log_to_nonexistent_log_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Handles logging to non-existent log gracefully"),
          % Should not crash, just ignore
          ok = yawl_xes:log_pattern_start(<<"nonexistent">>, <<"test">>, <<"p">>),
          ?assertEqual(ok, ok)  % Just ensure no crash
         ]
     end}.

empty_log_export_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Exports empty log"),
          {ok, LogId} = yawl_xes:new_log(),
          {ok, XES} = yawl_xes:export_xes(LogId),
          ?assert(is_binary(XES)),
          ?assert(string:str(<<"<log">>, XES))
         ]
     end}.

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

init_test_() ->
    [
     ?_test("Initializes state"),
     State = #state{},
     {ok, State} = yawl_xes:init([]),
     ?assertEqual(#{}, State#state.logs),
     ?assertEqual(0, State#state.next_event_id)
    ].

handle_call_new_log_test_() ->
    [
     ?_test("Creates new log on call"),
     State = #state{},
     {reply, {ok, LogId}, NewState} = yawl_xes:handle_call({new_log, #{}}, self, State),

     ?assert(is_binary(LogId)),
     ?assertNotEqual(maps:get(LogId, NewState#state.logs), undefined)
    ].

handle_call_get_log_test_() ->
    [
     ?_test("Gets log on call"),
     State = #state{logs = #{<<"log-1">> = #xes_log{log_id = <<"log-1">>}}},
     {reply, {ok, Log}, State} = yawl_xes:handle_call({get_log, <<"log-1">>}, self, State),

     ?assertEqual(<<"log-1">>, Log#xes_log.log_id)
    ].

handle_call_get_log_not_found_test_() ->
    [
     ?_test("Returns error for non-existent log"),
     State = #state{},
     {reply, {error, not_found}, State} = yawl_xes:handle_call({get_log, <<"bad">>}, self, State)
    ].

handle_cast_add_event_test_() ->
    [
     ?_test("Adds event on cast"),
     LogId = <<"log-cast">>,
     Event = #xes_event{
         event_id = <<"e1">>,
         timestamp = 1000,
         case_id = undefined,
         concept = #{},
         lifecycle = #{},
         data = #{}
     },
     State0 = #state{logs = #{LogId => #xes_log{log_id = LogId, events = []}}},
     {noreply, State1} = yawl_xes:handle_cast({add_event, LogId, Event}, State0),

     ?assertEqual(1, length(maps:get(LogId, State1#state.logs#xes_log.events)))
    ].

handle_cast_add_event_unknown_log_test_() ->
    [
     ?_test("Handles cast to unknown log gracefully"),
     Event = #xes_event{event_id = <<"e1">>, timestamp = 1, case_id = undefined, concept = #{}, lifecycle = #{}, data = #{}}},
     State0 = #state{},
     {noreply, StateFinal} = yawl_xes:handle_cast({add_event, <<"unknown">>, Event}, State0),

     ?assertEqual(State0, StateFinal)
    ].
