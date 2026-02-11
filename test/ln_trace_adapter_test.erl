-module(ln_trace_adapter_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Adapter Tests (US-004)
%%%===================================================================

to_telemetry_span_test() ->
    %% Verify conversion to telemetry span format
    Event = #{
        timestamp => 1000,
        type => case_started,
        seq => 0,
        data => #{case_id => "case1"}
    },

    Span = ln_trace_adapter:to_telemetry_span(Event),

    ?assertEqual("case_started", maps:get(name, Span)),
    ?assertEqual(1000, maps:get(start_time, Span)),
    ?assertEqual(1000, maps:get(end_time, Span)),
    ?assertEqual(#{case_id => "case1"}, maps:get(attributes, Span)),
    ?assert(is_list(maps:get(trace_id, Span))),
    ?assert(is_list(maps:get(span_id, Span))),

    ok.

to_yawl_log_test() ->
    %% Verify conversion to yawl_logging format
    Event = #{
        timestamp => 1000,
        type => step_completed,
        seq => 1,
        data => #{case_id => "case1", step => "step1"}
    },

    LogEntry = ln_trace_adapter:to_yawl_log(Event),

    ?assertEqual(<<"1">>, maps:get(id, LogEntry)),
    ?assertEqual(1000, maps:get(timestamp, LogEntry)),
    ?assertEqual(info, maps:get(level, LogEntry)),
    ?assertEqual(step_completed, maps:get(type, LogEntry)),
    ?assertEqual("case1", maps:get(case_id, LogEntry)),
    ?assertEqual(undefined, maps:get(workitem_id, LogEntry)),
    ?assert(is_list(maps:get(message, LogEntry))),

    ok.

to_yawl_log_with_workitem_test() ->
    %% Verify conversion with workitem_id
    Event = #{
        timestamp => 1000,
        type => workitem_completed,
        seq => 2,
        data => #{case_id => "case1", workitem_id => "wi1"}
    },

    LogEntry = ln_trace_adapter:to_yawl_log(Event),

    ?assertEqual("wi1", maps:get(workitem_id, LogEntry)),
    ?assertEqual("case1", maps:get(case_id, LogEntry)),

    ok.

export_to_telemetry_test() ->
    %% Verify export to telemetry gen_server doesn't crash
    Trace0 = ln_trace:new(),
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_completed, Trace1),

    %% Create a mock telemetry server
    MockPid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),

    %% Export should not crash
    Result = ln_trace_adapter:export_to_telemetry(Trace2, MockPid),
    ?assertEqual(ok, Result),

    ok.

export_to_yawl_logging_test() ->
    %% Verify export to yawl_logging gen_server doesn't crash
    Trace0 = ln_trace:new(),
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_completed, Trace1),

    %% Create a mock logging server
    MockPid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),

    %% Export should not crash
    Result = ln_trace_adapter:export_to_yawl_logging(Trace2, MockPid),
    ?assertEqual(ok, Result),

    ok.

export_to_invalid_pid_test() ->
    %% Verify that export doesn't crash when pid is invalid
    Trace0 = ln_trace:new(),
    Trace1 = ln_trace:emit(case_started, Trace0),

    %% Use an invalid PID
    InvalidPid = spawn(fun() -> ok end),

    %% Should not crash even if the process dies
    Result = ln_trace_adapter:export_to_telemetry(Trace1, InvalidPid),
    ?assertEqual(ok, Result),

    ok.

data_preservation_test() ->
    %% Verify that data is preserved through conversion
    Event = #{
        timestamp => 1000,
        type => case_started,
        seq => 0,
        data => #{case_id => "case1", user => "alice", timestamp => 12345}
    },

    %% Convert to telemetry span
    Span = ln_trace_adapter:to_telemetry_span(Event),
    ?assertEqual(#{case_id => "case1", user => "alice", timestamp => 12345},
                 maps:get(attributes, Span)),

    %% Convert to yawl log
    Log = ln_trace_adapter:to_yawl_log(Event),
    ?assertEqual(#{case_id => "case1", user => "alice", timestamp => 12345},
                 maps:get(data, Log)),

    ok.
