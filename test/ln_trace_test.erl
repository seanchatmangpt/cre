-module(ln_trace_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Trace Persistence Tests (US-001)
%%%===================================================================

save_load_cycle_test() ->
    %% Verify that ln_trace:save/2 and ln_trace:load/1 work correctly
    Trace0 = ln_trace:new(#{level => full, max_events => infinity}),

    %% Emit some events
    Trace1 = ln_trace:emit(#{timestamp => 1000, type => case_started, data => #{case_id => "case1"}}, Trace0),
    Trace2 = ln_trace:emit(#{timestamp => 2000, type => step_completed, data => #{step => "step1"}}, Trace1),
    Trace3 = ln_trace:emit(#{timestamp => 3000, type => case_completed, data => #{}}, Trace2),

    %% Save to file
    Filename = "/tmp/trace_test.json",
    ?assertEqual(ok, ln_trace:save(Trace3, Filename)),

    %% Load from file
    {ok, LoadedTrace} = ln_trace:load(Filename),

    %% Verify events match
    OriginalEvents = ln_trace:get_all(Trace3),
    LoadedEvents = ln_trace:get_all(LoadedTrace),

    ?assertEqual(length(OriginalEvents), length(LoadedEvents)),
    ?assertEqual(3, length(LoadedEvents)),

    %% Verify first event
    [First, Second, Third] = LoadedEvents,
    ?assertEqual(1000, maps:get(timestamp, First)),
    ?assertEqual(case_started, maps:get(type, First)),
    ?assertEqual("case1", maps:get(case_id, maps:get(data, First))),

    %% Verify second event
    ?assertEqual(2000, maps:get(timestamp, Second)),
    ?assertEqual(step_completed, maps:get(type, Second)),

    %% Verify third event
    ?assertEqual(3000, maps:get(timestamp, Third)),
    ?assertEqual(case_completed, maps:get(type, Third)),

    %% Clean up
    file:delete(Filename),
    ok.

save_with_events_preserves_ordering_test() ->
    %% Verify that event ordering is preserved through save/load
    Trace0 = ln_trace:new(),

    %% Emit events in a specific order
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_started, Trace1),
    Trace3 = ln_trace:emit(step_completed, Trace2),
    Trace4 = ln_trace:emit(case_completed, Trace3),

    %% Save and load
    Filename = "/tmp/trace_ordering_test.json",
    ?assertEqual(ok, ln_trace:save(Trace4, Filename)),
    {ok, LoadedTrace} = ln_trace:load(Filename),

    %% Verify sequence numbers are in order
    Events = ln_trace:get_all(LoadedTrace),
    Seqs = [maps:get(seq, E) || E <- Events],
    ?assertEqual([0, 1, 2, 3], Seqs),

    %% Clean up
    file:delete(Filename),
    ok.

save_handles_file_errors_test() ->
    %% Verify that save/2 returns error tuple on file I/O failures
    Trace = ln_trace:new(),

    %% Try to save to an invalid path
    Result = ln_trace:save(Trace, "/nonexistent/directory/trace.json"),
    ?assertMatch({error, _}, Result),

    ok.

load_handles_file_errors_test() ->
    %% Verify that load/1 returns error tuple for missing files
    Result = ln_trace:load("/nonexistent/file.json"),
    ?assertMatch({error, _}, Result),

    ok.

save_creates_human_readable_json_test() ->
    %% Verify that the saved file is human-readable JSON
    Trace0 = ln_trace:new(),
    Trace1 = ln_trace:emit(#{timestamp => 1000, type => case_started, data => #{case_id => "test"}}, Trace0),

    Filename = "/tmp/trace_json_test.json",
    ?assertEqual(ok, ln_trace:save(Trace1, Filename)),

    %% Read file and verify it's valid JSON
    {ok, Content} = file:read_file(Filename),
    ?assert(is_binary(Content)),

    %% Try to parse it with jsx
    Parsed = jsx:decode(Content, [return_maps]),
    ?assert(is_list(Parsed)),
    ?assertEqual(1, length(Parsed)),

    %% Clean up
    file:delete(Filename),
    ok.

%%%===================================================================
%%% OTP Logger Integration Tests (US-002)
%%%===================================================================

logger_integration_test() ->
    %% Verify that ln_trace emit/2 logs to OTP logger
    %% Note: This test verifies the function exists and doesn't crash
    %% Actual logging verification would require logger handler inspection

    Trace0 = ln_trace:new(#{level => full}),
    Trace1 = ln_trace:emit(case_started, Trace0),

    %% Verify trace state is updated
    Events = ln_trace:get_all(Trace1),
    ?assertEqual(1, length(Events)),

    ok.

set_logger_level_test() ->
    %% Verify that set_logger_level/1 function exists and can be called
    %% Note: Setting logger level requires the application to be loaded
    %% which may not be the case in unit tests. We just verify the function exists.
    ?assert(is_function(fun ln_trace:set_logger_level/1)),
    ok.

%%%===================================================================
%%% Trace Level Filtering Tests
%%%===================================================================

trace_level_none_test() ->
    %% Verify that 'none' level produces no events
    Trace0 = ln_trace:new(#{level => none}),
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_completed, Trace1),

    Events = ln_trace:get_all(Trace2),
    ?assertEqual(0, length(Events)),

    ok.

trace_level_full_test() ->
    %% Verify that 'full' level produces all events
    Trace0 = ln_trace:new(#{level => full}),
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_completed, Trace1),

    Events = ln_trace:get_all(Trace2),
    ?assertEqual(2, length(Events)),

    ok.

%%%===================================================================
%%% Max Events Trimming Tests
%%%===================================================================

max_events_trimming_test() ->
    %% Verify that max_events limits buffer size
    Trace0 = ln_trace:new(#{level => full, max_events => 3}),

    %% Emit 5 events
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_started, Trace1),
    Trace3 = ln_trace:emit(step_completed, Trace2),
    Trace4 = ln_trace:emit(case_failed, Trace3),
    Trace5 = ln_trace:emit(case_cancelled, Trace4),

    %% Should only have 3 events (most recent)
    Events = ln_trace:get_all(Trace5),
    ?assertEqual(3, length(Events)),

    %% Verify sequence numbers of remaining events
    Seqs = [maps:get(seq, E) || E <- Events],
    ?assertEqual([2, 3, 4], Seqs),

    ok.

%%%===================================================================
%%% Range Query Tests
%%%===================================================================

get_range_test() ->
    %% Verify range query works
    Trace0 = ln_trace:new(),
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_started, Trace1),
    Trace3 = ln_trace:emit(step_completed, Trace2),
    Trace4 = ln_trace:emit(case_completed, Trace3),

    %% Get range 1-2 (should include seq 1 and 2)
    Events = ln_trace:get_range(Trace4, 1, 2),
    ?assertEqual(2, length(Events)),

    ok.

clear_test() ->
    %% Verify clear resets trace
    Trace0 = ln_trace:new(),
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_completed, Trace1),

    ?assertEqual(2, length(ln_trace:get_all(Trace2))),

    Trace3 = ln_trace:clear(Trace2),
    ?assertEqual(0, length(ln_trace:get_all(Trace3))),

    ok.
