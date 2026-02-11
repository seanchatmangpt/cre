-module(ln_trace_replay_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Trace Verification Tests (US-003)
%%%===================================================================

verify_exact_match_test() ->
    %% Verify that two identical traces return ok
    Trace0 = ln_trace:new(),
    Trace1 = ln_trace:emit(case_started, Trace0),
    Trace2 = ln_trace:emit(step_completed, Trace1),
    Trace3 = ln_trace:emit(case_completed, Trace2),

    %% Verify identical trace
    ?assertEqual(ok, ln_trace_replay:verify(Trace3, Trace3)),

    ok.

verify_different_traces_test() ->
    %% Verify that different traces return error with diff
    Trace1 = ln_trace:new(),
    Trace1a = ln_trace:emit(case_started, Trace1),
    Trace1b = ln_trace:emit(step_completed, Trace1a),

    Trace2 = ln_trace:new(),
    Trace2a = ln_trace:emit(case_started, Trace2),
    Trace2b = ln_trace:emit(case_failed, Trace2a),  % Different event

    Result = ln_trace_replay:verify(Trace1b, Trace2b),
    ?assertMatch({error, _Diff}, Result),

    ok.

verify_empty_traces_test() ->
    %% Verify that empty traces match
    Trace1 = ln_trace:new(),
    Trace2 = ln_trace:new(),

    ?assertEqual(ok, ln_trace_replay:verify(Trace1, Trace2)),

    ok.

verify_different_lengths_test() ->
    %% Verify that traces of different lengths return error
    Trace1 = ln_trace:new(),
    Trace1a = ln_trace:emit(case_started, Trace1),

    Trace2 = ln_trace:new(),
    Trace2a = ln_trace:emit(case_started, Trace2),
    Trace2b = ln_trace:emit(step_completed, Trace2a),

    Result = ln_trace_replay:verify(Trace1a, Trace2b),
    ?assertMatch({error, _Diff}, Result),

    ok.

diff_missing_events_test() ->
    %% Verify that diff identifies missing events
    Trace1 = ln_trace:new(),
    Trace1a = ln_trace:emit(case_started, Trace1),
    Trace1b = ln_trace:emit(step_completed, Trace1a),
    Trace1c = ln_trace:emit(case_completed, Trace1b),

    Trace2 = ln_trace:new(),
    Trace2a = ln_trace:emit(case_started, Trace2),
    Trace2b = ln_trace:emit(step_completed, Trace2a),
    %% Trace2 is missing case_completed

    Diff = ln_trace_replay:diff(Trace2b, Trace1c),  % diff(Actual, Expected)
    Missing = maps:get(missing, Diff),

    ?assertEqual(1, length(Missing)),
    ?assertEqual(case_completed, maps:get(type, hd(Missing))),

    ok.

diff_extra_events_test() ->
    %% Verify that diff identifies extra events
    Trace1 = ln_trace:new(),
    Trace1a = ln_trace:emit(case_started, Trace1),
    Trace1b = ln_trace:emit(case_completed, Trace1a),

    Trace2 = ln_trace:new(),
    Trace2a = ln_trace:emit(case_started, Trace2),
    Trace2b = ln_trace:emit(step_completed, Trace2a),
    Trace2c = ln_trace:emit(case_completed, Trace2b),

    Diff = ln_trace_replay:diff(Trace2c, Trace1b),  % diff(Actual, Expected)
    Extra = maps:get(extra, Diff),

    ?assertEqual(1, length(Extra)),
    ?assertEqual(case_completed, maps:get(type, hd(Extra))),  % case_completed at seq 2

    ok.

diff_different_data_test() ->
    %% Verify that diff identifies events with different data
    Trace1 = ln_trace:new(),
    Trace1a = ln_trace:emit(#{timestamp => 1000, type => case_started, data => #{case_id => "case1"}}, Trace1),

    Trace2 = ln_trace:new(),
    Trace2a = ln_trace:emit(#{timestamp => 2000, type => case_started, data => #{case_id => "case2"}}, Trace2),

    Diff = ln_trace_replay:diff(Trace1a, Trace2a),
    Different = maps:get(different, Diff),

    ?assertEqual(1, length(Different)),
    DiffDetail = hd(Different),
    ?assertEqual(0, maps:get(seq, DiffDetail)),

    ok.

diff_complex_traces_test() ->
    %% Verify diff with multiple differences
    Trace1 = ln_trace:new(),
    Trace1a = ln_trace:emit(case_started, Trace1),
    Trace1b = ln_trace:emit(step_started, Trace1a),
    Trace1c = ln_trace:emit(step_completed, Trace1b),
    Trace1d = ln_trace:emit(case_completed, Trace1c),

    Trace2 = ln_trace:new(),
    Trace2a = ln_trace:emit(case_started, Trace2),
    Trace2b = ln_trace:emit(step_failed, Trace2a),  % Different type at seq 1
    Trace2c = ln_trace:emit(case_completed, Trace2b),  % Missing step_started and step_completed

    Diff = ln_trace_replay:diff(Trace2c, Trace1d),  % diff(Actual, Expected)

    Missing = maps:get(missing, Diff),
    ?assertEqual(1, length(Missing)),  % case_completed at seq 3

    Different = maps:get(different, Diff),
    ?assertEqual(2, length(Different)),  % seq 1 and seq 2

    ok.
