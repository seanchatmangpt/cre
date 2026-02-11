%%% @doc WF Trace Test Suite
%%%
%%% Comprehensive tests for wf_trace module covering:
%%% - Trace event creation and structure
%%% - Trace log operations
%%% - Event filtering and querying
%%% - Deterministic replay
%%% - Trace comparison and diff
%%% - Serialization and persistence
%%% - Statistics and analysis
%%%
%%% @end
-module(wf_trace_test).

-include_lib("eunit/include/eunit.hrl").

%%% BASIC EVENT CREATION ====================================================

event_creation_test() ->
    Ctx = #{data => #{x => 1}, signals => [], results => #{}, token_data => #{}},
    Event = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    ?assertMatch({0, task_enter, task_enter, _, _, [], false}, Event).

event_with_timestamp_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    Ts = 123456789,
    Event = wf_trace:event(0, task_ok, task_call, Ctx, Ts, [{seq, 1}], false),
    ?assertMatch({0, task_ok, task_call, _, 123456789, [{seq, 1}], false}, Event).

%%% TRACE LOG OPERATIONS ====================================================

new_log_test() ->
    Log = wf_trace:new_log(),
    ?assertEqual([], Log),
    ?assertEqual(0, wf_trace:log_size(Log)).

append_event_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    Log = wf_trace:new_log(),
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, [], false),

    Log1 = wf_trace:append_event(Log, E1),
    ?assertEqual(1, wf_trace:log_size(Log1)),

    Log2 = wf_trace:append_event(Log1, E2),
    ?assertEqual(2, wf_trace:log_size(Log2)),
    ?assertEqual([E1, E2], wf_trace:get_events(Log2)).

%%% EVENT FILTERING =========================================================

filter_by_type_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, [], false),
    E3 = wf_trace:event(2, task_exit, task_exit, Ctx, [], false),
    E4 = wf_trace:event(3, task_enter, task_enter, Ctx, [], false),
    Log = [E1, E2, E3, E4],

    Enters = wf_trace:filter_by_type(Log, task_enter),
    ?assertEqual(2, length(Enters)),
    ?assertEqual([E1, E4], Enters),

    Exits = wf_trace:filter_by_type(Log, task_exit),
    ?assertEqual(1, length(Exits)),
    ?assertEqual([E3], Exits).

filter_by_opcode_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, [], false),
    E3 = wf_trace:event(2, task_exit, task_exit, Ctx, [], false),
    Log = [E1, E2, E3],

    Calls = wf_trace:filter_by_opcode(Log, task_call),
    ?assertEqual(1, length(Calls)),
    ?assertEqual([E2], Calls).

filter_by_scope_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [{seq, 1}], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, [{seq, 1}], false),
    E3 = wf_trace:event(2, par_fork, par_fork, Ctx, [{par, 2}], false),
    Log = [E1, E2, E3],

    SeqEvents = wf_trace:filter_by_scope(Log, {seq, 1}),
    ?assertEqual(2, length(SeqEvents)),
    ?assertEqual([E1, E2], SeqEvents).

get_last_event_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    EmptyLog = wf_trace:new_log(),
    ?assertEqual({error, empty}, wf_trace:get_last_event(EmptyLog)),

    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, [], false),
    Log = [E1, E2],

    ?assertEqual({ok, E2}, wf_trace:get_last_event(Log)).

get_event_at_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, [], false),
    E3 = wf_trace:event(2, task_exit, task_exit, Ctx, [], false),
    Log = [E1, E2, E3],

    ?assertEqual({ok, E1}, wf_trace:get_event_at(Log, 0)),
    ?assertEqual({ok, E2}, wf_trace:get_event_at(Log, 1)),
    ?assertEqual({ok, E3}, wf_trace:get_event_at(Log, 2)),
    ?assertEqual({error, not_found}, wf_trace:get_event_at(Log, 99)).

%%% TRACE COMPARISON ========================================================

traces_match_identical_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [], false),
    Log1 = [E1, E2],
    Log2 = [E1, E2],

    ?assert(wf_trace:traces_match(Log1, Log2)).

traces_match_different_length_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [], false),
    Log1 = [E1],
    Log2 = [E1, E2],

    ?assertNot(wf_trace:traces_match(Log1, Log2)).

traces_match_different_type_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2a = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [], false),
    E2b = wf_trace:event(1, task_error, task_call, Ctx, 2000, [], false),
    Log1 = [E1, E2a],
    Log2 = [E1, E2b],

    ?assertNot(wf_trace:traces_match(Log1, Log2)).

compare_traces_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [], false),
    Log1 = [E1, E2],
    Log2 = [E1, E2],

    Diff = wf_trace:compare_traces(Log1, Log2),
    ?assertEqual(2, maps:get(matching, Diff)),
    ?assertEqual([], maps:get(mismatched, Diff)),
    ?assertEqual({2, 2}, maps:get(length_diff, Diff)).

diff_traces_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2a = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [], false),
    E2b = wf_trace:event(1, task_error, task_call, Ctx, 2000, [], false),
    Log1 = [E1, E2a],
    Log2 = [E1, E2b],

    Diff = wf_trace:diff_traces(Log1, Log2),
    ?assertEqual(1, maps:get(matching, Diff)),
    Mismatches = maps:get(mismatches, Diff),
    ?assert(length(Mismatches) > 0),
    ?assertEqual(2, maps:get(expected_length, Diff)),
    ?assertEqual(2, maps:get(actual_length, Diff)).

%%% SERIALIZATION ===========================================================

to_list_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    Log = [E1],

    List = wf_trace:to_list(Log),
    ?assertEqual(Log, List).

from_list_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, [], false),
    List = [E1],

    Log = wf_trace:from_list(List),
    ?assertEqual(List, Log).

binary_serialization_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [], false),
    Log = [E1, E2],

    Bin = wf_trace:to_binary(Log),
    ?assert(is_binary(Bin)),

    {ok, Restored} = wf_trace:from_binary(Bin),
    ?assertEqual(Log, Restored).

binary_deserialization_invalid_test() ->
    InvalidBin = <<"garbage data">>,
    Result = wf_trace:from_binary(InvalidBin),
    ?assertMatch({error, {deserialization_failed, _}}, Result).

%%% STATISTICS AND ANALYSIS =================================================

trace_stats_empty_test() ->
    Log = wf_trace:new_log(),
    Stats = wf_trace:trace_stats(Log),

    ?assertEqual(0, maps:get(total_events, Stats)),
    ?assertEqual(#{}, maps:get(by_type, Stats)),
    ?assertEqual(#{}, maps:get(by_opcode, Stats)),
    ?assertEqual(0, maps:get(max_scope_depth, Stats)),
    ?assertEqual(0, maps:get(execution_time_us, Stats)),
    ?assertEqual(0, maps:get(cancelled_steps, Stats)).

trace_stats_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [{seq, 1}], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [{seq, 1}], false),
    E3 = wf_trace:event(2, task_exit, task_exit, Ctx, 3000, [{seq, 1}], true),
    E4 = wf_trace:event(3, task_enter, task_enter, Ctx, 4000, [{seq, 1}, {par, 2}], false),
    Log = [E1, E2, E3, E4],

    Stats = wf_trace:trace_stats(Log),

    ?assertEqual(4, maps:get(total_events, Stats)),
    ?assertEqual(1, maps:get(cancelled_steps, Stats)),
    ?assertEqual(2, maps:get(max_scope_depth, Stats)),
    ?assertEqual(3000, maps:get(execution_time_us, Stats)),

    ByType = maps:get(by_type, Stats),
    ?assertEqual(2, maps:get(task_enter, ByType)),
    ?assertEqual(1, maps:get(task_ok, ByType)),
    ?assertEqual(1, maps:get(task_exit, ByType)),

    ByOpcode = maps:get(by_opcode, Stats),
    ?assertEqual(2, maps:get(task_enter, ByOpcode)),
    ?assertEqual(1, maps:get(task_call, ByOpcode)),
    ?assertEqual(1, maps:get(task_exit, ByOpcode)).

execution_timeline_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = wf_trace:event(1, task_ok, task_call, Ctx, 2000, [], false),
    E3 = wf_trace:event(2, task_exit, task_exit, Ctx, 3000, [], false),
    Log = [E1, E2, E3],

    Timeline = wf_trace:execution_timeline(Log),

    ?assertEqual(3, length(Timeline)),
    ?assertEqual([
        {1000, task_enter, task_enter},
        {2000, task_ok, task_call},
        {3000, task_exit, task_exit}
    ], Timeline).

scope_depth_analysis_test() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = wf_trace:event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = wf_trace:event(1, seq_enter, seq_enter, Ctx, 2000, [{seq, 1}], false),
    E3 = wf_trace:event(2, par_fork, par_fork, Ctx, 3000, [{seq, 1}, {par, 2}], false),
    Log = [E1, E2, E3],

    DepthAnalysis = wf_trace:scope_depth_analysis(Log),

    ?assertEqual([
        {0, 0},
        {1, 1},
        {2, 2}
    ], DepthAnalysis).

%%% INTEGRATION TESTS WITH WF_EXEC ==========================================

simple_task_trace_test() ->
    %% Create a simple task pattern
    TaskFun = fun(Ctx) -> {ok, Ctx} end,
    Pattern = wf_term:task(simple_task, TaskFun),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),

    %% Execute until halt
    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),

    %% Extract trace
    Trace = wf_vm:exec_trace(FinalState),

    %% Verify trace contains expected events
    ?assert(wf_trace:log_size(Trace) >= 3),  % At least: task_enter, task_ok, task_exit

    %% Check for task_enter event
    TaskEnters = wf_trace:filter_by_type(Trace, task_enter),
    ?assertEqual(1, length(TaskEnters)).

sequence_trace_test() ->
    %% Create a sequence pattern
    TaskFun = fun(Ctx) -> {ok, Ctx} end,
    Task1 = wf_term:task(task_a, TaskFun),
    Task2 = wf_term:task(task_b, TaskFun),
    Pattern = wf_term:seq(Task1, Task2),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),

    %% Execute until halt
    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),

    %% Extract and analyze trace
    Trace = wf_vm:exec_trace(FinalState),
    Stats = wf_trace:trace_stats(Trace),

    %% Should have events from both tasks
    ?assert(maps:get(total_events, Stats) >= 6),  % 2 tasks × 3 events each

    %% Check event sequence
    TaskEnters = wf_trace:filter_by_type(Trace, task_enter),
    ?assertEqual(2, length(TaskEnters)).

parallel_trace_test() ->
    %% Create a parallel pattern
    TaskFun = fun(Ctx) -> {ok, Ctx} end,
    Task1 = wf_term:task(task_a, TaskFun),
    Task2 = wf_term:task(task_b, TaskFun),
    Pattern = wf_term:par([Task1, Task2]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),

    %% Execute until halt
    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),

    %% Extract and analyze trace
    Trace = wf_vm:exec_trace(FinalState),

    %% Should have par_fork and par_join events
    ParForks = wf_trace:filter_by_type(Trace, par_fork),
    ?assertEqual(1, length(ParForks)),

    ParJoins = wf_trace:filter_by_type(Trace, par_join),
    ?assertEqual(1, length(ParJoins)).

%%% DETERMINISTIC REPLAY TESTS ==============================================

replay_simple_task_test() ->
    %% Create and execute a pattern
    TaskFun = fun(Ctx) -> {ok, maps:put(result, ok, Ctx)} end,
    Pattern = wf_term:task(simple_task, TaskFun),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),

    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),
    OriginalTrace = wf_vm:exec_trace(FinalState),

    %% Replay execution
    {ok, ReplayedState} = wf_trace:replay(Compiled, OriginalTrace),
    ReplayedTrace = wf_vm:exec_trace(ReplayedState),

    %% Traces should match
    ?assert(wf_trace:traces_match(OriginalTrace, ReplayedTrace)).

replay_validation_test() ->
    %% Create a simple pattern
    TaskFun = fun(Ctx) -> {ok, Ctx} end,
    Pattern = wf_term:task(test_task, TaskFun),

    %% Execute and get trace
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),

    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),
    Trace = wf_vm:exec_trace(FinalState),

    %% Validate replay
    Result = wf_trace:replay_validate(Pattern, InitCtx, Trace),
    ?assertEqual(ok, Result).
