%%% @doc WF Trace Usage Examples
%%%
%%% This module demonstrates how to use the wf_trace module for
%%% structured event tracing, deterministic replay, and execution analysis.
%%%
%%% @end
-module(wf_trace_example).

-export([
    basic_tracing/0,
    trace_analysis/0,
    deterministic_replay/0,
    trace_comparison/0,
    trace_persistence/0
]).

%%% BASIC TRACING ===========================================================

%% @doc Basic example: Execute a workflow and collect trace events.
basic_tracing() ->
    %% Define a simple workflow: task A -> task B
    TaskA = wf_term:task(task_a, fun(Ctx) ->
        io:format("Executing Task A~n"),
        {ok, maps:put(a_result, 42, Ctx)}
    end),

    TaskB = wf_term:task(task_b, fun(Ctx) ->
        io:format("Executing Task B~n"),
        AResult = maps:get(a_result, maps:get(data, Ctx, #{}), 0),
        {ok, maps:put(b_result, AResult * 2, Ctx)}
    end),

    Pattern = wf_term:seq(TaskA, TaskB),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),

    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),

    %% Extract trace
    Trace = wf_vm:exec_trace(FinalState),

    io:format("~nTrace Summary:~n"),
    io:format("  Total events: ~p~n", [wf_trace:log_size(Trace)]),
    io:format("  First event: ~p~n", [hd(Trace)]),
    io:format("  Last event: ~p~n", [element(1, wf_trace:get_last_event(Trace))]),

    {ok, Trace}.

%%% TRACE ANALYSIS ==========================================================

%% @doc Analyze trace events to understand execution behavior.
trace_analysis() ->
    %% Execute a parallel workflow
    TaskFun = fun(Name) ->
        fun(Ctx) ->
            timer:sleep(rand:uniform(10)),  % Simulate variable execution time
            io:format("Task ~p completed~n", [Name]),
            {ok, Ctx}
        end
    end,

    Pattern = wf_term:par([
        wf_term:task(task_1, TaskFun(task_1)),
        wf_term:task(task_2, TaskFun(task_2)),
        wf_term:task(task_3, TaskFun(task_3))
    ]),

    %% Execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),

    Trace = wf_vm:exec_trace(FinalState),

    %% Compute statistics
    Stats = wf_trace:trace_stats(Trace),

    io:format("~nExecution Statistics:~n"),
    io:format("  Total events: ~p~n", [maps:get(total_events, Stats)]),
    io:format("  Execution time: ~p μs~n", [maps:get(execution_time_us, Stats)]),
    io:format("  Max scope depth: ~p~n", [maps:get(max_scope_depth, Stats)]),
    io:format("  Cancelled steps: ~p~n", [maps:get(cancelled_steps, Stats)]),

    io:format("~nEvents by type:~n"),
    ByType = maps:get(by_type, Stats),
    maps:foreach(fun(Type, Count) ->
        io:format("    ~p: ~p~n", [Type, Count])
    end, ByType),

    %% Timeline analysis
    Timeline = wf_trace:execution_timeline(Trace),
    io:format("~nExecution timeline:~n"),
    lists:foreach(fun({Ts, Type, Opcode}) ->
        io:format("  ~p μs: ~p (~p)~n", [Ts, Type, Opcode])
    end, lists:sublist(Timeline, 10)),  % Show first 10 events

    %% Scope depth analysis
    DepthAnalysis = wf_trace:scope_depth_analysis(Trace),
    MaxDepth = lists:max([Depth || {_, Depth} <- DepthAnalysis]),
    io:format("~nMaximum nesting depth: ~p~n", [MaxDepth]),

    {ok, Stats}.

%%% DETERMINISTIC REPLAY ====================================================

%% @doc Demonstrate deterministic replay capability.
deterministic_replay() ->
    %% Create a workflow
    Counter = counters:new(1, []),
    TaskFun = fun(Ctx) ->
        %% Increment counter on each execution
        counters:add(Counter, 1, 1),
        Count = counters:get(Counter, 1),
        io:format("Task execution #~p~n", [Count]),
        {ok, maps:put(count, Count, Ctx)}
    end,

    Pattern = wf_term:seq(
        wf_term:task(task_a, TaskFun),
        wf_term:task(task_b, TaskFun)
    ),

    %% First execution
    io:format("~n=== First Execution ===~n"),
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State1 = wf_exec:exec_init(Compiled, InitCtx),
    {halt, ok, FinalState1} = wf_exec:exec_until_halt(State1),
    Trace1 = wf_vm:exec_trace(FinalState1),

    io:format("First execution completed with ~p events~n", [wf_trace:log_size(Trace1)]),

    %% Reset counter for replay
    counters:put(Counter, 1, 0),

    %% Replay execution
    io:format("~n=== Replay Execution ===~n"),
    {ok, ReplayedState} = wf_trace:replay(Compiled, Trace1),
    Trace2 = wf_vm:exec_trace(ReplayedState),

    io:format("Replay completed with ~p events~n", [wf_trace:log_size(Trace2)]),

    %% Compare traces
    Match = wf_trace:traces_match(Trace1, Trace2),
    io:format("~nTraces match: ~p~n", [Match]),

    if
        Match ->
            io:format("✓ Deterministic replay successful!~n");
        true ->
            Diff = wf_trace:diff_traces(Trace1, Trace2),
            io:format("✗ Replay diverged:~n"),
            io:format("  Matching events: ~p~n", [maps:get(matching, Diff)]),
            io:format("  Mismatches: ~p~n", [length(maps:get(mismatches, Diff))])
    end,

    {ok, Match}.

%%% TRACE COMPARISON ========================================================

%% @doc Compare traces from different executions.
trace_comparison() ->
    %% Create two similar but different workflows
    Pattern1 = wf_term:seq(
        wf_term:task(task_a, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(task_b, fun(Ctx) -> {ok, Ctx} end)
    ),

    Pattern2 = wf_term:seq(
        wf_term:task(task_a, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(task_c, fun(Ctx) -> {ok, Ctx} end)  % Different task
    ),

    %% Execute both
    {ok, Compiled1} = wf_compile:compile(Pattern1),
    {ok, Compiled2} = wf_compile:compile(Pattern2),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},

    State1 = wf_exec:exec_init(Compiled1, InitCtx),
    {halt, ok, Final1} = wf_exec:exec_until_halt(State1),
    Trace1 = wf_vm:exec_trace(Final1),

    State2 = wf_exec:exec_init(Compiled2, InitCtx),
    {halt, ok, Final2} = wf_exec:exec_until_halt(State2),
    Trace2 = wf_vm:exec_trace(Final2),

    %% Compare traces
    io:format("~n=== Trace Comparison ===~n"),
    Comparison = wf_trace:compare_traces(Trace1, Trace2),

    io:format("Matching events: ~p~n", [maps:get(matching, Comparison)]),
    io:format("Mismatched events: ~p~n", [length(maps:get(mismatched, Comparison))]),
    io:format("Length difference: ~p~n", [maps:get(length_diff, Comparison)]),

    %% Detailed diff
    Diff = wf_trace:diff_traces(Trace1, Trace2),
    Mismatches = maps:get(mismatches, Diff),

    io:format("~nFirst few mismatches:~n"),
    lists:foreach(fun(Mismatch) ->
        io:format("  Seq ~p, Field ~p: expected ~p, got ~p~n", [
            maps:get(seq, Mismatch),
            maps:get(field, Mismatch),
            maps:get(expected, Mismatch),
            maps:get(actual, Mismatch)
        ])
    end, lists:sublist(Mismatches, 5)),

    {ok, Comparison}.

%%% TRACE PERSISTENCE =======================================================

%% @doc Save and restore trace logs.
trace_persistence() ->
    %% Execute a workflow
    Pattern = wf_term:task(sample_task, fun(Ctx) -> {ok, Ctx} end),
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(State),
    OriginalTrace = wf_vm:exec_trace(FinalState),

    io:format("~n=== Trace Persistence ===~n"),
    io:format("Original trace size: ~p events~n", [wf_trace:log_size(OriginalTrace)]),

    %% Serialize to binary
    Binary = wf_trace:to_binary(OriginalTrace),
    io:format("Serialized size: ~p bytes~n", [byte_size(Binary)]),

    %% Deserialize
    {ok, RestoredTrace} = wf_trace:from_binary(Binary),
    io:format("Restored trace size: ~p events~n", [wf_trace:log_size(RestoredTrace)]),

    %% Verify integrity
    Match = wf_trace:traces_match(OriginalTrace, RestoredTrace),
    io:format("Integrity check: ~p~n", [Match]),

    if
        Match ->
            io:format("✓ Trace successfully persisted and restored~n");
        true ->
            io:format("✗ Trace corruption detected~n")
    end,

    %% Could save to file in real application
    %% file:write_file("trace.bin", Binary),
    %% {ok, Binary2} = file:read_file("trace.bin"),
    %% {ok, Trace2} = wf_trace:from_binary(Binary2),

    {ok, Binary}.
