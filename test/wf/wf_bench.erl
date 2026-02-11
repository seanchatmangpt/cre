%%% @doc WF Substrate Performance Benchmark
%%%
%%% Performance benchmarking harness for the WF substrate. Validates that
%%% pattern execution meets specified performance targets:
%%%
%%% - Pure sequence: < 1 μs per step
%%% - Parallel join: < 10 μs per join
%%% - Cancellation: < 100 μs for cancel_region
%%% - Effect yield/resume: < 50 μs
%%% - Memory: < 1MB per case
%%%
%%% @end
-module(wf_bench).

-export([
    run_all/0,
    bench_sequence/0,
    bench_parallel/0,
    bench_cancellation/0,
    bench_effect/0,
    bench_memory/0,
    bench_custom/2,
    report/1
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-type bench_result() :: #{
    name := string(),
    iterations := non_neg_integer(),
    total_time_us := non_neg_integer(),
    avg_time_us := float(),
    min_time_us := non_neg_integer(),
    max_time_us := non_neg_integer(),
    memory_bytes := non_neg_integer(),
    passed := boolean(),
    target_us := float()
}.

%%% PUBLIC API ==============================================================

%% @doc Run all benchmarks and report results.
-spec run_all() -> [bench_result()].
run_all() ->
    Results = [
        bench_sequence(),
        bench_parallel(),
        bench_cancellation(),
        bench_effect(),
        bench_memory()
    ],
    io:format("~n=== WF SUBSTRATE BENCHMARK RESULTS ===~n~n", []),
    lists:foreach(fun report/1, Results),
    io:format("~n", []),
    Results.

%% @doc Benchmark pure sequence execution (target: < 1 μs per step).
-spec bench_sequence() -> bench_result().
bench_sequence() ->
    %% Build a sequence of 10,000 trivial tasks
    NumSteps = 10000,
    Pattern = build_sequence(NumSteps),

    %% Compile pattern
    {ok, Compiled} = wf_compile:compile(Pattern),

    %% Benchmark execution
    InitCtx = #{data => 0},

    {Times, Memory} = run_benchmark(
        fun() ->
            State = wf_exec:exec_init(Compiled, InitCtx),
            {FinalState, _} = wf_exec:exec_steps(State, NumSteps * 3),
            wf_exec:get_result(FinalState)
        end,
        100  % 100 iterations
    ),

    TotalTime = lists:sum(Times),
    AvgTime = TotalTime / length(Times),
    TimePerStep = AvgTime / NumSteps,

    #{
        name => "Pure Sequence (10k steps)",
        iterations => length(Times),
        total_time_us => TotalTime,
        avg_time_us => AvgTime,
        min_time_us => lists:min(Times),
        max_time_us => lists:max(Times),
        time_per_step_us => TimePerStep,
        memory_bytes => Memory,
        passed => TimePerStep < 1.0,
        target_us => 1.0
    }.

%% @doc Benchmark parallel join (target: < 10 μs per join).
-spec bench_parallel() -> bench_result().
bench_parallel() ->
    %% Build a pattern with 100 parallel branches
    NumBranches = 100,
    Pattern = build_parallel(NumBranches),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => 0},

    {Times, Memory} = run_benchmark(
        fun() ->
            State = wf_exec:exec_init(Compiled, InitCtx),
            {FinalState, _} = wf_exec:exec_steps(State, 10000),
            wf_exec:get_result(FinalState)
        end,
        1000  % 1000 iterations
    ),

    TotalTime = lists:sum(Times),
    AvgTime = TotalTime / length(Times),
    TimePerJoin = AvgTime / NumBranches,

    #{
        name => "Parallel Join (100 branches)",
        iterations => length(Times),
        total_time_us => TotalTime,
        avg_time_us => AvgTime,
        min_time_us => lists:min(Times),
        max_time_us => lists:max(Times),
        time_per_join_us => TimePerJoin,
        memory_bytes => Memory,
        passed => TimePerJoin < 10.0,
        target_us => 10.0
    }.

%% @doc Benchmark cancellation (target: < 100 μs for cancel_region).
-spec bench_cancellation() -> bench_result().
bench_cancellation() ->
    %% Build a pattern with 50 activities in a cancellable region
    NumActivities = 50,
    Pattern = build_cancellable_region(NumActivities),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => 0, cancel_at => 25},

    {Times, Memory} = run_benchmark(
        fun() ->
            State = wf_exec:exec_init(Compiled, InitCtx),
            {FinalState, _} = wf_exec:exec_steps(State, 10000),
            wf_exec:get_result(FinalState)
        end,
        1000  % 1000 iterations
    ),

    TotalTime = lists:sum(Times),
    AvgTime = TotalTime / length(Times),

    #{
        name => "Cancellation (50 activities)",
        iterations => length(Times),
        total_time_us => TotalTime,
        avg_time_us => AvgTime,
        min_time_us => lists:min(Times),
        max_time_us => lists:max(Times),
        memory_bytes => Memory,
        passed => AvgTime < 100.0,
        target_us => 100.0
    }.

%% @doc Benchmark effect yield/resume (target: < 50 μs round-trip).
-spec bench_effect() -> bench_result().
bench_effect() ->
    %% Build a pattern with an effect-yielding task
    Pattern = wf_term:task(effect_task, fun(Ctx) ->
        case maps:get(mode, Ctx, normal) of
            yield_effect ->
                {effect, {effect, test_effect, #{}, effect1}, Ctx#{mode := resume}};
            resume ->
                {ok, Ctx}
        end
    end),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => 0, mode => yield_effect},

    {Times, Memory} = run_benchmark(
        fun() ->
            State = wf_exec:exec_init(Compiled, InitCtx),
            case wf_exec:exec_until_effect(State) of
                {yield, _Spec, YieldState} ->
                    %% Simulate effect execution
                    ResumeState = wf_vm:exec_set_ctx(YieldState, InitCtx#{mode := resume}),
                    wf_exec:exec_until_halt(ResumeState);
                Other ->
                    Other
            end
        end,
        1000  % 1000 iterations
    ),

    TotalTime = lists:sum(Times),
    AvgTime = TotalTime / length(Times),

    #{
        name => "Effect Yield/Resume",
        iterations => length(Times),
        total_time_us => TotalTime,
        avg_time_us => AvgTime,
        min_time_us => lists:min(Times),
        max_time_us => lists:max(Times),
        memory_bytes => Memory,
        passed => AvgTime < 50.0,
        target_us => 50.0
    }.

%% @doc Benchmark memory usage (target: < 1MB per case).
-spec bench_memory() -> bench_result().
bench_memory() ->
    %% Build a complex pattern
    Pattern = build_complex_pattern(),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => lists:seq(1, 100)},

    %% Run once and measure memory
    State = wf_exec:exec_init(Compiled, InitCtx),
    {FinalState, _} = wf_exec:exec_steps(State, 10000),

    %% Calculate memory usage
    Memory = calculate_memory(FinalState),

    #{
        name => "Memory Usage (complex pattern)",
        iterations => 1,
        total_time_us => 0,
        avg_time_us => 0.0,
        min_time_us => 0,
        max_time_us => 0,
        memory_bytes => Memory,
        passed => Memory < 1048576,  % 1MB in bytes
        target_us => 0.0
    }.

%% @doc Run a custom benchmark.
-spec bench_custom(Pattern :: wf_term:wf_term(), Iterations :: non_neg_integer()) -> bench_result().
bench_custom(Pattern, Iterations) ->
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => 0},

    {Times, Memory} = run_benchmark(
        fun() ->
            State = wf_exec:exec_init(Compiled, InitCtx),
            {FinalState, _} = wf_exec:exec_steps(State, 10000),
            wf_exec:get_result(FinalState)
        end,
        Iterations
    ),

    TotalTime = lists:sum(Times),
    AvgTime = TotalTime / length(Times),

    #{
        name => "Custom Benchmark",
        iterations => length(Times),
        total_time_us => TotalTime,
        avg_time_us => AvgTime,
        min_time_us => lists:min(Times),
        max_time_us => lists:max(Times),
        memory_bytes => Memory,
        passed => true,
        target_us => 0.0
    }.

%% @doc Print a benchmark result.
-spec report(Result :: bench_result()) -> ok.
report(Result) ->
    Name = maps:get(name, Result),
    Iterations = maps:get(iterations, Result),
    AvgTime = maps:get(avg_time_us, Result),
    MinTime = maps:get(min_time_us, Result),
    MaxTime = maps:get(max_time_us, Result),
    Memory = maps:get(memory_bytes, Result),
    Passed = maps:get(passed, Result),
    Target = maps:get(target_us, Result),

    Status = case Passed of
        true -> "PASS";
        false -> "FAIL"
    end,

    io:format("~s: ~s~n", [Name, Status]),
    io:format("  Iterations: ~p~n", [Iterations]),
    io:format("  Avg Time:   ~.2f μs~n", [AvgTime]),
    io:format("  Min Time:   ~p μs~n", [MinTime]),
    io:format("  Max Time:   ~p μs~n", [MaxTime]),
    io:format("  Memory:     ~p bytes (~.2f KB)~n", [Memory, Memory / 1024]),

    case maps:find(time_per_step_us, Result) of
        {ok, TimePerStep} ->
            io:format("  Time/Step:  ~.4f μs (target: < ~.2f μs)~n", [TimePerStep, Target]);
        error ->
            case maps:find(time_per_join_us, Result) of
                {ok, TimePerJoin} ->
                    io:format("  Time/Join:  ~.4f μs (target: < ~.2f μs)~n", [TimePerJoin, Target]);
                error ->
                    case Target > 0 of
                        true ->
                            io:format("  Target:     < ~.2f μs~n", [Target]);
                        false ->
                            ok
                    end
            end
    end,

    io:format("~n", []),
    ok.

%%% INTERNAL HELPERS ========================================================

%% @doc Build a sequence pattern with N tasks.
-spec build_sequence(N :: non_neg_integer()) -> wf_term:wf_term().
build_sequence(1) ->
    wf_term:task(step1, fun(Ctx) -> {ok, Ctx} end);
build_sequence(N) when N > 1 ->
    Task = wf_term:task(list_to_atom("step" ++ integer_to_list(N)),
                        fun(Ctx) -> {ok, Ctx} end),
    wf_term:seq(Task, build_sequence(N - 1)).

%% @doc Build a parallel pattern with N branches.
-spec build_parallel(N :: non_neg_integer()) -> wf_term:wf_term().
build_parallel(N) ->
    Branches = [wf_term:task(list_to_atom("branch" ++ integer_to_list(I)),
                              fun(Ctx) -> {ok, Ctx} end)
                || I <- lists:seq(1, N)],
    wf_term:par(Branches).

%% @doc Build a cancellable region with N activities.
-spec build_cancellable_region(N :: non_neg_integer()) -> wf_term:wf_term().
build_cancellable_region(N) ->
    Body = build_sequence(N),
    wf_term:cancel_scope({region, test_region}, Body).

%% @doc Build a complex pattern for memory testing.
-spec build_complex_pattern() -> wf_term:wf_term().
build_complex_pattern() ->
    %% Nested sequence with parallel sections
    Par1 = wf_term:par([
        wf_term:task(p1, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(p2, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(p3, fun(Ctx) -> {ok, Ctx} end)
    ]),

    Loop = wf_term:loop({max_iter, 5},
        wf_term:task(loop_task, fun(Ctx) -> {ok, Ctx} end)),

    Par2 = wf_term:par([
        wf_term:task(p4, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(p5, fun(Ctx) -> {ok, Ctx} end)
    ]),

    wf_term:seq(Par1, wf_term:seq(Loop, Par2)).

%% @doc Run a benchmark function multiple times and measure execution time.
-spec run_benchmark(Fun :: fun(() -> any()), Iterations :: non_neg_integer()) ->
    {Times :: [non_neg_integer()], Memory :: non_neg_integer()}.
run_benchmark(Fun, Iterations) ->
    %% Warm up
    _ = Fun(),

    %% Run benchmark
    Times = [measure_time(Fun) || _ <- lists:seq(1, Iterations)],

    %% Measure memory
    Memory = measure_memory(Fun),

    {Times, Memory}.

%% @doc Measure execution time of a function in microseconds.
-spec measure_time(Fun :: fun(() -> any())) -> non_neg_integer().
measure_time(Fun) ->
    T0 = erlang:monotonic_time(microsecond),
    _ = Fun(),
    T1 = erlang:monotonic_time(microsecond),
    T1 - T0.

%% @doc Measure memory usage of a function in bytes.
-spec measure_memory(Fun :: fun(() -> any())) -> non_neg_integer().
measure_memory(Fun) ->
    erlang:garbage_collect(),
    {_, M0} = erlang:process_info(self(), memory),
    Result = Fun(),
    {_, M1} = erlang:process_info(self(), memory),
    %% Keep result alive to prevent GC optimization
    _ = Result,
    erlang:max(0, M1 - M0).

%% @doc Calculate memory usage of an execution state.
-spec calculate_memory(State :: wf_vm:exec_state()) -> non_neg_integer().
calculate_memory(State) ->
    %% Use erts_debug:flat_size to get accurate size
    Words = erts_debug:flat_size(State),
    WordSize = erlang:system_info(wordsize),
    Words * WordSize.

%%% EUNIT TESTS =============================================================

sequence_bench_test() ->
    Result = bench_sequence(),
    ?assert(maps:get(passed, Result)),
    ?assert(maps:get(time_per_step_us, Result) < 1.0).

parallel_bench_test() ->
    Result = bench_parallel(),
    ?assert(maps:get(passed, Result)),
    ?assert(maps:get(time_per_join_us, Result) < 10.0).

cancellation_bench_test() ->
    Result = bench_cancellation(),
    ?assert(maps:get(passed, Result)),
    ?assert(maps:get(avg_time_us, Result) < 100.0).

effect_bench_test() ->
    Result = bench_effect(),
    ?assert(maps:get(passed, Result)),
    ?assert(maps:get(avg_time_us, Result) < 50.0).

memory_bench_test() ->
    Result = bench_memory(),
    ?assert(maps:get(passed, Result)),
    ?assert(maps:get(memory_bytes, Result) < 1048576).
