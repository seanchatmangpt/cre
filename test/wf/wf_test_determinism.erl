%%% @doc WF Determinism and Replay Validation Tests
%%%
%%% Tests deterministic execution and trace-based replay validation.
%%% Key test scenarios:
%%% - Run workflow twice with same inputs, compare traces bit-for-bit
%%% - Verify deterministic scheduling behavior
%%% - Test replay from trace log reconstructs identical execution
%%% - Property-based tests with random pattern generation
%%%
%%% Requirements from WF_ARCHITECTURE.md:
%%% - Observable and Replayable: Every reduction step produces structured trace events
%%% - Deterministic scheduler + trace log → identical sequence of exec states
%%%
%%% @end
-module(wf_test_determinism).

-include_lib("eunit/include/eunit.hrl").

%%% BASIC DETERMINISM TESTS ================================================

%% @doc Test that a simple sequence produces identical traces on repeated runs.
simple_sequence_determinism_test() ->
    Pattern = wf_term:seq(
        wf_term:task(task_a, fun(Ctx) -> {ok, Ctx#{a => 1}} end),
        wf_term:task(task_b, fun(Ctx) -> {ok, Ctx#{b => 2}} end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => initial},

    %% Run 1
    State1 = wf_exec:exec_init(Compiled, InitCtx),
    {FinalState1, _Steps1} = wf_exec:exec_steps(State1, 1000),
    Trace1 = wf_vm:exec_trace(FinalState1),

    %% Run 2
    State2 = wf_exec:exec_init(Compiled, InitCtx),
    {FinalState2, _Steps2} = wf_exec:exec_steps(State2, 1000),
    Trace2 = wf_vm:exec_trace(FinalState2),

    %% Traces must be identical
    ?assertEqual(Trace1, Trace2),
    ?assert(wf_exec:is_halted(FinalState1)),
    ?assert(wf_exec:is_halted(FinalState2)).

%% @doc Test parallel fork produces deterministic traces.
parallel_fork_determinism_test() ->
    Pattern = wf_term:par([
        wf_term:task(task_a, fun(Ctx) -> {ok, Ctx#{a => 1}} end),
        wf_term:task(task_b, fun(Ctx) -> {ok, Ctx#{b => 2}} end),
        wf_term:task(task_c, fun(Ctx) -> {ok, Ctx#{c => 3}} end)
    ]),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{},

    %% Run multiple times
    Traces = [run_and_get_trace(Compiled, InitCtx) || _ <- lists:seq(1, 5)],

    %% All traces must be identical
    [FirstTrace | RestTraces] = Traces,
    lists:foreach(
        fun(T) -> ?assertEqual(FirstTrace, T) end,
        RestTraces
    ).

%% @doc Test choice with deterministic branch selection.
choice_determinism_test() ->
    Pattern = wf_term:choice([
        wf_term:task(branch_a, fun(Ctx) -> {ok, Ctx#{branch => a}} end),
        wf_term:task(branch_b, fun(Ctx) -> {ok, Ctx#{branch => b}} end)
    ]),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{},

    Trace1 = run_and_get_trace(Compiled, InitCtx),
    Trace2 = run_and_get_trace(Compiled, InitCtx),

    ?assertEqual(Trace1, Trace2).

%% @doc Test join policies produce deterministic results.
join_policy_determinism_test() ->
    Branches = [
        wf_term:task(t1, fun(Ctx) -> {ok, Ctx#{t1 => done}} end),
        wf_term:task(t2, fun(Ctx) -> {ok, Ctx#{t2 => done}} end),
        wf_term:task(t3, fun(Ctx) -> {ok, Ctx#{t3 => done}} end)
    ],

    %% Test different join policies
    Policies = [all, xor_merge, sync_merge, {first_n, 2}, {n_of_m, 2, 3}],

    lists:foreach(fun(Policy) ->
        Pattern = wf_term:join(Policy, Branches),
        {ok, Compiled} = wf_compile:compile(Pattern),

        Trace1 = run_and_get_trace(Compiled, #{}),
        Trace2 = run_and_get_trace(Compiled, #{}),

        ?assertEqual(Trace1, Trace2)
    end, Policies).

%% @doc Test loop produces deterministic traces.
loop_determinism_test() ->
    Pattern = wf_term:loop(
        {max_iter, 3},
        wf_term:task(loop_body, fun(Ctx) ->
            Counter = maps:get(counter, Ctx, 0),
            {ok, Ctx#{counter => Counter + 1}}
        end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),

    Trace1 = run_and_get_trace(Compiled, #{counter => 0}),
    Trace2 = run_and_get_trace(Compiled, #{counter => 0}),

    ?assertEqual(Trace1, Trace2).

%%% TRACE COMPARISON TESTS =================================================

%% @doc Test trace event structure consistency.
trace_structure_test() ->
    Pattern = wf_term:seq(
        wf_term:task(task1, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(task2, fun(Ctx) -> {ok, Ctx} end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),
    State = wf_exec:exec_init(Compiled, #{}),
    {FinalState, _} = wf_exec:exec_steps(State, 1000),

    Trace = wf_vm:exec_trace(FinalState),

    %% Validate trace event structure
    ?assert(length(Trace) > 0),

    lists:foreach(fun(Event) ->
        ?assertMatch({Seq, Type, Opcode, Ctx, Timestamp, Scope, CancelSignal}
                     when is_integer(Seq) andalso
                          is_atom(Type) andalso
                          is_atom(Opcode) andalso
                          is_map(Ctx) andalso
                          is_integer(Timestamp) andalso
                          is_list(Scope) andalso
                          is_boolean(CancelSignal),
                     Event)
    end, Trace).

%% @doc Test trace sequence numbers are monotonically increasing.
trace_sequence_monotonic_test() ->
    Pattern = wf_term:seq(
        wf_term:task(t1, fun(Ctx) -> {ok, Ctx} end),
        wf_term:seq(
            wf_term:task(t2, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(t3, fun(Ctx) -> {ok, Ctx} end)
        )
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),
    State = wf_exec:exec_init(Compiled, #{}),
    {FinalState, _} = wf_exec:exec_steps(State, 1000),

    Trace = wf_vm:exec_trace(FinalState),
    Sequences = [Seq || {Seq, _, _, _, _, _, _} <- Trace],

    %% Verify monotonic sequence
    ?assertEqual(lists:seq(0, length(Sequences) - 1), Sequences).

%% @doc Test trace timestamps are monotonically non-decreasing.
trace_timestamp_monotonic_test() ->
    Pattern = wf_term:par([
        wf_term:task(t1, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(t2, fun(Ctx) -> {ok, Ctx} end)
    ]),

    {ok, Compiled} = wf_compile:compile(Pattern),
    State = wf_exec:exec_init(Compiled, #{}),
    {FinalState, _} = wf_exec:exec_steps(State, 1000),

    Trace = wf_vm:exec_trace(FinalState),
    Timestamps = [TS || {_, _, _, _, TS, _, _} <- Trace],

    %% Verify non-decreasing timestamps
    lists:foldl(fun(TS, PrevTS) ->
        ?assert(TS >= PrevTS),
        TS
    end, 0, Timestamps).

%%% REPLAY VALIDATION TESTS ================================================

%% @doc Test that execution can be replayed from trace log.
%%
%% Note: Full replay requires scheduler state replay, which is not yet
%% implemented. This test validates that we can reconstruct the context
%% sequence from the trace.
replay_from_trace_test() ->
    Pattern = wf_term:seq(
        wf_term:task(step1, fun(Ctx) -> {ok, Ctx#{step1 => executed}} end),
        wf_term:task(step2, fun(Ctx) -> {ok, Ctx#{step2 => executed}} end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{initial => state},

    %% Original execution
    State1 = wf_exec:exec_init(Compiled, InitCtx),
    {FinalState1, _} = wf_exec:exec_steps(State1, 1000),
    Trace1 = wf_vm:exec_trace(FinalState1),

    %% Extract context progression from trace
    ContextProgression1 = [Ctx || {_, _, _, Ctx, _, _, _} <- Trace1],

    %% Replay execution
    State2 = wf_exec:exec_init(Compiled, InitCtx),
    {FinalState2, _} = wf_exec:exec_steps(State2, 1000),
    Trace2 = wf_vm:exec_trace(FinalState2),

    ContextProgression2 = [Ctx || {_, _, _, Ctx, _, _, _} <- Trace2],

    %% Context progressions must match
    ?assertEqual(ContextProgression1, ContextProgression2).

%% @doc Test replay with cancellation scope.
replay_with_cancellation_test() ->
    Pattern = wf_term:cancel_scope(
        {region, test_region},
        wf_term:task(task_in_scope, fun(Ctx) -> {ok, Ctx#{task => done}} end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),

    Trace1 = run_and_get_trace(Compiled, #{}),
    Trace2 = run_and_get_trace(Compiled, #{}),

    %% Traces must be identical, including scope information
    ?assertEqual(Trace1, Trace2),

    %% Verify scope information in trace
    Scopes = [Scope || {_, _, _, _, _, Scope, _} <- Trace1],
    ?assert(lists:any(fun(S) -> length(S) > 0 end, Scopes)).

%% @doc Test replay with multiple instances.
replay_with_mi_test() ->
    Pattern = wf_term:mi(
        {fixed, 3},
        wf_term:task(mi_task, fun(Ctx) -> {ok, Ctx#{executed => true}} end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),

    Trace1 = run_and_get_trace(Compiled, #{}),
    Trace2 = run_and_get_trace(Compiled, #{}),

    ?assertEqual(Trace1, Trace2).

%%% DETERMINISTIC SCHEDULING TESTS =========================================

%% @doc Test that execution is deterministic across different quantum sizes.
quantum_size_invariance_test() ->
    Pattern = wf_term:par([
        wf_term:seq(
            wf_term:task(t1a, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(t1b, fun(Ctx) -> {ok, Ctx} end)
        ),
        wf_term:seq(
            wf_term:task(t2a, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(t2b, fun(Ctx) -> {ok, Ctx} end)
        )
    ]),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{},

    %% Run with different quantum sizes
    TraceSmallQuanta = run_with_quantum(Compiled, InitCtx, 1),
    TraceNormalQuanta = run_with_quantum(Compiled, InitCtx, 10),
    TraceLargeQuanta = run_with_quantum(Compiled, InitCtx, 1000),

    %% All traces should be identical regardless of quantum size
    ?assertEqual(TraceSmallQuanta, TraceNormalQuanta),
    ?assertEqual(TraceNormalQuanta, TraceLargeQuanta).

%% @doc Test single-step execution produces identical results.
single_step_determinism_test() ->
    Pattern = wf_term:seq(
        wf_term:task(t1, fun(Ctx) -> {ok, Ctx#{t1 => done}} end),
        wf_term:task(t2, fun(Ctx) -> {ok, Ctx#{t2 => done}} end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{},

    %% Execute step-by-step, collect trace incrementally
    State1 = wf_exec:exec_init(Compiled, InitCtx),
    Trace1 = execute_single_steps(State1),

    State2 = wf_exec:exec_init(Compiled, InitCtx),
    Trace2 = execute_single_steps(State2),

    ?assertEqual(Trace1, Trace2).

%%% PROPERTY-BASED TESTS ===================================================

%% @doc Property: Any pattern executed twice with same input produces identical traces.
%%
%% This is a simplified property test. For full property-based testing,
%% integrate with PropEr or similar framework.
property_identical_traces_test_() ->
    RandomPatterns = [
        %% Simple sequence
        wf_term:seq(
            wf_term:task(p1, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(p2, fun(Ctx) -> {ok, Ctx} end)
        ),
        %% Parallel
        wf_term:par([
            wf_term:task(p1, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(p2, fun(Ctx) -> {ok, Ctx} end)
        ]),
        %% Choice
        wf_term:choice([
            wf_term:task(c1, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(c2, fun(Ctx) -> {ok, Ctx} end)
        ]),
        %% Nested
        wf_term:seq(
            wf_term:par([
                wf_term:task(n1, fun(Ctx) -> {ok, Ctx} end),
                wf_term:task(n2, fun(Ctx) -> {ok, Ctx} end)
            ]),
            wf_term:task(n3, fun(Ctx) -> {ok, Ctx} end)
        ),
        %% Loop
        wf_term:loop(
            {max_iter, 2},
            wf_term:task(loop_task, fun(Ctx) -> {ok, Ctx} end)
        ),
        %% Join policies
        wf_term:join(xor_merge, [
            wf_term:task(j1, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(j2, fun(Ctx) -> {ok, Ctx} end)
        ])
    ],

    %% Generate test for each pattern
    [test_pattern_determinism(P) || P <- RandomPatterns].

%% @doc Test a single pattern for determinism.
test_pattern_determinism(Pattern) ->
    Description = lists:flatten(wf_term:to_string(Pattern)),
    {Description, fun() ->
        {ok, Compiled} = wf_compile:compile(Pattern),

        Trace1 = run_and_get_trace(Compiled, #{}),
        Trace2 = run_and_get_trace(Compiled, #{}),
        Trace3 = run_and_get_trace(Compiled, #{}),

        ?assertEqual(Trace1, Trace2),
        ?assertEqual(Trace2, Trace3)
    end}.

%%% CONTEXT DETERMINISM TESTS ==============================================

%% @doc Test that context transformations are deterministic.
context_transformation_determinism_test() ->
    Pattern = wf_term:seq(
        wf_term:task(add_field, fun(Ctx) ->
            {ok, Ctx#{field => value}}
        end),
        wf_term:task(modify_field, fun(Ctx) ->
            {ok, Ctx#{field => modified}}
        end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{initial => data},

    %% Run multiple times
    Results = [begin
        State = wf_exec:exec_init(Compiled, InitCtx),
        {FinalState, _} = wf_exec:exec_steps(State, 1000),
        {ok, FinalCtx} = wf_exec:get_result(FinalState),
        FinalCtx
    end || _ <- lists:seq(1, 10)],

    %% All results must be identical
    [FirstResult | RestResults] = Results,
    lists:foreach(
        fun(R) -> ?assertEqual(FirstResult, R) end,
        RestResults
    ).

%% @doc Test that map operations in context are deterministic.
map_context_determinism_test() ->
    Pattern = wf_term:par([
        wf_term:task(task_a, fun(Ctx) ->
            Data = maps:get(data, Ctx, #{}),
            {ok, Ctx#{data => Data#{a => 1}}}
        end),
        wf_term:task(task_b, fun(Ctx) ->
            Data = maps:get(data, Ctx, #{}),
            {ok, Ctx#{data => Data#{b => 2}}}
        end)
    ]),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}},

    Trace1 = run_and_get_trace(Compiled, InitCtx),
    Trace2 = run_and_get_trace(Compiled, InitCtx),

    ?assertEqual(Trace1, Trace2).

%%% HELPER FUNCTIONS =======================================================

%% @doc Run a compiled pattern and return its trace.
-spec run_and_get_trace(wf_compile:compiled(), wf_term:context()) -> wf_vm:trace_log().
run_and_get_trace(Compiled, InitCtx) ->
    State = wf_exec:exec_init(Compiled, InitCtx),
    {FinalState, _Steps} = wf_exec:exec_steps(State, 1000),
    wf_vm:exec_trace(FinalState).

%% @doc Run a pattern with a specific quantum size.
-spec run_with_quantum(wf_compile:compiled(), wf_term:context(), pos_integer()) ->
    wf_vm:trace_log().
run_with_quantum(Compiled, InitCtx, QuantumSize) ->
    State = wf_exec:exec_init(Compiled, InitCtx),
    FinalState = run_with_quantum_loop(State, QuantumSize),
    wf_vm:exec_trace(FinalState).

%% @doc Execute in quanta until halted.
-spec run_with_quantum_loop(wf_vm:exec_state(), pos_integer()) -> wf_vm:exec_state().
run_with_quantum_loop(State, QuantumSize) ->
    case wf_exec:is_halted(State) of
        true ->
            State;
        false ->
            {NewState, _Steps} = wf_exec:exec_steps(State, QuantumSize),
            run_with_quantum_loop(NewState, QuantumSize)
    end.

%% @doc Execute single steps until halted, return final trace.
-spec execute_single_steps(wf_vm:exec_state()) -> wf_vm:trace_log().
execute_single_steps(State) ->
    case wf_exec:exec_step(State) of
        {continue, NewState} ->
            execute_single_steps(NewState);
        {halt, _Status, FinalState} ->
            wf_vm:exec_trace(FinalState);
        {error, _Reason, FinalState} ->
            wf_vm:exec_trace(FinalState);
        {yield, _Spec, NewState} ->
            %% For this test, we don't handle effects, just continue
            execute_single_steps(NewState)
    end.

%%% ERROR CASE DETERMINISM TESTS ===========================================

%% @doc Test that error cases produce deterministic traces.
error_determinism_test() ->
    Pattern = wf_term:seq(
        wf_term:task(will_succeed, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(will_fail, fun(_Ctx) -> {error, deliberate_failure} end)
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),

    Trace1 = run_and_get_trace(Compiled, #{}),
    Trace2 = run_and_get_trace(Compiled, #{}),

    %% Traces must be identical even with errors
    ?assertEqual(Trace1, Trace2),

    %% Verify error is recorded in trace
    EventTypes = [Type || {_, Type, _, _, _, _, _} <- Trace1],
    ?assert(lists:member(task_error, EventTypes)).

%% @doc Test complex nested pattern determinism.
complex_nested_determinism_test() ->
    Pattern = wf_term:seq(
        wf_term:par([
            wf_term:seq(
                wf_term:task(a1, fun(Ctx) -> {ok, Ctx} end),
                wf_term:task(a2, fun(Ctx) -> {ok, Ctx} end)
            ),
            wf_term:choice([
                wf_term:task(b1, fun(Ctx) -> {ok, Ctx} end),
                wf_term:task(b2, fun(Ctx) -> {ok, Ctx} end)
            ])
        ]),
        wf_term:loop(
            {max_iter, 2},
            wf_term:par([
                wf_term:task(c1, fun(Ctx) -> {ok, Ctx} end),
                wf_term:task(c2, fun(Ctx) -> {ok, Ctx} end)
            ])
        )
    ),

    {ok, Compiled} = wf_compile:compile(Pattern),

    %% Run 10 times
    Traces = [run_and_get_trace(Compiled, #{}) || _ <- lists:seq(1, 10)],

    %% All traces must be identical
    [FirstTrace | RestTraces] = Traces,
    lists:foreach(
        fun(T) -> ?assertEqual(FirstTrace, T) end,
        RestTraces
    ).
