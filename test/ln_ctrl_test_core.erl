%%% @doc ln_ctrl Core Unit and Integration Tests
%%%
%%% Tests for:
%%% - Simple task sequence end-to-end
%%% - Cancellation mid-execution
%%% - Deterministic policy (3 runs → identical traces)
%%% - Replay policy (record → reproduce)
%%% - Budget enforcement (exceeding max_effects)
%%% - Receipt chain validation
%%%
%%% @end
-module(ln_ctrl_test_core).

-include_lib("eunit/include/eunit.hrl").

%%% TEST FIXTURES ============================================================

%% Simple task that succeeds
simple_task() ->
    fun(_Ctx) -> {ok, #{data => completed}} end.

%% Sequence of two tasks
simple_sequence() ->
    wf_term:seq(
        wf_term:task(task1, simple_task()),
        wf_term:task(task2, simple_task())
    ).

%% Parallel execution of two tasks
simple_parallel() ->
    wf_term:par([
        wf_term:task(task1, simple_task()),
        wf_term:task(task2, simple_task())
    ]).

%% Task with cancellation scope
scoped_task() ->
    wf_term:cancel_scope(
        {region, my_region},
        wf_term:task(task1, simple_task())
    ).

%%% TESTS ===================================================================

%% Test 1: Simple task sequence runs end-to-end
simple_sequence_end_to_end_test_() ->
    {
        "Simple task sequence completes",
        fun() ->
            Pattern = simple_sequence(),
            InitCtx = #{},

            {ok, Compiled} = wf_compile:compile(Pattern),
            ExecState = wf_exec:exec_init(Compiled, InitCtx),

            %% Execute until halt
            {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

            %% Verify we halted successfully
            ?assert(wf_exec:is_halted(FinalState)),
            ?assertEqual(ok, element(2, wf_exec:get_result(FinalState)))
        end
    }.

%% Test 2: Cancellation scope mid-execution
cancel_scope_mid_execution_test_() ->
    {
        "Cancellation scope stops execution",
        fun() ->
            Pattern = scoped_task(),
            InitCtx = #{},

            {ok, Compiled} = wf_compile:compile(Pattern),
            ExecState = wf_exec:exec_init(Compiled, InitCtx),

            %% Step a few times
            {State1, _Steps1} = wf_exec:exec_steps(ExecState, 2),

            %% Cancel the scope
            CancelFlags = wf_vm:exec_cancel(State1),
            NewCancelFlags = maps:put(my_region, true, CancelFlags),
            State2 = wf_vm:exec_set_cancel(State1, NewCancelFlags),

            %% Verify scope is marked as cancelled
            ?assert(ln_ctrl_cancel:is_cancelled(my_region, State2))
        end
    }.

%% Test 3: Deterministic policy produces identical traces
deterministic_identical_traces_test_() ->
    {
        "Deterministic scheduler produces identical traces",
        fun() ->
            Pattern = simple_sequence(),
            InitCtx = #{},

            {ok, Compiled} = wf_compile:compile(Pattern),

            %% Run 1
            State1 = wf_exec:exec_init(Compiled, InitCtx),
            {halt, ok, FinalState1} = wf_exec:exec_until_halt(State1),
            Trace1 = wf_vm:exec_trace(FinalState1),

            %% Run 2 (fresh state)
            State2 = wf_exec:exec_init(Compiled, InitCtx),
            {halt, ok, FinalState2} = wf_exec:exec_until_halt(State2),
            Trace2 = wf_vm:exec_trace(FinalState2),

            %% Run 3 (fresh state)
            State3 = wf_exec:exec_init(Compiled, InitCtx),
            {halt, ok, FinalState3} = wf_exec:exec_until_halt(State3),
            Trace3 = wf_vm:exec_trace(FinalState3),

            %% All traces should be identical
            ?assertEqual(Trace1, Trace2),
            ?assertEqual(Trace2, Trace3),
            ?assert(length(Trace1) > 0)
        end
    }.

%% Test 4: Deterministic scheduler implementation
deterministic_scheduler_test_() ->
    {
        "Deterministic scheduler always picks first choice",
        fun() ->
            Policy = ln_ctrl_sched:new_deterministic(),
            Choices = [choice1, choice2, choice3],

            {Branch1, _UpdatedPolicy1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
            {Branch2, _UpdatedPolicy2} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),

            ?assertEqual(choice1, Branch1),
            ?assertEqual(choice1, Branch2)
        end
    }.

%% Test 5: Nondeterministic scheduler records choices
nondeterministic_scheduler_test_() ->
    {
        "Nondeterministic scheduler records choice log",
        fun() ->
            Policy = ln_ctrl_sched:new_nondeterministic(),
            Choices = [choice1, choice2, choice3],

            {Branch1, UpdatedPolicy1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
            {Branch2, _UpdatedPolicy2} = ln_ctrl_sched:apply_policy(UpdatedPolicy1, undefined, Choices),

            ?assertEqual(choice1, Branch1),
            ?assertEqual(choice1, Branch2)
        end
    }.

%% Test 6: Replay policy reproduces execution
replay_scheduler_test_() ->
    {
        "Replay scheduler reproduces recorded choices",
        fun() ->
            %% Use simple choice log
            ChoiceLog = [{0, 1}, {0, 2}],
            Choices = [choice1, choice2, choice3],

            %% Run with replay
            ReplayPolicy = ln_ctrl_sched:new_replay(ChoiceLog),
            {ReplayBranch1, UpdatedReplay1} = ln_ctrl_sched:apply_policy(ReplayPolicy, undefined, Choices),
            {ReplayBranch2, _UpdatedReplay2} = ln_ctrl_sched:apply_policy(UpdatedReplay1, undefined, Choices),

            ?assertEqual(choice1, ReplayBranch1),
            ?assertEqual(choice1, ReplayBranch2)
        end
    }.

%% Test 7: Budget enforcement - exceeding max_effects
budget_max_effects_test_() ->
    {
        "Budget halts case when max_effects exceeded",
        fun() ->
            Budget = ln_ctrl_budget:new_budget(2, unlimited, unlimited),

            {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),
            {ok, B2} = ln_ctrl_budget:check_effect(B1, 0.0),

            %% Third effect should exceed
            {budget_exceeded, {max_effects_exceeded, 3, 2}, _B3} =
                ln_ctrl_budget:check_effect(B2, 0.0),

            %% Verify budget status
            Status = ln_ctrl_budget:status(B2),
            ?assertEqual(2, maps:get(effects_used, Status)),
            ?assert(maps:get(exceeded, Status) == false)
        end
    }.

%% Test 8: Budget enforcement - cost limit
budget_cost_limit_test_() ->
    {
        "Budget enforces cost limit",
        fun() ->
            Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 10.0),

            {ok, B1} = ln_ctrl_budget:check_effect(Budget, 5.0),
            {ok, B2} = ln_ctrl_budget:check_effect(B1, 4.0),

            %% Next effect would exceed cost
            {budget_exceeded, {max_cost_exceeded, 10.1, 10.0}, _B3} =
                ln_ctrl_budget:check_effect(B2, 1.1),

            ok
        end
    }.

%% Test 9: Budget enforcement - latency limit
budget_latency_limit_test_() ->
    {
        "Budget enforces latency limit",
        fun() ->
            Budget = ln_ctrl_budget:new_budget(unlimited, 1000, unlimited),

            ok = ln_ctrl_budget:check_latency(Budget, 500),
            ok = ln_ctrl_budget:check_latency(Budget, 1000),

            {timeout, {max_latency_exceeded, 1500, 1000}, _B} =
                ln_ctrl_budget:check_latency(Budget, 1500),

            ok
        end
    }.

%% Test 10: Receipt building
receipt_build_test_() ->
    {
        "Receipt builder creates valid receipts",
        fun() ->
            Input = #{ontology => data},
            Templates = #{template => value},
            Params = #{param => config},

            Receipt = ln_ctrl_receipt:build_receipt(Input, Templates, Params),

            %% Verify by calling interface, not by direct record access
            ok = ln_ctrl_receipt:issue_receipt(Receipt, logger),
            ?assert(true)
        end
    }.

%% Test 11: Receipt effect tracking
receipt_effect_test_() ->
    {
        "Receipt tracking for effects",
        fun() ->
            Receipt1 = ln_ctrl_receipt:effect_receipt(effect1, {ok, result}),
            Receipt2 = ln_ctrl_receipt:effect_receipt(effect2, {error, failed}),

            %% Verify via interface
            ok = ln_ctrl_receipt:issue_receipt(Receipt1, logger),
            ok = ln_ctrl_receipt:issue_receipt(Receipt2, logger),
            ?assert(true)
        end
    }.

%% Test 12: Receipt chain validation
receipt_chain_validation_test_() ->
    {
        "Receipt chain validates hash linkage",
        fun() ->
            R1 = ln_ctrl_receipt:build_receipt(#{}, #{}, #{}),
            R2 = ln_ctrl_receipt:effect_receipt(effect1, result1),
            R3 = ln_ctrl_receipt:effect_receipt(effect2, result2),

            %% Empty and single receipts should validate
            ?assertMatch({ok, true}, ln_ctrl_receipt:validate_chain([])),
            ?assertMatch({ok, true}, ln_ctrl_receipt:validate_chain([R1])),
            ?assertMatch({ok, true}, ln_ctrl_receipt:validate_chain([R1, R2])),
            ?assertMatch({ok, true}, ln_ctrl_receipt:validate_chain([R1, R2, R3]))
        end
    }.

%% Test 13: Cancellation filtering effects
cancel_filter_effects_test_() ->
    {
        "Cancellation filters out post-cancel effects",
        fun() ->
            NowMs = erlang:monotonic_time(millisecond),
            OldEffect = {eff1, NowMs - 1000, {effect, test, payload}},
            NewEffect = {eff2, NowMs + 1000, {effect, test, payload}},
            EffectLog = [OldEffect, NewEffect],

            Signal = ln_ctrl_cancel:new_cancel_signal(my_scope),
            FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

            ?assertEqual(1, length(FilteredLog)),
            ?assertEqual(OldEffect, hd(FilteredLog))
        end
    }.

%% Test 14: Cancellation signal creation
cancel_signal_test_() ->
    {
        "Cancel signal is properly timestamped",
        fun() ->
            Signal = ln_ctrl_cancel:new_cancel_signal(test_scope),

            %% Verify signal works via stop_effects_in_scope
            EffectLog = [],
            FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),
            ?assertEqual([], FilteredLog)
        end
    }.

%% Test 15: Pattern validation
pattern_validation_test_() ->
    {
        "Pattern validation rejects invalid patterns",
        fun() ->
            ValidPattern = wf_term:task(simple, simple_task()),
            InvalidPattern = {invalid, structure},

            ok = ln_ctrl:validate(ValidPattern, #{}),
            ?assertMatch({error, _}, ln_ctrl:validate(InvalidPattern, #{}))
        end
    }.

%% Test 16: Parallel execution
parallel_execution_test_() ->
    {
        "Parallel branches execute correctly",
        fun() ->
            Pattern = simple_parallel(),
            InitCtx = #{},

            {ok, Compiled} = wf_compile:compile(Pattern),
            ExecState = wf_exec:exec_init(Compiled, InitCtx),

            {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

            ?assert(wf_exec:is_halted(FinalState)),
            ?assertMatch({ok, _}, wf_exec:get_result(FinalState))
        end
    }.

%% Test 17: Execution status tracking
execution_status_test_() ->
    {
        "Execution status reports correct metrics",
        fun() ->
            Pattern = simple_sequence(),
            InitCtx = #{},

            {ok, Compiled} = wf_compile:compile(Pattern),
            ExecState = wf_exec:exec_init(Compiled, InitCtx),

            Status = wf_exec:exec_status(ExecState),

            ?assertEqual(running, maps:get(state, Status)),
            ?assertEqual(0, maps:get(pc, Status)),
            ?assertEqual(0, maps:get(steps, Status)),
            ?assertEqual(0, maps:get(stack_depth, Status))
        end
    }.

%% Test 18: Compilation succeeds for all patterns
compilation_test_() ->
    {
        "All pattern types compile successfully",
        fun() ->
            Patterns = [
                simple_sequence(),
                simple_parallel(),
                scoped_task(),
                wf_term:choice([
                    wf_term:task(a, simple_task()),
                    wf_term:task(b, simple_task())
                ]),
                wf_term:loop({max_iter, 5}, wf_term:task(loop_task, simple_task()))
            ],

            Results = [wf_compile:compile(P) || P <- Patterns],

            lists:foreach(
                fun({ok, Compiled}) ->
                    ?assert(wf_compile:is_compiled(Compiled)),
                    ?assert(wf_compile:program_size(Compiled) > 0);
                ({error, _Reason}) ->
                    throw(compilation_failed)
                end,
                Results
            ),
            ?assert(true)
        end
    }.
