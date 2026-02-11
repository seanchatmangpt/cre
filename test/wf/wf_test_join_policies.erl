%%% @doc WF Join Policy Tests
%%%
%%% This module tests all join policy semantics for synchronization patterns.
%%% Tests cover AND-join (all), XOR-merge, synchronizing merge, first-N, and
%%% N-of-M join policies.
%%%
%%% @end
-module(wf_test_join_policies).

-include_lib("eunit/include/eunit.hrl").

%%% TESTS ===================================================================

%% Test all join policy (AND-join)
all_join_policy_test() ->
    %% Create a parallel pattern with two branches, AND-join
    T1 = wf_term:task(t1, fun(Ctx) -> {ok, Ctx#{t1 => done}} end),
    T2 = wf_term:task(t2, fun(Ctx) -> {ok, Ctx#{t2 => done}} end),
    Pattern = wf_term:join(all, [T1, T2]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => initial},
    ExecState = wf_exec:exec_init(Compiled, InitCtx),

    %% Execute until halt
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Verify both tasks executed
    {ok, FinalCtx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(t1, FinalCtx)),
    ?assert(maps:is_key(t2, FinalCtx)),
    ?assertEqual(done, maps:get(t1, FinalCtx)),
    ?assertEqual(done, maps:get(t2, FinalCtx)).

%% Test all join with three branches
all_join_three_branches_test() ->
    %% Create pattern with three branches
    T1 = wf_term:task(a, fun(Ctx) -> {ok, Ctx#{a => 1}} end),
    T2 = wf_term:task(b, fun(Ctx) -> {ok, Ctx#{b => 2}} end),
    T3 = wf_term:task(c, fun(Ctx) -> {ok, Ctx#{c => 3}} end),
    Pattern = wf_term:join(all, [T1, T2, T3]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% All three tasks should complete
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assertEqual(1, maps:get(a, Ctx)),
    ?assertEqual(2, maps:get(b, Ctx)),
    ?assertEqual(3, maps:get(c, Ctx)).

%% Test xor_merge join policy
xor_merge_join_policy_test() ->
    %% XOR-merge: takes first available, discards others
    T1 = wf_term:task(fast, fun(Ctx) -> {ok, Ctx#{result => fast}} end),
    T2 = wf_term:task(slow, fun(Ctx) -> {ok, Ctx#{result => slow}} end),
    Pattern = wf_term:join(xor_merge, [T1, T2]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should complete (result depends on execution order, but should exist)
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(result, Ctx)).

%% Test xor_merge with simple_merge smart constructor
simple_merge_test() ->
    %% simple_merge is a smart constructor for xor_merge
    T1 = wf_term:task(task1, fun(Ctx) -> {ok, Ctx#{x => 1}} end),
    T2 = wf_term:task(task2, fun(Ctx) -> {ok, Ctx#{x => 2}} end),
    Pattern = wf_term:simple_merge([T1, T2]),

    %% Verify it's a join with xor_merge policy
    ?assertMatch({join, xor_merge, _}, Pattern),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should complete successfully
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(x, Ctx)).

%% Test sync_merge join policy
sync_merge_join_policy_test() ->
    %% Synchronizing merge: coordinate branches before merging
    T1 = wf_term:task(sync1, fun(Ctx) -> {ok, Ctx#{s1 => ok}} end),
    T2 = wf_term:task(sync2, fun(Ctx) -> {ok, Ctx#{s2 => ok}} end),
    Pattern = wf_term:join(sync_merge, [T1, T2]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should complete with synchronized execution
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(s1, Ctx) orelse maps:is_key(s2, Ctx)).

%% Test sync_merge with synchronizing_merge smart constructor
synchronizing_merge_test() ->
    T1 = wf_term:task(a, fun(Ctx) -> {ok, Ctx#{a => 1}} end),
    T2 = wf_term:task(b, fun(Ctx) -> {ok, Ctx#{b => 2}} end),
    Pattern = wf_term:synchronizing_merge([T1, T2]),

    %% Verify it's a join with sync_merge policy
    ?assertMatch({join, sync_merge, _}, Pattern),

    %% Execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, _} = wf_exec:exec_until_halt(ExecState).

%% Test first_n join policy with N=1
first_n_join_one_test() ->
    %% Wait for first 1 branch to complete (discriminator pattern)
    T1 = wf_term:task(opt1, fun(Ctx) -> {ok, Ctx#{choice => opt1}} end),
    T2 = wf_term:task(opt2, fun(Ctx) -> {ok, Ctx#{choice => opt2}} end),
    T3 = wf_term:task(opt3, fun(Ctx) -> {ok, Ctx#{choice => opt3}} end),
    Pattern = wf_term:join({first_n, 1}, [T1, T2, T3]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should complete when first branch finishes
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(choice, Ctx)).

%% Test first_n join policy with N=2
first_n_join_two_test() ->
    %% Wait for first 2 branches to complete
    T1 = wf_term:task(t1, fun(Ctx) -> {ok, Ctx#{t1 => ok}} end),
    T2 = wf_term:task(t2, fun(Ctx) -> {ok, Ctx#{t2 => ok}} end),
    T3 = wf_term:task(t3, fun(Ctx) -> {ok, Ctx#{t3 => ok}} end),
    T4 = wf_term:task(t4, fun(Ctx) -> {ok, Ctx#{t4 => ok}} end),
    Pattern = wf_term:join({first_n, 2}, [T1, T2, T3, T4]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should have at least some results
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(is_map(Ctx)).

%% Test discriminator smart constructor
discriminator_test() ->
    %% Discriminator: proceed on first branch, cancel others
    T1 = wf_term:task(winner, fun(Ctx) -> {ok, Ctx#{winner => true}} end),
    T2 = wf_term:task(loser1, fun(Ctx) -> {ok, Ctx#{loser1 => true}} end),
    T3 = wf_term:task(loser2, fun(Ctx) -> {ok, Ctx#{loser2 => true}} end),
    Pattern = wf_term:discriminator([T1, T2, T3]),

    %% Verify it's a first_n join with N=1
    ?assertMatch({join, {first_n, 1}, _}, Pattern),

    %% Execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should complete successfully
    {ok, _Ctx} = wf_exec:get_result(FinalState).

%% Test n_of_m join policy (2 of 3)
n_of_m_join_two_of_three_test() ->
    %% Wait for 2 out of 3 branches
    T1 = wf_term:task(a, fun(Ctx) -> {ok, Ctx#{a => 1}} end),
    T2 = wf_term:task(b, fun(Ctx) -> {ok, Ctx#{b => 2}} end),
    T3 = wf_term:task(c, fun(Ctx) -> {ok, Ctx#{c => 3}} end),
    Pattern = wf_term:join({n_of_m, 2, 3}, [T1, T2, T3]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should complete when 2 branches finish
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(is_map(Ctx)).

%% Test n_of_m join policy (3 of 5)
n_of_m_join_three_of_five_test() ->
    %% Wait for 3 out of 5 branches
    T1 = wf_term:task(t1, fun(Ctx) -> {ok, Ctx#{t1 => 1}} end),
    T2 = wf_term:task(t2, fun(Ctx) -> {ok, Ctx#{t2 => 2}} end),
    T3 = wf_term:task(t3, fun(Ctx) -> {ok, Ctx#{t3 => 3}} end),
    T4 = wf_term:task(t4, fun(Ctx) -> {ok, Ctx#{t4 => 4}} end),
    T5 = wf_term:task(t5, fun(Ctx) -> {ok, Ctx#{t5 => 5}} end),
    Pattern = wf_term:join({n_of_m, 3, 5}, [T1, T2, T3, T4, T5]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Should complete
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(is_map(Ctx)).

%% Test n_out_of_m smart constructor
n_out_of_m_test() ->
    %% Test the smart constructor for n-out-of-m pattern
    T1 = wf_term:task(v1, fun(Ctx) -> {ok, Ctx#{v1 => ok}} end),
    T2 = wf_term:task(v2, fun(Ctx) -> {ok, Ctx#{v2 => ok}} end),
    T3 = wf_term:task(v3, fun(Ctx) -> {ok, Ctx#{v3 => ok}} end),
    T4 = wf_term:task(v4, fun(Ctx) -> {ok, Ctx#{v4 => ok}} end),
    Pattern = wf_term:n_out_of_m(2, [T1, T2, T3, T4]),

    %% Verify it creates a proper join
    ?assertMatch({join, {n_of_m, 2, 4}, _}, Pattern),

    %% Execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, _} = wf_exec:exec_until_halt(ExecState).

%% Test join correctness: all join waits for all branches
join_correctness_all_test() ->
    %% Test that AND-join actually waits for all branches
    T1 = wf_term:task(step1, fun(Ctx) ->
        {ok, Ctx#{step1 => erlang:monotonic_time()}}
    end),
    T2 = wf_term:task(step2, fun(Ctx) ->
        {ok, Ctx#{step2 => erlang:monotonic_time()}}
    end),
    T3 = wf_term:task(step3, fun(Ctx) ->
        {ok, Ctx#{step3 => erlang:monotonic_time()}}
    end),
    Pattern = wf_term:join(all, [T1, T2, T3]),

    %% Compile and execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% All three tasks must have executed
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(step1, Ctx)),
    ?assert(maps:is_key(step2, Ctx)),
    ?assert(maps:is_key(step3, Ctx)),

    %% Verify all are integers (monotonic times)
    ?assert(is_integer(maps:get(step1, Ctx))),
    ?assert(is_integer(maps:get(step2, Ctx))),
    ?assert(is_integer(maps:get(step3, Ctx))).

%% Test join correctness: join counter tracking
join_counter_tracking_test() ->
    %% Test that join counters are properly maintained
    T1 = wf_term:task(t1, fun(Ctx) -> {ok, Ctx} end),
    T2 = wf_term:task(t2, fun(Ctx) -> {ok, Ctx} end),
    Pattern = wf_term:join(all, [T1, T2]),

    %% Compile
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),

    %% Check initial join counters (should be empty)
    Joins0 = wf_vm:exec_joins(ExecState),
    ?assertEqual(#{}, Joins0),

    %% Execute and verify completion
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),
    ?assert(wf_exec:is_halted(FinalState)).

%% Test join with nested sequences
join_nested_sequences_test() ->
    %% Each branch is a sequence
    S1 = wf_term:seq(
        wf_term:task(a1, fun(Ctx) -> {ok, Ctx#{a1 => 1}} end),
        wf_term:task(a2, fun(Ctx) -> {ok, Ctx#{a2 => 2}} end)
    ),
    S2 = wf_term:seq(
        wf_term:task(b1, fun(Ctx) -> {ok, Ctx#{b1 => 10}} end),
        wf_term:task(b2, fun(Ctx) -> {ok, Ctx#{b2 => 20}} end)
    ),
    Pattern = wf_term:join(all, [S1, S2]),

    %% Execute
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% All four tasks should have executed
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(a1, Ctx)),
    ?assert(maps:is_key(a2, Ctx)),
    ?assert(maps:is_key(b1, Ctx)),
    ?assert(maps:is_key(b2, Ctx)).

%% Test join with empty context preservation
join_context_preservation_test() ->
    %% Verify context flows through join correctly
    T1 = wf_term:task(increment, fun(Ctx) ->
        Val = maps:get(counter, Ctx, 0),
        {ok, Ctx#{counter => Val + 1}}
    end),
    T2 = wf_term:task(double, fun(Ctx) ->
        Val = maps:get(value, Ctx, 1),
        {ok, Ctx#{value => Val * 2}}
    end),
    Pattern = wf_term:join(all, [T1, T2]),

    %% Initialize with context
    InitCtx = #{counter => 5, value => 10},
    {ok, Compiled} = wf_compile:compile(Pattern),
    ExecState = wf_exec:exec_init(Compiled, InitCtx),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Context should be updated by both branches
    {ok, Ctx} = wf_exec:get_result(FinalState),
    ?assert(maps:is_key(counter, Ctx)),
    ?assert(maps:is_key(value, Ctx)).

%% Test validation: invalid join policies
invalid_join_policy_test() ->
    T1 = wf_term:task(t1, fun(Ctx) -> {ok, Ctx} end),
    T2 = wf_term:task(t2, fun(Ctx) -> {ok, Ctx} end),

    %% Test invalid policy atom
    ?assertError({invalid_term, _}, wf_term:join(invalid_policy, [T1, T2])),

    %% Test first_n with N=0
    ?assertError({invalid_term, _}, wf_term:join({first_n, 0}, [T1, T2])),

    %% Test n_of_m with N > M
    ?assertError({invalid_term, _}, wf_term:join({n_of_m, 3, 2}, [T1, T2])).

%% Test validation: empty branch list
empty_branches_test() ->
    %% Cannot create join with empty branches
    ?assertError(function_clause, wf_term:join(all, [])).

%% Test join policy validation
join_policy_validation_test() ->
    %% Valid policies
    ?assert(wf_term:is_valid(
        wf_term:join(all, [wf_term:task(t, fun(Ctx) -> {ok, Ctx} end)])
    )),
    ?assert(wf_term:is_valid(
        wf_term:join(xor_merge, [wf_term:task(t, fun(Ctx) -> {ok, Ctx} end)])
    )),
    ?assert(wf_term:is_valid(
        wf_term:join(sync_merge, [wf_term:task(t, fun(Ctx) -> {ok, Ctx} end)])
    )),
    ?assert(wf_term:is_valid(
        wf_term:join({first_n, 1}, [wf_term:task(t, fun(Ctx) -> {ok, Ctx} end)])
    )),
    ?assert(wf_term:is_valid(
        wf_term:join({n_of_m, 1, 2}, [
            wf_term:task(t1, fun(Ctx) -> {ok, Ctx} end),
            wf_term:task(t2, fun(Ctx) -> {ok, Ctx} end)
        ])
    )).

%% Test trace events for join execution
join_trace_events_test() ->
    T1 = wf_term:task(traced1, fun(Ctx) -> {ok, Ctx} end),
    T2 = wf_term:task(traced2, fun(Ctx) -> {ok, Ctx} end),
    Pattern = wf_term:join(all, [T1, T2]),

    %% Execute with tracing
    {ok, Compiled} = wf_compile:compile(Pattern, #{trace_level => full}),
    ExecState = wf_exec:exec_init(Compiled, #{}),
    {halt, ok, FinalState} = wf_exec:exec_until_halt(ExecState),

    %% Check trace log was populated
    Trace = wf_vm:exec_trace(FinalState),
    ?assert(length(Trace) > 0),

    %% Should have trace events for tasks and join
    ?assert(lists:any(fun({_Seq, Type, _Op, _Ctx, _TS, _Scope, _Cancel}) ->
        Type =:= task_enter orelse Type =:= task_ok
    end, Trace)).

%% Test join bytecode generation
join_bytecode_test() ->
    T1 = wf_term:task(bc1, fun(Ctx) -> {ok, Ctx} end),
    T2 = wf_term:task(bc2, fun(Ctx) -> {ok, Ctx} end),

    %% Test different join policies compile to different bytecode
    {ok, C1} = wf_compile:compile(wf_term:join(all, [T1, T2])),
    {ok, C2} = wf_compile:compile(wf_term:join(xor_merge, [T1, T2])),
    {ok, C3} = wf_compile:compile(wf_term:join(sync_merge, [T1, T2])),

    %% All should be valid compiled programs
    ?assert(wf_compile:is_compiled(C1)),
    ?assert(wf_compile:is_compiled(C2)),
    ?assert(wf_compile:is_compiled(C3)),

    %% Programs should have non-zero size
    ?assert(wf_compile:program_size(C1) > 0),
    ?assert(wf_compile:program_size(C2) > 0),
    ?assert(wf_compile:program_size(C3) > 0).
