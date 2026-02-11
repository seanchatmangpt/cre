%%%-------------------------------------------------------------------
%%% @doc EUnit tests for ln_compile module.
%%% @end
%%%-------------------------------------------------------------------
-module(ln_compile_test).
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Generators
%%%-------------------------------------------------------------------

%% @doc Test compile single task.
compile_single_task_test() ->
    Plan = ln_plan:task(my_task),
    {ok, Bytecode} = ln_compile:compile(Plan),
    ?assertMatch(#{program := _, joins := _, scopes := _}, Bytecode),
    ?assert(is_list(maps:get(program, Bytecode))).

%% @doc Test compile sequence.
compile_sequence_test() ->
    Plan = ln_plan:seq([ln_plan:task(a), ln_plan:task(b)]),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Program = maps:get(program, Bytecode),
    ?assert(length(Program) > 0).

%% @doc Test compile parallel.
compile_parallel_test() ->
    Plan = ln_plan:par([ln_plan:task(a), ln_plan:task(b)]),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Joins = maps:get(joins, Bytecode),
    ?assert(maps:size(Joins) > 0).

%% @doc Test compile xor.
compile_xor_test() ->
    Plan = ln_plan:xor([ln_plan:task(a), ln_plan:task(b)]),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Program = maps:get(program, Bytecode),
    ?assert(length(Program) > 0).

%% @doc Test compile join.
compile_join_test() ->
    Plan = ln_plan:join(all, [ln_plan:task(a), ln_plan:task(b)]),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Joins = maps:get(joins, Bytecode),
    ?assert(maps:size(Joins) =:= 1).

%% @doc Test compile join with n_of_m.
compile_join_n_of_m_test() ->
    Plan = ln_plan:join({n_of_m, 2}, [ln_plan:task(a), ln_plan:task(b), ln_plan:task(c)]),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Joins = maps:get(joins, Bytecode),
    ?assert(maps:size(Joins) =:= 1),
    {JoinId, {_Label, BranchCount, Policy}} = maps:to_list(Joins),
    ?assertEqual({n_of_m, 2}, Policy),
    ?assertEqual(3, BranchCount).

%% @doc Test compile scope.
compile_scope_test() ->
    Plan = ln_plan:scope(my_scope, ln_plan:task(my_task)),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Scopes = maps:get(scopes, Bytecode),
    ?assert(maps:size(Scopes) =:= 1).

%% @doc Test compile nested plans.
compile_nested_test() ->
    Inner = ln_plan:par([ln_plan:task(a), ln_plan:task(b)]),
    Outer = ln_plan:seq([ln_plan:task(init), Inner, ln_plan:task(finalize)]),
    {ok, Bytecode} = ln_compile:compile(Outer),
    ?assertMatch(#{program := _, joins := _, scopes := _}, Bytecode).

%% @doc Test compile invalid plan returns error.
compile_invalid_test() ->
    Plan = {invalid_plan},
    ?assertMatch({error, _}, ln_compile:compile(Plan)).

%% @doc Test verify opcodes.
verify_opcodes_test() ->
    Plan = ln_plan:task(my_task),
    {ok, #{program := Program}} = ln_compile:compile(Plan),
    lists:foreach(fun({_Label, Opcode}) ->
        ?assert(is_tuple(Opcode)),
        ?assert(element(1, Opcode) =:= op_task_start orelse
                  element(1, Opcode) =:= op_task_complete orelse
                  element(1, Opcode) =:= op_halt)
    end, Program).

%% @doc Test verify labels are sequential.
verify_labels_test() ->
    Plan = ln_plan:seq([ln_plan:task(a), ln_plan:task(b)]),
    {ok, #{program := Program}} = ln_compile:compile(Plan),
    Labels = [Label || {Label, _} <- Program],
    ?assertEqual(lists:seq(0, length(Labels) - 1), lists:sort(Labels)).

%% @doc Test verify join table structure.
verify_join_table_test() ->
    Plan = ln_plan:join(all, [ln_plan:task(a), ln_plan:task(b)]),
    {ok, #{joins := Joins}} = ln_compile:compile(Plan),
    [{_JoinId, {_Label, BranchCount, Policy}}] = maps:to_list(Joins),
    ?assertEqual(all, Policy),
    ?assertEqual(2, BranchCount).

%% @doc Test verify scope table structure.
verify_scope_table_test() ->
    Plan = ln_plan:scope(my_scope, ln_plan:task(my_task)),
    {ok, #{scopes := Scopes}} = ln_compile:compile(Plan),
    [{ScopeId, {EnterLabel, ExitLabel}}] = maps:to_list(Scopes),
    ?assertEqual(my_scope, ScopeId),
    ?assert(EnterLabel < ExitLabel).

%% @doc Test entry label is always 0.
verify_entry_label_test() ->
    Plan = ln_plan:seq([ln_plan:task(a)]),
    {ok, Bytecode} = ln_compile:compile(Plan),
    ?assertEqual(0, maps:get(entry_label, Bytecode)).

%% @doc Test compile defer.
compile_defer_test() ->
    Plan = ln_plan:defer([ln_plan:task(a), ln_plan:task(b)]),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Program = maps:get(program, Bytecode),
    ?assert(length(Program) > 0).

%% @doc Test compile loop.
compile_loop_test() ->
    Plan = ln_plan:loop(while, ln_plan:task(my_task)),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Program = maps:get(program, Bytecode),
    ?assert(length(Program) > 0).

%% @doc Test compile multiple instances.
compile_mi_test() ->
    Plan = ln_plan:mi({concurrent, 3}, ln_plan:task(my_task)),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Joins = maps:get(joins, Bytecode),
    ?assert(maps:size(Joins) > 0).

%% @doc Test compile wait.
compile_wait_test() ->
    Plan = ln_plan:wait({signal, pattern}),
    {ok, Bytecode} = ln_compile:compile(Plan),
    Program = maps:get(program, Bytecode),
    ?assert(length(Program) > 0).
