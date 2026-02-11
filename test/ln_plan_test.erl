%%%-------------------------------------------------------------------
%%% @doc EUnit tests for ln_plan module.
%%% @end
%%%-------------------------------------------------------------------
-module(ln_plan_test).
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Generators
%%%-------------------------------------------------------------------

%% @doc Test task/1 constructor.
task_constructor_test() ->
    Plan = ln_plan:task(my_task),
    ?assertEqual({task, my_task}, Plan).

%% @doc Test seq/1 constructor.
seq_constructor_test() ->
    Plans = [ln_plan:task(a), ln_plan:task(b)],
    Plan = ln_plan:seq(Plans),
    ?assertMatch({seq, _}, Plan).

%% @doc Test seq/1 with empty list raises error.
seq_empty_test() ->
    ?assertError({empty_seq, _}, ln_plan:seq([])).

%% @doc Test par/1 constructor.
par_constructor_test() ->
    Plans = [ln_plan:task(a), ln_plan:task(b)],
    Plan = ln_plan:par(Plans),
    ?assertMatch({par, _}, Plan).

%% @doc Test par/1 with empty list raises error.
par_empty_test() ->
    ?assertError({empty_par, _}, ln_plan:par([])).

%% @doc Test xor/1 constructor.
xor_constructor_test() ->
    Plans = [ln_plan:task(a), ln_plan:task(b)],
    Plan = ln_plan:xor(Plans),
    ?assertMatch({xor, _}, Plan).

%% @doc Test xor/1 with empty list raises error.
xor_empty_test() ->
    ?assertError({empty_xor, _}, ln_plan:xor([])).

%% @doc Test join/2 constructor.
join_constructor_test() ->
    Plans = [ln_plan:task(a), ln_plan:task(b)],
    Plan = ln_plan:join(all, Plans),
    ?assertMatch({join, all, _}, Plan).

%% @doc Test join/2 with n_of_m policy.
join_n_of_m_test() ->
    Plans = [ln_plan:task(a), ln_plan:task(b), ln_plan:task(c)],
    Plan = ln_plan:join({n_of_m, 2}, Plans),
    ?assertMatch({join, {n_of_m, 2}, _}, Plan).

%% @doc Test join/2 with empty list raises error.
join_empty_test() ->
    ?assertError({empty_join, _}, ln_plan:join(all, [])).

%% @doc Test loop/2 constructor.
loop_constructor_test() ->
    InnerPlan = ln_plan:task(my_task),
    Plan = ln_plan:loop(while, InnerPlan),
    ?assertMatch({loop, while, _}, Plan).

%% @doc Test defer/1 constructor.
defer_constructor_test() ->
    Plans = [ln_plan:task(a), ln_plan:task(b)],
    Plan = ln_plan:defer(Plans),
    ?assertMatch({defer, _}, Plan).

%% @doc Test defer/1 with empty list raises error.
defer_empty_test() ->
    ?assertError({empty_defer, _}, ln_plan:defer([])).

%% @doc Test scope/2 constructor.
scope_constructor_test() ->
    InnerPlan = ln_plan:task(my_task),
    Plan = ln_plan:scope(my_scope, InnerPlan),
    ?assertMatch({scope, my_scope, _}, Plan).

%% @doc Test mi/2 constructor.
mi_constructor_test() ->
    InnerPlan = ln_plan:task(my_task),
    Plan = ln_plan:mi({concurrent, 3}, InnerPlan),
    ?assertMatch({mi, {concurrent, 3}, _}, Plan).

%% @doc Test wait/1 constructor.
wait_constructor_test() ->
    Plan = ln_plan:wait({signal, pattern}),
    ?assertMatch({wait, _}, Plan).

%% @doc Test validate/1 with valid task.
validate_task_test() ->
    Plan = ln_plan:task(my_task),
    ?assertEqual(ok, ln_plan:validate(Plan)).

%% @doc Test validate/1 with valid seq.
validate_seq_test() ->
    Plan = ln_plan:seq([ln_plan:task(a), ln_plan:task(b)]),
    ?assertEqual(ok, ln_plan:validate(Plan)).

%% @doc Test validate/1 with valid par.
validate_par_test() ->
    Plan = ln_plan:par([ln_plan:task(a), ln_plan:task(b)]),
    ?assertEqual(ok, ln_plan:validate(Plan)).

%% @doc Test validate/1 with valid xor.
validate_xor_test() ->
    Plan = ln_plan:xor([ln_plan:task(a), ln_plan:task(b)]),
    ?assertEqual(ok, ln_plan:validate(Plan)).

%% @doc Test validate/1 with valid join.
validate_join_test() ->
    Plan = ln_plan:join(all, [ln_plan:task(a), ln_plan:task(b)]),
    ?assertEqual(ok, ln_plan:validate(Plan)).

%% @doc Test validate/1 with valid scope.
validate_scope_test() ->
    Plan = ln_plan:scope(my_scope, ln_plan:task(my_task)),
    ?assertEqual(ok, ln_plan:validate(Plan)).

%% @doc Test validate/1 with invalid plan.
validate_invalid_test() ->
    ?assertMatch({error, {invalid_plan, _}}, ln_plan:validate({invalid, term})).

%% @doc Test nested seq validation.
validate_nested_seq_test() ->
    Inner = ln_plan:seq([ln_plan:task(a), ln_plan:task(b)]),
    Outer = ln_plan:seq([Inner, ln_plan:task(c)]),
    ?assertEqual(ok, ln_plan:validate(Outer)).

%% @doc Test nested par validation.
validate_nested_par_test() ->
    Inner = ln_plan:par([ln_plan:task(a), ln_plan:task(b)]),
    Outer = ln_plan:par([Inner, ln_plan:task(c)]),
    ?assertEqual(ok, ln_plan:validate(Outer)).

%% @doc Test complex nested plan validation.
validate_complex_test() ->
    Plan = ln_plan:seq([
        ln_plan:task(init),
        ln_plan:par([
            ln_plan:task(a),
            ln_plan:scope(s1, ln_plan:xor([ln_plan:task(b), ln_plan:task(c)]))
        ]),
        ln_plan:join(all, [ln_plan:task(d), ln_plan:task(e)])
    ]),
    ?assertEqual(ok, ln_plan:validate(Plan)).
