%%% @doc WF Substrate Example - Parallel Execution with Various Joins
%%%
%%% Demonstrates parallel split and various join patterns (all, xor_merge,
%%% sync_merge, first_n, n_of_m) using the WF substrate.
%%%
%%% @end
-module(wf_parallel_join).

-export([
    run_par_all/0,
    run_simple_merge/0,
    run_sync_merge/0,
    run_discriminator/0,
    run_n_out_of_m/0,
    workflow_par_all/0,
    workflow_simple_merge/0,
    workflow_sync_merge/0,
    workflow_discriminator/0,
    workflow_n_out_of_m/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% WORKFLOW DEFINITIONS =======================================================

%% @doc Parallel split with AND-join (wait for all branches).
-spec workflow_par_all() -> wf_term:wf_term().
workflow_par_all() ->
    wf_term:par([
        wf_term:task(branch_a, fun task_branch_a/1),
        wf_term:task(branch_b, fun task_branch_b/1),
        wf_term:task(branch_c, fun task_branch_c/1)
    ]).

%% @doc Simple merge (XOR-join, take first available).
-spec workflow_simple_merge() -> wf_term:wf_term().
workflow_simple_merge() ->
    wf_term:simple_merge([
        wf_term:task(option_1, fun task_option_1/1),
        wf_term:task(option_2, fun task_option_2/1),
        wf_term:task(option_3, fun task_option_3/1)
    ]).

%% @doc Synchronizing merge (coordinate branches before merging).
-spec workflow_sync_merge() -> wf_term:wf_term().
workflow_sync_merge() ->
    wf_term:synchronizing_merge([
        wf_term:task(sync_a, fun task_sync_a/1),
        wf_term:task(sync_b, fun task_sync_b/1)
    ]).

%% @doc Discriminator (proceed on first branch, cancel others).
-spec workflow_discriminator() -> wf_term:wf_term().
workflow_discriminator() ->
    wf_term:discriminator([
        wf_term:task(race_a, fun task_race_a/1),
        wf_term:task(race_b, fun task_race_b/1),
        wf_term:task(race_c, fun task_race_c/1)
    ]).

%% @doc N-out-of-M join (wait for 2 out of 4 branches).
-spec workflow_n_out_of_m() -> wf_term:wf_term().
workflow_n_out_of_m() ->
    wf_term:n_out_of_m(2, [
        wf_term:task(worker_1, fun task_worker_1/1),
        wf_term:task(worker_2, fun task_worker_2/1),
        wf_term:task(worker_3, fun task_worker_3/1),
        wf_term:task(worker_4, fun task_worker_4/1)
    ]).

%%% TASK IMPLEMENTATIONS =======================================================

%% Parallel branch tasks
-spec task_branch_a(wf_term:context()) -> {ok, wf_term:context()}.
task_branch_a(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Results = maps:get(results, Data, []),
    {ok, Ctx#{data => Data#{results => [branch_a | Results]}}}.

-spec task_branch_b(wf_term:context()) -> {ok, wf_term:context()}.
task_branch_b(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Results = maps:get(results, Data, []),
    {ok, Ctx#{data => Data#{results => [branch_b | Results]}}}.

-spec task_branch_c(wf_term:context()) -> {ok, wf_term:context()}.
task_branch_c(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Results = maps:get(results, Data, []),
    {ok, Ctx#{data => Data#{results => [branch_c | Results]}}}.

%% Simple merge option tasks
-spec task_option_1(wf_term:context()) -> {ok, wf_term:context()}.
task_option_1(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{chosen => option_1}}}.

-spec task_option_2(wf_term:context()) -> {ok, wf_term:context()}.
task_option_2(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{chosen => option_2}}}.

-spec task_option_3(wf_term:context()) -> {ok, wf_term:context()}.
task_option_3(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{chosen => option_3}}}.

%% Synchronizing merge tasks
-spec task_sync_a(wf_term:context()) -> {ok, wf_term:context()}.
task_sync_a(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{sync_a => completed}}}.

-spec task_sync_b(wf_term:context()) -> {ok, wf_term:context()}.
task_sync_b(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{sync_b => completed}}}.

%% Discriminator race tasks
-spec task_race_a(wf_term:context()) -> {ok, wf_term:context()}.
task_race_a(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{winner => race_a}}}.

-spec task_race_b(wf_term:context()) -> {ok, wf_term:context()}.
task_race_b(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{winner => race_b}}}.

-spec task_race_c(wf_term:context()) -> {ok, wf_term:context()}.
task_race_c(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{winner => race_c}}}.

%% N-out-of-M worker tasks
-spec task_worker_1(wf_term:context()) -> {ok, wf_term:context()}.
task_worker_1(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Workers = maps:get(workers, Data, []),
    {ok, Ctx#{data => Data#{workers => [worker_1 | Workers]}}}.

-spec task_worker_2(wf_term:context()) -> {ok, wf_term:context()}.
task_worker_2(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Workers = maps:get(workers, Data, []),
    {ok, Ctx#{data => Data#{workers => [worker_2 | Workers]}}}.

-spec task_worker_3(wf_term:context()) -> {ok, wf_term:context()}.
task_worker_3(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Workers = maps:get(workers, Data, []),
    {ok, Ctx#{data => Data#{workers => [worker_3 | Workers]}}}.

-spec task_worker_4(wf_term:context()) -> {ok, wf_term:context()}.
task_worker_4(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Workers = maps:get(workers, Data, []),
    {ok, Ctx#{data => Data#{workers => [worker_4 | Workers]}}}.

%%% EXECUTION ==================================================================

%% @doc Helper to execute a workflow pattern.
-spec execute_workflow(wf_term:wf_term()) -> {ok, wf_term:context()} | {error, term()}.
execute_workflow(Pattern) ->
    case wf_compile:compile(Pattern) of
        {ok, Compiled} ->
            InitCtx = #{data => #{}},
            ExecState = wf_exec:exec_init(Compiled, InitCtx),
            case wf_exec:exec_until_halt(ExecState) of
                {halt, ok, FinalState} ->
                    wf_exec:get_result(FinalState);
                {error, Reason, _State} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {compilation_failed, Reason}}
    end.

%% @doc Run parallel AND-join workflow.
-spec run_par_all() -> {ok, wf_term:context()} | {error, term()}.
run_par_all() ->
    execute_workflow(workflow_par_all()).

%% @doc Run simple merge workflow.
-spec run_simple_merge() -> {ok, wf_term:context()} | {error, term()}.
run_simple_merge() ->
    execute_workflow(workflow_simple_merge()).

%% @doc Run synchronizing merge workflow.
-spec run_sync_merge() -> {ok, wf_term:context()} | {error, term()}.
run_sync_merge() ->
    execute_workflow(workflow_sync_merge()).

%% @doc Run discriminator workflow.
-spec run_discriminator() -> {ok, wf_term:context()} | {error, term()}.
run_discriminator() ->
    execute_workflow(workflow_discriminator()).

%% @doc Run N-out-of-M join workflow.
-spec run_n_out_of_m() -> {ok, wf_term:context()} | {error, term()}.
run_n_out_of_m() ->
    execute_workflow(workflow_n_out_of_m()).

%%% TESTS ======================================================================

par_all_construction_test() ->
    Pattern = workflow_par_all(),
    ?assert(wf_term:is_valid(Pattern)),
    ?assertEqual(4, wf_term:term_size(Pattern)).

simple_merge_construction_test() ->
    Pattern = workflow_simple_merge(),
    ?assert(wf_term:is_valid(Pattern)).

sync_merge_construction_test() ->
    Pattern = workflow_sync_merge(),
    ?assert(wf_term:is_valid(Pattern)).

discriminator_construction_test() ->
    Pattern = workflow_discriminator(),
    ?assert(wf_term:is_valid(Pattern)).

n_out_of_m_construction_test() ->
    Pattern = workflow_n_out_of_m(),
    ?assert(wf_term:is_valid(Pattern)),
    ?assertEqual(5, wf_term:term_size(Pattern)).

par_all_execution_test() ->
    {ok, Result} = run_par_all(),
    Data = maps:get(data, Result),
    Results = maps:get(results, Data, []),
    %% All three branches should have executed
    ?assertEqual(3, length(Results)),
    ?assert(lists:member(branch_a, Results)),
    ?assert(lists:member(branch_b, Results)),
    ?assert(lists:member(branch_c, Results)).

simple_merge_compilation_test() ->
    Pattern = workflow_simple_merge(),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assert(wf_compile:is_compiled(Compiled)).

discriminator_compilation_test() ->
    Pattern = workflow_discriminator(),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assert(wf_compile:is_compiled(Compiled)).

n_out_of_m_join_policy_test() ->
    %% Verify that n_out_of_m creates proper join policy
    Pattern = wf_term:n_out_of_m(3, [
        wf_term:task(t1, fun(_) -> {ok, #{}} end),
        wf_term:task(t2, fun(_) -> {ok, #{}} end),
        wf_term:task(t3, fun(_) -> {ok, #{}} end),
        wf_term:task(t4, fun(_) -> {ok, #{}} end)
    ]),
    ?assertMatch({join, {n_of_m, 3, 4}, _}, Pattern).

complex_parallel_test() ->
    %% Complex pattern: parallel branches with nested sequences
    Pattern = wf_term:par([
        wf_term:seq(
            wf_term:task(a1, fun task_branch_a/1),
            wf_term:task(a2, fun task_branch_a/1)
        ),
        wf_term:seq(
            wf_term:task(b1, fun task_branch_b/1),
            wf_term:task(b2, fun task_branch_b/1)
        )
    ]),
    ?assert(wf_term:is_valid(Pattern)),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assert(wf_compile:is_compiled(Compiled)).
