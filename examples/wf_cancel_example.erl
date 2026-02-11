%%% @doc WF Substrate Example - Cancellation Patterns
%%%
%%% Demonstrates cancellation scopes and cancellation semantics in the WF substrate.
%%% Shows region cancellation, activity cancellation, and case cancellation.
%%%
%%% @end
-module(wf_cancel_example).

-export([
    run_region_cancel/0,
    run_activity_cancel/0,
    run_nested_cancel/0,
    workflow_region_cancel/0,
    workflow_activity_cancel/0,
    workflow_nested_cancel/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% WORKFLOW DEFINITIONS =======================================================

%% @doc Workflow with a cancellation region.
%%
%% Pattern: Region containing a sequence of tasks.
%% If region is cancelled, all tasks within halt immediately.
%%
%% @end
-spec workflow_region_cancel() -> wf_term:wf_term().
workflow_region_cancel() ->
    wf_term:cancel_scope(
        {region, critical_region},
        wf_term:seq(
            wf_term:task(prepare, fun task_prepare/1),
            wf_term:seq(
                wf_term:task(process, fun task_process/1),
                wf_term:task(finalize, fun task_finalize/1)
            )
        )
    ).

%% @doc Workflow with activity-level cancellation.
%%
%% Pattern: Parallel branches with cancellable activity scope.
%%
%% @end
-spec workflow_activity_cancel() -> wf_term:wf_term().
workflow_activity_cancel() ->
    wf_term:par([
        wf_term:cancel_scope(
            {activity, long_running_activity},
            wf_term:task(long_task, fun task_long_running/1)
        ),
        wf_term:task(fast_task, fun task_fast/1)
    ]).

%% @doc Workflow with nested cancellation scopes.
%%
%% Pattern: Outer region contains inner region.
%% Cancelling outer should cascade to inner.
%%
%% @end
-spec workflow_nested_cancel() -> wf_term:wf_term().
workflow_nested_cancel() ->
    wf_term:cancel_scope(
        {region, outer_region},
        wf_term:seq(
            wf_term:task(outer_start, fun task_outer_start/1),
            wf_term:seq(
                wf_term:cancel_scope(
                    {region, inner_region},
                    wf_term:seq(
                        wf_term:task(inner_start, fun task_inner_start/1),
                        wf_term:task(inner_work, fun task_inner_work/1)
                    )
                ),
                wf_term:task(outer_end, fun task_outer_end/1)
            )
        )
    ).

%% @doc Workflow demonstrating cancel-on-error pattern.
%%
%% Pattern: Parallel branches where error in one should cancel others.
%%
%% @end
-spec workflow_cancel_on_error() -> wf_term:wf_term().
workflow_cancel_on_error() ->
    wf_term:cancel_scope(
        {region, error_region},
        wf_term:par([
            wf_term:task(may_fail, fun task_may_fail/1),
            wf_term:task(other_work, fun task_other_work/1),
            wf_term:task(cleanup, fun task_cleanup/1)
        ])
    ).

%% @doc Workflow with discriminator + cancel pattern.
%%
%% Pattern: First branch to complete wins, others cancelled.
%%
%% @end
-spec workflow_discriminator_cancel() -> wf_term:wf_term().
workflow_discriminator_cancel() ->
    wf_term:cancel_scope(
        {region, race_region},
        wf_term:discriminator([
            wf_term:task(competitor_a, fun task_competitor_a/1),
            wf_term:task(competitor_b, fun task_competitor_b/1),
            wf_term:task(competitor_c, fun task_competitor_c/1)
        ])
    ).

%%% TASK IMPLEMENTATIONS =======================================================

-spec task_prepare(wf_term:context()) -> {ok, wf_term:context()}.
task_prepare(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Steps = maps:get(steps, Data, []),
    {ok, Ctx#{data => Data#{steps => [prepare | Steps]}}}.

-spec task_process(wf_term:context()) -> {ok, wf_term:context()}.
task_process(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Steps = maps:get(steps, Data, []),
    {ok, Ctx#{data => Data#{steps => [process | Steps]}}}.

-spec task_finalize(wf_term:context()) -> {ok, wf_term:context()}.
task_finalize(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Steps = maps:get(steps, Data, []),
    {ok, Ctx#{data => Data#{steps => [finalize | Steps]}}}.

-spec task_long_running(wf_term:context()) -> {ok, wf_term:context()}.
task_long_running(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{long_running => started}}}.

-spec task_fast(wf_term:context()) -> {ok, wf_term:context()}.
task_fast(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{fast => completed}}}.

-spec task_outer_start(wf_term:context()) -> {ok, wf_term:context()}.
task_outer_start(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Steps = maps:get(steps, Data, []),
    {ok, Ctx#{data => Data#{steps => [outer_start | Steps]}}}.

-spec task_inner_start(wf_term:context()) -> {ok, wf_term:context()}.
task_inner_start(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Steps = maps:get(steps, Data, []),
    {ok, Ctx#{data => Data#{steps => [inner_start | Steps]}}}.

-spec task_inner_work(wf_term:context()) -> {ok, wf_term:context()}.
task_inner_work(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Steps = maps:get(steps, Data, []),
    {ok, Ctx#{data => Data#{steps => [inner_work | Steps]}}}.

-spec task_outer_end(wf_term:context()) -> {ok, wf_term:context()}.
task_outer_end(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Steps = maps:get(steps, Data, []),
    {ok, Ctx#{data => Data#{steps => [outer_end | Steps]}}}.

-spec task_may_fail(wf_term:context()) -> {ok, wf_term:context()} | {error, term()}.
task_may_fail(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    case maps:get(should_fail, Data, false) of
        true ->
            {error, task_failed};
        false ->
            {ok, Ctx#{data => Data#{may_fail => completed}}}
    end.

-spec task_other_work(wf_term:context()) -> {ok, wf_term:context()}.
task_other_work(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{other_work => completed}}}.

-spec task_cleanup(wf_term:context()) -> {ok, wf_term:context()}.
task_cleanup(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{cleanup => completed}}}.

-spec task_competitor_a(wf_term:context()) -> {ok, wf_term:context()}.
task_competitor_a(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{winner => competitor_a}}}.

-spec task_competitor_b(wf_term:context()) -> {ok, wf_term:context()}.
task_competitor_b(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{winner => competitor_b}}}.

-spec task_competitor_c(wf_term:context()) -> {ok, wf_term:context()}.
task_competitor_c(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    {ok, Ctx#{data => Data#{winner => competitor_c}}}.

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

%% @doc Run region cancellation workflow.
-spec run_region_cancel() -> {ok, wf_term:context()} | {error, term()}.
run_region_cancel() ->
    execute_workflow(workflow_region_cancel()).

%% @doc Run activity cancellation workflow.
-spec run_activity_cancel() -> {ok, wf_term:context()} | {error, term()}.
run_activity_cancel() ->
    execute_workflow(workflow_activity_cancel()).

%% @doc Run nested cancellation workflow.
-spec run_nested_cancel() -> {ok, wf_term:context()} | {error, term()}.
run_nested_cancel() ->
    execute_workflow(workflow_nested_cancel()).

%%% TESTS ======================================================================

region_cancel_construction_test() ->
    Pattern = workflow_region_cancel(),
    ?assert(wf_term:is_valid(Pattern)).

activity_cancel_construction_test() ->
    Pattern = workflow_activity_cancel(),
    ?assert(wf_term:is_valid(Pattern)).

nested_cancel_construction_test() ->
    Pattern = workflow_nested_cancel(),
    ?assert(wf_term:is_valid(Pattern)).

cancel_on_error_construction_test() ->
    Pattern = workflow_cancel_on_error(),
    ?assert(wf_term:is_valid(Pattern)).

discriminator_cancel_construction_test() ->
    Pattern = workflow_discriminator_cancel(),
    ?assert(wf_term:is_valid(Pattern)).

region_cancel_compilation_test() ->
    Pattern = workflow_region_cancel(),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assert(wf_compile:is_compiled(Compiled)),
    Size = wf_compile:program_size(Compiled),
    ?assert(Size > 0).

region_cancel_execution_test() ->
    {ok, Result} = run_region_cancel(),
    Data = maps:get(data, Result),
    Steps = maps:get(steps, Data, []),
    %% All tasks should execute (no cancellation triggered)
    ?assert(lists:member(prepare, Steps)),
    ?assert(lists:member(process, Steps)),
    ?assert(lists:member(finalize, Steps)).

activity_cancel_execution_test() ->
    {ok, Result} = run_activity_cancel(),
    Data = maps:get(data, Result),
    %% Both tasks should execute in parallel
    ?assertEqual(completed, maps:get(fast, Data, undefined)).

nested_cancel_execution_test() ->
    {ok, Result} = run_nested_cancel(),
    Data = maps:get(data, Result),
    Steps = maps:get(steps, Data, []),
    %% All nested tasks should execute
    ?assert(lists:member(outer_start, Steps)),
    ?assert(lists:member(inner_start, Steps)),
    ?assert(lists:member(inner_work, Steps)),
    ?assert(lists:member(outer_end, Steps)).

cancel_scope_structure_test() ->
    %% Verify cancel scope structure
    Pattern = wf_term:cancel_scope(
        {region, test_region},
        wf_term:task(test_task, fun(_) -> {ok, #{}} end)
    ),
    ?assertMatch({cancel_scope, {region, test_region}, _}, Pattern).

multiple_scopes_test() ->
    %% Verify multiple independent cancel scopes
    Pattern = wf_term:par([
        wf_term:cancel_scope(
            {region, region_a},
            wf_term:task(task_a, fun task_prepare/1)
        ),
        wf_term:cancel_scope(
            {region, region_b},
            wf_term:task(task_b, fun task_process/1)
        )
    ]),
    ?assert(wf_term:is_valid(Pattern)),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assert(wf_compile:is_compiled(Compiled)).

scope_types_test() ->
    %% Test all scope types
    RegionScope = wf_term:cancel_scope({region, r}, wf_term:task(t, fun(_) -> {ok, #{}} end)),
    ActivityScope = wf_term:cancel_scope({activity, a}, wf_term:task(t, fun(_) -> {ok, #{}} end)),
    CaseScope = wf_term:cancel_scope({root_case, c}, wf_term:task(t, fun(_) -> {ok, #{}} end)),

    ?assert(wf_term:is_valid(RegionScope)),
    ?assert(wf_term:is_valid(ActivityScope)),
    ?assert(wf_term:is_valid(CaseScope)).
