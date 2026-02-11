%%% @doc WF Substrate Example - Simple Sequential Workflow
%%%
%%% Demonstrates basic sequential pattern execution using the WF substrate.
%%% Shows task creation, sequencing, context propagation, and compilation.
%%%
%%% @end
-module(wf_simple_sequence).

-export([
    run/0,
    run_with_data/1,
    workflow/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% WORKFLOW DEFINITION ========================================================

%% @doc Define a simple sequential workflow.
%%
%% Pattern: Task A -> Task B -> Task C
%% Each task increments a counter in the context.
%%
%% @end
-spec workflow() -> wf_term:wf_term().
workflow() ->
    wf_term:seq(
        wf_term:task(task_a, fun task_a/1),
        wf_term:seq(
            wf_term:task(task_b, fun task_b/1),
            wf_term:task(task_c, fun task_c/1)
        )
    ).

%%% TASK IMPLEMENTATIONS =======================================================

-spec task_a(wf_term:context()) -> {ok, wf_term:context()}.
task_a(Ctx) ->
    Counter = maps:get(counter, maps:get(data, Ctx, #{}), 0),
    NewCounter = Counter + 1,
    Data = #{counter => NewCounter, last_task => task_a},
    {ok, Ctx#{data => Data}}.

-spec task_b(wf_term:context()) -> {ok, wf_term:context()}.
task_b(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Counter = maps:get(counter, Data, 0),
    NewCounter = Counter + 10,
    NewData = Data#{counter => NewCounter, last_task => task_b},
    {ok, Ctx#{data => NewData}}.

-spec task_c(wf_term:context()) -> {ok, wf_term:context()}.
task_c(Ctx) ->
    Data = maps:get(data, Ctx, #{}),
    Counter = maps:get(counter, Data, 0),
    NewCounter = Counter + 100,
    NewData = Data#{counter => NewCounter, last_task => task_c},
    {ok, Ctx#{data => NewData}}.

%%% EXECUTION ==================================================================

%% @doc Run the workflow with default empty context.
-spec run() -> {ok, wf_term:context()} | {error, term()}.
run() ->
    run_with_data(#{}).

%% @doc Run the workflow with initial data.
-spec run_with_data(Data :: map()) -> {ok, wf_term:context()} | {error, term()}.
run_with_data(Data) ->
    %% Build the workflow pattern
    Pattern = workflow(),

    %% Compile to bytecode
    case wf_compile:compile(Pattern) of
        {ok, Compiled} ->
            %% Initialize execution state
            InitCtx = #{data => Data},
            ExecState = wf_exec:exec_init(Compiled, InitCtx),

            %% Execute until halt
            case wf_exec:exec_until_halt(ExecState) of
                {halt, ok, FinalState} ->
                    wf_exec:get_result(FinalState);
                {error, Reason, _State} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {compilation_failed, Reason}}
    end.

%%% TESTS ======================================================================

workflow_construction_test() ->
    Pattern = workflow(),
    ?assert(wf_term:is_valid(Pattern)),
    ?assertEqual(7, wf_term:term_size(Pattern)).

compilation_test() ->
    Pattern = workflow(),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assert(wf_compile:is_compiled(Compiled)),
    Size = wf_compile:program_size(Compiled),
    ?assert(Size > 0).

execution_test() ->
    {ok, Result} = run(),
    Data = maps:get(data, Result),
    %% Counter should be: 0 + 1 + 10 + 100 = 111
    ?assertEqual(111, maps:get(counter, Data)),
    ?assertEqual(task_c, maps:get(last_task, Data)).

execution_with_initial_data_test() ->
    {ok, Result} = run_with_data(#{counter => 5, initial => true}),
    Data = maps:get(data, Result),
    %% Counter should be: 5 + 1 + 10 + 100 = 116
    ?assertEqual(116, maps:get(counter, Data)),
    ?assertEqual(task_c, maps:get(last_task, Data)),
    ?assertEqual(true, maps:get(initial, Data)).

task_execution_order_test() ->
    Pattern = workflow(),
    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}},
    ExecState = wf_exec:exec_init(Compiled, InitCtx),

    %% Execute step by step and verify trace
    {FinalState, _StepsExecuted} = wf_exec:exec_steps(ExecState, 1000),
    Trace = wf_vm:exec_trace(FinalState),

    %% Should have trace events
    ?assert(length(Trace) > 0).

error_handling_test() ->
    %% Test with a task that fails
    FailTask = wf_term:task(fail_task, fun(_) -> {error, intentional_failure} end),
    Pattern = wf_term:seq(wf_term:task(task_a, fun task_a/1), FailTask),

    {ok, Compiled} = wf_compile:compile(Pattern),
    InitCtx = #{data => #{}},
    ExecState = wf_exec:exec_init(Compiled, InitCtx),

    case wf_exec:exec_until_halt(ExecState) of
        {error, intentional_failure, _State} ->
            ok;
        Other ->
            ?assertMatch({error, intentional_failure, _}, Other)
    end.
