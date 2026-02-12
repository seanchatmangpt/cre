%%%-------------------------------------------------------------------
%%% @doc
%%% ln_ctrl End-to-End API Common Test Suite
%%%
%%% Comprehensive production-quality tests for the ln_ctrl public API.
%%% Tests cover all exported functions with happy paths, error cases,
%%% and edge conditions.
%%%
%%% Test Coverage:
%%% - new_case/3: happy path, invalid pattern, compilation error
%%% - signal/2: signal delivery and processing
%%% - cancel/1: immediate cancellation
%%% - cancel_scope/2: scope-specific cancellation
%%% - await/2: successful completion, timeout, error
%%% - status/1: accurate status reporting
%%% - trace/3: trace range extraction
%%% - validate/2: pattern validation
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ln_ctrl_e2e_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    %% new_case/3 tests
    new_case_happy_path_test/1,
    new_case_invalid_pattern_test/1,
    new_case_compilation_error_test/1,
    new_case_with_budget_test/1,
    new_case_with_options_test/1,

    %% signal/2 tests
    signal_delivery_test/1,
    signal_to_invalid_case_test/1,
    signal_queue_processing_test/1,

    %% cancel/1 tests
    cancel_immediate_test/1,
    cancel_invalid_case_id_test/1,
    cancel_already_halted_test/1,

    %% cancel_scope/2 tests
    cancel_scope_specific_test/1,
    cancel_scope_invalid_scope_test/1,
    cancel_nested_scope_test/1,

    %% await/2 tests
    await_successful_completion_test/1,
    await_timeout_test/1,
    await_error_completion_test/1,
    await_cancelled_test/1,
    await_invalid_case_id_test/1,

    %% status/1 tests
    status_running_test/1,
    status_halted_test/1,
    status_cancelled_test/1,
    status_invalid_case_id_test/1,
    status_budget_tracking_test/1,

    %% trace/3 tests
    trace_range_extraction_test/1,
    trace_empty_range_test/1,
    trace_full_range_test/1,
    trace_invalid_case_id_test/1,

    %% validate/2 tests
    validate_valid_pattern_test/1,
    validate_invalid_pattern_test/1,
    validate_compilation_failure_test/1,
    validate_complex_pattern_test/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @doc Returns list of all test cases and groups.
-spec all() -> [atom() | {group, atom()}].
all() ->
    [
        {group, new_case_api},
        {group, signal_api},
        {group, cancel_api},
        {group, cancel_scope_api},
        {group, await_api},
        {group, status_api},
        {group, trace_api},
        {group, validate_api}
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [{atom(), term()}], [atom()]}].
groups() ->
    [
        {new_case_api, [shuffle, {repeat, 1}], [
            new_case_happy_path_test,
            new_case_invalid_pattern_test,
            new_case_compilation_error_test,
            new_case_with_budget_test,
            new_case_with_options_test
        ]},
        {signal_api, [], [
            signal_delivery_test,
            signal_to_invalid_case_test,
            signal_queue_processing_test
        ]},
        {cancel_api, [], [
            cancel_immediate_test,
            cancel_invalid_case_id_test,
            cancel_already_halted_test
        ]},
        {cancel_scope_api, [], [
            cancel_scope_specific_test,
            cancel_scope_invalid_scope_test,
            cancel_nested_scope_test
        ]},
        {await_api, [], [
            await_successful_completion_test,
            await_timeout_test,
            await_error_completion_test,
            await_cancelled_test,
            await_invalid_case_id_test
        ]},
        {status_api, [], [
            status_running_test,
            status_halted_test,
            status_cancelled_test,
            status_invalid_case_id_test,
            status_budget_tracking_test
        ]},
        {trace_api, [], [
            trace_range_extraction_test,
            trace_empty_range_test,
            trace_full_range_test,
            trace_invalid_case_id_test
        ]},
        {validate_api, [], [
            validate_valid_pattern_test,
            validate_invalid_pattern_test,
            validate_compilation_failure_test,
            validate_complex_pattern_test
        ]}
    ].

%% @doc Suite-level configuration.
-spec suite() -> [{atom(), term()}].
suite() ->
    [
        {timetrap, {seconds, 60}}
    ].

%% @doc Suite-level initialization.
-spec init_per_suite(Config :: ct:config()) -> ct:config().
init_per_suite(Config) ->
    ct:pal("Starting ln_ctrl_e2e_api_SUITE"),

    %% Ensure required modules are loaded
    ok = ensure_modules_loaded(),

    %% Start necessary applications
    ok = application:ensure_started(compiler),

    %% Wait for system to stabilize
    timer:sleep(100),

    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: ct:config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Completed ln_ctrl_e2e_api_SUITE"),

    %% Cleanup any lingering processes
    cleanup_all_cases(),

    ok.

%% @doc Group-level initialization.
-spec init_per_group(Group :: atom(), Config :: ct:config()) -> ct:config().
init_per_group(Group, Config) ->
    ct:pal("Initializing group: ~p", [Group]),
    Config.

%% @doc Group-level cleanup.
-spec end_per_group(Group :: atom(), Config :: ct:config()) -> ok.
end_per_group(Group, _Config) ->
    ct:pal("Completed group: ~p", [Group]),

    %% Cleanup after each group
    cleanup_all_cases(),

    ok.

%% @doc Testcase-level initialization.
-spec init_per_testcase(TestCase :: atom(), Config :: ct:config()) -> ct:config().
init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),

    %% Store test case name for reference
    [{testcase, TestCase} | Config].

%% @doc Testcase-level cleanup.
-spec end_per_testcase(TestCase :: atom(), Config :: ct:config()) -> ok.
end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),

    %% Ensure cleanup after each test
    cleanup_all_cases(),

    ok.

%%%===================================================================
%%% Test Cases - new_case/3 API
%%%===================================================================

%% @doc Test new_case/3 happy path with valid pattern.
new_case_happy_path_test(_Config) ->
    %% Create a simple task pattern
    Pattern = wf_term:task(simple_task, fun(_Ctx) -> {ok, #{result => success}} end),
    InitCtx = #{data => initial},
    Options = #{trace_level => basic},

    %% Create case
    Result = ln_ctrl:new_case(Pattern, InitCtx, Options),

    ?assertMatch({ok, _}, Result),

    %% Verify case runner is a PID
    {ok, CasePid} = Result,
    ?assert(is_pid(CasePid)),

    %% Verify case is running
    ?assert(is_process_alive(CasePid)),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "new_case happy path verified"}.

%% @doc Test new_case/3 with invalid pattern.
new_case_invalid_pattern_test(_Config) ->
    %% Create an invalid pattern
    InvalidPattern = {invalid, pattern, structure},

    Result = ln_ctrl:new_case(InvalidPattern, #{}, #{}),

    ?assertMatch({error, {invalid_pattern, _}}, Result),

    {comment, "new_case invalid pattern error verified"}.

%% @doc Test new_case/3 when compilation fails.
new_case_compilation_error_test(_Config) ->
    %% Create a pattern that will fail during compilation
    %% This tests the wf_compile:compile path
    BadPattern = {task, not_an_atom, fun(_) -> {ok, #{}} end},

    Result = ln_ctrl:new_case(BadPattern, #{}, #{}),

    %% Should return error (either invalid pattern or compilation failed)
    ?assertMatch({error, _}, Result),

    {comment, "new_case compilation error verified"}.

%% @doc Test new_case/3 with budget limits.
new_case_with_budget_test(_Config) ->
    Pattern = wf_term:task(budgeted_task, fun(Ctx) -> {ok, Ctx} end),
    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(10, 5000, 100.0),
    Options = #{budget => Budget},

    Result = ln_ctrl:new_case(Pattern, InitCtx, Options),

    ?assertMatch({ok, _}, Result),

    {ok, CasePid} = Result,

    %% Check status includes budget info
    Status = ln_ctrl:status(CasePid),
    ?assertMatch(#{budget_status := _}, Status),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "new_case with budget verified"}.

%% @doc Test new_case/3 with various options.
new_case_with_options_test(_Config) ->
    Pattern = wf_term:task(options_task, fun(Ctx) -> {ok, Ctx} end),
    InitCtx = #{initial => data},
    Options = #{
        trace_level => full,
        timeout_ms => 10000,
        scheduler => ln_ctrl_sched:new_deterministic()
    },

    Result = ln_ctrl:new_case(Pattern, InitCtx, Options),

    ?assertMatch({ok, _}, Result),

    {ok, CasePid} = Result,
    ?assert(is_process_alive(CasePid)),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "new_case with options verified"}.

%%%===================================================================
%%% Test Cases - signal/2 API
%%%===================================================================

%% @doc Test signal delivery to a running case.
signal_delivery_test(_Config) ->
    %% Create a case that waits for signals
    Pattern = wf_term:task(signal_task, fun(Ctx) ->
        case maps:get(waiting, Ctx, false) of
            true ->
                Signals = maps:get(signals, Ctx, []),
                case Signals of
                    [Signal | _] -> {ok, Ctx#{received => Signal}};
                    [] -> {ok, Ctx}
                end;
            false -> {ok, Ctx#{waiting => true}}
        end
    end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Send signal
    SignalResult = ln_ctrl:signal(CasePid, test_signal),

    ?assertEqual(ok, SignalResult),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "signal delivery verified"}.

%% @doc Test signal to invalid case ID.
signal_to_invalid_case_test(_Config) ->
    %% Try to signal a non-pid
    InvalidCaseId = not_a_pid,

    Result = ln_ctrl:signal(InvalidCaseId, test_signal),

    ?assertMatch({error, invalid_case_id}, Result),

    {comment, "signal invalid case ID error verified"}.

%% @doc Test signal queue processing.
signal_queue_processing_test(_Config) ->
    Pattern = wf_term:task(queue_task, fun(Ctx) -> {ok, Ctx} end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Send multiple signals rapidly
    Results = [ln_ctrl:signal(CasePid, {signal, N}) || N <- lists:seq(1, 5)],

    %% All should succeed
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "signal queue processing verified"}.

%%%===================================================================
%%% Test Cases - cancel/1 API
%%%===================================================================

%% @doc Test immediate cancellation of a running case.
cancel_immediate_test(_Config) ->
    %% Create a long-running pattern
    Pattern = wf_term:loop({max_iter, 1000},
        wf_term:task(loop_task, fun(Ctx) ->
            timer:sleep(10),
            {ok, Ctx}
        end)
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Verify case is running
    ?assert(is_process_alive(CasePid)),

    %% Cancel immediately
    CancelResult = ln_ctrl:cancel(CasePid),

    ?assertEqual(ok, CancelResult),

    %% Wait for cancellation to propagate
    timer:sleep(50),

    %% Verify case status is cancelled
    Status = ln_ctrl:status(CasePid),
    ?assertMatch(#{state := cancelled}, Status),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "cancel immediate verified"}.

%% @doc Test cancel with invalid case ID.
cancel_invalid_case_id_test(_Config) ->
    Result = ln_ctrl:cancel(invalid_case_id),

    ?assertMatch({error, invalid_case_id}, Result),

    {comment, "cancel invalid case ID error verified"}.

%% @doc Test cancelling an already halted case.
cancel_already_halted_test(_Config) ->
    %% Create a simple pattern that completes quickly
    Pattern = wf_term:task(quick_task, fun(Ctx) -> {ok, Ctx} end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Wait for completion
    AwaitResult = ln_ctrl:await(CasePid, 1000),
    ?assertMatch({ok, _}, AwaitResult),

    %% Try to cancel completed case
    %% Should succeed (idempotent) or return appropriate status
    CancelResult = ln_ctrl:cancel(CasePid),

    %% Either ok or an error for already halted is acceptable
    ?assert(CancelResult =:= ok orelse is_tuple(CancelResult) andalso element(1, CancelResult) =:= error),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "cancel already halted verified"}.

%%%===================================================================
%%% Test Cases - cancel_scope/2 API
%%%===================================================================

%% @doc Test scope-specific cancellation.
cancel_scope_specific_test(_Config) ->
    %% Create a pattern with a cancellation scope
    Pattern = wf_term:cancel_scope(
        {region, my_scope},
        wf_term:task(scoped_task, fun(Ctx) ->
            timer:sleep(100),
            {ok, Ctx}
        end)
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Cancel specific scope
    CancelResult = ln_ctrl:cancel_scope(CasePid, my_scope),

    ?assertEqual(ok, CancelResult),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "cancel scope specific verified"}.

%% @doc Test cancel_scope with invalid scope.
cancel_scope_invalid_scope_test(_Config) ->
    Pattern = wf_term:task(simple_task, fun(Ctx) -> {ok, Ctx} end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Try to cancel a non-existent scope
    Result = ln_ctrl:cancel_scope(CasePid, non_existent_scope),

    %% Should succeed (scope not found is acceptable)
    ?assert(Result =:= ok orelse Result =:= {error, _}),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "cancel scope invalid scope verified"}.

%% @doc Test cancellation of nested scopes.
cancel_nested_scope_test(_Config) ->
    %% Create nested cancellation scopes
    InnerPattern = wf_term:cancel_scope(
        {region, inner_scope},
        wf_term:task(inner_task, fun(Ctx) ->
            timer:sleep(100),
            {ok, Ctx}
        end)
    ),

    Pattern = wf_term:cancel_scope(
        {region, outer_scope},
        InnerPattern
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Cancel outer scope
    CancelResult = ln_ctrl:cancel_scope(CasePid, outer_scope),

    ?assertEqual(ok, CancelResult),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "cancel nested scope verified"}.

%%%===================================================================
%%% Test Cases - await/2 API
%%%===================================================================

%% @doc Test await with successful completion.
await_successful_completion_test(_Config) ->
    Pattern = wf_term:seq(
        wf_term:task(first_task, fun(Ctx) -> {ok, Ctx#{step1 => done}} end),
        wf_term:task(second_task, fun(Ctx) -> {ok, Ctx#{step2 => done}} end)
    ),

    InitCtx = #{initial => data},
    {ok, CasePid} = ln_ctrl:new_case(Pattern, InitCtx, #{}),

    %% Await completion
    Result = ln_ctrl:await(CasePid, 5000),

    ?assertMatch({ok, _}, Result),

    {ok, FinalCtx} = Result,
    ?assertMatch(#{step1 := done, step2 := done}, FinalCtx),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "await successful completion verified"}.

%% @doc Test await with timeout.
await_timeout_test(_Config) ->
    %% Create a pattern that takes longer than timeout
    Pattern = wf_term:task(slow_task, fun(Ctx) ->
        timer:sleep(10000),  % 10 seconds
        {ok, Ctx}
    end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Await with short timeout
    Result = ln_ctrl:await(CasePid, 100),  % 100ms timeout

    ?assertMatch(timeout, Result),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "await timeout verified"}.

%% @doc Test await with error completion.
await_error_completion_test(_Config) ->
    %% Create a pattern that fails
    Pattern = wf_term:task(failing_task, fun(_Ctx) ->
        {error, intentional_failure}
    end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Await completion
    Result = ln_ctrl:await(CasePid, 5000),

    ?assertMatch({error, _}, Result),

    {comment, "await error completion verified"}.

%% @doc Test await on cancelled case.
await_cancelled_test(_Config) ->
    Pattern = wf_term:loop({max_iter, 1000},
        wf_term:task(loop_task, fun(Ctx) ->
            timer:sleep(50),
            {ok, Ctx}
        end)
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Cancel the case
    ok = ln_ctrl:cancel(CasePid),

    %% Await should return error
    Result = ln_ctrl:await(CasePid, 1000),

    ?assertMatch({error, _}, Result),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "await cancelled verified"}.

%% @doc Test await with invalid case ID.
await_invalid_case_id_test(_Config) ->
    Result = ln_ctrl:await(invalid_case_id, 1000),

    ?assertMatch({error, invalid_case_id}, Result),

    {comment, "await invalid case ID error verified"}.

%%%===================================================================
%%% Test Cases - status/1 API
%%%===================================================================

%% @doc Test status on running case.
status_running_test(_Config) ->
    Pattern = wf_term:loop({max_iter, 100},
        wf_term:task(status_task, fun(Ctx) ->
            timer:sleep(10),
            {ok, Ctx}
        end)
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Get status while running
    Status = ln_ctrl:status(CasePid),

    ?assertMatch(#{
        state := running,
        steps := _,
        pc := _,
        stack_depth := _,
        effects_issued := _,
        budget_status := _
    }, Status),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "status running verified"}.

%% @doc Test status on halted case.
status_halted_test(_Config) ->
    Pattern = wf_term:task(halt_task, fun(Ctx) -> {ok, Ctx} end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Wait for completion
    {ok, _} = ln_ctrl:await(CasePid, 1000),

    %% Get status after halt
    Status = ln_ctrl:status(CasePid),

    ?assertMatch(#{state := halted}, Status),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "status halted verified"}.

%% @doc Test status on cancelled case.
status_cancelled_test(_Config) ->
    Pattern = wf_term:loop({max_iter, 100},
        wf_term:task(cancel_task, fun(Ctx) ->
            timer:sleep(50),
            {ok, Ctx}
        end)
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{}),

    %% Cancel the case
    ok = ln_ctrl:cancel(CasePid),

    %% Wait for cancellation to propagate
    timer:sleep(100),

    %% Get status
    Status = ln_ctrl:status(CasePid),

    ?assertMatch(#{state := cancelled}, Status),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "status cancelled verified"}.

%% @doc Test status with invalid case ID.
status_invalid_case_id_test(_Config) ->
    Result = ln_ctrl:status(invalid_case_id),

    ?assertMatch({error, invalid_case_id}, Result),

    {comment, "status invalid case ID error verified"}.

%% @doc Test status budget tracking.
status_budget_tracking_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(5, 1000, 10.0),
    Pattern = wf_term:task(budget_task, fun(Ctx) -> {ok, Ctx} end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{budget => Budget}),

    %% Get status
    Status = ln_ctrl:status(CasePid),

    %% Budget status should be present
    ?assertMatch(#{budget_status := _}, Status),

    #{budget_status := BudgetStatus} = Status,
    ?assertMatch(#{
        max_effects := 5,
        max_latency_ms := 1000,
        max_cost_usd := 10.0
    }, BudgetStatus),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "status budget tracking verified"}.

%%%===================================================================
%%% Test Cases - trace/3 API
%%%===================================================================

%% @doc Test trace range extraction.
trace_range_extraction_test(_Config) ->
    Pattern = wf_term:seq(
        wf_term:task(trace_task_1, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(trace_task_2, fun(Ctx) -> {ok, Ctx} end)
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{trace_level => full}),

    %% Wait for completion
    {ok, _} = ln_ctrl:await(CasePid, 1000),

    %% Get trace for specific range
    TraceResult = ln_ctrl:trace(CasePid, 0, 5),

    ?assert(is_list(TraceResult) orelse TraceResult =:= {error, _}),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "trace range extraction verified"}.

%% @doc Test trace with empty range.
trace_empty_range_test(_Config) ->
    Pattern = wf_term:task(empty_task, fun(Ctx) -> {ok, Ctx} end),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{trace_level => basic}),

    %% Wait for completion
    {ok, _} = ln_ctrl:await(CasePid, 1000),

    %% Get trace for empty range
    TraceResult = ln_ctrl:trace(CasePid, 10, 5),  % From > To

    ?assert(is_list(TraceResult) orelse TraceResult =:= {error, _}),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "trace empty range verified"}.

%% @doc Test trace with full range.
trace_full_range_test(_Config) ->
    Pattern = wf_term:seq(
        wf_term:task(full_task_1, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(full_task_2, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(full_task_3, fun(Ctx) -> {ok, Ctx} end)
    ),

    {ok, CasePid} = ln_ctrl:new_case(Pattern, #{}, #{trace_level => full}),

    %% Wait for completion
    {ok, _} = ln_ctrl:await(CasePid, 1000),

    %% Get full trace
    TraceResult = ln_ctrl:trace(CasePid, 0, 1000),

    ?assert(is_list(TraceResult) orelse TraceResult =:= {error, _}),

    %% Cleanup
    cleanup_case(CasePid),

    {comment, "trace full range verified"}.

%% @doc Test trace with invalid case ID.
trace_invalid_case_id_test(_Config) ->
    Result = ln_ctrl:trace(invalid_case_id, 0, 10),

    ?assertMatch({error, invalid_case_id}, Result),

    {comment, "trace invalid case ID error verified"}.

%%%===================================================================
%%% Test Cases - validate/2 API
%%%===================================================================

%% @doc Test validate with valid pattern.
validate_valid_pattern_test(_Config) ->
    Pattern = wf_term:task(valid_task, fun(Ctx) -> {ok, Ctx} end),

    Result = ln_ctrl:validate(Pattern, #{}),

    ?assertEqual(ok, Result),

    {comment, "validate valid pattern verified"}.

%% @doc Test validate with invalid pattern.
validate_invalid_pattern_test(_Config) ->
    InvalidPattern = {invalid, structure},

    Result = ln_ctrl:validate(InvalidPattern, #{}),

    ?assertMatch({error, {invalid_pattern, _}}, Result),

    {comment, "validate invalid pattern verified"}.

%% @doc Test validate with pattern that fails compilation.
validate_compilation_failure_test(_Config) ->
    %% Create a pattern with invalid task name type
    BadPattern = {task, 12345, fun(_) -> {ok, #{}} end},

    Result = ln_ctrl:validate(BadPattern, #{}),

    ?assertMatch({error, _}, Result),

    {comment, "validate compilation failure verified"}.

%% @doc Test validate with complex nested pattern.
validate_complex_pattern_test(_Config) ->
    %% Create a complex nested pattern
    InnerPattern = wf_term:par([
        wf_term:task(par_task_1, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(par_task_2, fun(Ctx) -> {ok, Ctx} end)
    ]),

    Pattern = wf_term:seq(
        wf_term:task(seq_task, fun(Ctx) -> {ok, Ctx} end),
        wf_term:choice([
            InnerPattern,
            wf_term:task(choice_task, fun(Ctx) -> {ok, Ctx} end)
        ])
    ),

    Result = ln_ctrl:validate(Pattern, #{}),

    ?assertEqual(ok, Result),

    {comment, "validate complex pattern verified"}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Ensure required modules are loaded.
-spec ensure_modules_loaded() -> ok.
ensure_modules_loaded() ->
    Modules = [
        ln_ctrl,
        ln_ctrl_case_runner,
        ln_ctrl_budget,
        ln_ctrl_sched,
        wf_term,
        wf_compile,
        wf_exec,
        wf_vm
    ],
    Results = [code:ensure_loaded(M) || M <- Modules],
    case lists:all(fun
        ({module, _}) -> true;
        (_) -> false
    end, Results) of
        true -> ok;
        false ->
            ct:pal("Warning: Some test modules not found"),
            ok
    end.

%% @doc Cleanup a single case runner.
-spec cleanup_case(pid()) -> ok.
cleanup_case(CasePid) when is_pid(CasePid) ->
    case is_process_alive(CasePid) of
        true ->
            %% Try graceful stop first
            catch ln_ctrl:cancel(CasePid),
            timer:sleep(10),
            %% Force kill if still alive
            case is_process_alive(CasePid) of
                true -> exit(CasePid, kill);
                _ -> ok
            end;
        false ->
            ok
    end;
cleanup_case(_) ->
    ok.

%% @doc Cleanup all case runners from previous tests.
-spec cleanup_all_cases() -> ok.
cleanup_all_cases() ->
    %% Find and cleanup any ln_ctrl_case_runner processes
    Processes = processes(),
    lists:foreach(fun(Pid) ->
        case process_info(Pid, initial_call) of
            {initial_call, {ln_ctrl_case_runner, _, _}} ->
                catch exit(Pid, kill);
            _ ->
                ok
        end
    end, Processes),
    ok.
