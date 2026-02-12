%%%-------------------------------------------------------------------
%%% @doc
%%% ln_ctrl_budget Common Test Suite
%%%
%%% Production-quality tests for budget enforcement in ln_ctrl.
%%% Tests cover all budget operations including creation, effect checking,
%%% latency checking, exceeded state propagation, and status reporting.
%%%
%%% Test Coverage:
%%% - new_budget/3: unlimited budgets, limited budgets, mixed configurations
%%% - check_effect/2: boundary conditions, cost tracking, exceeded detection
%%% - check_latency/2: timeout detection, unlimited latency, exact boundaries
%%% - status/1: accurate reporting of all budget dimensions
%%% - Exceeded state propagation across operations
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ln_ctrl_budget_SUITE).
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
    %% new_budget/3 tests
    new_budget_all_unlimited_test/1,
    new_budget_limited_effects_only_test/1,
    new_budget_limited_latency_only_test/1,
    new_budget_limited_cost_only_test/1,
    new_budget_all_limited_test/1,
    new_budget_mixed_limits_test/1,

    %% check_effect/2 boundary tests
    check_effect_at_exact_boundary_test/1,
    check_effect_below_boundary_test/1,
    check_effect_above_boundary_test/1,
    check_effect_zero_limit_test/1,
    check_effect_single_effect_budget_test/1,

    %% check_effect/2 cost tracking tests
    check_effect_cost_tracking_test/1,
    check_effect_cost_accumulation_test/1,
    check_effect_cost_exact_boundary_test/1,
    check_effect_cost_fractional_test/1,
    check_effect_zero_cost_test/1,

    %% check_latency/2 tests
    check_latency_below_limit_test/1,
    check_latency_at_exact_limit_test/1,
    check_latency_above_limit_test/1,
    check_latency_unlimited_test/1,
    check_latency_zero_limit_test/1,
    check_latency_multiple_checks_test/1,

    %% Exceeded state propagation tests
    exceeded_state_propagation_effects_test/1,
    exceeded_state_propagation_cost_test/1,
    exceeded_state_propagation_latency_test/1,
    exceeded_state_after_multiple_exceedances_test/1,
    exceeded_state_persists_test/1,

    %% Status reporting tests
    status_initial_state_test/1,
    status_after_effect_test/1,
    status_after_multiple_effects_test/1,
    status_latency_tracking_test/1,
    status_cost_tracking_test/1,
    status_exceeded_flag_test/1,
    status_all_dimensions_test/1,

    %% Edge cases and integration tests
    concurrent_limit_checks_test/1,
    large_numbers_test/1,
    minimal_budget_test/1,
    budget_exhaustion_sequence_test/1
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type config() :: ct:config().

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @doc Returns list of all test cases and groups.
-spec all() -> [atom() | {group, atom()}].
all() ->
    [
        {group, new_budget_tests},
        {group, check_effect_boundary_tests},
        {group, check_effect_cost_tests},
        {group, check_latency_tests},
        {group, exceeded_state_tests},
        {group, status_reporting_tests},
        {group, edge_case_tests}
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [{atom(), term()}], [atom()]}].
groups() ->
    [
        {new_budget_tests, [shuffle, {repeat, 1}], [
            new_budget_all_unlimited_test,
            new_budget_limited_effects_only_test,
            new_budget_limited_latency_only_test,
            new_budget_limited_cost_only_test,
            new_budget_all_limited_test,
            new_budget_mixed_limits_test
        ]},
        {check_effect_boundary_tests, [shuffle, {repeat, 1}], [
            check_effect_at_exact_boundary_test,
            check_effect_below_boundary_test,
            check_effect_above_boundary_test,
            check_effect_zero_limit_test,
            check_effect_single_effect_budget_test
        ]},
        {check_effect_cost_tests, [shuffle, {repeat, 1}], [
            check_effect_cost_tracking_test,
            check_effect_cost_accumulation_test,
            check_effect_cost_exact_boundary_test,
            check_effect_cost_fractional_test,
            check_effect_zero_cost_test
        ]},
        {check_latency_tests, [shuffle, {repeat, 1}], [
            check_latency_below_limit_test,
            check_latency_at_exact_limit_test,
            check_latency_above_limit_test,
            check_latency_unlimited_test,
            check_latency_zero_limit_test,
            check_latency_multiple_checks_test
        ]},
        {exceeded_state_tests, [shuffle, {repeat, 1}], [
            exceeded_state_propagation_effects_test,
            exceeded_state_propagation_cost_test,
            exceeded_state_propagation_latency_test,
            exceeded_state_after_multiple_exceedances_test,
            exceeded_state_persists_test
        ]},
        {status_reporting_tests, [shuffle, {repeat, 1}], [
            status_initial_state_test,
            status_after_effect_test,
            status_after_multiple_effects_test,
            status_latency_tracking_test,
            status_cost_tracking_test,
            status_exceeded_flag_test,
            status_all_dimensions_test
        ]},
        {edge_case_tests, [], [
            concurrent_limit_checks_test,
            large_numbers_test,
            minimal_budget_test,
            budget_exhaustion_sequence_test
        ]}
    ].

%% @doc Suite-level configuration.
-spec suite() -> [{atom(), term()}].
suite() ->
    [
        {timetrap, {seconds, 30}}
    ].

%% @doc Suite-level initialization.
-spec init_per_suite(Config :: config()) -> config().
init_per_suite(Config) ->
    ct:pal("Starting ln_ctrl_budget_SUITE"),

    %% Ensure the budget module is loaded
    {module, ln_ctrl_budget} = code:ensure_loaded(ln_ctrl_budget),

    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Completed ln_ctrl_budget_SUITE"),
    ok.

%% @doc Group-level initialization.
-spec init_per_group(Group :: atom(), Config :: config()) -> config().
init_per_group(Group, Config) ->
    ct:pal("Initializing group: ~p", [Group]),
    Config.

%% @doc Group-level cleanup.
-spec end_per_group(Group :: atom(), Config :: config()) -> ok.
end_per_group(Group, _Config) ->
    ct:pal("Completed group: ~p", [Group]),
    ok.

%% @doc Testcase-level initialization.
-spec init_per_testcase(TestCase :: atom(), Config :: config()) -> config().
init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    [{testcase, TestCase} | Config].

%% @doc Testcase-level cleanup.
-spec end_per_testcase(TestCase :: atom(), Config :: config()) -> ok.
end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases - new_budget/3
%%%===================================================================

%% @doc Test new_budget with all dimensions unlimited.
-spec new_budget_all_unlimited_test(config()) -> {comment, string()}.
new_budget_all_unlimited_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, unlimited),
    Status = ln_ctrl_budget:status(Budget),

    ?assertEqual(unlimited, maps:get(max_effects, Status)),
    ?assertEqual(unlimited, maps:get(max_latency_ms, Status)),
    ?assertEqual(unlimited, maps:get(max_cost_usd, Status)),
    ?assertEqual(0, maps:get(effects_used, Status)),
    ?assertEqual(0, maps:get(latency_used_ms, Status)),
    ?assertEqual(0.0, maps:get(cost_used_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "new_budget with all unlimited verified"}.

%% @doc Test new_budget with only effects limited.
-spec new_budget_limited_effects_only_test(config()) -> {comment, string()}.
new_budget_limited_effects_only_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(100, unlimited, unlimited),
    Status = ln_ctrl_budget:status(Budget),

    ?assertEqual(100, maps:get(max_effects, Status)),
    ?assertEqual(unlimited, maps:get(max_latency_ms, Status)),
    ?assertEqual(unlimited, maps:get(max_cost_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "new_budget with limited effects only verified"}.

%% @doc Test new_budget with only latency limited.
-spec new_budget_limited_latency_only_test(config()) -> {comment, string()}.
new_budget_limited_latency_only_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 5000, unlimited),
    Status = ln_ctrl_budget:status(Budget),

    ?assertEqual(unlimited, maps:get(max_effects, Status)),
    ?assertEqual(5000, maps:get(max_latency_ms, Status)),
    ?assertEqual(unlimited, maps:get(max_cost_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "new_budget with limited latency only verified"}.

%% @doc Test new_budget with only cost limited.
-spec new_budget_limited_cost_only_test(config()) -> {comment, string()}.
new_budget_limited_cost_only_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 100.0),
    Status = ln_ctrl_budget:status(Budget),

    ?assertEqual(unlimited, maps:get(max_effects, Status)),
    ?assertEqual(unlimited, maps:get(max_latency_ms, Status)),
    ?assertEqual(100.0, maps:get(max_cost_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "new_budget with limited cost only verified"}.

%% @doc Test new_budget with all dimensions limited.
-spec new_budget_all_limited_test(config()) -> {comment, string()}.
new_budget_all_limited_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(50, 10000, 25.50),
    Status = ln_ctrl_budget:status(Budget),

    ?assertEqual(50, maps:get(max_effects, Status)),
    ?assertEqual(10000, maps:get(max_latency_ms, Status)),
    ?assertEqual(25.50, maps:get(max_cost_usd, Status)),
    ?assertEqual(0, maps:get(effects_used, Status)),
    ?assertEqual(0.0, maps:get(cost_used_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "new_budget with all limited verified"}.

%% @doc Test new_budget with mixed limit configurations.
-spec new_budget_mixed_limits_test(config()) -> {comment, string()}.
new_budget_mixed_limits_test(_Config) ->
    %% Effects and cost limited, latency unlimited
    Budget1 = ln_ctrl_budget:new_budget(10, unlimited, 5.0),
    Status1 = ln_ctrl_budget:status(Budget1),
    ?assertEqual(10, maps:get(max_effects, Status1)),
    ?assertEqual(unlimited, maps:get(max_latency_ms, Status1)),
    ?assertEqual(5.0, maps:get(max_cost_usd, Status1)),

    %% Effects and latency limited, cost unlimited
    Budget2 = ln_ctrl_budget:new_budget(100, 30000, unlimited),
    Status2 = ln_ctrl_budget:status(Budget2),
    ?assertEqual(100, maps:get(max_effects, Status2)),
    ?assertEqual(30000, maps:get(max_latency_ms, Status2)),
    ?assertEqual(unlimited, maps:get(max_cost_usd, Status2)),

    %% Latency and cost limited, effects unlimited
    Budget3 = ln_ctrl_budget:new_budget(unlimited, 60000, 50.0),
    Status3 = ln_ctrl_budget:status(Budget3),
    ?assertEqual(unlimited, maps:get(max_effects, Status3)),
    ?assertEqual(60000, maps:get(max_latency_ms, Status3)),
    ?assertEqual(50.0, maps:get(max_cost_usd, Status3)),

    {comment, "new_budget with mixed limits verified"}.

%%%===================================================================
%%% Test Cases - check_effect/2 Boundary Tests
%%%===================================================================

%% @doc Test check_effect at exact boundary (at limit = ok, over = exceeded).
%% CRITICAL: This tests the boundary semantics - exactly at limit is OK,
%% exceeding by 1 triggers budget_exceeded.
-spec check_effect_at_exact_boundary_test(config()) -> {comment, string()}.
check_effect_at_exact_boundary_test(_Config) ->
    %% Budget allows exactly 5 effects
    Budget = ln_ctrl_budget:new_budget(5, unlimited, unlimited),

    %% Effects 1-5 should all succeed
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),
    ?assertEqual(1, maps:get(effects_used, ln_ctrl_budget:status(B1))),

    {ok, B2} = ln_ctrl_budget:check_effect(B1, 0.0),
    ?assertEqual(2, maps:get(effects_used, ln_ctrl_budget:status(B2))),

    {ok, B3} = ln_ctrl_budget:check_effect(B2, 0.0),
    ?assertEqual(3, maps:get(effects_used, ln_ctrl_budget:status(B3))),

    {ok, B4} = ln_ctrl_budget:check_effect(B3, 0.0),
    ?assertEqual(4, maps:get(effects_used, ln_ctrl_budget:status(B4))),

    %% Effect 5 should succeed - at exact boundary
    {ok, B5} = ln_ctrl_budget:check_effect(B4, 0.0),
    ?assertEqual(5, maps:get(effects_used, ln_ctrl_budget:status(B5))),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B5))),

    %% Effect 6 should fail - exceeds boundary
    Result = ln_ctrl_budget:check_effect(B5, 0.0),
    ?assertMatch({budget_exceeded, {max_effects_exceeded, 6, 5}, _}, Result),

    {budget_exceeded, {max_effects_exceeded, 6, 5}, ExceededBudget} = Result,
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(ExceededBudget))),

    {comment, "check_effect at exact boundary verified"}.

%% @doc Test check_effect below boundary (always OK).
-spec check_effect_below_boundary_test(config()) -> {comment, string()}.
check_effect_below_boundary_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(100, unlimited, unlimited),

    %% Execute 50 effects (well below 100 limit)
    FinalBudget = lists:foldl(
        fun(_N, CurrentBudget) ->
            {ok, NextBudget} = ln_ctrl_budget:check_effect(CurrentBudget, 0.0),
            NextBudget
        end,
        Budget,
        lists:seq(1, 50)
    ),

    Status = ln_ctrl_budget:status(FinalBudget),
    ?assertEqual(50, maps:get(effects_used, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "check_effect below boundary verified"}.

%% @doc Test check_effect above boundary (immediate exceeded).
-spec check_effect_above_boundary_test(config()) -> {comment, string()}.
check_effect_above_boundary_test(_Config) ->
    %% Very restrictive budget - 2 effects
    Budget = ln_ctrl_budget:new_budget(2, unlimited, unlimited),

    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 0.0),

    %% Third effect exceeds
    Result = ln_ctrl_budget:check_effect(B2, 0.0),
    ?assertMatch({budget_exceeded, {max_effects_exceeded, 3, 2}, _}, Result),

    {comment, "check_effect above boundary verified"}.

%% @doc Test check_effect with zero limit (immediate exceeded on first effect).
-spec check_effect_zero_limit_test(config()) -> {comment, string()}.
check_effect_zero_limit_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(0, unlimited, unlimited),

    %% First effect should immediately exceed
    Result = ln_ctrl_budget:check_effect(Budget, 0.0),
    ?assertMatch({budget_exceeded, {max_effects_exceeded, 1, 0}, _}, Result),

    {budget_exceeded, {max_effects_exceeded, 1, 0}, ExceededBudget} = Result,
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(ExceededBudget))),

    {comment, "check_effect with zero limit verified"}.

%% @doc Test check_effect with single effect budget.
-spec check_effect_single_effect_budget_test(config()) -> {comment, string()}.
check_effect_single_effect_budget_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(1, unlimited, unlimited),

    %% First effect should succeed
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),
    ?assertEqual(1, maps:get(effects_used, ln_ctrl_budget:status(B1))),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B1))),

    %% Second effect should fail
    Result = ln_ctrl_budget:check_effect(B1, 0.0),
    ?assertMatch({budget_exceeded, {max_effects_exceeded, 2, 1}, _}, Result),

    {comment, "check_effect with single effect budget verified"}.

%%%===================================================================
%%% Test Cases - check_effect/2 Cost Tracking Tests
%%%===================================================================

%% @doc Test check_effect with cost tracking.
-spec check_effect_cost_tracking_test(config()) -> {comment, string()}.
check_effect_cost_tracking_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 100.0),

    %% Issue effect with cost
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 10.0),
    Status1 = ln_ctrl_budget:status(B1),
    ?assertEqual(10.0, maps:get(cost_used_usd, Status1)),

    %% Issue another effect with cost
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 25.0),
    Status2 = ln_ctrl_budget:status(B2),
    ?assertEqual(35.0, maps:get(cost_used_usd, Status2)),

    {comment, "check_effect cost tracking verified"}.

%% @doc Test check_effect cost accumulation over multiple effects.
-spec check_effect_cost_accumulation_test(config()) -> {comment, string()}.
check_effect_cost_accumulation_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 100.0),

    %% Issue 5 effects at 10.0 each = 50.0 total
    Costs = [10.0, 10.0, 10.0, 10.0, 10.0],
    FinalBudget = lists:foldl(
        fun(Cost, CurrentBudget) ->
            {ok, NextBudget} = ln_ctrl_budget:check_effect(CurrentBudget, Cost),
            NextBudget
        end,
        Budget,
        Costs
    ),

    Status = ln_ctrl_budget:status(FinalBudget),
    ?assertEqual(50.0, maps:get(cost_used_usd, Status)),
    ?assertEqual(5, maps:get(effects_used, Status)),

    {comment, "check_effect cost accumulation verified"}.

%% @doc Test check_effect cost at exact boundary.
-spec check_effect_cost_exact_boundary_test(config()) -> {comment, string()}.
check_effect_cost_exact_boundary_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 100.0),

    %% Use exactly 100.0
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 50.0),
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 50.0),

    Status2 = ln_ctrl_budget:status(B2),
    ?assertEqual(100.0, maps:get(cost_used_usd, Status2)),
    ?assertEqual(false, maps:get(exceeded, Status2)),

    %% Any additional cost should exceed
    Result = ln_ctrl_budget:check_effect(B2, 0.01),
    ?assertMatch({budget_exceeded, {max_cost_exceeded, _, 100.0}, _}, Result),

    {comment, "check_effect cost at exact boundary verified"}.

%% @doc Test check_effect with fractional costs.
-spec check_effect_cost_fractional_test(config()) -> {comment, string()}.
check_effect_cost_fractional_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 1.0),

    %% Use fractional costs
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.25),
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 0.25),
    {ok, B3} = ln_ctrl_budget:check_effect(B2, 0.25),
    {ok, B4} = ln_ctrl_budget:check_effect(B3, 0.25),

    Status4 = ln_ctrl_budget:status(B4),
    ?assertEqual(1.0, maps:get(cost_used_usd, Status4)),
    ?assertEqual(false, maps:get(exceeded, Status4)),

    %% Even tiny additional cost should exceed
    Result = ln_ctrl_budget:check_effect(B4, 0.001),
    ?assertMatch({budget_exceeded, {max_cost_exceeded, _, 1.0}, _}, Result),

    {comment, "check_effect fractional cost verified"}.

%% @doc Test check_effect with zero cost (free effects).
-spec check_effect_zero_cost_test(config()) -> {comment, string()}.
check_effect_zero_cost_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(10, unlimited, 0.5),

    %% Issue effects with zero cost
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 0.0),
    {ok, B3} = ln_ctrl_budget:check_effect(B2, 0.0),

    Status = ln_ctrl_budget:status(B3),
    ?assertEqual(3, maps:get(effects_used, Status)),
    ?assertEqual(0.0, maps:get(cost_used_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "check_effect with zero cost verified"}.

%%%===================================================================
%%% Test Cases - check_latency/2 Tests
%%%===================================================================

%% @doc Test check_latency below limit.
-spec check_latency_below_limit_test(config()) -> {comment, string()}.
check_latency_below_limit_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 1000, unlimited),

    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 0)),
    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 500)),
    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 999)),

    {comment, "check_latency below limit verified"}.

%% @doc Test check_latency at exact limit (boundary test).
%% CRITICAL: At exact limit is OK, over is timeout.
-spec check_latency_at_exact_limit_test(config()) -> {comment, string()}.
check_latency_at_exact_limit_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 1000, unlimited),

    %% At exact limit should be OK
    Result1 = ln_ctrl_budget:check_latency(Budget, 1000),
    ?assertEqual(ok, Result1),

    {comment, "check_latency at exact limit verified"}.

%% @doc Test check_latency above limit (timeout detection).
-spec check_latency_above_limit_test(config()) -> {comment, string()}.
check_latency_above_limit_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 1000, unlimited),

    %% Over limit should return timeout
    Result = ln_ctrl_budget:check_latency(Budget, 1001),
    ?assertMatch({timeout, {max_latency_exceeded, 1001, 1000}, _}, Result),

    {timeout, {max_latency_exceeded, 1001, 1000}, ExceededBudget} = Result,
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(ExceededBudget))),

    %% Far over limit
    Result2 = ln_ctrl_budget:check_latency(Budget, 5000),
    ?assertMatch({timeout, {max_latency_exceeded, 5000, 1000}, _}, Result2),

    {comment, "check_latency above limit verified"}.

%% @doc Test check_latency with unlimited latency.
-spec check_latency_unlimited_test(config()) -> {comment, string()}.
check_latency_unlimited_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, unlimited),

    %% Any latency should be OK
    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 0)),
    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 1000)),
    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 1000000)),
    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 16#FFFFFFFF)),

    {comment, "check_latency with unlimited verified"}.

%% @doc Test check_latency with zero limit.
-spec check_latency_zero_limit_test(config()) -> {comment, string()}.
check_latency_zero_limit_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 0, unlimited),

    %% At zero should be OK
    ?assertEqual(ok, ln_ctrl_budget:check_latency(Budget, 0)),

    %% Any positive latency should timeout
    Result = ln_ctrl_budget:check_latency(Budget, 1),
    ?assertMatch({timeout, {max_latency_exceeded, 1, 0}, _}, Result),

    {comment, "check_latency with zero limit verified"}.

%% @doc Test check_latency with multiple checks updating latency_used.
-spec check_latency_multiple_checks_test(config()) -> {comment, string()}.
check_latency_multiple_checks_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 1000, unlimited),

    %% First check at 500ms
    ok = ln_ctrl_budget:check_latency(Budget, 500),

    %% Second check at 800ms
    ok = ln_ctrl_budget:check_latency(Budget, 800),

    %% Third check at 1000ms - still OK
    ok = ln_ctrl_budget:check_latency(Budget, 1000),

    %% Fourth check at 1001ms - timeout
    Result = ln_ctrl_budget:check_latency(Budget, 1001),
    ?assertMatch({timeout, _, _}, Result),

    {comment, "check_latency multiple checks verified"}.

%%%===================================================================
%%% Test Cases - Exceeded State Propagation
%%%===================================================================

%% @doc Test exceeded state propagation when effects limit is hit.
-spec exceeded_state_propagation_effects_test(config()) -> {comment, string()}.
exceeded_state_propagation_effects_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(3, unlimited, unlimited),

    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B1))),

    {ok, B2} = ln_ctrl_budget:check_effect(B1, 0.0),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B2))),

    {ok, B3} = ln_ctrl_budget:check_effect(B2, 0.0),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B3))),

    %% Exceed on 4th effect
    {budget_exceeded, _, ExceededBudget} = ln_ctrl_budget:check_effect(B3, 0.0),
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(ExceededBudget))),

    {comment, "exceeded state propagation for effects verified"}.

%% @doc Test exceeded state propagation when cost limit is hit.
-spec exceeded_state_propagation_cost_test(config()) -> {comment, string()}.
exceeded_state_propagation_cost_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 50.0),

    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 25.0),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B1))),

    {ok, B2} = ln_ctrl_budget:check_effect(B1, 25.0),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B2))),

    %% Exceed on 3rd effect cost
    {budget_exceeded, {max_cost_exceeded, _, 50.0}, ExceededBudget} =
        ln_ctrl_budget:check_effect(B2, 1.0),
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(ExceededBudget))),

    {comment, "exceeded state propagation for cost verified"}.

%% @doc Test exceeded state propagation when latency limit is hit.
-spec exceeded_state_propagation_latency_test(config()) -> {comment, string()}.
exceeded_state_propagation_latency_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 5000, unlimited),

    ok = ln_ctrl_budget:check_latency(Budget, 4000),
    ok = ln_ctrl_budget:check_latency(Budget, 5000),

    %% Exceed on latency
    {timeout, {max_latency_exceeded, 5001, 5000}, ExceededBudget} =
        ln_ctrl_budget:check_latency(Budget, 5001),

    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(ExceededBudget))),

    {comment, "exceeded state propagation for latency verified"}.

%% @doc Test exceeded state after multiple exceedances (idempotent).
-spec exceeded_state_after_multiple_exceedances_test(config()) -> {comment, string()}.
exceeded_state_after_multiple_exceedances_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(1, unlimited, unlimited),

    %% First effect OK
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),

    %% First exceedance
    {budget_exceeded, _, B2} = ln_ctrl_budget:check_effect(B1, 0.0),
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(B2))),

    %% Second exceedance - should still be exceeded
    {budget_exceeded, _, B3} = ln_ctrl_budget:check_effect(B2, 0.0),
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(B3))),

    {comment, "exceeded state after multiple exceedances verified"}.

%% @doc Test that exceeded state persists in returned budget.
-spec exceeded_state_persists_test(config()) -> {comment, string()}.
exceeded_state_persists_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(2, 100, 10.0),

    %% Use up effects
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 3.0),
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 3.0),

    %% Exceed effects
    {budget_exceeded, _, B3} = ln_ctrl_budget:check_effect(B2, 3.0),

    %% Exceeded flag should be true and stay true
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(B3))),

    %% Check latency on exceeded budget - still returns timeout
    {timeout, _, B4} = ln_ctrl_budget:check_latency(B3, 200),
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(B4))),

    {comment, "exceeded state persists verified"}.

%%%===================================================================
%%% Test Cases - Status Reporting
%%%===================================================================

%% @doc Test status reports accurate initial state.
-spec status_initial_state_test(config()) -> {comment, string()}.
status_initial_state_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(100, 5000, 50.0),
    Status = ln_ctrl_budget:status(Budget),

    %% Verify all initial values
    ?assertEqual(100, maps:get(max_effects, Status)),
    ?assertEqual(5000, maps:get(max_latency_ms, Status)),
    ?assertEqual(50.0, maps:get(max_cost_usd, Status)),
    ?assertEqual(0, maps:get(effects_used, Status)),
    ?assertEqual(0, maps:get(latency_used_ms, Status)),
    ?assertEqual(0.0, maps:get(cost_used_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "status initial state verified"}.

%% @doc Test status after single effect.
-spec status_after_effect_test(config()) -> {comment, string()}.
status_after_effect_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(100, unlimited, 100.0),
    {ok, UpdatedBudget} = ln_ctrl_budget:check_effect(Budget, 15.5),
    Status = ln_ctrl_budget:status(UpdatedBudget),

    ?assertEqual(1, maps:get(effects_used, Status)),
    ?assertEqual(15.5, maps:get(cost_used_usd, Status)),
    ?assertEqual(0, maps:get(latency_used_ms, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "status after effect verified"}.

%% @doc Test status after multiple effects.
-spec status_after_multiple_effects_test(config()) -> {comment, string()}.
status_after_multiple_effects_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(100, unlimited, 100.0),

    %% Issue 10 effects at 5.0 each
    FinalBudget = lists:foldl(
        fun(_N, B) ->
            {ok, NextB} = ln_ctrl_budget:check_effect(B, 5.0),
            NextB
        end,
        Budget,
        lists:seq(1, 10)
    ),

    Status = ln_ctrl_budget:status(FinalBudget),
    ?assertEqual(10, maps:get(effects_used, Status)),
    ?assertEqual(50.0, maps:get(cost_used_usd, Status)),

    {comment, "status after multiple effects verified"}.

%% @doc Test status latency tracking.
-spec status_latency_tracking_test(config()) -> {comment, string()}.
status_latency_tracking_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 10000, unlimited),

    %% Check latency at various points
    ok = ln_ctrl_budget:check_latency(Budget, 100),
    ok = ln_ctrl_budget:check_latency(Budget, 500),
    {timeout, _, ExceededBudget} = ln_ctrl_budget:check_latency(Budget, 15000),

    Status = ln_ctrl_budget:status(ExceededBudget),
    ?assertEqual(15000, maps:get(latency_used_ms, Status)),
    ?assertEqual(true, maps:get(exceeded, Status)),

    {comment, "status latency tracking verified"}.

%% @doc Test status cost tracking accuracy.
-spec status_cost_tracking_test(config()) -> {comment, string()}.
status_cost_tracking_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 1000.0),

    %% Issue effects with varying costs
    Costs = [1.5, 2.25, 3.125, 4.0625, 5.03125],
    FinalBudget = lists:foldl(
        fun(Cost, B) ->
            {ok, NextB} = ln_ctrl_budget:check_effect(B, Cost),
            NextB
        end,
        Budget,
        Costs
    ),

    Status = ln_ctrl_budget:status(FinalBudget),
    ExpectedTotal = lists:sum(Costs),
    ActualCost = maps:get(cost_used_usd, Status),

    %% Allow small floating point tolerance
    ?assert(abs(ActualCost - ExpectedTotal) < 0.0001),

    {comment, "status cost tracking accuracy verified"}.

%% @doc Test status exceeded flag accuracy.
-spec status_exceeded_flag_test(config()) -> {comment, string()}.
status_exceeded_flag_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(5, 100, 10.0),

    %% Use up effects - should trigger exceeded
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.0),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B1))),

    {ok, B2} = ln_ctrl_budget:check_effect(B1, 0.0),
    {ok, B3} = ln_ctrl_budget:check_effect(B2, 0.0),
    {ok, B4} = ln_ctrl_budget:check_effect(B3, 0.0),
    {ok, B5} = ln_ctrl_budget:check_effect(B4, 0.0),

    %% 5th effect at limit - still not exceeded
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B5))),

    %% 6th effect - exceeded
    {budget_exceeded, _, B6} = ln_ctrl_budget:check_effect(B5, 0.0),
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(B6))),

    {comment, "status exceeded flag accuracy verified"}.

%% @doc Test status reports all dimensions accurately.
-spec status_all_dimensions_test(config()) -> {comment, string()}.
status_all_dimensions_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(10, 1000, 100.0),

    %% Perform some operations
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 10.0),
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 20.0),
    ok = ln_ctrl_budget:check_latency(B2, 500),

    Status = ln_ctrl_budget:status(B2),

    %% Verify all dimensions
    ?assertEqual(10, maps:get(max_effects, Status)),
    ?assertEqual(1000, maps:get(max_latency_ms, Status)),
    ?assertEqual(100.0, maps:get(max_cost_usd, Status)),
    ?assertEqual(2, maps:get(effects_used, Status)),
    %% Note: latency_used is updated in the budget passed to check_latency,
    %% but B2 was captured before that call. Verify B2 has latency 0.
    ?assertEqual(0, maps:get(latency_used_ms, Status)),
    ?assertEqual(30.0, maps:get(cost_used_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    {comment, "status all dimensions verified"}.

%%%===================================================================
%%% Test Cases - Edge Cases and Integration
%%%===================================================================

%% @doc Test concurrent limit checks (effects and cost together).
-spec concurrent_limit_checks_test(config()) -> {comment, string()}.
concurrent_limit_checks_test(_Config) ->
    %% Tight budget on both effects and cost
    Budget = ln_ctrl_budget:new_budget(10, unlimited, 50.0),

    %% Use up cost before effects
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 25.0),
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 24.0),  % Total 49.0

    %% Effects used = 2, cost used = 49.0
    Status2 = ln_ctrl_budget:status(B2),
    ?assertEqual(2, maps:get(effects_used, Status2)),
    ?assertEqual(49.0, maps:get(cost_used_usd, Status2)),

    %% Next effect at 1.0 exceeds cost (50.0) but not effects (10)
    Result = ln_ctrl_budget:check_effect(B2, 1.0),
    ?assertMatch({budget_exceeded, {max_cost_exceeded, _, 50.0}, _}, Result),

    {comment, "concurrent limit checks verified"}.

%% @doc Test with large numbers.
-spec large_numbers_test(config()) -> {comment, string()}.
large_numbers_test(_Config) ->
    %% Large limits
    Budget = ln_ctrl_budget:new_budget(1000000, 3600000, 1000000.0),

    %% Many effects
    FinalBudget = lists:foldl(
        fun(N, B) ->
            {ok, NextB} = ln_ctrl_budget:check_effect(B, 0.001),
            NextB
        end,
        Budget,
        lists:seq(1, 1000)
    ),

    Status = ln_ctrl_budget:status(FinalBudget),
    ?assertEqual(1000, maps:get(effects_used, Status)),
    ?assertEqual(1.0, maps:get(cost_used_usd, Status)),
    ?assertEqual(false, maps:get(exceeded, Status)),

    %% Large latency
    ok = ln_ctrl_budget:check_latency(FinalBudget, 1800000),

    {comment, "large numbers verified"}.

%% @doc Test minimal budget (1 effect, 1ms, 0.01 cost).
-spec minimal_budget_test(config()) -> {comment, string()}.
minimal_budget_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(1, 1, 0.01),

    %% One effect at exact limit
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 0.01),
    Status1 = ln_ctrl_budget:status(B1),
    ?assertEqual(1, maps:get(effects_used, Status1)),
    ?assertEqual(0.01, maps:get(cost_used_usd, Status1)),
    ?assertEqual(false, maps:get(exceeded, Status1)),

    %% One more effect exceeds
    {budget_exceeded, _, _} = ln_ctrl_budget:check_effect(B1, 0.0),

    %% Latency at limit
    ok = ln_ctrl_budget:check_latency(Budget, 1),

    %% Latency over limit
    {timeout, _, _} = ln_ctrl_budget:check_latency(Budget, 2),

    {comment, "minimal budget verified"}.

%% @doc Test complete budget exhaustion sequence.
-spec budget_exhaustion_sequence_test(config()) -> {comment, string()}.
budget_exhaustion_sequence_test(_Config) ->
    Budget = ln_ctrl_budget:new_budget(3, 500, 30.0),

    %% Effect 1: cost 10.0
    {ok, B1} = ln_ctrl_budget:check_effect(Budget, 10.0),
    ok = ln_ctrl_budget:check_latency(B1, 100),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B1))),

    %% Effect 2: cost 10.0
    {ok, B2} = ln_ctrl_budget:check_effect(B1, 10.0),
    ok = ln_ctrl_budget:check_latency(B2, 200),
    ?assertEqual(false, maps:get(exceeded, ln_ctrl_budget:status(B2))),

    %% Effect 3: cost 10.0 (at all limits)
    {ok, B3} = ln_ctrl_budget:check_effect(B2, 10.0),
    ok = ln_ctrl_budget:check_latency(B3, 500),
    Status3 = ln_ctrl_budget:status(B3),
    ?assertEqual(3, maps:get(effects_used, Status3)),
    ?assertEqual(30.0, maps:get(cost_used_usd, Status3)),
    ?assertEqual(false, maps:get(exceeded, Status3)),

    %% Effect 4: exceeds effects
    {budget_exceeded, {max_effects_exceeded, 4, 3}, ExceededByEffects} =
        ln_ctrl_budget:check_effect(B3, 0.0),
    ?assertEqual(true, maps:get(exceeded, ln_ctrl_budget:status(ExceededByEffects))),

    {comment, "budget exhaustion sequence verified"}.
