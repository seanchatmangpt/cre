%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Contributors
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @module ln_ctrl_e2e_SUITE
%% @doc End-to-End Test Suite for ln_ctrl Workflow Execution
%%
%% This module contains comprehensive end-to-end tests for the ln_ctrl
%% workflow orchestration system, testing:
%%
%% Lifecycle Tests:
%% - W01: Complete workflow lifecycle - start, execute steps, complete normally
%% - W02: Workflows with parallel branches (split/join patterns)
%% - W03: Workflows with cancellation scenarios
%% - W04: Workflows with budget enforcement
%% - W05: Workflows with error handling and recovery
%% - W06: Workflows that generate and validate receipts
%% - W07: Andon status signaling throughout execution
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_ctrl_e2e_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test callbacks
-export([all/0, init_per_suitegroup/1, end_per_suitegroup/2]).

%%====================================================================
%% Common Test Callbacks
%%====================================================================

-spec all() -> [{group(), all()}.
all() ->
    [
        {group, workflow_lifecycle},
        {group, parallel_branches},
        {group, cancellation_scenarios},
        {group, budget_enforcement},
        {group, error_handling},
        {group, receipt_generation},
        {group, andon_signaling}
    ].

-spec init_per_suitegroup(group()) -> [term()] | skip.
init_per_suitegroup(Group) ->
    setup_group(Group).

-spec end_per_suitegroup(group(), [term()]) -> ok | {save_config, atom()}.
end_per_suitegroup(_Group, Config) ->
    cleanup_group(Config).

%%====================================================================
%% Group Definitions
%%====================================================================

groups() ->
    [
        {workflow_lifecycle, [], workflow_lifecycle_tests()},
        {parallel_branches, [], parallel_branches_tests()},
        {cancellation_scenarios, [], cancellation_tests()},
        {budget_enforcement, [], budget_tests()},
        {error_handling, [], error_handling_tests()},
        {receipt_generation, [], receipt_tests()},
        {andon_signaling, [], andon_tests()}
    ].

%% Test Lists
workflow_lifecycle_tests() ->
    [
        {"W01-01: Start and complete simple workflow",
         fun test_w01_01_simple_complete/0},

        {"W01-02: Multi-step sequential execution",
         fun test_w01_02_multi_step_seq/0},

        {"W01-03: Workflow with context passing",
         fun test_w01_03_context_passing/0},

        {"W01-04: Workflow status during execution",
         fun test_w01_04_status_during_exec/0},

        {"W01-05: Workflow completion with result",
         fun test_w01_05_completion_with_result/0},

        {"W01-06: Trace history captured",
         fun test_w01_06_trace_captured/0},

        {"W01-07: Timeout on await",
         fun test_w01_07_await_timeout/0}
    ].

parallel_branches_tests() ->
    [
        {"W02-01: Parallel split with two branches",
         fun test_w02_01_par_two_branches/0},

        {"W02-02: Parallel join waits for all",
         fun test_w02_02_par_join_all/0},

        {"W02-03: N-of-M join policy",
         fun test_w02_03_n_of_m_join/0},

        {"W02-04: First-to-complete join",
         fun test_w02_04_first_join/0},

        {"W02-05: Nested parallel constructs",
         fun test_w02_05_nested_parallel/0},

        {"W02-06: Choice pattern execution",
         fun test_w02_06_choice_pattern/0},

        {"W02-07: Loop construct execution",
         fun test_w02_07_loop_construct/0},

        {"W02-08: Defer external choice",
         fun test_w02_08_defer_pattern/0},

        {"W02-09: Scope isolation in parallel",
         fun test_w02_09_scope_isolation/0},

        {"W02-10: Data flow across branches",
         fun test_w02_10_data_flow_across/0}
    ].

cancellation_tests() ->
    [
        {"W03-01: Cancel entire case",
         fun test_w03_01_cancel_case/0},

        {"W03-02: Cancel specific scope",
         fun test_w03_02_cancel_scope/0},

        {"W03-03: Nested scope cancellation",
         fun test_w03_03_nested_cancel/0},

        {"W03-04: Cancel during parallel execution",
         fun test_w03_04_cancel_during_par/0},

        {"W03-05: Cancel signal propagation",
         fun test_w03_05_cancel_propagation/0},

        {"W03-06: Idempotent cancel calls",
         fun test_w03_06_idempotent_cancel/0},

        {"W03-07: Cancel after completion",
         fun test_w03_07_cancel_after_complete/0},

        {"W03-08: Effect filtering after cancel",
         fun test_w03_08_effect_filtering/0}
    ].

budget_tests() ->
    [
        {"W04-01: Execute within effect budget",
         fun test_w04_01_within_effect_budget/0},

        {"W04-02: Execute within latency budget",
         fun test_w04_02_within_latency_budget/0},

        {"W04-03: Execute within cost budget",
         fun test_w04_03_within_cost_budget/0},

        {"W04-04: Effect limit exceeded halts",
         fun test_w04_04_effect_exceeded/0},

        {"W04-05: Latency limit exceeded",
         fun test_w04_05_latency_exceeded/0},

        {"W04-06: Unlimited budget allows many effects",
         fun test_w04_06_unlimited_budget/0},

        {"W04-07: Budget status reporting",
         fun test_w04_07_budget_status/0},

        {"W04-08: Combined budget limits",
         fun test_w04_08_combined_limits/0},

        {"W04-09: Budget enforcement at effect boundary",
         fun test_w04_09_boundary_enforcement/0}
    ].

error_handling_tests() ->
    [
        {"W05-01: Effect failure propagates",
         fun test_w05_01_effect_failure_propagates/0},

        {"W05-02: Error in sequential flow",
         fun test_w05_02_error_in_seq/0},

        {"W05-03: Error in parallel branch",
         fun test_w05_03_error_in_par/0},

        {"W05-04: Continue after recoverable error",
         fun test_w05_04_recoverable_error/0},

        {"W05-05: Halt on critical error",
         fun test_w05_05_critical_error_halt/0},

        {"W05-06: Error context preserved",
         fun test_w05_06_error_context/0},

        {"W05-07: Multiple error handling",
         fun test_w05_07_multiple_errors/0},

        {"W05-08: Error recovery with receipts",
         fun test_w05_08_error_recovery_receipts/0}
    ].

receipt_tests() ->
    [
        {"W06-01: Build receipt generation",
         fun test_w06_01_build_receipt/0},

        {"W06-02: Effect receipt creation",
         fun test_w06_02_effect_receipt/0},

        {"W06-03: Receipt chain validation",
         fun test_w06_03_chain_validation/0},

        {"W06-04: Receipt hash integrity",
         fun test_w06_04_hash_integrity/0},

        {"W06-05: Tamper detection",
         fun test_w06_05_tamper_detection/0},

        {"W06-06: Receipt idempotency",
         fun test_w06_06_idempotency/0},

        {"W06-07: Receipt timestamp ordering",
         fun test_w06_07_timestamp_order/0}
    ].

andon_tests() ->
    [
        {"W07-01: Andon green on normal execution",
         fun test_w07_01_andon_green/0},

        {"W07-02: Andon yellow on warning",
         fun test_w07_02_andon_yellow/0},

        {"W07-03: Andon red on error",
         fun test_w07_03_andon_red/0},

        {"W07-04: Andon red on budget exceeded",
         fun test_w07_04_andon_red_budget/0},

        {"W07-05: Andon status transitions",
         fun test_w07_05_andon_transitions/0},

        {"W07-06: Andon status retrieval",
         fun test_w07_06_andon_status/0},

        {"W07-07: Multiple andon instances",
         fun test_w07_07_multiple_andon/0},

        {"W07-08: Andon HTTP exposure",
         fun test_w07_08_andon_http/0}
    ].

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup_group(Group) ->
    [
        {group, Group},
        {test_pid, undefined},
        {andon_handle, undefined},
        {receipt_log, []},
        {ets_tables, []}
    ].

cleanup_group(Config) ->
    %% Cleanup any spawned processes
    case proplists:get_value(test_pid, Config) of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> exit(Pid, normal);
                false -> ok
            end
    end,

    %% Cleanup ETS tables
    Tables = proplists:get_value(ets_tables, Config, []),
    lists:foreach(fun(T) ->
        case ets:whereis(T) of
            undefined -> ok;
            _Tid -> ets:delete(T)
        end
    end, Tables),

    ok.

%%====================================================================
%% W01: Workflow Lifecycle Tests
%%====================================================================

test_w01_01_simple_complete(Config) ->
    %% Test: Start and complete a simple workflow
    Plan = ln_plan:task(simple_task),
    InitCtx = #{input => test_data},

    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Await completion
    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),
    {ok, FinalCtx} = Result,

    %% Verify final context
    ?assertEqual(test_data, maps:get(input, FinalCtx)),

    %% Store pid for cleanup
    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w01_02_multi_step_seq(Config) ->
    %% Test: Multi-step sequential execution
    Plan = ln_plan:seq([
        ln_plan:task(step1),
        ln_plan:task(step2),
        ln_plan:task(step3)
    ]),

    InitCtx = #{counter => 0},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Await completion
    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w01_03_context_passing(Config) ->
    %% Test: Workflow with context passing
    Plan = ln_plan:seq([
        ln_plan:task(add_value),
        ln_plan:task(multiply_value),
        ln_plan:task(finalize)
    ]),

    InitCtx = #{value => 10},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, #{value := _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w01_04_status_during_exec(Config) ->
    %% Test: Workflow status during execution
    Plan = ln_plan:seq([
        ln_plan:task(long_task)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Give it a moment to start
    timer:sleep(100),

    %% Get status
    Status = get_case_status(CaseId),

    ?assert(is_map(Status)),
    ?assert(maps:is_key(state, Status)),
    ?assert(maps:is_key(steps, Status)),

    cleanup_test_case(CaseId),
    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w01_05_completion_with_result(Config) ->
    %% Test: Workflow completion with result
    Plan = ln_plan:seq([
        ln_plan:task(compute)
    ]),

    InitCtx = #{x => 5, y => 3},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, #{result := _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w01_06_trace_captured(Config) ->
    %% Test: Trace history captured
    Plan = ln_plan:seq([
        ln_plan:task(trace_task1),
        ln_plan:task(trace_task2)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{trace_level => full}),

    await_test_case(CaseId, 5000),

    %% Get trace
    Trace = get_case_trace(CaseId, 0, 100),

    ?assert(is_list(Trace)),
    ?assert(length(Trace) > 0),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w01_07_await_timeout(Config) ->
    %% Test: Timeout on await
    Plan = ln_plan:seq([
        ln_plan:task(slow_task)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Short timeout
    Result = await_test_case(CaseId, 100),

    ?assertEqual(timeout, Result),

    cleanup_test_case(CaseId),
    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

%%====================================================================
%% W02: Parallel Branches Tests
%%====================================================================

test_w02_01_par_two_branches(Config) ->
    %% Test: Parallel split with two branches
    Plan = ln_plan:par([
        ln_plan:task(branch_a),
        ln_plan:task(branch_b)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_02_par_join_all(Config) ->
    %% Test: Parallel join waits for all
    Plan = ln_plan:join(all, [
        ln_plan:task(join_branch_a),
        ln_plan:task(join_branch_b),
        ln_plan:task(join_branch_c)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_03_n_of_m_join(Config) ->
    %% Test: N-of-M join policy
    Plan = ln_plan:join({n_of_m, 2}, [
        ln_plan:task(n_branch_1),
        ln_plan:task(n_branch_2),
        ln_plan:task(n_branch_3)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_04_first_join(Config) ->
    %% Test: First-to-complete join
    Plan = ln_plan:join(first, [
        ln_plan:task(race_branch_1),
        ln_plan:task(race_branch_2),
        ln_plan:task(race_branch_3)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_05_nested_parallel(Config) ->
    %% Test: Nested parallel constructs
    InnerPar = ln_plan:par([
        ln_plan:task(inner_a),
        ln_plan:task(inner_b)
    ]),

    Plan = ln_plan:seq([
        ln_plan:task(outer),
        InnerPar,
        ln_plan:task(final)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_06_choice_pattern(Config) ->
    %% Test: Choice pattern execution
    Plan = ln_plan:choice([
        ln_plan:task(choice_a),
        ln_plan:task(choice_b)
    ]),

    InitCtx = #{select => a},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_07_loop_construct(Config) ->
    %% Test: Loop construct execution
    Plan = ln_plan:loop({times, 3}, ln_plan:task(loop_body)),

    InitCtx = #{counter => 0},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, #{counter := _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_08_defer_pattern(Config) ->
    %% Test: Defer external choice
    Plan = ln_plan:defer([
        ln_plan:task(defer_opt1),
        ln_plan:task(defer_opt2)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_09_scope_isolation(Config) ->
    %% Test: Scope isolation in parallel
    Plan = ln_plan:seq([
        ln_plan:scope(scope1, ln_plan:task(scoped_task)),
        ln_plan:task(outside_task)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w02_10_data_flow_across(Config) ->
    %% Test: Data flow across branches
    Plan = ln_plan:seq([
        ln_plan:task(initiator),
        ln_plan:par([
            ln_plan:task(parallel_a),
            ln_plan:task(parallel_b)
        ]),
        ln_plan:task(aggregator)
    ]),

    InitCtx = #{data => []},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, #{data := _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

%%====================================================================
%% W03: Cancellation Tests
%%====================================================================

test_w03_01_cancel_case(Config) ->
    %% Test: Cancel entire case
    Plan = ln_plan:seq([
        ln_plan:task(long_running),
        ln_plan:task(never_completes)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Let it start
    timer:sleep(100),

    %% Cancel the case
    cancel_test_case(CaseId),

    Result = await_test_case(CaseId, 1000),

    ?assertMatch({error, cancelled}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w03_02_cancel_scope(Config) ->
    %% Test: Cancel specific scope
    Plan = ln_plan:seq([
        ln_plan:scope(cancelable_scope, ln_plan:task(inner_task)),
        ln_plan:task(outer_task)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Let it start
    timer:sleep(100),

    %% Cancel specific scope
    cancel_scope_test(CaseId, cancelable_scope),

    Result = await_test_case(CaseId, 2000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w03_03_nested_cancel(Config) ->
    %% Test: Nested scope cancellation
    InnerScope = ln_plan:scope(inner, ln_plan:task(inner_task)),

    Plan = ln_plan:seq([
        ln_plan:scope(outer, InnerScope),
        ln_plan:task(final_task)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    timer:sleep(100),

    %% Cancel outer scope (should cancel inner too)
    cancel_scope_test(CaseId, outer),

    Result = await_test_case(CaseId, 2000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w03_04_cancel_during_par(Config) ->
    %% Test: Cancel during parallel execution
    Plan = ln_plan:par([
        ln_plan:task(par_a),
        ln_plan:task(par_b),
        ln_plan:task(par_c)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    timer:sleep(100),

    %% Cancel case
    cancel_test_case(CaseId),

    Result = await_test_case(CaseId, 1000),

    ?assertMatch({error, cancelled}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w03_05_cancel_propagation(Config) ->
    %% Test: Cancel signal propagation
    Plan = ln_plan:seq([
        ln_plan:scope(prop_scope, ln_plan:task(propagate_task)),
        ln_plan:task(after_scope)
    ]),

    InitCtx = #{cancelled => false},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    timer:sleep(100),

    %% Cancel and verify propagation
    cancel_test_case(CaseId),

    Result = await_test_case(CaseId, 1000),

    ?assertMatch({error, cancelled}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w03_06_idempotent_cancel(Config) ->
    %% Test: Idempotent cancel calls
    Plan = ln_plan:task(simple),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Cancel multiple times
    ok = cancel_test_case(CaseId),
    ok = cancel_test_case(CaseId),

    Result = await_test_case(CaseId, 1000),

    ?assertMatch({error, cancelled}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w03_07_cancel_after_complete(Config) ->
    %% Test: Cancel after completion
    Plan = ln_plan:task(completes_quickly),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    %% Wait for completion
    timer:sleep(500),

    %% Try to cancel (should be no-op)
    ok = cancel_test_case(CaseId),

    Result = await_test_case(CaseId, 100),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w03_08_effect_filtering(Config) ->
    %% Test: Effect filtering after cancel
    Plan = ln_plan:seq([
        ln_plan:scope(eff_scope, ln_plan:task(effect_task)),
        ln_plan:task(no_effect_task)
    ]),

    InitCtx = #{effects => []},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    timer:sleep(100),

    %% Cancel scope
    cancel_scope_test(CaseId, eff_scope),

    Result = await_test_case(CaseId, 1000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

%%====================================================================
%% W04: Budget Enforcement Tests
%%====================================================================

test_w04_01_within_effect_budget(Config) ->
    %% Test: Execute within effect budget
    Plan = ln_plan:seq([
        ln_plan:task(budgeted_task)
    ]),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(100, unlimited, unlimited),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w04_02_within_latency_budget(Config) ->
    %% Test: Execute within latency budget
    Plan = ln_plan:seq([
        ln_plan:task(fast_task)
    ]),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(unlimited, 5000, unlimited),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w04_03_within_cost_budget(Config) ->
    %% Test: Execute within cost budget
    Plan = ln_plan:task(low_cost_task),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 100.0),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w04_04_effect_exceeded(Config) ->
    %% Test: Effect limit exceeded halts
    Plan = ln_plan:seq([
        ln_plan:task(task_consuming_effects)
    ]),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(2, unlimited, unlimited),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({error, {budget_exceeded, _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w04_05_latency_exceeded(Config) ->
    %% Test: Latency limit exceeded
    Plan = ln_plan:task(very_slow_task),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(unlimited, 10, unlimited),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 2000),

    ?assertMatch({error, {timeout, _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w04_06_unlimited_budget(Config) ->
    %% Test: Unlimited budget allows many effects
    Plan = ln_plan:loop({times, 100}, ln_plan:task(unlimited_task)),

    InitCtx = #{count => 0},
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, unlimited),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, #{count := 100}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w04_07_budget_status(Config) ->
    %% Test: Budget status reporting
    Plan = ln_plan:task(status_task),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(50, 10000, 10.0),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    %% Get status during execution
    timer:sleep(100),
    Status = get_case_status(CaseId),

    ?assert(maps:is_key(budget_status, Status)),

    cleanup_test_case(CaseId),
    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w04_08_combined_limits(Config) ->
    %% Test: Combined budget limits
    Plan = ln_plan:seq([
        ln_plan:task(multi_budget_task)
    ]),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(10, 5000, 5.0),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w04_09_boundary_enforcement(Config) ->
    %% Test: Budget enforcement at effect boundary
    Plan = ln_plan:seq([
        ln_plan:task(exactly_at_limit)
    ]),

    InitCtx = #{},
    Budget = ln_ctrl_budget:new_budget(5, unlimited, unlimited),

    CaseId = start_test_case(Plan, InitCtx, #{budget => Budget}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

%%====================================================================
%% W05: Error Handling Tests
%%====================================================================

test_w05_01_effect_failure_propagates(Config) ->
    %% Test: Effect failure propagates
    Plan = ln_plan:seq([
        ln_plan:task(failing_effect)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({error, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w05_02_error_in_seq(Config) ->
    %% Test: Error in sequential flow
    Plan = ln_plan:seq([
        ln_plan:task(ok_step),
        ln_plan:task(error_step),
        ln_plan:task(not_reached)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({error, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w05_03_error_in_par(Config) ->
    %% Test: Error in parallel branch
    Plan = ln_plan:join(all, [
        ln_plan:task(ok_branch),
        ln_plan:task(error_branch),
        ln_plan:task(another_ok)
    ]),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({error, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w05_04_recoverable_error(Config) ->
    %% Test: Continue after recoverable error
    Plan = ln_plan:seq([
        ln_plan:task(retryable_task)
    ]),

    InitCtx = #{attempts => 0},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({ok, #{attempts := _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, CaseId}).

test_w05_05_critical_error_halt(Config) ->
    %% Test: Halt on critical error
    Plan = ln_plan:task(critical_error_task),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({error, critical}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w05_06_error_context(Config) ->
    %% Test: Error context preserved
    Plan = ln_plan:task(contextual_error),

    InitCtx = #{},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    ?assertMatch({error, #{error_context := _}}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w05_07_multiple_errors(Config) ->
    %% Test: Multiple error handling
    Plan = ln_plan:seq([
        ln_plan:task(first_error),
        ln_plan:task(second_error)
    ]),

    InitCtx = #{errors => []},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    %% Should get first error
    ?assertMatch({error, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

test_w05_08_error_recovery_receipts(Config) ->
    %% Test: Error recovery with receipts
    Plan = ln_plan:seq([
        ln_plan:task(receipt_error_task)
    ]),

    InitCtx = #{receipts => []},
    CaseId = start_test_case(Plan, InitCtx, #{}),

    Result = await_test_case(CaseId, 5000),

    %% Even on error, receipts should be generated
    ?assertMatch({error, _}, Result),

    lists:keyreplace(test_pid, 1, Config, {test_pid, undefined}).

%%====================================================================
%% W06: Receipt Generation Tests
%%====================================================================

test_w06_01_build_receipt(Config) ->
    %% Test: Build receipt generation
    InputOntology = #{type => test_workflow},
    Templates = #{template => test_template},
    Params = #{param => value},

    Receipt = ln_ctrl_receipt:build_receipt(InputOntology, Templates, Params),

    ?assertEqual(build, element(2, Receipt)),
    ?assert(is_reference(element(3, Receipt))),

    %% Validate input/output hashes
    ?assert(byte_size(element(5, Receipt)) == 8),  % input_hash
    ?assert(byte_size(element(6, Receipt)) == 8),  % output_hash

    Config.

test_w06_02_effect_receipt(Config) ->
    %% Test: Effect receipt creation
    EffectID = test_effect,
    Result = #{result => success, data => test_data},

    Receipt = ln_ctrl_receipt:effect_receipt(EffectID, Result),

    ?assertEqual(effect, element(2, Receipt)),
    ?assertEqual(test_effect, element(4, Receipt)),
    ?assertEqual(Result, element(7, Receipt)),
    ?assert(is_reference(element(3, Receipt))),

    Config.

test_w06_03_chain_validation(Config) ->
    %% Test: Receipt chain validation
    R1 = ln_ctrl_receipt:build_receipt(#{}, #{}, #{}),
    R2 = ln_ctrl_receipt:effect_receipt(eff1, ok),

    %% Link R2 to R1
    R2Linked = setelement(7, R2, element(5, R1)),

    Receipts = [R1, R2Linked],

    Result = ln_ctrl_receipt:validate_chain(Receipts),

    ?assertMatch({ok, true}, Result),

    Config.

test_w06_04_hash_integrity(Config) ->
    %% Test: Receipt hash integrity
    InputData = #{key => value},
    Templates = #{tmpl => data},
    Params = #{p => v},

    R1 = ln_ctrl_receipt:build_receipt(InputData, Templates, Params),

    %% Same input should produce same hash
    R2 = ln_ctrl_receipt:build_receipt(InputData, Templates, Params),

    ?assertEqual(element(5, R1), element(5, R2)),  % input_hash
    ?assertEqual(element(6, R1), element(6, R2)),  % output_hash

    Config.

test_w06_05_tamper_detection(Config) ->
    %% Test: Tamper detection
    R1 = ln_ctrl_receipt:build_receipt(#{}, #{}, #{}),

    %% Create tampered version
    OriginalHash = element(6, R1),
    TamperedHash = <<0:64>>,
    R1Tampered = setelement(6, R1, TamperedHash),

    %% Chain should fail validation
    Receipts = [R1, R1Tampered],
    Result = ln_ctrl_receipt:validate_chain(Receipts),

    ?assertMatch({error, hash_chain_broken, _}, Result),

    Config.

test_w06_06_idempotency(Config) ->
    %% Test: Receipt idempotency
    EffectID = idempotent_effect,
    Input1 = #{input => 1},
    Input2 = #{input => 1},  % Same input

    %% Create hash from input
    InputHash = erlang:phash2(Input1),

    %% Check idempotency cache
    CacheKey = {EffectID, InputHash},

    %% Not cached initially
    ?assertEqual(not_found, ln_ctrl_receipt:idempotent_receipt(EffectID, InputHash)),

    Config.

test_w06_07_timestamp_order(Config) ->
    %% Test: Receipt timestamp ordering
    T1 = ln_ctrl_receipt:build_receipt(#{}, #{}, #{}),

    %% Wait a bit
    timer:sleep(10),

    T2 = ln_ctrl_receipt:build_receipt(#{}, #{}, #{}),

    ?assert(element(8, T2) > element(8, T1)),

    Config.

%%====================================================================
%% W07: Andon Signaling Tests
%%====================================================================

test_w07_01_andon_green(Config) ->
    %% Test: Andon green on normal execution
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    ok = ln_receipt_andon:set_green(AndonHandle),

    {Color, Details} = ln_receipt_andon:status(AndonHandle),

    ?assertEqual(green, Color),
    ?assertEqual(true, maps:get(nominal, Details)),

    Config.

test_w07_02_andon_yellow(Config) ->
    %% Test: Andon yellow on warning
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    Warnings = [high_latency, resource_low],

    ok = ln_receipt_andon:set_yellow(AndonHandle, Warnings),

    {Color, Details} = ln_receipt_andon:status(AndonHandle),

    ?assertEqual(yellow, Color),
    ?assertEqual(Warnings, maps:get(warnings, Details)),

    Config.

test_w07_03_andon_red(Config) ->
    %% Test: Andon red on error
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    HaltReason = budget_exceeded,

    ok = ln_receipt_andon:set_red(AndonHandle, HaltReason),

    {Color, Details} = ln_receipt_andon:status(AndonHandle),

    ?assertEqual(red, Color),
    ?assertEqual(HaltReason, maps:get(halt_reason, Details)),

    Config.

test_w07_04_andon_red_budget(Config) ->
    %% Test: Andon red on budget exceeded
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    BudgetReason = max_effects_exceeded,

    ok = ln_receipt_andon:set_red(AndonHandle, BudgetReason),

    {Color, Details} = ln_receipt_andon:status(AndonHandle),

    ?assertEqual(red, Color),
    ?assert(true, maps:get(critical, Details)),

    Config.

test_w07_05_andon_transitions(Config) ->
    %% Test: Andon status transitions
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    ?assertEqual(green, element(1, ln_receipt_andon:status(AndonHandle))),

    ok = ln_receipt_andon:set_yellow(AndonHandle, [warning]),

    ?assertEqual(yellow, element(1, ln_receipt_andon:status(AndonHandle))),

    ok = ln_receipt_andon:set_red(AndonHandle, error),

    ?assertEqual(red, element(1, ln_receipt_andon:status(AndonHandle))),

    ok = ln_receipt_andon:set_green(AndonHandle),

    ?assertEqual(green, element(1, ln_receipt_andon:status(AndonHandle))),

    Config.

test_w07_06_andon_status(Config) ->
    %% Test: Andon status retrieval
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    ok = ln_receipt_andon:set_green(AndonHandle),

    {Color, Details} = ln_receipt_andon:status(AndonHandle),

    ?assert(is_atom(Color)),
    ?assert(is_map(Details)),
    ?assert(maps:is_key(timestamp, Details)),

    Config.

test_w07_07_multiple_andon(Config) ->
    %% Test: Multiple andon instances
    {ok, Andon1} = ln_receipt_andon:new_andon(),
    {ok, Andon2} = ln_receipt_andon:new_andon(),

    ok = ln_receipt_andon:set_green(Andon1),
    ok = ln_receipt_andon:set_red(Andon2, error),

    {Color1, _} = ln_receipt_andon:status(Andon1),
    {Color2, _} = ln_receipt_andon:status(Andon2),

    ?assertEqual(green, Color1),
    ?assertEqual(red, Color2),

    Config.

test_w07_08_andon_http(Config) ->
    %% Test: Andon HTTP exposure
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    ok = ln_receipt_andon:expose_http(AndonHandle, 8080),

    %% Verify the registry was created
    ?assertEqual(ok, ln_receipt_andon:expose_http(AndonHandle, 8080)),

    Config.

%%====================================================================
%% Helper Functions for Test Execution
%%====================================================================

%% @doc Start a test case using a simple callback module
start_test_case(Plan, InitCtx, Options) ->
    %% For E2E tests, we use ln_ctrl:new_case which returns a PID
    case ln_ctrl:new_case(Plan, InitCtx, Options) of
        {ok, CaseId} when is_pid(CaseId) ->
            CaseId;
        {error, Reason} ->
            ct:fail({start_failed, Reason})
    end.

%% @doc Wait for case completion
await_test_case(CaseId, Timeout) ->
    ln_ctrl:await(CaseId, Timeout).

%% @doc Get case status
get_case_status(CaseId) ->
    ln_ctrl:status(CaseId).

%% @doc Get case trace
get_case_trace(CaseId, From, To) ->
    ln_ctrl:trace(CaseId, From, To).

%% @doc Cancel a test case
cancel_test_case(CaseId) ->
    ln_ctrl:cancel(CaseId).

%% @doc Cancel specific scope
cancel_scope_test(CaseId, ScopeId) ->
    ln_ctrl:cancel_scope(CaseId, ScopeId).

%% @doc Cleanup test case
cleanup_test_case(CaseId) ->
    case is_process_alive(CaseId) of
        true -> exit(CaseId, kill);
        false -> ok
    end.

%% @doc Check if process is alive
is_process_alive(Pid) when is_pid(Pid) ->
    erlang:is_process_alive(Pid);
is_process_alive(_) ->
    false.
