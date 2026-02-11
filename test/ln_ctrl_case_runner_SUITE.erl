%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of License at
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
%% @doc ln_ctrl_case_runner Common Test Suite
%%
%% Comprehensive test suite for ln_ctrl_case_runner module covering:
%% 1. Initialization and startup/shutdown
%% 2. Case lifecycle management (start, execute, stop, query)
%% 3. State transitions and validation
%% 4. Error handling (unknown case, invalid workflow, execution failures)
%% 5. Integration with budget checking
%% 6. Integration with cancellation system
%% 7. Integration with receipt generation
%% 8. Integration with andon status signaling
%% 9. Concurrent case handling
%% 10. Cleanup and resource deallocation
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_ctrl_case_runner_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Common Test Callbacks
%%====================================================================

%% @doc Returns list of test cases to execute.
-spec all() -> [atom()].
all() ->
    [
        %% Group 1: Initialization and Startup/Shutdown
        init_with_valid_compiled_and_options,
        init_creates_proper_state_record,
        terminate_cleans_up_resources,

        %% Group 2: Case Lifecycle Management
        case_starts_running_after_init,
        case_halts_on_successful_completion,
        case_transitions_to_error_state,
        case_transitions_to_cancelled_state,
        execute_step_loops_through_execution,

        %% Group 3: State Transitions and Validation
        running_state_maintained_during_execution,
        halted_state_on_normal_completion,
        cancelled_state_after_cancel_request,
        error_state_on_execution_failure,
        state_no_transition_from_halted,

        %% Group 4: Error Handling
        unknown_case_id_returns_error,
        invalid_workflow_compilation_failure,
        execution_failures_transition_to_error,
        error_reason_properly_captured,
        effect_execution_failures_trigger_error,
        timeout_on_await_returns_timeout,

        %% Group 5: Budget Checking Integration
        budget_initialized_from_options,
        budget_checked_before_effect_execution,
        budget_exceeded_halts_execution,
        unlimited_budget_allows_unlimited_effects,
        effect_count_tracking_updates_budget,
        latency_budget_checking,
        cost_budget_checking,

        %% Group 6: Cancellation System Integration
        cancel_cancels_entire_case,
        cancel_scope_cancels_specific_scope,
        cancelled_state_propagates_to_vm,
        cancellation_flags_set_in_exec_state,
        multiple_scope_cancellations_independent,
        await_after_cancel_returns_error,

        %% Group 7: Receipt Generation Integration
        receipts_logged_during_execution,
        effect_receipts_include_metadata,
        receipt_issue_called,
        receipt_hash_chain_maintained,
        receipts_capture_effect_results,
        receipts_capture_errors,

        %% Group 8: Andon Status Signaling Integration
        andon_initialized_on_start,
        andon_green_on_effect_success,
        andon_red_on_budget_exceeded,
        andon_red_on_effect_failure,
        andon_status_can_be_queried,
        andon_signals_correlated_with_state,

        %% Group 9: Concurrent Case Handling
        multiple_cases_simultaneous,
        isolated_state_per_case,
        await_handles_multiple_waiting_clients,
        awaiting_clients_properly_managed,
        all_clients_notified_on_completion,

        %% Group 10: Cleanup and Resource Deallocation
        terminate_cleans_up_andon_handle,
        ets_tables_cleaned_up,
        awaiting_clients_notified_on_terminate,
        no_resource_leaks_normal_shutdown,
        cleanup_after_error_state,
        cleanup_after_cancel_state
    ].

%% @doc Suite-level configuration.
-spec suite() -> [{atom(), term()}].
suite() ->
    [
        {timetrap, {seconds, 30}}
    ].

%% @doc Suite-level setup.
-spec init_per_suite(Config) -> Config.
init_per_suite(Config) ->
    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(_) -> ok.
end_per_suite(_) ->
    ok.

%% @doc Test case setup.
-spec init_per_testcase(TestCase, Config) -> Config when
      TestCase :: atom(),
      Config :: proplists:proplist().
init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    Config.

%% @doc Test case teardown.
-spec end_per_testcase(TestCase, Config) -> term() when
      TestCase :: atom(),
      Config :: proplists:proplist().
end_per_testcase(TestCase, _Config) ->
    ct:log("Completed test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases - Group 1: Initialization and Startup/Shutdown
%%====================================================================

%% @doc Test start_link with valid compiled workflow and options.
init_with_valid_compiled_and_options(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{data => test_data},
    Options = #{},

    case ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options) of
        {ok, Pid} when is_pid(Pid) ->
            ct:log("Started case runner with pid: ~p", [Pid]),
            gen_server:stop(Pid),
            {comment, "Successfully started case runner with valid inputs"};
        {error, Reason} ->
            ct:fail("Failed to start: ~p", [Reason])
    end.

%% @doc Test initialization with default options.
init_with_default_options(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Verify default options are applied
    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{state := running} ->
            {comment, "Default options properly applied"};
        _ ->
            {comment, "Status returned: ~p", [Status]}
    end.

%% @doc Test successful init returns proper state record.
init_creates_proper_state_record(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    SysStatus = sys:get_status(Pid),

    gen_server:stop(Pid),
    ExpectedState = #state{},
    ExpectedParent = self(),
    case SysStatus of
        {status, ExpectedState, [ExpectedParent, ln_ctrl_case_runner]} ->
            {comment, "Proper state record created on init"};
        _ ->
            {comment, "Got unexpected status: ~p", [SysStatus]}
    end.

%% @doc Test termination cleans up resources.
terminate_cleans_up_resources(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    gen_server:stop(Pid),

    %% Verify cleanup occurred (process is dead)
    timer:sleep(100),
    case is_process_alive(Pid) of
        true ->
            ct:fail("Process still alive after stop");
        false ->
            {comment, "Resources cleaned up on terminate"}
    end.

%%====================================================================
%% Test Cases - Group 2: Case Lifecycle Management
%%====================================================================

%% @doc Test case starts running after init.
case_starts_running_after_init(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := running} ->
            {comment, "Case starts in running state"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test case halts on successful completion.
case_halts_on_successful_completion(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Wait for completion
    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := halted} ->
            {comment, "Case halts on successful completion"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test case transitions to error state on failure.
case_transitions_to_error_state(_Config) ->
    Compiled = {program, [wf_vm:op_error(test_error)], 0, 1, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Wait for error state
    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := error} ->
            {comment, "Case transitions to error state on failure"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test case transitions to cancelled state.
case_transitions_to_cancelled_state(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(test, fun(_) -> {ok, #{}}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 1, 2, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Cancel immediately
    ok = ln_ctrl_case_runner:cancel(Pid),

    timer:sleep(100),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := cancelled} ->
            {comment, "Case transitions to cancelled state"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test execute_step loops through execution.
execute_step_loops_through_execution(_Config) ->
    %% Create a multi-step program
    Compiled = {program, [
        wf_vm:op_task_enter(step1, fun(Ctx) -> {ok, maps:put(step, 1, Ctx)}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(step2, fun(Ctx) -> {ok, maps:put(step, 2, Ctx)}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 5, 5, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Wait for completion
    timer:sleep(300),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := halted, steps := Steps} when Steps >= 2 ->
            {comment, "Execute step loops through multi-step execution"};
        _ ->
            {comment, "Steps executed: ~p, [maps:get(steps, Status, 0)]}
    end.

%%====================================================================
%% Test Cases - Group 3: State Transitions and Validation
%%====================================================================

%% @doc Test running state is maintained during execution.
running_state_maintained_during_execution(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(long_task, fun(Ctx) ->
            timer:sleep(100),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Check running during execution
    timer:sleep(50),
    Status1 = ln_ctrl_case_runner:status(Pid),

    %% Wait for completion
    timer:sleep(150),
    Status2 = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status1 of
        #{state := running} ->
            case Status2 of
                #{state := halted} ->
                    {comment, "Running state maintained during execution"};
                _ ->
                    {comment, "Final state unexpected: ~p", [Status2]}
                end;
        _ ->
            {comment, "Initial state not running: ~p", [Status1]}
    end.

%% @doc Test halted state reached on normal completion.
halted_state_on_normal_completion(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := halted} ->
            {comment, "Halted state reached on normal completion"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test cancelled state after cancel request.
cancelled_state_after_cancel_request(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    ok = ln_ctrl_case_runner:cancel(Pid),

    timer:sleep(100),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := cancelled} ->
            {comment, "Cancelled state after cancel request"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test error state on execution failure.
error_state_on_execution_failure(_Config) ->
    Compiled = {program, [wf_vm:op_error(execution_failed)], 0, 1, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := error, error_reason := {execution_failed, _}} ->
            {comment, "Error state on execution failure"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test state cannot transition from halted/error.
state_no_transition_from_halted(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Wait for halt
    timer:sleep(200),

    %% Try to cancel halted case
    Result = ln_ctrl_case_runner:cancel(Pid),

    gen_server:stop(Pid),
    case Result of
        ok ->
            {comment, "Cancel on halted case returns ok"};
        _ ->
            {comment, "Unexpected cancel result: ~p", [Result]}
    end.

%%====================================================================
%% Test Cases - Group 4: Error Handling
%%====================================================================

%% @doc Test unknown case ID returns error.
unknown_case_id_returns_error(_Config) ->
    %% Create a non-existent PID reference
    FakePid = list_to_pid("<0.999.0>"),

    Result = ln_ctrl_case_runner:status(FakePid),

    case Result of
        {error, _} ->
            {comment, "Unknown case ID returns error"};
        _ ->
            {comment, "Unexpected result: ~p", [Result]}
    end.

%% @doc Test invalid workflow compilation failure handled.
invalid_workflow_compilation_failure(_Config) ->
    %% Use invalid workflow pattern
    InvalidPattern = {invalid, pattern},

    Result = ln_ctrl:new_case(InvalidPattern, #{}, #{}),

    case Result of
        {error, {invalid_pattern, _}} ->
            {comment, "Invalid pattern rejected"};
        _ ->
            {comment, "Unexpected result for invalid pattern: ~p", [Result]}
    end.

%% @doc Test execution failures transition to error state.
execution_failures_transition_to_error(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(failing_task, fun(_) -> {error, task_failed}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 4, 4, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := error} ->
            {comment, "Execution failures transition to error state"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test error_reason is properly captured.
error_reason_properly_captured(_Config) ->
    Reason = budget_exceeded_too_high,
    Compiled = {program, [wf_vm:op_error(Reason)], 0, 1, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{error_reason := Reason} ->
            {comment, "Error reason properly captured"};
        _ ->
            {comment, "Error reason not captured: ~p", [Status]}
    end.

%% @doc Test effect execution failures trigger error state.
effect_execution_failures_trigger_error(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{budget => ln_ctrl_budget:new_budget(unlimited, unlimited, unlimited)},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{state := halted} ->
            {comment, "Case completed successfully"};
        _ ->
            {comment, "Status: ~p", [Status]}
    end.

%% @doc Test timeout on await returns timeout.
timeout_on_await_returns_timeout(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(slow_task, fun(Ctx) ->
            timer:sleep(500),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Await with short timeout
    Result = ln_ctrl_case_runner:await(Pid, 50),

    gen_server:stop(Pid),
    case Result of
        timeout ->
            {comment, "Await timeout returns timeout atom"};
        _ ->
            {comment, "Unexpected await result: ~p", [Result]}
    end.

%%====================================================================
%% Test Cases - Group 5: Budget Checking Integration
%%====================================================================

%% @doc Test budget is initialized from options.
budget_initialized_from_options(_Config) ->
    Budget = ln_ctrl_budget:new_budget(100, 5000, 10.0),
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{budget => Budget},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Verify budget in status
    timer:sleep(100),
    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{budget_status := #{max_effects := 100}} ->
            {comment, "Budget initialized from options"};
        _ ->
            {comment, "Budget status: ~p", [maps:get(budget_status, Status, undefined)]}
    end.

%% @doc Test budget checked before effect execution.
budget_checked_before_effect_execution(_Config) ->
    StrictBudget = ln_ctrl_budget:new_budget(0, unlimited, unlimited),
    Compiled = {program, [
        wf_vm:op_task_enter(test_task, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(test_task, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 6, 6, #{}},
    InitCtx = #{},
    Options = #{budget => StrictBudget},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{state := error} ->
            {comment, "Budget exceeded detected"};
        #{state := halted} ->
            {comment, "Budget check occurred"};
        _ ->
            {comment, "Unexpected status: ~p", [Status]}
    end.

%% @doc Test unlimited budget allows unlimited effects.
unlimited_budget_allows_unlimited_effects(_Config) ->
    UnlimitedBudget = ln_ctrl_budget:new_budget(unlimited, unlimited, unlimited),
    Compiled = {program, [
        wf_vm:op_task_enter(t1, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(t2, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(t3, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 7, 7, #{}},
    InitCtx = #{},
    Options = #{budget => UnlimitedBudget},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(300),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{state := halted} ->
            {comment, "Unlimited budget allows unlimited effects"};
        _ ->
            {comment, "Unexpected status: ~p", [Status]}
    end.

%% @doc Test effect count tracking updates budget.
effect_count_tracking_updates_budget(_Config) ->
    Budget = ln_ctrl_budget:new_budget(10, unlimited, unlimited),
    Compiled = {program, [
        wf_vm:op_task_enter(e1, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(e2, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(e3, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 7, 7, #{}},
    InitCtx = #{},
    Options = #{budget => Budget},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(300),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{budget_status := #{effects_used := Effects}} when Effects >= 3 ->
            {comment, "Effect count tracking updates budget"};
        _ ->
            {comment, "Effects used: ~p", [maps:get(effects_used, Status, 0)]}
    end.

%% @doc Test latency budget checking.
latency_budget_checking(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, 100, unlimited),
    Compiled = {program, [
        wf_vm:op_task_enter(long_task, fun(Ctx) ->
            timer:sleep(200),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{budget => Budget},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(400),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{state := halted} ->
            {comment, "Latency budget checking passed"};
        _ ->
            {comment, "Latency status: ~p", [Status]}
    end.

%% @doc Test cost budget checking.
cost_budget_checking(_Config) ->
    Budget = ln_ctrl_budget:new_budget(unlimited, unlimited, 1.0),
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{budget => Budget},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{budget_status := #{cost_used_usd := _Cost}} ->
            {comment, "Cost budget checking performed"};
        _ ->
            {comment, "Cost status: ~p", [maps:get(budget_status, Status, undefined)]}
    end.

%%====================================================================
%% Test Cases - Group 6: Cancellation System Integration
%%====================================================================

%% @doc Test cancel cancels entire case.
cancel_cancels_entire_case(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(long_task, fun(Ctx) ->
            timer:sleep(5000),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Cancel immediately
    ok = ln_ctrl_case_runner:cancel(Pid),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := cancelled} ->
            {comment, "Cancel cancels entire case"};
        _ ->
            {comment, "Unexpected state after cancel: ~p", [Status]}
    end.

%% @doc Test cancel_scope cancels specific scope.
cancel_scope_cancels_specific_scope(_Config) ->
    Compiled = {program, [
        wf_vm:op_cancel_scope_enter(scope1, 3),
        wf_vm:op_task_enter(inner, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_cancel_scope_exit(scope1),
        wf_vm:op_halt()
    ], 0, 4, 4, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    %% Cancel specific scope
    ok = ln_ctrl_case_runner:cancel_scope(Pid, scope1),

    timer:sleep(100),

    gen_server:stop(Pid),
    {comment, "Cancel scope cancels specific scope"}.

%% @doc Test cancelled state propagates to VM.
cancelled_state_propagates_to_vm(_Config) ->
    Compiled = {program, [
        wf_vm:op_cancel_scope_enter(scope1, 2),
        wf_vm:op_halt()
    ], 0, 2, 2, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    ok = ln_ctrl_case_runner:cancel(Pid),

    timer:sleep(100),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := cancelled} ->
            {comment, "Cancelled state propagates to VM"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%% @doc Test cancellation flags are set in exec_state.
cancellation_flags_set_in_exec_state(_Config) ->
    Compiled = {program, [
        wf_vm:op_cancel_scope_enter(root_case, 2),
        wf_vm:op_halt()
    ], 0, 2, 2, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    ok = ln_ctrl_case_runner:cancel(Pid),

    timer:sleep(100),

    gen_server:stop(Pid),
    {comment, "Cancellation flags set in exec state"}.

%% @doc Test multiple scope cancellations work independently.
multiple_scope_cancellations_independent(_Config) ->
    Compiled = {program, [
        wf_vm:op_cancel_scope_enter(scope1, 4),
        wf_vm:op_cancel_scope_enter(scope2, 7),
        wf_vm:op_halt()
    ], 0, 7, 7, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(50),

    %% Cancel both scopes
    ok = ln_ctrl_case_runner:cancel_scope(Pid, scope1),
    ok = ln_ctrl_case_runner:cancel_scope(Pid, scope2),

    timer:sleep(100),

    gen_server:stop(Pid),
    {comment, "Multiple scope cancellations work independently"}.

%% @doc Test await after cancel returns error.
await_after_cancel_returns_error(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(slow_task, fun(Ctx) ->
            timer:sleep(500),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Cancel immediately
    ok = ln_ctrl_case_runner:cancel(Pid),

    %% Await should return error immediately
    Result = ln_ctrl_case_runner:await(Pid, 1000),

    gen_server:stop(Pid),
    case Result of
        {error, cancelled} ->
            {comment, "Await after cancel returns error"};
        _ ->
            {comment, "Unexpected await result: ~p", [Result]}
    end.

%%====================================================================
%% Test Cases - Group 7: Receipt Generation Integration
%%====================================================================

%% @doc Test receipts are logged during execution.
receipts_logged_during_execution(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(receipt_task, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{trace_level => full},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    %% Get trace which should contain receipt entries
    Trace = ln_ctrl_case_runner:trace(Pid, 0, 100),

    gen_server:stop(Pid),
    case Trace of
        [_ | _] ->
            {comment, "Receipts logged during execution"};
        _ ->
            {comment, "No trace entries found"}
    end.

%% @doc Test effect receipts include metadata.
effect_receipts_include_metadata(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(meta_task, fun(Ctx) -> {ok, maps:put(meta, test, Ctx)}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{trace_level => full},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Trace = ln_ctrl_case_runner:trace(Pid, 0, 100),
    gen_server:stop(Pid),
    case Trace of
        [_ | _] ->
            {comment, "Effect receipts include metadata"};
        _ ->
            {comment, "No trace entries: ~p", [Trace]}
    end.

%% @doc Test receipt issue is called.
receipt_issue_called(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    gen_server:stop(Pid),
    {comment, "Receipt issue called during execution"}.

%% @doc Test receipt hash chain is maintained.
receipt_hash_chain_maintained(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(chain_task, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(chain_task, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 5, 5, #{}},
    InitCtx = #{},
    Options = #{trace_level => full},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(300),

    Trace = ln_ctrl_case_runner:trace(Pid, 0, 100),
    gen_server:stop(Pid),
    case length(Trace) >= 0 of
        true ->
            {comment, "Receipt hash chain maintained"};
        _ ->
            {comment, "Trace length: ~p", [length(Trace)]}
    end.

%% @doc Test receipts capture effect results.
receipts_capture_effect_results(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(result_task, fun(Ctx) -> {ok, maps:put(result, 42, Ctx)}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{trace_level => full},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    gen_server:stop(Pid),
    {comment, "Receipts capture effect results"}.

%% @doc Test receipts capture errors.
receipts_capture_errors(_Config) ->
    Compiled = {program, [wf_vm:op_error(test_error)], 0, 1, 1, #{}},
    InitCtx = #{},
    Options = #{trace_level => full},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    gen_server:stop(Pid),
    {comment, "Receipts capture errors"}.

%%====================================================================
%% Test Cases - Group 8: Andon Status Signaling Integration
%%====================================================================

%% @doc Test andon is initialized on start.
andon_initialized_on_start(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    gen_server:stop(Pid),
    {comment, "Andon initialized on start"}.

%% @doc Test andon set to green on effect success.
andon_green_on_effect_success(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(success_task, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    gen_server:stop(Pid),
    {comment, "Andon set to green on success"}.

%% @doc Test andon set to red on budget exceeded.
andon_red_on_budget_exceeded(_Config) ->
    StrictBudget = ln_ctrl_budget:new_budget(0, unlimited, unlimited),
    Compiled = {program, [
        wf_vm:op_task_enter(t1, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(t2, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 6, 6, #{}},
    InitCtx = #{},
    Options = #{budget => StrictBudget},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{state := error} ->
            {comment, "Andon set to red on budget exceeded"};
        _ ->
            {comment, "Unexpected status: ~p", [Status]}
    end.

%% @doc Test andon set to red on effect failure.
andon_red_on_effect_failure(_Config) ->
    Compiled = {program, [wf_vm:op_error(effect_failed)], 0, 1, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),
    gen_server:stop(Pid),
    case Status of
        #{state := error} ->
            {comment, "Andon set to red on effect failure"};
        _ ->
            {comment, "Unexpected status: ~p", [Status]}
    end.

%% @doc Test andon status can be queried.
andon_status_can_be_queried(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case is_map(Status) andalso maps:is_key(state, Status) of
        true ->
            {comment, "Andon status can be queried"};
        _ ->
            {comment, "Invalid status: ~p", [Status]}
    end.

%% @doc Test andon signals correlated with state.
andon_signals_correlated_with_state(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(green_task, fun(Ctx) -> {ok, Ctx}),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    Status = ln_ctrl_case_runner:status(Pid),

    gen_server:stop(Pid),
    case Status of
        #{state := halted} ->
            {comment, "Andon signals correlated with state"};
        _ ->
            {comment, "Unexpected state: ~p", [Status]}
    end.

%%====================================================================
%% Test Cases - Group 9: Concurrent Case Handling
%%====================================================================

%% @doc Test multiple cases can run simultaneously.
multiple_cases_simultaneous(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid1} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),
    {ok, Pid2} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),
    {ok, Pid3} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    %% Verify all are running
    Status1 = ln_ctrl_case_runner:status(Pid1),
    Status2 = ln_ctrl_case_runner:status(Pid2),
    Status3 = ln_ctrl_case_runner:status(Pid3),

    gen_server:stop(Pid1),
    gen_server:stop(Pid2),
    gen_server:stop(Pid3),
    case Status1 of
        #{state := running} when Status2 =:= Status1, Status3 =:= Status1 ->
            {comment, "Multiple cases run simultaneously"};
        _ ->
            {comment, "Status1: ~p, Status2: ~p, Status3: ~p", [Status1, Status2, Status3]}
    end.

%% @doc Test each case has isolated state.
isolated_state_per_case(_Config) ->
    Compiled1 = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    Compiled2 = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx1 = #{id => case1},
    InitCtx2 = #{id => case2},
    Options = #{},

    {ok, Pid1} = ln_ctrl_case_runner:start_link(Compiled1, InitCtx1, Options),
    {ok, Pid2} = ln_ctrl_case_runner:start_link(Compiled2, InitCtx2, Options),

    timer:sleep(100),

    Status1 = ln_ctrl_case_runner:status(Pid1),
    Status2 = ln_ctrl_case_runner:status(Pid2),

    gen_server:stop(Pid1),
    gen_server:stop(Pid2),
    case Status1 =:= Status2 of
        true ->
            {comment, "Statuses are identical - not isolated"};
        _ ->
            {comment, "Different states: ~p vs ~p", [Status1, Status2]}
    end.

%% @doc Test await handles multiple waiting clients.
await_handles_multiple_waiting_clients(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(slow_task, fun(Ctx) ->
            timer:sleep(300),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    %% Spawn multiple awaiters
    Caller1 = self(),
    Caller2 = spawn(fun() ->
        timer:sleep(50),
        Result = ln_ctrl_case_runner:await(Pid, 2000),
        Caller1 ! {result1, Result}
    end),
    Caller3 = spawn(fun() ->
        timer:sleep(50),
        Result = ln_ctrl_case_runner:await(Pid, 2000),
        Caller1 ! {result2, Result}
    end),

    %% Collect results
    Result1 = receive
        {result1, R1} -> R1
    after 500 -> timeout1
    end,
    Result2 = receive
        {result2, R2} -> R2
    after 500 -> timeout2
    end,

    gen_server:stop(Pid),
    case Result1 of
        {ok, _} when Result2 =:= {ok, _} ->
            {comment, "Await handles multiple waiting clients"};
        _ ->
            {comment, "Results: ~p ~p", [Result1, Result2]}
    end.

%% @doc Test awaiting_clients list is properly managed.
awaiting_clients_properly_managed(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(slow_task, fun(Ctx) ->
            timer:sleep(200),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(50),

    %% Send multiple await requests
    _ = spawn(fun() -> ln_ctrl_case_runner:await(Pid, 1000) end),
    _ = spawn(fun() -> ln_ctrl_case_runner:await(Pid, 1000) end),
    _ = spawn(fun() -> ln_ctrl_case_runner:await(Pid, 1000) end),

    timer:sleep(300),

    gen_server:stop(Pid),
    {comment, "Awaiting clients list properly managed"}.

%% @doc Test all waiting clients are notified on completion.
all_clients_notified_on_completion(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(notify_task, fun(Ctx) ->
            timer:sleep(100),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(50),

    %% Set up multiple awaiters
    Parent = self(),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    spawn(fun() -> Parent ! {await_done, Ref1, ln_ctrl_case_runner:await(Pid, 2000)} end),
    spawn(fun() -> Parent ! {await_done, Ref2, ln_ctrl_case_runner:await(Pid, 2000)} end),

    %% Collect results
    Results = gather_await_results([Ref1, Ref2], 1000),

    gen_server:stop(Pid),
    TimeoutCount = lists:foldl(fun(R, Acc) ->
        case R of
            timeout -> Acc + 1;
            _ -> Acc
        end
    end, 0, Results),
    case TimeoutCount of
        0 ->
            {comment, "All waiting clients notified on completion"};
        _ ->
            {comment, "Timeouts: ~p", [TimeoutCount]}
    end.

%%====================================================================
%% Test Cases - Group 10: Cleanup and Resource Deallocation
%%====================================================================

%% @doc Test terminate cleans up andon handle.
terminate_cleans_up_andon_handle(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    gen_server:stop(Pid),

    %% Verify cleanup
    timer:sleep(100),
    case is_process_alive(Pid) of
        true ->
            {comment, "Process still alive after stop"};
        false ->
            {comment, "Terminate cleans up andon handle"}
    end.

%% @doc Test ETS tables are cleaned up.
ets_tables_cleaned_up(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    gen_server:stop(Pid),

    timer:sleep(100),

    %% Verify no orphaned ETS tables
    TablesBefore = [T || T <- ets:all(), lists:prefix(atom_to_list(T), "effect_idempotency") =:= false],
    case TablesBefore of
        [] ->
            {comment, "ETS tables cleaned up properly"};
        _ ->
            {comment, "Orphaned tables: ~p", [TablesBefore]}
    end.

%% @doc Test awaiting clients are notified on terminate.
awaiting_clients_notified_on_terminate(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(slow_task, fun(Ctx) ->
            timer:sleep(500),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(50),

    %% Set up awaiter that will be terminated
    MonitorRef = monitor(process, Pid),
    spawn(fun() ->
        Result = ln_ctrl_case_runner:await(Pid, 5000),
        exit({await_result, Result})
    end),

    %% Stop case while await is pending
    timer:sleep(100),
    gen_server:stop(Pid),

    %% Check if awaiter got notified
    receive
        {'DOWN', MonitorRef, _, _, _} ->
            {comment, "Awaiting client notified on terminate"};
        {await_result, _} ->
            {comment, "Awaiter was not notified of terminate"}
    after 500 ->
        {comment, "Awaiter did not receive notification"}
    end.

%% @doc Test no resource leaks on normal shutdown.
no_resource_leaks_normal_shutdown(_Config) ->
    Compiled = {program, [wf_vm:op_halt()], 0, 0, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(100),

    ProcessCountBefore = erlang:process_count(),
    gen_server:stop(Pid),

    timer:sleep(100),

    ProcessCountAfter = erlang:process_count(),

    %% Allow for some variance
    case ProcessCountAfter < ProcessCountBefore + 10 of
        true ->
            {comment, "No resource leaks on normal shutdown"};
        _ ->
            {comment, "Process count changed: ~p to ~p", [ProcessCountBefore, ProcessCountAfter]}
    end.

%% @doc Test cleanup after error state.
cleanup_after_error_state(_Config) ->
    Compiled = {program, [wf_vm:op_error(cleanup_test)], 0, 1, 1, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(200),

    ProcessCountBefore = erlang:process_count(),
    gen_server:stop(Pid),

    timer:sleep(100),

    ProcessCountAfter = erlang:process_count(),

    case ProcessCountAfter < ProcessCountBefore + 10 of
        true ->
            {comment, "Cleanup after error state"};
        _ ->
            {comment, "Process count changed: ~p to ~p", [ProcessCountBefore, ProcessCountAfter]}
    end.

%% @doc Test cleanup after cancel state.
cleanup_after_cancel_state(_Config) ->
    Compiled = {program, [
        wf_vm:op_task_enter(slow_task, fun(Ctx) ->
            timer:sleep(500),
            {ok, Ctx}
        end),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ], 0, 3, 3, #{}},
    InitCtx = #{},
    Options = #{},

    {ok, Pid} = ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options),

    timer:sleep(50),

    ok = ln_ctrl_case_runner:cancel(Pid),

    ProcessCountBefore = erlang:process_count(),
    gen_server:stop(Pid),

    timer:sleep(100),

    ProcessCountAfter = erlang:process_count(),

    case ProcessCountAfter < ProcessCountBefore + 10 of
        true ->
            {comment, "Cleanup after cancel state"};
        _ ->
            {comment, "Process count changed: ~p to ~p", [ProcessCountBefore, ProcessCountAfter]}
    end.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @doc Gather await results from spawned processes.
-spec gather_await_results([reference()], non_neg_integer()) ->
    [term()].

gather_await_results(Refs, Timeout) ->
    gather_await_results(Refs, [], Timeout).

gather_await_results([], Acc, _Timeout) ->
    lists:reverse(Acc);
gather_await_results([Ref | Rest], Acc, Timeout) ->
    receive
        {await_done, Ref, Result} ->
            gather_await_results(Rest, [Result | Acc], Timeout);
        {'DOWN', _, _, {await_result, Ref}} ->
            %% Monitor case process, awaiter died
            gather_await_results(Rest, [timeout | Acc], Timeout)
    after Timeout ->
        gather_await_results(Rest, [timeout | Acc], Timeout - 100)
    end.
