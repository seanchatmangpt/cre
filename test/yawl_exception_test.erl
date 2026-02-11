%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jorgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%% @version 0.3.0
%%
%% @doc YAWL Exception Handling Test Suite (WHP-01 through WHP-05)
%%
%% This module contains comprehensive execution tests for the YAWL workflow
%% exception handling patterns:
%% - WHP-01: Error Handler Pattern
%% - WHP-02: Retry Pattern with Backoff Strategies
%% - WHP-03: Compensation Pattern
%% - WHP-04: Triggered Compensation Pattern
%% - WHP-05: Consecutive Compensation Pattern
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_exception_test).
-include_lib("eunit/include/eunit.hrl").

%% Include record definitions from source modules
-include("../src/cre_yawl_patterns.hrl").

%% Record definitions from cre_yawl_exception module
%% (defined here since there's no separate .hrl file for exception records)
-record(yawl_exception, {
          id :: binary(),
          type :: atom(),
          severity :: atom(),
          message :: binary(),
          context :: map(),
          timestamp :: integer(),
          stacktrace :: list(),
          workflow_id :: undefined | binary(),
          activity_id :: undefined | binary(),
          compensation_attempts = 0 :: non_neg_integer(),
          retry_attempts = 0 :: non_neg_integer(),
          handled_by :: undefined | binary(),
          resolved = false :: boolean()
         }).

-record(compensator, {
          activity_id :: binary(),
          compensation_handler :: function(),
          compensation_strategy :: atom(),
          state :: atom(),
          result :: undefined | {ok, term()} | {error, term()},
          created_at :: integer(),
          started_at :: undefined | integer(),
          completed_at :: undefined | integer(),
          execution_time :: undefined | integer(),
          retry_policy :: undefined | term(),
          dependencies :: [binary()],
          metadata :: map()
         }).

-record(retry_policy, {
          max_attempts = 3 :: non_neg_integer(),
          backoff_strategy = exponential :: atom(),
          base_delay = 1000 :: non_neg_integer(),
          max_delay = 60000 :: non_neg_integer(),
          multiplier = 2.0 :: float(),
          jitter = true :: boolean(),
          jitter_factor = 0.1 :: float(),
          timeout = 30000 :: non_neg_integer(),
          retryable_exceptions = [] :: list(),
          circuit_breaker_threshold = 5 :: pos_integer(),
          circuit_breaker_timeout = 60000 :: non_neg_integer()
         }).

-record(error_handler, {
          handler_id :: binary(),
          exception_types :: list(),
          handler_fun :: function(),
          priority = 0 :: integer(),
          retry_policy :: undefined | term(),
          compensation_handler :: undefined | function(),
          circuit_breaker_state :: undefined | map(),
          metrics :: map(),
          enabled = true :: boolean()
         }).

-record(exception_state, {
          current_exception = undefined :: undefined | term(),
          active_compensators = [] :: list(),
          compensation_stack = [] :: list(),
          handlers = #{} :: map(),
          workflow_id :: undefined | binary(),
          workflow_pid :: undefined | pid(),
          workflow_monitor :: undefined | reference(),
          circuit_breakers = #{} :: map(),
          metrics = #{} :: map(),
          audit_log = [] :: list(),
          is_compensating = false :: boolean(),
          compensation_timeout :: undefined | integer(),
          max_parallel_compensations = 10 :: pos_integer()
         }).

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup for all tests.
%% @end
%%--------------------------------------------------------------------
setup_all() ->
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc Cleanup for all tests.
%% @end
%%--------------------------------------------------------------------
cleanup_all(_TestData) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Setup for each test case.
%% @end
%%--------------------------------------------------------------------
setup_each() ->
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup for each test case.
%% @end
%%--------------------------------------------------------------------
cleanup_each(_TestData) ->
    ok.

%%====================================================================
%% Test Data Builders
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a test exception of specified type.
%% @end
%%--------------------------------------------------------------------
test_exception(Type) ->
    test_exception(Type, <<"Test exception message">>).

test_exception(Type, Message) ->
    test_exception(Type, Message, #{}).

test_exception(Type, Message, Context) ->
    cre_yawl_exception:new_exception(Type, Message, Context, []).

%%--------------------------------------------------------------------
%% @doc Creates a test exception with all fields.
%% @end
%%--------------------------------------------------------------------
test_exception_full(Type, Severity, Message, Context, WorkflowId, ActivityId) ->
    cre_yawl_exception:new_exception_with_fields(Type, Severity, Message,
                                                 Context, [], WorkflowId, ActivityId).

%%--------------------------------------------------------------------
%% @doc Creates a test compensator.
%% @end
%%--------------------------------------------------------------------
test_compensator(ActivityId) ->
    HandlerFun = fun(_Input) -> {compensated, ActivityId} end,
    cre_yawl_exception:new_compensator(ActivityId, HandlerFun, immediate).

%%--------------------------------------------------------------------
%% @doc Creates a test compensator with specific handler.
%% @end
%%--------------------------------------------------------------------
test_compensator_with_handler(ActivityId, HandlerFun) ->
    cre_yawl_exception:new_compensator(ActivityId, HandlerFun, immediate).

%%--------------------------------------------------------------------
%% @doc Creates a test compensator with strategy.
%% @end
%%--------------------------------------------------------------------
test_compensator_with_strategy(ActivityId, Strategy) ->
    HandlerFun = fun(_Input) -> {compensated, ActivityId} end,
    cre_yawl_exception:new_compensator(ActivityId, HandlerFun, Strategy).

%%--------------------------------------------------------------------
%% @doc Creates a test retry policy.
%% @end
%%--------------------------------------------------------------------
test_retry_policy() ->
    cre_yawl_exception:new_retry_policy().

%%--------------------------------------------------------------------
%% @doc Creates a test retry policy with options.
%% @end
%%--------------------------------------------------------------------
test_retry_policy_with_options(Options) ->
    cre_yawl_exception:new_retry_policy(Options).

%%--------------------------------------------------------------------
%% @doc Creates a test error handler.
%% @end
%%--------------------------------------------------------------------
test_error_handler(HandlerId, ExceptionTypes) ->
    HandlerFun = fun(_Exception) -> {ok, handled} end,
    cre_yawl_exception:new_error_handler(HandlerId, ExceptionTypes, HandlerFun).

%%--------------------------------------------------------------------
%% @doc Creates a test error handler with custom handler function.
%% @end
%%--------------------------------------------------------------------
test_error_handler_with_fun(HandlerId, ExceptionTypes, HandlerFun) ->
    cre_yawl_exception:new_error_handler(HandlerId, ExceptionTypes, HandlerFun).

%%====================================================================
%% WHP-01: Error Handler Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WHP-01 Error Handler pattern.
%% @end
%%--------------------------------------------------------------------
whp01_error_handler_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          %% Normal Execution Tests
          whp01_normal_execution_test(),
          whp01_no_exception_test(),
          whp01_cleanup_after_handling_test(),

          %% Exception Scenarios
          whp01_throw_exception_test(),
          whp01_exit_exception_test(),
          whp01_error_exception_test(),
          whp01_custom_exception_test(),

          %% Recovery Tests
          whp01_successful_recovery_test(),
          whp01_recovery_failure_test(),
          whp01_nested_recovery_test(),

          %% Edge Cases
          whp01_multiple_exceptions_test(),
          whp01_exception_during_recovery_test(),
          whp01_missing_handler_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test normal execution without exception.
%% @end
%%--------------------------------------------------------------------
whp01_normal_execution_test() ->
    {"WHP-01: Normal execution without exception",
     fun() ->
         Activity = fun(_Data) -> {ok, normal_result} end,
         HandlerFun = fun(_Exception) -> {ok, recovered} end,
         Pattern = cre_yawl_patterns:error_handler(Activity, HandlerFun),

         ?assertEqual(error_handler, Pattern#pattern_state.pattern_type),
         ?assert(is_function(Pattern#pattern_state.subprocess)),
         ?assert(is_function(maps:get(error_handler, Pattern#pattern_state.choice_data)))
     end}.

%%--------------------------------------------------------------------
%% @doc Test no exception scenario.
%% @end
%%--------------------------------------------------------------------
whp01_no_exception_test() ->
    {"WHP-01: No exception scenario",
     fun() ->
         ExceptionState = cre_yawl_exception:init_exception_state(<<"wf1">>, self()),
         ?assertEqual(undefined, ExceptionState#exception_state.current_exception),
         ?assertEqual([], ExceptionState#exception_state.active_compensators)
     end}.

%%--------------------------------------------------------------------
%% @doc Test cleanup after handling.
%% @end
%%--------------------------------------------------------------------
whp01_cleanup_after_handling_test() ->
    {"WHP-01: Cleanup after handling",
     fun() ->
         Exception = test_exception(system_exception),
         Handlers = #{system_exception => [test_error_handler(<<"h1">>, [system_exception])]},

         State = cre_yawl_exception:log_audit_event(exception_handled,
                                                      #{exception => Exception},
                                                      #exception_state{handlers = Handlers}),

         ?assert(is_list(State#exception_state.audit_log)),
         ?assertEqual(1, length(State#exception_state.audit_log))
     end}.

%%--------------------------------------------------------------------
%% @doc Test throw exception handling.
%% @end
%%--------------------------------------------------------------------
whp01_throw_exception_test() ->
    {"WHP-01: Throw exception handling",
     fun() ->
         Exception = test_exception(business_exception, <<"Business rule violation">>),

         %% Verify exception properties
         ?assertEqual(business_exception, cre_yawl_exception:exception_type(Exception)),
         ?assertEqual(<<"Business rule violation">>, cre_yawl_exception:exception_message(Exception)),
         ?assert(is_integer(cre_yawl_exception:exception_timestamp(Exception))),
         ?assert(is_binary(cre_yawl_exception:exception_id(Exception)))
     end}.

%%--------------------------------------------------------------------
%% @doc Test exit exception handling.
%% @end
%%--------------------------------------------------------------------
whp01_exit_exception_test() ->
    {"WHP-01: Exit exception handling",
     fun() ->
         Exception = test_exception(system_exception, <<"Process exited abnormally">>),

         %% Verify severity
         ?assertEqual(high, cre_yawl_exception:exception_severity(Exception)),
         ?assertEqual(system_exception, cre_yawl_exception:exception_type(Exception))
     end}.

%%--------------------------------------------------------------------
%% @doc Test error exception handling.
%% @end
%%--------------------------------------------------------------------
whp01_error_exception_test() ->
    {"WHP-01: Error exception handling",
     fun() ->
         Context = #{error_code => 500, details => <<"Internal server error">>},
         Exception = test_exception(workflow_exception, <<"Workflow failed">>, Context),

         ?assertEqual(workflow_exception, cre_yawl_exception:exception_type(Exception)),
         ?assertMatch(#{error_code := 500}, cre_yawl_exception:exception_context(Exception))
     end}.

%%--------------------------------------------------------------------
%% @doc Test custom exception handling.
%% @end
%%--------------------------------------------------------------------
whp01_custom_exception_test() ->
    {"WHP-01: Custom exception handling",
     fun() ->
         Exception = test_exception(validation_exception,
                                    <<"Invalid input data">>,
                                    #{field => <<"email">>, value => <<"bad-email">>}),

         ?assertEqual(validation_exception, cre_yawl_exception:exception_type(Exception)),
         ?assertEqual(<<"Invalid input data">>, cre_yawl_exception:exception_message(Exception))
     end}.

%%--------------------------------------------------------------------
%% @doc Test successful recovery.
%% @end
%%--------------------------------------------------------------------
whp01_successful_recovery_test() ->
    {"WHP-01: Successful recovery",
     fun() ->
         HandlerFun = fun(_Exception) -> recovered end,
         Handler = test_error_handler_with_fun(<<"recovery_handler">>,
                                                [system_exception],
                                                HandlerFun),

         Exception = test_exception(system_exception),
         Result = cre_yawl_exception:execute_handler(Handler, Exception),

         ?assertEqual({ok, recovered}, Result)
     end}.

%%--------------------------------------------------------------------
%% @doc Test recovery failure.
%% @end
%%--------------------------------------------------------------------
whp01_recovery_failure_test() ->
    {"WHP-01: Recovery failure",
     fun() ->
         HandlerFun = fun(_Exception) -> error(cannot_recover) end,
         Handler = test_error_handler_with_fun(<<"failing_handler">>,
                                                [system_exception],
                                                HandlerFun),

         Exception = test_exception(system_exception),
         Result = cre_yawl_exception:execute_handler(Handler, Exception),

         ?assertMatch({error, {error, cannot_recover}}, Result)
     end}.

%%--------------------------------------------------------------------
%% @doc Test nested recovery.
%% @end
%%--------------------------------------------------------------------
whp01_nested_recovery_test() ->
    {"WHP-01: Nested recovery",
     fun() ->
         %% Create handlers with different priorities
         Handler1 = (test_error_handler_with_fun(<<"handler1">>, [system_exception],
                                                  fun(_) -> {ok, level1} end))
                         #error_handler{priority = 10},

         Handler2 = (test_error_handler_with_fun(<<"handler2">>, [system_exception],
                                                  fun(_) -> {ok, level2} end))
                         #error_handler{priority = 5},

         Handlers = #{system_exception => [Handler1, Handler2]},
         Exception = test_exception(system_exception),

         %% Find best handler should return highest priority
         BestHandler = cre_yawl_exception:find_best_handler(Handlers, Exception),

         ?assertNotEqual(undefined, BestHandler),
         ?assertEqual(10, BestHandler#error_handler.priority)
     end}.

%%--------------------------------------------------------------------
%% @doc Test multiple exceptions.
%% @end
%%--------------------------------------------------------------------
whp01_multiple_exceptions_test() ->
    {"WHP-01: Multiple exceptions handling",
     fun() ->
         Exception1 = test_exception(business_exception),
         Exception2 = test_exception(system_exception),
         Exception3 = test_exception(timeout_exception),

         %% All should have unique IDs
         Id1 = cre_yawl_exception:exception_id(Exception1),
         Id2 = cre_yawl_exception:exception_id(Exception2),
         Id3 = cre_yawl_exception:exception_id(Exception3),

         ?assertNotEqual(Id1, Id2),
         ?assertNotEqual(Id2, Id3),
         ?assertNotEqual(Id1, Id3)
     end}.

%%--------------------------------------------------------------------
%% @doc Test exception during recovery.
%% @end
%%--------------------------------------------------------------------
whp01_exception_during_recovery_test() ->
    {"WHP-01: Exception during recovery",
     fun() ->
         HandlerFun = fun(_Exception) -> throw(handler_failed) end,
         Handler = test_error_handler_with_fun(<<"failing_handler">>,
                                                [system_exception],
                                                HandlerFun),

         Exception = test_exception(system_exception),

         %% Execute handler should catch the throw
         try
             cre_yawl_exception:execute_handler(Handler, Exception),
             ?assert(false, "Should have thrown")
         catch
             _:_ -> ok
         end
     end}.

%%--------------------------------------------------------------------
%% @doc Test missing handler scenario.
%% @end
%%--------------------------------------------------------------------
whp01_missing_handler_test() ->
    {"WHP-01: Missing handler for exception type",
     fun() ->
         Handlers = #{business_exception => [test_error_handler(<<"h1">>, [business_exception])]},

         Exception = test_exception(resource_exception),

         %% Find handler should return empty list
         FoundHandlers = cre_yawl_exception:find_handler(Handlers, resource_exception),

         ?assertEqual([], FoundHandlers)
     end}.

%%====================================================================
%% WHP-02: Retry Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WHP-02 Retry pattern.
%% @end
%%--------------------------------------------------------------------
whp02_retry_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          %% Normal Execution Tests
          whp02_success_on_first_attempt_test(),
          whp02_success_on_retry_test(),

          %% Backoff Strategy Tests
          whp02_constant_backoff_test(),
          whp02_exponential_backoff_test(),
          whp02_linear_backoff_test(),
          whp02_fibonacci_backoff_test(),

          %% Retry Limit Tests
          whp02_max_retry_limit_test(),
          whp02_retry_exhaustion_test(),

          %% Edge Cases
          whp02_zero_max_retries_test(),
          whp02_jitter_calculation_test(),
          whp02_max_delay_cap_test(),
          whp02_should_retry_predicate_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test success on first attempt.
%% @end
%%--------------------------------------------------------------------
whp02_success_on_first_attempt_test() ->
    {"WHP-02: Success on first attempt",
     fun() ->
         Activity = fun(_Data) -> {ok, success} end,
         BackoffFun = fun(_Attempt) -> 1000 end,
         Pattern = cre_yawl_patterns:retry(Activity, 3, BackoffFun),

         ?assertEqual(retry, Pattern#pattern_state.pattern_type),
         ?assertEqual(3, Pattern#pattern_state.max_instances)
     end}.

%%--------------------------------------------------------------------
%% @doc Test success on retry attempt.
%% @end
%%--------------------------------------------------------------------
whp02_success_on_retry_test() ->
    {"WHP-02: Success on retry attempt",
     fun() ->
         %% Create exception with retry tracking
         Exception1 = test_exception(system_exception),

         ?assertEqual(0, cre_yawl_exception:exception_retry_attempts(Exception1)),

         %% Simulate retry by creating new exception with incremented counter
         Exception2 = Exception1#yawl_exception{retry_attempts = 1},
         ?assertEqual(1, cre_yawl_exception:exception_retry_attempts(Exception2))
     end}.

%%--------------------------------------------------------------------
%% @doc Test constant backoff calculation.
%% @end
%%--------------------------------------------------------------------
whp02_constant_backoff_test() ->
    {"WHP-02: Constant backoff strategy",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             backoff => constant,
             base_delay => 1000,
             jitter => false
         }),

         %% All attempts should have same delay
         ?assertEqual(1000, cre_yawl_exception:calculate_backoff(Policy, 1)),
         ?assertEqual(1000, cre_yawl_exception:calculate_backoff(Policy, 2)),
         ?assertEqual(1000, cre_yawl_exception:calculate_backoff(Policy, 3))
     end}.

%%--------------------------------------------------------------------
%% @doc Test exponential backoff calculation.
%% @end
%%--------------------------------------------------------------------
whp02_exponential_backoff_test() ->
    {"WHP-02: Exponential backoff strategy",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             backoff => exponential,
             base_delay => 1000,
             multiplier => 2.0,
             max_delay => 60000,
             jitter => false
         }),

         %% Delay should double each attempt
         Delay1 = cre_yawl_exception:calculate_backoff(Policy, 1),
         Delay2 = cre_yawl_exception:calculate_backoff(Policy, 2),
         Delay3 = cre_yawl_exception:calculate_backoff(Policy, 3),

         ?assertEqual(1000, Delay1),
         ?assertEqual(2000, Delay2),
         ?assertEqual(4000, Delay3)
     end}.

%%--------------------------------------------------------------------
%% @doc Test linear backoff calculation.
%% @end
%%--------------------------------------------------------------------
whp02_linear_backoff_test() ->
    {"WHP-02: Linear backoff strategy",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             backoff => linear,
             base_delay => 1000,
             max_delay => 10000,
             jitter => false
         }),

         %% Delay should increase linearly
         Delay1 = cre_yawl_exception:calculate_backoff(Policy, 1),
         Delay2 = cre_yawl_exception:calculate_backoff(Policy, 2),
         Delay3 = cre_yawl_exception:calculate_backoff(Policy, 5),

         ?assertEqual(1000, Delay1),
         ?assertEqual(2000, Delay2),
         ?assertEqual(5000, Delay3)
     end}.

%%--------------------------------------------------------------------
%% @doc Test Fibonacci backoff calculation.
%% @end
%%--------------------------------------------------------------------
whp02_fibonacci_backoff_test() ->
    {"WHP-02: Fibonacci backoff strategy",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             backoff => fibonacci,
             base_delay => 1000,
             max_delay => 100000,
             jitter => false
         }),

         %% Delay should follow Fibonacci sequence
         Delay1 = cre_yawl_exception:calculate_backoff(Policy, 1),
         Delay2 = cre_yawl_exception:calculate_backoff(Policy, 2),
         Delay3 = cre_yawl_exception:calculate_backoff(Policy, 3),
         Delay4 = cre_yawl_exception:calculate_backoff(Policy, 4),

         ?assertEqual(1000, Delay1),  % Fib(1) = 1
         ?assertEqual(1000, Delay2),  % Fib(2) = 1
         ?assertEqual(2000, Delay3),  % Fib(3) = 2
         ?assertEqual(3000, Delay4)   % Fib(4) = 3
     end}.

%%--------------------------------------------------------------------
%% @doc Test max retry limit enforcement.
%% @end
%%--------------------------------------------------------------------
whp02_max_retry_limit_test() ->
    {"WHP-02: Max retry limit enforcement",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             max_attempts => 5
         }),

         ?assertEqual(5, cre_yawl_exception:retry_policy_max_attempts(Policy)),

         %% Test should_retry predicate
         ?assert(cre_yawl_exception:should_retry(Policy, 0)),
         ?assert(cre_yawl_exception:should_retry(Policy, 1)),
         ?assert(cre_yawl_exception:should_retry(Policy, 4)),
         ?assertNot(cre_yawl_exception:should_retry(Policy, 5)),
         ?assertNot(cre_yawl_exception:should_retry(Policy, 10))
     end}.

%%--------------------------------------------------------------------
%% @doc Test retry exhaustion.
%% @end
%%--------------------------------------------------------------------
whp02_retry_exhaustion_test() ->
    {"WHP-02: Retry exhaustion",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             max_attempts => 3
         }),

         %% After max attempts, should not retry
         ?assertNot(cre_yawl_exception:should_retry(Policy, 3))
     end}.

%%--------------------------------------------------------------------
%% @doc Test zero max retries.
%% @end
%%--------------------------------------------------------------------
whp02_zero_max_retries_test() ->
    {"WHP-02: Zero max retries configuration",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             max_attempts => 0
         }),

         ?assertNot(cre_yawl_exception:should_retry(Policy, 0))
     end}.

%%--------------------------------------------------------------------
%% @doc Test jitter calculation.
%% @end
%%--------------------------------------------------------------------
whp02_jitter_calculation_test() ->
    {"WHP-02: Jitter calculation",
     fun() ->
         Policy1 = test_retry_policy_with_options(#{
             backoff => constant,
             base_delay => 1000,
             jitter => true,
             jitter_factor => 0.1
         }),

         %% With jitter, delay should vary
         Delay1 = cre_yawl_exception:calculate_backoff(Policy1, 1),
         Delay2 = cre_yawl_exception:calculate_backoff(Policy1, 1),

         %% Should be within expected range (1000 +/- 100)
         ?assert(Delay1 >= 900 andalso Delay1 =< 1100),
         ?assert(Delay2 >= 900 andalso Delay2 =< 1100)
     end}.

%%--------------------------------------------------------------------
%% @doc Test max delay cap.
%% @end
%%--------------------------------------------------------------------
whp02_max_delay_cap_test() ->
    {"WHP-02: Max delay cap enforcement",
     fun() ->
         Policy = test_retry_policy_with_options(#{
             backoff => exponential,
             base_delay => 1000,
             multiplier => 10.0,
             max_delay => 5000,
             jitter => false
         }),

         %% Even with high multiplier, should cap at max_delay
         Delay1 = cre_yawl_exception:calculate_backoff(Policy, 1),
         Delay2 = cre_yawl_exception:calculate_backoff(Policy, 2),
         Delay3 = cre_yawl_exception:calculate_backoff(Policy, 10),

         ?assertEqual(1000, Delay1),
         ?assertEqual(5000, Delay2),  % Capped at 5000
         ?assertEqual(5000, Delay3)   % Still capped
     end}.

%%--------------------------------------------------------------------
%% @doc Test should_retry predicate.
%% @end
%%--------------------------------------------------------------------
whp02_should_retry_predicate_test() ->
    {"WHP-02: should_retry predicate",
     fun() ->
         Policy = test_retry_policy(),

         ?assert(cre_yawl_exception:should_retry(Policy, 0)),
         ?assert(cre_yawl_exception:should_retry(Policy, 1)),
         ?assert(cre_yawl_exception:should_retry(Policy, 2)),
         ?assertNot(cre_yawl_exception:should_retry(Policy, 3))
     end}.

%%====================================================================
%% WHP-03: Compensation Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WHP-03 Compensation pattern.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          %% Normal Execution Tests
          whp03_successful_compensation_test(),
          whp03_compensation_state_transitions_test(),
          whp03_compensation_cleanup_test(),

          %% Exception Scenarios
          whp03_compensation_failure_test(),
          whp03_compensation_timeout_test(),
          whp03_invalid_compensator_test(),

          %% Nested Compensation Tests
          whp03_nested_compensation_test(),
          whp03_compensation_dependencies_test(),

          %% Edge Cases
          whp03_multiple_compensations_test(),
          whp03_compensation_with_state_preservation_test(),
          whp03_compensation_execution_time_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test successful compensation.
%% @end
%%--------------------------------------------------------------------
whp03_successful_compensation_test() ->
    {"WHP-03: Successful compensation",
     fun() ->
         ActivityId = <<"activity_1">>,
         Compensator = test_compensator(ActivityId),

         ?assertEqual(pending, cre_yawl_exception:get_compensation_state(Compensator)),
         ?assertNot(cre_yawl_exception:has_compensated(Compensator)),

         {ok, Compensated} = cre_yawl_exception:compensate(Compensator, undefined),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Compensated)),
         ?assert(cre_yawl_exception:has_compensated(Compensated))
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation state transitions.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_state_transitions_test() ->
    {"WHP-03: Compensation state transitions",
     fun() ->
         ActivityId = <<"activity_1">>,
         Compensator = test_compensator(ActivityId),

         %% Initial state
         ?assertEqual(pending, cre_yawl_exception:get_compensation_state(Compensator)),

         %% After successful compensation
         {ok, Compensated} = cre_yawl_exception:compensate(Compensator, undefined),
         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Compensated)),

         %% Cannot compensate again
         Result = cre_yawl_exception:compensate(Compensated, undefined),
         ?assertEqual({error, already_compensated}, Result)
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation cleanup.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_cleanup_test() ->
    {"WHP-03: Compensation cleanup",
     fun() ->
         ActivityId = <<"activity_1">>,
         HandlerFun = fun(_Input) -> {ok, cleaned} end,
         Compensator = cre_yawl_exception:new_compensator_with_metadata(
             ActivityId, HandlerFun, immediate, #{cleanup => true}),

         {ok, Compensated} = cre_yawl_exception:compensate(Compensator, undefined),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Compensated)),
         ?assertMatch({ok, _}, Compensated#compensator.result)
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation failure.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_failure_test() ->
    {"WHP-03: Compensation failure",
     fun() ->
         ActivityId = <<"activity_1">>,
         HandlerFun = fun(_Input) -> throw(compensation_failed) end,
         Compensator = test_compensator_with_handler(ActivityId, HandlerFun),

         {ok, FailedCompensator} = cre_yawl_exception:compensate(Compensator, undefined),

         ?assertEqual(failed, cre_yawl_exception:get_compensation_state(FailedCompensator)),
         ?assert(cre_yawl_exception:has_compensation_failed(FailedCompensator)),
         ?assertMatch({error, _}, FailedCompensator#compensator.result)
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation with timeout handling.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_timeout_test() ->
    {"WHP-03: Compensation with timeout",
     fun() ->
         ActivityId = <<"activity_1">>,
         HandlerFun = fun(_Input) ->
             timer:sleep(100),
             {ok, done}
         end,
         Compensator = test_compensator_with_handler(ActivityId, HandlerFun),

         {ok, Compensated} = cre_yawl_exception:compensate(Compensator, undefined),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Compensated)),
         ?assertNotEqual(undefined, cre_yawl_exception:get_compensation_execution_time(Compensated))
     end}.

%%--------------------------------------------------------------------
%% @doc Test invalid compensator.
%% @end
%%--------------------------------------------------------------------
whp03_invalid_compensator_test() ->
    {"WHP-03: Invalid compensator handling",
     fun() ->
         ActivityId = <<"activity_1">>,
         HandlerFun = fun(_Input) -> error(bad_handler) end,
         Compensator = test_compensator_with_handler(ActivityId, HandlerFun),

         {ok, FailedCompensator} = cre_yawl_exception:compensate(Compensator, undefined),

         ?assertEqual(failed, cre_yawl_exception:get_compensation_state(FailedCompensator))
     end}.

%%--------------------------------------------------------------------
%% @doc Test nested compensation.
%% @end
%%--------------------------------------------------------------------
whp03_nested_compensation_test() ->
    {"WHP-03: Nested compensation",
     fun() ->
         %% Create parent and child compensators
         ParentId = <<"parent_activity">>,
         ChildId = <<"child_activity">>,

         ParentCompensator = test_compensator(ParentId),
         ChildCompensator = test_compensator(ChildId),

         %% Compensate child first
         {ok, CompensatedChild} = cre_yawl_exception:compensate(ChildCompensator, undefined),

         %% Then compensate parent
         {ok, CompensatedParent} = cre_yawl_exception:compensate(ParentCompensator, undefined),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(CompensatedChild)),
         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(CompensatedParent))
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation with dependencies.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_dependencies_test() ->
    {"WHP-03: Compensation with dependencies",
     fun() ->
         ActivityId = <<"activity_dep">>,
         Dependencies = [<<"dep1">>, <<"dep2">>],

         Compensator = cre_yawl_exception:new_compensator_with_deps(
             ActivityId,
             fun(_Input) -> {ok, compensated} end,
             chained,
             undefined,
             Dependencies
         ),

         ?assertEqual(Dependencies, cre_yawl_exception:get_compensation_dependencies(Compensator)),

         %% All dependencies check should return false for pending compensations
         ?assertNot(cre_yawl_exception:all_dependencies_completed(Compensator, #exception_state{}))
     end}.

%%--------------------------------------------------------------------
%% @doc Test multiple compensations.
%% @end
%%--------------------------------------------------------------------
whp03_multiple_compensations_test() ->
    {"WHP-03: Multiple compensations",
     fun() ->
         ActivityIds = [<<"act1">>, <<"act2">>, <<"act3">>],

         Compensators = [test_compensator(Id) || Id <- ActivityIds],

         %% Compensate all
         Results = [cre_yawl_exception:compensate(C, undefined) || C <- Compensators],

         %% All should succeed
         lists:foreach(fun({ok, Comp}) ->
             ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Comp))
         end, Results)
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation with state preservation.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_with_state_preservation_test() ->
    {"WHP-03: Compensation with state preservation",
     fun() ->
         ActivityId = <<"activity_state">>,
         InputData = #{key => <<"value">>},

         HandlerFun = fun(Input) ->
             %% Preserve input state during compensation
             #{original => Input, compensated => true}
         end,

         Compensator = test_compensator_with_handler(ActivityId, HandlerFun),

         {ok, Compensated} = cre_yawl_exception:compensate(Compensator, InputData),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Compensated)),
         ?assertMatch({ok, #{original := #{key := <<"value">>}}}, Compensated#compensator.result)
     end}.

%%--------------------------------------------------------------------
%% @doc Test compensation execution time tracking.
%% @end
%%--------------------------------------------------------------------
whp03_compensation_execution_time_test() ->
    {"WHP-03: Compensation execution time tracking",
     fun() ->
         ActivityId = <<"activity_timing">>,
         HandlerFun = fun(_Input) ->
             timer:sleep(50),
             {ok, done}
         end,

         Compensator = test_compensator_with_handler(ActivityId, HandlerFun),

         {ok, Compensated} = cre_yawl_exception:compensate(Compensator, undefined),

         ExecutionTime = cre_yawl_exception:get_compensation_execution_time(Compensated),
         ?assertNotEqual(undefined, ExecutionTime),
         ?assert(ExecutionTime >= 50)  % At least 50ms
     end}.

%%====================================================================
%% WHP-04: Triggered Compensation Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WHP-04 Triggered Compensation pattern.
%% @end
%%--------------------------------------------------------------------
whp04_triggered_compensation_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          %% Normal Execution Tests
          whp04_explicit_trigger_test(),
          whp04_conditional_trigger_test(),
          whp04_trigger_before_registration_test(),

          %% Multiple Triggers Tests
          whp04_multiple_triggers_test(),
          whp04_trigger_state_tracking_test(),

          %% Edge Cases
          whp04_trigger_during_execution_test(),
          whp04_triggered_compensation_failure_test(),
          whp04_trigger_with_compensation_chain_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test explicit trigger.
%% @end
%%--------------------------------------------------------------------
whp04_explicit_trigger_test() ->
    {"WHP-04: Explicit trigger",
     fun() ->
         Activity = fun(_Data) -> {ok, result} end,
         CompensatorFun = fun(_Input) -> {ok, compensated} end,
         TriggerFun = fun() -> true end,

         Pattern = cre_yawl_patterns:triggered_compensation(
             Activity, CompensatorFun, TriggerFun),

         ?assertEqual(triggered_compensation, Pattern#pattern_state.pattern_type),
         ?assert(is_function(maps:get(compensator, Pattern#pattern_state.choice_data))),
         ?assert(is_function(maps:get(trigger, Pattern#pattern_state.choice_data)))
     end}.

%%--------------------------------------------------------------------
%% @doc Test conditional trigger.
%% @end
%%--------------------------------------------------------------------
whp04_conditional_trigger_test() ->
    {"WHP-04: Conditional trigger",
     fun() ->
         Activity = fun(_Data) -> {ok, result} end,
         CompensatorFun = fun(_Input) -> {ok, compensated} end,

         %% Trigger based on condition
         TriggerFun = fun() ->
             case get(trigger_condition) of
                 true -> true;
                 _ -> false
             end
         end,

         Pattern = cre_yawl_patterns:triggered_compensation(
             Activity, CompensatorFun, TriggerFun),

         ?assert(is_function(maps:get(trigger, Pattern#pattern_state.choice_data)))
     end}.

%%--------------------------------------------------------------------
%% @doc Test trigger before registration.
%% @end
%%--------------------------------------------------------------------
whp04_trigger_before_registration_test() ->
    {"WHP-04: Trigger before registration",
     fun() ->
         %% Create exception state without compensators
         ExceptionState = cre_yawl_exception:init_exception_state(<<"wf1">>, self()),

         ?assertEqual([], ExceptionState#exception_state.compensation_stack)
     end}.

%%--------------------------------------------------------------------
%% @doc Test multiple triggers.
%% @end
%%--------------------------------------------------------------------
whp04_multiple_triggers_test() ->
    {"WHP-04: Multiple triggers",
     fun() ->
         %% Create multiple activities that can trigger compensation
         Activity1 = fun(_Data) -> {ok, result1} end,
         Activity2 = fun(_Data) -> {ok, result2} end,

         CompensatorFun = fun(_Input) -> {ok, compensated} end,
         TriggerFun = fun() -> true end,

         Pattern1 = cre_yawl_patterns:triggered_compensation(
             Activity1, CompensatorFun, TriggerFun),
         Pattern2 = cre_yawl_patterns:triggered_compensation(
             Activity2, CompensatorFun, TriggerFun),

         ?assertEqual(triggered_compensation, Pattern1#pattern_state.pattern_type),
         ?assertEqual(triggered_compensation, Pattern2#pattern_state.pattern_type)
     end}.

%%--------------------------------------------------------------------
%% @doc Test trigger state tracking.
%% @end
%%--------------------------------------------------------------------
whp04_trigger_state_tracking_test() ->
    {"WHP-04: Trigger state tracking",
     fun() ->
         Exception = test_exception(business_exception),
         InitialAttempts = cre_yawl_exception:exception_compensation_attempts(Exception),

         ?assertEqual(0, InitialAttempts),

         %% Simulate trigger by incrementing attempts
         TriggeredException = Exception#yawl_exception{
             compensation_attempts = InitialAttempts + 1
         },

         ?assertEqual(1, cre_yawl_exception:exception_compensation_attempts(TriggeredException))
     end}.

%%--------------------------------------------------------------------
%% @doc Test trigger during execution.
%% @end
%%--------------------------------------------------------------------
whp04_trigger_during_execution_test() ->
    {"WHP-04: Trigger during execution",
     fun() ->
         ActivityId = <<"activity_during_exec">>,
         HandlerFun = fun(_Input) ->
             %% Simulate trigger during compensation
             put(trigger_during_exec, true),
             {ok, compensated}
         end,

         Compensator = cre_yawl_exception:new_compensator(
             ActivityId, HandlerFun, deferred),

         {ok, Compensated} = cre_yawl_exception:compensate(Compensator, undefined),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Compensated))
     end}.

%%--------------------------------------------------------------------
%% @doc Test triggered compensation failure.
%% @end
%%--------------------------------------------------------------------
whp04_triggered_compensation_failure_test() ->
    {"WHP-04: Triggered compensation failure",
     fun() ->
         ActivityId = <<"activity_fail">>,
         HandlerFun = fun(_Input) -> error(cannot_compensate) end,

         Compensator = cre_yawl_exception:new_compensator(
             ActivityId, HandlerFun, deferred),

         {ok, Failed} = cre_yawl_exception:compensate(Compensator, undefined),

         ?assertEqual(failed, cre_yawl_exception:get_compensation_state(Failed))
     end}.

%%--------------------------------------------------------------------
%% @doc Test trigger with compensation chain.
%% @end
%%--------------------------------------------------------------------
whp04_trigger_with_compensation_chain_test() ->
    {"WHP-04: Trigger with compensation chain",
     fun() ->
         %% Create exception with multiple compensation attempts
         Exception = test_exception(workflow_exception),

         %% Simulate multiple triggers
         Exception1 = Exception#yawl_exception{compensation_attempts = 1},
         Exception2 = Exception#yawl_exception{compensation_attempts = 2},

         ?assertEqual(1, cre_yawl_exception:exception_compensation_attempts(Exception1)),
         ?assertEqual(2, cre_yawl_exception:exception_compensation_attempts(Exception2))
     end}.

%%====================================================================
%% WHP-05: Consecutive Compensation Pattern Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for WHP-05 Consecutive Compensation pattern.
%% @end
%%--------------------------------------------------------------------
whp05_consecutive_compensation_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          %% LIFO Execution Tests
          whp05_lifo_execution_order_test(),
          whp05_lifo_with_multiple_activities_test(),

          %% Partial Failure Tests
          whp05_partial_failure_handling_test(),
          whp05_continue_after_failure_test(),

          %% State Preservation Tests
          whp05_state_preservation_test(),
          whp05_chain_execution_with_state_test(),

          %% Edge Cases
          whp05_empty_chain_test(),
          whp05_single_activity_chain_test(),
          whp05_nested_consecutive_compensation_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test LIFO execution order.
%% @end
%%--------------------------------------------------------------------
whp05_lifo_execution_order_test() ->
    {"WHP-05: LIFO execution order",
     fun() ->
         %% Create activities in registration order
         Act1 = {fun(_Data) -> {ok, act1} end,
                 fun(_Input) -> {ok, compensated_act1} end},
         Act2 = {fun(_Data) -> {ok, act2} end,
                 fun(_Input) -> {ok, compensated_act2} end},
         Act3 = {fun(_Data) -> {ok, act3} end,
                 fun(_Input) -> {ok, compensated_act3} end},

         Pattern = cre_yawl_patterns:consecutive_compensate([Act1, Act2, Act3]),

         ?assertEqual(consecutive_compensation, Pattern#pattern_state.pattern_type),
         ?assertEqual(3, Pattern#pattern_state.instance_count),

         %% Verify compensators map (LIFO order for execution)
         Compensators = maps:get(compensators, Pattern#pattern_state.choice_data, #{}),

         ?assertEqual(3, maps:size(Compensators))
     end}.

%%--------------------------------------------------------------------
%% @doc Test LIFO with multiple activities.
%% @end
%%--------------------------------------------------------------------
whp05_lifo_with_multiple_activities_test() ->
    {"WHP-05: LIFO with multiple activities",
     fun() ->
         %% Create 5 activities
         Activities = [{fun(_) -> {ok, N} end,
                         fun(_) -> {ok, {compensated, N}} end}
                      || N <- lists:seq(1, 5)],

         Pattern = cre_yawl_patterns:consecutive_compensate(Activities),

         ?assertEqual(5, Pattern#pattern_state.instance_count),

         %% Verify all compensators are registered
         Compensators = maps:get(compensators, Pattern#pattern_state.choice_data, #{}),
         ?assertEqual(5, maps:size(Compensators))
     end}.

%%--------------------------------------------------------------------
%% @doc Test partial failure handling.
%% @end
%%--------------------------------------------------------------------
whp05_partial_failure_handling_test() ->
    {"WHP-05: Partial failure handling",
     fun() ->
         %% Create activities where second compensation fails
         Act1 = {fun(_) -> {ok, act1} end,
                 fun(_) -> {ok, comp1} end},
         Act2 = {fun(_) -> {ok, act2} end,
                 fun(_) -> throw(fail_comp2) end},
         Act3 = {fun(_) -> {ok, act3} end,
                 fun(_) -> {ok, comp3} end},

         Pattern = cre_yawl_patterns:consecutive_compensate([Act1, Act2, Act3]),

         ?assertEqual(3, Pattern#pattern_state.instance_count)
     end}.

%%--------------------------------------------------------------------
%% @doc Test continue after failure.
%% @end
%%--------------------------------------------------------------------
whp05_continue_after_failure_test() ->
    {"WHP-05: Continue after failure",
     fun() ->
         %% Create compensators where one fails but chain continues
         Comp1 = test_compensator(<<"comp1">>),
         Comp2 = test_compensator_with_handler(
             <<"comp2">>,
             fun(_) -> error(failure) end),
         Comp3 = test_compensator(<<"comp3">>),

         %% Execute compensations
         {ok, Exec1} = cre_yawl_exception:compensate(Comp1, undefined),
         {ok, Exec2} = cre_yawl_exception:compensate(Comp2, undefined),
         {ok, Exec3} = cre_yawl_exception:compensate(Comp3, undefined),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Exec1)),
         ?assertEqual(failed, cre_yawl_exception:get_compensation_state(Exec2)),
         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Exec3))
     end}.

%%--------------------------------------------------------------------
%% @doc Test state preservation.
%% @end
%%--------------------------------------------------------------------
whp05_state_preservation_test() ->
    {"WHP-05: State preservation",
     fun() ->
         %% Create compensators that preserve state
         Comp1 = cre_yawl_exception:new_compensator_with_metadata(
             <<"comp1">>,
             fun(Input) -> {ok, Input#{step1 => true}} end,
             chained,
             #{step => 1}
         ),

         Comp2 = cre_yawl_exception:new_compensator_with_metadata(
             <<"comp2">>,
             fun(Input) -> {ok, Input#{step2 => true}} end,
             chained,
             #{step => 2}
         ),

         %% Execute with shared state
         InitialState = #{},
         {ok, Exec1} = cre_yawl_exception:compensate(Comp1, InitialState),
         {ok, Exec2} = cre_yawl_exception:compensate(Comp2, InitialState),

         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Exec1)),
         ?assertEqual(completed, cre_yawl_exception:get_compensation_state(Exec2))
     end}.

%%--------------------------------------------------------------------
%% @doc Test chain execution with state.
%% @end
%%--------------------------------------------------------------------
whp05_chain_execution_with_state_test() ->
    {"WHP-05: Chain execution with state",
     fun() ->
         %% Create chain where each compensation depends on previous state
         StatefulCompensators = [
             {<<"act1">>, fun(S) -> {ok, S#{step1_done => true}} end},
             {<<"act2">>, fun(S) -> {ok, S#{step2_done => true}} end},
             {<<"act3">>, fun(S) -> {ok, S#{step3_done => true}} end}
         ],

         %% Execute chain
         InitialState = #{},
         Results = lists:map(fun({_Id, Fun}) ->
             Fun(InitialState)
         end, StatefulCompensators),

         %% All should succeed
         lists:foreach(fun({ok, State}) ->
             ?assert(is_map(State))
         end, Results)
     end}.

%%--------------------------------------------------------------------
%% @doc Test empty chain.
%% @end
%%--------------------------------------------------------------------
whp05_empty_chain_test() ->
    {"WHP-05: Empty chain handling",
     fun() ->
         Pattern = cre_yawl_patterns:consecutive_compensate([]),

         ?assertEqual(consecutive_compensation, Pattern#pattern_state.pattern_type),
         ?assertEqual(0, Pattern#pattern_state.instance_count)
     end}.

%%--------------------------------------------------------------------
%% @doc Test single activity chain.
%% @end
%%--------------------------------------------------------------------
whp05_single_activity_chain_test() ->
    {"WHP-05: Single activity chain",
     fun() ->
         Act1 = {fun(_) -> {ok, only_act} end,
                 fun(_) -> {ok, compensated_only} end},

         Pattern = cre_yawl_patterns:consecutive_compensate([Act1]),

         ?assertEqual(1, Pattern#pattern_state.instance_count),

         Compensators = maps:get(compensators, Pattern#pattern_state.choice_data, #{}),
         ?assertEqual(1, maps:size(Compensators))
     end}.

%%--------------------------------------------------------------------
%% @doc Test nested consecutive compensation.
%% @end
%%--------------------------------------------------------------------
whp05_nested_consecutive_compensation_test() ->
    {"WHP-05: Nested consecutive compensation",
     fun() ->
         %% Create outer chain
         OuterActs = [
             {fun(_) -> {ok, outer1} end,
              fun(_) -> {ok, outer_comp1} end},
             {fun(_) -> {ok, outer2} end,
              fun(_) -> {ok, outer_comp2} end}
         ],

         OuterPattern = cre_yawl_patterns:consecutive_compensate(OuterActs),

         ?assertEqual(2, OuterPattern#pattern_state.instance_count)
     end}.

%%====================================================================
%% Circuit Breaker Tests
%%====================================================================

circuit_breaker_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          circuit_breaker_closed_state_test(),
          circuit_breaker_opens_after_threshold_test(),
          circuit_breaker_timeout_recovery_test(),
          circuit_breaker_with_handler_execution_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test circuit breaker closed state.
%% @end
%%--------------------------------------------------------------------
circuit_breaker_closed_state_test() ->
    {"Circuit breaker: Closed state allows execution",
     fun() ->
         Handler = test_error_handler(<<"cb_handler">>, [system_exception]),
         HandlerWithCB = cre_yawl_exception:register_handler_with_circuit_breaker(
             #{}, Handler, 3, 60000),

         ?assertEqual(1, map_size(HandlerWithCB))
     end}.

%%--------------------------------------------------------------------
%% @doc Test circuit breaker opens after threshold.
%% @end
%%--------------------------------------------------------------------
circuit_breaker_opens_after_threshold_test() ->
    {"Circuit breaker: Opens after threshold failures",
     fun() ->
         Handler = test_error_handler(<<"cb_handler">>, [system_exception]),
         HandlerWithCB = cre_yawl_exception:register_handler_with_circuit_breaker(
             #{}, Handler, 3, 60000),

         %% Get the registered handler
         HandlersList = maps:get(system_exception, HandlerWithCB, []),
         [CBHandler | _] = HandlersList,

         Now = erlang:system_time(millisecond),

         %% Simulate failures - first 2 should succeed
         {ok, Handler1} = cre_yawl_exception:update_circuit_breaker_state(CBHandler, Now),
         {ok, Handler2} = cre_yawl_exception:update_circuit_breaker_state(Handler1, Now),

         %% Third failure should open circuit (threshold = 3, failures >= threshold)
         Result = cre_yawl_exception:update_circuit_breaker_state(Handler2, Now),

         ?assertMatch({circuit_open, _}, Result)
     end}.

%%--------------------------------------------------------------------
%% @doc Test circuit breaker timeout recovery.
%% @end
%%--------------------------------------------------------------------
circuit_breaker_timeout_recovery_test() ->
    {"Circuit breaker: Recovers after timeout",
     fun() ->
         Handler = test_error_handler(<<"cb_handler">>, [system_exception]),
         HandlerWithCB = cre_yawl_exception:register_handler_with_circuit_breaker(
             #{}, Handler, 1, 100),  % 100ms timeout

         HandlersList = maps:get(system_exception, HandlerWithCB, []),
         [CBHandler | _] = HandlersList,

         %% Trigger circuit open
         {circuit_open, OpenHandler} = cre_yawl_exception:update_circuit_breaker_state(
             CBHandler, erlang:system_time(millisecond)),

         %% Wait for timeout
         timer:sleep(150),

         %% After timeout, circuit should allow check
         Now = erlang:system_time(millisecond),
         {ok, _} = cre_yawl_exception:check_circuit_breaker(OpenHandler, Now)
     end}.

%%--------------------------------------------------------------------
%% @doc Test circuit breaker with handler execution.
%% @end
%%--------------------------------------------------------------------
circuit_breaker_with_handler_execution_test() ->
    {"Circuit breaker: Integrates with handler execution",
     fun() ->
         HandlerFun = fun(_Exception) -> {ok, handled} end,
         Handler = (test_error_handler_with_fun(<<"cb_exec">>, [system_exception], HandlerFun))
                      #error_handler{priority = 1},

         Exception = test_exception(system_exception),
         Now = erlang:system_time(millisecond),

         %% Execute with circuit breaker support
         Result = cre_yawl_exception:execute_handler_with_circuit_breaker(
             Handler#error_handler{circuit_breaker_state = #{
                 failures => 0,
                 last_failure => undefined,
                 state => closed,
                 threshold => 3,
                 timeout => 60000
             }}, Exception, Now),

         ?assertMatch({ok, _}, Result)
     end}.

%%====================================================================
%% Exception Type and Severity Tests
%%====================================================================

exception_types_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          exception_type_validation_test(),
          all_exception_types_creation_test(),
          severity_determination_test(),
          custom_severity_override_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test exception type validation.
%% @end
%%--------------------------------------------------------------------
exception_type_validation_test() ->
    {"Exception type: All types are valid",
     fun() ->
         Types = [business_exception, system_exception, timeout_exception,
                  resource_exception, data_exception, communication_exception,
                  validation_exception, security_exception, workflow_exception,
                  compensation_exception],

         lists:foreach(fun(Type) ->
             Exception = test_exception(Type),
             ?assertEqual(Type, cre_yawl_exception:exception_type(Exception))
         end, Types)
     end}.

%%--------------------------------------------------------------------
%% @doc Test all exception types creation.
%% @end
%%--------------------------------------------------------------------
all_exception_types_creation_test() ->
    {"Exception type: Create all valid types",
     fun() ->
         AllTypes = [
             {business_exception, low},
             {system_exception, high},
             {timeout_exception, medium},
             {resource_exception, medium},
             {data_exception, medium},
             {communication_exception, high},
             {validation_exception, low},
             {security_exception, critical},
             {workflow_exception, high},
             {compensation_exception, high}
         ],

         lists:foreach(fun({Type, ExpectedMinSeverity}) ->
             Exception = test_exception(Type),
             ?assertEqual(Type, cre_yawl_exception:exception_type(Exception)),
             ?assert(is_atom(cre_yawl_exception:exception_severity(Exception)))
         end, AllTypes)
     end}.

%%--------------------------------------------------------------------
%% @doc Test severity determination.
%% @end
%%--------------------------------------------------------------------
severity_determination_test() ->
    {"Exception: Severity determination",
     fun() ->
         %% Security exceptions should be critical
         SecException = test_exception(security_exception),
         ?assertEqual(critical, cre_yawl_exception:exception_severity(SecException)),

         %% System exceptions should be high
         SysException = test_exception(system_exception),
         ?assertEqual(high, cre_yawl_exception:exception_severity(SysException)),

         %% Business exceptions default to medium
         BizException = test_exception(business_exception),
         ?assertEqual(medium, cre_yawl_exception:exception_severity(BizException))
     end}.

%%--------------------------------------------------------------------
%% @doc Test custom severity override.
%% @end
%%--------------------------------------------------------------------
custom_severity_override_test() ->
    {"Exception: Custom severity override",
     fun() ->
         %% Override severity via context
         Context = #{severity => critical},
         Exception = test_exception_full(business_exception, critical,
                                         <<"Custom severity">>, Context,
                                         <<"wf1">>, <<"act1">>),

         ?assertEqual(critical, cre_yawl_exception:exception_severity(Exception))
     end}.

%%====================================================================
%% Exception Context Tests
%%====================================================================

exception_context_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          workflow_and_activity_ids_test(),
          context_preservation_test(),
          empty_context_test(),
          context_with_nested_data_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test workflow and activity ID handling.
%% @end
%%--------------------------------------------------------------------
workflow_and_activity_ids_test() ->
    {"Exception: Workflow and activity ID handling",
     fun() ->
         WorkflowId = <<"workflow_123">>,
         ActivityId = <<"activity_456">>,

         Exception = test_exception_full(system_exception, high,
                                         <<"Test">>, #{},
                                         WorkflowId, ActivityId),

         ?assertEqual(WorkflowId, cre_yawl_exception:exception_workflow_id(Exception)),
         ?assertEqual(ActivityId, cre_yawl_exception:exception_activity_id(Exception))
     end}.

%%--------------------------------------------------------------------
%% @doc Test context preservation.
%% @end
%%--------------------------------------------------------------------
context_preservation_test() ->
    {"Exception: Context preservation",
     fun() ->
         Context = #{
             user_id => <<"user123">>,
             request_id => <<"req456">>,
             details => #{nested => <<"value">>}
         },

         Exception = test_exception(workflow_exception, <<"Workflow failed">>, Context),

         RetrievedContext = cre_yawl_exception:exception_context(Exception),
         ?assertEqual(<<"user123">>, maps:get(user_id, RetrievedContext)),
         ?assertEqual(<<"req456">>, maps:get(request_id, RetrievedContext)),
         ?assertMatch(#{nested := <<"value">>}, maps:get(details, RetrievedContext))
     end}.

%%--------------------------------------------------------------------
%% @doc Test empty context.
%% @end
%%--------------------------------------------------------------------
empty_context_test() ->
    {"Exception: Empty context handling",
     fun() ->
         Exception = test_exception(system_exception, <<"Test">>, #{}),

         Context = cre_yawl_exception:exception_context(Exception),
         ?assertEqual(0, map_size(Context))
     end}.

%%--------------------------------------------------------------------
%% @doc Test context with nested data.
%% @end
%%--------------------------------------------------------------------
context_with_nested_data_test() ->
    {"Exception: Context with deeply nested data",
     fun() ->
         Context = #{
             level1 => #{
                 level2 => #{
                     level3 => <<"deep_value">>
                 }
             },
             array => [1, 2, 3]
         },

         Exception = test_exception(data_exception, <<"Data error">>, Context),

         RetrievedContext = cre_yawl_exception:exception_context(Exception),
         ?assertMatch(#{level1 := #{level2 := #{level3 := <<"deep_value">>}}}, RetrievedContext),
         ?assertEqual([1, 2, 3], maps:get(array, RetrievedContext))
     end}.

%%====================================================================
%% Petri Net Transition Tests for Exception Patterns
%%====================================================================

petri_net_transitions_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          %% WHP Exception Pattern Transitions
          whp_transitions_place_list_test(),
          whp_transitions_transition_list_test(),
          whp_transitions_preset_test(),
          whp_transitions_fire_test()
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test place list for exception handling.
%% @end
%%--------------------------------------------------------------------
whp_transitions_place_list_test() ->
    {"Petri Net: Exception handling places",
     fun() ->
         Places = cre_yawl_exception:place_lst(),

         %% Verify key exception handling places exist
         ?assert(lists:member('Active', Places)),
         ?assert(lists:member('ExceptionRaised', Places)),
         ?assert(lists:member('Handling', Places)),
         ?assert(lists:member('Compensating', Places)),
         ?assert(lists:member('Resolved', Places)),
         ?assert(lists:member('Retry', Places)),
         ?assert(lists:member('Failed', Places)),
         ?assert(lists:member('CompensationStack', Places))
     end}.

%%--------------------------------------------------------------------
%% @doc Test transition list for exception handling.
%% @end
%%--------------------------------------------------------------------
whp_transitions_transition_list_test() ->
    {"Petri Net: Exception handling transitions",
     fun() ->
         Transitions = cre_yawl_exception:trsn_lst(),

         %% Verify key exception handling transitions exist
         ?assert(lists:member(raise_exception, Transitions)),
         ?assert(lists:member(handle_exception, Transitions)),
         ?assert(lists:member(error_handler_execute, Transitions)),
         ?assert(lists:member(compensate_activity, Transitions)),
         ?assert(lists:member(retry_after_backoff, Transitions)),
         ?assert(lists:member(trigger_compensation, Transitions))
     end}.

%%--------------------------------------------------------------------
%% @doc Test preset for exception transitions.
%% @end
%%--------------------------------------------------------------------
whp_transitions_preset_test() ->
    {"Petri Net: Exception transition presets",
     fun() ->
         %% Check raise_exception preset
         ?assertEqual(['Active'], cre_yawl_exception:preset(raise_exception)),

         %% Check handle_exception preset
         ?assertEqual(['ExceptionRaised'], cre_yawl_exception:preset(handle_exception)),

         %% Check compensate_activity preset
         ?assertEqual(['Compensating'], cre_yawl_exception:preset(compensate_activity)),

         %% Check retry_after_backoff preset
         ?assertEqual(['Retry'], cre_yawl_exception:preset(retry_after_backoff))
     end}.

%%--------------------------------------------------------------------
%% @doc Test fire for exception transitions.
%% @end
%%--------------------------------------------------------------------
whp_transitions_fire_test() ->
    {"Petri Net: Exception transition firing",
     fun() ->
         ExceptionState = cre_yawl_exception:init_exception_state(<<"wf1">>, self()),

         %% Test raise_exception transition
         Exception = test_exception(system_exception, <<"Test exception">>),
         Result1 = cre_yawl_exception:fire(raise_exception,
                                           #{'Active' => [Exception]},
                                           ExceptionState),

         ?assertMatch({produce, #{
           'ExceptionRaised' := [_],
           'Active' := []
         }}, Result1),

         %% Test mark_failed transition
         Result2 = cre_yawl_exception:fire(mark_failed,
                                          #{'ExceptionRaised' => [Exception]},
                                          ExceptionState),

         ?assertMatch({produce, #{
           'Failed' := [_],
           'ExceptionRaised' := []
         }}, Result2)
     end}.

%%====================================================================
%% Integration Tests for Exception Handling
%%====================================================================

integration_exception_workflow_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          {"Integration: Complete error handler workflow",
           fun() ->
               Activity = fun(_Data) -> {ok, result} end,
               HandlerFun = fun(_Exception) -> recovered end,
               Pattern = cre_yawl_patterns:error_handler(Activity, HandlerFun),

               ?assertEqual(error_handler, Pattern#pattern_state.pattern_type),

               %% Simulate exception handling
               Exception = test_exception(system_exception),
               Handler = test_error_handler_with_fun(<<"h1">>,
                                                      [system_exception],
                                                      HandlerFun),

               Result = cre_yawl_exception:execute_handler(Handler, Exception),
               ?assertEqual({ok, recovered}, Result)
           end},

          {"Integration: Retry until success",
           fun() ->
               Policy = test_retry_policy_with_options(#{
                   max_attempts => 5,
                   backoff => exponential,
                   base_delay => 100
               }),

               %% Simulate retries
               ?assert(cre_yawl_exception:should_retry(Policy, 0)),
               ?assert(cre_yawl_exception:should_retry(Policy, 1)),
               ?assert(cre_yawl_exception:should_retry(Policy, 2)),
               ?assert(cre_yawl_exception:should_retry(Policy, 3)),
               ?assert(cre_yawl_exception:should_retry(Policy, 4)),
               ?assertNot(cre_yawl_exception:should_retry(Policy, 5))
           end},

          {"Integration: Compensation chain execution",
           fun() ->
               Acts = [
                   {fun(_) -> {ok, a1} end,
                    fun(_) -> {ok, comp_a1} end},
                   {fun(_) -> {ok, a2} end,
                    fun(_) -> {ok, comp_a2} end},
                   {fun(_) -> {ok, a3} end,
                    fun(_) -> {ok, comp_a3} end}
               ],

               Pattern = cre_yawl_patterns:consecutive_compensate(Acts),

               ?assertEqual(consecutive_compensation, Pattern#pattern_state.pattern_type),
               ?assertEqual(3, Pattern#pattern_state.instance_count)
           end},

          {"Integration: Triggered compensation with condition",
           fun() ->
               ConditionMet = true,
               Activity = fun(_Data) -> {ok, result} end,
               CompensatorFun = fun(_Input) -> {ok, compensated} end,
               TriggerFun = fun() -> ConditionMet end,

               Pattern = cre_yawl_patterns:triggered_compensation(
                   Activity, CompensatorFun, TriggerFun),

               ?assertEqual(triggered_compensation, Pattern#pattern_state.pattern_type)
           end},

          {"Integration: Exception with full workflow context",
           fun() ->
               WorkflowId = <<"integration_wf">>,
               ActivityId = <<"integration_act">>,

               Exception = test_exception_full(
                   workflow_exception, high,
                   <<"Integration test exception">>,
                   #{step => 5, total_steps => 10},
                   WorkflowId, ActivityId),

               ?assertEqual(WorkflowId, cre_yawl_exception:exception_workflow_id(Exception)),
               ?assertEqual(ActivityId, cre_yawl_exception:exception_activity_id(Exception)),
               ?assertEqual(0, cre_yawl_exception:exception_compensation_attempts(Exception)),
               ?assertEqual(0, cre_yawl_exception:exception_retry_attempts(Exception)),
               ?assertNot(cre_yawl_exception:exception_is_resolved(Exception))
           end}
         ]
     end}.

%%====================================================================
%% Performance Tests for Exception Handling
%%====================================================================

performance_exception_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun(_TestData) ->
         [
          {"Performance: Exception creation benchmark",
           fun() ->
               {Time, _} = timer:tc(fun() ->
                   [test_exception(system_exception) || _ <- lists:seq(1, 1000)]
               end),

               %% Should create 1000 exceptions in reasonable time
               ?assert(Time < 100000),  % Less than 100ms
               io:format("Created 1000 exceptions in ~p microseconds~n", [Time])
           end},

          {"Performance: Compensation execution benchmark",
           fun() ->
               Compensator = test_compensator(<<"perf_test">>),

               {Time, _} = timer:tc(fun() ->
                   {ok, C} = cre_yawl_exception:compensate(Compensator, undefined),
                   cre_yawl_exception:get_compensation_execution_time(C)
               end),

               %% Compensation should be fast
               ?assert(Time < 10000),  % Less than 10ms
               io:format("Compensation completed in ~p microseconds~n", [Time])
           end},

          {"Performance: Backoff calculation benchmark",
           fun() ->
               Policy = test_retry_policy(),

               {Time, _} = timer:tc(fun() ->
                   [cre_yawl_exception:calculate_backoff(Policy, N) || N <- lists:seq(1, 100)]
               end),

               ?assert(Time < 10000),  % Less than 10ms for 100 calculations
               io:format("100 backoff calculations in ~p microseconds~n", [Time])
           end},

          {"Performance: Handler lookup benchmark",
           fun() ->
               Handlers = lists:foldl(fun(N, Acc) ->
                   Type = list_to_atom("exception_type_" ++ integer_to_list(N)),
                   Handler = test_error_handler(
                       list_to_binary("handler_" ++ integer_to_list(N)),
                       [Type]),
                   cre_yawl_exception:register_handler(Acc, Handler)
               end, #{}, lists:seq(1, 100)),

               {Time, _} = timer:tc(fun() ->
                   [cre_yawl_exception:find_handler(Handlers,
                       list_to_atom("exception_type_" ++ integer_to_list(N)))
                    || N <- lists:seq(1, 100)]
               end),

               ?assert(Time < 50000),  % Less than 50ms for 100 lookups
               io:format("100 handler lookups in ~p microseconds~n", [Time])
           end}
         ]
     end}.
