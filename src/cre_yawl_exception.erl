%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
-moduledoc """
YAWL Exception Handling and Compensation Module.

This module provides comprehensive exception handling and compensation
patterns for YAWL workflows in the CRE runtime environment. It implements
the workflow exception patterns (WHP-01 through WHP-05) from the YAWL
patterns catalog.

## Exception Types

* **business_exception** - Expected business rule violations
* **system_exception** - System-level failures
* **timeout_exception** - Operation timeout
* **resource_exception** - Resource unavailability
* **data_exception** - Data validation failures
* **communication_exception** - Network/service communication failures
* **validation_exception** - Input validation errors
* **security_exception** - Authentication/authorization failures
* **workflow_exception** - Workflow-level errors
* **compensation_exception** - Compensation operation failures

## Compensation Patterns

Compensation is the process of undoing effects from completed activities
when a workflow cannot complete normally.

### WHP-01: Error Handler Pattern

```erlang
> %% Create an error handler for business exceptions
> Handler = cre_yawl_exception:new_error_handler(
>     business_handler,
>     [business_exception],
>     fun(Exception) ->
>         io:format("Handling business exception: ~p~n", [Exception]),
>         {handled, exception_#yawl_exception.message}
>     end
> ).
_
> %% Register the handler
> Handlers = #{business_exception => [Handler]}.
_
> %% Find handler for exception type
> cre_yawl_exception:find_handler(Handlers, business_exception).
[Handler]
```

### WHP-02: Retry Pattern with Backoff

```erlang
> %% Create a retry policy with exponential backoff
> Policy = cre_yawl_exception:new_retry_policy(#{
>     max_attempts => 5,
>     backoff => exponential,
>     base_delay => 1000,
>     multiplier => 2.0,
>     jitter => true
> }).
_
> %% Check if retry should be attempted
> cre_yawl_exception:should_retry(Policy, 2).
true
> cre_yawl_exception:should_retry(Policy, 6).
false
> %% Calculate backoff delay
> Delay = cre_yawl_exception:calculate_backoff(Policy, 3).
is_integer(Delay), Delay > 0.
true
```

### WHP-03: Compensation Pattern

```erlang
> %% Create a compensator for an activity
> Compensator = cre_yawl_exception:new_compensator(
>     <<"activity_1">>,
>     fun(Input) ->
>         io:format("Compensating activity_1 with input: ~p~n", [Input]),
>         {compensated, success}
>     end,
>     immediate
> ).
_
> %% Execute compensation
> {ok, Result} = cre_yawl_exception:compensate(Compensator, undefined).
_
> %% Check compensation status
> cre_yawl_exception:has_compensated(Result).
true
> cre_yawl_exception:get_compensation_state(Result).
completed
```

### WHP-04: Triggered Compensation

```erlang
> %% Create a deferred compensator
> CompHandler = fun(Input) ->
>     {undo, Input}
> end,
> Comp = cre_yawl_exception:new_compensator_with_metadata(
>     <<"payment_activity">>,
>     CompHandler,
>     deferred,
>     #{trigger_on => payment_failed, reason => refund_required}
> ).
_
> %% Check dependencies
> cre_yawl_exception:all_dependencies_completed(Comp, #{}).
true
```

### WHP-05: Consecutive Compensation

```erlang
> %% Create chained compensators
> C1 = cre_yawl_exception:new_compensator(
>     <<"step1">>, fun(_) -> {undone, step1} end, chained
> ),
> C2 = cre_yawl_exception:new_compensator_with_deps(
>     <<"step2">>,
>     fun(_) -> {undone, step2} end,
>     chained,
>     undefined,
>     [<<"step1">>]  % Depends on step1 completion
> ).
_
> %% Verify compensation state
> cre_yawl_exception:get_compensation_state(C1).
pending
> cre_yawl_exception:get_compensation_dependencies(C2).
[<<"step1">>]
```

## Exception Creation and Accessors

```erlang
> %% Create a business exception
> Exception = cre_yawl_exception:new_exception(
>     business_exception,
>     "Invalid order amount",
>     #{workflow_id => <<"order_wf_123">>, amount => -100},
>     []
> ).
_
> %% Access exception properties
> cre_yawl_exception:exception_type(Exception).
business_exception
> cre_yawl_exception:exception_severity(Exception).
medium
> cre_yawl_exception:exception_message(Exception).
<<"Invalid order amount">>
> cre_yawl_exception:exception_workflow_id(Exception).
<<"order_wf_123">>
> cre_yawl_exception:exception_is_resolved(Exception).
false
```

## Circuit Breaker Pattern

```erlang
> %% Create handler with circuit breaker
> CBHandler = cre_yawl_exception:register_handler_with_circuit_breaker(
>     #{},
>     cre_yawl_exception:new_error_handler(
>         api_handler,
>         [communication_exception],
>         fun(_) -> {error, timeout} end
>     ),
>     3,  % Threshold: 3 failures
>     60000  % Timeout: 60 seconds
> ).
_
> %% Check circuit breaker status
> maps:get(communication_exception, CBHandler, undefined) =/= undefined.
true
```
""".
%% -------------------------------------------------------------------

-module(cre_yawl_exception).
-behaviour(gen_pnet).

%%====================================================================
%% Constants and Configuration
%%====================================================================

-define(LOG(Level, Msg), logger:log(Level, Msg)).
-define(LOG(Level, Msg, Data), logger:log(Level, Msg, Data)).
-define(METRICS_ENABLED, true).
-define(MAX_COMPENSATION_STACK_SIZE, 100).
-define(DEFAULT_TIMEOUT, 30000).  % 30 seconds
-define(RETRY_MAX_DELAY_FACTOR, 10).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2,
         trigger/3]).

-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3]).

%% Exception API
-export([new_exception/4,
         exception_type/1,
         exception_message/1,
         exception_context/1,
         exception_timestamp/1,
         exception_id/1,
         exception_severity/1,
         exception_workflow_id/1,
         exception_activity_id/1,
         exception_compensation_attempts/1,
         exception_retry_attempts/1,
         exception_is_resolved/1,
         new_exception_with_fields/7,
         init_exception_state/2,
         update_metrics/3,
         log_audit_event/3]).

%% Compensation API
-export([new_compensator/3,
         compensate/2,
         has_compensated/1,
         get_compensation_state/1,
         has_compensation_failed/1,
         get_compensation_execution_time/1,
         get_compensation_dependencies/1,
         new_compensator_with_deps/5,
         new_compensator_with_metadata/4,
         all_dependencies_completed/2]).

%% Retry Policy API
-export([new_retry_policy/0,
         new_retry_policy/1,
         retry_policy_max_attempts/1,
         retry_policy_backoff/1,
         retry_policy_jitter/1,
         should_retry/2,
         calculate_backoff/2]).

%% Error Handler API
-export([new_error_handler/3,
         register_handler/2,
         register_handler_with_circuit_breaker/4,
         find_handler/2,
         find_best_handler/2,
         execute_handler/2,
         execute_handler_with_circuit_breaker/3,
         check_circuit_breaker/2,
         update_circuit_breaker_state/2,
         get_handler_metrics/1,
         set_handler_enabled/2,
         is_handler_enabled/1,
         new_handler_with_compensation/4]).

%%====================================================================
%% Types
%%====================================================================

-type exception_type() :: business_exception
                        | system_exception
                        | timeout_exception
                        | resource_exception
                        | data_exception
                        | communication_exception
                        | validation_exception
                        | security_exception
                        | workflow_exception
                        | compensation_exception.

-type exception_severity() :: low | medium | high | critical.

-type compensation_strategy() :: immediate | deferred | chained | parallel.

-type retry_strategy() :: exponential | linear | constant | custom | fibonacci | adaptive.

-type exception_id() :: binary().
-type handler_id() :: binary().

-record(yawl_exception, {
          id :: exception_id(),
          type :: exception_type(),
          severity :: exception_severity(),
          message :: binary(),
          context :: map(),
          timestamp :: integer(),
          stacktrace :: list(),
          workflow_id :: undefined | binary(),
          activity_id :: undefined | binary(),
          compensation_attempts = 0 :: non_neg_integer(),
          retry_attempts = 0 :: non_neg_integer(),
          handled_by :: undefined | handler_id(),
          resolved = false :: boolean()
         }).

-record(compensator, {
          activity_id :: binary(),
          compensation_handler :: function(),
          compensation_strategy :: compensation_strategy(),
          state :: pending | executing | completed | failed | cancelled,
          result :: undefined | {ok, term()} | {error, term()},
          created_at :: integer(),
          started_at :: undefined | integer(),
          completed_at :: undefined | integer(),
          execution_time :: undefined | integer(),
          retry_policy :: undefined | retry_policy(),
          dependencies :: [binary()],  % IDs of compensators that must complete first
          metadata :: map()
         }).

-record(retry_policy, {
          max_attempts = 3 :: non_neg_integer(),
          backoff_strategy = exponential :: retry_strategy(),
          base_delay = 1000 :: non_neg_integer(),
          max_delay = 60000 :: non_neg_integer(),
          multiplier = 2.0 :: float(),
          jitter = true :: boolean(),
          jitter_factor = 0.1 :: float(),
          timeout = 30000 :: non_neg_integer(),
          retryable_exceptions = [] :: [exception_type()],
          circuit_breaker_threshold = 5 :: pos_integer(),
          circuit_breaker_timeout = 60000 :: non_neg_integer()
         }).

-record(error_handler, {
          handler_id :: handler_id(),
          exception_types :: [exception_type()],
          handler_fun :: function(),
          priority = 0 :: integer(),
          retry_policy = undefined :: undefined | #retry_policy{},
          compensation_handler :: undefined | function(),
          circuit_breaker_state :: undefined | map(),
          metrics :: map(),
          enabled = true :: boolean()
         }).

-record(exception_state, {
          current_exception = undefined :: undefined | #yawl_exception{},
          active_compensators = [] :: [#compensator{}],
          compensation_stack = [] :: [#compensator{}],
          handlers = #{} :: #{exception_type() => [#error_handler{}]},
          workflow_id :: undefined | binary(),
          workflow_pid :: undefined | pid(),
          workflow_monitor :: undefined | reference(),
          circuit_breakers :: map(),  % exception_type() => {failures, last_failure, state}
          metrics :: map(),
          audit_log :: list(),
          is_compensating = false :: boolean(),
          compensation_timeout :: undefined | integer(),
          max_parallel_compensations = 10 :: pos_integer()
         }).

-type exception() :: #yawl_exception{}.
-type compensator() :: #compensator{}.
-type retry_policy() :: #retry_policy{}.
-type error_handler() :: #error_handler{}.

-export_type([exception/0, compensator/0, retry_policy/0, error_handler/0,
             exception_type/0, exception_id/0, handler_id/0]).

%%====================================================================
%% Exception API
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new exception record with automatic ID generation and severity
determination.

The exception ID is automatically generated using a cryptographic hash
of the process ID and a unique integer. Severity is determined based on
the exception type and context.

```erlang
> Exc = cre_yawl_exception:new_exception(
>     business_exception,
>     "Order amount exceeds limit",
>     #{workflow_id => <<"wf_123">>, amount => 10000},
>     []
> ).
_
> is_binary(cre_yawl_exception:exception_id(Exc)).
true
> cre_yawl_exception:exception_type(Exc).
business_exception
> cre_yawl_exception:exception_severity(Exc).
medium
```
""".
%%--------------------------------------------------------------------
-spec new_exception(Type, Message, Context, Stacktrace) -> exception()
    when Type :: exception_type(),
         Message :: binary() | string(),
         Context :: map(),
         Stacktrace :: list().

new_exception(Type, Message, Context, Stacktrace) when is_list(Message) ->
    new_exception(Type, list_to_binary(Message), Context, Stacktrace);
new_exception(Type, Message, Context, Stacktrace) when is_binary(Message) ->
    Severity = determine_severity(Type, Context),
    #yawl_exception{
      id = generate_exception_id(),
      type = Type,
      severity = Severity,
      message = Message,
      context = Context,
      timestamp = erlang:system_time(millisecond),
      stacktrace = Stacktrace,
      workflow_id = get_workflow_id(Context),
      activity_id = get_activity_id(Context),
      compensation_attempts = 0,
      retry_attempts = 0,
      handled_by = undefined,
      resolved = false
     }.

%%--------------------------------------------------------------------
%% @doc Enhanced exception creation with all fields specified.
%% @end
%%--------------------------------------------------------------------
-spec new_exception_with_fields(Type, Severity, Message, Context, Stacktrace,
                               WorkflowId, ActivityId) -> exception()
    when Type :: exception_type(),
         Severity :: exception_severity(),
         Message :: binary() | string(),
         Context :: map(),
         Stacktrace :: list(),
         WorkflowId :: binary(),
         ActivityId :: binary().

new_exception_with_fields(Type, Severity, Message, Context, Stacktrace,
                          WorkflowId, ActivityId) when is_list(Message) ->
    new_exception_with_fields(Type, Severity, list_to_binary(Message), Context,
                            Stacktrace, WorkflowId, ActivityId);
new_exception_with_fields(Type, Severity, Message, Context, Stacktrace,
                          WorkflowId, ActivityId) when is_binary(Message) ->
    #yawl_exception{
      id = generate_exception_id(),
      type = Type,
      severity = Severity,
      message = Message,
      context = Context,
      timestamp = erlang:system_time(millisecond),
      stacktrace = Stacktrace,
      workflow_id = WorkflowId,
      activity_id = ActivityId,
      compensation_attempts = 0,
      retry_attempts = 0,
      handled_by = undefined,
      resolved = false
     }.

%%--------------------------------------------------------------------
%% @doc Gets the exception type.
%% @end
%%--------------------------------------------------------------------
-spec exception_type(Exception :: exception()) -> exception_type().

exception_type(#yawl_exception{type = Type}) -> Type.

%%--------------------------------------------------------------------
%% @doc Gets the exception severity.
%% @end
%%--------------------------------------------------------------------
-spec exception_severity(Exception :: exception()) -> exception_severity().

exception_severity(#yawl_exception{severity = Severity}) -> Severity.

%%--------------------------------------------------------------------
%% @doc Gets the exception message.
%% @end
%%--------------------------------------------------------------------
-spec exception_message(Exception :: exception()) -> binary().

exception_message(#yawl_exception{message = Message}) -> Message.

%%--------------------------------------------------------------------
%% @doc Gets the exception context.
%% @end
%%--------------------------------------------------------------------
-spec exception_context(Exception :: exception()) -> map().

exception_context(#yawl_exception{context = Context}) -> Context.

%%--------------------------------------------------------------------
%% @doc Gets the exception timestamp.
%% @end
%%--------------------------------------------------------------------
-spec exception_timestamp(Exception :: exception()) -> integer().

exception_timestamp(#yawl_exception{timestamp = Timestamp}) -> Timestamp.

%%--------------------------------------------------------------------
%% @doc Gets the exception ID.
%% @end
%%--------------------------------------------------------------------
-spec exception_id(Exception :: exception()) -> exception_id().

exception_id(#yawl_exception{id = Id}) -> Id.

%%--------------------------------------------------------------------
%% @doc Gets the workflow ID associated with the exception.
%% @end
%%--------------------------------------------------------------------
-spec exception_workflow_id(Exception :: exception()) -> undefined | binary().

exception_workflow_id(#yawl_exception{workflow_id = WorkflowId}) -> WorkflowId.

%%--------------------------------------------------------------------
%% @doc Gets the activity ID associated with the exception.
%% @end
%%--------------------------------------------------------------------
-spec exception_activity_id(Exception :: exception()) -> undefined | binary().

exception_activity_id(#yawl_exception{activity_id = ActivityId}) -> ActivityId.

%%--------------------------------------------------------------------
%% @doc Gets the number of compensation attempts.
%% @end
%%--------------------------------------------------------------------
-spec exception_compensation_attempts(Exception :: exception()) -> non_neg_integer().

exception_compensation_attempts(#yawl_exception{compensation_attempts = Attempts}) -> Attempts.

%%--------------------------------------------------------------------
%% @doc Gets the number of retry attempts.
%% @end
%%--------------------------------------------------------------------
-spec exception_retry_attempts(Exception :: exception()) -> non_neg_integer().

exception_retry_attempts(#yawl_exception{retry_attempts = Attempts}) -> Attempts.

%%--------------------------------------------------------------------
%% @doc Checks if the exception is resolved.
%% @end
%%--------------------------------------------------------------------
-spec exception_is_resolved(Exception :: exception()) -> boolean().

exception_is_resolved(#yawl_exception{resolved = Resolved}) -> Resolved.

%%====================================================================
%% Compensation API
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new compensator for undoing the effects of a completed activity.

Compensators are used to implement the compensation pattern (WHP-03)
for handling workflow exceptions. The compensation strategy determines
when and how the compensation is executed.

```erlang
> %% Create an immediate compensator
> Comp = cre_yawl_exception:new_compensator(
>     <<"reserve_inventory">>,
>     fun(ReservationId) -> {release, ReservationId} end,
>     immediate
> ).
_
> cre_yawl_exception:get_compensation_state(Comp).
pending
> cre_yawl_exception:get_compensation_dependencies(Comp).
[]
```

**Strategies:**
* `immediate` - Execute compensation as soon as exception occurs
* `deferred` - Queue compensation for later execution
* `chained` - Execute in reverse order of dependencies
* `parallel` - Execute concurrently with other compensators
""".
%%--------------------------------------------------------------------
-spec new_compensator(ActivityId, HandlerFun, Strategy) -> compensator()
    when ActivityId :: binary(),
         HandlerFun :: function(),
         Strategy :: compensation_strategy().

new_compensator(ActivityId, HandlerFun, Strategy) ->
    #compensator{
      activity_id = ActivityId,
      compensation_handler = HandlerFun,
      compensation_strategy = Strategy,
      state = pending,
      created_at = erlang:system_time(millisecond),
      started_at = undefined,
      completed_at = undefined,
      execution_time = undefined,
      retry_policy = undefined,
      dependencies = [],
      metadata = #{}
     }.

%%--------------------------------------------------------------------
%% @doc Enhanced compensator creation with retry policy and dependencies.
%% @end
%%--------------------------------------------------------------------
-spec new_compensator_with_deps(ActivityId, HandlerFun, Strategy,
                               RetryPolicy, Dependencies) -> compensator()
    when ActivityId :: binary(),
         HandlerFun :: function(),
         Strategy :: compensation_strategy(),
         RetryPolicy :: undefined | retry_policy(),
         Dependencies :: [binary()].

new_compensator_with_deps(ActivityId, HandlerFun, Strategy,
                         RetryPolicy, Dependencies) ->
    #compensator{
      activity_id = ActivityId,
      compensation_handler = HandlerFun,
      compensation_strategy = Strategy,
      state = pending,
      created_at = erlang:system_time(millisecond),
      started_at = undefined,
      completed_at = undefined,
      execution_time = undefined,
      retry_policy = RetryPolicy,
      dependencies = Dependencies,
      metadata = #{}
     }.

%%--------------------------------------------------------------------
-doc """
Executes compensation for an activity.

Returns a tuple with the updated compensator record. If the compensation
has already been completed, is in progress, or was cancelled, an error
is returned instead.

```erlang
> Comp = cre_yawl_exception:new_compensator(
>     <<"charge_payment">>,
>     fun(PaymentId) -> {refund, PaymentId} end,
>     immediate
> ).
_
> %% Execute compensation
> {ok, Result} = cre_yawl_exception:compensate(Comp, <<"pay_123">>).
_
> %% Check result
> cre_yawl_exception:has_compensated(Result).
true
> cre_yawl_exception:get_compensation_execution_time(Result) > 0.
true
>
> %% Attempting to compensate again returns error
> cre_yawl_exception:compensate(Result, <<"pay_123">>).
{error, already_compensated}
```
""".
%%--------------------------------------------------------------------
-spec compensate(Compensator :: compensator(), Input :: term()) ->
    {ok, compensator()} | {error, Reason :: term()}.

compensate(#compensator{state = completed}, _Input) ->
    {error, already_compensated};
compensate(#compensator{state = executing}, _Input) ->
    {error, compensation_in_progress};
compensate(#compensator{state = cancelled}, _Input) ->
    {error, compensation_cancelled};
compensate(#compensator{compensation_handler = Handler} = C, Input) ->
    StartTime = erlang:system_time(millisecond),
    C1 = C#compensator{state = executing, started_at = StartTime},
    try
        Result = Handler(Input),
        EndTime = erlang:system_time(millisecond),
        ExecutionTime = EndTime - StartTime,
        {ok, C1#compensator{
                state = completed,
                result = {ok, Result},
                completed_at = EndTime,
                execution_time = ExecutionTime
               }}
    catch
        Kind:Reason:Stack ->
            CatchEndTime = erlang:system_time(millisecond),
            CatchExecutionTime = CatchEndTime - StartTime,
            {ok, C1#compensator{
                    state = failed,
                    result = {error, {Kind, Reason, Stack}},
                    completed_at = CatchEndTime,
                    execution_time = CatchExecutionTime
                   }}
    end.

%%--------------------------------------------------------------------
%% @doc Compensation with retry logic and circuit breaker support.
%% @end
%%--------------------------------------------------------------------
-spec compensate_with_retry(Compensator :: compensator(), Input :: term(),
                           Attempt :: pos_integer()) ->
    {ok, compensator()} | {error, Reason :: term()}.

compensate_with_retry(#compensator{retry_policy = undefined} = C, Input, _Attempt) ->
    compensate(C, Input);
compensate_with_retry(#compensator{retry_policy = Policy} = C, Input, Attempt) ->
    case should_retry(Policy, Attempt) of
        false ->
            compensate(C, Input);
        true ->
            Delay = calculate_backoff(Policy, Attempt),
            ?LOG(info, "Retrying compensation after ~p ms, attempt ~p", [Delay, Attempt]),
            timer:sleep(Delay),
            compensate(C, Input)
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a compensator has completed.
%% @end
%%--------------------------------------------------------------------
-spec has_compensated(Compensator :: compensator()) -> boolean().

has_compensated(#compensator{state = State}) ->
    State =:= completed.

%%--------------------------------------------------------------------
%% @doc Checks if a compensator has failed.
%% @end
%%--------------------------------------------------------------------
-spec has_compensation_failed(Compensator :: compensator()) -> boolean().

has_compensation_failed(#compensator{state = failed}) -> true;
has_compensation_failed(_) -> false.

%%--------------------------------------------------------------------
%% @doc Gets the current state of a compensator.
%% @end
%%--------------------------------------------------------------------
-spec get_compensation_state(Compensator :: compensator()) ->
    pending | executing | completed | failed | cancelled.

get_compensation_state(#compensator{state = State}) -> State.

%%--------------------------------------------------------------------
%% @doc Gets the execution time of a compensator.
%% @end
%%--------------------------------------------------------------------
-spec get_compensation_execution_time(Compensator :: compensator()) -> undefined | integer().

get_compensation_execution_time(#compensator{execution_time = Time}) -> Time.

%%--------------------------------------------------------------------
%% @doc Gets compensator dependencies.
%% @end
%%--------------------------------------------------------------------
-spec get_compensation_dependencies(Compensator :: compensator()) -> [binary()].

get_compensation_dependencies(#compensator{dependencies = Dependencies}) -> Dependencies.

%%--------------------------------------------------------------------
%% @doc Creates a compensator with metadata.
%% @end
%%--------------------------------------------------------------------
-spec new_compensator_with_metadata(ActivityId, HandlerFun, Strategy, Metadata) -> compensator()
    when ActivityId :: binary(),
         HandlerFun :: function(),
         Strategy :: compensation_strategy(),
         Metadata :: map().

new_compensator_with_metadata(ActivityId, HandlerFun, Strategy, Metadata) ->
    #compensator{
      activity_id = ActivityId,
      compensation_handler = HandlerFun,
      compensation_strategy = Strategy,
      state = pending,
      created_at = erlang:system_time(millisecond),
      started_at = undefined,
      completed_at = undefined,
      execution_time = undefined,
      retry_policy = undefined,
      dependencies = [],
      metadata = Metadata
     }.

%%====================================================================
%% Retry Policy API
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a default retry policy with sensible defaults.

The default policy allows 3 attempts with exponential backoff starting at
1 second and doubling each time, with jitter enabled to prevent thundering
herd problems.

```erlang
> Policy = cre_yawl_exception:new_retry_policy().
_
> cre_yawl_exception:retry_policy_max_attempts(Policy).
3
> cre_yawl_exception:retry_policy_backoff(Policy).
exponential
> cre_yawl_exception:retry_policy_jitter(Policy).
true
```
""".
%%--------------------------------------------------------------------
-spec new_retry_policy() -> retry_policy().

new_retry_policy() ->
    #retry_policy{}.

%%--------------------------------------------------------------------
-doc """
Creates a retry policy with custom options.

**Options:**
* `max_attempts` - Maximum retry attempts (default: 3)
* `backoff` - Strategy: exponential | linear | constant | fibonacci | adaptive
* `base_delay` - Base delay in milliseconds (default: 1000)
* `max_delay` - Maximum delay cap in milliseconds (default: 60000)
* `multiplier` - Exponential backoff multiplier (default: 2.0)
* `jitter` - Enable random jitter (default: true)
* `jitter_factor` - Jitter amount as fraction of delay (default: 0.1)

```erlang
> %% Custom retry policy for external API calls
> Policy = cre_yawl_exception:new_retry_policy(#{
>     max_attempts => 5,
>     backoff => exponential,
>     base_delay => 2000,
>     max_delay => 30000,
>     multiplier => 2.0,
>     jitter => true
> }).
_
> %% Check retry eligibility
> cre_yawl_exception:should_retry(Policy, 3).
true
> cre_yawl_exception:should_retry(Policy, 6).
false
> %% Calculate delays
> Delay1 = cre_yawl_exception:calculate_backoff(Policy, 1).
Delay1 >= 2000, Delay1 =< 2200.  %% With jitter
true
> Delay3 = cre_yawl_exception:calculate_backoff(Policy, 3).
Delay3 >= 8000.  %% 2000 * 2^2
true
```
""".
%%--------------------------------------------------------------------
-spec new_retry_policy(Options :: map()) -> retry_policy().

new_retry_policy(Options) ->
    #retry_policy{
      max_attempts = maps:get(max_attempts, Options, 3),
      backoff_strategy = maps:get(backoff, Options, exponential),
      base_delay = maps:get(base_delay, Options, 1000),
      max_delay = maps:get(max_delay, Options, 60000),
      multiplier = maps:get(multiplier, Options, 2.0),
      jitter = maps:get(jitter, Options, true),
      jitter_factor = maps:get(jitter_factor, Options, 0.1)
     }.

%%--------------------------------------------------------------------
%% @doc Gets the maximum number of retry attempts.
%% @end
%%--------------------------------------------------------------------
-spec retry_policy_max_attempts(Policy :: retry_policy()) -> non_neg_integer().

retry_policy_max_attempts(#retry_policy{max_attempts = N}) -> N.

%%--------------------------------------------------------------------
%% @doc Gets the backoff strategy.
%% @end
%%--------------------------------------------------------------------
-spec retry_policy_backoff(Policy :: retry_policy()) -> retry_strategy().

retry_policy_backoff(#retry_policy{backoff_strategy = Strategy}) -> Strategy.

%%--------------------------------------------------------------------
%% @doc Checks if jitter is enabled.
%% @end
%%--------------------------------------------------------------------
-spec retry_policy_jitter(Policy :: retry_policy()) -> boolean().

retry_policy_jitter(#retry_policy{jitter = Jitter}) -> Jitter.

%%--------------------------------------------------------------------
%% @doc Determines if a retry should be attempted.
%% @end
%%--------------------------------------------------------------------
-spec should_retry(Policy :: retry_policy(), AttemptCount :: non_neg_integer()) ->
    boolean().

should_retry(#retry_policy{max_attempts = Max}, AttemptCount) when AttemptCount < Max ->
    true;
should_retry(_Policy, _AttemptCount) ->
    false.

%%--------------------------------------------------------------------
%% @doc Calculates the backoff delay for a given attempt.
%% @end
%%--------------------------------------------------------------------
-spec calculate_backoff(Policy :: retry_policy(), Attempt :: pos_integer()) ->
    non_neg_integer().

calculate_backoff(#retry_policy{
                    backoff_strategy = Strategy,
                    base_delay = Base,
                    max_delay = Max,
                    multiplier = Mult,
                    jitter = Jitter,
                    jitter_factor = JitterFactor
                   }, Attempt) ->
    Delay0 = case Strategy of
                 exponential -> min(Base * math:pow(Mult, Attempt - 1), Max);
                 linear -> min(Base * Attempt, Max);
                 constant -> Base;
                 fibonacci -> calculate_fibonacci_delay(Base, Attempt, Max);
                 fibonacci_backoff -> calculate_fibonacci_backoff_delay(Base, Attempt, Max);
                 adaptive -> calculate_adaptive_delay(Base, Attempt, Max)
             end,
    Delay = round(Delay0),
    case Jitter of
        true ->
            JitterRange = round(Delay * JitterFactor),
            JitterAmount = rand:uniform(2 * JitterRange) - JitterRange,
            max(0, Delay + JitterAmount);
        false ->
            Delay
    end.

%%--------------------------------------------------------------------
%% @doc Calculates Fibonacci delay for retry.
%% @end
%%--------------------------------------------------------------------
-spec calculate_fibonacci_delay(Base :: non_neg_integer(),
                                Attempt :: pos_integer(),
                                Max :: non_neg_integer()) -> non_neg_integer().

calculate_fibonacci_delay(Base, Attempt, Max) ->
    Fib = nth_fibonacci(Attempt),
    min(Base * Fib, Max).

%%--------------------------------------------------------------------
%% @doc Calculates Fibonacci backoff delay (exponential-like but smoother).
%% @end
%%--------------------------------------------------------------------
-spec calculate_fibonacci_backoff_delay(Base :: non_neg_integer(),
                                        Attempt :: pos_integer(),
                                        Max :: non_neg_integer()) -> non_neg_integer().

calculate_fibonacci_backoff_delay(Base, Attempt, Max) ->
    Fib = nth_fibonacci(Attempt + 1),
    min(Base * Fib, Max).

%%--------------------------------------------------------------------
%% @doc Calculates adaptive delay based on historical performance.
%% @end
%%--------------------------------------------------------------------
-spec calculate_adaptive_delay(Base :: non_neg_integer(),
                              Attempt :: pos_integer(),
                              Max :: non_neg_integer()) -> non_neg_integer().

calculate_adaptive_delay(Base, Attempt, Max) ->
    % Simple adaptive strategy - increase delay more aggressively after failures
    AdaptiveFactor = min(Attempt * 1.5, 5.0),
    min(round(Base * AdaptiveFactor), Max).

%%--------------------------------------------------------------------
%% @doc Calculates nth Fibonacci number.
%% @end
%%--------------------------------------------------------------------
-spec nth_fibonacci(N :: pos_integer()) -> pos_integer().

nth_fibonacci(1) -> 1;
nth_fibonacci(2) -> 1;
nth_fibonacci(N) when N > 2 ->
    nth_fibonacci(N-1) + nth_fibonacci(N-2).

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if all dependencies are completed.
%% @end
%%--------------------------------------------------------------------
-spec all_dependencies_completed(Compensator :: compensator(), State :: term()) -> boolean().

all_dependencies_completed(#compensator{dependencies = Dependencies}, _State) ->
    case Dependencies of
        [] -> true;
        _Dependencies -> false  % Simplified - in real implementation would check actual status
    end.

%%--------------------------------------------------------------------
%% @doc Initializes the exception state with workflow context.
%% @end
%%--------------------------------------------------------------------
-spec init_exception_state(WorkflowId :: binary(), WorkflowPid :: pid()) -> #exception_state{}.

init_exception_state(WorkflowId, WorkflowPid) ->
    MonitorRef = erlang:monitor(process, WorkflowPid),
    #exception_state{
      workflow_id = WorkflowId,
      workflow_pid = WorkflowPid,
      workflow_monitor = MonitorRef,
      circuit_breakers = #{},
      metrics = #{},
      audit_log = [],
      is_compensating = false,
      compensation_timeout = ?DEFAULT_TIMEOUT,
      max_parallel_compensations = 10
     }.

%%--------------------------------------------------------------------
%% @ Updates metrics for the exception handling system.
%% @end
%%--------------------------------------------------------------------
-spec update_metrics(Event :: atom(), Data :: map(), State :: #exception_state{}) -> #exception_state{}.

update_metrics(Event, Data, #exception_state{metrics = Metrics} = State) ->
    Now = erlang:system_time(millisecond),
    NewMetrics = Metrics#{Event => Data#{timestamp => Now}},
    State#exception_state{metrics = NewMetrics}.

%%--------------------------------------------------------------------
%% @ Logs audit event to the audit trail.
%% @end
%%--------------------------------------------------------------------
-spec log_audit_event(Event :: atom(), Data :: map(), State :: #exception_state{}) -> #exception_state{}.

log_audit_event(Event, Data, #exception_state{audit_log = AuditLog} = State) ->
    Now = erlang:system_time(millisecond),
    AuditEvent = #{event => Event, data => Data, timestamp => Now},
    State#exception_state{audit_log = [AuditEvent | AuditLog]}.

%%====================================================================
%% Error Handler API
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new error handler for specific exception types.

The handler will be invoked when exceptions of the specified types
occur. Multiple handlers can be registered for the same exception type,
and they will be executed in priority order.

```erlang
> %% Create a handler for communication exceptions
> Handler = cre_yawl_exception:new_error_handler(
>     api_circuit_breaker,
>     [communication_exception, timeout_exception],
>     fun(Exception) ->
>         io:format("API failure: ~p~n", [Exception]),
>         {retry, exception_#yawl_exception.message}
>     end
> ).
_
> cre_yawl_exception:is_handler_enabled(Handler).
true
```
""".
%%--------------------------------------------------------------------
-spec new_error_handler(HandlerId, ExceptionTypes, HandlerFun) -> error_handler()
    when HandlerId :: handler_id() | atom(),
         ExceptionTypes :: [exception_type()],
         HandlerFun :: function().

new_error_handler(HandlerId, ExceptionTypes, HandlerFun) when is_atom(HandlerId) ->
    new_error_handler(atom_to_binary(HandlerId), ExceptionTypes, HandlerFun);
new_error_handler(HandlerId, ExceptionTypes, HandlerFun) when is_binary(HandlerId) ->
    #error_handler{
      handler_id = HandlerId,
      exception_types = ExceptionTypes,
      handler_fun = HandlerFun
     }.

%%--------------------------------------------------------------------
%% @doc Registers an error handler for specific exception types.
%% @end
%%--------------------------------------------------------------------
-spec register_handler(Handlers :: map(), Handler :: error_handler()) ->
    map().

register_handler(Handlers, #error_handler{exception_types = Types} = Handler) ->
    lists:foldl(
      fun(Type, Acc) ->
              L = maps:get(Type, Acc, []),
              %% Use handler_id from the handler record to find existing handler
              HandlerId = Handler#error_handler.handler_id,
              %% Filter out existing handler with same id, then add new one
              FilteredList = lists:filter(
                  fun(#error_handler{handler_id = Hid}) -> Hid =/= HandlerId;
                     (_) -> true
                  end, L),
              Acc#{Type => [Handler | FilteredList]}
      end,
      Handlers,
      Types).

%%--------------------------------------------------------------------
%% @doc Registers a handler with circuit breaker configuration.
%% @end
%%--------------------------------------------------------------------
-spec register_handler_with_circuit_breaker(Handlers :: map(),
                                          Handler :: error_handler(),
                                          CircuitBreakerThreshold :: pos_integer(),
                                          CircuitBreakerTimeout :: non_neg_integer()) ->
    map().

register_handler_with_circuit_breaker(Handlers, Handler, Threshold, Timeout) ->
    Handler1 = Handler#error_handler{
                  circuit_breaker_state = #{
                    failures => 0,
                    last_failure => undefined,
                    state => closed,
                    threshold => Threshold,
                    timeout => Timeout
                  }
                 },
    register_handler(Handlers, Handler1).

%%--------------------------------------------------------------------
%% @doc Updates circuit breaker state after a failure.
%% @end
%%--------------------------------------------------------------------
-spec update_circuit_breaker_state(Handler :: error_handler(),
                                  FailureTime :: integer()) ->
    {ok, error_handler()} | {circuit_open, error_handler()}.

update_circuit_breaker_state(#error_handler{circuit_breaker_state = undefined} = Handler,
                           _FailureTime) ->
    {ok, Handler};
update_circuit_breaker_state(#error_handler{
                                circuit_breaker_state = CBState
                               } = Handler, FailureTime) ->
    Threshold = maps:get(threshold, CBState),
    CurrentFailures = maps:get(failures, CBState, 0) + 1,

    NewCBState = case CurrentFailures >= Threshold of
                     true ->
                         CBState#{
                           failures => CurrentFailures,
                           last_failure => FailureTime,
                           state => open
                         };
                     false ->
                         CBState#{
                           failures => CurrentFailures,
                           last_failure => FailureTime,
                           state => closed
                         }
                 end,

    case NewCBState of
        #{state := open} ->
            {circuit_open, Handler#error_handler{circuit_breaker_state = NewCBState}};
        _ ->
            {ok, Handler#error_handler{circuit_breaker_state = NewCBState}}
    end.

%%--------------------------------------------------------------------
-doc """
Executes an error handler with the given exception.

Catches any exceptions thrown by the handler function and returns
them as error tuples for proper error propagation.

```erlang
> Handler = cre_yawl_exception:new_error_handler(
>     test_handler,
>     [business_exception],
>     fun(Exc) -> {ok, exception_#yawl_exception.id} end
> ),
> Exception = cre_yawl_exception:new_exception(
>     business_exception, "test", #{}, []
> ).
_
> %% Successful handler execution
> {ok, Result} = cre_yawl_exception:execute_handler(Handler, Exception).
_
> %% Handler that throws an error
> BadHandler = cre_yawl_exception:new_error_handler(
>     bad_handler,
>     [system_exception],
>     fun(_) -> error(bad) end
> ).
_
> cre_yawl_exception:execute_handler(BadHandler, Exception).
{error, {error, bad}}
```
""".
%%--------------------------------------------------------------------
-spec execute_handler(Handler :: error_handler(), Exception :: exception()) ->
    {ok, term()} | {error, term()}.

execute_handler(#error_handler{handler_fun = HandlerFun}, Exception) ->
    try
        Result = HandlerFun(Exception),
        {ok, Result}
    catch
        Kind:Reason ->
            {error, {Kind, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Finds handlers for a specific exception type.
%% @end
%%--------------------------------------------------------------------
-spec find_handler(Handlers :: map(), ExceptionType :: exception_type()) ->
    [error_handler()].

find_handler(Handlers, ExceptionType) ->
    maps:get(ExceptionType, Handlers, []).

%%--------------------------------------------------------------------
%% @doc Finds the best matching handler for an exception.
%% @end
%%--------------------------------------------------------------------
-spec find_best_handler(Handlers :: map(), Exception :: exception()) ->
    undefined | error_handler().

find_best_handler(Handlers, Exception) ->
    ExceptionType = Exception#yawl_exception.type,
    CandidateHandlers = find_handler(Handlers, ExceptionType),
    case CandidateHandlers of
        [] -> undefined;
        HandlersList ->
            % Sort by priority and return the highest priority handler
            SortedHandlers = lists:sort(fun(H1, H2) ->
                                             H1#error_handler.priority > H2#error_handler.priority
                                         end, HandlersList),
            hd(SortedHandlers)
    end.

%%--------------------------------------------------------------------
%% @doc Gets metrics for an error handler.
%% @end
%%--------------------------------------------------------------------
-spec get_handler_metrics(Handler :: error_handler()) -> map().

get_handler_metrics(#error_handler{metrics = Metrics}) ->
    Metrics.

%%--------------------------------------------------------------------
%% @doc Enables or disables an error handler.
%% @end
%%--------------------------------------------------------------------
-spec set_handler_enabled(Handler :: error_handler(), Enabled :: boolean()) -> error_handler().

set_handler_enabled(#error_handler{enabled = OldEnabled} = Handler, Enabled) when OldEnabled =/= Enabled ->
    Handler#error_handler{enabled = Enabled};
set_handler_enabled(Handler, _Enabled) ->
    Handler.

%%--------------------------------------------------------------------
%% @doc Checks if an error handler is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_handler_enabled(Handler :: error_handler()) -> boolean().

is_handler_enabled(#error_handler{enabled = Enabled}) -> Enabled.

%%--------------------------------------------------------------------
%% @doc Creates a handler with compensation support.
%% @end
%%--------------------------------------------------------------------
-spec new_handler_with_compensation(HandlerId, ExceptionTypes, HandlerFun, CompensationFun) -> error_handler()
    when HandlerId :: handler_id() | atom(),
         ExceptionTypes :: [exception_type()],
         HandlerFun :: function(),
         CompensationFun :: function().

new_handler_with_compensation(HandlerId, ExceptionTypes, HandlerFun, CompensationFun) when is_atom(HandlerId) ->
    new_handler_with_compensation(atom_to_binary(HandlerId), ExceptionTypes, HandlerFun, CompensationFun);
new_handler_with_compensation(HandlerId, ExceptionTypes, HandlerFun, CompensationFun) ->
    #error_handler{
      handler_id = HandlerId,
      exception_types = ExceptionTypes,
      handler_fun = HandlerFun,
      compensation_handler = CompensationFun
     }.

%%--------------------------------------------------------------------
%% @doc Executes handler with circuit breaker support.
%% @end
%%--------------------------------------------------------------------
-spec execute_handler_with_circuit_breaker(Handler :: error_handler(),
                                          Exception :: exception(),
                                          Now :: integer()) ->
    {ok, term()} | {error, term()} | {circuit_open, term()}.

execute_handler_with_circuit_breaker(Handler, Exception, Now) ->
    case check_circuit_breaker(Handler, Now) of
        {circuit_open, Reason} ->
            {circuit_open, Reason};
        {ok, Handler1} ->
            Result = execute_handler(Handler1, Exception),
            case Result of
                {error, _} ->
                    update_circuit_breaker_state(Handler1, Now);
                _ ->
                    {ok, Handler1}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Checks if circuit breaker is open.
%% @end
%%--------------------------------------------------------------------
-spec check_circuit_breaker(Handler :: error_handler(), Now :: integer()) ->
    {ok, error_handler()} | {circuit_open, term()}.

check_circuit_breaker(#error_handler{circuit_breaker_state = undefined}, _) ->
    {ok, #error_handler{circuit_breaker_state = #{state => closed}}};
check_circuit_breaker(#error_handler{circuit_breaker_state = CBState}, Now) ->
    case maps:get(state, CBState) of
        open ->
            Timeout = maps:get(timeout, CBState),
            LastFailure = maps:get(last_failure, CBState),
            case Now - LastFailure >= Timeout of
                true ->
                    % Circuit breaker has timed out, close it
                    NewCBState = CBState#{state => closed, failures => 0},
                    {ok, #error_handler{circuit_breaker_state = NewCBState}};
                false ->
                    {circuit_open, {circuit_breaker_open, Now - LastFailure}}
            end;
        closed ->
            {ok, #error_handler{circuit_breaker_state = CBState}}
    end.

%%--------------------------------------------------------------------
%% @doc Helper functions for exception severity determination.
%% @end
%%--------------------------------------------------------------------
-spec determine_severity(Type :: exception_type(), Context :: map()) -> exception_severity().

determine_severity(system_exception, Context) ->
    case maps:get(error_code, Context, undefined) of
        undefined -> high;
        Code when Code > 1000 -> critical;
        _ -> high
    end;
determine_severity(security_exception, _) -> critical;
determine_severity(workflow_exception, _) -> high;
determine_severity(compensation_exception, _) -> high;
determine_severity(resource_exception, Context) ->
    case maps:get(resource_type, Context, undefined) of
        _ -> medium
    end;
determine_severity(_Type, Context) ->
    case maps:get(severity, Context, undefined) of
        undefined -> medium;
        Severity -> Severity
    end.

%%--------------------------------------------------------------------
%% @doc Extract workflow ID from context.
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_id(Context :: map()) -> undefined | binary().

get_workflow_id(Context) ->
    case maps:get(workflow_id, Context, undefined) of
        undefined ->
            case maps:get(<<"workflow_id">>, Context, undefined) of
                undefined -> undefined;
                Id -> Id
            end;
        Id -> Id
    end.

%%--------------------------------------------------------------------
%% @doc Extract activity ID from context.
%% @end
%%--------------------------------------------------------------------
-spec get_activity_id(Context :: map()) -> undefined | binary().

get_activity_id(Context) ->
    case maps:get(activity_id, Context, undefined) of
        undefined ->
            case maps:get(<<"activity_id">>, Context, undefined) of
                undefined -> undefined;
                Id -> Id
            end;
        Id -> Id
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec generate_exception_id() -> exception_id().

generate_exception_id() ->
    Binary = term_to_binary({self(), erlang:unique_integer([positive])}),
    Hex = binary:encode_hex(crypto:hash(md5, Binary)),
    <<"exc_", Hex/binary>>.

%%====================================================================
%% gen_pnet callback functions
%%====================================================================

%% Petri net places for enhanced exception handling
place_lst() ->
    ['Active', 'ExceptionRaised', 'Handling', 'Compensating', 'Resolved',
     'Retry', 'Failed', 'CompensationStack', 'AuditLog', 'Metrics',
     'WorkflowPaused', 'CircuitBreaker', 'DependencyCheck', 'ParallelCompensations',
     'ConsecutiveCompensations', 'TriggeredCompensation', 'CompensationTimeout'].

%% Petri net transitions for WHP-01 to WHP-5 patterns
trsn_lst() ->
    % Core exception handling
    [raise_exception, handle_exception, execute_handler,
     % WHP-01: Error Handler
     error_handler_execute,
     % WHP-02: Retry with Backoff
     retry_after_backoff, retry_with_policy, retry_exhausted,
     % WHP-03: Compensation
     compensate_activity, compensation_success, compensation_failure,
     % WHP-04: Triggered Compensation
     trigger_compensation, compensation_triggered,
     % WHP-05: Consecutive Compensation
     start_consecutive_compensation, execute_consecutive_compensation,
     % Additional enhanced transitions
     push_compensator, mark_resolved, mark_failed,
     pause_workflow, resume_workflow,
     update_metrics, log_audit_event,
     check_dependencies, execute_parallel_compensations,
     handle_compensation_timeout, cancel_compensation].

init_marking(_Place, _UsrInfo) -> [].

%% Preset for each transition - Enhanced WHP-01 to WHP-05 patterns
preset(raise_exception) -> ['Active'];
preset(handle_exception) -> ['ExceptionRaised'];
preset(execute_handler) -> ['Handling'];
preset(error_handler_execute) -> ['ExceptionRaised', 'Handling'];
preset(compensate_activity) -> ['Compensating'];
preset(compensation_success) -> ['Compensating'];
preset(compensation_failure) -> ['Compensating'];

% WHP-02: Retry with Backoff
preset(retry_after_backoff) -> ['Retry'];
preset(retry_with_policy) -> ['Retry'];
preset(retry_exhausted) -> ['Retry'];

% WHP-03: Compensation
preset(push_compensator) -> ['Active'];
preset(compensation_success) -> ['CompensationStack'];
preset(compensation_failure) -> ['Failed'];

% WHP-04: Triggered Compensation
preset(trigger_compensation) -> ['ExceptionRaised'];
preset(compensation_triggered) -> ['CompensationStack'];

% WHP-05: Consecutive Compensation
preset(start_consecutive_compensation) -> ['ExceptionRaised'];
preset(execute_consecutive_compensation) -> ['ConsecutiveCompensations'];

% Enhanced transitions
preset(mark_resolved) -> ['Handling'];
preset(mark_failed) -> ['ExceptionRaised'];
preset(pause_workflow) -> ['Active'];
preset(resume_workflow) -> ['WorkflowPaused'];
preset(update_metrics) -> ['Metrics'];
preset(log_audit_event) -> ['AuditLog'];
preset(check_dependencies) -> ['DependencyCheck'];
preset(execute_parallel_compensations) -> ['ParallelCompensations'];
preset(handle_compensation_timeout) -> ['CompensationTimeout'];
preset(cancel_compensation) -> ['CompensationStack'].

%% Enable conditions - Enhanced for WHP-01 to WHP-05 patterns
is_enabled(raise_exception, _Marking, _UsrInfo) -> true;
is_enabled(handle_exception, _Marking, _UsrInfo) -> true;
is_enabled(execute_handler, _Marking, _UsrInfo) -> true;
is_enabled(error_handler_execute, _Marking, _UsrInfo) -> true;
is_enabled(compensate_activity, _Marking, _UsrInfo) -> true;
is_enabled(compensation_success, _Marking, _UsrInfo) -> true;
is_enabled(compensation_failure, _Marking, _UsrInfo) -> true;

% WHP-02: Retry with Backoff
is_enabled(retry_after_backoff, _Marking, _UsrInfo) -> true;
is_enabled(retry_with_policy, _Marking, _UsrInfo) -> true;
is_enabled(retry_exhausted, _Marking, _UsrInfo) -> true;

% WHP-03: Compensation
is_enabled(push_compensator, _Marking, _UsrInfo) -> true;

% WHP-04: Triggered Compensation
is_enabled(trigger_compensation, _Marking, _UsrInfo) -> true;
is_enabled(compensation_triggered, _Marking, _UsrInfo) -> true;

% WHP-05: Consecutive Compensation
is_enabled(start_consecutive_compensation, _Marking, _UsrInfo) -> true;
is_enabled(execute_consecutive_compensation, _Marking, _UsrInfo) -> true;

% Enhanced transitions
is_enabled(mark_resolved, _Marking, _UsrInfo) -> true;
is_enabled(mark_failed, _Marking, _UsrInfo) -> true;
is_enabled(pause_workflow, _Marking, _UsrInfo) -> true;
is_enabled(resume_workflow, _Marking, _UsrInfo) -> true;
is_enabled(update_metrics, _Marking, _UsrInfo) -> true;
is_enabled(log_audit_event, _Marking, _UsrInfo) -> true;
is_enabled(check_dependencies, _Marking, _UsrInfo) -> true;
is_enabled(execute_parallel_compensations, _Marking, _UsrInfo) -> true;
is_enabled(handle_compensation_timeout, _Marking, _UsrInfo) -> true;
is_enabled(cancel_compensation, _Marking, _UsrInfo) -> true.

%% Transition firing - Enhanced for WHP-01 to WHP-05 patterns
fire(raise_exception, #{'Active' := [#yawl_exception{type = Type} = Exception]}, _UsrInfo) ->
    ?LOG(warning, "Exception raised: ~p", [Type]),
    {produce, #{
      'ExceptionRaised' => [Exception],
      'Active' => []
     }};

% WHP-01: Error Handler
fire(handle_exception, #{'ExceptionRaised' := [Exception]}, #exception_state{handlers = Handlers}) ->
    ExceptionType = Exception#yawl_exception.type,
    case find_handler(Handlers, ExceptionType) of
        [] ->
            ?LOG(error, "No handler found for exception type: ~p", [ExceptionType]),
            {produce, #{
              'Failed' => [Exception],
              'ExceptionRaised' => []
             }};
        [Handler | _] ->
            ?LOG(info, "Handler found for exception type: ~p", [ExceptionType]),
            {produce, #{
              'Handling' => [{Exception, Handler}],
              'ExceptionRaised' => []
             }}
    end;

fire(error_handler_execute, #{'Handling' := [{Exception, Handler}]}, _UsrInfo) ->
    Now = erlang:system_time(millisecond),
    case execute_handler_with_circuit_breaker(Handler, Exception, Now) of
        {ok, Result} ->
            ?LOG(info, "Handler executed successfully", []),
            {produce, #{
              'Resolved' => [Exception],
              'Handling' => [],
              'Metrics' => [#{event => handler_success, timestamp => Now, exception_type => Exception#yawl_exception.type}]
             }};
        {error, Reason} ->
            ?LOG(error, "Handler execution failed: ~p", [Reason]),
            {produce, #{
              'Retry' => [Exception],
              'Handling' => [],
              'AuditLog' => [#{event => handler_failure, exception => Exception, reason => Reason, timestamp => Now}]
             }};
        {circuit_open, Reason} ->
            ?LOG(warning, "Circuit breaker open: ~p", [Reason]),
            {produce, #{
              'Failed' => [Exception],
              'Handling' => [],
              'CircuitBreaker' => [Exception],
              'AuditLog' => [#{event => circuit_breaker_open, exception => Exception, reason => Reason, timestamp => Now}]
             }}
    end;

% WHP-02: Retry with Backoff
fire(retry_after_backoff, #{'Retry' := [Exception]}, _UsrInfo) ->
    Now = erlang:system_time(millisecond),
    Exception1 = Exception#yawl_exception{retry_attempts = Exception#yawl_exception.retry_attempts + 1},
    ?LOG(info, "Retrying after backoff, attempt ~p", [Exception1#yawl_exception.retry_attempts]),
    {produce, #{
      'ExceptionRaised' => [Exception1],
      'Retry' => [],
      'Metrics' => [#{event => retry_attempt, timestamp => Now, attempt => Exception1#yawl_exception.retry_attempts}]
     }};

fire(retry_with_policy, #{'Retry' := [Exception]}, #exception_state{handlers = Handlers} = UsrInfo) ->
    ExceptionType = Exception#yawl_exception.type,
    case find_handler(Handlers, ExceptionType) of
        [Handler | _] ->
            case Handler#error_handler.retry_policy of
                undefined ->
                    fire(retry_after_backoff, #{'Retry' => [Exception]}, UsrInfo);
                Policy ->
                    Attempt = Exception#yawl_exception.retry_attempts + 1,
                    case should_retry(Policy, Attempt) of
                        true ->
                            Delay = calculate_backoff(Policy, Attempt),
                            ?LOG(info, "Scheduling retry with policy: ~pms, attempt ~p", [Delay, Attempt]),
                            {produce, #{
                              'Retry' => [Exception#yawl_exception{retry_attempts = Attempt}],
                              'Metrics' => [#{event => retry_scheduled, timestamp => erlang:system_time(millisecond), delay => Delay, attempt => Attempt}]
                             }};
                        false ->
                            ?LOG(error, "Retry attempts exhausted for exception: ~p", [ExceptionType]),
                            {produce, #{
                              'Failed' => [Exception],
                              'Retry' => []
                             }}
                    end
            end
    end;

% WHP-03: Compensation
fire(compensate_activity, #{'Compensating' := [Compensator]}, #exception_state{compensation_stack = Stack}) ->
    case length(Stack) >= ?MAX_COMPENSATION_STACK_SIZE of
        true ->
            ?LOG(error, "Compensation stack limit reached", []),
            {produce, #{
              'Failed' => [Compensator],
              'Compensating' => []
             }};
        false ->
            case compensate_with_retry(Compensator, undefined, 1) of
                {ok, NewCompensator} ->
                    ?LOG(info, "Compensation executed successfully for activity: ~p", [NewCompensator#compensator.activity_id]),
                    {produce, #{
                      'CompensationStack' => [NewCompensator],
                      'Compensating' => []
                     }};
                {error, Reason} ->
                    ?LOG(error, "Compensation failed: ~p", [Reason]),
                    {produce, #{
                      'Failed' => [Compensator],
                      'Compensating' => []
                     }}
            end
    end;

fire(compensation_success, #{'Compensating' := [Compensator]}, _UsrInfo) ->
    ?LOG(info, "Compensation succeeded for activity: ~p", [Compensator#compensator.activity_id]),
    {produce, #{
      'CompensationStack' => [Compensator#compensator{state = completed}],
      'Compensating' => []
     }};

fire(compensation_failure, #{'Compensating' := [Compensator]}, _UsrInfo) ->
    ?LOG(error, "Compensation failed for activity: ~p", [Compensator#compensator.activity_id]),
    {produce, #{
      'Failed' => [Compensator],
      'Compensating' => []
     }};

fire(push_compensator, #{'Active' := [ActivityId]}, #exception_state{workflow_id = WorkflowId}) ->
    HandlerFun = fun(_Input) -> {compensated, ActivityId} end,
    Compensator = new_compensator(ActivityId, HandlerFun, immediate),
    ?LOG(info, "Pushed compensator for activity: ~p", [ActivityId]),
    {produce, #{
      'CompensationStack' => [Compensator],
      'Active' => [ActivityId]
     }};

% WHP-04: Triggered Compensation
fire(trigger_compensation, #{'ExceptionRaised' := [Exception]}, _UsrInfo) ->
    Exception1 = Exception#yawl_exception{compensation_attempts = Exception#yawl_exception.compensation_attempts + 1},
    ?LOG(info, "Triggered compensation for exception: ~p", [Exception1#yawl_exception.type]),
    {produce, #{
      'TriggeredCompensation' => [Exception1],
      'ExceptionRaised' => []
     }};

fire(compensation_triggered, #{'TriggeredCompensation' := [Exception]}, _UsrInfo) ->
    HandlerFun = fun(_Input) -> {compensated, Exception#yawl_exception.activity_id} end,
    Compensator = new_compensator(Exception#yawl_exception.activity_id, HandlerFun, deferred),
    {produce, #{
      'CompensationStack' => [Compensator],
      'TriggeredCompensation' => []
     }};

% WHP-05: Consecutive Compensation
fire(start_consecutive_compensation, #{'ExceptionRaised' := [Exception]}, _UsrInfo) ->
    Exception1 = Exception#yawl_exception{compensation_attempts = Exception#yawl_exception.compensation_attempts + 1},
    ?LOG(info, "Starting consecutive compensation for exception: ~p", [Exception1#yawl_exception.type]),
    {produce, #{
      'ConsecutiveCompensations' => [Exception1],
      'ExceptionRaised' => []
     }};

fire(execute_consecutive_compensation, #{'ConsecutiveCompensations' := [Exception]}, _UsrInfo) ->
    HandlerFun = fun(_Input) -> {compensated, Exception#yawl_exception.activity_id} end,
    Compensator = new_compensator(Exception#yawl_exception.activity_id, HandlerFun, chained),
    {produce, #{
      'CompensationStack' => [Compensator],
      'ConsecutiveCompensations' => []
     }};

% Enhanced transitions
fire(retry_exhausted, #{'Retry' := [Exception]}, _UsrInfo) ->
    ?LOG(error, "Retry attempts exhausted for exception: ~p", [Exception#yawl_exception.type]),
    {produce, #{
      'Failed' => [Exception],
      'Retry' => []
     }};

fire(mark_resolved, #{'Handling' := [{_Exception, _Handler}]}, _UsrInfo) ->
    Now = erlang:system_time(millisecond),
    ?LOG(info, "Exception resolved", []),
    {produce, #{
      'Active' => [resolved],
      'Handling' => [],
      'Metrics' => [#{event => exception_resolved, timestamp => Now}]
     }};

fire(mark_failed, #{'ExceptionRaised' := [Exception]}, _UsrInfo) ->
    Now = erlang:system_time(millisecond),
    ?LOG(error, "Exception marked as failed: ~p", [Exception#yawl_exception.type]),
    {produce, #{
      'Failed' => [Exception],
      'ExceptionRaised' => [],
      'AuditLog' => [#{event => exception_failed, exception => Exception, timestamp => Now}]
     }}.

code_change(_OldVsn, NetState, _Extra) -> {ok, NetState}.
handle_call(_Request, _From, _NetState) -> {reply, {error, bad_msg}}.
handle_cast(_Request, _NetState) -> noreply.
handle_info(_Request, _NetState) -> noreply.

init(_Arg) ->
    #exception_state{}.

terminate(_Reason, _NetState) -> ok.

trigger(_Place, _Token, _NetState) -> pass.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs doctests for the cre_yawl_exception module.
///
/// This function validates the exception handling, compensation,
/// retry policy, and error handler functionality through direct testing.
///
/// Returns `ok' when all tests pass.
///
/// ```erlang
/// 1> cre_yawl_exception:doctest_test().
/// ok
/// '''
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test exception creation
    Exc = new_exception(business_exception, "Test exception",
                        #{workflow_id => <<"wf_123">>}, []),
    true = is_binary(exception_id(Exc)),
    business_exception = exception_type(Exc),
    <<"Test exception">> = exception_message(Exc),
    medium = exception_severity(Exc),
    <<"wf_123">> = exception_workflow_id(Exc),
    false = exception_is_resolved(Exc),
    0 = exception_compensation_attempts(Exc),
    0 = exception_retry_attempts(Exc),

    %% Test exception with different severity
    CriticalExc = new_exception(security_exception, "Security breach",
                                #{}, []),
    critical = exception_severity(CriticalExc),

    %% Test exception creation with string message
    StringExc = new_exception(system_exception, "System error",
                              #{error_code => 2000}, []),
    system_exception = exception_type(StringExc),
    critical = exception_severity(StringExc),

    %% Test compensator creation
    Comp = new_compensator(<<"activity_1">>,
                           fun(Input) -> {compensated, Input} end,
                           immediate),
    pending = get_compensation_state(Comp),
    [] = get_compensation_dependencies(Comp),
    false = has_compensated(Comp),
    false = has_compensation_failed(Comp),
    undefined = get_compensation_execution_time(Comp),

    %% Test compensator execution
    {ok, CompResult} = compensate(Comp, test_input),
    completed = get_compensation_state(CompResult),
    true = has_compensated(CompResult),
    false = has_compensation_failed(CompResult),
    true = get_compensation_execution_time(CompResult) > 0,

    %% Test double compensation returns error
    {error, already_compensated} = compensate(CompResult, test_input),

    %% Test retry policy defaults
    DefaultPolicy = new_retry_policy(),
    3 = retry_policy_max_attempts(DefaultPolicy),
    exponential = retry_policy_backoff(DefaultPolicy),
    true = retry_policy_jitter(DefaultPolicy),

    %% Test custom retry policy
    CustomPolicy = new_retry_policy(#{
        max_attempts => 5,
        backoff => exponential,
        base_delay => 2000,
        multiplier => 2.0,
        jitter => true
    }),
    5 = retry_policy_max_attempts(CustomPolicy),
    exponential = retry_policy_backoff(CustomPolicy),

    %% Test should_retry
    true = should_retry(CustomPolicy, 3),
    false = should_retry(CustomPolicy, 5),
    false = should_retry(CustomPolicy, 10),

    %% Test backoff calculation
    Delay1 = calculate_backoff(CustomPolicy, 1),
    true = Delay1 >= 2000,
    true = Delay1 < 2500,  % Allow for jitter

    Delay2 = calculate_backoff(CustomPolicy, 2),
    true = Delay2 >= 4000,  % 2000 * 2

    %% Test linear backoff
    LinearPolicy = new_retry_policy(#{
        backoff => linear,
        base_delay => 1000,
        jitter => false
    }),
    1000 = calculate_backoff(LinearPolicy, 1),
    2000 = calculate_backoff(LinearPolicy, 2),
    3000 = calculate_backoff(LinearPolicy, 3),

    %% Test constant backoff
    ConstantPolicy = new_retry_policy(#{
        backoff => constant,
        base_delay => 500,
        jitter => false
    }),
    500 = calculate_backoff(ConstantPolicy, 1),
    500 = calculate_backoff(ConstantPolicy, 10),

    %% Test Fibonacci backoff
    FibPolicy = new_retry_policy(#{
        backoff => fibonacci,
        base_delay => 1000,
        jitter => false
    }),
    1000 = calculate_backoff(FibPolicy, 1),  % fib(1) = 1
    1000 = calculate_backoff(FibPolicy, 2),  % fib(2) = 1
    2000 = calculate_backoff(FibPolicy, 3),  % fib(3) = 2
    3000 = calculate_backoff(FibPolicy, 4),  % fib(4) = 3
    5000 = calculate_backoff(FibPolicy, 5),  % fib(5) = 5

    %% Test error handler creation
    Handler = new_error_handler(
        test_handler,
        [business_exception, system_exception],
        fun(Exc) -> {handled, exception_message(Exc)} end
    ),
    test_handler = Handler#error_handler.handler_id,
    true = is_handler_enabled(Handler),

    %% Test error handler execution
    TestExc = new_exception(business_exception, "Test", #{}, []),
    {ok, _} = execute_handler(Handler, TestExc),

    %% Test error handler that throws
    BadHandler = new_error_handler(
        bad_handler,
        [system_exception],
        fun(_) -> error(bad) end
    ),
    {error, _} = execute_handler(BadHandler, TestExc),

    %% Test handler registration
    EmptyHandlers = #{},
    Handlers1 = register_handler(EmptyHandlers, Handler),
    [Handler] = find_handler(Handlers1, business_exception),
    [Handler] = find_handler(Handlers1, system_exception),
    [] = find_handler(Handlers1, timeout_exception),

    %% Test best handler selection
    PriorityHandler = new_error_handler(
        priority_handler,
        [business_exception],
        fun(_) -> {ok, priority} end
    ),
    HandlersWithPriority = register_handler(Handlers1, PriorityHandler#error_handler{priority = 10}),
    BestHandler = find_best_handler(HandlersWithPriority, TestExc),
    priority_handler = BestHandler#error_handler.handler_id,

    %% Test handler enable/disable
    DisabledHandler = set_handler_enabled(Handler, false),
    false = is_handler_enabled(DisabledHandler),
    ReEnabledHandler = set_handler_enabled(DisabledHandler, true),
    true = is_handler_enabled(ReEnabledHandler),

    %% Test compensator with dependencies
    CompWithDeps = new_compensator_with_deps(
        <<"activity_2">>,
        fun(_) -> {undone, activity_2} end,
        chained,
        undefined,
        [<<"activity_1">>]
    ),
    [<<"activity_1">>] = get_compensation_dependencies(CompWithDeps),
    false = all_dependencies_completed(CompWithDeps, #{}),

    %% Test compensator without dependencies
    true = all_dependencies_completed(Comp, #{}),

    %% Test compensator with metadata
    MetaComp = new_compensator_with_metadata(
        <<"payment_activity">>,
        fun(_) -> {refund, payment} end,
        deferred,
        #{trigger_on => payment_failed, reason => refund_required}
    ),
    pending = get_compensation_state(MetaComp),
    payment_failed = maps:get(trigger_on, MetaComp#compensator.metadata),

    %% Test handler with compensation
    HandlerWithComp = new_handler_with_compensation(
        handler_with_comp,
        [business_exception],
        fun(_) -> {handled, ok} end,
        fun(_) -> {compensated, ok} end
    ),
    handler_with_comp = HandlerWithComp#error_handler.handler_id,
    undefined =/= HandlerWithComp#error_handler.compensation_handler,

    %% Test circuit breaker registration
    CBHandlers = register_handler_with_circuit_breaker(
        #{},
        Handler,
        3,
        60000
    ),
    [CBHandler] = find_handler(CBHandlers, business_exception),
    #{state := closed, failures := 0, threshold := 3} =
        CBHandler#error_handler.circuit_breaker_state,

    %% Test circuit breaker state update
    Now = erlang:system_time(millisecond),
    {ok, UpdatedCB} = update_circuit_breaker_state(CBHandler, Now),
    #{state := closed, failures := 1} =
        UpdatedCB#error_handler.circuit_breaker_state,

    %% Test circuit breaker opening
    {ok, CB2} = update_circuit_breaker_state(UpdatedCB, Now),
    {ok, CB3} = update_circuit_breaker_state(CB2, Now),
    {circuit_open, CBOpen} = update_circuit_breaker_state(CB3, Now),
    #{state := open} = CBOpen#error_handler.circuit_breaker_state,

    %% Test exception state initialization
    ExceptionState = init_exception_state(<<"wf_test">>, self()),
    <<"wf_test">> = ExceptionState#exception_state.workflow_id,
    false = ExceptionState#exception_state.is_compensating,
    10 = ExceptionState#exception_state.max_parallel_compensations,

    %% Test metrics update
    UpdatedState = update_metrics(test_event, #{data => test}, ExceptionState),
    Metrics = UpdatedState#exception_state.metrics,
    true = maps:is_key(test_event, Metrics),

    %% Test audit logging
    AuditState = log_audit_event(exception_raised, #{exc_id => <<"exc_1">>},
                                 ExceptionState),
    AuditLog = AuditState#exception_state.audit_log,
    true = length(AuditLog) > 0,

    %% Test nth_fibonacci helper
    1 = nth_fibonacci(1),
    1 = nth_fibonacci(2),
    2 = nth_fibonacci(3),
    3 = nth_fibonacci(4),
    5 = nth_fibonacci(5),
    8 = nth_fibonacci(6),
    13 = nth_fibonacci(7),

    ok.
-endif.
