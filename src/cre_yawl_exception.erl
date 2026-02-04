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
%% @doc YAWL Exception Handling and Compensation Module
%%
%% This module provides exception handling and compensation patterns
%% for YAWL workflows in the CRE runtime environment.
%%
%% <h3>Exception Types</h3>
%% <ul>
%%   <li><code>business_exception</code> - Expected business rule violations</li>
%%   <li><code>system_exception</code> - System-level failures</li>
%%   <li><code>timeout_exception</code> - Operation timeout</li>
%%   <li><code>resource_exception</code> - Resource unavailability</li>
%% </ul>
%%
%% <h3>Compensation</h3>
%% Compensation is the process of undoing effects from completed
%% activities when a workflow cannot complete normally.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_yawl_exception).
-behaviour(gen_pnet).

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
         exception_id/1]).

%% Compensation API
-export([new_compensator/3,
         compensate/2,
         has_compensated/1,
         get_compensation_state/1]).

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
         find_handler/2,
         execute_handler/2]).

%%====================================================================
%% Types
%%====================================================================

-type exception_type() :: business_exception
                        | system_exception
                        | timeout_exception
                        | resource_exception
                        | data_exception
                        | communication_exception.

-type exception_id() :: binary().
-type handler_id() :: binary().

-record(yawl_exception, {
          id :: exception_id(),
          type :: exception_type(),
          message :: binary(),
          context :: map(),
          timestamp :: integer(),
          stacktrace :: list()
         }).

-record(compensator, {
          activity_id :: binary(),
          compensation_handler :: function(),
          state :: pending | executing | completed | failed,
          result :: undefined | {ok, term()} | {error, term()},
          created_at :: integer(),
          completed_at :: undefined | integer()
         }).

-record(retry_policy, {
          max_attempts = 3 :: non_neg_integer(),
          backoff_strategy = exponential :: exponential | linear | constant | custom,
          base_delay = 1000 :: non_neg_integer(),
          max_delay = 60000 :: non_neg_integer(),
          multiplier = 2.0 :: float(),
          jitter = true :: boolean(),
          jitter_factor = 0.1 :: float()
         }).

-record(error_handler, {
          handler_id :: handler_id(),
          exception_types :: [exception_type()],
          handler_fun :: function(),
          priority = 0 :: integer(),
          retry_policy = undefined :: undefined | #retry_policy{}
         }).

-record(exception_state, {
          current_exception = undefined :: undefined | #yawl_exception{},
          active_compensators = [] :: [#compensator{}],
          compensation_stack = [] :: [#compensator{}],
          handlers = #{} :: #{exception_type() => [#error_handler{}]}
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
%% @doc Creates a new exception record.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_exception(Type, Message, Context, Stacktrace) -> exception()
    when Type :: exception_type(),
         Message :: binary() | string(),
         Context :: map(),
         Stacktrace :: list().

new_exception(Type, Message, Context, Stacktrace) when is_list(Message) ->
    new_exception(Type, list_to_binary(Message), Context, Stacktrace);
new_exception(Type, Message, Context, Stacktrace) when is_binary(Message) ->
    #yawl_exception{
      id = generate_exception_id(),
      type = Type,
      message = Message,
      context = Context,
      timestamp = erlang:system_time(millisecond),
      stacktrace = Stacktrace
     }.

%%--------------------------------------------------------------------
%% @doc Gets the exception type.
%% @end
%%--------------------------------------------------------------------
-spec exception_type(Exception :: exception()) -> exception_type().

exception_type(#yawl_exception{type = Type}) -> Type.

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

%%====================================================================
%% Compensation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new compensator for an activity.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_compensator(ActivityId, HandlerFun, InitialState) -> compensator()
    when ActivityId :: binary(),
         HandlerFun :: function(),
         InitialState :: term().

new_compensator(ActivityId, HandlerFun, _InitialState) ->
    #compensator{
      activity_id = ActivityId,
      compensation_handler = HandlerFun,
      state = pending,
      created_at = erlang:system_time(millisecond)
     }.

%%--------------------------------------------------------------------
%% @doc Executes compensation for an activity.
%%
%% @end
%%--------------------------------------------------------------------
-spec compensate(Compensator :: compensator(), Input :: term()) ->
    {ok, compensator()} | {error, Reason :: term()}.

compensate(#compensator{state = completed}, _Input) ->
    {error, already_compensated};
compensate(#compensator{state = executing}, _Input) ->
    {error, compensation_in_progress};
compensate(#compensator{compensation_handler = Handler} = C, Input) ->
    C1 = C#compensator{state = executing},
    try
        Result = Handler(Input),
        {ok, C1#compensator{
                state = completed,
                result = {ok, Result},
                completed_at = erlang:system_time(millisecond)
               }}
    catch
        Kind:Reason:Stack ->
            {ok, C1#compensator{
                    state = failed,
                    result = {error, {Kind, Reason, Stack}},
                    completed_at = erlang:system_time(millisecond)
                   }}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a compensator has completed.
%% @end
%%--------------------------------------------------------------------
-spec has_compensated(Compensator :: compensator()) -> boolean().

has_compensated(#compensator{state = State}) ->
    State =:= completed.

%%--------------------------------------------------------------------
%% @doc Gets the current state of a compensator.
%% @end
%%--------------------------------------------------------------------
-spec get_compensation_state(Compensator :: compensator()) ->
    pending | executing | completed | failed.

get_compensation_state(#compensator{state = State}) -> State.

%%====================================================================
%% Retry Policy API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a default retry policy.
%% @end
%%--------------------------------------------------------------------
-spec new_retry_policy() -> retry_policy().

new_retry_policy() ->
    #retry_policy{}.

%%--------------------------------------------------------------------
%% @doc Creates a retry policy with options.
%%
%% Options:
%% - max_attempts: Maximum number of retry attempts (default: 3)
%% - backoff: exponential | linear | constant (default: exponential)
%% - base_delay: Base delay in milliseconds (default: 1000)
%% - max_delay: Maximum delay in milliseconds (default: 60000)
%% - multiplier: Multiplier for exponential backoff (default: 2.0)
%% - jitter: Enable jitter (default: true)
%% - jitter_factor: Jitter factor (default: 0.1)
%%
%% @end
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
-spec retry_policy_backoff(Policy :: retry_policy()) -> exponential | linear | constant.

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
                 constant -> Base
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

%%====================================================================
%% Error Handler API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new error handler.
%% @end
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
              Acc#{Type => lists:keystore(
                               element(2, Handler),
                               2,
                               L,
                               [Handler]
                              )}
      end,
      Handlers,
      Types).

%%--------------------------------------------------------------------
%% @doc Finds handlers for a specific exception type.
%% @end
%%--------------------------------------------------------------------
-spec find_handler(Handlers :: map(), ExceptionType :: exception_type()) ->
    [error_handler()].

find_handler(Handlers, ExceptionType) ->
    maps:get(ExceptionType, Handlers, []).

%%--------------------------------------------------------------------
%% @doc Executes an error handler with an exception.
%% @end
%%--------------------------------------------------------------------
-spec execute_handler(Handler :: error_handler(), Exception :: exception()) ->
    {ok, term()} | {error, term()}.

execute_handler(#error_handler{handler_fun = HandlerFun}, Exception) ->
    try
        {ok, HandlerFun(Exception)}
    catch
        Kind:Reason ->
            {error, {Kind, Reason}}
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

%% Petri net places for exception handling
place_lst() ->
    ['Active', 'ExceptionRaised', 'Handling', 'Compensating', 'Resolved',
     'Retry', 'Failed', 'CompensationStack'].

%% Petri net transitions
trsn_lst() ->
    [raise_exception, handle_exception, execute_handler, compensate_activity,
     push_compensator, retry_after_backoff, mark_resolved, mark_failed].

init_marking(_Place, _UsrInfo) -> [].

%% Preset for each transition
preset(raise_exception) -> ['Active'];
preset(handle_exception) -> ['ExceptionRaised'];
preset(execute_handler) -> ['Handling'];
preset(compensate_activity) -> ['Compensating'];
preset(push_compensator) -> ['Active'];
preset(retry_after_backoff) -> ['Retry'];
preset(mark_resolved) -> ['Handling'];
preset(mark_failed) -> ['ExceptionRaised'].

%% Enable conditions
is_enabled(raise_exception, _Marking, _UsrInfo) -> true;
is_enabled(handle_exception, _Marking, _UsrInfo) -> true;
is_enabled(execute_handler, _Marking, _UsrInfo) -> true;
is_enabled(compensate_activity, _Marking, _UsrInfo) -> true;
is_enabled(push_compensator, _Marking, _UsrInfo) -> true;
is_enabled(retry_after_backoff, _Marking, _UsrInfo) -> true;
is_enabled(mark_resolved, _Marking, _UsrInfo) -> true;
is_enabled(mark_failed, _Marking, _UsrInfo) -> true.

%% Transition firing
fire(raise_exception, #{'Active' := [Exception]}, _UsrInfo) when is_record(Exception, yawl_exception) ->
    {produce, #{
      'ExceptionRaised' => [Exception],
      'Active' => []
     }};

fire(handle_exception, #{'ExceptionRaised' := [Exception]}, #exception_state{handlers = Handlers}) ->
    ExceptionType = Exception#yawl_exception.type,
    case find_handler(Handlers, ExceptionType) of
        [] ->
            {produce, #{
              'Failed' => [Exception],
              'ExceptionRaised' => []
             }};
        [Handler | _] ->
            {produce, #{
              'Handling' => [{Exception, Handler}],
              'ExceptionRaised' => []
             }}
    end;

fire(execute_handler, #{'Handling' := [{Exception, Handler}]}, _UsrInfo) ->
    case execute_handler(Handler, Exception) of
        {ok, _Result} ->
            {produce, #{
              'Resolved' => [Exception],
              'Handling' => []
             }};
        {error, _Reason} ->
            {produce, #{
              'Retry' => [Exception],
              'Handling' => []
             }}
    end;

fire(compensate_activity, #{'Compensating' := [Compensator]}, _UsrInfo) ->
    case compensate(Compensator, undefined) of
        {ok, NewCompensator} ->
            {produce, #{
              'CompensationStack' => [NewCompensator],
              'Compensating' => []
             }};
        {error, _Reason} ->
            {produce, #{
              'Failed' => [Compensator],
              'Compensating' => []
             }}
    end;

fire(push_compensator, #{'Active' := [ActivityId]}, _UsrInfo) ->
    HandlerFun = fun(_Input) -> {compensated, ActivityId} end,
    Compensator = new_compensator(ActivityId, HandlerFun, undefined),
    {produce, #{
      'CompensationStack' => [Compensator],
      'Active' => [ActivityId]
     }};

fire(retry_after_backoff, #{'Retry' := [Exception]}, _UsrInfo) ->
    {produce, #{
      'ExceptionRaised' => [Exception],
      'Retry' => []
     }};

fire(mark_resolved, #{'Handling' := [{_Exception, _Handler}]}, _UsrInfo) ->
    {produce, #{
      'Active' => [resolved],
      'Handling' => []
     }};

fire(mark_failed, #{'ExceptionRaised' := [Exception]}, _UsrInfo) ->
    {produce, #{
      'Failed' => [Exception],
      'ExceptionRaised' => []
     }}.

code_change(_OldVsn, NetState, _Extra) -> {ok, NetState}.
handle_call(_Request, _From, _NetState) -> {reply, {error, bad_msg}}.
handle_cast(_Request, _NetState) -> noreply.
handle_info(_Request, _NetState) -> noreply.

init(_Arg) ->
    #exception_state{}.

terminate(_Reason, _NetState) -> ok.

trigger(_Place, _Token, _NetState) -> pass.
