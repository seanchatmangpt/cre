%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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

-module(wf_exception).

-moduledoc "
Exception handling constructs for workflow engine.

Provides try-catch regions, compensation tracking, and exception propagation
for robust workflow error handling.

Exception types:
- `application_error`: Business logic errors
- `system_error`: Technical/system failures
- `timeout_error`: Time-based exceptions
- `resource_error`: Resource unavailable
- `validation_error`: Input validation failures

```erlang
> %% Create an application error exception
> E = wf_exception:new(application_error, payment_failed, #{amount => 100}).
> wf_exception:type(E).
application_error
> wf_exception:data(E).
#{amount => 100}

> %% Create compensation action
> C = wf_exception:compensation(refund_payment, #{txn_id => tx123}).
> wf_exception:comp_action(C).
refund_payment
> wf_exception:comp_data(C).
#{txn_id => tx123}

> %% Handle exception by type
> Handler = wf_exception:handler(fun(E) -> wf_exception:type(E) =:= application_error end,
>                                 fun(_, _) -> {error_handled, app} end).
> wf_exception:handle(E, Handler).
{error_handled, app}

> %% Unhandled exception propagates
> Handler2 = wf_exception:handler(fun(E) -> wf_exception:type(E) =:= system_error end,
>                                  fun(_, _) -> {error_handled, sys} end).
> wf_exception:handle(E, Handler2).
{unhandled, E}
```
".

%%====================================================================
%% Exports
%%====================================================================

%% Exception creation
-export([new/3, new/2, from_error/1]).

%% Exception accessors
-export([type/1, reason/1, data/1, source/1, timestamp/1]).

%% Compensation actions
-export([compensation/2, compensation/1, comp_action/1, comp_data/1]).

%% Exception handlers
-export([handler/2, handle/2, can_handle/2]).

%% Exception bubbling
-export([bubble/1, is_bubbleable/1, set_source/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Exception type classification.
%%
%% Defines categories of exceptions that can occur in workflow execution:
%% <ul>
%%   <li><b>application_error:</b> Business logic/validation errors</li>
%%   <li><b>system_error:</b> Technical failures (crash, OOM)</li>
%%   <li><b>timeout_error:</b> Operation exceeded time limit</li>
%%   <li><b>resource_error:</b> Resource unavailable or locked</li>
%%   <li><b>validation_error:</b> Input data validation failure</li>
%% </ul>
%%--------------------------------------------------------------------
-type exception_type() :: application_error | system_error |
                          timeout_error | resource_error | validation_error.

%%--------------------------------------------------------------------
%% @doc Exception reason - a specific error identifier.
%%
%% Typically an atom identifying the specific error condition
%% within the exception type.
%%--------------------------------------------------------------------
-type reason() :: atom() | binary().

%%--------------------------------------------------------------------
%% @doc Exception data payload.
%%
%% Additional context about the error - amounts, IDs, messages, etc.
%%--------------------------------------------------------------------
-type exception_data() :: map().

%%--------------------------------------------------------------------
%% @doc Exception source identifier.
%%
%% Indicates where the exception originated - typically a transition
%% name, task ID, or service name.
%%--------------------------------------------------------------------
-type source() :: atom() | binary() | undefined.

%%--------------------------------------------------------------------
%% @doc Exception record.
%%
%% Complete exception information including type, reason, data,
%% source, and when it occurred.
%%--------------------------------------------------------------------
-type exception() :: #{
    type := exception_type(),
    reason := reason(),
    data := exception_data(),
    source := source(),
    timestamp := integer()
}.

%%--------------------------------------------------------------------
%% @doc Compensation action record.
%%
%% Defines an undo operation to execute when rolling back
%% a workflow region.
%%--------------------------------------------------------------------
-type compensation() :: #{
    action := atom() | function(),
    data := exception_data(),
    executed := boolean()
}.

%%--------------------------------------------------------------------
%% @doc Exception handler predicate function.
%%
%% Returns true if this handler can process the given exception.
%%--------------------------------------------------------------------
-type handler_predicate() :: fun((exception()) -> boolean()).

%%--------------------------------------------------------------------
%% @doc Exception handler function.
%%
%% Takes the exception and optional context, returns handler result.
%%--------------------------------------------------------------------
-type handler_function() :: fun((exception(), term()) -> term()).

%%--------------------------------------------------------------------
%% @doc Exception handler record.
%%
%% Combines a predicate to match exceptions with a function to handle them.
%%--------------------------------------------------------------------
-type handler() :: #{
    predicate := handler_predicate(),
    function := handler_function()
}.

%%--------------------------------------------------------------------
%% @doc Handler result.
%%
%% Either the handler function's result if matched, or the unhandled exception.
%%--------------------------------------------------------------------
-type handler_result() :: {handled, term()} | {unhandled, exception()}.

%% Export types
-export_type([exception_type/0, exception/0, compensation/0,
             handler/0, handler_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new exception with type, reason, and data.
%%
%% @param Type The category of exception
%% @param Reason Specific error identifier
%% @param Data Additional error context
%% @return An exception map with timestamp
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Type :: exception_type(),
          Reason :: reason(),
          Data :: exception_data()) -> exception().

new(Type, Reason, Data) when is_atom(Type), is_atom(Reason) ->
    #{
        type => Type,
        reason => Reason,
        data => Data,
        source => undefined,
        timestamp => erlang:monotonic_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Creates a new exception with empty data.
%%
%% @param Type The category of exception
%% @param Reason Specific error identifier
%% @return An exception map with empty data and timestamp
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Type :: exception_type(), Reason :: reason()) -> exception().

new(Type, Reason) ->
    new(Type, Reason, #{}).

%%--------------------------------------------------------------------
%% @doc Creates an exception from an Erlang error term.
%%
%% Converts standard Erlang throw/exit/error into wf_exception format.
%%
%% @param ErrorTerm The error from catch/catch
%% @return An appropriate exception map
%%
%% @end
%%--------------------------------------------------------------------
-spec from_error(ErrorTerm :: term()) -> exception().

from_error({throw, Term}) ->
    new(application_error, throw, #{value => Term});
from_error({'EXIT', Reason}) ->
    new(system_error, exit, #{reason => Reason});
from_error({error, Reason}) ->
    new(system_error, error, #{reason => Reason});
from_error({Type, Reason, _Stack}) when is_atom(Type) ->
    new(Type, Reason, #{});
from_error(Error) when is_atom(Error) ->
    new(system_error, Error, #{});
from_error(Error) ->
    new(application_error, unknown_error, #{value => Error}).

%%--------------------------------------------------------------------
%% @doc Gets the exception type.
%%
%% @param Exception The exception to query
%% @return The exception type atom
%%
%% @end
%%--------------------------------------------------------------------
-spec type(Exception :: exception()) -> exception_type().

type(#{type := Type}) ->
    Type.

%%--------------------------------------------------------------------
%% @doc Gets the exception reason.
%%
%% @param Exception The exception to query
%% @return The reason identifier
%%
%% @end
%%--------------------------------------------------------------------
-spec reason(Exception :: exception()) -> reason().

reason(#{reason := Reason}) ->
    Reason.

%%--------------------------------------------------------------------
%% @doc Gets the exception data payload.
%%
%% @param Exception The exception to query
%% @return The exception data map
%%
%% @end
%%--------------------------------------------------------------------
-spec data(Exception :: exception()) -> exception_data().

data(#{data := Data}) ->
    Data.

%%--------------------------------------------------------------------
%% @doc Gets the exception source.
%%
%% @param Exception The exception to query
%% @return The source identifier or undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec source(Exception :: exception()) -> source().

source(#{source := Source}) ->
    Source.

%%--------------------------------------------------------------------
%% @doc Gets the exception timestamp.
%%
%% @param Exception The exception to query
%% @return Timestamp in milliseconds
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp(Exception :: exception()) -> integer().

timestamp(#{timestamp := TS}) ->
    TS.

%%--------------------------------------------------------------------
%% @doc Creates a compensation action.
%%
%% @param Action Function or atom identifying the undo operation
%% @param Data Data needed for the compensation
%% @return A compensation map
%%
%% @end
%%--------------------------------------------------------------------
-spec compensation(Action :: atom() | function(), Data :: exception_data()) ->
          compensation().

compensation(Action, Data) ->
    #{
        action => Action,
        data => Data,
        executed => false
    }.

%%--------------------------------------------------------------------
%% @doc Creates a compensation action with no data.
%%
%% @param Action Function or atom identifying the undo operation
%% @return A compensation map with empty data
%%
%% @end
%%--------------------------------------------------------------------
-spec compensation(Action :: atom() | function()) -> compensation().

compensation(Action) ->
    compensation(Action, #{}).

%%--------------------------------------------------------------------
%% @doc Gets the action from a compensation.
%%
%% @param Comp The compensation record
%% @return The action function or atom
%%
%% @end
%%--------------------------------------------------------------------
-spec comp_action(Comp :: compensation()) -> atom() | function().

comp_action(#{action := Action}) ->
    Action.

%%--------------------------------------------------------------------
%% @doc Gets the data from a compensation.
%%
%% @param Comp The compensation record
%% @return The compensation data map
%%
%% @end
%%--------------------------------------------------------------------
-spec comp_data(Comp :: compensation()) -> exception_data().

comp_data(#{data := Data}) ->
    Data.

%%--------------------------------------------------------------------
%% @doc Creates an exception handler.
%%
%% @param Predicate Function to match exceptions
%% @param Function Function to handle matched exceptions
%% @return A handler map
%%
%% @end
%%--------------------------------------------------------------------
-spec handler(Predicate :: handler_predicate(),
              Function :: handler_function()) -> handler().

handler(Predicate, Function) ->
    #{
        predicate => Predicate,
        function => Function
    }.

%%--------------------------------------------------------------------
%% @doc Checks if a handler can process an exception.
%%
%% @param Exception The exception to check
%% @param Handler The handler to test
%% @return true if handler predicate matches
%%
%% @end
%%--------------------------------------------------------------------
-spec can_handle(Exception :: exception(), Handler :: handler()) -> boolean().

can_handle(Exception, #{predicate := Pred}) ->
    try
        Pred(Exception)
    catch
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Attempts to handle an exception with a handler.
%%
%% If the handler's predicate matches, executes the handler function.
%% Otherwise returns the exception as unhandled.
%%
%% @param Exception The exception to handle
%% @param Handler The handler to use
%% @return {handled, Result} or {unhandled, Exception}
%%
%% @end
%%--------------------------------------------------------------------
-spec handle(Exception :: exception(), Handler :: handler()) -> handler_result().

handle(Exception, #{predicate := Pred, function := Fun}) ->
    case can_handle(Exception, #{predicate => Pred}) of
        true ->
            try
                Result = Fun(Exception, undefined),
                {handled, Result}
            catch
                Error:Reason:Stack ->
                    %% Handler itself failed - create new exception
                    HandlerError = new(system_error, handler_failed,
                                       #{original => Exception,
                                         error => Error,
                                         reason => Reason,
                                         stack => Stack}),
                    {unhandled, HandlerError}
            end;
        false ->
            {unhandled, Exception}
    end.

%%--------------------------------------------------------------------
%% @doc Marks an exception for bubbling up the call stack.
%%
%% Wrapped exceptions carry source information for propagation tracking.
%%
%% @param Exception The exception to bubble
%% @return The exception with bubble flag
%%
%% @end
%%--------------------------------------------------------------------
-spec bubble(Exception :: exception()) -> exception().

bubble(#{data := Data} = Exception) ->
    Exception#{data => Data#{bubble => true}}.

%%--------------------------------------------------------------------
%% @doc Checks if an exception is configured to bubble.
%%
%% @param Exception The exception to check
%% @return true if exception should bubble up
%%
%% @end
%%--------------------------------------------------------------------
-spec is_bubbleable(Exception :: exception()) -> boolean().

is_bubbleable(#{data := Data}) ->
    maps:get(bubble, Data, false);
is_bubbleable(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Sets the source of an exception.
%%
%% Use to track where exceptions originated during workflow execution.
%%
%% @param Exception The exception to annotate
%% @param Source The source identifier
%% @return Exception with source set
%%
%% @end
%%--------------------------------------------------------------------
-spec set_source(Exception :: exception(), Source :: source()) -> exception().

set_source(Exception, Source) ->
    Exception#{source => Source}.

%%====================================================================
%% Internal Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Unit Tests
%%--------------------------------------------------------------------

new_test() ->
    Exc = new(application_error, payment_failed, #{amount => 100}),
    ?assertEqual(application_error, type(Exc)),
    ?assertEqual(payment_failed, reason(Exc)),
    ?assertEqual(#{amount => 100}, data(Exc)),
    ?assert(is_integer(timestamp(Exc))).

new_no_data_test() ->
    Exc = new(system_error, crash),
    ?assertEqual(system_error, type(Exc)),
    ?assertEqual(crash, reason(Exc)),
    ?assertEqual(#{}, data(Exc)).

from_error_throw_test() ->
    Exc = from_error({throw, foo}),
    ?assertEqual(application_error, type(Exc)),
    ?assertEqual(throw, reason(Exc)),
    ?assertEqual(#{value => foo}, data(Exc)).

from_error_exit_test() ->
    Exc = from_error({'EXIT', normal}),
    ?assertEqual(system_error, type(Exc)),
    ?assertEqual(exit, reason(Exc)),
    ?assertEqual(#{reason => normal}, data(Exc)).

from_error_error_test() ->
    Exc = from_error({error, ebadf}),
    ?assertEqual(system_error, type(Exc)),
    ?assertEqual(error, reason(Exc)),
    ?assertEqual(#{reason => ebadf}, data(Exc)).

compensation_test() ->
    Comp = compensation(refund, #{txn_id => tx123}),
    ?assertEqual(refund, comp_action(Comp)),
    ?assertEqual(#{txn_id => tx123}, comp_data(Comp)),
    ?assertEqual(false, maps:get(executed, Comp)).

compensation_no_data_test() ->
    Comp = compensation(rollback),
    ?assertEqual(rollback, comp_action(Comp)),
    ?assertEqual(#{}, comp_data(Comp)).

handler_match_test() ->
    Exc = new(application_error, foo),
    Handler = handler(
        fun(E) -> type(E) =:= application_error end,
        fun(_, _) -> handled end
    ),
    ?assertEqual({handled, handled}, handle(Exc, Handler)).

handler_no_match_test() ->
    Exc = new(application_error, foo),
    Handler = handler(
        fun(E) -> type(E) =:= system_error end,
        fun(_, _) -> handled end
    ),
    ?assertMatch({unhandled, _}, handle(Exc, Handler)).

bubble_test() ->
    Exc = new(application_error, foo),
    BubbleExc = bubble(Exc),
    ?assert(is_bubbleable(BubbleExc)),
    ?assertNot(is_bubbleable(Exc)).

set_source_test() ->
    Exc = new(application_error, foo),
    Exc2 = set_source(Exc, task_123),
    ?assertEqual(task_123, source(Exc2)).

-endif.
