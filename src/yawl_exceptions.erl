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
%% @doc YAWL Exception Hierarchy
%%
%% This module defines the exception hierarchy for YAWL workflow execution
%% in the CRE runtime environment.
%%
%% <h3>Exception Types</h3>
%%
%% <ul>
%%   <li><b>yawl_syntax_exception</b> - Syntax errors in specifications</li>
%%   <li><b>yawl_validation_exception</b> - Validation failures</li>
%%   <li><b>yawl_runtime_exception</b> - General runtime errors</li>
%%   <li><b>yawl_state_exception</b> - Invalid state transitions</li>
%%   <li><b>yawl_authentication_exception</b> - Authentication failures</li>
%%   <li><b>yawl_authorization_exception</b> - Authorization failures</li>
%%   <li><b>yawl_resourcing_exception</b> - Resource allocation failures</li>
%%   <li><b>yawl_communication_exception</b> - Communication errors</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% Exceptions are created using new_exception/3 and can be raised
%% using erlang:error/1 or erlang:raise/3.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_exceptions).

%%====================================================================
%% Exports
%%====================================================================

%% Exception creation
-export([new_exception/3,
         new_syntax_exception/2,
         new_validation_exception/2,
         new_runtime_exception/2,
         new_state_exception/2,
         new_authentication_exception/2,
         new_authorization_exception/2,
         new_resourcing_exception/2,
         new_communication_exception/2]).

%% Exception accessors
-export([exception_type/1,
         exception_message/1,
         exception_cause/1,
         exception_stacktrace/1,
         exception_context/1,
         format_exception/1]).

%% Exception testing
-export([is_syntax_exception/1,
         is_validation_exception/1,
         is_runtime_exception/1,
         is_state_exception/1,
         is_authentication_exception/1,
         is_authorization_exception/1,
         is_resourcing_exception/1,
         is_communication_exception/1,
         is_yawl_exception/1]).

%% Exception handling
-export([raise/1]).

%%====================================================================
%% Types
%%====================================================================

-type yawl_exception() ::
    yawl_syntax_exception |
    yawl_validation_exception |
    yawl_runtime_exception |
    yawl_state_exception |
    yawl_authentication_exception |
    yawl_authorization_exception |
    yawl_resourcing_exception |
    yawl_communication_exception.

-type exception_record() :: #{
        type => yawl_exception(),
        message => binary(),
        cause => term(),
        stacktrace => list() | undefined,
        context => map(),
        timestamp => integer()
       }.

-export_type([yawl_exception/0, exception_record/0]).

%%====================================================================
%% Exception Creation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new YAWL exception.
%%
%% @param Type The exception type
%% @param Message Human-readable error message
%% @param Cause The underlying cause (exception, error, or term)
%% @end
%%--------------------------------------------------------------------
-spec new_exception(Type :: yawl_exception(),
                    Message :: binary() | string(),
                    Cause :: term()) -> exception_record().

new_exception(Type, Message, Cause) when is_list(Message) ->
    new_exception(Type, list_to_binary(Message), Cause);
new_exception(Type, Message, Cause) when is_binary(Message) ->
    #{
        type => Type,
        message => Message,
        cause => Cause,
        stacktrace => undefined,
        context => #{},
        timestamp => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Creates a new syntax exception.
%%
%% Used for parsing errors in YAWL specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_syntax_exception(Message :: binary() | string(),
                           Cause :: term()) -> exception_record().

new_syntax_exception(Message, Cause) ->
    new_exception(yawl_syntax_exception, Message, Cause).

%%--------------------------------------------------------------------
%% @doc Creates a new validation exception.
%%
%% Used for specification validation failures.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_validation_exception(Message :: binary() | string(),
                               Cause :: term()) -> exception_record().

new_validation_exception(Message, Cause) ->
    new_exception(yawl_validation_exception, Message, Cause).

%%--------------------------------------------------------------------
%% @doc Creates a new runtime exception.
%%
%% Used for general runtime errors during workflow execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_runtime_exception(Message :: binary() | string(),
                            Cause :: term()) -> exception_record().

new_runtime_exception(Message, Cause) ->
    new_exception(yawl_runtime_exception, Message, Cause).

%%--------------------------------------------------------------------
%% @doc Creates a new state exception.
%%
%% Used for invalid state transitions in workflow cases.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_state_exception(Message :: binary() | string(),
                          Cause :: term()) -> exception_record().

new_state_exception(Message, Cause) ->
    new_exception(yawl_state_exception, Message, Cause).

%%--------------------------------------------------------------------
%% @doc Creates a new authentication exception.
%%
%% Used for authentication failures.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_authentication_exception(Message :: binary() | string(),
                                   Cause :: term()) -> exception_record().

new_authentication_exception(Message, Cause) ->
    new_exception(yawl_authentication_exception, Message, Cause).

%%--------------------------------------------------------------------
%% @doc Creates a new authorization exception.
%%
%% Used for authorization/permission failures.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_authorization_exception(Message :: binary() | string(),
                                  Cause :: term()) -> exception_record().

new_authorization_exception(Message, Cause) ->
    new_exception(yawl_authorization_exception, Message, Cause).

%%--------------------------------------------------------------------
%% @doc Creates a new resourcing exception.
%%
%% Used for resource allocation failures.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_resourcing_exception(Message :: binary() | string(),
                               Cause :: term()) -> exception_record().

new_resourcing_exception(Message, Cause) ->
    new_exception(yawl_resourcing_exception, Message, Cause).

%%--------------------------------------------------------------------
%% @doc Creates a new communication exception.
%%
%% Used for communication errors between components.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_communication_exception(Message :: binary() | string(),
                                  Cause :: term()) -> exception_record().

new_communication_exception(Message, Cause) ->
    new_exception(yawl_communication_exception, Message, Cause).

%%====================================================================
%% Exception Accessors
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets the exception type.
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_type(exception_record()) -> yawl_exception().

exception_type(#{type := Type}) ->
    Type.

%%--------------------------------------------------------------------
%% @doc Gets the exception message.
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_message(exception_record()) -> binary().

exception_message(#{message := Message}) ->
    Message.

%%--------------------------------------------------------------------
%% @doc Gets the underlying cause of the exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_cause(exception_record()) -> term().

exception_cause(#{cause := Cause}) ->
    Cause.

%%--------------------------------------------------------------------
%% @doc Gets the stacktrace if available.
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_stacktrace(exception_record()) -> list() | undefined.

exception_stacktrace(#{stacktrace := Stacktrace}) ->
    Stacktrace.

%%--------------------------------------------------------------------
%% @doc Gets the exception context map.
%%
%% Context contains additional debugging information.
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_context(exception_record()) -> map().

exception_context(#{context := Context}) ->
    Context.

%%--------------------------------------------------------------------
%% @doc Formats an exception for logging/display.
%%
%% Returns a human-readable formatted string.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_exception(exception_record()) -> binary().

format_exception(#{type := Type, message := Message, cause := Cause, stacktrace := Stack}) ->
    TypeBin = atom_to_binary(Type, utf8),
    CauseStr = format_cause(Cause),
    StackStr = format_stacktrace(Stack),

    Formatted = io_lib:format("[~s] ~s~n  Cause: ~s~s~n",
                             [TypeBin, Message, CauseStr, StackStr]),
    iolist_to_binary(Formatted).

%% @private
%% @doc Formats the cause term.
-spec format_cause(term()) -> binary().

format_cause({ExceptionType, ExceptionReason, _Stack}) when is_atom(ExceptionType) ->
    io_lib:format("~p:~p", [ExceptionType, ExceptionReason]);
format_cause({ExceptionType, ExceptionReason}) when is_atom(ExceptionType) ->
    io_lib:format("~p:~p", [ExceptionType, ExceptionReason]);
format_cause(Cause) ->
    io_lib:format("~p", [Cause]).

%% @private
%% @doc Formats the stacktrace.
-spec format_stacktrace(list() | undefined) -> binary().

format_stacktrace(undefined) ->
    <<>>;
format_stacktrace([]) ->
    <<>>;
format_stacktrace(Stack) ->
    Lines = [format_stack_entry(Entry) || Entry <- Stack],
    iolist_to_binary(["\n  Stacktrace:\n    " | lists:join("\n    ", Lines)]).

%% @private
%% @doc Formats a single stacktrace entry.
-spec format_stack_entry(term()) -> binary().

format_stack_entry({Module, Function, Arity, Location}) when is_list(Location) ->
    File = proplists:get_value(file, Location, "?"),
    Line = proplists:get_value(line, Location, "?"),
    iolist_to_binary([atom_to_binary(Module), ":",
                     atom_to_binary(Function), "/",
                     integer_to_binary(Arity),
                     " (", File, ":", integer_to_list(Line), ")"]);
format_stack_entry({Module, Function, Arity}) ->
    iolist_to_binary([atom_to_binary(Module), ":",
                     atom_to_binary(Function), "/",
                     integer_to_binary(Arity)]);
format_stack_entry(Entry) ->
    io_lib:format("~p", [Entry]).

%%====================================================================
%% Exception Testing
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a syntax exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_syntax_exception(exception_record()) -> boolean().

is_syntax_exception(#{type := yawl_syntax_exception}) -> true;
is_syntax_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a validation exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_validation_exception(exception_record()) -> boolean().

is_validation_exception(#{type := yawl_validation_exception}) -> true;
is_validation_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a runtime exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_runtime_exception(exception_record()) -> boolean().

is_runtime_exception(#{type := yawl_runtime_exception}) -> true;
is_runtime_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a state exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_state_exception(exception_record()) -> boolean().

is_state_exception(#{type := yawl_state_exception}) -> true;
is_state_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is an authentication exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_authentication_exception(exception_record()) -> boolean().

is_authentication_exception(#{type := yawl_authentication_exception}) -> true;
is_authentication_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is an authorization exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_authorization_exception(exception_record()) -> boolean().

is_authorization_exception(#{type := yawl_authorization_exception}) -> true;
is_authorization_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a resourcing exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_resourcing_exception(exception_record()) -> boolean().

is_resourcing_exception(#{type := yawl_resourcing_exception}) -> true;
is_resourcing_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a communication exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_communication_exception(exception_record()) -> boolean().

is_communication_exception(#{type := yawl_communication_exception}) -> true;
is_communication_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if a record is any YAWL exception.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_yawl_exception(term()) -> boolean().

is_yawl_exception(#{type := Type}) when is_atom(Type) ->
    Type =:= yawl_syntax_exception orelse
    Type =:= yawl_validation_exception orelse
    Type =:= yawl_runtime_exception orelse
    Type =:= yawl_state_exception orelse
    Type =:= yawl_authentication_exception orelse
    Type =:= yawl_authorization_exception orelse
    Type =:= yawl_resourcing_exception orelse
    Type =:= yawl_communication_exception;
is_yawl_exception(_) ->
    false.

%%====================================================================
%% Raising Exceptions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Raises a YAWL exception with current stacktrace.
%%
%% Usage: throw(yawl_exceptions:raise(ExcRec))
%%
%% @end
%%--------------------------------------------------------------------
-spec raise(exception_record()) -> no_return().

raise(ExcRec = #{type := Type, message := Message, cause := Cause}) ->
    %% Note: Stacktrace is captured by the new try/catch syntax
    %% when this exception is caught and re-raised
    ExcRec1 = ExcRec#{
        stacktrace => undefined,
        context => #{pid => self(), node => node()}
    },
    %% Exported for use by exception handling modules
    erlang:error({yawl_exception, Type, Message, Cause}, [ExcRec1]).
