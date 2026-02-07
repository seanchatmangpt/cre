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
-export([raise/1, doctest_test/0]).

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
%% ## Examples
%%
%% Create exception with binary message:
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_exception(yawl_runtime_exception, <<"task failed">>, timeout).
%% #{type => yawl_runtime_exception, message => <<"task failed">>, ...}
%% 2> yawl_exceptions:exception_type(Exc).
%% yawl_runtime_exception
%% ```
%%
%% Create exception with string message (auto-converted to binary):
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_exception(yawl_syntax_exception, "invalid XML", {error, parse}).
%% #{type => yawl_syntax_exception, message => <<"invalid XML">>, ...}
%% 2> is_map(Exc).
%% true
%% ```
%%
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_syntax_exception(<<"missing end tag">>, {line, 42}).
%% #{type := yawl_syntax_exception, message := <<"missing end tag">>} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_validation_exception(<<"invalid task reference">>, unknown_task).
%% #{type := yawl_validation_exception} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"worker timeout">>, {timeout, 5000}).
%% #{type := yawl_runtime_exception, cause := {timeout, 5000}} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_state_exception(<<"invalid transition: running to paused">>, invalid_state).
%% #{type := yawl_state_exception} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_authentication_exception(<<"invalid credentials">>, bad_password).
%% #{type := yawl_authentication_exception} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_authorization_exception(<<"access denied">>, forbidden).
%% #{type := yawl_authorization_exception} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_resourcing_exception(<<"no available workers">>, resource_exhausted).
%% #{type := yawl_resourcing_exception} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_communication_exception(<<"node not responding">>, nodedown).
%% #{type := yawl_communication_exception} = Exc
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"error">>, cause).
%% 2> yawl_exceptions:exception_type(Exc).
%% yawl_runtime_exception
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_type(exception_record()) -> yawl_exception().

exception_type(#{type := Type}) ->
    Type.

%%--------------------------------------------------------------------
%% @doc Gets the exception message.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"task failed">>, cause).
%% 2> yawl_exceptions:exception_message(Exc).
%% <<"task failed">>
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_message(exception_record()) -> binary().

exception_message(#{message := Message}) ->
    Message.

%%--------------------------------------------------------------------
%% @doc Gets the underlying cause of the exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"error">>, {timeout, 5000}).
%% 2> yawl_exceptions:exception_cause(Exc).
%% {timeout, 5000}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec exception_cause(exception_record()) -> term().

exception_cause(#{cause := Cause}) ->
    Cause.

%%--------------------------------------------------------------------
%% @doc Gets the stacktrace if available.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"error">>, cause).
%% 2> yawl_exceptions:exception_stacktrace(Exc).
%% undefined
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"error">>, cause).
%% 2> Context = yawl_exceptions:exception_context(Exc).
%% 3> is_map(Context).
%% true
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"task failed">>, timeout).
%% 2> Formatted = yawl_exceptions:format_exception(Exc).
%% 3> is_binary(Formatted).
%% true
%% 4> binary:match(Formatted, <<"yawl_runtime_exception">>) =/= nomatch.
%% true
%% ```
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
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_syntax_exception(<<"parse error">>, bad_xml).
%% 2> yawl_exceptions:is_syntax_exception(Exc).
%% true
%% 3> Exc2 = yawl_exceptions:new_runtime_exception(<<"error">>, cause).
%% 4> yawl_exceptions:is_syntax_exception(Exc2).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_syntax_exception(exception_record()) -> boolean().

is_syntax_exception(#{type := yawl_syntax_exception}) -> true;
is_syntax_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a validation exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_validation_exception(<<"invalid">>, reason).
%% 2> yawl_exceptions:is_validation_exception(Exc).
%% true
%% 3> yawl_exceptions:is_syntax_exception(Exc).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_validation_exception(exception_record()) -> boolean().

is_validation_exception(#{type := yawl_validation_exception}) -> true;
is_validation_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a runtime exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"error">>, cause).
%% 2> yawl_exceptions:is_runtime_exception(Exc).
%% true
%% 3> yawl_exceptions:is_validation_exception(Exc).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_runtime_exception(exception_record()) -> boolean().

is_runtime_exception(#{type := yawl_runtime_exception}) -> true;
is_runtime_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a state exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_state_exception(<<"bad transition">>, reason).
%% 2> yawl_exceptions:is_state_exception(Exc).
%% true
%% 3> yawl_exceptions:is_runtime_exception(Exc).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_state_exception(exception_record()) -> boolean().

is_state_exception(#{type := yawl_state_exception}) -> true;
is_state_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is an authentication exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_authentication_exception(<<"bad auth">>, reason).
%% 2> yawl_exceptions:is_authentication_exception(Exc).
%% true
%% 3> yawl_exceptions:is_state_exception(Exc).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_authentication_exception(exception_record()) -> boolean().

is_authentication_exception(#{type := yawl_authentication_exception}) -> true;
is_authentication_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is an authorization exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_authorization_exception(<<"forbidden">>, reason).
%% 2> yawl_exceptions:is_authorization_exception(Exc).
%% true
%% 3> yawl_exceptions:is_authentication_exception(Exc).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_authorization_exception(exception_record()) -> boolean().

is_authorization_exception(#{type := yawl_authorization_exception}) -> true;
is_authorization_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a resourcing exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_resourcing_exception(<<"no workers">>, reason).
%% 2> yawl_exceptions:is_resourcing_exception(Exc).
%% true
%% 3> yawl_exceptions:is_authorization_exception(Exc).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_resourcing_exception(exception_record()) -> boolean().

is_resourcing_exception(#{type := yawl_resourcing_exception}) -> true;
is_resourcing_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if an exception is a communication exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_communication_exception(<<"node down">>, reason).
%% 2> yawl_exceptions:is_communication_exception(Exc).
%% true
%% 3> yawl_exceptions:is_resourcing_exception(Exc).
%% false
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec is_communication_exception(exception_record()) -> boolean().

is_communication_exception(#{type := yawl_communication_exception}) -> true;
is_communication_exception(_) -> false.

%%--------------------------------------------------------------------
%% @doc Tests if a record is any YAWL exception.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"error">>, cause).
%% 2> yawl_exceptions:is_yawl_exception(Exc).
%% true
%% 3> yawl_exceptions:is_yawl_exception(#{not => => an_exception}).
%% false
%% 4> yawl_exceptions:is_yawl_exception(not_a_map).
%% false
%% ```
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
%% The function throws an error exception that can be caught with try/catch.
%%
%% ## Examples
%%
%% ```erlang
%% 1> Exc = yawl_exceptions:new_runtime_exception(<<"error">>, cause).
%% 2> try yawl_exceptions:raise(Exc) catch error:{yawl_exception, T, M, C} -> {T, M, C} end.
%% {yawl_runtime_exception, <<"error">>, cause}
%% ```
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

%%--------------------------------------------------------------------
%% @doc Runs doctests for the module.
%%
%% Validates exception creation, classification, and formatting.
%%
%% ## Examples
%%
%% ```erlang
%% 1> yawl_exceptions:doctest_test().
%% ok
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Create exceptions with binary message
    Exc1 = new_exception(yawl_runtime_exception, <<"task failed">>, timeout),
    yawl_runtime_exception = exception_type(Exc1),
    <<"task failed">> = exception_message(Exc1),
    timeout = exception_cause(Exc1),

    %% Test 2: Create exceptions with string message (auto-converted)
    Exc2 = new_syntax_exception("missing tag", {line, 42}),
    yawl_syntax_exception = exception_type(Exc2),
    <<"missing tag">> = exception_message(Exc2),

    %% Test 3: Exception type-specific constructors
    Exc3 = new_validation_exception(<<"invalid ref">>, unknown_task),
    true = is_validation_exception(Exc3),
    false = is_syntax_exception(Exc3),

    Exc4 = new_runtime_exception(<<"worker timeout">>, {timeout, 5000}),
    true = is_runtime_exception(Exc4),
    false = is_validation_exception(Exc4),

    Exc5 = new_state_exception(<<"bad transition">>, invalid_state),
    true = is_state_exception(Exc5),

    Exc6 = new_authentication_exception(<<"bad creds">>, bad_password),
    true = is_authentication_exception(Exc6),

    Exc7 = new_authorization_exception(<<"access denied">>, forbidden),
    true = is_authorization_exception(Exc7),

    Exc8 = new_resourcing_exception(<<"no workers">>, exhausted),
    true = is_resourcing_exception(Exc8),

    Exc9 = new_communication_exception(<<"node down">>, nodedown),
    true = is_communication_exception(Exc9),

    %% Test 4: is_yawl_exception
    true = is_yawl_exception(Exc1),
    false = is_yawl_exception(#{not_an_exception => true}),
    false = is_yawl_exception(not_a_map),

    %% Test 5: Format exception
    Formatted = format_exception(Exc1),
    true = is_binary(Formatted),
    true = binary:match(Formatted, <<"yawl_runtime_exception">>) =/= nomatch,
    true = binary:match(Formatted, <<"task failed">>) =/= nomatch,

    %% Test 6: Exception accessors
    undefined = exception_stacktrace(Exc1),
    Context = exception_context(Exc1),
    true = is_map(Context),

    %% Test 7: Raise and catch exception
    Exc10 = new_runtime_exception(<<"test error">>, test_cause),
    try raise(Exc10)
    catch
        error:{yawl_exception, Type, Msg, Cause} ->
            yawl_runtime_exception = Type,
            <<"test error">> = Msg,
            test_cause = Cause
    end,

    ok.
