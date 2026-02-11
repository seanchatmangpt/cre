%%% @doc WF Effect Boundaries and Tool Calls
%%%
%%% This module implements effect boundaries for external tool calls and
%%% async effect handling. Effects allow tasks to yield control while waiting
%%% for external operations (HTTP calls, database queries, tool invocations)
%%% without blocking the execution engine.
%%%
%%% Effects follow a yield/resume protocol:
%%% 1. Task yields effect specification
%%% 2. Executor suspends execution state
%%% 3. Effect handler executes async operation
%%% 4. Result delivered back to executor
%%% 5. Executor resumes with result integrated into context
%%%
%%% @end
-module(wf_effect).

-export([
    %% Effect specification constructors
    effect/3,
    effect/4,
    effect_with_timeout/4,
    effect_with_timeout/5,

    %% Effect execution
    execute_effect/1,
    execute_effect/2,
    execute_async/2,
    execute_async/3,

    %% Effect result handling
    await_result/2,
    await_result/3,
    integrate_result/3,
    cancel_effect/1,

    %% Effect spec validation
    is_valid_effect/1,
    effect_type/1,
    effect_payload/1,
    effect_id/1,
    effect_timeout/1
]).

-export_type([
    effect_spec/0,
    effect_type/0,
    effect_id/0,
    effect_payload/0,
    effect_result/0,
    effect_timeout/0,
    effect_executor/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

%% Effect specification (what work needs to be done)
-type effect_spec() :: {
    effect,
    effect_type(),         % Type of effect (http, db, tool, etc.)
    effect_payload(),      % Effect-specific payload
    effect_id()            % Unique identifier for this effect
} | {
    effect,
    effect_type(),
    effect_payload(),
    effect_id(),
    effect_timeout()       % Optional timeout in milliseconds
}.

%% Effect type classification
-type effect_type() ::
      http                 % HTTP request
    | db                   % Database query
    | tool                 % External tool invocation
    | async_task           % Async computation
    | file_io              % File I/O operation
    | rpc                  % Remote procedure call
    | custom               % Custom effect type
    | atom().              % User-defined effect types

%% Effect unique identifier
-type effect_id() :: atom() | {atom(), term()} | binary() | reference().

%% Effect payload (type-specific data)
-type effect_payload() :: #{
    atom() => term()       % Payload data specific to effect type
}.

%% Effect timeout in milliseconds
-type effect_timeout() :: non_neg_integer() | infinity.

%% Effect result (what comes back from execution)
-type effect_result() ::
      {ok, term()}         % Success with result
    | {error, term()}      % Failure with reason
    | {timeout, effect_id()}. % Effect timed out

%% Effect executor function type
-type effect_executor() :: fun((effect_spec()) -> effect_result()).

%%% EFFECT SPECIFICATION CONSTRUCTORS =======================================

%% @doc Create an effect specification without timeout.
-spec effect(Type :: effect_type(), Payload :: effect_payload(), Id :: effect_id()) ->
    effect_spec().
effect(Type, Payload, Id) when is_atom(Type), is_map(Payload) ->
    {effect, Type, Payload, Id}.

%% @doc Create an effect specification with custom timeout.
-spec effect(
    Type :: effect_type(),
    Payload :: effect_payload(),
    Id :: effect_id(),
    Timeout :: effect_timeout()
) -> effect_spec().
effect(Type, Payload, Id, Timeout) when is_atom(Type), is_map(Payload) ->
    {effect, Type, Payload, Id, Timeout}.

%% @doc Create an effect specification with timeout (alias for effect/4).
-spec effect_with_timeout(
    Type :: effect_type(),
    Payload :: effect_payload(),
    Id :: effect_id(),
    Timeout :: effect_timeout()
) -> effect_spec().
effect_with_timeout(Type, Payload, Id, Timeout) ->
    effect(Type, Payload, Id, Timeout).

%% @doc Create an effect specification (5-arity version for compatibility).
-spec effect_with_timeout(
    Type :: effect_type(),
    Payload :: effect_payload(),
    Id :: effect_id(),
    Timeout :: effect_timeout(),
    _Extra :: term()
) -> effect_spec().
effect_with_timeout(Type, Payload, Id, Timeout, _Extra) ->
    effect(Type, Payload, Id, Timeout).

%%% EFFECT EXECUTION ========================================================

%% @doc Execute an effect synchronously using default executor.
%%
%% This is a blocking call that executes the effect immediately and
%% returns the result. Use execute_async/2 for non-blocking execution.
%%
%% @end
-spec execute_effect(Spec :: effect_spec()) -> effect_result().
execute_effect(Spec) ->
    execute_effect(Spec, fun default_executor/1).

%% @doc Execute an effect synchronously with custom executor.
-spec execute_effect(Spec :: effect_spec(), Executor :: effect_executor()) ->
    effect_result().
execute_effect(Spec, Executor) when is_function(Executor, 1) ->
    case is_valid_effect(Spec) of
        true ->
            Timeout = effect_timeout(Spec),
            execute_with_timeout(Spec, Executor, Timeout);
        false ->
            {error, {invalid_effect_spec, Spec}}
    end.

%% @doc Execute an effect asynchronously.
%%
%% Spawns a process to execute the effect and returns immediately with
%% a reference that can be used to await the result.
%%
%% @end
-spec execute_async(Spec :: effect_spec(), Callback :: pid() | {pid(), reference()}) ->
    {ok, reference()}.
execute_async(Spec, Callback) ->
    execute_async(Spec, Callback, fun default_executor/1).

%% @doc Execute an effect asynchronously with custom executor.
-spec execute_async(
    Spec :: effect_spec(),
    Callback :: pid() | {pid(), reference()},
    Executor :: effect_executor()
) -> {ok, reference()}.
execute_async(Spec, Callback, Executor) when is_function(Executor, 1) ->
    Ref = make_ref(),
    EffectId = effect_id(Spec),
    Timeout = effect_timeout(Spec),

    spawn_link(fun() ->
        Result = execute_with_timeout(Spec, Executor, Timeout),
        send_result(Callback, EffectId, Ref, Result)
    end),

    {ok, Ref}.

%%% EFFECT RESULT HANDLING ==================================================

%% @doc Wait for an effect result with default timeout.
-spec await_result(EffectId :: effect_id(), Ref :: reference()) ->
    effect_result().
await_result(EffectId, Ref) ->
    await_result(EffectId, Ref, 5000).

%% @doc Wait for an effect result with custom timeout.
-spec await_result(
    EffectId :: effect_id(),
    Ref :: reference(),
    Timeout :: effect_timeout()
) -> effect_result().
await_result(EffectId, Ref, Timeout) ->
    receive
        {effect_result, EffectId, Ref, Result} ->
            Result
    after Timeout ->
        {timeout, EffectId}
    end.

%% @doc Integrate an effect result into the execution context.
%%
%% Takes an effect result and merges it into the context's results map.
%% The result is stored under the effect ID for later retrieval.
%%
%% @end
-spec integrate_result(
    EffectId :: effect_id(),
    Result :: effect_result(),
    Ctx :: wf_term:context()
) -> wf_term:context().
integrate_result(EffectId, Result, Ctx) when is_map(Ctx) ->
    Results = maps:get(results, Ctx, #{}),
    NewResults = maps:put(EffectId, Result, Results),
    maps:put(results, NewResults, Ctx).

%% @doc Cancel a running effect.
%%
%% Attempts to cancel an effect by effect ID. This is best-effort;
%% effects that have already completed cannot be cancelled.
%%
%% @end
-spec cancel_effect(EffectId :: effect_id()) -> ok.
cancel_effect(_EffectId) ->
    %% TODO: Implement effect cancellation registry
    %% For now, this is a no-op placeholder
    ok.

%%% EFFECT SPEC VALIDATION ==================================================

%% @doc Validate an effect specification.
-spec is_valid_effect(Spec :: term()) -> boolean().
is_valid_effect({effect, Type, Payload, Id}) ->
    is_atom(Type) andalso is_map(Payload) andalso is_valid_effect_id(Id);
is_valid_effect({effect, Type, Payload, Id, Timeout}) ->
    is_atom(Type) andalso
    is_map(Payload) andalso
    is_valid_effect_id(Id) andalso
    is_valid_timeout(Timeout);
is_valid_effect(_) ->
    false.

%% @doc Extract effect type from specification.
-spec effect_type(Spec :: effect_spec()) -> effect_type().
effect_type({effect, Type, _Payload, _Id}) -> Type;
effect_type({effect, Type, _Payload, _Id, _Timeout}) -> Type.

%% @doc Extract effect payload from specification.
-spec effect_payload(Spec :: effect_spec()) -> effect_payload().
effect_payload({effect, _Type, Payload, _Id}) -> Payload;
effect_payload({effect, _Type, Payload, _Id, _Timeout}) -> Payload.

%% @doc Extract effect ID from specification.
-spec effect_id(Spec :: effect_spec()) -> effect_id().
effect_id({effect, _Type, _Payload, Id}) -> Id;
effect_id({effect, _Type, _Payload, Id, _Timeout}) -> Id.

%% @doc Extract effect timeout from specification.
-spec effect_timeout(Spec :: effect_spec()) -> effect_timeout().
effect_timeout({effect, _Type, _Payload, _Id}) -> 5000; % Default 5 seconds
effect_timeout({effect, _Type, _Payload, _Id, Timeout}) -> Timeout.

%%% INTERNAL HELPERS ========================================================

%% @doc Default effect executor.
%%
%% This is a simple executor that handles basic effect types.
%% Production use should provide custom executors for specific effect types.
%%
%% @end
-spec default_executor(Spec :: effect_spec()) -> effect_result().
default_executor(Spec) ->
    Type = effect_type(Spec),
    Payload = effect_payload(Spec),
    Id = effect_id(Spec),

    case Type of
        http ->
            execute_http(Payload);
        db ->
            execute_db(Payload);
        tool ->
            execute_tool(Payload);
        async_task ->
            execute_async_task(Payload);
        file_io ->
            execute_file_io(Payload);
        rpc ->
            execute_rpc(Payload);
        custom ->
            execute_custom(Payload);
        _ ->
            {error, {unsupported_effect_type, Type, Id}}
    end.

%% @doc Execute with timeout wrapper.
-spec execute_with_timeout(
    Spec :: effect_spec(),
    Executor :: effect_executor(),
    Timeout :: effect_timeout()
) -> effect_result().
execute_with_timeout(Spec, Executor, infinity) ->
    Executor(Spec);
execute_with_timeout(Spec, Executor, Timeout) when is_integer(Timeout) ->
    Parent = self(),
    Ref = make_ref(),

    Pid = spawn_link(fun() ->
        Result = Executor(Spec),
        Parent ! {executor_result, Ref, Result}
    end),

    receive
        {executor_result, Ref, Result} ->
            Result
    after Timeout ->
        exit(Pid, kill),
        {timeout, effect_id(Spec)}
    end.

%% @doc Send result to callback.
-spec send_result(
    Callback :: pid() | {pid(), reference()},
    EffectId :: effect_id(),
    Ref :: reference(),
    Result :: effect_result()
) -> ok.
send_result(Pid, EffectId, Ref, Result) when is_pid(Pid) ->
    Pid ! {effect_result, EffectId, Ref, Result},
    ok;
send_result({Pid, _Tag}, EffectId, Ref, Result) when is_pid(Pid) ->
    Pid ! {effect_result, EffectId, Ref, Result},
    ok.

%% @doc Validate effect ID.
-spec is_valid_effect_id(Id :: term()) -> boolean().
is_valid_effect_id(Id) when is_atom(Id) -> true;
is_valid_effect_id(Id) when is_binary(Id) -> true;
is_valid_effect_id(Id) when is_reference(Id) -> true;
is_valid_effect_id({Name, _Tag}) when is_atom(Name) -> true;
is_valid_effect_id(_) -> false.

%% @doc Validate timeout value.
-spec is_valid_timeout(Timeout :: term()) -> boolean().
is_valid_timeout(infinity) -> true;
is_valid_timeout(T) when is_integer(T), T >= 0 -> true;
is_valid_timeout(_) -> false.

%%% EFFECT TYPE EXECUTORS ===================================================

%% HTTP effect executor
-spec execute_http(Payload :: effect_payload()) -> effect_result().
execute_http(#{url := _Url} = _Payload) ->
    %% TODO: Implement actual HTTP client integration
    {error, not_implemented};
execute_http(_) ->
    {error, invalid_http_payload}.

%% Database effect executor
-spec execute_db(Payload :: effect_payload()) -> effect_result().
execute_db(#{query := _Query} = _Payload) ->
    %% TODO: Implement actual database client integration
    {error, not_implemented};
execute_db(_) ->
    {error, invalid_db_payload}.

%% Tool invocation executor
-spec execute_tool(Payload :: effect_payload()) -> effect_result().
execute_tool(#{tool := _Tool, args := _Args} = _Payload) ->
    %% TODO: Implement tool invocation system
    {error, not_implemented};
execute_tool(_) ->
    {error, invalid_tool_payload}.

%% Async task executor
-spec execute_async_task(Payload :: effect_payload()) -> effect_result().
execute_async_task(#{fun := Fun, args := Args} = _Payload) when is_function(Fun) ->
    try
        Result = apply(Fun, Args),
        {ok, Result}
    catch
        _:Reason ->
            {error, Reason}
    end;
execute_async_task(_) ->
    {error, invalid_async_task_payload}.

%% File I/O executor
-spec execute_file_io(Payload :: effect_payload()) -> effect_result().
execute_file_io(#{operation := read, path := Path} = _Payload) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, Content};
        {error, Reason} -> {error, Reason}
    end;
execute_file_io(#{operation := write, path := Path, content := Content} = _Payload) ->
    case file:write_file(Path, Content) of
        ok -> {ok, ok};
        {error, Reason} -> {error, Reason}
    end;
execute_file_io(_) ->
    {error, invalid_file_io_payload}.

%% RPC executor
-spec execute_rpc(Payload :: effect_payload()) -> effect_result().
execute_rpc(#{node := Node, module := Mod, function := Fun, args := Args} = _Payload) ->
    try
        Result = rpc:call(Node, Mod, Fun, Args),
        {ok, Result}
    catch
        _:Reason ->
            {error, Reason}
    end;
execute_rpc(_) ->
    {error, invalid_rpc_payload}.

%% Custom executor (extensibility hook)
-spec execute_custom(Payload :: effect_payload()) -> effect_result().
execute_custom(#{executor := Executor} = Payload) when is_function(Executor, 1) ->
    Executor(Payload);
execute_custom(_) ->
    {error, invalid_custom_payload}.

%%% TESTS ===================================================================

effect_construction_test_() ->
    [
        ?_assert(is_valid_effect(effect(http, #{url => "http://example.com"}, req1))),
        ?_assert(is_valid_effect(effect(db, #{query => "SELECT *"}, q1, 3000))),
        ?_assert(is_valid_effect(effect(tool, #{tool => grep, args => []}, t1, infinity))),
        ?_assertNot(is_valid_effect({effect, invalid})),
        ?_assertNot(is_valid_effect({effect, http, not_a_map, id1}))
    ].

effect_accessors_test_() ->
    Spec = effect(http, #{url => "http://example.com"}, req1, 1000),
    [
        ?_assertEqual(http, effect_type(Spec)),
        ?_assertEqual(#{url => "http://example.com"}, effect_payload(Spec)),
        ?_assertEqual(req1, effect_id(Spec)),
        ?_assertEqual(1000, effect_timeout(Spec))
    ].

effect_timeout_default_test() ->
    Spec = effect(http, #{url => "http://example.com"}, req1),
    ?assertEqual(5000, effect_timeout(Spec)).

integrate_result_test() ->
    Ctx = #{data => foo, results => #{}},
    NewCtx = integrate_result(effect1, {ok, 42}, Ctx),
    Results = maps:get(results, NewCtx),
    ?assertEqual({ok, 42}, maps:get(effect1, Results)).

file_io_effect_test() ->
    %% Create a temp file for testing
    Path = "/tmp/wf_effect_test.txt",
    Content = <<"test data">>,

    %% Test write
    WriteSpec = effect(file_io, #{operation => write, path => Path, content => Content}, write1),
    ?assertMatch({ok, ok}, execute_effect(WriteSpec)),

    %% Test read
    ReadSpec = effect(file_io, #{operation => read, path => Path}, read1),
    ?assertMatch({ok, <<"test data">>}, execute_effect(ReadSpec)),

    %% Cleanup
    file:delete(Path).

async_task_effect_test() ->
    Spec = effect(async_task, #{fun => fun(X, Y) -> X + Y end, args => [2, 3]}, task1),
    ?assertEqual({ok, 5}, execute_effect(Spec)).

timeout_test() ->
    %% Create an effect that sleeps longer than timeout
    Spec = effect(
        async_task,
        #{fun => fun() -> timer:sleep(200), ok end, args => []},
        slow_task,
        50
    ),
    Result = execute_effect(Spec),
    ?assertEqual({timeout, slow_task}, Result).
