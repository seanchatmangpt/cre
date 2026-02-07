%% -*- erlang -*-
%%
-module(wf_try_region).

%%--------------------------------------------------------------------
%% @doc Exception handling and compensation for workflow engine.
%%
%% This module provides try-catch regions for workflow execution with
%% automatic compensation on error.
%%
%% == Doctests ==
%%
%% Try-catch that catches error and runs handler:
%% ```erlang
%% > wf_try_region:execute(
%% ..     fun() -> erlang:error(test_error) end,
%% ..     fun(Exc) -> {caught, wf_exception:reason(Exc)} end,
%% ..     self(),
%% ..     []
%% .. ).
%% {caught, test_error}
%% ```
%%
%% With compensation - undo actions on error:
%% ```erlang
%% > Comp = wf_exception:compensation(rollback, #{id => 1}),
%% > wf_try_region:execute(
%% ..     fun() -> throw(test_error) end,
%% ..     fun(Exc) -> wf_exception:reason(Exc) end,
%% ..     self(),
%% ..     [Comp]
%% .. ).
%% throw
%% ```
%% @end
%%--------------------------------------------------------------------

%%====================================================================
%% Exports
%%====================================================================

-export([execute/4]).
-export([add_compensation/2]).
-export([raise/4]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a function with exception handling and compensation.
%%
%% @param TryFun Function to execute with exception protection
%% @param CatchFun Function to call on exception
%% @param Engine Engine pid or name
%% @param Compensations Existing compensation stack
%% @return Result of TryFun or CatchFun
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(TryFun :: fun(() -> term()),
              CatchFun :: fun((wf_exception:exception()) -> term()),
              Engine :: pid() | atom(),
              Compensations :: [wf_exception:compensation()]) -> term().

execute(TryFun, CatchFun, Engine, Compensations) ->
    try
        TryFun()
    catch
        Type:Reason:Stack ->
            Exc = wf_exception:new(Type, Reason,
                                  #{stack => Stack,
                                    compensations_count => length(Compensations)}),
            %% Run compensation actions in reverse order (LIFO)
            run_compensations(Engine, Exc, Compensations),
            %% Call the catch function
            CatchFun(Exc)
    end.

%%--------------------------------------------------------------------
%% @doc Adds a compensation action to the compensation stack.
%%
%% This should be called within a try_region before performing
%% operations that may need to be undone on error.
%%
%% @param Compensations Current compensation stack
%% @param Compensation New compensation action to add
%% @return Updated compensation stack
%%
%% @end
%%--------------------------------------------------------------------
-spec add_compensation(Compensations :: [wf_exception:compensation()],
                       Compensation :: wf_exception:compensation()) ->
          [wf_exception:compensation()].

add_compensation(Compensations, Compensation) ->
    [Compensation | Compensations].

%%--------------------------------------------------------------------
%% @doc Raises an exception from within workflow execution.
%%
%% @param Type Exception type
%% @param Reason Specific error identifier
%% @param Data Additional error context
%% @param Source Where the exception originated
%%
%% @end
%%--------------------------------------------------------------------
-spec raise(Type :: wf_exception:exception_type(),
            Reason :: wf_exception:reason(),
            Data :: wf_exception:exception_data(),
            Source :: atom() | binary()) -> no_return().

raise(Type, Reason, Data, Source) ->
    Exc = wf_exception:new(Type, Reason, Data),
    ExcWithSource = wf_exception:set_source(Exc, Source),
    throw(ExcWithSource).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Executes compensation actions in reverse order.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_compensations(Engine :: pid() | atom(),
                        Exception :: wf_exception:exception(),
                        Compensations :: [wf_exception:compensation()]) -> ok.

run_compensations(_Engine, _Exception, []) ->
    ok;
run_compensations(_Engine, Exception, Compensations) ->
    lists:foreach(
        fun(Comp) ->
            Action = wf_exception:comp_action(Comp),
            Data = wf_exception:comp_data(Comp),
            execute_compensation(Action, Data, Exception)
        end,
        lists:reverse(Compensations)
    ),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a single compensation action.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_compensation(Action :: atom() | function(),
                           Data :: map(),
                           Exception :: wf_exception:exception()) -> ok.

execute_compensation(Action, Data, Exception) when is_function(Action) ->
    try
        Action(Data, Exception)
    catch
        E:R:S ->
            error_logger:error_msg("Compensation failed: ~p:~p~nStack: ~p~n", [E, R, S])
    end,
    ok;
execute_compensation(Action, Data, Exception) when is_atom(Action) ->
    error_logger:info_msg("Executing compensation: ~p with data: ~p for exception: ~p~n",
                          [Action, Data, maps:get(reason, Exception)]),
    ok.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test that try-catch catches errors and runs handler
%%--------------------------------------------------------------------
try_catch_catches_error_test() ->
    TryFun = fun() -> erlang:error(test_error) end,
    CatchFun = fun(Exc) -> {caught, wf_exception:reason(Exc)} end,
    Result = execute(TryFun, CatchFun, self(), []),
    ?assertEqual({caught, test_error}, Result).

%%--------------------------------------------------------------------
%% @doc Test that normal execution returns result
%%--------------------------------------------------------------------
normal_execution_returns_result_test() ->
    TryFun = fun() -> {ok, success} end,
    CatchFun = fun(_) -> should_not_be_called end,
    Result = execute(TryFun, CatchFun, self(), []),
    ?assertEqual({ok, success}, Result).

%%--------------------------------------------------------------------
%% @doc Test that add_compensation adds to stack
%%--------------------------------------------------------------------
add_compensation_to_stack_test() ->
    Comp1 = wf_exception:compensation(action1, #{}),
    Comp2 = wf_exception:compensation(action2, #{}),
    Stack0 = [],
    Stack1 = add_compensation(Stack0, Comp1),
    Stack2 = add_compensation(Stack1, Comp2),
    ?assertEqual([Comp2, Comp1], Stack2).

%%--------------------------------------------------------------------
%% @doc Doctest for module documentation examples
%%--------------------------------------------------------------------
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

-endif.
