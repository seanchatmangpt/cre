%% -*- erlang -*-
%% @doc Workflow Exception Handling Patterns for YAWL.
%%
%% Implements workflow exception handling patterns (WHP-01 through WHP-05)
%% from the Workflow Exception Patterns paper (2008).
%%
%% These patterns provide exception handling, compensation, retry, and
%% escalation capabilities for workflow execution in the CRE runtime.
%%
%% == Patterns ==
%%
%% <ul>
%%   <li><b>cancel_activity/2</b> (WHP-P19): Cancel a specific activity and
%%       any enabled downstream tasks</li>
%%   <li><b>cancel_case/1</b> (WHP-P20): Cancel entire workflow case</li>
%%   <li><b>cancel_region/2</b> (WHP-P25): Cancel all activities within a region</li>
%%   <li><b>compensation_handler/2</b> (WHP-03): Trigger compensation for
%%       completed activities</li>
%%   <li><b>retry_activity/2</b> (WHP-02): Retry failed activity with backoff
%%       strategy</li>
%%   <li><b>escalate_exception/2</b> (WHP-01): Escalate exception to higher
%%       level handler</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(exception_patterns).
-moduledoc """
Workflow exception handling pattern functions for YAWL workflows.

Provides pattern-level exception handling functions that modify markings
and control flow state during workflow execution.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Cancellation patterns
-export([cancel_activity/2,
         cancel_case/1,
         cancel_region/2]).

%% Compensation patterns
-export([compensation_handler/2,
         trigger_compensation/2,
         consecutive_compensation/1]).

%% Retry patterns
-export([retry_activity/2,
         retry_with_backoff/3]).

%% Escalation patterns
-export([escalate_exception/2,
         propagate_exception/2]).

%% Utility functions
-export([is_cancelled/1,
         mark_cancelled/2,
         get_cancelled_activities/1,
         clear_cancelled/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% Each place atom maps to a list of tokens currently in that place.
%%--------------------------------------------------------------------
-type marking() :: #{atom() => [term()]}.

%%--------------------------------------------------------------------
%% @doc Control flow state tracks workflow execution status.
%%
%% Includes cancelled activities, exception state, and retry counts.
%%--------------------------------------------------------------------
-type control_state() :: #{
    cancelled_activities => [atom()],
    exception_state => exception_state() | undefined,
    retry_counts => #{atom() => non_neg_integer()},
    compensation_stack => [compensation()]
}.

%%--------------------------------------------------------------------
%% @doc Exception state for tracking active exceptions.
%%--------------------------------------------------------------------
-type exception_state() :: #{
    type => atom(),
    reason => term(),
    source => atom() | undefined,
    timestamp => integer(),
    handled => boolean(),
    escalated => boolean()
}.

%%--------------------------------------------------------------------
%% @doc Compensation action for undoing completed activities.
%%--------------------------------------------------------------------
-type compensation() :: #{
    activity_id => atom(),
    handler => function(),
    state => pending | executing | completed | failed,
    data => map()
}.

%%--------------------------------------------------------------------
%% @doc Retry strategy configuration.
%%--------------------------------------------------------------------
-type retry_strategy() :: exponential | linear | constant | fibonacci.
-type retry_config() :: #{
    max_attempts => non_neg_integer(),
    strategy => retry_strategy(),
    base_delay => non_neg_integer(),
    max_delay => non_neg_integer(),
    multiplier => float()
}.

%%--------------------------------------------------------------------
%% @doc Result of pattern execution.
%%--------------------------------------------------------------------
-type pattern_result() ::
    {ok, marking()} |
    {ok, marking(), control_state()} |
    {error, term()}.

-export_type([marking/0, control_state/0, compensation/0, retry_config/0]).

%%====================================================================
%% Cancellation Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cancels a specific activity and any enabled downstream tasks.
%%
%% Implements WHP-P19: Cancel Activity pattern. Removes tokens from
%% the specified activity place and any connected downstream places.
%%
%% @param Marking Current Petri net marking
%% @param Activity Activity atom to cancel
%% @return Modified marking with activity cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_activity(Marking :: marking(), Activity :: atom()) -> marking().

cancel_activity(Marking, Activity) when is_map(Marking), is_atom(Activity) ->
    %% Remove tokens from the activity being cancelled
    Marking1 = maps:put(Activity, [], Marking),

    %% Find and cancel downstream activities by removing their tokens
    Downstream = find_downstream_activities(Activity, Marking),
    Marking2 = lists:foldl(
        fun(DownAct, Acc) ->
            maps:put(DownAct, [], Acc)
        end,
        Marking1,
        Downstream
    ),

    %% Add cancellation token to indicate activity was cancelled
    CancelPlace = list_to_atom(atom_to_list(Activity) ++ "_cancelled"),
    maps:put(CancelPlace, [cancelled], Marking2).

%%--------------------------------------------------------------------
%% @doc Cancels entire workflow case.
%%
%% Implements WHP-P20: Cancel Case pattern. Terminates all active
%% activities in the current workflow instance.
%%
%% @param Marking Current Petri net marking
%% @return Modified marking with all activities cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_case(Marking :: marking()) -> marking().

cancel_case(Marking) when is_map(Marking) ->
    %% Clear all active places
    ActivePlaces = find_active_places(Marking),

    %% Create cancelled marking
    lists:foldl(
        fun(Place, Acc) ->
            maps:put(Place, [], Acc)
        end,
        Marking,
        ActivePlaces
    ).

%%--------------------------------------------------------------------
%% @doc Cancels all activities within a specific region.
%%
%% Implements WHP-P25: Cancel Region pattern. Removes tokens from
%% all places within the specified region boundary.
%%
%% @param Marking Current Petri net marking
%% @param Region List of place atoms defining the region
%% @return Modified marking with region cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_region(Marking :: marking(), Region :: [atom()]) -> marking().

cancel_region(Marking, Region) when is_map(Marking), is_list(Region) ->
    %% Clear all places within the region
    lists:foldl(
        fun(Place, Acc) ->
            case maps:is_key(Place, Acc) of
                true -> maps:put(Place, [], Acc);
                false -> Acc
            end
        end,
        Marking,
        Region
    ).

%%====================================================================
%% Compensation Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Triggers compensation for completed activities.
%%
%% Implements WHP-03: Compensation pattern. Executes compensation
%% handlers for activities that need to be undone.
%%
%% @param Marking Current Petri net marking
%% @param Compensations List of compensation actions to execute
%% @return Modified marking after compensation
%%
%% @end
%%--------------------------------------------------------------------
-spec compensation_handler(Marking :: marking(),
                          Compensations :: [compensation()]) -> marking().

compensation_handler(Marking, []) when is_map(Marking) ->
    Marking;
compensation_handler(Marking, Compensations) when is_map(Marking), is_list(Compensations) ->
    %% Execute compensations in reverse order (LIFO)
    lists:foldl(
        fun(Comp, Acc) ->
            execute_compensation(Comp, Acc)
        end,
        Marking,
        lists:reverse(Compensations)
    ).

%%--------------------------------------------------------------------
%% @doc Triggers immediate compensation for a specific activity.
%%
%% Creates and executes a compensation action for the given activity.
%%
%% @param Marking Current Petri net marking
%% @param Activity Activity atom to compensate
%% @return Modified marking after compensation
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger_compensation(Marking :: marking(), Activity :: atom()) -> marking().

trigger_compensation(Marking, Activity) when is_map(Marking), is_atom(Activity) ->
    %% Create a default compensation handler
    Handler = fun(_Data) -> {compensated, Activity} end,
    Comp = #{
        activity_id => Activity,
        handler => Handler,
        state => pending,
        data => #{activity => Activity}
    },
    compensation_handler(Marking, [Comp]).

%%--------------------------------------------------------------------
%% @doc Executes consecutive compensations for multiple activities.
%%
%% Implements WHP-05: Consecutive Compensation pattern. Executes
%% compensations sequentially in dependency order.
%%
%% @param Compensations List of compensations to execute
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec consecutive_compensation(Compensations :: [compensation()]) ->
    ok | {error, term()}.

consecutive_compensation([]) ->
    ok;
consecutive_compensation(Compensations) when is_list(Compensations) ->
    %% Execute compensations in order (consecutive pattern)
    execute_consecutive_compensations(Compensations).

%%====================================================================
%% Retry Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Retries a failed activity with default configuration.
%%
%% Implements WHP-02: Retry pattern. Re-enables a failed activity
%% for retry execution.
%%
%% @param Marking Current Petri net marking
%% @param Activity Activity atom to retry
%% @return Modified marking with activity re-enabled
%%
%% @end
%%--------------------------------------------------------------------
-spec retry_activity(Marking :: marking(), Activity :: atom()) -> marking().

retry_activity(Marking, Activity) when is_map(Marking), is_atom(Activity) ->
    %% Default retry config: 3 attempts, exponential backoff
    Config = #{
        max_attempts => 3,
        strategy => exponential,
        base_delay => 1000,
        max_delay => 60000,
        multiplier => 2.0
    },
    retry_with_backoff(Marking, Activity, Config).

%%--------------------------------------------------------------------
%% @doc Retries a failed activity with configurable backoff strategy.
%%
%% Re-enables the activity and applies backoff delay based on
%% retry count and strategy.
%%
%% @param Marking Current Petri net marking
%% @param Activity Activity atom to retry
%% @param Config Retry configuration with strategy and limits
%% @return Modified marking with activity re-enabled
%%
%% @end
%%--------------------------------------------------------------------
-spec retry_with_backoff(Marking :: marking(),
                         Activity :: atom(),
                         Config :: retry_config()) -> marking().

retry_with_backoff(Marking, Activity, Config)
        when is_map(Marking), is_atom(Activity), is_map(Config) ->
    %% Get current retry count
    RetryCount = maps:get(Activity, maps:get(retry_counts, Config, #{}), 0),
    MaxAttempts = maps:get(max_attempts, Config, 3),

    case RetryCount < MaxAttempts of
        true ->
            %% Calculate backoff delay
            Delay = calculate_backoff_delay(
                maps:get(strategy, Config, exponential),
                RetryCount + 1,
                maps:get(base_delay, Config, 1000),
                maps:get(max_delay, Config, 60000),
                maps:get(multiplier, Config, 2.0)
            ),

            %% Apply delay (in real execution would use timer:sleep)
            %% For marking manipulation, we record the delay for later use
            RetryPlace = list_to_atom(atom_to_list(Activity) ++ "_retry"),
            Marking1 = maps:put(RetryPlace, [{retry, RetryCount + 1, Delay}], Marking),

            %% Re-enable the activity
            maps:put(Activity, [enabled], Marking1);
        false ->
            %% Max retries exceeded, mark as failed
            FailedPlace = list_to_atom(atom_to_list(Activity) ++ "_failed"),
            maps:put(FailedPlace, [max_retries_exceeded], Marking)
    end.

%%====================================================================
%% Escalation Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Escalates an exception to a higher level handler.
%%
%% Implements WHP-01: Error Handler pattern with escalation.
%% Propagates exception up the workflow hierarchy.
%%
%% @param Marking Current Petri net marking
%% @param Exception Exception map to escalate
%% @return Modified marking with exception escalated
%%
%% @end
%%--------------------------------------------------------------------
-spec escalate_exception(Marking :: marking(), Exception :: map()) -> marking().

escalate_exception(Marking, Exception) when is_map(Marking), is_map(Exception) ->
    %% Mark exception as escalated
    Exception1 = Exception#{escalated => true},

    %% Place exception in escalation place
    EscalationPlace = maps:get(escalation_place, Exception, 'exception_escalated'),
    maps:put(EscalationPlace, [Exception1], Marking).

%%--------------------------------------------------------------------
%% @doc Propagates exception to parent workflow or handler.
%%
%% Moves exception token to parent handler place for processing.
%%
%% @param Marking Current Petri net marking
%% @param Exception Exception map to propagate
%% @return Modified marking with exception propagated
%%
%% @end
%%--------------------------------------------------------------------
-spec propagate_exception(Marking :: marking(), Exception :: map()) -> marking().

propagate_exception(Marking, Exception) when is_map(Marking), is_map(Exception) ->
    %% Get source to determine parent
    Source = maps:get(source, Exception, undefined),

    %% Remove from current place
    CurrentPlace = maps:get(current_place, Exception, 'exception_raised'),
    Marking1 = maps:put(CurrentPlace, [], Marking),

    %% Add to parent place
    ParentPlace = determine_parent_place(Source),
    maps:put(ParentPlace, [Exception], Marking1).

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a place has been cancelled.
%%
%% @param Marking Current Petri net marking
%% @return true if any cancellation tokens present
%%
%% @end
%%--------------------------------------------------------------------
-spec is_cancelled(Marking :: marking()) -> boolean().

is_cancelled(Marking) when is_map(Marking) ->
    maps:fold(
        fun(_Place, Tokens, Acc) ->
            case Acc of
                true -> true;
                false -> lists:member(cancelled, Tokens) orelse
                        lists:keymember(cancelled, 1, Tokens)
            end
        end,
        false,
        Marking
    ).

%%--------------------------------------------------------------------
%% @doc Marks a place as cancelled in the marking.
%%
%% @param Marking Current Petri net marking
%% @param Place Place atom to mark as cancelled
%% @return Modified marking with place cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec mark_cancelled(Marking :: marking(), Place :: atom()) -> marking().

mark_cancelled(Marking, Place) when is_map(Marking), is_atom(Place) ->
    CancelledPlace = list_to_atom(atom_to_list(Place) ++ "_cancelled"),
    maps:put(CancelledPlace, [cancelled], Marking).

%%--------------------------------------------------------------------
%% @doc Gets list of cancelled activities from marking.
%%
%% @param Marking Current Petri net marking
%% @return List of cancelled activity atoms
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cancelled_activities(Marking :: marking()) -> [atom()].

get_cancelled_activities(Marking) when is_map(Marking) ->
    maps:fold(
        fun(Place, Tokens, Acc) ->
            case lists:member(cancelled, Tokens) of
                true ->
                    %% Extract base activity name from cancelled place
                    BasePlace = remove_cancelled_suffix(Place),
                    [BasePlace | Acc];
                false ->
                    Acc
            end
        end,
        [],
        Marking
    ).

%%--------------------------------------------------------------------
%% @doc Clears cancellation tokens from the marking.
%%
%% @param Marking Current Petri net marking
%% @return Modified marking with cancellations cleared
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_cancelled(Marking :: marking()) -> marking().

clear_cancelled(Marking) when is_map(Marking) ->
    maps:map(
        fun(Place, Tokens) ->
            case lists:suffix("_cancelled", atom_to_list(Place)) of
                true -> [];
                false -> Tokens
            end
        end,
        Marking
    ).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Finds downstream activities connected to a given activity.
%%
%% @param Activity Source activity atom
%% @param Marking Current Petri net marking
%% @return List of downstream activity atoms
%%
%% @end
%%--------------------------------------------------------------------
-spec find_downstream_activities(Activity :: atom(), Marking :: marking()) -> [atom()].

find_downstream_activities(Activity, Marking) ->
    %% In a real implementation, this would traverse the workflow graph
    %% For now, we check for places with names following YAWL conventions
    DownstreamSuffixes = ["_next", "_after", "_output", "_result"],
    maps:fold(
        fun(Place, _Tokens, Acc) ->
            PlaceStr = atom_to_list(Place),
            case lists:any(fun(Suffix) ->
                lists:suffix(Suffix, PlaceStr) andalso
                lists:prefix(atom_to_list(Activity), PlaceStr)
            end, DownstreamSuffixes) of
                true -> [Place | Acc];
                false -> Acc
            end
        end,
        [],
        Marking
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds all active places in the marking.
%%
%% @param Marking Current Petri net marking
%% @return List of place atoms with tokens
%%
%% @end
%%--------------------------------------------------------------------
-spec find_active_places(Marking :: marking()) -> [atom()].

find_active_places(Marking) ->
    maps:fold(
        fun(Place, Tokens, Acc) ->
            case Tokens of
                [] -> Acc;
                _ -> [Place | Acc]
            end
        end,
        [],
        Marking
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a single compensation action.
%%
%% @param Comp Compensation record
%% @param Marking Current Petri net marking
%% @return Modified marking after compensation
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_compensation(Comp :: compensation(), Marking :: marking()) -> marking().

execute_compensation(#{activity_id := Activity, handler := Handler, state := State}, Marking) ->
    case State of
        completed ->
            %% Already compensated
            Marking;
        _ ->
            %% Execute the compensation handler
            try
                Handler(#{activity => Activity}),
                %% Mark compensation as complete
                CompPlace = list_to_atom(atom_to_list(Activity) ++ "_compensated"),
                maps:put(CompPlace, [compensated], Marking)
            catch
                _:_ ->
                    %% Compensation failed
                    FailedPlace = list_to_atom(atom_to_list(Activity) ++ "_comp_failed"),
                    maps:put(FailedPlace, [compensation_failed], Marking)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Executes compensations consecutively in order.
%%
%% @param Compensations List of compensations to execute
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_consecutive_compensations([compensation()]) -> ok | {error, term()}.

execute_consecutive_compensations([]) ->
    ok;
execute_consecutive_compensations([#{activity_id := Activity, handler := Handler} | Rest]) ->
    try
        Handler(#{activity => Activity}),
        execute_consecutive_compensations(Rest)
    catch
        Error:Reason ->
            {error, {compensation_failed, Activity, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates backoff delay based on strategy.
%%
%% @param Strategy Retry strategy atom
%% @param Attempt Current attempt number (1-based)
%% @param BaseDelay Base delay in milliseconds
%% @param MaxDelay Maximum delay cap
%% @param Multiplier Exponential multiplier
%% @return Delay in milliseconds
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_backoff_delay(Strategy :: retry_strategy(),
                             Attempt :: pos_integer(),
                             BaseDelay :: non_neg_integer(),
                             MaxDelay :: non_neg_integer(),
                             Multiplier :: float()) -> non_neg_integer().

calculate_backoff_delay(exponential, Attempt, BaseDelay, MaxDelay, Multiplier) ->
    Delay = round(BaseDelay * math:pow(Multiplier, Attempt - 1)),
    min(Delay, MaxDelay);
calculate_backoff_delay(linear, Attempt, BaseDelay, MaxDelay, _Multiplier) ->
    Delay = BaseDelay * Attempt,
    min(Delay, MaxDelay);
calculate_backoff_delay(constant, _Attempt, BaseDelay, _MaxDelay, _Multiplier) ->
    BaseDelay;
calculate_backoff_delay(fibonacci, Attempt, BaseDelay, MaxDelay, _Multiplier) ->
    Fib = nth_fibonacci(Attempt),
    Delay = BaseDelay * Fib,
    min(Delay, MaxDelay).

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates nth Fibonacci number.
%%
%% @end
%%--------------------------------------------------------------------
-spec nth_fibonacci(N :: pos_integer()) -> pos_integer().

nth_fibonacci(1) -> 1;
nth_fibonacci(2) -> 1;
nth_fibonacci(N) when N > 2 ->
    nth_fibonacci(N - 1) + nth_fibonacci(N - 2).

%%--------------------------------------------------------------------
%% @private
%% @doc Determines parent handler place for exception escalation.
%%
%% @end
%%--------------------------------------------------------------------
-spec determine_parent_place(Source :: atom() | undefined) -> atom().

determine_parent_place(undefined) -> 'exception_parent';
determine_parent_place(Source) ->
    %% Generate parent place name from source
    SourceBin = atom_to_binary(Source, utf8),
    ParentBin = <<SourceBin/binary, "_parent">>,
    binary_to_atom(ParentBin, utf8).

%%--------------------------------------------------------------------
%% @private
%% @doc Removes _cancelled suffix from place name.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_cancelled_suffix(Place :: atom()) -> atom().

remove_cancelled_suffix(Place) ->
    PlaceStr = atom_to_list(Place),
    case lists:suffix("_cancelled", PlaceStr) of
        true ->
            BaseStr = lists:sublist(PlaceStr, length(PlaceStr) - length("_cancelled")),
            list_to_atom(BaseStr);
        false ->
            Place
    end.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test cancel_activity removes activity tokens
%%--------------------------------------------------------------------
cancel_activity_test() ->
    Marking = #{task1 => [token], task1_next => [token], task2 => [token]},
    Result = cancel_activity(Marking, task1),
    ?assertEqual([], maps:get(task1, Result)),
    ?assertEqual([], maps:get(task1_next, Result)),
    ?assertEqual([token], maps:get(task2, Result)),
    ?assertEqual([cancelled], maps:get(task1_cancelled, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancel_case clears all active places
%%--------------------------------------------------------------------
cancel_case_test() ->
    Marking = #{task1 => [token], task2 => [token], task3 => []},
    Result = cancel_case(Marking),
    ?assertEqual([], maps:get(task1, Result)),
    ?assertEqual([], maps:get(task2, Result)),
    ?assertEqual([], maps:get(task3, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancel_region clears only region places
%%--------------------------------------------------------------------
cancel_region_test() ->
    Marking = #{task1 => [token], task2 => [token], task3 => [token]},
    Result = cancel_region(Marking, [task1, task2]),
    ?assertEqual([], maps:get(task1, Result)),
    ?assertEqual([], maps:get(task2, Result)),
    ?assertEqual([token], maps:get(task3, Result)).

%%--------------------------------------------------------------------
%% @doc Test compensation_handler with no compensations
%%--------------------------------------------------------------------
compensation_handler_empty_test() ->
    Marking = #{place1 => [token]},
    Result = compensation_handler(Marking, []),
    ?assertEqual(Marking, Result).

%%--------------------------------------------------------------------
%% @doc Test retry_activity re-enables activity
%%--------------------------------------------------------------------
retry_activity_test() ->
    Marking = #{task1 => [], task1_failed => [failed]},
    Result = retry_activity(Marking, task1),
    ?assertEqual([enabled], maps:get(task1, Result)),
    ?assertMatch([{retry, 1, _Delay}], maps:get(task1_retry, Result)).

%%--------------------------------------------------------------------
%% @doc Test escalate_exception marks as escalated
%%--------------------------------------------------------------------
escalate_exception_test() ->
    Marking = #{active => [token]},
    Exception = #{type => error, reason => test},
    Result = escalate_exception(Marking, Exception),
    ?assertEqual([Exception#{escalated => true}],
                 maps:get('exception_escalated', Result)).

%%--------------------------------------------------------------------
%% @doc Test is_cancelled detects cancellation
%%--------------------------------------------------------------------
is_cancelled_test() ->
    Marking1 = #{task1 => [token]},
    ?assertNot(is_cancelled(Marking1)),
    Marking2 = #{task1_cancelled => [cancelled]},
    ?assert(is_cancelled(Marking2)).

%%--------------------------------------------------------------------
%% @doc Test mark_cancelled adds cancellation token
%%--------------------------------------------------------------------
mark_cancelled_test() ->
    Marking = #{task1 => [token]},
    Result = mark_cancelled(Marking, task1),
    ?assertEqual([cancelled], maps:get(task1_cancelled, Result)).

%%--------------------------------------------------------------------
%% @doc Test get_cancelled_activities extracts cancelled list
%%--------------------------------------------------------------------
get_cancelled_activities_test() ->
    Marking = #{task1_cancelled => [cancelled],
                task2_cancelled => [cancelled],
                task3 => [token]},
    Result = get_cancelled_activities(Marking),
    ?assert(lists:member(task1, Result)),
    ?assert(lists:member(task2, Result)),
    ?assertNot(lists:member(task3, Result)).

%%--------------------------------------------------------------------
%% @doc Test clear_cancelled removes cancellation tokens
%%--------------------------------------------------------------------
clear_cancelled_test() ->
    Marking = #{task1_cancelled => [cancelled], task2 => [token]},
    Result = clear_cancelled(Marking),
    ?assertEqual([], maps:get(task1_cancelled, Result)),
    ?assertEqual([token], maps:get(task2, Result)).

-endif.
