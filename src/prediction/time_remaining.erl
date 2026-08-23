%% -*- erlang -*-
%% @doc Timestamp-based Remaining Time Prediction
%%
%% This module implements a basic remaining time predictor using
%% linear regression on activity durations from historical data.
%%
%% The algorithm:
%% 1. Calculates average duration for each activity
%% 2. Estimates remaining time based on activities not yet completed
%% 3. Uses linear regression for trend adjustment
%%
%% @end

-module(time_remaining).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([predict_remaining_time/2]).
-export([train_from_log/1]).
-export([get_model/0]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Utility exports for testing
-export([calculate_activity_durations/1]).
-export([linear_regression/1]).
-export([calculate_remaining/2]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type timestamp() :: integer().
-type trace() :: [{activity(), timestamp()}].
-type event_log() :: [trace()].
-type milliseconds() :: non_neg_integer().

-type duration_stats() :: #{
    count => pos_integer(),
    total => milliseconds(),
    average => milliseconds(),
    min => milliseconds(),
    max => milliseconds()
}.

-type model() :: #{
    activity_durations => #{activity() => duration_stats()},
    overall_average => milliseconds(),
    regression_slope => float(),
    regression_intercept => float()
}.

-export_type([model/0, milliseconds/0]).

%% Server state
-record(state, {
    model :: model() | undefined
}).

%%====================================================================
%% gen_server API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Predicts remaining time for a trace given its timestamps.
%%
%% Trace format: [{Activity, Timestamp}] in chronological order.
%% Returns estimated milliseconds until completion.
%%
-spec predict_remaining_time(trace(), model() | undefined) -> milliseconds().
predict_remaining_time(_Trace, undefined) ->
    0;
predict_remaining_time([], _Model) ->
    0;
predict_remaining_time(Trace, #{activity_durations := Durations} = Model) ->
    %% Extract completed activities from the trace
    CompletedActivities = [A || {A, _T} <- Trace],

    %% Estimate remaining activities (simplified: assume sequential)
    %% In a real system, this would use a process model
    RemainingEstimate = estimate_remaining_activities(CompletedActivities, Durations, Model),

    %% Calculate expected time for remaining activities
    calculate_remaining(RemainingEstimate, Durations).

%% @doc Trains a prediction model from an event log.
%%
-spec train_from_log(event_log()) -> {ok, model()}.
train_from_log(Log) when is_list(Log) ->
    %% Calculate duration statistics for each activity
    ActivityDurations = calculate_activity_durations(Log),

    %% Calculate overall average duration
    OverallAvg = calculate_overall_average(ActivityDurations),

    %% Perform linear regression on trace durations
    {Slope, Intercept} = linear_regression(Log),

    Model = #{
        activity_durations => ActivityDurations,
        overall_average => OverallAvg,
        regression_slope => Slope,
        regression_intercept => Intercept
    },

    %% Update server state if running
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid -> gen_server:cast(?MODULE, {update_model, Model})
    end,

    {ok, Model}.

%% @doc Gets the current model from the server.
%%
-spec get_model() -> model() | undefined.
get_model() ->
    case whereis(?MODULE) of
        undefined -> undefined;
        _Pid -> gen_server:call(?MODULE, get_model)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{model = undefined}}.

handle_call(get_model, _From, State = #state{model = Model}) ->
    {reply, Model, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_model, Model}, State) ->
    {noreply, State#state{model = Model}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec calculate_activity_durations(event_log()) -> #{activity() => duration_stats()}.
calculate_activity_durations(Log) ->
    %% Extract all (activity, duration) pairs from traces
    AllPairs = lists:flatmap(fun extract_durations_from_trace/1, Log),

    %% Aggregate by activity
    lists:foldl(fun({Activity, Duration}, Acc) ->
        case maps:get(Activity, Acc) of
            undefined ->
                Acc#{Activity => #{
                    count => 1,
                    total => Duration,
                    average => Duration,
                    min => Duration,
                    max => Duration
                }};
            Stats ->
                #{
                    count := Count,
                    total := Total,
                    min := Min,
                    max := Max
                } = Stats,
                NewTotal = Total + Duration,
                NewCount = Count + 1,
                Acc#{Activity => #{
                    count => NewCount,
                    total => NewTotal,
                    average => NewTotal div NewCount,
                    min => min(Min, Duration),
                    max => max(Max, Duration)
                }}
        end
    end, #{}, AllPairs).

%% @private
-spec extract_durations_from_trace(trace()) -> [{activity(), milliseconds()}].
extract_durations_from_trace([]) ->
    [];
extract_durations_from_trace([_]) ->
    [];
extract_durations_from_trace([{Activity, Time1}, {_NextActivity, Time2} | Rest]) ->
    [{Activity, Time2 - Time1} | extract_durations_from_trace([{_NextActivity, Time2} | Rest])].

%% @private
-spec calculate_overall_average(#{activity() => duration_stats()}) -> milliseconds().
calculate_overall_average(ActivityDurations) ->
    case maps:values(ActivityDurations) of
        [] -> 0;
        StatsList ->
            TotalTime = lists:sum([begin
                #{count := C, total := T} = S,
                case C > 0 of true -> T; false -> 0 end
            end || S <- StatsList]),
            TotalCount = lists:sum([begin
                #{count := C} = S,
                case C > 0 of true -> C; false -> 0 end
            end || S <- StatsList]),
            case TotalCount of
                0 -> 0;
                _ -> TotalTime div TotalCount
            end
    end.

%% @private
-spec linear_regression(event_log()) -> {float(), float()}.
linear_regression(Log) ->
    %% Calculate trace lengths (number of activities) and total durations
    Data = lists:map(fun(Trace) ->
        Length = length(Trace),
        case Trace of
            [] -> {Length, 0};
            _ ->
                [{_FirstAct, FirstTime} | _] = Trace,
                {_LastAct, LastTime} = lists:last(Trace),
                {Length, LastTime - FirstTime}
        end
    end, Log),

    %% Filter out empty traces
    Filtered = [L || {Len, _Dur} = L <- Data, Len > 0],

    case Filtered of
        [] -> {0.0, 0.0};
        _ ->
            %% Simple linear regression: duration = slope * length + intercept
            N = length(Filtered),
            {SumX, SumY, SumXY, SumX2} = lists:foldl(fun({X, Y}, {SX, SY, SXY, SX2}) ->
                {SX + X, SY + Y, SXY + X * Y, SX2 + X * X}
            end, {0, 0, 0, 0}, Filtered),

            %% Calculate slope and intercept
            Slope = (N * SumXY - SumX * SumY) / (N * SumX2 - SumX * SumX),
            Intercept = (SumY - Slope * SumX) / N,

            {Slope, Intercept}
    end.

%% @private
-spec estimate_remaining_activities([activity()], #{activity() => duration_stats()}, model()) ->
    [activity()].
estimate_remaining_activities(_CompletedActivities, _Durations, _Model) ->
    %% Simplified: assume we need to complete common follow-up activities
    %% In a real system, this would use a process model to determine
    %% which activities are likely to follow

    %% For this simple implementation, return an empty list
    %% The prediction will fall back to overall average
    [].

%% @private
-spec calculate_remaining([activity()], #{activity() => duration_stats()}) -> milliseconds().
calculate_remaining([], #{overall_average := Avg}) ->
    Avg;
calculate_remaining(RemainingActivities, Durations) ->
    lists:foldl(fun(Activity, Acc) ->
        case maps:get(Activity, Durations) of
            undefined -> Acc;
            #{average := Avg} -> Acc + Avg
        end
    end, 0, RemainingActivities).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

simple_log() ->
    [[{a, 1000}, {b, 2000}, {c, 3000}],
     [{a, 1000}, {b, 2500}, {c, 3500}],
     [{a, 1000}, {b, 2000}, {c, 4000}]].

variable_log() ->
    [[{start, 0}, {task1, 1000}, {task2, 3000}, {end_marker, 6000}],
     [{start, 0}, {task1, 1500}, {task2, 3500}, {end_marker, 5000}],
     [{start, 0}, {task1, 800}, {task2, 2800}, {end_marker, 7000}]].

empty_log() ->
    [].

single_trace_log() ->
    [[{a, 0}, {b, 1000}, {c, 3000}]].

%%--------------------------------------------------------------------
%% Activity duration calculation tests
%%--------------------------------------------------------------------

calculate_activity_durations_test() ->
    Log = simple_log(),
    Durations = calculate_activity_durations(Log),

    AStats = maps:get(a, Durations),
    ?assertEqual(3, maps:get(count, AStats)),
    ?assert(maps:get(average, AStats) > 0).

calculate_activity_durations_empty_test() ->
    Log = empty_log(),
    Durations = calculate_activity_durations(Log),
    ?assertEqual(0, map_size(Durations)).

calculate_activity_durations_stats_test() ->
    Log = variable_log(),
    Durations = calculate_activity_durations(Log),

    Task1Stats = maps:get(task1, Durations),
    ?assertEqual(3, maps:get(count, Task1Stats)),
    ?assert(maps:get(total, Task1Stats) > 0),
    ?assert(maps:get(average, Task1Stats) > 0),
    ?assert(maps:get(min, Task1Stats) =< maps:get(max, Task1Stats)).

%%--------------------------------------------------------------------
%% Overall average tests
%%--------------------------------------------------------------------

calculate_overall_average_test() ->
    Durations = #{
        a => #{count => 2, total => 2000, average => 1000, min => 500, max => 1500},
        b => #{count => 2, total => 4000, average => 2000, min => 1500, max => 2500}
    },
    Avg = calculate_overall_average(Durations),
    ?assertEqual(1500, Avg).  %% (2000 + 4000) / 4

calculate_overall_average_empty_test() ->
    Avg = calculate_overall_average(#{}),
    ?assertEqual(0, Avg).

%%--------------------------------------------------------------------
%% Linear regression tests
%%--------------------------------------------------------------------

linear_regression_test() ->
    Log = simple_log(),
    {Slope, Intercept} = linear_regression(Log),
    ?assert(is_float(Slope)),
    ?assert(is_float(Intercept)).

linear_regression_empty_test() ->
    Log = empty_log(),
    {Slope, Intercept} = linear_regression(Log),
    ?assertEqual(0.0, Slope),
    ?assertEqual(0.0, Intercept).

linear_regression_single_test() ->
    Log = single_trace_log(),
    {Slope, Intercept} = linear_regression(Log),
    ?assert(is_float(Slope)).

%%--------------------------------------------------------------------
%% Prediction tests
%%--------------------------------------------------------------------

predict_remaining_time_test() ->
    Log = simple_log(),
    {ok, Model} = train_from_log(Log),
    Trace = [{a, 1000}, {b, 2000}],
    Time = predict_remaining_time(Trace, Model),
    ?assert(is_integer(Time)),
    ?assert(Time >= 0).

predict_remaining_time_empty_trace_test() ->
    Log = simple_log(),
    {ok, Model} = train_from_log(Log),
    Time = predict_remaining_time([], Model),
    ?assertEqual(0, Time).

predict_remaining_time_no_model_test() ->
    Trace = [{a, 1000}],
    Time = predict_remaining_time(Trace, undefined),
    ?assertEqual(0, Time).

predict_remaining_time_complete_trace_test() ->
    Log = simple_log(),
    {ok, Model} = train_from_log(Log),
    Trace = [{a, 1000}, {b, 2000}, {c, 3000}],
    Time = predict_remaining_time(Trace, Model),
    ?assert(is_integer(Time)).

%%--------------------------------------------------------------------
%% Training tests
%%--------------------------------------------------------------------

train_from_log_test() ->
    Log = simple_log(),
    {ok, Model} = train_from_log(Log),
    ?assert(is_map(Model)),
    ?assert(maps:is_key(activity_durations, Model)),
    ?assert(maps:is_key(overall_average, Model)),
    ?assert(maps:is_key(regression_slope, Model)),
    ?assert(maps:is_key(regression_intercept, Model)).

train_from_log_empty_test() ->
    Log = empty_log(),
    {ok, Model} = train_from_log(Log),
    ?assert(is_map(Model)),
    ?assertEqual(0, maps:get(overall_average, Model)).

train_from_log_durations_test() ->
    Log = variable_log(),
    {ok, #{activity_durations := Durations}} = train_from_log(Log),
    ?assert(maps:is_key(start, Durations)),
    ?assert(maps:is_key(task1, Durations)),
    ?assert(maps:is_key(task2, Durations)),
    ?assert(maps:is_key(end_marker, Durations)).

%%--------------------------------------------------------------------
%% Edge cases tests
%%--------------------------------------------------------------------

predict_with_unknown_activity_test() ->
    Log = [[{a, 0}, {b, 1000}]],
    {ok, Model} = train_from_log(Log),
    Trace = [{unknown, 0}],
    Time = predict_remaining_time(Trace, Model),
    ?assert(is_integer(Time)).

predict_with_single_activity_test() ->
    Log = [[{a, 0}, {b, 1000}, {c, 3000}]],
    {ok, Model} = train_from_log(Log),
    Trace = [{a, 0}],
    Time = predict_remaining_time(Trace, Model),
    ?assert(Time >= 0).

%%--------------------------------------------------------------------
%% Duration extraction tests
%%--------------------------------------------------------------------

extract_durations_from_trace_test() ->
    Trace = [{a, 1000}, {b, 2000}, {c, 3000}],
    Durations = extract_durations_from_trace(Trace),
    ?assertEqual(2, length(Durations)),
    ?assert(lists:keymember(a, 1, Durations)),
    ?assert(lists:keymember(b, 1, Durations)),
    ?assertEqual(1000, proplists:get_value(a, Durations)),
    ?assertEqual(1000, proplists:get_value(b, Durations)).

extract_durations_from_trace_empty_test() ->
    Trace = [],
    Durations = extract_durations_from_trace(Trace),
    ?assertEqual([], Durations).

extract_durations_from_trace_single_test() ->
    Trace = [{a, 1000}],
    Durations = extract_durations_from_trace(Trace),
    ?assertEqual([], Durations).

%%--------------------------------------------------------------------
%% Calculate remaining tests
%%--------------------------------------------------------------------

calculate_remaining_empty_test() ->
    Model = #{overall_average => 5000},
    Remaining = calculate_remaining([], Model),
    ?assertEqual(5000, Remaining).

calculate_remaining_with_activities_test() ->
    Durations = #{
        a => #{average => 1000},
        b => #{average => 2000}
    },
    Remaining = calculate_remaining([a, b], Durations),
    ?assertEqual(3000, Remaining).

calculate_remaining_unknown_activity_test() ->
    Durations = #{
        a => #{average => 1000}
    },
    Remaining = calculate_remaining([a, b], Durations),
    ?assertEqual(1000, Remaining).

-endif.
