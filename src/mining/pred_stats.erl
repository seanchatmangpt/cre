%% -*- erlang -*-
%% @doc Statistical Fallback Models for Prediction
%%
%% Markov chains, exponential smoothing, and linear regression.
%%
%% @end

-module(pred_stats).

%% Markov chain
-export([fit_markov/1, predict_markov/2, get_transitions/1]).

%% Exponential smoothing
-export([fit_ema/2, predict_ema/2]).

%% Linear regression
-export([fit_linear/1, predict_linear/2]).

%% Records
-record(markov_model, {
    transitions :: map(),  %% {From, To} -> Probability
    activities :: [atom()]
}).

-record(ema_model, {
    alpha :: float(),
    values :: [float()],
    last_ema :: float()
}).

-record(linear_model, {
    slope :: float(),
    intercept :: float(),
    r_squared :: float()
}).

%% Types
-type markov_model() :: #markov_model{}.
-type ema_model() :: #ema_model{}.
-type linear_model() :: #linear_model{}.

-export_type([markov_model/0, ema_model/0, linear_model/0]).

%%====================================================================
%% Markov Chain
%%====================================================================

%% @doc Fit a Markov chain model from traces.
-spec fit_markov([[atom()]]) -> #markov_model{}.
fit_markov(Traces) ->
    %% Collect all transitions
    AllTransitions = lists:foldl(fun(Trace, Acc) ->
        extract_transitions(Trace, Acc)
    end, #{}, Traces),

    %% Calculate transition probabilities
    Transitions = calculate_transition_probs(AllTransitions),

    %% Collect unique activities
    Activities = lists:usort(lists:flatten(Traces)),

    #markov_model{
        transitions = Transitions,
        activities = Activities
    }.

%% @doc Predict next activities using Markov model.
-spec predict_markov([[atom()]], atom()) -> [{atom(), float()}].
predict_markov(Traces, LastActivity) when is_atom(LastActivity) ->
    Model = fit_markov(Traces),
    predict_markov(Model, LastActivity);
predict_markov(#markov_model{transitions = Transitions, activities = _Activities}, LastActivity) ->
    %% Get all transitions from LastActivity
    case maps:get(LastActivity, Transitions, undefined) of
        undefined -> [];
        Probs -> maps:to_list(Probs)
    end.

%% @doc Get transition matrix from model.
-spec get_transitions(#markov_model{}) -> map().
get_transitions(#markov_model{transitions = Transitions}) ->
    Transitions.

%% @private
-spec extract_transitions([atom()], map()) -> map().
extract_transitions([], Acc) ->
    Acc;
extract_transitions([_Single], Acc) ->
    Acc;
extract_transitions(Trace, Acc) ->
    extract_transitions(Trace, [], Acc).

%% @private
-spec extract_transitions([atom()], [atom()], map()) -> map().
extract_transitions([_Last], _Rev, Acc) ->
    Acc;
extract_transitions([From, To | Rest], Rev, Acc) ->
    Key = {From, To},
    NewAcc = maps:put(Key, maps:get(Key, Acc, 0) + 1, Acc),
    extract_transitions([To | Rest], [From | Rev], NewAcc).

%% @private
calculate_transition_probs(Transitions) ->
    %% Group by source activity
    BySource = lists:foldl(fun({{From, To}, Count}, Acc) ->
        Acc#{From => maps:put(To, Count, maps:get(From, Acc, #{}))}
    end, #{}, maps:to_list(Transitions)),

    %% Normalize to probabilities
    maps:map(fun(_From, ToMap) ->
        Total = lists:sum(maps:values(ToMap)),
        maps:map(fun(_To, Count) -> Count / Total end, ToMap)
    end, BySource).

%%====================================================================
%% Exponential Smoothing
%%====================================================================

%% @doc Fit EMA model to values.
-spec fit_ema([float()], float()) -> #ema_model{}.
fit_ema(Values, Alpha) when is_list(Values), is_float(Alpha), Alpha > 0, Alpha =< 1 ->
    %% Calculate initial EMA as mean
    Initial = lists:sum(Values) / length(Values),
    %% Smooth values
    {_, FinalEMA} = lists:foldl(fun(_V, {Idx, EMA}) ->
        NewEMA = Alpha * _V + (1 - Alpha) * EMA,
        {Idx + 1, NewEMA}
    end, {1, Initial}, Values),
    #ema_model{
        alpha = Alpha,
        values = Values,
        last_ema = FinalEMA
    }.

%% @doc Predict using EMA.
-spec predict_ema(#ema_model{}, integer()) -> float().
predict_ema(#ema_model{last_ema = EMA}, _StepsAhead) ->
    EMA.

%%====================================================================
%% Linear Regression
%%====================================================================

%% @doc Fit linear regression to values.
-spec fit_linear([float()]) -> #linear_model{}.
fit_linear([]) ->
    #linear_model{slope = 0.0, intercept = 0.0, r_squared = 0.0};
fit_linear(Values) when is_list(Values) ->
    N = length(Values),
    X = lists:seq(1, N),
    Y = Values,

    SumX = lists:sum(X),
    SumY = lists:sum(Y),
    SumXX = lists:sum([Xi * Xi || Xi <- X]),
    SumXY = lists:sum(lists:zipwith(fun(Xi, Yi) -> Xi * Yi end, X, Y)),

    Slope = case (N * SumXX - SumX * SumX) of
        0.0 -> 0.0;
        Denom when Denom =/= 0.0 -> (N * SumXY - SumX * SumY) / Denom;
        _ -> 0.0
    end,

    Intercept = (SumY - Slope * SumX) / N,

    %% Calculate R-squared
    case N of
        1 ->
            %% Perfect fit for single point
            #linear_model{slope = Slope, intercept = Intercept, r_squared = 1.0};
        _ ->
            YMean = lists:sum(Y) / N,
            SST = lists:sum([(Yi - YMean) * (Yi - YMean) || Yi <- Y]),
            SSR = case SST of
                0.0 -> 0.0;
                _ -> lists:sum([math:pow(Yi - (Intercept + Slope * Xi), 2) || {Yi, Xi} <- lists:zip(Y, X)])
            end,
            RSquared = case SST of
                0.0 -> 1.0;
                _ -> 1.0 - (SSR / SST)
            end,
            #linear_model{
                slope = Slope,
                intercept = Intercept,
                r_squared = RSquared
            }
    end.

%% @doc Predict using linear model.
-spec predict_linear(#linear_model{}, integer()) -> float().
predict_linear(#linear_model{slope = Slope, intercept = Intercept}, X) ->
    Slope * X + Intercept.
