%% -*- erlang -*-
%% @doc Rule-based Outcome Prediction
%%
%% This module implements a decision tree-based outcome predictor
%% for process traces. It uses configurable rules to predict
%% whether a process will succeed or fail.
%%
%% The algorithm:
%% 1. Applies a series of rule-based checks to the trace
%% 2. Each rule produces an outcome with a confidence score
%% 3. Combines rule predictions using weighted voting
%%
%% @end

-module(outcome_rules).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([predict_outcome/1]).
-export([predict_outcome/2]).
-export([train_from_log/1]).
-export([add_rule/2]).
-export([get_model/0]).
-export([get_default_rules/0]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type trace() :: [activity()].
-type outcome() :: success | failure | unknown.
-type confidence() :: float().

-type rule() :: #{
    name => atom(),
    predicate => fun((trace()) -> boolean()),
    outcome => outcome(),
    weight => float()
}.

-type rule_prediction() :: #{outcome => outcome(), confidence => confidence()}.

-type model() :: #{
    rules => [rule()],
    default_outcome => outcome(),
    default_confidence => confidence()
}.

-export_type([outcome/0, confidence/0, model/0, rule/0]).

%% Server state
-record(state, {
    model :: model()
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

%% @doc Predicts the outcome of a trace using the current model.
%%
%% Returns {Outcome, Confidence} where Outcome is success | failure | unknown.
%%
-spec predict_outcome(trace()) -> {outcome(), confidence()}.
predict_outcome(Trace) ->
    predict_outcome(Trace, get_model()).

%% @doc Predicts outcome using a specific model.
%%
-spec predict_outcome(trace(), model() | undefined) -> {outcome(), confidence()}.
predict_outcome(_Trace, undefined) ->
    {unknown, 0.0};
predict_outcome([], #{default_outcome := Default, default_confidence := Conf}) ->
    {Default, Conf};
predict_outcome(Trace, #{rules := Rules} = Model) ->
    %% Apply all rules to the trace
    Predictions = apply_rules(Trace, Rules),

    case Predictions of
        [] ->
            #{default_outcome := Default, default_confidence := Conf} = Model,
            {Default, Conf};
        _ ->
            combine_predictions(Predictions)
    end.

%% @doc Trains an outcome prediction model from an event log.
%%
%% Analyzes traces to learn which patterns lead to success/failure.
%%
-spec train_from_log([{trace(), outcome()}]) -> {ok, model()}.
train_from_log(LabeledTraces) when is_list(LabeledTraces) ->
    %% Separate successful and failed traces
    {SuccessTraces, FailureTraces} = lists:partition(
        fun({_Trace, Outcome}) -> Outcome =:= success end,
        LabeledTraces
    ),

    %% Learn rules from the data
    Rules = learn_rules(SuccessTraces, FailureTraces),

    %% Set default based on majority class
    DefaultOutcome = case length(SuccessTraces) >= length(FailureTraces) of
        true -> success;
        false -> failure
    end,

    Model = #{
        rules => Rules,
        default_outcome => DefaultOutcome,
        default_confidence => 0.5
    },

    %% Update server state if running
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid -> gen_server:cast(?MODULE, {update_model, Model})
    end,

    {ok, Model}.

%% @doc Adds a custom rule to the current model.
%%
-spec add_rule(fun((trace()) -> boolean()), outcome()) -> ok.
add_rule(Predicate, Outcome) when is_function(Predicate, 1) ->
    Rule = #{
        name => list_to_atom("custom_" ++ integer_to_list(erlang:unique_integer([positive]))),
        predicate => Predicate,
        outcome => Outcome,
        weight => 1.0
    },
    gen_server:cast(?MODULE, {add_rule, Rule}),
    ok.

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
    %% Initialize with default rules
    DefaultRules = get_default_rules(),
    Model = #{
        rules => DefaultRules,
        default_outcome => success,
        default_confidence => 0.5
    },
    {ok, #state{model = Model}}.

handle_call(get_model, _From, State = #state{model = Model}) ->
    {reply, Model, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_model, Model}, State) ->
    {noreply, State#state{model = Model}};
handle_cast({add_rule, Rule}, State = #state{model = Model}) ->
    CurrentRules = maps:get(rules, Model, []),
    NewRules = [Rule | CurrentRules],
    NewModel = Model#{rules => NewRules},
    {noreply, State#state{model = NewModel}};
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
-spec get_default_rules() -> [rule()].
get_default_rules() ->
    [
        %% Rule: Long traces are more likely to fail
        #{
            name => long_trace_failure,
            predicate => fun(Trace) -> length(Trace) > 10 end,
            outcome => failure,
            weight => 0.7
        },
        %% Rule: Very short traces often succeed
        #{
            name => short_trace_success,
            predicate => fun(Trace) -> length(Trace) =< 3 end,
            outcome => success,
            weight => 0.6
        },
        %% Rule: Presence of error activities indicates failure
        #{
            name => error_activity_failure,
            predicate => fun(Trace) ->
                lists:any(fun(A) ->
                    lists:member(A, [error, exception, fail, abort])
                end, Trace)
            end,
            outcome => failure,
            weight => 0.9
        },
        %% Rule: Repeated activities might indicate retry/loop (potential failure)
        #{
            name => repeated_activity_warning,
            predicate => fun(Trace) ->
                has_repeats(Trace)
            end,
            outcome => failure,
            weight => 0.4
        },
        %% Rule: Complete workflow suggests success
        #{
            name => complete_workflow_success,
            predicate => fun(Trace) ->
                lists:member(complete, Trace) orelse lists:member(finish, Trace)
            end,
            outcome => success,
            weight => 0.8
        }
    ].

%% @private
-spec has_repeats(trace()) -> boolean().
has_repeats(Trace) ->
    TraceSet = sets:from_list(Trace),
    length(Trace) > sets:size(TraceSet).

%% @private
-spec apply_rules(trace(), [rule()]) -> [rule_prediction()].
apply_rules(Trace, Rules) ->
    lists:foldl(fun(#{predicate := Pred, outcome := Outcome, weight := Weight}, Acc) ->
        try
            case Pred(Trace) of
                true ->
                    [#{outcome => Outcome, confidence => Weight} | Acc];
                false ->
                    Acc
            end
        catch
            _:_ -> Acc  %% Ignore rule errors
        end
    end, [], Rules).

%% @private
-spec combine_predictions([rule_prediction()]) -> {outcome(), confidence()}.
combine_predictions(Predictions) ->
    %% Separate by outcome
    {SuccessScores, FailureScores} = lists:foldl(fun(Pred, {SuccAcc, FailAcc}) ->
        case Pred of
            #{outcome := success, confidence := Conf} ->
                {[Conf | SuccAcc], FailAcc};
            #{outcome := failure, confidence := Conf} ->
                {SuccAcc, [Conf | FailAcc]};
            _ ->
                {SuccAcc, FailAcc}
        end
    end, {[], []}, Predictions),

    SuccessTotal = lists:sum(SuccessScores),
    FailureTotal = lists:sum(FailureScores),
    Total = SuccessTotal + FailureTotal,

    if
        Total =:= 0 ->
            {unknown, 0.0};
        SuccessTotal > FailureTotal ->
            {success, SuccessTotal / Total};
        FailureTotal > SuccessTotal ->
            {failure, FailureTotal / Total};
        true ->
            %% Tie: return with lower confidence
            {success, 0.5}
    end.

%% @private
-spec learn_rules([trace()], [trace()]) -> [rule()].
learn_rules(SuccessTraces, FailureTraces) ->
    %% Start with default rules
    DefaultRules = get_default_rules(),

    %% Learn activity-based rules from the data
    ActivityRules = learn_activity_rules(SuccessTraces, FailureTraces),

    DefaultRules ++ ActivityRules.

%% @private
-spec learn_activity_rules([trace()], [trace()]) -> [rule()].
learn_activity_rules(SuccessTraces, FailureTraces) ->
    %% Find activities that are common in successful traces
    SuccessActivities = count_activities_in_traces(SuccessTraces),
    FailureActivities = count_activities_in_traces(FailureTraces),

    %% Find activities that appear more in success
    SuccessIndicators = find_discriminative_activities(
        SuccessActivities, FailureActivities, success
    ),

    %% Find activities that appear more in failure
    FailureIndicators = find_discriminative_activities(
        FailureActivities, SuccessActivities, failure
    ),

    %% Convert to rules
    lists:map(fun({Activity, Outcome}) ->
        #{
            name => list_to_atom(atom_to_list(Activity) ++ "_" ++ atom_to_list(Outcome)),
            predicate => fun(Trace) -> lists:member(Activity, Trace) end,
            outcome => Outcome,
            weight => 0.5
        }
    end, SuccessIndicators ++ FailureIndicators).

%% @private
-spec count_activities_in_traces([trace()]) -> #{activity() => pos_integer()}.
count_activities_in_traces(Traces) ->
    lists:foldl(fun(Trace, Acc) ->
        lists:foldl(fun(Activity, InnerAcc) ->
            InnerAcc#{Activity => maps:get(Activity, InnerAcc, 0) + 1}
        end, Acc, Trace)
    end, #{}, Traces).

%% @private
-spec find_discriminative_activities(#{activity() => pos_integer()},
                                     #{activity() => pos_integer()},
                                     outcome()) -> [{activity(), outcome()}].
find_discriminative_activities(CountAMap, CountBMap, Outcome) ->
    Threshold = 2,  %% Minimum occurrence to be considered

    maps:fold(fun(Activity, CountA, Acc) ->
        CountB = maps:get(Activity, CountBMap, 0),
        case CountA > CountB * 2 andalso CountA >= Threshold of
            true ->
                [{Activity, Outcome} | Acc];
            false ->
                Acc
        end
    end, [], CountAMap).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

labeled_traces() ->
    [
        {[[start, task1, task2, complete], success]},
        {[[start, task1, task2, complete], success]},
        {[[start, task1, error, abort], failure]},
        {[[start, task1, task2, task3, task4, task5, task6, task7, task8, task9, task10, task11], failure]},
        {[[start, finish], success]}
    ].

success_traces() ->
    [[start, process, complete],
     [start, task1, task2, complete],
     [a, b, c]].

failure_traces() ->
    [[start, error, abort],
     [start, process, exception],
     [start, task1, task2, task3, task4, task5, task6, task7, task8, task9, task10, task11]].

%%--------------------------------------------------------------------
%% Prediction tests
%%--------------------------------------------------------------------

predict_outcome_success_test() ->
    Model = #{
        rules => get_default_rules(),
        default_outcome => success,
        default_confidence => 0.5
    },
    Trace = [start, process, complete],
    {Outcome, Conf} = predict_outcome(Trace, Model),
    ?assertEqual(success, Outcome),
    ?assert(Conf > 0.5).

predict_outcome_failure_test() ->
    Model = #{
        rules => get_default_rules(),
        default_outcome => success,
        default_confidence => 0.5
    },
    Trace = [start, error, abort],
    {Outcome, _Conf} = predict_outcome(Trace, Model),
    ?assertEqual(failure, Outcome).

predict_outcome_long_trace_test() ->
    Model = #{
        rules => get_default_rules(),
        default_outcome => success,
        default_confidence => 0.5
    },
    LongTrace = lists:seq(1, 15),  %% 15 activities
    {Outcome, _Conf} = predict_outcome(LongTrace, Model),
    ?assertEqual(failure, Outcome).

predict_outcome_empty_trace_test() ->
    Model = #{
        rules => [],
        default_outcome => success,
        default_confidence => 0.7
    },
    {Outcome, Conf} = predict_outcome([], Model),
    ?assertEqual(success, Outcome),
    ?assertEqual(0.7, Conf).

predict_outcome_no_model_test() ->
    Trace = [a, b, c],
    {Outcome, Conf} = predict_outcome(Trace, undefined),
    ?assertEqual(unknown, Outcome),
    ?assertEqual(0.0, Conf).

%%--------------------------------------------------------------------
%% Training tests
%%--------------------------------------------------------------------

train_from_log_test() ->
    Labeled = labeled_traces(),
    {ok, Model} = train_from_log(Labeled),
    ?assert(is_map(Model)),
    ?assert(maps:is_key(rules, Model)),
    ?assert(maps:is_key(default_outcome, Model)).

train_from_log_balanced_test() ->
    Labeled = [
        {[a, b], success},
        {[c, d], failure}
    ],
    {ok, #{default_outcome := Default}} = train_from_log(Labeled),
    %% With equal counts, should prefer success
    ?assertEqual(success, Default).

train_from_log_failure_majority_test() ->
    Labeled = [
        {[a], failure},
        {[b], failure},
        {[c], success}
    ],
    {ok, #{default_outcome := Default}} = train_from_log(Labeled),
    ?assertEqual(failure, Default).

%%--------------------------------------------------------------------
%% Rule application tests
%%--------------------------------------------------------------------

apply_rules_test() ->
    Rules = [
        #{
            name => test_rule1,
            predicate => fun(T) -> length(T) > 5 end,
            outcome => failure,
            weight => 0.8
        },
        #{
            name => test_rule2,
            predicate => fun(T) -> lists:member(complete, T) end,
            outcome => success,
            weight => 0.9
        }
    ],
    Trace = [start, task1, task2, task3, task4, task5, task6],
    Predictions = apply_rules(Trace, Rules),
    ?assertEqual(1, length(Predictions)),
    ?assertEqual(failure, maps:get(outcome, hd(Predictions))).

apply_rules_no_match_test() ->
    Rules = [
        #{
            name => test_rule,
            predicate => fun(T) -> length(T) > 100 end,
            outcome => failure,
            weight => 0.8
        }
    ],
    Trace = [a, b, c],
    Predictions = apply_rules(Trace, Rules),
    ?assertEqual([], Predictions).

%%--------------------------------------------------------------------
%% Combine predictions tests
%%--------------------------------------------------------------------

combine_predictions_unanimous_success_test() ->
    Predictions = [
        #{outcome => success, confidence => 0.8},
        #{outcome => success, confidence => 0.6}
    ],
    {Outcome, Conf} = combine_predictions(Predictions),
    ?assertEqual(success, Outcome),
    ?assert(Conf > 0.6).

combine_predictions_unanimous_failure_test() ->
    Predictions = [
        #{outcome => failure, confidence => 0.9},
        #{outcome => failure, confidence => 0.7}
    ],
    {Outcome, Conf} = combine_predictions(Predictions),
    ?assertEqual(failure, Outcome),
    ?assert(Conf > 0.7).

combine_predictions_mixed_test() ->
    Predictions = [
        #{outcome => success, confidence => 0.8},
        #{outcome => failure, confidence => 0.3}
    ],
    {Outcome, _Conf} = combine_predictions(Predictions),
    ?assertEqual(success, Outcome).

combine_predictions_tie_test() ->
    Predictions = [
        #{outcome => success, confidence => 0.5},
        #{outcome => failure, confidence => 0.5}
    ],
    {Outcome, Conf} = combine_predictions(Predictions),
    ?assertEqual(success, Outcome),  %% Tiebreaker: success
    ?assertEqual(0.5, Conf).

combine_predictions_empty_test() ->
    Predictions = [],
    {Outcome, Conf} = combine_predictions(Predictions),
    ?assertEqual(unknown, Outcome),
    ?assertEqual(0.0, Conf).

%%--------------------------------------------------------------------
%% Helper function tests
%%--------------------------------------------------------------------

has_repeats_test() ->
    ?assert(has_repeats([a, b, a])),
    ?assert(has_repeats([a, a, b])),
    ?assertNot(has_repeats([a, b, c])),
    ?assertNot(has_repeats([])),
    ?assertNot(has_repeats([a])).

count_activities_in_traces_test() ->
    Traces = [[a, b, c], [a, b], [c, d]],
    Counts = count_activities_in_traces(Traces),
    ?assertEqual(2, maps:get(a, Counts)),
    ?assertEqual(2, maps:get(b, Counts)),
    ?assertEqual(2, maps:get(c, Counts)),
    ?assertEqual(1, maps:get(d, Counts)).

%%--------------------------------------------------------------------
%% Default rules tests
%%--------------------------------------------------------------------

default_rules_long_trace_test() ->
    Rules = get_default_rules(),
    LongTrace = lists:seq(1, 15),
    LongRule = lists:keyfind(long_trace_failure, #{
        name => '_',
        predicate => '_',
        outcome => '_',
        weight => '_'
    }, Rules),
    #{predicate := Pred} = LongRule,
    ?assert(Pred(LongTrace)).

default_rules_error_activity_test() ->
    Rules = get_default_rules(),
    ErrorRule = lists:keyfind(error_activity_failure, #{
        name => '_',
        predicate => '_',
        outcome => '_',
        weight => '_'
    }, Rules),
    #{predicate := Pred} = ErrorRule,
    ?assert(Pred([start, error, stop])),
    ?assertNot(Pred([start, task, stop])).

%%--------------------------------------------------------------------
%% Edge cases tests
%%--------------------------------------------------------------------

predict_with_special_atoms_test() ->
    Model = #{
        rules => get_default_rules(),
        default_outcome => success,
        default_confidence => 0.5
    },
    Trace = ['start', 'process', 'complete'],
    {Outcome, _Conf} = predict_outcome(Trace, Model),
    ?assert(is_atom(Outcome)).

predict_with_single_activity_test() ->
    Model = #{
        rules => get_default_rules(),
        default_outcome => success,
        default_confidence => 0.5
    },
    Trace = [start],
    {Outcome, _Conf} = predict_outcome(Trace, Model),
    ?assertEqual(success, Outcome).

-endif.
