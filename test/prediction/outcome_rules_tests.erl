%% -*- erlang -*-
%% @doc Test suite for outcome_rules module

-module(outcome_rules_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Data
%%====================================================================

labeled_traces() ->
    [
        {[start, task1, task2, complete], success},
        {[start, task1, task2, complete], success},
        {[start, task1, error, abort], failure},
        {[start, task1, task2, task3, task4, task5, task6, task7, task8, task9, task10, task11], failure},
        {[start, finish], success}
    ].

success_traces() ->
    [[start, process, complete],
     [start, task1, task2, complete],
     [a, b, c]].

failure_traces() ->
    [[start, error, abort],
     [start, process, exception],
     [start, task1, task2, task3, task4, task5, task6, task7, task8, task9, task10, task11]].

mixed_traces() ->
    [
        {[start, task, complete], success},
        {[start, task, complete], success},
        {[start, error, stop], failure},
        {[start, task1, task2, task3, task4, task5, task6, task7, task8, task9, task10], failure}
    ].

%%====================================================================
%% Prediction Tests
%%====================================================================

predict_outcome_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Model = #{
            rules => outcome_rules:get_default_rules(),
            default_outcome => success,
            default_confidence => 0.5
         },
         Trace = [start, process, complete],
         {Outcome, Conf} = outcome_rules:predict_outcome(Trace, Model),
         ?assertEqual(success, Outcome),
         ?assert(Conf > 0.5)
     end),

      ?_test(begin
         Model = #{
            rules => outcome_rules:get_default_rules(),
            default_outcome => success,
            default_confidence => 0.5
         },
         Trace = [start, error, abort],
         {Outcome, _Conf} = outcome_rules:predict_outcome(Trace, Model),
         ?assertEqual(failure, Outcome)
     end),

      ?_test(begin
         Model = #{
            rules => outcome_rules:get_default_rules(),
            default_outcome => success,
            default_confidence => 0.5
         },
         LongTrace = lists:seq(1, 15),
         {Outcome, _Conf} = outcome_rules:predict_outcome(LongTrace, Model),
         ?assertEqual(failure, Outcome)
     end),

      ?_test(begin
         Model = #{
            rules => [],
            default_outcome => success,
            default_confidence => 0.7
         },
         {Outcome, Conf} = outcome_rules:predict_outcome([], Model),
         ?assertEqual(success, Outcome),
         ?assertEqual(0.7, Conf)
     end),

      ?_test(begin
         Trace = [a, b, c],
         {Outcome, Conf} = outcome_rules:predict_outcome(Trace, undefined),
         ?assertEqual(unknown, Outcome),
         ?assertEqual(0.0, Conf)
     end)
     ]}.

%%====================================================================
%% Training Tests
%%====================================================================

train_from_log_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Labeled = labeled_traces(),
         {ok, Model} = outcome_rules:train_from_log(Labeled),
         ?assert(is_map(Model)),
         ?assert(maps:is_key(rules, Model)),
         ?assert(maps:is_key(default_outcome, Model))
     end),

      ?_test(begin
         Labeled = [
             {[a, b], success},
             {[c, d], failure}
         ],
         {ok, #{default_outcome := Default}} = outcome_rules:train_from_log(Labeled),
         ?assertEqual(success, Default)
     end),

      ?_test(begin
         Labeled = [
             {[a], failure},
             {[b], failure},
             {[c], success}
         ],
         {ok, #{default_outcome := Default}} = outcome_rules:train_from_log(Labeled),
         ?assertEqual(failure, Default)
     end)
     ]}.

%%====================================================================
%% Rule Application Tests
%%====================================================================

apply_rules_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Rules = [
             #{name => test_rule1, predicate => fun(T) -> length(T) > 5 end, outcome => failure, weight => 0.8},
             #{name => test_rule2, predicate => fun(T) -> lists:member(complete, T) end, outcome => success, weight => 0.9}
         ],
         Trace = [start, task1, task2, task3, task4, task5, task6],
         Predictions = outcome_rules:apply_rules(Trace, Rules),
         ?assertEqual(1, length(Predictions)),
         ?assertEqual(failure, maps:get(outcome, hd(Predictions)))
     end),

      ?_test(begin
         Rules = [
             #{name => test_rule, predicate => fun(T) -> length(T) > 100 end, outcome => failure, weight => 0.8}
         ],
         Trace = [a, b, c],
         Predictions = outcome_rules:apply_rules(Trace, Rules),
         ?assertEqual([], Predictions)
     end),

      ?_test(begin
         Rules = [
             #{name => always_match, predicate => fun(_) -> true end, outcome => success, weight => 0.5},
             #{name => never_match, predicate => fun(_) -> false end, outcome => failure, weight => 0.9}
         ],
         Trace = [a, b],
         Predictions = outcome_rules:apply_rules(Trace, Rules),
         ?assertEqual(1, length(Predictions)),
         ?assertEqual(success, maps:get(outcome, hd(Predictions)))
     end)
     ]}.

%%====================================================================
%% Combine Predictions Tests
%%====================================================================

combine_predictions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Predictions = [
             #{outcome => success, confidence => 0.8},
             #{outcome => success, confidence => 0.6}
         ],
         {Outcome, Conf} = outcome_rules:combine_predictions(Predictions),
         ?assertEqual(success, Outcome),
         ?assert(Conf > 0.6)
     end),

      ?_test(begin
         Predictions = [
             #{outcome => failure, confidence => 0.9},
             #{outcome => failure, confidence => 0.7}
         ],
         {Outcome, Conf} = outcome_rules:combine_predictions(Predictions),
         ?assertEqual(failure, Outcome),
         ?assert(Conf > 0.7)
     end),

      ?_test(begin
         Predictions = [
             #{outcome => success, confidence => 0.8},
             #{outcome => failure, confidence => 0.3}
         ],
         {Outcome, _Conf} = outcome_rules:combine_predictions(Predictions),
         ?assertEqual(success, Outcome)
     end),

      ?_test(begin
         Predictions = [
             #{outcome => success, confidence => 0.5},
             #{outcome => failure, confidence => 0.5}
         ],
         {Outcome, Conf} = outcome_rules:combine_predictions(Predictions),
         ?assertEqual(success, Outcome),
         ?assertEqual(0.5, Conf)
     end),

      ?_test(begin
         Predictions = [],
         {Outcome, Conf} = outcome_rules:combine_predictions(Predictions),
         ?assertEqual(unknown, Outcome),
         ?assertEqual(0.0, Conf)
     end)
     ]}.

%%====================================================================
%% Helper Function Tests
%%====================================================================

helper_functions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         ?assert(outcome_rules:has_repeats([a, b, a])),
         ?assert(outcome_rules:has_repeats([a, a, b])),
         ?assertNot(outcome_rules:has_repeats([a, b, c])),
         ?assertNot(outcome_rules:has_repeats([])),
         ?assertNot(outcome_rules:has_repeats([a]))
     end),

      ?_test(begin
         Traces = [[a, b, c], [a, b], [c, d]],
         Counts = outcome_rules:count_activities_in_traces(Traces),
         ?assertEqual(2, maps:get(a, Counts)),
         ?assertEqual(2, maps:get(b, Counts)),
         ?assertEqual(2, maps:get(c, Counts)),
         ?assertEqual(1, maps:get(d, Counts))
     end)
     ]}.

%%====================================================================
%% Default Rules Tests
%%====================================================================

default_rules_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Rules = outcome_rules:get_default_rules(),
         ?assert(length(Rules) > 0)
     end),

      ?_test(begin
         Rules = outcome_rules:get_default_rules(),
         LongTrace = lists:seq(1, 15),
         LongRule = lists:keyfind(long_trace_failure, #{
             name => '_', predicate => '_', outcome => '_', weight => '_'
         }, Rules),
         #{predicate := Pred} = LongRule,
         ?assert(Pred(LongTrace))
     end),

      ?_test(begin
         Rules = outcome_rules:get_default_rules(),
         ErrorRule = lists:keyfind(error_activity_failure, #{
             name => '_', predicate => '_', outcome => '_', weight => '_'
         }, Rules),
         #{predicate := Pred} = ErrorRule,
         ?assert(Pred([start, error, stop])),
         ?assertNot(Pred([start, task, stop]))
     end),

      ?_test(begin
         Rules = outcome_rules:get_default_rules(),
         CompleteRule = lists:keyfind(complete_workflow_success, #{
             name => '_', predicate => '_', outcome => '_', weight => '_'
         }, Rules),
         #{predicate := Pred} = CompleteRule,
         ?assert(Pred([start, process, complete])),
         ?assert(Pred([start, finish])),
         ?assertNot(Pred([start, process]))
     end)
     ]}.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Model = #{
            rules => outcome_rules:get_default_rules(),
            default_outcome => success,
            default_confidence => 0.5
         },
         Trace = ['start', 'process', 'complete'],
         {Outcome, _Conf} = outcome_rules:predict_outcome(Trace, Model),
         ?assert(is_atom(Outcome))
     end),

      ?_test(begin
         Model = #{
            rules => outcome_rules:get_default_rules(),
            default_outcome => success,
            default_confidence => 0.5
         },
         Trace = [start],
         {Outcome, _Conf} = outcome_rules:predict_outcome(Trace, Model),
         ?assertEqual(success, Outcome)
     end),

      ?_test(begin
         Model = #{
            rules => outcome_rules:get_default_rules(),
            default_outcome => failure,
            default_confidence => 0.3
         },
         Trace = [a, b, c, d, e, f, g, h, i, j, k, l],
         {Outcome, _Conf} = outcome_rules:predict_outcome(Trace, Model),
         ?assertEqual(failure, Outcome)
     end)
     ]}.

%%====================================================================
%% Test Suite
%%====================================================================

outcome_rules_test_() ->
    [
     {"Prediction tests", predict_outcome_test_()},
     {"Training tests", train_from_log_test_()},
     {"Rule application tests", apply_rules_test_()},
     {"Combine predictions tests", combine_predictions_test_()},
     {"Helper function tests", helper_functions_test_()},
     {"Default rules tests", default_rules_test_()},
     {"Edge cases tests", edge_cases_test_()}
    ].
