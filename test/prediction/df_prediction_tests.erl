%% -*- erlang -*-
%% @doc Test suite for df_prediction module

-module(df_prediction_tests).
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

simple_log() ->
    [[a, b, c],
     [a, b, c],
     [a, b, d]].

branching_log() ->
    [[a, b, c],
     [a, b, d],
     [a, b, c],
     [a, b, d],
     [a, b, e]].

complex_log() ->
    [[start, register, approve, complete],
     [start, register, reject, complete],
     [start, verify, approve, complete]].

empty_log() ->
    [].

single_trace_log() ->
    [[a, b, c]].

%%====================================================================
%% DF Matrix Building Tests
%%====================================================================

build_df_matrix_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = simple_log(),
         Matrix = df_prediction:build_df_matrix(Log),
         ?assertEqual(2, maps:get({a, b}, Matrix)),
         ?assertEqual(2, maps:get({b, c}, Matrix)),
         ?assertEqual(1, maps:get({b, d}, Matrix))
     end),

      ?_test(begin
         Log = empty_log(),
         Matrix = df_prediction:build_df_matrix(Log),
         ?assertEqual(0, map_size(Matrix))
     end),

      ?_test(begin
         Log = single_trace_log(),
         Matrix = df_prediction:build_df_matrix(Log),
         ?assertEqual(1, maps:get({a, b}, Matrix)),
         ?assertEqual(1, maps:get({b, c}, Matrix)),
         ?assertEqual(undefined, maps:get({c, a}, Matrix, undefined))
     end)
     ]}.

%%====================================================================
%% Find Followers Tests
%%====================================================================

find_followers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Matrix = #{{a, b} => 2, {a, c} => 1, {b, d} => 3},
         Followers = df_prediction:find_followers(a, Matrix),
         ?assertEqual(2, length(Followers)),
         ?assert(lists:keymember(b, 1, Followers)),
         ?assert(lists:keymember(c, 1, Followers))
     end),

      ?_test(begin
         Matrix = #{{a, b} => 2},
         Followers = df_prediction:find_followers(b, Matrix),
         ?assertEqual([], Followers)
     end),

      ?_test(begin
         Matrix = #{},
         Followers = df_prediction:find_followers(a, Matrix),
         ?assertEqual([], Followers)
     end)
     ]}.

%%====================================================================
%% Calculate Probabilities Tests
%%====================================================================

calculate_probabilities_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Followers = [{b, 2}, {c, 1}, {d, 1}],
         Probs = df_prediction:calculate_probabilities(Followers),
         ?assertEqual(3, length(Probs)),
         ?assertEqual(0.5, proplists:get_value(b, Probs)),
         ?assertEqual(0.25, proplists:get_value(c, Probs)),
         ?assertEqual(0.25, proplists:get_value(d, Probs))
     end),

      ?_test(begin
         Followers = [],
         Probs = df_prediction:calculate_probabilities(Followers),
         ?assertEqual([], Probs)
     end),

      ?_test(begin
         Followers = [{b, 5}],
         Probs = df_prediction:calculate_probabilities(Followers),
         ?assertEqual(1.0, proplists:get_value(b, Probs))
     end)
     ]}.

%%====================================================================
%% Prediction Tests
%%====================================================================

predict_next_activity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = simple_log(),
         {ok, Model} = df_prediction:train_from_log(Log),
         Predictions = df_prediction:predict_next_activity([a, b], Model),
         ?assert(length(Predictions) > 0),
         {Activity, Prob} = hd(Predictions),
         ?assert(is_atom(Activity)),
         ?assert(is_float(Prob)),
         ?assert(Prob > 0.0)
     end),

      ?_test(begin
         Log = branching_log(),
         {ok, Model} = df_prediction:train_from_log(Log),
         Predictions = df_prediction:predict_next_activity([a, b], Model),
         ?assertEqual(3, length(Predictions)),
         ?assert(lists:keymember(c, 1, Predictions)),
         ?assert(lists:keymember(d, 1, Predictions)),
         ?assert(lists:keymember(e, 1, Predictions))
     end),

      ?_test(begin
         Log = simple_log(),
         {ok, Model} = df_prediction:train_from_log(Log),
         Predictions = df_prediction:predict_next_activity([], Model),
         ?assertEqual([], Predictions)
     end),

      ?_test(begin
         Predictions = df_prediction:predict_next_activity([a, b], undefined),
         ?assertEqual([], Predictions)
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
         Log = simple_log(),
         {ok, Model} = df_prediction:train_from_log(Log),
         ?assert(is_map(Model)),
         ?assert(maps:is_key(df_matrix, Model)),
         ?assert(maps:is_key(total_transitions, Model)),
         ?assert(maps:is_key(activities, Model))
     end),

      ?_test(begin
         Log = simple_log(),
         {ok, #{total_transitions := Total}} = df_prediction:train_from_log(Log),
         ?assertEqual(5, Total)
     end),

      ?_test(begin
         Log = complex_log(),
         {ok, #{activities := Activities}} = df_prediction:train_from_log(Log),
         ?assert(sets:is_element(start, Activities)),
         ?assert(sets:is_element(register, Activities)),
         ?assert(sets:is_element(approve, Activities)),
         ?assert(sets:is_element(complete, Activities))
     end),

      ?_test(begin
         Log = empty_log(),
         {ok, Model} = df_prediction:train_from_log(Log),
         ?assert(is_map(Model))
     end)
     ]}.

%%====================================================================
%% Probability Sum Tests
%%====================================================================

probabilities_sum_to_one_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = branching_log(),
         {ok, Model} = df_prediction:train_from_log(Log),
         Predictions = df_prediction:predict_next_activity([a, b], Model),
         Total = lists:sum([P || {_A, P} <- Predictions]),
         ?assert(Total >= 0.99),
         ?assert(Total =< 1.01)
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
         Log = [[a, b, c]],
         {ok, Model} = df_prediction:train_from_log(Log),
         Predictions = df_prediction:predict_next_activity([unknown], Model),
         ?assertEqual([], Predictions)
     end),

      ?_test(begin
         Log = [[a, b, c], [x, y, z]],
         {ok, Model} = df_prediction:train_from_log(Log),
         Predictions = df_prediction:predict_next_activity([a], Model),
         ?assert(length(Predictions) > 0)
     end),

      ?_test(begin
         Log = [[a]],
         {ok, Model} = df_prediction:train_from_log(Log),
         Predictions = df_prediction:predict_next_activity([a], Model),
         ?assertEqual([], Predictions)
     end)
     ]}.

%%====================================================================
%% Test Suite
%%====================================================================

df_prediction_test_() ->
    [
     {"DF Matrix building tests", build_df_matrix_test_()},
     {"Find followers tests", find_followers_test_()},
     {"Calculate probabilities tests", calculate_probabilities_test_()},
     {"Prediction tests", predict_next_activity_test_()},
     {"Training tests", train_from_log_test_()},
     {"Probability sum tests", probabilities_sum_to_one_test_()},
     {"Edge cases tests", edge_cases_test_()}
    ].
