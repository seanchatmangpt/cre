%% -*- erlang -*-
%% @doc Test suite for time_remaining module

-module(time_remaining_tests).
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

complex_log() ->
    [[{start, 0}, {register, 5000}, {approve, 15000}, {complete, 20000}],
     [{start, 0}, {register, 6000}, {review, 12000}, {approve, 18000}, {complete, 25000}],
     [{start, 0}, {verify, 3000}, {approve, 10000}, {complete, 15000}]].

%%====================================================================
%% Activity Duration Calculation Tests
%%====================================================================

calculate_activity_durations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = simple_log(),
         Durations = time_remaining:calculate_activity_durations(Log),
         AStats = maps:get(a, Durations),
         ?assertEqual(3, maps:get(count, AStats)),
         ?assert(maps:get(average, AStats) > 0)
     end),

      ?_test(begin
         Log = empty_log(),
         Durations = time_remaining:calculate_activity_durations(Log),
         ?assertEqual(0, map_size(Durations))
     end),

      ?_test(begin
         Log = variable_log(),
         Durations = time_remaining:calculate_activity_durations(Log),
         Task1Stats = maps:get(task1, Durations),
         ?assertEqual(3, maps:get(count, Task1Stats)),
         ?assert(maps:get(total, Task1Stats) > 0),
         ?assert(maps:get(average, Task1Stats) > 0),
         ?assert(maps:get(min, Task1Stats) =< maps:get(max, Task1Stats))
     end)
     ]}.

%%====================================================================
%% Overall Average Tests
%%====================================================================

calculate_overall_average_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Durations = #{
            a => #{count => 2, total => 2000, average => 1000, min => 500, max => 1500},
            b => #{count => 2, total => 4000, average => 2000, min => 1500, max => 2500}
         },
         Avg = time_remaining:calculate_overall_average(Durations),
         ?assertEqual(1500, Avg)
     end),

      ?_test(begin
         Avg = time_remaining:calculate_overall_average(#{}),
         ?assertEqual(0, Avg)
     end)
     ]}.

%%====================================================================
%% Linear Regression Tests
%%====================================================================

linear_regression_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = simple_log(),
         {Slope, Intercept} = time_remaining:linear_regression(Log),
         ?assert(is_float(Slope)),
         ?assert(is_float(Intercept))
     end),

      ?_test(begin
         Log = empty_log(),
         {Slope, Intercept} = time_remaining:linear_regression(Log),
         ?assertEqual(0.0, Slope),
         ?assertEqual(0.0, Intercept)
     end),

      ?_test(begin
         Log = single_trace_log(),
         {Slope, _Intercept} = time_remaining:linear_regression(Log),
         ?assert(is_float(Slope))
     end)
     ]}.

%%====================================================================
%% Prediction Tests
%%====================================================================

predict_remaining_time_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = simple_log(),
         {ok, Model} = time_remaining:train_from_log(Log),
         Trace = [{a, 1000}, {b, 2000}],
         Time = time_remaining:predict_remaining_time(Trace, Model),
         ?assert(is_integer(Time)),
         ?assert(Time >= 0)
     end),

      ?_test(begin
         Log = simple_log(),
         {ok, Model} = time_remaining:train_from_log(Log),
         Time = time_remaining:predict_remaining_time([], Model),
         ?assertEqual(0, Time)
     end),

      ?_test(begin
         Time = time_remaining:predict_remaining_time([{a, 1000}], undefined),
         ?assertEqual(0, Time)
     end),

      ?_test(begin
         Log = simple_log(),
         {ok, Model} = time_remaining:train_from_log(Log),
         Trace = [{a, 1000}, {b, 2000}, {c, 3000}],
         Time = time_remaining:predict_remaining_time(Trace, Model),
         ?assert(is_integer(Time))
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
         {ok, Model} = time_remaining:train_from_log(Log),
         ?assert(is_map(Model)),
         ?assert(maps:is_key(activity_durations, Model)),
         ?assert(maps:is_key(overall_average, Model)),
         ?assert(maps:is_key(regression_slope, Model)),
         ?assert(maps:is_key(regression_intercept, Model))
     end),

      ?_test(begin
         Log = empty_log(),
         {ok, Model} = time_remaining:train_from_log(Log),
         ?assert(is_map(Model)),
         ?assertEqual(0, maps:get(overall_average, Model))
     end),

      ?_test(begin
         Log = variable_log(),
         {ok, #{activity_durations := Durations}} = time_remaining:train_from_log(Log),
         ?assert(maps:is_key(start, Durations)),
         ?assert(maps:is_key(task1, Durations)),
         ?assert(maps:is_key(task2, Durations)),
         ?assert(maps:is_key(end_marker, Durations))
     end)
     ]}.

%%====================================================================
%% Duration Extraction Tests
%%====================================================================

extract_durations_from_trace_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Trace = [{a, 1000}, {b, 2000}, {c, 3000}],
         Durations = time_remaining:extract_durations_from_trace(Trace),
         ?assertEqual(2, length(Durations)),
         ?assert(lists:keymember(a, 1, Durations)),
         ?assert(lists:keymember(b, 1, Durations)),
         ?assertEqual(1000, proplists:get_value(a, Durations)),
         ?assertEqual(1000, proplists:get_value(b, Durations))
     end),

      ?_test(begin
         Trace = [],
         Durations = time_remaining:extract_durations_from_trace(Trace),
         ?assertEqual([], Durations)
     end),

      ?_test(begin
         Trace = [{a, 1000}],
         Durations = time_remaining:extract_durations_from_trace(Trace),
         ?assertEqual([], Durations)
     end)
     ]}.

%%====================================================================
%% Calculate Remaining Tests
%%====================================================================

calculate_remaining_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Model = #{overall_average => 5000},
         Remaining = time_remaining:calculate_remaining([], Model),
         ?assertEqual(5000, Remaining)
     end),

      ?_test(begin
         Durations = #{a => #{average => 1000}, b => #{average => 2000}},
         Remaining = time_remaining:calculate_remaining([a, b], Durations),
         ?assertEqual(3000, Remaining)
     end),

      ?_test(begin
         Durations = #{a => #{average => 1000}},
         Remaining = time_remaining:calculate_remaining([a, b], Durations),
         ?assertEqual(1000, Remaining)
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
         Log = [[{a, 0}, {b, 1000}]],
         {ok, Model} = time_remaining:train_from_log(Log),
         Trace = [{unknown, 0}],
         Time = time_remaining:predict_remaining_time(Trace, Model),
         ?assert(is_integer(Time))
     end),

      ?_test(begin
         Log = [[{a, 0}, {b, 1000}, {c, 3000}]],
         {ok, Model} = time_remaining:train_from_log(Log),
         Trace = [{a, 0}],
         Time = time_remaining:predict_remaining_time(Trace, Model),
         ?assert(Time >= 0)
     end),

      ?_test(begin
         Log = complex_log(),
         {ok, Model} = time_remaining:train_from_log(Log),
         Trace = [{start, 0}, {register, 5000}],
         Time = time_remaining:predict_remaining_time(Trace, Model),
         ?assert(is_integer(Time))
     end)
     ]}.

%%====================================================================
%% Complex Scenario Tests
%%====================================================================

complex_scenario_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = complex_log(),
         {ok, Model} = time_remaining:train_from_log(Log),
         #{activity_durations := Durations} = Model,

         RegisterStats = maps:get(register, Durations),
         ?assert(maps:get(count, RegisterStats) >= 1),
         ?assert(maps:get(average, RegisterStats) > 0),

         ApproveStats = maps:get(approve, Durations),
         ?assert(maps:get(count, ApproveStats) >= 1)
     end)
     ]}.

%%====================================================================
%% Test Suite
%%====================================================================

time_remaining_test_() ->
    [
     {"Activity duration calculation tests", calculate_activity_durations_test_()},
     {"Overall average tests", calculate_overall_average_test_()},
     {"Linear regression tests", linear_regression_test_()},
     {"Prediction tests", predict_remaining_time_test_()},
     {"Training tests", train_from_log_test_()},
     {"Duration extraction tests", extract_durations_from_trace_test_()},
     {"Calculate remaining tests", calculate_remaining_test_()},
     {"Edge cases tests", edge_cases_test_()},
     {"Complex scenario tests", complex_scenario_test_()}
    ].
