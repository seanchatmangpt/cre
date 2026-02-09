%% -*- erlang -*-
%% @doc Test suite for recommender_simple module

-module(recommender_simple_tests).
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
    [[a, b, c, d],
     [a, b, c, d],
     [a, b, c, e, d],
     [a, b, f, d]].

parallel_log() ->
    [[a, b, c, d],
     [a, c, b, d],
     [a, b, c, d]].

complex_log() ->
    [[start, register, approve, complete],
     [start, register, review, approve, complete],
     [start, register, reject, complete],
     [start, verify, approve, complete]].

empty_log() ->
    [].

single_trace_log() ->
    [[a, b, c]].

%%====================================================================
%% Activity Counting Tests
%%====================================================================

count_activities_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Trace = [a, b, a, c],
         Counts = recommender_simple:count_activities(Trace),
         ?assertEqual(2, maps:get(a, Counts)),
         ?assertEqual(1, maps:get(b, Counts)),
         ?assertEqual(1, maps:get(c, Counts))
     end),

      ?_test(begin
         Trace = [],
         Counts = recommender_simple:count_activities(Trace),
         ?assertEqual(0, map_size(Counts))
     end),

      ?_test(begin
         Trace = [a, a, a, a],
         Counts = recommender_simple:count_activities(Trace),
         ?assertEqual(4, maps:get(a, Counts))
     end)
     ]}.

%%====================================================================
%% Vector Conversion Tests
%%====================================================================

trace_to_vector_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Trace = [a, b, c],
         AllActivities = sets:from_list([a, b, c, d]),
         Vector = recommender_simple:trace_to_vector(Trace, AllActivities),
         ?assertEqual(1, maps:get(a, Vector)),
         ?assertEqual(1, maps:get(b, Vector)),
         ?assertEqual(1, maps:get(c, Vector)),
         ?assertEqual(0, maps:get(d, Vector))
     end),

      ?_test(begin
         Trace = [a, b, a],
         AllActivities = sets:from_list([a, b]),
         Vector = recommender_simple:trace_to_vector(Trace, AllActivities),
         ?assertEqual(2, maps:get(a, Vector)),
         ?assertEqual(1, maps:get(b, Vector))
     end),

      ?_test(begin
         Trace = [],
         AllActivities = sets:from_list([a, b, c]),
         Vector = recommender_simple:trace_to_vector(Trace, AllActivities),
         ?assertEqual(0, maps:get(a, Vector)),
         ?assertEqual(0, maps:get(b, Vector)),
         ?assertEqual(0, maps:get(c, Vector))
     end)
     ]}.

%%====================================================================
%% Cosine Similarity Tests
%%====================================================================

cosine_similarity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Vec1 = #{a => 1, b => 1, c => 1},
         Vec2 = #{a => 1, b => 1, c => 1},
         Similarity = recommender_simple:cosine_similarity(Vec1, Vec2),
         ?assert(Similarity >= 0.999),
         ?assert(Similarity =< 1.001)
     end),

      ?_test(begin
         Vec1 = #{a => 1, b => 0},
         Vec2 = #{a => 0, b => 1},
         ?assertEqual(0.0, recommender_simple:cosine_similarity(Vec1, Vec2))
     end),

      ?_test(begin
         Vec1 = #{a => 1, b => 1},
         Vec2 = #{a => 1, b => 1, c => 1},
         Similarity = recommender_simple:cosine_similarity(Vec1, Vec2),
         ?assert(Similarity > 0.0),
         ?assert(Similarity < 1.0)
     end),

      ?_test(begin
         Vec1 = #{},
         Vec2 = #{a => 1},
         ?assertEqual(0.0, recommender_simple:cosine_similarity(Vec1, Vec2))
     end),

      ?_test(begin
         Vec1 = #{a => 2, b => 2},
         Vec2 = #{a => 1, b => 1},
         Similarity = recommender_simple:cosine_similarity(Vec1, Vec2),
         ?assert(Similarity >= 0.999),
         ?assert(Similarity =< 1.001)
     end)
     ]}.

%%====================================================================
%% Similar Traces Tests
%%====================================================================

find_similar_traces_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Trace = [a, b],
         Log = [[a, b, c], [x, y, z], [a, b]],
         AllActivities = sets:from_list([a, b, c, x, y, z]),
         TraceVector = recommender_simple:trace_to_vector(Trace, AllActivities),
         Similar = recommender_simple:find_similar_traces(TraceVector, Log, 0.1),
         ?assert(length(Similar) >= 1)
     end),

      ?_test(begin
         Trace = [],
         Log = [[a, b], [c, d]],
         AllActivities = sets:from_list([a, b, c, d]),
         TraceVector = recommender_simple:trace_to_vector(Trace, AllActivities),
         Similar = recommender_simple:find_similar_traces(TraceVector, Log, 0.5),
         ?assertEqual([], Similar)
     end),

      ?_test(begin
         Trace = [a, b, c],
         Log = [[a, b, c], [a, b, c], [x, y, z]],
         AllActivities = sets:from_list([a, b, c, x, y, z]),
         TraceVector = recommender_simple:trace_to_vector(Trace, AllActivities),
         Similar = recommender_simple:find_similar_traces(TraceVector, Log, 0.9),
         ?assertEqual(2, length(Similar))
     end)
     ]}.

%%====================================================================
%% Recommendation Tests
%%====================================================================

recommend_next_activity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Trace = [a, b, c],
         Log = simple_log(),
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         ?assert(is_list(Recommendations)),
         case Recommendations of
             [] -> ok;
             _ ->
                 {Activity, Score} = hd(Recommendations),
                 ?assert(is_atom(Activity)),
                 ?assert(is_float(Score)),
                 ?assert(Score >= 0.0),
                 ?assert(Score =< 1.0)
         end
     end),

      ?_test(begin
         Trace = [start, register],
         Log = complex_log(),
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         ?assert(is_list(Recommendations)),
         case Recommendations of
             [] -> ok;
             _ -> ?assert(length(Recommendations) > 0)
         end
     end),

      ?_test(begin
         Trace = [],
         Log = simple_log(),
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         ?assert(is_list(Recommendations))
     end),

      ?_test(begin
         Trace = [a, b],
         Log = empty_log(),
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         ?assertEqual([], Recommendations)
     end)
     ]}.

recommend_next_activities_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Trace = [a, b, c],
         Log = parallel_log(),
         Recommendations = recommender_simple:recommend_next_activities(Trace, Log),
         ?assert(is_list(Recommendations))
     end)
     ]}.

%%====================================================================
%% Score Validation Tests
%%====================================================================

score_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Trace = [a, b],
         Log = [[a, b, c], [a, b, d], [a, b, e]],
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         case Recommendations of
             [] -> ok;
             _ ->
                 Total = lists:sum([Score || {_Activity, Score} <- Recommendations]),
                 ?assert(Total >= 0.99),
                 ?assert(Total =< 1.01)
         end
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
         Trace = [a, b, a],
         Log = [[a, b, a, c], [a, b, a, c]],
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         ?assert(is_list(Recommendations))
     end),

      ?_test(begin
         Trace = [a, b, c, d],
         Log = [[a, b, c, d]],
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         ?assert(is_list(Recommendations))
     end),

      ?_test(begin
         Trace = [unknown_activity],
         Log = simple_log(),
         Recommendations = recommender_simple:recommend_next_activity(Trace, Log),
         ?assert(is_list(Recommendations))
     end)
     ]}.

%%====================================================================
%% Test Suite
%%====================================================================

recommender_simple_test_() ->
    [
     {"Activity counting tests", count_activities_test_()},
     {"Vector conversion tests", trace_to_vector_test_()},
     {"Cosine similarity tests", cosine_similarity_test_()},
     {"Similar traces tests", find_similar_traces_test_()},
     {"Recommendation tests", recommend_next_activity_test_()},
     {"Recommend next activities tests", recommend_next_activities_test_()},
     {"Score validation tests", score_validation_test_()},
     {"Edge cases tests", edge_cases_test_()}
    ].
