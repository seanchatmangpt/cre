%% -*- erlang -*-
%% @doc ProReco Recommender Tests

-module(proreco_recommender_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include record definitions
-include("src/mining/proreco_recommender.erl").

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% Characterization tests
%%--------------------------------------------------------------------

extract_characteristics_simple_test() ->
    Log = #{traces => [[a, b, c], [a, b, c]]},
    Char = propreco_recommender:extract_characteristics(Log),
    ?assertEqual(2, Char#log_characteristics.trace_count),
    ?assertEqual(6, Char#log_characteristics.event_count).

extract_characteristics_parallel_test() ->
    Log = #{traces => [[a, b, c, d], [a, c, b, d]]},
    Char = propreco_recommender:extract_characteristics(Log),
    ?assert(Char#log_characteristics.concurrency > 0.0).

extract_characteristics_noisy_test() ->
    Log = #{traces => [[a, b, c], [a, x, b, c], [a, b, y, c]]},
    Char = propreco_recommender:extract_characteristics(Log),
    ?assert(Char#log_characteristics.noise > 0.0).

extract_characteristics_loop_test() ->
    Log = #{traces => [[a, b, a, b, c]]},
    Char = propreco_recommender:extract_characteristics(Log),
    ?assert(Char#log_characteristics.loops > 0.0).

%%--------------------------------------------------------------------
%% Recommendation tests
%%--------------------------------------------------------------------

recommend_algorithm_simple_test() ->
    Log = #{traces => [[a, b, c], [a, b, c]]},
    State = #state{
        transition_system = #{},
        algorithm_profiles = propreco_recommender:init_default_profiles(),
        recommendation_cache = #{},
        learning_data = #{},
        cache_size = 100
    },
    Result = propreco_recommender:generate_recommendation(
        propreco_recommender:extract_characteristics(Log), State
    ),
    ?assert(maps:is_key(algorithm, Result)),
    ?assert(maps:is_key(confidence, Result)),
    ?assert(is_list(maps:get(reasoning, Result))).

score_algorithm_test() ->
    Profile = #algorithm_profile{
        name = test,
        strengths = [parallel, loops],
        weaknesses = [noise],
        complexity = medium,
        scalability = medium,
        noise_tolerance = 0.2
    },
    Char = #log_characteristics{
        trace_count = 100,
        event_count = 500,
        unique_activities = 10,
        variability = 0.5,
        concurrency = 0.4,
        loops = 0.3,
        noise = 0.1
    },
    Score = propreco_recommender:score_algorithm(Profile, Char),
    ?assert(Score >= 0.0),
    ?assert(Score =< 2.0).

normalize_confidence_test() ->
    Scores = [{a, 1.5}, {b, 1.0}, {c, 0.5}],
    ?assert(propreco_recommender:normalize_confidence(1.5, Scores) >
            propreco_recommender:normalize_confidence(1.0, Scores)),
    ?assert(propreco_recommender:normalize_confidence(1.0, Scores) >
            propreco_recommender:normalize_confidence(0.5, Scores)).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

estimate_loops_test() ->
    ?assert(propreco_recommender:estimate_loops([[a, b, c]]) < 0.1),
    ?assert(propreco_recommender:estimate_loops([[a, b, a, b, c]]) > 0.1).

count_repeats_test() ->
    ?assertEqual(0.0, propreco_recommender:count_repeats([a, b, c])),
    ?assert(propreco_recommender:count_repeats([a, b, a]) > 0.0).

categorize_test() ->
    ?assertEqual(low, propreco_recommender:categorize_1(0.1)),
    ?assertEqual(medium, propreco_recommender:categorize_1(0.4)),
    ?assertEqual(high, propreco_recommender:categorize_1(0.8)).

%%--------------------------------------------------------------------
%% Record tests
%%--------------------------------------------------------------------

log_characteristics_record_test() ->
    Char = #log_characteristics{
        trace_count = 10,
        event_count = 100,
        unique_activities = 5,
        variability = 0.5,
        concurrency = 0.3,
        loops = 0.2,
        noise = 0.1
    },
    ?assertEqual(10, Char#log_characteristics.trace_count),
    ?assertEqual(100, Char#log_characteristics.event_count).

algorithm_profile_record_test() ->
    Profile = #algorithm_profile{
        name = test_algo,
        strengths = [simple],
        weaknesses = [noise],
        complexity = low,
        scalability = high,
        noise_tolerance = 0.2
    },
    ?assertEqual(test_algo, Profile#algorithm_profile.name),
    ?assertEqual(low, Profile#algorithm_profile.complexity).
