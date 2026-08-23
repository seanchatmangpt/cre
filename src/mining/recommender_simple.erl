%% -*- erlang -*-
%% @doc Simple Recommender System for Process Mining
%%
%% This module implements a user-based collaborative filtering recommender
%% system for suggesting next activities in a process based on similar traces.
%%
%% The algorithm:
%% 1. Represents traces as feature vectors (activity presence/counts)
%% 2. Computes cosine similarity between traces
%% 3. Recommends activities from similar traces
%%
%% @end

-module(recommender_simple).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([recommend_next_activity/2]).
-export([recommend_next_activities/2]).

%% Utility functions for testing
-export([cosine_similarity/2]).
-export([trace_to_vector/2]).
-export([find_similar_traces/3]).
-export([count_activities/1]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type trace() :: [activity()].
-type event_log() :: [trace()].
-type activity_vector() :: #{activity() => number()}.
-type similarity_score() :: float().

-export_type([event_log/0, similarity_score/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Recommends the next activity for a given trace.
%%
%% Uses collaborative filtering to find similar traces and suggests
%% the most likely next activity based on historical patterns.
%%
%% Returns {Activity, Score} pairs sorted by score (descending).
%%
-spec recommend_next_activity(trace(), event_log()) -> [{activity(), float()}].
recommend_next_activity(Trace, Log) when is_list(Trace), is_list(Log) ->
    %% Get all unique activities from the log
    AllActivities = extract_all_activities(Log),

    %% Convert trace to vector
    TraceVector = trace_to_vector(Trace, AllActivities),

    %% Find similar traces
    SimilarTraces = find_similar_traces(TraceVector, Log, 0.0),

    case SimilarTraces of
        [] ->
            %% No similar traces, return empty
            [];
        _ ->
            %% Extract next activities from similar traces
            NextActivities = extract_next_activities(Trace, SimilarTraces),

            %% Score by similarity-weighted frequency
            Scored = score_activities(NextActivities, SimilarTraces, AllActivities),

            %% Sort by score descending
            lists:reverse(lists:keysort(2, Scored))
    end.

%% @doc Recommends multiple next activities with scores.
%%
%% Same as recommend_next_activity/2 but returns all scored candidates.
%%
-spec recommend_next_activities(trace(), event_log()) -> [{activity(), float()}].
recommend_next_activities(Trace, Log) ->
    recommend_next_activity(Trace, Log).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec extract_all_activities(event_log()) -> sets:set(activity()).
extract_all_activities(Log) ->
    lists:foldl(fun(Trace, Acc) ->
        lists:foldl(fun(Activity, Set) ->
            sets:add_element(Activity, Set)
        end, Acc, Trace)
    end, sets:new(), Log).

%% @private
-spec trace_to_vector(trace(), sets:set(activity())) -> activity_vector().
trace_to_vector(Trace, AllActivities) ->
    %% Count activity occurrences
    Counts = count_activities(Trace),

    %% Build vector with zero for missing activities
    lists:foldl(fun(Activity, Acc) ->
        Acc#{Activity => maps:get(Activity, Counts, 0)}
    end, #{}, sets:to_list(AllActivities)).

%% @private
-spec count_activities(trace()) -> activity_vector().
count_activities(Trace) ->
    lists:foldl(fun(Activity, Acc) ->
        Acc#{Activity => maps:get(Activity, Acc, 0) + 1}
    end, #{}, Trace).

%% @doc Computes cosine similarity between two activity vectors.
%%
%% Cosine similarity = (A . B) / (||A|| * ||B||)
%% Returns a value between 0.0 (no similarity) and 1.0 (identical).
%%
-spec cosine_similarity(activity_vector(), activity_vector()) -> similarity_score().
cosine_similarity(Vec1, Vec2) ->
    %% Calculate dot product
    DotProduct = maps:fold(fun(Activity, Count1, Acc) ->
        Count2 = maps:get(Activity, Vec2, 0),
        Acc + Count1 * Count2
    end, 0, Vec1),

    %% Calculate magnitudes
    Mag1 = math:sqrt(maps:fold(fun(_Activity, Count, Acc) ->
        Acc + Count * Count
    end, 0, Vec1)),

    Mag2 = math:sqrt(maps:fold(fun(_Activity, Count, Acc) ->
        Acc + Count * Count
    end, 0, Vec2)),

    %% Avoid division by zero
    case Mag1 * Mag2 of
        0.0 -> 0.0;
        Product -> DotProduct / Product
    end.

%% @doc Finds traces similar to a given trace vector.
%%
%% Returns traces with similarity above the threshold.
%%
-spec find_similar_traces(activity_vector(), event_log(), float()) ->
    [{trace(), similarity_score()}].
find_similar_traces(TraceVector, Log, Threshold) ->
    AllActivities = maps:keys(TraceVector),

    lists:foldl(fun(CandidateTrace, Acc) ->
        CandidateVector = trace_to_vector(CandidateTrace, sets:from_list(AllActivities)),
        Similarity = cosine_similarity(TraceVector, CandidateVector),

        case Similarity > Threshold of
            true -> [{CandidateTrace, Similarity} | Acc];
            false -> Acc
        end
    end, [], Log).

%% @private
-spec extract_next_activities(trace(), [{trace(), similarity_score()}]) ->
    [{activity(), similarity_score()}].
extract_next_activities(CurrentTrace, SimilarTraces) ->
    %% Find the position in the current trace
    CurrentLength = length(CurrentTrace),

    %% For each similar trace, find the activity at the next position
    lists:foldl(fun({SimilarTrace, Similarity}, Acc) ->
        case length(SimilarTrace) > CurrentLength of
            true ->
                NextActivity = lists:nth(CurrentLength + 1, SimilarTrace),
                [{NextActivity, Similarity} | Acc];
            false ->
                Acc
        end
    end, [], SimilarTraces).

%% @private
-spec score_activities([{activity(), similarity_score()}],
                       [{trace(), similarity_score()}],
                       sets:set(activity())) -> [{activity(), float()}].
score_activities(NextActivities, SimilarTraces, AllActivities) ->
    %% Aggregate scores by activity
    Scored = lists:foldl(fun({Activity, Similarity}, Acc) ->
        CurrentScore = maps:get(Activity, Acc, 0.0),
        Acc#{Activity => CurrentScore + Similarity}
    end, #{}, NextActivities),

    %% Normalize to get probabilities
    TotalScore = lists:sum(maps:values(Scored)),

    case TotalScore of
        0.0 ->
            %% Return all activities with equal probability
            lists:map(fun(A) -> {A, 1.0 / sets:size(AllActivities)} end,
                      sets:to_list(AllActivities));
        _ ->
            maps:fold(fun(Activity, Score, Acc) ->
                [{Activity, Score / TotalScore} | Acc]
            end, [], Scored)
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% Activity counting tests
%%--------------------------------------------------------------------

count_activities_test() ->
    Trace = [a, b, a, c],
    Counts = count_activities(Trace),
    ?assertEqual(2, maps:get(a, Counts)),
    ?assertEqual(1, maps:get(b, Counts)),
    ?assertEqual(1, maps:get(c, Counts)).

count_activities_empty_test() ->
    Trace = [],
    Counts = count_activities(Trace),
    ?assertEqual(0, map_size(Counts)).

%%--------------------------------------------------------------------
%% Vector conversion tests
%%--------------------------------------------------------------------

trace_to_vector_test() ->
    Trace = [a, b, c],
    AllActivities = sets:from_list([a, b, c, d]),
    Vector = trace_to_vector(Trace, AllActivities),
    ?assertEqual(1, maps:get(a, Vector)),
    ?assertEqual(1, maps:get(b, Vector)),
    ?assertEqual(1, maps:get(c, Vector)),
    ?assertEqual(0, maps:get(d, Vector)).

trace_to_vector_duplicates_test() ->
    Trace = [a, b, a],
    AllActivities = sets:from_list([a, b]),
    Vector = trace_to_vector(Trace, AllActivities),
    ?assertEqual(2, maps:get(a, Vector)),
    ?assertEqual(1, maps:get(b, Vector)).

%%--------------------------------------------------------------------
%% Cosine similarity tests
%%--------------------------------------------------------------------

cosine_similarity_identical_test() ->
    Vec1 = #{a => 1, b => 1, c => 1},
    Vec2 = #{a => 1, b => 1, c => 1},
    ?assertEqual(1.0, cosine_similarity(Vec1, Vec2)).

cosine_similarity_orthogonal_test() ->
    Vec1 = #{a => 1, b => 0},
    Vec2 = #{a => 0, b => 1},
    ?assertEqual(0.0, cosine_similarity(Vec1, Vec2)).

cosine_similarity_partial_test() ->
    Vec1 = #{a => 1, b => 1},
    Vec2 = #{a => 1, b => 1, c => 1},
    Similarity = cosine_similarity(Vec1, Vec2),
    ?assert(Similarity > 0.0),
    ?assert(Similarity < 1.0).

cosine_similarity_zero_test() ->
    Vec1 = #{},
    Vec2 = #{a => 1},
    ?assertEqual(0.0, cosine_similarity(Vec1, Vec2)).

%%--------------------------------------------------------------------
%% Similar traces tests
%%--------------------------------------------------------------------

find_similar_traces_test() ->
    Trace = [a, b],
    Log = [[a, b, c], [x, y, z], [a, b]],
    AllActivities = sets:from_list([a, b, c, x, y, z]),
    TraceVector = trace_to_vector(Trace, AllActivities),

    Similar = find_similar_traces(TraceVector, Log, 0.1),
    ?assert(length(Similar) >= 1).

find_similar_traces_empty_test() ->
    Trace = [],
    Log = [[a, b], [c, d]],
    AllActivities = sets:from_list([a, b, c, d]),
    TraceVector = trace_to_vector(Trace, AllActivities),

    Similar = find_similar_traces(TraceVector, Log, 0.5),
    ?assertEqual([], Similar).

%%--------------------------------------------------------------------
%% Recommendation tests
%%--------------------------------------------------------------------

recommend_next_activity_test() ->
    Trace = [a, b, c],
    Log = simple_log(),
    Recommendations = recommend_next_activity(Trace, Log),
    ?assert(is_list(Recommendations)),
    case Recommendations of
        [] -> ok;
        _ ->
            ?assert(length(hd(Recommendations)) =:= 2),
            {Activity, Score} = hd(Recommendations),
            ?assert(is_atom(Activity)),
            ?assert(is_float(Score)),
            ?assert(Score >= 0.0),
            ?assert(Score =< 1.0)
    end.

recommend_next_activity_partial_test() ->
    Trace = [start, register],
    Log = complex_log(),
    Recommendations = recommend_next_activity(Trace, Log),
    ?assert(is_list(Recommendations)),
    case Recommendations of
        [] -> ok;
        _ -> ?assert(length(Recommendations) > 0)
    end.

recommend_next_activity_empty_trace_test() ->
    Trace = [],
    Log = simple_log(),
    Recommendations = recommend_next_activity(Trace, Log),
    ?assert(is_list(Recommendations)).

recommend_next_activity_empty_log_test() ->
    Trace = [a, b],
    Log = [],
    Recommendations = recommend_next_activity(Trace, Log),
    ?assertEqual([], Recommendations).

recommend_next_activities_test() ->
    Trace = [a, b, c],
    Log = parallel_log(),
    Recommendations = recommend_next_activities(Trace, Log),
    ?assert(is_list(Recommendations)).

%%--------------------------------------------------------------------
%% Edge cases tests
%%--------------------------------------------------------------------

recommend_with_duplicates_test() ->
    Trace = [a, b, a],
    Log = [[a, b, a, c], [a, b, a, c]],
    Recommendations = recommend_next_activity(Trace, Log),
    ?assert(is_list(Recommendations)).

recommend_with_no_next_test() ->
    Trace = [a, b, c, d],
    Log = [[a, b, c, d]],
    Recommendations = recommend_next_activity(Trace, Log),
    %% No next activity available
    ?assert(is_list(Recommendations)).

%%--------------------------------------------------------------------
%% Score validation tests
%%--------------------------------------------------------------------

recommend_scores_sum_to_one_test() ->
    Trace = [a, b],
    Log = [[a, b, c], [a, b, d], [a, b, e]],
    Recommendations = recommend_next_activity(Trace, Log),

    case Recommendations of
        [] -> ok;
        _ ->
            Total = lists:sum([Score || {_Activity, Score} <- Recommendations]),
            ?assert(Total >= 0.99),
            ?assert(Total =< 1.01)
    end.

-endif.
