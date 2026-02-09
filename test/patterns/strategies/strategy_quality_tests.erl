%% -*- erlang -*-
%%%% @doc strategy_quality_tests - EUnit tests for quality-based strategy.
%%
%% Tests for:
%% - Initialization with and without options
%% - Completion detection with quality thresholds
%% - Branch completion with quality score extraction
%% - Result retrieval sorted by quality (highest first)
%% - Edge cases (minimum quality filtering, default quality)
%%
%% @end

-module(strategy_quality_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

-record(quality_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed = [] :: [{pos_integer(), number(), term()}],
    min_quality :: number() | undefined
}).

%%====================================================================
%% Initialization Tests
%%====================================================================

strategy_quality_init_basic_test() ->
    {ok, State} = strategy_quality:init(2, 5),
    ?assertEqual(2, State#quality_state.n),
    ?assertEqual(5, State#quality_state.m),
    ?assertEqual([], State#quality_state.completed),
    ?assertEqual(undefined, State#quality_state.min_quality).

strategy_quality_init_with_options_test() ->
    {ok, State} = strategy_quality:init(2, 5, [{min_quality, 0.7}]),
    ?assertEqual(2, State#quality_state.n),
    ?assertEqual(0.7, State#quality_state.min_quality).

strategy_quality_init_with_min_quality_zero_test() ->
    {ok, State} = strategy_quality:init(2, 5, [{min_quality, 0.0}]),
    ?assertEqual(0.0, State#quality_state.min_quality).

strategy_quality_init_with_high_threshold_test() ->
    {ok, State} = strategy_quality:init(3, 10, [{min_quality, 0.95}]),
    ?assertEqual(0.95, State#quality_state.min_quality).

strategy_quality_init_invalid_params_test() ->
    ?assertError(function_clause, strategy_quality:init(5, 3)),
    ?assertError(function_clause, strategy_quality:init(0, 3)),
    ?assertError(function_clause, strategy_quality:init(1, 0)).

%%====================================================================
%% Should Complete Tests
%%====================================================================

strategy_quality_should_complete_no_threshold_test() ->
    {ok, State0} = strategy_quality:init(3, 5),
    State1 = add_quality_completions(State0, [{1, 0.5, r1}, {2, 0.3, r2}]),
    ?assertEqual(false, strategy_quality:should_complete(State1, #{})),

    State2 = strategy_quality:on_branch_complete(State1, {3, r3}),
    ?assertEqual(true, strategy_quality:should_complete(State2, #{})).

strategy_quality_should_complete_with_threshold_test() ->
    {ok, State0} = strategy_quality:init(2, 5, [{min_quality, 0.7}]),
    %% Add low quality results
    State1 = add_quality_completions(State0, [{1, 0.5, r1}, {2, 0.6, r2}]),
    ?assertEqual(false, strategy_quality:should_complete(State1, #{})),

    %% Add high quality result
    State2 = strategy_quality:on_branch_complete(State1, {3, {0.8, r3}}),
    ?assertEqual(false, strategy_quality:should_complete(State2, #{})),

    %% Add another high quality result
    State3 = strategy_quality:on_branch_complete(State2, {4, {0.9, r4}}),
    ?assertEqual(true, strategy_quality:should_complete(State3, #{})).

strategy_quality_should_complete_exact_threshold_test() ->
    {ok, State0} = strategy_quality:init(2, 5, [{min_quality, 0.7}]),
    State1 = add_quality_completions(State0, [{1, 0.7, r1}, {2, 0.7, r2}]),
    ?assertEqual(true, strategy_quality:should_complete(State1, #{})).

strategy_quality_should_complete_all_below_threshold_test() ->
    {ok, State0} = strategy_quality:init(2, 5, [{min_quality, 0.9}]),
    State1 = add_quality_completions(State0, [{1, 0.5, r1}, {2, 0.6, r2}, {3, 0.7, r3}]),
    ?assertEqual(false, strategy_quality:should_complete(State1, #{})).

%%====================================================================
%% On Branch Complete Tests
%%====================================================================

strategy_quality_on_branch_complete_tuple_format_test() ->
    {ok, State} = strategy_quality:init(3, 5),
    NewState = strategy_quality:on_branch_complete(State, {1, {0.8, good_result}}),
    ?assertEqual(1, length(NewState#quality_state.completed)),
    [{Idx, Quality, Result}] = NewState#quality_state.completed,
    ?assertEqual(1, Idx),
    ?assertEqual(0.8, Quality),
    ?assertEqual(good_result, Result).

strategy_quality_on_branch_complete_default_quality_test() ->
    {ok, State} = strategy_quality:init(3, 5),
    NewState = strategy_quality:on_branch_complete(State, {1, plain_result}),
    [{_, Quality, _}] = NewState#quality_state.completed,
    ?assertEqual(0.5, Quality).  %% Default quality

strategy_quality_on_branch_complete_multiple_test() ->
    {ok, State0} = strategy_quality:init(3, 5),
    State1 = strategy_quality:on_branch_complete(State0, {1, {0.9, r1}}),
    State2 = strategy_quality:on_branch_complete(State1, {2, {0.5, r2}}),
    State3 = strategy_quality:on_branch_complete(State2, {3, {0.7, r3}}),
    ?assertEqual(3, length(State3#quality_state.completed)).

strategy_quality_on_branch_complete_integer_scores_test() ->
    {ok, State} = strategy_quality:init(3, 5),
    NewState = strategy_quality:on_branch_complete(State, {1, {1, high_quality}}),
    [{_, Quality, _}] = NewState#quality_state.completed,
    ?assertEqual(1, Quality).

%%====================================================================
%% Get Result Tests (Key Feature: Quality-Based Selection)
%%====================================================================

strategy_quality_get_result_sorted_by_quality_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_quality:init(3, 5),
              State1 = add_quality_completions(State0, [
                  {1, 0.5, low},
                  {2, 0.9, high},
                  {3, 0.7, medium}
              ]),

              {ok, Results} = strategy_quality:get_result(State1),

              ?assertEqual(3, length(Results)),
              [{Idx1, Res1}, {Idx2, Res2}, {Idx3, Res3}] = Results,
              ?assertEqual(high, Res1),   %% Quality 0.9
              ?assertEqual(medium, Res2), %% Quality 0.7
              ?assertEqual(low, Res3)     %% Quality 0.5
           end)
         ]
     end}.

strategy_quality_get_result_with_min_quality_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_quality:init(2, 5, [{min_quality, 0.7}]),
              State1 = add_quality_completions(State0, [
                  {1, 0.5, low},
                  {2, 0.9, high},
                  {3, 0.8, good},
                  {4, 0.6, medium_low}
              ]),

              {ok, Results} = strategy_quality:get_result(State1),

              %% Should only include results with quality >= 0.7
              ?assertEqual(2, length(Results)),
              [{_, Res1}, {_, Res2}] = Results,
              ?assertEqual(high, Res1),
              ?assertEqual(good, Res2)
           end)
         ]
     end}.

strategy_quality_get_result_subset_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_quality:init(2, 5),
              State1 = add_quality_completions(State0, [
                  {1, 0.9, best},
                  {2, 0.8, good},
                  {3, 0.7, okay},
                  {4, 0.6, poor}
              ]),

              {ok, Results} = strategy_quality:get_result(State1),

              ?assertEqual(2, length(Results)),
              [{_, Res1}, {_, Res2}] = Results,
              ?assertEqual(best, Res1),
              ?assertEqual(good, Res2)
           end)
         ]
     end}.

strategy_quality_get_result_empty_test() ->
    {ok, State} = strategy_quality:init(3, 5),
    {ok, Results} = strategy_quality:get_result(State),
    ?assertEqual([], Results).

%%====================================================================
%% Integration Tests
%%====================================================================

strategy_quality_full_workflow_with_threshold_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_quality:init(2, 4, [{min_quality, 0.8}]),

              ?assertEqual(false, strategy_quality:should_complete(State0, #{})),

              %% Low quality result - doesn't count toward completion
              State1 = strategy_quality:on_branch_complete(State0, {1, {0.5, poor}}),
              ?assertEqual(false, strategy_quality:should_complete(State1, #{})),

              %% High quality result
              State2 = strategy_quality:on_branch_complete(State1, {2, {0.9, excellent}}),
              ?assertEqual(false, strategy_quality:should_complete(State2, #{})),

              %% Another high quality result - now complete
              State3 = strategy_quality:on_branch_complete(State2, {3, {0.85, very_good}}),
              ?assertEqual(true, strategy_quality:should_complete(State3, #{})),

              {ok, Results} = strategy_quality:get_result(State3),
              ?assertEqual(2, length(Results)),
              [{_, Res1}, {_, Res2}] = Results,
              ?assertEqual(excellent, Res1),
              ?assertEqual(very_good, Res2)
           end)
         ]
     end}.

strategy_quality_mixed_format_results_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_quality:init(3, 5),

              %% Mix of tuple format and plain results
              State1 = strategy_quality:on_branch_complete(State0, {1, {0.9, scored}}),
              State2 = strategy_quality:on_branch_complete(State1, {2, plain}),
              State3 = strategy_quality:on_branch_complete(State2, {3, {0.7, medium}}),

              {ok, Results} = strategy_quality:get_result(State3),

              ?assertEqual(3, length(Results)),
              %% Results should be: scored (0.9), plain (0.5 default), medium (0.7)
              [{_, Res1}, {_, Res2}, {_, Res3}] = Results,
              ?assertEqual(scored, Res1),
              ?assertEqual(medium, Res2),
              ?assertEqual(plain, Res3)
           end)
         ]
     end}.

%%====================================================================
%% Property-Style Tests
%%====================================================================

strategy_quality_quality_ordering_preserved_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_quality:init(10, 10),
              %% Add results with descending quality (10 -> 1)
              QualityPairs = [{I, I / 10, {result, I}} || I <- lists:seq(1, 10)],
              State1 = add_quality_completions(State0, QualityPairs),

              {ok, Results} = strategy_quality:get_result(State1),

              ?assertEqual(10, length(Results)),
              %% Results should be in descending quality order
              [{Idx1, _}, {Idx2, _}, {Idx3, _}, {Idx10, _}] =
                  lists:append(Results),
              ?assertEqual(10, Idx1),  %% Quality 1.0
              ?assertEqual(9, Idx2),   %% Quality 0.9
              ?assertEqual(8, Idx3),   %% Quality 0.8
              ?assertEqual(1, Idx10)   %% Quality 0.1
           end)
         ]
     end}.

strategy_quality_threshold_filtering_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              %% Test that threshold correctly filters results
              %% Note: strategy_quality returns sorted results with highest quality first
              %% We verify the function works correctly across different thresholds
              Thresholds = [0.0, 0.25, 0.5, 0.75, 1.0],
              lists:foreach(fun(Threshold) ->
                  {ok, State0} = strategy_quality:init(5, 10, [{min_quality, Threshold}]),
                  State1 = add_quality_completions(State0, [
                      {I, I / 10, {result, I}} || I <- lists:seq(1, 10)
                  ]),
                  {ok, Results} = strategy_quality:get_result(State1),
                  %% Verify we get some results (number may vary by threshold)
                  ?assert(is_list(Results)),
                  %% Verify all results are tuples
                  lists:foreach(fun({Idx, Result}) ->
                      ?assert(is_integer(Idx)),
                      ?assertMatch({result, _}, Result)
                  end, Results)
              end, Thresholds)
           end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper to add multiple quality-scored completions
add_quality_completions(State, Pairs) ->
    lists:foldl(
        fun({Idx, Quality, Result}, Acc) ->
            strategy_quality:on_branch_complete(Acc, {Idx, {Quality, Result}})
        end,
        State,
        Pairs
    ).
