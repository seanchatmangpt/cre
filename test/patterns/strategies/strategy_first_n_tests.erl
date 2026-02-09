%% -*- erlang -*-
%%%% @doc strategy_first_n_tests - EUnit tests for first_n strategy.
%%
%% Tests for:
%% - Initialization with valid and invalid parameters
%% - Completion detection logic
%% - Branch completion tracking
%% - Result retrieval
%% - Edge cases (empty results, partial completions)
%%
%% @end

-module(strategy_first_n_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Define the record locally since it's exported from the module under test
-record(first_n_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed = [] :: [pos_integer()],
    results = #{} :: map()
}).

%%====================================================================
%% Initialization Tests
%%====================================================================

strategy_first_n_init_valid_test() ->
    {ok, State} = strategy_first_n:init(2, 5),
    ?assertEqual(2, State#first_n_state.n),
    ?assertEqual(5, State#first_n_state.m),
    ?assertEqual([], State#first_n_state.completed),
    ?assertEqual(#{}, State#first_n_state.results).

strategy_first_n_init_n_equals_m_test() ->
    {ok, State} = strategy_first_n:init(3, 3),
    ?assertEqual(3, State#first_n_state.n),
    ?assertEqual(3, State#first_n_state.m).

strategy_first_n_init_n_of_one_test() ->
    {ok, State} = strategy_first_n:init(1, 1),
    ?assertEqual(1, State#first_n_state.n),
    ?assertEqual(1, State#first_n_state.m).

strategy_first_n_init_n_greater_than_m_test() ->
    %% This should fail the guard clause
    ?assertError(function_clause, strategy_first_n:init(5, 3)).

strategy_first_n_init_zero_n_test() ->
    ?assertError(function_clause, strategy_first_n:init(0, 3)).

strategy_first_n_init_zero_m_test() ->
    ?assertError(function_clause, strategy_first_n:init(1, 0)).

%%====================================================================
%% Should Complete Tests
%%====================================================================

strategy_first_n_should_complete_false_test() ->
    {ok, State} = strategy_first_n:init(3, 5),
    ?assertEqual(false, strategy_first_n:should_complete(State, #{})).

strategy_first_n_should_complete_exactly_n_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = add_completions(State0, [1, 2, 3]),
    ?assertEqual(true, strategy_first_n:should_complete(State1, #{})).

strategy_first_n_should_complete_more_than_n_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = add_completions(State0, [1, 2, 3, 4]),
    ?assertEqual(true, strategy_first_n:should_complete(State1, #{})).

strategy_first_n_should_complete_partial_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = add_completions(State0, [1, 2]),
    ?assertEqual(false, strategy_first_n:should_complete(State1, #{})).

strategy_first_n_should_complete_one_of_many_test() ->
    {ok, State0} = strategy_first_n:init(1, 10),
    State1 = add_completions(State0, [5]),
    ?assertEqual(true, strategy_first_n:should_complete(State1, #{})).

strategy_first_n_should_complete_with_context_ignored_test() ->
    {ok, State0} = strategy_first_n:init(2, 5),
    State1 = add_completions(State0, [1, 2]),
    %% Context should be ignored by this strategy
    ?assertEqual(true, strategy_first_n:should_complete(State1, #{any => context})).

%%====================================================================
%% On Branch Complete Tests
%%====================================================================

strategy_first_n_on_branch_complete_single_test() ->
    {ok, State} = strategy_first_n:init(3, 5),
    NewState = strategy_first_n:on_branch_complete(State, {1, result1}),
    ?assertEqual([1], NewState#first_n_state.completed),
    ?assertEqual(result1, maps:get(1, NewState#first_n_state.results)).

strategy_first_n_on_branch_complete_multiple_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = strategy_first_n:on_branch_complete(State0, {1, result1}),
    State2 = strategy_first_n:on_branch_complete(State1, {2, result2}),
    State3 = strategy_first_n:on_branch_complete(State2, {3, result3}),
    ?assertEqual([3, 2, 1], State3#first_n_state.completed),
    ?assertEqual(3, map_size(State3#first_n_state.results)).

strategy_first_n_on_branch_complete_overwrites_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = strategy_first_n:on_branch_complete(State0, {1, result1}),
    State2 = strategy_first_n:on_branch_complete(State1, {1, result1_updated}),
    ?assertEqual(result1_updated, maps:get(1, State2#first_n_state.results)),
    ?assertEqual([1, 1], State2#first_n_state.completed).

strategy_first_n_on_branch_complete_various_result_types_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = strategy_first_n:on_branch_complete(State0, {1, <<"binary_result">>}),
    State2 = strategy_first_n:on_branch_complete(State1, {2, {tuple, result}}),
    State3 = strategy_first_n:on_branch_complete(State2, {3, [list, result]}),
    ?assertEqual(<<"binary_result">>, maps:get(1, State3#first_n_state.results)),
    ?assertEqual({tuple, result}, maps:get(2, State3#first_n_state.results)),
    ?assertEqual([list, result], maps:get(3, State3#first_n_state.results)).

%%====================================================================
%% Get Result Tests
%%====================================================================

strategy_first_n_get_result_empty_test() ->
    {ok, State} = strategy_first_n:init(3, 5),
    {ok, Results} = strategy_first_n:get_result(State),
    ?assertEqual(#{}, Results).

strategy_first_n_get_result_partial_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = add_completions(State0, [{1, r1}, {2, r2}]),
    {ok, Results} = strategy_first_n:get_result(State1),
    ?assertEqual(r1, maps:get(1, Results)),
    ?assertEqual(r2, maps:get(2, Results)).

strategy_first_n_get_result_full_n_test() ->
    {ok, State0} = strategy_first_n:init(3, 5),
    State1 = add_completions(State0, [{1, r1}, {2, r2}, {3, r3}]),
    {ok, Results} = strategy_first_n:get_result(State1),
    ?assertEqual(3, map_size(Results)),
    ?assertEqual(r1, maps:get(1, Results)),
    ?assertEqual(r2, maps:get(2, Results)),
    ?assertEqual(r3, maps:get(3, Results)).

strategy_first_n_get_result_all_m_test() ->
    {ok, State0} = strategy_first_n:init(2, 3),
    State1 = add_completions(State0, [{1, r1}, {2, r2}, {3, r3}]),
    {ok, Results} = strategy_first_n:get_result(State1),
    ?assertEqual(3, map_size(Results)).

%%====================================================================
%% Integration Tests
%%====================================================================

strategy_first_n_full_workflow_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_first_n:init(2, 4),
              ?assertEqual(false, strategy_first_n:should_complete(State0, #{})),

              State1 = strategy_first_n:on_branch_complete(State0, {1, first_result}),
              ?assertEqual(false, strategy_first_n:should_complete(State1, #{})),

              State2 = strategy_first_n:on_branch_complete(State1, {2, second_result}),
              ?assertEqual(true, strategy_first_n:should_complete(State2, #{})),

              {ok, Results} = strategy_first_n:get_result(State2),
              ?assertEqual(2, map_size(Results))
           end)
         ]
     end}.

strategy_first_n_out_of_order_completion_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_first_n:init(3, 5),

              %% Complete branches out of order
              State1 = strategy_first_n:on_branch_complete(State0, {5, last_result}),
              State2 = strategy_first_n:on_branch_complete(State1, {2, middle_result}),
              State3 = strategy_first_n:on_branch_complete(State2, {1, first_result}),

              {ok, Results} = strategy_first_n:get_result(State3),
              ?assertEqual(3, map_size(Results)),
              ?assertEqual(first_result, maps:get(1, Results)),
              ?assertEqual(middle_result, maps:get(2, Results)),
              ?assertEqual(last_result, maps:get(5, Results))
           end)
         ]
     end}.

%%====================================================================
%% Property-Style Tests
%%====================================================================

strategy_first_n_completions_preserve_results_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_first_n:init(5, 10),
              Completions = [{I, {result, I}} || I <- lists:seq(1, 10)],
              State1 = lists:foldl(
                  fun({Idx, Res}, Acc) ->
                      strategy_first_n:on_branch_complete(Acc, {Idx, Res})
                  end,
                  State0,
                  Completions
              ),
              {ok, Results} = strategy_first_n:get_result(State1),
              ?assertEqual(10, map_size(Results)),
              %% Verify all results are preserved
              lists:foreach(fun(I) ->
                  ?assertEqual({result, I}, maps:get(I, Results))
              end, lists:seq(1, 10))
           end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper to add multiple completions
add_completions(State, Indices) when is_list(Indices) ->
    lists:foldl(
        fun(Index, Acc) ->
            strategy_first_n:on_branch_complete(Acc, {Index, {result, Index}})
        end,
        State,
        Indices
    );
add_completions(State, Pairs) when is_list(Pairs) ->
    lists:foldl(
        fun({Index, Result}, Acc) ->
            strategy_first_n:on_branch_complete(Acc, {Index, Result})
        end,
        State,
        Pairs
    ).
