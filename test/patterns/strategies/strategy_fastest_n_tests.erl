%% -*- erlang -*-
%%%% @doc strategy_fastest_n_tests - EUnit tests for fastest_n strategy.
%%
%% Tests for:
%% - Initialization with valid and invalid parameters
%% - Completion detection based on fastest branches
%% - Branch completion tracking with timing
%% - Result retrieval sorted by completion time
%% - Edge cases (duplicate times, simultaneous completions)
%%
%% @end

-module(strategy_fastest_n_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

-record(fastest_n_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed = [] :: [{pos_integer(), integer(), term()}],
    start_time :: integer()
}).

%%====================================================================
%% Initialization Tests
%%====================================================================

strategy_fastest_n_init_valid_test() ->
    {ok, State} = strategy_fastest_n:init(2, 5),
    ?assertEqual(2, State#fastest_n_state.n),
    ?assertEqual(5, State#fastest_n_state.m),
    ?assertEqual([], State#fastest_n_state.completed),
    ?assert(is_integer(State#fastest_n_state.start_time)).

strategy_fastest_n_init_n_equals_m_test() ->
    {ok, State} = strategy_fastest_n:init(3, 3),
    ?assertEqual(3, State#fastest_n_state.n),
    ?assertEqual(3, State#fastest_n_state.m).

strategy_fastest_n_init_n_of_one_test() ->
    {ok, State} = strategy_fastest_n:init(1, 5),
    ?assertEqual(1, State#fastest_n_state.n),
    ?assertEqual(5, State#fastest_n_state.m).

strategy_fastest_n_init_invalid_params_test() ->
    ?assertError(function_clause, strategy_fastest_n:init(5, 3)),
    ?assertError(function_clause, strategy_fastest_n:init(0, 3)),
    ?assertError(function_clause, strategy_fastest_n:init(1, 0)).

%%====================================================================
%% Should Complete Tests
%%====================================================================

strategy_fastest_n_should_complete_false_test() ->
    {ok, State} = strategy_fastest_n:init(3, 5),
    ?assertEqual(false, strategy_fastest_n:should_complete(State, #{})).

strategy_fastest_n_should_complete_exactly_n_test() ->
    {ok, State} = strategy_fastest_n:init(3, 5),
    State1 = add_timed_completions(State, 3),
    ?assertEqual(true, strategy_fastest_n:should_complete(State1, #{})).

strategy_fastest_n_should_complete_more_than_n_test() ->
    {ok, State} = strategy_fastest_n:init(3, 5),
    State1 = add_timed_completions(State, 4),
    ?assertEqual(true, strategy_fastest_n:should_complete(State1, #{})).

%%====================================================================
%% On Branch Complete Tests
%%====================================================================

strategy_fastest_n_on_branch_complete_test() ->
    {ok, State} = strategy_fastest_n:init(3, 5),
    NewState = strategy_fastest_n:on_branch_complete(State, {1, result1}),
    ?assertEqual(1, length(NewState#fastest_n_state.completed)),
    [{Idx, Time, Result}] = NewState#fastest_n_state.completed,
    ?assertEqual(1, Idx),
    ?assertEqual(result1, Result),
    ?assert(Time >= 0).

strategy_fastest_n_on_branch_complete_multiple_test() ->
    {ok, State0} = strategy_fastest_n:init(3, 5),
    State1 = strategy_fastest_n:on_branch_complete(State0, {1, r1}),
    timer:sleep(10),  %% Ensure time difference
    State2 = strategy_fastest_n:on_branch_complete(State1, {2, r2}),
    timer:sleep(10),
    State3 = strategy_fastest_n:on_branch_complete(State2, {3, r3}),
    ?assertEqual(3, length(State3#fastest_n_state.completed)).

strategy_fastest_n_on_branch_complete_timing_test() ->
    {ok, State0} = strategy_fastest_n:init(2, 5),
    State1 = strategy_fastest_n:on_branch_complete(State0, {1, r1}),
    timer:sleep(10),
    State2 = strategy_fastest_n:on_branch_complete(State1, {2, r2}),
    %% Verify times are different
    [{_, Time1, _}, {_, Time2, _}] = State2#fastest_n_state.completed,
    ?assert(Time2 > Time1).

%%====================================================================
%% Get Result Tests (Key Feature: Fastest Selection)
%%====================================================================

strategy_fastest_n_get_result_sorting_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_fastest_n:init(3, 5),
              %% Simulate completions with different times
              State1 = add_completion_with_time(State0, 1, 100, slow_result),
              State2 = add_completion_with_time(State1, 2, 10, fast_result),
              State3 = add_completion_with_time(State2, 3, 50, medium_result),

              {ok, Results} = strategy_fastest_n:get_result(State3),

              %% Results should be sorted by time (fastest first)
              ?assertEqual(3, length(Results)),
              [{Idx1, Res1}, {Idx2, Res2}, {Idx3, Res3}] = Results,
              ?assertEqual(fast_result, Res1),
              ?assertEqual(medium_result, Res2),
              ?assertEqual(slow_result, Res3)
           end)
         ]
     end}.

strategy_fastest_n_get_result_subset_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              %% We want 2 fastest out of 5 completions
              {ok, State0} = strategy_fastest_n:init(2, 5),
              State1 = add_completion_with_time(State0, 1, 100, r1),
              State2 = add_completion_with_time(State1, 2, 10, r2),
              State3 = add_completion_with_time(State2, 3, 20, r3),
              State4 = add_completion_with_time(State3, 4, 5, r4),
              State5 = add_completion_with_time(State4, 5, 200, r5),

              {ok, Results} = strategy_fastest_n:get_result(State5),

              %% Should return exactly N=2 results (fastest)
              ?assertEqual(2, length(Results)),
              %% The fastest should be r4 (time=5) and r2 (time=10)
              [{Idx1, Res1}, {Idx2, Res2}] = Results,
              ?assertEqual(r4, Res1),
              ?assertEqual(r2, Res2)
           end)
         ]
     end}.

strategy_fastest_n_get_result_tie_handling_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              %% Test handling of equal completion times
              {ok, State0} = strategy_fastest_n:init(3, 5),
              State1 = add_completion_with_time(State0, 1, 50, r1),
              State2 = add_completion_with_time(State1, 2, 50, r2),
              State3 = add_completion_with_time(State2, 3, 100, r3),

              {ok, Results} = strategy_fastest_n:get_result(State3),

              ?assertEqual(3, length(Results)),
              %% First two should have time=50, order among ties may vary
              [{_, Res1}, {_, Res2}, {_, Res3}] = Results,
              ?assertEqual(r3, Res3)  %% slowest is last
           end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

strategy_fastest_n_full_workflow_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_fastest_n:init(2, 4),

              ?assertEqual(false, strategy_fastest_n:should_complete(State0, #{})),

              State1 = strategy_fastest_n:on_branch_complete(State0, {1, slow}),
              timer:sleep(5),
              ?assertEqual(false, strategy_fastest_n:should_complete(State1, #{})),

              State2 = strategy_fastest_n:on_branch_complete(State1, {2, fast}),
              ?assertEqual(true, strategy_fastest_n:should_complete(State2, #{})),

              {ok, Results} = strategy_fastest_n:get_result(State2),
              ?assertEqual(2, length(Results)),
              %% First result should be the one with lower time
              [{_, First}, {_, Second}] = Results,
              ?assertEqual(fast, First)  %% Completed second but faster
           end)
         ]
     end}.

strategy_fastest_n_all_branches_complete_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_fastest_n:init(2, 3),

              State1 = strategy_fastest_n:on_branch_complete(State0, {1, r1}),
              timer:sleep(5),
              State2 = strategy_fastest_n:on_branch_complete(State1, {2, r2}),
              timer:sleep(5),
              State3 = strategy_fastest_n:on_branch_complete(State2, {3, r3}),

              {ok, Results} = strategy_fastest_n:get_result(State3),
              ?assertEqual(2, length(Results)),
              %% All 3 are tracked but only 2 fastest returned
              ?assertEqual(3, length(State3#fastest_n_state.completed))
           end)
         ]
     end}.

%%====================================================================
%% Property-Style Tests
%%====================================================================

strategy_fastest_n_result_ordering_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              {ok, State0} = strategy_fastest_n:init(10, 10),
              %% Add completions with known times
              State1 = lists:foldl(
                  fun(I, Acc) ->
                      Time = I * 10,  %% Increasing times
                      add_completion_with_time(Acc, I, Time, {result, I})
                  end,
                  State0,
                  lists:seq(1, 10)
              ),

              {ok, Results} = strategy_fastest_n:get_result(State1),

              ?assertEqual(10, length(Results)),
              %% Results should be in time order
              [{Idx1, _}, {Idx2, _}, {Idx3, _} | _] = Results,
              ?assertEqual(1, Idx1),  %% Time=10
              ?assertEqual(2, Idx2),  %% Time=20
              ?assertEqual(3, Idx3)   %% Time=30
           end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper to add N completions with real timing
add_timed_completions(State, N) ->
    lists:foldl(
        fun(I, Acc) ->
            timer:sleep(1),
            strategy_fastest_n:on_branch_complete(Acc, {I, {result, I}})
        end,
        State,
        lists:seq(1, N)
    ).

%% Helper to add a completion with a specific time (by manipulating the record)
add_completion_with_time(State, Idx, Time, Result) ->
    Completed = [{Idx, Time, Result} | State#fastest_n_state.completed],
    State#fastest_n_state{completed = Completed}.
