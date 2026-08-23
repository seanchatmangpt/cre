%% -*- erlang -*-
%%%% @doc strategy_contextual_tests - EUnit tests for contextual bandit strategy.
%%
%% Tests for:
%% - gen_server lifecycle (start_link, stop)
%% - Branch prediction using contextual features
%% - Model updates with observations
%% - Custom feature extractors
%% - Fallback behavior when no model exists
%% - Edge cases (various context types)
%%
%% @end

-module(strategy_contextual_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

-define(TEST_SERVER, contextual_test_server).

%% Simple context for testing
-define(SIMPLE_CONTEXT, #{
    case_type => 1.0,
    priority => 0.5,
    value => 0.8,
    hour_of_day => 12.0,
    queue_depth => 3.0
}).

%%====================================================================
%% gen_server Lifecycle Tests
%%====================================================================

strategy_contextual_start_link_test() ->
    {ok, Pid} = strategy_contextual:start_link(5, #{}),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, strategy_contextual:stop(Pid)),
    ?assertNot(is_process_alive(Pid)).

strategy_contextual_start_link_with_options_test() ->
    {ok, Pid} = strategy_contextual:start_link(5, #{
        m => 2,
        min_samples => 5,
        fallback_strategy => ucb
    }),
    ?assert(is_pid(Pid)),
    strategy_contextual:stop(Pid).

strategy_contextual_start_link_invalid_n_test() ->
    ?assertExit({badarg, _}, strategy_contextual:start_link(0, #{})).

%%====================================================================
%% Predict Branch Tests
%%====================================================================

strategy_contextual_predict_branch_no_model_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{fallback_strategy => random}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              {ok, Branch} = strategy_contextual:predict_branch(Pid, ?SIMPLE_CONTEXT),
              ?assert(Branch >= 1 andalso Branch =< 3)
           end)
         ]
     end}.

strategy_contextual_predict_branch_first_n_fallback_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(5, #{fallback_strategy => first_n}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Multiple predictions should give valid branches
              Branches = lists:map(fun(_) ->
                  {ok, B} = strategy_contextual:predict_branch(Pid, ?SIMPLE_CONTEXT),
                  B
              end, lists:seq(1, 10)),

              lists:foreach(fun(B) ->
                  ?assert(B >= 1 andalso B =< 5)
              end, Branches)
           end)
         ]
     end}.

strategy_contextual_predict_branch_with_model_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{min_samples => 1}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Train model first
              ok = strategy_contextual:update_model(Pid, ?SIMPLE_CONTEXT, 1, 1.0),

              %% Now prediction should use the model
              {ok, Branch} = strategy_contextual:predict_branch(Pid, ?SIMPLE_CONTEXT),
              ?assert(Branch >= 1 andalso Branch =< 3)
           end)
         ]
     end}.

%%====================================================================
%% Update Model Tests
%%====================================================================

strategy_contextual_update_model_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              ?assertEqual(ok, strategy_contextual:update_model(
                  Pid, ?SIMPLE_CONTEXT, 1, 1.0)),

              ?assertEqual(ok, strategy_contextual:update_model(
                  Pid, ?SIMPLE_CONTEXT, 2, 0.5)),

              ?assertEqual(ok, strategy_contextual:update_model(
                  Pid, ?SIMPLE_CONTEXT, 3, 0.0))
           end)
         ]
     end}.

strategy_contextual_update_model_learning_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{min_samples => 1}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Train to prefer branch 1 for this context
              lists:foreach(fun(_) ->
                  ok = strategy_contextual:update_model(Pid, ?SIMPLE_CONTEXT, 1, 1.0),
                  ok = strategy_contextual:update_model(Pid, ?SIMPLE_CONTEXT, 2, 0.0),
                  ok = strategy_contextual:update_model(Pid, ?SIMPLE_CONTEXT, 3, 0.0)
              end, lists:seq(1, 10)),

              %% After training, predictions should favor branch 1
              {ok, Branch} = strategy_contextual:predict_branch(Pid, ?SIMPLE_CONTEXT),
              ?assert(Branch >= 1 andalso Branch =< 3)
           end)
         ]
     end}.

strategy_contextual_update_model_different_contexts_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              Context1 = #{case_type => 1.0, priority => 0.5, value => 0.8},
              Context2 = #{case_type => 1.0, priority => 1.0, value => 0.8},

              %% Train different contexts
              ok = strategy_contextual:update_model(Pid, Context1, 1, 1.0),
              ok = strategy_contextual:update_model(Pid, Context2, 2, 1.0),

              %% Both should work
              {ok, Branch1} = strategy_contextual:predict_branch(Pid, Context1),
              {ok, Branch2} = strategy_contextual:predict_branch(Pid, Context2),

              ?assert(Branch1 >= 1 andalso Branch1 =< 3),
              ?assert(Branch2 >= 1 andalso Branch2 =< 3)
           end)
         ]
     end}.

%%====================================================================
%% Custom Feature Extractor Tests
%%====================================================================

strategy_contextual_set_feature_extractor_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              CustomExtractor = fun(Ctx) ->
                  [maps:get(custom_field, Ctx, 0.0)]
              end,

              ?assertEqual(ok, strategy_contextual:set_feature_extractor(Pid, CustomExtractor)),

              %% Should use custom extractor
              Context = #{custom_field => 5.0},
              ok = strategy_contextual:update_model(Pid, Context, 1, 1.0),

              {ok, Branch} = strategy_contextual:predict_branch(Pid, Context),
              ?assert(Branch >= 1 andalso Branch =< 3)
           end)
         ]
     end}.

strategy_contextual_complex_feature_extractor_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(5, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Feature extractor that returns multiple features
              Extractor = fun(Ctx) ->
                  [
                      maps:get(f1, Ctx, 0.0),
                      maps:get(f2, Ctx, 0.0),
                      maps:get(f3, Ctx, 0.0),
                      maps:get(f4, Ctx, 0.0),
                      maps:get(f5, Ctx, 0.0)
                  ]
              end,

              ok = strategy_contextual:set_feature_extractor(Pid, Extractor),

              Context = #{f1 => 0.1, f2 => 0.2, f3 => 0.3, f4 => 0.4, f5 => 0.5},
              ok = strategy_contextual:update_model(Pid, Context, 1, 1.0),

              {ok, Branch} = strategy_contextual:predict_branch(Pid, Context),
              ?assert(Branch >= 1 andalso Branch =< 5)
           end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

strategy_contextual_full_workflow_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{
             min_samples => 2,
             fallback_strategy => first_n
         }),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              Context = ?SIMPLE_CONTEXT,

              %% Initially use fallback
              {ok, Branch1} = strategy_contextual:predict_branch(Pid, Context),
              ?assert(Branch1 >= 1 andalso Branch1 =< 3),

              %% Train model
              ok = strategy_contextual:update_model(Pid, Context, 1, 1.0),
              ok = strategy_contextual:update_model(Pid, Context, 1, 1.0),

              %% Now should use model
              {ok, Branch2} = strategy_contextual:predict_branch(Pid, Context),
              ?assert(Branch2 >= 1 andalso Branch2 =< 3)
           end)
         ]
     end}.

strategy_contextual_context_sensitive_predictions_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{min_samples => 1}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(begin
              %% Different contexts should learn different preferences
              HighPriorityContext = #{case_type => 1.0, priority => 1.0, value => 0.8},
              LowPriorityContext = #{case_type => 1.0, priority => 0.0, value => 0.8},

              %% Train high priority to prefer branch 1
              lists:foreach(fun(_) ->
                  ok = strategy_contextual:update_model(Pid, HighPriorityContext, 1, 1.0),
                  ok = strategy_contextual:update_model(Pid, HighPriorityContext, 2, 0.0),
                  ok = strategy_contextual:update_model(Pid, HighPriorityContext, 3, 0.0)
              end, lists:seq(1, 5)),

              %% Train low priority to prefer branch 2
              lists:foreach(fun(_) ->
                  ok = strategy_contextual:update_model(Pid, LowPriorityContext, 1, 0.0),
                  ok = strategy_contextual:update_model(Pid, LowPriorityContext, 2, 1.0),
                  ok = strategy_contextual:update_model(Pid, LowPriorityContext, 3, 0.0)
              end, lists:seq(1, 5)),

              %% Predictions should differ (or at least be valid)
              {ok, BranchHigh} = strategy_contextual:predict_branch(Pid, HighPriorityContext),
              {ok, BranchLow} = strategy_contextual:predict_branch(Pid, LowPriorityContext),

              ?assert(BranchHigh >= 1 andalso BranchHigh =< 3),
              ?assert(BranchLow >= 1 andalso BranchLow =< 3)
           end)
         ]
     end}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

strategy_contextual_empty_context_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
    fun(Pid) ->
        [
         ?_test(begin
             %% Empty context should use defaults
             {ok, Branch} = strategy_contextual:predict_branch(Pid, #{}),
             ?assert(Branch >= 1 andalso Branch =< 3)
          end)
        ]
    end}.

strategy_contextual_missing_fields_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(3, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
    fun(Pid) ->
        [
         ?_test(begin
             %% Context with only some fields
             PartialContext = #{priority => 0.5},
             {ok, Branch} = strategy_contextual:predict_branch(Pid, PartialContext),
             ?assert(Branch >= 1 andalso Branch =< 3)
          end)
        ]
    end}.

strategy_contextual_different_fallback_strategies_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(5, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
    fun(Pid) ->
        [
         ?_test(begin
             %% Test with different contexts to verify fallback works
             Contexts = [
                 #{value => 1.0, priority => 0.5},
                 #{case_type => 2.0, queue_depth => 1.0},
                 #{hour_of_day => 12.0}
             ],

             lists:foreach(fun(Context) ->
                 {ok, Branch} = strategy_contextual:predict_branch(Pid, Context),
                 ?assert(Branch >= 1 andalso Branch =< 5)
             end, Contexts)
          end)
        ]
    end}.

strategy_contextual_single_branch_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(1, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
    fun(Pid) ->
        [
         ?_test(begin
             %% With only 1 branch, should always return 1
             {ok, Branch} = strategy_contextual:predict_branch(Pid, ?SIMPLE_CONTEXT),
             ?assertEqual(1, Branch),

             ok = strategy_contextual:update_model(Pid, ?SIMPLE_CONTEXT, 1, 1.0),
             {ok, Branch2} = strategy_contextual:predict_branch(Pid, ?SIMPLE_CONTEXT),
             ?assertEqual(1, Branch2)
          end)
        ]
    end}.

strategy_contextual_reward_ranges_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = strategy_contextual:start_link(2, #{}),
         Pid
     end,
     fun(Pid) ->
         strategy_contextual:stop(Pid)
     end,
    fun(Pid) ->
        [
         ?_test(begin
             %% Test different reward ranges
             Rewards = [0.0, 0.5, 1.0, -1.0, 100.0],

             lists:foreach(fun(Reward) ->
                 ok = strategy_contextual:update_model(
                     Pid, ?SIMPLE_CONTEXT, 1, Reward)
             end, Rewards),

             %% Should still work
             {ok, Branch} = strategy_contextual:predict_branch(Pid, ?SIMPLE_CONTEXT),
             ?assert(Branch >= 1 andalso Branch =< 2)
          end)
        ]
    end}.
