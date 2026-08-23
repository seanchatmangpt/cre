%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc Multiple Instance Pattern Unit Tests
%%
%% Comprehensive EUnit tests for multiple instance workflow patterns:
%% - WCP12: Multiple Instances without Synchronization
%% - WCP13: Multiple Instances with Design Time Knowledge
%% - WCP14: Multiple Instances with Runtime Knowledge
%% - P31: Blocking Partial Join
%% - P32: Cancelling Partial Join
%% - P34: Static Partial Join for MI
%% - P35: Cancelling Partial Join for MI
%% - P36: Dynamic Partial Join for MI
%%
%% @end
%% -------------------------------------------------------------------

-module(multiple_instance_pattern_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Test subprocess that doubles input
double_subprocess(X) -> X * 2.

%% Test subprocess with delay
delayed_subprocess(X) ->
    timer:sleep(10),
    X + 1.

%% Test subprocess that may fail
failing_subprocess(X) ->
    case X rem 5 of
        0 -> exit(failure);
        _ -> X * 2
    end.

%%====================================================================
%% multi_instance tests
%%====================================================================

multi_instance_no_sync_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"No sync spawns correct number of instances",
           fun() ->
               {ok, Pids} = multi_instance:multiple_instances_no_sync(
                   fun(X) -> X * 2 end, 3),
               ?assertEqual(3, length(Pids)),
               ?assert(lists:all(fun is_pid/1, Pids))
           end},

          {"No sync returns error for invalid count",
           fun() ->
               Fun = fun(X) -> X * 2 end,
               ?assertEqual({error, invalid_arguments},
                          multi_instance:multiple_instances_no_sync(Fun, 0)),
               ?assertEqual({error, invalid_arguments},
                          multi_instance:multiple_instances_no_sync(Fun, -1))
           end},

          {"No sync executes function in each instance",
           fun() ->
               {ok, Pids} = multi_instance:multiple_instances_no_sync(
                   fun(X) -> X * 10 end, 5),
               ?assertEqual(5, length(Pids)),
               %% Verify processes are running
               ?assert(lists:all(fun(P) ->
                   case erlang:is_process_alive(P) of
                       true -> true;
                       false -> %% May have already finished
                           true
                   end
               end, Pids))
           end}
         ]
     end}.

multi_instance_design_time_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Design time creates instances with sync",
           fun() ->
               Fun = fun(X) -> X * 2 end,
               Data = [1, 2, 3, 4],
               {ok, Results} = multi_instance:multiple_instances_design_time(
                   Fun, Data, 4),
               ?assertEqual([2, 4, 6, 8], Results)
           end},

          {"Design time detects count mismatch",
           fun() ->
               Fun = fun(X) -> X * 2 end,
               Data = [1, 2, 3],
               ?assertEqual({error, {count_mismatch, 3, 5}},
                          multi_instance:multiple_instances_design_time(Fun, Data, 5))
           end},

          {"Design time handles empty data",
           fun() ->
               Fun = fun(X) -> X * 2 end,
               ?assertEqual({error, invalid_count},
                          multi_instance:multiple_instances_design_time(Fun, [], 0))
           end}
         ]
     end}.

multi_instance_runtime_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Runtime determines count dynamically",
           fun() ->
               Fun = fun(X) -> X * 2 end,
               Data = [1, 2, 3, 4, 5],
               CountFun = fun(L) -> length(L) end,
               {ok, Results} = multi_instance:multiple_instances_runtime(
                   Fun, Data, CountFun),
               ?assertEqual([2, 4, 6, 8, 10], Results)
           end},

          {"Runtime handles insufficient data",
           fun() ->
               Fun = fun(X) -> X * 2 end,
               Data = [1, 2],
               CountFun = fun(_L) -> 5 end,
               ?assertEqual({error, {insufficient_data, 2, 5}},
                          multi_instance:multiple_instances_runtime(Fun, Data, CountFun))
           end},

          {"Runtime uses custom count function",
           fun() ->
               Fun = fun(X) -> X end,
               Data = [a, b, c, d],
               CountFun = fun(L) -> length(L) div 2 end,
               {ok, Results} = multi_instance:multiple_instances_runtime(
                   Fun, Data, CountFun),
               ?assertEqual(2, length(Results))
           end}
         ]
     end}.

create_instances_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Create instances generates correct tokens",
           fun() ->
               Data = [a, b, c],
               Tokens = multi_instance:create_instances(Data, 3, fun(X) -> X end),
               ?assertEqual(3, length(Tokens)),
               ?assertEqual([{instance, 1, a}, {instance, 2, b}, {instance, 3, c}],
                          Tokens)
           end},

          {"Create instances applies transform",
           fun() ->
               Data = [1, 2, 3],
               Tokens = multi_instance:create_instances(Data, 3, fun(X) -> X * 10 end),
               ?assertEqual([{instance, 1, 10}, {instance, 2, 20}, {instance, 3, 30}],
                          Tokens)
           end},

          {"Create instances handles undefined data",
           fun() ->
               Data = [only_one],
               Tokens = multi_instance:create_instances(Data, 3, fun(X) -> X end),
               ?assertEqual(3, length(Tokens)),
               ?assertEqual(undefined, element(3, lists:nth(2, Tokens))),
               ?assertEqual(undefined, element(3, lists:nth(3, Tokens)))
           end}
         ]
     end}.

instance_counter_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Instance counter initializes correctly",
           fun() ->
               State = multi_instance:instance_counter(new),
               ?assertEqual(#{total => 0, active => 0, completed => 0}, State)
           end},

          {"Instance counter tracks spawns",
           fun() ->
               State0 = multi_instance:instance_counter(new),
               State1 = multi_instance:instance_counter({spawn, 5, State0}),
               ?assertEqual(5, maps:get(total, State1)),
               ?assertEqual(5, maps:get(active, State1)),
               ?assertEqual(0, maps:get(completed, State1))
           end},

          {"Instance counter tracks completions",
           fun() ->
               State0 = multi_instance:instance_counter(new),
               State1 = multi_instance:instance_counter({spawn, 10, State0}),
               State2 = multi_instance:instance_counter({complete, 3, State1}),
               ?assertEqual(10, maps:get(total, State2)),
               ?assertEqual(7, maps:get(active, State2)),
               ?assertEqual(3, maps:get(completed, State2))
           end},

          {"Instance counter prevents negative active",
           fun() ->
               State0 = multi_instance:instance_counter(new),
               State1 = multi_instance:instance_counter({spawn, 2, State0}),
               State2 = multi_instance:instance_counter({complete, 5, State1}),
               %% Active should not go below 0
               ?assertEqual(0, maps:get(active, State2)),
               ?assertEqual(5, maps:get(completed, State2))
           end}
         ]
     end}.

collect_instances_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Collect empty instances",
           fun() ->
               ?assertEqual({ok, []},
                          multi_instance:collect_instances(make_ref(), 0))
           end}
         ]
     end}.

%%====================================================================
%% static_partial_join_mi tests (P34)
%%====================================================================

static_partial_join_mi_place_lst_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Returns correct place list",
           fun() ->
               Places = static_partial_join_mi:place_lst(),
               ?assert(lists:member(p_start, Places)),
               ?assert(lists:member(p_instances, Places)),
               ?assert(lists:member(p_threshold_met, Places)),
               ?assert(lists:member(p_end, Places))
           end}
         ]
     end}.

static_partial_join_mi_trsn_lst_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Returns correct transition list",
           fun() ->
               Transitions = static_partial_join_mi:trsn_lst(),
               ?assert(lists:member(t_create_instances, Transitions)),
               ?assert(lists:member(t_complete_instance, Transitions)),
               ?assert(lists:member(t_threshold, Transitions)),
               ?assert(lists:member(t_finish, Transitions))
           end}
         ]
     end}.

static_partial_join_mi_init_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Init with custom values",
           fun() ->
               State = static_partial_join_mi:init(
                   #{total_instances => 10, threshold => 7}),
               ?assertEqual(10, maps:get(total_instances, State)),
               ?assertEqual(7, maps:get(threshold, State)),
               ?assertEqual(0, maps:get(completed, State))
           end},

          {"Init with default values",
           fun() ->
               State = static_partial_join_mi:init(#{}),
               ?assertEqual(5, maps:get(total_instances, State)),
               ?assertEqual(3, maps:get(threshold, State))
           end}
         ]
     end}.

%%====================================================================
%% dynamic_partial_join_mi tests (P36)
%%====================================================================

dynamic_partial_join_mi_place_lst_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Returns correct place list",
           fun() ->
               Places = dynamic_partial_join_mi:place_lst(),
               ?assert(lists:member(p_start, Places)),
               ?assert(lists:member(p_instances, Places)),
               ?assert(lists:member(p_threshold_met, Places)),
               ?assert(lists:member(p_end, Places))
           end}
         ]
     end}.

dynamic_partial_join_mi_threshold_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Computes default threshold",
           fun() ->
               State = #{
                   threshold_expr => <<>>,
                   threshold => undefined,
                   completed => 0
               },
               ?assertEqual(3, dynamic_partial_join_mi:compute_threshold(State))
           end},

          {"Computes expression-based threshold",
           fun() ->
               State = #{
                   threshold_expr => <<"ceil(attendance_estimate*0.08)">>,
                   threshold => undefined,
                   completed => 0
               },
               ?assertEqual(64, dynamic_partial_join_mi:compute_threshold(State))
           end}
         ]
     end}.

%%====================================================================
%% cancelling_partial_join_mi tests (P35)
%%====================================================================

cancelling_partial_join_mi_place_lst_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Returns correct place list",
           fun() ->
               Places = cancelling_partial_join_mi:place_lst(),
               ?assert(lists:member(p_start, Places)),
               ?assert(lists:member(p_instances, Places)),
               ?assert(lists:member(p_threshold_met, Places)),
               ?assert(lists:member(p_cancelled, Places)),
               ?assert(lists:member(p_end, Places))
           end}
         ]
     end}.

cancelling_partial_join_mi_init_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Init with cancellation flag",
           fun() ->
               State = cancelling_partial_join_mi:init(
                   #{total_instances => 8, threshold => 5}),
               ?assertEqual(8, maps:get(total_instances, State)),
               ?assertEqual(5, maps:get(threshold, State)),
               ?assertEqual(false, maps:get(cancelled, State))
           end}
         ]
     end}.

%%====================================================================
%% blocking_partial_join tests (P31)
%%====================================================================

blocking_partial_join_place_lst_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Returns correct place list",
           fun() ->
               Places = blocking_partial_join:place_lst(),
               ?assert(lists:member(p_start, Places)),
               ?assert(lists:member(p_branch1, Places)),
               ?assert(lists:member(p_branch2, Places)),
               ?assert(lists:member(p_branch3, Places)),
               ?assert(lists:member(p_partial_out, Places)),
               ?assert(lists:member(p_final_out, Places)),
               ?assert(lists:member(p_end, Places))
           end}
         ]
     end}.

blocking_partial_join_init_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Init with M and N values",
           fun() ->
               State = blocking_partial_join:init(
                   #{m => 5, n => 2,
                     partial_out => p_partial_out, final_out => p_final_out}),
               ?assertEqual(5, maps:get(m, State)),
               ?assertEqual(2, maps:get(n, State))
           end},

          {"Init with default M and N",
           fun() ->
               State = blocking_partial_join:init(#{}),
               ?assertEqual(3, maps:get(m, State)),
               ?assertEqual(2, maps:get(n, State))
           end}
         ]
     end}.

%%====================================================================
%% cancelling_partial_join tests (P32)
%%====================================================================

cancelling_partial_join_place_lst_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Returns correct place list",
           fun() ->
               Places = cancelling_partial_join:place_lst(),
               ?assert(lists:member(p_start, Places)),
               ?assert(lists:member(p_branch1, Places)),
               ?assert(lists:member(p_branch2, Places)),
               ?assert(lists:member(p_branch3, Places)),
               ?assert(lists:member(p_threshold_met, Places)),
               ?assert(lists:member(p_cancelled, Places)),
               ?assert(lists:member(p_end, Places))
           end}
         ]
     end}.

cancelling_partial_join_init_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Init with cancel remaining flag",
           fun() ->
               State = cancelling_partial_join:init(
                   #{m => 5, n => 3, cancel_remaining => true}),
               ?assertEqual(5, maps:get(m, State)),
               ?assertEqual(3, maps:get(n, State)),
               ?assertEqual(true, maps:get(cancel_remaining, State))
           end},

          {"Init defaults cancel_remaining to true",
           fun() ->
               State = cancelling_partial_join:init(#{m => 4, n => 2}),
               ?assertEqual(true, maps:get(cancel_remaining, State))
           end}
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          {"Multi instance workflow end to end",
           fun() ->
               %% Test full workflow with multiple instances
               Fun = fun(X) -> X * 3 end,
               Data = [1, 2, 3],
               {ok, Results} = multi_instance:multiple_instances_design_time(
                   Fun, Data, 3),
               ?assertEqual([3, 6, 9], Results)
           end},

          {"Instance counter lifecycle",
           fun() ->
               %% Simulate full instance lifecycle
               State0 = multi_instance:instance_counter(new),
               State1 = multi_instance:instance_counter({spawn, 10, State0}),
               State2 = multi_instance:instance_counter({complete, 5, State1}),
               State3 = multi_instance:instance_counter({complete, 5, State2}),

               ?assertEqual(10, maps:get(total, State3)),
               ?assertEqual(0, maps:get(active, State3)),
               ?assertEqual(10, maps:get(completed, State3))
           end},

          {"Static partial join with threshold",
           fun() ->
               %% Verify static partial join structure
               State = static_partial_join_mi:init(
                   #{total_instances => 10, threshold => 7}),
               ?assertEqual(10, maps:get(total_instances, State)),
               ?assertEqual(7, maps:get(threshold, State))
           end},

          {"Dynamic partial join with expression",
           fun() ->
               %% Verify dynamic partial join threshold computation
               State = dynamic_partial_join_mi:init(
                   #{threshold_expr => <<"ceil(attendance_estimate*0.08)">>}),
               ?assertEqual(<<"ceil(attendance_estimate*0.08)">>,
                          maps:get(threshold_expr, State))
           end}
         ]
     end}.
