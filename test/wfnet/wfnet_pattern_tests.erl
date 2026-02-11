%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @author CRE Team
%% @version 0.3.0
%% @doc Individual Workflow Pattern Tests
%%
%% Comprehensive test suite for individual workflow patterns.
%% Tests pattern execution semantics, state management, and completion.
%% @end
%% -------------------------------------------------------------------

-module(wfnet_pattern_tests).
-author('CRE Team').

-include_lib("eunit/include/eunit.hrl").
-include("gen_pnet.hrl").
-include("gen_wfnet.hrl").

%%====================================================================
%% Test Setup/Cleanup
%%====================================================================

setup() ->
    %% Start CRE application
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Wait for cre_master to be registered
    timer:sleep(100),
    case erlang:whereis(cre_master) of
        undefined ->
            timer:sleep(200),
            CrePid = erlang:whereis(cre_master),
            {ok, CrePid};
        CrePid ->
            {ok, CrePid}
    end.

cleanup(_CrePid) ->
    ok.

%%====================================================================
%% Basic Pattern Tests (WCP-1 to WCP-10)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc WCP-1: Sequence Pattern Test
%%--------------------------------------------------------------------
sequence_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create sequence pattern
                     SeqPattern = wfnet_patterns:sequence([task1, task2]),
                     
                     %% Validate pattern structure
                     ?assert(is_record(SeqPattern, 'EPC')),
                     ?assertEqual(2, maps:size(SeqPattern#EPC.transitions)),
                     ?assertEqual(3, maps:size(SeqPattern#EPC.places)),
                     
                     %% Check initial marking
                     ?assert(maps:is_key(p_start, SeqPattern#EPC.marking)),
                     ?assertEqual([p_start], maps:get(p_start, SeqPattern#EPC.marking))
                 end),
          ?_test(begin
                     %% Test execution semantics
                     SeqPattern = wfnet_patterns:sequence([task1, task2]),
                     InitialMarking = SeqPattern#EPC.marking,
                     
                     %% Execute first task
                     Enabled = wfnet_utils:enabled_transitions(SeqPattern, InitialMarking),
                     ?assertEqual([task1], Enabled),
                     
                     %% Fire transition
                     After1 = wfnet_utils:fire(SeqPattern, task1, InitialMarking),
                     ?assert(maps:is_key(task1, After1)),
                     
                     %% Check next task enabled
                     Enabled2 = wfnet_utils:enabled_transitions(SeqPattern, After1),
                     ?assertEqual([task2], Enabled2)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-2: Parallel Split Pattern Test
%%--------------------------------------------------------------------
parallel_split_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create parallel split pattern
                     ParPattern = wfnet_patterns:parallel_split(split_task, [branch1, branch2]),
                     
                     %% Validate pattern structure
                     ?assert(is_record(ParPattern, 'EPC')),
                     ?assertEqual(3, maps:size(ParPattern#EPC.transitions)),
                     ?assertEqual(4, maps:size(ParPattern#EPC.places)),
                     
                     %% Check split transition type
                     SplitProps = maps:get(split_task, ParPattern#EPC.transitions),
                     ?assertEqual(and_split, proplists:get_value(type, SplitProps))
                 end),
          ?_test(begin
                     %% Test parallel execution semantics
                     ParPattern = wfnet_patterns:parallel_split(split_task, [branch1, branch2]),
                     InitialMarking = ParPattern#EPC.marking,
                     
                     %% Fire split
                     AfterSplit = wfnet_utils:fire(ParPattern, split_task, InitialMarking),
                     ?assert(maps:is_key(branch1, AfterSplit)),
                     ?assert(maps:is_key(branch2, AfterSplit)),
                     
                     %% Both branches should be enabled
                     Enabled = wfnet_utils:enabled_transitions(ParPattern, AfterSplit),
                     ?assert(lists:member(branch1, Enabled)),
                     ?assert(lists:member(branch2, Enabled))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-3: Synchronization Pattern Test
%%--------------------------------------------------------------------
synchronization_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create synchronization pattern
                     SyncPattern = wfnet_patterns:synchronization(join_task, [branch1, branch2]),
                     
                     %% Validate pattern structure
                     ?assert(is_record(SyncPattern, 'EPC')),
                     ?assertEqual(3, maps:size(SyncPattern#EPC.transitions)),
                     ?assertEqual(4, maps:size(SyncPattern#EPC.places)),
                     
                     %% Check join transition type
                     JoinProps = maps:get(join_task, SyncPattern#EPC.transitions),
                     ?assertEqual(and_join, proplists:get_value(type, JoinProps))
                 end),
          ?_test(begin
                     %% Test synchronization semantics
                     SyncPattern = wfnet_patterns:synchronization(join_task, [branch1, branch2]),
                     
                     %% Simulate both branches completed
                     Marking = #{join_task => [branch1, branch2]},
                     
                     %% Join should be enabled
                     Enabled = wfnet_utils:enabled_transitions(SyncPattern, Marking),
                     ?assertEqual([join_task], Enabled)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-4: Exclusive Choice Pattern Test
%%--------------------------------------------------------------------
exclusive_choice_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create exclusive choice pattern
                     ChoicePattern = wfnet_patterns:exclusive_choice(
                         choice_task, 
                         [option1, option2]
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(ChoicePattern, 'EPC')),
                     ?assertEqual(3, maps:size(ChoicePattern#EPC.transitions)),
                     ?assertEqual(3, maps:size(ChoicePattern#EPC.places)),
                     
                     %% Check choice transition type
                     ChoiceProps = maps:get(choice_task, ChoicePattern#EPC.transitions),
                     ?assertEqual(xor_split, proplists:get_value(type, ChoiceProps))
                 end),
          ?_test(begin
                     %% Test exclusive choice semantics
                     ChoicePattern = wfnet_patterns:exclusive_choice(
                         choice_task, 
                         [option1, option2]
                     ),
                     InitialMarking = ChoicePattern#EPC.marking,
                     
                     %% Fire choice (simulating selection)
                     AfterChoice = wfnet_utils:fire(ChoicePattern, choice_task, InitialMarking),
                     
                     %% Only one option should be enabled
                     Enabled = wfnet_utils:enabled_transitions(ChoicePattern, AfterChoice),
                     ?assertEqual(1, length(Enabled))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-5: Simple Merge Pattern Test
%%--------------------------------------------------------------------
simple_merge_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create simple merge pattern
                     MergePattern = wfnet_patterns:simple_merge(
                         merge_task, 
                         [in1, in2]
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(MergePattern, 'EPC')),
                     ?assertEqual(3, maps:size(MergePattern#EPC.transitions)),
                     ?assertEqual(3, maps:size(MergePattern#EPC.places)),
                     
                     %% Check merge transition type
                     MergeProps = maps:get(merge_task, MergePattern#EPC.transitions),
                     ?assertEqual(xor_join, proplists:get_value(type, MergeProps))
                 end),
          ?_test(begin
                     %% Test simple merge semantics
                     MergePattern = wfnet_patterns:simple_merge(
                         merge_task, 
                         [in1, in2]
                     ),
                     
                     %% Simulate input place marked
                     Marking = #{merge_task => [in1]},
                     
                     %% Merge should be enabled
                     Enabled = wfnet_utils:enabled_transitions(MergePattern, Marking),
                     ?assertEqual([merge_task], Enabled)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-6: Multi-Choice Pattern Test
%%--------------------------------------------------------------------
multi_choice_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create multi-choice pattern
                     MultiChoicePattern = wfnet_patterns:multi_choice(
                         choice_task, 
                         [option1, option2, option3]
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(MultiChoicePattern, 'EPC')),
                     ?assertEqual(4, maps:size(MultiChoicePattern#EPC.transitions)),
                     ?assertEqual(4, maps:size(MultiChoicePattern#EPC.places)),
                     
                     %% Check transition type
                     ChoiceProps = maps:get(choice_task, MultiChoicePattern#EPC.transitions),
                     ?assertEqual(or_split, proplists:get_value(type, ChoiceProps))
                 end),
          ?_test(begin
                     %% Test multi-choice semantics
                     MultiChoicePattern = wfnet_patterns:multi_choice(
                         choice_task, 
                         [option1, option2, option3]
                     ),
                     InitialMarking = MultiChoicePattern#EPC.marking,
                     
                     %% Fire choice (can select multiple)
                     AfterChoice = wfnet_utils:fire(MultiChoicePattern, choice_task, InitialMarking),
                     
                     %% Multiple options could be enabled
                     Enabled = wfnet_utils:enabled_transitions(MultiChoicePattern, AfterChoice),
                     ?assert(length(Enabled) >= 1)
                 end)
         ]
     end}.

%%====================================================================
%% Advanced Pattern Tests (WCP-11 to WCP-43)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc WCP-11: Multiple Instances (No Synchronization)
%%--------------------------------------------------------------------
multiple_instances_no_sync_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create multiple instance pattern without sync
                     MiPattern = wfnet_patterns:multiple_instances_no_sync(
                         fun(X) -> X * 2 end,  %% Transformation function
                         [1, 2, 3],             %% Data
                         3                      %% Max instances
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(MiPattern, 'EPC')),
                     ?assertEqual(3, maps:size(MiPattern#EPC.transitions)),
                     ?assertEqual(5, maps:size(MiPattern#EPC.places))
                 end),
          ?_test(begin
                     %% Test execution semantics
                     MiPattern = wfnet_patterns:multiple_instances_no_sync(
                         fun(X) -> X + 1 end,
                         [1, 2],
                         2
                     ),
                     InitialMarking = MiPattern#EPC.marking,
                     
                     %% Should enable all instances
                     Enabled = wfnet_utils:enabled_transitions(MiPattern, InitialMarking),
                     ?assertEqual(2, length(Enabled))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-12: Multiple Instances (Static)
%%--------------------------------------------------------------------
multiple_instances_static_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create static multiple instance pattern
                     MiStaticPattern = wfnet_patterns:multiple_instances_static(
                         fun(X) -> X * 2 end,
                         3,           %% Instance count
                         [1, 2, 3]    %% Data
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(MiStaticPattern, 'EPC')),
                     ?assertEqual(3, maps:size(MiStaticPattern#EPC.transitions))
                 end),
          ?_test(begin
                     %% Test static instance management
                     MiStaticPattern = wfnet_patterns:multiple_instances_static(
                         fun(X) -> X end,
                         3,
                         [a, b, c]
                     ),
                     
                     %% Check instance state
                     PatternState = wfnet_utils:get_pattern_state(MiStaticPattern),
                     ?assertEqual(3, PatternState#pattern_state.max_instances)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-13: Multiple Instances (Runtime)
%%--------------------------------------------------------------------
multiple_instances_runtime_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create runtime multiple instance pattern
                     MiRuntimePattern = wfnet_patterns:multiple_instances_runtime(
                         fun(X) -> X * 2 end,
                         fun() -> length(erlang:get(data_queue)) end,  %% Count function
                         [1, 2, 3]  %% Initial data
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(MiRuntimePattern, 'EPC')),
                     ?assertNotEqual(undefined, MiRuntimePattern#EPC.conditions)
                 end),
          ?_test(begin
                     %% Test dynamic instance creation
                     Self = self(),
                     CountFun = fun() -> erlang:get(instance_count) end,
                     
                     MiRuntimePattern = wfnet_patterns:multiple_instances_runtime(
                         fun(X) -> {result, X} end,
                         CountFun,
                         [1, 2, 3]
                     ),
                     
                     %% Should respond to dynamic data
                     erlang:put(instance_count, 3),
                     Enabled = wfnet_utils:enabled_transitions(MiRuntimePattern, MiRuntimePattern#EPC.marking),
                     ?assert(length(Enabled) > 0)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-14: Deferred Choice
%%--------------------------------------------------------------------
deferred_choice_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create deferred choice pattern
                     DeferredPattern = wfnet_patterns:deferred_choice(
                         fun() -> option_a end,  %% Option A selector
                         fun() -> option_b end,  %% Option B selector
                         fun(Choice) -> Choice =:= option_a end  %% Validator
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(DeferredPattern, 'EPC')),
                     ?assertNotEqual(undefined, DeferredPattern#EPC.conditions)
                 end),
          ?_test(begin
                     %% Test deferred choice semantics
                     DeferredPattern = wfnet_patterns:deferred_choice(
                         fun() -> option_a end,
                         fun() -> option_b end,
                         fun(Choice) -> Choice =:= option_a end
                     ),
                     InitialMarking = DeferredPattern#EPC.marking,
                     
                     %% Should enable selector functions
                     Enabled = wfnet_utils:enabled_transitions(DeferredPattern, InitialMarking),
                     ?assert(length(Enabled) > 0)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc WCP-15: Interleaved Routing
%%--------------------------------------------------------------------
interleaved_routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create interleaved routing pattern
                     Branches = #{a => fun() -> a end, b => fun() -> b end},
                     InterleavedPattern = wfnet_patterns:interleaved_routing(
                         Branches,
                         undefined  %% No initial choice
                     ),
                     
                     %% Validate pattern structure
                     ?assert(is_record(InterleavedPattern, 'EPC')),
                     ?assertEqual(maps:size(Branches), maps:size(InterleavedPattern#EPC.transitions))
                 end),
          ?_test(begin
                     %% Test interleaved execution
                     Branches = #{a => fun() -> a end, b => fun() -> b end},
                     InterleavedPattern = wfnet_patterns:interleaved_routing(
                         Branches,
                         undefined
                     ),
                     
                     %% Should enable all branches initially
                     Enabled = wfnet_utils:enabled_transitions(InterleavedPattern, InterleavedPattern#EPC.marking),
                     ?assert(length(Enabled) == maps:size(Branches))
                 end)
         ]
     end}.

%%====================================================================
%% State Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test pattern state management
%%--------------------------------------------------------------------
pattern_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create sequence pattern
                     SeqPattern = wfnet_patterns:sequence([task1, task2]),
                     
                     %% Get initial state
                     InitialState = wfnet_utils:get_pattern_state(SeqPattern),
                     ?assert(is_record(InitialState, pattern_state)),
                     ?assertEqual(sequence, InitialState#pattern_state.pattern_type),
                     ?assertEqual(0, InitialState#pattern_state.instance_count)
                 end),
          ?_test(begin
                     %% Test state updates
                     SeqPattern = wfnet_patterns:sequence([task1, task2]),
                     InitialState = wfnet_utils:get_pattern_state(SeqPattern),
                     
                     %% Simulate execution
                     UpdatedState = InitialState#pattern_state{
                         instance_count = 1,
                         active_instances = [task1],
                         completed_instances = []
                     },
                     
                     ?assertEqual(1, UpdatedState#pattern_state.instance_count),
                     ?assertEqual([task1], UpdatedState#pattern_state.active_instances)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pattern completion detection
%%--------------------------------------------------------------------
pattern_completion_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create simple sequence pattern
                     SeqPattern = wfnet_patterns:sequence([task1, task2]),
                     
                     %% Start state - not complete
                     ?assertNotEqual(
                         completed,
                         wfnet_utils:get_pattern_status(SeqPattern)
                     ),
                     
                     %% Simulate both tasks completed
                     CompleteMarking = SeqPattern#EPC.marking,
                     CompleteMarking1 = maps:put(task1, [p_complete], CompleteMarking),
                     CompleteMarking2 = maps:put(task2, [p_complete], CompleteMarking1),
                     
                     %% Should be complete
                     ?assertEqual(
                         completed,
                         wfnet_utils:get_pattern_status(SeqPattern, CompleteMarking2)
                     )
                 end),
          ?_test(begin
                     %% Test completion with conditions
                     SeqPattern = wfnet_patterns:sequence([task1]),
                     CompleteMarking = maps:put(task1, [p_complete], SeqPattern#EPC.marking),
                     
                     %% Check completion with custom conditions
                     IsComplete = wfnet_utils:is_pattern_complete(SeqPattern, CompleteMarking),
                     ?assert(IsComplete)
                 end)
         ]
     end}.

%%====================================================================
%% Error Handling and Recovery Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test pattern error handling
%%--------------------------------------------------------------------
pattern_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create pattern that throws error
                     ErrorPattern = wfnet_patterns:error_task(error_pattern),
                     
                     %% Test error handling
                     ?assertException(
                         error,
                         task_error,
                         wfnet_utils:fire(ErrorPattern, error_task, ErrorPattern#EPC.marking)
                     )
                 end),
          ?_test(begin
                     %% Test pattern with retry
                     RetryPattern = wfnet_patterns:retry_task(
                         retry_task,
                         fun() -> throw(retry_error) end,
                         3  %% Max retries
                     ),
                     
                     %% Should attempt retries
                     ?assertException(
                         error,
                         max_retries_exceeded,
                         wfnet_utils:fire(RetryPattern, retry_task, RetryPattern#EPC.marking)
                     )
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pattern recovery mechanisms
%%--------------------------------------------------------------------
pattern_recovery_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create recoverable pattern
                     RecoveryPattern = wfnet_patterns:recoverable_task(
                         recover_task,
                         fun(State) -> 
                             %% Recovery function
                             case State of
                                 error -> ok;
                                 _ -> State
                             end
                         end
                     ),
                     
                     %% Test recovery
                     ErrorMarking = maps:put(recover_task, [error], RecoveryPattern#EPC.marking),
                     RecoveryMarking = wfnet_utils:recover_pattern(RecoveryPattern, ErrorMarking),
                     
                     %% Should recover from error state
                     ?assertEqual(
                         ok,
                         wfnet_utils:get_pattern_status(RecoveryPattern, RecoveryMarking)
                     )
                 end)
         ]
     end}.

%%====================================================================
%% Performance Tests for Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test pattern creation performance
%%--------------------------------------------------------------------
pattern_creation_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Test creation of many simple patterns
                     Start = erlang:monotonic_time(microsecond),
                     
                     %% Create 50 sequence patterns
                     Patterns = [wfnet_patterns:sequence([<<"task_", (integer_to_binary(I))/binary>>])
                               || I <- lists:seq(1, 50)],
                     
                     End = erlang:monotonic_time(microsecond),
                     Duration = (End - Start) / 1000.0,  %% milliseconds
                     
                     ?assert(length(Patterns) == 50),
                     ?assert(Duration < 1000),  %% Should complete in under 1 second
                     io:format("50 pattern creations took ~.2f ms~n", [Duration])
                 end),
          ?_test(begin
                     %% Test complex pattern creation
                     Start = erlang:monotonic_time(microsecond),
                     
                     %% Create nested pattern
                     NestedPattern = wfnet_patterns:parallel_split(
                         split,
                         [
                             wfnet_patterns:sequence([branch1_1, branch1_2]),
                             wfnet_patterns:sequence([branch2_1, branch2_2])
                         ]
                     ),
                     
                     End = erlang:monotonic_time(microsecond),
                     Duration = (End - Start) / 1000.0,
                     
                     ?assert(is_record(NestedPattern, 'EPC')),
                     ?assert(Duration < 100),  %% Should complete quickly
                     io:format("Nested pattern creation took ~.2f ms~n", [Duration])
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pattern execution performance
%%--------------------------------------------------------------------
pattern_execution_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create simple sequence for performance test
                     Pattern = wfnet_patterns:sequence(
                         [<<"task_", (integer_to_binary(I))/binary>> 
                          || I <- lists:seq(1, 20)]
                     ),
                     
                     %% Measure execution time
                     Start = erlang:monotonic_time(microsecond),
                     Marking = Pattern#EPC.marking,
                     
                     %% Execute sequence step by step
                     CurrentMarking = lists:foldl(
                         fun(Trans, Acc) ->
                             wfnet_utils:fire(Pattern, Trans, Acc)
                         end,
                         Marking,
                         [<<"task_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 20)]
                     ),
                     
                     End = erlang:monotonic_time(microsecond),
                     Duration = (End - Start) / 1000.0,  %% milliseconds
                     
                     ?assertEqual(
                         completed,
                         wfnet_utils:get_pattern_status(Pattern, CurrentMarking)
                     ),
                     ?assert(Duration < 500),  %% Should execute quickly
                     io:format("20-step sequence execution took ~.2f ms~n", [Duration])
                 end)
         ]
     end}.
