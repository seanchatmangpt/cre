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
%% @doc Workflow Net Composition Tests
%%
%% Comprehensive test suite for wfnet_compose module.
%% Tests all composition operators and pattern adapters.
%% @end
%% -------------------------------------------------------------------

-module(wfnet_compose_tests).
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
%% Composition Operator Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test sequence composition operator
%%--------------------------------------------------------------------
sequence_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create basic tasks
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     
                     %% Compose sequence
                     SeqWorkflow = wfnet_compose:sequence(TaskA, TaskB),
                     
                     %% Validate structure
                     ?assert(is_record(SeqWorkflow, 'EPC')),
                     ?assertEqual(2, maps:size(SeqWorkflow#EPC.transitions)),
                     ?assertEqual(3, maps:size(SeqWorkflow#EPC.places)),
                     
                     %% Check sequence semantics
                     Transitions = SeqWorkflow#EPC.transitions,
                     ?assert(maps:is_key(task_a, Transitions)),
                     ?assert(maps:is_key(task_b, Transitions))
                 end),
          ?_test(begin
                     %% Test sequence with parameters
                     TaskA = wfnet_compose:task(validate_order, [{timeout, 5000}]),
                     TaskB = wfnet_compose:task(process_order),
                     SeqWorkflow = wfnet_compose:sequence(TaskA, TaskB),
                     
                     %% Validate properties are preserved
                     Transitions = SeqWorkflow#EPC.transitions,
                     ValidateProps = maps:get(validate_order, Transitions),
                     ?assertEqual(5000, proplists:get_value(timeout, ValidateProps, 0))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test parallel composition operator
%%--------------------------------------------------------------------
parallel_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create tasks for parallel execution
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     
                     %% Compose parallel workflow
                     ParWorkflow = wfnet_compose:parallel(TaskA, TaskB),
                     
                     %% Validate structure
                     ?assert(is_record(ParWorkflow, 'EPC')),
                     ?assertEqual(2, maps:size(ParWorkflow#EPC.transitions)),
                     ?assertEqual(4, maps:size(ParWorkflow#EPC.places)),
                     
                     %% Check parallel structure (should have split and join)
                     ?assertNotEqual(undefined, 
                                   wfnet_compose:find_split(ParWorkflow)),
                     ?assertNotEqual(undefined, 
                                   wfnet_compose:find_join(ParWorkflow))
                 end),
          ?_test(begin
                     %% Test parallel with multiple branches
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     TaskC = wfnet_compose:task(task_c),
                     ParWorkflow = wfnet_compose:parallel([TaskA, TaskB, TaskC]),
                     
                     %% Validate multiple branches
                     ?assertEqual(3, maps:size(ParWorkflow#EPC.transitions)),
                     ?assertEqual(6, maps:size(ParWorkflow#EPC.places))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test choice composition operator
%%--------------------------------------------------------------------
choice_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create exclusive choice tasks
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     
                     %% Compose choice workflow
                     ChoiceWorkflow = wfnet_compose:choice(TaskA, TaskB),
                     
                     %% Validate structure
                     ?assert(is_record(ChoiceWorkflow, 'EPC')),
                     ?assertEqual(2, maps:size(ChoiceWorkflow#EPC.transitions)),
                     ?assertEqual(3, maps:size(ChoiceWorkflow#EPC.places)),
                     
                     %% Check choice semantics (should have XOR split)
                     Split = wfnet_compose:find_split(ChoiceWorkflow),
                     ?assertNotEqual(undefined, Split),
                     SplitProps = maps:get(Split, ChoiceWorkflow#EPC.transitions),
                     ?assertEqual(xor_split, proplists:get_value(type, SplitProps))
                 end),
          ?_test(begin
                     %% Test choice with condition
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     Condition = fun(Data) -> maps:get(priority, Data) > 5 end,
                     ChoiceWorkflow = wfnet_compose:choice(TaskA, TaskB, Condition),
                     
                     %% Validate condition is attached
                     ?assertNotEqual(undefined, ChoiceWorkflow#EPC.conditions)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test loop composition operator
%%--------------------------------------------------------------------
loop_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create task with loop
                     Task = wfnet_compose:task(process_item),
                     Condition = fun(Data) -> maps:get(retry_count, Data) < 3 end,
                     LoopWorkflow = wfnet_compose:loop(Task, Condition),
                     
                     %% Validate structure
                     ?assert(is_record(LoopWorkflow, 'EPC')),
                     ?assertEqual(2, maps:size(LoopWorkflow#EPC.transitions)),
                     ?assertEqual(3, maps:size(LoopWorkflow#EPC.places)),
                     
                     %% Check loop structure (should have loopback connection)
                     ?assertNotEqual(undefined, 
                                   wfnet_compose:find_loopback(LoopWorkflow))
                 end),
          ?_test(begin
                     %% Test while loop
                     Task = wfnet_compose:task(iterate),
                     Condition = fun(Data) -> maps:get(counter, Data) > 0 end,
                     LoopWorkflow = wfnet_compose:loop(Task, Condition, while),
                     
                     %% Validate type
                     ?assertNotEqual(undefined, LoopWorkflow#EPC.conditions)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test merge composition operator
%%--------------------------------------------------------------------
merge_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create multiple tasks to merge
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     
                     %% Compose merge workflow
                     MergeWorkflow = wfnet_compose:merge([TaskA, TaskB]),
                     
                     %% Validate structure
                     ?assert(is_record(MergeWorkflow, 'EPC')),
                     ?assertEqual(2, maps:size(MergeWorkflow#EPC.transitions)),
                     ?assertEqual(3, maps:size(MergeWorkflow#EPC.places)),
                     
                     %% Check merge semantics (should have join)
                     Join = wfnet_compose:find_join(MergeWorkflow),
                     ?assertNotEqual(undefined, Join),
                     JoinProps = maps:get(Join, MergeWorkflow#EPC.transitions),
                     ?assertEqual(or_join, proplists:get_value(type, JoinProps))
                 end)
         ]
     end}.

%%====================================================================
%% Pattern Adapter Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test sequence pattern adapter
%%--------------------------------------------------------------------
sequence_adapter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create YAWL sequence pattern
                     YawlSeq = yawl_pattern_reference:sequence([task1, task2]),
                     
                     %% Convert to wfnet workflow
                     WfnetSeq = wfnet_compose:from_yawl(YawlSeq),
                     
                     %% Validate conversion
                     ?assert(is_record(WfnetSeq, 'EPC')),
                     ?assertEqual(2, maps:size(WfnetSeq#EPC.transitions))
                 end),
          ?_test(begin
                     %% Test bidirectional conversion
                     Original = wfnet_compose:sequence(
                         wfnet_compose:task(task_a),
                         wfnet_compose:task(task_b)
                     ),
                     Converted = wfnet_compose:from_yawl(
                         wfnet_compose:to_yawl(Original)
                     ),
                     %% Should be equivalent
                     ?assertEqual(length(Original#EPC.transitions),
                                 length(Converted#EPC.transitions))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test parallel pattern adapter
%%--------------------------------------------------------------------
parallel_adapter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create YAWL parallel split pattern
                     YawlPar = yawl_pattern_reference:parallel_split(),
                     
                     %% Convert to wfnet workflow
                     WfnetPar = wfnet_compose:from_yawl(YawlPar),
                     
                     %% Validate parallel structure
                     ?assert(is_record(WfnetPar, 'EPC')),
                     Split = wfnet_compose:find_split(WfnetPar),
                     ?assertNotEqual(undefined, Split),
                     SplitProps = maps:get(Split, WfnetPar#EPC.transitions),
                     ?assertEqual(and_split, proplists:get_value(type, SplitProps))
                 end)
         ]
     end}.

%%====================================================================
%% Complex Composition Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test nested compositions
%%--------------------------------------------------------------------
nested_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create nested workflow: (A || B) -> C
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     TaskC = wfnet_compose:task(task_c),
                     
                     %% Parallel then sequence
                     Nested = wfnet_compose:sequence(
                         wfnet_compose:parallel(TaskA, TaskB),
                         TaskC
                     ),
                     
                     %% Validate complex structure
                     ?assert(is_record(Nested, 'EPC')),
                     ?assertEqual(3, maps:size(Nested#EPC.transitions)),
                     ?assertEqual(5, maps:size(Nested#EPC.places)),
                     
                     %% Should have both split and join
                     ?assertNotEqual(undefined, wfnet_compose:find_split(Nested)),
                     ?assertNotEqual(undefined, wfnet_compose:find_join(Nested))
                 end),
          ?_test(begin
                     %% Test deeper nesting: A -> (B || C -> D)
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     TaskC = wfnet_compose:task(task_c),
                     TaskD = wfnet_compose:task(task_d),
                     
                     %% Inner sequence: C -> D
                     InnerSeq = wfnet_compose:sequence(TaskC, TaskD),
                     
                     %% Outer: A -> (B || inner)
                     Nested = wfnet_compose:sequence(
                         TaskA,
                         wfnet_compose:parallel(TaskB, InnerSeq)
                     ),
                     
                     ?assertEqual(4, maps:size(Nested#EPC.transitions))
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test dynamic composition
%%--------------------------------------------------------------------
dynamic_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create dynamic workflow builder
                     Builder = wfnet_compose:builder(),
                     
                     %% Add components dynamically
                     Builder1 = wfnet_compose:add_task(Builder, task1, []),
                     Builder2 = wfnet_compose:add_task(Builder1, task2, []),
                     Builder3 = wfnet_compose:connect(
                         Builder2, 
                         task1, 
                         task2, 
                        [{type, sequence}]
                     ),
                     
                     %% Build final workflow
                     Workflow = wfnet_compose:build(Builder3),
                     
                     ?assert(is_record(Workflow, 'EPC')),
                     ?assertEqual(2, maps:size(Workflow#EPC.transitions))
                 end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test invalid composition handling
%%--------------------------------------------------------------------
invalid_composition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Test sequence with missing end
                     Task = wfnet_compose:task(task_a),
                     %% Should fail or create incomplete workflow
                     ?assertException(
                         error,
                         badarg,
                         wfnet_compose:sequence(Task, undefined)
                     )
                 end),
          ?_test(begin
                     %% Test parallel with empty list
                     ?assertException(
                         error,
                         badarg,
                         wfnet_compose:parallel([])
                     )
                 end),
          ?_test(begin
                     %% Test invalid connection
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     Builder = wfnet_compose:add_task(wfnet_compose:builder(), task_a, []),
                     
                     %% Should fail trying to connect non-existent transition
                     ?assertException(
                         error,
                         badarg,
                         wfnet_compose:connect(Builder, task_a, task_b, [])
                     )
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test validation errors in composition
%%--------------------------------------------------------------------
validation_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Create conflicting transitions
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_a),  %% Duplicate ID
                     
                     %% Should detect duplicate
                     ?assertException(
                         error,
                         duplicate_transition,
                         wfnet_compose:merge([TaskA, TaskB])
                     )
                 end),
          ?_test(begin
                     %% Test invalid condition
                     Task = wfnet_compose:task(task_a),
                     InvalidCond = fun(_) -> throw(bad_condition) end,
                     
                     ?assertException(
                         error,
                         bad_condition,
                         wfnet_compose:loop(Task, InvalidCond)
                     )
                 end)
         ]
     end}.

%%====================================================================
%% Performance Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test composition performance
%%--------------------------------------------------------------------
performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     %% Build large workflow composition
                     %% This measures composition time, not execution time
                     Start = erlang:monotonic_time(microsecond),
                     
                     %% Create 100 task sequence
                     Tasks = [wfnet_compose:task(<<"task_", (integer_to_binary(I))/binary>>) 
                             || I <- lists:seq(1, 100)],
                     
                     %% Sequential composition
                     Workflow = lists:foldl(
                         fun(Task, Acc) -> wfnet_compose:sequence(Acc, Task) end,
                         wfnet_compose:task(start),
                         Tasks
                     ),
                     
                     End = erlang:monotonic_time(microsecond),
                     Duration = (End - Start) / 1000.0,  %% milliseconds
                     
                     ?assert(is_record(Workflow, 'EPC')),
                     ?assert(Duration < 1000),  %% Should complete in under 1 second
                     io:format("100-task sequence composition took ~.2f ms~n", [Duration])
                 end),
          ?_test(begin
                     %% Test parallel composition performance
                     Start = erlang:monotonic_time(microsecond),
                     
                     Tasks = [wfnet_compose:task(<<"p_", (integer_to_binary(I))/binary>>) 
                             || I <- lists:seq(1, 50)],
                     
                     Workflow = wfnet_compose:parallel(Tasks),
                     
                     End = erlang:monotonic_time(microsecond),
                     Duration = (End - Start) / 1000.0,
                     
                     ?assert(is_record(Workflow, 'EPC')),
                     ?assert(Duration < 500),  %% Should complete in under 500ms
                     io:format("50-task parallel composition took ~.2f ms~n", [Duration])
                 end)
         ]
     end}.

%%====================================================================
%% Property-Based Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test composition properties using PropEr
%% Note: This requires PropEr to be installed
%%--------------------------------------------------------------------
composition_property_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         %% This would require PropEr framework
         %% For now, we'll test manually
         [
          ?_test(begin
                     %% Test composition associativity
                     %% (A :: B) :: C should be equivalent to A :: (B :: C)
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     TaskC = wfnet_compose:task(task_c),
                     
                     LeftAssoc = wfnet_compose:sequence(
                         wfnet_compose:sequence(TaskA, TaskB),
                         TaskC
                     ),
                     
                     RightAssoc = wfnet_compose:sequence(
                         TaskA,
                         wfnet_compose:sequence(TaskB, TaskC)
                     ),
                     
                     %% Should have same number of transitions
                     ?assertEqual(
                         maps:size(LeftAssoc#EPC.transitions),
                         maps:size(RightAssoc#EPC.transitions)
                     )
                 end),
          ?_test(begin
                     %% Test parallel commutativity
                     TaskA = wfnet_compose:task(task_a),
                     TaskB = wfnet_compose:task(task_b),
                     
                     Par1 = wfnet_compose:parallel(TaskA, TaskB),
                     Par2 = wfnet_compose:parallel(TaskB, TaskA),
                     
                     %% Should have same structure
                     ?assertEqual(
                         maps:size(Par1#EPC.transitions),
                         maps:size(Par2#EPC.transitions)
                     )
                 end)
         ]
     end}.
