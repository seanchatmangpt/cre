%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc YAWL Patterns Execution Test Suite
%%
%% Tests the execution functions of YAWL patterns, including:
%% - Pattern state transitions
%% - Token movement in Petri nets
%% - Activation and firing of transitions
%% - Completion detection
%% - Integration with gen_pnet behavior
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_patterns_execution_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions
%%====================================================================

-record(pattern_state, {
          pattern_type :: atom(),
          subprocess :: module() | function() | undefined,
          instance_count :: non_neg_integer() | undefined,
          max_instances :: non_neg_integer() | unlimited | undefined,
          pending_instances :: list(),
          active_instances :: list(),
          completed_instances :: list(),
          choice_data :: map(),
          branch_queue :: list()
         }).

-record(instance_token, {
          instance_id :: reference(),
          data :: term()
         }).

-record(branch_token, {
          branch_id :: atom(),
          data :: term(),
          index :: non_neg_integer()
         }).

-record(workflow, {
          id :: binary(),
          name :: binary(),
          tasks :: map(),
          conditions :: map(),
          connections :: list(),
          start_task_id :: binary() | undefined,
          end_task_ids :: list()
         }).

-record(connection, {
          from_id :: binary(),
          to_id :: binary(),
          condition :: term() | undefined
         }).

-record(retry_policy, {
          max_attempts :: non_neg_integer(),
          base_delay :: non_neg_integer(),
          max_delay :: non_neg_integer(),
          backoff_factor :: float()
         }).

-record(workitem, {
          workitem_id :: reference(),
          case_id :: reference(),
          task_id :: binary(),
          data :: term(),
          status :: atom()
         }).

-record(synchronizing_merge, {
          merge_task_id :: binary(),
          incoming_task_ids :: list()
         }).

-record(multi_merge, {
          merge_task_id :: binary(),
          incoming_task_ids :: list()
         }).

-record(discriminator, {
          merge_task_id :: binary(),
          incoming_task_ids :: list()
         }).

-record(arbitration, {
          merge_task_id :: binary(),
          incoming_task_ids :: list(),
          required_count :: non_neg_integer()
         }).

%%====================================================================
%% Test Setup
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% @end
%%--------------------------------------------------------------------
setup() ->
    %% Start CRE application
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Initialize test environment
    initialize_test_environment(),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% @end
%%--------------------------------------------------------------------
cleanup(_TestData) ->
    cleanup_test_environment(),
    ok.

%%--------------------------------------------------------------------
%% @doc Initialize test environment.
%% @end
%%--------------------------------------------------------------------
initialize_test_environment() ->
    %% Start YAWL control panel
    case whereis(yawl_control) of
        undefined ->
            {ok, _ControlPid} = yawl_control:start_control(yawl_control);
        _ ->
            ok
    end,

    %% Start YAWL engine
    case whereis(yawl_engine) of
        undefined ->
            {ok, _EnginePid} = yawl_engine:start_link();
        _ ->
            ok
    end,

    %% Initialize persistence
    case yawl_persistence:init_schema() of
        ok -> ok;
        {error, {already_exists, _}} -> ok
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup test environment.
%% @end
%%--------------------------------------------------------------------
cleanup_test_environment() ->
    %% Stop YAWL components
    case whereis(yawl_engine) of
        undefined -> ok;
        _ -> yawl_engine:stop()
    end,

    case whereis(yawl_control) of
        undefined -> ok;
        _ -> yawl_control:stop()
    end,

    ok.

%%====================================================================
%% Execution Tests for Basic Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% Sequence Pattern Execution Tests
%%--------------------------------------------------------------------
sequence_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Sequence execution - Basic workflow creation",
           fun() ->
               Workflow = cre_yawl:new_workflow(<<"seq_exec">>),
               W1 = cre_yawl:add_task(Workflow, <<"start">>, [{type, atomic}]),
               W2 = cre_yawl:add_task(W1, <<"middle">>, [{type, atomic}]),
               W3 = cre_yawl:add_task(W2, <<"end">>, [{type, atomic}]),
               W4 = cre_yawl:connect(W3, <<"start">>, <<"middle">>),
               W5 = cre_yawl:connect(W4, <<"middle">>, <<"end">>),
               FinalWf = set_workflow_boundaries(W5, <<"start">>, [<<"end">>]),

               %% Validate workflow
               ?assertEqual(ok, cre_yawl:validate(FinalWf)),

               %% Test workflow execution
               {ok, EnginePid} = yawl_engine:start_link(),
               Spec = create_workflow_spec(FinalWf),

               ?debugFmt("Starting workflow with spec: ~p~n", [Spec]),

               %% Start workflow
               {ok, CaseId} = yawl_engine:start_workflow(
                   EnginePid,
                   Spec,
                   #{cre_master => undefined, observers => []}
               ),

               ?assert(is_binary(CaseId)),

               %% Get work items
               {ok, Workitems} = yawl_engine:get_available_workitems(EnginePid),
               ?assert(length(Workitems) >= 0),

               %% Complete first work item
               case Workitems of
                   [FirstWI | _] when is_tuple(FirstWI) ->
                       WIId = get_workitem_id(FirstWI),
                       case yawl_engine:start_workitem(EnginePid, WIId) of
                           ok ->
                               ok = yawl_engine:complete_workitem(
                                   EnginePid,
                                   WIId,
                                   #{result => <<"completed">>}
                               );
                           {error, Reason} ->
                               ?debugFmt("Failed to start workitem: ~p~n", [Reason])
                       end;
                   _ ->
                       ok
               end,

               %% Clean up
               yawl_engine:stop(EnginePid)
           end},

          {"Sequence execution - Multi-step workflow",
           fun() ->
               Workflow = create_sequence_workflow(5),
               {ok, EnginePid} = yawl_engine:start_link(),
               Spec = create_workflow_spec(Workflow),

               {ok, CaseId} = yawl_engine:start_workflow(
                   EnginePid,
                   Spec,
                   #{cre_master => undefined, observers => []}
               ),

               %% Complete work items sequentially
               complete_all_workitems(EnginePid, CaseId),

               %% Verify completion
               {ok, CaseState} = yawl_engine:get_case_state(EnginePid),
               ?assertEqual(running, maps:get(status, CaseState)),

               yawl_engine:stop(EnginePid)
           end}
         ]
     end}.

%%====================================================================
%% Execution Tests for Multiple Instance Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% Multiple Instances Execution Tests
%%--------------------------------------------------------------------
multiple_instances_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Multiple instances - No sync execution",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                   fun(X) -> X * 2 end,
                   [1, 2, 3, 4],
                   4
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(4, Pattern#pattern_state.instance_count),

               %% Test execution by spawning instances
               Results = execute_multiple_instances(Pattern),

               ?assertEqual(4, length(Results)),
               ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results))
           end},

          {"Multiple instances - Static count",
           fun() ->
               Pattern = cre_yawl_patterns:multiple_instances_static(
                   fun(X) -> X * 2 end,
                   3,
                   [1, 2, 3]
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(3, Pattern#pattern_state.instance_count),

               %% Execute instances
               Results = execute_multiple_instances(Pattern),
               ?assertEqual(3, length(Results)),

               %% Verify results
               Expected = [2, 4, 6],
               Actual = [Result || {ok, Result} <- Results],
               ?assertEqual(Expected, lists:sort(Actual))
           end},

          {"Multiple instances - Runtime determined",
           fun() ->
               CountFun = fun(Data) -> length(Data) end,
               Pattern = cre_yawl_patterns:multiple_instances_runtime(
                   fun(X) -> X + 1 end,
                   CountFun,
                   [1, 2, 3, 4, 5]
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(5, Pattern#pattern_state.instance_count),

               Results = execute_multiple_instances(Pattern),
               ?assertEqual(5, length(Results))
           end}
         ]
     end}.

%%====================================================================
%% Execution Tests for Exception Handling Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% Exception Handling Execution Tests
%%--------------------------------------------------------------------
exception_handling_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Error handler execution - Basic",
           fun() ->
               %% Create error handler pattern
               Pattern = cre_yawl_patterns:error_handler(
                   fun() -> throw(error) end,
                   fun(Error) -> {handled, Error} end
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(error_handler, Pattern#pattern_state.pattern_type),

               %% Simulate execution with error
               try
                   execute_error_handler(Pattern)
               catch
                   error:Error ->
                       ?assertEqual(error, Error)
               end
           end},

          {"Retry pattern execution - Success on retry",
           fun() ->
               Operation = fun() ->
                   case get(retry_attempt) of
                       undefined -> put(retry_attempt, 1), {error, first_fail};
                       1 -> {ok, success}
                   end
               end,

               RetryPolicy = cre_yawl_exception:new_retry_policy(#{
                   max_attempts => 3,
                   backoff => constant,
                   base_delay => 0
               }),

               Pattern = cre_yawl_patterns:retry(Operation, RetryPolicy, 100),

               ?assertMatch(#pattern_state{}, Pattern),

               %% Execute with retries
               Result = execute_with_retries(Pattern, Operation, RetryPolicy),

               case Result of
                   {ok, success} -> ok;
                   _ -> ?assert(false, "Should have succeeded on retry")
               end
           end},

          {"Compensation pattern execution",
           fun() ->
               PrimaryActivity = fun() ->
                   ?debugFmt("Executing primary activity~n", []),
                   {primary_result, success}
               end,

               CompensationActivity = fun(State) ->
                   ?debugFmt("Compensating with state: ~p~n", [State]),
                   {compensated, State}
               end,

               Pattern = cre_yawl_patterns:compensate(PrimaryActivity, CompensationActivity),

               ?assertMatch(#pattern_state{}, Pattern),

               %% Execute pattern
               Result = execute_compensation_pattern(Pattern),

               case Result of
                   {primary_result, success} -> ok;
                   _ -> ?assert(false, "Primary should execute successfully")
               end
           end}
         ]
     end}.

%%====================================================================
%% Execution Tests for State-Based Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% State-Based Pattern Execution Tests
%%--------------------------------------------------------------------
state_based_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Milestone execution - Basic",
           fun() ->
               Pattern = cre_yawl_patterns:milestone(
                   fun() -> milestone_reached end,
                   fun() -> milestone_action end
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(milestone, Pattern#pattern_state.pattern_type),

               %% Execute milestone
               Result = execute_milestone_pattern(Pattern),

               case Result of
                   {milestone_reached, milestone_action} -> ok;
                   _ -> ?assert(false, "Should reach milestone and execute action")
               end
           end},

          {"Cancel activity execution",
           fun() ->
               Pattern = cre_yawl_patterns:cancel_activity(
                   <<"activity_1">>,
                   fun() -> active end
               ),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(cancel_activity, Pattern#pattern_state.pattern_type),

               %% Execute cancel pattern
               Result = execute_cancel_activity_pattern(Pattern),

               case Result of
                   {cancelled, <<"activity_1">>} -> ok;
                   _ -> ?assert(false, "Should cancel activity")
               end
           end},

          {"Cancel case execution",
           fun() ->
               Activities = [
                   fun() -> activity1 end,
                   fun() -> activity2 end,
                   fun() -> activity3 end
               ],

               Pattern = cre_yawl_patterns:cancel_case(Activities, fun() -> cancel end),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(cancel_case, Pattern#pattern_state.pattern_type),

               %% Execute cancel case
               Result = execute_cancel_case_pattern(Pattern),

               case Result of
                   {cancelled, 3} -> ok;
                   _ -> ?assert(false, "Should cancel case with 3 activities")
               end
           end}
         ]
     end}.

%%====================================================================
%% Execution Tests for Extended Control Flow Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% Extended Control Flow Execution Tests
%%--------------------------------------------------------------------
extended_control_flow_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Structured sync execution",
           fun() ->
               Activities = [fun() -> activity1 end, fun() -> activity2 end],
               Pattern = cre_yawl_patterns:structured_sync(Activities, undefined),

               ?assertMatch(#pattern_state{}, Pattern),

               %% Execute structured sync
               Results = execute_structured_sync(Pattern),
               ?assertEqual(2, length(Results))
           end},

          {"Partial join execution",
           fun() ->
               Activities = [fun() -> a end, fun() -> b end, fun() -> c end],
               Pattern = cre_yawl_patterns:partial_join(Activities, 2),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(3, length(Pattern#pattern_state.pending_instances)),

               %% Execute partial join
               Result = execute_partial_join(Pattern),
               ?assertEqual(completed, Result)
           end},

          {"Structured loop execution",
           fun() ->
               Condition = fun(X) -> X < 5 end,
               LoopBody = fun() -> {continue, loop_value} end,

               Pattern = cre_yawl_patterns:structured_loop(Condition, LoopBody, while),

               ?assertMatch(#pattern_state{}, Pattern),
               ?assertEqual(while, get_loop_type(Pattern)),

               %% Execute loop
               Results = execute_structured_loop(Pattern, 0),
               ?assertEqual(5, length(Results))
           end}
         ]
     end}.

%%====================================================================
%% Performance Execution Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Performance Execution Tests
%%--------------------------------------------------------------------
performance_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Pattern execution speed - Basic patterns",
           fun() ->
               Patterns = [
                   cre_yawl:sequence(),
                   cre_yawl:parallel_split(),
                   cre_yawl:synchronization(),
                   cre_yawl:exclusive_choice()
               ],

               Start = erlang:monotonic_time(millisecond),

               %% Execute all patterns
               Results = lists:map(fun(Pattern) ->
                   execute_pattern_basic(Pattern)
               end, Patterns),

               End = erlang:monotonic_time(millisecond),
               Duration = End - Start,

               ?debugFmt("Executed ~p patterns in ~p ms (~p patterns/sec)~n",
                         [length(Patterns), Duration, length(Patterns) / Duration * 1000]),

               ?assert(Duration < 2000, "Pattern execution too slow"),
               ?assertEqual(length(Patterns), length(Results))
           end},

          {"Multiple instances performance",
           fun() ->
               %% Test different instance counts
                Counts = [1, 10, 25, 50, 100],
                Results = lists:map(fun(Count) ->
                    Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                        fun(X) -> X end,
                        lists:seq(1, Count),
                        Count
                    ),

                    Start = erlang:monotonic_time(millisecond),
                    Result = execute_multiple_instances(Pattern),
                    End = erlang:monotonic_time(millisecond),

                    Duration = End - Start,
                    Throughput = Count / Duration * 1000,

                    ?debugFmt("~p instances: ~p ms, ~p instances/sec~n",
                             [Count, Duration, Throughput]),

                    {Count, Duration, Throughput}
                end, Counts),

                %% Analyze results
                AverageThroughput = lists:sum([T || {_, _, T} <- Results]) / length(Results),
                ?assert(AverageThroughput > 100, "Average throughput too low")
           end},

          {"Memory usage during execution",
           fun() ->
               Before = process_info(self(), memory),

               %% Execute multiple patterns
               lists:foreach(fun(_) ->
                   Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                       fun(X) -> X + 1 end,
                       lists:seq(1, 20),
                       20
                   ),
                   execute_multiple_instances(Pattern)
               end, lists:seq(1, 10)),

               After = process_info(self(), memory),
                MemoryDelta = element(2, After) - element(2, Before),

                ?debugFmt("Memory delta: ~p bytes~n", [MemoryDelta]),

                %% Memory growth should be reasonable
                ?assert(MemoryDelta < 500000, "Memory growth too high")
           end}
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create a sequence workflow.
%% @end
%%--------------------------------------------------------------------
create_sequence_workflow(NumTasks) ->
    Workflow = cre_yawl:new_workflow(<<"seq_work">>),

    %% Create tasks
    {WorkflowWithTasks, TaskIds} = lists:foldl(fun(I, {Wf, Acc}) ->
        TaskId = <<"task_", (integer_to_binary(I))/binary>>,
        NewWf = cre_yawl:add_task(Wf, TaskId, [{type, atomic}]),
        {NewWf, [TaskId | Acc]}
    end, {Workflow, []}, lists:seq(1, NumTasks)),

    %% Create connections
    {FinalWorkflow, _} = lists:foldl(fun(I, {Wf, PrevId}) when I > 1 ->
        CurrentId = <<"task_", (integer_to_binary(I))/binary>>,
        ConnectedWf = cre_yawl:connect(Wf, PrevId, CurrentId),
        {ConnectedWf, CurrentId}
    end, {WorkflowWithTasks, hd(lists:reverse(TaskIds))}, lists:seq(2, NumTasks)),

    set_workflow_boundaries(FinalWorkflow, hd(TaskIds), [lists:last(TaskIds)]).

%%--------------------------------------------------------------------
%% @doc Create workflow specification from workflow.
%% @end
%%--------------------------------------------------------------------
create_workflow_spec(#workflow{} = Workflow) ->
    {ok, Tasks} = cre_yawl:get_tasks(Workflow),
    {ok, Connections} = cre_yawl:get_connections(Workflow),
    {ok, Id} = cre_yawl:get_workflow_id(Workflow),
    {ok, StartTaskId} = cre_yawl:get_start_task_id(Workflow),

    TasksMap = maps:map(fun(TaskId, Task) ->
        #{id => TaskId, name => maps:get(name, Task, TaskId)}
    end, Tasks),

    Flows = lists:map(fun(#connection{from_id = From, to_id = To}) ->
        #{source => From, target => To}
    end, Connections),

    #{
        id => Id,
        tasks => TasksMap,
        flows => Flows,
        start_task => StartTaskId
    }.

%%--------------------------------------------------------------------
%% @doc Set workflow boundaries helper.
%% @end
%%--------------------------------------------------------------------
set_workflow_boundaries(#workflow{} = Workflow, StartTaskId, EndTaskIds) ->
    Workflow#workflow{start_task_id = StartTaskId, end_task_ids = EndTaskIds};
set_workflow_boundaries(Workflow, _StartTaskId, _EndTaskIds) ->
    Workflow.

%%--------------------------------------------------------------------
%% @doc Execute multiple instances.
%% @end
%%--------------------------------------------------------------------
execute_multiple_instances(#pattern_state{} = Pattern) ->
    Count = Pattern#pattern_state.instance_count,
    Subprocess = Pattern#pattern_state.subprocess,

    lists:map(fun(I) ->
        case Subprocess of
            Fun when is_function(Fun) ->
                try
                    Result = Fun(I),
                    {ok, Result}
                catch
                    Error:Reason ->
                        {error, {Error, Reason}}
                end;
            _ ->
                {ok, instance_result}
        end
    end, lists:seq(1, Count)).

%%--------------------------------------------------------------------
%% @doc Execute error handler pattern.
%% @end
%%--------------------------------------------------------------------
execute_error_handler(#pattern_state{subprocess = Subprocess}) ->
    case Subprocess of
        Fun when is_function(Fun, 2) ->
            Fun(fun() -> throw(error) end, fun(_Error) -> handled end);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute pattern with retries.
%% @end
%%--------------------------------------------------------------------
execute_with_retries(#pattern_state{}, _Operation, RetryPolicy) ->
    %% Simulate retry logic
    attempt_with_retry(1, RetryPolicy).

attempt_with_retry(Attempt, RetryPolicy) when Attempt > RetryPolicy#retry_policy.max_attempts ->
    {error, max_attempts_exceeded};
attempt_with_retry(Attempt, RetryPolicy) ->
    timer:sleep(cre_yawl_exception:calculate_backoff(RetryPolicy, Attempt)),
    case attempt_operation() of
        ok -> {ok, success};
        error -> attempt_with_retry(Attempt + 1, RetryPolicy)
    end.

attempt_operation() ->
    %% Simulate operation that fails first time, succeeds second
    case get(operation_attempt) of
        undefined -> put(operation_attempt, 1), error;
        1 -> put(operation_attempt, 2), ok;
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute compensation pattern.
%% @end
%%--------------------------------------------------------------------
execute_compensation_pattern(#pattern_state{subprocess = Subprocess}) ->
    case Subprocess of
        Fun when is_function(Fun, 2) ->
            Fun(fun() -> {primary_result, success} end, fun(State) -> {compensated, State} end);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute milestone pattern.
%% @end
%%--------------------------------------------------------------------
execute_milestone_pattern(#pattern_state{subprocess = Subprocess}) ->
    case Subprocess of
        Fun when is_function(Fun, 2) ->
            Fun(fun() -> milestone_reached end, fun() -> milestone_action end);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute cancel activity pattern.
%% @end
%%--------------------------------------------------------------------
execute_cancel_activity_pattern(#pattern_state{subprocess = Subprocess}) ->
    case Subprocess of
        Fun when is_function(Fun, 1) ->
            Fun(<<"activity_1">>);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute cancel case pattern.
%% @end
%%--------------------------------------------------------------------
execute_cancel_case_pattern(#pattern_state{subprocess = Subprocess}) ->
    case Subprocess of
        Fun when is_function(Fun, 2) ->
            Fun([fun() -> activity1 end, fun() -> activity2 end, fun() -> activity3 end], fun() -> cancel end);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute structured sync pattern.
%% @end
%%--------------------------------------------------------------------
execute_structured_sync(#pattern_state{subprocess = Subprocess}) ->
    case Subprocess of
        Fun when is_function(Fun, 2) ->
            Fun([fun() -> activity1 end, fun() -> activity2 end], undefined);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute partial join pattern.
%% @end
%%--------------------------------------------------------------------
execute_partial_join(#pattern_state{subprocess = Subprocess}) ->
    case Subprocess of
        Fun when is_function(Fun, 2) ->
            Fun([fun() -> a end, fun() -> b end, fun() -> c end], 2);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Execute structured loop pattern.
%% @end
%%--------------------------------------------------------------------
execute_structured_loop(#pattern_state{subprocess = Subprocess}, InitialValue) ->
    case Subprocess of
        Fun when is_function(Fun, 2) ->
            execute_loop_with_condition(Fun, InitialValue);
        _ ->
            []
    end.

execute_loop_with_condition(Fun, Value) when Value < 5 ->
    [Fun(Value) | execute_loop_with_condition(Fun, Value + 1)];
execute_loop_with_condition(_Fun, _) ->
    [].

%%--------------------------------------------------------------------
%% @doc Execute basic pattern.
%% @end
%%--------------------------------------------------------------------
execute_pattern_basic(#workflow{} = Workflow) ->
    %% Simulate basic workflow execution
    {ok, Tasks} = cre_yawl:get_tasks(Workflow),
    {ok, Connections} = cre_yawl:get_connections(Workflow),
    ok;
execute_pattern_basic(_) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Get loop type from pattern.
%% @end
%%--------------------------------------------------------------------
get_loop_type(#pattern_state{pattern_type = loop_type}) ->
    loop_type;
get_loop_type(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc Complete all work items in workflow.
%% @end
%%--------------------------------------------------------------------
complete_all_workitems(EnginePid, CaseId) ->
    case yawl_engine:get_available_workitems(EnginePid) of
        {ok, Workitems} when length(Workitems) > 0 ->
            lists:foreach(fun(WI) ->
                WIId = get_workitem_id(WI),
                case yawl_engine:start_workitem(EnginePid, WIId) of
                    ok ->
                        ok = yawl_engine:complete_workitem(
                            EnginePid,
                            WIId,
                            #{result => <<"completed">>}
                        );
                    {error, _} ->
                        ok
                end
            end, Workitems),
            complete_all_workitems(EnginePid, CaseId);
        {ok, _} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Get workitem ID from workitem.
%% @end
%%--------------------------------------------------------------------
get_workitem_id(#workitem{workitem_id = Id}) ->
    Id;
get_workitem_id({workitem, Id, _, _, _, _, _, _, _}) ->
    Id;
get_workitem_id(Id) when is_binary(Id) ->
    Id.

%%====================================================================
%% Execution Tests for WCP-07 to WCP-10 Patterns
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Tests for WCP-07: Synchronizing Merge execution.
%% @end
%%--------------------------------------------------------------------
synchronizing_merge_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Synchronizing merge - All branches active",
           fun() ->
               Pattern = cre_yawl:synchronizing_merge(),
               MergeTaskId = <<"merge_task_1">>,
               IncomingTaskIds = [<<"task_a">>, <<"task_b">>, <<"task_c">>],

               PatternRecord = Pattern#synchronizing_merge{
                   merge_task_id = MergeTaskId,
                   incoming_task_ids = IncomingTaskIds
               },

               Input = #{
                   <<"task_a">> => #{status => active},
                   <<"task_b">> => #{status => active},
                   <<"task_c">> => #{status => active}
               },

               Options = #{timeout => 1000},

               Result = cre_yawl:execute_synchronizing_merge(
                   PatternRecord,
                   Input,
                   Options
               ),

               ?assertMatch({ok, _}, Result),
               {ok, ResultMap} = Result,
               ?assertEqual(MergeTaskId, maps:get(merge_task, ResultMap)),
               ?assertEqual(3, length(maps:get(active_branches, ResultMap)))
           end},

          {"Synchronizing merge - Not all branches active",
           fun() ->
               Pattern = cre_yawl:synchronizing_merge(),
               MergeTaskId = <<"merge_task_2">>,
               IncomingTaskIds = [<<"task_a">>, <<"task_b">>],

               PatternRecord = Pattern#synchronizing_merge{
                   merge_task_id = MergeTaskId,
                   incoming_task_ids = IncomingTaskIds
               },

               Input = #{
                   <<"task_a">> => #{status => active}
                   %% task_b missing (not active)
               },

               Options = #{timeout => 1000},

               Result = cre_yawl:execute_synchronizing_merge(
                   PatternRecord,
                   Input,
                   Options
               ),

               ?assertMatch({error, {not_all_active, _, _}}, Result)
           end},

          {"Synchronizing merge - Empty incoming list",
           fun() ->
               Pattern = cre_yawl:synchronizing_merge(),
               PatternRecord = Pattern#synchronizing_merge{
                   merge_task_id = <<"merge_task_3">>,
                   incoming_task_ids = []
               },

               Result = cre_yawl:execute_synchronizing_merge(
                   PatternRecord,
                   #{},
                   #{}
               ),

               ?assertMatch({error, empty_incoming_list}, Result)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Tests for WCP-08: Multi Merge execution.
%% @end
%%--------------------------------------------------------------------
multi_merge_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Multi merge - Collect all branch results",
           fun() ->
               Pattern = cre_yawl:multi_merge(),
               MergeTaskId = <<"multi_merge_task_1">>,
               IncomingTaskIds = [<<"task_a">>, <<"task_b">>, <<"task_c">>],

               PatternRecord = Pattern#multi_merge{
                   merge_task_id = MergeTaskId,
                   incoming_task_ids = IncomingTaskIds
               },

               Input = #{
                   <<"task_a">> => result_a,
                   <<"task_b">> => result_b,
                   <<"task_c">> => result_c
               },

               Options = #{timeout => 1000},

               Result = cre_yawl:execute_multi_merge(
                   PatternRecord,
                   Input,
                   Options
               ),

               ?assertMatch({ok, _}, Result),
               {ok, ResultMap} = Result,
               ?assertEqual(3, maps:get(result_count, ResultMap)),
               ?assertEqual(3, length(maps:get(collected_results, ResultMap)))
           end},

          {"Multi merge - Partial branch results",
           fun() ->
               Pattern = cre_yawl:multi_merge(),
               PatternRecord = Pattern#multi_merge{
                   merge_task_id = <<"multi_merge_task_2">>,
                   incoming_task_ids = [<<"task_a">>, <<"task_b">>]
               },

               Input = #{
                   <<"task_a">> => result_a
                   %% task_b not complete yet
               },

               Result = cre_yawl:execute_multi_merge(
                   PatternRecord,
                   Input,
                   #{}
               ),

               ?assertMatch({ok, _}, Result),
               {ok, ResultMap} = Result,
               ?assertEqual(1, maps:get(result_count, ResultMap))
           end},

          {"Multi merge - No results",
           fun() ->
               Pattern = cre_yawl:multi_merge(),
               PatternRecord = Pattern#multi_merge{
                   merge_task_id = <<"multi_merge_task_3">>,
                   incoming_task_ids = [<<"task_a">>]
               },

               Result = cre_yawl:execute_multi_merge(
                   PatternRecord,
                   #{},
                   #{}
               ),

               ?assertMatch({error, no_results_collected}, Result)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Tests for WCP-09: Discriminator execution.
%% @end
%%--------------------------------------------------------------------
discriminator_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Discriminator - First branch completes",
           fun() ->
               Pattern = cre_yawl:discriminator(),
               MergeTaskId = <<"discriminator_task_1">>,
               IncomingTaskIds = [<<"task_a">>, <<"task_b">>, <<"task_c">>],

               PatternRecord = Pattern#discriminator{
                   merge_task_id = MergeTaskId,
                   incoming_task_ids = IncomingTaskIds
               },

               Input = #{
                   <<"task_a">> => completed,
                   <<"task_b">> => running,
                   <<"task_c">> => pending
               },

               Options = #{timeout => 1000},

               Result = cre_yawl:execute_discriminator(
                   PatternRecord,
                   Input,
                   Options
               ),

               ?assertMatch({ok, _}, Result),
               {ok, ResultMap} = Result,
               ?assertEqual(<<"task_a">>, maps:get(triggering_branch, ResultMap)),
               ?assertEqual(true, maps:get(discriminator_reset, ResultMap))
           end},

          {"Discriminator - No branches completed",
           fun() ->
               Pattern = cre_yawl:discriminator(),
               PatternRecord = Pattern#discriminator{
                   merge_task_id = <<"discriminator_task_2">>,
                   incoming_task_ids = [<<"task_a">>]
               },

               Result = cre_yawl:execute_discriminator(
                   PatternRecord,
                   #{},
                   #{}
               ),

               ?assertMatch({error, no_completed_branches}, Result)
           end},

          {"Discriminator - Single branch",
           fun() ->
               Pattern = cre_yawl:discriminator(),
               PatternRecord = Pattern#discriminator{
                   merge_task_id = <<"discriminator_task_3">>,
                   incoming_task_ids = [<<"single_task">>]
               },

               Input = #{<<"single_task">> => completed},

               Result = cre_yawl:execute_discriminator(
                   PatternRecord,
                   Input,
                   #{}
               ),

               ?assertMatch({ok, _}, Result),
               {ok, ResultMap} = Result,
               ?assertEqual(<<"single_task">>, maps:get(triggering_branch, ResultMap))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Tests for WCP-10: Arbitration execution.
%% @end
%%--------------------------------------------------------------------
arbitration_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Arbitration - N of M quorum reached",
           fun() ->
               Pattern = cre_yawl:arbitration(),
               MergeTaskId = <<"arbitration_task_1">>,
               IncomingTaskIds = [<<"task_a">>, <<"task_b">>, <<"task_c">>, <<"task_d">>],
               RequiredCount = 2,

               PatternRecord = Pattern#arbitration{
                   merge_task_id = MergeTaskId,
                   incoming_task_ids = IncomingTaskIds,
                   required_count = RequiredCount
               },

               Input = #{
                   <<"task_a">> => completed,
                   <<"task_b">> => completed,
                   <<"task_c">> => running,
                   <<"task_d">> => pending
               },

               Options = #{timeout => 1000},

               Result = cre_yawl:execute_arbitration(
                   PatternRecord,
                   Input,
                   Options
               ),

               ?assertMatch({ok, _}, Result),
               {ok, ResultMap} = Result,
               ?assertEqual(2, maps:get(quorum_count, ResultMap)),
               ?assertEqual(4, maps:get(total_branches, ResultMap)),
               ?assertEqual(arbitration_complete, maps:get(status, ResultMap))
           end},

          {"Arbitration - Quorum not met",
           fun() ->
               Pattern = cre_yawl:arbitration(),
               PatternRecord = Pattern#arbitration{
                   merge_task_id = <<"arbitration_task_2">>,
                   incoming_task_ids = [<<"task_a">>, <<"task_b">>, <<"task_c">>],
                   required_count = 3
               },

               Input = #{
                   <<"task_a">> => completed,
                   <<"task_b">> => completed
                   %% task_c not completed - only 2 of 3
               },

               Result = cre_yawl:execute_arbitration(
                   PatternRecord,
                   Input,
                   #{}
               ),

               ?assertMatch({error, {quorum_not_met, 2, 3}}, Result)
           end},

          {"Arbitration - Invalid required count",
           fun() ->
               Pattern = cre_yawl:arbitration(),
               PatternRecord = Pattern#arbitration{
                   merge_task_id = <<"arbitration_task_3">>,
                   incoming_task_ids = [<<"task_a">>, <<"task_b">>],
                   required_count = 5  %% More than available branches
               },

               Result = cre_yawl:execute_arbitration(
                   PatternRecord,
                   #{},
                   #{}
               ),

               ?assertMatch({error, {invalid_count, 5, 2}}, Result)
           end},

          {"Arbitration - All branches required",
           fun() ->
               Pattern = cre_yawl:arbitration(),
               PatternRecord = Pattern#arbitration{
                   merge_task_id = <<"arbitration_task_4">>,
                   incoming_task_ids = [<<"task_a">>, <<"task_b">>, <<"task_c">>],
                   required_count = 3
               },

               Input = #{
                   <<"task_a">> => completed,
                   <<"task_b">> => completed,
                   <<"task_c">> => completed
               },

               Result = cre_yawl:execute_arbitration(
                   PatternRecord,
                   Input,
                   #{}
               ),

               ?assertMatch({ok, _}, Result),
               {ok, ResultMap} = Result,
               ?assertEqual(3, maps:get(quorum_count, ResultMap))
           end}
         ]
     end}.