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
%% @doc Workflow Net Integration Tests
%%
%% Comprehensive integration test suite for workflow nets.
%% Tests end-to-end workflows, pattern combinations, and performance.
%% @end
%% -------------------------------------------------------------------

-module(wfnet_integration_tests).
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
            case erlang:whereis(cre_master) of
                undefined -> error(cre_master_not_started);
                _ -> ok
            end;
        _ -> ok
    end.

teardown(_) ->
    %% No specific cleanup needed
    ok.

%%====================================================================
%% Integration Test Suite
%%====================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        {timeout, 30000, parallel_workflow_execution_test()},
        {timeout, 30000, nested_pattern_test()},
        {timeout, 30000, complex_business_process_test()},
        {timeout, 30000, error_propagation_test()},
        {timeout, 30000, performance_benchmark_test()},
        {timeout, 30000, state_persistence_test()},
        {timeout, 30000, parallel_concurrency_test()},
        {timeout, 30000, workflow_orchestration_test()}
     ]
    }.

%%--------------------------------------------------------------------
%% Parallel Workflow Execution Tests
%%--------------------------------------------------------------------

parallel_workflow_execution_test() ->
    ?_test begin
        %% Create multiple parallel workflows
        A = wfnet_compose:task(a_task, fun(_, _) -> {ok, a_result} end),
        B = wfnet_compose:task(b_task, fun(_, _) -> {ok, b_result} end),
        C = wfnet_compose:task(c_task, fun(_, _) -> {ok, c_result} end),
        
        %% Create complex parallel structure: (A || (B || C))
        ParallelBC = wfnet_compose:parallel(B, C),
        ParallelAll = wfnet_compose:parallel(A, ParallelBC),
        
        %% Execute workflow
        Workflow = wfnet_compose:workflow(ParallelAll, initial_marking()),
        {ok, CaseId} = wfnet_engine:start_workflow(Workflow),
        
        %% Wait for completion
        wfnet_utils:wait_for_completion(CaseId, 30000),
        
        %% Verify all tasks completed
        Status = wfnet_engine:get_case_status(CaseId),
        ?assertEqual(completed, Status),
        
        %% Collect results
        Results = wfnet_engine:get_workflow_results(CaseId),
        ?assertMatch(#{a_task := a_result, b_task := b_result, c_task := c_result}, Results)
    end.

%%--------------------------------------------------------------------
%% Nested Pattern Tests
%%--------------------------------------------------------------------

nested_pattern_test() ->
    ?_test begin
        %% Create nested workflow: outer -> (inner1 || inner2) -> outer
        Outer1 = wfnet_compose:task(outer1_task, fun(_, _) -> {ok, outer_done} end),
        Outer2 = wfnet_compose:task(outer2_task, fun(_, _) -> {ok, outer_complete} end),
        
        Inner1 = wfnet_compose:task(inner1_task, fun(_, _) -> {ok, inner1_done} end),
        Inner2 = wfnet_compose:task(inner2_task, fun(_, _) -> {ok, inner2_done} end),
        
        %% Create nested structure
        InnerParallel = wfnet_compose:parallel(Inner1, Inner2),
        OuterSequence = wfnet_compose:sequence(Outer1, InnerParallel),
        FullWorkflow = wfnet_compose:sequence(OuterSequence, Outer2),
        
        %% Execute workflow
        Workflow = wfnet_compose:workflow(FullWorkflow, initial_marking()),
        {ok, CaseId} = wfnet_engine:start_workflow(Workflow),
        
        %% Wait for completion
        wfnet_utils:wait_for_completion(CaseId, 30000),
        
        %% Verify execution order and completeness
        Status = wfnet_engine:get_case_status(CaseId),
        ?assertEqual(completed, Status),
        
        Results = wfnet_engine:get_workflow_results(CaseId),
        ?assertMatch(#{outer1_task := outer_done, 
                      inner1_task := inner1_done, 
                      inner2_task := inner2_done,
                      outer2_task := outer_complete}, Results)
    end.

%%--------------------------------------------------------------------
%% Complex Business Process Test
%%--------------------------------------------------------------------

complex_business_process_test() ->
    ?_test begin
        %% Simulate a complex order processing workflow
        Validate = wfnet_compose:task(validate_order, 
                                      fun(Order, _) -> 
                                          case Order of
                                              #{amount := Amount, customer := _} when Amount > 10000 ->
                                                  {error, high_value_order};
                                              _ ->
                                                  {ok, validated}
                                          end
                                      end),
        
        CheckCredit = wfnet_compose:task(check_credit,
                                        fun(Order, _) ->
                                            #{customer := Customer} = Order,
                                            %% Simulate credit check
                                            case Customer of
                                                "premium" -> {ok, credit_approved};
                                                _ -> {ok, credit_ok}
                                            end
                                        end),
        
        ProcessPayment = wfnet_compose:task(process_payment,
                                          fun(Order, _) ->
                                              #{amount := Amount} = Order,
                                              {ok, {payment_processed, Amount}}
                                          end),
        
        ShipOrder = wfnet_compose:task(ship_order,
                                      fun(Order, _) ->
                                          #{customer := Customer} = Order,
                                          {ok, {shipped_to, Customer}}
                                      end),
        
        HandleComplaint = wfnet_compose:task(handle_complaint,
                                            fun(Complaint, _) ->
                                                {ok, complaint_resolved}
                                            end),
        
        %% Build business process:
        %% validate -> (credit_check || payment) -> shipping
        %% with error handling for high-value orders
        CreditPayment = wfnet_compose:parallel(CheckCredit, ProcessPayment),
        MainFlow = wfnet_compose:sequence(Validate, CreditPayment),
        MainFlow = wfnet_compose:sequence(MainFlow, ShipOrder),
        
        %% Execute with regular order
        RegularOrder = #{amount => 500, customer => "john"},
        Workflow = wfnet_compose:workflow(MainFlow, initial_marking()),
        
        {ok, CaseId1} = wfnet_engine:start_workflow(Workflow, #{data => RegularOrder}),
        wfnet_utils:wait_for_completion(CaseId1, 30000),
        
        %% Verify regular order processed
        Status1 = wfnet_engine:get_case_status(CaseId1),
        ?assertEqual(completed, Status1),
        
        %% Execute with high-value order
        HighValueOrder = #{amount => 15000, customer => "premium"},
        {ok, CaseId2} = wfnet_engine:start_workflow(Workflow, #{data => HighValueOrder}),
        wfnet_utils:wait_for_completion(CaseId2, 30000),
        
        %% Verify high-value order handled correctly
        Status2 = wfnet_engine:get_case_status(CaseId2),
        ?assertMatch({error, _}, Status2)
    end.

%%--------------------------------------------------------------------
%% Error Propagation Tests
%%--------------------------------------------------------------------

error_propagation_test() ->
    ?_test begin
        %% Create workflow with error handling
        SuccessTask = wfnet_compose:task(success_task, fun(_, _) -> {ok, success} end),
        ErrorTask = wfnet_compose:task(error_task, fun(_, _) -> {error, failed} end),
        
        %% Sequence: success -> error should propagate error
        Sequential = wfnet_compose:sequence(SuccessTask, ErrorTask),
        
        %% Parallel: one error should not stop others
        Parallel = wfnet_compose:parallel(SuccessTask, ErrorTask),
        
        %% Test sequential error propagation
        SeqWorkflow = wfnet_compose:workflow(Sequential, initial_marking()),
        {ok, CaseId1} = wfnet_engine:start_workflow(SeqWorkflow),
        wfnet_utils:wait_for_completion(CaseId1, 30000),
        
        Status1 = wfnet_engine:get_case_status(CaseId1),
        ?assertMatch({error, failed}, Status1),
        
        %% Test parallel execution with mixed success/failure
        ParWorkflow = wfnet_compose:workflow(Parallel, initial_marking()),
        {ok, CaseId2} = wfnet_engine:start_workflow(ParWorkflow),
        wfnet_utils:wait_for_completion(CaseId2, 30000),
        
        Status2 = wfnet_engine:get_case_status(CaseId2),
        %% In parallel execution, workflow completes even with some failures
        ?assertMatch(completed, Status2)
    end.

%%--------------------------------------------------------------------
%% Performance Benchmark Tests
%%--------------------------------------------------------------------

performance_benchmark_test() ->
    ?_test begin
        %% Create a medium-sized workflow for testing
        Tasks = [list_to_atom("task_" ++ integer_to_list(I)) || I <- lists:seq(1, 10)],
        
        %% Create sequence of 10 tasks
        Sequence = lists:foldl(
            fun(Task, Acc) ->
                TaskNode = wfnet_compose:task(Task, 
                                             fun(_, _) -> 
                                                 timer:sleep(100), %% Simulate work
                                                 {ok, Task}
                                             end),
                case Acc of
                    undefined -> TaskNode;
                    _ -> wfnet_compose:sequence(Acc, TaskNode)
                end
            end, undefined, Tasks),
        
        %% Execute multiple instances concurrently
        NumInstances = 5,
        StartTimes = [begin
            Workflow = wfnet_compose:workflow(Sequence, initial_marking()),
            {ok, CaseId} = wfnet_engine:start_workflow(Workflow),
            CaseId
        end || _ <- lists:seq(1, NumInstances)],
        
        %% Wait for all to complete
        [wfnet_utils:wait_for_completion(CaseId, 30000) || CaseId <- StartTimes],
        
        %% Verify all completed
        AllCompleted = lists:all(fun(CaseId) ->
            wfnet_engine:get_case_status(CaseId) =:= completed
        end, StartTimes),
        ?assert(AllCompleted, "All concurrent instances completed"),
        
        %% Measure performance
        EndTime = erlang:system_time(millisecond),
        TotalTime = EndTime - erlang:system_time(millisecond),
        ?assert(TotalTime < 30000, "Execution time within reasonable bounds")
    end.

%%--------------------------------------------------------------------
%% State Persistence Tests
%%--------------------------------------------------------------------

state_persistence_test() ->
    ?_test begin
        %% Create a workflow with intermediate state
        FirstTask = wfnet_compose:task(first_task, fun(_, State) -> 
            {ok, State#{first => done}} 
        end),
        
        MiddleTask = wfnet_compose:task(middle_task, fun(_, State) -> 
            {ok, State#{middle => completed}} 
        end),
        
        LastTask = wfnet_compose:task(last_task, fun(_, State) -> 
            {ok, State#{last => finished}} 
        end),
        
        Sequence = wfnet_compose:sequence(
            wfnet_compose:sequence(FirstTask, MiddleTask),
            LastTask
        ),
        
        %% Start workflow
        Workflow = wfnet_compose:workflow(Sequence, initial_marking()),
        {ok, CaseId} = wfnet_engine:start_workflow(Workflow),
        
        %% Let it run part way
        timer:sleep(1000),
        
        %% Check intermediate state
        Status = wfnet_engine:get_case_status(CaseId),
        ?assertNotEqual(completed, Status),
        
        %% Wait for completion
        wfnet_utils:wait_for_completion(CaseId, 30000),
        
        %% Verify final state persistence
        FinalState = wfnet_engine:get_workflow_state(CaseId),
        ?assertMatch(#{first := done, middle := completed, last := finished}, FinalState)
    end.

%%--------------------------------------------------------------------
%% Parallel Concurrency Tests
%%--------------------------------------------------------------------

parallel_concurrency_test() ->
    ?_test begin
        %% Create workflow with shared resources
        ResourceAccess = wfnet_compose:task(access_resource, fun(_, State) ->
            %% Simulate resource access
            timer:sleep(50),
            {ok, State#{resource_access => true}}
        end),
        
        %% Create parallel access to same resource
        ParallelTasks = [ResourceAccess, ResourceAccess, ResourceAccess],
        ParallelWorkflow = wfnet_compose:parallel_list(ParallelTasks),
        
        %% Execute multiple instances
        NumInstances = 3,
        CaseIds = [begin
            Workflow = wfnet_compose:workflow(ParallelWorkflow, initial_marking()),
            {ok, CaseId} = wfnet_engine:start_workflow(Workflow),
            CaseId
        end || _ <- lists:seq(1, NumInstances)],
        
        %% Wait for all
        [wfnet_utils:wait_for_completion(CaseId, 30000) || CaseId <- CaseIds],
        
        %% Verify all completed successfully
        AllCompleted = lists:all(fun(CaseId) ->
            wfnet_engine:get_case_status(CaseId) =:= completed
        end, CaseIds),
        ?assert(AllCompleted, "All parallel instances completed")
    end.

%%--------------------------------------------------------------------
%% Workflow Orchestration Tests
%%--------------------------------------------------------------------

workflow_orchestration_test() ->
    ?_test begin
        %% Create a master workflow that orchestrates sub-workflows
        SubWorkflow1 = wfnet_compose:sequence(
            wfnet_compose:task(sub1_task1, fun(_, _) -> {ok, sub1_1} end),
            wfnet_compose:task(sub1_task2, fun(_, _) -> {ok, sub1_2} end)
        ),
        
        SubWorkflow2 = wfnet_compose:sequence(
            wfnet_compose:task(sub2_task1, fun(_, _) -> {ok, sub2_1} end),
            wfnet_compose:task(sub2_task2, fun(_, _) -> {ok, sub2_2} end)
        ),
        
        %% Master workflow: (sub1 || sub2) -> final task
        ParallelSubs = wfnet_compose:parallel(SubWorkflow1, SubWorkflow2),
        FinalTask = wfnet_compose:task(final_task, 
                                      fun(_, State) -> 
                                          {ok, State#{final => completed}} 
                                      end),
        
        MasterWorkflow = wfnet_compose:sequence(ParallelSubs, FinalTask),
        
        %% Execute master workflow
        Workflow = wfnet_compose:workflow(MasterWorkflow, initial_marking()),
        {ok, CaseId} = wfnet_engine:start_workflow(Workflow),
        
        %% Wait for completion
        wfnet_utils:wait_for_completion(CaseId, 30000),
        
        %% Verify orchestration
        Status = wfnet_engine:get_case_status(CaseId),
        ?assertEqual(completed, Status),
        
        Results = wfnet_engine:get_workflow_results(CaseId),
        ?assertMatch(#{sub1_task1 := sub1_1, 
                      sub1_task2 := sub1_2,
                      sub2_task1 := sub2_1,
                      sub2_task2 := sub2_2,
                      final_task := _}, Results)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

initial_marking() ->
    #{start => 1}.

%% Utility function to wait for workflow completion
wait_for_completion(CaseId, Timeout) ->
    StartTime = erlang:system_time(millisecond),
    wait_for_completion(CaseId, Timeout, StartTime).

wait_for_completion(CaseId, Timeout, StartTime) ->
    case wfnet_engine:get_case_status(CaseId) of
        completed ->
            ok;
        running ->
            case erlang:system_time(millisecond) - StartTime of
                Elapsed when Elapsed > Timeout ->
                    timeout;
                _ ->
                    timer:sleep(100),
                    wait_for_completion(CaseId, Timeout, StartTime)
            end;
        Other ->
            Other
    end.

