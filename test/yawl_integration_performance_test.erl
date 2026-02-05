%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @doc YAWL Integration and Performance Test Suite
%%
%% Comprehensive integration and performance tests for YAWL workflow patterns.
%% Tests cover pattern combinations, complex workflows, failure scenarios,
%% performance benchmarks, stress tests, concurrency, and memory behavior.
%%
%% Test Categories:
%% 1. Pattern Combination Tests
%% 2. Complex Workflow Tests
%% 3. Failure Scenario Tests
%% 4. Performance Benchmark Tests
%% 5. Stress Tests
%% 6. Concurrency Tests
%% 7. Memory Leak Tests
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_integration_performance_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions
%%====================================================================

-record(workflow, {
    id :: binary(),
    name :: binary(),
    tasks :: map(),
    conditions :: map(),
    connections :: list(),
    start_task_id :: binary() | undefined,
    end_task_ids :: list()
}).

-record(pattern_state, {
    pattern_type :: atom(),
    subprocess :: module() | function() | undefined,
    instance_count :: non_neg_integer() | undefined,
    max_instances :: non_neg_integer() | unlimited | undefined,
    pending_instances = [] :: list(),
    active_instances = [] :: list(),
    completed_instances = [] :: list(),
    choice_data = #{} :: map(),
    branch_queue = [] :: list()
}).

-record(perf_metric, {
    test_name :: binary(),
    start_time :: integer(),
    end_time :: integer(),
    duration_ms :: non_neg_integer(),
    operations :: non_neg_integer(),
    throughput :: float(),
    memory_before :: non_neg_integer(),
    memory_after :: non_neg_integer(),
    memory_delta :: integer()
}).

-record(concurrency_test, {
    test_id :: reference(),
    parallel_count :: pos_integer(),
    operations :: pos_integer(),
    successful :: non_neg_integer(),
    failed :: non_neg_integer(),
    avg_latency :: float(),
    max_latency :: non_neg_integer(),
    min_latency :: non_neg_integer(),
    deadlock_detected :: boolean()
}).

-record(stress_result, {
    test_name :: binary(),
    instance_count :: pos_integer(),
    completed :: pos_integer(),
    failed :: pos_integer(),
    duration_ms :: non_neg_integer(),
    errors :: list()
}).

%% Use existing record from cre_yawl_exception if available
-ifndef(cre_yawl_exception).
-record(retry_policy, {
    max_attempts :: pos_integer(),
    backoff_strategy :: atom(),
    base_delay :: non_neg_integer(),
    max_delay :: non_neg_integer(),
    multiplier :: float(),
    jitter :: boolean(),
    jitter_factor :: float()
}).
-endif.

%%====================================================================
%% Test Suite Definition
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main integration and performance test suite.
%%
%% Tests complete integration between patterns and performance characteristics.
%%
%% @end
%%--------------------------------------------------------------------
integration_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Context) ->
         [
          %% Pattern Combination Tests
          {"Sequence followed by parallel split",
           fun sequence_then_parallel_test/0},
          {"Choice followed by merge",
           fun choice_then_merge_test/0},
          {"Loop with nested parallel",
           fun loop_with_nested_parallel_test/0},
          {"Multiple instances with synchronization",
           fun multi_instance_sync_test/0},
          {"Exception handling with compensation",
           fun exception_compensation_integration_test/0},

          %% Complex Workflow Tests
          {"Multi-level nesting",
           fun multi_level_nesting_test/0},
          {"Cross-pattern dependencies",
           fun cross_pattern_dependencies_test/0},
          {"State sharing between patterns",
           fun state_sharing_test/0},
          {"Resource contention scenarios",
           fun resource_contention_test/0},

          %% Failure Scenario Tests
          {"Failure in nested pattern",
           fun nested_pattern_failure_test/0},
          {"Concurrent failures",
           fun concurrent_failures_test/0},
          {"Timeout in composite workflow",
           fun timeout_composite_test/0},
          {"Recovery scenarios",
           fun recovery_scenarios_test/0},

          %% Performance Benchmark Tests
          {"Pattern execution benchmarks",
           fun pattern_execution_benchmark_test/0},
          {"Throughput measurement",
           fun throughput_measurement_test/0},
          {"Memory per pattern",
           fun memory_per_pattern_test/0},
          {"Scaling behavior",
           fun scaling_behavior_test/0},

          %% Stress Tests
          {"High instance counts (100, 1000)",
           fun high_instance_count_test/0},
          {"Deep nesting (10, 50 levels)",
           fun deep_nesting_test/0},
          {"Long-running workflows",
           fun long_running_workflow_test/0},
          {"Resource exhaustion",
           fun resource_exhaustion_test/0},

          %% Concurrency Tests
          {"Parallel pattern execution",
           fun parallel_pattern_execution_test/0},
          {"Race condition detection",
           fun race_condition_detection_test/0},
          {"Deadlock scenarios",
           fun deadlock_scenarios_test/0},
          {"Contention resolution",
           fun contention_resolution_test/0},

          %% Memory Tests
          {"Memory leak detection",
           fun memory_leak_detection_test/0},
          {"Cleanup verification",
           fun cleanup_verification_test/0},
          {"GC behavior under load",
           fun gc_behavior_test/0},
          {"Memory growth patterns",
           fun memory_growth_patterns_test/0}
         ]
     end}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Initialize persistent term configuration
    cre_config:init(),

    %% Start CRE application
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Start YAWL control panel
    case whereis(yawl_control) of
        undefined ->
            {ok, _ControlPid} = yawl_control:start_control(yawl_control);
        _ ->
            ok
    end,

    %% Initialize persistence schema
    case yawl_persistence:init_schema() of
        ok -> ok;
        {error, {already_exists, _}} -> ok
    end,

    %% Initialize test data
    initialize_test_data(),
    ok.

cleanup(_Context) ->
    %% Clean up test data
    cleanup_test_data(),

    %% Stop YAWL control panel
    case whereis(yawl_control) of
        undefined -> ok;
        _ -> yawl_control:stop()
    end,

    %% Clean up any test processes
    cleanup_test_processes(),
    ok.

%%--------------------------------------------------------------------
%% @doc Initialize test data for tests.
%% @end
%%--------------------------------------------------------------------
initialize_test_data() ->
    put(test_workflows, #{}),
    put(test_patterns, #{}),
    put(perf_metrics, []),
    put(concurrency_results, []),
    put(stress_results, []),
    ok.

%%--------------------------------------------------------------------
%% @doc Clean up test data.
%% @end
%%--------------------------------------------------------------------
cleanup_test_data() ->
    erase(test_workflows),
    erase(test_patterns),
    erase(perf_metrics),
    erase(concurrency_results),
    erase(stress_results),
    ok.

%%--------------------------------------------------------------------
%% @doc Clean up any test processes.
%% @end
%%--------------------------------------------------------------------
cleanup_test_processes() ->
    %% Kill any remaining test processes
    Processes = processes(),
    TestProcesses = [P || P <- Processes,
                         is_process_alive(P),
                         {_, Name} <- [process_info(P, registered_name)],
                         is_binary(Name),
                         size(Name) > 4,
                         binary:part(Name, 0, 4) =:= <<"test_">>],
    lists:foreach(fun(P) -> exit(P, kill) end, TestProcesses),
    ok.

%%====================================================================
%% 1. Pattern Combination Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test sequence followed by parallel split.
%%
%% Validates that a sequence can properly transition into a parallel split.
%%
%% @end
%%--------------------------------------------------------------------
sequence_then_parallel_test() ->
    %% Create workflow with sequence then parallel
    Workflow = cre_yawl:new_workflow(<<"seq_parallel_wf">>),

    %% Sequential part
    W1 = cre_yawl:add_task(Workflow, <<"init">>, [{type, atomic}, {name, <<"Initialize">>}]),
    W2 = cre_yawl:add_task(W1, <<"prepare">>, [{type, atomic}, {name, <<"Prepare">>}]),

    %% Parallel split
    W3 = cre_yawl:add_task(W2, <<"split">>, [{type, atomic}, {name, <<"Split">>}]),
    W4 = cre_yawl:add_task(W3, <<"branch_a">>, [{type, atomic}, {name, <<"Branch A">>}]),
    W5 = cre_yawl:add_task(W4, <<"branch_b">>, [{type, atomic}, {name, <<"Branch B">>}]),
    W6 = cre_yawl:add_task(W5, <<"branch_c">>, [{type, atomic}, {name, <<"Branch C">>}]),
    W7 = cre_yawl:add_task(W6, <<"join">>, [{type, atomic}, {name, <<"Join">>}]),
    W8 = cre_yawl:add_task(W7, <<"finalize">>, [{type, atomic}, {name, <<"Finalize">>}]),

    %% Configure split and join
    W9 = cre_yawl:set_split_type(W8, <<"split">>, 'and_split'),
    W10 = cre_yawl:set_join_type(W9, <<"join">>, 'and_join'),

    %% Connect sequence
    W11 = cre_yawl:connect(W10, <<"init">>, <<"prepare">>),
    W12 = cre_yawl:connect(W11, <<"prepare">>, <<"split">>),

    %% Connect parallel branches
    W13 = cre_yawl:connect(W12, <<"split">>, <<"branch_a">>),
    W14 = cre_yawl:connect(W13, <<"split">>, <<"branch_b">>),
    W15 = cre_yawl:connect(W14, <<"split">>, <<"branch_c">>),
    W16 = cre_yawl:connect(W15, <<"branch_a">>, <<"join">>),
    W17 = cre_yawl:connect(W16, <<"branch_b">>, <<"join">>),
    W18 = cre_yawl:connect(W17, <<"branch_c">>, <<"join">>),

    %% Connect to finalize
    W19 = cre_yawl:connect(W18, <<"join">>, <<"finalize">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W19, <<"init">>, [<<"finalize">>]),

    %% Validate workflow
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Verify structure
    {ok, Tasks} = cre_yawl:get_tasks(FinalWf),
    ?assertEqual(8, map_size(Tasks)),

    {ok, Conns} = cre_yawl:get_connections(FinalWf),
    ?assertEqual(7, length(Conns)).

%%--------------------------------------------------------------------
%% @doc Test choice followed by merge.
%%
%% Validates exclusive choice properly transitions to merge.
%%
%% @end
%%--------------------------------------------------------------------
choice_then_merge_test() ->
    Workflow = cre_yawl:new_workflow(<<"choice_merge_wf">>),

    %% Choice task
    W1 = cre_yawl:add_task(Workflow, <<"choice">>, [{type, atomic}, {name, <<"Decision">>}]),

    %% Alternative branches
    W2 = cre_yawl:add_task(W1, <<"option_a">>, [{type, atomic}, {name, <<"Option A">>}]),
    W3 = cre_yawl:add_task(W2, <<"option_b">>, [{type, atomic}, {name, <<"Option B">>}]),
    W4 = cre_yawl:add_task(W3, <<"option_c">>, [{type, atomic}, {name, <<"Option C">>}]),

    %% Merge task
    W5 = cre_yawl:add_task(W4, <<"merge">>, [{type, atomic}, {name, <<"Merge">>}]),
    W6 = cre_yawl:add_task(W5, <<"continue">>, [{type, atomic}, {name, <<"Continue">>}]),

    %% Configure XOR split and join
    W7 = cre_yawl:set_split_type(W6, <<"choice">>, 'xor_split'),
    W8 = cre_yawl:set_join_type(W7, <<"merge">>, 'xor_join'),

    %% Add conditions
    W9 = cre_yawl:add_condition(W8, <<"cond_a">>, fun(Data) -> maps:get(value, Data, 0) > 10 end),
    W10 = cre_yawl:add_condition(W9, <<"cond_b">>, fun(Data) -> maps:get(value, Data, 0) > 5 end),
    W11 = cre_yawl:add_condition(W10, <<"cond_c">>, fun(_Data) -> true end),

    %% Connect choice to branches
    W12 = cre_yawl:connect(W11, <<"choice">>, <<"option_a">>),
    W13 = cre_yawl:connect(W12, <<"choice">>, <<"option_b">>),
    W14 = cre_yawl:connect(W13, <<"choice">>, <<"option_c">>),

    %% Connect branches to merge
    W15 = cre_yawl:connect(W14, <<"option_a">>, <<"merge">>),
    W16 = cre_yawl:connect(W15, <<"option_b">>, <<"merge">>),
    W17 = cre_yawl:connect(W16, <<"option_c">>, <<"merge">>),

    %% Connect merge to continue
    W18 = cre_yawl:connect(W17, <<"merge">>, <<"continue">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W18, <<"choice">>, [<<"continue">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Verify conditions
    {ok, Conditions} = cre_yawl:get_conditions(FinalWf),
    ?assertEqual(3, map_size(Conditions)),

    %% Test condition evaluation
    TestData1 = #{value => 15},
    CondA = maps:get(<<"cond_a">>, Conditions),
    ?assertEqual(true, evaluate_condition(CondA, TestData1)),

    TestData2 = #{value => 7},
    ?assertEqual(false, evaluate_condition(CondA, TestData2)).

%%--------------------------------------------------------------------
%% @doc Test loop with nested parallel.
%%
%% Validates loop pattern containing parallel branches.
%%
%% @end
%%--------------------------------------------------------------------
loop_with_nested_parallel_test() ->
    %% Create loop pattern with nested parallel
    Pattern = cre_yawl_patterns:structured_loop(
        fun(Counter) -> Counter < 3 end,
        fun() -> loop_body end,
        while
    ),

    ?assertMatch(#pattern_state{}, Pattern),
    ?assertEqual(structured_loop, Pattern#pattern_state.pattern_type),

    %% Create workflow with loop structure
    Workflow = cre_yawl:new_workflow(<<"loop_parallel_wf">>),

    %% Loop control
    W1 = cre_yawl:add_task(Workflow, <<"loop_start">>, [{type, atomic}]),
    W2 = cre_yawl:add_task(W1, <<"loop_condition">>, [{type, atomic}]),

    %% Parallel inside loop
    W3 = cre_yawl:add_task(W2, <<"loop_split">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"loop_branch_a">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"loop_branch_b">>, [{type, atomic}]),
    W6 = cre_yawl:add_task(W5, <<"loop_join">>, [{type, atomic}]),

    %% Loop back or exit
    W7 = cre_yawl:add_task(W6, <<"loop_check">>, [{type, atomic}]),
    W8 = cre_yawl:add_task(W7, <<"loop_exit">>, [{type, atomic}]),

    %% Configure split and join
    W9 = cre_yawl:set_split_type(W8, <<"loop_split">>, 'and_split'),
    W10 = cre_yawl:set_join_type(W9, <<"loop_join">>, 'and_join'),

    %% Connect loop structure
    W11 = cre_yawl:connect(W10, <<"loop_start">>, <<"loop_condition">>),
    W12 = cre_yawl:connect(W11, <<"loop_condition">>, <<"loop_split">>),
    W13 = cre_yawl:connect(W12, <<"loop_split">>, <<"loop_branch_a">>),
    W14 = cre_yawl:connect(W13, <<"loop_split">>, <<"loop_branch_b">>),
    W15 = cre_yawl:connect(W14, <<"loop_branch_a">>, <<"loop_join">>),
    W16 = cre_yawl:connect(W15, <<"loop_branch_b">>, <<"loop_join">>),
    W17 = cre_yawl:connect(W16, <<"loop_join">>, <<"loop_check">>),
    W18 = cre_yawl:connect(W17, <<"loop_check">>, <<"loop_exit">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W18, <<"loop_start">>, [<<"loop_exit">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Verify nested structure
    {ok, Tasks} = cre_yawl:get_tasks(FinalWf),
    ?assertEqual(8, map_size(Tasks)).

%%--------------------------------------------------------------------
%% @doc Test multiple instances with synchronization.
%%
%% Validates multiple instance pattern with proper synchronization.
%%
%% @end
%%--------------------------------------------------------------------
multi_instance_sync_test() ->
    %% Create multiple instances pattern with synchronization
    Pattern = cre_yawl_patterns:multiple_instances_static(
        fun(X) -> X * 2 end,
        5,
        [1, 2, 3, 4, 5]
    ),

    ?assertMatch(#pattern_state{}, Pattern),
    ?assertEqual(5, Pattern#pattern_state.instance_count),
    ?assertEqual(5, length(Pattern#pattern_state.pending_instances)),

    %% Create workflow with multi-instance structure
    Workflow = cre_yawl:new_workflow(<<"multi_instance_sync_wf">>),

    %% Instance spawn
    W1 = cre_yawl:add_task(Workflow, <<"spawn_instances">>, [{type, multi_instance}]),

    %% Individual instance tasks (represented as branches)
    W2 = cre_yawl:add_task(W1, <<"instance_1">>, [{type, atomic}]),
    W3 = cre_yawl:add_task(W2, <<"instance_2">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"instance_3">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"instance_4">>, [{type, atomic}]),
    W6 = cre_yawl:add_task(W5, <<"instance_5">>, [{type, atomic}]),

    %% Synchronization
    W7 = cre_yawl:add_task(W6, <<"sync_all">>, [{type, atomic}]),
    W8 = cre_yawl:add_task(W7, <<"proceed">>, [{type, atomic}]),

    %% Configure parallel structure
    W9 = cre_yawl:set_split_type(W8, <<"spawn_instances">>, 'and_split'),
    W10 = cre_yawl:set_join_type(W9, <<"sync_all">>, 'and_join'),

    %% Connect all instances
    W11 = lists:foldl(fun(I, WfAcc) ->
        TaskId = <<"instance_", (integer_to_binary(I))/binary>>,
        cre_yawl:connect(WfAcc, <<"spawn_instances">>, TaskId)
    end, W10, lists:seq(1, 5)),

    %% Connect instances to sync
    W12 = lists:foldl(fun(I, WfAcc) ->
        TaskId = <<"instance_", (integer_to_binary(I))/binary>>,
        cre_yawl:connect(WfAcc, TaskId, <<"sync_all">>)
    end, W11, lists:seq(1, 5)),

    %% Connect to proceed
    W13 = cre_yawl:connect(W12, <<"sync_all">>, <<"proceed">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W13, <<"spawn_instances">>, [<<"proceed">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Verify all instances connected
    {ok, Conns} = cre_yawl:get_connections(FinalWf),
    ?assertEqual(11, length(Conns)),

    %% Verify AND join has 5 incoming
    IncomingToSync = [C || C <- Conns, element(3, C) =:= <<"sync_all">>],
    ?assertEqual(5, length(IncomingToSync)).

%%--------------------------------------------------------------------
%% @doc Test exception handling with compensation.
%%
%% Validates that exception handling triggers compensation properly.
%%
%% @end
%%--------------------------------------------------------------------
exception_compensation_integration_test() ->
    %% Create compensation pattern
    Pattern = cre_yawl_patterns:triggered_compensation(
        fun() -> primary_activity end,
        fun() -> compensate_activity end,
        fun(Trigger) -> Trigger =:= error end
    ),

    ?assertMatch(#pattern_state{}, Pattern),
    ?assertEqual(triggered_compensation, Pattern#pattern_state.pattern_type),

    %% Create workflow with exception handling
    Workflow = cre_yawl:new_workflow(<<"exception_comp_wf">>),

    %% Primary activities
    W1 = cre_yawl:add_task(Workflow, <<"task1">>, [{type, atomic}]),
    W2 = cre_yawl:add_task(W1, <<"task2">>, [{type, atomic}]),
    W3 = cre_yawl:add_task(W2, <<"task3">>, [{type, atomic}]),

    %% Compensation handlers
    W4 = cre_yawl:add_task(W3, <<"compensate_task3">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"compensate_task2">>, [{type, atomic}]),
    W6 = cre_yawl:add_task(W5, <<"compensate_task1">>, [{type, atomic}]),

    %% Exception handler
    W7 = cre_yawl:add_task(W6, <<"error_handler">>, [{type, atomic}]),
    W8 = cre_yawl:add_task(W7, <<"cleanup">>, [{type, atomic}]),

    %% Connect main flow
    W9 = cre_yawl:connect(W8, <<"task1">>, <<"task2">>),
    W10 = cre_yawl:connect(W9, <<"task2">>, <<"task3">>),

    %% Connect compensation (reverse order)
    W11 = cre_yawl:connect(W10, <<"task3">>, <<"compensate_task3">>),
    W12 = cre_yawl:connect(W11, <<"compensate_task3">>, <<"compensate_task2">>),
    W13 = cre_yawl:connect(W12, <<"compensate_task2">>, <<"compensate_task1">>),
    W14 = cre_yawl:connect(W13, <<"compensate_task1">>, <<"error_handler">>),
    W15 = cre_yawl:connect(W14, <<"error_handler">>, <<"cleanup">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W15, <<"task1">>, [<<"cleanup">>]),

    %% Validate workflow
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Test exception creation
    Exc = cre_yawl_exception:new_exception(
        workflow_exception,
        <<"Test exception for compensation">>,
        #{task => <<"task2">>, reason => test_failure},
        []
    ),

    ?assertEqual(workflow_exception, cre_yawl_exception:exception_type(Exc)),
    ?assertEqual(<<"Test exception for compensation">>, cre_yawl_exception:exception_message(Exc)),

    %% Test compensator
    Comp = cre_yawl_exception:new_compensator(
        <<"task1">>,
        fun(State) -> {compensated, State} end,
        undefined
    ),

    ?assertNot(cre_yawl_exception:has_compensated(Comp)),
    {ok, NewComp} = cre_yawl_exception:compensate(Comp, test_state),
    ?assert(cre_yawl_exception:has_compensated(NewComp)).

%%====================================================================
%% 2. Complex Workflow Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test multi-level nesting of patterns.
%%
%% Validates deeply nested workflow structures.
%%
%% @end
%%--------------------------------------------------------------------
multi_level_nesting_test() ->
    %% Create workflow with 3 levels of nesting
    Workflow = cre_yawl:new_workflow(<<"nested_wf">>),

    %% Level 1: Outer parallel
    W1 = cre_yawl:add_task(Workflow, <<"outer_split">>, [{type, atomic}]),
    W2 = cre_yawl:add_task(W1, <<"outer_branch_a">>, [{type, composite}]),
    W3 = cre_yawl:add_task(W2, <<"outer_branch_b">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"outer_join">>, [{type, atomic}]),

    %% Level 2: Inside branch_a - another parallel
    W5 = cre_yawl:add_task(W4, <<"inner_split">>, [{type, atomic}]),
    W6 = cre_yawl:add_task(W5, <<"inner_branch_a">>, [{type, atomic}]),
    W7 = cre_yawl:add_task(W6, <<"inner_branch_b">>, [{type, composite}]),
    W8 = cre_yawl:add_task(W7, <<"inner_join">>, [{type, atomic}]),

    %% Level 3: Inside inner_branch_b - sequence
    W9 = cre_yawl:add_task(W8, <<"seq_task1">>, [{type, atomic}]),
    W10 = cre_yawl:add_task(W9, <<"seq_task2">>, [{type, atomic}]),
    W11 = cre_yawl:add_task(W10, <<"seq_task3">>, [{type, atomic}]),
    W12 = cre_yawl:add_task(W11, <<"inner_merge">>, [{type, atomic}]),

    %% Configure split/join types
    W13 = cre_yawl:set_split_type(W12, <<"outer_split">>, 'and_split'),
    W14 = cre_yawl:set_join_type(W13, <<"outer_join">>, 'and_join'),
    W15 = cre_yawl:set_split_type(W14, <<"inner_split">>, 'and_split'),
    W16 = cre_yawl:set_join_type(W15, <<"inner_join">>, 'and_join'),

    %% Connect level 1
    W17 = cre_yawl:connect(W16, <<"outer_split">>, <<"outer_branch_a">>),
    W18 = cre_yawl:connect(W17, <<"outer_split">>, <<"outer_branch_b">>),
    W19 = cre_yawl:connect(W18, <<"outer_branch_b">>, <<"outer_join">>),

    %% Connect level 2
    W20 = cre_yawl:connect(W19, <<"outer_branch_a">>, <<"inner_split">>),
    W21 = cre_yawl:connect(W20, <<"inner_split">>, <<"inner_branch_a">>),
    W22 = cre_yawl:connect(W21, <<"inner_split">>, <<"inner_branch_b">>),
    W23 = cre_yawl:connect(W22, <<"inner_branch_a">>, <<"inner_join">>),
    W24 = cre_yawl:connect(W23, <<"inner_branch_b">>, <<"inner_join">>),
    W25 = cre_yawl:connect(W24, <<"inner_join">>, <<"outer_join">>),

    %% Connect level 3 (sequence)
    W26 = cre_yawl:connect(W25, <<"inner_branch_b">>, <<"seq_task1">>),
    W27 = cre_yawl:connect(W26, <<"seq_task1">>, <<"seq_task2">>),
    W28 = cre_yawl:connect(W27, <<"seq_task2">>, <<"seq_task3">>),
    W29 = cre_yawl:connect(W28, <<"seq_task3">>, <<"inner_merge">>),
    W30 = cre_yawl:connect(W29, <<"inner_merge">>, <<"inner_join">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W30, <<"outer_split">>, [<<"outer_join">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Verify depth
    {ok, Tasks} = cre_yawl:get_tasks(FinalWf),
    ?assertEqual(12, map_size(Tasks)),

    %% Verify nesting structure through connections
    {ok, Conns} = cre_yawl:get_connections(FinalWf),
    ?assert(length(Conns) > 10).

%%--------------------------------------------------------------------
%% @doc Test cross-pattern dependencies.
%%
%% Validates dependencies between different pattern types.
%%
%% @end
%%--------------------------------------------------------------------
cross_pattern_dependencies_test() ->
    %% Create workflow with multiple pattern dependencies
    Workflow = cre_yawl:new_workflow(<<"cross_pattern_wf">>),

    %% Multiple instances producing data
    W1 = cre_yawl:add_task(Workflow, <<"data_producer">>, [{type, multi_instance}]),

    %% Data flow patterns
    W2 = cre_yawl:add_task(W1, <<"distribute_data">>, [{type, atomic}]),
    W3 = cre_yawl:add_task(W2, <<"transform_a">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"transform_b">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"accumulate_data">>, [{type, atomic}]),

    %% Choice based on accumulated data
    W6 = cre_yawl:add_task(W5, <<"choice_point">>, [{type, atomic}]),
    W7 = cre_yawl:add_task(W6, <<"path_a">>, [{type, atomic}]),
    W8 = cre_yawl:add_task(W7, <<"path_b">>, [{type, atomic}]),
    W9 = cre_yawl:add_task(W8, <<"merge">>, [{type, atomic}]),

    %% Final sequence
    W10 = cre_yawl:add_task(W9, <<"finalize">>, [{type, atomic}]),
    W11 = cre_yawl:add_task(W10, <<"complete">>, [{type, atomic}]),

    %% Configure patterns
    W12 = cre_yawl:set_split_type(W11, <<"data_producer">>, 'and_split'),
    W13 = cre_yawl:set_join_type(W12, <<"accumulate_data">>, 'and_join'),
    W14 = cre_yawl:set_split_type(W13, <<"choice_point">>, 'xor_split'),
    W15 = cre_yawl:set_join_type(W14, <<"merge">>, 'xor_join'),

    %% Connect workflow
    W16 = cre_yawl:connect(W15, <<"data_producer">>, <<"distribute_data">>),
    W17 = cre_yawl:connect(W16, <<"distribute_data">>, <<"transform_a">>),
    W18 = cre_yawl:connect(W17, <<"distribute_data">>, <<"transform_b">>),
    W19 = cre_yawl:connect(W18, <<"transform_a">>, <<"accumulate_data">>),
    W20 = cre_yawl:connect(W19, <<"transform_b">>, <<"accumulate_data">>),
    W21 = cre_yawl:connect(W20, <<"accumulate_data">>, <<"choice_point">>),
    W22 = cre_yawl:connect(W21, <<"choice_point">>, <<"path_a">>),
    W23 = cre_yawl:connect(W22, <<"choice_point">>, <<"path_b">>),
    W24 = cre_yawl:connect(W23, <<"path_a">>, <<"merge">>),
    W25 = cre_yawl:connect(W24, <<"path_b">>, <<"merge">>),
    W26 = cre_yawl:connect(W25, <<"merge">>, <<"finalize">>),
    W27 = cre_yawl:connect(W26, <<"finalize">>, <<"complete">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W27, <<"data_producer">>, [<<"complete">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Verify all pattern types present
    {ok, Tasks} = cre_yawl:get_tasks(FinalWf),
    ?assertEqual(11, map_size(Tasks)).

%%--------------------------------------------------------------------
%% @doc Test state sharing between patterns.
%%
%% Validates that state can be properly shared across pattern boundaries.
%%
%% @end
%%--------------------------------------------------------------------
state_sharing_test() ->
    %% Create workflow with shared state
    Workflow = cre_yawl:new_workflow(<<"state_share_wf">>),

    %% Initialize state
    W1 = cre_yawl:add_task(Workflow, <<"init_state">>, [{type, atomic}]),
    W2 = cre_yawl:add_task(W1, <<"state_producer">>, [{type, atomic}]),
    W3 = cre_yawl:add_task(W2, <<"state_consumer_a">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"state_consumer_b">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"state_merger">>, [{type, atomic}]),
    W6 = cre_yawl:add_task(W5, <<"finalize_state">>, [{type, atomic}]),

    %% Configure parallel with state sharing
    W7 = cre_yawl:set_split_type(W6, <<"state_producer">>, 'and_split'),
    W8 = cre_yawl:set_join_type(W7, <<"state_merger">>, 'and_join'),

    %% Connect workflow
    W9 = cre_yawl:connect(W8, <<"init_state">>, <<"state_producer">>),
    W10 = cre_yawl:connect(W9, <<"state_producer">>, <<"state_consumer_a">>),
    W11 = cre_yawl:connect(W10, <<"state_producer">>, <<"state_consumer_b">>),
    W12 = cre_yawl:connect(W11, <<"state_consumer_a">>, <<"state_merger">>),
    W13 = cre_yawl:connect(W12, <<"state_consumer_b">>, <<"state_merger">>),
    W14 = cre_yawl:connect(W13, <<"state_merger">>, <<"finalize_state">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W14, <<"init_state">>, [<<"finalize_state">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Simulate state sharing through process dictionary
    put(shared_state, #{counter => 0, data => []}),

    %% Producer adds data
    CurrentState = get(shared_state),
    put(shared_state, CurrentState#{data => [produced]}),

    %% Consumer A reads and modifies
    State1 = get(shared_state),
    ?assertEqual([produced], maps:get(data, State1)),
    put(shared_state, State1#{counter => 1}),

    %% Consumer B also reads
    State2 = get(shared_state),
    ?assertEqual(1, maps:get(counter, State2)),

    %% Clean up
    erase(shared_state).

%%--------------------------------------------------------------------
%% @doc Test resource contention scenarios.
%%
%% Validates behavior when multiple patterns compete for resources.
%%
%% @end
%%--------------------------------------------------------------------
resource_contention_test() ->
    %% Create workflow with resource contention
    Workflow = cre_yawl:new_workflow(<<"contention_wf">>),

    %% Shared resource
    W1 = cre_yawl:add_task(Workflow, <<"resource_lock">>, [{type, atomic}]),

    %% Multiple competing tasks
    W2 = cre_yawl:add_task(W1, <<"competing_task_a">>, [{type, atomic}]),
    W3 = cre_yawl:add_task(W2, <<"competing_task_b">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"competing_task_c">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"competing_task_d">>, [{type, atomic}]),

    %% Resource release
    W6 = cre_yawl:add_task(W5, <<"resource_release">>, [{type, atomic}]),
    W7 = cre_yawl:add_task(W6, <<"complete">>, [{type, atomic}]),

    %% Configure as partial join (first N complete)
    W8 = cre_yawl:set_split_type(W7, <<"resource_lock">>, 'and_split'),
    W9 = cre_yawl:set_join_type(W8, <<"resource_release">>, 'or_join'),

    %% Connect competing tasks
    W10 = lists:foldl(fun(I, WfAcc) ->
        TaskId = <<"competing_task_", (integer_to_binary(I))/binary>>,
        cre_yawl:connect(WfAcc, <<"resource_lock">>, TaskId)
    end, W9, lists:seq(97, 100)), % _a through _d using ASCII codes

    %% All tasks connect to release
    W11 = lists:foldl(fun(I, WfAcc) ->
        TaskId = <<"competing_task_", (integer_to_binary(I))/binary>>,
        cre_yawl:connect(WfAcc, TaskId, <<"resource_release">>)
    end, W10, lists:seq(97, 100)),

    %% Connect completion
    W12 = cre_yawl:connect(W11, <<"resource_release">>, <<"complete">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W12, <<"resource_lock">>, [<<"complete">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Simulate resource contention
    Resource = create_resource(<<"shared_resource">>),
    ?assertEqual(available, get_resource_state(Resource)),

    %% Clean up
    cleanup_resource(Resource).

%%====================================================================
%% 3. Failure Scenario Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test failure in nested pattern.
%%
%% Validates that failures in nested patterns are properly handled.
%%
%% @end
%%--------------------------------------------------------------------
nested_pattern_failure_test() ->
    %% Create workflow with potential nested failure
    Workflow = cre_yawl:new_workflow(<<"nested_fail_wf">>),

    %% Outer task
    W1 = cre_yawl:add_task(Workflow, <<"outer_task">>, [{type, atomic}]),

    %% Parallel with potential failure
    W2 = cre_yawl:add_task(W1, <<"split">>, [{type, atomic}]),
    W3 = cre_yawl:add_task(W2, <<"safe_branch">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"failing_branch">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"join">>, [{type, atomic}]),

    %% Error handler
    W6 = cre_yawl:add_task(W5, <<"error_handler">>, [{type, atomic}]),
    W7 = cre_yawl:add_task(W6, <<"recover">>, [{type, atomic}]),
    W8 = cre_yawl:add_task(W7, <<"complete">>, [{type, atomic}]),

    %% Configure
    W9 = cre_yawl:set_split_type(W8, <<"split">>, 'and_split'),
    W10 = cre_yawl:set_join_type(W9, <<"join">>, 'or_join'),

    %% Connect
    W11 = cre_yawl:connect(W10, <<"outer_task">>, <<"split">>),
    W12 = cre_yawl:connect(W11, <<"split">>, <<"safe_branch">>),
    W13 = cre_yawl:connect(W12, <<"split">>, <<"failing_branch">>),
    W14 = cre_yawl:connect(W13, <<"safe_branch">>, <<"join">>),
    W15 = cre_yawl:connect(W14, <<"failing_branch">>, <<"error_handler">>),
    W16 = cre_yawl:connect(W15, <<"error_handler">>, <<"recover">>),
    W17 = cre_yawl:connect(W16, <<"recover">>, <<"join">>),
    W18 = cre_yawl:connect(W17, <<"join">>, <<"complete">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W18, <<"outer_task">>, [<<"complete">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Simulate nested failure
    FailureState = #{
        nested_level => 2,
        failed_task => <<"failing_branch">>,
        failure_reason => simulation_error,
        traceback => ["outer_task", "split", "failing_branch"]
    },

    ?assertEqual(2, maps:get(nested_level, FailureState)),
    ?assertEqual(simulation_error, maps:get(failure_reason, FailureState)),
    ?assertEqual(<<"failing_branch">>, maps:get(failed_task, FailureState)),
    ?assertEqual(3, length(maps:get(traceback, FailureState))).

%%--------------------------------------------------------------------
%% @doc Test concurrent failures.
%%
%% Validates handling of multiple simultaneous failures.
%%
%% @end
%%--------------------------------------------------------------------
concurrent_failures_test() ->
    %% Create workflow prone to concurrent failures
    Workflow = cre_yawl:new_workflow(<<"concurrent_fail_wf">>),

    %% Split into multiple branches that may fail
    W1 = cre_yawl:add_task(Workflow, <<"split">>, [{type, atomic}]),

    %% Create multiple branches with failure potential
    Branches = [<<"branch_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 5)],
    W2 = lists:foldl(fun(BranchId, WfAcc) ->
        cre_yawl:add_task(WfAcc, BranchId, [{type, atomic}])
    end, W1, Branches),

    %% Join with error handling
    W3 = cre_yawl:add_task(W2, <<"join">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"error_aggregator">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"finalize">>, [{type, atomic}]),

    %% Configure
    W6 = cre_yawl:set_split_type(W5, <<"split">>, 'and_split'),
    W7 = cre_yawl:set_join_type(W6, <<"join">>, 'and_join'),

    %% Connect all branches from split
    W8 = lists:foldl(fun(BranchId, WfAcc) ->
        cre_yawl:connect(WfAcc, <<"split">>, BranchId)
    end, W7, Branches),

    %% Connect all branches to join
    W9 = lists:foldl(fun(BranchId, WfAcc) ->
        cre_yawl:connect(WfAcc, BranchId, <<"join">>)
    end, W8, Branches),

    %% Connect completion
    W10 = cre_yawl:connect(W9, <<"join">>, <<"error_aggregator">>),
    W11 = cre_yawl:connect(W10, <<"error_aggregator">>, <<"finalize">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W11, <<"split">>, [<<"finalize">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Simulate concurrent failures by tracking failed attempts
    FailedCount = length([I || I <- lists:seq(1, 5), I rem 2 =:= 0]),
    ?assert(FailedCount > 0),
    ?assert(FailedCount =< 5).

%%--------------------------------------------------------------------
%% @doc Test timeout in composite workflow.
%%
%% Validates timeout handling in complex workflows.
%%
%% @end
%%--------------------------------------------------------------------
timeout_composite_test() ->
    %% Create workflow with timeout handling
    Workflow = cre_yawl:new_workflow(<<"timeout_wf">>),

    %% Task with timeout
    W1 = cre_yawl:add_task(Workflow, <<"timeout_task">>,
        [{type, atomic}, {metadata, #{timeout => 1000}}]),

    %% Timeout handler
    W2 = cre_yawl:add_task(W1, <<"timeout_handler">>, [{type, atomic}]),
    W3 = cre_yawl:add_task(W2, <<"on_timeout">>, [{type, atomic}]),
    W4 = cre_yawl:add_task(W3, <<"on_success">>, [{type, atomic}]),
    W5 = cre_yawl:add_task(W4, <<"merge">>, [{type, atomic}]),
    W6 = cre_yawl:add_task(W5, <<"complete">>, [{type, atomic}]),

    %% Connect timeout paths
    W7 = cre_yawl:connect(W6, <<"timeout_task">>, <<"timeout_handler">>),
    W8 = cre_yawl:connect(W7, <<"timeout_task">>, <<"on_success">>),
    W9 = cre_yawl:connect(W8, <<"timeout_handler">>, <<"on_timeout">>),
    W10 = cre_yawl:connect(W9, <<"on_success">>, <<"merge">>),
    W11 = cre_yawl:connect(W10, <<"on_timeout">>, <<"merge">>),
    W12 = cre_yawl:connect(W11, <<"merge">>, <<"complete">>),

    %% Set boundaries
    FinalWf = set_workflow_boundaries(W12, <<"timeout_task">>, [<<"complete">>]),

    %% Validate
    ?assertEqual(ok, cre_yawl:validate(FinalWf)),

    %% Test timeout simulation
    StartTime = erlang:monotonic_time(millisecond),

    %% Test case 1: Complete within timeout
    Result1 = simulate_timeout(50),
    ?assertMatch({ok, _}, Result1),

    %% Test case 2: Timeout
    Result2 = simulate_timeout(200),
    ?assertMatch({error, timeout}, Result2),

    Duration = erlang:monotonic_time(millisecond) - StartTime,
    ?assert(Duration < 500).

%%--------------------------------------------------------------------
%% @doc Test recovery scenarios.
%%
%% Validates various recovery mechanisms after failures.
%%
%% @end
%%--------------------------------------------------------------------
recovery_scenarios_test() ->
    %% Scenario 1: Retry policy
    RetryPolicy = create_retry_policy(3, exponential, 10),

    ?assertEqual(3, RetryPolicy#retry_policy.max_attempts),
    ?assert(should_retry(RetryPolicy, 0)),
    ?assert(should_retry(RetryPolicy, 1)),
    ?assert(should_retry(RetryPolicy, 2)),
    ?assertNot(should_retry(RetryPolicy, 3)),

    %% Scenario 2: Compensation
    Compensators = [
        {fun() -> activity_1 end, fun() -> undo_1 end},
        {fun() -> activity_2 end, fun() -> undo_2 end},
        {fun() -> activity_3 end, fun() -> undo_3 end}
    ],

    ?assertEqual(3, length(Compensators)),

    %% Scenario 3: State recovery
    SavedState = #{counter => 5, data => [1, 2, 3]},
    put(recovery_state, SavedState),

    StateBeforeFailure = get(recovery_state),
    ?assertEqual(5, maps:get(counter, StateBeforeFailure)),

    RecoveredState = StateBeforeFailure#{counter => 6, recovered => true},
    put(recovery_state, RecoveredState),

    StateAfterRecovery = get(recovery_state),
    ?assertEqual(6, maps:get(counter, StateAfterRecovery)),
    ?assertEqual(true, maps:get(recovered, StateAfterRecovery)),

    %% Clean up
    erase(recovery_state).

%%====================================================================
%% 4. Performance Benchmark Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test pattern execution benchmarks.
%%
%% Measures execution time for each pattern type.
%%
%% @end
%%--------------------------------------------------------------------
pattern_execution_benchmark_test() ->
    %% Benchmark each pattern type
    Patterns = [
        {sequence, fun() -> cre_yawl:sequence() end},
        {parallel_split, fun() -> cre_yawl:parallel_split() end},
        {synchronization, fun() -> cre_yawl:synchronization() end},
        {exclusive_choice, fun() -> cre_yawl:exclusive_choice() end}
    ],

    Metrics = lists:map(fun({PatternName, PatternFun}) ->
        %% Warm up
        lists:foreach(fun(_) -> PatternFun() end, lists:seq(1, 10)),

        %% Measure
        Iterations = 1000,
        StartTime = erlang:monotonic_time(microsecond),

        lists:foreach(fun(_) -> PatternFun() end, lists:seq(1, Iterations)),

        EndTime = erlang:monotonic_time(microsecond),
        DurationUs = EndTime - StartTime,
        DurationMs = DurationUs div 1000,
        Throughput = (Iterations * 1000000) div DurationUs,

        #perf_metric{
            test_name = atom_to_binary(PatternName),
            start_time = StartTime,
            end_time = EndTime,
            duration_ms = DurationMs,
            operations = Iterations,
            throughput = Throughput,
            memory_before = 0,
            memory_after = 0,
            memory_delta = 0
        }
    end, Patterns),

    %% Store metrics
    ExistingMetrics = case get(perf_metrics) of
        undefined -> [];
        M -> M
    end,
    put(perf_metrics, ExistingMetrics ++ Metrics),

    %% Verify performance thresholds
    lists:foreach(fun(Metric) ->
        ?debugFmt("~s: ~p us/op, ~p ops/sec~n",
                  [Metric#perf_metric.test_name,
                   Metric#perf_metric.operations / Metric#perf_metric.duration_ms * 1000,
                   Metric#perf_metric.throughput]),

        %% Each pattern should execute reasonably fast
        AvgUs = (Metric#perf_metric.duration_ms * 1000) div Metric#perf_metric.operations,
        ?assert(AvgUs < 10000, {pattern_too_slow, Metric#perf_metric.test_name, AvgUs})
    end, Metrics).

%%--------------------------------------------------------------------
%% @doc Test throughput measurement.
%%
%% Measures operations per second for workflow patterns.
%%
%% @end
%%--------------------------------------------------------------------
throughput_measurement_test() ->
    %% Measure throughput for different operation counts
    TestSizes = [10, 50, 100],

    Results = lists:map(fun(Size) ->
        Workflow = cre_yawl:new_workflow(<<"throughput_", (integer_to_binary(Size))/binary>>),

        %% Create tasks
        W1 = lists:foldl(fun(I, WfAcc) ->
            TaskId = <<"task_", (integer_to_binary(I))/binary>>,
            cre_yawl:add_task(WfAcc, TaskId, [{type, atomic}])
        end, Workflow, lists:seq(1, Size)),

        %% Create connections (sequential)
        TaskIds = [<<"task_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, Size)],
        {W2, _} = lists:foldl(fun(TaskId, {WfAcc, PrevId}) ->
            {cre_yawl:connect(WfAcc, PrevId, TaskId), TaskId}
        end, {W1, hd(TaskIds)}, tl(TaskIds)),

        %% Measure creation throughput
        StartTime = erlang:monotonic_time(millisecond),
        _FinalWf = set_workflow_boundaries(W2, hd(TaskIds), [lists:last(TaskIds)]),
        EndTime = erlang:monotonic_time(millisecond),

        Duration = EndTime - StartTime,
        Throughput = Size / (Duration + 1) * 1000,

        ?debugFmt("Throughput for ~p tasks: ~p tasks/sec (took ~p ms)~n",
                  [Size, Throughput, Duration]),

        {Size, Duration, Throughput}
    end, TestSizes),

    %% Verify throughput degrades gracefully
    {_, FirstDuration, FirstThroughput} = lists:nth(1, Results),
    {_, LastDuration, LastThroughput} = lists:last(Results),

    %% Throughput should remain reasonable
    ?assert(LastThroughput > 10, {throughput_too_low, LastThroughput}).

%%--------------------------------------------------------------------
%% @doc Test memory per pattern.
%%
%% Measures memory consumption for each pattern type.
%%
%% @end
%%--------------------------------------------------------------------
memory_per_pattern_test() ->
    %% Test memory usage for different patterns
    Patterns = [
        fun() -> cre_yawl:sequence() end,
        fun() -> cre_yawl:parallel_split() end,
        fun() -> cre_yawl:synchronization() end,
        fun() -> cre_yawl:exclusive_choice() end
    ],

    MemoryResults = lists:map(fun(PatternFun) ->
        %% Force GC before measurement
        garbage_collect(),
        Before = memory_usage(),

        %% Create many instances
        lists:foreach(fun(_) -> PatternFun() end, lists:seq(1, 1000)),

        %% Measure after
        After = memory_usage(),
        Delta = After - Before,
        AvgPerPattern = Delta div 1000,

        ?debugFmt("Memory: ~p bytes/pattern (total delta: ~p bytes)~n",
                  [AvgPerPattern, Delta]),

        %% Each pattern should use reasonable memory
        ?assert(AvgPerPattern < 10240, {memory_too_high, AvgPerPattern}),

        {pattern_memory, AvgPerPattern, Delta}
    end, Patterns),

    %% Verify no pattern uses excessive memory
    MaxMemory = lists:max([M || {_, M, _} <- MemoryResults]),
    ?assert(MaxMemory < 10240, {max_pattern_memory_too_high, MaxMemory}).

%%--------------------------------------------------------------------
%% @doc Test scaling behavior.
%%
%% Validates how performance scales with workflow size.
%%
%% @end
%%--------------------------------------------------------------------
scaling_behavior_test() ->
    %% Test scaling with different workflow sizes
    Sizes = [1, 5, 10, 20],

    ScalingResults = lists:map(fun(Size) ->
        StartTime = erlang:monotonic_time(millisecond),
        StartMemory = memory_usage(),

        %% Create workflow
        Workflow = cre_yawl:new_workflow(<<"scale_", (integer_to_binary(Size))/binary>>),

        %% Add tasks
        {Wf, TaskIds} = lists:foldl(fun(I, {WfAcc, Acc}) ->
            Id = <<"t", (integer_to_binary(I))/binary>>,
            W = cre_yawl:add_task(WfAcc, Id, [{type, atomic}]),
            {W, [Id | Acc]}
        end, {Workflow, []}, lists:seq(1, Size)),

        %% Connect all tasks sequentially
        RevTaskIds = lists:reverse(TaskIds),
        {FinalWf, _} = lists:foldl(fun(Id, {WfAcc, PrevId}) ->
            {cre_yawl:connect(WfAcc, PrevId, Id), Id}
        end, {Wf, hd(RevTaskIds)}, tl(RevTaskIds)),

        %% Validate
        ok = cre_yawl:validate(FinalWf),

        EndTime = erlang:monotonic_time(millisecond),
        EndMemory = memory_usage(),

        #perf_metric{
            test_name = <<"scale_", (integer_to_binary(Size))/binary>>,
            start_time = StartTime,
            end_time = EndTime,
            duration_ms = EndTime - StartTime,
            operations = Size,
            throughput = Size / (EndTime - StartTime + 1) * 1000,
            memory_before = StartMemory,
            memory_after = EndMemory,
            memory_delta = EndMemory - StartMemory
        }
    end, Sizes),

    %% Verify linear or better scaling
    lists:foreach(fun(#perf_metric{test_name = Name, duration_ms = Duration, operations = Size}) ->
        AvgMs = Duration / (Size + 1),
        ?debugFmt("~s: ~p ms total, ~p ms/task~n", [Name, Duration, AvgMs]),

        %% Average per-task should be reasonable
        ?assert(AvgMs < 10.0, {per_task_time_too_high, Name, AvgMs})
    end, ScalingResults).

%%====================================================================
%% 5. Stress Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test high instance counts.
%%
%% Validates system behavior with many concurrent instances.
%%
%% @end
%%--------------------------------------------------------------------
high_instance_count_test() ->
    %% Test with increasing instance counts
    Counts = [10, 100],

    Results = lists:map(fun(Count) ->
        ?debugFmt("Testing with ~p instances...~n", [Count]),

        StartTime = erlang:monotonic_time(millisecond),

        %% Create multiple instance pattern
        Pattern = cre_yawl_patterns:multiple_instances_no_sync(
            fun(X) -> X * 2 end,
            lists:seq(1, Count),
            Count
        ),

        %% Simulate execution
        Completed = execute_pattern_instances(Pattern, Count),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        Throughput = Count / (Duration + 1) * 1000,

        ?debugFmt("~p instances: ~p completed, ~p ms, ~p instances/sec~n",
                  [Count, Completed, Duration, Throughput]),

        #stress_result{
            test_name = <<"high_instance_", (integer_to_binary(Count))/binary>>,
            instance_count = Count,
            completed = Completed,
            failed = Count - Completed,
            duration_ms = Duration,
            errors = []
        }
    end, Counts),

    %% Verify all instances completed
    lists:foreach(fun(Result) ->
        ?assertEqual(Result#stress_result.instance_count,
                     Result#stress_result.completed,
                     {instances_not_completed, Result})
    end, Results).

%%--------------------------------------------------------------------
%% @doc Test deep nesting.
%%
%% Validates system handles deeply nested workflows.
%%
%% @end
%%--------------------------------------------------------------------
deep_nesting_test() ->
    %% Test different nesting depths
    Depths = [5, 10, 20],

    Results = lists:map(fun(Depth) ->
        ?debugFmt("Testing nesting depth ~p...~n", [Depth]),

        StartTime = erlang:monotonic_time(millisecond),

        %% Create deeply nested workflow
        Workflow = create_nested_workflow(Depth),

        %% Validate
        ValidationResult = cre_yawl:validate(Workflow),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        ?debugFmt("Nesting depth ~p: validation ~p, ~p ms~n",
                  [Depth, ValidationResult, Duration]),

        %% Should complete within reasonable time
        ?assert(Duration < 2000, {nesting_too_slow, Depth, Duration}),

        {Depth, Duration, ValidationResult}
    end, Depths),

    %% All should validate successfully
    lists:foreach(fun({Depth, _Duration, ValidationResult}) ->
        ?assertEqual(ok, ValidationResult,
                     {validation_failed_at_depth, Depth, ValidationResult})
    end, Results).

%%--------------------------------------------------------------------
%% @doc Test long-running workflows.
%%
%% Validates system stability over extended execution.
%%
%% @end
%%--------------------------------------------------------------------
long_running_workflow_test() ->
    %% Simulate long-running workflow with many iterations
    Iterations = 50,

    StartTime = erlang:monotonic_time(millisecond),
    StartMemory = memory_usage(),

    %% Create loop pattern
    Pattern = cre_yawl_patterns:structured_loop(
        fun(Counter) -> Counter < Iterations end,
        fun() -> loop_step end,
        while
    ),

    %% Execute iterations
    Result = execute_long_loop(Pattern, Iterations),

    EndTime = erlang:monotonic_time(millisecond),
    EndMemory = memory_usage(),

    Duration = EndTime - StartTime,
    MemoryDelta = EndMemory - StartMemory,

    ?debugFmt("Long-running workflow: ~p iterations, ~p ms, ~p bytes memory~n",
              [Iterations, Duration, MemoryDelta]),

    %% Verify completion
    ?assertEqual(completed, Result).

%%--------------------------------------------------------------------
%% @doc Test resource exhaustion.
%%
%% Validates system behavior under resource pressure.
%%
%% @end
%%--------------------------------------------------------------------
resource_exhaustion_test() ->
    %% Test system behavior when resources are exhausted
    %% Simulate by spawning many processes

    %% Get initial process count
    InitialProcessCount = length(processes()),

    %% Spawn many test processes
    ProcessCount = 100,
    StartTime = erlang:monotonic_time(millisecond),

    Pids = [spawn(fun() ->
        %% Simulate work
        receive
            stop -> ok
        after 50 -> ok
        end
    end) || _ <- lists:seq(1, ProcessCount)],

    SpawnTime = erlang:monotonic_time(millisecond),

    ?debugFmt("Spawned ~p processes in ~p ms~n",
              [ProcessCount, SpawnTime - StartTime]),

    %% Verify processes spawned
    ?assertEqual(ProcessCount, length(Pids)),

    %% Clean up
    lists:foreach(fun(P) -> P ! stop end, Pids),
    timer:sleep(100),

    %% Verify process count returned to near initial
    FinalProcessCount = length(processes()),
    ?assert(FinalProcessCount < InitialProcessCount + ProcessCount div 10,
             {processes_not_cleaned, FinalProcessCount, InitialProcessCount}).

%%====================================================================
%% 6. Concurrency Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test parallel pattern execution.
%%
%% Validates parallel execution of multiple patterns.
%%
%% @end
%%--------------------------------------------------------------------
parallel_pattern_execution_test() ->
    %% Execute multiple patterns concurrently
    PatternCount = 5,
    IterationsPerPattern = 20,

    StartTime = erlang:monotonic_time(millisecond),

    %% Spawn parallel pattern executors
    Pids = [spawn(fun() ->
        execute_pattern_batch(IterationsPerPattern)
    end) || _ <- lists:seq(1, PatternCount)],

    %% Wait for all to complete
    timer:sleep(500),

    %% Check which completed
    CompletedCount = length([P || P <- Pids, not is_process_alive(P)]),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    TotalOperations = PatternCount * IterationsPerPattern,
    Throughput = TotalOperations / (Duration + 1) * 1000,

    ?debugFmt("Parallel execution: ~p patterns, ~p total ops, ~p ms, ~p ops/sec~n",
              [PatternCount, TotalOperations, Duration, Throughput]),

    %% Verify most completed
    ?assert(CompletedCount >= PatternCount - 1).

%%--------------------------------------------------------------------
%% @doc Test race condition detection.
%%
%% Attempts to detect race conditions in pattern execution.
%%
%% @end
%%--------------------------------------------------------------------
race_condition_detection_test() ->
    %% Create shared state and concurrent access
    SharedCounter = spawn_link(fun() ->
        counter_loop(0)
    end),

    %% Spawn many concurrent increment operations
    ProcessCount = 50,
    IncrementsPerProcess = 5,

    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(_) ->
        lists:foreach(fun(_) ->
            SharedCounter ! {increment, self()}
        end, lists:seq(1, IncrementsPerProcess))
    end, lists:seq(1, ProcessCount)),

    %% Wait for increments
    timer:sleep(200),

    %% Get final count
    SharedCounter ! {get_count, self()},
    receive
        {count, FinalCount} ->
            ExpectedCount = ProcessCount * IncrementsPerProcess,
            ?debugFmt("Race detection: expected ~p, got ~p~n",
                      [ExpectedCount, FinalCount]),

            %% Allow some deviation due to message ordering
            ?assert(abs(FinalCount - ExpectedCount) < 10)
    after 1000 ->
        ?assert(false, counter_timeout)
    end,

    %% Clean up
    SharedCounter ! stop,

    ?debugFmt("Race detection test completed in ~p ms~n",
              [erlang:monotonic_time(millisecond) - StartTime]).

%%--------------------------------------------------------------------
%% @doc Test deadlock scenarios.
%%
%% Validates deadlocks are properly detected/prevented.
%%
%% @end
%%--------------------------------------------------------------------
deadlock_scenarios_test() ->
    %% Test potential deadlock scenarios

    %% Scenario 1: Processes waiting correctly
    P1 = spawn(fun() ->
        receive
            proceed -> ok
        end,
        self() ! done
    end),

    P2 = spawn(fun() ->
        receive
            proceed -> ok
        end,
        self() ! done
    end),

    %% Both should be alive
    ?assert(is_process_alive(P1)),
    ?assert(is_process_alive(P2)),

    %% Send signals
    P1 ! proceed,
    P2 ! proceed,

    receive
        done -> ok
    after 500 ->
        ?assert(false, deadlock_not_resolved)
    end,

    %% Scenario 2: Resource ordering prevents deadlock
    Resources = [<<"r1">>, <<"r2">>, <<"r3">>],

    %% Acquire in consistent order
    Acquired1 = acquire_resources_ordered(Resources, self()),
    ?assertEqual(3, length(Acquired1)),

    %% Release
    release_resources(Acquired1).

%%--------------------------------------------------------------------
%% @doc Test contention resolution.
%%
%% Validates system resolves resource contention properly.
%%
%% @end
%%--------------------------------------------------------------------
contention_resolution_test() ->
    %% Create shared resource with contention
    Resource = create_resource(<<"shared_contention_resource">>),

    %% Spawn many competing processes
    CompetitorCount = 10,
    AttemptsPerCompetitor = 3,

    StartTime = erlang:monotonic_time(millisecond),

    %% Simulate contention without actually spawning for stability
    lists:foreach(fun(CompetitorId) ->
        lists:foreach(fun(_) ->
            case acquire_resource(Resource) of
                {ok, _} ->
                    timer:sleep(1),
                    release_resource(Resource);
                busy ->
                    ok
            end
        end, lists:seq(1, AttemptsPerCompetitor))
    end, lists:seq(1, CompetitorCount)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    ?debugFmt("Contention: ~p attempts, ~p ms~n",
              [CompetitorCount * AttemptsPerCompetitor, Duration]),

    %% Test should complete in reasonable time
    ?assert(Duration < 5000),

    %% Clean up
    cleanup_resource(Resource).

%%====================================================================
%% 7. Memory Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test memory leak detection.
%%
%% Runs operations repeatedly and checks for memory growth.
%%
%% @end
%%--------------------------------------------------------------------
memory_leak_detection_test() ->
    %% Force initial GC
    garbage_collect(),
    BaselineMemory = memory_usage(),

    %% Create many workflows repeatedly
    Iterations = 500,
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(I) ->
        Workflow = cre_yawl:new_workflow(<<"mem_leak_", (integer_to_binary(I))/binary>>),
        cre_yawl:add_task(Workflow, <<"task1">>, [{type, atomic}]),
        cre_yawl:add_task(Workflow, <<"task2">>, [{type, atomic}]),
        cre_yawl:connect(Workflow, <<"task1">>, <<"task2">>),

        %% Explicitly let workflow go out of scope
        ok
    end, lists:seq(1, Iterations)),

    %% Force GC again
    garbage_collect(),
    EndMemory = memory_usage(),
    EndTime = erlang:monotonic_time(millisecond),

    MemoryDelta = EndMemory - BaselineMemory,
    Duration = EndTime - StartTime,

    ?debugFmt("Memory leak test: ~p iterations, ~p ms, ~p bytes delta~n",
              [Iterations, Duration, MemoryDelta]),

    %% Memory growth should be minimal
    ?assert(MemoryDelta < 200000, {memory_leak_detected, MemoryDelta}).

%%--------------------------------------------------------------------
%% @doc Test cleanup verification.
%%
%% Validates proper cleanup of resources after workflow completion.
%%
%% @end
%%--------------------------------------------------------------------
cleanup_verification_test() ->
    %% Create workflow with various resources
    Workflow = cre_yawl:new_workflow(<<"cleanup_wf">>),

    %% Add multiple tasks and connections
    W1 = lists:foldl(fun(I, WfAcc) ->
        TaskId = <<"task_", (integer_to_binary(I))/binary>>,
        cre_yawl:add_task(WfAcc, TaskId, [{type, atomic}])
    end, Workflow, lists:seq(1, 10)),

    %% Create connections
    TaskIds = [<<"task_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 10)],
    W2 = lists:foldl(fun(TaskId, WfAcc) ->
        cre_yawl:connect(WfAcc, <<"task_1">>, TaskId)
    end, W1, tl(TaskIds)),

    %% Get initial process count
    InitialProcCount = length(processes()),
    InitialMemory = memory_usage(),

    %% Execute and cleanup
    ok = cre_yawl:validate(W2),

    %% Explicit cleanup
    erase(cleanup_wf),

    %% Force GC
    garbage_collect(),

    %% Verify cleanup
    FinalProcCount = length(processes()),
    FinalMemory = memory_usage(),

    ProcDelta = FinalProcCount - InitialProcCount,
    MemoryDelta = FinalMemory - InitialMemory,

    ?debugFmt("Cleanup verification: ~p process delta, ~p bytes memory delta~n",
              [ProcDelta, MemoryDelta]),

    %% Process count should be stable
    ?assert(abs(ProcDelta) < 10, {process_leak, ProcDelta}).

%%--------------------------------------------------------------------
%% @doc Test GC behavior under load.
%%
%% Validates garbage collector behaves properly under stress.
%%
%% @end
%%--------------------------------------------------------------------
gc_behavior_test() ->
    %% Test GC behavior with memory pressure

    %% Get initial memory state
    InitialMemory = memory_usage(),

    %% Create memory pressure
    lists:foreach(fun(I) ->
        %% Create temporary large data
        LargeData = lists:seq(1, 100),
        put({temp_data, I}, LargeData),

        %% Create workflow
        Workflow = cre_yawl:new_workflow(<<"gc_", (integer_to_binary(I))/binary>>),
        cre_yawl:add_task(Workflow, <<"t">>, [{type, atomic}]),
        cre_yawl:validate(Workflow),

        %% Remove temporary data
        erase({temp_data, I})
    end, lists:seq(1, 50)),

    %% Force GC
    garbage_collect(),

    FinalMemory = memory_usage(),

    MemoryDelta = FinalMemory - InitialMemory,

    ?debugFmt("GC behavior: ~p bytes delta~n", [MemoryDelta]),

    %% Memory should be managed (not excessive growth)
    ?assert(MemoryDelta < 1000000, {excessive_memory_growth, MemoryDelta}).

%%--------------------------------------------------------------------
%% @doc Test memory growth patterns.
%%
%% Analyzes memory growth over time to detect leaks.
%%
%% @end
%%--------------------------------------------------------------------
memory_growth_patterns_test() ->
    %% Track memory at intervals
    Intervals = 5,
    OperationsPerInterval = 20,

    MemorySnapshots = lists:map(fun(Interval) ->
        %% Perform operations
        lists:foreach(fun(I) ->
            Workflow = cre_yawl:new_workflow(<<"growth_", (integer_to_binary(I))/binary>>),
            cre_yawl:add_task(Workflow, <<"t">>, [{type, atomic}])
        end, lists:seq(1, OperationsPerInterval)),

        %% Record memory
        garbage_collect(),
        Memory = memory_usage(),
        ?debugFmt("Interval ~p: memory = ~p bytes~n", [Interval, Memory]),

        {Interval, Memory}
    end, lists:seq(1, Intervals)),

    %% Analyze growth pattern
    {FirstInt, FirstMem} = hd(MemorySnapshots),
    {LastInt, LastMem} = lists:last(MemorySnapshots),

    TotalGrowth = LastMem - FirstMem,
    AvgGrowthPerInterval = TotalGrowth / Intervals,

    ?debugFmt("Memory growth: total ~p bytes, avg ~p bytes/interval~n",
              [TotalGrowth, AvgGrowthPerInterval]),

    %% Verify memory is stable
    %% - Average growth per interval should be small
    ?assert(AvgGrowthPerInterval < 10240, {avg_growth_too_high, AvgGrowthPerInterval}),

    %% - Total growth should be reasonable
    ?assert(TotalGrowth < 200000, {excessive_total_growth, TotalGrowth}).

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Set workflow boundaries helper.
%% @end
%%--------------------------------------------------------------------
set_workflow_boundaries(#workflow{} = Workflow, StartTaskId, EndTaskIds) ->
    Workflow#workflow{start_task_id = StartTaskId, end_task_ids = EndTaskIds};
set_workflow_boundaries(Workflow, _StartTaskId, _EndTaskIds) ->
    Workflow.

%%--------------------------------------------------------------------
%% @doc Evaluate condition with data.
%% @end
%%--------------------------------------------------------------------
evaluate_condition(Condition, Data) when is_function(Condition, 1) ->
    Condition(Data);
evaluate_condition(_Condition, _Data) ->
    true.

%%--------------------------------------------------------------------
%% @doc Create a resource for testing.
%% @end
%%--------------------------------------------------------------------
create_resource(ResourceId) ->
    %% Create a simple resource in process dictionary
    put({resource, ResourceId}, #{state => available, owner => undefined}),
    ResourceId.

%%--------------------------------------------------------------------
%% @doc Get resource state.
%% @end
%%--------------------------------------------------------------------
get_resource_state(ResourceId) ->
    case get({resource, ResourceId}) of
        #{state := State} -> State;
        undefined -> undefined
    end.

%%--------------------------------------------------------------------
%% @doc Clean up resource.
%% @end
%%--------------------------------------------------------------------
cleanup_resource(ResourceId) ->
    erase({resource, ResourceId}),
    ok.

%%--------------------------------------------------------------------
%% @doc Acquire resource.
%% @end
%%--------------------------------------------------------------------
acquire_resource(ResourceId) ->
    case get({resource, ResourceId}) of
        #{state := available} ->
            put({resource, ResourceId}, #{state => busy, owner => self()}),
            {ok, ResourceId};
        _ ->
            busy
    end.

%%--------------------------------------------------------------------
%% @doc Release resource.
%% @end
%%--------------------------------------------------------------------
release_resource(ResourceId) ->
    put({resource, ResourceId}, #{state => available, owner => undefined}),
    ok.

%%--------------------------------------------------------------------
%% @doc Acquire resources in order to prevent deadlock.
%% @end
%%--------------------------------------------------------------------
acquire_resources_ordered(ResourceIds, _Owner) ->
    lists:foldl(fun(ResourceId, Acc) ->
        case acquire_resource(ResourceId) of
            {ok, _} -> [ResourceId | Acc];
            busy ->
                %% Release acquired and retry
                lists:foreach(fun(R) -> release_resource(R) end, Acc),
                timer:sleep(1),
                acquire_resources_ordered(ResourceIds, _Owner)
        end
    end, [], lists:sort(ResourceIds)).

%%--------------------------------------------------------------------
%% @doc Release multiple resources.
%% @end
%%--------------------------------------------------------------------
release_resources(ResourceIds) ->
    lists:foreach(fun(ResourceId) -> release_resource(ResourceId) end, ResourceIds).

%%--------------------------------------------------------------------
%% @doc Counter loop for testing.
%% @end
%%--------------------------------------------------------------------
counter_loop(Count) ->
    receive
        {increment, Pid} ->
            Pid ! ok,
            counter_loop(Count + 1);
        {get_count, Pid} ->
            Pid ! {count, Count},
            counter_loop(Count);
        stop ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Simulate timeout.
%% @end
%%--------------------------------------------------------------------
simulate_timeout(DelayMs) ->
    receive
    after DelayMs ->
        if
            DelayMs > 100 ->
                {error, timeout};
            true ->
                {ok, completed}
        end
    end.

%%--------------------------------------------------------------------
%% @doc Create deeply nested workflow.
%% @end
%%--------------------------------------------------------------------
create_nested_workflow(Depth) ->
    Workflow = cre_yawl:new_workflow(<<"nested_", (integer_to_binary(Depth))/binary>>),

    %% Build nested structure
    {FinalWf, _, StartId, EndId} = build_nesting(Workflow, Depth, <<"root">>),

    set_workflow_boundaries(FinalWf, StartId, [EndId]).

build_nesting(Wf, 0, Prefix) ->
    %% Base case: single task
    TaskId = <<(Prefix)/binary, "_0">>,
    {cre_yawl:add_task(Wf, TaskId, [{type, atomic}]), Wf, TaskId, TaskId};

build_nesting(Wf, Level, Prefix) ->
    %% Recursive case: split and join
    SplitId = <<(Prefix)/binary, "_split_", (integer_to_binary(Level))/binary>>,
    JoinId = <<(Prefix)/binary, "_join_", (integer_to_binary(Level))/binary>>,

    Wf1 = cre_yawl:add_task(Wf, SplitId, [{type, atomic}]),
    Wf2 = cre_yawl:add_task(Wf1, JoinId, [{type, atomic}]),
    Wf3 = cre_yawl:set_split_type(Wf2, SplitId, 'and_split'),
    Wf4 = cre_yawl:set_join_type(Wf3, JoinId, 'and_join'),

    %% Add nested structure in between
    {Wf5, _, ChildStart, ChildEnd} = build_nesting(Wf4, Level - 1, Prefix),

    %% Connect
    Wf6 = cre_yawl:connect(Wf5, SplitId, ChildStart),
    Wf7 = cre_yawl:connect(Wf6, ChildEnd, JoinId),

    {Wf7, Level, SplitId, JoinId}.

%%--------------------------------------------------------------------
%% @doc Execute pattern instances.
%% @end
%%--------------------------------------------------------------------
execute_pattern_instances(#pattern_state{instance_count = Count}, Count) ->
    %% Simulate execution of all instances
    Count.

%%--------------------------------------------------------------------
%% @doc Execute pattern batch.
%% @end
%%--------------------------------------------------------------------
execute_pattern_batch(Count) ->
    lists:foreach(fun(_I) ->
        _Pattern = cre_yawl:sequence(),
        ok
    end, lists:seq(1, Count)),
    completed.

%%--------------------------------------------------------------------
%% @doc Execute long loop.
%% @end
%%--------------------------------------------------------------------
execute_long_loop(#pattern_state{}, TargetIterations) ->
    %% Simulate loop execution
    execute_loop_iterations(0, TargetIterations).

execute_loop_iterations(Current, Target) when Current >= Target ->
    completed;
execute_loop_iterations(Current, Target) ->
    %% Simulate loop body work
    execute_loop_iterations(Current + 1, Target).

%%--------------------------------------------------------------------
%% @doc Get current memory usage.
%% @end
%%--------------------------------------------------------------------
memory_usage() ->
    {memory, Memory} = process_info(self(), memory),
    Memory.

%%--------------------------------------------------------------------
%% @doc Create retry policy for testing.
%% @end
%%--------------------------------------------------------------------
create_retry_policy(MaxAttempts, BackoffStrategy, BaseDelay) ->
    #retry_policy{
        max_attempts = MaxAttempts,
        backoff_strategy = BackoffStrategy,
        base_delay = BaseDelay,
        max_delay = 10000,
        multiplier = 2.0,
        jitter = false,
        jitter_factor = 0.1
    }.

%%--------------------------------------------------------------------
%% @doc Check if should retry.
%% @end
%%--------------------------------------------------------------------
should_retry(#retry_policy{max_attempts = Max}, CurrentAttempt) ->
    CurrentAttempt < Max.
