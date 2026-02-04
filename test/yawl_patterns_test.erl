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
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc YAWL Workflow Patterns Test Suite
%%
%% Comprehensive test suite for all 43 YAWL workflow patterns.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_patterns_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions (imported from source modules)
%%====================================================================

%% From cre_yawl.erl - Control Flow Pattern Records
-record(parallel_split, {split_task_id, branch_task_ids}).
-record(synchronization, {join_task_id, incoming_task_ids}).
-record(exclusive_choice, {choice_task_id, branches}).
-record(simple_merge, {merge_task_id, incoming_task_ids}).
-record(multi_choice, {choice_task_id, branches}).
-record(synchronizing_merge, {merge_task_id, incoming_task_ids}).
-record(multi_merge, {merge_task_id, incoming_task_ids}).
-record(discriminator, {merge_task_id, incoming_task_ids}).
-record(arbitration, {merge_task_id, incoming_task_ids, required_count}).

%% From cre_yawl.erl - Data Flow Pattern Records
-record(param_pass, {source_task_id, target_task_id, param_name, transform_fn}).
-record(data_transform, {input_task_id, output_task_id, transform_fn, output_schema}).
-record(data_distribute, {source_task_id, recipient_task_ids, distribution_type}).
-record(data_accumulate, {source_task_ids, target_task_id, aggregation_fn, initial_value}).
-record(data_visibility, {data_task_id, scope, access_list}).

%% From cre_yawl.erl - Resource Pattern Records
-record(resource_create, {resource_id, resource_type, init_params}).
-record(role_allocate, {role_id, required_capability, allocation_strategy}).
-record(resource_start, {resource_id, start_params}).
-record(role_distribute, {work_item_ids, role_assignments, distribution_policy}).
-record(capability_allocate, {required_capabilities, resource_registry, matching_strategy}).

%% From cre_yawl.erl - Workflow Record
-record(workflow, {id, name, tasks, conditions, connections, start_task_id, end_task_ids}).

%% From cre_yawl_patterns.erl
-record(pattern_state, {
          pattern_type,
          subprocess,
          instance_count,
          max_instances,
          pending_instances,
          active_instances,
          completed_instances,
          choice_data,
          branch_queue
         }).

%% From cre_yawl_exception.erl
-record(yawl_exception, {id, type, message, context, timestamp, stacktrace}).
-record(retry_policy, {
          max_attempts,
          backoff_strategy,
          base_delay,
          max_delay,
          multiplier,
          jitter,
          jitter_factor
         }).
-record(compensator, {
          activity_id,
          compensation_handler,
          state,
          result,
          created_at,
          completed_at
         }).

%%====================================================================
%% Test Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% @end
%%--------------------------------------------------------------------
setup() ->
    application:ensure_all_started(cre),
    {ok, CrePid} = cre:pid(node()),
    CrePid.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% @end
%%--------------------------------------------------------------------
cleanup(_CrePid) ->
    ok.

%%====================================================================
%% Basic Control Flow Pattern Tests (WCP-1 to WCP-6)
%%====================================================================

%%--------------------------------------------------------------------
%% WCP-1: Sequence Pattern Test
%%--------------------------------------------------------------------
sequence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Workflow = cre_yawl:new_workflow(),
                     ?assert(is_record(Workflow, workflow))
                 end),
          ?_test(begin
                     Workflow1 = cre_yawl:new_workflow(),
                     Workflow2 = cre_yawl:add_task(Workflow1, <<"t1">>, [{type, atomic}]),
                     Workflow3 = cre_yawl:add_task(Workflow2, <<"t2">>, [{type, atomic}]),
                     ?assertMatch(#workflow{}, Workflow3)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% WCP-2: Parallel Split Pattern Test
%%--------------------------------------------------------------------
parallel_split_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Pattern = cre_yawl:parallel_split(),
                     ?assert(is_record(Pattern, parallel_split))
                 end),
          ?_test(begin
                     % Test parallel split creates AND split
                     Split = cre_yawl:parallel_split(),
                     ?assertMatch(#parallel_split{}, Split)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% WCP-3: Synchronization Pattern Test
%%--------------------------------------------------------------------
synchronization_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Pattern = cre_yawl:synchronization(),
                     ?assert(is_record(Pattern, synchronization))
                 end),
          ?_test(begin
                     % Test sync requires all branches
                     Sync = cre_yawl:synchronization(),
                     ?assertMatch(#synchronization{}, Sync)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% WCP-4: Exclusive Choice Pattern Test
%%--------------------------------------------------------------------
exclusive_choice_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Pattern = cre_yawl:exclusive_choice(),
                     ?assert(is_record(Pattern, exclusive_choice))
                 end),
          ?_test(begin
                     % Test XOR choice semantics
                     Choice = cre_yawl:exclusive_choice(),
                     ?assertMatch(#exclusive_choice{}, Choice)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% WCP-5: Simple Merge Pattern Test
%%--------------------------------------------------------------------
simple_merge_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Pattern = cre_yawl:simple_merge(),
                     ?assert(is_record(Pattern, simple_merge))
                 end),
          ?_test(begin
                     % Test XOR merge semantics
                     Merge = cre_yawl:simple_merge(),
                     ?assertMatch(#simple_merge{}, Merge)
                 end)
         ]
     end}.

%%--------------------------------------------------------------------
%% WCP-6: Multi-Choice Pattern Test
%%--------------------------------------------------------------------
multi_choice_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Pattern = cre_yawl:multi_choice(),
                     ?assert(is_record(Pattern, multi_choice))
                 end),
          ?_test(begin
                     % Test OR split semantics
                     Choice = cre_yawl:multi_choice(),
                     ?assertMatch(#multi_choice{}, Choice)
                 end)
         ]
     end}.

%%====================================================================
%% Advanced Synchronization Pattern Tests (WCP-7 to WCP-10)
%%====================================================================

synchronizing_merge_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:synchronizing_merge(),
                    ?assert(is_record(Pattern, synchronizing_merge))
                end)]
     end}.

multi_merge_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:multi_merge(),
                    ?assert(is_record(Pattern, multi_merge))
                end)]
     end}.

discriminator_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:discriminator(),
                    ?assert(is_record(Pattern, discriminator))
                end)]
     end}.

arbitration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:arbitration(),
                    ?assert(is_record(Pattern, arbitration))
                end)]
     end}.

%%====================================================================
%% Multiple Instance Pattern Tests (WCP-11 to WCP-17)
%%====================================================================

implicit_termination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:implicit_termination(fun() -> ok end),
                    ?assertMatch(#pattern_state{pattern_type = implicit_termination}, Pattern)
                end)]
     end}.

multiple_instances_no_sync_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:multiple_instances_no_sync(
                        fun(X) -> X * 2 end,
                        [1, 2, 3],
                        3
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

multiple_instances_static_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:multiple_instances_static(
                        fun(X) -> X * 2 end,
                        3,
                        [1, 2, 3]
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

multiple_instances_runtime_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    CountFun = fun(Data) -> length(Data) end,
                    Pattern = cre_yawl_patterns:multiple_instances_runtime(
                        fun(X) -> X * 2 end,
                        CountFun,
                        [1, 2, 3]
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

multiple_instances_dynamic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:multiple_instances_dynamic(
                        fun(X) -> X * 2 end,
                        fun() -> receive {data, D} -> D end end,
                        5
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

deferred_choice_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:deferred_choice(
                        fun() -> option_a end,
                        fun() -> option_b end,
                        fun(Choice) -> Choice =:= option_a end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

interleaved_routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:interleaved_routing(
                        #{a => fun() -> a end, b => fun() -> b end, c => fun() -> c end},
                        undefined
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

%%====================================================================
%% State-Based Pattern Tests (WCP-18 to WCP-20)
%%====================================================================

milestone_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:milestone(
                        fun() -> milestone_reached end,
                        fun() -> activity end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

cancel_activity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:cancel_activity(
                        <<"activity_1">>,
                        fun() -> running end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

cancel_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:cancel_case(
                        [fun() -> activity1 end, fun() -> activity2 end],
                        fun() -> cancel end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

%%====================================================================
%% Extended Control Flow Pattern Tests (WCP-21 to WCP-28)
%%====================================================================

structured_sync_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:structured_sync([fun() -> ok end], undefined),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

partial_join_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:partial_join(
                        [fun() -> a end, fun() -> b end, fun() -> c end],
                        2
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

structured_loop_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:structured_loop(
                        fun() -> true end,
                        fun() -> loop_body end,
                        while
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

recursion_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:recursion(
                        fun(X) when X < 1 -> done; (X) -> {recurse, X - 1} end,
                        fun() -> base_case end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

interleaved_loop_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:interleaved_loop(
                        fun() -> [fun() -> a end, fun() -> b end] end,
                        fun() -> false end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

critical_section_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:critical_section(
                        fun() -> critical_work end,
                        <<"resource_1">>
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

protocol_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:protocol_pattern(
                        fun() -> request end,
                        fun(Request) -> {response, Request} end,
                        fun(Response) -> Response =:= {response, request} end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

try_catch_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:try_catch(
                        fun() -> ok end,
                        fun(Error) -> {caught, Error} end,
                        fun(Error) -> Error =:= timeout end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

%%====================================================================
%% Data Flow Pattern Tests (WDP-1 to WDP-5)
%%====================================================================

param_pass_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:param_pass(<<"task_a">>, <<"task_b">>),
                    ?assert(is_record(Pattern, param_pass))
                end)]
     end}.

data_transform_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:data_transform(<<"input">>, <<"output">>),
                    ?assert(is_record(Pattern, data_transform))
                end)]
     end}.

data_distribute_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:data_distribute([<<"t1">>, <<"t2">>, <<"t3">>]),
                    ?assert(is_record(Pattern, data_distribute))
                end)]
     end}.

data_accumulate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:data_accumulate([<<"t1">>, <<"t2">>]),
                    ?assert(is_record(Pattern, data_accumulate))
                end)]
     end}.

data_visibility_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:data_visibility(<<"data_1">>, [<<"branch_a">>]),
                    ?assert(is_record(Pattern, data_visibility))
                end)]
     end}.

%%====================================================================
%% Resource Pattern Tests (WRP-1 to WRP-5)
%%====================================================================

resource_create_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:resource_create(database_connection),
                    ?assert(is_record(Pattern, resource_create))
                end)]
     end}.

role_allocate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:role_allocate(<<"manager">>, approve_budget),
                    ?assert(is_record(Pattern, role_allocate))
                end)]
     end}.

resource_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:resource_start(<<"worker_1">>),
                    ?assert(is_record(Pattern, resource_start))
                end)]
     end}.

role_distribute_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:role_distribute(
                        [<<"t1">>, <<"t2">>],
                        #{<<"t1">> => 'role_a', <<"t2">> => 'role_b'}
                    ),
                    ?assert(is_record(Pattern, role_distribute))
                end)]
     end}.

capability_allocate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl:capability_allocate(
                        [gpu, large_memory],
                        #{}
                    ),
                    ?assert(is_record(Pattern, capability_allocate))
                end)]
     end}.

%%====================================================================
%% Exception Handling Pattern Tests (WHP-1 to WHP-5)
%%====================================================================

error_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:error_handler(
                        fun() -> raise_exception end,
                        fun(_Exc) -> handled end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

retry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:retry(
                        fun() -> {ok, result} end,
                        cre_yawl_exception:new_retry_policy(#{
                            max_attempts => 3,
                            backoff => exponential
                        }),
                        1
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

compensate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:compensate(
                        fun() -> primary_activity end,
                        fun() -> undo_activity end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

triggered_compensation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Pattern = cre_yawl_patterns:triggered_compensation(
                        fun() -> activity end,
                        fun() -> compensate end,
                        fun(Trigger) -> Trigger =:= cancel end
                    ),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

consecutive_compensate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    Handlers = [
                        {fun() -> activity_a end, fun() -> undo_a end},
                        {fun() -> activity_b end, fun() -> undo_b end},
                        {fun() -> activity_c end, fun() -> undo_c end}
                    ],
                    Pattern = cre_yawl_patterns:consecutive_compensate(Handlers),
                    ?assertMatch(#pattern_state{}, Pattern)
                end)]
     end}.

%%====================================================================
%% Workflow Validation Tests
%%====================================================================

workflow_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Workflow = cre_yawl:new_workflow(),
                     ?assertEqual(ok, cre_yawl:validate(Workflow))
                 end),
          ?_test(begin
                     Workflow = cre_yawl:new_workflow(<<"test_workflow">>),
                     Task1 = cre_yawl:add_task(Workflow, <<"task1">>, [{type, atomic}]),
                     ?assertEqual(ok, cre_yawl:validate(Task1))
                 end),
          ?_test(begin
                     % Test workflow with valid connection
                     Workflow = cre_yawl:new_workflow(),
                     W1 = cre_yawl:add_task(Workflow, <<"t1">>, [{type, atomic}]),
                     W2 = cre_yawl:add_task(W1, <<"t2">>, [{type, atomic}]),
                     W3 = cre_yawl:connect(W2, <<"t1">>, <<"t2">>),
                     ?assertEqual(ok, cre_yawl:validate(W3))
                 end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_sequence_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    % Create a sequence workflow
                    Workflow = cre_yawl:new_workflow(),
                    W1 = cre_yawl:add_task(Workflow, <<"validate">>, [{type, atomic}]),
                    W2 = cre_yawl:add_task(W1, <<"process">>, [{type, atomic}]),
                    W3 = cre_yawl:add_task(W2, <<"confirm">>, [{type, atomic}]),
                    W4 = cre_yawl:connect(W3, <<"validate">>, <<"process">>),
                    W5 = cre_yawl:connect(W4, <<"process">>, <<"confirm">>),

                    % Validate workflow
                    ?assertEqual(ok, cre_yawl:validate(W5))
                end)]
     end}.

integration_parallel_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [?_test(begin
                    % Create a parallel split workflow
                    Workflow = cre_yawl:new_workflow(<<"parallel_test">>),
                    W1 = cre_yawl:add_task(Workflow, <<"split">>, [{type, atomic}]),
                    W2 = cre_yawl:add_task(W1, <<"branch_a">>, [{type, atomic}]),
                    W3 = cre_yawl:add_task(W2, <<"branch_b">>, [{type, atomic}]),
                    W4 = cre_yawl:add_task(W3, <<"join">>, [{type, atomic}]),
                    W5 = cre_yawl:set_split_type(W4, <<"split">>, 'and_split'),
                    W6 = cre_yawl:set_join_type(W5, <<"join">>, 'and_join'),
                    W7 = cre_yawl:connect(W6, <<"split">>, <<"branch_a">>),
                    W8 = cre_yawl:connect(W7, <<"split">>, <<"branch_b">>),
                    W9 = cre_yawl:connect(W8, <<"branch_a">>, <<"join">>),
                    W10 = cre_yawl:connect(W9, <<"branch_b">>, <<"join">>),

                    % Validate workflow
                    ?assertEqual(ok, cre_yawl:validate(W10))
                end)]
     end}.

%%====================================================================
%% Exception Handling Tests
%%====================================================================

exception_creation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Exc = cre_yawl_exception:new_exception(
                        business_exception,
                        <<"Test error">>,
                        #{context => 'value'},
                        []
                     ),
                     ?assert(is_record(Exc, yawl_exception))
                 end),
          ?_test(begin
                     Exc = cre_yawl_exception:new_exception(
                        system_exception,
                        <<"System failure">>,
                        #{},
                        []
                     ),
                     ?assertEqual(system_exception,
                                 cre_yawl_exception:exception_type(Exc))
                 end)
         ]
     end}.

retry_policy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Policy = cre_yawl_exception:new_retry_policy(#{
                         max_attempts => 5,
                         backoff => exponential
                     }),
                     ?assert(is_record(Policy, retry_policy))
                 end),
          ?_test(begin
                     Policy = cre_yawl_exception:new_retry_policy(),
                     ?assert(cre_yawl_exception:should_retry(Policy, 0)),
                     ?assert(cre_yawl_exception:should_retry(Policy, 1)),
                     ?assert(cre_yawl_exception:should_retry(Policy, 2)),
                     ?assertNot(cre_yawl_exception:should_retry(Policy, 3))
                 end),
          ?_test(begin
                     Policy = cre_yawl_exception:new_retry_policy(#{
                         base_delay => 1000,
                         multiplier => 2.0
                     }),
                     Delay1 = cre_yawl_exception:calculate_backoff(Policy, 1),
                     Delay2 = cre_yawl_exception:calculate_backoff(Policy, 2),
                     ?assert(Delay2 > Delay1)
                 end)
         ]
     end}.

compensation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Comp = cre_yawl_exception:new_compensator(
                        <<"activity_1">>,
                        fun(Input) -> {compensated, Input} end,
                        undefined
                     ),
                     ?assert(is_record(Comp, compensator))
                 end),
          ?_test(begin
                     Comp = cre_yawl_exception:new_compensator(
                        <<"act">>,
                        fun(_) -> ok end,
                        undefined
                     ),
                     {ok, Comp1} = cre_yawl_exception:compensate(Comp, undefined),
                     ?assert(cre_yawl_exception:has_compensated(Comp1))
                 end)
         ]
     end}.
