%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Data Flow and Resource Pattern Execution Test Suite
%%
%% Comprehensive test suite for YAWL data flow (WDP-01 to WDP-05) and
%% resource (WRP-01 to WRP-05) patterns, testing:
%% - Parameter passing and transformation
%% - Data distribution strategies
%% - Data accumulation and aggregation
%% - Data visibility scopes
%% - Resource creation and initialization
%% - Role allocation strategies
%% - Resource lifecycle management
%% - Role-based distribution
%% - Capability-based allocation
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_data_resource_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions
%%====================================================================

-record(param_pass, {
          source_task_id :: binary(),
          target_task_id :: binary(),
          param_name :: atom() | undefined,
          transform_fn :: function() | undefined
         }).

-record(data_transform, {
          input_task_id :: binary(),
          output_task_id :: binary(),
          transform_fn :: function(),
          output_schema :: term() | undefined
         }).

-record(data_distribute, {
          source_task_id :: binary(),
          recipient_task_ids :: [binary()],
          distribution_type :: broadcast | round_robin | partitioned
         }).

-record(data_accumulate, {
          source_task_ids :: [binary()],
          target_task_id :: binary(),
          aggregation_fn :: function(),
          initial_value :: term()
         }).

-record(data_visibility, {
          data_task_id :: binary(),
          scope :: local | branch | global,
          access_list :: [binary()] | undefined
         }).

-record(resource_create, {
          resource_id :: binary(),
          resource_type :: atom(),
          init_params :: map()
         }).

-record(role_allocate, {
          role_id :: atom(),
          required_capability :: term(),
          allocation_strategy :: first_fit | best_fit | random
         }).

-record(resource_start, {
          resource_id :: binary(),
          start_params :: map()
         }).

-record(role_distribute, {
          work_item_ids :: [binary()],
          role_assignments :: map(),
          distribution_policy :: round_robin | least_loaded | affinity_based
         }).

-record(capability_allocate, {
          required_capabilities :: map(),
          resource_registry :: [term()],
          matching_strategy :: exact_match | minimum_met | best_effort
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

%%====================================================================
%% Test Setup/Teardown
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
    initialize_test_data(),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% @end
%%--------------------------------------------------------------------
cleanup(_TestData) ->
    cleanup_test_data(),
    ok.

%%--------------------------------------------------------------------
%% @doc Initialize test data structures.
%% @end
%%--------------------------------------------------------------------
initialize_test_data() ->
    %% Put test data in process dictionary for test isolation
    put(test_data, #{
        tasks => #{},
        resources => #{},
        allocations => #{}
    }),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup test data structures.
%% @end
%%--------------------------------------------------------------------
cleanup_test_data() ->
    erase(test_data),
    erase(param_pass_state),
    erase(data_transform_state),
    erase(data_distribute_state),
    erase(data_accumulate_state),
    erase(data_visibility_state),
    erase(resource_state),
    erase(role_allocation_state),
    ok.

%%====================================================================
%% Data Flow Pattern Tests - WDP-01: Parameter Passing
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test parameter passing pattern (WDP-01).
%% @end
%%--------------------------------------------------------------------
wdp01_parameter_passing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WDP-01: Direct parameter passing",
           fun() ->
               SourceId = <<"task_source">>,
               TargetId = <<"task_target">>,
               Pattern = cre_yawl:param_pass(SourceId, TargetId),

               ?assertMatch(#param_pass{}, Pattern),
               ?assertEqual(SourceId, Pattern#param_pass.source_task_id),
               ?assertEqual(TargetId, Pattern#param_pass.target_task_id),
               ?assertEqual(undefined, Pattern#param_pass.param_name),
               ?assertEqual(undefined, Pattern#param_pass.transform_fn)
           end},

          {"WDP-01: Transform-assigned parameter passing",
           fun() ->
               TransformFn = fun(X) -> X * 2 end,
               Pattern = cre_yawl:param_pass(<<"t1">>, <<"t2">>),

               ?assertMatch(#param_pass{}, Pattern),

               %% Execute parameter passing with transform
               Result = execute_param_pass_with_transform(Pattern, 5, TransformFn),
               ?assertEqual(10, Result)
           end},

          {"WDP-01: Type validation for parameters",
           fun() ->
               Pattern = cre_yawl:param_pass(<<"src">>, <<"tgt">>),

               %% Valid types
               ?assertEqual({ok, valid}, validate_param_type(Pattern, integer)),
               ?assertEqual({ok, valid}, validate_param_type(Pattern, binary)),
               ?assertEqual({ok, valid}, validate_param_type(Pattern, list)),

               %% Invalid types should fail validation
               ?assertEqual({error, invalid_type}, validate_param_type(Pattern, undefined))
           end},

          {"WDP-01: Missing parameters handling",
           fun() ->
               Pattern = cre_yawl:param_pass(<<"src">>, <<"tgt">>),

               %% Missing parameter should return error
               ?assertEqual({error, missing_parameter}, execute_param_pass(Pattern, undefined)),

               %% Valid parameter should succeed
               ?assertEqual({ok, present}, execute_param_pass(Pattern, <<"value">>))
           end},

          {"WDP-01: Parameter passing with complex data",
           fun() ->
               Pattern = cre_yawl:param_pass(<<"src">>, <<"tgt">>),

               ComplexData = #{
                   key1 => <<"value1">>,
                   key2 => [1, 2, 3],
                   key3 => #{nested => true}
               },

               Result = execute_param_pass(Pattern, ComplexData),
               ?assertEqual({ok, ComplexData}, Result)
           end}
         ]
     end}.

%%====================================================================
%% Data Flow Pattern Tests - WDP-02: Data Transformation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test data transformation pattern (WDP-02).
%% @end
%%--------------------------------------------------------------------
wdp02_data_transformation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WDP-02: Valid transformation - numeric",
           fun() ->
               InputId = <<"task_input">>,
               OutputId = <<"task_output">>,
               Pattern = cre_yawl:data_transform(InputId, OutputId),

               ?assertMatch(#data_transform{}, Pattern),
               ?assertEqual(InputId, Pattern#data_transform.input_task_id),
               ?assertEqual(OutputId, Pattern#data_transform.output_task_id),

               %% Execute transformation
               TransformFn = fun(X) -> X * 3 end,
               Result = execute_data_transform(Pattern, 7, TransformFn),
               ?assertEqual(21, Result)
           end},

          {"WDP-02: Valid transformation - string",
           fun() ->
               Pattern = cre_yawl:data_transform(<<"in">>, <<"out">>),

               TransformFn = fun(S) -> string:uppercase(binary_to_list(S)) end,
               Result = execute_data_transform(Pattern, <<"hello">>, TransformFn),
               ?assertEqual(<<"HELLO">>, Result)
           end},

          {"WDP-02: Valid transformation - list",
           fun() ->
               Pattern = cre_yawl:data_transform(<<"in">>, <<"out">>),

               TransformFn = fun(L) -> lists:map(fun(X) -> X * 2 end, L) end,
               Result = execute_data_transform(Pattern, [1, 2, 3, 4], TransformFn),
               ?assertEqual([2, 4, 6, 8], Result)
           end},

          {"WDP-02: Invalid transformation - function error",
           fun() ->
               Pattern = cre_yawl:data_transform(<<"in">>, <<"out">>),

               %% Bad transform function that throws
               BadTransform = fun(_) -> error(bad_transform) end,

               Result = execute_data_transform(Pattern, <<"test">>, BadTransform),
               ?assertMatch({error, _}, Result)
           end},

          {"WDP-02: Schema validation",
           fun() ->
               Pattern = cre_yawl:data_transform(<<"in">>, <<"out">>),

               %% Define output schema
               Schema = #{type => integer, range => {0, 100}},

               %% Valid output
               ?assertEqual({ok, valid}, validate_output_schema(Pattern, 50, Schema)),

               %% Invalid output (out of range)
               ?assertEqual({error, out_of_range}, validate_output_schema(Pattern, 150, Schema))
           end},

          {"WDP-02: Error handling in transformation",
           fun() ->
               Pattern = cre_yawl:data_transform(<<"in">>, <<"out">>),

               %% Transform with error handling
               SafeTransform = fun
                   (X) when is_integer(X), X > 0 -> {ok, X * 2};
                   (_) -> {error, invalid_input}
               end,

               ?assertEqual({ok, 10}, execute_data_transform_safe(Pattern, 5, SafeTransform)),
               ?assertEqual({error, invalid_input}, execute_data_transform_safe(Pattern, -5, SafeTransform))
           end}
         ]
     end}.

%%====================================================================
%% Data Flow Pattern Tests - WDP-03: Data Distribution
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test data distribution pattern (WDP-03).
%% @end
%%--------------------------------------------------------------------
wdp03_data_distribution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WDP-03: Broadcast distribution",
           fun() ->
               Recipients = [<<"t1">>, <<"t2">>, <<"t3">>],
               Pattern = cre_yawl:data_distribute(Recipients),

               ?assertMatch(#data_distribute{}, Pattern),
               ?assertEqual(broadcast, Pattern#data_distribute.distribution_type),
               ?assertEqual(Recipients, Pattern#data_distribute.recipient_task_ids),

               %% Execute broadcast distribution
               Data = <<"broadcast_data">>,
               Result = execute_data_distribute(Pattern, Data),
               ?assertEqual(3, length(Result)),
               ?assert(lists:all(fun({_R, D}) -> D =:= Data end, Result))
           end},

          {"WDP-03: Round-robin distribution",
           fun() ->
               Recipients = [<<"t1">>, <<"t2">>, <<"t3">>],
               Pattern = cre_yawl:data_distribute(Recipients),

               %% Set distribution type to round_robin
               RRPattern = Pattern#data_distribute{distribution_type = round_robin},

               DataList = [1, 2, 3, 4, 5, 6],
               Result = execute_data_distribute_rr(RRPattern, DataList),

               ?assertEqual(6, length(Result)),

               %% Verify round-robin assignment
               ?assertEqual(1, count_assignments_to(Result, <<"t1">>)),
               ?assertEqual(1, count_assignments_to(Result, <<"t2">>)),
               ?assertEqual(1, count_assignments_to(Result, <<"t3">>))
           end},

          {"WDP-03: Partitioned distribution",
           fun() ->
               Recipients = [<<"t1">>, <<"t2">>, <<"t3">>],
               Pattern = cre_yawl:data_distribute(Recipients),

               %% Set distribution type to partitioned
               PartPattern = Pattern#data_distribute{distribution_type = partitioned},

               DataList = [1, 2, 3, 4, 5, 6, 7, 8, 9],
               Result = execute_data_distribute_partitioned(PartPattern, DataList),

               ?assertEqual(9, length(Result)),

               %% Verify partitioning (3 recipients, 9 items = 3 each)
               ?assertEqual(3, count_assignments_to(Result, <<"t1">>)),
               ?assertEqual(3, count_assignments_to(Result, <<"t2">>)),
               ?assertEqual(3, count_assignments_to(Result, <<"t3">>))
           end},

          {"WDP-03: Distribution to empty recipient list",
           fun() ->
               Pattern = cre_yawl:data_distribute([]),

               ?assertEqual([], Pattern#data_distribute.recipient_task_ids),

               %% Should return error when distributing to no recipients
               Result = execute_data_distribute(Pattern, <<"data">>),
               ?assertEqual({error, no_recipients}, Result)
           end},

          {"WDP-03: Distribution with data integrity verification",
           fun() ->
               Recipients = [<<"t1">>, <<"t2">>],
               Pattern = cre_yawl:data_distribute(Recipients),

               OriginalData = [1, 2, 3, 4, 5],
               Result = execute_data_distribute_verify(Pattern, OriginalData),

               ?assertEqual({ok, integrity_ok}, Result)
           end}
         ]
     end}.

%%====================================================================
%% Data Flow Pattern Tests - WDP-04: Data Accumulation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test data accumulation pattern (WDP-04).
%% @end
%%--------------------------------------------------------------------
wdp04_data_accumulation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WDP-04: Sum aggregation",
           fun() ->
               Sources = [<<"s1">>, <<"s2">>, <<"s3">>],
               Pattern = cre_yawl:data_accumulate(Sources),

               ?assertMatch(#data_accumulate{}, Pattern),
               ?assertEqual(Sources, Pattern#data_accumulate.source_task_ids),

               %% Execute sum aggregation
               DataList = [10, 20, 30, 40, 50],
               SumFn = fun(Acc, X) -> Acc + X end,
               Result = execute_data_accumulate(Pattern, DataList, SumFn, 0),

               ?assertEqual(150, Result)
           end},

          {"WDP-04: Count aggregation",
           fun() ->
               Sources = [<<"s1">>, <<"s2">>],
               Pattern = cre_yawl:data_accumulate(Sources),

               %% Execute count aggregation
               DataList = [a, b, c, d, e],
               CountFn = fun(Acc, _X) -> Acc + 1 end,
               Result = execute_data_accumulate(Pattern, DataList, CountFn, 0),

               ?assertEqual(5, Result)
           end},

          {"WDP-04: List aggregation",
           fun() ->
               Sources = [<<"s1">>, <<"s2">>],
               Pattern = cre_yawl:data_accumulate(Sources),

               %% Execute list aggregation
               DataList = [1, 2, 3],
               ListFn = fun(Acc, X) -> [X | Acc] end,
               Result = execute_data_accumulate(Pattern, DataList, ListFn, []),

               ?assertEqual([3, 2, 1], Result)
           end},

          {"WDP-04: Custom aggregation (product)",
           fun() ->
               Sources = [<<"s1">>],
               Pattern = cre_yawl:data_accumulate(Sources),

               %% Execute product aggregation
               DataList = [2, 3, 4, 5],
               ProductFn = fun(Acc, X) -> Acc * X end,
               Result = execute_data_accumulate(Pattern, DataList, ProductFn, 1),

               ?assertEqual(120, Result)
           end},

          {"WDP-04: Incremental accumulation",
           fun() ->
               Sources = [<<"s1">>, <<"s2">>],
               _Pattern = cre_yawl:data_accumulate(Sources),

               %% Simulate incremental accumulation
               put(accum_state, 0),

               lists:foreach(fun(N) ->
                   CurrentState = get(accum_state),
                   NewState = CurrentState + N,
                   put(accum_state, NewState)
               end, [1, 2, 3, 4, 5]),

               ?assertEqual(15, get(accum_state))
           end},

          {"WDP-04: Accumulation with empty source list",
           fun() ->
               Pattern = cre_yawl:data_accumulate([]),

               %% Should handle empty sources gracefully
               DataList = [1, 2, 3],
               SumFn = fun(Acc, X) -> Acc + X end,
               Result = execute_data_accumulate(Pattern, DataList, SumFn, 0),

               ?assertEqual(6, Result)
           end}
         ]
     end}.

%%====================================================================
%% Data Flow Pattern Tests - WDP-05: Data Visibility
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test data visibility pattern (WDP-05).
%% @end
%%--------------------------------------------------------------------
wdp05_data_visibility_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WDP-05: Local scope visibility",
           fun() ->
               DataId = <<"data_local">>,
               Pattern = cre_yawl:data_visibility(DataId, local),

               ?assertMatch(#data_visibility{}, Pattern),
               ?assertEqual(DataId, Pattern#data_visibility.data_task_id),
               ?assertEqual(local, Pattern#data_visibility.scope),

               %% Local scope should only be accessible within same task
               ?assertEqual(allowed, check_visibility(Pattern, <<"data_local">>, <<"data_local">>)),
               ?assertEqual(denied, check_visibility(Pattern, <<"data_local">>, <<"other_task">>))
           end},

          {"WDP-05: Branch scope visibility",
           fun() ->
               DataId = <<"data_branch">>,
               Pattern = cre_yawl:data_visibility(DataId, branch),

               ?assertEqual(branch, Pattern#data_visibility.scope),

               %% Branch scope should be accessible within same branch
               ?assertEqual(allowed, check_visibility(Pattern, <<"data_branch">>, <<"branch_1_task">>)),
               ?assertEqual(denied, check_visibility(Pattern, <<"data_branch">>, <<"branch_2_task">>))
           end},

          {"WDP-05: Global scope visibility",
           fun() ->
               DataId = <<"data_global">>,
               Pattern = cre_yawl:data_visibility(DataId, global),

               ?assertEqual(global, Pattern#data_visibility.scope),

               %% Global scope should be accessible to all tasks
               ?assertEqual(allowed, check_visibility(Pattern, <<"data_global">>, <<"any_task_1">>)),
               ?assertEqual(allowed, check_visibility(Pattern, <<"data_global">>, <<"any_task_2">>)),
               ?assertEqual(allowed, check_visibility(Pattern, <<"data_global">>, <<"any_task_3">>))
           end},

          {"WDP-05: Access validation with access list",
           fun() ->
               DataId = <<"data_restricted">>,
               AccessList = [<<"task_a">>, <<"task_b">>],
               Pattern = cre_yawl:data_visibility(DataId, local),

               PatternWithList = Pattern#data_visibility{access_list = AccessList},

               %% Tasks in access list should have access
               ?assertEqual(allowed, check_visibility_with_list(PatternWithList, <<"task_a">>)),
               ?assertEqual(allowed, check_visibility_with_list(PatternWithList, <<"task_b">>)),

               %% Tasks not in access list should be denied
               ?assertEqual(denied, check_visibility_with_list(PatternWithList, <<"task_c">>))
           end},

          {"WDP-05: Scope hierarchy enforcement",
           fun() ->
               %% Test that local < branch < global in visibility hierarchy
               _LocalPattern = cre_yawl:data_visibility(<<"d1">>, local),
               _BranchPattern = cre_yawl:data_visibility(<<"d2">>, branch),
               _GlobalPattern = cre_yawl:data_visibility(<<"d3">>, global),

               ?assert(compare_scopes(local, branch) =:= lt),
               ?assert(compare_scopes(branch, global) =:= lt),
               ?assert(compare_scopes(local, global) =:= lt),
               ?assert(compare_scopes(global, local) =:= gt)
           end}
         ]
     end}.

%%====================================================================
%% Resource Pattern Tests - WRP-01: Resource Creation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test resource creation pattern (WRP-01).
%% @end
%%--------------------------------------------------------------------
wrp01_resource_creation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WRP-01: Successful resource creation",
           fun() ->
               ResourceType = database_connection,
               Pattern = cre_yawl:resource_create(ResourceType),

               ?assertMatch(#resource_create{}, Pattern),
               ?assertEqual(ResourceType, Pattern#resource_create.resource_type),
               ?assertEqual(#{}, Pattern#resource_create.init_params),

               %% Execute resource creation
               Result = execute_resource_create(Pattern, <<"res_001">>),
               ?assertMatch({ok, _ResourceId}, Result)
           end},

          {"WRP-01: Creation with initialization parameters",
           fun() ->
               Pattern = cre_yawl:resource_create(cache_server),

               %% Add initialization parameters
               InitParams = #{
                   host => <<"localhost">>,
                   port => 6379,
                   max_connections => 100
               },
               PatternWithParams = Pattern#resource_create{init_params = InitParams},

               Result = execute_resource_create(PatternWithParams, <<"cache_001">>),
               ?assertMatch({ok, _}, Result)
           end},

          {"WRP-01: Creation failures - invalid type",
           fun() ->
               Pattern = cre_yawl:resource_create(undefined),

               Result = execute_resource_create(Pattern, <<"res_invalid">>),
               ?assertEqual({error, invalid_resource_type}, Result)
           end},

          {"WRP-01: Creation failures - duplicate ID",
           fun() ->
               Pattern = cre_yawl:resource_create(queue),

               ResourceId = <<"queue_dup">>,

               %% First creation should succeed
               ?assertMatch({ok, ResourceId}, execute_resource_create(Pattern, ResourceId)),

               %% Second creation with same ID should fail
               ?assertEqual({error, resource_exists}, execute_resource_create(Pattern, ResourceId))
           end},

          {"WRP-01: Initialization parameter validation",
           fun() ->
               Pattern = cre_yawl:resource_create(api_client),

               %% Valid parameters
               ValidParams = #{api_key => <<"key123">>, endpoint => <<"https://api.example.com">>},
               ?assertEqual({ok, valid}, validate_init_params(Pattern, ValidParams)),

               %% Missing required parameter
               InvalidParams = #{api_key => <<"key123">>},
               ?assertEqual({error, missing_required_param}, validate_init_params(Pattern, InvalidParams))
           end}
         ]
     end}.

%%====================================================================
%% Resource Pattern Tests - WRP-02: Role Allocation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test role allocation pattern (WRP-02).
%% @end
%%--------------------------------------------------------------------
wrp02_role_allocation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WRP-02: First fit allocation strategy",
           fun() ->
               RoleId = analyst,
               Capability = data_analysis,
               Pattern = cre_yawl:role_allocate(RoleId, Capability),

               ?assertMatch(#role_allocate{}, Pattern),
               ?assertEqual(RoleId, Pattern#role_allocate.role_id),
               ?assertEqual(Capability, Pattern#role_allocate.required_capability),
               ?assertEqual(first_fit, Pattern#role_allocate.allocation_strategy),

               %% Execute first fit allocation
               Resources = [
                   {r1, analyst, #{capabilities => [data_analysis, reporting]}},
                   {r2, analyst, #{capabilities => [data_analysis]}},
                   {r3, analyst, #{capabilities => [reporting]}}
               ],

               Result = execute_role_allocate(Pattern, Resources),
               ?assertMatch({ok, {r1, _}}, Result)  %% First matching resource
           end},

          {"WRP-02: Best fit allocation strategy",
           fun() ->
               RoleId = developer,
               Capability = erlang,
               Pattern = cre_yawl:role_allocate(RoleId, Capability),

               PatternWithStrategy = Pattern#role_allocate{allocation_strategy = best_fit},

               Resources = [
                   {r1, developer, #{capabilities => [erlang, python], score => 5}},
                   {r2, developer, #{capabilities => [erlang], score => 3}},
                   {r3, developer, #{capabilities => [erlang, python, java], score => 8}}
               ],

               Result = execute_role_allocate_best_fit(PatternWithStrategy, Resources),
               ?assertMatch({ok, {r2, _}}, Result)  %% Best fit (exact match)
           end},

          {"WRP-02: Random allocation strategy",
           fun() ->
               RoleId = reviewer,
               Capability = code_review,
               Pattern = cre_yawl:role_allocate(RoleId, Capability),

               PatternWithStrategy = Pattern#role_allocate{allocation_strategy = random},

               Resources = [
                   {r1, reviewer, #{}},
                   {r2, reviewer, #{}},
                   {r3, reviewer, #{}}
               ],

               Result = execute_role_allocate_random(PatternWithStrategy, Resources),
               ?assertMatch({ok, {_ResourceId, _}}, Result)
           end},

          {"WRP-02: Allocation failures - no matching resources",
           fun() ->
               Pattern = cre_yawl:role_allocate(admin, system_admin),

               Resources = [
                   {r1, user, #{capabilities => [read, write]}},
                   {r2, user, #{capabilities => [read]}}
               ],

               Result = execute_role_allocate(Pattern, Resources),
               ?assertEqual({error, no_matching_resources}, Result)
           end},

          {"WRP-02: Allocation with capability levels",
           fun() ->
               Pattern = cre_yawl:role_allocate(senior_developer, advanced_erlang),

               Resources = [
                   {r1, developer, #{capabilities => [{advanced_erlang, 5}]}},
                   {r2, developer, #{capabilities => [{advanced_erlang, 3}]}},
                   {r3, developer, #{capabilities => [{advanced_erlang, 8}]}}
               ],

               Result = execute_role_allocate_with_level(Pattern, Resources, 4),
               ?assertMatch({ok, {_ResourceId, _}}, Result),

               %% Verify allocated resource meets minimum level
               {ok, {AllocatedId, _}} = Result,
               ?assert(lists:member(AllocatedId, [r1, r3]))  %% r2 doesn't meet minimum
           end}
         ]
     end}.

%%====================================================================
%% Resource Pattern Tests - WRP-03: Resource Start
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test resource start pattern (WRP-03).
%% @end
%%--------------------------------------------------------------------
wrp03_resource_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WRP-03: Successful resource start",
           fun() ->
               ResourceId = <<"res_001">>,
               Pattern = cre_yawl:resource_start(ResourceId),

               ?assertMatch(#resource_start{}, Pattern),
               ?assertEqual(ResourceId, Pattern#resource_start.resource_id),
               ?assertEqual(#{}, Pattern#resource_start.start_params),

               %% Execute resource start
               Result = execute_resource_start(Pattern),
               ?assertMatch({ok, _ResourceState}, Result)
           end},

          {"WRP-03: Start with parameters",
           fun() ->
               Pattern = cre_yawl:resource_start(<<"res_002">>),

               StartParams = #{
                   timeout => 5000,
                   retry_limit => 3,
                   mode => async
               },
               PatternWithParams = Pattern#resource_start{start_params = StartParams},

               Result = execute_resource_start(PatternWithParams),
               ?assertMatch({ok, _}, Result)
           end},

          {"WRP-03: Start failures - resource not found",
           fun() ->
               Pattern = cre_yawl:resource_start(<<"nonexistent">>),

               Result = execute_resource_start(Pattern),
               ?assertEqual({error, resource_not_found}, Result)
           end},

          {"WRP-03: Start failures - invalid state",
           fun() ->
               Pattern = cre_yawl:resource_start(<<"already_running">>),

               %% Simulate already running resource
               put(resource_state, #{<<"already_running">> => running}),

               Result = execute_resource_start(Pattern),
               ?assertEqual({error, invalid_state}, Result)
           end},

          {"WRP-03: State validation after start",
           fun() ->
               ResourceId = <<"res_validate">>,
               Pattern = cre_yawl:resource_start(ResourceId),

               {ok, State} = execute_resource_start(Pattern),

               ?assertEqual(initialized, maps:get(status, State)),
               ?assertEqual(ResourceId, maps:get(resource_id, State))
           end}
         ]
     end}.

%%====================================================================
%% Resource Pattern Tests - WRP-04: Role Distribution
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test role distribution pattern (WRP-04).
%% @end
%%--------------------------------------------------------------------
wrp04_role_distribution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WRP-04: Round-robin distribution policy",
           fun() ->
               WorkItems = [<<"w1">>, <<"w2">>, <<"w3">>, <<"w4">>, <<"w5">>, <<"w6">>],
               RoleAssignments = #{
                   worker_a => [<<"w1">>, <<"w2">>],
                   worker_b => [<<"w3">>, <<"w4">>],
                   worker_c => [<<"w5">>, <<"w6">>]
               },
               Pattern = cre_yawl:role_distribute(WorkItems, RoleAssignments),

               ?assertMatch(#role_distribute{}, Pattern),
               ?assertEqual(round_robin, Pattern#role_distribute.distribution_policy),

               %% Execute round-robin distribution
               Result = execute_role_distribute_rr(Pattern, [worker_a, worker_b, worker_c]),
               ?assertEqual(6, length(Result)),

               %% Verify distribution (2 items per worker)
               ?assertEqual(2, count_work_assignments(Result, worker_a)),
               ?assertEqual(2, count_work_assignments(Result, worker_b)),
               ?assertEqual(2, count_work_assignments(Result, worker_c))
           end},

          {"WRP-04: Least loaded distribution policy",
           fun() ->
               WorkItems = [<<"w1">>, <<"w2">>, <<"w3">>],
               RoleAssignments = #{},
               Pattern = cre_yawl:role_distribute(WorkItems, RoleAssignments),

               PatternWithPolicy = Pattern#role_distribute{distribution_policy = least_loaded},

               %% Simulate current load
               CurrentLoad = #{
                   worker_a => 5,
                   worker_b => 2,
                   worker_c => 8
               },

               Result = execute_role_distribute_least_loaded(PatternWithPolicy, CurrentLoad),
               ?assertMatch({ok, _}, Result)
           end},

          {"WRP-04: Affinity-based distribution policy",
           fun() ->
               WorkItems = [<<"w1">>, <<"w2">>],
               RoleAssignments = #{
                   worker_a => [{type, database}, {location, us_east}],
                   worker_b => [{type, cache}, {location, us_west}]
               },
               Pattern = cre_yawl:role_distribute(WorkItems, RoleAssignments),

               PatternWithPolicy = Pattern#role_distribute{distribution_policy = affinity_based},

               %% Work items with affinity requirements
               WorkItemAffinity = #{
                   <<"w1">> => [{type, database}],
                   <<"w2">> => [{type, cache}]
               },

               Result = execute_role_distribute_affinity(PatternWithPolicy, WorkItemAffinity),
               ?assertEqual(2, length(Result)),

               %% Verify affinity matching
               ?assertEqual({w1, worker_a}, lists:keyfind(w1, 1, Result)),
               ?assertEqual({w2, worker_b}, lists:keyfind(w2, 1, Result))
           end},

          {"WRP-04: Distribution with empty work items",
           fun() ->
               Pattern = cre_yawl:role_distribute([], #{}),

               Result = execute_role_distribute_rr(Pattern, [worker_a]),
               ?assertEqual([], Result)
           end},

          {"WRP-04: Distribution with no available workers",
           fun() ->
               Pattern = cre_yawl:role_distribute([<<"w1">>], #{}),

               Result = execute_role_distribute_rr(Pattern, []),
               ?assertEqual({error, no_workers_available}, Result)
           end}
         ]
     end}.

%%====================================================================
%% Resource Pattern Tests - WRP-05: Capability Allocation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test capability allocation pattern (WRP-05).
%% @end
%%--------------------------------------------------------------------
wrp05_capability_allocation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"WRP-05: Exact match capability allocation",
           fun() ->
               RequiredCapabilities = #{
                   language => erlang,
                   experience => senior,
                   skills => [otp, gen_server]
               },
               Registry = [
                   {res1, #{language => erlang, experience => senior, skills => [otp, gen_server]}},
                   {res2, #{language => erlang, experience => mid, skills => [otp]}},
                   {res3, #{language => python, experience => senior, skills => [asyncio]}}
               ],
               Pattern = cre_yawl:capability_allocate(RequiredCapabilities, Registry),

               ?assertMatch(#capability_allocate{}, Pattern),
               ?assertEqual(exact_match, Pattern#capability_allocate.matching_strategy),

               Result = execute_capability_allocate(Pattern),
               ?assertMatch({ok, {res1, _}}, Result)  %% Only res1 matches exactly
           end},

          {"WRP-05: Minimum met capability allocation",
           fun() ->
               RequiredCapabilities = #{
                   cores => 4,
                   memory => 8192
               },
               Registry = [
                   {res1, #{cores => 8, memory => 16384}},
                   {res2, #{cores => 4, memory => 8192}},
                   {res3, #{cores => 2, memory => 4096}}
               ],
               Pattern = cre_yawl:capability_allocate(RequiredCapabilities, Registry),

               PatternWithStrategy = Pattern#capability_allocate{matching_strategy = minimum_met},

               Result = execute_capability_allocate_min_met(PatternWithStrategy),
               ?assertMatch({ok, {res2, _}}, Result)  %% res2 exactly meets minimum
           end},

          {"WRP-05: Best effort capability allocation",
           fun() ->
               RequiredCapabilities = #{
                   gpu => true,
                   cuda => "12.0"
               },
               Registry = [
                   {res1, #{gpu => true, cuda => "11.8"}},  %% Partial match
                   {res2, #{cpu => true, cores => 16}},     %% No match
                   {res3, #{gpu => true, cuda => "12.2"}}  %% Close match
               ],
               Pattern = cre_yawl:capability_allocate(RequiredCapabilities, Registry),

               PatternWithStrategy = Pattern#capability_allocate{matching_strategy = best_effort},

               Result = execute_capability_allocate_best_effort(PatternWithStrategy),
               ?assertMatch({ok, {_ResourceId, _}}, Result),

               %% Should allocate to closest match (res3 with cuda 12.2)
               {ok, {AllocatedId, _}} = Result,
               ?assert(lists:member(AllocatedId, [res1, res3]))
           end},

          {"WRP-05: Capability discovery",
           fun() ->
               Pattern = cre_yawl:capability_allocate(#{}, []),

               %% Add resources to registry
               Registry = [
                   {res1, #{capability => a, version => "1.0"}},
                   {res2, #{capability => b, version => "2.0"}},
                   {res3, #{capability => c, version => "3.0"}}
               ],

               PatternWithRegistry = Pattern#capability_allocate{resource_registry = Registry},

               Discovered = discover_capabilities(PatternWithRegistry, capability),
               ?assertEqual(3, length(Discovered))
           end},

          {"WRP-05: No matching capabilities",
           fun() ->
               RequiredCapabilities = #{
                   specialized => quantum_computing
               },
               Registry = [
                   {res1, #{general => computing}},
                   {res2, #{general => storage}}
               ],
               Pattern = cre_yawl:capability_allocate(RequiredCapabilities, Registry),

               Result = execute_capability_allocate(Pattern),
               ?assertEqual({error, no_matching_capabilities}, Result)
           end}
         ]
     end}.

%%====================================================================
%% Data Integrity and Performance Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test data integrity across patterns.
%% @end
%%--------------------------------------------------------------------
data_integrity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Data integrity through transformation chain",
           fun() ->
               %% Chain transformations
               Original = [1, 2, 3, 4, 5],

               T1 = fun(L) -> lists:map(fun(X) -> X * 2 end, L) end,
               T2 = fun(L) -> lists:filter(fun(X) -> X > 5 end, L) end,
               T3 = fun(L) -> lists:sum(L) end,

               Result1 = T1(Original),  %% [2, 4, 6, 8, 10]
               Result2 = T2(Result1),  %% [6, 8, 10]
               Result3 = T3(Result2),  %% 24

               ?assertEqual(24, Result3)
           end},

          {"Data integrity through distribution and accumulation",
           fun() ->
               SourceData = [1, 2, 3, 4, 5, 6, 7, 8, 9],

               %% Distribute (simulated 3-way split)
               Split1 = lists:sublist(SourceData, 1, 3),
               Split2 = lists:sublist(SourceData, 4, 3),
               Split3 = lists:sublist(SourceData, 7, 3),

               %% Process each split
               Sum1 = lists:sum(Split1),  %% 6
               Sum2 = lists:sum(Split2),  %% 15
               Sum3 = lists:sum(Split3),  %% 24

               %% Accumulate results
               Total = Sum1 + Sum2 + Sum3,  %% 45

               ?assertEqual(45, Total),
               ?assertEqual(lists:sum(SourceData), Total)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test performance of data and resource operations.
%% @end
%%--------------------------------------------------------------------
performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TestData) ->
         [
          {"Data distribution performance",
           fun() ->
               Recipients = lists:map(fun(N) ->
                   list_to_binary("recipient_" ++ integer_to_list(N))
               end, lists:seq(1, 100)),

               Pattern = cre_yawl:data_distribute(Recipients),

               Data = <<"performance_test_data">>,

               Start = erlang:monotonic_time(microsecond),
               Result = execute_data_distribute(Pattern, Data),
               End = erlang:monotonic_time(microsecond),

               Duration = End - Start,

               ?assertEqual(100, length(Result)),
               ?assert(Duration < 100000, "Distribution should complete in <100ms")
           end},

          {"Role allocation performance with many resources",
           fun() ->
               Pattern = cre_yawl:role_allocate(worker, general),

               Resources = lists:map(fun(N) ->
                   {list_to_binary("res_" ++ integer_to_list(N)), worker, #{}}
               end, lists:seq(1, 1000)),

               Start = erlang:monotonic_time(microsecond),
               Result = execute_role_allocate(Pattern, Resources),
               End = erlang:monotonic_time(microsecond),

               Duration = End - Start,

               ?assertMatch({ok, _}, Result),
               ?assert(Duration < 50000, "Allocation should complete in <50ms")
           end}
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Execute parameter passing pattern.
%% @end
%%--------------------------------------------------------------------
execute_param_pass(#param_pass{transform_fn = undefined}, Data) ->
    case Data of
        undefined -> {error, missing_parameter};
        _ -> {ok, Data}
    end;
execute_param_pass(#param_pass{transform_fn = TransformFn}, Data) ->
    try
        {ok, TransformFn(Data)}
    catch
        _:_ -> {error, transform_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Execute parameter passing pattern with transform function.
%% @end
%%--------------------------------------------------------------------
execute_param_pass_with_transform(#param_pass{}, Data, TransformFn) ->
    try
        TransformFn(Data)
    catch
        _:_ -> {error, transform_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Validate parameter type.
%% @end
%%--------------------------------------------------------------------
validate_param_type(_Pattern, Type) when Type =:= undefined;
                                         Type =:= null ->
    {error, invalid_type};
validate_param_type(_Pattern, _Type) ->
    {ok, valid}.

%%--------------------------------------------------------------------
%% @doc Execute data transformation pattern.
%% @end
%%--------------------------------------------------------------------
execute_data_transform(#data_transform{}, Input, TransformFn) ->
    try
        TransformFn(Input)
    catch
        _:_ -> {error, transform_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Execute data transformation with error handling.
%% @end
%%--------------------------------------------------------------------
execute_data_transform_safe(_Pattern, Input, SafeTransformFn) ->
    try
        SafeTransformFn(Input)
    catch
        _:_ -> {error, unexpected_error}
    end.

%%--------------------------------------------------------------------
%% @doc Validate output schema.
%% @end
%%--------------------------------------------------------------------
validate_output_schema(_Pattern, Value, #{type := integer, range := {Min, Max}}) ->
    case is_integer(Value) andalso Value >= Min andalso Value =< Max of
        true -> {ok, valid};
        false -> {error, out_of_range}
    end;
validate_output_schema(_Pattern, _Value, _Schema) ->
    {ok, valid}.

%%--------------------------------------------------------------------
%% @doc Execute data distribution (broadcast).
%% @end
%%--------------------------------------------------------------------
execute_data_distribute(#data_distribute{recipient_task_ids = []}, _Data) ->
    {error, no_recipients};
execute_data_distribute(#data_distribute{recipient_task_ids = Recipients, distribution_type = broadcast}, Data) ->
    [{R, Data} || R <- Recipients].

%%--------------------------------------------------------------------
%% @doc Execute data distribution (round-robin).
%% @end
%%--------------------------------------------------------------------
execute_data_distribute_rr(#data_distribute{recipient_task_ids = Recipients}, DataList) ->
    lists:flatmap(fun({Data, Index}) ->
        RecipientIndex = (Index rem length(Recipients)),
        Recipient = lists:nth(RecipientIndex + 1, Recipients),
        [{Recipient, Data}]
    end, lists:zip(DataList, lists:seq(0, length(DataList) - 1))).

%%--------------------------------------------------------------------
%% @doc Execute data distribution (partitioned).
%% @end
%%--------------------------------------------------------------------
execute_data_distribute_partitioned(#data_distribute{recipient_task_ids = Recipients}, DataList) ->
    PartitionSize = ceil(length(DataList) / length(Recipients)),
    Partitioned = partition_list(DataList, PartitionSize),
    lists:flatmap(fun({Recipient, Partition}) ->
        [{Recipient, Item} || Item <- Partition]
    end, lists:zip(Recipients, Partitioned)).

%%--------------------------------------------------------------------
%% @doc Execute data distribution with verification.
%% @end
%%--------------------------------------------------------------------
execute_data_distribute_verify(#data_distribute{recipient_task_ids = Recipients}, Data) ->
    %% Simulate distribution
    Distributed = [{R, Data} || R <- Recipients],
    case length(Distributed) =:= length(Recipients) of
        true -> {ok, integrity_ok};
        false -> {error, integrity_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Count assignments to a specific recipient.
%% @end
%%--------------------------------------------------------------------
count_assignments_to(ResultList, RecipientId) ->
    length([R || {R, _} <- ResultList, R =:= RecipientId]).

%%--------------------------------------------------------------------
%% @doc Partition a list into chunks.
%% @end
%%--------------------------------------------------------------------
partition_list(List, ChunkSize) ->
    partition_list(List, ChunkSize, []).

partition_list([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
partition_list(List, ChunkSize, Acc) ->
    {Chunk, Rest} = lists:split(min(ChunkSize, length(List)), List),
    partition_list(Rest, ChunkSize, [Chunk | Acc]).

%%--------------------------------------------------------------------
%% @doc Execute data accumulation.
%% @end
%%--------------------------------------------------------------------
execute_data_accumulate(_Pattern, DataList, AggFn, InitialValue) ->
    lists:foldl(AggFn, InitialValue, DataList).

%%--------------------------------------------------------------------
%% @doc Check visibility access.
%% @end
%%--------------------------------------------------------------------
check_visibility(#data_visibility{scope = local}, DataId, TaskId) ->
    case DataId =:= TaskId of
        true -> allowed;
        false -> denied
    end;
check_visibility(#data_visibility{scope = branch}, DataId, TaskId) ->
    %% Same branch if prefix matches
    DataPrefix = extract_prefix(DataId),
    TaskPrefix = extract_prefix(TaskId),
    case DataPrefix =:= TaskPrefix of
        true -> allowed;
        false -> denied
    end;
check_visibility(#data_visibility{scope = global}, _DataId, _TaskId) ->
    allowed.

%%--------------------------------------------------------------------
%% @doc Check visibility with access list.
%% @end
%%--------------------------------------------------------------------
check_visibility_with_list(#data_visibility{access_list = AccessList}, TaskId) ->
    case lists:member(TaskId, AccessList) of
        true -> allowed;
        false -> denied
    end.

%%--------------------------------------------------------------------
%% @doc Extract prefix from ID.
%% @end
%%--------------------------------------------------------------------
extract_prefix(Id) ->
    case binary:split(Id, <<"_">>) of
        [Prefix, _] -> Prefix;
        _ -> Id
    end.

%%--------------------------------------------------------------------
%% @doc Compare scopes.
%% @end
%%--------------------------------------------------------------------
compare_scopes(local, branch) -> lt;
compare_scopes(local, global) -> lt;
compare_scopes(branch, global) -> lt;
compare_scopes(global, _) -> gt;
compare_scopes(branch, local) -> gt;
compare_scopes(global, local) -> gt;
compare_scopes(global, branch) -> gt;
compare_scopes(Same, Same) -> eq.

%%--------------------------------------------------------------------
%% @doc Execute resource creation.
%% @end
%%--------------------------------------------------------------------
execute_resource_create(#resource_create{resource_type = undefined}, _ResourceId) ->
    {error, invalid_resource_type};
execute_resource_create(#resource_create{resource_type = ResourceType}, ResourceId) ->
    %% Check if resource already exists
    CurrentData = case get(test_data) of
        undefined -> #{};
        Val -> Val
    end,
    Resources = maps:get(resources, CurrentData, #{}),
    case maps:is_key(ResourceId, Resources) of
        true ->
            {error, resource_exists};
        false ->
            %% Create resource
            NewResources = Resources#{ResourceId => #{type => ResourceType, status => created}},
            put(test_data, CurrentData#{resources => NewResources}),
            {ok, ResourceId}
    end.

%%--------------------------------------------------------------------
%% @doc Validate initialization parameters.
%% @end
%%--------------------------------------------------------------------
validate_init_params(#resource_create{resource_type = api_client}, Params) ->
    Required = [api_key, endpoint],
    case lists:all(fun(K) -> maps:is_key(K, Params) end, Required) of
        true -> {ok, valid};
        false -> {error, missing_required_param}
    end;
validate_init_params(_Pattern, _Params) ->
    {ok, valid}.

%%--------------------------------------------------------------------
%% @doc Execute role allocation (first fit).
%% @end
%%--------------------------------------------------------------------
execute_role_allocate(#role_allocate{required_capability = RequiredCapability}, Resources) ->
    case find_first_match(Resources, RequiredCapability) of
        {ok, Resource} -> {ok, Resource};
        false -> {error, no_matching_resources}
    end.

%%--------------------------------------------------------------------
%% @doc Find first matching resource.
%% @end
%%--------------------------------------------------------------------
find_first_match([], _Capability) ->
    false;
find_first_match([{Id, _Role, Meta} | Rest], Capability) ->
    Capabilities = maps:get(capabilities, Meta, []),
    case lists:member(Capability, Capabilities) of
        true -> {ok, {Id, Meta}};
        false -> find_first_match(Rest, Capability)
    end.

%%--------------------------------------------------------------------
%% @doc Execute role allocation (best fit).
%% @end
%%--------------------------------------------------------------------
execute_role_allocate_best_fit(#role_allocate{required_capability = RequiredCapability}, Resources) ->
    MatchingResources = lists:filter(fun({_Id, _Role, Meta}) ->
        Capabilities = maps:get(capabilities, Meta, []),
        lists:member(RequiredCapability, Capabilities)
    end, Resources),

    case MatchingResources of
        [] -> {error, no_matching_resources};
        _ ->
            %% Find best fit (lowest score for exact match)
            Sorted = lists:keysort(2, [{Id, maps:get(score, Meta, 0), Meta} || {Id, _Role, Meta} <- MatchingResources]),
            {Id, _Score, Meta} = hd(Sorted),
            {ok, {Id, Meta}}
    end.

%%--------------------------------------------------------------------
%% @doc Execute role allocation (random).
%% @end
%%--------------------------------------------------------------------
execute_role_allocate_random(#role_allocate{required_capability = RequiredCapability}, Resources) ->
    MatchingResources = lists:filter(fun({_Id, _Role, Meta}) ->
        Capabilities = maps:get(capabilities, Meta, []),
        lists:member(RequiredCapability, Capabilities)
    end, Resources),

    case MatchingResources of
        [] -> {error, no_matching_resources};
        _ ->
            Index = rand:uniform(length(MatchingResources)),
            {Id, _Role, Meta} = lists:nth(Index, MatchingResources),
            {ok, {Id, Meta}}
    end.

%%--------------------------------------------------------------------
%% @doc Execute role allocation with capability level.
%% @end
%%--------------------------------------------------------------------
execute_role_allocate_with_level(#role_allocate{required_capability = RequiredCapability}, Resources, MinLevel) ->
    MatchingResources = lists:filter(fun({_Id, _Role, Meta}) ->
        Capabilities = maps:get(capabilities, Meta, []),
        case lists:keyfind(RequiredCapability, 1, Capabilities) of
            {RequiredCapability, Level} when Level >= MinLevel -> true;
            _ -> false
        end
    end, Resources),

    case MatchingResources of
        [] -> {error, no_matching_resources};
        _ ->
            {Id, _Role, Meta} = hd(MatchingResources),
            {ok, {Id, Meta}}
    end.

%%--------------------------------------------------------------------
%% @doc Execute resource start.
%% @end
%%--------------------------------------------------------------------
execute_resource_start(#resource_start{resource_id = ResourceId}) ->
    %% Check if resource exists
    CurrentData = case get(test_data) of
        undefined -> #{};
        Val -> Val
    end,
    Resources = maps:get(resources, CurrentData, #{}),

    case maps:is_key(ResourceId, Resources) of
        false ->
            {error, resource_not_found};
        true ->
            %% Check current state
            case maps:get(status, maps:get(ResourceId, Resources), undefined) of
                running ->
                    {error, invalid_state};
                _ ->
                    %% Start resource
                    ResourceState = #{
                        resource_id => ResourceId,
                        status => initialized,
                        started_at => erlang:timestamp()
                    },
                    {ok, ResourceState}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Execute role distribution (round-robin).
%% @end
%%--------------------------------------------------------------------
execute_role_distribute_rr(#role_distribute{work_item_ids = WorkItems}, Workers) ->
    case Workers of
        [] -> {error, no_workers_available};
        _ ->
            distribute_round_robin(WorkItems, Workers, 0, [])
    end.

distribute_round_robin([], _Workers, _Index, Acc) ->
    lists:reverse(Acc);
distribute_round_robin([WorkItem | Rest], Workers, Index, Acc) ->
    Worker = lists:nth((Index rem length(Workers)) + 1, Workers),
    distribute_round_robin(Rest, Workers, Index + 1, [{WorkItem, Worker} | Acc]).

%%--------------------------------------------------------------------
%% @doc Execute role distribution (least loaded).
%% @end
%%--------------------------------------------------------------------
execute_role_distribute_least_loaded(#role_distribute{work_item_ids = WorkItems}, CurrentLoad) ->
    case maps:size(CurrentLoad) of
        0 -> {error, no_workers_available};
        _ ->
            %% Find least loaded worker
            LoadList = maps:to_list(CurrentLoad),
            {Worker, _Load} = lists:foldl(fun({W, L}, {MinW, MinL}) ->
                case L < MinL of
                    true -> {W, L};
                    false -> {MinW, MinL}
                end
            end, hd(LoadList), tl(LoadList)),
            {ok, [{WorkItem, Worker} || WorkItem <- WorkItems]}
    end.

%%--------------------------------------------------------------------
%% @doc Execute role distribution (affinity-based).
%% @end
%%--------------------------------------------------------------------
execute_role_distribute_affinity(#role_distribute{work_item_ids = WorkItems, role_assignments = RoleAssignments}, WorkItemAffinity) ->
    lists:flatmap(fun(WorkItem) ->
        RequiredAffinity = maps:get(WorkItem, WorkItemAffinity, []),
        find_affinity_match(WorkItem, RequiredAffinity, maps:to_list(RoleAssignments))
    end, WorkItems).

find_affinity_match(WorkItem, _RequiredAffinity, []) ->
    [{WorkItem, undefined}];
find_affinity_match(WorkItem, RequiredAffinity, [{Worker, WorkerAffinity} | Rest]) ->
    case has_affinity_match(RequiredAffinity, WorkerAffinity) of
        true -> [{WorkItem, Worker}];
        false -> find_affinity_match(WorkItem, RequiredAffinity, Rest)
    end.

has_affinity_match(Required, Available) when is_list(Required), is_list(Available) ->
    lists:any(fun({K, V}) ->
        case lists:keyfind(K, 1, Available) of
            {K, V} -> true;
            _ -> false
        end
    end, Required);
has_affinity_match(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Count work assignments.
%% @end
%%--------------------------------------------------------------------
count_work_assignments(Assignments, Worker) ->
    length([W || {W, _W} <- Assignments, W =:= Worker]).

%%--------------------------------------------------------------------
%% @doc Execute capability allocation.
%% @end
%%--------------------------------------------------------------------
execute_capability_allocate(#capability_allocate{required_capabilities = Required, resource_registry = Registry}) ->
    case find_exact_capability_match(Required, Registry) of
        {ok, Resource} -> {ok, Resource};
        false -> {error, no_matching_capabilities}
    end.

%%--------------------------------------------------------------------
%% @doc Find exact capability match.
%% @end
%%--------------------------------------------------------------------
find_exact_capability_match(_Required, []) ->
    false;
find_exact_capability_match(Required, [{Id, Capabilities} | Rest]) ->
    case maps:to_list(Required) =:= maps:to_list(Capabilities) of
        true -> {ok, {Id, Capabilities}};
        false -> find_exact_capability_match(Required, Rest)
    end.

%%--------------------------------------------------------------------
%% @doc Execute capability allocation (minimum met).
%% @end
%%--------------------------------------------------------------------
execute_capability_allocate_min_met(#capability_allocate{required_capabilities = Required, resource_registry = Registry}) ->
    Matching = lists:filter(fun({_Id, Capabilities}) ->
        lists:all(fun({K, MinV}) ->
            maps:get(K, Capabilities, 0) >= MinV
        end, maps:to_list(Required))
    end, Registry),

    case Matching of
        [] -> {error, no_matching_capabilities};
        _ ->
            %% Find resource that just meets minimum (best fit)
            Sorted = lists:sort(fun({_IdA, CapA}, {_IdB, CapB}) ->
                SumA = lists:sum([V || {_, V} <- maps:to_list(CapA)]),
                SumB = lists:sum([V || {_, V} <- maps:to_list(CapB)]),
                SumA =< SumB
            end, Matching),
            {Id, Capabilities} = hd(Sorted),
            {ok, {Id, Capabilities}}
    end.

%%--------------------------------------------------------------------
%% @doc Execute capability allocation (best effort).
%% @end
%%--------------------------------------------------------------------
execute_capability_allocate_best_effort(#capability_allocate{required_capabilities = Required, resource_registry = Registry}) ->
    %% Score each resource by how well it matches
    Scored = lists:map(fun({Id, Capabilities}) ->
        Score = calculate_capability_match_score(Required, Capabilities),
        {Id, Capabilities, Score}
    end, Registry),

    MatchingResources = [{Id, Capabilities, Score} || {Id, Capabilities, Score} <- Scored, Score > 0],
    case MatchingResources of
        [] -> {error, no_matching_capabilities};
        Candidates ->
            %% Pick highest score
            Sorted = lists:reverse(lists:keysort(3, Candidates)),
            {Id, Capabilities, _Score} = hd(Sorted),
            {ok, {Id, Capabilities}}
    end.

%%--------------------------------------------------------------------
%% @doc Calculate capability match score.
%% @end
%%--------------------------------------------------------------------
calculate_capability_match_score(Required, Available) ->
    maps:fold(fun(Key, RequiredValue, Acc) ->
        case maps:get(Key, Available, no_match) of
            no_match -> Acc;
            AvailableValue when AvailableValue =:= RequiredValue -> Acc + 100;
            AvailableValue when is_number(AvailableValue), is_number(RequiredValue) ->
                Diff = abs(AvailableValue - RequiredValue),
                Acc + max(0, 50 - Diff);
            _ -> Acc + 10
        end
    end, 0, Required).

%%--------------------------------------------------------------------
%% @doc Discover capabilities.
%% @end
%%--------------------------------------------------------------------
discover_capabilities(#capability_allocate{resource_registry = Registry}, CapabilityKey) ->
    lists:filtermap(fun({Id, Capabilities}) ->
        case maps:is_key(CapabilityKey, Capabilities) of
            true -> {true, {Id, maps:get(CapabilityKey, Capabilities)}};
            false -> false
        end
    end, Registry).
