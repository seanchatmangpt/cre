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
%% @doc Multiple Instance Test Suite
%%
%% Tests for WF Substrate multiple instance patterns according to
%% docs/WF_ARCHITECTURE.md.
%%
%% Tests cover:
%% - Fixed MI policy
%% - Dynamic MI policy
%% - MI spawning opcodes
%% - MI join opcodes
%% - Context distribution
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_test_mi).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixed MI Policy Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test fixed MI policy with N=1 instance.
%%--------------------------------------------------------------------
fixed_mi_policy_single_test() ->
    Policy = {fixed, 1},
    ?assert(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test fixed MI policy with N=5 instances.
%%--------------------------------------------------------------------
fixed_mi_policy_multiple_test() ->
    Policy = {fixed, 5},
    ?assert(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test fixed MI policy with N=100 instances.
%%--------------------------------------------------------------------
fixed_mi_policy_large_test() ->
    Policy = {fixed, 100},
    ?assert(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test fixed MI policy rejects zero instances.
%%--------------------------------------------------------------------
fixed_mi_policy_zero_invalid_test() ->
    Policy = {fixed, 0},
    ?assertNot(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test fixed MI policy rejects negative instances.
%%--------------------------------------------------------------------
fixed_mi_policy_negative_invalid_test() ->
    Policy = {fixed, -1},
    ?assertNot(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test fixed MI policy rejects non-integer count.
%%--------------------------------------------------------------------
fixed_mi_policy_non_integer_invalid_test() ->
    Policy = {fixed, "5"},
    ?assertNot(wf_term:is_valid_mi_policy(Policy)).

%%====================================================================
%% Dynamic MI Policy Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test dynamic MI policy with iterator function.
%%--------------------------------------------------------------------
dynamic_mi_policy_function_test() ->
    IteratorFun = fun(_Ctx) -> done end,
    Policy = {dynamic, IteratorFun},
    ?assert(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test dynamic MI policy rejects non-function.
%%--------------------------------------------------------------------
dynamic_mi_policy_non_function_invalid_test() ->
    Policy = {dynamic, 5},
    ?assertNot(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test dynamic MI policy rejects wrong arity function.
%%--------------------------------------------------------------------
dynamic_mi_policy_wrong_arity_invalid_test() ->
    WrongArityFun = fun() -> 5 end,
    Policy = {dynamic, WrongArityFun},
    ?assertNot(wf_term:is_valid_mi_policy(Policy)).

%%--------------------------------------------------------------------
%% @doc Test dynamic MI policy with iterator returns next or done.
%%--------------------------------------------------------------------
dynamic_mi_policy_data_driven_test() ->
    IteratorFun = fun(Ctx) ->
        case maps:get(remaining, Ctx, []) of
            [] -> done;
            [Item | Rest] -> {next, Ctx#{remaining => Rest, current => Item}}
        end
    end,
    Policy = {dynamic, IteratorFun},
    ?assert(wf_term:is_valid_mi_policy(Policy)),

    Ctx1 = #{remaining => [a, b, c]},
    ?assertMatch({next, _}, IteratorFun(Ctx1)),

    Ctx2 = #{remaining => []},
    ?assertEqual(done, IteratorFun(Ctx2)).

%%====================================================================
%% MI Term Construction Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test MI term with fixed policy.
%%--------------------------------------------------------------------
mi_term_fixed_test() ->
    Task = {task, process_item, fun(_) -> ok end},
    MITerm = wf_term:mi({fixed, 3}, Task),
    ?assertMatch({mi, {fixed, 3}, _}, MITerm).

%%--------------------------------------------------------------------
%% @doc Test MI term with dynamic policy.
%%--------------------------------------------------------------------
mi_term_dynamic_test() ->
    Task = {task, process_item, fun(_) -> {ok, #{}} end},
    IteratorFun = fun(_) -> done end,
    MITerm = wf_term:mi({dynamic, IteratorFun}, Task),
    ?assertMatch({mi, {dynamic, _}, _}, MITerm).

%%--------------------------------------------------------------------
%% @doc Test MI term validation with nested tasks.
%%--------------------------------------------------------------------
mi_term_nested_valid_test() ->
    Task = {task, inner, fun(_) -> ok end},
    Seq = {seq, Task, Task},
    MITerm = wf_term:mi({fixed, 2}, Seq),
    ?assert(wf_term:is_valid(MITerm)).

%%--------------------------------------------------------------------
%% @doc Test MI term validation rejects invalid policy.
%%--------------------------------------------------------------------
mi_term_invalid_policy_test() ->
    Task = {task, process, fun(_) -> ok end},
    ?assertException(error, _, wf_term:mi(invalid_policy, Task)).

%%====================================================================
%% MI Spawning Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test fixed MI spawn opcode creation.
%%--------------------------------------------------------------------
mi_spawn_opcode_fixed_test() ->
    Policy = {fixed, 5},
    Opcode = wf_vm:op_mi_spawn(Policy),
    ?assertEqual({mi_spawn, {fixed, 5}}, Opcode),
    ?assert(wf_vm:is_opcode(Opcode)).

%%--------------------------------------------------------------------
%% @doc Test dynamic MI spawn opcode creation.
%%--------------------------------------------------------------------
mi_spawn_opcode_dynamic_test() ->
    IteratorFun = fun(_) -> done end,
    Policy = {dynamic, IteratorFun},
    Opcode = wf_vm:op_mi_spawn(Policy),
    ?assertMatch({mi_spawn, {dynamic, _}}, Opcode),
    ?assert(wf_vm:is_opcode(Opcode)).

%%--------------------------------------------------------------------
%% @doc Test MI spawn opcode type detection.
%%--------------------------------------------------------------------
mi_spawn_opcode_type_test() ->
    Opcode = wf_vm:op_mi_spawn({fixed, 3}),
    ?assertEqual(mi_spawn, wf_vm:opcode_type(Opcode)).

%%--------------------------------------------------------------------
%% @doc Test MI spawn opcode arity.
%%--------------------------------------------------------------------
mi_spawn_opcode_arity_test() ->
    Opcode = wf_vm:op_mi_spawn({fixed, 3}),
    ?assertEqual(1, wf_vm:opcode_arity(Opcode)).

%%--------------------------------------------------------------------
%% @doc Test fixed MI spawn with N=3 instances.
%%--------------------------------------------------------------------
mi_spawn_fixed_three_instances_test() ->
    Policy = {fixed, 3},
    Opcode = wf_vm:op_mi_spawn(Policy),
    ?assertMatch({mi_spawn, {fixed, 3}}, Opcode).

%%--------------------------------------------------------------------
%% @doc Test fixed MI spawn with N=1 instance.
%%--------------------------------------------------------------------
mi_spawn_fixed_single_instance_test() ->
    Policy = {fixed, 1},
    Opcode = wf_vm:op_mi_spawn(Policy),
    ?assertMatch({mi_spawn, {fixed, 1}}, Opcode).

%%====================================================================
%% MI Join Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test MI join opcode creation.
%%--------------------------------------------------------------------
mi_join_opcode_test() ->
    Policy = {fixed, 5},
    Opcode = wf_vm:op_mi_join(Policy),
    ?assertEqual({mi_join, {fixed, 5}}, Opcode),
    ?assert(wf_vm:is_opcode(Opcode)).

%%--------------------------------------------------------------------
%% @doc Test MI join opcode type detection.
%%--------------------------------------------------------------------
mi_join_opcode_type_test() ->
    Opcode = wf_vm:op_mi_join({fixed, 3}),
    ?assertEqual(mi_join, wf_vm:opcode_type(Opcode)).

%%--------------------------------------------------------------------
%% @doc Test MI join opcode arity.
%%--------------------------------------------------------------------
mi_join_opcode_arity_test() ->
    Opcode = wf_vm:op_mi_join({fixed, 3}),
    ?assertEqual(1, wf_vm:opcode_arity(Opcode)).

%%--------------------------------------------------------------------
%% @doc Test MI join with fixed policy.
%%--------------------------------------------------------------------
mi_join_fixed_policy_test() ->
    Policy = {fixed, 7},
    Opcode = wf_vm:op_mi_join(Policy),
    ?assertMatch({mi_join, {fixed, 7}}, Opcode).

%%--------------------------------------------------------------------
%% @doc Test MI join with dynamic policy.
%%--------------------------------------------------------------------
mi_join_dynamic_policy_test() ->
    CountFun = fun(_) -> 4 end,
    Policy = {dynamic, CountFun},
    Opcode = wf_vm:op_mi_join(Policy),
    ?assertMatch({mi_join, {dynamic, _}}, Opcode).

%%====================================================================
%% Context Distribution Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test context replication for fixed MI instances.
%%--------------------------------------------------------------------
context_distribution_fixed_test() ->
    InitialCtx = #{data => [1, 2, 3]},
    InstanceCount = 3,

    Contexts = replicate_context(InitialCtx, InstanceCount),

    ?assertEqual(3, length(Contexts)),
    ?assert(lists:all(fun(Ctx) -> Ctx =:= InitialCtx end, Contexts)).

%%--------------------------------------------------------------------
%% @doc Test context partition for dynamic MI instances.
%%--------------------------------------------------------------------
context_distribution_dynamic_test() ->
    Items = [a, b, c, d, e],
    InitialCtx = #{items => Items},

    PartitionedCtxs = partition_context(InitialCtx, length(Items)),

    ?assertEqual(5, length(PartitionedCtxs)),

    ExtractedItems = [maps:get(item, Ctx) || Ctx <- PartitionedCtxs],
    ?assertEqual(Items, ExtractedItems).

%%--------------------------------------------------------------------
%% @doc Test context merge after MI completion.
%%--------------------------------------------------------------------
context_merge_test() ->
    InstanceResults = [
        #{result => 1},
        #{result => 2},
        #{result => 3}
    ],

    MergedCtx = merge_contexts(InstanceResults),

    ?assertEqual(#{results => [1, 2, 3]}, MergedCtx).

%%--------------------------------------------------------------------
%% @doc Test context indexing for MI instances.
%%--------------------------------------------------------------------
context_indexing_test() ->
    BaseCtx = #{data => value},
    Count = 5,

    IndexedCtxs = index_contexts(BaseCtx, Count),

    ?assertEqual(5, length(IndexedCtxs)),

    Indices = [maps:get(index, Ctx) || Ctx <- IndexedCtxs],
    ?assertEqual(lists:seq(0, 4), Indices),

    ?assert(lists:all(fun(Ctx) -> maps:get(data, Ctx) =:= value end, IndexedCtxs)).

%%====================================================================
%% Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test MI compilation with fixed policy.
%%--------------------------------------------------------------------
mi_compile_fixed_test() ->
    Task = {task, work, fun(_) -> ok end},
    MITerm = wf_term:mi({fixed, 3}, Task),

    Bytecode = wf_compile:compile(MITerm),

    ?assert(is_list(Bytecode)),
    ?assert(length(Bytecode) > 0),

    HasMISpawn = lists:any(fun
        ({mi_spawn, _}) -> true;
        (_) -> false
    end, Bytecode),

    HasMIJoin = lists:any(fun
        ({mi_join, _}) -> true;
        (_) -> false
    end, Bytecode),

    ?assert(HasMISpawn),
    ?assert(HasMIJoin).

%%--------------------------------------------------------------------
%% @doc Test MI compilation with dynamic policy.
%%--------------------------------------------------------------------
mi_compile_dynamic_test() ->
    Task = {task, work, fun(_) -> ok end},
    CountFun = fun(Ctx) -> maps:get(count, Ctx, 1) end,
    MITerm = wf_term:mi({dynamic, CountFun}, Task),

    Bytecode = wf_compile:compile(MITerm),

    ?assert(is_list(Bytecode)),

    HasMISpawn = lists:any(fun
        ({mi_spawn, _}) -> true;
        (_) -> false
    end, Bytecode),

    ?assert(HasMISpawn).

%%--------------------------------------------------------------------
%% @doc Test MI with nested sequence.
%%--------------------------------------------------------------------
mi_nested_sequence_test() ->
    Task1 = {task, a, fun(_) -> ok end},
    Task2 = {task, b, fun(_) -> ok end},
    Seq = {seq, Task1, Task2},
    MITerm = wf_term:mi({fixed, 2}, Seq),

    ?assert(wf_term:is_valid(MITerm)),

    Bytecode = wf_compile:compile(MITerm),
    ?assert(is_list(Bytecode)),
    ?assert(length(Bytecode) > 0).

%%--------------------------------------------------------------------
%% @doc Test MI with parallel body.
%%--------------------------------------------------------------------
mi_parallel_body_test() ->
    Task1 = {task, x, fun(_) -> ok end},
    Task2 = {task, y, fun(_) -> ok end},
    Par = {par, [Task1, Task2]},
    MITerm = wf_term:mi({fixed, 3}, Par),

    ?assert(wf_term:is_valid(MITerm)),

    Bytecode = wf_compile:compile(MITerm),
    ?assert(is_list(Bytecode)).

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Replicate context N times.
%%--------------------------------------------------------------------
-spec replicate_context(Ctx :: map(), N :: pos_integer()) -> [map()].
replicate_context(Ctx, N) when N > 0 ->
    [Ctx || _ <- lists:seq(1, N)].

%%--------------------------------------------------------------------
%% @doc Partition context items for distribution.
%%--------------------------------------------------------------------
-spec partition_context(Ctx :: map(), N :: pos_integer()) -> [map()].
partition_context(#{items := Items} = _Ctx, N) when N > 0 ->
    [#{item => Item} || Item <- lists:sublist(Items, N)];
partition_context(Ctx, _N) ->
    [Ctx].

%%--------------------------------------------------------------------
%% @doc Merge contexts from multiple instances.
%%--------------------------------------------------------------------
-spec merge_contexts([map()]) -> map().
merge_contexts(Contexts) ->
    Results = [maps:get(result, Ctx) || Ctx <- Contexts],
    #{results => Results}.

%%--------------------------------------------------------------------
%% @doc Index contexts with instance number.
%%--------------------------------------------------------------------
-spec index_contexts(BaseCtx :: map(), N :: pos_integer()) -> [map()].
index_contexts(BaseCtx, N) when N > 0 ->
    [maps:put(index, Idx, BaseCtx) || Idx <- lists:seq(0, N - 1)].
