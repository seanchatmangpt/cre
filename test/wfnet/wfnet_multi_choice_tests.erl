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
%% @doc Multi-Choice Workflow Pattern (WCP-07) Tests
%%
%% Comprehensive test suite for wfnet_multi_choice pattern including:
%% - Basic multi-choice execution with multiple selected branches
%% - Single branch selection (exclusive choice behavior)
%% - No branches selected
%% - All branches selected
%% - Configuration options (allow_none, selection_mode, merge_mode)
%% - Workflow specification validation
%% - State tracking
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_multi_choice_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_pnet.hrl").

%% Include the multi_choice state record definition
-include("../../src/wfnet/patterns/wfnet_multi_choice.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main test generator.
%%--------------------------------------------------------------------
wfnet_multi_choice_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"multiple branches selected", fun multiple_branches_test/0},
        {"single branch selected", fun single_branch_test/0},
        {"no branches selected", fun no_branches_test/0},
        {"all branches selected", fun all_branches_test/0},
        {"workflow spec is valid", fun workflow_spec_test/0},
        {"init creates valid state", fun init_test/0},
        {"init_marking returns correct tokens", fun init_marking_test/0},
        {"is_enabled checks transition enablement", fun is_enabled_test/0},
        {"fire evaluate selects branches", fun fire_evaluate_test/0},
        {"fire evaluate with all selected", fun fire_evaluate_all_test/0},
        {"fire merge completes workflow", fun fire_merge_test/0},
        {"normalize_branches converts atoms to tuples", fun normalize_branches_test/0},
        {"branch_place generates correct names", fun branch_place_test/0},
        {"new with allow_none option", fun new_with_allow_none_test/0},
        {"new with selection_mode option", fun new_with_selection_mode_test/0},
        {"new with merge_mode option", fun new_with_merge_mode_test/0},
        {"five branches workflow", fun five_branches_test/0},
        {"preset and postset structure", fun preset_postset_test/0},
        {"execute with conditions", fun execute_conditions_test/0},
        {"evaluate_condition handles crashes", fun evaluate_condition_crash_test/0},
        {"find_completed_branch identifies branch", fun find_completed_branch_test/0}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_SetupState) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that multiple branches can be selected.
%%--------------------------------------------------------------------
multiple_branches_test() ->
    Branches = [
        {a, #{condition => fun(X) -> X > 0 end}},
        {b, #{condition => fun(X) -> X < 10 end}},
        {c, #{condition => fun(X) -> X rem 2 =:= 0 end}}
    ],
    {ok, Results} = wfnet_multi_choice:execute(Branches, 5),
    %% 5 > 0 (true), 5 < 10 (true), 5 rem 2 =:= 0 (false)
    ?assertMatch(#{a := _, b := _}, Results),
    ?assertNot(maps:is_key(c, Results)).

%%--------------------------------------------------------------------
%% @doc Test that single branch selection works.
%%--------------------------------------------------------------------
single_branch_test() ->
    Branches = [
        {a, #{condition => fun(X) -> X > 100 end}},
        {b, #{condition => fun(X) -> X < 10 end}},
        {c, #{condition => fun(X) -> X > 50 end}}
    ],
    {ok, Results} = wfnet_multi_choice:execute(Branches, 5),
    %% Only b should be selected (5 < 10)
    ?assertMatch(#{b := _}, Results),
    ?assertNot(maps:is_key(a, Results)),
    ?assertNot(maps:is_key(c, Results)).

%%--------------------------------------------------------------------
%% @doc Test that no branches selected returns empty map.
%%--------------------------------------------------------------------
no_branches_test() ->
    Branches = [
        {a, #{condition => fun(X) -> X > 100 end}},
        {b, #{condition => fun(X) -> X > 200 end}}
    ],
    ?assertEqual({ok, #{}}, wfnet_multi_choice:execute(Branches, 5)).

%%--------------------------------------------------------------------
%% @doc Test that all branches can be selected.
%%--------------------------------------------------------------------
all_branches_test() ->
    Branches = [
        {a, #{condition => fun(_) -> true end}},
        {b, #{condition => fun(_) -> true end}},
        {c, #{condition => fun(_) -> true end}}
    ],
    {ok, Results} = wfnet_multi_choice:execute(Branches, any_input),
    ?assertEqual(3, maps:size(Results)),
    ?assert(maps:is_key(a, Results)),
    ?assert(maps:is_key(b, Results)),
    ?assert(maps:is_key(c, Results)).

%%--------------------------------------------------------------------
%% @doc Test that workflow spec is valid.
%%--------------------------------------------------------------------
workflow_spec_test() ->
    Spec = wfnet_multi_choice:new([a, b, c]),

    %% Check required fields
    ?assert(maps:is_key(places, Spec)),
    ?assert(maps:is_key(transitions, Spec)),
    ?assert(maps:is_key(start_place, Spec)),
    ?assert(maps:is_key(end_place, Spec)),
    ?assert(maps:is_key(preset, Spec)),
    ?assert(maps:is_key(postset, Spec)),

    %% Check structure
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertEqual(3, length(maps:get(transitions, Spec))),

    %% Check optional metadata
    Optional = maps:get(optional, Spec),
    ?assertEqual(multi_choice, maps:get(pattern, Optional)),
    ?assertEqual(3, maps:get(branch_count, Optional)).

%%--------------------------------------------------------------------
%% @doc Test init creates valid state.
%%--------------------------------------------------------------------
init_test() ->
    Branches = [a, b, c],
    {ok, State} = wfnet_multi_choice:init(Branches),

    ?assertEqual(3, State#multi_choice_state.branch_count),
    ?assertEqual([], State#multi_choice_state.selected),
    ?assertEqual([], State#multi_choice_state.completed),
    ?assertEqual(some, State#multi_choice_state.selection_mode),
    ?assertEqual(false, State#multi_choice_state.allow_none),
    ?assertEqual(sync, State#multi_choice_state.merge_mode).

%%--------------------------------------------------------------------
%% @doc Test init_marking returns correct tokens.
%%--------------------------------------------------------------------
init_marking_test() ->
    {ok, State} = wfnet_multi_choice:init([a, b]),

    ?assertEqual([init], wfnet_multi_choice:init_marking(start, State)),
    ?assertEqual([], wfnet_multi_choice:init_marking('end', State)),
    ?assertEqual([], wfnet_multi_choice:init_marking(evaluate, State)),
    ?assertEqual([], wfnet_multi_choice:init_marking(merge, State)).

%%--------------------------------------------------------------------
%% @doc Test is_enabled checks transition enablement.
%%--------------------------------------------------------------------
is_enabled_test() ->
    {ok, State} = wfnet_multi_choice:init([a, b]),
    Mode = #{start => [init]},

    ?assert(wfnet_multi_choice:is_enabled(evaluate, Mode, State)),

    %% Merge is enabled when all selected branches completed
    State2 = State#multi_choice_state{selected = [a, b], completed = [a, b]},
    Mode2 = #{merge => [all_done]},
    ?assert(wfnet_multi_choice:is_enabled(merge, Mode2, State2)),

    %% Not all completed - merge disabled
    State3 = State#multi_choice_state{selected = [a, b], completed = [a]},
    ?assertNot(wfnet_multi_choice:is_enabled(merge, Mode2, State3)).

%%--------------------------------------------------------------------
%% @doc Test fire evaluate selects branches.
%%--------------------------------------------------------------------
fire_evaluate_test() ->
    BranchesConfig = #{
        a => #{condition => fun(_) -> true end},
        b => #{condition => fun(_) -> false end}
    },
    State = #multi_choice_state{branches = BranchesConfig, allow_none = false},
    Mode = #{start => [init]},

    {produce, ProduceMap, NewState} = wfnet_multi_choice:fire(evaluate, Mode, State),

    %% Check selected branches
    ?assertEqual([a], NewState#multi_choice_state.selected),
    ?assertEqual([selected], maps:get(a_branch, ProduceMap)).

%%--------------------------------------------------------------------
%% @doc Test fire evaluate with all branches selected.
%%--------------------------------------------------------------------
fire_evaluate_all_test() ->
    BranchesConfig = #{
        a => #{condition => fun(_) -> true end},
        b => #{condition => fun(_) -> true end}
    },
    State = #multi_choice_state{branches = BranchesConfig},
    Mode = #{start => [init]},

    {produce, ProduceMap, NewState} = wfnet_multi_choice:fire(evaluate, Mode, State),

    %% Check all branches selected
    ?assertEqual([a, b], lists:sort(NewState#multi_choice_state.selected)),
    ?assertEqual([selected], maps:get(a_branch, ProduceMap)),
    ?assertEqual([selected], maps:get(b_branch, ProduceMap)).

%%--------------------------------------------------------------------
%% @doc Test fire merge completes workflow.
%%--------------------------------------------------------------------
fire_merge_test() ->
    State = #multi_choice_state{selected = [a, b]},
    Mode = #{merge => [all_done]},

    {produce, ProduceMap, _NewState} = wfnet_multi_choice:fire(merge, Mode, State),

    ?assertEqual([], maps:get(merge, ProduceMap)),
    ?assertMatch([{multi_choice_complete, [a, b]}], maps:get('end', ProduceMap)).

%%--------------------------------------------------------------------
%% @doc Test normalize_branches converts atoms to tuples.
%%--------------------------------------------------------------------
normalize_branches_test() ->
    ?assertEqual([{a, #{}}, {b, #{}}], wfnet_multi_choice:normalize_branches([a, b])).

%%--------------------------------------------------------------------
%% @doc Test branch_place generates correct names.
%%--------------------------------------------------------------------
branch_place_test() ->
    ?assertEqual(a_branch, wfnet_multi_choice:branch_place(a)),
    ?assertEqual(my_branch_branch, wfnet_multi_choice:branch_place(my_branch)).

%%--------------------------------------------------------------------
%% @doc Test new with allow_none option.
%%--------------------------------------------------------------------
new_with_allow_none_test() ->
    Spec = wfnet_multi_choice:new([a, b], #{allow_none => true}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(true, maps:get(allow_none, Optional)).

%%--------------------------------------------------------------------
%% @doc Test new with selection_mode option.
%%--------------------------------------------------------------------
new_with_selection_mode_test() ->
    Spec = wfnet_multi_choice:new([a, b], #{selection_mode => all}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(all, maps:get(selection_mode, Optional)).

%%--------------------------------------------------------------------
%% @doc Test new with merge_mode option.
%%--------------------------------------------------------------------
new_with_merge_mode_test() ->
    Spec = wfnet_multi_choice:new([a, b], #{merge_mode => async}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(async, maps:get(merge_mode, Optional)).

%%--------------------------------------------------------------------
%% @doc Test workflow with five branches.
%%--------------------------------------------------------------------
five_branches_test() ->
    Spec = wfnet_multi_choice:new([a, b, c, d, e]),
    Optional = maps:get(optional, Spec),
    ?assertEqual(5, maps:get(branch_count, Optional)),
    ?assertEqual(9, length(maps:get(places, Spec))).

%%--------------------------------------------------------------------
%% @doc Test preset and postset structure.
%%--------------------------------------------------------------------
preset_postset_test() ->
    Spec = wfnet_multi_choice:new([a, b]),
    Preset = maps:get(preset, Spec),
    Postset = maps:get(postset, Spec),

    %% Check preset
    ?assertEqual([start], maps:get(evaluate, Preset)),
    ?assertEqual([a_branch, b_branch], lists:sort(maps:get(complete_branch, Preset))),
    ?assertEqual([merge], maps:get(merge, Preset)),

    %% Check postset
    ?assert(lists:member(evaluate, maps:get(evaluate, Postset))),
    ?assert(lists:member(a_branch, maps:get(evaluate, Postset))),
    ?assert(lists:member(b_branch, maps:get(evaluate, Postset))),
    ?assertEqual([merge], maps:get(complete_branch, Postset)),
    ?assertEqual(['end'], maps:get(merge, Postset)).

%%--------------------------------------------------------------------
%% @doc Test execute with conditions.
%%--------------------------------------------------------------------
execute_conditions_test() ->
    Branches = [
        {a, #{condition => fun(X) -> X > 0 end, handler => fun(X) -> X * 2 end}},
        {b, #{condition => fun(X) -> X < 10 end, handler => fun(X) -> X + 1 end}}
    ],
    ?assertMatch({ok, #{a := 10, b := 6}}, wfnet_multi_choice:execute(Branches, 5)).

%%--------------------------------------------------------------------
%% @doc Test evaluate_condition handles crashes.
%%--------------------------------------------------------------------
evaluate_condition_crash_test() ->
    Cond = fun(_) -> error(bad) end,
    ?assertNot(wfnet_multi_choice:evaluate_condition(Cond, input)).

%%--------------------------------------------------------------------
%% @doc Test find_completed_branch identifies branch.
%%--------------------------------------------------------------------
find_completed_branch_test() ->
    State = #multi_choice_state{selected = [a, b]},
    Mode = #{a_branch => [selected], b_branch => []},
    ?assertEqual(a, wfnet_multi_choice:find_completed_branch(Mode, State)).

%%====================================================================
%% Property-Based Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Property: Selected branches are subset of defined branches.
%%--------------------------------------------------------------------
prop_selected_subset_test() ->
    %% For any input, selected branches should be a subset of defined branches
    Branches = [
        {a, #{condition => fun(X) -> X > 0 end}},
        {b, #{condition => fun(X) -> X < 10 end}},
        {c, #{condition => fun(X) -> X rem 2 =:= 0 end}}
    ],
    {ok, Results} = wfnet_multi_choice:execute(Branches, 5),
    SelectedKeys = maps:keys(Results),
    ?assert(lists:all(fun(K) -> lists:member(K, [a, b, c]) end, SelectedKeys)).

%%====================================================================
%% Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test workflow spec structure for composition.
%%--------------------------------------------------------------------
integration_spec_test_() ->
    {setup,
     fun() ->
         %% Verify spec can be used for composition
         Spec1 = wfnet_multi_choice:new([branch1, branch2]),
         Spec2 = wfnet_multi_choice:new([a, b, c], #{allow_none => true}),
         {Spec1, Spec2}
     end,
     fun(_Specs) -> ok end,
     [
      {"spec1 has correct structure", fun({Spec1, _}) ->
         ?assertEqual(start, maps:get(start_place, Spec1)),
         ?assertEqual('end', maps:get(end_place, Spec1))
      end},
      {"spec2 has allow_none option", fun({_, Spec2}) ->
         Optional = maps:get(optional, Spec2),
         ?assertEqual(true, maps:get(allow_none, Optional))
      end}
     ]}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test empty branches error.
%%--------------------------------------------------------------------
empty_branches_error_test() ->
    ?assertError(empty_branches, wfnet_multi_choice:new([])).

%%--------------------------------------------------------------------
%% @doc Test with no condition function (default true).
%%--------------------------------------------------------------------
no_condition_test() ->
    Branches = [
        {a, #{}},  %% No condition, defaults to true
        {b, #{}}
    ],
    {ok, Results} = wfnet_multi_choice:execute(Branches, any_input),
    %% Both should be selected since no condition = true
    ?assertEqual(2, maps:size(Results)).

%%--------------------------------------------------------------------
%% @doc Test branch place name generation for various atoms.
%%--------------------------------------------------------------------
branch_place_various_test() ->
    ?assertEqual(simple_branch, wfnet_multi_choice:branch_place(simple)),
    ?assertEqual(abc_123_branch, wfnet_multi_choice:branch_place(abc_123)),
    ?assertEqual('_private_branch', wfnet_multi_choice:branch_place('_private')).

%%--------------------------------------------------------------------
%% @doc Test selection_mode all option.
%%--------------------------------------------------------------------
selection_mode_all_test() ->
    Spec = wfnet_multi_choice:new([a, b], #{selection_mode => all}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(all, maps:get(selection_mode, Optional)).

%%--------------------------------------------------------------------
%% @doc Test selection_mode one option.
%%--------------------------------------------------------------------
selection_mode_one_test() ->
    Spec = wfnet_multi_choice:new([a, b, c], #{selection_mode => one}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(one, maps:get(selection_mode, Optional)).
