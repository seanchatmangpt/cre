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

-module(wf_compose_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helper Functions
%%====================================================================

-spec basic_workflow() -> wf_compose:workflow_spec().
basic_workflow() ->
    #{
        places => [in, out],
        transitions => #{t1 => #{type => task}},
        init_marking => #{in => [token]},
        preset => #{t1 => [in]}
    }.

-spec workflow_two_places() -> wf_compose:workflow_spec().
workflow_two_places() ->
    #{
        places => [p1, p2],
        transitions => #{t1 => #{type => task}},
        init_marking => #{p1 => [a]},
        preset => #{t1 => [p1]}
    }.

-spec workflow_three_places() -> wf_compose:workflow_spec().
workflow_three_places() ->
    #{
        places => [p1, p2, p3],
        transitions => #{t1 => #{type => task}, t2 => #{type => task}},
        init_marking => #{p1 => [x], p2 => []},
        preset => #{t1 => [p1], t2 => [p2]}
    }.

-spec empty_workflow() -> wf_compose:workflow_spec().
empty_workflow() ->
    #{
        places => [],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    }.

%%====================================================================
%% Sequential Composition Tests
%%====================================================================

sequential_empty_list_test() ->
    Result = wf_compose:sequential([]),
    ?assert(is_map(Result)),
    ?assertEqual([], maps:get(places, Result)),
    ?assertEqual(#{}, maps:get(transitions, Result)),
    ?assertEqual(#{}, maps:get(init_marking, Result)),
    ?assertEqual(#{}, maps:get(preset, Result)).

sequential_single_workflow_returns_same_test() ->
    W = basic_workflow(),
    Result = wf_compose:sequential([W]),
    ?assertEqual(W, Result).

sequential_two_workflows_combines_places_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    W2 = #{
        places => [p3, p4],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    Result = wf_compose:sequential([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ?assert(lists:member(p4, Places)).

sequential_two_workflows_merges_transitions_test() ->
    W1 = #{
        places => [p1],
        transitions => #{t1 => #{type => task}},
        init_marking => #{},
        preset => #{}
    },
    W2 = #{
        places => [p2],
        transitions => #{t2 => #{type => task}},
        init_marking => #{},
        preset => #{}
    },
    Result = wf_compose:sequential([W1, W2]),
    Transitions = maps:get(transitions, Result),
    ?assert(maps:is_key(t1, Transitions)),
    ?assert(maps:is_key(t2, Transitions)).

sequential_two_workflows_merges_markings_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{p1 => [a], p2 => []},
        preset => #{}
    },
    W2 = #{
        places => [p3, p4],
        transitions => #{},
        init_marking => #{p3 => [b], p4 => []},
        preset => #{}
    },
    Result = wf_compose:sequential([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([a], maps:get(p1, Markings)),
    ?assertEqual([b], maps:get(p3, Markings)).

sequential_two_workflows_merges_presets_test() ->
    W1 = #{
        places => [p1],
        transitions => #{},
        init_marking => #{},
        preset => #{t1 => [p1]}
    },
    W2 = #{
        places => [p2],
        transitions => #{},
        init_marking => #{},
        preset => #{t2 => [p2]}
    },
    Result = wf_compose:sequential([W1, W2]),
    Presets = maps:get(preset, Result),
    ?assert(maps:is_key(t1, Presets)),
    ?assert(maps:is_key(t2, Presets)).

sequential_three_workflows_all_places_included_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:sequential([W1, W2, W3]),
    Places = maps:get(places, Result),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)).

sequential_returns_valid_workflow_spec_test() ->
    W1 = basic_workflow(),
    W2 = workflow_two_places(),
    Result = wf_compose:sequential([W1, W2]),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(places, Result)),
    ?assert(maps:is_key(transitions, Result)),
    ?assert(maps:is_key(init_marking, Result)),
    ?assert(maps:is_key(preset, Result)),
    ?assert(is_list(maps:get(places, Result))),
    ?assert(is_map(maps:get(transitions, Result))),
    ?assert(is_map(maps:get(init_marking, Result))),
    ?assert(is_map(maps:get(preset, Result))).

sequential_four_workflows_test() ->
    Workflows = [
        #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
        #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
        #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
        #{places => [p4], transitions => #{}, init_marking => #{}, preset => #{}}
    ],
    Result = wf_compose:sequential(Workflows),
    Places = maps:get(places, Result),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ?assert(lists:member(p4, Places)).

sequential_adds_connector_transitions_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:sequential([W1, W2]),
    Transitions = maps:get(transitions, Result),
    %% Should have at least one connector transition added
    TransitionCount = maps:size(Transitions),
    ?assert(TransitionCount >= 1).

sequential_connector_preset_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:sequential([W1, W2]),
    Presets = maps:get(preset, Result),
    %% Presets map should have entries for connector transitions
    ?assert(maps:size(Presets) >= 1).

%%====================================================================
%% Parallel Composition Tests
%%====================================================================

parallel_empty_list_test() ->
    Result = wf_compose:parallel([]),
    ?assert(is_map(Result)),
    ?assertEqual([], maps:get(places, Result)),
    ?assertEqual(#{}, maps:get(transitions, Result)),
    ?assertEqual(#{}, maps:get(init_marking, Result)),
    ?assertEqual(#{}, maps:get(preset, Result)).

parallel_single_workflow_returns_same_test() ->
    W = basic_workflow(),
    Result = wf_compose:parallel([W]),
    ?assertEqual(W, Result).

parallel_two_workflows_adds_split_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(split_place, Places)).

parallel_two_workflows_adds_join_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(join_place, Places)).

parallel_two_workflows_adds_merge_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(merge_place, Places)).

parallel_two_workflows_creates_split_transition_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Transitions = maps:get(transitions, Result),
    ?assert(maps:is_key(split_trsn, Transitions)).

parallel_two_workflows_creates_join_transition_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_mapping => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Transitions = maps:get(transitions, Result),
    ?assert(maps:is_key(join_trsn, Transitions)).

parallel_split_transition_has_branch_count_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Transitions = maps:get(transitions, Result),
    SplitTrsn = maps:get(split_trsn, Transitions),
    ?assertEqual(2, maps:get(branches, SplitTrsn)).

parallel_join_transition_has_branch_count_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Transitions = maps:get(transitions, Result),
    JoinTrsn = maps:get(join_trsn, Transitions),
    ?assertEqual(2, maps:get(branches, JoinTrsn)).

parallel_includes_workflow_places_test() ->
    W1 = #{places => [p1, p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p3, p4], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ?assert(lists:member(p4, Places)).

parallel_initializes_split_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([start], maps:get(split_place, Markings)).

parallel_initializes_join_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([], maps:get(join_place, Markings)).

parallel_initializes_merge_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([], maps:get(merge_place, Markings)).

parallel_three_workflows_has_three_branches_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:parallel([W1, W2, W3]),
    Transitions = maps:get(transitions, Result),
    SplitTrsn = maps:get(split_trsn, Transitions),
    ?assertEqual(3, maps:get(branches, SplitTrsn)).

parallel_returns_valid_workflow_spec_test() ->
    W1 = basic_workflow(),
    W2 = workflow_two_places(),
    Result = wf_compose:parallel([W1, W2]),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(places, Result)),
    ?assert(maps:is_key(transitions, Result)),
    ?assert(maps:is_key(init_marking, Result)),
    ?assert(maps:is_key(preset, Result)),
    ?assert(is_list(maps:get(places, Result))),
    ?assert(is_map(maps:get(transitions, Result))),
    ?assert(is_map(maps:get(init_marking, Result))),
    ?assert(is_map(maps:get(preset, Result))).

parallel_merges_workflow_markings_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{p1 => [a], p2 => []},
        preset => #{}
    },
    W2 = #{
        places => [p3, p4],
        transitions => #{},
        init_marking => #{p3 => [b], p4 => []},
        preset => #{}
    },
    Result = wf_compose:parallel([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([a], maps:get(p1, Markings)),
    ?assertEqual([b], maps:get(p3, Markings)).

%%====================================================================
%% Choice Composition Tests
%%====================================================================

choice_empty_list_test() ->
    Result = wf_compose:choice([]),
    ?assert(is_map(Result)),
    ?assertEqual([], maps:get(places, Result)),
    ?assertEqual(#{}, maps:get(transitions, Result)),
    ?assertEqual(#{}, maps:get(init_marking, Result)),
    ?assertEqual(#{}, maps:get(preset, Result)).

choice_single_workflow_returns_same_test() ->
    W = basic_workflow(),
    Result = wf_compose:choice([W]),
    ?assertEqual(W, Result).

choice_two_workflows_adds_choice_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(choice_place, Places)).

choice_two_workflows_adds_merge_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(choice_merge, Places)).

choice_two_workflows_creates_choice_transition_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Transitions = maps:get(transitions, Result),
    ?assert(maps:is_key(choice_trsn, Transitions)).

choice_choice_transition_has_branch_count_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Transitions = maps:get(transitions, Result),
    ChoiceTrsn = maps:get(choice_trsn, Transitions),
    ?assertEqual(2, maps:get(branches, ChoiceTrsn)).

choice_includes_workflow_places_test() ->
    W1 = #{places => [p1, p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p3, p4], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ?assert(lists:member(p4, Places)).

choice_initializes_choice_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([choice], maps:get(choice_place, Markings)).

choice_initializes_merge_place_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([], maps:get(choice_merge, Markings)).

choice_choice_transition_preset_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2]),
    Presets = maps:get(preset, Result),
    ?assert(maps:is_key(choice_trsn, Presets)),
    ?assertEqual([choice_place], maps:get(choice_trsn, Presets)).

choice_three_workflows_has_three_branches_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Result = wf_compose:choice([W1, W2, W3]),
    Transitions = maps:get(transitions, Result),
    ChoiceTrsn = maps:get(choice_trsn, Transitions),
    ?assertEqual(3, maps:get(branches, ChoiceTrsn)).

choice_returns_valid_workflow_spec_test() ->
    W1 = basic_workflow(),
    W2 = workflow_two_places(),
    Result = wf_compose:choice([W1, W2]),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(places, Result)),
    ?assert(maps:is_key(transitions, Result)),
    ?assert(maps:is_key(init_marking, Result)),
    ?assert(maps:is_key(preset, Result)),
    ?assert(is_list(maps:get(places, Result))),
    ?assert(is_map(maps:get(transitions, Result))),
    ?assert(is_map(maps:get(init_marking, Result))),
    ?assert(is_map(maps:get(preset, Result))).

choice_merges_workflow_markings_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{p1 => [a], p2 => []},
        preset => #{}
    },
    W2 = #{
        places => [p3, p4],
        transitions => #{},
        init_marking => #{p3 => [b], p4 => []},
        preset => #{}
    },
    Result = wf_compose:choice([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assertEqual([a], maps:get(p1, Markings)),
    ?assertEqual([b], maps:get(p3, Markings)).

choice_four_workflows_test() ->
    Workflows = [
        #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
        #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
        #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
        #{places => [p4], transitions => #{}, init_marking => #{}, preset => #{}}
    ],
    Result = wf_compose:choice(Workflows),
    Transitions = maps:get(transitions, Result),
    ChoiceTrsn = maps:get(choice_trsn, Transitions),
    ?assertEqual(4, maps:get(branches, ChoiceTrsn)).

%%====================================================================
%% Nested Composition Tests
%%====================================================================

nested_sequential_in_parallel_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Seq = wf_compose:sequential([W1, W2]),
    Result = wf_compose:parallel([Seq, W3]),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(places, Result)),
    Places = maps:get(places, Result),
    ?assert(lists:member(split_place, Places)).

nested_choice_in_sequential_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Choice = wf_compose:choice([W1, W2]),
    Result = wf_compose:sequential([Choice, W3]),
    ?assert(is_map(Result)),
    Places = maps:get(places, Result),
    ?assert(lists:member(choice_place, Places)),
    ?assert(lists:member(p3, Places)).

nested_parallel_in_choice_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Par = wf_compose:parallel([W1, W2]),
    Result = wf_compose:choice([Par, W3]),
    ?assert(is_map(Result)),
    Places = maps:get(places, Result),
    ?assert(lists:member(split_place, Places)),
    ?assert(lists:member(choice_place, Places)).

%%====================================================================
%% Composition Spec Validation Tests
%%====================================================================

sequential_spec_has_all_required_keys_test() ->
    W1 = basic_workflow(),
    W2 = workflow_two_places(),
    Result = wf_compose:sequential([W1, W2]),
    RequiredKeys = [places, transitions, init_marking, preset],
    lists:foreach(
        fun(Key) -> ?assert(maps:is_key(Key, Result)) end,
        RequiredKeys
    ).

parallel_spec_has_all_required_keys_test() ->
    W1 = basic_workflow(),
    W2 = workflow_two_places(),
    Result = wf_compose:parallel([W1, W2]),
    RequiredKeys = [places, transitions, init_marking, preset],
    lists:foreach(
        fun(Key) -> ?assert(maps:is_key(Key, Result)) end,
        RequiredKeys
    ).

choice_spec_has_all_required_keys_test() ->
    W1 = basic_workflow(),
    W2 = workflow_two_places(),
    Result = wf_compose:choice([W1, W2]),
    RequiredKeys = [places, transitions, init_marking, preset],
    lists:foreach(
        fun(Key) -> ?assert(maps:is_key(Key, Result)) end,
        RequiredKeys
    ).

%%====================================================================
%% Workflow with Missing Fields Tests
%%====================================================================

sequential_workflow_missing_places_test() ->
    W1 = #{transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = basic_workflow(),
    %% Should use default empty list for places
    Result = wf_compose:sequential([W1, W2]),
    Places = maps:get(places, Result),
    ?assert(is_list(Places)).

parallel_workflow_missing_transitions_test() ->
    W1 = #{places => [p1], init_marking => #{}, preset => #{}},
    W2 = basic_workflow(),
    %% Should use default empty map for transitions
    Result = wf_compose:parallel([W1, W2]),
    Transitions = maps:get(transitions, Result),
    ?assert(is_map(Transitions)).

choice_workflow_missing_init_marking_test() ->
    W1 = #{places => [p1], transitions => #{}, preset => #{}},
    W2 = basic_workflow(),
    %% Should use default empty map for init_marking
    Result = wf_compose:choice([W1, W2]),
    Markings = maps:get(init_marking, Result),
    ?assert(is_map(Markings)).

%%====================================================================
%% Merging Behavior Tests
%%====================================================================

sequential_merges_distinct_transitions_test() ->
    W1 = #{
        places => [p1],
        transitions => #{t1 => #{type => task}},
        init_marking => #{},
        preset => #{}
    },
    W2 = #{
        places => [p2],
        transitions => #{t2 => #{type => task}},
        init_marking => #{},
        preset => #{}
    },
    Result = wf_compose:sequential([W1, W2]),
    Transitions = maps:get(transitions, Result),
    ?assertEqual(2, maps:size(Transitions)),
    ?assert(maps:is_key(t1, Transitions)),
    ?assert(maps:is_key(t2, Transitions)).

parallel_merges_distinct_presets_test() ->
    W1 = #{
        places => [p1],
        transitions => #{},
        init_marking => #{},
        preset => #{t1 => [p1]}
    },
    W2 = #{
        places => [p2],
        transitions => #{},
        init_marking => #{},
        preset => #{t2 => [p2]}
    },
    Result = wf_compose:parallel([W1, W2]),
    Presets = maps:get(preset, Result),
    ?assert(maps:is_key(t1, Presets)),
    ?assert(maps:is_key(t2, Presets)).

choice_merges_marking_with_duplicates_test() ->
    W1 = #{
        places => [p1],
        transitions => #{},
        init_marking => #{p1 => [a]},
        preset => #{}
    },
    W2 = #{
        places => [p1],
        transitions => #{},
        init_marking => #{p1 => [b]},
        preset => #{}
    },
    Result = wf_compose:choice([W1, W2]),
    Markings = maps:get(init_marking, Result),
    P1Marking = maps:get(p1, Markings),
    %% Markings should be merged (concatenated)
    ?assert(lists:member(a, P1Marking)),
    ?assert(lists:member(b, P1Marking)).

%%====================================================================
%% Composability Tests
%%====================================================================

composed_result_is_valid_workflow_spec_test() ->
    W1 = basic_workflow(),
    W2 = workflow_two_places(),
    Seq = wf_compose:sequential([W1, W2]),
    %% Composed workflow should be usable in another composition
    Result = wf_compose:parallel([Seq, W2]),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(places, Result)).

double_sequential_composition_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Seq1 = wf_compose:sequential([W1, W2]),
    Seq2 = wf_compose:sequential([Seq1, W3]),
    Places = maps:get(places, Seq2),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)).

double_parallel_composition_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Par1 = wf_compose:parallel([W1, W2]),
    Par2 = wf_compose:parallel([Par1, W3]),
    Places = maps:get(places, Par2),
    ?assert(lists:member(split_place, Places)).

double_choice_composition_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Ch1 = wf_compose:choice([W1, W2]),
    Ch2 = wf_compose:choice([Ch1, W3]),
    Places = maps:get(places, Ch2),
    ?assert(lists:member(choice_place, Places)).
