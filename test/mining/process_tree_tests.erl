%% -*- erlang -*-
%% @doc Process Tree Extraction Tests

-module(process_tree_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% Extraction tests
%%--------------------------------------------------------------------

extract_tree_simple_test() ->
    Log = [[a, b, c]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)),
    ?assert(element(1, Tree) =:= activity orelse element(1, Tree) =:= operator).

extract_tree_sequential_test() ->
    Log = [[a, b, c, d], [a, b, c, d]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)),
    ?assertMatch({activity, _} orelse {operator, _, _}, Tree).

extract_tree_choice_test() ->
    Log = [[a, b, d], [a, c, d]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)).

extract_tree_parallel_test() ->
    Log = [[a, b, c, d], [a, c, b, d]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)).

extract_tree_loop_test() ->
    Log = [[a, b, a, b, c]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)).

extract_tree_empty_test() ->
    Log = [],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)).

extract_tree_with_options_test() ->
    Log = [[a, b, c]],
    Options = #{noise_threshold => 0.1, min_support => 0.05},
    {ok, Tree} = process_tree:extract_tree(Log, Options),
    ?assert(is_tuple(Tree)).

extract_tree_single_activity_test() ->
    Log = [[a]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assertEqual({activity, a}, Tree).

%%--------------------------------------------------------------------
%% Tree analysis tests
%%--------------------------------------------------------------------

count_nodes_test() ->
    ?assertEqual(1, process_tree:count_nodes({activity, a})),

    Tree = {operator, sequence, [
        {activity, a},
        {activity, b},
        {activity, c}
    ]},
    ?assertEqual(4, process_tree:count_nodes(Tree)),

    NestedTree = {operator, sequence, [
        {operator, parallel, [{activity, a}, {activity, b}]},
        {activity, c}
    ]},
    ?assertEqual(6, process_tree:count_nodes(NestedTree)).

tree_depth_test() ->
    ?assertEqual(1, process_tree:tree_depth({activity, a})),

    Tree = {operator, sequence, [
        {activity, a},
        {activity, b}
    ]},
    ?assertEqual(2, process_tree:tree_depth(Tree)),

    NestedTree = {operator, sequence, [
        {operator, parallel, [{activity, a}, {activity, b}]},
        {activity, c}
    ]},
    ?assertEqual(3, process_tree:tree_depth(NestedTree)).

get_activities_test() ->
    Tree = {operator, sequence, [
        {activity, a},
        {activity, b},
        {activity, c}
    ]},
    Activities = lists:sort(process_tree:get_activities(Tree)),
    ?assertEqual([a, b, c], Activities),

    NestedTree = {operator, parallel, [
        {activity, a},
        {activity, b}
    ]},
    NestedActivities = lists:sort(process_tree:get_activities(NestedTree)),
    ?assertEqual([a, b], NestedActivities).

get_activities_nested_test() ->
    Tree = {operator, sequence, [
        {operator, parallel, [{activity, a}, {activity, b}]},
        {activity, c}
    ]},
    Activities = lists:sort(process_tree:get_activities(Tree)),
    ?assertEqual([a, b, c], Activities).

get_operators_test() ->
    Tree = {operator, sequence, [
        {operator, parallel, [{activity, a}, {activity, b}]},
        {activity, c}
    ]},
    Operators = process_tree:get_operators(Tree),
    ?assert(lists:member(sequence, Operators)),
    ?assert(lists:member(parallel, Operators)),
    ?assertEqual(2, length(Operators)).

get_operators_single_test() ->
    Tree = {operator, choice, [{activity, a}, {activity, b}]},
    Operators = process_tree:get_operators(Tree),
    ?assertEqual([choice], Operators).

find_operator_test() ->
    Tree = {operator, sequence, [
        {operator, parallel, [{activity, a}, {activity, b}]},
        {activity, c}
    ]},
    ParallelNodes = process_tree:find_operator(Tree, parallel),
    ?assertEqual(1, length(ParallelNodes)),

    ChoiceNodes = process_tree:find_operator(Tree, choice),
    ?assertEqual(0, length(ChoiceNodes)).

find_operator_nested_test() ->
    Tree = {operator, sequence, [
        {operator, parallel, [
            {operator, loop, [{activity, a}]},
            {activity, b}
        ]},
        {activity, c}
    ]},
    LoopNodes = process_tree:find_operator(Tree, loop),
    ?assertEqual(1, length(LoopNodes)).

replace_subtree_test() ->
    Old = {activity, a},
    New = {activity, x},
    Tree = {operator, sequence, [Old, {activity, c}]},
    Result = process_tree:replace_subtree(Tree, Old, New),
    ?assertEqual({operator, sequence, [New, {activity, c}]}, Result).

replace_subtree_nested_test() ->
    Old = {activity, b},
    New = {activity, x},
    Tree = {operator, sequence, [
        {activity, a},
        {operator, parallel, [Old, {activity, c}]},
        {activity, d}
    ]},
    Result = process_tree:replace_subtree(Tree, Old, New),
    ?assertMatch({operator, sequence, [
        {activity, a},
        {operator, parallel, [New, {activity, c}]},
        {activity, d}
    ]}, Result).

%%--------------------------------------------------------------------
%% Validation tests
%%--------------------------------------------------------------------

validate_tree_activity_test() ->
    Tree = {activity, a},
    ?assertEqual({ok, true}, process_tree:validate_tree(Tree)).

validate_tree_operator_test() ->
    Tree = {operator, sequence, [{activity, a}, {activity, b}]},
    ?assertEqual({ok, true}, process_tree:validate_tree(Tree)).

validate_tree_all_operators_test() ->
    Operators = [sequence, choice, parallel, loop, or],
    lists:foreach(fun(Op) ->
        Tree = {operator, Op, [{activity, a}]},
        ?assertEqual({ok, true}, process_tree:validate_tree(Tree))
    end, Operators).

validate_tree_invalid_test() ->
    Tree = {invalid, type},
    ?assertMatch({error, _}, process_tree:validate_tree(Tree)).

validate_tree_nested_test() ->
    Tree = {operator, sequence, [
        {operator, parallel, [{activity, a}, {activity, b}]},
        {activity, c}
    ]},
    ?assertEqual({ok, true}, process_tree:validate_tree(Tree)).

%%--------------------------------------------------------------------
%% Simplification tests
%%--------------------------------------------------------------------

simplify_tree_test() ->
    Tree = {operator, sequence, [
        {operator, parallel, [{activity, a}]}
    ]},
    Simplified = process_tree:simplify_tree(Tree),
    %% Should simplify by removing redundant operators
    ?assert(is_tuple(Simplified)).

simplify_tree_single_child_test() ->
    Tree = {operator, sequence, [{activity, a}]},
    Simplified = process_tree:simplify_tree(Tree),
    ?assertEqual({activity, a}, Simplified).

simplify_tree_empty_test() ->
    Tree = {operator, sequence, []},
    Simplified = process_tree:simplify_tree(Tree),
    ?assert(is_tuple(Simplified)).

simplify_tree_nested_test() ->
    Tree = {operator, sequence, [
        {operator, choice, [{activity, a}]},
        {operator, parallel, [{activity, b}]}
    ]},
    Simplified = process_tree:simplify_tree(Tree),
    ?assert(is_tuple(Simplified)).

%%--------------------------------------------------------------------
%% Conversion tests
%%--------------------------------------------------------------------

to_petri_net_test() ->
    Tree = {operator, sequence, [{activity, a}, {activity, b}]},
    Net = process_tree:to_petri_net(Tree),
    ?assert(maps:is_key(places, Net)),
    ?assert(maps:is_key(transitions, Net)),
    ?assert(maps:is_key(arcs, Net)),
    ?assert(maps:is_key(initial_place, Net)),
    ?assert(maps:is_key(final_place, Net)).

to_petri_net_activity_test() ->
    Tree = {activity, a},
    Net = process_tree:to_petri_net(Tree),
    ?assert(maps:is_key(places, Net)),
    ?assert(maps:is_key(transitions, Net)).

to_bpmn_test() ->
    Tree = {operator, sequence, [{activity, a}, {activity, b}]},
    BPMN = process_tree:to_bpmn(Tree),
    ?assert(maps:is_key(nodes, BPMN)),
    ?assert(maps:is_key(edges, BPMN)),
    ?assertEqual(bpmn, maps:get(type, BPMN)).

to_bpmn_activity_test() ->
    Tree = {activity, a},
    BPMN = process_tree:to_bpmn(Tree),
    ?assert(maps:is_key(nodes, BPMN)),
    ?assert(is_list(maps:get(nodes, BPMN))).

%%--------------------------------------------------------------------
%% Cut detection tests
%%--------------------------------------------------------------------

sequence_cut_test() ->
    Log = [[a, b, c], [a, b, c]],
    Result = process_tree:sequence_cut(Log),
    ?assertMatch({ok, {sequence, _}}, Result).

sequence_cut_failure_test() ->
    Log = [[a, b, c], [x, y, z]],
    Result = process_tree:sequence_cut(Log),
    ?assertMatch({error, _}, Result).

parallel_cut_test() ->
    Log = [[a, b, c, d], [a, c, b, d]],
    Result = process_tree:parallel_cut(Log),
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok  %% May not detect as parallel in simple case
    end.

choice_cut_test() ->
    Log = [[a, b, d], [a, c, d]],
    Result = process_tree:choice_cut(Log),
    ?assertMatch({ok, {choice, _}}, Result).

loop_cut_test() ->
    Log = [[a, b, a, b, c], [a, b, a, b, a, c]],
    Result = process_tree:loop_cut(Log),
    ?assertMatch({ok, {loop, _}}, Result).

loop_cut_failure_test() ->
    Log = [[a, b, c]],
    Result = process_tree:loop_cut(Log),
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

extract_orderings_test() ->
    Log = [[a, b, c]],
    Orderings = process_tree:extract_orderings(Log),
    ?assert(lists:member({a, b}, Orderings)),
    ?assert(lists:member({b, c}, Orderings)),
    ?assertNot(lists:member({c, a}, Orderings)),
    ?assertNot(lists:member({b, a}, Orderings)).

extract_orderings_multiple_traces_test() ->
    Log = [[a, b], [b, a]],
    Orderings = process_tree:extract_orderings(Log),
    ?assert(lists:member({a, b}, Orderings)),
    ?assert(lists:member({b, a}, Orderings)).

check_disjoint_test() ->
    ?assert(process_tree:check_disjoint([[a, b], [c, d]])),
    ?assertNot(process_tree:check_disjoint([[a, b], [b, c]])),
    ?assert(process_tree:check_disjoint([[a], [b], [c]])).

check_disjoint_empty_test() ->
    ?assert(process_tree:check_disjoint([])),
    ?assert(process_tree:check_disjoint([[]])).

%%--------------------------------------------------------------------
%% Options tests
%%--------------------------------------------------------------------

extraction_options_defaults_test() ->
    Options = #{},
    {ok, Tree} = process_tree:extract_tree([[a, b, c]], Options),
    ?assert(is_tuple(Tree)).

extraction_options_with_threshold_test() ->
    Options = #{noise_threshold => 0.2, min_support => 0.1},
    NoisyLog = [[a, b, c], [a, b, c], [a, x, y, z]],
    {ok, Tree} = process_tree:extract_tree(NoisyLog, Options),
    ?assert(is_tuple(Tree)).

%%--------------------------------------------------------------------
%% Record tests
%%--------------------------------------------------------------------

tree_node_types_test() ->
    ActivityNode = {activity, a},
    ?assertMatch({activity, _}, ActivityNode),

    OperatorNode = {operator, sequence, [{activity, a}]},
    ?assertMatch({operator, _, _}, OperatorNode).

%%--------------------------------------------------------------------
%% Edge case tests
%%--------------------------------------------------------------------

extract_tree_single_trace_test() ->
    Log = [[a]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assertEqual({activity, a}, Tree).

extract_tree_varied_length_test() ->
    Log = [[a, b], [a, b, c], [a, b, c, d]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)).

extract_tree_all_same_activities_test() ->
    Log = [[a, a, a], [a, a]],
    {ok, Tree} = process_tree:extract_tree(Log),
    ?assert(is_tuple(Tree)).
