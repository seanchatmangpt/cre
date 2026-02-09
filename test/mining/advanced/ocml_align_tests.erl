%% -*- erlang -*-
%% @doc Tests for Object-Centric Alignment (OCML)

-module(ocml_align_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

simple_ocel_log() ->
    [
        #{
            id => <<"e1">>,
            activity => create_order,
            timestamp => 1000,
            objects => [{<<"order">>, <<"o1">>}, {<<"customer">>, <<"c1">>}]
        },
        #{
            id => <<"e2">>,
            activity => add_item,
            timestamp => 2000,
            objects => [{<<"order">>, <<"o1">>}, {<<"item">>, <<"i1">>}]
        },
        #{
            id => <<"e3">>,
            activity => pay,
            timestamp => 3000,
            objects => [{<<"order">>, <<"o1">>}, {<<"customer">>, <<"c1">>}]
        }
    ].

full_workflow_log() ->
    [
        #{id => <<"e1">>, activity => create_order, timestamp => 1000,
          objects => [{<<"order">>, <<"o1">>}, {<<"customer">>, <<"c1">>}]},
        #{id => <<"e2">>, activity => add_item, timestamp => 2000,
          objects => [{<<"order">>, <<"o1">>}, {<<"item">>, <<"i1">>}]},
        #{id => <<"e3">>, activity => pay, timestamp => 3000,
          objects => [{<<"order">>, <<"o1">>}, {<<"customer">>, <<"c1">>}]},
        #{id => <<"e4">>, activity => ship, timestamp => 4000,
          objects => [{<<"order">>, <<"o1">>}, {<<"item">>, <<"i1">>}]}
    ].

simple_model() ->
    #{
        object_types => [<<"order">>, <<"customer">>, <<"item">>],
        activities => [create_order, add_item, pay, ship],
        lifecycles => #{<<"order">> => [created, paid, shipped]},
        interactions => [
            #{from => <<"customer">>, to => <<"order">>},
            #{from => <<"order">>, to => <<"item">>}
        ]
    }.

empty_log() ->
    [].

%%====================================================================
%% Object Type Extraction Tests
%%====================================================================

extract_object_types_test() ->
    Log = simple_ocel_log(),
    Types = ocml_align:extract_object_types(Log),
    ?assert(lists:member(<<"order">>, Types)),
    ?assert(lists:member(<<"customer">>, Types)),
    ?assert(lists:member(<<"item">>, Types)).

extract_object_types_empty_test() ->
    Log = empty_log(),
    Types = ocml_align:extract_object_types(Log),
    ?assertEqual([], Types).

extract_object_types_single_test() ->
    Log = [
        #{id => <<"e1">>, activity => a, timestamp => 1000,
          objects => [{<<"type1">>, <<"id1">>}]}
    ],
    Types = ocml_align:extract_object_types(Log),
    ?assertEqual([<<"type1">>], Types).

%%====================================================================
%% Object Graph Tests
%%====================================================================

build_object_graph_test() ->
    Log = simple_ocel_log(),
    Graph = ocml_align:build_object_graph(Log),
    ?assert(is_map(Graph)),
    ?assert(maps:is_key({<<"order">>, <<"customer">>}, Graph)).

build_object_graph_empty_test() ->
    Log = empty_log(),
    Graph = ocml_align:build_object_graph(Log),
    ?assertEqual(#{}, Graph).

build_object_graph_count_test() ->
    Log = [
        #{id => <<"e1">>, activity => a, timestamp => 1000,
          objects => [{<<"x">>, <<"1">>}, {<<"y">>, <<"2">>}]},
        #{id => <<"e2">>, activity => a, timestamp => 2000,
          objects => [{<<"x">>, <<"1">>}, {<<"y">>, <<"3">>}]}
    ],
    Graph = ocml_align:build_object_graph(Log),
    ?assertEqual(2, maps:get({<<"x">>, <<"y">>}, Graph)).

%%====================================================================
%% Lifecycle Validation Tests
%%====================================================================

validate_lifecycle_test() ->
    Model = simple_model(),
    {ok, IsValid} = ocml_align:validate_lifecycle(<<"order">>, Model),
    ?assertEqual(true, IsValid).

validate_lifecycle_unknown_test() ->
    Model = simple_model(),
    {ok, IsValid} = ocml_align:validate_lifecycle(<<"unknown">>, Model),
    ?assertEqual(true, IsValid).

validate_lifecycle_empty_states_test() ->
    Model = simple_model#{lifecycles => #{<<"order">> => []}},
    {ok, IsValid} = ocml_align:validate_lifecycle(<<"order">>, Model),
    ?assertEqual(true, IsValid).

%%====================================================================
%% Alignment Tests
%%====================================================================

align_ocml_test() ->
    Log = simple_ocel_log(),
    Model = simple_model(),
    {ok, Result} = ocml_align:align_ocml(Log, Model),
    ?assert(maps:is_key(fitness, Result)),
    ?assert(maps:is_key(alignment, Result)),
    ?assert(maps:is_key(object_consistency, Result)),
    ?assert(maps:is_key(interaction_validity, Result)).

align_ocml_empty_log_test() ->
    Log = empty_log(),
    Model = simple_model(),
    {ok, Result} = ocml_align:align_ocml(Log, Model),
    ?assert(is_map(Result)).

align_trace_test() ->
    Log = simple_ocel_log(),
    Model = simple_model(),
    {ok, Alignment} = ocml_align:align_trace(Log, Model),
    ?assert(is_list(Alignment)),
    ?assert(length(Alignment) > 0).

align_trace_empty_test() ->
    Model = simple_model(),
    {ok, Alignment} = ocml_align:align_trace([], Model),
    ?assertEqual([], Alignment).

%%====================================================================
%% Fitness Computation Tests
%%====================================================================

compute_fitness_from_result_test() ->
    Result = #{fitness => 0.85, alignment => [], total_cost => 0.3},
    Fitness = ocml_align:compute_fitness(Result),
    ?assertEqual(0.85, Fitness).

compute_fitness_from_alignment_test() ->
    Alignment = [
        #{type => sync_move, activity => a, cost => 0.0},
        #{type => log_move, activity => b, cost => 1.0}
    ],
    Fitness = ocml_align:compute_fitness(Alignment),
    ?assert(Fitness >= 0.0 andalso Fitness =< 1.0).

compute_fitness_empty_alignment_test() ->
    Fitness = ocml_align:compute_fitness([]),
    ?assertEqual(1.0, Fitness).

%%====================================================================
%% Object Consistency Tests
%%====================================================================

compute_object_consistency_test() ->
    Log = simple_ocel_log(),
    Model = simple_model(),
    Consistency = ocml_align:compute_object_consistency(Log, Model),
    ?assert(Consistency >= 0.0 andalso Consistency =< 1.0).

compute_object_consistency_empty_test() ->
    Log = empty_log(),
    Model = simple_model(),
    Consistency = ocml_align:compute_object_consistency(Log, Model),
    ?assertEqual(1.0, Consistency).

%%====================================================================
%% Integration Tests
%%====================================================================

align_ocml_full_workflow_test() ->
    Log = full_workflow_log(),
    Model = simple_model(),
    {ok, Result} = ocml_align:align_ocml(Log, Model),
    ?assert(maps:is_key(fitness, Result)),
    ?assert(maps:get(fitness, Result) >= 0.0),
    ?assert(maps:get(fitness, Result) =< 1.0).

align_ocml_multi_object_test() ->
    Log = [
        #{id => <<"e1">>, activity => a, timestamp => 1000,
          objects => [{<<"order">>, <<"o1">>}, {<<"customer">>, <<"c1">>}]},
        #{id => <<"e2">>, activity => b, timestamp => 2000,
          objects => [{<<"order">>, <<"o1">>}, {<<"item">>, <<"i1">>}]},
        #{id => <<"e3">>, activity => c, timestamp => 3000,
          objects => [{<<"order">>, <<"o2">>}, {<<"customer">>, <<"c1">>}]}
    ],
    Model = simple_model(),
    {ok, Result} = ocml_align:align_ocml(Log, Model),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(fitness, Result)).

align_ocml_perfect_alignment_test() ->
    %% All activities are in the model
    Log = full_workflow_log(),
    Model = simple_model(),
    {ok, Result} = ocml_align:align_ocml(Log, Model),
    ?assertEqual(1.0, maps:get(fitness, Result)).

align_ocml_violations_test() ->
    %% Log has activities not in model
    Log = [
        #{id => <<"e1">>, activity => unknown_activity, timestamp => 1000,
          objects => [{<<"order">>, <<"o1">>}]}
    ],
    Model = simple_model(),
    {ok, Result} = ocml_align:align_ocml(Log, Model),
    ?assert(maps:get(fitness, Result) < 1.0).
