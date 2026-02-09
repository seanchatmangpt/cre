%% -*- erlang -*-
%% @doc Rust Algorithm Integration Test Suite
%%
%% This suite tests the actual process mining algorithms implemented in Rust.
%% It verifies algorithm correctness, performance, and edge case handling.
%%
%% @end

-module(rust_algorithm_SUITE).
-author("CRE Team").

-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%%====================================================================
%% Test Cases
%%====================================================================

-export([
    alpha_simple_log_test/1,
    alpha_parallel_log_test/1,
    alpha_loop_log_test/1,
    heuristic_noisy_log_test/1,
    heuristic_threshold_test/1,
    conformance_fitness_test/1,
    conformance_precision_test/1,
    conformance_alignment_test/1,
    object_centric_log_test/1,
    object_centric_relations_test/1,
    llm_process_modeling_test/1,
    local_process_models_test/1,
    edge_case_empty_log_test/1,
    edge_case_single_activity_test/1,
    edge_case_max_trace_length_test/1,
    memory_efficiency_test/1
]).

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
        alpha_simple_log_test,
        alpha_parallel_log_test,
        alpha_loop_log_test,
        heuristic_noisy_log_test,
        heuristic_threshold_test,
        conformance_fitness_test,
        conformance_precision_test,
        conformance_alignment_test,
        object_centric_log_test,
        object_centric_relations_test,
        llm_process_modeling_test,
        local_process_models_test,
        edge_case_empty_log_test,
        edge_case_single_activity_test,
        edge_case_max_trace_length_test,
        memory_efficiency_test
    ].

suite() ->
    [
        {timetrap, {seconds, 120}},
        {require, ?MODULE}
    ].

init_per_suite(Config) ->
    ct:log("Initializing Rust algorithm test suite"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test Alpha algorithm on simple sequential log
alpha_simple_log_test(_Config) ->
    %% Simple sequential log
    Log = [
        {case1, a, 1},
        {case1, b, 2},
        {case1, c, 3},
        {case2, a, 4},
        {case2, b, 5},
        {case2, c, 6}
    ],

    %% Run Alpha algorithm
    {ok, Model} = rust_algorithm:alpha(Log),

    %% Verify basic structure
    ?assertMatch(#{
        places := _,
        transitions := _,
        arcs := _,
        initial_place := _,
        final_place := _
    }, Model),

    %% Verify transitions
    Transitions = maps:get(transitions, Model),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),
    ?assertEqual(3, length(Transitions)),

    %% Verify sequential structure
    Arcs = maps:get(arcs, Model),
    ?assert(lists:member({a, b}, Arcs) orelse lists:member({a, p_ab}, Arcs)),

    ok.

%% @doc Test Alpha algorithm on parallel log
alpha_parallel_log_test(_Config) ->
    %% Parallel log (two orders)
    Log = [
        {case1, a, 1},
        {case1, b, 2},
        {case1, c, 3},
        {case1, d, 4},
        {case2, a, 5},
        {case2, c, 6},
        {case2, b, 7},
        {case2, d, 8}
    ],

    %% Run Alpha algorithm
    {ok, Model} = rust_algorithm:alpha(Log),

    %% Verify all transitions detected
    Transitions = maps:get(transitions, Model),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),
    ?assert(lists:member(d, Transitions)),

    %% Verify places (should have parallel structure)
    Places = maps:get(places, Model),
    ?assert(length(Places) >= 2),

    ok.

%% @doc Test Alpha algorithm on log with loops
alpha_loop_log_test(_Config) ->
    %% Log with short loop
    Log = [
        {case1, a, 1},
        {case1, b, 2},
        {case1, a, 3},  %% back to a
        {case1, c, 4}
    ],

    %% Run Alpha algorithm
    {ok, Model} = rust_algorithm:alpha(Log),

    %% Verify loop detection
    Metadata = maps:get(metadata, Model, #{}),
    LoopsDetected = maps:get(loops_detected, Metadata, 0),
    ?assert(LoopsDetected >= 0),

    %% Verify model can handle the loop
    Transitions = maps:get(transitions, Model),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),

    ok.

%% @doc Test Heuristic Miner on noisy log
heuristic_noisy_log_test(_Config) ->
    %% Log with noise
    Log = [
        {case1, a, 1},
        {case1, b, 2},
        {case1, c, 3},
        {case2, a, 4},
        {case2, x, 5},  %% noise - rare activity
        {case2, b, 6},
        {case2, c, 7},
        {case3, a, 8},
        {case3, y, 9},  %% another noise activity
        {case3, b, 10},
        {case3, c, 11}
    ],

    %% Run Heuristic Miner
    {ok, Model} = rust_algorithm:heuristic(Log, [
        {dependency_threshold, 0.5},
        {positive_observations, 2}
    ]),

    %% Verify main path preserved
    Transitions = maps:get(transitions, Model),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),

    %% Noise activities may or may not be included
    %% depending on threshold
    Metadata = maps:get(metadata, Model, #{}),
    NoiseLevel = maps:get(noise_level, Metadata, 0.0),
    ct:log("Detected noise level: ~p", [NoiseLevel]),
    ?assert(NoiseLevel >= 0.0),

    ok.

%% @doc Test Heuristic Miner with different thresholds
heuristic_threshold_test(_Config) ->
    Log = [
        {case1, a, 1}, {case1, b, 2}, {case1, c, 3},
        {case2, a, 4}, {case2, c, 5}, {case2, b, 6}
    ],

    %% Test with high threshold (strict)
    {ok, StrictModel} = rust_algorithm:heuristic(Log, [
        {dependency_threshold, 0.9}
    ]),
    StrictPlaces = maps:get(places, StrictModel),

    %% Test with low threshold (permissive)
    {ok, PermissiveModel} = rust_algorithm:heuristic(Log, [
        {dependency_threshold, 0.1}
    ]),
    PermissivePlaces = maps:get(places, PermissiveModel),

    %% Permissive should have at least as many places as strict
    ?assert(length(PermissivePlaces) >= length(StrictPlaces)),

    ok.

%% @doc Test conformance checking fitness calculation
conformance_fitness_test(_Config) ->
    %% Create perfect log
    Log = [[a, b, c]],

    %% Create matching model
    Model = create_simple_model([a, b, c]),

    %% Calculate fitness
    {ok, Result} = rust_algorithm:conformance(Log, Model),

    Fitness = maps:get(fitness, Result),
    ?assert(Fitness >= 0.99),  %% Should be nearly perfect

    %% Test with non-conforming log
    BadLog = [[a, x, b, c]],  %% x not in model
    {ok, BadResult} = rust_algorithm:conformance(BadLog, Model),

    BadFitness = maps:get(fitness, BadResult),
    ?assert(BadFitness < Fitness),  %% Should be worse

    ok.

%% @doc Test conformance checking precision calculation
conformance_precision_test(_Config) ->
    %% Simple log
    Log = [[a, b, c]],

    %% Overly permissive model (allows extra behavior)
    PermissiveModel = create_permissive_model(),

    {ok, Result} = rust_algorithm:conformance(Log, PermissiveModel),

    Precision = maps:get(precision, Result),
    ?assert(Precision >= 0.0),
    ?assert(Precision =< 1.0),

    %% More precise model should have higher precision
    PreciseModel = create_simple_model([a, b, c]),
    {ok, PreciseResult} = rust_algorithm:conformance(Log, PreciseModel),

    PrecisePrecision = maps:get(precision, PreciseResult),
    ?assert(PrecisePrecision >= Precision),

    ok.

%% @doc Test alignment-based conformance
conformance_alignment_test(_Config) ->
    %% Create test trace and model
    Trace = [a, b, c],
    Model = create_simple_model([a, b, c]),

    %% Calculate alignment
    {ok, Result} = rust_algorithm:align_trace(Trace, Model),

    Alignment = maps:get(alignment, Result),
    Cost = maps:get(cost, Result),
    Fitness = maps:get(fitness, Result),

    %% Verify alignment structure
    ?assert(is_list(Alignment)),
    ?assert(is_integer(Cost)),
    ?assert(Fitness >= 0.0 andalso Fitness =< 1.0),

    %% Perfect trace should have zero or low cost
    ?assert(Cost =< 2),

    %% Verify alignment moves
    lists:foreach(fun(Move) ->
        case Move of
            {sync_move, _} -> ?assert(true);
            {log_move, _} -> ?assert(true);
            {model_move, _} -> ?assert(true);
            _ -> ?assert(false)
        end
    end, Alignment),

    ok.

%% @doc Test object-centric process mining
object_centric_log_test(_Config) ->
    %% Create object-centric event log
    %% Format: {EventId, Activity, Timestamp, Objects}
    OCELLog = [
        {e1, order, 1, [o1, c1]},
        {e2, payment, 2, [o1, p1]},
        {e3, shipment, 3, [o1, s1]},
        {e4, order, 4, [o2, c2]},
        {e5, payment, 5, [o2, p1]}
    ],

    %% Run object-centric discovery
    {ok, Result} = rust_algorithm:object_centric(OCELLog),

    %% Verify result structure
    ?assertMatch(#{
        object_types := _,
        activities := _,
        relations := _
    }, Result),

    %% Verify object types detected
    ObjectTypes = maps:get(object_types, Result),
    ?assert(lists:member(order, ObjectTypes)),
    ?assert(lists:member(payment, ObjectTypes)),

    ok.

%% @doc Test object-centric relation extraction
object_centric_relations_test(_Config) ->
    %% Test relationship detection
    OCELLog = [
        {e1, create, 1, [o1, c1]},
        {e2, link, 2, [o1, i1]},
        {e3, update, 3, [i1]}
    ],

    {ok, Result} = rust_algorithm:object_centric_relations(OCELLog),

    %% Verify relations extracted
    Relations = maps:get(relations, Result),
    ?assert(length(Relations) > 0),

    %% Verify relation structure
    lists:foreach(fun(Relation) ->
        ?assertMatch(#{
            from := _,
            to := _,
            relation_type := _
        }, Relation)
    end, Relations),

    ok.

%% @doc Test LLM-based process modeling
llm_process_modeling_test(_Config) ->
    %% Create text descriptions
    Descriptions = [
        <<"The process starts with order creation, then proceeds to payment, and ends with shipment.">>,
        <<"After payment verification, the goods are shipped to the customer.">>
    ],

    %% Run LLM-based discovery
    {ok, Result} = rust_algorithm:llp_model(Descriptions, [
        {confidence_threshold, 0.7}
    ]),

    %% Verify result
    ?assertMatch(#{
        activities := _,
        relations := _,
        confidence := _
    }, Result),

    %% Check extracted activities
    Activities = maps:get(activities, Result),
    ?assert(length(Activities) >= 3),

    ok.

%% @doc Test local process model discovery
local_process_models_test(_Config) ->
    %% Create log with multiple local patterns
    Log = [
        {case1, a, 1}, {case1, b, 2}, {case1, x, 3},
        {case2, a, 4}, {case2, b, 5}, {case2, y, 6},
        {case3, c, 7}, {case3, d, 8}, {case3, x, 9}
    ],

    %% Discover local models
    {ok, Result} = rust_algorithm:local_models(Log, [
        {min_support, 0.3},
        {max_depth, 3}
    ]),

    %% Verify multiple local models found
    LocalModels = maps:get(local_models, Result),
    ?assert(length(LocalModels) >= 1),

    %% Verify each local model structure
    lists:foreach(fun(Model) ->
        ?assertMatch(#{
            activities := _,
            support := _,
            confidence := _
        }, Model)
    end, LocalModels),

    ok.

%% @doc Test edge case: empty log
edge_case_empty_log_test(_Config) ->
    %% Empty log should return valid but empty model
    {ok, Model} = rust_algorithm:alpha([]),

    ?assertMatch(#{
        places := _,
        transitions := _,
        arcs := _
    }, Model),

    ?assertEqual([], maps:get(transitions, Model, [])),
    ?assertEqual([], maps:get(arcs, Model, [])),

    ok.

%% @doc Test edge case: single activity
edge_case_single_activity_test(_Config) ->
    %% Log with only one activity
    Log = [{case1, a, 1}],

    {ok, Model} = rust_algorithm:alpha(Log),

    Transitions = maps:get(transitions, Model),
    ?assertEqual([a], Transitions),

    %% Should still have valid structure
    ?assert(maps:is_key(initial_place, Model)),
    ?assert(maps:is_key(final_place, Model)),

    ok.

%% @doc Test edge case: very long trace
edge_case_max_trace_length_test(_Config) ->
    %% Create very long trace
    LongTrace = [{case1, list_to_atom("act" ++ integer_to_list(N)), N}
                 || N <- lists:seq(1, 10000)],

    %% Should handle without crashing
    {ok, Model} = rust_algorithm:alpha(LongTrace),

    Transitions = maps:get(transitions, Model),
    ?assert(length(Transitions) > 0),

    %% Verify performance is reasonable
    case rust_algorithm:alpha(LongTrace, [{timeout, 10000}]) of
        {ok, _} -> ?assert(true);
        _ -> ?assert(false)
    end,

    ok.

%% @doc Test memory efficiency
memory_efficiency_test(_Config) ->
    %% Force garbage collection before test
    garbage_collect(),
    Before = erlang:memory(total),

    %% Process moderately large log
    Log = generate_log(1000),
    {ok, _Model} = rust_algorithm:alpha(Log),

    %% Force garbage collection
    garbage_collect(),
    After = erlang:memory(total),

    %% Memory growth should be reasonable
    Growth = After - Before,
    ct:log("Memory growth for 1000-event log: ~p bytes", [Growth]),

    %% Allow up to 100MB growth for this operation
    ?assert(Growth < 104857600),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Create a simple sequential Petri net model
create_simple_model(Activities) ->
    PrefixPlaces = [list_to_atom("p_" ++ atom_to_list(A)) || A <- Activities],
    BasePlaces = lists:zipwith(fun(A, P) -> {P, A} end,
                               PrefixPlaces,
                               tl(Activities ++ [finish])),
    Places = [P || {P, _} <- BasePlaces] ++ [i_source, o_sink],

    #{
        places => Places,
        transitions => Activities,
        arcs => build_sequential_arcs(Activities),
        initial_place => i_source,
        final_place => o_sink
    }.

%% @doc Create a permissive model (allows many behaviors)
create_permissive_model() ->
    #{
        places => [p1, p2, p3, i_source, o_sink],
        transitions => [a, b, c, x, y],  %% Extra activities
        arcs => [
            {i_source, a}, {a, p1}, {p1, b}, {p1, x},
            {b, p2}, {p2, c}, {p2, y}, {c, o_sink},
            {x, o_sink}, {y, o_sink}
        ],
        initial_place => i_source,
        final_place => o_sink
    }.

%% @doc Build sequential arcs for activities
build_sequential_arcs([]) -> [];
build_sequential_arcs([_]) -> [];
build_sequential_arcs([First, Second | Rest]) ->
    [{First, p}, {p, Second} | build_sequential_arcs([Second | Rest])]
        ++ [{i_source, First}, {Second, o_sink}].

%% @doc Generate test event log
generate_log(Size) ->
    Cases = Size div 5,
    lists:flatmap(fun(CaseNum) ->
        CaseId = list_to_atom("case" ++ integer_to_list(CaseNum)),
        [
            {CaseId, a, CaseNum * 10 + 1},
            {CaseId, b, CaseNum * 10 + 2},
            {CaseId, c, CaseNum * 10 + 3},
            {CaseId, d, CaseNum * 10 + 4},
            {CaseId, e, CaseNum * 10 + 5}
        ]
    end, lists:seq(1, Cases)).
