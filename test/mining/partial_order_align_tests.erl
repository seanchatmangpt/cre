%% -*- erlang -*-
%% @doc Partial Order Alignment Tests

-module(partial_order_align_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

simple_model() ->
    #{
        places => [p1, p2, p3, source, sink],
        transitions => [a, b, c],
        arcs => [
            {source, a}, {a, p1},
            {p1, b}, {b, p2},
            {p2, c}, {c, sink}
        ],
        initial_place => source,
        final_place => sink
    }.

parallel_model() ->
    #{
        places => [p1, p2, p3, p4, source, sink],
        transitions => [a, b, c, d],
        arcs => [
            {source, a}, {a, p1},
            {p1, b}, {b, p3},
            {p1, c}, {c, p3},
            {p3, d}, {d, sink}
        ],
        initial_place => source,
        final_place => sink
    }.

%%--------------------------------------------------------------------
%% Alignment tests
%%--------------------------------------------------------------------

align_trace_perfect_test() ->
    Trace = [a, b, c],
    Model = simple_model(),
    Result = partial_order_align:align_trace(Trace, Model),

    ?assert(maps:is_key(alignment, Result)),
    ?assert(maps:is_key(cost, Result)),
    ?assert(maps:is_key(fitness, Result)),

    #{alignment := Alignment, cost := Cost, fitness := Fitness} = Result,

    ?assert(is_list(Alignment)),
    ?assert(is_integer(Cost)),
    ?assert(Fitness >= 0.0),
    ?assert(Fitness =< 1.0).

align_trace_with_deviation_test() ->
    Trace = [a, x, c],
    Model = simple_model(),
    Result = partial_order_align:align_trace(Trace, Model),

    #{cost := Cost, fitness := Fitness} = Result,

    ?assert(Cost >= 0),
    ?assert(Fitness < 1.0).

align_trace_parallel_test() ->
    Trace = [a, b, c, d],
    Model = parallel_model(),
    Result = partial_order_align:align_trace(Trace, Model),

    ?assert(maps:is_key(alignment, Result)),
    ?assert(maps:is_key(fitness, Result)).

align_log_test() ->
    Log = [[a, b, c], [a, b, c]],
    Model = simple_model(),
    Results = partial_order_align:align_log(Log, Model),

    ?assertEqual(2, length(Results)),
    ?assert(lists:all(fun(R) -> maps:is_key(alignment, R) end, Results)).

%%--------------------------------------------------------------------
%% Partial order tests
%%--------------------------------------------------------------------

partial_order_from_trace_test() ->
    Trace = [a, b, c],
    PO = partial_order_align:partial_order_from_trace(Trace),
    ?assertEqual(3, length(PO)),
    Activities = [maps:get(activity, E) || E <- lists:reverse(PO)],
    ?assertEqual([a, b, c], Activities).

check_concurrency_sequential_test() ->
    Trace = [a, b, c],
    PO = partial_order_align:partial_order_from_trace(Trace),
    ?assertNot(partial_order_align:check_concurrency({a, b}, PO)),
    ?assertNot(partial_order_align:check_concurrency({b, c}, PO)).

check_concurrency_empty_test() ->
    PO = [],
    ?assertNot(partial_order_align:check_concurrency({a, b}, PO)).

%%--------------------------------------------------------------------
%% Fitness and precision tests
%%--------------------------------------------------------------------

compute_fitness_perfect_test() ->
    Result = #{
        alignment => [{sync_move, a}, {sync_move, b}, {sync_move, c}],
        cost => 0,
        trace => [a, b, c]
    },
    Fitness = partial_order_align:compute_fitness(Result),
    ?assertEqual(1.0, Fitness).

compute_fitness_with_errors_test() ->
    Result = #{
        alignment => [{sync_move, a}, {log_move, x}, {sync_move, c}],
        cost => 1,
        trace => [a, x, c]
    },
    Fitness = partial_order_align:compute_fitness(Result),
    ?assert(Fitness < 1.0),
    ?assert(Fitness > 0.0).

compute_precision_test() ->
    Log = [[a, b, c]],
    Model = simple_model(),
    Precision = partial_order_align:compute_precision(Log, Model),
    ?assert(Precision >= 0.0),
    ?assert(Precision =< 1.0).

compute_precision_empty_test() ->
    Log = [],
    Model = simple_model(),
    Precision = partial_order_align:compute_precision(Log, Model),
    ?assert(Precision >= 0.0).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

alignment_cost_test() ->
    ?assertEqual(0, partial_order_align:alignment_cost([
        {sync_move, a}, {sync_move, b}
    ])),
    ?assertEqual(1, partial_order_align:alignment_cost([
        {sync_move, a}, {log_move, b}
    ])),
    ?assertEqual(2, partial_order_align:alignment_cost([
        {log_move, a}, {model_move, b}
    ])),
    ?assertEqual(0, partial_order_align:alignment_cost([])).

count_moves_test() ->
    Alignment = [{sync_move, a}, {log_move, b}, {model_move, c}, {sync_move, d}],
    Counts = partial_order_align:count_moves(Alignment),
    ?assertEqual(1, maps:get(log_moves, Counts)),
    ?assertEqual(1, maps:get(model_moves, Counts)),
    ?assertEqual(2, maps:get(sync_moves, Counts)).

count_moves_empty_test() ->
    Alignment = [],
    Counts = partial_order_align:count_moves(Alignment),
    ?assertEqual(0, maps:get(log_moves, Counts)),
    ?assertEqual(0, maps:get(model_moves, Counts)),
    ?assertEqual(0, maps:get(sync_moves, Counts)).

trace_to_partial_order_test() ->
    ?assertEqual(partial_order_align:partial_order_from_trace([a, b]),
                 partial_order_align:trace_to_partial_order([a, b])).

merge_alignments_test() ->
    A1 = [{sync_move, a}],
    A2 = [{log_move, b}],
    Merged = partial_order_align:merge_alignments(A1, A2),
    ?assertEqual([{sync_move, a}, {log_move, b}], Merged).

merge_alignments_empty_test() ->
    Merged = partial_order_align:merge_alignments([], []),
    ?assertEqual([], Merged).

%%--------------------------------------------------------------------
%% Model analysis tests
%%--------------------------------------------------------------------

simple_model_consistency_test() ->
    Model = simple_model(),
    ?assert(maps:is_key(places, Model)),
    ?assert(maps:is_key(transitions, Model)),
    ?assert(maps:is_key(arcs, Model)),
    ?assert(maps:is_key(initial_place, Model)),
    ?assert(maps:is_key(final_place, Model)).

parallel_model_consistency_test() ->
    Model = parallel_model(),
    ?assert(maps:is_key(places, Model)),
    ?assert(lists:member(b, maps:get(transitions, Model))),
    ?assert(lists:member(c, maps:get(transitions, Model))).

%%--------------------------------------------------------------------
%% Concurrency tests
%%--------------------------------------------------------------------

extract_concurrency_sequential_test() ->
    Trace = [a, b, c],
    PO = partial_order_align:partial_order_from_trace(Trace),
    Concurrency = partial_order_align:extract_concurrency(PO),
    ?assert(is_set(Concurrency)),
    ?assertEqual(0, sets:size(Concurrency)).

%%--------------------------------------------------------------------
%% Edge case tests
%%--------------------------------------------------------------------

align_trace_empty_test() ->
    Trace = [],
    Model = simple_model(),
    Result = partial_order_align:align_trace(Trace, Model),
    ?assert(maps:is_key(alignment, Result)),
    ?assertEqual([], maps:get(trace, Result)).

align_trace_single_activity_test() ->
    Trace = [a],
    Model = simple_model(),
    Result = partial_order_align:align_trace(Trace, Model),
    ?assert(maps:is_key(fitness, Result)).
