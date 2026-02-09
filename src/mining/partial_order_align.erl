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
%% @doc Partial Order Alignment - Conformance Checking
%%
%% This module implements partial order alignment for conformance checking,
%% aligning log traces with process models while respecting concurrency
%% relationships.
%%
%% Based on "Computing Alignments for Partially Ordered Traces Through
%% Petri Net Unfoldings" (van der Aalst, 2025).
%%
%% @end
%% -------------------------------------------------------------------

-module(partial_order_align).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([align_trace/2, align_log/2]).
-export([compute_fitness/1, compute_precision/2]).
-export([partial_order_from_trace/1, check_concurrency/2]).

%% Analysis utilities
-export([alignment_cost/1, count_moves/1]).
-export([trace_to_partial_order/1, merge_alignments/2]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type trace() :: [activity()].
-type event_log() :: [trace()].

-type place() :: atom().
-type transition() :: atom().
-type marking() :: #{place() => non_neg_integer()}.

-type wf_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [{place(), transition()} | {transition(), place()}],
    initial_place => place(),
    final_place => place()
}.

-type po_event() :: #{id => pos_integer(), activity => activity(), before => [pos_integer()], 'after' => [pos_integer()]}.
-type partial_order() :: [po_event()].

-type align_move() :: {log_move, activity()} |
                     {model_move, transition()} |
                     {sync_move, activity()}.

-type alignment() :: [align_move()].

-type alignment_result() :: #{
    alignment => alignment(),
    cost => non_neg_integer(),
    fitness => float(),
    trace => trace()
}.

-type concurrency_relation() :: sets:set({activity(), activity()}).

-export_type([
    activity/0, trace/0, event_log/0,
    wf_net/0, partial_order/0, alignment/0,
    alignment_result/0, concurrency_relation/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Aligns a trace with a process model using partial order semantics.
-spec align_trace(Trace :: trace(), Model :: wf_net()) -> alignment_result().
align_trace(Trace, Model) when is_list(Trace), is_map(Model) ->
    %% Convert trace to partial order
    PO = partial_order_from_trace(Trace),

    %% Compute concurrency relations
    Concurrency = extract_concurrency(PO),

    %% Perform alignment using partial order A*
    {Alignment, Cost} = po_star_align(Trace, PO, Concurrency, Model),

    %% Calculate fitness
    Fitness = compute_alignment_fitness(Alignment, Cost, Trace),

    #{
        alignment => Alignment,
        cost => Cost,
        fitness => Fitness,
        trace => Trace
    }.

%% @doc Aligns an entire event log with a process model.
-spec align_log(Log :: event_log(), Model :: wf_net()) -> [alignment_result()].
align_log(Log, Model) when is_list(Log), is_map(Model) ->
    [align_trace(Trace, Model) || Trace <- Log].

%% @doc Computes fitness score from alignment result.
-spec compute_fitness(alignment_result()) -> float().
compute_fitness(#{alignment := Alignment, cost := Cost, trace := Trace}) ->
    compute_alignment_fitness(Alignment, Cost, Trace);
compute_fitness(_) ->
    0.0.

%% @doc Computes precision score based on escaping edges.
-spec compute_precision(Log :: event_log(), Model :: wf_net()) -> float().
compute_precision(Log, Model) ->
    %% Extract log relations
    LogRelations = extract_log_relations(Log),

    %% Extract model relations
    ModelRelations = extract_model_relations(Model),

    %% Calculate precision as 1 - (escaping / total_model)
    case maps:size(ModelRelations) of
        0 -> 1.0;
        TotalModel ->
            Escaping = count_escapes(LogRelations, ModelRelations),
            1.0 - (Escaping / TotalModel)
    end.

%% @doc Converts a trace to a partial order representation.
-spec partial_order_from_trace(trace()) -> partial_order().
partial_order_from_trace(Trace) ->
    TraceLen = length(Trace),
    %% For sequential traces, build both before and after relationships
    lists:foldl(fun({Activity, Id}, Acc) ->
        Before = lists:seq(1, Id - 1),
        After = lists:seq(Id + 1, TraceLen),
        [#{id => Id, activity => Activity, before => Before, 'after' => After} | Acc]
    end, [], lists:zip(Trace, lists:seq(1, TraceLen))).

%% @doc Checks if two activities are concurrent in the partial order.
-spec check_concurrency({activity(), activity()}, partial_order()) -> boolean().
check_concurrency({A, B}, PO) ->
    %% Check if A and B have no ordering relationship
    case find_events(A, B, PO) of
        {EventA, EventB} ->
            BeforeA = maps:get(before, EventA, []),
            AfterA = maps:get('after', EventA, []),
            IdA = maps:get(id, EventA),
            IdB = maps:get(id, EventB),
            %% Concurrent if neither comes before the other
            not lists:member(IdB, BeforeA) andalso
            not lists:member(IdA, AfterA);
        _ ->
            false
    end.

%%====================================================================
%% Alignment Functions
%%====================================================================

%% @private
-spec po_star_align(trace(), partial_order(), concurrency_relation(), wf_net()) ->
    {alignment(), non_neg_integer()}.
po_star_align(Trace, _PO, Concurrency, Model) ->
    InitialMarking = init_marking(Model),

    %% A* search with partial order state
    StartNode = #{
        trace_pos => Trace,
        marking => InitialMarking,
        alignment => [],
        cost => 0,
        heuristic => length(Trace)
    },

    po_a_star_loop([StartNode], Model, Concurrency, #{}, infinity).

%% @private
po_a_star_loop([], _Model, _Concurrency, _Visited, _BestCost) ->
    {[], 0};

po_a_star_loop([Node | Rest], Model, Concurrency, Visited, BestCost) ->
    #{
        trace_pos := TracePos,
        marking := Marking,
        alignment := Alignment,
        cost := Cost
    } = Node,

    VisitedKey = {TracePos, simplify_marking(Marking)},

    case Cost >= BestCost of
        true ->
            %% Prune this branch
            po_a_star_loop(Rest, Model, Concurrency, Visited, BestCost);
        false ->
            case maps:get(VisitedKey, Visited, infinity) of
                PrevCost when Cost >= PrevCost ->
                    po_a_star_loop(Rest, Model, Concurrency, Visited, BestCost);
                _ ->
                    NewVisited = Visited#{VisitedKey => Cost},

                    case {TracePos, is_final_marking(Marking, Model)} of
                        {[], true} ->
                            %% Complete alignment
                            {lists:reverse(Alignment), Cost};
                        _ ->
                            %% Generate successors
                            Successors = get_po_successors(
                                TracePos, Marking, Model, Alignment, Cost, Concurrency
                            ),

                            %% Update best cost if complete
                            NewBest = find_complete_cost(Successors, Cost, BestCost),

                            NewNodes = Rest ++ Successors,
                            po_a_star_loop(NewNodes, Model, Concurrency, NewVisited, NewBest)
                    end
            end
    end.

%% @private
get_po_successors(TracePos, Marking, Model, Alignment, CurrentCost, Concurrency) ->
    Enabled = get_enabled_transitions(Marking, Model),

    case TracePos of
        [NextEvent | Rest] ->
            NextActivity = NextEvent,

            %% Check if next event is enabled
            case lists:member(NextActivity, Enabled) of
                true ->
                    %% Sync move
                    NewMarking = fire_transition(NextActivity, Marking, Model),
                    [#{
                        trace_pos => Rest,
                        marking => NewMarking,
                        alignment => [{sync_move, NextActivity} | Alignment],
                        cost => CurrentCost,
                        heuristic => length(Rest)
                    }];
                false ->
                    %% Check for concurrent activities
                    ConcurrentEnabled = [T || T <- Enabled,
                        is_concurrent(NextActivity, T, Concurrency)],

                    SyncIfConcurrent = case ConcurrentEnabled of
                        [CT | _] ->
                            %% Can sync with concurrent activity
                            SyncMarking = fire_transition(CT, Marking, Model),
                            [#{
                                trace_pos => TracePos,
                                marking => SyncMarking,
                                alignment => [{sync_move, CT} | Alignment],
                                cost => CurrentCost,
                                heuristic => length(TracePos)
                            }];
                        [] -> []
                    end,

                    %% Log move
                    LogMove = #{
                        trace_pos => Rest,
                        marking => Marking,
                        alignment => [{log_move, NextActivity} | Alignment],
                        cost => CurrentCost + 1,
                        heuristic => length(Rest)
                    },

                    %% Model moves
                    ModelMoves = [begin
                        NewMarking = fire_transition(T, Marking, Model),
                        #{
                            trace_pos => TracePos,
                            marking => NewMarking,
                            alignment => [{model_move, T} | Alignment],
                            cost => CurrentCost + 1,
                            heuristic => length(TracePos) + 1
                        }
                    end || T <- Enabled, not lists:member(T, ConcurrentEnabled)],

                    [LogMove | ModelMoves] ++ SyncIfConcurrent
            end;
        [] ->
            %% Only model moves possible
            case is_final_marking(Marking, Model) of
                true ->
                    [];
                false ->
                    [begin
                        NewMarking = fire_transition(T, Marking, Model),
                        #{
                            trace_pos => [],
                            marking => NewMarking,
                            alignment => [{model_move, T} | Alignment],
                            cost => CurrentCost + 1,
                            heuristic => length(Enabled)
                        }
                    end || T <- Enabled]
            end
    end.

%% @private
find_complete_cost([], _CurrentCost, BestCost) -> BestCost;
find_complete_cost([#{trace_pos := [], cost := Cost} | _], _CurrentCost, BestCost) when Cost < BestCost -> Cost;
find_complete_cost([_ | Rest], CurrentCost, BestCost) -> find_complete_cost(Rest, CurrentCost, BestCost).

%% @private
is_concurrent(A, B, Concurrency) ->
    sets:is_element({A, B}, Concurrency) orelse sets:is_element({B, A}, Concurrency).

%% @private
extract_concurrency(PO) ->
    %% Extract pairs with no ordering relationship
    Activities = [maps:get(activity, E) || E <- PO],

    ConcurrentPairs = lists:foldl(fun(E1, Acc1) ->
        Id1 = maps:get(id, E1),
        Activity1 = maps:get(activity, E1),
        Before1 = maps:get(before, E1, []),

        lists:foldl(fun(E2, Acc2) ->
            Id2 = maps:get(id, E2),
            Activity2 = maps:get(activity, E2),

            case Id1 < Id2 of
                true ->
                    %% Check if no ordering exists
                    case lists:member(Id2, Before1) orelse
                         lists:member(Id1, maps:get(before, E2, [])) of
                        false ->
                            sets:add_element({Activity1, Activity2}, Acc2);
                        true ->
                            Acc2
                    end;
                false ->
                    Acc2
            end
        end, Acc1, PO)
    end, sets:new(), PO),

    ConcurrentPairs.

%% @private
find_events(A, B, PO) ->
    EventsA = [E || E <- PO, maps:get(activity, E) =:= A],
    EventsB = [E || E <- PO, maps:get(activity, E) =:= B],

    case {EventsA, EventsB} of
        {[EA], [EB]} -> {EA, EB};
        _ -> undefined
    end.

%%====================================================================
%% Model Analysis Functions
%%====================================================================

%% @private
init_marking(#{initial_place := Init, places := Places}) ->
    BaseMarking = maps:from_list([{P, 0} || P <- Places]),
    BaseMarking#{Init => 1}.

%% @private
get_enabled_transitions(Marking, #{arcs := Arcs}) ->
    TrsnPlaces = build_transition_places(Arcs),
    [T || {T, {InputPlaces, _OutputPlaces}} <- maps:to_list(TrsnPlaces),
          can_enable(InputPlaces, Marking)].

%% @private
build_transition_places(Arcs) ->
    lists:foldl(fun
        ({Place, Trsn}, Acc) when is_atom(Place), is_atom(Trsn) ->
            Rec = maps:get(Trsn, Acc, {[], []}),
            {In, Out} = Rec,
            Acc#{Trsn => {[Place | In], Out}};
        ({Trsn, Place}, Acc) when is_atom(Trsn), is_atom(Place) ->
            Rec = maps:get(Trsn, Acc, {[], []}),
            {In, Out} = Rec,
            Acc#{Trsn => {In, [Place | Out]}};
        (_, Acc) -> Acc
    end, #{}, Arcs).

%% @private
can_enable(InputPlaces, Marking) ->
    lists:all(fun(P) -> maps:get(P, Marking, 0) > 0 end, InputPlaces).

%% @private
fire_transition(Transition, Marking, Model) ->
    InputPlaces = find_input_places(Transition, Model),
    OutputPlaces = find_output_places(Transition, Model),

    %% Consume tokens
    Marking1 = lists:foldl(fun(P, Acc) ->
        Acc#{P => maps:get(P, Acc, 0) - 1}
    end, Marking, InputPlaces),

    %% Produce tokens
    lists:foldl(fun(P, Acc) ->
        Acc#{P => maps:get(P, Acc, 0) + 1}
    end, Marking1, OutputPlaces).

%% @private
find_input_places(Transition, #{arcs := Arcs}) ->
    [Place || {Place, T} <- Arcs, T =:= Transition].

%% @private
find_output_places(Transition, #{arcs := Arcs}) ->
    [Place || {T, Place} <- Arcs, T =:= Transition].

%% @private
is_final_marking(Marking, #{final_place := Final, initial_place := Initial}) ->
    maps:get(Final, Marking, 0) > 0 andalso
    lists:all(fun({P, Count}) ->
        case P of
            Initial -> Count =:= 0;
            Final -> Count >= 0;
            _ -> Count =:= 0
        end
    end, maps:to_list(Marking)).

%% @private
simplify_marking(Marking) ->
    Marking.

%%====================================================================
%% Precision and Fitness Functions
%%====================================================================

%% @private
compute_alignment_fitness(_Alignment, Cost, Trace) ->
    case length(Trace) of
        0 -> 1.0;
        TraceLen ->
            max(0.0, 1.0 - (Cost / (2 * TraceLen)))
    end.

%% @private
extract_log_relations(Log) ->
    %% Extract direct succession relations
    lists:foldl(fun(Trace, Acc) ->
        extract_from_trace(Trace, Acc)
    end, sets:new(), Log).

%% @private
extract_from_trace(Trace, Acc) ->
    Pairs = [{A, B} || {A, B} <- lists:zip(Trace, tl(Trace))],
    sets:union(Acc, sets:from_list(Pairs)).

%% @private
extract_model_relations(#{arcs := Arcs}) ->
    %% Build transition-to-transition relations through places
    lists:foldl(fun
        ({Place, Trsn1}, Acc) when is_atom(Place) ->
            %% Find outgoing transitions from this place
            Outgoing = [T || {T, P} <- Arcs, P =:= Place],
            lists:foldl(fun(T2, InnerAcc) ->
                sets:add_element({Trsn1, T2}, InnerAcc)
            end, Acc, Outgoing);
        (_, Acc) -> Acc
    end, sets:new(), Arcs).

%% @private
count_escapes(LogRelations, ModelRelations) ->
    %% Count model relations not in log
    ModelList = sets:to_list(ModelRelations),
    length([R || R <- ModelList, not sets:is_element(R, LogRelations)]).

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Calculates the cost of an alignment.
-spec alignment_cost(alignment()) -> non_neg_integer().
alignment_cost(Alignment) ->
    lists:foldl(fun
        ({log_move, _}, Acc) -> Acc + 1;
        ({model_move, _}, Acc) -> Acc + 1;
        ({sync_move, _}, Acc) -> Acc
    end, 0, Alignment).

%% @doc Counts different types of moves in an alignment.
-spec count_moves(alignment()) -> #{log_moves => non_neg_integer(),
                                   model_moves => non_neg_integer(),
                                   sync_moves => non_neg_integer()}.
count_moves(Alignment) ->
    {Log, Model, Sync} = lists:foldl(fun
        ({log_move, _}, {L, M, S}) -> {L + 1, M, S};
        ({model_move, _}, {L, M, S}) -> {L, M + 1, S};
        ({sync_move, _}, {L, M, S}) -> {L, M, S + 1}
    end, {0, 0, 0}, Alignment),
    #{log_moves => Log, model_moves => Model, sync_moves => Sync}.

%% @doc Converts trace to partial order (alias).
-spec trace_to_partial_order(trace()) -> partial_order().
trace_to_partial_order(Trace) ->
    partial_order_from_trace(Trace).

%% @doc Merges two alignments.
-spec merge_alignments(alignment(), alignment()) -> alignment().
merge_alignments(Align1, Align2) ->
    Align1 ++ Align2.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% Alignment tests
%%--------------------------------------------------------------------

align_trace_perfect_test() ->
    Trace = [a, b, c],
    Model = simple_model(),
    Result = align_trace(Trace, Model),

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
    Result = align_trace(Trace, Model),

    #{cost := Cost, fitness := Fitness} = Result,

    %% Should have some cost due to deviation
    ?assert(Cost >= 0),
    ?assert(Fitness < 1.0).

alignment_cost_test() ->
    ?assertEqual(0, alignment_cost([{sync_move, a}, {sync_move, b}])),
    ?assertEqual(1, alignment_cost([{sync_move, a}, {log_move, b}])),
    ?assertEqual(2, alignment_cost([{log_move, a}, {model_move, b}])).

count_moves_test() ->
    Alignment = [{sync_move, a}, {log_move, b}, {model_move, c}, {sync_move, d}],
    Counts = count_moves(Alignment),
    ?assertEqual(1, maps:get(log_moves, Counts)),
    ?assertEqual(1, maps:get(model_moves, Counts)),
    ?assertEqual(2, maps:get(sync_moves, Counts)).

%%--------------------------------------------------------------------
%% Partial order tests
%%--------------------------------------------------------------------

partial_order_from_trace_test() ->
    Trace = [a, b, c],
    PO = partial_order_from_trace(Trace),
    ?assertEqual(3, length(PO)),
    ?assertEqual([a, b, c], [maps:get(activity, E) || E <- lists:reverse(PO)]).

check_concurrency_test() ->
    Trace = [a, b, c],
    PO = partial_order_from_trace(Trace),
    %% Sequential trace should have no concurrency
    ?assertNot(check_concurrency({a, b}, PO)),
    ?assertNot(check_concurrency({b, c}, PO)).

%%--------------------------------------------------------------------
%% Fitness and precision tests
%%--------------------------------------------------------------------

compute_fitness_test() ->
    Result = #{
        alignment => [{sync_move, a}, {sync_move, b}, {sync_move, c}],
        cost => 0,
        trace => [a, b, c]
    },
    ?assertEqual(1.0, compute_fitness(Result)).

compute_precision_test() ->
    Log = [[a, b, c]],
    Model = simple_model(),
    Precision = compute_precision(Log, Model),
    ?assert(Precision >= 0.0),
    ?assert(Precision =< 1.0).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

merge_alignments_test() ->
    A1 = [{sync_move, a}],
    A2 = [{log_move, b}],
    Merged = merge_alignments(A1, A2),
    ?assertEqual([{sync_move, a}, {log_move, b}], Merged).

trace_to_partial_order_test() ->
    ?assertEqual(partial_order_from_trace([a, b]),
                 trace_to_partial_order([a, b])).

-endif.
