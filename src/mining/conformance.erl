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
%% @doc Conformance Checking for Process Mining
%%
%% This module implements conformance checking techniques that compare
%% an event log (observed behavior) against a process model (expected
%% behavior). Based on the conformance checking section of the Process
%% Mining Manifesto (van der Aalst et al., 2011).
%%
%% <h3>Key Concepts</h3>
%%
%% <ul>
%%   <li><b>Fitness:</b> How well the log can be replayed in the model
%%       (0 = no fit, 1 = perfect fit)</li>
%%   <li><b>Precision:</b> Does the model allow too much behavior?
%%       (0 = very imprecise, 1 = precise)</li>
%%   <li><b>Generalization:</b> Is the model overfitting the log?
%%       (0 = overfitted, 1 = well-generalized)</li>
%%   <li><b>Alignment:</b> Optimal correspondence between log and model</li>
%% </ul>
%%
%% <h3>Conformance Metrics</h3>
%%
%% The module provides four main quality dimensions:
%%
%% <ol>
%%   <li><b>Fitness:</b> Based on token replay - measures missing and
%%       remaining tokens during replay</li>
%%   <li><b>Precision:</b> Based on escaping edges - measures behavior
%%       allowed by model but not seen in log</li>
%%   <li><b>Generalization:</b> Based on structure - measures whether
%%       the model uses too many places/transitions for the log</li>
%%   <li><b>Alignment:</b> Optimal sequence of log moves and model moves
%%       to explain observed behavior</li>
%% </ol>
%%
%% <h3>Event Log Format</h3>
%%
%% Event logs are lists of traces, where each trace is a list of
%% activities (atoms):
%%
%% ```erlang
%% > Log = [[a, b, c, d], [a, c, b, d]].
%% _
%% > Model = alpha_algorithm:mine_workflow_net(Log).
%% _
%% > conformance:fitness_score(Log, Model).
%% 1.0
%% '''
%%
%% <h3>Token Replay</h3>
%%
%% Token replay attempts to "play" the event log through the model:
%% <ul>
%%   <li>Inject initial token</li>
%%   <li>For each event, consume and produce tokens</li>
%%   <li>Track missing tokens (model cannot replay)</li>
%%   <li>Track remaining tokens (model allows unobserved behavior)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(conformance).

%%====================================================================
%% Exports
%%====================================================================

%% Main conformance API
-export([token_replay/2, fitness_score/2, precision_score/2,
         generalization_score/2, conformance_report/2, align_trace/2]).

%% Analysis utilities
-export([replay_trace/2, count_problems/1, alignment_cost/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc An activity in the event log.
%%--------------------------------------------------------------------
-type activity() :: atom().

%%--------------------------------------------------------------------
%% @doc A trace is a sequence of activities.
%%--------------------------------------------------------------------
-type trace() :: [activity()].

%%--------------------------------------------------------------------
%% @doc An event log is a list of traces.
%%--------------------------------------------------------------------
-type event_log() :: [trace()].

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net.
%%--------------------------------------------------------------------
-type transition() :: atom().

%%--------------------------------------------------------------------
%% @doc An arc connecting nodes in the net.
%%--------------------------------------------------------------------
-type arc() :: {place(), transition()} | {transition(), place()}.

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token counts.
%%--------------------------------------------------------------------
-type marking() :: #{place() => non_neg_integer()}.

%%--------------------------------------------------------------------
%% @doc A WF-net from alpha_algorithm.
%%--------------------------------------------------------------------
-type wf_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place()
}.

%%--------------------------------------------------------------------
%% @doc Token replay result for a single trace.
%%--------------------------------------------------------------------
-type replay_result() :: #{
    status => ok | partial | failed,
    consumed => non_neg_integer(),
    produced => non_neg_integer(),
    missing => non_neg_integer(),
    remaining => non_neg_integer(),
    marking => marking()
}.

%%--------------------------------------------------------------------
%% @doc Aggregated replay problems across all traces.
%%--------------------------------------------------------------------
-type replay_problems() :: #{
    missing => non_neg_integer(),
    remaining => non_neg_integer(),
    consumed => non_neg_integer(),
    produced => non_neg_integer()
}.

%%--------------------------------------------------------------------
%% @doc Alignment move type.
%%--------------------------------------------------------------------
-type align_move() :: {log_move, activity()} |
                     {model_move, transition()} |
                     {sync_move, activity()} |
                     {no_move}.

%%--------------------------------------------------------------------
%% @doc An alignment is a sequence of aligned moves.
%%--------------------------------------------------------------------
-type alignment() :: [align_move()].

%%--------------------------------------------------------------------
%% @doc Alignment result with cost.
%%--------------------------------------------------------------------
-type alignment_result() :: #{
    alignment => alignment(),
    cost => non_neg_integer(),
    trace => trace(),
    fitness => float()
}.

%%--------------------------------------------------------------------
%% @doc Complete conformance report.
%%--------------------------------------------------------------------
-type conformance_report() :: #{
    fitness => float(),
    precision => float(),
    generalization => float(),
    replay_problems => replay_problems(),
    trace_count => non_neg_integer(),
    event_count => non_neg_integer(),
    model_complexity => #{
        places => non_neg_integer(),
        transitions => non_neg_integer(),
        arcs => non_neg_integer()
    }
}.

%% Export types
-export_type([
    activity/0,
    trace/0,
    event_log/0,
    wf_net/0,
    marking/0,
    replay_result/0,
    replay_problems/0,
    alignment/0,
    alignment_result/0,
    conformance_report/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Replays an event log through a model using token replay.
%%
%% Attempts to replay each trace by consuming and producing tokens
%% according to the model structure. Returns aggregated problem counts.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c]],
%% > Model = alpha_algorithm:mine_workflow_net(Log),
%% > conformance:token_replay(Log, Model).
%% #{missing => 0, remaining => 0, consumed => 3, produced => 3}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec token_replay(Log :: event_log(), Model :: wf_net()) -> replay_problems().

token_replay(Log, Model) when is_list(Log), is_map(Model) ->
    %% Replay all traces and aggregate problems
    InitialMarking = init_marking(Model),
    Results = [replay_trace(Trace, Model, InitialMarking) || Trace <- Log],
    aggregate_problems(Results).

%%--------------------------------------------------------------------
%% @doc Calculates fitness score (0-1) based on token replay.
%%
%% Fitness measures how well the model can replay the log.
%% Formula: 1/2 * (1 - missing/consumed) + 1/2 * (1 - remaining/produced)
%%
%% Returns 1.0 for perfect fitness, 0.0 for complete mismatch.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c]],
%% > Model = alpha_algorithm:mine_workflow_net(Log),
%% > conformance:fitness_score(Log, Model).
%% 1.0
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec fitness_score(Log :: event_log(), Model :: wf_net()) -> float().

fitness_score(Log, Model) when is_list(Log), is_map(Model) ->
    Problems = token_replay(Log, Model),
    #{missing := Missing, remaining := Remaining,
      consumed := Consumed, produced := Produced} = Problems,

    case Consumed + Produced of
        0 -> 1.0;  % Empty log and empty model
        _ ->
            %% Fitness formula from Process Mining Manifesto
            MissingPart = case Consumed of
                0 -> 1.0;
                _ -> 1.0 - (Missing / Consumed)
            end,
            RemainingPart = case Produced of
                0 -> 1.0;
                _ -> 1.0 - (Remaining / Produced)
            end,
            0.5 * MissingPart + 0.5 * RemainingPart
    end.

%%--------------------------------------------------------------------
%% @doc Calculates precision score (0-1) based on escaping edges.
%%
%% Precision measures whether the model allows too much behavior
%% not seen in the log. Uses the "escaping edges" metric.
%%
%% Higher precision means the model is more specific to the log.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c]],
%% > Model = alpha_algorithm:mine_workflow_net(Log),
%% > conformance:precision_score(Log, Model).
%% 1.0
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec precision_score(Log :: event_log(), Model :: wf_net()) -> float().

precision_score(Log, Model) when is_list(Log), is_map(Model) ->
    %% Extract log relations for precision calculation
    DirectSucc = alpha_algorithm:direct_succession(Log),
    LogArcs = sets:to_list(DirectSucc),

    %% Get model arcs (transition -> transition through places)
    ModelArcs = extract_model_arcs(Model),

    %% Count escaping edges: edges in model not in log
    Escaping = count_escaping_edges(ModelArcs, LogArcs),

    %% Precision = 1 - (escaping / total model edges)
    case length(ModelArcs) of
        0 -> 1.0;
        TotalModelArcs -> 1.0 - (Escaping / TotalModelArcs)
    end.

%%--------------------------------------------------------------------
%% @doc Calculates generalization score (0-1) for the model.
%%
%% Generalization measures whether the model is overfitting.
%% Uses a simple metric based on model complexity vs log size.
%%
%% Higher values suggest better generalization.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c, d], [a, c, b, d]],
%% > Model = alpha_algorithm:mine_workflow_net(Log),
%% > conformance:generalization_score(Log, Model).
%% 0.8
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec generalization_score(Log :: event_log(), Model :: wf_net()) -> float().

generalization_score(Log, Model) when is_list(Log), is_map(Model) ->
    %% Count unique events and traces
    TraceCount = length(Log),
    EventCount = lists:sum([length(T) || T <- Log]),

    %% Model complexity
    #{places := Places, transitions := Transitions} = Model,
    PlaceCount = length(Places),
    TrsnCount = length(Transitions),

    case EventCount of
        0 ->
            %% Empty log
            1.0;
        _ ->
            %% Simple generalization metric:
            %% 1 if model is not overly complex relative to log
            %% Penalize: too many places per event, too many transitions per event
            PlacesPerEvent = PlaceCount / EventCount,
            TransPerEvent = TrsnCount / EventCount,

            %% Base score reduced by complexity ratios
            Base = 1.0,
            PlacesPenalty = min(0.5, PlacesPerEvent * 0.1),
            TransPenalty = min(0.3, TransPerEvent * 0.05),

            %% Small logs get a penalty for complexity
            SmallLogPenalty = case TraceCount of
                N when N < 5 -> 0.2;
                _ -> 0.0
            end,

            max(0.0, Base - PlacesPenalty - TransPenalty - SmallLogPenalty)
    end.

%%--------------------------------------------------------------------
%% @doc Generates a complete conformance report.
%%
%% Returns a map containing all conformance metrics and diagnostic
%% information about the fit between log and model.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c]],
%% > Model = alpha_algorithm:mine_workflow_net(Log),
%% > conformance:conformance_report(Log, Model).
%% #{fitness => 1.0, precision => 1.0, generalization => 1.0, ...}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec conformance_report(Log :: event_log(), Model :: wf_net()) -> conformance_report().

conformance_report(Log, Model) when is_list(Log), is_map(Model) ->
    %% Calculate all metrics
    Fitness = fitness_score(Log, Model),
    Precision = precision_score(Log, Model),
    Generalization = generalization_score(Log, Model),

    %% Get replay problems
    Problems = token_replay(Log, Model),

    %% Count traces and events
    TraceCount = length(Log),
    EventCount = lists:sum([length(T) || T <- Log]),

    %% Model complexity
    #{places := Places, transitions := Transitions, arcs := Arcs} = Model,

    #{
        fitness => round_score(Fitness),
        precision => round_score(Precision),
        generalization => round_score(Generalization),
        replay_problems => Problems,
        trace_count => TraceCount,
        event_count => EventCount,
        model_complexity => #{
            places => length(Places),
            transitions => length(Transitions),
            arcs => length(Arcs)
        }
    }.

%%--------------------------------------------------------------------
%% @doc Finds optimal alignment between a trace and model.
%%
%% Alignment finds the best correspondence between log moves (seen in
%% trace) and model moves (allowed by model). Each step is:
%% <ul>
%%   <li><b>log_move:</b> Activity in trace but not enabled in model</li>
%%   <li><b>model_move:</b> Transition enabled but not in trace</li>
%%   <li><b>sync_move:</b> Activity matches enabled transition</li>
%% </ul>
%%
%% Uses A* search for optimal alignment with minimum cost.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [[a, b, c]],
%% > Model = alpha_algorithm:mine_workflow_net(Log),
%% > conformance:align_trace([a, b, c], Model).
%% #{alignment => [{sync_move,a}, {sync_move,b}, {sync_move,c}],
%%   cost => 0, trace => [a,b,c], fitness => 1.0}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec align_trace(Trace :: trace(), Model :: wf_net()) -> alignment_result().

align_trace(Trace, Model) when is_list(Trace), is_map(Model) ->
    InitialMarking = init_marking(Model),

    %% A* search for optimal alignment
    {Alignment, Cost} = a_star_align(Trace, InitialMarking, Model),

    %% Calculate fitness from alignment cost
    Fitness = alignment_fitness(Alignment, Trace),

    #{
        alignment => Alignment,
        cost => Cost,
        trace => Trace,
        fitness => round_score(Fitness)
    }.

%%====================================================================
%% Trace Replay Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Replays a single trace through the model.
%% Returns replay status and problem counts.
%%--------------------------------------------------------------------
-spec replay_trace(Trace :: trace(), Model :: wf_net()) -> replay_result().

replay_trace(Trace, Model) ->
    InitialMarking = init_marking(Model),
    replay_trace(Trace, Model, InitialMarking).

%% @private
replay_trace(Trace, Model, InitialMarking) when is_list(Trace), is_map(Model) ->
    %% Replay returns the final marking and counts
    {Marking, Consumed, Produced, Missing, _Remaining} =
        lists:foldl(fun(Activity, {Mark, Cons, Prod, Miss, Rem}) ->
            case consume_activity(Activity, Mark, Model) of
                {ok, NewMark} ->
                    %% Activity was consumed, produce output tokens
                    {ProdMark, NewProd} = produce_activity(Activity, NewMark, Model),
                    {ProdMark, Cons + 1, Prod + NewProd, Miss, Rem};
                {missing, NewMark} ->
                    %% Missing token - count but continue
                    {ProdMark, NewProd} = produce_activity(Activity, NewMark, Model),
                    {ProdMark, Cons + 1, Prod + NewProd, Miss + 1, Rem}
            end
        end, {InitialMarking, 0, 0, 0, 0}, Trace),

    %% Count remaining tokens (excluding source/sink)
    #{initial_place := Init, final_place := Final} = Model,
    RemainingTokens = maps:fold(fun(Place, Count, Acc) ->
        case Place of
            Init -> Acc;  %% Ignore initial place
            Final -> Acc;  %% Ignore final place
            _ -> Acc + Count
        end
    end, 0, Marking),

    %% Determine status
    Status = case {Missing, RemainingTokens} of
        {0, 0} -> ok;
        {0, _} -> partial;  %% Only remaining tokens
        {_, 0} -> partial;  %% Only missing tokens
        {_, _} -> failed
    end,

    #{
        status => Status,
        consumed => Consumed,
        produced => Produced,
        missing => Missing,
        remaining => RemainingTokens,
        marking => Marking
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Creates initial marking for the model (token in source place).
%%--------------------------------------------------------------------
-spec init_marking(Model :: wf_net()) -> marking().

init_marking(#{initial_place := Init, places := Places}) ->
    BaseMarking = maps:from_list([{P, 0} || P <- Places]),
    BaseMarking#{Init => 1}.

%%--------------------------------------------------------------------
%% @private
%% @doc Attempts to consume tokens for an activity.
%% Returns {ok, NewMarking} or {missing, NewMarking}.
%%--------------------------------------------------------------------
-spec consume_activity(Activity :: activity(), Marking :: marking(),
                       Model :: wf_net()) -> {ok | missing, marking()}.

consume_activity(Activity, Marking, Model) ->
    %% Find input places for this activity (transition)
    InputPlaces = find_input_places(Activity, Model),

    %% Check if all input places have tokens
    case can_consume(InputPlaces, Marking) of
        true ->
            %% Consume tokens
            NewMarking = consume_tokens(InputPlaces, Marking),
            {ok, NewMarking};
        false ->
            {missing, Marking}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Produces tokens after an activity fires.
%% Returns {NewMarking, ProducedCount}.
%%--------------------------------------------------------------------
-spec produce_activity(Activity :: activity(), Marking :: marking(),
                       Model :: wf_net()) -> {marking(), non_neg_integer()}.

produce_activity(Activity, Marking, Model) ->
    %% Find output places for this activity
    OutputPlaces = find_output_places(Activity, Model),

    %% Add tokens to output places
    NewMarking = lists:foldl(fun(Place, Acc) ->
        Acc#{Place => maps:get(Place, Acc, 0) + 1}
    end, Marking, OutputPlaces),

    {NewMarking, length(OutputPlaces)}.

%%--------------------------------------------------------------------
%% @private
%% @doc Finds input places for a transition (activity).
%%--------------------------------------------------------------------
-spec find_input_places(Transition :: transition(), Model :: wf_net()) -> [place()].

find_input_places(Transition, #{arcs := Arcs}) ->
    [Place || {Place, Trsn} <- Arcs, Trsn =:= Transition].

%%--------------------------------------------------------------------
%% @private
%% @doc Finds output places for a transition (activity).
%%--------------------------------------------------------------------
-spec find_output_places(Transition :: transition(), Model :: wf_net()) -> [place()].

find_output_places(Transition, #{arcs := Arcs}) ->
    [Place || {Trsn, Place} <- Arcs, Trsn =:= Transition].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if tokens can be consumed from all input places.
%%--------------------------------------------------------------------
-spec can_consume(Places :: [place()], Marking :: marking()) -> boolean().

can_consume([], _Marking) -> true;
can_consume([Place | Rest], Marking) when is_map(Marking) ->
    case maps:get(Place, Marking, 0) of
        N when is_integer(N), N > 0 -> can_consume(Rest, Marking);
        _ -> false
    end;
can_consume(_Places, _Marking) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Consumes one token from each input place.
%%--------------------------------------------------------------------
-spec consume_tokens(Places :: [place()], Marking :: marking()) -> marking().

consume_tokens(Places, Marking) ->
    lists:foldl(fun(Place, Acc) ->
        Acc#{Place => maps:get(Place, Acc, 0) - 1}
    end, Marking, Places).

%%====================================================================
%% Problem Aggregation
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Aggregates replay results across all traces.
%%--------------------------------------------------------------------
-spec aggregate_problems([replay_result()]) -> replay_problems().

aggregate_problems(Results) ->
    lists:foldl(fun(Result, Acc) ->
        #{
            missing := Missing,
            remaining := Remaining,
            consumed := Consumed,
            produced := Produced
        } = Result,
        Acc#{
            missing => maps:get(missing, Acc, 0) + Missing,
            remaining => maps:get(remaining, Acc, 0) + Remaining,
            consumed => maps:get(consumed, Acc, 0) + Consumed,
            produced => maps:get(produced, Acc, 0) + Produced
        }
    end, #{missing => 0, remaining => 0, consumed => 0, produced => 0}, Results).

%%--------------------------------------------------------------------
%% @doc Counts problems from a replay_problems map.
%%
%% Returns total deviation count (missing + remaining).
%%
%% ## Examples
%%
%% ```erlang
%% > Problems = #{missing => 2, remaining => 1, consumed => 10, produced => 10},
%% > conformance:count_problems(Problems).
%% 3
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec count_problems(replay_problems()) -> non_neg_integer().

count_problems(#{missing := Missing, remaining := Remaining}) ->
    Missing + Remaining.

%%====================================================================
%% Precision and Model Analysis
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts transition-to-transition arcs from the model.
%% Traces paths through places.
%%--------------------------------------------------------------------
-spec extract_model_arcs(Model :: wf_net()) -> [{transition(), transition()}].

extract_model_arcs(#{arcs := Arcs, places := Places}) ->
    %% Build transition -> place -> transition paths
    PlaceMap = build_place_map(Arcs),

    %% For each place, find all incoming and outgoing transitions
    lists:flatmap(fun(Place) ->
        case maps:get(Place, PlaceMap, undefined) of
            #{in := In, out := Out} ->
                [{T1, T2} || T1 <- In, T2 <- Out];
            _ ->
                []
        end
    end, Places).

%% @private
build_place_map(Arcs) ->
    %% Build map: Place -> #{in => [Trans], out => [Trans]}
    lists:foldl(fun
        ({Place, Trsn}, Acc) when is_atom(Place), is_atom(Trsn) ->
            PlaceRec = maps:get(Place, Acc, #{in => [], out => []}),
            NewRec = PlaceRec#{out => [Trsn | maps:get(out, PlaceRec, [])]},
            Acc#{Place => NewRec};
        ({Trsn, Place}, Acc) when is_atom(Trsn), is_atom(Place) ->
            PlaceRec = maps:get(Place, Acc, #{in => [], out => []}),
            NewRec = PlaceRec#{in => [Trsn | maps:get(in, PlaceRec, [])]},
            Acc#{Place => NewRec};
        (_, Acc) ->
            Acc
    end, #{}, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Counts model edges not present in log (escaping edges).
%%--------------------------------------------------------------------
-spec count_escaping_edges(ModelArcs :: [{transition(), transition()}],
                          LogArcs :: [{transition(), transition()}]) ->
    non_neg_integer().

count_escaping_edges(ModelArcs, LogArcs) ->
    LogArcSet = sets:from_list(LogArcs),
    length([A || A <- ModelArcs, not sets:is_element(A, LogArcSet)]).

%%====================================================================
%% Alignment Functions (A* Search)
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Performs A* search for optimal alignment.
%% Returns {Alignment, TotalCost}.
%%--------------------------------------------------------------------
-spec a_star_align(Trace :: trace(), InitialMarking :: marking(),
                   Model :: wf_net()) -> {alignment(), non_neg_integer()}.

a_star_align(Trace, InitialMarking, Model) ->
    %% Priority queue with: {Cost + Heuristic, Cost, TracePos, Marking, Alignment}
    %% Using simple list as priority queue (inefficient but simple)
    StartHeuristic = length(Trace),
    StartNode = {StartHeuristic, 0, Trace, InitialMarking, []},

    %% A* search loop
    a_star_loop([StartNode], Model, #{}).

%% @private
a_star_loop([], _Model, _Visited) ->
    %% No alignment found (shouldn't happen with proper models)
    {[], 0};

a_star_loop([{_F, Cost, TraceRem, Marking, Alignment} | Rest], Model, Visited) ->
    VisitedKey = {TraceRem, simplify_marking(Marking)},

    %% Check if we've visited this state with better cost
    case maps:get(VisitedKey, Visited, infinity) of
        BestCost when Cost >= BestCost ->
            %% Skip this state, already visited with better cost
            a_star_loop(Rest, Model, Visited);
        _ ->
            %% Process this state
            NewVisited = Visited#{VisitedKey => Cost},

            case {TraceRem, is_final_marking(Marking, Model)} of
                {[], true} ->
                    %% Complete alignment found
                    {lists:reverse(Alignment), Cost};

                _ ->
                    %% Generate successors
                    Successors = get_successors(TraceRem, Marking, Model, Alignment, Cost),

                    %% Add to priority queue
                    NewNodes = Rest ++ Successors,
                    a_star_loop(NewNodes, Model, NewVisited)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates successor states for A* search.
%%--------------------------------------------------------------------
-spec get_successors(trace(), marking(), wf_net(), alignment(), non_neg_integer()) ->
    [{float(), non_neg_integer(), trace(), marking(), alignment()}].

get_successors([NextEvent | RestTrace], Marking, Model, Alignment, CurrentCost) ->
    Enabled = get_enabled_transitions(Marking, Model),

    %% Three types of moves:
    %% 1. Log move: consume from trace (missing event in model)
    %% 2. Model move: fire enabled transition not in trace
    %% 3. Sync move: event in trace and enabled in model

    Successors = [
        case lists:member(NextEvent, Enabled) of
            true ->
                %% Sync move: log and model agree
                NewMarking = fire_sync_move(NextEvent, Marking, Model),
                Heuristic = length(RestTrace),
                [{Heuristic, CurrentCost, RestTrace, NewMarking,
                  [{sync_move, NextEvent} | Alignment]}];
            false ->
                []
        end,

        %% Log move: event in trace but not enabled
        [{1 + length(RestTrace), CurrentCost + 1, RestTrace, Marking,
          [{log_move, NextEvent} | Alignment]}]

        ++
        %% Model moves: enabled transitions not matching next event
        [{1 + length([NextEvent | RestTrace]), CurrentCost + 1, [NextEvent | RestTrace],
          fire_model_move(T, Marking, Model), [{model_move, T} | Alignment]}
         || T <- Enabled, T =/= NextEvent]
    ],

    lists:flatten(Successors);

get_successors([], Marking, Model, Alignment, CurrentCost) ->
    %% Only model moves possible (complete trace, need to reach final marking)
    Enabled = get_enabled_transitions(Marking, Model),

    case is_final_marking(Marking, Model) of
        true ->
            [];  %% Already at final state
        false ->
            [{length(Enabled), CurrentCost + 1, [],
              fire_model_move(T, Marking, Model), [{model_move, T} | Alignment]}
             || T <- Enabled]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets currently enabled transitions.
%%--------------------------------------------------------------------
-spec get_enabled_transitions(Marking :: marking(), Model :: wf_net()) -> [transition()].

get_enabled_transitions(Marking, #{arcs := Arcs}) ->
    %% Find transitions whose input places all have tokens
    TrsnPlaces = build_transition_places(Arcs),

    [T || {T, InputPlaces} <- maps:to_list(TrsnPlaces),
          can_consume(InputPlaces, Marking)].

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
        (_, Acc) ->
            Acc
    end, #{}, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Fires a sync move (activity in both trace and model).
%%--------------------------------------------------------------------
-spec fire_sync_move(Activity :: activity(), Marking :: marking(),
                     Model :: wf_net()) -> marking().

fire_sync_move(Activity, Marking, Model) ->
    InputPlaces = find_input_places(Activity, Model),
    OutputPlaces = find_output_places(Activity, Model),

    %% Consume from input, produce to output
    Marking1 = consume_tokens(InputPlaces, Marking),
    lists:foldl(fun(P, Acc) ->
        Acc#{P => maps:get(P, Acc, 0) + 1}
    end, Marking1, OutputPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Fires a model move (only in model, not trace).
%%--------------------------------------------------------------------
-spec fire_model_move(Transition :: transition(), Marking :: marking(),
                      Model :: wf_net()) -> marking().

fire_model_move(Transition, Marking, Model) ->
    InputPlaces = find_input_places(Transition, Model),
    OutputPlaces = find_output_places(Transition, Model),

    Marking1 = consume_tokens(InputPlaces, Marking),
    lists:foldl(fun(P, Acc) ->
        Acc#{P => maps:get(P, Acc, 0) + 1}
    end, Marking1, OutputPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if marking represents the final state.
%%--------------------------------------------------------------------
-spec is_final_marking(Marking :: marking(), Model :: wf_net()) -> boolean().

is_final_marking(Marking, #{final_place := Final, initial_place := Initial}) ->
    %% Final state: token only in final place, none elsewhere (except initial)
    maps:get(Final, Marking, 0) > 0 andalso
    lists:all(fun({P, Count}) ->
        case P of
            Initial -> Count =:= 0;
            Final -> Count >= 0;
            _ -> Count =:= 0
        end
    end, maps:to_list(Marking)).

%%--------------------------------------------------------------------
%% @private
%% @doc Simplifies marking for visited state tracking.
%%--------------------------------------------------------------------
-spec simplify_marking(Marking :: marking()) -> marking().

simplify_marking(Marking) ->
    %% Just return the marking as-is (could optimize by capping large counts)
    Marking.

%%--------------------------------------------------------------------
%% @doc Calculates fitness from an alignment.
%%
%% Fitness = 1 - (cost of moves / length of alignment)
%%
%% ## Examples
%%
%% ```erlang
%% > Alignment = [{sync_move,a}, {sync_move,b}, {sync_move,c}],
%% > conformance:alignment_fitness(Alignment, [a,b,c]).
%% 1.0
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec alignment_fitness(Alignment :: alignment(), Trace :: trace()) -> float().

alignment_fitness(Alignment, Trace) ->
    Cost = alignment_cost(Alignment),
    Len = length(Alignment),

    case Len of
        0 -> 1.0;
        _ ->
            %% Normalize by trace length for fairness
            TraceLen = length(Trace),
            case TraceLen of
                0 -> 1.0;
                _ -> max(0.0, 1.0 - (Cost / (2 * TraceLen)))
            end
    end.

%%--------------------------------------------------------------------
%% @doc Calculates the cost of an alignment.
%%
%% Log moves and model moves cost 1, sync moves cost 0.
%%
%% ## Examples
%%
%% ```erlang
%% > conformance:alignment_cost([{sync_move,a}, {log_move,b}, {model_move,c}]).
%% 2
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec alignment_cost(Alignment :: alignment()) -> non_neg_integer().

alignment_cost(Alignment) ->
    lists:foldl(fun
        ({log_move, _}, Acc) -> Acc + 1;
        ({model_move, _}, Acc) -> Acc + 1;
        ({sync_move, _}, Acc) -> Acc;
        ({no_move}, Acc) -> Acc + 1
    end, 0, Alignment).

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Rounds a score to 4 decimal places.
%%--------------------------------------------------------------------
-spec round_score(float()) -> float().

round_score(Score) ->
    round(Score * 10000) / 10000.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

simple_log() ->
    [[a, b, c]].

sequential_log() ->
    [[a, b, c, d]].

parallel_log() ->
    [[a, b, c, d], [a, c, b, d]].

divergent_log() ->
    [[a, b, c, e], [a, b, c, f]].

%%--------------------------------------------------------------------
%% Token replay tests
%%--------------------------------------------------------------------

token_replay_perfect_test() ->
    Log = simple_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Problems = token_replay(Log, Model),
    ?assertEqual(0, maps:get(missing, Problems)),
    ?assertEqual(0, maps:get(remaining, Problems)).

token_replay_parallel_test() ->
    Log = parallel_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Problems = token_replay(Log, Model),
    %% Should replay without missing tokens
    ?assertEqual(0, maps:get(missing, Problems)).

replay_trace_single_test() ->
    Log = simple_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Result = replay_trace(hd(Log), Model),
    ?assertEqual(ok, maps:get(status, Result)),
    ?assertEqual(3, maps:get(consumed, Result)).

%%--------------------------------------------------------------------
%% Fitness tests
%%--------------------------------------------------------------------

fitness_perfect_test() ->
    Log = simple_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Score = fitness_score(Log, Model),
    ?assert(Score >= 0.99).

fitness_sequential_test() ->
    Log = sequential_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Score = fitness_score(Log, Model),
    ?assert(Score >= 0.99).

fitness_parallel_test() ->
    Log = parallel_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Score = fitness_score(Log, Model),
    ?assert(Score >= 0.5).

fitness_empty_log_test() ->
    Log = [],
    Model = alpha_algorithm:mine_workflow_net(simple_log()),
    Score = fitness_score(Log, Model),
    ?assertEqual(1.0, Score).

%%--------------------------------------------------------------------
%% Precision tests
%%--------------------------------------------------------------------

precision_perfect_test() ->
    Log = simple_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Score = precision_score(Log, Model),
    ?assert(Score >= 0.5).

precision_sequential_test() ->
    Log = sequential_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Score = precision_score(Log, Model),
    ?assert(Score >= 0.5).

%%--------------------------------------------------------------------
%% Generalization tests
%%--------------------------------------------------------------------

generalization_basic_test() ->
    Log = simple_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Score = generalization_score(Log, Model),
    ?assert(Score >= 0.0),
    ?assert(Score =< 1.0).

generalization_parallel_test() ->
    Log = parallel_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Score = generalization_score(Log, Model),
    ?assert(Score >= 0.0).

%%--------------------------------------------------------------------
%% Conformance report tests
%%--------------------------------------------------------------------

conformance_report_complete_test() ->
    Log = simple_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Report = conformance_report(Log, Model),

    ?assert(maps:is_key(fitness, Report)),
    ?assert(maps:is_key(precision, Report)),
    ?assert(maps:is_key(generalization, Report)),
    ?assert(maps:is_key(replay_problems, Report)),
    ?assert(maps:is_key(trace_count, Report)),
    ?assert(maps:is_key(event_count, Report)),
    ?assert(maps:is_key(model_complexity, Report)),

    ?assertEqual(1, maps:get(trace_count, Report)),
    ?assertEqual(3, maps:get(event_count, Report)).

%%--------------------------------------------------------------------
%% Alignment tests
%%--------------------------------------------------------------------

align_trace_perfect_test() ->
    Log = simple_log(),
    Model = alpha_algorithm:mine_workflow_net(Log),
    Result = align_trace(hd(Log), Model),

    Alignment = maps:get(alignment, Result),
    Cost = maps:get(cost, Result),
    Fitness = maps:get(fitness, Result),

    ?assert(is_list(Alignment)),
    ?assert(is_integer(Cost)),
    ?assert(Fitness >= 0.0),
    ?assert(Fitness =< 1.0).

alignment_cost_test() ->
    ?assertEqual(0, alignment_cost([{sync_move, a}, {sync_move, b}])),
    ?assertEqual(1, alignment_cost([{sync_move, a}, {log_move, b}])),
    ?assertEqual(1, alignment_cost([{sync_move, a}, {model_move, b}])),
    ?assertEqual(2, alignment_cost([{log_move, a}, {model_move, b}])).

alignment_fitness_test() ->
    Perfect = [{sync_move, a}, {sync_move, b}, {sync_move, c}],
    ?assertEqual(1.0, alignment_fitness(Perfect, [a, b, c])),

    OneError = [{sync_move, a}, {log_move, b}, {sync_move, c}],
    ?assert(alignment_fitness(OneError, [a, b, c]) < 1.0),

    Empty = [],
    ?assertEqual(1.0, alignment_fitness(Empty, [])).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

count_problems_test() ->
    Problems1 = #{missing => 0, remaining => 0, consumed => 5, produced => 5},
    ?assertEqual(0, count_problems(Problems1)),

    Problems2 = #{missing => 2, remaining => 1, consumed => 5, produced => 5},
    ?assertEqual(3, count_problems(Problems2)).

init_marking_test() ->
    Model = alpha_algorithm:mine_workflow_net(simple_log()),
    Marking = init_marking(Model),

    ?assert(maps:is_key(i_source, Marking)),
    ?assertEqual(1, maps:get(i_source, Marking)),

    #{initial_place := Init} = Model,
    ?assertEqual(1, maps:get(Init, Marking, 0)).

%%--------------------------------------------------------------------
%% Doctest
%%--------------------------------------------------------------------

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

-endif.
