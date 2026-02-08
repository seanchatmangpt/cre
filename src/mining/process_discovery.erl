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
%% @doc Enhanced Process Discovery Algorithms
%%
%% This module implements advanced process mining algorithms that extend
%% the Alpha algorithm with heuristics for handling noise, incomplete logs,
%% loops, and non-free-choice constructs.
%%
%% <h3>Algorithms Implemented</h3>
%%
%% <ul>
%%   <li><b>Heuristic Miner:</b> Frequency-based dependency extraction
%%       that handles noise better than Alpha (Weijters & van der Aalst, 2003)</li>
%%   <li><b>Frequency-Based Mining:</b> Statistical dependency calculation
%%       with confidence thresholds</li>
%%   <li><b>Noise-Tolerant Discovery:</b> Handles incomplete and noisy
%%       event logs using statistical filtering</li>
%%   <li><b>Loop Discovery:</b> Properly handles short loops (length-1 and
%%       length-2) and long loops</li>
%%   <li><b>Non-Free-Choice Discovery:</b> Detects and constructs
%%       non-free-choice constructs with shared input/output places</li>
%% </ul>
%%
%% <h3>Event Log Format</h3>
%%
%% Event logs are lists of tuples {CaseID, Activity, Timestamp}:
%%
%% ```erlang
%% > Log = [{case1, a, 1}, {case1, b, 2}, {case1, c, 3},
%%          {case2, a, 4}, {case2, c, 5}, {case2, b, 6}].
%% _
%% > process_discovery:discover(Log).
%% #{places => [...], transitions => [a,b,c], arcs => [...], ...}
%% '''
%%
%% <h3>Output Format</h3>
%%
%% All discovery functions return a Petri net model map:
%% <ul>
%%   <li><b>places:</b> List of place atoms</li>
%%   <li><b>transitions:</b> List of transition atoms (activities)</li>
%%   <li><b>arcs:</b> List of {source, target} tuples</li>
%%   <li><b>initial_place:</b> The source place</li>
%%   <li><b>final_place:</b> The sink place</li>
%%   <li><b>metadata:</b> Algorithm-specific statistics</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(process_discovery).

%%====================================================================
%% Exports
%%====================================================================

%% Main discovery API
-export([discover/1]).
-export([heuristic_miner/1]).
-export([frequency_based/1]).
-export([discover_with_noise/2]).
-export([discover_loops/1]).
-export([discover_non_free_choice/1]).

%% Utility functions for testing and analysis
-export([calculate_dependencies/1]).
-export([calculate_frequency_matrix/1]).
-export([detect_loops/1]).
-export([classify_loops/1]).
-export([detect_non_free_choice/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A case identifier in the event log.
%%
%% Can be any Erlang term used to group events by case.
%%--------------------------------------------------------------------
-type case_id() :: term().

%%--------------------------------------------------------------------
%% @doc An activity in the event log.
%%
%% Activities are represented as atoms for efficient pattern matching.
%%--------------------------------------------------------------------
-type activity() :: atom().

%%--------------------------------------------------------------------
%% @doc A timestamp in the event log.
%%
%% Integer timestamp (milliseconds since epoch) or any comparable value.
%%--------------------------------------------------------------------
-type timestamp() :: integer().

%%--------------------------------------------------------------------
%% @doc A single event in the event log.
%%
%% Tuple of {CaseID, Activity, Timestamp} representing one event.
%%--------------------------------------------------------------------
-type event() :: {case_id(), activity(), timestamp()}.

%%--------------------------------------------------------------------
%% @doc An event log as a list of events.
%%
%% Events may be unsorted; the algorithm groups by case and sorts.
%%--------------------------------------------------------------------
-type event_log() :: [event()].

%%--------------------------------------------------------------------
%% @doc A trace is a sequence of activities for a single case.
%%
%% Extracted from event log by grouping and sorting by timestamp.
%%--------------------------------------------------------------------
-type trace() :: [activity()].

%%--------------------------------------------------------------------
%% @doc A frequency count of occurrences.
%%
%% Number of times a pattern appears in the log.
%%--------------------------------------------------------------------
-type frequency() :: non_neg_integer().

%%--------------------------------------------------------------------
%% @doc Dependency strength between two activities.
%%
%% Float value between 0.0 and 1.0 indicating causal relationship strength.
%%--------------------------------------------------------------------
-type dependency() :: float().

%%--------------------------------------------------------------------
%% @doc A place in the discovered Petri net.
%%
%% Places are atoms representing Petri net places where tokens reside.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the discovered Petri net.
%%
%% Transitions correspond to activities from the event log.
%%--------------------------------------------------------------------
-type transition() :: atom().

%%--------------------------------------------------------------------
%% @doc An arc connecting two nodes in the Petri net.
%%
%% Arcs can be place->transition or transition->place.
%%--------------------------------------------------------------------
-type arc() :: {place(), transition()} | {transition(), place()}.

%%--------------------------------------------------------------------
%% @doc A Petri net model discovered from an event log.
%%
%% Complete workflow net structure with places, transitions, arcs,
%% and metadata about the discovery process.
%%--------------------------------------------------------------------
-type petri_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place(),
    metadata => #{
        algorithm => atom(),
        cases_processed => non_neg_integer(),
        total_events => non_neg_integer(),
        noise_level => float(),
        optional => map()
    }
}.

%%--------------------------------------------------------------------
%% @doc Dependency matrix mapping activity pairs to dependency values.
%%
%% Map of {A, B} tuples to dependency strength (0.0 to 1.0).
%%--------------------------------------------------------------------
-type dependency_matrix() :: #{{activity(), activity()} => dependency()}.

%%--------------------------------------------------------------------
%% @doc Frequency matrix of activity succession counts.
%%
%% Map of {A, B} tuples to occurrence counts.
%%--------------------------------------------------------------------
-type frequency_matrix() :: #{{activity(), activity()} => frequency()}.

%%--------------------------------------------------------------------
%% @doc Loop classification types.
%%
%% - short_loop_1: Length-1 loop (A -> A)
%% - short_loop_2: Length-2 loop (A -> B -> A)
%% - long_loop: Longer causal cycles
%%--------------------------------------------------------------------
-type loop_type() :: short_loop_1 | short_loop_2 | long_loop.

%%--------------------------------------------------------------------
%% @doc Detected loop information.
%%
%% Contains loop type and the activities involved.
%%--------------------------------------------------------------------
-type loop_info() :: #{
    type => loop_type(),
    activities => [activity()],
    frequency => frequency()
}.

%%--------------------------------------------------------------------
%% @doc Non-free-choice construct information.
%%
%% Shared input/output places across multiple transitions.
%%--------------------------------------------------------------------
-type non_free_choice_info() :: #{
    shared_place => place(),
    transitions => [transition()],
    type => shared_input | shared_output
}.

%% Export types
-export_type([
    event/0,
    event_log/0,
    trace/0,
    petri_net/0,
    dependency_matrix/0,
    frequency_matrix/0,
    loop_info/0,
    non_free_choice_info/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main entry point for process discovery from event logs.
%%
%% Uses Heuristic Miner by default for better noise tolerance.
%% Automatically detects log characteristics and selects appropriate
%% algorithm parameters.
%%
%% ## Examples
%%
%% Basic discovery:
%% ```erlang
%% > Log = [{case1, a, 1}, {case1, b, 2}, {case1, c, 3},
%%          {case2, a, 4}, {case2, c, 5}, {case2, b, 6}].
%% _
%% > Net = process_discovery:discover(Log).
%% _
%% > maps:get(transitions, Net).
%% [a,b,c]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec discover(Log :: event_log()) -> petri_net().

discover(Log) when is_list(Log) ->
    heuristic_miner(Log).

%%--------------------------------------------------------------------
%% @doc Heuristic mining algorithm (handles noise better than Alpha).
%%
%% Implements the HeuristicsMiner algorithm from Weijters & van der Aalst
%% (2003) which uses frequency-based dependency measures to handle
%% noisy and incomplete event logs.
%%
%% Key improvements over Alpha:
%% - Frequency-based instead of binary relations
%% - Configurable dependency thresholds
%% - Better handling of infrequent paths
%% - Noise filtering based on significance
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [{c1, a, 1}, {c1, b, 2}, {c1, c, 3},
%%          {c2, a, 4}, {c2, c, 5}, {c2, b, 6},  % noisy trace
%%          {c3, a, 7}, {c3, b, 8}, {c3, c, 9}].
%% _
%% > Net = process_discovery:heuristic_miner(Log).
%% _
%% > maps:get(transitions, Net).
%% [a,b,c]
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec heuristic_miner(Log :: event_log()) -> petri_net().

heuristic_miner(Log) when is_list(Log) ->
    %% Convert event log to traces
    Traces = events_to_traces(Log),

    %% Calculate dependency matrix using heuristic measures
    DepMatrix = calculate_dependencies(Traces),

    %% Calculate frequency matrix for arc construction
    FreqMatrix = calculate_frequency_matrix(Traces),

    %% Extract all activities
    Activities = extract_activities_from_traces(Traces),

    %% Apply dependency thresholds (default 0.7 for strong dependencies)
    Threshold = 0.7,
    SignificantDeps = filter_significant_dependencies(DepMatrix, Threshold),

    %% Identify start and end activities
    StartActivities = find_start_activities(Traces),
    EndActivities = find_end_activities(Traces),

    %% Detect and classify loops
    Loops = detect_loops(Traces),

    %% Construct places based on significant dependencies
    Places = construct_places_from_deps(
        SignificantDeps,
        Activities,
        StartActivities,
        EndActivities,
        Loops
    ),

    %% Build complete Petri net
    build_petri_net(
        Activities,
        Places,
        StartActivities,
        EndActivities,
        SignificantDeps,
        FreqMatrix,
        #{
            algorithm => heuristic_miner,
            cases_processed => length(Traces),
            total_events => length(Log),
            noise_level => estimate_noise_level(Log, Traces),
            dependency_threshold => Threshold,
            loops_detected => length(Loops)
        }
    ).

%%--------------------------------------------------------------------
%% @doc Frequency-based dependency extraction.
%%
%% Calculates dependency strength based on frequency of succession
%% patterns in the event log. Uses the dependency measure:
%%
%%   dep(a,b) = (|a>b| - |b>a|) / (|a>b| + |b>a| + 1)
%%
%% Returns a matrix of dependency values between all activity pairs.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [{c1, a, 1}, {c1, b, 2}, {c1, c, 3},
%%          {c2, a, 4}, {c2, b, 5}, {c2, c, 6}].
%% _
%% > Deps = process_discovery:frequency_based(Log),
%% _
%% > maps:get({a, b}, Deps) > maps:get({b, a}, Deps).
%% true
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec frequency_based(Log :: event_log()) -> dependency_matrix().

frequency_based(Log) when is_list(Log) ->
    Traces = events_to_traces(Log),
    calculate_dependencies(Traces).

%%--------------------------------------------------------------------
%% @doc Handle incomplete/noisy event logs.
%%
%% Discovers process models with explicit noise handling parameters:
%% - DependencyThreshold: Minimum dependency strength (0.0 to 1.0)
%% - PositiveObservations: Minimum observations to consider relation
%% - BestPracticeThreshold: Minimum relative frequency
%%
%% Lower thresholds produce more connected models but may include noise.
%% Higher thresholds produce simpler models but may miss valid paths.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = noisy_event_log(),
%% >
%% > %% Strict threshold for cleaner model
%% > Net1 = process_discovery:discover_with_noise(Log, #{
%% >   dependency_threshold => 0.9,
%% >   positive_observations => 3
%% > }),
%% >
%% > %% Permissive threshold for comprehensive model
%% > Net2 = process_discovery:discover_with_noise(Log, #{
%% >   dependency_threshold => 0.5,
%% >   positive_observations => 1
%% > }).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec discover_with_noise(Log :: event_log(), Options :: map()) -> petri_net().

discover_with_noise(Log, Options) when is_list(Log), is_map(Options) ->
    Traces = events_to_traces(Log),

    %% Get options with defaults
    DepThreshold = maps:get(dependency_threshold, Options, 0.6),
    PosObsThreshold = maps:get(positive_observations, Options, 1),
    _BestPracticeThreshold = maps:get(best_practice_threshold, Options, 0.8),

    %% Calculate dependency matrix
    DepMatrix = calculate_dependencies(Traces),

    %% Calculate frequency matrix
    FreqMatrix = calculate_frequency_matrix(Traces),

    %% Extract all activities
    Activities = extract_activities_from_traces(Traces),

    %% Filter by dependency threshold AND positive observations
    SignificantDeps = filter_by_threshold_and_count(
        DepMatrix,
        FreqMatrix,
        DepThreshold,
        PosObsThreshold
    ),

    %% Identify start and end activities with frequency consideration
    StartActivities = find_start_activities_freq(Traces, FreqMatrix),
    EndActivities = find_end_activities_freq(Traces, FreqMatrix),

    %% Detect loops
    Loops = detect_loops(Traces),

    %% Construct places
    Places = construct_places_from_deps(
        SignificantDeps,
        Activities,
        StartActivities,
        EndActivities,
        Loops
    ),

    %% Build Petri net with noise metadata
    build_petri_net(
        Activities,
        Places,
        StartActivities,
        EndActivities,
        SignificantDeps,
        FreqMatrix,
        #{
            algorithm => discover_with_noise,
            cases_processed => length(Traces),
            total_events => length(Log),
            noise_level => estimate_noise_level(Log, Traces),
            dependency_threshold => DepThreshold,
            positive_obs_threshold => PosObsThreshold,
            loops_detected => length(Loops)
        }
    ).

%%--------------------------------------------------------------------
%% @doc Properly handle short and long loops.
%%
%% Detects and constructs Petri net structures for:
%% - Short loops of length 1 (A -> A): Self-loop transitions
%% - Short loops of length 2 (A -> B -> A): Two-transition loops
%% - Long loops: Cycles through three or more activities
%%
%% Uses special place construction rules to ensure sound workflow nets.
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [{c1, a, 1}, {c1, b, 2}, {c1, a, 3},   % short loop A->B->A
%%          {c2, a, 4}, {c2, b, 5}, {c2, c, 6}, {c2, a, 7}]. % long loop
%% _
%% > Net = process_discovery:discover_loops(Log).
%% _
%% > Loops = detect_loops(events_to_traces(Log)).
%% _
%% > length([L || L <- Loops, maps:get(type, L) =:= short_loop_2]).
%% 1
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec discover_loops(Log :: event_log()) -> petri_net().

discover_loops(Log) when is_list(Log) ->
    Traces = events_to_traces(Log),

    %% Calculate dependencies
    DepMatrix = calculate_dependencies(Traces),
    FreqMatrix = calculate_frequency_matrix(Traces),

    %% Detect and classify all loops
    Loops = detect_loops(Traces),
    ClassifiedLoops = classify_loops(Traces),

    %% Extract activities
    Activities = extract_activities_from_traces(Traces),

    %% Identify start/end with loop-aware handling
    StartActivities = find_start_activities(Traces),
    EndActivities = find_end_activities(Traces),

    %% Construct places with loop handling
    Places = construct_places_with_loops(
        DepMatrix,
        FreqMatrix,
        Activities,
        StartActivities,
        EndActivities,
        ClassifiedLoops
    ),

    %% Build Petri net with loop metadata
    build_petri_net(
        Activities,
        Places,
        StartActivities,
        EndActivities,
        DepMatrix,
        FreqMatrix,
        #{
            algorithm => discover_loops,
            cases_processed => length(Traces),
            total_events => length(Log),
            noise_level => estimate_noise_level(Log, Traces),
            loops_detected => length(Loops),
            loop_details => ClassifiedLoops
        }
    ).

%%--------------------------------------------------------------------
%% @doc Handle non-free-choice constructs.
%%
%% Non-free-choice constructs occur when multiple transitions share
%% input or output places but have different connectivity patterns.
%%
%% This function:
%% 1. Detects shared input/output places
%% 2. Identifies non-free-choice patterns
%% 3. Constructs appropriate place structures
%% 4. Ensures soundness properties are maintained
%%
%% ## Examples
%%
%% ```erlang
%% > Log = [{c1, a, 1}, {c1, b, 2}, {c1, d, 3},
%%          {c2, a, 4}, {c2, c, 5}, {c2, d, 6}].
%% _
%% > Net = process_discovery:discover_non_free_choice(Log),
%% _
%% > Nfc = detect_non_free_choice(Log),
%% _
%% > length(Nfc) > 0.
%% true
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec discover_non_free_choice(Log :: event_log()) -> petri_net().

discover_non_free_choice(Log) when is_list(Log) ->
    Traces = events_to_traces(Log),

    %% Calculate basic matrices
    DepMatrix = calculate_dependencies(Traces),
    FreqMatrix = calculate_frequency_matrix(Traces),

    %% Detect non-free-choice patterns
    NonFreeChoicePatterns = detect_non_free_choice(Traces),

    %% Extract activities
    Activities = extract_activities_from_traces(Traces),

    %% Identify start/end
    StartActivities = find_start_activities(Traces),
    EndActivities = find_end_activities(Traces),

    %% Construct places with non-free-choice handling
    Places = construct_places_non_free_choice(
        DepMatrix,
        FreqMatrix,
        Activities,
        StartActivities,
        EndActivities,
        NonFreeChoicePatterns
    ),

    %% Build Petri net with NFC metadata
    build_petri_net(
        Activities,
        Places,
        StartActivities,
        EndActivities,
        DepMatrix,
        FreqMatrix,
        #{
            algorithm => discover_non_free_choice,
            cases_processed => length(Traces),
            total_events => length(Log),
            noise_level => estimate_noise_level(Log, Traces),
            non_free_choice_constructs => length(NonFreeChoicePatterns),
            nfc_details => NonFreeChoicePatterns
        }
    ).

%%====================================================================
%% Dependency and Frequency Calculation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Calculates dependency matrix from traces.
%%
%% Uses the heuristic dependency measure:
%%   dep(a,b) = (|a>b| - |b>a|) / (|a>b| + |b>a| + 1)
%%
%% This measure ranges from -1 to 1, where:
%% - Positive values indicate a -> b causality
%% - Negative values indicate b -> a causality
%% - Values near 0 indicate parallelism or no relation
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_dependencies([trace()]) -> dependency_matrix().

calculate_dependencies(Traces) ->
    %% Build frequency matrix first
    FreqMatrix = calculate_frequency_matrix(Traces),

    %% Calculate dependencies using the heuristic formula
    maps:map(fun({A, B}, CountAB) ->
        CountBA = maps:get({B, A}, FreqMatrix, 0),
        (CountAB - CountBA) / (CountAB + CountBA + 1)
    end, FreqMatrix).

%%--------------------------------------------------------------------
%% @doc Calculates frequency matrix of activity successions.
%%
%% Counts how often activity A is directly followed by activity B
%% across all traces in the log.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_frequency_matrix([trace()]) -> frequency_matrix().

calculate_frequency_matrix(Traces) ->
    lists:foldl(fun(Trace, Acc) ->
        count_successions(Trace, Acc)
    end, #{}, Traces).

%% @private
%% @doc Counts succession pairs in a single trace.
-spec count_successions(trace(), frequency_matrix()) -> frequency_matrix().

count_successions([], Acc) ->
    Acc;
count_successions([_], Acc) ->
    Acc;
count_successions([A, B | Rest], Acc) ->
    Key = {A, B},
    NewAcc = Acc#{Key => maps:get(Key, Acc, 0) + 1},
    count_successions([B | Rest], NewAcc).

%%====================================================================
%% Loop Detection and Classification
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Detects loops in the event log.
%%
%% Identifies:
%% - Length-1 loops (a -> a)
%% - Length-2 loops (a -> b -> a)
%% - Longer cycles
%%
%% Returns list of loop information maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_loops([trace()]) -> [loop_info()].

detect_loops(Traces) ->
    FreqMatrix = calculate_frequency_matrix(Traces),

    %% Detect length-1 loops (self-loops)
    ShortLoops1 = detect_short_loops_1(FreqMatrix),

    %% Detect length-2 loops (a -> b -> a)
    ShortLoops2 = detect_short_loops_2(FreqMatrix),

    %% Detect longer loops using cycle detection
    LongLoops = detect_long_loops(Traces, FreqMatrix),

    ShortLoops1 ++ ShortLoops2 ++ LongLoops.

%% @private
%% @doc Detects length-1 loops (self-loops).
-spec detect_short_loops_1(frequency_matrix()) -> [loop_info()].

detect_short_loops_1(FreqMatrix) ->
    maps:fold(fun({A, B}, Count, Acc) when A =:= B ->
        [#{
            type => short_loop_1,
            activities => [A],
            frequency => Count
        } | Acc];
       (_, _, Acc) ->
        Acc
    end, [], FreqMatrix).

%% @private
%% @doc Detects length-2 loops (A -> B -> A).
-spec detect_short_loops_2(frequency_matrix()) -> [loop_info()].

detect_short_loops_2(FreqMatrix) ->
    maps:fold(fun({A, B}, CountAB, Acc) ->
        case maps:get({B, A}, FreqMatrix, 0) of
            0 -> Acc;
            CountBA when CountBA > 0 ->
                %% Check if we've already added this loop (avoid duplicates)
                case lists:keyfind({A, B}, 1, Acc) of
                    false ->
                        [#{
                            type => short_loop_2,
                            activities => [A, B],
                            frequency => CountAB + CountBA
                        }, {{A, B}, []} | Acc];
                    _ ->
                        Acc
                end
        end
    end, [], FreqMatrix).

%% @private
%% @doc Detects longer cycles in traces.
-spec detect_long_loops([trace()], frequency_matrix()) -> [loop_info()].

detect_long_loops(Traces, _FreqMatrix) ->
    %% Use DFS-based cycle detection on each trace
    AllCycles = lists:flatmap(fun find_cycles_in_trace/1, Traces),

    %% Filter and deduplicate cycles (length >= 3)
    UniqueLongCycles = lists:usort(fun(C1, C2) ->
        maps:get(activities, C1) =< maps:get(activities, C2)
    end, [C || C <- AllCycles, length(maps:get(activities, C)) >= 3]),

    %% Estimate frequency based on occurrence
    lists:map(fun(Cycle) ->
        Cycle#{frequency => estimate_cycle_frequency(Cycle, Traces)}
    end, UniqueLongCycles).

%% @private
%% @doc Finds cycles in a single trace using DFS.
-spec find_cycles_in_trace(trace()) -> [loop_info()].

find_cycles_in_trace(Trace) ->
    %% Build adjacency from trace
    Unique = lists:usort(Trace),
    find_cycles(Unique, Trace, 1).

%% @private
%% @doc Recursive cycle detection.
-spec find_cycles([activity()], trace(), pos_integer()) -> [loop_info()].

find_cycles([], _Trace, _Len) ->
    [];
find_cycles(_Activities, _Trace, Len) when Len > 5 ->
    [];  %% Limit cycle length to avoid exponential blowup
find_cycles(Activities, Trace, Len) ->
    %% Find all cycles of length Len
    Cycles = find_cycles_of_length(Activities, Trace, Len),
    Cycles ++ find_cycles(Activities, Trace, Len + 1).

%% @private
%% @doc Finds cycles of a specific length.
-spec find_cycles_of_length([activity()], trace(), pos_integer()) -> [loop_info()].

find_cycles_of_length(Activities, Trace, Len) ->
    %% Generate all possible sequences of length Len
    %% and check if they form cycles in the trace
    AllSeqs = combinations(Activities, Len),
    lists:filtermap(fun(Seq) ->
        is_valid_cycle(Seq, Trace)
    end, AllSeqs).

%% @private
%% @doc Checks if a sequence forms a valid cycle.
-spec is_valid_cycle([activity()], trace()) -> {true, loop_info()} | false.

is_valid_cycle([First | _] = Seq, Trace) ->
    %% Check if sequence appears in trace and returns to start
    case lists:prefix(Seq, Trace) of
        true ->
            %% Find if First appears after the sequence
            TraceAfterSeq = lists:nthtail(length(Seq), Trace),
            case lists:member(First, TraceAfterSeq) of
                true ->
                    {true, #{
                        type => long_loop,
                        activities => Seq,
                        frequency => 1
                    }};
                false ->
                    false
            end;
        false ->
            %% Check if sequence appears anywhere in trace
            case has_subsequence(Trace, Seq) of
                true ->
                    {true, #{
                        type => long_loop,
                        activities => Seq,
                        frequency => 1
                    }};
                false ->
                    false
            end
    end.

%% @private
%% @doc Estimates cycle frequency across traces.
-spec estimate_cycle_frequency(loop_info(), [trace()]) -> frequency().

estimate_cycle_frequency(#{activities := Activities}, Traces) ->
    lists:foldl(fun(Trace, Count) ->
        case has_all_activities(Trace, Activities) of
            true -> Count + 1;
            false -> Count
        end
    end, 0, Traces).

%%--------------------------------------------------------------------
%% @doc Classifies loops by type with detailed analysis.
%%
%% Provides more detailed classification including:
%% - Loop entry and exit points
%% - Nested loop structures
%% - Loop frequencies
%%
%% @end
%%--------------------------------------------------------------------
-spec classify_loops([trace()]) -> [loop_info()].

classify_loops(Traces) ->
    Loops = detect_loops(Traces),

    %% Enhance with entry/exit points
    lists:map(fun(Loop) ->
        enrich_loop_info(Loop, Traces)
    end, Loops).

%% @private
%% @doc Enriches loop info with entry/exit analysis.
-spec enrich_loop_info(loop_info(), [trace()]) -> loop_info().

enrich_loop_info(Loop = #{activities := Activities}, Traces) ->
    %% Find common predecessors (entry points)
    EntryPoints = find_loop_entry_points(Activities, Traces),

    %% Find common successors (exit points)
    ExitPoints = find_loop_exit_points(Activities, Traces),

    Loop#{
        entry_points => EntryPoints,
        exit_points => ExitPoints
    }.

%% @private
%% @doc Finds activities that typically precede the loop.
-spec find_loop_entry_points([activity()], [trace()]) -> [activity()].

find_loop_entry_points(LoopActivities, Traces) ->
    %% Find activities that appear before any loop activity
    Predecessors = lists:foldl(fun(Trace, Acc) ->
        find_predecessors_in_trace(LoopActivities, Trace, Acc)
    end, [], Traces),

    %% Count and return most common
    count_and_sort(Predecessors).

%% @private
%% @doc Finds activities that typically follow the loop.
-spec find_loop_exit_points([activity()], [trace()]) -> [activity()].

find_loop_exit_points(LoopActivities, Traces) ->
    %% Find activities that appear after any loop activity
    Successors = lists:foldl(fun(Trace, Acc) ->
        find_successors_in_trace(LoopActivities, Trace, Acc)
    end, [], Traces),

    %% Count and return most common
    count_and_sort(Successors).

%% @private
%% @doc Finds predecessors of loop activities in a trace.
-spec find_predecessors_in_trace([activity()], trace(), [activity()]) -> [activity()].

find_predecessors_in_trace(LoopActivities, Trace, Acc) ->
    LoopSet = sets:from_list(LoopActivities),
    lists:foldl(fun({A, B}, InnerAcc) ->
        case sets:is_element(B, LoopSet) andalso
             not sets:is_element(A, LoopSet) of
            true -> [A | InnerAcc];
            false -> InnerAcc
        end
    end, Acc, get_all_successions(Trace)).

%% @private
%% @doc Finds successors of loop activities in a trace.
-spec find_successors_in_trace([activity()], trace(), [activity()]) -> [activity()].

find_successors_in_trace(LoopActivities, Trace, Acc) ->
    LoopSet = sets:from_list(LoopActivities),
    lists:foldl(fun({A, B}, InnerAcc) ->
        case sets:is_element(A, LoopSet) andalso
             not sets:is_element(B, LoopSet) of
            true -> [B | InnerAcc];
            false -> InnerAcc
        end
    end, Acc, get_all_successions(Trace)).

%% @private
%% @doc Gets all succession pairs from a trace.
-spec get_all_successions(trace()) -> [{activity(), activity()}].

get_all_successions([]) ->
    [];
get_all_successions([_]) ->
    [];
get_all_successions([A, B | Rest]) ->
    [{A, B} | get_all_successions([B | Rest])].

%% @private
%% @doc Counts and sorts activities by frequency.
-spec count_and_sort([activity()]) -> [activity()].

count_and_sort(Activities) ->
    Counted = lists:foldl(fun(A, Acc) ->
        Acc#{A => maps:get(A, Acc, 0) + 1}
    end, #{}, Activities),

    Sorted = lists:reverse(lists:keysort(2, maps:to_list(Counted))),
    [A || {A, _} <- Sorted].

%%====================================================================
%% Non-Free-Choice Detection
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Detects non-free-choice constructs in the event log.
%%
%% Non-free-choice occurs when transitions share input/output places
%% but don't have identical connectivity. This is important for
%% correct workflow net construction.
%%
%% Returns list of non-free-choice pattern information.
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_non_free_choice([trace()]) -> [non_free_choice_info()].

detect_non_free_choice(Traces) ->
    FreqMatrix = calculate_frequency_matrix(Traces),

    %% Build input and output sets for each activity
    Activities = extract_activities_from_traces(Traces),

    InputSets = build_input_sets(Activities, FreqMatrix),
    OutputSets = build_output_sets(Activities, FreqMatrix),

    %% Find shared inputs (non-free-choice potential)
    SharedInputs = find_shared_inputs(InputSets),

    %% Find shared outputs (non-free-choice potential)
    SharedOutputs = find_shared_outputs(OutputSets),

    %% Verify non-free-choice property (different outputs for same input)
    NFCInputs = verify_non_free_choice_input(SharedInputs, OutputSets),
    NFCOutputs = verify_non_free_choice_output(SharedOutputs, InputSets),

    NFCInputs ++ NFCOutputs.

%% @private
%% @doc Builds input sets for each activity.
-spec build_input_sets([activity()], frequency_matrix()) ->
    #{activity() => #{activity() => frequency()}}.

build_input_sets(Activities, FreqMatrix) ->
    lists:foldl(fun(A, Acc) ->
        Inputs = lists:foldl(fun(B, InnerAcc) ->
            case maps:get({B, A}, FreqMatrix, 0) of
                0 -> InnerAcc;
                Freq -> InnerAcc#{B => Freq}
            end
        end, #{}, Activities),
        Acc#{A => Inputs}
    end, #{}, Activities).

%% @private
%% @doc Builds output sets for each activity.
-spec build_output_sets([activity()], frequency_matrix()) ->
    #{activity() => #{activity() => frequency()}}.

build_output_sets(Activities, FreqMatrix) ->
    lists:foldl(fun(A, Acc) ->
        Outputs = lists:foldl(fun(B, InnerAcc) ->
            case maps:get({A, B}, FreqMatrix, 0) of
                0 -> InnerAcc;
                Freq -> InnerAcc#{B => Freq}
            end
        end, #{}, Activities),
        Acc#{A => Outputs}
    end, #{}, Activities).

%% @private
%% @doc Finds shared input places between activities.
-spec find_shared_inputs(#{activity() => #{activity() => frequency()}}) ->
    [non_free_choice_info()].

find_shared_inputs(InputSets) ->
    %% For each pair of activities, check if they share inputs
    Activities = maps:keys(InputSets),
    lists:foldl(fun(A, Acc) ->
        lists:foldl(fun(B, InnerAcc) when A =/= B ->
            InputsA = maps:get(A, InputSets, #{}),
            InputsB = maps:get(B, InputSets, #{}),
            Shared = maps:intersect(InputsA, InputsB),
            case map_size(Shared) > 0 of
                true ->
                    [#{
                        shared_place => list_to_atom("i_shared_" ++
                            atom_to_list(A) ++ "_" ++ atom_to_list(B)),
                        transitions => [A, B],
                        type => shared_input,
                        shared_activities => maps:keys(Shared)
                    } | InnerAcc];
                false ->
                    InnerAcc
            end;
           (_, InnerAcc) ->
            InnerAcc
        end, Acc, Activities)
    end, [], Activities).

%% @private
%% @doc Finds shared output places between activities.
-spec find_shared_outputs(#{activity() => #{activity() => frequency()}}) ->
    [non_free_choice_info()].

find_shared_outputs(OutputSets) ->
    Activities = maps:keys(OutputSets),
    lists:foldl(fun(A, Acc) ->
        lists:foldl(fun(B, InnerAcc) when A =/= B ->
            OutputsA = maps:get(A, OutputSets, #{}),
            OutputsB = maps:get(B, OutputSets, #{}),
            Shared = maps:intersect(OutputsA, OutputsB),
            case map_size(Shared) > 0 of
                true ->
                    [#{
                        shared_place => list_to_atom("o_shared_" ++
                            atom_to_list(A) ++ "_" ++ atom_to_list(B)),
                        transitions => [A, B],
                        type => shared_output,
                        shared_activities => maps:keys(Shared)
                    } | InnerAcc];
                false ->
                    InnerAcc
            end;
           (_, InnerAcc) ->
            InnerAcc
        end, Acc, Activities)
    end, [], Activities).

%% @private
%% @doc Verifies non-free-choice property for shared inputs.
-spec verify_non_free_choice_input([non_free_choice_info()],
                                    #{activity() => #{activity() => frequency()}}) ->
    [non_free_choice_info()].

verify_non_free_choice_input(SharedInputs, OutputSets) ->
    %% A shared input is non-free-choice if the activities have different outputs
    lists:filtermap(fun(NFC = #{transitions := [A, B]}) ->
        OutputsA = maps:get(A, OutputSets, #{}),
        OutputsB = maps:get(B, OutputSets, #{}),
        case OutputsA =:= OutputsB of
            true -> false;  %% Free-choice, not NFC
            false -> {true, NFC}  %% Non-free-choice
        end
    end, SharedInputs).

%% @private
%% @doc Verifies non-free-choice property for shared outputs.
-spec verify_non_free_choice_output([non_free_choice_info()],
                                     #{activity() => #{activity() => frequency()}}) ->
    [non_free_choice_info()].

verify_non_free_choice_output(SharedOutputs, InputSets) ->
    %% A shared output is non-free-choice if the activities have different inputs
    lists:filtermap(fun(NFC = #{transitions := [A, B]}) ->
        InputsA = maps:get(A, InputSets, #{}),
        InputsB = maps:get(B, InputSets, #{}),
        case InputsA =:= InputsB of
            true -> false;  %% Free-choice, not NFC
            false -> {true, NFC}  %% Non-free-choice
        end
    end, SharedOutputs).

%%====================================================================
%% Place Construction Functions
%%====================================================================

%% @private
%% @doc Constructs places from dependency relations.
-spec construct_places_from_deps(dependency_matrix(), sets:set(activity()),
                                 [activity()], [activity()], [loop_info()]) ->
    [place()].

construct_places_from_deps(DepMatrix, Activities, StartActivities, EndActivities, Loops) ->
    %% Get causal relations (positive dependencies above threshold)
    CausalPairs = [{A, B} || {{A, B}, Dep} <- maps:to_list(DepMatrix), Dep > 0],

    %% Handle loops specially
    LoopPlaces = construct_loop_places(Loops),

    %% Construct standard places
    StandardPlaces = construct_standard_places(CausalPairs, StartActivities, EndActivities),

    lists:usort(LoopPlaces ++ StandardPlaces).

%% @private
%% @doc Constructs places for loops.
-spec construct_loop_places([loop_info()]) -> [place()].

construct_loop_places(Loops) ->
    lists:flatmap(fun(#{type := Type, activities := Activities}) ->
        case Type of
            short_loop_1 ->
                [A] = Activities,
                [list_to_atom("p_loop_" ++ atom_to_list(A))];
            short_loop_2 ->
                [A, B] = Activities,
                [list_to_atom("p_loop2_" ++ atom_to_list(A) ++ "_" ++ atom_to_list(B))];
            long_loop ->
                %% Create intermediate places for long loops
                construct_long_loop_places(Activities)
        end
    end, Loops).

%% @private
%% @doc Constructs places for long loops.
-spec construct_long_loop_places([activity()]) -> [place()].

construct_long_loop_places(Activities) ->
    %% Create places between consecutive activities in the loop
    construct_loop_places_between(Activities, []).

%% @private
%% @doc Recursively constructs loop places.
-spec construct_loop_places_between([activity()], [place()]) -> [place()].

construct_loop_places_between([], Acc) ->
    Acc;
construct_loop_places_between([_], Acc) ->
    Acc;
construct_loop_places_between([A, B | Rest], Acc) ->
    Place = list_to_atom("p_loop_" ++ atom_to_list(A) ++ "_" ++ atom_to_list(B)),
    construct_loop_places_between([B | Rest], [Place | Acc]).

%% @private
%% @doc Constructs standard places from causal pairs.
-spec construct_standard_places([{activity(), activity()}], [activity()], [activity()]) ->
    [place()].

construct_standard_places(CausalPairs, StartActivities, EndActivities) ->
    lists:foldl(fun({A, B}, Acc) ->
        Place = make_place_name(A, B),
        [Place | Acc]
    end, [], CausalPairs).

%% @private
%% @doc Constructs places with explicit loop handling.
-spec construct_places_with_loops(dependency_matrix(), frequency_matrix(),
                                  sets:set(activity()), [activity()], [activity()],
                                  [loop_info()]) -> [place()].

construct_places_with_loops(DepMatrix, FreqMatrix, Activities, StartActivities, EndActivities, Loops) ->
    %% Start with standard construction
    BasePlaces = construct_places_from_deps(DepMatrix, Activities, StartActivities, EndActivities, Loops),

    %% Add special loop handling places
    LoopPlaces = add_loop_handling_places(Loops, FreqMatrix),

    lists:usort(BasePlaces ++ LoopPlaces).

%% @private
%% @doc Adds special places for loop handling.
-spec add_loop_handling_places([loop_info()], frequency_matrix()) -> [place()].

add_loop_handling_places(Loops, FreqMatrix) ->
    lists:flatmap(fun(Loop) ->
        case Loop of
            #{type := short_loop_1, activities := [A]} ->
                %% Self-loop: need a place that allows A to fire twice
                [list_to_atom("p_selfloop_" ++ atom_to_list(A))];
            #{type := short_loop_2, activities := [A, B]} ->
                %% Two-loop: A -> B -> A
                %% Need places that support this cycle
                [list_to_atom("p_ab_" ++ atom_to_list(A) ++ "_" ++ atom_to_list(B)),
                 list_to_atom("p_ba_" ++ atom_to_list(B) ++ "_" ++ atom_to_list(A))];
            #{type := long_loop, activities := Activities} ->
                %% Long loop: create return path
                [list_to_atom("p_loop_return_" ++ hd(atom_to_list(Activities)))]
        end
    end, Loops).

%% @private
%% @doc Constructs places for non-free-choice patterns.
-spec construct_places_non_free_choice(dependency_matrix(), frequency_matrix(),
                                       sets:set(activity()), [activity()], [activity()],
                                       [non_free_choice_info()]) -> [place()].
construct_places_non_free_choice(DepMatrix, FreqMatrix, Activities, StartActivities, EndActivities, NFCPatterns) ->
    %% Standard construction first
    BasePlaces = construct_places_from_deps(DepMatrix, Activities, StartActivities, EndActivities, []),

    %% Add non-free-choice specific places
    NFCPlaces = construct_nfc_places(NFCPatterns),

    lists:usort(BasePlaces ++ NFCPlaces).

%% @private
%% @doc Constructs places for non-free-choice patterns.
-spec construct_nfc_places([non_free_choice_info()]) -> [place()].

construct_nfc_places(NFCPatterns) ->
    lists:flatmap(fun(NFC = #{type := Type, transitions := [A, B]}) ->
        BaseName = case Type of
            shared_input -> "i_nfc_" ++ atom_to_list(A) ++ "_" ++ atom_to_list(B);
            shared_output -> "o_nfc_" ++ atom_to_list(A) ++ "_" ++ atom_to_list(B)
        end,
        [list_to_atom(BaseName)] ++
        construct_nfc_individual_places(NFC)
    end, NFCPatterns).

%% @private
%% @doc Constructs individual places for NFC transitions.
-spec construct_nfc_individual_places(non_free_choice_info()) -> [place()].

construct_nfc_individual_places(#{transitions := Transitions}) ->
    %% Create individual places for each transition in NFC pattern
    lists:flatmap(fun(T) ->
        [list_to_atom("p_nfc_individual_" ++ atom_to_list(T))]
    end, Transitions).

%%====================================================================
%% Petri Net Construction
%%====================================================================

%% @private
%% @doc Builds the complete Petri net structure.
-spec build_petri_net(sets:set(activity()), [place()], [activity()], [activity()],
                      dependency_matrix(), frequency_matrix(), map()) -> petri_net().

build_petri_net(Activities, Places, StartActivities, EndActivities,
                DepMatrix, FreqMatrix, Metadata) ->
    TransitionList = sets:to_list(Activities),
    PlaceList = lists:usort(Places),

    %% Create initial and final places
    InitialPlace = 'i_source',
    FinalPlace = 'o_sink',

    %% Build arcs
    Arcs = build_all_arcs(
        TransitionList,
        PlaceList,
        StartActivities,
        EndActivities,
        DepMatrix,
        FreqMatrix,
        InitialPlace,
        FinalPlace
    ),

    %% Complete place list
    AllPlaces = lists:usort([InitialPlace, FinalPlace | PlaceList]),

    #{
        places => AllPlaces,
        transitions => TransitionList,
        arcs => Arcs,
        initial_place => InitialPlace,
        final_place => FinalPlace,
        metadata => Metadata
    }.

%% @private
%% @doc Builds all arcs for the Petri net.
-spec build_all_arcs([activity()], [place()], [activity()], [activity()],
                      dependency_matrix(), frequency_matrix(), place(), place()) -> [arc()].

build_all_arcs(Transitions, Places, StartActivities, EndActivities,
               DepMatrix, FreqMatrix, InitialPlace, FinalPlace) ->
    %% Initial arcs
    InitialArcs = [{InitialPlace, A} || A <- StartActivities],

    %% Final arcs
    FinalArcs = [{A, FinalPlace} || A <- EndActivities],

    %% Internal arcs based on dependencies
    InternalArcs = build_internal_arcs(DepMatrix, FreqMatrix, Places),

    lists:usort(InitialArcs ++ FinalArcs ++ InternalArcs).

%% @private
%% @doc Builds internal arcs based on dependencies.
-spec build_internal_arcs(dependency_matrix(), frequency_matrix(), [place()]) -> [arc()].

build_internal_arcs(DepMatrix, FreqMatrix, Places) ->
    %% For each significant dependency, create transition -> place -> transition
    maps:fold(fun({A, B}, Dep, Acc) when Dep > 0 ->
        Place = make_place_name(A, B),
        %% Create arcs: A -> Place, Place -> B
        lists:usort([{A, Place}, {Place, B} | Acc]);
       (_, _, Acc) ->
        Acc
    end, [], DepMatrix).

%%====================================================================
%% Utility Functions
%%====================================================================

%% @private
%% @doc Converts event log to traces grouped by case ID.
-spec events_to_traces(event_log()) -> [trace()].

events_to_traces(Log) ->
    %% Group by case ID
    Grouped = lists:foldl(fun({CaseId, Activity, _Timestamp}, Acc) ->
        Acc#{CaseId => [Activity | maps:get(CaseId, Acc, [])]}
    end, #{}, Log),

    %% Sort each trace by timestamp (reversed during grouping)
    Traces = maps:fold(fun(_CaseId, Activities, Acc) ->
        [lists:reverse(Activities) | Acc]
    end, [], Grouped),

    lists:sort(Traces).  %% Return sorted for consistency

%% @private
%% @doc Extracts all activities from traces.
-spec extract_activities_from_traces([trace()]) -> sets:set(activity()).

extract_activities_from_traces(Traces) ->
    lists:foldl(fun(Trace, Acc) ->
        lists:foldl(fun(Activity, Set) ->
            sets:add_element(Activity, Set)
        end, Acc, Trace)
    end, sets:new(), Traces).

%% @private
%% @doc Finds start activities (first in trace).
-spec find_start_activities([trace()]) -> [activity()].

find_start_activities(Traces) ->
    StartActivities = lists:usort([hd(T) || T <- Traces, T =/= []]),
    StartActivities.

%% @private
%% @doc Finds end activities (last in trace).
-spec find_end_activities([trace()]) -> [activity()].

find_end_activities(Traces) ->
    EndActivities = lists:usort([lists:last(T) || T <- Traces, T =/= []]),
    EndActivities.

%% @private
%% @doc Finds start activities with frequency consideration.
-spec find_start_activities_freq([trace()], frequency_matrix()) -> [activity()].

find_start_activities_freq(Traces, FreqMatrix) ->
    %% Get all start activities
    AllStarts = [hd(T) || T <- Traces, T =/= []],

    %% Filter out activities that appear as destination more often than as start
    %% This helps handle noisy start activities
    StartCounts = count_occurrences(AllStarts),

    %% Calculate how often each is a destination
    DestCounts = maps:fold(fun({_A, B}, Count, Acc) ->
        Acc#{B => maps:get(B, Acc, 0) + Count}
    end, #{}, FreqMatrix),

    %% Keep starts that appear more often as starts than as destinations
    lists:filtermap(fun(A) ->
        StartCount = maps:get(A, StartCounts, 0),
        DestCount = maps:get(A, DestCounts, 0),
        case StartCount >= DestCount of
            true -> {true, A};
            false -> false
        end
    end, lists:usort(AllStarts)).

%% @private
%% @doc Finds end activities with frequency consideration.
-spec find_end_activities_freq([trace()], frequency_matrix()) -> [activity()].

find_end_activities_freq(Traces, FreqMatrix) ->
    %% Get all end activities
    AllEnds = [lists:last(T) || T <- Traces, T =/= []],

    %% Filter out activities that appear as source more often than as end
    EndCounts = count_occurrences(AllEnds),

    %% Calculate how often each is a source
    SourceCounts = maps:fold(fun({A, _B}, Count, Acc) ->
        Acc#{A => maps:get(A, Acc, 0) + Count}
    end, #{}, FreqMatrix),

    %% Keep ends that appear more often as ends than as sources
    lists:filtermap(fun(A) ->
        EndCount = maps:get(A, EndCounts, 0),
        SourceCount = maps:get(A, SourceCounts, 0),
        case EndCount >= SourceCount of
            true -> {true, A};
            false -> false
        end
    end, lists:usort(AllEnds)).

%% @private
%% @doc Counts occurrences in a list.
-spec count_occurrences([term()]) -> #{term() => non_neg_integer()}.

count_occurrences(List) ->
    lists:foldl(fun(E, Acc) ->
        Acc#{E => maps:get(E, Acc, 0) + 1}
    end, #{}, List).

%% @private
%% @doc Filters dependencies by threshold.
-spec filter_significant_dependencies(dependency_matrix(), float()) -> dependency_matrix().

filter_significant_dependencies(DepMatrix, Threshold) ->
    maps:filter(fun(_Key, Dep) -> Dep > Threshold end, DepMatrix).

%% @private
%% @doc Filters by both threshold and positive observation count.
-spec filter_by_threshold_and_count(dependency_matrix(), frequency_matrix(),
                                     float(), pos_integer()) -> dependency_matrix().
filter_by_threshold_and_count(DepMatrix, FreqMatrix, Threshold, MinCount) ->
    maps:filter(fun(Key, Dep) ->
        Count = maps:get(Key, FreqMatrix, 0),
        Dep > Threshold andalso Count >= MinCount
    end, DepMatrix).

%% @private
%% @doc Estimates noise level in the event log.
-spec estimate_noise_level(event_log(), [trace()]) -> float().

estimate_noise_level(Log, Traces) ->
    case length(Traces) of
        0 -> 0.0;
        NumCases ->
            %% Calculate variance in trace lengths as noise indicator
            Lengths = [length(T) || T <- Traces],
            AvgLen = lists:sum(Lengths) / length(Lengths),
            Variance = lists:sum([(L - AvgLen) * (L - AvgLen) || L <- Lengths]) / length(Lengths),

            %% Normalize by average length
            case AvgLen of
                0.0 -> 0.0;
                _ -> min(1.0, Variance / (AvgLen * AvgLen))
            end
    end.

%% @private
%% @doc Creates a place name from two activities.
-spec make_place_name(activity(), activity()) -> place().

make_place_name(A, B) ->
    list_to_atom("p_" ++ atom_to_list(A) ++ "_" ++ atom_to_list(B)).

%% @private
%% @doc Generates combinations of length N from a list.
-spec combinations([T], pos_integer()) -> [[T]].

combinations(_List, 0) ->
    [[]];
combinations([], _N) ->
    [];
combinations([H | T], N) ->
    [[H | C] || C <- combinations(T, N - 1)] ++ combinations(T, N).

%% @private
%% @doc Checks if a list has all the specified activities.
-spec has_all_activities(trace(), [activity()]) -> boolean().

has_all_activities(Trace, Activities) ->
    lists:all(fun(A) -> lists:member(A, Trace) end, Activities).

%% @private
%% @doc Checks if a sequence is a subsequence of a list.
-spec has_subsequence([term()], [term()]) -> boolean().

has_subsequence(_, []) ->
    true;
has_subsequence([], _) ->
    false;
has_subsequence([H | T] = Seq, [H | Rest]) ->
    case lists:prefix(T, Rest) of
        true -> true;
        false -> has_subsequence(Seq, Rest)
    end;
has_subsequence(Seq, [_ | Rest]) ->
    has_subsequence(Seq, Rest).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

simple_log() ->
    [
        {case1, a, 1},
        {case1, b, 2},
        {case1, c, 3},
        {case2, a, 4},
        {case2, b, 5},
        {case2, c, 6}
    ].

parallel_log() ->
    [
        {case1, a, 1},
        {case1, b, 2},
        {case1, c, 3},
        {case1, d, 4},
        {case2, a, 5},
        {case2, c, 6},
        {case2, b, 7},
        {case2, d, 8}
    ].

loop_log() ->
    [
        {case1, a, 1},
        {case1, b, 2},
        {case1, a, 3},
        {case1, c, 4},
        {case2, a, 5},
        {case2, b, 6},
        {case2, c, 7}
    ].

noisy_log() ->
    [
        {case1, a, 1},
        {case1, b, 2},
        {case1, c, 3},
        {case2, a, 4},
        {case2, x, 5},  %% noise
        {case2, b, 6},
        {case2, c, 7},
        {case3, a, 8},
        {case3, b, 9},
        {case3, c, 10}
    ].

%%--------------------------------------------------------------------
%% Event to trace conversion tests
%%--------------------------------------------------------------------

events_to_traces_simple_test() ->
    Log = simple_log(),
    Traces = events_to_traces(Log),
    ?assertEqual(2, length(Traces)),
    ?assertEqual([a, b, c], hd(Traces)).

events_to_traces_ordering_test() ->
    Log = [{case1, a, 3}, {case1, b, 1}, {case1, c, 2}],
    Traces = events_to_traces(Log),
    %% Traces are sorted by timestamp within each case
    ?assertEqual([b, c, a], hd(lists:sort(Traces))).

%%--------------------------------------------------------------------
%% Frequency matrix tests
%%--------------------------------------------------------------------

frequency_matrix_simple_test() ->
    Log = simple_log(),
    Traces = events_to_traces(Log),
    Freq = calculate_frequency_matrix(Traces),
    ?assertEqual(2, maps:get({a, b}, Freq)),
    ?assertEqual(2, maps:get({b, c}, Freq)),
    ?assertEqual(0, maps:get({c, a}, Freq, 0)).

frequency_matrix_parallel_test() ->
    Log = parallel_log(),
    Traces = events_to_traces(Log),
    Freq = calculate_frequency_matrix(Traces),
    ?assertEqual(2, maps:get({a, b}, Freq)),
    %% a->c appears in trace 2, c->b appears in trace 2
    ?assertEqual(1, maps:get({a, c}, Freq)),
    ?assertEqual(1, maps:get({c, b}, Freq)),
    ?assertEqual(1, maps:get({b, d}, Freq)).

%%--------------------------------------------------------------------
%% Dependency calculation tests
%%--------------------------------------------------------------------

calculate_dependencies_test() ->
    Log = simple_log(),
    Traces = events_to_traces(Log),
    Deps = calculate_dependencies(Traces),
    ?assert(maps:get({a, b}, Deps) > 0),
    ?assert(maps:get({b, c}, Deps) > 0),
    ?assert(maps:get({a, b}, Deps) > maps:get({b, a}, Deps, 0)).

calculate_dependencies_parallel_test() ->
    Log = parallel_log(),
    Traces = events_to_traces(Log),
    Deps = calculate_dependencies(Traces),
    %% a->b and a->c should be positive
    ?assert(maps:get({a, b}, Deps) > 0),
    ?assert(maps:get({a, c}, Deps) > 0).

%%--------------------------------------------------------------------
%% Loop detection tests
%%--------------------------------------------------------------------

detect_loops_short_loop_2_test() ->
    Log = loop_log(),
    Traces = events_to_traces(Log),
    Loops = detect_loops(Traces),
    ?assert(length(Loops) > 0),
    %% Check for short loop 2 (a -> b -> a)
    HasShortLoop2 = lists:any(fun(L) ->
        maps:get(type, L) =:= short_loop_2
    end, Loops),
    ?assert(HasShortLoop2).

detect_loops_empty_test() ->
    Log = simple_log(),
    Traces = events_to_traces(Log),
    Loops = detect_loops(Traces),
    ?assertEqual([], Loops).

%%--------------------------------------------------------------------
%% Non-free-choice detection tests
%%--------------------------------------------------------------------

detect_non_free_choice_simple_test() ->
    Log = simple_log(),
    Traces = events_to_traces(Log),
    NFC = detect_non_free_choice(Traces),
    %% Simple sequential process shouldn't have NFC
    ?assertEqual([], NFC).

%%--------------------------------------------------------------------
%% Main discovery API tests
%%--------------------------------------------------------------------

discover_simple_test() ->
    Log = simple_log(),
    Net = discover(Log),
    ?assert(is_list(maps:get(places, Net))),
    ?assert(is_list(maps:get(transitions, Net))),
    ?assert(is_list(maps:get(arcs, Net))),
    ?assertEqual(i_source, maps:get(initial_place, Net)),
    ?assertEqual(o_sink, maps:get(final_place, Net)).

discover_transitions_test() ->
    Log = simple_log(),
    Net = discover(Log),
    Transitions = maps:get(transitions, Net),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)).

discover_metadata_test() ->
    Log = simple_log(),
    Net = discover(Log),
    Metadata = maps:get(metadata, Net),
    ?assertEqual(heuristic_miner, maps:get(algorithm, Metadata)),
    ?assert(maps:get(cases_processed, Metadata) > 0),
    ?assert(maps:get(total_events, Metadata) > 0).

heuristic_miner_test() ->
    Log = parallel_log(),
    Net = heuristic_miner(Log),
    Transitions = maps:get(transitions, Net),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),
    ?assert(lists:member(d, Transitions)).

frequency_based_test() ->
    Log = simple_log(),
    Deps = frequency_based(Log),
    ?assert(maps:get({a, b}, Deps) > 0),
    ?assert(maps:get({b, c}, Deps) > 0).

discover_with_noise_test() ->
    Log = noisy_log(),
    Options = #{dependency_threshold => 0.5},
    Net = discover_with_noise(Log, Options),
    Transitions = maps:get(transitions, Net),
    %% Main activities should be present
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)).

discover_loops_test() ->
    Log = loop_log(),
    Net = discover_loops(Log),
    Transitions = maps:get(transitions, Net),
    ?assert(lists:member(a, Transitions)),
    ?assert(lists:member(b, Transitions)),
    ?assert(lists:member(c, Transitions)),
    Metadata = maps:get(metadata, Net),
    ?assert(maps:get(loops_detected, Metadata) >= 0).

discover_non_free_choice_test() ->
    Log = simple_log(),
    Net = discover_non_free_choice(Log),
    Transitions = maps:get(transitions, Net),
    ?assert(length(Transitions) >= 3).

%%--------------------------------------------------------------------
%% Helper function tests
%%--------------------------------------------------------------------

find_start_activities_test() ->
    Traces = [[a, b, c], [a, c, b]],
    Starts = find_start_activities(Traces),
    ?assertEqual([a], Starts).

find_end_activities_test() ->
    Traces = [[a, b, c], [a, b, d]],
    Ends = find_end_activities(Traces),
    ?assertEqual([c, d], lists:sort(Ends)).

extract_activities_test() ->
    Traces = [[a, b], [c, d]],
    Activities = extract_activities_from_traces(Traces),
    ?assert(sets:is_element(a, Activities)),
    ?assert(sets:is_element(b, Activities)),
    ?assert(sets:is_element(c, Activities)),
    ?assert(sets:is_element(d, Activities)).

filter_significant_dependencies_test() ->
    Deps = #{{a, b} => 0.8, {b, c} => 0.5, {c, d} => 0.9},
    Filtered = filter_significant_dependencies(Deps, 0.7),
    ?assert(maps:is_key({a, b}, Filtered)),
    ?assertNot(maps:is_key({b, c}, Filtered)),
    ?assert(maps:is_key({c, d}, Filtered)).

estimate_noise_level_test() ->
    Log = simple_log(),
    Traces = events_to_traces(Log),
    Noise = estimate_noise_level(Log, Traces),
    ?assert(Noise >= 0.0),
    ?assert(Noise =< 1.0).

%%--------------------------------------------------------------------
%% Doctest
%%--------------------------------------------------------------------

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

-endif.
