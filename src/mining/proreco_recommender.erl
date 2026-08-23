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
%% @doc ProReco Recommender - Process-Aware Recommendation System
%%
%% This module implements a process discovery recommender system based on
%% the ProReco approach (van der Aalst, 2025). It analyzes event log
%% characteristics and recommends the most suitable process discovery
%% algorithm.
%%
%% <h3>Key Concepts</h3>
%%
%% <ul>
%%   <li><b>Log Characteristics:</b> Size, complexity, variability metrics</li>
%%   <li><b>Algorithm Profiling:</b> Performance characteristics of algorithms</li>
%%   <li><b>Transition System:</b> State-based representation for recommendations</li>
%%   <li><b>Recommendation Engine:</b> Multi-criteria decision making</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(proreco_recommender).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([analyze_log/1, recommend_algorithm/1]).
-export([get_recommendation/1, set_algorithm_profile/2]).
-export([get_state/0, reset_learning/0]).
-export([extract_characteristics/1, generate_recommendation/2, score_algorithm/2]).
-export([init_default_profiles/0, normalize_confidence/2]).
-export([estimate_loops/1, count_repeats/1, categorize_1/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(state, {
    transition_system :: map(),
    algorithm_profiles :: map(),
    recommendation_cache :: map(),
    learning_data :: map(),
    cache_size :: pos_integer()
}).

-record(log_characteristics, {
    trace_count :: non_neg_integer(),
    event_count :: non_neg_integer(),
    unique_activities :: non_neg_integer(),
    variability :: float(),
    concurrency :: float(),
    loops :: float(),
    noise :: float()
}).

-record(algorithm_profile, {
    name :: atom(),
    strengths :: [atom()],
    weaknesses :: [atom()],
    complexity :: low | medium | high,
    scalability :: low | medium | high,
    noise_tolerance :: float()
}).

-record(transition_system, {
    states :: map(),
    transitions :: map(),
    current_state :: term()
}).

%% Types
-type log_characteristics() :: #log_characteristics{}.
-type algorithm_profile() :: #algorithm_profile{}.
-type recommendation_result() :: #{
    algorithm => atom(),
    confidence => float(),
    reasoning => [binary()],
    alternatives => [{atom(), float()}]
}.

-export_type([log_characteristics/0, algorithm_profile/0, recommendation_result/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Analyzes an event log and extracts its characteristics.
-spec analyze_log(map()) -> {ok, log_characteristics()}.
analyze_log(EventLog) ->
    gen_server:call(?SERVER, {analyze_log, EventLog}).

%% @doc Recommends the best algorithm for the given event log.
-spec recommend_algorithm(map()) -> {ok, recommendation_result()}.
recommend_algorithm(EventLog) ->
    gen_server:call(?SERVER, {recommend, EventLog}).

%% @doc Gets a recommendation based on pre-computed characteristics.
-spec get_recommendation(log_characteristics()) -> {ok, recommendation_result()}.
get_recommendation(Characteristics) ->
    gen_server:call(?SERVER, {get_recommendation, Characteristics}).

%% @doc Sets or updates an algorithm profile.
-spec set_algorithm_profile(atom(), algorithm_profile()) -> ok.
set_algorithm_profile(Name, Profile) ->
    gen_server:cast(?SERVER, {set_profile, Name, Profile}).

%% @doc Gets the current transition system state.
-spec get_state() -> {ok, map()}.
get_state() ->
    gen_server:call(?SERVER, get_state).

%% @doc Resets learning data.
-spec reset_learning() -> ok.
reset_learning() ->
    gen_server:cast(?SERVER, reset_learning).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize with default algorithm profiles
    Profiles = init_default_profiles(),
    State = #state{
        transition_system = init_transition_system(),
        algorithm_profiles = Profiles,
        recommendation_cache = #{},
        learning_data = #{},
        cache_size = 1000
    },
    {ok, State}.

handle_call({analyze_log, EventLog}, _From, State) ->
    Characteristics = extract_characteristics(EventLog),
    {reply, {ok, Characteristics}, State};

handle_call({recommend, EventLog}, _From, State) ->
    Characteristics = extract_characteristics(EventLog),
    Recommendation = generate_recommendation(Characteristics, State),
    %% Update learning data
    NewLearning = update_learning(State#state.learning_data, Characteristics, Recommendation),
    NewState = State#state{learning_data = NewLearning},
    {reply, {ok, Recommendation}, NewState};

handle_call({get_recommendation, Characteristics}, _From, State) ->
    Recommendation = generate_recommendation(Characteristics, State),
    {reply, {ok, Recommendation}, State};

handle_call(get_state, _From, State) ->
    TS = State#state.transition_system,
    {reply, {ok, TS}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({set_profile, Name, Profile}, State) ->
    NewProfiles = maps:put(Name, Profile, State#state.algorithm_profiles),
    {noreply, State#state{algorithm_profiles = NewProfiles}};

handle_cast(reset_learning, State) ->
    {noreply, State#state{learning_data = #{}, recommendation_cache = #{}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
init_default_profiles() ->
    #{
        alpha => #algorithm_profile{
            name = alpha,
            strengths = [simple, structured, fast],
            weaknesses = [noise, parallel, loops],
            complexity = low,
            scalability = high,
            noise_tolerance = 0.1
        },
        heuristic => #algorithm_profile{
            name = heuristic,
            strengths = [noise, parallel, scalable],
            weaknesses = [complex_loops, invisible],
            complexity = medium,
            scalability = high,
            noise_tolerance = 0.3
        },
        inductive => #algorithm_profile{
            name = inductive,
            strengths = [parallel, loops, structured],
            weaknesses = [noise, invisible],
            complexity = medium,
            scalability = medium,
            noise_tolerance = 0.2
        },
        genetic => #algorithm_profile{
            name = genetic,
            strengths = [unstructured, complex, flexible],
            weaknesses = [slow, random_quality],
            complexity = high,
            scalability = low,
            noise_tolerance = 0.4
        }
    }.

%% @private
init_transition_system() ->
    #{
        states => #{initial => #{transitions => [analyze, recommend]}},
        transitions => #{
            analyze => #{target => analyzing},
            recommend => #{target => recommending}
        },
        current_state => initial
    }.

%% @private
extract_characteristics(EventLog) ->
    %% Extract traces from log
    Traces = case maps:get(traces, EventLog, undefined) of
        undefined when is_list(EventLog) -> EventLog;
        T when is_list(T) -> T;
        _ -> []
    end,

    TraceCount = length(Traces),
    EventCount = lists:sum([length(T) || T <- Traces]),

    %% Get unique activities
    Activities = lists:usort(lists:flatten(Traces)),
    UniqueActivities = length(Activities),

    %% Calculate variability (unique traces / total traces)
    UniqueTraces = length(lists:usort(Traces)),
    Variability = case TraceCount of
        0 -> 0.0;
        _ -> UniqueTraces / TraceCount
    end,

    %% Calculate concurrency (using simple heuristic)
    Concurrency = estimate_concurrency(Traces),

    %% Calculate loops (repeated activities in trace)
    Loops = estimate_loops(Traces),

    %% Estimate noise (infrequent patterns)
    Noise = estimate_noise(Traces),

    #log_characteristics{
        trace_count = TraceCount,
        event_count = EventCount,
        unique_activities = UniqueActivities,
        variability = Variability,
        concurrency = Concurrency,
        loops = Loops,
        noise = Noise
    }.

%% @private
estimate_concurrency(Traces) ->
    case Traces of
        [] -> 0.0;
        _ ->
            %% Count how often activities appear in different orders
            ActivityOrders = lists:foldl(fun(Trace, Acc) ->
                extract_orders(Trace, Acc)
            end, #{}, Traces),

            %% Calculate average variation in ordering
            OrdersList = maps:values(ActivityOrders),
            case OrdersList of
                [] -> 0.0;
                _ ->
                    AvgOrders = lists:sum([length(O) || O <- OrdersList]) / length(OrdersList),
                    min(1.0, AvgOrders / 3.0)
            end
    end.

extract_orders(Trace, Acc) ->
    %% Extract adjacent pairs (handle single-element traces)
    Pairs = try
        case Trace of
            [_] -> [];
            [_ | Rest] -> lists:zip(Trace, Rest)
        end
    catch
        _:_ -> []
    end,
    lists:foldl(fun({A, B}, InnerAcc) ->
        Key = {A, B},
        InnerAcc#{Key => maps:get(Key, InnerAcc, 0) + 1}
    end, Acc, Pairs).

%% @private
estimate_loops(Traces) ->
    case Traces of
        [] -> 0.0;
        _ ->
            %% Count activities that repeat in traces
            LoopCounts = [count_repeats(T) || T <- Traces],
            case length(LoopCounts) of
                0 -> 0.0;
                _ -> lists:sum(LoopCounts) / length(LoopCounts)
            end
    end.

count_repeats(Trace) ->
    Unique = lists:usort(Trace),
    TraceLen = length(Trace),
    UniqueLen = length(Unique),
    case TraceLen of
        0 -> 0;
        _ -> (TraceLen - UniqueLen) / TraceLen
    end.

%% @private
estimate_noise(Traces) ->
    case length(Traces) of
        N when N < 2 -> 0.0;
        _ ->
            %% Identify outliers using trace length deviation
            Lengths = [length(T) || T <- Traces],
            AvgLen = lists:sum(Lengths) / length(Lengths),
            Variance = lists:sum([math:pow(L - AvgLen, 2) || L <- Lengths]) / length(Lengths),
            StdDev = math:sqrt(Variance),
            %% Outliers are more than 2 std deviations away
            Outliers = [L || L <- Lengths, abs(L - AvgLen) > 2 * StdDev],
            min(1.0, length(Outliers) / length(Lengths))
    end.

%% @private
generate_recommendation(Characteristics, #state{algorithm_profiles = Profiles}) ->
    #log_characteristics{
        variability = Variability,
        concurrency = Concurrency,
        loops = Loops,
        noise = Noise,
        trace_count = TraceCount
    } = Characteristics,

    %% Score each algorithm based on characteristics
    Scores = maps:fold(fun(Name, Profile, Acc) ->
        Score = score_algorithm(Profile, Characteristics),
        Acc#{Name => Score}
    end, #{}, Profiles),

    %% Sort by score
    SortedScores = lists:sort(fun({_, A}, {_, B}) -> A > B end, maps:to_list(Scores)),

    case SortedScores of
        [] ->
            #{algorithm => alpha, confidence => 0.5, reasoning => [<<"No profiles available">>], alternatives => []};
        [{BestAlgo, BestScore} | Rest] ->
            Confidence = normalize_confidence(BestScore, SortedScores),
            Reasoning = generate_reasoning(BestAlgo, maps:get(BestAlgo, Profiles), Characteristics),
            Alternatives = [{Algo, normalize_confidence(Score, SortedScores)} || {Algo, Score} <- Rest],
            #{
                algorithm => BestAlgo,
                confidence => Confidence,
                reasoning => Reasoning,
                alternatives => Alternatives
            }
    end.

%% @private
score_algorithm(Profile, Characteristics) ->
    #algorithm_profile{
        strengths = Strengths,
        weaknesses = Weaknesses,
        noise_tolerance = NoiseTolerance
    } = Profile,

    #log_characteristics{
        concurrency = Concurrency,
        loops = Loops,
        noise = Noise,
        trace_count = TraceCount,
        variability = Variability
    } = Characteristics,

    %% Base score
    BaseScore = 1.0,

    %% Strength bonuses
    StrengthBonus = lists:foldl(fun(Strength, Acc) ->
        case Strength of
            parallel when Concurrency > 0.3 -> Acc + 0.2;
            loops when Loops > 0.2 -> Acc + 0.2;
            noise when Noise > 0.2 -> Acc + 0.2;
            scalable when TraceCount > 1000 -> Acc + 0.2;
            simple when Variability < 0.3 -> Acc + 0.2;
            structured when Variability < 0.5 -> Acc + 0.1;
            _ -> Acc
        end
    end, 0.0, Strengths),

    %% Weakness penalties
    WeaknessPenalty = lists:foldl(fun(Weakness, Acc) ->
        case Weakness of
            parallel when Concurrency > 0.3 -> Acc + 0.3;
            loops when Loops > 0.2 -> Acc + 0.3;
            noise when Noise > NoiseTolerance -> Acc + 0.3;
            scalable when TraceCount > 1000 -> Acc + 0.2;
            _ -> Acc
        end
    end, 0.0, Weaknesses),

    max(0.0, BaseScore + StrengthBonus - WeaknessPenalty).

%% @private
normalize_confidence(Score, AllScores) ->
    %% Normalize score to 0-1 range relative to other scores
    Scores = [S || {_, S} <- AllScores],
    case Scores of
        [] -> 0.5;
        _ ->
            MaxScore = lists:max(Scores),
            MinScore = lists:min(Scores),
            case MaxScore - MinScore of
                0.0 -> 0.5;
                Range -> (Score - MinScore) / Range
            end
    end.

%% @private
generate_reasoning(AlgoName, Profile, Characteristics) ->
    #log_characteristics{
        variability = Variability,
        concurrency = Concurrency,
        loops = Loops,
        noise = Noise
    } = Characteristics,

    Reasoning = [
        case {Concurrency > 0.3, lists:member(parallel, Profile#algorithm_profile.strengths)} of
            {true, true} -> <<"Log shows high concurrency, algorithm handles parallelism well">>;
            {true, false} -> <<"Log shows high concurrency, algorithm may struggle with parallel paths">>;
            {false, _} -> <<>>
        end,
        case {Loops > 0.2, lists:member(loops, Profile#algorithm_profile.strengths)} of
            {true, true} -> <<"Log contains loops, algorithm supports cyclic behavior">>;
            {true, false} -> <<"Log contains loops, algorithm may not handle cycles well">>;
            {false, _} -> <<>>
        end,
        case {Noise > 0.2, Noise > Profile#algorithm_profile.noise_tolerance} of
            {true, true} -> <<"Log has high noise, algorithm has good noise tolerance">>;
            {true, false} -> <<"Log has high noise, consider preprocessing">>;
            {false, _} -> <<>>
        end,
        case {Variability < 0.3, lists:member(simple, Profile#algorithm_profile.strengths)} of
            {true, true} -> <<"Log has low variability, simple algorithm is efficient">>;
            {true, false} -> <<"Log has low variability, consider simpler algorithm">>;
            {false, _} -> <<>>
        end
    ],

    [R || R <- Reasoning, R =/= <<>>].

%% @private
update_learning(LearningData, Characteristics, Recommendation) ->
    #{
        algorithm := Algo,
        confidence := Confidence
    } = Recommendation,

    %% Update learning statistics
    Key = {Algo, characterize_key(Characteristics)},
    CurrentCount = maps:get(Key, LearningData, 0),
    LearningData#{Key => CurrentCount + 1}.

%% @private
characterize_key(Char) ->
    %% Create a simplified key for learning
    #log_characteristics{
        variability = V,
        concurrency = C,
        loops = L,
        noise = N
    } = Char,
    {
        categorize_1(V),
        categorize_1(C),
        categorize_1(L),
        categorize_1(N)
    }.

%% @private
categorize_1(Value) when Value < 0.2 -> low;
categorize_1(Value) when Value < 0.6 -> medium;
categorize_1(_Value) -> high.
categorize(Value) when Value < 0.6 -> medium;
categorize(_) -> high.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

simple_log() ->
    #{traces => [[a, b, c], [a, b, c]]}.

parallel_log() ->
    #{traces => [[a, b, c, d], [a, c, b, d]]}.

noisy_log() ->
    #{traces => [[a, b, c, d], [a, x, b, c, d], [a, b, y, c, d], [a, b, z]]}.

loop_log() ->
    #{traces => [[a, b, a, b, c], [a, b, a, b, a, c]]}.

%%--------------------------------------------------------------------
%% Characterization tests
%%--------------------------------------------------------------------

extract_characteristics_simple_test() ->
    Log = simple_log(),
    Char = extract_characteristics(Log),
    ?assertEqual(2, Char#log_characteristics.trace_count),
    ?assertEqual(6, Char#log_characteristics.event_count),
    ?assertEqual(3, Char#log_characteristics.unique_activities),
    ?assert(Char#log_characteristics.variability < 0.6).

extract_characteristics_parallel_test() ->
    Log = parallel_log(),
    Char = extract_characteristics(Log),
    ?assertEqual(4, Char#log_characteristics.unique_activities),
    ?assert(Char#log_characteristics.concurrency >= 0.0).

extract_characteristics_noisy_test() ->
    Log = noisy_log(),
    Char = extract_characteristics(Log),
    ?assert(Char#log_characteristics.noise >= 0.0).

extract_characteristics_loop_test() ->
    Log = loop_log(),
    Char = extract_characteristics(Log),
    ?assert(Char#log_characteristics.loops > 0.0).

%%--------------------------------------------------------------------
%% Recommendation tests
%%--------------------------------------------------------------------

recommend_algorithm_simple_test() ->
    Log = simple_log(),
    State = #state{
        algorithm_profiles = init_default_profiles(),
        transition_system = init_transition_system(),
        recommendation_cache = #{},
        learning_data = #{},
        cache_size = 100
    },
    Result = generate_recommendation(extract_characteristics(Log), State),
    ?assert(maps:is_key(algorithm, Result)),
    ?assert(maps:is_key(confidence, Result)),
    ?assert(maps:is_key(reasoning, Result)),
    ?assert(is_list(maps:get(reasoning, Result))).

score_algorithm_test() ->
    Profile = #algorithm_profile{
        name = test,
        strengths = [parallel, loops],
        weaknesses = [noise],
        complexity = medium,
        scalability = medium,
        noise_tolerance = 0.2
    },
    Char = #log_characteristics{
        trace_count = 100,
        event_count = 500,
        unique_activities = 10,
        variability = 0.5,
        concurrency = 0.4,
        loops = 0.3,
        noise = 0.1
    },
    Score = score_algorithm(Profile, Char),
    ?assert(Score >= 0.0),
    ?assert(Score =< 2.0).

normalize_confidence_test() ->
    Scores = [{a, 1.5}, {b, 1.0}, {c, 0.5}],
    ?assert(normalize_confidence(1.5, Scores) > normalize_confidence(1.0, Scores)),
    ?assert(normalize_confidence(1.0, Scores) > normalize_confidence(0.5, Scores)).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

estimate_loops_test() ->
    ?assert(estimate_loops([[a, b, c]]) < 0.1),
    ?assert(estimate_loops([[a, b, a, b, c]]) > 0.1).

count_repeats_test() ->
    ?assertEqual(0.0, count_repeats([a, b, c])),
    ?assert(count_repeats([a, b, a]) > 0.0).

categorize_1_test() ->
    ?assertEqual(low, categorize_1(0.1)),
    ?assertEqual(medium, categorize_1(0.4)),
    ?assertEqual(high, categorize_1(0.8)).

-endif.
