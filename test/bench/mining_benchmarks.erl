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
%% @doc Mining Algorithm Benchmarks
%%
%% Comprehensive benchmark suite for process mining algorithms.
%% Measures performance of Alpha, Heuristic, Inductive, and other algorithms.
%%
%% <h3>Algorithms Benchmarked</h3>
%%
%% <ul>
%%   <li><b>Alpha Algorithm:</b> Basic process discovery (van der Aalst 2001)</li>
%%   <li><b>Heuristic Miner:</b> Noise-tolerant discovery</li>
%%   <li><b>Inductive Miner:</b> Process discovery with guarantees</li>
%%   <li><b>Frequency-based:</b> Statistical dependency extraction</li>
%%   <li><b>Conformance:</b> Fitness and precision metrics</li>
%% </ul>
%%
%% <h3>Test Data</h3>
%%
%% Benchmarks use synthetic event logs of varying sizes:
%% <ul>
%%   <li><b>Small:</b> 100 cases, 5 activities</li>
%%   <li><b>Medium:</b> 1000 cases, 10 activities</li>
%%   <li><b>Large:</b> 10000 cases, 20 activities</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(mining_benchmarks).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    run_all_benchmarks/0,
    run_algorithm_benchmark/1,
    run_size_benchmark/2,
    compare_to_baseline/0,
    export_results/1,
    get_results/0,
    generate_synthetic_log/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Constants
%%%===================================================================

-define(WARMUP_ITERATIONS, 3).
-define(BENCHMARK_ITERATIONS, 10).
-define(BASELINE_FILE, "test/bench/mining_baseline.json").

%%%===================================================================
%%% Algorithm Definitions
%%%===================================================================

-define(ALGORITHMS, [
    {alpha, fun alpha_algorithm:mine_workflow_net/1},
    {heuristic, fun process_discovery:heuristic_miner/1},
    {frequency, fun process_discovery:frequency_based/1},
    {discover, fun process_discovery:discover/1},
    {discover_noise, fun(Log) -> process_discovery:discover_with_noise(Log, #{}) end}
]).

%%%===================================================================
%%% Log Size Definitions
%%%===================================================================

-define(LOG_SIZES, [
    {small, {100, 5}},      %% 100 cases, 5 activities
    {medium, {1000, 10}},   %% 1000 cases, 10 activities
    {large, {10000, 20}}    %% 10000 cases, 20 activities
]).

%%%===================================================================
%%% State Record
%%%===================================================================

-record(state, {
    results = #{} :: map(),
    baseline = undefined :: undefined | map()
}).

-record(mining_result, {
    algorithm :: atom(),
    log_size :: atom(),
    num_cases :: pos_integer(),
    num_activities :: pos_integer(),
    iterations :: pos_integer(),
    total_time_us :: integer(),
    avg_time_us :: float(),
    min_time_us :: integer(),
    max_time_us :: integer(),
    memory_bytes :: integer(),
    throughput_cases_per_sec :: float()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the benchmark server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run all algorithm benchmarks.
-spec run_all_benchmarks() -> {ok, map()}.
run_all_benchmarks() ->
    gen_server:call(?MODULE, run_all_benchmarks, 600000).

%% @doc Run benchmark for a single algorithm.
-spec run_algorithm_benchmark(atom()) -> {ok, map()}.
run_algorithm_benchmark(Algorithm) ->
    gen_server:call(?MODULE, {run_algorithm, Algorithm}, 300000).

%% @doc Run benchmark for specific log size.
-spec run_size_benchmark(atom(), atom()) -> {ok, #mining_result{}}.
run_size_benchmark(Algorithm, Size) ->
    gen_server:call(?MODULE, {run_size_benchmark, Algorithm, Size}, 300000).

%% @doc Compare current results to baseline.
-spec compare_to_baseline() -> {ok, map()}.
compare_to_baseline() ->
    gen_server:call(?MODULE, compare_baseline).

%% @doc Export results to file.
-spec export_results(file:filename()) -> ok.
export_results(File) ->
    gen_server:call(?MODULE, {export, File}).

%% @doc Get current benchmark results.
-spec get_results() -> map().
get_results() ->
    gen_server:call(?MODULE, get_results).

%% @doc Generate a synthetic event log for testing.
-spec generate_synthetic_log(pos_integer(), pos_integer()) -> process_discovery:event_log().
generate_synthetic_log(NumCases, NumActivities) ->
    Activities = [list_to_atom("a" ++ integer_to_list(N)) || N <- lists:seq(1, NumActivities)],
    generate_traces(NumCases, Activities).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    {ok, #state{baseline = load_baseline()}}.

handle_call(run_all_benchmarks, _From, State) ->
    Results = run_all_mining_benchmarks(),
    {reply, {ok, Results}, State#state{results = Results}};

handle_call({run_algorithm, Algorithm}, _From, State) ->
    Results = benchmark_algorithm_all_sizes(Algorithm),
    {reply, {ok, Results}, State};

handle_call({run_size_benchmark, Algorithm, Size}, _From, State) ->
    Result = benchmark_algorithm_size(Algorithm, Size),
    {reply, {ok, Result}, State};

handle_call(compare_baseline, _From, State = #state{results = Results, baseline = Baseline}) ->
    Comparison = compare_results_to_baseline(Results, Baseline),
    {reply, {ok, Comparison}, State};

handle_call({export, File}, _From, State = #state{results = Results}) ->
    ok = export_to_file(File, Results),
    {reply, ok, State};

handle_call(get_results, _From, State = #state{results = Results}) ->
    {reply, Results, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Benchmark Functions
%%%===================================================================

%% @doc Run all mining benchmarks across algorithms and sizes.
-spec run_all_mining_benchmarks() -> map().
run_all_mining_benchmarks() ->
    lists:foldl(fun({Algorithm, _Fun}, Acc) ->
        SizeResults = benchmark_algorithm_all_sizes(Algorithm),
        maps:put(Algorithm, SizeResults, Acc)
    end, #{}, ?ALGORITHMS).

%% @doc Benchmark an algorithm across all log sizes.
-spec benchmark_algorithm_all_sizes(atom()) -> map().
benchmark_algorithm_all_sizes(Algorithm) ->
    lists:foldl(fun({Size, _}, Acc) ->
        Result = benchmark_algorithm_size(Algorithm, Size),
        maps:put(Size, Result, Acc)
    end, #{}, ?LOG_SIZES).

%% @doc Benchmark algorithm with specific log size.
-spec benchmark_algorithm_size(atom(), atom()) -> #mining_result{}.
benchmark_algorithm_size(Algorithm, Size) ->
    {NumCases, NumActivities} = proplists:get_value(Size, ?LOG_SIZES),

    %% Generate synthetic log
    Log = generate_synthetic_log(NumCases, NumActivities),

    %% Get algorithm function
    Fun = proplists:get_value(Algorithm, ?ALGORITHMS),

    %% Warmup
    lists:foreach(fun(_) -> catch Fun(Log) end, lists:seq(1, ?WARMUP_ITERATIONS)),

    %% Measure memory before
    garbage_collect(),
    MemBefore = erlang:memory(total),

    %% Benchmark iterations
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {TimeUs, _Result} = timer:tc(fun() -> Fun(Log) end),
        TimeUs
    end, lists:seq(1, Iterations)),

    %% Measure memory after
    MemAfter = erlang:memory(total),
    MemDelta = MemAfter - MemBefore,

    %% Calculate statistics
    TotalTime = lists:sum(Times),
    AvgTime = TotalTime / Iterations,
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),
    Throughput = case AvgTime of
        0.0 -> 0.0;
        _ -> (NumCases * 1000000.0) / AvgTime
    end,

    #mining_result{
        algorithm = Algorithm,
        log_size = Size,
        num_cases = NumCases,
        num_activities = NumActivities,
        iterations = Iterations,
        total_time_us = TotalTime,
        avg_time_us = AvgTime,
        min_time_us = MinTime,
        max_time_us = MaxTime,
        memory_bytes = MemDelta,
        throughput_cases_per_sec = Throughput
    }.

%%%===================================================================
%%% Synthetic Log Generation
%%%===================================================================

%% @doc Generate synthetic event traces.
-spec generate_traces(pos_integer(), [atom()]) -> process_discovery:event_log().
generate_traces(NumCases, Activities) ->
    lists:map(fun(CaseId) ->
        generate_trace(CaseId, Activities)
    end, lists:seq(1, NumCases)).

%% @doc Generate a single trace with some variation.
-spec generate_trace(pos_integer(), [atom()]) -> process_discovery:trace().
generate_trace(CaseId, Activities) ->
    %% Add some randomness to trace structure
    Seed = CaseId rem 10,
    Trace = case Seed of
        0 -> Activities;  %% Full sequence
        1 -> lists:sublist(Activities, max(1, length(Activities) - 1));
        2 -> lists:reverse(Activities);  %% Reverse
        3 -> Activities ++ [hd(Activities)];  %% Loop
        _ -> Activities
    end,

    %% Convert to event format with timestamps
    lists:zipwith(fun(I, Activity) ->
        Timestamp = I * 1000,
        {CaseId, Activity, Timestamp}
    end, lists:seq(1, length(Trace)), Trace).

%%%===================================================================
%%% Baseline Functions
%%%===================================================================

%% @doc Load baseline from file.
-spec load_baseline() -> undefined | map().
load_baseline() ->
    case file:read_file(?BASELINE_FILE) of
        {ok, Bin} ->
            try jsone:decode(Bin) of
                Map -> Map
            catch
                _:_ -> undefined
            end;
        {error, _} ->
            undefined
    end.

%% @doc Compare results to baseline.
-spec compare_results_to_baseline(map(), undefined | map()) -> map().
compare_results_to_baseline(_Results, undefined) ->
    #{status => no_baseline};
compare_results_to_baseline(Results, Baseline) ->
    Comparisons = maps:fold(fun(Algorithm, SizeResults, AccAlgo) ->
        maps:fold(fun(Size, Result, AccSize) ->
            Key = <<(atom_to_binary(Algorithm))/binary, "_",
                    (atom_to_binary(Size))/binary>>,
            case maps:get(Key, Baseline, undefined) of
                undefined -> AccSize;
                BaselineData -> maps:put(Key, compare_single(Result, BaselineData), AccSize)
            end
        end, AccAlgo, SizeResults)
    end, #{}, Results),

    #{
        status => compared,
        comparisons => Comparisons
    }.

%% @doc Compare single result to baseline.
-spec compare_single(#mining_result{}, map()) -> map().
compare_single(Result, BaselineData) ->
    Current = Result#mining_result.avg_time_us,
    Baseline = maps:get(<<"avg_time_us">>, BaselineData, 0),

    DiffPercent = case Baseline of
        0 -> 0.0;
        _ -> ((Current - Baseline) / Baseline) * 100
    end,

    Status = case DiffPercent of
        P when P > 20 -> regression;
        P when P > 10 -> warning;
        P when P < -10 -> improvement;
        _ -> ok
    end,

    #{
        status => Status,
        current_us => Current,
        baseline_us => Baseline,
        diff_percent => DiffPercent
    }.

%%%===================================================================
%%% Export Functions
%%%===================================================================

%% @doc Export results to JSON file.
-spec export_to_file(file:filename(), map()) -> ok.
export_to_file(File, Results) ->
    JSON = maps:map(fun(Algorithm, SizeResults) ->
        maps:map(fun(Size, Result) ->
            #mining_result{
                algorithm = Algorithm,
                log_size = Size,
                num_cases = Cases,
                num_activities = Activities,
                iterations = Iterations,
                total_time_us = Total,
                avg_time_us = Avg,
                min_time_us = Min,
                max_time_us = Max,
                memory_bytes = Memory,
                throughput_cases_per_sec = Throughput
            } = Result,

            #{
                <<"algorithm">> => atom_to_binary(Algorithm),
                <<"log_size">> => atom_to_binary(Size),
                <<"num_cases">> => Cases,
                <<"num_activities">> => Activities,
                <<"iterations">> => Iterations,
                <<"total_time_us">> => Total,
                <<"avg_time_us">> => Avg,
                <<"min_time_us">> => Min,
                <<"max_time_us">> => Max,
                <<"memory_bytes">> => Memory,
                <<"throughput_cases_per_sec">> => Throughput,
                <<"timestamp">> => integer_to_binary(erlang:system_time(second))
            }
        end, SizeResults)
    end, Results),

    Encoded = jsone:encode(JSON, [pretty]),
    ok = file:write_file(File, Encoded).

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

-ifdef(TEST).

generate_synthetic_log_test() ->
    Log = generate_synthetic_log(10, 3),
    ?assertEqual(10, length(Log)),
    ?assert(is_list(hd(Log))).

benchmark_algorithm_size_test() ->
    %% Quick test with small log
    Result = benchmark_algorithm_size(alpha, small),
    ?assertEqual(alpha, Result#mining_result.algorithm),
    ?assertEqual(small, Result#mining_result.log_size),
    ?assert(Result#mining_result.num_cases > 0).

-endif.
