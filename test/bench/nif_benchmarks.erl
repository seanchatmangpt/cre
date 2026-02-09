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
%% @doc NIF Performance Benchmarks
%%
%% Benchmark suite comparing Rust NIF performance vs pure Erlang.
%% Measures overhead, throughput, and memory usage for native operations.
%%
%% <h3>Operations Benchmarked</h3>
%%
%% <ul>
%%   <li><b>Alpha Algorithm:</b> Process discovery with/without NIF</li>
%%   <li><b>Frequency Calculation:</b> Matrix operations</li>
%%   <li><b>String Operations:</b> Text processing</li>
%%   <li><b>Math Operations:</b> Numerical computations</li>
%% </ul>
%%
%% <h3>Metrics</h3>
%%
%% <ul>
%%   <li><b>Speedup:</b> NIF time / Pure Erlang time</li>
%%   <li><b>Overhead:</b> NIF call overhead</li>
%%   <li><b>Memory:</b> Memory usage comparison</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(nif_benchmarks).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    run_all_benchmarks/0,
    run_operation_benchmark/1,
    compare_nif_vs_pure/1,
    export_results/1,
    get_results/0,
    check_nif_available/0
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

-define(WARMUP_ITERATIONS, 5).
-define(BENCHMARK_ITERATIONS, 100).
-define(BASELINE_FILE, "test/bench/nif_baseline.json").

%%%===================================================================
%%% Benchmark Operations
%%%===================================================================

-define(OPERATIONS, [
    {alpha_discovery, fun run_alpha_benchmark/1},
    {frequency_matrix, fun run_frequency_benchmark/1},
    {dependency_calc, fun run_dependency_benchmark/1},
    {log_conversion, fun run_conversion_benchmark/1}
]).

%%%===================================================================
%%% State Record
%%%===================================================================

-record(state, {
    results = #{} :: map(),
    nif_available = false :: boolean(),
    baseline = undefined :: undefined | map()
}).

-record(nif_result, {
    operation :: atom(),
    data_size :: atom(),
    nif_time_us :: float(),
    pure_time_us :: float(),
    speedup :: float(),
    overhead_us :: float(),
    nif_memory :: integer(),
    pure_memory :: integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the benchmark server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run all NIF benchmarks.
-spec run_all_benchmarks() -> {ok, map()}.
run_all_benchmarks() ->
    gen_server:call(?MODULE, run_all_benchmarks, 600000).

%% @doc Run benchmark for a single operation.
-spec run_operation_benchmark(atom()) -> {ok, map()}.
run_operation_benchmark(Operation) ->
    gen_server:call(?MODULE, {run_operation, Operation}, 300000).

%% @doc Compare NIF vs pure Erlang for an operation.
-spec compare_nif_vs_pure(atom()) -> {ok, #nif_result{}}.
compare_nif_vs_pure(Operation) ->
    gen_server:call(?MODULE, {compare, Operation}, 300000).

%% @doc Export results to file.
-spec export_results(file:filename()) -> ok.
export_results(File) ->
    gen_server:call(?MODULE, {export, File}).

%% @doc Get current benchmark results.
-spec get_results() -> map().
get_results() ->
    gen_server:call(?MODULE, get_results).

%% @doc Check if NIF is available.
-spec check_nif_available() -> boolean().
check_nif_available() ->
    gen_server:call(?MODULE, check_nif).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    NifAvailable = detect_nif(),
    {ok, #state{
        nif_available = NifAvailable,
        baseline = load_baseline()
    }}.

handle_call(run_all_benchmarks, _From, State) ->
    Results = run_all_nif_benchmarks(State#state.nif_available),
    {reply, {ok, Results}, State#state{results = Results}};

handle_call({run_operation, Operation}, _From, State) ->
    Result = benchmark_operation(Operation, State#state.nif_available),
    {reply, {ok, Result}, State};

handle_call({compare, Operation}, _From, State) ->
    Result = compare_nif_pure(Operation, State#state.nif_available),
    {reply, {ok, Result}, State};

handle_call({export, File}, _From, State = #state{results = Results}) ->
    ok = export_to_file(File, Results),
    {reply, ok, State};

handle_call(get_results, _From, State = #state{results = Results}) ->
    {reply, Results, State};

handle_call(check_nif, _From, State = #state{nif_available = Available}) ->
    {reply, Available, State};

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
%%% Detection Functions
%%%===================================================================

%% @doc Detect if Rust NIF is available.
-spec detect_nif() -> boolean().
detect_nif() ->
    %% Check if rust_nif module is loaded and NIF functions are available
    case code:ensure_loaded(rust_nif) of
        {module, rust_nif} ->
            %% Try to call a simple NIF function to verify it works
            try
                case rust_nif:nif_info() of
                    {ok, _} -> true;
                    _ -> false
                end
            catch
                _:_ -> false
            end;
        _ ->
            false
    end.

%%%===================================================================
%%% Benchmark Functions
%%%===================================================================

%% @doc Run all NIF benchmarks.
-spec run_all_nif_benchmarks(boolean()) -> map().
run_all_nif_benchmarks(NifAvailable) ->
    lists:foldl(fun({Operation, _Fun}, Acc) ->
        Result = benchmark_operation(Operation, NifAvailable),
        maps:put(Operation, Result, Acc)
    end, #{}, ?OPERATIONS).

%% @doc Benchmark a single operation.
-spec benchmark_operation(atom(), boolean()) -> map().
benchmark_operation(Operation, NifAvailable) ->
    Sizes = [small, medium, large],
    lists:foldl(fun(Size, Acc) ->
        Result = compare_nif_pure_size(Operation, Size, NifAvailable),
        maps:put(Size, Result, Acc)
    end, #{}, Sizes).

%% @doc Compare NIF vs pure Erlang for a specific operation and size.
-spec compare_nif_pure_size(atom(), atom(), boolean()) -> #nif_result{}.
compare_nif_pure_size(Operation, Size, NifAvailable) ->
    %% Generate test data
    TestData = generate_test_data(Operation, Size),

    %% Benchmark pure Erlang version
    PureTime = benchmark_pure(Operation, TestData),
    PureMemory = measure_pure_memory(Operation, TestData),

    %% Benchmark NIF version (if available)
    {NifTime, NifMemory, Overhead} = case NifAvailable of
        true ->
            NT = benchmark_nif(Operation, TestData),
            NM = measure_nif_memory(Operation, TestData),
            OH = measure_nif_overhead(),
            {NT, NM, OH};
        false ->
            {0.0, 0, 0.0}
    end,

    %% Calculate speedup
    Speedup = case NifTime of
        0.0 -> 0.0;
        _ -> PureTime / NifTime
    end,

    #nif_result{
        operation = Operation,
        data_size = Size,
        nif_time_us = NifTime,
        pure_time_us = PureTime,
        speedup = Speedup,
        overhead_us = Overhead,
        nif_memory = NifMemory,
        pure_memory = PureMemory
    }.

%% @doc Compare NIF vs pure Erlang.
-spec compare_nif_pure(atom(), boolean()) -> #nif_result{}.
compare_nif_pure(Operation, NifAvailable) ->
    compare_nif_pure_size(Operation, medium, NifAvailable).

%% @doc Benchmark pure Erlang version.
-spec benchmark_pure(atom(), term()) -> float().
benchmark_pure(alpha_discovery, Log) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch alpha_algorithm:mine_workflow_net(Log)
        end),
        Time / 1000.0  %% Convert to ms
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations;

benchmark_pure(frequency_matrix, Log) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch process_discovery:calculate_frequency_matrix(Log)
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations;

benchmark_pure(dependency_calc, Log) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch process_discovery:calculate_dependencies(Log)
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations;

benchmark_pure(log_conversion, RawLog) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch process_discovery:events_to_traces(RawLog)
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations.

%% @doc Benchmark NIF version.
-spec benchmark_nif(atom(), term()) -> float().
benchmark_nif(alpha_discovery, Log) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch rust_nif:alpha_mine(Log)
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations;

benchmark_nif(frequency_matrix, Log) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch rust_nif:frequency_matrix(Log)
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations;

benchmark_nif(dependency_calc, Log) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch rust_nif:dependencies(Log)
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations;

benchmark_nif(log_conversion, RawLog) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch rust_nif:convert_log(RawLog)
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    lists:sum(Times) / Iterations.

%% @doc Measure pure Erlang memory usage.
-spec measure_pure_memory(atom(), term()) -> integer().
measure_pure_memory(Operation, TestData) ->
    garbage_collect(),
    MemBefore = erlang:memory(total),

    case Operation of
        alpha_discovery -> alpha_algorithm:mine_workflow_net(TestData);
        frequency_matrix -> process_discovery:calculate_frequency_matrix(TestData);
        dependency_calc -> process_discovery:calculate_dependencies(TestData);
        log_conversion -> process_discovery:events_to_traces(TestData)
    end,

    MemAfter = erlang:memory(total),
    MemAfter - MemBefore.

%% @doc Measure NIF memory usage.
-spec measure_nif_memory(atom(), term()) -> integer().
measure_nif_memory(Operation, TestData) ->
    garbage_collect(),
    MemBefore = erlang:memory(total),

    case Operation of
        alpha_discovery -> rust_nif:alpha_mine(TestData);
        frequency_matrix -> rust_nif:frequency_matrix(TestData);
        dependency_calc -> rust_nif:dependencies(TestData);
        log_conversion -> rust_nif:convert_log(TestData)
    end,

    MemAfter = erlang:memory(total),
    MemAfter - MemBefore.

%% @doc Measure NIF call overhead.
-spec measure_nif_overhead() -> float().
measure_nif_overhead() ->
    %% Measure a trivial NIF call
    Iterations = 1000,
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            catch rust_nif:nif_info()
        end, lists:seq(1, Iterations))
    end),
    TimeUs / Iterations.

%%%===================================================================
%%% Test Data Generation
%%%===================================================================

%% @doc Generate test data for benchmarks.
-spec generate_test_data(atom(), atom()) -> term().
generate_test_data(alpha_discovery, small) ->
    generate_event_log(100, 5);
generate_test_data(alpha_discovery, medium) ->
    generate_event_log(500, 10);
generate_test_data(alpha_discovery, large) ->
    generate_event_log(2000, 20);

generate_test_data(frequency_matrix, Size) ->
    Log = generate_test_data(alpha_discovery, Size),
    process_discovery:events_to_traces(Log);

generate_test_data(dependency_calc, Size) ->
    Log = generate_test_data(alpha_discovery, Size),
    process_discovery:events_to_traces(Log);

generate_test_data(log_conversion, Size) ->
    generate_raw_event_log(100, 5).

%% @doc Generate event log for testing.
-spec generate_event_log(pos_integer(), pos_integer()) -> [[atom()]].
generate_event_log(NumCases, NumActivities) ->
    Activities = [list_to_atom("a" ++ integer_to_list(N)) || N <- lists:seq(1, NumActivities)],
    lists:map(fun(CaseId) ->
        vary_trace(Activities, CaseId rem 5)
    end, lists:seq(1, NumCases)).

%% @doc Generate raw event log (with timestamps).
-spec generate_raw_event_log(pos_integer(), pos_integer()) -> process_discovery:event_log().
generate_raw_event_log(NumCases, NumActivities) ->
    Activities = [list_to_atom("a" ++ integer_to_list(N)) || N <- lists:seq(1, NumActivities)],
    lists:flatmap(fun(CaseId) ->
        Trace = vary_trace(Activities, CaseId rem 5),
        lists:zipwith(fun(I, Activity) ->
            {CaseId, Activity, I * 1000}
        end, lists:seq(1, length(Trace)), Trace)
    end, lists:seq(1, NumCases)).

%% @doc Vary a trace for testing.
-spec vary_trace([atom()], integer()) -> [atom()].
vary_trace(Activities, 0) -> Activities;
vary_trace(Activities, 1) -> lists:sublist(Activities, max(1, length(Activities) - 1));
vary_trace(Activities, 2) -> lists:reverse(Activities);
vary_trace(Activities, 3) -> Activities ++ [hd(Activities)];
vary_trace(_Activities, _) -> [].

%%%===================================================================
%%% Benchmark Wrappers for gen_server
%%%===================================================================

%% @private
run_alpha_benchmark(_Data) ->
    %% Placeholder for alpha benchmark
    ok.

%% @private
run_frequency_benchmark(_Data) ->
    %% Placeholder for frequency benchmark
    ok.

%% @private
run_dependency_benchmark(_Data) ->
    %% Placeholder for dependency benchmark
    ok.

%% @private
run_conversion_benchmark(_Data) ->
    %% Placeholder for conversion benchmark
    ok.

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

%%%===================================================================
%%% Export Functions
%%%===================================================================

%% @doc Export results to JSON file.
-spec export_to_file(file:filename(), map()) -> ok.
export_to_file(File, Results) ->
    JSON = maps:map(fun(Operation, SizeResults) ->
        maps:map(fun(Size, Result) ->
            #nif_result{
                operation = Operation,
                data_size = Size,
                nif_time_us = NifTime,
                pure_time_us = PureTime,
                speedup = Speedup,
                overhead_us = Overhead,
                nif_memory = NifMem,
                pure_memory = PureMem
            } = Result,

            #{
                <<"operation">> => atom_to_binary(Operation),
                <<"data_size">> => atom_to_binary(Size),
                <<"nif_time_us">> => NifTime,
                <<"pure_time_us">> => PureTime,
                <<"speedup">> => Speedup,
                <<"overhead_us">> => Overhead,
                <<"nif_memory">> => NifMem,
                <<"pure_memory">> => PureMem,
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

generate_event_log_test() ->
    Log = generate_event_log(10, 3),
    ?assertEqual(10, length(Log)),
    ?assert(is_list(hd(Log))).

detect_nif_test() ->
    Available = detect_nif(),
    ?assert(is_boolean(Available)).

benchmark_pure_test() ->
    Log = generate_event_log(10, 3),
    Time = benchmark_pure(alpha_discovery, Log),
    ?assert(Time >= 0.0).

-endif.
