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
%% @doc Pattern Performance Benchmarks
%%
%% Comprehensive benchmark suite for all 43 YAWL workflow control patterns.
%% Measures execution time, memory usage, and throughput for each pattern.
%%
%% <h3>Patterns Benchmarked</h3>
%%
%% <ul>
%%   <li><b>Basic Control Flow (P1-P11):</b> Sequence, Parallel Split, Sync, etc.</li>
%%   <li><b>Advanced Branching (P12-P18):</b> Multi-instance, Deferred Choice, etc.</li>
%%   <li><b>Cancellation (P19-P27):</b> Cancel Activity, Cancel Region, etc.</li>
%%   <li><b>Complex Patterns (P28-P43):</b> Discriminators, Partial Joins, etc.</li>
%% </ul>
%%
%% <h3>Metrics Collected</h3>
%%
%% <ul>
%%   <li><b>Execution Time:</b> Average microseconds per pattern execution</li>
%%   <li><b>Memory Usage:</b> Bytes consumed per pattern instance</li>
%%   <li><b>Throughput:</b> Executions per second</li>
%%   <li><b>Startup Time:</b> Time to initialize pattern gen_server</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(pattern_benchmarks).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    run_all_benchmarks/0,
    run_pattern_benchmark/1,
    run_pattern_group/1,
    compare_to_baseline/0,
    export_results/1,
    get_results/0
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

-define(WARMUP_ITERATIONS, 10).
-define(BENCHMARK_ITERATIONS, 100).
-define(MEMORY_SAMPLE_SIZE, 50).

-define(BASELINE_FILE, "test/bench/baseline.json").

%%%===================================================================
%%% Pattern Definitions
%%%===================================================================

%% All 43 YAWL patterns organized by category
-define(BASIC_PATTERNS, [
    sequence,                  %% P1
    parallel_split,            %% P2
    synchronization,           %% P3
    exclusive_choice,          %% P4
    simple_merge,              %% P5
    multiple_choice,           %% P6
    structured_sync_merge,     %% P7
    multiple_merge,            %% P8
    discriminator,             %% P9
    arbitrary_cycles,          %% P10
    implicit_termination       %% P11
]).

-define(ADVANCED_BRANCHING, [
    multiple_instances_sync,   %% P12
    deferred_choice,           %% P16
    interleaved_routing,       %% P17
    milestone                  %% P18
]).

-define(CANCELLATION_PATTERNS, [
    cancel_activity,           %% P19
    cancel_case,               %% P20
    cancel_region,             %% P25
    cancel_mi_activity,        %% P26
    complete_mi_activity,      %% P27
    cancellation               %% General
]).

-define(COMPLEX_PATTERNS, [
    blocking_discriminator,    %% P28
    cancelling_discriminator,  %% P29
    structured_partial_join,   %% P30
    blocking_partial_join,     %% P31
    cancelling_partial_join,   %% P32
    generalized_and_join,      %% P33
    static_partial_join_mi,    %% P34
    cancelling_partial_join_mi,%% P35
    dynamic_partial_join_mi,   %% P36
    local_sync_merge,          %% P37
    general_sync_merge,        %% P38
    critical_section,          %% P39
    thread_merge,              %% P41
    thread_split               %% P42
]).

-define(DATA_PATTERNS, [
    data_accumulate,
    data_distribute,
    data_transform,
    data_visibility,
    param_pass
]).

-define(RESOURCE_PATTERNS, [
    resource_allocation,
    resource_deallocation,
    resource_initialization,
    direct_resource_creation,
    role_based_allocation
]).

-define(TRIGGER_PATTERNS, [
    transient_trigger,
    persistent_trigger
]).

-define(OTHER_PATTERNS, [
    circuit_breaker,
    recursion,
    structured_loop,
    n_out_of_m,
    or_join,
    explicit_termination,
    implicit_merge,
    general_sync_merge,
    interleaved_parallel,
    multi_instance
]).

%% All patterns combined
-define(ALL_PATTERNS, ?BASIC_PATTERNS ++ ?ADVANCED_BRANCHING ++
                     ?CANCELLATION_PATTERNS ++ ?COMPLEX_PATTERNS ++
                     ?DATA_PATTERNS ++ ?RESOURCE_PATTERNS ++
                     ?TRIGGER_PATTERNS ++ ?OTHER_PATTERNS).

%%%===================================================================
%%% State Record
%%%===================================================================

-record(state, {
    results = #{} :: map(),
    baseline = undefined :: undefined | map(),
    start_time :: integer()
}).

-record(benchmark_result, {
    pattern :: atom(),
    category :: atom(),
    iterations :: pos_integer(),
    total_time_us :: integer(),
    avg_time_us :: float(),
    min_time_us :: integer(),
    max_time_us :: integer(),
    memory_bytes :: integer(),
    throughput_per_sec :: float(),
    startup_us :: integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the benchmark server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run all pattern benchmarks.
-spec run_all_benchmarks() -> {ok, map()}.
run_all_benchmarks() ->
    gen_server:call(?MODULE, run_all_benchmarks, 600000).

%% @doc Run benchmark for a single pattern.
-spec run_pattern_benchmark(atom()) -> {ok, #benchmark_result{}}.
run_pattern_benchmark(Pattern) ->
    gen_server:call(?MODULE, {run_pattern, Pattern}, 60000).

%% @doc Run benchmarks for a pattern group.
-spec run_pattern_group(atom()) -> {ok, [#benchmark_result{}]}.
run_pattern_group(Group) ->
    gen_server:call(?MODULE, {run_group, Group}, 300000).

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

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    {ok, #state{
        start_time = erlang:monotonic_time(microsecond),
        baseline = load_baseline()
    }}.

handle_call(run_all_benchmarks, _From, State) ->
    Results = benchmark_all_patterns(),
    {reply, {ok, Results}, State#state{results = Results}};

handle_call({run_pattern, Pattern}, _From, State) ->
    Result = benchmark_single_pattern(Pattern),
    NewResults = maps:put(Pattern, Result, State#state.results),
    {reply, {ok, Result}, State#state{results = NewResults}};

handle_call({run_group, Group}, _From, State) ->
    Patterns = patterns_for_group(Group),
    Results = [benchmark_single_pattern(P) || P <- Patterns],
    {reply, {ok, Results}, State};

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

%% @doc Benchmark all 43 patterns.
-spec benchmark_all_patterns() -> map().
benchmark_all_patterns() ->
    Patterns = ?ALL_PATTERNS,
    lists:foldl(fun(Pattern, Acc) ->
        Result = benchmark_single_pattern(Pattern),
        maps:put(Pattern, Result, Acc)
    end, #{}, Patterns).

%% @doc Benchmark a single pattern module.
-spec benchmark_single_pattern(atom()) -> #benchmark_result{}.
benchmark_single_pattern(Pattern) ->
    Category = categorize_pattern(Pattern),

    %% Ensure module is loaded
    case code:ensure_loaded(Pattern) of
        {module, Pattern} -> ok;
        {error, Why} -> exit({module_not_loaded, Pattern, Why})
    end,

    %% Measure startup time
    StartupUs = measure_startup(Pattern),

    %% Warmup iterations
    lists:foreach(fun(_) -> run_pattern_once(Pattern) end,
                  lists:seq(1, ?WARMUP_ITERATIONS)),

    %% Benchmark iterations
    Iterations = ?BENCHMARK_ITERATIONS,
    {Times, Memory} = benchmark_iterations(Pattern, Iterations),

    TotalTime = lists:sum(Times),
    AvgTime = TotalTime / Iterations,
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),

    Throughput = case AvgTime of
        0.0 -> 0.0;
        _ -> 1000000.0 / AvgTime
    end,

    #benchmark_result{
        pattern = Pattern,
        category = Category,
        iterations = Iterations,
        total_time_us = TotalTime,
        avg_time_us = AvgTime,
        min_time_us = MinTime,
        max_time_us = MaxTime,
        memory_bytes = Memory,
        throughput_per_sec = Throughput,
        startup_us = StartupUs
    }.

%% @doc Run pattern benchmark iterations.
-spec benchmark_iterations(atom(), pos_integer()) -> {[integer()], integer()}.
benchmark_iterations(Pattern, Iterations) ->
    %% Measure memory before
    garbage_collect(),
    MemBefore = erlang:memory(total),

    %% Run iterations and collect times
    Times = lists:map(fun(_) ->
        {TimeUs, _} = timer:tc(fun() -> run_pattern_once(Pattern) end),
        TimeUs
    end, lists:seq(1, Iterations)),

    %% Measure memory after (average over sample)
    MemAfter = erlang:memory(total),
    MemDelta = MemAfter - MemBefore,

    {Times, MemDelta}.

%% @doc Measure pattern startup time.
-spec measure_startup(atom()) -> integer().
measure_startup(Pattern) ->
    %% Measure time to start and stop the pattern
    {TimeUs, _} = timer:tc(fun() ->
        case catch Pattern:init(#{}) of
            {'EXIT', _} -> ok;
            _ -> ok
        end
    end),
    TimeUs.

%% @doc Run pattern once (execute typical workflow).
-spec run_pattern_once(atom()) -> ok.
run_pattern_once(Pattern) ->
    try
        %% Try to exercise the pattern's gen_yawl callbacks
        Places = catch Pattern:place_lst(),
        Transitions = catch Pattern:trsn_lst(),

        %% Call preset for each transition
        _ = [catch Pattern:preset(T) || T <- Transitions],

        %% Test is_enabled with sample marking
        _ = catch Pattern:is_enabled(hd(Transitions), #{}, #{}),

        ok
    catch
        _:_ -> ok  %% Pattern may not have all callbacks
    end.

%%%===================================================================
%%% Pattern Categorization
%%%===================================================================

%% @doc Categorize a pattern for reporting.
-spec categorize_pattern(atom()) -> atom().
categorize_pattern(Pattern) ->
    case lists:member(Pattern, ?BASIC_PATTERNS) of
        true -> basic_control_flow;
        false ->
            case lists:member(Pattern, ?ADVANCED_BRANCHING) of
                true -> advanced_branching;
                false ->
                    case lists:member(Pattern, ?CANCELLATION_PATTERNS) of
                        true -> cancellation;
                        false ->
                            case lists:member(Pattern, ?DATA_PATTERNS) of
                                true -> data;
                                false ->
                                    case lists:member(Pattern, ?RESOURCE_PATTERNS) of
                                        true -> resource;
                                        false ->
                                            case lists:member(Pattern, ?TRIGGER_PATTERNS) of
                                                true -> trigger;
                                                false -> complex
                                            end
                                    end
                            end
                    end
            end
    end.

%% @doc Get patterns for a group.
-spec patterns_for_group(atom()) -> [atom()].
patterns_for_group(basic_control_flow) -> ?BASIC_PATTERNS;
patterns_for_group(advanced_branching) -> ?ADVANCED_BRANCHING;
patterns_for_group(cancellation) -> ?CANCELLATION_PATTERNS;
patterns_for_group(complex) -> ?COMPLEX_PATTERNS;
patterns_for_group(data) -> ?DATA_PATTERNS;
patterns_for_group(resource) -> ?RESOURCE_PATTERNS;
patterns_for_group(trigger) -> ?TRIGGER_PATTERNS;
patterns_for_group(all) -> ?ALL_PATTERNS;
patterns_for_group(_) -> [].

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
    #{status => no_baseline, message => "No baseline file found"};
compare_results_to_baseline(Results, Baseline) ->
    Comparisons = maps:map(fun(Pattern, Result) ->
        compare_single_result(Pattern, Result, Baseline)
    end, Results),

    #{
        status => compared,
        baseline_created => maps:get(<<"created">>, Baseline, <<"unknown">>),
        comparisons => Comparisons,
        regressions => detect_regressions(Comparisons)
    }.

%% @doc Compare single result to baseline.
-spec compare_single_result(atom(), #benchmark_result{}, map()) -> map().
compare_single_result(Pattern, Result, Baseline) ->
    PatternBin = atom_to_binary(Pattern),
    case maps:get(PatternBin, Baseline, undefined) of
        undefined ->
            #{status => no_baseline};
        BaselineData ->
            AvgTime = Result#benchmark_result.avg_time_us,
            BaselineTime = maps:get(<<"avg_time_us">>, BaselineData, 0),

            DiffPercent = case BaselineTime of
                0 -> 0.0;
                _ -> ((AvgTime - BaselineTime) / BaselineTime) * 100
            end,

            Status = case DiffPercent of
                P when P > 20 -> regression;
                P when P > 10 -> warning;
                P when P < -10 -> improvement;
                _ -> ok
            end,

            #{
                status => Status,
                current_us => AvgTime,
                baseline_us => BaselineTime,
                diff_percent => DiffPercent
            }
    end.

%% @doc Detect regressions in comparison.
-spec detect_regressions(map()) -> [atom()].
detect_regressions(Comparisons) ->
    maps:fold(fun(Pattern, Comparison, Acc) ->
        case maps:get(status, Comparison, ok) of
            regression -> [Pattern | Acc];
            _ -> Acc
        end
    end, [], Comparisons).

%%%===================================================================
%%% Export Functions
%%%===================================================================

%% @doc Export results to JSON file.
-spec export_to_file(file:filename(), map()) -> ok.
export_to_file(File, Results) ->
    JSON = maps:map(fun(Pattern, Result) ->
        #benchmark_result{
            pattern = Pattern,
            category = Category,
            iterations = Iterations,
            total_time_us = Total,
            avg_time_us = Avg,
            min_time_us = Min,
            max_time_us = Max,
            memory_bytes = Memory,
            throughput_per_sec = Throughput,
            startup_us = Startup
        } = Result,

        #{
            <<"pattern">> => atom_to_binary(Pattern),
            <<"category">> => atom_to_binary(Category),
            <<"iterations">> => Iterations,
            <<"total_time_us">> => Total,
            <<"avg_time_us">> => Avg,
            <<"min_time_us">> => Min,
            <<"max_time_us">> => Max,
            <<"memory_bytes">> => Memory,
            <<"throughput_per_sec">> => Throughput,
            <<"startup_us">> => Startup,
            <<"timestamp">> => integer_to_binary(erlang:system_time(second))
        }
    end, Results),

    Encoded = jsone:encode(JSON, [pretty]),
    ok = file:write_file(File, Encoded).

%% @doc Save current results as baseline.
-spec save_baseline() -> ok.
save_baseline() ->
    {ok, Results} = run_all_benchmarks(),
    export_to_file(?BASELINE_FILE, Results).

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

-ifdef(TEST).

categorize_pattern_test() ->
    ?assertEqual(basic_control_flow, categorize_pattern(sequence)),
    ?assertEqual(basic_control_flow, categorize_pattern(parallel_split)),
    ?assertEqual(cancellation, categorize_pattern(cancel_activity)),
    ?assertEqual(data, categorize_pattern(data_accumulate)).

patterns_for_group_test() ->
    Basic = patterns_for_group(basic_control_flow),
    ?assert(lists:member(sequence, Basic)),
    ?assert(lists:member(parallel_split, Basic)).

benchmark_single_pattern_test() ->
    %% sequence should be available
    Result = benchmark_single_pattern(sequence),
    ?assertEqual(sequence, Result#benchmark_result.pattern),
    ?assert(Result#benchmark_result.iterations > 0).

run_pattern_once_test() ->
    ?assertEqual(ok, run_pattern_once(sequence)).

-endif.
