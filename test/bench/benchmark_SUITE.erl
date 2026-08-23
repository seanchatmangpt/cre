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
%% @doc Benchmark Common Test Suite
%%
%% Automated CI/CD benchmark execution with regression detection.
%%
%% @end
%% -------------------------------------------------------------------

-module(benchmark_SUITE).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    %% Pattern Benchmarks
    pattern_basic_control_flow/1,
    pattern_advanced_branching/1,
    pattern_cancellation/1,
    pattern_complex/1,

    %% Mining Benchmarks
    mining_alpha_algorithm/1,
    mining_heuristic_miner/1,
    mining_scalability/1,

    %% Compilation Benchmarks
    compilation_spec_sizes/1,
    compilation_real_world/1,

    %% NIF Benchmarks
    nif_availability/1,
    nif_comparison/1,

    %% Regression Detection
    check_regressions/1,
    export_all_results/1
]).

%%%===================================================================
%%% Test Configuration
%%%===================================================================

-define(REGRESSION_THRESHOLD, 20).  %% 20% slowdown = regression

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @doc Returns list of all test cases and groups.
-spec all() -> [atom() | {group, atom()}].
all() ->
    [
        {group, pattern_benchmarks},
        {group, mining_benchmarks},
        {group, compilation_benchmarks},
        {group, nif_benchmarks},
        check_regressions,
        export_all_results
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [], [atom()]}].
groups() ->
    [
        {pattern_benchmarks, [], [
            pattern_basic_control_flow,
            pattern_advanced_branching,
            pattern_cancellation,
            pattern_complex
        ]},
        {mining_benchmarks, [], [
            mining_alpha_algorithm,
            mining_heuristic_miner,
            mining_scalability
        ]},
        {compilation_benchmarks, [], [
            compilation_spec_sizes,
            compilation_real_world
        ]},
        {nif_benchmarks, [], [
            nif_availability,
            nif_comparison
        ]}
    ].

%% @doc Suite-level initialization.
-spec init_per_suite(Config :: ct:config()) -> ct:config().
init_per_suite(Config) ->
    ct:pal("Starting Benchmark Suite"),
    ok = application:ensure_all_started(cre),

    %% Ensure benchmark modules are compiled
    Modules = [
        pattern_benchmarks,
        mining_benchmarks,
        compilation_benchmarks,
        nif_benchmarks
    ],
    lists:foreach(fun(M) ->
        case code:ensure_loaded(M) of
            {module, M} -> ok;
            _ ->
                ct:pal("Compiling ~p...", [M]),
                {ok, M} = compile:file(filename:join(["test/bench", atom_to_list(M) ++ ".erl"]))
        end
    end, Modules),

    %% Start benchmark servers
    {ok, _} = pattern_benchmarks:start_link(),
    {ok, _} = mining_benchmarks:start_link(),
    {ok, _} = compilation_benchmarks:start_link(),
    {ok, _} = nif_benchmarks:start_link(),

    ct:pal("Benchmark servers started"),
    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: ct:config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Completed Benchmark Suite"),
    ok.

%% @doc Group-level initialization.
-spec init_per_group(Group :: atom(), Config :: ct:config()) -> ct:config().
init_per_group(Group, Config) ->
    ct:pal("Starting benchmark group: ~p", [Group]),
    %% Force GC for consistent measurements
    garbage_collect(),
    Config.

%% @doc Group-level cleanup.
-spec end_per_group(Group :: atom(), Config :: ct:config()) -> ok.
end_per_group(Group, _Config) ->
    ct:pal("Completed benchmark group: ~p", [Group]),
    ok.

%%%===================================================================
%%% Pattern Benchmark Tests
%%%===================================================================

%% @doc Benchmark basic control flow patterns.
-spec pattern_basic_control_flow(Config :: ct:config()) -> ok.
pattern_basic_control_flow(_Config) ->
    ct:pal("Benchmarking basic control flow patterns"),

    {ok, Results} = pattern_benchmarks:run_pattern_group(basic_control_flow),

    %% Verify results
    Patterns = [sequence, parallel_split, synchronization,
                exclusive_choice, simple_merge],
    lists:foreach(fun(P) ->
        case maps:get(P, Results, undefined) of
            undefined -> ct:fail("Missing result for ~p", [P]);
            _ -> ok
        end
    end, Patterns),

    ct:pal("Basic control flow patterns benchmarked successfully"),
    ok.

%% @doc Benchmark advanced branching patterns.
-spec pattern_advanced_branching(Config :: ct:config()) -> ok.
pattern_advanced_branching(_Config) ->
    ct:pal("Benchmarking advanced branching patterns"),

    {ok, Results} = pattern_benchmarks:run_pattern_group(advanced_branching),

    %% Verify at least one result
    case map_size(Results) of
        0 -> ct:fail("No results for advanced branching");
        _ -> ok
    end,

    ct:pal("Advanced branching patterns benchmarked successfully"),
    ok.

%% @doc Benchmark cancellation patterns.
-spec pattern_cancellation(Config :: ct:config()) -> ok.
pattern_cancellation(_Config) ->
    ct:pal("Benchmarking cancellation patterns"),

    {ok, Results} = pattern_benchmarks:run_pattern_group(cancellation),

    case map_size(Results) of
        0 -> ct:fail("No results for cancellation patterns");
        _ -> ok
    end,

    ct:pal("Cancellation patterns benchmarked successfully"),
    ok.

%% @doc Benchmark complex patterns.
-spec pattern_complex(Config :: ct:config()) -> ok.
pattern_complex(_Config) ->
    ct:pal("Benchmarking complex patterns"),

    {ok, Results} = pattern_benchmarks:run_pattern_group(complex),

    case map_size(Results) of
        0 -> ct:fail("No results for complex patterns");
        _ -> ok
    end,

    ct:pal("Complex patterns benchmarked successfully"),
    ok.

%%%===================================================================
%%% Mining Benchmark Tests
%%%===================================================================

%% @doc Benchmark Alpha algorithm.
-spec mining_alpha_algorithm(Config :: ct:config()) -> ok.
mining_alpha_algorithm(_Config) ->
    ct:pal("Benchmarking Alpha algorithm"),

    {ok, Results} = mining_benchmarks:run_algorithm_benchmark(alpha),

    %% Verify we have results for different sizes
    Sizes = [small, medium, large],
    lists:foreach(fun(S) ->
        case maps:get(S, Results, undefined) of
            undefined -> ct:fail("Missing result for size ~p", [S]);
            _ -> ok
        end
    end, Sizes),

    ct:pal("Alpha algorithm benchmarked successfully"),
    ok.

%% @doc Benchmark Heuristic Miner.
-spec mining_heuristic_miner(Config :: ct:config()) -> ok.
mining_heuristic_miner(_Config) ->
    ct:pal("Benchmarking Heuristic Miner"),

    {ok, Results} = mining_benchmarks:run_algorithm_benchmark(heuristic),

    case map_size(Results) of
        0 -> ct:fail("No results for Heuristic Miner");
        _ -> ok
    end,

    ct:pal("Heuristic Miner benchmarked successfully"),
    ok.

%% @doc Benchmark mining algorithm scalability.
-spec mining_scalability(Config :: ct:config()) -> ok.
mining_scalability(_Config) ->
    ct:pal("Benchmarking mining scalability"),

    %% Generate large synthetic log
    Log = mining_benchmarks:generate_synthetic_log(1000, 15),

    %% Measure discovery time
    {TimeUs, _} = timer:tc(fun() ->
        process_discovery:discover(Log)
    end),

    TimeMs = TimeUs / 1000,
    ct:pal("Large log discovery time: ~.2f ms", [TimeMs]),

    %% Should complete within 10 seconds
    case TimeMs < 10000 of
        true -> ok;
        false -> ct:fail("Mining too slow: ~p ms", [TimeMs])
    end,

    ok.

%%%===================================================================
%%% Compilation Benchmark Tests
%%%===================================================================

%% @doc Benchmark different specification sizes.
-spec compilation_spec_sizes(Config :: ct:config()) -> ok.
compilation_spec_sizes(_Config) ->
    ct:pal("Benchmarking compilation for different spec sizes"),

    {ok, Results} = compilation_benchmarks:run_all_benchmarks(),

    %% Verify results for each size
    Sizes = [tiny, small, medium],
    lists:foreach(fun(S) ->
        case maps:get(S, Results, undefined) of
            undefined -> ct:fail("Missing result for size ~p", [S]);
            _ -> ok
        end
    end, Sizes),

    ct:pal("Compilation benchmarks completed successfully"),
    ok.

%% @doc Benchmark real-world YAWL file.
-spec compilation_real_world(Config :: ct:config()) -> ok.
compilation_real_world(Config) ->
    ct:pal("Benchmarking real-world YAWL file compilation"),

    %% Try to find order fulfillment YAWL file
    YawlFile = filename:join([
        proplists:get_value(priv_dir, Config),
        "..", "..", "..", "test", "fixtures", "orderfulfillment_2_1.yawl"
    ]),

    case filelib:is_file(YawlFile) of
        true ->
            {ok, Result} = compilation_benchmarks:benchmark_yawl_file(YawlFile),
            ParseTime = maps:get(parse_time_us, Result),
            ct:pal("Real YAWL file parse time: ~p us", [ParseTime]),
            ok;
        false ->
            ct:pal("Real YAWL file not found, skipping"),
            ok
    end.

%%%===================================================================
%%% NIF Benchmark Tests
%%%===================================================================

%% @doc Check NIF availability.
-spec nif_availability(Config :: ct:config()) -> ok.
nif_availability(_Config) ->
    ct:pal("Checking NIF availability"),

    Available = nif_benchmarks:check_nif_available(),
    ct:pal("NIF available: ~p", [Available]),

    %% Don't fail if NIF not available (optional feature)
    ok.

%% @doc Compare NIF vs pure Erlang.
-spec nif_comparison(Config :: ct:config()) -> ok.
nif_comparison(_Config) ->
    ct:pal("Comparing NIF vs pure Erlang performance"),

    case nif_benchmarks:check_nif_available() of
        true ->
            {ok, Result} = nif_benchmarks:compare_nif_vs_pure(alpha_discovery),
            Speedup = maps:get(speedup, Result),
            ct:pal("NIF speedup: ~.2fx", [Speedup]),

            %% NIF should be at least as fast
            case Speedup >= 0.8 of
                true -> ok;
                false -> ct:pal("Warning: NIF slower than pure Erlang")
            end,
            ok;
        false ->
            ct:pal("NIF not available, skipping comparison"),
            ok
    end.

%%%===================================================================
%%% Regression Detection Tests
%%%===================================================================

%% @doc Check for performance regressions.
-spec check_regressions(Config :: ct:config()) -> ok.
check_regressions(_Config) ->
    ct:pal("Checking for performance regressions"),

    {ok, Comparison} = pattern_benchmarks:compare_to_baseline(),

    case maps:get(status, Comparison, no_baseline) of
        no_baseline ->
            ct:pal("No baseline available for regression detection"),
            ok;
        compared ->
            Regressions = maps:get(regressions, Comparison, []),
            case Regressions of
                [] ->
                    ct:pal("No regressions detected"),
                    ok;
                _ ->
                    ct:pal("Regressions detected: ~p", [Regressions]),
                    %% Fail if significant regression
                    lists:foreach(fun({Pattern, Info}) ->
                        DiffPercent = maps:get(diff_percent, Info),
                        case DiffPercent > ?REGRESSION_THRESHOLD of
                            true ->
                                ct:fail("Performance regression in ~p: ~p% slower",
                                        [Pattern, round(DiffPercent)]);
                            false ->
                                ct:pal("Warning: ~p degraded by ~p%",
                                        [Pattern, round(DiffPercent)])
                        end
                    end, Regressions),
                    ok
            end
    end.

%% @doc Export all benchmark results.
-spec export_all_results(Config :: ct:config()) -> ok.
export_all_results(Config) ->
    ct:pal("Exporting all benchmark results"),

    OutputDir = proplists:get_value(priv_dir, Config),

    %% Export pattern results
    PatternFile = filename:join(OutputDir, "pattern_results.json"),
    ok = pattern_benchmarks:export_results(PatternFile),
    ct:pal("Exported pattern results to: ~s", [PatternFile]),

    %% Export mining results
    MiningFile = filename:join(OutputDir, "mining_results.json"),
    ok = mining_benchmarks:export_results(MiningFile),
    ct:pal("Exported mining results to: ~s", [MiningFile]),

    %% Export compilation results
    CompFile = filename:join(OutputDir, "compilation_results.json"),
    ok = compilation_benchmarks:export_results(CompFile),
    ct:pal("Exported compilation results to: ~s", [CompFile]),

    %% Export NIF results
    NifFile = filename:join(OutputDir, "nif_results.json"),
    ok = nif_benchmarks:export_results(NifFile),
    ct:pal("Exported NIF results to: ~s", [NifFile]),

    ok.
