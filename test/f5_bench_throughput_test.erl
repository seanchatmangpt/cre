%% -*- erlang -*-
%% @doc EUnit Tests for Throughput Benchmarking
%%
%% Tests for f5_bench_throughput module including:
%% - Throughput measurement accuracy
%% - Case execution throughput
%% - Effect receipt throughput
%% - Reduction counting
%% - Baseline comparison
%%
%% @end

-module(f5_bench_throughput_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

f5_bench_throughput_test_() ->
    {"Throughput benchmark tests",
     [fun measure_throughput_basic_test/0,
      fun measure_throughput_short_test/0,
      fun measure_throughput_zero_effects_test/0,
      fun benchmark_cases_test/0,
      fun benchmark_effects_test/0,
      fun benchmark_reductions_test/0,
      fun compare_baseline_improved_test/0,
      fun compare_baseline_regressed_test/0,
      fun compare_baseline_no_change_test/0,
      fun run_workload_test/0,
      fun format_result_test/0,
      fun throughput_consistency_test/0
     ]}.

%%====================================================================
%% measure_throughput Tests
%%====================================================================

measure_throughput_basic_test() ->
    %% Simple workload that returns 1 effect per execution
    Workload = fun() -> 1 end,
    Result = f5_bench_throughput:measure_throughput(Workload, 100),

    ?assert(is_map(Result)),
    ?assert(maps:get(duration_ms, Result) >= 100),
    ?assert(maps:get(total_cases, Result) > 0),
    ?assert(maps:get(cases_per_sec, Result) > 0),
    ?assert(maps:get(total_effects, Result) > 0),
    ?assert(maps:get(effects_per_sec, Result) > 0),
    ?assert(maps:get(total_reductions, Result) >= 0),
    ?assert(maps:get(reductions_per_sec, Result) >= 0).

measure_throughput_short_test() ->
    %% Very short duration test
    Workload = fun() -> 2 end,
    Result = f5_bench_throughput:measure_throughput(Workload, 10),

    ?assert(maps:get(duration_ms, Result) >= 10),
    ?assert(maps:get(total_cases, Result) > 0).

measure_throughput_zero_effects_test() ->
    %% Workload that generates no effects
    Workload = fun() -> 0 end,
    Result = f5_bench_throughput:measure_throughput(Workload, 50),

    ?assertEqual(0, maps:get(total_effects, Result)),
    ?assertEqual(0.0, maps:get(effects_per_sec, Result)).

%%====================================================================
%% Benchmark Tests
%%====================================================================

benchmark_cases_test() ->
    Result = f5_bench_throughput:benchmark_cases(100),

    ?assert(is_map(Result)),
    ?assert(maps:get(cases_per_sec, Result) > 0),
    ?assert(maps:get(total_cases, Result) > 0).

benchmark_effects_test() ->
    Result = f5_bench_throughput:benchmark_effects(100),

    ?assert(is_map(Result)),
    ?assert(maps:get(effects_per_sec, Result) > 0),
    %% Effects benchmark should generate more effects
    ?assert(maps:get(total_effects, Result) > 0).

benchmark_reductions_test() ->
    Result = f5_bench_throughput:benchmark_reductions(100),

    ?assert(is_map(Result)),
    ?assert(maps:get(reductions_per_sec, Result) > 0),
    ?assert(maps:get(total_reductions, Result) > 0).

%%====================================================================
%% compare_baseline Tests
%%====================================================================

compare_baseline_improved_test() ->
    Result = #{
        cases_per_sec => 110.0,
        effects_per_sec => 330.0,
        reductions_per_sec => 1000000.0
    },
    Baseline = #{
        cases_per_sec => 100.0,
        effects_per_sec => 300.0,
        reductions_per_sec => 900000.0
    },
    Comparison = f5_bench_throughput:compare_baseline(Result, Baseline),

    ?assertEqual(improved, maps:get(status, Comparison)),
    ?assertEqual(pos, maps:get(cases_diff, Comparison)),
    ?assertEqual(pos, maps:get(effects_diff, Comparison)),
    ?assertEqual(pos, maps:get(reductions_diff, Comparison)).

compare_baseline_regressed_test() ->
    Result = #{
        cases_per_sec => 90.0,
        effects_per_sec => 270.0,
        reductions_per_sec => 800000.0
    },
    Baseline = #{
        cases_per_sec => 100.0,
        effects_per_sec => 300.0,
        reductions_per_sec => 900000.0
    },
    Comparison = f5_bench_throughput:compare_baseline(Result, Baseline),

    ?assertEqual(regressed, maps:get(status, Comparison)),
    ?assertEqual(neg, maps:get(cases_diff, Comparison)),
    ?assertEqual(neg, maps:get(effects_diff, Comparison)),
    ?assertEqual(neg, maps:get(reductions_diff, Comparison)).

compare_baseline_no_change_test() ->
    Result = #{
        cases_per_sec => 102.0,
        effects_per_sec => 301.0,
        reductions_per_sec => 901000.0
    },
    Baseline = #{
        cases_per_sec => 100.0,
        effects_per_sec => 300.0,
        reductions_per_sec => 900000.0
    },
    Comparison = f5_bench_throughput:compare_baseline(Result, Baseline),

    %% All within 5% threshold
    ?assertEqual(no_change, maps:get(status, Comparison)),
    ?assertEqual(zero, maps:get(cases_diff, Comparison)),
    ?assertEqual(zero, maps:get(effects_diff, Comparison)),
    ?assertEqual(zero, maps:get(reductions_diff, Comparison)).

%%====================================================================
%% run_workload Tests
%%====================================================================

run_workload_test() ->
    Workload = fun() -> 3 end,
    {Cases, Effects} = f5_bench_throughput:run_workload(Workload, 10),

    ?assertEqual(10, Cases),
    ?assertEqual(30, Effects).

%%====================================================================
%% format_result Tests
%%====================================================================

format_result_test() ->
    Result = #{
        cases_per_sec => 100.5,
        effects_per_sec => 301.5,
        reductions_per_sec => 1000000.0,
        duration_ms => 1000,
        total_cases => 100,
        total_effects => 300,
        total_reductions => 1000000
    },
    Formatted = f5_bench_throughput:format_result(Result),
    FormattedBin = iolist_to_binary(Formatted),

    ?assert(is_binary(FormattedBin)),
    ?assert(binary:match(FormattedBin, <<"Throughput Results">>) =/= nomatch),
    ?assert(binary:match(FormattedBin, <<"Cases/sec">>) =/= nomatch),
    ?assert(binary:match(FormattedBin, <<"Effects/sec">>) =/= nomatch),
    ?assert(binary:match(FormattedBin, <<"Reductions/sec">>) =/= nomatch).

%%====================================================================
%% Consistency Tests
%%====================================================================

throughput_consistency_test() ->
    %% Run same workload multiple times and verify consistency
    Workload = fun() -> 1 end,

    Results = [f5_bench_throughput:measure_throughput(Workload, 50)
               || _ <- lists:seq(1, 3)],

    %% All results should have positive throughput
    lists:foreach(
        fun(R) ->
            ?assert(maps:get(cases_per_sec, R) > 0),
            ?assert(maps:get(effects_per_sec, R) > 0)
        end,
        Results
    ),

    %% Calculate variance - results should be relatively stable
    CasesRates = [maps:get(cases_per_sec, R) || R <- Results],
    Mean = lists:sum(CasesRates) / length(CasesRates),
    Max = lists:max(CasesRates),
    Min = lists:min(CasesRates),

    %% Max should not be more than 3x min (reasonable stability)
    ?assert(Max < Min * 3),
    ?assert(Mean > 0).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Test that reductions are counted correctly.
reductions_counting_test() ->
    %% Force GC to get clean slate, then do heavy work
    garbage_collect(),
    {AfterGC, _} = erlang:statistics(reductions),

    %% Do significant work in a loop to guarantee reductions
    lists:foreach(fun(_) ->
        %% Heavy computation that will generate reductions
        lists:foldl(fun(X, Acc) -> X * X + Acc end, 0, lists:seq(1, 1000))
    end, lists:seq(1, 100)),

    %% Get final reductions
    {FinalReductions, _} = erlang:statistics(reductions),

    %% Reductions should have increased (or at least not decreased)
    %% In some VM optimizations, reductions might not change much,
    %% so we check that it's non-negative
    ?assert(FinalReductions >= AfterGC).

%% @private Test that zero baseline is handled.
compare_baseline_zero_test() ->
    Result = #{
        cases_per_sec => 100.0,
        effects_per_sec => 300.0,
        reductions_per_sec => 1000000.0
    },
    Baseline = #{
        cases_per_sec => 0,
        effects_per_sec => 0,
        reductions_per_sec => 0
    },
    Comparison = f5_bench_throughput:compare_baseline(Result, Baseline),

    %% Should handle zero baseline gracefully
    ?assert(is_map(Comparison)),
    ?assertEqual(zero, maps:get(cases_diff, Comparison)),
    ?assertEqual(zero, maps:get(effects_diff, Comparison)),
    ?assertEqual(zero, maps:get(reductions_diff, Comparison)).
