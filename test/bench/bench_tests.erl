%% -*- erlang -*-
%% @doc EUnit Tests for Benchmark Framework
%%
%% Tests for erl_bench, stat, and mem_bench modules.
%%
%% @end

-module(bench_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

bench_test_() ->
    {"Benchmark framework tests",
     [fun erl_bench_basic_test/0,
      fun erl_bench_with_warmup_test/0,
      fun erl_bench_mfa_test/0,
      fun stat_mean_test/0,
      fun stat_median_test/0,
      fun stat_std_dev_test/0,
      fun stat_percentile_test/0,
      fun stat_compare_test/0,
      fun stat_regression_test/0,
      fun mem_bench_usage_test/0,
      fun mem_bench_diff_test/0,
      fun mem_bench_measure_test/0,
      fun microbenchmark_accuracy_test/0,
      fun microbenchmark_consistency_test/0
     ]}.

%%====================================================================
%% erl_bench Tests
%%====================================================================

erl_bench_basic_test() ->
    %% Simple function returning a constant
    Fun = fun() -> 42 end,
    Result = erl_bench:bench(Fun, 10),

    ?assertEqual(10, maps:get(iterations, Result)),
    ?assert(maps:get(min, Result) >= 0),
    ?assert(maps:get(max, Result) >= maps:get(min, Result)),
    ?assert(maps:get(mean, Result) >= 0),
    ?assert(maps:get(median, Result) >= 0),
    ?assert(maps:get(p95, Result) >= 0),
    ?assert(maps:get(p99, Result) >= 0),
    ?assert(maps:get(p999, Result) >= 0),
    ?assert(maps:get(std_dev, Result) >= 0).

erl_bench_with_warmup_test() ->
    Fun = fun() -> lists:sum(lists:seq(1, 100)) end,
    Result = erl_bench:bench(Fun, 5, 10),

    ?assertEqual(5, maps:get(iterations, Result)),
    ?assert(maps:get(total_time, Result) > 0).

erl_bench_mfa_test() ->
    %% Test MFA format benchmark
    Result = erl_bench:bench({erl_bench, identity_function, [test_input]}, 5),

    ?assertEqual(5, maps:get(iterations, Result)),
    ?assert(maps:get(mean, Result) >= 0).

erl_bench_with_options_test() ->
    Fun = fun() -> lists:reverse(lists:seq(1, 100)) end,
    Options = #{warmup => 2, gc => true},
    Result = erl_bench:bench(Fun, 5, Options),

    ?assertEqual(5, maps:get(iterations, Result)).

%%====================================================================
%% stat Tests
%%====================================================================

stat_mean_test() ->
    ?assertEqual(5.0, stat:mean([5, 5, 5])),
    ?assertEqual(3.0, stat:mean([1, 2, 3, 4, 5])),

    ?assertError(_, stat:mean([])).

stat_median_test() ->
    ?assertEqual(3.0, stat:median([1, 2, 3, 4, 5])),
    ?assertEqual(2.5, stat:median([1, 2, 3, 4])),
    ?assertEqual(10.0, stat:median([10])),
    ?assertEqual(5.0, stat:median([5, 5, 5, 5, 5])),

    ?assertError(_, stat:median([])).

stat_std_dev_test() ->
    ?assertEqual(0.0, stat:std_dev([5, 5, 5])),
    StdDev = stat:std_dev([2, 4, 4, 4, 5, 5, 7, 9]),
    ?assert(StdDev > 0).

stat_percentile_test() ->
    Samples = lists:seq(1, 100),
    ?assertEqual(1.0, stat:percentile(Samples, 0)),
    ?assertEqual(100.0, stat:percentile(Samples, 100)),
    ?assert(stat:percentile(Samples, 50) > 40.0),
    ?assert(stat:percentile(Samples, 50) < 60.0),
    ?assert(stat:percentile(Samples, 95) >= 90.0).

stat_compare_test() ->
    Before = [100, 100, 100, 100, 100],
    AfterSame = [100, 100, 100, 100, 100],
    AfterFaster = [90, 90, 90, 90, 90],
    AfterSlower = [110, 110, 110, 110, 110],

    ResultSame = stat:compare(Before, AfterSame),
    ?assertEqual(no_change, maps:get(status, ResultSame)),

    ResultFaster = stat:compare(Before, AfterFaster),
    ?assertEqual(improved, maps:get(status, ResultFaster)),

    ResultSlower = stat:compare(Before, AfterSlower),
    ?assertEqual(regressed, maps:get(status, ResultSlower)).

stat_regression_test() ->
    Before = [100, 100, 100, 100, 100],
    AfterNoReg = [102, 98, 101, 99, 100],
    AfterReg = [115, 120, 110, 118, 112],

    ?assertEqual(false, stat:regression_detected(Before, AfterNoReg)),
    ?assertEqual(true, stat:regression_detected(Before, AfterReg)),
    ?assertEqual(false, stat:regression_detected(Before, AfterReg, 0.25)).

stat_summarize_test() ->
    Samples = [1, 2, 3, 4, 5],
    Summary = stat:summarize(Samples),

    ?assertEqual(5, maps:get(n, Summary)),
    ?assertEqual(1, maps:get(min, Summary)),
    ?assertEqual(5, maps:get(max, Summary)),
    ?assertEqual(3.0, maps:get(mean, Summary)),
    ?assertEqual(3.0, maps:get(median, Summary)),
    ?assert(is_float(maps:get(median, Summary))).

%%====================================================================
%% mem_bench Tests
%%====================================================================

mem_bench_usage_test() ->
    MemInfo = mem_bench:mem_usage(),

    ?assert(maps:is_key(total, MemInfo)),
    ?assert(maps:is_key(processes, MemInfo)),
    ?assert(maps:is_key(system, MemInfo)),
    ?assert(maps:get(total, MemInfo) > 0).

mem_bench_diff_test() ->
    Before = #{total => 1000, processes => 500, system => 500,
               atom => 0, binary => 0, code => 0, ets => 0,
               processes_used => 400},
    AfterMem = #{total => 1200, processes => 600, system => 600,
              atom => 10, binary => 20, code => 0, ets => 0,
              processes_used => 500},

    Diff = mem_bench:mem_diff(Before, AfterMem),

    ?assertEqual(Before, maps:get(before, Diff)),
    ?assertEqual(AfterMem, maps:get(after_mem, Diff)),
    DiffMap = maps:get(diff, Diff),
    ?assertEqual(200, maps:get(total, DiffMap)),
    ?assertEqual(10, maps:get(atom, DiffMap)).

mem_bench_measure_test() ->
    Fun = fun() ->
        %% Allocate some memory
        lists:duplicate(100, 42)
    end,

    {Result, Diff} = mem_bench:measure_fun(Fun),

    ?assert(is_list(Result)),
    ?assertEqual(100, length(Result)),
    ?assert(is_map(Diff)),
    ?assert(maps:is_key(before, Diff)),
    ?assert(maps:is_key(after_mem, Diff)),
    ?assert(maps:is_key(diff, Diff)).

%%====================================================================
%% Microbenchmark Accuracy Tests
%%====================================================================

microbenchmark_accuracy_test() ->
    %% Test that we can measure very fast operations
    FastFun = fun() -> 1 + 1 end,

    Result = erl_bench:bench(FastFun, 100),

    %% Even fast operations should take measurable time
    ?assert(maps:get(mean, Result) >= 0),
    ?assert(maps:get(total_time, Result) > 0),

    %% All iterations should have completed
    ?assertEqual(100, maps:get(iterations, Result)).

microbenchmark_consistency_test() ->
    %% Test that benchmarks are reasonably consistent
    Fun = fun() -> lists:sum(lists:seq(1, 1000)) end,

    %% Run multiple benchmarks
    Results = [erl_bench:bench(Fun, 50) || _ <- lists:seq(1, 5)],

    %% Check that all results are similar (within 2x)
    Means = [maps:get(mean, R) || R <- Results],
    MinMean = lists:min(Means),
    MaxMean = lists:max(Means),

    %% Max should not be more than 2x min for stable code
    ?assert(MaxMean < MinMean * 2).

%%====================================================================
%% Helper Functions
%%====================================================================
