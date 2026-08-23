%% -*- erlang -*-
%% @doc Benchmark Test Suite (Common Test)
%%
%% Integration suite for running benchmarks and detecting
%% performance regressions across test runs.
%%
%% @end

-module(bench_SUITE).
-author("CRE Team").

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [simple_bench, pattern_bench, memory_bench, regression_bench].

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Simple benchmark test case.
simple_bench(_Config) ->
    %% Define a simple function to benchmark
    Fun = fun() ->
        lists:seq(1, 100),
        lists:map(fun(X) -> X * 2 end, lists:seq(1, 100))
    end,

    %% Run benchmark
    Result = erl_bench:bench(Fun, 100),

    %% Verify results
    true = is_map(Result),
    true = maps:is_key(min, Result),
    true = maps:is_key(max, Result),
    true = maps:is_key(mean, Result),
    true = maps:is_key(median, Result),
    true = maps:is_key(p95, Result),
    true = maps:is_key(p99, Result),
    true = maps:is_key(std_dev, Result),
    100 = maps:get(iterations, Result),
    true = maps:get(min, Result) =< maps:get(mean, Result),
    true = maps:get(mean, Result) =< maps:get(max, Result),
    ok.

%% @doc Benchmark pattern execution.
pattern_bench(_Config) ->
    %% Benchmark list processing pattern
    ListFun = fun() ->
        lists:foldl(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 1000))
    end,

    Result = erl_bench:bench(ListFun, 50),

    %% Verify reasonable timing (should complete in ms)
    MaxUs = maps:get(max, Result),
    true = MaxUs < 1_000_000, %% Less than 1 second
    ok.

%% @doc Memory benchmark test.
memory_bench(_Config) ->
    %% Memory-intensive operation
    MemFun = fun() ->
        %% Allocate a large binary
        << <<X>> || X <- lists:seq(1, 10000) >>
    end,

    {Result, MemDiff} = mem_bench:measure_fun(MemFun),

    %% Verify result
    true = is_binary(Result),
    true = byte_size(Result) > 0,

    %% Verify memory tracking
    Diff = maps:get(diff, MemDiff),
    true = is_map(Diff),

    %% Verify structure
    Before = maps:get(before, MemDiff),
    AfterMem = maps:get(after_mem, MemDiff),
    true = is_map(Before),
    true = is_map(AfterMem),
    ok.

%% @doc Regression detection test.
regression_bench(_Config) ->
    %% Create baseline measurements
    BeforeSamples = [100, 102, 98, 101, 99, 100, 103, 97, 101, 99],

    %% Create "after" measurements that are similar (no regression)
    AfterNoRegression = [101, 100, 99, 102, 98, 101, 100, 99, 102, 100],

    %% Create "after" measurements that show regression (10% slower)
    AfterRegression = [110, 112, 108, 111, 109, 110, 113, 107, 111, 109],

    %% Test no regression
    false = stat:regression_detected(BeforeSamples, AfterNoRegression),

    %% Test with regression
    true = stat:regression_detected(BeforeSamples, AfterRegression),

    %% Test with custom threshold
    false = stat:regression_detected(BeforeSamples, AfterRegression, 0.15),

    ok.
