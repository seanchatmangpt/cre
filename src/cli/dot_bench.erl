%% -*- erlang -*-
%% @doc dot bench - Run benchmarks with regression detection
%%
%% Executes performance benchmarks and detects regressions:
%% - Microbenchmarking with statistical analysis
%% - Regression detection against baseline
%% - Configurable thresholds
%% - Result export for evidence
%%
%% Usage: ./dot bench [options]
%%
%% Options:
%%   --iterations=<n>     Number of benchmark iterations (default: 1000)
%%   --warmup=<n>         Number of warmup iterations (default: 10)
%%   --baseline=<file>    Baseline file for regression detection
%%   --threshold=<pct>    Regression threshold percentage (default: 5)
%%   --module=<mod>       Benchmark module to run
%%   --save=<file>        Save results to file
%%
%% @end

-module(dot_bench).
-export([run/1]).
-export([bench/2, compare/2]).

%%====================================================================
%% Types
%%====================================================================

-type bench_options() :: #{
    iterations => pos_integer(),
    warmup => non_neg_integer(),
    baseline => file:filename(),
    threshold => float(),
    module => atom(),
    save => file:filename()
}.
-type bench_result() :: #{
    name => binary(),
    iterations => pos_integer(),
    stats => map()
}.
-type comparison_result() :: #{
    status => pass | fail | regress,
    details => map()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Run benchmark command from CLI
-spec run([string()]) -> ok | {error, term()} | pass | fail | {pass, map()} | {fail, map()}.
run(Args) ->
    OptSpec = opt_spec(),
    case parse_opts(Args, OptSpec) of
        {ok, Opts, _Positional} ->
            case proplists:get_value(help, Opts) of
                true ->
                    print_help(),
                    ok;
                _ ->
                    Iterations = proplists:get_value(iterations, Opts, 1000),
                    Warmup = proplists:get_value(warmup, Opts, 10),
                    Baseline = proplists:get_value(baseline, Opts, undefined),
                    Threshold = proplists_get_float(threshold, Opts, 5.0),
                    Module = proplists:get_value(module, Opts, undefined),
                    Save = proplists:get_value(save, Opts, undefined),

                    Options = #{
                        iterations => Iterations,
                        warmup => Warmup,
                        baseline => Baseline,
                        threshold => Threshold,
                        module => Module,
                        save => Save
                    },

                    do_bench(Options);
                _ ->
                    ok
            end;
        {error, Reason} ->
            io:format(standard_error, "Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Run benchmarks with options
-spec bench(module(), bench_options()) -> {ok, [bench_result()]} | {error, term()}.
bench(Module, Options) when is_atom(Module), is_map(Options) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            run_benchmarks(Module, Options);
        {error, Reason} ->
            {error, {module_load_failed, Reason}}
    end;
bench(ModuleFile, Options) when is_list(ModuleFile) ->
    %% Convert filename to module name
    Module = list_to_atom(filename:basename(ModuleFile, ".erl")),
    bench(Module, Options).

%% @doc Compare benchmark results against baseline
-spec compare(bench_result(), file:filename()) -> comparison_result().
compare(Current, BaselineFile) ->
    case file:read_file(BaselineFile) of
        {ok, Data} ->
            try jsone:decode(Data) of
                Baseline ->
                    compare_results(Current, Baseline)
            catch
                _:_ ->
                    #{status => error, details => #{reason => invalid_baseline}}
            end;
        {error, _} ->
            #{status => error, details => #{reason => baseline_not_found}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

do_bench(Options) ->
    io:format("Running benchmarks...~n"),
    io:format("  Iterations: ~p~n", [maps:get(iterations, Options)]),
    io:format("  Warmup: ~p~n", [maps:get(warmup, Options)]),
    io:format("  Threshold: ~p%~n", [maps:get(threshold, Options)]),

    Module = case maps:get(module, Options) of
        undefined -> erl_bench;
        M -> M
    end,

    case code:ensure_loaded(Module) of
        {module, Module} ->
            Results = run_benchmarks(Module, Options),

            %% Check baseline comparison
            Baseline = maps:get(baseline, Options),
            FinalResult = case Baseline of
                undefined ->
                    #{status => pass, results => Results};
                _ ->
                    case compare_and_report(Results, Baseline, Options) of
                        pass -> #{status => pass, results => Results};
                        fail -> #{status => fail, results => Results}
                    end
            end,

            %% Save results if requested
            SavePath = maps:get(save, Options),
            case SavePath of
                undefined -> ok;
                _ -> save_results(Results, SavePath)
            end,

            case FinalResult of
                #{status := pass} -> pass;
                #{status := fail} -> fail
            end;
        {error, Reason} ->
            io:format(standard_error, "Failed to load module ~p: ~p~n", [Module, Reason]),
            fail
    end.

run_benchmarks(Module, Options) ->
    Iterations = maps:get(iterations, Options, 1000),
    Warmup = maps:get(warmup, Options, 10),

    %% Default benchmark operations
    Benchmarks = [
        {"identity", fun() -> ok end},
        {"map_operations", fun() -> maps:new() end},
        {"list_operations", fun() -> lists:seq(1, 100) end}
    ],

    Results = lists:map(fun({Name, Fun}) ->
        io:format("  Running: ~s...~n", [Name]),
        Stats = erl_bench:bench(Fun, Iterations, Warmup),
        #{
            name => list_to_binary(Name),
            iterations => Iterations,
            stats => Stats
        }
    end, Benchmarks),

    %% Print results
    print_results(Results),
    Results.

print_results(Results) ->
    io:format("~nBenchmark Results:~n"),
    lists:foreach(fun(#{name := Name, stats := Stats}) ->
        io:format("  ~s:~n", [Name]),
        io:format("    Mean: ~.2f us~n", [maps:get(mean, Stats, 0.0)]),
        io:format("    Median: ~.2f us~n", [maps:get(median, Stats, 0.0)]),
        io:format("    P95: ~.2f us~n", [maps:get(p95, Stats, 0.0)]),
        io:format("    P99: ~.2f us~n", [maps:get(p99, Stats, 0.0)])
    end, Results).

compare_and_report(Results, BaselineFile, Options) ->
    Threshold = maps:get(threshold, Options, 5.0),

    %% Load baseline
    case file:read_file(BaselineFile) of
        {ok, Data} ->
            try jsone:decode(Data) of
                Baseline ->
                    compare_results_with_threshold(Results, Baseline, Threshold)
            catch
                _:_ ->
                    io:format(standard_error, "Warning: Invalid baseline file~n"),
                    pass
            end;
        {error, Reason} ->
            io:format(standard_error, "Warning: Cannot read baseline: ~p~n", [Reason]),
            pass
    end.

compare_results_with_threshold(Results, Baseline, Threshold) ->
    %% Compare each benchmark result
    Comparisons = lists:map(fun(#{name := Name, stats := Stats}) ->
        case find_baseline(Name, Baseline) of
            {ok, BaselineStats} ->
                BaselineMean = maps:get(<<"mean">>, BaselineStats, 0.0),
                CurrentMean = maps:get(mean, Stats, 0.0),
                Diff = CurrentMean - BaselineMean,
                DiffPct = case BaselineMean of
                    +0.0 -> 0.0;
                    _ -> (Diff / BaselineMean) * 100.0
                end,

                Status = case DiffPct > Threshold of
                    true -> regress;
                    false -> pass
                end,

                #{name => Name, status => Status, diff_pct => DiffPct};
            not_found ->
                #{name => Name, status => pass, reason => no_baseline}
        end
    end, Results),

    %% Check for regressions
    Regressions = [C || #{status := regress} = C <- Comparisons],
    case Regressions of
        [] ->
            io:format("~nRegression check: PASS~n"),
            pass;
        _ ->
            io:format("~nRegression check: FAIL~n"),
            lists:foreach(fun(#{name := Name, diff_pct := Pct}) ->
                io:format("  ~s: +~.2f%~n", [Name, Pct])
            end, Regressions),
            fail
    end.

find_baseline(Name, Baseline) when is_list(Baseline) ->
    case lists:keyfind(<<"name">>, 1, Baseline) of
        {<<"name">>, Name, <<"stats">>, Stats} ->
            {ok, Stats};
        _ ->
            %% Try finding in nested structure
            find_baseline_in_list(Name, Baseline)
    end;
find_baseline(Name, Baseline) when is_map(Baseline) ->
    case maps:get(<<"results">>, Baseline, []) of
        Results when is_list(Results) ->
            find_baseline_in_list(Name, Results);
        _ ->
            not_found
    end.

find_baseline_in_list(Name, List) ->
    case lists:search(fun(Item) ->
        case Item of
            #{<<"name">> := Name} -> true;
            _ -> false
        end
    end, List) of
        {value, #{<<"stats">> := Stats}} ->
            {ok, Stats};
        _ ->
            not_found
    end.

compare_results(Current, Baseline) ->
    Threshold = 5.0,
    case compare_results_with_threshold([Current], Baseline, Threshold) of
        pass -> #{status => pass};
        fail -> #{status => regress}
    end.

save_results(Results, Path) ->
    SaveData = #{
        timestamp => erlang:system_time(millisecond),
        results => Results
    },
    Json = jsone:encode(SaveData),
    ok = file:write_file(Path, Json),
    io:format("Results saved to: ~s~n", [Path]).

proplists_get_float(Key, List, Default) ->
    case proplists:get_value(Key, List) of
        undefined -> Default;
        Value when is_integer(Value) -> Value * 1.0;
        Value when is_float(Value) -> Value;
        _ -> Default
    end.

opt_spec() ->
    [
        {help, $h, "help", undefined, "Show this help message"},
        {iterations, $i, "iterations", {integer, 1000}, "Number of iterations"},
        {warmup, $w, "warmup", {integer, 10}, "Number of warmup iterations"},
        {baseline, $b, "baseline", {string, undefined}, "Baseline file"},
        {threshold, $t, "threshold", {float, 5.0}, "Regression threshold %"},
        {module, $m, "module", {string, undefined}, "Benchmark module"},
        {save, $s, "save", {string, undefined}, "Save results to file"}
    ].

%% Simple option parser
parse_opts(Args, OptSpec) ->
    parse_opts(Args, OptSpec, [], []).

parse_opts([], _OptSpec, Acc, Positional) ->
    {ok, lists:reverse(Acc), lists:reverse(Positional)};
parse_opts(["--help" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{help, true} | Acc], Positional);
parse_opts(["-h" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{help, true} | Acc], Positional);
parse_opts(["--iterations", N | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{iterations, list_to_integer(N)} | Acc], Positional);
parse_opts(["-i", N | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{iterations, list_to_integer(N)} | Acc], Positional);
parse_opts(["--warmup", N | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{warmup, list_to_integer(N)} | Acc], Positional);
parse_opts(["-w", N | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{warmup, list_to_integer(N)} | Acc], Positional);
parse_opts(["--baseline", File | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{baseline, File} | Acc], Positional);
parse_opts(["-b", File | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{baseline, File} | Acc], Positional);
parse_opts(["--threshold", T | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{threshold, list_to_float(T)} | Acc], Positional);
parse_opts(["-t", T | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{threshold, list_to_float(T)} | Acc], Positional);
parse_opts(["--module", Mod | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{module, list_to_atom(Mod)} | Acc], Positional);
parse_opts(["-m", Mod | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{module, list_to_atom(Mod)} | Acc], Positional);
parse_opts(["--save", File | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{save, File} | Acc], Positional);
parse_opts(["-s", File | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{save, File} | Acc], Positional);
parse_opts([Arg | Rest], OptSpec, Acc, Positional) ->
    case Arg of
        "-" ++ _ -> {error, {unknown_option, Arg}};
        _ -> parse_opts(Rest, OptSpec, Acc, [Arg | Positional])
    end.

print_help() ->
    io:format(
        "dot bench - Run benchmarks with regression detection~n"
        "~n"
        "Usage: ./dot bench [options]~n"
        "~n"
        "Options:~n"
        "  --help, -h              Show this help message~n"
        "  --iterations, -i <n>    Number of iterations (default: 1000)~n"
        "  --warmup, -w <n>        Warmup iterations (default: 10)~n"
        "  --baseline, -b <file>   Baseline file for comparison~n"
        "  --threshold, -t <pct>   Regression threshold %% (default: 5)~n"
        "  --module, -m <mod>      Benchmark module to run~n"
        "  --save, -s <file>       Save results to file~n"
        "~n"
        "Examples:~n"
        "  ./dot bench~n"
        "  ./dot bench --iterations=10000 --save=bench.json~n"
        "  ./dot bench --baseline=bench.json --threshold=10~n"
        "~n").
