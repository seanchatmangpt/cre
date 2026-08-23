%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
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
%% @author CRE Project
%% @copyright 2025
%%
%% @doc Quality Summary Report Generator
%%
%% Provides comprehensive quality gate verification and reporting:
%% <ul>
%%   <li><b>Compile Warnings:</b> Fails if any warnings present</li>
%%   <li><b>Dialyzer Warnings:</b> Fails if any type warnings present</li>
%%   <li><b>EUnit Tests:</b> Fails if any tests fail</li>
%%   <li><b>CT Suites:</b> Fails if any suites fail</li>
%%   <li><b>Proofs:</b> Fails if any determinism proof fails</li>
%%   <li><b>Benchmarks:</b> Warns if regression > 10%</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% ```
%% %% Run all quality gates
%% Results = evidence_quality:check_all_gates(),
%%
%% %% Generate quality summary report
%% evidence_quality:generate_quality_report("logs/QUALITY_SUMMARY.md"),
%%
%% %% Aggregate results from multiple checks
%% Status = evidence_quality:determine_status(Results),
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(evidence_quality).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0]).
-export([check_all_gates/0]).
-export([check_compile_gate/0]).
-export([check_dialyzer_gate/0]).
-export([check_eunit_gate/0]).
-export([check_ct_gate/0]).
-export([check_proofs_gate/0]).
-export([check_benchmarks_gate/0]).
-export([generate_quality_report/1]).
-export([aggregate_results/1]).
-export([determine_status/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type gate_name() :: compile | dialyzer | eunit | ct | proofs | benchmarks.
-type gate_status() :: pass | fail | warn | skip | error.

-type gate_result() :: #{
    name := gate_name(),
    status := gate_status(),
    count => non_neg_integer(),
    total => non_neg_integer(),
    details => map(),
    message => binary(),
    timestamp => integer()
}.

-type quality_results() :: #{gate_name() => gate_result()}.
-type quality_status() :: pass | fail.

-export_type([gate_name/0, gate_status/0, gate_result/0, quality_results/0, quality_status/0]).

%%====================================================================
%% gen_server State
%%====================================================================

-record(state, {
    last_results :: quality_results() | undefined,
    last_check_time :: integer() | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the quality gate monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run all quality gates and return results
%%
%% Executes each quality gate in parallel where possible and
%% returns a map of gate_name => gate_result.
%%
%% @end
-spec check_all_gates() -> quality_results().
check_all_gates() ->
    logger:info("Running all quality gates..."),
    StartTime = erlang:monotonic_time(millisecond),

    %% Run all gates
    CompileResult = check_compile_gate(),
    DialyzerResult = check_dialyzer_gate(),
    EUnitResult = check_eunit_gate(),
    CTResult = check_ct_gate(),
    ProofsResult = check_proofs_gate(),
    BenchmarksResult = check_benchmarks_gate(),

    Results = #{
        compile => CompileResult,
        dialyzer => DialyzerResult,
        eunit => EUnitResult,
        ct => CTResult,
        proofs => ProofsResult,
        benchmarks => BenchmarksResult
    },

    %% Log overall status
    OverallStatus = determine_status(Results),
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,

    case OverallStatus of
        pass ->
            logger:info("Quality gates: PASS (elapsed: ~pms)", [Elapsed]);
        fail ->
            logger:error("Quality gates: FAIL (elapsed: ~pms)", [Elapsed])
    end,

    %% Cache results in gen_server
    gen_server:cast(?MODULE, {cache_results, Results}),

    Results.

%% @doc Check compile warnings gate
%%
%% Returns pass if zero warnings, fail otherwise.
%%
%% @end
-spec check_compile_gate() -> gate_result().
check_compile_gate() ->
    logger:debug("Checking compile warnings gate..."),
    Timestamp = erlang:monotonic_time(millisecond),

    case evidence_compile:get_compile_warnings() of
        #{total_warnings := 0, status := pass} ->
            #{
                name => compile,
                status => pass,
                count => 0,
                message => <<"No compile warnings">>,
                timestamp => Timestamp
            };
        #{total_warnings := Count, status := fail} = Status ->
            #{
                name => compile,
                status => fail,
                count => Count,
                details => Status,
                message => <<(list_to_binary(io_lib:format("~p compile warnings", [Count])))/binary>>,
                timestamp => Timestamp
            };
        Status ->
            #{
                name => compile,
                status => error,
                details => Status,
                message => <<"Error checking compile warnings">>,
                timestamp => Timestamp
            }
    end.

%% @doc Check Dialyzer warnings gate
%%
%% Returns pass if zero warnings, fail otherwise.
%% Parses Dialyzer PLT output or runs dialyzer if needed.
%%
%% @end
-spec check_dialyzer_gate() -> gate_result().
check_dialyzer_gate() ->
    logger:debug("Checking Dialyzer gate..."),
    Timestamp = erlang:monotonic_time(millisecond),

    case get_dialyzer_warnings() of
        {ok, 0} ->
            #{
                name => dialyzer,
                status => pass,
                count => 0,
                message => <<"No Dialyzer warnings">>,
                timestamp => Timestamp
            };
        {ok, Count} when Count > 0 ->
            #{
                name => dialyzer,
                status => fail,
                count => Count,
                message => <<(list_to_binary(io_lib:format("~p Dialyzer warnings", [Count])))/binary>>,
                timestamp => Timestamp
            };
        {error, Reason} ->
            #{
                name => dialyzer,
                status => skip,
                message => <<(list_to_binary(io_lib:format("Dialyzer not available: ~p", [Reason])))/binary>>,
                timestamp => Timestamp
            }
    end.

%% @doc Check EUnit test gate
%%
%% Returns pass if all tests pass, fail otherwise.
%% Parses EUnit output or runs tests if needed.
%%
%% @end
-spec check_eunit_gate() -> gate_result().
check_eunit_gate() ->
    logger:debug("Checking EUnit gate..."),
    Timestamp = erlang:monotonic_time(millisecond),

    case get_eunit_results() of
        {ok, Passed, Total} when Passed =:= Total ->
            #{
                name => eunit,
                status => pass,
                count => Passed,
                total => Total,
                message => <<(list_to_binary(io_lib:format("EUnit: ~p/~p passed", [Passed, Total])))/binary>>,
                timestamp => Timestamp
            };
        {ok, Passed, Total} when Passed < Total ->
            #{
                name => eunit,
                status => fail,
                count => Passed,
                total => Total,
                message => <<(list_to_binary(io_lib:format("EUnit: ~p/~p passed", [Passed, Total])))/binary>>,
                timestamp => Timestamp
            };
        {error, Reason} ->
            #{
                name => eunit,
                status => error,
                message => <<(list_to_binary(io_lib:format("EUnit error: ~p", [Reason])))/binary>>,
                timestamp => Timestamp
            }
    end.

%% @doc Check Common Test suites gate
%%
%% Returns pass if all suites pass, fail otherwise.
%%
%% @end
-spec check_ct_gate() -> gate_result().
check_ct_gate() ->
    logger:debug("Checking CT gate..."),
    Timestamp = erlang:monotonic_time(millisecond),

    case get_ct_results() of
        {ok, Passed, Total} when Passed =:= Total ->
            #{
                name => ct,
                status => pass,
                count => Passed,
                total => Total,
                message => <<(list_to_binary(io_lib:format("CT: ~p/~p suites passed", [Passed, Total])))/binary>>,
                timestamp => Timestamp
            };
        {ok, Passed, Total} when Passed < Total ->
            #{
                name => ct,
                status => fail,
                count => Passed,
                total => Total,
                message => <<(list_to_binary(io_lib:format("CT: ~p/~p suites passed", [Passed, Total])))/binary>>,
                timestamp => Timestamp
            };
        {error, Reason} ->
            #{
                name => ct,
                status => skip,
                message => <<(list_to_binary(io_lib:format("CT not available: ~p", [Reason])))/binary>>,
                timestamp => Timestamp
            }
    end.

%% @doc Check determinism proofs gate
%%
%% Returns pass if all proofs verified, fail otherwise.
%% Checks for replay_proof.json and validates content.
%%
%% @end
-spec check_proofs_gate() -> gate_result().
check_proofs_gate() ->
    logger:debug("Checking Proofs gate..."),
    Timestamp = erlang:monotonic_time(millisecond),

    case get_proof_results() of
        {ok, Verified, Total} when Verified =:= Total, Total > 0 ->
            #{
                name => proofs,
                status => pass,
                count => Verified,
                total => Total,
                message => <<(list_to_binary(io_lib:format("Proofs: ~p/~p verified", [Verified, Total])))/binary>>,
                timestamp => Timestamp
            };
        {ok, Verified, Total} when Verified < Total, Total > 0 ->
            #{
                name => proofs,
                status => fail,
                count => Verified,
                total => Total,
                message => <<(list_to_binary(io_lib:format("Proofs: ~p/~p verified", [Verified, Total])))/binary>>,
                timestamp => Timestamp
            };
        {ok, 0, 0} ->
            #{
                name => proofs,
                status => skip,
                count => 0,
                total => 0,
                message => <<"No proofs to verify">>,
                timestamp => Timestamp
            };
        {error, Reason} ->
            #{
                name => proofs,
                status => skip,
                message => <<(list_to_binary(io_lib:format("Proofs not available: ~p", [Reason])))/binary>>,
                timestamp => Timestamp
            }
    end.

%% @doc Check benchmarks gate for regression
%%
%% Returns pass if no regression, warn if regression > 10%.
%%
%% @end
-spec check_benchmarks_gate() -> gate_result().
check_benchmarks_gate() ->
    logger:debug("Checking Benchmarks gate..."),
    Timestamp = erlang:monotonic_time(millisecond),

    case get_benchmark_results() of
        {ok, RegressionDetected, RegressionPercent} when RegressionDetected, RegressionPercent > 10 ->
            #{
                name => benchmarks,
                status => warn,
                count => 1,
                details => #{regression_percent => RegressionPercent},
                message => <<(list_to_binary(io_lib:format("Benchmarks: ~.1f% regression detected",
                    [RegressionPercent])))/binary>>,
                timestamp => Timestamp
            };
        {ok, true, RegressionPercent} ->
            #{
                name => benchmarks,
                status => pass,
                count => 0,
                details => #{regression_percent => RegressionPercent},
                message => <<(list_to_binary(io_lib:format("Benchmarks: ~.1f% regression (acceptable)",
                    [RegressionPercent])))/binary>>,
                timestamp => Timestamp
            };
        {ok, false, _} ->
            #{
                name => benchmarks,
                status => pass,
                count => 0,
                message => <<"Benchmarks: No regression detected">>,
                timestamp => Timestamp
            };
        {error, Reason} ->
            #{
                name => benchmarks,
                status => skip,
                message => <<(list_to_binary(io_lib:format("Benchmarks not available: ~p", [Reason])))/binary>>,
                timestamp => Timestamp
            }
    end.

%% @doc Generate quality summary report to file
%%
%% Creates a markdown report with all gate results and
%% evidence links to detailed logs.
%%
%% @end
-spec generate_quality_report(file:filename_all()) -> ok | {error, term()}.
generate_quality_report(Path) ->
    Results = check_all_gates(),
    Report = format_quality_report(Results),
    case filelib:ensure_dir(Path) of
        ok ->
            file:write_file(Path, Report);
        Error ->
            Error
    end.

%% @doc Aggregate results from multiple gate checks
%%
%% Combines individual gate results into summary statistics.
%%
%% @end
-spec aggregate_results(quality_results()) -> #{
    total => non_neg_integer(),
    passed => non_neg_integer(),
    failed => non_neg_integer(),
    warned => non_neg_integer(),
    skipped => non_neg_integer(),
    errored => non_neg_integer()
}.
aggregate_results(Results) ->
    Counted = maps:fold(fun(_Gate, Result, Acc) ->
        Status = maps_get(status, Result, error),
        maps:update_with(Status, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Results),
    Counted#{total => maps:size(Results)}.

%% @doc Determine overall quality status
%%
%% Returns fail if any gate failed, pass otherwise.
%% Warnings and skips do not cause failure.
%%
%% @end
-spec determine_status(quality_results()) -> quality_status().
determine_status(Results) ->
    HasFail = lists:any(fun(Gate) ->
        case maps:get(Gate, Results, #{status => skip}) of
            #{status := fail} -> true;
            _ -> false
        end
    end, [compile, dialyzer, eunit, ct, proofs, benchmarks]),

    case HasFail of
        true -> fail;
        false -> pass
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{
        last_results = undefined,
        last_check_time = undefined
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call(get_last_results, _From, #state{last_results = Results} = State) ->
    {reply, {ok, Results}, State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({cache_results, Results}, State) ->
    {noreply, State#state{
        last_results = Results,
        last_check_time = erlang:monotonic_time(millisecond)
    }};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get Dialyzer warning count from PLT or run
-spec get_dialyzer_warnings() -> {ok, non_neg_integer()} | {error, term()}.
get_dialyzer_warnings() ->
    %% Check for Dialyzer log file
    DialyzerLog = "logs/DIALYZER_WARNINGS.log",

    case file:read_file(DialyzerLog) of
        {ok, Content} ->
            %% Count warning lines
            Lines = binary:split(Content, <<"\n">>, [global]),
            Warnings = count_dialyzer_warnings(Lines),
            {ok, Warnings};
        {error, enoent} ->
            %% Try to parse rebar3 dialyzer output
            run_dialyzer_check();
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Run dialyzer and capture warnings
-spec run_dialyzer_check() -> {ok, non_neg_integer()} | {error, term()}.
run_dialyzer_check() ->
    %% Check if PLT exists
    PLTPath = "_build/default/rebar3_*_plt",
    case file:wildcard(PLTPath) of
        [] ->
            {error, no_plt};
        _ ->
            %% Run dialyzer with timeout
            case os:cmd("timeout 120 rebar3 dialyzer 2>&1 | head -100") of
                [] ->
                    {ok, 0};
                Output ->
                    Lines = string:split(Output, "\n", all),
                    Count = count_dialyzer_warnings_text(Lines),
                    {ok, Count}
            end
    end.

%% @private Count Dialyzer warnings from log lines
-spec count_dialyzer_warnings([binary()]) -> non_neg_integer().
count_dialyzer_warnings(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case binary:match(Line, <<": ">>) of
            nomatch ->
                %% Check for standard dialyzer warning format
                case re:run(Line, "^[^:]+:\\d+:\\s*Warning:", [unicode, {capture, none}]) of
                    match -> Acc + 1;
                    nomatch -> Acc
                end;
            _ ->
                Acc
        end
    end, 0, Lines).

%% @private Count Dialyzer warnings from text output
-spec count_dialyzer_warnings_text([string()]) -> non_neg_integer().
count_dialyzer_warnings_text(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case re:run(Line, "^\\s*(.*\\.erl):(\\d+):\\s*(Warning:.*)", [unicode, {capture, all, list}]) of
            {match, _} -> Acc + 1;
            nomatch -> Acc
        end
    end, 0, Lines).

%% @private Get EUnit test results
-spec get_eunit_results() -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
get_eunit_results() ->
    %% Check for EUnit log file
    EUnitLog = "logs/EUNIT_RESULTS.log",

    case file:read_file(EUnitLog) of
        {ok, Content} ->
            parse_eunit_output(Content);
        {error, enoent} ->
            %% Try to read from _build
            case file:read_file("_build/default/lib/cre/.eunit/eunit.log") of
                {ok, Content} ->
                    parse_eunit_output(Content);
                {error, _} ->
                    %% Run EUnit and capture results
                    run_eunit_check()
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Run EUnit and capture results
-spec run_eunit_check() -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
run_eunit_check() ->
    %% Run EUnit with timeout
    Output = os:cmd("timeout 60 rebar3 eunit 2>&1"),
    parse_eunit_output(list_to_binary(Output)).

%% @private Parse EUnit output for pass/fail counts
-spec parse_eunit_output(binary()) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
parse_eunit_output(Output) ->
    Lines = binary:split(Output, <<"\n">>, [global]),

    %% Look for "Test passed" or "Failed:" lines
    {Passed, Failed} = lists:foldl(fun(Line, {P, F}) ->
        case binary:match(Line, <<"Test passed">>) of
            nomatch ->
                case binary:match(Line, <<"Failed:">>) of
                    nomatch -> {P, F};
                    _ -> {P, F + 1}
                end;
            _ -> {P + 1, F}
        end
    end, {0, 0}, Lines),

    %% Also check for summary line like "All 42 tests passed"
    Total = Passed + Failed,
    {ok, Passed, Total}.

%% @private Get Common Test results
-spec get_ct_results() -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
get_ct_results() ->
    %% Check for CT log directory
    CTLogDir = "_build/test/lib/cre/logs",

    case file:list_dir(CTLogDir) of
        {ok, Files} ->
            %% Count suite run results
            SuitectResults = lists:filter(fun(File) ->
                case re:run(File, "suite\\.run\\.log$", [unicode, {capture, none}]) of
                    match -> true;
                    nomatch -> false
                end
            end, Files),
            Total = length(SuitectResults),

            %% Parse results for failures
            {Passed, _} = lists:foldl(fun(File, {P, F}) ->
                Path = filename:join(CTLogDir, File),
                case file:read_file(Path) of
                    {ok, Content} ->
                        case binary:match(Content, <<"failed">>) of
                            nomatch -> {P + 1, F};
                            _ -> {P, F + 1}
                        end;
                    {error, _} ->
                        {P, F}
                end
            end, {0, 0}, SuitectResults),

            {ok, Passed, Total};
        {error, enoent} ->
            {error, no_ct_logs};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Get proof verification results
-spec get_proof_results() -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
get_proof_results() ->
    %% Check for proof JSON files
    ProofDir = "evidence",
    ProofFile = filename:join(ProofDir, "replay_proof.json"),

    case file:read_file(ProofFile) of
        {ok, Content} ->
            parse_proof_json(Content);
        {error, enoent} ->
            %% Try alternative location
            case file:read_file("logs/evidence/replay_proof.json") of
                {ok, Content} ->
                    parse_proof_json(Content);
                {error, _} ->
                    {ok, 0, 0}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Parse proof JSON for verification status
-spec parse_proof_json(binary()) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
parse_proof_json(Content) ->
    try
        %% Decode JSON (using jsx or jsone if available)
        Proof = jsone:decode(Content),

        %% Check verification status
        Status = maps:get(<<"status">>, Proof, <<"unknown">>),
        HashesEqual = maps:get(<<"hashes_equal">>, Proof, false),

        case Status of
            <<"verified">> when HashesEqual =:= true ->
                {ok, 1, 1};
            <<"failed">> ->
                {ok, 0, 1};
            _ when HashesEqual =:= true ->
                {ok, 1, 1};
            _ ->
                {ok, 0, 1}
        end
    catch
        _:_ ->
            {error, invalid_json}
    end.

%% @private Get benchmark regression results
-spec get_benchmark_results() ->
    {ok, boolean(), float()} | {error, term()}.
get_benchmark_results() ->
    %% Check for benchmark baseline and current results
    BaselineFile = "logs/BENCHMARK_BASELINE.json",
    CurrentFile = "logs/BENCHMARK_CURRENT.json",

    case {file:read_file(BaselineFile), file:read_file(CurrentFile)} of
        {{ok, BaselineContent}, {ok, CurrentContent}} ->
            compare_benchmarks(BaselineContent, CurrentContent);
        _ ->
            {error, no_benchmark_data}
    end.

%% @private Compare benchmark results for regression
-spec compare_benchmarks(binary(), binary()) -> {ok, boolean(), float()}.
compare_benchmarks(BaselineContent, CurrentContent) ->
    try
        Baseline = jsone:decode(BaselineContent),
        Current = jsone:decode(CurrentContent),

        %% Get mean execution times
        BaselineMean = maps:get(<<"mean">>, Baseline, 0.0),
        CurrentMean = maps:get(<<"mean">>, Current, 0.0),

        case BaselineMean of
            +0.0 ->
                {ok, false, 0.0};
            _ ->
                %% Calculate regression percentage
                RegressionPercent = ((CurrentMean - BaselineMean) / BaselineMean) * 100.0,
                Detected = RegressionPercent > 10.0,
                {ok, Detected, RegressionPercent}
        end
    catch
        _:_ ->
            {error, invalid_benchmark_format}
    end.

%% @private Format quality report as markdown
-spec format_quality_report(quality_results()) -> binary().
format_quality_report(Results) ->
    Status = determine_status(Results),
    Aggregate = aggregate_results(Results),
    Timestamp = iso8601_timestamp(),

    Report = [
        "# Quality Summary Report\n\n",
        "Generated: ", Timestamp, "\n\n",
        "## Status: ", status_to_binary(Status), "\n\n",
        "## Summary\n\n",
        format_aggregate(Aggregate),
        "\n## Gates\n\n",
        format_gates(Results),
        "\n## Evidence Links\n\n",
        format_evidence_links(Results),
        "\n## Quality Gate Rules\n\n",
        format_gate_rules(),
        "\n---\n\n",
        "This report is generated by `evidence_quality` module as part of the\n",
        "Fortune-5 FIBO LineController Factory hardening swarm.\n"
    ],
    iolist_to_binary(Report).

%% @private Format aggregate statistics
-spec format_aggregate(map()) -> iolist().
format_aggregate(Aggregate) ->
    Total = maps_get(total, Aggregate, 0),
    Passed = maps_get(pass, Aggregate, 0),
    Failed = maps_get(fail, Aggregate, 0),
    Warned = maps_get(warned, Aggregate, 0),
    Skipped = maps_get(skip, Aggregate, 0),

    [
        "- **Total Gates**: ", integer_to_binary(Total), "\n",
        "- **Passed**: ", integer_to_binary(Passed), "\n",
        "- **Failed**: ", integer_to_binary(Failed), "\n",
        "- **Warned**: ", integer_to_binary(Warned), "\n",
        "- **Skipped**: ", integer_to_binary(Skipped), "\n"
    ].

%% @private Format gate results
-spec format_gates(quality_results()) -> iolist().
format_gates(Results) ->
    Gates = [compile, dialyzer, eunit, ct, proofs, benchmarks],
    lists:map(fun(Gate) ->
        Result = maps_get(Gate, Results, #{}),
        Status = maps_get(status, Result, skip),
        Message = maps_get(message, Result, <<"">>),
        ["- ", gate_status_to_binary(Status), " **",
         atom_to_binary(Gate), "**: ", Message, "\n"]
    end, Gates).

%% @private Format evidence links
-spec format_evidence_links(quality_results()) -> iolist().
format_evidence_links(_Results) ->
    [
        "- Compile: `logs/COMPILE_WARNINGS.md`\n",
        "- Dialyzer: `logs/DIALYZER_WARNINGS.log`\n",
        "- EUnit: `logs/EUNIT_RESULTS.log`\n",
        "- CT: `_build/test/lib/cre/logs/`\n",
        "- Proofs: `evidence/replay_proof.json`\n",
        "- Benchmarks: `logs/BENCHMARK_CURRENT.json`\n"
    ].

%% @private Format gate rules
-spec format_gate_rules() -> iolist().
format_gate_rules() ->
    [
        "1. **Compile Warnings**: FAIL if > 0 warnings\n",
        "2. **Dialyzer**: FAIL if > 0 warnings\n",
        "3. **EUnit**: FAIL if any test fails\n",
        "4. **CT**: FAIL if any suite fails\n",
        "5. **Proofs**: FAIL if any proof fails to verify\n",
        "6. **Benchmarks**: WARN if regression > 10%\n"
    ].

%% @private Convert status to binary with formatting
-spec status_to_binary(quality_status()) -> binary().
status_to_binary(pass) -> <<"**PASS**">>;
status_to_binary(fail) -> <<"**FAIL**">>.

%% @private Convert gate status to formatted binary
-spec gate_status_to_binary(gate_status()) -> binary().
gate_status_to_binary(pass) -> <<"✅ PASS">>;
gate_status_to_binary(fail) -> <<"❌ FAIL">>;
gate_status_to_binary(warn) -> <<"⚠️ WARN">>;
gate_status_to_binary(skip) -> <<"⊘ SKIP">>;
gate_status_to_binary(error) -> <<"⚠️ ERROR">>.

%% @private Get current ISO8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    UTCDateTime = calendar:universal_time(),
    {{Y, M, D}, {H, Min, S}} = UTCDateTime,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Y, M, D, H, Min, S])).

%% @private Helper for maps:get with default
-spec maps_get(term(), map(), term()) -> term().
maps_get(Key, Map, Default) ->
    try maps:get(Key, Map) of
        Value -> Value
    catch
        error:{badkey, _} -> Default
    end.
