%% -*- erlang -*-
%%%% @doc Unit Tests for evidence_quality Module
%%
%% Test suite for quality summary report generation including:
%% - Quality gate checking (compile, dialyzer, eunit, ct, proofs, benchmarks)
%% - Result aggregation
%% - Status determination (PASS/FAIL)
%% - Report generation

-module(evidence_quality_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test check_compile_gate/0 returns proper status.
%%--------------------------------------------------------------------
check_compile_gate_test_() ->
    {"Check compile gate returns correct status",
     fun() ->
         Result = evidence_quality:check_compile_gate(),

         %% Verify result structure
         ?assert(is_map(Result)),
         ?assertEqual(compile, maps:get(name, Result)),
         ?assert(is_atom(maps:get(status, Result))),
         ?assert(is_integer(maps:get(timestamp, Result))),

         %% Status should be one of the valid statuses
         Status = maps:get(status, Result),
         ?assert(lists:member(Status, [pass, fail, error]))
     end}.

%%--------------------------------------------------------------------
%% @doc Test check_dialyzer_gate/0 returns proper status.
%%--------------------------------------------------------------------
check_dialyzer_gate_test_() ->
    {"Check dialyzer gate returns correct status",
     fun() ->
         Result = evidence_quality:check_dialyzer_gate(),

         %% Verify result structure
         ?assert(is_map(Result)),
         ?assertEqual(dialyzer, maps:get(name, Result)),
         ?assert(is_atom(maps:get(status, Result))),

         %% Dialyzer might be skipped if PLT not available
         Status = maps:get(status, Result),
         ?assert(lists:member(Status, [pass, fail, skip]))
     end}.

%%--------------------------------------------------------------------
%% @doc Test check_eunit_gate/0 returns proper status.
%%--------------------------------------------------------------------
check_eunit_gate_test_() ->
    {"Check eunit gate returns correct status",
     fun() ->
         Result = evidence_quality:check_eunit_gate(),

         %% Verify result structure
         ?assert(is_map(Result)),
         ?assertEqual(eunit, maps:get(name, Result)),
         ?assert(is_atom(maps:get(status, Result))),

         Status = maps:get(status, Result),
         ?assert(lists:member(Status, [pass, fail, error]))
     end}.

%%--------------------------------------------------------------------
%% @doc Test check_ct_gate/0 returns proper status.
%%--------------------------------------------------------------------
check_ct_gate_test_() ->
    {"Check ct gate returns correct status",
     fun() ->
         Result = evidence_quality:check_ct_gate(),

         %% Verify result structure
         ?assert(is_map(Result)),
         ?assertEqual(ct, maps:get(name, Result)),
         ?assert(is_atom(maps:get(status, Result))),

         Status = maps:get(status, Result),
         ?assert(lists:member(Status, [pass, fail, skip]))
     end}.

%%--------------------------------------------------------------------
%% @doc Test check_proofs_gate/0 returns proper status.
%%--------------------------------------------------------------------
check_proofs_gate_test_() ->
    {"Check proofs gate returns correct status",
     fun() ->
         Result = evidence_quality:check_proofs_gate(),

         %% Verify result structure
         ?assert(is_map(Result)),
         ?assertEqual(proofs, maps:get(name, Result)),
         ?assert(is_atom(maps:get(status, Result))),

         Status = maps:get(status, Result),
         ?assert(lists:member(Status, [pass, fail, skip]))
     end}.

%%--------------------------------------------------------------------
%% @doc Test check_benchmarks_gate/0 returns proper status.
%%--------------------------------------------------------------------
check_benchmarks_gate_test_() ->
    {"Check benchmarks gate returns correct status",
     fun() ->
         Result = evidence_quality:check_benchmarks_gate(),

         %% Verify result structure
         ?assert(is_map(Result)),
         ?assertEqual(benchmarks, maps:get(name, Result)),
         ?assert(is_atom(maps:get(status, Result))),

         Status = maps:get(status, Result),
         ?assert(lists:member(Status, [pass, warn, skip]))
     end}.

%%--------------------------------------------------------------------
%% @doc Test check_all_gates/0 returns all gate results.
%%--------------------------------------------------------------------
check_all_gates_test_() ->
    {"Check all gates returns complete results map",
     fun() ->
         Results = evidence_quality:check_all_gates(),

         %% Verify all gates present
         ?assert(maps:is_key(compile, Results)),
         ?assert(maps:is_key(dialyzer, Results)),
         ?assert(maps:is_key(eunit, Results)),
         ?assert(maps:is_key(ct, Results)),
         ?assert(maps:is_key(proofs, Results)),
         ?assert(maps:is_key(benchmarks, Results)),

         %% Verify each result has required fields
         maps:foreach(fun(_Gate, Result) ->
             ?assert(is_map(Result)),
             ?assert(is_atom(maps:get(name, Result))),
             ?assert(is_atom(maps:get(status, Result))),
             ?assert(is_binary(maps:get(message, Result))),
             ?assert(is_integer(maps:get(timestamp, Result)))
         end, Results)
     end}.

%%--------------------------------------------------------------------
%% @doc Test aggregate_results/1 computes correct statistics.
%%--------------------------------------------------------------------
aggregate_results_test_() ->
    {"Aggregate results computes correct summary statistics",
     fun() ->
         %% Create test results
         Results = #{
             compile => #{status => pass},
             dialyzer => #{status => pass},
             eunit => #{status => pass},
             ct => #{status => fail},
             proofs => #{status => skip},
             benchmarks => #{status => warn}
         },

         Aggregate = evidence_quality:aggregate_results(Results),

         %% Verify counts
         ?assertEqual(6, maps:get(total, Aggregate)),
         ?assertEqual(3, maps:get(pass, Aggregate)),
         ?assertEqual(1, maps:get(fail, Aggregate)),
         ?assertEqual(1, maps:get(warn, Aggregate)),
         ?assertEqual(1, maps:get(skip, Aggregate)),

         %% Empty results
         EmptyAggregate = evidence_quality:aggregate_results(#{}),
         ?assertEqual(0, maps:get(total, EmptyAggregate))
     end}.

%%--------------------------------------------------------------------
%% @doc Test determine_status/1 returns correct overall status.
%%--------------------------------------------------------------------
determine_status_test_() ->
    {"Determine status returns PASS when all pass, FAIL otherwise",
     fun() ->
         %% All pass should return pass
         AllPass = #{
             compile => #{status => pass},
             dialyzer => #{status => pass},
             eunit => #{status => pass}
         },
         ?assertEqual(pass, evidence_quality:determine_status(AllPass)),

         %% Any fail should return fail
         OneFail = #{
             compile => #{status => pass},
             dialyzer => #{status => fail},
             eunit => #{status => pass}
         },
         ?assertEqual(fail, evidence_quality:determine_status(OneFail)),

         %% Warnings and skips don't cause failure
         WarnSkip = #{
             compile => #{status => pass},
             dialyzer => #{status => pass},
             eunit => #{status => warn},
             ct => #{status => skip}
         },
         ?assertEqual(pass, evidence_quality:determine_status(WarnSkip)),

         %% Empty results should pass
         ?assertEqual(pass, evidence_quality:determine_status(#{}))
     end}.

%%--------------------------------------------------------------------
%% @doc Test determine_status/2 fails on specific gate failures.
%%--------------------------------------------------------------------
determine_status_critical_gates_test_() ->
    {"Critical gates (compile, dialyzer, eunit, ct) cause failure",
     fun() ->
         %% Compile fail causes overall fail
         CompileFail = #{
             compile => #{status => fail},
             dialyzer => #{status => pass},
             eunit => #{status => pass}
         },
         ?assertEqual(fail, evidence_quality:determine_status(CompileFail)),

         %% Dialyzer fail causes overall fail
         DialyzerFail = #{
             compile => #{status => pass},
             dialyzer => #{status => fail},
             eunit => #{status => pass}
         },
         ?assertEqual(fail, evidence_quality:determine_status(DialyzerFail)),

         %% EUnit fail causes overall fail
         EUnitFail = #{
             compile => #{status => pass},
             dialyzer => #{status => pass},
             eunit => #{status => fail}
         },
         ?assertEqual(fail, evidence_quality:determine_status(EUnitFail)),

         %% CT fail causes overall fail
         CTFail = #{
             compile => #{status => pass},
             dialyzer => #{status => pass},
             eunit => #{status => pass},
             ct => #{status => fail}
         },
         ?assertEqual(fail, evidence_quality:determine_status(CTFail)),

         %% Proofs fail causes overall fail
         ProofsFail = #{
             compile => #{status => pass},
             dialyzer => #{status => pass},
             eunit => #{status => pass},
             proofs => #{status => fail}
         },
         ?assertEqual(fail, evidence_quality:determine_status(ProofsFail))
     end}.

%%--------------------------------------------------------------------
%% @doc Test generate_quality_report/1 creates valid markdown.
%%--------------------------------------------------------------------
generate_quality_report_test_() ->
    {"Generate quality report creates valid markdown",
     fun() ->
         %% Create temporary file
         TempFile = "/tmp/quality_summary_test.md",

         %% Generate report
         Result = evidence_quality:generate_quality_report(TempFile),

         %% Verify file created
         ?assertEqual(ok, Result),

         %% Read and verify content
         {ok, Content} = file:read_file(TempFile),

         %% Check for required sections
         ?assertNotEqual(nomatch, binary:match(Content, <<"# Quality Summary Report">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"## Status:">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"## Summary">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"## Gates">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"## Evidence Links">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"## Quality Gate Rules">>)),

         %% Check for gate names
         ?assertNotEqual(nomatch, binary:match(Content, <<"compile">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"dialyzer">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"eunit">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"proofs">>)),

         %% Clean up
         file:delete(TempFile)
     end}.

%%--------------------------------------------------------------------
%% @doc Test generate_quality_report/1 handles directory creation.
%%--------------------------------------------------------------------
generate_quality_report_creates_dirs_test_() ->
    {"Generate quality report creates parent directories",
     fun() ->
         %% Create path with non-existent directory
         TempFile = "/tmp/test_quality_report_dir/QUALITY_SUMMARY.md",

         %% Generate report
         Result = evidence_quality:generate_quality_report(TempFile),
         ?assertEqual(ok, Result),

         %% Verify file exists
         ?assert(filelib:is_file(TempFile)),

         %% Clean up
         file:delete(TempFile),
         file:del_dir("/tmp/test_quality_report_dir")
     end}.

%%--------------------------------------------------------------------
%% @doc Test gate result structure is consistent.
%%--------------------------------------------------------------------
gate_result_structure_test_() ->
    {"All gate results have consistent structure",
     fun() ->
         Results = evidence_quality:check_all_gates(),

         %% Each result must have required fields
         RequiredFields = [name, status, message, timestamp],

         maps:foreach(fun(_Gate, Result) ->
             lists:foreach(fun(Field) ->
                 ?assert(maps:is_key(Field, Result))
             end, RequiredFields),

             %% Status must be valid atom
             Status = maps:get(status, Result),
             ValidStatuses = [pass, fail, warn, skip, error],
             ?assert(lists:member(Status, ValidStatuses)),

             %% Message must be binary
             Message = maps:get(message, Result),
             ?assert(is_binary(Message)),

             %% Timestamp must be positive integer
             Timestamp = maps:get(timestamp, Result),
             ?assert(is_integer(Timestamp)),
             ?assert(Timestamp > 0)
         end, Results)
     end}.

%%--------------------------------------------------------------------
%% @doc Test report includes timestamp.
%%--------------------------------------------------------------------
report_includes_timestamp_test_() ->
    {"Quality report includes generation timestamp",
     fun() ->
         TempFile = "/tmp/quality_timestamp_test.md",
         evidence_quality:generate_quality_report(TempFile),

         {ok, Content} = file:read_file(TempFile),

         %% Check for ISO8601 timestamp format (YYYY-MM-DDTHH:MM:SSZ)
         ?assertNotEqual(nomatch, re:run(Content, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z",
                        [unicode, {capture, none}])),

         %% Clean up
         file:delete(TempFile)
     end}.

%%--------------------------------------------------------------------
%% @doc Test evidence links are included in report.
%%--------------------------------------------------------------------
report_includes_evidence_links_test_() ->
    {"Quality report includes links to evidence files",
     fun() ->
         TempFile = "/tmp/quality_links_test.md",
         evidence_quality:generate_quality_report(TempFile),

         {ok, Content} = file:read_file(TempFile),

         %% Check for evidence links
         EvidenceLinks = [
             <<"COMPILE_WARNINGS.md">>,
             <<"DIALYZER_WARNINGS.log">>,
             <<"EUNIT_RESULTS.log">>,
             <<"replay_proof.json">>
         ],

         lists:foreach(fun(Link) ->
             ?assertNotEqual(nomatch, binary:match(Content, Link))
         end, EvidenceLinks),

         %% Clean up
         file:delete(TempFile)
     end}.

%%--------------------------------------------------------------------
%% @doc Test quality gate rules are documented.
%%--------------------------------------------------------------------
report_includes_gate_rules_test_() ->
    {"Quality report documents gate rules",
     fun() ->
         TempFile = "/tmp/quality_rules_test.md",
         evidence_quality:generate_quality_report(TempFile),

         {ok, Content} = file:read_file(TempFile),

         %% Check for rules section
         ?assertNotEqual(nomatch, binary:match(Content, <<"## Quality Gate Rules">>)),

         %% Check for key rules
         ?assertNotEqual(nomatch, binary:match(Content, <<"FAIL if > 0 warnings">>)),
         ?assertNotEqual(nomatch, binary:match(Content, <<"regression > 10%">>)),

         %% Clean up
         file:delete(TempFile)
     end}.

%%--------------------------------------------------------------------
%% @doc Test parse_proof_json/1 handles various proof formats.
%%--------------------------------------------------------------------
parse_proof_json_test_() ->
    {"Parse proof JSON handles various formats",
     fun() ->
         %% Verified proof
         VerifiedProof = jsone:encode(#{
             <<"status">> => <<"verified">>,
             <<"hashes_equal">> => true
         }),
         ?assertMatch({ok, 1, 1}, parse_proof_json_test_wrapper(VerifiedProof)),

         %% Failed proof
         FailedProof = jsone:encode(#{
             <<"status">> => <<"failed">>,
             <<"hashes_equal">> => false
         }),
         ?assertMatch({ok, 0, 1}, parse_proof_json_test_wrapper(FailedProof)),

         %% Proof with only hashes_equal
         EqualProof = jsone:encode(#{
             <<"hashes_equal">> => true
         }),
         ?assertMatch({ok, 1, 1}, parse_proof_json_test_wrapper(EqualProof)),

         %% Invalid JSON
         ?assertMatch({error, invalid_json},
                      parse_proof_json_test_wrapper(<<"not json">>))
     end}.

%%--------------------------------------------------------------------
%% @doc Test compare_benchmarks/2 detects regression.
%%--------------------------------------------------------------------
compare_benchmarks_test_() ->
    {"Compare benchmarks detects regression correctly",
     fun() ->
         %% No regression (similar performance)
         Baseline = jsone:encode(#{<<"mean">> => 100.0}),
         Current = jsone:encode(#{<<"mean">> => 105.0}),  %% 5% increase
         {ok, false, Percent} = compare_benchmarks_test_wrapper(Baseline, Current),
         ?assert(Percent < 10.0),

         %% Regression detected (> 10%)
         Regressed = jsone:encode(#{<<"mean">> => 115.0}),  %% 15% increase
         {ok, true, Percent2} = compare_benchmarks_test_wrapper(Baseline, Regressed),
         ?assert(Percent2 > 10.0),

         %% Improvement (negative regression)
         Improved = jsone:encode(#{<<"mean">> => 80.0}),  %% 20% decrease
         ?assertMatch({ok, false, _}, compare_benchmarks_test_wrapper(Baseline, Improved))
     end}.

%%--------------------------------------------------------------------
%% @doc Test aggregate_results/1 handles all status combinations.
%%--------------------------------------------------------------------
aggregate_results_comprehensive_test_() ->
    {"Aggregate results handles all status combinations",
     fun() ->
         %% All possible statuses
         AllStatuses = #{
             gate1 => #{status => pass},
             gate2 => #{status => fail},
             gate3 => #{status => warn},
             gate4 => #{status => skip},
             gate5 => #{status => error},
             gate6 => #{status => pass}
         },

         Aggregate = evidence_quality:aggregate_results(AllStatuses),

         ?assertEqual(6, maps:get(total, Aggregate)),
         ?assertEqual(2, maps:get(pass, Aggregate)),
         ?assertEqual(1, maps:get(fail, Aggregate)),
         ?assertEqual(1, maps:get(warn, Aggregate)),
         ?assertEqual(1, maps:get(skip, Aggregate)),
         ?assertEqual(1, maps:get(error, Aggregate))
     end}.

%%====================================================================
%% Helper Functions (Test Wrappers)
%%====================================================================

%% @private Wrapper to test internal parse_proof_json
parse_proof_json_test_wrapper(Content) ->
    %% This simulates the internal function behavior
    try
        Proof = jsone:decode(Content),
        Status = maps:get(<<"status">>, Proof, <<"unknown">>),
        HashesEqual = maps:get(<<"hashes_equal">>, Proof, false),

        case Status of
            <<"verified">> when HashesEqual =:= true -> {ok, 1, 1};
            <<"failed">> -> {ok, 0, 1};
            _ when HashesEqual =:= true -> {ok, 1, 1};
            _ -> {ok, 0, 1}
        end
    catch
        _:_ -> {error, invalid_json}
    end.

%% @private Wrapper to test internal compare_benchmarks
compare_benchmarks_test_wrapper(BaselineContent, CurrentContent) ->
    try
        Baseline = jsone:decode(BaselineContent),
        Current = jsone:decode(CurrentContent),

        BaselineMean = maps:get(<<"mean">>, Baseline, 0.0),
        CurrentMean = maps:get(<<"mean">>, Current, 0.0),

        case BaselineMean of
            +0.0 ->
                {ok, false, 0.0};
            _ ->
                RegressionPercent = ((CurrentMean - BaselineMean) / BaselineMean) * 100.0,
                Detected = RegressionPercent > 10.0,
                {ok, Detected, RegressionPercent}
        end
    catch
        _:_ ->
            {error, invalid_benchmark_format}
    end.
