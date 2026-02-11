%% Certification Pipeline End-to-End Test
-module(f5_cert_pipeline_test).
-include_lib("eunit/include/eunit.hrl").

%%% Tests

cert_runner_lifecycle_test() ->
    %% Start the cert_runner application
    application:ensure_all_started(f5_cert_runner),

    %% Verify services are running
    ?assert(whereis(f5_cert_runner) =/= undefined),
    ?assert(whereis(f5_cert_scheduler) =/= undefined),

    %% Get status
    Status = f5_cert_runner:get_status(),
    ?assertMatch(#{collection_count := 0}, Status),

    %% Stop
    application:stop(f5_cert_runner),
    ok.

evidence_collection_test() ->
    %% Start evidence collectors
    ok = f5_uptime_logger:start(),
    ok = f5_load_tester:start(),
    ok = chaos_controller:start(),

    %% Collect evidence from each module
    {ok, UptimeEvidence} = f5_uptime_logger:collect(),
    ?assertMatch(#{module := f5_uptime_logger, type := uptime_monitoring}, UptimeEvidence),
    ?assertMatch(#{evidence_hash := _}, UptimeEvidence),

    {ok, LoadTestEvidence} = f5_load_tester:collect(),
    ?assertMatch(#{module := f5_load_tester, type := load_testing}, LoadTestEvidence),
    ?assertMatch(#{evidence_hash := _}, LoadTestEvidence),

    {ok, ChaosEvidence} = chaos_controller:collect(),
    ?assertMatch(#{module := chaos_controller, type := chaos_engineering}, ChaosEvidence),
    ?assertMatch(#{evidence_hash := _}, ChaosEvidence),

    %% Verify evidence files exist
    ?assert(filelib:is_file("evidence/uptime/continuous_operation.json")),
    ?assert(filelib:is_file("evidence/load_tests/10k_concurrent_test.json")),
    ?assert(filelib:is_file("evidence/chaos/resilience_test.json")),

    %% Verify evidence
    ?assertEqual(ok, f5_uptime_logger:verify()),
    ?assertEqual(ok, f5_load_tester:verify()),
    ?assertEqual(ok, chaos_controller:verify()),

    %% Stop
    ok = f5_uptime_logger:stop(),
    ok = f5_load_tester:stop(),
    ok = chaos_controller:stop(),
    ok.

certification_report_test() ->
    %% Generate certification report
    Report = generate_certification_report:generate(),

    %% Verify report structure
    ?assertMatch(#{report_type := certification_report}, Report),
    ?assertMatch(#{evidence := _}, Report),
    ?assertMatch(#{assessment := _}, Report),

    %% Verify report file exists
    ?assert(filelib:is_file("evidence/reports/certification_latest.json")),

    ok.

verdict_with_evidence_test() ->
    %% Build verdict with evidence summary
    Verdict = verdict_builder:build_verdict(#{
        apps_generated => [<<"f5_app_02">>],
        ontology_hash => <<"test123">>,
        proofs_summary => #{},
        failing_tests => []
    }),

    %% Verify evidence summary is included
    ?assertMatch(#{evidence_summary := _}, Verdict),
    EvidenceSummary = maps:get(evidence_summary, Verdict),

    %% Verify evidence summary structure
    ?assertMatch(#{uptime := _}, EvidenceSummary),
    ?assertMatch(#{load_test := _}, EvidenceSummary),
    ?assertMatch(#{chaos := _}, EvidenceSummary),
    ?assertMatch(#{certification_report := _}, EvidenceSummary),

    %% Verify verdict
    ?assertEqual(ok, verdict_builder:verify_verdict(Verdict)),

    ok.

orchestrated_collection_test() ->
    %% Start cert_runner
    application:ensure_all_started(f5_cert_runner),

    %% Trigger collection
    {ok, Results} = f5_cert_runner:start_collection(),

    %% Verify all collectors succeeded
    ?assertEqual(3, length(Results)),
    lists:foreach(fun({Module, Status, _Result}) ->
        ?assertEqual(ok, Status),
        ?assert(lists:member(Module, [f5_uptime_logger, f5_load_tester, chaos_controller]))
    end, Results),

    %% Generate report
    {ok, Report} = f5_cert_runner:generate_report(),
    ?assertMatch(#{report_type := certification_report}, Report),

    %% Stop
    application:stop(f5_cert_runner),
    ok.

scheduler_info_test() ->
    %% Start cert_runner
    application:ensure_all_started(f5_cert_runner),

    %% Get scheduler info
    Info = f5_cert_scheduler:get_schedule_info(),

    %% Verify structure
    ?assertMatch(#{interval_ms := _, paused := _, collections := _}, Info),
    ?assertMatch(#{runtime_days := _, target_days := 90}, Info),
    ?assertMatch(#{progress_percent := _}, Info),

    %% Stop
    application:stop(f5_cert_runner),
    ok.
