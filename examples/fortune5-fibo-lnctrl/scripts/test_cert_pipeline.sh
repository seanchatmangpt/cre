#!/bin/sh
# Test Certification Pipeline
# Runs the full certification pipeline end-to-end

set -e

echo "=== Testing Certification Pipeline ==="

# Clean up previous evidence
rm -rf evidence/
mkdir -p evidence/uptime evidence/load_tests evidence/chaos evidence/reports receipts/

# Start Erlang shell and run tests
erl -pa _build/default/lib/*/ebin -noshell -eval "
    io:format(\"~n=== Starting Evidence Collectors ===~n\"),
    ok = f5_uptime_logger:start(),
    ok = f5_load_tester:start(),
    ok = chaos_controller:start(),

    io:format(\"~n=== Collecting Evidence ===~n\"),
    {ok, UptimeEvidence} = f5_uptime_logger:collect(),
    io:format(\"Uptime evidence: ~p~n\", [UptimeEvidence]),

    {ok, LoadTestEvidence} = f5_load_tester:collect(),
    io:format(\"Load test evidence: ~p~n\", [LoadTestEvidence]),

    {ok, ChaosEvidence} = chaos_controller:collect(),
    io:format(\"Chaos evidence: ~p~n\", [ChaosEvidence]),

    io:format(\"~n=== Verifying Evidence ===~n\"),
    ok = f5_uptime_logger:verify(),
    ok = f5_load_tester:verify(),
    ok = chaos_controller:verify(),
    io:format(\"All evidence verified successfully~n\"),

    io:format(\"~n=== Generating Certification Report ===~n\"),
    Report = generate_certification_report:generate(),
    io:format(\"Report: ~p~n\", [Report]),

    io:format(\"~n=== Building Verdict with Evidence ===~n\"),
    Verdict = verdict_builder:build_verdict(#{
        apps_generated => [<<\"f5_app_02\">>],
        ontology_hash => <<\"test123\">>,
        proofs_summary => #{},
        failing_tests => []
    }),
    io:format(\"Verdict: ~p~n\", [Verdict]),

    ok = verdict_builder:verify_verdict(Verdict),
    io:format(\"Verdict verified successfully~n\"),

    io:format(\"~n=== Starting Cert Runner ===~n\"),
    application:ensure_all_started(f5_cert_runner),

    timer:sleep(1000),

    {ok, Results} = f5_cert_runner:start_collection(),
    io:format(\"Collection results: ~p~n\", [Results]),

    Status = f5_cert_runner:get_status(),
    io:format(\"Runner status: ~p~n\", [Status]),

    SchedulerInfo = f5_cert_scheduler:get_schedule_info(),
    io:format(\"Scheduler info: ~p~n\", [SchedulerInfo]),

    {ok, FinalReport} = f5_cert_runner:generate_report(),
    io:format(\"Final report: ~p~n\", [FinalReport]),

    application:stop(f5_cert_runner),

    io:format(\"~n=== PIPELINE TEST COMPLETE ===~n\"),
    init:stop()
" -s init stop

echo ""
echo "=== Evidence Files ==="
find evidence/ -type f -ls

echo ""
echo "=== Receipt Files ==="
find receipts/ -type f -ls

echo ""
echo "=== Test Complete ==="
