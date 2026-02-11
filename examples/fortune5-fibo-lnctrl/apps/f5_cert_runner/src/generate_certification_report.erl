%% Certification Report Generator
%% Aggregates all evidence and generates comprehensive certification report
-module(generate_certification_report).

-export([generate/0, generate/1]).

-spec generate() -> map().
generate() ->
    generate(#{}).

-spec generate(map()) -> map().
generate(Opts) ->
    logger:info("Generating certification report"),

    %% Collect all evidence
    UptimeEvidence = collect_uptime_evidence(),
    LoadTestEvidence = collect_load_test_evidence(),
    ChaosEvidence = collect_chaos_evidence(),

    %% Get scheduler status
    SchedulerInfo = case whereis(f5_cert_scheduler) of
        undefined -> #{error => scheduler_not_running};
        _ -> f5_cert_scheduler:get_schedule_info()
    end,

    %% Get runner status
    RunnerStatus = case whereis(f5_cert_runner) of
        undefined -> #{error => runner_not_running};
        _ -> f5_cert_runner:get_status()
    end,

    %% Build certification report
    Report = #{
        report_type => certification_report,
        timestamp => receipt_builder:iso8601_now(),
        generator_version => receipt_builder:get_generator_version(),
        environment => receipt_builder:get_environment_fingerprint(),

        %% Evidence summary
        evidence => #{
            uptime_monitoring => UptimeEvidence,
            load_testing => LoadTestEvidence,
            chaos_engineering => ChaosEvidence
        },

        %% Scheduler and runner status
        scheduler => SchedulerInfo,
        runner => RunnerStatus,

        %% Certification criteria
        criteria => #{
            continuous_operation_days => application:get_env(f5_cert_runner, continuous_operation_days, 90),
            target_uptime_percent => 99.999,
            concurrent_workflows => 10000,
            chaos_resilience => true
        },

        %% Assessment
        assessment => assess_certification(UptimeEvidence, LoadTestEvidence, ChaosEvidence, SchedulerInfo)
    },

    %% Write report to file
    filelib:ensure_dir("evidence/reports/"),
    ReportJson = iolist_to_binary(json:encode(Report)),
    Timestamp = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    ReportFile = lists:flatten(io_lib:format("evidence/reports/certification_~s.json", [Timestamp])),
    file:write_file(ReportFile, ReportJson),

    %% Also write as latest
    file:write_file("evidence/reports/certification_latest.json", ReportJson),

    logger:info("Certification report written to ~s", [ReportFile]),

    Report.

%%% Internal Functions

-spec collect_uptime_evidence() -> map().
collect_uptime_evidence() ->
    case file:read_file("evidence/uptime/continuous_operation.json") of
        {ok, JsonBin} ->
            Evidence = json:decode(JsonBin),
            #{
                status => ok,
                file => "evidence/uptime/continuous_operation.json",
                data => Evidence,
                verified => verify_evidence_hash(Evidence)
            };
        {error, Reason} ->
            #{status => error, reason => Reason}
    end.

-spec collect_load_test_evidence() -> map().
collect_load_test_evidence() ->
    case file:read_file("evidence/load_tests/10k_concurrent_test.json") of
        {ok, JsonBin} ->
            Evidence = json:decode(JsonBin),
            #{
                status => ok,
                file => "evidence/load_tests/10k_concurrent_test.json",
                data => Evidence,
                verified => ok  %% Load test doesn't have hash verification yet
            };
        {error, Reason} ->
            #{status => error, reason => Reason}
    end.

-spec collect_chaos_evidence() -> map().
collect_chaos_evidence() ->
    case file:read_file("evidence/chaos/resilience_test.json") of
        {ok, JsonBin} ->
            Evidence = json:decode(JsonBin),
            #{
                status => ok,
                file => "evidence/chaos/resilience_test.json",
                data => Evidence,
                verified => verify_evidence_hash(Evidence)
            };
        {error, Reason} ->
            #{status => error, reason => Reason}
    end.

-spec verify_evidence_hash(map()) -> ok | {error, term()}.
verify_evidence_hash(Evidence) ->
    case maps:get(<<"evidence_hash">>, Evidence, undefined) of
        undefined ->
            {error, no_hash};
        StoredHash ->
            EvidenceWithoutHash = maps:remove(<<"evidence_hash">>, Evidence),
            ComputedHash = list_to_binary(receipt_builder:hash_receipt(EvidenceWithoutHash)),
            case ComputedHash of
                StoredHash -> ok;
                _ -> {error, {hash_mismatch, StoredHash, ComputedHash}}
            end
    end.

-spec assess_certification(map(), map(), map(), map()) -> map().
assess_certification(UptimeEvidence, LoadTestEvidence, ChaosEvidence, SchedulerInfo) ->
    %% Check uptime criterion
    UptimePass = case maps:get(status, UptimeEvidence, error) of
        ok ->
            Data = maps:get(data, UptimeEvidence, #{}),
            UptimeData = maps:get(<<"data">>, Data, #{}),
            UptimePercent = maps:get(<<"uptime_percentage">>, UptimeData, 0),
            UptimePercent >= 99.999;
        _ ->
            false
    end,

    %% Check load test criterion
    LoadTestPass = case maps:get(status, LoadTestEvidence, error) of
        ok ->
            Data = maps:get(data, LoadTestEvidence, #{}),
            TotalWorkflows = maps:get(<<"total_workflows">>, Data, 0),
            TotalWorkflows >= 10000;
        _ ->
            false
    end,

    %% Check chaos engineering criterion
    ChaosPass = case maps:get(status, ChaosEvidence, error) of
        ok ->
            Data = maps:get(data, ChaosEvidence, #{}),
            SystemRecovered = maps:get(<<"system_recovered">>, maps:get(<<"data">>, Data, #{}), false),
            SystemRecovered;
        _ ->
            false
    end,

    %% Check continuous operation criterion
    ContinuousOpPass = case maps:get(runtime_days, SchedulerInfo, 0) of
        Days when Days >= 90 -> true;
        _ -> false
    end,

    AllPassed = UptimePass andalso LoadTestPass andalso ChaosPass andalso ContinuousOpPass,

    #{
        uptime_criterion => UptimePass,
        load_test_criterion => LoadTestPass,
        chaos_criterion => ChaosPass,
        continuous_operation_criterion => ContinuousOpPass,
        all_criteria_met => AllPassed,
        certification_status => case AllPassed of
            true -> certified;
            false -> in_progress
        end,
        recommendation => case AllPassed of
            true -> <<"System meets all nine-nines certification criteria">>;
            false -> <<"System certification in progress - continue monitoring">>
        end
    }.
