%% Verdict Builder - Test Results with Proofs
-module(verdict_builder).

-export([
    build_verdict/1,
    verify_verdict/1
]).

-type verdict() :: #{
    apps_generated := [binary()],
    chain := #{prev_hash := binary() | null, this_hash := binary()},
    environment_fingerprint := map(),
    evidence_summary := map(),
    failing_tests := [#{test_id := binary(), reason := binary()}],
    generator_version := binary(),
    ontology_hash := binary(),
    proofs_summary := map(),
    suite := binary(),
    tests_passed := boolean(),
    timestamp := binary()
}.

%%% API

-spec build_verdict(map()) -> verdict().
build_verdict(Params) ->
    PrevHash = case file:read_file("receipts/verdict.last.json") of
        {ok, PrevJson} ->
            PrevReceipt = json:decode(PrevJson),
            case maps:get(<<"chain">>, PrevReceipt, undefined) of
                undefined -> null;  %% Old format without chain
                Chain -> maps:get(<<"this_hash">>, Chain, null)
            end;
        _ ->
            null
    end,

    %% Collect evidence summary
    EvidenceSummary = collect_evidence_summary(),

    Verdict0 = #{
        apps_generated => maps:get(apps_generated, Params, []),
        chain => #{prev_hash => PrevHash},
        environment_fingerprint => receipt_builder:get_environment_fingerprint(),
        evidence_summary => EvidenceSummary,
        failing_tests => maps:get(failing_tests, Params, []),
        generator_version => receipt_builder:get_generator_version(),
        ontology_hash => maps:get(ontology_hash, Params, <<"unknown">>),
        proofs_summary => maps:get(proofs_summary, Params, #{}),
        suite => maps:get(suite, Params, <<"nine_nines">>),
        tests_passed => length(maps:get(failing_tests, Params, [])) =:= 0,
        timestamp => receipt_builder:iso8601_now()
    },

    ThisHash = receipt_builder:hash_receipt(Verdict0),
    Verdict = Verdict0#{chain := (maps:get(chain, Verdict0))#{this_hash => ThisHash}},

    filelib:ensure_dir("receipts/"),
    CanonicalJson = receipt_builder:canonical_json(Verdict),
    file:write_file("receipts/verdict.last.json", CanonicalJson),

    Verdict.

-spec verify_verdict(verdict()) -> ok | {error, term()}.
verify_verdict(Verdict) ->
    receipt_builder:verify_receipt(Verdict).

%%% Internal Functions

-spec collect_evidence_summary() -> map().
collect_evidence_summary() ->
    %% Collect evidence files and link to them in verdict
    EvidenceFiles = #{
        uptime => "evidence/uptime/continuous_operation.json",
        load_test => "evidence/load_tests/10k_concurrent_test.json",
        chaos => "evidence/chaos/resilience_test.json",
        certification_report => "evidence/reports/certification_latest.json"
    },

    %% Check which files exist and compute their hashes
    Summary = maps:fold(fun(Type, File, Acc) ->
        case file:read_file(File) of
            {ok, Content} ->
                Hash = receipt_builder:hash_receipt(json:decode(Content)),
                Acc#{Type => #{file => list_to_binary(File), hash => Hash, status => ok}};
            {error, enoent} ->
                Acc#{Type => #{file => list_to_binary(File), status => not_yet_collected}};
            {error, Reason} ->
                Acc#{Type => #{file => list_to_binary(File), status => error, reason => Reason}}
        end
    end, #{}, EvidenceFiles),

    Summary.
