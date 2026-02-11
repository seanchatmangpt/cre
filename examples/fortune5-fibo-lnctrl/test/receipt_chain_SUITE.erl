%%%-------------------------------------------------------------------
%%% @doc Receipt Chain Integrity Test Suite
%%% @end
%%% Verifies Block A requirements:
%%% - A1: Canonical JSON schema enforcement
%%% - A2: Receipt hash chaining (prev_hash → this_hash)
%%% - A3: Deterministic generation (same input = same hash)
%%% - A4: Chain verifier catches tampering
%%%-------------------------------------------------------------------
-module(receipt_chain_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [
        test_build_receipt_chain,
        test_evidence_manifest,
        test_verdict_generation,
        test_deterministic_hashing,
        test_chain_integrity_verification,
        test_tamper_detection
    ].

init_per_suite(Config) ->
    %% Add code paths (handle CT working directory change)
    RootDir = case filelib:is_dir("apps") of
        true -> ".";
        false -> "../../.."  % CT changes to test run directory
    end,
    EbinDir = filename:join([RootDir, "apps", "f5_receipts", "ebin"]),
    code:add_patha(EbinDir),

    %% Change back to root directory for file operations
    case filelib:is_dir("apps") of
        false -> file:set_cwd(RootDir);
        true -> ok
    end,

    %% Ensure receipts directory exists
    filelib:ensure_dir("receipts/"),

    %% Clean old receipts for fresh test
    file:delete("receipts/build.last.json"),
    file:delete("receipts/build.last.sha"),
    file:delete("receipts/evidence.last.json"),
    file:delete("receipts/verdict.last.json"),

    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_build_receipt_chain(_Config) ->
    %% Build first receipt (no previous hash)
    Receipt1 = receipt_builder:build_receipt(#{
        counts => #{apps => 10, modules => 100, loc => 10000, tests => 50},
        ontology_hash => <<"test-ontology-hash-1">>,
        timings => #{generation_us => 5000, validation_us => 1000}
    }),

    %% Verify receipt structure
    ?assertMatch(#{chain := #{prev_hash := null, this_hash := _}}, Receipt1),

    Chain1 = maps:get(chain, Receipt1),
    Hash1 = maps:get(this_hash, Chain1),
    ?assert(is_binary(Hash1)),
    ?assertEqual(64, byte_size(Hash1)),  % SHA-256 hex = 64 chars

    %% Build second receipt (should chain to first)
    Receipt2 = receipt_builder:build_receipt(#{
        counts => #{apps => 20, modules => 200, loc => 20000, tests => 100},
        ontology_hash => <<"test-ontology-hash-2">>,
        timings => #{generation_us => 6000, validation_us => 1200}
    }),

    %% Verify chain link
    Chain2 = maps:get(chain, Receipt2),
    PrevHash2 = maps:get(prev_hash, Chain2),
    Hash2 = maps:get(this_hash, Chain2),

    ?assertEqual(Hash1, PrevHash2),  % Receipt 2 prev_hash = Receipt 1 this_hash
    ?assertNotEqual(Hash1, Hash2),   % Different receipts have different hashes

    ct:pal("Receipt chain verified: ~s -> ~s", [Hash1, Hash2]),
    ok.

test_evidence_manifest(_Config) ->
    %% Create test evidence files
    filelib:ensure_dir("evidence/test/"),
    file:write_file("evidence/test/file1.txt", <<"test data 1">>),
    file:write_file("evidence/test/file2.txt", <<"test data 2">>),

    %% Collect evidence and build manifest
    Manifest = evidence_collector:collect_evidence(),

    %% Verify manifest structure
    ?assertMatch(#{
        chain := #{prev_hash := _, this_hash := _},
        evidence_files := _,
        manifest_hash := _
    }, Manifest),

    %% Verify evidence.sha256 file was created
    ?assertMatch({ok, _}, file:read_file("evidence/evidence.sha256")),

    %% Verify manifest verification passes
    ?assertEqual(ok, evidence_collector:verify_manifest()),

    %% Tamper with a file
    file:write_file("evidence/test/file1.txt", <<"tampered data">>),

    %% Verify manifest verification fails
    Result = evidence_collector:verify_manifest(),
    ?assertMatch({error, _}, Result),

    %% Restore original file
    file:write_file("evidence/test/file1.txt", <<"test data 1">>),

    %% Clean up test files
    file:delete("evidence/test/file1.txt"),
    file:delete("evidence/test/file2.txt"),
    file:del_dir("evidence/test"),

    ok.

test_verdict_generation(_Config) ->
    %% Build test verdict
    Verdict = verdict_builder:build_verdict(#{
        apps_generated => [<<"f5_app_01">>, <<"f5_app_02">>],
        failing_tests => [],
        ontology_hash => <<"test-ontology">>,
        proofs_summary => #{
            supervisor_active => true,
            deterministic_generation => true,
            hot_upgrade_capable => true
        }
    }),

    %% Verify verdict structure
    ?assertMatch(#{
        chain := #{prev_hash := _, this_hash := _},
        apps_generated := _,
        failing_tests := [],
        tests_passed := true,
        proofs_summary := _
    }, Verdict),

    %% Verify tests_passed is computed correctly
    ?assertEqual(true, maps:get(tests_passed, Verdict)),

    %% Build verdict with failures
    VerdictFail = verdict_builder:build_verdict(#{
        apps_generated => [<<"f5_app_01">>],
        failing_tests => [{<<"test_1">>, <<"reason">>}],
        ontology_hash => <<"test-ontology">>,
        proofs_summary => #{}
    }),

    ?assertEqual(false, maps:get(tests_passed, VerdictFail)),

    ok.

test_deterministic_hashing(_Config) ->
    %% Same input should produce same hash
    Params = #{
        counts => #{apps => 5, modules => 50, loc => 5000, tests => 25},
        ontology_hash => <<"determinism-test">>,
        timings => #{generation_us => 3000, validation_us => 500}
    },

    %% Build receipt without chain (test receipt_builder:hash_receipt directly)
    Receipt0 = #{
        counts => maps:get(counts, Params),
        ontology_hash => maps:get(ontology_hash, Params),
        timings => maps:get(timings, Params),
        timestamp => <<"2026-02-11T14:00:00Z">>,  % Fixed timestamp
        environment_fingerprint => #{
            arch => <<"x86_64">>,
            emulator => <<"14.2.5">>,
            os => <<"linux">>,
            otp_version => <<"28">>
        },
        generator_version => <<"test-version">>
    },

    %% Hash multiple times
    Hash1 = receipt_builder:hash_receipt(Receipt0),
    Hash2 = receipt_builder:hash_receipt(Receipt0),
    Hash3 = receipt_builder:hash_receipt(Receipt0),

    ?assertEqual(Hash1, Hash2),
    ?assertEqual(Hash2, Hash3),

    ct:pal("Deterministic hash verified: ~s", [Hash1]),

    %% Modify one field, hash should change
    Receipt1 = Receipt0#{ontology_hash := <<"different-hash">>},
    Hash4 = receipt_builder:hash_receipt(Receipt1),

    ?assertNotEqual(Hash1, Hash4),

    ok.

test_chain_integrity_verification(_Config) ->
    %% Build a valid chain
    R1 = receipt_builder:build_receipt(#{
        counts => #{apps => 1, modules => 10, loc => 1000, tests => 5},
        ontology_hash => <<"hash-1">>,
        timings => #{generation_us => 1000, validation_us => 200}
    }),

    R2 = receipt_builder:build_receipt(#{
        counts => #{apps => 2, modules => 20, loc => 2000, tests => 10},
        ontology_hash => <<"hash-2">>,
        timings => #{generation_us => 2000, validation_us => 400}
    }),

    %% Verify each receipt independently
    ?assertEqual(ok, receipt_builder:verify_receipt(R1)),
    ?assertEqual(ok, receipt_builder:verify_receipt(R2)),

    %% Verify chain link
    ?assertEqual(ok, receipt_builder:verify_chain(R1, R2)),

    %% Try to break the chain by modifying prev_hash
    R2_Tampered = R2#{
        chain := (maps:get(chain, R2))#{prev_hash := <<"fake-hash">>}
    },

    ?assertMatch({error, {chain_broken, _, _}},
                 receipt_builder:verify_chain(R1, R2_Tampered)),

    ok.

test_tamper_detection(_Config) ->
    %% Build a receipt
    Receipt = receipt_builder:build_receipt(#{
        counts => #{apps => 3, modules => 30, loc => 3000, tests => 15},
        ontology_hash => <<"tamper-test">>,
        timings => #{generation_us => 1500, validation_us => 300}
    }),

    %% Verify original receipt
    ?assertEqual(ok, receipt_builder:verify_receipt(Receipt)),

    %% Tamper with counts (but keep this_hash the same)
    TamperedReceipt = Receipt#{
        counts := #{apps => 999, modules => 9999, loc => 999999, tests => 9999}
    },

    %% Verification should fail (hash mismatch)
    ?assertMatch({error, {hash_mismatch, _, _}},
                 receipt_builder:verify_receipt(TamperedReceipt)),

    ok.
