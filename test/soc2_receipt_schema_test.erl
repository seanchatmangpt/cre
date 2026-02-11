%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for soc2_receipt_schema validator
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_receipt_schema_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
build_receipt() ->
    #{
        <<"chain">> => #{
            <<"prev_hash">> => <<"8178ac3b9b08fe9607f82873d1f17e8b776bcdf2f454f1ec8ced55063a6f6320">>,
            <<"this_hash">> => <<"ad60b9158472ae2312cd240ad31e18acbdcb0b312e4451613356103187837de4">>
        },
        <<"counts">> => #{
            <<"apps">> => 1,
            <<"modules">> => 10,
            <<"loc">> => 1000,
            <<"tests">> => 5
        },
        <<"environment_fingerprint">> => #{
            <<"arch">> => <<"x86_64-pc-linux-gnu">>,
            <<"emulator">> => <<"16.2">>,
            <<"os">> => <<"linux">>,
            <<"otp_version">> => <<"28">>
        },
        <<"generator_version">> => <<"ab6ee990179ca9088559200b6124f3a1076c6655">>,
        <<"ontology_hash">> => <<"test123">>,
        <<"timestamp">> => <<"2026-02-11T14:03:36+00:00">>,
        <<"timings">> => #{
            <<"generation_us">> => 1000,
            <<"validation_us">> => 500
        }
    }.

evidence_receipt() ->
    #{
        <<"chain">> => #{
            <<"prev_hash">> => <<"8178ac3b9b08fe9607f82873d1f17e8b776bcdf2f454f1ec8ced55063a6f6320">>,
            <<"this_hash">> => <<"ad60b9158472ae2312cd240ad31e18acbdcb0b312e4451613356103187837de4">>
        },
        <<"environment_fingerprint">> => #{
            <<"arch">> => <<"x86_64-pc-linux-gnu">>,
            <<"emulator">> => <<"16.2">>,
            <<"os">> => <<"linux">>,
            <<"otp_version">> => <<"28">>
        },
        <<"evidence_files">> => [
            #{
                <<"path">> => <<"evidence/file1.txt">>,
                <<"sha256">> => <<"abc123">>,
                <<"size_bytes">> => 1024
            }
        ],
        <<"generator_version">> => <<"ab6ee990179ca9088559200b6124f3a1076c6655">>,
        <<"manifest_hash">> => <<"def456">>,
        <<"ontology_hash">> => <<"test123">>,
        <<"timestamp">> => <<"2026-02-11T14:03:36+00:00">>
    }.

verdict_receipt() ->
    #{
        <<"apps_generated">> => [<<"app1">>],
        <<"chain">> => #{
            <<"prev_hash">> => <<"8178ac3b9b08fe9607f82873d1f17e8b776bcdf2f454f1ec8ced55063a6f6320">>,
            <<"this_hash">> => <<"ad60b9158472ae2312cd240ad31e18acbdcb0b312e4451613356103187837de4">>
        },
        <<"environment_fingerprint">> => #{
            <<"arch">> => <<"x86_64-pc-linux-gnu">>,
            <<"emulator">> => <<"16.2">>,
            <<"os">> => <<"linux">>,
            <<"otp_version">> => <<"28">>
        },
        <<"failing_tests">> => [],
        <<"generator_version">> => <<"ab6ee990179ca9088559200b6124f3a1076c6655">>,
        <<"ontology_hash">> => <<"test123">>,
        <<"proofs_summary">> => #{
            <<"validator1">> => #{
                <<"passed">> => true,
                <<"proof">> => <<"proof_data">>
            }
        },
        <<"suite">> => <<"nine_nines">>,
        <<"tests_passed">> => true,
        <<"timestamp">> => <<"2026-02-11T14:03:36+00:00">>
    }.

%% ===================================================================
%% Tests for get_receipt_type
%% ===================================================================

get_receipt_type_build_test() ->
    Receipt = build_receipt(),
    ?assertEqual(build, soc2_receipt_schema:get_receipt_type(Receipt)).

get_receipt_type_evidence_test() ->
    Receipt = evidence_receipt(),
    ?assertEqual(evidence, soc2_receipt_schema:get_receipt_type(Receipt)).

get_receipt_type_verdict_test() ->
    Receipt = verdict_receipt(),
    ?assertEqual(verdict, soc2_receipt_schema:get_receipt_type(Receipt)).

get_receipt_type_unknown_test() ->
    ?assertEqual(unknown, soc2_receipt_schema:get_receipt_type(#{})).

%% ===================================================================
%% Tests for validate_build_receipt
%% ===================================================================

validate_build_receipt_valid_test() ->
    Receipt = build_receipt(),
    {ok, _} = soc2_receipt_schema:validate_build_receipt(Receipt).

validate_build_receipt_missing_counts_test() ->
    Receipt = maps:remove(<<"counts">>, build_receipt()),
    {error, Errors} = soc2_receipt_schema:validate_build_receipt(Receipt),
    ?assert(length(Errors) > 0).

validate_build_receipt_invalid_counts_test() ->
    Receipt = build_receipt(),
    BadReceipt = Receipt#{<<"counts">> => <<"not_a_map">>},
    {error, Errors} = soc2_receipt_schema:validate_build_receipt(BadReceipt),
    ?assert(length(Errors) > 0).

validate_build_receipt_missing_timing_test() ->
    Receipt = maps:remove(<<"timings">>, build_receipt()),
    {error, Errors} = soc2_receipt_schema:validate_build_receipt(Receipt),
    ?assert(length(Errors) > 0).

%% ===================================================================
%% Tests for validate_evidence_receipt
%% ===================================================================

validate_evidence_receipt_valid_test() ->
    Receipt = evidence_receipt(),
    {ok, _} = soc2_receipt_schema:validate_evidence_receipt(Receipt).

validate_evidence_receipt_missing_manifest_hash_test() ->
    Receipt = maps:remove(<<"manifest_hash">>, evidence_receipt()),
    {error, Errors} = soc2_receipt_schema:validate_evidence_receipt(Receipt),
    ?assert(length(Errors) > 0).

validate_evidence_receipt_invalid_files_test() ->
    Receipt = evidence_receipt(),
    BadReceipt = Receipt#{<<"evidence_files">> => <<"not_a_list">>},
    {error, Errors} = soc2_receipt_schema:validate_evidence_receipt(BadReceipt),
    ?assert(length(Errors) > 0).

validate_evidence_receipt_missing_file_hash_test() ->
    Receipt = evidence_receipt(),
    BadFiles = [
        #{
            <<"path">> => <<"evidence/file1.txt">>,
            <<"size_bytes">> => 1024
            %% Missing sha256
        }
    ],
    BadReceipt = Receipt#{<<"evidence_files">> => BadFiles},
    {error, Errors} = soc2_receipt_schema:validate_evidence_receipt(BadReceipt),
    ?assert(length(Errors) > 0).

%% ===================================================================
%% Tests for validate_verdict_receipt
%% ===================================================================

validate_verdict_receipt_valid_test() ->
    Receipt = verdict_receipt(),
    {ok, _} = soc2_receipt_schema:validate_verdict_receipt(Receipt).

validate_verdict_receipt_missing_suite_test() ->
    Receipt = maps:remove(<<"suite">>, verdict_receipt()),
    {error, Errors} = soc2_receipt_schema:validate_verdict_receipt(Receipt),
    ?assert(length(Errors) > 0).

validate_verdict_receipt_invalid_tests_passed_test() ->
    Receipt = verdict_receipt(),
    BadReceipt = Receipt#{<<"tests_passed">> => <<"not_boolean">>},
    {error, Errors} = soc2_receipt_schema:validate_verdict_receipt(BadReceipt),
    ?assert(length(Errors) > 0).

validate_verdict_receipt_invalid_failing_tests_test() ->
    Receipt = verdict_receipt(),
    BadReceipt = Receipt#{<<"failing_tests">> => <<"not_a_list">>},
    {error, Errors} = soc2_receipt_schema:validate_verdict_receipt(BadReceipt),
    ?assert(length(Errors) > 0).

validate_verdict_receipt_invalid_proofs_test() ->
    Receipt = verdict_receipt(),
    BadReceipt = Receipt#{<<"proofs_summary">> => <<"not_a_map">>},
    {error, Errors} = soc2_receipt_schema:validate_verdict_receipt(BadReceipt),
    ?assert(length(Errors) > 0).

%% ===================================================================
%% Tests for validate_receipt (auto-detection)
%% ===================================================================

validate_receipt_build_auto_test() ->
    Receipt = build_receipt(),
    {ok, _} = soc2_receipt_schema:validate_receipt(Receipt).

validate_receipt_evidence_auto_test() ->
    Receipt = evidence_receipt(),
    {ok, _} = soc2_receipt_schema:validate_receipt(Receipt).

validate_receipt_verdict_auto_test() ->
    Receipt = verdict_receipt(),
    {ok, _} = soc2_receipt_schema:validate_receipt(Receipt).

validate_receipt_explicit_type_test() ->
    Receipt = build_receipt(),
    {ok, _} = soc2_receipt_schema:validate_receipt(Receipt, build).

validate_receipt_type_mismatch_test() ->
    Receipt = build_receipt(),
    {error, _} = soc2_receipt_schema:validate_receipt(Receipt, evidence).

%% ===================================================================
%% Tests for common field validation
%% ===================================================================

validate_missing_chain_test() ->
    Receipt = maps:remove(<<"chain">>, build_receipt()),
    {error, Errors} = soc2_receipt_schema:validate_build_receipt(Receipt),
    ?assert(length(Errors) > 0).

validate_missing_timestamp_test() ->
    Receipt = maps:remove(<<"timestamp">>, build_receipt()),
    {error, Errors} = soc2_receipt_schema:validate_build_receipt(Receipt),
    ?assert(length(Errors) > 0).

validate_invalid_timestamp_format_test() ->
    Receipt = build_receipt(),
    BadReceipt = Receipt#{<<"timestamp">> => <<"not-a-timestamp">>},
    {error, Errors} = soc2_receipt_schema:validate_build_receipt(BadReceipt),
    ?assert(length(Errors) > 0).

validate_chain_null_prev_hash_test() ->
    %% Genesis block with null prev_hash is valid
    Receipt = build_receipt(),
    Chain = maps:get(<<"chain">>, Receipt),
    NewChain = Chain#{<<"prev_hash">> => null},
    GoodReceipt = Receipt#{<<"chain">> => NewChain},
    {ok, _} = soc2_receipt_schema:validate_build_receipt(GoodReceipt).

validate_environment_fingerprint_test() ->
    Receipt = maps:remove(<<"environment_fingerprint">>, build_receipt()),
    {error, Errors} = soc2_receipt_schema:validate_build_receipt(Receipt),
    ?assert(length(Errors) > 0).

%% ===================================================================
%% Tests for edge cases
%% ===================================================================

validate_empty_failing_tests_test() ->
    %% Empty failing_tests list should be valid
    Receipt = verdict_receipt(),
    {ok, _} = soc2_receipt_schema:validate_verdict_receipt(Receipt).

validate_empty_evidence_files_test() ->
    %% Empty evidence_files list is technically valid
    Receipt = evidence_receipt(),
    EmptyReceipt = Receipt#{<<"evidence_files">> => []},
    {ok, _} = soc2_receipt_schema:validate_evidence_receipt(EmptyReceipt).

validate_non_map_receipt_test() ->
    ?assertEqual(unknown, soc2_receipt_schema:get_receipt_type(<<"not_a_map">>)).
