%%%-------------------------------------------------------------------
%%% @doc
%%% Example: Using soc2_receipt_schema for receipt validation
%%%
%%% This example demonstrates how to validate receipts against the
%%% canonical schema before accepting them into the receipt chain.
%%%
%%% @end
%%%-------------------------------------------------------------------

%% Example 1: Validate a valid build receipt
example_valid_build() ->
    Receipt = #{
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
    },

    %% Auto-detect and validate
    {ok, ValidReceipt} = soc2_receipt_schema:validate_receipt(Receipt),
    io:format("Build receipt validated: ~p~n", [ValidReceipt]).

%% Example 2: Explicit type validation
example_explicit_type() ->
    Receipt = #{
        <<"chain">> => #{
            <<"prev_hash">> => null,
            <<"this_hash">> => <<"genesis_hash">>
        },
        <<"counts">> => #{
            <<"apps">> => 1,
            <<"modules">> => 5,
            <<"loc">> => 500,
            <<"tests">> => 2
        },
        <<"environment_fingerprint">> => #{
            <<"arch">> => <<"aarch64">>,
            <<"emulator">> => <<"16.2">>,
            <<"os">> => <<"linux">>,
            <<"otp_version">> => <<"28">>
        },
        <<"generator_version">> => <<"commit_hash">>,
        <<"ontology_hash">> => <<"ontology_hash">>,
        <<"timestamp">> => <<"2026-02-11T14:00:00Z">>,
        <<"timings">> => #{
            <<"generation_us">> => 500,
            <<"validation_us">> => 200
        }
    },

    %% Validate with explicit type
    {ok, _} = soc2_receipt_schema:validate_receipt(Receipt, build),
    io:format("Explicit type validation succeeded~n").

%% Example 3: Invalid receipt (missing field)
example_invalid_receipt() ->
    InvalidReceipt = #{
        <<"chain">> => #{
            <<"prev_hash">> => null,
            <<"this_hash">> => <<"hash">>
        }
        %% Missing counts, environment_fingerprint, etc.
    },

    case soc2_receipt_schema:validate_receipt(InvalidReceipt) of
        {ok, _} ->
            io:format("Receipt valid~n");
        {error, Errors} ->
            io:format("Validation errors: ~p~n", [Errors])
    end.

%% Example 4: Evidence receipt validation
example_evidence_receipt() ->
    Receipt = #{
        <<"chain">> => #{
            <<"prev_hash">> => <<"prev_hash_value">>,
            <<"this_hash">> => <<"this_hash_value">>
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
                <<"sha256">> => <<"abc123def456">>,
                <<"size_bytes">> => 1024
            },
            #{
                <<"path">> => <<"evidence/file2.txt">>,
                <<"sha256">> => <<"ghi789jkl012">>,
                <<"size_bytes">> => 2048
            }
        ],
        <<"generator_version">> => <<"commit_hash">>,
        <<"manifest_hash">> => <<"manifest_hash_value">>,
        <<"ontology_hash">> => <<"ontology_hash_value">>,
        <<"timestamp">> => <<"2026-02-11T14:05:00Z">>
    },

    {ok, _} = soc2_receipt_schema:validate_evidence_receipt(Receipt),
    io:format("Evidence receipt validated~n").

%% Example 5: Verdict receipt validation
example_verdict_receipt() ->
    Receipt = #{
        <<"apps_generated">> => [<<"app1">>, <<"app2">>],
        <<"chain">> => #{
            <<"prev_hash">> => <<"prev_hash">>,
            <<"this_hash">> => <<"this_hash">>
        },
        <<"environment_fingerprint">> => #{
            <<"arch">> => <<"x86_64-pc-linux-gnu">>,
            <<"emulator">> => <<"16.2">>,
            <<"os">> => <<"linux">>,
            <<"otp_version">> => <<"28">>
        },
        <<"failing_tests">> => [
            #{
                <<"test_id">> => <<"test_001">>,
                <<"reason">> => <<"Timeout after 5000ms">>
            }
        ],
        <<"generator_version">> => <<"commit_hash">>,
        <<"ontology_hash">> => <<"ontology_hash">>,
        <<"proofs_summary">> => #{
            <<"validator1">> => #{
                <<"passed">> => true,
                <<"proof">> => #{<<"evidence">> => <<"data">>}
            },
            <<"validator2">> => #{
                <<"passed">> => false,
                <<"proof">> => <<"reason">>
            }
        },
        <<"suite">> => <<"nine_nines">>,
        <<"tests_passed">> => false,
        <<"timestamp">> => <<"2026-02-11T14:10:00Z">>
    },

    {ok, _} = soc2_receipt_schema:validate_verdict_receipt(Receipt),
    io:format("Verdict receipt validated~n").

%% Example 6: Type detection
example_type_detection() ->
    BuildReceipt = #{
        <<"chain">> => #{<<"prev_hash">> => null, <<"this_hash">> => <<"h1">>},
        <<"counts">> => #{<<"apps">> => 1, <<"modules">> => 5, <<"loc">> => 100, <<"tests">> => 2},
        <<"environment_fingerprint">> => #{<<"arch">> => <<"x86">>, <<"emulator">> => <<"16.2">>, <<"os">> => <<"linux">>, <<"otp_version">> => <<"28">>},
        <<"generator_version">> => <<"hash">>,
        <<"ontology_hash">> => <<"hash">>,
        <<"timestamp">> => <<"2026-02-11T14:00:00Z">>,
        <<"timings">> => #{<<"generation_us">> => 100, <<"validation_us">> => 50}
    },

    EvidenceReceipt = #{
        <<"chain">> => #{<<"prev_hash">> => null, <<"this_hash">> => <<"h2">>},
        <<"environment_fingerprint">> => #{<<"arch">> => <<"x86">>, <<"emulator">> => <<"16.2">>, <<"os">> => <<"linux">>, <<"otp_version">> => <<"28">>},
        <<"evidence_files">> => [#{<<"path">> => <<"f1">>, <<"sha256">> => <<"hash">>, <<"size_bytes">> => 100}],
        <<"generator_version">> => <<"hash">>,
        <<"manifest_hash">> => <<"hash">>,
        <<"ontology_hash">> => <<"hash">>,
        <<"timestamp">> => <<"2026-02-11T14:00:00Z">>
    },

    VerdictReceipt = #{
        <<"apps_generated">> => [<<"app">>],
        <<"chain">> => #{<<"prev_hash">> => null, <<"this_hash">> => <<"h3">>},
        <<"environment_fingerprint">> => #{<<"arch">> => <<"x86">>, <<"emulator">> => <<"16.2">>, <<"os">> => <<"linux">>, <<"otp_version">> => <<"28">>},
        <<"failing_tests">> => [],
        <<"generator_version">> => <<"hash">>,
        <<"ontology_hash">> => <<"hash">>,
        <<"proofs_summary">> => #{<<"v1">> => #{<<"passed">> => true}},
        <<"suite">> => <<"test_suite">>,
        <<"tests_passed">> => true,
        <<"timestamp">> => <<"2026-02-11T14:00:00Z">>
    },

    build = soc2_receipt_schema:get_receipt_type(BuildReceipt),
    evidence = soc2_receipt_schema:get_receipt_type(EvidenceReceipt),
    verdict = soc2_receipt_schema:get_receipt_type(VerdictReceipt),
    io:format("Type detection working correctly~n").

%% Example 7: Integration with receipt chain
example_receipt_chain_integration() ->
    Receipt = #{
        <<"chain">> => #{
            <<"prev_hash">> => null,
            <<"this_hash">> => <<"genesis">>
        },
        <<"counts">> => #{
            <<"apps">> => 1,
            <<"modules">> => 5,
            <<"loc">> => 100,
            <<"tests">> => 2
        },
        <<"environment_fingerprint">> => #{
            <<"arch">> => <<"x86">>,
            <<"emulator">> => <<"16.2">>,
            <<"os">> => <<"linux">>,
            <<"otp_version">> => <<"28">>
        },
        <<"generator_version">> => <<"hash">>,
        <<"ontology_hash">> => <<"hash">>,
        <<"timestamp">> => <<"2026-02-11T14:00:00Z">>,
        <<"timings">> => #{
            <<"generation_us">> => 100,
            <<"validation_us">> => 50
        }
    },

    %% This will validate and return ok or {error, ...}
    case soc2_receipt_chain:append_receipt(Receipt) of
        ok ->
            io:format("Receipt appended to chain~n");
        {error, Reason} ->
            io:format("Failed to append receipt: ~p~n", [Reason])
    end.
