#!/usr/bin/env escript
%%! -sname external_verification

%% External Verification Services for Compliance Receipts
%% Simulates third-party verification with feature flags
%%
%% Feature flags:
%% - timestamp_authority: External TSA verification
%% - witness_server: Independent witness verification
%% - third_party_monitor: External monitoring service
%% - blockchain_anchor: Blockchain anchoring (simulated)

-mode(compile).

-record(verification_result, {
    service :: atom(),
    status :: pass | fail,
    timestamp :: binary(),
    signature :: binary(),
    metadata :: map()
}).

main([]) ->
    io:format("Usage: external_verification.erl <receipt_file> [flags]~n"),
    io:format("Flags: --tsa --witness --monitor --blockchain~n"),
    halt(1);
main(Args) ->
    [ReceiptFile | FlagArgs] = Args,

    %% Parse feature flags
    Flags = parse_flags(FlagArgs),

    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║         External Verification Services (Simulated)        ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n~n"),

    %% Load receipt
    case file:read_file(ReceiptFile) of
        {ok, ReceiptJson} ->
            code:ensure_loaded(json),
            Receipt = json:decode(ReceiptJson),
            ReceiptHash = maps:get(<<"receipt_hash">>, Receipt),

            io:format("Receipt: ~s~n", [ReceiptFile]),
            io:format("Hash: ~s~n~n", [ReceiptHash]),

            %% Run verifications based on flags
            Results = run_verifications(ReceiptHash, Receipt, Flags),

            %% Display results
            display_verification_results(Results),

            %% Generate verification attestation
            Attestation = generate_attestation(Receipt, Results),

            %% Write attestation file
            AttestationFile = ReceiptFile ++ ".attestation.json",
            file:write_file(AttestationFile, Attestation),

            io:format("~nAttestation written to: ~s~n", [AttestationFile]),

            %% Exit with status
            AllPassed = lists:all(fun(#verification_result{status = S}) -> S =:= pass end, Results),
            case AllPassed of
                true -> halt(0);
                false -> halt(1)
            end;

        {error, Reason} ->
            io:format("Error reading receipt: ~p~n", [Reason]),
            halt(1)
    end.

parse_flags(Args) ->
    lists:foldl(fun
        ("--tsa", Acc) -> Acc#{timestamp_authority => true};
        ("--witness", Acc) -> Acc#{witness_server => true};
        ("--monitor", Acc) -> Acc#{third_party_monitor => true};
        ("--blockchain", Acc) -> Acc#{blockchain_anchor => true};
        ("--all", Acc) -> Acc#{
            timestamp_authority => true,
            witness_server => true,
            third_party_monitor => true,
            blockchain_anchor => true
        };
        (_, Acc) -> Acc
    end, #{}, Args).

run_verifications(ReceiptHash, Receipt, Flags) ->
    io:format("Running external verifications...~n~n"),

    Results = [],

    %% Timestamp Authority
    Results1 = case maps:get(timestamp_authority, Flags, false) of
        true -> [verify_timestamp_authority(ReceiptHash, Receipt) | Results];
        false -> Results
    end,

    %% Witness Server
    Results2 = case maps:get(witness_server, Flags, false) of
        true -> [verify_witness_server(ReceiptHash, Receipt) | Results1];
        false -> Results1
    end,

    %% Third-Party Monitor
    Results3 = case maps:get(third_party_monitor, Flags, false) of
        true -> [verify_third_party_monitor(ReceiptHash, Receipt) | Results2];
        false -> Results2
    end,

    %% Blockchain Anchor
    Results4 = case maps:get(blockchain_anchor, Flags, false) of
        true -> [verify_blockchain_anchor(ReceiptHash, Receipt) | Results3];
        false -> Results3
    end,

    case Results4 of
        [] ->
            io:format("⚠️  No verification flags specified~n~n"),
            [];
        _ ->
            Results4
    end.

%% Simulate Timestamp Authority verification
verify_timestamp_authority(ReceiptHash, Receipt) ->
    io:format("[ 1/4] Timestamp Authority (RFC 3161)....... "),

    %% Simulate TSA request/response
    timer:sleep(50),  % Simulate network latency

    Timestamp = maps:get(<<"timestamp">>, Receipt),

    %% Generate TSA signature (simulated)
    TsaData = <<ReceiptHash/binary, Timestamp/binary>>,
    TsaSignature = crypto:hash(sha256, TsaData),
    TsaSignatureHex = list_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= TsaSignature]),

    io:format("✓ VERIFIED~n"),
    io:format("         TSA: DigiCert Timestamp Authority~n"),
    io:format("         Time: ~s~n", [Timestamp]),
    io:format("         Signature: ~s...~n~n", [binary:part(TsaSignatureHex, 0, 16)]),

    #verification_result{
        service = timestamp_authority,
        status = pass,
        timestamp = Timestamp,
        signature = TsaSignatureHex,
        metadata = #{
            authority => <<"DigiCert TSA (Simulated)">>,
            protocol => <<"RFC 3161">>,
            algorithm => <<"SHA-256">>
        }
    }.

%% Simulate Witness Server verification
verify_witness_server(ReceiptHash, Receipt) ->
    io:format("[ 2/4] Independent Witness Server........... "),

    %% Simulate witness verification
    timer:sleep(50),

    Timestamp = maps:get(<<"timestamp">>, Receipt),

    %% Generate witness attestation
    WitnessData = <<ReceiptHash/binary, <<"witness">>/binary>>,
    WitnessSignature = crypto:hash(sha256, WitnessData),
    WitnessSignatureHex = list_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= WitnessSignature]),

    io:format("✓ VERIFIED~n"),
    io:format("         Witness: witness.compliance.cloud~n"),
    io:format("         Recorded: ~s~n", [Timestamp]),
    io:format("         Attestation: ~s...~n~n", [binary:part(WitnessSignatureHex, 0, 16)]),

    #verification_result{
        service = witness_server,
        status = pass,
        timestamp = Timestamp,
        signature = WitnessSignatureHex,
        metadata = #{
            witness => <<"witness.compliance.cloud (Simulated)">>,
            location => <<"us-central1">>,
            availability => <<"99.99%">>
        }
    }.

%% Simulate Third-Party Monitoring verification
verify_third_party_monitor(ReceiptHash, Receipt) ->
    io:format("[ 3/4] Third-Party Monitoring Service....... "),

    %% Simulate monitoring service check
    timer:sleep(50),

    Timestamp = maps:get(<<"timestamp">>, Receipt),
    Compliance = maps:get(<<"compliance_percent">>, maps:get(<<"validation">>, Receipt)),

    %% Generate monitoring signature
    MonitorData = <<ReceiptHash/binary, <<"datadog">>/binary>>,
    MonitorSignature = crypto:hash(sha256, MonitorData),
    MonitorSignatureHex = list_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= MonitorSignature]),

    io:format("✓ VERIFIED~n"),
    io:format("         Monitor: Datadog Compliance SLO~n"),
    io:format("         Compliance: ~.4f%~n", [Compliance]),
    io:format("         Verification: ~s...~n~n", [binary:part(MonitorSignatureHex, 0, 16)]),

    #verification_result{
        service = third_party_monitor,
        status = pass,
        timestamp = Timestamp,
        signature = MonitorSignatureHex,
        metadata = #{
            service => <<"Datadog (Simulated)">>,
            compliance_slo => Compliance,
            uptime_verified => true
        }
    }.

%% Simulate Blockchain Anchoring verification
verify_blockchain_anchor(ReceiptHash, Receipt) ->
    io:format("[ 4/4] Blockchain Anchor (Ethereum)......... "),

    %% Simulate blockchain transaction
    timer:sleep(100),  % Longer latency for blockchain

    Timestamp = maps:get(<<"timestamp">>, Receipt),

    %% Generate transaction hash
    TxData = <<ReceiptHash/binary, <<"eth">>/binary>>,
    TxHash = crypto:hash(sha256, TxData),
    TxHashHex = list_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= TxHash]),

    %% Simulate block number
    BlockNumber = 18500000 + rand:uniform(1000),

    io:format("✓ ANCHORED~n"),
    io:format("         Network: Ethereum Mainnet~n"),
    io:format("         Block: ~p~n", [BlockNumber]),
    io:format("         Tx: 0x~s...~n~n", [binary:part(TxHashHex, 0, 16)]),

    #verification_result{
        service = blockchain_anchor,
        status = pass,
        timestamp = Timestamp,
        signature = TxHashHex,
        metadata = #{
            network => <<"Ethereum Mainnet (Simulated)">>,
            block_number => BlockNumber,
            confirmations => 12,
            gas_used => 21000
        }
    }.

display_verification_results(Results) ->
    io:format("═══════════════════════════════════════════════════════════════~n"),
    io:format("  EXTERNAL VERIFICATION SUMMARY~n"),
    io:format("═══════════════════════════════════════════════════════════════~n~n"),

    PassCount = length([R || R <- Results, R#verification_result.status =:= pass]),
    TotalCount = length(Results),

    lists:foreach(fun(#verification_result{service = Svc, status = Status}) ->
        case Status of
            pass -> io:format("  [~p] ✓ VERIFIED~n", [Svc]);
            fail -> io:format("  [~p] ✗ FAILED~n", [Svc])
        end
    end, Results),

    io:format("~n  Total: ~p/~p verified~n", [PassCount, TotalCount]).

generate_attestation(Receipt, Results) ->
    Timestamp = calendar:universal_time(),

    Attestation = #{
        <<"attestation_version">> => <<"1.0">>,
        <<"timestamp">> => list_to_binary(format_datetime(Timestamp)),
        <<"original_receipt_hash">> => maps:get(<<"receipt_hash">>, Receipt),
        <<"verifications">> => [
            #{
                <<"service">> => atom_to_binary(Svc, utf8),
                <<"status">> => atom_to_binary(Status, utf8),
                <<"timestamp">> => Ts,
                <<"signature">> => Sig,
                <<"metadata">> => Meta
            }
            || #verification_result{
                service = Svc,
                status = Status,
                timestamp = Ts,
                signature = Sig,
                metadata = Meta
            } <- Results
        ],
        <<"attestation_signature">> => generate_attestation_signature(Receipt, Results)
    },

    code:ensure_loaded(json),
    json:encode(Attestation).

generate_attestation_signature(Receipt, Results) ->
    ReceiptHash = maps:get(<<"receipt_hash">>, Receipt),

    %% Combine all verification signatures
    AllSigs = [Sig || #verification_result{signature = Sig} <- Results],
    CombinedData = iolist_to_binary([ReceiptHash | AllSigs]),

    %% Generate final attestation signature
    AttestationHash = crypto:hash(sha256, CombinedData),
    list_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= AttestationHash]).

format_datetime({{Y,M,D},{H,Min,S}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                                [Y,M,D,H,Min,S])).
