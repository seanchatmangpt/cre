%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Receipt Schema Validator
%%%
%%% Validates receipt JSON against the canonical schema defined in
%%% docs/RECEIPT_SCHEMA.md. Ensures all required fields, types, and
%%% structure conform to specification before receipts are accepted
%%% into the receipt chain.
%%%
%%% Receipt Types:
%%% 1. build.last.json    - Build metadata and timing
%%% 2. evidence.last.json - Evidence file manifest and hashes
%%% 3. verdict.last.json  - Test verdicts and proofs
%%%
%%% Joe Armstrong: "Make it correct first, then make it fast."
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_receipt_schema).

%% API
-export([
    validate_receipt/1,
    validate_receipt/2,
    validate_build_receipt/1,
    validate_evidence_receipt/1,
    validate_verdict_receipt/1,
    get_receipt_type/1
]).

-type receipt() :: map().
-type receipt_type() :: build | evidence | verdict | unknown.
-type validation_error() :: {invalid, string()} | {missing_field, string()}.
-type validation_result() :: {ok, receipt()} | {error, [validation_error()]}.

%%%===================================================================
%%% API
%%%===================================================================

%%@doc Validate a receipt and detect its type automatically.
-spec validate_receipt(receipt()) -> validation_result().
validate_receipt(Receipt) ->
    validate_receipt(Receipt, auto).

%%@doc Validate a receipt, optionally specifying the expected type.
-spec validate_receipt(receipt(), atom() | auto) -> validation_result().
validate_receipt(Receipt, ExpectedType) when is_map(Receipt) ->
    Type = get_receipt_type(Receipt),
    validate_by_type(Receipt, Type, ExpectedType).

%%@doc Validate a build receipt specifically.
-spec validate_build_receipt(receipt()) -> validation_result().
validate_build_receipt(Receipt) when is_map(Receipt) ->
    Errors = validate_build_receipt_internal(Receipt),
    case Errors of
        [] -> {ok, Receipt};
        _ -> {error, Errors}
    end.

%%@doc Validate an evidence receipt specifically.
-spec validate_evidence_receipt(receipt()) -> validation_result().
validate_evidence_receipt(Receipt) when is_map(Receipt) ->
    Errors = validate_evidence_receipt_internal(Receipt),
    case Errors of
        [] -> {ok, Receipt};
        _ -> {error, Errors}
    end.

%%@doc Validate a verdict receipt specifically.
-spec validate_verdict_receipt(receipt()) -> validation_result().
validate_verdict_receipt(Receipt) when is_map(Receipt) ->
    Errors = validate_verdict_receipt_internal(Receipt),
    case Errors of
        [] -> {ok, Receipt};
        _ -> {error, Errors}
    end.

%%@doc Determine receipt type by inspecting structure.
-spec get_receipt_type(receipt()) -> receipt_type().
get_receipt_type(Receipt) when is_map(Receipt) ->
    % Detect type by checking for discriminating fields
    HasCounts = maps:is_key(<<"counts">>, Receipt),
    HasEvidence = maps:is_key(<<"evidence_files">>, Receipt),
    HasTests = maps:is_key(<<"tests_passed">>, Receipt),
    HasSuite = maps:is_key(<<"suite">>, Receipt),

    cond_type(HasCounts, HasEvidence, HasTests, HasSuite);

get_receipt_type(_) ->
    unknown.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Determine receipt type from flags
cond_type(true, false, false, false) -> build;
cond_type(false, true, false, false) -> evidence;
cond_type(false, false, true, true) -> verdict;
cond_type(_, _, _, _) -> unknown.

%% Validate receipt by type
validate_by_type(Receipt, Type, ExpectedType) ->
    % Check type match if specified
    TypeMatch = case ExpectedType of
        auto -> true;
        Type -> true;
        _ -> false
    end,

    case TypeMatch of
        false ->
            {error, [{invalid, "Receipt type mismatch"}]};
        true ->
            case Type of
                build -> validate_build_receipt(Receipt);
                evidence -> validate_evidence_receipt(Receipt);
                verdict -> validate_verdict_receipt(Receipt);
                unknown -> {error, [{invalid, "Unable to determine receipt type"}]}
            end
    end.

%% ===================================================================
%% Build Receipt Validation (receipts/build.last.json)
%% ===================================================================

validate_build_receipt_internal(Receipt) ->
    CommonErrors = validate_common_fields(Receipt),
    BuildErrors = [
        validate_required_field(Receipt, <<"counts">>, map),
        validate_required_field(Receipt, <<"generator_version">>, binary),
        validate_required_field(Receipt, <<"ontology_hash">>, binary),
        validate_required_field(Receipt, <<"timings">>, map),
        validate_counts_structure(Receipt),
        validate_timings_structure(Receipt)
    ],
    lists:filter(fun(E) -> E =/= ok end, CommonErrors ++ BuildErrors).

validate_counts_structure(Receipt) ->
    case maps:get(<<"counts">>, Receipt, undefined) of
        undefined -> ok;
        Counts when is_map(Counts) ->
            CountsErrors = [
                validate_required_field(Counts, <<"apps">>, integer),
                validate_required_field(Counts, <<"modules">>, integer),
                validate_required_field(Counts, <<"loc">>, integer),
                validate_required_field(Counts, <<"tests">>, integer)
            ],
            lists:filter(fun(E) -> E =/= ok end, CountsErrors);
        _ ->
            {invalid, "counts must be a map"}
    end.

validate_timings_structure(Receipt) ->
    case maps:get(<<"timings">>, Receipt, undefined) of
        undefined -> ok;
        Timings when is_map(Timings) ->
            TimingsErrors = [
                validate_required_field(Timings, <<"generation_us">>, integer),
                validate_required_field(Timings, <<"validation_us">>, integer)
            ],
            lists:filter(fun(E) -> E =/= ok end, TimingsErrors);
        _ ->
            {invalid, "timings must be a map"}
    end.

%% ===================================================================
%% Evidence Receipt Validation (receipts/evidence.last.json)
%% ===================================================================

validate_evidence_receipt_internal(Receipt) ->
    CommonErrors = validate_common_fields(Receipt),
    EvidenceErrors = [
        validate_required_field(Receipt, <<"evidence_files">>, list),
        validate_required_field(Receipt, <<"manifest_hash">>, binary),
        validate_required_field(Receipt, <<"generator_version">>, binary),
        validate_required_field(Receipt, <<"ontology_hash">>, binary),
        validate_evidence_files_structure(Receipt)
    ],
    lists:filter(fun(E) -> E =/= ok end, CommonErrors ++ EvidenceErrors).

validate_evidence_files_structure(Receipt) ->
    case maps:get(<<"evidence_files">>, Receipt, undefined) of
        undefined -> ok;
        Files when is_list(Files) ->
            FileErrors = lists:filtermap(
                fun(File) ->
                    case validate_evidence_file_entry(File) of
                        ok -> false;
                        Error -> {true, Error}
                    end
                end,
                Files
            ),
            FileErrors;
        _ ->
            {invalid, "evidence_files must be a list"}
    end.

validate_evidence_file_entry(File) when is_map(File) ->
    Errors = [
        validate_required_field(File, <<"path">>, binary),
        validate_required_field(File, <<"sha256">>, binary),
        validate_required_field(File, <<"size_bytes">>, integer)
    ],
    case lists:filter(fun(E) -> E =/= ok end, Errors) of
        [] -> ok;
        _ -> {invalid, "evidence_files entry validation failed"}
    end;

validate_evidence_file_entry(_) ->
    {invalid, "evidence_files entries must be maps"}.

%% ===================================================================
%% Verdict Receipt Validation (receipts/verdict.last.json)
%% ===================================================================

validate_verdict_receipt_internal(Receipt) ->
    CommonErrors = validate_common_fields(Receipt),
    VerdictErrors = [
        validate_required_field(Receipt, <<"suite">>, binary),
        validate_required_field(Receipt, <<"tests_passed">>, boolean),
        validate_required_field(Receipt, <<"apps_generated">>, list),
        validate_required_field(Receipt, <<"failing_tests">>, list),
        validate_required_field(Receipt, <<"proofs_summary">>, map),
        validate_required_field(Receipt, <<"generator_version">>, binary),
        validate_required_field(Receipt, <<"ontology_hash">>, binary),
        validate_failing_tests_structure(Receipt),
        validate_proofs_summary_structure(Receipt)
    ],
    lists:filter(fun(E) -> E =/= ok end, CommonErrors ++ VerdictErrors).

validate_failing_tests_structure(Receipt) ->
    case maps:get(<<"failing_tests">>, Receipt, undefined) of
        undefined -> ok;
        Tests when is_list(Tests) ->
            TestErrors = lists:filtermap(
                fun(Test) ->
                    case validate_failing_test_entry(Test) of
                        ok -> false;
                        Error -> {true, Error}
                    end
                end,
                Tests
            ),
            TestErrors;
        _ ->
            {invalid, "failing_tests must be a list"}
    end.

validate_failing_test_entry(Test) when is_map(Test) ->
    Errors = [
        validate_required_field(Test, <<"test_id">>, binary),
        validate_required_field(Test, <<"reason">>, binary)
    ],
    case lists:filter(fun(E) -> E =/= ok end, Errors) of
        [] -> ok;
        _ -> {invalid, "failing_tests entry validation failed"}
    end;

validate_failing_test_entry(_) ->
    {invalid, "failing_tests entries must be maps"}.

validate_proofs_summary_structure(Receipt) ->
    case maps:get(<<"proofs_summary">>, Receipt, undefined) of
        undefined -> ok;
        Proofs when is_map(Proofs) ->
            ProofErrors = maps:fold(
                fun(_ValidatorId, ProofEntry, Acc) ->
                    case validate_proof_entry(ProofEntry) of
                        ok -> Acc;
                        Error -> [Error | Acc]
                    end
                end,
                [],
                Proofs
            ),
            ProofErrors;
        _ ->
            {invalid, "proofs_summary must be a map"}
    end.

validate_proof_entry(ProofEntry) when is_map(ProofEntry) ->
    Errors = [
        validate_required_field(ProofEntry, <<"passed">>, boolean)
        % proof field can be any JSON-serializable term, so we don't validate it
    ],
    case lists:filter(fun(E) -> E =/= ok end, Errors) of
        [] -> ok;
        _ -> {invalid, "proofs_summary entry validation failed"}
    end;

validate_proof_entry(_) ->
    {invalid, "proofs_summary entries must be maps"}.

%% ===================================================================
%% Common Field Validation (All Receipt Types)
%% ===================================================================

validate_common_fields(Receipt) ->
    [
        validate_required_field(Receipt, <<"chain">>, map),
        validate_required_field(Receipt, <<"environment_fingerprint">>, map),
        validate_required_field(Receipt, <<"timestamp">>, binary),
        validate_chain_structure(Receipt),
        validate_environment_fingerprint_structure(Receipt),
        validate_timestamp_format(Receipt)
    ].

validate_chain_structure(Receipt) ->
    case maps:get(<<"chain">>, Receipt, undefined) of
        undefined -> ok;
        Chain when is_map(Chain) ->
            ChainErrors = [
                validate_required_field(Chain, <<"this_hash">>, binary),
                validate_prev_hash_field(Chain)
            ],
            lists:filter(fun(E) -> E =/= ok end, ChainErrors);
        _ ->
            {invalid, "chain must be a map"}
    end.

validate_prev_hash_field(Chain) ->
    case maps:get(<<"prev_hash">>, Chain, undefined) of
        undefined -> {missing_field, "chain.prev_hash"};
        null -> ok;  % null is valid for genesis block
        PrevHash when is_binary(PrevHash) -> ok;
        _ -> {invalid, "chain.prev_hash must be binary or null"}
    end.

validate_environment_fingerprint_structure(Receipt) ->
    case maps:get(<<"environment_fingerprint">>, Receipt, undefined) of
        undefined -> ok;
        EnvFp when is_map(EnvFp) ->
            EnvErrors = [
                validate_required_field(EnvFp, <<"arch">>, binary),
                validate_required_field(EnvFp, <<"emulator">>, binary),
                validate_required_field(EnvFp, <<"os">>, binary),
                validate_required_field(EnvFp, <<"otp_version">>, binary)
            ],
            lists:filter(fun(E) -> E =/= ok end, EnvErrors);
        _ ->
            {invalid, "environment_fingerprint must be a map"}
    end.

validate_timestamp_format(Receipt) ->
    case maps:get(<<"timestamp">>, Receipt, undefined) of
        undefined -> ok;
        Timestamp when is_binary(Timestamp) ->
            case is_valid_iso8601(Timestamp) of
                true -> ok;
                false -> {invalid, "timestamp must be valid ISO8601 format"}
            end;
        _ ->
            {invalid, "timestamp must be binary"}
    end.

%% Simple ISO8601 validation (format: YYYY-MM-DDTHH:MM:SS+HH:MM or Z)
is_valid_iso8601(Timestamp) ->
    TsStr = binary_to_list(Timestamp),
    case length(TsStr) >= 19 of
        false -> false;
        true ->
            % Check for basic ISO8601 pattern
            case re:match(TsStr, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}") of
                {match, _} -> true;
                nomatch -> false
            end
    end.

%% ===================================================================
%% Field Validation Helpers
%% ===================================================================

%%@doc Validate that a required field exists and is of the correct type.
-spec validate_required_field(map(), binary(), atom()) -> ok | validation_error().
validate_required_field(Map, FieldName, ExpectedType) ->
    case maps:get(FieldName, Map, undefined) of
        undefined ->
            {missing_field, binary_to_list(FieldName)};
        Value ->
            validate_type(Value, ExpectedType, FieldName)
    end.

%%@doc Validate the type of a value.
-spec validate_type(term(), atom(), binary()) -> ok | validation_error().
validate_type(Value, Type, FieldName) ->
    case Type of
        binary when is_binary(Value) -> ok;
        binary -> {invalid, "Field " ++ binary_to_list(FieldName) ++ " must be binary"};
        integer when is_integer(Value) -> ok;
        integer -> {invalid, "Field " ++ binary_to_list(FieldName) ++ " must be integer"};
        boolean when is_boolean(Value) -> ok;
        boolean -> {invalid, "Field " ++ binary_to_list(FieldName) ++ " must be boolean"};
        map when is_map(Value) -> ok;
        map -> {invalid, "Field " ++ binary_to_list(FieldName) ++ " must be map"};
        list when is_list(Value) -> ok;
        list -> {invalid, "Field " ++ binary_to_list(FieldName) ++ " must be list"};
        _ -> {invalid, "Unknown type constraint: " ++ atom_to_list(Type)}
    end.
