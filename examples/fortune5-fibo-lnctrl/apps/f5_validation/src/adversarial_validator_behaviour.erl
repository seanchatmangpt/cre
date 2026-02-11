%%% @doc Adversarial Validator Behavior
%%%
%%% Formal behavior for adversarial validators that demand proof, not claims.
%%% Each validator implements this behavior to provide evidence-based validation.
%%%
%%% Callbacks:
%%% - init/0: Initialize validator state and return metadata
%%% - run_tests/1: Execute all tests and return results
%%% - format_results/1: Format results for output/storage
%%%
%%% All validators must provide cryptographic receipts of their proofs.

-module(adversarial_validator_behaviour).

%% Behavior definition
-callback init() ->
    {ok, ValidatorMeta :: map()} | {error, Reason :: term()}.

-callback run_tests(Config :: map()) ->
    {ok, Results :: [test_result()]} | {error, Reason :: term()}.

-callback format_results(Results :: [test_result()]) ->
    {ok, FormattedOutput :: map()} | {error, Reason :: term()}.

%% Types
-export_type([test_result/0, test_status/0, receipt/0]).

-type test_status() :: passed | failed | skipped.

-type test_result() :: #{
    test_id := binary(),
    test_name := binary(),
    status := test_status(),
    duration_us := non_neg_integer(),
    proof := map(),
    receipt := receipt(),
    error => term()
}.

-type receipt() :: #{
    timestamp := binary(),
    validator := binary(),
    test_id := binary(),
    proof_hash := binary(),
    metadata => map()
}.

%% API
-export([
    validate_implementation/1,
    generate_receipt/2,
    verify_receipt/1
]).

-spec validate_implementation(module()) -> ok | {error, term()}.
validate_implementation(Module) ->
    Required = [init, run_tests, format_results],
    Exports = Module:module_info(exports),

    Missing = lists:filter(fun(Callback) ->
        case Callback of
            init -> not lists:member({init, 0}, Exports);
            run_tests -> not lists:member({run_tests, 1}, Exports);
            format_results -> not lists:member({format_results, 1}, Exports)
        end
    end, Required),

    case Missing of
        [] -> ok;
        _ -> {error, {missing_callbacks, Missing}}
    end.

-spec generate_receipt(binary(), map()) -> receipt().
generate_receipt(TestId, Proof) ->
    Timestamp = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
    ProofBin = term_to_binary(Proof),
    ProofHash = crypto:hash(sha256, ProofBin),
    ProofHashHex = bin_to_hex(ProofHash),

    #{
        timestamp => Timestamp,
        validator => atom_to_binary(?MODULE),
        test_id => TestId,
        proof_hash => ProofHashHex,
        metadata => #{
            erlang_version => list_to_binary(erlang:system_info(otp_release)),
            node => atom_to_binary(node())
        }
    }.

-spec verify_receipt(receipt()) -> {ok, valid} | {error, term()}.
verify_receipt(#{timestamp := _Timestamp, proof_hash := Hash} = Receipt)
  when is_binary(Hash) ->
    %% Basic validation: receipt has required fields and valid format
    RequiredFields = [timestamp, validator, test_id, proof_hash],
    HasAllFields = lists:all(fun(Field) ->
        maps:is_key(Field, Receipt)
    end, RequiredFields),

    case HasAllFields of
        true -> {ok, valid};
        false -> {error, missing_required_fields}
    end;
verify_receipt(_) ->
    {error, invalid_receipt_format}.

%% Internal helpers
bin_to_hex(Bin) ->
    <<<<(hex_digit(N div 16)), (hex_digit(N rem 16))>> || <<N>> <= Bin>>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.
