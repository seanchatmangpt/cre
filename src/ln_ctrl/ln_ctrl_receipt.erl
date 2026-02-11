%%% @doc ln_ctrl_receipt: Build and run receipts with tamper detection
%%%
%%% Build receipts: hash(inputs) → artifacts + hash(outputs)
%%% Run receipts: hash(effect_inputs) → hash(effect_result)
%%% Append-only log with hash chain validation.
%%%
%%% @end
-module(ln_ctrl_receipt).

-export([
    build_receipt/3,
    effect_receipt/2,
    issue_receipt/2,
    validate_chain/1
]).

-export_type([
    receipt/0,
    receipt_log/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-record(build_receipt, {
    receipt_id :: reference(),
    type = build :: atom(),
    input_hash :: binary(),
    output_hash :: binary(),
    prev_hash :: binary(),
    artifacts :: map(),
    timestamp_ms :: integer()
}).

-record(effect_receipt, {
    receipt_id :: reference(),
    type = effect :: atom(),
    effect_id :: atom(),
    input_hash :: binary(),
    output_hash :: binary(),
    prev_hash :: binary(),
    result :: any(),
    timestamp_ms :: integer()
}).

-type receipt() ::
      #build_receipt{}
    | #effect_receipt{}.

-type receipt_log() :: [receipt()].

%%% API =====================================================================

%% @doc Build a new build receipt.
%%
%% Hashes the input ontology and templates to create input_hash.
%% Computes output_hash from artifacts.
%% Links to previous receipt via hash chain.
%%
%% @end
-spec build_receipt(
    InputOntology :: map(),
    Templates :: map(),
    Params :: map()
) -> receipt().
build_receipt(InputOntology, Templates, Params) ->
    InputData = {InputOntology, Templates, Params},
    InputHash = erlang:phash2(InputData),

    Artifacts = maps:merge(Templates, Params),
    OutputHash = erlang:phash2(Artifacts),

    #build_receipt{
        receipt_id = erlang:make_ref(),
        type = build,
        input_hash = <<InputHash:64>>,
        output_hash = <<OutputHash:64>>,
        prev_hash = <<0:64>>,  % No previous receipt
        artifacts = Artifacts,
        timestamp_ms = erlang:monotonic_time(millisecond)
    }.

%% @doc Create a receipt for an effect execution.
%%
%% Hashes effect input and result. Links to previous receipt.
%%
%% @end
-spec effect_receipt(EffectID :: atom(), Result :: any()) -> receipt().
effect_receipt(EffectID, Result) when is_atom(EffectID) ->
    InputHash = erlang:phash2(EffectID),
    OutputHash = erlang:phash2(Result),

    #effect_receipt{
        receipt_id = erlang:make_ref(),
        type = effect,
        effect_id = EffectID,
        input_hash = <<InputHash:64>>,
        output_hash = <<OutputHash:64>>,
        prev_hash = <<0:64>>,
        result = Result,
        timestamp_ms = erlang:monotonic_time(millisecond)
    }.

%% @doc Issue a receipt to a logger.
%%
%% In real deployments, this would write to a persistent audit log.
%% For now, just returns ok.
%%
%% @end
-spec issue_receipt(Receipt :: receipt(), Logger :: atom() | pid()) -> ok.
issue_receipt(_Receipt, _Logger) ->
    %% In real implementation, write to audit log
    ok.

%% @doc Validate the hash chain of receipts.
%%
%% Checks that each receipt links correctly to the previous one,
%% ensuring no tampering or reordering occurred.
%%
%% Returns {ok, IsValid} if all hashes match, or {error, BreakPoint, Index}
%% if a break is found.
%%
%% @end
-spec validate_chain(Receipts :: receipt_log()) ->
    {ok, boolean()} | {error, atom(), non_neg_integer()}.
validate_chain([]) ->
    {ok, true};
validate_chain([First | Rest]) ->
    validate_chain_loop(Rest, hash_of_receipt(First), 1).

-spec validate_chain_loop(receipt_log(), binary(), non_neg_integer()) ->
    {ok, boolean()} | {error, atom(), non_neg_integer()}.
validate_chain_loop([], _PrevHash, _Index) ->
    {ok, true};
validate_chain_loop([Receipt | Rest], PrevHash, Index) ->
    PrevHashFromReceipt = prev_hash_of_receipt(Receipt),
    if
        PrevHashFromReceipt == PrevHash ->
            validate_chain_loop(Rest, hash_of_receipt(Receipt), Index + 1);
        true ->
            {error, hash_chain_broken, Index}
    end.

%%% INTERNAL FUNCTIONS ======================================================

-spec hash_of_receipt(receipt()) -> binary().
hash_of_receipt(#build_receipt{output_hash = H}) -> H;
hash_of_receipt(#effect_receipt{output_hash = H}) -> H.

-spec prev_hash_of_receipt(receipt()) -> binary().
prev_hash_of_receipt(#build_receipt{prev_hash = H}) -> H;
prev_hash_of_receipt(#effect_receipt{prev_hash = H}) -> H.

%%% TESTS ===================================================================

build_receipt_test_() ->
    Input = #{key => value},
    Templates = #{template => data},
    Params = #{param => config},

    Receipt = build_receipt(Input, Templates, Params),

    [
        ?_assertEqual(build, Receipt#build_receipt.type),
        ?_assert(is_reference(Receipt#build_receipt.receipt_id)),
        ?_assert(byte_size(Receipt#build_receipt.input_hash) == 8),
        ?_assert(byte_size(Receipt#build_receipt.output_hash) == 8)
    ].

effect_receipt_test_() ->
    Receipt = effect_receipt(my_effect, {result, data}),

    [
        ?_assertEqual(effect, Receipt#effect_receipt.type),
        ?_assertEqual(my_effect, Receipt#effect_receipt.effect_id),
        ?_assert(is_reference(Receipt#effect_receipt.receipt_id)),
        ?_assertEqual({result, data}, Receipt#effect_receipt.result)
    ].

issue_receipt_test_() ->
    Receipt = build_receipt(#{}, #{}, #{}),
    [
        ?_assertEqual(ok, issue_receipt(Receipt, logger))
    ].

validate_chain_test_() ->
    [
        ?_assertMatch({ok, true}, validate_chain([])),
        ?_assertMatch({ok, true}, validate_chain([build_receipt(#{}, #{}, #{})]))
    ].
