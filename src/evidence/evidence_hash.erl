%% -*- erlang -*-
%%%% @doc Evidence Hash Chain for Determinism Verification
%%
%% This module provides hash chain infrastructure for input/output hashing
%% and double-sync determinism proof in workflow execution.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Hash chain computation from list of terms</li>
%%   <li>Input/output hashing for determinism verification</li>
%%   <li>Hash combination for Merkle-like tree structures</li>
%%   <li>Chain integrity verification</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Computing a hash chain from a list of terms:
%% ```erlang
%% > Terms = [input1, input2, input3].
%% > {ok, RootHash} = evidence_hash:hash_chain(Terms).
%% {ok, << binary >>}
%% ```
%%
%% Hashing input values for determinism verification:
%% ```erlang
%% > Input = #{workflow => wf1, marking => #{p1 => 1}}.
%% > {ok, Hash} = evidence_hash:input_hash(Input).
%% {ok, << binary >>}
%% ```
%%
%% Combining two hashes into a parent hash:
%% ```erlang
%% > H1 = <<1,2,3,...>>,
%% > H2 = <<4,5,6,...>>,
%% > {ok, Parent} = evidence_hash:combine_hashes(H1, H2).
%% {ok, << binary >>}
%% ```
%%
%% Verifying hash chain integrity:
%% ```erlang
%% > Chain = [{term1, hash1}, {term2, hash2}],
%% > {ok, Root} = evidence_hash:hash_chain([term1, term2]),
%% > ok = evidence_hash:verify_chain(Chain, Root).
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(evidence_hash).

%%====================================================================
%% Exports
%%====================================================================

%% Hash chain operations
-export([hash_chain/1, input_hash/1, output_hash/1, combine_hashes/2, verify_chain/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc SHA-256 hash binary (32 bytes).
%%--------------------------------------------------------------------
-type hash() :: <<_:256>>.

%%--------------------------------------------------------------------
%% @doc Hash chain entry: term and its computed hash.
%%--------------------------------------------------------------------
-type hash_entry() :: {term(), hash()}.

%%--------------------------------------------------------------------
%% @doc Hash chain result with root hash and all intermediate hashes.
%%--------------------------------------------------------------------
-type hash_chain_result() :: {ok, hash(), [hash_entry()]}.

%% Export types
-export_type([hash/0, hash_entry/0, hash_chain_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Computes hash chain from a list of terms.
%%
%% Returns the root hash and all intermediate hash entries.
%% The hash chain is computed by iteratively hashing each term and
%% combining hashes in a Merkle-like structure.
%%
%% @param Terms List of terms to hash
%% @returns {ok, RootHash, Entries} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec hash_chain([term()]) -> hash_chain_result().

hash_chain([]) ->
    %% Empty chain returns zero hash
    {ok, <<0:256>>, []};

hash_chain(Terms) when is_list(Terms) ->
    %% Compute hash for each term
    Entries = [{Term, hash_term(Term)} || Term <- Terms],
    %% Compute root hash from all entries
    RootHash = compute_root_hash(Entries),
    {ok, RootHash, Entries}.

%%--------------------------------------------------------------------
%% @doc Hashes input values for determinism verification.
%%
%% Input hashes are used to verify that the same inputs always produce
%% the same outputs (determinism).
%%
%% @param Input Input term (map, record, or any Erlang term)
%% @returns {ok, Hash} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec input_hash(term()) -> {ok, hash()}.

input_hash(Input) ->
    {ok, hash_term(Input)}.

%%--------------------------------------------------------------------
%% @doc Hashes output values for receipt chain.
%%
%% Output hashes are stored in the receipt chain to prove what
%% outputs were produced from given inputs.
%%
%% @param Output Output term (map, record, or any Erlang term)
%% @returns {ok, Hash} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec output_hash(term()) -> {ok, hash()}.

output_hash(Output) ->
    {ok, hash_term(Output)}.

%%--------------------------------------------------------------------
%% @doc Combines two hashes into a parent hash.
%%
%% This is used to build Merkle-tree-like structures from multiple
%% hashes. The combination is deterministic: hash(H1 ++ H2).
%%
%% @param Hash1 First hash (left child)
%% @param Hash2 Second hash (right child)
%% @returns {ok, ParentHash} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec combine_hashes(hash(), hash()) -> {ok, hash()}.

combine_hashes(Hash1, Hash2) when is_binary(Hash1), byte_size(Hash1) =:= 32,
                                   is_binary(Hash2), byte_size(Hash2) =:= 32 ->
    %% Deterministic combination: hash of concatenated hashes
    ParentHash = crypto:hash(sha256, <<Hash1/binary, Hash2/binary>>),
    {ok, ParentHash};

combine_hashes(_Hash1, _Hash2) ->
    {error, invalid_hash}.

%%--------------------------------------------------------------------
%% @doc Verifies hash chain integrity.
%%
%% Checks that all entries in the chain hash correctly and that
%% the computed root hash matches the expected root hash.
%%
%% @param Chain List of {term(), hash()} entries to verify
%% @param ExpectedRoot The expected root hash of the chain
%% @returns ok if verification succeeds, {error, Reason} otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_chain([hash_entry()], hash()) -> ok | {error, term()}.

verify_chain([], <<0:256>>) ->
    %% Empty chain with zero hash is valid
    ok;

verify_chain([], _ExpectedRoot) ->
    %% Empty chain with non-zero hash is invalid
    {error, root_mismatch};

verify_chain(Chain, ExpectedRoot) ->
    %% Verify each entry hashes correctly
    case verify_entries(Chain, []) of
        {error, Reason} ->
            {error, Reason};
        ComputedHashes ->
            %% Compute root from verified hashes
            ComputedRoot = compute_root_hash(Chain),
            case ComputedRoot of
                ExpectedRoot ->
                    ok;
                _ ->
                    {error, root_mismatch}
            end
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Computes the SHA-256 hash of an Erlang term.
-spec hash_term(term()) -> hash().

hash_term(Term) ->
    Binary = term_to_binary(Term),
    crypto:hash(sha256, Binary).

%% @private
%% @doc Computes root hash from a list of hash entries.
%% Uses pairwise combination for Merkle-like structure.
-spec compute_root_hash([hash_entry()]) -> hash().

compute_root_hash([]) ->
    <<0:256>>;

compute_root_hash([{_Term, Hash}]) ->
    %% Single entry returns its hash
    Hash;

compute_root_hash(Entries) ->
    %% Extract hashes and combine pairwise
    Hashes = [H || {_T, H} <- Entries],
    combine_hash_list(Hashes).

%% @private
%% @doc Combines a list of hashes into a single root hash.
%% Pairs up hashes and combines them iteratively.
-spec combine_hash_list([hash()]) -> hash().

combine_hash_list([Hash]) ->
    Hash;

combine_hash_list(Hashes) when length(Hashes) rem 2 =:= 0 ->
    %% Even number: pair and combine
    Paired = pair_hashes(Hashes, []),
    Combined = [combine_hashes_direct(H1, H2) || {H1, H2} <- Paired],
    combine_hash_list(Combined);

combine_hash_list(Hashes) ->
    %% Odd number: pair all but last, then append last
    [Last | Rest] = lists:reverse(Hashes),
    Paired = pair_hashes(Rest, []),
    Combined = [combine_hashes_direct(H1, H2) || {H1, H2} <- Paired],
    combine_hash_list(lists:reverse([Last | Combined])).

%% @private
%% @doc Pairs up hashes into tuples for combination.
-spec pair_hashes([hash()], [{hash(), hash()}]) -> [{hash(), hash()}].

pair_hashes([], Acc) ->
    lists:reverse(Acc);

pair_hashes([H1, H2 | Rest], Acc) ->
    pair_hashes(Rest, [{H1, H2} | Acc]);

pair_hashes([_Single], _Acc) ->
    %% Should not happen if called correctly
    error({odd_length, unexpected}).

%% @private
%% @doc Direct hash combination without {ok, ...} wrapper.
-spec combine_hashes_direct(hash(), hash()) -> hash().

combine_hashes_direct(Hash1, Hash2) ->
    crypto:hash(sha256, <<Hash1/binary, Hash2/binary>>).

%% @private
%% @doc Verifies that each entry's hash matches its term.
-spec verify_entries([hash_entry()], [hash()]) -> [hash()] | {error, term()}.

verify_entries([], Acc) ->
    lists:reverse(Acc);

verify_entries([{Term, StoredHash} | Rest], Acc) ->
    ComputedHash = hash_term(Term),
    case ComputedHash of
        StoredHash ->
            verify_entries(Rest, [ComputedHash | Acc]);
        _ ->
            {error, {hash_mismatch, Term, StoredHash, ComputedHash}}
    end.
