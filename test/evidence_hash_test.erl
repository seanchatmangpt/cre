%% -*- erlang -*-
%%%% @doc Unit Tests for evidence_hash Module
%%
%% Test suite for hash chain operations including:
%% - Hash chain computation
%% - Input/output hashing
%% - Hash combination
%% - Chain integrity verification

-module(evidence_hash_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test hash_chain/1 with various inputs.
%%--------------------------------------------------------------------
hash_chain_test_() ->
    {"Hash chain computes root and entries correctly",
     fun() ->
         %% Test with simple atoms
         {ok, Root1, Entries1} = evidence_hash:hash_chain([a, b, c]),
         ?assert(is_binary(Root1)),
         ?assertEqual(32, byte_size(Root1)),
         ?assertEqual(3, length(Entries1)),

         %% Test with complex maps
         Terms = [
             #{workflow => wf1, input => 1},
             #{workflow => wf2, input => 2}
         ],
         {ok, Root2, Entries2} = evidence_hash:hash_chain(Terms),
         ?assert(is_binary(Root2)),
         ?assertEqual(2, length(Entries2)),

         %% Test with empty list
         {ok, RootEmpty, EntriesEmpty} = evidence_hash:hash_chain([]),
         ?assertEqual(<<0:256>>, RootEmpty),
         ?assertEqual([], EntriesEmpty),

         %% Test with single term
         {ok, RootSingle, [EntrySingle]} = evidence_hash:hash_chain([single]),
         ?assert(is_binary(RootSingle)),
         ?assertMatch({single, _}, EntrySingle)
     end}.

%%--------------------------------------------------------------------
%% @doc Test input_hash/1 with various term types.
%%--------------------------------------------------------------------
input_hash_test_() ->
    {"Input hash produces consistent SHA-256 hashes",
     fun() ->
         %% Test with atom
         {ok, Hash1} = evidence_hash:input_hash(test_atom),
         ?assert(is_binary(Hash1)),
         ?assertEqual(32, byte_size(Hash1)),

         %% Test with map
         {ok, Hash2} = evidence_hash:input_hash(#{key => value, nested => #{inner => 1}}),
         ?assert(is_binary(Hash2)),
         ?assertEqual(32, byte_size(Hash2)),

         %% Test with list
         {ok, Hash3} = evidence_hash:input_hash([1, 2, 3, four]),
         ?assert(is_binary(Hash3)),
         ?assertEqual(32, byte_size(Hash3)),

         %% Test determinism: same input produces same hash
         {ok, Hash4} = evidence_hash:input_hash(#{determinism => test}),
         {ok, Hash5} = evidence_hash:input_hash(#{determinism => test}),
         ?assertEqual(Hash4, Hash5),

         %% Test with tuple
         {ok, Hash6} = evidence_hash:input_hash({complex, tuple, with, {nested, elements}}),
         ?assert(is_binary(Hash6)),
         ?assertEqual(32, byte_size(Hash6))
     end}.

%%--------------------------------------------------------------------
%% @doc Test output_hash/1 produces valid hashes.
%%--------------------------------------------------------------------
output_hash_test_() ->
    {"Output hash produces valid SHA-256 hashes",
     fun() ->
         %% Test with result map
         {ok, Hash1} = evidence_hash:output_hash(#{result => success, value => 42}),
         ?assert(is_binary(Hash1)),
         ?assertEqual(32, byte_size(Hash1)),

         %% Test with error term
         {ok, Hash2} = evidence_hash:output_hash({error, timeout}),
         ?assert(is_binary(Hash2)),
         ?assertEqual(32, byte_size(Hash2)),

         %% Test with binary
         {ok, Hash3} = evidence_hash:output_hash(<<"binary output">>),
         ?assert(is_binary(Hash3)),
         ?assertEqual(32, byte_size(Hash3))
     end}.

%%--------------------------------------------------------------------
%% @doc Test combine_hashes/2 produces valid parent hashes.
%%--------------------------------------------------------------------
combine_hashes_test_() ->
    {"Combine hashes produces deterministic parent hash",
     fun() ->
         %% Create two test hashes
         H1 = crypto:hash(sha256, term_to_binary(left)),
         H2 = crypto:hash(sha256, term_to_binary(right)),

         %% Combine them
         {ok, Parent} = evidence_hash:combine_hashes(H1, H2),
         ?assert(is_binary(Parent)),
         ?assertEqual(32, byte_size(Parent)),

         %% Test determinism
         {ok, Parent2} = evidence_hash:combine_hashes(H1, H2),
         ?assertEqual(Parent, Parent2),

         %% Test commutativity fails (order matters)
         {ok, ParentReversed} = evidence_hash:combine_hashes(H2, H1),
         ?assertNotEqual(Parent, ParentReversed),

         %% Test invalid hash size
         {error, invalid_hash} = evidence_hash:combine_hashes(<<1,2,3>>, H2),
         {error, invalid_hash} = evidence_hash:combine_hashes(H1, <<1,2,3>>),

         %% Test non-binary input
         {error, invalid_hash} = evidence_hash:combine_hashes(atom, H2),

         %% Test self-combination
         {ok, ParentSelf} = evidence_hash:combine_hashes(H1, H1),
         ?assert(is_binary(ParentSelf)),
         ?assertEqual(32, byte_size(ParentSelf))
     end}.

%%--------------------------------------------------------------------
%% @doc Test verify_chain/2 with valid and invalid chains.
%%--------------------------------------------------------------------
verify_chain_test_() ->
    {"Verify chain checks integrity correctly",
     fun() ->
         %% Create a valid chain
         Terms = [term1, term2, term3],
         {ok, Root, Entries} = evidence_hash:hash_chain(Terms),

         %% Verify valid chain
         ok = evidence_hash:verify_chain(Entries, Root),

         %% Test with empty chain
         ok = evidence_hash:verify_chain([], <<0:256>>),

         %% Test empty chain with wrong root
         {error, root_mismatch} = evidence_hash:verify_chain([], <<1:256>>),

         %% Test with tampered entry
         [{_T1, H1}, {_T2, H2}, {_T3, H3}] = Entries,
         TamperedEntries = [{term1, H1}, {tampered, H2}, {term3, H3}],
         {error, {hash_mismatch, _, _, _}} = evidence_hash:verify_chain(TamperedEntries, Root),

         %% Test with wrong root hash
         {error, root_mismatch} = evidence_hash:verify_chain(Entries, <<1:256>>),

         %% Test single entry chain
         {ok, SingleRoot, [SingleEntry]} = evidence_hash:hash_chain([single]),
         ok = evidence_hash:verify_chain([SingleEntry], SingleRoot),

         %% Test modified stored hash
         [{STerm, _SHash}] = [SingleEntry],
         BadEntry = {STerm, <<0:256>>},
         {error, {hash_mismatch, _, _, _}} = evidence_hash:verify_chain([BadEntry], SingleRoot)
     end}.

%%--------------------------------------------------------------------
%% @doc Test hash chain determinism properties.
%%--------------------------------------------------------------------
hash_chain_determinism_test_() ->
    {"Hash chain is deterministic across multiple computations",
     fun() ->
         Terms = [#{a => 1, b => 2}, {complex, tuple}, [list, containing, items]],

         %% Compute twice
         {ok, Root1, Entries1} = evidence_hash:hash_chain(Terms),
         {ok, Root2, Entries2} = evidence_hash:hash_chain(Terms),

         %% Roots must match
         ?assertEqual(Root1, Root2),

         %% Entry hashes must match
         ?assertEqual(length(Entries1), length(Entries2)),
         lists:foreach(fun({{_T, H1}, {_T2, H2}}) ->
             ?assertEqual(H1, H2)
         end, lists:zip(Entries1, Entries2))
     end}.

%%--------------------------------------------------------------------
%% @doc Test hash chain with odd number of entries.
%%--------------------------------------------------------------------
hash_chain_odd_entries_test_() ->
    {"Hash chain handles odd number of entries",
     fun() ->
         %% Test with 3 entries
         {ok, Root3, Entries3} = evidence_hash:hash_chain([a, b, c]),
         ?assert(is_binary(Root3)),
         ?assertEqual(3, length(Entries3)),

         %% Test with 5 entries
         {ok, Root5, Entries5} = evidence_hash:hash_chain([1, 2, 3, 4, 5]),
         ?assert(is_binary(Root5)),
         ?assertEqual(5, length(Entries5)),

         %% Test with 7 entries
         {ok, Root7, Entries7} = evidence_hash:hash_chain([one, two, three, four, five, six, seven]),
         ?assert(is_binary(Root7)),
         ?assertEqual(7, length(Entries7))
     end}.

%%--------------------------------------------------------------------
%% @doc Test hash chain avalanche effect (small input change = big output change).
%%--------------------------------------------------------------------
hash_chain_avalanche_test_() ->
    {"Hash chain exhibits avalanche effect",
     fun() ->
         Terms1 = [a, b, c, d],
         Terms2 = [a, b, x, d],  %% Changed one element

         {ok, Root1, _} = evidence_hash:hash_chain(Terms1),
         {ok, Root2, _} = evidence_hash:hash_chain(Terms2),

         %% Roots should be completely different
         ?assertNotEqual(Root1, Root2),

         %% Count bit differences (should be significant)
         <<I1:256>> = Root1,
         <<I2:256>> = Root2,
         Diff = I1 bxor I2,
         BitCount = bit_count(Diff),
         %% At least 50 bits should differ (avalanche effect)
         ?assert(BitCount > 50)
     end}.

%%--------------------------------------------------------------------
%% @doc Test input/output hash for workflow receipts.
%%--------------------------------------------------------------------
workflow_receipt_test_() ->
    {"Input and output hashes work for workflow receipts",
     fun() ->
         %% Simulate workflow input
         Input = #{
             workflow => example_wf,
             case_id => <<"case-123">>,
             marking => #{
                 place1 => 1,
                 place2 => 0
             }
         },
         {ok, InputHash} = evidence_hash:input_hash(Input),

         %% Simulate workflow output
         Output = #{
             workflow => example_wf,
             case_id => <<"case-123">>,
             result => completed,
             marking => #{
                 place1 => 0,
                 place2 => 1
             }
         },
         {ok, OutputHash} = evidence_hash:output_hash(Output),

         %% Combine into receipt hash
         {ok, ReceiptHash} = evidence_hash:combine_hashes(InputHash, OutputHash),

         %% Verify receipt hash is valid
         ?assert(is_binary(ReceiptHash)),
         ?assertEqual(32, byte_size(ReceiptHash)),

         %% Different inputs produce different hashes
         Input2 = Input#{case_id => <<"case-456">>},
         {ok, InputHash2} = evidence_hash:input_hash(Input2),
         ?assertNotEqual(InputHash, InputHash2)
     end}.

%%--------------------------------------------------------------------
%% @doc Test double-sync determinism proof.
%%--------------------------------------------------------------------
double_sync_test_() ->
    {"Double-sync determinism proof with hash chains",
     fun() ->
         %% First execution
         Exec1Input = [#{step => 1}, #{step => 2}],
         {ok, Exec1Root, Exec1Entries} = evidence_hash:hash_chain(Exec1Input),

         %% Second execution (same inputs)
         Exec2Input = [#{step => 1}, #{step => 2}],
         {ok, Exec2Root, _} = evidence_hash:hash_chain(Exec2Input),

         %% Roots must match for determinism
         ?assertEqual(Exec1Root, Exec2Root),

         %% Verify chain integrity
         ok = evidence_hash:verify_chain(Exec1Entries, Exec1Root),

         %% Different execution produces different root
         Exec3Input = [#{step => 1}, #{step => 3}],  %% Changed step 2
         {ok, Exec3Root, _} = evidence_hash:hash_chain(Exec3Input),
         ?assertNotEqual(Exec1Root, Exec3Root)
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
%% @doc Counts set bits in an integer (for avalanche test).
-spec bit_count(non_neg_integer()) -> non_neg_integer().
bit_count(0) -> 0;
bit_count(N) -> (N band 1) + bit_count(N bsr 1).
