%% -*- erlang -*-
%%%% @doc wf_audit_log Integration Test - Roundtrip Receipt Verification
%%
%% Test Slice 5 of compound doctests - audit log roundtrip.
%%
%% Verifies:
%% 1. wf_audit_log:open/1 opens log
%% 2. wf_audit_log:append/2 appends receipt
%% 3. wf_audit_log:read/3 reads with cursor
%% 4. wf_audit_log:close/1 closes log
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_audit_log_integration_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function - ensures clean test environment.
%%--------------------------------------------------------------------
setup() ->
    %% Ensure no existing disk_log processes interfere
    catch disk_log:close(receipts),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function - closes any open logs.
%%--------------------------------------------------------------------
cleanup(_) ->
    catch disk_log:close(receipts),
    ok.

%%====================================================================
%% Roundtrip Test - Single Receipt
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test the complete roundtrip flow:
%% {ok, Log} = wf_audit_log:open(#{name => receipts, file => tmp}),
%% ok = wf_audit_log:append(Log, Rfire),
%% {ok, [Rfire], C1} = wf_audit_log:read(Log, 0, 10),
%% {ok, [], _C2} = wf_audit_log:read(Log, C1, 10),
%% ok = wf_audit_log:close(Log).
%%--------------------------------------------------------------------
roundtrip_single_receipt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
         %% Step 1: Open log with temp file
         {ok, Log} = wf_audit_log:open(#{name => receipts, file => tmp}),

         %% Step 2: Create a receipt (simulating a transition firing)
         BeforeHash = crypto:hash(md5, term_to_binary({marking, 'before'})),
         AfterHash = crypto:hash(md5, term_to_binary({marking, 'after'})),
         Rfire = #{
             before_hash => BeforeHash,
             after_hash => AfterHash,
             move => #{
                 trsn => fire,
                 mode => #{},
                 produce => #{output => [token]}
             },
             ts => erlang:unique_integer([positive, monotonic])
         },

         %% Step 3: Append the receipt
         ok = wf_audit_log:append(Log, Rfire),

         %% Step 4: Read back with cursor pagination
         {ok, [Rread], C1} = wf_audit_log:read(Log, 0, 10),

         %% Step 5: Verify receipts match (before_hash, after_hash match)
         ?assertEqual(maps:get(before_hash, Rfire),
                      maps:get(before_hash, Rread)),
         ?assertEqual(maps:get(after_hash, Rfire),
                      maps:get(after_hash, Rread)),
         ?assertEqual(maps:get(move, Rfire),
                      maps:get(move, Rread)),

         %% Step 6: Verify cursor advanced
         ?assertEqual(1, C1),

         %% Step 7: Read past end returns empty list
         {ok, [], _C2} = wf_audit_log:read(Log, C1, 10),

         %% Step 8: Close log
         ok = wf_audit_log:close(Log)
     end}.

%%====================================================================
%% Roundtrip Test - Multiple Receipts
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test roundtrip with multiple receipts in sequence.
%% Verifies that all receipts are stored and retrieved correctly.
%%--------------------------------------------------------------------
roundtrip_multiple_receipts_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
         {ok, Log} = wf_audit_log:open(#{name => multi_roundtrip, file => tmp}),

         %% Create and append three receipts
         R1 = make_receipt(t1, <<"hash_b1">>, <<"hash_a1">>),
         R2 = make_receipt(t2, <<"hash_b2">>, <<"hash_a2">>),
         R3 = make_receipt(t3, <<"hash_b3">>, <<"hash_a3">>),

         ok = wf_audit_log:append(Log, R1),
         ok = wf_audit_log:append(Log, R2),
         ok = wf_audit_log:append(Log, R3),

         %% Read all at once
         {ok, [RR1, RR2, RR3], C} = wf_audit_log:read(Log, 0, 10),

         %% Verify all receipts match
         verify_receipt_match(R1, RR1),
         verify_receipt_match(R2, RR2),
         verify_receipt_match(R3, RR3),

         %% Cursor at end
         ?assertEqual(3, C),

         %% Read past end is empty
         {ok, [], _} = wf_audit_log:read(Log, C, 10),

         ok = wf_audit_log:close(Log)
     end}.

%%====================================================================
%% Cursor Pagination Test
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test cursor-based pagination with multiple reads.
%%--------------------------------------------------------------------
cursor_pagination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
         {ok, Log} = wf_audit_log:open(#{name => cursor_test, file => tmp}),

         %% Append 5 receipts
         Receipts = [make_receipt(list_to_atom("t" ++ integer_to_list(I)),
                                   <<"b">>, <<"a">>) || I <- lists:seq(1, 5)],
         lists:foreach(fun(R) -> wf_audit_log:append(Log, R) end, Receipts),

         %% Read in pages of 2
         {ok, [R1], C1} = wf_audit_log:read(Log, 0, 1),
         verify_receipt_match(hd(Receipts), R1),
         ?assertEqual(1, C1),

         {ok, [R2], C2} = wf_audit_log:read(Log, C1, 1),
         verify_receipt_match(lists:nth(2, Receipts), R2),
         ?assertEqual(2, C2),

         {ok, [R3, R4], C4} = wf_audit_log:read(Log, C2, 2),
         verify_receipt_match(lists:nth(3, Receipts), R3),
         verify_receipt_match(lists:nth(4, Receipts), R4),
         ?assertEqual(4, C4),

         {ok, [R5], C5} = wf_audit_log:read(Log, C4, 10),
         verify_receipt_match(lists:nth(5, Receipts), R5),
         ?assertEqual(5, C5),

         %% Past end
         {ok, [], _} = wf_audit_log:read(Log, C5, 10),

         ok = wf_audit_log:close(Log)
     end}.

%%====================================================================
%% Receipt Hash Match Test
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that before_hash and after_hash match exactly on roundtrip.
%% This is critical for audit trail integrity.
%%--------------------------------------------------------------------
hash_integrity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
         {ok, Log} = wf_audit_log:open(#{name => hash_test, file => tmp}),

         %% Create receipt with real crypto hashes
         BeforeMarking = #{p1 => [a, b], p2 => [c]},
         AfterMarking = #{p1 => [a], p3 => [d]},

         BeforeHash = crypto:hash(sha256, term_to_binary(BeforeMarking)),
         AfterHash = crypto:hash(sha256, term_to_binary(AfterMarking)),

         Receipt = #{
             before_hash => BeforeHash,
             after_hash => AfterHash,
             move => #{trsn => transition, mode => #{consume => #{p1 => 1}}, produce => #{}},
             ts => 12345
         },

         ok = wf_audit_log:append(Log, Receipt),

         {ok, [ReadReceipt], _} = wf_audit_log:read(Log, 0, 10),

         %% Hashes must match exactly for audit integrity
         ?assertEqual(BeforeHash, maps:get(before_hash, ReadReceipt)),
         ?assertEqual(AfterHash, maps:get(after_hash, ReadReceipt)),
         ?assertEqual(12345, maps:get(ts, ReadReceipt)),

         ok = wf_audit_log:close(Log)
     end}.

%%====================================================================
%% Empty Log Test
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test reading from an empty log returns empty list.
%%--------------------------------------------------------------------
empty_log_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
         {ok, Log} = wf_audit_log:open(#{name => empty_test, file => tmp}),

         %% Read from empty log
         {ok, [], C0} = wf_audit_log:read(Log, 0, 10),
         ?assertEqual(0, C0),

         ok = wf_audit_log:close(Log)
     end}.

%%====================================================================
%% Reopen Existing Log Test
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test closing and reopening a log preserves data.
%%--------------------------------------------------------------------
reopen_preserves_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
         LogName = reopen_test,

         %% Write initial data
         {ok, Log1} = wf_audit_log:open(#{name => LogName, file => "/tmp/reopen_test.log"}),
         R1 = make_receipt(t_write, <<"write_before">>, <<"write_after">>),
         ok = wf_audit_log:append(Log1, R1),
         ok = wf_audit_log:close(Log1),

         %% Reopen and verify
         {ok, Log2} = wf_audit_log:open(#{name => LogName, file => "/tmp/reopen_test.log"}),
         {ok, [Rread], _} = wf_audit_log:read(Log2, 0, 10),
         verify_receipt_match(R1, Rread),
         ok = wf_audit_log:close(Log2),

         %% Clean up
         file:delete("/tmp/reopen_test.log")
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a test receipt with given transition and hashes.
%%--------------------------------------------------------------------
make_receipt(Trsn, BeforeHash, AfterHash) ->
    #{
        before_hash => BeforeHash,
        after_hash => AfterHash,
        move => #{
            trsn => Trsn,
            mode => #{},
            produce => #{output => [token]}
        },
        ts => erlang:unique_integer([positive, monotonic])
    }.

%%--------------------------------------------------------------------
%% @doc Verifies that two receipts match on critical fields.
%%--------------------------------------------------------------------
verify_receipt_match(Expected, Actual) ->
    ?assertEqual(maps:get(before_hash, Expected),
                 maps:get(before_hash, Actual)),
    ?assertEqual(maps:get(after_hash, Expected),
                 maps:get(after_hash, Actual)),
    ?assertEqual(maps:get(move, Expected),
                 maps:get(move, Actual)).
