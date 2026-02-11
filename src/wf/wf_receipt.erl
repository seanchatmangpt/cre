%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------

-module(wf_receipt).
-moduledoc """
Causal tracking and idempotence for WF Substrate operations.

Receipts provide immutable audit records for workflow operations with
causal ordering guarantees and duplicate detection. Each receipt contains
an idempotence key that ensures at-most-once semantics for operations.

```erlang
> Key = wf_receipt:generate_idempotence_key(<<"case_123">>, 42).
<<"case_123:42">>

> R1 = wf_receipt:create(<<"case_123">>, 1, start, #{}).
> wf_receipt:is_receipt(R1).
true

> wf_receipt:case_id(R1).
<<"case_123">>

> wf_receipt:sequence_number(R1).
1

> R2 = wf_receipt:create(<<"case_123">>, 2, step_exec, #{opcode => seq_enter}).
> wf_receipt:is_causal_successor(R1, R2).
true

> Store = wf_receipt:new_store().
> {ok, Store2} = wf_receipt:add_receipt(Store, R1).
> wf_receipt:is_duplicate(Store2, R1).
true

> wf_receipt:is_duplicate(Store2, R2).
false
```

<h3>Causal Ordering</h3>

Receipts maintain causal ordering within a case through sequence numbers.
A receipt R2 is a causal successor of R1 if they share the same case_id
and R2's sequence number is greater than R1's.

<h3>Idempotence Keys</h3>

Each receipt has a unique idempotence key combining case_id and sequence
number. This key ensures that duplicate operations can be detected and
prevented, supporting exactly-once semantics at the application level.

<h3>Receipt Validation</h3>

Receipts can be validated for:
- Structural correctness (required fields present)
- Causal consistency (sequence numbers increase)
- Duplicate detection (idempotence key uniqueness)
""".

%%====================================================================
%% Exports
%%====================================================================

%% Receipt creation
-export([create/4, generate_idempotence_key/2]).

%% Receipt inspection
-export([is_receipt/1, case_id/1, sequence_number/1, operation/1,
         context/1, idempotence_key/1, timestamp/1]).

%% Causal ordering
-export([is_causal_successor/2, compare_causal/2]).

%% Receipt validation
-export([validate/1, validate_causal_sequence/1]).

%% Duplicate detection
-export([new_store/0, add_receipt/2, is_duplicate/2, find_receipt/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A receipt records the execution of a workflow operation.
%%
%% Contains the case identifier, sequence number for ordering,
%% the operation that was executed, context data, an idempotence key,
%% and a timestamp.
%%--------------------------------------------------------------------
-type receipt() :: #{case_id := case_id(),
                     seq := non_neg_integer(),
                     operation := atom(),
                     context := map(),
                     idempotence_key := binary(),
                     ts := integer()}.

%%--------------------------------------------------------------------
%% @doc Unique identifier for a workflow case.
%%
%% Binary uniquely identifying a case instance within the system.
%%--------------------------------------------------------------------
-type case_id() :: binary().

%%--------------------------------------------------------------------
%% @doc Receipt store for duplicate detection.
%%
%% Maps idempotence keys to receipts for O(1) lookup.
%%--------------------------------------------------------------------
-type receipt_store() :: #{binary() => receipt()}.

%%--------------------------------------------------------------------
%% @doc Causal ordering comparison result.
%%
%% Describes the causal relationship between two receipts.
%%--------------------------------------------------------------------
-type causal_order() :: before | after | concurrent | same_case_unordered.

-export_type([receipt/0, case_id/0, receipt_store/0, causal_order/0]).

%%====================================================================
%% Receipt Creation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new receipt for a workflow operation.
%%
%% Generates a receipt with the provided case identifier, sequence
%% number, operation type, and context. The idempotence key and
%% timestamp are automatically generated.
%%
%% @param CaseId Unique identifier for the workflow case.
%% @param SeqNum Sequence number for causal ordering (must be non-negative).
%% @param Operation Atom describing the operation performed.
%% @param Context Map containing operation-specific context data.
%% @return A complete receipt with all required fields.
%%
%% @end
%%--------------------------------------------------------------------
-spec create(CaseId :: case_id(),
             SeqNum :: non_neg_integer(),
             Operation :: atom(),
             Context :: map()) -> receipt().

create(CaseId, SeqNum, Operation, Context)
  when is_binary(CaseId), is_integer(SeqNum), SeqNum >= 0,
       is_atom(Operation), is_map(Context) ->
    IdemKey = generate_idempotence_key(CaseId, SeqNum),
    #{
      case_id => CaseId,
      seq => SeqNum,
      operation => Operation,
      context => Context,
      idempotence_key => IdemKey,
      ts => timestamp()
    }.

%%--------------------------------------------------------------------
%% @doc Generates an idempotence key from case ID and sequence number.
%%
%% Creates a unique key by combining the case identifier and sequence
%% number. This key is used for duplicate detection.
%%
%% @param CaseId The case identifier.
%% @param SeqNum The sequence number.
%% @return Binary idempotence key.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_idempotence_key(CaseId :: case_id(),
                                 SeqNum :: non_neg_integer()) -> binary().

generate_idempotence_key(CaseId, SeqNum)
  when is_binary(CaseId), is_integer(SeqNum), SeqNum >= 0 ->
    SeqBin = integer_to_binary(SeqNum),
    <<CaseId/binary, ":", SeqBin/binary>>.

%%====================================================================
%% Receipt Inspection Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid receipt.
%%
%% Validates that the term is a map containing all required fields
%% with correct types. Returns true only for structurally valid receipts.
%%
%% @param Term The term to check.
%% @return true if term is a valid receipt, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_receipt(Term :: term()) -> boolean().

is_receipt(#{case_id := CaseId, seq := SeqNum, operation := Op,
             context := Ctx, idempotence_key := IdemKey, ts := Ts})
  when is_binary(CaseId), is_integer(SeqNum), SeqNum >= 0,
       is_atom(Op), is_map(Ctx), is_binary(IdemKey), is_integer(Ts) ->
    true;
is_receipt(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Extracts the case ID from a receipt.
%%
%% @param Receipt The receipt to inspect.
%% @return The case identifier.
%%
%% @end
%%--------------------------------------------------------------------
-spec case_id(Receipt :: receipt()) -> case_id().

case_id(#{case_id := CaseId}) ->
    CaseId.

%%--------------------------------------------------------------------
%% @doc Extracts the sequence number from a receipt.
%%
%% @param Receipt The receipt to inspect.
%% @return The sequence number.
%%
%% @end
%%--------------------------------------------------------------------
-spec sequence_number(Receipt :: receipt()) -> non_neg_integer().

sequence_number(#{seq := SeqNum}) ->
    SeqNum.

%%--------------------------------------------------------------------
%% @doc Extracts the operation from a receipt.
%%
%% @param Receipt The receipt to inspect.
%% @return The operation atom.
%%
%% @end
%%--------------------------------------------------------------------
-spec operation(Receipt :: receipt()) -> atom().

operation(#{operation := Op}) ->
    Op.

%%--------------------------------------------------------------------
%% @doc Extracts the context from a receipt.
%%
%% @param Receipt The receipt to inspect.
%% @return The context map.
%%
%% @end
%%--------------------------------------------------------------------
-spec context(Receipt :: receipt()) -> map().

context(#{context := Ctx}) ->
    Ctx.

%%--------------------------------------------------------------------
%% @doc Extracts the idempotence key from a receipt.
%%
%% @param Receipt The receipt to inspect.
%% @return The idempotence key.
%%
%% @end
%%--------------------------------------------------------------------
-spec idempotence_key(Receipt :: receipt()) -> binary().

idempotence_key(#{idempotence_key := IdemKey}) ->
    IdemKey.

%%--------------------------------------------------------------------
%% @doc Extracts the timestamp from a receipt.
%%
%% @param Receipt The receipt to inspect.
%% @return The timestamp in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp(Receipt :: receipt()) -> integer().

timestamp(#{ts := Ts}) ->
    Ts.

%%====================================================================
%% Causal Ordering Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if R2 is a causal successor of R1.
%%
%% Returns true if both receipts belong to the same case and R2's
%% sequence number is greater than R1's sequence number, indicating
%% that R2 happened after R1 in the causal order.
%%
%% @param R1 The potential predecessor receipt.
%% @param R2 The potential successor receipt.
%% @return true if R2 is a causal successor of R1, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_causal_successor(R1 :: receipt(), R2 :: receipt()) -> boolean().

is_causal_successor(#{case_id := CaseId, seq := Seq1},
                     #{case_id := CaseId, seq := Seq2}) ->
    Seq2 > Seq1;
is_causal_successor(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Compares the causal ordering of two receipts.
%%
%% Determines the causal relationship between two receipts:
%% - `before`: R1 causally precedes R2 (same case, R1.seq < R2.seq)
%% - `after`: R1 causally follows R2 (same case, R1.seq > R2.seq)
%% - `concurrent`: R1 and R2 are from different cases
%% - `same_case_unordered`: R1 and R2 have the same sequence number (should not occur)
%%
%% @param R1 First receipt.
%% @param R2 Second receipt.
%% @return The causal ordering relationship.
%%
%% @end
%%--------------------------------------------------------------------
-spec compare_causal(R1 :: receipt(), R2 :: receipt()) -> causal_order().

compare_causal(#{case_id := CaseId1, seq := Seq1},
               #{case_id := CaseId2, seq := Seq2}) ->
    case CaseId1 =:= CaseId2 of
        true ->
            if
                Seq1 < Seq2 -> before;
                Seq1 > Seq2 -> after;
                true -> same_case_unordered
            end;
        false ->
            concurrent
    end.

%%====================================================================
%% Receipt Validation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a receipt for structural correctness.
%%
%% Checks that all required fields are present and have valid types.
%% Also verifies that the idempotence key matches the case_id and
%% sequence number.
%%
%% @param Receipt The receipt to validate.
%% @return ok if valid, {error, Reason} if invalid.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(Receipt :: term()) -> ok | {error, term()}.

validate(Receipt) ->
    case is_receipt(Receipt) of
        false ->
            {error, invalid_receipt_structure};
        true ->
            validate_idempotence_key(Receipt)
    end.

%%--------------------------------------------------------------------
%% @doc Validates a sequence of receipts for causal consistency.
%%
%% Checks that all receipts in the list:
%% - Belong to the same case
%% - Have strictly increasing sequence numbers
%% - Have monotonically increasing timestamps
%%
%% Returns ok if the sequence is valid, {error, Reason} otherwise.
%%
%% @param Receipts List of receipts to validate (may be empty).
%% @return ok if valid, {error, Reason} if invalid.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_causal_sequence(Receipts :: [receipt()]) -> ok | {error, term()}.

validate_causal_sequence([]) ->
    ok;
validate_causal_sequence([_Single]) ->
    ok;
validate_causal_sequence([R1, R2 | Rest]) ->
    case validate_pair(R1, R2) of
        ok ->
            validate_causal_sequence([R2 | Rest]);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Duplicate Detection Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new empty receipt store.
%%
%% @return An empty receipt store.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_store() -> receipt_store().

new_store() ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Adds a receipt to the store.
%%
%% If the receipt's idempotence key already exists in the store,
%% returns {error, duplicate}. Otherwise, adds the receipt and
%% returns {ok, UpdatedStore}.
%%
%% @param Store The receipt store.
%% @param Receipt The receipt to add.
%% @return {ok, UpdatedStore} or {error, duplicate}.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_receipt(Store :: receipt_store(), Receipt :: receipt()) ->
    {ok, receipt_store()} | {error, duplicate}.

add_receipt(Store, Receipt) when is_map(Store) ->
    IdemKey = idempotence_key(Receipt),
    case maps:is_key(IdemKey, Store) of
        true ->
            {error, duplicate};
        false ->
            {ok, Store#{IdemKey => Receipt}}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a receipt is a duplicate.
%%
%% Returns true if a receipt with the same idempotence key exists
%% in the store, false otherwise.
%%
%% @param Store The receipt store.
%% @param Receipt The receipt to check.
%% @return true if duplicate, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_duplicate(Store :: receipt_store(), Receipt :: receipt()) -> boolean().

is_duplicate(Store, Receipt) when is_map(Store) ->
    IdemKey = idempotence_key(Receipt),
    maps:is_key(IdemKey, Store).

%%--------------------------------------------------------------------
%% @doc Finds a receipt by idempotence key.
%%
%% Searches the store for a receipt with the given idempotence key.
%% Returns {ok, Receipt} if found, {error, not_found} otherwise.
%%
%% @param Store The receipt store.
%% @param IdemKey The idempotence key to search for.
%% @return {ok, Receipt} or {error, not_found}.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_receipt(Store :: receipt_store(), IdemKey :: binary()) ->
    {ok, receipt()} | {error, not_found}.

find_receipt(Store, IdemKey) when is_map(Store), is_binary(IdemKey) ->
    case maps:find(IdemKey, Store) of
        {ok, Receipt} ->
            {ok, Receipt};
        error ->
            {error, not_found}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the current timestamp in milliseconds.
%%
%% Uses erlang:monotonic_time(millisecond) for monotonic timestamps
%% suitable for ordering receipts in a sequence.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> integer().

timestamp() ->
    erlang:monotonic_time(millisecond).

%%--------------------------------------------------------------------
%% @private
%% @doc Validates that the idempotence key matches the receipt data.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_idempotence_key(Receipt :: receipt()) -> ok | {error, term()}.

validate_idempotence_key(#{case_id := CaseId, seq := SeqNum,
                            idempotence_key := IdemKey}) ->
    Expected = generate_idempotence_key(CaseId, SeqNum),
    case IdemKey =:= Expected of
        true ->
            ok;
        false ->
            {error, {idempotence_key_mismatch, Expected, IdemKey}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validates a pair of consecutive receipts.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_pair(R1 :: receipt(), R2 :: receipt()) -> ok | {error, term()}.

validate_pair(#{case_id := CaseId1}, #{case_id := CaseId2})
  when CaseId1 =/= CaseId2 ->
    {error, {case_id_mismatch, CaseId1, CaseId2}};
validate_pair(#{seq := Seq1}, #{seq := Seq2}) when Seq1 >= Seq2 ->
    {error, {sequence_not_increasing, Seq1, Seq2}};
validate_pair(#{ts := Ts1}, #{ts := Ts2}) when Ts1 > Ts2 ->
    {error, {timestamp_not_monotonic, Ts1, Ts2}};
validate_pair(_, _) ->
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% doctest examples
%%--------------------------------------------------------------------
doctest_test() ->
    %% Test generate_idempotence_key/2
    Key = generate_idempotence_key(<<"case_123">>, 42),
    ?assertEqual(<<"case_123:42">>, Key),

    %% Test create/4 and is_receipt/1
    R1 = create(<<"case_123">>, 1, start, #{}),
    ?assertEqual(true, is_receipt(R1)),

    %% Test case_id/1
    ?assertEqual(<<"case_123">>, case_id(R1)),

    %% Test sequence_number/1
    ?assertEqual(1, sequence_number(R1)),

    %% Test is_causal_successor/2
    R2 = create(<<"case_123">>, 2, step_exec, #{opcode => seq_enter}),
    ?assertEqual(true, is_causal_successor(R1, R2)),

    %% Test store operations
    Store = new_store(),
    {ok, Store2} = add_receipt(Store, R1),
    ?assertEqual(true, is_duplicate(Store2, R1)),
    ?assertEqual(false, is_duplicate(Store2, R2)),

    ok.

%%--------------------------------------------------------------------
%% create/4 tests
%%--------------------------------------------------------------------
create_test() ->
    R = create(<<"case_1">>, 0, start, #{data => test}),
    ?assertEqual(<<"case_1">>, maps:get(case_id, R)),
    ?assertEqual(0, maps:get(seq, R)),
    ?assertEqual(start, maps:get(operation, R)),
    ?assertEqual(#{data => test}, maps:get(context, R)),
    ?assert(is_binary(maps:get(idempotence_key, R))),
    ?assert(is_integer(maps:get(ts, R))).

create_with_sequence_test() ->
    R = create(<<"case_2">>, 42, step_exec, #{opcode => par_fork}),
    ?assertEqual(42, maps:get(seq, R)),
    ?assertEqual(<<"case_2:42">>, maps:get(idempotence_key, R)).

%%--------------------------------------------------------------------
%% generate_idempotence_key/2 tests
%%--------------------------------------------------------------------
generate_idempotence_key_test() ->
    ?assertEqual(<<"test:0">>, generate_idempotence_key(<<"test">>, 0)),
    ?assertEqual(<<"test:1">>, generate_idempotence_key(<<"test">>, 1)),
    ?assertEqual(<<"test:999">>, generate_idempotence_key(<<"test">>, 999)),
    ?assertEqual(<<"another:123">>, generate_idempotence_key(<<"another">>, 123)).

%%--------------------------------------------------------------------
%% is_receipt/1 tests
%%--------------------------------------------------------------------
is_receipt_valid_test() ->
    R = create(<<"test">>, 1, op, #{}),
    ?assertEqual(true, is_receipt(R)).

is_receipt_invalid_test() ->
    ?assertEqual(false, is_receipt(#{})),
    ?assertEqual(false, is_receipt(not_a_map)),
    ?assertEqual(false, is_receipt(#{case_id => <<"test">>})),
    ?assertEqual(false, is_receipt(#{case_id => <<"test">>, seq => not_integer})).

%%--------------------------------------------------------------------
%% Inspection function tests
%%--------------------------------------------------------------------
inspection_functions_test() ->
    R = create(<<"case_x">>, 5, test_op, #{key => value}),
    ?assertEqual(<<"case_x">>, case_id(R)),
    ?assertEqual(5, sequence_number(R)),
    ?assertEqual(test_op, operation(R)),
    ?assertEqual(#{key => value}, context(R)),
    ?assertEqual(<<"case_x:5">>, idempotence_key(R)),
    ?assert(is_integer(timestamp(R))).

%%--------------------------------------------------------------------
%% is_causal_successor/2 tests
%%--------------------------------------------------------------------
is_causal_successor_same_case_test() ->
    R1 = create(<<"case">>, 1, op1, #{}),
    R2 = create(<<"case">>, 2, op2, #{}),
    ?assertEqual(true, is_causal_successor(R1, R2)),
    ?assertEqual(false, is_causal_successor(R2, R1)).

is_causal_successor_different_case_test() ->
    R1 = create(<<"case1">>, 1, op, #{}),
    R2 = create(<<"case2">>, 2, op, #{}),
    ?assertEqual(false, is_causal_successor(R1, R2)).

is_causal_successor_same_seq_test() ->
    R1 = create(<<"case">>, 1, op1, #{}),
    R2 = create(<<"case">>, 1, op2, #{}),
    ?assertEqual(false, is_causal_successor(R1, R2)).

%%--------------------------------------------------------------------
%% compare_causal/2 tests
%%--------------------------------------------------------------------
compare_causal_before_test() ->
    R1 = create(<<"case">>, 1, op1, #{}),
    R2 = create(<<"case">>, 2, op2, #{}),
    ?assertEqual(before, compare_causal(R1, R2)).

compare_causal_after_test() ->
    R1 = create(<<"case">>, 2, op1, #{}),
    R2 = create(<<"case">>, 1, op2, #{}),
    ?assertEqual(after, compare_causal(R1, R2)).

compare_causal_concurrent_test() ->
    R1 = create(<<"case1">>, 1, op, #{}),
    R2 = create(<<"case2">>, 1, op, #{}),
    ?assertEqual(concurrent, compare_causal(R1, R2)).

compare_causal_unordered_test() ->
    R1 = create(<<"case">>, 1, op1, #{}),
    R2 = create(<<"case">>, 1, op2, #{}),
    ?assertEqual(same_case_unordered, compare_causal(R1, R2)).

%%--------------------------------------------------------------------
%% validate/1 tests
%%--------------------------------------------------------------------
validate_valid_receipt_test() ->
    R = create(<<"case">>, 1, op, #{}),
    ?assertEqual(ok, validate(R)).

validate_invalid_structure_test() ->
    ?assertEqual({error, invalid_receipt_structure}, validate(#{})),
    ?assertEqual({error, invalid_receipt_structure}, validate(not_a_map)).

validate_invalid_idempotence_key_test() ->
    R = create(<<"case">>, 1, op, #{}),
    BadR = R#{idempotence_key => <<"wrong_key">>},
    ?assertMatch({error, {idempotence_key_mismatch, _, _}}, validate(BadR)).

%%--------------------------------------------------------------------
%% validate_causal_sequence/1 tests
%%--------------------------------------------------------------------
validate_causal_sequence_empty_test() ->
    ?assertEqual(ok, validate_causal_sequence([])).

validate_causal_sequence_single_test() ->
    R = create(<<"case">>, 1, op, #{}),
    ?assertEqual(ok, validate_causal_sequence([R])).

validate_causal_sequence_valid_test() ->
    R1 = create(<<"case">>, 1, op1, #{}),
    timer:sleep(1), % Ensure timestamp increases
    R2 = create(<<"case">>, 2, op2, #{}),
    timer:sleep(1),
    R3 = create(<<"case">>, 3, op3, #{}),
    ?assertEqual(ok, validate_causal_sequence([R1, R2, R3])).

validate_causal_sequence_case_mismatch_test() ->
    R1 = create(<<"case1">>, 1, op, #{}),
    R2 = create(<<"case2">>, 2, op, #{}),
    ?assertMatch({error, {case_id_mismatch, _, _}},
                 validate_causal_sequence([R1, R2])).

validate_causal_sequence_not_increasing_test() ->
    R1 = create(<<"case">>, 2, op1, #{}),
    R2 = create(<<"case">>, 1, op2, #{}),
    ?assertMatch({error, {sequence_not_increasing, _, _}},
                 validate_causal_sequence([R1, R2])).

%%--------------------------------------------------------------------
%% Receipt store tests
%%--------------------------------------------------------------------
new_store_test() ->
    Store = new_store(),
    ?assertEqual(#{}, Store).

add_receipt_test() ->
    Store = new_store(),
    R = create(<<"case">>, 1, op, #{}),
    {ok, Store2} = add_receipt(Store, R),
    ?assertEqual(1, maps:size(Store2)).

add_receipt_duplicate_test() ->
    Store = new_store(),
    R = create(<<"case">>, 1, op, #{}),
    {ok, Store2} = add_receipt(Store, R),
    ?assertEqual({error, duplicate}, add_receipt(Store2, R)).

is_duplicate_test() ->
    Store = new_store(),
    R1 = create(<<"case">>, 1, op, #{}),
    R2 = create(<<"case">>, 2, op, #{}),

    ?assertEqual(false, is_duplicate(Store, R1)),

    {ok, Store2} = add_receipt(Store, R1),
    ?assertEqual(true, is_duplicate(Store2, R1)),
    ?assertEqual(false, is_duplicate(Store2, R2)).

find_receipt_test() ->
    Store = new_store(),
    R = create(<<"case">>, 1, op, #{}),
    IdemKey = idempotence_key(R),

    ?assertEqual({error, not_found}, find_receipt(Store, IdemKey)),

    {ok, Store2} = add_receipt(Store, R),
    ?assertEqual({ok, R}, find_receipt(Store2, IdemKey)).

find_receipt_not_found_test() ->
    Store = new_store(),
    ?assertEqual({error, not_found}, find_receipt(Store, <<"nonexistent">>)).

%%--------------------------------------------------------------------
%% Timestamp tests
%%--------------------------------------------------------------------
timestamp_monotonic_test() ->
    T1 = timestamp(),
    timer:sleep(1),
    T2 = timestamp(),
    ?assert(T2 >= T1).

timestamp_in_receipt_test() ->
    R1 = create(<<"case">>, 1, op, #{}),
    timer:sleep(1),
    R2 = create(<<"case">>, 2, op, #{}),
    ?assert(timestamp(R2) >= timestamp(R1)).

-endif.
