%% -*- erlang -*-
%%%% @doc Linear Nesting Effect Receipt
%%
%% This module provides receipt generation and storage for effect execution
%% within the Linear Nesting effect system. Receipts serve as immutable
%% proofs of effect execution with cryptographic verification.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Immutable receipt records with SHA-256 spec hashing</li>
%%   <li>Scope-based receipt storage and lookup</li>
%%   <li>Result summarization for introspection</li>
%%   <li>Pure functional state management</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Creating a receipt:
%% ```erlang
%% > EffectSpec = #{module => my_effect, action => compute},
%% > ScopeId = {case_id, "12345"},
%% > Receipt = ln_receipt:new(EffectSpec, ScopeId).
%% {receipt, <<"hash...">>, ..., ...}
%% ```
%%
%% Hashing an effect spec:
%% ```erlang
%% > Hash = ln_receipt:hash(EffectSpec).
%% <<240,18,133,...>>
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_receipt).

%%====================================================================
%% Exports
%%====================================================================

%% Receipt creation
-export([new/2]).

%% Hashing
-export([hash/1]).

%% Comparison
-export([is_equal/2]).

%% Summarization
-export([summarize/1]).

%% Storage operations
-export([add/2, lookup/2]).

%%====================================================================
%% Records
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Effect specification.
%%
%% An opaque term representing an effect to be executed.
%% Typically a map with module and action keys.
%%--------------------------------------------------------------------
-type effect_spec() :: term().

%%--------------------------------------------------------------------
%% @doc Effect result.
%%
%% The result of executing an effect spec. Can be any term.
%%--------------------------------------------------------------------
-type effect_result() :: term().

%%--------------------------------------------------------------------
%% @doc Scope identifier.
%%
%% Identifies the execution scope (e.g., case ID, workflow instance).
%%--------------------------------------------------------------------
-type scope_id() :: term().

%%--------------------------------------------------------------------
%% @doc Effect receipt.
%%
%% Records the execution of an effect with cryptographic verification.
%%--------------------------------------------------------------------
-record(receipt, {
    effect_id :: reference(),
    spec_hash :: binary(),
    created_at :: integer(),
    completed_at :: integer(),
    result_summary :: term(),
    scope_id :: scope_id()
}).

%%--------------------------------------------------------------------
%% @doc Receipt storage.
%%
%% ETS-based storage for receipts indexed by scope.
%%--------------------------------------------------------------------
-record(storage, {
    table :: ets:tid(),
    scope_index :: ets:tid()
}).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Receipt handle.
%%
%% Opaque record representing a completed effect execution.
%%--------------------------------------------------------------------
-opaque receipt() :: #receipt{}.

%%--------------------------------------------------------------------
%% @doc Spec hash.
%%
%% SHA-256 hash of an effect specification.
%%--------------------------------------------------------------------
-type spec_hash() :: binary().

%%--------------------------------------------------------------------
%% @doc Storage handle.
%%
%% Opaque record for ETS-based receipt storage.
%%--------------------------------------------------------------------
-opaque storage() :: #storage{}.

%% Export types
-export_type([receipt/0, spec_hash/0, scope_id/0, storage/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new receipt for an executed effect.
%%
%% Generates a receipt with a unique effect ID, SHA-256 spec hash,
%% and timestamps. The result is summarized for compact storage.
%%
%% @param EffectSpec The effect specification that was executed
%% @param ScopeId The scope in which the effect was executed
%% @returns New receipt record
%%
%% @end
%%--------------------------------------------------------------------
-spec new(EffectSpec :: effect_spec(), ScopeId :: scope_id()) -> receipt().

new(EffectSpec, ScopeId) ->
    Now = erlang:monotonic_time(millisecond),
    EffectId = make_ref(),
    SpecHash = hash(EffectSpec),

    #receipt{
        effect_id = EffectId,
        spec_hash = SpecHash,
        created_at = Now,
        completed_at = Now,
        result_summary = undefined,
        scope_id = ScopeId
    }.

%%--------------------------------------------------------------------
%% @doc Computes the SHA-256 hash of an effect specification.
%%
%% Uses crypto:hash/2 with term_to_binary/1 for deterministic
%% hashing of any Erlang term.
%%
%% @param EffectSpec The effect specification to hash
%% @returns 32-byte SHA-256 binary hash
%%
%% @end
%%--------------------------------------------------------------------
-spec hash(EffectSpec :: effect_spec()) -> spec_hash().

hash(EffectSpec) ->
    crypto:hash(sha256, term_to_binary(EffectSpec)).

%%--------------------------------------------------------------------
%% @doc Compares two receipts for equality.
%%
%% Two receipts are equal if they have the same effect ID and spec hash.
%% This provides a strong equality check that verifies both the
%% execution instance and the effect specification match.
%%
%% @param ReceiptA First receipt to compare
%% @param ReceiptB Second receipt to compare
%% @returns true if equal, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_equal(ReceiptA :: receipt(), ReceiptB :: receipt()) -> boolean().

is_equal(#receipt{effect_id = IdA, spec_hash = HashA},
         #receipt{effect_id = IdB, spec_hash = HashB}) ->
    IdA =:= IdB andalso HashA =:= HashB.

%%--------------------------------------------------------------------
%% @doc Creates a summary of an effect result.
%%
%% Converts a potentially large result into a compact summary
%% for storage in receipts. The summary captures the essential
%% information without storing the full result.
%%
%% For simple terms (numbers, atoms, binaries), returns the term as-is.
%% For complex terms, returns a size indicator.
%%
%% @param Result The effect result to summarize
%% @returns Summarized result
%%
%% @end
%%--------------------------------------------------------------------
-spec summarize(Result :: effect_result()) -> term().

summarize(Result) when is_number(Result); is_atom(Result); is_binary(Result) ->
    Result;
summarize(Result) when is_list(Result) ->
    {list, length(Result)};
summarize(Result) when is_tuple(Result) ->
    {tuple, tuple_size(Result)};
summarize(Result) when is_map(Result) ->
    {map, maps:size(Result)};
summarize(Result) when is_pid(Result) ->
    {pid, pid_to_list(Result)};
summarize(Result) when is_reference(Result) ->
    {ref, ref_to_list(Result)};
summarize(Result) when is_port(Result) ->
    {port, erlang:port_to_list(Result)};
summarize(_Result) ->
    opaque.

%%--------------------------------------------------------------------
%% @doc Adds a receipt to storage.
%%
%% Stores the receipt in ETS tables indexed by effect ID and scope.
%% Returns the updated storage handle.
%%
%% @param Storage Current storage handle
%% @param Receipt Receipt to store
%% @returns Updated storage handle
%%
%% @end
%%--------------------------------------------------------------------
-spec add(Storage :: storage(), Receipt :: receipt()) -> storage().

add(#storage{table = Table, scope_index = ScopeIndex} = Storage,
    #receipt{effect_id = EffectId, scope_id = ScopeId} = Receipt) ->
    true = ets:insert(Table, {EffectId, Receipt}),
    true = ets:insert(ScopeIndex, {ScopeId, EffectId}),
    Storage.

%%--------------------------------------------------------------------
%% @doc Looks up receipts by scope ID.
%%
%% Returns all receipts associated with the given scope.
%% Returns an empty list if no receipts are found.
%%
%% @param Storage Storage handle to query
%% @param ScopeId Scope ID to look up
%% @returns List of receipts for the scope
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup(Storage :: storage(), ScopeId :: scope_id()) -> [receipt()].

lookup(#storage{table = Table, scope_index = ScopeIndex}, ScopeId) ->
    case ets:lookup(ScopeIndex, ScopeId) of
        [] ->
            [];
        [{ScopeId, EffectIds}] when is_list(EffectIds) ->
            [Receipt || Id <- EffectIds,
                        {Id, Receipt} <- [ets:lookup(Table, Id)],
                        Receipt =/= undefined];
        [{ScopeId, EffectId}] ->
            case ets:lookup(Table, EffectId) of
                [{EffectId, Receipt}] ->
                    [Receipt];
                [] ->
                    []
            end
    end.
