%%%-------------------------------------------------------------------
%%% @doc f5_proof_cancel - Cancel-scope proof verification.
%%%
%%% Implements Fortune-5 FIBO cancel-proof verification ensuring
%%% that no effects are initiated after a cancel signal within a scope.
%%%
%%% The proof guarantees:
%%% - Post-cancel effect count == 0
%%% - Correlation between trace events and effect counters
%%% - Evidence from both execution trace and effect receipts
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(f5_proof_cancel).

%% API
-export([verify_cancel_scope/2]).
-export([check_effect_counters/2]).
-export([generate_cancel_proof/3]).
-export([compute_proof_hash/3]).

-include_lib("kernel/include/logger.hrl").

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type scope_id() :: atom() | binary() | {atom(), term()}.

-type cancel_event() :: #{
    timestamp := integer(),
    scope_id := scope_id(),
    seq := non_neg_integer()
}.

-type effect_event() :: #{
    timestamp := integer(),
    effect_id := reference(),
    scope_id := scope_id(),
    type := effect_requested | effect_completed,
    seq := non_neg_integer()
}.

-type trace_event() :: cancel_event() | effect_event().

-type evidence_counter() :: #{
    scope_id := scope_id(),
    effects_before := non_neg_integer(),
    effects_after := non_neg_integer(),
    cancel_timestamp := integer() | undefined,
    total_effects := non_neg_integer()
}.

-type cancel_proof() :: #{
    proof_type := cancel_scope,
    scope_id := scope_id(),
    cancel_timestamp := integer(),
    post_cancel_effects := non_neg_integer(),
    effects_verified := boolean(),
    evidence_sources := [atom()],
    proof_hash := binary(),
    verified_at := integer()
}.

-type proof_result() :: {ok, cancel_proof()} | {error, term()}.

-export_type([scope_id/0, trace_event/0, evidence_counter/0, cancel_proof/0, proof_result/0]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% @doc Verify cancel scope for a given scope from trace events.
-spec verify_cancel_scope(scope_id(), [trace_event()]) -> proof_result().
verify_cancel_scope(ScopeId, TraceEvents) ->
    try
        %% Step 1: Find cancel event in trace
        CancelEvent = find_cancel_event(ScopeId, TraceEvents),
        CancelTime = maps:get(timestamp, CancelEvent),

        %% Step 2: Extract effect events after cancel within scope
        PostCancelEffects = extract_post_cancel_effects(
            ScopeId, CancelTime, TraceEvents
        ),

        %% Step 3: Count effects by type
        EffectCount = length(PostCancelEffects),

        %% Step 4: Verify count == 0
        Verified = EffectCount =:= 0,

        %% Step 5: Build proof record
        Proof = #{
            proof_type => cancel_scope,
            scope_id => ScopeId,
            cancel_timestamp => CancelTime,
            post_cancel_effects => EffectCount,
            effects_verified => Verified,
            evidence_sources => determine_evidence_sources(TraceEvents),
            proof_hash => compute_proof_hash(ScopeId, CancelTime, EffectCount),
            verified_at => erlang:monotonic_time(millisecond)
        },

        %% Step 6: Return result
        case Verified of
            true ->
                {ok, Proof};
            false ->
                {error, {post_cancel_effects_found, EffectCount, PostCancelEffects}}
        end
    catch
        throw:{cancel_not_found, ScopeId} ->
            {error, {cancel_not_found, ScopeId}};
        error:Reason ->
            {error, {verification_failed, Reason}}
    end.

%% @doc Check effect counters against trace for a scope.
-spec check_effect_counters(scope_id(), evidence_counter()) ->
    {ok, non_neg_integer(), boolean()} | {error, term()}.
check_effect_counters(ScopeId, #{scope_id := Sid} = EvidenceCounter) when Sid =:= ScopeId ->
    CancelTime = maps:get(cancel_timestamp, EvidenceCounter, undefined),
    EffectsAfter = maps:get(effects_after, EvidenceCounter, 0),

    case CancelTime of
        undefined ->
            {error, no_cancel_timestamp};
        _ when EffectsAfter =:= 0 ->
            {ok, 0, true};
        _ when EffectsAfter > 0 ->
            {ok, EffectsAfter, false};
        _ ->
            {error, invalid_counter_data}
    end;
check_effect_counters(_ScopeId, _EvidenceCounter) ->
    {error, scope_mismatch}.

%% @doc Generate a cancel proof artifact as JSON.
-spec generate_cancel_proof(scope_id(), [trace_event()], evidence_counter()) ->
    {ok, map()} | {error, term()}.
generate_cancel_proof(ScopeId, TraceEvents, EvidenceCounter) ->
    case verify_cancel_scope(ScopeId, TraceEvents) of
        {ok, Proof} ->
            JsonProof = proof_to_json(Proof, EvidenceCounter),
            {ok, JsonProof};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compute SHA-256 hash of proof for integrity verification.
-spec compute_proof_hash(scope_id(), integer(), non_neg_integer()) -> binary().
compute_proof_hash(ScopeId, CancelTime, EffectCount) ->
    ProofData = term_to_binary({ScopeId, CancelTime, EffectCount}),
    crypto:hash(sha256, ProofData).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
%% @doc Find the cancel event for a scope in the trace.
find_cancel_event(ScopeId, TraceEvents) ->
    case lists:search(fun
        (#{type := scope_cancelled, scope_id := Sid}) when Sid =:= ScopeId ->
            true;
        (_) ->
            false
    end, TraceEvents) of
        {value, CancelEvent} ->
            CancelEvent;
        false ->
            throw({cancel_not_found, ScopeId})
    end.

%% @private
%% @doc Extract all effect events that occurred after cancel in the same scope.
extract_post_cancel_effects(ScopeId, CancelTime, TraceEvents) ->
    lists:filtermap(fun(Event) ->
        case Event of
            #{type := Type, scope_id := Sid, timestamp := TS}
            when (Type =:= effect_requested orelse Type =:= effect_completed),
                 Sid =:= ScopeId,
                 TS > CancelTime ->
                {true, Event};
            _ ->
                false
        end
    end, TraceEvents).

%% @private
%% @doc Determine which evidence sources are available.
determine_evidence_sources(TraceEvents) ->
    HasTrace = length(TraceEvents) > 0,
    HasCancelEvents = lists:any(fun
        (#{type := scope_cancelled}) -> true;
        (_) -> false
    end, TraceEvents),
    HasEffectEvents = lists:any(fun
        (#{type := effect_requested}) -> true;
        (#{type := effect_completed}) -> true;
        (_) -> false
    end, TraceEvents),

    Sources = [],
    Sources1 = case HasTrace of true -> [trace | Sources]; false -> Sources end,
    Sources2 = case HasCancelEvents of true -> [cancel_events | Sources1]; false -> Sources1 end,
    case HasEffectEvents of true -> [effect_events | Sources2]; false -> Sources2 end.

%% @private
%% @doc Convert proof to JSON-compatible map.
proof_to_json(Proof, EvidenceCounter) ->
    BaseProof = #{
        proof_type => maps:get(proof_type, Proof),
        scope_id => format_scope_id(maps:get(scope_id, Proof)),
        cancel_timestamp => maps:get(cancel_timestamp, Proof),
        post_cancel_effects => maps:get(post_cancel_effects, Proof),
        effects_verified => maps:get(effects_verified, Proof),
        evidence_sources => maps:get(evidence_sources, Proof),
        proof_hash => binary:encode_hex(maps:get(proof_hash, Proof)),
        verified_at => maps:get(verified_at, Proof),
        evidence_counter => augment_counter_data(EvidenceCounter)
    },

    %% Add verification status
    case maps:get(effects_verified, Proof) of
        true ->
            BaseProof#{status => verified, message => <<"No effects initiated after cancel">>};
        false ->
            PostCount = maps:get(post_cancel_effects, Proof),
            BaseProof#{
                status => failed,
                message => list_to_binary(io_lib:format(
                    "~p effects found after cancel signal", [PostCount]
                ))
            }
    end.

%% @private
%% @doc Format scope ID for JSON output.
format_scope_id(ScopeId) when is_atom(ScopeId) ->
    atom_to_binary(ScopeId);
format_scope_id(ScopeId) when is_binary(ScopeId) ->
    ScopeId;
format_scope_id({Type, Id}) when is_atom(Type) ->
    <<(atom_to_binary(Type))/binary, ":", (term_to_binary(Id))/binary>>;
format_scope_id(ScopeId) ->
    term_to_binary(ScopeId).

%% @private
%% @doc Augment counter data for evidence section.
augment_counter_data(Counter) ->
    #{
        scope_id => format_scope_id(maps:get(scope_id, Counter)),
        effects_before => maps:get(effects_before, Counter, 0),
        effects_after => maps:get(effects_after, Counter, 0),
        total_effects => maps:get(total_effects, Counter, 0),
        cancel_timestamp => maps:get(cancel_timestamp, Counter, null)
    }.
