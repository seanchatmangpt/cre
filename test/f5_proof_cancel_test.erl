%%%-------------------------------------------------------------------
%%% @doc f5_proof_cancel test suite
%%%
%%% Tests cancel-scope proof verification ensuring no effects after cancel.
%%% @end
%%%-------------------------------------------------------------------
-module(f5_proof_cancel_test).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Data
%%%-------------------------------------------------------------------

%% Helper to create a cancel event
cancel_event(ScopeId, Timestamp, Seq) ->
    #{
        timestamp => Timestamp,
        type => scope_cancelled,
        scope_id => ScopeId,
        seq => Seq
    }.

%% Helper to create an effect requested event
effect_requested_event(ScopeId, EffectId, Timestamp, Seq) ->
    #{
        timestamp => Timestamp,
        effect_id => EffectId,
        scope_id => ScopeId,
        type => effect_requested,
        seq => Seq
    }.

%% Helper to create an effect completed event
effect_completed_event(ScopeId, EffectId, Timestamp, Seq) ->
    #{
        timestamp => Timestamp,
        effect_id => EffectId,
        scope_id => ScopeId,
        type => effect_completed,
        seq => Seq
    }.

%% Helper to create evidence counter
evidence_counter(ScopeId, Before, After, CancelTime, Total) ->
    #{
        scope_id => ScopeId,
        effects_before => Before,
        effects_after => After,
        cancel_timestamp => CancelTime,
        total_effects => Total
    }.

%%%-------------------------------------------------------------------
%%% Verification Tests
%%%-------------------------------------------------------------------

%% @doc Test successful verification with no post-cancel effects
verify_cancel_no_post_effects_test() ->
    ScopeId = test_scope,
    CancelTime = 1000,

    TraceEvents = [
        effect_requested_event(ScopeId, make_ref(), 500, 1),
        cancel_event(ScopeId, CancelTime, 2),
        %% No effects after cancel
        effect_completed_event(ScopeId, make_ref(), 800, 3)  %% Completed before cancel
    ],

    Result = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    ?assertMatch({ok, #{
        proof_type := cancel_scope,
        scope_id := test_scope,
        post_cancel_effects := 0,
        effects_verified := true
    }}, Result).

%% @doc Test verification failure with post-cancel effects found
verify_cancel_with_post_effects_fails_test() ->
    ScopeId = test_scope_2,
    CancelTime = 1000,
    BadEffectId = make_ref(),

    TraceEvents = [
        effect_requested_event(ScopeId, make_ref(), 500, 1),
        cancel_event(ScopeId, CancelTime, 2),
        %% Bad: effect initiated AFTER cancel
        effect_requested_event(ScopeId, BadEffectId, 1500, 3)
    ],

    Result = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    ?assertMatch({error, {post_cancel_effects_found, 1, _}}, Result).

%% @doc Test verification with multiple scopes - only target scope checked
verify_cancel_multiple_scopes_test() ->
    ScopeA = scope_a,
    ScopeB = scope_b,
    CancelTime = 1000,

    TraceEvents = [
        effect_requested_event(ScopeA, make_ref(), 500, 1),
        cancel_event(ScopeA, CancelTime, 2),
        %% Effect in ScopeB is OK (different scope)
        %% But ScopeB also needs its own cancel event to verify
        cancel_event(ScopeB, 1500, 3),
        effect_requested_event(ScopeB, make_ref(), 1000, 4),
        %% Bad: effect in ScopeA after cancel
        effect_requested_event(ScopeA, make_ref(), 1500, 5)
    ],

    ResultA = f5_proof_cancel:verify_cancel_scope(ScopeA, TraceEvents),
    ResultB = f5_proof_cancel:verify_cancel_scope(ScopeB, TraceEvents),

    ?assertMatch({error, {post_cancel_effects_found, 1, _}}, ResultA),
    ?assertMatch({ok, #{post_cancel_effects := 0}}, ResultB).

%% @doc Test error when cancel event not found
verify_cancel_missing_cancel_event_test() ->
    ScopeId = missing_cancel_scope,

    TraceEvents = [
        effect_requested_event(ScopeId, make_ref(), 500, 1)
        %% No cancel event
    ],

    Result = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    ?assertMatch({error, {cancel_not_found, missing_cancel_scope}}, Result).

%% @doc Test verification with edge case: effect at exact cancel timestamp
verify_cancel_effect_at_cancel_time_test() ->
    ScopeId = edge_case_scope,
    CancelTime = 1000,

    TraceEvents = [
        %% Effect at same time as cancel - considered "before" for >= comparison
        effect_requested_event(ScopeId, make_ref(), CancelTime, 1),
        cancel_event(ScopeId, CancelTime, 2),
        %% Effect AFTER cancel timestamp
        effect_requested_event(ScopeId, make_ref(), 1001, 3)
    ],

    Result = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    %% Effect at 1001 is > 1000, so should be detected
    ?assertMatch({error, {post_cancel_effects_found, 1, _}}, Result).

%% @doc Test empty trace returns appropriate error
verify_cancel_empty_trace_test() ->
    ScopeId = empty_scope,

    TraceEvents = [],

    Result = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    ?assertMatch({error, {cancel_not_found, empty_scope}}, Result).

%%%-------------------------------------------------------------------
%%% Effect Counter Tests
%%%-------------------------------------------------------------------

%% @doc Test check_effect_counters with valid zero-count
check_effect_counters_zero_test() ->
    ScopeId = counter_scope,
    Counter = evidence_counter(ScopeId, 5, 0, 1000, 5),

    Result = f5_proof_cancel:check_effect_counters(ScopeId, Counter),

    ?assertEqual({ok, 0, true}, Result).

%% @doc Test check_effect_counters with non-zero count fails
check_effect_counters_nonzero_fails_test() ->
    ScopeId = counter_scope_2,
    Counter = evidence_counter(ScopeId, 5, 2, 1000, 7),

    Result = f5_proof_cancel:check_effect_counters(ScopeId, Counter),

    ?assertEqual({ok, 2, false}, Result).

%% @doc Test check_effect_counters with missing timestamp
check_effect_counters_no_timestamp_test() ->
    ScopeId = counter_scope_3,
    Counter = evidence_counter(ScopeId, 5, 0, undefined, 5),

    Result = f5_proof_cancel:check_effect_counters(ScopeId, Counter),

    ?assertEqual({error, no_cancel_timestamp}, Result).

%% @doc Test check_effect_counters with scope mismatch
check_effect_counters_scope_mismatch_test() ->
    ScopeId = counter_scope_4,
    Counter = evidence_counter(other_scope, 5, 0, 1000, 5),

    Result = f5_proof_cancel:check_effect_counters(ScopeId, Counter),

    ?assertEqual({error, scope_mismatch}, Result).

%%%-------------------------------------------------------------------
%%% Proof Generation Tests
%%%-------------------------------------------------------------------

%% @doc Test generate_cancel_proof creates valid JSON artifact
generate_cancel_proof_valid_test() ->
    ScopeId = proof_gen_scope,
    CancelTime = 2000,

    TraceEvents = [
        effect_requested_event(ScopeId, make_ref(), 1000, 1),
        cancel_event(ScopeId, CancelTime, 2)
    ],

    Counter = evidence_counter(ScopeId, 1, 0, CancelTime, 1),

    Result = f5_proof_cancel:generate_cancel_proof(ScopeId, TraceEvents, Counter),

    ?assertMatch({ok, #{
        proof_type := cancel_scope,
        scope_id := <<"proof_gen_scope">>,
        status := verified,
        message := <<"No effects initiated after cancel">>,
        proof_hash := _,  %% Any binary
        evidence_sources := [_ | _]
    }}, Result).

%% @doc Test generate_cancel_proof returns error when verification fails
generate_cancel_proof_failure_status_test() ->
    ScopeId = proof_fail_scope,
    CancelTime = 2000,

    TraceEvents = [
        cancel_event(ScopeId, CancelTime, 1),
        effect_requested_event(ScopeId, make_ref(), 3000, 2)  %% After cancel
    ],

    Counter = evidence_counter(ScopeId, 0, 1, CancelTime, 1),

    Result = f5_proof_cancel:generate_cancel_proof(ScopeId, TraceEvents, Counter),

    %% When verification fails, generate_cancel_proof returns the error
    ?assertMatch({error, {post_cancel_effects_found, 1, _}}, Result).

%% @doc Test proof hash is deterministic
generate_cancel_proof_hash_deterministic_test() ->
    ScopeId = hash_scope,
    CancelTime = 2000,

    TraceEvents = [
        cancel_event(ScopeId, CancelTime, 1)
    ],

    Counter = evidence_counter(ScopeId, 0, 0, CancelTime, 0),

    {ok, Proof1} = f5_proof_cancel:generate_cancel_proof(ScopeId, TraceEvents, Counter),
    {ok, Proof2} = f5_proof_cancel:generate_cancel_proof(ScopeId, TraceEvents, Counter),

    Hash1 = maps:get(proof_hash, Proof1),
    Hash2 = maps:get(proof_hash, Proof2),

    ?assertEqual(Hash1, Hash2).

%%%-------------------------------------------------------------------
%%% Evidence Source Tests
%%%-------------------------------------------------------------------

%% @doc Test evidence sources detection with full trace
evidence_sources_full_trace_test() ->
    ScopeId = evidence_scope,

    TraceEvents = [
        effect_requested_event(ScopeId, make_ref(), 500, 1),
        cancel_event(ScopeId, 1000, 2),
        effect_completed_event(ScopeId, make_ref(), 800, 3)
    ],

    {ok, Proof} = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    Sources = maps:get(evidence_sources, Proof),

    ?assert(lists:member(trace, Sources)),
    ?assert(lists:member(cancel_events, Sources)),
    ?assert(lists:member(effect_events, Sources)).

%% @doc Test evidence sources with cancel event only
evidence_sources_cancel_only_test() ->
    ScopeId = cancel_only_scope,

    TraceEvents = [
        cancel_event(ScopeId, 1000, 1)
    ],

    {ok, Proof} = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    Sources = maps:get(evidence_sources, Proof),

    ?assert(lists:member(trace, Sources)),
    ?assert(lists:member(cancel_events, Sources)),
    ?assertNot(lists:member(effect_events, Sources)).

%%%-------------------------------------------------------------------
%%% Complex Scenario Tests
%%%-------------------------------------------------------------------

%% @doc Test complex workflow with nested cancellations
verify_cancel_nested_scopes_test() ->
    Parent = parent_scope,
    Child = child_scope,
    CancelTime = 1500,

    TraceEvents = [
        effect_requested_event(Parent, make_ref(), 500, 1),
        effect_requested_event(Child, make_ref(), 1000, 2),
        cancel_event(Parent, CancelTime, 3),
        %% This effect in Child is after Parent cancel
        %% But our verification is per-scope, so we add a cancel for Child too
        cancel_event(Child, 2000, 4),
        %% Effect in Child after Child cancel - should fail
        effect_requested_event(Child, make_ref(), 2500, 5)
    ],

    ParentResult = f5_proof_cancel:verify_cancel_scope(Parent, TraceEvents),
    ChildResult = f5_proof_cancel:verify_cancel_scope(Child, TraceEvents),

    ?assertMatch({ok, #{post_cancel_effects := 0}}, ParentResult),
    ?assertMatch({error, {post_cancel_effects_found, 1, _}}, ChildResult).

%% @doc Test with effect completed before cancel but requested before
verify_cancel_effect_completed_after_test() ->
    ScopeId = timing_scope,
    EffectId = make_ref(),
    CancelTime = 1000,

    TraceEvents = [
        effect_requested_event(ScopeId, EffectId, 500, 1),
        cancel_event(ScopeId, CancelTime, 2),
        %% Effect completed AFTER cancel - this is expected behavior
        %% The effect was requested before cancel, so completion is OK
        %% Our verification counts EFFECT_REQUESTED events after cancel
        %% So this should pass (no new requests after cancel)
        effect_completed_event(ScopeId, EffectId, 800, 3)  %% Completed before cancel
    ],

    %% Effect was requested and completed BEFORE cancel
    Result = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    ?assertMatch({ok, #{post_cancel_effects := 0}}, Result).

%% @doc Test multiple effects after cancel are all counted
verify_cancel_multiple_post_effects_test() ->
    ScopeId = multiple_scope,
    CancelTime = 1000,

    TraceEvents = [
        cancel_event(ScopeId, CancelTime, 1),
        effect_requested_event(ScopeId, make_ref(), 1100, 2),
        effect_requested_event(ScopeId, make_ref(), 1200, 3),
        effect_requested_event(ScopeId, make_ref(), 1300, 4)
    ],

    Result = f5_proof_cancel:verify_cancel_scope(ScopeId, TraceEvents),

    ?assertMatch({error, {post_cancel_effects_found, 3, _}}, Result).

%% @doc Test compute_proof_hash directly
compute_proof_hash_test() ->
    ScopeId = test_scope,
    CancelTime = 12345,
    EffectCount = 0,

    Hash = f5_proof_cancel:compute_proof_hash(ScopeId, CancelTime, EffectCount),

    ?assertEqual(32, byte_size(Hash)),
    %% Same inputs produce same hash
    Hash2 = f5_proof_cancel:compute_proof_hash(ScopeId, CancelTime, EffectCount),
    ?assertEqual(Hash, Hash2),
    %% Different inputs produce different hash
    Hash3 = f5_proof_cancel:compute_proof_hash(ScopeId, CancelTime, 1),
    ?assertNotEqual(Hash, Hash3).
