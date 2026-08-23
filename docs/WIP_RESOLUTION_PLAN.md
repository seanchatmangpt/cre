# WIP Resolution Plan

**Generated**: 2026-02-08
**Project**: CRE (Common Runtime Environment)
**Total WIP Items**: 7
**Total Estimated Effort**: 53 hours

---

## Overview

This document provides detailed implementation plans for resolving all identified Work-In-Progress (WIP) items in the CRE codebase. Each WIP item includes specific resolution steps, testing requirements, and acceptance criteria.

---

## Resolution Phase Timeline

```
Phase 1 (Week 1): Critical Infrastructure         [10 hours]
Phase 2 (Week 2-3): Pattern Integration          [28 hours]
Phase 3 (Week 4): ML Enhancement                 [8 hours]
Phase 4 (Week 5): Cleanup and Polish             [7 hours]
```

---

## Critical Items (Phase 1)

### WIP-001: Mnesia Timer Restoration

**Estimated Effort**: 4 hours
**Priority**: CRITICAL
**Dependencies**: None

#### Resolution Steps

**Step 1: Add Transaction Safety (1 hour)**
```erlang
%% In wf_persistent_timer.erl
restore_scheduled_timers(State) ->
    Now = erlang:system_time(millisecond),
    Transaction = fun() ->
        case mnesia:match_object(#persistent_timer{_ = '_'}) of
            Timers when is_list(Timers) ->
                Unexpired = [T || T <- Timers,
                                 T#persistent_timer.status =:= scheduled,
                                 T#persistent_timer.target_time > Now],
                {ok, Unexpired}
        end
    end,
    case mnesia:transaction(Transaction) of
        {aborted, Reason} ->
            ?LOG_ERROR("Timer restoration transaction aborted: ~p", [Reason]),
            ok;
        {atomic, {ok, Timers}} ->
            lists:foreach(fun(T) -> restore_single_timer(T, Now) end, Timers),
            ok
    end.
```

**Step 2: Add Distributed Node Coordination (1.5 hours)**
- Implement node preference list for timer ownership
- Add handoff protocol for node shutdown
- Handle split-brain with timestamp-based conflict resolution

**Step 3: Add Recovery State Validation (1 hour)**
- Validate timer callback modules exist
- Check target time is in the future
- Verify callback arity matches expected signature

**Step 4: Add Orphan Cleanup (0.5 hours)**
- Scan for timer references without Mnesia records
- Cancel Erlang timers for non-persistent records

#### Acceptance Criteria
- [ ] All timers restored after node restart
- [ ] No duplicate timer executions
- [ ] Transaction rollback on Mnesia errors
- [ ] Distributed node failover works correctly
- [ ] Orphaned timer references cleaned up

#### Testing Requirements
```erlang
%% Test: wf_persistent_timer_SUITE.erl
t_restore_timers_after_restart(Config) ->
    %% Create timers, restart, verify restoration
t_restore_distributed_nodes(Config) ->
    %% Test multi-node timer restoration
t_restore_orphan_cleanup(Config) ->
    %% Verify orphaned references removed
```

---

### WIP-002: YAML Parsing for GA Constitution

**Estimated Effort**: 6 hours
**Priority**: CRITICAL
**Dependencies**: `yamerl` library

#### Resolution Steps

**Step 1: Implement Schema Validation (2 hours)**
```erlang
%% In ga_constitution.erl
-record(ga_schema, {
    required_fields :: [binary()],
    optional_fields :: [binary()],
    field_types :: map(),  %% field -> type constraint
    custom_validators :: map()  %% field -> validation function
}).

-spec validate_schema(map(), #ga_schema{}) -> {ok, map()} | {error, [binary()]}.
validate_schema(Data, Schema) ->
    RequiredChecks = [check_required_field(F, Data) || F <- Schema#ga_schema.required_fields],
    TypeChecks = [check_field_type(F, maps:get(F, Data, undefined), Schema) || F <- Schema#ga_schema.field_types],
    Errors = lists:filtermap(fun(E) -> case E of {error, Msg} -> {true, Msg}; _ -> false end end,
                              RequiredChecks ++ TypeChecks),
    case Errors of
        [] -> {ok, Data};
        _ -> {error, Errors}
    end.
```

**Step 2: Add Error Reporting (1.5 hours)**
- Include line numbers in parse errors
- Provide context snippets for errors
- Suggest corrections for common mistakes

**Step 3: Support YAML Includes (1.5 hours)**
```erlang
-spec process_includes(map(), binary()) -> {ok, map()} | {error, term()}.
process_includes(Data, BasePath) ->
    IncludePaths = maps:get(<<"$include">>, Data, []),
    process_include_list(IncludePaths, BasePath, Data).
```

**Step 4: Add Complex Type Support (1 hour)**
- YAML anchors and aliases
- Multi-line strings
- Typed values (boolean, integer, float)

#### Acceptance Criteria
- [ ] Schema validation catches all malformed configs
- [ ] Error messages include line numbers and context
- [ ] YAML includes work recursively
- [ ] Anchors and aliases resolve correctly

#### Testing Requirements
```erlang
%% Test: ga_constitution_SUITE.erl
t_valid_schema_parsing(Config) ->
    %% Test valid constitution loads
t_invalid_schema_rejection(Config) ->
    %% Test various schema violations
t_yaml_include_resolution(Config) ->
    %% Test include file processing
t_anchor_alias_resolution(Config) ->
    %% Test YAML references
```

---

## High Priority Items (Phase 2)

### WIP-003: Active Token Integration

**Estimated Effort**: 16 hours
**Priority**: HIGH
**Dependencies**: WIP-007, gen_pnet

#### Resolution Steps

**Step 1: Implement Token Migration Protocol (4 hours)**
```erlang
%% In gen_active_token.erl
-spec migrate(token_id(), place_id()) -> ok | {error, term()}.
migrate(TokenId, TargetPlace) ->
    gen_server:call(TokenId, {migrate, TargetPlace}).

handle_call({migrate, TargetPlace}, _From, State) ->
    case place_coordinator:request_migration(TargetPlace, State) of
        {ok, MigrationRef} ->
            {reply, {ok, migrating}, State#active_token{
                state = migrating,
                migration_ref = MigrationRef
            }};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
```

**Step 2: Define Communication Protocol (4 hours)**
```erlang
-type token_message() :: #{
    from => token_id(),
    to => token_id(),
    payload => term(),
    timestamp => integer()
}.

-spec communicate(token_id(), token_message()) -> ok.
communicate(FromToken, Message) ->
    gen_server:cast(FromToken, {send_message, Message}).
```

**Step 3: Integrate with Place Coordinator (4 hours)**
- Token registration on place entry
- Token tracking during residence
- Notification on place firing

**Step 4: Connect to gen_pnet Firing (4 hours)**
- Hook into transition enabling
- Token participation in firing
- Token updates after firing

#### Acceptance Criteria
- [ ] Tokens can migrate between places
- [ ] Token-to-token communication works
- [ ] Place coordinator tracks all tokens
- [ ] Tokens participate in transition firing
- [ ] Token history reflects all movements

#### Testing Requirements
```erlang
%% Test: active_token_SUITE.erl
t_token_lifecycle(Config) ->
    %% Test birth, migration, death
t_token_communication(Config) ->
    %% Test message passing between tokens
t_place_coordination(Config) ->
    %% Test place coordinator integration
t_token_in_transition_firing(Config) ->
    %% Test token participation in firing
```

---

### WIP-004: RL Strategy Implementations

**Estimated Effort**: 12 hours
**Priority**: HIGH
**Dependencies**: rl_agent.erl

#### Resolution Steps

**Step 1: Complete Thompson Sampling (3 hours)**
```erlang
%% In strategy_thompson_sampling.erl
-spec sample_beta(pos_integer(), pos_integer()) -> float().
sample_beta(Alpha, Beta) ->
    %% Generate random sample from Beta distribution
    %% Using gamma distribution relationship: Beta(a,b) = Gamma(a) / (Gamma(a) + Gamma(b))
    Ga1 = rand:gamma(Alpha, 1.0),
    Ga2 = rand:gamma(Beta, 1.0),
    Ga1 / (Ga1 + Ga2).

select_branch(#thompson_state{arms = Arms}) ->
    Samples = [{A#bandit_arm.branch_id, sample_beta(A#bandit_arm.alpha, A#bandit_arm.beta)} || A <- Arms],
    {BranchId, _Value} = lists:max(fun({_, V1}, {_, V2}) -> V1 > V2 end, Samples),
    BranchId.
```

**Step 2: Complete UCB Strategy (3 hours)**
- Implement UCB1 confidence bound calculation
- Add exploration parameter tuning
- Handle cold-start (initial plays)

**Step 3: Complete Q-Learning (3 hours)**
- Q-table update: Q(s,a) = Q(s,a) + alpha * (reward + gamma * max(Q(s',a')) - Q(s,a))
- Eligibility trace integration
- Experience replay buffer

**Step 4: Complete Contextual Strategy (3 hours)**
- Feature extraction from context
- Linear model for action values
- Online learning with SGD

#### Acceptance Criteria
- [ ] Thompson sampling converges to optimal arm
- [ ] UCB balances exploration/exploitation
- [ ] Q-learning learns optimal policy
- [ ] Contextual strategy uses features effectively

#### Testing Requirements
```erlang
%% Test: rl_strategy_SUITE.erl
t_thompson_convergence(Config) ->
    %% Test convergence to best arm
t_ucb_exploration(Config) ->
    %% Test exploration bonus
t_q_learning_policy(Config) ->
    %% Test Q-value learning
t_contextual_features(Config) ->
    %% Test feature-based selection
```

---

## Medium Priority Items (Phase 3)

### WIP-005: Predictive Mining RNN Training

**Estimated Effort**: 8 hours
**Priority**: MEDIUM
**Dependencies**: predictive_mining.erl, pred_rnn.erl

#### Resolution Steps

**Step 1: Add Sequence Padding (2 hours)**
```erlang
%% In pred_training.erl
-spec pad_sequences([[atom()]], non_neg_integer()) -> [[atom()]].
pad_sequences(Sequences, MaxLength) ->
    [pad_sequence(S, MaxLength) || S <- Sequences].

pad_sequence(Sequence, MaxLength) when length(Sequence) < MaxLength ->
    Sequence ++ lists:duplicate(MaxLength - length(Sequence), <<"<PAD>">>);
pad_sequence(Sequence, MaxLength) when length(Sequence) > MaxLength ->
    lists:sublist(Sequence, MaxLength);
pad_sequence(Sequence, _MaxLength) ->
    Sequence.
```

**Step 2: Add Batch Normalization (2 hours)**
- Feature-wise mean and std calculation
- Normalization application
- Statistics persistence

**Step 3: Add Cross-Validation (2 hours)**
- K-fold split implementation
- Stratified split by outcome
- Aggregate metrics across folds

**Step 4: Add Model Checkpointing (2 hours)**
```erlang
-spec save_checkpoint(binary(), map()) -> ok.
save_checkpoint(ModelId, CheckpointData) ->
    Filename = <<"checkpoints/", ModelId/binary, "_", (integer_to_binary(erlang:system_time(millisecond)))/binary, ".term">>,
    file:write_file(Filename, term_to_binary(CheckpointData)).

-spec load_checkpoint(binary()) -> {ok, map()} | {error, term()}.
load_checkpoint(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> {ok, binary_to_term(Binary)};
        {error, Reason} -> {error, Reason}
    end.
```

#### Acceptance Criteria
- [ ] Variable-length sequences handled correctly
- [ ] Features normalized consistently
- [ ] Cross-validation produces stable metrics
- [ ] Models can be saved and restored

#### Testing Requirements
```erlang
%% Test: pred_training_SUITE.erl
t_sequence_padding(Config) ->
    %% Test padding and truncation
t_batch_normalization(Config) ->
    %% Test feature normalization
t_cross_validation(Config) ->
    %% Test K-fold splitting
t_model_checkpoint(Config) ->
    %% Test save and restore
```

---

## Low Priority Items (Phase 4)

### WIP-006: Place Coordinator Cleanup

**Estimated Effort**: 4 hours
**Priority**: LOW
**Dependencies**: gen_active_token.erl

#### Resolution Steps

**Step 1: Add Token GC (1.5 hours)**
```erlang
%% In place_coordinator.erl
-spec garbage_collect_tokens(place_id(), integer()) -> {ok, [token_id()]}.
garbage_collect_tokens(PlaceId, TTL) ->
    Now = erlang:system_time(millisecond),
    ExpiredTokens = [T || T <- get_tokens_at(PlaceId),
                          token_age(T, Now) > TTL],
    lists:foreach(fun expire_token/1, ExpiredTokens),
    {ok, ExpiredTokens}.
```

**Step 2: Add Capacity Limits (1 hour)**
- Configure max tokens per place
- Backpressure on token entry
- Priority-based admission

**Step 3: Add Deadlock Detection (1 hour)**
- Cycle detection in token dependencies
- Timeout-based deadlock resolution
- Alerting on detected deadlocks

**Step 4: Add Telemetry (0.5 hours)**
- Token count metrics
- Token residency duration
- GC operation counts

#### Acceptance Criteria
- [ ] Expired tokens are garbage collected
- [ ] Places respect capacity limits
- [ ] Deadlocks are detected and resolved
- [ ] Telemetry data is emitted

#### Testing Requirements
```erlang
%% Test: place_coordinator_SUITE.erl
t_token_gc(Config) ->
    %% Test expired token removal
t_capacity_limits(Config) ->
    %% Test place capacity enforcement
t_deadlock_detection(Config) ->
    %% Test deadlock detection
t_telemetry_emission(Config) ->
    %% Test metrics emission
```

---

### WIP-007: Rust NIF Error Handling Fallbacks

**Estimated Effort**: 3 hours
**Priority**: LOW
**Dependencies**: Rust NIF bindings

#### Resolution Steps

**Step 1: Implement Automatic Fallback (1 hour)**
```erlang
%% In rust_nif.erl
-spec call_nif_with_fallback(atom(), atom(), list(), function()) -> term().
call_nif_with_fallback(Module, Function, Args, Fallback) ->
    try
        erlang:apply(Module, Function, Args)
    catch
        error:undef ->
            ?LOG_WARNING("NIF ~p:~p not available, using fallback", [Module, Function]),
            Fallback(Args);
        error:{nif_not_loaded, _} ->
            ?LOG_WARNING("NIF ~p:~p not loaded, using fallback", [Module, Function]),
            Fallback(Args)
    end.
```

**Step 2: Add Feature Parity Tests (1 hour)**
- Compare NIF vs fallback outputs
- Performance benchmarking
- Document any semantic differences

**Step 3: Add Fallback Telemetry (0.5 hours)**
- Track fallback usage count
- Monitor performance impact
- Alert on excessive fallbacks

**Step 4: Document Behavior (0.5 hours)**
- Document fallback behavior for each function
- Add examples of fallback usage
- Explain performance implications

#### Acceptance Criteria
- [ ] Fallback activates automatically on NIF failure
- [ ] Fallback produces semantically equivalent results
- [ ] Fallback usage is tracked
- [ ] Behavior is documented

#### Testing Requirements
```erlang
%% Test: rust_nif_SUITE.erl
t_auto_fallback_on_failure(Config) ->
    %% Test fallback activation
t_fallback_feature_parity(Config) ->
    %% Test equivalent results
t_fallback_telemetry(Config) ->
    %% Test tracking of fallback usage
```

---

## Resolution Checklist

### Phase 1: Critical Infrastructure
- [ ] WIP-001: Mnesia Timer Restoration
  - [ ] Transaction safety
  - [ ] Distributed coordination
  - [ ] Recovery validation
  - [ ] Orphan cleanup
- [ ] WIP-002: YAML Parsing
  - [ ] Schema validation
  - [ ] Error reporting
  - [ ] Include support
  - [ ] Complex types

### Phase 2: Pattern Integration
- [ ] WIP-003: Active Token Integration
  - [ ] Migration protocol
  - [ ] Communication protocol
  - [ ] Place coordinator integration
  - [ ] gen_pnet firing integration
- [ ] WIP-004: RL Strategies
  - [ ] Thompson sampling
  - [ ] UCB implementation
  - [ ] Q-learning
  - [ ] Contextual strategy

### Phase 3: ML Enhancement
- [ ] WIP-005: Predictive Mining RNN
  - [ ] Sequence padding
  - [ ] Batch normalization
  - [ ] Cross-validation
  - [ ] Model checkpointing

### Phase 4: Cleanup and Polish
- [ ] WIP-006: Place Coordinator Cleanup
  - [ ] Token GC
  - [ ] Capacity limits
  - [ ] Deadlock detection
  - [ ] Telemetry
- [ ] WIP-007: Rust NIF Fallbacks
  - [ ] Automatic fallback
  - [ ] Parity tests
  - [ ] Telemetry
  - [ ] Documentation

---

## Success Metrics

### Code Quality
- All WIP items resolved
- Test coverage > 85% for modified modules
- Dialyzer warnings < 10
- Compilation warnings = 0

### Functional
- All acceptance criteria met
- All tests passing
- No regressions in existing functionality

### Documentation
- All new code documented
- API references updated
- Examples provided

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Mnesia data loss during migration | Backup before changes, test on staging |
| RL strategy non-convergence | Start with simple bandit, validate incrementally |
| Active token deadlock | Implement timeout-based recovery |
| NIF fallback performance | Document and monitor, optimize hot paths |

---

**Last Updated**: 2026-02-08
**Plan Owner**: Development Team
**Next Review**: Weekly during resolution phase
