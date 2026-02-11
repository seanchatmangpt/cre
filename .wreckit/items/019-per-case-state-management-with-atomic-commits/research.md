# Research: Per-case state management with atomic commits

**Date**: 2025-02-11
**Item**: 019-per-case-state-management-with-atomic-commits

## Research Question

Distributed state management can lead to inconsistency. Need single source of truth per case with atomic updates and clear boundaries for side effects.

**Motivation:** Ensures consistency, enables restart/recovery, supports replay from any commit point, provides clear audit trail through receipts.

**Success criteria:**
- Atomic commits with no partials
- Single coherent state store per case
- All side effects have receipts

**Technical constraints:**
- One state store per case/shard
- Atomic commit boundaries
- No shadow state duplication across engines

**Signals:** priority: high, urgency: Core data management component

## Summary

The CRE codebase has **multiple overlapping state management systems** that create inconsistency risks. The research reveals a **fragmented architecture** with at least **5 different state stores** across the system:

1. **gen_pnet/net_state** (`#net_state{}`) - Base Petri net execution state
2. **gen_yawl/wrapper_state** (`#wrapper_state{}`) - Wrapper around net_state
3. **wf_engine/wf_case** (`#wf_case{}`) - Workflow engine case state
4. **wf_persistence** (Mnesia tables) - Persistent storage for cases, work items, events
5. **yawl_recovery** (Mnesia `yawl_checkpoint` table) - Checkpoint storage

**Critical Finding:** These state stores are **NOT atomically coordinated**. When a workflow case updates its state, multiple writes happen independently:
- `net_state#net_state.marking` updates in gen_pnet process memory
- `wf_case#wf_case.data` updates in wf_engine process memory
- Mnesia tables update via `wf_persistence:save_case/1`
- Checkpoints save via `yawl_recovery:checkpoint/4`

**This creates a window for inconsistency** - if a crash occurs between these writes, the system has partial state with no clear source of truth.

**Key Gaps:**
1. ❌ No **atomic commit protocol** coordinating all state updates
2. ❌ No **single state store per case** - state is scattered across processes
3. ❌ **Shadow state duplication** - net_state and wf_case store overlapping data
4. ❌ No **transaction boundaries** for multi-step state mutations
5. ❌ Receipts exist but are **not coupled to commits**

## Current State Analysis

### Existing Implementation

#### 1. Multi-Process State Architecture

**gen_pnet Process State** (`/Users/sac/cre/src/core/gen_pnet.erl:30-37`):
```erlang
-record(net_state, {
    marking,      % Petri net marking (place => [tokens])
    net_mod,      % Callback module
    usr_info,     % User-defined state
    stats,        % Throughput statistics
    tstart,       % Statistics start timestamp
    cnt           % Fire counter
}).
```
- Stored in **process memory** of gen_pnet/gen_yawl process
- Lost if process crashes
- No coordination with other state stores

**gen_yawl Wrapper State** (`/Users/sac/cre/src/core/gen_yawl.erl:177-193`):
```erlang
-record(wrapper_state, {
    net_mod,
    net_state,          % Contains #net_state{}
    net_arg = #{},
    fire_timeout = 5000,
    progress_timeout = 30000,
    shutting_down = false,
    active_fires = 0,
    marking_history = [],     % Cycle detection
    max_marking_history = 10,
    continue_count = 0,
    max_continue = 1000,
    regions = #{},
    checkpoint_interval = 0,
    drain_step_count = 0
}).
```
- Wraps net_state in a gen_server
- Adds checkpoint_interval field
- **No commit mechanism** - checkpoints are periodic, not atomic with state updates

**wf_engine Case State** (`/Users/sac/cre/src/wf/wf_engine.hrl:27-40`):
```erlang
-record(wf_case, {
    case_id,
    status,
    work_items = #{},
    data = #{},           % Workflow variables
    receipts = [],        % Audit trail
    events = [],
    log = [],
    marking,             % DUPLICATE: also in net_state
    rng_state,           % Random state for deterministic replay
    scheduled_at,
    timestamps
}).
```
- **Critical duplication:** `marking` field duplicates `net_state.marking`
- Lives in **separate process** (wf_engine gen_server)
- No synchronization with gen_pnet state

#### 2. Persistence Layer Fragmentation

**wf_persistence** (`/Users/sac/cre/src/wf/wf_persistence.erl:85-118`):
```erlang
-record(wf_persistent_case, {
    case_id,
    status,
    marking,             % DUPLICATE #3
    data,                % DUPLICATE #4
    receipts,            % Receipts
    rng_state,
    timestamps,
    scheduled_at
}).
```
- Mnesia table with disc_copies
- Independent writes via `save_case/1` (line 254-279)
- **No transaction** with net_state updates

**yawl_recovery** (`/Users/sac/cre/include/yawl_recovery.hrl:9-17`):
```erlang
-record(yawl_checkpoint, {
    checkpoint_id,       % Primary key
    spec_id,
    case_id,
    marking,             % DUPLICATE #5
    data,                % DUPLICATE #6
    timestamp,
    version
}).
```
- Checkpoints saved periodically via `maybe_checkpoint/5` (line 223-260)
- Triggered by step count, **not** by state changes
- No coordination with wf_persistence writes

**Audit Log** (`/Users/sac/cre/src/wf/wf_audit_log.erl:97-102`):
```erlang
-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := map(),
    ts := integer()
}.
```
- Append-only log using disk_log
- Receipts track state transitions
- **Not tied to commits** - logged independently

#### 3. State Update Flow - No Atomicity

Current update sequence for a workflow step:
1. gen_pnet fires transition → updates `net_state.marking` (in process memory)
2. gen_yawl wrapper checks `checkpoint_interval` → may call `yawl_recovery:checkpoint/4`
3. wf_engine updates `wf_case.marking` (in separate process)
4. wf_persistence writes to Mnesia (independent transaction)

**Problem:** Steps 1-4 are **not atomic**. If crash occurs at any point, state is inconsistent.

### Key Files

**State definitions:**
- `/Users/sac/cre/include/gen_pnet.hrl:30-37` - #net_state{} definition
- `/Users/sac/cre/include/yawl_recovery.hrl:9-17` - #yawl_checkpoint{} definition
- `/Users/sac/cre/src/wf/wf_engine.hrl:27-40` - #wf_case{} definition
- `/Users/sac/cre/src/core/gen_yawl.erl:177-193` - #wrapper_state{} definition

**State persistence:**
- `/Users/sac/cre/src/wf/wf_persistence.erl:85-118` - Mnesia table records
- `/Users/sac/cre/src/wf/yawl_recovery.erl:95-511` - Checkpoint implementation
- `/Users/sac/cre/src/wf/yawl_checkpoint.erl:36-43` - Alternative checkpoint layer

**State mutation:**
- `/Users/sac/cre/src/core/gen_pnet.erl:706-721` - Continue loop (state updates)
- `/Users/sac/cre/src/core/gen_yawl.erl:928-1048` - Wrapper continue loop
- `/Users/sac/cre/src/wf/wf_persistence.erl:254-279` - save_case/1 implementation

**Receipts and audit:**
- `/Users/sac/cre/src/wf/wf_audit_log.erl:1-493` - Append-only receipt log
- `/Users/sac/cre/src/pnet/pnet_receipt.erl:1-184` - Petri net transition receipts

## Technical Considerations

### Dependencies

**Internal modules that manage state:**
- `gen_pnet` - Base Petri net state (marking + usr_info)
- `gen_yawl` - Wrapper around gen_pnet with checkpoint hooks
- `wf_engine` - Workflow case management (separate process)
- `wf_persistence` - Mnesia persistence layer
- `yawl_recovery` - Checkpoint system
- `wf_audit_log` - Receipt storage

**External dependencies:**
- **Mnesia** - Distributed database (already used for persistence)
- **disk_log** - Append-only log storage (used for audit log)
- **crypto** - Hash generation for receipts

### Patterns to Follow

**1. Mnesia transaction pattern** (`/Users/sac/cre/src/wf/yawl_recovery.erl:303-322`):
```erlang
Transaction = fun() ->
    %% Verify previous checkpoint exists
    case mnesia:read(yawl_checkpoint, PrevCpid) of
        [] -> mnesia:abort({previous_checkpoint_not_found, PrevCpid});
        _ -> ok
    end,
    mnesia:write(CheckpointRecord),
    {ok, CheckpointId}
end,

case mnesia:transaction(Transaction) of
    {atomic, {ok, Cpid}} -> {ok, Cpid};
    {aborted, Reason} -> {error, Reason}
end.
```
This pattern ensures atomic writes within Mnesia. Need to extend to coordinate all state stores.

**2. Receipt generation pattern** (`/Users/sac/cre/src/wf/wf_audit_log.erl:97-102`):
```erlang
-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := map(),
    ts := integer()
}.
```
Receipts already track state transitions with before/after hashes. Need to tie these to commit boundaries.

**3. Checkpoint trigger pattern** (`/Users/sac/cre/src/wf/yawl_recovery.erl:223-260`):
```erlang
maybe_checkpoint(StepCount, Interval, NetArg, Marking, UsrInfo) ->
    case StepCount rem Interval =:= 0 of
        false -> ok;
        true ->
            {do_checkpoint, SpecId, CaseId, Marking, Data}
    end.
```
Current checkpointing is step-based, not state-change-based. Need event-driven checkpoints.

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **No atomic commits across state stores** | Critical - Partial state on crash, inconsistent recovery | Implement two-phase commit protocol coordinating gen_pnet, wf_engine, and Mnesia writes |
| **Shadow state duplication** | High - Marking stored in 3+ places, diverges on updates | Consolidate to single source of truth per case; other layers are views/caches |
| **Checkpoint timing gaps** | High - State changes between checkpoints lost | Event-driven checkpointing on every state transition, not periodic |
| **Process crash loses in-memory state** | High - net_state and wrapper_state lost on gen_server crash | Make Mnesia the primary state store; processes are stateless workers |
| **No rollback mechanism** | Medium - Failed commits leave partial state | Implement compensating transactions using receipts for undo |
| **RNG state not checkpointed atomically** | High - Breaks deterministic replay | Include rng_state in atomic commit with marking and usr_info |
| **Receipts not tied to commits** | Medium - Audit trail may diverge from actual state | Generate receipt atomically with commit, include commit ID |
| **Multiple state update paths** | Critical - Some code bypasses wf_persistence | Enforce all state mutations go through commit function |

## Recommended Approach

### High-Level Strategy

**Core Principle:** One state store per case with atomic commits.

**Architecture:**
```
┌─────────────────────────────────────────────────────────┐
│                   Case State Manager                    │
│  (single gen_server per case, stateless coordinator)    │
├─────────────────────────────────────────────────────────┤
│  Operations:                                            │
│  - commit(Marking, UsrInfo, Effects)                    │
│  - get_state(CaseId) → {Marking, UsrInfo, Receipts}     │
│  - replay_from_commit(CommitId)                         │
└────────────┬────────────────────────────────────────────┘
             │
             │ (atomic commit via Mnesia transaction)
             ▼
┌─────────────────────────────────────────────────────────┐
│              Mnesia Case State Table                     │
│  ┌──────────────────────────────────────────────────┐  │
│  │ - case_id (primary key)                          │  │
│  │ - commit_id (logical timestamp)                  │  │
│  │ - marking (Petri net marking)                    │  │
│  │ - usr_info (workflow variables)                  │  │
│  │ - receipts (audit trail)                         │  │
│  │ - rng_state (deterministic replay)               │  │
│  │ - effects_receipt (side effect tracking)         │  │
│  │ - parent_commit_id (for rollback)                │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

**Key changes:**
1. **Single state table per case** in Mnesia (eliminate shadow state)
2. **Atomic commit function** that updates marking, usr_info, receipts in one transaction
3. **Commit log** - append-only list of all commits for replay
4. **Stateless execution engines** - gen_pnet/gen_yawl become workers, not state owners
5. **Receipt generation** tied to commits (every commit has exactly one receipt)

### Phase 1: Design Atomic Commit Protocol

**Objective:** Ensure all state updates happen atomically.

1. **Design commit record structure:**
```erlang
-record(case_commit, {
    commit_id :: binary(),          % UUID v4
    case_id :: binary(),
    parent_commit_id :: binary() | undefined,  % For rollback
    marking :: pnet_types:marking(),
    usr_info :: term(),
    rng_state :: rand:state(),
    receipts :: [term()],
    effects :: [effect_receipt()],
    timestamp :: integer(),
    version :: non_neg_integer()
}).
```

2. **Implement commit/4 function:**
```erlang
-spec commit(CaseId, Marking, UsrInfo, Effects) ->
    {ok, CommitId, Receipt} | {error, Reason}.

commit(CaseId, Marking, UsrInfo, Effects) ->
    CommitId = generate_commit_id(),
    RNGState = rand:export_seed(),

    Transaction = fun() ->
        %% Get parent commit
        ParentCommit = get_latest_commit(CaseId),

        %% Generate receipt
        Receipt = generate_receipt(ParentCommit, Marking, Effects),

        %% Write commit record
        Commit = #case_commit{
            commit_id = CommitId,
            case_id = CaseId,
            parent_commit_id = case ParentCommit of
                {ok, PC} -> PC#case_commit.commit_id;
                undefined -> undefined
            end,
            marking = Marking,
            usr_info = UsrInfo,
            rng_state = RNGState,
            receipts = [Receipt],
            effects = Effects,
            timestamp = erlang:system_time(millisecond),
            version = increment_version(ParentCommit)
        },

        ok = mnesia:write(case_commits, Commit, write),
        ok = mnesia:write(case_latest, {CaseId, CommitId}, write),

        {ok, CommitId, Receipt}
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.
```

3. **Add rollback support:**
```erlang
-spec rollback(CaseId, CommitId) -> ok | {error, Reason}.

rollback(CaseId, TargetCommitId) ->
    Transaction = fun() ->
        %% Verify target commit exists
        case mnesia:read(case_commits, TargetCommitId) of
            [#case_commit{case_id = CaseId}] -> ok;
            [] -> mnesia:abort(commit_not_found)
        end,

        %% Update case_latest pointer
        ok = mnesia:write(case_latest, {CaseId, TargetCommitId}, write)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.
```

**Estimated effort:** 5-7 days

### Phase 2: Migrate Execution Engines to Stateless Workers

**Objective:** Eliminate shadow state in gen_pnet/gen_yawl processes.

1. **Modify gen_yawl to fetch state on each step:**
```erlang
%% Before (stateful):
handle_cast(continue, WrapperState) ->
    NetState = WrapperState#wrapper_state.net_state,
    {delta, Mode, Pm, NewUsrInfo} = progress(NetState, ...),
    %% ... update in-memory state

%% After (stateless):
handle_cast(continue, WrapperState) ->
    CaseId = WrapperState#wrapper_state.case_id,
    {ok, CurrentCommit} = case_state_manager:get_state(CaseId),

    %% Execute transition
    {ok, NewMarking, NewUsrInfo} = progress(
        CurrentCommit#case_commit.marking,
        CurrentCommit#case_commit.usr_info,
        ...
    ),

    %% Commit new state atomically
    {ok, CommitId, Receipt} = case_state_manager:commit(
        CaseId, NewMarking, NewUsrInfo, []
    ),

    %% Continue loop
    continue(self()),
    {noreply, WrapperState};
```

2. **Remove shadow state fields:**
   - Delete `marking` from `#wf_case{}` (exists in commits)
   - Delete `usr_info` from `#net_state{}` (exists in commits)
   - Keep process state minimal (timeout, config, etc.)

3. **Update all state mutation paths:**
   - `gen_pnet:fire/3` → returns new marking, doesn't mutate state
   - `gen_yawl:continue/1` → calls commit instead of updating memory
   - `wf_engine:complete/5` → commits state atomically

**Estimated effort:** 10-14 days (high complexity)

### Phase 3: Integrate Receipts with Commits

**Objective:** Every commit generates exactly one receipt.

1. **Extend receipt format:**
```erlang
-type commit_receipt() :: #{
    commit_id := binary(),
    before_hash := binary(),      % Hash of parent commit
    after_hash := binary(),       % Hash of this commit
    move := map(),                % Transition fired
    effects := [effect_receipt()],
    ts := integer(),
    case_id := binary()
}.
```

2. **Generate receipt atomically in commit transaction:**
```erlang
Receipt = #{
    commit_id => CommitId,
    before_hash => hash_commit(ParentCommit),
    after_hash => hash_commit(Commit),
    move => #{transition => Transition, ...},
    effects => Effects,
    ts => Timestamp,
    case_id => CaseId
},
```

3. **Store receipts in separate table for audit:**
```erlang
-record(case_receipt, {
    receipt_id :: binary(),        % Composite: {CaseId, CommitId}
    commit_id :: binary(),
    case_id :: binary(),
    receipt :: commit_receipt(),
    timestamp :: integer()
}).
```

**Estimated effort:** 3-5 days

### Phase 4: Implement Deterministic Replay

**Objective:** Support replay from any commit point.

1. **Add replay function:**
```erlang
-spec replay_from_commit(CaseId, CommitId) -> ok | {error, Reason}.

replay_from_commit(CaseId, CommitId) ->
    Transaction = fun() ->
        %% Load commit
        [#case_commit{marking = Marking, usr_info = UsrInfo, rng_state = RNG}] =
            mnesia:read(case_commits, CommitId),

        %% Load receipts from this point
        Receipts = mnesia:index_read(case_receipts, CommitId, #case_receipt.commit_id),

        %% Restore RNG state
        rand:seed(exs1024, RNG),

        {ok, Marking, UsrInfo, Receipts}
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.
```

2. **Verify replay determinism:**
```erlang
-spec verify_replay(CaseId) -> boolean().

verify_replay(CaseId) ->
    {ok, Commits} = get_commit_history(CaseId),

    %% Replay from first commit
    {ok, _} = replay_from_commit(CaseId, hd(Commits)),

    %% Execute all transitions
    {ok, FinalCommit} = execute_all(CaseId),

    %% Compare final hash with original
    FinalCommit#case_commit.commit_id =:= lists:last(Commits).
```

**Estimated effort:** 5-7 days

### Phase 5: Testing and Migration

**Objective:** Ensure correctness and migrate existing workflows.

1. **Property-based tests for atomic commits:**
```erlang
prop_commit_atomic() ->
    ?FORALL({Marking, UsrInfo}, {marking_gen(), usr_info_gen()},
        begin
            CaseId = <<"test_case">>,
            {ok, CommitId1, _} = commit(CaseId, Marking, UsrInfo, []),

            %% Crash simulation
            exit(whereis(case_state_manager), kill),

            %% Verify recovery
            {ok, LoadedCommit} = get_latest_commit(CaseId),
            equals(Marking, LoadedCommit#case_commit.marking)
        end).
```

2. **Migration tool for existing state:**
```erlang
migrate_to_commits() ->
    %% For each active case
    {ok, Cases} = wf_persistence:list_active_cases(),

    lists:foreach(fun(CaseMap) ->
        CaseId = maps:get(case_id, CaseMap),

        %% Load current state from old tables
        {ok, Case} = wf_persistence:load_case(CaseId),
        Marking = Case#wf_case.marking,
        UsrInfo = Case#wf_case.data,

        %% Create initial commit
        {ok, CommitId, _} = commit(CaseId, Marking, UsrInfo, []),

        logger:info("Migrated case ~p to commit ~p", [CaseId, CommitId])
    end, Cases).
```

**Estimated effort:** 7-10 days

## Open Questions

1. **Performance impact of Mnesia transactions on every step:** Mnesia transactions have overhead. Can we use dirty reads for hot path and transactions only for commits?
   - **Recommendation:** Benchmark with 1000+ commits/sec. If too slow, use ETS for in-memory commits with periodic Mnesia flush.

2. **Commit retention policy:** How many commits should we retain per case? Infinite log grows unbounded.
   - **Recommendation:** Retain all commits for audit, but compact old commits (merge into snapshot).

3. **Distributed commit coordination:** If case spans multiple nodes, how do we coordinate atomic commits?
   - **Recommendation:** Single case state manager process (could failover to another node via Mnesia replication).

4. **Backwards compatibility:** Existing workflows expect gen_yawl:marking/1 to return marking from process state.
   - **Recommendation:** gen_yawl:marking/1 becomes a wrapper that queries case_state_manager.

5. **Effect execution within commit transaction:** Should effects be executed before, during, or after commit?
   - **Recommendation:** Effects execute AFTER commit succeeds (effect receipts recorded in commit for compensating transactions).

6. **Commit granularity:** Is one commit per transition too granular? Should we batch multiple transitions?
   - **Recommendation:** One commit per transition ensures maximum recoverability. Batching can be optimization later.

7. **Conflict resolution:** What if two processes try to commit to the same case simultaneously?
   - **Recommendation:** Use Mnesia's built-in locking (first transaction wins, second aborts with {aborted, {no_transaction, _}}).

8. **Hash algorithm for receipts:** Current code uses SHA-256. Is this sufficient?
   - **Recommendation:** SHA-256 is sufficient (collision probability negligible). Consider BLAKE3 for performance.

9. **Parent commit pointer chain:** Should we use a linked list (parent pointer) or Merkle tree structure?
   - **Recommendation:** Start with linked list (simpler). Merkle tree if we need parallel branch support later.

10. **Testing strategy for crash recovery:** How do we verify atomic commits survive crashes?
    - **Recommendation:** Use chaotic testing (kill processes at random points) and verify state consistency via commit logs.

## Integration Points

**Depends on:**
- Item 001 (Centralize gen_yawl) - Eliminates dual execution paths
- Item 013 (Deterministic scheduling) - Provides RNG state for commits
- Item 016 (Effect system) - Effects need receipts in commits

**Blocks:**
- Item 017 (Structured tracing) - Traces need commit IDs for correlation
- Item 021 (Pattern implementations) - Patterns must use commit API
- Item 024 (Testing infrastructure) - Tests need to verify atomic commits

**Parallel work:**
- Can design commit protocol independently
- Can implement case_state_manager gen_server
- Migrations must wait for Item 001 completion
