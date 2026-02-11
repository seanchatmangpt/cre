# Per-case state management with atomic commits Implementation Plan

## Implementation Plan Title
Atomic Per-Case State Management with Single Source of Truth

## Overview

This plan implements atomic commits for workflow case state, eliminating distributed state fragmentation that creates inconsistency risks. The current CRE system has **5 separate state stores** (gen_pnet, gen_yawl, wf_engine, wf_persistence, yawl_recovery) that update independently, creating windows for partial state on crashes.

This implementation creates a **single source of truth per case** in Mnesia with atomic commit boundaries, while maintaining backwards compatibility through migration and adapter patterns.

## Current State

### State Duplication Problem

The codebase has critical state fragmentation:

1. **gen_pnet#net_state{}** (`/Users/sac/cre/include/gen_pnet.hrl:30-37`)
   - Stores `marking` and `usr_info` in process memory
   - Updated in `handle_cast(continue, ...)` at `/Users/sac/cre/src/core/gen_pnet.erl:717-731`
   - Lost if process crashes

2. **gen_yawl#wrapper_state{}** (`/Users/sac/cre/src/core/gen_yawl.erl:177-193`)
   - Wraps net_state, adds `checkpoint_interval`
   - Independent checkpointing via `yawl_recovery:maybe_checkpoint/5` (line 994-1000)
   - Checkpoints are periodic, NOT atomic with state updates

3. **wf_engine#wf_case{}** (`/Users/sac/cre/src/wf/wf_engine.hrl:27-40`)
   - **Duplicate marking** (line 35) - same as net_state.marking
   - Separate process from gen_yawl
   - Updated via `complete_workitem/6` (line 1240-1285)

4. **wf_persistence#wf_persistent_case{}** (`/Users/sac/cre/src/wf/wf_persistence.erl:85-94`)
   - Mnesia table with duplicate `marking` and `data`
   - Independent writes via `save_case/1` (line 254-279)
   - **No transaction** with net_state updates

5. **yawl_recovery#yawl_checkpoint{}** (`/Users/sac/cre/include/yawl_recovery.hrl:9-17`)
   - Checkpoint storage with duplicate `marking` and `data`
   - Triggered by step count, NOT state changes
   - No coordination with wf_persistence

### Update Flow Without Atomicity

Current workflow execution sequence:
```
1. gen_pnet fires transition → updates net_state.marking (process memory)
2. gen_yawl wrapper checks checkpoint_interval → may call yawl_recovery:checkpoint/4
3. wf_engine updates wf_case.marking (separate process)
4. wf_persistence writes to Mnesia (independent transaction)
```

**Problem:** Steps 1-4 are NOT atomic. If crash occurs at any point, state is inconsistent.

### Existing Patterns to Follow

**Mnesia Transaction Pattern** (`/Users/sac/cre/src/wf/yawl_recovery.erl:303-322`):
```erlang
Transaction = fun() ->
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

**Receipt Pattern** (`/Users/sac/cre/src/wf/wf_audit_log.erl:97-102`):
```erlang
-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := map(),
    ts := integer()
}.
```

## Desired End State

### Architecture

```
┌────────────────────────────────────────────────────────────┐
│                 case_state_manager (NEW)                   │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ - commit/4 (atomic state transition)                  │ │
│  │ - get_state/2 (read current state)                    │ │
│  │ - get_commit/2 (read specific commit)                 │ │
│  │ - rollback/2 (revert to previous commit)              │ │
│  │ - get_commit_history/1 (audit trail)                  │ │
│  └──────────────────────────────────────────────────────┘ │
└────────────────────────┬───────────────────────────────────┘
                         │ Mnesia transaction
                         ▼
┌────────────────────────────────────────────────────────────┐
│                    Mnesia Tables                           │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ case_commits (append-only log)                       │ │
│  │  - commit_id (PK)                                     │ │
│  │  - case_id (index)                                    │ │
│  │  - parent_commit_id                                   │ │
│  │  - marking                                            │ │
│  │  - usr_info                                           │ │
│  │  - rng_state                                          │ │
│  │  - transition (fired transition)                      │ │
│  │  - effects_receipt (side effects)                     │ │
│  │  - timestamp                                          │ │
│  │  - version                                            │ │
│  └──────────────────────────────────────────────────────┘ │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ case_latest (pointer to current state)               │ │
│  │  - case_id (PK)                                       │ │
│  │  - commit_id (FK to case_commits)                    │ │
│  └──────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────┘
```

### State Flow After Implementation

```
1. gen_yawl fires transition → produces new marking, usr_info
2. calls case_state_manager:commit(CaseId, Marking, UsrInfo, Effects)
3. case_state_manager runs Mnesia transaction:
   - Generate commit_id (UUID)
   - Read parent commit (from case_latest table)
   - Generate receipt with before/after hashes
   - Write commit record to case_commits
   - Update case_latest pointer
   - Return {ok, CommitId, Receipt}
4. gen_yawl continues with next step using new commit ID
```

**Key guarantee:** All state changes in ONE transaction. Either all succeed or all fail.

## What We're NOT Doing

- **NOT changing the Petri net execution model** - gen_pnet fire/3 remains unchanged
- **NOT modifying workflow specification language** - transitions, places unchanged
- **NOT changing external APIs** - wf_engine:complete/5 interface unchanged
- **NOT implementing distributed transactions** - single Mnesia node per case
- **NOT building a new storage engine** - using existing Mnesia infrastructure
- **NOT modifying receipt format** - existing before_hash/after_hash pattern preserved
- **NOT implementing multi-master replication** - Mnesia's built-in replication sufficient
- **NOT changing checkpoint semantics** - checkpoints become views of commit log

## Implementation Approach

### High-Level Strategy

**Incremental migration with backwards compatibility:**

1. **Phase 1:** Create case_state_manager module with commit protocol (no changes to existing code)
2. **Phase 2:** Migrate wf_engine to use case_state_manager (gen_yawl unchanged)
3. **Phase 3:** Migrate gen_yawl to use case_state_manager (gen_pnet unchanged)
4. **Phase 4:** Remove shadow state from wf_case and wrapper_state
5. **Phase 5:** Add rollback and replay capabilities

**Key design decisions:**
- **Mnesia as single source of truth** - processes become stateless workers
- **Append-only commit log** - all commits retained for audit
- **Pointer table for fast reads** - case_latest avoids scanning full log
- **Receipts IN commits** - every commit has exactly one receipt
- **UUID v4 for commit IDs** - collision-free, no central coordinator
- **SHA-256 for state hashes** - existing crypto:hash/2 calls preserved

---

## Phases

### Phase 1: Create case_state_manager Module

#### Overview
Create new module providing atomic commit API. No changes to existing code - this is the foundation that subsequent phases will build on.

#### Changes Required:

##### 1. Create Mnesia schema
**File**: `/Users/sac/cre/src/wf/case_state_manager.erl` (NEW)
**Changes**: Create new module with Mnesia table definitions and init function

```erlang
-module(case_state_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, commit/4, get_state/2, get_commit/2,
         get_commit_history/1, rollback/2, init_schema/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("case_state_manager.hrl").

%% Records
-record(case_commit, {
    commit_id :: binary(),
    case_id :: binary(),
    parent_commit_id :: binary() | undefined,
    marking :: pnet_types:marking(),
    usr_info :: term(),
    rng_state :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
    transition :: atom() | undefined,
    effects_receipt :: term() | undefined,
    timestamp :: integer(),
    version :: non_neg_integer()
}).

-record(case_latest, {
    case_id :: binary(),
    commit_id :: binary(),
    version :: non_neg_integer()
}).

%% Initialize Mnesia tables
init_schema() ->
    mnesia:create_table(case_commit, [
        {attributes, record_info(fields, case_commit)},
        {type, set},
        {disc_copies, [node()]},
        {index, [case_id]}
    ]),
    mnesia:create_table(case_latest, [
        {attributes, record_info(fields, case_latest)},
        {type, set},
        {disc_copies, [node()]}
    ]),
    mnesia:wait_for_tables([case_commit, case_latest], 5000).
```

##### 2. Implement commit/4 function
**File**: `/Users/sac/cre/src/wf/case_state_manager.erl`
**Changes**: Add commit API with Mnesia transaction

```erlang
%% Public API
-spec commit(CaseId :: binary(),
             Marking :: pnet_types:marking(),
             UsrInfo :: term(),
             Effects :: [term()]) ->
    {ok, CommitId :: binary(), Receipt :: map()} | {error, term()}.

commit(CaseId, Marking, UsrInfo, Effects) ->
    gen_server:call(?MODULE, {commit, CaseId, Marking, UsrInfo, Effects}).

%% gen_server callback
handle_call({commit, CaseId, Marking, UsrInfo, Effects}, _From, State) ->
    CommitId = generate_commit_id(),

    Transaction = fun() ->
        %% Get parent commit
        ParentCommit = case mnesia:read(case_latest, CaseId) of
            [#case_latest{commit_id = PCommitId, version = V}] ->
                case mnesia:read(case_commit, PCommitId) of
                    [PC] -> {PC, V};
                    [] -> mnesia:abort({parent_commit_not_found, PCommitId})
                end;
            [] ->
                {undefined, 0}
        end,

        {ParentCommitRecord, ParentVersion} = ParentCommit,

        %% Generate receipt
        BeforeHash = case ParentCommitRecord of
            undefined -> crypto:hash(sha256, <<>>);
            PC -> hash_commit(PC)
        end,

        CommitRecord = #case_commit{
            commit_id = CommitId,
            case_id = CaseId,
            parent_commit_id = case ParentCommitRecord of
                undefined -> undefined;
                PC -> PC#case_commit.commit_id
            end,
            marking = Marking,
            usr_info = UsrInfo,
            rng_state = rand:export_seed(),
            transition = extract_transition(Effects),
            effects_receipt = Effects,
            timestamp = erlang:system_time(millisecond),
            version = ParentVersion + 1
        },

        AfterHash = hash_commit(CommitRecord),

        %% Write commit
        ok = mnesia:write(case_commit, CommitRecord, write),

        %% Update latest pointer
        ok = mnesia:write(case_latest, #case_latest{
            case_id = CaseId,
            commit_id = CommitId,
            version = ParentVersion + 1
        }, write),

        Receipt = #{
            commit_id => CommitId,
            before_hash => BeforeHash,
            after_hash => AfterHash,
            effects => Effects,
            timestamp => CommitRecord#case_commit.timestamp
        },

        {ok, CommitId, Receipt}
    end,

    Reply = case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end,

    {reply, Reply, State};
```

##### 3. Implement read functions
**File**: `/Users/sac/cre/src/wf/case_state_manager.erl`
**Changes**: Add get_state, get_commit, get_commit_history APIs

```erlang
-spec get_state(CaseId :: binary(), Opts :: map()) ->
    {ok, pnet_types:marking(), term(), [term()]} | {error, term()}.

get_state(CaseId, _Opts) ->
    Transaction = fun() ->
        case mnesia:read(case_latest, CaseId) of
            [#case_latest{commit_id = CommitId}] ->
                [#case_commit{marking = M, usr_info = UI}] =
                    mnesia:read(case_commit, CommitId),
                {ok, M, UI, []};
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

-spec get_commit(CaseId :: binary(), CommitId :: binary()) ->
    {ok, #case_commit{}} | {error, term()}.

get_commit(_CaseId, CommitId) ->
    Transaction = fun() ->
        case mnesia:read(case_commit, CommitId) of
            [Commit] -> {ok, Commit};
            [] -> {error, not_found}
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

-spec get_commit_history(CaseId :: binary()) ->
    {ok, [#case_commit{}]} | {error, term()}.

get_commit_history(CaseId) ->
    Transaction = fun() ->
        Commits = mnesia:index_read(case_commit, CaseId, #case_commit.case_id),
        {ok, lists:sort(fun(C1, C2) ->
            C1#case_commit.version < C2#case_commit.version
        end, Commits)}
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.
```

##### 4. Implement rollback/2 function
**File**: `/Users/sac/cre/src/wf/case_state_manager.erl`
**Changes**: Add rollback API

```erlang
-spec rollback(CaseId :: binary(), TargetCommitId :: binary()) ->
    ok | {error, term()}.

rollback(CaseId, TargetCommitId) ->
    Transaction = fun() ->
        %% Verify target commit exists and belongs to this case
        case mnesia:read(case_commit, TargetCommitId) of
            [#case_commit{case_id = CaseId}] -> ok;
            [] -> mnesia:abort(commit_not_found);
            [#case_commit{case_id = OtherId}] ->
                mnesia:abort({commit_belongs_to_other_case, OtherId})
        end,

        %% Update case_latest pointer (rewind history)
        [#case_commit{version = TargetVersion}] =
            mnesia:read(case_commit, TargetCommitId),

        ok = mnesia:write(case_latest, #case_latest{
            case_id = CaseId,
            commit_id = TargetCommitId,
            version = TargetVersion
        }, write)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.
```

##### 5. Add helper functions
**File**: `/Users/sac/cre/src/wf/case_state_manager.erl`
**Changes**: Private functions for hash generation and commit ID generation

```erlang
%% Private helpers
generate_commit_id() ->
    UUID = uuid_v4(),
    binary:encode_hex(UUID).

uuid_v4() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    <<A:32, B:16, (C band 16#0fff) bor 16#4000:16,
      (D band 16#3fff) bor 16#8000:16, E:48>>.

hash_commit(#case_commit{} = Commit) ->
    Data = term_to_binary({
        Commit#case_commit.marking,
        Commit#case_commit.usr_info,
        Commit#case_commit.rng_state
    }),
    crypto:hash(sha256, Data).

extract_transition(Effects) ->
    case lists:keyfind(transition, 1, Effects) of
        {transition, Trsn} -> Trsn;
        false -> undefined
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 compile` succeeds in `/Users/sac/cre`
- [ ] `rebar3 eunit --module=case_state_manager` passes all unit tests
- [ ] `rebar3 dialyzer` shows no errors for case_state_manager
- [ ] Mnesia tables created successfully: `mnesia:table_info(case_commit, where_to_read)` returns node

##### Manual Verification:
- [ ] Start case_state_manager: `case_state_manager:start_link()`
- [ ] Initialize schema: `case_state_manager:init_schema()`
- [ ] Create test commit: `{ok, CommitId, _} = case_state_manager:commit(<<"test1">>, #{}, #{}, [])`
- [ ] Read state back: `{ok, M, UI, _} = case_state_manager:get_state(<<"test1">>, #{})`
- [ ] Verify commit history: `{ok, History} = case_state_manager:get_commit_history(<<"test1">>)`
- [ ] Test rollback: `ok = case_state_manager:rollback(<<"test1">>, CommitId)`

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Integrate case_state_manager into wf_engine

#### Overview
Modify wf_engine to use case_state_manager for all state mutations. gen_yawl remains unchanged in this phase. This creates a hybrid state model where wf_engine uses commits but gen_yawl still uses in-memory state.

#### Changes Required:

##### 1. Modify wf_engine gen_server state
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Add case_state_manager reference to engine_state

```erlang
%% Modify engine_state record to track commit-based cases
-record(engine_state, {
    spec :: map(),
    cases :: #{binary() => #wf_case{}},
    seq :: non_neg_integer(),
    rng_state :: rand:state(),
    case_state_manager :: pid() | undefined  % NEW FIELD
}).

%% Update init/1 to start case_state_manager
init(Args) ->
    Spec = maps:get(spec, Args),
    Seed = maps:get(seed, Args, 1),
    RngState = rand:seed(exs1024, Seed),

    %% Start case_state_manager if not already running
    {ok, CSM} = case whereis(case_state_manager) of
        undefined -> case_state_manager:start_link();
        Pid -> {ok, Pid}
    end,

    State = #engine_state{
        spec = Spec,
        cases = #{},
        seq = 0,
        rng_state = RngState,
        case_state_manager = CSM
    },

    {ok, State}.
```

##### 2. Modify start_case/3 to create initial commit
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Create initial commit when starting a case

```erlang
%% Find handle_call({start_case, ...}) around line 850
handle_call({start_case, Opts, Now}, _From, State) ->
    CaseId = generate_case_id(State#engine_state.seq),
    Spec = State#engine_state.spec,
    Data = maps:get(data, Opts, #{}),

    %% Get initial marking from spec
    InitMarking = maps:get(initial_marking, Spec, #{p_start => [init]}),

    %% Create initial commit (atomic)
    CommitResult = case_state_manager:commit(CaseId, InitMarking, Data, []),
    case CommitResult of
        {ok, FirstCommitId, _} ->
            %% Create case record (minimal state, marking in commit)
            Case = #wf_case{
                case_id = CaseId,
                status = running,
                work_items = #{},
                data = Data,
                receipts = [],
                events = [],
                log = [],
                marking = undefined,  % Will be fetched from commits
                rng_state = rand:export_seed(),
                scheduled_at = undefined,
                timestamps = #{
                    created_at => Now,
                    updated_at => Now
                }
            },

            %% Process enabled transitions
            Case1 = process_enabled(Case, Spec, Now),

            Cases = maps:put(CaseId, Case1, State#engine_state.cases),
            {reply, {ok, CaseId}, State#engine_state{cases = Cases, seq = State#engine_state.seq + 1}};
        {error, Reason} ->
            {reply, {error, {commit_failed, Reason}}, State}
    end;
```

##### 3. Modify complete_workitem/6 to use commits
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Replace direct state mutation with commit

```erlang
%% Around line 1240 - replace complete_workitem implementation
complete_workitem(#wf_case{case_id = CaseId, work_items = WIs, data = CaseData} = Case,
                   WiId, Task, Data, Now, State) ->

    %% Update work item status (still in memory, committed separately)
    WI = maps:get(WiId, WIs),
    WI1 = WI#work_item{status = completed},
    WIs1 = maps:put(WiId, WI1, WIs),

    %% Merge data into case data
    NewCaseData = maps:merge(CaseData, Data),

    %% Load current state from case_state_manager
    {ok, CurrentMarking, CurrentUsrInfo, _} =
        case_state_manager:get_state(CaseId, #{}),

    %% Create completion receipt
    BeforeHash = pnet_marking:hash(CurrentMarking),

    %% Fire the transition for this task
    Spec = State#engine_state.spec,
    ProduceMap = fire_transition(Task, Data, Spec, true),

    %% Update marking
    Marking1 = apply_produce_map(CurrentMarking, ProduceMap),
    AfterHash = pnet_marking:hash(Marking1),

    %% Prepare effects for commit
    Effects = [
        {transition, Task},
        {work_item_completed, WiId},
        {produce_map, ProduceMap}
    ],

    %% ATOMIC COMMIT: Write new state atomically
    CommitResult = case_state_manager:commit(
        CaseId, Marking1, NewCaseData, Effects
    ),

    case CommitResult of
        {ok, CommitId, _CommitReceipt} ->
            %% Create legacy receipt for backwards compatibility
            Move = #{
                trsn => Task,
                mode => #{},
                produce => ProduceMap
            },
            LegacyReceipt = pnet_receipt:make(BeforeHash, AfterHash, Move),

            %% Update case (marking now undefined, fetched from commits)
            Case1 = Case#wf_case{
                work_items = WIs1,
                marking = undefined,  % NOT stored here anymore
                data = NewCaseData,
                receipts = [LegacyReceipt | Case#wf_case.receipts],
                timestamps = maps:put(updated_at, Now, Case#wf_case.timestamps)
            },

            %% Check for completion
            Case2 = check_completion(Case1, Spec, Now),

            %% Process enabled transitions
            Case3 = process_enabled(Case2, Spec, Now),

            Cases = maps:put(Case3#wf_case.case_id, Case3, State#engine_state.cases),
            {Case3, State#engine_state{cases = Cases}};
        {error, Reason} ->
            logger:error("Failed to commit work item completion: ~p", [Reason]),
            %% Restore old state
            {Case, State}
    end.
```

##### 4. Add state accessor helper
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Add helper to load marking from commits

```erlang
%% Private helper
-spec load_case_marking(binary()) -> pnet_types:marking() | undefined.

load_case_marking(CaseId) ->
    case case_state_manager:get_state(CaseId, #{}) of
        {ok, Marking, _UsrInfo, _} -> Marking;
        {error, _} -> undefined
    end.
```

##### 5. Update functions that read marking
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Update process_enabled and other functions to call load_case_marking

```erlang
%% Around line 1315 - modify process_enabled to load marking
process_enabled(#wf_case{case_id = CaseId} = Case, Spec, Now) ->
    %% Load current marking from commits
    Marking = load_case_marking(CaseId),

    Transitions = maps:get(transitions, Spec, #{}),
    Enabled = lists:filter(
        fun(Trsn) -> is_transition_enabled(Trsn, Marking, Spec) end,
        maps:keys(Transitions)
    ),

    %% Fire one enabled transition (non-deterministic choice)
    case Enabled of
        [] ->
            Case;
        _ ->
            RngState = case Case#wf_case.rng_state of
                undefined -> rand:seed(exs1024, 1);
                RS -> RS
            end,
            Trsn = pick_transition(Enabled, RngState),
            fire_enabled_transition(Case, Trsn, Spec, Now)
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit --module=wf_engine` passes all existing tests
- [ ] `rebar3 eunit --module=case_state_manager` still passes
- [ ] No dialyzer warnings

##### Manual Verification:
- [ ] Start wf_engine with test spec
- [ ] Start a case: `{ok, CaseId} = wf_engine:start_case(Eng, #{data => #{}}, 0)`
- [ ] Verify commit created: `{ok, History} = case_state_manager:get_commit_history(CaseId)` shows 1 commit
- [ ] Complete a work item: `ok = wf_engine:complete(Eng, WiId, alice, #{}, 1)`
- [ ] Verify commit count: History now shows 2 commits
- [ ] Check state consistency: Marking in commit matches what wf_engine would have calculated
- [ ] Crash recovery: Kill wf_engine process, restart, verify case state loads from commits

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Migrate gen_yawl to use case_state_manager

#### Overview
Modify gen_yawl to use case_state_manager for commits instead of periodic checkpoints. This eliminates the independent checkpoint path and ensures all state transitions go through the atomic commit protocol.

#### Changes Required:

##### 1. Modify gen_yawl init to extract case_id
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`
**Changes**: Ensure net_arg contains case_id for commits

```erlang
%% Around line 430 - modify init/1
init({NetMod, NetArg, Options}) ->
    ...existing init code...

    %% Extract case_id from net_arg for state management
    CaseId = maps:get(case_id, NetArg, undefined),

    WrapperState = #wrapper_state{
        net_mod = NetMod,
        net_state = InitialNetState,
        net_arg = NetArg,
        fire_timeout = maps:get(fire_timeout, Options, 5000),
        progress_timeout = maps:get(progress_timeout, Options, 30000),
        shutting_down = false,
        active_fires = 0,
        marking_history = [],
        max_marking_history = maps:get(max_marking_history, Options, 10),
        continue_count = 0,
        max_continue = maps:get(max_continue, Options, 1000),
        regions = maps:get(regions, Options, #{}),
        checkpoint_interval = 0,  % DISABLED - using commits now
        drain_step_count = 0
    },

    {ok, WrapperState}.
```

##### 2. Replace checkpoint_interval with commit-based approach
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`
**Changes**: Remove maybe_checkpoint/5 call, add commit call

```erlang
%% Around line 993 - replace checkpoint logic with commit
handle_cast(continue,
            WrapperState = #wrapper_state{
                              net_arg = NetArg,
                              net_state = NetState0 = #net_state{
                                                        stats = Stats,
                                                        tstart = T1,
                                                        cnt = Cnt,
                                                        marking = OldMarking,
                                                        usr_info = OldUsrInfo
                                                       },
                              marking_history = History,
                              max_marking_history = MaxHistory,
                              continue_count = ContCount,
                              max_continue = MaxCont
                             }) ->

    %% ... existing cycle detection and timeout checks ...

    case progress(NetState0, FireTimeout) of
        abort ->
            %% ... existing abort handling ...
            {noreply, WrapperState#wrapper_state{
                marking_history = [],
                continue_count = 0
            }};

        {delta, Mode, Pm, NewUsrInfo} ->
            %% ... existing elapsed timeout checks ...

            %% Update net state with consumed tokens
            NetState1 = cns(Mode, NetState0),

            %% Update user info if provided by fire/3 3-tuple return
            NetState2 = case NewUsrInfo of
                undefined -> NetState1;
                _ -> NetState1#net_state{usr_info = NewUsrInfo}
            end,

            %% Handle trigger and produce tokens
            NetMod = WrapperState#wrapper_state.net_mod,
            NetState3 = handle_trigger(Pm, NetState2, NetMod),

            %% NEW: Atomic commit instead of periodic checkpoint
            #net_state{marking = NewMarking, usr_info = FinalUsrInfo} = NetState3,

            CaseId = maps:get(case_id, NetArg, undefined),

            NetState4 = case CaseId of
                undefined ->
                    %% No case_id - skip commit (e.g. ad-hoc nets)
                    NetState3;
                _ ->
                    %% Extract transition info from Pm
                    Transition = extract_transition_from_pm(Pm),

                    %% Prepare effects
                    Effects = [
                        {transition, Transition},
                        {mode, Mode},
                        {produce_map, Pm}
                    ],

                    %% ATOMIC COMMIT
                    case case_state_manager:commit(
                        CaseId, NewMarking, FinalUsrInfo, Effects
                    ) of
                        {ok, _CommitId, _Receipt} ->
                            %% Commit successful, continue with new state
                            NetState3;
                        {error, Reason} ->
                            logger:error("Commit failed: ~p, halting net", [Reason]),
                            %% Halt execution on commit failure
                            NetState3#net_state{marking = OldMarking}
                    end
            end,

            %% ... existing cycle detection and continue logic ...

            {noreply, WrapperState#wrapper_state{
                net_state = NetState4,
                marking_history = NewHistory,
                continue_count = ContCount + 1
            }}
    end;
```

##### 3. Add helper to extract transition from produce map
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`
**Changes**: Add private helper function

```erlang
%% Private helper
extract_transition_from_pm(Pm) ->
    case maps:is_key(trsn, Pm) of
        true -> maps:get(trsn, Pm);
        false -> undefined
    end.
```

##### 4. Remove checkpoint_interval handling
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`
**Changes**: Remove checkpoint_interval from wrapper_state (already done in init above)

**Actions:**
- Remove `checkpoint_interval` field from `#wrapper_state{}` record (line 191)
- Remove checkpoint handling from `handle_cast(continue, ...)` (lines 994-1000)
- Remove `yawl_recovery:maybe_checkpoint/5` import if present

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit --module=gen_yawl` passes all tests
- [ ] `rebar3 eunit --module=yawl_recovery` still passes (if tests exist)
- [ ] No dialyzer warnings

##### Manual Verification:
- [ ] Start gen_yawl net with case_id: `{ok, Pid} = gen_yawl:start_link(test_net, #{case_id => <<"test1">>}, [])`
- [ ] Trigger transitions: `gen_yawl:cast(Pid, continue)`
- [ ] Verify commits created: `{ok, History} = case_state_manager:get_commit_history(<<"test1">>)` shows N commits
- [ ] Verify no checkpoints created: `yawl_recovery:list_checkpoints(<<"spec">>, <<"test1">>)` returns empty or error
- [ ] Crash recovery: Kill gen_yawl process, restart, verify state resumes from latest commit

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 4.

---

### Phase 4: Remove Shadow State

#### Overview
Eliminate duplicate state storage. Now that all state updates go through case_state_manager, remove marking from wf_case and net_state records. These become references to commit-based state.

#### Changes Required:

##### 1. Remove marking from wf_case record
**File**: `/Users/sac/cre/src/wf/wf_engine.hrl`
**Changes**: Remove marking field (or mark as deprecated)

```erlang
-record(wf_case, {
    case_id :: binary(),
    status :: pending | running | suspended | cancelled | completed | failed | scheduled,
    work_items = #{} :: #{binary() => #work_item{}},
    data = #{} :: map(),
    receipts = [] :: [term()],
    events = [] :: [term()],
    log = [] :: [term()],
    %% marking :: map(),  %% REMOVED - stored in case_state_manager commits
    rng_state :: {non_neg_integer(), non_neg_integer(), non_neg_integer()} |
                {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()},
    scheduled_at :: integer() | undefined,
    timestamps :: map()
}).
```

##### 2. Update all wf_case construction
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Remove marking field from all #wf_case{} constructions

**Find all occurrences:**
- `start_case/3` handler (around line 850)
- `complete_workitem/6` (around line 1270)
- `check_completion/3` (around line 1295)
- `process_enabled/3` (around line 1313)

**Pattern to apply:**
```erlang
%% BEFORE:
Case1 = Case#wf_case{
    work_items = WIs1,
    marking = Marking1,  %% REMOVE THIS LINE
    data = NewCaseData,
    ...
},

%% AFTER:
Case1 = Case#wf_case{
    work_items = WIs1,
    data = NewCaseData,
    ...
},
```

##### 3. Update functions that read wf_case.marking
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Replace marking reads with load_case_marking/1 calls

**Find all occurrences:**
- `check_completion/3` (line 1295): `Case#wf_case.marking` → `load_case_marking(Case#wf_case.case_id)`
- `process_enabled/3` (line 1313): `Case#wf_case.marking` → `load_case_marking(Case#wf_case.case_id)`
- `fire_enabled_transition/5` (line 1399): Marking parameter
- `is_transition_enabled/3` (line 1359): Marking parameter
- `inject_service_reply/4` (line 1569): `Case#wf_case.marking` → `load_case_marking(Case#wf_case.case_id)`

##### 4. Update wf_persistence to not persist marking
**File**: `/Users/sac/cre/src/wf/wf_persistence.erl`
**Changes**: Remove marking from persistent storage, or store as reference

```erlang
%% Option A: Remove marking entirely (recommended)
-record(wf_persistent_case, {
    case_id :: binary(),
    status :: running | suspended | cancelled | completed | failed | scheduled,
    %% marking :: map(),  %% REMOVED - in case_commits table
    data :: map(),
    receipts :: [map()],
    rng_state :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
    timestamps :: map(),
    scheduled_at :: integer() | undefined
}).

%% Update save_case/1 to skip marking
save_case(#wf_case{case_id = CaseId} = Case) ->
    %% Load latest commit ID instead of marking
    {ok, CommitId} = case case_state_manager:get_state(CaseId, #{}) of
        {ok, _Marking, _UsrInfo, _} ->
            %% Get commit ID from case_latest table
            Transaction = fun() ->
                case mnesia:read(case_latest, CaseId) of
                    [#case_latest{commit_id = CID}] -> CID;
                    [] -> undefined
                end
            end,
            mnesia:transaction(Transaction);
        {error, _} ->
            undefined
    end,

    PersistentCase = #wf_persistent_case{
        case_id = CaseId,
        status = Case#wf_case.status,
        %% marking = ...,  %% REMOVED
        data = Case#wf_case.data,
        receipts = serialize_receipts(Case#wf_case.receipts),
        rng_state = serialize_rng_state(Case#wf_case.rng_state),
        timestamps = Case#wf_case.timestamps,
        scheduled_at = Case#wf_case.scheduled_at,
        latest_commit_id = CommitId  %% NEW: reference to commit
    },

    ... rest of save_case/1 ...
```

##### 5. Remove marking from net_state (optional, Phase 4b)
**File**: `/Users/sac/cre/include/gen_pnet.hrl`
**Changes**: Consider removing marking from net_state (deferred to future phase)

**Rationale:** gen_pnet is a generic Petri net engine and may be used outside workflow context. Keep marking in net_state for now, but document that for workflow cases, it's a cache of the commit's marking.

**Decision:** DEFER - marking stays in net_state for Phase 4

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit --module=wf_engine` passes all tests
- [ ] `rebar3 eunit --module=wf_persistence` passes all tests
- [ ] No dialyzer warnings about undefined marking fields

##### Manual Verification:
- [ ] Run full workflow: start case → complete tasks → verify completion
- [ ] Check wf_case records: `wf_engine:case_state(Eng, CaseId)` shows correct status
- [ ] Verify marking not in wf_case: Inspect `#wf_case{}` record, marking field missing/undefined
- [ ] Verify marking in commits: `case_state_manager:get_commit_history(CaseId)` shows all markings
- [ ] Persistence check: `wf_persistence:save_case(Case)` succeeds without marking
- [ ] Recovery check: Restart wf_engine, verify case loads from commit reference

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 5.

---

### Phase 5: Add Rollback and Replay Support

#### Overview
Now that we have an append-only commit log, add ability to rollback to previous commits and replay from any point. This enables powerful debugging and recovery capabilities.

#### Changes Required:

##### 1. Implement replay_from_commit/2
**File**: `/Users/sac/cre/src/wf/case_state_manager.erl`
**Changes**: Add replay function to restore state and resume execution

```erlang
%% Public API
-spec replay_from_commit(CaseId :: binary(), CommitId :: binary()) ->
    {ok, pnet_types:marking(), term(), rand:state()} | {error, term()}.

replay_from_commit(CaseId, CommitId) ->
    Transaction = fun() ->
        case mnesia:read(case_commit, CommitId) of
            [#case_commit{case_id = CaseId} = Commit] ->
                %% Restore marking and usr_info
                Marking = Commit#case_commit.marking,
                UsrInfo = Commit#case_commit.usr_info,

                %% Restore RNG state
                RngState = case Commit#case_commit.rng_state of
                    {A, B, C} when is_integer(A), is_integer(B), is_integer(C) ->
                        rand:seed(exs1024, {A, B, C});
                    {A, B, C, D} ->
                        rand:seed(exsplus, {A, B, C, D});
                    _ ->
                        rand:seed(exs1024, 1)
                end,

                {ok, Marking, UsrInfo, RngState};
            [] ->
                mnesia:abort(commit_not_found);
            [#case_commit{case_id = OtherId}] ->
                mnesia:abort({commit_belongs_to_other_case, OtherId})
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.
```

##### 2. Add verify_replay/1 for deterministic replay testing
**File**: `/Users/sac/cre/src/wf/case_state_manager.erl`
**Changes**: Add function to verify replay produces same result

```erlang
%% Public API
-spec verify_replay(CaseId :: binary()) -> boolean().

verify_replay(CaseId) ->
    %% Get commit history
    {ok, Commits} = get_commit_history(CaseId),

    case length(Commits) of
        0 ->
            true;
        N when N > 1 ->
            %% Replay from first commit
            FirstCommit = lists:last(Commits),  % Oldest commit
            {ok, Marking, UsrInfo, _Rng} =
                replay_from_commit(CaseId, FirstCommit#case_commit.commit_id),

            %% Verify we can reach the latest commit
            %% (This is a simplified check - full replay would re-execute)
            LatestCommit = lists:nth(1, Commits),  % Newest commit

            %% Hash comparison
            FirstHash = hash_commit(FirstCommit),
            LatestHash = hash_commit(LatestCommit),

            %% For now, just verify commit chain is intact
            %% Full execution replay requires engine support
            verify_commit_chain(Commits)
    end.

%% Private helper
verify_commit_chain([_Single]) ->
    true;
verify_commit_chain([Commit1, Commit2 | Rest]) ->
    %% Verify Commit2's parent points to Commit1
    case Commit2#case_commit.parent_commit_id of
        Commit1#case_commit.commit_id ->
            verify_commit_chain([Commit2 | Rest]);
        _Other ->
            false
    end.
```

##### 3. Add rollback API to wf_engine
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Expose rollback through wf_engine API

```erlang
%% Public API (around line 500)
-spec rollback_case(Engine :: pid() | atom(), CaseId :: binary(), CommitId :: binary()) ->
    ok | {error, term()}.

rollback_case(Engine, CaseId, CommitId) ->
    gen_server:call(Engine, {rollback_case, CaseId, CommitId}).

%% Handle call (add to handle_call)
handle_call({rollback_case, CaseId, CommitId}, _From, State) ->
    case maps:get(CaseId, State#engine_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #wf_case{} = Case ->
            %% Delegate to case_state_manager
            case case_state_manager:rollback(CaseId, CommitId) of
                ok ->
                    %% Reload case state from new commit
                    {ok, NewMarking, NewUsrInfo, _} =
                        case_state_manager:get_state(CaseId, #{}),

                    %% Update case (data reset to commit point)
                    Case1 = Case#wf_case{
                        data = NewUsrInfo,
                        receipts = [],  % Clear in-memory receipts
                        events = [],
                        timestamps = maps:put(
                            rolled_back_at,
                            erlang:system_time(millisecond),
                            Case#wf_case.timestamps
                        )
                    },

                    Cases = maps:put(CaseId, Case1, State#engine_state.cases),
                    {reply, ok, State#engine_state{cases = Cases}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;
```

##### 4. Add get_commit_history API to wf_engine
**File**: `/Users/sac/cre/src/wf/wf_engine.erl`
**Changes**: Expose commit history through wf_engine

```erlang
%% Public API
-spec commit_history(Engine :: pid() | atom(), CaseId :: binary()) ->
    {ok, [term()]} | {error, term()}.

commit_history(Engine, CaseId) ->
    gen_server:call(Engine, {commit_history, CaseId}).

%% Handle call
handle_call({commit_history, CaseId}, _From, State) ->
    Reply = case maps:get(CaseId, State#engine_state.cases, undefined) of
        undefined ->
            {error, not_found};
        #wf_case{} ->
            case case_state_manager:get_commit_history(CaseId) of
                {ok, Commits} ->
                    %% Convert to external format (hide internal details)
                    History = lists:map(fun(C) ->
                        #{
                            commit_id => C#case_commit.commit_id,
                            parent_commit_id => C#case_commit.parent_commit_id,
                            transition => C#case_commit.transition,
                            timestamp => C#case_commit.timestamp,
                            version => C#case_commit.version
                        }
                    end, Commits),
                    {ok, History};
                {error, Reason} ->
                    {error, Reason}
            end
    end,
    {reply, Reply, State}.
```

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit --module=case_state_manager` passes all tests including new replay tests
- [ ] `rebar3 eunit --module=wf_engine` passes all tests including new rollback tests
- [ ] Property-based test for verify_replay/1 passes 1000 runs

##### Manual Verification:
- [ ] Create workflow with multiple steps
- [ ] Get commit history: `{ok, History} = wf_engine:commit_history(Eng, CaseId)`
- [ ] Note second commit ID: `SecondCommitId = lists:nth(2, History)`
- [ ] Rollback: `ok = wf_engine:rollback_case(Eng, CaseId, SecondCommitId)`
- [ ] Verify state: Case state matches second commit
- [ ] Execute new transition: Verify new commits branch from rollback point
- [ ] Verify commit chain: `case_state_manager:verify_replay(CaseId)` returns true
- [ ] Test replay: `case_state_manager:replay_from_commit(CaseId, FirstCommitId)` returns correct state

**Note**: Complete all automated verification, then pause for manual confirmation. This is the final phase.

---

## Testing Strategy

### Unit Tests

#### case_state_manager tests:
```erlang
%% test/case_state_manager_tests.erl
commit_atomic_test() ->
    %% Verify commit is atomic
    CaseId = <<"test_atomic">>,
    {ok, C1, _} = case_state_manager:commit(CaseId, #{p => [t]}, #{}, []),
    {ok, C2, _} = case_state_manager:commit(CaseId, #{p => [t], q => [t]}, #{}, []),

    %% Verify parent chain
    {ok, Commit2} = case_state_manager:get_commit(CaseId, C2),
    ?assertEqual(C1, Commit2#case_commit.parent_commit_id).

rollback_test() ->
    %% Verify rollback works
    CaseId = <<"test_rollback">>,
    {ok, C1, _} = case_state_manager:commit(CaseId, #{p => [t]}, #{}, []),
    {ok, C2, _} = case_state_manager:commit(CaseId, #{p => [t], q => [t]}, #{}, []),

    ok = case_state_manager:rollback(CaseId, C1),

    %% Verify latest is C1
    {ok, State} = case_state_manager:get_state(CaseId, #{}),
    ?assertEqual(#{p => [t]}, element(1, State)).
```

#### wf_engine integration tests:
```erlang
%% test/wf_engine_commit_tests.erl
commit_on_complete_test() ->
    %% Verify complete/5 creates commit
    {ok, Eng} = wf_engine:start_link(#{spec => test_spec(), seed => 1}),
    {ok, CaseId} = wf_engine:start_case(Eng, #{data => #{}}, 0),

    %% Complete work item
    [WI] = wf_engine:worklist(Eng, alice),
    WiId = maps:get(wi, WI),
    ok = wf_engine:allocate(Eng, WiId, alice, 0),
    ok = wf_engine:start_work(Eng, WiId, alice, 0),
    ok = wf_engine:complete(Eng, WiId, alice, #{}, 1),

    %% Verify commits created
    {ok, History} = case_state_manager:get_commit_history(CaseId),
    ?assert(length(History) >= 2).  % Initial + completion
```

### Integration Tests

#### Crash recovery test:
```erlang
crash_recovery_test() ->
    %% Start case, execute steps, crash engine, restart
    {ok, Eng} = wf_engine:start_link(#{spec => test_spec(), seed => 1}),
    {ok, CaseId} = wf_engine:start_case(Eng, #{data => #{}}, 0),

    %% Execute some steps
    execute_work_items(Eng, 3),

    %% Get commit count before crash
    {ok, History1} = case_state_manager:get_commit_history(CaseId),
    Count1 = length(History1),

    %% Crash engine
    exit(Eng, kill),

    %% Restart
    timer:sleep(100),
    {ok, Eng2} = wf_engine:start_link(#{spec => test_spec(), seed => 1}),

    %% Verify state recovered
    running = wf_engine:case_state(Eng2, CaseId),

    %% Continue execution
    execute_work_items(Eng2, 2),

    %% Verify commits continued
    {ok, History2} = case_state_manager:get_commit_history(CaseId),
    Count2 = length(History2),
    ?assert(Count2 > Count1).
```

### Property-Based Tests

#### Commits form valid chain:
```erlang
prop_commit_chain_valid() ->
    ?FORALL({Markings, Data},
        {non_empty(list(marking_gen())), map_gen()},
        begin
            CaseId = <<"prop_test">>,
            CommitIds = [begin
                {ok, Cid, _} = case_state_manager:commit(
                    CaseId, M, D, []),
                Cid
            end || {M, D} <- lists:zip(Markings, Data)],

            %% Verify chain
            {ok, Commits} = case_state_manager:get_commit_history(CaseId),
            verify_commit_chain(lists:reverse(Commits))
        end).

verify_commit_chain([_Single]) ->
    true;
verify_commit_chain([C1, C2 | Rest]) ->
    C2#case_commit.parent_commit_id =:= C1#case_commit.commit_id
        andalso verify_commit_chain([C2 | Rest]).
```

### Manual Testing Steps

1. **Start workflow:**
   ```erlang
   {ok, Eng} = wf_engine:start_link(#{spec => my_spec, seed => 1}),
   {ok, CaseId} = wf_engine:start_case(Eng, #{data => #{}}, 0).
   ```

2. **Execute steps:**
   ```erlang
   [WI] = wf_engine:worklist(Eng, alice),
   WiId = maps:get(wi, WI),
   ok = wf_engine:complete(Eng, WiId, alice, #{result => ok}, 1).
   ```

3. **Check commits:**
   ```erlang
   {ok, History} = wf_engine:commit_history(Eng, CaseId),
   length(History).  % Should be 2 (initial + completion)
   ```

4. **Test rollback:**
   ```erlang
   FirstCommit = lists:last(History),
   ok = wf_engine:rollback_case(Eng, CaseId, FirstCommit#commit_id).
   ```

5. **Verify replay:**
   ```erlang
   true = case_state_manager:verify_replay(CaseId).
   ```

## Migration Notes

### Data Migration for Existing Cases

When deploying this change to production with existing active cases:

1. **Run migration before starting new engines:**
   ```erlang
   %% Migration script
   migrate_existing_cases() ->
       {ok, Cases} = wf_persistence:list_all_cases(),
       lists:foreach(fun(CaseMap) ->
           CaseId = maps:get(case_id, CaseMap),
           migrate_case(CaseId)
       end, Cases).

   migrate_case(CaseId) ->
       case wf_persistence:load_case(CaseId) of
           {ok, #wf_case{marking = Marking, data = Data}} ->
               %% Create initial commit from current state
               {ok, CommitId, _} = case_state_manager:commit(
                   CaseId, Marking, Data, [{migration, true}]
               ),
               logger:info("Migrated case ~p to commit ~p", [CaseId, CommitId]);
           {error, not_found} ->
               logger:warning("Case ~p not found during migration", [CaseId])
       end.
   ```

2. **Handle yawl_checkpoint table:**
   - Keep table for backwards compatibility (read-only)
   - New code uses case_commits
   - Old checkpoints remain accessible but not updated

3. **Backwards compatibility:**
   - wf_case:marking/1 becomes a wrapper that calls case_state_manager
   - gen_yawl:marking/1 returns cached marking from net_state (accepting staleness)

### Rollback Plan

If deployment fails:

1. **Revert code:** Deploy previous version without case_state_manager
2. **Revert Mnesia schema:** Keep case_commits table (don't delete), but old code ignores it
3. **Data consistency:** Old wf_persistent_case table still has marking, so old code can read it

**No data loss risk** - old tables remain untouched, new tables are additive.

## References

### Research
- `/Users/sac/cre/.wreckit/items/019-per-case-state-management-with-atomic-commits/research.md`

### State Records
- `/Users/sac/cre/include/gen_pnet.hrl:30-37` - #net_state{} definition
- `/Users/sac/cre/src/core/gen_yawl.erl:177-193` - #wrapper_state{} definition
- `/Users/sac/cre/src/wf/wf_engine.hrl:27-40` - #wf_case{} definition
- `/Users/sac/cre/include/yawl_recovery.hrl:9-17` - #yawl_checkpoint{} definition

### State Mutation
- `/Users/sac/cre/src/core/gen_pnet.erl:717-731` - handle_cast(continue, ...)
- `/Users/sac/cre/src/core/gen_yawl.erl:928-1048` - wrapper continue loop
- `/Users/sac/cre/src/wf/wf_engine.erl:1240-1285` - complete_workitem/6

### Persistence
- `/Users/sac/cre/src/wf/wf_persistence.erl:254-279` - save_case/1
- `/Users/sac/cre/src/wf/yawl_recovery.erl:303-322` - checkpoint transaction pattern

### Receipts
- `/Users/sac/cre/src/wf/wf_audit_log.erl:97-102` - receipt type definition
