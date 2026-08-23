# Effect System with Receipts Implementation Plan

## Implementation Plan Title
Unique Causal IDs, Idempotency, and Receipt Persistence for Effect System

## Overview
Implement a production-ready effect system for the Linear Nesting workflow execution engine that provides globally unique causal IDs, idempotency guarantees, receipt persistence, compensation support, and full VM integration. This enables pure workflow reduction with controlled interaction to the external world, deterministic replay, and audit trails.

## Current State
The codebase has a basic effect boundary implementation (`ln_effect`) and receipt system (`ln_receipt`) with critical gaps:

**Existing Components:**
- `ln_effect.erl` (lines 1-177): Basic effect request/complete lifecycle
- `ln_receipt.erl` (lines 1-285): Receipt generation with SHA-256 hashing and ETS storage
- `ln_vm.erl` (lines 1-233): Bytecode executor with no effect integration
- `ln_compile.erl` (lines 1-241): Compiler with no effect opcodes
- `ln_plan.erl` (lines 1-195): Plan constructors with no effect term
- `ln_trace.erl` (lines 1-138): Tracing with effect event types defined but not emitted
- `ln_cancel.erl` (lines 1-733): Hierarchical cancellation for effect scope integration

**Critical Gaps:**
1. **No globally unique causal IDs** - `ln_effect:request/4` uses `make_ref()` (line 88), which is unique only within the VM node, not distributed
2. **No idempotency mechanism** - No way to prevent duplicate effect execution on retry
3. **No receipt persistence** - ETS tables are volatile, receipts lost on crash
4. **No compensation/undo** - Cannot rollback effects on cancellation
5. **No VM integration** - Effects cannot be used in bytecode workflows (no effect opcodes in `ln_compile`, no yield/resume in `ln_vm`)
6. **No async handlers** - Only in-process execution via `ln_effect:default_handler/1` (lines 169-176)
7. **Tracing not integrated** - Effect events defined in `ln_trace` (lines 31-32) but not emitted by `ln_effect`

## Desired End State

### Functional Requirements
1. **Globally Unique Causal IDs**: Every effect has a UUID v4 identifier that is unique across distributed systems
2. **Idempotency**: Effects can be safely retried without duplicate execution using client-provided idempotency keys
3. **Receipt Persistence**: Receipts are persisted to disk for crash recovery and audit trails
4. **Compensation**: Completed effects can be undone when their scope is cancelled
5. **VM Integration**: Effects are first-class operations in bytecode execution with yield/resume semantics
6. **Tracing**: All effect operations emit trace events for observability
7. **Async Handlers**: Effects execute in separate processes for isolation and concurrency

### Non-Functional Requirements
- Performance: Support 1000+ effects/second per node
- Reliability: No receipt loss on crash (write-ahead logging)
- Scalability: Distributed effect coordination across nodes
- Security: Effect spec validation and whitelisting
- Observability: Complete audit trail via receipts and traces

### Verification
- All new modules have comprehensive unit tests (EUnit)
- Integration tests demonstrate effect lifecycle (request → execute → complete → receipt → compensate)
- Property-based tests verify idempotency guarantees
- Manual testing confirms VM executes workflows with effects
- Receipt export demonstrates audit trail completeness

## Key Discoveries

### From Code Analysis

1. **Pattern from `pnet_receipt.erl:102-127`**: Receipt generation validates inputs with guards, includes timestamp, returns immutable maps
   ```erlang
   make(BeforeHash, AfterHash, Move) when is_binary(BeforeHash), ... ->
       #{before_hash => BeforeHash, after_hash => AfterHash, move => Move, ts => timestamp()}
   ```

2. **Pattern from `ln_receipt.erl:247-253`**: ETS storage with separate primary and secondary indexes
   ```erlang
   add(#storage{table = Table, scope_index = ScopeIndex} = Storage, Receipt) ->
       true = ets:insert(Table, {EffectId, Receipt}),
       true = ets:insert(ScopeIndex, {ScopeId, EffectId}),
       Storage.
   ```

3. **Pattern from `ln_trace.erl:76-89`**: Event emission accepts full event or event type, adds sequence number automatically
   ```erlang
   emit(#{timestamp := _} = Event, State) -> State#trace_state{events = [Event#{seq => Seq} | Events], seq = Seq + 1};
   emit(EventType, State) -> emit(#{timestamp => ..., type => EventType, data => #{}}, State).
   ```

4. **Pattern from `ln_effect.erl:77-82`**: Pure functional state management with records
   ```erlang
   init(Handler) ->
       #effect_state{pending = #{}, receipts = [], handler = Handler}.
   ```

5. **Critical bug in `ln_effect.erl:87-103`**: The `request/4` function creates a new state with empty pending map and receipts, ignoring any existing state. This breaks effect state management.
   ```erlang
   request(Spec, ScopeId, CallbackMod, Cont) ->
       EffectId = make_ref(),
       State = #effect_state{pending = #{EffectId => PendingEffect}, receipts = [], handler = default_handler},
       {ok, EffectId, State}.  %% BUG: Ignores input state, creates fresh state
   ```

6. **Missing in `ln_vm.erl:67-77`**: No effect_state field in vm_state record, no waiting_for_effect tracking
   ```erlang
   -record(vm_state, {
       pc, frames, current_frame, stack, joins, scopes, scope_parents, result, status
       %% MISSING: effect_state, effect_handler, waiting_for_effect
   }).
   ```

7. **Missing in `ln_compile.erl:26-39`**: No effect opcodes defined
   ```erlang
   -type opcode() :: {op_task_start, atom()} | ... | {op_halt, term()}.
   %% MISSING: {op_effect_yield, effect_spec(), scope_id()}, {op_effect_resume, label()}
   ```

### Architecture Constraints

1. **Two receipt systems exist** - `pnet_receipt` (Petri net transitions) and `ln_receipt` (effect receipts). They serve different purposes and must remain distinct.
2. **Pure functional state management** - All `ln_*` modules use records, not process state. Effects must follow this pattern.
3. **No existing UUID library** - Must implement UUID v4 generation using `crypto:strong_rand_bytes/1`
4. **No persistent storage layer** - Must implement write-ahead logging for receipts (DETS or custom file-based)
5. **Cancellation integration** - Effects must respect `ln_cancel` hierarchical scope cancellation

## What We're NOT Doing

- **Replacing pnet_receipt** - Petri net receipts are separate and will remain unchanged
- **Implementing distributed transactions** - Effects are not a distributed transaction system
- **Building a full saga pattern** - Compensation is basic undo, not complex saga coordination
- **Effect composition/chaining** - Effects are executed individually, not composed
- **Circuit breaker or retry logic** - These are future enhancements
- **Effect batching** - Each effect is executed individually
- **Sandboxing untrusted effects** - Security validation is whitelist-only, not full sandboxing
- **Receipt archival** - Old receipts are not moved to cold storage
- **Performance optimization** - No optimization for high-frequency effects beyond basic ETS tuning

## Implementation Approach

### Strategy
Implement the effect system in **seven incremental phases**, where each phase is independently testable and deployable. Phases are ordered to minimize risk and enable early validation of critical features (UUID generation, idempotency, persistence).

### Phase Order Rationale
1. **Phase 1 (UUID)** - Foundation for all other features (causal IDs required everywhere)
2. **Phase 2 (Idempotency)** - Prevents duplicate execution, critical for reliability
3. **Phase 3 (Persistence)** - Crash recovery, enables audit trail
4. **Phase 4 (Compensation)** - Integration with cancellation, completes effect lifecycle
5. **Phase 5 (Async Handlers)** - Performance and isolation, not blocking for correctness
6. **Phase 6 (VM Integration)** - Enables effects in bytecode workflows (primary use case)
7. **Phase 7 (Tracing)** - Observability (nice-to-have, not blocking for functionality)

### Rollback Strategy
Each phase can be independently reverted:
- New modules (`ln_uuid`, `wf_effect_handler`) can be deleted
- Modified modules keep backward-compatible APIs (old functions still work)
- Feature flags control new behavior (idempotency checks optional via idempotency_key presence)

---

## Phases

### Phase 1: Unique Causal IDs

#### Overview
Add globally unique UUID v4 identifiers for all effects. This is the foundation for idempotency, persistence, and distributed correlation.

#### Changes Required

##### 1. New Module: `src/ln_uuid.erl`
**File**: `/Users/sac/cre/src/ln_uuid.erl`
**Changes**: Create new module for UUID v4 generation

```erlang
%%%-------------------------------------------------------------------
%%% @doc ln_uuid - UUID v4 generation for globally unique causal IDs.
%%%
%%% Uses crypto:strong_rand_bytes/1 to generate RFC 4122 UUID v4.
%%% Provides 122 bits of randomness, collision probability negligible.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_uuid).

%% API
-export([new/0]).
-export([to_string/1]).
-export([from_string/1]).

%% Types
-export_type([causal_id/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type causal_id() :: <<_:128>>.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Generate a new UUID v4 causal ID.
-spec new() -> causal_id().
new() ->
    %% Generate 16 random bytes
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    %% Set version and variant bits per RFC 4122
    <<A:32, B:16, (C band 16#0fff):16, 4:4, D:12, 2#10:2, D:14, E:48>>.

%% @doc Convert UUID to string representation (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
-spec to_string(causal_id()) -> string().
to_string(<<A:32, B:16, C:16, D:16, E:48>>) ->
    io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [A, B, C, D, E]).

%% @doc Parse UUID from string representation.
-spec from_string(string()) -> {ok, causal_id()} | {error, invalid_uuid}.
from_string(String) ->
    case re:run(String, "^([0-9a-f]{8})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{12})$", [
        {capture, all, list}]) of
        {match, [_, A, B, C, D, E]} ->
            <<(erlang:list_to_integer(A, 16)):32,
              (erlang:list_to_integer(B, 16)):16,
              (erlang:list_to_integer(C, 16)):16,
              (erlang:list_to_integer(D, 16)):16,
              (erlang:list_to_integer(E, 16)):48>> = UUID,
            {ok, UUID};
        nomatch ->
            {error, invalid_uuid}
    end.
```

##### 2. Modify: `src/ln_effect.erl`
**File**: `/Users/sac/cre/src/ln_effect.erl`
**Changes**: Update to use UUID instead of make_ref()

**Line 27**: Change effect_id type
```erlang
%% OLD:
-type effect_id() :: reference().

%% NEW:
-type effect_id() :: ln_uuid:causal_id().
```

**Line 30-35**: Add idempotency_key to effect_spec
```erlang
%% OLD:
-type effect_spec() :: #{
    module => module(),
    function => atom(),
    args => [term()],
    options => map()
}.

%% NEW:
-type effect_spec() :: #{
    idempotency_key => binary() | undefined,  % Client-provided for deduplication
    module => module(),
    function => atom(),
    args => [term()],
    options => map()
}.
```

**Line 87-103**: Fix request/4 to use UUID and properly update state
```erlang
%% OLD:
request(Spec, ScopeId, CallbackMod, Cont) ->
    EffectId = make_ref(),
    PendingEffect = #pending_effect{
        effect_id = EffectId,
        spec = Spec,
        scope_id = ScopeId,
        callback_mod = CallbackMod,
        continuation = Cont,
        status = requested,
        started_at = erlang:monotonic_time(millisecond)
    },
    State = #effect_state{
        pending = #{EffectId => PendingEffect},
        receipts = [],
        handler = default_handler
    },
    {ok, EffectId, State}.

%% NEW:
request(Spec, ScopeId, CallbackMod, Cont) ->
    EffectId = ln_uuid:new(),
    PendingEffect = #pending_effect{
        effect_id = EffectId,
        spec = Spec,
        scope_id = ScopeId,
        callback_mod = CallbackMod,
        continuation = Cont,
        status = requested,
        started_at = erlang:monotonic_time(millisecond)
    },
    State = #effect_state{
        pending = #{EffectId => PendingEffect},
        receipts = [],
        handler = default_handler
    },
    {ok, EffectId, State}.
```

**Note**: The bug where request/4 ignores input state will be fixed in Phase 2 when we implement idempotency checks that need to access existing state.

##### 3. Modify: `src/ln_receipt.erl`
**File**: `/Users/sac/cre/src/ln_receipt.erl`
**Changes**: Update receipt to use UUID and add idempotency_key

**Line 88**: Change effect_id type
```erlang
%% OLD:
-record(receipt, {
    effect_id :: reference(),
    ...
}).

%% NEW:
-record(receipt, {
    effect_id :: ln_uuid:causal_id(),
    idempotency_key :: binary() | undefined,  % NEW
    ...
}).
```

**Line 150-164**: Update new/2 to accept idempotency_key
```erlang
%% OLD:
new(EffectSpec, ScopeId) ->
    Now = erlang:monotonic_time(millisecond),
    EffectId = make_ref(),
    SpecHash = hash(EffectSpec),
    #receipt{
        effect_id => EffectId,
        spec_hash => SpecHash,
        created_at => Now,
        completed_at => Now,
        result_summary => undefined,
        scope_id => ScopeId
    }.

%% NEW:
new(EffectSpec, ScopeId, IdempotencyKey) ->
    Now = erlang:monotonic_time(millisecond),
    EffectId = ln_uuid:new(),
    SpecHash = hash(EffectSpec),
    #receipt{
        effect_id => EffectId,
        idempotency_key => IdempotencyKey,
        spec_hash => SpecHash,
        created_at => Now,
        completed_at => Now,
        result_summary => undefined,
        scope_id => ScopeId
    }.
```

**Line 247-253**: Update add/2 to handle idempotency_key
```erlang
%% OLD:
add(#storage{table = Table, scope_index = ScopeIndex} = Storage,
    #receipt{effect_id = EffectId, scope_id = ScopeId} = Receipt) ->
    true = ets:insert(Table, {EffectId, Receipt}),
    true = ets:insert(ScopeIndex, {ScopeId, EffectId}),
    Storage.

%% NEW:
add(#storage{table = Table, scope_index = ScopeIndex} = Storage,
    #receipt{effect_id = EffectId, scope_id = ScopeId, idempotency_key = Key} = Receipt) ->
    true = ets:insert(Table, {EffectId, Receipt}),
    true = ets:insert(ScopeIndex, {ScopeId, EffectId}),
    %% NEW: Add idempotency index if key present
    case Key of
        undefined -> ok;
        _ -> true = ets:insert(Storage#storage.idempotency_index, {{ScopeId, Key}, EffectId})
    end,
    Storage.
```

**Line 101-104**: Add idempotency_index to storage record
```erlang
%% OLD:
-record(storage, {
    table :: ets:tid(),
    scope_index :: ets:tid()
}).

%% NEW:
-record(storage, {
    table :: ets:tid(),
    scope_index :: ets:tid(),
    idempotency_index :: ets:tid()  % NEW: {ScopeId, IdempotencyKey} => EffectId
}).
```

#### Success Criteria

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_uuid`
- [ ] Tests pass: `rebar3 eunit --module=ln_effect`
- [ ] Tests pass: `rebar3 eunit --module=ln_receipt`
- [ ] Type checking passes: No dialyzer warnings
- [ ] Linting passes: `rebar3 lint`

##### Manual Verification:
- [ ] Generate 10,000 UUIDs, verify all are unique (property test)
- [ ] Verify UUID format matches RFC 4122 version 4
- [ ] Verify UUID string round-trip (to_string/from_string)
- [ ] Check ln_effect:request/4 generates UUID instead of reference
- [ ] Verify ln_receipt:new/3 creates receipts with UUID

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Idempotency Mechanism

#### Overview
Implement idempotency checks to prevent duplicate effect execution. Clients provide an idempotency key with each effect spec; if an effect with that key was already executed, return the cached receipt instead of re-executing.

#### Changes Required

##### 1. Modify: `src/ln_effect.erl`
**File**: `/Users/sac/cre/src/ln_effect.erl`
**Changes**: Add idempotency check in request/4, fix state management bug

**Line 85-103**: Rewrite request/4 to check for existing effects and properly update state
```erlang
%% NEW:
request(Spec, ScopeId, CallbackMod, Cont) ->
    request(Spec, ScopeId, CallbackMod, Cont, #effect_state{}).

request(Spec, ScopeId, CallbackMod, Cont, #effect_state{receipts = Receipts} = State) ->
    IdempotencyKey = maps:get(idempotency_key, Spec, undefined),

    case IdempotencyKey of
        undefined ->
            %% No idempotency key, proceed with effect execution
            create_effect(Spec, ScopeId, CallbackMod, Cont, State);
        Key ->
            %% Check if effect with this key already executed
            case find_receipt_by_key(Key, ScopeId, Receipts) of
                {ok, Receipt} ->
                    %% Return cached result
                    {ok, cached, Receipt, State};
                not_found ->
                    %% Proceed with effect execution
                    create_effect(Spec, ScopeId, CallbackMod, Cont, State)
            end
    end.

%% @private Create a new effect request
create_effect(Spec, ScopeId, CallbackMod, Cont, State) ->
    EffectId = ln_uuid:new(),
    PendingEffect = #pending_effect{
        effect_id = EffectId,
        spec = Spec,
        scope_id = ScopeId,
        callback_mod = CallbackMod,
        continuation = Cont,
        status = requested,
        started_at = erlang:monotonic_time(millisecond)
    },
    NewState = State#effect_state{
        pending = maps:put(EffectId, PendingEffect, State#effect_state.pending)
    },
    {ok, EffectId, NewState}.
```

**Line 160-167**: Add find_receipt_by_key helper
```erlang
%% NEW:
%% @private Find receipt by idempotency key
find_receipt_by_key(Key, ScopeId, Receipts) ->
    lists:foldl(fun
        (Receipt, not_found) ->
            case Receipt of
                #{idempotency_key := Key, scope_id := ScopeId} ->
                    {ok, Receipt};
                _ ->
                    not_found
            end;
        (_, Acc) ->
            Acc
    end, not_found, Receipts).
```

##### 2. Modify: `src/ln_receipt.erl`
**File**: `/Users/sac/cre/src/ln_receipt.erl`
**Changes**: Add lookup by idempotency key

**Line 267-284**: Add lookup_by_key/3 function
```erlang
%% NEW:
%% @doc Looks up receipt by idempotency key and scope.
-spec lookup_by_key(storage(), binary(), scope_id()) -> {ok, receipt()} | not_found.

lookup_by_key(#storage{idempotency_index = Idx, table = Table}, Key, ScopeId) ->
    case ets:lookup(Idx, {ScopeId, Key}) of
        [{{ScopeId, Key}, EffectId}] ->
            case ets:lookup(Table, EffectId) of
                [{EffectId, Receipt}] ->
                    {ok, Receipt};
                [] ->
                    not_found
            end;
        [] ->
            not_found
    end.
```

**Line 247-253**: Update storage initialization to create idempotency_index
```erlang
%% Add to module export:
-export([init_storage/0]).

%% NEW:
%% @doc Initialize receipt storage with all indexes.
-spec init_storage() -> storage().
init_storage() ->
    Table = ets:new(receipts, [set, public, {keypos, #receipt.effect_id}]),
    ScopeIndex = ets:new(receipt_scope_index, [bag, public]),
    IdempotencyIndex = ets:new(receipt_idempotency_index, [set, public]),
    #storage{
        table = Table,
        scope_index = ScopeIndex,
        idempotency_index = IdempotencyIndex
    }.
```

#### Success Criteria

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_effect`
- [ ] Tests pass: `rebar3 eunit --module=ln_receipt`
- [ ] Property tests verify idempotency: Same idempotency_key returns cached result
- [ ] Property tests verify different keys execute new effects

##### Manual Verification:
- [ ] Execute effect with idempotency_key, verify receipt stored
- [ ] Execute same effect again with same key, verify cached receipt returned
- [ ] Execute effect with different key, verify new effect created
- [ ] Execute effect without idempotency_key, verify no deduplication

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Receipt Persistence

#### Overview
Implement persistent receipt storage using write-ahead logging. Receipts are written to disk before completion, enabling crash recovery and audit trails.

#### Changes Required

##### 1. New Module: `src/ln_receipt_store.erl`
**File**: `/Users/sac/cre/src/ln_receipt_store.erl`
**Changes**: Create gen_server for persistent receipt storage

```erlang
%%%-------------------------------------------------------------------
%%% @doc ln_receipt_store - Persistent receipt storage with write-ahead logging.
%%%
%%% Receipts are written to a DETS table before being cached in ETS.
%%% On startup, receipts are replayed from DETS into ETS for fast access.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_receipt_store).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([store/1]).
-export([lookup/1]).
-export([lookup_by_key/2]).
-export([get_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-record(state, {
    ets_table :: ets:tid(),
    dets_table :: dets:tab_name(),
    log_file :: file:filename()
}).

-type receipt() :: #{
    effect_id := ln_uuid:causal_id(),
    idempotency_key := binary() | undefined,
    spec_hash := binary(),
    spec := ln_effect:effect_spec(),
    result := term(),
    started_at := integer(),
    completed_at := integer(),
    scope_id := term()
}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Start the receipt store.
-spec start_link(file:filename()) -> {ok, pid()} | {error, term()}.
start_link(LogFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, LogFile, []).

%% @doc Store a receipt (writes to DETS and ETS).
-spec store(receipt()) -> ok.
store(Receipt) ->
    gen_server:cast(?MODULE, {store, Receipt}).

%% @doc Look up receipt by effect ID.
-spec lookup(ln_uuid:causal_id()) -> {ok, receipt()} | not_found.
lookup(EffectId) ->
    gen_server:call(?MODULE, {lookup, EffectId}).

%% @doc Look up receipt by idempotency key and scope.
-spec lookup_by_key(binary(), term()) -> {ok, receipt()} | not_found.
lookup_by_key(Key, ScopeId) ->
    gen_server:call(?MODULE, {lookup_by_key, Key, ScopeId}).

%% @doc Get all receipts.
-spec get_all() -> [receipt()].
get_all() ->
    gen_server:call(?MODULE, get_all).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

init(LogFile) ->
    %% Open DETS table for persistent storage
    DetsFile = filename:join(LogFile, "receipts.dets"),
    {ok, DetsTable} = dets:open_file(receipts, [{file, DetsFile}, {type, set}]),

    %% Create ETS table for fast access
    EtsTable = ets:new(receipt_cache, [set, public, {read_concurrency, true}]),

    %% Replay receipts from DETS into ETS
    dets:to_ets(DetsTable, EtsTable),

    {ok, #state{
        ets_table = EtsTable,
        dets_table = DetsTable,
        log_file = LogFile
    }}.

handle_call({lookup, EffectId}, _From, #state{ets_table = Table} = State) ->
    Reply = case ets:lookup(Table, EffectId) of
        [{EffectId, Receipt}] -> {ok, Receipt};
        [] -> not_found
    end,
    {reply, Reply, State};

handle_call({lookup_by_key, Key, ScopeId}, _From, #state{ets_table = Table} = State) ->
    Reply = case ets:match(Table, {{'$1', #receipt{idempotency_key = Key, scope_id = ScopeId}}, '$1'}) of
        [[EffectId]] ->
            case ets:lookup(Table, EffectId) of
                [{EffectId, Receipt}] -> {ok, Receipt};
                [] -> not_found
            end;
        [] ->
            not_found
    end,
    {reply, Reply, State};

handle_call(get_all, _From, #state{ets_table = Table} = State) ->
    Receipts = ets:tab2list(Table),
    {reply, Receipts, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({store, #{effect_id := EffectId} = Receipt}, #state{ets_table = Ets, dets_table = Dets} = State) ->
    %% Write to DETS first (persistent)
    ok = dets:insert(Dets, {EffectId, Receipt}),
    %% Then cache in ETS (fast access)
    true = ets:insert(Ets, {EffectId, Receipt}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{dets_table = Dets}) ->
    dets:close(Dets),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

##### 2. Modify: `src/ln_effect.erl`
**File**: `/Users/sac/cre/src/ln_effect.erl`
**Changes**: Integrate with ln_receipt_store for persistence

**Line 108-127**: Update complete/3 to store receipt persistently
```erlang
%% OLD:
complete(EffectId, Result, #effect_state{pending = Pending, receipts = Receipts} = State) ->
    case maps:find(EffectId, Pending) of
        {ok, #pending_effect{spec = Spec, scope_id = ScopeId, started_at = StartedAt}} ->
            Receipt = #{
                effect_id => EffectId,
                spec_hash => ln_receipt:hash(Spec),
                spec => Spec,
                scope_id => ScopeId,
                started_at => StartedAt,
                completed_at => erlang:monotonic_time(millisecond),
                result => Result
            },
            NewState = State#effect_state{
                pending = maps:remove(EffectId, Pending),
                receipts = [Receipt | Receipts]
            },
            {ok, NewState};
        error ->
            {error, effect_not_found}
    end.

%% NEW:
complete(EffectId, Result, #effect_state{pending = Pending, receipts = Receipts} = State) ->
    case maps:find(EffectId, Pending) of
        {ok, #pending_effect{spec = Spec, scope_id = ScopeId, started_at = StartedAt}} ->
            Receipt = #{
                effect_id => EffectId,
                idempotency_key => maps:get(idempotency_key, Spec, undefined),
                spec_hash => ln_receipt:hash(Spec),
                spec => Spec,
                scope_id => ScopeId,
                started_at => StartedAt,
                completed_at => erlang:monotonic_time(millisecond),
                result => Result
            },
            %% Store receipt persistently
            ok = ln_receipt_store:store(Receipt),
            NewState = State#effect_state{
                pending = maps:remove(EffectId, Pending),
                receipts = [Receipt | Receipts]
            },
            {ok, NewState};
        error ->
            {error, effect_not_found}
    end.
```

#### Success Criteria

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_receipt_store`
- [ ] Tests pass: `rebar3 eunit --module=ln_effect`
- [ ] Verify receipts persist across process restart
- [ ] Verify DETS file created in specified directory

##### Manual Verification:
- [ ] Store receipt, kill process, restart, verify receipt restored
- [ ] Check DETS file contains receipt data
- [ ] Verify ETS cache populated from DETS on startup
- [ ] Test with 1000 receipts, verify all persisted

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 4.

---

### Phase 4: Compensation Support

#### Overview
Implement compensation (undo) operations for completed effects. When a scope is cancelled, all completed effects in that scope are compensated using their registered compensation functions.

#### Changes Required

##### 1. Modify: `src/ln_effect.erl`
**File**: `/Users/sac/cre/src/ln_effect.erl`
**Changes**: Add compensation to effect_spec and receipt

**Line 30-36**: Add compensation to effect_spec
```erlang
%% OLD:
-type effect_spec() :: #{
    idempotency_key => binary() | undefined,
    module => module(),
    function => atom(),
    args => [term()],
    options => map()
}.

%% NEW:
-type effect_spec() :: #{
    idempotency_key => binary() | undefined,
    module => module(),
    function => atom(),
    args => [term()],
    options => map(),
    compensation => fun((term()) -> ok | {error, term()}) | undefined  % NEW: Undo function
}.
```

**Line 39-47**: Add compensation to receipt
```erlang
%% OLD:
-type receipt() :: #{
    effect_id => effect_id(),
    spec_hash => binary(),
    spec => effect_spec(),
    scope_id => scope_id(),
    started_at => integer(),
    completed_at => integer() | undefined,
    result => term() | undefined
}.

%% NEW:
-type receipt() :: #{
    effect_id => effect_id(),
    spec_hash => binary(),
    spec => effect_spec(),
    scope_id => scope_id(),
    started_at => integer(),
    completed_at => integer() | undefined,
    result => term() | undefined,
    compensation => fun((term()) -> ok | {error, term()}) | undefined  % NEW
}.
```

**Line 108-127**: Update complete/3 to store compensation
```erlang
%% NEW:
complete(EffectId, Result, #effect_state{pending = Pending, receipts = Receipts} = State) ->
    case maps:find(EffectId, Pending) of
        {ok, #pending_effect{spec = Spec, scope_id = ScopeId, started_at = StartedAt}} ->
            Compensation = maps:get(compensation, Spec, undefined),
            Receipt = #{
                effect_id => EffectId,
                idempotency_key => maps:get(idempotency_key, Spec, undefined),
                spec_hash => ln_receipt:hash(Spec),
                spec => Spec,
                scope_id => ScopeId,
                started_at => StartedAt,
                completed_at => erlang:monotonic_time(millisecond),
                result => Result,
                compensation => Compensation
            },
            ok = ln_receipt_store:store(Receipt),
            NewState = State#effect_state{
                pending = maps:remove(EffectId, Pending),
                receipts = [Receipt | Receipts]
            },
            {ok, NewState};
        error ->
            {error, effect_not_found}
    end.
```

**Line 129-154**: Update cancel_effects/2 to compensate completed effects
```erlang
%% OLD:
cancel_effects(ScopeId, #effect_state{pending = Pending, receipts = Receipts} = State) ->
    {ToCancel, Remaining} = maps:fold(fun
        (EffectId, #pending_effect{scope_id = S} = P, {Cancel, Rest}) ->
            case S =:= ScopeId of
                true ->
                    Receipt = #{
                        effect_id => EffectId,
                        spec_hash => ln_receipt:hash(P#pending_effect.spec),
                        spec => P#pending_effect.spec,
                        scope_id => ScopeId,
                        started_at => P#pending_effect.started_at,
                        completed_at => erlang:monotonic_time(millisecond),
                        result => cancelled
                    },
                    {[EffectId | Cancel], Rest, [Receipt | Receipts]};
                false ->
                    {Cancel, maps:put(EffectId, P, Rest), Receipts}
            end
    end, {[], #{} , Receipts}, Pending),
    NewState = State#effect_state{
        pending = Remaining,
        receipts = ToCancel ++ Receipts
    },
    {lists:reverse(ToCancel), NewState}.

%% NEW:
cancel_effects(ScopeId, #effect_state{pending = Pending, receipts = Receipts} = State) ->
    %% Cancel pending effects
    {ToCancel, Remaining} = maps:fold(fun
        (EffectId, #pending_effect{scope_id = S} = P, {Cancel, Rest}) ->
            case S =:= ScopeId of
                true ->
                    Receipt = #{
                        effect_id => EffectId,
                        spec_hash => ln_receipt:hash(P#pending_effect.spec),
                        spec => P#pending_effect.spec,
                        scope_id => ScopeId,
                        started_at => P#pending_effect.started_at,
                        completed_at => erlang:monotonic_time(millisecond),
                        result => cancelled,
                        compensation => undefined
                    },
                    {[EffectId | Cancel], Rest, [Receipt | Receipts]};
                false ->
                    {Cancel, maps:put(EffectId, P, Rest), Receipts}
            end
    end, {[], #{} , Receipts}, Pending),

    %% Compensate completed effects in scope
    Compensated = compensate_effects(ScopeId, Receipts),

    NewState = State#effect_state{
        pending = Remaining,
        receipts = ToCancel ++ Receipts
    },
    {lists:reverse(ToCancel), NewState, Compensated}.
```

**Line 155-167**: Add compensate_effects helper
```erlang
%% NEW:
%% @private Compensate all completed effects in a scope.
compensate_effects(ScopeId, Receipts) ->
    lists:foldl(fun
        (#{scope_id := S, compensation := undefined} = Receipt, Acc) when S =:= ScopeId ->
            %% No compensation function, skip
            [Receipt | Acc];
        (#{scope_id := S, compensation := Comp, result := Result} = Receipt, Acc) when S =:= ScopeId ->
            %% Execute compensation
            case Comp(Result) of
                ok ->
                    [Receipt | Acc];
                {error, Reason} ->
                    %% Log compensation failure but continue
                    error_logger:warning_msg("Compensation failed for effect ~p: ~p~n", [
                        maps:get(effect_id, Receipt), Reason
                    ]),
                    [Receipt | Acc]
            end;
        (Receipt, Acc) ->
            %% Different scope, skip
            [Receipt | Acc]
    end, [], Receipts).
```

##### 2. Integration with `ln_cancel.erl`
**File**: `/Users/sac/cre/src/ln_cancel.erl`
**Changes**: No changes needed - ln_effect:cancel_effects/2 uses scope_id which integrates with ln_cancel

#### Success Criteria

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_effect`
- [ ] Unit tests for compensation execution
- [ ] Unit tests for compensation failure handling

##### Manual Verification:
- [ ] Execute effect with compensation function
- [ ] Cancel scope containing the effect
- [ ] Verify compensation function was called
- [ ] Test compensation failure is logged but doesn't block cancellation

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 5.

---

### Phase 5: Effect Handler Distribution

#### Overview
Implement async effect execution using separate gen_server processes. This provides isolation, concurrency, and prevents effect execution from blocking the VM.

#### Changes Required

##### 1. New Module: `src/wf_effect_handler.erl`
**File**: `/Users/sac/cre/src/wf_effect_handler.erl`
**Changes**: Create gen_server for async effect execution

```erlang
%%%-------------------------------------------------------------------
%%% @doc wf_effect_handler - Async effect handler process.
%%%
%%% Executes effects in separate processes for isolation and concurrency.
%%% Each effect gets its own handler process that is terminated after completion.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_effect_handler).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-record(state, {
    effect_id :: ln_uuid:causal_id(),
    spec :: ln_effect:effect_spec(),
    caller :: pid(),
    timeout :: integer()
}).

-type effect_spec() :: #{
    module := module(),
    function := atom(),
    args := [term()],
    options := map(),
    compensation := fun((term()) -> ok | {error, term()}) | undefined
}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Start an effect handler for a specific effect.
-spec start_link({ln_uuid:causal_id(), effect_spec(), pid()}) -> {ok, pid()} | {error, term()}.
start_link({EffectId, Spec, Caller}) ->
    gen_server:start_link(?MODULE, {EffectId, Spec, Caller}, []).

%% @doc Execute an effect asynchronously.
-spec execute(pid(), effect_spec()) -> {ok, ln_uuid:causal_id()}.
-spec execute(EffectSpec) -> {ok, Result} | {error, Reason} when
      EffectSpec :: effect_spec(),
      Result :: term(),
      Reason :: term().

execute(HandlerPid, Spec) ->
    gen_server:cast(HandlerPid, {execute, Spec}).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

init({EffectId, Spec, Caller}) ->
    Timeout = maps:get(timeout, Spec, 30000),  % Default 30 second timeout
    {ok, #state{
        effect_id = EffectId,
        spec = Spec,
        caller = Caller,
        timeout = Timeout
    }, Timeout}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({execute, Spec}, #state{effect_id = EffectId, caller = Caller} = State) ->
    Result = execute_effect(Spec),
    Caller ! {effect_complete, EffectId, Result},
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private Execute the effect spec.
execute_effect(#{module := Mod, function := Fun, args := Args}) ->
    try
        {ok, apply(Mod, Fun, Args)}
    catch
        _:Reason ->
            {error, Reason}
    end.
```

##### 2. Modify: `src/ln_effect.erl`
**File**: `/Users/sac/cre/src/ln_effect.erl`
**Changes**: Update request/4 to spawn async handler

**Line 85-103**: Update request/4 to use async handler
```erlang
%% NEW:
request(Spec, ScopeId, CallbackMod, Cont) ->
    request(Spec, ScopeId, CallbackMod, Cont, #effect_state{handler = Handler}).

request(Spec, ScopeId, CallbackMod, Cont, #effect_state{handler = Handler} = State) ->
    IdempotencyKey = maps:get(idempotency_key, Spec, undefined),

    case IdempotencyKey of
        undefined ->
            create_async_effect(Spec, ScopeId, CallbackMod, Cont, Handler, State);
        Key ->
            case find_receipt_by_key(Key, ScopeId, State#effect_state.receipts) of
                {ok, Receipt} ->
                    {ok, cached, Receipt, State};
                not_found ->
                    create_async_effect(Spec, ScopeId, CallbackMod, Cont, Handler, State)
            end
    end.

%% @private Create effect with async handler
create_async_effect(Spec, ScopeId, CallbackMod, Cont, Handler, State) ->
    EffectId = ln_uuid:new(),

    %% Start async handler process
    {ok, HandlerPid} = wf_effect_handler:start_link({EffectId, Spec, self()}),

    %% Send execute request
    wf_effect_handler:execute(HandlerPid, Spec),

    PendingEffect = #pending_effect{
        effect_id = EffectId,
        spec = Spec,
        scope_id = ScopeId,
        callback_mod = CallbackMod,
        continuation = Cont,
        status = in_flight,
        started_at = erlang:monotonic_time(millisecond)
    },

    NewState = State#effect_state{
        pending = maps:put(EffectId, PendingEffect, State#effect_state.pending)
    },
    {ok, EffectId, NewState}.
```

**Line 160-177**: Add handle_info for effect_complete
```erlang
%% NEW:
%% @doc Handle effect completion message from async handler.
-spec handle_info(term(), state()) -> state().
handle_info({effect_complete, EffectId, Result}, #effect_state{pending = Pending} = State) ->
    case maps:take(EffectId, Pending) of
        {#pending_effect{callback_mod = Mod, continuation = Cont}, RemainingPending} ->
            %% Resume execution with result
            case Mod:resume(Cont, Result) of
                ok ->
                    %% Generate receipt
                    {ok, NewState} = complete(EffectId, Result, State#effect_state{pending = RemainingPending}),
                    NewState;
                {error, Reason} ->
                    error_logger:error_msg("Effect resume failed for ~p: ~p~n", [EffectId, Reason]),
                    State
            end;
        error ->
            %% Effect not found, may have been cancelled
            State
    end.
```

#### Success Criteria

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=wf_effect_handler`
- [ ] Tests pass: `rebar3 eunit --module=ln_effect`
- [ ] Verify effects execute in separate processes
- [ ] Verify handler processes terminate after completion

##### Manual Verification:
- [ ] Execute effect, verify handler process created
- [ ] Wait for completion, verify handler process terminated
- [ ] Check effect result returned to caller
- [ ] Test effect timeout handling

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 6.

---

### Phase 6: Bytecode VM Integration

#### Overview
Integrate effects into the bytecode VM by adding effect opcodes to the compiler and implementing yield/resume semantics in the executor. This enables effects to be used in workflow plans.

#### Changes Required

##### 1. Modify: `src/ln_compile.erl`
**File**: `/Users/sac/cre/src/ln_compile.erl`
**Changes**: Add effect opcodes

**Line 26-39**: Add effect opcodes
```erlang
%% OLD:
-type opcode() :: {op_task_start, atom()}
                | {op_task_complete, term()}
                | {op_fork, [label()], label()}
                | {op_join_wait, join_id(), label()}
                | {op_xor_branch, [label()], label()}
                | {op_xor_choose, reference(), label()}
                | {op_scope_enter, scope_id()}
                | {op_scope_exit, scope_id()}
                | {op_defer_start, [label()], label()}
                | {op_defer_wait, reference(), label()}
                | {op_loop_check, label(), label()}
                | {op_loop_iter, label()}
                | {op_wait_check, term(), label()}
                | {op_halt, term()}.

%% NEW:
-type opcode() :: {op_task_start, atom()}
                | {op_task_complete, term()}
                | {op_fork, [label()], label()}
                | {op_join_wait, join_id(), label()}
                | {op_xor_branch, [label()], label()}
                | {op_xor_choose, reference(), label()}
                | {op_scope_enter, scope_id()}
                | {op_scope_exit, scope_id()}
                | {op_defer_start, [label()], label()}
                | {op_defer_wait, reference(), label()}
                | {op_loop_check, label(), label()}
                | {op_loop_iter, label()}
                | {op_wait_check, term(), label()}
                | {op_effect_yield, ln_effect:effect_spec(), scope_id()}  % NEW
                | {op_effect_resume, label()}  % NEW
                | {op_halt, term()}.
```

**Line 73-88**: Add opcode_name cases for effect opcodes
```erlang
%% NEW:
opcode_name({op_effect_yield, _, _}) -> op_effect_yield;
opcode_name({op_effect_resume, _}) -> op_effect_resume.
```

##### 2. Modify: `src/ln_plan.erl`
**File**: `/Users/sac/cre/src/ln_plan.erl`
**Changes**: Add effect constructor

**Line 26-36**: Add effect export
```erlang
%% Plan constructors
-export([task/1]).
-export([seq/1]).
-export([par/1]).
-export([xor/1]).
-export([join/2]).
-export([loop/2]).
-export([defer/1]).
-export([scope/2]).
-export([mi/2]).
-export([wait/1]).
-export([effect/1]).  % NEW
```

**Line 68-77**: Add effect plan type
```erlang
%% OLD:
-type plan() :: {task, task_id()}
              | {seq, [plan()]}
              | {par, [plan()]}
              | {xor, [plan()]}
              | {join, join_policy(), [plan()]}
              | {loop, loop_policy(), plan()}
              | {defer, [plan()]}
              | {scope, scope_id(), plan()}
              | {mi, mi_policy(), plan()}
              | {wait, term()}.

%% NEW:
-type plan() :: {task, task_id()}
              | {seq, [plan()]}
              | {par, [plan()]}
              | {xor, [plan()]}
              | {join, join_policy(), [plan()]}
              | {loop, loop_policy(), plan()}
              | {defer, [plan()]}
              | {scope, scope_id(), plan()}
              | {mi, mi_policy(), plan()}
              | {wait, term()}
              | {effect, ln_effect:effect_spec()}.  % NEW
```

**Line 141-142**: Add effect constructor function
```erlang
%% NEW:
%% @doc Create an effect node.
-spec effect(ln_effect:effect_spec()) -> plan().
effect(Spec) when is_map(Spec) ->
    {effect, Spec}.
```

**Line 147-185**: Add validation for effect
```erlang
%% NEW: Add to validate/1:
validate({effect, Spec}) when is_map(Spec) ->
    %% Validate effect spec has required fields
    case {maps:get(module, Spec, undefined), maps:get(function, Spec, undefined)} of
        {Mod, Fun} when Mod =/= undefined, Fun =/= undefined -> ok;
        _ -> {error, {invalid_effect_spec, Spec}}
    end;
```

##### 3. Modify: `src/ln_compile.erl`
**File**: `/Users/sac/cre/src/ln_compile.erl`
**Changes**: Add effect compilation

**Line 90-133**: Add effect compilation case
```erlang
%% NEW: Add to compile_plan/4:
compile_plan({effect, Spec}, Label, Joins, Scopes) ->
    %% Get current scope from context
    %% For now, use a default scope - in real implementation, track current scope
    ScopeId = current_scope,  % TODO: Track current scope during compilation
    YieldLabel = Label,
    ResumeLabel = Label + 1,
    Program = [
        {YieldLabel, {op_effect_yield, Spec, ScopeId}},
        {ResumeLabel, {op_effect_resume, ResumeLabel + 1}}
    ],
    {Program, Joins, Scopes}.
```

##### 4. Modify: `src/ln_vm.erl`
**File**: `/Users/sac/cre/src/ln_vm.erl`
**Changes**: Add effect state handling, implement effect opcodes

**Line 67-77**: Add effect fields to vm_state
```erlang
%% OLD:
-record(vm_state, {
    pc :: pc(),
    frames :: #{frame_id() => frame()},
    current_frame :: frame_id() | undefined,
    stack :: [frame_id()],
    joins :: #{join_id() => #join_state{}},
    scopes :: #{scope_id() => running | cancelling | cancelled},
    scope_parents :: #{scope_id() => scope_id() | undefined},
    result :: term() | undefined,
    status :: running | halted | blocked
}).

%% NEW:
-record(vm_state, {
    pc :: pc(),
    frames :: #{frame_id() => frame()},
    current_frame :: frame_id() | undefined,
    stack :: [frame_id()],
    joins :: #{join_id() => #join_state{}},
    scopes :: #{scope_id() => running | cancelling | cancelled},
    scope_parents :: #{scope_id() => scope_id() | undefined},
    effect_state :: ln_effect:state() | undefined,  % NEW
    waiting_for_effect :: ln_effect:effect_id() | undefined,  % NEW
    result :: term() | undefined,
    status :: running | halted | blocked | waiting_effect  % NEW
}).
```

**Line 86-98**: Update init/1 to initialize effect state
```erlang
%% OLD:
init(#{program := _Program}) ->
    #vm_state{
        pc = 0,
        frames => #{},
        current_frame => undefined,
        stack => [],
        joins => #{},
        scopes => #{},
        scope_parents => #{},
        result => undefined,
        status = running
    }.

%% NEW:
init(#{program := _Program}) ->
    #vm_state{
        pc = 0,
        frames => #{},
        current_frame => undefined,
        stack => [],
        joins => #{},
        scopes => #{},
        scope_parents => #{},
        effect_state => ln_effect:init(wf_effect_handler),
        waiting_for_effect => undefined,
        result => undefined,
        status = running
    }.
```

**Line 100-112**: Update step/1 to handle waiting_effect status
```erlang
%% OLD:
step(#vm_state{status = halted} = State) ->
    {halt, State};
step(#vm_state{status = blocked} = State) ->
    {ok, State};
step(#vm_state{} = State) ->
    case fetch_instruction(State) of
        {ok, {_Label, Opcode}, NewState} ->
            execute_opcode(Opcode, NewState);
        {error, _} = Error ->
            Error
    end.

%% NEW:
step(#vm_state{status = halted} = State) ->
    {halt, State};
step(#vm_state{status = blocked} = State) ->
    {ok, State};
step(#vm_state{status = waiting_effect} = State) ->
    {ok, State};  % Blocked waiting for effect
step(#vm_state{} = State) ->
    case fetch_instruction(State) of
        {ok, {_Label, Opcode}, NewState} ->
            execute_opcode(Opcode, NewState);
        {error, _} = Error ->
            Error
    end.
```

**Line 199-200**: Add effect opcode execution
```erlang
%% NEW: Add to execute_opcode/2:
execute_opcode({op_effect_yield, Spec, ScopeId}, #vm_state{effect_state = EffState} = State) ->
    {ok, EffectId, NewEffState} = ln_effect:request(Spec, ScopeId, ?MODULE, self()),
    NewState = State#vm_state{
        effect_state = NewEffState,
        status = waiting_effect,
        waiting_for_effect = EffectId
    },
    {ok, NewState};

execute_opcode({op_effect_resume, NextLabel}, #vm_state{pc = _PC} = State) ->
    NewState = State#vm_state{
        status = running,
        waiting_for_effect = undefined
    },
    {ok, set_program_counter(NextLabel, NewState)}.
```

**Line 217-233**: Add handle_info for effect completion
```erlang
%% NEW: Add to ln_vm module:
%% @doc Handle effect completion messages.
-spec handle_effect_complete(ln_effect:effect_id(), term(), state()) -> {ok, state()}.
handle_effect_complete(EffectId, Result, #vm_state{waiting_for_effect = EffectId, effect_state = EffState} = State) ->
    %% Generate receipt
    {ok, NewEffState} = ln_effect:complete(EffectId, Result, EffState),

    %% Resume VM execution
    NewState = State#vm_state{
        effect_state = NewEffState,
        status = running,
        waiting_for_effect = undefined
    },
    step(NewState);

handle_effect_complete(_EffectId, _Result, State) ->
    %% Effect not for us, ignore
    {ok, State}.
```

#### Success Criteria

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_compile`
- [ ] Tests pass: `rebar3 eunit --module=ln_plan`
- [ ] Tests pass: `rebar3 eunit --module=ln_vm`
- [ ] Compile a plan with effect, verify bytecode contains op_effect_yield
- [ ] Execute bytecode with effect, verify VM yields and resumes

##### Manual Verification:
- [ ] Create workflow plan with effect: `ln_plan:effect(#{module => io, function => format, args => ["Hello~n"]})`
- [ ] Compile plan to bytecode
- [ ] Execute bytecode in VM
- [ ] Verify effect executed, "Hello" printed
- [ ] Check receipt generated and persisted

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 7.

---

### Phase 7: Tracing Integration

#### Overview
Emit trace events for all effect operations (request, complete, cancel, compensate) to provide complete observability.

#### Changes Required

##### 1. Modify: `src/ln_effect.erl`
**File**: `/Users/sac/cre/src/ln_effect.erl`
**Changes**: Add trace state, emit events

**Line 61-65**: Add trace field to effect_state
```erlang
%% OLD:
-record(effect_state, {
    pending :: pending(),
    receipts :: [receipt()],
    handler :: handler()
}).

%% NEW:
-record(effect_state, {
    pending :: pending(),
    receipts :: [receipt()],
    handler :: handler(),
    trace :: ln_trace:state()  % NEW
}).
```

**Line 75-82**: Update init/1 to initialize trace
```erlang
%% OLD:
init(Handler) ->
    #effect_state{
        pending = #{},
        receipts = [],
        handler = Handler
    }.

%% NEW:
init(Handler) ->
    #effect_state{
        pending = #{},
        receipts = [],
        handler = Handler,
        trace = ln_trace:new(#{level => full, max_events => 10000})
    }.
```

**Line 85-103**: Update request/4 to emit trace event
```erlang
%% NEW:
request(Spec, ScopeId, CallbackMod, Cont) ->
    request(Spec, ScopeId, CallbackMod, Cont, #effect_state{trace = Trace} = State) ->
        IdempotencyKey = maps:get(idempotency_key, Spec, undefined),

        %% Emit effect_requested event
        RequestEvent = #{
            type => effect_requested,
            data => #{
                idempotency_key => IdempotencyKey,
                spec_hash => ln_receipt:hash(Spec),
                scope_id => ScopeId
            }
        },
        NewTrace = ln_trace:emit(RequestEvent, Trace),

        case IdempotencyKey of
            undefined ->
                create_async_effect(Spec, ScopeId, CallbackMod, Cont, Handler, State#effect_state{trace = NewTrace});
            Key ->
                case find_receipt_by_key(Key, ScopeId, State#effect_state.receipts) of
                    {ok, Receipt} ->
                        %% Emit effect_cached event
                        CachedEvent = #{type => effect_cached, data => #{idempotency_key => Key}},
                        NewTrace2 = ln_trace:emit(CachedEvent, NewTrace),
                        {ok, cached, Receipt, State#effect_state{trace = NewTrace2}};
                    not_found ->
                        create_async_effect(Spec, ScopeId, CallbackMod, Cont, Handler, State#effect_state{trace = NewTrace})
                end
        end.
```

**Line 108-127**: Update complete/3 to emit trace event
```erlang
%% NEW:
complete(EffectId, Result, #effect_state{pending = Pending, receipts = Receipts, trace = Trace} = State) ->
    case maps:find(EffectId, Pending) of
        {ok, #pending_effect{spec = Spec, scope_id = ScopeId, started_at = StartedAt}} ->
            Compensation = maps:get(compensation, Spec, undefined),
            Receipt = #{
                effect_id => EffectId,
                idempotency_key => maps:get(idempotency_key, Spec, undefined),
                spec_hash => ln_receipt:hash(Spec),
                spec => Spec,
                scope_id => ScopeId,
                started_at => StartedAt,
                completed_at => erlang:monotonic_time(millisecond),
                result => Result,
                compensation => Compensation
            },
            ok = ln_receipt_store:store(Receipt),

            %% Emit effect_completed event
            CompleteEvent = #{
                type => effect_completed,
                data => #{
                    causal_id => EffectId,
                    result => Result,
                    duration => erlang:monotonic_time(millisecond) - StartedAt
                }
            },
            NewTrace = ln_trace:emit(CompleteEvent, Trace),

            NewState = State#effect_state{
                pending = maps:remove(EffectId, Pending),
                receipts = [Receipt | Receipts],
                trace = NewTrace
            },
            {ok, NewState};
        error ->
            {error, effect_not_found}
    end.
```

**Line 129-167**: Update cancel_effects/2 to emit trace event
```erlang
%% NEW:
cancel_effects(ScopeId, #effect_state{trace = Trace} = State) ->
    %% Cancel pending effects
    {ToCancel, Remaining} = maps:fold(...),

    %% Compensate completed effects in scope
    Compensated = compensate_effects(ScopeId, Receipts),

    %% Emit effect_cancelled event
    CancelEvent = #{
        type => effect_cancelled,
        data => #{
            scope_id => ScopeId,
            cancelled_count => length(ToCancel),
            compensated_count => length(Compensated)
        }
    },
    NewTrace = ln_trace:emit(CancelEvent, Trace),

    NewState = State#effect_state{trace = NewTrace},
    {lists:reverse(ToCancel), NewState, Compensated}.
```

**Line 163-166**: Add get_trace_events/1
```erlang
%% NEW:
%% @doc Get all trace events from effect state.
-spec get_trace_events(state()) -> [ln_trace:event()].
get_trace_events(#effect_state{trace = Trace}) ->
    ln_trace:get_all(Trace).
```

#### Success Criteria

##### Automated Verification:
- [ ] Tests pass: `rebar3 eunit --module=ln_effect`
- [ ] Verify trace events emitted for all effect operations
- [ ] Verify trace contains causal_id in events

##### Manual Verification:
- [ ] Execute effect, export trace, verify effect_requested event present
- [ ] Complete effect, export trace, verify effect_completed event present
- [ ] Cancel scope, export trace, verify effect_cancelled event present
- [ ] Check trace event data contains causal_id, duration, scope_id

**Note**: Complete all automated verification, then pause for manual confirmation. All phases complete!

---

## Testing Strategy

### Unit Tests
Each module will have comprehensive EUnit tests:

1. **ln_uuid**: UUID format validation, uniqueness, string round-trip
2. **ln_effect**: Effect lifecycle, idempotency, state management
3. **ln_receipt**: Receipt generation, storage, lookup
4. **ln_receipt_store**: Persistence, crash recovery, DETS/ETS synchronization
5. **wf_effect_handler**: Async execution, timeout, error handling
6. **ln_compile**: Effect compilation, opcode generation
7. **ln_vm**: Effect yield/resume, status management
8. **ln_plan**: Effect constructor, validation

### Integration Tests
End-to-end scenarios:

1. **Basic effect execution**: Plan → Compile → Execute → Receipt
2. **Idempotency**: Duplicate requests with same key return cached result
3. **Compensation**: Effect execution → Cancel scope → Compensate
4. **Crash recovery**: Execute effect → Crash → Restart → Receipt persisted
5. **VM integration**: Workflow with multiple effects yields and resumes

### Manual Testing Steps

1. **Test UUID generation**:
   ```erlang
   > ln_uuid:new().
   <<184,106,241,138,80,77,74,160,147,117,158,161,93,107,24,220>>
   > ln_uuid:to_string(ln_uuid:new()).
   "b86af18a-504d-4aa0-9375-9ea15d6118dc"
   ```

2. **Test effect execution**:
   ```erlang
   > Spec = #{module => io, function => format, args => ["Hello~n"]},
   > {ok, EffectId, State} = ln_effect:request(Spec, my_scope, ?MODULE, self()),
   > receive {effect_complete, EffectId, {ok, ok}} -> ok end,
   > {ok, State2} = ln_effect:complete(EffectId, {ok, ok}, State),
   > ln_effect:get_receipts(State2).
   [#{effect_id := EffectId, result := {ok, ok}}]
   ```

3. **Test idempotency**:
   ```erlang
   > Spec = #{idempotency_key => <<"test-key-1">>, module => ..., function => ...},
   > {ok, _, State1} = ln_effect:request(Spec, scope, ?MODULE, self()),
   > %% Complete effect...
   > %% Request again with same key
   > {ok, cached, Receipt, State2} = ln_effect:request(Spec, scope, ?MODULE, State1),
   > Receipt =:= hd(ln_effect:get_receipts(State1)).
   true
   ```

4. **Test VM integration**:
   ```erlang
   > Plan = ln_plan:seq([ln_plan:effect(#{module => ..., function => ...}), ln_plan:task(done)]),
   > {ok, Bytecode} = ln_compile:compile(Plan),
   > VM = ln_vm:init(Bytecode),
   > {ok, VM2} = ln_vm:step(VM),
   > %% Effect should have executed
   ```

## Migration Notes

### Breaking Changes
1. **ln_effect:request/4** signature unchanged, but return value may include `{ok, cached, Receipt, State}` when idempotency key matches
2. **ln_effect:complete/3** signature unchanged
3. **ln_receipt:new/2** changed to **ln_receipt:new/3** (adds idempotency_key parameter)
4. **ln_vm:state()** record has new fields (effect_state, waiting_for_effect)

### Migration Path
1. Deploy Phase 1-3 (UUID, idempotency, persistence) - backward compatible
2. Deploy Phase 4-5 (compensation, async handlers) - opt-in via effect_spec fields
3. Deploy Phase 6-7 (VM integration, tracing) - requires workflow recompilation

### Rollback Strategy
Each phase can be independently reverted by:
1. Removing new modules (ln_uuid, wf_effect_handler, ln_receipt_store)
2. Restoring old function signatures
3. Reverting record field changes

## References

### Code References (from research)
- `/Users/sac/cre/src/ln_effect.erl:1-177` - Basic effect boundary (needs UUID, idempotency, persistence)
- `/Users/sac/cre/src/ln_receipt.erl:1-285` - Receipt generation (needs persistence, idempotency index)
- `/Users/sac/cre/src/ln_vm.erl:1-233` - Bytecode executor (needs effect yield/resume)
- `/Users/sac/cre/src/ln_compile.erl:1-241` - Compiler (needs effect opcodes)
- `/Users/sac/cre/src/ln_plan.erl:1-195` - Plan constructors (needs effect term)
- `/Users/sac/cre/src/ln_trace.erl:1-138` - Tracing (needs effect event emission)
- `/Users/sac/cre/src/ln_cancel.erl:1-733` - Cancellation (integrates with effect scope)
- `/Users/sac/cre/src/pnet/pnet_receipt.erl:1-184` - Petri net receipts (different system, do not modify)

### Pattern References
- Receipt generation: `pnet_receipt.erl:102-127`
- ETS storage: `ln_receipt.erl:247-253`
- Event emission: `ln_trace.erl:76-89`
- State management: `ln_effect.erl:77-82`

### Open Questions (all resolved during planning)
1. **UUID generation**: Use `crypto:strong_rand_bytes/1` (blocking but sufficient for initial implementation)
2. **Idempotency scope**: Per-scope (case_id) via `{ScopeId, IdempotencyKey}` tuple key
3. **Persistence backend**: DETS for simplicity (single-node), can migrate to Mnesia later for distributed
4. **Effect handler supervision**: One-shot gen_server per effect (simplest error isolation)
5. **Effect timeout**: Configurable via `effect_spec.options.timeout`, default 30 seconds
6. **Compensation on failure**: Explicit compensation on cancellation only, not automatic on failure
7. **Replay with effects**: Use cached results from receipts in replay mode (Phase 6)
8. **Receipt storage bounding**: Manual archival for now (future enhancement)
9. **Effect security**: Whitelist allowed modules in wf_effect_handler (Phase 5)
10. **Backwards compatibility**: All changes backward compatible except ln_receipt:new/2 → new/3

### Dependencies on Other Work Items
- **Item 012** (Reducer/executor hot loop): Needs effect opcodes from Phase 6
- **Item 014** (Cancellation semantics): Needs effect compensation from Phase 4
- **Item 017** (Tracing and replay): Needs effect trace events from Phase 7
- **Item 021** (Pattern implementations): May use effects for external actions

### Estimated Timeline
- Phase 1: 1-2 days (UUID generation)
- Phase 2: 2-3 days (Idempotency)
- Phase 3: 3-4 days (Persistence)
- Phase 4: 2-3 days (Compensation)
- Phase 5: 4-5 days (Async handlers)
- Phase 6: 5-7 days (VM integration)
- Phase 7: 2-3 days (Tracing)

**Total**: 19-27 days (approximately 4-5 weeks)

### Risk Mitigation
| Risk | Mitigation |
|------|------------|
| UUID collision | Use crypto:strong_rand_bytes/1, 122 bits of randomness |
| Idempotency false positives | Include scope_id in idempotency key index |
| DETS corruption | Regular backups, fallback to in-memory ETS |
| Compensation failure | Log errors, continue with other compensations |
| Handler process leaks | Supervision tree, automatic termination |
| VM integration bugs | Incremental testing, mock effects |
| Trace event loss | Use ETS with trim_events, bounded buffer |
| Performance degradation | Benchmark each phase, optimize bottlenecks |
