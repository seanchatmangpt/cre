# Research: Effect system with receipts

**Date**: 2025-01-14
**Item**: 016-effect-system-with-receipts

## Research Question
Pure workflow reduction needs controlled interaction with external world. Must ensure effects are mediated, tracked, replayable, and support compensation without side-effects during pure execution.

**Motivation:** Separates pure workflow logic from external effects, enables deterministic replay (effects can be mocked), provides audit trail via receipts, supports cancellation and compensation of external actions.

**Success criteria:**
- All effects mediated through wf_effect manager
- Each effect has unique causal ID
- Receipts enable idempotency
- Effects can be canceled when supported
- Trace events record all effect invocations

**Technical constraints:**
- task/2 yields {effect, Spec, ContCtx}
- wf_effect executes Spec and returns Result
- Reducer resumes with Result and produces receipt
- No direct side effects in reducer loop

**Signals:** priority: high, urgency: Critical for real-world workflows

## Summary

The CRE codebase has **two parallel execution layers** that need effect system integration:

1. **Existing Petri Net Layer** (`gen_pnet`, `gen_yawl`) - Production workflow execution engine
2. **Emerging Linear Nesting Layer** (`ln_*` modules) - New bytecode-based execution system

**Key Finding:** The `ln_effect` module **already exists** (`/Users/sac/cre/src/ln_effect.erl:1-177`) with a basic effect boundary implementation, but it has **critical gaps** for production use:

- ✅ Basic effect request/complete lifecycle exists
- ❌ No unique causal ID generation (uses `make_ref()` which is not globally unique)
- ❌ No idempotency mechanism (receipts don't enable deduplication)
- ❌ No proper integration with reducer execution loop
- ❌ Limited effect handler support (only in-process `default_handler`)
- ❌ No compensation/undo operations
- ❌ No integration with bytecode executor (`ln_vm`) execution flow

**Additional Finding:** The codebase has **two receipt systems**:
1. `pnet_receipt` (`/Users/sac/cre/src/pnet/pnet_receipt.erl`) - Petri net transition receipts
2. `ln_receipt` (`/Users/sac/cre/src/ln_receipt.erl`) - Linear Nesting effect receipts

These systems are **disconnected** and serve different purposes. The effect system needs to bridge them while adding missing capabilities.

## Current State Analysis

### Existing Implementation

#### 1. Effect Boundary Module (`ln_effect`)

**Location**: `/Users/sac/cre/src/ln_effect.erl:1-177`

**Current capabilities:**

```erlang
-type effect_spec() :: #{
    module => module(),
    function => atom(),
    args => [term()],
    options => map()
}.

-record(pending_effect, {
    effect_id :: effect_id(),          % Uses make_ref() - NOT globally unique
    spec :: effect_spec(),
    scope_id :: scope_id(),
    callback_mod :: module(),
    continuation :: term(),
    status :: effect_status(),
    started_at :: integer()
}).
```

**API:**
- `init/1` - Initialize effect state with handler
- `request/4` - Request new effect (generates `effect_id` with `make_ref()`)
- `complete/3` - Complete effect with result, generate receipt
- `cancel_effects/2` - Cancel all effects in a scope
- `get_pending/1` - Get pending effects
- `get_receipts/1` - Get all receipts
- `default_handler/1` - In-process effect handler (for testing)

**Critical gaps:**
1. **No unique causal IDs** - `make_ref()` is unique only within the VM node, not distributed
2. **No idempotency** - No way to check if an effect was already executed
3. **No persistent receipts** - Receipts stored in-memory, lost on crash
4. **No compensation** - No way to undo completed effects
5. **No async handlers** - Only supports in-process `apply(Module, Function, Args)`

#### 2. Receipt System (`ln_receipt`)

**Location**: `/Users/sac/cre/src/ln_receipt.erl:1-285`

**Current capabilities:**

```erlang
-record(receipt, {
    effect_id :: reference(),          % From make_ref()
    spec_hash :: binary(),              % SHA-256 hash
    created_at :: integer(),
    completed_at :: integer(),
    result_summary :: term(),
    scope_id :: scope_id()
}).
```

**API:**
- `new/2` - Create receipt for executed effect
- `hash/1` - Compute SHA-256 hash of effect spec
- `is_equal/2` - Compare receipts
- `summarize/1` - Summarize result for compact storage
- `add/2` - Add receipt to ETS storage
- `lookup/2` - Look up receipts by scope

**Strengths:**
- ✅ Uses SHA-256 for spec hashing (good for deduplication)
- ✅ ETS-based storage (supports concurrent access)
- ✅ Scope-based indexing (supports efficient queries)
- ✅ Timestamp tracking (supports ordering)

**Gaps:**
1. **Spec hash alone insufficient for idempotency** - Two identical specs might be different effects
2. **No global effect ID** - Cannot correlate effects across nodes
3. **No receipt persistence** - ETS tables are volatile
4. **No compensation metadata** - No undo information

#### 3. Bytecode VM (`ln_vm`)

**Location**: `/Users/sac/cre/src/ln_vm.erl:1-150+`

**Current state:**
- Executes bytecode opcodes produced by `ln_compile`
- Has frame stack, join state, scope tracking
- **No effect integration** - Opcodes exist but no effect handling in execution loop

**Relevant opcodes from `ln_compile.erl:26-39`:**
```erlang
-type opcode() :: {op_task_start, atom()}
                | {op_task_complete, term()}
                | {op_fork, [label()], label()}
                | {op_join_wait, join_id(), label()}
                | {op_scope_enter, scope_id()}
                | {op_scope_exit, scope_id()}
                %% ... other opcodes
                | {op_wait_check, term(), label()}
                | {op_halt, term()}.
```

**Missing:**
- No `op_effect_yield` or `op_effect_resume` opcodes
- No effect handler coordination in VM state
- No effect resumption mechanism in `step/1` loop

#### 4. Tracing Infrastructure (`ln_trace`)

**Location**: `/Users/sac/cre/src/ln_trace.erl:1-138`

**Current capabilities:**
- Event emission with timestamps
- Event buffering with configurable levels
- Event export (map, list, json)
- Sequence numbering

**Event types:**
```erlang
-type event_type() :: case_started
                     | step_started
                     | step_completed
                     | effect_requested    % Exists but not integrated
                     | effect_completed    % Exists but not integrated
                     | scope_cancelled
                     | case_completed
                     | case_failed.
```

**Gap:** Effect events are defined but not emitted by `ln_effect` module.

### Key Files

#### Effect System Files

1. **`/Users/sac/cre/src/ln_effect.erl:1-177`**
   - **Purpose**: Effect boundary for external side effects
   - **Current status**: Basic implementation exists
   - **Key gaps**: No unique IDs, no idempotency, no compensation
   - **Integration point**: Should integrate with `ln_vm` executor

2. **`/Users/sac/cre/src/ln_receipt.erl:1-285`**
   - **Purpose**: Receipt generation and storage for effects
   - **Current status**: Basic receipt with ETS storage
   - **Strengths**: SHA-256 hashing, scope indexing
   - **Gaps**: No persistence, no compensation metadata

3. **`/Users/sac/cre/src/ln_compile.erl:1-241`**
   - **Purpose**: Plan to bytecode compiler
   - **Relevance**: Defines opcodes, needs effect opcodes
   - **Gap**: No `op_effect_yield`, `op_effect_resume` opcodes

4. **`/Users/sac/cre/src/ln_vm.erl:1-150+`**
   - **Purpose**: Virtual machine for bytecode execution
   - **Current status**: Executes basic opcodes
   - **Gap**: No effect handling in execution loop
   - **Integration needed**: Effect yield/resume in `step/1`

5. **`/Users/sac/cre/src/ln_trace.erl:1-138`**
   - **Purpose**: Structured event tracing
   - **Current status**: Buffering and export works
   - **Gap**: Effect events defined but not emitted
   - **Integration needed**: `ln_effect` should emit trace events

#### Related Infrastructure

6. **`/Users/sac/cre/src/pnet/pnet_receipt.erl:1-184`**
   - **Purpose**: Petri net transition receipts
   - **Relevance**: Different from effect receipts, shows pattern
   - **Pattern**: Immutable receipts with before/after hashes

7. **`/Users/sac/cre/src/ln_cancel.erl:1-100+`**
   - **Purpose**: Hierarchical cancellation management
   - **Relevance**: Effects need cancellation support
   - **Integration**: `ln_effect:cancel_effects/2` uses scopes

8. **`/Users/sac/cre/src/wf/wf_engine.hrl:1-43`**
   - **Purpose**: Workflow engine state records
   - **Relevance**: Shows receipts list in case state
   - **Pattern**: `#wf_case{receipts = [] :: [term()]}`

9. **`/Users/sac/cre/src/ln_plan.erl:1-100+`**
   - **Purpose**: Plan term constructors for workflows
   - **Relevance**: Plans compile to bytecode
   - **Gap**: No effect constructor (should have `{effect, Spec}`)

#### Supporting Files

10. **`/Users/sac/cre/src/ln_join.erl`** - Join state management
11. **`/Users/sac/cre/src/ln_defer.erl`** - Deferred choice patterns
12. **`/Users/sac/cre/src/ln_budget.erl`** - Quanta management for executor
13. **`/Users/sac/cre/src/ln_sched.erl`** - Deterministic scheduling
14. **`/Users/sac/cre/src/ln_introspect.erl`** - Runtime introspection

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- `ln_vm` - Bytecode executor (needs effect handling in hot loop)
- `ln_compile` - Compiler (needs effect opcodes)
- `ln_trace` - Tracing (needs effect event emission)
- `ln_cancel` - Cancellation (effect scope cancellation)
- `ln_receipt` - Receipt storage (needs persistence)
- `ln_plan` - Plan terms (needs effect constructors)
- `pnet_receipt` - Petri net receipts (different system, avoid confusion)

**External dependencies:**
- Erlang/OTP `gen_server` for effect handler processes
- ETS tables for receipt storage (already used)
- `crypto:hash/2` for spec hashing (already used)
- UUID library for unique causal IDs (need to add)

### Patterns to Follow

**1. Pure functional state management:**
From `ln_effect.erl:77-82`:
```erlang
init(Handler) ->
    #effect_state{
        pending = #{},
        receipts = [],
        handler = Handler
    }.
```
- Keep state in records, not process state
- Return updated state from all operations
- No side effects in pure functions

**2. Receipt generation pattern:**
From `pnet_receipt.erl:102-107`:
```erlang
make(BeforeHash, AfterHash, Move)
  when is_binary(BeforeHash), is_binary(AfterHash), is_map(Move) ->
    #{
      before_hash => BeforeHash,
      after_hash => AfterHash,
      move => Move,
      ts => timestamp()
    }.
```
- Validate inputs with guards
- Include timestamp for ordering
- Return immutable maps

**3. ETS storage pattern:**
From `ln_receipt.erl:247-253`:
```erlang
add(#storage{table = Table, scope_index = ScopeIndex} = Storage,
    #receipt{effect_id = EffectId, scope_id = ScopeId} = Receipt) ->
    true = ets:insert(Table, {EffectId, Receipt}),
    true = ets:insert(ScopeIndex, {ScopeId, EffectId}),
    Storage.
```
- Use separate tables for primary and secondary indexes
- Return updated storage handle
- Use `true =` for assertion

**4. Tracing integration:**
From `ln_trace.erl:76-89`:
```erlang
emit(#{timestamp := _} = Event, #trace_state{events = Events, max_events = Max, seq = Seq} = State) ->
    NewEvents = [Event#{seq => Seq} | Events],
    Trimmed = trim_events(NewEvents, Max),
    State#trace_state{events = Trimmed, seq = Seq + 1};
emit(EventType, State) ->
    emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => EventType,
        data => #{}
    }, State).
```
- Accept full event or event type
- Add sequence number automatically
- Trim to max_events limit

### Architecture Recommendations

**Effect system architecture:**

```erlang
%% Unique causal ID generation
-type causal_id() :: <<_:128>>.  % UUID v4

%% Effect spec with idempotency key
-type effect_spec() :: #{
    idempotency_key => binary(),     % Client-provided for deduplication
    module => module(),
    function => atom(),
    args => [term()],
    options => map()
}.

%% Effect receipt with compensation
-type effect_receipt() :: #{
    causal_id => causal_id(),         % Globally unique
    idempotency_key => binary(),      % For deduplication
    spec_hash => binary(),            % SHA-256 of spec
    spec => effect_spec(),
    result => term() | cancelled,
    compensation => fun(() -> ok) | undefined,
    started_at => integer(),
    completed_at => integer(),
    scope_id => scope_id()
}.

%% Effect handler behavior
-callback execute(effect_spec()) -> {ok, term()} | {error, term()}.
-callback compensate(effect_spec(), term()) -> ok | {error, term()}.
```

**Executor integration:**

```erlang
%% VM state with effect support
-record(vm_state, {
    pc :: pc(),
    frames :: #{frame_id() => frame()},
    current_frame :: frame_id() | undefined,
    stack :: [frame_id()],
    joins :: #{join_id() => #join_state{}},
    scopes :: #{scope_id() => running | cancelling | cancelled},
    %% NEW: Effect support
    effect_state :: ln_effect:state() | undefined,
    effect_handler :: pid() | undefined,
    %% Existing fields
    result :: term() | undefined,
    status :: running | halted | blocked | waiting_effect
}).

%% Effect yield opcode
{op_effect_yield, effect_spec(), scope_id()}.

%% Executor step with effect handling
step(#vm_state{status = waiting_effect} = State) ->
    {blocked, State};
step(State) ->
    case fetch_instruction(State) of
        {ok, {_Label, {op_effect_yield, Spec, ScopeId}}, NewState} ->
            %% Yield to effect handler
            {effect, Spec, ScopeId, NewState};
        {ok, {_Label, Opcode}, NewState} ->
            execute_opcode(Opcode, NewState)
    end.
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **No unique causal IDs** | Critical - Cannot correlate effects across distributed system | Implement UUID v4 generation; integrate with case ID for traceability |
| **No idempotency mechanism** | High - Duplicate effect execution on retry | Add idempotency_key to effect spec; check receipt store before execution |
| **No receipt persistence** | High - Lost receipts on crash, no audit trail | Implement persistent receipt store (mnesia/dets); write-ahead logging |
| **No compensation/undo** | High - Cannot rollback effects on cancellation | Add compensation functor to receipt; execute on scope cancellation |
| **Effect handler not distributed** | Medium - Only in-process execution | Implement gen_server-based effect handler with async execution |
| **No VM integration** | High - Effects cannot be used in bytecode workflows | Add effect opcodes to ln_compile; implement yield/resume in ln_vm |
| **Tracing not integrated** | Low - Missing observability | Add trace event emission in ln_effect request/complete/cancel |
| **Two receipt systems** | Medium - Confusion between pnet_receipt and ln_receipt | Document distinct purposes; consider unification or clear separation |
| **Spec hash collisions** | Low - SHA-256 collision probability negligible | Accept risk; add spec size to receipt for verification |
| **Effect ordering in replay** | Medium - Need deterministic replay | Record effect execution order in trace; replay must preserve order |

## Recommended Approach

### Phase 1: Unique Causal IDs (Foundation)

**Objective:** Add globally unique identifiers for effects.

1. **Implement UUID generation:**
   ```erlang
   -module(ln_uuid).
   -export([new/0]).

   -type causal_id() :: <<_:128>>.

   -spec new() -> causal_id().
   new() ->
       %% UUID v4 random
       <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
       <<A:32, B:16, (C band 16#0fff):16, 4:4, D:12, 2#10:2, D:14, E:48>>.
   ```

2. **Add causal_id to effect_spec:**
   ```erlang
   -type effect_spec() :: #{
       causal_id => ln_uuid:causal_id(),
       idempotency_key => binary() | undefined,
       module => module(),
       function => atom(),
       args => [term()],
       options => map()
   }.
   ```

3. **Update ln_effect:request/4:**
   ```erlang
   request(Spec, ScopeId, CallbackMod, Cont) ->
       CausalId = maps:get(causal_id, Spec, ln_uuid:new()),
       PendingEffect = #pending_effect{
           effect_id = CausalId,  % Use UUID instead of make_ref()
           spec = Spec#{causal_id => CausalId},
           %% ... rest of fields
       },
       %% ... rest of function
   ```

**Estimated effort:** 1-2 days

### Phase 2: Idempotency Mechanism

**Objective:** Prevent duplicate effect execution.

1. **Add idempotency_key to effect_spec:**
   ```erlang
   request(#{idempotency_key := Key} = Spec, ScopeId, CallbackMod, Cont) ->
       %% Check if effect with this key already executed
       case find_receipt_by_key(Key, ScopeId) of
           {ok, Receipt} ->
               %% Return cached result
               {ok, cached, Receipt};
           not_found ->
               %% Proceed with effect execution
               CausalId = ln_uuid:new(),
               %% ... create pending effect
       end;
   request(Spec, ScopeId, CallbackMod, Cont) ->
       %% No idempotency key, proceed normally
   ```

2. **Implement receipt lookup by key:**
   ```erlang
   -spec find_receipt_by_key(binary(), scope_id()) -> {ok, receipt()} | not_found.
   find_receipt_by_key(Key, ScopeId) ->
       case ets:lookup(idempotency_index, {ScopeId, Key}) of
           [{{ScopeId, Key}, CausalId}] ->
               case ets:lookup(receipt_table, CausalId) of
                   [{CausalId, Receipt}] -> {ok, Receipt};
                   [] -> not_found
               end;
           [] ->
               not_found
       end.
   ```

3. **Update receipt storage:**
   ```erlang
   add(#storage{table = Table, idempotency_index = Idx} = Storage,
       #receipt{causal_id = Id, idempotency_key = Key, scope_id = ScopeId} = Receipt) ->
       true = ets:insert(Table, {Id, Receipt}),
       case Key of
           undefined -> ok;
           _ -> true = ets:insert(Idx, {{ScopeId, Key}, Id})
       end,
       Storage.
   ```

**Estimated effort:** 2-3 days

### Phase 3: Receipt Persistence

**Objective:** Persist receipts for audit trail and replay.

1. **Choose persistence strategy:**
   - **Option A:** Mnesia (distributed, in-memory)
   - **Option B:** DETS (disk-based, single-node)
   - **Option C:** Write-ahead log + ETS cache

2. **Implement persistent receipt store:**
   ```erlang
   -module(ln_receipt_store).
   -behaviour(gen_server).

   -record(state, {
       table :: ets:tid(),
       log_file :: file:filename()
   }).

   init([LogFile]) ->
       Table = ets:new(receipts, [set, public, {keypos, #receipt.causal_id}]),
       %% Replay log into ETS
       replay_log(LogFile, Table),
       {ok, #state{table = Table, log_file = LogFile}}.

   handle_cast({store, #receipt{} = Receipt}, #state{table = Table, log_file = Log} = State) ->
       true = ets:insert(Table, Receipt),
       append_to_log(Log, Receipt),
       {noreply, State}.
   ```

3. **Add to ln_effect:complete/3:**
   ```erlang
   complete(EffectId, Result, #effect_state{} = State) ->
       %% ... create receipt
       ok = ln_receipt_store:store(Receipt),
       %% ... update state
   ```

**Estimated effort:** 3-4 days

### Phase 4: Compensation Support

**Objective:** Enable effect rollback on cancellation.

1. **Add compensation to effect_spec:**
   ```erlang
   -type effect_spec() :: #{
       %% ... existing fields
       compensation => fun((term()) -> ok) | undefined
   }.
   ```

2. **Update receipt to store compensation:**
   ```erlang
   -type receipt() :: #{
       %% ... existing fields
       compensation => fun(() -> ok) | undefined
   }.
   ```

3. **Implement compensation execution:**
   ```erlang
   -spec compensate(receipt()) -> ok | {error, term()}.
   compensate(#{compensation := undefined}) ->
       ok;
   compensate(#{compensation := CompFun, result := Result}) ->
       try CompFun(Result) of
           ok -> ok;
           {error, _} = Error -> Error
       catch
           _:_ -> {error, compensation_failed}
       end.
   ```

4. **Integrate with cancellation:**
   ```erlang
   cancel_effects(ScopeId, #effect_state{receipts = Receipts} = State) ->
       %% Cancel pending effects
       %% ... existing code

       %% Compensate completed effects in scope
       Compensated = [compensate(R) || R <- Receipts,
                                      maps:get(scope_id, R) =:= ScopeId],
       %% ... update state
   ```

**Estimated effort:** 2-3 days

### Phase 5: Effect Handler Distribution

**Objective:** Support async effect execution in separate processes.

1. **Define effect handler behavior:**
   ```erlang
   -module(wf_effect_handler).
   -behaviour(gen_server).

   -callback init(Args :: term()) -> {ok, State :: term()}.
   -callback handle_execute(effect_spec(), State :: term()) ->
       {ok, Result :: term(), NewState :: term()} |
       {error, Reason :: term(), NewState :: term()}.
   -callback handle_compensate(effect_spec(), Result :: term(), State :: term()) ->
       {ok, NewState :: term()} |
       {error, Reason :: term(), NewState :: term()}.
   ```

2. **Implement handler supervisor:**
   ```erlang
   -module(wf_effect_handler_sup).
   -behaviour(supervisor).

   init(_) ->
       {ok, {{simple_one_for_one, 10, 60}, [
           #{id => effect_handler,
             start => {wf_effect_handler, start_link, []},
             restart => temporary,
             shutdown => 5000,
             type => worker,
             modules => [wf_effect_handler]}
       ]}}.
   ```

3. **Update ln_effect to use async handler:**
   ```erlang
   request(Spec, ScopeId, CallbackMod, Cont) ->
       CausalId = ln_uuid:new(),
       %% Start handler process
       {ok, HandlerPid} = wf_effect_handler_sup:start_handler(Spec),
       %% Send execute request
       gen_server:cast(HandlerPid, {execute, CausalId, Spec, self()}),

       %% Create pending effect waiting for response
       PendingEffect = #pending_effect{
           effect_id = CausalId,
           spec = Spec,
           handler_pid = HandlerPid,  % Track handler process
           %% ... rest of fields
       },
       %% ... rest of function
   ```

4. **Handle handler response:**
   ```erlang
   handle_info({effect_result, CausalId, Result}, #effect_state{pending = Pending} = State) ->
       case maps:get(CausalId, Pending) of
           #pending_effect{callback_mod = Mod, continuation = Cont} ->
               %% Resume execution with result
               Mod:resume(Cont, Result),
               %% Generate receipt
               complete(CausalId, Result, State)
       end.
   ```

**Estimated effort:** 4-5 days

### Phase 6: Bytecode VM Integration

**Objective:** Integrate effects into bytecode execution.

1. **Add effect opcodes to ln_compile:**
   ```erlang
   -type opcode() :: %% ... existing opcodes
                   | {op_effect_yield, effect_spec(), scope_id()}
                   | {op_effect_resume, label()}.

   %% Add effect constructor to ln_plan
   -export([effect/1]).

   effect(Spec) when is_map(Spec) ->
       {effect, Spec}.

   %% Compile effect plan
   compile_plan({effect, Spec}, Label, Joins, Scopes) ->
       YieldLabel = Label,
       ResumeLabel = Label + 1,
       Program = [
           {YieldLabel, {op_effect_yield, Spec, current_scope}},
           {ResumeLabel, {op_effect_resume, ResumeLabel + 1}}
       ],
       {Program, Joins, Scopes}.
   ```

2. **Update ln_vm state:**
   ```erlang
   -record(vm_state, {
       %% ... existing fields
       effect_state :: ln_effect:state() | undefined,
       effect_handler :: pid() | undefined,
       waiting_for_effect :: effect_id() | undefined
   }).

   init(#{program := _Program} = Bytecode) ->
       #vm_state{
           %% ... existing initialization
           effect_state = ln_effect:init(wf_effect_handler),
           effect_handler = undefined,
           waiting_for_effect = undefined
       }.
   ```

3. **Implement effect yield handling:**
   ```erlang
   execute_opcode({op_effect_yield, Spec, ScopeId}, #vm_state{effect_state = EffState} = State) ->
       {ok, EffectId, NewEffState} = ln_effect:request(Spec, ScopeId, ?MODULE, self()),

       %% Block VM until effect completes
       NewState = State#vm_state{
           effect_state = NewEffState,
           status = waiting_effect,
           waiting_for_effect = EffectId
       },
       {blocked, NewState};

   execute_opcode({op_effect_resume, NextLabel}, #vm_state{pc = _PC} = State) ->
       %% Resume execution at next label
       NewState = State#vm_state{
           status = running,
           waiting_for_effect = undefined
       },
       {ok, set_program_counter(NextLabel, NewState)}.
   ```

4. **Add effect result handling:**
   ```erlang
   handle_info({effect_complete, EffectId, Result}, #vm_state{waiting_for_effect = EffectId} = State) ->
       %% Generate receipt
       {ok, NewEffState} = ln_effect:complete(EffectId, Result, State#vm_state.effect_state),

       %% Resume VM execution
       {ok, NewState} = step(State#vm_state{
           effect_state = NewEffState
       }),
       {noreply, NewState};
   ```

**Estimated effort:** 5-7 days

### Phase 7: Tracing Integration

**Objective:** Emit trace events for all effect operations.

1. **Add trace state to ln_effect:**
   ```erlang
   -record(effect_state, {
       pending :: pending(),
       receipts :: [receipt()],
       handler :: handler(),
       trace :: ln_trace:state()  % NEW
   }).

   init(Handler) ->
       #effect_state{
           pending = #{},
           receipts = [],
           handler = Handler,
           trace = ln_trace:new(#{level => full, max_events => 10000})
       }.
   ```

2. **Emit events in request/4:**
   ```erlang
   request(Spec, ScopeId, CallbackMod, #effect_state{trace = Trace} = State) ->
       CausalId = ln_uuid:new(),
       Event = #{
           type => effect_requested,
           data => #{
               causal_id => CausalId,
               spec_hash => ln_receipt:hash(Spec),
               scope_id => ScopeId
           }
       },
       NewTrace = ln_trace:emit(Event, Trace),
       %% ... rest of function with updated trace
   ```

3. **Emit events in complete/3:**
   ```erlang
   complete(EffectId, Result, #effect_state{trace = Trace} = State) ->
       %% ... generate receipt

       Event = #{
           type => effect_completed,
           data => #{
               causal_id => EffectId,
               result => Result,
               receipt => Receipt
           }
       },
       NewTrace = ln_trace:emit(Event, Trace),

       NewState = State#effect_state{
           receipts = [Receipt | Receipts],
           trace = NewTrace
       },
       {ok, NewState}.
   ```

4. **Emit events in cancel_effects/2:**
   ```erlang
   cancel_effects(ScopeId, #effect_state{trace = Trace} = State) ->
       %% ... cancel effects

       Event = #{
           type => effect_cancelled,
           data => #{
               scope_id => ScopeId,
               cancelled_count => length(ToCancel)
           }
       },
       NewTrace = ln_trace:emit(Event, Trace),

       %% ... update state with new trace
   ```

5. **Export trace events:**
   ```erlang
   -spec get_trace_events(state()) -> [ln_trace:event()].
   get_trace_events(#effect_state{trace = Trace}) ->
       ln_trace:get_all(Trace).
   ```

**Estimated effort:** 2-3 days

## Open Questions

1. **UUID generation strategy:** Should we use crypto:strong_rand_bytes/1 (blocking) or implement a non-blocking UUID generator with entropy pooling?
   - **Recommendation:** Use `crypto:strong_rand_bytes/1` initially (simpler), optimize later if needed

2. **Idempotency key scope:** Should idempotency keys be globally unique or per-scope?
   - **Recommendation:** Per-scope (case_id) to allow same effect in different workflows

3. **Receipt persistence backend:** Mnesia (distributed, in-memory) or DETS (disk-based, single-node)?
   - **Recommendation:** Start with DETS for simplicity, migrate to Mnesia for distributed deployment

4. **Effect handler supervision:** Should each effect have its own handler process, or should handlers be pooled?
   - **Recommendation:** One-shot handler processes per effect (simpler error isolation)

5. **Effect timeout:** Should effects have a default timeout? How should timeouts be handled?
   - **Recommendation:** Configurable timeout per effect spec, default 30 seconds; timeout generates error receipt

6. **Compensation on failure:** Should compensation be automatic on effect failure, or explicit?
   - **Recommendation:** Explicit compensation triggered by cancellation, not automatic on failure

7. **Effect ordering in parallel:** When multiple effects are in-flight, should we preserve completion order?
   - **Recommendation:** No, effects complete asynchronously; use causal IDs for correlation

8. **Replay with effects:** How do we replay effects without re-executing side effects?
   - **Recommendation:** Replay mode uses cached results from receipts; effect handler not called

9. **Receipt storage size:** How do we bound receipt storage for long-running workflows?
   - **Recommendation:** Implement receipt archival; move old receipts to cold storage

10. **Effect security:** How do we prevent malicious effect specs from executing arbitrary code?
    - **Recommendation:** Whitelist allowed modules/functions in effect handler; validate specs

11. **Backwards compatibility:** How do we migrate existing workflows that don't use effects?
    - **Recommendation:** Effect system is opt-in; existing workflows continue to work

12. **Performance impact:** What is the overhead of effect tracking for high-frequency effects?
    - **Recommendation:** Benchmark with 1000+ effects/second; optimize if needed

## Recommendations

1. **Immediate priorities:**
   - Implement UUID generation for unique causal IDs
   - Add idempotency mechanism with receipt lookup
   - Integrate effect tracing

2. **Architecture decisions:**
   - Keep `ln_effect` as pure functional state manager
   - Use separate gen_server for effect handler processes
   - Integrate effects into `ln_vm` with yield/resume opcodes
   - Maintain separate receipt systems (pnet_receipt vs ln_receipt) but document clearly

3. **Testing strategy:**
   - Unit tests for UUID generation (uniqueness, format)
   - Property-based tests for idempotency (duplicate calls return same result)
   - Integration tests for effect lifecycle (request → execute → complete → receipt)
   - Cancellation tests (compensation execution, scope cleanup)
   - Replay tests (effects use cached results in replay mode)

4. **Documentation needs:**
   - Effect system architecture document
   - Unique causal ID specification
   - Idempotency key usage guide
   - Compensation pattern documentation
   - Receipt storage and persistence guide

5. **Migration path:**
   - Phase 1-3: Core effect infrastructure (UUID, idempotency, persistence)
   - Phase 4-5: Advanced features (compensation, async handlers)
   - Phase 6-7: Integration (VM, tracing)
   - Each phase can be deployed independently

6. **Dependencies to coordinate with:**
   - Item 012 (Reducer/executor hot loop) - needs effect opcodes
   - Item 014 (Cancellation semantics) - needs effect compensation
   - Item 017 (Tracing and replay) - needs effect trace events
   - Item 021 (Pattern implementations) - may use effects for external actions

7. **Performance considerations:**
   - ETS receipt storage: ~1000 writes/sec per node
   - UUID generation: crypto:strong_rand_bytes may block under entropy starvation
   - Effect handler processes: one gen_server per effect (lightweight)
   - Idempotency checks: O(1) with ETS index

8. **Security considerations:**
   - Validate effect specs before execution
   - Whitelist allowed modules/functions
   - Sandboxing for untrusted effects
   - Audit trail via receipts

9. **Observability:**
   - Trace events for all effect operations
   - Metrics: effect count, latency, error rate
   - Receipt export for audit
   - Causal ID correlation across distributed systems

10. **Future enhancements:**
    - Effect batching for bulk operations
    - Effect composition (chained effects)
    - Circuit breaker for failing effects
    - Effect retry with exponential backoff
    - Distributed effect coordination across nodes
