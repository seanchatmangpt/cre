# Multiple Instance Pattern Implementations - Implementation Plan

## Implementation Plan Title
Unified Multiple Instance Pattern Support with Join Policies and Dynamic Spawning

## Overview

Implement a comprehensive multiple instance (MI) pattern framework for the CRE workflow substrate that supports all major synchronization variants (all, first-n, n-of-m, discriminator), both fixed and dynamic instance spawning, and integration with the existing gen_yawl Petri net infrastructure while aligning with the long-term wf_term bytecode vision.

The current codebase has fragmented MI support across multiple modules (`wf_mi` utilities, `n_out_of_m` pattern, `multiple_instances_sync`, `blocking_discriminator`) but lacks a unified API and complete coverage of synchronization variants. This implementation will consolidate MI patterns into a cohesive, production-ready framework.

## Current State

### Existing Implementations

1. **wf_mi.erl** (`/Users/sac/cre/src/wf/wf_mi.erl:1-497`)
   - Pure utility functions for MI detection, evaluation, token creation
   - Supports instance count evaluation with min/max bounds
   - No synchronization logic (utilities only)
   - Status: ✅ Functional, keep as-is

2. **n_out_of_m.erl** (`/Users/sac/cre/src/patterns/n_out_of_m.erl:1-657`)
   - **Production-ready reference implementation** for WCP-22 quorum join
   - Complete gen_yawl behavior with Petri net semantics
   - XES logging integration
   - Pure functional design with comprehensive documentation
   - Status: ✅ Model for all new MI patterns

3. **multiple_instances_sync.erl** (`/Users/sac/cre/src/patterns/multiple_instances_sync.erl:1-776`)
   - WCP-12 implementation (all instances sync)
   - Hardcoded for 4 instances only
   - Follows gen_yawl pattern correctly
   - Status: ⚠️ Limited, needs generalization

4. **blocking_discriminator.erl** (`/Users/sac/cre/src/patterns/blocking_discriminator.erl:1-83`)
   - WCP-09 discriminator (first completion triggers)
   - Standalone pattern, not integrated with MI framework
   - Minimal implementation
   - Status: ⚠️ Needs integration

5. **static_partial_join_mi.erl** (`/Users/sac/cre/src/patterns/static_partial_join_mi.erl:1-87`)
   - Basic static N-of-M implementation
   - Minimal Petri net structure
   - No result aggregation
   - Status: ⚠️ Incomplete

6. **wf_term.erl** (`/Users/sac/cre/src/wf/wf_term.erl:1-500+`)
   - **Already implements `mi(Policy, Body)` constructor** (line 226-229)
   - Supports `{fixed, N}` and `{dynamic, CollectorFun}` policies
   - Status: ✅ Pattern algebra exists (Items 010/011 mostly complete)

7. **wf_compile.erl** (`/Users/sac/cre/src/wf/wf_compile.erl:1-500+`)
   - **Already compiles `mi()` terms to MI_SPAWN/MI_JOIN opcodes** (line 182-187)
   - Status: ✅ Compiler support exists

### Critical Gaps

1. **Missing gen_yawl pattern implementations:**
   - No `mi_all_pattern.erl` (generalized "wait for all M")
   - No `mi_first_n_pattern.erl` (proceed after N complete)
   - No unified `mi_pattern.erl` facade

2. **Incomplete wf_exec support:**
   - Opcodes defined (MI_SPAWN, MI_JOIN) but executor may lack handlers
   - Need to verify `wf_exec.erl` has MI opcode implementations

3. **Test suite issues:**
   - Tests reference `cre_yawl_patterns` module which doesn't exist
   - `yawl_multiple_instances_test.erl:194+` calls non-existent API

4. **Cancellation integration:**
   - Item 014 (cancellation) not integrated with MI patterns
   - No per-instance cancellation scopes

## Desired End State

A unified multiple instance framework with:

1. **Complete gen_yawl pattern suite:**
   - `mi_all_pattern` - Wait for all M instances (generalized from multiple_instances_sync)
   - `mi_first_n_pattern` - Proceed after first N complete (NEW)
   - `mi_discriminator_pattern` - First completion wins, cancel rest (integrate blocking_discriminator)
   - `mi_n_of_m_pattern` - Already exists as n_out_of_m.erl, keep as-is
   - `mi_dynamic_pattern` - Data-driven spawning with backpressure (WCP-15)

2. **Unified facade module:**
   - `mi_pattern.erl` with `execute/3` API
   - Validates specs and policies
   - Dispatches to appropriate gen_yawl pattern

3. **Bytecode execution support:**
   - Verify wf_exec handles MI_SPAWN/MI_JOIN opcodes
   - If missing, implement handlers

4. **Test suite fixes:**
   - Update tests to use new `mi_pattern` facade
   - Remove dependency on non-existent `cre_yawl_patterns`

5. **Cancellation integration:**
   - Per-instance cancellation scopes (align with item 014)
   - trigger/3 callback for cancellation token filtering

### Key Discoveries

- **Items 010/011 are mostly complete:** `wf_term:mi/2` and `wf_compile` already handle MI patterns (wf_term.erl:226-229, wf_compile.erl:182-187)
- **Reference implementation exists:** `n_out_of_m.erl` is the production-ready model for all gen_yawl MI patterns
- **Fragmentation is the main issue:** Multiple partial implementations exist but no unified API
- **Test gap is blocking:** Tests call `cre_yawl_patterns` which doesn't exist

## What We're NOT Doing

- **NOT replacing gen_yawl patterns:** Keep Petri net patterns for backward compatibility
- **NOT implementing full Item 010/011 scope:** Pattern algebra and compiler are mostly done, focus on gen_yawl patterns
- **NOT changing wf_mi utilities:** The pure functional utilities are working correctly
- **NOT breaking existing YAWL XML specs:** Maintain backward compatibility
- **NOT implementing distributed MI execution:** All instances run in same OTP node for now
- **NOT implementing persistent instance state:** In-memory only (ETS can be added later)

## Implementation Approach

### Strategy: Hybrid Gen_Yawl + Bytecode Migration

**Phase 1 (Item 023 scope):** Complete gen_yawl patterns for immediate use
**Phase 2 (future):** Migrate to bytecode execution as Items 010/011/012 mature

**Rationale:**
- gen_yawl patterns are battle-tested and well-understood
- Bytecode VM (wf_exec) may need additional work
- Hybrid approach provides migration path
- Users get working MI patterns now, optimized execution later

---

## Phases

### Phase 1: Create Missing Gen_Yawl MI Patterns

#### Overview
Implement the missing synchronization variants as standalone gen_yawl modules, following the `n_out_of_m.erl` reference implementation.

#### Changes Required:

##### 1. Create `mi_all_pattern.erl` (Generalized "Wait for All M")
**File**: `/Users/sac/cre/src/patterns/mi_all_pattern.erl` (NEW)
**Changes**: Create new gen_yawl module

```erlang
-module(mi_all_pattern).
-moduledoc """
Multiple Instances - All Synchronization Pattern (WCP-12 Generalized).

Waits for all M instances to complete before proceeding.
Extends multiple_instances_sync.erl to support arbitrary M.
""".
-behaviour(gen_yawl).

%% gen_yawl callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3, trigger/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([new/2, start/2, run/2, execute/2]).

-record(state, {
    subprocess :: function(),
    instance_count :: pos_integer(),
    completed = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    log_id :: binary() | undefined
}).

%% Petri net places
place_lst() ->
    [p_start, p_spawn, p_instance_pool, p_active, p_complete, p_all_done, p_end].

%% Transitions
trsn_lst() ->
    [t_spawn, t_execute, t_complete, t_sync, t_finish].

%% Implementation follows n_out_of_m.erl pattern with N=M for "all" policy
```

**Key design decisions:**
- Use single `p_active` place with list of instance tokens (not hardcoded p_active_1/2/3/4)
- Support arbitrary instance count M (not limited to 4)
- XES logging for process mining
- Pure functional fire/3 with 3-tuple returns for state updates

##### 2. Create `mi_first_n_pattern.erl` (Proceed After N Complete)
**File**: `/Users/sac/cre/src/patterns/mi_first_n_pattern.erl` (NEW)
**Changes**: Create new gen_yawl module

```erlang
-module(mi_first_n_pattern).
-moduledoc """
Multiple Instances - First N Pattern.

Proceeds when first N instances complete, continues waiting for remaining M-N instances.
""".
-behaviour(gen_yawl).

-record(state, {
    subprocess :: function(),
    instance_count :: pos_integer(),
    threshold :: pos_integer(),  % N: proceed after this many complete
    completed = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    threshold_met = false :: boolean(),
    log_id :: binary() | undefined
}).

%% Petri net: similar to n_out_of_m but with different continuation semantics
%% After threshold met, produce output to p_proceed AND continue collecting
place_lst() ->
    [p_start, p_spawn, p_instance_pool, p_active, p_complete, p_threshold_met, p_proceed, p_all_done, p_end].
```

**Key design decisions:**
- Split output: `p_proceed` (trigger downstream) and `p_all_done` (track completion)
- Non-blocking: downstream flow starts after N complete, doesn't wait for all M
- Continue processing remaining instances after threshold
- Result aggregation: return first N results immediately, all M at completion

##### 3. Create `mi_discriminator_pattern.erl` (First Wins, Cancel Rest)
**File**: `/Users/sac/cre/src/patterns/mi_discriminator_pattern.erl` (NEW)
**Changes**: Create new gen_yawl module, integrate blocking_discriminator logic

```erlang
-module(mi_discriminator_pattern).
-moduledoc """
Multiple Instances - Discriminator Pattern (WCP-09).

First instance to complete triggers output and cancels all other instances.
""".
-behaviour(gen_yawl).

-record(state, {
    subprocess :: function(),
    instance_count :: pos_integer(),
    completed = [] :: [pos_integer()],
    winner = undefined :: pos_integer() | undefined,
    cancelled = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    log_id :: binary() | undefined
}).

%% Petri net includes cancellation propagation
place_lst() ->
    [p_start, p_spawn, p_instance_pool, p_active, p_complete, p_winner, p_cancel, p_end].

%% trigger/3 filters completion tokens after winner selected
trigger(p_active, {instance, Id}, #state{winner = undefined}) ->
    pass;
trigger(p_active, {instance, Id}, #state{winner = Winner}) when Winner =/= undefined ->
    %% Cancel all other instances
    {consume, [{instance, Id}]}.
```

**Key design decisions:**
- First completion wins, produces output immediately
- trigger/3 callback cancels remaining instance tokens
- Cancellation tokens propagate to instance execution
- XES log cancellation events for observability

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 compile` succeeds (all modules compile without errors)
- [ ] `rebar3 eunit` passes (existing tests still pass)
- [ ] Dialyzer type checking passes: `rebar3 dialyzer`
- [ ] New modules have doctests that pass: `erl -eval "eunit:test(mi_all_pattern, [verbose])"`

##### Manual Verification:
- [ ] Pattern modules follow n_out_of_m.erl structure (record definitions, callbacks)
- [ ] Petri net structures are sound (option to complete, proper completion, no dead transitions)
- [ ] XES logging events are emitted (check logs for spawn/complete/sync events)
- [ ] Code review confirms pure functional design (fire/3 has no side effects)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Create Unified Facade Module

#### Overview
Create `mi_pattern.erl` that provides a single API for all MI patterns, validating specs and dispatching to appropriate gen_yawl implementation.

#### Changes Required:

##### 1. Create `mi_pattern.erl` Facade
**File**: `/Users/sac/cre/src/patterns/mi_pattern.erl` (NEW)
**Changes**: Create unified API module

```erlang
-module(mi_pattern).
-moduledoc """
Unified Multiple Instance Pattern Facade.

Provides a single API for all MI pattern variants:
- Spec: {fixed, M} | {dynamic, DataFun, MaxInstances}
- JoinPolicy: all | {first_n, N} | {n_of_m, N, M} | discriminator

Examples:
  %% Fixed 5 instances, wait for all
  {ok, Results} = mi_pattern:execute({fixed, 5}, all, InstanceFuns).

  %% Dynamic with data source, proceed after first 10
  DataFun = fun() -> case get_data() of {ok, D} -> {more, D}; eof -> done end end,
  {ok, Results} = mi_pattern:execute({dynamic, DataFun, 100}, {first_n, 10}, []).

  %% Quorum: 3 of 5
  {ok, Results} = mi_pattern:execute({fixed, 5}, {n_of_m, 3, 5}, Funs).
""".
-export([execute/3, validate_spec/1, validate_join_policy/2, pattern_name/1]).

%% Spec types
-type mi_spec() :: {fixed, pos_integer()} | {dynamic, function(), pos_integer()}.
-type join_policy() :: all | {first_n, pos_integer()} | {n_of_m, pos_integer(), pos_integer()} | discriminator.
-type instance_funs() :: [function()].

-spec execute(Spec :: mi_spec(), JoinPolicy :: join_policy(), Funs :: instance_funs()) ->
    {ok, term()} | {error, term()}.

execute(Spec, JoinPolicy, Funs) ->
    case {validate_spec(Spec), validate_join_policy(Spec, JoinPolicy)} of
        {ok, ValidatedSpec}, {ok, ValidatedPolicy} ->
            Module = pattern_module(ValidatedPolicy),
            Module:start(ValidatedSpec, Funs);
        {error, Reason}, _ ->
            {error, {invalid_spec, Reason}};
        _, {error, Reason} ->
            {error, {invalid_policy, Reason}}
    end.

%% Validation helpers
validate_spec({fixed, M}) when is_integer(M), M > 0 -> {ok, {fixed, M}};
validate_spec({dynamic, Fun, Max}) when is_function(Fun, 0), is_integer(Max), Max > 0 -> {ok, {dynamic, Fun, Max}};
validate_spec(Spec) -> {error, {invalid_spec, Spec}}.

validate_join_policy({fixed, M}, all) -> {ok, all};
validate_join_policy({fixed, M}, {first_n, N}) when N =< M -> {ok, {first_n, N}};
validate_join_policy({fixed, M}, {n_of_m, N, M}) when N =< M -> {ok, {n_of_m, N, M}};
validate_join_policy({fixed, _M}, discriminator) -> {ok, discriminator};
validate_join_policy(Spec, Policy) -> {error, {policy_mismatch, Spec, Policy}}.

%% Dispatch
pattern_module(all) -> mi_all_pattern;
pattern_module({first_n, _N}) -> mi_first_n_pattern;
pattern_module({n_of_m, _N, _M}) -> n_out_of_m;  % Already exists
pattern_module(discriminator) -> mi_discriminator_pattern.
```

##### 2. Update Test Suite to Use Facade
**File**: `/Users/sac/cre/test/yawl_multiple_instances_test.erl:194+`
**Changes**: Replace `cre_yawl_patterns` calls with `mi_pattern` facade

```erlang
%% OLD (line 194):
Pattern = cre_yawl_patterns:implicit_termination(Subprocess),

%% NEW:
Pattern = mi_pattern:execute({fixed, 1}, all, [Subprocess]),
```

**Search and replace pattern:**
- `cre_yawl_patterns:multiple_instances_no_sync(N, Funs)` → `mi_pattern:execute({fixed, N}, all, Funs)`
- `cre_yawl_patterns:multiple_instances_design_time(N, M, Funs)` → `mi_pattern:execute({fixed, M}, {n_of_m, N, M}, Funs)`
- All other cre_yawl_patterns calls → appropriate mi_pattern calls

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 eunit` passes (updated tests execute correctly)
- [ ] Test coverage > 80% for mi_pattern.erl: `rebar3 cover`
- [ ] No compiler warnings about undefined functions

##### Manual Verification:
- [ ] Test suite executes all MI patterns successfully
- [ ] API is intuitive and well-documented
- [ ] Error messages are helpful for invalid specs/policies

**Note**: Pause for manual code review of facade API design before proceeding to Phase 3.

---

### Phase 3: Verify and Complete Bytecode Execution

#### Overview
Ensure the bytecode VM (wf_exec) properly handles MI_SPAWN and MI_JOIN opcodes. Implement handlers if missing.

#### Changes Required:

##### 1. Inspect wf_exec.erl for MI Opcode Handlers
**File**: `/Users/sac/cre/src/wf/wf_exec.erl` (inspect)
**Changes**: Determine if MI opcode handlers exist

**Search for:**
```erlang
%% Look for MI pattern handling
exec({mi_spawn, Policy}, State) ->
exec({mi_join, Policy}, State) ->
```

##### 2. Implement MI Handlers (if missing)
**File**: `/Users/sac/cre/src/wf/wf_exec.erl` (add if needed)
**Changes**: Add MI opcode execution logic

```erlang
%% Execute MI_SPAWN opcode
exec({mi_spawn, {fixed, M}}, State) ->
    %% Create M instance frames and push to stack
    InstanceFrames = [wf_vm:frame(mi, {instance, I}) || I <- lists:seq(1, M)],
    NewState = wf_vm:exec_push_frame(State, InstanceFrames),
    {continue, NewState};

exec({mi_spawn, {dynamic, DataFun, Max}}, State) ->
    %% Start dynamic instance collection
    case DataFun() of
        {more, Data} ->
            %% Spawn first instance, will loop for more
            InstanceFrame = wf_vm:frame(mi, {instance, 1, Data}),
            NewState = wf_vm:exec_push_frame(State, InstanceFrame),
            {continue, NewState};
        done ->
            %% No instances to spawn
            {continue, State}
    end;

%% Execute MI_JOIN opcode
exec({mi_join, {fixed, M}}, #{completed := Completed} = State) when length(Completed) =:= M ->
    %% All instances complete, pop frame
    NewState = wf_vm:exec_pop_frame(State),
    {continue, NewState};

exec({mi_join, {first_n, N}}, #{completed := Completed} = State) when length(Completed) >= N ->
    %% Threshold met, can proceed (may continue processing remaining)
    {continue, State};

exec({mi_join, _Policy}, State) ->
    %% Join not yet satisfied, wait
    {wait, State}.
```

**Note:** This is placeholder logic. Actual implementation depends on wf_exec state structure and may need integration with join counters and cancellation flags.

##### 3. Add MI Unit Tests for Bytecode Execution
**File**: `/Users/sac/cre/test/wf_mi_exec_test.erl` (NEW)
**Changes**: Create tests for MI opcode execution

```erlang
-module(wf_mi_exec_test).
-include_lib("eunit/include/eunit.hrl").

mi_fixed_all_test() ->
    Pattern = wf_term:mi({fixed, 3}, wf_term:task(double, fun(X) -> X * 2 end)),
    {ok, Compiled} = wf_compile:compile(Pattern),
    {ok, Results} = wf_exec:run(Compiled, #{data => [1,2,3]}),
    ?assertEqual([2,4,6], Results).

mi_first_n_test() ->
    Pattern = wf_term:mi({fixed, 5}, wf_term:task(id, fun(X) -> X end)),
    {ok, Compiled} = wf_compile:compile(Pattern, #{}),
    %% Execute with first_n=2 policy
    ?assertMatch({ok, _}, wf_exec:run(Compiled, #{data => [1,2,3,4,5]})).
```

#### Success Criteria:

##### Automated Verification:
- [ ] `rebar3 eunit` passes (including new MI exec tests)
- [ ] Code coverage shows MI opcode handlers are exercised
- [ ] No VM crashes or unexpected halts during MI execution

##### Manual Verification:
- [ ] Bytecode execution produces same results as gen_yawl patterns
- [ ] Performance is acceptable (benchmark against gen_yawl)
- [ ] XES trace logs show correct MI event sequence

**Note**: Complete automated verification, then manually benchmark gen_yawl vs bytecode execution before Phase 4.

---

### Phase 4: Integrate Cancellation Support

#### Overview
Add per-instance cancellation scope support, integrating with Item 014 (structured cancellation). Use gen_yawl's trigger/3 callback to filter cancellation tokens atomically.

#### Changes Required:

##### 1. Extend MI Patterns with Cancellation
**File**: All `mi_*_pattern.erl` modules (extend)
**Changes**: Add cancellation token handling in trigger/3

```erlang
%% In mi_all_pattern, mi_first_n_pattern, mi_discriminator_pattern

-record(state, {
    %% ... existing fields ...
    cancel_requested = false :: boolean(),
    cancelled_instances = [] :: [pos_integer()]
}).

%% trigger/3 callback for cancellation token filtering
trigger(p_active, {instance, Id}, #state{cancel_requested = true}) ->
    %% Consume (cancel) instance token
    {consume, [{instance, Id}]};

trigger(p_active, {instance, Id}, #state{cancel_requested = false}) ->
    %% Allow instance to proceed
    pass;

trigger(p_cancel, cancel, State) ->
    %% Cancel all active instances
    {consume, [cancel], State#state{cancel_requested = true}};

trigger(_Place, _Token, _State) ->
    pass.
```

##### 2. Add Cancellation API
**File**: `/Users/sac/cre/src/patterns/mi_pattern.erl` (extend)
**Changes**: Add cancel/1 function

```erlang
-export([cancel/1]).

-spec cancel(Pid :: pid()) -> ok.
cancel(Pid) ->
    gen_yawl:cast(Pid, {cancel, all}).
```

##### 3. Update gen_yawl Callbacks to Handle Cancel Cast
**File**: All `mi_*_pattern.erl` modules (extend)
**Changes**: Add handle_cast for cancel messages

```erlang
handle_cast({cancel, all}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    NewUsrInfo = UsrInfo#state{cancel_requested = true},
    NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
    %% Trigger immediate evaluation to process cancellation
    gen_yawl:sync(NewNetState, 0),
    {noreply, NewNetState};
```

#### Success Criteria:

##### Automated Verification:
- [ ] Cancellation tests pass: instances halt when cancelled
- [ ] No race conditions: instance completing as cancellation arrives
- [ ] Dialyzer confirms trigger/3 return types are correct

##### Manual Verification:
- [ ] Manual cancellation test: start pattern, send cancel, verify instances stop
- [ ] Check XES logs for cancellation events
- [ ] Verify no orphaned processes after cancellation

**Note**: This phase integrates with Item 014. If Item 014 is not complete, implement minimal cancellation support and document integration points.

---

### Phase 5: Documentation and Examples

#### Overview
Create comprehensive documentation and usage examples for the unified MI framework.

#### Changes Required:

##### 1. Create MI Pattern Guide
**File**: `/Users/sac/cre/docs/mi_patterns.md` (NEW)
**Changes**: Write user-facing documentation

```markdown
# Multiple Instance Patterns Guide

## Overview
The CRE workflow substrate supports multiple instance (MI) patterns for parallel execution with flexible synchronization.

## Pattern Types

### 1. Wait All (WCP-12)
Execute M instances, proceed when all complete.

```erlang
Fun = fun(X) -> process(X) end,
Data = [1, 2, 3, 4, 5],
{ok, Results} = mi_pattern:execute({fixed, 5}, all, [fun() -> Fun(D) end || D <- Data]).
```

### 2. First N
Proceed after first N instances complete (non-blocking).

```erlang
%% Start processing after 3 complete, continue with remaining 7
{ok, FirstResults} = mi_pattern:execute({fixed, 10}, {first_n, 3}, Funs).
```

### 3. N of M Quorum
Wait for N out of M instances (N ≤ M).

```erlang
%% 3 of 5 quorum
{ok, QuorumResults} = mi_pattern:execute({fixed, 5}, {n_of_m, 3, 5}, Funs).
```

### 4. Discriminator
First completion wins, cancel rest.

```erlang
%% Fastest response wins
{ok, Winner} = mi_pattern:execute({fixed, 3}, discriminator, [fast(), medium(), slow()]).
```

### 5. Dynamic Spawning (WCP-15)
Spawn instances based on runtime data.

```erlang
%% Read from stream until exhausted
DataFun = fun() ->
    case file:read_line(Stream) of
        {ok, Line} -> {more, Line};
        eof -> done
    end
end,
{ok, Results} = mi_pattern:execute({dynamic, DataFun, 1000}, all, []).
```

## Cancellation
```erlang
{ok, Pid} = mi_pattern:start({fixed, 100}, all, Funs),
%% Cancel after 5 seconds
timer:sleep(5000),
mi_pattern:cancel(Pid).
```
```

##### 2. Add Examples to Repository
**File**: `/Users/sac/cre/examples/mi_patterns_examples.erl` (NEW)
**Changes**: Create runnable examples

```erlang
-module(mi_patterns_examples).
-export([parallel_map/2, quorum_decision/1, race/1, stream_processor/2]).

%% Parallel map using MI all
parallel_map(Fun, List) ->
    Funs = [fun() -> Fun(X) end || X <- List],
    {ok, Results} = mi_pattern:execute({fixed, length(List)}, all, Funs),
    Results.

%% Quorum-based decision (3 of 5 agree)
quorum_decision(Options) ->
    Funs = [fun() -> evaluate_option(Opt) end || Opt <- Options],
    {ok, Results} = mi_pattern:execute({fixed, 5}, {n_of_m, 3, 5}, Funs),
    majority_vote(Results).

%% Race - fastest service wins
race(Services) ->
    Funs = [fun() -> call_service(Svc) end || Svc <- Services],
    {ok, Winner} = mi_pattern:execute({fixed, length(Services)}, discriminator, Funs),
    Winner.

%% Process stream with backpressure
stream_processor(StreamFun, MaxConcurrent) ->
    DataFun = fun() -> case StreamFun() of {ok, Data} -> {more, Data}; eof -> done end end,
    {ok, Results} = mi_pattern:execute({dynamic, DataFun, MaxConcurrent}, all, []),
    Results.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All examples compile without errors
- [ ] Examples can be executed: `erl -noshell -s mi_patterns_examples parallel_map fun(X) -> X * 2 end, [1,2,3] -s init stop`
- [ ] Documentation renders correctly (if using ExDoc or similar)

##### Manual Verification:
- [ ] Examples are clear and well-commented
- [ ] Use cases are realistic and helpful
- [ ] Performance characteristics are documented

---

## Testing Strategy

### Unit Tests

Each MI pattern module (`mi_all_pattern`, `mi_first_n_pattern`, `mi_discriminator_pattern`) will have:

1. **Normal execution tests:**
   - Execute with M instances, verify M results
   - Test with various M values (1, 5, 10, 100)

2. **Join policy tests:**
   - "all": verify all M complete before output
   - "first_n": verify output after N complete
   - "n_of_m": verify quorum behavior
   - "discriminator": verify first wins, rest cancelled

3. **State validation tests:**
   - Check Petri net markings at each step
   - Verify completion counts
   - Check result ordering

4. **Error handling tests:**
   - Instance failure (exception)
   - Timeout scenarios
   - Invalid specs

5. **Property-based tests (optional):**
   - Use PropEr or PropCheck for invariants
   - Property: output count matches spec
   - Property: all instance IDs are unique

### Integration Tests

1. **End-to-end workflow tests:**
   - MI pattern in sequence with other tasks
   - MI pattern inside cancellation scope
   - MI pattern with deferred choice

2. **Interoperability tests:**
   - gen_yawl patterns vs. bytecode execution
   - Same inputs produce same outputs
   - Performance comparison

3. **Cancellation tests:**
   - Cancel before instances start
   - Cancel during execution
   - Cancel after quorum met

### Manual Testing Steps

1. **Smoke test - basic MI execution:**
   ```erlang
   $ erl -pa ebin
   1> {ok, Results} = mi_pattern:execute({fixed, 3}, all, [fun() -> 1 end, fun() -> 2 end, fun() -> 3 end]).
   {ok, [1,2,3]}
   ```

2. **Stress test - large instance counts:**
   ```erlang
   N = 1000,
   Funs = [fun(X) -> X end || X <- lists:seq(1, N)],
   {ok, Results} = mi_pattern:execute({fixed, N}, all, Funs),
   length(Results) =:= N.
   ```

3. **Concurrency test - parallel execution:**
   ```erlang
   %% Verify instances run in parallel (not sequential)
   %% Use timer to measure completion time
   Start = erlang:monotonic_time(millisecond),
   Funs = [fun() -> timer:sleep(100), 1 end || _ <- lists:seq(1, 10)],
   {ok, _Results} = mi_pattern:execute({fixed, 10}, all, Funs),
   End = erlang:monotonic_time(millisecond),
   %% Should complete in ~100ms (parallel), not 1000ms (sequential)
   End - Start < 200.
   ```

4. **Cancellation test - verify cleanup:**
   ```erlang
   {ok, Pid} = mi_pattern:execute({fixed, 100}, all, SlowFuns),
   timer:sleep(50),
   mi_pattern:cancel(Pid),
   %% Verify no orphaned processes
   erlang:processes() - Before < 10.
   ```

5. **Dynamic spawning test - data-driven:**
   ```erlang
   Counter = spawn_link(fun() -> counter_loop(0) end),
   DataFun = fun() ->
       case get_next_data() of
           {ok, Data} -> {more, Data};
           eof -> done
       end
   end,
   {ok, Results} = mi_pattern:execute({dynamic, DataFun, 100}, all, []),
   %% Verify correct number of instances spawned
   ```

## Migration Notes

### For Existing Users

1. **Gen_Yawl patterns remain:**
   - All existing gen_yawl MI patterns continue to work
   - No breaking changes to existing code

2. **New facade API:**
   - Use `mi_pattern:execute/3` for new code
   - Or call individual pattern modules directly

3. **Bytecode execution (future):**
   - When wf_exec is production-ready, migrate: `wf_exec:run(wf_compile:compile(wf_term:mi(...)))`
   - Performance improvements expected
   - Same semantics, different runtime

### For Test Suite

1. **Fix cre_yawl_patterns references:**
   - Replace with `mi_pattern` facade calls
   - Update assertions to match new API

2. **Add new tests:**
   - Test first_n pattern
   - Test discriminator pattern
   - Test cancellation

## References

- Research: `/Users/sac/cre/.wreckit/items/023-multiple-instance-pattern-implementations/research.md`
- Model implementation: `/Users/sac/cre/src/patterns/n_out_of_m.erl:1-657`
- Pattern algebra: `/Users/sac/cre/src/wf/wf_term.erl:218-229`
- Compiler: `/Users/sac/cre/src/wf/wf_compile.erl:182-187`
- VM opcodes: `/Users/sac/cre/src/wf/wf_vm.erl:28-29, 102-103`
- Tests: `/Users/sac/cre/test/yawl_multiple_instances_test.erl:194+`
- IDEAS.md: `/Users/sac/cre/IDEAS.md:107-136` (pattern algebra)
- IDEAS.md: `/Users/sac/cre/IDEAS.md:163-167` (bytecode strategy)
