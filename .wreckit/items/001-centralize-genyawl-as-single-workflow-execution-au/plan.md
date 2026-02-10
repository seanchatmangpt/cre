# Centralize gen_yawl as single workflow execution authority Implementation Plan

## Implementation Plan Title
Centralize all workflow execution through gen_yawl to eliminate split-brain execution model

## Overview
This refactor centralizes all workflow execution through `gen_yawl`, making `gen_pnet` an internal dependency. The current codebase has a **split-brain execution model** where both `gen_pnet` and `gen_yawl` are used as entry points, creating hidden state mutation paths, inconsistent usr_info handling, and non-replayable state.

The solution ensures:
- All state mutations flow through the 3-tuple fire/3 return pattern `{produce, Map, NewUsrInfo}`
- Checkpoints capture both marking and usr_info consistently
- Every transition is observable and traceable through gen_yawl
- Recovery and replay work correctly

## Current State

### Existing Architecture Issues

**1. Dual Execution Loops**
Both `gen_pnet` and `gen_yawl` have independent `continue(self())` loops:
- `/Users/sac/cre/src/core/gen_pnet.erl:706-721` - gen_pnet progress loop
- `/Users/sac/cre/src/core/gen_yawl.erl:928-1048` - gen_yawl progress loop

Only gen_yawl handles the 3-tuple return with `NewUsrInfo` update.

**2. API Surface Confusion**
- `yawl_execution` module calls `gen_pnet:start_link` directly (lines 148, 175) instead of `gen_yawl`
- 7 workflow modules use `-behaviour(gen_pnet)` instead of `-behaviour(gen_yawl)`
- 51 pattern modules correctly use `-behaviour(gen_yawl)` and return 3-tuples

**3. Modules Bypassing gen_yawl**
Direct `gen_pnet` usage in production workflows:
- `/Users/sac/cre/src/order_fulfillment.erl:40,153` - Uses `gen_pnet:start_link`
- `/Users/sac/cre/src/freight_in_transit.erl:34,142` - Uses `gen_pnet:start_link`
- `/Users/sac/cre/src/cre_worker.erl:229,249` - Worker processes using gen_pnet
- `/Users/sac/cre/src/cre_yawl_exception.erl:41` - Exception handling

**4. usr_info Mutation Inconsistency**
- Modules using `gen_pnet` cannot update usr_info in fire/3 (no 3-tuple support)
- Must use `handle_call/3` or `handle_cast/2` to mutate state, creating hidden state mutations
- These mutations are not observable as transitions

### Key Discoveries

**Pattern to Follow (from `/Users/sac/cre/src/patterns/sequence.erl:45-54`):**
```erlang
fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_task1 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_task2 => [token]}, UsrInfo};
```

**State-Mutating Pattern (from `/Users/sac/cre/src/patterns/structured_loop.erl:434-444`):**
```erlang
fire('t_execute_body', #{'p_body_active' := [{state, CurrentState}]},
     #loop_state{body_fun = BodyFun} = State) ->
    NewState = BodyFun(CurrentState),
    {produce, #{
        'p_body_active' => [],
        'p_body_done' => [{state, NewState}]
    }, State#loop_state{current_state = NewState}}.
```

**Critical Discovery:** The `yawl_execution` module is meant to be the **high-level API**, but calls `gen_pnet` instead of `gen_yawl`:
- Line 147-148: `start_link/2` calls `gen_pnet:start_link/3`
- Line 174-175: `start_link/4` calls `gen_pnet:start_link/4`

**Checkpoint Integration (from `/Users/sac/cre/src/core/gen_yawl.erl:894-900`):**
```erlang
case yawl_recovery:maybe_checkpoint(StepCount, CheckpointInterval,
        NetArg, NetState3#net_state.marking, NetState3#net_state.usr_info) of
    {do_checkpoint, SpecId, CaseId, Marking, Data} ->
        _ = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),
        ok;
    ok -> ok
end
```

gen_yawl captures **both marking and usr_info**, gen_pnet does not.

## Desired End State

**Success Criteria:**
1. All fire/3 implementations return `{produce, Map, NewUsrInfo}` 3-tuple explicitly
2. No direct gen_pnet calls outside gen_yawl (only internal dependency)
3. All external usr_info mutation moved into fire/3 or trigger/3 callbacks
4. Every state mutation representable as `(Marking_before, Fire, Marking_after, UsrInfo_after)`
5. No module spins its own continue loop or calls fire/3 directly
6. All execution passes through `gen_yawl:step/1`, `gen_yawl:drain/2`, or `continue(self())`

**Verification:**
- All workflow starts use `gen_yawl:start_link` or `yawl_execution` (which calls gen_yawl)
- All 3-tuple returns flow through gen_yawl's progress loop
- Checkpoints include marking + usr_info
- Recovery/replay produces deterministic state

## What We're NOT Doing

**Out of Scope (explicitly):**
- Changing the fundamental gen_pnet implementation (it becomes internal)
- Removing gen_pnet (mark as internal dependency instead)
- Modifying core Petri net semantics
- Changing the checkpoint data format
- Modifying the recovery infrastructure (yawl_recovery)

**What We're NOT Touching:**
- Test infrastructure using gen_pnet directly (acceptable for unit testing)
- gen_pnet's core Petri net execution logic
- The structure of #net_state{} record
- trigger/3 callback behavior

## Implementation Approach

**High-Level Strategy:**
1. **Fix the public API first** - Update `yawl_execution` to call `gen_yawl` instead of `gen_pnet`
2. **Migrate workflow modules** - Convert 7 workflow modules from `gen_pnet` to `gen_yawl` behavior
3. **Eliminate hidden state mutations** - Move all usr_info mutations into fire/3
4. **Add guard rails** - Mark gen_pnet as internal with deprecation warnings
5. **Verify** - Add checkpoint/replay tests

**Key Principle:**
- Each phase is independently testable
- Maintain backwards compatibility where possible
- Incremental migration to minimize risk
- Clear rollback strategy for each phase

---

## Phases

### Phase 1: Fix Public API (yawl_execution)

#### Overview
Update `yawl_execution` module to call `gen_yawl` instead of `gen_pnet` for workflow starts. This is the **highest impact, lowest risk** change - fixes the high-level API immediately for all users.

#### Changes Required:

##### 1. Fix start_link/2 in yawl_execution
**File**: `/Users/sac/cre/src/wf/yawl_execution.erl`
**Changes**: Update line 148 to call `gen_yawl:start_link`

```erlang
%% BEFORE (line 147-148):
start_link(NetMod, NetArg) when is_atom(NetMod) ->
    gen_pnet:start_link(NetMod, NetArg, []).

%% AFTER:
start_link(NetMod, NetArg) when is_atom(NetMod) ->
    gen_yawl:start_link(NetMod, NetArg, []).
```

##### 2. Fix start_link/4 in yawl_execution
**File**: `/Users/sac/cre/src/wf/yawl_execution.erl`
**Changes**: Update line 174-175 to call `gen_yawl:start_link`

```erlang
%% BEFORE (line 174-175):
start_link(ServerName, NetMod, NetArg) when is_atom(NetMod) ->
    gen_pnet:start_link(ServerName, NetMod, NetArg, []).

%% AFTER:
start_link(ServerName, NetMod, NetArg) when is_atom(NetMod) ->
    gen_yawl:start_link(ServerName, NetMod, NetArg, []).
```

##### 3. Update module documentation
**File**: `/Users/sac/cre/src/wf/yawl_execution.erl`
**Changes**: Update moduledoc to reflect gen_yawl usage

```erlang
%% BEFORE (line 4-7):
%% This module provides utilities for executing compiled YAWL workflows
%% with gen_pnet. It offers a high-level API for starting workflows,

%% AFTER:
%% This module provides utilities for executing compiled YAWL workflows
%% with gen_yawl. It offers a high-level API for starting workflows,
```

##### 4. Update type references
**File**: `/Users/sac/cre/src/wf/yawl_execution.erl`
**Changes**: Update type specs from `gen_pnet:server_name()` to `gen_yawl:server_name()`

```erlang
%% BEFORE (line 169):
-spec start_link(ServerName :: gen_pnet:server_name(),

%% AFTER:
-spec start_link(ServerName :: gen_yawl:server_name(),
```

Also update line 218.

##### 5. Fix stop/1 to call gen_yawl
**File**: `/Users/sac/cre/src/wf/yawl_execution.erl`
**Changes**: Update line 244 to call `gen_yawl:stop`

```erlang
%% BEFORE (line 243-244):
stop(Name) ->
    gen_pnet:stop(Name).

%% AFTER:
stop(Name) ->
    gen_yawl:stop(Name).
```

#### Success Criteria:

##### Automated Verification:
- [ ] All existing tests pass: `rebar3 eunit`
- [ ] Type checking passes: `rebar3 dialyzer`
- [ ] Linting passes: `rebar3 lint`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Verify `yawl_execution` test suite passes (lines 479-565)
- [ ] Start a workflow using `yawl_execution:start_workflow/2` and verify it uses gen_yawl
- [ ] Inject input using `yawl_execution:inject_input/2` and verify it works
- [ ] Execute step using `yawl_execution:execute_step/1` and verify 3-tuple support
- [ ] Check that workflow can be checkpointed and recovered
- [ ] No regressions in existing workflow execution

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Migrate Workflow Modules to gen_yawl

#### Overview
Migrate 7 workflow modules from `-behaviour(gen_pnet)` to `-behaviour(gen_yawl)`. This enables 3-tuple fire/3 returns and automatic usr_info updates.

#### Changes Required:

##### 1. Migrate order_fulfillment.erl
**File**: `/Users/sac/cre/src/order_fulfillment.erl`

**Change 1.1: Update behavior declaration (line 40)**
```erlang
%% BEFORE:
-behaviour(gen_pnet).

%% AFTER:
-behaviour(gen_yawl).
```

**Change 1.2: Update start/1 to call gen_yawl (line 153)**
```erlang
%% BEFORE:
start(OrderInput) ->
    FulfillmentState = new(OrderInput),
    gen_pnet:start_link(?MODULE, FulfillmentState, []).

%% AFTER:
start(OrderInput) ->
    FulfillmentState = new(OrderInput),
    gen_yawl:start_link(?MODULE, FulfillmentState, []).
```

**Change 1.3: Update run/1 to call gen_yawl (lines 166, 169)**
```erlang
%% BEFORE:
wait_for_completion(Pid, 300000) of
    {ok, State} ->
        gen_pnet:stop(Pid),
        {ok, State};
    {error, Reason} ->
        gen_pnet:stop(Pid),

%% AFTER:
wait_for_completion(Pid, 300000) of
    {ok, State} ->
        gen_yawl:stop(Pid),
        {ok, State};
    {error, Reason} ->
        gen_yawl:stop(Pid),
```

**Change 1.4: Update get_status/1 to call gen_yawl (line 182)**
```erlang
%% BEFORE:
get_status(Pid) ->
    case gen_pnet:call(Pid, get_status) of

%% AFTER:
get_status(Pid) ->
    case gen_yawl:call(Pid, get_status) of
```

**Change 1.5: Update get_state/1 to call gen_yawl (line 193)**
```erlang
%% BEFORE:
get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%% AFTER:
get_state(Pid) ->
    gen_yawl:call(Pid, get_state).
```

**Change 1.6: Update fire/3 to return 3-tuples (need to read full implementation)**
```erlang
%% Pattern to follow for all fire/3 clauses:
fire(Transition, Mode, UsrInfo) ->
    %% ... existing logic ...
    {produce, ProduceMap, UsrInfo}.  %% Add UsrInfo as 3rd element
```

##### 2. Migrate freight_in_transit.erl
**File**: `/Users/sac/cre/src/freight_in_transit.erl`

**Change 2.1: Update behavior declaration (line 34)**
```erlang
%% BEFORE:
-behaviour(gen_pnet).

%% AFTER:
-behaviour(gen_yawl).
```

**Change 2.2: Update start/1 to call gen_yawl (line 142)**

**Change 2.3: Update fire/3 to return 3-tuples**

**Change 2.4: Update all gen_pnet API calls to gen_yawl**

##### 3. Migrate cre_worker.erl
**File**: `/Users/sac/cre/src/cre_worker.erl`

**Change 3.1: Update behavior declaration**

**Change 3.2: Update gen_pnet:start_link calls (lines 229, 249)**

**Change 3.3: Update fire/3 to return 3-tuples**

##### 4. Migrate cre_yawl_exception.erl
**File**: `/Users/sac/cre/src/cre_yawl_exception.erl`

**Change 4.1: Update behavior declaration (line 41)**

**Change 4.2: Update all gen_pnet API calls**

**Change 4.3: Update fire/3 to return 3-tuples**

##### 5. Update test modules (optional but recommended)
**Files**: `wf_test_net_resume.erl`, `wf_test_net_receipt.erl`, `wf_test_stub_net.erl`, `wf_test_net_trigger_drop.erl`

**Note**: Test modules can stay on gen_pnet with explicit comments:
```erlang
%% Note: Test module uses gen_pnet directly for unit testing purposes.
%% Production workflows MUST use gen_yawl.
-behaviour(gen_pnet).
```

#### Success Criteria:

##### Automated Verification:
- [ ] All workflow module tests pass: `rebar3 eunit`
- [ ] Type checking passes: `rebar3 dialyzer`
- [ ] No dialyzer warnings about 3-tuple returns
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Start order_fulfillment workflow and verify it uses gen_yawl
- [ ] Start freight_in_transit workflow and verify it uses gen_yawl
- [ ] Start cre_worker and verify it uses gen_yawl
- [ ] Verify all fire/3 callbacks return 3-tuples
- [ ] Verify usr_info is updated in fire/3, not handle_call/handle_cast
- [ ] Check that workflows can be checkpointed and recovered
- [ ] No regressions in workflow execution

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Eliminate Hidden State Mutations

#### Overview
Move all usr_info mutations from `handle_call/3` and `handle_cast/2` into `fire/3`. This ensures all state mutations are observable as transitions.

#### Changes Required:

##### 1. Audit all modules for hidden usr_info mutations
**Action**: Search for patterns like:
```erlang
NetState#net_state{usr_info = NewValue}
```

**Files to audit**:
- `/Users/sac/cre/src/order_fulfillment.erl`
- `/Users/sac/cre/src/freight_in_transit.erl`
- `/Users/sac/cre/src/cre_worker.erl`
- `/Users/sac/cre/src/cre_yawl_exception.erl`
- All 51 pattern modules

##### 2. Migrate handle_call state mutations to fire/3
**Pattern to follow**:

```erlang
%% WRONG: State mutation in handle_call
handle_call({update_state, NewState}, _From, NetState) ->
    {noreply, NetState#net_state{usr_info = NewState}}.

%% CORRECT: State mutation in fire/3
fire(t_update, _Mode, #state{count = C} = State) ->
    {produce, #{p_next => [token]}, State#state{count = C + 1}}.
```

**Example Migration**:
If a module uses `handle_call` to update state:
```erlang
%% BEFORE:
handle_call({set_result, Result}, _From, NetState = #net_state{usr_info = State}) ->
    {reply, ok, NetState#net_state{usr_info = State#fulfillment_state{result = Result}}}.

%% AFTER:
%% 1. Add a token to trigger the update
%% 2. Handle the update in fire/3
fire(t_set_result, _Mode, #fulfillment_state{} = State) ->
    {produce, #{p_next => [token]}, State#fulfillment_state{result = Result}}.
```

##### 3. Add runtime assertion to detect state mutations
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`

**Add assertion in handle_call/3 (around line 764-784)**:
```erlang
handle_call({call, Request}, From,
            WrapperState = #wrapper_state{net_mod = NetMod, net_state = NetState}) ->
    OldUsrInfo = NetState#net_state.usr_info,
    case NetMod:handle_call(Request, From, NetState) of
        {reply, Reply} ->
            %% ASSERTION: usr_info should not change in handle_call
            NewUsrInfo = WrapperState#wrapper_state.net_state#net_state.usr_info,
            case OldUsrInfo =:= NewUsrInfo of
                true -> ok;
                false ->
                    logger:warning("gen_yawl detected usr_info mutation in handle_call. "
                                   "Use fire/3 3-tuple return instead: ~p", [Request])
            end,
            {reply, Reply, WrapperState};

        %% ... other cases ...
    end.
```

##### 4. Add telemetry event for usr_info changes
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`

**Add telemetry when NewUsrInfo is updated (around line 982-986)**:
```erlang
%% Update user info if provided by fire/3 3-tuple return
NetState2 = case NewUsrInfo of
    undefined -> NetState1;
    _ ->
        %% Emit telemetry for usr_info change
        _ = logger:debug("gen_yawl usr_info updated via fire/3 3-tuple"),
        NetState1#net_state{usr_info = NewUsrInfo}
end,
```

#### Success Criteria:

##### Automated Verification:
- [ ] All tests pass: `rebar3 eunit`
- [ ] No warnings about usr_info mutations in handle_call/handle_cast
- [ ] Telemetry logs show usr_info updates happening in fire/3

##### Manual Verification:
- [ ] Review all modules and confirm no usr_info mutations in handle_call/handle_cast
- [ ] Add test case that attempts to mutate usr_info in handle_call - should log warning
- [ ] Verify all state updates are observable in telemetry
- [ ] Check that checkpoint/replay produces identical state

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 4.

---

### Phase 4: Mark gen_pnet as Internal

#### Overview
Add deprecation warnings and documentation to signal that `gen_pnet` is an internal module. External code should use `gen_yawl` instead.

#### Changes Required:

##### 1. Add deprecation notice to gen_pnet module documentation
**File**: `/Users/sac/cre/src/core/gen_pnet.erl`

**Update moduledoc (lines 2-36)**:
```erlang
%% BEFORE:
%%%% @doc gen_pnet - A generic Petri net OTP behavior.
%%
%% `gen_pnet' is a behavior module for implementing Petri net workflows
%% as Erlang/OTP gen_server processes.

%% AFTER:
%%%% @doc gen_pnet - A generic Petri net OTP behavior (INTERNAL).
%%
%% @warning **DEPRECATED**: Use `gen_yawl' instead. This module is now
%% an internal implementation detail of gen_yawl and should not be used
%% directly in production code.
%%
%% `gen_pnet' is a behavior module for implementing Petri net workflows
%% as Erlang/OTP gen_server processes. It is wrapped by `gen_yawl' which
%% provides enhanced fire/3 support with automatic usr_info updates.
%%
%% <h3>Migration Guide</h3>
%%
%% To migrate from gen_pnet to gen_yawl:
%%
%% <ul>
%%   <li>Replace `-behaviour(gen_pnet).' with `-behaviour(gen_yawl).'</li>
%%   <li>Update `gen_pnet:start_link' calls to `gen_yawl:start_link'</li>
%%   <li>Update fire/3 to return 3-tuple: `{produce, Map, NewUsrInfo}'</li>
%%   <li>Move usr_info mutations from handle_call/handle_cast into fire/3</li>
%% </ul>
%%
%% @end
```

##### 2. Add deprecation warnings to gen_pnet API functions
**File**: `/Users/sac/cre/src/core/gen_pnet.erl`

**Add warning to start_link/3 (around line 235)**:
```erlang
%% BEFORE:
-spec start_link(NetMod :: atom(), NetArg :: term(), Options :: [prop()]) ->
          start_link_result().

start_link(NetMod, NetArg, Options)
  when is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(?MODULE, {NetMod, NetArg}, Options).

%% AFTER:
-spec start_link(NetMod :: atom(), NetArg :: term(), Options :: [prop()]) ->
          start_link_result().

start_link(NetMod, NetArg, Options)
  when is_atom(NetMod), is_list(Options) ->
    logger:warning("gen_pnet:start_link is deprecated. Use gen_yawl:start_link instead."),
    gen_server:start_link(?MODULE, {NetMod, NetArg}, Options).
```

**Add similar warnings to**:
- `start_link/4` (line 262)
- `step/1` (line 514)
- `drain/2` (line 537)

##### 3. Update gen_pnet callback documentation
**File**: `/Users/sac/cre/src/core/gen_pnet.erl`

**Update fire/3 callback documentation (lines 155-159)**:
```erlang
%% BEFORE:
%% Returns tokens produced when a transition fires in a given mode.
%% Only called for modes where is_enabled returns true.
%% Returns `{produce, ProduceMap}' to produce tokens or `abort' to cancel.
-callback fire(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              abort | {produce, #{atom() => [_]}}.

%% AFTER:
%% Returns tokens produced when a transition fires in a given mode.
%% Only called for modes where is_enabled returns true.
%% Returns `{produce, ProduceMap}' to produce tokens or `abort' to cancel.
%%
%% @note For automatic usr_info updates, use `gen_yawl' behavior instead,
%%       which supports 3-tuple returns: `{produce, ProduceMap, NewUsrInfo}'.
-callback fire(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              abort | {produce, #{atom() => [_]}}.
```

##### 4. Add @internal tags to gen_pnet exported functions
**File**: `/Users/sac/cre/src/core/gen_pnet.erl`

**Add @internal to API functions**:
```erlang
%% @private
%% @internal For use by gen_yawl only. Use gen_yawl:step/1 instead.
%% @end
%%--------------------------------------------------------------------
-spec step(Name :: name()) -> abort | {ok, Receipt :: #{atom() => [_]}}.

step(Name) ->
    gen_server:call(Name, step).
```

##### 5. Create migration guide documentation
**File**: `/Users/sac/cre/docs/migration_gen_pnet_to_gen_yawl.md`

```markdown
# Migrating from gen_pnet to gen_yawl

## Overview
`gen_pnet` is now an internal implementation detail. All production workflows
should use `gen_yawl` for enhanced features including 3-tuple fire/3 returns,
automatic usr_info updates, checkpoint integration, and telemetry.

## Step-by-Step Migration

### 1. Update Behavior Declaration
```erlang
%% BEFORE:
-behaviour(gen_pnet).

%% AFTER:
-behaviour(gen_yawl).
```

### 2. Update start_link Calls
```erlang
%% BEFORE:
gen_pnet:start_link(my_workflow, InitArg, []).

%% AFTER:
gen_yawl:start_link(my_workflow, InitArg, []).
```

### 3. Update fire/3 to Return 3-Tuples
```erlang
%% BEFORE:
fire(t1, _Mode, _UsrInfo) ->
    {produce, #{p_out => [token]}}.

%% AFTER:
fire(t1, _Mode, UsrInfo) ->
    {produce, #{p_out => [token]}, UsrInfo}.
```

### 4. Move State Mutations to fire/3
```erlang
%% BEFORE:
handle_call({update_state, NewState}, _From, NetState) ->
    {reply, ok, NetState#net_state{usr_info = NewState}}.

%% AFTER:
fire(t_update, _Mode, State) ->
    {produce, #{p_next => [token]}, State#state{updated = true}}.
```

## Benefits
- Automatic usr_info updates via 3-tuple returns
- Checkpoint integration with marking + usr_info
- Cycle detection and monitoring
- Telemetry integration
- Single execution authority
```

#### Success Criteria:

##### Automated Verification:
- [ ] Build succeeds: `rebar3 compile`
- [ ] Deprecation warnings appear in logs when gen_pnet is used
- [ ] All tests pass: `rebar3 eunit`

##### Manual Verification:
- [ ] Call a gen_pnet API function and verify deprecation warning is logged
- [ ] Read migration guide and verify it's clear and complete
- [ ] Check that gen_yawl documentation references the migration guide
- [ ] Verify no production code still uses gen_pnet directly

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 5.

---

### Phase 5: Verification and Testing

#### Overview
Add comprehensive tests to verify checkpoint/replay correctness and that all state mutations are observable.

#### Changes Required:

##### 1. Add checkpoint/replay integration test
**File**: `/Users/sac/cre/test/checkpoint_replay_test.erl` (create new file)

```erlang
-module(checkpoint_replay_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that checkpoint + replay produces identical state
checkpoint_replay_identity_test() ->
    %% Setup
    {ok, Pid} = gen_yawl:start_link(sequence, #{from => a, to => b}, []),
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start]}),
    {ok, _} = gen_yawl:step(Pid),

    %% Capture checkpoint
    Marking = gen_yawl:marking(Pid),
    UsrInfo = gen_yawl:usr_info(Pid),
    gen_yawl:stop(Pid),

    %% Replay from checkpoint
    {ok, Pid2} = gen_yawl:start_link(sequence, #{from => a, to => b},
                                      [{marking, Marking}, {usr_info, UsrInfo}]),

    %% Verify state is identical
    ?assertEqual(Marking, gen_yawl:marking(Pid2)),
    ?assertEqual(UsrInfo, gen_yawl:usr_info(Pid2)),
    gen_yawl:stop(Pid2).
```

##### 2. Add usr_info mutation detection test
**File**: `/Users/sac/cre/test/usr_info_mutation_test.erl` (create new file)

```erlang
-module(usr_info_mutation_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that usr_info mutations in handle_call are detected
usr_info_mutation_in_handle_call_test() ->
    %% Create a module that mutates usr_info in handle_call
    %% Verify warning is logged
    %% (Implementation depends on adding assertion in Phase 3)
    ok.

%% Test that usr_info updates in fire/3 work correctly
usr_info_update_in_fire_test() ->
    %% Use structured_loop pattern which updates state in fire/3
    {ok, Pid} = gen_yawl:start_link(structured_loop,
                                      #{
                                        body_fun => fun(X) -> X + 1 end,
                                        condition_fun => fun(X) -> X >= 5 end,
                                        loop_type => while
                                      }, []),
    {ok, _} = gen_yawl:inject(Pid, #{'p_start' => [start]}),

    %% Execute loop iterations
    {ok, Receipts} = gen_yawl:drain(Pid, 100),

    %% Verify usr_info was updated
    FinalUsrInfo = gen_yawl:usr_info(Pid),
    ?assertEqual(5, FinalUsrInfo#loop_state.current_state),
    gen_yawl:stop(Pid).
```

##### 3. Add state mutation observability test
**File**: `/Users/sac/cre/test/state_observability_test.erl` (create new file)

```erlang
-module(state_observability_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that every state mutation is representable as (Marking_before, Fire, Marking_after, UsrInfo_after)
state_mutation_observability_test() ->
    %% Execute a workflow and capture all transitions
    {ok, Pid} = gen_yawl:start_link(sequence, #{from => a, to => b},
                                      [{checkpoint_interval, 1}]),
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start]}),
    {ok, Receipts} = gen_yawl:drain(Pid, 10),

    %% Verify each transition is observable
    %% (This requires telemetry to be enabled)
    lists:foreach(fun(Receipt) ->
        ?assert(maps:is_key(trsn, Receipt)),
        ?assert(maps:is_key(produce, Receipt))
    end, Receipts),
    gen_yawl:stop(Pid).
```

##### 4. Add end-to-end workflow test with checkpointing
**File**: `/Users/sac/cre/test/e2e_workflow_checkpoint_test.erl` (create new file)

```erlang
-module(e2e_workflow_checkpoint_test).
-include_lib("eunit/include/eunit.hrl").

e2e_order_fulfillment_checkpoint_test() ->
    %% Start order fulfillment workflow
    Order = #order{
        order_id = <<"TEST-001">>,
        customer_id = <<"CUST-001">>,
        items = [#item{sku = <<"SKU-001">>, quantity => 1}],
        total = 100.0
    },

    {ok, Pid} = gen_yawl:start_link(order_fulfillment, Order,
                                     [{checkpoint_interval, 5}]),

    %% Execute until complete
    {ok, _Receipts} = gen_yawl:drain(Pid, 1000),

    %% Verify final state
    FinalState = gen_yawl:usr_info(Pid),
    ?assertMatch(#fulfillment_state{}, FinalState),
    gen_yawl:stop(Pid).
```

##### 5. Update existing test suites
**Files**:
- `/Users/sac/cre/src/wf/yawl_execution.erl` (test section lines 479-565)
- All pattern module test suites

**Action**: Ensure all tests use gen_yawl instead of gen_pnet

#### Success Criteria:

##### Automated Verification:
- [ ] All new tests pass: `rebar3 eunit`
- [ ] All existing tests still pass: `rebar3 eunit`
- [ ] Type checking passes: `rebar3 dialyzer`
- [ ] Code coverage shows good coverage of new tests (aim for >80%)

##### Manual Verification:
- [ ] Run checkpoint/replay test and verify it produces identical state
- [ ] Run usr_info mutation test and verify warnings are logged
- [ ] Run state observability test and verify all transitions are captured
- [ ] Run end-to-end workflow test and verify it completes successfully
- [ ] Check that telemetry shows all state mutations
- [ ] Verify that manual checkpoint and replay work correctly

**Note**: This is the final phase. Complete all verification and confirm all success criteria from the overview are met.

---

## Testing Strategy

### Unit Tests:
- **yawl_execution API tests** - Verify start_link, inject_input, execute_step, drain_workflow
- **Workflow module tests** - Verify order_fulfillment, freight_in_transit, cre_worker
- **Pattern module tests** - Verify all 51 patterns work with gen_yawl
- **usr_info mutation tests** - Verify mutations in handle_call are detected

### Integration Tests:
- **Checkpoint/replay tests** - Verify checkpoint → resume produces same final state
- **End-to-end workflow tests** - Verify complete workflows run correctly
- **State observability tests** - Verify every state mutation is captured
- **Replay determinism tests** - Verify replay from checkpoint is deterministic

### Manual Testing Steps:
1. Start a workflow using `gen_yawl:start_link` or `yawl_execution:start_workflow`
2. Inject input data using `gen_yawl:inject` or `yawl_execution:inject_input`
3. Execute steps using `gen_yawl:step` or `yawl_execution:execute_step`
4. Verify that usr_info is updated in fire/3 (check telemetry logs)
5. Verify that checkpoint captures marking + usr_info
6. Stop the workflow
7. Replay from checkpoint using `gen_yawl:start_link` with marking + usr_info
8. Verify that replay produces identical state
9. Verify that all transitions are observable in telemetry

## Migration Notes

### Backwards Compatibility Strategy:
- **Phase 1-2**: Add gen_yawl support while keeping gen_pnet working
- **Phase 3**: Add warnings for gen_pnet usage
- **Phase 4**: Mark gen_pnet as deprecated
- **Future**: Consider removing gen_pnet from public API entirely

### Rollback Plan:
Each phase can be independently rolled back:
- **Phase 1**: Revert yawl_execution.erl changes (4 specific edits)
- **Phase 2**: Revert workflow module behavior declarations (4 modules)
- **Phase 3**: Remove assertions and telemetry (1 file)
- **Phase 4**: Remove deprecation warnings (1 file)
- **Phase 5**: Remove new test files (4 files)

### Monitoring During Migration:
- Watch for deprecation warnings in logs
- Monitor usr_info mutation detections
- Check checkpoint/replay success rate
- Verify telemetry shows all state mutations

## References

### Research:
- `/Users/sac/cre/.wreckit/items/001-centralize-genyawl-as-single-workflow-execution-au/research.md`

### Core Modules:
- `/Users/sac/cre/src/core/gen_yawl.erl` - Wrapper with 3-tuple support (1556 lines)
- `/Users/sac/cre/src/core/gen_pnet.erl` - Base Petri net behavior (1556 lines)
- `/Users/sac/cre/src/wf/yawl_execution.erl` - High-level API (567 lines)

### Pattern Modules (already using gen_yawl correctly):
- `/Users/sac/cre/src/patterns/sequence.erl:45-54` - Example of correct 3-tuple fire/3
- `/Users/sac/cre/src/patterns/structured_loop.erl:434-444` - State mutation in fire/3
- All 51 modules in `/Users/sac/cre/src/patterns/` directory

### Workflow Modules (need migration):
- `/Users/sac/cre/src/order_fulfillment.erl:40,153` - Order fulfillment orchestrator
- `/Users/sac/cre/src/freight_in_transit.erl:34,142` - Freight tracking workflow
- `/Users/sac/cre/src/cre_worker.erl:229,249` - Generic worker process
- `/Users/sac/cre/src/cre_yawl_exception.erl:41` - Exception handling

### Checkpoint and Recovery:
- `/Users/sac/cre/src/wf/yawl_recovery.erl:223-260` - `maybe_checkpoint/5` integration
