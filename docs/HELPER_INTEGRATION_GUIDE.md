# Helper Integration Guide

This guide documents how the helper modules are used across the CRE (Common Runtime Environment) system. The helper modules provide pure-functional building blocks for Petri net execution, workflow patterns, and token management.

## Table of Contents

1. [Helper Modules Overview](#helper-modules-overview)
2. [Cross-Cutting Usage Matrix](#cross-cutting-usage-matrix)
3. [Integration Points](#integration-points)
4. [Data Flow Diagrams](#data-flow-diagrams)
5. [Usage Examples](#usage-examples)
6. [Migration Notes](#migration-notes)

---

## Helper Modules Overview

The helper modules are pure-functional utilities located in `/src/` that support workflow execution:

| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `pnet_marking` | Petri net token state management | `new/1`, `get/2`, `set/3`, `add/2`, `take/2`, `apply/3`, `hash/1`, `snapshot/1` |
| `pnet_choice` | Deterministic nondeterminism for reproducible execution | `seed/1`, `pick/2`, `pick_weighted/2` |
| `pnet_mode` | Mode enumeration for transition firing | `preset_counts/1`, `enum_modes/2`, `enum_cmodes/4` |
| `pnet_receipt` | Audit trail receipts for state transitions | `make/3`, `timestamp/0`, `effects/1` |
| `wf_task` | Task lifecycle token constructors | `enabled/3`, `running/3`, `done/3`, `failed/3`, `cancelled/3` |
| `wf_timerq` | Deadline-based timer queue for timeouts | `new/0`, `arm/4`, `disarm/2`, `poll/2`, `is_empty/1`, `size/1`, `peek/1` |
| `wf_scope` | Scope boundary mapping for nested workflows | `enter/3`, `leave/3`, `bindings/2` |
| `pnet_types` | Type definitions and validation helpers | `is_marking/1`, `is_mode/1`, `is_binding/1`, etc. |

---

## Cross-Cutting Usage Matrix

### Module Usage Overview

| Module | pnet_marking | pnet_choice | pnet_mode | pnet_receipt | wf_task | wf_timerq | wf_scope | pnet_types |
|--------|--------------|-------------|-----------|--------------|---------|-----------|----------|------------|
| **yawl_engine** | ✓ | - | - | - | - | - | - | - |
| **yawl_control** | ✓ | ✓ | - | - | ✓ | - | - | - |
| **yawl_patterns** | ✓ | ✓ | ✓ | - | - | ✓ | ✓ | - |
| **yawl_approval** | - | - | - | ✓ | ✓ | - | - | - |
| **cre_yawl_worker** | - | - | - | - | - | - | - | - |
| **pnet_receipt** | - | - | - | - | - | - | - | ✓ |

### Detailed Usage by Module

#### `yawl_engine`
- **pnet_marking**: Core marking operations for workflow state
  - `new/1` - Initialize workflow places
  - `add/2` - Add initial tokens
  - `get/2` - Query place tokens
  - `set/3` - Update place tokens
  - `take/2` - Consume tokens on transition
  - `apply/3` - Execute transitions atomically
  - `hash/1` - Compute state hashes for comparison
  - `snapshot/1` - Create state snapshots

#### `yawl_control`
- **pnet_marking**: Control state tracking for cases
  - `new/1` - Initialize control marking with status places
  - `add/2` - Add tokens when cases register
  - `take/2` - Remove tokens when cases unregister
  - `apply/3` - Atomic status transitions (running→suspended, etc.)
  - `snapshot/1` - Consistent state views for statistics
- **pnet_choice**: Deterministic control flow decisions
  - `seed/1` - Initialize RNG with timestamp
- **wf_task**: Control token lifecycle
  - `enabled/3` - New case registration
  - `running/3` - Resume from suspension
  - `done/3` - Case completion
  - `cancelled/3` - Case cancellation
  - `failed/3` - Case failure

#### `yawl_patterns`
- **pnet_marking**: Pattern validation and execution
  - `new/1` - Create initial marking for patterns
  - `snapshot/1` - Validate pattern state
  - `apply/3` - Execute pattern transitions
- **pnet_choice**: Pattern selection
  - `seed/1` - Initialize RNG
  - `pick/2` - Select pattern from list
  - `pick_weighted/2` - Select pattern with weights
- **pnet_mode**: Mode enumeration
  - `enum_modes/2` - Get valid firing modes
- **wf_timerq**: Timeout patterns
  - `new/0` - Create empty timer queue
  - `arm/4` - Set up timeout
  - `disarm/2` - Cancel timeout
- **wf_scope**: Nested pattern boundaries
  - `enter/3` - Translate tokens to child namespace
  - `leave/3` - Translate tokens back to parent
  - `bindings/2` - Get scope mappings

#### `yawl_approval`
- **pnet_receipt**: Audit trail for approvals
  - `make/3` - Create receipt on approve/deny
  - `timestamp/0` - Timestamp receipts
- **wf_task**: Approval task lifecycle
  - `enabled/3` - Request approval (pending state)
  - `done/3` - Approve checkpoint
  - `failed/3` - Deny checkpoint

#### `pnet_receipt`
- **pnet_types**: Type definitions
  - Uses `pnet_types:mode()`, `pnet_types:cmode()`, `pnet_types:produce_map()`

---

## Integration Points

### 1. Marking Lifecycle Integration

```
yawl_engine:init()
     |
     v
pnet_marking:new(Places)  -->  Initial empty marking
     |
     v
pnet_marking:add(Marking, ProduceMap)  -->  Add initial tokens
     |
     v
[Workflow Execution Loop]
     |
     +---> pnet_marking:get(Marking, Place)  -->  Check for tokens
     |
     +---> pnet_marking:take(Marking, ConsumeMap)  -->  Fire transition
     |
     +---> pnet_marking:set(Marking, Place, Tokens)  -->  Direct manipulation
     |
     +---> pnet_marking:apply(Marking, Consume, Produce)  -->  Atomic transition
     |
     v
pnet_marking:snapshot(Marking)  -->  State persistence
pnet_marking:hash(Marking)  -->  State comparison
```

### 2. Control Flow Integration

The `yawl_control` module uses helper modules for case lifecycle management:

```
[Register Case]
     |
     v
wf_task:enabled(CaseId, SpecId, Place)  -->  Create enabled token
     |
     v
pnet_marking:add(CtrlMarking, ProduceMap)  -->  Add to control marking
     |
     v
[Status Change: running -> suspended]
     |
     v
wf_task:enabled(CaseId, Reason, SuspendedPlace)  -->  Suspend token
     |
     v
pnet_marking:apply(CtrlMarking, ConsumeMap, ProduceMap)  -->  Atomic transition
```

### 3. Pattern Composition Integration

The `yawl_patterns` module composes helpers for complex patterns:

```
[Timeout Pattern]
     |
     v
wf_timerq:new()  -->  Create timer queue
     |
     v
wf_timerq:arm(TimerQ, Key, Deadline, Event)  -->  Schedule timeout
     |
     v
wf_timerq:poll(TimerQ, Now)  -->  Check for expired timers
     |
     v
wf_timerq:disarm(TimerQ, Key)  -->  Cancel timeout

[Nested Pattern]
     |
     v
wf_scope:enter(BindingTable, ScopeId, ParentMarking)  -->  Enter child scope
     |
     v
[Execute child pattern]
     |
     v
wf_scope:leave(BindingTable, ScopeId, ChildMarking)  -->  Return to parent
```

### 4. Approval Workflow Integration

The `yawl_approval` module integrates task lifecycle with receipts:

```
[Request Approval]
     |
     v
wf_task:enabled(CheckpointId, Context, 'p_approval_pending')  -->  Pending token
     |
     v
[Decision Made]
     |
     +---> [Approve]
     |       |
     |       v
     |   wf_task:done(CheckpointId, Decision, 'p_approval_complete')
     |       |
     |       v
     |   pnet_receipt:make(BeforeHash, AfterHash, Move)
     |
     +---> [Deny]
             |
             v
         wf_task:failed(CheckpointId, Reason, 'p_approval_failed')
             |
             v
         pnet_receipt:make(BeforeHash, AfterHash, Move)
```

---

## Data Flow Diagrams

### Token Flow Through Marking Operations

```
                  +-----------------+
                  | Initial Marking |
                  +-----------------+
                            |
                  +-----------------+
                  |  pnet_marking   |
                  |      :new/1     |
                  +-----------------+
                            |
                            v
                  +-----------------+
                  |  Empty Marking  |
                  |  #{p1 => [],    |
                  |    p2 => []}     |
                  +-----------------+
                            |
                  +-----------------+
                  |  pnet_marking   |
                  |      :add/2     |
                  +-----------------+
                            |
                            v
                  +-----------------+
                  |  Add Token      |
                  |  #{p1 => [a]}   |
                  +-----------------+
                            |
                  +-----------------+
                  |  pnet_marking   |
                  |      :get/2     |
                  +-----------------+
                            |
                            v
                  +-----------------+
                  |  [a]            |
                  +-----------------+
                            |
                  +-----------------+
                  |  pnet_marking   |
                  |      :take/2    |
                  +-----------------+
                            |
                            v
                  +-----------------+
                  |  Consume [a]    |
                  |  #{p1 => []}    |
                  +-----------------+
                            |
                  +-----------------+
                  |  pnet_marking   |
                  |      :apply/3   |
                  +-----------------+
                            |
                            v
                  +-----------------+
                  |  Atomic Update  |
                  |  #{p2 => [b]}   |
                  +-----------------+
```

### Receipt Generation Flow

```
+------------------+       +------------------+
| Before State     |       | After State      |
+------------------+       +------------------+
| Marking before   |       | Marking after    |
+------------------+       +------------------+
         |                         |
         | crypto:hash(sha256)     | crypto:hash(sha256)
         v                         v
+------------------+       +------------------+
| BeforeHash       |       | AfterHash        |
+------------------+       +------------------+
         |                         |
         +-----------+-------------+
                     |
                     v
           +-------------------+
           | pnet_receipt:make |
           +-------------------+
                     |
                     v
           +-------------------+
           | Receipt            |
           | #{before_hash =>  |
           |   BeforeHash,     |
           |  after_hash =>    |
           |   AfterHash,      |
           |  move => Move,    |
           |  ts => Timestamp} |
           +-------------------+
                     |
                     v
           +-------------------+
           | Audit Trail        |
           +-------------------+
```

---

## Usage Examples

### Example 1: Basic Marking Operations (from `yawl_engine`)

```erlang
%% Import types for marking operations
-type marking() :: pnet_marking:marking().
-type place() :: pnet_marking:place().
-type token() :: pnet_marking:token().

%% Initialize workflow places
Places = [input, output, task1, task2],
InitialMarking = pnet_marking:new(Places),
%% => #{input => [], output => [], task1 => [], task2 => []}

%% Add initial token to start workflow
MarkingWithToken = pnet_marking:add(InitialMarking, #{input => [start]}),
%% => #{input => [start], output => [], task1 => [], task2 => []}

%% Fire a transition: consume from input, produce to task1
case pnet_marking:apply(MarkingWithToken,
                         #{input => [start]},
                         #{task1 => [data]}) of
    {ok, NewMarking} ->
        %% Transition fired successfully
        %% => #{input => [], output => [], task1 => [data], task2 => []}
        {ok, NewMarking};
    {error, Reason} ->
        %% Transition failed (e.g., insufficient tokens)
        {error, Reason}
end.

%% Get tokens at a specific place
{ok, Tokens} = pnet_marking:get(NewMarking, task1),
%% => {ok, [data]}

%% Compute hash for state comparison
Hash = pnet_marking:hash(NewMarking),
%% => <<...binary hash...>>

%% Create snapshot for persistence
Snapshot = pnet_marking:snapshot(NewMarking).
```

### Example 2: Control State Transitions (from `yawl_control`)

```erlang
%% Define control places
-define(CTRL_PLACE_RUNNING, 'ctrl_running').
-define(CTRL_PLACE_SUSPENDED, 'ctrl_suspended').
-define(CTRL_PLACE_COMPLETED, 'ctrl_completed').

%% Initialize control marking
CtrlMarking = pnet_marking:new([?CTRL_PLACE_RUNNING,
                                ?CTRL_PLACE_SUSPENDED,
                                ?CTRL_PLACE_COMPLETED]),

%% Register a new case (running -> running)
{produce, ProduceMap} = wf_task:enabled(CaseId, SpecId, ?CTRL_PLACE_RUNNING),
%% => {produce, #{'ctrl_running' => [{task, CaseId, enabled, SpecId}]}}

CtrlMarking1 = pnet_marking:add(CtrlMarking, ProduceMap),

%% Suspend a case (running -> suspended)
ConsumeMap = #{?CTRL_PLACE_RUNNING => [{case_token, CaseId}]},
{produce, SuspendProduceMap} = wf_task:enabled(CaseId, Reason, ?CTRL_PLACE_SUSPENDED),

case pnet_marking:'apply'(CtrlMarking1, ConsumeMap, SuspendProduceMap) of
    {ok, SuspendedMarking} ->
        %% Case is now suspended
        ok;
    {error, Reason} ->
        {error, marking_failed}
end.

%% Resume a case (suspended -> running)
ConsumeMap2 = #{?CTRL_PLACE_SUSPENDED => [{case_token, CaseId}]},
{produce, ResumeProduceMap} = wf_task:running(CaseId, Reason, ?CTRL_PLACE_RUNNING),

case pnet_marking:'apply'(SuspendedMarking, ConsumeMap2, ResumeProduceMap) of
    {ok, RunningMarking} ->
        %% Case is now running
        ok
end.
```

### Example 3: Pattern Selection with Choice (from `yawl_patterns`)

```erlang
%% Select a pattern using deterministic choice
Patterns = [
    #pattern{pattern_type = option_a, ...},
    #pattern{pattern_type = option_b, ...},
    #pattern{pattern_type = option_c, ...}
],

%% Seed RNG for reproducible selection
Rng = pnet_choice:seed(12345),

%% Pick a pattern
case pnet_choice:pick(Patterns, Rng) of
    {SelectedPattern, NewRng} ->
        %% Use SelectedPattern for execution
        execute_pattern(SelectedPattern);
    {error, empty} ->
        {error, no_patterns_available}
end.

%% Weighted pattern selection
WeightedPatterns = [
    {PatternA, 1},  % Low weight
    {PatternB, 3},  % High weight (3x more likely)
    {PatternC, 1}   % Low weight
],

case pnet_choice:pick_weighted(WeightedPatterns, Rng) of
    {SelectedPattern, _NewRng} ->
        execute_pattern(SelectedPattern);
    {error, bad_weights} ->
        {error, invalid_weights}
end.
```

### Example 4: Timeout Pattern with Timer Queue (from `yawl_patterns`)

```erlang
%% Create a timeout pattern
Duration = 5000,  % 5 seconds
TimeoutFun = fun() -> {error, timeout} end,
NormalFun = fun(Data) -> {ok, process(Data)} end,

%% Initialize timer queue
TimerQ = wf_timerq:new(),
TimerKey = make_ref(),
Deadline = erlang:system_time(millisecond) + Duration,

%% Arm the timeout
Event = {produce, #{'p_timed_out' => [timed_out]}},
TimerQ1 = wf_timerq:arm(TimerQ, TimerKey, Deadline, Event),

%% In a polling loop:
poll_timeout(TimerQ) ->
    Now = erlang:system_time(millisecond),
    case wf_timerq:poll(TimerQ, Now) of
        {[], TimerQ2} ->
            %% No expired timers, continue waiting
            timer:sleep(100),
            poll_timeout(TimerQ2);
        {[ExpiredEvent | _], TimerQ2} ->
            %% Timeout fired, handle it
            handle_timeout(ExpiredEvent)
    end.

%% Cancel timeout if operation completes early
TimerQ2 = wf_timerq:disarm(TimerQ1, TimerKey).
```

### Example 5: Nested Pattern with Scope (from `yawl_patterns`)

```erlang
%% Define binding table for parent-child place mapping
BindingTable => #{
    my_subflow => #{
        parent_input => child_input,
        parent_output => child_output
    }
},

%% Enter scope: translate parent tokens to child places
ParentTokens = #{parent_input => [data1, data2]},
ChildProduceMap = wf_scope:enter(BindingTable, my_subflow, ParentTokens),
%% => #{child_input => [data1, data2]}

%% Execute child pattern with translated tokens
ChildResult = execute_child_pattern(ChildProduceMap),

%% Leave scope: translate child results back to parent places
ChildTokens = #{child_output => [result]},
ParentProduceMap = wf_scope:leave(BindingTable, my_subflow, ChildTokens),
%% => #{parent_output => [result]}

%% Get scope bindings if needed
case wf_scope:bindings(BindingTable, my_subflow) of
    #{parent_input := child_input, parent_output := child_output} = Bindings ->
        %% Use bindings for validation or debugging
        ok;
    {error, unknown_scope} ->
        {error, invalid_scope}
end.
```

### Example 6: Approval Receipt Generation (from `yawl_approval`)

```erlang
%% Create approval checkpoint
CheckpointId = <<"approval_12345">>,

%% When approved:
BeforeHash = crypto:hash(sha256, term_to_binary({checkpoint, CheckpointId})),
Decision = #approval_decision{
    checkpoint_id = CheckpointId,
    approved = true,
    decision_maker = human,
    reason = <<"Approved by manager">>,
    decided_at = erlang:system_time(millisecond)
},
AfterHash = crypto:hash(sha256, term_to_binary({decision, Decision})),

%% Create the move record
Move = #{
    trsn => approve,
    mode => #{checkpoint => CheckpointId, approver => human},
    produce => #{'p_approval_complete' => [{task, CheckpointId, done, Decision}]}
},

%% Generate receipt
Receipt = pnet_receipt:make(BeforeHash, AfterHash),

%% Receipt contains:
%% #{
%%   before_hash => BeforeHash,
%%   after_hash => AfterHash,
%%   move => Move,
%%   ts => Timestamp
%% }

%% Extract effects (custom implementations can extend this)
Effects = pnet_receipt:effects(Receipt),
%% => [] (default implementation)

%% Store receipt for audit trail
store_receipt(CheckpointId, Receipt).
```

---

## Migration Notes

### Mapping from Old Code to Helper Usage

#### Direct Token Manipulation → `pnet_marking`

**Before (old style):**
```erlang
%% Direct map manipulation
Marking = #{p1 => [a, b], p2 => [c]},
Marking2 = Marking#{p1 => [a, b, d]},  % Add token
Marking3 = maps:put(p2, [], Marking2).  % Clear place
```

**After (with helpers):**
```erlang
%% Using pnet_marking for safety and consistency
Marking = pnet_marking:new([p1, p2]),
Marking1 = pnet_marking:add(Marking, #{p1 => [a, b], p2 => [c]}),
case pnet_marking:add(Marking1, #{p1 => [d]}) of
    {error, Reason} -> handle_error(Reason);
    Marking2 -> Marking2
end,
case pnet_marking:set(Marking2, p2, []) of
    {error, Reason} -> handle_error(Reason);
    Marking3 -> Marking3
end.
```

#### Status Tracking → `wf_task` + `pnet_marking`

**Before (old style):**
```erlang
%% Ad-hoc status tracking
CaseState = #{status => running, task => my_task},
case maps:get(status, CaseState) of
    running -> handle_running();
    completed -> handle_completed()
end.
```

**After (with helpers):**
```erlang
%% Using wf_task for consistent lifecycle tokens
{produce, RunningToken} = wf_task:running(CaseId, Input, 'p_running'),
ControlMarking1 = pnet_marking:add(ControlMarking, RunningToken),

%% For status transitions:
case pnet_marking:apply(ControlMarking1,
                        #{'p_running' => [running_token]},
                        #{'p_completed' => [done_token]}) of
    {ok, NewMarking} -> case_completed();
    {error, Reason} -> handle_error(Reason)
end.
```

#### Timer Management → `wf_timerq`

**Before (old style):**
```erlang
%% Using erlang:send_after directly
TimerRef = erlang:send_after(5000, self(), timeout),
receive
    timeout -> handle_timeout()
after 3000 -> cancel  % Can't easily cancel!
end.
```

**After (with helpers):**
```erlang
%% Using wf_timerq for managed timeouts
TimerQ = wf_timerq:new(),
TimerKey = make_ref(),
Deadline = erlang:system_time(millisecond) + 5000,
TimerQ1 = wf_timerq:arm(TimerQ, TimerKey, Deadline, {timeout, data}),

%% Poll for expired timers
case wf_timerq:poll(TimerQ1, erlang:system_time(millisecond)) of
    {[{timeout, Data} | _], TimerQ2} -> handle_timeout(Data);
    {[], TimerQ2} -> continue_waiting(TimerQ2)
end,

%% Cancel if needed (can disarm before expiration)
TimerQ2 = wf_timerq:disarm(TimerQ1, TimerKey).
```

#### Subworkflow Place Mapping → `wf_scope`

**Before (old style):**
```erlang
%% Manual place translation
ParentTokens = #{parent_input => [data]},
%% Manual translation required
ChildTokens = maps:map(fun
    (parent_input, V) -> {child_input, V};
    (parent_output, V) -> {child_output, V}
end, ParentTokens).
```

**After (with helpers):**
```erlang
%% Using wf_scope for systematic translation
BindingTable => #{
    my_subflow => #{
        parent_input => child_input,
        parent_output => child_output
    }
},

%% Automatic translation
ChildTokens = wf_scope:enter(BindingTable, my_subflow, ParentTokens),
%% => #{child_input => [data]}

%% And translation back
ParentResult = wf_scope:leave(BindingTable, my_subflow, ChildTokens),
%% => #{parent_output => [result]}
```

### Key Benefits of Migration

1. **Consistency**: All modules use the same token format and marking operations
2. **Error Handling**: Helpers return `{error, Reason}` instead of crashing
3. **Audit Trail**: Receipts provide complete execution history
4. **Testability**: Pure functions are easier to test than stateful operations
5. **Type Safety**: Exported types enable dialyzer verification

---

## Quick Reference

### Common Patterns

#### Create and Initialize a Marking
```erlang
Marking = pnet_marking:new([place1, place2, place3])
```

#### Add Tokens to a Marking
```erlang
case pnet_marking:add(Marking, #{place1 => [token1, token2]}) of
    {error, Reason} -> handle_error(Reason);
    NewMarking -> NewMarking
end
```

#### Execute a Transition
```erlang
case pnet_marking:apply(Marking,
                        #{place1 => [token1]},
                        #{place2 => [token2]}) of
    {ok, NewMarking} -> NewMarking;
    {error, insufficient} -> not_enough_tokens;
    {error, bad_place} -> invalid_place
end
```

#### Create a Task Token
```erlang
{produce, ProduceMap} = wf_task:done(TaskId, Result, 'p_complete')
```

#### Set Up a Timeout
```erlang
TimerQ1 = wf_timerq:arm(TimerQ, my_key, Deadline, {produce, #{'p_timeout' => [timed_out]}})
```

#### Enter/Leave a Scope
```erlang
ChildTokens = wf_scope:enter(BindingTable, scope_id, ParentTokens),
ParentTokens = wf_scope:leave(BindingTable, scope_id, ChildTokens)
```

#### Generate an Audit Receipt
```erlang
Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move)
```

---

## Further Reading

- **`/Users/sac/cre/src/pnet_marking.erl`** - Marking algebra implementation
- **`/Users/sac/cre/src/wf_task.erl`** - Task lifecycle token constructors
- **`/Users/sac/cre/src/yawl_engine.erl`** - Core workflow engine using helpers
- **`/Users/sac/cre/src/yawl_control.erl`** - Control panel integration
- **`/Users/sac/cre/src/yawl_patterns.erl`** - Pattern composition using helpers

---

*Document Version: 1.0.0*
*Last Updated: 2025-02-06*
