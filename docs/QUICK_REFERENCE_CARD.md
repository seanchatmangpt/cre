# Quick Reference Card

## Architecture Overview

### Joe Armstrong Design Principle
**"One real OTP runner, everything else pure helpers/utilities + message contracts"**

```
gen_pnet (single OTP process)
├── pnet_types (type definitions)
├── pnet_marking (multiset operations)
├── pnet_mode (mode enumeration)
├── pnet_choice (deterministic RNG)
├── pnet_receipt (audit trails)
├── wf_timerq (deadline queue)
├── wf_task (task lifecycle)
└── wf_scope (boundary mapping)
```

## Key Data Structures

### Basic Types
```erlang
-type place() :: atom().
-type trsn() :: atom().
-type token() :: term().
-type marking() :: #{place() => [token()]}.
```

### Colored Net Types
```erlang
-type var() :: atom().
-type binding() :: #{var() => term()}.
-type cmode() :: {binding(), mode()}.
```

### Execution Types
```erlang
-type move() :: #{trsn := trsn(), mode := mode(), produce := produce_map()}.
-type receipt() :: #{before_hash := binary(), after_hash := binary(), move := move(), ts := integer()}.
```

## Core Utility Functions

### pnet_marking
```erlang
new([p1, p2]) -> #{p1 => [], p2 => []}
get(Marking, Place) -> {ok, Tokens} | {error, bad_place}
set(Marking, Place, Tokens) -> Marking | {error, bad_place}
add(Marking, ProduceMap) -> Marking | {error, bad_place}
take(Marking, ConsumeMap) -> {ok, Marking} | {error, bad_place | insufficient}
apply(Marking, ConsumeMap, ProduceMap) -> {ok, Marking} | {error, bad_place | insufficient}
hash(Marking) -> binary()
snapshot(Marking) -> Marking
```

### pnet_choice
```erlang
seed(12345) -> RngState
pick([a, b, c], RngState) -> {Element, NewRngState}
pick_weighted([{a,1}, {b,3}], RngState) -> {Element, NewRngState}
```

### pnet_mode
```erlang
preset_counts([p1, p2]) -> #{p1 => 1, p2 => 1}
preset_counts([p, p, q]) -> #{p => 2, q => 1}  % handles multiplicity
enum_modes([p1, p2], #{p1 => [a], p2 => [b]}) -> [#{p1 => [a], p2 => [b]}]
enum_cmodes(Trsn, Marking, UsrInfo, NetMod) -> [{Binding, Mode}]
```

### wf_task
```erlang
enabled(TaskId, Payload, Place) -> {produce, #{Place => [{task, TaskId, enabled, Payload}]}}
running(TaskId, Payload, Place) -> {produce, #{Place => [{task, TaskId, running, Payload}]}}
done(TaskId, Payload, Place) -> {produce, #{Place => [{task, TaskId, done, Payload}]}}
failed(TaskId, Payload, Place) -> {produce, #{Place => [{task, TaskId, failed, Payload}]}}
cancelled(TaskId, Payload, Place) -> {produce, #{Place => [{task, TaskId, cancelled, Payload}]}}
```

### wf_timerq
```erlang
new() -> []
arm(TimerQ, Key, Deadline, Event) -> NewTimerQ
disarm(TimerQ, Key) -> NewTimerQ
poll(TimerQ, Now) -> {[Event], NewTimerQ}
is_empty(TimerQ) -> boolean()
```

### wf_scope
```erlang
enter(BindingTable, ScopeId, Tokens) -> ChildProduceMap
leave(BindingTable, ScopeId, Tokens) -> ParentProduceMap
bindings(BindingTable, ScopeId) -> Mapping | {error, unknown_scope}
```

## YAWL Pattern Interface

### Required Callbacks (gen_yawl behavior)
```erlang
-behaviour(gen_yawl).

-export([place_lst/0, trsn_lst/0, preset/1, init/1,
         init_marking/2, is_enabled/3, fire/3, trigger/3]).

place_lst() -> [p1, p2, p3].
trsn_lst() -> [t1, t2].

preset(t1) -> [p1].

init(_NetArg) -> [].

init_marking(p1, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

is_enabled(t1, _Mode, _UsrInfo) -> true.
%% is_enabled/3: (Trsn, Mode, UsrInfo) -> boolean()

fire(t1, #{p1 => []}, _UsrInfo) -> {produce, #{p2 => [done]}}.
%% fire/3: Can return {produce, ProduceMap} or {produce, ProduceMap, NewUsrInfo}
```

### gen_yawl vs gen_pnet
- `gen_yawl`: YAWL wrapper with enhanced state updates
- `gen_pnet`: Core Petri net runner
- Callback names differ: `place_lst/0`/`trsn_lst/0` vs `places/0`/`transitions/0`

## Common Patterns

### Parallel Split (WCP-2)
```
start → [t1] → branch1, branch2
```

### Exclusive Choice (WCP-4)
```
start → [t_choice] → branch1 or branch2
```

### Simple Merge (WCP-5)
```
branch1 → [t_merge] → end
branch2 → [t_merge] → end
```

### N-out-of-M
```
input1 → [t_threshold] → end
input2 → [t_threshold] → end
input3 → [t_threshold] → end
```

## Workflow Creation Template

### gen_yawl Behavior (Recommended for YAWL workflows)

```erlang
-module(my_workflow).
-behaviour(gen_yawl).

-export([place_lst/0, trsn_lst/0, preset/1, init/1,
         init_marking/2, is_enabled/3, fire/3, trigger/3]).

%% Places and transitions
place_lst() -> [start, task1, task2, decision, end].
trsn_lst() -> [t_start, t_task1, t_task2, t_decision, t_end].

%% Presets (inputs to transitions)
preset(t_start) -> [start].
preset(t_task1) -> [start].
preset(t_task2) -> [start].
preset(t_decision) -> [task1, task2].
preset(t_end) -> [decision].

%% Initialization
init(_NetArg) -> [].

%% Initial marking
init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

%% Enable check (is_enabled/3: Trsn, Mode, UsrInfo) -> boolean()
is_enabled(t_decision, _Mode, _UsrInfo) ->
    %% Return true when transition should fire
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Transition firing (can return 2-tuple or 3-tuple for UsrInfo updates)
fire(t_start, #{start := [init]}, _UsrInfo) ->
    {produce, #{task1 => [enabled], task2 => [enabled]}};

fire(t_decision, #{task1 := [done], task2 := [done]}, UsrInfo) ->
    %% Example: return 3-tuple to update UsrInfo
    NewUsrInfo = maps:put(decision_made, true, UsrInfo),
    {produce, #{decision => [approved], end => [complete]}, NewUsrInfo};

fire(t_end, #{decision := [approved]}, _UsrInfo) ->
    {produce, #{}}.

%% External trigger (for human-in-the-loop, timers, etc.)
trigger(_Trsn, _TriggerData, _UsrInfo) ->
    {error, no_trigger}.
```

### pnet_net Behavior (Direct Petri net access)

```erlang
-module(my_pnet).
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/3, fire/3]).

%% Places and transitions
places() -> [p1, p2, p3].
transitions() -> [t1, t2].

%% Presets
preset(t1) -> [p1].
preset(t2) -> [p2].

%% Initialization
init(_NetArg) -> [].

%% Initial marking
init_marking(p1, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

%% Mode enumeration (modes/3: Trsn, Marking, UsrInfo)
modes(t1, Marking, _UsrInfo) ->
    case maps:get(p1, Marking, []) of
        [] -> [];
        _Tokens -> [#{p1 => []}]
    end;
modes(_Trsn, _Marking, _UsrInfo) ->
    [{}].

%% Transition firing
fire(t1, #{p1 := [Token]}, _UsrInfo) ->
    {produce, #{p2 => [Token]}}.
```

## Error Handling Patterns

### Total Functions
```erlang
% Always return {ok, Result} or {error, Reason}
get_tokens(Marking, Place) ->
    case pnet_marking:get(Marking, Place) of
        {ok, Tokens} -> {ok, Tokens};
        {error, bad_place} -> {error, invalid_place}
    end.
```

### Enable Validation (gen_yawl)
```erlang
is_enabled(Trsn, Mode, _UsrInfo) ->
    %% is_enabled/3: Return boolean() for gen_yawl
    %% Check guard conditions before allowing transition to fire
    case validate_mode(Mode) of
        true -> true;
        false -> false
    end.
```

### Mode Validation (pnet_net)
```erlang
modes(Transition, Marking, _UsrInfo) ->
    %% modes/3: Return list of valid modes for this transition (pnet_net)
    %% Each mode is a map of places to token lists to consume
    ValidModes = pnet_mode:enum_modes(PresetPlaces, Marking),
    lists:filter(fun is_mode_valid/1, ValidModes).
```

### Receipt Handling
```erlang
%% Receipts are automatically generated by gen_yawl/gen_pnet
%% Access them from the marking for audit trails
get_receipts(Marking) ->
    case pnet_marking:get(Marking, '_receipts') of
        {ok, Receipts} -> Receipts;
        {error, bad_place} -> []
    end.
```

## Debugging Tips

### State Inspection
```erlang
% Check current marking
{ok, Tokens} = pnet_marking:get(Marking, Place).

% Check available modes
Modes = pnet_mode:enum_modes(PresetPlaces, Marking).

% Check workflow progress
Receipts = pnet_receipt:extract_receipts(Marking).
```

### Common Issues
1. **No modes available**: Check token requirements
2. **Deadlock**: Ensure all paths can reach end
3. **Memory leaks**: Use pnet_marking:snapshot/1
4. **Non-determinism**: Use pnet_choice with seed

## Performance Tips

### Enable Caching (gen_yawl)
```erlang
% Cache static enable checks
is_enabled(t_static, _Mode, _UsrInfo) ->
    true.  % Always enabled for simple transitions
```

### Mode Caching (pnet_net)
```erlang
% Cache static modes
modes(t_static, _Marking, _UsrInfo) ->
    [{}].  % Single empty mode for simple transitions
```

### Efficient State Updates
```erlang
% Use immutable operations
NewMarking = pnet_marking:set(Marking, Place, Tokens).

% Batch updates
UpdateMap = #{p1 => [a], p2 => [b]},
NewMarking = pnet_marking:add(Marking, UpdateMap).
```

### Timer Optimization
```erlang
% Clean up old timers
TimerQ = wf_timerq:poll(TimerQ, CurrentTime),
TimerQ = wf_timerq:disarm(TimerQ, ExpiredKeys).
```

---

## Quick Commands

### Building
```bash
rebar3 deps           # Install dependencies
rebar3 compile        # Compile project
rebar3 shell          # Start interactive shell
```

### Testing
```bash
rebar3 ct             # Run common tests
rebar3 eunit          # Run unit tests
rebar3 cover          # Coverage analysis
```

### Quality
```bash
rebar3 dialyzer       # Type analysis
rebar3 xref           # Dependency check
rebar3 efmt -c        # Format code
```

### Examples
```bash
cd examples
./yawl_pnet_demo.sh run  # Run demo workflow
```

---

## Emergency Reference

### Critical Components
- `gen_pnet`: Only OTP process maintaining state
- `pnet_marking`: Core state management
- `pnet_choice`: Reproducible execution
- `pnet_receipt`: Audit trails

### Must-Read Documentation
- `docs/ARCHITECTURE.md` - Architecture concepts
- `docs/COMPLETE_API_REFERENCE.md` - Full API
- `docs/UTILITY_MODULES_GUIDE.md` - Utilities guide
- `docs/YAWL_PATTERNS_REFERENCE.md` - Patterns guide

### Quick Start
1. Read architecture docs
2. Work through getting started tutorial
3. Study utility modules
4. Implement simple workflow
5. Review examples

---

This quick reference provides the essential information needed to start working with the new CRE architecture. Keep it handy while learning and implementing workflows.