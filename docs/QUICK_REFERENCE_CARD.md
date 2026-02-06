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

### Required Callbacks
```erlang
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/3, fire/3]).

places() -> [p1, p2, p3].
transitions() -> [t1, t2].

preset(t1) -> [p1].
preset(t2) -> [p2].

init(_NetArg) -> [].

init_marking(p1, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{p1 := [init]}, _UsrInfo) -> [#{p1 => []}].
%% modes/3: (Trsn, Marking, UsrInfo) -> [mode()]

fire(t1, #{p1 => []}, _UsrInfo) -> {produce, #{p2 => [done]}}.
```

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

```erlang
-module(my_workflow).
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/3, fire/3]).

%% Places and transitions
places() -> [start, task1, task2, decision, end].
transitions() -> [t_start, t_task1, t_task2, t_decision, t_end].

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

%% Mode enumeration (modes/3: Trsn, Marking, UsrInfo)
modes(t_decision, Marking, _UsrInfo) ->
    %% Only fire when both tasks are done
    case maps:get(task1, Marking, []) =/= [] andalso
         maps:get(task2, Marking, []) =/= [] of
        true -> [#{task1 => [], task2 => []}];
        false -> []
    end;
modes(_Trsn, _Marking, _UsrInfo) ->
    [{}].  % Default empty mode

%% Transition firing
fire(t_start, #{start := [init]}, _UsrInfo) ->
    {produce, #{task1 => [enabled], task2 => [enabled]}};

fire(t_decision, #{task1 := [done], task2 := [done]}, _UsrInfo) ->
    {produce, #{decision => [approved], end => [complete]}}.

fire(t_end, #{decision := [approved]}, _UsrInfo) ->
    {produce, #{}}.  % End state
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

### Mode Validation
```erlang
modes(Transition, Marking, _UsrInfo) ->
    %% modes/3: Return list of valid modes for this transition
    %% Each mode is a map of places to token lists to consume
    ValidModes = enum_valid_modes(Transition, Marking),
    lists:filter(fun is_mode_valid/1, ValidModes).
```

### Receipt Creation
```erlang
fire(Transition, Mode, _UsrInfo) ->
    BeforeHash = pnet_marking:hash(Marking),
    %% Execute transition with consume+produce
    {ok, NewMarking} = pnet_marking:apply(Marking, Mode, ProduceMap),
    AfterHash = pnet_marking:hash(NewMarking),
    Move = #{trsn => Transition, mode => Mode, produce => ProduceMap},
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    {produce, maps:merge(ProduceMap, #{receipt => [Receipt]})}.
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

### Mode Caching
```erlang
% Cache static modes
modes(t_static, _Marking, _UsrInfo) ->
    get_cached_modes(t_static).
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
- `docs/DIATAXIS_ARCHITECTURE.md` - Architecture concepts
- `docs/COMPLETE_API_REFERENCE.md` - Full API
- `docs/UTILITY_MODULES_GUIDE.md` - Utilities guide
- `docs/YAWL_PATTERNS_GUIDE.md` - Patterns guide

### Quick Start
1. Read architecture docs
2. Work through getting started tutorial
3. Study utility modules
4. Implement simple workflow
5. Review examples

---

This quick reference provides the essential information needed to start working with the new CRE architecture. Keep it handy while learning and implementing workflows.