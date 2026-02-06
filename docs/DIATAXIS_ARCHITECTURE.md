# Diataxis Architecture Documentation

## Overview

This documentation describes the refactored CRE (Concurrent Runtime Environment) architecture following Joe Armstrong's principle: **one real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts**.

The new architecture consists of:
- **12 pure helper modules** for types, marking algebra, mode enumeration, choice enumeration, receipts, timers, tasks, scopes, YAWL validation, and YAWL compilation
- **1 OTP behavior** (`gen_pnet`) as the only required runtime component
- **Message contracts** for clean inter-process communication
- **YAWL tooling** for validation and compilation to the new interface

### Implementation Status

The implementation is currently in progress with core utility modules being actively developed:
- ✅ `pnet_types.erl` - Complete with total validation functions
- ✅ `pnet_marking.erl` - Complete with multiset marking algebra
- ✅ `pnet_mode.erl` - Complete with mode enumeration utilities

---

## Tutorial (Getting Started)

### Quick Start: Creating Your First YAWL Workflow

#### 1. Define a Simple YAWL Net

```erlang
% Define your YAWL net module implementing the pnet_net behaviour
-module(my_simple_workflow).
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1, init_marking/2,
         modes/2, fire/3]).

places() -> [start, task1, task2, end].
transitions() -> [t1, t2].

preset(t1) -> [start];
preset(t2) -> [task1].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{start := [init]}, _UsrInfo) -> [#{start => []}].
modes(t2, #{task1 := [done]}, _UsrInfo) -> [#{task1 => []}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{task1 => [done]}}.

fire(t2, #{task1 => []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.
```

#### 2. Start the Workflow Engine

```erlang
% Start the gen_pnet process
{ok, Pid} = gen_pnet:start_link(my_simple_workflow, [], []).

% Inject initial marking (optional - will be auto-created from init_marking)
ok = gen_pnet:produce(Pid, #{start => [init]}).

% Check current marking
#{start := [], task1 := [], task2 := [], end := []} = gen_pnet:marking(Pid).

% Progress happens automatically when tokens enable transitions
```

#### 3. Monitor Progress

```erlang
% Get the last receipt
Receipt = gen_pnet:last_receipt(Pid),
#{
    before_hash := Hash1,
    after_hash := Hash2,
    move := #{
        trsn := t1,
        mode := #{start => []},
        produce := #{task1 => [done]}
    }
} = Receipt.

% Subscribe to receipts for monitoring
spawn_monitor(fun() ->
    receive
        {pnet_receipt, Receipt} ->
            io:format("Transition fired: ~p~n", [Receipt])
    end
end).
```

#### 4. Working with Timers

```erlang
% Create a timer event for delayed task execution
TimerQ = wf_timerq:new(),
Now = erlang:monotonic_time(millisecond),
Deadline = Now + 5000, % 5 seconds from now
Event = {produce, #{timer_trigger => [ring]}},
TimerQ1 = wf_timerq:arm(TimerQ, my_timer, Deadline, Event),

% Poll the timer queue (in your process loop)
case wf_timerq:poll(TimerQ1, Now) of
    {[], TimerQ2} -> % No events ready yet
        timer:sleep(100), % Check again later
        timer_loop(TimerQ2);
    {[Event], TimerQ2} ->
        % Inject the event into gen_pnet
        gen_pnet:produce(Pid, Event),
        timer_loop(TimerQ2)
end.
```

#### 5. Working with External Tasks

```erlang
% Define task tokens for external work
TaskEvent = wf_task:done(task123, {result, "success"}, completion_result),
ok = gen_pnet:produce(Pid, TaskEvent).

% The task transition will fire when the task_result place has tokens
% Your external process can inject task status updates using wf_task constructors
```

---

## How-To Guides (Recipes)

### Recipe 1: Handling Colored Petri Nets

```erlang
% Define a colored net with variable bindings
-module(my_colored_workflow).
-behaviour(pnet_net).

% Export colored callbacks
-export([cmodes/2, cfire/3]).

% Colored net implementation
cmodes(t1, Marking, _UsrInfo) ->
    % Find all possible bindings for the transition
    case pnet_marking:get(Marking, input) of
        [Data] ->
            % Create binding for each possible transformation
            [
                {#{var => x}, #{input => [], output => [Data]}}
            ];
        _ -> []
    end.

cfire(t1, Binding, Mode, _UsrInfo) ->
    {Binding, #{var := X}} = Binding,
    {produce, #{output => [X]}}.

% Usage: gen_pnet automatically uses cmodes/cfire when colored
```

### Recipe 2: Implementing Custom Receipt Effects

```erlang
% Define a net with custom receipt effects
-module(my_monitored_workflow).
-behaviour(pnet_net).

-export([on_receipt/2]).

on_receipt(Receipt, State) ->
    % Log the receipt to external system
    spawn(fun() ->
        audit_log(Receipt),
        send_metrics(Receipt)
    end),

    % Emit effect commands for other processes
    [monitor_command, notification_command].

% Effects can be received by subscribers or handled by the runner
```

### Recipe 3: Managing Workflow Scopes

```erlang
% Create a hierarchical workflow using scopes
BindingTable = wf_scope:bindings_table(#{
    subworkflow => #{
        parent_start => child_start,
        parent_end => child_end
    }
}),

% Enter a scope (parent to child mapping)
ChildProduceMap = wf_scope:enter(BindingTable, subworkflow,
                                #{parent_start => [init_token]}),
ok = gen_pnet:produce(Pid, ChildProduceMap).

% Leave a scope (child to parent mapping)
ParentProduceMap = wf_scope:leave(BindingTable, subworkflow,
                                 #{child_end => [complete_token]}),
ok = gen_pnet:produce(Pid, ParentProduceMap).
```

### Recipe 4: Deterministic Choice Selection

```erlang
% When multiple transitions are enabled, choose deterministically
RngState = pnet_choice:seed(42),
EnabledTransitions = [t1, t2, t3],

% Pick one transition
case pnet_choice:pick(EnabledTransitions, RngState) of
    {SelectedTrsn, NewRngState} ->
        % Fire the selected transition
        Mode = get_mode_for_transition(SelectedTrsn),
        gen_pnet:produce(Pid, #{SelectedTrsn => Mode})
end.

% Weighted choice for priority-based selection
WeightedChoices = [{t1, 10}, {t2, 5}, {t3, 1}],
case pnet_choice:pick_weighted(WeightedChoices, RngState) of
    {SelectedTrsn, NewRngState} ->
        % Handle selected transition
end.
```

---

## Explanation (Reference Documentation)

### Architecture Philosophy

**Joe Armstrong's Design Principle:**
- **One real OTP runner**: `gen_pnet` is the only process with state
- **Everything else pure**: Helper modules are stateless utilities
- **Message contracts**: Clean interfaces between components
- **No code duplication**: Common functionality in shared modules

### Core Components

#### 1. `pnet_types` - Shared Type System

```erlang
% Basic types
place() :: atom()          % Place identifiers
trsn() :: atom()           % Transition identifiers
token() :: term()          % Token values
marking() :: #{place() => [token()]}  % Multiset markings

% Colored extension
var() :: atom()            % Variable names
binding() :: #{var() => term()}  % Variable bindings
cmode() :: {binding(), mode()}  % Colored modes

% Movement tracking
move() :: #{trsn := trsn(), mode := mode() | cmode(), produce := produce_map()}
receipt() :: #{before_hash := binary(), after_hash := binary(), move := move(), ts := integer()}
```

**Purpose**: Provides type definitions and validation functions for all components. Enforces consistency across the system.

#### 2. `pnet_marking` - Multiset Algebra

```erlang
% Core operations
new(Places) -> marking()                    % Create empty marking
get(Marking, Place) -> [token()]           % Get tokens from place
set(Marking, Place, Tokens) -> marking()    % Set tokens at place
add(Marking, ProduceMap) -> marking()       % Add tokens
take(Marking, ConsumeMap) -> {ok, marking()} % Remove tokens
apply(Marking, ConsumeMap, ProduceMap) -> {ok, marking()} % Atomic consume+produce

% Utilities
snapshot(Marking) -> marking()             % Copy marking
hash(Marking) -> binary()                  % Hash for equality checking
```

**Key Concept**: **Multiset operations** - multiplicity matters. `[a, a]` ≠ `[a]`.

#### 3. `pnet_mode` - Mode Enumeration

```erlang
% Basic mode enumeration
preset_counts(PresetPlaces) -> #{place() => non_neg_integer()}
enum_modes(PresetPlaces, Marking) -> [mode()]

% Colored extension (optional)
enum_cmodes(Trsn, Marking, Ctx, NetMod) -> [cmode()]
```

**Purpose**: Enumerates all possible modes (combinations of input tokens) for a given transition in the current marking.

#### 4. `pnet_choice` - Deterministic Nondeterminism

```erlang
% Random selection
seed(SeedTerm) -> rng_state()
pick(List, RngState) -> {T, RngState} | {error, empty}
pick_weighted(Items, RngState) -> {T, RngState} | {error, empty | bad_weights}
```

**Contract**: All nondeterminism in the system goes through this module, ensuring deterministic behavior when seeded.

#### 5. `pnet_receipt` - Receipt Tracking

```erlang
% Receipt creation
make(BeforeHash, AfterHash, Move) -> receipt()
timestamp() -> integer()  % Monotonic or system time

% Effects extraction
effects(Receipt) -> [term()]  % Default: [], extensible by net modules
```

**Purpose**: Provides audit trails and observability for all state changes.

#### 6. `wf_timerq` - Deadline Queue

```erlang
new() -> timerq()
arm(TimerQ, Key, Deadline, Event) -> timerq()
disarm(TimerQ, Key) -> timerq()
poll(TimerQ, Now) -> {Events, TimerQ1}
```

**Contract**: Pure utility - returns `{produce, ...}` events for injection into `gen_pnet`.

#### 7. `wf_task` - External Task Tokens

```erlang
% Task state constructors
enabled(TaskId, Payload, Place) -> {produce, produce_map()}
running(TaskId, Payload, Place) -> {produce, produce_map()}
done(TaskId, Output, Place) -> {produce, produce_map()}
failed(TaskId, Reason, Place) -> {produce, produce_map()}
cancelled(TaskId, Reason, Place) -> {produce, produce_map()}
```

**Contract**: Pure utility - constructs produce maps for task state transitions.

#### 8. `wf_scope` - Boundary Mapping

```erlang
enter(BindingTable, ScopeId, Delta) -> ChildProduceMap
leave(BindingTable, ScopeId, Delta) -> ParentProduceMap
bindings(BindingTable, ScopeId) -> Binding | {error, unknown_scope}
```

**Purpose**: Manages hierarchical workflows through scope boundaries.

#### 9. `pnet_net` - Net Semantics Behavior

```erlang
% Required callbacks (uncolored)
places() -> [place()]
transitions() -> [trsn()]
preset(Trsn) -> [place()]
init(NetArg) -> UsrInfo
init_marking(Place, UsrInfo) -> [token()]
modes(Trsn, Marking, UsrInfo) -> [mode()]
fire(Trsn, Mode, UsrInfo) -> abort | {produce, produce_map()}

% Optional callbacks
on_produce(Place, Token, StateSummary) -> keep | drop
on_receipt(Receipt, StateSummary) -> [effect_commands]

% Colored extension (optional)
cmodes(Trsn, Marking, UsrInfo) -> [cmode()]
cfire(Trsn, Binding, Mode, UsrInfo) -> abort | {produce, produce_map()}
```

**StateSummary Contract**: Minimal representation for decision-making (e.g., marking hash + usr_info).

#### 10. `gen_pnet` - OTP Runner

```erlang
% Process lifecycle
start_link(NetMod, NetArg, Options) -> {ok, pid()}
start_link(ServerName, NetMod, NetArg, Options) -> {ok, pid()}

% Introspection
marking(Name) -> marking()
ls(Name, Place) -> {ok, [token()]} | {error, bad_place}
usr_info(Name) -> term()

% Core workflow primitive
produce(Name, ProduceMap) -> ok  % Async injection

% External protocol passthrough
call(Name, Request) -> Reply
cast(Name, Request) -> ok
stop(Name) -> ok

% Receipts/observability (choose one)
last_receipt(Name) -> receipt() | undefined
receipts(Name, N) -> [receipt()]
subscribe(Name, Pid) -> ok  % Delivers {pnet_receipt, Receipt}
```

**Semantics Contract**:
1. After any `{produce, ProduceMap}` injection:
   - Apply production to marking
   - Run progress loop until blocked
   - Emit receipts and effect commands

#### 11. `yawl_validate` - YAWL Validation

```erlang
validate(XmlOrSpec) -> ok | {error, Reason}
```

**Purpose**: Validates YAWL XML specifications against the interface requirements.

#### 12. `yawl_compile` - YAWL Compilation

```erlang
% Basic compilation
compile(XmlOrSpec, Options) -> {ok, NetMod} | {error, Reason}

% Advanced compilation (with scope/task support)
compile(XmlOrSpec, Options) ->
    {ok, NetMod, ScopeTable, TimerPlan, TaskMap} | {error, Reason}
```

**Output Integration**:
- `ScopeTable` → `wf_scope`
- `TimerPlan` → `wf_timerq` events
- `TaskMap` → `wf_task` token mappings

### Message Contracts

#### To `gen_pnet`
- `{produce, ProduceMap}` - Async token injection
- `{cast, Request}` - Async cast message
- `{call, Request}` - Sync call (wrapped through API)

#### From `gen_pnet`
- `{pnet_receipt, Receipt}` - State change notification (if subscribed)
- `{pnet_effect, EffectCmd}` - Effect command (if enabled)

### Design Patterns

#### 1. Pure Utility Pattern
All helper modules are stateless with total functions (implemented in WIP files):
```erlang
% Good - total function that never crashes
is_marking(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_marking(_) -> false.

% Bad - may crash on bad input
is_marking(Term) -> is_map(Term).  % Incomplete validation
```

#### 2. Message Contract Pattern
Components communicate only through defined messages:
```erlang
% Direct function calls (avoid for inter-process communication)
wf_timerq:arm(TimerQ, Key, Deadline, Event).

% Message passing (preferred - through gen_pnet)
gen_pnet:produce(Pid, {produce, Event}).

% WIP implementation shows pattern for enum_modes
enum_cmodes(Trsn, Marking, Ctx, NetMod) ->
    case erlang:function_exported(NetMod, cmodes, 3) of
        true -> NetMod:cmodes(Trsn, Marking, Ctx);
        false -> [{#{}, M} || M <- enum_modes(NetMod:preset(Trsn), Marking)]
    end.
```

#### 3. Callback-Based Extension
Net modules extend functionality through callbacks:
```erlang
% Optional behavior in WIP implementation
enum_cmodes(Trsn, Marking, Ctx, NetMod) ->
    case erlang:function_exported(NetMod, cmodes, 3) of
        true -> NetMod:cmodes(Trsn, Marking, Ctx);
        false -> [{#{}, M} || M <- enum_modes(NetMod:preset(Trsn), Marking)]
    end.

% Runner calls when appropriate
% Net module decides effects without direct state access
% Pattern shown in pnet_mode.erl for colored net support
```

#### 4. Progress Loop Pattern
The `gen_pnet` maintains a progress loop (detailed implementation in WIP):
```erlang
progress_loop(Marking) ->
    Enabled = find_enabled_transitions(Marking),
    case Enabled of
        [] -> Blocked, stop
        [T|_] ->
            Mode = choose_mode(T, Marking),
            {produce, NewTokens} = fire_transition(T, Mode),
            NewMarking = apply_production(Marking, NewTokens),
            progress_loop(NewMarking)
    end.

% WIP implementation shows consume_tokens pattern for multiset semantics
consume_tokens(AvailableTokens, []) ->
    {ok, AvailableTokens};
consume_tokens([], [_|_]) ->
    {error, insufficient};
consume_tokens([Token | RestAvailable], TokensToTake) ->
    case lists:member(Token, TokensToTake) of
        true ->
            RemainingToTake = lists:delete(Token, TokensToTake),
            consume_tokens(RestAvailable, RemainingToTake);
        false ->
            case consume_tokens(RestAvailable, TokensToTake) of
                {ok, Remaining} -> {ok, [Token | Remaining]};
                {error, _} = Error -> Error
            end
    end.
```

---

## Reference (Complete API)

### Complete Module APIs

#### `pnet_types`
```erlang
-export([
    is_marking/1,
    is_consume_map/1,
    is_produce_map/1,
    is_mode/1,
    is_binding/1,
    is_cmode/1
]).
```

#### `pnet_marking`
```erlang
-export([
    new/1,
    get/2,
    set/3,
    add/2,
    take/2,
    apply/3,
    snapshot/1,
    hash/1
]).
```

#### `pnet_mode`
```erlang
-export([
    preset_counts/1,
    enum_modes/2,
    enum_cmodes/4
]).
```

#### `pnet_choice`
```erlang
-export([
    seed/1,
    pick/2,
    pick_weighted/2
]).
```

#### `pnet_receipt`
```erlang
-export([
    make/3,
    timestamp/0,
    effects/1
]).
```

#### `wf_timerq`
```erlang
-export([
    new/0,
    arm/4,
    disarm/2,
    poll/2
]).
```

#### `wf_task`
```erlang
-export([
    enabled/3,
    running/3,
    done/3,
    failed/3,
    cancelled/3
]).
```

#### `wf_scope`
```erlang
-export([
    enter/3,
    leave/3,
    bindings/2
]).
```

#### `gen_pnet`
```erlang
% API functions (exported)
-export([
    start_link/2, start_link/4,
    marking/1,
    ls/2,
    usr_info/1,
    produce/2,
    call/2, call/3,
    cast/2,
    stop/1,
    last_receipt/1,
    receipts/2,
    subscribe/2,
    reply/2
]).

% Internal callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).
```

#### `yawl_validate`
```erlang
-export([validate/1]).
```

#### `yawl_compile`
```erlang
-export([compile/2]).
```

### Type Definitions (Complete)

```erlang
% Basic types
-type place() :: atom().
-type trsn() :: atom().
-type token() :: term().
-type marking() :: #{place() => [token()]}.
-type consume_map() :: #{place() => [token()]}.
-type produce_map() :: #{place() => [token()]}.
-type mode() :: #{place() => [token()]}.

% Colored extension
-type var() :: atom().
-type binding() :: #{var() => term()}.
-type cmode() :: {binding(), mode()}.

% Movement tracking
-type move() :: #{
    trsn := trsn(),
    mode := mode() | cmode(),
    produce := produce_map()
}.
-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := move(),
    ts := integer()
}.

% Timer types
-type timerq() :: term().  % Internal representation
-type timer_key() :: term().
-type deadline() :: integer().
-type timer_event() :: {produce, produce_map()}.

% Task types
-type task_id() :: term().
-type task_event() :: {task, task_id(),
                      enabled | running | done | failed | cancelled,
                      term()}.

% Scope types
-type scope_id() :: term().
-type binding_table() :: #{scope_id() => #{place() => place()}}.

% RNG state
-type rng_state() :: term().

% Net module types
-type net_arg() :: term().
-type usr_info() :: term().
-type state_summary() :: term().
-type effect_command() :: term().
```

This architecture provides a clean, maintainable foundation for workflow systems with clear separation between the runtime engine and utility functions, following Joe Armstrong's principles of Erlang/OTP design.