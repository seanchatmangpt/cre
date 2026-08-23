# Petri Net Core Modules

This document describes the core Petri net formalism in CRE, including the `gen_pnet` behavior, type definitions, marking algebra, and YAWL extensions.

## Overview

CRE implements Petri nets as Erlang/OTP behaviors. A Petri net is a bipartite graph consisting of:

- **Places**: Nodes that hold tokens (represented as atoms)
- **Transitions**: Nodes that consume tokens from input places and produce tokens to output places
- **Arcs**: Directed edges connecting places to transitions and transitions to places
- **Tokens**: Data values that reside in places and flow through the net

The `gen_pnet` module provides a behavior for implementing Petri net workflows as `gen_server` processes, with automatic transition firing, token processing, and statistics tracking.

## Core Module Architecture

```
gen_pnet (Behavior)
    |
    +-- pnet_types     (Type validators)
    +-- pnet_marking   (Marking algebra)
    +-- pnet_mode      (Mode enumeration)
    +-- pnet_choice    (Deterministic choice)
    +-- pnet_receipt   (Execution receipts)

gen_yawl (Wrapper)
    |
    +-- Extends gen_pnet with 3-tuple fire/3 returns
```

## Type Definitions

The `pnet_types` module defines the core data structures:

### Basic Types

| Type | Definition | Description |
|------|------------|-------------|
| `place()` | `atom()` | A location where tokens reside |
| `trsn()` | `atom()` | A transition that consumes/produces tokens |
| `token()` | `term()` | Any Erlang term - tokens can carry any data |

### State Types

| Type | Definition | Description |
|------|------------|-------------|
| `marking()` | `#{place() => [token()]}` | Maps each place to its token multiset |
| `consume_map()` | `#{place() => [token()]}` | Tokens to be consumed during firing |
| `produce_map()` | `#{place() => [token()]}` | Tokens to be produced during firing |
| `mode()` | `#{place() => [token()]}` | A complete firing configuration (one valid way to fire) |

### Colored Petri Net Types

| Type | Definition | Description |
|------|------------|-------------|
| `var()` | `atom()` | Variable name for colored nets |
| `binding()` | `#{var() => term()}` | Maps variables to concrete values |
| `cmode()` | `{binding(), mode()}` | Colored mode with variable bindings |

### Execution Types

| Type | Definition | Description |
|------|------------|-------------|
| `move()` | `#{trsn := trsn(), mode := mode()\|cmode(), produce := produce_map()}` | A complete transition firing |
| `receipt()` | `#{before_hash := binary(), after_hash := binary(), move := move(), ts := integer()}` | Immutable audit record of a firing |

## gen_pnet Behavior

The `gen_pnet` behavior implements Petri net workflows as `gen_server` processes.

### Callback Contract

#### Structure Callbacks (Required)

These six callbacks define the Petri net topology and initial marking:

```erlang
-callback place_lst() -> [atom()].
%% Returns the names of all places in the net

-callback trsn_lst() -> [atom()].
%% Returns the names of all transitions in the net

-callback init_marking(Place :: atom(), UsrInfo :: _) -> [_].
%% Returns the initial token list for a given place

-callback preset(Trsn :: atom()) -> [atom()].
%% Returns the input (preset) places of a transition

-callback is_enabled(Trsn :: atom(),
                     Mode :: #{atom() => [_]},
                     UsrInfo :: _) -> boolean().
%% Determines if a transition is enabled in a given mode

-callback fire(Trsn :: atom(),
               Mode :: #{atom() => [_]},
               UsrInfo :: _) ->
    abort | {produce, #{atom() => [_]}}.
%% Returns tokens produced when a transition fires
```

#### Interface Callbacks (Required)

Seven callbacks determine how the net appears as an Erlang process:

```erlang
-callback init(NetArg :: _) -> _.
%% Initializes the net instance, returns UsrInfo

-callback handle_call(Request :: _,
                      From :: {pid(), _},
                      NetState :: #net_state{}) ->
    {reply, _} |
    {reply, _, #{atom() => [_]}} |
    noreply |
    {noreply, #{atom() => [_]}} |
    {stop, _, _}.

-callback handle_cast(Request :: _, NetState :: #net_state{}) ->
    noreply |
    {noreply, #{atom() => [_]}} |
    {stop, _}.

-callback handle_info(Info :: _, NetState :: #net_state{}) ->
    noreply |
    {noreply, #{atom() => [_]}} |
    {stop, _}.

-callback code_change(OldVsn :: _, NetState :: #net_state{}, Extra :: _) ->
    {ok, #net_state{}} | {error, _}.

-callback terminate(Reason :: _, NetState :: #net_state{}) -> ok.

-callback trigger(Place :: atom(), Token :: _, NetState :: #net_state{}) ->
    pass | drop.
%% Called for each token produced; can filter tokens
```

### The net_state Record

The internal state of a gen_pnet instance is stored in the `#net_state{}` record:

```erlang
-record(net_state, {
    marking   :: #{atom() => [_]},  % Current marking
    net_mod   :: atom(),              % Callback module
    usr_info  :: _,                   % User-defined state
    stats     :: #stats{} | undefined,
    tstart    :: integer(),            % Statistics start time
    cnt       :: non_neg_integer()     % Fire counter
}).
```

### API Functions

| Function | Description |
|----------|-------------|
| `start_link(NetMod, NetArg, Options)` | Start unregistered net |
| `start_link(ServerName, NetMod, NetArg, Options)` | Start registered net |
| `ls(Name, Place)` | Query tokens on a place |
| `marking(Name)` | Get complete marking map |
| `usr_info(Name)` | Get user info |
| `stats(Name)` | Get throughput statistics |
| `reset_stats(Name)` | Clear statistics |
| `call(Name, Request)` | Synchronous request |
| `call(Name, Request, Timeout)` | Synchronous with timeout |
| `cast(Name, Request)` | Asynchronous message |
| `reply(Client, Reply)` | Reply to deferred call |
| `state_property(Name, Pred, PlaceLst)` | Check predicate on marking |
| `inject(Name, ProduceMap)` | Inject tokens into net |
| `step(Name)` | Fire at most one transition |
| `drain(Name, MaxSteps)` | Fire until quiescent or limit |
| `stop(Name)` | Terminate the net |

### Transition Firing Process

1. **Progress Loop**: The net continuously sends `continue` messages to itself
2. **Mode Enumeration**: For each transition, enumerate all possible firing modes
3. **Enablement Check**: Call `is_enabled/3` to filter enabled modes
4. **Random Selection**: Pick a random enabled transition and mode
5. **Firing**: Call `fire/3`, consume tokens, then produce via `trigger/3`
6. **Statistics Update**: Track throughput every 1000 firings

## Marking Algebra

The `pnet_marking` module provides multiset operations on markings.

### Core Operations

```erlang
%% Create a new empty marking
pnet_marking:new([p1, p2]).
%% => #{p1 => [], p2 => []}

%% Get tokens at a place (total function)
pnet_marking:get(Marking, p1).
%% => {ok, [a, b]}  or {ok, []} for missing/empty

%% Set tokens at a place
pnet_marking:set(Marking, p1, [a, b]).

%% Add tokens (multiset union)
pnet_marking:add(Marking, #{p1 => [b], p2 => [c]}).

%% Take tokens (multiset subtraction)
pnet_marking:take(Marking, #{p1 => [a]}).
%% => {ok, UpdatedMarking} | {error, insufficient}

%% Apply a move atomically (consume + produce)
pnet_marking:apply(Marking, ConsumeMap, ProduceMap).
%% => {ok, UpdatedMarking} | {error, insufficient}
```

### Multiset Semantics

Tokens are treated as multisets - multiplicity matters:

```erlang
%% Adding [a,b] to [a] gives [a,a,b]
M1 = #{p => [a]},
M2 = pnet_marking:add(M1, #{p => [a, b]}),
%% => #{p => [a, a, b]}

%% Taking [a] from [a,a,b] leaves [a,b]
{ok, M3} = pnet_marking:take(M2, #{p => [a]}),
%% => #{p => [a, b]}
```

### Hash Function

```erlang
pnet_marking:hash(Marking).
%% => <<192,45,23,...>>  (SHA-256)
```

The hash is canonical (independent of insertion order) for reliable comparison.

## Mode Enumeration

The `pnet_mode` module enumerates all possible firing modes for a transition.

### Preset Multiplicity

A preset may contain duplicate places, indicating multiple arcs:

```erlang
pnet_mode:preset_counts([p1, p2, p1]).
%% => #{p1 => 2, p2 => 1}
```

This means the transition consumes 2 tokens from p1 and 1 from p2.

### Mode Enumeration

```erlang
%% For preset [p1, p2] and marking #{p1 => [a,b], p2 => [c]}
Modes = pnet_mode:enum_modes([p1, p2], #{p1 => [a,b], p2 => [c]}).
%% => [#{p1 => [a], p2 => [c]}, #{p1 => [b], p2 => [c]}]

%% For preset [p1, p1] and marking #{p1 => [a,b,c]}
Modes2 = pnet_mode:enum_modes([p1, p1], #{p1 => [a,b,c]}).
%% => [#{p1 => [a,b]}, #{p1 => [a,c]}, #{p1 => [b,c]}]
```

### Colored Modes

For colored Petri nets with variable bindings:

```erlang
pnet_mode:enum_cmodes(Trsn, Marking, UsrInfo, NetMod).
%% => {ok, [{#{}, #{p => [a]}}, {#{x => 1}, #{p => [b]}}]}
```

## Deterministic Choice

The `pnet_choice` module provides deterministic random selection with explicit RNG state threading.

```erlang
%% Create seeded RNG state
R0 = pnet_choice:seed(42).

%% Pick uniformly from a list
{Elem, R1} = pnet_choice:pick([a, b, c], R0).

%% Pick with weights
{Weighted, R2} = pnet_choice:pick_weighted([{a, 1}, {b, 3}, {c, 1}], R1).
```

The same seed always produces the same sequence, ensuring reproducibility.

## Receipts

The `pnet_receipt` module creates immutable audit records of state transitions.

```erlang
%% Create a receipt
Move = #{trsn => t1, mode => #{p => [a]}, produce => #{q => [b]}},
Receipt = pnet_receipt:make(HashBefore, HashAfter, Move).

%% Classify effects
pnet_receipt:effects(Receipt).
%% => {silent, Receipt} | {single_production, Receipt} | {multiple_production, Receipt}
```

## gen_yawl Extension

The `gen_yawl` module wraps `gen_pnet` with enhanced `fire/3` semantics.

### Enhanced fire/3 Return

In addition to the standard gen_pnet returns:

```erlang
%% Standard 2-tuple (no user info update)
{produce, ProduceMap}

%% Enhanced 3-tuple (updates user info)
{produce, ProduceMap, NewUsrInfo}

%% Abort the transition
abort
```

### Additional API Functions

| Function | Description |
|----------|-------------|
| `sync(Name, Timeout)` | Wait for net to stabilize |
| `enabled_transitions(Name)` | List currently enabled transitions |
| `withdraw(Name, WithdrawMap)` | Remove tokens from marking |
| `cancel_region(Name, RegionId)` | Cancel all tokens in a region |

### Wrapper State

The gen_yawl wrapper maintains additional state:

```erlang
-record(wrapper_state, {
    net_mod,
    net_state,
    fire_timeout = 5000,
    progress_timeout = 30000,
    marking_history = [],      % For cycle detection
    max_marking_history = 10,
    continue_count = 0,
    max_continue = 1000,
    regions = #{},             % For cancel_region/2
    checkpoint_interval = 0
}).
```

## Implementing a Custom Pattern

To implement a custom workflow pattern:

1. Create a module implementing the `gen_pnet` (or `gen_yawl`) behavior
2. Define places and transitions via `place_lst/0` and `trsn_lst/0`
3. Set initial marking via `init_marking/2`
4. Define net topology via `preset/1`
5. Implement enablement logic via `is_enabled/3`
6. Define token production via `fire/3`
7. Optionally filter tokens via `trigger/3`

### Example: Simple Sequence

```erlang
-module(sequence_net).
-behaviour(gen_pnet).

%% Structure callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1]).

%% Interface callbacks
-export([handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, trigger/3]).

-include("gen_pnet.hrl").

%% Places: start -> p1 -> p2 -> end
place_lst() -> [start, p1, p2, end].

%% Single transition
trsn_lst() -> [t].

%% Initial: one token in start
init_marking(start, _UsrInfo) -> [token];
init_marking(_, _UsrInfo) -> [].

%% Arc: start -> t -> p1 -> t -> p2 -> t -> end
preset(t) -> [start, p1, p2].

%% Enabled when all input places have tokens
is_enabled(t, Mode, _UsrInfo) ->
    maps:get(start, Mode, []) =/= [] andalso
    maps:get(p1, Mode, []) =/= [] andalso
    maps:get(p2, Mode, []) =/= [].

%% Consume one from each input, produce to output
%% (For a proper sequence, you'd use conditional logic here)
fire(t, _Mode, _UsrInfo) ->
    {produce, #{end => [done]}}.

init(_NetArg) -> #{}.

%% Standard gen_server callbacks...
handle_call(_Request, _From, _State) -> {reply, ok, _State}.
handle_cast(_Request, _State) -> {noreply, _State}.
handle_info(_Info, _State) -> {noreply, _State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _State) -> pass.
```

### Using gen_yawl for State Updates

```erlang
%% In fire/3, return 3-tuple to update user info
fire(t, Mode, UsrInfo) ->
    %% Update workflow variables
    NewUsrInfo = UsrInfo#{step_count => maps:get(step_count, UsrInfo, 0) + 1},
    {produce, #{next => [go]}, NewUsrInfo}.
```

## Testing Patterns

```erlang
%% Start a net
{ok, Pid} = gen_pnet:start_link(my_pattern, #{}, []).

%% Wait for stabilization (gen_yawl)
{ok, Marking} = gen_yawl:sync(Pid, 5000).

%% Query state
#{start := [], end := [Done]} = gen_yawl:marking(Pid).

%% Step-by-step execution
{ok, Receipt} = gen_yawl:step(Pid).

%% Drain until quiescent
{ok, Receipts} = gen_yawl:drain(Pid, 100).
```

## See Also

- `gen_pnet` - Core behavior module
- `gen_yawl` - YAWL wrapper with state updates
- `pnet_types` - Type validators
- `pnet_marking` - Marking algebra
- `pnet_mode` - Mode enumeration
- `pnet_choice` - Deterministic choice
- `pnet_receipt` - Execution receipts
- Workflow patterns in `src/patterns/`
