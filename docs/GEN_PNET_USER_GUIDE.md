# gen_pnet User Guide

**Generic Petri Net OTP Behavior - Single Runtime Component**

## Overview

`gen_pnet` is the core OTP behavior module that implements the Joe Armstrong design principle: **one real OTP runner, everything else pure helpers/utilities**. It serves as the single runtime component that maintains workflow state while all other modules are stateless utilities.

## Key Features

- **Single Runtime Component**: Only OTP process maintaining state in the entire system
- **Automatic Progress Loop**: Automatic token processing and transition firing
- **Receipt-Based Audit Trail**: Complete workflow execution tracking
- **Statistics Tracking**: Performance metrics (fps, token counts)
- **Flexible Callback System**: 13 callbacks for complete customization
- **Integration Ready**: Works with `gen_yawl` wrapper for 3-tuple fire/3 support

## Quick Start

```erlang
% Define a simple workflow
-module(sequence_workflow).
-behaviour(gen_pnet).

places() -> [start, step1, step2, end].
transitions() -> [t1, t2].

preset(t1) -> [start];
preset(t2) -> [step1].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{start := [init]}, _UsrInfo) -> [#{start => []}].
modes(t2, #{step1 := [done]}, _UsrInfo) -> [#{step1 => []}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{step1 => [done]}}.

fire(t2, #{step1 => []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.

% Start the workflow
{ok, Pid} = gen_pnet:start_link(sequence_workflow, [], []).

% Check current marking
{ok, Marking} = gen_pnet:marking(Pid).
% #{start => [], step1 => [], step2 => [], end => [complete]}

% Execute one step
{ok, Receipt} = gen_pnet:step(Pid).
% #{step1 => [done], end => [complete]}
```

## API Reference

### Core Lifecycle Functions

#### `start_link(NetMod, NetArg, Options) -> Result`
Start a new gen_pnet instance.

**Parameters:**
- `NetMod` - Module implementing gen_pnet behaviour
- `NetArg` - Initialization argument passed to `init/1` callback
- `Options` - List of configuration options

**Returns:**
- `{ok, Pid}` - Successfully started
- `{error, Reason}` - Failed to start

**Options:**
- `{name, Name}` - Registered process name
- `{debug, DebugFlags}` - Debug options
- `{timeout, Timeout}` - Startup timeout

#### `stop(Name) -> ok`
Stop a running gen_pnet instance.

**Parameters:**
- `Name` - Process name or PID

#### `step(Name) -> abort | {ok, Receipt}`
Execute one transition firing step.

**Parameters:**
- `Name` - Process name or PID

**Returns:**
- `abort` - No enabled transitions
- `{ok, Receipt}` - Transition fired with produced tokens

### State Inspection Functions

#### `marking(Name) -> {ok, Marking} | {error, Reason}`
Get current marking of all places.

**Parameters:**
- `Name` - Process name or PID

**Returns:**
- `{ok, Marking}` - Current marking as a map
- `{error, Reason}` - Error (e.g., process not running)

**Example:**
```erlang
{ok, Marking} = gen_pnet:marking(Pid).
% #{start => [], step1 => [done], step2 => [], end => [complete]}
```

#### `ls(Place, Name) -> {ok, Tokens} | {error, Reason}`
Get tokens in a specific place.

**Parameters:**
- `Place` - Place name (atom)
- `Name` - Process name or PID

**Returns:**
- `{ok, Tokens}` - List of tokens in the place
- `{error, #bad_place{}}` - Place not found

#### `usr_info(Name) -> UsrInfo`
Get user information.

**Parameters:**
- `Name` - Process name or PID

**Returns:**
- `UsrInfo` - User information from `init_marking/2`

#### `stats(Name) -> Stats`
Get performance statistics.

**Parameters:**
- `Name` - Process name or PID

**Returns:**
- `Stats` - Statistics record

**Stats Structure:**
```erlang
-record(stats, {
    start_time,      % When the process started
    last_step,       % Time of last step
    total_steps,     % Total steps executed
    total_tokens,    % Total tokens processed
    fps = 0.0,       % Frames per second (steps/second)
    marking_size = 0  % Current marking size
}).
```

#### `reset_stats(Name) -> ok`
Reset performance statistics.

**Parameters:**
- `Name` - Process name or PID

### Control Functions

#### `inject(Name, ProduceMap) -> Receipt`
Manually inject tokens into places.

**Parameters:**
- `Name` - Process name or PID
- `ProduceMap` - Map of places to token lists

**Returns:**
- `Receipt` - Receipt of injected tokens

**Example:**
```erlang
Receipt = gen_pnet:inject(Pid, #{start => [user_input]}).
% #{start => [user_input]}
```

#### `drain(Name, MaxSteps) -> Result`
Execute steps until no more transitions are enabled or MaxSteps reached.

**Parameters:**
- `Name` - Process name or PID
- `MaxSteps` - Maximum number of steps to execute

**Returns:**
- `{ok, {FinalReceipt, StepsTaken}}` - Successfully drained
- `abort` - No enabled transitions initially

### Communication Functions

#### `call(Name, Request) -> Reply | {error, Reason}`
Send synchronous request to gen_pnet.

**Parameters:**
- `Name` - Process name or PID
- `Request` - Request term

**Returns:**
- `Reply` - Response from handle_call/3
- `{error, Reason}` - Error (e.g., timeout)

#### `call(Name, Request, Timeout) -> Reply | {error, Reason}`
Send synchronous request with timeout.

**Parameters:**
- `Name` - Process name or PID
- `Request` - Request term
- `Timeout` - Timeout in milliseconds or `infinity`

#### `cast(Name, Request) -> ok`
Send asynchronous request to gen_pnet.

**Parameters:**
- `Name` - Process name or PID
- `Request` - Request term

#### `reply(Client, Reply) -> ok`
Send reply to a synchronous request.

**Parameters:**
- `Client` - `{pid(), Tag}` from the handle_call/3 callback
- `Reply` - Reply term

## Callback Functions

### Net Structure Callbacks (6 required)

#### `place_lst() -> [Place]`
Return all place names in the net.

**Returns:**
- `List` - List of atom place names

#### `trsn_lst() -> [Transition]`
Return all transition names in the net.

**Returns:**
- `List` - List of atom transition names

#### `init_marking(Place, UsrInfo) -> [Token]`
Return initial marking for a place.

**Parameters:**
- `Place` - Place name
- `UsrInfo` - User information from init

**Returns:**
- `Tokens` - List of tokens for the place (empty list if no initial tokens)

#### `preset(Transition) -> [Place]`
Return preset (input) places for a transition.

**Parameters:**
- `Transition` - Transition name

**Returns:**
- `Places` - List of input place names

#### `is_enabled(Mode, NetState, UsrInfo) -> boolean`
Determine if transition is enabled in a given mode.

**Parameters:**
- `Mode` - Current mode map
- `NetState` - Current net state
- `UsrInfo` - User information

**Returns:**
- `true` - Transition is enabled
- `false` - Transition is not enabled

#### `fire(Transition, Mode, UsrInfo) -> Action`
Define what happens when transition fires.

**Parameters:**
- `Transition` - Transition name
- `Mode` - Current mode map
- `UsrInfo` - User information

**Returns:**
- `{produce, ProduceMap}` - Map of places to tokens to produce
- `{consume, ConsumeMap}` - Map of places to tokens to consume
- `{produce, ProduceMap, Reply}` - With reply to caller
- `{consume, ConsumeMap, Reply}` - With reply to caller

### Interface Callbacks (7 standard OTP)

#### `init(NetArg) -> {ok, NetState} | {stop, Reason}`
Initialize the net instance.

**Parameters:**
- `NetArg` - Argument from start_link/3

**Returns:**
- `{ok, NetState}` - Successfully initialized
- `{stop, Reason}` - Failed to initialize

#### `handle_call(Request, From, NetState) -> Result`
Handle synchronous calls.

**Parameters:**
- `Request` - Call request
- `From` - `{pid(), Tag}` of the caller
- `NetState` - Current net state

**Returns:**
- `{reply, Reply, NewNetState}`
- `{noreply, NewNetState}`
- `{stop, Reason, Reply, NewNetState}`
- `{stop, Reason, NewNetState}`

#### `handle_cast(Request, NetState) -> Result`
Handle asynchronous calls.

**Parameters:**
- `Request` - Cast request
- `NetState` - Current net state

**Returns:**
- `{noreply, NewNetState}`
- `{stop, Reason, NewNetState}`

#### `handle_info(Info, NetState) -> Result`
Handle other messages.

**Parameters:**
- `Info` - Info message
- `NetState` - Current net state

**Returns:**
- `{noreply, NewNetState}`
- `{stop, Reason, NewNetState}`

#### `code_change(OldVsn, NetState, Extra) -> {ok, NewNetState}`
Handle hot code reload.

**Parameters:**
- `OldVsn` - Old version (any | {down, Version})
- `NetState` - Current net state
- `Extra` - Extra term from sys:change_code/4

**Returns:**
- `{ok, NewNetState}` - Successfully upgraded

#### `terminate(Reason, NetState) -> ok`
Cleanup on termination.

**Parameters:**
- `Reason` - Termination reason
- `NetState` - Current net state

**Returns:**
- `ok` - Always return ok

#### `trigger(Transition, Mode, UsrInfo) -> ok`
Execute side effects when tokens are produced.

**Parameters:**
- `Transition` - Transition name
- `Mode` - Current mode map
- `UsrInfo` - User information

**Returns:**
- `ok` - Always return ok

## Progress Loop Execution

The progress loop automatically executes transitions in sequence:

1. **Find enabled transitions**: Use `is_enabled/3` callback
2. **Pick transition**: Use deterministic choice if multiple enabled
3. **Fire transition**: Use `fire/3` callback to get new tokens
4. **Update marking**: Consume input tokens, produce output tokens
5. **Execute triggers**: Call `trigger/3` for side effects
6. **Update statistics**: Track performance metrics
7. **Repeat**: Continue until no transitions enabled

## Receipt System

Every firing operation returns a receipt that documents:

- **Transition fired**: Which transition executed
- **Mode used**: The mode in which it fired
- **Tokens produced**: Map of places to new tokens
- **Timestamp**: When the firing occurred

Receipts are essential for:
- **Audit trails**: Complete workflow execution history
- **Debugging**: Understanding token flow
- **Recovery**: Checkpoint and restore workflows

## Integration with gen_yawl

The `gen_yawl` wrapper provides 3-tuple fire/3 support:

```erlang
% Instead of fire/3 returning {produce, Map}
% fire/3 can return {Transition, Mode, Reply}

% In your gen_yawl module:
fire(t1, #{start := [init]}, _UsrInfo) ->
    {t1, #{start => []}, reply_to_client}.

% gen_yawl handles the 3-tuple format
% and automatically manages the reply
```

## Performance Considerations

- **FPS Tracking**: Steps per second measurement for performance monitoring
- **Token Counting**: Total tokens processed for workload analysis
- **Memory Efficiency**: Uses ETS for large markings
- **Deterministic Choice**: Consistent transition selection when multiple enabled

## Error Handling

- **Bad Place**: Returned when requesting tokens from non-existent place
- **Process Not Running**: All API functions handle gracefully
- **Invalid Mode**: `is_enabled/3` should handle invalid modes gracefully
- **Transition Errors**: Errors in fire/3 callback are caught and logged

## Examples

### Simple Workflow
```erlang
-module(simple_workflow).
-behaviour(gen_pnet).

places() -> [start, task, end].
transitions() -> [t1].

preset(t1) -> [start].
init(_NetArg) -> [].
init_marking(start, _UsrInfo) -> [init].
is_enabled(t1, #{start := [init]}, _UsrInfo) -> true.
fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{task => [done], end => [complete]}}.
```

### Workflow with State
```erlang
-module(stateful_workflow).
-behaviour(gen_pnet).

places() -> [start, process, complete].
transitions() -> [t1, t2].

preset(t1) -> [start].
preset(t2) -> [process].

init(UserId) -> #{user_id => UserId}.
init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

is_enabled(t1, #{start := [init]}, _UsrInfo) -> true.
is_enabled(t2, #{process := [done]}, _UsrInfo) -> true.

fire(t1, #{start := []}, #{user_id := UserId}) ->
    {produce, #{process => [{user_id, UserId, processing}]}}.

fire(t2, #{process := [Data]}, #{user_id := UserId}) ->
    {produce, #{complete => [Data]}}.
```

## Best Practices

1. **Pure Callbacks**: Keep fire/3 and is_enabled/3 pure functions
2. **Meaningful Tokens**: Use structured tokens, not just atoms
3. **Error Handling**: Handle all edge cases in callbacks
4. **Performance**: Use maps for fast token lookups
5. **Documentation**: Document all callback contracts
6. **Testing**: Test each callback independently

## Related Modules

- **gen_yawl**: Wrapper with 3-tuple fire/3 support
- **pnet_types**: Type definitions and validation
- **pnet_marking**: Multiset marking algebra
- **yawl_compile**: YAWL XML compilation to gen_pnet modules