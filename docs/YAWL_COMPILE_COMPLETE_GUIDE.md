# YAWL Compilation Complete Guide

**From YAWL XML to gen_pnet Modules**

## Overview

The YAWL compiler (`yawl_compile`) transforms YAWL 2.1/2.2 specifications into `gen_pnet` compatible Erlang modules. This enables the execution of standard YAWL workflows within the CRE system, providing a bridge between YAWL XML specifications and efficient runtime execution.

## Key Features

- **Complete YAWL Support**: Full YAWL 2.1/2.2 specification compliance
- **Automatic Compilation**: XML to gen_pnet module generation
- **Total Validation**: Comprehensive validation of YAWL specifications
- **Pattern Integration**: All 43 workflow patterns supported
- **Code Generation**: Production-ready Erlang modules with complete callbacks

## Quick Start

### Basic Compilation

```erlang
% Compile YAWL specification to module
-spec compile(Spec :: yawl_spec(), Options :: map()) ->
    {ok, Module :: module()} | {error, Reason :: term()}.

% Example
{ok, OrderFulfillmentNet} = yawl_compile:compile(OrderFulfillmentSpec, #{
    seed => 12345,
    module_prefix => "order"
}).
```

### Compilation with File Output

```erlang
% Compile and save to file
-spec compile_to_file(Spec :: yawl_spec(), OutputDir :: file:name(), Options :: map()) ->
    ok | {error, Reason :: term()}.

% Example
ok = yawl_compile:compile_to_file(OrderFulfillmentSpec,
                                 "_build/default/lib/cre/src/gen_net",
                                 #{gen_observer => true}).
```

## Compiler Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `seed` | integer | random | Seed for deterministic choice |
| `module_prefix` | binary | "yawl_" | Prefix for generated module names |
| `output_dir` | string | undefined | Directory to save generated files |
| `include_source` | boolean | true | Include source code in compiled modules |
| `gen_observer` | boolean | false | Generate observer support |

### Detailed Options

#### `seed` - Deterministic Choice
Controls the random seed for nondeterministic transitions.

```erlang
#{seed => 12345}  % Deterministic behavior
#{seed => random}  % Random behavior
```

#### `module_prefix` - Module Naming
Prefix for generated YAWL module names.

```erlang
#{module_prefix => "order_"}  % Creates order_NetName modules
#{module_prefix => "wf_"}     % Creates wf_NetName modules
```

#### `output_dir` - File Output
Directory where generated modules are saved.

```erlang
#{output_dir => "/path/to/output"}
% Saves modules as files for development
```

#### `include_source` - Source Code
Include generated source code in compiled modules.

```erlang
#{include_source => true}   % Include source code
#{include_source => false}  % Compiled only
```

#### `gen_observer` - Observer Support
Generate observer callbacks for monitoring.

```erlang
#{gen_observer => true}  % Include observer support
#{gen_observer => false} % No observer support
```

## API Reference

### Core Functions

#### `compile(Spec, Options) -> Result`
Compile YAWL specification to gen_pnet module.

**Parameters:**
- `Spec` - YAWL specification (opaque type from `wf_spec:yawl_spec()`)
- `Options` - Compilation options map

**Returns:**
- `{ok, Module}` - Successfully compiled to module
- `{error, Reason}` - Compilation failed

#### `compile_to_file(Spec, OutputDir, Options) -> Result`
Compile YAWL specification and save to file.

**Parameters:**
- `Spec` - YAWL specification
- `OutputDir` - Directory path for output files
- `Options` - Compilation options map

**Returns:**
- `ok` - Successfully saved
- `{error, Reason}` - Save failed

### Utility Functions

#### `generate_module(NetId, NetInfo) -> ModuleInfo`
Generate module information for a YAWL net.

**Parameters:**
- `NetId` - Network identifier
- `NetInfo` - Network information structure

**Returns:**
- `ModuleInfo` - Module information structure

#### `generate_places(NetInfo) -> Places`
Generate place definitions for a YAWL net.

**Parameters:**
- `NetInfo` - Network information structure

**Returns:**
- `Places` - List of place definitions

#### `generate_transitions(NetInfo) -> Transitions`
Generate transition definitions for a YAWL net.

**Parameters:**
- `NetInfo` - Network information structure

**Returns:**
- `Transitions` - List of transition definitions

### Helper Functions

#### `sanitize_atom_name(Name) -> Atom`
Convert YAWL names to valid Erlang atoms.

**Parameters:**
- `Name` - Binary, atom, or list name

**Returns:**
- `Atom` - Valid Erlang atom

#### `place_atom(TaskId, FlowType) -> PlaceAtom`
Generate place atom for task/flow.

**Parameters:**
- `TaskId` - Task identifier
- `FlowType` - Flow type (input, output, etc.)

**Returns:**
- `PlaceAtom` - Valid place atom

#### `transition_atom(TaskId, Prefix) -> TransitionAtom`
Generate transition atom for task.

**Parameters:**
- `TaskId` - Task identifier
- `Prefix` - Optional prefix

**Returns:**
- `TransitionAtom` - Valid transition atom

## Generated Module Structure

### Basic Module Template

Each generated module follows this structure:

```erlang
-module(yawl_OrderFulfillment).
-behaviour(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% API exports
-export([start_link/2, start_link/3,
         ls/2,
         marking/1,
         call/2, call/3,
         cast/2,
         stats/1,
         reply/2,
         reset_stats/1,
         stop/1,
         usr_info/1,
         state_property/3,
         inject/2,
         step/1,
         drain/2]).

%% Callback exports
-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/1,
         is_enabled/3,
         fire/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2,
         trigger/3]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_pnet.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type task_id() :: binary().
-type place() :: atom().
-type transition() :: atom().
-type mode() :: #{place() => [term()]}.

%%====================================================================
%% Callback Functions
%%====================================================================

%% @doc Return all place names in the net
-spec place_lst() -> [place()].
place_lst() -> [input, output, task1, task2, task3].

%% @doc Return all transition names in the net
-spec trsn_lst() -> [transition()].
trsn_lst() -> [t_task1, t_task2, t_task3].

%% @doc Return initial marking for a place
-spec init_marking(place(), term()) -> [term()].
init_marking(input, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

%% @doc Return preset (input) places for a transition
-spec preset(transition()) -> [place()].
preset(t_task1) -> [input];
preset(t_task2) -> [task1];
preset(t_task3) -> [task2].

%% @doc Check if transition is enabled
-spec is_enabled(transition()) -> boolean().
is_enabled(T) when T == t_task1; T == t_task2; T == t_task3 -> true.

%% @doc Detailed enabled check
-spec is_enabled(transition(), mode(), term()) -> boolean().
is_enabled(t_task1, #{input := [init]}, _UsrInfo) -> true;
is_enabled(t_task2, #{task1 := [done]}, _UsrInfo) -> true;
is_enabled(t_task3, #{task2 := [done]}, _UsrInfo) -> true.

%% @doc Execute transition firing
-spec fire(transition(), mode(), term()) -> action().
fire(t_task1, #{input := []}, _UsrInfo) ->
    {produce, #{task1 => [done]}};
fire(t_task2, #{task1 := []}, _UsrInfo) ->
    {produce, #{task2 => [done]}};
fire(t_task3, #{task2 := []}, _UsrInfo) ->
    {produce, #{output => [complete]}}.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(_NetArg) -> [].
handle_call(Request, From, NetState) -> ... % generated
handle_cast(Request, NetState) -> ... % generated
handle_info(Info, NetState) -> ... % generated
code_change(OldVsn, NetState, Extra) -> {ok, NetState}.
terminate(Reason, NetState) -> ok.
trigger(Transition, Mode, UsrInfo) -> pass.
```

## YAWL Specification Format

### Input Specification

The compiler accepts YAWL specifications as opaque terms from `wf_spec:yawl_spec()`.

```erlang
% Example YAWL specification structure
-record(yawl_spec, {
    name :: binary(),
    tasks :: [task()],
    flows :: [flow()],
    conditions :: [condition()],
    decomposition :: decomposition()
}).

-record(task, {
    id :: task_id(),
    name :: binary(),
    type :: atomic | composite | tool | manual
}).

-record(flow, {
    source :: task_id(),
    target :: task_id(),
    type :: input_flow | output_flow | use_flow | condition_flow
}).
```

### Generated Callbacks

#### Place List Generation

```erlang
% Based on tasks and flows
place_lst() -> [input, output] ++
                [binary_to_atom(TaskId, utf8) || TaskId <- TaskIds] ++
                [binary_to_atom(ConditionId, utf8) || ConditionId <- ConditionIds].
```

#### Transition Generation

```erlang
% One transition per task
trsn_lst() -> [list_to_binary("t_" ++ binary_to_list(TaskId)) || TaskId <- TaskIds].
```

#### Preset Generation

```erlang
% Based on input flows
preset(T) -> case get_input_flows(T) of
                [F] -> [get_source_place(F)];
                [F1, F2] -> [get_source_place(F1), get_source_place(F2)]
              end.
```

## Validation Functions

### Total Validation

The compiler performs comprehensive validation:

1. **Net Structure**: Valid YAWL decomposition structure
2. **Task Connectivity**: All tasks properly connected with flows
3. **Condition Logic**: Input/output conditions correctly defined
4. **Flow Types**: Proper flow relationships
5. **Duplicate Names**: No conflicting task/condition names

### Error Handling

Common compilation errors:

- **Invalid YAWL Structure**: Malformed decomposition
- **Missing Tasks**: Tasks without flows
- **Circular Dependencies**: Circular flow references
- **Invalid Names**: Non-atomic task/condition names
- **Missing Conditions**: Conditions without proper flows

## Integration with Workflow Specifications

### Using `yawl_compiled`

The `yawl_compiled` module provides accessor functions:

```erlang
% Access compiled specifications
-spec get_net_list() -> [net_id()].
-spec get_net_info(net_id()) -> net_info().
-spec get_net_module(net_id()) -> module().
```

### Example Workflow

```erlang
% 1. Define YAWL specification
Spec = wf_spec:create_order_fulfillment(),

% 2. Compile to module
{ok, OrderNet} = yawl_compile:compile(Spec, #{
    seed => 42,
    module_prefix => "order_"
}),

% 3. Start the workflow
{ok, Pid} = gen_pnet:start_link(OrderNet, #{user_id => "user123"}, []),

% 4. Execute workflow
{ok, Receipt} = gen_pnet:step(Pid),
% Executes first transition
```

## Best Practices

### Compilation Guidelines

1. **Validate First**: Use `yawl_validate:validate/1` before compilation
2. **Use Deterministic Seeds**: For reproducible behavior
3. **Choose Meaningful Prefixes**: Avoid conflicts
4. **Include Source**: For debugging and documentation

### Performance Considerations

1. **Pre-compile**: Compile at build time for production
2. **Module Caching**: Cache compiled modules for reuse
3. **Minimal Options**: Use only necessary compilation options
4. **Observer Support**: Enable only when needed

### Debugging

1. **Source Code**: Always include source in development
2. **Error Messages**: Check validation errors carefully
3. **Generated Modules**: Inspect generated code for issues
4. **Logging**: Enable debug logging for compilation process

## Example: Complete Workflow

### Order Fulfillment YAWL

```erlang
% Define YAWL specification
OrderSpec = #{
    name => <<"OrderFulfillment">>,
    tasks => [
        #task{id => <<"ProcessOrder">>, type => atomic},
        #task{id => <<"CheckInventory">>, type => atomic},
        #task{id => <<"ShipOrder">>, type => atomic}
    ],
    flows => [
        #flow{source => <<"Start">>, target => <<"ProcessOrder">>, type => input_flow},
        #flow{source => <<"ProcessOrder">>, target => <<"CheckInventory">>, type => use_flow},
        #flow{source => <<"CheckInventory">>, target => <<"ShipOrder">>, type => use_flow},
        #flow{source => <<"ShipOrder">>, target => <<"Complete">>, type => output_flow}
    ]
},

% Compile with options
{ok, OrderFulfillment} = yawl_compile:compile(OrderSpec, #{
    seed => 123,
    module_prefix => "order_",
    include_source => true
}).
```

### Generated Module Usage

```erlang
% Start workflow
{ok, Pid} = gen_pnet:start_link(OrderFulfillment, #{order_id => "12345"}, []),

% Check initial marking
{ok, InitialMarking} = gen_pnet:marking(Pid),
% #{input => [init], ProcessOrder => [], CheckInventory => [], ShipOrder => [], output => []}

% Execute workflow step by step
{ok, Receipt1} = gen_pnet:step(Pid),
% Executes ProcessOrder transition

{ok, Receipt2} = gen_pnet:step(Pid),
% Executes CheckInventory transition

{ok, Receipt3} = gen_pnet:step(Pid),
% Executes ShipOrder transition

% Final marking
{ok, FinalMarking} = gen_pnet:marking(Pid),
% #{input => [], ProcessOrder => [], CheckInventory => [], ShipOrder => [], output => [complete]}
```

## Related Modules

- **`yawl_validate`**: YAWL specification validation
- **`yawl_compiled`**: Compiled specification accessor functions
- **`wf_spec`**: YAWL specification creation utilities
- **`gen_pnet`**: Generic Petri net behavior
- **`gen_yawl`**: YAWL wrapper with enhanced features