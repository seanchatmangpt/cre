# PNet Core Modules Comprehensive Reference

## Overview

This document provides comprehensive documentation for the two core PNET modules that form the foundation of the CRE workflow engine:

1. **`pnet_types`** - Type validators for Petri net data structures
2. **`pnet_mode`** - Mode enumeration utilities for transition enablement

These modules implement the mathematical foundations of Petri net semantics, providing type safety and enablement analysis for both basic and colored Petri nets.

---

# Module 1: `pnet_types`

## Module Overview

The `pnet_types` module provides comprehensive type validation for all Petri net data structures in the CRE workflow engine. All validators are **total** - they return `boolean()` and never crash, making them safe to use in guards and assertions.

### Key Features

- **Total Validation Functions**: Never crash, always return boolean()
- **Comprehensive Type System**: Covers all Petri net data structures
- **Colored Net Support**: Variable bindings and colored modes
- **Execution Tracking**: Move and receipt structures for audit trails
- **Error Safety**: Safe for use in any context including guards

## Type Categories

### Basic Types
- `place()` - Nodes where tokens reside
- `trsn()` - Transition nodes that consume/produce tokens
- `token()` - Any Erlang term flowing through the net

### State Types
- `marking()` - Current token distribution across places
- `mode()` - Token availability for transition enablement
- `consume_map()` - Tokens to be consumed during firing
- `produce_map()` - Tokens to be produced during firing

### Colored Types
- `var()` - Variable names in colored Petri nets
- `binding()` - Variable-to-value mappings
- `cmode()` - Colored mode with binding and token mode

### Execution Types
- `move()` - Complete transition firing record
- `receipt()` - Audit trail for move execution

## Type Definitions

### Basic Types

#### `place()`
```erlang
-type place() :: atom().
```

**Description**: A place in the Petri net.

Places are nodes where tokens reside. Represented as atoms for efficient pattern matching and comparison.

**Examples**:
```erlang
p1          % Place with atom name
start       % Start place
end         % End place
step1       % Intermediate place
```

**Usage**:
```erlang
% All map keys in marking/mode/consume/produce maps must be place atoms
Marking = #{p1 => [a], p2 => [b]},
Mode = #{start => [init]},
```

#### `trsn()`
```erlang
-type trsn() :: atom().
```

**Description**: A transition in the Petri net.

Transitions are nodes that consume tokens from input places and produce tokens to output places when enabled and fired.

**Examples**:
```erlang
t1                 % Simple transition
approve            % Human approval transition
process_data       % Data processing transition
complete           % Completion transition
```

**Usage**:
```erlang
% Used in move structures to identify the transition being fired
Move = #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}},
```

#### `token()`
```erlang
-type token() :: term().
```

**Description**: A token in the Petri net.

Tokens can be any Erlang term, allowing for flexible data flow through the net.

**Examples**:
```erlang
1                      % Integer token
"data"                 % String token
#{key => value}       % Map token
true                   % Boolean token
[1,2,3]               % List token
```

**Usage**:
```erlang
% Tokens flow through the net according to transition firing rules
Marking = #{p1 => [1, "data"], p2 => [true]},
```

### State Types

#### `marking()`
```erlang
-type marking() :: #{place() => [token()]}.
```

**Description**: A marking maps places to their token multisets.

Each place atom maps to a list of tokens currently in that place. Empty lists represent places with no tokens.

**Valid Examples**:
```erlang
#{p1 => [a,b], p2 => []}                            % Two places with tokens
#{start => [init], step1 => [], step2 => [data]}    % Multiple places
#{}                                                % Empty marking
```

**Invalid Examples**:
```erlang
#{p1 => a}        % Value must be a list, not a single term
{"p1" => [a]}     % Keys must be atoms, not strings
#{p1 => [a,b]}    % Valid - single place with two tokens
```

#### `mode()`
```erlang
-type mode() :: #{place() => [token()]}.
```

**Description**: A mode specifies token availability for transition firing.

Maps each input place to the list of tokens that enable the transition. A transition is enabled when its mode is satisfied.

**Valid Examples**:
```erlang
#{p1 => [a], p2 => [b]}    % Transition requires tokens 'a' in p1 and 'b' in p2
#{p1 => []}                % Transition requires no tokens (always enabled)
#{start => [init]}         % Transition requires 'init' token in start place
```

**Invalid Examples**:
```erlang
#{p1 => a}        % Value must be a list
#{p1 => [a,b]}    % Valid - requires specific tokens
{"p1" => [a]}     % Keys must be atoms
```

#### `consume_map()`
```erlang
-type consume_map() :: #{place() => [token()]}.
```

**Description**: A consume map specifies tokens to be consumed.

Maps places to the specific list of tokens that will be removed during transition firing.

**Valid Examples**:
```erlang
#{p1 => [a], p2 => [b]}    % Consume 'a' from p1 and 'b' from p2
#{p1 => []}                % Consume nothing from p1
#{p1 => [a,b]}             % Consume both 'a' and 'b' from p1
```

**Invalid Examples**:
```erlang
#{p1 => a}        % Value must be a list
#{p1 => [a,b]}    % Valid - consumes multiple tokens
{"p1" => [a]}     % Keys must be atoms
```

#### `produce_map()`
```erlang
-type produce_map() :: #{place() => [token()]}.
```

**Description**: A produce map specifies tokens to be produced.

Maps places to the list of tokens that will be added during transition firing.

**Valid Examples**:
```erlang
#{p1 => [c], p2 => [d]}    % Produce 'c' in p1 and 'd' in p2
#{p1 => [result]}         % Produce single token 'result' in p1
#{p1 => [], p2 => []}      % Produce nothing
```

**Invalid Examples**:
```erlang
#{p1 => c}        % Value must be a list
#{p1 => [c,d]}    % Valid - produces multiple tokens
{"p1" => [c]}     % Keys must be atoms
```

### Colored Types

#### `var()`
```erlang
-type var() :: atom().
```

**Description**: A variable name in colored Petri nets.

Variables allow token values to be bound and referenced in colored Petri net expressions.

**Examples**:
```erlang
X          % Variable X
Y          % Variable Y
Data       % Variable for data
UserId     % Variable for user ID
```

**Usage**:
```erlang
% Variables are bound to concrete values during execution
Binding = #{X => 1, Y => <<"user">>},
```

#### `binding()`
```erlang
-type binding() :: #{var() => term()}.
```

**Description**: A binding maps variables to their concrete values.

Used in colored Petri nets to instantiate variable-based transition guards and arc expressions.

**Valid Examples**:
```erlang
#{x => 1, y => <<"ok">>}    % Variables bound to values
#{X => data, Y => 42}     % Variable names bound to terms
#{}
```

**Invalid Examples**:
```erlang
#{1 => x}        % Keys must be atoms (variable names)
{"x" => 1}       % Keys must be atoms
#{x => [y]}      % Valid - variable bound to list
```

#### `cmode()`
```erlang
-type cmode() :: {binding(), mode()}.
```

**Description**: A colored mode combines a binding with a token mode.

Extends the basic mode with variable bindings for colored Petri net execution. The binding provides values for variables used in transition evaluation.

**Valid Examples**:
```erlang
{{}, #{p1 => [a]}}                          % Empty binding with mode
#{x => 1}, #{p1 => [x]}                     % Variable bound to value
#{X => data}, #{p1 => [X], p2 => [result]} % Multiple variables
```

**Invalid Examples**:
```erlang
{{}, p1 => [a]}     % Must be a tuple, not a map
{#{}, #{p1 => a}}   % Mode values must be lists
{1, #{}}           % First element must be valid binding
```

### Execution Types

#### `move()`
```erlang
-type move() :: #{trsn := trsn(),
                  mode := mode() | cmode(),
                  produce := produce_map()}.
```

**Description**: A move represents a complete transition firing.

Contains the transition being fired, the mode (or cmode) under which it fires, and the produce map of output tokens. This is the unit of execution in Petri net semantics.

**Valid Examples**:
```erlang
#{trsn := t1, mode := #{p1 => [a]}, produce := #{p2 => [b]}}
#{trsn := approve, mode := cm, produce := #{result => [ok]}}
#{trsn := process, mode := {{X => 1}, #{p1 => [X]}}, produce := #{p2 => [result]}}
```

**Invalid Examples**:
```erlang
#{trsn := t1, mode := p1, produce => [b]}    % Invalid structure
#{t1 => t1, mode => #{}, produce => #{}}      % Wrong key names
#{trsn := t1}                                % Missing required fields
```

#### `receipt()`
```erlang
-type receipt() :: #{before_hash := binary(),
                     after_hash := binary(),
                     move := move(),
                     ts := integer()}.
```

**Description**: A receipt records the execution of a move.

Contains hashes of the marking before and after execution, the move that was executed, and a timestamp for ordering and verification purposes.

**Valid Example**:
```erlang
#{before_hash => <<123...>>, after_hash => <<456...>>,
  move => #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}},
  ts => 1640995200000}
```

## Validation Functions

All validation functions are **total** - they return `boolean()` and never crash, making them safe to use in guards and assertions.

### `is_marking/1`

```erlang
-spec is_marking(term()) -> boolean().
```

**Description**: Checks if a term is a valid marking.

A valid marking is a map where all keys are atoms (places) and all values are lists (of tokens). The function never crashes.

**Implementation**:
```erlang
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
is_marking(_) ->
    false.
```

**Doctest Examples**:
```erlang
> pnet_types:is_marking(#{p1 => [a,b], p2 => []}).
true
> pnet_types:is_marking(#{p1 => a}).
false
> pnet_types:is_marking(#{start => [init], step1 => [], step2 => [data]}).
true
> pnet_types:is_marking(#{}).
true
> pnet_types:is_marking([p1, [a,b]]).
false
> pnet_types:is_marking("not_a_map").
false
```

### `is_consume_map/1`

```erlang
-spec is_consume_map(term()) -> boolean().
```

**Description**: Checks if a term is a valid consume_map.

A valid consume_map is a map where all keys are atoms (places) and all values are lists (of tokens to consume). The function never crashes.

**Doctest Examples**:
```erlang
> pnet_types:is_consume_map(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_consume_map(#{p1 => []}).
true
> pnet_types:is_consume_map(#{p1 => a}).
false
> pnet_types:is_consume_map([p1, [a]]).
false
```

### `is_produce_map/1`

```erlang
-spec is_produce_map(term()) -> boolean().
```

**Description**: Checks if a term is a valid produce_map.

A valid produce_map is a map where all keys are atoms (places) and all values are lists (of tokens to produce). The function never crashes.

**Doctest Examples**:
```erlang
> pnet_types:is_produce_map(#{p1 => [c], p2 => [d]}).
true
> pnet_types:is_produce_map(#{p1 => [result]}).
true
> pnet_types:is_produce_map(#{p1 => c}).
false
> pnet_types:is_produce_map(#{p1 => [c,d]}).
false
```

### `is_mode/1`

```erlang
-spec is_mode(term()) -> boolean().
```

**Description**: Checks if a term is a valid mode.

A valid mode is a map where all keys are atoms (places) and all values are lists (of tokens). The function never crashes.

**Doctest Examples**:
```erlang
> pnet_types:is_mode(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_mode(#{p1 => []}).
true
> pnet_types:is_mode(#{}).
true
> pnet_types:is_mode(#{p1 => a}).
false
> pnet_types:is_mode(#{p1 => [a,b]}).
false
> pnet_types:is_mode(p1 => [a]).
false
```

### `is_binding/1`

```erlang
-spec is_binding(term()) -> boolean().
```

**Description**: Checks if a term is a valid binding.

A valid binding is a map where all keys are atoms (variables) and all values are any term (the bound values). The function never crashes.

**Implementation**:
```erlang
is_binding(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, _, _) when is_atom(K) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_binding(_) ->
    false.
```

**Doctest Examples**:
```erlang
> pnet_types:is_binding(#{x => 1, y => <<"ok">>}).
true
> pnet_types:is_binding(#{X => data, Y => 42}).
true
> pnet_types:is_binding(#{}).
true
> pnet_types:is_binding(#{1 => x}).
false
> pnet_types:is_binding(#{x => [y]}).
false
> pnet_types:is_binding([x,y]).
false
```

### `is_cmode/1`

```erlang
-spec is_cmode(term()) -> boolean().
```

**Description**: Checks if a term is a valid cmode.

A valid cmode is a 2-tuple where the first element is a binding and the second element is a mode. The function never crashes.

**Implementation**:
```erlang
is_cmode({Binding, Mode}) when is_tuple(Binding), tuple_size(Binding) =:= 0;
                              is_map(Binding);
                              is_list(Binding) ->
    %% For binding: accept empty tuple as map sentinel, or actual map
    %% The gen_pnet library uses {} as an empty binding representation
    IsBindingValid = case Binding of
        {} -> true;
        _ when is_map(Binding) -> is_binding(Binding);
        _ -> false
    end,
    IsModeValid = is_mode(Mode),
    IsBindingValid andalso IsModeValid;
is_cmode({Binding, Mode}) ->
    %% Try as map binding
    is_binding(Binding) andalso is_mode(Mode);
is_cmode(_) ->
    false.
```

**Doctest Examples**:
```erlang
> pnet_types:is_cmode({#{}, #{p1 => [a]}}).
true
> pnet_types:is_cmode({#{x => 1}, #{p1 => [x]}}).
true
> pnet_types:is_cmode({#{}, #{}}).
true
> pnet_types:is_cmode({{}, p1 => [a]}).
false
> pnet_types:is_cmode({#{}, #{p1 => a}}).
false
> pnet_types:is_cmode([{}, #{}]).
false
```

## Usage Patterns

### Basic Type Validation

```erlang
% Validate workflow components
validate_workflow(NetDef) ->
    ValidMarkings = lists:all(fun pnet_types:is_marking/1,
                              maps:values(NetDef#net.markings)),
    ValidModes = lists:all(fun pnet_types:is_mode/1,
                           maps:values(NetDef#net.modes)),
    ValidMarkings and ValidModes.
```

### Guard Usage

```erlang
% Safe to use in guards due to total nature
transition_enabled(Mode, Marking) when
    pnet_types:is_mode(Mode),
    pnet_types:is_marking(Marking) ->
    % Both types are validated, safe to use
    check_enablement(Mode, Marking).
```

### Colored Petri Net Pattern

```erlang
% Validate colored mode with binding
ColoredMode = {{X => UserId}, #{p1 => [X], p2 => [result]}},
case pnet_types:is_cmode(ColoredMode) of
    true ->
        % Colored mode is valid, substitute variables
        SubstitutedMode = substitute_variables(ColoredMode),
        execute_transition(SubstitutedMode);
    false ->
        % Invalid colored mode, handle error
        error_logger:error_msg("Invalid colored mode: ~p", [ColoredMode])
end.
```

### Move Validation

```erlang
% Validate complete move before execution
validate_and_execute(Move) ->
    case pnet_types:is_move(Move) of
        true ->
            % Move is structurally valid, proceed
            gen_pnet:fire_transition(Move);
        false ->
            % Invalid move, report error
            error_logger:error_msg("Invalid move: ~p", [Move])
    end.
```

## Error Handling

All validation functions implement comprehensive error handling:

1. **Type Checking**: First check if the input is the expected type (map for most functions)
2. **Content Validation**: Use try/catch to safely validate map contents
3. **Total Behavior**: Always return boolean(), never crash
4. **Early Termination**: Stop validation at first error found

### Error Case Examples

```erlang
% Wrong data types
pnet_types:is_marking(not_a_map).            % -> false
pnet_types:is_mode([p1, [a]]).              % -> false
pnet_types:is_binding(1).                   % -> false
pnet_types:is_cmode(1).                     % -> false

% Invalid map structures
pnet_types:is_marking(#{p1 => a}).          % -> false (values must be lists)
pnet_types:is_mode(#{p1 => [a,b]}).         % -> false (invalid mode format)
pnet_types:is_binding(#{1 => x}).           % -> false (keys must be atoms)
pnet_types:is_cmode({1, #{}}).              % -> false (invalid binding)

% Empty vs invalid
pnet_types:is_marking(#{}).                 % -> true (empty marking)
pnet_types:is_binding({}).                   % -> false (not a map)
```

## Performance Notes

1. **Try/Catch Pattern**: All functions use try/catch to ensure total behavior
2. **Map Folding**: Efficient validation using `maps:fold`
3. **Early Termination**: Validation stops at first error
4. **Guard Optimization**: Total functions optimize guard clause performance

## Integration with Other Modules

### With `pnet_marking`

```erlang
% Validate marking operations
update_marking(CaseId, Place, Tokens) ->
    NewMarking = pnet_marking:add_tokens(CaseId, Place, Tokens),
    case pnet_types:is_marking(NewMarking) of
        true -> ok;
        false -> error(invalid_marking)
    end.
```

### With `gen_pnet`

```erlang
% Validate workflow execution
start_workflow(CaseId, InitialMarking) ->
    case pnet_types:is_marking(InitialMarking) of
        true ->
            gen_pnet:start_workflow(CaseId, InitialMarking);
        false ->
            error(invalid_initial_marking)
    end.
```

### With YAWL Compilation

```erlang
% Validate YAWL compilation results
validate_yawl_net(NetDef) ->
    ValidateMarking = fun(M) -> pnet_types:is_marking(M) end,
    ValidateMode = fun(M) -> pnet_types:is_mode(M) end,

    MarkingsOk = lists:all(ValidateMarking, maps:values(NetDef#net.markings)),
    ModesOk = lists:all(ValidateMode, maps:values(NetDef#net.modes)),
    MarkingsOk and ModesOk.
```

---

# Module 2: `pnet_mode`

## Module Overview

The `pnet_mode` module provides mode enumeration utilities for the gen_pnet Petri net framework. Modes represent the different ways a transition can be fired given the current marking.

### Key Concepts

- **Mode**: Specifies which tokens are consumed from each input place when a transition fires
- **Colored Mode (cmode)**: Extends basic modes with variable bindings for colored Petri nets
- **Mode Enumeration**: Generates all possible valid firing modes for a transition
- **Deterministic Order**: Results are always returned in consistent term order

## Theoretical Foundation

### Mode Enumeration

Mode enumeration determines all possible ways a transition can be enabled based on the current marking. For a transition `t` with preset places `P`, the modes are all possible selections of tokens from these places.

### Mathematical Definition

Given:
- `P` = preset places of transition `t`
- `M` = current marking
- For each place `p ∈ P`, `M[p]` = list of tokens at `p`

The modes are all possible combinations:
```
∏_{p ∈ P} M[p]
```

This is the Cartesian product of token lists from all preset places.

## API Functions

### `preset_counts/1`

```erlang
-spec preset_counts([place()]) -> #{place() => non_neg_integer()}.
```

**Description**: Returns the count of tokens needed from each preset place.

For basic Petri nets, each place needs at least 1 token. This function returns a map indicating the minimum requirement.

**Parameters**:
- `PresetPlaces` - List of input places for a transition

**Return**: Map of places to required token count (always 1 for uncolored)

**Doctest Examples**:
```erlang
% Basic preset counting
> pnet_mode:preset_counts([p, p, q]).
#{p => 2, q => 1}

% Single place
> pnet_mode:preset_counts([p1]).
#{p1 => 1}

> pnet_mode:preset_counts([p1, p2]).
#{p1 => 1, p2 => 1}

> pnet_mode:preset_counts([]).
#{}

% Empty list input
> pnet_mode:preset_counts([p]).
#{p => 1}
```

### `enum_modes/2`

```erlang
-spec enum_modes([place()], marking()) -> [mode()].
```

**Description**: Enumerates all possible modes given the current marking.

A mode represents one valid way to fire a transition by selecting tokens from each preset place. This function generates the Cartesian product of available tokens across all preset places.

**Parameters**:
- `PresetPlaces` - List of input places for the transition
- `Marking` - Current marking of the net

**Return**: List of all valid modes

**Doctest Examples**:
```erlang
% Basic mode enumeration
> pnet_mode:enum_modes([p], #{p => [a]}).
[#{p => [a]}]

> pnet_mode:enum_modes([p], #{p => [a,b]}).
[#{p => [a]}, #{p => [b]}]

> pnet_mode:enum_modes([p,q], #{p => [a], q => [x]}).
[#{p => [a], q => [x]}]

> pnet_mode:enum_modes([p,q], #{p => [a], q => [x,y]}).
[#{p => [a], q => [x]}, #{p => [a], q => [y]}]

% Empty token cases
> pnet_mode:enum_modes([p], #{p => []}).
[]  % No tokens available

> pnet_mode:enum_modes([p,q], #{p => [a], q => []}).
[]  % Insufficient tokens in q
```

### `enum_cmodes/4`

```erlang
-spec enum_cmodes(trsn(), marking(), term(), module()) -> [cmode()].
```

**Description**: Enumerates colored modes with variable bindings.

For colored Petri nets, this function calls the net module's `cmodes` callback to get modes that include variable bindings. If the net module doesn't implement colored modes, falls back to basic mode enumeration.

**Parameters**:
- `Trsn` - The transition to enumerate modes for
- `Marking` - Current marking of the net
- `Ctx` - User context (usr_info) for the net
- `NetMod` - The net module implementing pnet_net behaviour

**Return**: List of all valid colored modes

**Doctest Examples**:
```erlang
% Colored mode enumeration with binding
> pnet_mode:enum_cmodes(auth, #{user => [alice,bob]}, #{}, user_net).
[{#{user => alice}, #{user => [alice]}},
 {#{user => bob}, #{user => [bob]}}]

% Complex colored mode
> pnet_mode:enum_cmodes(process, #{p1 => [X], p2 => [data]}, #{X => 1}, net).
[{#{X => 1}, #{p1 => [1], p2 => [data]}}]

> pnet_mode:enum_cmodes(auth, #{user => [alice,bob]}, #{}, user_net).
[{#{user => alice}, #{user => [alice]}},
 {#{user => bob}, #{user => [bob]}}]

% Colored mode with multiple variables
> pnet_mode:enum_cmodes(complex, #{p1 => [X], p2 => [Y]}, #{X => 1, Y => data}, net).
[{#{X => 1, Y => data}, #{p1 => [1], p2 => [data]}}]

% No tokens available
> pnet_mode:enum_cmodes(t, #{}, #{}, net).
[]  % No tokens to bind
```

## Cartesian Product Enumeration

The `enum_modes/2` function implements Cartesian product enumeration:

```erlang
% Mathematical: ∏_{p ∈ P} M[p]
> pnet_mode:enum_modes([p1,p2], #{p1 => [a,b], p2 => [1,2]}).
[
  #{p1 => [a], p2 => [1]},
  #{p1 => [a], p2 => [2]},
  #{p1 => [b], p2 => [1]},
  #{p1 => [b], p2 => [2]}
]
```

## Colored Net Theory

Colored Petri nets extend basic nets with:
- **Variables**: Place parameters that can be bound to values
- **Bindings**: Variable-to-value mappings
- **Colored modes**: Enablement with variable substitution

```erlang
% Variable binding pattern
{Binding, Mode} = cmode(),
% Where:
% - Binding: #{var => value}
% - Mode: #{place => [token]}

% Substitution process
SubstitutedMode = substitute_variables(Binding, Mode).
```

## Usage Examples

### Basic Enumeration

```erlang
% Simple case
> pnet_mode:enum_modes([p], #{p => [a,b,c]}).
[#{p => [a]}, #{p => [b]}, #{p => [c]}]

% Cartesian product
> pnet_mode:enum_modes([p,q], #{p => [a,b], q => [1,2]}).
[#{p => [a], q => [1]}, #{p => [a], q => [2]},
 #{p => [b], q => [1]}, #{p => [b], q => [2]}]

% Multiplicity
> pnet_mode:enum_modes([p,p,q], #{p => [a,b], q => [x]}).
[#{p => [a,b], q => [x]}]
```

### Colored Net Enumeration

```erlang
% Colored with binding
> pnet_mode:enum_cmodes(auth, #{user => [alice,bob]}, #{}, auth_net).
[{#{user => alice}, #{user => [alice]}},
 {#{user => bob}, #{user => [bob]}}]

% Empty binding fallback
> pnet_mode:enum_cmodes(t1, #{p => [a,b]}, ctx, basic_net).
[{#{}, #{p => [a]}}, {#{}, #{p => [b]}}]
```

### Error Cases

```erlang
% Insufficient tokens
> pnet_mode:enum_modes([p,p,p], #{p => [a,b]}).
[]  % Need 3, only 2 available

% Empty marking
> pnet_mode:enum_modes([p], #{p => []}).
[]  % No tokens available
```

## Integration Patterns

### With Transition Firing

```erlang
% Check transition enablement
is_transition_enabled(Transition, Marking) ->
    case pnet_mode:enum_modes(get_preset(Transition), Marking) of
        [] -> false;  % No modes enabled
        _ -> true     % At least one mode enabled
    end.

% Get all enabled modes for a transition
get_enabled_modes(Transition, Marking) ->
    pnet_mode:enum_modes(get_preset(Transition), Marking).
```

### With Colored Transitions

```erlang
% Process colored transition enablement
process_colored_transition(Transition, Marking, Context) ->
    case pnet_mode:enum_cmodes(Transition, Marking, Context, get_net_module()) of
        [] -> {error, not_enabled};
        Modes -> {ok, Modes}
    end.

% Example: User authentication workflow
authenticate_user(UserId) ->
    Marking = #{user => [UserId], password => [pwd]},
    Context = #{},
    case pnet_mode:enum_cmodes(auth, Marking, Context, user_net) of
        [{#{user => UserId}, AuthMode}] ->
            execute_auth(AuthMode);
        [] ->
            {error, invalid_credentials}
    end.
```

### With Move Validation

```erlang
% Validate move before execution
validate_move(Move, Marking) ->
    #{
        trsn := Trsn,
        mode := Mode,
        produce := Produce
    } = Move,

    % Get preset places for transition
    Preset = get_preset(Trsn),

    % Check if mode is valid for current marking
    case lists:member(Mode, pnet_mode:enum_modes(Preset, Marking)) of
        true -> valid;
        false -> invalid_mode
    end.
```

## Performance Optimization

### Early Termination

```erlang
% Check if any mode exists without full enumeration
has_any_mode(Preset, Marking) ->
    enum_modes(Preset, Marking) =/= [].

% Fast enablement check
is_enabled_fast(Trsn, Marking) ->
    Preset = get_preset(Trsn),
    has_any_mode(Preset, Marking).
```

### Mode Filtering

```erlang
% Filter modes by criteria
filter_modes(Modes, Criteria) ->
    lists:filter(fun(Mode) ->
        meets_criteria(Mode, Criteria)
    end, Modes).

% Example: Filter by specific token values
> Modes = [#{p1 => [a]}, #{p1 => [b]}],
> filter_modes(Modes, fun(#{p1 := [T]}) -> T =:= a end).
[#{p1 => [a]}]
```

### Caching Strategies

```erlang
% Cache mode enumerations for performance
-define(MODE_CACHE, mode_cache).

get_cached_modes(Key, ComputeFun) ->
    case cache:lookup(?MODE_CACHE, Key) of
        {ok, Modes} -> Modes;
        not_found ->
            Modes = ComputeFun(),
            cache:store(?MODE_CACHE, Key, Modes),
            Modes
    end.

% Usage example
get_enumerated_modes(Trsn, Marking) ->
    Key = {Trsn, pnet_marking:hash(Marking)},
    get_cached_modes(Key, fun() ->
        pnet_mode:enum_modes(get_preset(Trsn), Marking)
    end).
```

## Best Practices

### 1. Always Validate Input

```erlang
% Good - validate before enumeration
safe_enum_modes(Preset, Marking) ->
    case validate_preset_places(Preset) of
        {error, Reason} -> {error, Reason};
        ok ->
            case validate_marking(Marking) of
                {error, Reason} -> {error, Reason};
                ok -> pnet_mode:enum_modes(Preset, Marking)
            end
    end.
```

### 2. Handle Empty Results Gracefully

```erlang
% Good - handle empty mode list
handle_enumeration_result(Modes) ->
    case Modes of
        [] -> {error, no_enabled_modes};
        [Mode | _] -> {ok, Mode}  % Or process all modes
    end.

% Bad - assume non-empty
FirstMode = hd(Modes).  % May crash
```

### 3. Use Context Appropriately

```erlang
% Good - pass relevant context for colored nets
process_with_context(Trsn, Marking, UserContext) ->
    Cmodes = pnet_mode:enum_cmodes(Trsn, Marking, UserContext, user_net),
    process_colored_modes(Cmodes).

% Bad - ignore context
process_without_context(Trsn, Marking) ->
    % Missing important context information
    incomplete_processing().
```

### 4. Cache Expensive Operations

```erlang
% Good - cache expensive enumerations
get_cached_enumeration(Key, ComputeFun) ->
    case cache:get(Key) of
        undefined ->
            Result = ComputeFun(),
            cache:put(Key, Result),
            Result;
        Result -> Result
    end.

% Bad - recompute every time
expensive_enumeration() ->
    pnet_mode:enum_modes(large_preset, large_marking).  % Slow!
```

## Debugging Utilities

### Mode Inspection

```erlang
% Inspect available modes
inspect_modes(Trsn, Marking) ->
    Preset = get_preset(Trsn),
    Modes = pnet_mode:enum_modes(Preset, Marking),

    io:format("Transition: ~p~n", [Trsn]),
    io:format("Preset places: ~p~n", [Preset]),
    io:format("Current marking: ~p~n", [Marking]),
    io:format("Enabled modes: ~p~n", [Modes]).

% Example usage
> inspect_modes(t1, #{p1 => [a], p2 => [x]}).
Transition: t1
Preset places: [p1,p2]
Current marking: #{p1 => [a], p2 => [x]}
Enabled modes: [#{p1 => [a], p2 => [x]}]
```

### Performance Measurement

```erlang
% Measure enumeration performance
benchmark_enumeration(Preset, Marking, Iterations) ->
    Start = erlang:monotonic_time(millisecond),

    lists:foldl(fun(_, _) ->
        pnet_mode:enum_modes(Preset, Marking)
    end, ok, lists:seq(1, Iterations)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,
    io:format("Enumerated ~p modes in ~p ms (~p modes/ms)~n",
              [Iterations, Duration, (Iterations*1000)/Duration]).
```

## Error Handling Patterns

### Insufficient Tokens

```erlang
% Handle cases where tokens are insufficient
safe_mode_enumeration(Preset, Marking) ->
    case pnet_mode:enum_modes(Preset, Marking) of
        [] -> {error, insufficient_tokens};
        Modes -> {ok, Modes}
    end.

% Example usage
> safe_mode_enumeration([p], #{p => []}).
{error, insufficient_tokens}
```

### Invalid Place Names

```erlang
% Validate preset place names
validate_preset_places(Preset) ->
    case lists:all(fun is_atom/1, Preset) of
        false -> {error, invalid_place_names};
        true -> ok
    end.
```

### Variable Binding Issues

```erlang
% Handle binding failures in colored nets
safe_colored_enumeration(Trsn, Marking, Context) ->
    try
        Modes = pnet_mode:enum_cmodes(Trsn, Marking, Context, get_net_module()),
        case Modes of
            [] -> {error, no_valid_bindings};
            _ -> {ok, Modes}
        end
    catch
        Error:Reason -> {error, {enumeration_failed, Error, Reason}}
    end.
```

## Performance Notes

- **Time Complexity**: O(Π(C_i choose k_i)) where C_i = tokens in place i, k_i = required count
- **Space Complexity**: Grows exponentially with number of places/tokens
- **Deterministic Ordering**: Results always returned in consistent term order
- **Early Termination**: Stops if insufficient tokens anywhere in preset
- **Total Functions**: All functions are safe to use in any context

## Integration with Other Modules

### With `pnet_types`

```erlang
% Validate both marking and modes together
validate_transition_enablement(Trsn, Marking) ->
    Preset = get_preset(Trsn),
    Modes = pnet_mode:enum_modes(Preset, Marking),

    % Validate all enumerated modes
    ValidModes = lists:all(fun pnet_types:is_mode/1, Modes),
    case ValidModes of
        true -> {ok, Modes};
        false -> {error, invalid_modes}
    end.
```

### With `gen_pnet`

```erlang
% Complete transition enablement check
is_transition_ready(Trsn, Marking) ->
    Preset = get_preset(Trsn),
    case pnet_mode:enum_modes(Preset, Marking) of
        [] -> false;
        Modes -> gen_pnet:can_fire(Trsn, Modes)
    end.
```

### With YAWL Compilation

```erlang
% Validate YAWL net mode enumeration
validate_yawl_modes(NetDef) ->
    AllModes = maps:values(NetDef#net.modes),
    ValidateMode = fun(M) -> pnet_types:is_mode(M) end,

    lists:all(ValidateMode, AllModes).
```

## Summary

The `pnet_mode` module provides the mathematical foundation for transition enablement in Petri nets. Its mode enumeration capabilities support both basic and colored Petri nets, with comprehensive error handling and performance optimization strategies. The module integrates seamlessly with the type system provided by `pnet_types` to ensure type safety throughout the workflow engine.

Together, these two modules form the core foundation of the CRE workflow engine, providing:
- **Type Safety**: Comprehensive validation of all data structures
- **Enablement Analysis**: Mathematical enumeration of transition firing modes
- **Colored Net Support**: Variable binding and substitution capabilities
- **Performance Optimization**: Caching and early termination strategies
- **Error Handling**: Graceful handling of edge cases and invalid inputs

## Related Modules

- `pnet_marking` - Multiset marking algebra operations
- `gen_pnet` - Main Petri net runner and execution engine
- `yawl_validate` - YAWL specification validation
- `yawl_compile` - YAWL compilation to net modules