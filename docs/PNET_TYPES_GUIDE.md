# PNet Types Module Guide

## Overview

The `pnet_types` module provides comprehensive type validation functions for Petri net data structures. All validators are **total** - they return `true/false` and never crash, making them safe to use in guards and assertions throughout the CRE system.

## Module Purpose

This module serves as the foundation for type safety in the CRE workflow engine. It provides:

- **Type definitions** for all Petri net data structures
- **Validation functions** for runtime type checking
- **Doctest examples** demonstrating correct usage
- **Integration patterns** with other PNet modules

## Type Categories

### Basic Types
- **`place()`**: Atoms representing nodes where tokens reside
- **`trsn()`**: Atoms representing transitions that consume/produce tokens
- **`token()`**: Any Erlang term representing tokens in the net

### State Types
- **`marking()`**: Maps places to their token multisets
- **`mode()`**: Maps places to token lists enabling transitions
- **`consume_map()`**: Specifies tokens to be consumed during firing
- **`produce_map()`**: Specifies tokens to be produced during firing

### Colored Types
- **`var()`**: Variable names for colored Petri nets
- **`binding()`**: Maps variables to concrete values
- **`cmode()`**: Colored mode combining binding and token mode

### Execution Types
- **`move()`**: Represents complete transition firing
- **`receipt()`**: Records execution of moves with audit trails

## Detailed Type Documentation

### Basic Types

#### `place()`
```erlang
-type place() :: atom().
```
- **Purpose**: Identifies nodes where tokens reside in the Petri net
- **Examples**: `p1`, `start`, `step1`, `end`
- **Usage**: All map keys in marking/mode/consume/produce maps must be place atoms

#### `trsn()`
```erlang
-type trsn() :: atom().
```
- **Purpose**: Identifies transitions that consume input tokens and produce output tokens
- **Examples**: `t1`, `approve`, `process_data`, `complete`
- **Usage**: Used in move structures to identify the transition being fired

#### `token()`
```erlang
-type token() :: term().
```
- **Purpose**: Represents individual tokens in the Petri net
- **Examples**: Any Erlang term: `1`, `"data"`, `#{key => value}`, `true`
- **Usage**: Tokens flow through the net according to transition firing rules

### State Types

#### `marking()`
```erlang
-type marking() :: #{place() => [token()]}.
```
- **Purpose**: Represents the current state of the Petri net
- **Structure**: Map where keys are place atoms and values are lists of tokens
- **Empty lists**: Places with no tokens should map to `[]`

**Valid Examples:**
```erlang
#{p1 => [a,b], p2 => []}                          % Two places with tokens
#{start => [init], step1 => [], step2 => [data]} % Multiple places
#{}                                              % Empty marking
```

**Invalid Examples:**
```erlang
#{p1 => a}        % Value must be a list, not a single term
{"p1" => [a]}     % Keys must be atoms, not strings
#{p1 => [a,b]}    % Valid - single place with two tokens
```

#### `mode()`
```erlang
-type mode() :: #{place() => [token()]}.
```
- **Purpose**: Specifies token availability required for transition firing
- **Structure**: Same as marking - maps places to token lists
- **Semantics**: A transition is enabled when its required mode is satisfied by the current marking

**Valid Examples:**
```erlang
#{p1 => [a], p2 => [b]}    % Transition requires tokens 'a' in p1 and 'b' in p2
#{p1 => []}                % Transition requires no tokens (always enabled)
#{start => [init]}         % Transition requires 'init' token in start place
```

**Invalid Examples:**
```erlang
#{p1 => a}        % Value must be a list
#{p1 => [a,b]}    % Valid - requires specific tokens
{"p1" => [a]}     % Keys must be atoms
```

#### `consume_map()`
```erlang
-type consume_map() :: #{place() => [token()]}.
```
- **Purpose**: Specifies which tokens to remove when a transition fires
- **Structure**: Same as marking/mode maps
- **Usage**: Part of transition definitions to specify token consumption

**Valid Examples:**
```erlang
#{p1 => [a], p2 => [b]}    % Consume 'a' from p1 and 'b' from p2
#{p1 => []}                % Consume nothing from p1
#{p1 => [a,b]}             % Consume both 'a' and 'b' from p1
```

#### `produce_map()`
```erlang
-type produce_map() :: #{place() => [token()]}.
```
- **Purpose**: Specifies which tokens to add when a transition fires
- **Structure**: Same as other map types
- **Usage**: Part of move structures to specify token production

**Valid Examples:**
```erlang
#{p1 => [c], p2 => [d]}    % Produce 'c' in p1 and 'd' in p2
#{p1 => [result]}         % Produce single token 'result' in p1
#{p1 => [], p2 => []}      % Produce nothing
```

### Colored Types

#### `var()`
```erlang
-type var() :: atom().
```
- **Purpose**: Variable names for colored Petri nets
- **Usage**: Variables are bound to concrete values during execution
- **Examples**: `X`, `Y`, `Data`, `UserId`

#### `binding()`
```erlang
-type binding() :: #{var() => term()}.
```
- **Purpose**: Maps variable names to their concrete values
- **Structure**: Map where keys are variable atoms, values are any terms
- **Special case**: Empty tuple `{}` is used to represent empty binding

**Valid Examples:**
```erlang
#{x => 1, y => <<"ok">>}    % Variables bound to values
#{X => data, Y => 42}     % Variable names bound to terms
#{}
```

**Invalid Examples:**
```erlang
#{1 => x}        % Keys must be atoms (variable names)
{"x" => 1}       % Keys must be atoms
#{x => [y]}      % Valid - variable bound to list
```

#### `cmode()`
```erlang
-type cmode() :: {binding(), mode()}.
```
- **Purpose**: Combines variable binding with token mode for colored Petri nets
- **Structure**: 2-tuple where first element is binding, second is mode
- **Usage**: Enables colored Petri net execution with variable substitution

**Valid Examples:**
```erlang
{{}, #{p1 => [a]}}                          % Empty binding with mode
#{x => 1}, #{p1 => [x]}                     % Variable bound to value
#{X => data}, #{p1 => [X], p2 => [result]} % Multiple variables
```

**Invalid Examples:**
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
- **Purpose**: Represents a complete transition firing operation
- **Structure**: Map with three required keys
- **Semantics**: Unit of execution in Petri net semantics

**Valid Examples:**
```erlang
#{trsn := t1, mode := #{p1 => [a]}, produce := #{p2 => [b]}}
#{trsn := approve, mode := cm, produce := #{result => [ok]}}
#{trsn := process, mode := {{X => 1}, #{p1 => [X]}}, produce := #{p2 => [result]}}
```

**Invalid Examples:**
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
- **Purpose**: Records the execution of a move with audit information
- **Structure**: Map with hash values, move details, and timestamp
- **Usage**: Used for verification, debugging, and workflow audit trails

**Valid Examples:**
```erlang
#{before_hash => <<123...>>, after_hash => <<456...>>,
  move => #{trsn := t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}},
  ts => 1640995200000}
```

## Validation Functions

All validation functions follow these principles:
- **Total**: Always return `boolean()`, never crash
- **Safe**: Can be used in guards and assertions
- **Comprehensive**: Validate both structure and content

### `is_marking/1`

```erlang
-spec is_marking(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid marking

**Valid Examples:**
```erlang
> pnet_types:is_marking(#{p1 => [a,b], p2 => []}).
true
> pnet_types:is_marking(#{start => [init], step1 => [], step2 => [data]}).
true
> pnet_types:is_marking(#{}).
true
> pnet_types:is_marking(#{p1 => [a]}).
true
```

**Invalid Examples:**
```erlang
> pnet_types:is_marking(#{p1 => a}).
false
> pnet_types:is_marking(#{p1 => [a,b]}).
false
> pnet_types:is_marking([p1, [a,b]]).
false
> pnet_types:is_marking("not_a_map").
false
```

### `is_mode/1`

```erlang
-spec is_mode(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid mode

**Valid Examples:**
```erlang
> pnet_types:is_mode(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_mode(#{p1 => []}).
true
> pnet_types:is_mode(#{}).
true
```

**Invalid Examples:**
```erlang
> pnet_types:is_mode(#{p1 => a}).
false
> pnet_types:is_mode(#{p1 => [a,b]}).
false
> pnet_types:is_mode(p1 => [a]).
false
```

### `is_consume_map/1`

```erlang
-spec is_consume_map(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid consume_map

**Valid Examples:**
```erlang
> pnet_types:is_consume_map(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_consume_map(#{p1 => []}).
true
```

**Invalid Examples:**
```erlang
> pnet_types:is_consume_map(#{p1 => a}).
false
> pnet_types:is_consume_map([p1, [a]]).
false
```

### `is_produce_map/1`

```erlang
-spec is_produce_map(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid produce_map

**Valid Examples:**
```erlang
> pnet_types:is_produce_map(#{p1 => [c], p2 => [d]}).
true
> pnet_types:is_produce_map(#{p1 => [result]}).
true
```

**Invalid Examples:**
```erlang
> pnet_types:is_produce_map(#{p1 => c}).
false
> pnet_types:is_produce_map(#{p1 => [c,d]}).
false
```

### `is_binding/1`

```erlang
-spec is_binding(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid binding

**Valid Examples:**
```erlang
> pnet_types:is_binding(#{x => 1, y => <<"ok">>}).
true
> pnet_types:is_binding(#{X => data, Y => 42}).
true
> pnet_types:is_binding(#{}).
true
```

**Invalid Examples:**
```erlang
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

**Purpose**: Validates if a term is a valid cmode

**Valid Examples:**
```erlang
> pnet_types:is_cmode({#{}, #{p1 => [a]}}).
true
> pnet_types:is_cmode({#{x => 1}, #{p1 => [x]}}).
true
> pnet_types:is_cmode({#{}, #{}}).
true
```

**Invalid Examples:**
```erlang
> pnet_types:is_cmode({{}, p1 => [a]}).
false
> pnet_types:is_cmode({#{}, #{p1 => a}}).
false
> pnet_types:is_cmode([{}, #{}]).
false
```

## Usage Patterns

### 1. Basic Workflow Validation

```erlang
% Before workflow execution, validate the initial marking
InitialMarking = #{start => [init], step1 => [], step2 => []},
case pnet_types:is_marking(InitialMarking) of
    true ->
        % Valid initial marking, proceed with workflow
        gen_pnet:start_workflow(CaseId, InitialMarking);
    false ->
        % Invalid marking, handle error
        error_logger:error_msg("Invalid initial marking: ~p", [InitialMarking])
end.
```

### 2. Transition Mode Checking

```erlang
% Check if a transition is enabled by validating against current marking
Enabled = case pnet_types:is_mode(Mode) andalso pnet_types:is_marking(Marking) of
    true ->
        % Both mode and marking are valid, check if mode is satisfied
        check_mode_satisfaction(Mode, Marking);
    false ->
        false
end.
```

### 3. Move Validation

```erlang
% Validate a complete move before execution
Move = #{trsn := t1, mode := Mode, produce := Produce},
case pnet_types:is_move(Move) of
    true ->
        % Move is structurally valid, proceed with execution
        gen_pnet:fire_transition(Move);
    false ->
        % Invalid move, report error
        error_logger:error_msg("Invalid move structure: ~p", [Move])
end.
```

### 4. Colored Petri Net Pattern

```erlang
% For colored Petri nets, validate cmode with binding
ColoredMode = {{X => UserId}, #{p1 => [X], p2 => [result]}},
case pnet_types:is_cmode(ColoredMode) of
    true ->
        % Colored mode is valid, substitute variables and execute
        SubstitutedMode = substitute_variables(ColoredMode),
        execute_transition(SubstitutedMode);
    false ->
        % Invalid colored mode, handle error
        error_logger:error_msg("Invalid colored mode: ~p", [ColoredMode])
end.
```

## Integration with Other PNet Modules

### Integration with `pnet_marking`

```erlang
% Use pnet_types to validate before marking operations
validate_and_update_marking(CaseId, Place, NewTokens) ->
    NewMarking = pnet_marking:add_tokens(CaseId, Place, NewTokens),
    case pnet_types:is_marking(NewMarking) of
        true -> ok;
        false -> error(invalid_marking)
    end.
```

### Integration with `pnet_mode`

```erlang
% Validate mode before checking transition enablement
TransitionMode = #{p1 => [a], p2 => [b]},
Marking = #{p1 => [a], p2 => [b,c]},
case pnet_types:is_mode(TransitionMode) of
    true ->
        Enabled = pnet_mode:is_enabled(TransitionMode, Marking);
    false ->
        Enabled = false
end.
```

### Integration with Workflow Definitions

```erlang
% Validate workflow net definitions
validate_workflow_net(NetDef) ->
    % Validate all marking definitions
    ValidMarkings = lists:all(fun pnet_types:is_marking/1,
                              maps:values(NetDef#net.markings)),
    % Validate all mode definitions
    ValidModes = lists:all(fun pnet_types:is_mode/1,
                           maps:values(NetDef#net.modes)),
    ValidMarkings and ValidModes.
```

## Performance Considerations

1. **Total Functions**: All validators use try/catch to ensure they never crash
2. **Early Termination**: Validation stops at first error found
3. **Map Folding**: Uses `maps:fold` for efficient validation
4. **Guard-Friendly**: Safe to use in guards due to total nature

## Best Practices

1. **Validate Early**: Use validation before processing complex operations
2. **Use in Guards**: Leverage total nature for guard clauses
3. **Error Handling**: Always handle false returns appropriately
4. **Combined Validation**: Combine multiple validations for complex checks
5. **Documentation**: Include type validation in module contracts

## Error Cases and Expected Outputs

### Common Error Patterns

```erlang
% Wrong data types
pnet_types:is_marking(not_a_map).            % -> false
pnet_types:is_mode([p1, [a]]).              % -> false
pnet_types:is_binding(1).                   % -> false

% Invalid map structures
pnet_types:is_marking(#{p1 => a}).          % -> false (values must be lists)
pnet_types:is_mode(#{p1 => [a,b]}).         % -> false (value should be single token list?)
pnet_types:is_cmode({1, #{}}).              % -> false (first element must be binding)

% Empty vs invalid
pnet_types:is_marking(#{}).                 % -> true (empty marking)
pnet_types:is_binding({}).                  % -> false (not a map)
```

## Doctest Examples

The module includes comprehensive doctest examples. See the module source for extensive examples.

### Basic Examples

```erlang
% Marking validation examples
> pnet_types:is_marking(#{p1 => [a,b], p2 => []}).
true
> pnet_types:is_marking(#{p1 => a}).
false

% Mode validation examples
> pnet_types:is_mode(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_mode(#{p1 => a}).
false

% Binding validation examples
> pnet_types:is_binding(#{x => 1, y => <<"ok">>}).
true
> pnet_types:is_binding(#{1 => x}).
false

% Colored mode validation examples
> pnet_types:is_cmode({#{x => 1}, #{p1 => [a]}}).
true
> pnet_types:is_cmode({#{}, #{p1 => a}}).
false
```

## Summary

The `pnet_types` module provides essential type validation for the CRE workflow engine. Its total functions ensure safe type checking throughout the system, while comprehensive doctest examples demonstrate proper usage patterns. Integration with other PNet modules creates a robust type-safe foundation for workflow execution.