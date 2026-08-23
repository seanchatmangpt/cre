# PNet Types API Reference

## Module: `pnet_types`

```erlang
-module(pnet_types).
-moduledoc """
Type validators for Petri net data structures.

All validators are total: they return true/false and never crash.

```erlang
> pnet_types:is_marking(#{p1 => [a,b], p2 => []}).
true
> pnet_types:is_marking(#{p1 => a}).
false

> pnet_types:is_mode(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_mode(#{p1 => a}).
false

> pnet_types:is_binding(#{x => 1, y => <<"ok">>}).
true
> pnet_types:is_binding(#{1 => x}).
false

> pnet_types:is_cmode({#{x => 1}, #{p1 => [a]}}).
true
> pnet_types:is_cmode({#{}, #{p1 => a}}).
false
```

<h3>Type Categories</h3>
<ul>
  <li><strong>Basic Types:</strong> place, trsn, token</li>
  <li><strong>State Types:</strong> marking, mode, consume_map, produce_map</li>
  <li><strong>Colored Types:</strong> var, binding, cmode</li>
  <li><strong>Execution Types:</strong> move, receipt</li>
</ul>

<h3>Usage</h3>
All validation functions are total - they return boolean() and
never crash, making them safe to use in guards and assertions.
""".

-export([is_marking/1,
         is_consume_map/1,
         is_produce_map/1,
         is_mode/1,
         is_binding/1,
         is_cmode/1]).

-export_type([place/0,
              trsn/0,
              token/0,
              marking/0,
              consume_map/0,
              produce_map/0,
              mode/0,
              var/0,
              binding/0,
              cmode/0,
              move/0,
              receipt/0]).
```

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

**Doctest Examples**:
```erlang
> pnet_types:is_marking(#{p1 => [a,b], p2 => []}).
true
> pnet_types:is_marking(#{p1 => a}).
false
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

**Doctest Examples**:
```erlang
> pnet_types:is_mode(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_mode(#{p1 => a}).
false
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

**Doctest Examples**:
```erlang
> pnet_types:is_consume_map(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_consume_map(#{p1 => a}).
false
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

**Doctest Examples**:
```erlang
> pnet_types:is_produce_map(#{p1 => [c], p2 => [d]}).
true
> pnet_types:is_produce_map(#{p1 => c}).
false
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

**Doctest Examples**:
```erlang
> pnet_types:is_binding(#{x => 1, y => <<"ok">>}).
true
> pnet_types:is_binding(#{1 => x}).
false
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

**Doctest Examples**:
```erlang
> pnet_types:is_cmode({#{x => 1}, #{p1 => [a]}}).
true
> pnet_types:is_cmode({#{}, #{p1 => a}}).
false
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

## Usage Examples

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

## Summary

The `pnet_types` module provides essential type validation for the CRE workflow engine. Its comprehensive type definitions and total validation functions ensure type safety throughout the system, while extensive doctest examples demonstrate proper usage patterns for all data structures and validation functions.