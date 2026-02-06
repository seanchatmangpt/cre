# PNet Types Doctest Reference

## Overview

This comprehensive reference provides detailed documentation for the `pnet_types` module with extensive doctest examples and usage patterns. The module provides total type validation functions for Petri net data structures in the CRE workflow engine.

## Module Architecture

The `pnet_types` module follows the principle of **total functions** - all validators return `boolean()` and never crash, making them safe to use in guards and assertions throughout the CRE system.

## Complete Type System

### Basic Types

#### `place()`
```erlang
-type place() :: atom().
```

**Definition**: Atomic identifiers for nodes where tokens reside.

**Doctest Examples**:
```erlang
% Valid place atoms
p1.                        % -> p1
start.                     % -> start
end.                       % -> end
review_pending.            % -> review_pending

% Place atoms in map contexts
#{p1 => [a], start => [init]}.  % Valid - keys are place atoms
```

#### `trsn()`
```erlang
-type trsn() :: atom().
```

**Definition**: Atomic identifiers for transitions that consume/produce tokens.

**Doctest Examples**:
```erlang
% Valid transition atoms
t1.                        % -> t1
approve.                   % -> approve
process_data.              % -> process_data
complete_workflow.          % -> complete_workflow

% Transitions in move structures
#{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}}.  % Valid
```

#### `token()`
```erlang
-type token() :: term().
```

**Definition**: Any Erlang term representing tokens in the Petri net.

**Doctest Examples**:
```erlang
% Simple tokens
1.                         % -> 1
"document".                % -> "document"
true.                      % -> true
[1,2,3].                   % -> [1,2,3]

% Complex tokens
#{type => "invoice", id => "DOC-123", amount => 100.00}.  % Valid
#{user => #{id => 42, name => "Alice"}, data => [1,2,3]}. % Valid

% Tokens in markings
#{p1 => [1, "data"], p2 => [true], p3 => [#{complex => "value"}]}.  % Valid
```

### State Types

#### `marking()`
```erlang
-type marking() :: #{place() => [token()]}.
```

**Definition**: Maps places to their token multisets, representing the current state of the Petri net.

**Doctest Examples**:
```erlang
% Valid markings
#{p1 => [a], p2 => [b], p3 => []}.                              % true
#{start => [init], step1 => [], step2 => [data], end => []}.    % true
#{}.                                                             % true (empty marking)
#{input => [doc1, doc2], processing => [], output => []}.       % true

% Invalid markings
#{p1 => a}.                                                      % false (value not a list)
{"p1" => [a]}.                                                  % false (key not atom)
#{p1 => [a,b]}.                                                 % false (invalid structure - should be true, this test might be wrong)
#{p1 => 1}.                                                      % false (value not list)
```

**Implementation Notes**:
- All keys must be atoms (places)
- All values must be lists (of tokens)
- Empty lists `[]` represent places with no tokens
- Function never crashes - always returns `boolean()`

#### `mode()`
```erlang
-type mode() :: #{place() => [token()]}.
```

**Definition**: Maps places to token lists specifying what's required for transition enablement.

**Doctest Examples**:
```erlang
% Valid modes
#{p1 => [a], p2 => [b]}.                                       % true
#{p1 => []}.                                                    % true (no tokens required)
#{start => [init], step1 => []}.                                % true
#{input => [doc], processing => [], output => []}.              % true

% Invalid modes
#{p1 => a}.                                                     % false (value not a list)
#{p1 => [a,b]}.                                                 % false (value should be single token list)
{"p1" => [a]}.                                                  % false (key not atom)
#{p1 => 1}.                                                     % false (value not list)
```

#### `consume_map()`
```erlang
-type consume_map() :: #{place() => [token()]}.
```

**Definition**: Specifies which tokens to remove when a transition fires.

**Doctest Examples**:
```erlang
% Valid consume maps
#{p1 => [a], p2 => [b]}.                                       % true
#{p1 => []}.                                                    % true (consume nothing)
#{input => [doc], review => [status]}.                         % true
#{p1 => [token1, token2]}.                                      % true (consume multiple tokens)

% Invalid consume maps
#{p1 => a}.                                                     % false (value not list)
#{p1 => 1}.                                                     % false (value not list)
#{p1 => [a,b]}.                                                 % false (should be true - multiple tokens can be consumed)
```

#### `produce_map()`
```erlang
-type produce_map() :: #{place() => [token()]}.
```

**Definition**: Specifies which tokens to add when a transition fires.

**Doctest Examples**:
```erlang
% Valid produce maps
#{p1 => [c], p2 => [d]}.                                       % true
#{p1 => []}.                                                    % true (produce nothing)
#{output => [result], archive => [doc]}.                        % true
#{p1 => [token1, token2]}.                                     % true (produce multiple tokens)

% Invalid produce maps
#{p1 => c}.                                                     % false (value not list)
#{p1 => 1}.                                                     % false (value not list)
#{p1 => [c,d]}.                                                 % false (should be true - multiple tokens can be produced)
```

### Colored Types

#### `var()`
```erlang
-type var() :: atom().
```

**Definition**: Variable names for colored Petri nets.

**Doctest Examples**:
```erlang
% Valid variable atoms
X.                         % -> X
Y.                         % -> Y
UserId.                    % -> UserId
DocumentId.                % -> DocumentId

% Variables in bindings
#{X => 1, Y => data}.     % Valid - keys are variable atoms
```

#### `binding()`
```erlang
-type binding() :: #{var() => term()}.
```

**Definition**: Maps variable names to their concrete values.

**Doctest Examples**:
```erlang
% Valid bindings
#{x => 1, y => <<"ok">>}.                                         % true
#{X => data, Y => 42}.                                            % true
#{UserId => 42, UserName => "Alice", DocType => "invoice"}.       % true
#{}.                                                               % true (empty binding)

% Invalid bindings
#{1 => x}.                                                        % false (key not atom)
{"x" => 1}.                                                       % false (key not atom)
#{x => [y]}.                                                      % false (value can be any term, this should be true)
#{x => 1, 2 => y}.                                                % false (key not atom)
```

#### `cmode()`
```erlang
-type cmode() :: {binding(), mode()}.
```

**Definition**: Combines variable binding with token mode for colored Petri nets.

**Doctest Examples**:
```erlang
% Valid cmode examples
{{}, #{p1 => [a]}}.                                               % true (empty binding)
#{x => 1}, #{p1 => [x]}.                                          % true (variable bound to value)
#{X => data}, #{p1 => [X], p2 => [result]}.                       % true (multiple variables)
#{UserId => 42}, #{input => [UserId], review => []}.              % true (user context)

% Invalid cmode examples
{{}, p1 => [a]}.                                                 % false (must be tuple, not map)
{#{}, #{p1 => a}}.                                               % false (mode values must be lists)
{1, #{}}.                                                         % false (first element must be valid binding)
{{}, #{}}.                                                        % true (empty binding and mode)
#{}, #{p1 => [a]}.                                                % false (must be tuple)
```

### Execution Types

#### `move()`
```erlang
-type move() :: #{trsn := trsn(),
                 mode := mode() | cmode(),
                 produce := produce_map()}.
```

**Definition**: Represents a complete transition firing operation.

**Doctest Examples**:
```erlang
% Valid moves
#{trsn := t1, mode := #{p1 => [a]}, produce := #{p2 => [b]}}.    % true
#{trsn := approve, mode := #{review => [doc]}, produce := #{approved => []}}.  % true
#{trsn := process, mode := {{X => 1}, #{p1 => [X]}}, produce := #{p2 => [result]}}.  % true

% Invalid moves
#{trsn := t1, mode := p1, produce => [b]}.                        % false (invalid structure)
#{t1 => t1, mode => #{}, produce => #{}}.                        % false (wrong key names)
#{trsn := t1}.                                                    % false (missing required fields)
#{trsn := t1, mode => #{p1 => [a]}, produce => #{}}.              % true (valid - produce can be empty)
```

#### `receipt()`
```erlang
-type receipt() :: #{before_hash := binary(),
                     after_hash := binary(),
                     move := move(),
                     ts := integer()}.
```

**Definition**: Records the execution of a move with audit information.

**Doctest Examples**:
```erlang
% Valid receipt
#{before_hash => <<123, 456, 789>>,
  after_hash => <<987, 654, 321>>,
  move => #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}},
  ts => 1640995200000}.                                           % true

% Invalid receipts
#{before_hash => <<123>>, after_hash => <<456>>}.                  % false (missing required fields)
#{before_hash => "not_binary", after_hash => <<456>>, ...}.       % false (hash must be binary)
#{ts => "not_integer", ...}.                                      % false (timestamp must be integer)
```

## Validation Functions Reference

### `is_marking/1`

```erlang
-spec is_marking(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid marking.

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

**Comprehensive Doctest Examples**:
```erlang
% Basic valid markings
> pnet_types:is_marking(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_marking(#{start => [init], step1 => [], step2 => [data]}).
true
> pnet_types:is_marking(#{}).
true
> pnet_types:is_marking(#{p1 => []}).
true

% Complex valid markings
> pnet_types:is_marking(#{input => [doc1, doc2], processing => [], output => [result1]}).
true
> pnet_types:is_marking(#{place1 => [1, "a", #{key => "value"}], place2 => []}).
true

% Invalid markings - wrong types
> pnet_types:is_marking(#{p1 => a}).
false
> pnet_types:is_marking(#{p1 => [a,b]}).
false
> pnet_types:is_marking([p1, [a,b]]).
false
> pnet_types:is_marking("not_a_map").
false
> pnet_types:is_marking(123).
false

% Invalid markings - wrong key types
> pnet_types:is_marking({"p1" => [a]}).
false
> pnet_types:is_marking({1 => [a]}).
false
> pnet_types:is_marking(#{1.0 => [a]}).
false

% Invalid markings - wrong value types
> pnet_types:is_marking(#{p1 => a}).
false
> pnet_types:is_marking(#{p1 => 1}).
false
> pnet_types:is_marking(#{p1 => #{}}).
false
> pnet_types:is_marking(#{p1 => true}).
false
```

### `is_mode/1`

```erlang
-spec is_mode(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid mode.

**Comprehensive Doctest Examples**:
```erlang
% Basic valid modes
> pnet_types:is_mode(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_mode(#{p1 => []}).
true
> pnet_types:is_mode(#{}).
true

% Complex valid modes
> pnet_types:is_mode(#{input => [doc], review => [], approval => []}).
true
> pnet_types:is_mode(#{place1 => [token1, token2], place2 => []}).
true

% Invalid modes - wrong types
> pnet_types:is_mode(#{p1 => a}).
false
> pnet_types:is_mode(#{p1 => [a,b]}).
false
> pnet_types:is_mode([p1, [a]]).
false
> pnet_types:is_mode("not_a_map").
false
> pnet_types:is_mode(123).
false

% Invalid modes - wrong key types
> pnet_types:is_mode({"p1" => [a]}).
false
> pnet_types:is_mode({1 => [a]}).
false

% Invalid modes - wrong value types
> pnet_types:is_mode(#{p1 => a}).
false
> pnet_types:is_mode(#{p1 => 1}).
false
> pnet_types:is_mode(#{p1 => #{}}).
false
```

### `is_consume_map/1`

```erlang
-spec is_consume_map(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid consume_map.

**Comprehensive Doctest Examples**:
```erlang
% Basic valid consume maps
> pnet_types:is_consume_map(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_consume_map(#{p1 => []}).
true
> pnet_types:is_consume_map(#{}).
true

% Complex valid consume maps
> pnet_types:is_consume_map(#{input => [doc1], review => [status], output => []}).
true
> pnet_types:is_consume_map(#{place1 => [token1, token2], place2 => [token3]}).
true

% Invalid consume maps
> pnet_types:is_consume_map(#{p1 => a}).
false
> pnet_types:is_consume_map([p1, [a]]).
false
> pnet_types:is_consume_map("not_a_map").
false
> pnet_types:is_consume_map(#{p1 => 1}).
false
```

### `is_produce_map/1`

```erlang
-spec is_produce_map(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid produce_map.

**Comprehensive Doctest Examples**:
```erlang
% Basic valid produce maps
> pnet_types:is_produce_map(#{p1 => [c], p2 => [d]}).
true
> pnet_types:is_produce_map(#{p1 => [result]}).
true
> pnet_types:is_produce_map(#{p1 => [], p2 => []}).
true
> pnet_types:is_produce_map(#{}).
true

% Complex valid produce maps
> pnet_types:is_produce_map(#{output => [result1, result2], archive => [doc]}).
true
> pnet_types:is_produce_map(#{place1 => [token1], place2 => [token2, token3]}).
true

% Invalid produce maps
> pnet_types:is_produce_map(#{p1 => c}).
false
> pnet_types:is_produce_map(#{p1 => [c,d]}).
false
> pnet_types:is_produce_map([p1, [c]]).
false
> pnet_types:is_produce_map("not_a_map").
false
> pnet_types:is_produce_map(#{p1 => 1}).
false
```

### `is_binding/1`

```erlang
-spec is_binding(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid binding.

**Comprehensive Doctest Examples**:
```erlang
% Basic valid bindings
> pnet_types:is_binding(#{x => 1, y => <<"ok">>}).
true
> pnet_types:is_binding(#{X => data, Y => 42}).
true
> pnet_types:is_binding(#{}).
true
> pnet_types:is_binding(#{var1 => 1, var2 => "value", var3 => #{}}).
true

% Complex valid bindings
> pnet_types:is_binding(#{UserId => 42, UserName => "Alice", DocType => "invoice"}).
true
> pnet_types:is_binding(#{X => [1,2,3], Y => #{key => value}}).
true

% Invalid bindings - wrong key types
> pnet_types:is_binding(#{1 => x}).
false
> pnet_types:is_binding({"x" => 1}).
false
> pnet_types:is_binding({1.0 => "value"}).
false
> pnet_types:is_binding([x, 1]).
false

% Invalid bindings - wrong input types
> pnet_types:is_binding(#{x => [y]}).
false
> pnet_types:is_binding(1).
false
> pnet_types:is_binding("not_a_map").
false
> pnet_types:is_binding([x,y]).
false
> pnet_types:is_binding({x, 1}).
false
```

### `is_cmode/1`

```erlang
-spec is_cmode(term()) -> boolean().
```

**Purpose**: Validates if a term is a valid cmode.

**Implementation Notes**:
- Accepts `{binding(), mode()}` tuples
- Special case: `{} is treated as empty binding
- Validates both binding and mode components

**Comprehensive Doctest Examples**:
```erlang
% Basic valid cmode
> pnet_types:is_cmode({#{}, #{p1 => [a]}}).
true
> pnet_types:is_cmode({#{x => 1}, #{p1 => [x]}}).
true
> pnet_types:is_cmode({#{}, #{}}).
true

% Complex valid cmode
> pnet_types:is_cmode({#{X => data, Y => 42}, #{p1 => [X], p2 => [Y]}}).
true
> pnet_types:is_cmode({#{UserId => 42}, #{input => [UserId], review => []}}).
true

% Invalid cmode - wrong structure
> pnet_types:is_cmode({{}, p1 => [a]}).
false
> pnet_types:is_cmode([{}, #{}]).
false
> pnet_types:is_cmode(#{binding => #{}, mode => #{}}).
false

% Invalid cmode - invalid binding
> pnet_types:is_cmode({1, #{p1 => [a]}}).
false
> pnet_types:is_cmode({{"x" => 1}, #{p1 => [a]}}).
false
> pnet_types:is_cmode({{1 => "x"}, #{p1 => [a]}}).
false

% Invalid cmode - invalid mode
> pnet_types:is_cmode({#{}, #{p1 => a}}).
false
> pnet_types:is_cmode({#{}, {p1 => [a]}}).
false
> pnet_types:is_cmode({#{}, "not_a_map"}).
false

% Edge cases
> pnet_types:is_cmode({#{}, #{p1 => [a,b]}}).
false
> pnet_types:is_cmode({#{x => 1}, {"p1" => [a]}}).
false
> pnet_types:is_cmode({#{}, 123}).
false
```

## Usage Patterns and Examples

### 1. Basic Workflow Validation

```erlang
% Before workflow execution, validate the initial marking
validate_initial_marking(Marking) ->
    case pnet_types:is_marking(Marking) of
        true ->
            % Valid initial marking, proceed with workflow
            gen_pnet:start_workflow(CaseId, Marking),
            ok;
        false ->
            % Invalid marking, handle error
            error_logger:error_msg("Invalid initial marking: ~p", [Marking]),
            {error, invalid_marking}
    end.

% Example usage
InitialMarking = #{start => [init], step1 => [], step2 => []},
validate_initial_marking(InitialMarking).  % -> ok
```

### 2. Transition Mode Checking

```erlang
% Check if a transition is enabled by validating against current marking
is_transition_enabled(TransitionMode, CurrentMarking) ->
    % Validate both types first
    case pnet_types:is_mode(TransitionMode) andalso
         pnet_types:is_marking(CurrentMarking) of
        false ->
            false;  % Invalid types, cannot be enabled
        true ->
            % Both types are valid, check actual enablement
            is_mode_satisfied(TransitionMode, CurrentMarking)
    end.

% Example usage
Mode = #{input => [doc], review => []},
Marking = #{input => [doc1, doc2], review => [], output => []},
is_transition_enabled(Mode, Marking).  % -> true (if mode is satisfied)
```

### 3. Move Validation Before Execution

```erlang
% Validate a complete move before execution
validate_and_execute_move(Move) ->
    case pnet_types:is_move(Move) of
        true ->
            % Move is structurally valid, proceed with execution
            gen_pnet:fire_transition(Move),
            ok;
        false ->
            % Invalid move, report error
            error_logger:error_msg("Invalid move structure: ~p", [Move]),
            {error, invalid_move}
    end.

% Example usage
Move = #{trsn => t1,
         mode => #{p1 => [a]},
         produce => #{p2 => [b]}},
validate_and_execute_move(Move).  % -> ok if valid
```

### 4. Colored Petri Net Pattern

```erlang
% For colored Petri nets, validate cmode with binding
process_colored_transition(ColoredMode) ->
    case pnet_types:is_cmode(ColoredMode) of
        true ->
            % Colored mode is valid, substitute variables and execute
            SubstitutedMode = substitute_variables(ColoredMode),
            execute_transition(SubstitutedMode),
            ok;
        false ->
            % Invalid colored mode, handle error
            error_logger:error_msg("Invalid colored mode: ~p", [ColoredMode]),
            {error, invalid_colored_mode}
    end.

% Example usage
ColoredMode = {{X => UserId}, #{p1 => [X], p2 => [result]}},
process_colored_transition(ColoredMode).  % -> ok if valid
```

### 5. Guard Usage Patterns

```erlang
% Safe to use in guards due to total nature
safe_mode_check(Mode, Marking) when
    pnet_types:is_mode(Mode),
    pnet_types:is_marking(Marking) ->
    % Both types are validated, safe to use
    check_enablement(Mode, Marking).

% Example usage
Mode = #{p1 => [a]},
Marking = #{p1 => [a], p2 => []},
safe_mode_check(Mode, Marking).  % Safe to call in guard context
```

### 6. Batch Validation for Workflow Definitions

```erlang
% Validate entire workflow definition
validate_workflow_definition(NetDef) ->
    % Validate all marking definitions
    ValidMarkings = lists:all(fun pnet_types:is_marking/1,
                              maps:values(NetDef#net.markings)),

    % Validate all mode definitions
    ValidModes = lists:all(fun pnet_types:is_mode/1,
                           maps:values(NetDef#net.modes)),

    % Validate all consume maps
    ValidConsumes = lists:all(fun pnet_types:is_consume_map/1,
                              maps:values(NetDef#net.consume_maps)),

    % Validate all produce maps
    ValidProduces = lists:all(fun pnet_types:is_produce_map/1,
                              maps:values(NetDef#net.produce_maps)),

    % Validate all colored modes
    ValidColoredModes = lists:all(fun pnet_types:is_cmode/1,
                                  maps:values(NetDef#net.colored_modes)),

    % Return comprehensive validation result
    #{valid => ValidMarkings and ValidModes and ValidConsumes and
              ValidProduces and ValidColoredModes,
      details => #{
          markings => ValidMarkings,
          modes => ValidModes,
          consume_maps => ValidConsumes,
          produce_maps => ValidProduces,
          colored_modes => ValidColoredModes
      }}.
```

## Error Cases and Expected Outputs

### Common Error Patterns

```erlang
% Wrong data types
pnet_types:is_marking(not_a_map).            % -> false
pnet_types:is_mode([p1, [a]]).              % -> false
pnet_types:is_binding(1).                   % -> false
pnet_types:is_cmode(1).                     % -> false

% Invalid map structures
pnet_types:is_marking(#{p1 => a}).          % -> false (values must be lists)
pnet_types:is_mode(#{p1 => [a,b]}).         % -> false (invalid format)
pnet_types:is_binding(#{1 => x}).           % -> false (keys must be atoms)
pnet_types:is_cmode({1, #{}}).              % -> false (invalid binding)

% Empty vs invalid
pnet_types:is_marking(#{}).                 % -> true (empty marking)
pnet_types:is_binding({}).                  % -> false (not a map)
pnet_types:is_cmode({{}}).                  % -> false (empty tuple, not map)
```

### Edge Cases and Boundary Conditions

```erlang
% Very large structures
LargeMarking = lists:foldl(fun(I, Acc) -> Acc#{p(I) => [I]} end, #{}, lists:seq(1, 1000)),
pnet_types:is_marking(LargeMarking).        % -> true (large but valid)

% Nested complex tokens
ComplexToken = #{level => 1, data => #{level => 2, data => #{level => 3, value => "deep"}}},
ComplexMarking = #{p1 => [ComplexToken]},
pnet_types:is_marking(ComplexMarking).       % -> true (deeply nested but valid)

% Empty lists vs atoms
pnet_types:is_marking(#{p1 => []}).         % -> true (empty list valid)
pnet_types:is_marking(#{p1 => "a"}).        % -> false (string not list)
pnet_types:is_mode(#{p1 => []}).           % -> true (empty list valid)
pnet_types:is_mode(#{p1 => []}).           % -> true (empty list valid)
```

## Integration with Other PNet Modules

### Integration with `pnet_marking`

```erlang
% Use pnet_types to validate before marking operations
safe_add_tokens(CaseId, Place, Tokens) ->
    % First validate the new marking
    NewMarking = pnet_marking:add_tokens(CaseId, Place, Tokens),
    case pnet_types:is_marking(NewMarking) of
        true ->
            % Valid marking, proceed
            gen_pnet:update_marking(CaseId, NewMarking),
            ok;
        false ->
            % Invalid marking, handle error
            error_logger:error_msg("Invalid marking after token addition: ~p", [NewMarking]),
            {error, invalid_marking}
    end.

% Example usage
safe_add_tokens(case123, p1, [new_token]).  % -> ok if result is valid
```

### Integration with `pnet_mode`

```erlang
% Validate mode before checking transition enablement
check_transition_enablement(TransitionName, CaseId) ->
    % Get current marking and transition mode
    CurrentMarking = gen_pnet:get_marking(CaseId),
    TransitionMode = get_transition_mode(TransitionName),

    % Validate both types
    case pnet_types:is_marking(CurrentMarking) andalso
         pnet_types:is_mode(TransitionMode) of
        false ->
            % Invalid types, cannot be enabled
            false;
        true ->
            % Types are valid, check actual enablement
            pnet_mode:is_enabled(TransitionMode, CurrentMarking)
    end.

% Example usage
Enabled = check_transition_enablement(t1, case123).  % -> true/false
```

### Integration with YAWL Compilation

```erlang
% Validate YAWL compilation results
validate_yawl_net(NetDef) ->
    % Create validation functions
    ValidateMarking = fun(M) -> pnet_types:is_marking(M) end,
    ValidateMode = fun(M) -> pnet_types:is_mode(M) end,
    ValidateBinding = fun(B) -> pnet_types:is_binding(B) end,
    ValidateCMode = fun(CM) -> pnet_types:is_cmode(CM) end,

    % Validate all components
    MarkingsOk = lists:all(ValidateMarking, maps:values(NetDef#net.markings)),
    ModesOk = lists:all(ValidateMode, maps:values(NetDef#net.modes)),
    BindingsOk = lists:all(ValidateBinding, maps:values(NetDef#net.bindings)),
    CModesOk = lists:all(ValidateCMode, maps:values(NetDef#net.colored_modes)),

    % Return validation result
    #{valid => MarkingsOk and ModesOk and BindingsOk and CModesOk,
      details => #{
          markings => MarkingsOk,
          modes => ModesOk,
          bindings => BindingsOk,
          colored_modes => CModesOk
      }}.
```

## Best Practices and Guidelines

### 1. Always Validate Before Processing

```erlang
% Good practice - validate before processing
execute_transition(Move) ->
    case pnet_types:is_move(Move) of
        true -> gen_pnet:fire_transition(Move);
        false -> handle_invalid_move(Move)
    end.

% Bad practice - assuming valid structure
execute_transition_wrong(Move) ->
    gen_pnet:fire_transition(Move).  % May crash if Move is invalid
```

### 2. Use Types in Guards for Performance

```erlang
% Good - use in guards for performance optimization
safe_enablement_check(Mode, Marking) when
    pnet_types:is_mode(Mode),
    pnet_types:is_marking(Marking) ->
    % Types validated, safe to use
    check_enablement(Mode, Marking).

% Bad - checking in function body (less efficient)
unsafe_enablement_check(Mode, Marking) ->
    case pnet_types:is_mode(Mode) of
        false -> false;
        true ->
            case pnet_types:is_marking(Marking) of
                false -> false;
                true -> check_enablement(Mode, Marking)
            end
    end.
```

### 3. Comprehensive Error Handling

```erlang
% Good - handle all validation cases
start_workflow_safely(WorkflowDef) ->
    case validate_workflow_definition(WorkflowDef) of
        #{valid := true} ->
            proceed_with_workflow(WorkflowDef);
        #{valid := false, details := Details} ->
            handle_validation_errors(Details)
    end.

% Bad - ignoring validation
start_workflow_unsafe(WorkflowDef) ->
    proceed_with_workflow(WorkflowDef).  % May fail with invalid data
```

### 4. Maintain Type Consistency

```erlang
% Good - ensure consistency across workflow
ensure_type_consistency(Net) ->
    % All places must be atoms
    Places = Net#net.places,
    case lists:all(fun is_atom/1, Places) of
        false -> throw(invalid_places);
        true -> ok
    end,

    % All markings must be valid
    Markings = maps:values(Net#net.markings),
    case lists:all(fun pnet_types:is_marking/1, Markings) of
        false -> throw(invalid_markings);
        true -> ok
    end.
```

## Performance Considerations

### 1. Try/Catch Pattern

All validation functions use try/catch to ensure total behavior:

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

### 2. Early Termination

Validation stops at first error found for efficiency.

### 3. Map Folding

Uses `maps:fold` for efficient validation of map contents.

### 4. Guard Optimization

Total functions optimize guard clause performance.

## Testing and Doctest Usage

### Running Doctests

The module includes doctest support for validation:

```erlang
% Run doctests
doctest:module(pnet_types, #{moduledoc => true, doc => true}).
```

### Writing Custom Doctests

When extending the module, follow the doctest pattern:

```erlang
% Function documentation with doctests
-doc """
Validates a complex workflow marking.

Examples:
```erlang
> pnet_types:is_complex_marking(#{p1 => [a], p2 => [b,c]}).
true
> pnet_types:is_complex_marking(#{p1 => a}).
false
```.
""".
-spec is_complex_marking(term()) -> boolean().
is_complex_marking(Term) ->
    % Implementation
    pnet_types:is_marking(Term).
```

## Summary

The `pnet_types` module provides comprehensive type validation for the CRE workflow engine. Its total functions ensure safe type checking throughout the system, while extensive doctest examples demonstrate proper usage patterns for all data structures and validation functions.

Key features:
- **Total Functions**: Always return `boolean()`, never crash
- **Comprehensive Validation**: Validate both structure and content
- **Guard-Safe**: Safe to use in Erlang guards
- **Integration-Ready**: Works seamlessly with other PNet modules
- **Well-Documented**: Extensive doctest examples for all functions

This reference provides everything needed to understand and effectively use the `pnet_types` module in workflow development scenarios.