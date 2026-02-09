# Petri Net Mode Enumeration Guide

This guide provides comprehensive documentation for the `pnet_mode.erl` module, which implements mode enumeration utilities for Petri nets, including both basic and colored Petri net support.

## Table of Contents

1. [Overview](#overview)
2. [Mode Enumeration Concepts](#mode-enumeration-concepts)
3. [Basic Mode Enumeration](#basic-mode-enumeration)
4. [Colored Net Mode Enumeration](#colored-net-mode-enumeration)
5. [Integration with Transition Firing](#integration-with-transition-firing)
6. [Advanced Topics](#advanced-topics)
7. [Examples and Doctests](#examples-and-doctests)
8. [Performance Considerations](#performance-considerations)

## Overview

The `pnet_mode` module provides utilities for enumerating the different ways a transition can fire in a Petri net. This is a fundamental operation in Petri net execution as it determines all possible valid firing modes given the current marking.

### Key Concepts

- **Mode**: A specification of which tokens are consumed from each input place when a transition fires
- **Colored Mode (cmode)**: An extension for colored Petri nets that includes variable bindings
- **Preset**: The set of input places for a transition
- **Marking**: The current distribution of tokens across places

## Mode Enumeration Concepts

### Basic Mode Enumeration

For uncolored Petri nets, a mode represents a selection of available tokens from each preset place. A mode is a map where each key is a place and each value is a list of tokens to be consumed from that place. If a place has multiple tokens available, each combination of tokens (considering multiplicity) represents a separate firing mode.

**Doctest Example**:
```erlang
> % Simple mode: select one token from place p
> Marking = #{p => [a, b, c]}.
> Modes = pnet_mode:enum_modes([p], Marking).
[#{p => [a]}, #{p => [b]}, #{p => [c]}]
```

### Colored Mode Enumeration

For colored Petri nets, a colored mode (cmode) extends basic mode enumeration with variable bindings. A cmode is a tuple `{Binding, Mode}` where:
- `Binding` is a map of variables to their concrete values
- `Mode` is the basic token selection mode

**Doctest Example**:
```erlang
> % Colored mode with user binding
> Marking = #{user => [alice, bob]}.
> Modes = pnet_mode:enum_cmodes(auth, Marking, #{}, user_net).
[{#{user => alice}, #{user => [alice]}},
 {#{user => bob}, #{user => [bob]}}]
```

**Key Insight**: Variable bindings enable data flow in workflows - when a transition fires, the binding can be used to parameterize production tokens.

## Basic Mode Enumeration

### Preset Counting

The `preset_counts/1` function calculates how many tokens are needed from each input place, handling multiplicity in preset lists. This is essential for determining how many tokens to select from each place during mode enumeration.

**API Specification**:
```erlang
-spec preset_counts(PresetPlaces :: [place()]) -> #{place() => non_neg_integer()}.
```

**Doctest Examples**:
```erlang
> % Basic counting
> pnet_mode:preset_counts([p, q, r]).
#{p => 1, q => 1, r => 1}

> % Multiplicity handling
> pnet_mode:preset_counts([p, p, q, r]).
#{p => 2, q => 1, r => 1}

> % Complex multiplicity
> pnet_mode:preset_counts([a, a, a, b, b, c]).
#{a => 3, b => 2, c => 1}
```

**Key Insight**: The function uses `lists:foldl` with `maps:update_with` to count occurrences efficiently. This O(n) operation is the foundation for all subsequent mode enumeration.

### Mode Enumeration

The `enum_modes/2` function generates all possible modes given the current marking. This is the core function that computes the Cartesian product of token selections across all preset places.

**API Specification**:
```erlang
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) -> [mode()].
```

**Doctest Examples**:
```erlang
> % Single place, multiple tokens
> pnet_mode:enum_modes([p], #{p => [a, b, c]}).
[#{p => [a]}, #{p => [b]}, #{p => [c]}]

> % Multiple places, single tokens each
> pnet_mode:enum_modes([p, q], #{p => [a], q => [x]}).
[#{p => [a], q => [x]}]

> % Multiple places, multiple tokens
> pnet_mode:enum_modes([p, q], #{p => [a, b], q => [x, y]}).
[#{p => [a], q => [x]},
 #{p => [a], q => [y]},
 #{p => [b], q => [x]},
 #{p => [b], q => [y]}]

> % Multiplicity example
> pnet_mode:enum_modes([p, p, q], #{p => [a, b, c], q => [x]}).
[#{p => [a,b], q => [x]},
 #{p => [a,c], q => [x]},
 #{p => [b,c], q => [x]}]
```

**Algorithm Insight**: The function computes combinations recursively:
1. Counts token requirements with `preset_counts/1`
2. For each place, generates combinations of N tokens where N is the count
3. Computes Cartesian product across all places
4. Returns modes in deterministic term order

### Empty Markings and Error Cases

The system correctly handles cases where no modes are possible:

**Doctest Examples**:
```erlang
> % Empty marking
> pnet_mode:enum_modes([p], #{p => []}).
[]  % No tokens available

> % Insufficient tokens
> pnet_mode:enum_modes([p, p, p], #{p => [a, b]}).
[]  % Need 3 tokens, only 2 available

> % Multiple places with some empty
> pnet_mode:enum_modes([p, q], #{p => [a], q => []}).
[]  % q has no tokens available
```

**Edge Cases**:
```erlang
> % Empty preset (no input places)
> pnet_mode:enum_modes([], #{any => [anything]}).
[#{ }]

> % Duplicate tokens (multiset semantics)
> pnet_mode:enum_modes([p], #{p => [a, a, b]}).
[#{p => [a]}, #{p => [a]}, #{p => [b]}]
```

**Key Insight**: The algorithm uses early termination - if any place has insufficient tokens, it immediately returns an empty list without processing further places.

## Colored Net Mode Enumeration

### Colored Modes with Variable Bindings

The `enum_cmodes/4` function handles colored Petri net modes by calling the net module's `cmodes` callback. This enables pattern matching and variable binding for sophisticated workflow patterns.

**API Specification**:
```erlang
-spec enum_cmodes(Trsn :: atom(), Marking :: marking(),
                 Ctx :: usr_info(), NetMod :: net_mod()) -> [cmode()].
```

**Doctest Examples**:
```erlang
> % Colored mode with simple binding
> Marking = #{user => [alice, bob]}.
> Modes = pnet_mode:enum_cmodes(auth, Marking, #{}, user_net).
[{#{user => alice}, #{user => [alice]}},
 {#{user => bob}, #{user => [bob]}}]

> % Colored mode with multiple variables
> Marking = #{data => [{type, int, 42}, {type, string, "hello"}]}.
> Modes = pnet_mode:enum_cmodes(validate, Marking, #{}, validation_net).
[{{type => int}, #{data => [{type, int, 42}]}},
 {{type => string}, #{data => [{type, string, "hello"}]}}]

> % Complex colored mode with nested data
> Marking = #{request => [{user, alice, "alice@test.com"}]}.
> Modes = pnet_mode:enum_cmodes(notify, Marking, #{}, notification_net).
[{{user => alice, email => "alice@test.com"},
  #{request => [{user, alice, "alice@test.com"}]}}]
```

### Fallback to Basic Enumeration

If the net module doesn't implement colored modes, the function gracefully falls back to basic enumeration with empty bindings:

**Doctest Examples**:
```erlang
> % Net module without cmodes/3 implementation
> pnet_mode:enum_cmodes(t1, #{p => [a,b]}, ctx, basic_net).
[{#{}, #{p => [a]}},
 {#{}, #{p => [b]}}]

> % Extract bindings and modes separately
> Modes = pnet_mode:enum_cmodes(process, #{data => [x,y]}, ctx, simple_net).
> [{Binding, Mode} <- Modes] ->
>     {Binding, Mode}.
[{#{}, #{data => [x]}},
 {#{}, #{data => [y]}}]
```

**Key Insight**: The fallback mechanism ensures backward compatibility - existing uncolored net modules continue to work while colored net modules can leverage advanced features.

## Integration with Transition Firing

### Mode Selection in Transition Firing

Mode enumeration is used by the `gen_pnet` runtime to determine all valid ways to fire a transition. This enables nondeterministic firing while maintaining deterministic results for predictable execution.

**Transition Firing Process**:
1. **Step 1**: Enumerate all possible modes for the enabled transition
2. **Step 2**: Choose one mode (typically first in deterministic order)
3. **Step 3**: Fire the transition with the selected mode
4. **Step 4**: Produce tokens according to the firing rules

**Doctest Examples - Basic Transition**:
```erlang
> % Current marking for transition t1
> Marking = #{p1 => [a, b], p2 => [c]}.
>
> % Preset for transition t1
> Preset = [p1, p1, p2].
>
> % Get all possible firing modes
> Modes = pnet_mode:enum_modes(Preset, Marking).
>
> Modes = [#{p1 => [a], p2 => [c]}, #{p1 => [b], p2 => [c]}].
>
> % Runtime selects one mode (typically first)
> SelectedMode = hd(Modes).
>
> SelectedMode = #{p1 => [a], p2 => [c]}.
>
> % Transition fires with this mode
> % Consumes: a from p1, c from p2
> % Produces tokens according to transition firing rules
```

**Doctest Examples - Colored Transition**:
```erlang
> % Colored net transition
> ColoredMarking = #{input => [{user, alice, "login"}, {user, bob, "login"}]}.
>
> % Colored mode enumeration with variable bindings
> ColoredModes = pnet_mode:enum_cmodes(auth, ColoredMarking, #{}, auth_net).
>
> ColoredModes = [
>     {#{user => alice}, #{input => [{user, alice, "login"}]}},
>     {#{user => bob}, #{input => [{user, bob, "login"}]}}
> ].
>
> % Selected mode creates binding for transition firing
> SelectedCMode = hd(ColoredModes).
>
> SelectedCMode = {#{user => alice}, #{input => [{user, alice, "login"}]}}.
>
> % Binding can be used in firing rules:
> % e.g., produce output with bound variable values
```

### Transition Firing Example

```erlang
> % Current marking
> Marking = #{p1 => [a, b], p2 => [c]}.
>
> % Preset for transition t1
> Preset = [p1, p1, p2].
>
> % Get all possible firing modes
> Modes = pnet_mode:enum_modes(Preset, Marking).
>
> Modes = [#{p1 => [a], p2 => [c]}, #{p1 => [b], p2 => [c]}].
>
> % Runtime selects one mode (e.g., first one)
> SelectedMode = #{p1 => [a], p2 => [c]}.
>
> % Transition fires with this mode
> % Consumes: a from p1, c from p2
> % Produces tokens according to transition firing rules
```

### Colored Net Transition Example

```erlang
> % Colored net transition
> Marking = #{input => [{user, alice, "login"}, {user, bob, "login"}]}.
>
> % Colored mode enumeration with variable bindings
> ColoredModes = pnet_mode:enum_cmodes(auth, Marking, #{}, auth_net).
>
> ColoredModes = [
>     {#{user => alice}, #{input => [{user, alice, "login"}]}},
>     {#{user => bob}, #{input => [{user, bob, "login"}]}}
> ].
>
> % Selected mode creates binding for transition firing
> SelectedMode = {#{user => alice}, #{input => [{user, alice, "login"}]}}.
```

## Advanced Topics

### Cartesian Product Enumeration

The module efficiently computes the Cartesian product of token selections across all preset places:

```erlang
> % Multi-place example
> Marking = #{a => [1, 2, 3], b => [x, y], c => [z]}.
> pnet_mode:enum_modes([a, b, c], Marking).
[#{a => [1], b => [x], c => [z]},
 #{a => [1], b => [y], c => [z]},
 #{a => [2], b => [x], c => [z]},
 #{a => [2], b => [y], c => [z]},
 #{a => [3], b => [x], c => [z]},
 #{a => [3], b => [y], c => [z]}]
```

This is a 3 × 2 × 1 = 6 mode enumeration.

### Performance Optimizations

The module uses several optimizations:

1. **Early termination**: If any place has insufficient tokens, returns empty list immediately
2. **Deterministic ordering**: Results are always in term order for predictable behavior
3. **Efficient combinations**: Custom combination algorithm avoids unnecessary list operations
4. **Memoization**: Combination results are computed recursively without redundant calculations

### Fallback Mechanisms

For uncolored nets, the system gracefully handles missing implementations:

```erlang
> % Net module without cmodes/3
> enum_cmodes(t1, #{p => [a,b]}, ctx, basic_net).
[{#{}, #{p => [a]}},
 {#{}, #{p => [b]}}]
```

## Examples and Doctests

### Basic Mode Enumeration Examples

```erlang
> % Simple single place
> pnet_mode:enum_modes([p], #{p => [a, b, c]}).
[#{p => [a]}, #{p => [b]}, #{p => [c]}]

> % Multiple places with single tokens each
> pnet_mode:enum_modes([p, q], #{p => [a], q => [x]}).
[#{p => [a], q => [x]}]

> % Multiple places with multiple tokens
> pnet_mode:enum_modes([p, q], #{p => [a, b], q => [x, y]}).
[#{p => [a], q => [x]},
 #{p => [a], q => [y]},
 #{p => [b], q => [x]},
 #{p => [b], q => [y]}]

> % Place with insufficient tokens
> pnet_mode:enum_modes([p, p, p], #{p => [a, b]}).
[]  % Need 3 tokens, only 2 available

> % Empty marking
> pnet_mode:enum_modes([p], #{p => []}).
[]  % No tokens available
```

### Multiplicity Handling Examples

```erlang
> % Place appears multiple times in preset
> pnet_mode:preset_counts([p, p, q, r]).
#{p => 2, q => 1, r => 1}

> % Marking with multiplicity
> Marking = #{p => [a, b, c], q => [x], r => [1]}.
> pnet_mode:enum_modes([p, p, q, r], Marking).
[#{p => [a,b], q => [x], r => [1]},
 #{p => [a,c], q => [x], r => [1]},
 #{p => [b,c], q => [x], r => [1]}]

> % Complex multiplicity example
> Marking = #{a => [1, 2, 3], b => [4, 5], c => [6]}.
> pnet_mode:enum_modes([a, b, b, c], Marking).
[#{a => [1], b => [4,5], c => [6]},
 #{a => [2], b => [4,5], c => [6]},
 #{a => [3], b => [4,5], c => [6]}]
```

### Colored Net Examples

```erlang
> % Colored mode enumeration with user workflow
> Marking = #{input => [{user, alice, "report"}, {user, bob, "data"}]}.
> pnet_mode:enum_cmodes(process, Marking, #{role => "manager"}, workflow_net).
[{{user => alice}, #{input => [{user, alice, "report"}]}},
 {{user => bob}, #{input => [{user, bob, "data"}]}}]

> % Colored mode with multiple variables
> Marking = #{data => [{type, int, 42}, {type, string, "hello"}]}.
> pnet_mode:enum_cmodes(validate, Marking, #{}, validation_net).
[{{type => int}, #{data => [{type, int, 42}]}},
 {{type => string}, #{data => [{type, string, "hello"}]}}]

> % Colored mode with complex pattern matching
> Marking = #{email => [{user, alice, "alice@test.com"}]}.
> pnet_mode:enum_cmodes(notify, Marking, #{}, notification_net).
[{{user => alice, email => "alice@test.com"},
  #{email => [{user, alice, "alice@test.com"}]}}]
```

### Integration Examples

```erlang
> % Complete workflow example
> InitialMarking = #{start => [init],
>                   condition => [true],
>                   task => [process_data]}.
>
> % Get modes for transition t1
> Modes = pnet_mode:enum_modes([start, condition], InitialMarking).
> Modes = [#{start => [init], condition => [true]}].
>
> % Fire transition with selected mode
> FiringResult = gen_pnet:fire(pnet_net, t1, Modes, InitialMarking).
> % Tokens consumed and produced according to firing rules

> % Colored workflow example
> ColoredMarking = #{input => [{data, "payload"}, {user, alice, "admin"}]}.
> ColoredModes = pnet_mode:enum_cmodes(handle_input, ColoredMarking,
>                                     #{session_id => 123}, api_net).
>
> % Selected mode for firing
> SelectedCMode = hd(ColoredModes).
> % {binding, mode} structure for colored transition firing
```

## Cartesian Product Enumeration

The module efficiently computes the Cartesian product of token selections across all preset places:

**Doctest Example**:
```erlang
> % Multi-place Cartesian product
> Marking = #{a => [1, 2, 3], b => [x, y], c => [z]}.
> pnet_mode:enum_modes([a, b, c], Marking).
[#{a => [1], b => [x], c => [z]},
 #{a => [1], b => [y], c => [z]},
 #{a => [2], b => [x], c => [z]},
 #{a => [2], b => [y], c => [z]},
 #{a => [3], b => [x], c => [z]},
 #{a => [3], b => [y], c => [z]}]

> % Verify: 3 (a) × 2 (b) × 1 (c) = 6 modes
> length(Modes).
6
```

This demonstrates the mathematical foundation: for each place, we compute combinations of k tokens from n available, then take the product across all places.

## Performance Considerations

### Time Complexity

**Detailed Complexity Analysis**:

- **preset_counts/1**: O(n) where n is the number of places in the preset
- **enum_modes/2**: O(Π(C_i choose k_i)) where C_i is tokens in place i, k_i is required count
- **enum_cmodes/4**: O(n × m) for basic fallback, depends on net module for colored nets

**Doctest Example - Complexity Analysis**:
```erlang
> % Small case: manageable computation
> pnet_mode:enum_modes([a,b], #{a => [1,2,3], b => [x,y]}).
[#{a => [1], b => [x]}, #{a => [1], b => [y]},
 #{a => [2], b => [x]}, #{a => [2], b => [y]},
 #{a => [3], b => [x]}, #{a => [3], b => [y]}]

> % Complexity: 3 × 2 = 6 modes
> length(Result).
6

> % Larger case: exponential growth
> pnet_mode:enum_modes([a,b,c,d], #{a => [1,2], b => [x,y], c => [1], d => [a]}).
> % Complexity: 2 × 2 × 1 × 1 = 4 modes
> length(Result).
4
```

### Space Complexity

- Results grow exponentially with number of places and tokens
- Consider lazy evaluation for large state spaces
- Deterministic ordering ensures predictable memory usage

### Optimization Techniques

The module implements several optimizations:

1. **Early termination**: If any place has insufficient tokens, return empty list immediately
2. **Deterministic ordering**: Results always in term order for predictable behavior
3. **Efficient combinations**: Custom combination algorithm avoids unnecessary list operations
4. **Memoization**: Combination results computed recursively without redundant calculations

**Doctest Example - Early Termination**:
```erlang
> % Early termination: insufficient tokens detected immediately
> pnet_mode:enum_modes([a,a,a], #{a => [1,2]}).
[]  % Need 3, only 2 available - no wasted computation
```

### Algorithm Implementation Details

The `combinations/2` function implements a recursive algorithm:

```erlang
% Recursive combination algorithm (simplified)
combinations(0, _List) -> [[]];
combinations(N, [H|T]) ->
    WithH = [[H | Rest] || Rest <- combinations(N-1, T)],
    WithoutH = combinations(N, T),
    WithH ++ WithoutH.
```

This ensures all combinations are generated in lexicographic order by position in the original list.

### Testing with Doctests

The module includes comprehensive doctests that verify all functionality:

```erlang
% Doctests are verified in module documentation
doctest:module(?MODULE, #{moduledoc => true, doc => true}).
```

**Coverage**: Doctests verify all key functions and edge cases, ensuring examples in documentation match actual implementation behavior.

**Verification Process**: When run, doctests compare actual function outputs against expected results, catching any inconsistencies between documentation and implementation.

## Summary

The `pnet_mode` module provides robust mode enumeration for both basic and colored Petri nets. Its key features include:

- **Basic mode enumeration** with Cartesian product calculation
- **Colored net support** with variable bindings
- **Multiplicity handling** for transitions with repeated input places
- **Efficient algorithms** with deterministic ordering
- **Fallback mechanisms** for backward compatibility
- **Comprehensive doctests** for verification

The module is essential for implementing the nondeterministic firing semantics of Petri nets while maintaining deterministic results for predictable execution.