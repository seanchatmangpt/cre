# Petri Net Mode Enumeration - Quick Reference

## Overview

The `pnet_mode` module enumerates valid firing modes for Petri net transitions, supporting both basic and colored Petri nets.

## Core Functions

### preset_counts/1
```erlang
> pnet_mode:preset_counts([p, p, q]).
#{p => 2, q => 1}
```
Count token requirements from preset places (handles multiplicity).

### enum_modes/2
```erlang
> Marking = #{p => [a,b], q => [x,y]}.
> pnet_mode:enum_modes([p,q], Marking).
[#{p => [a], q => [x]},
 #{p => [a], q => [y]},
 #{p => [b], q => [x]},
 #{p => [b], q => [y]}]
```
Enumerate all valid modes given current marking.

### enum_cmodes/4
```erlang
> pnet_mode:enum_cmodes(t1, #{p => [a,b]}, ctx, my_net).
[{#{}, #{p => [a]}},
 {#{}, #{p => [b]}}]
```
Enumerate colored modes with variable bindings, falls back to basic if needed.

## Key Concepts

### Mode
A map specifying which tokens to consume from each input place:
```erlang
#{place1 => [token1, token2], place2 => [token3]}
```

### Colored Mode (cmode)
Tuple with variable binding and token mode:
```erlang
{#{var1 => val1}, #{place1 => [token1]}}
```

### Multiplicity Handling
Places can appear multiple times in preset:
- `[p, p, q]` needs 2 tokens from p, 1 from q
- Each combination represents a separate firing mode

## Examples

### Basic Enumeration
```erlang
> % Simple case
> pnet_mode:enum_modes([p], #{p => [a,b,c]}).
[#{p => [a]}, #{p => [b]}, #{p => [c]}]

> % Cartesian product
> pnet_mode:enum_modes([p,q], #{p => [a,b], q => [1,2]}).
[#{p => [a], q => [1]}, #{p => [a], q => [2]},
 #{p => [b], q => [1]}, #{p => [b], q => [2]}]

> % Multiplicity
> pnet_mode:enum_modes([p,p,q], #{p => [a,b], q => [x]}).
[#{p => [a,b], q => [x]}]
```

### Colored Net Enumeration
```erlang
> % Colored with binding
> pnet_mode:enum_cmodes(auth, #{user => [alice,bob]}, #{}, auth_net).
[{#{user => alice}, #{user => [alice]}},
 {#{user => bob}, #{user => [bob]}}]

> % Empty binding fallback
> pnet_mode:enum_cmodes(t1, #{p => [a,b]}, ctx, basic_net).
[{#{}, #{p => [a]}}, {#{}, #{p => [b]}}]
```

## Error Cases

```erlang
> % Insufficient tokens
> pnet_mode:enum_modes([p,p,p], #{p => [a,b]}).
[]  % Need 3, only 2 available

> % Empty marking
> pnet_mode:enum_modes([p], #{p => []}).
[]  % No tokens available
```

## Performance

- Time: O(Π(C_i choose k_i)) where C_i = tokens in place i, k_i = required count
- Space: Grows exponentially with number of places/tokens
- Always deterministic ordering (term order)
- Early termination if insufficient tokens anywhere

## Integration

Used by `gen_pnet` for transition firing:
1. Enumerate modes with `enum_modes/2` or `enum_cmodes/4`
2. Select mode (typically first/only)
3. Fire transition with selected mode
4. Consume/produce tokens according to rules

## Related Modules

- `pnet_types` - Type definitions and validation
- `pnet_marking` - Multiset marking algebra
- `gen_pnet` - Main Petri net runner