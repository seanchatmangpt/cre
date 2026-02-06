# PNet Types Quick Reference

## Core Types

| Type | Definition | Purpose | Example |
|------|------------|---------|---------|
| `place()` | `atom()` | Node identifier | `p1`, `start`, `end` |
| `trsn()` | `atom()` | Transition identifier | `t1`, `approve`, `process` |
| `token()` | `term()` | Any token value | `1`, `"data"`, `#{}` |
| `marking()` | `#{place() => [token()]}` | Current net state | `#{p1 => [a], p2 => []}` |
| `mode()` | `#{place() => [token()]}` | Transition enablement | `#{p1 => [a], p2 => [b]}` |
| `consume_map()` | `#{place() => [token()]}` | Tokens to consume | `#{p1 => [a], p2 => [b]}` |
| `produce_map()` | `#{place() => [token()]}` | Tokens to produce | `#{p1 => [c], p2 => [d]}` |
| `var()` | `atom()` | Variable name | `X`, `Y`, `UserId` |
| `binding()` | `#{var() => term()}` | Variable binding | `#{X => 1, Y => data}` |
| `cmode()` | `{binding(), mode()}` | Colored mode | `{{X => 1}, #{p1 => [X]}}` |
| `move()` | `#{trsn := trsn(), mode := mode()\|cmode(), produce := produce_map()}` | Transition firing | `#{trsn => t1, mode => #{}, produce => #{}}` |
| `receipt()` | `#{before_hash := binary(), after_hash := binary(), move := move(), ts := integer()}` | Execution record | `#{before_hash => <<...>>, move => #{...}, ts => 123456}` |

## Validation Functions

All functions return `boolean()` and never crash.

### Basic Validation

```erlang
is_marking(Term)      % Validate marking maps
is_mode(Term)         % Validate mode maps
is_consume_map(Term)  % Validate consume maps
is_produce_map(Term)  % Validate produce maps
is_binding(Term)      % Validate variable bindings
is_cmode(Term)        % Validate colored modes
```

### Quick Examples

#### Marking Validation
```erlang
> pnet_types:is_marking(#{p1 => [a], p2 => []}).
true
> pnet_types:is_marking(#{p1 => a}).
false
```

#### Mode Validation
```erlang
> pnet_types:is_mode(#{p1 => [a], p2 => [b]}).
true
> pnet_types:is_mode(#{p1 => a}).
false
```

#### Binding Validation
```erlang
> pnet_types:is_binding(#{x => 1, y => <<"ok">>}).
true
> pnet_types:is_binding(#{1 => x}).
false
```

#### Colored Mode Validation
```erlang
> pnet_types:is_cmode({#{x => 1}, #{p1 => [a]}}).
true
> pnet_types:is_cmode({#{}, #{p1 => a}}).
false
```

## Common Patterns

### Validate Before Processing
```erlang
case pnet_types:is_marking(Marking) of
    true -> process_marking(Marking);
    false -> handle_error()
end.
```

### Guard Usage (Safe)
```erlang
validate_transition(Mode, Marking) when
    pnet_types:is_mode(Mode),
    pnet_types:is_marking(Marking) ->
    % Safe to use validated types
    check_enablement(Mode, Marking).
```

### Colored Petri Net Pattern
```erlang
ColoredMode = {{X => UserId}, #{p1 => [X], p2 => [result]}},
case pnet_types:is_cmode(ColoredMode) of
    true -> substitute_and_execute(ColoredMode);
    false -> error(invalid_colored_mode)
end.
```

## Error Cases

| Function | Invalid Input | Result |
|----------|---------------|--------|
| `is_marking/1` | `#{p1 => a}` | `false` |
| `is_mode/1` | `#{p1 => [a,b]}` | `false` |
| `is_binding/1` | `#{1 => x}` | `false` |
| `is_cmode/1` | `{1, #{}}` | `false` |

## Key Points

1. **Total Functions**: Always return `boolean()`, never crash
2. **Map Structure**: All map types require:
   - Keys: Atoms (places or variables)
   - Values: Lists of tokens (except binding)
3. **Colored Types**: `cmode/0` is a 2-tuple `{binding(), mode()}`
4. **Empty Values**: Empty lists `[]` are valid (no tokens)
5. **Guard Safe**: Safe to use in Erlang guards

## Comprehensive Documentation

For extensive doctest examples, detailed usage patterns, and advanced scenarios, see [PNet Types Doctest Reference](./PNET_TYPES_DOCTEST_REFERENCE.md).