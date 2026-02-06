# PNet Mode Doctest Reference

This document provides comprehensive documentation for the `pnet_mode` module, including all doctest examples and usage patterns from the source code.

## Overview

The `pnet_mode` module provides utilities for mode enumeration in Petri nets, supporting both basic and colored Petri nets. It implements the mathematical foundations of transition enablement through mode enumeration.

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

## Core Functions

### `preset_counts/1`

Counts tokens at preset places for marking analysis.

```erlang
-spec preset_counts([place()]) -> #{place() => non_neg_integer()}.
```

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

Enumerates all basic modes for transition enablement.

```erlang
-spec enum_modes([place()], marking()) -> [mode()].
```

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

Enumerates colored modes with variable bindings.

```erlang
-spec enum_cmodes(trsn(), marking(), term(), module()) -> [cmode()].
```

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

## Advanced Topics

### Cartesian Product Enumeration

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

### Colored Net Theory

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

### Binding Analysis

```erlang
% Analyze variable bindings in colored nets
analyze_bindings(Trsn, Marking, Context) ->
    Cmodes = pnet_mode:enum_cmodes(Trsn, Marking, Context, get_net_module()),

    io:format("Colored modes for ~p:~n", [Trsn]),
    lists:foreach(fun({Binding, Mode}) ->
        io:format("  Binding: ~p~n", [Binding]),
        io:format("  Mode: ~p~n", [Mode])
    end, Cmodes).
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

## Testing Strategies

### Property-Based Testing

```erlang
% Property: enumeration should be idempotent
prop_idempotent_enumeration(Preset, Marking) ->
    Modes1 = pnet_mode:enum_modes(Preset, Marking),
    Modes2 = pnet_mode:enum_modes(Preset, Marking),
    Modes1 =:= Modes2.

% Property: ordering should not affect result
prop_order_invariant(Preset, Marking) ->
    Modes1 = pnet_mode:enum_modes(Preset, Marking),
    PresetShuffled = shuffle(Preset),
    Modes2 = pnet_mode:enum_modes(PresetShuffled, Marking),
    lists:sort(Modes1) =:= lists:sort(Modes2).
```

### Integration Tests

```erlang
% Test with real workflow scenarios
test_workflow_enablement() ->
    % Setup test workflow
    Preset = [input, condition],
    Marking = #{input => [doc1], condition => [approved]},

    % Test enumeration
    Modes = pnet_mode:enum_modes(Preset, Marking),

    % Verify expected behavior
    Expected = [#{input => [doc1], condition => [approved]}],
    Modes =:= Expected.
```

This comprehensive reference covers all doctest examples and usage patterns for the `pnet_mode` module, providing everything needed to understand and effectively use mode enumeration in both basic and colored Petri net scenarios.