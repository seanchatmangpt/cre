# PNet Marking API Reference

## Module: `pnet_marking`

A comprehensive multiset marking algebra for Petri net workflow patterns.

## Summary

This module provides the mathematical foundation for representing and manipulating workflow state in the CRE system. It implements multiset operations with precise error handling, canonical hashing, and atomic transition firing.

## Data Types

### place()

```erlang
-type place() :: atom().
```

A workflow location represented as an atom. Examples: `start`, `end`, `decision_point`.

**Examples**:
```erlang
start    :: place(),
task_1   :: place(),
approval :: place(),
end      :: place().
```

### token()

```erlang
-type token() :: term().
```

Any Erlang term representing a workflow entity. Can be data, work items, or any other information.

**Examples**:
```erlang
"work_item_id"     :: token(),
#{id => 123}        :: token(),
{task, "process"}  :: token(),
42                  :: token().
```

### marking()

```erlang
-type marking() :: #{place() => [token()]}.
```

Represents the current state of a workflow as a map from places to their token multisets.

**Example**:
```erlang
#{start => ["init"],
  queue => ["task1", "task1", "task2"],
  end   => []}
```

### consume_map()

```erlang
-type consume_map() :: #{place() => [token()]}.
```

Specifies tokens to be removed from the marking during transition firing.

**Example**:
```erlang
#{queue => ["task1"],
  temp   => ["lock"]}
```

### produce_map()

```erlang
-type produce_map() :: #{place() => [token()]}.
```

Specifies tokens to be added to the marking during transition firing.

**Example**:
```erlang
#{output => ["result1"],
  log     => ["log_entry"]}
```

## Public Functions

### new(Places) -> marking()

Creates a new empty marking with specified places.

**Parameters**:
- `Places` - List of place atoms to include in the marking

**Return Value**:
- A new marking with all places initialized to empty token lists

**Examples**:
```erlang
% Create a simple workflow
> pnet_marking:new([start, end]).
#{start => [], end => []}

% Create a complex workflow
> pnet_marking:new([input, queue, processing, output]).
#{input => [], queue => [], processing => [], output => []}
```

**Performance**:
- Time: O(n) where n is the number of places
- Space: O(n)

### get(Marking, Place) -> {ok, [token()]} | {error, bad_place}

Retrieves tokens at a specific place.

**Parameters**:
- `Marking` - The marking to query
- `Place` - The place to get tokens from

**Return Value**:
- `{ok, Tokens}` if place exists
- `{error, bad_place}` if place doesn't exist

**Examples**:
```erlang
% Get tokens from existing place
> M = #{p1 => [a,b,c], p2 => [d]}.
> pnet_marking:get(M, p1).
{ok, [a,b,c]}

% Get tokens from empty place
> pnet_marking:get(M, p2).
{ok, [d]}

% Get tokens from non-existent place
> pnet_marking:get(M, missing).
{error, bad_place}
```

**Performance**:
- Time: O(1) average case (Erlang map lookup)
- Space: O(1)

### set(Marking, Place, Tokens) -> marking() | {error, bad_place}

Sets tokens at a specific place, replacing existing tokens.

**Parameters**:
- `Marking` - The marking to modify
- `Place` - The place to set tokens at
- `Tokens` - The new token list for the place

**Return Value**:
- Updated marking if successful
- `{error, bad_place}` if place doesn't exist

**Examples**:
```erlang
% Set tokens at a place
> pnet_marking:set(#{p => []}, p, [a,b]).
#{p => [a,b]}

% Set tokens with different order (same multiset)
> pnet_marking:set(#{p => [a,b]}, p, [b,a]).
#{p => [b,a]}

% Set tokens at non-existent place
> pnet_marking:set(#{p => [a]}, missing, [x]).
{error, bad_place}
```

**Performance**:
- Time: O(1) average case
- Space: O(k) where k is the number of tokens

### add(Marking, ProduceMap) -> marking() | {error, bad_place}

Adds tokens to places using a produce map.

**Parameters**:
- `Marking` - The marking to add tokens to
- `ProduceMap` - Map of places to token lists to add

**Return Value**:
- Updated marking if successful
- `{error, bad_place}` if any place doesn't exist

**Examples**:
```erlang
% Add tokens to a place
> M0 = #{p => []}.
> pnet_marking:add(M0, #{p => [a]}).
#{p => [a]}

% Add tokens to multiple places
> pnet_marking:add(M0, #{p => [a,b], q => [c]}).
#{p => [a,b], q => [c]}

% Add tokens to non-existent place
> pnet_marking:add(M0, #{missing => [x]}).
{error, bad_place}
```

**Performance**:
- Time: O(m) where m is total tokens to add
- Space: O(m)

### take(Marking, ConsumeMap) -> {ok, marking()} | {error, bad_place | insufficient}

Removes tokens using consume map semantics.

**Parameters**:
- `Marking` - The marking to take tokens from
- `ConsumeMap` - Map of places to token lists to remove

**Return Value**:
- `{ok, UpdatedMarking}` on success
- `{error, bad_place}` if any place doesn't exist
- `{error, insufficient}` if tokens can't be consumed

**Examples**:
```erlang
% Take exact tokens (success)
> M1 = #{p => [a,b]}.
> pnet_marking:take(M1, #{p => [a,b]}).
{ok, #{p => []}}

% Take tokens in different order
> pnet_marking:take(M1, #{p => [b,a]}).
{ok, #{p => []}}

% Take insufficient tokens
> pnet_marking:take(M1, #{p => [a,a]}).
{error, insufficient}

% Take from non-existent place
> pnet_marking:take(M1, #{missing => [x]}).
{error, bad_place}
```

**Performance**:
- Time: O(m × n) where m is available tokens, n is tokens to take
- Space: O(1)

### apply(Marking, ConsumeMap, ProduceMap) -> {ok, marking()} | {error, bad_place | insufficient}

Atomic transition firing - consume then produce.

**Parameters**:
- `Marking` - The marking to apply the operation to
- `ConsumeMap` - Map of places to token lists to remove
- `ProduceMap` - Map of places to token lists to add

**Return Value**:
- `{ok, UpdatedMarking}` on success
- Error if consumption or production fails

**Examples**:
```erlang
% Successful atomic operation
> M1 = #{p => [a,b], q => [c]}.
> pnet_marking:apply(M1, #{p => [a]}, #{q => [d]}).
{ok, #{p => [b], q => [c,d]}}

% Failed consumption (marking unchanged)
> pnet_marking:apply(M1, #{p => [a,a]}, #{q => [d]}).
{error, insufficient}

% Bad place in consume map
> pnet_marking:apply(M1, #{missing => [a]}, #{q => [d]}).
{error, bad_place}
```

**Performance**:
- Time: O(m + n) where m is consume tokens, n is produce tokens
- Space: O(1)

### snapshot(Marking) -> marking()

Creates a deep copy of the marking.

**Parameters**:
- `Marking` - The marking to snapshot

**Return Value**:
- An identical copy of the marking

**Examples**:
```erlang
% Create snapshot
> M1 = #{p => [a,b]}.
> M2 = pnet_marking:snapshot(M1).
#{p => [a,b]}

% Verify immutability
> M2#{p => [x]}.
#{p => [x]}
> M1.
#{p => [a,b]}
```

**Performance**:
- Time: O(1) (due to Erlang immutability)
- Space: O(1) (same reference returned)

### hash(Marking) -> binary()

Computes a canonical hash of the marking.

**Parameters**:
- `Marking` - The marking to hash

**Return Value**:
- Binary hash of the marking (SHA-256)

**Examples**:
```erlang
> M1 = #{p => [a,b]}.
> M2 = #{p => [b,a]}.
> M3 = #{p => [a,c]}.

> pnet_marking:hash(M1).
<<98,247,139,121,58,179,157,212,...>>

> pnet_marking:hash(M1) =:= pnet_marking:hash(M2).
true

> pnet_marking:hash(M1) =:= pnet_marking:hash(M3).
false
```

**Performance**:
- Time: O(m) where m is total tokens
- Space: O(32) bytes (SHA-256 hash)

## Error Conditions

### bad_place

Returned when:
- Accessing non-existent places
- Using invalid place references in any operation

**Examples**:
```erlang
pnet_marking:get(#{p => [a]}, missing).
{error, bad_place}

pnet_marking:add(#{p => [a]}, #{missing => [x]}).
{error, bad_place}
```

### insufficient

Returned when:
- Trying to take more tokens than available
- Missing required tokens for consumption

**Examples**:
```erlang
pnet_marking:take(#{p => [a]}, #{p => [a,a]}).
{error, insufficient}

pnet_marking:apply(#{p => [a]}, #{p => [b]}, #{q => [c]}).
{error, insufficient}
```

## Usage Patterns

### 1. Workflow Initialization

```erlang
% Create a simple workflow
Workflow = pnet_marking:new([start, task, end]).

% Add initial token
{ok, Active} = pnet_marking:add(Workflow, #{start => [init]}).
```

### 2. Transition Firing

```erlang
% Define transition inputs and outputs
Consume = #{task => ["work_item"]},
Produce = #{end => ["completed"]}.

% Apply transition
case pnet_marking:apply(Marking, Consume, Produce) of
    {ok, NewMarking} -> % proceed to next state
    {error, insufficient} -> % handle insufficient resources
    {error, bad_place} -> % handle invalid places
end.
```

### 3. State Comparison

```erlang
% Check if marking changed
MarkingHash1 = pnet_marking:hash(Marking1),
MarkingHash2 = pnet_marking:hash(Marking2),

if
    MarkingHash1 =:= MarkingHash2 ->
        % State unchanged
    true ->
        % State changed
end.
```

### 4. Error Handling

```erlang
% Robust operation handling
case pnet_marking:apply(Marking, ConsumeMap, ProduceMap) of
    {ok, NewMarking} ->
        % Success - proceed with new state
        handle_success(NewMarking);
    {error, insufficient} ->
        % Handle insufficient tokens
        handle_insufficient();
    {error, bad_place} ->
        % Handle invalid places
        handle_bad_place()
end.
```

## Testing

The module includes comprehensive doctests that can be run with:

```bash
rebar3 eunit
```

Doctests verify all operations including:
- Basic operations (new, get, set)
- Multiset operations (add, take, apply)
- Error conditions (bad_place, insufficient)
- Hash consistency
- Edge cases

## Performance Notes

### Optimization Guidelines

1. **Use atomic operations**: Prefer `apply/3` for transition firing
2. **Minimize hash computations**: Hashing is O(m) operation
3. **Handle errors early**: Check place existence before operations
4. **Consider token batching**: For large token lists

### Memory Considerations

- Memory usage scales linearly with token count
- Erlang immutability ensures thread safety
- Consider token compaction for large workflows

### Concurrency

- All operations are thread-safe due to immutability
- No locks or synchronization needed
- Can be used safely in concurrent workflows

## Related Modules

- `pnet_types` - Type definitions and validation
- `pnet_mode` - Mode enumeration utilities
- `gen_pnet` - OTP process maintaining workflow state
- `yawl_validate` - YAWL specification validation
- `yawl_compile` - YAWL compilation to net modules

## See Also

- `pnet_types.erl` - Complete type system
- `pnet_mode.erl` - Mode enumeration utilities
- `gen_pnet.erl` - Workflow process implementation
- `docs/pnet_marking_algebra.md` - Mathematical concepts
- `docs/pnet_marking_implementation.md` - Implementation details

## Authors

- CRE Team
- Based on mathematical foundations of Petri nets
- Implemented for the Common Runtime Environment (CRE)

## License

Apache License, Version 2.0

## Version History

- **1.0.0**: Initial implementation with complete multiset algebra
- **1.1.0**: Added canonical hashing and snapshot operations
- **1.2.0**: Enhanced error handling and performance optimizations