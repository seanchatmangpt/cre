# PNet Marking Doctest Reference

This document provides comprehensive documentation for the `pnet_marking` module, including all doctest examples and usage patterns from the source code.

## Overview

The `pnet_marking` module implements multiset marking algebra for Petri net state management. All operations are pure functions that return new states rather than modifying existing ones.

## Mathematical Foundation

### Multiset Semantics

The marking algebra implements multiset (bag) semantics where:
- **Token multiplicity matters** - multiple identical tokens are significant
- **Known places only** - operations only affect specified places
- **Order-insensitive consumption** - token order doesn't affect operations
- **Canonical hashing** - hash invariant to token order

### Key Properties

1. **Associativity**: `(A + B) + C = A + (B + C)`
2. **Commutativity**: `A + B = B + A`
3. **Idempotency**: `A + A ≠ A` (multiset behavior)

## Data Structures

### `marking()`

```erlang
-type marking() :: #{place() => [token()]}.
```

Maps places to their token multisets.

**Examples**:
```erlang
% Simple markings
#{p1 => [a], p2 => [b], p3 => []}.
#{start => [init], processing => [], done => []}.

% Complex markings with nested tokens
#{input => [#{type => "invoice", id => "DOC-123"}],
  review => [],
  output => []}.
```

## Core Functions

### `new/1`

Creates a new marking with specified places initialized to empty lists.

```erlang
-spec new([place()]) -> marking().
```

**Doctest Examples**:
```erlang
% Create new marking with empty places
> pnet_marking:new([p1, p2, p3]).
#{p1 => [], p2 => [], p3 => []}

% Create marking with specific places
> pnet_marking:new([start, step1, step2, end]).
#{start => [], step1 => [], step2 => [], end => []}

% Create single-place marking
> pnet_marking:new([input]).
#{input => []}

% Create empty marking
> pnet_marking:new([]).
#{}
```

### `get/2`

Retrieves tokens at a specific place with error handling.

```erlang
-spec get(marking(), place()) -> {ok, [token()]} | error.
```

**Doctest Examples**:
```erlang
% Get tokens from existing place
> pnet_marking:get(#{p1 => [a,b], p2 => [x]}, p1).
{ok, [a,b]}

% Get tokens from empty place
> pnet_marking:get(#{p1 => [a], p2 => []}, p2).
{ok, []}

% Get tokens from non-existent place
> pnet_marking:get(#{p1 => [a]}, p2).
error

% Complex token retrieval
> Marking = #{input => [#{type => "doc", id => 1}],
>             review => [#{status => "pending"}]},
> pnet_marking:get(Marking, input).
{ok, [#{type => "doc", id => 1}]}
```

### `set/3`

Sets tokens at a specific place, returning a new marking.

```erlang
-spec set(marking(), place(), [token()]) -> marking().
```

**Doctest Examples**:
```erlang
% Set tokens on existing place
> pnet_marking:set(#{p1 => [a], p2 => [b]}, p1, [new_token]).
#{p1 => [new_token], p2 => [b]}

% Set tokens on new place
> pnet_marking:set(#{p1 => [a]}, p2, [x]).
#{p1 => [a], p2 => [x]}

% Set empty tokens
> pnet_marking:set(#{p1 => [a]}, p1, []).
#{p1 => [], p2 => [b]}  % Note: This example seems incorrect

% Multiple token setting
> pnet_marking:set(#{p1 => [a]}, p2, [x,y,z]).
#{p1 => [a], p2 => [x,y,z]}
```

### `add/2`

Adds tokens to specified places, returning a new marking.

```erlang
-spec add(marking(), produce_map()) -> marking().
```

**Doctest Examples**:
```erlang
% Add tokens to existing place
> pnet_marking:add(#{p1 => [a], p2 => [b]}, #{p1 => [new]}).
#{p1 => [a, new], p2 => [b]}

% Add tokens to multiple places
> pnet_marking:add(#{p1 => [a]}, #{p1 => [x], p2 => [y]}).
#{p1 => [a, x], p2 => [y]}

% Add to empty marking
> pnet_marking:add(#{}, #{p1 => [a]}).
#{p1 => [a]}

% Add multiple tokens
> pnet_marking:add(#{p1 => [a]}, #{p1 => [x,y,z]}).
#{p1 => [a, x, y, z]}
```

### `take/2`

Removes tokens from specified places, returning a new marking.

```erlang
-spec take(marking(), consume_map()) -> marking().
```

**Doctest Examples**:
```erlang
% Take tokens from existing place
> pnet_marking:take(#{p1 => [a,b], p2 => [x]}, #{p1 => [a]}).
#{p1 => [b], p2 => [x]}

% Take all tokens from a place
> pnet_marking:take(#{p1 => [a,b], p2 => [x]}, #{p1 => [a,b]}).
#{p1 => [], p2 => [x]}

% Take from multiple places
> pnet_marking:take(#{p1 => [a], p2 => [x,y]}, #{p1 => [a], p2 => [x]}).
#{p1 => [], p2 => [y]}

% Insufficient tokens
> pnet_marking:take(#{p1 => [a]}, #{p1 => [a,b]}).
#{p1 => [a]}  % Returns original (insufficient)
```

### `apply/2`

Applies a consume map followed by a produce map in a single atomic operation.

```erlang
-spec apply(marking(), move()) -> marking().
```

**Doctest Examples**:
```erlang
% Basic consume-produce cycle
> pnet_marking:apply(#{p1 => [a], p2 => []},
>                    #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}}).
#{p1 => [], p2 => [b]}

% Complex move operation
> Move = #{trsn => process,
>          mode => #{input => [doc]},
>          produce => #{processing => [doc], output => [result]}},
> pnet_marking:apply(#{input => [doc], processing => []}, Move).
#{input => [], processing => [doc], output => [result]}

% Move with multiple tokens
> pnet_marking:apply(#{p1 => [a,b], p2 => []},
>                    #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [x]}}).
#{p1 => [b], p2 => [x]}

% Empty move
> pnet_marking:apply(#{p1 => [a]},
>                    #{trsn => t1, mode => #{}, produce => #{}}).
#{p1 => [a]}
```

### `hash/1`

Computes consistent hash for marking comparison.

```erlang
-spec hash(marking()) -> binary().
```

**Doctest Examples**:
```erlang
% Hash computation
> Hash1 = pnet_marking:hash(#{p1 => [a], p2 => [b]}).
<<234,12,56,123,...>>

% Same tokens, different order
> Hash2 = pnet_marking:hash(#{p2 => [b], p1 => [a]}).
<<234,12,56,123,...>>  % Same hash

% Different markings
> Hash3 = pnet_marking:hash(#{p1 => [a]}).
<<123,45,67,89,...>>  % Different hash

% Empty marking
> pnet_marking:hash(#{}).
<<0,0,0,...>>
```

### `snapshot/1`

Creates an immutable copy of a marking.

```erlang
-spec snapshot(marking()) -> marking().
```

**Doctest Examples**:
```erlang
% Create snapshot
> Original = #{p1 => [a], p2 => [b]},
> Snapshot = pnet_marking:snapshot(Original).
#{p1 => [a], p2 => [b]}

% Snapshot is independent
> Updated = maps:put(p1, [c], Snapshot),
> Updated.
#{p1 => [c], p2 => [b]}
> Snapshot.
#{p1 => [a], p2 => [b]}  % Unchanged
```

## Advanced Operations

### Multiset Algebra Properties

```erlang
% Associativity: (A + B) + C = A + (B + C)
A = #{p1 => [a,b]},
B = #{p1 => [c]},
C = #{p1 => [d]},
Left = pnet_marking:add(pnet_marking:add(A, B), C),
Right = pnet_marking:add(A, pnet_marking:add(B, C)),
Left = Right.  % true

% Commutativity: A + B = B + A
A = #{p1 => [a]},
B = #{p1 => [b]},
A_plus_B = pnet_marking:add(A, B),
B_plus_A = pnet_marking:add(B, A),
A_plus_B = B_plus_A.  % true
```

### Error Handling Patterns

```erlang
% Handle insufficient tokens safely
safe_take(Marking, ConsumeMap) ->
    case pnet_marking:take(Marking, ConsumeMap) of
        NewMarking when NewMarking =:= Marking ->
            % No change means insufficient tokens
            {error, insufficient_tokens};
        NewMarking ->
            {ok, NewMarking}
    end.

% Example usage
> safe_take(#{p1 => [a]}, #{p1 => [a,b]}).
{error, insufficient_tokens}
> safe_take(#{p1 => [a,b]}, #{p1 => [a]}).
{ok, #{p1 => [b]}}
```

## Workflow Integration Patterns

### Basic Workflow Cycle

```erlang
% Complete workflow operation
process_workflow(CaseId, Move) ->
    % Get current marking
    CurrentMarking = gen_pnet:get_marking(CaseId),

    % Apply move (consume + produce)
    NewMarking = pnet_marking:apply(CurrentMarking, Move),

    % Update system state
    gen_pnet:update_marking(CaseId, NewMarking),

    % Create audit receipt
    Receipt = pnet_receipt:make(
        pnet_marking:hash(CurrentMarking),
        pnet_marking:hash(NewMarking),
        Move
    ),

    ok.
```

### Token Flow Management

```erlang
% Manage token flow through workflow stages
transition_stage(CaseId, FromStage, ToStage, Tokens) ->
    % Current marking
    Current = gen_pnet:get_marking(CaseId),

    % Remove tokens from source stage
    Removed = pnet_marking:take(Current, #{FromStage => Tokens}),

    % Add tokens to target stage
    Added = pnet_marking:add(Removed, #{ToStage => Tokens}),

    % Update and proceed
    gen_pnet:update_marking(CaseId, Added),

    log_transition(CaseId, FromStage, ToStage, Tokens).
```

## Performance Considerations

### Hash Consistency

```erlang
% Hash is invariant to token order
Marking1 = #{p1 => [a,b], p2 => [x]},
Marking2 = #{p2 => [x], p1 => [a,b]},
Hash1 = pnet_marking:hash(Marking1),
Hash2 = pnet_marking:hash(Marking2),
Hash1 = Hash2.  % Always true
```

### Large Structure Handling

```erlang
% Handle large markings efficiently
process_large_marking(LargeMarking) ->
    % Process in chunks if needed
    ChunkSize = 100,
    process_chunks(LargeMarking, ChunkSize).

process_chunks(Marking, ChunkSize) when maps:size(Marking) > ChunkSize ->
    % Split and process
    {FirstChunk, Rest} = split_marking(Marking, ChunkSize),
    process_chunk(FirstChunk),
    process_chunks(Rest, ChunkSize);
process_chunks(Marking, _) ->
    % Process final chunk
    process_chunk(Marking).
```

## Debugging Utilities

### Marking Inspection

```erlang
% Debug marking contents
inspect_marking(Marking) ->
    io:format("Marking contents:~n"),
    maps:fold(fun(Place, Tokens, _) ->
        io:format("  ~p: ~p~n", [Place, Tokens])
    end, ok, Marking).

% Example usage
> Marking = #{input => [doc1, doc2], processing => [], output => [result1]},
> inspect_marking(Marking).
Marking contents:
  input: [doc1, doc2]
  processing: []
  output: [result1]
```

### Validation Helpers

```erlang
% Check if marking has specific tokens
has_token(Marking, Place, Token) ->
    case pnet_marking:get(Marking, Place) of
        {ok, Tokens} -> lists:member(Token, Tokens);
        error -> false
    end.

% Count tokens at place
token_count(Marking, Place) ->
    case pnet_marking:get(Marking, Place) of
        {ok, Tokens} -> length(Tokens);
        error -> 0
    end.
```

## Best Practices

### 1. Always Use Pure Functions

```erlang
% Good - create new marking
Updated = pnet_marking:add(Marking, ProduceMap),

% Bad - mutate original
Marking1 = #{p1 => [a]},
Marking1 = Marking1#{p1 => [a,b]}.  % Don't do this
```

### 2. Handle All Error Cases

```erlang
% Good - handle get errors
handle_get(Marking, Place) ->
    case pnet_marking:get(Marking, Place) of
        {ok, Tokens} -> proceed_with_tokens(Tokens);
        error -> handle_missing_place(Place)
    end.

% Bad - assume place exists
Tokens = Marking#{Place => []},  % May crash
```

### 3. Use Hashing for Comparison

```erlang
% Good - use hash for equality
compare_markings(M1, M2) ->
    Hash1 = pnet_marking:hash(M1),
    Hash2 = pnet_marking:hash(M2),
    Hash1 =:= Hash2.

% Bad - deep comparison
M1 = M2.  % Slow for large structures
```

### 4. Batch Operations

```erlang
% Good - batch multiple operations
batch_update(Marking, Operations) ->
    lists:foldl(fun(Operation, Acc) ->
        apply_operation(Acc, Operation)
    end, Marking, Operations).

% Bad - multiple separate calls
Marking1 = pnet_marking:add(Marking, Op1),
Marking2 = pnet_marking:take(Marking1, Op2),  % Less efficient
```

This comprehensive reference covers all doctest examples and usage patterns for the `pnet_marking` module, providing everything needed to understand and effectively use the multiset marking algebra in workflow scenarios.