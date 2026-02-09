# PNet Marking Algebra - Quick Reference

## Core Concepts

### Marking = Multiset of Tokens at Places
```erlang
% Example marking
#{start => ["init"],
  queue => ["task1", "task1", "task2"],
  end   => []}
```

### Key Properties
- **Multiplicity matters**: Token counts affect operations
- **Known places only**: Unknown places return `{error, bad_place}`
- **Order-insensitive consumption**: Token order doesn't matter for take/apply
- **Canonical hashing**: Hash invariant to token order

## Essential Operations

### 1. Creation
```erlang
% Create new empty marking
M0 = pnet_marking:new([start, end]).
#{start => [], end => []}
```

### 2. Basic Access
```erlang
% Get tokens at a place
pnet_marking:get(Marking, place).
{ok, [token1, token2]} | {error, bad_place}

% Set tokens at a place
pnet_marking:set(Marking, place, [token1, token2]).
Marking | {error, bad_place}
```

### 3. Multiset Operations
```erlang
% Add tokens (append to existing)
pnet_marking:add(Marking, #{place => [new_token]}).
UpdatedMarking | {error, bad_place}

% Take tokens (exact match required)
pnet_marking:take(Marking, #{place => [token1, token2]}).
{ok, UpdatedMarking} | {error, bad_place | insufficient}

% Atomic transition firing
pnet_marking:apply(Marking, ConsumeMap, ProduceMap).
{ok, UpdatedMarking} | {error, bad_place | insufficient}
```

### 4. Utilities
```erlang
% Create snapshot (same reference, clear semantics)
pnet_marking:snapshot(Marking).
Marking

% Compute canonical hash
pnet_marking:hash(Marking).
<<binary_hash>>
```

## Common Patterns

### Workflow Initialization
```erlang
% Create and start workflow
Workflow = pnet_marking:new([start, task, end]),
{ok, Active} = pnet_marking:add(Workflow, #{start => [init]}).
```

### Transition Firing
```erlang
% Define transition
Consume = #{task => ["work_item"]},
Produce = #{end => ["completed"]}.

% Apply transition
case pnet_marking:apply(Active, Consume, Produce) of
    {ok, NewState} -> % proceed
    {error, insufficient} -> % handle error
end.
```

### State Management
```erlang
% Check if state changed
Hash1 = pnet_marking:hash(State1),
Hash2 = pnet_marking:hash(State2),
Hash1 =:= Hash2.  % Same state?
```

## Error Handling

### Always Handle These Errors
```erlang
case Operation of
    {ok, Result} -> % success
    {error, bad_place} -> % invalid place
    {error, insufficient} -> % not enough tokens
end
```

### Common Error Scenarios
```erlang
% Bad place
pnet_marking:get(#{p => [a]}, missing).
{error, bad_place}

% Insufficient tokens
pnet_marking:take(#{p => [a]}, #{p => [a,a]}).
{error, insufficient}
```

## Doctest Examples

### Basic Operations
```erlang
% Create and query
> M0 = pnet_marking:new([p1, p2]).
#{p1 => [], p2 => []}
> pnet_marking:get(M0, p1).
{ok, []}

% Add tokens
> {ok, M1} = pnet_marking:add(M0, #{p1 => [a,b]}).
#{p1 => [a,b], p2 => []}

% Take tokens
> {ok, M2} = pnet_marking:take(M1, #{p1 => [b,a]}).
#{p1 => [], p2 => []}
```

### Multiset Semantics
```erlang
% Hash is order-invariant
> {ok, Ma} = pnet_marking:set(pnet_marking:new([p]), p, [a,b]).
> {ok, Mb} = pnet_marking:set(pnet_marking:new([p]), p, [b,a]).
> pnet_marking:hash(Ma) =:= pnet_marking:hash(Mb).
true
```

## Performance Tips

### Fast Operations
- `get/2` - O(1) average case
- `set/3` - O(1) average case
- `snapshot/1` - O(1) (immutable)

### Slower Operations
- `take/2` - O(m × n) multiset matching
- `apply/3` - O(m + n) transition firing
- `hash/1` - O(m) hashing

### Optimization Rules
1. Use `apply/3` for atomic operations (not separate take/add)
2. Minimize hash computations in loops
3. Validate places before operations
4. Handle errors early to avoid unnecessary processing

## Workflow Example

### Simple Pipeline
```erlang
% Initialize
> Init = pnet_marking:new([input, output]).
#{input => [], output => []}

% Add input
> {ok, Active} = pnet_marking:add(Init, #{input => ["data"]}.
#{input => ["data"], output => []}

% Process
> {ok, Complete} = pnet_marking:apply(Active, #{input => ["data"]}, #{output => ["result"]}.
#{input => [], output => ["result"]}
```

## API Summary

| Function | Purpose | Performance |
|----------|---------|-------------|
| `new/1` | Create empty marking | O(n) places |
| `get/2` | Get tokens at place | O(1) |
| `set/3` | Set tokens at place | O(1) |
| `add/2` | Add tokens (append) | O(m) tokens |
| `take/2` | Remove tokens | O(m × n) |
| `apply/3` | Atomic transition | O(m + n) |
| `snapshot/1` | Copy marking | O(1) |
| `hash/1` | Compute hash | O(m) |

## Key Rules

1. **Always validate places** - `{error, bad_place}` is common
2. **Check sufficient tokens** - `{error, insufficient}` prevents invalid states
3. **Use atomic operations** - `apply/3` ensures consistency
4. **Hash for comparison** - Fast state equality checking
5. **Handle all errors** - Robust workflow execution

## Troubleshooting

### Common Issues
```erlang
% "Why do I get {error, bad_place}?"
% Answer: You're referencing a place that doesn't exist

% "Why do I get {error, insufficient}?"
% Answer: You're trying to take more tokens than available

% "Why is my state unchanged after apply?"
% Answer: Consumption failed - check token availability
```

### Debug Tips
```erlang
% Print current marking
io:format("~p~n", [Marking]).

% Check hash value
io:format("Hash: ~p~n", [pnet_marking:hash(Marking)]).

% Validate operation step by step
case pnet_marking:take(Marking, Consume) of
    {ok, TmpMarking} -> % then add
    {error, Reason} -> % debug reason
end.
```