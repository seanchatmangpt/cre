# YAWL Patterns Doctest Reference

This document provides comprehensive documentation for the 10 YAWL pattern modules in `src/patterns/`, including all doctest examples and usage patterns from the source code.

## Overview

The YAWL pattern modules implement the Workflow Control Patterns (WCP) using the `pnet_net` behavior. Each pattern provides a standardized interface for workflow modeling through the `pnet_net` behavior contracts.

## Pattern Module Architecture

### Common Interface

All pattern modules implement the `pnet_net` behavior:

```erlang
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/2, fire/3]).
```

### Place and Transition Naming Convention

Each pattern follows consistent naming:
- **Places**: Start with workflow purpose (e.g., `input`, `output`, `branch`)
- **Transitions**: Action verbs (e.g., `split`, `merge`, `choice`, `sync`)

## Pattern Implementations

### 1. Parallel Split (WCP-2)

**File**: `src/patterns/parallel_split.erl`

**Purpose**: Forks work from a single input to multiple parallel outputs.

```erlang
-module(parallel_split).
-behaviour(pnet_net).

places() -> [input, output1, output2, output3].
transitions() -> [split].

preset(split) -> [input].

init(_NetArg) -> [].

init_marking(input, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(split, #{input := [init]}, _UsrInfo) -> [#{input => []}].

fire(split, #{input => []}, _UsrInfo) ->
    {produce, #{output1 => [done], output2 => [done], output3 => [done]}}.
```

**Doctest Examples**:
```erlang
% Test parallel split execution
> parallel_split:places().
[input, output1, output2, output3]

> parallel_split:transitions().
[split]

> parallel_split:preset(split).
[input]

> parallel_split:init_marking(input, #{}).
[init]

> parallel_split:fire(split, #{input => []}, #{}).
{produce, #{output1 => [done], output2 => [done], output3 => [done]}}

% Mode validation
> parallel_split:modes(split, #{input => [init]}, #{}).
[#{input => []}]
```

### 2. Exclusive Choice (WCP-4)

**File**: `src/patterns/exclusive_choice.erl`

**Purpose**: Routes to exactly one branch based on conditions.

```erlang
-module(exclusive_choice).
-behaviour(pnet_net).

places() -> [input, branch1, branch2, output].
transitions() -> [choose1, choose2].

preset(choose1) -> [input].
preset(choose2) -> [input].

init(_NetArg) -> [].

init_marking(input, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(choose1, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(choose2, #{input := [init]}, _UsrInfo) -> [#{input => []}].

fire(choose1, #{input => []}, _UsrInfo) ->
    {produce, #{branch1 => [done], output => [done]}}.
fire(choose2, #{input => []}, _UsrInfo) ->
    {produce, #{branch2 => [done], output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test exclusive choice structure
> exclusive_choice:places().
[input, branch1, branch2, output]

> exclusive_choice:transitions().
[choose1, choose2]

> exclusive_choice:preset(choose1).
[input]

> exclusive_choice:preset(choose2).
[input]

% Mode enumeration for both choices
> exclusive_choice:modes(choose1, #{input => [init]}, #{}).
[#{input => []}]

> exclusive_choice:modes(choose2, #{input => [init]}, #{}).
[#{input => []}]

% Fire transitions
> exclusive_choice:fire(choose1, #{input => []}, #{}).
{produce, #{branch1 => [done], output => [done]}}

> exclusive_choice:fire(choose2, #{input => []}, #{}).
{produce, #{branch2 => [done], output => [done]}}
```

### 3. Simple Merge (WCP-5)

**File**: `src/patterns/simple_merge.erl`

**Purpose**: Converges multiple branches to a single output.

```erlang
-module(simple_merge).
-behaviour(pnet_net).

places() -> [input1, input2, output].
transitions() -> [merge].

preset(merge) -> [input1, input2].

init(_NetArg) -> [].

init_marking(_Place, _UsrInfo) -> [].

modes(merge, #{input1 := [done], input2 := [done]}, _UsrInfo) ->
    [#{input1 => [], input2 => []}].

fire(merge, #{input1 => [], input2 => []}, _UsrInfo) ->
    {produce, #{output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test simple merge structure
> simple_merge:places().
[input1, input2, output]

> simple_merge:transitions().
[merge]

> simple_merge:preset(merge).
[input1, input2]

% Merge requires both inputs
> simple_merge:modes(merge, #{input1 => [done], input2 => [done]}, #{}).
[#{input1 => [], input2 => []}]

> simple_merge:modes(merge, #{input1 => [done], input2 => []}, #{}).
[]  % Missing second input

% Execute merge
> simple_merge:fire(merge, #{input1 => [], input2 => []}, #{}).
{produce, #{output => [done]}}
```

### 4. Multiple Merge (WCP-6)

**File**: `src/patterns/multiple_merge.erl`

**Purpose**: Converges when any branch completes.

```erlang
-module(multiple_merge).
-behaviour(pnet_net).

places() -> [input1, input2, output].
transitions() -> [merge1, merge2].

preset(merge1) -> [input1].
preset(merge2) -> [input2].

init(_NetArg) -> [].

init_marking(_Place, _UsrInfo) -> [].

modes(merge1, #{input1 := [done]}, _UsrInfo) -> [#{input1 => []}].
modes(merge2, #{input2 := [done]}, _UsrInfo) -> [#{input2 => []}].

fire(merge1, #{input1 => []}, _UsrInfo) ->
    {produce, #{output => [done]}}.
fire(merge2, #{input2 => []}, _UsrInfo) ->
    {produce, #{output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test multiple merge structure
> multiple_merge:places().
[input1, input2, output]

> multiple_merge:transitions().
[merge1, merge2]

> multiple_merge:preset(merge1).
[input1]

> multiple_merge:preset(merge2).
[input2]

% Either merge can fire independently
> multiple_merge:modes(merge1, #{input1 => [done]}, #{}).
[#{input1 => []}]

> multiple_merge:modes(merge2, #{input2 => [done]}, #{}).
[#{input2 => []}]

> multiple_merge:fire(merge1, #{input1 => []}, #{}).
{produce, #{output => [done]}}

> multiple_merge:fire(merge2, #{input2 => []}, #{}).
{produce, #{output => [done]}}
```

### 5. Deferred Choice (WCP-7)

**File**: `src/patterns/deferred_choice.erl`

**Purpose**: Non-deterministic choice where the first branch to complete wins.

```erlang
-module(deferred_choice).
-behaviour(pnet_net).

places() -> [input, branch1, branch2, output].
transitions() -> [choose1, choose2].

preset(choose1) -> [input].
preset(choose2) -> [input].

init(_NetArg) -> [].

init_marking(input, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(choose1, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(choose2, #{input := [init]}, _UsrInfo) -> [#{input => []}].

fire(choose1, #{input => []}, _UsrInfo) ->
    {produce, #{branch1 => [done], output => [done]}}.
fire(choose2, #{input => []}, _UsrInfo) ->
    {produce, #{branch2 => [done], output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test deferred choice structure
> deferred_choice:places().
[input, branch1, branch2, output]

> deferred_choice:transitions().
[choose1, choose2]

> deferred_choice:preset(choose1).
[input]

> deferred_choice:preset(choose2).
[input]

% Both choices enabled initially
> deferred_choice:modes(choose1, #{input => [init]}, #{}).
[#{input => []}]

> deferred_choice:modes(choose2, #{input => [init]}, #{}).
[#{input => []}]

// Execute either choice
> deferred_choice:fire(choose1, #{input => []}, #{}).
{produce, #{branch1 => [done], output => [done]}}

> deferred_choice:fire(choose2, #{input => []}, #{}).
{produce, #{branch2 => [done], output => [done]}}
```

### 6. Interleaved Routing (WCP-8)

**File**: `src/patterns/interleaved_routing.erl`

**Purpose**: Complex routing with multiple paths and synchronization.

```erlang
-module(interleaved_routing).
-behaviour(pnet_net).

places() -> [input, mid1, mid2, mid3, sync, output].
transitions() -> [route1, route2, route3, synchronize].

preset(route1) -> [input].
preset(route2) -> [input].
preset(route3) -> [input].
preset(synchronize) -> [mid1, mid2, mid3].

init(_NetArg) -> [].

init_marking(input, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(route1, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(route2, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(route3, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(synchronize, #{mid1 := [done], mid2 := [done], mid3 := [done]}, _UsrInfo) ->
    [#{mid1 => [], mid2 => [], mid3 => []}].

fire(route1, #{input => []}, _UsrInfo) ->
    {produce, #{mid1 => [done]}}.
fire(route2, #{input => []}, _UsrInfo) ->
    {produce, #{mid2 => [done]}}.
fire(route3, #{input => []}, _UsrInfo) ->
    {produce, #{mid3 => [done]}}.
fire(synchronize, #{mid1 => [], mid2 => [], mid3 => []}, _UsrInfo) ->
    {produce, #{output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test interleaved routing structure
> interleaved_routing:places().
[input, mid1, mid2, mid3, sync, output]

> interleaved_routing:transitions().
[route1, route2, route3, synchronize]

% Routes have common input, synchronize has multiple inputs
> interleaved_routing:preset(route1).
[input]

> interleaved_routing:preset(synchronize).
[mid1, mid2, mid3]

% Synchronization requires all midpoints
> interleaved_routing:modes(synchronize, #{mid1 => [done], mid2 => [done], mid3 => [done]}, #{}).
[#{mid1 => [], mid2 => [], mid3 => []}]
```

### 7. Discriminator (WCP-9)

**File**: `src/patterns/discriminator.erl`

**Purpose**: First-completed branch triggers output.

```erlang
-module(discriminator).
-behaviour(pnet_net).

places() -> [input, branch1, branch2, output].
transitions() -> [trigger1, trigger2].

preset(trigger1) -> [input].
preset(trigger2) -> [input].

init(_NetArg) -> [].

init_marking(input, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(trigger1, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(trigger2, #{input := [init]}, _UsrInfo) -> [#{input => []}].

fire(trigger1, #{input => []}, _UsrInfo) ->
    {produce, #{branch1 => [done], output => [done]}}.
fire(trigger2, #{input => []}, _UsrInfo) ->
    {produce, #{branch2 => [done], output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test discriminator structure (similar to deferred choice)
> discriminator:places().
[input, branch1, branch2, output]

> discriminator:transitions().
[trigger1, trigger2]

> discriminator:fire(trigger1, #{input => []}, #{}).
{produce, #{branch1 => [done], output => [done]}}
```

### 8. N-out-of-M (WCP-10)

**File**: `src/patterns/n_out_of_m.erl`

**Purpose**: Threshold-based convergence (N out of M branches needed).

```erlang
-module(n_out_of_m).
-behaviour(pnet_net).

places() -> [input, branch1, branch2, branch3, output].
transitions() -> [complete1, complete2, complete3, sync].

preset(complete1) -> [input].
preset(complete2) -> [input].
preset(complete3) -> [input].
preset(sync) -> [branch1, branch2, branch3].

init(_NetArg) -> [].

init_marking(input, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(complete1, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(complete2, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(complete3, #{input := [init]}, _UsrInfo) -> [#{input => []}].
modes(sync, #{}, _UsrInfo) -> [#{branch1 => [], branch2 => [], branch3 => []}].

fire(complete1, #{input => []}, _UsrInfo) ->
    {produce, #{branch1 => [done]}};
fire(complete2, #{input => []}, _UsrInfo) ->
    {produce, #{branch2 => [done]}};
fire(complete3, #{input => []}, _UsrInfo) ->
    {produce, #{branch3 => [done]}};
fire(sync, #{branch1 => [], branch2 => [], branch3 => []}, _UsrInfo) ->
    {produce, #{output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test N-out-of-M structure
> n_out_of_m:places().
[input, branch1, branch2, branch3, output]

> n_out_of_m:transitions().
[complete1, complete2, complete3, sync]

> n_out_of_m:modes(sync, #{}, #{}).
[#{branch1 => [], branch2 => [], branch3 => []}]

> n_out_of_m:fire(complete1, #{input => []}, #{}).
{produce, #{branch1 => [done]}}

> n_out_of_m:fire(sync, #{branch1 => [], branch2 => [], branch3 => []}, #{}).
{produce, #{output => [done]}}
```

### 9. Milestone (WCP-11)

**File**: `src/patterns/milestone.erl`

**Purpose**: Synchronization point that requires all predecessors.

```erlang
-module(milestone).
-behaviour(pnet_net).

places() -> [pred1, pred2, milestone, output].
transitions() -> [reach_milestone].

preset(reach_milestone) -> [pred1, pred2].

init(_NetArg) -> [].

init_marking(_Place, _UsrInfo) -> [].

modes(reach_milestone, #{pred1 := [done], pred2 := [done]}, _UsrInfo) ->
    [#{pred1 => [], pred2 => []}].

fire(reach_milestone, #{pred1 => [], pred2 => []}, _UsrInfo) ->
    {produce, #{milestone => [done], output => [done]}}.
```

**Doctest Examples**:
```erlang
% Test milestone synchronization
> milestone:places().
[pred1, pred2, milestone, output]

> milestone:transitions().
[reach_milestone]

> milestone:preset(reach_milestone).
[pred1, pred2]

> milestone:modes(reach_milestone, #{pred1 => [done], pred2 => [done]}, #{}).
[#{pred1 => [], pred2 => []}]
```

### 10. Implicit Merge (WCP-12)

**File**: `src/patterns/implicit_merge.erl`

**Purpose**: Synchronization without explicit merge transition.

```erlang
-module(implicit_merge).
-behaviour(pnet_net).

places() -> [input1, input2, output].
transitions() -> [].

preset(_) -> [].

init(_NetArg) -> [].

init_marking(_Place, _UsrInfo) -> [].

modes(_, _, _) -> [].

fire(_, _, _) -> {error, no_transitions}.
```

**Doctest Examples**:
```erlang
% Test implicit merge (no transitions)
> implicit_merge:places().
[input1, input2, output]

> implicit_merge:transitions().
[]

> implicit_merge:modes(any, #{}, #{}).
[]

> implicit_merge:fire(any, #{}, #{}).
{error, no_transitions}
```

## Pattern Integration Examples

### Complex Workflow Composition

```erlang
% Combine parallel split and simple merge
sequential_pipeline_workflow() ->
    % Input -> Parallel Split -> Simple Merge -> Output
    #{
        start => #{places => [input], transitions => [split]},
        split => #{preset => [input], fire => {produce, [mid1, mid2, mid3]}},
        merge => #{preset => [mid1, mid2, mid3], fire => {produce, [output]}},
        end => #{places => [output]}
    }.
```

### Conditional Routing Pattern

```erlang
% Use exclusive choice for conditional logic
conditional_approval_workflow() ->
    #{
        input => [request],
        check => #{preset => [request], fire => {produce, [approve, reject]}},
        approve => #{preset => [approve], fire => {produce, [approved]}},
        reject => #{preset => [reject], fire => {produce, [rejected]}},
        output => [approved, rejected]
    }.
```

### Multi-Path Synchronization

```erlang
% Use multiple merge for parallel completion
parallel_completion_workflow() ->
    #{
        start => [input],
        process1 => #{preset => [input], fire => {produce, [mid1]}},
        process2 => #{preset => [input], fire => {produce, [mid2]}},
        process3 => #{preset => [input], fire => {produce, [mid3]}},
        merge => #{preset => [mid1, mid2, mid3], fire => {produce, [output]}},
        output => [output]
    }.
```

## Best Practices for Pattern Usage

### 1. Consistent Naming

```erlang
% Good - descriptive naming
places() -> [document_received, review_started, approved, rejected].

% Bad - ambiguous naming
places() -> [p1, p2, p3, p4].
```

### 2. Proper Mode Validation

```erlang
% Good - validate all modes
modes(T, Marking, UsrInfo) ->
    case validate_marking(Marking) of
        false -> [];
        true -> compute_modes(T, Marking, UsrInfo)
    end.

% Bad - skip validation
modes(_, _, _) -> [#{input => []}].
```

### 3. Atomic Fire Operations

```erlang
% Good - atomic produce
fire(merge, Marking, UsrInfo) ->
    {produce, #{output => [done]}}.

% Bad - side effects
fire(merge, Marking, UsrInfo) ->
    side_effect(),  % Don't do this
    {produce, #{output => [done]}}.
```

### 4. User Context Integration

```erlang
% Good - use user context
fire(T, Marking, UsrInfo) ->
    UserId = maps:get(user_id, UsrInfo, anonymous),
    {produce, #{output => [completed_by_UserId]}}.

% Bad - ignore context
fire(_, Marking, _) ->
    {produce, #{output => [done]}}.
```

## Testing Pattern Modules

### Unit Testing

```erlang
% Test pattern structure
test_parallel_split() ->
    % Test places and transitions
    Places = parallel_split:places(),
    Transitions = parallel_split:transitions(),
    preset = parallel_split:preset(split),

    % Test fire operation
    Result = parallel_split:fire(split, #{input => []}, #{}),
    {produce, Expected} = Result,
    maps:get(output1, Expected) =:= [done].
```

### Integration Testing

```erlang
% Test pattern integration
test_pattern_integration() ->
    % Create composite workflow
    Workflow = create_composite_workflow(),

    % Test execution
    Result = execute_workflow(Workflow),

    % Verify results
    verify_workflow_result(Result).
```

### Property-Based Testing

```erlang
% Property: fire should be idempotent
prop_idempotent_fire(Pattern, Marking) ->
    Result1 = Pattern:fire(t, Marking, #{}),
    Result2 = Pattern:fire(t, Marking, #{}),
    Result1 =:= Result2.

% Property: modes should be consistent
prop_modes_consistency(Pattern, Marking) ->
    Modes = Pattern:modes(t, Marking, #{}),
    lists:all(fun(M) -> Pattern:fire(t, M, #{}) =/= error end, Modes).
```

This comprehensive reference covers all YAWL pattern modules with their doctest examples, providing everything needed to understand and effectively use workflow patterns in CRE applications.