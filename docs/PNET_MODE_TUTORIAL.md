# Petri Net Mode Enumeration Tutorial

This tutorial provides a comprehensive, step-by-step introduction to mode enumeration in Petri nets using the `pnet_mode` module.

## Prerequisites

Before starting this tutorial, make sure you understand:
- Basic Petri net concepts (places, transitions, tokens, markings)
- Erlang syntax and basic data structures
- The difference between basic and colored Petri nets

## Tutorial Outline

1. [Getting Started](#getting-started)
2. [Basic Mode Enumeration](#basic-mode-enumeration)
3. [Understanding Preset Counts](#understanding-preset-counts)
4. [Cartesian Product Enumeration](#cartesian-product-enumeration)
5. [Handling Edge Cases](#handling-edge-cases)
6. [Colored Net Mode Enumeration](#colored-net-mode-enumeration)
7. [Real-world Examples](#real-world-examples)
8. [Integration with Transition Firing](#integration-with-transition-firing)

## Getting Started

Let's start with a simple Petri net example:

```erlang
% Define a simple Petri net
-module(simple_net).
-behaviour(pnet_net).

places() -> [start, process, end].
transitions() -> [t1, t2].

preset(t1) -> [start].
preset(t2) -> [process].

init(_NetArg) -> [].
init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{start := [init]}, _UsrInfo) -> [#{start => []}].
modes(t2, #{process := [done]}, _UsrInfo) -> [#{process => []}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{process => [done]}}.
fire(t2, #{process => []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.
```

Now let's import the pnet_mode module and start exploring:

```erlang
> % Import the module
> c(pnet_mode).
{ok, pnet_mode}

> % Check the current marking
> Marking = #{start => [init], process => [], end => []}.
#{start => [init],process => [],end => []}
```

## Basic Mode Enumeration

### Step 1: Simple Single Place Mode Enumeration

Let's start with the simplest case - a transition with a single input place:

```erlang
> % Define a preset with one place
> Preset = [start].

> % Current marking has one token in start
> Marking = #{start => [init]}.

> % Enumerate all possible modes
> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{start => [init]}]

> % Result: only one mode possible
> hd(Modes).
#{start => [init]}
```

**Explanation**: With only one token available and only one place needing a token, there's exactly one way to fire this transition.

### Step 2: Multiple Tokens in a Single Place

What if we have multiple tokens in the input place?

```erlang
> % Multiple tokens in single place
> Marking = #{p => [a, b, c]}.
> Preset = [p].

> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{p => [a]},
 #{p => [b]},
 #{p => [c]}]

> % Check the number of modes
> length(Modes).
3

> % Each mode selects a different token
> [maps:get(p, M) || M <- Modes].
[[a],[b],[c]]
```

**Explanation**: When a place has N tokens and needs K tokens (K=1 here), the system generates N modes, each selecting one different token.

### Step 3: Multiple Places with Single Tokens

Now let's explore transitions with multiple input places:

```erlang
> % Multiple places with single tokens each
> Marking = #{p1 => [a], p2 => [x]}.
> Preset = [p1, p2].

> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{p1 => [a],p2 => [x]}]

> % Only one mode possible since each place has exactly one token
> length(Modes).
1
```

**Explanation**: When each input place has exactly one token, there's only one way to fire the transition - use the token from each place.

### Step 4: Multiple Places with Multiple Tokens (Cartesian Product)

This is where mode enumeration gets interesting:

```erlang
> % Multiple places with multiple tokens each
> Marking = #{p1 => [a, b], p2 => [x, y]}.
> Preset = [p1, p2].

> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{p1 => [a],p2 => [x]},
 #{p1 => [a],p2 => [y]},
 #{p1 => [b],p2 => [x]},
 #{p1 => [b],p2 => [y]}]

> % Total number of modes
> length(Modes).
4

> % Verify it's a Cartesian product
> Expected = [[a], [b]],  % From p1
> XExpected = [[x], [y]], % From p2
> 2 * 2 = 4.
true
```

**Key Insight**: The system computes the Cartesian product of possible token selections from each input place.

## Understanding Preset Counts

### Step 5: Preset Multiplicity

What if a transition needs multiple tokens from the same place?

```erlang
> % Place appears multiple times in preset
> Preset = [p, p, q].

> % Count how many tokens needed from each place
> Counts = pnet_mode:preset_counts(Preset).
#{p => 2, q => 1}

> % Marking with sufficient tokens
> Marking = #{p => [a, b, c], q => [x]}.

> % Enumerate modes
> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{p => [a,b],q => [x]},
 #{p => [a,c],q => [x]},
 #{p => [b,c],q => [x]}]

> % Number of modes: 3 choose 2 = 3
> length(Modes).
3
```

**Explanation**: For place `p`, we need to choose 2 tokens from 3 available (3 combinations: a&b, a&c, b&c). For place `q`, we need 1 token from 1 available (only option: x). Total modes = 3 × 1 = 3.

### Step 6: Complex Multiplicity Example

Let's try a more complex example:

```erlang
> % Complex preset: need 3 tokens from a, 2 from b, 1 from c
> Preset = [a, a, a, b, b, c].

> Counts = pnet_mode:preset_counts(Preset).
#{a => 3, b => 2, c => 1}

> % Marking with various tokens
> Marking = #{a => [1,2,3,4], b => [x,y,z], c => [val]}.

> % Enumerate modes (this might take a moment)
> Modes = pnet_mode:enum_modes(Preset, Marking).

> % Should be 4 choose 3 × 3 choose 2 × 1 choose 1 = 4 × 3 × 1 = 12 modes
> ExpectedModes = 4 * 3 * 1.
12
> length(Modes) =:= ExpectedModes.
true
```

## Cartesian Product Enumeration

### Step 7: Understanding the Algorithm

Let's trace through the Cartesian product calculation:

```erlang
> % Manual example: [a,b] from p1, [x,y,z] from p2, [1] from p3
> Marking = #{p1 => [a,b], p2 => [x,y,z], p3 => [1]}.
> Preset = [p1, p2, p3].

> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{p1 => [a],p2 => [x],p3 => [1]},
 #{p1 => [a],p2 => [y],p3 => [1]},
 #{p1 => [a],p2 => [z],p3 => [1]},
 #{p1 => [b],p2 => [x],p3 => [1]},
 #{p1 => [b],p2 => [y],p3 => [1]},
 #{p1 => [b],p2 => [z],p3 => [1]}]

> % 2 (p1) × 3 (p2) × 1 (p3) = 6 modes
> length(Modes).
6
```

**Algorithm Breakdown**:
1. For each place, generate all combinations of the required number of tokens
2. Compute the Cartesian product across all places
3. Each combination becomes a separate mode

### Step 8: Empty Result Cases

The system correctly handles cases where no modes are possible:

```erlang
> % Case 1: Place has insufficient tokens
> Marking = #{p => [a,b]}.
> Preset = [p, p, p].  % Need 3 tokens

> Modes = pnet_mode:enum_modes(Preset, Marking).
[]  % Need 3, only 2 available

> % Case 2: Empty marking
> Marking = #{p => []}.
> Preset = [p].

> Modes = pnet_mode:enum_modes(Preset, Marking).
[]  % No tokens available
```

## Handling Edge Cases

### Step 9: Empty Preset

What happens with an empty preset (no input places)?

```erlang
> % Empty preset - transition with no inputs
> Preset = [].
> Marking = #{any => [anything]}.

> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{ }]

> % Result: one empty mode
> length(Modes).
1
> hd(Modes).
#{}
```

**Explanation**: A transition with no input places always has exactly one mode - the empty mode, representing immediate firing without consuming any tokens.

### Step 10: Duplicate Tokens

What if the marking contains duplicate tokens?

```erlang
> % Marking with duplicate values (allowed in multisets)
> Marking = #{p => [a, a, b]}.
> Preset = [p].

> Modes = pnet_mode:enum_modes(Preset, Marking).
[#{p => [a]},
 #{p => [a]},
 #{p => [b]}]

> % Duplicates create distinct modes
> length(Modes).
3
```

**Note**: This demonstrates that the system treats duplicates as distinct tokens, which is correct for multiset semantics.

## Colored Net Mode Enumeration

### Step 11: Introduction to Colored Modes

Colored Petri nets extend basic nets with data types and variable bindings:

```erlang
% Define a simple colored net
-module(colored_net).
-behaviour(pnet_net).

places() -> [input, output].
transitions() -> [process].

preset(process) -> [input].
preset(_T) -> [].

init(_NetArg) -> [].
init_marking(input, _UsrInfo) -> [{user, alice}, {user, bob}].
init_marking(output, _UsrInfo) -> [].

% Colored mode enumeration with variable binding
cmodes(process, #{input := Users}, _Ctx) ->
    [{#{user => U}, #{input => [U]}} || U <- Users].

fire(process, #{input := [{user, U}]}, _UsrInfo) ->
    {produce, #{output => [{processed, U}]}}.
```

### Step 12: Using enum_cmodes/4

Let's see how colored mode enumeration works:

```erlang
> % Colored mode enumeration
> Marking = #{input => [{user, alice}, {user, bob}]}.
> Modes = pnet_mode:enum_cmodes(process, Marking, #{}, colored_net).

> Modes = [{#{user => alice}, #{input => [{user, alice}]}},
>         {#{user => bob}, #{input => [{user, bob}]}}]

> % Extract just the token portions
> [TokenMode || {_Binding, TokenMode} <- Modes].
[#{input => [{user,alice}]},#{input => [{user,bob}]}]

> % Extract just the bindings
> [Binding || {Binding, _TokenMode} <- Modes].
[#{user => alice},#{user => bob}]
```

### Step 13: Colored Mode with Multiple Variables

Colored modes can handle multiple variables:

```erlang
% Complex colored net
-module(multi_var_net).
-behaviour(pnet_net).

places() -> [orders].
transitions() -> [process_order].

preset(process_order) -> [orders].

init(_NetArg) -> [].
init_marking(orders, _UsrInfo) ->
    [{order, 1, "item", 10.99},
     {order, 2, "item", 24.99}].

cmodes(process_order, #{orders := Orders}, _Ctx) ->
    [{#{id => Id, item => Item, price => Price},
      #{orders => [{order, Id, Item, Price}]}} ||
     {order, Id, Item, Price} <- Orders].

fire(process_order, #{orders := [{order, Id, Item, Price}]}, _Ctx) ->
    {produce, #{processed => [{processed_order, Id, Item, Price}]}}.
```

```erlang
> % Multiple variable colored modes
> Marking = #{orders => [{order,1,"book",12.99}, {order,2,"cd",8.99}]}.
> Modes = pnet_mode:enum_cmodes(process_order, Marking, #{}, multi_var_net).

> Modes = [
>   {#{id => 1, item => "book", price => 12.99},
>    #{orders => [{order,1,"book",12.99}]}},
>   {#{id => 2, item => "cd", price => 8.99},
>    #{orders => [{order,2,"cd",8.99}]}}
> ]
```

### Step 14: Fallback to Basic Enumeration

What if the net module doesn't implement colored modes?

```erlang
> % Net module without cmodes/3
> Marking = #{p => [a,b]}.
> Modes = pnet_mode:enum_cmodes(t1, Marking, ctx, basic_net).

> Modes = [{#{}, #{p => [a]}}, {#{}, #{p => [b]}}]

> % Each colored mode has empty binding and basic mode
> [{Binding || {Binding, _Mode} <- Modes}].
[#{},#{}]

> [Mode || {_Binding, Mode} <- Modes].
[#{p => [a]},#{p => [b]}]
```

## Real-world Examples

### Step 15: User Approval Workflow

Let's model a user approval workflow:

```erlang
% User approval workflow net
-module(approval_net).
-behaviour(pnet_net).

places() -> [requests, approved, rejected].
transitions() -> [approve, reject].

preset(approve) -> [requests].
preset(reject) -> [requests].

init(_NetArg) -> [].
init_marking(requests, #{role := manager}) ->
    [{request, "doc1", user}, {request, "doc2", user}].
init_marking(approved, _Ctx) -> [].
init_marking(rejected, _Ctx) -> [].

cmodes(approve, #{requests := Requests}, #{role := manager}) ->
    [{#{request => R, user => U}, #{requests => [R]} ||
     {request, R, U} <- Requests].

cmodes(reject, #{requests := Requests}, #{role := manager}) ->
    [{#{request => R, user => U}, #{requests => [R]} ||
     {request, R, U} <- Requests].

fire(approve, #{requests := [{request, R, U}]}, #{role := manager}) ->
    {produce, #{approved => [{approved_request, R, U}]}}.
fire(reject, #{requests := [{request, R, U}]}, #{role := manager}) ->
    {produce, #{rejected => [{rejected_request, R, U}]}}.
```

```erlang
> % Approval workflow example
> Marking = #{requests => [{request, "doc1", alice}, {request, "doc2", bob}]}.
> Ctx = #{role => manager}.

> % Enumerate approval modes
> ApproveModes = pnet_mode:enum_cmodes(approve, Marking, Ctx, approval_net).

> ApproveModes = [
>   {#{request => "doc1", user => alice},
>    #{requests => [{request, "doc1", alice}]}},
>   {#{request => "doc2", user => bob},
>    #{requests => [{request, "doc2", bob}]}}
> ]

> % Enumerate reject modes
> RejectModes = pnet_mode:enum_cmodes(reject, Marking, Ctx, approval_net).

> RejectModes = [
>   {#{request => "doc1", user => alice},
>    #{requests => [{request, "doc1", alice}]}},
>   {#{request => "doc2", user => bob},
>    #{requests => [{request, "doc2", bob}]}}
> ]
```

### Step 16: Data Validation Network

Let's model a data validation workflow:

```erlang
% Data validation network
-module(validation_net).
-behaviour(pnet_net).

places() -> [input_data, valid_data, invalid_data].
transitions() -> [validate_data].

preset(validate_data) -> [input_data].

init(_NetArg) -> [].
init_marking(input_data, _Ctx) ->
    [{data, "name", string, "Alice"},
     {data, "age", int, 25},
     {data, "email", string, "invalid"}].
init_marking(valid_data, _Ctx) -> [].
init_marking(invalid_data, _Ctx) -> [].

cmodes(validate_data, #{input_data := Data}, _Ctx) ->
    [{#{field => Field, type => Type, value => Value},
      #{input_data => [{data, Field, Type, Value]}} ||
     {data, Field, Type, Value} <- Data].

fire(validate_data, #{input_data := [{data, Field, Type, Value}]}, _Ctx) ->
    case Type of
        string when is_list(Value) ->
            {produce, #{valid_data => [{validated, Field, Value}]}};
        int when is_integer(Value) ->
            {produce, #{valid_data => [{validated, Field, Value}]}};
        _ ->
            {produce, #{invalid_data => [{error, Field, invalid_type}]}}
    end.
```

```erlang
> % Validation example
> Marking = #{input_data =>
>             [{data, "name", string, "Alice"},
>              {data, "age", int, 25},
>              {data, "email", string, "invalid"}]}.
>
> Modes = pnet_mode:enum_cmodes(validate_data, Marking, #{}, validation_net).
>
> % Each mode represents validating one field
> length(Modes).
3

> % Check field extraction from modes
> [maps:get(field, B) || {B, _} <- Modes].
["name","age","email"]
```

## Integration with Transition Firing

### Step 17: Complete Workflow Example

Let's see how mode enumeration integrates with the full transition firing process:

```erlang
> % Initial state
> InitialMarking = #{start => [init], condition => []}.
>
> % Enumerate modes for transition t1
> Preset = gen_pnet:preset(t1, my_net).
> Modes = pnet_mode:enum_modes(Preset, InitialMarking).
>
> Modes = [#{start => [init]}].
>
> % Fire the transition
> FiringResult = gen_pnet:fire(my_net, t1, Modes, InitialMarking).
>
> % Tokens consumed and produced
> NewMarking = gen_pnet:marking(FiringResult, my_net).
> NewMarking = #{start => [], condition => [check]}.
```

### Step 18: Colored Net Transition Firing

For colored nets, the process is similar but includes variable bindings:

```erlang
> % Colored net initial state
> ColoredMarking = #{input => [{user, alice, "data"}, {user, bob, "data"}]}.
>
> % Enumerate colored modes
> ColoredModes = pnet_mode:enum_cmodes(process, ColoredMarking, #{}, user_net).
>
> % Select one mode (e.g., first one)
> {SelectedBinding, SelectedMode} = hd(ColoredModes).
>
> % Fire with colored mode
> ColoredResult = gen_pnet:fire(user_net, process, ColoredModes, ColoredMarking).
>
> % Binding is used in transition firing
> % Variable binding: #{user => alice} can be used in arc expressions
```

### Step 19: Nondeterministic Mode Selection

In systems with multiple modes, the runtime selects one nondeterministically:

```erlang
> % Multiple modes available
> Marking = #{p1 => [a,b], p2 => [x,y]}.
> Modes = pnet_mode:enum_modes([p1, p2], Marking).
>
> Modes = [#{p1 => [a],p2 => [x]},
>         #{p1 => [a],p2 => [y]},
>         #{p1 => [b],p2 => [x]},
>         #{p1 => [b],p2 => [y]}].
>
> % Runtime selects one mode (e.g., first in deterministic order)
> SelectedMode = hd(Modes).
>
> % Fire with selected mode
> Result = gen_pnet:fire(my_net, transition, [SelectedMode], Marking).
```

## Summary

In this tutorial, we've explored:

1. **Basic mode enumeration** - How to enumerate firing modes for transitions
2. **Preset counting** - Handling multiplicity in input places
3. **Cartesian products** - Understanding how mode combinations work
4. **Edge cases** - Handling insufficient tokens and empty markings
5. **Colored net enumeration** - Variable bindings and pattern matching
6. **Real-world examples** - Workflows, approvals, validation
7. **Integration** - How mode enumeration connects to transition firing

The `pnet_mode` module provides the fundamental capability for nondeterministic firing in Petri nets while maintaining deterministic results for predictable execution. Understanding mode enumeration is key to implementing complex workflow patterns and ensuring correct Petri net behavior.

## Next Steps

1. Experiment with different Petri net patterns
2. Implement your own colored net modules
3. Explore performance optimization techniques
4. Integrate with larger workflow systems

Remember to always test your mode enumerations with realistic data to ensure correctness!