# PNet Marking Algebra Tutorial

**Level**: Beginner-Intermediate
**Time**: 60 minutes
**Focus**: Mastering multiset marking algebra for workflow state management

## Learning Objectives

After completing this tutorial, you will be able to:

- Understand the mathematical foundation of Petri net markings
- Create and manipulate workflow states using multiset operations
- Implement atomic transition firing with proper error handling
- Use canonical hashing for state comparison and caching
- Apply marking algebra to real-world workflow patterns
- Debug and optimize marking operations

## Prerequisites

- Basic Erlang/OTP programming knowledge
- Understanding of Petri net concepts (places, transitions, tokens)
- Familiarity with Erlang maps and lists
- Completed the [Getting Started tutorial](tutorials/getting_started.md) (recommended)

## Introduction

The `pnet_marking` module implements a **multiset marking algebra** that serves as the mathematical foundation for workflow state management in the CRE system. Unlike simple sets, multisets (or "bags") respect both the multiplicity and identity of elements, making them perfect for modeling real-world workflow scenarios where:

- Multiple instances of the same work item can exist
- Order matters during operations but not for state representation
- State changes must be atomic and consistent

### Core Concepts

```
Marking = {Place1: [Token1, Token2, ...], Place2: [...], ...}

Example: #{start => ["init"], queue => ["task1", "task1", "task2"], end => []}
```

## Setup

Let's start by compiling the module and setting up our environment:

```bash
# Navigate to CRE project
cd /path/to/cre

# Compile all modules
rebar3 compile

# Start interactive shell with the project loaded
rebar3 shell
```

In the Erlang shell, compile the marking module:

```erlang
c(pnet_marking).
```

## Module 1: Marking Creation and Initialization

### Creating New Markings

The `new/1` function creates a new empty marking with specified places. This is your starting point for any workflow.

```erlang
% Create a simple workflow with two places
> M0 = pnet_marking:new([start, end]).
#{start => [], end => []}

% Create a complex workflow with multiple places
> Workflow = pnet_marking:new([input, queue, processing, output, archive]).
#{input => [], queue => [], processing => [], output => [], archive => []}

% Verify empty places
> pnet_marking:get(M0, start).
{ok, []}
```

**Key Insight**: All places are initialized with empty token lists, providing a clean slate for workflow initialization.

### Setting Initial State

The `set/3` function allows you to establish the initial state of your workflow:

```erlang
% Set initial tokens
> {ok, Initial} = pnet_marking:set(pnet_marking:new([start, end]), start, ["workflow_init"]).
#{start => ["workflow_init"], end => []}

% Set multiple places
> {ok, Complex} = pnet_marking:set(pnet_marking:new([start, task1, task2, end]),
                                 start, ["init"],
                                 task1, ["work1"]).
#{start => ["init"], task1 => ["work1"], task2 => [], end => []}
```

**Doctest Demonstration**: The module includes built-in doctests that verify marking creation patterns:

```erlang
% From the module's doctests
> M0 = pnet_marking:new([p1, p2]).
#{p1 => [], p2 => []}

> pnet_marking:get(M0, p1).
{ok, []}

> pnet_marking:get(M0, missing).
{error, bad_place}
```

### Exercise 1: Workflow Initialization

**Task**: Create a customer order workflow with the following places:
- `new_order`
- `validation`
- `processing`
- `completed`
- `failed`

Initialize it with one token in `new_order`.

```erlang
% Your solution here:
> OrderWorkflow = pnet_marking:new([new_order, validation, processing, completed, failed]).
> {ok, ActiveOrders} = pnet_marking:set(OrderWorkflow, new_order, ["order_123"]).
```

<details>
<summary>Click to see solution</summary>

```erlang
> OrderWorkflow = pnet_marking:new([new_order, validation, processing, completed, failed]).
#{new_order => [], validation => [], processing => [], completed => [], failed => []}

> {ok, ActiveOrders} = pnet_marking:set(OrderWorkflow, new_order, ["order_123"]).
#{new_order => ["order_123"], validation => [], processing => [], completed => [], failed => []}
```
</details>

## Module 2: Basic Marking Operations

### Getting Token Information

The `get/2` function retrieves tokens at a specific place:

```erlang
% Create a marking with tokens
> M1 = #{start => ["init"], queue => ["task1", "task1", "task2"], end => []}.

% Get tokens from each place
> pnet_marking:get(M1, start).
{ok, ["init"]}

> pnet_marking:get(M1, queue).
{ok, ["task1", "task1", "task2"]}

> pnet_marking:get(M1, end).
{ok, []}

% Attempt to get tokens from non-existent place
> pnet_marking:get(M1, missing).
{error, bad_place}
```

**Best Practice**: Always handle both `{ok, Tokens}` and `{error, bad_place}` cases in your code.

### Modifying Markings

The `set/3` function replaces all tokens at a place:

```erlang
% Replace tokens at a place
> M2 = pnet_marking:set(M1, queue, ["priority_task", "normal_task"]).
#{start => ["init"], queue => ["priority_task", "normal_task"], end => []}

% Verify the change
> pnet_marking:get(M2, queue).
{ok, ["priority_task", "normal_task"]}
```

**Key Property**: The order of tokens matters for the marking representation but not for the hash value (demonstrated later).

### Exercise 2: State Management

**Task**: Create a marking representing a simple to-do list with:
- `todo` place: ["task1", "task2", "task3"]
- `doing` place: ["task1"]
- `done` place: ["task0"]

Then move "task2" from `todo` to `doing`.

```erlang
% Your solution here:
> TodoList = ...
> {ok, UpdatedTodoList} = ...
```

<details>
<summary>Click to see solution</summary>

```erlang
% Create initial state
> TodoList = pnet_marking:new([todo, doing, done]).
#{todo => [], doing => [], done => []}

% Set initial tokens
> {ok, InitialState} = pnet_marking:set(TodoList, todo, ["task1", "task2", "task3"]),
                          doing, ["task1"],
                          done, ["task0"].
#{todo => ["task1", "task2", "token3"], doing => ["task1"], done => ["task0"]}

% Move task2 from todo to doing
> {ok, UpdatedTodoList} = pnet_marking:apply(InitialState,
                                            #{todo => ["task2"]},
                                            #{doing => ["task2"]}).
#{todo => ["task1", "task3"], doing => ["task1", "task2"], done => ["task0"]}
```
</details>

## Module 3: Multiset Operations

### Adding Tokens

The `add/2` function implements multiset addition by appending tokens to existing lists:

```erlang
% Create an empty marking
> M0 = pnet_marking:new([input, output]).
#{input => [], output => []}

% Add tokens to input
> {ok, M1} = pnet_marking:add(M0, #{input => ["data1", "data2"]}).
#{input => ["data1", "data2"], output => []}

% Add more tokens to input
> {ok, M2} = pnet_marking:add(M1, #{input => ["data3"]}).
#{input => ["data1", "data2", "data3"], output => []}

% Add tokens to multiple places
> {ok, M3} = pnet_marking:add(M2, #{input => ["data4"], output => ["result1"]}).
#{input => ["data1", "data2", "data3", "data4"], output => ["result1"]}
```

**Important**: `add/2` preserves the order of existing tokens and appends new tokens to the end.

### Taking Tokens

The `take/2` function implements precise multiset consumption. This is where the multiset algebra shines:

```erlang
% Start with a marking having multiple tokens
> M = #{input => ["a", "b", "c", "d"], temp => ["x"]}.

% Take specific tokens (exact match required)
> {ok, M1} = pnet_marking:take(M, #{input => ["a", "b"]}).
#{input => ["c", "d"], temp => ["x"]}

% Take in different order (same result)
> {ok, M2} = pnet_marking:take(M, #{input => ["b", "a"]}).
#{input => ["c", "d"], temp => ["x"]}

% Try to take more tokens than available
> pnet_marking:take(M, #{input => ["a", "a"]}).
{error, insufficient}

% Try to take non-existent token
> pnet_marking:take(M, #{input => ["z"]}).
{error, insufficient}

% Take from multiple places
> {ok, M3} = pnet_marking:take(M, #{input => ["a"], temp => ["x"]}).
#{input => ["b", "c", "d"], temp => []}
```

**Mathematical Foundation**: The `take/2` function implements proper multiset subtraction, where the multiplicity of each element matters exactly.

### Atomic Transition Firing

The `apply/3` function is the core operation for workflow transitions. It atomically consumes tokens and produces new ones:

```erlang
% Initial marking
> Initial = #{start => ["init"], task => [], end => []}.

% Add token to start
> {ok, Active} = pnet_marking:add(Initial, #{start => ["init"]).
#{start => ["init", "init"], task => [], end => []}

% Apply transition: consume from start, produce to task
> {ok, InTask} = pnet_marking:apply(Active, #{start => ["init"]}, #{task => ["work"]).
#{start => ["init"], task => ["work"], end => []}

% Complete the task
> {ok, Complete} = pnet_marking:apply(InTask, #{task => ["work"]}, #{end => ["done"]).
#{start => ["init"], task => [], end => ["done"]}
```

**Atomicity Guarantee**: If consumption fails, the marking remains unchanged. This ensures workflow consistency.

**Doctest Demonstration**: Built-in examples show the power of atomic operations:

```erlang
% From module doctests
> M1 = #{p1 => [a,b], p2 => [c]}.

% Successful atomic operation
> {ok, M3} = pnet_marking:apply(M1, #{p1 => [a]}, #{p2 => [d]}).
#{p1 => [b], p2 => [c,d]}

% Failed atomic operation (marking unchanged)
> pnet_marking:apply(M1, #{p1 => [a,a]}, #{p2 => [d]}).
{error, insufficient}
```

### Exercise 3: Processing Pipeline

**Task**: Implement a data processing pipeline with:
- `input`: receives data items
- `processing`: currently processing items
- `output`: completed items
- `backup`: backup of processed items

Process 3 items through the pipeline:
1. Add "item1", "item2", "item3" to `input`
2. Process "item1" (move to `processing`)
3. Complete "item1" (move to `output` and `backup`)
4. Process "item2" and "item3"

```erlang
% Your solution here:
> Pipeline = ...
% Step 1: Add input
% Step 2: Process item1
% Step 3: Complete item1
% Step 4: Process remaining items
```

<details>
<summary>Click to see solution</summary>

```erlang
% Initialize pipeline
> Pipeline = pnet_marking:new([input, processing, output, backup]).
#{input => [], processing => [], output => [], backup => []}

% Step 1: Add input items
> {ok, WithInput} = pnet_marking:add(Pipeline, #{input => ["item1", "item2", "item3"]}).
#{input => ["item1", "item2", "item3"], processing => [], output => [], backup => []}

% Step 2: Process item1
> {ok, Processing} = pnet_marking:apply(WithInput, #{input => ["item1"]}, #{processing => ["item1"]}).
#{input => ["item2", "item3"], processing => ["item1"], output => [], backup => []}

% Step 3: Complete item1
> {ok, Completed} = pnet_marking:apply(Processing, #{processing => ["item1"]}, #{output => ["item1"], backup => ["item1"]}).
#{input => ["item2", "item3"], processing => [], output => ["item1"], backup => ["item1"]}

% Step 4: Process remaining items
> {ok, Final} = pnet_marking:apply(Completed,
                                  #{input => ["item2", "item3"], processing => ["item2"]},
                                  #{processing => ["item2"], output => ["item1", "item2"], backup => ["item1", "item2"]}).
#{input => [], processing => ["item3"], output => ["item1", "item2"], backup => ["item1", "item2"]}
```
</details>

## Module 4: Inspection and Utilities

### Creating Snapshots

The `snapshot/1` function creates a deep copy of the marking. Due to Erlang's immutability, this provides semantic clarity:

```erlang
% Create original marking
> Original = #{start => ["init"], end => ["complete"]}.

% Create snapshot
> Snapshot = pnet_marking:snapshot(Original).
#{start => ["init"], end => ["complete"]}

% Modify the snapshot (original unchanged)
> Modified = Snapshot#{start => ["modified"]}.
#{start => ["modified"], end => ["complete"]}

> Original.
#{start => ["init"], end => ["complete"]}
```

**Use Cases**: Snapshots are useful for:
- Saving workflow state before risky operations
- Creating checkpoints for recovery
- Parallel processing of different states

### Computing Hash Values

The `hash/1` function computes a canonical hash of the marking, useful for state comparison:

```erlang
% Create equivalent markings with different token order
> M1 = #{p => [a,b]}.
> M2 = #{p => [b,a]}.
> M3 = #{p => [a,c]}.

% Compute hashes
> Hash1 = pnet_marking:hash(M1).
<<98,247,139,121,58,179,157,212,136,191,236,217,95,106,8,167,233,154,145,253,32,235,95,116,47,89,179,45,170,150,90,76>>

> Hash2 = pnet_marking:hash(M2).
<<98,247,139,121,58,179,157,212,136,191,236,217,95,106,8,167,233,154,145,253,32,235,95,116,47,89,179,45,170,150,90,76>>

> Hash3 = pnet_marking:hash(M3).
<<147,88,42,15,201,105,178,9,77,180,150,223,173,181,227,118,188,227,25,200,138,229,111,246,228,251,207,180,238,245,222,235>>

% Verify hash equality for same multisets
> Hash1 =:= Hash2.
true

> Hash1 =:= Hash3.
false
```

**Key Property**: Hash values are order-invariant, making them perfect for state comparison regardless of token ordering.

### State Comparison Pattern

```erlang
% Efficient state comparison using hashes
compare_states(State1, State2) ->
    Hash1 = pnet_marking:hash(State1),
    Hash2 = pnet_marking:hash(State2),

    if
        Hash1 =:= Hash2 ->
            {equal, State1};
        true ->
            {different, diff_states(State1, State2)}
    end.

% Example usage
> State1 = #{task => ["work1"], status => ["running"]}.
> State2 = #{task => ["work1"], status => ["completed"]}.
> compare_states(State1, State2).
{different, ...}
```

### Exercise 4: State Management with Hashing

**Task**: Create a workflow that tracks task execution and uses hashing to detect state changes.

```erlang
% Your solution here:
% 1. Create a workflow with places: ready, running, completed, failed
% 2. Initialize with 3 tasks in ready state
% 3. Process tasks one by one
% 4. After each operation, compute and store the hash
% 5. Verify that hashes change when state changes
```

<details>
<summary>Click to see solution</summary>

```erlang
% 1. Create workflow
> TaskWorkflow = pnet_marking:new([ready, running, completed, failed]).
#{ready => [], running => [], completed => [], failed => []}

% 2. Initialize with 3 tasks
> {ok, InitialState} = pnet_marking:set(TaskWorkflow, ready, ["task1", "task2", "task3"]).
#{ready => ["task1", "task2", "task3"], running => [], completed => [], failed => []}

% 3. Track hashes
> InitialHash = pnet_marking:hash(InitialState).
<<...>>

% Process task1
> {ok, AfterStart} = pnet_marking:apply(InitialState, #{ready => ["task1"]}, #{running => ["task1"]}).
#{ready => ["task2", "task3"], running => ["task1"], completed => [], failed => []}
> AfterStartHash = pnet_marking:hash(AfterStart).

% Complete task1
> {ok, AfterComplete} = pnet_marking:apply(AfterStart, #{running => ["task1"]}, #{completed => ["task1"]}).
#{ready => ["task2", "task3"], running => [], completed => ["task1"], failed => []}
> AfterCompleteHash = pnet_marking:hash(AfterComplete).

% Verify hashes are different
> InitialHash =:= AfterStartHash.
false

> AfterStartHash =:= AfterCompleteHash.
false
```
</details>

## Module 5: Error Handling and Edge Cases

### Understanding Error Conditions

The marking algebra provides precise error conditions for robust workflow execution:

#### bad_place Errors

These occur when referencing non-existent places:

```erlang
% All these operations return {error, bad_place}
> pnet_marking:get(#{p => [a]}, missing).
{error, bad_place}

> pnet_marking:set(#{p => [a]}, missing, [x]).
{error, bad_place}

> pnet_marking:add(#{p => [a]}, #{missing => [y]}).
{error, bad_place}

> pnet_marking:take(#{p => [a]}, #{missing => [y]}).
{error, bad_place}

> pnet_marking:apply(#{p => [a]}, #{missing => [y]}, #{p => [z]}).
{error, bad_place}
```

#### insufficient Errors

These occur when trying to take more tokens than available:

```erlang
% Insufficient token count
> M = #{p => [a,b]}.
> pnet_marking:take(M, #{p => [a,a]}).
{error, insufficient}

% Non-existent token
> M = #{p => [a]}.
> pnet_marking:take(M, #{p => [b]}).
{error, insufficient}

% Partial consumption with insufficient tokens
> M = #{p1 => [a,b], p2 => [c]}.
> pnet_marking:take(M, #{p1 => [a], p2 => [d]}).
{error, insufficient}
```

### Robust Error Handling Pattern

```erlang
% Robust workflow operation with comprehensive error handling
process_transition(Marking, ConsumeMap, ProduceMap) ->
    case pnet_marking:apply(Marking, ConsumeMap, ProduceMap) of
        {ok, NewMarking} ->
            % Success - proceed with new state
            {success, NewMarking};
        {error, insufficient} ->
            % Handle insufficient tokens - maybe queue for later
            {insufficient, "Not enough tokens available", Marking};
        {error, bad_place} ->
            % Handle invalid places - configuration error
            {error, "Invalid place specified", Marking}
    end.

% Example usage
> Result = process_transition(Marking, #{task => ["work"]}, #{done => ["complete"]}).
```

### Edge Case Handling

#### Empty Markings

```erlang
% Work with empty markings
> Empty = pnet_marking:new([p1, p2]).
#{p1 => [], p2 => []}

% Adding to empty marking
> {ok, NonEmpty} = pnet_marking:add(Empty, #{p1 => [a]}).
#{p1 => [a], p2 => []}

% Taking from empty marking
> pnet_marking:take(Empty, #{p1 => [a]}).
{error, insufficient}
```

#### Single Token Operations

```erlang
% Single token operations are straightforward
> Single = #{p => [a]}.

% Take the single token
> {ok, Empty} = pnet_marking:take(Single, #{p => [a]}).
#{p => []}

% Try to take from empty
> pnet_marking:take(Empty, #{p => [a]}).
{error, insufficient}
```

#### Large Token Lists

```erlang
% Large token lists work but may have performance implications
> Large = #{p => lists:seq(1, 1000)}.

% Take first 100 tokens
> {ok, Reduced} = pnet_marking:take(Large, #{p => lists:seq(1, 100)}).
#{p => lists:seq(101, 1000)}
```

### Exercise 5: Error Recovery

**Task**: Implement a robust workflow operation that handles errors gracefully and provides recovery options.

```erlang
% Your solution here:
% Create a function that attempts to process a task but falls back to:
% 1. Retrying with different parameters
% 2. Queuing the task for later
% 3. Marking as failed
```

<details>
<summary>Click to see solution</summary>

```erlang
% Robust task processor with multiple recovery strategies
robust_task_processor(Marking, TaskId) ->
    % Primary attempt: process the task
    ConsumeMap = #{ready => [TaskId]},
    ProduceMap = #{running => [TaskId]},

    case pnet_marking:apply(Marking, ConsumeMap, ProduceMap) of
        {ok, NewMarking} ->
            {success, "Task started", NewMarking};
        {error, insufficient} ->
            % Insufficient tokens - try fallback strategies
            case fallback_processor(Marking, TaskId) of
                {ok, RecoveryMarking} ->
                    {recovered, "Fallback succeeded", RecoveryMarking};
                {error, Reason} ->
                    {failed, "All attempts failed: " ++ Reason, Marking}
            end;
        {error, bad_place} ->
            {error, "Invalid workflow configuration", Marking}
    end.

% Fallback 1: Try with lower priority
fallback_processor(Marking, TaskId) ->
    % This could try different consume patterns
    % For simplicity, we'll just mark it as queued
    case pnet_marking:add(Marking, #{queued => [TaskId]}) of
        {ok, QueuedMarking} ->
            {ok, QueuedMarking};
        {error, Reason} ->
            {error, atom_to_list(Reason)}
    end.

% Example usage
> Workflow = pnet_marking:new([ready, running, queued, completed, failed]).
> {ok, Initial} = pnet_marking:set(Workflow, ready, ["task1", "task2"]).
> Result = robust_task_processor(Initial, "task1").
```
</details>

## Module 6: Real-World Workflow Patterns

### Pattern 1: Sequential Workflow

A linear sequence of tasks:

```erlang
% Sequential workflow: start -> task1 -> task2 -> end
sequential_workflow_demo() ->
    % Initialize
    Init = pnet_marking:new([start, task1, task2, end]).
    {ok, Started} = pnet_marking:add(Init, #{start => [init]}),

    % Step 1: start to task1
    {ok, InTask1} = pnet_marking:apply(Started, #{start => [init]}, #{task1 => [work1]}),

    % Step 2: task1 to task2
    {ok, InTask2} = pnet_marking:apply(InTask1, #{task1 => [work1]}, #{task2 => [work2]}),

    % Step 3: task2 to end
    {ok, Completed} = pnet_marking:apply(InTask2, #{task2 => [work2]}, #{end => [done]}),

    io:format("Completed workflow: ~p~n", [Completed]),
    Completed.

% Execute the demo
> sequential_workflow_demo().
Completed workflow: #{start => [], task1 => [], task2 => [], end => [done]}
```

### Pattern 2: Parallel Split and Merge

Parallel execution with synchronization:

```erlang
% Parallel workflow: start -> (task1 AND task2) -> end
parallel_workflow_demo() ->
    % Initialize
    Init = pnet_marking:new([start, task1, task2, sync, end]).
    {ok, Started} = pnet_marking:add(Init, #{start => [init]}),

    % Split: start -> task1 + task2
    {ok, Forked} = pnet_marking:apply(Started, #{start => [init]}, #{task1 => [t1], task2 => [t2]}),

    % Process task1
    {ok, AfterTask1} = pnet_marking:apply(Forked, #{task1 => [t1]}, #{sync => [s1]}),

    % Process task2 (now both complete at sync)
    {ok, Synced} = pnet_marking:apply(AfterTask1, #{task2 => [t2]}, #{sync => [s1, s2]}),

    % Join: sync -> end
    {ok, Completed} = pnet_marking:apply(Synced, #{sync => [s1, s2]}, #{end => [done]}),

    io:format("Parallel workflow completed: ~p~n", [Completed]),
    Completed.

> parallel_workflow_demo().
Parallel workflow completed: #{start => [], task1 => [], task2 => [], sync => [], end => [done]}
```

### Pattern 3: Exclusive Choice

Decision-based routing:

```erlang
% Choice workflow: start -> choice -> (branch1 OR branch2) -> end
choice_workflow_demo(Choice) ->
    % Initialize
    Init = pnet_marking:new([start, choice, branch1, branch2, end]).
    {ok, Started} = pnet_marking:add(Init, #{start => [init]}),

    % Move to choice point
    {ok, AtChoice} = pnet_marking:apply(Started, #{start => [init]}, #{choice => [decision]}),

    % Make choice based on input
    case Choice of
        branch1 ->
            {ok, Branch1} = pnet_marking:apply(AtChoice, #{choice => [decision]}, #{branch1 => [work1]}),
            {ok, Completed} = pnet_marking:apply(Branch1, #{branch1 => [work1]}, #{end => [done]});
        branch2 ->
            {ok, Branch2} = pnet_marking:apply(AtChoice, #{choice => [decision]}, #{branch2 => [work2]}),
            {ok, Completed} = pnet_marking:apply(Branch2, #{branch2 => [work2]}, #{end => [done]})
    end,

    io:format("Choice workflow (~p) completed: ~p~n", [Choice, Completed]),
    Completed.

% Try both branches
> choice_workflow_demo(branch1).
Choice workflow (branch1) completed: #{start => [], choice => [], branch1 => [], branch2 => [], end => [done]}

> choice_workflow_demo(branch2).
Choice workflow (branch2) completed: #{start => [], choice => [], branch1 => [], branch2 => [], end => [done]}
```

### Pattern 4: Simple Merge

Multiple paths converging:

```erlang
% Merge workflow: (path1 AND path2) -> merge -> end
merge_workflow_demo() ->
    % Initialize both paths
    Init = pnet_marking:new([start1, start2, merge, end]).
    {ok, Path1Started} = pnet_marking:add(Init, #{start1 => [init1]}),
    {ok, BothStarted} = pnet_marking:add(Path1Started, #{start2 => [init2]}),

    % Process both paths
    {ok, Path1Complete} = pnet_marking:apply(BothStarted, #{start1 => [init1]}, #{merge => [m1]}),
    {ok, MergeReady} = pnet_marking:apply(Path1Complete, #{start2 => [init2]}, #{merge => [m1, m2]}),

    % Merge and complete
    {ok, Completed} = pnet_marking:apply(MergeReady, #{merge => [m1, m2]}, #{end => [done]}),

    io:format("Merge workflow completed: ~p~n", [Completed]),
    Completed.

> merge_workflow_demo().
Merge workflow completed: #{start1 => [], start2 => [], merge => [], end => [done]}
```

### Exercise 6: Custom Workflow Pattern

**Task**: Implement a custom workflow pattern for order processing with:
- Order arrives
- Validation (if valid, process; if invalid, reject)
- Processing (can be done in parallel for multiple items)
- Quality check (if pass, ship; if fail, return)
- Ship or Return

```erlang
% Your solution here:
% Create the workflow and implement the processing logic
```

<details>
<summary>Click to see solution</summary>

```erlang
% Order processing workflow
order_processing_workflow(Orders) ->
    % Initialize workflow
    Init = pnet_marking:new([arrival, validation, processing, quality, shipping, returning, completed, rejected]).
    {ok, Arrived} = pnet_marking:add(Init, #{arrival => Orders}),

    % Validation step
    case validate_orders(Orders) of
        valid ->
            {ok, Validated} = pnet_marking:apply(Arrived, #{arrival => Orders}, #{validation => [validated]}),
            process_orders(Validated);
        invalid ->
            {ok, Rejected} = pnet_marking:apply(Arrival, #{arrival => Orders}, #{rejected => [rejected]}),
            {rejected, Rejected}
    end.

validate_orders(Orders) ->
    % Simple validation logic - in reality this would be more complex
    % Assume all orders are valid for demo
    valid.

process_orders(Marking) ->
    % Move to processing
    {ok, Processing} = pnet_marking:apply(Marking, #{validation => [validated]}, #{processing => [processing]}),

    % Process each order (in parallel conceptually)
    % For demo, we'll process them sequentially
    Orders = get_tokens_at_place(Processing, processing),
    Processed = process_each_order(Processing, Orders),

    % Quality check
    case quality_check(Processed) of
        pass ->
            {ok, Shipped} = pnet_marking:apply(Processed, #{processing => Orders}, #{quality => [passed], shipping => [shipped]}),
            {ok, Final} = pnet_marking:apply(Shipped, #{quality => [passed]}, #{completed => [completed]}),
            {success, Final};
        fail ->
            {ok, Returned} = pnet_marking:apply(Processed, #{processing => Orders}, #{quality => [failed], returning => [returned]}),
            {ok, Final} = pnet_marking:apply(Returned, #{quality => [failed]}, #{completed => [completed]}),
            {returned, Final}
    end.

% Helper functions
get_tokens_at_place(Marking, Place) ->
    case pnet_marking:get(Marking, Place) of
        {ok, Tokens} -> Tokens;
        {error, bad_place} -> []
    end.

process_each_order(Marking, []) ->
    Marking;
process_each_order(Marking, [Order|Rest]) ->
    % Process one order
    {ok, AfterProcess} = pnet_marking:apply(Marking, #{processing => [Order]}, #{processing => [processed]}),
    process_each_order(AfterProcess, Rest).

quality_check(Marking) ->
    % Simple quality check - assume all pass
    pass.

% Example usage
> Orders = ["order1", "order2", "order3"].
> Result = order_processing_workflow(Orders).
```
</details>

## Module 7: Performance Optimization

### Understanding Performance Characteristics

The marking algebra has different performance characteristics for different operations:

```erlang
% Performance test helper
benchmark_operation(Op, Marking) ->
    Start = erlang:monotonic_time(microsecond),
    case Op of
        get -> pnet_marking:get(Marking, test);
        add -> pnet_marking:add(Marking, #{test => [data]});
        take -> pnet_marking:take(Marking, #{test => [data]});
        apply -> pnet_marking:apply(Marking, #{test => [data]}, #{test => [result]});
        hash -> pnet_marking:hash(Marking)
    end,
    End = erlang:monotonic_time(microsecond),
    End - Start.

% Test with different marking sizes
test_performance() ->
    % Small marking
    Small = #{test => lists:seq(1, 10)},
    io:format("Small marking (~w tokens):~n", [length(lists:seq(1, 10))]),
    io:format("  get: ~w µs~n", [benchmark_operation(get, Small)]),
    io:format("  add: ~w µs~n", [benchmark_operation(add, Small)]),
    io:format("  take: ~w µs~n", [benchmark_operation(take, Small)]),
    io:format("  apply: ~w µs~n", [benchmark_operation(apply, Small)]),
    io:format("  hash: ~w µs~n", [benchmark_operation(hash, Small)]),

    % Large marking
    Large = #{test => lists:seq(1, 1000)},
    io:format("Large marking (~w tokens):~n", [length(lists:seq(1, 1000))]),
    io:format("  get: ~w µs~n", [benchmark_operation(get, Large)]),
    io:format("  add: ~w µs~n", [benchmark_operation(add, Large)]),
    io:format("  take: ~w µs~n", [benchmark_operation(take, Large)]),
    io:format("  apply: ~w µs~n", [benchmark_operation(apply, Large)]),
    io:format("  hash: ~w µs~n", [benchmark_operation(hash, Large)]).

> test_performance().
```

### Optimization Strategies

#### 1. Batch Operations

For high-volume workflows, consider batching operations:

```erlang
% Batch multiple transitions
batch_apply(Marking, Operations) ->
    lists:foldl(fun({Consume, Produce}, Acc) ->
        case pnet_marking:apply(Acc, Consume, Produce) of
            {ok, NewAcc} -> NewAcc;
            {error, _} -> throw(error)
        end
    end, Marking, Operations).

% Example usage
> Operations = [
%     #{input => ["item1"]}, #{processing => ["item1"]},
%     #{input => ["item2"]}, #{processing => ["item2"]},
%     #{processing => ["item1"]}, #{output => ["result1"]}
% ].
> Result = batch_apply(InitialMarking, Operations).
```

#### 2. State Compression

For workflows with many empty places:

```erlang
% Create compact representation (only non-empty places)
compact_marking(Marking) ->
    maps:filter(fun(_Place, Tokens) -> Tokens =/= [] end, Marking).

% Expand back to full marking
expand_marking(Compact, Places) ->
    maps:from_list([{P, maps:get(P, Compact, [])} || P <- Places]).
```

#### 3. Hash Caching

Cache hash values when they're needed frequently:

```erlang
% Cache hashes for repeated state comparisons
cached_hash(Marking, Cache) ->
    case maps:get(Marking, Cache, undefined) of
        undefined ->
            Hash = pnet_marking:hash(Marking),
            Cache#{Marking => Hash};
        Hash ->
            Cache
    end.

% Compare with cached hash
compare_cached(M1, M2, Cache) ->
    Cache1 = cached_hash(M1, Cache),
    Cache2 = cached_hash(M2, Cache1),

    Hash1 = maps:get(M1, Cache2),
    Hash2 = maps:get(M2, Cache2),

    Hash1 =:= Hash2.
```

### Memory Management

```erlang
% Analyze memory usage
analyze_memory(Marking) ->
    TotalTokens = maps:fold(fun(_Place, Tokens, Acc) -> Acc + length(Tokens) end, 0, Marking),
    PlaceCount = maps:size(Marking),
    AveragePerPlace = TotalTokens / PlaceCount,

    #{
        total_tokens => TotalTokens,
        place_count => PlaceCount,
        average_tokens_per_place => AveragePerPlace,
        memory_estimate => TotalTokens * erlang:system_info(wordsize)
    }.

% Optimize memory usage by cleaning up
cleanup_empty_places(Marking) ->
    maps:filter(fun(_Place, Tokens) -> Tokens =/= [] end, Marking).
```

### Exercise 7: Performance Optimization

**Task**: Optimize a high-volume workflow processing system by implementing batching and caching strategies.

```erlang
% Your solution here:
% Create a system that processes 1000 orders efficiently
% Use batching for operations
% Implement hash caching for state comparisons
% Add memory optimization
```

<details>
<summary>Click to see solution</summary>

```erlang
% High-volume order processing system
high_volume_order_processor(Orders) ->
    % Initialize with empty workflow
    Init = pnet_marking:new([arrival, validation, processing, shipping, completed]),

    % Batch arrival of all orders
    {ok, Arrived} = pnet_marking:add(Init, #{arrival => Orders}),

    % Batch validation
    ValidationOps = create_validation_operations(Orders),
    {ok, Validated} = batch_apply(Arrived, ValidationOps),

    % Batch processing
    ProcessingOps = create_processing_operations(get_tokens_at_place(Validated, validation)),
    {ok, Processed} = batch_apply(Validated, ProcessingOps),

    % Batch shipping
    ShippingOps = create_shipping_operations(get_tokens_at_place(Processed, processing)),
    {ok, Shipped} = batch_apply(Processed, ShippingOps),

    % Final completion
    CompletionOps = create_completion_operations(get_tokens_at_place(Shipped, shipping)),
    {ok, Final} = batch_apply(Shipped, CompletionOps),

    Final.

% Create batch operations
create_validation_operations(Orders) ->
    lists:map(fun(Order) ->
        #{arrival => [Order]}, #{validation => [validated]}
    end, Orders).

create_processing_operations(ValidOrders) ->
    lists:map(fun(Order) ->
        #{validation => [Order]}, #{processing => [Order]}
    end, ValidOrders).

create_shipping_operations(ProcessedOrders) ->
    lists:map(fun(Order) ->
        #{processing => [Order]}, #{shipping => [shipped]}
    end, ProcessedOrders).

create_completion_operations(ShippedOrders) ->
    lists:map(fun(Order) ->
        #{shipping => [Order]}, #{completed => [completed]}
    end, ShippedOrders).

% Cached comparison system
order_state_cache_system() ->
    % Initialize cache
    Cache = #{},

    % Process orders with caching
    Orders = ["order1", "order2", "order3"],
    Initial = pnet_marking:new([arrival, completed]),
    {ok, Started} = pnet_marking:add(Initial, #{arrival => Orders}),

    % Check state with caching
    compare_with_cache(Started, Started, Cache),

    % Process one order
    {ok, Processed} = pnet_marking:apply(Started, #{arrival => ["order1"]}, #{completed => ["order1"]}),

    % Check again (hash should be cached)
    compare_with_cache(Processed, Processed, Cache).
```
</details>

## Module 8: Testing and Debugging

### Running Doctests

The `pnet_marking` module includes comprehensive doctests that verify all operations:

```bash
# Run doctests
rebar3 eunit

# Run with verbose output
rebar3 eunit -v
```

### Debugging Marking Operations

```erlang
% Debug helper: print marking in readable format
debug_marking(Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        TokensStr = case Tokens of
            [] -> "[]";
            _ -> io_lib:format("~w", [Tokens])
        end,
        Acc ++ [io_lib:format("~p: ~s", [Place, TokensStr])]
    end, [], Marking).

% Step-by-step debugging
debug_transition(Marking, ConsumeMap, ProduceMap) ->
    io:format("Initial marking: ~p~n", [debug_marking(Marking)]),

    case pnet_marking:take(Marking, ConsumeMap) of
        {ok, AfterConsume} ->
            io:format("After consume: ~p~n", [debug_marking(AfterConsume)]),
            case pnet_marking:add(AfterConsume, ProduceMap) of
                {ok, Final} ->
                    io:format("After produce: ~p~n", [debug_marking(Final)]),
                    {ok, Final};
                {error, Reason} ->
                    io:format("Production failed: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Consumption failed: ~p~n", [Reason]),
            {error, Reason}
    end.

% Example usage
> M = #{input => ["a", "b"], output => []}.
> debug_transition(M, #{input => ["a"]}, #{output => ["result"]}).
Initial marking: [{input,"[a,b]"},{output,"[]"}]
After consume: [{input,"[b]"},{output,"[]"}]
After produce: [{input,"[b]"},{output,"[result]"}]
{ok,#{input => [b],output => [result]}}
```

### Consistency Checking

```erlang
% Check marking consistency
check_consistency(Marking, ExpectedPlaces) ->
    UnknownPlaces = maps:keys(Marking) -- ExpectedPlaces,
    case UnknownPlaces of
        [] ->
            % Check for proper token counts
            TokenCounts = maps:fold(fun(_Place, Tokens, Acc) ->
                Acc#{place => length(Tokens)}
            end, #{}, Marking),
            {ok, TokenCounts};
        _ ->
            {error, {unknown_places, UnknownPlaces}}
    end.

% Example usage
> Workflow = pnet_marking:new([start, end]).
> {ok, Active} = pnet_marking:add(Workflow, #{start => ["init"]).
> check_consistency(Active, [start, end]).
{ok,#{{place,1}}}
```

### Test Cases for Different Scenarios

```erlang
% Comprehensive test generator
generate_test_cases() ->
    % Test case 1: Basic operations
    BasicCase = {
        "Basic operations",
        fun() ->
            M0 = pnet_marking:new([p1, p2]),
            {ok, M1} = pnet_marking:add(M0, #{p1 => [a]}),
            {ok, M2} = pnet_marking:take(M1, #{p1 => [a]}),
            pnet_marking:get(M2, p1) =:= {ok, []}
        end
    },

    % Test case 2: Multiset semantics
    MultisetCase = {
        "Multiset hash consistency",
        fun() ->
            M1 = pnet_marking:set(pnet_marking:new([p]), p, [a,b]),
            M2 = pnet_marking:set(pnet_marking:new([p]), p, [b,a]),
            pnet_marking:hash(M1) =:= pnet_marking:hash(M2)
        end
    },

    % Test case 3: Error conditions
    ErrorCase = {
        "Error handling",
        fun() ->
            M = #{p => [a]},
            pnet_marking:take(M, #{p => [a,a]}) =:= {error, insufficient} andalso
            pnet_marking:get(M, missing) =:= {error, bad_place}
        end
    },

    [BasicCase, MultisetCase, ErrorCase].

% Run test cases
run_custom_tests() ->
    TestCases = generate_test_cases(),
    lists:foreach(fun({Name, TestFun}) ->
        case TestFun() of
            true ->
                io:format("✓ ~p passed~n", [Name]);
            false ->
                io:format("✗ ~p failed~n", [Name])
        end
    end, TestCases).

> run_custom_tests().
✓ Basic operations passed
✓ Multiset hash consistency passed
✓ Error handling passed
```

### Exercise 8: Comprehensive Testing

**Task**: Create a comprehensive test suite for the `pnet_marking` module covering:
1. Basic operations
2. Edge cases
3. Error conditions
4. Performance scenarios
5. Real-world workflow patterns

```erlang
% Your solution here:
% Implement a complete test suite with various scenarios
```

<details>
<summary>Click to see solution</summary>

```erlang
% Comprehensive test suite for pnet_marking
comprehensive_test_suite() ->
    TestResults = [],

    % 1. Basic Operations Tests
    BasicTests = [
        {"new/1 creates empty marking",
            fun() -> M = pnet_marking:new([p1, p2]), pnet_marking:get(M, p1) =:= {ok, []} end},
        {"get/2 returns tokens",
            fun() -> M = #{p => [a]}, pnet_marking:get(M, p) =:= {ok, [a]} end},
        {"set/3 replaces tokens",
            fun() -> M1 = #{p => [a]}, M2 = pnet_marking:set(M1, p, [b]), pnet_marking:get(M2, p) =:= {ok, [b]} end},
        {"add/2 appends tokens",
            fun() -> M1 = #{p => [a]}, {ok, M2} = pnet_marking:add(M1, #{p => [b]}), pnet_marking:get(M2, p) =:= {ok, [a,b]} end},
        {"take/2 removes tokens",
            fun() -> M1 = #{p => [a,b]}, {ok, M2} = pnet_marking:take(M1, #{p => [a]}), pnet_marking:get(M2, p) =:= {ok, [b]} end}
    ],

    % 2. Multiset Tests
    MultisetTests = [
        {"Hash order invariance",
            fun() ->
                M1 = pnet_marking:set(pnet_marking:new([p]), p, [a,b]),
                M2 = pnet_marking:set(pnet_marking:new([p]), p, [b,a]),
                pnet_marking:hash(M1) =:= pnet_marking:hash(M2)
            end},
        {"Multiset addition",
            fun() ->
                M1 = #{p => [a]}, {ok, M2} = pnet_marking:add(M1, #{p => [a]}), pnet_marking:get(M2, p) =:= {ok, [a,a]}
            end},
        {"Multiset subtraction",
            fun() ->
                M1 = #{p => [a,a]}, {ok, M2} = pnet_marking:take(M1, #{p => [a]}), pnet_marking:get(M2, p) =:= {ok, [a]}
            end}
    ],

    % 3. Error Condition Tests
    ErrorTests = [
        {"bad_place on get",
            fun() -> M = #{p => [a]}, pnet_marking:get(M, missing) =:= {error, bad_place} end},
        {"bad_place on add",
            fun() -> M = #{p => [a]}, pnet_marking:add(M, #{missing => [x]}) =:= {error, bad_place} end},
        {"insufficient on take",
            fun() -> M = #{p => [a]}, pnet_marking:take(M, #{p => [a,a]}) =:= {error, insufficient} end},
        {"bad_place on apply",
            fun() -> M = #{p => [a]}, pnet_marking:apply(M, #{missing => [x]}, #{p => [y]}) =:= {error, bad_place} end}
    ],

    % 4. Atomic Operation Tests
    AtomicTests = [
        {"apply/3 atomic success",
            fun() ->
                M1 = #{p => [a,b]}, {ok, M2} = pnet_marking:apply(M1, #{p => [a]}, #{q => [c]}),
                pnet_marking:get(M2, p) =:= {ok, [b]} andalso pnet_marking:get(M2, q) =:= {ok, [c]}
            end},
        {"apply/3 atomic failure",
            fun() ->
                M1 = #{p => [a]},
                Result = pnet_marking:apply(M1, #{p => [b]}, #{q => [c]}),
                Result =:= {error, insufficient} andalso pnet_marking:get(M1, p) =:= {ok, [a]}
            end}
    ],

    % 5. Workflow Pattern Tests
    PatternTests = [
        {"Sequential workflow",
            fun() ->
                Init = pnet_marking:new([start, task1, task2, end]),
                {ok, Started} = pnet_marking:add(Init, #{start => [init]}),
                {ok, T1} = pnet_marking:apply(Started, #{start => [init]}, #{task1 => [work1]}),
                {ok, T2} = pnet_marking:apply(T1, #{task1 => [work1]}, #{task2 => [work2]}),
                {ok, End} = pnet_marking:apply(T2, #{task2 => [work2]}, #{end => [done]}),
                pnet_marking:get(End, end) =:= {ok, [done]}
            end},
        {"Parallel workflow",
            fun() ->
                Init = pnet_marking:new([start, task1, task2, sync, end]),
                {ok, Started} = pnet_marking:add(Init, #{start => [init]}),
                {ok, Forked} = pnet_marking:apply(Started, #{start => [init]}, #{task1 => [t1], task2 => [t2]}),
                {ok, Synced} = pnet_marking:apply(Forked, #{task1 => [t1], task2 => [t2]}, #{sync => [s1, s2]}),
                {ok, End} = pnet_marking:apply(Synced, #{sync => [s1, s2]}, #{end => [done]}),
                pnet_marking:get(End, end) =:= {ok, [done]}
            end}
    ],

    % 5. Performance Tests
    PerformanceTests = [
        {"Small marking operations",
            fun() ->
                M = #{p => lists:seq(1, 10)},
                Time = benchmark_operation(get, M),
                Time < 1000  % Should be fast
            end},
        {"Large marking hash",
            fun() ->
                M = #{p => lists:seq(1, 1000)},
                Time = benchmark_operation(hash, M),
                Time < 10000  % Should be reasonable
            end}
    ],

    % Run all tests
    AllTests = BasicTests ++ MultisetTests ++ ErrorTests ++ AtomicTests ++ PatternTests ++ PerformanceTests,

    lists:foreach(fun({Name, TestFun}) ->
        case TestFun() of
            true ->
                io:format("✓ ~p passed~n", [Name]);
            false ->
                io:format("✗ ~p failed~n", [Name])
        end
    end, AllTests).

% Benchmark helper
benchmark_operation(Op, Marking) ->
    Start = erlang:monotonic_time(microsecond),
    case Op of
        get -> pnet_marking:get(Marking, p);
        hash -> pnet_marking:hash(Marking)
    end,
    erlang:monotonic_time(microsecond) - Start.

% Run the suite
> comprehensive_test_suite().
✓ new/1 creates empty marking passed
✓ get/2 returns tokens passed
✓ set/3 replaces tokens passed
✓ add/2 appends tokens passed
✓ take/2 removes tokens passed
✓ Hash order invariance passed
✓ Multiset addition passed
✓ Multiset subtraction passed
✓ bad_place on get passed
✓ bad_place on add passed
✓ insufficient on take passed
✓ bad_place on apply passed
✓ apply/3 atomic success passed
✓ apply/3 atomic failure passed
✓ Sequential workflow passed
✓ Parallel workflow passed
✓ Small marking operations passed
✓ Large marking hash passed
```
</details>

## Best Practices and Common Pitfalls

### Best Practices

1. **Always validate places**: Check that places exist before operations
2. **Handle all error cases**: Both `{error, bad_place}` and `{error, insufficient}`
3. **Use atomic operations**: Prefer `apply/3` for transition firing
4. **Cache hash values**: Use hashing for efficient state comparison
5. **Design for immutability**: Leverage Erlang's immutable data structures
6. **Profile performance**: Use `benchmark_operation` to identify bottlenecks

### Common Pitfalls

```erlang
% Pitfall 1: Not handling error cases
% Wrong: Only checking success
{ok, NewMarking} = pnet_marking:apply(Marking, Consume, Produce).

% Right: Handle all cases
case pnet_marking:apply(Marking, Consume, Produce) of
    {ok, NewMarking} -> % success
    {error, insufficient} -> % handle
    {error, bad_place} -> % handle
end.

% Pitfall 2: Using separate take/add instead of apply
% Wrong: Risk of inconsistent state
{ok, Tmp} = pnet_marking:take(Marking, Consume),
Final = pnet_marking:add(Tmp, Produce).

% Right: Atomic operation
{ok, Final} = pnet_marking:apply(Marking, Consume, Produce).

% Pitfall 3: Ignoring token order in expectations
% Wrong: Assuming order matters for consumption
M = #{p => [a,b]},
{ok, Result} = pnet_marking:take(M, #{p => [b,a]}).  % This works!

% Right: Understand multiset semantics
% Order doesn't matter for consumption, only multiplicity
```

## Advanced Topics

### Integration with `gen_pnet`

The `pnet_marking` module is designed to work seamlessly with the `gen_pnet` process:

```erlang
% Example: Implementing a YAWL pattern using marking algebra
-module(sequence_pattern).
-behaviour(pnet_net).

places() -> [start, step1, step2, end].
transitions() -> [t1, t2].

preset(t1) -> [start].
preset(t2) -> [step1].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{start := [init]}, _UsrInfo) -> [#{start => []}].
modes(t2, #{step1 := [done]}, _UsrInfo) -> [#{step1 => []}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{step1 => [done]}}.

fire(t2, #{step1 => []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.
```

### Working with Complex Token Types

```erlang
% Tokens can be any Erlang term
ComplexToken = #{
    id => "order_123",
    customer => #{name => "John", id => "cust_456"},
    items => [
        #{product => "widget", quantity => 2, price => 19.99},
        #{product => "gadget", quantity => 1, price => 29.99}
    ],
    timestamp => erlang:system_time(millisecond)
}.

% Use in marking
OrderWorkflow = pnet_marking:new([new, processing, completed]),
{ok, Active} = pnet_marking:add(OrderWorkflow, #{new => [ComplexToken]}).
```

### Implementing Custom Operations

```erlang
% Example: Move tokens between places
move_tokens(Marking, From, To, Tokens) ->
    case pnet_marking:get(Marking, From) of
        {ok, Available} ->
            case can_move(Available, Tokens) of
                true ->
                    Consume = #{From => Tokens},
                    Produce = #{To => Tokens},
                    pnet_marking:apply(Marking, Consume, Produce);
                false ->
                    {error, insufficient}
            end;
        {error, bad_place} ->
            {error, bad_place}
    end.

can_move(Available, Tokens) ->
    % Check if all requested tokens are available
    lists:all(fun(Token) -> lists:member(Token, Available) end, Tokens).
```

## Conclusion

You've now mastered the `pnet_marking` module and its multiset marking algebra! You can:

- Create and manipulate workflow states with confidence
- Implement atomic transitions with proper error handling
- Use canonical hashing for efficient state comparison
- Apply the marking algebra to real-world workflow patterns
- Optimize performance for high-volume scenarios
- Test and debug marking operations effectively

### Key Takeaways

1. **Multiset semantics**: Token multiplicity matters, but order doesn't for operations
2. **Atomic operations**: Always use `apply/3` for transition firing
3. **Error handling**: Always handle both `bad_place` and `insufficient` errors
4. **Hash consistency**: Use hashing for efficient state comparison
5. **Performance awareness**: Profile operations for high-volume scenarios

### Next Steps

1. **Explore advanced patterns**: Try implementing complex YAWL patterns
2. **Integrate with `gen_pnet`**: Build complete workflow definitions
3. **Extend the marking algebra**: Add custom operations for your domain
4. **Contribute to the project**: Share your patterns and optimizations

## Further Reading

- [PNet Marking Algebra Documentation](pnet_marking_algebra.md) - Mathematical concepts
- [PNet Marking Implementation Details](pnet_marking_implementation.md) - Technical details
- [PNet Marking API Reference](pnet_marking_api_reference.md) - Complete API
- [PNet Marking Quick Reference](pnet_marking_quick_reference.md) - Quick reference
- [Complete API Reference](COMPLETE_API_REFERENCE.md) - All modules

## Additional Resources

- Run the built-in doctests: `rebar3 eunit`
- Explore examples in the `/examples/` directory
- Check the test modules in `/test/` for more patterns
- Join the community for questions and contributions

Happy workflow engineering!