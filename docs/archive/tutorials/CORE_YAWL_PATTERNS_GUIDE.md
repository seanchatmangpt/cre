# Core Permutations Guide - 80/20 Documentation

> This guide covers the 20% of permutation functionality that handles 80% of real-world usage scenarios in CRE/YAWL workflow patterns.

## Table of Contents
- [Basic Permutation Operations](#basic-permutation-operations)
- [Common Permutation Patterns](#common-permutation-patterns)
- [Essential Use Cases](#essential-use-cases)
- [Error Handling](#error-handling)
- [Performance Considerations](#performance-considerations)

---

## Basic Permutation Operations

### 1. Marking Creation and Basic Operations

**Creating a basic marking:**
```erlang
% Create empty marking with places
M0 = pnet_marking:new([start, task1, task2, end]),
io:format("Initial marking: ~p~n", [M0]).
% #{start => [], task1 => [], task2 => [], end => []}
```

**Setting and getting tokens:**
```erlang
% Add tokens to places
M1 = pnet_marking:set(M0, start, [init_token]),
{ok, Tokens} = pnet_marking:get(M1, start),
io:format("Start tokens: ~p~n", [Tokens]).
% [init_token]
```

### 2. Basic Transition Firing

**Single transition firing:**
```erlang
% Define a move (transition firing)
Move = #{mode => #{start => [init_token]},
         produce => #{task1 => [task_token1]}}.

% Apply the move
{ok, M2} = pnet_marking:apply(M1, Move),
{ok, TaskTokens} = pnet_marking:get(M2, task1),
io:format("After firing: ~p~n", [TaskTokens]).
% [task_token1]
```

**Complete sequential workflow:**
```erlang
% Sequential workflow: start -> task1 -> task2 -> end
SequentialMove = [
    #{mode => #{start => [init_token]}, produce => #{task1 => [t1]}},
    #{mode => #{task1 => [t1]}, produce => #{task2 => [t2]}},
    #{mode => #{task2 => [t2]}, produce => #{end => [complete]}}
].

% Execute sequence step by step
FinalMarking = lists:foldl(fun(Move, AccMarking) ->
    {ok, NewMarking} = pnet_marking:apply(AccMarking, Move),
    NewMarking
end, M0, SequentialMove),

{ok, FinalTokens} = pnet_marking:get(FinalMarking, end),
io:format("Final state: ~p~n", [FinalTokens]).
% [complete]
```

---

## Common Permutation Patterns

### 1. Sequential Pattern (WCP-01)

**Pattern description:** Linear execution of tasks in order.

**Implementation:**
```erlang
% Create sequential workflow
sequence_pattern() ->
    #{
        places => [p_start, p_task1, p_task2, p_end],
        transitions => [t_start, t_task1, t_task2],
        initial_marking => #{p_start => [start]},
        preset => #{
            t_start => [p_start],
            t_task1 => [p_task1],
            t_task2 => [p_task2]
        },
        postset => #{
            t_start => [p_task1],
            t_task1 => [p_task2],
            t_task2 => [p_end]
        }
    }.

% Execute sequential pattern
execute_sequential() ->
    Pattern = sequence_pattern(),
    InitialMarking = maps:get(initial_marking, Pattern),

    % Fire transitions in order
    Marking1 = fire_transition(Pattern, t_start, InitialMarking),
    Marking2 = fire_transition(Pattern, t_task1, Marking1),
    Marking3 = fire_transition(Pattern, t_task2, Marking2),

    {ok, Marking3}.
```

### 2. Parallel Split Pattern (WCP-02)

**Pattern description:** Split into concurrent branches that execute simultaneously.

**Implementation:**
```erlang
% Simple parallel execution with 2 branches
parallel_execution(BranchFuns, InputData) ->
    BranchCount = length(BranchFuns),
    Ref = make_ref(),
    Parent = self(),

    % Spawn all branches in parallel
    Pids = lists:map(fun({Fun, Index}) ->
        spawn(fun() ->
            Result = Fun(InputData),
            Parent ! {Ref, {branch_complete, Index}, Result}
        end)
    end, lists:zip(BranchFuns, lists:seq(1, BranchCount))),

    % Wait for all branches to complete
    wait_all_results(Ref, Pids, BranchCount, #{}).

% Example usage
parallel_demo() ->
    Fun1 = fun(X) -> X + 10 end,
    Fun2 = fun(X) -> X * 2 end,
    parallel_execution([Fun1, Fun2], 5).
    % Returns {ok, #{1 => 15, 2 => 10}}
```

### 3. Exclusive Choice Pattern (WCP-04)

**Pattern description:** Select one path from multiple options based on conditions.

**Implementation:**
```

```erlang
exclusive_choice(Condition, Options) ->
    % Condition determines which path to take
    case Condition of
        value1 -> execute_path(Options, path1);
        value2 -> execute_path(Options, path2);
        _ -> execute_default(Options)
    end.

% Example: Order processing based on amount
process_order(OrderAmount) ->
    case OrderAmount of
        Amount when Amount > 1000 ->
            process_premium_order(OrderAmount);
        Amount when Amount > 100 ->
            process_standard_order(OrderAmount);
        _ ->
            process_basic_order(OrderAmount)
    end.
```

### 4. Simple Merge Pattern (WCP-05)

**Pattern description:** Multiple paths merge into a single path (OR-join).

**Implementation:**
```erlang
% Merge results from multiple possible paths
merge_results(Path1Result, Path2Result, Path3Result) ->
    Results = [Path1Result, Path2Result, Path3Result],
    % Combine results, removing duplicates if needed
    Combined = lists:flatten(Results),
    % Process merged data
    process_merged_data(Combined).

% Example: Collect data from different sources
collect_user_data(Source) ->
    Data = case Source of
        database -> fetch_from_db();
        api -> call_external_api();
        cache -> get_from_cache()
    end,

    % Merge with existing data
    merge_results(existing_data(), Data).
```

---

## Essential Use Cases

### 1. Basic Approval Workflow

**Use case:** Multi-step approval process with manual review.

```erlang
-define(APPROVAL_TIMEOUT, 30000).

% Define approval workflow
approval_workflow(Data) ->
    Steps = [
        #{
            id => initial_review,
            function => fun initial_validation/1,
            approval_required => false
        },
        #{
            id => manager_approval,
            function => fun manager_review/1,
            approval_required => true,
            timeout => ?APPROVAL_TIMEOUT
        },
        #{
            id => final_processing,
            function => fun finalize/1,
            approval_required => false
        }
    ],

    execute_approval_steps(Steps, Data).

% Execute approval steps
execute_approval_steps([Step | Rest], Data) ->
    case execute_step(Step, Data) of
        {approved, NewData} when Rest =/= [] ->
            execute_approval_steps(Rest, NewData);
        {approved, FinalData} ->
            {ok, completed, FinalData};
        {denied, Reason} ->
            {error, denied, Reason};
        {error, Reason} ->
            {error, failed, Reason}
    end.
```

### 2. Parallel Data Processing

**Use case:** Process multiple data streams concurrently.

```erlang
% Parallel data processing pipeline
parallel_data_processing(InputList) ->
    % Split data into chunks
    ChunkSize = length(InputList) div 4,
    Chunks = split_into_chunks(InputList, ChunkSize),

    % Process chunks in parallel
    ProcessFun = fun(Chunk) ->
        lists:map(fun process_single_item/1, Chunk)
    end,

    Results = parallel_execution([ProcessFun, ProcessFun, ProcessFun, ProcessFun], Chunks),

    % Merge results
    lists:flatten(maps:values(Results)).
```

### 3. Conditional Error Handling

**Use case:** Handle different types of errors appropriately.

```erlang
% Error handling with different recovery strategies
handle_operation(Operation) ->
    try
        Result = Operation(),
        {ok, Result}
    catch
        error:resource_not_available ->
            handle_resource_error();
        error:timeout ->
            handle_timeout_error();
        error:validation_failed ->
            handle_validation_error();
        Error:Reason ->
            handle_generic_error(Error, Reason)
    end.

% Specific error handlers
handle_resource_error() ->
    % Retry or use fallback
    retry_operation().

handle_timeout_error() ->
    % Log and notify
    log_timeout(),
    notify_admin(),
    {error, timeout}.
```

---

## Error Handling

### 1. Common Error Scenarios

**Insufficient tokens:**
```erlang
% When trying to consume more tokens than available
case pnet_marking:take(Marking, #{task1 => [token1, token2, token3]}) of
    {error, insufficient} ->
        io:format("Not enough tokens available~n"),
        handle_insufficient_tokens();
    {ok, NewMarking} ->
        continue_with(NewMarking)
end.
```

**Invalid transition:**
```erlang
% When a transition is not enabled in the current marking
case is_enabled(Transition, CurrentMarking) of
    true ->
        fire_transition(Transition, CurrentMarking);
    false ->
        io:format("Transition not enabled in current state~n"),
        handle_disabled_transition()
end.
```

**Timeout handling:**
```erlang
% Handle operation timeouts with fallback
with_timeout(Operation, TimeoutMs, Fallback) ->
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Result = Operation(),
        self() ! {Ref, {result, Result}}
    end),

    receive
        {Ref, {result, Result}} ->
            Result
    after TimeoutMs ->
        % Timeout occurred, execute fallback
        Fallback()
    end.
```

### 2. Validation and Recovery

**Marking validation:**
```erlang
% Validate marking before operations
validate_marking(Marking) ->
    case pnet_types:is_marking(Marking) of
        true ->
            case check_consistency(Marking) of
                valid -> {ok, Marking};
                {invalid, Reason} -> {error, Reason}
            end;
        false ->
            {error, invalid_marking_format}
    end.

% Recovery strategy for invalid states
recover_from_invalid(Marking) ->
    case validate_marking(Marking) of
        {ok, ValidMarking} -> ValidMarking;
        {error, _} -> pnet_marking:new(maps:keys(Marking))
    end.
```

---

## Performance Considerations

### 1. Basic Operations Performance

**Marking operations are O(n):**
```erlang
% Fast marking creation from list
new_marking(Places) when is_list(Places) ->
    maps:from_list([{P, []} || P <- Places]).

% Efficient token addition
add_tokens(Marking, Place, Tokens) ->
    Existing = maps:get(Place, Marking, []),
    Marking#{Place => Existing ++ Tokens}.
```

**Transition firing complexity:**
```erlang
% O(m) where m is number of places in consume/produce maps
fire_transition(Marking, #{mode := Consume, produce := Produce}) ->
    case pnet_marking:take(Marking, Consume) of
        {ok, Marking1} ->
            {ok, pnet_marking:add(Marking1, Produce)};
        {error, insufficient} ->
            {error, insufficient}
    end.
```

### 2. Optimization Tips

**Use pattern matching for enabled transitions:**
```erlang
% Fast enabled checking via pattern matching
is_enabled(t_split, #{p_start := [_]}, _State) -> true;
is_enabled(t_join, #{p_branch1 := [_], p_branch2 := [_]}, _State) -> true;
is_enabled(_Transition, _Mode, _State) -> false.
```

**Cache frequently used patterns:**
```erlang
% Cache common patterns for reuse
-define(COMMON_PATTERNS, #{
    sequential => create_sequential_pattern(),
    parallel => create_parallel_pattern(),
    choice => create_choice_pattern()
}).

get_cached_pattern(Type) ->
    maps:get(Type, ?COMMON_PATTERNS).
```

**Minimize marking copies:**
```erlang
% Use references where possible
handle_workflow(Pid, Operation) ->
    gen_yawl:call(Pid, {get_state}),
    gen_yawl:cast(Pid, Operation),
    gen_yawl:call(Pid, {get_result}).
```

### 3. Memory Management

**Large workflow handling:**
```erlang
% Process workflows in chunks to avoid memory issues
process_large_workflow(Steps) ->
    ChunkSize = 100,
    process_chunks(Steps, ChunkSize, []).

process_chunks(Steps, ChunkSize, Acc) when length(Steps) =< ChunkSize ->
    CurrentResult = process_step_list(Steps),
    {ok, lists:reverse([CurrentResult | Acc])};
process_chunks(Steps, ChunkSize, Acc) ->
    {Chunk, Rest} = lists:split(ChunkSize, Steps),
    CurrentResult = process_step_list(Chunk),
    process_chunks(Rest, ChunkSize, [CurrentResult | Acc]).
```

---

## Quick Reference

### Essential Functions

| Function | Purpose | Return Type |
|----------|---------|-------------|
| `pnet_marking:new(Places)` | Create empty marking | `marking()` |
| `pnet_marking:get(Marking, Place)` | Get tokens at place | `{ok, [token()]}` |
| `pnet_marking:apply(Marking, Move)` | Fire transition | `{ok, marking()} | {error, insufficient}` |
| `pnet_types:is_marking(Term)` | Validate marking | `boolean()` |
| `yawl_patterns:new_pattern(Type, Config)` | Create pattern | `#pattern{}` |

### Common Patterns

| Pattern | Use Case | Key Functions |
|---------|----------|--------------|
| **Sequential** | Linear workflow | `fire_transition/2`, `chain_patterns/1` |
| **Parallel** | Concurrent execution | `parallel_execution/2`, `spawn/1` |
| **Exclusive Choice** | Conditional branching | `case`, `if`, `cond/3` |
| **Simple Merge** | OR-join | `lists:flatten/1`, `maps:values/1` |

### Error Handling Patterns

| Error Type | Recovery Strategy |
|------------|-------------------|
| `insufficient` | Check marking, add tokens, retry |
| `invalid_transition` | Validate enabled state, check marking |
| `timeout` | Implement fallback, retry with backoff |
| `invalid_marking` | Reset to initial state, log error |

---

This guide covers the essential permutation operations that handle the majority of real-world workflow scenarios. For advanced patterns and edge cases, refer to the full YAWL documentation and pattern reference guides.