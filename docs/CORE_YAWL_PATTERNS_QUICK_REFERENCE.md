# Core Permutations Quick Reference

> 80/20 guide for the most common YAWL workflow patterns in CRE

## Pattern Cheat Sheet

### 🔄 Sequential Workflow (WCP-01)
```erlang
% Basic linear execution
Sequence = [
    #{mode => #{start => [init]}, produce => #{task1 => [t1]}},
    #{mode => #{task1 => [t1]}, produce => #{task2 => [t2]}},
    #{mode => #{task2 => [t2]}, produce => #{end => [done]}}
].

FinalMarking = lists:foldl(fun(Move, Acc) ->
    {ok, New} = pnet_marking:apply(Acc, Move),
    New
end, pnet_marking:new([start, task1, task2, end]), Sequence).
```

### ⚡ Parallel Split (WCP-02)
```erlang
% Concurrent execution
parallel_run(Funs, Data) ->
    Pids = [spawn(fun() -> Fun(Data) end) || Fun <- Funs],
    collect_results(Pids, []).

% Example
Tasks = [fun(X) -> X*2 end, fun(X) -> X+1 end],
Results = parallel_run(Tasks, 5).  % [10, 6]
```

### 🎯 Exclusive Choice (WCP-04)
```erlang
% Conditional branching
process_order(Amount) ->
    case Amount of
        A when A > 1000 -> process_premium(Amount);
        A when A > 100 -> process_standard(Amount);
        _ -> process_basic(Amount)
    end.
```

### 🔀 Simple Merge (WCP-05)
```erlang
% OR-join results
merge_sources(Source1, Source2, Source3) ->
    lists:flatten([Source1, Source2, Source3]).

% Or with maps
combine_results(Results) ->
    maps:merge(Results, maps:merge(Results, Results2)).
```

## Core API Functions

### Marking Operations
```erlang
% Create empty marking
M0 = pnet_marking:new([p1, p2, p3]).

% Get tokens at place
{ok, Tokens} = pnet_marking:get(M0, p1).

% Set tokens at place
M1 = pnet_marking:set(M0, p1, [token1, token2]).

% Add tokens (multiset union)
M2 = pnet_marking:add(M1, #{p2 => [token3]}).

% Remove tokens (multiset subtraction)
{ok, M3} = pnet_marking:take(M2, #{p1 => [token1]}).
```

### Transition Firing
```erlang
% Define a move (transition firing)
Move = #{mode => #{p1 => [token1]}, produce => #{p2 => [token2]}}.

% Fire transition
{ok, NewMarking} = pnet_marking:apply(CurrentMarking, Move).

% Check if enabled
Enabled = is_enabled(Transition, Marking).
```

## Common Error Handling

### Token Insufficient Error
```erlang
case pnet_marking:take(Marking, #{task => [t1, t2, t3]}) of
    {error, insufficient} ->
        % Add missing tokens or wait
        io:format("Not enough tokens~n");
    {ok, Updated} ->
        continue(Updated)
end.
```

### Invalid Transition
```erlang
case is_enabled(my_transition, CurrentMarking) of
    true ->
        fire_transition(my_transition, CurrentMarking);
    false ->
        % Wait for conditions or take alternative path
        io:format("Transition not enabled~n")
end.
```

## Performance Tips

### Fast Marking Creation
```erlang
% Good: Create from list
M0 = pnet_marking:new([p1, p2, p3]).

% Better: Pre-allocate for large workflows
create_large_workflow(Places) ->
    lists:foldl(fun(P, Acc) -> Acc#{P => []} end, #{}, Places).
```

### Efficient Token Management
```erlang
% Use pattern matching for enabled checks
is_enabled(t_split, #{p_start := [_]}, _State) -> true;
is_enabled(t_join, #{p1 := [_], p2 := [_]}, _State) -> true;
is_enabled(_, _, _) -> false.
```

## Real-world Examples

### Basic Approval Workflow
```erlang
approval_process(Data) ->
    Steps = [
        #{id => review, func => review_data(Data), approve => false},
        #{id => approve, func => approve(Data), approve => true}
    ],
    execute_steps(Steps).

execute_steps([Step | Rest]) ->
    case execute_step(Step) of
        {ok, Result} when Rest =/= [] ->
            execute_steps(Rest);
        {ok, Final} ->
            {ok, Final};
        {error, Reason} ->
            {error, Reason}
    end.
```

### Parallel Processing
```erlang
process_dataset(Data) ->
    Split = split_data(Data, 4),
    Results = parallel_map([fun process_chunk/1, fun process_chunk/1,
                          fun process_chunk/1, fun process_chunk/1], Split),
    combine_results(Results).
```

## Key Data Types

### Marking
```erlang
% #{place() => [token()]}
#{start => [init], task1 => [], task2 => [], end => []}
```

### Move
```erlang
% #{mode := consume_map(), produce := produce_map()}
#{mode => #{start => [init]}, produce => #{task1 => [t1]}}
```

### Mode
```erlang
% #{place() => [token()]}
#{task1 => [t1], task2 => []}
```

## Common Patterns

### 1. Timeout Handling
```erlang
with_timeout(Fun, Timeout, Fallback) ->
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Result = Fun(),
        self() ! {Ref, {result, Result}}
    end),

    receive
        {Ref, {result, Res}} -> Res;
    after Timeout ->
        Fallback()
    end.
```

### 2. Retry Logic
```erlang
retry(Fun, MaxRetries, Delay) ->
    try Fun()
    catch
        _:_ when MaxRetries > 0 ->
            timer:sleep(Delay),
            retry(Fun, MaxRetries - 1, Delay * 2)
    end.
```

### 3. State Management
```erlang
update_state(State, Transition, Result) ->
    NewState = State#{
        last_transition => Transition,
        last_result => Result,
        timestamp => erlang:system_time(millisecond)
    },
    validate_state(NewState).
```

## Quick Debugging

### Check Enabled Transitions
```erlang
% Get all enabled transitions in current marking
Enabled = [T || T <- transitions, is_enabled(T, CurrentMarking)].
```

### Print Marking State
```erlang
% Debug helper
print_marking(Marking) ->
    lists:map(fun({Place, Tokens}) ->
        io:format("~p: ~p~n", [Place, Tokens])
    end, maps:to_list(Marking)).
```

### Token Flow Tracking
```erlang
track_token_flow(Marking, Transition, NextMarking) ->
    io:format("Transition: ~p~n", [Transition]),
    io:format("Before: ~p~n", [Marking]),
    io:format("After: ~p~n", [NextMarking]).
```

---

**Tip:** Start with sequential workflows, then add complexity with parallel and conditional patterns. Use the error handling patterns to make your workflows robust.