# Basic Workflow Patterns Tutorial

**Learn fundamental YAWL control flow patterns step-by-step**

---

## What You'll Learn

In this tutorial, you will:

- Understand the 6 basic workflow control patterns (WCP-01 to WCP-06)
- Implement each pattern using gen_pnet
- Practice with hands-on exercises
- Learn when to use each pattern in real-world scenarios

**Time required**: 60 minutes
**Prerequisites**: Completed [Getting Started](getting_started.md)

---

## Overview of Basic Control Patterns

| Pattern | WCP # | Description | Use Case |
|---------|-------|-------------|----------|
| Sequence | WCP-01 | Tasks execute one after another | Order processing |
| Parallel Split | WCP-02 | Split into concurrent branches | Multi-step verification |
| Synchronization | WCP-03 | Wait for all branches to complete | Parallel aggregation |
| Exclusive Choice | WCP-04 | Choose one branch based on condition | Routing decisions |
| Simple Merge | WCP-05 | Merge single incoming path | Alternative completion |
| Multiple Choice | WCP-06 | Activate multiple branches simultaneously | Notifications |

---

## Pattern 1: Sequence (WCP-01)

### Understanding Sequence

The Sequence pattern is the simplest workflow pattern. Tasks execute one after another in a defined order.

**Visual Representation:**
```
[Task 1] --> [Task 2] --> [Task 3] --> [Complete]
```

**Petri Net Structure:**
```
p_start --t_start--> p_task1 --t_complete1--> p_task2 --t_complete2--> p_task3 --...--> p_end
```

### Implementation

Create `sequence_pattern.erl`:

```erlang
%% @doc WCP-01: Sequence Pattern Implementation
-module(sequence_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/1]).

-define(TOKEN(Ctx), #colored_token{
    id = generate_id(),
    type = Ctx,
    payload = Payload,
    created_at = erlang:system_time(millisecond)
}).

%% API
run() -> run([validate, process, finalize]).
run(Tasks) when is_list(Tasks) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{tasks => Tasks}),
    wait_for_result(Pid, 5000).

wait_for_result(Pid, Timeout) ->
    receive
        {sequence_complete, Result} -> {ok, Result};
        {sequence_error, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() -> [p_start, p_task1, p_task2, p_task3, p_end].

trsn_lst() -> [t_start, t_complete1, t_complete2, t_complete3, t_finalize].

init_marking(p_start, _) -> [?TOKEN(initial)];
init_marking(_, _) -> [].

preset(t_start) -> [p_start];
preset(t_complete1) -> [p_task1];
preset(t_complete2) -> [p_task2];
preset(t_complete3) -> [p_task3];
preset(t_finalize) -> [p_end];
preset(_) -> [].

is_enabled(T, Mode, _) ->
    case {T, maps:get(element(2, T), Mode, [])} of
        {_, [Token|_]} when Token =/= [] -> true;
        _ -> false
    end.

fire(t_start, _, UsrInfo) ->
    Payload = #{
        task => hd(maps:get(tasks, UsrInfo)),
        step => 1
    },
    {produce, #{p_task1 => [?TOKEN(task_start)], p_start => []}};

fire(t_complete1, #{p_task1 := [Token]}, UsrInfo) ->
    Tasks = maps:get(tasks, UsrInfo),
    handle_task_completion(Token, Tasks, 1);

fire(t_complete2, #{p_task2 := [Token]}, UsrInfo) ->
    Tasks = maps:get(tasks, UsrInfo),
    handle_task_completion(Token, Tasks, 2);

fire(t_complete3, #{p_task3 := [Token]}, UsrInfo) ->
    Tasks = maps:get(tasks, UsrInfo),
    handle_task_completion(Token, Tasks, 3);

fire(t_finalize, _, _) ->
    self() ! {sequence_complete, #{status => complete}},
    {produce, #{}};

fire(_, _, _) -> abort.

handle_task_completion(Token, Tasks, Step) ->
    TaskName = lists:nth(Step, Tasks),
    io:format("Completed task: ~p~n", [TaskName]),

    NextStep = Step + 1,
    case NextStep > length(Tasks) of
        true ->
            {produce, [{p_end, [?TOKEN(completion)]}, {element(2, element(1, lists:keyfind(Step - 1, 2, place_lst()))}, []}]};
        false ->
            NextPlace = list_to_existing_atom([p_task, integer_to_list(NextStep)]),
            NewToken = ?TOKEN(task_start),
            CurrentPlace = element(2, element(1, lists:keyfind(Step, 2, place_lst()))),
            {produce, [{NextPlace, [NewToken]}, {CurrentPlace, []}]}
    end.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.
```

### Testing the Sequence Pattern

```erlang
c(sequence_pattern).
sequence_pattern:run([validate, process, finalize]).
```

**Expected Output:**
```
Completed task: validate
Completed task: process
Completed task: finalize
```

### Exercise: Add Error Handling

Modify the sequence pattern to handle errors at any step and stop execution.

**Hint**: Add an error place and transition that can be reached from any task.

---

## Pattern 2: Parallel Split (WCP-02)

### Understanding Parallel Split

The Parallel Split pattern divides execution into multiple concurrent branches. All branches execute simultaneously.

**Visual Representation:**
```
     --> [Branch A]
    /
[Split] --> [Branch B]
    \
     --> [Branch C]
```

**Key Characteristics:**
- All branches start simultaneously
- Branches execute independently
- No coordination between branches during execution

### Implementation

Create `parallel_split_pattern.erl`:

```erlang
%% @doc WCP-02: Parallel Split Pattern
-module(parallel_split_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

%% API
run() -> run(3, fun branch_task/2).
run(BranchCount, TaskFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        branch_count => BranchCount,
        task_function => TaskFun
    }),
    wait_for_completion(Pid, 10000).

wait_for_completion(Pid, Timeout) ->
    receive
        {parallel_complete, Results} -> {ok, Results};
        {parallel_error, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_split, p_branch_1, p_branch_2, p_branch_3, p_join_buffer].

trsn_lst() -> [t_split, t_complete_branch].

init_marking(p_split, _) -> [?TOKEN(initial, #{})];
init_marking(_, _) -> [].

preset(t_split) -> [p_split];
preset(t_complete_branch) -> [p_branch_1, p_branch_2, p_branch_3];
preset(_) -> [].

is_enabled(t_split, #{p_split := [Token]}, _) when Token =/= [] -> true;
is_enabled(t_complete_branch, Mode, _) ->
    lists:any(fun(P) ->
        case maps:get(P, Mode, []) of
            [T|_] when T =/= [] -> true;
            _ -> false
        end
    end, [p_branch_1, p_branch_2, p_branch_3]);
is_enabled(_, _, _) -> false.

fire(t_split, _, UsrInfo) ->
    BranchCount = maps:get(branch_count, UsrInfo, 3),
    TaskFun = maps:get(task_function, UsrInfo),

    %% Create tokens for each branch
    BranchTokens = lists:map(fun(N) ->
        BranchId = list_to_existing_atom("branch_" ++ integer_to_list(N)),
        spawn(fun() ->
            Result = TaskFun(BranchId, #{branch => N}),
            self() ! {branch_complete, BranchId, Result}
        end),
        ?TOKEN(branch_start, #{branch_id => BranchId, number => N})
    end, lists:seq(1, BranchCount)),

    %% Distribute to branch places
    lists:foldl(fun({Token, Place}, Acc) ->
        maps:put(Place, [Token], Acc)
    end, #{}, lists:zip(BranchTokens, [p_branch_1, p_branch_2, p_branch_3])),

    %% Clear split place
    {produce, maps:merge(#{p_split => []},
        lists:foldl(fun({T, P}, A) -> maps:put(P, [T], A) end,
                     #{},
                     lists:zip(BranchTokens, [p_branch_1, p_branch_2, p_branch_3])))};

fire(t_complete_branch, Mode, UsrInfo) ->
    %% Find which branch has a token
    case lists:search(fun(P) ->
        case maps:get(P, Mode, []) of
            [T|_] when T =/= [] -> true;
            _ -> false
        end
    end, [p_branch_1, p_branch_2, p_branch_3]) of
        {value, Place} ->
            [Token|_] = maps:get(Place, Mode, []),
            io:format("Completed branch: ~p~n", [Token#colored_token.payload]),
            {produce, #{Place => [], p_join_buffer => [?TOKEN(branch_done, Token#colored_token.payload)]}};
        false ->
            abort
    end;

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info({branch_complete, BranchId, Result}, State) ->
    io:format("Branch ~p completed with: ~p~n", [BranchId, Result]),
    {noreply, State};
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

%% Default branch task function
branch_task(BranchId, Payload) ->
    io:format("Executing branch ~p~n", [BranchId]),
    timer:sleep(1000),  % Simulate work
    #{branch => BranchId, result => ok, payload => Payload}.
```

### Testing Parallel Split

```erlang
c(parallel_split_pattern).
parallel_split_pattern:run(3, fun(Branch, _) ->
    io:format("Working on ~p~n", [Branch]),
    timer:sleep(rand:uniform(2000)),
    {Branch, complete}
end).
```

### Exercise: Add Result Collection

Modify the parallel split to collect results from all branches and return them as a list.

---

## Pattern 3: Synchronization (WCP-03)

### Understanding Synchronization

Synchronization waits for ALL parallel branches to complete before proceeding.

**Visual Representation:**
```
[Branch A] --\
            --> [Sync] --> [Continue]
[Branch B] --/
```

### Implementation

Create `synchronization_pattern.erl`:

```erlang
%% @doc WCP-03: Synchronization Pattern
-module(synchronization_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

-record(state, {
    expected_count :: integer(),
    completed_count :: integer(),
    results :: [term()]
}).

%% API
run() -> run(3, fun sync_task/2).
run(BranchCount, TaskFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        expected_count => BranchCount,
        task_function => TaskFun
    }),
    wait_for_sync(Pid, 15000).

wait_for_sync(Pid, Timeout) ->
    receive
        {sync_complete, Results} -> {ok, Results};
        {sync_error, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_branches, p_sync_buffer, p_complete].

trsn_lst() -> [t_spawn, t_complete_branch, t_sync].

init_marking(p_start, _) -> [?TOKEN(initial, #{})];
init_marking(_, _) -> [].

preset(t_spawn) -> [p_start];
preset(t_complete_branch) -> [p_branches];
preset(t_sync) -> [p_sync_buffer];
preset(_) -> [].

is_enabled(t_spawn, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_complete_branch, #{p_branches := Tokens}, _) when length(Tokens) > 0 -> true;
is_enabled(t_sync, #{p_sync_buffer := Tokens}, UsrInfo) ->
    length(Tokens) >= maps:get(expected_count, UsrInfo, 3);
is_enabled(_, _, _) -> false.

fire(t_spawn, _, UsrInfo) ->
    Count = maps:get(expected_count, UsrInfo, 3),
    Tokens = [?TOKEN(branch_start, #{number => N}) || N <- lists:seq(1, Count)],
    {produce, #{p_branches => Tokens, p_start => []}};

fire(t_complete_branch, #{p_branches := [Token|Rest]}, UsrInfo) ->
    TaskFun = maps:get(task_function, UsrInfo),
    N = maps:get(number, Token#colored_token.payload, 0),
    Result = TaskFun(branch, N),
    NewToken = ?TOKEN(branch_done, #{number => N, result => Result}),
    {produce, #{
        p_branches => Rest,
        p_sync_buffer => [NewToken]
    }};

fire(t_sync, #{p_sync_buffer := Tokens}, _) ->
    io:format("All branches synchronized! Collected ~p results~n", [length(Tokens)]),
    Results = [T#colored_token.payload || T <- Tokens],
    self() ! {sync_complete, Results},
    {produce, #{
        p_sync_buffer => [],
        p_complete => [?TOKEN(completion, #{results => Results})]
    }};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

sync_task(branch, N) ->
    timer:sleep(rand:uniform(1000)),
    {branch_complete, N}.
```

### Testing Synchronization

```erlang
c(synchronization_pattern).
synchronization_pattern:run(4, fun(_, N) ->
    timer:sleep(rand:uniform(2000)),
    N * 10
end).
```

---

## Pattern 4: Exclusive Choice (WCP-04)

### Understanding Exclusive Choice

Exclusive Choice selects exactly ONE branch based on conditions (XOR semantics).

**Visual Representation:**
```
            --> [Branch A] (condition A true)
           /
[Choice] --+--> [Branch B] (condition B true)
           \
            --> [Branch C] (condition C true)
```

### Implementation

Create `exclusive_choice_pattern.erl`:

```erlang
%% @doc WCP-04: Exclusive Choice Pattern
-module(exclusive_choice_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

%% API
run() -> run(priority_check, fun(Condition) -> Condition end).
run(Data, ConditionFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        condition_function => ConditionFun,
        input_data => Data
    })),
    wait_for_result(Pid, 5000).

wait_for_result(Pid, Timeout) ->
    receive
        {choice_result, Result} -> {ok, Result};
        {choice_error, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_input, p_choice, p_branch_high, p_branch_medium, p_branch_low, p_result].

trsn_lst() -> [t_evaluate, t_select_high, t_select_medium, t_select_low].

init_marking(p_input, UsrInfo) ->
    Data = maps:get(input_data, UsrInfo, undefined),
    [?TOKEN(initial, Data)];
init_marking(_, _) -> [].

preset(t_evaluate) -> [p_input];
preset(t_select_high) -> [p_choice];
preset(t_select_medium) -> [p_choice];
preset(t_select_low) -> [p_choice];
preset(_) -> [].

is_enabled(t_evaluate, #{p_input := [T]}, _) when T =/= [] -> true;
is_enabled(T, #{p_choice := [Token]}, UsrInfo) when T =/= t_evaluate ->
    ConditionFun = maps:get(condition_function, UsrInfo),
    case T of
        t_select_high -> ConditionFun(high) =:= true;
        t_select_medium -> ConditionFun(medium) =:= true;
        t_select_low -> ConditionFun(low) =:= true;
        _ -> false
    end;
is_enabled(_, _, _) -> false.

fire(t_evaluate, #{p_input := [Token]}, UsrInfo) ->
    ConditionFun = maps:get(condition_function, UsrInfo),
    Data = Token#colored_token.payload,

    %% Evaluate conditions and store result in token
    NewToken = ?TOKEN(choice_pending, #{
        data => Data,
        conditions => #{
            high => ConditionFun(high),
            medium => ConditionFun(medium),
            low => ConditionFun(low)
        }
    }),
    {produce, #{p_choice => [NewToken], p_input => []}};

fire(t_select_high, #{p_choice := [Token]}, _) ->
    io:format("Selected HIGH priority branch~n"),
    self() ! {choice_result, #{branch => high, data => maps:get(data, Token#colored_token.payload)}},
    {produce, #{p_choice => [], p_branch_high => [?TOKEN(branch_active, #{branch => high})]}};

fire(t_select_medium, #{p_choice := [Token]}, _) ->
    io:format("Selected MEDIUM priority branch~n"),
    self() ! {choice_result, #{branch => medium, data => maps:get(data, Token#colored_token.payload)}},
    {produce, #{p_choice => [], p_branch_medium => [?TOKEN(branch_active, #{branch => medium})]}};

fire(t_select_low, #{p_choice := [Token]}, _) ->
    io:format("Selected LOW priority branch~n"),
    self() ! {choice_result, #{branch => low, data => maps:get(data, Token#colored_token.payload)}},
    {produce, #{p_choice => [], p_branch_low => [?TOKEN(branch_active, #{branch => low})]}};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.
```

### Testing Exclusive Choice

```erlang
c(exclusive_choice_pattern).

%% Test with high priority
exclusive_choice_pattern:run(high, fun
    (high) -> true;
    (_) -> false
end).

%% Test with medium priority
exclusive_choice_pattern:run(medium, fun
    (medium) -> true;
    (_) -> false
end).
```

### Exercise: Add Default Branch

Add a default branch that executes when no conditions are true.

---

## Pattern 5: Simple Merge (WCP-05)

### Understanding Simple Merge

Simple Merge combines multiple alternative paths into a single output. Only ONE path should be active (XOR join).

**Visual Representation:**
```
[Path A] --\
           --> [Merged Output]
[Path B] --/
```

**Key Difference from Synchronization:**
- **Synchronization (WCP-03)**: Wait for ALL paths
- **Simple Merge (WCP-05)**: First path to complete proceeds

### Implementation

Create `simple_merge_pattern.erl`:

```erlang
%% @doc WCP-05: Simple Merge Pattern
-module(simple_merge_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0]).

-define(TOKEN(Type), #colored_token{
    id = generate_id(),
    type = Type,
    payload => #{},
    created_at = erlang:system_time(millisecond)
}).

%% API
run() ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{}),
    wait_for_merge(Pid, 5000).

wait_for_merge(Pid, Timeout) ->
    receive
        {merge_complete} -> ok;
        {merge_error, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_path_a, p_path_b, p_merge, p_output].

trsn_lst() -> [t_complete_a, t_complete_b, t_merge].

init_marking(p_path_a, _) -> [?TOKEN(path_start)];
init_marking(_, _) -> [].

preset(t_complete_a) -> [p_path_a];
preset(t_complete_b) -> [p_path_b];
preset(t_merge) -> [p_merge];
preset(_) -> [].

is_enabled(t_complete_a, #{p_path_a := [T]}, _) when T =/= [] -> true;
is_enabled(t_complete_b, #{p_path_b := [T]}, _) when T =/= [] -> true;
is_enabled(t_merge, #{p_merge := [T|_]}, _) when T =/= [] -> true;
is_enabled(_, _, _) -> false.

fire(t_complete_a, #{p_path_a := [Token]}, _) ->
    io:format("Path A completed~n"),
    {produce, #{p_path_a => [], p_merge => [Token#colored_token{type = path_complete}]}};

fire(t_complete_b, #{p_path_b := [Token]}, _) ->
    io:format("Path B completed~n"),
    {produce, #{p_path_b => [], p_merge => [Token#colored_token{type = path_complete}]}};

fire(t_merge, #{p_merge := [_Token]}, _) ->
    io:format("Paths merged~n"),
    self() ! {merge_complete},
    {produce, #{p_merge => [], p_output => [?TOKEN(merge_complete)]}};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.
```

---

## Pattern 6: Multiple Choice (WCP-06)

### Understanding Multiple Choice

Multiple Choice activates MULTIPLE branches simultaneously based on conditions (OR semantics).

**Visual Representation:**
```
            --> [Branch A] (condition A)
           /---> [Branch B] (condition B)
[Choice] --+
           \---> [Branch C] (condition C)
            \
             --> [Branch D] (condition D)
```

### Implementation

Create `multiple_choice_pattern.erl`:

```erlang
%% @doc WCP-06: Multiple Choice Pattern
-module(multiple_choice_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

%% API
run() -> run(sample_data, fun evaluate_conditions/1).
run(Data, ConditionFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        input_data => Data,
        condition_function => ConditionFun
    })),
    wait_for_completion(Pid, 10000).

wait_for_completion(Pid, Timeout) ->
    receive
        {multi_choice_complete, Results} -> {ok, Results};
        {multi_choice_error, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_input, p_evaluated, p_branch_1, p_branch_2, p_branch_3, p_results].

trsn_lst() -> [t_evaluate, t_activate_1, t_activate_2, t_activate_3, t_collect].

init_marking(p_input, UsrInfo) ->
    Data = maps:get(input_data, UsrInfo, undefined),
    [?TOKEN(initial, Data)];
init_marking(_, _) -> [].

preset(t_evaluate) -> [p_input];
preset(t_activate_1) -> [p_evaluated];
preset(t_activate_2) -> [p_evaluated];
preset(t_activate_3) -> [p_evaluated];
preset(t_collect) -> [p_branch_1, p_branch_2, p_branch_3];
preset(_) -> [].

is_enabled(t_evaluate, #{p_input := [T]}, _) when T =/= [] -> true;
is_enabled(T, #{p_evaluated := [Token]}, UsrInfo) when T =/= t_evaluate ->
    ConditionFun = maps:get(condition_function, UsrInfo),
    Conditions = Token#colored_token.payload,
    case T of
        t_activate_1 -> maps:get(branch_1, Conditions, false);
        t_activate_2 -> maps:get(branch_2, Conditions, false);
        t_activate_3 -> maps:get(branch_3, Conditions, false);
        _ -> false
    end;
is_enabled(t_collect, Mode, _) ->
    lists:any(fun(P) -> maps:get(P, Mode, []) =/= [] end,
               [p_branch_1, p_branch_2, p_branch_3]);
is_enabled(_, _, _) -> false.

fire(t_evaluate, #{p_input := [Token]}, UsrInfo) ->
    ConditionFun = maps:get(condition_function, UsrInfo),
    Data = Token#colored_token.payload,

    Conditions = #{
        branch_1 => ConditionFun(branch_1, Data),
        branch_2 => ConditionFun(branch_2, Data),
        branch_3 => ConditionFun(branch_3, Data)
    },

    io:format("Evaluated conditions: ~p~n", [Conditions]),

    NewToken = ?TOKEN(evaluated, Conditions),
    {produce, #{p_evaluated => [NewToken], p_input => []}};

fire(t_activate_1, #{p_evaluated := [Token]}, _) ->
    io:format("Activating Branch 1~n"),
    {produce, #{p_branch_1 => [?TOKEN(branch_active, #{id => 1})}]}};

fire(t_activate_2, #{p_evaluated := [Token]}, _) ->
    io:format("Activating Branch 2~n"),
    {produce, #{p_branch_2 => [?TOKEN(branch_active, #{id => 2})}]}};

fire(t_activate_3, #{p_evaluated := [Token]}, _) ->
    io:format("Activating Branch 3~n"),
    {produce, #{p_branch_3 => [?TOKEN(branch_active, #{id => 3})}]}};

fire(t_collect, Mode, _) ->
    %% Collect all active branches
    Results = lists:filtermap(fun(P) ->
        case maps:get(P, Mode, []) of
            [T|_] -> {true, T#colored_token.payload};
            _ -> false
        end
    end, [p_branch_1, p_branch_2, p_branch_3]),

    io:format("Collected results from ~p branches~n", [length(Results)]),

    self() ! {multi_choice_complete, Results},

    %% Clear all branch places
    {produce, maps:from_list([{P, []} || P <- [p_branch_1, p_branch_2, p_branch_3]])};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

%% Example condition function
evaluate_conditions(branch, Data) ->
    case {branch, Data} of
        {branch_1, sample_data} -> true;
        {branch_2, sample_data} -> true;
        {branch_3, _} -> false;
        _ -> false
    end.
```

---

## Summary: Pattern Selection Guide

| Pattern | Use When... | Example |
|---------|-------------|---------|
| **Sequence** | Tasks must complete in order | Order processing |
| **Parallel Split** | Tasks can run independently | Multi-channel notifications |
| **Synchronization** | Need all parallel results | Aggregating data |
| **Exclusive Choice** | One path based on condition | Routing by priority |
| **Simple Merge** | Alternative completion paths | Multiple finish options |
| **Multiple Choice** | Multiple paths simultaneously | Multi-channel marketing |

---

## Exercises

### Exercise 1: Order Processing Pipeline

Create a workflow that:
1. Validates an order (Sequence)
2. Checks inventory and process payment in parallel (Parallel Split)
3. Waits for both to complete (Synchronization)
4. Ships the order (Sequence)

### Exercise 2: Dynamic Routing

Create a workflow that:
1. Accepts a request (Input)
2. Routes to different handlers based on request type (Exclusive Choice)
   - `high_priority` -> Express handler
   - `normal_priority` -> Standard handler
   - `low_priority` -> Batch handler
3. Merges results (Simple Merge)

### Exercise 3: Notification System

Create a workflow that:
1. Evaluates which notification channels are available (Multiple Choice)
   - Email: if email address exists
   - SMS: if phone number exists
   - Push: if device token exists
2. Sends to all available channels simultaneously
3. Collects delivery confirmations

---

**Next Steps**: Continue to [Advanced Patterns Tutorial](advanced_patterns_tutorial.md) to learn about complex synchronization and multiple instance patterns.
