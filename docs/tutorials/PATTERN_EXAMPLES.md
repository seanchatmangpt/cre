# Pattern Examples Guide

**Real-World Pattern Implementations from Test Nets**

## Overview

This guide provides practical examples of YAWL workflow patterns implemented using the test net helper modules. Each example demonstrates the complete workflow pattern implementation with proper callback implementations and usage examples.

## Pattern Implementation Structure

### Basic Pattern Template

```erlang
-module(pattern_name_example).
-behaviour(gen_pnet).

% Callback exports
-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3,
         init/1,
         trigger/3]).

-include("gen_pnet.hrl").

% Callback implementations
place_lst() -> [start, task1, task2, end].
trsn_lst() -> [t1, t2].

preset(t1) -> [start].
preset(t2) -> [task1].

init(_NetArg) -> [].
init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

% Pattern-specific logic
is_enabled(t1, #{start := [init]}, _UsrInfo) -> true;
is_enabled(t2, #{task1 := [done]}, _UsrInfo) -> true.

fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{task1 => [done]}}.
fire(t2, #{task1 := []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.

trigger(Transition, Mode, _UsrInfo) ->
    io:format("Transition ~p executed~n", [Transition]).
```

## Pattern Examples

### 1. Basic Sequential Pattern

**File**: `wf_test_net_basic.erl`

**Purpose**: Demonstrate basic sequential workflow execution

**Implementation**:

```erlang
-module(wf_test_net_basic).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, trigger/3]).

-include("gen_pnet.hrl").

places() -> [start, task_a, task_b, end].
transitions() -> [a_start, b_complete].

preset(a_start) -> [start].
preset(b_complete) -> [task_a].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

is_enabled(a_start, #{start := [init]}, _UsrInfo) -> true;
is_enabled(b_complete, #{task_a := [done]}, _UsrInfo) -> true.

fire(a_start, #{start := []}, _UsrInfo) ->
    {produce, #{task_a => [done]}}.
fire(b_complete, #{task_a := []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.

trigger(Transition, _Mode, _UsrInfo) ->
    case Transition of
        a_start -> io:format("Task A started~n");
        b_complete -> io:format("Task B completed~n")
    end.
```

**Usage**:
```erlang
% Start the workflow
{ok, Pid} = gen_pnet:start_link(wf_test_net_basic, [], []).

% Execute steps
{ok, Receipt1} = gen_pnet:step(Pid),  % Executes a_start
{ok, Receipt2} = gen_pnet:step(Pid),  % Executes b_complete
{ok, FinalMarking} = gen_pnet:marking(Pid),
% #{start => [], task_a => [], task_b => [], end => [complete]}
```

### 2. Deterministic Choice Pattern

**File**: `wf_test_net_choice.erl`

**Purpose**: Demonstrate XOR choice between transitions

**Implementation**:

```erlang
-module(wf_test_net_choice).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, trigger/3]).

-include("gen_pnet.hrl").

places() -> [start, choice_a, choice_b, end].
transitions() -> [t_choice_a, t_choice_b].

preset(t_choice_a) -> [start].
preset(t_choice_b) -> [start].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

% Only one transition enabled at a time (XOR choice)
is_enabled(t_choice_a, #{start := [init]}, _UsrInfo) ->
    not is_enabled_choice_b();
is_enabled(t_choice_b, #{start := [init]}, _UsrInfo) ->
    not is_enabled_choice_a().

fire(t_choice_a, #{start := []}, _UsrInfo) ->
    {produce, #{choice_a => [done], end => [complete]}};
fire(t_choice_b, #{start := []}, _UsrInfo) ->
    {produce, #{choice_b => [done], end => [complete]}}.

% Helper functions
is_enabled_choice_a() ->
    % Logic to determine if choice_a should be enabled
    case random:uniform(2) of
        1 -> false;
        2 -> true
    end.

is_enabled_choice_b() ->
    % XOR: if A is enabled, B is not
    not is_enabled_choice_a().

trigger(Transition, _Mode, _UsrInfo) ->
    io:format("Choice transition ~p executed~n", [Transition]).
```

**Usage**:
```erlang
% The choice is determined by the random seed
{ok, Pid} = gen_pnet:start_link(wf_test_net_choice, [], [#{seed => 1}]).

% Step will execute either t_choice_a or t_choice_b (but not both)
{ok, Receipt} = gen_pnet:step(Pid),
% One of the choice transitions will fire
```

### 3. Receipt Tracking Pattern

**File**: `wf_test_net_receipt.erl`

**Purpose**: Demonstrate receipt generation and tracking

**Implementation**:

```erlang
-module(wf_test_net_receipt).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, trigger/3]).

-include("gen_pnet.hrl").

places() -> [start, process, complete].
transitions() -> [t_process, t_complete].

preset(t_process) -> [start].
preset(t_complete) -> [process].

init(UserData) -> UserData.

init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

is_enabled(t_process, #{start := [init]}, _UsrInfo) -> true.
is_enabled(t_complete, #{process := [done]}, _UsrInfo) -> true.

% Generate detailed receipts with metadata
fire(t_process, #{start := []}, _UsrInfo) ->
    {produce, #{process => [done]}, {process_complete, timestamp()}}.
fire(t_complete, #{process := []}, _UsrInfo) ->
    {produce, #{complete => [final]}, {workflow_complete, timestamp()}}.

trigger(Transition, _Mode, UsrInfo) ->
    io:format("Transition ~p triggered for user ~p~n",
              [Transition, UsrInfo#usr_info.user_id]).

% Helper function
timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    Mega * 1000000000 + Sec * 1000 + Micro div 1000.
```

**Usage**:
```erlang
% Start with user context
{ok, Pid} = gen_pnet:start_link(wf_test_net_receipt,
                               #{user_id => "user123"}, []).

% Execute step with receipt
{ok, Receipt} = gen_pnet:step(Pid),
% Receipt contains:
% - before_hash: hash of previous marking
% - after_hash: hash of new marking
% - move: transition fired and tokens produced
% - ts: timestamp

% The receipt shows complete execution trace
```

### 4. Checkpoint/Resume Pattern

**File**: `wf_test_net_resume.erl`

**Purpose**: Demonstrate workflow checkpointing and resumption

**Implementation**:

```erlang
-module(wf_test_net_resume).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, trigger/3, code_change/3]).

-include("gen_pnet.hrl").

places() -> [start, checkpoint1, work, checkpoint2, complete].
transitions() -> [t_checkpoint1, t_work, t_checkpoint2, t_complete].

preset(t_checkpoint1) -> [start].
preset(t_work) -> [checkpoint1].
preset(t_checkpoint2) -> [work].
preset(t_complete) -> [checkpoint2].

init(ResumeInfo) -> ResumeInfo.

init_marking(start, #{resume := false}) -> [init];
init_marking(start, #{resume := true}) -> [checkpoint1];
init_marking(_Place, _UsrInfo) -> [].

% Enable based on resume state
is_enabled(T, M, #{resume := false}) ->
    normal_enabled(T, M);
is_enabled(T, M, #{resume := true}) ->
    resume_enabled(T, M).

% Normal workflow progression
normal_enabled(t_checkpoint1, #{start := [init]}, _) -> true;
normal_enabled(t_work, #{checkpoint1 := [checkpoint]}, _) -> true;
normal_enabled(t_checkpoint2, #{work := [working]}, _) -> true;
normal_enabled(t_complete, #{checkpoint2 := [checkpoint]}, _) -> true;
normal_enabled(_, _, _) -> false.

% Resume workflow progression
resume_enabled(t_work, #{checkpoint1 := [checkpoint]}, _) -> true;
resume_enabled(_, _, _) -> false.

% Generate checkpoint receipts
fire(t_checkpoint1, #{start := []}, #{resume := false} = UsrInfo) ->
    {produce, #{checkpoint1 => [checkpoint]},
     {checkpoint_saved, checkpoint1, timestamp()}};
fire(t_work, #{checkpoint1 := []}, _UsrInfo) ->
    {produce, #{work => [working]}, work_completed};
fire(t_checkpoint2, #{work := []}, _UsrInfo) ->
    {produce, #{checkpoint2 => [checkpoint]},
     {checkpoint_saved, checkpoint2, timestamp()}}.

% Hot code reload support
code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

trigger(Transition, _Mode, #{resume := Resume} = UsrInfo) ->
    case Resume of
        false ->
            io:format("Normal execution: ~p~n", [Transition]);
        true ->
            io:format("Resumed execution: ~p~n", [Transition])
    end.
```

**Usage**:
```erlang
% Normal execution
{ok, Pid1} = gen_pnet:start_link(wf_test_net_resume, #{resume => false}, []).

% Execute first checkpoint
{ok, Receipt1} = gen_pnet:step(Pid1),
% checkpoint1 executed

% Save state and restart for resume
State = gen_pnet:get_state(Pid1),
ok = gen_pnet:stop(Pid1),

% Resume from checkpoint
{ok, Pid2} = gen_pnet:start_link(wf_test_net_resume, #{resume => true}, []),
% Workflow continues from checkpoint1
```

### 5. Task Completion Gate Pattern

**File**: `wf_test_net_task_gate.erl`

**Purpose**: Demonstrate external task completion gate

**Implementation**:

```erlang
-module(wf_test_net_task_gate).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, trigger/3]).

-include("gen_pnet.hrl").

places() -> [start, external_task, task_completed, end].
transitions() -> [t_external, t_gate].

preset(t_external) -> [start].
preset(t_gate) -> [external_task].

init(UserId) -> #{user_id => UserId}.

init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

% External task requires external completion
is_enabled(t_external, #{start := [init]}, _) -> true.
is_enabled(t_gate, #{external_task := [pending]}, _) -> true.

% Create external task token
fire(t_external, #{start := []}, #{user_id := UserId} = UsrInfo) ->
    {produce, #{external_task => [{external_task, UserId, pending, timestamp()}]}},
    {external_task_created, UserId, timestamp()}.

% Complete external task
fire(t_gate, #{external_task := [Task]}, _UsrInfo) ->
    case is_task_completed(Task) of
        true ->
            {produce, #{end => [complete]}};
        false ->
            abort
    end.

% Check if task is completed (external system integration)
is_task_completed({external_task, UserId, _Status, _Timestamp}) ->
    % In real implementation, check external task system
    % For demo, simulate completion after 5 seconds
    case get_time_diff(Timestamp) of
        Diff when Diff > 5000 -> true;
        _ -> false
    end.

trigger(Transition, _Mode, UsrInfo) ->
    case Transition of
        t_external ->
            io:format("External task created for user ~p~n",
                     [UsrInfo#usr_info.user_id]);
        t_gate ->
            io:format("Checking task completion...~n")
    end.
```

**Usage**:
```erlang
% Start workflow with external task
{ok, Pid} = gen_pnet:start_link(wf_test_net_task_gate,
                               #{user_id => "user123"}, []).

% Create external task
{ok, Receipt1} = gen_pnet:step(Pid),
% External task token created

% Wait for external completion
timer:sleep(6000),  % Wait for task completion

% Try to complete workflow
{ok, Receipt2} = gen_pnet:step(Pid),
% Gate transition fires, completes workflow
```

### 6. Token Filtering Pattern

**File**: `wf_test_net_trigger_drop.erl`

**Purpose**: Demonstrate token filtering based on conditions

**Implementation**:

```erlang
-module(wf_test_net_trigger_drop).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, trigger/3]).

-include("gen_pnet.hrl").

places() -> [start, filtered, unfiltered, end].
transitions() -> [t_filter, t_unfiltered].

preset(t_filter) -> [start].
preset(t_unfiltered) -> [start].

init(Config) -> Config.

init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

% Filter based on token type
is_enabled(t_filter, #{start := Tokens}, _UsrInfo) ->
    has_filtered_tokens(Tokens);
is_enabled(t_unfiltered, #{start := Tokens}, _UsrInfo) ->
    has_unfiltered_tokens(Tokens).

% Filter transitions
fire(t_filter, #{start := []}, _UsrInfo) ->
    {produce, #{filtered => [filtered_token, timestamp()]}};
fire(t_unfiltered, #{start := []}, _UsrInfo) ->
    {produce, #{unfiltered => [unfiltered_token, timestamp()]}},
    {produce, #{end => [complete]}}.

% Token type checking
has_filtered_tokens([init]) -> false;
has_filtered_tokens([Token | _]) ->
    is_filtered_token(Token);
has_filtered_tokens([]) -> false.

has_unfiltered_tokens([init]) -> true;
has_unfiltered_tokens([_ | _]) -> false.

is_filtered_token(Token) ->
    % Check if token should be filtered
    case Token of
        {urgent, _} -> true;
        _ -> false
    end.

trigger(Transition, Mode, _UsrInfo) ->
    TokenCount = length(maps:get(start, Mode, [])),
    io:format("Transition ~p executed with ~p tokens~n",
              [Transition, TokenCount]).
```

**Usage**:
```erlang
% Start workflow
{ok, Pid} = gen_pnet:start_link(wf_test_net_trigger_drop, [], []).

% Check initial state
{ok, Marking} = gen_pnet:marking(Pid),
% #{start => [init], filtered => [], unfiltered => [], end => []}

% Execute - unfiltered path will fire
{ok, Receipt} = gen_pnet:step(Pid),
% t_unfiltered fires, creates end token

% Final marking
{ok, FinalMarking} = gen_pnet:marking(Pid),
% #{start => [], filtered => [], unfiltered => [unfiltered_token], end => [complete]}
```

## Advanced Patterns

### 7. Multi-Instance Pattern

**File**: `wf_test_net_multi.erl` (conceptual)

```erlang
-module(wf_test_net_multi).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, trigger/3]).

-include("gen_pnet.hrl").

places() -> [start, task1, task2, task3, end].
transitions() -> [t1, t2, t3].

preset(T) when T == t1; T == t2; T == t3 -> [start].

init(Count) -> #{instances => Count}.

init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

% Enable all transitions (parallel execution)
is_enabled(T, #{start := [init]}, _UsrInfo) -> true.

% Each transition produces its own task
fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{task1 => [done]}};
fire(t2, #{start := []}, _UsrInfo) ->
    {produce, #{task2 => [done]}};
fire(t3, #{start := []}, _UsrInfo) ->
    {produce, #{task3 => [done]}}.

trigger(Transition, _Mode, _UsrInfo) ->
    io:format("Parallel task ~p completed~n", [Transition]).
```

## Pattern Testing

### Test Pattern Functions

Each test net pattern can be tested with these functions:

```erlang
% Test basic functionality
test_pattern(Module) ->
    {ok, Pid} = gen_pnet:start_link(Module, [], []),

    % Execute all steps
    test_steps(Pid, []),

    % Verify final state
    {ok, FinalMarking} = gen_pnet:marking(Pid),
    io:format("Final marking: ~p~n", [FinalMarking]).

% Execute steps until abort
test_steps(Pid, Receipts) ->
    case gen_pnet:step(Pid) of
        abort -> Receipts;
        {ok, Receipt} -> test_steps(Pid, [Receipt | Receipts])
    end.
```

## Best Practices

### 1. Pattern Design
- Keep patterns simple and focused
- Use meaningful token names
- Document pattern behavior clearly
- Test edge cases thoroughly

### 2. Testing
- Test each pattern in isolation
- Verify all execution paths
- Check performance characteristics
- Document expected behavior

### 3. Integration
- Use patterns as building blocks
- Combine patterns for complex workflows
- Maintain consistency across patterns
- Document integration points

## Troubleshooting

### Common Issues
1. **Deadlocks**: Check transition dependencies
2. **Starvation**: Ensure fair transition selection
3. **Memory**: Monitor token accumulation
4. **Performance**: Profile execution patterns

### Debug Patterns
1. **Step Execution**: Execute one step at a time
2. **Token Tracking**: Monitor token flow
3. **State Inspection**: Check marking at each step
4. **Error Logging**: Enable debug logging