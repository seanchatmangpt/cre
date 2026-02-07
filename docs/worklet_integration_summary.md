# Worklet Integration with wf_engine

This document describes the worklet integration for dynamic task replacement in the workflow engine.

## Overview

Worklets allow runtime replacement of tasks with alternative sub-workflows based on data conditions.

## API Functions

### set_worklet/2

Sets a worklet for dynamic task replacement on the engine.

```erlang
-spec set_worklet(Engine :: pid() | atom(), Worklet :: wf_worklet:worklet()) -> ok.
```

**Example:**
```erlang
Rules = [
    #{task => approve, 'when' => {gt, amount, 1000}, choose => approve_big},
    #{task => approve, 'when' => any, choose => approve_small}
],
Worklet = wf_worklet:new(Rules),
ok = wf_engine:set_worklet(Eng, Worklet).
```

### apply_worklet/4

Applies worklet rules to check if a task should be replaced.

```erlang
-spec apply_worklet(Worklet :: wf_worklet:worklet(), Task :: atom(),
                   Data :: map(), Now :: integer()) ->
          {ok, atom()} | {error, no_matching_rule}.
```

**Example:**
```erlang
{ok, approve_big} = wf_engine:apply_worklet(Worklet, approve, #{amount => 5000}, 0).
{ok, approve_small} = wf_engine:apply_worklet(Worklet, approve, #{amount => 100}, 0).
{error, no_matching_rule} = wf_engine:apply_worklet(Worklet, unknown_task, #{}, 0).
```

## Engine State Extension

The engine_state record should include a worklet field:

```erlang
-record(engine_state, {
    spec :: map(),
    org :: map(),
    cases = #{} :: #{case_id() => wf_case()},
    timerq :: wf_timerq:timerq(),
    next_seq = 1 :: non_neg_integer(),
    current_time = 0 :: integer(),
    rng_state :: pnet_choice:rng_state(),
    worklet :: wf_worklet:worklet() | undefined
}).
```

## Internal Helper Functions

### process_enabled_with_worklet/4

Processes enabled transitions with worklet support.

```erlang
-spec process_enabled_with_worklet(wf_case(), map(), wf_worklet:worklet() | undefined,
                                   integer()) -> wf_case().
```

### process_enabled_worklet/4

Processes enabled transitions with worklet checking. For each enabled transition,
checks if a worklet rule applies and substitutes the task with a replacement workflow.

```erlang
-spec process_enabled_worklet(wf_case(), map(), wf_worklet:worklet(), integer()) -> wf_case().
```

### fire_worklet_replacement/5

Fires a worklet replacement transition, creating a receipt and marking update.

```erlang
-spec fire_worklet_replacement(wf_case(), atom(), atom(), map(), integer()) -> wf_case().
```

## gen_server Handler

Add to handle_call:

```erlang
handle_call({set_worklet, Worklet}, _From, State) ->
    {reply, ok, State#engine_state{worklet = Worklet}};
```

## Initialization

Update init to accept worklet from config:

```erlang
init(Config) ->
    Worklet = maps:get(worklet, Config, undefined),
    ...
    {ok, #engine_state{..., worklet = Worklet}}.
```

## Usage Pattern

1. Create worklet with rules
2. Set worklet on engine via `set_worklet/2`
3. Engine checks rules before executing tasks
4. Matching rules replace tasks with alternative workflows
5. Results are logged and merged into case data

## Doctests

```erlang
%% Worklet rule matches and executes replacement
{ok, Replacement} = wf_engine:apply_worklet(Worklet, Task, Data, 0).

%% Worklet results merged into case data
Receipts = wf_engine:drain_receipts(Eng, CaseId),
{worklet_replacement, OriginalTask, ReplacementWorkflow, _Ts} = 
    lists:keyfind(worklet_replacement, 1, Receipts).

%% No match means original task executes
{error, no_matching_rule} = wf_engine:apply_worklet(Worklet, UnknownTask, #{}, 0).
```
