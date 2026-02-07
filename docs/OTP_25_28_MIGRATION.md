# OTP 25-28 Migration Guide for gen_pnet Callbacks

**Document Version:** 1.0
**Date:** February 6, 2026
**Applies to:** CRE 0.2.0 / 2.1.0
**OTP Versions:** 25.0, 26, 27, 28

---

## Executive Summary

This guide documents the `gen_pnet` callback compatibility changes required for OTP 25-28 support. The primary changes affect two critical callbacks:

1. **`fire/3`** - Return value changed from 3-tuple to 2-tuple (with optional 3-tuple via `gen_yawl`)
2. **`init/1`** - Return value changed from `{ok, State}` to plain `State`

These changes were introduced to align with modern OTP practices and provide better state management for YAWL workflow patterns.

---

## Table of Contents

1. [Overview of Changes](#overview-of-changes)
2. [fire/3 Return Value Changes](#fire3-return-value-changes)
3. [init/1 Return Value Changes](#init1-return-value-changes)
4. [Modules Affected](#modules-affected)
5. [Migration Guide for Custom Patterns](#migration-guide-for-custom-patterns)
6. [Code Examples](#code-examples)
7. [Testing Recommendations](#testing-recommendations)
8. [Backward Compatibility](#backward-compatibility)

---

## Overview of Changes

### Why These Changes Were Made

The callback changes address several issues:

1. **OTP 28 Compatibility** - Earlier `gen_pnet` versions had type mismatches with OTP 28
2. **Simplified State Management** - Direct state updates in `fire/3` reduce boilerplate
3. **YAWL Workflow Requirements** - Complex workflow patterns need automatic state propagation
4. **Consistent with OTP Behaviors** - Aligns with `gen_server` and other OTP behaviors

### The gen_yawl Wrapper

To support both the old and new callback styles, CRE introduced `gen_yawl` - a wrapper around `gen_pnet` that:

- Accepts the standard 2-tuple `{produce, Map}` return from `fire/3`
- Optionally accepts a 3-tuple `{produce, Map, NewUsrInfo}` for automatic state updates
- Handles `init/1` returning plain `State` instead of `{ok, State}`
- Provides drop-in replacement for `gen_pnet` in YAWL workflow modules

---

## fire/3 Return Value Changes

### Before (Old Style)

```erlang
%% Old: 3-tuple return with consume map and produce map
-callback fire(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    {consume, map(), produce, map()} | abort.
```

### After (New Style)

```erlang
%% New: 2-tuple return with produce map only
-callback fire(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    {produce, map()} | abort.
```

### With gen_yawl (Enhanced)

```erlang
%% Enhanced: Optional 3-tuple for automatic usr_info updates
-callback fire(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    abort |
    {produce, map()} |
    {produce, map(), NewUsrInfo :: term()}.
```

### Key Differences

| Aspect | Old Style | New Style (gen_pnet) | Enhanced Style (gen_yawl) |
|--------|-----------|---------------------|--------------------------|
| Return type | `{consume, ConsumeMap, produce, ProduceMap}` | `{produce, ProduceMap}` | `{produce, ProduceMap}` or `{produce, ProduceMap, NewUsrInfo}` |
| State updates | Manual via handle_call/cast | Via trigger/3 | Automatic with 3-tuple |
| Complexity | Higher (explicit consume) | Lower (implicit consume) | Moderate (optional state update) |

---

## init/1 Return Value Changes

### Before (Old Style)

```erlang
%% Old: Return {ok, State} like gen_server
-callback init(NetArg :: term()) ->
    {ok, UsrInfo :: term()}.
```

### After (New Style)

```erlang
%% New: Return plain State
-callback init(NetArg :: term()) ->
    UsrInfo :: term().
```

### Key Differences

| Aspect | Old Style | New Style |
|--------|-----------|-----------|
| Return value | `{ok, State}` | `State` |
| Error handling | `{stop, Reason}` or `{ok, State}` | Exceptions via `error/1`, `exit/1` |
| Alignment with gen_server | Yes | No (intentional for Petri nets) |

---

## Modules Affected

### Core Modules

All pattern modules in `/Users/sac/cre/src/patterns/` were updated:

| Module | Pattern | Behavior |
|--------|---------|----------|
| `parallel_split.erl` | WCP-02 - Parallel Split | gen_yawl |
| `or_join.erl` | WCP-03 - Synchronization | gen_yawl |
| `exclusive_choice.erl` | WCP-04 - Exclusive Choice | gen_yawl |
| `simple_merge.erl` | WCP-05 - Simple Merge | gen_yawl |
| `multiple_choice.erl` | WCP-06 - Multi-Choice | gen_yawl |
| `multiple_merge.erl` | WCP-07 - Synchronizing Merge | gen_yawl |
| `discriminator.erl` | WCP-09 - Discriminator | gen_yawl |
| `n_out_of_m.erl` | WCP-10 - N-out-of-M | gen_yawl |
| `implicit_termination.erl` | WCP-11 - Implicit Termination | gen_yawl |
| `multiple_instances_sync.erl` | WCP-13 - Multi-Instance Static | gen_yawl |
| `deferred_choice.erl` | WCP-16 - Deferred Choice | gen_yawl |
| `interleaved_routing.erl` | WCP-17 - Interleaved Routing | gen_yawl |
| `milestone.erl` | WCP-18 - Milestone | gen_yawl |
| `structured_loop.erl` | WCP-23 - Structured Loop | gen_yawl |
| `critical_section.erl` | WCP-26 - Critical Section | gen_yawl |
| `param_pass.erl` | WDP-01 - Parameter Passing | gen_yawl |
| `data_transform.erl` | WDP-02 - Data Transform | gen_yawl |
| `data_distribute.erl` | WDP-03 - Data Distribute | gen_yawl |
| `data_accumulate.erl` | WDP-04 - Data Accumulate | gen_yawl |
| `data_visibility.erl` | WDP-05 - Data Visibility | gen_yawl |
| `direct_resource_creation.erl` | WRP-01 - Resource Creation | gen_yawl |
| `role_based_allocation.erl` | WRP-02 - Role Allocation | gen_yawl |
| `resource_initialization.erl` | WRP-03 - Resource Init | gen_yawl |
| `resource_deallocation.erl` | WRP-04 - Resource Dealloc | gen_yawl |
| `resource_allocation.erl` | WRP-05 - Capability Alloc | gen_yawl |

### Behavior Modules

| Module | Purpose | Changes |
|--------|---------|---------|
| `/Users/sac/cre/src/core/gen_pnet.erl` | Core Petri net behavior | Updated callback specs |
| `/Users/sac/cre/src/core/gen_yawl.erl` | YAWL workflow wrapper | New module with enhanced fire/3 |

---

## Migration Guide for Custom Patterns

If you have custom gen_pnet modules, follow these steps:

### Step 1: Update fire/3 Return Values

**Old code:**
```erlang
fire('t_example', #{'p_input' := [token]}, _UsrInfo) ->
    {consume, #{'p_input' => []}, produce, #{'p_output' => [result]}}.
```

**New code:**
```erlang
fire('t_example', #{'p_input' := [token]}, _UsrInfo) ->
    {produce, #{
        'p_input' => [],
        'p_output' => [result]
    }}.
```

**With state update (gen_yawl only):**
```erlang
fire('t_example', #{'p_input' := [token]}, #my_state{count = N} = State) ->
    {produce, #{
        'p_input' => [],
        'p_output' => [result]
    }, State#my_state{count = N + 1}}.
```

### Step 2: Update init/1 Return Values

**Old code:**
```erlang
init(InitArg) ->
    State = #my_state{value = InitArg},
    {ok, State}.
```

**New code:**
```erlang
init(InitArg) ->
    #my_state{value = InitArg}.
```

### Step 3: Change Behavior Declaration

If using the enhanced 3-tuple fire/3:

**Old code:**
```erlang
-module(my_pattern).
-behaviour(gen_pnet).
```

**New code:**
```erlang
-module(my_pattern).
-behaviour(gen_yawl).
```

### Step 4: Update Callback Specs

**Old code:**
```erlang
-spec fire(atom(), map(), term()) ->
    {consume, map(), produce, map()} | abort.
```

**New code:**
```erlang
-spec fire(atom(), map(), term()) ->
    {produce, map()} | {produce, map(), term()} | abort.
```

**Old code:**
```erlang
-spec init(term()) -> {ok, term()}.
```

**New code:**
```erlang
-spec init(term()) -> term().
```

---

## Code Examples

### Example 1: Simple Transition (No State Update)

**Before (Old):**
```erlang
-module(example_pattern).
-behaviour(gen_pnet).

fire('t_forward', #{'p_input' := [_Token]}, _UsrInfo) ->
    {consume, #{'p_input' => []}, produce, #{'p_output' => [forwarded]}}.

init(InitArg) ->
    State = #example_state{data = InitArg},
    {ok, State}.
```

**After (New - gen_pnet):**
```erlang
-module(example_pattern).
-behaviour(gen_pnet).

fire('t_forward', #{'p_input' := [_Token]}, _UsrInfo) ->
    {produce, #{
        'p_input' => [],
        'p_output' => [forwarded]
    }}.

init(InitArg) ->
    #example_state{data = InitArg}.
```

### Example 2: Transition With State Update

**Before (Old):**
```erlang
-module(counter_pattern).
-behaviour(gen_pnet).

fire('t_increment', #{'p_input' := [inc]}, #counter_state{value = N} = State) ->
    %% Had to use handle_call to update state
    {consume, #{'p_input' => []}, produce, #{'p_output' => [N + 1]}},
    %% State update had to happen elsewhere
    {noreply, State#counter_state{value = N + 1}}.
```

**After (New - gen_yawl):**
```erlang
-module(counter_pattern).
-behaviour(gen_yawl).

fire('t_increment', #{'p_input' := [inc]}, #counter_state{value = N} = State) ->
    {produce, #{
        'p_input' => [],
        'p_output' => [N + 1]
    }, State#counter_state{value = N + 1}}.
```

### Example 3: Complete Pattern Module

**Before (Old):**
```erlang
-module(old_pattern).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {count}).

place_lst() -> ['p_in', 'p_out'].
trsn_lst() -> ['t_process'].

init_marking('p_in', _State) -> [start];
init_marking(_, _State) -> [].

preset('t_process') -> ['p_in'];
preset(_) -> [].

is_enabled('t_process', #{'p_in' := [start]}, _State) -> true;
is_enabled(_, _, _) -> false.

fire('t_process', Mode, #state{count = N}) ->
    {consume, #{'p_in' => []}, produce, #{'p_out' => [done]}}.

init(InitArg) ->
    {ok, #state{count = InitArg}}.

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
```

**After (New - gen_yawl):**
```erlang
-module(new_pattern).
-behaviour(gen_yawl).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, trigger/3]).

-record(state, {count}).

place_lst() -> ['p_in', 'p_out'].
trsn_lst() -> ['t_process'].

init_marking('p_in', _State) -> [start];
init_marking(_, _State) -> [].

preset('t_process') -> ['p_in'];
preset(_) -> [].

is_enabled('t_process', #{'p_in' := [start]}, _State) -> true;
is_enabled(_, _, _) -> false.

fire('t_process', _Mode, #state{count = N} = State) ->
    {produce, #{
        'p_in' => [],
        'p_out' => [done]
    }, State#state{count = N + 1}}.

init(InitArg) ->
    #state{count = InitArg}.

handle_call(_Request, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {error, bad_msg}, NetState}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

handle_info(_Info, NetState) ->
    {noreply, NetState}.

terminate(_Reason, _NetState) ->
    ok.

trigger(_Place, _Token, _NetState) ->
    pass.
```

---

## Testing Recommendations

### 1. Unit Testing

Test each transition's `fire/3` callback:

```erlang
%% Test fire/3 returns 2-tuple or 3-tuple
fire_returns_produce_tuple_test() ->
    State = #my_state{},
    Result = my_pattern:fire('t_example', #{'p_input' => [start]}, State),
    ?assertMatch({produce, _Map}, Result),
    %% Optionally check for 3-tuple with state update
    case Result of
        {produce, _Map, _NewState} -> ok;  % gen_yawl style
        {produce, _Map} -> ok              % gen_pnet style
    end.

%% Test init/1 returns plain state (not {ok, State})
init_returns_state_test() ->
    Result = my_pattern:init(initial_arg),
    ?assertNotMatch({ok, _}, Result),
    ?assert(is_record(Result, my_state)).
```

### 2. Integration Testing

Test the full workflow:

```erlang
%% Test workflow completes successfully
workflow_completion_test() ->
    {ok, Pid} = gen_yawl:start_link(my_pattern, init_arg, []),
    {ok, Result} = gen_yawl:sync(Pid, 5000),
    ?assertMatch(#{'p_complete' := [_]}, Result),
    gen_yawl:stop(Pid).
```

### 3. OTP Version Testing

Test across OTP versions:

```bash
# Test with OTP 25
kerl list 25
rebar3 eunit

# Test with OTP 26
kerl list 26
rebar3 eunit

# Test with OTP 27
kerl list 27
rebar3 eunit

# Test with OTP 28
kerl list 28
rebar3 eunit
```

### 4. Dialyzer Type Checking

Ensure types are correct:

```bash
rebar3 dialyzer
```

---

## Backward Compatibility

### gen_pnet vs gen_yawl

| Feature | gen_pnet | gen_yawl |
|---------|----------|----------|
| `fire/3` 2-tuple | Yes | Yes |
| `fire/3` 3-tuple | No | Yes |
| `init/1` plain State | Yes | Yes |
| `init/1` `{ok, State}` | No | No |
| Direct `gen_pnet:start_link` | Yes | No |
| YAWL workflow support | Basic | Full |

### Migration Strategy

For existing code:

1. **If using plain gen_pnet** - Update to 2-tuple `fire/3` and plain `init/1`
2. **If needing state updates** - Switch to `gen_yawl` behavior and use 3-tuple `fire/3`
3. **If maintaining compatibility** - Create adapter functions

### Adapter Pattern Example

```erlang
%% Adapter for old-style modules
-module(old_pattern_adapter).
-behaviour(gen_yawl).

%% Wrap old fire/3
fire(Trsn, Mode, UsrInfo) ->
    case old_pattern:fire(Trsn, Mode, UsrInfo) of
        {consume, _ConsumeMap, produce, ProduceMap} ->
            {produce, ProduceMap};
        abort ->
            abort
    end.

%% Wrap old init/1
init(InitArg) ->
    {ok, State} = old_pattern:init(InitArg),
    State.
```

---

## Appendix A: Complete Callback Reference

### gen_pnet Callbacks (OTP 25-28)

```erlang
-callback place_lst() -> [atom()].
-callback trsn_lst() -> [atom()].
-callback init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].
-callback preset(Trsn :: atom()) -> [atom()].
-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
    boolean().
-callback fire(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
    abort | {produce, #{atom() => [term()]}}.
-callback init(NetArg :: term()) -> term().
-callback trigger(Place :: atom(), Token :: term(), NetState :: #net_state{}) ->
    pass | drop.
-callback handle_call(Request :: term(), From :: {pid(), term()}, NetState :: #net_state{}) ->
    {reply, term()} | {reply, term(), #{atom() => [term()]}} | noreply |
    {noreply, #{atom() => [term()]}} | {stop, term(), term()}.
-callback handle_cast(Request :: term(), NetState :: #net_state{}) ->
    noreply | {noreply, #{atom() => [term()]}} | {stop, term()}.
-callback handle_info(Info :: term(), NetState :: #net_state{}) ->
    noreply | {noreply, #{atom() => [term()]}} | {stop, term()}.
-callback terminate(Reason :: term(), NetState :: #net_state{}) -> ok.
-callback code_change(OldVsn :: term(), NetState :: #net_state{}, Extra :: term()) ->
    {ok, #net_state{}} | {error, term()}.
```

### gen_yawl Callbacks (OTP 25-28 Enhanced)

```erlang
-callback place_lst() -> [atom()].
-callback trsn_lst() -> [atom()].
-callback init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].
-callback preset(Trsn :: atom()) -> [atom()].
-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
    boolean().
-callback fire(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
    abort |
    {produce, #{atom() => [term()]}} |
    {produce, #{atom() => [term()]}, NewUsrInfo :: term()}.
-callback init(NetArg :: term()) -> term().
-callback trigger(Place :: atom(), Token :: term(), NetState :: term()) ->
    pass | drop.
-callback handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
    {reply, term()} | {reply, term(), #{atom() => [term()]}} | noreply |
    {noreply, #{atom() => [term()]}} | {stop, term(), term()}.
-callback handle_cast(Request :: term(), NetState :: term()) ->
    noreply | {noreply, #{atom() => [term()]}} | {stop, term()}.
-callback handle_info(Info :: term(), NetState :: term()) ->
    noreply | {noreply, #{atom() => [term()]}} | {stop, term()}.
-callback terminate(Reason :: term(), NetState :: term()) -> ok.
-callback code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
    {ok, term()} | {error, term()}.
```

---

## Appendix B: Troubleshooting

### Common Errors

**Error:** `{badmatch, {ok, State}}` in init/1

**Solution:** Remove the `{ok, _}` wrapper:
```erlang
%% Wrong
init(Arg) -> {ok, #state{arg = Arg}}.

%% Correct
init(Arg) -> #state{arg = Arg}.
```

**Error:** `{badmatch, {consume, _, produce, _}}` in fire/3

**Solution:** Use 2-tuple or 3-tuple format:
```erlang
%% Wrong (old style)
fire(T, M, S) -> {consume, C, produce, P}.

%% Correct (new style)
fire(T, M, S) -> {produce, maps:merge(C, P)}.
```

**Error:** `function_clause` in fire/3

**Solution:** Ensure all return values match the spec:
```erlang
%% Add fallback clause
fire(_Trsn, _Mode, _UsrInfo) -> abort.
```

---

**Document Version:** 1.0
**Last Updated:** 2026-02-06
**Related Documents:**
- `/Users/sac/cre/docs/FINAL_MERGE_SUMMARY.md`
- `/Users/sac/cre/docs/RELEASE_NOTES_2.1.0.md`
- `/Users/sac/cre/src/core/gen_pnet.erl`
- `/Users/sac/cre/src/core/gen_yawl.erl`
