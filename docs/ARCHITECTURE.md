# CRE Architecture - Joe Armstrong Design

**CRE Version:** 0.2.0 / 2.1.0
**OTP Support:** 25.0, 26, 27, 28
**Last Updated:** 2026-02-06

---

## Executive Summary

CRE (Common Runtime Environment) is a YAWL (Yet Another Workflow Language) workflow engine built on Erlang/OTP, implementing the Joe Armstrong design philosophy: **one real OTP runner, everything else pure helpers/utilities**.

The system uses **Petri nets** as its formal foundation, with `gen_pnet` as the sole OTP behavior maintaining state. All other modules are pure functional utilities that transform data without side effects.

---

## Core Design Philosophy

### Joe Armstrong Principle

> "One real OTP runner (gen_pnet), everything else pure helpers/utilities"

**Key implications:**
- Only `gen_pnet` (and its wrapper `gen_yawl`) are OTP processes maintaining state
- All workflow logic lives in pure functional modules
- Message contracts define communication patterns
- State changes flow through token production/consumption
- Deterministic execution through pure functions

---

## System Architecture

### Component Layers

```
┌─────────────────────────────────────────────────────────────────┐
│                        Application Layer                         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐               │
│  │ cre_app     │  │ cre_sup     │  │ cre_master  │               │
│  └─────────────┘  └─────────────┘  └─────────────┘               │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                         OTP Runner Layer                        │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  gen_yawl (wrapper) ───────► gen_pnet (state machine)   │  │
│  │  - 3-tuple fire/3 support   - Token management          │  │
│  │  - usr_info updates         - Transition firing         │  │
│  │  - Timeout handling          - Progress loop            │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                       Pure Helper Modules                       │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐           │
│  │ pnet_*       │  │ wf_*         │  │ yawl_*       │           │
│  │ (pure)       │  │ (pure)       │  │ (utilities)  │           │
│  └──────────────┘  └──────────────┘  └──────────────┘           │
│  ┌──────────────────────────────────────────────────────┐      │
│  │ src/patterns/*.erl (gen_yawl behaviors)              │      │
│  │ - Each pattern is a gen_pnet behavior                │      │
│  │ - Pure functional helper logic                         │      │
│  └──────────────────────────────────────────────────────┘      │
└─────────────────────────────────────────────────────────────────┘
```

---

## Module Organization

### Directory Structure

| Directory | Purpose | OTP Components |
|-----------|---------|----------------|
| `src/core/` | Core OTP behaviors | `gen_pnet`, `gen_yawl` |
| `src/pnet/` | Petri Net pure utilities | Stateless functions |
| `src/wf/` | Workflow utilities | Stateless functions |
| `src/yawl/` | YAWL-specific modules | `gen_server` (engine only) |
| `src/patterns/` | Workflow patterns | `gen_yawl` behaviors |
| `src/api/` | Client APIs | `gen_server` |
| `src/http/` | HTTP handlers | Cowboy handlers |
| `src/integration/` | External integrations | `gen_server` |

### Core OTP Behaviors

#### gen_pnet - The Single OTP Runner

**File:** `/Users/sac/cre/src/core/gen_pnet.erl`

The only OTP behavior that maintains Petri net state. All workflow nets implement this behavior.

**Structure Callbacks (define the net):**
- `place_lst/0` - Returns list of place atoms
- `trsn_lst/0` - Returns list of transition atoms
- `init_marking/2` - Initial token distribution
- `preset/1` - Input places for each transition
- `is_enabled/3` - Check if transition can fire
- `fire/3` - Produce tokens when transition fires

**Interface Callbacks (process interaction):**
- `init/1` - Initialize user info
- `handle_call/3` - Synchronous messages
- `handle_cast/2` - Asynchronous messages
- `handle_info/2` - Unformatted messages
- `code_change/3` - Hot code reload
- `terminate/2` - Cleanup
- `trigger/3` - Filter produced tokens

#### gen_yawl - Enhanced Wrapper

**File:** `/Users/sac/cre/src/core/gen_yawl.erl`

Wrapper around `gen_pnet` that supports 3-tuple returns from `fire/3`:

```erlang
%% Standard 2-tuple (gen_pnet compatible)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap}

%% Enhanced 3-tuple (gen_yawl extension)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap, NewUsrInfo}
```

The 3-tuple form allows automatic `usr_info` updates during transition firing, essential for workflow state tracking.

---

## Pure Helper Modules

### Petri Net Utilities (src/pnet/)

| Module | Purpose | State |
|--------|---------|-------|
| `pnet_types` | Type validators | Pure |
| `pnet_marking` | Multiset marking algebra | Pure |
| `pnet_mode` | Mode enumeration | Pure |
| `pnet_receipt` | Audit trail records | Pure |
| `pnet_choice` | Deterministic nondeterminism | Pure |

**Key Example: Marking Algebra**

```erlang
%% Create empty marking
M0 = pnet_marking:new([p1, p2, p3]).

%% Add tokens (multiset union)
M1 = pnet_marking:add(M0, #{p1 => [a, b], p2 => [c]}).

%% Consume tokens (multiset subtraction)
{ok, M2} = pnet_marking:take(M1, #{p1 => [a]}).
%% M2 now has: p1 => [b], p2 => [c], p3 => []
```

### Workflow Utilities (src/wf/)

Pure helper modules for workflow operations:

- `wf_spec` - YAWL specification parsing
- `wf_engine` - Workflow execution helpers
- `wf_task` - Task token constructors
- `wf_scope` - Boundary mapping
- `wf_timerq` - Deadline queue for token injection
- `wf_rules` - Business rule evaluation
- `wf_data` - Data flow operations

### YAWL Compiler (src/core/)

- `yawl_validate` - Specification validation
- `yawl_compile` - YAWL to gen_pnet compilation
- `yawl_compiled` - Compiled net execution

---

## Pattern Module Structure

### Pattern Organization

All 43 YAWL workflow patterns live in `/Users/sac/cre/src/patterns/`:

| Category | Patterns (26/43 implemented) |
|----------|----------------------------|
| Basic Control Flow (WCP-01 to WCP-10) | `parallel_split`, `or_join`, `exclusive_choice`, `simple_merge`, `multiple_choice`, `multiple_merge`, `discriminator`, `n_out_of_m`, `implicit_termination` |
| Advanced Synchronization | `structured_loop`, `milestone`, `critical_section` |
| State-Based | `deferred_choice`, `interleaved_routing` |
| Data Flow (WDP-01 to WDP-05) | `param_pass`, `data_transform`, `data_distribute`, `data_accumulate`, `data_visibility` |
| Resource Patterns (WRP-01 to WRP-05) | `direct_resource_creation`, `role_based_allocation`, `resource_initialization`, `resource_allocation`, `resource_deallocation` |

### Pattern Structure Example

Each pattern is a `gen_yawl` behavior:

```erlang
-module(parallel_split).
-behaviour(gen_yawl).

%% Petri net places
place_lst() -> [p_start, p_branch1, p_branch2, p_join_ready, p_end].

%% Transitions
trsn_lst() -> [t_split, t_join_branch1, t_join_branch2, t_finish].

%% Initial tokens
init_marking(p_start, _UsrInfo) -> [start];
init_marking(_, _UsrInfo) -> [].

%% Transition wiring
preset(t_split) -> [p_start];
preset(t_join_branch1) -> [p_branch1];
preset(t_finish) -> [p_join_ready];
preset(_) -> [].

%% Enablement logic
is_enabled(t_split, _, _) -> true;
is_enabled(t_finish, #{p_join_ready := Tokens}, #parallel_split_state{branch_count = N}) ->
    length(Tokens) =:= N;
is_enabled(_, _, _) -> false.

%% Firing logic (produces tokens, optionally updates usr_info)
fire(t_split, #{p_start := [start]}, #parallel_split_state{branch_count = 2}) ->
    {produce, #{
        p_start => [],
        p_branch1 => [{branch, 1}],
        p_branch2 => [{branch, 2}]
    }};
fire(t_finish, _, _) ->
    {produce, #{p_join_ready => [], p_end => [complete]}}.
```

---

## Data Flow Through the System

### Token Flow

```
┌────────────────┐
│  Client Request │
└────────┬────────┘
         │
         ▼
┌──────────────────────────────────────────────────────────────┐
│  gen_yawl:start_link(NetMod, InitArg)                       │
│  └─ Creates gen_pnet process                                 │
│  └─ Calls NetMod:init/1 to get UsrInfo                       │
│  └─ Initializes marking from place_lst() and init_marking/2    │
└────────┬─────────────────────────────────────────────────────┘
         │
         ▼
┌──────────────────────────────────────────────────────────────┐
│  Progress Loop (gen_pnet)                                     │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │ 1. Find enabled transitions                             │ │
│  │    - For each transition: enumerate modes from preset    │ │
│  │    - Check is_enabled/3 for each mode                    │ │
│  │                                                          │ │
│  │ 2. Pick random enabled transition and mode              │ │
│  │    - lib_combin:pick_from (deterministic nondeterminism) │ │
│  │                                                          │ │
│  │ 3. Call fire/3                                          │ │
│  │    - Returns {produce, ProduceMap}                       │ │
│  │    - Or {produce, ProduceMap, NewUsrInfo} (gen_yawl)     │ │
│  │                                                          │ │
│  │ 4. Consume tokens from mode                             │ │
│  │    - Multiset subtraction                               │ │
│  │                                                          │ │
│  │ 5. Call trigger/3 for each produced token               │ │
│  │    - Returns pass or drop                                │ │
│  │                                                          │ │
│  │ 6. Add passed tokens to marking                         │ │
│  │    - Multiset union                                     │ │
│  │                                                          │ │
│  │ 7. Continue until no transitions enabled                │ │
│  └─────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```

### State Management

**All state lives in the gen_pnet process:**

```erlang
-record(net_state, {
    net_mod :: atom(),          % Module implementing callbacks
    usr_info :: term(),         % User-defined state (updated via fire/3)
    marking :: #{atom() => [[term()]]},  % Place -> tokens
    stats :: #stats{} | undefined,
    tstart :: integer(),
    cnt :: non_neg_integer()
}).
```

**No other modules maintain state** - they are pure functions that:

1. Take input (markings, modes, usr_info)
2. Return transformed output
3. Have no side effects
4. Are easily testable

---

## Module Dependencies

### Dependency Graph

```
gen_yawl (OTP)
  │
  ├─► gen_pnet (OTP) ──────────────────────────────────────┐
  │     │                                                   │
  │     ├─► pnet_types (pure) ── Type validators           │
  │     ├─► pnet_marking (pure) ── Marking algebra          │
  │     ├─► pnet_mode (pure) ──── Mode enumeration         │
  │     ├─► pnet_receipt (pure) ── Receipt tracking         │
  │     └─► pnet_choice (pure) ─── Deterministic choice     │
  │                                                         │
  └─► cre_yawl_worker (cre_worker behavior)               │
                                                           │
Pattern modules (gen_yawl behaviors):                       │
  ├─► parallel_split, exclusive_choice, etc.               │
  │     │                                                   │
  │     └─► Use pnet_* utilities for logic                  │
  │                                                         │
Workflow utilities (pure):                                 │
  ├─► wf_spec, wf_engine, wf_task, wf_scope, etc.        │
  │     │                                                   │
  │     └─► Transform data, no state                        │
  │                                                         │
YAWL utilities (mixed):                                     │
  ├─► yawl_engine (gen_server) ── Workflow case mgmt       │
  ├─► yawl_validate (pure) ───── Specification validation │
  ├─► yawl_compile (pure) ────── YAWL → gen_pnet code     │
  └─► yawl_marshal, yawl_persistence, etc.                  │
                                                           │
└───────────────────────────────────────────────────────────┘
```

---

## Petri Net Structure in Patterns

### Place and Transition Naming Conventions

**Places** represent state/conditions:
- `p_start` - Entry point
- `p_end` - Exit/completion
- `p_<function>` - Functional states (e.g., `p_choice`, `p_join_ready`)
- `p_branch<N>` - Parallel branch places

**Transitions** represent actions:
- `t_<action>` - Action name (e.g., `t_split`, `t_finish`, `t_join_branch1`)

### Example: Parallel Split

```erlang
%% Places: where tokens reside
place_lst() -> [p_start, p_branch1, p_branch2, p_join_ready, p_end].

%% Transitions: what moves tokens
trsn_lst() -> [t_split, t_join_branch1, t_join_branch2, t_finish].

%% Wiring: what connects to what
preset(t_split) -> [p_start].           % t_split consumes from p_start
preset(t_join_branch1) -> [p_branch1].  % t_join_branch1 consumes from p_branch1
preset(t_finish) -> [p_join_ready].     % t_finish consumes from p_join_ready
```

### Token Types

Tokens can be any Erlang term:
- Atoms for control flow: `start`, `complete`, `done`
- Tuples for data: `{branch, 1}`, `{data, Value}`
- Complex structures: `{task_complete, TaskId, Result}`

---

## Integration Points

### Starting a Workflow

```erlang
%% Direct gen_yawl start
{ok, Pid} = gen_yawl:start_link(parallel_split,
    #{branch_funs => [Fun1, Fun2], branch_count => 2},
    [{fire_timeout, 10000}]).

%% Query state
{ok, Marking} = gen_yawl:sync(Pid, 5000).

%% Get user info (pattern state)
UsrInfo = gen_yawl:usr_info(Pid).

%% Stop
gen_yawl:stop(Pid).
```

### Compiling YAWL Specifications

```erlang
%% Parse YAWL XML
{ok, Spec} = wf_spec:from_xml_file("workflow.yawl"),

%% Validate
{ok, Warnings} = yawl_validate:validate(Spec),

%% Compile to gen_pnet module
{ok, Compiled} = yawl_compile:compile(Spec, #{}),

%% Write to file
{ok, Files} = yawl_compile:compile_to_file(Spec, #{}, "src/compiled").
```

### Worker Integration

```erlang
%% Workers implement cre_worker behavior (gen_pnet-based)
%% 12 places model: stagein → execution → stageout

%% Start worker
{ok, Pid} = cre_worker:start_link(cre_master, my_worker, #{}),

%% Send work request
cre_worker:worker_request(Pid, #{task => data}),

%% Worker lifecycle:
%% WorkerRequest → Stagein → StageinOk/StageinError → PreSync →
%% Result/Error → Stageout → StageoutOk/StageoutError → PostSync →
%% WorkerResult
```

---

## State Management Approach

### Single Source of Truth

**All workflow state lives in `#net_state{}`:**

```erlang
-record(net_state, {
    net_mod :: atom(),                  % Module implementing the pattern
    usr_info :: term(),                 % Pattern-specific state (record)
    marking :: #{atom() => [[term()]]}, % Petri net marking
    stats :: #stats{} | undefined,      % Performance stats
    tstart :: integer(),                 % Start time for stats
    cnt :: non_neg_integer()            % Firing counter
}).
```

### State Updates via fire/3

**Gen_pnet (no state update):**
```erlang
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap}.
```

**Gen_yawl (with state update):**
```erlang
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap, NewUsrInfo}.
```

The 3-tuple form automatically updates `usr_info` in `#net_state{}`.

### Pattern State Records

Each pattern defines its state record:

```erlang
-record(parallel_split_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

This record is the `usr_info` field, updated via 3-tuple `fire/3` returns.

---

## YAWL Pattern Implementation Status

### Implemented Patterns (26/43)

| ID | Pattern Name | Module | Status |
|----|-------------|--------|--------|
| WCP-02 | Parallel Split | `parallel_split` | Complete |
| WCP-03 | Synchronization | `or_join` | Complete |
| WCP-04 | Exclusive Choice | `exclusive_choice` | Complete |
| WCP-05 | Simple Merge | `simple_merge` | Complete |
| WCP-06 | Multi-Choice | `multiple_choice` | Complete |
| WCP-07 | Synchronizing Merge | `multiple_merge` | Complete |
| WCP-09 | Discriminator | `discriminator` | Complete |
| WCP-10 | N-out-of-M | `n_out_of_m` | Complete |
| WCP-11 | Implicit Termination | `implicit_termination` | Complete |
| WCP-13 | Multi-Instance (Static) | `multiple_instances_sync` | Complete |
| WCP-16 | Deferred Choice | `deferred_choice` | Complete |
| WCP-17 | Interleaved Routing | `interleaved_routing` | Complete |
| WCP-18 | Milestone | `milestone` | Complete |
| WCP-23 | Structured Loop | `structured_loop` | Complete |
| WCP-26 | Critical Section | `critical_section` | Complete |
| WDP-01 | Parameter Passing | `param_pass` | Complete |
| WDP-02 | Data Transform | `data_transform` | Complete |
| WDP-03 | Data Distribute | `data_distribute` | Complete |
| WDP-04 | Data Accumulate | `data_accumulate` | Complete |
| WDP-05 | Data Visibility | `data_visibility` | Complete |
| WRP-01 | Resource Creation | `direct_resource_creation` | Complete |
| WRP-02 | Role Allocation | `role_based_allocation` | Complete |
| WRP-03 | Resource Init | `resource_initialization` | Complete |
| WRP-04 | Resource Dealloc | `resource_deallocation` | Complete |
| WRP-05 | Capability Alloc | `resource_allocation` | Complete |

### Remaining Patterns (17 needed)

| Category | Missing Patterns |
|----------|------------------|
| Advanced Synchronization | Structured Synchronization, Partial Join |
| Multiple Instances | No Sync, Runtime Knowledge, Without Prior Knowledge |
| State-Based | Cancel Activity, Cancel Case |
| Extended Control Flow | Recursion, Interleaved Loop, Protocol Pattern |
| Exception Handling | Error Handler, Retry, Compensation (3 variants) |

---

## OTP Version Support

| OTP Version | Support | Notes |
|-------------|---------|-------|
| 25.0 | Full | Minimum supported version |
| 26.x | Full | Tested and passing |
| 27.x | Full | Tested and passing |
| 28.x | Full | Primary target (2026) |
| < 25.0 | Dropped | No longer supported |

---

## File Locations

| Component | Path |
|-----------|------|
| Core OTP behaviors | `/Users/sac/cre/src/core/` |
| Petri net utilities | `/Users/sac/cre/src/pnet/` |
| Workflow utilities | `/Users/sac/cre/src/wf/` |
| YAWL modules | `/Users/sac/cre/src/yawl/` |
| Pattern implementations | `/Users/sac/cre/src/patterns/` |
| API modules | `/Users/sac/cre/src/api/` |
| HTTP handlers | `/Users/sac/cre/src/http/` |
| Integration modules | `/Users/sac/cre/src/integration/` |
| Tests | `/Users/sac/cre/test/` |
| Documentation | `/Users/sac/cre/docs/` |

---

## References

- [YAWL Pattern Reference](./YAWL_PATTERNS_REFERENCE.md) - Complete 43-pattern documentation
- [API Reference](./COMPLETE_API_REFERENCE.md) - Full API documentation
- [gen_pnet User Guide](./GEN_PNET_USER_GUIDE.md) - Core behavior documentation
- [TEST_STATUS](./TEST_STATUS.md) - Current test results
- [FINAL_MERGE_SUMMARY](./FINAL_MERGE_SUMMARY.md) - Recent merge details

---

**Document Version:** 2.0
**Generated:** 2026-02-06
**Reflects:** Joe Armstrong refactor, new pattern structure
