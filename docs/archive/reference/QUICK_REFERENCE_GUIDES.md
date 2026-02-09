# CRE Quick Reference Guides

**Version:** 0.3.0 | **OTP:** 25-28 | **Last Updated:** 2026-02-08

---

## 1. Workflow Creation - 5-Step Process

### Step 1: Define the Module
```erlang
-module(my_workflow).
-behaviour(gen_yawl).

-export([place_lst/0, trsn_lst/0, preset/1, init/1,
         init_marking/2, is_enabled/3, fire/3, trigger/3]).
```

### Step 2: Declare Places and Transitions
```erlang
%% Places represent states/conditions
place_lst() -> [p_start, p_task1, p_decision, p_end].

%% Transitions represent actions
trsn_lst() -> [t_start, t_task1, t_decision, t_end].
```

### Step 3: Define Presets (Wiring)
```erlang
preset(t_start) -> [p_start].
preset(t_task1) -> [p_start].
preset(t_decision) -> [p_task1].
preset(t_end) -> [p_decision].
preset(_) -> [].
```

### Step 4: Define Marking and Enablement
```erlang
init(_NetArg) -> [].

init_marking(p_start, _UsrInfo) -> [start];
init_marking(_Place, _UsrInfo) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.
```

### Step 5: Define Firing Logic
```erlang
%% 2-tuple return (gen_pnet compatible)
fire(t_start, #{p_start := [start]}, _UsrInfo) ->
    {produce, #{p_start => [], p_task1 => [enabled]}};

%% 3-tuple return (gen_yawl extension - updates usr_info)
fire(t_decision, #{p_task1 := [done]}, UsrInfo) ->
    NewUsrInfo = maps:put(decision_made, true, UsrInfo),
    {produce, #{p_task1 => [], p_end => [complete]}, NewUsrInfo};
```

### Quick Template
```erlang
-module(template).
-behaviour(gen_yawl).

-record(state, {counter = 0}).

-export([place_lst/0, trsn_lst/0, preset/1, init/1,
         init_marking/2, is_enabled/3, fire/3, trigger/3]).

place_lst() -> [p_start, p_end].
trsn_lst() -> [t_go].

preset(t_go) -> [p_start];
preset(_) -> [].

init(_) -> #state{}.
init_marking(p_start, _) -> [init];
init_marking(_, _) -> [].

is_enabled(_, _, _) -> true.

fire(t_go, #{p_start := [init]}, State) ->
    {produce, #{p_start => [], p_end => [done]}, State}.

trigger(_, _, _) -> {error, no_trigger}.
```

---

## 2. Pattern Reference - All 43 Patterns

| ID | Pattern Name | Module | Category | Description |
|----|--------------|--------|----------|-------------|
| P1 | Sequence | `sequence` | Basic | Execute tasks in order |
| P2 | Parallel Split | `parallel_split` | Basic | Split into concurrent branches |
| P3 | Synchronization | `synchronization` | Basic | Wait for all branches (AND-join) |
| P4 | Exclusive Choice | `exclusive_choice` | Basic | Choose one branch (XOR-split) |
| P5 | Simple Merge | `simple_merge` | Basic | Merge one path (XOR-join) |
| P6 | Multi-Choice | `multiple_choice` | Basic | Enable multiple branches (OR-split) |
| P7 | Structured Sync Merge | `structured_sync_merge` | Advanced | Synchronized merge with conditions |
| P8 | Multiple Merge | `multiple_merge` | Advanced | Merge multiple paths |
| P9 | Discriminator | `discriminator` | Advanced | Pass first N, ignore rest |
| P10 | N-out-of-M | `n_out_of_m` | Advanced | Wait for threshold |
| P11 | Implicit Termination | `implicit_termination` | Advanced | Auto-terminate when complete |
| P12 | Multi-Instance (Static) | `multiple_instances_sync` | Multi-Instance | Fixed parallel instances |
| P13 | Multi-Instance (Dynamic) | `multi_instance` | Multi-Instance | Runtime-determined instances |
| P14 | Deferred Choice | `deferred_choice` | State-Based | Choose based on data availability |
| P15 | Interleaved Routing | `interleaved_routing` | State-Based | Alternate between branches |
| P16 | Milestone | `milestone` | State-Based | Enable/disable based on state |
| P17 | Cancel Activity | `cancel_activity` | Cancellation | Cancel single task |
| P18 | Cancel Case | `cancel_case` | Cancellation | Cancel entire workflow |
| P19 | Structured Loop | `structured_loop` | Control Flow | while/until loops |
| P20 | Recursion | `recursion` | Control Flow | Recursive workflow calls |
| P21 | Transient Trigger | `transient_trigger` | Trigger | One-time event trigger |
| P22 | Persistent Trigger | `persistent_trigger` | Trigger | Reusable event trigger |
| P23 | Cancel Region | `cancel_region` | Cancellation | Cancel workflow section |
| P24 | Cancel MI Activity | `cancel_mi_activity` | Cancellation | Cancel multi-instance |
| P25 | Complete MI Activity | `complete_mi_activity` | Multi-Instance | Force complete remaining |
| P26 | Blocking Discriminator | `blocking_discriminator` | Advanced | Block until N complete |
| P27 | Cancelling Discriminator | `cancelling_discriminator` | Cancellation | Cancel after N complete |
| P28 | Structured Partial Join | `structured_partial_join` | Multi-Instance | Partial join with structure |
| P29 | Blocking Partial Join | `blocking_partial_join` | Multi-Instance | Blocking partial join |
| P30 | Cancelling Partial Join | `cancelling_partial_join` | Cancellation | Cancel on partial join |
| P31 | Generalized AND Join | `generalized_and_join` | Advanced | General AND-join pattern |
| P32 | Static Partial Join MI | `static_partial_join_mi` | Multi-Instance | Static partial join |
| P33 | Cancelling Partial Join MI | `cancelling_partial_join_mi` | Cancellation | Cancel MI on partial |
| P34 | Dynamic Partial Join MI | `dynamic_partial_join_mi` | Multi-Instance | Dynamic partial join |
| P35 | Local Sync Merge | `local_sync_merge` | Advanced | Local synchronization |
| P36 | General Sync Merge | `general_sync_merge` | Advanced | General synchronization |
| P37 | Critical Section | `critical_section` | Concurrency | Mutual exclusion |
| P38 | Interleaved Parallel | `interleaved_parallel` | Concurrency | Interleaved execution |
| P39 | Thread Merge | `thread_merge` | Concurrency | Merge interleaved threads |
| P40 | Thread Split | `thread_split` | Concurrency | Split into threads |
| P41 | Arbitrary Cycles | `arbitrary_cycles` | Control Flow | Unstructured loops |
| P42 | Explicit Termination | `explicit_termination` | Control Flow | Manual termination |
| P43 | Exception Patterns | `exception_patterns` | Exception | Try/catch/retry |

### Data Flow Patterns (WDP)
| ID | Pattern | Module | Description |
|----|---------|--------|-------------|
| WDP-01 | Parameter Passing | `param_pass` | Pass data between tasks |
| WDP-02 | Data Transform | `data_transform` | Transform data formats |
| WDP-03 | Data Distribute | `data_distribute` | Split data to branches |
| WDP-04 | Data Accumulate | `data_accumulate` | Collect data from branches |
| WDP-05 | Data Visibility | `data_visibility` | Scope data visibility |

### Resource Patterns (WRP)
| ID | Pattern | Module | Description |
|----|---------|--------|-------------|
| WRP-01 | Direct Resource Creation | `direct_resource_creation` | Create resources on demand |
| WRP-02 | Role-Based Allocation | `role_based_allocation` | Assign by role |
| WRP-03 | Resource Initialization | `resource_initialization` | Initialize resources |
| WRP-04 | Resource Deallocation | `resource_deallocation` | Release resources |
| WRP-05 | Capability Allocation | `resource_allocation` | Allocate by capability |

---

## 3. API Quick Reference

### Core CRE API
```erlang
%% Create new workflow
cre_yawl:new_workflow(Name :: binary()) -> workflow()

%% Add task to workflow
cre_yawl:add_task(Workflow, TaskId, Options) -> workflow()
  Options: [{type, atomic|approval|multi_instance},
            {module, Module},
            {function, Function},
            {cardinality, N}]

%% Connect tasks
cre_yawl:connect(Workflow, FromTask, ToTask) -> workflow()

%% Execute workflow
cre_yawl:execute(Workflow) -> {ok, Result} | {error, Reason}

%% Validate workflow
cre_yawl:validate(Workflow) -> ok | {error, Reason}
```

### gen_yawl Process API
```erlang
%% Start workflow process
gen_yawl:start_link(Module, InitArg, Opts) -> {ok, Pid}

%% Get current marking (state)
gen_yawl:sync(Pid, Timeout) -> {ok, Marking}

%% Get user info (state record)
gen_yawl:usr_info(Pid) -> term()

%% Stop workflow
gen_yawl:stop(Pid) -> ok

%% Apply function to state
gen_yawl:apply(Pid, Fun, Timeout) -> {ok, Result}
```

### Petri Net Marking API
```erlang
%% Create new marking
pnet_marking:new(Places) -> Marking

%% Get tokens from place
pnet_marking:get(Marking, Place) -> {ok, Tokens} | {error, bad_place}

%% Set tokens at place
pnet_marking:set(Marking, Place, Tokens) -> Marking

%% Add tokens (multiset union)
pnet_marking:add(Marking, ProduceMap) -> Marking

%% Take tokens (multiset subtraction)
pnet_marking:take(Marking, ConsumeMap) -> {ok, Marking} | {error, Reason}

%% Hash marking for comparison
pnet_marking:hash(Marking) -> binary()
```

### Mode Enumeration API
```erlang
%% Count tokens in preset places
pnet_mode:preset_counts(Places) -> #{Place => Count}

%% Enumerate all modes for transition
pnet_mode:enum_modes(Places, Marking) -> [Mode]

%% Enumerate colored modes (with bindings)
pnet_mode:enum_cmodes(Trsn, Marking, UsrInfo, NetMod) -> [{Binding, Mode}]
```

### Choice API (Deterministic Non-determinism)
```erlang
%% Seed RNG for reproducibility
pnet_choice:seed(Seed :: integer()) -> RngState

%% Pick random element
pnet_choice:pick(List, RngState) -> {Element, NewRngState}

%% Pick weighted random element
pnet_choice:pick_weighted([{Item, Weight}], RngState) -> {Element, NewRngState}
```

### Timer Queue API
```erlang
%% Create new timer queue
wf_timerq:new() -> TimerQ

%% Arm a timer
wf_timerq:arm(TimerQ, Key, Deadline, Event) -> NewTimerQ

%% Disarm a timer
wf_timerq:disarm(TimerQ, Key) -> NewTimerQ

%% Poll for expired timers
wf_timerq:poll(TimerQ, Now) -> {[Event], NewTimerQ}

%% Check if empty
wf_timerq:is_empty(TimerQ) -> boolean()
```

### Task Token API
```erlang
%% Create enabled task token
wf_task:enabled(TaskId, Payload, Place) -> {produce, ProduceMap}

%% Create running task token
wf_task:running(TaskId, Payload, Place) -> {produce, ProduceMap}

%% Create done task token
wf_task:done(TaskId, Payload, Place) -> {produce, ProduceMap}

%% Create failed task token
wf_task:failed(TaskId, Payload, Place) -> {produce, ProduceMap}
```

### Config API
```erlang
%% Get configuration value
cre_config:get(Key) -> Value | undefined

%% Set configuration value
cre_config:set(Key, Value) -> ok

%% Reload configuration
cre_config:reload() -> ok
```

---

## 4. Configuration

### Environment Variables
| Variable | Default | Description |
|----------|---------|-------------|
| `CRE_DEFAULT_PORT` | 4142 | HTTP service port |
| `CRE_STATUS_ROUTE` | `/[status.json]` | Status endpoint |
| `CRE_AUTH_PBKDF2_ITERATIONS` | 100000 | Password hash iterations |
| `CRE_AUTH_SESSION_TIMEOUT` | 3600 | Session timeout (seconds) |
| `YAWL_STATELESS_CHECKPOINT_DIR` | `priv/checkpoints` | Checkpoint directory |
| `YAWL_TIMEOUT_DEFAULT` | 30000 | Default timeout (ms) |
| `YAWL_TIMEOUT_DEADLOCK_INTERVAL` | 5000 | Deadlock check interval (ms) |

### sys.config Example
```erlang
[
  {cre, [
    {telemetry_enabled, true},
    {task_timeout, 30000},
    {human_in_the_loop, true},
    {dashboard, [
      {port, 8080},
      {enabled, true}
    ]},
    {logging, [
      {level, info},
      {format, text}
    ]}
  ]}
].
```

### rebar.config Key Settings
```erlang
{erl_opts, [
  debug_info,
  bin_opt_info,
  {platform_define, "^[0-9]+", 'OTP_25_PLUS'},
  {src_dirs, ["src", "src/core", "src/pnet", "src/wf", "src/patterns"]}
]}.

{deps, [
  {gen_pnet, {git, "https://github.com/joergen7/gen_pnet.git", {branch, "master"}}},
  {lib_combin, {git, "https://github.com/joergen7/lib_combin.git", {ref, "953273d"}}},
  {cowboy, "2.14.2"},
  {jsx, "3.1.0"}
]}.
```

---

## 5. Testing Commands

### Build Commands
```bash
# Compile project
rebar3 compile

# Clean build artifacts
rebar3 clean

# Format check
rebar3 efmt -c

# Format files
rebar3 efmt -w
```

### Unit Tests (EUnit)
```bash
# Run all EUnit tests
rebar3 eunit

# Run specific module tests
rebar3 eunit --module=parallel_split

# Run with coverage
rebar3 eunit --cover

# Generate coverage report
rebar3 cover
```

### Integration Tests (Common Test)
```bash
# Run all Common Test suites
rebar3 ct

# Run specific suite
rebar3 ct --suite=cre_yawl_SUITE

# Run with verbose output
rebar3 ct -v

# Run specific test case
rebar3 ct --suite=cre_yawl_SUITE --case=parallel_split_test
```

### Static Analysis
```bash
# Dialyzer type analysis
rebar3 dialyzer

# XREF dependency check
rebar3 xref

# Build PLT for Dialyzer
rebar3 dialyzer --build_plt
```

### Shell and Debugging
```bash
# Start interactive shell
rebar3 shell

# Start with config
rebar3 shell --config sys.config

# Start with node name
rebar3 shell --name mynode@localhost

# Attach to running node
rebar3 shell --setcookie mycookie --remsh mynode@localhost
```

---

## 6. Troubleshooting

### Common Issues and Fixes

| Issue | Symptom | Fix |
|-------|---------|-----|
| **OTP Version Mismatch** | Compilation errors | Use OTP 25-28: `erl -version` |
| **Module Not Found** | `{error, module_not_found}` | Run `rebar3 compile` first |
| **Place Not in Marking** | `{error, bad_place}` | Ensure place in `place_lst/0` |
| **No Modes Available** | Workflow stalls | Check `preset/1` wiring |
| **Deadlock** | No transitions fire | Verify all paths reach end |
| **State Not Updating** | usr_info unchanged | Use 3-tuple `fire/3` return |
| **Port Already in Use** | Dashboard won't start | Change port or kill process |
| **Timeout** | `{error, timeout}` | Increase timeout or fix blocking call |

### Debugging Commands
```erlang
%% Check current marking
{ok, Marking} = gen_yawl:sync(Pid, 5000).

%% Get user info
UsrInfo = gen_yawl:usr_info(Pid).

%% Check process info
erlang:process_info(Pid).

%% Check message queue
erlang:process_info(Pid, messages).

%% Enable tracing
erlang:trace(Pid, true, [send, 'receive']).

%% Check for enabled transitions
%% In gen_yawl callback, trace enabled:
is_enabled(Trsn, Mode, _UsrInfo) ->
    io:format("Checking ~p with mode ~p~n", [Trsn, Mode]),
    true.
```

### State Inspection
```erlang
%% Extract tokens from marking
{ok, Tokens} = pnet_marking:get(Marking, p_start).

%% Count tokens in place
length(Tokens).

%% Check receipt history
Receipts = pnet_receipt:extract_receipts(Marking).

%% Hash marking for comparison
Hash = pnet_marking:hash(Marking).
```

### Performance Tips
| Issue | Solution |
|-------|----------|
| Slow workflow execution | Use `is_enabled/2` caching |
| High memory usage | Call `pnet_marking:snapshot/1` |
| Token bottleneck | Batch `pnet_marking:add/2` calls |
| Timer overhead | Use `wf_timerq:disarm/2` cleanup |

---

## 7. Glossary

| Term | Definition |
|-------|------------|
| **Place** | A location in a Petri net where tokens reside; represents a state or condition |
| **Transition** | An action that consumes tokens from input places and produces tokens to output places |
| **Marking** | The distribution of tokens across all places in a Petri net |
| **Mode** | A specific binding of tokens to be consumed by a transition |
| **Token** | A unit of data that flows through places in a Petri net |
| **Preset** | The input places for a transition |
| **Postset** | The output places for a transition |
| **usr_info** | User-defined state maintained by gen_yawl, updated via 3-tuple fire/3 return |
| **gen_yawl** | OTP behavior wrapper around gen_pnet with enhanced state management |
| **gen_pnet** | Core OTP behavior implementing Petri net execution |
| **YAWL** | Yet Another Workflow Language - workflow specification standard |
| **WCP** | Workflow Control Pattern - standard workflow patterns |
| **WDP** | Workflow Data Pattern - data flow patterns |
| **WRP** | Workflow Resource Pattern - resource management patterns |
| **Receipt** | Audit trail record generated after each transition firing |
| **Binding** | Association of variables to values in colored Petri nets |
| **Colored Net** | Petri net with typed tokens and variable bindings |
| **Soundness** | Property guaranteeing workflows can complete without deadlock |
| **OTP** | Open Telecom Platform - Erlang/OTP framework |
| **Supervision Tree** | OTP hierarchical process supervision structure |
| **Hot Code Reload** | Runtime code upgrade without stopping system |

### Pattern-Specific Terms
| Term | Definition |
|-------|------------|
| **AND-split** | Execute all outgoing branches in parallel |
| **OR-split** | Execute multiple (not all) branches |
| **XOR-split** | Execute exactly one branch |
| **AND-join** | Wait for all incoming branches |
| **XOR-join** | Continue with first incoming branch |
| **Multi-instance** | Execute same task multiple times concurrently |
| **Deferred choice** | Choose branch based on runtime data availability |
| **Milestone** | Enable/disable tasks based on workflow state |
| **Critical Section** | Mutually exclusive execution region |

---

## Quick Lookup Index

### By Category
- **Basic Control Flow**: sequence, parallel_split, synchronization, exclusive_choice, simple_merge, multiple_choice
- **Advanced Sync**: structured_sync_merge, multiple_merge, discriminator, n_out_of_m, implicit_termination
- **Multi-Instance**: multiple_instances_sync, multi_instance, structured_partial_join, blocking_partial_join
- **Cancellation**: cancel_activity, cancel_case, cancel_region, cancelling_discriminator
- **State-Based**: deferred_choice, interleaved_routing, milestone
- **Loops**: structured_loop, arbitrary_cycles, recursion
- **Triggers**: transient_trigger, persistent_trigger
- **Data Flow**: param_pass, data_transform, data_distribute, data_accumulate, data_visibility
- **Resources**: direct_resource_creation, role_based_allocation, resource_initialization, resource_deallocation, resource_allocation

### By Module Location
- `/src/core/` - gen_pnet, gen_yawl
- `/src/pnet/` - pnet_marking, pnet_mode, pnet_choice, pnet_receipt, pnet_types
- `/src/wf/` - wf_task, wf_timerq, wf_scope, wf_engine, wf_spec
- `/src/patterns/` - All 43 pattern modules
- `/src/yawl/` - YAWL compiler, engine, executor
- `/src/api/` - Client APIs
- `/test/` - EUnit and Common Test suites

---

**For detailed documentation, see:**
- `docs/ARCHITECTURE.md` - System design
- `docs/YAWL_PATTERNS_REFERENCE.md` - Complete patterns guide
- `docs/QUICK_REFERENCE_CARD.md` - Developer reference
- `docs/DEPLOYMENT.md` - Production deployment
