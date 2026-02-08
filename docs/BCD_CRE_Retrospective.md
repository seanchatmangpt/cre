# Business Context Document (BCD)
## Common Runtime Environment (CRE) - YAWL Workflow Execution Engine

---

### Document Header

| Field | Value |
|-------|-------|
| **BCD ID** | BCD-CRE-2025-001 |
| **Workflow ID** | CRE-YAWL-EXECUTION |
| **Role** | Generative Analysis - Retrospective Compilation |
| **Version** | 0.3.0 |
| **Date** | 2025-02-07 |
| **Status** | Final - Compiled from Implementation |
| **Scope** | Complete CRE system architecture and implementation traceability |

---

### 1. Business Context

#### 1.1 Problem Statement

The CRE (Common Runtime Environment) exists to enable **workflow-driven swarm execution** using formal Petri Net semantics compiled from YAWL (Yet Another Workflow Language) specifications. The system must support:

1. **Declarative Workflow Specification** - Business analysts define workflows in YAWL XML or YAML format
2. **Automatic Compilation to Runtime** - Specifications compile to Petri Net modules without manual coding
3. **Complete Pattern Coverage** - All 43 YAWL workflow control patterns must be natively supported
4. **Fault-Tolerant Execution** - Swarm coordination with automatic recovery and checkpointing
5. **Runtime Observability** - Full execution trace via XES logging and OpenTelemetry integration

#### 1.2 Design Philosophy

Following **Joe Armstrong's design principle**: One real OTP runner (`gen_yawl`), everything else pure helpers/utilities + message contracts.

The CRE architecture separates concerns into:
- **One runtime component**: `gen_yawl` (wrapper around `gen_pnet`) for workflow execution
- **Pure helper modules**: Stateless transformations for compilation, validation, and expansion
- **Behavior modules**: Individual pattern implementations as `gen_yawl` callbacks

#### 1.3 Domain Requirements

The system targets **swarm coordination workflows** where:
- Multiple agents execute in parallel with synchronization points
- Decision routing determines which agent branches execute
- Cancellation regions allow terminating sub-workflows on events
- Multiple instances of tasks spawn dynamically based on data
- Critical sections enforce mutual exclusion on shared resources
- External events (triggers) can enable or consume workflow tokens

---

### 2. Role Definitions

#### 2.1 gen_yawl - The Universal Runtime

**Module**: `/Users/sac/cre/src/core/gen_yawl.erl`

**Purpose**: Wrapper around `gen_pnet` that supports 3-tuple returns from `fire/3` for automatic user info updates.

**Key Capability**: When a transition fires, it can return `{produce, ProduceMap, NewUsrInfo}` to update workflow state atomically with token production.

**Type Signatures**:
```erlang
-type name() :: atom() | {atom(), atom()} | {global, _} | {via, atom(), _} | pid().
-type fire_result() :: abort | {produce, #{atom() => [term()]}} | {produce, #{atom() => [term()]}, term()}.

-spec start_link(NetMod :: atom(), NetArg :: term(), Options :: [prop()]) -> start_link_result().
-spec start_link(ServerName :: server_name(), NetMod :: atom(), InitArg :: term(), Options :: [prop()]) -> start_link_result().
-spec inject(Name :: name(), ProduceMap :: #{atom() => [term()]}) -> {ok, #{atom() => [term()]}} | {error, term()}.
-spec withdraw(Name :: name(), WithdrawMap :: #{atom() => [term()]}) -> ok | {error, term()}.
-spec cancel_region(Name :: name(), RegionId :: binary() | atom()) -> ok | {error, term()}.
-spec step(Name :: name()) -> abort | {ok, #{atom() => [term()]}}.
-spec drain(Name :: name(), MaxSteps :: non_neg_integer()) -> {ok, [#{atom() => [term()]}]} | {error, limit}.
-spec sync(Name :: name(), Timeout :: non_neg_integer() | infinity) -> {ok, #{atom() => [term()]}} | {error, term()}.
-spec usr_info(Name :: name()) -> term().
-spec marking(Name :: name()) -> #{atom() => [term()]}.
```

**State Management**:
```erlang
-record(wrapper_state, {
          net_mod :: atom(),
          net_state :: term(),
          fire_timeout = 5000 :: pos_integer(),
          progress_timeout = 30000 :: pos_integer(),
          shutting_down = false :: boolean(),
          active_fires = 0 :: non_neg_integer(),
          marking_history = [] :: [non_neg_integer()],
          max_marking_history = 10 :: pos_integer(),
          continue_count = 0 :: non_neg_integer(),
          max_continue = 1000 :: pos_integer(),
          regions = #{} :: #{binary() | atom() => [atom()]}
         }).
```

#### 2.2 yawl_compile - The Specification Compiler

**Module**: `/Users/sac/cre/src/core/yawl_compile.erl`

**Purpose**: Pure functional code generation from YAWL specs to gen_pnet-compatible modules.

**Design Principle**: Stateless - all compilation functions are pure transformations from YAWL specs to Erlang module source code.

**Key Functions**:
```erlang
-spec compile(Spec :: spec(), Options :: map()) -> {ok, compile_result()} | {error, term()}.
-spec compile_to_file(Spec :: spec(), Options :: map(), OutputDir :: file:filename_all()) -> {ok, [file:filename_all()]} | {error, term()}.
-spec generate_module(NetId :: net_id(), NetInfo :: net_info()) -> {ok, binary()} | {error, term()}.
```

**Generated Module Structure**:
Each compiled module implements the gen_pnet callbacks:
```erlang
-module(yawl_NetId).
-behaviour(gen_pnet).

place_lst() -> [input, output, task1, task2, ...].
trsn_lst() -> [t_task1, t_task2, ...].
init_marking(Place, UsrInfo) -> ... % initial tokens
preset(Transition) -> ... % input places
is_enabled(Transition, Mode, UsrInfo) -> ...
fire(Transition, Mode, UsrInfo) -> ...
init(NetArg) -> UsrInfo.
handle_call/3, handle_cast/2, handle_info/2, ...
```

#### 2.3 wf_yawl_executor - High-Level Execution API

**Module**: `/Users/sac/cre/src/wf/wf_yawl_executor.erl`

**Purpose**: Complete end-to-end execution API integrating parsing, compilation, and runtime.

**Lifecycle Operations**:
```erlang
-spec load_workflow(FilePath :: file:filename_all()) -> {ok, executor()} | {error, term()}.
-spec load_workflow_from_yaml(FilePath :: file:filename_all()) -> {ok, executor()} | {error, term()}.
-spec compile_workflow(Spec :: wf_spec:yawl_spec(), Options :: map()) -> {ok, executor()} | {error, term()}.
-spec start_workflow(Executor :: executor(), InitialData :: initial_data()) -> {ok, pid(), case_id()} | {error, term()}.
-spec stop_workflow(Pid :: pid()) -> ok | {error, term()}.
```

**Execution Operations**:
```erlang
-spec execute_step(Pid :: pid()) -> {ok, receipt()} | abort | {error, term()}.
-spec execute_step(Pid :: pid(), MaxSteps :: non_neg_integer()) -> {ok, [receipt()]} | {error, term()}.
-spec inject_token(Pid :: pid(), Place :: atom(), Token :: term()) -> {ok, receipt()} | {error, term()}.
-spec execute_workflow(FilePath, InitialData, Options) -> {ok, exec_result()} | {error, term()}.
```

**State Inspection**:
```erlang
-spec get_workflow_state(Pid :: pid()) -> {ok, workflow_state()} | {error, term()}.
-spec is_quiescent(Pid :: pid()) -> boolean().
```

#### 2.4 yawl_pattern_expander - Pattern Macro Expansion

**Module**: `/Users/sac/cre/src/core/yawl_pattern_expander.erl`

**Purpose**: Expands YAML pattern instances into Petri net structures (places, transitions, flows).

**Key Functions**:
```erlang
-spec expand_pattern(pattern_instance(), map()) -> net_structure().
-spec expand_all_patterns([pattern_instance()], map()) -> net_structure().
-spec expand_patterns_for_net([pattern_instance()], NetId :: binary() | atom(), map()) -> net_structure().
```

**Pattern Mapping**: Uses `yawl_pattern_registry:pattern_module/1` to resolve pattern macros (P1_Sequence, P2_ParallelSplit, etc.) to their gen_yawl module implementations.

---

### 3. Pattern Context

#### 3.1 The 43 YAWL Workflow Control Patterns

The CRE system implements all 43 patterns from the YAWL pattern catalog:

| Pattern ID | Pattern Name | Module | Purpose |
|------------|-------------|---------|---------|
| P1 | Sequence | `sequence` | Sequential execution of tasks |
| P2 | Parallel Split | `parallel_split` | Split into concurrent branches |
| P3 | Synchronization | `synchronization` | Wait for all parallel branches |
| P4 | Exclusive Choice | `exclusive_choice` | Select exactly one branch |
| P5 | Simple Merge | `simple_merge` | Merge without synchronization |
| P6 | Multiple Choice | `multiple_choice` | Select multiple branches |
| P7 | Structured Sync Merge | `structured_sync_merge` | OR-join with structured semantics |
| P8 | Multiple Merge | `multiple_merge` | Merge duplicates allowed |
| P9 | Discriminator | `discriminator` | First branch triggers continuation |
| P10 | Arbitrary Cycles | `arbitrary_cycles` | Unstructured loops |
| P11 | Implicit Termination | `implicit_termination` | End when no work remains |
| P12-P15 | Multiple Instances (4 variants) | `multiple_instances_sync` | Dynamic task spawning |
| P16 | Deferred Choice | `deferred_choice` | Runtime branch selection |
| P17 | Interleaved Routing | `interleaved_routing` | Alternate task execution |
| P18 | Milestone | `milestone` | Process progress checkpoint |
| P19 | Cancel Activity | `cancel_activity` | Terminate single task |
| P20 | Cancel Case | `cancel_case` | Terminate entire workflow |
| P21 | Structured Loop | `structured_loop` | Pre-test loop construct |
| P22 | Recursion | `recursion` | Self-referential subnet call |
| P23 | Transient Trigger | `transient_trigger` | Event enables without consuming |
| P24 | Persistent Trigger | `persistent_trigger` | Event remains after enable |
| P25 | Cancel Region | `cancel_region` | Cancel all tasks in region |
| P26 | Cancel MI Activity | `cancel_mi_activity` | Cancel multi-instance task |
| P27 | Complete MI Activity | `complete_mi_activity` | Partial MI completion |
| P28 | Blocking Discriminator | `blocking_discriminator` | Blocking first-wins |
| P29 | Cancelling Discriminator | `cancelling_discriminator` | First-wins cancels rest |
| P30 | Structured Partial Join | `structured_partial_join` | N-of-M join semantics |
| P31 | Blocking Partial Join | `blocking_partial_join` | N-of-M with blocking |
| P32 | Cancelling Partial Join | `cancelling_partial_join` | N-of-M with cancellation |
| P33 | Generalized AND Join | `generalized_and_join` | M-of-N exact join |
| P34-P36 | Partial Join MI (3 variants) | `static_partial_join_mi`, `cancelling_partial_join_mi`, `dynamic_partial_join_mi` | MI with partial joins |
| P37 | Local Sync Merge | `local_sync_merge` | Local OR-join |
| P38 | General Sync Merge | `general_sync_merge` | Multi-source OR-join |
| P39 | Critical Section | `critical_section` | Mutual exclusion |
| P40 | Interleaved Routing | `interleaved_routing` | Round-robin routing |
| P41 | Thread Merge | `thread_merge` | Merge parallel threads |
| P42 | Thread Split | `thread_split` | Split into parallel threads |
| P43 | Explicit Termination | `explicit_termination` | Explicit end marker |

#### 3.2 Pattern Registry

**Module**: `/Users/sac/cre/src/core/yawl_pattern_registry.erl`

Maps pattern macros to module names:

```erlang
pattern_module(<<"P1_Sequence">>) -> sequence;
pattern_module(<<"P2_ParallelSplit">>) -> parallel_split;
pattern_module(<<"P3_Synchronization">>) -> synchronization;
...
pattern_module(<<"P43_ExplicitTermination">>) -> explicit_termination.
```

#### 3.3 Pattern Example: Sequence (P1)

**Module**: `/Users/sac/cre/src/patterns/sequence.erl`

```erlang
place_lst() -> [p_start, p_task1, p_task2, p_end].
trsn_lst() -> [t_start, t_complete1, t_complete2, t_finish].

preset(t_start) -> [p_start];
preset(t_complete1) -> [p_task1];
preset(t_complete2) -> [p_task2];
preset(t_finish) -> [p_task2].

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_task1 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_task2 => [token]}, UsrInfo};
...
```

---

### 4. Workflow Variables

#### 4.1 Core System State

The CRE system maintains workflow state through the `usr_info` field in the `net_state` record:

```erlang
-record(net_state, {
          net_mod :: atom(),
          usr_info :: term(),          % Workflow variables
          marking :: #{atom() => [term()]},
          stats :: #stats{} | undefined,
          tstart :: integer(),
          cnt :: non_neg_integer()
         }).
```

#### 4.2 Variable Initialization

Variables are initialized from the YAML specification:

```yaml
variables:
  - name: approval_status
    type: boolean
    initial: false
  - name: threshold
    type: integer
    initial: 5
```

The compiler generates initialization code:
```erlang
init(NetArg) ->
    DefaultVars = #{
        approval_status => false,
        threshold => 5
    },
    case NetArg of
        #{initial_data := Id} when is_map(Id) -> maps:merge(DefaultVars, Id);
        _ -> DefaultVars
    end.
```

#### 4.3 Variable Updates

During transition firing, the 3-tuple return format updates variables:

```erlang
fire(t_approve, #{p_request := [Request]}, UsrInfo) ->
    NewUsrInfo = UsrInfo#{approval_status => true, approver => Request#request.from},
    {produce, #{p_approved => [done]}, NewUsrInfo}.
```

---

### 5. Decision Schema

#### 5.1 Type Signatures for Key Operations

**Specification Loading**:
```erlang
-spec wf_spec:from_xml_file(FilePath :: file:filename_all()) ->
    {ok, wf_spec:yawl_spec()} | {error, term()}.
-spec wf_yaml_spec:from_yaml_file(FilePath :: file:filename_all()) ->
    {ok, wf_yaml_spec:yawl_yaml_spec()} | {error, term()}.
```

**Compilation**:
```erlang
-type compile_result() :: #{
    spec_id := binary(),
    modules := #{binary() => module()},
    places := #{binary() => [place()]},
    transitions := #{binary() => [transition()]},
    net_info := #{binary() => map()}
}.

-spec yawl_compile:compile(Spec :: wf_spec:yawl_spec() | wf_yaml_spec:yawl_yaml_spec(),
                           Options :: map()) ->
    {ok, compile_result()} | {error, term()}.
```

**Execution**:
```erlang
-type receipt() :: #{
    trsn := atom(),
    mode => #{atom() => [term()]},
    produce => #{atom() => [term()]}
}.

-type workflow_state() :: #{
    marking := #{atom() => [term()]},
    status := running | completed | cancelled | error,
    case_id := case_id(),
    spec_id := binary()
}.

-spec wf_yawl_executor:execute_workflow(FilePath :: file:filename_all(),
                                        InitialData :: initial_data(),
                                        Options :: exec_options()) ->
    {ok, exec_result()} | {error, term()}.
```

**State Queries**:
```erlang
-spec gen_yawl:marking(Name :: name()) -> #{atom() => [term()]}.
-spec gen_yawl:usr_info(Name :: name()) -> term().
-spec gen_yawl:ls(Name :: name(), Place :: atom()) -> {ok, [term()]} | {error, #bad_place{}}.
-spec gen_yawl:stats(Name :: name()) -> #stats{}.
```

**Token Injection**:
```erlang
-spec gen_yawl:inject(Name :: name(), ProduceMap :: #{atom() => [term()]}) ->
    {ok, #{atom() => [term()]}} | {error, term()}.
-spec gen_yawl:withdraw(Name :: name(), WithdrawMap :: #{atom() => [term()]}) ->
    ok | {error, term()}.
-spec gen_yawl:cancel_region(Name :: name(), RegionId :: binary() | atom()) ->
    ok | {error, term()}.
```

#### 5.2 Pattern Expansion Types

```erlang
-type pattern_instance() :: #{
    id => binary(),
    pattern => binary(),
    net => binary(),
    split_task => binary() | undefined,
    branches => [binary()] | undefined,
    waits_for => [binary()] | undefined,
    from => binary() | undefined,
    to => binary() | undefined,
    choices => [binary()] | undefined,
    threshold => non_neg_integer() | undefined,
    ...
}.

-type net_structure() :: #{
    places => [atom()],
    transitions => [atom()],
    flows => [{atom(), atom()}],
    preset => #{atom() => [atom()]},
    postset => #{atom() => [atom()]}
}.
```

---

### 6. Traceability

#### 6.1 Requirements to Module Mapping

| Requirement | Module | Key Functions |
|-------------|--------|---------------|
| Workflow specification parsing | `wf_spec`, `wf_yaml_spec` | `from_xml_file/1`, `from_yaml_file/1` |
| YAML 0.2 format support | `wf_yaml_spec` | `from_yaml/1`, parse specification set |
| Pattern instance expansion | `yawl_pattern_expander` | `expand_pattern/2`, `expand_patterns_for_net/3` |
| Pattern registry resolution | `yawl_pattern_registry` | `pattern_module/1`, `all_patterns/0` |
| Specification compilation | `yawl_compile` | `compile/2`, `generate_module/2` |
| Runtime execution | `gen_yawl` | `start_link/3`, `inject/2`, `step/1`, `drain/2` |
| Unified execution API | `wf_yawl_executor` | `load_workflow/1`, `start_workflow/2`, `execute_workflow/3` |
| Petri net behavior | `gen_pnet` | Core behavior definition |
| Individual patterns (P1-P43) | `/Users/sac/cre/src/patterns/*.erl` | 43 pattern implementations |

#### 6.2 File Structure Traceability

```
/Users/sac/cre/src/
├── core/                          # Core runtime and compilation
│   ├── gen_pnet.erl               # Petri net behavior (foundation)
│   ├── gen_yawl.erl               # YAWL runtime wrapper (3-tuple fire/3)
│   ├── yawl_compile.erl           # YAWL to Petri net compiler
│   ├── yawl_pattern_expander.erl  # Pattern macro expansion
│   └── yawl_pattern_registry.erl   # Pattern name to module mapping
│
├── patterns/                      # 43 pattern implementations
│   ├── sequence.erl               # P1: Sequence
│   ├── parallel_split.erl         # P2: Parallel Split
│   ├── synchronization.erl        # P3: Synchronization
│   ├── exclusive_choice.erl       # P4: Exclusive Choice
│   ├── simple_merge.erl           # P5: Simple Merge
│   ├── multiple_choice.erl        # P6: Multiple Choice
│   ├── structured_sync_merge.erl  # P7: Structured Sync Merge
│   ├── multiple_merge.erl         # P8: Multiple Merge
│   ├── discriminator.erl          # P9: Discriminator
│   ├── arbitrary_cycles.erl       # P10: Arbitrary Cycles
│   ├── implicit_termination.erl   # P11: Implicit Termination
│   ├── multiple_instances_sync.erl # P12-P15: Multiple Instances
│   ├── deferred_choice.erl        # P16: Deferred Choice
│   ├── interleaved_routing.erl    # P17/P40: Interleaved Routing
│   ├── milestone.erl              # P18: Milestone
│   ├── cancel_activity.erl        # P19: Cancel Activity
│   ├── cancel_case.erl            # P20: Cancel Case
│   ├── structured_loop.erl        # P21: Structured Loop
│   ├── recursion.erl              # P22: Recursion
│   ├── transient_trigger.erl      # P23: Transient Trigger
│   ├── persistent_trigger.erl     # P24: Persistent Trigger
│   ├── cancel_region.erl          # P25: Cancel Region
│   ├── cancel_mi_activity.erl     # P26: Cancel MI Activity
│   ├── complete_mi_activity.erl   # P27: Complete MI Activity
│   ├── blocking_discriminator.erl # P28: Blocking Discriminator
│   ├── cancelling_discriminator.erl # P29: Cancelling Discriminator
│   ├── structured_partial_join.erl # P30: Structured Partial Join
│   ├── blocking_partial_join.erl # P31: Blocking Partial Join
│   ├── cancelling_partial_join.erl # P32: Cancelling Partial Join
│   ├── generalized_and_join.erl   # P33: Generalized AND Join
│   ├── static_partial_join_mi.erl # P34: Static Partial Join MI
│   ├── cancelling_partial_join_mi.erl # P35: Cancelling Partial Join MI
│   ├── dynamic_partial_join_mi.erl # P36: Dynamic Partial Join MI
│   ├── local_sync_merge.erl       # P37: Local Sync Merge
│   ├── general_sync_merge.erl     # P38/P7: General Sync Merge
│   ├── critical_section.erl       # P39: Critical Section
│   ├── thread_split.erl           # P42: Thread Split
│   └── thread_merge.erl           # P41: Thread Merge
│
├── wf/                            # Workflow utilities and execution
│   ├── wf_yawl_executor.erl       # High-level execution API
│   ├── wf_spec.erl                # XML YAWL specification parser
│   ├── wf_yaml_spec.erl           # YAML YAWL specification parser
│   ├── yawl_recovery.erl          # Recovery and checkpointing
│   └── ...
│
├── yawl/                          # YAWL-specific modules
│   ├── yawl_validate.erl          # Specification validation
│   ├── yawl_persistence.erl       # State persistence
│   └── ...
│
└── api/                           # Client APIs
    └── cre_yawl_client.erl         # Client API for workflow execution
```

#### 6.3 Design Decision Traceability

**Joe Armstrong's "One Runner" Principle**:
- Single OTP runtime: `gen_yawl` in `/Users/sac/cre/src/core/gen_yawl.erl`
- All other modules are pure helpers (stateless transformations)
- Message contracts define interaction patterns

**Pattern-Based Compilation**:
- YAML pattern instances → `yawl_pattern_expander:expand_pattern/2`
- Pattern registry lookup → `yawl_pattern_registry:pattern_module/1`
- Generated modules implement `gen_pnet` behavior callbacks

**3-Tuple fire/3 Extension**:
- Standard `gen_pnet`: `fire/3` returns `{produce, ProduceMap}` or `abort`
- CRE `gen_yawl`: adds `{produce, ProduceMap, NewUsrInfo}` for atomic state updates
- Enables workflow variable updates synchronized with token production

**Cancellation Support**:
- Region definitions in YAML → `cancel_region/2` API
- P25_CancelRegion pattern → Automatic token withdrawal
- Cancellation sets propagate through nested workflows

---

### 7. Implementation Verification

This BCD has been compiled from the actual CRE codebase implementation:

1. **gen_yawl.erl** (lines 1-1462): Runtime wrapper with 3-tuple fire/3 support
2. **yawl_compile.erl** (lines 1-1270): Pure functional compiler from YAWL to gen_pnet
3. **yawl_pattern_expander.erl** (lines 1-735): Pattern macro expansion with place/transition mapping
4. **yawl_pattern_registry.erl** (lines 1-199): Complete 43-pattern registry
5. **wf_yawl_executor.erl** (lines 1-1321): End-to-end execution API
6. **wf_yaml_spec.erl** (lines 1-894): YAML 0.2 specification parser
7. **Pattern modules** (e.g., sequence.erl, parallel_split.erl, exclusive_choice.erl): Concrete implementations

The CRE system demonstrates a **complete traceable path** from declarative YAML workflow specifications through pattern-based compilation to fault-tolerant Petri net execution, fully implementing the YAWL workflow control pattern catalog with Erlang/OTP reliability guarantees.

---

*End of BCD-CRE-2025-001*
