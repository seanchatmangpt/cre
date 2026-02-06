# New Files Overview - CRE Architecture Refactoring

This document provides a comprehensive overview of all new files created in the architectural refactoring commit.

## Architecture Summary

The refactoring implements Joe Armstrong's design principle: **one real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts**.

### Core Architecture Components

```
gen_pnet (single OTP process)
├── pnet_types (pure utility) - Type definitions and validation
├── pnet_marking (pure utility) - Multiset marking algebra
├── pnet_mode (pure utility) - Mode enumeration for transitions
├── pnet_choice (pure utility) - Deterministic nondeterminism
├── pnet_receipt (pure utility) - Receipt tracking and audit trails
├── wf_timerq (pure utility) - Deadline queue for token injection
├── wf_task (pure utility) - External task token constructors
└── wf_scope (pure utility) - Scope boundary mapping for hierarchical workflows
```

## File Categories

### 1. Core Utility Modules (8 files)
- **Location**: `/src/`
- **Purpose**: Pure-functional building blocks for Petri net execution
- **Key Features**: Stateless, total functions, immutable operations

### 2. YAWL Pattern Modules (10 files)
- **Location**: `/src/yawl_patterns/`
- **Purpose**: Workflow pattern implementations
- **Key Features**: Standard YAWL patterns with consistent interfaces

### 3. Documentation (11 files)
- **Location**: `/docs/`
- **Purpose**: Comprehensive guides and references
- **Key Features**: Diataxis architecture, tutorials, migration guides

### 4. Examples (2 files)
- **Location**: `/examples/`
- **Purpose**: Demonstrative implementations
- **Key Features**: Working examples with execution scripts

### 5. Specifications (2 files)
- **Location**: `/docs/yawl_patterns/`
- **Purpose**: Formal specifications and integration guides
- **Key Features**: Technical specifications for advanced users

---

## Detailed File Documentation

### Core Utility Modules

#### 1. `src/pnet_types.erl` - Type System and Validation

**Purpose**: Defines all type definitions and validation functions for the Petri net framework.

**Key Types**:
```erlang
% Basic Petri net types
-type place() :: atom().
-type trsn() :: atom().
-type token() :: term().
-type marking() :: #{place() => [token()]}.

% Colored Petri net extension
-type var() :: atom().
-type binding() :: #{var() => term()}.
-type cmode() :: {binding(), mode()}.

% Execution types
-type move() :: #{
    trsn := trsn(),
    mode := mode() | cmode(),
    produce := produce_map()
}.
-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := move(),
    ts := integer()
}.
```

**Key Functions**:
- `is_marking/1` - Total function validation
- `is_mode/1` - Mode structure validation
- `is_binding/1` - Variable binding validation
- `is_cmode/1` - Colored mode validation

**Usage**: All validation functions are total - they return boolean() and never crash.

#### 2. `src/pnet_marking.erl` - Multiset Marking Algebra

**Purpose**: Implements multiset operations for token state management with total functions.

**Key Operations**:
```erlang
% Marking creation and inspection
new(Places) -> marking()                    % Initialize with empty places
get(Marking, Place) -> {ok, Tokens} | error  % Get tokens at place
set(Marking, Place, Tokens) -> marking()    % Set tokens at place

% Multiset operations
add(Marking, ProduceMap) -> marking()      % Add tokens
take(Marking, ConsumeMap) -> marking()     % Remove tokens
apply(Marking, Move) -> marking()          % Apply transition

% State management
hash(Marking) -> binary()                   % Consistent hash
snapshot(Marking) -> marking()             % Immutable copy
```

**Key Features**:
- Multiset semantics (token multiplicity matters)
- Total functions (always return {ok, ...} or {error, ...})
- Immutable operations
- Consistent hashing for state comparison

#### 3. `src/pnet_choice.erl` - Deterministic Nondeterminism

**Purpose**: Provides reproducible random selection for workflow execution.

**Key Operations**:
```erlang
% Random number generation
seed(SeedTerm) -> rng_state()             % Create deterministic RNG
pick(List, RngState) -> {Element, NewRngState}
pick_weighted(Items, RngState) -> {Element, NewRngState}

% Weighted selection
% Items = [{Element, pos_integer()}]
% Uses cumulative weights for probability distribution
```

**Key Features**:
- Pure-functional RNG (no process state)
- Deterministic behavior when seeded
- Support for weighted random choice
- Returns updated state with each operation

#### 4. `src/pnet_mode.erl` - Mode Enumeration

**Purpose**: Enumerates all possible firing modes for transitions.

**Key Operations**:
```erlang
% Basic mode enumeration
preset_counts(PresetPlaces) -> #{place() => non_neg_integer()}
enum_modes(PresetPlaces, Marking) -> [mode()]

% Colored net extension
enum_cmodes(Trsn, Marking, Ctx, NetMod) -> [cmode()]
```

**Key Features**:
- Cartesian product of available tokens
- Colored Petri net support with variable bindings
- Falls back to basic enumeration for uncolored nets
- Handles empty token lists gracefully

#### 5. `src/pnet_receipt.erl` - Receipt Tracking

**Purpose**: Creates audit trails for all state transitions.

**Key Operations**:
```erlang
% Receipt creation
make(BeforeHash, AfterHash, Move) -> receipt()
timestamp() -> integer()                    % Current Unix timestamp

% Receipt analysis
effects(Receipt) -> {Type, Details}        % Extract transition effects
```

**Key Features**:
- Immutable audit trail records
- Timestamped state transitions
- Before/after state hashing
- Transition effect extraction

#### 6. `src/wf_timerq.erl` - Deadline Queue

**Purpose**: Manages time-based token injection for deadlines and timeouts.

**Key Operations**:
```erlang
% Queue management
new() -> timerq()                          % Empty timer queue
arm(TimerQ, Key, Deadline, Event) -> timerq()
disarm(TimerQ, Key) -> timerq()
poll(TimerQ, Now) -> {[Event], NewTimerQ}

% Queue inspection
is_empty(TimerQ) -> boolean()
size(TimerQ) -> non_neg_integer()
peek(TimerQ) -> {Deadline, Event} | undefined
```

**Key Features**:
- Pure-functional priority queue
- Deadline-based token injection
- Key-based disarming
- Efficient polling for ready events

#### 7. `src/wf_task.erl` - Task Lifecycle Constructors

**Purpose**: Creates produce maps for external task state transitions.

**Key Operations**:
```erlang
% Task state transitions
enabled(TaskId, Payload, Place) -> {produce, ProduceMap}
running(TaskId, Payload, Place) -> {produce, ProduceMap}
done(TaskId, Payload, Place) -> {produce, ProduceMap}
failed(TaskId, Payload, Place) -> {produce, ProduceMap}
cancelled(TaskId, Payload, Place) -> {produce, ProduceMap}
```

**Key Features**:
- Standardized task state tokens
- Consistent produce map format
- Integration with external work systems
- Audit trail support via receipts

#### 8. `src/wf_scope.erl` - Scope Boundary Mapping

**Purpose**: Handles place namespace translation between parent and child workflows.

**Key Operations**:
```erlang
% Scope boundary operations
enter(BindingTable, ScopeId, ParentTokens) -> ChildProduceMap
leave(BindingTable, ScopeId, ChildTokens) -> ParentProduceMap
bindings(BindingTable, ScopeId) -> Mapping | {error, unknown_scope}
```

**Key Features**:
- Parent-to-child namespace translation
- Child-to-parent reverse translation
- Binding table management
- Hierarchical workflow support

### YAWL Pattern Modules

All pattern modules implement the `pnet_net` behavior:

#### Common Interface
```erlang
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/2, fire/3]).
```

#### 1. `src/yawl_patterns/parallel_split.erl` - WCP-2
**Purpose**: Implements Parallel Split pattern (one input, multiple outputs).
**Use Case**: Forking work to multiple parallel branches.

#### 2. `src/yawl_patterns/exclusive_choice.erl` - WCP-4
**Purpose**: Implements Exclusive Choice pattern (conditional branching).
**Use Case**: Routing based on conditions or data.

#### 3. `src/yawl_patterns/simple_merge.erl` - WCP-5
**Purpose**: Implements Simple Merge pattern (multiple inputs, one output).
**Use Case**: Converging parallel branches.

#### 4. `src/yawl_patterns/n_out_of_m.erl`
**Purpose**: Implements N-out-of-M pattern (partial synchronization).
**Use Case**: Majority voting or threshold-based convergence.

#### 5. `src/yawl_patterns/interleaved_routing.erl`
**Purpose**: Implements Interleaved Routing pattern (complex branching).
**Use Case**: Complex workflow routing with multiple paths.

#### 6. `src/yawl_patterns/implicit_merge.erl`
**Purpose**: Implements Implicit Merge pattern (synchronization).
**Use Case**: Waiting for multiple conditions to be met.

#### Additional Patterns (deferred_choice, discriminator, milestone, multiple_merge)
Each follows the same interface with pattern-specific logic.

### Documentation Files

#### 1. `docs/DIATAXIS_ARCHITECTURE.md`
**Purpose**: Comprehensive architecture overview following diataxis principles.
**Content**:
- Architecture rationale and design principles
- Tutorial with step-by-step examples
- Reference documentation for all components
- Implementation status and integration points

#### 2. `docs/COMPLETE_API_REFERENCE.md`
**Purpose**: Complete API specification for all modules.
**Content**:
- All function signatures and type specifications
- Usage examples for each module
- Error handling documentation
- Integration examples

#### 3. `docs/HELPER_INTEGRATION_GUIDE.md`
**Purpose**: Cross-cutting usage matrix and integration patterns.
**Content**:
- Module dependencies and usage matrix
- Integration points across the system
- Data flow diagrams
- Performance considerations

#### 4. `docs/MIGRATION_GUIDE.md`
**Purpose**: Migration path from old architecture to new.
**Content**:
- Architecture comparison
- Step-by-step migration strategy
- Code examples for old vs new
- Common migration pitfalls

#### 5. `docs/tutorials/` Directory
- **`getting_started.md`**: Basic workflow creation
- **`basic_patterns_tutorial.md`**: Core YAWL patterns
- **`advanced_patterns_tutorial.md`**: Complex pattern composition
- **`colored_tokens_tutorial.md`**: Colored Petri net features
- **`workflow_migration_tutorial.md`**: Migration examples

#### 6. `docs/yawl_patterns/` Directory
- **`GEN_PNET_API_SPECIFICATION.md`**: Formal API specification
- **`GEN_PNET_INTEGRATION_ARCHITECTURE.md`**: Integration architecture

### Example Files

#### 1. `examples/yawl_pnet_demo.erl`
**Purpose**: Demonstrative workflow implementation.
**Features**:
- Complete workflow definition
- Integration with helper modules
- Task and timer usage
- Receipt tracking

#### 2. `examples/yawl_pnet_demo.sh`
**Purpose**: Execution script for demo workflow.
**Features**:
- Command-line interface
- Error handling
- Configuration examples

### Integration Points

The new modules integrate with existing components:

1. **yawl_engine.erl** - Uses `pnet_marking` for state management
2. **yawl_control.erl** - Uses `pnet_choice` for nondeterminism and `wf_task` for external work
3. **yawl_patterns.erl** - Uses all helper modules for pattern composition
4. **yawl_approval.erl** - Uses `wf_task` and `pnet_receipt` for approval workflows
5. **cre_master.erl** - Coordinates helper modules for worker management

### Best Practices

1. **Pure Functions**: All helper modules are stateless and pure
2. **Total Functions**: All operations handle all inputs gracefully
3. **Type Safety**: Comprehensive type definitions and validation
4. **Immutability**: All operations return new states rather than modifying existing ones
5. **Audit Trails**: Complete receipt tracking for all state changes
6. **Determinism**: Reproducible execution through deterministic choice

### Testing Strategy

All new modules include:
- Comprehensive unit tests
- Integration tests with the framework
- Property-based testing where applicable
- Edge case handling
- Performance benchmarking

---

This overview provides a complete picture of the new architecture and all supporting files created in the refactoring commit.