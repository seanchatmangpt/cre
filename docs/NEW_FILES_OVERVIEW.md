# New Files Overview - CRE Architecture Refactoring

This document provides a comprehensive overview of the new CRE source directory organization following Joe Armstrong's design principle: **one real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts**.

## Architecture Summary

The refactoring implements Joe Armstrong's design principle with a clean separation of concerns:
- **Core**: The single OTP behavior wrapper (`gen_yawl`)
- **Pure Utilities**: Stateless helper modules organized by domain
- **Legacy**: Old OTP processes remain in `src/` for backward compatibility

### New Directory Structure

```
src/
├── core/                   # Core OTP behaviors
│   └── gen_yawl.erl        # YAWL behavior wrapper (the OTP runner)
│
├── pnet/                   # Pure Petri net utilities (stateless)
│   ├── pnet_types.erl      # Type validators
│   ├── pnet_marking.erl    # Multiset marking algebra
│   ├── pnet_mode.erl       # Mode enumeration
│   ├── pnet_choice.erl     # Deterministic choice
│   └── pnet_receipt.erl    # Receipt tracking
│
├── wf/                     # Pure workflow utilities (stateless)
│   ├── wf_timerq.erl       # Deadline queue
│   ├── wf_task.erl         # Task token constructors
│   └── wf_scope.erl        # Boundary mapping
│
├── yawl/                   # YAWL-specific pure utilities (stateless)
│   ├── yawl_schema.erl     # Schema validation
│   ├── yawl_elements.erl   # Element definitions
│   ├── yawl_marshal.erl    # Data marshaling
│   ├── yawl_util.erl       # General utilities
│   └── yawl_patterns.erl   # Pattern utilities
│
├── patterns/               # YAWL workflow patterns
│   ├── parallel_split.erl  # Parallel split pattern
│   ├── exclusive_choice.erl
│   ├── simple_merge.erl
│   ├── multiple_merge.erl
│   ├── deferred_choice.erl
│   ├── interleaved_routing.erl
│   ├── discriminator.erl
│   ├── n_out_of_m.erl
│   ├── milestone.erl
│   └── implicit_merge.erl
│
├── api/                    # Client API modules
│   ├── cre_client.erl      # Main client API
│   └── cre_yawl_client.erl # YAWL client API
│
├── integration/            # External system integrations (stateless)
│   ├── telemetry.erl       # Telemetry utilities
│   ├── gen_smtp_client.erl # SMTP client
│   └── yawl_claude_bridge.erl # Claude AI bridge
│
├── http/                   # HTTP handlers
│   ├── cre_status_handler.erl    # Status endpoint
│   ├── cre_history_handler.erl   # History endpoint
│   └── yawl_interface_d.erl      # Interface handler
│
├── app/                    # Application entry points
│   ├── cre.erl             # Application module
│   └── cre_sup.erl         # Application supervisor
│
└── [legacy files]          # Old OTP processes stay in src/ root
    ├── cre_master.erl
    ├── cre_worker.erl
    ├── yawl_engine.erl
    └── ... (20+ more)
```

## File Categories

### 1. Core (1 file)
- **Location**: `/src/core/`
- **Purpose**: OTP behavior wrapper for YAWL workflows
- **Key Features**: Single stateful OTP process

### 2. Petri Net Utilities (5 files)
- **Location**: `/src/pnet/`
- **Purpose**: Pure-functional building blocks for Petri net execution
- **Key Features**: Stateless, total functions, immutable operations

### 3. Workflow Utilities (3 files)
- **Location**: `/src/wf/`
- **Purpose**: Pure workflow utilities
- **Key Features**: Stateless helper functions

### 4. YAWL Pure Utilities (5 files)
- **Location**: `/src/yawl/`
- **Purpose**: YAWL-specific pure utilities
- **Key Features**: Stateless helper functions for YAWL

### 5. Pattern Modules (10 files)
- **Location**: `/src/patterns/` (renamed from `yawl_patterns/`)
- **Purpose**: Workflow pattern implementations
- **Key Features**: Standard YAWL patterns with consistent interfaces

### 6. API (2 files)
- **Location**: `/src/api/`
- **Purpose**: Client API modules
- **Key Features**: Main client interfaces

### 7. Integration (3 files)
- **Location**: `/src/integration/`
- **Purpose**: External system integrations
- **Key Features**: Telemetry, SMTP, AI bridge

### 8. HTTP (3 files)
- **Location**: `/src/http/`
- **Purpose**: HTTP endpoint handlers
- **Key Features**: REST API endpoints

### 9. App (2 files)
- **Location**: `/src/app/`
- **Purpose**: Application entry points
- **Key Features**: Application module and supervisor

### 10. Legacy (25+ files)
- **Location**: `/src/` (root)
- **Purpose**: Old OTP processes for backward compatibility
- **Key Features**: Remain in place, not moved

---

## Detailed File Documentation

### Core Utility Modules

#### 1. `src/pnet/pnet_types.erl` - Type System and Validation

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

#### 2. `src/pnet/pnet_marking.erl` - Multiset Marking Algebra

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

#### 3. `src/pnet/pnet_choice.erl` - Deterministic Nondeterminism

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

#### 4. `src/pnet/pnet_mode.erl` - Mode Enumeration

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

#### 5. `src/pnet/pnet_receipt.erl` - Receipt Tracking

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

#### 6. `src/wf/wf_timerq.erl` - Deadline Queue

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

#### 7. `src/wf/wf_task.erl` - Task Lifecycle Constructors

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

#### 8. `src/wf/wf_scope.erl` - Scope Boundary Mapping

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

#### 1. `src/patterns/parallel_split.erl` - WCP-2
**Purpose**: Implements Parallel Split pattern (one input, multiple outputs).
**Use Case**: Forking work to multiple parallel branches.

#### 2. `src/patterns/exclusive_choice.erl` - WCP-4
**Purpose**: Implements Exclusive Choice pattern (conditional branching).
**Use Case**: Routing based on conditions or data.

#### 3. `src/patterns/simple_merge.erl` - WCP-5
**Purpose**: Implements Simple Merge pattern (multiple inputs, one output).
**Use Case**: Converging parallel branches.

#### 4. `src/patterns/n_out_of_m.erl`
**Purpose**: Implements N-out-of-M pattern (partial synchronization).
**Use Case**: Majority voting or threshold-based convergence.

#### 5. `src/patterns/interleaved_routing.erl`
**Purpose**: Implements Interleaved Routing pattern (complex branching).
**Use Case**: Complex workflow routing with multiple paths.

#### 6. `src/patterns/implicit_merge.erl`
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