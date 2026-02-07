# YAWL Workflow Patterns: Quick Reference Card

---

## 80/20 Rule: The Most Important 20%

### 3 Patterns That Cover 80% of Workflow Usage

| Pattern | Category | When to Use | Example |
|---------|----------|-------------|---------|
| **Sequence Pattern (WCP-01)** | Basic Control Flow | Linear business processes | Document approval workflow |
| **Parallel Split (WCP-02)** | Basic Control Flow | Concurrent task execution | Order processing with parallel checks |
| **Synchronization (WCP-03)** | Basic Control Flow | Wait for all parallel branches | Loan approval with multiple checks |

---

## Quick Decision Tree

```
What workflow structure do you need?
├── Linear sequence ────────────────→ WCP-01: Sequence
├── Parallel execution ────────────→ WCP-02: Parallel Split
├── Wait for all branches ────────→ WCP-03: Synchronization
├── Exclusive choice ──────────────→ WCP-04: Exclusive Choice
├── Multiple merge ───────────────→ WCP-05: Multiple Merge
├── Discriminator ───────────────→ WCP-06: Discriminator
├── Multi-instance ───────────────→ WCP-11: Multiple Instances
└── Advanced patterns ────────────→ WCP-07 to WCP-43
```

---

## Core Pattern Categories

### Basic Control Flow (WCP-01-06)
- **WCP-01**: Sequence - Linear task execution
- **WCP-02**: Parallel Split - Concurrent branches
- **WCP-03**: Synchronization - Wait for all branches
- **WCP-04**: Exclusive Choice - One of many paths
- **WCP-05**: Multiple Merge - Combine multiple paths
- **WCP-06**: Discriminator - Trigger on any completion

### Advanced Patterns (WCP-07-28)
- **WCP-07**: N-ary Split - Parallel to N branches
- **WCP-08**: N-ary Merge - Wait for N branches
- **WCP-09**: Synchronizing Merge - Wait with conditions
- **WCP-10**: Multi-choice - Complex exclusive choices
- **WCP-21**: deferred choice - Time-based selection
- **WCP-22**: Interleaved Parallel - Mixed sequence/parallel

### Data Patterns (WDP-01-05)
- **WDP-01**: Data-based XOR - Data-driven routing
- **WDP-02**: Data-based AND - Data distribution
- **WDP-03**: Multiple instances - Data iteration
- **WDP-04**: Prioritized selection - Data priority
- **WDP-05**: Data merge - Data combination

---

## Key Modules at a Glance

### Core Engine Modules
- **wf_engine.erl** - Main workflow execution engine
- **pnet_types.erl** - Type definitions and validation
- **pnet_marking.erl** - Multiset marking algebra
- **pnet_mode.erl** - Mode enumeration utilities
- **pnet_choice.erl** - Deterministic choice logic
- **pnet_receipt.erl** - Receipt tracking and effects

### Helper Modules
- **wf_timerq.erl** - Deadline queue for tokens
- **wf_task.erl** - External task constructors
- **wf_scope.erl** - Boundary mapping helpers
- **yawl_validate.erl** - YAWL specification validation
- **yawl_compile.erl** - YAWL compilation to net modules

---

## Common Use Cases

### Business Process Automation
- **Document Approval**: Sequence + Exclusive Choice
- **Order Processing**: Parallel Split + Synchronization
- **Loan Approval**: Multiple Instances + Data Patterns

### Human-in-the-Loop Workflows
- **Task Assignment**: wf_task for external tasks
- **Approval Flows**: wf_engine:allocate/4, complete/5
- **Work Item Management**: worklist/2, case_state/2

### System Integration
- **Service Orchestration**: wf_engine:service_reply/5
- **Event Handling**: wf_engine:publish/4
- **Timeout Management**: wf_timerq integration

---

## API Quick Reference

### Workflow Engine Core Functions
```erlang
% Start workflow
wf_engine:start_link(#{spec => Compiled, org => Org, seed => 1, now => 0})
wf_engine:start_case(Engine, #{data => #{amount => 10}}, 0)

% Work item management
wf_engine:allocate(Engine, WiId, User, Now)
wf_engine:start_work(Engine, WiId, User, Now)
wf_engine:complete(Engine, WiId, User, #{approved => true}, 0)

% State checking
wf_engine:case_state(Engine, CaseId)
wf_engine:worklist(Engine, alice)
wf_engine:enabled(Engine, CaseId)
```

### Petri Net Operations
```erlang
% Marking operations
pnet_marking:new(Places)
pnet_marking:hash(Marking)
pnet_types:produce_map()

% Choice and transitions
pnet_choice:seed(Seed)
pnet_choice:pick(Transitions, RngState)
pnet_receipt:make(BeforeHash, AfterHash, Move)
```

---

## Common Patterns Implementation

### Simple Sequence Pattern
```erlang
-module(sequence_pattern).
-behaviour(pnet_net).

places() -> [start, step1, step2, end].
transitions() -> [t1, t2].

preset(t1) -> [start];
preset(t2) -> [step1].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{start := [init]}, _UsrInfo) -> [#{start => []}].
modes(t2, #{step1 := [done]}, _UsrInfo) -> [#{step1 => []}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{step1 => [done]}}.

fire(t2, #{step1 => []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.
```

### Parallel Split Pattern
```erlang
-module(parallel_split).
-behaviour(pnet_net).

places() -> [start, branch1, branch2, branch3, end].
transitions() -> [split, t1, t2, t3, merge].

preset(split) -> [start];
preset(t1) -> [start];
preset(t2) -> [start];
preset(t3) -> [start];
preset(merge) -> [branch1, branch2, branch3].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(split, #{start := [init]}, _UsrInfo) ->
    [#{start => [], branch1 => [init], branch2 => [init], branch3 => [init]}].

modes(t1, #{branch1 := [init]}, _UsrInfo) ->
    [#{branch1 => []}].
modes(t2, #{branch2 := [init]}, _UsrInfo) ->
    [#{branch2 => []}].
modes(t3, #{branch3 := [init]}, _UsrInfo) ->
    [#{branch3 => []}].

fire(split, #{start => []}, _UsrInfo) ->
    {produce, #{branch1 => [init], branch2 => [init], branch3 => [init]}}.

fire(t1, #{branch1 => []}, _UsrInfo) ->
    {produce, #{}}.
fire(t2, #{branch2 => []}, _UsrInfo) ->
    {produce, #{}}.
fire(t3, #{branch3 => []}, _UsrInfo) ->
    {produce, #{}}.

fire(merge, #{branch1 := [_], branch2 := [_], branch3 := [_]}, _UsrInfo) ->
    {produce, #{end => [complete]}}.
```

---

## Configuration Parameters

### Workflow Engine Configuration
```erlang
#{ spec => CompiledSpec,      % YAWL specification
  org => OrgConfig,          % Organization configuration
  seed => 1,                 % Random seed for deterministic choice
  now => 0,                  % Current timestamp
  worklet => WorkletConfig   % Dynamic task replacement rules
}
```

### Common YAWL Specification Keys
- `places`: List of place names
- `transitions`: Transition definitions
- `preset`: Input places for transitions
- `produce`: Output places for transitions
- `is_task`: Mark as human task
- `is_service`: Mark as service call
- `timeout`: Service timeout in ms

---

## Best Practices

### 1. Pattern Selection
- Start with basic patterns (WCP-01-06) for most use cases
- Use advanced patterns only when necessary
- Consider human-in-the-loop for complex decisions

### 2. Performance Optimization
- Use deterministic choice for reproducible results
- Implement proper timeout handling
- Monitor with OpenTelemetry

### 3. Error Handling
- Always handle completion states
- Use receipts for audit trails
- Implement proper cleanup on cancellation

### 4. Testing
- Test all pattern combinations
- Validate with yawl_validate:validate/1
- Use wf_test_net_* modules for examples

---

**Remember**: Master the basic 6 patterns first - they handle 80% of workflow scenarios!