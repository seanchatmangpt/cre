# YAWL Workflow Patterns Learning Path

## Overview

This learning path provides a structured approach to mastering the 43 YAWL (Yet Another Workflow Language) workflow patterns implemented in the CRE workflow engine. The guide follows a progressive learning methodology, starting from basic patterns and advancing to complex compositions, with hands-on examples tied to the actual implementation in `wf_engine.erl`.

### Learning Philosophy

- **Progressive Complexity**: Start with basic control flow patterns and gradually advance to complex compositions
- **Implementation-Driven**: Each pattern is directly tied to the `wf_engine.erl` implementation
- **Practical Focus**: 80/20 rule - focus on patterns that cover 80% of real-world use cases
- **Hands-on Practice**: Code examples and exercises for each pattern

## Version Information

- **CRE Version**: v0.2.1
- **OTP Support**: 25.0 - 28.x
- **Patterns Count**: 26 of 43 YAWL patterns implemented (60.5%)
- **Engine Module**: `wf_engine.erl`
- **Last Updated**: February 2025

## Quick Links

- [Learning Prerequisites](#learning-prerequisites)
- [Pattern Categories](#pattern-categories)
- [Core Engine Concepts](#core-engine-concepts)
- [Learning Path by Phase](#learning-path-by-phase)
- [Practice Exercises](#practice-exercises)

## Learning Prerequisites

### Required Knowledge
- Basic understanding of Erlang/OTP
- Familiarity with Petri net concepts
- Understanding of workflow fundamentals

### Development Setup
```bash
# Clone and setup the project
git clone <repository>
cd cre
rebar3 deps
rebar3 compile
rebar3 shell
```

### Key Files to Study
- `src/wf/wf_engine.erl` - Core workflow engine
- `src/patterns/*.erl` - Individual pattern implementations
- `src/yawl/yawl_patterns.erl` - Pattern composition utilities
- `test/wf_engine_SUITE.erl` - Engine test examples

## Core Engine Concepts

### Workflow Engine Architecture
The `wf_engine.erl` provides the runtime environment for executing YAWL patterns:

```erlang
% Start a workflow engine
{ok, Eng} = wf_engine:start_link(#{
    spec => Compiled,
    org => Org,
    seed => 1,
    now => 0
}).

% Start a workflow case
{ok, CaseId} = wf_engine:start_case(Eng, #{data => #{amount => 10}}, 0).

% Check case state
wf_engine:case_state(Eng, CaseId).  % running | completed | suspended
```

### Work Item Lifecycle
Engine manages work items through four states:
1. **offered** - Available for allocation
2. **allocated** - Assigned to a resource
3. **started** - Work in progress
4. **completed** - Work finished

```erlang
% Work item operations
[WI] = wf_engine:worklist(Eng, alice).  % Get available work items
WiId = maps:get(wi, WI),
wf_engine:allocate(Eng, WiId, alice, 0),
wf_engine:start_work(Eng, WiId, alice, 0),
wf_engine:complete(Eng, WiId, alice, #{approved => true}, 0).
```

### Receipt System
The engine maintains an audit trail through receipts:
```erlang
Receipts = wf_engine:drain_receipts(Eng, CaseId).
% Each receipt contains:
% - before_hash: State before transition
% - after_hash: State after transition
% - move: Transition details
% - ts: Timestamp
```

## Pattern Categories

| Category | Pattern Range | Complexity | Focus Areas |
|----------|---------------|------------|-------------|
| **Basic Control Flow** | WCP-01 to WCP-06 | Beginner | Sequential execution, simple choices |
| **Advanced Synchronization** | WCP-07 to WCP-10 | Intermediate | Complex joins, multi-branch synchronization |
| **Multiple Instances** | WCP-11 to WCP-17 | Advanced | Concurrent execution, dynamic patterns |
| **State-Based** | WCP-18 to WCP-20 | Intermediate | State-dependent behavior |
| **Extended Control Flow** | WCP-21 to WCP-28 | Advanced | Complex flow control, error handling |
| **Data Flow** | WDP-01 to WDP-05 | Intermediate | Data transformation, routing |
| **Resource** | WRP-01 to WRP-05 | Advanced | Resource management, allocation |
| **Exception Handling** | WHP-01 to WHP-05 | Advanced | Error recovery, compensation |

## Learning Path by Phase

### Phase 1: Foundation (1-2 days) - Basic Control Flow Patterns

#### Day 1: Sequential and Simple Branching
**Patterns**: WCP-01 (Sequence), WCP-02 (Parallel Split), WCP-03 (Synchronization), WCP-04 (Exclusive Choice)

**Learning Focus**:
- Understanding token flow through Petri nets
- Basic workflow state transitions
- Simple decision points

**Hands-on Practice**:
```erlang
% Start with a simple sequence pattern
{ok, Eng} = wf_engine:start_link(#{
    spec => sequence_pattern,  % Custom pattern module
    org => Org,
    seed => 1,
    now => 0
}).

% Execute sequential workflow
{ok, CaseId} = wf_engine:start_case(Eng, #{data => #{step => 1}}, 0).

% Work items will appear sequentially
work_items = wf_engine:worklist(Eng, alice).
```

**Key Files to Study**:
- `src/patterns/sequential.erl` (if exists)
- `src/patterns/parallel_split.erl`
- `src/patterns/exclusive_choice.erl`

#### Day 2: Merging Patterns
**Patterns**: WCP-05 (Simple Merge), WCP-06 (Multiple Merge), WCP-07 (Discriminator), WCP-08 (Deferred Choice)

**Learning Focus**:
- Branch convergence patterns
- Condition-based merging
- Nondeterministic choice

**Hands-on Practice**:
```erlang
% Practice exclusive choice with conditions
{ok, CaseId} = wf_engine:start_case(Eng, #{data => #{amount => 100}}, 0).

% Work item will show available choices
[WI] = wf_engine:worklist(Eng, alice),
Transition = maps:get(task, WI),  % Will show available choices

% Complete the work item with decision
wf_engine:complete(Eng, maps:get(wi, WI), alice, #{choice => 'approve'}, 0).
```

### Phase 2: Intermediate Patterns (3-4 days) - Synchronization and State

#### Day 3: Synchronization Patterns
**Patterns**: WCP-09 (Synchronization), WCP-10 (OR-Join), WCP-11 (Multi-Instance)

**Learning Focus**:
- Complex synchronization requirements
- Branch coordination
- Instance management

**Key Engine Features**:
- `wf_engine:enabled(Eng, CaseId)` - Check enabled transitions
- `wf_engine:tick(Eng, CaseId)` - Progress workflow
- State management in joins

#### Day 4: State-Based Patterns
**Patterns**: WCP-12 (Triggered Choice), WCP-13 (Milestone), WCP-14 (Cancel Case)

**Learning Focus**:
- State-dependent execution
- Event-driven workflows
- Cancellation handling

**Practice Exercise**:
```erlang
% Implement a milestone pattern
-module(milestone_example).
-behaviour(gen_yawl).

places() -> [start, milestone_reached, end].
transitions() -> [reach_milestone, complete].

preset(reach_milestone) -> [start];
preset(complete) -> [milestone_reached].

init(_) -> [].

fire(reach_milestone, #{start := [init]}, _) ->
    {produce, #{milestone_reached => [reached]}}.

fire(complete, #{milestone_reached := [reached]}, _) ->
    {produce, #{end => [done]}}.
```

### Phase 3: Advanced Patterns (5-7 days) - Multiple Instances and Error Handling

#### Day 5-6: Multiple Instance Patterns
**Patterns**: WCP-15 (Multiple Instance Sequential), WCP-16 (Multiple Instance Parallel), WCP-17 (N out of M)

**Learning Focus**:
- Dynamic instance creation
- Instance synchronization
- Voting and consensus patterns

**Engine Integration**:
```erlang
% Multiple instance workflow setup
{ok, Eng} = wf_engine:start_link(#{
    spec => n_out_of_m_pattern,
    org => Org,
    seed => 1,
    now => 0
}).

% Start with multiple instances
{ok, CaseId} = wf_engine:start_case(Eng, #{data => #{n => 2, m => 3}}, 0).

% Multiple work items will be created
work_items = wf_engine:worklist(Eng, alice).

% Complete instances according to N condition
wf_engine:complete(Eng, WiId1, alice, #{result => 'success'}, 0),
wf_engine:complete(Eng, WiId2, alice, #{result => 'success'}, 0).
```

#### Day 7: Exception Handling Patterns
**Patterns**: WCP-18 (Error Handler), WCP-19 (Retry), WCP-20 (Compensation)

**Learning Focus**:
- Error detection and recovery
- Retry mechanisms
- Compensation workflows

**Key Engine Features**:
- Error handling in `wf_engine:complete/5`
- Receipt-based audit trails
- State recovery mechanisms

### Phase 4: Expert Level (1-2 weeks) - Complex Patterns and Composition

#### Patterns Covered:
- **Extended Control Flow**: WCP-21 (Cancel Activity), WCP-22 (Cancel Case), WCP-23 (Terminate)
- **Data Flow**: WDP-01 (Data-Based Exclusive Choice), WDP-02 (Data-Based Parallel)
- **Resource**: WRP-01 (Sequential Allocation), WRP-02 (Parallel Allocation)
- **Exception**: WHP-01 (Cancel Region), WHP-02 (Cancel Case), WHP-03 (Compensation)

**Advanced Practice**:
- Pattern composition using `yawl_patterns:combine_patterns/2`
- Custom pattern development
- Performance optimization

## Practice Exercises

### Exercise 1: Basic Sequence
Create a simple sequence workflow that processes an order:
1. Receive order
2. Validate order
3. Process payment
4. Ship items
5. Complete order

```erlang
% Implement as a gen_yawl behaviour
-module(order_processing).
-behaviour(gen_yawl).

places() -> [receive_order, validate_order, process_payment, ship_items, complete_order].
transitions() -> [validate, process, ship, finish].

preset(validate) -> [receive_order];
preset(process) -> [validate_order];
preset(ship) -> [process_payment];
preset(finish) -> [ship_items].

init(_) -> [].

% Implement fire/3 functions
```

### Exercise 2: Parallel Processing
Implement a parallel split for order processing:
1. Single order received
2. Split into parallel branches:
   - Inventory check
   - Customer validation
   - Payment processing
3. Wait for all branches to complete
4. Ship order

### Exercise 3: Exception Handling
Create a workflow with error handling:
1. Process payment
2. If payment fails, retry up to 3 times
3. If still failed, trigger compensation
4. Log all errors and recovery attempts

### Exercise 4: Human-in-the-Loop
Implement an approval workflow:
1. Order submitted
2. Manager review (manual task)
3. If approved, continue
4. If rejected, send notification
5. Use `wf_engine:allocate/4` for human work items

## Advanced Topics

### Pattern Composition
Combine multiple patterns using the yawl_patterns module:

```erlang
% Combine parallel split with exclusive choice
Combined = yawl_patterns:combine_patterns([
    parallel_split_pattern,
    exclusive_choice_pattern
], #{strategy => sequential}).
```

### Performance Optimization
- Use `wf_engine:tick/2` for controlled workflow progression
- Monitor worklist sizes for load balancing
- Optimize transition firing logic

### Custom Pattern Development
Create new patterns by implementing the `gen_yawl` behaviour:

```erlang
-module(custom_pattern).
-behaviour(gen_yawl).

% Required callbacks
-export([places/0, transitions/0, preset/1, init/1, fire/3]).
```

## Troubleshooting Guide

### Common Issues

**1. Workflow stuck in pending state**
- Check initial marking in pattern implementation
- Verify transition preset functions
- Use `wf_engine:drain_events/1` to debug

**2. Work items not appearing**
- Check worklist queries with correct user/resource
- Verify transition firing conditions
- Review marking state

**3. Deadlock situations**
- Check Petri net soundness properties
- Verify transition dependencies
- Use engine's deadlock detection

### Debugging Tips
```erlang
% Check case state
wf_engine:case_state(Eng, CaseId).

% Get worklist
wf_engine:worklist(Eng, Resource).

% Get receipts for audit trail
wf_engine:drain_receipts(Eng, CaseId).

% Check enabled transitions
wf_engine:enabled(Eng, CaseId).
```

## Learning Resources

### Documentation
- [YAWL Patterns Reference](YAWL_PATTERNS_REFERENCE.md)
- [Complete API Reference](../COMPLETE_API_REFERENCE.md)
- [wf_engine.erl Documentation](../../src/wf/wf_engine.erl)

### Examples
- `test/wf_engine_SUITE.erl` - Engine test examples
- `src/patterns/*.erl` - Pattern implementations
- `examples/` - Workflow examples

### Community
- GitHub Issues: Report bugs and request features
- Documentation: Contribute improvements
- Tests: Add test cases for new patterns

## Assessment Checklist

### Basic Level Checklist
- [ ] Can implement basic sequence workflows
- [ ] Understands work item lifecycle
- [ ] Can use exclusive choice patterns
- [ ] Can create simple parallel workflows
- [ ] Understands basic error handling

### Intermediate Level Checklist
- [ ] Can implement complex synchronization patterns
- [ ] Understands multiple instance patterns
- [ ] Can use state-dependent workflows
- [ ] Understands milestone and cancellation patterns
- [ ] Can debug common workflow issues

### Advanced Level Checklist
- [ ] Can compose complex patterns
- [ ] Can implement custom patterns
- [ ] Understands performance optimization
- [ ] Can handle advanced error scenarios
- [ ] Understands human-in-the-loop workflows

### Expert Level Checklist
- [ ] Can design complex workflow systems
- [ ] Can optimize for performance and scale
- [ ] Can troubleshoot complex issues
- [ ] Can contribute to engine development
- [ ] Understands YAWL theory deeply

---

This learning path provides a comprehensive guide to mastering YAWL workflow patterns in the CRE engine. Remember to practice each pattern thoroughly before moving to the next level, and always refer to the actual implementation in `wf_engine.erl` for detailed understanding.