# YAWL Workflow Patterns: 80/20 Workbook

## Practice Exercises Following the 80/20 Learning Path

---

## Phase 1: Quick Start Exercises

### Exercise Set 1: Basic Pattern Identification (5 minutes)

**Instructions**: Identify the correct YAWL pattern for each scenario and note which Erlang modules would be involved.

1. **Document Approval Process**: Sequential steps of review → approval → filing
   - Pattern: _________________________
   - Key Modules: _____________________
   - Implementation: _____________________

2. **Order Processing**: Check inventory, process payment, ship order in parallel
   - Pattern: _________________________
   - Key Modules: _____________________
   - Implementation: _____________________

3. **Multi-Stage Interview**: Candidate must pass all interview stages sequentially
   - Pattern: _________________________
   - Key Modules: _____________________
   - Implementation: _____________________

4. **Loan Application Review**: Multiple departments review application, proceed if any approve
   - Pattern: _________________________
   - Key Modules: _____________________
   - Implementation: _____________________

5. **Website Deployment**: Build, test, deploy sequentially with rollback option
   - Pattern: _________________________
   - Key Modules: _____________________
   - Implementation: _____________________

### Exercise Set 2: Basic Pattern Implementation (10 minutes)

**Instructions**: Implement simple YAWL patterns using the pnet_net behaviour.

1. **Simple Sequence Pattern**:
```erlang
% Implement a 3-step sequence: start → task1 → task2 → end
% Fill in the missing parts
-module(sequence_exercise).
-behaviour(pnet_net).

places() -> [_________________________].
transitions() -> [_________________________].

preset(t1) -> [start];
preset(t2) -> [task1].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [_________________________].

% Implement modes and fire functions
modes(t1, #{_________________________}, _UsrInfo) -> [#{_________________________}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{task1 => [_________________________]}}.
```

2. **Parallel Split Pattern**:
```erlang
% Implement parallel split with 3 branches
-module(parallel_exercise).
-behaviour(pnet_net).

places() -> [start, branch1, branch2, branch3, end].
transitions() -> [_________________________].

preset(split) -> [start];
preset(t1) -> [branch1];
preset(t2) -> [branch2];
preset(t3) -> [branch3].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init].

modes(split, #{start := [init]}, _UsrInfo) ->
    [#{start => [], branch1 => [_________________________],
       branch2 => [_________________________], branch3 => [_________________________]}].
```

### Exercise Set 3: Basic API Usage (15 minutes)

**Instructions**: Use the wf_engine API to manage workflow cases.

1. **Workflow Creation and Start**:
```erlang
% Create a simple workflow engine and start a case
-module(engine_exercise).

-include_lib("kernel/include/logger.hrl").

start_workflow() ->
    % Create workflow specification (sequence of 2 tasks)
    Spec = #{_________________________},
    % Start the engine
    {ok, Eng} = wf_engine:start_link(#{
        spec => Spec,
        org => #{},
        seed => 1,
        now => 0
    }),
    % Start a case with some data
    {ok, CaseId} = wf_engine:start_case(Eng, #{_________________________}, 0),
    ?INFO("Started case: ~p", [CaseId]),
    {ok, Eng, CaseId}.
```

2. **Work Item Management**:
```erlang
% Allocate and complete work items
manage_workitem(Eng, CaseId) ->
    % Get worklist for user 'alice'
    Worklist = wf_engine:_________________________(Eng, alice),
    ?INFO("Worklist: ~p", [Worklist]),

    case Worklist of
        [WI | _] ->
            WiId = WI#work_item.wi,
            % Allocate work item to alice
            ok = wf_engine:_________________________(Eng, WiId, alice, 0),
            % Start work
            ok = wf_engine:_________________________(Eng, WiId, alice, 0),
            % Complete work
            ok = wf_engine:_________________________(Eng, WiId, alice, #{approved => true}, 0);
        _ ->
            ?INFO("No work items available")
    end.
```

---

## Phase 2: Core Mastery Exercises

### Exercise Set 4: Advanced Pattern Implementation (20 minutes)

**Instructions**: Implement more complex YAWL patterns.

1. **Exclusive Choice Pattern**:
```erlang
% Implement workflow with conditional routing based on data
-module(choice_exercise).
-behaviour(pnet_net).

places() -> [start, high_priority, low_priority, end].
transitions() -> [check_priority, high_route, low_route].

preset(check_priority) -> [start];
preset(high_route) -> [high_priority];
preset(low_route) -> [low_priority].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init].

% Mode depends on data value
modes(check_priority, #{start := [init]}, UsrInfo) ->
    Data = maps:get(data, UsrInfo, #{}),
    case maps:get(priority, Data, normal) of
        high ->
            [#{start => [], high_priority => [init]}];
        _ ->
            [#{start => [], low_priority => [init]}]
    end.

fire(check_priority, #{start => []}, _UsrInfo) ->
    {produce, #{}}.
```

2. **Multiple Instances Pattern**:
```erlang
% Implement multiple instances of a task
-module(multi_instance_exercise).
-behaviour(pnet_net).

places() -> [start, instance1, instance2, instance3, end].
transitions() -> [split, t1, t2, t3, merge].

% Implement the rest of the pattern...
% - preset configuration
% - mode handling for multiple instances
% - firing logic
```

### Exercise Set 5: Human-in-the-Loop Workflows (20 minutes)

**Instructions**: Implement workflows with human tasks.

1. **Approval Workflow**:
```erlang
% Create a workflow with human approval tasks
-module(approval_workflow).
-behaviour(pnet_net).

places() -> [start, manager_review, director_review, end].
transitions() -> [start_review, manager_approve, director_approve].

% Mark as human tasks
start_review(#{start := [init]}, _UsrInfo) ->
    {produce, #{manager_review => [{task, "manager_review_task"}]}}.

% Add proper preset, modes, and implement fire logic
```

2. **Work Item Lifecycle**:
```erlang
% Implement complete work item lifecycle
-module(workitem_lifecycle).

% Test the complete flow:
% 1. Start case with human task
% 2. Get worklist
% 3. Allocate work item
% 4. Start work
% 5. Complete with result data
% 6. Verify case completion
test_complete_flow() ->
    % Your implementation here
    ok.
```

### Exercise Set 6: Service Integration (20 minutes)

**Instructions**: Implement workflows with external service calls.

1. **Service Orchestration**:
```erlang
% Create workflow with service calls
-module(service_workflow).
-behaviour(pnet_net).

places() -> [start, verify_user, check_inventory, process_payment, ship_order, end].
transitions() -> [start_process, verify_service, inventory_service, payment_service, ship_service].

% Mark transitions as service calls
verify_service(#{verify_user := [init]}, _UsrInfo) ->
    {produce, #{check_inventory => [init]}}.

% Add service call configuration and timeouts
```

2. **Service Error Handling**:
```erlang
% Implement service timeout and retry logic
-module(service_error_handling).

% Create a workflow that:
% 1. Calls external service
% 2. Handles timeout
% 3. Implements retry logic
% 4. Falls back to alternative flow
handle_service_timeout() ->
    % Your implementation here
    ok.
```

### Exercise Set 7: Advanced API Features (20 minutes)

**Instructions**: Use advanced workflow engine features.

1. **Event Handling and Publishing**:
```erlang
% Implement event-driven workflow
-module(event_workflow).

% Use wf_engine:publish/3 to send events
% Use wf_engine:drain_events/1 to consume events
test_event_handling() ->
    % Create workflow with event publishing
    % Start case
    % Publish events from workflow
    % Drain and verify events
    ok.
```

2. **Worklet Integration**:
```erlang
% Implement dynamic task replacement with worklets
-module(worklet_example).

% Define worklet rules for task replacement
% Apply worklet to workflow execution
% Test task replacement logic
test_worklet_replacement() ->
    Worklet = #{
        rules => [
            {#{task => review_task, data => #{amount => high}},
             #{replacement => premium_review_task}}
        ]
    },
    % Your test implementation
    ok.
```

---

## Phase 3: Advanced Integration Exercises

### Exercise Set 8: Complex Business Workflows (30 minutes)

**Instructions**: Implement end-to-end business workflows.

1. **Loan Approval Workflow**:
```erlang
% Complete loan approval with multiple instances and data flow
-module(loan_approval_workflow).

% Implement:
% - Application submission
% - Credit check (service call)
% - Income verification (service call)
% - Multiple reviewer instances
% - Final decision logic
% - Notification system
```

2. **Order Fulfillment System**:
```erlang
% Complete order processing with inventory, payment, shipping
-module(order_fulfillment).

% Implement:
% - Order validation
% - Parallel inventory check, payment processing, shipping preparation
% - Synchronization of all services
% - Error handling and retry logic
% - Customer notification
```

### Exercise Set 9: Performance Optimization (30 minutes)

**Instructions**: Optimize workflow performance.

1. **Large-Scale Workflow**:
```erlang
% Implement workflow with 1000+ instances
-module(large_scale_workflow).

% Test:
% - Memory usage optimization
% - Token management efficiency
% - Deterministic choice performance
% - Parallel execution scaling
```

2. **Caching and Optimization**:
```erlang
% Implement workflow optimization strategies
-module(optimization_test).

% Test:
% - Persistent term usage for configuration
% - Efficient marking operations
% - Batch processing of work items
% - Connection pooling for services
```

---

## Implementation Projects

### Project 1: Document Management System
**Goal**: Implement a complete document approval workflow

**Requirements**:
- Document upload
- Sequential review stages
- Conditional routing based on document type
- Human approval tasks
- Version control
- Audit trail with receipts

**Implementation**:
```erlang
% Your complete implementation goes here
```

### Project 2: Customer Onboarding System
**Goal**: Implement customer onboarding with multiple services

**Requirements**:
- Customer data collection
- Credit check service
- Background verification service
- Multiple approval levels
- Welcome email generation
- Error handling and retry logic

**Implementation**:
```erlang
% Your complete implementation goes here
```

### Project 3: Manufacturing Workflow
**Goal**: Implement complex manufacturing process workflow

**Requirements**:
- Order processing
- Material requirements planning
- Production scheduling
- Quality control checkpoints
- Shipping coordination
- Inventory management

**Implementation**:
```erlang
% Your complete implementation goes here
```

---

## Testing Framework

### Unit Testing Patterns
```erlang
-include_lib("eunit/include/eunit.hrl").

sequence_test() ->
    % Test sequence pattern execution
    ok.

workitem_allocation_test() ->
    % Test work item allocation and completion
    ok.

service_integration_test() ->
    % Test service calls and timeouts
    ok.
```

### Integration Testing
```erlang
integration_test() ->
    % Test complete workflow end-to-end
    % Validate state transitions
    % Verify work item lifecycle
    % Check audit trails
    ok.
```

### Performance Testing
```erlang
performance_test() ->
    % Test with large datasets
    % Measure memory usage
    % Test concurrent execution
    % Benchmark critical operations
    ok.
```

---

## Answer Key

### Phase 1 Exercises

**Exercise Set 1**:
1. Sequence Pattern - wf_engine, pnet_types
2. Parallel Split Pattern - wf_engine, pnet_marking
3. Sequence Pattern - wf_engine, wf_task
4. Discriminator Pattern - wf_engine, pnet_choice
5. Exclusive Choice Pattern - wf_engine, wf_data

**Exercise Set 2**:
[Fill in the missing pattern implementations]

**Exercise Set 3**:
[Fill in the missing API calls]

### Phase 2 Exercises

**Exercise Set 4-7**:
[Advanced pattern implementations]

### Phase 3 Exercises

**Exercise Set 8-9**:
[Complex workflow implementations]

### Implementation Projects
[Complete project implementations]

---

## Progress Tracking

### Self-Assessment Checklist

**Phase 1 Mastery**:
- [ ] Can identify basic YAWL patterns correctly
- [ ] Can implement the 6 essential patterns
- [ ] Can use basic wf_engine API functions
- [ ] Can create simple workflow specifications

**Phase 2 Mastery**:
- [ ] Can implement advanced patterns (WCP-07 to WCP-28)
- [ ] Can handle human-in-the-loop workflows
- [ ] Can integrate external services
- [ ] Can use advanced API features

**Phase 3 Mastery**:
- [ ] Can implement complex business workflows
- [ ] Can optimize performance for large-scale systems
- [ ] Can handle complex error scenarios
- [ ] Can create comprehensive test suites

This workbook provides comprehensive practice following the 80/20 principle, ensuring mastery of the most valuable YAWL workflow patterns first.