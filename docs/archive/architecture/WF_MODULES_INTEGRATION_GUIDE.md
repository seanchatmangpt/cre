# Workflow Modules Integration Guide

**13 New Workflow Utility Modules - Pure Utilities Architecture**

## Overview

This guide documents the 13 new workflow utility modules that implement the Joe Armstrong design principle: **all are pure utilities (stateless) except `wf_pool_worker` (OTP gen_server placeholder)**. These modules provide specialized functionality for workflow execution, testing, and management.

## Architecture Principles

### Pure Utilities (Stateless)
All modules are stateless and can be called independently without side effects.

### OTP Exceptions
Only `wf_pool_worker` is an OTP gen_server placeholder for worker pool management.

### Message Contracts
All modules follow consistent message contracts for integration.

## Module Categories

### 1. Audit & Logging

#### `wf_audit_log` - Append-Only Receipt Audit Log

**Purpose**: Disk-based audit logging for workflow receipts

**Key Features**:
- Append-only logging using `disk_log`
- Receipt tracking and audit trails
- Automatic log rotation and management

**API**:
```erlang
% Log a receipt
-spec log_receipt(receipt()) -> ok | {error, term()}.

% Get receipts for a workflow
-spec get_receipts(case_id()) -> [receipt()].

% Search receipts by criteria
-spec search_receipts(criteria()) -> [receipt()].

% Get audit summary
-spec audit_summary(case_id()) -> audit_summary().
```

**Integration**:
```erlang
% After each workflow step
case gen_pnet:step(Pid) of
    {ok, Receipt} ->
        wf_audit_log:log_receipt(Receipt),
        continue_workflow();
    abort ->
        handle_completion()
end.
```

### 2. Cancellation

#### `wf_cancel` - YAWL Cancellation Region Tokens

**Purpose**: Handle workflow cancellation with proper cleanup

**Key Features**:
- Cancellation region token generation
- Cleanup token injection
- Cancellation state management

**API**:
```erlang
% Create cancellation tokens
-spec create_cancellation_tokens(case_id(), [transition()]) -> cancel_map().

% Inject cancellation tokens
-spec inject_cancellation(case_id(), cancel_map()) -> ok.

% Check cancellation state
-spec is_cancelled(case_id()) -> boolean().

% Handle cancellation completion
-spec complete_cancellation(case_id()) -> ok.
```

**Integration**:
```erlang
% In workflow transition
fire(Transition, Mode, UsrInfo) ->
    case wf_cancel:is_cancelled(UsrInfo#usr_info.case_id) of
        true -> handle_cancelled_state();
        false -> handle_normal_state()
    end.
```

### 3. Concurrency

#### `wf_conc` - Concuerror Specification Support

**Purpose**: Concurrency testing with Concuerror specifications

**Key Features**:
- Concuerror behavior specifications
- Race condition detection
- Concurrency test generation

**API**:
```erlang
% Get Concuerror specifications
-spec conc_specs() -> [conc_spec()].

% Generate test cases
-spec generate_conc_tests() -> [test_case()].

% Check for race conditions
-spec check_races([operation()]) -> [race_condition()].

% Get concurrency safety analysis
-spec safety_analysis() -> safety_report().
```

**Integration**:
```erlang
% In test suites
case wf_conc:check_races(Operations) of
    [] -> test_passed();
    Races -> test_failed(Races)
end.
```

#### `wf_pool` - Worker Pool Management

**Purpose**: Efficient worker pooling for workflow execution

**Key Features**:
- Poolboy wrapper for worker management
- Dynamic scaling
- Worker health monitoring

**API**:
```erlang
% Start worker pool
-spec start_pool(pool_config()) -> {ok, pid()}.

% Get worker from pool
-spec get_worker(pool_ref()) -> pid().

% Return worker to pool
-spec return_worker(pool_ref(), pid()) -> ok.

% Monitor pool health
-spec pool_stats(pool_ref()) -> pool_stats().
```

**Integration**:
```erlang
% In workflow execution
case wf_pool:get_worker(Pool) of
    {ok, Worker} ->
        gen_pnet:call(Worker, execute_task, Task);
    {error, no_workers} ->
        handle_no_workers()
end.
```

#### `wf_pool_worker` - OTP Worker Placeholder (Stateful)

**Purpose**: Individual worker process in the pool

**Key Features**:
- OTP gen_server implementation
- Task execution isolation
- Resource cleanup

**API**:
```erlang
% Start worker
-spec start_link() -> {ok, pid()}.

% Execute task
-spec execute_task(term()) -> term().

% Health check
-spec health_check() -> boolean().
```

### 4. Decision Making

#### `wf_rules` - Erlog-Backed Rules Engine

**Purpose**: Business rule evaluation with Erlog backend

**Key Features**:
- Datalog-based rule evaluation
- Rule compilation and caching
- Consistency checking

**API**:
```erlang
% Load rules
-spec load_rules([rule()]) -> ok.

% Evaluate rules
-spec evaluate_rule(rule(), context()) -> boolean().

% Get rule dependencies
-spec rule_dependencies(rule()) -> [rule()].

% Validate rule set
-spec validate_rules([rule()]) -> validation_result().
```

**Integration**:
```erlang
% In workflow transitions
fire(Transition, Mode, UsrInfo) ->
    Context = build_context(Mode, UsrInfo),
    case wf_rules:evaluate_rule(approval_rule, Context) of
        true -> grant_approval();
        false -> deny_approval()
    end.
```

#### `wf_yawl_pred` - XPath to Datalog Translation

**Purpose**: Convert XPath expressions to Datalog predicates

**Key Features**:
- XPath pattern translation
- Datalog query optimization
- Pattern compilation

**API**:
```erlang
% Translate XPath to Datalog
-spec xpath_to_datalog(xpath()) -> datalog_query().

% Compile patterns
-spec compile_patterns([xpath()]) -> compiled_patterns().

% Evaluate pattern
-spec evaluate_pattern(xpath(), context()) -> [match()].
```

**Integration**:
```erlang
% In condition evaluation
is_enabled(Transition, Mode, UsrInfo) ->
    XPath = get_transition_condition(Transition),
    case wf_yawl_pred:evaluate_pattern(XPath, Mode) of
        [] -> false;
        Matches -> true
    end.
```

### 5. Multi-Instance

#### `wf_mi` - Parallel/Sequential/N-of-M Tasks

**Purpose**: Multi-instance task patterns with advanced features

**Key Features**:
- Parallel execution
- Sequential execution
- N-of-M pattern support
- Dynamic task generation

**API**:
```erlang
% Create multi-instance task
-spec create_mi_task(task_config()) -> mi_task().

% Execute parallel instances
-spec execute_parallel(mi_task(), [context()]) -> [result()].

% Execute sequential instances
-spec execute_sequential(mi_task(), [context()]) -> [result()].

% N-of-M execution
-spec execute_n_of_m(mi_task(), n(), m(), [context()]) -> [result()].
```

**Integration**:
```erlang
% In workflow definition
mi_task = wf_mi:create_mi_task(#{
    task => approval_task,
    type => parallel,
    instances => 3,
    data => ApprovalData
}),

Results = wf_mi:execute_parallel(mi_task, Contexts),
```

### 6. Operations

#### `wf_ops` - Process Monitoring

**Purpose**: Process monitoring with recon fallback

**Key Features**:
- Process statistics collection
- Memory monitoring
- Performance analysis
- Recon integration

**API**:
```erlang
% Get process statistics
-spec process_stats(pid()) -> process_stats().

% Monitor memory usage
-spec memory_stats(pid()) -> memory_stats().

% Get performance metrics
-spec performance_metrics(pid()) -> metrics().

% Health check
-spec health_check(pid()) -> health_status().
```

**Integration**:
```erlang
% In monitoring system
case wf_ops:health_check(Pid) of
    healthy -> continue;
    unhealthy -> take_action()
end.
```

### 7. Persistence

#### `wf_store` - Mnesia-Backed Workflow State

**Purpose**: Persistent workflow state storage

**Key Features**:
- Mnesia backend
- Transaction support
- State recovery
- Performance optimization

**API**:
```erlang
% Save workflow state
-spec save_state(case_id(), term()) -> ok | {error, term()}.

% Load workflow state
-spec load_state(case_id()) -> {ok, term()} | {error, term()}.

% Delete workflow state
-spec delete_state(case_id()) -> ok | {error, term()}.

% Get all workflow states
-spec list_states() -> [case_id()].
```

**Integration**:
```erlang
% In workflow lifecycle
init(NetArg) ->
    State = wf_store:load_state(NetArg#net_arg.case_id),
    {ok, State};

terminate(_Reason, State) ->
    wf_store:save_case_id(State),
    ok.
```

### 8. Testing

#### `wf_prop` - Property-Based Testing

**Purpose**: Property-based testing with PropEr

**Key Features**:
- Property generation
- Test case generation
- QuickCheck integration
- Shrink support

**API**:
```erlang
% Generate properties
-spec generate_property(prop_type()) -> property().

% Run property tests
-spec run_property_test(property()) -> test_result().

% Generate test cases
-spec generate_test_cases(property()) -> [test_case()].

% Property documentation
-spec property_doc(property()) -> string().
```

**Integration**:
```erlang
% In test suites
Property = wf_prop:generate_property(workflow_property()),
Result = wf_prop:run_property_test(Property),
case Result of
    true -> test_passed();
    false -> test_failed()
end.
```

### 9. Time Management

#### `wf_time` - Deadline Management

**Purpose**: Deadline and timeout handling

**Key Features**:
- Deadline tracking
- Timeout management
- Time-based triggers
- ISO 8601 duration parsing

**API**:
```erlang
% Set deadline
-spec set_deadline(case_id(), timeout()) -> ok.

% Check deadline
-spec check_deadline(case_id()) -> deadline_status().

% Get time remaining
-spec time_remaining(case_id()) -> timeout().

% Parse ISO duration
-spec parse_duration(binary()) -> duration().
```

**Integration**:
```erlang
% In workflow execution
is_enabled(Transition, Mode, UsrInfo) ->
    case wf_time:check_deadline(UsrInfo#usr_info.case_id) of
        valid -> check_normal_conditions();
        expired -> handle_expired()
    end.
```

#### `wf_timer` - Timer Management

**Purpose**: Timer-based workflow triggers

**Key Features**:
- One-shot timers
- Periodic timers
- Timer cancellation
- Timer events

**API**:
```erlang
% Start one-shot timer
-spec start_timer(case_id(), timeout(), event()) -> timer_ref().

% Start periodic timer
-spec start_periodic_timer(case_id(), interval(), event()) -> timer_ref().

% Cancel timer
-spec cancel_timer(timer_ref()) -> ok.

% Get active timers
-spec active_timers() -> [timer_ref()].
```

**Integration**:
```erlang
% In workflow initialization
init(NetArg) ->
    Timer = wf_timer:start_timer(NetArg#net_arg.case_id, 30000, timeout_event()),
    {ok, NetArg#{timer => Timer}};
```

### 10. Additional Utilities

#### `wf_task` - External Task Tokens

**Purpose**: External task integration

**Key Features**:
- Task token generation
- External task coordination
- Task state management

**API**:
```erlang
% Create task token
-spec create_task_token(task_info()) -> task_token().

% Complete task
-spec complete_task(task_token(), result()) -> ok.

% Get task status
-spec task_status(task_token()) -> task_status().
```

## Integration Patterns

### 1. Core Integration Pattern

```erlang
% Standard workflow integration
workflow_integration() ->
    % Initialize utilities
    wf_audit_log:init(),
    wf_store:init(),
    wf_time:init(),

    % Main workflow loop
    case gen_pnet:step(Pid) of
        {ok, Receipt} ->
            wf_audit_log:log_receipt(Receipt),
            wf_store:save_case_id(State),
            continue();
        abort ->
            handle_completion()
    end.
```

### 2. Concurrency Integration Pattern

```erlang
% Multi-instance execution
multi_instance_workflow() ->
    Task = wf_mi:create_mi_task(#{...}),
    Results = wf_mi:execute_parallel(Task, Contexts),

    % Process results
    case wf_rules:evaluate_rule(success_rule, Results) of
        true -> proceed();
        false -> retry()
    end.
```

### 3. Persistence Integration Pattern

```erlang
% Stateful workflow
stateful_workflow() ->
    % Load state
    case wf_store:load_case_id(CaseId) of
        {ok, State} -> continue_with_state(State);
        {error, not_found} -> start_new()
    end,

    % Save state periodically
    wf_store:save_case_id(CurrentState).
```

## Best Practices

### 1. Utility Usage
- Use pure utilities independently
- Keep state management in gen_pnet
- Follow message contracts consistently

### 2. Error Handling
- Handle all utility errors gracefully
- Provide meaningful error messages
- Implement retry logic where appropriate

### 3. Performance
- Cache expensive operations
- Use lazy evaluation where possible
- Profile utility performance

### 4. Testing
- Test utilities in isolation
- Test integration scenarios
- Use property-based testing for utilities

## Troubleshooting

### Common Issues

1. **State Persistence**: Ensure proper state saving/loading
2. **Timer Management**: Handle timer cleanup properly
3. **Memory Usage**: Monitor memory usage in pools
4. **Performance**: Profile utility calls for bottlenecks

### Debug Tips

1. **Logging**: Enable debug logging for utilities
2. **Tracing**: Use process tracing for workflow execution
3. **Monitoring**: Monitor pool and utility performance
4. **Documentation**: Refer to specific module documentation

## Migration Guide

### From Old Utilities
- Identify equivalent new utilities
- Update function calls to new APIs
- Add error handling for new features
- Update integration patterns

### To New Utilities
- Start with one utility at a time
- Test integration thoroughly
- Monitor performance impact
- Update documentation