# CRE Workflow Integration Tests

This directory contains comprehensive end-to-end integration tests for workflow execution in the CRE (Common Runtime Environment) YAWL workflow engine.

## Overview

The integration test suites validate the complete lifecycle of workflow execution including creation, execution, patterns, timers, error handling, and state persistence. All tests are written using Erlang/OTP Common Test framework.

## Test Suites

### 1. workflow_basic_SUITE.erl

**Purpose:** Test basic workflow lifecycle operations

**Test Groups:**
- `basic_lifecycle` - Workflow creation, starting, execution, and completion
- `token_management` - Token flow, inject/withdraw, step/drain operations
- `state_management` - State queries, statistics, sync operations
- `concurrency` - Multiple instances, termination, timeout handling

**Test Cases:** 15 test cases covering:
- Creating and stopping workflows
- Starting workflows with initial data
- Executing single transitions
- Complete workflow execution
- Token flow through places
- Marking state transitions
- State query operations
- Multiple concurrent workflow instances
- Graceful termination
- Timeout handling

**Run Command:**
```bash
rebar3 ct --suite test/integration/workflow_basic_SUITE
```

---

### 2. workflow_patterns_SUITE.erl

**Purpose:** Test pattern-based workflow execution (WCP-01 to WCP-10)

**Test Groups:**
- `basic_patterns` - Sequence patterns
- `split_join_patterns` - Parallel split and synchronization
- `choice_merge_patterns` - Exclusive choice, multi-choice, merges
- `advanced_patterns` - Multi-merge, discriminator, arbitration
- `complex_workflows` - Combined patterns and nested structures

**Test Cases:** 21 test cases covering:
- **WCP-01 Sequence:** Basic sequence, long chains, data passing
- **WCP-02 Parallel Split:** Basic split, multiple branches, token distribution
- **WCP-03 Synchronization:** Basic join, multiple inputs, deadlock prevention
- **WCP-04 Exclusive Choice:** Condition evaluation, default paths
- **WCP-05 Simple Merge:** Basic merge operations
- **WCP-06 Multi-Choice:** Multiple path selection
- **WCP-07 Synchronizing Merge:** Synchronized merge operations
- **WCP-08 Multi-Merge:** Multiple token collection
- **WCP-09 Discriminator:** First-completion patterns
- **WCP-10 Arbitration:** N-of-M synchronization
- Complex workflows combining multiple patterns

**Run Command:**
```bash
rebar3 ct --suite test/integration/workflow_patterns_SUITE
```

---

### 3. workflow_timer_SUITE.erl

**Purpose:** Test timer-based workflow operations

**Test Groups:**
- `deadline_management` - Task deadlines and enforcement
- `delay_management` - Delayed task activation
- `timeout_management` - Task and workflow timeouts
- `periodic_execution` - Periodic task execution and cancellation
- `timer_queue_ops` - Timer queue ordering and priority
- `time_based_routing` - Conditional routing based on time
- `sla_management` - SLA monitoring and enforcement

**Test Cases:** 21 test cases covering:
- Deadline enforcement and violation handling
- Deadline extension
- Delayed task activation
- Multiple delayed tasks
- Delay cancellation
- Task and workflow timeouts
- Timeout recovery
- Periodic task execution and cancellation
- Backpressure handling
- Timer queue ordering and priority
- Time-based conditional routing
- Business hours routing
- Calendar-based routing
- SLA monitoring and metrics
- SLA violation escalation

**Run Command:**
```bash
rebar3 ct --suite test/integration/workflow_timer_SUITE
```

---

### 4. workflow_scope_SUITE.erl

**Purpose:** Test scope management and error handling

**Test Groups:**
- `exception_handling` - Exception throw, error, exit, propagation
- `compensation` - Compensation handlers and chains
- `cancellation` - Activity, case, region, and scope cancellation
- `recovery` - Retry, fallback, checkpoint recovery
- `fault_tolerance` - Fault isolation, circuit breakers
- `rollback` - State rollback mechanisms
- `saga_pattern` - Saga pattern with compensation
- `try_catch` - Try-catch-finally blocks

**Test Cases:** 28 test cases covering:
- Exception handling (throw, error, exit)
- Exception propagation through nested scopes
- Custom exception handlers
- Basic and chained compensation
- Partial and nested compensation
- WCP-19 Cancel Activity
- WCP-20 Cancel Case
- Region-based cancellation
- Scope cancellation with propagation
- Retry recovery strategies
- Fallback recovery
- Checkpoint-based recovery
- Resume after recovery
- Fault tolerance and isolation
- Circuit breaker pattern
- Basic and nested rollback
- Distributed rollback
- Saga pattern implementation
- Saga compensation on failure
- Parallel saga execution
- Try-catch-finally blocks

**Run Command:**
```bash
rebar3 ct --suite test/integration/workflow_scope_SUITE
```

---

### 5. workflow_persistence_SUITE.erl

**Purpose:** Test workflow state persistence and recovery

**Test Groups:**
- `checkpointing` - State checkpointing operations
- `recovery` - Recovery from checkpoints
- `serialization` - State serialization/deserialization
- `distributed_state` - Distributed checkpoint management
- `migration` - Version migration and schema changes
- `storage_optimization` - Compression, deduplication, cleanup
- `mnesia_integration` - Mnesia database integration
- `advanced_persistence` - Audit trails, snapshots, event sourcing

**Test Cases:** 27 test cases covering:
- Basic checkpoint save and load
- Interval-based checkpointing
- Incremental checkpointing
- Checkpoint compression
- Checkpoint versioning
- Recovery from checkpoints
- Partial state recovery
- Corrupted checkpoint handling
- Multiple instance recovery
- Marking and user info serialization
- Complex data serialization
- Deserialization validation
- Distributed checkpoint operations
- Distributed recovery
- State synchronization across nodes
- Version upgrade migration
- Schema change migration
- Backward compatibility
- Storage compression
- Storage deduplication
- Storage cleanup
- Mnesia checkpoint save/load
- Mnesia transactions
- Audit trail persistence
- Snapshot-based persistence
- Event sourcing

**Run Command:**
```bash
rebar3 ct --suite test/integration/workflow_persistence_SUITE
```

---

## Running All Integration Tests

### Run all integration suites:
```bash
rebar3 ct --dir test/integration
```

### Run specific test group:
```bash
rebar3 ct --suite test/integration/workflow_basic_SUITE --group basic_lifecycle
```

### Run specific test case:
```bash
rebar3 ct --suite test/integration/workflow_basic_SUITE --case create_simple_workflow_test
```

### Run with coverage:
```bash
rebar3 ct --cover --dir test/integration
```

## Test Statistics

| Test Suite | Test Groups | Test Cases | Coverage Areas |
|------------|-------------|------------|----------------|
| workflow_basic_SUITE | 4 | 15 | Lifecycle, tokens, state, concurrency |
| workflow_patterns_SUITE | 5 | 21 | WCP-01 to WCP-10, complex patterns |
| workflow_timer_SUITE | 7 | 21 | Timers, delays, timeouts, SLA |
| workflow_scope_SUITE | 8 | 28 | Errors, compensation, cancellation |
| workflow_persistence_SUITE | 8 | 27 | Checkpoints, recovery, storage |
| **TOTAL** | **32** | **112** | **Complete E2E coverage** |

## Test Architecture

### Setup/Teardown Hierarchy

```
init_per_suite/1           - Suite-level setup (Mnesia, modules)
  └─ init_per_group/2      - Group-level setup
      └─ init_per_testcase/2   - Test case setup
          └─ Test Execution
      └─ end_per_testcase/2    - Test case cleanup
  └─ end_per_group/2       - Group-level cleanup
end_per_suite/1            - Suite-level cleanup
```

### Common Test Callbacks

All suites implement:
- `all/0` - Returns list of test groups
- `groups/0` - Defines test group structure
- `init_per_suite/1` - Suite initialization
- `end_per_suite/1` - Suite cleanup
- `init_per_group/2` - Group initialization
- `end_per_group/2` - Group cleanup
- `init_per_testcase/2` - Test case setup
- `end_per_testcase/2` - Test case teardown

### Assertions

Tests use EUnit assertion macros:
- `?assertEqual(Expected, Actual)` - Exact equality
- `?assertMatch(Pattern, Value)` - Pattern matching
- `?assert(Condition)` - Boolean condition
- `?assertNot(Condition)` - Negated boolean
- `?assertException(Class, Pattern, Expr)` - Exception checking

## Mock Workflow Modules

The test suites reference mock workflow modules that would typically be implemented separately:

### Basic Workflow Mocks
- `simple_sequence_net` - Basic A->B->C sequence
- `data_workflow_net` - Workflow with data passing
- `error_prone_net` - Workflow that may encounter errors

### Pattern Workflow Mocks
- `sequence_pattern_net` - Configurable sequence
- `parallel_split_net` - Parallel split pattern
- `synchronization_net` - Synchronization pattern
- `exclusive_choice_net` - Choice pattern
- `simple_merge_net` - Merge pattern
- `multi_choice_net` - Multi-choice pattern
- `synchronizing_merge_net` - Synchronizing merge
- `multi_merge_net` - Multi-merge pattern
- `discriminator_net` - Discriminator pattern
- `arbitration_net` - N-of-M arbitration

### Timer Workflow Mocks
- `deadline_workflow_net` - Deadline enforcement
- `delayed_workflow_net` - Delayed activation
- `timeout_workflow_net` - Timeout handling
- `periodic_workflow_net` - Periodic execution
- `timer_queue_net` - Timer queue management
- `sla_workflow_net` - SLA monitoring

### Scope Workflow Mocks
- `exception_workflow_net` - Exception handling
- `compensation_workflow_net` - Compensation handling
- `cancel_region_net` - Region cancellation
- `recovery_retry_net` - Retry recovery
- `saga_workflow_net` - Saga pattern
- `try_catch_net` - Try-catch blocks

### Persistence Workflow Mocks
- `persistence_workflow_net` - General persistence testing

## Integration with CI/CD

### Docker-based Testing

```bash
# Build and run tests in container
docker buildx bake --load
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 ct --dir test/integration
```

### Cloud Build Integration

The integration tests are designed to run in Cloud Build pipelines:

```yaml
# cloudbuild.yaml snippet
- name: 'cre:0.3.0'
  args: ['rebar3', 'ct', '--dir', 'test/integration']
  env:
    - 'MIX_ENV=test'
```

## Test Output

Test results are generated in:
- `_build/test/logs/` - HTML test reports
- `_build/test/cover/` - Coverage reports (if --cover used)

View HTML report:
```bash
open _build/test/logs/index.html
```

## Writing New Integration Tests

### Template for New Test Suite

```erlang
-module(my_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([my_test_case/1]).

all() -> [{group, my_group}].

groups() -> [{my_group, [], [my_test_case]}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

my_test_case(_Config) ->
    {ok, Pid} = gen_yawl:start_link(my_net, #{}, []),
    ?assert(is_process_alive(Pid)),
    ok = gen_yawl:stop(Pid),
    ok.
```

## Best Practices

1. **Test Isolation:** Each test case should be independent
2. **Cleanup:** Always stop workflow processes in teardown
3. **Timeouts:** Use appropriate timeouts for async operations
4. **Logging:** Use `ct:pal/2` for test debugging
5. **Assertions:** Use specific assertions for clear failure messages
6. **Documentation:** Document test purpose and expected behavior

## Troubleshooting

### Common Issues

**Test timeout:**
- Increase timeout in `gen_yawl:sync/2`
- Check for deadlocks in workflow definition

**Process not stopping:**
- Ensure `gen_yawl:stop/1` is called in cleanup
- Check for lingering timers or messages

**Mnesia errors:**
- Verify Mnesia is initialized in init_per_suite
- Check schema creation succeeded

**Module not found:**
- Ensure modules are compiled with `rebar3 compile`
- Check module names match expectations

## Contributing

When adding new integration tests:
1. Create tests in appropriate suite or new suite
2. Follow existing naming conventions
3. Add documentation to this README
4. Update test statistics table
5. Ensure tests pass: `rebar3 ct --dir test/integration`

## References

- [Common Test User Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)
- [EUnit Documentation](https://www.erlang.org/doc/apps/eunit/chapter.html)
- [YAWL Workflow Patterns](http://www.workflowpatterns.com/)
- [CRE Documentation](../../docs/)

---

**Last Updated:** 2026-02-11
**Test Suites:** 5
**Total Test Cases:** 112
**Coverage:** Complete end-to-end workflow execution
