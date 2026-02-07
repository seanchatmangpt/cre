# Test Organization Reference

**Test Structure and Organization for YAWL Workflow Engine**

## Overview

This document provides a comprehensive reference for the test organization of the CRE YAWL workflow engine. The test suite is organized by "slice" to provide clear mapping between functionality and test coverage, ensuring comprehensive testing across all components.

## Test Organization by Slice

The test suite is organized into "slices" that group related functionality together. Each slice represents a level of complexity and type of testing required.

### Slice Concept

A "slice" represents a logical grouping of functionality that can be tested together. The slice concept helps organize tests by complexity and ensures complete coverage of all system components.

| Slice | Complexity | Test Type | Coverage Target | Key Components |
|-------|------------|-----------|-----------------|----------------|
| **0-3** | Basic | Unit Tests | 100% | Petri net fundamentals |
| **4-7** | Intermediate | Integration Tests | 90% | YAWL patterns |
| **8-10** | Advanced | Property Tests | 85% | Advanced features |
| **11-13** | Complex | Performance Tests | Baseline | Scalability |

## Slice 0-3: Basic Petri Net Operations

### Files in Slice 0-3

```
test/
├── pnet_types_SUITE.erl        # Type validation tests
├── pnet_marking_SUITE.erl     # Marking algebra tests
├── pnet_mode_SUITE.erl        # Mode enumeration tests
├── pnet_choice_SUITE.erl      # Choice logic tests
├── gen_pnet_SUITE.erl         # Generic Petri net behavior
├── wf_audit_log_SUITE.erl     # Audit logging tests
├── wf_cancel_SUITE.erl        # Cancellation tests
├── wf_conc_SUITE.erl         # Concurrency tests
└── wf_timerq_SUITE.erl       # Timer queue tests
```

### Test Coverage by Module

#### `pnet_types_SUITE.erl`

```erlang
-module(pnet_types_SUITE).
-include_lib("common_test/include/ct.hrl").

% Test type validation
type_validation_test(_Config) ->
    ValidType = #transition{id => t1, name => <<"Task1">>},
    ?assertEqual(ok, pnet_types:validate_transition(ValidType)),

    InvalidType = #transition{id => invalid_id, name => <<"">>},
    ?assertMatch({error, _}, pnet_types:validate_transition(InvalidType)).

% Test total validation functions
total_validation_test(_Config) ->
    Net = create_test_net(),
    ?assertEqual(ok, pnet_types:validate_net(Net)).
```

#### `pnet_marking_SUITE.erl`

```erlang
-module(pnet_marking_SUITE).
-include_lib("common_test/include/ct.hrl").

% Test multiset operations
multiset_operations_test(_Config) ->
    Marking1 = #{place1 => [token1, token2]},
    Marking2 = #{place1 => [token3]},

    % Union
    Union = pnet_marking:union(Marking1, Marking2),
    ?assertEqual([token1, token2, token3], maps:get(place1, Union)),

    % Difference
    Diff = pnet_marking:diff(Marking1, #{place1 => [token2]}),
    ?assertEqual([token1], maps:get(place1, Diff)).
```

#### `gen_pnet_SUITE.erl`

```erlang
-module(gen_pnet_SUITE).
-include_lib("common_test/include/ct.hrl").

% Test basic workflow execution
basic_execution_test(_Config) ->
    Spec = create_simple_spec(),
    {ok, Pid} = gen_pnet:start_link(Spec, #{}, []),

    {ok, Receipt} = gen_pnet:step(Pid),
    ?assert(is_record(Receipt, receipt)),

    gen_pnet:stop(Pid).
```

### Testing Approach for Slice 0-3

- **Unit Testing**: Each component tested in isolation
- **Mock Dependencies**: External services mocked using meck
- **Boundary Testing**: Test edge cases and error conditions
- **Performance**: Ensure basic operations are fast

## Slice 4-7: YAWL Pattern Implementations

### Files in Slice 4-7

```
test/
├── yawl_validate_SUITE.erl      # YAWL specification validation
├── yawl_compile_SUITE.erl      # Compilation tests
├── yawl_compiled_SUITE.erl     # Compiled net access tests
├── orderfulfillment_2_1_doctest.erl  # Order fulfillment tests
├── wf_scope_SUITE.erl          # Workflow scope tests
├── wf_spec_SUITE.erl          # Specification creation tests
├── wf_rules_SUITE.erl         # Rules engine tests
├── wf_yawl_pred_SUITE.erl     # YAWL predicate tests
├── wf_mi_SUITE.erl            # Multi-instance tests
├── wf_ops_SUITE.erl           # Process monitoring tests
├── wf_store_SUITE.erl         # State persistence tests
├── wf_prop_SUITE.erl          # Property-based tests
├── wf_time_SUITE.erl          # Time management tests
└── wf_timer_SUITE.erl         # Timer tests
```

### Test Coverage by Module

#### `yawl_validate_SUITE.erl`

```erlang
-module(yawl_validate_SUITE).
-include_lib("common_test/include/ct.hrl").

% Test YAWL specification validation
valid_spec_test(_Config) ->
    Spec = create_valid_yawl_spec(),
    ?assertEqual(ok, yawl_validate:validate(Spec)).

invalid_spec_test(_Config) ->
    Spec = create_invalid_yawl_spec(),
    ?assertMatch({error, _}, yawl_validate:validate(Spec)).
```

#### `yawl_compile_SUITE.erl`

```erlang
-module(yawl_compile_SUITE).
-include_lib("common_test/include/ct.hrl").

% Test compilation to gen_pnet module
compile_test(_Config) ->
    Spec = create_yawl_spec(),
    {ok, Module} = yawl_compile:compile(Spec, #{seed => 123}),

    % Test generated module
    ?assert([t_task1, t_task2] = Module:trsn_lst()),
    ?assert([input, task1, task2, output] = Module:place_lst()).

compile_to_file_test(_Config) ->
    Spec = create_yawl_spec(),
    OutputDir = "/tmp/test_output",
    ok = yawl_compile:compile_to_file(Spec, OutputDir, #{}),

    % Verify file was created
    Files = file:list_dir(OutputDir),
    ?assert(lists:any(fun(F) -> lists:suffix(".erl", F) end, Files)).
```

#### `orderfulfillment_2_1_doctest.erl`

```erlang
-module(orderfulfillment_2_1_doctest).
-include_lib("eunit/include/eunit.hrl").

% Test order fulfillment workflow
order_processing_test() ->
    Spec = wf_spec:create_order_fulfillment(),
    {ok, OrderNet} = yawl_compile:compile(Spec, #{seed => 123}),

    {ok, Pid} = gen_pnet:start_link(OrderNet, #{order_id => "12345"}, []),

    % Execute workflow
    {ok, Receipt1} = gen_pnet:step(Pid),  % Process order
    {ok, Receipt2} = gen_pnet:step(Pid),  % Check inventory
    {ok, Receipt3} = gen_pnet:step(Pid),  % Ship order

    % Verify completion
    {ok, FinalMarking} = gen_pnet:marking(Pid),
    ?assertMatch(#{output := [_]}, FinalMarking),

    gen_pnet:stop(Pid).
```

### Testing Approach for Slice 4-7

- **Integration Testing**: Test complete YAWL workflows
- **Real Scenarios**: Use realistic workflow examples
- **Pattern Testing**: Test all 43 workflow patterns
- **End-to-End**: Test workflow from start to finish

## Slice 8-10: Advanced Features

### Files in Slice 8-10

```
test/
├── wf_audit_log_integration_test.erl    # Audit log integration
├── wf_checkpoint_resume_test.erl        # Checkpoint/resume tests
├── wf_compound_doctests.erl             # Compound pattern tests
├── wf_compound_integration_SUITE.erl    # Compound integration tests
├── wf_deterministic_replay_test.erl     # Replay tests
├── wf_pool_transaction_test.erl         # Pool transaction tests
├── wf_receipt_test.erl                  # Receipt tracking tests
├── yawl_cancellation_test.erl          # Cancellation tests
└── yawl_performance_SUITE.erl           # Performance tests
```

### Test Coverage by Module

#### `wf_checkpoint_resume_test.erl`

```erlang
-module(wf_checkpoint_resume_test).
-include_lib("eunit/include/eunit.hrl").

% Test checkpoint and resume functionality
checkpoint_resume_test() ->
    % Create workflow with checkpoints
    Spec = create_checkpoint_workflow(),
    {ok, Pid} = gen_pnet:start_link(Spec, #{}, []),

    % Execute to first checkpoint
    {ok, Receipt1} = gen_pnet:step(Pid),
    {ok, State1} = gen_pnet:state(Pid),

    % Save state
    SavedState = term_to_binary(State1),

    % Continue workflow
    {ok, Receipt2} = gen_pnet:step(Pid),

    % Stop and restart from checkpoint
    gen_pnet:stop(Pid),

    {ok, Pid2} = gen_pnet:start_link(Spec, #{resume => binary_to_term(SavedState)}, []),
    {ok, Receipt3} = gen_pnet:step(Pid2).

% Test state recovery
state_recovery_test() ->
    % Create workflow
    Spec = create_checkpoint_workflow(),
    {ok, Pid} = gen_pnet:start_link(Spec, #{}, []),

    % Execute steps
    {ok, _} = gen_pnet:step(Pid),
    {ok, _} = gen_pnet:step(Pid),

    % Get state
    {ok, State} = gen_pnet:state(Pid),

    % Verify state is recoverable
    ?assert(is_binary(term_to_binary(State))).
```

#### `wf_receipt_test.erl`

```erlang
-module(wf_receipt_test).
-include_lib("eunit/include/eunit.hrl").

% Test receipt generation and tracking
receipt_generation_test() ->
    Spec = create_simple_spec(),
    {ok, Pid} = gen_pnet:start_link(Spec, #{}, []),

    {ok, Receipt} = gen_pnet:step(Pid),

    % Verify receipt structure
    ?assert(is_record(Receipt, receipt)),
    ?assert(is_binary(Receipt#receipt.before_hash)),
    ?assert(is_binary(Receipt#receipt.after_hash)).

receipt_chain_test() ->
    Spec = create_multi_step_spec(),
    {ok, Pid} = gen_pnet:start_link(Spec, #{}, []),

    % Generate multiple receipts
    {ok, Receipt1} = gen_pnet:step(Pid),
    {ok, Receipt2} = gen_pnet:step(Pid),
    {ok, Receipt3} = gen_pnet:step(Pid),

    % Verify chain integrity
    ?assertEqual(Receipt1#receipt.after_hash, Receipt2#receipt.before_hash),
    ?assertEqual(Receipt2#receipt.after_hash, Receipt3#receipt.before_hash).
```

### Testing Approach for Slice 8-10

- **Property-Based Testing**: Use PropEr for edge cases
- **Integration Testing**: Test complex interactions
- **Recovery Testing**: Test failure and recovery scenarios
- **Performance Testing**: Test under load and stress

## Slice 11-13: Performance and Scalability

### Files in Slice 11-13

```
test/
├── yawl_performance_SUITE.erl            # Performance benchmarks
├── wf_pool_transaction_test.erl         # Pool transaction performance
└── wf_ops_performance_SUITE.erl        # Process monitoring performance
```

### Test Coverage by Module

#### `yawl_performance_SUITE.erl`

```erlang
-module(yawl_performance_SUITE).
-include_lib("common_test/include/ct.hrl").

% Benchmark workflow throughput
throughput_benchmark_test(_Config) ->
    Sizes = [10, 50, 100, 500],
    Results = lists:map(fun(Size) ->
        {ok, Pid} = create_workflowOfSize(Size),
        StartTime = os:timestamp(),

        execute_workflow(Pid),
        EndTime = os:timestamp(),

        gen_pnet:stop(Pid),

        Duration = timer:now_diff(EndTime, StartTime) / 1000,
        {Size, Duration}
    end, Sizes),

    ct:pal("Performance results: ~p", [Results]).

% Test memory usage
memory_usage_test(_Config) ->
    Before = memory_usage(),

    % Create many workflows
    {ok, Pids} = create_many_workflows(1000),
    execute_all_workflows(Pids),

    After = memory_usage(),
    Delta = After - Before,

    ct:pal("Memory usage: ~p bytes", [Delta]),
    ?assert(Delta < 10000000).  % Less than 10MB overhead
```

### Testing Approach for Slice 11-13

- **Benchmarking**: Measure performance characteristics
- **Load Testing**: Test under high load
- **Scalability**: Test scaling limits
- **Memory Profiling**: Monitor memory usage

## Test File Structure

### Directory Layout

```
test/
├── fixtures/                    # Test data fixtures
│   ├── orders/
│   ├── customers/
│   └── inventory/
├── unit/                       # Unit tests (EUnit)
│   ├── yawl_of_helpers_test.erl
│   ├── pnet_types_test.erl
│   └── ...
├── integration/                # Integration tests (Common Test)
│   ├── yawl_engine_SUITE.erl
│   ├── orderfulfillment_SUITE.erl
│   └── ...
├── property/                   # Property tests (PropEr)
│   ├── wf_prop_test.erl
│   └── ...
├── performance/                # Performance tests
│   ├── yawl_performance_SUITE.erl
│   └── ...
└── common/                    # Common test utilities
    ├── test_helpers.erl
    └── assertions.erl
```

### Test Naming Convention

- **Unit Tests**: `*_test.erl` (EUnit)
- **Integration Tests**: `*_SUITE.erl` (Common Test)
- **Property Tests**: `*_property.erl` (PropEr)
- **Performance Tests**: `*_benchmark.erl` or `*_performance_SUITE.erl`

## Test Dependencies

### Required Applications

The test suite depends on the following applications:

```erlang
{deps, [
    {rebar3, "3.24.0"},
    {eunit, "2.3.6"},
    {proper, "1.4"},
    {meck, "0.9.2"},
    {jsx, "3.1.0"}
]}.
```

### Test Configuration

```er
% test/test.config
{logdir, "_build/test/logs"}.
{cover, true}.
{cover_spec, "test/cover.spec"}.
{cover_enabled, true}.
{cover_opts, [verbose]}.
```

## Test Coverage Analysis

### Coverage Reports

Coverage reports are generated in `_build/test/cover/` and include:

1. **Module Coverage**: Coverage percentage for each module
2. **Function Coverage**: Coverage percentage for each function
3. **Line Coverage**: Detailed line-by-line coverage
4. **Branch Coverage**: Coverage decision branches

### Coverage Targets

| Module Type | Coverage Target | Notes |
|-------------|----------------|-------|
| Core Modules | 100% | Critical path coverage |
| Utility Modules | 95% | High reliability required |
| Workflow Modules | 90% | Comprehensive testing |
| Integration Tests | 85% | Edge case coverage |
| Performance Tests | Baseline | Performance characterization |

### Coverage Analysis Script

```bash
#!/bin/bash
# scripts/coverage_analysis.sh

# Generate coverage report
rebar3 cover --export

# Parse coverage results
echo "=== Coverage Analysis ==="
cat _build/test/cover/index.html | grep -E 'coverage|line' | head -10

# Check specific modules
echo "=== Module Coverage ==="
modules=("pnet_types" "pnet_marking" "gen_pnet")
for module in "${modules[@]}"; do
    coverage=$(cat _build/test/cover/index.html | grep -A5 $module | grep coverage | cut -d'"' -f4)
    echo "$module: $coverage%"
done
```

## Test Data Management

### Test Data Generators

#### `test_data_generator.erl`

```erlang
-module(test_data_generator).

% Generate test orders
-spec generate_order(integer()) -> map().
generate_order(OrderId) ->
    Items = generate_items(random:uniform(5)),
    #{order_id => integer_to_binary(OrderId),
      customer_id => generate_customer_id(),
      items => Items,
      total_amount => calculate_total(Items)}.

% Generate test customers
-spec generate_customer_id() -> binary().
generate_customer_id() ->
    <<"customer", (integer_to_binary(random:uniform(10000)))/binary>>.
```

### Test Fixtures

#### JSON Fixtures

```json
// test/fixtures/orders/simple_order.json
{
    "order_id": "12345",
    "customer_id": "customer001",
    "items": [
        {"sku": "SKU001", "quantity": 2, "price": 10.00}
    ],
    "total_amount": 20.00
}
```

#### Erlang Term Fixtures

```erlang
% test/fixtures/customer_data.erl
-define(CUSTOMER_FIXTURES, [
    #{
        id => <<"customer001">>,
        name => "John Doe",
        email => "john@example.com",
        status => active
    },
    #{
        id => <<"customer002">>,
        name => "Jane Smith",
        email => "jane@example.com",
        status => banned
    }
]).
```

## Test Execution Strategies

### Running Specific Test Slices

```bash
# Run Slice 0-3 (Basic)
rebar3 ct -c test/pnet_types_SUITE.erl
rebar3 ct -c test/pnet_marking_SUITE.erl
rebar3 ct -c test/gen_pnet_SUITE.erl

# Run Slice 4-7 (YAWL Patterns)
rebar3 ct -c test/yawl_validate_SUITE.erl
rebar3 ct -c test/yawl_compile_SUITE.erl
rebar3 ct -c test/orderfulfillment_integration_SUITE.erl

# Run Slice 8-10 (Advanced Features)
rebar3 ct -c test/wf_checkpoint_resume_test.erl
rebar3 ct -c test/wf_audit_log_integration_test.erl

# Run Slice 11-13 (Performance)
rebar3 ct -c test/yawl_performance_SUITE.erl
```

### Parallel Test Execution

```bash
# Run tests in parallel
rebar3 ct -j 4  # Use 4 parallel workers

# Run with parallel directory
rebar3 ct -d test/integration -j 4
```

## Continuous Integration

### GitHub Actions Configuration

```yaml
# .github/workflows/test.yml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        slice: [0-3, 4-7, 8-10, 11-13]

    steps:
      - uses: actions/checkout@v2

      - name: Run Slice Tests
        run: |
          case ${{ matrix.slice }} in
            0-3) rebar3 ct -c test/pnet_SUITE.erl ;;
            4-7) rebar3 ct -c test/yawl_compile_SUITE.erl ;;
            8-10) rebar3 ct -c test/wf_checkpoint_resume_test.erl ;;
            11-13) rebar3 ct -c test/yawl_performance_SUITE.erl ;;
          esac

      - name: Upload Coverage
        uses: codecov/codecov-action@v1
```

## Test Maintenance

### Adding New Tests

1. **Determine Slice**: Identify which slice the new functionality belongs to
2. **Create Test File**: Follow naming conventions
3. **Write Test Cases**: Ensure comprehensive coverage
4. **Add to CI**: Update CI configuration
5. **Update Documentation**: Update this document

### Test Refactoring

1. **Identify Duplicated Code**: Look for common patterns
2. **Extract Test Helpers**: Create reusable test utilities
3. **Improve Test Organization**: Reorganize if needed
4. **Update Coverage**: Ensure refactored tests maintain coverage

### Test Maintenance Checklist

- [ ] Update test data for new requirements
- [ ] Add edge cases for new features
- [ ] Update performance baselines
- [ ] Review and update test documentation
- [ ] Update CI/CD pipelines
- [ ] Verify all tests still pass

## Test Metrics and Reporting

### Key Metrics

1. **Test Coverage**: Overall and by module
2. **Test Execution Time**: Speed of test runs
3. **Failure Rate**: Percentage of failing tests
4. **Performance Benchmarks**: Throughput and latency
5. **Code Coverage**: Line, branch, and function coverage

### Reporting Tools

```bash
# Generate test report
rebar3 ct --verbose

# Generate coverage report
rebar3 cover --export

# Generate performance report
rebar3 ct -c test/yawl_performance_SUITE --logdir test/logs
```

This comprehensive test organization reference ensures that the YAWL workflow engine is thoroughly tested across all components, with clear structure and maintainable test organization.