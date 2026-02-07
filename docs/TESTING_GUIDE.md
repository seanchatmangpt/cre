# Testing Guide

**Comprehensive Testing Strategy for YAWL Workflow Engine**

## Overview

This guide provides a comprehensive testing strategy for the CRE YAWL workflow engine, covering unit testing, integration testing, property-based testing, and performance testing. The testing infrastructure follows Chicago School TDD principles with comprehensive coverage requirements.

## Testing Philosophy

### Chicago School TDD

All testing follows the Chicago School TDD approach:
1. **Test First**: Write tests before implementation
2. **Red-Green-Refactor**: Red → Green → Refactor cycle
3. **One Assert**: One test case = one assertion
4. **Fast Tests**: All tests should run in seconds
5. **Independent Tests**: Tests don't depend on each other

### Coverage Requirements

- **Unit Tests**: 100% coverage for all utility modules
- **Integration Tests**: 90% coverage for workflow patterns
- **Property Tests**: Cover all edge cases and invariants
- **Performance Tests**: Baseline established for all critical paths

## Test Organization

### Test File Naming Convention

- **Unit Tests**: `*_test.erl` (EUnit)
- **Integration Tests**: `*_SUITE.erl` (Common Test)
- **Property Tests**: `*_property.erl` (PropEr)
- **Performance Tests**: `*_benchmark.erl` (Common Test)

### Test Structure by "Slice"

The test suite is organized by "slice" to map functionality to test coverage:

| Slice | Functionality | Test Type | Coverage Target |
|-------|--------------|-----------|----------------|
| 0-3 | Basic Petri net operations | Unit Tests | 100% |
| 4-7 | YAWL pattern implementations | Integration Tests | 90% |
| 8-10 | Advanced workflow features | Property Tests | 85% |
| 11-13 | Performance and scalability | Benchmark Tests | Baseline |

## Running Tests

### Unit Tests (EUnit)

```bash
# Run all EUnit tests
rebar3 eunit

# Run specific test module
rebar3 eunit -m yawl_of_helpers_test

# Run with verbose output
rebar3 eunit -v

# Run with coverage
rebar3 cover --export
```

### Integration Tests (Common Test)

```bash
# Run all Common Test suites
rebar3 ct

# Run specific test suite
rebar3 ct -c test/yawl_engine_SUITE

# Run with verbose output
rebar3 ct -v

# Run with coverage
rebar3 cover --export
```

### Property-Based Testing (PropEr)

```bash
# Run property tests
rebar3 eunit -m wf_prop_test

# Run property tests with verbose output
rebar3 eunit -v -m wf_prop_test

# Generate test cases
cd src && erl -pa ../_build/test/lib/*/ebin
1> wf_prop:generate_test_cases().
```

### Performance Tests

```bash
# Run performance benchmarks
rebar3 ct -c test/yawl_performance_SUITE

# Generate performance report
rebar3 cover --verbose

# Profile specific operation
rebar3 ct -c test/yawl_performance_SUITE -d performance
```

## Test Development

### Writing Unit Tests

#### Basic Unit Test Template

```erlang
-module(yawl_of_helpers_tests).
-include_lib("eunit/include/eunit.hrl").

% Setup function
setup() ->
    % Initialize test environment
    meck:new(customer_db, [passthrough]),
    meck:new(inventory_db, [passthrough]),
    meck:new(payment_gateway, [passthrough]),
    ok.

% Cleanup function
cleanup() ->
    % Clean up mocks
    meck:unload(),
    ok.

% Test with setup/teardown
handle_order_received_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/0,
        fun(_) ->
            Context = #{items => [#{sku => "SKU001", quantity => 2}]},
            UsrInfo = #{user_id => "user123"},

            {ok, NewContext} = yawl_of_helpers:handle_order_received(Context, UsrInfo),

            ?assert(maps:is_key(order_id, NewContext)),
            ?assert(maps:is_key(order_state, NewContext))
        end
    }.

% Simple inline test
simple_test() ->
    ?assert(true).
```

#### Testing with Mocks

```erlang
% Mock external dependencies
customer_exists_test_() ->
    [{setup,
        fun setup/0,
        fun cleanup/0,
        fun(_) ->
            % Mock customer exists
            meck:expect(customer_db, find, fun("customer123") -> {ok, #{}} end),

            Result = yawl_of_helpers:customer_exists("customer123"),
            ?assert(Result)
        end
    }].

% Mock failure case
customer_not_found_test_() ->
    [{setup,
        fun setup/0,
        fun cleanup/0,
        fun(_) ->
            % Mock customer not found
            meck:expect(customer_db, find, fun("invalid") -> {error, not_found} end),

            Result = yawl_of_helpers:customer_exists("invalid"),
            ?assert(not Result)
        end
    }].
```

### Writing Integration Tests

#### Integration Test Template

```erlang
-module(orderfulfillment_integration_SUITE).
-include_lib("common_test/include/ct.hrl").

% Suite configuration
init_per_suite(Config) ->
    % Start test applications
    {ok, _} = application:ensure_all_started(cre),
    Config.

end_per_suite(_Config) ->
    % Stop applications
    application:stop(cre),
    ok.

init_per_testcase(TestCase, Config) ->
    % Setup for each test case
    meck:new(customer_db, [passthrough]),
    meck:new(payment_gateway, [passthrough]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    % Cleanup for each test case
    meck:unload(),
    ok.

% Test case
order_fulfillment_test(_Config) ->
    % Setup test data
    OrderSpec = wf_spec:create_order_fulfillment(),
    {ok, OrderNet} = yawl_compile:compile(OrderSpec, #{seed => 123}),

    % Start workflow
    {ok, Pid} = gen_pnet:start_link(OrderNet, #{order_id => "12345"}, []),

    % Execute workflow steps
    {ok, Receipt1} = gen_pnet:step(Pid),  % Process order
    {ok, Receipt2} = gen_pnet:step(Pid),  % Check inventory
    {ok, Receipt3} = gen_pnet:step(Pid),  % Ship order

    % Verify final state
    {ok, FinalMarking} = gen_pnet:marking(Pid),
    ?assert(maps:is_key(output, FinalMarking)),

    % Cleanup
    gen_pnet:stop(Pid).
```

### Writing Property Tests

#### Property Test Template

```erlang
-module(yawl_property_tests).
-include_lib("proper/include/proper.hrl").

% Property: Order processing should be idempotent
prop_order_processing_idempotent() ->
    ?FORALL(OrderData, order_data(),
        begin
            % First processing
            Result1 = process_order(OrderData),
            % Second processing with same data
            Result2 = process_order(OrderData),
            % Results should be equivalent
            equivalent_results(Result1, Result2)
        end).

% Property: Inventory check should be consistent
prop_inventory_consistency() ->
    ?FORALL(Items, list(item()),
        begin
            InitialInventory = get_inventory_state(),
            {Result, FinalInventory} = check_inventory(Items),
            % Inventory changes should be consistent with result
            case Result of
                sufficient -> FinalInventory =< InitialInventory;
                insufficient -> FinalInventory > InitialInventory
            end
        end).

% Helper generators
order_data() ->
    ?LET({Items, CustomerId},
        {list(item()), binary()},
        #{items => Items, customer_id => CustomerId}
    ).

item() ->
    ?LET({SKU, Quantity},
        {binary(), pos_integer()},
        #{sku => SKU, quantity => Quantity}
    ).

% Test runner
run_property_tests() ->
    proper:unit(yawl_property_tests).
```

## Test Utilities

### Test Helper Modules

#### `wf_test_helpers.erl`

```erlang
-module(wf_test_helpers).

% Create test workflow specification
-spec create_test_workflow(task_config()) -> yawl_spec().
create_test_workflow(Config) ->
    Tasks = maps:get(tasks, Config, []),
    Flows = maps:get(flows, Config, []),

    #yawl_spec{
        name = <<"TestWorkflow">>,
        tasks = Tasks,
        flows = Flows,
        conditions = [],
        decomposition = #decomposition{}
    }.

% Mock external services
-spec mock_service(service(), term()) -> ok.
mock_service(Service, Response) ->
    meck:new(Service, [passthrough]),
    meck:expect(Service, handle_request, fun(_) -> Response end).

% Verify service calls
-spec verify_service_calls(service(), integer()) -> ok.
verify_service_calls(Service, ExpectedCalls) ->
    ?assertEqual(ExpectedCalls, meck:num_calls(Service, handle_request, '_')).
```

#### `test_assertions.erl`

```erlang
-module(test_assertions).

% Custom assertions for workflow testing
-assert_workflow_complete(Pid) ->
    {ok, Marking} = gen_pnet:marking(Pid),
    ?assertMatch(#{output := [_]}, Marking).

%assert_workflow_failed(Pid, ExpectedReason) ->
    {ok, State} = gen_pnet:state(Pid),
    ?assertEqual(failed, State#case_state.status),
    ?assertEqual(ExpectedReason, State#case_state.failure_reason).

% Check receipts for audit trail
-assert_receipt_count(Pid, ExpectedCount) ->
    Receipts = gen_pnet:receipts(Pid),
    ?assertEqual(ExpectedCount, length(Receipts)).

% Verify token movement
-assert_tokens_moved(Pid, Transition, ExpectedTokens) ->
    {ok, Receipts} = gen_pnet:receipts(Pid),
    lists:foreach(fun(Receipt) ->
        case Receipt#receipt.move of
            {Transition, Tokens} ->
                ?assertEqual(ExpectedTokens, Tokens);
            _ ->
                ok
        end
    end, Receipts).
```

## Performance Testing

### Benchmarking Strategy

#### Throughput Benchmarks

```erlang
-module(yawl_performance_SUITE).
-include_lib("common_test/include/ct.hrl").

% Benchmark workflow execution
throughput_benchmark(Config) ->
    % Setup
    TestSizes = [10, 50, 100, 500],
    Results = lists:map(fun(Size) ->
        Time = benchmark_size(Size),
        {Size, Time}
    end, TestSizes),

    % Generate report
    generate_throughput_report(Results).

benchmark_size(Size) ->
    % Create workflow with Size tasks
    Spec = create_large_workflow(Size),
    {ok, Pid} = gen_pnet:start_link(Spec, #{}, []),

    % Measure execution time
    Start = os:timestamp(),
    execute_workflow_steps(Pid),
    End = os:timestamp(),

    % Cleanup
    gen_pnet:stop(Pid),

    timer:now_diff(End, Start) / 1000.  % Convert to milliseconds
```

#### Memory Usage Analysis

```erlang
% Memory usage test
memory_usage_test(_Config) ->
    % Measure memory before
    Before = memory_usage(),

    % Create and execute workflows
    {ok, Pids} = create_many_workflows(100),
    execute_all_workflows(Pids),

    % Measure memory after
    After = memory_usage(),

    % Calculate difference
    Delta = After - Before,
    ?assert(Delta < 1000000),  % Less than 1MB per workflow
```

## Test Data Management

### Test Data Generators

#### `test_data.erl`

```erlang
-module(test_data).

% Generate realistic test orders
-spec generate_order(pos_integer()) -> map().
generate_order(OrderId) ->
    Items = generate_items(random:uniform(5)),
    CustomerId = generate_customer_id(),

    #{order_id => integer_to_binary(OrderId),
      customer_id => CustomerId,
      items => Items,
      total_amount => calculate_total(Items)}.

% Generate test items
-spec generate_items(pos_integer()) -> [map()].
generate_items(Count) ->
    generate_items(Count, 1, []).

generate_items(0, _, Acc) -> Acc;
generate_items(Count, Index, Acc) ->
    Item = #{
        sku => <<"SKU", (integer_to_binary(Index))/binary>>,
        quantity = random:uniform(10),
        price => random:uniform(100) + 1
    },
    generate_items(Count - 1, Index + 1, [Item | Acc]).
```

### Fixtures

#### Test Fixtures Directory

```
test/fixtures/
├── orders/
│   ├── simple_order.json
│   ├── complex_order.json
│   └── invalid_order.json
├── customers/
│   ├── valid_customer.json
│   └── banned_customer.json
└── inventory/
    ├── in_stock.json
    └── out_of_stock.json
```

## Test Coverage

### Coverage Analysis

```bash
# Generate coverage report
rebar3 cover --export

# View coverage in browser
open _build/test/cover/index.html

# Check specific module coverage
rebar3 cover --verbose --module yawl_of_helpers
```

### Coverage Requirements

- **Core Modules**: 100% coverage required
- **Utility Modules**: 95% coverage required
- **Workflow Modules**: 90% coverage required
- **Integration Tests**: 85% coverage required

## Debugging Tests

### Common Test Debugging

```bash
# Run with debug logging
rebar3 ct -v -d

# Run specific test with debug
rebar3 ct -c test/yawl_engine_SUITE -o case=order_fulfillment_test

# Generate test report
rebar3 ct --logdir test/logs
```

### EUnit Debugging

```bash
# Run with verbose output
rebar3 eunit -v

# Run specific test
rebar3 eunit -m yawl_of_helpers_test -v

# Interactive debugging
rebar3 shell
1> eunit:test(yawl_of_helpers_test).
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
        otp-version: [25.0, 26.0, 27.0, 28.0]
        erlang-version: [25.0, 26.0, 27.0, 28.0]

    steps:
      - uses: actions/checkout@v2

      - name: Setup Erlang
        uses: erlang-actions/setup-erlang@v1
        with:
          otp-version: ${{ matrix.otp-version }}
          erlang-version: ${{ matrix.erlang-version }}

      - name: Install dependencies
        run: rebar3 deps

      - name: Run tests
        run: |
          rebar3 eunit
          rebar3 ct
          rebar3 cover --export

      - name: Upload coverage
        uses: codecov/codecov-action@v1
```

### Quality Gates

```bash
# Pre-commit quality check
#!/bin/bash
# .git/hooks/pre-commit

rebar3 efmt -c
rebar3 dialyzer
rebar3 eunit
rebar3 ct
rebar3 cover --export

if [ $? -ne 0 ]; then
    echo "Quality gates failed. Please fix issues before committing."
    exit 1
fi
```

## Best Practices

### Test Writing Best Practices

1. **Test One Thing**: Each test should verify one specific behavior
2. **Use Descriptive Names**: Test names should clearly state what they test
3. **Setup/Teardown**: Always clean up after tests
4. **Mock External Dependencies**: Use meck to isolate tests
5. **Test Edge Cases**: Include boundary conditions and error cases
6. **Performance Baselines**: Establish and maintain performance baselines

### Test Organization Best Practices

1. **Separate Concerns**: Unit tests, integration tests, and performance tests
2. **Modular Test Helpers**: Reusable test utilities
3. **Consistent Naming**: Follow established conventions
4. **Test Data Management**: Use fixtures and generators for test data
5. **Documentation**: Document test strategies and patterns

### CI/CD Integration Best Practices

1. **Fast Feedback**: Run tests on every commit
2. **Coverage Tracking**: Monitor coverage trends
3. **Performance Monitoring**: Track performance regression
4. **Quality Gates**: Block merges on test failures
5. **Parallel Execution**: Run tests in parallel for speed

## Troubleshooting

### Common Test Issues

1. **Test Dependencies**: Missing dependencies in test environment
2. **State Leakage**: Tests sharing state incorrectly
3. **Timeouts**: Flaky tests due to timeouts
4. **Resource Leaks**: Memory or file descriptor leaks
5. **Environment Issues**: Differences between test and production environments

### Debug Techniques

1. **Verbose Logging**: Enable debug logging for test runs
2. **Interactive Debugging**: Use rebar3 shell for manual testing
3. **Isolation**: Run tests individually to isolate failures
4. **Mock Verification**: Verify mock calls and expectations
5. **State Inspection**: Check workflow state at each step

## Test Examples

### Complete Workflow Test

```erlang
-module(orderfulfillment_complete_SUITE).
-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    % Start test applications
    {ok, _} = application:ensure_all_started(cre),
    meck:new(customer_db, [passthrough]),
    meck:new(payment_gateway, [passthrough]),
    meck:new(inventory_db, [passthrough]),
    Config.

end_per_suite(_Config) ->
    application:stop(cre),
    meck:unload(),
    ok.

complete_workflow_test(_Config) ->
    % Create workflow
    Spec = wf_spec:create_order_fulfillment(),
    {ok, OrderNet} = yawl_compile:compile(Spec),

    % Start workflow
    {ok, Pid} = gen_pnet:start_link(OrderNet, #{order_id => "12345"}, []),

    % Verify initial state
    {ok, InitialMarking} = gen_pnet:marking(Pid),
    ?assertMatch(#{input := [_]}, InitialMarking),

    % Execute workflow
    Steps = [process_order, check_inventory, ship_order],
    execute_workflow_steps(Pid, Steps),

    % Verify completion
    {ok, FinalMarking} = gen_pnet:marking(Pid),
    ?assertMatch(#{output := [_]}, FinalMarking),

    % Cleanup
    gen_pnet:stop(Pid).

execute_workflow_steps(Pid, Steps) ->
    execute_workflow_steps(Pid, Steps, 1).

execute_workflow_steps(_, [], _) ->
    ok;
execute_workflow_steps(Pid, [Step | Rest], Attempt) ->
    case gen_pnet:step(Pid) of
        {ok, Receipt} ->
            ct:pal("Step ~p completed: ~p", [Step, Receipt]),
            execute_workflow_steps(Pid, Rest, Attempt);
        abort when Attempt < 3 ->
            ct:pal("Retrying step ~p (attempt ~p)", [Step, Attempt]),
            timer:sleep(1000),
            execute_workflow_steps(Pid, [Step | Rest], Attempt + 1);
        abort ->
            ct:fail("Workflow failed at step ~p", [Step])
    end.
```

This comprehensive testing guide provides everything needed to implement a robust testing strategy for the YAWL workflow engine, ensuring high-quality, maintainable code with comprehensive coverage.