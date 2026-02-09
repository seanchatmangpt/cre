# Testing Guide

**Comprehensive Testing Documentation for CRE YAWL Workflow Engine**

## Table of Contents

- [Overview](#overview)
- [Running Tests](#running-tests)
- [Test Organization](#test-organization)
- [Test Fixtures and Setup](#test-fixtures-and-setup)
- [Mnesia Test Initialization](#mnesia-test-initialization)
- [Writing New Tests](#writing-new-tests)
- [Doctest Usage](#doctest-usage)
- [Test Coverage Reporting](#test-coverage-reporting)
- [Troubleshooting Test Failures](#troubleshooting-test-failures)

## Overview

The CRE project uses a comprehensive testing strategy combining:

- **EUnit** - Unit testing framework built into Erlang/OTP
- **Common Test** - Integration and system testing framework
- **Doctest** - Documentation-based testing using `rebar3 proper`
- **Concuerror** - Concurrency property-based testing (optional)

### Test Configuration

Test configuration is defined in `rebar.config`:

```erlang
{profiles, [
    {test, [
        {cover_enabled, false},
        {erl_opts, [debug_info, {doc, "excerpt"}, {d, 'TEST'}, {i, "src/wf"}]},
        {deps, [{meck, "0.9.2"}]}
    ]},
    {concuerror, [{deps, [{concuerror, "0.21.0"}]}]}
]}.

{eunit_tests, [{application, cre}]}.
{eunit_opts, [no_tty]}.
```

### Required Test Dependencies

- `eunit` - Built-in unit test framework
- `common_test` - Built-in integration test framework
- `meck` - Mocking library for tests
- `proper` - Property-based testing (optional, for Concuerror)

## Running Tests

### Running All Tests

```bash
# Run all EUnit tests
rebar3 eunit

# Run all Common Test suites
rebar3 ct

# Run both EUnit and Common Test
rebar3 ct && rebar3 eunit
```

### Running Specific Tests

```bash
# Run specific EUnit test module
rebar3 eunit -m yawl_engine_test

# Run specific Common Test suite
rebar3 ct --suite yawl_integration_SUITE

# Run specific test case within a suite
rebar3 ct --suite yawl_integration_SUITE --case workflow_lifecycle_test
```

### Running Tests with Verbose Output

```bash
# EUnit verbose
rebar3 eunit -v

# Common Test verbose
rebar3 ct -v

# Common Test with specific verbosity
rebar3 ct --verbosity 50
```

### Running Tests by Directory

```bash
# Run all tests in a directory
rebar3 ct --dir test/

# Run specific suite file
rebar3 ct -c test/yawl_performance_SUITE.erl
```

### Running Concurrency Tests

```bash
# Run Concuerror for concurrency testing
rebar3 as concuerror compile
rebar3 concuerror
```

## Test Organization

### File Naming Conventions

| Pattern | Framework | Purpose | Example |
|---------|-----------|---------|---------|
| `*_test.erl` | EUnit | Unit tests | `yawl_engine_test.erl` |
| `*_SUITE.erl` | Common Test | Integration tests | `yawl_integration_SUITE.erl` |
| `*_doctest.erl` | Doctest | Documentation tests | `wf_compound_doctests.erl` |

### Directory Structure

```
test/
|-- fixtures/                          # Test data and fixtures
|   |-- orderfulfillment_2_1.yawl     # YAWL specification for testing
|-- *_test.erl                         # EUnit unit tests (60+ files)
|-- *_SUITE.erl                        # Common Test suites (10+ files)
|-- *_doctest.erl                      # Doctest files (3 files)
|-- test_helper.erl                    # EUnit setup/teardown
|-- test_helper.hrl                    # Test records and macros
|-- logic_client.erl                   # Test client utilities
|-- logic_worker.erl                   # Test worker utilities
```

### Test Categories

#### Unit Tests (EUnit)

Unit tests focus on individual modules and functions. They are:
- Fast to run
- Isolated (no external dependencies)
- Use mocks for external services

Example unit test files:
- `yawl_engine_test.erl` - Engine core functionality
- `pnet_marking_test.erl` - Marking algebra
- `wf_receipt_test.erl` - Receipt tracking
- `yawl_auth_test.erl` - Authentication

#### Integration Tests (Common Test)

Integration tests verify component interactions:
- Full workflow execution
- Database operations
- HTTP endpoints
- Multi-process coordination

Example integration test suites:
- `yawl_integration_SUITE.erl` - End-to-end workflows
- `orderfulfillment_integration_SUITE.erl` - Real-world workflow
- `yawl_performance_SUITE.erl` - Performance benchmarks
- `cre_yawl_exception_SUITE.erl` - Exception handling

#### Doctests

Doctests verify code examples in documentation:
- `wf_compound_doctests.erl` - Compound pattern examples
- `orderfulfillment_2_1_doctest.erl` - Order fulfillment examples

## Test Fixtures and Setup

### EUnit Setup/Teardown

The `test_helper.erl` module provides common setup:

```erlang
%% test/test_helper.erl
-module(test_helper).
-export([setup/0, cleanup/1]).

setup() ->
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, cre}} -> ok;
        {error, Reason} ->
            io:format(standard_error, "Warning: cre app start failed: ~p~n", [Reason]),
            ok
    end.

cleanup(_) ->
    ok.
```

### Using EUnit Fixtures

```erlang
%% In your test module
my_test_() ->
    {setup,
     fun test_helper:setup/0,      % Setup function
     fun test_helper:cleanup/1,     % Cleanup function
     fun(_Context) ->
         [?_test(begin
                    %% Your test code here
                    ?assert(true)
                end)]
     end}.
```

### Common Test Setup

Common Test suites use callback functions:

```erlang
%% Common Test suite callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Suite-level setup
init_per_suite(Config) ->
    ct:pal("Starting test suite"),
    {ok, _} = application:ensure_all_started(cre),
    Config.

%% Suite-level cleanup
end_per_suite(_Config) ->
    application:stop(cre),
    ok.

%% Test case setup
init_per_testcase(TestCase, Config) ->
    ct:pal("Starting: ~p", [TestCase]),
    Config.

%% Test case cleanup
end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed: ~p", [TestCase]),
    ok.
```

## Mnesia Test Initialization

### Starting Mnesia for Tests

Many tests require Mnesia for persistence:

```erlang
%% Initialize Mnesia schema
init_mnesia() ->
    %% Stop any existing Mnesia
    mnesia:stop(),
    mnesia:delete_schema([node()]),

    %% Create schema with test directory
    MnesiaDir = "test_data/mnesia",
    filelib:ensure_dir(MnesiaDir),
    mnesia:create_schema([node()]),

    %% Start Mnesia with custom directory
    application:set_env(mnesia, dir, MnesiaDir),
    {ok, _} = mnesia:start(),

    %% Create tables
    {atomic, ok} = mnesia:create_table(
        workflow_cases,
        [{attributes, record_info(fields, workflow_case)},
         {disc_copies, [node()]}]
    ),

    %% Wait for tables
    mnesia:wait_for_tables([workflow_cases], 5000).
```

### Cleaning Up Mnesia

```erlang
%% Clean Mnesia after test
cleanup_mnesia(_Config) ->
    mnesia:clear_table(workflow_cases),
    ok.
```

### Using Mnesia in Tests

```erlang
mnesia_workflow_test(_Config) ->
    %% Start transaction
    {atomic, Result} = mnesia:transaction(fun() ->
        %% Write test data
        mnesia:write(#workflow_case{
            case_id = <<"test_case_1">>,
            spec_id = <<"test_spec">>,
            status = running,
            data = #{}
        }),

        %% Read it back
        [Case] = mnesia:read(workflow_cases, <<"test_case_1">>),
        Case
    end),

    %% Verify result
    ?assertEqual(<<"test_case_1">>, Result#workflow_case.case_id).
```

## Writing New Tests

### EUnit Test Template

```erlang
%% -*- erlang -*-
%% @doc My Module Tests
-module(my_module_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Initialize test environment
    ok.

cleanup(_State) ->
    %% Clean up after tests
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

%% @doc Test basic functionality
basic_functionality_test() ->
    Input = test_input,
    Expected = expected_output,
    Actual = my_module:process(Input),
    ?assertEqual(Expected, Actual).

%% @doc Test with setup/teardown
complex_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [?_test(begin
                    %% Test code here
                    ?assert(true)
                end)]
     end}.

%% @doc Test generator for multiple cases
multiple_cases_test_() ->
    Cases = [
        {input1, expected1},
        {input2, expected2},
        {input3, expected3}
    ],
    lists:map(fun({Input, Expected}) ->
        ?_test(begin
            Actual = my_module:process(Input),
            ?assertEqual(Expected, Actual)
        end)
    end, Cases).
```

### Common Test Suite Template

```erlang
%% -*- erlang -*-
%% @doc My Module Integration Tests
-module(my_module_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Exports
%%====================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_case_1/1,
    test_case_2/1
]).

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
        {group, test_group}
    ].

groups() ->
    [
        {test_group, [], [
            test_case_1,
            test_case_2
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting ~p", [?MODULE]),
    {ok, _} = application:ensure_all_started(cre),
    Config.

end_per_suite(_Config) ->
    application:stop(cre),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting: ~p", [TestCase]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_case_1(_Config) ->
    ct:pal("Test case 1"),
    ?assert(true).

test_case_2(_Config) ->
    ct:pal("Test case 2"),
    ?assertNot(false).
```

### Test Assertions

#### EUnit Assertions

```erlang
%% Equality
?assertEqual(Expected, Actual)

%% Match
?assertMatch(Pattern, Actual)

%% Boolean condition
?assert(Condition)
?assertNot(Condition)

%% Exception
?assertException(Class, Pattern, Fun)

%% Error
?error(BadExpression)

%% Fail immediately
?fail(Reason)
```

#### Common Test Assertions

```erlang
%% Use ct:pal for output
ct:pal("Value: ~p", [Value]).

%% Use EUnit macros (include eunit.hrl)
?assertEqual(Expected, Actual).

%% Direct exit on failure
case Actual of
    Expected -> ok;
    Other -> ct:fail("Expected ~p, got ~p", [Expected, Other])
end.
```

## Doctest Usage

### Creating Doctests

Doctests combine documentation with testing:

```erlang
%% @doc Demonstrates deterministic choice with same seed.
%% Returns: The same element from the list when using the same seed.
%% ```
%% Seed = {42, 12345, 54321},
%% List = [a, b, c, d],
%% {First1, _} = pnet_choice:make(Seed, List),
%% {First2, _} = pnet_choice:make(Seed, List),
%% First1 =:= First2.
%% '''
%% @end
```

### Running Doctests

Doctests run as part of the EUnit test suite:

```bash
# Run doctests
rebar3 eunit -m wf_compound_doctests
```

### Doctest Best Practices

1. **Keep examples simple** - Complex examples are hard to test
2. **Use deterministic data** - Avoid random values
3. **Show expected results** - Make output clear
4. **Test edge cases** - Include boundary conditions

## Test Coverage Reporting

### Generating Coverage Reports

```bash
# Generate coverage after running tests
rebar3 cover

# Export coverage to HTML
rebar3 cover --export

# Open coverage in browser
open _build/test/cover/index.html
```

### Coverage Configuration

Coverage is configured in `rebar.config`:

```erlang
{profiles, [
    {test, [
        {cover_enabled, false},  % Disabled during test, enabled manually
        {cover_opts, [verbose]}
    ]}
]}.
```

### Interpreting Coverage

Coverage reports show:
- **Line Coverage** - Percentage of lines executed
- **Function Coverage** - Percentage of functions called
- **Module Coverage** - Aggregate coverage per module

Target coverage levels:
- Core modules: 100%
- Utility modules: 95%
- Workflow modules: 90%

### Coverage by Module

After running coverage, view specific module coverage:

```bash
# View coverage for a specific module
rebar3 cover --module=yawl_engine
```

## Troubleshooting Test Failures

### Common Issues and Solutions

#### Issue: Tests Fail with "no match of right hand value"

**Problem**: Pattern match failure in assertions.

**Solution**:
```erlang
%% Instead of:
?assertEqual(expected, actual),

%% Debug with:
ct:pal("Expected: ~p", [expected]),
ct:pal("Actual: ~p", [actual]),
?assertEqual(expected, actual).
```

#### Issue: Mnesia Tables Not Found

**Problem**: Mnesia not initialized for tests.

**Solution**:
```erlang
%% Add to init_per_suite
init_per_suite(Config) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(your_table, []),
    Config.
```

#### Issue: Application Not Started

**Problem**: CRE application not available.

**Solution**:
```erlang
%% Ensure application starts in setup
setup() ->
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.
```

#### Issue: Test Isolation Problems

**Problem**: Tests sharing state.

**Solution**:
```erlang
%% Use init_per_testcase/end_per_testcase
init_per_testcase(_TestCase, Config) ->
    %% Create fresh state for each test
    {ok, Pid} = start_fresh_process(),
    [{pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(pid, Config),
    stop_process(Pid),
    ok.
```

#### Issue: Timeout in Common Test

**Problem**: Test takes too long.

**Solution**:
```erlang
%% Increase timeout for specific test case
{timeout, 30, test_case_name(_Config) ->
    %% Test code here
end}.
```

### Debugging Tests

#### Verbose Output

```bash
# Maximum verbosity
rebar3 ct -v --verbosity 100
```

#### Interactive Debugging

```bash
# Start shell with test code loaded
rebar3 shell --apps cre

# Run tests manually from shell
1> eunit:test(yawl_engine_test, [verbose]).
```

#### Tracing Function Calls

```erlang
%% Add to test to trace calls
dbg:tracer(),
dbg:p(all, c),
dbg:tp(yawl_engine, handle_call, x),
%% Run test
dbg:stop().
```

### Getting Help

When tests fail unexpectedly:

1. **Check logs**: `_build/test/logs/` for Common Test output
2. **Run single test**: Isolate the failing test
3. **Add debug output**: Use `ct:pal/2` or `io:format/2`
4. **Check dependencies**: Ensure all required apps are running
5. **Verify Mnesia**: Check tables are created and accessible

## Test Quick Reference

### EUnit Commands

| Command | Purpose |
|---------|---------|
| `rebar3 eunit` | Run all EUnit tests |
| `rebar3 eunit -v` | Verbose EUnit output |
| `rebar3 eunit -m MODULE` | Run specific module |
| `rebar3 eunit -f FUNCTION` | Run specific function |
| `rebar3 cover` | Generate coverage report |

### Common Test Commands

| Command | Purpose |
|---------|---------|
| `rebar3 ct` | Run all Common Test suites |
| `rebar3 ct -v` | Verbose output |
| `rebar3 ct --suite SUITE` | Run specific suite |
| `rebar3 ct --case CASE` | Run specific test case |
| `rebar3 ct --dir DIR` | Run all tests in directory |

### Test Files Reference

| File Type | Pattern | Example |
|-----------|---------|---------|
| Unit test | `*_test.erl` | `yawl_engine_test.erl` |
| Integration test | `*_SUITE.erl` | `yawl_integration_SUITE.erl` |
| Doctest | `*_doctest.erl` | `wf_compound_doctests.erl` |
| Helper | `test_helper.erl` | Shared utilities |
| Fixtures | `test/fixtures/*` | Test data files |

---

**Additional Documentation:**

- [TEST_ORGANIZATION.md](TEST_ORGANIZATION.md) - Detailed test structure
- [COVERAGE_REPORT.md](COVERAGE_REPORT.md) - Coverage analysis
- [TEST_STATUS.md](TEST_STATUS.md) - Current test status
