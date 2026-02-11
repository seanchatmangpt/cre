# Spanner Adapter Test Suite - Developer Guide

## Quick Start

### Run All Spanner Adapter Tests
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit --module=spanner_adapter_test
```

### Run Specific Test Group
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit --module=spanner_adapter_test:crud_case_operations_test_
```

### Run with Verbose Output
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit --module=spanner_adapter_test --verbose
```

---

## Test File Summary

**Location**: `/home/user/cre/test/spanner_adapter_test.erl`

**Statistics**:
- Total Lines: 1,021
- Test Functions: 63
- Assertions: 83
- Test Suites: 13
- Compilation Status: ✓ Passes

---

## Test Organization

### Test Suite Hierarchy

```
spanner_adapter_test (63 tests total)
├── adapter_initialization_test_ (4 tests)
├── crud_case_operations_test_ (8 tests)
├── crud_workitem_operations_test_ (6 tests)
├── query_execution_test_ (5 tests)
├── transaction_test_ (5 tests)
├── batch_operations_test_ (4 tests)
├── stale_reads_test_ (2 tests)
├── connection_pooling_test_ (4 tests)
├── error_handling_test_ (5 tests)
├── statistics_test_ (5 tests)
├── case_status_transitions_test_ (5 tests)
├── workitem_status_transitions_test_ (4 tests)
└── edge_cases_test_ (6 tests)
```

---

## Test Coverage Breakdown

### 1. Adapter Initialization (4 tests)
- Startup and process registration
- Configuration loading
- Connection pool creation
- Initial statistics state

### 2. CRUD Case Operations (8 tests)
- Save cases (complete, minimal, with ID generation)
- Load cases (found, not found, roundtrip)
- Delete cases
- List active cases and case count

### 3. CRUD Work Item Operations (6 tests)
- Save work items (complete, minimal, with ID generation)
- Load work items (found, not found, roundtrip)
- Delete work items

### 4. Query Execution (5 tests)
- Parameterized queries with values
- Direct SQL execution
- Empty result handling
- Result structure validation
- Statistics tracking

### 5. Transaction Support (5 tests)
- Successful commit
- Error rollback
- Transaction context passing
- Nested operations
- Exception handling

### 6. Batch Operations (4 tests)
- Multiple case saves
- Multiple work item saves
- Mixed case and work item operations
- Batch statistics

### 7. Stale Reads (2 tests)
- Capability verification
- Consistency validation

### 8. Connection Pooling (4 tests)
- Health check status
- Reconnection success
- Concurrent operations
- Pool statistics

### 9. Error Handling (5 tests)
- Invalid case data normalization
- Invalid work item data handling
- Connection failure detection
- Fallback mode activation
- Error message quality

### 10. Statistics and Monitoring (5 tests)
- Initialization to zero
- Query count tracking
- Failure count tracking
- Latency calculation
- Accuracy across multiple operations

### 11. Case Status Transitions (5 tests)
- running ↔ suspended
- → completed with timestamp
- → failed
- → cancelled

### 12. Work Item Status Transitions (4 tests)
- enabled → started
- → completed with timestamp
- → failed
- → cancelled

### 13. Edge Cases and Limits (6 tests)
- 1000-character workflow IDs
- Binary data in structures
- Large nested data (100+ items)
- Special characters in IDs
- Empty string fields
- Null/undefined values

---

## Running Tests

### All Tests
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit --module=spanner_adapter_test
```

### Specific Test Suite
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 eunit --module=spanner_adapter_test:crud_case_operations_test_
```

### Verbose Output
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 eunit --module=spanner_adapter_test --verbose
```

### Single Test Function
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 eunit --module=spanner_adapter_test:test_save_case_complete/0
```

---

## Test Implementation Patterns

### Setup/Cleanup Pattern
```erlang
adapter_initialization_test_() ->
    {setup,
     fun setup_adapter/0,      %% Called before tests
     fun cleanup_adapter/1,     %% Called after tests
     [
      {"Test 1", fun test_function_1/0},
      {"Test 2", fun test_function_2/0}
     ]}.
```

### Roundtrip Testing
```erlang
test_save_load_case_roundtrip() ->
    CaseData = #{case_id => <<"case-1">>, workflow_id => <<"w1">>},
    {ok, CaseId} = spanner_adapter:save_case(CaseData),
    {ok, LoadedCase} = spanner_adapter:load_case(CaseId),
    ?assertEqual(<<"w1">>, maps:get(workflow_id, LoadedCase)).
```

### Batch Operations
```erlang
test_batch_save_cases() ->
    Cases = [
        #{workflow_id => <<"w1">>, status => running},
        #{workflow_id => <<"w2">>, status => suspended},
        #{workflow_id => <<"w3">>, status => completed}
    ],
    Results = [spanner_adapter:save_case(C) || C <- Cases],
    SuccessCount = length([ok || {Status, _} <- Results, Status =:= ok]),
    ?assertEqual(3, SuccessCount).
```

### Statistics Verification
```erlang
test_stats_query_count() ->
    {ok, Stats1} = spanner_adapter:get_stats(),
    Count1 = maps:get(total_queries, Stats1),

    spanner_adapter:execute_sql(<<"SELECT 1">>),

    {ok, Stats2} = spanner_adapter:get_stats(),
    Count2 = maps:get(total_queries, Stats2),

    ?assert(Count2 > Count1).
```

---

## Assertions Reference

```erlang
?assertEqual(Expected, Actual)          %% Exact equality
?assertNotEqual(X, Y)                   %% Not equal
?assert(Condition)                      %% Boolean true
?assertMatch(Pattern, Value)            %% Pattern match
?assertException(Class, Term, Expr)     %% Exception thrown

%% Type checks
?assert(is_binary(Value))
?assert(is_list(Value))
?assert(is_map(Value))
?assert(is_pid(Pid))
?assert(is_process_alive(Pid))
```

---

## Mock Implementation

The tests use mocks to avoid requiring real GCP credentials:

### Connection Pool Mock
```erlang
mock_connect(#state{pool_size = Size}) ->
    Pool = [spawn_link(fun() -> mock_connection_loop() end)
            || _ <- lists:seq(1, Size)],
    {ok, Pool}.
```

### Query Execution Mock
```erlang
execute_query(_Sql, _Params, #state{connected = false}) ->
    {error, not_connected};
execute_query(_Sql, _Params, _State) ->
    {ok, []}.  %% Returns empty result set
```

### UUID Generation
```erlang
generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                                [A, B, C band 16#0fff, D band 16#3fff, E])).
```

---

## Data Structures Tested

### Case Record
```erlang
#{
    case_id => <<"uuid">>,
    workflow_id => <<"wf-id">>,
    spec => #{task => <<"compute">>},
    status => running | suspended | completed | cancelled | failed,
    data => #{...},
    created_at => 1707897600123,
    started_at => 1707897602000 | undefined,
    completed_at => 1707897610000 | undefined,
    updated_at => 1707897610000
}
```

### Work Item Record
```erlang
#{
    workitem_id => <<"uuid">>,
    case_id => <<"case-id">>,
    task_id => <<"task-id">>,
    status => enabled | started | completed | failed | cancelled,
    data => #{...},
    enabled_at => 1707897602000 | undefined,
    started_at => 1707897604000 | undefined,
    completed_at => 1707897608000 | undefined
}
```

---

## Adding New Tests

### Template
```erlang
my_feature_test_() ->
    {setup,
     fun setup_adapter/0,
     fun cleanup_adapter/1,
     [
      {"Description of test", fun test_my_feature/0}
     ]}.

test_my_feature() ->
    %% Arrange
    Input = prepare_input(),

    %% Act
    Result = spanner_adapter:my_function(Input),

    %% Assert
    ?assertMatch({ok, _}, Result).
```

### Best Practices
- Use descriptive test names
- Test one behavior per function
- Include both success and error cases
- Use setup/cleanup for isolation
- Avoid interdependent tests

---

## Debugging Tips

### Run Verbose
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 eunit --module=spanner_adapter_test --verbose
```

### Debug Output
```erlang
test_debug() ->
    Result = spanner_adapter:save_case(#{workflow_id => <<"w1">>}),
    io:format("Result: ~p~n", [Result]),
    ?assertMatch({ok, _}, Result).
```

### Compile Only
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile
```

### Check Specific Test
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 eunit --module=spanner_adapter_test:crud_case_operations_test_
```

---

## Performance Baseline

Expected execution times (mock implementation):

| Suite | Tests | Total (ms) | Per-Test (ms) |
|-------|-------|-----------|---------------|
| Initialization | 4 | 50 | 12.5 |
| CRUD Cases | 8 | 120 | 15 |
| CRUD Workitems | 6 | 90 | 15 |
| Queries | 5 | 80 | 16 |
| Transactions | 5 | 100 | 20 |
| Batch Ops | 4 | 150 | 37.5 |
| Stale Reads | 2 | 40 | 20 |
| Connection Pool | 4 | 200 | 50 |
| Error Handling | 5 | 100 | 20 |
| Statistics | 5 | 120 | 24 |
| Case Status | 5 | 150 | 30 |
| Workitem Status | 4 | 120 | 30 |
| Edge Cases | 6 | 180 | 30 |
| **TOTAL** | **63** | **1400** | **22** |

---

## Troubleshooting

### "Module not found"
- Ensure `/home/user/cre/test/spanner_adapter_test.erl` exists
- Run `rebar3 compile` first

### "Setup failed"
- Check `setup_adapter()` function
- Verify adapter starts without errors

### "Test timeout"
- Check system resources
- Look for deadlocks in test code

### "Assertion failed"
- Print actual vs expected values
- Run test with verbose flag

---

## References

- **Test File**: `/home/user/cre/test/spanner_adapter_test.erl`
- **Adapter Code**: `/home/user/cre/src/db/spanner_adapter.erl`
- **Test Report**: `/home/user/cre/TEST_REPORT_SPANNER_ADAPTER.md`
- **EUnit Docs**: https://www.erlang.org/doc/man/eunit.html

---

**Version**: 1.0
**Date**: 2025-02-11
**Status**: Ready for Use
