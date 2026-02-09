# CRE Mock Utilities

Comprehensive mocking utilities for CRE (Common Runtime Environment) testing.

## Overview

This directory provides mock factories and test utilities for generating realistic test data across all CRE subsystems:

- **Event logs** - Synthetic workflow event logs for process mining
- **Petri nets** - Mock net structures and markings
- **Workflows** - YAWL workflow specifications and tasks
- **Mnesia** - In-memory database helpers
- **Time** - Time manipulation for deterministic testing

## Modules

### event_log_mocks

Generate synthetic event logs for workflow mining and predictive analytics.

```erlang
%% Simple event log with 3 cases
Log = event_log_mocks:simple_log(),

%% Noisy log with malformed events (30% noise)
NoisyLog = event_log_mocks:noisy_log([{noise_level, 0.3}]),

%% Large log for performance testing (1000 events)
LargeLog = event_log_mocks:large_log(1000),

%% Log with various trace patterns
TraceLog = event_log_mocks:trace_log(),

%% Sequential trace
SeqLog = event_log_mocks:sequential_trace([{case_count, 5}]),

%% Parallel trace (concurrent branches)
ParLog = event_log_mocks:parallel_trace(),

%% Loop trace (repeating activities)
LoopLog = event_log_mocks:loop_trace(),
```

#### Event Structure

Events are maps with the following fields:

- `timestamp` - Millisecond timestamp (integer)
- `type` - Event type atom (case_created, workitem_started, etc.)
- `case_id` - Case identifier (binary)
- `data` - Additional event data (map, optional)

#### Utility Functions

```erlang
%% Validate log structure
{ok, ValidEvents} = event_log_mocks:validate_log(Log),

%% Count events by type
Count = event_log_mocks:count_events(Log, case_created),

%% Filter by case ID
CaseEvents = event_log_mocks:filter_by_case(Log, <<"case_001">>),

%% Sort by timestamp
SortedLog = event_log_mocks:sort_by_timestamp(Log),
```

### pnet_mocks

Generate Petri net structures for testing gen_pnet and gen_yawl patterns.

```erlang
%% Simple linear net
Net = pnet_mocks:simple_net(),

%% Parallel split net
ParNet = pnet_mocks:parallel_net(),

%% Loop net
LoopNet = pnet_mocks:loop_net(),

%% Custom net with options
CustomNet = pnet_mocks:mock_net([{places, [p1, p2, p3]}]),
```

#### Markings

```erlang
%% Initial empty marking
Marking = pnet_mocks:initial_marking([p1, p2, p3]),

%% Marking with tokens
TokenMarking = pnet_mocks:marking_with_tokens([p1, p2], #{p1 => 2, p2 => 0}),
%% => #{p1 => [token, token], p2 => []}
```

#### Net State

```erlang
%% Mock net state for gen_yawl
State = pnet_mocks:mock_net_state(),

%% With custom marking
State2 = pnet_mocks:net_state_with_marking(#{p_start => [start]}),

%% With custom user info
State3 = pnet_mocks:net_state_with_usr_info(#{my_data => value}),
```

#### Validation

```erlang
%% Validate Petri net structure
true = pnet_mocks:is_valid_net(Net),
```

### workflow_mocks

Generate YAWL workflow specifications and task definitions.

```erlang
%% Simple workflow
WF = workflow_mocks:simple_workflow(),

%% Complex workflow with patterns
ComplexWF = workflow_mocks:complex_workflow(),

%% Specific workflow patterns
ApprovalWF = workflow_mocks:approval_workflow(),
ParallelWF = workflow_mocks:parallel_workflow(),
LoopWF = workflow_mocks:loop_workflow(),
```

#### Tasks

```erlang
%% Atomic task
Task1 = workflow_mocks:atomic_task(<<"task1">>),

%% Composite (sub-workflow) task
CompTask = workflow_mocks:composite_task(<<"main">>),

%% Multi-instance task
MITask = workflow_mocks:multi_instance_task(<<"process">>, {2, unlimited}),
```

#### Flows and Conditions

```erlang
%% Simple flow
Flow = workflow_mocks:mock_flow(#{from => <<"t1">>, to => <<"t2">>}),

%% Conditional flow
CondFlow = workflow_mocks:conditional_flow(<<"check">>, <<"approve">>, <<"approved">>),

%% Input/output conditions
InputCond = workflow_mocks:input_condition(<<"start">>),
OutputCond = workflow_mocks:output_condition(<<"end">>),
```

#### YAWL Spec (wf_spec compatible)

```erlang
%% Generate wf_spec-compatible map
Spec = workflow_mocks:yawl_spec([{id, <<"my_wf">>}]),

%% From task list
Tasks = [workflow_mocks:atomic_task(<<"t1">>),
         workflow_mocks:atomic_task(<<"t2">>)],
Spec2 = workflow_mocks:spec_from_tasks(Tasks),
```

### mnesia_mocks

In-memory Mnesia database helpers for testing.

```erlang
%% Setup test database
ok = mnesia_mocks:setup_db(),

%% Initialize tables
ok = mnesia_mocks:init_tables([
    {users, [id, name, email]},
    {posts, [id, user_id, content]}
]),

%% ... run tests ...

%% Teardown
ok = mnesia_mocks:teardown_db(),
```

#### Mock Operations

```erlang
%% Write within transaction
ok = mnesia_mocks:mock_write(users, {users, 1, <<"Alice">>, <<"alice@example.com">>}),

%% Read within transaction
{ok, Record} = mnesia_mocks:mock_read(users, 1),

%% Delete within transaction
ok = mnesia_mocks:mock_delete(users, 1),

%% Fold over all records
{ok, Count} = mnesia_mocks:mock_fold(users, fun(_, Acc) -> Acc + 1 end, 0),
```

#### Dirty Operations (no transaction)

```erlang
%% Dirty write
ok = mnesia_mocks:dirty_write(users, Record),

%% Dirty read
{ok, Record} = mnesia_mocks:dirty_read(users, 1),

%% Dirty delete
ok = mnesia_mocks:dirty_delete(users, 1),
```

#### Utility Functions

```erlang
%% Check if table exists
true = mnesia_mocks:is_table(users),

%% Get table info
{ok, Attrs} = mnesia_mocks:table_info(users, attributes),

%% Get all keys
{ok, Keys} = mnesia_mocks:all_keys(users),

%% Select by pattern
{ok, Results} = mnesia_mocks:select(users, {users, '_', '_', '_'}),
```

### time_mocks

Time manipulation for deterministic testing.

```erlang
%% Freeze time at current moment
ok = time_mocks:freeze_time(),

%% Freeze at specific timestamp (milliseconds since epoch)
ok = time_mocks:freeze_time(1704067200000),  %% 2024-01-01 00:00:00 UTC

%% Get frozen timestamp
Ts = time_mocks:mock_timestamp(),  %% => 1704067200000

%% Advance frozen time
ok = time_mocks:advance_time(1000),           %% +1 second
ok = time_mocks:advance_time(minute, 5),      %% +5 minutes

%% Unfreeze (return to normal system time)
ok = time_mocks:unfreeze_time(),
```

#### Timestamp Generation

```erlang
%% Current (possibly frozen) timestamp
Now = time_mocks:mock_timestamp(),

%% Offset from frozen time
Future = time_mocks:mock_timestamp(3600000),  %% +1 hour

%% Current datetime
{{Year, Month, Day}, {Hour, Min, Sec}} = time_mocks:mock_datetime(),
```

#### Time Conversion

```erlang
%% Milliseconds to datetime
DT = time_mocks:millis_to_datetime(1704067200000),
%% => {{2024, 1, 1}, {0, 0, 0}}

%% Datetime to milliseconds
Ms = time_mocks:datetime_to_millis({{2024, 1, 1}, {0, 0, 0}}),
%% => 1704067200000

%% Add/subtract milliseconds
Later = time_mocks:add_millis(1000, 500),      %% => 1500
Earlier = time_mocks:subtract_millis(1000, 500), %% => 500
```

#### Test Helpers

```erlang
%% Wait until predicate is true
ok = time_mocks:wait_until(fun() -> some_condition() end, 5000),

%% Wait with custom check interval
ok = time_mocks:wait_until(fun() -> ready() end, 5000, 100),

%% Mock sleep that respects frozen time
ok = time_mocks:sleep_mock(1000),  %% advances frozen time by 1000ms
```

## Running Tests

Run all mock utility tests:

```bash
rebar3 eunit --module=mocks_tests
```

Run tests for individual mock modules:

```bash
rebar3 eunit --module=event_log_mocks
rebar3 eunit --module=pnet_mocks
rebar3 eunit --module=workflow_mocks
rebar3 eunit --module=mnesia_mocks
rebar3 eunit --module=time_mocks
```

## Usage with Meck

These mock utilities are designed to work well with Meck for module mocking:

```erlang
%% Mock a function to return synthetic event log
meck:new(event_log_mocks, [passthrough]),
meck:expect(event_log_mocks, simple_log, fun() ->
    event_log_mocks:simple_log([{case_count, 10}])
end),

%% ... run tests using the mocked function ...

meck:unload(event_log_mocks),
```

## Best Practices

1. **Always teardown** - Use `teardown_db()` after `setup_db()`, and `unfreeze_time()` after `freeze_time()`
2. **Validate logs** - Use `validate_log/1` to ensure generated logs are well-formed
3. **Use time mocks** - Freeze time for deterministic tests involving timestamps
4. **In-memory Mnesia** - Use `setup_db()` instead of real Mnesia in tests
5. **Start simple** - Use `simple_workflow()`, `simple_net()` etc. for basic tests

## See Also

- `src/wf/wf_xes.erl` - Event log to XES conversion
- `src/wf/wf_spec.erl` - YAWL specification parser
- `src/core/gen_yawl.erl` - YAWL workflow runtime
- `test/mocks/*.erl` - Additional mock modules
