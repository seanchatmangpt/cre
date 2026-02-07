# WDP (Data Flow) Patterns Reference

This document provides detailed reference for the WDP (Workflow Data Patterns) - data flow patterns in YAWL. These patterns manage how data flows through workflow processes, including parameter passing, transformation, distribution, accumulation, and visibility control.

## Overview

The WDP patterns implement the **Workflow Data Patterns** from the workflow patterns catalog. These patterns complement the control flow patterns (WCP) by managing the data perspective of workflow execution.

### Pattern Categories

| Pattern ID | Name | Module | Purpose |
|-----------|------|--------|---------|
| WDP-01 | Parameter Passing | `param_pass` | Pass data between workflow activities |
| WDP-02 | Data Transformation | `data_transform` | Convert data between formats |
| WDP-03 | Data Distribution | `data_distribute` | Distribute data to multiple recipients |
| WDP-04 | Data Accumulation | `data_accumulate` | Aggregate data from multiple sources |
| WDP-05 | Data Visibility | `data_visibility` | Control data access and scope |

### Common Interface

All WDP patterns implement the `gen_yawl` behavior:

```erlang
-behaviour(gen_yawl).

% Petri Net Callbacks
-export([place_lst/0, trsn_lst/0, preset/1, init/1,
         init_marking/2, is_enabled/3, fire/3, trigger/3]).

% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
```

---

## WDP-01: Parameter Passing

**Module**: `param_pass`

**Purpose**: Enables data to be passed between workflow activities. Defines how input data is provided to tasks and how output data is made available to subsequent tasks.

### Use Cases

- Passing variables from one workflow task to another
- Providing configuration data to subprocesses
- Returning results from workflow completion
- Maintaining data context across workflow boundaries

### State Record

```erlang
-record(param_pass_state, {
    params       :: map(),      % Parameters to pass
    target       :: undefined | pid(),
    start_time   :: integer()   % Creation timestamp
}).
```

### API Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `new/1` | `(Params) -> State` | Create parameter passing state |
| `start/1` | `(Params) -> {ok, Pid}` | Start as gen_pnet process |
| `get_state/1` | `(Pid) -> {ok, State}` | Get current state |
| `pass/2` | `(Params, Target) -> {ok, Params}` | Pass parameters to target |

### Petri Net Structure

```
Places:
  p_start        - Start of parameter passing
  p_param_hold   - Holds parameters during transfer
  p_end          - End of the pattern

Transitions:
  t_pass         - Pass parameters to target
  t_finish       - Complete the pattern
```

### Diagram

```
    p_start
       |
    [t_pass]
       |
    p_param_hold
       |
  [t_finish]
       |
    p_end
```

### Example Usage

```erlang
% Create parameter passing state
Params = #{customer_id => 12345, order_total => 99.99},
State = param_pass:new(Params).

% Start the workflow
{ok, Pid} = param_pass:start(Params).

% Pass parameters to a target
{ok, PassedParams} = param_pass:pass(Params, self()),
% => {ok, #{customer_id => 12345, order_total => 99.99, target => <0.123.0>}}
```

### Petri Net Callbacks

```erlang
place_lst() -> [p_start, p_param_hold, p_end].

trsn_lst() -> [t_pass, t_finish].

preset(t_pass) -> [p_start];
preset(t_finish) -> [p_param_hold];
preset(_) -> [].

is_enabled(t_pass, #{p_start := [start]}, _UsrInfo) -> true;
is_enabled(t_finish, #{p_param_hold := [_]}, _UsrInfo) -> true;
is_enabled(_, _, _) -> false.
```

---

## WDP-02: Data Transformation

**Module**: `data_transform`

**Purpose**: Enables data to be transformed between different formats or representations as it flows through the workflow. Includes format conversion, data mapping, and value transformations.

### Use Cases

- Converting XML to JSON and vice versa
- Data normalization and validation
- Currency conversion
- Date/time format transformations
- Custom business logic transformations

### State Record

```erlang
-record(data_transform_state, {
    transform_fun :: function(),  % Transformation function
    input_data    :: term(),       % Input to transform
    output_data   :: undefined | term(),
    start_time    :: integer()
}).
```

### API Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `new/2` | `(Fun, Data) -> State` | Create transformation state |
| `start/2` | `(Fun, Data) -> {ok, Pid}` | Start as gen_pnet process |
| `get_state/1` | `(Pid) -> {ok, State}` | Get current state |
| `transform/2` | `(Fun, Data) -> {ok, Result} \| {error, Reason}` | Transform data |

### Petri Net Structure

```
Places:
  p_start         - Start of transformation
  p_transforming  - Data being transformed
  p_end           - End of the pattern

Transitions:
  t_transform     - Apply transformation function
  t_finish        - Complete the pattern
```

### Diagram

```
    p_start
       |
 [t_transform]
       |
  p_transforming
       |
  [t_finish]
       |
    p_end
```

### Example Usage

```erlang
% Define a transformation function
ToUpper = fun(String) -> string:uppercase(String) end.

% Transform data
{ok, Result} = data_transform:transform(ToUpper, "hello"),
% => {ok, "HELLO"}

% With a workflow process
{ok, Pid} = data_transform:start(ToUpper, "hello").
```

### Complex Transformations

```erlang
% Currency conversion
ToUSD = fun({Amount, EUR}) -> Amount * 1.10 end,
data_transform:transform(ToUSD, {100, EUR}),
% => {ok, 110.0}

% Data mapping
MapFields = fun(#{name := N, age := A}) -> #{username => N, years => A} end,
data_transform:transform(MapFields, #{name => "Alice", age => 30}),
% => {ok, #{username => "Alice", years => 30}}
```

### Petri Net Callbacks

```erlang
place_lst() -> [p_start, p_transforming, p_end].

trsn_lst() -> [t_transform, t_finish].

preset(t_transform) -> [p_start];
preset(t_finish) -> [p_transforming];
preset(_) -> [].
```

---

## WDP-03: Data Distribution

**Module**: `data_distribute`

**Purpose**: Enables data to be distributed to multiple recipients or targets in the workflow. Includes one-to-many distribution, broadcast, and selective routing.

### Use Cases

- Broadcasting notifications to multiple users
- Sending data to multiple processing nodes
- Fan-out message patterns
- Multi-channel publishing
- Parallel task dispatch with shared data

### State Record

```erlang
-record(data_distribute_state, {
    data              :: term(),
    targets           :: [term()],
    distributed_count :: non_neg_integer(),
    start_time        :: integer()
}).
```

### API Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `new/3` | `(Data, Targets, Count) -> State` | Create distribution state |
| `start/2` | `(Data, Targets) -> {ok, Pid}` | Start as gen_pnet process |
| `get_state/1` | `(Pid) -> {ok, State}` | Get current state |
| `distribute/3` | `(Data, Targets, Fun) -> {ok, Results}` | Distribute to all targets |

### Petri Net Structure

```
Places:
  p_start        - Start of distribution
  p_targets      - Targets to receive data
  p_distributed  - Data has been distributed
  p_end          - End of the pattern

Transitions:
  t_distribute   - Distribute data to targets
  t_finish       - Complete the pattern
```

### Diagram

```
      p_start
         |
   [t_distribute]
         |
    p_targets
         |
      [t_finish]
         |
       p_end
```

### Example Usage

```erlang
% Distribute data to multiple targets
Targets = [user1, user2, user3],
Data = #{message => "System maintenance at 10 PM"},

% Using a distribution function
SendFun = fun(Data, Target) ->
    {sent, Target, Data}
end,

{ok, Results} = data_distribute:distribute(Data, Targets, SendFun),
% => {ok, [{sent, user1, Data}, {sent, user2, Data}, {sent, user3, Data}]}

% Start as a workflow process
{ok, Pid} = data_distribute:start(Data, Targets).
```

### Distribution Patterns

```erlang
% Broadcast pattern
Broadcast = fun(Message, Target) ->
    notify_target(Target, Message)
end,
data_distribute:distribute(Message, AllUsers, Broadcast).

% Selective routing (filter targets first)
FilteredTargets = [T || T <- AllTargets, is_eligible(T)],
data_transform:distribute(Data, FilteredTargets, DistributeFun).
```

### Petri Net Callbacks

```erlang
place_lst() -> [p_start, p_targets, p_distributed, p_end].

trsn_lst() -> [t_distribute, t_finish].

preset(t_distribute) -> [p_start];
preset(t_finish) -> [p_distributed];
preset(_) -> [].
```

---

## WDP-04: Data Accumulation

**Module**: `data_accumulate`

**Purpose**: Enables data from multiple sources to be collected and aggregated into a single result. Includes many-to-one accumulation, reduction, and aggregation operations.

### Use Cases

- Aggregating results from parallel tasks
- Computing sums/averages across multiple values
- Collecting responses from multiple services
- Building composite data structures
- Reducing collections to single values

### State Record

```erlang
-record(data_accumulate_state, {
    sources          :: [term()],
    accumulator_fun  :: function(),
    collected_data   :: [term()],
    source_count     :: pos_integer(),
    start_time       :: integer()
}).
```

### API Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `new/3` | `(Sources, Fun, Count) -> State` | Create accumulation state |
| `start/2` | `(Sources, Fun) -> {ok, Pid}` | Start as gen_pnet process |
| `get_state/1` | `(Pid) -> {ok, State}` | Get current state |
| `accumulate/3` | `(Sources, Fun, Initial) -> {ok, Result}` | Accumulate data |

### Petri Net Structure

```
Places:
  p_start        - Start of accumulation
  p_collecting   - Collecting data from sources
  p_accumulated  - Data has been accumulated
  p_end          - End of the pattern

Transitions:
  t_accumulate   - Accumulate data from sources
  t_finish       - Complete the pattern
```

### Diagram

```
      p_start
         |
  [t_accumulate]
         |
   p_collecting
   p_accumulated
         |
      [t_finish]
         |
       p_end
```

### Example Usage

```erlang
% Sum accumulation
SumFun = fun(Acc, X) -> Acc + X end,
{ok, Sum} = data_accumulate:accumulate([1, 2, 3, 4, 5], SumFun, 0),
% => {ok, 15}

% Product accumulation
ProdFun = fun(Acc, X) -> Acc * X end,
{ok, Product} = data_accumulate:accumulate([2, 3, 4], ProdFun, 1),
% => {ok, 24}

% List building
CollectFun = fun(Acc, X) -> Acc ++ [X * 2] end,
{ok, Doubled} = data_accumulate:accumulate([1, 2, 3], CollectFun, []),
% => {ok, [2, 4, 6]}
```

### Advanced Aggregations

```erlang
% Average calculation
AvgAccumulator = fun
    ({Sum, Count}, X) -> {Sum + X, Count + 1}
end,
{ok, {Total, Num}} = data_accumulate:accumulate([10, 20, 30], AvgAccumulator, {0, 0}),
Average = Total / Num.  % => 20.0

% Map aggregation (group by key)
GroupFun = fun(Map, {Key, Value}) ->
    maps:put(Key, [Value | maps:get(Key, Map, [])], Map)
end,
data_accumulate:accumulate([{a, 1}, {b, 2}, {a, 3}], GroupFun, #{}).
% => {ok, #{a => [3, 1], b => [2]}}
```

### Petri Net Callbacks

```erlang
place_lst() -> [p_start, p_collecting, p_accumulated, p_end].

trsn_lst() -> [t_accumulate, t_finish].

preset(t_accumulate) -> [p_start];
preset(t_finish) -> [p_accumulated];
preset(_) -> [].
```

---

## WDP-05: Data Visibility

**Module**: `data_visibility`

**Purpose**: Controls data visibility and access within the workflow. Defines which parts of the workflow can access specific data items, implementing scope and access control rules.

### Use Cases

- Implementing read/write access control
- Scope-based data isolation
- Permission checks before data access
- Conditional data exposure
- Security-sensitive data handling

### State Record

```erlang
-record(data_visibility_state, {
    data             :: term(),
    scope            :: term(),
    access_check_fun :: function(),
    access_granted   :: boolean(),
    start_time       :: integer()
}).
```

### API Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `new/3` | `(Data, Scope, Fun) -> State` | Create visibility state |
| `start/3` | `(Data, Scope, Fun) -> {ok, Pid}` | Start as gen_pnet process |
| `get_state/1` | `(Pid) -> {ok, State}` | Get current state |
| `check_visibility/3` | `(Data, Scope, Fun) -> {ok, boolean()} \| {error, Reason}` | Check visibility |

### Petri Net Structure

```
Places:
  p_start         - Start of visibility check
  p_check_scope   - Checking access scope
  p_granted       - Access granted
  p_denied        - Access denied
  p_end           - End of the pattern

Transitions:
  t_check         - Check visibility/access permissions
  t_grant         - Grant access
  t_deny          - Deny access
  t_finish        - Complete the pattern
```

### Diagram

```
           p_start
              |
          [t_check]
              |
         p_check_scope
          /        \
    [t_grant]    [t_deny]
       |            |
    p_granted    p_denied
       |
    [t_finish]
       |
       p_end
```

### Example Usage

```erlang
% Simple scope check
IsPublic = fun(_Data, public) -> true; (_Data, _Other) -> false end,

{ok, Visible} = data_visibility:check_visibility(sensitive_data, public, IsPublic),
% => {ok, false}

{ok, Visible} = data_visibility:check_visibility(sensitive_data, private, IsPublic),
% => {ok, false}

% Role-based access control
CanAccess = fun(Data, #{role := admin}) -> true;
              (Data, #{role := User}) when User =:= element(1, Data) -> true;
              (_Data, _Scope) -> false
           end,

% User can access own data
data_visibility:check_visibility({alice, info}, #{role => alice}, CanAccess),
% => {ok, true}

% Admin can access any data
data_visibility:check_visibility({bob, info}, #{role => admin}, CanAccess),
% => {ok, true}

% Other users cannot access
data_visibility:check_visibility({alice, info}, #{role => bob}, CanAccess),
% => {ok, false}
```

### Visibility Levels

```erlang
% Multi-level visibility
LevelCheck = fun
    (Data, {user, Level}, #{clearance := Clearance}) when Clearance >= Level -> true;
    (_Data, _Scope, _Context) -> false
end,

data_visibility:check_visibility(
    classified_doc,
    {user, 3},  % Requires level 3
    #{clearance => 5},  % User has level 5
    LevelCheck
).
% => {ok, true}
```

### Petri Net Callbacks

```erlang
place_lst() -> [p_start, p_check_scope, p_granted, p_denied, p_end].

trsn_lst() -> [t_check, t_grant, t_deny, t_finish].

preset(t_check) -> [p_start];
preset(t_grant) -> [p_check_scope];
preset(t_deny) -> [p_check_scope];
preset(t_finish) -> [p_granted];
preset(_) -> [].
```

---

## Pattern Composition

WDP patterns can be combined to create complex data flow scenarios:

### Example: Pipeline with Transformation and Distribution

```erlang
% Transform data, then distribute to multiple targets
pipeline(Data, Targets) ->
    % Step 1: Transform
    TransformFun = fun(D) -> normalize(D) end,
    {ok, Transformed} = data_transform:transform(TransformFun, Data),

    % Step 2: Distribute
    DistributeFun = fun(D, T) -> send_to_target(T, D) end,
    data_distribute:distribute(Transformed, Targets, DistributeFun).
```

### Example: Fan-out then Accumulate

```erlang
% Distribute work, then accumulate results
parallel_process(Data, Workers) ->
    % Fan-out
    {ok, _} = data_distribute:distribute(Data, Workers, fun do_work/2),

    % Collect results (implementation depends on worker response pattern)
    Results = collect_responses(Workers),

    % Accumulate
    data_accumulate:accumulate(Results, fun merge/2, #{}).
```

### Example: Visibility-Gated Distribution

```erlang
% Only distribute to targets with visibility access
secure_distribution(Data, Targets, AccessChecker) ->
    % Check visibility first
    case data_visibility:check_visibility(Data, targets, AccessChecker) of
        {ok, true} ->
            % Proceed with distribution
            data_distribute:distribute(Data, Targets, fun secure_send/2);
        {ok, false} ->
            {error, access_denied}
    end.
```

---

## Testing

Each WDP pattern includes EUnit tests:

```bash
# Run all WDP pattern tests
rebar3 eunit --module=param_pass
rebar3 eunit --module=data_transform
rebar3 eunit --module=data_distribute
rebar3 eunit --module=data_accumulate
rebar3 eunit --module=data_visibility
```

### Test Coverage

- **Place/Transition enumeration**: `place_lst/0`, `trsn_lst/0`
- **Preset/Postset**: `preset/1`
- **Initial marking**: `init_marking/2`
- **Transition enabling**: `is_enabled/3`
- **Transition firing**: `fire/3`
- **API functions**: All exported API functions

---

## References

- **Workflow Patterns**: [Workflow Patterns Initiative](http://www.workflowpatterns.com/)
- **YAWL Specification**: Yet Another Workflow Language
- **gen_pnet**: CRE Petri Net execution engine
- **gen_yawl**: YAWL workflow behavior on top of gen_pnet

---

## See Also

- `/Users/sac/cre/docs/YAWL_PATTERNS_GUIDE.md` - Core YAWL pattern reference
- `/Users/sac/cre/docs/CORE_YAWL_PATTERNS_GUIDE.md` - Core patterns guide
