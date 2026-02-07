# Exception Handling in CRE

## Overview

CRE provides exception handling constructs for robust workflow error management through two modules:

- **`wf_exception`**: Core exception types, compensation tracking, and exception propagation
- **`wf_try_region`**: Try-catch regions for workflow execution with automatic compensation

## Exception Types

| Type | Description |
|------|-------------|
| `application_error` | Business logic errors (validation, domain rules) |
| `system_error` | Technical failures (crash, out of memory) |
| `timeout_error` | Operation exceeded time limit |
| `resource_error` | Resource unavailable or locked |
| `validation_error` | Input data validation failure |

## Module: wf_exception

### Creating Exceptions

```erlang
%% Create exception with type, reason, and data
Exc = wf_exception:new(application_error, payment_failed, #{amount => 100}),

%% Create with empty data
Exc2 = wf_exception:new(system_error, crash),

%% From Erlang error term
Exc3 = wf_exception:from_error({error, ebadf}).
```

### Exception Accessors

```erlang
Type = wf_exception:type(Exc),
Reason = wf_exception:reason(Exc),
Data = wf_exception:data(Exc),
Source = wf_exception:source(Exc),
TS = wf_exception:timestamp(Exc).
```

### Compensation Actions

```erlang
%% Create compensation action
Comp = wf_exception:compensation(refund_payment, #{txn_id => tx123}),

%% Get action and data
Action = wf_exception:comp_action(Comp),
Data = wf_exception:comp_data(Comp).
```

### Exception Handlers

```erlang
%% Create handler
Handler = wf_exception:handler(
    fun(E) -> wf_exception:type(E) =:= application_error end,
    fun(_, _) -> {handled, recovered} end
),

%% Check if handler can process
CanHandle = wf_exception:can_handle(Exc, Handler),

%% Handle exception
Result = wf_exception:handle(Exc, Handler).
```

### Exception Bubbling

```erlang
%% Mark exception for bubbling
BubbleExc = wf_exception:bubble(Exc),

%% Check if bubbleable
IsBubbleable = wf_exception:is_bubbleable(BubbleExc),

%% Set source
ExcWithSource = wf_exception:set_source(Exc, payment_gateway).
```

## Module: wf_try_region

### Execute with Exception Handling

```erlang
%% Basic try-catch
Result = wf_try_region:execute(
    fun() -> risky_operation() end,
    fun(Exc) -> handle_error(Exc) end,
    self(),
    []
).
```

### Execute with Compensation

```erlang
%% With compensation - undo actions on error
Comp1 = wf_exception:compensation(rollback_step1, #{id => 1}),
Comp2 = wf_exception:compensation(rollback_step2, #{id => 2}),
CompStack = wf_try_region:add_compensation([Comp1], Comp2),

Result = wf_try_region:execute(
    fun() ->
        %% Do work that may need compensation
        critical_operation()
    end,
    fun(Exc) -> log_error(Exc) end,
    self(),
    CompStack
).
```

### Raise Exception

```erlang
wf_try_region:raise(
    application_error,
    payment_failed,
    #{amount => 100, reason => insufficient_funds},
    payment_gateway
).
```

## Doctests

```erlang
%% Try-catch catches error and runs handler
> wf_try_region:execute(
..     fun() -> erlang:error(test_error) end,
..     fun(Exc) -> {caught, wf_exception:reason(Exc)} end,
..     self(),
..     []
.. ).
{caught, test_error}

%% Normal execution returns result
> wf_try_region:execute(
..     fun() -> {ok, success} end,
..     fun(_) -> should_not_be_called end,
..     self(),
..     []
.. ).
{ok, success}

%% Add compensation to stack
> Comp1 = wf_exception:compensation(action1, #{}),
> Comp2 = wf_exception:compensation(action2, #{}),
> Stack = wf_try_region:add_compensation(
..     wf_try_region:add_compensation([], Comp1),
..     Comp2
.. ).
[Comp2, Comp1]
```

## Compensation Flow

When an exception occurs in a try_region:

1. Exception is caught and converted to `wf_exception:exception()`
2. Compensation actions execute in **reverse order** (LIFO - Last In First Out)
3. Each compensation receives:
   - Its associated data map
   - The exception that triggered compensation
4. After all compensations run, the catch handler is invoked
5. Result returned to caller

## Error Paths

Exceptions can be handled at multiple levels:

1. **Local handler** - Directly in try_region catch function
2. **Bubbling** - Exception propagates up if handler returns `unhandled`
3. **Global handler** - Top-level workflow exception handler
4. **System default** - Unhandled exceptions fail the workflow case

## Exception Records

An exception record contains:

```erlang
#{
    type := exception_type(),
    reason := reason(),
    data := map(),
    source := source(),
    timestamp := integer()
}
```

## Compensation Records

A compensation record contains:

```erlang
#{
    action := atom() | function(),
    data := map(),
    executed := boolean()
}
```
