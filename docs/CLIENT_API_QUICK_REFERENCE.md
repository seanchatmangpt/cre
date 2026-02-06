# CRE Client API Quick Reference

## Overview

This quick reference provides essential information for using the CRE client API modules. It covers the most common functions, patterns, and usage scenarios for both `cre_client` and `cre_yawl_client`.

## cre_client Module

### Core Functions

#### Starting Clients
```erlang
%% Anonymous client
{ok, Pid} = cre_client:start_link(CreName, ClientMod, ClientArg)

%% Named client
{ok, Pid} = cre_client:start_link(Name, CreName, ClientMod, ClientArg)
```

#### Workflow Evaluation
```erlang
%% Synchronous evaluation (blocking)
Result = cre_client:eval(ClientPid, WorkflowExpr)

%% Handle worker replies
cre_client:cre_reply(ClientPid, From, TaskArg, ResultValue)

%% Stop client
cre_client:stop(ClientPid)
```

### Callback Interface

**Required callbacks:**
- `init(InitArg) -> UsrInfo`
- `is_value(E, UsrInfo) -> boolean()`
- `step(E, UsrInfo) -> {ok, NewE, TaskList}`
- `recv(E, Replies, UsrInfo) -> NewUsrInfo`
- `load(E, UsrInfo) -> LoadedState`
- `unload(E, UsrInfo) -> FinalResult`

## cre_yawl_client Module

### Core Functions

#### Starting Clients
```erlang
%% Anonymous YAWL client
{ok, Pid} = cre_yawl_client:start_link(CreName, WorkflowExpr)

%% Named YAWL client
{ok, Pid} = cre_yawl_client:start_link(Name, CreName, WorkflowExpr)
```

#### Workflow Execution
```erlang
%% Execute complete workflow
{ok, Result} = cre_yawl_client:execute_workflow(ClientPid, InitialData)

%% Execute single pattern
{ok, Result} = cre_yawl_client:execute_pattern(ClientPid, Pattern, InputData)

%% Compose multiple patterns
WorkflowExpr = cre_yawl_client:compose_patterns([Pattern1, Pattern2], Options)

%% Get workflow state
{ok, State} = cre_yawl_client:get_workflow_state(ClientPid)

%% Get workflow results
{ok, Results} = cre_yawl_client:get_workflow_results(ClientPid)

%% Terminate workflow
cre_yawl_client:terminate_workflow(ClientPid)
```

### Workflow Patterns

#### Control Flow
- `sequence([Tasks])` - Execute tasks sequentially
- `parallel_split([Tasks])` - Execute tasks in parallel
- `synchronization()` - Wait for all tasks to complete
- `exclusive_choice([{Task, Condition}])` - Select one branch
- `simple_merge()` - Merge from single source
- `multi_choice([{Task, Condition}])` - Select multiple branches
- `synchronizing_merge()` - Wait for multiple sources
- `multi_merge()` - Merge from multiple sources
- `discriminator()` - Select based on condition
- `arbitration()` - Select one of multiple options

#### Data Flow
- `param_pass(Source, Target)` - Pass parameters
- `data_transform(Source, Target, TransformFun)` - Transform data
- `data_distribute(Source, Targets)` - Distribute data
- `data_accumulate(Sources, Target)` - Accumulate data
- `data_visibility(Source, Scope)` - Control visibility

#### Resource Management
- `resource_create(ResourceDef)` - Create resource
- `role_allocate(Task, Role)` - Allocate by role
- `resource_start(Resource)` - Start resource
- `role_distribute(Task, Roles)` - Distribute roles
- `capability_allocate(Task, Capability)` - Allocate capability

### Pattern Composition Options
```erlang
Options = #{
    mode => sequence | parallel | conditional,  % Composition mode
    error_handling => continue | stop,          % Error handling strategy
    timeout => integer()                      % Optional timeout
}
```

## Common Usage Patterns

### Basic Workflow
```erlang
%% 1. Start CRE master
{ok, MasterPid} = cre_master:start_link(my_cre),

%% 2. Start client
{ok, ClientPid} = cre_yawl_client:start_link(my_cre, Workflow),

%% 3. Execute workflow
{ok, Result} = cre_yawl_client:execute_workflow(ClientPid, #{}),

%% 4. Cleanup
cre_yawl_client:terminate_workflow(ClientPid),
cre_master:stop(MasterPid).
```

### Simple Sequence Workflow
```erlang
Workflow = cre_yawl:sequence([
    {task1, "Step 1", []},
    {task2, "Step 2", []},
    {task3, "Step 3", []}
]).
```

### Parallel Processing
```erlang
Workflow = cre_yawl:parallel_split([
    {process_data, "Process data", []},
    {validate_data, "Validate data", []}
]).
```

### Conditional Execution
```erlang
Workflow = cre_yawl:exclusive_choice([
    {happy_path, fun() -> is_valid_data() end},
    {error_path, fun() -> true end}
]).
```

### Error Handling
```erlang
try
    {ok, Result} = cre_yawl_client:execute_workflow(ClientPid, Data),
    handle_success(Result)
catch
    Error:Reason ->
        handle_error(Error, Reason)
end.
```

## Configuration

### CRE Master Configuration
```erlang
%% Start with custom name
{ok, MasterPid} = cre_master:start_link(my_cre_instance).

%% Start anonymous
{ok, MasterPid} = cre_master:start_link().
```

### Client Poll Interval
```erlang
%% Set via persistent_term
persistent_term:put(cre_client_poll_interval, 500).  % 500ms
```

## Return Values

### Success Results
```erlang
{ok, Result}                          % Success with result
{ok, State}                           % State query success
```

### Error Results
```erlang
{error, Reason}                       % General error
{error, {validation_failed, Errors}}  % Workflow validation error
{error, client_not_found}             % Client not found
```

### Workflow Result Structure
```erlang
#{
    status => completed | failed | terminated,
    results => #{TaskId => TaskResult},
    errors => [{ErrorType, Reason}],
    duration_ms => integer(),
    completed_at => DateTime
}
```

## Quick Examples

### 1. Simple Task Execution
```erlang
{ok, Client} = cre_yawl_client:start_link(my_cre,
    cre_yawl:sequence([{my_task, "Do work", []}])),

{ok, Result} = cre_yawl_client:execute_workflow(Client, #{}),
```

### 2. Parallel Data Processing
```erlang
ParallelWorkflow = cre_yawl:parallel_split([
    {process_file1, "File 1", []},
    {process_file2, "File 2", []}
]),
```

### 3. Conditional Branching
```erlang
ConditionalWorkflow = cre_yawl:exclusive_choice([
    {success_branch, fun() -> check_data_quality() end},
    {cleanup_branch, fun() -> true end}
]),
```

### 4. Custom Client
```erlang
-module(my_client).
-behaviour(cre_client).

init(_Arg) -> #{user => "me"}.
is_value(E, _Info) -> is_workflow_complete(E).
step(E, Info) -> {ok, E, [task]}.
recv(_, _, Info) -> Info.
load(E, _) -> E.
unload(_, Info) -> Info.

%% Usage
{ok, Pid} = cre_client:start_link(my_cre, my_client, []),
Result = cre_client:eval(Pid, workflow),
```

## Error Codes

### Common Errors
- `{invalid_workflow_expression, Expr}` - Invalid workflow definition
- `{workflow_validation_failed, Errors}` - YAWL validation errors
- `client_not_found` - Client process not found
- `timeout` - Operation timed out
- `cre_down` - CRE master connection lost

### Best Practices
1. Always validate workflows before execution
2. Use named clients for easier debugging
3. Implement proper error handling
4. Clean up client processes when done
5. Monitor workflow progress for long-running tasks

This quick reference should help you get started with the CRE client API. For detailed documentation, see the complete API reference.