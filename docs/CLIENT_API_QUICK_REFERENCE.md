# Client API Quick Reference Card

## cre_client Module

### Core Functions

```erlang
%% Start anonymous client
{ok, Pid} = cre_client:start_link(CreName, ClientMod, ClientArg).

%% Start named client
{ok, Pid} = cre_client:start_link(ClientName, CreName, ClientMod, ClientArg).

%% Evaluate workflow (blocking)
Result = cre_client:eval(ClientPid, WorkflowExpr).

%% Handle worker reply
ok = cre_client:cre_reply(ClientPid, From, TaskArg, Result).

%% Stop client
ok = cre_client:stop(ClientPid).
```

### Callback Interface

```erlang
%% Required callbacks
init(InitArg) -> UsrInfo.
is_value(E, UsrInfo) -> boolean().
step(E, UsrInfo) -> {ok, NewUsrInfo, Tasks}.
recv(E, ReplyLst, UsrInfo) -> NewUsrInfo.
load(E, UsrInfo) -> NewUsrInfo.
unload(E, UsrInfo) -> FinalResult.
```

### Example Usage

```erlang
%% Basic client lifecycle
{ok, ClientPid} = cre_client:start_link(my_cre, my_client, []).
Result = cre_client:eval(ClientPid, workflow_expr).
cre_client:stop(ClientPid).
```

## cre_yawl_client Module

### Core Functions

```erlang
%% Start YAWL client
{ok, Pid} = cre_yawl_client:start_link(CreName, WorkflowExpr).

%% Execute workflow
{ok, Results} = cre_yawl_client:execute_workflow(ClientPid, InitialData).

%% Execute single pattern
{ok, Results} = cre_yawl_client:execute_pattern(ClientPid, Pattern, InputData).

%% Get workflow state
{ok, State} = cre_yawl_client:get_workflow_state(ClientPid).

%% Get results
{ok, Results} = cre_yawl_client:get_workflow_results(ClientPid).

%% Terminate workflow
ok = cre_yawl_client:terminate_workflow(ClientPid).
```

### Pattern Functions

```erlang
%% Create patterns
Pattern = cre_yawl:sequence([task1, task2]).
Pattern = cre_yawl:parallel_split([task1, task2, task3]).
Pattern = cre_yawl:exclusive_choice([{task1, Condition}, {task2, Condition}]).

%% Compose patterns
Composed = cre_yawl_client:compose_patterns([P1, P2], #{mode => sequence}).
```

### Example Usage

```erlang
%% Simple workflow
Workflow = cre_yawl:new_workflow().
Workflow1 = cre_yawl:add_task(Workflow, <<"task1">>, [{name, "Task 1"}]).
Workflow2 = cre_yawl:connect(Workflow1, <<"task1">>, <<"task2">>).

{ok, ClientPid} = cre_yawl_client:start_link(CrePid, Workflow2).
{ok, Results} = cre_yawl_client:execute_workflow(ClientPid, #{data => input}).
```

## Common Patterns

### Basic Workflow

```erlang
%% 1. Start CRE
cre:start().
{ok, CrePid} = cre:pid(node()).

%% 2. Create client
{ok, ClientPid} = cre_client:start_link(CrePid, my_client, []).

%% 3. Execute workflow
Result = cre_client:eval(ClientPid, workflow_expr).

%% 4. Clean up
cre_client:stop(ClientPid).
```

### Error Handling

```erlang
%% Handle errors gracefully
case cre_client:eval(ClientPid, workflow_expr) of
    {ok, Result} ->
        handle_success(Result);
    {error, Reason} ->
        handle_error(Reason)
end.
```

### YAWL Patterns

```erlang
%% Sequence pattern
Sequence = cre_yawl:sequence([
    cre_yawl:task(<<"task1">>, module, function, []),
    cre_yawl:task(<<"task2">>, module, function, [])
]).

%% Parallel pattern
Parallel = cre_yawl:parallel_split([
    cre_yawl:task(<<"task1">>, module, function, []),
    cre_yawl:task(<<"task2">>, module, function, [])
]).

%% Choice pattern
Choice = cre_yawl:exclusive_choice([
    {cre_yawl:task(<<"task1">>, module, function, []), Condition1},
    {cre_yawl:task(<<"task2">>, module, function, []), Condition2}
]).
```

## Configuration

### Client Configuration

```erlang
%% Poll interval (persistent_term for O(1) access)
persistent_term:put(cre_client_poll_interval, 250).

%% Max concurrent tasks
persistent_term:put(cre_client_max_concurrent, 100).

%% Timeout
persistent_term:put(cre_client_timeout, 30000).
```

## Common Return Values

### Success Responses

```erlang
%% Generic client success
{ok, Result}

%% YAWL client success
{ok, #{status => completed, results => #{...}, duration_ms => 1500}}
```

### Error Responses

```erlang
%% Client errors
{error, Reason}

%% YAWL client errors
{error, workflow_validation_failed}
{error, client_not_found}
{error, timeout}
```

## Doctest Examples

### cre_client Examples

```erlang
%% Start client
1> {ok, Pid} = cre_client:start_link(my_cre, my_client, []).
{ok,<0.123.0>}

%% Evaluate workflow
2> Result = cre_client:eval(Pid, workflow_expr).
{ok, #{result => "completed"}}

%% Stop client
3> cre_client:stop(Pid).
ok
```

### cre_yawl_client Examples

```erlang
%% Start YAWL client
1> {ok, Pid} = cre_yawl_client:start_link(my_cre, workflow).
{ok,<0.456.0>}

%% Execute workflow
2> {ok, Results} = cre_yawl_client:execute_workflow(Pid, #{data => input}).
{ok, #{status => completed, duration_ms => 1500}}

%% Get workflow state
3> {ok, State} = cre_yawl_client:get_workflow_state(Pid).
{ok, #{execution_state => running, active_tasks => 1}}
```

## Integration Patterns

### Human-in-the-Loop

```erlang
%% Workflow with human task
Workflow = cre_yawl:add_task(base_workflow, <<"approval">>,
    [{name, "Human Approval"}, {type, human}, {assignee, "manager"}]).
```

### Error Recovery

```erlang
%% Retry on errors
execute_with_retry(Fun, MaxRetries) ->
    execute_with_retry(Fun, MaxRetries, 1000).

execute_with_retry(Fun, 0, _) -> Fun();
execute_with_retry(Fun, Retries, Delay) ->
    case Fun() of
        {ok, Result} -> Result;
        {error, _} ->
            timer:sleep(Delay),
            execute_with_retry(Fun, Retries - 1, Delay * 2)
    end.
```

### Performance Monitoring

```erlang
%% Monitor execution time
{Time, Result} = timer:tc(fun() ->
    cre_client:eval(ClientPid, workflow_expr)
end).

%% Log metrics
metrics:log(cre_execution_time, Time).
```

## Checklist

### Client Setup Checklist

- [ ] Start CRE system
- [ ] Create client process
- [ ] Configure client options
- [ ] Set up callback module
- [ ] Prepare workflow expressions
- [ ] Handle errors gracefully
- [ ] Clean up resources

### Workflow Execution Checklist

- [ ] Validate workflow specification
- [ ] Initialize client state
- [ ] Execute workflow steps
- [ | Monitor progress
- [ | Handle worker replies
- [ | Collect results
- [ | Clean up state

### Error Handling Checklist

- [ ] Handle client startup errors
- [ ] Handle workflow validation errors
- [ ] Handle task execution errors
- [ | Handle timeout conditions
- [ | Handle resource limitations
- [ | Implement retry logic
- [ | Provide meaningful error messages