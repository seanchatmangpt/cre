# Migration Guide: Old CRE → New Architecture

This guide explains how to migrate existing CRE workflows to the new Joe Armstrong architecture with `gen_pnet` as the only runtime component.

---

## Architecture Comparison

### Old Architecture
```
cre_master (gen_server)
├── cre_client (gen_server)
├── cre_worker (gen_pnet)
├── yawl_engine (gen_server)
└── Multiple worker processes
```

### New Architecture
```
gen_pnet (single OTP process)
├── pnet_types (pure utility)
├── pnet_marking (pure utility)
├── pnet_mode (pure utility)
├── pnet_choice (pure utility)
├── pnet_receipt (pure utility)
├── wf_timerq (pure utility)
├── wf_task (pure utility)
└── wf_scope (pure utility)
```

---

## Migration Strategy

### Phase 1: Understand the New Interface

#### Key Changes
1. **Single Runtime Process**: Only `gen_pnet` maintains state
2. **Pure Utilities**: All helper modules are stateless
3. **Message Contracts**: Clear inter-process communication
4. **Progress Loop**: Automatic token processing in `gen_pnet`

#### Core Concepts
- **Marking**: Multiset representation of token counts
- **Modes**: All possible input token combinations for transitions
- **Fire**: Transition execution producing new tokens
- **Receipts**: Audit trail of all state changes

### Phase 2: Rewrite Net Module

#### Old Net Module (cre_engine)
```erlang
-module(cre_engine).
-behaviour(gen_server).

% Old callbacks
-export([start_link/1, handle_call/3, handle_cast/2]).

% Old state management
-record(state, {
    workflow_id,
    cases = [],
    workers = []
}).
```

#### New Net Module (simple_workflow)
```erlang
-module(simple_workflow).
-behaviour(pnet_net).

% New callbacks - pure functions
-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/2, fire/3]).

places() -> [start, task1, task2, done].
transitions() -> [t1, t2].

preset(t1) -> [start];
preset(t2) -> [task1].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{start := [init]}, _UsrInfo) -> [#{start => []}].
modes(t2, #{task1 := [done]}, _UsrInfo) -> [#{task1 => []}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{task1 => [done]}}.

fire(t2, #{task1 => []}, _UsrInfo) ->
    {produce, #{done => [complete]}}.
```

#### Migration Checklist
- [ ] Remove gen_server behavior
- [ ] Convert state management to pure functions
- [ ] Replace process-based coordination with token injection
- [ ] Simplify callback signatures
- [ ] Remove all side effects from callbacks

### Phase 3: Rewrite Client Code

#### Old Client (cre_client)
```erlang
% Old way - direct process calls
cre:start(),
{ok, CrePid} = cre:pid(node()),
{ok, ClientPid} = cre_client:start_link(CrePid, logic_client, []),
Result = cre_client:eval(ClientPid, Expression).
```

#### New Client (using gen_pnet)
```erlang
% New way - token injection
{ok, Pid} = gen_pnet:start_link(my_workflow, [], []),
ok = gen_pnet:produce(Pid, #{start => [init]}),
% Progress happens automatically
Marking = gen_pnet:marking(Pid).
```

#### Migration Patterns
```erlang
% Pattern 1: Task submission
% Old: cre_client:eval(ClientPid, {task, my_task})
% New: gen_pnet:produce(Pid, wf_task:enabled(my_task, data, task_place))

% Pattern 2: Result retrieval
% Old: receive {result, Value} -> ...
% New: gen_pnet:call(Pid, {get, result_place})

% Pattern 3: Workflow control
% Old: cre_master:start_workflow(WorkflowDef)
% New: gen_pnet:start_link(workflow_net, WorkflowDef, [])
```

### Phase 4: Rewrite Worker Code

#### Old Worker (cre_worker)
```erlang
-module(my_worker).
-behaviour(cre_worker).

-export([init/1, run/2]).

init(_Args) -> my_state.
run(Task, _State) ->
    {ok, Result}.
```

#### New Workflow Integration
```erlang
% Workers become external processes that inject task tokens
-module(my_external_worker).

run_workflow(WorkflowPid, TaskId, Data) ->
    % Inject enabled task
    gen_pnet:produce(WorkflowPid, wf_task:enabled(TaskId, Data, task_queue)),

    % Poll for completion
    poll_completion(WorkflowPid, TaskId).

poll_completion(Pid, TaskId) ->
    case gen_pnet:ls(Pid, done) of
        {ok, [{task, TaskId, done, Result}]} -> Result;
        {ok, _} -> timer:sleep(100), poll_completion(Pid, TaskId)
    end.
```

### Phase 5: Update Configuration

#### Old Configuration (cre_config.erl)
```erlang
-record(config, {
    master_node,
    worker_pool_size,
    timeout,
    auth_enabled
}).
```

#### New Configuration (Net module init/1)
```erlang
% Pass configuration through net_arg
init(NetArg) ->
    #{pool_size := PoolSize, timeout := Timeout} = NetArg,
    % Initialize state
    #{pool_size => PoolSize, timeout => Timeout}.
```

---

## Step-by-Step Migration Example

### Step 1: Original Workflow
```erlang
% Old YAWL workflow definition
WorkflowDef = [
    {workflow, "order_processing"},
    {places, [order_received, payment_pending, payment_approved, shipping, completed]},
    {transitions, [process_payment, ship_order]},
    {preset, process_payment, [order_received]},
    {preset, ship_order, [payment_approved]},
    {postset, process_payment, [payment_pending]},
    {postset, ship_order, [shipping]},
    {flow, payment_pending, payment_approved, []}
].
```

### Step 2: Rewrite as Net Module
```erlang
-module(order_workflow).
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/2, fire/3]).

places() -> [order_received, payment_pending, payment_approved, shipping, completed].
transitions() -> [process_payment, ship_order].

preset(process_payment) -> [order_received];
preset(ship_order) -> [payment_approved].

init(_NetArg) -> #{orders => #{}}.

init_marking(order_received, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(process_payment, #{order_received := [init]}, _UsrInfo) ->
    [#{order_received => []}].

modes(ship_order, #{payment_pending := [approved]}, _UsrInfo) ->
    [#{payment_pending => []}].

fire(process_payment, #{order_received => []}, UsrInfo) ->
    OrderId = generate_order_id(),
    {produce, #{
        order_received => [],
        payment_pending => [OrderId]
    }}.

fire(ship_order, #{payment_pending => [OrderId]}, _UsrInfo) ->
    {produce, #{
        payment_pending => [],
        shipping => [OrderId],
        completed => [OrderId]
    }}.
```

### Step 3: Update Client Code
```erlang
% Before: cre_client:start_link + cre_client:eval
% After: gen_pnet:start_link + gen_pnet:produce

start_order_processing() ->
    {ok, Pid} = gen_pnet:start_link(order_workflow, #{}, []),
    ok = gen_pnet:produce(Pid, #{order_received => [init]}),
    Pid.

get_order_status(Pid, OrderId) ->
    case gen_pnet:ls(Pid, completed) of
        {ok, Tokens} when OrderId == OrderId ->
            completed;
        {ok, _} ->
            payment_pending
    end.
```

### Step 4: Replace Timers
```erlang
% Old: Direct timer in worker
timer:send_after(Timeout, self(), {timeout, TaskId}),

% New: wf_timerq
Timeout = 5000,
Now = erlang:monotonic_time(millisecond),
Deadline = Now + Timeout,
TimerEvent = {produce, #{payment_timeout => [OrderId]}},
TimerQ = wf_timerq:arm(TimerQ, payment_timeout, Deadline, TimerEvent),
gen_pnet:produce(Pid, TimerEvent).
```

### Step 5: Add Observability
```erlang
% Subscribe to receipts for monitoring
spawn(fun() ->
    receive
        {pnet_receipt, Receipt} ->
            log_transition(Receipt)
    end
end).

% Get receipts for debugging
Receipts = gen_pnet:receipts(Pid, 10),
lists:foreach(fun(R) -> io:format("~p~n", [R]) end, Receipts).
```

---

## Common Migration Issues

### Issue 1: State Management
**Problem**: Old code used gen_server state
**Solution**: Move state to net module usr_info, use token injection for updates

```erlang
% Old: gen_server:call(Pid, {update_state, NewState})
% New: gen_pnet:produce(Pid, #{state_update => [NewState]})
```

### Issue 2: Multiple Processes
**Problem**: Old system had multiple worker processes
**Solution**: Use external processes that inject task tokens

```erlang
% Old: cre_worker:start_link(CrePid, MyWorker, Args)
% New: spawn(fun() -> MyWorker:run(TaskId, Data) end)
```

### Issue 3: Complex Workflows
**Problem**: Workflows spanned multiple processes
**Solution**: Use wf_scope for hierarchical workflows

```erlang
% Define parent-child relationship
BindingTable = #{subworkflow => #{parent_start => child_start}},
% Enter scope
ChildTokens = wf_scope:enter(BindingTable, subworkflow, ParentDelta),
gen_pnet:produce(Pid, ChildTokens).
```

### Issue 4: Error Handling
**Problem**: Old error handling used process links
**Solution**: Use token-based error reporting

```erlang
% Old: exit({error, Reason}) to crash process
% New: gen_pnet:produce(Pid, wf_task:failed(TaskId, Reason, error_place))
```

---

## Testing Strategy

### Unit Testing Net Modules
```erlang
-module(order_workflow_tests).
-include_lib("eunit/include/eunit.hrl").

fire_test() ->
    Marking = pnet_marking:new([order_received, payment_pending, payment_approved]),
    UsrInfo = #{orders => #{}},

    % Test payment processing
    Modes = order_workflow:modes(process_payment, Marking, UsrInfo),
    ?assertMatch([#{order_received := []}], Modes),

    {produce, NewTokens} = order_workflow:fire(process_payment, #{order_received => []}, UsrInfo),
    ?assert maps:is_key(payment_pending, NewTokens).
```

### Integration Testing
```erlang
integration_test() ->
    {ok, Pid} = gen_pnet:start_link(order_workflow, #{}, []),

    % Start workflow
    ok = gen_pnet:produce(Pid, #{order_received => [init]}),

    % Check progress
    ?timer_sleep(100),
    ?assertMatch(#{completed := [OrderId]}, gen_pnet:marking(Pid)).
```

---

## Performance Considerations

### Memory Usage
- **Old**: Multiple processes maintaining state
- **New**: Single process with marking state

### Concurrency
- **Old**: Process-based concurrency
- **New**: Token-based parallelism in progress loop

### Monitoring
- **Old**: Process monitoring via gen_server calls
- **New**: Receipt-based monitoring via subscriptions

---

## Timeline for Migration

### Week 1: Understanding and Planning
- [ ] Study new architecture documentation
- [ ] Inventory existing workflows
- [ ] Create migration plan

### Week 2: Net Module Migration
- [ ] Rewrite 2-3 key workflows as net modules
- [ ] Test basic functionality
- [ ] Update client integration

### Week 3: External Process Migration
- [ ] Rewrite worker processes to external agents
- [ ] Implement task token injection
- [ ] Test end-to-end workflows

### Week 4: Advanced Features
- [ ] Implement timers and scopes
- [ ] Add observability (receipts)
- [ ] Performance testing and optimization

### Week 5: Deployment
- [ ] Deploy to staging environment
- [ ] Monitor and debug
- [ ] Complete migration rollback plan

---

## Rollback Plan

If migration issues arise:

1. **Keep Old Service Running**: Run both old and new systems in parallel
2. **Feature Flag**: Use feature flags to toggle between old/new
3. **Gradual Migration**: Migrate one workflow at a time
4. **Monitoring**: Compare behavior between both systems
5. **Rollback**: Switch back to old system if issues detected

This guide provides a comprehensive approach to migrating from the old CRE architecture to the new Joe Armstrong design, ensuring a smooth transition while maintaining functionality.