# Quick Reference Cheatsheet

**Essential Commands and APIs for YAWL Workflow Engine**

## Quick Start

### Basic Workflow Creation and Execution

```erlang
% 1. Create a simple workflow
Spec = create_simple_workflow(),
{ok, Module} = yawl_compile:compile(Spec).

% 2. Start workflow
{ok, Pid} = gen_pnet:start_link(Module, #{user_id => "user123"}, []).

% 3. Execute step
{ok, Receipt} = gen_pnet:step(Pid).

% 4. Check state
{ok, Marking} = gen_pnet:marking(Pid).

% 5. Stop workflow
gen_pnet:stop(Pid).
```

## Core APIs

### gen_pnet (Main OTP Component)

```erlang
% Start workflows
gen_pnet:start_link(Module, NetArg, Options)
gen_pnet:start(Module, NetArg, Options)

% Execute workflows
gen_pnet:step(Pid)                    % Single step
gen_pnet:inject(Pid, Tokens)          % Inject tokens
gen_pnet:concurrent_step(Pid)         % Concurrent execution

% State inspection
gen_pnet:marking(Pid)                % Current marking
gen_pnet:state(Pid)                  % Internal state
gen_pnet:receipts(Pid)               % Execution receipts
gen_pnet:stats(Pid)                 % Statistics

% Communication
gen_pnet:call(Pid, Request)         % Synchronous call
gen_pnet:cast(Pid, Request)         % Asynchronous call
gen_pnet:reply(From, Reply)          % Send reply

% Lifecycle
gen_pnet:stop(Pid)                  % Stop workflow
gen_pnet:reset_stats(Pid)           % Reset statistics
```

### YAWL Compilation

```erlang
% Compile YAWL specification to module
yawl_compile:compile(Spec, #{seed => 123, module_prefix => "yawl_"})

% Compile and save to file
yawl_compile:compile_to_file(Spec, "output_dir", #{gen_observer => true})

% Access compiled specifications
yawl_compiled:get_net_list()         % List all nets
yawl_compiled:get_net_info(NetId)    % Get net info
yawl_compiled:get_net_module(NetId)  % Get compiled module
```

### Workflow Utilities

```erlang
% Audit logging
wf_audit_log:log_receipt(Receipt)    % Log receipt
wf_audit_log:get_receipts(CaseId)    % Get receipts
wf_audit_log:audit_summary(CaseId)   % Get summary

% State management
wf_store:save_state(CaseId, State)   % Save state
wf_store:load_state(CaseId)         % Load state
wf_store:delete_state(CaseId)       % Delete state

% Time management
wf_time:set_deadline(CaseId, Timeout) % Set deadline
wf_time:check_deadline(CaseId)      % Check deadline
wf_time:parse_duration("PT30M")      % Parse ISO duration

% Timer management
wf_timer:start_timer(CaseId, Timeout, Event)  % Start timer
wf_timer:cancel_timer(TimerRef)      % Cancel timer
wf_timer:active_timers()             % Get active timers

% Rules engine
wf_rules:load_rules(Rules)          % Load rules
wf_rules:evaluate_rule(Rule, Context) % Evaluate rule
wf_rules:validate_rules(Rules)       % Validate rules

% Multi-instance tasks
wf_mi:create_mi_task(Config)        % Create MI task
wf_mi:execute_parallel(Task, Contexts)  % Execute parallel
wf_mi:execute_sequential(Task, Contexts) % Execute sequential
wf_mi:execute_n_of_m(Task, N, M, Contexts) % N-of-M execution

% Process monitoring
wf_ops:process_stats(Pid)           % Process statistics
wf_ops:memory_stats(Pid)            % Memory statistics
wf_ops:health_check(Pid)            % Health check

% Property testing
wf_prop:generate_property(Type)      % Generate property
wf_prop:run_property_test(Property)  % Run property test
wf_prop:generate_test_cases(Property) % Generate test cases
```

## Key Data Structures

### Records

```erlang
% Transition record
-record(transition, {
    id :: atom(),           % Unique identifier
    name :: binary(),       % Human-readable name
    type :: atomic | composite | tool | manual,
    preset :: [place()],    % Input places
    postset :: [place()],   % Output places
    conditions :: [condition()]
}).

% Place record
-record(place, {
    id :: atom(),           % Unique identifier
    name :: binary(),       % Human-readable name
    type :: input | output | normal,
    marking :: [term()]     % Current tokens
}).

% Receipt record
-record(receipt, {
    case_id :: binary(),    % Workflow instance ID
    before_hash :: binary(),% Hash before execution
    after_hash :: binary(), % Hash after execution
    move :: term(),         % Transition fired
    ts :: integer(),        % Timestamp
    usr_info :: usr_info()  % User information
}).

% User information record
-record(usr_info, {
    user_id :: binary(),   % User identifier
    case_id :: binary(),   % Workflow case ID
    timestamp :: integer(), % Creation time
    data :: map()          % Additional data
}).
```

## Common Patterns

### Basic Sequential Workflow

```erlang
% Define places
places() -> [start, task1, task2, end].

% Define transitions
transitions() -> [t1, t2].

% Input places for transitions
preset(t1) -> [start];
preset(t2) -> [task1].

% Initial marking
init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

% Check if transition is enabled
is_enabled(t1, #{start := [init]}, _UsrInfo) -> true;
is_enabled(t2, #{task1 := [done]}, _UsrInfo) -> true.

% Execute transition
fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{task1 => [done]}};
fire(t2, #{task1 := []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.
```

### Conditional Workflow

```erlang
% Enable based on condition
is_enabled(t_approve, Mode, UsrInfo) ->
    case get_order_amount(Mode) of
        Amount when Amount > 1000 -> needs_manager_approval(UsrInfo);
        _ -> true
    end.

% Execute with condition
fire(t_approve, Mode, UsrInfo) ->
    case wf_rules:evaluate_rule(approval_rule, build_context(Mode)) of
        true -> {produce, #{approved => [yes]}};
        false -> {produce, #{approved => [no]}}
    end.
```

### Parallel Execution

```erlang
% Enable multiple transitions
is_enabled(t1, #{start := [init]}, _UsrInfo) -> true;
is_enabled(t2, #{start := [init]}, _UsrInfo) -> true;
is_enabled(t3, #{start := [init]}, _UsrInfo) -> true.

% Execute in parallel
fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{task1 => [done]}};
fire(t2, #{start := []}, _UsrInfo) ->
    {produce, #{task2 => [done]}};
fire(t3, #{start := []}, _UsrInfo) ->
    {produce, #{task3 => [done]}}.
```

## Configuration

### Environment Variables

```bash
# Workflow configuration
export CRE_MAX_CONCURRENT=1000
export CRE_TTL=3600000
export CRE_TIMEOUT=30000

# Network configuration
export CRE_NODE=cre@localhost
export CRE_COOKIE=secret

# HTTP service
export CRE_PORT=4142
export CRE_HTTP=true
```

### Application Configuration

```erlang
% In sys.config
[
    {cre, [
        {max_concurrent, 1000},
        {ttl, 3600000},
        {timeout, 30000},
        {http_port, 4142},
        {http, true}
    ]}
].
```

## Testing Commands

### Unit Tests

```bash
# Run all EUnit tests
rebar3 eunit

# Run specific test
rebar3 eunit -m yawl_of_helpers_test

# Run with verbose output
rebar3 eunit -v
```

### Integration Tests

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

### Property Tests

```bash
# Run property tests
rebar3 eunit -m wf_prop_test

# Generate test cases
cd src && erl -pa ../_build/test/lib/*/ebin
1> wf_prop:generate_test_cases().
```

## Troubleshooting

### Common Issues

```erlang
% Debug workflow state
{ok, State} = gen_pnet:state(Pid),
io:format("State: ~p~n", [State]).

% Check enabled transitions
Enabled = gen_pnet:enabled(Pid),
io:format("Enabled: ~p~n", [Enabled]).

% Check marking
{ok, Marking} = gen_pnet:marking(Pid),
io:format("Marking: ~p~n", [Marking]).
```

### Debug Commands

```bash
# Start with debug logging
rebar3 shell

# Enable tracing
gen_pnet:trace(Pid, true)

# Get statistics
Stats = gen_pnet:stats(Pid),
io:format("Stats: ~p~n", [Stats]).
```

## Performance

### Performance Commands

```bash
% Run performance benchmarks
rebar3 ct -c test/yawl_performance_SUITE

% Generate performance report
rebar3 cover --verbose

% Profile memory usage
{memory, Memory} = process_info(self(), memory),
io:format("Memory: ~p KB~n", [Memory]).
```

### Optimization Tips

```erlang
% Use compiled modules for better performance
{ok, CompiledModule} = yawl_compile:compile(Spec).

% Use token injection for bulk operations
Tokens = [token1, token2, token3],
gen_pnet:inject(Pid, Tokens).

% Enable observer for monitoring
observer:start().
```

## Commands Summary

| Category | Command | Description |
|----------|---------|-------------|
| **Workflow** | `gen_pnet:start_link/3` | Start workflow |
| | `gen_pnet:step/1` | Execute step |
| | `gen_pnet:marking/1` | Check state |
| | `gen_pnet:stop/1` | Stop workflow |
| **Compilation** | `yawl_compile:compile/2` | Compile YAWL |
| | `yawl_compile:compile_to_file/3` | Compile to file |
| | `yawl_compiled:get_net_list/0` | List nets |
| **Utilities** | `wf_audit_log:log_receipt/1` | Log receipt |
| | `wf_store:save_state/2` | Save state |
| | `wf_timer:start_timer/3` | Start timer |
| | `wf_rules:evaluate_rule/2` | Evaluate rule |
| **Testing** | `rebar3 eunit` | Run unit tests |
| | `rebar3 ct` | Run integration tests |
| | `rebar3 cover --export` | Generate coverage |
| **Performance** | `rebar3 ct -c test/yawl_performance_SUITE` | Run benchmarks |
| | `gen_pnet:stats/1` | Get statistics |

## Quick Examples

### Simple Order Processing

```erlang
% Define order workflow
OrderSpec = wf_spec:create_order_fulfillment(),
{ok, OrderNet} = yawl_compile:compile(OrderSpec).

% Start order
{ok, OrderPid} = gen_pnet:start_link(OrderNet, #{order_id => "12345"}, []).

% Process order
{ok, Receipt} = gen_pnet:step(OrderPid).

% Check status
{ok, Marking} = gen_pnet:marking(OrderPid).
```

### Concurrent Task Execution

```erlang
% Define parallel workflow
ParallelSpec = create_parallel_workflow(),
{ok, ParallelNet} = yawl_compile:compile(ParallelSpec).

% Start workflow
{ok, ParallelPid} = gen_pnet:start_link(ParallelNet, #{}, []).

% Execute concurrently
{ok, Receipts} = gen_pnet:concurrent_step(ParallelPid).

% Process results
lists:foreach(fun(Receipt) ->
    io:format("Completed: ~p~n", [Receipt#receipt.move])
end, Receipts).
```

### State Management

```erlang
% Save workflow state
wf_store:save_state(<<"case123">>, #{data => my_data}).

% Load workflow state
{ok, State} = wf_store:load_state(<<"case123">>).

% Use state in workflow
fire(Task, Mode, UsrInfo) ->
    State = wf_store:load_state(UsrInfo#usr_info.case_id),
    {produce, update_state(State)}.
```

This quick reference provides the essential commands and patterns needed to work with the YAWL workflow engine. For detailed documentation, refer to the complete API reference and user guides.