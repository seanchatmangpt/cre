# CRE Workflow Examples

This directory contains example workflow demonstrations using gen_yawl (YAWL workflow engine).

## Running Examples

First ensure CRE is compiled:

```bash
cd /Users/sac/cre
rebar3 compile
```

Then start an Erlang shell:

```bash
rebar3 shell
```

## Simple Examples

### hello_world.erl

The simplest possible workflow demonstrating basic gen_yawl behavior.

**Workflow:** Single transition from start to end.

**Run:**
```erlang
1> {ok, Pid} = hello_world:start_link().
1> hello_world:run(Pid).
  [hello_world] Starting workflow
  [hello_world] t_greet fired: Hello, World!
  [hello_world] Workflow completed
  ok
1> hello_world:stop(Pid).
```

**Pattern:** WCP-01 (Sequence)

---

### data_processing.erl

Multi-step data processing pipeline with validation, transformation, and aggregation.

**Workflow:** Input -> Validate -> Transform -> Aggregate -> Output

**Run:**
```erlang
1> {ok, Pid} = data_processing:start_link().
1> data_processing:process(Pid, [
    #{id => 1, name => <<"item1">>, value => 10},
    #{id => 2, name => <<"item2">>, value => 20},
    #{id => 3, name => <<"item3">>, value => 30}
  ]).
  [data_processing] Validating 3 items
  [data_processing] Transforming item1: value 10 -> 20.0
  [data_processing] Transforming item2: value 20 -> 40.0
  [data_processing] Transforming item3: value 30 -> 60.0
  [data_processing] Aggregation complete. Total: 120.0
  {ok, #{total => 120.0, count => 3}}
1> data_processing:stop(Pid).
```

**Patterns:**
- WCP-01: Sequence
- WDP-01: Parameter Pass
- Data transformation pipeline

---

### parallel_tasks.erl

Demonstrates parallel execution of multiple independent tasks with synchronization.

**Workflow:** Split -> [Task1, Task2, Task3, Task4] -> Join -> Complete

**Run:**
```erlang
1> {ok, Pid} = parallel_tasks:start_link().
1> parallel_tasks:run_parallel(Pid, [
    fun() -> timer:sleep(100), {task_a, 2 * 10} end,
    fun() -> timer:sleep(150), {task_b, 3 * 10} end,
    fun() -> timer:sleep(200), {task_c, 4 * 10} end,
    fun() -> timer:sleep(50), {task_d, 5 * 10} end
  ]).
  [parallel_tasks] Starting 4 parallel tasks
  [parallel_tasks] Task A completed: 20
  [parallel_tasks] Task B completed: 30
  [parallel_tasks] Task C completed: 40
  [parallel_tasks] Task D completed: 50
  [parallel_tasks] All 4 tasks completed
  {ok, #{task_a => 20, task_b => 30, task_c => 40, task_d => 50}}
1> parallel_tasks:stop(Pid).
```

**Patterns:**
- WCP-02: Parallel Split
- WCP-03: Synchronization

---

### human_interaction.erl

Human-in-the-loop workflow with approval waiting and timeout handling.

**Workflow:** Request -> [Wait Approval] --timeout--> [Auto Reject/Complete]

**Run:**
```erlang
1> {ok, Pid} = human_interaction:start_link().
1> {ok, Ref} = human_interaction:submit_request(Pid, #{
    type => vacation,
    employee => <<"Alice">>,
    days => 5
  }).
1> %% Simulate manager approval:
1> human_interaction:approve(Pid, Ref, approved).
1> %% Or simulate rejection:
1> human_interaction:approve(Pid, Ref, rejected).
1> %% Or wait for timeout:
1> human_interaction:wait_result(Pid, Ref, 10000).
  {approved, <<"Alice">>}  % approved
  {rejected, <<"Alice">>}  % rejected
  {timeout, #{}}            % timeout after configured period
1> human_interaction:stop(Pid).
```

**Patterns:**
- User task pattern (external approval)
- Timeout handling
- Result aggregation

---

### conditional_flow.erl

Conditional routing based on customer type (premium, standard, basic).

**Workflow:** Input -> [Route based on type] -> [Handle branch] -> Merge -> Output

**Run:**
```erlang
1> {ok, Pid} = conditional_flow:start_link().
1> %% Premium customer:
1> conditional_flow:process(Pid, #{type => premium, amount => 100}).
  {ok, #{route => premium, discount => 0.2, total => 80.0}}
1> %% Standard customer:
1> conditional_flow:process(Pid, #{type => standard, amount => 100}).
  {ok, #{route => standard, discount => 0.1, total => 90.0}}
1> %% Basic customer:
1> conditional_flow:process(Pid, #{type => basic, amount => 100}).
  {ok, #{route => basic, discount => 0.0, total => 100.0}}
1> %% Invalid customer:
1> conditional_flow:process(Pid, #{type => unknown, amount => 100}).
  {ok, #{route => error, error => invalid_customer_type}}
1> conditional_flow:stop(Pid).
```

**Patterns:**
- WCP-04: Exclusive Choice
- Data-driven routing
- Multiple branch handling with merge

---

### loop_example.erl

Iterative processing with repeat-until condition and accumulator patterns.

**Features:**
- `process_until/2` - Process until condition is met
- `process_fold/3` - Fold accumulator across iterations
- `process_filter/3` - Filter list during iteration

**Run:**
```erlang
1> {ok, Pid} = loop_example:start_link().
1> %% Process until threshold:
1> loop_example:process_until(Pid, fun(X) -> X >= 100 end).
  {ok, #{iterations => 10, final_value => 100}}
1> %% Process with accumulator:
1> loop_example:process_fold(Pid, 0, fun(X, Acc) -> Acc + X end).
  {ok, #{iterations => 10, sum => 55}}
1> %% Process with filter:
1> loop_example:process_filter(Pid, [1,2,3,4,5,6,7,8,9,10], fun(X) -> X rem 2 =:= 0 end).
  {ok, #{iterations => 5, sum => 30, filtered => [2,4,6,8,10]}}
1> loop_example:stop(Pid).
```

**Patterns:**
- WCP-10: Arbitrary Cycles

---

### error_handling.erl

Multi-instance pattern demonstrations with fault tolerance.

**Features:**
- `process_static/2` - Fixed N instances with synchronization
- `process_dynamic/3` - Runtime-determined N instances
- `process_no_sync/2` - Fire-and-forget instances

**Run:**
```erlang
1> {ok, Pid} = error_handling:start_link().
1> %% Static instances (design time):
1> error_handling:process_static(Pid, [
    fun(N) -> N * 2 end,
    fun(N) -> N * 3 end,
    fun(N) -> N * 4 end
  ]).
  {ok, #{total => 90, instances => 3}}
1> %% Dynamic instances (runtime):
1> Data = [10, 20, 30, 40, 50],
1> CountFun = fun(L) -> length(L) end,
1> error_handling:process_dynamic(Pid, Data, CountFun).
  {ok, #{total => 150, instances => 5}}
1> %% No synchronization:
1> error_handling:process_no_sync(Pid, [
    fun(N) -> timer:sleep(100), N end,
    fun(N) -> timer:sleep(150), N * 2 end,
    fun(N) -> timer:sleep(200), N * 3 end
  ]).
  {ok, #{instances => 3}}
1> error_handling:stop(Pid).
```

**Patterns:**
- WCP-13: Multiple Instances with Design Time Knowledge
- WCP-14: Multiple Instances with Runtime Knowledge
- WCP-15: Multiple Instances without Synchronization

---

## Integration Examples

### rest_api_example.erl

REST API integration with Cowboy HTTP server for workflow management.

**Endpoints:**
- `POST /api/workflow` - Start a new workflow
- `GET /api/workflow` - List all workflows
- `GET /api/workflow/:id` - Get workflow status
- `GET /api/workflow/:id/result` - Get workflow result

**Run:**
```erlang
1> {ok, _Pid} = rest_api_example:start_link().
%% Server running on http://localhost:8080
%% Use curl to interact:
%% curl -X POST http://localhost:8080/api/workflow -H "Content-Type: application/json" -d '{"type": "data_processing", "input": [10, 20, 30]}'
```

**Note:** Requires cowboy dependency for full REST server functionality.

---

### custom_pattern_example.erl

Reusable pattern building blocks for creating custom workflows.

**Available Pattern Constructors:**
- `pipeline(Steps)` - Sequential execution pipeline
- `fanout(Branches)` - Parallel fan-out
- `fanin(JoinStrategy)` - Fan-in with join
- `router(Routes)` - Conditional routing
- `retry(MaxAttempts, Backoff)` - Retry with exponential backoff
- `batch(Size, Window)` - Batched processing
- `throttle(Rate)` - Rate-limited processing

**Run:**
```erlang
1> {ok, Pid} = custom_pattern_example:start_link().
1> Pipeline = custom_pattern_example:pipeline([
    fun(X) -> X * 2 end,
    fun(X) -> X + 10 end,
    fun(X) -> X div 2 end
  ]).
1> {ok, Pid2} = Pipeline:start_link().
1> Pipeline:run(Pid2, 100).
  {ok, [result, 110, 55]}
1> custom_pattern_example:stop(Pid).
```

**Pattern:** Custom composition from reusable building blocks

---

## Common Patterns Reference

| Pattern ID | Name | Example |
|-------------|------|---------|
| WCP-01 | Sequence | hello_world.erl |
| WCP-02 | Parallel Split | parallel_tasks.erl |
| WCP-03 | Synchronization | parallel_tasks.erl |
| WCP-04 | Exclusive Choice | conditional_flow.erl |
| WCP-10 | Arbitrary Cycles | loop_example.erl |
| WCP-13 | MI (Static) | error_handling.erl |
| WCP-14 | MI (Dynamic) | error_handling.erl |
| WCP-15 | MI (No Sync) | error_handling.erl |

## Pattern Libraries

The `src/patterns/` directory contains pattern implementations:

- `sequence.erl` - Sequential execution
- `parallel_split.erl` - Parallel branching
- `exclusive_choice.erl` - Conditional routing
- `multi_instance.erl` - Multiple instance patterns
- `synchronization.erl` - Join patterns

## Testing

Run individual example tests:

```bash
# Test a specific example
rebar3 eunit --module=hello_world
```

Or run from Erlang shell:

```erlang
# Compile and run
1> c(hello_world).
1> hello_world:start_link().
```

## Key Concepts

### Places
Nodes in the Petri net where tokens reside. Represent workflow state.

### Transitions
Actions that consume tokens from input places and produce tokens to output places. Represent workflow steps.

### Tokens
Data units that flow through the net. Can be any Erlang term.

### Modes
The current marking (tokens in each place) that enables a transition to fire.

### User Info (usr_info)
Custom state passed along with transitions using the 3-tuple return form:
```erlang
{produce, ProduceMap, NewUsrInfo}
```

This allows workflows to maintain state across transition firings.

## Best Practices

1. **Always initialize usr_info** in your `init/1` callback
2. **Use the 3-tuple return** to update state atomically
3. **Handle all cases in fire/3** - return `abort` for unhandled cases
4. **Implement trigger/3** to process tokens at output places
5. **Use descriptive logging** via `logger` module (not `io:format`)
6. **Handle timeout** in API calls with appropriate fallbacks
7. **Validate input** before starting workflows
8. **Clean up resources** in `terminate/2` callback

## Telemetry Integration

Examples can integrate with telemetry:

```erlang
%% Include telemetry headers
-include_lib("kernel/include/logger.hrl").

%% Log workflow events
logger:info(Workflow ~s step ~p completed", [WorkflowId, StepId]),

%% Log with structured data
logger:info("Processing item",
    #{workflow => WorkflowId, item => ItemId, value => Value}).
```

See `src/telemetry/` for telemetry backends.
