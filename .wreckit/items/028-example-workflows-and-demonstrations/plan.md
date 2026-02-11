# Example Workflows and Demonstrations Implementation Plan

## Implementation Plan Title
Create 4 Progressive CRE Workflow Examples with Runnable Demos, Trace Documentation, and Verification Tests

## Overview

This implementation creates **4 comprehensive, runnable workflow examples** that demonstrate CRE's capabilities from basic to advanced usage patterns. Each example includes a complete implementation module, demo script, expected trace documentation, and verification tests.

**Current State:**
- Documentation (`docs/EXAMPLES.md:11-13`) claims examples exist at `examples/workflows/` but directory is empty
- All 43 YAWL patterns implemented in `src/patterns/` but not exposed as user-facing examples
- Test demos exist (`test/yawl_of_demo.erl`, `test/demo_runner.erl`) but not user-friendly
- Tracing system (`ln_trace.erl`) complete but not demonstrated in examples
- Tutorials reference examples that don't exist

**Desired End State:**
- 4 progressive examples demonstrating: basic sequence, parallel sync, human approval, order fulfillment
- Each example has runnable module (`examples/example_*.erl`) with comprehensive documentation
- Demo scripts (`examples/*_demo.sh`) for easy execution
- Expected trace output (`examples/*_trace.md`) documenting execution flow
- Trace verification tests (`test/*_trace_test.erl`) ensuring correctness
- Documentation integrated with tutorials and navigation hub

**Key Discoveries:**
- `src/ln_trace.erl:30-40` defines 11 event types for structured tracing
- `src/patterns/sequence.erl:1-67` shows minimal gen_yawl implementation pattern
- `test/yawl_of_demo.erl:82-100` provides demo entry point pattern with options map
- `test/yawl_of_demo.erl:242-276` shows result formatting and printing pattern
- `src/patterns/parallel_split.erl:22-97` demonstrates comprehensive moduledoc pattern
- `rebar.config:4-24` shows `src/patterns` is in source directories
- Pattern infrastructure complete but examples directory empty

## What We're NOT Doing

- **NOT modifying existing pattern implementations** - patterns are complete and working
- **NOT creating new YAWL patterns** - all 43 already implemented
- **NOT changing the tracing system** - `ln_trace` is feature-complete
- **NOT implementing the full order fulfillment from scratch** - will extract from `test/yawl_of_demo.erl`
- **NOT building complex external integrations** - examples use mocks/stubs for external deps
- **NOT adding new dependencies** - using existing gen_pnet, gen_yawl, ln_trace
- **NOT implementing advanced features** - focusing on core workflow patterns
- **NOT creating interactive tutorials** - examples are standalone with documentation
- **NOT implementing all error scenarios** - only basic error handling in Example 1

## Implementation Approach

**High-Level Strategy:**
Create 4 progressive examples (basic → advanced), each independently testable. Use existing infrastructure (gen_pnet/gen_yawl, ln_trace) with minimal new code. Follow established patterns from `test/yawl_of_demo.erl` and `src/patterns/`.

**Decision Points:**
1. **Use gen_pnet for Examples 1-2** (simplicity) and **gen_yawl for Examples 3-4** (realism with XES logging)
2. **Create new `examples/` directory** (standard location, currently empty)
3. **Test trace structure, not exact sequence** (avoids brittleness from non-determinism)
4. **Mock external dependencies** (Claude API for approval, payment gateways)
5. **Use fixed random seeds** for deterministic trace output

---

## Phases

### Phase 1: Basic Sequence Example (Example 1)

#### Overview
Create the "Hello World" of CRE workflows - a simple 3-step sequential process demonstrating the fundamentals of workflow definition, execution, and tracing.

**Patterns:** WCP-01 (Sequence)
**Use Case:** Document processing workflow (Receive → Validate → Store)
**Complexity:** Beginner

#### Changes Required:

##### 1. Create `examples/example_1_basic_sequence.erl`
**File:** `examples/example_1_basic_sequence.erl`
**Changes:** New module implementing 3-step sequential workflow

```erlang
%% -*- erlang -*-
%%% @doc Example 1: Basic Sequential Workflow
%%%
%%% This example demonstrates the simplest workflow pattern - sequential
%%% execution of three tasks. It serves as the "Hello World" of CRE workflows.
%%%
%%% == Workflow Steps ==
%%%
%%% 1. Receive document - Accept incoming document
%%% 2. Validate document - Check document validity
%%% 3. Store document - Persist validated document
%%%
%%% == Usage ==
%%%
%%% Run the example:
%%% <pre>
%%% > example_1_basic_sequence:run().
%%% </pre>
%%%
%%% == Expected Trace ==
%%%
%%% The workflow emits the following trace events:
%%% - `case_started` - Workflow execution begins
%%% - `step_started` (receive) - Document receipt starts
%%% - `step_completed` (receive) - Document receipt completes
%%% - `step_started` (validate) - Validation starts
%%% - `step_completed` (validate) - Validation completes
%%% - `step_started` (store) - Storage starts
%%% - `step_completed` (store) - Storage completes
%%% - `case_completed` - Workflow finishes
%%%
%%% @end

-module(example_1_basic_sequence).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([new/0, start/0, run/0, get_trace/1, print_trace/1]).

%% Types
-type document() :: #{
    id := binary(),
    content := binary(),
    status := pending | validated | stored
}.
-type workflow_state() :: #{
    document => document(),
    trace => ln_trace:state()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create a new workflow instance.
-spec new() -> workflow_state().
new() ->
    #{
        document => #{
            id => <<"DOC-001">>,
            content => <<"Example document content">>,
            status => pending
        },
        trace => ln_trace:new()
    }.

%% @doc Start the workflow asynchronously.
-spec start() -> {ok, pid()}.
start() ->
    gen_pnet:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run the workflow synchronously and return result.
-spec run() -> #{status := complete, steps := non_neg_integer(), trace => [ln_trace:event()]}.
run() ->
    State = new(),
    #{trace := Trace} = execute_workflow(State),
    Events = ln_trace:get_all(Trace),
    #{
        status => complete,
        steps => length(Events),
        trace => Events
    }.

%% @doc Get trace events from workflow state.
-spec get_trace(workflow_state()) -> [ln_trace:event()].
get_trace(#{trace := Trace}) ->
    ln_trace:get_all(Trace).

%% @doc Print trace events in human-readable format.
-spec print_trace(workflow_state()) -> ok.
print_trace(State) ->
    Events = get_trace(State),
    io:format("=== Example 1: Basic Sequence Trace ===~n"),
    lists:foreach(fun print_event/1, Events),
    io:format("~nTotal events: ~p~n", [length(Events)]),
    ok.

%%%===================================================================
%%% gen_pnet Callbacks
%%%===================================================================

place_lst() ->
    [p_start, p_receive, p_validate, p_store, p_end].

trsn_lst() ->
    [t_receive, t_validate, t_store, t_complete].

init_marking(p_start, _UsrInfo) -> [start];
init_marking(_Place, _UsrInfo) -> [].

preset(t_receive) -> [p_start];
preset(t_validate) -> [p_receive];
preset(t_store) -> [p_validate];
preset(t_complete) -> [p_store];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_receive, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate document receipt
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => receive, result => document_received}
    }, Trace1),
    {produce, #{p_receive => [token]}, State#{trace => Trace2}};

fire(t_validate, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate validation
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => validate, result => valid}
    }, Trace1),
    {produce, #{p_validate => [token]}, State#{trace => Trace2}};

fire(t_store, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate storage
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => store, result => stored}
    }, Trace1),
    {produce, #{p_store => [token]}, State#{trace => Trace2}};

fire(t_complete, _Mode, State) ->
    Trace1 = ln_trace:emit(case_completed, maps:get(trace, State)),
    {produce, #{p_end => [done]}, State#{trace => Trace1}};

fire(_Trsn, _Mode, State) ->
    abort.

init([]) ->
    State0 = new(),
    Trace0 = maps:get(trace, State0),
    Trace1 = ln_trace:emit(case_started, Trace0),
    State0#{trace => Trace1}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Execute workflow steps synchronously (simplified).
execute_workflow(State) ->
    %% Fire transitions in sequence
    {_, State1} = fire(t_receive, undefined, State),
    {_, State2} = fire(t_validate, undefined, State1),
    {_, State3} = fire(t_store, undefined, State2),
    {_, State4} = fire(t_complete, undefined, State3),
    State4.

%% @doc Print a single trace event.
print_event(#{type := Type, seq := Seq, data := Data}) ->
    io:format("[~p] ~p: ~p~n", [Seq, Type, Data]).
```

##### 2. Create `examples/example_1_basic_sequence_demo.sh`
**File:** `examples/example_1_basic_sequence_demo.sh`
**Changes:** New shell script to compile and run demo

```bash
#!/bin/bash
# Demo script for Example 1: Basic Sequential Workflow

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CRE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== Example 1: Basic Sequential Workflow Demo ==="
echo ""

# Compile the example
echo "Compiling example module..."
cd "$CRE_ROOT"
rebar3 compile

# Create a temporary Erlang script to run the demo
cat > /tmp/example_1_demo.erl << 'EOF'
#!/usr/bin/env escript
main(_) ->
    code:add_patha("_build/default/lib/cre/ebin"),
    code:add_patha("_build/default/lib/gen_pnet/ebin"),

    io:format("~nRunning Example 1: Basic Sequence~n"),
    io:format("======================================~n~n"),

    Result = example_1_basic_sequence:run(),

    io:format("~nWorkflow completed successfully!~n"),
    io:format("Status: ~p~n", [maps:get(status, Result)]),
    io:format("Total steps: ~p~n", [maps:get(steps, Result)]),
    io:format("Trace events: ~p~n", [length(maps:get(trace, Result, []))]),

    ok.
EOF

# Run the demo
echo ""
echo "Executing workflow..."
erlc -I include -o /tmp /tmp/example_1_demo.erl
erl -pa _build/default/lib/cre/ebin -pa _build/default/lib/gen_pnet/ebin -pa /tmp -noshell -s example_1_demo main -s init stop

echo ""
echo "=== Demo Complete ==="
```

##### 3. Create `examples/example_1_basic_sequence_trace.md`
**File:** `examples/example_1_basic_sequence_trace.md`
**Changes:** Document expected trace structure

````markdown
# Example 1: Basic Sequence - Expected Trace

## Overview

This document describes the expected trace output when running the Basic Sequence workflow example.

## Expected Event Sequence

The workflow emits the following trace events in order:

### 1. case_started
- **Type:** `case_started`
- **Data:** `{}`
- **Description:** Workflow execution begins

### 2. step_started (receive)
- **Type:** `step_started`
- **Data:** `#{step => receive}`
- **Description:** Document receipt step starts

### 3. step_completed (receive)
- **Type:** `step_completed`
- **Data:** `#{step => receive, result => document_received}`
- **Description:** Document received successfully

### 4. step_started (validate)
- **Type:** `step_started`
- **Data:** `#{step => validate}`
- **Description:** Document validation starts

### 5. step_completed (validate)
- **Type:** `step_completed`
- **Data:** `#{step => validate, result => valid}`
- **Description:** Document validated successfully

### 6. step_started (store)
- **Type:** `step_started`
- **Data:** `#{step => store}`
- **Description:** Document storage starts

### 7. step_completed (store)
- **Type:** `step_completed`
- **Data:** `#{step => store, result => stored}`
- **Description:** Document stored successfully

### 8. case_completed
- **Type:** `case_completed`
- **Data:** `{}`
- **Description:** Workflow execution completes

## Sample Output

```erlang
[0] case_started: #{}
[1] step_started: #{step => receive}
[2] step_completed: #{step => receive, result => document_received}
[3] step_started: #{step => validate}
[4] step_completed: #{step => validate, result => valid}
[5] step_started: #{step => store}
[6] step_completed: #{step => store, result => stored}
[7] case_completed: #{}

Total events: 8
```

## Verification

To verify the trace structure:

1. Run `example_1_basic_sequence:run()`
2. Check that 8 events are emitted
3. Verify event types match the sequence above
4. Verify final event is `case_completed`

The test suite (`test/example_1_trace_test.erl`) verifies this structure automatically.
````

##### 4. Create `test/example_1_trace_test.erl`
**File:** `test/example_1_trace_test.erl`
**Changes:** New test module verifying trace structure

```erlang
%% -*- erlang -*-
%%% @doc Trace verification tests for Example 1: Basic Sequence

-module(example_1_trace_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that example module compiles and runs
basic_execution_test() ->
    Result = example_1_basic_sequence:run(),
    ?assertEqual(complete, maps:get(status, Result)),
    ?assert(maps:get(steps, Result) > 0).

%% Test trace structure
trace_structure_test() ->
    Result = example_1_basic_sequence:run(),
    Trace = maps:get(trace, Result),

    %% Should have 8 events total
    ?assertEqual(8, length(Trace)),

    %% First event should be case_started
    [First | _] = Trace,
    ?assertEqual(case_started, maps:get(type, First)),

    %% Last event should be case_completed
    Last = lists:last(Trace),
    ?assertEqual(case_completed, maps:get(type, Last)).

%% Test event sequence (relaxed - only check structure, not exact order)
event_sequence_test() ->
    Result = example_1_basic_sequence:run(),
    Trace = maps:get(trace, Result),

    %% Count event types
    EventTypes = [maps:get(type, E) || E <- Trace],

    %% Should have 1 case_started
    ?assertEqual(1, lists:count(fun(T) -> T =:= case_started end, EventTypes)),

    %% Should have 1 case_completed
    ?assertEqual(1, lists:count(fun(T) -> T =:= case_completed end, EventTypes)),

    %% Should have 3 step_started events
    ?assertEqual(3, lists:count(fun(T) -> T =:= step_started end, EventTypes)),

    %% Should have 3 step_completed events
    ?assertEqual(3, lists:count(fun(T) -> T =:= step_completed end, EventTypes)).

%% Test that demo script exists and is executable
demo_script_test() ->
    case file:read_file_info("examples/example_1_basic_sequence_demo.sh") of
        {ok, _} ->
            ?assert(true);
        _ ->
            ?assert(false, "Demo script not found")
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles without errors: `rebar3 compile`
- [ ] Unit tests pass: `rebar3 eunit --module example_1_trace_test`
- [ ] Demo script runs: `./examples/example_1_basic_sequence_demo.sh`
- [ ] Trace structure verified by tests

##### Manual Verification:
- [ ] Demo runs successfully and outputs trace events
- [ ] Trace matches expected structure in `example_1_basic_sequence_trace.md`
- [ ] Documentation is clear and complete
- [ ] No regressions in existing tests

**Note:** Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Parallel + Synchronization Example (Example 2)

#### Overview
Create a workflow demonstrating parallel execution and synchronization using WCP-02 (Parallel Split) and WCP-03 (Synchronization) patterns.

**Patterns:** WCP-02 (Parallel Split), WCP-03 (Synchronization)
**Use Case:** Loan application verification (credit check + background check in parallel)
**Complexity:** Intermediate

#### Changes Required:

##### 1. Create `examples/example_2_parallel_sync.erl`
**File:** `examples/example_2_parallel_sync.erl`
**Changes:** New module implementing parallel verification workflow

```erlang
%% -*- erlang -*-
%%% @doc Example 2: Parallel Workflow with Synchronization
%%%
%%% This example demonstrates parallel execution and synchronization
%%% using the Parallel Split (WCP-02) and Synchronization (WCP-03) patterns.
%%%
%%% == Workflow Steps ==
%%%
%%% 1. Receive application - Accept loan application
%%% 2. Split into parallel branches:
%%%    - Credit check - Verify credit history
%%%    - Background check - Verify employment and references
%%% 3. Synchronize - Wait for both checks to complete
%%% 4. Make decision - Approve or deny based on results
%%%
%%% == Usage ==
%%%
%%% Run the example:
%%% <pre>
%%% > example_2_parallel_sync:run().
%%% </pre>
%%%
%%% @end

-module(example_2_parallel_sync).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([new/0, run/0, get_trace/1]).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    #{trace => ln_trace:new()}.

run() ->
    State = new(),
    execute_parallel_workflow(State),
    State.

get_trace(#{trace := Trace}) ->
    ln_trace:get_all(Trace).

%%%===================================================================
%%% gen_pnet Callbacks
%%%===================================================================

place_lst() ->
    [p_start, p_received, p_credit_check, p_background_check,
     p_credit_done, p_background_done, p_decision, p_end].

trsn_lst() ->
    [t_receive, t_split, t_credit_check, t_background_check,
     t_synchronize, t_decision, t_complete].

init_marking(p_start, _UsrInfo) -> [start];
init_marking(_Place, _UsrInfo) -> [].

preset(t_receive) -> [p_start];
preset(t_split) -> [p_received];
preset(t_credit_check) -> [p_credit_check];
preset(t_background_check) -> [p_background_check];
preset(t_synchronize) -> [p_credit_done, p_background_done];
preset(t_decision) -> [p_decision];
preset(t_complete) -> [p_decision];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_receive, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => receive_app}
    }, Trace1),
    {produce, #{p_received => [token]}, State#{trace => Trace2}};

fire(t_split, _Mode, State) ->
    Trace1 = ln_trace:emit(branch_chosen, maps:get(trace, State)),
    %% Split into two parallel branches
    {produce, #{
        p_credit_check => [token],
        p_background_check => [token]
    }, State#{trace => Trace1}};

fire(t_credit_check, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate credit check
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => credit_check, result => good_credit}
    }, Trace1),
    {produce, #{p_credit_done => [token]}, State#{trace => Trace2}};

fire(t_background_check, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate background check
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => background_check, result => clean}
    }, Trace1),
    {produce, #{p_background_done => [token]}, State#{trace => Trace2}};

fire(t_synchronize, _Mode, State) ->
    %% Both branches complete - synchronization point
    Trace1 = ln_trace:emit(join_waiting, maps:get(trace, State)),
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => synchronize, branches => 2}
    }, Trace1),
    {produce, #{p_decision => [token]}, State#{trace => Trace2}};

fire(t_decision, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Make approval decision
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => decision, result => approved}
    }, Trace1),
    {produce, #{p_decision => [token]}, State#{trace => Trace2}};

fire(t_complete, _Mode, State) ->
    Trace1 = ln_trace:emit(case_completed, maps:get(trace, State)),
    {produce, #{p_end => [done]}, State#{trace => Trace1}};

fire(_Trsn, _Mode, State) ->
    abort.

init([]) ->
    State0 = new(),
    Trace0 = maps:get(trace, State0),
    Trace1 = ln_trace:emit(case_started, Trace0),
    State0#{trace => Trace1}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

execute_parallel_workflow(State) ->
    %% Simulate parallel execution
    {_, State1} = fire(t_receive, undefined, State),
    {_, State2} = fire(t_split, undefined, State1),
    {_, State3} = fire(t_credit_check, undefined, State2),
    {_, State4} = fire(t_background_check, undefined, State3),
    {_, State5} = fire(t_synchronize, undefined, State4),
    {_, State6} = fire(t_decision, undefined, State5),
    {_, _State7} = fire(t_complete, undefined, State6),
    ok.
```

##### 2. Create demo script, trace docs, and test (similar to Example 1)
**Files:**
- `examples/example_2_parallel_sync_demo.sh`
- `examples/example_2_parallel_sync_trace.md`
- `test/example_2_trace_test.erl`

**Changes:** Follow same pattern as Example 1

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module example_2_trace_test`
- [ ] Demo script runs: `./examples/example_2_parallel_sync_demo.sh`
- [ ] Trace shows branch_chosen and join_waiting events

##### Manual Verification:
- [ ] Parallel execution demonstrated in trace
- [ ] Synchronization point visible
- [ ] Both branches complete before decision

**Note:** Complete verification before Phase 3.

---

### Phase 3: Human Approval Example (Example 3)

#### Overview
Create a workflow demonstrating human-in-the-loop approval with checkpoint system, using gen_yawl behavior for advanced features.

**Patterns:** Custom approval checkpoint, WHP-01 (Error Handler)
**Use Case:** Code deployment approval workflow
**Complexity:** Advanced

#### Changes Required:

##### 1. Create `examples/example_3_human_approval.erl`
**File:** `examples/example_3_human_approval.erl`
**Changes:** New module using gen_yawl behavior with approval integration

```erlang
%% -*- erlang -*-
%%% @doc Example 3: Human-in-the-Loop Approval Workflow
%%%
%%% This example demonstrates human approval checkpoints with
%%% simulated Claude LLM integration for automated decision making.
%%%
%%% == Workflow Steps ==
%%%
%%% 1. Compile code - Build application code
%%% 2. Create approval checkpoint - Request approval
%%% 3. Wait for approval (auto/human/simulated modes)
%%% 4. Deploy if approved - Deploy to production
%%% 5. Rollback if denied - Rollback changes
%%%
%%% == Usage ==
%%%
%%% Run with simulated approval:
%%% <pre>
%%% > example_3_human_approval:run(simulated).
%%% </pre>
%%%
%%% Run with auto-approval:
%%% <pre>
%%% > example_3_human_approval:run(auto).
%%% </pre>
%%%
%%% @end

-module(example_3_human_approval).
-behaviour(gen_yawl).

%% gen_yawl callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         trigger/3]).

%% API
-export([new/1, run/1, get_trace/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(ApprovalMode) ->
    #{
        approval_mode => ApprovalMode,
        trace => ln_trace:new()
    }.

run(ApprovalMode) ->
    State = new(ApprovalMode),
    execute_approval_workflow(State),
    State.

get_trace(#{trace := Trace}) ->
    ln_trace:get_all(Trace).

%%%===================================================================
%%% gen_yawl Callbacks
%%%===================================================================

place_lst() ->
    [p_start, p_compiled, p_approval_requested, p_approved,
     p_denied, p_deployed, p_rolled_back, p_end].

trsn_lst() ->
    [t_compile, t_request_approval, t_approve, t_deny,
     t_deploy, t_rollback, t_complete].

init_marking(p_start, _UsrInfo) -> [start];
init_marking(_Place, _UsrInfo) -> [].

preset(t_compile) -> [p_start];
preset(t_request_approval) -> [p_compiled];
preset(t_approve) -> [p_approval_requested];
preset(t_deny) -> [p_approval_requested];
preset(t_deploy) -> [p_approved];
preset(t_rollback) -> [p_denied];
preset(t_complete) -> [p_deployed, p_rolled_back];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_compile, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate compilation
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => compile, result => success}
    }, Trace1),
    {produce, #{p_compiled => [token]}, State#{trace => Trace2}};

fire(t_request_approval, _Mode, State) ->
    Mode = maps:get(approval_mode, State, simulated),
    Trace1 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => effect_requested,
        data => #{checkpoint => approval, mode => Mode}
    }, maps:get(trace, State)),

    %% Simulate approval decision based on mode
    Decision = case Mode of
        auto -> approve;
        simulated -> approve;  %% Could randomize
        human -> waiting_for_human
    end,

    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => effect_completed,
        data => #{decision => Decision}
    }, Trace1),

    %% Route to appropriate place
    case Decision of
        approve ->
            {produce, #{p_approved => [token]}, State#{trace => Trace2}};
        deny ->
            {produce, #{p_denied => [token]}, State#{trace => Trace2}};
        waiting_for_human ->
            %% In real implementation, would wait for human input
            {produce, #{p_approved => [token]}, State#{trace => Trace2}}
    end;

fire(t_approve, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => deploy}
    }, Trace1),
    {produce, #{p_deployed => [token]}, State#{trace => Trace2}};

fire(t_deny, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => rollback}
    }, Trace1),
    {produce, #{p_rolled_back => [token]}, State#{trace => Trace2}};

fire(t_deploy, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate deployment
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => deploy, result => deployed}
    }, Trace1),
    {produce, #{p_deployed => [token]}, State#{trace => Trace2}};

fire(t_rollback, _Mode, State) ->
    Trace1 = ln_trace:emit(step_started, maps:get(trace, State)),
    %% Simulate rollback
    Trace2 = ln_trace:emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => step_completed,
        data => #{step => rollback, result => rolled_back}
    }, Trace1),
    {produce, #{p_rolled_back => [token]}, State#{trace => Trace2}};

fire(t_complete, _Mode, State) ->
    Trace1 = ln_trace:emit(case_completed, maps:get(trace, State)),
    {produce, #{p_end => [done]}, State#{trace => Trace1}};

fire(_Trsn, _Mode, State) ->
    abort.

init([]) ->
    State0 = new(simulated),
    Trace0 = maps:get(trace, State0),
    Trace1 = ln_trace:emit(case_started, Trace0),
    State0#{trace => Trace1}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

execute_approval_workflow(State) ->
    {_, State1} = fire(t_compile, undefined, State),
    {_, State2} = fire(t_request_approval, undefined, State1),

    %% Determine which path was taken
    #{trace := Trace2} = State2,
    Events = ln_trace:get_all(Trace2),
    LastDecision = find_last_decision(Events),

    State3 = case LastDecision of
        #{data := #{decision := approve}} ->
            {_, S3} = fire(t_deploy, undefined, State2),
            S3;
        #{data := #{decision := deny}} ->
            {_, S3} = fire(t_rollback, undefined, State2),
            S3;
        _ ->
            State2
    end,

    {_, _State4} = fire(t_complete, undefined, State3),
    ok.

find_last_decision(Events) ->
    EffectEvents = [E || E <- Events, maps:get(type, E) =:= effect_completed],
    case EffectEvents of
        [] -> #{};
        [Last | _] -> Last
    end.
```

##### 2. Create supporting files
**Files:**
- `examples/example_3_human_approval_demo.sh`
- `examples/example_3_human_approval_trace.md`
- `test/example_3_trace_test.erl`

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module example_3_trace_test`
- [ ] Demo runs with simulated approval: `./examples/example_3_human_approval_demo.sh`
- [ ] Trace shows effect_requested and effect_completed events

##### Manual Verification:
- [ ] Approval checkpoint created
- [ ] Decision flow visible in trace
- [ ] Both approve and deny paths work

**Note:** Complete verification before Phase 4.

---

### Phase 4: Order Fulfillment Example (Example 4)

#### Overview
Extract and adapt the existing Order Fulfillment demo from `test/yawl_of_demo.erl` into a user-facing example demonstrating complex multi-pattern workflows.

**Patterns:** WCP-01, WCP-04, WCP-06, WCP-23, WHP-01, WCP-25, WCP-16, WCP-18
**Use Case:** E-commerce order processing (5 subprocesses)
**Complexity:** Expert

#### Changes Required:

##### 1. Extract from `test/yawl_of_demo.erl`
**File:** `examples/example_4_order_fulfillment.erl`
**Changes:** Create user-facing module adapted from test code

Reference `test/yawl_of_demo.erl:1-452` for structure. Simplify for demonstration:
- Keep 5 subprocess structure (Ordering, Carrier, Payment, Transit, Delivery)
- Add trace integration with `ln_trace`
- Provide demo entry point similar to `yawl_of_demo:run/0`

##### 2. Create supporting files
**Files:**
- `examples/example_4_order_fulfillment_demo.sh`
- `examples/example_4_order_fulfillment_trace.md` (comprehensive multi-page trace)
- `test/example_4_trace_test.erl`

**Changes:** Follow established patterns, adapt for complex workflow

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module example_4_trace_test`
- [ ] Demo runs: `./examples/example_4_order_fulfillment_demo.sh`
- [ ] Complex trace with ~50 events

##### Manual Verification:
- [ ] All 5 subprocesses execute
- [ ] Multiple patterns visible in trace
- [ ] Documentation explains complexity

**Note:** This is the final phase.

---

### Phase 5: Documentation Integration

#### Overview
Integrate examples with existing documentation and navigation structure.

#### Changes Required:

##### 1. Update `docs/EXAMPLES.md`
**File:** `docs/EXAMPLES.md`
**Changes:** Replace placeholder content with actual examples

```markdown
## Quick Start Examples

| Example | File | Patterns | Complexity |
|---------|------|----------|------------|
| Basic Sequence | `examples/example_1_basic_sequence.erl` | WCP-01 | Beginner |
| Parallel Sync | `examples/example_2_parallel_sync.erl` | WCP-02, WCP-03 | Intermediate |
| Human Approval | `examples/example_3_human_approval.erl` | Approval Checkpoint | Advanced |
| Order Fulfillment | `examples/example_4_order_fulfillment.erl` | 10+ patterns | Expert |

## Running Examples

Each example includes a demo script:

```bash
# Basic Sequence
./examples/example_1_basic_sequence_demo.sh

# Parallel + Sync
./examples/example_2_parallel_sync_demo.sh

# Human Approval
./examples/example_3_human_approval_demo.sh

# Order Fulfillment
./examples/example_4_order_fulfillment_demo.sh
```

## Trace Documentation

Each example includes detailed trace documentation:
- `examples/example_1_basic_sequence_trace.md`
- `examples/example_2_parallel_sync_trace.md`
- `examples/example_3_human_approval_trace.md`
- `examples/example_4_order_fulfillment_trace.md`
```

##### 2. Update `docs/START_HERE.md`
**File:** `docs/START_HERE.md:39`
**Changes:** Update examples reference from `../examples/` to specific files

##### 3. Create `examples/README.md`
**File:** `examples/README.md`
**Changes:** Quick start guide for examples

```markdown
# CRE Workflow Examples

This directory contains runnable workflow examples demonstrating CRE's capabilities.

## Quick Start

1. **Compile:** `rebar3 compile`
2. **Run any demo:** `./examples/example_N_name_demo.sh`
3. **View trace:** See corresponding `*_trace.md` file

## Examples Progression

| # | Example | Patterns | What You'll Learn |
|---|---------|----------|-------------------|
| 1 | Basic Sequence | WCP-01 | Workflow basics, tracing |
| 2 | Parallel Sync | WCP-02, WCP-03 | Parallel execution |
| 3 | Human Approval | Approval Checkpoint | Human-in-the-loop |
| 4 | Order Fulfillment | 10+ patterns | Complex workflows |

## For Each Example

- **Module:** `example_N_name.erl` - Complete implementation
- **Demo:** `example_N_name_demo.sh` - Runnable script
- **Trace:** `example_N_name_trace.md` - Expected trace documentation
- **Test:** `test/example_N_trace_test.erl` - Verification tests
```

#### Success Criteria:

##### Automated Verification:
- [ ] Documentation links work
- [ ] All demo scripts are executable
- [ ] No broken references

##### Manual Verification:
- [ ] Navigation from START_HERE.md works
- [ ] Examples README is clear
- [ ] Cross-references are accurate

---

## Testing Strategy

### Unit Tests:
- **Trace structure verification** - Each example has `test/example_N_trace_test.erl`
- **Event type counting** - Verify correct number of each event type
- **Sequence validation** - Check first/last events, key transitions
- **Demo script tests** - Verify scripts exist and are executable

### Integration Tests:
- **End-to-end execution** - Run complete workflow, verify completion
- **Trace export** - Verify ln_trace export to JSON works
- **Error handling** - Test error paths in Example 1

### Manual Testing Steps:
1. **Compile all examples:** `rebar3 compile`
2. **Run each demo script:** `./examples/example_*_demo.sh`
3. **Verify trace output** matches documentation
4. **Run tests:** `rebar3 eunit`
5. **Check navigation** from docs to examples

## Migration Notes

No data migration required. This is new functionality.

## References

- Research: `/Users/sac/cre/.wreckit/items/028-example-workflows-and-demonstrations/research.md`
- `docs/EXAMPLES.md:11-13` - Claims examples exist at `examples/workflows/`
- `test/yawl_of_demo.erl:1-452` - Order Fulfillment demo pattern
- `test/yawl_of_demo.erl:82-100` - Demo entry point with options
- `test/yawl_of_demo.erl:242-276` - Result printing pattern
- `src/ln_trace.erl:30-40` - Event type definitions
- `src/ln_trace.erl:80-95` - Event emission pattern
- `src/patterns/sequence.erl:1-67` - Minimal gen_yawl pattern
- `src/patterns/parallel_split.erl:22-97` - Comprehensive moduledoc pattern
- `rebar.config:4-24` - Source directories including `src/patterns`
- `docs/START_HERE.md:39` - Navigation hub with examples link
