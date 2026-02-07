# YAWL gen_yawl API Specification

**Version**: 1.0.0
**Date**: 2026-02-06
**Status**: API Specification

## Table of Contents

1. [Overview](#overview)
2. [Why gen_yawl Extends gen_pnet](#why-gen_yawl-extends-gen_pnet)
3. [The 3-Tuple Return Value Problem and Solution](#the-3-tuple-return-value-problem-and-solution)
4. [Complete API Reference](#complete-api-reference)
5. [Callback Specifications](#callback-specifications)
6. [Migration Guide from gen_pnet to gen_yawl](#migration-guide-from-gen_pnet-to-gen_yawl)
7. [Example YAWL Pattern Implementation](#example-yawl-pattern-implementation)

---

## Overview

`gen_yawl` is an OTP behavior that extends `gen_pnet` (Generic Petri Net) to provide specialized support for implementing YAWL (Yet Another Workflow Language) workflow patterns. While `gen_pnet` provides the foundational Petri net execution engine, `gen_yawl adds YAWL-specific semantics including:

- **Workflow Case Management**: Lifecycle management for workflow instances
- **Token Color Support**: Rich data carrying through tokens
- **Split/Join Semantics**: AND-split, OR-split, XOR-split with corresponding joins
- **Multi-Instance Patterns**: Static and dynamic instance creation
- **State-Based Patterns**: Milestones, cancellation regions
- **Exception Handling**: Integrated exception propagation and recovery

### Relationship to gen_pnet

```
gen_pnet (Generic Petri Net)
    |
    |--- Core Petri net execution
    |--- Token-based flow control
    |--- Place/Transition semantics
    |--- Mode enumeration
    |
    v
gen_yawl (YAWL Workflow Extensions)
    |
    |--- Workflow case lifecycle
    |--- YAWL pattern semantics
    |--- Data flow enrichment
    |--- Human task integration
    |--- Event notifications
```

---

## Why gen_yawl Extends gen_pnet

The YAWL workflow language has specific requirements that go beyond basic Petri net semantics:

### 1. Complex Synchronization Patterns

YAWL supports 43+ workflow control-flow patterns including:
- **Parallel Split (AND-split)**: Multiple concurrent branches
- **Synchronization (AND-join)**: Wait for all branches
- **Exclusive Choice (XOR-split)**: Choose one branch
- **Simple Merge (XOR-join)**: Merge from any one branch
- **Multi-instance**: Static/dynamic instance creation with synchronization

### 2. Data Flow Separation

YAWL separates control flow from data flow:
- Control tokens enable/disable transitions
- Data tokens carry variables between tasks
- Rich data types and transformation support

### 3. Work Item Semantics

YAWL introduces "work items" representing:
- Task assignments to resources (people/systems)
- Input/output data binding
- Lifecycle states (Created, Started, Completed, Failed)
- Deadlines and priorities

### 4. Exception Handling

YAWL provides:
- Structured exception escalation
- Exception handlers and compensation
- Workflow and subprocess cancellation
- Restart and recovery semantics

---

## The 3-Tuple Return Value Problem and Solution

### The Problem

The base `gen_pnet:fire/3` callback returns either:
```erlang
abort | {produce, ProduceMap}
```

Where `ProduceMap` is `#{atom() => [token()]}`. This creates a limitation for YAWL workflows:

**YAWL requires three simultaneous concerns:**
1. **Token Production**: Where to place control tokens
2. **State Updates**: Modifying workflow state (case data, variables)
3. **Side Effects**: Logging, notifications, external triggers

### The Solution: Enhanced Return Values

`gen_yawl` extends the return value to support a 3-tuple:

```erlang
%% Standard gen_pnet return
{produce, ProduceMap}

%% gen_yawl extended returns (state update)
{produce, ProduceMap, NewUsrInfo}

%% gen_yawl extended returns (with side effects)
{produce, ProduceMap, NewUsrInfo, Effects}

%% Abort semantics
abort
```

Where `Effects` is a map of side effects:
```erlang
#{log => LogEntry,
  notify => [Recipient],
  trigger => [TriggerSpec],
  compensating => Compensator}
```

### Example: Comparing Approaches

**gen_pnet approach (limited state management):**
```erlang
fire('t_complete', #{'p_active' := [{work_item, Id}]}, UsrInfo) ->
    %% Can only return tokens - state must be managed externally
    {produce, #{
        'p_active' => [],
        'p_complete' => [completed]
    }}.
```

**gen_yawl approach (integrated state management):**
```erlang
fire('t_complete', #{'p_active' := [{work_item, Id}]}, State) ->
    %% Update state atomically with token production
    NewState = State#workflow_state{
        completed = [Id | State#workflow_state.completed],
        status = complete
    },
    Effects = #{
        log => #{
            event => task_complete,
            task_id => Id,
            timestamp => erlang:system_time(millisecond)
        }
    },
    {produce, #{
        'p_active' => [],
        'p_complete' => [completed]
    }, NewState, Effects}.
```

### Benefits

1. **Atomic State Updates**: State changes are guaranteed to happen with token production
2. **Audit Trail**: Built-in logging with each transition firing
3. **Event Notifications**: Automatic subscriber notifications on state changes
4. **Type Safety**: Record-based state with Dialyzer support

---

## Complete API Reference

### Module: yawl_pattern (Primary API)

The `yawl_pattern` module provides the main API for creating and managing YAWL workflow patterns as gen_pnet instances.

#### Starting a Workflow Pattern

```erlang
%% @doc Starts a new YAWL workflow pattern as a gen_pnet instance
-spec start_workflow(PatternModule :: module(),
                     PatternConfig :: map()) ->
    {ok, pid()} | {error, term()}.

-spec start_workflow(PatternModule :: module(),
                     PatternConfig :: map(),
                     Options :: list()) ->
    {ok, pid()} | {error, term()}.
```

**Parameters:**
- `PatternModule`: Module implementing gen_yawl callbacks
- `PatternConfig`: Configuration map with keys:
  - `workflow_id`: Binary workflow identifier
  - `initial_data`: Initial case data (map)
  - `instance_count`: Number of instances (for multi-instance patterns)
  - `branch_count`: Number of parallel branches
  - `condition_fun`: Function for conditional routing (xor_split)
  - `timeout_ms`: Execution timeout
- `Options`: gen_pnet options (debug, spawn_opt, timeout)

**Returns:**
- `{ok, Pid}`: Successfully started workflow
- `{error, {invalid_pattern, Reason}}`: Pattern module validation failed
- `{error, {invalid_config, Reason}}`: Configuration validation failed

#### Querying Workflow State

```erlang
%% @doc Gets the current marking of the workflow
-spec get_marking(Pid :: pid()) ->
    #{atom() => [token()]}.

%% @doc Gets tokens on a specific place
-spec get_place_tokens(Pid :: pid(), Place :: atom()) ->
    {ok, [token()]} | {error, not_found}.

%% @doc Gets the workflow case state
-spec get_case_state(Pid :: pid()) ->
    {ok, #case_state{}} | {error, term()}.
```

#### State Manipulation

```erlang
%% @doc Injects a token into a specific place
-spec inject_token(Pid :: pid(),
                  Place :: atom(),
                  Token :: token()) ->
    ok | {error, term()}.

%% @doc Sets a workflow variable
-spec set_variable(Pid :: pid(),
                  Key :: term(),
                  Value :: term()) ->
    ok | {error, term()}.

%% @doc Gets a workflow variable
-spec get_variable(Pid :: pid(), Key :: term()) ->
    {ok, term()} | {error, not_found}.
```

#### Control Functions

```erlang
%% @doc Cancels a running workflow case
-spec cancel_case(Pid :: pid()) -> ok | {error, term()}.

%% @doc Suspends a running workflow case
-spec suspend_case(Pid :: pid()) -> ok | {error, term()}.

%% @doc Resumes a suspended workflow case
-spec resume_case(Pid :: pid()) -> ok | {error, term()}.
```

#### Observer Functions

```erlang
%% @doc Subscribes to workflow events
-spec subscribe(Pid :: pid(), Subscriber :: pid()) -> ok.

%% @doc Unsubscribes from workflow events
-spec unsubscribe(Pid :: pid(), Subscriber :: pid()) -> ok.
```

### Module: yawl_tokens (Token Utilities)

```erlang
%% @doc Creates a workflow start token
-spec initial_token(Data :: term()) -> token().

%% @doc Creates a task work item token
-spec work_item_token(TaskId :: binary(), Input :: term()) -> token().

%% @doc Creates a task completion token
-spec completion_token(TaskId :: binary(), Output :: term()) -> token().

%% @doc Creates a branch token for parallel execution
-spec branch_token(BranchId :: atom(), Data :: term()) -> token().

%% @doc Creates a cancel token
-spec cancel_token(Reason :: term()) -> token().

%% @doc Creates an error token
-spec error_token(Reason :: term()) -> token().

%% @doc Extracts token type
-spec get_token_type(token()) -> token_type().

%% @doc Extracts token payload
-spec get_token_payload(token()) -> term().
```

### Module: yawl_case_state (Case State Management)

```erlang
%% @doc Creates a new case state
-spec new_case_state(WorkflowId :: binary(),
                    InitialData :: map()) ->
    #case_state{}.

%% @doc Updates case status
-spec update_status(#case_state{}, case_status()) ->
    #case_state{}.

%% @doc Adds a work item to the case
-spec add_work_item(#case_state{}, #work_item{}) ->
    #case_state{}.

%% @doc Updates work item status
-spec update_work_item(#case_state{},
                      binary(),
                      work_item_status()) ->
    #case_state{}.

%% @doc Gets active work items
-spec get_active_work_items(#case_state{}) ->
    [#work_item{}].
```

---

## Callback Specifications

### Required gen_yawl Callbacks

Modules implementing `gen_yawl` must implement all `gen_pnet` callbacks plus the following YAWL-specific callbacks:

#### workflow_info/0

```erlang
%% @doc Returns metadata about the workflow pattern
-spec workflow_info() ->
    #{
        id := binary(),
        name := binary(),
        category := pattern_category(),
        wcp_number => {integer(), integer()},
        description := binary()
    }.
```

**Example:**
```erlang
workflow_info() ->
    #{
        id => <<"parallel_split">>,
        name => <<"Parallel Split">>,
        category => basic_control_flow,
        wcp_number => {2, 1},
        description => <<"Splits execution into parallel branches">>
    }.
```

#### init_case/2

```erlang
%% @doc Initializes a new workflow case
-spec init_case(CaseId :: binary(),
                InitialData :: map()) ->
    #case_state{}.
```

#### validate_config/1

```erlang
%% @doc Validates pattern configuration
-spec validate_config(Config :: map()) ->
    ok | {error, term()}.
```

### Extended fire/3 Callback

The `fire/3` callback supports enhanced return values:

```erlang
%% @doc Fires a transition with state updates
-spec fire(Trsn :: atom(),
          Mode :: #{atom() => [token()]},
          UsrInfo :: #case_state{}) ->
    abort |
    {produce, #{atom() => [token()]}} |
    {produce, #{atom() => [token()]}, #case_state{}} |
    {produce, #{atom() => [token()]}, #case_state{}, Effects :: map()}.
```

**Return Value Examples:**

```erlang
%% Simple token production (gen_pnet compatible)
{produce, #{'p_next' => [token]}}.

%% Token production with state update
{produce, #{'p_next' => [token]}, NewState}.

%% Token production with state update and effects
{produce, #{'p_next' => [token]}, NewState,
 #{log => #{event => complete}, notify => [Subscriber]}}.

%% Abort the transition firing
abort.
```

### Optional Callbacks

#### on_task_start/2

```erlang
%% @doc Called when a task work item is started
-spec on_task_start(TaskId :: binary(),
                   CaseState :: #case_state{}) ->
    {ok, #case_state{}} | {error, term()}.
```

#### on_task_complete/3

```erlang
%% @doc Called when a task work item is completed
-spec on_task_complete(TaskId :: binary(),
                      Result :: term(),
                      CaseState :: #case_state{}) ->
    {ok, #case_state{}} | {error, term()}.
```

#### on_exception/3

```erlang
%% @doc Called when an exception occurs
-spec on_exception(Exception :: term(),
                  Context :: map(),
                  CaseState :: #case_state{}) ->
    {handle, #case_state{}} | {propagate, #case_state{}} | {abort, #case_state{}}.
```

---

## Migration Guide from gen_pnet to gen_yawl

### Step 1: Update Module Declaration

**Before (gen_pnet):**
```erlang
-module(my_pattern).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
```

**After (gen_yawl):**
```erlang
-module(my_pattern).
-behaviour(gen_yawl).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3,
         workflow_info/0, init_case/2, validate_config/1]).
```

### Step 2: Add YAWL State Record

**Before:**
```erlang
%% Using simple usr_info term
init(_UsrInfo) ->
    {ok, []}.
```

**After:**
```erlang
-record(case_state, {
    case_id :: binary(),
    workflow_id :: binary(),
    status :: running | completed | cancelled,
    data = #{} :: map(),
    work_items = #{} :: map(),
    timestamps = #{} :: map()
}).

init_case(CaseId, InitialData) ->
    #case_state{
        case_id = CaseId,
        workflow_id = <<"my_workflow">>,
        status = running,
        data = InitialData,
        timestamps => #{
            created_at => erlang:system_time(millisecond)
        }
    }.
```

### Step 3: Update fire/3 Return Values

**Before:**
```erlang
fire('t_complete', Mode, _UsrInfo) ->
    %% Only return tokens
    {produce, #{
        'p_active' => [],
        'p_complete' => [done]
    }}.
```

**After:**
```erlang
fire('t_complete', #{'p_active' := [{work_item, Id}]}, State) ->
    %% Return tokens with state update and effects
    NewState = State#case_state{
        status = completed,
        timestamps = maps:put(completed_at, erlang:system_time(millisecond),
                             State#case_state.timestamps)
    },
    Effects = #{
        log => #{event => workflow_complete, case_id => State#case_state.case_id},
        notify => get_subscribers(State)
    },
    {produce, #{
        'p_active' => [],
        'p_complete' => [done]
    }, NewState, Effects}.
```

### Step 4: Add Workflow Metadata

**Add the workflow_info/0 callback:**
```erlang
workflow_info() ->
    #{
        id => <<"my_pattern">>,
        name => <<"My Custom Pattern">>,
        category => basic_control_flow,
        wcp_number => {1, 1},  % WCP-01: Sequence
        description => <<"A custom workflow pattern">>
    }.
```

### Step 5: Update Token Types

**Before (simple atoms):**
```erlang
init_marking('p_start', _UsrInfo) -> [start].
```

**After (structured tokens):**
```erlang
init_marking('p_start', _State) ->
    [{workflow_start, #{timestamp => erlang:system_time(millisecond)}}].
```

### Step 6: Add Configuration Validation

```erlang
validate_config(Config) ->
    case maps:get(workflow_id, Config, undefined) of
        undefined ->
            {error, missing_workflow_id};
        _WorkflowId when is_binary(_WorkflowId) ->
            ok;
        _ ->
            {error, invalid_workflow_id_type}
    end.
```

### Migration Checklist

- [ ] Update `-behaviour(gen_pnet)` to `-behaviour(gen_yawl)`
- [ ] Add `workflow_info/0` callback
- [ ] Add `init_case/2` callback
- [ ] Add `validate_config/1` callback
- [ ] Define `#case_state{}` record
- [ ] Update `fire/3` to use enhanced return values
- [ ] Add logging/effects to transitions
- [ ] Update token structures to carry data
- [ ] Add exception handling callbacks

---

## Example YAWL Pattern Implementation

The following example demonstrates implementing the Parallel Split pattern (WCP-02) using gen_yawl:

### Complete Module Implementation

```erlang
%% -*- erlang -*-
-module(parallel_split).
-behaviour(gen_yawl).

%% gen_yawl callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    trigger/3,
    workflow_info/0,
    init_case/2,
    validate_config/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
]).

%% API exports
-export([
    new/2,
    start/1,
    execute/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(case_state, {
    case_id :: binary(),
    workflow_id :: binary(),
    status :: running | completed | cancelled,
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed_branches = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    log_id :: binary() | undefined,
    timestamps :: map()
}).

-record(branch_token, {
    index :: pos_integer(),
    input :: term()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Creates a new parallel split configuration
new(BranchFuns, BranchCount) when length(BranchFuns) =:= BranchCount ->
    #{
        branch_funs => BranchFuns,
        branch_count => BranchCount
    }.

%% @doc Starts a parallel split workflow
start(Config) ->
    yawl_pattern:start_workflow(?MODULE, Config).

%% @doc Executes parallel split synchronously
execute(BranchFuns, InputData) ->
    Config = #{
        branch_funs => BranchFuns,
        branch_count => length(BranchFuns),
        initial_data => #{input => InputData}
    },
    case start(Config) of
        {ok, Pid} ->
            wait_for_completion(Pid, 30000);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% gen_yawl Callbacks
%%====================================================================

workflow_info() ->
    #{
        id => <<"parallel_split">>,
        name => <<"Parallel Split">>,
        category => basic_control_flow,
        wcp_number => {2, 1},
        description => <<"Splits execution into N parallel branches">>
    }.

init_case(CaseId, InitialData) ->
    #case_state{
        case_id = CaseId,
        workflow_id = maps:get(workflow_id, InitialData, <<"parallel_split">>),
        status = running,
        branch_count = maps:get(branch_count, InitialData, 2),
        branch_funs = maps:get(branch_funs, InitialData, []),
        timestamps => #{
            created_at => erlang:system_time(millisecond)
        }
    }.

validate_config(Config) ->
    BranchCount = maps:get(branch_count, Config, 0),
    BranchFuns = maps:get(branch_funs, Config, []),
    case BranchCount >= 2 andalso length(BranchFuns) =:= BranchCount of
        true -> ok;
        false -> {error, invalid_branch_configuration}
    end.

%%--------------------------------------------------------------------
%% Petri Net Structure
%%--------------------------------------------------------------------

place_lst() ->
    [
        'p_start',          % Initial place
        'p_branch_active',  % Branches executing
        'p_branch_done',    % Branch completion
        'p_all_done',       % All branches complete
        'p_complete'        % Final state
    ].

trsn_lst() ->
    [
        't_split',          % Split into branches
        't_complete_branch',% Individual branch completes
        't_join'            % Join all branches
    ].

init_marking('p_start', _State) ->
    [start];
init_marking(_Place, _State) ->
    [].

preset('t_split') -> ['p_start'];
preset('t_complete_branch') -> ['p_branch_active'];
preset('t_join') -> ['p_branch_done'];
preset(_Trsn) -> [].

%%--------------------------------------------------------------------
%% Transition Logic
%%--------------------------------------------------------------------

is_enabled('t_split', #{'p_start' := [start]}, _State) ->
    true;
is_enabled('t_complete_branch', #{'p_branch_active' := [_Tokens]}, #case_state{completed_branches = Completed, branch_count = Count}) when length(Completed) < Count ->
    true;
is_enabled('t_join', #{'p_branch_done' := DoneTokens}, #case_state{branch_count = Count}) when length(DoneTokens) =:= Count ->
    true;
is_enabled(_Trsn, _Mode, _State) ->
    false.

%% @doc Split transition - creates branch tokens
fire('t_split', #{'p_start' := [start]}, #case_state{branch_count = Count, branch_funs = Funs} = State) ->
    %% Create branch tokens
    BranchTokens = [#branch_token{index = I, input = Input} || {I, Input} <- lists:zip(lists:seq(1, Count), Funs)],

    Effects = #{
        log => #{
            event => parallel_split,
            branch_count => Count,
            timestamp => erlang:system_time(millisecond)
        }
    },

    {produce, #{
        'p_start' => [],
        'p_branch_active' => BranchTokens
    }, State, Effects};

%% @doc Complete individual branch
fire('t_complete_branch', #{'p_branch_active' := [Token | Rest]}, #case_state{completed_branches = Completed, results = Results} = State) ->
    #branch_token{index = Index} = Token,

    NewCompleted = [Index | Completed],
    NewResults = Results,

    NewState = State#case_state{
        completed_branches = NewCompleted,
        results = NewResults
    },

    Effects = #{
        log => #{
            event => branch_complete,
            branch_index => Index,
            timestamp => erlang:system_time(millisecond)
        }
    },

    %% Move token to done place
    {produce, #{
        'p_branch_active' => Rest,
        'p_branch_done' => [Token]
    }, NewState, Effects};

%% @doc Join all branches
fire('t_join', Mode, #case_state{completed_branches = Completed, results = Results} = State) ->
    AllTokens = maps:get('p_branch_done', Mode, []),
    NewState = State#case_state{
        status = completed,
        timestamps = maps:put(completed_at, erlang:system_time(millisecond), State#case_state.timestamps)
    },

    Effects = #{
        log => #{
            event => parallel_join,
            all_branches => lists:sort(Completed),
            timestamp => erlang:system_time(millisecond)
        },
        notify => State#case_state.subscribers
    },

    {produce, #{
        'p_branch_done' => [],
        'p_all_done' => AllTokens,
        'p_complete' => [complete, Results]
    }, NewState, Effects};

fire(_Trsn, _Mode, _State) ->
    abort.

%%--------------------------------------------------------------------
%% Trigger Callback
%%--------------------------------------------------------------------

trigger(_Place, _Token, _State) ->
    pass.

%%--------------------------------------------------------------------
%% Standard gen_server Callbacks
%%--------------------------------------------------------------------

init(InitArg) ->
    State = InitArg,
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

wait_for_completion(Pid, Timeout) ->
    receive
        {yawl_complete, Pid, Result} ->
            {ok, Result}
    after Timeout ->
        {error, timeout}
    end.
```

### Usage Example

```erlang
%% Define branch functions
Branch1 = fun(Input) ->
    {ok, Input * 2}
end,

Branch2 = fun(Input) ->
    {ok, Input + 10}
end,

Branch3 = fun(Input) ->
    {ok, Input div 2}
end,

%% Execute parallel split
{ok, Pid} = parallel_split:start([Branch1, Branch2, Branch3]),

%% Or execute synchronously
{ok, Results} = parallel_split:execute([Branch1, Branch2, Branch3], 100).
%% Results = #{1 => 200, 2 => 110, 3 => 50}
```

---

## Type Specifications

### Token Types

```erlang
-type token_type() ::
    workflow_start     |
    task_work_item     |
    task_complete      |
    branch_token       |
    control_token      |
    cancel_token       |
    error_token        |
    completion_token.

-type token() :: token_type() | {token_type(), term()}.

%% YAWL-specific token records
-record(work_item, {
    task_id :: binary(),
    input_data :: term(),
    status :: pending | started | completed | failed
}).

-record(branch_token, {
    index :: pos_integer(),
    input :: term()
}).

-record(cancel_token, {
    reason :: term(),
    scope :: task | case | workflow
}).
```

### Case State Types

```erlang
-type case_status() ::
    initialized |
    running |
    suspended |
    completed |
    cancelled |
    failed.

-type case_state() :: #case_state{
    case_id :: binary(),
    workflow_id :: binary(),
    status :: case_status(),
    data :: map(),
    work_items :: map(),
    timestamps :: map()
}.
```

### Pattern Categories

```erlang
-type pattern_category() ::
    basic_control_flow      |  % WCP-01 to WCP-06
    advanced_branching      |  % WCP-07 to WCP-12
    multiple_instances      |  % WCP-13 to WCP-17
    state_based             |  % WCP-18 to WCP-21
    cancellation            |  % WCP-22 to WCP-25
    exception_handling.      |  % Additional patterns
```

---

## Event Notification Protocol

### Event Types

```erlang
-record(yawl_event, {
    event_type   :: atom(),
    case_id      :: binary(),
    workflow_id  :: binary(),
    timestamp    :: integer(),
    data         :: term()
}).
```

### Event Flow

```
Workflow Execution                  Subscriber
     |                                   |
     ├───► {yawl_start, Pid, CaseId} ────►
     ├───► {yawl_complete, Pid, Result} ──►
     ├───► {yawl_cancel, Pid, Reason} ────►
     ├───► {yawl_error, Pid, Error} ──────►
     ├───► {yawl_token_produced, Pid, {Place, Token}} ─►
     └───► {yawl_transition_fired, Pid, Transition} ──►
```

---

## Best Practices

### 1. Token Design

Use structured tokens for better traceability:

```erlang
%% Good - structured token
{task_complete, #{
    task_id => <<"task_1">>,
    result => #{value => 42},
    timestamp => 1234567890
}}

%% Avoid - simple atoms
task_complete
```

### 2. State Updates

Always return updated state with fire/3:

```erlang
fire(Trsn, Mode, State) ->
    NewState = update_state(Trsn, Mode, State),
    Effects = create_effects(Trsn, NewState),
    {produce, ProduceMap, NewState, Effects}.
```

### 3. Error Handling

Use error tokens for exception propagation:

```erlang
fire('t_execute', Mode, State) ->
    try
        Result = execute_task(Mode),
        {produce, success_tokens(Result), State}
    catch
        Error:Reason:Stack ->
            ErrorToken = {error, #{error => Error, reason => Reason, stack => Stack}},
            {produce, #{'p_error' => [ErrorToken]}, State}
    end.
```

### 4. Logging

Include structured logging in effects:

```erlang
Effects = #{
    log => #{
        event => transition_fired,
        transition => Trsn,
        case_id => State#case_state.case_id,
        timestamp => erlang:system_time(millisecond)
    }
}.
```

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-06
**Related Documents**: GEN_PNET_API_SPECIFICATION.md, GEN_PNET_INTEGRATION_ARCHITECTURE.md
