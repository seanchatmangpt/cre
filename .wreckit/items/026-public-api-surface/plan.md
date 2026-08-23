# Public API Surface Implementation Plan

## Implementation Plan Title
Unified Public API for Workflow Case Management

## Overview
Create a clean, well-documented public API surface for creating and managing workflow cases without exposing internal complexity. The API will provide a unified interface to the existing workflow engine (wf_engine), case supervisor (wf_case_sup), and choreography control (ln_ctrl) modules while supporting the required options: scheduler_policy, step_quanta, trace_level, and effect_handler.

## Current State
The CRE codebase has multiple layers of workflow execution APIs scattered across several modules:

- **wf_engine** - Core workflow engine with case management but no option support
- **wf_case_sup** - Case lifecycle management through supervision tree
- **wf_case_runner** - Gen_server wrapper for individual cases
- **ln_ctrl** - Choreography control with scheduler_policy, step_quanta, trace_level, effect_handler options
- **cre_client** - Well-documented gen_server pattern to follow
- **cre_yawl_client** - YAWL-specific client with stub implementation

The required options already exist in ln_ctrl but are not exposed through workflow case management APIs. Documentation quality varies significantly across modules.

## Desired End State
A unified public API module (`cre_case`) that provides:

1. **Clean case lifecycle API** with consistent error handling
2. **Option support** (scheduler_policy, step_quanta, trace_level, effect_handler)
3. **Comprehensive documentation** following cre_client style
4. **Facade pattern** delegating to existing modules
5. **Backward compatibility** with existing wf_engine and wf_case_sup APIs

### Key Discoveries:
- **Documentation pattern**: `src/api/cre_client.erl:119-225` demonstrates target documentation style with -moduledoc, -doc, examples, and type specs
- **Option handling pattern**: `src/ln_ctrl.erl:153-170` shows proplists:get_value pattern with defaults for option parsing
- **Case management APIs**: `src/wf/wf_engine.erl:70-76` and `src/wf/wf_case_sup.erl:13-15` provide the core functionality to wrap
- **State management**: Cases have states (pending|running|suspended|cancelled|completed|failed) defined in `src/wf/wf_engine.hrl:27-40`
- **Supervision integration**: `src/wf/wf_case_sup.erl` provides OTP supervision tree for fault tolerance

## What We're NOT Doing
- ❌ Modifying wf_engine or wf_case_sup internal implementations
- ❌ Changing ln_ctrl option handling or behavior
- ❌ Implementing new workflow execution logic
- ❌ Completing or fixing cre_yawl_client stubs
- ❌ Creating new case storage mechanisms
- ❌ Breaking backward compatibility with existing APIs
- ❌ Adding asynchronous/non-blocking API variants
- ❌ Implementing work item allocation at this stage (deemed too low-level for initial API)

## Implementation Approach

**Strategy**: Facade pattern with minimal wrapper layer
- Create `cre_case` module as the primary public interface
- Delegate to existing modules (wf_case_sup, wf_engine, ln_ctrl)
- Use proplists for option passing with sensible defaults
- Follow cre_client documentation and error handling patterns
- Maintain backward compatibility by keeping existing APIs intact

**Rationale**:
- Minimal risk - no changes to existing tested code
- Clear separation - public API vs internal implementation
- Incremental - can add features without breaking existing code
- Documented - follows established patterns from cre_client

---

## Phases

### Phase 1: Create cre_case Module Skeleton

#### Overview
Create the basic module structure with type definitions, exported function signatures, and comprehensive documentation template following the cre_client pattern.

#### Changes Required:

##### 1. Create src/api/cre_case.erl
**File**: `src/api/cre_case.erl`
**Changes**: New file with module skeleton, documentation, and type definitions

```erlang
-module(cre_case).
-moduledoc("""
Workflow Case Management API.

This module provides a clean, well-documented interface for creating and managing
workflow cases without exposing internal complexity. It serves as a facade over
the internal workflow engine (wf_engine), case supervisor (wf_case_sup), and
choreography control (ln_ctrl) modules.

## Features

- Case lifecycle management (start, suspend, resume, cancel)
- Work item queries and management
- Configurable execution options (scheduler, tracing, effects)
- OTP supervision tree integration for fault tolerance

## Example

```erlang
%% Start a case with options
{ok, CaseId} = cre_case:start_case(
    <<"approval_workflow">>,
    #{requester => <<"alice">>, amount => 5000},
    #{scheduler_policy => deterministic, trace_level => basic}
),

%% Query case status
{ok, Status} = cre_case:get_case_status(CaseId),

%% Suspend the case
ok = cre_case:suspend_case(CaseId, <<"maintenance">>),

%% Resume later
ok = cre_case:resume_case(CaseId),

%% Get worklist for a user
{ok, WorkItems} = cre_case:get_worklist(CaseId, <<"bob">>).

```
""").

%% Exported Types
-export_type([
    case_id/0,
    case_status/0,
    case_info/0,
    option/0,
    options/0
]).

%% Case Lifecycle
-export([
    start_case/3,
    start_case/4,
    get_case_info/1,
    get_case_status/1,
    get_case_results/1,
    list_cases/0,
    find_case/1,
    suspend_case/2,
    resume_case/2,
    cancel_case/2
]).

%% Work Item Queries
-export([
    get_worklist/2,
    get_workitem_info/2
]).

-type case_id() :: binary().
-type case_status() :: pending | running | suspended | cancelled | completed | failed.
-type spec_id() :: binary().
-type initial_data() :: map().
-type reason() :: term().

-type option() ::
    {scheduler_policy, deterministic | nondeterministic} |
    {step_quanta, pos_integer()} |
    {trace_level, none | min | full} |
    {effect_handler, module()} |
    {timeout, timeout()}.

-type options() :: [option()].

-record(case_info, {
    id :: case_id(),
    spec_id :: spec_id(),
    status :: case_status(),
    created_at :: erlang:timestamp(),
    data :: map()
}).

-type case_info() :: #case_info{}.

-type worklist_item() :: #{
    id := binary(),
    task_name := binary(),
    offered_to => [binary()],
    status => offered | allocated | started | completed
}.

-type worklist() :: [worklist_item()].
```

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles without errors: `erlc -I include -o ebin src/api/cre_case.erl`
- [ ] Type checks pass: `dialyzer -r src/api`
- [ ] Module exports all required functions
- [ ] Documentation can be extracted: `erl -eval "edoc:application(cre, [{dir, \"doc\"}])" -s init stop`

##### Manual Verification:
- [ ] Module documentation is clear and comprehensive
- [ ] All exported functions have type specs
- [ ] Option type matches requirements from research

**Note**: Complete skeleton creation before proceeding to implementation.

---

### Phase 2: Implement Case Lifecycle Functions

#### Overview
Implement the core case lifecycle management functions: start_case, get_case_info, get_case_status, get_case_results, list_cases, find_case, suspend_case, resume_case, and cancel_case. These functions delegate to wf_case_sup and wf_engine.

#### Changes Required:

##### 1. Implement start_case/3,4
**File**: `src/api/cre_case.erl`
**Changes**: Add start_case implementations with option parsing

```erlang
-doc("""
Start a new workflow case.

Creates a new workflow case from the specified workflow specification with the
given initial data. Options can be provided to configure execution behavior.

## Parameters

- SpecId: Workflow specification identifier
- InitialData: Initial case data (map)
- Options: Execution options (see option() type)

## Returns

- {ok, CaseId}: Case started successfully, returns case identifier
- {error, Reason}: Case failed to start

## Options

- scheduler_policy: Determines task scheduling order
  - nondeterministic (default): Tasks execute as available
  - deterministic: Tasks execute in predictable order for testing
- step_quanta: Maximum steps per execution quantum (default: 1)
- trace_level: Logging verbosity
  - none: No tracing
  - min (default): Minimal tracing
  - full: Detailed execution tracing
- effect_handler: Module implementing effect handling callbacks (default: ln_effect)
- timeout: Case startup timeout in milliseconds (default: 5000)

## Examples

```erlang
%% Start with default options
{ok, CaseId} = cre_case:start_case(
    <<"order_processing">>,
    #{order_id => 12345, customer => <<"acme">>}
),

%% Start with custom options
{ok, CaseId} = cre_case:start_case(
    <<"order_processing">>,
    #{order_id => 12345},
    #{scheduler_policy => deterministic, trace_level => full}
).
```
""").
-spec start_case(SpecId, InitialData) -> Result when
    SpecId :: spec_id(),
    InitialData :: initial_data(),
    Result :: {ok, case_id()} | {error, term()}.
start_case(SpecId, InitialData) ->
    start_case(SpecId, InitialData, []).

-spec start_case(SpecId, InitialData, Options) -> Result when
    SpecId :: spec_id(),
    InitialData :: initial_data(),
    Options :: options(),
    Result :: {ok, case_id()} | {error, term()}.
start_case(SpecId, InitialData, Options) ->
    %% Extract options with defaults
    SchedulerPolicy = proplists:get_value(scheduler_policy, Options, nondeterministic),
    StepQuanta = proplists:get_value(step_quanta, Options, 1),
    TraceLevel = proplists:get_value(trace_level, Options, min),
    EffectHandler = proplists:get_value(effect_handler, Options, ln_effect),
    Timeout = proplists:get_value(timeout, Options, 5000),

    %% Validate options
    case validate_options(SchedulerPolicy, StepQuanta, TraceLevel, EffectHandler) of
        ok ->
            %% Delegate to wf_case_sup with options
            case wf_case_sup:start_case(SpecId, InitialData, #{
                scheduler => SchedulerPolicy,
                step_quanta => StepQuanta,
                trace => TraceLevel,
                effect_handler => EffectHandler,
                timeout => Timeout
            }) of
                {ok, _Pid, CaseId} -> {ok, CaseId};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, {invalid_option, Reason}}
    end.

%% Internal option validation
validate_options(deterministic, _, _, _) -> ok;
validate_options(nondeterministic, _, _, _) -> ok;
validate_options(_, Quanta, _, _) when is_integer(Quanta), Quanta > 0 -> ok;
validate_options(_, _, none, _) -> ok;
validate_options(_, _, min, _) -> ok;
validate_options(_, _, full, _) -> ok;
validate_options(_, _, _, Module) when is_atom(Module) -> ok;
validate_options(_, _, _, _) -> {error, invalid_option_value}.
```

##### 2. Implement case query functions
**File**: `src/api/cre_case.erl`
**Changes**: Add get_case_info, get_case_status, get_case_results, list_cases, find_case

```erlang
-doc("""
Get detailed information about a case.

Returns a case_info record containing case metadata and current state.

## Parameters

- CaseId: Case identifier

## Returns

- {ok, CaseInfo}: Case information record
- {error, not_found}: Case does not exist
- {error, Reason}: Other error

## Example

```erlang
{ok, CaseInfo} = cre_case:get_case_info(CaseId),
%% CaseInfo#case_info.status == running
```
""").
-spec get_case_info(CaseId) -> Result when
    CaseId :: case_id(),
    Result :: {ok, case_info()} | {error, term()}.
get_case_info(CaseId) ->
    case wf_case_sup:find_case(CaseId) of
        {ok, Pid} ->
            case wf_case_runner:get_info(Pid) of
                {ok, Info} ->
                    %% Sanitize internal state before returning
                    {ok, sanitize_case_info(Info)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-doc("""
Get the current status of a case.

Returns the case status atom: pending, running, suspended, cancelled,
completed, or failed.

## Parameters

- CaseId: Case identifier

## Returns

- {ok, Status}: Case status atom
- {error, not_found}: Case does not exist

## Example

```erlang
{ok, Status} = cre_case:get_case_status(CaseId),
case Status of
    running -> io:format("Case is running~n");
    completed -> io:format("Case completed~n")
end.
```
""").
-spec get_case_status(CaseId) -> Result when
    CaseId :: case_id(),
    Result :: {ok, case_status()} | {error, term()}.
get_case_status(CaseId) ->
    wf_case_sup:get_case_status(CaseId).

-doc("""
Get the results of a completed case.

Returns the case output data if the case has completed successfully.

## Parameters

- CaseId: Case identifier

## Returns

- {ok, Results}: Case results (map)
- {error, not_completed}: Case has not completed yet
- {error, Reason}: Other error

## Example

```erlang
{ok, Results} = cre_case:get_case_results(CaseId),
Approval = maps:get(approval, Results).
```
""").
-spec get_case_results(CaseId) -> Result when
    CaseId :: case_id(),
    Result :: {ok, map()} | {error, term()}.
get_case_results(CaseId) ->
    case get_case_status(CaseId) of
        {ok, completed} ->
            case wf_case_sup:find_case(CaseId) of
                {ok, Pid} ->
                    case wf_case_runner:get_info(Pid) of
                        {ok, Info} ->
                            {ok, maps:get(results, Info, #{})};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Status} ->
            {error, {not_completed, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-doc("""
List all active cases.

Returns a list of case identifiers for all cases in the system.

## Returns

- {ok, CaseIds}: List of case identifiers

## Example

```erlang
{ok, Cases} = cre_case:list_cases(),
io:format("Active cases: ~p~n", [Cases]).
```
""").
-spec list_cases() -> {ok, [case_id()]}.
list_cases() ->
    wf_case_sup:list_cases().

-doc("""
Find a case by identifier.

Returns the case supervisor Pid for the case.

## Parameters

- CaseId: Case identifier

## Returns

- {ok, Pid}: Case process found
- {error, not_found}: Case does not exist

## Example

```erlang
case cre_case:find_case(CaseId) of
    {ok, Pid} -> io:format("Case process: ~p~n", [Pid]);
    {error, not_found} -> io:format("Case not found~n")
end.
```
""").
-spec find_case(CaseId) -> Result when
    CaseId :: case_id(),
    Result :: {ok, pid()} | {error, not_found}.
find_case(CaseId) ->
    wf_case_sup:find_case(CaseId).

%% Internal function to sanitize case info before returning to users
%% Hides internal complexity like Petri net markings, RNG state, etc.
sanitize_case_info(Info) ->
    %% Extract only public fields
    #case_info{
        id = maps:get(id, Info),
        spec_id = maps:get(spec_id, Info),
        status = maps:get(status, Info),
        created_at = maps:get(created_at, Info),
        data = maps:get(data, Info, #{})
    }.
```

##### 3. Implement case control functions
**File**: `src/api/cre_case.erl`
**Changes**: Add suspend_case, resume_case, cancel_case

```erlang
-doc("""
Suspend a running case.

Pauses case execution. The case can be resumed later with resume_case/2.

## Parameters

- CaseId: Case identifier
- Reason: Reason for suspension (for logging/audit)

## Returns

- ok: Case suspended successfully
- {error, Reason}: Suspension failed

## Example

```erlang
ok = cre_case:suspend_case(CaseId, <<"system maintenance">>).
```
""").
-spec suspend_case(CaseId, Reason) -> Result when
    CaseId :: case_id(),
    Reason :: reason(),
    Result :: ok | {error, term()}.
suspend_case(CaseId, Reason) ->
    wf_engine:suspend_case(CaseId, self(), Reason).

-doc("""
Resume a suspended case.

Resumes execution of a previously suspended case.

## Parameters

- CaseId: Case identifier
- Reason: Reason for resumption (for logging/audit)

## Returns

- ok: Case resumed successfully
- {error, Reason}: Resumption failed

## Example

```erlang
ok = cre_case:resume_case(CaseId, <<"maintenance complete">>).
```
""").
-spec resume_case(CaseId, Reason) -> Result when
    CaseId :: case_id(),
    Reason :: reason(),
    Result :: ok | {error, term()}.
resume_case(CaseId, Reason) ->
    wf_engine:resume_case(CaseId, self(), Reason).

-doc("""
Cancel a case.

Terminates case execution. The case cannot be resumed after cancellation.

## Parameters

- CaseId: Case identifier
- Reason: Reason for cancellation (for logging/audit)

## Returns

- ok: Case cancelled successfully
- {error, Reason}: Cancellation failed

## Example

```erlang
ok = cre_case:cancel_case(CaseId, <<"user cancelled">>).
```
""").
-spec cancel_case(CaseId, Reason) -> Result when
    CaseId :: case_id(),
    Reason :: reason(),
    Result :: ok | {error, term()}.
cancel_case(CaseId, Reason) ->
    wf_engine:cancel_case(CaseId, self(), Reason).
```

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles without errors
- [ ] All type specs are valid
- [ ] Function signatures match documentation
- [ ] Dialyzer passes without warnings

##### Manual Verification:
- [ ] Functions successfully delegate to wf_case_sup and wf_engine
- [ ] Option parsing extracts correct values with defaults
- [ ] Error returns match underlying module errors
- [ ] sanitize_case_info removes internal fields

**Note**: Implement all lifecycle functions before moving to work item queries.

---

### Phase 3: Implement Work Item Query Functions

#### Overview
Implement work item query functions: get_worklist and get_workitem_info. These provide read-only access to work items without low-level allocation operations.

#### Changes Required:

##### 1. Implement worklist functions
**File**: `src/api/cre_case.erl`
**Changes**: Add get_worklist and get_workitem_info

```erlang
-doc("""
Get worklist for a user.

Returns all work items currently offered to or allocated to the specified user.

## Parameters

- CaseId: Case identifier (optional, use <<>> for all cases)
- UserId: User identifier

## Returns

- {ok, Worklist}: List of work items for the user
- {error, Reason}: Query failed

## Work Item Structure

Each work item is a map with:
- id: Work item identifier
- task_name: Name of the task
- offered_to: List of users the item is offered to
- status: offered | allocated | started | completed

## Example

```erlang
%% Get worklist for a specific case
{ok, Items} = cre_case:get_worklist(CaseId, <<"bob">>),

%% Get worklist across all cases
{ok, Items} = cre_case:get_worklist(<<>>, <<"bob">>).
```
""").
-spec get_worklist(CaseId, UserId) -> Result when
    CaseId :: case_id(),
    UserId :: binary(),
    Result :: {ok, worklist()} | {error, term()}.
get_worklist(CaseId, UserId) ->
    case wf_engine:worklist(CaseId, UserId) of
        {ok, WorkItems} ->
            %% Convert internal work_item records to maps
            Sanitized = [sanitize_work_item(WI) || WI <- WorkItems],
            {ok, Sanitized};
        {error, Reason} ->
            {error, Reason}
    end.

-doc("""
Get detailed information about a work item.

Returns detailed information for a specific work item.

## Parameters

- CaseId: Case identifier
- WorkItemId: Work item identifier

## Returns

- {ok, WorkItemInfo}: Work item details
- {error, not_found}: Work item does not exist
- {error, Reason}: Other error

## Example

```erlang
{ok, ItemInfo} = cre_case:get_workitem_info(CaseId, WorkItemId),
TaskName = maps:get(task_name, ItemInfo).
```
""").
-spec get_workitem_info(CaseId, WorkItemId) -> Result when
    CaseId :: case_id(),
    WorkItemId :: binary(),
    Result :: {ok, worklist_item()} | {error, term()}.
get_workitem_info(CaseId, WorkItemId) ->
    %% This would require adding a function to wf_engine to get specific work item
    %% For now, we'll return an error indicating this needs to be implemented
    {error, not_implemented}.

%% Internal function to sanitize work item records
sanitize_work_item(WorkItem) ->
    %% Convert work_item record to map, hiding internal fields
    #{
        id => element(2, WorkItem), %% work_item id field
        task_name => element(3, WorkItem), %% work_item task_name field
        offered_to => element(4, WorkItem), %% work_item offered_to field
        status => element(5, WorkItem) %% work_item status field
    }.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles without errors
- [ ] Type specs are valid
- [ ] Dialyzer passes

##### Manual Verification:
- [ ] get_worklist returns properly formatted maps
- [ ] Internal record fields are not exposed
- [ ] Error cases handled appropriately

**Note**: Work item functions are read-only queries. Allocation operations are deferred.

---

### Phase 4: Add Documentation and Examples

#### Overview
Add comprehensive documentation including doctests, usage examples, and integration with the existing documentation system.

#### Changes Required:

##### 1. Add doctest function
**File**: `src/api/cre_case.erl`
**Changes**: Add doctest_test/0 function

```erlang
%% Internal function for doctest testing
-doc(false).
-spec doctest_test() -> ok.
doctest_test() ->
    %% Test option validation
    ok = validate_options(nondeterministic, 1, min, ln_effect),
    ok = validate_options(deterministic, 10, full, ln_effect),
    {error, _} = validate_options(invalid, 1, min, ln_effect),

    %% Test sanitize_case_info
    Info = #{
        id => <<"case123">>,
        spec_id => <<"workflow1">>,
        status => running,
        created_at => erlang:timestamp(),
        data => #{key => value},
        %% Internal fields that should be hidden
        marking => #{},
        rng_state => {}
    },
    Sanitized = sanitize_case_info(Info),
    <<"case123">> = Sanitized#case_info.id,
    running = Sanitized#case_info.status,

    ok.
```

##### 2. Create example workflow
**File**: `docs/examples/cre_case_example.erl`
**Changes**: New example file demonstrating API usage

```erlang
-module(cre_case_example).
-moduledoc("""
Examples demonstrating the cre_case public API.

Run these examples in the Erlang shell:

```erlang
%% Compile and load examples
1> c(cre_case_example).

%% Run basic example
2> cre_case_example:basic_example().

%% Run options example
3> cre_case_example:options_example().
```
""").

-export([basic_example/0, options_example/0]).

basic_example() ->
    %% Start a simple approval workflow
    {ok, CaseId} = cre_case:start_case(
        <<"approval_workflow">>,
        #{requester => <<"alice">>, amount => 5000}
    ),

    %% Check case status
    {ok, Status} = cre_case:get_case_status(CaseId),
    io:format("Case status: ~p~n", [Status]),

    %% List all cases
    {ok, Cases} = cre_case:list_cases(),
    io:format("All cases: ~p~n", [Cases]),

    %% Get worklist for a user
    {ok, Worklist} = cre_case:get_worklist(CaseId, <<"manager">>),
    io:format("Manager worklist: ~p~n", [Worklist]),

    %% Suspend case
    ok = cre_case:suspend_case(CaseId, <<"example suspend">>),
    io:format("Case suspended~n"),

    %% Resume case
    ok = cre_case:resume_case(CaseId, <<"example resume">>),
    io:format("Case resumed~n"),

    %% Cancel case (cleanup)
    ok = cre_case:cancel_case(CaseId, <<"example done">>),
    io:format("Case cancelled~n"),

    ok.

options_example() ->
    %% Start case with deterministic scheduling and full tracing
    {ok, CaseId} = cre_case:start_case(
        <<"order_processing">>,
        #{order_id => 12345, customer => <<"acme">>},
        #{
            scheduler_policy => deterministic,
            step_quanta => 5,
            trace_level => full,
            effect_handler => ln_effect
        }
    ),

    %% Monitor case with detailed tracing
    {ok, Info} = cre_case:get_case_info(CaseId),
    io:format("Case info: ~p~n", [Info]),

    %% Cleanup
    ok = cre_case:cancel_case(CaseId, <<"options example done">>),

    ok.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles without errors
- [ ] doctest_test/0 runs successfully
- [ ] Example code compiles
- [ ] Documentation generation completes: `edoc:application(cre, [{dir, "doc"}])`

##### Manual Verification:
- [ ] Examples run successfully in Erlang shell
- [ ] Documentation is clear and comprehensive
- [ ] All public functions are documented

**Note**: Complete documentation before final testing phase.

---

### Phase 5: Integration Testing

#### Overview
Create integration tests to verify the cre_case API works correctly with wf_case_sup, wf_engine, and ln_ctrl modules.

#### Changes Required:

##### 1. Create integration test file
**File**: `test/cre_case_integration_tests.erl`
**Changes**: New test file

```erlang
-module(cre_case_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test start_case with defaults
start_case_default_test() ->
    {ok, CaseId} = cre_case:start_case(
        <<"test_workflow">>,
        #{test => true}
    ),
    ?assert(is_binary(CaseId)),
    {ok, Status} = cre_case:get_case_status(CaseId),
    ?assert(lists:member(Status, [pending, running, completed])),
    ok = cre_case:cancel_case(CaseId, <<"test done">>).

%% Test start_case with options
start_case_with_options_test() ->
    {ok, CaseId} = cre_case:start_case(
        <<"test_workflow">>,
        #{test => true},
        #{
            scheduler_policy => deterministic,
            step_quanta => 10,
            trace_level => full
        }
    ),
    ?assert(is_binary(CaseId)),
    ok = cre_case:cancel_case(CaseId, <<"test done">>).

%% Test case lifecycle
case_lifecycle_test() ->
    {ok, CaseId} = cre_case:start_case(
        <<"test_workflow">>,
        #{test => true}
    ),

    %% Suspend
    ok = cre_case:suspend_case(CaseId, <<"test suspend">>),
    {ok, suspended} = cre_case:get_case_status(CaseId),

    %% Resume
    ok = cre_case:resume_case(CaseId, <<"test resume">>),

    %% Cancel
    ok = cre_case:cancel_case(CaseId, <<"test done">>),
    {ok, cancelled} = cre_case:get_case_status(CaseId).

%% Test list_cases and find_case
case_queries_test() ->
    {ok, CaseId1} = cre_case:start_case(
        <<"test_workflow">>,
        #{id => 1}
    ),
    {ok, CaseId2} = cre_case:start_case(
        <<"test_workflow">>,
        #{id => 2}
    ),

    {ok, Cases} = cre_case:list_cases(),
    ?assert(length(Cases) >= 2),

    {ok, _Pid} = cre_case:find_case(CaseId1),

    %% Cleanup
    cre_case:cancel_case(CaseId1, <<"test done">>),
    cre_case:cancel_case(CaseId2, <<"test done">>).
```

#### Success Criteria:

##### Automated Verification:
- [ ] All tests pass: `erl -eval "eunit:test(cre_case_integration_tests, [verbose])" -s init stop`
- [ ] No test failures or errors
- [ ] Code coverage > 80%

##### Manual Verification:
- [ ] Tests can be run independently
- [ ] Tests clean up after themselves
- [ ] Tests exercise all major code paths

**Note**: All tests must pass before considering implementation complete.

---

## Testing Strategy

### Unit Tests:
- Option validation logic (validate_options)
- Info sanitization (sanitize_case_info, sanitize_work_item)
- Error handling paths
- Edge cases (invalid inputs, missing cases)

### Integration Tests:
- Full case lifecycle (start, suspend, resume, cancel)
- Option pass-through to ln_ctrl
- Work item queries
- Multiple concurrent cases
- Error scenarios

### Manual Testing Steps:
1. Start Erlang shell: `erl -pa ebin`
2. Compile module: `c(cre_case).`
3. Run basic example: `cre_case_example:basic_example().`
4. Run options example: `cre_case_example:options_example().`
5. Verify tracing output with trace_level => full
6. Check that deterministic scheduling produces reproducible results

## Migration Notes

### For Users of wf_engine API:
The cre_case API provides a cleaner interface but wf_engine remains available:
- Replace `wf_engine:start_case(SpecId, InitialData, Opts)` with `cre_case:start_case(SpecId, InitialData, OptionsMap)`
- Replace `wf_engine:case_state(CaseId, Opts)` with `cre_case:get_case_status(CaseId)`
- Other wf_engine functions can be accessed directly if needed

### For Users of wf_case_sup API:
The cre_case API wraps wf_case_sup with option support:
- cre_case:start_case/4 passes options to ln_ctrl through wf_case_sup
- Query functions delegate to wf_case_sup
- Case control functions delegate to wf_engine

### Backward Compatibility:
- All existing wf_engine and wf_case_sup APIs remain unchanged
- cre_case is an additional facade layer, not a replacement
- No breaking changes to existing code

## References
- Research: `/Users/sac/cre/.wreckit/items/026-public-api-surface/research.md`
- Documentation pattern: `src/api/cre_client.erl:119-225`
- Option handling: `src/ln_ctrl.erl:153-170`
- Case management: `src/wf/wf_engine.erl:70-76`, `src/wf/wf_case_sup.erl:13-15`
- State definitions: `src/wf/wf_engine.hrl:27-40`
