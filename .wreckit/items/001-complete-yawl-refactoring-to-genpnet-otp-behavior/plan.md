# Complete YAWL refactoring to gen_pnet OTP behavior Implementation Plan

## Implementation Plan Title
Refactor YAWL workflow execution engine to properly use gen_pnet behavior

## Overview
The current CRE YAWL implementation has a hybrid architecture:
- `yawl_engine.erl` uses `gen_server` with a custom Petri net state representation
- `cre_yawl_patterns.erl` implements `-behaviour(gen_pnet)` but provides manual implementations rather than utilizing gen_pnet's built-in token passing semantics
- gen_pnet is already a dependency (rebar.config:8-9) but its native capabilities are not being leveraged

This refactoring will transition the system to use gen_pnet's native Petri net engine for workflow execution, replacing the custom token management currently implemented.

## Current State

### Key Files Analyzed:

**Core Implementation Files:**
- `/Users/sac/cre/src/yawl_engine.erl` - gen_server managing workflow cases with custom net_state at line 892-896
- `/Users/sac/cre/src/cre_yawl_patterns.erl` - Declares gen_pnet behavior (line 65) but implements manual token logic
- `/Users/sac/cre/src/yawl_executor.erl` - Pattern executor with category dispatch (lines 216-233)
- `/Users/sac/cre/src/cre_yawl.erl` - Workflow builder with pattern records (lines 146-169)

**Pattern Definitions (from actual code):**
- Basic patterns (WCP-01 to WCP-10): sequence, parallel_split, synchronization, exclusive_choice, simple_merge, multi_choice, synchronizing_merge, multi_merge, discriminator, arbitration
- Multiple instances (WCP-11 to WCP-17): implicit_termination, multiple_instances_no_sync, multiple_instances_static, multiple_instances_runtime, multiple_instances_dynamic, deferred_choice, interleaved_routing
- State-based patterns (WCP-18 to WCP-20): milestone, cancel_activity, cancel_case
- Extended control (WCP-21 to WCP-28): structured_sync, partial_join, structured_loop, recursion, interleaved_loop, critical_section, protocol, try_catch
- Data flow (WDP-01 to WDP-05): param_pass, data_transform, data_distribute, data_accumulate, data_visibility
- Resource patterns (WRP-01 to WRP-05): resource_create, role_allocate, resource_start, role_distribute, capability_allocate
- Exception handling (WHP-01 to WHP-05): error_handler, retry, compensate, triggered_compensation, consecutive_compensate

### Key Discoveries:

1. **gen_pnet is Already a Dependency** (rebar.config:8-9)
   ```erlang
   {gen_pnet, {git, "https://github.com/joergen7/gen_pnet.git", {branch, "master"}}}
   ```

2. **Current Token Management is Manual** (yawl_engine.erl:892-896)
   ```erlang
   NetState = #{
     places => #{input => [start], output => []},
     transitions => #{},
     markings => #{input => [start]}
   },
   ```
   This is a custom map-based representation, not using gen_pnet's native marking.

3. **cre_yawl_patterns Has gen_pnet Callbacks** (lines 72-85)
   But the implementation needs to be verified against actual gen_pnet behavior requirements.

4. **No Actual yawl_workflow_instance.erl or yawl_orchestrator.erl Files**
   The item description mentioned these files, but they don't exist. The actual architecture uses:
   - `yawl_engine.erl` (gen_server) for workflow case management
   - `yawl_executor.erl` for pattern execution dispatch
   - `cre_yawl.erl` for workflow construction

5. **Persistence Already Exists** (yawl_engine.erl:434-454)
   Uses `yawl_persistence` module with Mnesia, handles case/workitem persistence.

6. **Test Coverage is Extensive**
   29 test files covering all pattern categories and integration scenarios.

## Desired End State

The refactored system should:
1. Use gen_pnet's native token passing semantics instead of custom marking maps
2. Replace `yawl_engine.erl`'s gen_server with gen_pnet behavior for workflow instances
3. Define proper gen_pnet place/transition structures for all 43 YAWL patterns
4. Maintain backward compatibility with existing workflow definitions and APIs
5. Support colored tokens (data-carrying) through gen_pnet's token_data mechanism
6. Handle cancellation patterns via gen_pnet events
7. Persist gen_pnet net_state instead of custom net_state maps

### Key Changes Required:

**Current Pattern API → gen_pnet Module Mapping:**
- Each YAWL pattern becomes a separate gen_pnet module
- Workflow definitions compose pattern modules
- Token data carries workflow variables between places/transitions

## What We're NOT Doing

1. **Changing YAWL Semantics** - The 43 patterns must behave identically to current implementation
2. **Breaking External APIs** - All existing `cre_yawl` and `yawl_engine` API calls must continue working
3. **Removing Persistence** - Mnesia-based persistence will be adapted for gen_pnet state
4. **Modifying Pattern Record Definitions** - Pattern constructor APIs remain stable
5. **Altering Test Contracts** - All 29 existing test files must pass without modification

## Implementation Approach

The refactoring will proceed incrementally, starting with a proof-of-concept for basic patterns and progressively migrating more complex patterns. A compatibility layer will ensure existing workflows continue functioning during the transition.

### Strategic Decision:
Use the **Adapter Pattern** - Create gen_pnet modules that wrap existing pattern logic, allowing gradual migration without breaking changes.

---

## Phases

### Phase 1: Foundation and Proof of Concept (Week 1)

#### Overview
Validate that gen_pnet can properly represent YAWL patterns and establish the migration infrastructure.

#### Changes Required:

##### 1. Create gen_pnet Module Template
**File**: `src/yawl_pattern_template.erl` (NEW)
**Changes**: Create a template module demonstrating proper gen_pnet callback implementation for YAWL patterns.

```erlang
%% Template for YAWL pattern gen_pnet modules
-module(yawl_pattern_template).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([init/1, place_info/1, transition_info/1, inhibit_info/1, token_data/1]).
-export([trigger/3]).

-record(state, {pattern_type, data}).

%% Place information: {PlaceName, InitialTokenCount}
place_info(_State) ->
    #{
        input => {0, 0},      % {min, max} tokens
        output => {0, unlimited},
        active => {0, unlimited}
    }.

%% Transition information
transition_info(_State) ->
    #{
        start => {[], [input, active]},  % Input places, Output places
        complete => {[input, active], [output]}
    }.

%% Inhibitor arcs (for cancellation patterns)
inhibit_info(_State) ->
    #{}.

%% Token data structure (colored tokens)
token_data(_State) ->
    #{}
    %% Maps: {PlaceName, TokenIndex} => DataRecord

%% Trigger callback for transition side effects
trigger(TransitionName, InputTokens, State) ->
    %% Execute transition logic
    {ok, OutputTokens, NewState}.
```

##### 2. Create Basic Sequence Pattern Module
**File**: `src/yawl_pnet_sequence.erl` (NEW)
**Changes**: Implement the simplest pattern (sequence) as a proof-of-concept gen_pnet module.

```erlang
-module(yawl_pnet_sequence).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([init/1, place_info/1, transition_info/1, inhibit_info/1, token_data/1, trigger/3]).

%% API
-export([new/1, execute/2]).

-record(state, {
    task_ids :: [binary()],
    current_index = 0 :: non_neg_integer(),
    data = #{} :: map()
}).

init(#{task_ids := TaskIds}) ->
    {ok, #state{task_ids = TaskIds}}.

place_info(#state{task_ids = TaskIds}) ->
    Places = #{
        start => {1, 1},
        complete => {0, 1}
    },
    %% Add places for each task
    lists:foldl(fun(Id, Acc) ->
        Acc#{binary_to_atom(Id) => {0, 1}}
    end, Places, TaskIds).

transition_info(#state{task_ids = TaskIds}) ->
    %% Create transitions between consecutive tasks
    Transitions = lists:foldl(fun({From, To}, Acc) ->
        Acc#{binary_to_atom(From ++ "_" ++ To) => {[From], [To]}}
    end, #{}, lists:zip(TaskIds, tl(TaskIds))),
    Transitions#{
        start_task => {[start], [hd(TaskIds)]},
        complete_task => {[lists:last(TaskIds)], [complete]}
    }.

inhibit_info(_State) -> #{}.

token_data(#state{data = Data}) -> Data.

trigger(_Transition, _InputTokens, State) ->
    %% Side effects: execute task logic
    {ok, [], State}.
```

##### 3. Create Compatibility Layer
**File**: `src/yawl_pnet_adapter.erl` (NEW)
**Changes**: Adapter that translates between current pattern records and gen_pnet modules.

```erlang
%% Adapts existing pattern records to gen_pnet modules
-module(yawl_pnet_adapter).

-export([to_pnet_module/1, execute_pattern/3]).

to_pnet_module(#sequence{task_ids = TaskIds}) ->
    {ok, yawl_pnet_sequence, #{task_ids => TaskIds}};
to_pnet_module(#parallel_split{...}) ->
    {ok, yawl_pnet_parallel_split, #{...}};
%% ... handle all pattern types

execute_pattern(Pattern, Input, Options) ->
    case to_pnet_module(Pattern) of
        {ok, Module, InitArg} ->
            case gen_pnet:start_link(Module, InitArg, Options) of
                {ok, Pid} ->
                    %% Inject input token and execute
                    gen_pnet:trigger(Pid, start, Input),
                    {ok, Pid};
                {error, Reason} ->
                    {error, Reason}
            end
    end.
```

##### 4. Update Persistence Layer
**File**: `src/yawl_persistence.erl`
**Changes**: Add functions to serialize/deserialize gen_pnet net_state.

```erlang
%% Add to existing module
-export([save_pnet_state/2, load_pnet_state/1]).

save_pnet_state(CaseId, PnetState) ->
    %% Serialize gen_pnet state for Mnesia storage
    Serialized = term_to_binary(PnetState),
    mnesia:write(#yawl_case_state{case_id = CaseId, state = Serialized}).

load_pnet_state(CaseId) ->
    case mnesia:read({yawl_case_state, CaseId}) of
        [#yawl_case_state{state = Binary}] ->
            {ok, binary_to_term(Binary)};
        [] ->
            {error, not_found}
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Sequence pattern executes end-to-end via gen_pnet: `rebar3 eunit --module=yawl_pnet_sequence_test`
- [ ] Adapter translates pattern records correctly: `rebar3 eunit --module=yawl_pnet_adapter_test`
- [ ] Persistence saves/loads gen_pnet state: `rebar3 eunit --module=yawl_persistence_test`
- [ ] Build succeeds: `rebar3 compile`

##### Manual Verification:
- [ ] Sequence pattern produces same output as current implementation
- [ ] Token flow matches expected sequence (start → task1 → task2 → complete)
- [ ] State persistence works across node restart

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Basic Control Flow Patterns (Week 2)

#### Overview
Implement WCP-01 through WCP-10 as gen_pnet modules: sequence, parallel_split, synchronization, exclusive_choice, simple_merge, multi_choice, synchronizing_merge, multi_merge, discriminator, arbitration.

#### Changes Required:

##### 1. Create Parallel Split Module
**File**: `src/yawl_pnet_parallel_split.erl` (NEW)
**Changes**: Implement AND split pattern with concurrent token production.

```erlang
-module(yawl_pnet_parallel_split).
-behaviour(gen_pnet).

%% Single transition that produces multiple tokens
transition_info(#state{branch_task_ids = BranchIds}) ->
    #{
        split => {[start], BranchIds}  % One token in, multiple tokens out
    }.
```

##### 2. Create Synchronization Module
**File**: `src/yawl_pnet_synchronization.erl` (NEW)
**Changes**: Implement AND join pattern requiring all incoming tokens.

```erlang
transition_info(#state{incoming_task_ids = IncomingIds}) ->
    #{
        join => {IncomingIds, [complete]}  % All tokens in, one token out
    }.
```

##### 3. Create Exclusive Choice Module
**File**: `src/yawl_pnet_exclusive_choice.erl` (NEW)
**Changes**: Implement XOR split with conditional routing.

```erlang
trigger(choice, [Token], #state{branches = Branches}) ->
    %% Evaluate conditions and select first matching branch
    {SelectedBranch, _Condition} = select_branch(Branches, Token),
    {ok, [Token], #state{selected = SelectedBranch}}.

%% Override preset to use inhibitor arcs for non-selected branches
inhibit_info(#state{selected = Selected}) ->
    %% Inhibit all branches except the selected one
    lists:foldl(fun(Branch, Acc) ->
        case Branch of
            Selected -> Acc;
            _ -> Acc#{Branch => true}
        end
    end, #{}, get_all_branches()).
```

##### 4. Update Adapter for Basic Patterns
**File**: `src/yawl_pnet_adapter.erl`
**Changes**: Add translations for all basic patterns.

##### 5. Create Integration Tests
**File**: `test/yawl_pnet_basic_patterns_test.erl` (NEW)
**Changes**: Tests comparing gen_pnet implementation against current implementation.

```erlang
basic_pattern equivalence_test() ->
    %% Test that each basic pattern produces same output
    Patterns = [
        #sequence{task_ids = [<<"t1">>, <<"t2">>]},
        #parallel_split{...},
        %% ... all basic patterns
    ],
    lists:foreach(fun(Pattern) ->
        {ok, OldResult} = yawl_executor:execute_pattern(Pattern, Input),
        {ok, NewResult} = yawl_pnet_adapter:execute_pattern(Pattern, Input),
        ?assertEqual(OldResult, NewResult)
    end, Patterns).
```

#### Success Criteria:

##### Automated Verification:
- [ ] All 10 basic patterns have gen_pnet modules
- [ ] Pattern equivalence tests pass: `rebar3 eunit --module=yawl_pnet_basic_patterns_test`
- [ ] Token semantics match YAWL specification
- [ ] Performance within 10% of current implementation (benchmark test)

##### Manual Verification:
- [ ] Parallel split creates concurrent tokens
- [ ] Synchronization waits for all branches
- [ ] Exclusive choice routes to single branch
- [ ] Integration test with workflow combining multiple basic patterns

---

### Phase 3: Multiple Instance Patterns (Week 3)

#### Overview
Implement WCP-11 through WCP-17: implicit_termination, multiple_instances_no_sync, multiple_instances_static, multiple_instances_runtime, multiple_instances_dynamic, deferred_choice, interleaved_routing.

#### Changes Required:

##### 1. Create Multiple Instance Modules
**Files**: `src/yawl_pnet_multiple_instances_*.erl` (NEW)
**Changes**: Implement patterns that spawn concurrent subprocess instances.

**Key Challenge**: Representing instance tokens in gen_pnet's colored token system.

```erlang
-module(yawl_pnet_multiple_instances_no_sync).
-behaviour(gen_pnet).

-record(instance_token, {
    instance_id :: reference(),
    data :: term()
}).

token_data(#state{instances = Instances}) ->
    %% Map place+token_index to instance data
    maps:from_list(
        [{{active, N}, #instance_token{id=Id, data=Data}} ||
         {N, Id, Data} <- enumerate(Instances)]
    ).

trigger(create_instances, [_Token], #state{count = N}) ->
    %% Spawn N instances, each gets a token
    InstanceTokens = [create_instance_token(I) || I <- lists:seq(1, N)],
    {ok, InstanceTokens, #state{instances = InstanceTokens}}.
```

##### 2. Create Deferred Choice Module
**File**: `src/yawl_pnet_deferred_choice.erl` (NEW)
**Changes**: Implement runtime choice based on data availability.

```erlang
trigger(evaluate_options, Tokens, #state{options = Options}) ->
    %% Choice deferred until runtime when data becomes available
    case first_available_option(Tokens, Options) of
        {selected, Option} ->
            {ok, [Option#option.data], #state{selected = Option}};
        none ->
            {ok, [], State}  % Wait for more data
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All multiple instance patterns implemented as gen_pnet modules
- [ ] Tests verify instance spawning and synchronization behavior
- [ ] Deferred choice correctly handles data-dependent routing

---

### Phase 4: State-Based and Cancellation Patterns (Week 4)

#### Overview
Implement WCP-18 through WCP-20 (milestone, cancel_activity, cancel_case) and verify cancellation semantics work correctly with gen_pnet.

#### Changes Required:

##### 1. Create Cancellation Pattern Modules
**Files**: `src/yawl_pnet_cancel_*.erl` (NEW)
**Changes**: Implement cancellation using gen_pnet's event mechanism.

```erlang
-module(yawl_pnet_cancel_activity).
-behaviour(gen_pnet).

%% Use gen_pnet:call/Pid, {cancel, RegionId} to trigger cancellation
handle_info({cancel, RegionId}, State) ->
    %% Remove all tokens from places in the cancellation region
    NewState = cancel_region_tokens(RegionId, State),
    {noreply, NewState}.

cancel_region_tokens(RegionId, State) ->
    PlaceList = get_places_in_region(RegionId),
    lists:foldl(fun(Place, AccState) ->
        %% Remove all tokens from this place
        NewMarking = maps:put(Place, [], AccState#state.marking),
        AccState#state{marking = NewMarking}
    end, State, PlaceList).
```

##### 2. Create Milestone Pattern Module
**File**: `src/yawl_pnet_milestone.erl` (NEW)
**Changes**: Implement milestone-based triggering.

```erlang
trigger(milestone_reached, [Token], #state{milestone = Milestone}) ->
    case milestone_condition(Milestone, Token) of
        true ->
            {ok, [Token], #state{milestone_active = true}};
        false ->
            {ok, [], State}  % Hold token until milestone reached
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Cancellation removes all tokens from affected regions
- [ ] Milestone pattern holds tokens until condition is met
- [ ] Cascading cancellations work correctly

---

### Phase 5: Extended Control Patterns (Week 5)

#### Overview
Implement WCP-21 through WCP-28: structured_sync, partial_join, structured_loop, recursion, interleaved_loop, critical_section, protocol, try_catch.

#### Changes Required:

##### 1. Create Loop Pattern Modules
**Files**: `src/yawl_pnet_structured_loop.erl`, `src/yawl_pnet_recursion.erl` (NEW)
**Changes**: Implement loops using token recycling.

```erlang
trigger(loop_body, [Token], #state{condition = Cond, iteration = N}) ->
    case evaluate_condition(Cond, Token) of
        true ->
            {ok, [Token], #state{iteration = N + 1}};  % Loop continues
        false ->
            {ok, [], #state{complete = true}}  % Exit loop
    end.
```

##### 2. Create Critical Section Module
**File**: `src/yawl_pnet_critical_section.erl` (NEW)
**Changes**: Implement mutual exclusion using inhibitor arcs.

```erlang
inhibit_info(#state{lock_held = true}) ->
    %% Inhibit critical_section entry while lock is held
    #{critical_section_entry => true};

trigger(acquire_lock, [Token], State) ->
    {ok, [Token], State#state{lock_held = true}};

trigger(release_lock, [Token], State) ->
    {ok, [Token], State#state{lock_held = false}}.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All extended control patterns implemented
- [ ] Loops terminate correctly
- [ ] Critical sections enforce mutual exclusion

---

### Phase 6: Data Flow and Resource Patterns (Week 6)

#### Overview
Implement WDP-01 through WDP-05 (param_pass, data_transform, data_distribute, data_accumulate, data_visibility) and WRP-01 through WRP-05 (resource_create, role_allocate, resource_start, role_distribute, capability_allocate).

#### Changes Required:

##### 1. Implement Colored Token Data Transformation
**Pattern**: All data flow patterns use gen_pnet's token_data callback.

```erlang
trigger(transform, [InputToken], #state{transform_fn = Fun}) ->
    %% Transform token data through the pipeline
    TransformedData = Fun(InputToken),
    OutputToken = InputToken#token{data = TransformedData},
    {ok, [OutputToken], State}.
```

##### 2. Create Resource Pattern Modules
**Files**: `src/yawl_pnet_resource_*.erl` (NEW)
**Changes**: Implement resource allocation using token attributes.

```erlang
trigger(allocate_resource, [Token], #state{resources = Resources}) ->
    case find_available_resource(Resources, Token) of
        {ok, ResourceId} ->
            OutputToken = Token#token{allocated_resource = ResourceId},
            {ok, [OutputToken], mark_resource_busy(ResourceId, State)};
        none ->
            {ok, [], State}  % Hold token until resource available
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All data flow patterns implemented
- [ ] Token data transformations preserve data correctly
- [ ] Resource allocation prevents double-booking

---

### Phase 7: Exception Handling Patterns (Week 6-7)

#### Overview
Implement WHP-01 through WHP-05: error_handler, retry, compensate, triggered_compensation, consecutive_compensate.

#### Changes Required:

##### 1. Create Exception Pattern Modules
**Files**: `src/yawl_pnet_exception_*.erl` (NEW)
**Changes**: Implement exception handling using inhibitor arcs and special error tokens.

```erlang
trigger(handle_error, [ErrorToken], #state{handler_fn = Handler}) ->
    case catch Handler(ErrorToken) of
        {ok, RecoveredData} ->
            {ok, [create_normal_token(RecoveredData)], State};
        {error, _} ->
            {ok, [], State#state{failed = true}}
    end.

trigger(retry, [ErrorToken], #state{retry_count = N, max_retries = Max}) when N < Max ->
    {ok, [create_retry_token(ErrorToken)], #state{retry_count = N + 1}};
trigger(retry, [_ErrorToken], #state{retry_count = N, max_retries = Max}) when N >= Max ->
    {ok, [], State#state{failed = true}}.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All exception handling patterns implemented
- [ ] Retry attempts respect max_retries limit
- [ ] Compensation reverses state correctly

---

### Phase 8: Engine Migration and Integration (Week 7)

#### Overview
Replace `yawl_engine.erl` gen_server with gen_pnet-based workflow management.

#### Changes Required:

##### 1. Create New gen_pnet-Based Engine
**File**: `src/yawl_pnet_engine.erl` (NEW)
**Changes**: New engine that uses gen_pnet for workflow instances.

```erlang
-module(yawl_pnet_engine).
-behaviour(gen_pnet).

%% Workflow is a composition of pattern modules
-record(state, {
    cases = #{} :: #{case_id() => pid()},  % gen_pnet PIDs
    next_case_id = 1
}).

init(_Arg) ->
    {ok, #state{}}.

place_info(_State) ->
    #{
        workflow_start => {0, unlimited},
        workflow_complete => {0, unlimited}
    }.

transition_info(_State) ->
    #{
        start_workflow => {[workflow_start], [running]},
        complete_workflow => {[running], [workflow_complete]}
    }.

%% API to start a workflow case
start_workflow(Spec, Options) ->
    %% Convert workflow spec to gen_pnet module
    {ok, Module, InitArg} = workflow_spec_to_pnet(Spec),
    case gen_pnet:start_link(Module, InitArg, Options) of
        {ok, CasePid} ->
            %% Inject start token
            gen_pnet:trigger(CasePid, start_workflow, #{}),
            {ok, CasePid};
        {error, Reason} ->
            {error, Reason}
    end.
```

##### 2. Update API Compatibility Layer
**File**: `src/yawl_engine.erl`
**Changes**: Add backward-compatible wrapper functions.

```erlang
%% Keep existing API, delegate to new gen_pnet engine
start_workflow(Engine, Spec, Options) ->
    yawl_pnet_engine:start_workflow(Spec, Options).

complete_workitem(Engine, WorkItemId, Results) ->
    %% Find the gen_pnet case PID for this workitem
    {ok, CasePid} = find_case_by_workitem(WorkItemId),
    gen_pnet:trigger(CasePid, {complete_workitem, WorkItemId}, Results).
```

##### 3. Update Persistence for gen_pnet State
**File**: `src/yawl_persistence.erl`
**Changes**: Save/restore gen_pnet net_state instead of custom maps.

```erlang
%% Add functions
save_case_state(CaseId, PnetPid) ->
    {ok, PnetState} = gen_pnet:net_state(PnetPid),
    save_pnet_state(CaseId, PnetState).

restore_case_state(CaseId) ->
    {ok, PnetState} = load_pnet_state(CaseId),
    gen_pnet:restore_state(PnetState).
```

#### Success Criteria:

##### Automated Verification:
- [ ] New gen_pnet engine passes all existing engine tests
- [ ] All 29 test files pass without modification: `rebar3 eunit`
- [ ] Backward compatibility verified with cre_yawl API tests

##### Manual Verification:
- [ ] End-to-end workflow execution works
- [ ] Persistence survives node restart
- [ ] Performance benchmarks meet or exceed current implementation

---

## Testing Strategy

### Unit Tests:
- Each gen_pnet pattern module has dedicated unit tests
- Tests verify token semantics match YAWL specification
- Tests compare output against current implementation

### Integration Tests:
- Workflow compositions using multiple pattern types
- Cancellation behavior in complex workflows
- Data flow through multi-step pipelines
- Resource allocation and cleanup

### Manual Testing Steps:
1. Execute each of the 43 patterns individually with sample data
2. Combine patterns into representative workflows
3. Test cancellation at various workflow stages
4. Verify persistence through restart cycles
5. Load test with high-concurrency scenarios

### Performance Benchmarks:
- Measure token passing throughput for each pattern
- Compare memory usage vs current implementation
- Profile hot paths and optimize as needed

## Migration Notes

### Compatibility Strategy:
1. **Parallel Deployment**: Run both old and new implementations side-by-side
2. **Feature Flags**: Allow per-workflow selection of implementation
3. **API Compatibility**: All existing `yawl_engine` and `cre_yawl` functions continue to work
4. **State Migration**: Script to convert existing Mnesia records to gen_pnet format

### Rollback Plan:
- Keep old `yawl_engine` code as `yawl_engine_legacy`
- Feature flag can revert to legacy implementation
- Database migration is reversible

## References
- `/Users/sac/cre/rebar.config` - gen_pnet dependency definition (lines 8-9)
- `/Users/sac/cre/src/yawl_engine.erl` - Current gen_server-based engine
- `/Users/sac/cre/src/cre_yawl_patterns.erl` - Pattern implementations with gen_pnet behavior (line 65)
- `/Users/sac/cre/src/yawl_executor.erl` - Pattern execution dispatch logic
- `/Users/sac/cre/src/cre_yawl.erl` - Workflow construction API (lines 146-169)
- `/Users/sac/cre/test/` - 29 test files covering all YAWL functionality
