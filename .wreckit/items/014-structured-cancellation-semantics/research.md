# Research: Structured cancellation semantics

**Date**: 2025-01-11
**Item**: 014-structured-cancellation-semantics

## Research Question
Workflows need predictable, safe cancellation at different granularities without corrupting state in other parts of the workflow. Need efficient propagation without scanning entire graphs.

**Motivation:** Provides clean error handling and timeout semantics, enables resource cleanup through compensation, supports long-running workflow management, and is essential for production reliability.

**Success criteria:**
- Cancellation terminates targeted scopes
- Unrelated scopes remain uncorrupted
- Compensation hooks execute correctly
- Cancellation propagation is O(scope size)

**Technical constraints:**
- Three scope types: activity, case, region
- Efficient propagation (no whole-graph scanning)
- Integration with cancellation tokens in exec state

**Signals:** priority: high, urgency: Core reliability feature required for production use

## Summary

CRE (Common Runtime Environment) is a YAWL workflow engine built on Erlang/OTP with **Petri nets as its formal foundation**. The system follows the Joe Armstrong design philosophy: "one real OTP runner (gen_pnet), everything else pure helpers/utilities."

**Key Finding:** CRE already has a **partial cancellation implementation** but lacks **structured, scope-based cancellation semantics** with efficient propagation. The existing implementation:

1. **Has basic cancellation token support** via `wf_cancel` module (`/Users/sac/cre/src/wf/wf_cancel.erl:21-435`)
2. **Implements cancel activity (WCP-19)** and **cancel case (WCP-20)** patterns
3. **Lacks scope-based propagation** - cancellation is currently token-triggered only
4. **No integration with execution state** - cancellation tokens are processed but not tied to workflow lifecycle
5. **Missing compensation hooks** - no resource cleanup on cancellation

**What needs to be done:**
1. Implement **scope-aware cancellation** that understands activity/case/region boundaries
2. Add **cancellation token propagation** through workflow execution state (`yawl_state`)
3. Implement **compensation hooks** that execute on scope cancellation
4. Ensure **O(scope size) cancellation** without scanning entire workflow graph
5. Integrate with **gen_pnet trigger/3 callback** for efficient token filtering

## Current State Analysis

### Existing Implementation

#### 1. Cancellation Token Infrastructure (`wf_cancel`)

**File:** `/Users/sac/cre/src/wf/wf_cancel.erl`

The `wf_cancel` module provides a **pure functional** API for managing cancellation tokens:

```erlang
%% Token type (line 112)
-type cancel_token() :: {cancel, [atom()]}.

%% Token creation (lines 187-192)
-spec create_cancel_token(Target :: atom() | [atom()]) -> cancel_token().
create_cancel_token(Target) when is_atom(Target) ->
    {cancel, [Target]};
create_cancel_token(Targets) when is_list(Targets) ->
    {cancel, Targets}.

%% Apply cancellation to marking (lines 237-244)
-spec apply_cancellation(Marking :: marking(), CancelSet :: cancellation_set()) ->
    marking().
apply_cancellation(Marking, CancelSet) when is_map(Marking), is_list(CancelSet) ->
    lists:foldl(fun(Place, Acc) ->
        Acc#{Place => []}
    end, Marking, CancelSet).
```

**Status:** ✅ Complete - Pure functional, well-tested, handles token validation and application

**Limitations:**
- No understanding of scope boundaries (activity/case/region)
- No propagation - only clears tokens in explicitly listed places
- No compensation hooks
- Token-based only - no integration with workflow lifecycle

#### 2. Cancellation Runtime (`yawl_cancel_runtime`)

**File:** `/Users/sac/cre/src/wf/yawl_cancel_runtime.erl`

Provides runtime processing of cancellation tokens:

```erlang
%% Main entry point (lines 275-294)
-spec process_cancel_tokens(Marking :: marking()) -> cancel_result().
process_cancel_tokens(Marking) ->
    CancelSets = extract_all_cancel_sets(Marking),
    {UpdatedMarking, CancelledRegions} = lists:foldl(
        fun(CancelSet, {AccMarking, AccRegions}) ->
            NewMarking = apply_cancellation(AccMarking, CancelSet),
            {NewMarking, [CancelSet | AccRegions]}
        end,
        {Marking, []},
        CancelSets
    ),
    {UpdatedMarking, lists:reverse(CancelledRegions)}.
```

**Status:** ✅ Functional - Scans marking for cancel tokens and applies them

**Limitations:**
- **O(N) where N = total places in marking** - scans entire marking
- No scope awareness - treats all places equally
- No integration with workflow specification (cancellation regions defined in XML but not used at runtime)

#### 3. Cancellation Pattern Implementations

**Cancel Activity (WCP-19):** `/Users/sac/cre/src/patterns/cancel_activity.erl`
```erlang
-record(state, {
    target :: atom(),
    cancel_event :: atom(),
    cancelled = false :: boolean()
}).

fire(t_cancel, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{cancelled = true},
    {produce, #{p_cancelled => [cancelled]}, NewState};
```

**Cancel Case (WCP-20):** `/Users/sac/cre/src/patterns/cancel_case.erl`
- Similar structure, cancels entire workflow case

**Status:** ⚠️ Pattern-level implementation only
- Patterns exist but are standalone
- No integration with general workflow execution
- No scope hierarchy (activity → region → case)

#### 4. Workflow State Management (`yawl_state`)

**File:** `/Users/sac/cre/src/wf/yawl_state.erl`

Pure functional state tracking for workflow cases:

```erlang
%% State includes status tracking (lines 202-208)
-type t() :: #{
    case_id := case_id(),
    status := status(),  % created | running | suspended | completed | cancelled
    workitems => workitems(),
    data => data(),
    timestamps => timestamps()
}.

%% Cancel status (lines 411-414, 897-901)
-spec is_cancelled(State :: t()) -> boolean().
is_cancelled(#{status := cancelled}) -> true;
is_cancelled(_) -> false.

-spec mark_cancelled(State :: t()) -> t().
mark_cancelled(State) ->
    S1 = set_timestamp(State, cancelled_at),
    S1#{status => cancelled}.
```

**Status:** ✅ Complete - Has cancellation status in state lifecycle

**Gap:** Status exists but **no mechanism to trigger it** from cancellation tokens

#### 5. Scope Boundary Mapping (`wf_scope`)

**File:** `/Users/sac/cre/src/wf/wf_scope.erl`

Maps parent-child place relationships for subflows:

```erlang
%% Enter scope (lines 128-146)
-spec enter(BindingTable :: binding_table(),
           ScopeId :: scope_id(),
           ParentDeltaOrMarking :: input()) ->
          produce_map().

enter(BindingTable, ScopeId, ParentDeltaOrMarking) ->
    case maps:get(ScopeId, BindingTable, undefined) of
        undefined ->
            normalize_to_produce_map(ParentDeltaOrMarking);
        Mapping when is_map(Mapping) ->
            translate_places(ParentDeltaOrMarking, Mapping)
    end.
```

**Status:** ✅ Complete - Handles place translation across scope boundaries

**Gap:** **Not used for cancellation** - could be the basis for scope-aware propagation

#### 6. YAWL Specification Parser (`wf_spec`)

**File:** `/Users/sac/cre/src/wf/wf_spec.erl`

Parses YAWL XML specifications including cancellation regions:

```erlang
%% Cancellation set extraction (lines 511-517)
-spec cancellation_set(Spec :: yawl_spec(), TaskId :: task_id()) -> [task_id()].
cancellation_set(#yawl_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{cancellation_set = CancelSet} -> CancelSet;
        _ -> []
    end.

%% All cancellation regions (lines 526-536)
-spec cancellation_regions(Spec :: yawl_spec()) ->
          [{task_id(), [task_id()]}].
cancellation_regions(#yawl_spec{tasks = Tasks}) ->
    maps:fold(fun(_TaskId, #task_info{cancellation_set = []}, Acc) ->
            Acc;
        (TaskId, #task_info{cancellation_set = CancelSet}, Acc) ->
            [{TaskId, CancelSet} | Acc];
        (_, _, Acc) ->
            Acc
    end, [], Tasks).
```

**Status:** ✅ Parses cancellation regions from YAWL XML

**Gap:** Parsed but **not used at runtime** - `yawl_cancel_runtime:should_cancel/3` exists but isn't integrated

### Key Files

| File | Lines | Purpose | Status |
|------|-------|---------|--------|
| `/Users/sac/cre/src/wf/wf_cancel.erl` | 21-435 | Cancellation token operations | ✅ Complete |
| `/Users/sac/cre/src/wf/yawl_cancel_runtime.erl` | 1-737 | Token processing runtime | ⚠️ No scope awareness |
| `/Users/sac/cre/src/wf/yawl_state.erl` | 1-1321 | Workflow case state | ✅ Has cancel status |
| `/Users/sac/cre/src/wf/wf_scope.erl` | 1-319 | Scope boundary mapping | ✅ Complete |
| `/Users/sac/cre/src/wf/wf_spec.erl` | 1-1569 | YAWL spec parser | ✅ Parses cancellation |
| `/Users/sac/cre/src/patterns/cancel_activity.erl` | 1-75 | WCP-19 pattern | ⚠️ Standalone |
| `/Users/sac/cre/src/patterns/cancel_case.erl` | 1-73 | WCP-20 pattern | ⚠️ Standalone |
| `/Users/sac/cre/docs/ARCHITECTURE.md` | 1-300 | System architecture | ✅ gen_pnet foundation |

## Technical Considerations

### Dependencies

#### Internal Modules to Integrate

1. **gen_pnet** (Core OTP behavior)
   - **File:** `/Users/sac/cre/src/core/gen_pnet.erl` (referenced in ARCHITECTURE.md:89)
   - **Purpose:** Single OTP runner maintaining Petri net state
   - **Integration:** Use `trigger/3` callback to filter cancellation tokens
   - **Key insight:** Only gen_pnet scans markings - pure modules don't

2. **wf_cancel** (Pure token utilities)
   - **File:** `/Users/sac/cre/src/wf/wf_cancel.erl:21-435`
   - **Purpose:** Token validation and cancellation application
   - **Usage:** Extend to support scope-aware cancellation sets

3. **yawl_state** (Pure state management)
   - **File:** `/Users/sac/cre/src/wf/yawl_state.erl:1-1321`
   - **Purpose:** Track workflow case status including `cancelled`
   - **Integration:** Trigger status changes when cancellation tokens processed

4. **wf_scope** (Pure boundary mapping)
   - **File:** `/Users/sac/cre/src/wf/wf_scope.erl:1-319`
   - **Purpose:** Maps parent-child place relationships
   - **Usage:** Translate cancellation sets across scope boundaries

5. **wf_spec** (Pure spec parser)
   - **File:** `/Users/sac/cre/src/wf/wf_spec.erl:511-536`
   - **Purpose:** Extract cancellation regions from YAWL XML
   - **Integration:** Use parsed regions for runtime cancellation decisions

#### External Dependencies

- **lib_combin** (from joergen7/lib_combin)
  - Used for deterministic nondeterminism in transition selection
  - Not directly relevant to cancellation but part of gen_pnet ecosystem

### Patterns to Follow

#### 1. Pure Functional Design

**Convention:** All modules except `gen_pnet`/`gen_yawl` are **pure functions**

**Example from `wf_cancel`:**
```erlang
%% Pure function - no side effects
-spec apply_cancellation(Marking :: marking(), CancelSet :: cancellation_set()) ->
    marking().
apply_cancellation(Marking, CancelSet) ->
    lists:foldl(fun(Place, Acc) ->
        Acc#{Place => []}
    end, Marking, CancelSet).
```

**Implication:** Cancellation scope logic must be pure - no process state

#### 2. Token-Based Communication

**Convention:** State changes flow through token production/consumption

**Example from `yawl_cancel_runtime`:**
```erlang
%% Cancellation signaled by special tokens
Marking = #{
    trigger => [{cancel, [p2, p3]}],  %% Cancel token
    p1 => [a],
    p2 => [b, c]
}.
```

**Implication:** Cancellation must be token-triggered, not message-based

#### 3. Callback-Based Architecture

**Convention:** `gen_pnet` callbacks define workflow behavior

**From ARCHITECTURE.md:93-108:**
```erlang
%% Structure callbacks
place_lst/0          % Places in the net
trsn_lst/0           % Transitions in the net
init_marking/2        % Initial tokens
preset/1              % Transition wiring
is_enabled/3         % Guard conditions
fire/3               % Token production
trigger/3            % Token filtering
```

**Implication:** Use `trigger/3` to intercept cancellation tokens

#### 4. 3-Tuple Fire Returns (gen_yawl)

**Convention:** Extended `fire/3` can update `usr_info`

**From ARCHITECTURE.md:116-122:**
```erlang
%% Standard 2-tuple (gen_pnet compatible)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap}

%% Enhanced 3-tuple (gen_yawl extension)
fire(Trsn, Mode, UsrInfo) -> {produce, ProduceMap, NewUsrInfo}
```

**Implication:** Can update workflow state when cancellation fires

### Integration Points

#### 1. Trigger Callback (gen_pnet)

**Purpose:** Filter tokens as they're produced

**Current usage:** Returns `pass` or `drop`

**Proposed usage:** Intercept `{cancel, _}` tokens, translate to scope operations

```erlang
%% In workflow pattern module
trigger(_Place, {cancel, CancelSet}, NetState) ->
    %% Process cancellation
    {pass, [{cancel, CancelSet}], NewNetState};
trigger(Place, Token, NetState) ->
    pass.
```

#### 2. Fire Callback (gen_yawl)

**Purpose:** Produce tokens when transition fires

**Proposed usage:** Emit cancellation tokens on timeout/error

```erlang
fire(t_timeout, _Mode, UsrInfo) ->
    %% Cancel downstream activities
    {produce, #{p_cancel => [{cancel, [p_activity1, p_activity2]}]}, UsrInfo};
```

#### 3. Spec Integration (wf_spec)

**Purpose:** Extract cancellation regions from YAWL XML

**Proposed usage:** Build cancellation scope map at workflow start

```erlang
%% At workflow initialization
{ok, Spec} = wf_spec:from_xml(Xml),
CancelRegions = wf_spec:cancellation_regions(Spec),
%% Store in usr_info for runtime use
UsrInfo = #{cancel_regions => CancelRegions}.
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Scope boundary complexity** - Activity/region/case hierarchy may be ambiguous | High | Use explicit scope IDs in `wf_scope` binding table; validate hierarchy at spec parse time |
| **Performance - O(N) token scanning** - Current implementation scans entire marking | High | Leverage `gen_pnet`'s `trigger/3` callback to filter tokens **before** they enter marking (O(1) per token) |
| **Compensation hook ordering** - Multiple hooks may fire in undefined order | Medium | Define total ordering: innermost scope first, post-order traversal; enforce through type system |
| **State corruption on cascade** - Nested cancellations may leave inconsistent state | High | Use **pure functional** cancellation - always return new marking, never mutate; validate invariants post-cancellation |
| **Missing scope metadata** - Parsed YAWL specs may lack cancellation region info | Medium | Provide default behavior: cancel only immediate descendants; require explicit scope declarations for complex hierarchies |
| **Integration with gen_pnet** - May require changes to core behavior | Medium | Work within existing `trigger/3` callback; avoid gen_pnet modifications; use `usr_info` for scope tracking |

## Recommended Approach

### High-Level Strategy

Based on the research, implement **structured cancellation semantics** in three phases:

#### Phase 1: Scope-Aware Cancellation Tokens

**Goal:** Extend `wf_cancel` to understand scope boundaries

1. **Define scope types**
   ```erlang
   -type cancel_scope() :: {activity, atom()} |
                          {region, [atom()]} |
                          {case, all}.
   ```

2. **Extend `wf_cancel` module**
   - Add `create_scope_cancel/2` - create token for specific scope
   - Add `resolve_scope/3` - translate scope to concrete places using binding table
   - Keep pure functional design - no state mutation

3. **Integrate with `wf_scope`**
   - Use existing `binding_table` to map parent-child relationships
   - Resolve activity → region → case hierarchy

#### Phase 2: Runtime Cancellation Processing

**Goal:** Integrate with `gen_pnet` for efficient propagation

1. **Leverage `trigger/3` callback**
   - Intercept cancellation tokens as they're produced
   - Resolve scope to affected places
   - Update `usr_info` with cancellation status

2. **Avoid marking scanning**
   - Current `yawl_cancel_runtime:process_cancel_tokens/1` scans entire marking (O(N))
   - New approach: Filter at token production time (O(1) per token)

3. **Update workflow state**
   - Call `yawl_state:mark_cancelled/1` when case cancelled
   - Track active cancellations in `usr_info`

#### Phase 3: Compensation Hooks

**Goal:** Execute cleanup on scope cancellation

1. **Define hook type**
   ```erlang
   -type compensation_hook() :: fun((Scope::cancel_scope(), Reason::term()) -> ok).
   ```

2. **Register hooks in `usr_info`**
   ```erlang
   UsrInfo = #{
       compensation_hooks => #{
           {activity, task1} => fun cleanup_task1/2,
           {region, [a, b, c]} => fun cleanup_region/2
       }
   }.
   ```

3. **Execute on cancellation**
   - When `trigger/3` intercepts cancel token, look up hooks
   - Execute in post-order (deepest scope first)
   - Handle hook failures gracefully (log, continue)

### Implementation Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    YAWL Workflow Spec                        │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Cancellation Regions (from XML)                       │ │
│  │  - Task → Cancel Set mappings                          │ │
│  │  - Scope hierarchy definitions                         │ │
│  └────────────────────────────────────────────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              gen_yawl Workflow Execution                     │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  usr_info                                              │ │
│  │  - cancel_regions: [{TaskId, CancelSet}]               │ │
│  │  - compensation_hooks: #{Scope => HookFun}             │ │
│  │  - active_cancellations: [Scope]                       │ │
│  └────────────────────────────────────────────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                    gen_pnet Progress Loop                    │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  1. Fire transition (fire/3)                           │ │
│  │     └─ Returns {produce, Tokens}                       │ │
│  │                                                          │ │
│  │  2. Call trigger/3 for each token                      │ │
│  │     ┌────────────────────────────────────────────────┐ │ │
│  │     │ trigger(Place, {cancel, Scope}, NetState)      │ │ │
│  │     │ └─ Resolve scope to places                     │ │ │
│  │     │ └─ Execute compensation hooks                   │ │ │
│  │     │ └─ Update yawl_state (mark_cancelled)           │ │ │
│  │     │ └─ Return {pass, [{cancel, ResolvedPlaces}]}    │ │ │
│  │     └────────────────────────────────────────────────┘ │ │
│  │                                                          │ │
│  │  3. Add passed tokens to marking                       │ │
│  └────────────────────────────────────────────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              wf_cancel (Pure Token Operations)               │
│  - create_scope_cancel/2   - Create scoped cancel token     │
│  - resolve_scope/3          - Translate scope to places     │
│  - apply_cancellation/2     - Clear tokens from places      │
│  - execute_compensation/2   - Run cleanup hooks             │
└─────────────────────────────────────────────────────────────┘
```

### Performance Considerations

**O(scope size) requirement achieved through:**

1. **Token-level filtering** (`trigger/3`)
   - Each cancellation token processed in O(1) amortized
   - No marking scanning required

2. **Scope resolution** (pure function)
   - Lookup in binding table: O(log N) where N = scopes in workflow
   - Typically N < 100 for real workflows

3. **Compensation execution**
   - Hooks execute in O(H) where H = scope hierarchy depth
   - Post-order traversal ensures single pass

**Total complexity:** O(N_tokens × log N_scopes + N_hooks × H_depth)

For typical workflows:
- N_tokens: 1-10 cancellation events
- N_scopes: 10-50 scopes
- H_depth: 3-5 levels
- **Result:** Sub-millisecond cancellation propagation

## Open Questions

1. **Scope hierarchy representation**
   - **Question:** Should scope hierarchy be explicitly modeled (tree structure) or derived from binding table?
   - **Impact:** Affects implementation complexity and validation strategy
   - **Options:**
     - Explicit: `#{parent => [child1, child2]}`
     - Derived: Infer from `wf_scope` binding table
   - **Recommendation:** Start with derived, add explicit if needed

2. **Compensation hook failure handling**
   - **Question:** What happens when a compensation hook fails?
   - **Impact:** Affects reliability and error recovery
   - **Options:**
     - Abort cancellation (risky - may leave inconsistent state)
     - Log and continue (may hide errors)
     - Retry with backoff (complexity)
   - **Recommendation:** Log and continue with telemetry

3. **Cancellation token persistence**
   - **Question:** Should cancellation tokens persist in marking after processing?
   - **Impact:** Affects debugging and audit trail
   - **Options:**
     - Remove after processing (cleaner state)
     - Keep as audit trail (better debugging)
   - **Recommendation:** Keep, add `processed_at` timestamp

4. **Integration with timeout subsystem**
   - **Question:** How does cancellation integrate with existing `wf_time` module?
   - **Impact:** Timeout-triggered cancellations
   - **Investigation needed:** Check `wf_time` module for timeout token format
   - **Recommendation:** Timeout tokens should emit `{cancel, Scope}` tokens

5. **Multi-instance task cancellation**
   - **Question:** How to cancel individual instances vs. entire multi-instance activity?
   - **Impact:** Granularity of control
   - **Investigation needed:** Check `wf_multi_instance` module for instance tracking
   - **Recommendation:** Support both granularities via scope type

6. **Testing strategy**
   - **Question:** How to verify O(scope size) performance?
   - **Impact:** Validation of success criteria
   - **Approach:**
     - Unit tests for pure functions (wf_cancel)
     - Property-based testing for cancellation invariants
     - Performance benchmarks with workflow size scaling
     - Concurrency testing with race conditions
   - **Recommendation:** Add to existing EUnit test suite

## Next Steps

1. **Clarify scope hierarchy representation** (Open Question #1)
2. **Review `wf_time` module** for integration points (Open Question #4)
3. **Design compensation hook type system** (Open Question #2)
4. **Prototype scope resolution** using `wf_scope` binding table
5. **Add cancellation token tests** to existing test suite
