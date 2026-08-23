# Structured Cancellation Semantics Implementation Plan

## Implementation Plan Title
Scope-Aware Cancellation with Efficient Propagation and Compensation Hooks

## Overview
Implement structured cancellation semantics for YAWL workflows that provides predictable, safe cancellation at activity/region/case granularities. The solution leverages the existing `gen_pnet:trigger/3` callback to achieve O(scope size) performance without scanning entire markings, while adding compensation hooks for resource cleanup.

## Current State
CRE has basic cancellation token infrastructure but lacks structured, scope-based cancellation:

- **`wf_cancel`** (lines 21-435): Pure functional token operations, but only handles explicit place lists
  - `create_cancel_token/1`: Creates `{cancel, [atom()]}` tokens
  - `apply_cancellation/2`: Clears tokens from specified places
  - **Gap**: No scope awareness (activity/region/case)

- **`yawl_cancel_runtime`** (lines 275-294): Token processing runtime
  - `process_cancel_tokens/1`: Scans entire marking for cancel tokens (O(N))
  - **Gap**: No integration with workflow lifecycle, inefficient whole-marking scan

- **`yawl_state`** (lines 897-901): State management with cancellation status
  - `mark_cancelled/1`: Sets workflow status to `cancelled`
  - `is_cancelled/1`: Checks if workflow is cancelled
  - **Gap**: No mechanism to trigger from cancellation tokens

- **`wf_spec`** (lines 511-536): Parses cancellation regions from YAWL XML
  - `cancellation_regions/1`: Extracts task → cancel set mappings
  - **Gap**: Parsed but not used at runtime

- **`wf_scope`** (lines 128-146): Scope boundary mapping
  - `enter/3`, `leave/3`: Translate places across scope boundaries
  - **Gap**: Not used for cancellation propagation

- **Pattern modules** (`cancel_activity.erl`, `cancel_case.erl`): Standalone implementations
  - `trigger/3`: Currently just returns `pass` (lines 74, 72)
  - **Gap**: No integration with general workflow execution

## Desired End State

### Functional Requirements
1. **Scope-aware cancellation**: Three types (activity, region, case) with automatic scope resolution
2. **Efficient propagation**: O(scope size) via `trigger/3` callback, no whole-marking scans
3. **Compensation hooks**: Execute cleanup on scope cancellation with post-order traversal
4. **Workflow state integration**: Automatic `yawl_state:mark_cancelled/1` on case cancellation
5. **Specification integration**: Use parsed cancellation regions from YAWL XML

### Non-Functional Requirements
1. **Performance**: O(scope size) = sub-millisecond for typical workflows (N_tokens × log N_scopes + N_hooks × H_depth)
2. **Pure functional**: All cancellation logic in pure modules, only `gen_yawl` maintains state
3. **Backward compatible**: Existing cancellation tokens still work, new scope types additive
4. **Testable**: Each component independently testable with EUnit

### Key Discoveries
- **Line 161-167 in `gen_pnet.erl`**: `trigger/3` callback signature allows token filtering during production
- **Line 81-84 in `gen_yawl.erl`**: 3-tuple `fire/3` return enables automatic `usr_info` updates
- **Line 401-427 in `yawl_cancel_runtime.erl`**: Current O(N) scan in `extract_all_cancel_sets/1` must be avoided
- **Line 526-536 in `wf_spec.erl`**: Cancellation regions already parsed but unused
- **Line 128-146 in `wf_scope.erl`**: Binding table can be reused for scope resolution

## What We're NOT Doing
- ❌ Modifying `gen_pnet` behavior - working within existing `trigger/3` callback
- ❌ Message-based cancellation - staying token-based for consistency
- ❌ New compensation hook DSL - using simple fun() functions
- ❌ Distributed cancellation - single workflow instance only
- ❌ Persistent cancellation tokens - tokens remain in marking for audit trail
- ❌ Automatic retry of failed compensation hooks - log and continue strategy

## Implementation Approach

### High-Level Strategy
Implement structured cancellation in three incremental phases, each independently testable:

1. **Phase 1**: Extend `wf_cancel` with scope types and resolution logic
2. **Phase 2**: Integrate with `gen_yawl:trigger/3` for efficient propagation
3. **Phase 3**: Add compensation hook infrastructure

### Key Design Decisions
1. **Scope hierarchy**: Derived from `wf_scope` binding table (not explicit tree structure)
2. **Hook failure handling**: Log error with telemetry, continue execution
3. **Token format**: Extended to `{cancel, {scope_type, scope_id}}` for backward compatibility
4. **Integration point**: `trigger/3` callback (not new gen_server messages)
5. **Performance**: O(1) per token via direct lookup, O(log N) scope resolution

---

## Phases

### Phase 1: Scope-Aware Cancellation Tokens

#### Overview
Extend `wf_cancel` module to understand and resolve scope boundaries (activity/region/case) to concrete place lists, enabling cancellation at semantic levels instead of just explicit places.

#### Changes Required:

##### 1. wf_cancel.erl
**File**: `/Users/sac/cre/src/wf/wf_cancel.erl`

**Changes**:
1. Add scope type definitions
2. Add scope creation functions
3. Add scope resolution using binding table
4. Keep pure functional design

```erlang
%%====================================================================
%% New Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cancellation scope types.
%%
%% Three granularity levels:
%% - `{activity, TaskId}`: Cancel single task/activity
%% - `{region, RegionId}`: Cancel all tasks in region
%% - `{case, all}`: Cancel entire workflow case
%%--------------------------------------------------------------------
-type cancel_scope() :: {activity, atom()} |
                       {region, atom()} |
                       {case, all}.

%%--------------------------------------------------------------------
%% @doc Extended cancel token supporting both legacy and scope-based formats.
%%
%% Legacy format: {cancel, [Place]} for backward compatibility
%% Scope format: {cancel, {ScopeType, ScopeId}} for structured cancellation
%%--------------------------------------------------------------------
-type cancel_token() :: {cancel, cancellation_set()} | % legacy
                       {cancel, cancel_scope()}.        % new

%% Export new types
-export_type([cancel_scope/0]).

%%====================================================================
%% Scope Creation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a cancellation token for an activity scope.
%%
%% Activity scope cancels a single task's places.
%%
%% ```erlang
%% > wf_cancel:create_activity_cancel(task1).
%% {cancel, {activity, task1}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec create_activity_cancel(TaskId :: atom()) -> cancel_token().
create_activity_cancel(TaskId) when is_atom(TaskId) ->
    {cancel, {activity, TaskId}}.

%%--------------------------------------------------------------------
%% @doc Creates a cancellation token for a region scope.
%%
%% Region scope cancels all places within a named region.
%%
%% ```erlang
%% > wf_cancel:create_region_cancel(payment_region).
%% {cancel, {region, payment_region}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec create_region_cancel(RegionId :: atom()) -> cancel_token().
create_region_cancel(RegionId) when is_atom(RegionId) ->
    {cancel, {region, RegionId}}.

%%--------------------------------------------------------------------
%% @doc Creates a cancellation token for case scope.
%%
%% Case scope cancels the entire workflow.
%%
%% ```erlang
%% > wf_cancel:create_case_cancel().
%% {cancel, {case, all}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec create_case_cancel() -> cancel_token().
create_case_cancel() ->
    {cancel, {case, all}}.

%%====================================================================
%% Scope Resolution Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Resolves a cancellation scope to a concrete list of places.
%%
%% Uses the binding table (from wf_spec) to map scope identifiers to
%% actual place atoms in the Petri net. Returns empty list for unknown
%% scopes to maintain totality.
%%
%% ```erlang
%% > BT = #{task1 => #{p1_in => child_p1_in, p1_out => child_p1_out}}.
%% > wf_cancel:resolve_scope({activity, task1}, BT, Spec).
%% [child_p1_in, child_p1_out]
%%
%% > wf_cancel:resolve_scope({case, all}, BT, Spec).
%% [p1, p2, p3, ...]  % all places in workflow
%% ```
%% @end
%%--------------------------------------------------------------------
-spec resolve_scope(Scope :: cancel_scope(),
                   BindingTable :: wf_scope:binding_table(),
                   Spec :: wf_spec:yawl_spec()) -> [atom()].

resolve_scope({activity, TaskId}, BindingTable, Spec) ->
    %% Get places for this task from binding table
    case maps:get(TaskId, BindingTable, undefined) of
        undefined ->
            %% No binding - try spec fallback
            case wf_spec:task_places(Spec, TaskId) of
                undefined -> [];
                Places -> Places
            end;
        Mapping when is_map(Mapping) ->
            %% Extract all child places from the mapping
            maps:values(Mapping)
    end;

resolve_scope({region, RegionId}, _BindingTable, Spec) ->
    %% Get cancellation set from spec
    case wf_spec:cancellation_set(Spec, RegionId) of
        [] -> [];
        TaskIds ->
            %% Resolve each task to its places
            lists:flatmap(
                fun(TaskId) ->
                    resolve_scope({activity, TaskId}, _BindingTable, Spec)
                end,
                TaskIds
            )
    end;

resolve_scope({case, all}, _BindingTable, Spec) ->
    %% Return all places in the workflow specification
    wf_spec:all_places(Spec).

%%====================================================================
%% Enhanced Token Inspection
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extracts cancellation targets from token (legacy or scope).
%%
%% Returns empty list for scope tokens (must use resolve_scope/3).
%% For legacy tokens, returns the place list directly.
%%
%% ```erlang
%% > wf_cancel:cancel_targets({cancel, [p1, p2]}).
%% [p1, p2]
%%
%% > wf_cancel:cancel_targets({cancel, {activity, task1}}).
%% []  % scope token - use resolve_scope/3
%% ```
%% @end
%%--------------------------------------------------------------------
-spec cancel_targets(Token :: cancel_token() | term()) -> [atom()].

cancel_targets({cancel, Targets}) when is_list(Targets) ->
    %% Legacy token format
    Targets;
cancel_targets({cancel, {_, _}}) ->
    %% Scope token - must be resolved
    [];
cancel_targets(_) ->
    [].

%%====================================================================
%% Validation Updates
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid cancel token (legacy or scope).
%%
%% ```erlang
%% > wf_cancel:is_cancel_token({cancel, [p1, p2]}).
%% true
%%
%% > wf_cancel:is_cancel_token({cancel, {activity, task1}}).
%% true
%%
%% > wf_cancel:is_cancel_token({other, tuple}).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_cancel_token(term()) -> boolean().

is_cancel_token({cancel, Targets}) when is_list(Targets) ->
    %% Legacy format - validate all are atoms
    lists:all(fun(T) -> is_atom(T) end, Targets);
is_cancel_token({cancel, {activity, TaskId}}) when is_atom(TaskId) ->
    true;
is_cancel_token({cancel, {region, RegionId}}) when is_atom(RegionId) ->
    true;
is_cancel_token({cancel, {case, all}}) ->
    true;
is_cancel_token(_) ->
    false.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All existing tests pass: `erl -eval "eunit:test(wf_cancel, [verbose])"`
- [ ] New scope creation functions return correct token format
- [ ] `is_cancel_token/1` accepts both legacy and scope tokens
- [ ] `resolve_scope/3` returns correct place lists for all scope types
- [ ] Pure functional design maintained (no side effects in functions)

##### Manual Verification:
- [ ] Scope resolution works with sample YAWL specifications
- [ ] Legacy tokens still function correctly (backward compatibility)
- [ ] Empty binding table handled gracefully
- [ ] Unknown task/region IDs return empty list (total function)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Runtime Cancellation Processing

#### Overview
Integrate scope-aware cancellation with `gen_yawl:trigger/3` callback to achieve efficient O(scope size) propagation without scanning entire markings. Connect cancellation tokens to workflow state lifecycle.

#### Changes Required:

##### 1. gen_yawl.erl (wrapper updates)
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`

**Changes**:
1. Store cancellation regions in wrapper state from NetArg (already done at line 700-704)
2. Store spec and binding table in wrapper state for scope resolution
3. Add helper to extract from usr_info

```erlang
%%====================================================================
%% Updated wrapper_state record
%%====================================================================

-record(wrapper_state, {
    net_mod :: atom(),
    net_state :: term(),
    net_arg = #{} :: term(),
    fire_timeout = 5000 :: pos_integer(),
    progress_timeout = 30000 :: pos_integer(),
    shutting_down = false :: boolean(),
    active_fires = 0 :: non_neg_integer(),
    marking_history = [] :: [non_neg_integer()],
    max_marking_history = 10 :: non_neg_integer(),
    continue_count = 0 :: non_neg_integer(),
    max_continue = 1000 :: pos_integer(),
    regions = #{} :: #{binary() | atom() => [atom()]},
    checkpoint_interval = 0 :: non_neg_integer(),
    drain_step_count = 0 :: non_neg_integer(),
    %% NEW: Store spec and binding table for cancellation
    spec = undefined :: wf_spec:yawl_spec() | undefined,
    binding_table = #{} :: wf_scope:binding_table()
}).

%%====================================================================
%% Updated init/1
%%====================================================================

init({NetMod, NetArg, Options}) ->
    %% ... existing timeout initialization ...

    %% Initialize user info from the callback module
    UsrInfo = NetMod:init(NetArg),

    %% ... existing marking initialization ...

    %% ... existing net state creation ...

    %% Start the Petri net execution
    case proplists:get_value(auto_continue, Options, true) of
        true -> continue(self());
        false -> ok
    end,

    %% Extract regions from NetArg
    Regions = case NetArg of
        #{regions := R} when is_map(R) -> R;
        _ -> #{}
    end,

    %% NEW: Extract spec and binding table for cancellation
    Spec = case NetArg of
        #{spec := S} -> S;
        _ -> undefined
    end,

    BindingTable = case NetArg of
        #{binding_table := BT} when is_map(BT) -> BT;
        _ -> #{}
    end,

    %% Create wrapper state with spec and binding table
    WrapperState = #wrapper_state{
        net_mod = NetMod,
        net_state = NetState,
        net_arg = NetArg,
        fire_timeout = FireTimeout,
        progress_timeout = ProgressTimeout,
        shutting_down = false,
        active_fires = 0,
        marking_history = [],
        max_marking_history = MaxHistory,
        continue_count = 0,
        max_continue = MaxCont,
        regions = Regions,
        checkpoint_interval = CheckpointInterval,
        drain_step_count = 0,
        spec = Spec,
        binding_table = BindingTable
    },

    {ok, WrapperState}.
```

##### 2. wf_cancel_runtime.erl (add new trigger-based processing)
**File**: `/Users/sac/cre/src/wf/yawl_cancel_runtime.erl`

**Changes**:
1. Add `handle_cancel_token/4` for trigger callback integration
2. Add `mark_workflow_cancelled/2` to update yawl_state
3. Keep existing `process_cancel_tokens/1` for backward compatibility

```erlang
%%====================================================================
%% Exports - add new functions
%%====================================================================

%% Trigger-based cancellation processing
-export([handle_cancel_token/4, mark_workflow_cancelled/2]).

%%====================================================================
%% Trigger-Based Cancellation Processing
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Handles a cancellation token during trigger callback.
%%
%% This function is called by gen_yawl:trigger/3 when a cancellation
%% token is produced. It resolves scopes, executes compensation hooks,
%% and updates workflow state.
%%
%% Returns {pass, UpdatedNetState} to allow the token to proceed,
%% or {drop, UpdatedNetState} to prevent it from entering the marking.
%%
%% ```erlang
%% > NetState = #net_state{usr_info = UsrInfo, marking = Marking},
%% > wf_cancel_runtime:handle_cancel_token(p_trigger, {cancel, {activity, task1}},
%% ..                                       NetState, WrapperState).
%% {pass, NetState1}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec handle_cancel_token(Place :: atom(),
                         Token :: wf_cancel:cancel_token(),
                         NetState :: pnet_code:net_state(),
                         WrapperState :: term()) ->
    {pass, pnet_code:net_state()} |
    {drop, pnet_code:net_state()}.

handle_cancel_token(Place, {cancel, Scope}, NetState, WrapperState) ->
    %% Extract spec and binding table from wrapper state
    #wrapper_state{spec = Spec, binding_table = BindingTable} = WrapperState,

    case Spec of
        undefined ->
            %% No spec available - can't resolve scope, drop token
            {drop, NetState};
        _ ->
            %% Resolve scope to concrete places
            CancelSet = wf_cancel:resolve_scope(Scope, BindingTable, Spec),

            case CancelSet of
                [] ->
                    %% Empty cancellation set - nothing to cancel
                    {pass, NetState};
                _ ->
                    %% Apply cancellation to marking
                    #net_state{marking = Marking, usr_info = UsrInfo} = NetState,
                    UpdatedMarking = wf_cancel:apply_cancellation(Marking, CancelSet),

                    %% Update workflow state if case-level cancellation
                    UpdatedUsrInfo =
                        case Scope of
                            {case, all} -> mark_workflow_cancelled(UsrInfo, {case_cancel, Place});
                            _ -> UsrInfo
                        end,

                    %% Execute compensation hooks
                    execute_compensation_hooks(Scope, CancelSet, UpdatedUsrInfo),

                    %% Return updated net state with token passed
                    {pass, NetState#net_state{marking = UpdatedMarking,
                                             usr_info = UpdatedUsrInfo}}
            end
    end;

handle_cancel_token(_Place, {cancel, CancelSet}, NetState, _WrapperState)
  when is_list(CancelSet) ->
    %% Legacy token format - apply directly
    #net_state{marking = Marking} = NetState,
    UpdatedMarking = wf_cancel:apply_cancellation(Marking, CancelSet),
    {pass, NetState#net_state{marking = UpdatedMarking}};

handle_cancel_token(_Place, _Token, NetState, _WrapperState) ->
    %% Not a cancellation token - pass through
    pass.

%%--------------------------------------------------------------------
%% @doc Marks the workflow case as cancelled in usr_info.
%%
%% Updates the yawl_state in usr_info to set status to 'cancelled'
%% and record cancellation timestamp.
%%
%% ```erlang
%% > UsrInfo = #{yawl_state => YawlState},
%% > wf_cancel_runtime:mark_workflow_cancelled(UsrInfo, timeout).
%% #{yawl_state => #{status => cancelled, cancelled_at => T, ...}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec mark_workflow_cancelled(UsrInfo :: term(), Reason :: term()) -> term().

mark_workflow_cancelled(UsrInfo, Reason) ->
    case maps:get(yawl_state, UsrInfo, undefined) of
        undefined ->
            %% No yawl_state in usr_info - add it
            UsrInfo#{yawl_state => yawl_state:mark_cancelled(
                               yawl_state:new(<<"auto-generated">>))};
        YawlState ->
            %% Mark existing state as cancelled
            UpdatedState = yawl_state:mark_cancelled(YawlState),
            %% Add reason to metadata if possible
            UsrInfo#{yawl_state => UpdatedState}
    end.

%%--------------------------------------------------------------------
%% @doc Executes compensation hooks for a cancelled scope.
%%
%% Looks up registered compensation hooks in usr_info and executes
%% them in post-order (deepest scope first). Logs errors but continues
%% execution (log-and-continue strategy).
%%
%% ```erlang
%% > UsrInfo = #{compensation_hooks => #{{activity, task1} => fun cleanup/2}}.
%% > wf_cancel_runtime:execute_compensation_hooks({activity, task1}, [p1], UsrInfo).
%% ok
%% ```
%% @end
%%--------------------------------------------------------------------
-spec execute_compensation_hooks(Scope :: wf_cancel:cancel_scope(),
                                 CancelSet :: [atom()],
                                 UsrInfo :: term()) -> ok.

execute_compensation_hooks(Scope, CancelSet, UsrInfo) ->
    case maps:get(compensation_hooks, UsrInfo, undefined) of
        undefined ->
            ok;
        Hooks when is_map(Hooks) ->
            %% Find matching hooks for this scope
            MatchingHooks = maps:to_list(
                maps:filter(
                    fun(Key, _Fun) ->
                        %% Match exact scope or parent scopes
                        hook_matches_scope(Key, Scope)
                    end,
                    Hooks
                )
            ),

            %% Execute hooks in post-order (children before parents)
            lists:foreach(
                fun({HookKey, HookFun}) ->
                    try
                        HookFun(Scope, CancelSet)
                    catch
                        Type:Error:Stack ->
                            %% Log error with telemetry, continue execution
                            telemetry:log(
                                compensation_hook_failed,
                                #{hook => HookKey,
                                  scope => Scope,
                                  error_type => Type,
                                  error => Error,
                                  stacktrace => Stack}
                            )
                    end
                end,
                MatchingHooks
            ),

            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a hook key matches a cancellation scope.
%%
%% Hook matches if:
%% - Exact scope match
%% - Parent scope (activity matches if cancelling region containing it)
%% - Wildcard ({any, any})
%%--------------------------------------------------------------------
-spec hook_matches_scope(HookKey :: wf_cancel:cancel_scope() | {any, any},
                         Scope :: wf_cancel:cancel_scope()) -> boolean().

hook_matches_scope({any, any}, _Scope) ->
    true;
hook_matches_scope(Scope, Scope) ->
    true;
hook_matches_scope({activity, _TaskId}, {region, _RegionId}) ->
    %% Activity hook matches region cancellation (activity is part of region)
    true;
hook_matches_scope(_, _) ->
    false.
```

##### 3. Example Pattern Integration (cancel_activity.erl)
**File**: `/Users/sac/cre/src/patterns/cancel_activity.erl`

**Changes**:
1. Update `trigger/3` to delegate to runtime handler

```erlang
%%--------------------------------------------------------------------
%% @doc Trigger callback - intercept cancellation tokens.
%%
%% Delegates to wf_cancel_runtime:handle_cancel_token/4 for structured
%% cancellation processing.
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: term()) ->
    pass | drop.

trigger(Place, {cancel, _Scope} = Token, NetState) ->
    %% Delegate to runtime handler
    %% Note: Need access to wrapper state - will be passed via usr_info
    case maps:get(wrapper_state, NetState, undefined) of
        undefined ->
            %% No wrapper state - fall back to pass
            pass;
        WrapperState ->
            {Result, UpdatedNetState} =
                wf_cancel_runtime:handle_cancel_token(Place, Token, NetState, WrapperState),
            Result
    end;
trigger(_Place, _Token, _NetState) ->
    pass.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All existing tests pass
- [ ] `handle_cancel_token/4` correctly resolves scope tokens
- [ ] `mark_workflow_cancelled/2` updates yawl_state status
- [ ] `execute_compensation_hooks/3` executes hooks in post-order
- [ ] Trigger callback integration doesn't break existing patterns
- [ ] Performance test: cancellation of 10-place scope in < 1ms

##### Manual Verification:
- [ ] Create test workflow with activity cancellation - verify only target cancelled
- [ ] Create test workflow with region cancellation - verify all places in region cleared
- [ ] Create test workflow with case cancellation - verify entire workflow terminated
- [ ] Test compensation hook execution with telemetry logging
- [ ] Test backward compatibility with legacy `{cancel, [p1, p2]}` tokens
- [ ] Verify O(scope size) performance with large workflow (100+ places)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Compensation Hook Infrastructure

#### Overview
Implement compensation hook registration and execution framework, allowing workflows to define cleanup logic that executes when scopes are cancelled.

#### Changes Required:

##### 1. wf_compensation.erl (new module)
**File**: `/Users/sac/cre/src/wf/wf_compensation.erl` (NEW)

**Purpose**: Pure functional compensation hook management

```erlang
%% -*- erlang -*-
%%
%% @doc Compensation hook management for workflow cancellation.
%%
%% Provides pure functional utilities for registering, executing, and
%% managing compensation hooks that run when workflow scopes are cancelled.

-module(wf_compensation).
-moduledoc """
Compensation hook management for YAWL workflow cancellation.

Compensation hooks are functions that execute when a workflow scope
is cancelled, allowing for resource cleanup, state rollback, or other
compensation actions.

## Example

```erlang
%% Define a compensation hook
cleanup_payment_task({activity, payment_task}, CancelledPlaces) ->
    %% Refund payment, release resources, etc.
    payment_service:refund(get_payment_id()),
    ok.

%% Register hook in usr_info
UsrInfo1 = wf_compensation:register_hook(UsrInfo,
                                         {activity, payment_task},
                                         fun cleanup_payment_task/2),

%% Hook executes automatically when payment_task is cancelled
```

## Hook Execution Order
Hooks execute in post-order traversal (children before parents) to
ensure inner scopes are cleaned up before outer scopes.

## Error Handling
Hook failures are logged via telemetry but do not prevent other
hooks from executing (log-and-continue strategy).
""".

%%====================================================================
%% Exports
%%====================================================================

%% Hook registration
-export([register_hook/3, unregister_hook/2, get_hooks/1]).

%% Hook execution
-export([execute_hooks/3, execute_hook/2]).

%% Hook validation
-export([is_valid_hook/1, is_valid_scope/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compensation hook function type.
%%
%% Hooks take the cancelled scope and the list of cancelled places,
%% and return `ok` on success or raise an exception on failure.
%%--------------------------------------------------------------------
-type compensation_hook() :: fun((wf_cancel:cancel_scope(), [atom()]) -> ok).

%%--------------------------------------------------------------------
%% @doc Hook registry maps scopes to their compensation functions.
%%
%% Uses a map for O(log N) lookup and execution.
%%--------------------------------------------------------------------
-type hook_registry() :: #{wf_cancel:cancel_scope() => compensation_hook()}.

%%--------------------------------------------------------------------
%% @doc Hook execution result.
%%
%% Returns list of successful executions and failed executions with
%% their error reasons.
%%--------------------------------------------------------------------
-type execution_result() :: #{success := [wf_cancel:cancel_scope()],
                              failed := [{wf_cancel:cancel_scope(), term()}]}.

%% Export types
-export_type([compensation_hook/0, hook_registry/0, execution_result/0]).

%%====================================================================
%% Hook Registration Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Registers a compensation hook for a scope.
%%
%% Adds the hook to the compensation_hooks map in usr_info. If a hook
%% already exists for the scope, it is replaced.
%%
%% ```erlang
%% > Hook = fun({activity, task1}, Places) -> cleanup(Places) end,
%% > UsrInfo1 = wf_compensation:register_hook(UsrInfo, {activity, task1}, Hook),
%% > maps:get({activity, task1}, maps:get(compensation_hooks, UsrInfo1)).
%% #Fun<erl_eval.44.97283095>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec register_hook(UsrInfo :: term(),
                   Scope :: wf_cancel:cancel_scope(),
                   Hook :: compensation_hook()) -> term().

register_hook(UsrInfo, Scope, Hook) ->
    %% Validate inputs
    true = is_valid_scope(Scope),
    true = is_valid_hook(Hook),

    %% Get or create hook registry
    Hooks = maps:get(compensation_hooks, UsrInfo, #{}),

    %% Register hook for scope
    UpdatedHooks = maps:put(Scope, Hook, Hooks),

    %% Update usr_info
    UsrInfo#{compensation_hooks => UpdatedHooks}.

%%--------------------------------------------------------------------
%% @doc Unregisters a compensation hook for a scope.
%%
%% Removes the hook from the registry. Returns updated usr_info.
%%
%% ```erlang
%% > UsrInfo1 = wf_compensation:unregister_hook(UsrInfo, {activity, task1}),
%% > maps:get({activity, task1}, maps:get(compensation_hooks, UsrInfo1), undefined).
%% undefined
%% ```
%% @end
%%--------------------------------------------------------------------
-spec unregister_hook(UsrInfo :: term(), Scope :: wf_cancel:cancel_scope()) -> term().

unregister_hook(UsrInfo, Scope) ->
    case maps:get(compensation_hooks, UsrInfo, undefined) of
        undefined ->
            UsrInfo;
        Hooks ->
            UpdatedHooks = maps:remove(Scope, Hooks),
            UsrInfo#{compensation_hooks => UpdatedHooks}
    end.

%%--------------------------------------------------------------------
%% @doc Gets all registered compensation hooks from usr_info.
%%
%% Returns the hook registry map, or empty map if none registered.
%%
%% ```erlang
%% > wf_compensation:get_hooks(UsrInfo).
%% #{{activity, task1} => #Fun<...>, {region, R1} => #Fun<...>}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec get_hooks(UsrInfo :: term()) -> hook_registry().

get_hooks(UsrInfo) ->
    maps:get(compensation_hooks, UsrInfo, #{}).

%%====================================================================
%% Hook Execution Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes all compensation hooks for a cancelled scope.
%%
%% Executes hooks in post-order (deepest scope first). Returns
%% execution result with success/failure lists.
%%
%% ```erlang
%% > {ok, Result} = wf_compensation:execute_hooks({region, payment}, [p1, p2], UsrInfo),
%% > maps:get(success, Result).
%% [{activity, authorize}, {activity, capture}]
%% ```
%% @end
%%--------------------------------------------------------------------
-spec execute_hooks(Scope :: wf_cancel:cancel_scope(),
                   CancelledPlaces :: [atom()],
                   UsrInfo :: term()) -> {ok, execution_result()} | {error, term()}.

execute_hooks(Scope, CancelledPlaces, UsrInfo) ->
    Hooks = get_hooks(UsrInfo),

    %% Find matching hooks (exact or parent scopes)
    MatchingHooks = maps:to_list(
        maps:filter(
            fun(HookScope, _HookFun) ->
                hook_matches_scope(HookScope, Scope)
            end,
            Hooks
        )
    ),

    %% Sort by specificity (deepest first for post-order)
    SortedHooks = sort_hooks_postorder(MatchingHooks, Scope),

    %% Execute each hook, collecting results
    {Success, Failed} = lists:foldl(
        fun({HookScope, HookFun}, {Succ, Fail}) ->
            case execute_hook(HookFun, Scope, CancelledPlaces) of
                ok -> {[HookScope | Succ], Fail};
                {error, Reason} -> {Succ, [{HookScope, Reason} | Fail]}
            end
        end,
        {[], []},
        SortedHooks
    ),

    {ok, #{success => lists:reverse(Success),
           failed => lists:reverse(Failed)}}.

%%--------------------------------------------------------------------
%% @doc Executes a single compensation hook.
%%
%% Wraps hook execution in try/catch for error handling.
%%
%% ```erlang
%% > Hook = fun({activity, task1}, _) -> ok end,
%% > wf_compensation:execute_hook(Hook, {activity, task1}, [p1]).
%% ok
%% ```
%% @end
%%--------------------------------------------------------------------
-spec execute_hook(Hook :: compensation_hook(),
                  Scope :: wf_cancel:cancel_scope(),
                  CancelledPlaces :: [atom()]) -> ok | {error, term()}.

execute_hook(Hook, Scope, CancelledPlaces) ->
    try
        Hook(Scope, CancelledPlaces),
        ok
    catch
        Type:Error:Stack ->
            %% Log via telemetry but return error
            telemetry:log(
                compensation_hook_failed,
                #{hook => Hook,
                  scope => Scope,
                  cancelled_places => CancelledPlaces,
                  error_type => Type,
                  error => Error,
                  stacktrace => Stack}
            ),
            {error, {Type, Error}}
    end.

%%====================================================================
%% Validation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid compensation hook function.
%%
%% Valid hooks are functions of arity 2.
%%--------------------------------------------------------------------
-spec is_valid_hook(term()) -> boolean().

is_valid_hook(Hook) when is_function(Hook, 2) ->
    true;
is_valid_hook(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid cancellation scope.
%%
%% Valid scopes are {activity, atom}, {region, atom}, or {case, all}.
%%--------------------------------------------------------------------
-spec is_valid_scope(term()) -> boolean().

is_valid_scope({activity, TaskId}) when is_atom(TaskId) -> true;
is_valid_scope({region, RegionId}) when is_atom(RegionId) -> true;
is_valid_scope({case, all}) -> true;
is_valid_scope(_) -> false.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a hook scope matches a cancellation scope.
%%
%% Matches if:
%% - Exact match
%% - Parent scope (activity hook matches region cancellation)
%% - Wildcard
%%--------------------------------------------------------------------
-spec hook_matches_scope(HookScope :: wf_cancel:cancel_scope(),
                         CancelScope :: wf_cancel:cancel_scope()) -> boolean().

hook_matches_scope({any, any}, _CancelScope) ->
    true;
hook_matches_scope(Scope, Scope) ->
    true;
hook_matches_scope({activity, _TaskId}, {region, _RegionId}) ->
    true;
hook_matches_scope(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc Sorts hooks in post-order (children before parents).
%%
%% More specific scopes execute before less specific ones.
%%--------------------------------------------------------------------
-spec sort_hooks_postorder([{wf_cancel:cancel_scope(), compensation_hook()}],
                           TargetScope :: wf_cancel:cancel_scope()) ->
    [{wf_cancel:cancel_scope(), compensation_hook()}].

sort_hooks_postorder(Hooks, TargetScope) ->
    %% Define specificity order: activity > region > case
    Specificity = fun({activity, _}, _) -> 3;
                     (_, {activity, _}) -> 3;
                     ({region, _}, _) -> 2;
                     (_, {region, _}) -> 2;
                     ({case, all}, _) -> 1;
                     (_, {case, all}) -> 1;
                     (_, _) -> 0
                 end,

    lists:sort(
        fun({A, _}, {B, _}) ->
            Specificity(A, TargetScope) >= Specificity(B, TargetScope)
        end,
        Hooks
    ).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test hook registration and retrieval
%%--------------------------------------------------------------------
register_hook_test() ->
    UsrInfo = #{},
    Hook = fun(_, _) -> ok end,

    %% Register hook
    UsrInfo1 = register_hook(UsrInfo, {activity, task1}, Hook),

    %% Retrieve hook
    Hooks = get_hooks(UsrInfo1),
    ?assertEqual(Hook, maps:get({activity, task1}, Hooks)).

%%--------------------------------------------------------------------
%% @doc Test hook unregister
%%--------------------------------------------------------------------
unregister_hook_test() ->
    Hook = fun(_, _) -> ok end,
    UsrInfo0 = #{},
    UsrInfo1 = register_hook(UsrInfo0, {activity, task1}, Hook),

    %% Unregister
    UsrInfo2 = unregister_hook(UsrInfo1, {activity, task1}),

    %% Verify removed
    Hooks = get_hooks(UsrInfo2),
    ?assertEqual(undefined, maps:get({activity, task1}, Hooks, undefined)).

%%--------------------------------------------------------------------
%% @doc Test hook execution success
%%--------------------------------------------------------------------
execute_hook_success_test() ->
    Hook = fun({activity, task1}, [p1]) -> ok end,
    ?assertEqual(ok, execute_hook(Hook, {activity, task1}, [p1])).

%%--------------------------------------------------------------------
%% @doc Test hook execution failure
%%--------------------------------------------------------------------
execute_hook_failure_test() ->
    Hook = fun(_, _) -> error(bad) end,
    Result = execute_hook(Hook, {activity, task1}, [p1]),
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% @doc Test hooks execution with post-order
%%--------------------------------------------------------------------
execute_hooks_postorder_test() ->
    Executed = [],

    ActivityHook = fun({activity, task1}, _) ->
        put(executed, [activity | get(executed)]), ok
    end,
    RegionHook = fun({region, r1}, _) ->
        put(executed, [region | get(executed)]), ok
    end,

    UsrInfo0 = #{},
    UsrInfo1 = register_hook(UsrInfo0, {region, r1}, RegionHook),
    UsrInfo2 = register_hook(UsrInfo1, {activity, task1}, ActivityHook),

    %% Execute for region cancellation
    {ok, Result} = execute_hooks({region, r1}, [p1, p2], UsrInfo2),

    %% Activity hook should execute before region hook (post-order)
    ?assertEqual([activity, region], get(executed)),
    ?assertEqual(2, length(maps:get(success, Result))).

%%--------------------------------------------------------------------
%% @doc Test validation functions
%%--------------------------------------------------------------------
is_valid_hook_test() ->
    ?assertEqual(true, is_valid_hook(fun(_, _) -> ok end)),
    ?assertEqual(false, is_valid_hook(fun(_) -> ok end)),
    ?assertEqual(false, is_valid_hook(not_a_function)).

is_valid_scope_test() ->
    ?assertEqual(true, is_valid_scope({activity, task1})),
    ?assertEqual(true, is_valid_scope({region, r1})),
    ?assertEqual(true, is_valid_scope({case, all})),
    ?assertEqual(false, is_valid_scope({invalid, type})),
    ?assertEqual(false, is_valid_scope(not_a_tuple)).

-endif.
```

##### 2. Update gen_yawl initialization
**File**: `/Users/sac/cre/src/core/gen_yawl.erl`

**Changes**: Initialize empty compensation hooks map in usr_info if not present

```erlang
init({NetMod, NetArg, Options}) ->
    %% ... existing code ...

    %% Initialize user info from the callback module
    UsrInfo = NetMod:init(NetArg),

    %% Ensure compensation hooks map exists
    UsrInfoWithHooks = case maps:get(compensation_hooks, UsrInfo, undefined) of
        undefined -> UsrInfo#{compensation_hooks => #{}};
        _ -> UsrInfo
    end,

    %% ... continue with UsrInfoWithHooks instead of UsrInfo ...
```

#### Success Criteria:

##### Automated Verification:
- [ ] New module `wf_compensation` compiles without errors
- [ ] All EUnit tests pass: `eunit:test(wf_compensation, [verbose])`
- [ ] Hook registration and retrieval work correctly
- [ ] Hook execution with post-order traversal verified
- [ ] Error handling with telemetry logging works
- [ ] Integration with existing cancellation runtime passes tests

##### Manual Verification:
- [ ] Register compensation hook for activity - verify execution on cancellation
- [ ] Register compensation hook for region - verify execution on region cancellation
- [ ] Test hook failure handling - verify error logged but execution continues
- [ ] Test post-order execution with nested scopes - children execute before parents
- [ ] Verify hooks can be registered from workflow init/1 callback
- [ ] Test with real workflow scenario (e.g., payment cancellation with refund)

**Note**: This is the final phase. After completion, perform full integration testing.

---

## Testing Strategy

### Unit Tests

#### Phase 1 Tests (wf_cancel)
- Scope creation functions return correct token format
- `is_cancel_token/1` accepts legacy and scope tokens
- `resolve_scope/3` correctly maps:
  - Activity scope → task places
  - Region scope → all places in region
  - Case scope → all workflow places
- Edge cases: empty binding table, unknown task IDs, missing spec

#### Phase 2 Tests (yawl_cancel_runtime)
- `handle_cancel_token/4` processes all scope types
- `mark_workflow_cancelled/2` updates yawl_state correctly
- `execute_compensation_hooks/3` calls hooks in correct order
- Trigger callback integration doesn't break existing patterns
- Performance: < 1ms for 10-place cancellation

#### Phase 3 Tests (wf_compensation)
- Hook registration/unregistration/retrieval
- Hook execution success/failure paths
- Post-order traversal correctness
- Error handling and telemetry logging
- Edge cases: no hooks, hook failures, nested scopes

### Integration Tests

#### End-to-End Scenarios
1. **Activity cancellation**: Single task cancelled, unrelated tasks unaffected
2. **Region cancellation**: All tasks in region cancelled, outer tasks unaffected
3. **Case cancellation**: Entire workflow terminated, state marked cancelled
4. **Compensation execution**: Hooks fire in correct order on cancellation
5. **Backward compatibility**: Legacy tokens still work
6. **Performance**: Large workflow (100+ places) cancellation < 10ms

### Manual Testing Steps

1. **Create test workflow** with:
   - 3 activities in a region
   - 2 activities outside region
   - Compensation hooks for each activity
   - Region-level hook

2. **Test activity cancellation**:
   ```erlang
   %% Cancel activity inside region
   gen_yawl:inject(Pid, {cancel, {activity, task2}}),
   %% Verify: task2 places empty, other tasks unaffected
   %% Verify: task2 compensation hook executed
   ```

3. **Test region cancellation**:
   ```erlang
   %% Cancel entire region
   gen_yawl:inject(Pid, {cancel, {region, region1}}),
   %% Verify: all tasks in region cancelled
   %% Verify: compensation hooks executed in post-order
   ```

4. **Test case cancellation**:
   ```erlang
   %% Cancel entire workflow
   gen_yawl:inject(Pid, {cancel, {case, all}}),
   %% Verify: workflow state marked cancelled
   %% Verify: all compensation hooks executed
   ```

5. **Verify backward compatibility**:
   ```erlang
   %% Legacy token format
   gen_yawl:inject(Pid, {cancel, [p1, p2, p3]}),
   %% Verify: places cleared directly
   ```

6. **Performance test**:
   ```erlang
   %% Create workflow with 100 places
   %% Measure cancellation time
   T0 = erlang:monotonic_time(microsecond),
   gen_yawl:inject(Pid, {cancel, {region, large_region}}),
   T1 = erlang:monotonic_time(microsecond),
   ?assert(T1 - T0 < 10000).  % < 10ms
   ```

## Migration Notes

### For Existing Workflows

1. **No breaking changes**: Legacy `{cancel, [Place]}` tokens still work
2. **Opt-in**: Scope-based cancellation is additive, not required
3. **Compensation hooks**: Optional, no hooks = no cleanup
4. **State updates**: Automatic, no workflow changes needed

### For New Workflows

1. **Register compensation hooks** in `init/1`:
   ```erlang
   init(Args) ->
       UsrInfo = #{
           spec => maps:get(spec, Args),
           binding_table => maps:get(binding_table, Args)
       },
       %% Register cleanup hooks
       wf_compensation:register_hook(UsrInfo, {activity, payment},
           fun cleanup_payment/2)
   ```

2. **Use scope tokens in fire/3**:
   ```erlang
   fire(t_timeout, _Mode, UsrInfo) ->
       {produce, #{p_cancel => [{cancel, {region, payment_region}}}], UsrInfo}
   ```

3. **Handle cancellation in trigger/3**:
   ```erlang
   trigger(Place, {cancel, _Scope} = Token, NetState) ->
       %% Delegate to runtime (handles automatically)
       wf_cancel_runtime:handle_cancel_token(Place, Token, NetState, WrapperState)
   ```

## References

### Research
- `/Users/sac/cre/.wreckit/items/014-structured-cancellation-semantics/research.md`

### Key Files (from analysis)
- `/Users/sac/cre/src/wf/wf_cancel.erl` (lines 21-435): Token operations
- `/Users/sac/cre/src/wf/yawl_cancel_runtime.erl` (lines 275-427): Token processing
- `/Users/sac/cre/src/wf/yawl_state.erl` (lines 897-901): Cancellation status
- `/Users/sac/cre/src/wf/wf_spec.erl` (lines 511-536): Cancellation regions
- `/Users/sac/cre/src/wf/wf_scope.erl` (lines 128-146): Scope boundaries
- `/Users/sac/cre/src/core/gen_pnet.erl` (lines 161-167): Trigger callback
- `/Users/sac/cre/src/core/gen_yawl.erl` (lines 81-84): Enhanced fire/3
- `/Users/sac/cre/src/patterns/cancel_activity.erl` (line 74): Trigger implementation
- `/Users/sac/cre/docs/ARCHITECTURE.md` (lines 89-108): System architecture

### Design Decisions
- **Pure functional**: All logic in pure modules, only gen_yawl maintains state
- **Trigger-based**: Using existing `trigger/3` callback, not new gen_server messages
- **Log-and-continue**: Failed hooks logged but don't block cancellation
- **Backward compatible**: Legacy tokens work, scope types are additive
- **O(scope size)**: Achieved through trigger callback, not marking scanning
