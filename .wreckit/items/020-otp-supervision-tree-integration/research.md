# Research: OTP supervision tree integration

**Date**: 2025-01-21
**Item**: 020-otp-supervision-tree-integration

## Research Question

Production Erlang systems require proper OTP supervision for resilience. Need supervision strategy appropriate for dynamic case creation/termination and effect worker management.

**Motivation:** Provides OTP-compliant fault tolerance, enables clean shutdown/restart, supports dynamic case lifecycle, integrates with Erlang/OTP ecosystem conventions.

**Technical constraints:**
- Use OTP behaviors (gen_server/gen_statem/supervisor)
- wf_case_sup uses simple_one_for_one or dynamic supervisor
- Per-case runners as gen_server or gen_statem
- Effect worker supervision optional

**Signals:** priority: medium, urgency: Required for production readiness

## Summary

The CRE system already has a well-established OTP supervision tree with several supervisors managing different concerns. The current architecture uses `gen_yawl` (a gen_server behavior wrapper around gen_pnet) for workflow case execution, and there are two existing workflow supervisors (`yawl_workflow_supervisor` and `yawl_supervisor`) that provide different approaches to dynamic workflow instance management.

**Key Finding:** The system already has partial supervision infrastructure but lacks a unified `wf_case_sup` that would provide consistent supervision for all workflow cases regardless of their execution mode (YAWL, wf_engine, or custom implementations). The existing `yawl_workflow_supervisor` uses `simple_one_for_one` strategy which is appropriate for dynamic child creation, but it's specific to gen_yawl processes.

**Recommended Approach:** Create a new `wf_case_sup` module that implements a hierarchical supervision tree where:
1. `wf_case_sup` sits under `cre_sup` as a permanent supervisor
2. Uses `simple_one_for_one` strategy for dynamic case creation
3. Each case runner is a gen_server/gen_statem that wraps the actual workflow execution (gen_yawl, wf_engine, or custom)
4. Effect workers can be optionally supervised under each case runner or under a separate supervisor

This approach maintains compatibility with existing code while providing production-grade fault tolerance and clean lifecycle management.

## Current State Analysis

### Existing Implementation

The CRE system has a mature supervision tree with the following structure:

**Top-level supervisor** (`/Users/sac/cre/src/app/cre_sup.erl:96-295`):
- Uses `one_for_one` strategy with intensity 0 (manual restart only)
- Manages 8 children including `cre_master`, `yawl_timeout`, `yawl_xes`, `yawl_approval`, `yawl_workflow_supervisor`, `yawl_worklist`, `yawl_registry`, and `license_sup`
- Child specs are properly defined with restart strategies (permanent for infrastructure, temporary for cre_master)

**Application entry point** (`/Users/sac/cre/src/app/cre.erl:40-450`):
- Standard OTP application behavior
- Initializes persistent_term configuration before starting supervision tree
- Starts HTTP web service for status/health endpoints
- Properly implemented `start/2` and `stop/1` callbacks

**Workflow supervisors**:

1. **yawl_workflow_supervisor** (`/Users/sac/cre/src/app/yawl_workflow_supervisor.erl:1-99`):
   - Uses `simple_one_for_one` strategy (intensity 10, period 60)
   - Manages gen_yawl workflow instances
   - Provides `start_workflow/3` and `stop_workflow/1` API
   - Child spec uses `temporary` restart (completed workflows not restarted)
   - Simple implementation focused on gen_yawl processes only

2. **yawl_supervisor** (`/Users/sac/cre/src/yawl/yawl_supervisor.erl:1-588`):
   - More feature-rich supervisor with gproc-based process registry
   - Supports multiple restart strategies (one_for_one, one_for_all, rest_for_one)
   - Provides comprehensive API: `start_workflow/3`, `stop_workflow/2`, `terminate_workflow/2`, `list_workflows/0`, `pause_workflow/1`, `resume_workflow/1`
   - Tracks workflow metadata (started_at, restart_count, status)
   - Uses workflow IDs (binary or atom) as child identifiers
   - More complex but also more complete supervision solution

3. **active_token_sup** (`/Users/sac/cre/src/active/active_token_sup.erl:1-93`):
   - Simple `one_for_one` supervisor for active token processes
   - Uses `transient` restart strategy
   - Provides `start_token/4` and `terminate_token/1` API
   - Good pattern to follow for dynamic child supervision

4. **license_sup** (`/Users/sac/cre/src/license/license_sup.erl:1-49`):
   - Simple `one_for_one` supervisor with one child
   - Standard pattern for supervising a small set of permanent workers

### Workflow Execution Models

**gen_yawl** (`/Users/sac/cre/src/core/gen_yawl.erl:1-1556`):
- Implements gen_server behavior
- Wraps gen_pnet with enhanced fire/3 callback supporting 3-tuple returns
- Each workflow instance is a gen_server process
- Supports timeout handling, cycle detection, checkpointing
- Uses `continue` cast message for autonomous progression
- Properly implements `terminate/2` with state persistence and telemetry

**wf_engine** (`/Users/sac/cre/src/wf/wf_engine.erl:1-300+`):
- Implements gen_server behavior for workflow case management
- Manages multiple cases within a single engine process
- Uses `#wf_case{}` records to track case state
- Not directly supervised as individual cases (cases are data within the engine)
- Provides `start_case/3`, `case_state/2`, `worklist/2`, `allocate/4`, `complete/5` APIs

**wf_yawl_executor** (`/Users/sac/cre/src/wf/wf_yawl_executor.erl:1-250+`):
- High-level executor that manages workflow lifecycle
- Creates gen_yawl processes via `gen_yawl:start_link/4` (line 615)
- Provides load/compile/start/stop workflow API
- Doesn't directly supervise - creates gen_yawl processes that should be supervised

### Key Files

#### Supervision Infrastructure
- `/Users/sac/cre/src/app/cre_sup.erl:96-295` - Top-level supervisor with 8 children, one_for_one strategy
- `/Users/sac/cre/src/app/cre.erl:183-231` - Application callbacks, starts cre_sup
- `/Users/sac/cre/src/app/yawl_workflow_supervisor.erl:1-99` - Simple gen_yawl workflow supervisor (simple_one_for_one)
- `/Users/sac/cre/src/yawl/yawl_supervisor.erl:1-588` - Feature-rich workflow supervisor with registry and metadata

#### Workflow Execution
- `/Users/sac/cre/src/core/gen_yawl.erl:1-1556` - gen_server wrapper around gen_pnet, core workflow execution engine
- `/Users/sac/cre/src/wf/wf_engine.erl:1-300+` - gen_server managing multiple workflow cases
- `/Users/sac/cre/src/wf/wf_yawl_executor.erl:1-250+` - High-level YAWL workflow executor
- `/Users/sac/cre/src/wf/wf_engine.hrl:1-43` - Workflow case and work item record definitions

#### Worker Pools
- `/Users/sac/cre/src/wf/wf_pool.erl:1-200+` - Poolboy wrapper for bounded concurrency
- `/Users/sac/cre/src/wf/wf_pool_worker.erl:1-140` - Placeholder worker implementation
- `/Users/sac/cre/src/cre_worker.erl` - Worker implementation (not reviewed in detail)

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- `gen_yawl` - Core workflow execution behavior (gen_server)
- `wf_engine` - Alternative workflow engine managing multiple cases
- `wf_yawl_executor` - High-level workflow executor
- `yawl_workflow_supervisor` - Existing gen_yawl supervisor
- `yawl_supervisor` - Existing feature-rich workflow supervisor
- `cre_sup` - Top-level supervisor (will add wf_case_sup as child)
- `gproc` - Process registry (used by yawl_supervisor for workflow lookup)

**External dependencies:**
- `gen_pnet` - Petri net execution engine (already used by gen_yawl)
- `poolboy` - Worker pool management (used by wf_pool)
- OTP 25+ - Minimum OTP version (supports modern supervisor child specs)

### Patterns to Follow

**1. Simple one_for_one for dynamic children:**
From `yawl_workflow_supervisor.erl:84-98`:
```erlang
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => workflow_instance,
        start => {gen_yawl, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [gen_yawl]
    },
    {ok, {SupFlags, [ChildSpec]}}.
```

**2. API wrapper for supervisor operations:**
From `yawl_workflow_supervisor.erl:57-62` and `:66-78`:
```erlang
start_workflow(NetMod, NetArg, Options) ->
    supervisor:start_child(?MODULE, [NetMod, NetArg, Options]).

stop_workflow(Pid) ->
    case supervisor:terminate_child(?MODULE, Pid) of
        ok -> ok;
        {error, not_found} ->
            %% Fallback to direct stop
            try gen_yawl:stop(Pid), ok catch _:_ -> {error, not_found} end
    end.
```

**3. Process registry for workflow lookup:**
From `yawl_supervisor.erl:442-446` and `:453-456`:
```erlang
register_workflow(WorkflowId, Pid) ->
    gproc:reg_local_name({workflow, WorkflowId}, Pid),
    gproc:reg_local_prop({workflow, WorkflowId}, started_at, erlang:system_time(millisecond)),
    true.

unregister_workflow(WorkflowId) ->
    gproc:unreg_local_name({workflow, WorkflowId}),
    gproc:unreg_local_prop({workflow, WorkflowId}, started_at),
    true.
```

**4. Child spec integration in cre_sup:**
From `cre_sup.erl:259-266`:
```erlang
WorkflowSupSpec = #{
    id => yawl_workflow_supervisor,
    start => {yawl_workflow_supervisor, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [yawl_workflow_supervisor]
},
```

**5. gen_server behavior for case runners:**
- Use `gen_server` or `gen_statem` for per-case runner processes
- Implement proper `init/1`, `handle_call/3`, `handle_cast/2`, `terminate/2`
- Use temporary restart for completed cases, permanent for infrastructure
- Support graceful shutdown with cleanup

### Architecture Decision Points

**1. Supervisor Strategy:**
- **simple_one_for_one** (recommended): Best for dynamic case creation, all children use same child spec template
- **one_for_one**: Requires unique child IDs for each case, more boilerplate
- **Dynamic supervisor** (OTP 26+): Modern alternative, but CRE targets OTP 25+

**2. Case Runner Implementation:**
- **Option A**: Wrapper gen_server that delegates to gen_yawl/wf_engine
  - Pros: Clean separation, can add metadata/tracking
  - Cons: Extra process overhead
- **Option B**: Direct gen_yawl supervision (current yawl_workflow_supervisor approach)
  - Pros: Simpler, less overhead
  - Cons: Limited to gen_yawl workflows
- **Option C**: Hybrid - case runners can be gen_yawl OR custom gen_server
  - Pros: Maximum flexibility
  - Cons: More complex child spec management

**3. Effect Worker Supervision:**
- **Option A**: Workers supervised under each case runner
  - Pros: Clean shutdown (stop case → stop workers)
  - Cons: Deep supervision tree, many supervisors
- **Option B**: Single global effect worker supervisor
  - Pros: Flatter tree, simpler
  - Cons: Workers need to be linked to case for cleanup
- **Option C**: No supervision (current approach)
  - Pros: Simplest
  - Cons: No fault tolerance for workers (violates OTP principles)

**4. Process Registry:**
- **gproc** (used by yawl_supervisor): Feature-rich, supports metadata
- **pg** (OTP 23+): Built-in process groups, simpler API
- **no registry**: Track state in supervisor only (simpler but less queryable)

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking existing workflow creation APIs | High | Keep existing APIs working, add new supervised APIs as opt-in |
| Deep supervision tree affecting performance | Medium | Use simple_one_for_one to minimize depth, monitor supervisor tree depth |
| Case termination leaving orphaned processes | High | Implement proper links/monitors in terminate/2, use shutdown hierarchy |
| Restart strategy causing unwanted restarts | Medium | Use `temporary` restart for completed cases, `permanent` for infrastructure only |
| Effect worker supervision complexity | Medium | Make effect worker supervision optional, document best practices |
| Testing complexity with supervision | Low | Use existing test patterns, supervisor:* which_children/1 for inspection |
| Hot code upgrade challenges | Medium | Follow OTP code_change/3 patterns, test upgrade/downgrade paths |
| Memory leaks from case metadata | Low | Implement case cleanup after completion, use gproc counters for monitoring |

## Recommended Approach

Based on the research, I recommend the following implementation plan:

### Phase 1: Create wf_case_sup Module

**File**: `/Users/sac/cre/src/wf/wf_case_sup.erl`

1. **Implement supervisor behavior**:
   - Use `simple_one_for_one` strategy (intensity 10, period 60)
   - Export `start_link/0` and `start_link/1` for configuration
   - Export `start_case/3`, `stop_case/1`, `stop_case/2`, `list_cases/0`, `find_case/1`

2. **Case runner process** (`wf_case_runner`):
   - Implements `gen_server` behavior
   - Wraps gen_yawl, wf_engine, or custom workflow implementations
   - Tracks case metadata (case_id, spec_id, started_at, status)
   - Properly implements `terminate/2` for cleanup
   - Uses `temporary` restart strategy (completed cases don't restart)

3. **Child spec template**:
   ```erlang
   ChildSpec = #{
       id => case_instance,
       start => {wf_case_runner, start_link, []},
       restart => temporary,
       shutdown => 5000,
       type => worker,
       modules => [wf_case_runner]
   }
   ```

4. **API design**:
   ```erlang
   % Start a case under supervision
   start_case(SpecId, InitialData, Options) ->
       supervisor:start_child(?MODULE, [SpecId, InitialData, Options]).

   % Stop a case gracefully
   stop_case(CaseId) ->
       stop_case(CaseId, 5000).

   stop_case(CaseId, Timeout) ->
       case find_case(CaseId) of
           {ok, Pid} -> supervisor:terminate_child(?MODULE, Pid);
           Error -> Error
       end.

   % List all active cases
   list_cases() ->
       [{Id, Pid, Modules} || {Id, Pid, _Type, Modules} <- supervisor:which_children(?MODULE)].

   % Find a case by ID
   find_case(CaseId) ->
       case lists:keyfind(CaseId, 1, list_cases()) of
           {CaseId, Pid, _Modules} -> {ok, Pid};
           false -> {error, not_found}
       end.
   ```

### Phase 2: Integrate into cre_sup

**Modify**: `/Users/sac/cre/src/app/cre_sup.erl`

Add `wf_case_sup` as a child after `yawl_workflow_supervisor`:

```erlang
CaseSupSpec = #{
    id => wf_case_sup,
    start => {wf_case_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [wf_case_sup]
},
```

Update child specs list to include CaseSupSpec (line 295).

### Phase 3: Implement wf_case_runner

**File**: `/Users/sac/cre/src/wf/wf_case_runner.erl`

1. **State record**:
   ```erlang
   -record(case_state, {
       case_id :: binary(),
       spec_id :: binary(),
       spec :: map(),  % Workflow spec
       workflow_pid :: pid() | undefined,  % gen_yawl or wf_engine pid
       status :: pending | running | completed | cancelled | failed,
       started_at :: integer(),
       completed_at :: integer() | undefined,
       data :: map(),
       options :: map()
   }).
   ```

2. **gen_server callbacks**:
   - `init/1`: Initialize case state, start underlying workflow
   - `handle_call/3`: Support queries (get_status, get_data)
   - `handle_cast/2`: Support commands (cancel, inject_token)
   - `handle_info/2`: Handle workflow termination messages
   - `terminate/2`: Cleanup workflow, persist state, emit telemetry

3. **Workflow execution**:
   ```erlang
   init([SpecId, InitialData, Options]) ->
       CaseId = generate_case_id(),
       State = #case_state{
           case_id = CaseId,
           spec_id = SpecId,
           status = pending,
           started_at = erlang:system_time(millisecond),
           data = InitialData,
           options = Options
       },
       % Start workflow based on type
       case maps:get(type, Options, gen_yawl) of
           gen_yawl ->
               {ok, WfPid} = start_gen_yawl(SpecId, CaseId, InitialData, Options),
               {ok, State#case_state{workflow_pid = WfPid, status = running}};
           wf_engine ->
               {ok, WfPid} = start_wf_engine(SpecId, CaseId, InitialData, Options),
               {ok, State#case_state{workflow_pid = WfPid, status = running}}
       end.
   ```

### Phase 4: Optional Effect Worker Supervisor

**File**: `/Users/sac/cre/src/wf/wf_effect_worker_sup.erl`

1. **Two-tier approach**:
   - Global `wf_effect_worker_sup` (under cre_sup)
   - Per-case worker pools linked to case runner

2. **Or simpler approach**:
   - Use existing `wf_pool` (poolboy) for effect workers
   - Case runner creates temporary pools for effect execution
   - Pools are automatically terminated when case terminates

### Phase 5: Migration Path

1. **Backward compatibility**:
   - Keep existing `yawl_workflow_supervisor` and `yawl_supervisor` unchanged
   - Add `wf_case_sup` as new, recommended approach
   - Update documentation to recommend `wf_case_sup` for new code

2. **Testing**:
   - Unit tests for supervisor behavior
   - Integration tests for case lifecycle
   - Fault injection tests (kill case runner, verify restart/no-restart)
   - Performance tests (measure overhead of supervision)

3. **Monitoring**:
   - Add telemetry events for case lifecycle
   - Expose metrics: active_cases, completed_cases, failed_cases
   - Integrate with existing `yawl_telemetry` module

## Open Questions

1. **Case ID format**: Should we use binary (UUID) or integer IDs? Binary is more flexible, integer is more compact.
   - **Recommendation**: Use binary UUIDs for uniqueness across distributed nodes

2. **Case data persistence**: Should wf_case_sup persist case state to disk?
   - **Recommendation**: Defer to existing `yawl_persistence` and `yawl_checkpoint` modules, don't duplicate

3. **Effect worker model**: Should effect workers be supervised or just monitored?
   - **Recommendation**: Start with monitoring (links), add supervision if fault tolerance is needed

4. **Process registry**: Use gproc or pg or custom ets table?
   - **Recommendation**: Use gproc (already a dependency, feature-rich with metadata support)

5. **Backward compatibility**: Should we migrate existing code to use wf_case_sup?
   - **Recommendation**: No, keep existing supervisors, use wf_case_sup for new code only

6. **Hot code upgrade**: How to upgrade wf_case_sup without dropping active cases?
   - **Recommendation**: Follow OTP appup pattern, use supervisor:terminate_child/2 before upgrade, restart after

7. **Distributed case execution**: Should wf_case_sup support cases across nodes?
   - **Recommendation**: Start with single-node, add distributed support later using pg:join/2

8. **Case timeout**: Should cases have a maximum execution time?
   - **Recommendation**: Add optional timeout parameter, default to infinity
