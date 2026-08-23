# Research: Public API surface

**Date**: 2025-01-18
**Item**: 026-public-api-surface

## Research Question
Users need clean, well-documented API for creating and managing workflow cases without exposing internal complexity.

**Motivation:** Provides clear abstraction boundary, enables ergonomic usage, supports integration into applications, follows Erlang/OTP conventions.

**Success criteria:**
- All documented functions implemented
- Options cover key configuration
- API tested through examples

**Technical constraints:**
- Minimal surface - only essential functions
- Options: scheduler_policy, step_quanta, trace_level, effect_handler

**Signals:** priority: medium, urgency: Required for usability

## Summary

The CRE codebase has multiple layers of workflow execution APIs with varying levels of abstraction and documentation quality. The current public API surface is fragmented across several modules:

1. **Low-level CRE client/worker API** (`cre_client`, `cre_worker`) - Well-documented with comprehensive -moduledoc and -doc attributes, follows gen_server/gen_pnet patterns
2. **YAWL-specific client** (`cre_yawl_client`) - Has extensive documentation but appears to be a stub/skeleton implementation with placeholder logic
3. **Workflow engine API** (`wf_engine`) - Comprehensive doctests and examples, provides case management (start_case, case_state, worklist, allocate, complete, suspend_case, resume_case, cancel_case)
4. **Case supervisor API** (`wf_case_sup`) - Provides case lifecycle management (start_case, stop_case, list_cases, find_case, get_case_status)
5. **Choreography control API** (`ln_ctrl`) - Implements OTP behavior for reliable choreography with scheduler_policy, step_quanta, trace_level, and effect_handler options
6. **Configuration API** (`cre_config`) - Uses persistent_term for O(1) access to configuration values

The required options (scheduler_policy, step_quanta, trace_level, effect_handler) already exist in the `ln_ctrl` module but are not exposed through a unified public API for workflow case management.

## Current State Analysis

### Existing Implementation

#### Core CRE API (src/api/)
- **`cre_client.erl`** (552 lines): Complete gen_server implementation with comprehensive -moduledoc and -doc attributes. Exports: `start_link/3,4`, `eval/2`, `cre_reply/4`, `stop/1`
- **`cre_yawl_client.erl`** (1019 lines): Implements cre_client behavior for YAWL workflows. Exports: `start_link/2,3`, `execute_workflow/2`, `execute_pattern/3`, `compose_patterns/2`, `get_workflow_state/1`, `get_workflow_results/1`, `terminate_workflow/1`. Has extensive type specs and documentation but implementation appears to be stub code

#### Workflow Engine (src/wf/)
- **`wf_engine.erl`**: Core engine with case management APIs:
  - `start_case/3` - Start a new workflow case
  - `case_state/2` - Get case state (pending|running|suspended|cancelled|completed|failed)
  - `worklist/2` - Get work items for a user
  - `allocate/4`, `start_work/4`, `complete/5` - Work item lifecycle
  - `suspend_case/3`, `resume_case/3`, `cancel_case/3` - Case control
  - `drain_receipts/2`, `drain_events/1` - Event handling
  - `offered_workitems/2` - Query offered work items

- **`wf_case_runner.erl`** (200 lines): Gen_server wrapper for workflow execution
  - `start_link/3` - Start a case runner
  - `get_info/1` - Get case information
  - `cancel/1` - Cancel running case

- **`wf_case_sup.erl`**: Case supervisor API
  - `start_case/3`, `stop_case/1,2` - Case lifecycle
  - `list_cases/0`, `find_case/1` - Case discovery
  - `get_case_status/1`, `case_count/0` - Case queries

#### Choreography Control (src/ln_ctrl.erl)
- **`ln_ctrl.erl`** (OTP behavior): Implements the required options:
  - `scheduler` → scheduler_policy (ln_sched:mode())
  - `step_quanta` → pos_integer()
  - `trace` → trace_level (none | min | full)
  - `effect_handler` → module()

  This is an OTP behavior (similar to gen_server) that provides workflow orchestration with deterministic scheduling, effect tracking, and structured cancellation.

#### Configuration (src/cre_config.erl)
- Uses persistent_term for O(1) configuration access
- Exports: `init/0`, `get/1,2`, `set/2`, `get_all/0`
- Well-documented with doctests

#### Master Process (src/cre_master.erl)
- Central coordinator for worker pools and task distribution
- Uses Petri net marking algebra for tracking worker availability
- Exports: `start_link/0,1`, `add_worker/2`, `worker_result/4`, `cre_request/4`, `get_status/1`, `get_history/1`

### Key Files

#### API Modules
- `src/api/cre_client.erl:67` - Start anonymous client, eval expression, handle replies
- `src/api/cre_client.erl:119-142` - Documented start_link/3 with moduledoc
- `src/api/cre_client.erl:190-225` - Documented eval/2 with detailed workflow evaluation description
- `src/api/cre_yawl_client.erl:69` - YAWL client exports including execute_workflow, execute_pattern, compose_patterns
- `src/api/cre_yawl_client.erl:368-389` - start_link functions that wrap cre_client
- `src/api/cre_yawl_client.erl:403-416` - execute_workflow implementation

#### Workflow Engine
- `src/wf/wf_engine.erl:70-76` - Core case management exports
- `src/wf/wf_engine.erl:285-289` - start_case/3 implementation
- `src/wf/wf_engine.erl:292-300` - case_state/2 for querying case status
- `src/wf/wf_engine.hrl:14-22` - work_item record definition
- `src/wf/wf_engine.hrl:27-40` - wf_case record with status field

#### Case Management
- `src/wf/wf_case_sup.erl:13-15` - start_case, stop_case, list_cases, find_case, get_case_status exports
- `src/wf/wf_case_runner.erl:42-58` - start_link, get_info, cancel API

#### Choreography Control (WithOptions)
- `src/ln_ctrl.erl:54-61` - option() type definition with scheduler, step_quanta, trace, effect_handler
- `src/ln_ctrl.erl:85-94` - start_link/3,4 functions accepting options
- `src/ln_ctrl.erl:153-230` - init/1 that parses options (scheduler, trace, effect_handler, step_quanta)

#### Configuration
- `src/cre_config.erl:132-136` - Core exports (init, get, set, get_all)
- `src/cre_config.erl:295-330` - get/1,2 implementations using persistent_term

## Technical Considerations

### Dependencies
- **gen_pnet**: Petri net execution engine used by cre_worker
- **gen_server**: OTP behavior for client/master/case_runner
- **persistent_term**: OTP 21+ for O(1) configuration access
- **gproc**: Process registry used by wf_case_runner for case registration
- **ln_sched, ln_trace, ln_budget, ln_cancel**: Choreography control components

### Patterns to Follow

#### Documentation Pattern
The `cre_client` module demonstrates the target documentation style:
- Comprehensive `-moduledoc("""...""")` at module top
- Detailed `-doc("""...""")` for each exported function
- Examples in docstrings with ```erlang code blocks
- Type specifications with -spec
- Doctest functions for verification

#### Option Handling Pattern
From `ln_ctrl.erl:153-170`:
```erlang
init({CallbackMod, CaseArg, Options}) ->
    SchedulerMode = proplists:get_value(scheduler, Options, nondeterministic),
    TraceLevel = proplists:get_value(trace, Options, min),
    EffectHandler = proplists:get_value(effect_handler, Options, ln_effect),
    StepQuanta = proplists:get_value(step_quanta, Options, 1),
    ...
```

This pattern should be replicated in the public API with appropriate defaults.

#### API Naming Conventions
- `start_*` - Create/start a workflow or case
- `get_*` - Query state or information
- `suspend_*/resume_*` - Pause and restart execution
- `cancel_*` - Terminate execution
- `list_*` - Enumerate items
- `find_*` - Locate specific items

### Architectural Decisions

1. **Three-Tier Architecture**:
   - **Public API Layer** (to be created): Clean, documented interface for users
   - **Service Layer** (wf_engine, wf_case_sup, ln_ctrl): Core workflow orchestration
   - **Execution Layer** (gen_yawl, gen_pnet): Low-level execution engines

2. **Supervision Tree Integration** (item 020):
   - `wf_case_sup` provides OTP supervision tree integration
   - Cases run under supervisor for fault tolerance
   - Use `wf_case_runner` as gen_server wrapper for each case

3. **Separation of Concerns**:
   - `cre_client`: Generic CRE client/worker pattern
   - `cre_yawl_client`: YAWL-specific workflow client (currently stub)
   - `wf_engine`: Core workflow engine with Petri net semantics
   - `wf_case_sup`: Case lifecycle management and supervision
   - `ln_ctrl`: Choreography control with advanced options (scheduler, tracing, effects)

4. **State Management** (item 019):
   - Cases have state: pending | running | suspended | cancelled | completed | failed
   - Work items have state: offered | allocated | started | completed
   - State transitions tracked in wf_case records

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **API Fragmentation** - Multiple entry points (cre_client, cre_yawl_client, wf_engine, wf_case_sup) confuse users | High | Create unified public API module that delegates to appropriate internal modules based on use case |
| **Incomplete Implementation** - cre_yawl_client appears to be stub code with placeholder logic | Medium | Verify which functions have real implementations vs stubs; either complete or deprecate stubs |
| **Option Inconsistency** - Options exist in ln_ctrl but not exposed through workflow APIs | Medium | Add option parameter to wf_case_sup:start_case/3 and wf_engine:start_case/3 that passes through to ln_ctrl |
| **Documentation Drift** - Some modules have comprehensive docs, others have minimal | Medium | Apply cre_client documentation style as template across all public APIs |
| **Breaking Changes** - Adding options may require signature changes | Low | Use Options :: map() parameter with default values for backward compatibility |
| **Performance** - Persistent_term lookups are fast but initial configuration setup cost exists | Low | Document that cre_config:init() should be called during application start |

## Recommended Approach

### Phase 1: Design Unified Public API

Create a new module `src/api/cre_case.erl` (or similar) that provides:

1. **Case Lifecycle Functions**:
   ```erlang
   -export([start_case/3, start_case/4]).
   -export([get_case_info/1, list_cases/0, find_case/1]).
   -export([suspend_case/2, resume_case/2, cancel_case/2]).
   -export([get_case_status/1, get_case_results/1]).
   ```

2. **Work Item Management**:
   ```erlang
   -export([get_worklist/2, allocate_workitem/3]).
   -export([start_work/3, complete_work/4]).
   ```

3. **Options Support**:
   ```erlang
   -type option() :: {scheduler_policy, deterministic | nondeterministic} |
                    {step_quanta, pos_integer()} |
                    {trace_level, none | basic | full} |
                    {effect_handler, module()} |
                    {timeout, timeout()}.
   ```

### Phase 2: Implementation Strategy

1. **Wrapper Pattern**: Implement cre_case as a facade that delegates to:
   - `wf_case_sup` for case lifecycle
   - `wf_engine` for work item operations
   - `ln_ctrl` for advanced scheduling/execution options

2. **Option Pass-Through**:
   ```erlang
   start_case(SpecId, InitialData, Options) ->
       SchedulerPolicy = maps:get(scheduler_policy, Options, nondeterministic),
       StepQuanta = maps:get(step_quanta, Options, 1),
       TraceLevel = maps:get(trace_level, Options, basic),
       EffectHandler = maps:get(effect_handler, Options, undefined),
       % Convert to ln_ctrl options and delegate
       wf_case_sup:start_case(SpecId, InitialData, #{...}).
   ```

3. **Documentation Template**: Use cre_client.erl documentation style:
   - Module-level moduledoc with overview and examples
   - Function-level doc with Parameters, Returns, Example sections
   - Type specs for all exported functions
   - Doctests for verification

### Phase 3: Testing and Examples

1. **Example Workflows**: Extend `docs/examples/basic_workflow.erl` to demonstrate:
   - Starting a case with options
   - Querying case status
   - Managing work items
   - Suspending/resuming cases
   - Handling cancellations

2. **Doctests**: Add doctest_test/0 function following cre_client pattern

3. **Integration Tests**: Verify interaction with wf_case_sup, wf_engine, and ln_ctrl

### Phase 4: Deprecation and Migration

1. **Document Legacy APIs**: Mark cre_yawl_client stub functions for deprecation or completion
2. **Migration Guide**: Provide examples showing how to migrate from direct wf_engine usage to cre_case API
3. **Backward Compatibility**: Keep wf_engine exports for existing code, add cre_case as recommended interface

## Open Questions

1. **API Module Name**: Should the unified API be called `cre_case`, `cre_workflow`, `cre_public`, or something else? Consider consistency with `cre_client` naming.

2. **Option Validation**: Should options be validated at API entry point or passed through to underlying modules? What error semantics for invalid options?

3. **Case ID Type**: wf_case uses binary() case_id generated by wf_case_runner, wf_engine also generates IDs. Should the public API expose generated IDs or allow user-specified IDs?

4. **Synchronous vs Asynchronous**: Should the API provide both sync (blocking) and async (message-based) variants? cre_client:eval/2 is blocking, wf_engine functions are gen_server:call (sync).

5. **Error Handling**: What should be the error return pattern? wf_engine uses {error, Reason}, cre_client throws errors. Need consistent approach.

6. **Options Default Values**: What are appropriate defaults for:
   - scheduler_policy: deterministic or nondeterministic?
   - step_quanta: 1, 10, 100?
   - trace_level: none, basic, or full?
   - effect_handler: What default module?

7. **Integration with cre_yawl_client**: Should cre_yawl_client be completed as a YAWL-specific API, or should all YAWL workflows go through the generic cre_case API?

8. **Backward Compatibility**: Is maintaining compatibility with existing wf_engine and wf_case_sup APIs a requirement, or can they be marked internal?

9. **Case State Queries**: Should get_case_info return the full wf_case record or a sanitized map? Need to hide internal complexity (marking, rng_state, etc.).

10. **Work Item Allocation**: Does the public API need work item allocation functions, or is that too low-level? Consider user role: application developer vs workflow engine user.
