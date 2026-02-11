# Research: Security and governance features

**Date**: 2025-01-14
**Item**: 025-security-and-governance-features

## Research Question
Marketplace-bound product needs governance controls to prevent abuse, ensure resource limits, and provide approval workflows for sensitive operations.

**Motivation:** Enables safe multi-tenancy, provides resource isolation, supports compliance requirements, essential for production deployment in shared environments.

**Success criteria:**
- Effect allowlist enforced per scope
- Effect budgets enforced
- Timeout policies terminate cases
- Approval points block until signal
- Guard failures produce structured errors

**Technical constraints:**
- Allowlist configuration per scope
- Budget tracking with receipts
- Approval point as special task pattern
- Structured error format

**Signals:** priority: medium, urgency: Required for marketplace readiness

## Summary

The CRE codebase has **significant governance infrastructure already in place** but requires integration and enhancement to meet marketplace requirements. The system has:

1. **Effect budget tracking** implemented in two modules (`ln_budget` and `ln_ctrl_budget`)
2. **Approval workflow system** implemented (`yawl_approval`)
3. **Guard/refusal system** for transition control (`yawl_refusal_guard`)
4. **Multi-tenant infrastructure** with schema-based isolation (`multi-tenant.schema.ts`)
5. **Effect system foundation** with receipts (`ln_effect`, `ln_receipt`)
6. **Timeout mechanisms** in workflow execution

**Key Finding:** These governance components are **disconnected** and operate independently. The primary gap is an **integrated governance layer** that:
- Enforces effect allowlists per scope
- Coordinates budget tracking across execution
- Integrates approval points into workflow control flow
- Provides unified error handling for guard failures

The codebase demonstrates enterprise-ready patterns from GCP Marketplace preparation (Item 005), including RBAC, network policies, and audit logging. However, runtime governance (effect-level controls) needs synthesis of existing components.

## Current State Analysis

### Existing Implementation

#### 1. Budget Tracking (Dual Implementation)

**Locations:**
- `/Users/sac/cre/src/ln_budget.erl:1-200` - Linear Nesting budget manager
- `/Users/sac/cre/src/ln_ctrl/ln_ctrl_budget.erl:1-191` - Control layer budget enforcement

**`ln_budget` module** (Lines 39-90):
```erlang
-record(config, {
    max_steps :: undefined | non_neg_integer(),
    max_effects :: undefined | non_neg_integer(),
    max_wall_ms :: undefined | pos_integer()
}).

-record(budget, {
    config :: #config{},
    steps = 0 :: non_neg_integer(),
    effects = 0 :: non_neg_integer(),
    start_time :: integer(),
    elapsed_ms = 0 :: non_neg_integer(),
    exceeded = false :: boolean()
}).
```

**Capabilities:**
- ✅ Step counting with configurable maximum
- ✅ Effect counting with configurable maximum
- ✅ Wall-clock time tracking (millisecond precision)
- ✅ Pure functional state management
- ✅ Budget status queries
- ✅ Error generation when exceeded

**`ln_ctrl_budget` module** (Lines 28-48):
```erlang
-record(budget, {
    max_effects :: budget_spec(),  % non_neg_integer() | unlimited
    max_latency_ms :: budget_spec(),
    max_cost_usd :: budget_spec(),
    effects_used :: non_neg_integer(),
    latency_used_ms :: non_neg_integer(),
    cost_used_usd :: float(),
    exceeded :: boolean()
}).
```

**Capabilities:**
- ✅ Effect count limits
- ✅ Latency tracking (milliseconds)
- ✅ Cost tracking (USD)
- ✅ Returns `{budget_exceeded, Reason, Budget}` on violation
- ✅ Integrates with "andon" red (triggers case halt)

**Assessment:** ✅ **Dual implementations** serve different purposes:
- `ln_budget` - Step/effect counting for executor hot loop
- `ln_ctrl_budget` - Cost/latency tracking for governance layer

**Gap:** No integration between the two budget systems. Need unified budget enforcement across execution layers.

#### 2. Approval Workflow System

**Location:** `/Users/sac/cre/src/yawl_approval.erl:1-200+`

**Approval checkpoint record** (Lines 142-146):
```erlang
-record(state, {
    checkpoints :: #{checkpoint_id() => #approval_checkpoint{}},
    decisions :: #{checkpoint_id() => #approval_decision{}},
    waiters :: #{checkpoint_id() => [pid()]},
    receipts :: #{checkpoint_id() => pnet_receipt:receipt()}
}).
```

**Approval types supported** (Lines 25-29):
- `human` - Manual human approval via API or CLI
- `simulated` - Claude Code headless mode for LLM approval
- `auto` - Auto-approve based on configurable rules

**API capabilities:**
- `create_checkpoint/3` - Create approval checkpoint
- `request_approval/1` - Request approval for checkpoint
- `approve/3`, `deny/3` - Record decision
- `wait_for_approval/1` - Block until decision or timeout
- `simulate_approval/2` - LLM-based approval simulation
- `list_pending/0` - List pending approvals
- `get_receipt/1` - Get approval receipt for audit trail

**Assessment:** ✅ **Complete approval workflow** with blocking semantics. Integrates with XES logging for audit trail.

**Gap:** Not integrated into workflow control flow. Approval points exist but don't block pattern execution automatically. Need to integrate as special task pattern.

#### 3. Guard/Refusal System

**Location:** `/Users/sac/cre/src/core/yawl_refusal_guard.erl:1-150+`

**Guard evaluation** (Lines 69-86):
```erlang
-record(compiled_guard, {
    guard_type :: expr | action | state | temporal,
    condition :: term(),
    metadata = #{} :: map()
}).

-record(guard_context, {
    mode :: map(),
    usr_info :: term(),
    transition :: atom(),
    marking :: map(),
    net_state :: #net_state{},
    timestamp :: integer()
}).
```

**Refusal categories** (Lines 92-101):
```erlang
-type refusal_category() ::
    missing_evidence |
    forbidden_action |
    scope_violation |
    external_boundary |
    resource_unavailable |
    safety_violation |
    validation_failure |
    timeout_exceeded |
    permission_denied.
```

**Guard language grammar** (Lines 10-20):
```
Guard        ::= ExprGuard | ActionGuard | StateGuard | TemporalGuard
ExprGuard    ::= OrExpr ('and' | 'or' | 'not')*
ActionGuard  ::= 'action' ActionName [('before'|'after_event') Guard]
StateGuard   ::= 'state' (StatusPred | MarkingPred | DataPred)
TemporalGuard ::= 'before' '(' Guard ')' | 'after_event' '(' Guard ')' |
                  'always' '(' Guard ')' | 'within' '(' Guard ',' Duration ')'
```

**Assessment:** ✅ **Sophisticated guard system** with temporal and state-based predicates. Provides "inadmissible-before" behavior for transitions.

**Gaps:**
1. No effect allowlist mechanism (guards operate on transitions, not effects)
2. No integration with effect system for effect-level authorization
3. Guard failures return structured errors but not integrated with effect error handling

#### 4. Effect System Foundation

**Locations:**
- `/Users/sac/cre/src/ln_effect.erl:1-177` - Effect boundary
- `/Users/sac/cre/src/ln_receipt.erl:1-285` - Receipt storage
- Item 016 research documents effect system architecture

**Effect spec** (Lines 60-75):
```erlang
-type effect_spec() :: #{
    module => module(),
    function => atom(),
    args => [term()],
    options => map()
}.

-record(pending_effect, {
    effect_id :: effect_id(),          % Uses make_ref() - NOT globally unique
    spec :: effect_spec(),
    scope_id :: scope_id(),
    callback_mod :: module(),
    continuation :: term(),
    status :: effect_status(),
    started_at :: integer()
}).
```

**Capabilities:**
- ✅ Effect request lifecycle
- ✅ Pending effect tracking
- ✅ Receipt generation on completion
- ✅ Effect cancellation by scope
- ✅ ETS-based receipt storage

**Gaps (from Item 016 research):**
1. ❌ No unique causal IDs (uses `make_ref()`)
2. ❌ No idempotency mechanism
3. ❌ No receipt persistence
4. ❌ No compensation/undo operations
5. ❌ No effect allowlist validation

**Integration point:** Effect system needs allowlist check before execution.

#### 5. Multi-Tenant Infrastructure

**Location:** `/Users/sac/cre/src/backend/database/schemas/multi-tenant.schema.ts:1-284`

**Tenant schema** (Lines 3-18):
```typescript
export interface Tenant {
  id: string;
  name: string;
  schema: string;
  createdAt: Date;
  updatedAt: Date;
  isActive: boolean;
  metadata?: Record<string, any>;
}

export interface TenantContext {
  tenantId: string;
  schema: string;
  userId?: string;
  permissions?: string[];
}
```

**Row-Level Security** (Lines 143-162):
```typescript
private async enableRowLevelSecurity(client: any, schemaName: string): Promise<void> {
    await client.query(`ALTER TABLE ${schemaName}.data ENABLE ROW LEVEL SECURITY;`);

    // Create RLS policy - users can only see data they created
    await client.query(`
      CREATE POLICY user_data_isolation ON ${schemaName}.data
      FOR SELECT
      USING (created_by = current_user_id());
    `);
}
```

**Capabilities:**
- ✅ Schema-based multi-tenancy
- ✅ Row-Level Security (RLS) for data isolation
- ✅ Tenant audit log
- ✅ Tenant context switching
- ✅ Active/inactive tenant management

**Assessment:** ✅ **Production-ready multi-tenancy** with PostgreSQL schema isolation and RLS.

**Gap:** No integration with effect allowlists. Per-tenant governance policies needed.

#### 6. Timeout Mechanisms

**Locations:**
- `/Users/sac/cre/src/yawl_timeout.erl` - YAWL timeout handling
- `/Users/sac/cre/src/wf/wf_timer.erl` - Workflow timer service
- `/Users/sac/cre/src/ln_ctrl/ln_ctrl_budget.erl:116-128` - Latency timeout check

**Latency timeout check** (Lines 116-128):
```erlang
-spec check_latency(Budget :: budget(), ElapsedMs :: non_neg_integer()) ->
    ok | {timeout, term(), budget()}.
check_latency(Budget, ElapsedMs) when is_integer(ElapsedMs), ElapsedMs >= 0 ->
    NewBudget = Budget#budget{latency_used_ms = ElapsedMs},

    case Budget#budget.max_latency_ms of
        unlimited ->
            ok;
        MaxLatency when ElapsedMs > MaxLatency ->
            return_exceeded(NewBudget, {max_latency_exceeded, ElapsedMs, MaxLatency});
        _ ->
            ok
    end.
```

**Assessment:** ✅ **Timeout mechanisms exist** but scattered across modules. Need unified timeout policy enforcement.

### Key Files

| File | Lines | Purpose | Governance Relevance |
|------|-------|---------|---------------------|
| `/Users/sac/cre/src/ln_budget.erl` | 1-200 | Executor budget tracking | Step/effect counting for hot loop |
| `/Users/sac/cre/src/ln_ctrl/ln_ctrl_budget.erl` | 1-191 | Governance budget enforcement | Cost/latency tracking, raises andon red |
| `/Users/sac/cre/src/yawl_approval.erl` | 1-200+ | Approval workflow | Human-in-the-loop checkpoints |
| `/Users/sac/cre/src/core/yawl_refusal_guard.erl` | 1-150+ | Transition guard evaluation | Refusal categories, guard language |
| `/Users/sac/cre/src/ln_effect.erl` | 1-177 | Effect boundary | Effect lifecycle, needs allowlist |
| `/Users/sac/cre/src/ln_receipt.erl` | 1-285 | Receipt storage | Audit trail for effects |
| `/Users/sac/cre/src/backend/database/schemas/multi-tenant.schema.ts` | 1-284 | Multi-tenant infrastructure | Per-tenant governance policies |
| `/Users/sac/cre/src/yawl_timeout.erl` | 1-100+ | Timeout handling | Case termination on timeout |
| `/Users/sac/cre/.wreckit/items/016-effect-system-with-receipts/research.md` | 1-933 | Effect system research | Unique IDs, idempotency, compensation |
| `/Users/sac/cre/.wreckit/items/005-harden-cre-security-and-compliance-for-enterprise-/research.md` | 1-549 | Security hardening | RBAC, network policies, audit logging |

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- `ln_effect` - Effect boundary (needs allowlist validation)
- `ln_budget` / `ln_ctrl_budget` - Budget tracking (needs unification)
- `yawl_approval` - Approval workflow (needs task pattern integration)
- `yawl_refusal_guard` - Guard evaluation (needs effect allowlist extension)
- `ln_vm` - Bytecode executor (needs governance checks)
- `multi_tenant_schema` - Tenant context (needs per-tenant policies)
- `ln_receipt` / `pnet_receipt` - Receipt storage (audit trail)

**External dependencies:**
- PostgreSQL (multi-tenant schemas, RLS policies)
- Redis (optional, for distributed governance state)
- ETS tables (in-memory governance state)
- Mnesia (persistent governance configuration)

### Patterns to Follow

**1. Pure functional state management:**
From `ln_budget.erl:83-90`:
```erlang
-record(budget, {
    config :: #config{},
    steps = 0 :: non_neg_integer(),
    effects = 0 :: non_neg_integer(),
    start_time :: integer(),
    elapsed_ms = 0 :: non_neg_integer(),
    exceeded = false :: boolean()
}).
```
- Keep state in records, not process state
- Return updated state from all operations
- No side effects in pure functions

**2. Receipt generation for audit trail:**
From `ln_receipt.erl:247-253`:
```erlang
add(#storage{table = Table, scope_index = ScopeIndex} = Storage,
    #receipt{effect_id = EffectId, scope_id = ScopeId} = Receipt) ->
    true = ets:insert(Table, {EffectId, Receipt}),
    true = ets:insert(ScopeIndex, {ScopeId, EffectId}),
    Storage.
```
- Use ETS for concurrent access
- Maintain secondary indexes for efficient queries
- Return updated storage handle

**3. Guard evaluation pattern:**
From `yawl_refusal_guard.erl:125-136`:
```erlang
check(Trsn, Mode, NetMod, UsrInfo) when is_atom(Trsn), is_map(Mode), is_atom(NetMod) ->
    try
        %% Build guard context
        Context = #guard_context{
            mode = Mode,
            usr_info = UsrInfo,
            transition = Trsn,
            marking = Marking,
            %% ...
        },
        evaluate(CompiledGuard, Context)
    catch
        _:_ -> {refused, validation_failure, <<"Guard evaluation failed">>}
    end.
```
- Build context from current state
- Evaluate guard in protected context
- Return structured refusal on failure

**4. Multi-tenant isolation pattern:**
From `multi-tenant.schema.ts:275-282`:
```erlang
async switchTenantSchema(client: any, tenantContext: TenantContext): Promise<void> {
    await client.query(`SET search_path TO ${tenantContext.schema}, public`);

    // Set context variables for RLS
    if (tenantContext.userId) {
        await client.query(`SELECT set_config('app.current_user_id', $1, false)`, [tenantContext.userId]);
    }
}
```
- Switch schema per request
- Set RLS context variables
- Maintain tenant context throughout execution

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **No effect allowlist enforcement** | Critical - Arbitrary code execution via effects | Implement effect allowlist check in `ln_effect:request/4` before execution; validate module/function against per-scope whitelist |
| **Budget systems disconnected** | High - Resource limits can be bypassed | Unify `ln_budget` and `ln_ctrl_budget`; create single `ln_governance_budget` module with step/effect/cost/latency tracking |
| **Approval points not blocking** | High - Sensitive operations execute without approval | Integrate `yawl_approval` checkpoints as special task pattern in bytecode compiler (`ln_compile`) |
| **No per-tenant governance policies** | High - All tenants share same limits | Extend multi-tenant metadata to include governance config (allowlists, budgets, timeouts); load policies on tenant context switch |
| **Guard failures not integrated with effects** | Medium - Effect authorization not enforced | Extend `yawl_refusal_guard` to support effect-level guards; add `permission_denied` refusal category for effect allowlist violations |
| **Timeout policies scattered** | Medium - Cases may not terminate on timeout | Create unified timeout policy service; integrate with budget checks in executor hot loop |
| **Receipts not persisted** | Medium - Audit trail lost on crash | Implement persistent receipt store (Mnesia/DETS) as documented in Item 016 research |
| **No structured error format for guard failures** | Low - Inconsistent error handling | Define `governance_error` type with categories (allowlist_violation, budget_exceeded, timeout, approval_denied); standardize error responses |

## Recommended Approach

### Phase 1: Effect Allowlist Enforcement (Foundation)

**Objective:** Prevent unauthorized effect execution.

1. **Define effect allowlist structure:**
   ```erlang
   -module(ln_governance_allowlist).

   -type effect_key() :: {module(), function(), arity()}.
   -type scope_id() :: binary() | atom().

   -record(allowlist_policy, {
       scope_id :: scope_id(),
       allowed_effects :: sets:set(effect_key()),
       denied_effects :: sets:set(effect_key()),
       default_allow :: boolean()
   }).

   -spec is_effect_allowed(effect_spec(), scope_id()) -> boolean().
   is_effect_allowed(#{module := Mod, function := Fun, args := Args}, ScopeId) ->
       Arity = length(Args),
       Policy = get_policy(ScopeId),
       EffectKey = {Mod, Fun, Arity},

       case sets:is_element(EffectKey, Policy#allowlist_policy.denied_effects) of
           true -> false;
           false ->
               case sets:is_element(EffectKey, Policy#allowlist_policy.allowed_effects) of
                   true -> true;
                   false -> Policy#allowlist_policy.default_allow
               end
       end.
   ```

2. **Integrate with effect request:**
   ```erlang
   %% In ln_effect.erl
   request(Spec, ScopeId, CallbackMod, Cont) ->
       case ln_governance_allowlist:is_effect_allowed(Spec, ScopeId) of
           false ->
               {error, {allowlist_violation, Spec, ScopeId}};
           true ->
               %% Proceed with effect request
               CausalId = ln_uuid:new(),
               %% ...
       end.
   ```

3. **Per-tenant allowlist configuration:**
   ```erlang
   %% Load allowlist from tenant metadata
   get_policy(ScopeId) ->
       case ln_multi_tenant:get_tenant_context(ScopeId) of
           {ok, #{governance := #{allowlist := AllowlistConfig}}} ->
               parse_allowlist_config(AllowlistConfig);
           {error, not_found} ->
               %% Default policy: deny all
               #allowlist_policy{
                   scope_id = ScopeId,
                   allowed_effects = sets:new(),
                   denied_effects = sets:new(),
                   default_allow = false
               }
       end.
   ```

**Estimated effort:** 3-4 days

### Phase 2: Unified Budget Tracking

**Objective:** Single source of truth for resource limits.

1. **Create unified budget module:**
   ```erlang
   -module(ln_governance_budget).

   -record(governance_budget, {
       scope_id :: scope_id(),
       max_steps :: non_neg_integer() | unlimited,
       max_effects :: non_neg_integer() | unlimited,
       max_wall_ms :: non_neg_integer() | unlimited,
       max_cost_usd :: float() | unlimited,
       steps_used = 0 :: non_neg_integer(),
       effects_used = 0 :: non_neg_integer(),
       elapsed_ms = 0 :: non_neg_integer(),
       cost_used_usd = 0.0 :: float(),
       exceeded = false :: boolean()
   }).

   -spec check_all(#governance_budget{}) ->
       ok | {error, budget_exceeded, term()}.
   check_all(Budget) ->
       Checks = [
           fun check_steps/1,
           fun check_effects/1,
           fun check_latency/1,
           fun check_cost/1
       ],
       lists:foldl(
           fun(Fun, ok) -> Fun(Budget);
              (_Fun, {error, _, _} = Error) -> Error
           end,
           ok, Checks).
   ```

2. **Integrate with executor hot loop:**
   ```erlang
   %% In ln_vm.erl or ln_ctrl case runner
   step(#vm_state{governance_budget = Budget} = State) ->
       case ln_governance_budget:check_all(Budget) of
           ok ->
               %% Proceed with execution
               execute_opcode(State);
           {error, budget_exceeded, Reason} ->
               %% Halt execution with structured error
               {halted, {budget_exceeded, Reason}, State}
       end.
   ```

**Estimated effort:** 2-3 days

### Phase 3: Approval Point Task Pattern

**Objective:** Block workflow execution at approval checkpoints.

1. **Create approval task pattern:**
   ```erlang
   -module(ln_pattern_approval).

   -export([approval/2]).

   %% Plan term for approval checkpoint
   approval(CheckpointId, Options) ->
       {approval, #{
           id => CheckpointId,
           timeout => maps:get(timeout, Options, 300000),  % 5 min default
           approvers => maps:get(approvers, Options, [human])
       }}.
   ```

2. **Compile approval to bytecode:**
   ```erlang
   %% In ln_compile.erl
   compile_plan({approval, #{id := Id, timeout := Timeout}}, Label, Joins, Scopes) ->
       WaitLabel = Label,
       ResumeLabel = Label + 1,

       %% Generate blocking wait
       Program = [
           {WaitLabel, {op_approval_wait, Id, Timeout, current_scope}},
           {ResumeLabel, {op_approval_resume, ResumeLabel + 1}}
       ],

       {Program, Joins, Scopes}.
   ```

3. **Execute approval wait:**
   ```erlang
   %% In ln_vm.erl
   execute_opcode({op_approval_wait, CheckpointId, Timeout, ScopeId}, State) ->
       case yawl_approval:request_approval(CheckpointId) of
           {ok, #approval_checkpoint{status = pending}} ->
               %% Block VM until approval
               NewState = State#vm_state{
                   status = waiting_approval,
                   waiting_for_approval = CheckpointId
               },
               {blocked, NewState};
           {error, Reason} ->
               {halted, {approval_failed, Reason}, State}
       end.
   ```

**Estimated effort:** 4-5 days

### Phase 4: Timeout Policy Integration

**Objective:** Unified timeout enforcement across all execution layers.

1. **Create timeout policy service:**
   ```erlang
   -module(ln_governance_timeout).

   -record(timeout_policy, {
       scope_id :: scope_id(),
       case_timeout_ms :: non_neg_integer() | unlimited,
       effect_timeout_ms :: non_neg_integer() | unlimited,
       approval_timeout_ms :: non_neg_integer()
   }).

   -spec check_case_timeout(integer(), #timeout_policy{}) ->
       ok | {error, timeout_exceeded, integer()}.
   check_case_timeout(ElapsedMs, #timeout_policy{case_timeout_ms = Max}) ->
       case Max of
           unlimited -> ok;
           MaxMs when ElapsedMs > MaxMs ->
               {error, timeout_exceeded, ElapsedMs};
           _ -> ok
       end.
   ```

2. **Integrate with budget checks:**
   ```erlang
   %% In ln_governance_budget
   check_all(Budget, TimeoutPolicy) ->
       Elapsed = erlang:monotonic_time(millisecond) - Budget#governance_budget.start_time,

       case ln_governance_timeout:check_case_timeout(Elapsed, TimeoutPolicy) of
           ok -> check_resource_limits(Budget);
           {error, timeout_exceeded, _} = Error -> Error
       end.
   ```

**Estimated effort:** 2-3 days

### Phase 5: Structured Error Format

**Objective:** Standardize governance error responses.

1. **Define governance error type:**
   ```erlang
   -module(ln_governance_error).

   -type error_category() ::
       allowlist_violation |
       budget_exceeded |
       timeout_exceeded |
       approval_denied |
       permission_denied |
       scope_violation.

   -type governance_error() :: #{
       category := error_category(),
       scope_id := scope_id(),
       reason := binary(),
       details => map(),
       timestamp := integer()
   }.

   -spec format_error(error_category(), scope_id(), term()) -> governance_error().
   format_error(Category, ScopeId, Reason) ->
       #{
           category => Category,
           scope_id => ScopeId,
           reason => format_reason(Reason),
           details => #{},
           timestamp => erlang:system_time(millisecond)
       }.
   ```

2. **Integrate with guard failures:**
   ```erlang
   %% In yawl_refusal_guard.erl
   check(Trsn, Mode, NetMod, UsrInfo) ->
       case evaluate_guard(...) of
           pass -> pass;
           {refused, Category, Reason} ->
               %% Convert to governance error
               Error = ln_governance_error:format_error(
                   permission_denied,
                   get_scope_id(UsrInfo),
                   {guard_refused, Category, Reason}
               ),
               {refused, Error}
       end.
   ```

**Estimated effort:** 1-2 days

### Phase 6: Per-Tenant Governance Configuration

**Objective:** Enable tenant-specific governance policies.

1. **Extend tenant metadata:**
   ```erlang
   %% In multi-tenant.schema.ts
   export interface GovernanceConfig {
       effectAllowlist?: {
           allowedEffects: Array<{module: string, function: string, arity: number}>,
           deniedEffects: Array<{module: string, function: string, arity: number}>,
           defaultAllow: boolean
       },
       budgetLimits?: {
           maxSteps?: number,
           maxEffects?: number,
           maxWallMs?: number,
           maxCostUsd?: number
       },
       timeoutPolicies?: {
           caseTimeoutMs?: number,
           effectTimeoutMs?: number,
           approvalTimeoutMs?: number
       }
   }

   export interface Tenant {
       // ... existing fields
       governance?: GovernanceConfig
   }
   ```

2. **Load governance policies on context switch:**
   ```erlang
   %% In ln_governance_supervisor
   init_tenant_context(TenantId) ->
       {ok, Tenant} = ln_multi_tenant:get_tenant(TenantId),

       Allowlist = parse_allowlist(Tenant),
       Budget = parse_budget_limits(Tenant),
       Timeouts = parse_timeouts(Tenant),

       %% Store in process dictionary or ETS
       put(governance_allowlist, Allowlist),
       put(governance_budget, Budget),
       put(governance_timeouts, Timeouts),

       ok.
   ```

**Estimated effort:** 3-4 days

## Open Questions

1. **Effect allowlist granularity:** Should allowlists be per-tenant, per-scope, or per-case?
   - **Recommendation:** Per-tenant defaults with per-case overrides for flexibility

2. **Budget enforcement point:** Should budgets be checked at step boundaries or effect boundaries?
   - **Recommendation:** Both - check steps in hot loop, effects in `ln_effect:request/4`

3. **Approval point storage:** Should approval checkpoints persist across case restarts?
   - **Recommendation:** Yes - store in Mnesia for durability; resume on restart

4. **Timeout policy hierarchy:** How do conflicting timeout policies resolve (case vs. effect vs. approval)?
   - **Recommendation:** Most specific wins - approval > effect > case

5. **Governance state distribution:** Should governance state be shared across nodes in distributed deployment?
   - **Recommendation:** Use Mnesia for distributed governance state; ETS for local cache

6. **Audit log integration:** Should governance events (allowlist violations, budget exceeded) be logged separately from workflow receipts?
   - **Recommendation:** Yes - create `governance_audit_log` table; integrate with XES logging

7. **Dynamic policy updates:** Can governance policies be updated without restarting cases?
   - **Recommendation:** Yes - reload policies on next boundary (step/effect); use versioned policies

8. **Structured error propagation:** How do governance errors propagate to workflow clients?
   - **Recommendation:** Return as structured error in workflow status; include error category and remediation

9. **Multi-tenant governance isolation:** Can tenants customize their governance policies, or are they system-wide?
   - **Recommendation:** Tenant-customizable within system-wide maximums (e.g., max steps ≤ 1M)

10. **Performance overhead:** What is the performance impact of governance checks on hot loop execution?
    - **Recommendation:** Benchmark with/without governance; target < 5% overhead; use ETS for fast lookups

11. **Compensation on budget exceeded:** Should we compensate executed effects when budget is exceeded?
    - **Recommendation:** Yes - use effect compensation from Item 016; rollback partial execution

12. **Approval delegation:** Can approvals be delegated to other users or roles?
    - **Recommendation:** Support role-based delegation in `yawl_approval`; integrate with tenant roles

13. **Effect allowlist validation:** How do we validate effect specs against allowlists efficiently?
    - **Recommendation:** Use ETS ordered_set for O(log N) lookups; cache compiled allowlists

14. **Budget reset on case restart:** Should budgets reset when cases are restarted from receipts?
    - **Recommendation:** Configurable - allow budget reset or carry-over based on use case

15. **Governance telemetry:** What metrics should be exposed for governance monitoring?
    - **Recommendation:** Track allowlist violations, budget exceeded events, approval wait times, timeout frequency

## Recommendations

1. **Immediate priorities (Week 1-2):**
   - Implement effect allowlist enforcement in `ln_effect:request/4`
   - Integrate approval checkpoints as task pattern in bytecode compiler
   - Define structured error format for governance failures

2. **Architecture decisions:**
   - Create `ln_governance` supervisor to manage allowlist, budget, timeout, and error modules
   - Use Mnesia for persistent governance configuration and audit logs
   - Integrate governance checks into executor hot loop (`ln_vm:step/1`)
   - Extend multi-tenant metadata to include governance policies

3. **Testing strategy:**
   - Unit tests for effect allowlist validation (allow, deny, default)
   - Integration tests for budget enforcement (steps, effects, latency, cost)
   - Property-based tests for timeout policies (bounded execution)
   - Cancellation tests (approval denial, budget exceeded)
   - Multi-tenant isolation tests (per-tenant policies)

4. **Documentation needs:**
   - Governance architecture document
   - Effect allowlist configuration guide
   - Budget policy reference
   - Approval checkpoint integration guide
   - Structured error handling guide

5. **Migration path:**
   - Phase 1-2: Core governance infrastructure (allowlist, budget)
   - Phase 3-4: Advanced features (approval, timeout)
   - Phase 5-6: Integration (errors, per-tenant config)
   - Each phase can be deployed independently

6. **Dependencies to coordinate with:**
   - Item 012 (Reducer/executor hot loop) - needs budget checks in step function
   - Item 014 (Cancellation semantics) - needs compensation on budget exceeded
   - Item 016 (Effect system) - needs allowlist validation before effect execution
   - Item 019 (Per-case state) - needs governance state in case record
   - Item 021 (Pattern implementations) - may use approval checkpoints

7. **Performance considerations:**
   - ETS allowlist lookups: O(log N) per effect
   - Budget checks: O(1) with record updates
   - Approval wait: blocks execution, no CPU overhead
   - Governance state size: ~1KB per case (allowlist + budget + timeouts)

8. **Security considerations:**
   - Validate governance policy updates (admin-only)
   - Audit log all governance events (allowlist violations, budget exceeded)
   - Prevent privilege escalation via effect allowlists
   - Encrypt sensitive governance policies at rest

9. **Observability:**
   - Metrics: governance_check_latency, allowlist_violation_total, budget_exceeded_total, approval_wait_duration_seconds
   - Trace events: governance_check, allowlist_decision, budget_update, approval_requested, timeout_triggered
   - Audit logs: Structured governance_error records with timestamps and scope IDs

10. **Future enhancements:**
    - Dynamic governance policy updates (runtime reload)
    - Governance policy versioning (rollback capability)
    - Machine learning for adaptive budget limits
    - Distributed governance coordination across nodes
    - Governance policy recommendation engine

---

**Document Version:** 1.0
**Last Updated:** 2025-01-14
**Status:** Research Complete
