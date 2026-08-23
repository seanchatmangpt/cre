# From Zero to Manufacturing: ln_ctrl with the "`.`" Operator

**Learn manufacturing-driven development: ontology → Erlang/OTP generation**

---

## What You'll Learn

This guide teaches you to manufacture Erlang/OTP applications from RDF specifications using the **"`.`" operator** - a deterministic generator that turns industry ontology into executable code.

**Manufacturing Model:**
- Input: RDF ontology (what workflow does)
- Pipeline: SPARQL extraction → Tera templates → Erlang/OTP modules
- Output: Compilable, testable, versioned code
- Evidence: Deterministic receipts (same inputs → identical outputs)

**Replacement Thesis:**
Integration engineers maintaining custom workflow code are replaced by ontology authors. Manufacturing speed: ontology change → deployed code in <30 seconds.

---

## Prerequisites

- **Erlang/OTP 28+** installed
- **rebar3** for Erlang builds
- **ggen** (optional for full pipeline): `cargo install ggen-cli`
- Basic understanding of: RDF/Turtle, SPARQL, Erlang/OTP

---

## Part 1: Understanding ln_ctrl and the "`.`" Operator

### What is ln_ctrl?

`ln_ctrl` is an Erlang/OTP behavior for running workflows ("lines") with:
- **Stop-the-line**: Cancel scopes halt effects deterministically
- **Budgets**: Effects, latency, cost limits
- **Receipts**: Tamper-evident audit trail
- **Determinism**: Replay from logged choices
- **Supervision**: OTP-standard fault tolerance

### What is the "`.`" Operator?

The "`.`" operator is your manufacturing command. It's a wrapper around `ggen` that:
1. Validates ontology and templates
2. Runs generation pipeline
3. Issues build receipts
4. Signals andon status (🟢/🟡/🔴)

**Why "`.`"?**
- Dot = "run the generator at project root"
- Mnemonic: "generate from here"
- Beginner-friendly (hides ggen complexity)

**Commands:**
```bash
./bin/dot validate   # Preflight: check ontology, queries, templates
./bin/dot sync       # Generate: ontology → Erlang/OTP modules
./bin/dot receipt    # Evidence: show last build receipt
./bin/dot help       # Reference: command list
```

---

## Part 2: RDF Ontology Basics

### The ln Ontology

`ln_ctrl` uses a minimal ontology with 5 primitives:

```turtle
ln:Line          # Workflow definition (name, plan root)
ln:Task          # Work unit (id, description, optional effect)
ln:Scope         # Cancellation boundary (id, description)
ln:Plan          # Composition (kind: seq/par/xor/scope/task)
ln:Effect        # External system call (type, endpoint)
```

### Example: Order Fulfillment Line

```turtle
@prefix ln: <http://linecontroller.factory/ontology/ln#> .
@prefix order: <http://linecontroller.factory/examples/order#> .

# Line definition
order:OrderFulfillmentLine a ln:Line ;
    ln:lineName "order_fulfillment_line" ;
    ln:planRoot order:RootPlan .

# Tasks
order:ValidateOrderTask a ln:Task ;
    ln:taskId "validate_order" ;
    ln:taskDescription "Validate order data" ;
    ln:hasEffect order:ValidateOrderEffect .

order:ProcessPaymentTask a ln:Task ;
    ln:taskId "process_payment" ;
    ln:taskDescription "Process payment" ;
    ln:hasEffect order:ProcessPaymentEffect .

# Effects
order:ValidateOrderEffect a ln:Effect ;
    ln:effectType "http_post" ;
    ln:effectEndpoint "/api/orders/validate" .

# Scopes
order:PaymentScope a ln:Scope ;
    ln:scopeId "payment_scope" ;
    ln:scopeDescription "Payment with rollback" .

# Plan (workflow structure)
order:RootPlan a ln:Plan ;
    ln:planKind "seq" ;
    ln:planChildren (
        order:ValidatePlan
        order:PaymentPlan
        order:NotifyPlan
    ) .

order:ValidatePlan a ln:Plan ;
    ln:planKind "task" ;
    ln:refTask order:ValidateOrderTask .

order:PaymentPlan a ln:Plan ;
    ln:planKind "scope" ;
    ln:refScope order:PaymentScope ;
    ln:scopeBody order:PaymentBody .
```

**Key Points:**
- Lines are graphs, not linear sequences
- Tasks describe what (not how) - effects are external
- Scopes enable stop-the-line cancellation
- Plans compose with seq/par/xor/scope

---

## Part 3: SPARQL Extraction

SPARQL queries extract entities from ontology for template rendering.

### Extract Tasks

`sparql/tasks.sparql`:
```sparql
PREFIX ln: <http://linecontroller.factory/ontology/ln#>
SELECT ?taskId ?taskDescription ?effectType ?effectEndpoint
WHERE {
    ?task a ln:Task .
    ?task ln:taskId ?taskId .
    ?task ln:taskDescription ?taskDescription .
    OPTIONAL {
        ?task ln:hasEffect ?effect .
        ?effect ln:effectType ?effectType .
        ?effect ln:effectEndpoint ?effectEndpoint .
    }
}
ORDER BY ?taskId
```

**Result:**
```
taskId              | taskDescription      | effectType | effectEndpoint
--------------------|----------------------|------------|-----------------------
validate_order      | Validate order data  | http_post  | /api/orders/validate
process_payment     | Process payment      | http_post  | /api/payments/process
```

### Extract Plan Structure

`sparql/plan_nodes.sparql`:
```sparql
PREFIX ln: <http://linecontroller.factory/ontology/ln#>
SELECT ?planId ?planKind ?refTaskId ?refScopeId ?childId
WHERE {
    ?plan a ln:Plan .
    ?plan ln:planKind ?planKind .
    OPTIONAL { ?plan ln:refTask ?refTask . ?refTask ln:taskId ?refTaskId . }
    OPTIONAL { ?plan ln:refScope ?refScope . ?refScope ln:scopeId ?refScopeId . }
    OPTIONAL { ?plan ln:planChildren ?childList . ?childList rdf:rest*/rdf:first ?child . }
}
ORDER BY ?planId ?childId
```

---

## Part 4: Template Generation

Templates transform SPARQL results into Erlang/OTP modules.

### Generate Plan Module

`templates/plan_module.tera`:
```erlang
-module({{project.name}}_plan).
-export([plan/0]).

plan() ->
    {seq, [
        {task, validate_order},
        {scope, payment_scope, {task, process_payment}},
        {task, notify_customer}
    ]}.
```

**Generated:** `src/generated/order_fulfillment_line_plan.erl`

### Generate Callback Module

`templates/callback_module.tera`:
```erlang
-module({{project.name}}_cb).
-behaviour(ln_ctrl).
-export([init/1, plan/1, task/3, effect_result/4]).

init(Ctx) -> {ok, Ctx}.

plan(Ctx) ->
    Plan = {{project.name}}_plan:plan(),
    {ok, Plan, Ctx}.

{% for task in tasks %}
task({{task.taskId}}, _Input, Ctx) ->
    %% {{task.taskDescription}}
    {% if task.effectEndpoint %}
    EffectSpec = #{type => {{task.effectType}}, endpoint => <<"{{task.effectEndpoint}}">>},
    Cont = fun(Result) -> {ok, Result} end,
    {effect, EffectSpec, Cont, Ctx};
    {% else %}
    {ok, ok, Ctx};
    {% endif %}
{% endfor %}
```

**Generated:** `src/order_fulfillment_line_cb.erl`

---

## Part 5: Manufacturing Pipeline

### Step-by-Step Workflow

```bash
# 1. Validate ontology and templates
./bin/dot validate

# Output:
# ================================================================
# LineController Factory - Preflight Validation
# ================================================================
# [1/5] Checking ontology files...
# 🟢 Ontology file found
# [2/5] Checking SPARQL queries...
#   ✓ line_meta.sparql
#   ✓ tasks.sparql
#   ✓ scopes.sparql
#   ✓ plan_nodes.sparql
# 🟢 All SPARQL queries found
# [3/5] Checking templates...
#   ✓ plan_module.tera
#   ✓ callback_module.tera
# 🟢 All templates found
# 🟢 VALIDATION PASSED

# 2. Generate Erlang/OTP modules
./bin/dot sync

# Output:
# ================================================================
# LineController Factory - Manufacturing Pipeline
# ================================================================
# [GENERATE] Running ggen sync...
# Generating modules from ontology...
#   → src/generated/order_fulfillment_line_plan.erl
#   → src/order_fulfillment_line_cb.erl
#   → src/order_fulfillment_line_app.erl
#   → test/order_fulfillment_line_tests.erl
# 🟢 Manufacturing successful
# Duration: 1847ms
# Receipt:  receipts/last.json

# 3. View build receipt
./bin/dot receipt

# Output:
# {
#   "timestamp": "2026-02-11T04:45:00Z",
#   "duration_ms": 1847,
#   "input_hash": "a7f3...",
#   "output_hash": "b2e9...",
#   "generated_files": ["src/generated/...", ...]
# }

# 4. Compile generated code
rebar3 compile

# 5. Run tests
rebar3 eunit
```

---

## Part 6: Generation Rules

The "`.`" operator uses rules defined in `ggen.toml`:

| Rule | Query | Template | Output |
|------|-------|----------|--------|
| generate-plan-module | plan_nodes.sparql | plan_module.tera | src/generated/*_plan.erl |
| generate-callback-module | tasks.sparql | callback_module.tera | src/*_cb.erl |
| generate-rebar-config | line_meta.sparql | rebar_config.tera | rebar.config |
| generate-app-src | line_meta.sparql | app_src.tera | src/*.app.src |
| generate-tests | tasks.sparql | tests.tera | test/*_tests.erl |

**Manufacturing Claim:**
- Ontology change → regenerated code in <30 seconds
- Deterministic: same ontology → identical code (verified via receipt hashes)

---

## Part 7: Modification Exercises

### Exercise 1: Add a New Task

**Goal:** Add a "ship_order" task to the fulfillment line.

**Steps:**
1. Edit `ontology/order_line.ttl`:
```turtle
order:ShipOrderTask a ln:Task ;
    ln:taskId "ship_order" ;
    ln:taskDescription "Ship order to customer" ;
    ln:hasEffect order:ShipOrderEffect .

order:ShipOrderEffect a ln:Effect ;
    ln:effectType "http_post" ;
    ln:effectEndpoint "/api/shipping/create" .
```

2. Add to plan:
```turtle
order:RootPlan a ln:Plan ;
    ln:planKind "seq" ;
    ln:planChildren (
        order:ValidatePlan
        order:PaymentPlan
        order:ShipPlan        # NEW
        order:NotifyPlan
    ) .

order:ShipPlan a ln:Plan ;
    ln:planKind "task" ;
    ln:refTask order:ShipOrderTask .
```

3. Regenerate:
```bash
./bin/dot sync
rebar3 compile
rebar3 eunit
```

4. Verify: Check `src/order_fulfillment_line_cb.erl` for new `task(ship_order, ...)` clause.

### Exercise 2: Add a Scope

**Goal:** Wrap payment processing in a cancellable scope.

```turtle
order:PaymentScope a ln:Scope ;
    ln:scopeId "payment_scope" .

order:PaymentPlan a ln:Plan ;
    ln:planKind "scope" ;
    ln:refScope order:PaymentScope ;
    ln:scopeBody order:PaymentBody .

order:PaymentBody a ln:Plan ;
    ln:planKind "task" ;
    ln:refTask order:ProcessPaymentTask .
```

Regenerate and test cancellation:
```erlang
{ok, CaseId} = ln_ctrl:new_case(#{callback_module => order_fulfillment_line_cb}),
ok = ln_ctrl:cancel_scope(CaseId, payment_scope),
{ok, Status} = ln_ctrl:status(CaseId),
?assertMatch(#{state := cancelled}, Status).
```

### Exercise 3: Add XOR Branch

**Goal:** Route high-value orders to manual approval.

```turtle
order:ApprovalGate a ln:Plan ;
    ln:planKind "xor" ;
    ln:planChildren (
        order:AutoApprovePlan
        order:ManualApprovePlan
    ) .
```

Regenerate and verify `{xor, [...]}` in plan term.

---

## Part 8: Troubleshooting

### Issue: ". sync fails with 'ggen not found'"

**Solution:**
```bash
cargo install ggen-cli
# OR use Docker:
docker run -v $(pwd):/work linecontroller/ggen sync
```

### Issue: "Generated code doesn't compile"

**Diagnosis:**
```bash
./bin/dot validate        # Check for ontology errors
rebar3 compile 2>&1 | grep error
```

**Common causes:**
- SPARQL query returns empty results → template renders empty module
- Template syntax error → malformed Erlang
- Missing dependency (ln_ctrl)

**Fix:**
1. Validate ontology: all `?` variables in SPARQL must bind
2. Test template in isolation: `ggen render template.tera context.json`
3. Check `rebar.config` has ln_ctrl dependency

### Issue: "Receipts show different hashes on identical inputs"

**Diagnosis:**
Non-deterministic generation (e.g., timestamps in generated code).

**Fix:**
Remove dynamic values from templates:
```tera
%% BAD:  Generated at {{now()}}
%% GOOD: Generated by ggen from ontology/order_line.ttl
```

---

## Part 9: Manufacturing Claims (Measurable)

**Claim 1: Speed**
- Target: Ontology change → deployed code <30 seconds
- Measure: `./bin/dot sync` duration from receipt

**Claim 2: Determinism**
- Target: Same ontology → identical code (hash match)
- Measure: `diff <(./bin/dot receipt | jq .output_hash) <(./bin/dot sync && ./bin/dot receipt | jq .output_hash)`

**Claim 3: Replacement**
- Target: 5:1 productivity ratio (ontology vs hand-coding)
- Measure: Time to add new connector: 1 day (ontology) vs 3-6 months (hand-code)

---

## Part 10: What Next?

### Beginner Path
1. Modify this example: add task, scope, XOR branch
2. Create new line: finance close, incident triage
3. Write custom templates: add metrics, logging

### Advanced Topics
- Multi-line composition: lines calling lines
- Dynamic plans: generate plan terms at runtime
- Effect handlers: HTTP, gRPC, message queues
- Testing: mock effects, inject failures
- Deployment: GCP Cloud Run, GKE

### Resources
- ln_ctrl docs: `/docs/LN_CTRL_GUIDE.md`
- ggen reference: `https://github.com/linecontroller/ggen`
- Examples: `/examples/` (security-ops, finance-close, it-ops)

---

## Appendix: File Inventory

```
lnctrl-zero-to-line/
├── bin/
│   └── dot                     # "." operator wrapper
├── ontology/
│   └── order_line.ttl          # RDF workflow spec
├── sparql/
│   ├── line_meta.sparql        # Extract line name
│   ├── tasks.sparql            # Extract tasks + effects
│   ├── scopes.sparql           # Extract scopes
│   └── plan_nodes.sparql       # Extract plan structure
├── templates/
│   ├── plan_module.tera        # Generate plan term
│   ├── callback_module.tera    # Generate ln_ctrl callback
│   ├── rebar_config.tera       # Generate rebar3 config
│   ├── app_src.tera            # Generate .app.src
│   ├── app_module.tera         # Generate OTP app
│   ├── supervisor.tera         # Generate OTP supervisor
│   └── tests.tera              # Generate EUnit tests
├── src/
│   ├── generated/
│   │   └── *_plan.erl          # GENERATED: plan module
│   ├── *_cb.erl                # GENERATED: callback
│   ├── *_app.erl               # GENERATED: OTP app
│   ├── *_sup.erl               # GENERATED: OTP supervisor
│   └── *.app.src               # GENERATED: app resource
├── test/
│   └── *_tests.erl             # GENERATED: EUnit tests
├── receipts/
│   ├── last.json               # Build receipt
│   └── last.sha                # Output hash
├── ggen.toml                   # Generation rules
├── rebar.config                # GENERATED
└── README.md                   # This file
```

---

## Manufacturing Operating Model

**Replacement Thesis:**
- Integration engineers maintaining workflow code → **REPLACED** by ontology authors
- Custom API glue per company → **REPLACED** by industry ontology (Σ) + connectors
- Manual testing and deployment → **REPLACED** by deterministic generation + receipts

**Standardization Mandate:**
- Companies adapt to ln ontology (Σ)
- We do NOT customize platform per company
- Companies refusing standardization are **replaced** by competitors adopting Σ

**Economics:**
- Fixed cost: Build ln ontology once
- Marginal cost: Near-zero deployment per company
- Productivity: 5:1 (ontology vs hand-coding)

**No euphemisms. This is replacement, not augmentation.**

---

**Built with LineController Factory**
Version 0.1.0 | Generated deterministically from ontology | Receipt-verified

