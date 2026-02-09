# Generative Analysis for Constructive Systems - FAQ

**Frequently Asked Questions about CRE (Common Runtime Environment) - A YAWL Workflow Engine**

Version: 0.3.0 | Last Updated: 2026-02-07

---

## Quick Start

**New to CRE?** Start here:

1. [What is CRE and why should I use it?](#1-what-is-cre)
2. [Do I need to know Erlang to use CRE?](#2-do-i-need-learn-erlang)
3. [How do I get started in 5 minutes?](#how-do-i-get-started-5-minute-quickstart)

---

## Table of Contents

1. [Getting Started](#getting-started)
2. [Business Context Documents](#business-context-documents)
3. [Workflow Patterns](#workflow-patterns)
4. [Compilation & Execution](#compilation--execution)
5. [Verification](#verification)
6. [Swarm Coordination](#swarm-coordination)
7. [Tooling](#tooling)
8. [Adoption](#adoption)

---

## Getting Started

### 1. What is CRE?

**CRE** (Common Runtime Environment) is a production-grade workflow engine based on **YAWL** (Yet Another Workflow Language) and implemented in **Erlang/OTP**. It provides:

- **43 workflow patterns** covering all common workflow constructs
- **Petri net formalism** for provably correct workflow execution
- **Human-in-the-loop** approval workflows with LLM integration
- **OpenTelemetry observability** for monitoring and debugging
- **YAML-to-code compilation** from declarative specifications

CRE implements the **Joe Armstrong design principle**: one real OTP runner (`gen_pnet`), everything else is pure helpers/utilities.

**Links:** [README.md](README.md) | [ARCHITECTURE.md](ARCHITECTURE.md)

---

### 2. Do I need to learn Erlang?

**Short answer:** No, not for basic usage.

**Longer answer:**

| Task | Erlang Required? |
|------|------------------|
| Writing YAML workflows | No |
| Running existing workflows | No |
| Basic configuration | No |
| Creating custom patterns | Yes (basic Erlang) |
| Advanced debugging | Yes |
| Contributing to core | Yes |

For most users, you define workflows in **YAML 0.2 format** and CRE compiles and executes them. You only need Erlang if you want to:
- Create custom workflow patterns
- Extend the core engine
- Debug complex issues

**Links:** [Quick Start](QUICK_START.md) | [YAML Specification](YAWL_COMPILE_COMPLETE_GUIDE.md)

---

### 3. How do I install CRE?

```bash
# Prerequisites: Erlang/OTP 25+ and rebar3
# From source:
git clone https://github.com/joergen7/cre.git
cd cre
rebar3 compile
rebar3 ct  # Run tests to verify

# Or use as a dependency in rebar.config:
{deps, [
    {cre, {git, "https://github.com/joergen7/cre.git", {branch, "master"}}}
]}.
```

**Links:** [BUILD_SYSTEM.md](BUILD_SYSTEM.md) | [DEPLOYMENT.md](DEPLOYMENT.md)

---

## Business Context Documents

### 4. Why write Business Context Documents (BCDs)?

**BCDs bridge the gap** between business requirements and technical implementation. Unlike traditional specifications which focus on **what** to build, BCDs explain **why** and **for whom**.

A BCD contains:
- **Business objectives** - What problem are we solving?
- **Stakeholders** - Who cares about this workflow?
- **Success criteria** - How do we know it works?
- **Assumptions and constraints** - What are our boundaries?
- **Traceability** - How do requirements map to implementation?

**Without a BCD**, you risk building technically correct solutions that don't solve the actual business problem.

**Example:**
```yaml
# business_context: order_fulfillment
#
# Objective: Reduce order-to-shipment time from 48h to 4h
# Stakeholders: Customers, Warehouse, Finance, Support
# Success: 95% of orders shipped within 4h, 99.9% accuracy
#
# Requirements:
# - R1: Validate inventory before payment (Finance requirement)
# - R2: Parallel warehouse prep and payment (speed requirement)
# - R3: Human approval for orders > $10k (risk requirement)
#
# Traceability:
# - R1 -> yawl:InventoryCheck task
# - R2 -> P2_ParallelSplit pattern
# - R3 -> yawl_approval checkpoint
```

---

### 5. Aren't specifications enough? Why add BCDs?

Technical specifications describe **structure** (tasks, flows, data). BCDs describe **intent** (why these tasks, why this order, why these decisions).

**Example distinction:**

| Specification | Business Context |
|---------------|------------------|
| "Task A calls Task B" | "Task B validates regulatory compliance before proceeding" |
| "XOR split with conditions" | "Only high-risk orders go through manual review" |
| "Timeout: 30 seconds" | "Customer experience requirement: no waiting > 30s" |

**Benefits:**
- **Better implementations** - Engineers understand the "why"
- **Easier maintenance** - Future changes respect original intent
- **Clearer testing** - Tests validate business outcomes, not just code paths
- **Stakeholder alignment** - Non-technical stakeholders can review and validate

---

### 6. How do I write a BCD?

Start with this template:

```markdown
# Business Context Document: [Workflow Name]

## Purpose
[What business problem does this solve?]

## Objectives
1. [Measurable goal 1]
2. [Measurable goal 2]

## Stakeholders
| Role | Interest | Success Criteria |
|------|----------|-----------------|
| [Who] | [What they care about] | [How they measure success] |

## Requirements
| ID | Description | Source | Priority |
|----|-------------|--------|----------|
| R1 | [What] | [Who requested] | P1/P2/P3 |

## Assumptions
- [Assumption 1]
- [Assumption 2]

## Constraints
- [Technical constraint]
- [Business constraint]
- [Regulatory constraint]

## Success Criteria
- [Metric 1]: [Target value]
- [Metric 2]: [Target value]

## Traceability
| Requirement | Implementation | Verification |
|-------------|----------------|--------------|
| R1 | [Which task/pattern] | [How we test it] |
```

---

## Workflow Patterns

### 7. Why 43 patterns? Which ones do I actually need?

The **43 YAWL workflow patterns** (from the Workflow Patterns Initiative) represent a **complete catalog** of workflow control flow constructs. You don't need all of them for any given workflow, but having the full catalog means:

- **Any workflow can be expressed** - No missing patterns
- **Standard terminology** - "Parallel Split" means the same thing everywhere
- **Composability** - Patterns combine predictably

**Most-used patterns (80/20 rule):**

| Pattern | Usage | Frequency |
|---------|--------|-----------|
| P1: Sequence | Linear processes | 100% |
| P2: Parallel Split | Concurrent tasks | 60% |
| P3: Synchronization | Join parallel branches | 55% |
| P4: Exclusive Choice | If/then/else | 80% |
| P5: Simple Merge | Converge branches | 70% |
| P6: Multiple Choice | Enable multiple paths | 40% |
| P10: Arbitrary Cycles | Loops | 35% |
| P11: Implicit Termination | Auto-complete | 90% |

**Links:** [YAWL_PATTERNS_REFERENCE.md](YAWL_PATTERNS_REFERENCE.md) | [43_PATTERNS_COMPLETE.md](43_PATTERNS_COMPLETE.md)

---

### 8. What's the difference between AND, OR, and XOR splits?

| Split Type | Meaning | Petri Net Semantics | Use When |
|------------|---------|---------------------|----------|
| **AND** | All branches execute | Fork tokens to all outputs | Parallel processing of independent tasks |
| **OR** | One or more branches execute | Conditionally fork to some outputs | Conditional parallel processing |
| **XOR** | Exactly one branch executes | Fork token to one output | Mutually exclusive alternatives |

**Example:**

```yaml
# AND split - All branches run in parallel
tasks:
  - name: prepare_shipping
    split: AND
    flows_to: [check_inventory, process_payment, arrange_pickup]

# XOR split - Only one branch runs
  - name: route_order
    split: XOR
    flows_to: [standard_shipping, express_shipping, pickup]
    conditions:
      - if: order.urgency == "high"
        then: express_shipping
      - if: order.type == "pickup"
        then: pickup
      - else: standard_shipping
```

---

### 9. How do I handle human approvals?

CRE supports **human-in-the-loop** workflows via approval checkpoints:

```yaml
tasks:
  - name: manager_approval
    type: human
    approval_config:
      required_approver: human
      timeout: 86400000  # 24 hours
      escalation:
        - level_1: manager
        - level_2: director
```

**Or via Erlang:**

```erlang
%% Create approval checkpoint
{ok, CheckpointId} = yawl_approval:create_checkpoint(
    <<"expense_report">>,
    manager_review,
    #{
        required_approver => human,
        timeout => 86400000,
        metadata => #{
            amount => 5000,
            requestor => <<"alice">>
        }
    }
).

%% Later, approve/deny
yawl_approval:approve(CheckpointId, human_cli, <<"Approved within budget">>).
```

**Links:** [HUMAN_IN_THE_LOOP.md](HUMAN_IN_THE_LOOP.md)

---

### 10. Can I mix patterns?

**Yes!** Patterns compose. Complex workflows are built from pattern combinations:

```yaml
# Example: Parallel (P2) → Choice (P4) → Loop (P10)
workflow: order_processing

tasks:
  - name: split_work
    pattern: P2_ParallelSplit
    flows_to: [inventory_check, payment_check]

  - name: decide_next
    pattern: P4_ExclusiveChoice
    flows_to: [ship_order, hold_order, cancel_order]

  - name: retry_failed
    pattern: P10_ArbitraryCycles
    flows_to: [payment_check]  # Loop back
```

**Compositional rules:**
- Patterns connect via **places** (Petri net nodes)
- Output tokens of pattern A feed into input places of pattern B
- Data flows along with control tokens

---

## Compilation & Execution

### 11. How does YAML become executable code?

CRE follows a **multi-stage compilation pipeline**:

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   YAML      │ -> │   Parse     │ -> │  Compile    │ -> │   Runtime   │
│ Spec 0.2    │    │ to records  │    │ to modules  │    │ Execution  │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
                        |                  |                  |
                        v                  v                  v
                  wf_yaml_spec     yawl_compile       gen_yawl
                  (yamerl)         (code gen)         (gen_pnet)
```

**Stage 1: Parse**
```erlang
{ok, Spec} = wf_yaml_spec:from_yaml_file("workflow.yaml").
```

**Stage 2: Compile**
```erlang
{ok, Compiled} = yawl_compile:compile(Spec, #{}).
%% Generates: yawl_<NetId>.erl modules
```

**Stage 3: Execute**
```erlang
{ok, Pid} = wf_yawl_executor:start_workflow(Compiled, #{}).
```

**Generated module structure:**
```erlang
-module(yawl_my_workflow).
-behaviour(gen_pnet).

place_lst() -> [input, task1, task2, output].
trsn_lst() -> [t_task1, t_task2].
init_marking(input, _) -> [start]; init_marking(_, _) -> [].
preset(t_task1) -> [input].
preset(t_task2) -> [task1].

is_enabled(t_task1, #{input := [start]}, _) -> true.
fire(t_task1, _, _) -> {produce, #{task1 => [done]}}.
```

**Links:** [YAWL_COMPILE_COMPLETE_GUIDE.md](YAWL_COMPILE_COMPLETE_GUIDE.md) | [wf_yawl_executor.erl](../src/wf/wf_yawl_executor.erl)

---

### 12. What happens at runtime?

CRE uses **Petri net token flow** for execution:

1. **Initialization**: Place `input` gets initial token
2. **Enablement**: Transition checks if all input places have tokens
3. **Firing**: Enabled transition consumes input tokens, produces output tokens
4. **Progress**: Tokens flow through the network until quiescence

```erlang
%% Start workflow
{ok, Pid} = gen_yawl:start_link(yawl_my_workflow, #{}).

%% Fire one transition
{ok, Receipt} = gen_yawl:step(Pid).

%% Fire until complete or stuck
{ok, Receipts} = gen_yawl:drain(Pid, 1000).

%% Check current state
{ok, Marking} = gen_yawl:marking(Pid).
```

**Key concepts:**
- **Marking**: Map of places to their tokens
- **Mode**: Specific token selection for firing
- **Receipt**: Audit record of each transition firing
- **Quiescence**: No more enabled transitions (workflow done or stuck)

**Links:** [GEN_PNET_USER_GUIDE.md](GEN_PNET_USER_GUIDE.md) | [pnet_marking_algebra.md](pnet_marking_algebra.md)

---

### 13. How do I debug a running workflow?

**1. Check the marking:**
```erlang
{ok, Marking} = gen_yawl:marking(Pid).
%% #{input => [], task1 => [done], task2 => []}
```

**2. Query specific place:**
```erlang
{ok, Tokens} = gen_yawl:ls(Pid, task1).
%% [done]
```

**3. Get execution statistics:**
```erlang
{ok, Stats} = gen_yawl:stats(Pid).
%% #stats{current = #stat{fps = 5}, ...}  % 5 fires per second
```

**4. Enable debug logging:**
```erlang
logger:set_primary_config(level, debug).
yawl_telemetry:set_verbosity(debug).
```

**5. Use OpenTelemetry traces:**
```bash
# View traces in Jaeger
docker run -d -p 16686:16686 jaegertracing/all-in-one:latest
open http://localhost:16686
```

**Links:** [TROUBLESHOOTING.md](TROUBLESHOOTING.md) | [YAWL_TELEMETRY_GUIDE.md](YAWL_TELEMETRY_GUIDE.md)

---

## Verification

### 14. How do I prove my workflow is correct?

CRE supports multiple levels of verification:

**Level 1: Static Validation**
```erlang
%% Validate spec structure
{ok, Warnings} = yawl_validate:validate(Spec).
```

Checks:
- All tasks have IDs
- Flows reference valid tasks
- Split/join types are compatible
- No circular dependencies (unless intended)

**Level 2: Property-Based Testing**
```erlang
%% Use PropEr to test invariants
%% "If workflow completes successfully, all tasks executed in valid order"
prop_task_ordering() ->
    ?FORALL(Spec, valid_workflow_gen(),
        begin
            {ok, Result} = execute_workflow(Spec),
            ?WHEN(is_completed(Result)),
            ?IMPLIES(true, verify_task_order(Result))
        end).
```

**Level 3: Model Checking**
```bash
# Use Concuerror for exhaustive state exploration
rebar3 conc -m yawl_workflow_SUITE
```

**Level 4: Soundness Verification**
For Petri nets, **soundness** means:
1. **Option to complete**: Every workflow instance can reach completion
2. **No dead tasks**: No transitions remain enabled after completion
3. **Proper completion**: Workflow completes with the correct final marking

```erlang
%% Check workflow soundness
yawl_validate:check_soundness(Spec).
```

**Links:** [VERIFICATION_REPORT.md](VERIFICATION_REPORT.md) | [TESTING.md](TESTING.md)

---

### 15. What testing tools does CRE provide?

| Tool | Purpose | Usage |
|------|---------|-------|
| **EUnit** | Unit tests | `rebar3 eunit` |
| **Common Test** | Integration tests | `rebar3 ct` |
| **PropEr** | Property-based tests | `rebar3 proper` |
| **Concuerror** | Model checking | `rebar3 conc` |
| **Dialyzer** | Static analysis | `rebar3 dialyzer` |
| **XES** | Process mining logs | `wf_xes:export_log/1` |

**Example test:**
```erlang
sequence_test() ->
    % Given: A simple sequence workflow
    Spec = test_workflow:sequence_spec(),

    % When: We execute it
    {ok, Result} = wf_yawl_executor:execute(Spec, #{}),

    % Then: All tasks complete in order
    ?assertEqual(completed, result_status(Result)),
    ?assertMatch([task1, task2, task3], execution_order(Result)).
```

**Links:** [TEST_STATUS.md](TEST_STATUS.md) | [TEST_ORGANIZATION.md](TEST_ORGANIZATION.md)

---

## Swarm Coordination

### 16. How do agents coordinate through workflows?

CRE's **Swarm Coordination** model treats workflows as the **coordination fabric** for multi-agent systems. Instead of agents coordinating directly (which creates brittle point-to-point connections), agents coordinate through a shared workflow.

**Traditional model (fragile):**
```
Agent A --rpc--> Agent B
   |               |
   +--rpc--> Agent C
```

**Swarm model (robust):**
```
Agent A --(inject)--> Workflow --(offer)--> Agent B
                          |
                          +--(offer)--> Agent C
```

**Benefits:**
- **Decoupling** - Agents don't need to know about each other
- **Audit trail** - All coordination events logged
- **Timeout/retry** - Built into workflow engine
- **Human oversight** - Approval checkpoints anywhere

**Example: AGI Symposium Ω**
```yaml
# 20 agents coordinate through a 5-net, 43-pattern workflow
nets:
  - name: Symposium
    patterns:
      - P42_ThreadSplit  # Split into 4 mega-threads
      - P3_Synchronization # Join after completion

  - name: ProgramThread
    roles: [ProgramChair, AreaChair, Reviewer, EthicsChair]
    patterns:
      - P1_Sequence
      - P4_ExclusiveChoice
      - P21_StructuredLoop
```

**Links:** [THESIS_PHD_WORKFLOW_SWARM.md](THESIS_PHD_WORKFLOW_SWARM.md)

---

### 17. What is the Swarm Turing Test?

The **Swarm Turing Test** is a witness test for **framework insufficiency**. It asks:

> "Did the system produce interactional events that cannot be generated by any known single-objective, fixed-topology AI formalism?"

**Traditional Turing Test:** Can AI imitate human conversation?
**Swarm Turing Test:** Can the system produce behaviors that escape current AI frameworks?

**Passing** the Swarm Turing Test demonstrates that:
1. Certain behaviors are **not representable** in fixed-topology frameworks
2. Workflow patterns generate **state-space topologies** that escape MDP/POMDP representation
3. The **transcript** (not the agents) is the phenomenon

**Links:** [THESIS_PHD_WORKFLOW_SWARM.md](THESIS_PHD_WORKFLOW_SWARM.md) | [AGI_SYMPOSIUM_SIMULATION_COMPLETE.md](AGI_SYMPOSIUM_SIMULATION_COMPLETE.md)

---

### 18. How do I implement an agent?

Agents implement the **cre_worker** behavior:

```erlang
-module(my_agent).
-behaviour(cre_worker).

%% Worker lifecycle
init([_]) -> {ok, #{state => ready}}.

%% Handle work offer from workflow
handle_work_offer(WorkItem, State) ->
    case should_accept(WorkItem) of
        true ->
            {accept_work, WorkItem, State#{state => working}};
        false ->
            {reject_work, State}
    end.

%% Execute the work
do_work(WorkItem, State) ->
    Result = process_task(WorkItem),
    {work_complete, Result, State#{state => ready}}.

%% Optional: Cleanup
terminate(_Reason, _State) -> ok.
```

**Register agent:**
```erlang
cre_worker:register(my_agent, AgentPid).
```

**Links:** [cre_worker.erl](../src/cre_worker.erl) | [cre_master.erl](../src/cre_master.erl)

---

## Tooling

### 19. What tools do I need?

**Essential tools:**

| Tool | Purpose | Install |
|------|---------|---------|
| **Erlang/OTP** | Runtime | [erlang.org](https://www.erlang.org/downloads) |
| **rebar3** | Build tool | `curl -O https://s3.amazonaws.com/rebar3/rebar3` |
| **git** | Version control | System package manager |

**Optional but recommended:**

| Tool | Purpose | Install |
|------|---------|---------|
| **efmt** | Erlang formatter | `rebar3 escriptize` |
| **observer** | Process monitoring | Built into OTP |
| **Jaeger** | Trace visualization | `docker run jaegertracing/all-in-one` |
| **Concuerror** | Model checker | `apt install concuerror` |

**Development setup:**
```bash
# Check versions
erl -version  # Should be OTP 25-28
rebar3 version

# Clone and build
git clone https://github.com/joergen7/cre.git
cd cre
rebar3 get-deps
rebar3 compile

# Run tests
rebar3 ct

# Start shell
rebar3 shell
```

**Links:** [BUILD_SYSTEM.md](BUILD_SYSTEM.md) | [DEPLOYMENT.md](DEPLOYMENT.md)

---

### 20. What IDEs work with CRE?

**Erlang-focused:**
- **Emacs** + erlang-mode (traditional)
- **VS Code** + erlang-formatter extension
- **IntelliJ IDEA** + Erlang plugin

**General-purpose with Erlang support:**
- **Neovim** + vim-erlang-runtime
- **Sublime Text** + Erlang package

**Key features needed:**
- Erlang syntax highlighting
- Rebar3 integration
- Dialyzer integration (for static analysis)

**Recommendation for beginners:** VS Code with extensions:
- `erlang-formatter` by `pguyot`
- `rebar3` integration
- `Erlang Language`

---

## Adoption

### 21. How do I introduce CRE to my team?

**Phase 1: Education (Week 1)**
1. Present the problem CRE solves (workflow orchestration headaches)
2. Demo a simple workflow (Hello World or approval flow)
3. Share documentation links

**Phase 2: Pilot (Weeks 2-4)**
1. Identify a non-critical workflow to migrate
2. Write the BCD together with stakeholders
3. Implement in YAML, validate, test
4. Measure against success criteria

**Phase 3: Review (Week 5)**
1. Document lessons learned
2. Refine the adoption process
3. Decide on wider rollout

**Phase 4: Rollout (Weeks 6+)**
1. Create internal patterns library
2. Train power users
3. Establish support processes

**Resources for onboarding:**
- [QUICK_START.md](QUICK_START.md) - 5-minute getting started
- [WORKFLOW_PATTERNS_LEARNING_PATH.md](WORKFLOW_PATTERNS_LEARNING_PATH.md) - Structured learning
- [examples/](../examples/) - Working code examples

---

### 22. What are common adoption pitfalls?

| Pitfall | Symptoms | Solution |
|---------|----------|----------|
| **Starting too complex** | Team overwhelmed, projects stall | Start with trivial workflows, iterate |
| **Skipping BCDs** | Requirements misunderstood, rework | Always write BCD first, even simple ones |
| **Ignoring observability** | Debugging in production is chaos | Enable telemetry from day one |
| **Custom patterns too early** | Reinventing the wheel | Use built-in 43 patterns first |
| **Poor testing** | Breaks in production | Adopt Test-Driven Development workflow |
| **No documentation** | Tribal knowledge, bus factor | Document workflows and decisions |

---

### 23. How does CRE compare to other workflow engines?

| Feature | CRE | Temporal | Airflow | Camunda |
|---------|-----|----------|---------|---------|
| **Language** | Erlang | Go | Python | Java |
| **Formal foundation** | Petri nets | DAGs | DAGs | BPMN |
| **Patterns** | 43 YAWL | ~10 | ~15 | ~20 |
| **Distributed** | Native (OTP) | Yes | Worker-based | Cluster |
| **Human tasks** | Built-in | Activities | Sensors | User tasks |
| **Verification** | Model checker | None | None | Partial |
| **LLM integration** | Native | Manual | Manual | Manual |

**When to choose CRE:**
- Need provably correct workflows
- Complex coordination patterns
- High reliability requirements
- Erlang/OTP ecosystem already

**When to consider alternatives:**
- Team already knows Go/Python/Java
- Simple DAG-based workflows
- Large ecosystem of existing connectors

---

### 24. What support resources are available?

**Documentation:**
- [docs/](.) - Complete documentation index
- [examples/](../examples/) - Working code examples
- [test/](../test/) - Test cases as examples

**Troubleshooting:**
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Common issues
- [GitHub Issues](https://github.com/joergen7/cre/issues) - Bug reports

**Community:**
- GitHub Discussions (coming soon)
- Stack Overflow tag `cre-workflow` (coming soon)

**Enterprise support:**
- Contact for licensing and SLAs

---

## Quick Reference

### Common Commands

```bash
# Build
rebar3 compile

# Test
rebar3 ct                    # Common Test
rebar3 eunit                 # Unit tests
rebar3 proper                # Property-based tests
rebar3 dialyzer              # Static analysis
rebar3 cover                 # Coverage report

# Run
rebar3 shell                # Interactive shell
rebar3 release              # Build release

# Format
rebar3 efmt -c              # Format check
rebar3 efmt -w              # Format write
```

### Common Erlang Snippets

```erlang
% Start a workflow
{ok, Pid} = gen_yawl:start_link(my_pattern, #{}).

% Fire transitions until complete
{ok, Receipts} = gen_yawl:drain(Pid, 1000).

% Check state
{ok, Marking} = gen_yawl:marking(Pid).

% Compile YAML
{ok, Spec} = wf_yaml_spec:from_yaml_file("workflow.yaml").
{ok, Compiled} = yawl_compile:compile(Spec, #{}).

% Execute workflow
{ok, Executor} = wf_yawl_executor:compile_workflow(Spec).
{ok, Pid, CaseId} = wf_yawl_executor:start_workflow(Executor, #{data => #{}}).

% Telemetry
yawl_telemetry:set_verbosity(debug).
{ok, Metrics} = yawl_telemetry:get_metrics(SpecId, CaseId).
```

### File Locations

| Type | Location |
|------|----------|
| Source | `/Users/sac/cre/src/` |
| Patterns | `/Users/sac/cre/src/patterns/` |
| Tests | `/Users/sac/cre/test/` |
| Docs | `/Users/sac/cre/docs/` |
| Examples | `/Users/sac/cre/examples/` |

---

**Still have questions?**

- Check the [Documentation Index](DOCUMENTATION_INDEX.md)
- Review [Troubleshooting Guide](TROUBLESHOOTING.md)
- Open an issue on [GitHub](https://github.com/joergen7/cre/issues)

---

*Version: 0.3.0 | Last Updated: 2026-02-07*
