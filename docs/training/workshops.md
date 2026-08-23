# Generative Analysis Workshop Materials

## Complete Hands-On Curriculum for Teaching Generative Analysis with YAWL Workflows

**Version**: 1.0.0
**Last Updated**: 2025-02-07
**Target Audience**: Developers, System Architects, Data Engineers
**Prerequisites**: Basic programming knowledge, no Erlang experience required

---

## Table of Contents

1. [Workshop Overview](#workshop-overview)
2. [Session 1: Introduction](#session-1-introduction)
3. [Session 2: BCD Writing](#session-2-bcd-writing)
4. [Session 3: Pattern Selection](#session-3-pattern-selection)
5. [Session 4: YAML Authoring](#session-4-yaml-authoring)
6. [Session 5: Compilation & Execution](#session-5-compilation--execution)
7. [Session 6: Verification](#session-6-verification)
8. [Appendix A: Pattern Reference Card](#appendix-a-pattern-reference-card)
9. [Appendix B: Common Pitfalls](#appendix-b-common-pitfalls)

---

## Workshop Overview

### What is Generative Analysis?

**Generative Analysis** is a methodology for designing and implementing workflow systems using:
- **YAWL** (Yet Another Workflow Language) - A formal workflow specification language
- **Petri Nets** - Mathematical modeling of distributed systems
- **BCD (Business Condition Description)** - Structured requirements documentation
- **Pattern-Based Design** - 43 reusable workflow control patterns

### Learning Objectives

By the end of this workshop, participants will be able to:

1. Write clear Business Condition Descriptions (BCDs)
2. Select appropriate YAWL workflow patterns for business requirements
3. Author YAML workflow specifications
4. Compile and execute workflows using the CRE runtime
5. Verify workflow correctness using formal methods

### Prerequisites

- **Software**: Erlang/OTP 25+, rebar3
- **Hardware**: Laptop with 4GB+ RAM
- **Materials**: This workshop guide, CRE codebase

### Session Structure

| Session | Duration | Focus |
|---------|----------|-------|
| Session 1 | 30 min | Concepts & Terminology |
| Session 2 | 60 min | Writing BCDs |
| Session 3 | 60 min | Pattern Selection |
| Session 4 | 60 min | YAML Authoring |
| Session 5 | 60 min | Compilation & Execution |
| Session 6 | 60 min | Verification |

**Total Time**: 5.5 hours

---

## Session 1: Introduction

### Duration: 30 minutes

### Learning Objectives

After this session, participants will understand:
- What Generative Analysis is
- The YAWL workflow language basics
- How CRE (Common Runtime Environment) executes workflows
- The 43 YAWL workflow patterns

### Required Tools

```bash
# Clone CRE repository
git clone https://github.com/your-org/cre.git
cd cre

# Install Erlang/OTP (25+)
# On macOS: brew install erlang
# On Ubuntu: sudo apt-get install erlang

# Install rebar3
# On macOS: brew install rebar3
# On Ubuntu: download from https://rebar3.org

# Verify installation
erl -version  # Should show OTP 25+
rebar3 version
```

### Core Concepts

#### What is YAWL?

**YAWL** (Yet Another Workflow Language) is a formal language for defining workflows. It provides:
- **Workflow Control Patterns** - 43 reusable patterns for common workflow scenarios
- **Formal Semantics** - Based on Petri nets for mathematical correctness
- **XML/YAML Specification** - Machine-readable workflow definitions

#### What is a Petri Net?

A **Petri Net** is a mathematical modeling tool for distributed systems:

```
Places (States) -----> Transitions (Actions)
     P1 --[t1]--> P2 --[t2]--> P3

Tokens flow through places, enabling transitions.
A transition fires when all input places have tokens.
```

**Key Terms**:
- **Place** (circle): A state or condition
- **Transition** (rectangle): An action or event
- **Token**: A unit of work flowing through the net
- **Marking**: The distribution of tokens across places

#### What is CRE?

**CRE** (Common Runtime Environment) is an Erlang/OTP-based workflow engine that:
1. Parses YAWL specifications (XML/YAML)
2. Compiles to Petri net modules (gen_pnet)
3. Executes workflows using gen_yawl behavior
4. Provides verification and monitoring tools

### The 43 Workflow Patterns

#### Basic Control Flow (Patterns 1-8)

| Pattern | ID | Description |
|---------|-----|-------------|
| Sequence | P1 | Execute tasks in order |
| Parallel Split | P2 | Execute tasks concurrently |
| Synchronization | P3 | Wait for all branches to complete |
| Exclusive Choice | P4 | Choose one branch based on condition |
| Simple Merge | P5 | Merge without synchronization |
| Multiple Choice | P6 | Choose multiple branches |
| Structured Sync Merge | P7 | Merge multiple paths with sync |
| Multiple Merge | P8 | Merge without waiting |

#### Advanced Patterns (Patterns 9-43)

See [Appendix A](#appendix-a-pattern-reference-card) for complete reference.

### Instructor Notes

**Timing Guidance**:
- 0-10 min: What is Gen Analysis? (Slides + Discussion)
- 10-20 min: YAWL, Petri Nets, CRE (Live Demo)
- 20-30 min: The 43 Patterns (Handout + Q&A)

**Suggested Activities**:
1. Show a simple workflow diagram (Order Processing)
2. Demonstrate running the `orderfulfillment_2_1.yawl` example
3. Have participants brainstorm workflows they encounter

### Exercise 1.1: Workflow Identification

**Instructions**: Identify a workflow from your domain and describe it in simple terms.

**Example**:
```
Workflow: Document Approval
Steps:
1. Author submits document
2. Manager reviews
3. If approved: Forward to Director
4. If rejected: Return to Author
5. Director makes final decision
6. Document is filed
```

**Your Turn**: Describe a workflow from your experience.

---

## Session 2: BCD Writing

### Duration: 60 minutes

### Learning Objectives

After this session, participants will be able to:
- Write clear Business Condition Descriptions (BCDs)
- Extract workflow requirements from BCDs
- Identify actors, data, and conditions
- Document edge cases and exceptions

### What is a BCD?

A **Business Condition Description (BCD)** is a structured document that describes:
- **Business Goal**: What the workflow achieves
- **Actors**: Who participates
- **Preconditions**: What must be true before starting
- **Postconditions**: What is true after completion
- **Normal Flow**: The happy path
- **Exception Flows**: Error handling and alternatives

### BCD Template

```markdown
# BCD: [Workflow Name]

## Business Goal
[What value does this workflow deliver?]

## Actors
| Actor | Role | Description |
|-------|------|-------------|
| [Name] | [Role] | [Responsibilities] |

## Preconditions
- [Condition 1]
- [Condition 2]

## Postconditions
- [Result 1]
- [Result 2]

## Normal Flow
1. [Step 1]
2. [Step 2]
3. [Step 3]

## Exception Flows
| Condition | Action |
|-----------|--------|
| [Exception] | [Handling] |

## Data Requirements
| Data | Type | Source |
|------|------|--------|
| [Name] | [Type] | [Source] |
```

### Exercise 2.1: Write a BCD for Loan Approval

**Instructions**: Complete the BCD below for a Loan Approval workflow.

**Template**:

```markdown
# BCD: Loan Approval Workflow

## Business Goal
Evaluate loan applications and make approval decisions

## Actors
| Actor | Role | Description |
|-------|------|-------------|
| Applicant | Initiator | Submits loan application |
| Credit System | Service | Runs credit check |
| Underwriter | Decision Maker | Reviews application |
| ??? | ??? | ??? |

## Preconditions
- Application form is complete
- ???

## Postconditions
- Applicant is notified of decision
- ???

## Normal Flow
1. Applicant submits loan application
2. System validates application data
3. Credit check is performed
4. ???
5. ???

## Exception Flows
| Condition | Action |
|-----------|--------|
| Invalid application data | Return to applicant for correction |
| Credit score below threshold | ??? |
| ?? | ??? |

## Data Requirements
| Data | Type | Source |
|------|------|--------|
| Application ID | String | User input |
| Credit Score | Integer | Credit System |
| ?? | ?? | ?? |
```

**Time**: 15 minutes

### Exercise 2.2: BCD Peer Review

**Instructions**: Exchange BCDs with a partner. Review and provide feedback on:
- Clarity of business goal
- Completeness of actors
- Missing exception flows
- Data requirements

**Review Checklist**:
- [ ] Business goal is clear and measurable
- [ ] All actors are identified
- [ ] Preconditions are realistic
- [ ] Normal flow covers the happy path
- [ ] Exception flows cover error cases
- [ ] Data requirements are complete

**Time**: 10 minutes

### Exercise 2.3: Advanced BCD - Order Fulfillment

**Instructions**: Study the Order Fulfillment BCD below and identify areas for improvement.

**Given BCD**:

```markdown
# BCD: Order Fulfillment

## Business Goal
Ship products to customers

## Actors
Customer, Warehouse, Payment System

## Normal Flow
1. Customer places order
2. Check inventory
3. Process payment
4. Ship order

## Exceptions
Out of stock
Payment failed
```

**Discussion Questions**:
1. What's missing from the business goal?
2. Are all actors identified?
3. What about partial fulfillment?
4. How are shipping notifications handled?
5. What about cancellations?

**Time**: 10 minutes (Group Discussion)

### Instructor Notes

**Timing Guidance**:
- 0-10 min: BCD Template explanation
- 10-25 min: Exercise 2.1 (Individual writing)
- 25-35 min: Exercise 2.2 (Peer review)
- 35-45 min: Exercise 2.3 (Group discussion)
- 45-60 min: Q&A and additional practice

**Common Mistakes**:
1. **Vague business goals** - "Process data" vs "Calculate monthly interest"
2. **Missing actors** - Forgetting system services, notification services
3. **Incomplete exceptions** - Only considering happy path
4. **Ambiguous conditions** - "If something is wrong" vs "If validation fails"

### Deliverables

By the end of this session, participants should have:
1. A completed BCD for their chosen workflow
2. Peer feedback on their BCD
3. Understanding of BCD best practices

---

## Session 3: Pattern Selection

### Duration: 60 minutes

### Learning Objectives

After this session, participants will be able to:
- Map BCD requirements to YAWL patterns
- Select appropriate patterns for workflow scenarios
- Compose multiple patterns into complete workflows
- Use the pattern decision tree

### Pattern Decision Tree

```
START
  |
  v
Is there a sequence of steps?
  YES --> Use P1 (Sequence)
  NO  --> v

Do tasks run concurrently?
  YES --> v
  |     Must ALL complete before continuing?
  |       YES --> Use P2+P3 (Parallel Split + Synchronization)
  |       NO  --> Use P9 (Discriminator) or P7 (Structured Sync Merge)
  NO  --> v

Choose between alternatives?
  YES --> v
  |     Is choice based on data/condition?
  |       YES --> Use P4 (Exclusive Choice)
  |       NO  --> Use P16 (Deferred Choice)
  NO  --> v

Repeat tasks multiple times?
  YES --> v
  |     Is count known at design time?
  |       YES --> Use P13 (MI Design Time)
  |       NO  --> Use P15 (MI Runtime Unknown)
  NO  --> v

Need to handle cancellation?
  YES --> See P19-P27 (Cancellation Patterns)
```

### Exercise 3.1: Pattern Matching

**Instructions**: Match each scenario to the correct YAWL pattern(s).

| Scenario | Pattern(s) | Rationale |
|----------|------------|-----------|
| Document review flows through: Draft -> Review -> Approve -> Publish | P1 | Sequential execution |
| Order processing: Check inventory, verify payment, prepare shipment in parallel | ? | ? |
| Loan application: If credit_score > 700 then "Fast Track" else "Full Review" | ? | ? |
| Wait for first department to respond, then proceed | ? | ? |
| Interview 3 candidates sequentially for a position | ? | ? |
| Generate reports for all departments, then continue when ALL are done | ? | ? |
| Process items from a queue (count unknown at design time) | ? | ? |
| Allow user to choose between A and B at runtime | ? | ? |

**Time**: 15 minutes

### Exercise 3.2: Pattern Selection for Loan Approval

**Instructions**: Using the Loan Approval BCD from Session 2, select the appropriate YAWL patterns.

**Given**:
```
Normal Flow:
1. Validate application
2. Run credit check (parallel)
3. Verify employment (parallel)
4. Review by underwriter (waits for 2 & 3)
5. Make decision (Approve/Reject)
6. Send notification
```

**Task**: Identify patterns for each step and justify your choices.

**Worksheet**:

```
Step 1: Validate application
Pattern: ___
Reason: ___

Step 2-3: Credit check + Employment verification (parallel)
Pattern: ___ + ___
Reason: ___

Step 4: Underwriter review
Pattern: ___
Reason: ___

Step 5: Decision (Approve/Reject)
Pattern: ___
Reason: ___

Step 6: Notification
Pattern: ___
Reason: ___
```

**Time**: 15 minutes

### Exercise 3.3: Pattern Composition

**Instructions**: Design a complete workflow by composing multiple patterns.

**Scenario**: E-Commerce Order Processing

**Requirements**:
1. Receive order from customer
2. Validate order details
3. Check inventory AND process payment (parallel)
4. If both succeed: Prepare shipment
5. If either fails: Cancel order, notify customer
6. Ship items
7. Send confirmation email

**Task**:
1. Draw the workflow using boxes and arrows
2. Label each decision point with the pattern ID
3. Identify where data flows between patterns

**Time**: 20 minutes

### Pattern Quick Reference

| ID | Pattern | Use When | Parameters |
|----|---------|----------|------------|
| P1 | Sequence | Steps must execute in order | from, to |
| P2 | Parallel Split | Multiple concurrent tasks | branches |
| P3 | Synchronization | Wait for all branches | waits_for |
| P4 | Exclusive Choice | Data-based routing | choices |
| P9 | Discriminator | First completion triggers next | race |
| P13 | MI Design Time | Fixed parallel instances | instances |
| P15 | MI Runtime Unknown | Dynamic instance creation | threshold |
| P19 | Cancel Activity | Cancel single task | target |
| P25 | Cancel Region | Cancel scoped activities | region |

### Instructor Notes

**Timing Guidance**:
- 0-10 min: Pattern decision tree explanation
- 10-25 min: Exercise 3.1 (Individual matching)
- 25-40 min: Exercise 3.2 (Apply to Loan Approval)
- 40-60 min: Exercise 3.3 (Group composition + sharing)

**Teaching Tips**:
1. Start with familiar scenarios from participants' domains
2. Draw patterns on whiteboard as you explain
3. Emphasize that patterns can be composed
4. Use color coding for different pattern types

**Common Challenges**:
1. **Confusing P4 (Exclusive Choice) with P16 (Deferred Choice)**
   - P4: Choice based on data/conditions (deterministic)
   - P16: Choice based on runtime events (non-deterministic)

2. **P3 vs P9 (Synchronization vs Discriminator)**
   - P3: Wait for ALL branches
   - P9: Proceed after FIRST branch completes

3. **Multi-instance pattern selection**
   - P13: Count known at design time
   - P14: Count known when workflow starts
   - P15: Count determined during execution

### Deliverables

By the end of this session, participants should have:
1. Completed pattern matching exercise
2. Selected patterns for their Loan Approval workflow
3. A composed workflow diagram for Order Processing
4. Understanding of pattern composition

---

## Session 4: YAML Authoring

### Duration: 60 minutes

### Learning Objectives

After this session, participants will be able to:
- Write YAWL YAML specifications (version 0.2)
- Define nets, nodes, flows, and pattern instances
- Use the YAML specification structure correctly
- Validate YAML specifications

### YAML Specification Structure

```yaml
yawl_yaml_version: "0.2"

specificationSet:
  yawl_schema_version: "2.1"
  uri: "workflow_identifier"
  metaData:
    title: "Workflow Title"
    version: "1.0"
    author: "Author Name"
    description: "Workflow description"

  rootNet: "main"

  roles:
    - "role1"
    - "role2"

  nets:
    - id: "main"
      type: "NetFacts"
      variables: []
      nodes:
        - id: "Start"
          kind: "inputCondition"
        - id: "task1"
          kind: "task"
          name: "First Task"
          taskType: "human"
        - id: "End"
          kind: "outputCondition"
      flows:
        - from: "Start"
          to: "task1"
        - from: "task1"
          to: "End"

  pattern_instances:
    - id: "pi_001"
      pattern: "P1_Sequence"
      net: "main"
      from: "Start"
      to: "End"

  pattern_registry:
    P1_Sequence:
      macro: "sequence"
```

### Exercise 4.1: Write a Simple Sequence Workflow

**Instructions**: Write a YAML specification for a Document Approval workflow with sequential steps.

**Requirements**:
1. Document is submitted
2. Manager reviews
3. Director approves
4. Document is published

**Template** (fill in the blanks):

```yaml
yawl_yaml_version: "0.2"

specificationSet:
  yawl_schema_version: "2.1"
  uri: "__________"
  metaData:
    title: "__________"
    version: "1.0"
  rootNet: "main"
  nets:
    - id: "main"
      nodes:
        - id: "__________"
          kind: "inputCondition"
        - id: "__________"
          kind: "task"
          name: "Manager Review"
        - id: "__________"
          kind: "task"
          name: "__________"
        - id: "__________"
          kind: "outputCondition"
      flows:
        - from: "__________"
          to: "__________"
        - from: "__________"
          to: "__________"
        - from: "__________"
          to: "__________"
        - from: "__________"
          to: "__________"
  pattern_instances:
    - id: "pi_sequence"
      pattern: "__________"
      net: "main"
      from: "__________"
      to: "__________"
  pattern_registry:
    P1_Sequence:
      macro: "sequence"
```

**Time**: 15 minutes

### Exercise 4.2: Write a Parallel Workflow

**Instructions**: Write a YAML specification for Order Processing with parallel payment and inventory checks.

**Requirements**:
1. Receive order
2. Check inventory AND verify payment (parallel)
3. Wait for both to complete
4. Ship order

**Template**:

```yaml
yawl_yaml_version: "0.2"

specificationSet:
  uri: "order_processing"
  metaData:
    title: "Order Processing"
  rootNet: "main"
  nets:
    - id: "main"
      nodes:
        - id: "Start"
          kind: "inputCondition"
        - id: "ReceiveOrder"
          kind: "task"
          name: "Receive Order"
        - id: "CheckInventory"
          kind: "task"
          name: "Check Inventory"
        - id: "VerifyPayment"
          kind: "task"
          name: "Verify Payment"
        - id: "ShipOrder"
          kind: "task"
          name: "Ship Order"
        - id: "End"
          kind: "outputCondition"
      flows:
        - from: "Start"
          to: "ReceiveOrder"
        # TODO: Add flows for parallel execution
        # TODO: Add flows for synchronization
        # TODO: Add flows to final task
  pattern_instances:
    # TODO: Add Parallel Split pattern instance
    # TODO: Add Synchronization pattern instance
  pattern_registry:
    P2_ParallelSplit:
      macro: "parallel_split"
    P3_Synchronization:
      macro: "synchronization"
```

**Time**: 20 minutes

### Exercise 4.3: Write an Exclusive Choice Workflow

**Instructions**: Write a YAML specification for Loan Decision with exclusive choice.

**Requirements**:
1. Complete credit check
2. If score > 700: Fast track approval
3. If score <= 700: Full underwriting
4. Either path leads to notification

**Template**:

```yaml
yawl_yaml_version: "0.2"

specificationSet:
  uri: "loan_decision"
  metaData:
    title: "Loan Decision Workflow"
  rootNet: "main"
  nets:
    - id: "main"
      nodes:
        - id: "Start"
          kind: "inputCondition"
        - id: "CreditCheck"
          kind: "task"
          name: "Credit Check"
        # TODO: Add nodes for decision branches
        # TODO: Add output condition
      flows:
        - from: "Start"
          to: "CreditCheck"
        # TODO: Add flows for each branch
        # TODO: Add merge flows
  pattern_instances:
    - id: "pi_choice"
      pattern: "P4_ExclusiveChoice"
      net: "main"
      at: "CreditCheck"
      choices: ["FastTrack", "FullUnderwriting"]
      # TODO: Add other required parameters
  pattern_registry:
    P4_ExclusiveChoice:
      macro: "xor_choice"
```

**Time**: 15 minutes

### YAML Validation

```bash
# Validate YAML syntax
erl -pa _build/default/lib/cre/ebin -eval "
  {ok, Spec} = wf_yaml_spec:from_yaml_file(\"workflow.yaml\"),
  case wf_yaml_spec:validate(Spec) of
    ok -> io:format(\"Validation OK~n\");
    {error, Errors} -> io:format(\"Validation failed: ~p~n\", [Errors])
  end,
  init:stop().
"
```

### Instructor Notes

**Timing Guidance**:
- 0-10 min: YAML structure explanation
- 10-25 min: Exercise 4.1 (Simple sequence)
- 25-45 min: Exercise 4.2 (Parallel workflow)
- 45-60 min: Exercise 4.3 (Exclusive choice + validation)

**Teaching Tips**:
1. Start with the simplest workflow (sequence)
2. Show live examples from the CRE codebase
3. Demonstrate validation errors and how to fix them
4. Use a YAML linter to catch syntax errors early

**Common YAML Mistakes**:
1. **Incorrect indentation** (YAML is space-sensitive)
2. **Missing required fields** (id, kind, pattern)
3. **Mismatched flow references** (from/to must reference existing nodes)
4. **Incorrect pattern IDs** (must match registry)
5. **Missing pattern_registry entries**

### Deliverables

By the end of this session, participants should have:
1. A working YAML file for Document Approval
2. A working YAML file for Order Processing
3. A working YAML file for Loan Decision
4. Understanding of YAML validation

---

## Session 5: Compilation & Execution

### Duration: 60 minutes

### Learning Objectives

After this session, participants will be able to:
- Compile YAWL YAML specifications to Erlang modules
- Execute workflows using the CRE runtime
- Interact with running workflows (inject tokens, step through)
- Debug workflow execution issues

### Compilation Process

```
YAML Specification
       |
       v
  wf_yaml_spec:from_yaml_file/1
       |
       v
  yawl_pattern_expander:expand_patterns/2
       |
       v
  yawl_compile:compile/2
       |
       v
  Erlang Modules (gen_pnet behavior)
       |
       v
  compile:file/1
       |
       v
  Loaded Modules
```

### Exercise 5.1: Compile a Workflow

**Instructions**: Compile your Document Approval workflow from Session 4.

**Step 1**: Save your YAML as `document_approval.yaml`

**Step 2**: Start an Erlang shell

```bash
cd /Users/sac/cre
rebar3 compile
rebar3 shell
```

**Step 3**: Compile the workflow

```erlang
%% Load YAML specification
{ok, Spec} = wf_yaml_spec:from_yaml_file("document_approval.yaml").

%% Validate specification
wf_yaml_spec:validate(Spec).

%% Compile to Erlang modules
{ok, Compiled} = yawl_compile:compile(Spec, #{}).

%% View compilation results
maps:get(modules, Compiled).
```

**Expected Output**:

```
{ok, #{
  spec_id => <<"document_approval">>,
  modules => #{
    <<"main">> => <<"-module(yawl_main)...">>
  },
  places => #{<<"main">> => [p_start, ..., p_end]},
  transitions => #{<<"main">> => [t_start, ..., t_finish]},
  ...
}}
```

**Time**: 10 minutes

### Exercise 5.2: Execute a Workflow

**Instructions**: Execute your Document Approval workflow.

**Step 1**: Load and compile (if not already done)

```erlang
{ok, Executor} = wf_yawl_executor:load_workflow_from_yaml("document_approval.yaml").
```

**Step 2**: Start a workflow instance

```erlang
{ok, Pid, CaseId} = wf_yawl_executor:start_workflow(Executor, #{document_id => "DOC001"}).
```

**Step 3**: Execute until quiescent

```erlang
{ok, Receipts} = wf_yawl_executor:execute_step(Pid, 100).
```

**Step 4**: Check final state

```erlang
{ok, State} = wf_yawl_executor:get_workflow_state(Pid).
```

**Step 5**: Stop the workflow

```erlang
ok = wf_yawl_executor:stop_workflow(Pid).
```

**Time**: 15 minutes

### Exercise 5.3: Interactive Execution

**Instructions**: Step through a workflow interactively with token injection.

**Scenario**: Human approval workflow requiring external input.

**YAML** (human_approval.yaml):

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  uri: "human_approval"
  metaData:
    title: "Human Approval Workflow"
  rootNet: "main"
  nets:
    - id: "main"
      nodes:
        - id: "Start"
          kind: "inputCondition"
        - id: "RequestApproval"
          kind: "task"
          name: "Request Approval"
          taskType: "human"
        - id: "ApprovalDecision"
          kind: "task"
          name: "Make Decision"
          taskType: "human"
        - id: "End"
          kind: "outputCondition"
      flows:
        - from: "Start"
          to: "RequestApproval"
        - from: "RequestApproval"
          to: "ApprovalDecision"
        - from: "ApprovalDecision"
          to: "End"
  pattern_instances:
    - id: "pi_seq"
      pattern: "P1_Sequence"
      net: "main"
      from: "Start"
      to: "End"
  pattern_registry:
    P1_Sequence:
      macro: "sequence"
```

**Step-by-Step Execution**:

```erlang
%% 1. Load workflow
{ok, Executor} = wf_yawl_executor:load_workflow_from_yaml("human_approval.yaml").

%% 2. Start workflow
{ok, Pid, CaseId} = wf_yawl_executor:start_workflow(Executor, #{}).

%% 3. Execute one step (fires RequestApproval)
{ok, Receipt1} = wf_yawl_executor:execute_step(Pid).
%% Receipt1 should show: trsn => t_RequestApproval

%% 4. Check state - should be waiting at ApprovalDecision
{ok, State1} = wf_yawl_executor:get_workflow_state(Pid).

%% 5. Inject token to simulate human decision
{ok, InjectReceipt} = wf_yawl_executor:inject_token(Pid, 'ApprovalDecision', approved).

%% 6. Execute to completion
{ok, Receipts} = wf_yawl_executor:execute_step(Pid, 10).

%% 7. Check final state
{ok, FinalState} = wf_yawl_executor:get_workflow_state(Pid).

%% 8. Stop workflow
ok = wf_yawl_executor:stop_workflow(Pid).
```

**Time**: 20 minutes

### Exercise 5.4: Debugging Workflow Issues

**Instructions**: Fix and debug a workflow with errors.

**Broken YAML** (broken_workflow.yaml):

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  uri: "broken_workflow"
  metaData:
    title: "Broken Workflow"
  rootNet: "main"
  nets:
    - id: "main"
      nodes:
        - id: "Start"
          kind: "inputCondition"
        - id: "Task1"
          kind: "task"
        # Missing output condition!
      flows:
        - from: "Start"
          to: "Task1"
        # Missing flow to end!
  pattern_instances: []
  pattern_registry: {}
```

**Task**:
1. Try to compile this workflow - what error do you get?
2. Fix the errors
3. Verify the workflow executes correctly

**Time**: 10 minutes

### Instructor Notes

**Timing Guidance**:
- 0-5 min: Compilation process overview
- 5-15 min: Exercise 5.1 (Compile)
- 15-30 min: Exercise 5.2 (Execute)
- 30-50 min: Exercise 5.3 (Interactive)
- 50-60 min: Exercise 5.4 (Debugging)

**Teaching Tips**:
1. Walk through compilation process step by step
2. Show the generated Erlang code
3. Demonstrate common errors and fixes
4. Use visual diagrams to show token flow

**Common Execution Issues**:
1. **Module not found** - Need to load compiled modules
2. **Workflow hangs** - Waiting for external input (use inject)
3. **Aborted execution** - No enabled transitions (check marking)
4. **Wrong place names** - Generated names may differ from YAML

### Deliverables

By the end of this session, participants should have:
1. Successfully compiled their workflows
2. Executed workflows to completion
3. Interacted with workflows using token injection
4. Debugged and fixed workflow issues

---

## Session 6: Verification

### Duration: 60 minutes

### Learning Objectives

After this session, participants will be able to:
- Verify workflow correctness using formal methods
- Check for soundness properties (liveness, boundedness)
- Test workflows with various inputs
- Use CRE verification tools

### Verification Concepts

#### What is Workflow Correctness?

A workflow is **correct** if it:
1. **Option to Complete**: Every execution path can reach completion
2. **Proper Completion**: Final state is well-defined
3. **No Dead Transitions**: All tasks are reachable
4. **Boundedness**: The number of tokens doesn't grow infinitely

#### Verification Methods

```
Static Analysis
  - Parse specification
  - Build Petri net model
  - Check structural properties

State Space Exploration
  - Generate all reachable states
  - Check for deadlocks
  - Verify liveness

Property Checking
  - Define properties (invariants)
  - Test against generated states
  - Counterexample generation
```

### Exercise 6.1: Visual Verification

**Instructions**: Draw the Petri net for your Document Approval workflow.

**Task**:
1. Identify all places (states)
2. Identify all transitions (actions)
3. Draw arcs showing token flow
4. Mark initial token (p_start)
5. Mark final token (p_end)

**Worksheet**:

```
Places: [p_start, ..., p_end]

Transitions: [t_start, ..., t_finish]

Arcs (preset -> transition -> postset):
  p_start --t_start--> ???
  ??? --> ???

Initial marking: p_start = [init]

Final marking: p_end = [done]
```

**Time**: 15 minutes

### Exercise 6.2: State Space Exploration

**Instructions**: Trace all possible execution paths for a workflow.

**Workflow**: Exclusive Choice (Credit Check)

```
Start -> CreditCheck -> [FastTrack OR FullUnderwriting] -> Notify -> End
```

**Task**: Complete the state transition table.

| State | Enabled Transitions | Next State(s) |
|-------|---------------------|---------------|
| {p_start=[init]} | t_CreditCheck | {p_CreditCheck=[checking]} |
| {p_CreditCheck=[checking]} | t_complete_check | ??? |
| ??? | ??? | ??? |

**Questions**:
1. Are all states reachable from the initial state?
2. Can the workflow reach the final state from all paths?
3. Are there any dead transitions?
4. Is the workflow bounded (tokens don't multiply)?

**Time**: 20 minutes

### Exercise 6.3: Property Checking

**Instructions**: Define and verify properties for a workflow.

**Workflow**: Order Processing with cancellation

**Properties to Verify**:
1. **Order shipped OR order cancelled**: Exactly one outcome
2. **Payment processed AT MOST once**: No double charging
3. **Inventory checked BEFORE shipment**: Ordering constraint
4. **Notification always sent**: Completion guarantee

**Task**: Write property checks using CRE verification API.

```erlang
%% Property 1: Order shipped OR order cancelled (exactly one)
property_exclusive_outcome(State) ->
    Shipped = maps:get(shipped, State, false),
    Cancelled = maps:get(cancelled, State, false),
    case {Shipped, Cancelled} of
        {true, false} -> ok;  %% Shipped, not cancelled
        {false, true} -> ok;  %% Cancelled, not shipped
        _ -> {error, invalid_outcome}
    end.

%% Property 2: Payment processed at most once
property_single_payment(Receipts) ->
    PaymentCount = lists:foldl(fun(Receipt, Acc) ->
        case maps:get(trsn, Receipt) of
            t_ProcessPayment -> Acc + 1;
            _ -> Acc
        end
    end, 0, Receipts),
    case PaymentCount of
        0 -> {error, no_payment};
        1 -> ok;
        N when N > 1 -> {error, multiple_payments}
    end.

%% TODO: Write Property 3
%% TODO: Write Property 4
```

**Verification Test**:

```erlang
%% Run workflow and verify properties
{ok, Executor} = wf_yawl_executor:load_workflow_from_yaml("order_processing.yaml").
{ok, Pid, CaseId} = wf_yawl_executor:start_workflow(Executor, #{}).
{ok, Receipts} = wf_yawl_executor:execute_step(Pid, 100).

%% Check properties
case property_exclusive_outcome(maps:get(final_state, Receipts)) of
    ok -> io:format("Property 1: OK~n");
    Error -> io:format("Property 1: FAILED - ~p~n", [Error])
end.
```

**Time**: 20 minutes

### Exercise 6.4: Testing Workflow Variations

**Instructions**: Test your workflow with different input scenarios.

**Test Cases**:

| Scenario | Input Data | Expected Result | Actual Result |
|----------|------------|-----------------|---------------|
| Normal case | #{valid => true} | Complete successfully | ??? |
| Error case | #{valid => false} | Handle gracefully | ??? |
| Edge case | #{empty => true} | ??? | ??? |
| Concurrent | #{parallel => true} | ??? | ??? |

**Template**:

```erlang
%% Test case 1: Normal case
test_normal_case() ->
    Input = #{document => "test.pdf", valid => true},
    {ok, Executor} = wf_yawl_executor:load_workflow_from_yaml("workflow.yaml"),
    {ok, Pid, _} = wf_yawl_executor:start_workflow(Executor, Input),
    {ok, Receipts} = wf_yawl_executor:execute_step(Pid, 100),
    %% TODO: Add assertions
    ok.

%% Test case 2: Error case
test_error_case() ->
    Input = #{document => "invalid.pdf", valid => false},
    %% TODO: Implement test
    ok.

%% Test case 3: Edge case
test_edge_case() ->
    %% TODO: Implement test
    ok.

%% Test case 4: Concurrent execution
test_concurrent() ->
    %% TODO: Implement test
    ok.
```

**Time**: 15 minutes

### Instructor Notes

**Timing Guidance**:
- 0-10 min: Verification concepts overview
- 10-25 min: Exercise 6.1 (Visual verification)
- 25-45 min: Exercise 6.2 (State space exploration)
- 45-60 min: Exercise 6.3 & 6.4 (Property checking + testing)

**Teaching Tips**:
1. Use diagrams to illustrate state spaces
2. Show counterexamples for failed properties
3. Demonstrate with a buggy workflow
4. Emphasize that verification finds bugs early

**Common Verification Issues**:
1. **State explosion** - Too many states to explore
2. **Incomplete specifications** - Missing edge cases
3. **Unreachable code** - Dead tasks in workflow
4. **Liveness violations** - Workflow can hang

### Deliverables

By the end of this session, participants should have:
1. Visual verification of their workflow
2. State space analysis completed
3. Property checks defined and executed
4. Test cases for their workflow

---

## Appendix A: Pattern Reference Card

### Quick Reference for All 43 Patterns

#### Basic Control Flow (P1-P8)

| ID | Pattern | Description | Key Parameters |
|----|---------|-------------|----------------|
| P1 | Sequence | Sequential task execution | from, to |
| P2 | Parallel Split | AND-split into N branches | branches |
| P3 | Synchronization | AND-join N branches | waits_for |
| P4 | Exclusive Choice | XOR-split based on data | choices, at |
| P5 | Simple Merge | XOR-merge without wait | froms, to |
| P6 | Multiple Choice | OR-split structured | choices, at |
| P7 | Structured Sync Merge | OR-join structured | froms, join |
| P8 | Multiple Merge | OR-join without wait | froms, to |

#### Advanced Routing (P9-P18)

| ID | Pattern | Description | Key Parameters |
|----|---------|-------------|----------------|
| P9 | Discriminator | First completion triggers | race, winner_to |
| P10 | Arbitrary Cycles | Unstructured loops | nodes |
| P11 | Implicit Termination | Auto-end when quiescent | - |
| P12 | MI: No Sync | Parallel without sync | instances |
| P13 | MI: Design Time | Fixed parallel instances | m, n |
| P14 | MI: Runtime Known | Dynamic known count | threshold |
| P15 | MI: Runtime Unknown | Dynamic unknown count | instances_expr |
| P16 | Deferred Choice | Runtime event choice | choices |
| P17 | Interleaved Routing | Non-deterministic parallel | tasks |
| P18 | Milestone | State-based execution | gate_task |

#### Cancellation (P19-P27)

| ID | Pattern | Description | Key Parameters |
|----|---------|-------------|----------------|
| P19 | Cancel Activity | Cancel single task | target |
| P20 | Cancel Case | Cancel entire workflow | cancel_event |
| P21 | Structured Loop | Loop with exit condition | entry, exit_condition |
| P22 | Recursion | Subnet calls itself | call |
| P23 | Transient Trigger | Event enables task | enabled_only_in |
| P24 | Persistent Trigger | Event consumed by task | consumed_in |
| P25 | Cancel Region | Cancel scoped tasks | region, cancel_event |
| P26 | Cancel MI Activity | Cancel multi-instance | mi_task |
| P27 | Complete MI Activity | Force MI completion | mi_task |

#### Advanced Synchronization (P28-P39)

| ID | Pattern | Description | Key Parameters |
|----|---------|-------------|----------------|
| P28 | Blocking Discriminator | Blocking first completion | blocks_until |
| P29 | Cancelling Discriminator | Cancel on first completion | race, cancel_rest |
| P30 | Structured Partial Join | N of M completion | threshold |
| P31 | Blocking Partial Join | Blocking partial completion | partial_out |
| P32 | Cancelling Partial Join | Cancel on partial | threshold, cancel_rest |
| P33 | Generalized AND Join | N of M with counting | froms, join |
| P34 | Static Partial Join MI | Partial MI join (static) | threshold |
| P35 | Cancelling Partial Join MI | Cancel partial MI | threshold |
| P36 | Dynamic Partial Join MI | Partial MI join (dynamic) | threshold |
| P37 | Local Sync Merge | Local OR-join | join |
| P38 | General Sync Merge | Generalized OR-join | froms, join |
| P39 | Critical Section | Mutual exclusion | mutex, protected |

#### Additional Patterns (P40-P43)

| ID | Pattern | Description | Key Parameters |
|----|---------|-------------|----------------|
| P40 | Interleaved Routing | Same as P17 | tasks |
| P41 | Thread Merge | Merge thread results | merge_task |
| P42 | Thread Split | Split into threads | branches |
| P43 | Explicit Termination | Explicit end event | terminator |

### Pattern Decision Guide

```
Need sequential execution?
  YES -> P1 (Sequence)

Need parallel execution?
  YES -> Must ALL complete?
    YES -> P2+P3 (Parallel Split + Sync)
    NO -> First completion? -> P9 (Discriminator)
         N of M? -> P30 (Structured Partial Join)
         Multi-instance? -> P12-P15 (MI patterns)

Need conditional routing?
  YES -> Data-driven? -> P4 (Exclusive Choice)
        Event-driven? -> P16 (Deferred Choice)
        Multiple paths? -> P6 (Multiple Choice)

Need loops?
  YES -> Counted? -> P12-P15 (MI patterns)
        Condition-based? -> P21 (Structured Loop)
        Recursive? -> P22 (Recursion)

Need cancellation?
  YES -> Single task? -> P19 (Cancel Activity)
        Entire workflow? -> P20 (Cancel Case)
        Scoped tasks? -> P25 (Cancel Region)
        Multi-instance? -> P26/P27 (Cancel/Complete MI)
```

---

## Appendix B: Common Pitfalls

### YAML Authoring Pitfalls

| Pitfall | Symptom | Fix |
|---------|---------|-----|
| Incorrect indentation | Parse errors | Use spaces, not tabs; align carefully |
| Missing pattern_registry | Compilation fails | Add pattern to pattern_registry |
| Invalid pattern ID | Pattern not found | Use correct ID (P1_Sequence, not sequence) |
| Missing rootNet | Cannot start workflow | Set rootNet to net ID |
| Mismatched flow IDs | Compilation error | Verify from/to reference existing nodes |

### Compilation Pitfalls

| Pitfall | Symptom | Fix |
|---------|---------|-----|
| Module already loaded | Compilation warning | Use code:purge/1 or restart shell |
| Pattern not found | {error, pattern_not_in_registry} | Add pattern_instance and registry entry |
| Missing places | enum_mode badmatch | Check pattern expansion parameters |
| Transition naming conflicts | Wrong transition fires | Use split_task/join_task parameters |

### Execution Pitfalls

| Pitfall | Symptom | Fix |
|---------|---------|-----|
| Workflow hangs | No transitions fire | Inject token for human tasks |
| Aborted immediately | No initial marking | Check init_marking/2 |
| Wrong task fires | Transition name collision | Use split_task parameter |
| Cannot find subnet | Module not loaded | Call ensure_modules_loaded/1 |

### Verification Pitfalls

| Pitfall | Symptom | Fix |
|---------|---------|-----|
| Deadlock detected | No transitions enabled | Check preset/postset connections |
| Unreachable task | Task never fires | Verify flows connect properly |
| State explosion | Verification too slow | Simplify workflow or use abstraction |
| Liveness violation | Workflow never ends | Check for cycles without exit |

---

## Additional Resources

### CRE Documentation

- `/Users/sac/cre/docs/YAWL_PATTERN_REFERENCE.md` - Complete pattern reference
- `/Users/sac/cre/docs/YAWL_PATTERNS_WORKBOOK.md` - Pattern exercises
- `/Users/sac/cre/src/wf/wf_yaml_spec.erl` - YAML parser source
- `/Users/sac/cre/src/core/yawl_compile.erl` - Compiler source

### Example Workflows

- `/Users/sac/cre/test/fixtures/orderfulfillment_2_1.yawl` - Order fulfillment example
- `/Users/sac/cre/test/omega_demo_runner.erl` - Symposium demo

### External References

- van der Aalst, W.M.P. et al. (2003). "Workflow Patterns"
- YAWL System: https://www.yawlfoundation.org/
- Petri Nets: https://petri nets/

---

## Instructor Notes Summary

### Timing Breakdown (5.5 hours total)

| Session | Theory | Exercises | Q&A |
|---------|--------|-----------|-----|
| Session 1 | 15 min | 10 min | 5 min |
| Session 2 | 10 min | 35 min | 15 min |
| Session 3 | 10 min | 35 min | 15 min |
| Session 4 | 10 min | 40 min | 10 min |
| Session 5 | 5 min | 45 min | 10 min |
| Session 6 | 10 min | 40 min | 10 min |

### Prerequisites Checklist

Before the workshop, ensure:
- [ ] Erlang/OTP 25+ installed on all machines
- [ ] CRE repository cloned and compiled
- [ ] rebar3 available in PATH
- [ ] YAML files accessible to participants
- [ ] Whiteboard or flipchart available

### Teaching Tips

1. **Start Simple**: Begin with sequence pattern, build complexity
2. **Use Examples**: Real-world workflows resonate more than abstract examples
3. **Hands-On**: Minimize lecture, maximize practice
4. **Peer Learning**: Encourage participants to help each other
5. **Visual Aids**: Draw workflows, show token flow diagrams

### Assessment

Participants demonstrate mastery by:
1. Writing a complete BCD
2. Selecting appropriate patterns
3. Writing valid YAML specifications
4. Compiling and executing workflows
5. Verifying workflow correctness

---

**Document Version**: 1.0.0
**Last Updated**: 2025-02-07
**For**: Generative Analysis Workshop
**Instructor**: [Your Name]
**Contact**: [Your Email]
