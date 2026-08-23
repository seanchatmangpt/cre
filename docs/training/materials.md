# Generative Analysis for Constructive Systems
## Training Course

**Version:** 1.0
**Last Updated:** 2026-02-07
**CRE Version:** v0.3.0

---

## Course Overview

This comprehensive training course teaches **Generative Analysis** - a systematic methodology for designing, implementing, and verifying constructive systems using the Common Runtime Environment (CRE) and YAWL (Yet Another Workflow Language) workflow patterns.

### What You Will Learn

- How to apply Generative Analysis to system design
- Writing Business Context Documents (BCDs)
- Using the 43 workflow patterns catalog
- Compiling YAML specifications to executable code
- Proving systems correct through formal verification
- Advanced topics: swarm coordination and exception handling
- Real-world case studies

### Target Audience

- System architects and designers
- Workflow engineers
- Erlang/OTP developers
- Business process analysts
- Verification engineers

### Prerequisites

- Basic understanding of Erlang/OTP
- Familiarity with workflow concepts
- Knowledge of YAML syntax
- Understanding of Petri nets (helpful but not required)

### Total Duration

- **Instructor-led:** 40 hours (5 days)
- **Self-paced:** 60-80 hours

---

# Module 1: Introduction to Generative Analysis

## Learning Objectives

By the end of this module, you will be able to:
- Define Generative Analysis and its role in constructive systems
- Explain the relationship between YAWL, Petri nets, and CRE
- Navigate the CRE toolchain and architecture
- Understand the Joe Armstrong design philosophy

## Duration

- **Instructor-led:** 4 hours
- **Self-paced:** 6 hours

## Prerequisites

- None (beginner module)

## Key Concepts

### 1.1 What is Generative Analysis?

**Generative Analysis** is a methodology for systematically transforming declarative specifications into verified, executable systems. It emphasizes:

- **Declarative specifications** - Describe WHAT, not HOW
- **Formal verification** - Prove correctness before execution
- **Pattern composition** - Build from verified primitives
- **Executable artifacts** - Generate code automatically

### 1.2 The CRE Architecture

The Common Runtime Environment follows Joe Armstrong's design principle:

```
┌─────────────────────────────────────────────────────────────┐
│                     CRE Architecture                         │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────────┐         ┌──────────────────┐         │
│  │  gen_yawl        │◄────────┤  gen_pnet        │         │
│  │  (YAWL Wrapper)  │         │  (Core Runtime)  │         │
│  └──────────────────┘         └──────────────────┘         │
│           ▲                            ▲                     │
│           │                            │                     │
│  ┌────────┴────────┐         ┌────────┴────────┐          │
│  │ Pattern Modules │         │ Helper Modules  │          │
│  │ (43 patterns)   │         │ (Pure, stateless)│         │
│  └─────────────────┘         └─────────────────┘          │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

**Key Design Principle:** One real OTP runner (`gen_pnet`), everything else is pure helpers/utilities.

### 1.3 YAWL and Petri Nets

**YAWL** (Yet Another Workflow Language) provides:
- 43 workflow control patterns
- Formal semantics based on Petri nets
- XML and YAML specification formats

**Petri Nets** provide:
- Mathematical foundation for workflow execution
- Places (states) and transitions (actions)
- Token-based execution model
- Formal verification capabilities

### 1.4 The Compilation Pipeline

```
YAML Specification (0.2)
         │
         ▼
   Parse (wf_yaml_spec)
         │
         ▼
   Expand Patterns (yawl_pattern_expander)
         │
         ▼
   Generate Code (yawl_compile)
         │
         ▼
   gen_pnet Module
         │
         ▼
   Execute Workflow
```

## Hands-on Exercises

### Exercise 1.1: Set Up Your Development Environment

```bash
# Clone the CRE repository
git clone <repository-url>
cd cre

# Compile the project
rebar3 compile

# Run the shell
rebar3 shell

# Verify installation
> yawl_pattern_registry:all_patterns().
% Should return list of 43 patterns
```

### Exercise 1.2: Explore a Simple Workflow

```erlang
% Load the order fulfillment example
> {ok, Spec} = wf_yaml_spec:from_yaml_file("examples/order_fulfillment.yaml").

% Examine the nets
> wf_yaml_spec:nets(Spec).

% Examine pattern instances
> wf_yaml_spec:pattern_instances(Spec).

% Compile the specification
> {ok, Compiled} = yawl_compile:compile(Spec, #{}).
```

### Exercise 1.3: Visualize Petri Net Structure

```erlang
% Generate DOT graph for a pattern
> {ok, Dot} = yawl_pattern_expander:to_dot(sequence).

% Save and visualize
> file:write_file("sequence.dot", Dot).
% Use Graphviz or online viewer to visualize
```

## Assessment Questions

1. **What is the key design principle of CRE?**
   - a) Multiple OTP runtimes for different patterns
   - b) One real OTP runner, pure helpers for everything else
   - c) Each pattern has its own gen_server
   - d) No runtime, pure interpretation only

2. **What does YAWL stand for?**
   - a) Yet Another Workflow Language
   - b) YAML Workflow Language
   - c) Your Automated Workflow Logic
   - d) Young Architect's Workflow Library

3. **How many workflow patterns are implemented in CRE?**
   - a) 7
   - b) 21
   - c) 43
   - d) 52

4. **What is a Petri net place?**
   - a) An action that transforms state
   - b) A location that holds tokens representing state
   - c) A connection between states
   - d) A workflow pattern

5. **What is the output of yawl_compile:compile/2?**
   - a) An executable binary
   - b) A gen_pnet module source code
   - c) A YAML file
   - d) A Petri net diagram

## Further Reading

- `docs/ARCHITECTURE.md` - Complete architecture overview
- `docs/JOE_ARMSTRONG_DESIGN_COMPLIANCE.md` - Design philosophy
- `docs/QUICK_START.md` - Getting started guide

---

# Module 2: Business Context Documents

## Learning Objectives

By the end of this module, you will be able to:
- Write effective Business Context Documents (BCDs)
- Translate business requirements into workflow specifications
- Define roles, variables, and constraints
- Structure YAML specifications properly

## Duration

- **Instructor-led:** 5 hours
- **Self-paced:** 8 hours

## Prerequisites

- Module 1: Introduction

## Key Concepts

### 2.1 What is a Business Context Document?

A **BCD** is a structured document that captures:
- Business objectives and constraints
- Stakeholder roles and responsibilities
- Data variables and their types
- Workflow structure and patterns
- Exception handling requirements

### 2.2 YAML Specification Structure

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "workflow_identifier"
  metaData:
    title: "Workflow Title"
    version: "0.2"
    creator: "Author Name"
    persistent: false

  rootNet: NetName

  roles:
    - Role1
    - Role2

  nets:
    - id: NetName
      type: NetFacts
      variables:
        - {name: var_name, type: string, initial: "value"}
      nodes:
        - {id: NodeId, kind: task|condition|inputCondition|outputCondition}
      flows:
        - {from: Source, to: Target}
      regions:
        - {id: RegionId, cancel_region: true}

  pattern_instances:
    - {id: InstanceId, pattern: PatternId, net: NetName, ...}
```

### 2.3 Node Types

| Kind | Description | Example |
|------|-------------|---------|
| `inputCondition` | Entry point to a net | Start |
| `outputCondition` | Exit point from a net | End |
| `task` | Work unit (automated/human/service) | ProcessOrder |
| `condition` | State/place in workflow | WaitingForApproval |

### 2.4 Task Types

| Type | Description | Examples |
|------|-------------|----------|
| `automated` | System-executed task | Calculations, data transformations |
| `human` | Requires human intervention | Approval, review |
| `service` | External service call | Payment processing, email |
| `decomposition` | Calls another net | Subprocess invocation |

## Hands-on Exercises

### Exercise 2.1: Write a Simple BCD

Create a BCD for a document approval workflow:

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "doc_approval"
  metaData:
    title: "Document Approval Workflow"
    version: "1.0"
    creator: "Your Name"

  rootNet: DocumentApproval

  roles:
    - Author
    - Reviewer
    - Approver

  nets:
    - id: DocumentApproval
      type: NetFacts
      variables:
        - {name: doc_status, type: string, initial: "draft"}
        - {name: approval_count, type: long, initial: 0}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: End, kind: outputCondition}
        - {id: Submit, kind: task, taskType: human}
        - {id: Review, kind: task, taskType: human}
        - {id: Approve, kind: task, taskType: human}
        - {id: Publish, kind: task, taskType: automated}

      flows:
        - {from: Start, to: Submit}
        - {from: Submit, to: Review}
        - {from: Review, to: Approve}
        - {from: Approve, to: Publish}
        - {from: Publish, to: End}

  pattern_instances:
    - {id: P1_main_sequence, pattern: P1_Sequence, net: DocumentApproval,
       from: Submit, to: Publish}
```

### Exercise 2.2: Add Branching Logic

Extend the workflow with conditional approval:

```yaml
  nodes:
    # ... existing nodes ...
    - {id: Reject, kind: task, taskType: service}

  flows:
    # ... existing flows ...
    - {from: Review, to: Reject}

  pattern_instances:
    # ... existing patterns ...
    - {id: P4_approval_choice, pattern: P4_ExclusiveChoice,
       net: DocumentApproval, at: Review,
       choices: [approve, reject, revise]}
```

### Exercise 2.3: Define Variables with Types

```yaml
  nets:
    - id: DocumentApproval
      type: NetFacts
      variables:
        # String variable
        - {name: doc_title, type: string, initial: ""}

        # Integer with constraints
        - {name: max_revisions, type: long,
           min: 1, max: 10, initial: 3}

        # Boolean
        - {name: requires_legal, type: boolean, initial: false}

        # Enumerated type
        - {name: priority, type: string,
           enum: ["low", "medium", "high", "urgent"],
           initial: "medium"}

        # Complex/nested (use string for YAML)
        - {name: metadata, type: string, initial: "{}"}
```

### Exercise 2.4: Validate Your BCD

```erlang
% Parse and validate your BCD
> {ok, Spec} = wf_yaml_spec:from_yaml_file("doc_approval.yaml").

% Check for errors
> case wf_yaml_spec:validate(Spec) of
    ok -> io:format("Valid!~n");
    {error, Errors} -> io:format("Errors: ~p~n", [Errors])
  end.

% View the structure
> io:format("~p~n", [wf_yaml_spec:to_map(Spec)]).
```

## Assessment Questions

1. **What is the purpose of a Business Context Document?**
   - a) To document code implementation
   - b) To capture business requirements for workflow specification
   - c) To store runtime state
   - d) To configure the Erlang VM

2. **Which YAML version does CRE use for YAWL specifications?**
   - a) 0.1
   - b) 0.2
   - c) 1.0
   - d) 2.1

3. **What is the difference between a human task and a service task?**
   - a) No difference
   - b) Human tasks require user interaction; service tasks call external systems
   - c) Service tasks are faster
   - d) Human tasks are automated

4. **How do you define a variable with constraints?**
   - a) Using the `constraint` key
   - b) Using `min`/`max`/`enum` keys
   - c) Using Erlang syntax
   - d) Constraints are not supported

5. **What is the rootNet used for?**
   - a) To define the root filesystem
   - b) To specify the entry point net for the workflow
   - c) To define administrator roles
   - d) To store database credentials

## Further Reading

- `docs/YAWL_COMPILE_COMPLETE_GUIDE.md` - Complete compilation guide
- `test/fixtures/agi_symposium_omega.yaml` - Full example specification
- `docs/SCHEMA_VALIDATION_INTEGRATION.md` - Schema validation details

---

# Module 3: The 43 Workflow Patterns Catalog

## Learning Objectives

By the end of this module, you will be able to:
- Identify and use all 43 YAWL workflow control patterns
- Select appropriate patterns for business requirements
- Compose patterns into complex workflows
- Understand pattern semantics and Petri net structures

## Duration

- **Instructor-led:** 8 hours
- **Self-paced:** 12 hours

## Prerequisites

- Module 1: Introduction
- Module 2: Business Context Documents

## Key Concepts

### 3.1 Pattern Categories

| Category | Patterns | Description |
|----------|----------|-------------|
| **Basic Control Flow** | P1-P10 | Fundamental workflow structures |
| **Multiple Instance** | P11-P17 | Concurrent and parallel execution |
| **State-Based** | P18-P20 | State-dependent behavior |
| **Advanced Control Flow** | P21-P28 | Complex flow control |
| **Triggers** | P23-P24 | Event-driven patterns |
| **Cancellation** | P19, P25-P27 | Cancellation and regions |
| **Partial Joins** | P28-P36 | Advanced synchronization |
| **Thread Patterns** | P37-P42 | Thread management |
| **Termination** | P11, P43 | Termination conditions |

### 3.2 Quick Reference Matrix

| Scenario | Pattern | ID |
|----------|---------|-----|
| Linear process | Sequence | P1 |
| Parallel execution | Parallel Split | P2 |
| Wait for all branches | Synchronization | P3 |
| Choose one path | Exclusive Choice | P4 |
| Merge one path | Simple Merge | P5 |
| Choose multiple paths | Multiple Choice | P6 |
| Merge multiple paths | Structured Synchronization | P7 |
| First-completed wins | Discriminator | P9 |
| Loop with condition | Structured Loop | P21 |
| Recursive call | Recursion | P22 |
| Event trigger | Persistent Trigger | P24 |
| Cancel activity | Cancel Activity | P19 |
| Cancel entire case | Cancel Case | P20 |
| Cancel region | Cancel Region | P25 |
| Split into threads | Thread Split | P42 |
| Merge threads | Thread Merge | P41 |
| Explicit termination | Explicit Termination | P43 |

### 3.3 Pattern Semantics

#### Sequence (P1)
```
p_start ──► t_task1 ──► p_task1 ──► t_task2 ──► p_task2 ──► p_end
```
Execute tasks in strict order.

#### Parallel Split (P2)
```
            ┌───► p_branch1
p_start ───►┼───► p_branch2
            └───► p_branch3
```
Create multiple concurrent branches.

#### Synchronization (P3)
```
p_branch1 ──►┐
p_branch2 ───►┴──► t_sync ──► p_merged
```
Wait for all branches to complete.

#### Exclusive Choice (P4)
```
            ┌───► p_branch1 (condition A)
p_start ────►┼───► p_branch2 (condition B)
            └───► p_branch3 (condition C)
```
Select exactly one branch based on conditions.

#### Discriminator (P9)
```
p_branch1 ──►┐
p_branch2 ────► t_discrim ──► p_selected
p_branch3 ───►┘               p_discarded
```
Accept first completion, discard others.

## Hands-on Exercises

### Exercise 3.1: Basic Patterns

Create a workflow using P1 (Sequence), P2 (Parallel Split), and P3 (Synchronization):

```yaml
pattern_instances:
  # Split processing into parallel lanes
  - {id: P2_parallel_split, pattern: P2_ParallelSplit,
     net: MainProcess, split_task: SplitProcessing,
     branches: [LaneA, LaneB, LaneC]}

  # Wait for all lanes to complete
  - {id: P3_sync, pattern: P3_Synchronization,
     net: MainProcess, join_task: MergeResults,
     waits_for: [LaneA, LaneB, LaneC]}

  # Sequence within each lane
  - {id: P1_lane_a, pattern: P1_Sequence,
     net: LaneA, from: StepA1, to: StepA2}
```

### Exercise 3.2: Choice Patterns

Implement conditional branching with P4 (Exclusive Choice):

```yaml
pattern_instances:
  - {id: P4_routing, pattern: P4_ExclusiveChoice,
     net: OrderProcessing, at: RouteOrder,
     choices: [standard, expedited, international]}

  # Simple merge after each choice
  - {id: P5_merge, pattern: P5_SimpleMerge,
     net: OrderProcessing, merge_task: ContinueProcessing}
```

### Exercise 3.3: Multiple Instance Patterns

Use P13 (Multiple Instances with Design Time Knowledge):

```yaml
pattern_instances:
  - {id: P13_parallel_review, pattern: P13_MultipleInstancesWithDesignTimeKnowledge,
     net: PaperReview, mi_task: ReviewPaper,
     instance_count: 3,  # Three reviewers in parallel
     splitting_mode: parallel,
     join_mode: and_join}
```

### Exercise 3.4: Loop Pattern

Implement a retry loop with P21 (Structured Loop):

```yaml
pattern_instances:
  - {id: P21_retry_loop, pattern: P21_StructuredLoop,
     net: PaymentProcessing, entry: AttemptPayment,
     body: [ValidateCard, ProcessCharge],
     exit_condition: "success||max_retries_exceeded"}
```

### Exercise 3.5: Cancellation Pattern

Define a cancellation region with P25 (Cancel Region):

```yaml
nets:
  - id: RiskyOperation
    regions:
      - {id: HighRiskZone, cancel_region: true,
         description: "Can be cancelled independently"}

pattern_instances:
  - {id: P25_cancel_region, pattern: P25_CancelRegion,
     net: RiskyOperation, region: HighRiskZone,
     trigger_task: EmergencyStop}
```

### Exercise 3.6: Thread Patterns

Use P42 (Thread Split) and P41 (Thread Merge):

```yaml
pattern_instances:
  # Split into concurrent threads
  - {id: P42_thread_split, pattern: P42_ThreadSplit,
     net: Symposium, split_task: SplitMegaThreads,
     branches: [ProgramThread, OpsThread, CommsThread]}

  # Merge threads back together
  - {id: P41_thread_merge, pattern: P41_ThreadMerge,
     net: Symposium, merge_task: MergeMegaThreads}
```

### Exercise 3.7: Explore All Patterns

```erlang
% List all available patterns
> AllPatterns = yawl_pattern_registry:all_patterns().
> lists:foreach(fun(P) -> io:format("~s~n", [P]) end, AllPatterns).

% Get module for a pattern
> yawl_pattern_registry:pattern_module(<<"P1_Sequence">>).
sequence

% Get pattern documentation
> sequence:module_info(attributes).
% Look for 'moduledoc' attributes

% Visualize a pattern's Petri net
> {ok, Dot} = yawl_pattern_expander:pattern_to_dot(sequence).
> file:write_file("sequence.dot", Dot).
```

## Assessment Questions

1. **Which pattern is used to execute tasks in strict sequential order?**
   - a) P2 Parallel Split
   - b) P1 Sequence
   - c) P4 Exclusive Choice
   - d) P21 Structured Loop

2. **What is the difference between P2 (Parallel Split) and P4 (Exclusive Choice)?**
   - a) No difference
   - b) P2 executes all branches; P4 selects one branch
   - c) P4 executes all branches; P2 selects one branch
   - d) P2 is for loops; P4 is for choices

3. **Which pattern waits for the FIRST branch to complete?**
   - a) P3 Synchronization
   - b) P9 Discriminator
   - c) P7 Structured Synchronization
   - d) P5 Simple Merge

4. **How do you implement a retry loop?**
   - a) Use P1 Sequence with duplicate tasks
   - b) Use P21 Structured Loop with exit condition
   - c) Use P2 Parallel Split with many branches
   - d) Use P19 Cancel Activity

5. **What is the purpose of P42 Thread Split?**
   - a) To create parallel threads that run independently
   - b) To split a string into characters
   - c) To terminate threads
   - d) To cancel threads

## Further Reading

- `docs/YAWL_PATTERNS_REFERENCE.md` - Complete pattern reference
- `docs/43_PATTERNS_COMPLETE.md` - Implementation status
- `docs/CORE_YAWL_PATTERNS_GUIDE.md` - Core patterns guide
- `docs/patterns/ADVANCED_PATTERNS.md` - Advanced patterns

---

# Module 4: Compilation - From YAML to Executable Code

## Learning Objectives

By the end of this module, you will be able to:
- Understand the compilation pipeline from YAML to executable
- Use yawl_compile to generate gen_pnet modules
- Customize compilation options
- Debug compilation issues
- Optimize generated code

## Duration

- **Instructor-led:** 6 hours
- **Self-paced:** 10 hours

## Prerequisites

- Module 1: Introduction
- Module 2: Business Context Documents
- Module 3: Patterns

## Key Concepts

### 4.1 Compilation Pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│                     Compilation Pipeline                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  YAML Specification (0.2)                                       │
│         │                                                        │
│         ▼                                                        │
│  ┌─────────────────┐                                           │
│  │ wf_yaml_spec    │ Parse YAML, extract nets and patterns     │
│  └─────────────────┘                                           │
│         │                                                        │
│         ▼                                                        │
│  ┌─────────────────┐                                           │
│  │ pattern_expander│ Expand patterns to Petri net structures   │
│  └─────────────────┘                                           │
│         │                                                        │
│         ▼                                                        │
│  ┌─────────────────┐                                           │
│  │ yawl_compile    │ Generate gen_pnet module source code       │
│  └─────────────────┘                                           │
│         │                                                        │
│         ▼                                                        │
│  Erlang Module (.erl)                                           │
│         │                                                        │
│         ▼                                                        │
│  Compiled Beam (.beam)                                          │
│         │                                                        │
│         ▼                                                        │
│  gen_pnet Execution                                             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### 4.2 Compilation API

```erlang
% Main compilation function
-spec compile(Spec :: yawl_yaml_spec(), Options :: map()) ->
    {ok, compile_result()} | {error, Reason}.

% Compile to file
-spec compile_to_file(Spec :: yawl_yaml_spec(), OutputDir :: string(), Options :: map()) ->
    {ok, [file:filename()]} | {error, Reason}.
```

### 4.3 Compilation Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `seed` | integer | 0 | Random seed for deterministic choice |
| `module_prefix` | binary | <<"yawl_">> | Prefix for generated module names |
| `output_dir` | string | undefined | Directory for output files |
| `include_source` | boolean | true | Include source in modules |
| `gen_observer` | boolean | false | Generate observer callbacks |

### 4.4 Generated Module Structure

Each compiled module implements the `gen_pnet` behavior:

```erlang
-module(yawl_NetId).
-behaviour(gen_pnet).

%% Structure callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2,
         preset/1, is_enabled/3, fire/3]).

%% Interface callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2,
         trigger/3]).
```

## Hands-on Exercises

### Exercise 4.1: Basic Compilation

```erlang
% Parse YAML specification
> {ok, Spec} = wf_yaml_spec:from_yaml_file("my_workflow.yaml").

% Compile with default options
> {ok, Compiled} = yawl_compile:compile(Spec, #{}).

% Examine compilation result
> maps:get(spec_id, Compiled).
> maps:get(modules, Compiled).
> maps:get(places, Compiled).
> maps:get(transitions, Compiled).
```

### Exercise 4.2: Custom Compilation Options

```erlang
% Compile with custom options
> {ok, Compiled} = yawl_compile:compile(Spec, #{
    seed => 12345,                      % Deterministic behavior
    module_prefix => "workflow_",       % Custom prefix
    include_source => true,             % Include source
    gen_observer => true                % Enable observers
  }).
```

### Exercise 4.3: Compile to File

```erlang
% Compile and write to directory
> {ok, Files} = yawl_compile:compile_to_file(
    Spec,
    "_build/default/lib/cre/src/gen_net",
    #{}
  ).

% List generated files
> lists:foreach(fun(F) -> io:format("~s~n", [F]) end, Files).
```

### Exercise 4.4: Load and Execute Compiled Module

```erlang
% Compile and load dynamically
> {ok, Compiled} = yawl_compile:compile(Spec, #{}).
> #{modules := Modules} = Compiled.

% Get a module
> {_, ModuleCode} = lists:keyfind(<<"MyNet">>, 1, maps:to_list(Modules)).

% Load into code path
> {module, Module} = code:load_binary(my_net, "my_net.erl", ModuleCode).

% Start the workflow
> {ok, Pid} = gen_pnet:start_link(Module, #{}, []).

% Execute steps
> {ok, Receipt} = gen_pnet:step(Pid).
> {ok, Receipt2} = gen_pnet:step(Pid).

% Check marking
> {ok, Marking} = gen_pnet:marking(Pid).
```

### Exercise 4.5: Debug Compilation Issues

```erlang
% Enable debug logging
> application:set_env(cre, log_level, debug).

% Parse with detailed errors
> case wf_yaml_spec:from_yaml_file("workflow.yaml") of
    {ok, Spec} ->
        {ok, Compiled} = yawl_compile:compile(Spec, #{
            debug => true  % Enable debug mode
        });
    {error, Reason} ->
        io:format("Parse error: ~p~n", [Reason]),
        % Get detailed error information
        wf_yaml_spec:format_error(Reason)
  end.

% Validate pattern usage
> yawl_pattern_registry:validate_all_patterns(Spec).
```

### Exercise 4.6: Pattern Expansion Inspection

```erlang
% Get pattern instances
> PatternInstances = wf_yaml_spec:pattern_instances(Spec).

% Expand a specific pattern
> Instance = lists:keyfind(<<"P1_Sequence">>, 2, PatternInstances).
> Expanded = yawl_pattern_expander:expand_pattern(Instance, #{}).

% Inspect expanded structure
> maps:get(places, Expanded).
> maps:get(transitions, Expanded).
> maps:get(flows, Expanded).
> maps:get(preset, Expanded).
> maps:get(postset, Expanded).
```

## Assessment Questions

1. **What is the first step in the compilation pipeline?**
   - a) Pattern expansion
   - b) YAML parsing with wf_yaml_spec
   - c) Code generation
   - d) Module loading

2. **Which option controls the random seed for nondeterministic choices?**
   - a) `random_seed`
   - b) `seed`
   - c) `deterministic`
   - d) `randomize`

3. **What behavior do compiled modules implement?**
   - a) gen_server
   - b) gen_fsm
   - c) gen_pnet
   - d) gen_statem

4. **How do you enable observer callbacks in compiled modules?**
   - a) Set `gen_observer => true` in options
   - b) Set `observer => true` in options
   - c) Observers are always enabled
   - d) Set `include_observers => true` in options

5. **What function compiles and writes modules to files?**
   - a) yawl_compile:compile/2
   - b) yawl_compile:compile_to_file/3
   - c) yawl_compile:write/2
   - d) yawl_compile:generate/3

## Further Reading

- `docs/YAWL_COMPILE_COMPLETE_GUIDE.md` - Complete compilation guide
- `src/core/yawl_compile.erl` - Compiler source code
- `src/core/yawl_pattern_expander.erl` - Pattern expander source

---

# Module 5: Verification - Proving Systems Correct

## Learning Objectives

By the end of this module, you will be able to:
- Understand formal verification in the context of workflow systems
- Use static analysis tools (Dialyzer, Xref)
- Write property-based tests
- Apply model checking techniques
- Verify workflow correctness properties

## Duration

- **Instructor-led:** 6 hours
- **Self-paced:** 10 hours

## Prerequisites

- Module 1: Introduction
- Module 2: Business Context Documents
- Module 4: Compilation

## Key Concepts

### 5.1 Verification Levels

```
┌─────────────────────────────────────────────────────────────┐
│                  Verification Pyramid                        │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│                    ┌─────────────┐                          │
│                   / Formal Proof \ (Theorem proving)        │
│                  └───────────────┘                          │
│                 ┌─────────────────┐                         │
│                / Model Checking   \ (State space exploration)│
│               └───────────────────┘                         │
│              ┌─────────────────────┐                        │
│             / Property-Based Tests \ (QuickCheck, PropEr)   │
│            └───────────────────────┘                        │
│           ┌─────────────────────────┐                       │
│          /    Static Analysis       \ (Dialyzer, Xref)       │
│         └───────────────────────────┘                       │
│       ┌───────────────────────────────┐                    │
│      /       Unit & Integration Tests \ (EUnit, CT)         │
│     └─────────────────────────────────┘                     │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### 5.2 Static Analysis Tools

| Tool | Purpose | Command |
|------|---------|---------|
| Dialyzer | Successor type analysis | `rebar3 dialyzer` |
| Xref | Cross-reference checking | `rebar3 xref` |
| Cover | Test coverage | `rebar3 cover` |
| Lint | Code style | `rebar3 lint` |

### 5.3 Verification Properties

**Safety Properties** (nothing bad happens):
- No deadlock
- No live lock
- No token loss
- No invalid state transitions

**Liveness Properties** (something good eventually happens):
- Eventual completion
- No starvation
- Progress guarantees

**Functional Properties**:
- Expected outputs
- Invariant preservation
- Data consistency

## Hands-on Exercises

### Exercise 5.1: Static Analysis

```bash
# Run Dialyzer (type analysis)
rebar3 dialyzer

# Run Xref (cross-reference check)
rebar3 xref

# Run with strict options
rebar3 dialyzer -Wunmatched_returns -Werror_handling -Wrace_conditions
```

### Exercise 5.2: Property-Based Testing

```erlang
%% Test workflow correctness with PropEr
-module(workflow_properties).
-include_lib("proper/include/proper.hrl").

% Property: Sequence preserves order
prop_sequence_order() ->
    ?FORALL(Inputs, list(non_neg_integer()),
        begin
            {ok, Spec} = create_sequence_workflow(Inputs),
            {ok, Result} = execute_workflow(Spec),
            lists:reverse(Inputs) =:= Result
        end).

% Property: Parallel split completes all branches
prop_parallel_completes_all() ->
    ?FORALL(Branches, non_empty(list(integer(1, 5))),
        begin
            {ok, Spec} = create_parallel_workflow(Branches),
            {ok, Results} = execute_workflow(Spec),
            length(Results) =:= length(Branches)
        end).

% Property: Exclusive choice selects exactly one
prop_exclusive_one() ->
    ?FORALL(Branches, non_empty(list(atom())),
        begin
            {ok, Spec} = create_choice_workflow(Branches),
            {ok, Result} = execute_workflow(Spec),
            is atom(Result) andalso
            lists:member(Result, Branches)
        end).
```

### Exercise 5.3: Model Checking with Concuerror

```erlang
%% Test for race conditions in concurrent workflows
-module(workflow_concurrency_test).

% Concuerror test for deadlock freedom
-deadlock_free({ok, Pid}) ->
    gen_pnet:cast(Pid, step),
    gen_pnet:cast(Pid, step),
    timer:sleep(100),
    gen_pnet:stop(Pid).

% Run with: concuerror -m workflow_concurrency_test
```

```bash
# Install Concuerror
rebar3 get deps

# Run Concuerror on a module
concuerror -m yawl_executor_test -t test_
```

### Exercise 5.4: Invariant Verification

```erlang
-module(workflow_invariants).

% Verify no tokens are lost
prop_no_token_loss() ->
    ?FORALL(Spec, workflow_spec(),
        begin
            InitialTokens = count_all_tokens(Spec),
            {ok, FinalState} = execute_to_completion(Spec),
            FinalTokens = count_all_tokens(FinalState),
            % All tokens must be in output places or accounted for
            token_accounting_holds(InitialTokens, FinalTokens)
        end).

% Verify place invariants
prop_place_invariants() ->
    ?FORALL({Spec, Place}, {workflow_spec(), place()},
        begin
        {ok, States} = execute_all_steps(Spec),
        lists:all(fun(State) ->
            check_place_invariant(Place, State)
        end, States)
    end).
```

### Exercise 5.5: Data Flow Verification

```erlang
-module(dataflow_verification).

% Verify schema validation
-export([verify_schema/2]).

verify_schema(Data, Schema) ->
    case yawl_schema:validate(Data, Schema) of
        ok -> {true, Schema};
        {error, Errors} -> {false, Errors}
    end.

% Property: Valid data always passes validation
prop_valid_data_passes() ->
    ?FORALL({Type, Value}, valid_type_value_pair(),
        begin
            Schema = #{type => Type},
            {Result, _} = verify_schema(Value, Schema),
            Result =:= true
        end).
```

### Exercise 5.6: Coverage Analysis

```bash
# Run tests with coverage
rebar3 cover --verbose

# Generate coverage report
rebar3 cover --export coverage.html

# View coverage for specific module
rebar3 cover -m yawl_compile --verbose
```

```erlang
% Generate coverage report in code
cover:compile_beam_directory("ebin").
cover:start(),
{ok, _} = cover:compile_module("src/yawl_compile.erl").
% Run tests...
cover:analyse_to_file(yawl_compile, "yawl_compile_coverage.html").
```

## Assessment Questions

1. **What is the difference between safety and liveness properties?**
   - a) Safety ensures progress; liveness prevents bad states
   - b) Safety prevents bad states; liveness ensures progress
   - c) No difference
   - d) Safety is for types; liveness is for processes

2. **Which tool performs successor type analysis in Erlang?**
   - a) Xref
   - b) Dialyzer
   - c) Cover
   - d) Typer

3. **What does property-based testing verify?**
   - a) Specific examples
   - b) Properties that hold for all inputs
   - c) Code style
   - d) Performance

4. **What is Concuerror used for?**
   - a) Type checking
   - b) Model checking concurrent Erlang programs
   - c) Code coverage
   - d) Documentation generation

5. **How do you verify no tokens are lost in a workflow?**
   - a) Count tokens before and after execution
   - b) Use dialyzer
   - c) Check the YAML syntax
   - d) Token loss is not verifiable

## Further Reading

- `docs/VERIFICATION_REPORT.md` - Verification methodology
- `test/nato_deterrence_concuerror_tests.erl` - Model checking examples
- `docs/TESTING.md` - Testing strategies

---

# Module 6: Advanced Topics

## Learning Objectives

By the end of this module, you will be able to:
- Implement swarm coordination patterns
- Handle exceptions and compensation
- Use cancellation regions effectively
- Implement advanced data flow patterns
- Integrate with external systems

## Duration

- **Instructor-led:** 8 hours
- **Self-paced:** 14 hours

## Prerequisites

- Module 1: Introduction
- Module 3: Patterns
- Module 5: Verification

## Key Concepts

### 6.1 Swarm Coordination

Swarm coordination enables multiple agents to collaborate on workflow execution:

```
┌─────────────┐       ┌─────────────┐
│   Agent 1   │       │   Agent 2   │
│  (Chair)    │◄──────►│(ProgramChair)│
└──────┬──────┘       └──────┬──────┘
       │                     │
       └──────────┬──────────┘
                  │
         ┌────────▼────────┐
         │  Workflow Engine │
         │   (gen_yawl)     │
         └─────────────────┘
```

### 6.2 Exception Handling

Exception handling patterns:

| Pattern | Description | Use Case |
|---------|-------------|----------|
| WHP-01 Error Handler | Catch-all error handling | Unexpected failures |
| WHP-02 Retry | Automatic retry with backoff | Transient failures |
| WHP-03 Compensation | Undo completed activities | Sagas, transactions |
| WHP-04 Triggered Compensation | Conditional undo | Event-driven rollback |
| WHP-05 Consecutive Compensation | Multiple undo operations | Complex rollbacks |

### 6.3 Cancellation Regions

Cancellation regions allow scoped cancellation:

```
┌─────────────────────────────────────────┐
│         Main Workflow                    │
│  ┌─────────────────────────────────┐    │
│  │     Cancellation Region         │    │
│  │  ┌─────┐ ┌─────┐ ┌─────┐       │    │
│  │  │ T1  │ │ T2  │ │ T3  │       │    │
│  │  └─────┘ └─────┘ └─────┘       │    │
│  │                                  │    │
│  │  Cancel Event ──► Cancel All     │    │
│  └─────────────────────────────────┘    │
│                                          │
│  Other tasks continue...                 │
└─────────────────────────────────────────┘
```

### 6.4 Data Flow Patterns

| Pattern | Description | Example |
|---------|-------------|---------|
| WDP-01 Parameter Passing | Pass data between tasks | `param_pass(task1, task2)` |
| WDP-02 Data Transformation | Convert data formats | XML to JSON |
| WDP-03 Data Distribution | Send to multiple tasks | Broadcast |
| WDP-04 Data Accumulation | Collect from multiple | Reduce operation |
| WDP-05 Data Visibility | Control access scope | Local, branch, global |

## Hands-on Exercises

### Exercise 6.1: Implement a Compensation Workflow

```yaml
# Saga pattern with compensation
pattern_instances:
  # Activities with compensators
  - {id: saga_step1, pattern: WHP03_Compensation,
     net: OrderProcessing,
     activity: ReserveInventory,
     compensator: ReleaseInventory}

  - {id: saga_step2, pattern: WHP03_Compensation,
     net: OrderProcessing,
     activity: ProcessPayment,
     compensator: RefundPayment}

  - {id: saga_step3, pattern: WHP03_Compensation,
     net: OrderProcessing,
     activity: ShipOrder,
     compensator: CancelShipment}
```

```erlang
% Implement compensation handlers
-module(order_compensators).

compensate(reserve_inventory, _Context) ->
    %% Release inventory reservation
    inventory:release(get(reservation_id)).

compensate(process_payment, Context) ->
    %% Refund payment
    payment:refund(maps:get(transaction_id, Context)).

compensate(ship_order, Context) ->
    %% Cancel shipment
    shipping:cancel(maps:get(shipment_id, Context)).
```

### Exercise 6.2: Use Cancellation Regions

```yaml
nets:
  - id: HighRiskWorkflow
    regions:
      - {id: DemoZone, cancel_region: true,
         description: "High-risk demo activities"}

    nodes:
      - {id: StartDemo, kind: task, taskType: automated}
      - {id: RunDemo, kind: task, taskType: automated}
      - {id: EmergencyStop, kind: task, taskType: automated}

pattern_instances:
  - {id: cancel_demo_region, pattern: P25_CancelRegion,
     net: HighRiskWorkflow, region: DemoZone,
     trigger_task: EmergencyStop}
```

### Exercise 6.3: Implement Data Transformation

```yaml
# Data flow between tasks
nets:
  - id: DataPipeline
    nodes:
      - {id: Source, kind: task, taskType: service}
      - {id: Transform, kind: task, taskType: automated}
      - {id: Target, kind: task, taskType: service}

pattern_instances:
  - {id: data_transform, pattern: WDP02_DataTransformation,
     net: DataPipeline,
     input_task: Source,
     output_task: Transform,
     transform_fun: "convert_xml_to_json"}

  - {id: data_distribute, pattern: WDP03_DataDistribution,
     net: DataPipeline,
     source_task: Transform,
     recipients: [Target, Archive, Analytics],
     distribution_type: broadcast}
```

```erlang
% Implement transformation function
-module(data_transformers).

convert_xml_to_json(XmlData) ->
    %% Parse XML
    {Xml, _} = xmerl_scan:string(XmlData),
    %% Extract data
    Data = extract_xml_data(Xml),
    %% Convert to JSON
    jsx:encode(Data).

extract_xml_data(Xml) ->
    %% Implementation
    #{}.
```

### Exercise 6.4: Swarm Agent Communication

```erlang
-module(swarm_coordinator).

% Start multiple agents for different roles
start_agents(Roles) ->
    lists:map(fun(Role) ->
        {ok, Pid} = agent:start_link(Role),
        register(Role, Pid),
        {Role, Pid}
    end, Roles).

% Coordinate agents for a task
coordinate_task(TaskId, Agents) ->
    %% Send task to all agents
    lists:foreach(fun({Role, Pid}) ->
        gen_server:cast(Pid, {task, TaskId, Role})
    end, Agents),

    %% Wait for completion
    wait_for_completion(Agents).

% Implement consensus protocol
consensus(Agents, Decision) ->
    %% Collect votes from all agents
    Votes = collect_votes(Agents),

    %% Apply quorum rules
    Quorum = length(Agents) div 2 + 1,
    case count_yes(Votes) >= Quorum of
        true -> {agreed, Decision};
        false -> {rejected, Votes}
    end.
```

### Exercise 6.5: External System Integration

```erlang
-module(external_integrations).

% HTTP service task
execute_http_task(Url, Method, Payload) ->
    %% Make HTTP request
    Request = case Method of
        get -> {Url, []};
        post -> {Url, [], "application/json", Payload}
    end,
    httpc:request(Method, Request, [], []).

% Email notification
send_notification(Recipient, Subject, Body) ->
    %% Send email via SMTP
    gen_smtp_client:send(
        {"workflow@example.com", [Recipient]},
        Subject,
        Body
    ).

% Database transaction with compensation
db_transaction(Operation) ->
    try
        Result = perform_db_operation(Operation),
        {ok, Result}
    catch
        error:Reason ->
            %% Compensate by rolling back
            rollback_transaction(),
            {error, Reason}
    end.
```

## Assessment Questions

1. **What is a cancellation region?**
   - a) A region where all tasks are cancelled together
   - b) A way to cancel individual tasks
   - c) A type of workflow pattern
   - d) An error handling mechanism

2. **Which pattern implements undo operations?**
   - a) WHP-01 Error Handler
   - b) WHP-02 Retry
   - c) WHP-03 Compensation
   - d) WHP-04 Triggered Compensation

3. **What is the saga pattern?**
   - a) A way to tell stories
   - b) A compensation pattern for distributed transactions
   - c) A type of workflow
   - d) A testing framework

4. **Which data flow pattern broadcasts data to multiple recipients?**
   - a) WDP-01 Parameter Passing
   - b) WDP-02 Data Transformation
   - c) WDP-03 Data Distribution
   - d) WDP-04 Data Accumulation

5. **How do agents coordinate in a swarm?**
   - a) Through message passing and consensus
   - b) Shared memory
   - c) Direct function calls
   - d) Email notifications

## Further Reading

- `docs/reference/EXCEPTION_HANDLING.md` - Exception handling reference
- `docs/worklet_integration_summary.md` - Worklet and exception patterns
- `src/wf/wf_exception.erl` - Exception handling implementation

---

# Module 7: Case Studies

## Learning Objectives

By the end of this module, you will be able to:
- Analyze real-world workflow implementations
- Understand design trade-offs
- Apply patterns to complex scenarios
- Learn from production examples

## Duration

- **Instructor-led:** 6 hours
- **Self-paced:** 10 hours

## Prerequisites

- Module 1: Introduction
- Module 2: Business Context Documents
- Module 3: Patterns
- Module 6: Advanced Topics

## Case Study 1: Order Fulfillment

### Overview
A complete order fulfillment system with payment processing, inventory management, and shipping coordination.

### Key Patterns Used

| Pattern | Usage |
|---------|-------|
| P1 Sequence | Linear order processing |
| P2/P3 Parallel Split/Sync | Concurrent inventory and payment checks |
| P4 Exclusive Choice | Payment method selection |
| P21 Structured Loop | Retry failed payments |
| P19 Cancel Activity | Cancel individual order items |
| WHP-03 Compensation | Refund on payment failure |

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│              Order Fulfillment Workflow                  │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────┐    ┌──────────┐    ┌──────────┐         │
│  │ Ordering │───►│ Payment  │───►│ Shipping │         │
│  └──────────┘    └──────────┘    └──────────┘         │
│                         │                              │
│                         ▼                              │
│                   ┌──────────┐                         │
│                   │ Carrier  │                         │
│                   │Appointment│                        │
│                   └──────────┘                         │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Exercise 7.1: Analyze Order Fulfillment

```erlang
% Load the order fulfillment specification
> {ok, Spec} = wf_yaml_spec:from_yaml_file("examples/order_fulfillment.yaml").

% Identify all patterns used
> Patterns = wf_yaml_spec:pattern_instances(Spec).
> lists:foreach(fun(P) ->
    Id = maps:get(<<"id">>, P, maps:get(id, P)),
    Pattern = maps:get(<<"pattern">>, P, maps:get(pattern, P)),
    Net = maps:get(<<"net">>, P, maps:get(net, P)),
    io:format("~s on ~s: ~s~n", [Id, Net, Pattern])
  end, Patterns).

% Execute the workflow
> {ok, Executor} = yawl_executor:compile_workflow(Spec).
> {ok, Result} = yawl_executor:execute(Executor,
    #{order_id => "12345",
      items => [#{sku => "ABC123", quantity => 2}]},
    #{}
  ).
```

## Case Study 2: AGI Symposium Omega

### Overview
A complex multi-threaded conference management system coordinating program, operations, communications, and incident response.

### Key Patterns Used

| Pattern | Usage |
|---------|-------|
| P42 Thread Split | Split into program/ops/comms/incident threads |
| P41 Thread Merge | Consolidate threads before Go/No-Go |
| P2/P3 | Parallel operations and synchronization |
| P21 Structured Loop | Review and revision cycles |
| P25 Cancel Region | High-risk demo zone cancellation |
| P23/P24 Triggers | Event-driven incident response |

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  AGI Symposium Omega                        │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│         ┌─────────────────────────────────────┐             │
│         │           Symposium (Root)           │             │
│         └──────────┬─────────────────────────┬─┘             │
│                    │ P42 Thread Split        │               │
│      ┌─────────────┼─────────────┬───────────┤              │
│      ▼             ▼             ▼           ▼              │
│  ┌───────┐    ┌───────┐    ┌───────┐   ┌───────┐           │
│  │Program│    │  Ops  │    │ Comms │   │Incident│           │
│  └───┬───┘    └───┬───┘    └───┬───┘   └───┬───┘           │
│      │            │            │           │                │
│      └────────────┼────────────┴───────────┘                │
│                   │ P41 Thread Merge                        │
│                   ▼                                         │
│            ┌─────────────┐                                  │
│            │ Go/No-Go    │                                  │
│            │   Council   │                                  │
│            └─────────────┘                                  │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### Exercise 7.2: Analyze AGI Symposium

```erlang
% Load AGI Symposium specification
> {ok, Spec} = wf_yaml_spec:from_yaml_file(
    "test/fixtures/agi_symposium_omega.yaml"
  ).

% Count pattern usage by category
> Patterns = wf_yaml_spec:pattern_instances(Spec).
> Categorized = lists:foldl(fun(P, Acc) ->
    Pattern = maps:get(<<"pattern">>, P, maps:get(pattern, P)),
    Category = pattern_category(Pattern),
    Acc#{Category => maps:get(Category, Acc, 0) + 1}
  end, #{}, Patterns).

% Identify all thread splits
> Splits = [P || P <- Patterns,
    is_split_pattern(maps:get(<<"pattern">>, P, maps:get(pattern, P)))].

% Visualize thread structure
> {ok, Dot} = wf_yaml_spec:to_dot(Spec).
> file:write_file("agi_symposium.dot", Dot).
```

## Case Study 3: NATO Deterrence System

### Overview
A high-reliability system for coordinating deterrence operations with formal verification guarantees.

### Key Patterns Used

| Pattern | Usage |
|---------|-------|
| P18 Milestone | Phase progression guards |
| P25 Cancel Region | Scoped operation cancellation |
| P39 Critical Section | Mutual exclusion for shared resources |
| WHP-02 Retry | Reliable message delivery |
| WHP-03 Compensation | Rollback on abort |

### Verification Approach

```erlang
% Formal verification with Concuerror
-module(nato_deterrence_concuerror_tests).

% Verify no race conditions in escalation
prop_escalation_no_race() ->
    ?FORALL(Scenario, escalation_scenario(),
        begin
        {ok, Pid} = start_deterrence_workflow(Scenario),
        %% Concurrent escalation requests
        spawn(fun() -> gen_pnet:cast(Pid, escalate) end),
        spawn(fun() -> gen_pnet:cast(Pid, escalate) end),
        timer:sleep(100),
        {ok, State} = gen_pnet:marking(Pid),
        %% Verify only one escalation processed
        count_tokens(State, escalate) =< 1
    end).
```

## Assessment Questions

1. **Which pattern is used in AGI Symposium to split into concurrent threads?**
   - a) P2 Parallel Split
   - b) P42 Thread Split
   - c) P4 Exclusive Choice
   - d) P21 Structured Loop

2. **What is the purpose of the Go/No-Go Council in AGI Symposium?**
   - a) To approve catering
   - b) To synchronize and approve readiness before the event
   - c) To cancel the event
   - d) To manage incident response

3. **Which case study demonstrates the saga pattern?**
   - a) Order Fulfillment
   - b) AGI Symposium
   - c) NATO Deterrence
   - d) None of the above

4. **What verification technique is used for NATO Deterrence?**
   - a) Manual testing
   - b) Concuerror model checking
   - c) Fuzz testing
   - d) No verification

5. **Why does Order Fulfillment use P19 Cancel Activity?**
   - a) To cancel the entire order
   - b) To cancel individual items that can't be fulfilled
   - c) To cancel payment
   - d) To cancel shipping

## Further Reading

- `docs/ORDER_FULFILLMENT_GUIDE.md` - Order fulfillment details
- `docs/AGI_SYMPOSIUM_SIMULATION_COMPLETE.md` - AGI Symposium case study
- `test/nato_deterrence_concuerror_tests.erl` - NATO verification examples

---

# Module 8: Workshop - Hands-on Project

## Learning Objectives

By the end of this module, you will have:
- Designed and implemented a complete workflow system
- Applied all concepts from the course
- Created a verified, production-ready specification
- Documented your implementation

## Duration

- **Instructor-led:** 8 hours (2 days)
- **Self-paced:** 20 hours

## Prerequisites

- All previous modules (1-7)

## Workshop Project: Document Collaboration System

Build a complete document collaboration workflow system with the following requirements:

### 8.1 Project Requirements

**Functional Requirements:**

1. Document creation and editing
2. Review and approval workflows
3. Version control and branching
4. Collaboration with multiple reviewers
5. Conflict resolution
6. Publication and archiving

**Non-Functional Requirements:**

1. Handle 1000+ concurrent documents
2. Response time < 100ms for operations
3. Zero data loss
4. Recoverable from failures
5. Audit trail for all operations

### 8.2 Phase 1: Design (2 hours)

**Tasks:**

1. Create Business Context Document
2. Define roles and permissions
3. Identify required patterns
4. Design data structures
5. Document verification approach

**Deliverables:**

- BCD in YAML format
- Pattern selection document
- Data schema definition
- Verification test plan

### 8.3 Phase 2: Implementation (4 hours)

**Tasks:**

1. Write complete YAML specification
2. Define all pattern instances
3. Specify data flow
4. Add exception handling
5. Include cancellation regions

**Template:**

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "document_collaboration"
  metaData:
    title: "Document Collaboration System"
    version: "1.0"
    creator: "Your Name"

  rootNet: DocumentCollaboration

  roles:
    - Author
    - Editor
    - Reviewer
    - Approver
    - Publisher

  nets:
    - id: DocumentCollaboration
      type: NetFacts
      variables:
        # TODO: Define your variables here
      nodes:
        # TODO: Define nodes
      flows:
        # TODO: Define flows
      regions:
        # TODO: Define cancellation regions if needed

    # TODO: Add subnet definitions

  pattern_instances:
    # TODO: Add pattern instances
```

### 8.4 Phase 3: Verification (4 hours)

**Tasks:**

1. Compile specification
2. Run static analysis
3. Write property-based tests
4. Execute model checking
5. Generate coverage report

**Test Template:**

```erlang
-module(doc_collab_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Property: Document state invariant
prop_document_state_invariant() ->
    ?FORALL(Operations, list(doc_operation()),
        begin
            {ok, Spec} = load_spec(),
            {ok, FinalState} = execute_operations(Spec, Operations),
            check_state_invariant(FinalState)
        end).

%% Test: Concurrent review handling
concurrent_review_test() ->
    {ok, Spec} = load_spec(),
    {ok, Pid} = start_workflow(Spec, #{doc_id => "test123"}),
    % Simulate concurrent reviews
    spawn(fun() -> submit_review(Pid, "reviewer1", approve) end),
    spawn(fun() -> submit_review(Pid, "reviewer2", approve) end),
    timer:sleep(200),
    {ok, State} = gen_pnet:marking(Pid),
    ?assertNotEqual(undefined, get_doc_state(State)).

%% TODO: Add more tests
```

### 8.5 Phase 4: Documentation (2 hours)

**Tasks:**

1. Write system overview
2. Document pattern usage
3. Create API documentation
4. Write operator guide
5. Document deployment

**Documentation Template:**

```markdown
# Document Collaboration System

## Overview
[Describe your system]

## Architecture
[Include diagrams]

## Patterns Used
[List and justify each pattern]

## Data Model
[Document data structures]

## API Reference
[Document all operations]

## Deployment
[Deployment instructions]

## Verification
[Summary of verification results]
```

### 8.6 Project Evaluation

**Evaluation Criteria:**

| Criteria | Points |
|----------|--------|
| BCD completeness | 20 |
| Pattern selection appropriateness | 20 |
| Implementation correctness | 20 |
| Verification thoroughness | 20 |
| Documentation quality | 20 |

**Passing Score:** 80/100

## Workshop Exercises

### Exercise 8.1: Pattern Selection Exercise

Given these requirements, select appropriate patterns:

1. **Requirement:** "Documents must be reviewed by at least 2 of 3 assigned reviewers before proceeding."
   - Which pattern? _________________
   - Justification: ____________________

2. **Requirement:** "Any reviewer can reject the document, which should stop the approval process."
   - Which pattern? _________________
   - Justification: ____________________

3. **Requirement:** "The review cycle can repeat until approved or withdrawn."
   - Which pattern? _________________
   - Justification: ____________________

4. **Requirement:** "Document editing and review can happen in parallel."
   - Which pattern? _________________
   - Justification: ____________________

5. **Requirement:** "If publication fails, all previous changes must be rolled back."
   - Which pattern? _________________
   - Justification: ____________________

### Exercise 8.2: Debug a Broken Specification

Find and fix the errors in this specification:

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  uri: "broken_workflow"

  rootNet: Main

  nets:
    - id: Main
      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Task1, kind: task, taskType: automated}
        - {id: Task2, kind: task, taskType: automated}
        # Missing End node!

      flows:
        - {from: Start, to: Task1}
        - {from: Task1, to: Task2}
        # Missing flow to End!

  pattern_instances:
    # This pattern reference doesn't exist!
    - {id: bad_pattern, pattern: P99_InvalidPattern, net: Main}
```

### Exercise 8.3: Write a Verification Test

Write a PropEr test for this property:
"No two users can edit the same document section simultaneously."

```erlang
%% TODO: Implement this test
prop_no_concurrent_edit() ->
    ?FORALL(Users, list(user_id()),
        begin
        %% Your code here
        end).
```

## Final Assessment

### Multiple Choice Questions

1. **Which module is the core runtime in CRE?**
   - a) gen_yawl
   - b) gen_pnet
   - c) yawl_engine
   - d) wf_engine

2. **What is the purpose of a pattern instance in YAML?**
   - a) To define a new pattern
   - b) To use an existing pattern in a specific net
   - c) To document patterns
   - d) To compile patterns

3. **How do you verify a workflow is deadlock-free?**
   - a) Use dialyzer
   - b) Use model checking (Concuerror)
   - c) Check YAML syntax
   - d) Run once and see

4. **What is the compensation pattern used for?**
   - a) Performance improvement
   - b) Rolling back completed activities
   - c) Parallel execution
   - d) Data validation

5. **Which tool compiles YAML to Erlang code?**
   - a) rebar3
   - b) yawl_compile
   - c) wf_yaml_spec
   - d) gen_pnet

### Practical Exercise

**Task:** Create a simple approval workflow with:
1. Document submission
2. 3-parallel reviewers (need 2 approvals)
3. Approve or reject decision
4. Notification of result

**Submit:** Complete YAML specification with verification tests.

## Further Resources

### Code Examples

- `examples/` - Complete workflow examples
- `test/` - Test suites and examples
- `src/patterns/` - Pattern implementations

### Documentation

- `docs/INDEX.md` - Documentation index
- `docs/TUTORIALS_INDEX.md` - Tutorial collection
- `docs/QUICK_REFERENCE_CHEATSHEET.md` - Quick reference

### Community

- CRE GitHub Repository
- Workflow Patterns Initiative (workflowpatterns.com)
- YAWL Community Resources

---

# Appendix A: Quick Reference

## Common Commands

```bash
# Build
rebar3 compile

# Run tests
rebar3 eunit          # Unit tests
rebar3 ct             # Common Test suites

# Static analysis
rebar3 dialyzer       # Type analysis
rebar3 xref           # Cross-reference check

# Documentation
rebar3 edoc           # Generate documentation

# Interactive shell
rebar3 shell

# Build executable
rebar3 escriptize
```

## Erlang Shell Quick Reference

```erlang
% Load and compile a file
> c("my_module.erl").

% Get help
> h(module).
> h(module, function).

% Get module info
> module:module_info().
> module:module_info(attributes).

% Pattern matching in shell
> {ok, Spec} = wf_yaml_spec:from_yaml_file("test.yaml").

% Format output
> io:format("~p~n", [ComplexTerm]).
> io:format("~s~n", [BinaryTerm]).

% Shell history
> h().              % History
> e(1).             % Repeat line 1
```

## Pattern Quick Reference

| ID | Pattern | Description |
|----|---------|-------------|
| P1 | Sequence | Execute tasks in order |
| P2 | Parallel Split | Create concurrent branches |
| P3 | Synchronization | Wait for all branches |
| P4 | Exclusive Choice | Select one branch |
| P5 | Simple Merge | Merge one branch |
| P6 | Multiple Choice | Select multiple branches |
| P7 | Structured Sync Merge | Merge with synchronization |
| P8 | Multiple Merge | Merge multiple paths |
| P9 | Discriminator | First-completion wins |
| P10 | Arbitrary Cycles | Cycle back to any point |
| P11 | Implicit Termination | Auto-terminate when done |
| P12 | Multiple Instances (No Sync) | Parallel without sync |
| P13 | Multiple Instances (Static) | Fixed parallelism |
| P14 | Multiple Instances (Runtime) | Runtime-determined count |
| P15 | Multiple Instances (Dynamic) | Dynamically created |
| P16 | Deferred Choice | Runtime branch selection |
| P17 | Interleaved Routing | Interleaved execution |
| P18 | Milestone | Guard activity with milestone |
| P19 | Cancel Activity | Cancel a single activity |
| P20 | Cancel Case | Cancel entire workflow |
| P21 | Structured Loop | While/until loops |
| P22 | Recursion | Recursive workflow calls |
| P23 | Transient Trigger | One-time event trigger |
| P24 | Persistent Trigger | Persistent event trigger |
| P25 | Cancel Region | Scoped cancellation |
| P26 | Cancel MI Activity | Cancel multi-instance |
| P27 | Complete MI Activity | Complete multi-instance |
| P28 | Blocking Discriminator | Blocking first-win |
| P29 | Cancelling Discriminator | Cancel on first-win |
| P30 | Structured Partial Join | Partial join with sync |
| P31 | Blocking Partial Join | Blocking partial join |
| P32 | Cancelling Partial Join | Cancel on partial join |
| P33 | Generalized AND Join | Complex AND join |
| P34 | Static Partial Join (MI) | Static MI partial join |
| P35 | Cancelling Partial Join (MI) | Cancel MI partial join |
| P36 | Dynamic Partial Join (MI) | Dynamic MI partial join |
| P37 | Local Sync Merge | Local synchronization |
| P38 | General Sync Merge | General synchronization |
| P39 | Critical Section | Mutual exclusion |
| P40 | Interleaved Routing (Alt) | Alternative interleaved |
| P41 | Thread Merge | Merge threads |
| P42 | Thread Split | Split threads |
| P43 | Explicit Termination | Explicit termination |

## Module Index

### Core Modules
- `gen_pnet` - Petri net runtime
- `gen_yawl` - YAWL wrapper
- `yawl_compile` - Compiler
- `yawl_pattern_registry` - Pattern lookup
- `yawl_pattern_expander` - Pattern expansion
- `wf_yaml_spec` - YAML parser

### Workflow Modules
- `yawl_executor` - Workflow executor
- `yawl_recovery` - Recovery and checkpointing
- `yawl_cancel_runtime` - Cancellation runtime
- `yawl_mi_runtime` - Multi-instance runtime
- `yawl_timer_runtime` - Timer runtime
- `yawl_telemetry` - OpenTelemetry integration

### Verification Modules
- `yawl_validate` - Specification validation
- `yawl_schema` - Schema validation
- `wf_verify` - Verification utilities

---

# Appendix B: Glossary

**BCD (Business Context Document)** - Structured document capturing business requirements for workflow specification.

**Cancellation Region** - A scoped area of a workflow that can be cancelled independently without affecting the rest of the workflow.

**Compensation** - The act of undoing a completed activity to maintain consistency when a downstream error occurs.

**Critical Section** - A section of code that must be executed by only one process at a time to prevent conflicts.

**Discriminator** - A pattern that accepts the first completion among multiple concurrent branches and discards the rest.

**Explicit Termination** - A pattern that explicitly terminates a workflow when a specific condition is met.

**Fire** - The act of a transition executing, consuming input tokens and producing output tokens.

**Gen_pnet** - Generic Petri net behavior, the core runtime in CRE.

**Gen_yawl** - YAWL wrapper around gen_pnet.

**Implicit Termination** - Automatic termination when no work remains.

**Marking** - The distribution of tokens across places in a Petri net, representing the current state.

**Milestone** - A condition that must be reached before an activity can be enabled.

**Mode** - The state of a Petri net, represented as a mapping from places to token lists.

**Multi-instance** - Multiple concurrent executions of the same task.

**Nondeterministic Choice** - A choice that is resolved at runtime, potentially with different results each time.

**Place** - A state location in a Petri net that holds tokens.

**Preset** - The input places for a transition.

**Postset** - The output places for a transition.

**Saga** - A pattern for managing distributed transactions using compensation.

**Thread Split/Merge** - Patterns for splitting a workflow into concurrent threads that can be merged back together.

**Token** - Represents workflow state or data, held in places.

**Transition** - An action in a Petri net that consumes input tokens and produces output tokens.

**Workflow** - A collection of tasks and patterns forming a business process.

**YAWL** - Yet Another Workflow Language, a formal workflow specification language.

---

# Appendix C: Troubleshooting

## Common Issues and Solutions

### Issue: Compilation Errors

**Symptom:** `{error, compile_error}`

**Solutions:**
1. Check YAML syntax using a YAML validator
2. Verify all referenced patterns exist
3. Ensure all nodes have unique IDs within a net
4. Check that flows reference valid node IDs

### Issue: Pattern Not Found

**Symptom:** `{error, {unknown_pattern, PatternId}}`

**Solutions:**
1. Check pattern ID against registry: `yawl_pattern_registry:all_patterns()`
2. Verify pattern name format: `P<number>_<Name>`
3. Ensure pattern module is compiled

### Issue: Token Deadlock

**Symptom:** Workflow appears stuck, no transitions enabled

**Solutions:**
1. Check current marking: `gen_pnet:marking(Pid)`
2. Verify all required places have tokens
3. Use model checking to verify no structural deadlocks
4. Check for circular dependencies

### Issue: Performance Problems

**Symptom:** Slow workflow execution

**Solutions:**
1. Profile using `fprof` or `eprof`
2. Reduce parallel instance count
3. Optimize data transformation functions
4. Enable caching for frequently used data

### Issue: Data Loss

**Symptom:** Tokens or data disappear during execution

**Solutions:**
1. Enable audit logging: `wf_audit_log`
2. Check compensation handlers
3. Verify transition fire logic
4. Use property-based tests to verify invariants

## Debugging Techniques

### Enable Debug Logging

```erlang
application:set_env(cre, log_level, debug).
```

### Inspect Petri Net State

```erlang
% Get current marking
{ok, Marking} = gen_pnet:marking(Pid).

% Get enabled transitions
Enabled = gen_pnet:ls(Pid, enabled).

% Get all places
Places = gen_pnet:ls(Pid, places).

% Get all transitions
Transitions = gen_pnet:ls(Pid, transitions).
```

### Visualize Workflow

```erlang
% Generate DOT graph
{ok, Dot} = wf_yaml_spec:to_dot(Spec).
file:write_file("workflow.dot", Dot).

% Use Graphviz to render
% dot -Tpng workflow.dot -o workflow.png
```

### Trace Execution

```erlang
% Enable tracing
 dbg:tracer(),
 dbg:p(all, c),
 dbg:tpl(gen_pnet, fire, 3, x),
 dbg:tpl(gen_pnet, is_enabled, 3, x).

% Run workflow
...

% Stop tracing
 dbg:stop_clear().
```

---

## Course Completion Certificate

Upon completion of this course, you should be able to:

- [ ] Design workflow specifications using BCD methodology
- [ ] Select and compose appropriate workflow patterns
- [ ] Write YAML specifications for complex workflows
- [ ] Compile specifications to executable code
- [ ] Verify workflow correctness using multiple techniques
- [ ] Handle exceptions and compensation
- [ ] Apply swarm coordination patterns
- [ ] Debug and optimize workflow performance

**Recommended Next Steps:**

1. Complete the workshop project
2. Contribute to the CRE project
3. Study the verification report
4. Explore the AGI Symposium case study
5. Read the complete pattern reference

**Contact:**

For questions about this course or CRE, please refer to:
- GitHub Repository
- Documentation Index
- Community Resources

---

**End of Training Materials**

*This document is part of the CRE (Common Runtime Environment) project, version 0.3.0*
