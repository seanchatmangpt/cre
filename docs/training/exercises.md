# Generative Analysis for Constructive Systems: Tutorial Workbook

**Hands-On Exercises for Learning YAWL Workflow Patterns and Petri Net Compilation**

---

## Introduction

This workbook provides hands-on exercises for learning Generative Analysis for Constructive Systems using the CRE (Common Runtime Environment) and YAWL (Yet Another Workflow Language) workflow engine. Each exercise builds on the previous ones, using the AGI Symposium Omega workflow as a running example.

**Prerequisites:**
- Erlang/OTP 25+
- Basic understanding of concurrent programming
- Familiarity with workflow concepts

**Setup:**
```bash
cd /Users/sac/cre
rebar3 compile
rebar3 shell
```

---

## Exercise 1: Writing a Business Context Document (BCD)

### Learning Objective
Learn to define the business context, stakeholders, and workflow requirements for a complex system.

### Prerequisites
- Text editor
- Understanding of the problem domain

### Instructions

**Step 1: Identify the Business Domain**

The AGI Symposium Omega workflow represents organizing an academic conference on Artificial General Intelligence. A BCD captures:
- Business objectives
- Stakeholders and their roles
- Key processes and workflows
- Success criteria

**Step 2: Define Business Objectives**

Create a business context document answering:
1. What is the primary business goal?
2. Who are the primary stakeholders?
3. What are the critical success factors?
4. What constraints exist?

**Step 3: Map Stakeholders to Roles**

For the AGI Symposium, identify which stakeholders map to which workflow roles:

| Stakeholder | Role | Responsibilities |
|-------------|------|------------------|
| Conference Organizer | Chair | Overall coordination |
| Technical Program Lead | ProgramChair | Paper selection |
| ... | ... | ... |

**Step 4: Define Workflow Boundaries**

Answer these questions:
- What triggers the workflow to start? (CFP announcement)
- What constitutes successful completion? (Published proceedings)
- What external systems interact with the workflow?
- What are the decision points?

### Expected Outcome

A document containing:

```markdown
# Business Context: AGI Symposium Omega

## Business Objectives
- Host a prestigious academic conference on AGI
- Review and select high-quality research papers
- Coordinate logistics for 800+ attendees

## Stakeholders
- **Chair**: Overall conference management
- **ProgramChair**: Academic program oversight
- **Reviewer**: Paper evaluation
- **OpsLead**: Venue and operations
- **SafetyOfficer**: Emergency response

## Key Workflows
1. Paper submission and review (ProgramThread)
2. Venue and operations planning (OpsThread)
3. Communications and press (CommsThread)
4. Incident response (IncidentThread)

## Success Criteria
- All 43 workflow patterns successfully executed
- Program published before deadline
- Zero safety incidents
- Attendee satisfaction > 90%
```

### Solution Verification

Check your BCD against:
1. Are all 20 roles from the YAML defined?
2. Are the 4 main threads (Program, Ops, Comms, Incident) described?
3. Is the entry condition (CFP) clear?
4. Is the exit condition (Published proceedings) defined?

### Common Mistakes

1. **Overspecifying technical details**: A BCD should focus on business needs, not implementation
2. **Missing stakeholders**: Forgetting implicit roles like Attendee or Speaker
3. **Unclear success criteria**: "Good conference" is not measurable; "Published proceedings" is
4. **Ignoring constraints**: Budget, time, and resource constraints must be documented

---

## Exercise 2: Pattern Selection

### Learning Objective
Learn to identify and select appropriate YAWL workflow patterns for business requirements.

### Prerequisites
- Completed Exercise 1
- Reference: `/Users/sac/cre/docs/YAWL_PATTERN_REFERENCE.md`

### Instructions

**Step 1: Analyze Business Requirements**

For each business requirement, identify the corresponding workflow pattern:

| Business Requirement | Pattern ID | Pattern Name |
|---------------------|------------|--------------|
| Tasks must complete in order | P1 | Sequence |
| Multiple tasks run concurrently | P2 | Parallel Split |
| Wait for all branches to complete | P3 | Synchronization |
| Choose exactly one option | P4 | Exclusive Choice |

**Step 2: Map AGI Symposium Requirements to Patterns**

Using the AGI Symposium Omega YAML, match each business requirement to its pattern:

Example:
- Requirement: "The symposium splits into program, operations, and communications workstreams"
- Pattern: P42 Thread Split

Complete the mapping for these requirements:
1. Review cycle needs 3 reviewers per paper
2. Go/No-Go decision waits for all threads to be ready
3. Papers can be accepted, rejected, or revised
4. Author rebuttal triggers another review cycle
5. First venue contract selected cancels other negotiations
6. Safety incident triggers immediate response
7. Demo zone can be cancelled independently
8. Emergency stops everything

**Step 3: Verify Pattern Coverage**

Check that all 43 patterns are used in the AGI Symposium specification:

```erlang
%% In the Erlang shell
{ok, Spec} = wf_yaml_spec:from_yaml_file("test/fixtures/agi_symposium_omega.yaml"),
Index = wf_yaml_spec:pattern_usage_index(Spec),
maps:keys(Index).
%% Should return [<<"P1">>, <<"P2">>, ..., <<"P43">>]
```

### Expected Outcome

A completed pattern mapping table:

| Requirement | Pattern | Instance ID |
|-------------|---------|-------------|
| 3 reviewers per paper | P13 MI Design-Time | P13_mi_reviews3 |
| Wait for all threads | P3 Synchronization | P3_sync_gonogo |
| Accept/reject/revise | P4 Exclusive Choice | P4_choice_decision |
| Rebuttal loop | P21 Structured Loop | P21_loop_revise |
| Venue race | P29 Cancelling Discriminator | P29_cancel_discriminator_venue |
| Incident response | P28 Blocking Discriminator | P28_blocking_discriminator_incident |
| Cancel demo zone | P25 Cancel Region | P25_cancel_region_highrisk |
| Emergency stop | P43 Explicit Termination | P43_explicit_termination_emergency |

### Solution Verification

1. Each requirement maps to exactly one pattern
2. All patterns are identified in the YAML
3. Pattern semantics match the business requirement

### Common Mistakes

1. **Confusing similar patterns**: P3 (Synchronization) vs P7 (Structured Sync Merge) vs P38 (General Sync Merge)
2. **Ignoring cardinality**: P13 (fixed instances) vs P14 (runtime-known) vs P15 (runtime-unknown)
3. **Missing cancellation semantics**: P19 (activity) vs P20 (case) vs P25 (region)
4. **Trigger vs condition**: P23 (transient trigger) vs P24 (persistent trigger)

---

## Exercise 3: YAML Specification

### Learning Objective
Learn to write a YAWL workflow specification in YAML format.

### Prerequisites
- Completed Exercise 2
- Understanding of YAML syntax
- Reference: `/Users/sac/cre/test/fixtures/agi_symposium_omega.yaml`

### Instructions

**Step 1: Create a Minimal Workflow Specification**

Create a file `my_workflow.yaml` with the basic structure:

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "my_first_workflow"
  metaData:
    title: "My First Workflow"
    version: "1.0"
    creator: "Your Name"

  rootNet: MainNet

  roles:
    - User
    - Approver

  nets:
    - id: MainNet
      type: NetFacts
      nodes:
        - {id: Start, kind: inputCondition}
        - {id: End, kind: outputCondition}
        - {id: Task1, kind: task, taskType: human, name: "Do Something"}
        - {id: Task2, kind: task, taskType: automated, name: "Process"}

      flows:
        - {from: Start, to: Task1}
        - {from: Task1, to: Task2}
        - {from: Task2, to: End}
```

**Step 2: Add Pattern Instances**

Add pattern instances to define workflow behavior:

```yaml
  pattern_instances:
    - {id: P1_sequence, pattern: P1_Sequence, net: MainNet,
       from: Start, to: End,
       label: "P1 Sequence: Sequential tasks"}
```

**Step 3: Define Variables**

Add workflow variables to track state:

```yaml
  nets:
    - id: MainNet
      type: NetFacts
      variables:
        - {name: approval_status, type: string, initial: "pending"}
        - {name: process_count, type: long, initial: 0}
      nodes:
        # ... nodes and flows
```

**Step 4: Create a Multi-Net Workflow**

Extend the workflow with subnets for parallel processing:

```yaml
  nets:
    - id: MainNet
      # ... main net definition
      subnets:
        - {id: ProcessingThread, entry: ProcessEntry, exit: ProcessExit}
        - {id: ApprovalThread, entry: ApproveEntry, exit: ApproveExit}

    - id: ProcessingThread
      type: NetFacts
      nodes:
        - {id: ProcessEntry, kind: condition}
        - {id: DoWork, kind: task, taskType: automated}
        - {id: ProcessExit, kind: condition}

    - id: ApprovalThread
      type: NetFacts
      nodes:
        - {id: ApproveEntry, kind: condition}
        - {id: ApproveWork, kind: task, taskType: human}
        - {id: ApproveExit, kind: condition}
```

**Step 5: Add Cancellation Regions**

Define regions that can be cancelled independently:

```yaml
  nets:
    - id: MainNet
      regions:
        - {id: Region_Optional, cancel_region: true,
           description: "Optional processing that can be cancelled"}
```

### Expected Outcome

A valid YAML specification that:
1. Parses without errors
2. Has at least 2 nets (root + 1 subnet)
3. Defines at least 3 pattern instances
4. Includes variables for state tracking

### Solution Verification

Parse your specification and verify:

```erlang
%% In the Erlang shell
{ok, Spec} = wf_yaml_spec:from_yaml_file("my_workflow.yaml").
%% Verify basic properties
wf_yaml_spec:id(Spec).
%% Should return <<"my_first_workflow">>

wf_yaml_spec:nets(Spec).
%% Should return [<<"MainNet">>, <<"ProcessingThread">>, <<"ApprovalThread">>]

wf_yaml_spec:pattern_instances(Spec).
%% Should return a list of pattern instances

wf_yaml_spec:validate(Spec).
%% Should return ok
```

### Common Mistakes

1. **Missing required fields**: Every net must have `id`, `type`, and at least one node
2. **Invalid node kinds**: Must be one of: `inputCondition`, `outputCondition`, `task`, `condition`
3. **Invalid task types**: Must be one of: `automated`, `human`, `service`
4. **Undefined flow targets**: All `from` and `to` values must reference existing node IDs
5. **Incorrect pattern IDs**: Must match the pattern registry (P1 through P43)

---

## Exercise 4: Compilation

### Learning Objective
Learn to compile a YAWL specification into executable Petri net modules.

### Prerequisites
- Completed Exercise 3
- Valid YAML specification file

### Instructions

**Step 1: Load the YAML Specification**

```erlang
%% In the Erlang shell
{ok, Spec} = wf_yaml_spec:from_yaml_file("test/fixtures/agi_symposium_omega.yaml").
```

**Step 2: Compile the Specification**

```erlang
%% Compile with default options
{ok, Compiled} = yawl_compile:compile(Spec, #{}).
```

**Step 3: Inspect the Compilation Result**

```erlang
%% Check what was compiled
maps:get(spec_id, Compiled).
%% Should return <<"agi_symposium_omega">>

maps:get(modules, Compiled).
%% Should return a map of net_id to module code

maps:get(places, Compiled).
%% Should return places for each net

maps:get(transitions, Compiled).
%% Should return transitions for each net
```

**Step 4: Compile with Options**

```erlang
%% Compile with custom module prefix
{ok, Compiled2} = yawl_compile:compile(Spec, #{
    module_prefix => <<"my_workflow_">>,
    seed => 42
}).
```

**Step 5: Write Compiled Modules to Files**

```erlang
%% Write to a temporary directory
{ok, Files} = yawl_compile:compile_to_file(
    Spec,
    #{},
    "/tmp/compiled_workflow"
).
%% Files will contain paths to generated .erl files
```

**Step 6: Load and Execute Using wf_yawl_executor**

```erlang
%% Load and compile using the high-level executor
{ok, Executor} = wf_yawl_executor:compile_workflow(Spec, #{}).

%% Ensure modules are loaded into the VM
{ok, Executor2} = wf_yawl_executor:ensure_modules_loaded(Executor).

%% Get executor information
Info = wf_yawl_executor:executor_info(Executor2).
```

### Expected Outcome

The compilation should produce:
1. One Erlang module per net (e.g., `yawl_Symposium.erl`)
2. Each module implements the `gen_pnet` behavior
3. Places, transitions, and flows correctly translated
4. Pattern instances expanded into Petri net structures

### Solution Verification

```erlang
%% Verify compilation succeeded
case wf_yawl_executor:compile_workflow(Spec, #{}) of
    {ok, Executor} ->
        io:format("Compilation successful!~n"),
        io:format("Root module: ~p~n", [wf_yawl_executor:get_root_module(Executor)]);
    {error, Reason} ->
        io:format("Compilation failed: ~p~n", [Reason])
end.

%% Check for specific nets in the compiled result
{ok, C} = yawl_compile:compile(Spec, #{}),
Nets = maps:get(modules, C),
io:format("Compiled ~p nets~n", [maps:size(Nets)]).
```

### Common Mistakes

1. **Missing pattern modules**: Some patterns require implementation modules in `src/patterns/`
2. **Invalid flow references**: Flows must reference existing nodes
3. **Circular dependencies**: Nets cannot reference each other in cycles
4. **Missing entry/exit for subnets**: All subnets must have entry and exit nodes defined
5. **Module naming conflicts**: Custom module prefixes may conflict with existing modules

---

## Exercise 5: Receipt Verification

### Learning Objective
Learn to analyze execution traces (receipts) to verify workflow behavior.

### Prerequisites
- Completed Exercise 4
- Running workflow instance

### Instructions

**Step 1: Start a Workflow Instance**

```erlang
%% Load, compile, and start
{ok, Spec} = wf_yaml_spec:from_yaml_file("test/fixtures/agi_symposium_omega.yaml"),
{ok, Executor} = wf_yawl_executor:compile_workflow(Spec, #{}),
{ok, Executor2} = wf_yawl_executor:ensure_modules_loaded(Executor),
{ok, Pid, CaseId} = wf_yawl_executor:start_workflow(Executor2, #{}).
```

**Step 2: Execute a Single Step**

```erlang
%% Execute one transition
case wf_yawl_executor:execute_step(Pid) of
    {ok, Receipt} ->
        io:format("Transition fired: ~p~n", [maps:get(trsn, Receipt)]);
    abort ->
        io:format("No transitions enabled~n");
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
end.
```

**Step 3: Inspect the Receipt**

```erlang
%% Get detailed receipt information
{ok, Receipt} = wf_yawl_executor:execute_step(Pid),

%% Receipt contains:
%% - trsn: The transition that fired
%% - mode: Token consumption (preset places)
%% - produce: Token production (postset places)

Trsn = maps:get(trsn, Receipt),
Mode = maps:get(mode, Receipt),
Produce = maps:get(produce, Receipt),

io:format("Transition: ~p~n", [Trsn]),
io:format("Consumed: ~p~n", [Mode]),
io:format("Produced: ~p~n", [Produce]).
```

**Step 4: Check Workflow State**

```erlang
%% Get current marking
Marking = gen_yawl:marking(Pid),
io:format("Current marking: ~p~n", [Marking]).

%% Get workflow state with status
{ok, State} = wf_yawl_executor:get_workflow_state(Pid),
io:format("Status: ~p~n", [maps:get(status, State)]).
```

**Step 5: Execute Multiple Steps**

```erlang
%% Execute until quiescent or max steps
{ok, Receipts} = wf_yawl_executor:execute_step(Pid, 100),
io:format("Executed ~p transitions~n", [length(Receipts)]).

%% Inspect the execution trace
lists:foreach(fun(R) ->
    Trsn = maps:get(trsn, R),
    io:format("  - ~p~n", [Trsn])
end, Receipts).
```

**Step 6: Verify Pattern Execution**

```erlang
%% Track which patterns executed
PatternExecuted = fun(Receipt) ->
    Trsn = maps:get(trsn, Receipt),
    %% Map transition to pattern based on naming convention
    case Trsn of
        t_split -> <<"P42 Thread Split">>;
        t_merge -> <<"P41 Thread Merge">>;
        _ -> <<"Unknown">>
    end
end,

Patterns = lists:map(PatternExecuted, Receipts),
io:format("Patterns executed: ~p~n", [lists:usort(Patterns)]).
```

### Expected Outcome

You should be able to:
1. See which transitions fired in what order
2. Verify tokens were consumed and produced correctly
3. Identify when the workflow reached quiescence
4. Map transitions back to pattern instances

### Solution Verification

Create a verification function:

```erlang
verify_receipts(Receipts, ExpectedTransitions) ->
    ActualTransitions = [maps:get(trsn, R) || R <- Receipts],
    Missing = ExpectedTransitions -- ActualTransitions,
    Extra = ActualTransitions -- ExpectedTransitions,
    case {Missing, Extra} of
        {[], []} ->
            io:format("Receipts match expected~n");
        {[], Extra} ->
            io:format("Warning: Extra transitions: ~p~n", [Extra]);
        {Missing, []} ->
            io:format("Error: Missing transitions: ~p~n", [Missing]);
        {Missing, Extra} ->
            io:format("Error: Missing: ~p, Extra: ~p~n", [Missing, Extra])
    end.
```

### Common Mistakes

1. **Assuming linear execution**: Workflows may have non-deterministic choices
2. **Ignoring abort conditions**: `abort` means no transitions are enabled
3. **Forgetting token multiplicity**: Places can hold multiple tokens
4. **Misreading receipts**: `mode` shows what was consumed, not the marking
5. **Not checking for blocking**: A workflow may be waiting for external input

---

## Exercise 6: Pattern Composition

### Learning Objective
Learn to compose simple patterns into complex workflows.

### Prerequisites
- Completed Exercises 1-5
- Understanding of pattern semantics

### Instructions

**Step 1: Design a Sequential Workflow**

Compose P1 (Sequence) pattern:

```yaml
pattern_instances:
  - {id: seq_1, pattern: P1_Sequence, net: MainNet,
     from: Start, to: TaskA, label: "Step 1"}
  - {id: seq_2, pattern: P1_Sequence, net: MainNet,
     from: TaskA, to: TaskB, label: "Step 2"}
  - {id: seq_3, pattern: P1_Sequence, net: MainNet,
     from: TaskB, to: End, label: "Step 3"}
```

**Step 2: Add Parallel Branching**

Compose P2 (Parallel Split) + P3 (Synchronization):

```yaml
pattern_instances:
  - {id: split_work, pattern: P2_ParallelSplit, net: MainNet,
     split_task: SplitWork, branches: [BranchA, BranchB, BranchC],
     label: "P2: Split into 3 branches"}
  - {id: join_work, pattern: P3_Synchronization, net: MainNet,
     join_task: JoinWork, waits_for: [BranchA, BranchB, BranchC],
     label: "P3: Join 3 branches"}
```

**Step 3: Add Conditional Logic**

Compose P4 (Exclusive Choice) + P5 (Simple Merge):

```yaml
pattern_instances:
  - {id: choose_path, pattern: P4_ExclusiveChoice, net: MainNet,
     at: DecisionPoint, choices: [PathA, PathB, PathC],
     label: "P4: Choose one path"}
  - {id: merge_paths, pattern: P5_SimpleMerge, net: MainNet,
     froms: [PathA, PathB, PathC], to: Continue,
     label: "P5: Merge without waiting"}
```

**Step 4: Add a Loop**

Compose P21 (Structured Loop):

```yaml
pattern_instances:
  - {id: review_loop, pattern: P21_StructuredLoop, net: MainNet,
     entry: ReviewEntry, body: [Review, Comment, Revise],
     exit_condition: "approved||rejected",
     label: "P21: Review until approved or rejected"}
```

**Step 5: Add Multi-Instance Processing**

Compose P13 (MI Design-Time) + P34 (Static Partial Join):

```yaml
pattern_instances:
  - {id: three_reviews, pattern: P13_MI_DesignTime, net: MainNet,
     task: PaperReview, instances: 3,
     label: "P13: Spawn 3 review instances"}
  - {id: two_sufficient, pattern: P34_StaticPartialJoinMI, net: MainNet,
     total_instances: 3, threshold: 2,
     label: "P34: 2 of 3 reviews sufficient"}
```

**Step 6: Add Cancellation**

Compose P25 (Cancel Region):

```yaml
nets:
  - id: MainNet
    regions:
      - {id: Region_Optional, cancel_region: true}

pattern_instances:
  - {id: cancel_optional, pattern: P25_CancelRegion, net: MainNet,
     region: Region_Optional, cancel_event: timeout,
     label: "P25: Cancel optional region on timeout"}
```

**Step 7: Build the Complete AGI Symposium**

Review the complete AGI Symposium Omega specification to see how all 43 patterns compose:

```erlang
%% Analyze pattern composition
{ok, Spec} = wf_yaml_spec:from_yaml_file("test/fixtures/agi_symposium_omega.yaml"),
Instances = wf_yaml_spec:pattern_instances(Spec),

%% Group by net
Grouped = lists:foldl(fun(I, Acc) ->
    Net = maps:get(<<"net">>, I, maps:get(net, I)),
    maps:append(Net, [I], Acc)
end, #{}, Instances),

%% Show patterns per net
maps:foreach(fun(Net, NetInstances) ->
    io:format("~s: ~p patterns~n", [Net, length(NetInstances)])
end, Grouped).
```

### Expected Outcome

A composed workflow that:
1. Demonstrates sequential execution
2. Shows parallel processing and synchronization
3. Includes conditional branching
4. Has looping behavior
5. Uses multi-instance patterns
6. Implements cancellation

### Solution Verification

```erlang
%% Verify pattern composition
verify_composition(Spec) ->
    Instances = wf_yaml_spec:pattern_instances(Spec),

    %% Check for complementary patterns
    HasSplit = lists:any(fun(I) ->
        Pattern = maps:get(<<"pattern">>, I, maps:get(pattern, I)),
        Pattern =:= <<"P2_ParallelSplit">> orelse Pattern =:= p2_parallel_split
    end, Instances),

    HasSync = lists:any(fun(I) ->
        Pattern = maps:get(<<"pattern">>, I, maps:get(pattern, I)),
        Pattern =:= <<"P3_Synchronization">> orelse Pattern =:= p3_synchronization
    end, Instances),

    %% P2 without P3 is usually an error
    case {HasSplit, HasSync} of
        {true, false} ->
            io:format("Warning: P2 without P3 (unmatched parallel split)~n");
        _ ->
            ok
    end,

    %% Check for loop exits
    Loops = [I || I <- Instances,
        begin
            Pattern = maps:get(<<"pattern">>, I, maps:get(pattern, I)),
            Pattern =:= <<"P21_StructuredLoop">> orelse Pattern =:= p21_structured_loop
        end],
    io:format("Found ~p structured loops~n", [length(Loops)]),

    ok.
```

### Common Mistakes

1. **Unmatched splits/merges**: P2 (Parallel Split) should pair with P3 (Synchronization)
2. **Dangling merges**: P5 (Simple Merge) with only one input is unnecessary
3. **Infinite loops**: P21 (Structured Loop) must have a clear exit condition
4. **Over-specified patterns**: P2 followed immediately by P4 is redundant
5. **Missing cancellation cleanup**: P25 (Cancel Region) needs clear region boundaries

---

## Appendix: Reference Materials

### Quick Command Reference

```bash
# Compile the project
rebar3 compile

# Run unit tests
rebar3 eunit

# Run Common Test suites
rebar3 ct

# Start Erlang shell
rebar3 shell

# Format code
rebar3 efmt -c

# Generate documentation
rebar3 edoc
```

### Key Modules Reference

| Module | Purpose |
|--------|---------|
| `wf_yaml_spec` | Parse YAML 0.2 specifications |
| `yawl_compile` | Compile specs to gen_pnet modules |
| `wf_yawl_executor` | High-level workflow execution |
| `gen_yawl` | Core workflow runtime |
| `yawl_pattern_registry` | Pattern ID to module mapping |

### Pattern Quick Reference

| ID | Name | Use Case |
|----|------|----------|
| P1 | Sequence | Tasks must complete in order |
| P2 | Parallel Split | Fork into concurrent branches |
| P3 | Synchronization | Wait for all branches |
| P4 | Exclusive Choice | Select exactly one option |
| P5 | Simple Merge | Continue without waiting |
| P6 | Multiple Choice | Select multiple options |
| P7 | Structured Sync Merge | Wait for selected branches |
| P8 | Multiple Merge | Continue from any input |
| P9 | Discriminator | First completion wins |
| P10 | Arbitrary Cycles | Unstructured loops |
| P11 | Implicit Termination | End when no work remains |
| P12-P15 | Multi-Instance | Parallel task instances |
| P16 | Deferred Choice | Runtime event selection |
| P17 | Interleaved Parallel | One-at-a-time parallel |
| P18 | Milestone | Time-based gate |
| P19 | Cancel Activity | Cancel single task |
| P20 | Cancel Case | Cancel entire workflow |
| P21 | Structured Loop | Bounded iteration |
| P22 | Recursion | Sub-workflow invocation |
| P23 | Transient Trigger | Event expires |
| P24 | Persistent Trigger | Event persists |
| P25 | Cancel Region | Scoped cancellation |
| P26-P27 | MI Activity Control | Control MI execution |
| P28-P29 | Discriminators | Advanced selection |
| P30-P32 | Partial Joins | N-of-M joins |
| P33 | Generalized AND-Join | Dynamic join |
| P34-P36 | Partial Join MI | MI-specific joins |
| P37 | Local Sync Merge | Scoped sync |
| P38 | General Sync Merge | Unstructured sync |
| P39 | Critical Section | Mutual exclusion |
| P40 | Interleaved Routing | Ordered interleaving |
| P41 | Thread Merge | Merge threads |
| P42 | Thread Split | Split threads |
| P43 | Explicit Termination | Hard stop |

---

## Solutions

### Exercise 1 Solution

See `/Users/sac/cre/test/fixtures/agi_symposium_omega.yaml` lines 1-35 for the complete business context encoded as YAML metadata.

### Exercise 2 Solution

```erlang
%% Get pattern usage index
{ok, Spec} = wf_yaml_spec:from_yaml_file("test/fixtures/agi_symposium_omega.yaml"),
Index = wf_yaml_spec:pattern_usage_index(Spec),

%% Print all pattern usages
maps:foreach(fun(PatternId, Instances) ->
    io:format("~s: ~p~n", [PatternId, Instances])
end, Index).
```

### Exercise 3 Solution

A minimal valid workflow is in `/Users/sac/cre/test/fixtures/orderfulfillment_2_1.yawl`

### Exercise 4 Solution

```erlang
%% Complete compilation workflow
compile_and_verify(File) ->
    {ok, Spec} = wf_yaml_spec:from_yaml_file(File),
    {ok, Compiled} = yawl_compile:compile(Spec, #{}),
    {ok, Executor} = wf_yawl_executor:compile_workflow(Spec, #{}),
    {ok, _} = wf_yawl_executor:ensure_modules_loaded(Executor),
    io:format("Compilation successful~n"),
    ok.
```

### Exercise 5 Solution

```erlang
%% Complete receipt verification
verify_execution_trace(Pid, ExpectedSteps) ->
    {ok, Receipts} = wf_yawl_executor:execute_step(Pid, 1000),
    ActualSteps = [maps:get(trsn, R) || R <- Receipts],
    case ActualSteps of
        ExpectedSteps ->
            io:format("Execution trace verified~n");
        _ ->
            io:format("Trace mismatch~nExpected: ~p~nActual: ~p~n",
                [ExpectedSteps, ActualSteps])
    end.
```

### Exercise 6 Solution

The complete AGI Symposium Omega specification demonstrates all composition patterns. Analyze it with:

```erlang
{ok, Spec} = wf_yaml_spec:from_yaml_file("test/fixtures/agi_symposium_omega.yaml"),
Instances = wf_yaml_spec:pattern_instances(Spec),

%% Find pattern composition relationships
find_compositions(Instances).
```

---

**Document Version:** 1.0.0
**Last Updated:** 2026-02-07
**For:** CRE Tutorial Users
