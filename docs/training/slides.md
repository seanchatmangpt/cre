# Generative Analysis for Constructive Systems
## Slide Deck Outline

**Version:** 1.0
**Date:** 2026-02-07
**Presentation Lengths:** 30 min | 60 min | 90 min

---

## Presentation Time Slot Guide

| Time Slot | Slides to Use | Approximate Timing |
|-----------|---------------|-------------------|
| **30 min** | Intro (1-5), Core (6-12), Selected Deep Dive (13-18), Case Study Highlights (31-35), Conclusion (46-48) | ~1-2 min per slide |
| **60 min** | Intro (1-8), Core (9-17), Full Deep Dive (18-30), Case Study (31-40), Conclusion (46-48) | ~1-1.5 min per slide |
| **90 min** | Complete deck (1-48) + live demos + Q&A | ~1.5-2 min per slide |

**[DEMO]** markers indicate where live demonstrations can be inserted.

---

# Part 1: Introduction (5-8 slides)

---

## Slide 1: Title Slide

### Generative Analysis for Constructive Systems
**From Petri Nets to Verified Workflows**

**Subtitle:** A Formal Approach to Workflow Specification and Execution

**Bullet Points:**
- YAWL (Yet Another Workflow Language) workflow engine
- 43 verified workflow control patterns
- Petri net formal foundation
- Joe Armstrong "one runner" design philosophy
- Production-ready Erlang/OTP implementation

**Diagram Suggestion:**
```
[Specification] -> [Compilation] -> [Petri Net] -> [Execution] -> [Verification]
     YAML/XML           gen_yawl           gen_pnet         runtime      receipts
```

**Speaker Notes:**
- Welcome everyone to this presentation on Generative Analysis for Constructive Systems
- We'll explore how formal methods (Petri nets) enable verified workflow execution
- Key theme: generating executable systems from declarative specifications
- The CRE (Common Runtime Environment) implements these ideas in production Erlang/OTP

---

## Slide 2: What is Generative Analysis?

### Defining the Approach

**Bullet Points:**
- **Generative**: Automatically creating executable code from specifications
- **Analysis**: Formal verification of properties before execution
- **Constructive**: Building systems that are correct by construction
- **Declarative Specification**: Describe WHAT, not HOW
- **Verified Execution**: Runtime confirms specification compliance

**Diagram Suggestion:**
```
                    +-------------------+
                    |  Declarative      |
                    |  Specification    |
                    +---------+---------+
                              |
                    +---------v---------+
                    |  Formal Analysis  |
                    |  - Validation     |
                    |  - Verification   |
                    +---------+---------+
                              |
                    +---------v---------+
                    |  Code Generation |
                    |  - gen_pnet       |
                    |  - gen_yawl       |
                    +---------+---------+
                              |
                    +---------v---------+
                    |  Verified Runtime |
                    +-------------------+
```

**Speaker Notes:**
- Generative Analysis combines specification language with formal verification
- Unlike imperative programming, we declare the workflow structure
- The system generates code and verifies properties automatically
- Think of it as "verified compilation" for workflows

---

## Slide 3: Why Generative Analysis Matters

### The Problem with Manual Workflow Implementation

**Bullet Points:**
- **Workflow complexity**: Real processes have dozens of interaction patterns
- **Edge cases**: Manual implementations miss corner cases
- **Verification burden**: Testing cannot prove correctness
- **Maintenance cost**: Changes ripple through fragile code
- **Business risk**: Workflow failures = business failures

**The Generative Analysis Advantage:**
- Formal properties verified before execution
- All 43 YAWL patterns with proven correctness
- Receipt-based audit trail for every transition
- Deterministic execution with reproducible results

**Diagram Suggestion:**
```
Manual Implementation:
[Requirements] -> [Code] -> [Test] -> [Deploy] -> HOPE
                                    ^      |
                                    +------+

Generative Analysis:
[Specification] -> [Verify] -> [Generate] -> [Deploy] -> CERTAINTY
       ^               |
       +---------------+
```

**Speaker Notes:**
- Traditional approach: write code, test some paths, deploy and hope
- Our approach: specify formally, verify properties, generate proven code
- The difference is confidence in correctness

---

## Slide 4: The CRE System Overview

### Common Runtime Environment Architecture

**Bullet Points:**
- **Language**: Erlang/OTP 25-28
- **Formal Foundation**: Petri nets with marking algebra
- **Workflow Language**: YAWL 2.1/2.2
- **Patterns**: 43/43 YAWL workflow control patterns implemented
- **Runtime**: Single OTP behavior (gen_pnet) + pure helpers

**Key Features:**
- Human-in-the-loop approval flows with LLM integration
- OpenTelemetry observability
- XES event logging for process mining
- Web-based dashboard for visualization
- Distributed execution across clusters

**Diagram Suggestion:**
```
+---------------------------------------------------------------+
|                        CRE Application                        |
+---------------------------------------------------------------+
|  Applications: AGI Symposium, Order Fulfillment, ...          |
+---------------------------------------------------------------+
|  YAWL Compiler: XML/YAML -> gen_pnet modules                  |
+---------------------------------------------------------------+
|  Pattern Library: 43 workflow patterns                        |
+---------------------------------------------------------------+
|  Runtime: gen_yawl -> gen_pnet (single OTP process)           |
+---------------------------------------------------------------+
|  Pure Helpers: pnet_marking, wf_*, yawl_* (stateless)         |
+---------------------------------------------------------------+
```

**Speaker Notes:**
- CRE implements generative analysis for workflow systems
- Built on Erlang for distributed, fault-tolerant execution
- Petri nets provide the formal foundation
- 43 patterns cover all known workflow control structures

---

## Slide 5: Petri Nets - The Formal Foundation

### Why Petri Nets?

**Bullet Points:**
- **Mathematical rigor**: Well-defined execution semantics
- **State representation**: Markings show exact system state
- **Concurrency**: Natural model of parallel execution
- **Verification**: Properties like liveness, boundedness provable
- **Visualization**: Intuitive graphical representation

**Petri Net Components:**
- **Places**: State locations (hold tokens)
- **Transitions**: Actions (consume and produce tokens)
- **Arcs**: Connections (places to/from transitions)
- **Tokens**: Workflow state/data
- **Marking**: Distribution of tokens across places

**Diagram Suggestion:**
```
Example: Parallel Split Pattern

  [p_start]
     |
     | token
     v
  [t_split] ----+---> [p_branch1] ---> [t_task1] ---> [p_join1]
                |
                +---> [p_branch2] ---> [t_task2] ---> [p_join2]
                                                           |
                                                           v
                                                      [t_join]
                                                           |
                                                           v
                                                        [p_end]
```

**Speaker Notes:**
- Petri nets are not just diagrams - they're executable state machines
- Each pattern has a formal Petri net definition
- The marking algebra enables precise state tracking
- This mathematical foundation is what enables verification

---

## Slide 6: The Joe Armstrong Design Philosophy

### "One Real OTP Runner, Everything Else Pure Helpers"

**Bullet Points:**
- **Single stateful component**: gen_pnet (OTP gen_server)
- **Pure helper modules**: All logic in stateless functions
- **Message contracts**: Clear boundaries via messages
- **Deterministic execution**: Same input = same output
- **Testability**: Pure functions trivially testable

**Architecture:**
```
gen_yawl (wrapper)
  |
  +---> gen_pnet (THE only stateful component)
          |
          +---> pnet_marking (pure marking algebra)
          +---> pnet_types (pure type validators)
          +---> pnet_mode (pure mode enumeration)
          +---> All 43 pattern modules (pure helpers)
```

**Speaker Notes:**
- Joe Armstrong's design principle guides our architecture
- Only one place holds mutable state: the gen_pnet process
- Everything else is pure functional transformation
- This makes reasoning about the system tractable

---

## Slide 7: YAWL - Yet Another Workflow Language

### The Specification Language for Generative Analysis

**Bullet Points:**
- **Academic foundation**: 20+ years of workflow pattern research
- **Expressiveness**: Covers all known workflow patterns (43 total)
- **XML-based**: Standard interchange format
- **YAML 0.2**: Human-friendly specification (CRE extension)
- **Tool support**: Editor, simulator, engine

**YAWL Specification Elements:**
- **Tasks**: Atomic, composite, manual, automated
- **Conditions**: OR join/split, XOR join/split
- **Flows**: Input, output, ordinary flows
- **Decompositions**: Nested workflow definitions
- **Data**: Parameters and variable binding

**Diagram Suggestion:**
```
YAWL Specification Structure:

<specification>
  <decomposition id="root">
    <processControlElements>
      <task id="T1">
        <flowsInto> <nextElementRef ref="T2"/> </flowsInto>
      </task>
      <condition id="C1"> ... </condition>
    </processControlElements>
  </decomposition>
</specification>
```

**Speaker Notes:**
- YAWL emerged from workflow pattern research
- More expressive than BPMN, more formal than BPEL
- CRE extends YAWL with YAML for better ergonomics
- The 43 patterns represent the complete set of workflow control structures

---

## Slide 8: Presentation Roadmap

### What We'll Cover Today

**Bullet Points:**
- **Part 1**: Introduction (you are here)
- **Part 2**: Core Concepts - BCD, patterns, compilation
- **Part 3**: Technical Deep Dive - Petri nets, receipts, verification
- **Part 4**: Case Study - AGI Symposium Omega walkthrough
- **Part 5**: Conclusion - Key takeaways and next steps

**Demo Markers:**
- [DEMO] Basic pattern execution
- [DEMO] YAML compilation
- [DEMO] Receipt inspection
- [DEMO] AGI Symposium simulation

**Learning Objectives:**
By the end of this presentation, you will understand:
1. How generative analysis produces verified workflows
2. The role of Petri nets as formal foundation
3. How 43 patterns compose into complex workflows
4. The architecture enabling production reliability

**Speaker Notes:**
- This roadmap helps orient for different time slots
- 30-minute version will skim technical details
- 90-minute version includes full code walkthroughs
- Demos are optional but highly recommended

---

# Part 2: Core Concepts (10-15 slides)

---

## Slide 9: Core Concept 1 - The Marking Algebra

### Multiset Semantics for Workflow State

**Bullet Points:**
- **Multiset**: Tokens with multiplicity matter
- **Place**: Named location holding tokens
- **Marking**: Complete state = token distribution across all places
- **Operations**: add, take, apply (consume+produce atomically)
- **Canonical hash**: State comparison via cryptographic hash

**The Algebra:**
```erlang
% Create empty marking
M0 = pnet_marking:new([p1, p2, p3]).
% Result: #{p1 => [], p2 => [], p3 => []}

% Add tokens
M1 = pnet_marking:add(M0, #{p1 => [a, b], p2 => [c]}).
% Result: #{p1 => [a, b], p2 => [c], p3 => []}

% Consume tokens (multiset subtraction)
{ok, M2} = pnet_marking:take(M1, #{p1 => [a]}).
% Result: #{p1 => [b], p2 => [c], p3 => []}

% Atomic transition firing
{ok, M3} = pnet_marking:apply(M2, #{p1 => [b]}, #{p3 => [d]}).
% Result: #{p1 => [], p2 => [c], p3 => [d]}
```

**Diagram Suggestion:**
```
Marking State Evolution:

Initial:     #{start => [init], task1 => [], task2 => [], end => []}
              |
              | apply(t_start: init -> task1_token)
              v
After t1:    #{start => [], task1 => [t1_token], task2 => [], end => []}
              |
              | apply(t_task1: t1_token -> task2_token)
              v
After t2:    #{start => [], task1 => [], task2 => [t2_token], end => []}
              |
              | apply(t_task2: t2_token -> end_token)
              v
Final:       #{start => [], task1 => [], task2 => [], end => [end_token]}
```

**Speaker Notes:**
- The marking algebra is the mathematical heart of the system
- Multiset semantics: [a, b] = [b, a] but [a, a] != [a]
- Operations are atomic - no inconsistent intermediate states
- This enables reliable checkpoint and recovery

---

## Slide 10: Core Concept 2 - Pattern Composition

### Building Complex Workflows from Simple Patterns

**Bullet Points:**
- **43 base patterns**: Atomic workflow building blocks
- **Composition**: Patterns nest and combine
- **Hierarchy**: Top-level workflows contain subprocesses
- **Boundaries**: Each pattern has well-defined interface
- **Compilation**: Hierarchical specification -> flat Petri net

**Pattern Categories:**
| Category | Patterns | Purpose |
|----------|----------|---------|
| Basic Control Flow | WCP 01-10 | Sequence, parallel, choice, merge |
| Advanced Synchronization | WCP 07-10 | Discriminator, n-out-of-m |
| Multiple Instances | WCP 11-17 | Static, dynamic, no prior knowledge |
| State-Based | WCP 18-20 | Milestone, cancel |
| Extended Control Flow | WCP 21-28 | Loops, recursion, critical section |
| Data Flow | WDP 01-05 | Parameter passing, transformation |
| Resource Patterns | WRP 01-05 | Allocation, deallocation |
| Exception Handling | WHP 01-05 | Retry, compensation |

**Diagram Suggestion:**
```
Composition Example: Order Fulfillment

[Order Received]
      |
      v
[Parallel Split] ----+----> [Check Inventory]
      |              |
      |              +----> [Process Payment]
      |              |
      |              +----> [Schedule Shipping]
      |
      v
[Synchronization] (wait for all 3)
      |
      v
[Confirm Order]
```

**Speaker Notes:**
- Patterns compose like LEGO blocks
- Each pattern is formally verified in isolation
- Composition preserves properties (with some caveats)
- The AGI Symphony case study shows complex composition

---

## Slide 11: Core Concept 3 - The Compilation Pipeline

### From YAML Specification to Executable Code

**Bullet Points:**
- **Input**: YAML 0.2 or YAWL XML specification
- **Parsing**: wf_yaml_spec extracts structure
- **Validation**: yawl_validate checks constraints
- **Expansion**: yawl_pattern_expander creates Petri net structure
- **Code Generation**: yawl_compile generates gen_pnet module
- **Output**: Executable Erlang module

**Pipeline Stages:**
```
1. Parse YAML/XML
   Input: agi_symposium.yaml
   Output: #yawl_yaml_spec{} record

2. Validate Specification
   Checks: Task connectivity, condition logic, decomposition structure
   Output: {ok, ValidatedSpec}

3. Expand Patterns
   For each pattern instance: Generate places, transitions, arcs
   Output: #yawl_expanded{} record

4. Generate Module
   Create: place_lst/0, trsn_lst/0, fire/3, etc.
   Output: Compiled Erlang source

5. Load and Execute
   Result: gen_yawl process running the workflow
```

**Diagram Suggestion:**
```
+------------+     +------------+     +--------------+     +--------+
 | YAML Spec | --> |  Parser    | --> |  Validator  | --> | Expand |
 +------------+     +------------+     +--------------+     +--------+
                                                                  |
                                                                  v
                                                            +-----------+
                                                            | Compile   |
                                                            +-----------+
                                                                  |
                                                                  v
                                                            +-----------+     +--------+
                                                            | gen_pnet   | --> | Runtime |
                                                            | Module     |     +--------+
                                                            +-----------+
```

**Speaker Notes:**
- Compilation is purely functional - no side effects
- Each stage can be verified independently
- Generated code is human-readable Erlang
- Errors caught at compile time, not runtime

---

## Slide 12: Core Concept 4 - The Pattern Registry

### Mapping Pattern IDs to Implementations

**Bullet Points:**
- **43 patterns**: All indexed by unique ID
- **Registry**: Pure stateless lookup module
- **Validation**: Pattern existence checked at compile time
- **Extensibility**: New patterns follow registration protocol
- **Usage index**: Track which patterns used in specification

**Registry Structure:**
```erlang
% Pattern ID to Module mapping
P1_Sequence  -> sequence
P2_ParallelSplit -> parallel_split
P3_Synchronization -> synchronization
...
P43_ExplicitTermination -> explicit_termination

% Registry functions
yawl_pattern_registry:all_patterns().  % List all 43
yawl_pattern_registry:pattern_module(<<"P1_Sequence">>).  % Get module
yawl_pattern_registry:validate_pattern(<<"P43_ExplicitTermination">>).  % Validate
```

**Diagram Suggestion:**
```
Pattern Registry Lookup:

User Request: "Use P2_ParallelSplit"
        |
        v
+-------------------+
| Pattern Registry  |
+-------------------+
| P1_Sequence      | -> sequence.erl
| P2_ParallelSplit | -> parallel_split.erl  <-- MATCH
| P3_Sync          | -> synchronization.erl
| ...              |
| P43_ExplicitTerm | -> explicit_termination.erl
+-------------------+
        |
        v
Result: parallel_split module loaded
```

**Speaker Notes:**
- The registry is the pattern library index
- Enables dynamic pattern selection from YAML
- Validation happens before code generation
- Registry is pure functional - no runtime state

---

## Slide 13: Core Concept 5 - Mode Enumeration

### Deterministic Nondeterminism

**Bullet Points:**
- **Problem**: Transitions may fire in multiple modes (token combinations)
- **Enumeration**: Generate all possible binding combinations
- **Selection**: Pick one mode (deterministically via seed)
- **Lib_combin**: Pure combinator library for mode generation
- **Reproducibility**: Same seed = same execution path

**Mode Enumeration Example:**
```erlang
% Transition has two input places with multiple tokens
Mode 1: #{p1 => [token_a], p2 => [token_x]}
Mode 2: #{p1 => [token_a], p2 => [token_y]}
Mode 3: #{p1 => [token_b], p2 => [token_x]}
Mode 4: #{p1 => [token_b], p2 => [token_y]}

% Pick one mode deterministically
Selected = lib_combin:pick_from([Mode1, Mode2, Mode3, Mode4], Seed).
```

**Diagram Suggestion:**
```
Mode Enumeration:

Transition t_process with preset [p_in1, p_in2]

Marking:
  p_in1 => [a, b]
  p_in2 => [x, y]

Enumerated Modes:
  Mode1: #{p_in1 => [a], p_in2 => [x]}
  Mode2: #{p_in1 => [a], p_in2 => [y]}
  Mode3: #{p_in1 => [b], p_in2 => [x]}
  Mode4: #{p_in1 => [b], p_in2 => [y]}

is_enabled(t_process, Mode1, UsrInfo) -> true | false
```

**Speaker Notes:**
- "Deterministic nondeterminism" sounds contradictory but makes sense
- We enumerate all possibilities, then pick one consistently
- Essential for reproducible testing and debugging
- Same seed always yields same execution path

---

## [DEMO] Slide 14: Live Demo - Basic Pattern Execution

### Seeing the Marking Algebra in Action

**Demo Setup:**
```erlang
% Start Erlang shell
rebar3 shell

% Create a simple sequence workflow
{ok, Pid} = sequence:start_link(#{}, []).

% Inspect initial marking
{ok, Marking} = gen_pnet:marking(Pid).

% Execute one step
{ok, Receipt} = gen_pnet:step(Pid).

% Examine the receipt
% See what changed
```

**What to Observe:**
- Initial marking: empty places
- After step: tokens moved through transitions
- Receipt shows: which transition fired, mode used, tokens produced
- State is immutable: each step produces new marking

**Speaker Notes:**
- [Perform live demo here]
- Point out the marking algebra in action
- Show the receipt structure
- Emphasize: no hidden state, everything is visible

---

## Slide 15: Core Concept 6 - The Receipt System

### Complete Audit Trail of Every Transition

**Bullet Points:**
- **What**: Record of every transition firing
- **Content**: Transition ID, mode, tokens consumed/produced, timestamp
- **Purpose**: Audit trail, debugging, replay, recovery
- **Format**: Structured Erlang term (serializable)
- **Accumulation**: Receipts form complete execution trace

**Receipt Structure:**
```erlang
#receipt{
    transition = t_parallel_split,
    mode = #{p_start => [init_token]},
    consumed = #{p_start => [init_token]},
    produced = #{
        p_branch1 => [{branch, 1}],
        p_branch2 => [{branch, 2}]
    },
    timestamp = 1678123456789
}
```

**Diagram Suggestion:**
```
Receipt Chain:

[Initial State]
    |
    | Receipt1: t_start fired
    v
[State 1] -----------> [Receipt Log: [Receipt1]]
    |
    | Receipt2: t_split fired
    v
[State 2] -----------> [Receipt Log: [Receipt1, Receipt2]]
    |
    | Receipt3: t_task1 fired
    v
[State 3] -----------> [Receipt Log: [Receipt1, Receipt2, Receipt3]]
```

**Speaker Notes:**
- Receipts are the key to observability
- Every state transition is documented
- Enables precise replay for debugging
- Foundation for checkpoint/recovery

---

## Slide 16: Core Concept 7 - Trigger/Filter Mechanism

### Side Effects Without Breaking Purity

**Bullet Points:**
- **Challenge**: Workflow needs side effects (logging, external calls)
- **Solution**: Separate pure computation from effects
- **Trigger callback**: Executed after tokens produced
- **Filter mechanism**: Tokens can be dropped after trigger
- **Purity preserved**: fire/3 remains pure function

**Trigger/Filter Flow:**
```
1. Transition fires (pure computation)
   fire(Transition, Mode, UsrInfo) -> {produce, ProduceMap}

2. Tokens produced (pure state update)
   NewMarking = pnet_marking:add(CurrentMarking, ProduceMap)

3. Trigger executed (side effects here!)
   trigger(Transition, Mode, UsrInfo) -> ok
   - Log event
   - Send message
   - Update external system

4. Filter applied (token acceptance)
   trigger(Transition, Mode, UsrInfo) -> pass | drop
   pass: Token enters marking
   drop: Token discarded
```

**Diagram Suggestion:**
```
Pure Computation vs Side Effects:

fire/3 (PURE):
  Input: Mode, UsrInfo
  Output: ProduceMap
  [No side effects]

trigger/3 (EFFECTS):
  Input: Transition, Mode, UsrInfo
  Output: pass | drop
  [Side effects OK here]
```

**Speaker Notes:**
- The trigger/callback mechanism is brilliant design
- Keeps the core pure while enabling real-world effects
- Filter allows tokens to be conditionally dropped
- This is how external integration happens

---

## Slide 17: Core Concept 8 - Checkpoint and Recovery

### Fault Tolerance Through Receipt-Based Recovery

**Bullet Points:**
- **Checkpoint**: Save marking + usr_info at any point
- **Recovery**: Restore from checkpoint, replay from receipt log
- **No state loss**: All transitions documented in receipts
- **Exactly-once semantics**: No duplicate or missed transitions
- **Fault tolerance**: Process crashes don't lose workflow state

**Recovery Protocol:**
```
Normal Execution:
  State0 --[Receipt1]--> State1 --[Receipt2]--> State2 --[Receipt3]--> State3

Crash at State3:
  [Process dies]

Recovery:
  1. Load checkpoint (State2 + ReceiptLog)
  2. Verify receipts match
  3. Resume from State2
  4. Replay Receipt3 if not confirmed
  5. Continue execution
```

**Diagram Suggestion:**
```
Checkpoint Timeline:

Time -->  t0        t1        t2        t3        t4
          |         |         |         |         |
State:    S0 ------> S1 ------> S2 ------> S3 ------> S4
          |         |         |         |         |
Receipt:  R1        R2        R3        R4

Checkpoints saved at t1, t3:
  - CP1: {state=S2, receipts=[R1,R2]}
  - CP2: {state=S4, receipts=[R1,R2,R3,R4]}

Recovery from CP1:
  - Restore S2
  - Replay R3, R4 to reach S4
```

**Speaker Notes:**
- Recovery is a key benefit of the receipt system
- Checkpoint cost: serialize marking and receipts
- Recovery: verify integrity, then resume
- Enables truly robust distributed workflows

---

## Slide 18: Core Concept 9 - OpenTelemetry Integration

### Observability for Production Workflows

**Bullet Points:**
- **Spans**: Each transition firing creates a span
- **Traces**: Complete workflow execution as distributed trace
- **Metrics**: Token counts, transition counts, FPS (steps/sec)
- **Logs**: Structured logging at each stage
- **Visualization**: Jaeger, Grafana, or custom dashboard

**Telemetry Data:**
```erlang
% Span attributes
#{
    workflow_id := <<"agi_symposium_omega">>,
    transition := <<"t_parallel_split">>,
    mode := <<"#{p_start => [init]}">>,
    tokens_produced := 2,
    tokens_consumed := 1,
    duration_ms := 5
}
```

**Diagram Suggestion:**
```
Distributed Trace:

[Span: Start Case] --[Span: Parallel Split]--+--> [Span: Branch 1 Task]
                                               |           |
                                               +--> [Span: Branch 2 Task]
                                               |           |
                                               +--> [Span: Branch 3 Task]
                                                           |
                                                           v
                                                 [Span: Synchronization]
                                                           |
                                                           v
                                                    [Span: Complete Case]
```

**Speaker Notes:**
- OpenTelemetry is standard for observability
- CRE integrates OTEL for production monitoring
- Every transition firing is visible
- Essential for debugging complex workflows

---

## Slide 19: Core Concept 10 - XES Event Logging

### Process Mining Compliance

**Bullet Points:**
- **XES standard**: IEEE standard for event logs
- **Process mining**: Extract process models from execution logs
- **Conformance checking**: Verify actual execution matches specification
- **Audit compliance**: Regulatory requirement for many domains
- **Tool ecosystem**: ProM, Disco, Celonis for analysis

**XES Event Structure:**
```xml
<trace>
  <event>
    <string key="concept:name" value="Start Review"/>
    <date key="time:timestamp" value="2026-02-07T10:00:00"/>
    <string key="lifecycle:transition" value="complete"/>
  </event>
  <event>
    <string key="concept:name" value="Approve"/>
    <date key="time:timestamp" value="2026-02-07T10:05:00"/>
  </event>
</trace>
```

**Diagram Suggestion:**
```
XES Logging Integration:

[Transition Fires]
        |
        v
[Receipt Generated]
        |
        +---> [Receipt Log] (internal)
        |
        v
[XES Logger]
        |
        v
[XES File] --[Process Mining Tool]--> [Process Discovery]
                                          |
                                          v
                                    [Conformance Analysis]
```

**Speaker Notes:**
- XES is the standard for process mining
- CRE automatically generates XES logs
- Enables discovery of actual process behavior
- Useful for audit and compliance

---

## Slide 20: Core Concept Summary

### The Generative Analysis Stack

**Bullet Points:**
- **Specification Layer**: YAML/XML YAWL definitions
- **Analysis Layer**: Validation, pattern verification, mode enumeration
- **Generation Layer**: Code generation for gen_pnet
- **Runtime Layer**: gen_yawl execution with receipts
- **Observability Layer**: OTEL, XES, dashboard

**The Stack in One View:**
```
+----------------------------------------------------------------+
|                    Specification (YAML/XML)                    |
+----------------------------------------------------------------+
                                 |
                                 v
+----------------------------------------------------------------+
|     Analysis (Validate | Expand | Generate | Verify)           |
+----------------------------------------------------------------+
                                 |
                                 v
+----------------------------------------------------------------+
|                     Generated Code (gen_pnet)                  |
+----------------------------------------------------------------+
                                 |
                                 v
+----------------------------------------------------------------+
|          Runtime (gen_yawl) + Observability (OTEL/XES)         |
+----------------------------------------------------------------+
```

**Key Takeaway:**
Generative analysis separates concerns: specify WHAT, analyze properties, generate HOW, observe execution.

**Speaker Notes:**
- This slide ties together all core concepts
- Each layer has distinct responsibility
- Clear boundaries enable independent evolution
- The stack is modular and extensible

---

# Part 3: Technical Deep Dive (15-20 slides)

---

## Slide 21: gen_pnet - The Single OTP Runner

### Deep Dive into the Core Behavior

**Bullet Points:**
- **Behavior**: gen_server (OTP standard)
- **State**: #net_state{net_mod, usr_info, marking, stats, ...}
- **Progress Loop**: Automatically fires enabled transitions
- **API**: step/1, inject/2, drain/2, marking/1, stats/1
- **Extensibility**: Callbacks for customization

**Callback Structure:**
```erlang
%% Net structure callbacks (define the Petri net)
place_lst() -> [p1, p2, p3].
trsn_lst() -> [t1, t2, t3].
init_marking(Place, UsrInfo) -> [initial_token].
preset(Transition) -> [input_places].

%% Execution callbacks
is_enabled(Transition, Mode, UsrInfo) -> boolean().
fire(Transition, Mode, UsrInfo) -> {produce, ProduceMap}.

%% Interface callbacks (OTP standard)
init(NetArg) -> {ok, UsrInfo}.
handle_call(Request, From, State) -> {reply, Reply, State}.
terminate(Reason, State) -> ok.
trigger(Transition, Mode, UsrInfo) -> pass | drop.
```

**Progress Loop Algorithm:**
```
1. Find all enabled transitions
   - For each transition: enumerate modes from preset
   - Check is_enabled/3 for each mode
   - Collect {Transition, Mode} pairs

2. Pick one enabled transition
   - Use deterministic choice (lib_combin:pick_from)

3. Fire the transition
   - Call fire/3 to get produce map

4. Update marking
   - Consume tokens from mode
   - Add produced tokens

5. Execute triggers
   - Call trigger/3 for each produced token
   - Filter based on pass/drop result

6. Update statistics
   - Track steps, tokens, FPS

7. Repeat until no enabled transitions
```

**Speaker Notes:**
- gen_pnet is the heart of the system
- Only place with mutable state
- Progress loop runs automatically
- Single source of truth for workflow state

---

## Slide 22: gen_yawl - The Enhanced Wrapper

### Adding 3-Tuple fire/3 Support

**Bullet Points:**
- **Purpose**: Extend gen_pnet with usr_info updates
- **Enhancement**: fire/3 can return {produce, Map, NewUsrInfo}
- **Automation**: usr_info automatically updated in net_state
- **Backward compatible**: 2-tuple {produce, Map} still works
- **Essential for**: Stateful workflow patterns

**The 3-Tuple Innovation:**
```erlang
%% Standard gen_pnet (no usr_info update)
fire(Trsn, Mode, UsrInfo) ->
    {produce, #{p_out => [new_token]}}.

%% Enhanced gen_yawl (usr_info update)
fire(Trsn, Mode, UsrInfo) ->
    NewUsrInfo = UsrInfo#pattern_state{
        completed = [Trsn | UsrInfo#pattern_state.completed],
        timestamp = erlang:system_time(millisecond)
    },
    {produce, #{p_out => [new_token]}, NewUsrInfo}.
```

**State Update Flow:**
```
Before gen_yawl:
  fire/3 returns {produce, Map}
  UsrInfo unchanged (manual handle_call needed)

With gen_yawl:
  fire/3 returns {produce, Map, NewUsrInfo}
  NetState automatically updated:
    #net_state{usr_info = NewUsrInfo}
```

**Speaker Notes:**
- gen_yawl wraps gen_pnet with a small but powerful extension
- The 3-tuple return eliminates boilerplate
- Essential for complex patterns that track state
- Shows thoughtful API design

---

## Slide 23: Pattern Implementation Structure

### How Patterns Are Built

**Bullet Points:**
- **Module**: Each pattern = separate gen_yawl module
- **Places**: State locations (p_start, p_end, p_*)
- **Transitions**: Actions (t_*)
- **Preset**: Wiring transitions to input places
- **Fire logic**: Token production/consumption rules

**Pattern Template:**
```erlang
-module(my_pattern).
-behaviour(gen_yawl).

%% State record
-record(my_pattern_state, {
    config :: map(),
    completed = [] :: list(),
    start_time :: integer()
}).

%% Places
place_lst() -> [p_start, p_processing, p_done, p_end].

%% Transitions
trsn_lst() -> [t_start, t_process, t_complete].

%% Initial marking
init_marking(p_start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

%% Preset (wiring)
preset(t_start) -> [p_start];
preset(t_process) -> [p_processing];
preset(t_complete) -> [p_done].

%% Enablement
is_enabled(t_start, #{p_start := [init]}, _UsrInfo) -> true;
is_enabled(t_process, #{p_processing := [work]}, _UsrInfo) -> true;
is_enabled(t_complete, #{p_done := [result]}, _UsrInfo) -> true;
is_enabled(_, _, _) -> false.

%% Firing
fire(t_start, #{p_start := []}, #my_pattern_state{config = Cfg}) ->
    WorkItem = create_work(Cfg),
    {produce, #{p_start => [], p_processing => [WorkItem]}};

fire(t_process, #{p_processing := [Work]}, _UsrInfo) ->
    Result = process_work(Work),
    {produce, #{p_processing => [], p_done => [Result]}};

fire(t_complete, #{p_done := [Result]}, _UsrInfo) ->
    {produce, #{p_done => [], p_end => [complete]}}.
```

**Diagram Suggestion:**
```
Pattern Petri Net:

[p_start] --t_start--> [p_processing] --t_process--> [p_done] --t_complete--> [p_end]
```

**Speaker Notes:**
- Every pattern follows this structure
- Clear separation of places, transitions, logic
- State record tracks pattern-specific data
- fire/3 where the pattern logic lives

---

## Slide 24: Pattern Categories Deep Dive

### Understanding the 43 Patterns

**Basic Control Flow (WCP 01-10):**
- **Sequence**: Linear task execution
- **Parallel Split**: Fork into concurrent branches
- **Synchronization**: Wait for all branches
- **Exclusive Choice**: XOR branch selection
- **Simple Merge**: XOR convergence
- **Multiple Choice**: OR branch selection
- **Synchronizing Merge**: Complex merge semantics
- **Multi-Merge**: Collect from multiple paths
- **Discriminator**: First-completion trigger
- **N-out-of-M**: Quorum-based completion

**Multiple Instances (WCP 11-17):**
- **Implicit Termination**: Auto-terminate when done
- **Without Synchronization**: Fire-and-forget instances
- **Static**: Fixed instance count (design time)
- **Runtime**: Instance count determined at runtime
- **Without Prior Knowledge**: Dynamic instance creation

**State-Based (WCP 18-20):**
- **Milestone**: Guard activity behind condition
- **Cancel Activity**: Individual task cancellation
- **Cancel Case**: Entire workflow cancellation

**Diagram Suggestion:**
```
Pattern Family Tree:

Workflow Patterns (43)
    |
    +-- Basic Control (10)
    |    +-- Sequence
    |    +-- Parallel (Split, Sync)
    |    +-- Choice (Exclusive, Multiple)
    |    +-- Merge (Simple, Sync, Multi)
    |    +-- Discriminator, N-of-M
    |
    +-- Multiple Instance (7)
    |    +-- Implicit Termination
    |    +-- No Sync / Static / Runtime
    |    +-- No Prior Knowledge
    |    +-- Deferred Choice, Interleaved
    |
    +-- State-Based (3)
    |    +-- Milestone, Cancel Activity/Case
    |
    +-- Advanced (8)
    +-- Data Flow (5)
    +-- Resource (5)
    +-- Exception (5)
```

**Speaker Notes:**
- The 43 patterns cover every known workflow structure
- Categorized by functionality
- Each pattern has proven Petri net semantics
- Together they form a complete workflow language

---

## Slide 25: Basic Patterns in Detail

### Sequence, Parallel Split, Exclusive Choice

**Sequence (P1):**
```erlang
% Linear execution: A -> B -> C
place_lst() -> [p_start, p_a, p_b, p_c, p_end].
fire(t_a, _, _) -> {produce, #{p_a => [], p_b => [b_token]}}.
fire(t_b, _, _) -> {produce, #{p_b => [], p_c => [c_token]}}.
fire(t_c, _, _) -> {produce, #{p_c => [], p_end => [done]}}.
```

**Parallel Split (P2):**
```erlang
% Fork: A -> (B, C, D)
fire(t_split, _, _) ->
    {produce, #{
        p_start => [],
        p_branch1 => [b1],
        p_branch2 => [b2],
        p_branch3 => [b3]
    }}.
```

**Exclusive Choice (P4):**
```erlang
% Choose one branch based on condition
is_enabled(t_branch_a, _, #state{condition = cond_a}) -> cond_a;
is_enabled(t_branch_b, _, #state{condition = cond_b}) -> cond_b;
% Only one branch fires
```

**Diagram Suggestion:**
```
Three Basic Patterns:

Sequence:      A --> B --> C

Parallel:      A -->+--> B
               +--> C
               +--> D

Exclusive:     A --> B   (only if condition A)
               --> C   (only if condition B)
```

**Speaker Notes:**
- These three are the foundation
- Most workflows composed of these primitives
- Understanding these enables understanding all patterns
- Examples show the power of simple composition

---

## Slide 26: Advanced Synchronization Patterns

### Discriminator, N-out-of-M, Partial Join

**Discriminator (P9):**
- Trigger on FIRST completion
- Ignore subsequent completions
- Reset after all branches complete

**N-out-of-M (P10):**
- Wait for N of M branches
- Configurable quorum
- Used for voting, consensus

**Partial Join (P30-P36):**
- Multiple variants (blocking, cancelling, static, dynamic)
- Complex coordination semantics
- Optimizes synchronization

**Diagram Suggestion:**
```
Discriminator vs N-out-of-M:

Discriminator (first-completion-wins):
  Branch1 ----+
  Branch2 ----+--> [Trigger] --> Continue
  Branch3 ----+     (ignores late arrivals)

N-out-of-M (quorum-based, N=2):
  Branch1 ----+
  Branch2 ----+--> [Quorum Check] --> Continue
  Branch3 ----+     (waits for 2 of 3)
```

**Speaker Notes:**
- Advanced patterns handle complex coordination
- Discriminator: racing branches
- N-of-M: quorum-based decisions
- Partial joins: optimization for many branches

---

## Slide 27: Multiple Instance Patterns

### Static, Dynamic, and Runtime Instances

**Static Multiple Instance (P13):**
```erlang
% Fixed number of instances known at design time
fire(t_spawn, _, #state{instance_count = N, input_data = Data}) ->
    Instances = lists:map(fun(I) -> {instance, I, Data} end,
                         lists:seq(1, N)),
    {produce, #{p_ready => Instances}}.
```

**Runtime Multiple Instance (P14):**
```erlang
% Count determined from input data at runtime
fire(t_eval_count, _, #state{input_data = Data}) ->
    Count = determine_instance_count(Data),
    {produce, #{p_instance_pool => lists:duplicate(Count, spawn_token)}, ...
```

**Without Prior Knowledge (P15):**
```erlang
% Dynamically spawn instances as data arrives
handle_info({data_available, NewData}, State) ->
    {ok, NewState} = spawn_instance(NewData, State),
    {noreply, NewState}.
```

**Diagram Suggestion:**
```
Instance Creation Strategies:

Static:      [Data] -> [Spawn N] -> N instances

Runtime:     [Data] -> [Eval Count] -> [Spawn N] -> N instances
                       (N = f(Data))

No Prior:    [Data Stream] -> [Spawn 1] -> 1 instance
                          -> [Spawn 1] -> 1 instance
                          -> ... (continuous)
```

**Speaker Notes:**
- Multiple instance patterns enable parallelism
- Static: predictable parallelism
- Runtime: adaptive parallelism
- No prior: streaming parallelism
- Each has different trade-offs

---

## Slide 28: State-Based Patterns

### Milestone, Cancellation

**Milestone (P18):**
```erlang
% Activity only enabled if milestone reached
is_enabled(t_activity, _, #state{milestone_reached = true}) ->
    true;
is_enabled(t_activity, _, #state{milestone_reached = false}) ->
    false.
```

**Cancel Activity (P19):**
```erlang
% Cancel individual running activity
handle_cast({cancel, ActivityId}, State) ->
    case maps:get(ActivityId, State#state.running_activities) of
        undefined -> {noreply, State};
        Pid ->
            gen_yawl:stop(Pid),
            {noreply, cleanup_activity(ActivityId, State)}
    end.
```

**Cancel Case (P20):**
```erlang
% Cancel entire workflow case
handle_cast(cancel_case, #state{case_pid = CasePid}) ->
    gen_yawl:stop(CasePid),
    cleanup_all_resources(State),
    {noreply, #state{cancelled = true}}.
```

**Diagram Suggestion:**
```
Milestone Pattern:

[Milestone Reached?] --[No]--> [Activity Disabled]
        |
       [Yes]
        |
        v
[Activity Enabled] --> [Execute] --> [Continue]

Cancellation:

[Running Activity] --[Cancel Signal]--> [Stop]--> [Cleanup]
```

**Speaker Notes:**
- State-based patterns react to workflow state
- Milestone: conditional execution
- Cancellation: emergency stop or user abort
- Essential for real-world workflows

---

## Slide 29: Data Flow Patterns

### Parameter Passing, Transformation, Distribution

**Parameter Passing (WDP-01):**
```erlang
% Pass data between tasks
fire(t_producer, _, _) ->
    {produce, #{p_out => [{data, Key, Value}]}}.

fire(t_consumer, #{p_in := [{data, Key, Value}]}, _) ->
    consume_data(Key, Value),
    {produce, #{p_in => [], p_next => [done]}}.
```

**Data Transformation (WDP-02):**
```erlang
% Transform data between tasks
fire(t_transform, #{p_in := [RawData]}, #state{transform_fn = Fn}) ->
    Transformed = Fn(RawData),
    {produce, #{p_in => [], p_out => [Transformed]}}.
```

**Data Distribution (WDP-03):**
```erlang
% Distribute to multiple recipients
fire(t_distribute, _, #state{recipients = Recipients, data = Data}) ->
    Tokens = [{recipient, R, Data} || R <- Recipients],
    {produce, maps:from_list([{p_recipients, Tokens}])}.
```

**Diagram Suggestion:**
```
Data Flow Patterns:

Param Pass:     [Task A] --data--> [Task B]

Transform:      [Raw] --> [Transform Fn] --> [Processed]

Distribute:     [Source] --+--> [Recipient A]
                             +--> [Recipient B]
                             +--> [Recipient C]

Accumulate:     [Task A] --+
                 [Task B] --+--> [Accumulator] --> [Aggregated]
                 [Task C] --+
```

**Speaker Notes:**
- Data patterns complement control patterns
- Parameter passing: basic data flow
- Transformation: format conversion
- Distribution: parallel processing
- Accumulation: result aggregation

---

## Slide 30: Resource and Exception Patterns

### Allocation, Retry, Compensation

**Resource Allocation (WRP-04):**
```erlang
% Allocate resource from pool
is_enabled(t_acquire, _, #state{available_resources = Available}) ->
    length(Available) > 0.

fire(t_acquire, _, #state{available_resources = [R|Rest]}) ->
    {produce, #{p_resource => [R]},
     #state{available_resources = Rest, allocated = [R|Allocated]}}.
```

**Retry (WHP-02):**
```erlang
% Retry with exponential backoff
fire(t_retry, _, #state{attempt = N, max_retries = Max, error = Error})
  when N < Max ->
    Delay = trunc(math:pow(2, N) * 1000),
    erlang:send_after(Delay, self(), {retry_attempt, N + 1}),
    {produce, #{p_waiting => [retry_token]},
     #state{attempt = N + 1}}.
```

**Compensation (WHP-03):**
```erlang
% Compensating transaction
fire(t_compensate, _, #state{completed = Completed, error = _Error}) ->
    Compensated = lists:map(fun compensate_task/1, lists:reverse(Completed)),
    {produce, #{p_cleanup => Compensated}}.
```

**Diagram Suggestion:**
```
Exception Handling:

Retry:      [Task] --[Fail]--> [Backoff]--> [Retry]
                             |
                             v
                          [Max Retries?] --[Yes]--> [Fail]

Compensate: [Task A] [Task B] [Task C]
                            |
                         [Error]
                            v
                   [Compensate C] --> [Compensate B] --> [Compensate A]
```

**Speaker Notes:**
- Resource patterns: manage external resources
- Exception patterns: handle errors gracefully
- Retry: transient failures
- Compensation: undo completed work (SAGA pattern)

---

## Slide 31: YAML 0.2 Specification Format

### Human-Readable Workflow Definitions

**Bullet Points:**
- **Ergonomics**: YAML easier than XML
- **Version 0.2**: CRE extension to YAWL
- **Pattern instances**: Direct pattern references
- **Decomposition**: Hierarchical workflows
- **Data binding**: Variable mapping

**YAML Structure:**
```yaml
yawl_yaml_version: "0.2"
spec_id: "agi_symposium_omega"
spec_name: "AGI Symposium Omega Conference"

root_decomposition:
  name: "conference_planning"
  tasks:
    - task_id: "setup_committee"
      pattern: "P1_Sequence"
      next: ["call_for_papers"]

    - task_id: "review_process"
      pattern: "P13_MultipleInstanceStatic"
      config:
        instance_count: 3
      next: ["accept_papers"]

    - task_id: "parallel_tracks"
      pattern: "P2_ParallelSplit"
      branches:
        - task_id: "track_research"
        - task_id: "track_tutorial"
        - task_id: "track_workshop"
```

**Diagram Suggestion:**
```
YAML to Execution:

[YAML File]
      |
      v
[Parse] --> [#yawl_yaml_spec record]
      |
      v
[Validate] --> [Pattern Registry Check]
      |
      v
[Expand] --> [Petri Net Structure]
      |
      v
[Compile] --> [gen_pnet Module]
      |
      v
[Execute]
```

**Speaker Notes:**
- YAML 0.2 makes workflow authoring accessible
- No need to write Erlang directly
- Pattern names map to registry
- Validation catches errors before execution

---

## Slide 32: The Pattern Expander

### From Pattern Instance to Petri Net Structure

**Bullet Points:**
- **Input**: Pattern instance with configuration
- **Output**: Places, transitions, arcs
- **Pure function**: No side effects
- **Composable**: Nested patterns work
- **Deterministic**: Same input = same output

**Expansion Process:**
```erlang
% Input: Pattern instance
#pattern_instance{
    id = <<"parallel_review">>,
    pattern = <<"P2_ParallelSplit">>,
    config => #{
        branches => [
            #{id => <<"reviewer_1">>},
            #{id => <<"reviewer_2">>},
            #{id => <<"reviewer_3">>}
        ]
    }
}

% Output: Expanded structure
#yawl_expanded{
    places => [p_start, p_branch1, p_branch2, p_branch3, p_sync, p_end],
    transitions => [t_split, t_branch1, t_branch2, t_branch3, t_sync],
    arcs => [
        {arc, p_start, t_split},
        {arc, t_split, p_branch1},
        {arc, t_split, p_branch2},
        {arc, t_split, p_branch3},
        {arc, p_branch1, t_branch1},
        {arc, p_branch2, t_branch2},
        {arc, p_branch3, t_branch3},
        {arc, t_branch1, p_sync},
        {arc, t_branch2, p_sync},
        {arc, t_branch3, p_sync},
        {arc, p_sync, t_sync},
        {arc, t_sync, p_end}
    ]
}
```

**Speaker Notes:**
- Pattern expander is the "compiler" from patterns to Petri nets
- Pure functional transformation
- Outputs complete net structure
- This is what enables code generation

---

## Slide 33: Code Generation

### From Expanded Net to Executable Module

**Bullet Points:**
- **Input**: Expanded Petri net structure
- **Output**: Erlang source file
- **Template**: gen_pnet behavior module
- **Callbacks**: All required callbacks generated
- **Human-readable**: Generated code is readable

**Generated Module Example:**
```erlang
%%% Generated by yawl_compile
%%% DO NOT EDIT MANUALLY

-module('agi_symposium_omega').
-behaviour(gen_pnet).

%% Place definitions
place_lst() ->
    [p_start, p_setup, p_cfp, p_review, p_accept, p_program, p_end].

%% Transition definitions
trsn_lst() ->
    [t_setup, t_cfp, t_review_start, t_review_instance, t_accept,
     t_program, t_finalize].

%% Initial marking
init_marking(p_start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

%% Preset (wiring)
preset(t_setup) -> [p_start];
preset(t_cfp) -> [p_setup];
preset(t_review_start) -> [p_cfp];
preset(t_review_instance) -> [p_review];
preset(t_accept) -> [p_review];
preset(t_program) -> [p_accept];
preset(t_finalize) -> [p_program].

%% Enablement (simplified)
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

%% Firing (simplified)
fire(t_setup, #{p_start := []}, _) ->
    {produce, #{p_start => [], p_setup => [setup_done]}};
%% ... (each transition implemented)
```

**Diagram Suggestion:**
```
Code Generation Flow:

[Expanded Net]
      |
      v
[Generate Module Header]
      |
      v
[Generate place_lst/0]
      |
      v
[Generate trsn_lst/0]
      |
      v
[Generate init_marking/2]
      |
      v
[Generate preset/1]
      |
      v
[Generate fire/3 for each transition]
      |
      v
[Write Erlang File]
      |
      v
[Load Module]
```

**Speaker Notes:**
- Code generation is straightforward transformation
- Each callback generated from net structure
- Generated code is readable and debuggable
- No "magic" in generated output

---

## Slide 34: [DEMO] Live Compilation Demo

### Seeing YAML Become Executable Code

**Demo Setup:**
```erlang
% Load YAML specification
{ok, Spec} = wf_yaml_spec:from_yaml_file("agi_symposium_omega.yaml").

% Validate specification
{ok, Warnings} = yawl_validate:validate(Spec).

% Compile to module
{ok, Compiled} = yawl_compile:compile(Spec, #{}).

% Inspect generated code
code:which(agi_symposium_omega).  % Show module location
```

**What to Observe:**
- YAML parsing: structure extraction
- Validation: pattern checks, connectivity
- Compilation: module generation
- Generated code: readable Erlang

**Speaker Notes:**
- [Perform live demo here]
- Show the YAML file
- Walk through each compilation stage
- Display generated code
- Emphasize: this is generative analysis in action

---

## Slide 35: Verification Methods

### How We Ensure Correctness

**Bullet Points:**
- **Static Analysis**: Compile-time property checking
- **State Exploration**: Enumerate all reachable states
- **Receipt Verification**: Post-execution verification
- **Property Testing**: QuickCheck-style testing
- **Concurrent Testing**: Concuerror for race detection

**Verification Techniques:**

**1. Structural Validation:**
```erlang
yawl_validate:validate(Spec) ->
    check_connectedness(Spec),
    check_condition_logic(Spec),
    check_decomposition_validity(Spec),
    check_pattern_usage(Spec).
```

**2. State Space Analysis:**
```erlang
% Enumerate all reachable states
analyze_state_space(NetMod) ->
    InitialMarking = get_initial_marking(NetMod),
    Reachable = explore_states(InitialMarking, []),
    verify_properties(Reachable).
```

**3. Receipt Verification:**
```erlang
% Verify receipt chain is valid
verify_receipts(Receipts, InitialMarking, FinalMarking) ->
    ComputedMarking = apply_receipts(InitialMarking, Receipts),
    case ComputedMarking of
        FinalMarking -> {ok, valid};
        _ -> {error, receipt_mismatch}
    end.
```

**Diagram Suggestion:**
```
Verification Layers:

[Specification] --[Static Analysis]--> [Errors Found?] --[No]--> [Compile]
                                                 |
                                                [Yes]
                                                 |
                                                 v
                                           [Fix Errors]

[Compiled Code] --[State Exploration]--> [Properties Hold?] --[Yes]--> [Deploy]
                                                      |
                                                     [No]
                                                      |
                                                      v
                                                [Fix Specification]
```

**Speaker Notes:**
- Verification happens at multiple levels
- Static: before code runs
- Dynamic: during execution
- Post-mortem: receipt analysis
- Multi-layered defense against bugs

---

## Slide 36: Performance Characteristics

### Bottlenecks and Optimizations

**Bullet Points:**
- **Marking operations**: O(m) where m = tokens affected
- **Mode enumeration**: O(n * k) where n = tokens, k = combinations
- **Receipt storage**: O(t) where t = total transitions fired
- **State snapshots**: O(p) where p = places in net
- **FPS tracking**: steps/second measurement

**Optimization Techniques:**
```erlang
% 1. Persistent terms for frequently accessed data
-define(PATTERN_REGISTRY, persistent_term:get(?MODULE)).

% 2. ETS for large markings (when token count > threshold)
-define(LARGE_MARKING_THRESHOLD, 1000).

% 3. Binary term storage for receipts
Receipts = [term_to_binary(R) || R <- ReceiptList].

% 4. Hash-based state comparison
StateHash = pnet_marking:hash(Marking),
```

**Performance Benchmarks:**
| Operation | Small Net | Medium Net | Large Net |
|-----------|-----------|------------|-----------|
| Marking update | < 1us | 1-5us | 5-50us |
| Mode enumeration | < 10us | 10-100us | 100-1000us |
| Transition firing | 10-50us | 50-500us | 500-5000us |
| State snapshot | < 1us | 1-10us | 10-100us |

**Speaker Notes:**
- Performance scales with net size
- Most workflows are small-medium
- Optimizations target large nets
- FPS (frames/steps per second) is key metric

---

## Slide 37: Distributed Execution

### Running Workflows Across Clusters

**Bullet Points:**
- **Erlang distribution**: Built-in transparent distribution
- **Node location**: Workflows can run on any node
- **Migration**: Live workflow migration between nodes
- **Fault tolerance**: Automatic failover
- **Observability**: Distributed tracing across nodes

**Distribution Architecture:**
```erlang
% Start workflow on specific node
{ok, Pid} = gen_yawl:start_link(WorkflowMod, InitArg,
    [{node, 'worker@host1'}]).

% Migrate workflow to another node
gen_yawl:migrate(Pid, 'worker@host2').

% Distributed pattern execution
% Each branch of parallel split runs on different node
fire(t_distribute, _, _) ->
    Nodes = [node1@host1, node2@host2, node3@host3],
    BranchPids = [start_branch_on_node(B, N) || {B, N} <- zip(Branches, Nodes)],
    {produce, #{p_branches => BranchPids}}.
```

**Diagram Suggestion:**
```
Distributed Workflow:

[Coordinator Node]
        |
        +---> [Branch 1] @ worker1
        |
        +---> [Branch 2] @ worker2
        |
        +---> [Branch 3] @ worker3
        |
        v
[Synchronization] @ coordinator
```

**Speaker Notes:**
- Erlang/OTP designed for distribution
- Workflows naturally distribute
- Each branch can run on different node
- Automatic failure detection and recovery

---

## Slide 38: Human-in-the-Loop Integration

### LLM-Powered Approval Workflows

**Bullet Points:**
- **Approval checkpoints**: Wait for human decision
- **LLM assistance**: AI analyzes and recommends
- **Web dashboard**: Human interface for decisions
- **Audit trail**: All decisions recorded
- **Compensation**: Undo capability for changes

**Approval Pattern:**
```erlang
% Wait for human approval
fire(t_request_approval, _, _) ->
    ApprovalToken = {approval_required, TaskId, Context},
    {produce, #{p_approval_queue => [ApprovalToken]}}.

% Human decision (via web dashboard)
handle_cast({approve, TaskId, Decision}, State) ->
    case Decision of
        approve ->
            fire(t_approve, #{p_approval_queue := [{approval_required, TaskId, _}]}, State);
        reject ->
            fire(t_reject, #{p_approval_queue := [{approval_required, TaskId, _}]}, State)
    end.
```

**LLM Integration:**
```erlang
% Get LLM recommendation
get_llm_recommendation(Context) ->
    Prompt = format_llm_prompt(Context),
    {ok, Response} = llm_client:complete(Prompt),
    parse_recommendation(Response).
```

**Diagram Suggestion:**
```
Human-in-the-Loop Flow:

[Task] --> [Request Approval] --> [Approval Queue]
                                     |
                                     v
                            [Web Dashboard]
                                     |
          +--------------------------+------------------+
          |                          |                  |
    [LLM Recommend]            [Human Approve]    [Human Reject]
          |                          |                  |
          +--------------------------+------------------+
                                     |
                                     v
                              [Record Decision]
                                     |
                                     v
                              [Continue Workflow]
```

**Speaker Notes:**
- Human-in-the-loop is key differentiator
- LLM assists but human decides
- Full audit trail of all decisions
- Web dashboard for user interface

---

## Slide 39: Technical Deep Dive Summary

### Key Architectural Insights

**Design Principles:**
1. **Single runner**: gen_pnet is the only stateful OTP component
2. **Pure helpers**: All logic in stateless functions
3. **Formal foundation**: Petri nets provide mathematical basis
4. **Receipt audit**: Every transition documented
5. **Composability**: Patterns compose into complex workflows

**Technical Takeaways:**
- Marking algebra enables precise state tracking
- Mode enumeration gives deterministic nondeterminism
- Pattern registry enables extensible pattern library
- Code generation produces verifiable output
- Receipt system enables recovery and verification

**Why This Matters:**
- Verified workflows for critical business processes
- Fault tolerance through receipt-based recovery
- Observability through integrated telemetry
- Extensibility through pure functional design

**Speaker Notes:**
- This deep dive covered the core technical elements
- Everything builds on these foundations
- Architecture enables reliability and extensibility
- Next: see it in action with AGI Symposium case study

---

# Part 4: Case Study (10-15 slides)

---

## Slide 40: AGI Symposium Omega - Overview

### A Complete Real-World Workflow

**Bullet Points:**
- **Domain**: Academic conference planning
- **Complexity**: 43 patterns used across workflow
- **Participants**: Chair, Program Chair, Reviewers, Ops Lead, Venue Lead, Press Lead, Safety Officer
- **Stages**: CFP, Review, Acceptance, Program Planning, Logistics
- **Constraints**: Deadlines, budget limits, capacity

**Workflow Stages:**
1. **Setup**: Committee formation, CFP announcement
2. **Submission**: Paper submissions, author registration
3. **Review**: Parallel review by multiple reviewers
4. **Decision**: Accept/reject decisions
5. **Program**: Schedule planning, track assignment
6. **Logistics**: Venue, catering, accommodation
7. **Execution**: Conference proceedings
8. **Post-event**: Proceedings publication, feedback

**Diagram Suggestion:**
```
AGI Symposium Omega Workflow:

[Setup] --> [CFP] --> [Submissions] --> [Reviews] --> [Decisions]
                                                      |
                                                      v
                                               [Program Plan]
                                                      |
                                                      v
[Logistics] <---[Planning]------------------+ [Scheduling]
    |                                      |
    +--> [Venue]                           +--> [Tracks]
    +--> [Catering]                        +--> [Sessions]
    +--> [Accommodation]
    |
    v
[Conference Execution]
    |
    v
[Post-Event]
```

**Speaker Notes:**
- AGI Symposium is our comprehensive case study
- Demonstrates all 43 patterns in realistic context
- Multiple participants with different roles
- Complex coordination requirements

---

## Slide 41: AGI Symposium - Pattern Usage

### Which Patterns Are Used Where

**Pattern Breakdown by Stage:**

| Stage | Patterns Used | Purpose |
|-------|--------------|---------|
| Setup | P1 (Sequence), P4 (Exclusive Choice) | Sequential tasks, venue selection |
| CFP | P2 (Parallel Split), P3 (Sync) | Announce across multiple channels |
| Submissions | P13 (Static MI) | Fixed submission categories |
| Reviews | P14 (Runtime MI), P37 (Local Sync) | Dynamic reviewer allocation |
| Decisions | P10 (N-of-M), P38 (General Sync) | Quorum-based acceptance |
| Program | P6 (Multi-choice), P7 (Sync Merge) | Track scheduling |
| Logistics | P2, P3, P26 (Critical Section) | Parallel prep, resource coordination |
| Execution | P17 (Interleaved), P39 (Critical) | Concurrent sessions |

**Pattern Interaction Diagram:**
```
[CFP Announcement]
(P2: Parallel Split)
        |
        +--> [Web Announcement]
        +--> [Email Lists]
        +--> [Social Media]
        |
(P3: Synchronization) <-- Wait for all channels
        |
        v
[Collect Submissions]
(P14: Runtime MI) <-- Dynamic reviewer assignment
        |
        +--> [Reviewer 1] --> (P37: Local Sync)
        +--> [Reviewer 2] --> (P37: Local Sync)
        +--> [Reviewer 3] --> (P37: Local Sync)
        |
(P38: General Sync Merge)
        |
        v
[Acceptance Decision]
(P10: N-out-of-M) <-- 2 of 3 reviewers must agree
```

**Speaker Notes:**
- Shows realistic pattern usage
- Patterns combine naturally
- No artificial forcing - patterns emerge from requirements
- Demonstrates expressiveness of pattern library

---

## Slide 42: AGI Symposium - The Review Process

### Multiple Instance Patterns in Action

**Bullet Points:**
- **Challenge**: Assign reviewers dynamically based on load
- **Pattern**: P14 (Multiple Instance with Runtime Knowledge)
- **Configuration**: 2-3 reviewers per paper
- **Load balancing**: Distribute based on reviewer capacity
- **Deadline enforcement**: Time limits for reviews

**Review Process Specification:**
```yaml
review_process:
  pattern: "P14_MultipleInstanceRuntime"
  config:
    min_instances: 2
    max_instances: 3
    count_function: "determine_reviewer_count"
    data_source: "paper_submissions"
  branches:
    - reviewer_1:
        pattern: "P37_LocalSyncMerge"
        config:
          sync_point: "review_complete"
    - reviewer_2:
        pattern: "P37_LocalSyncMerge"
        config:
          sync_point: "review_complete"
    - reviewer_3:
        pattern: "P37_LocalSyncMerge"
        condition: "heavy_paper"
```

**Dynamic Instance Creation:**
```erlang
% Determine reviewer count based on paper complexity
determine_reviewer_count(Paper) ->
    BaseCount = 2,
    case Paper#paper.complexity of
        high -> BaseCount + 1;
        medium -> BaseCount;
        low -> BaseCount
    end.

% Create reviewer instances
fire(t_create_reviewers, _, #state{paper = Paper, available_reviewers = Available}) ->
    Count = determine_reviewer_count(Paper),
    Reviewers = lists:sublist(Available, Count),
    ReviewerTokens = [create_reviewer_token(R, Paper) || R <- Reviewers],
    {produce, #{
        p_reviewers => ReviewerTokens,
        p_available => lists:nthtail(Count, Available)
    }}.
```

**Diagram Suggestion:**
```
Dynamic Review Process:

[Paper Submitted]
      |
      v
[Analyze Paper] --> [Determine Reviewer Count]
      |
      v
[Assign Reviewers] --+--> [Reviewer 1] --> [Review]
      |              +--> [Reviewer 2] --> [Review]
      |              +--> [Reviewer 3] --> [Review] (if complex)
      |
      v
[Collect Reviews] (P38: General Sync Merge)
      |
      v
[Make Decision]
```

**Speaker Notes:**
- This is a realistic multiple instance scenario
- Number of reviewers depends on paper complexity
- Each reviewer works independently
- Synchronization collects all reviews
- Demonstrates power of runtime instance determination

---

## Slide 43: AGI Symposium - Decision Making

### N-out-of-M and Quorum Patterns

**Bullet Points:**
- **Pattern**: P10 (N-out-of-M) for acceptance decisions
- **Configuration**: 2 of 3 reviewers must agree
- **Tie-breaking**: Program Chair decides on ties
- **Transparency**: All votes recorded
- **Appeal process**: Authors can appeal rejection

**Acceptance Decision Logic:**
```erlang
% N-out-of-M: 2 of 3 must agree
is_enabled(t_accept, _, #state{votes = Votes}) when length(Votes) >= 2 ->
    AcceptCount = lists:foldl(fun(V, Acc) ->
        case V of accept -> Acc + 1; _ -> Acc end
    end, 0, Votes),
    AcceptCount >= 2;

is_enabled(t_reject, _, #state{votes = Votes}) when length(Votes) >= 2 ->
    RejectCount = lists:foldl(fun(V, Acc) ->
        case V of reject -> Acc + 1; _ -> Acc end
    end, 0, Votes),
    RejectCount >= 2;
```

**Decision Tracking:**
```erlang
-record(decision_state, {
    paper_id :: binary(),
    reviewers :: [binary()],
    votes = [] :: [{accept | reject, binary(), binary()}],
    final_decision :: accept | reject | pending | tie,
    tie_breaker :: binary() | undefined
}).
```

**Diagram Suggestion:**
```
Quorum-Based Decision Making:

[Review 1: Accept] -----+
                        +--> [2 Accept] --> [Accept]
[Review 2: Accept] -----+      |
                        |      |
[Review 3: Reject] -----+      v
                       [Tie?] --> [Program Chair Decides]
```

**Speaker Notes:**
- N-out-of-M is crucial for consensus
- Prevents single reviewer veto
- Program Chair breaks ties
- Full audit trail of all votes
- Demonstrates advanced pattern usage

---

## Slide 44: AGI Symposium - Track Scheduling

### Complex Synchronization Patterns

**Bullet Points:**
- **Challenge**: Schedule sessions across multiple tracks
- **Patterns**: P2 (Parallel Split), P3 (Sync), P6 (Multi-Choice)
- **Constraints**: No speaker conflicts, room capacity limits
- **Optimization**: Minimize track conflicts
- **Changes**: Handle schedule changes gracefully

**Track Parallel Structure:**
```yaml
conference_tracks:
  pattern: "P2_ParallelSplit"
  branches:
    - track_research:
        sessions: 20
        room: "Main Hall"
        capacity: 200
    - track_tutorial:
        sessions: 10
        room: "Tutorial Room A"
        capacity: 50
    - track_workshop:
        sessions: 15
        room: "Workshop Room B"
        capacity: 30
    - track_keynote:
        sessions: 5
        room: "Main Hall"
        capacity: 200
  synchronization:
    pattern: "P7_SynchronizingMerge"
    config:
      sync_points: ["lunch", "breaks", "keynote"]
```

**Conflict Resolution:**
```erlang
% Check for speaker conflicts
check_speaker_conflict(Speaker, TimeSlot, ScheduledSessions) ->
    Conflicts = [S || S <- ScheduledSessions,
        S#session.speaker =:= Speaker,
        S#session.timeslot =:= TimeSlot],
    case Conflicts of
        [] -> {ok, no_conflict};
        _ -> {error, {speaker_conflict, Conflicts}}
    end.

% Room capacity check
check_room_capacity(Session, RoomCapacity) ->
    case Session#session.expected_attendees of
        N when N =< RoomCapacity -> {ok, fits};
        N -> {error, {exceeds_capacity, N, RoomCapacity}}
    end.
```

**Diagram Suggestion:**
```
Track Synchronization:

[Keynote] ----> [Parallel Tracks] ----> [Keynote]
                   |  |  |  |
                   +--+--+--+
                   |  |  |  |
             [Research] [Tutorial] [Workshop] [Poster]
                   |  |  |  |
                   +--+--+--+
                   |  |  |  |
             [Session] [Session] [Session] [Session]
                   |  |  |  |
                   +--+--+--+
                   |
                   v
              [Lunch Break]
                   |
                   v
              [Afternoon Tracks...]
```

**Speaker Notes:**
- Track scheduling demonstrates parallel patterns
- Multiple constraints must be satisfied
- Synchronization at common events (lunch, keynotes)
- Shows real-world complexity

---

## Slide 45: AGI Symposium - Emergency Handling

### Cancellation and Exception Patterns

**Bullet Points:**
- **Scenario**: Emergency requires immediate cancellation
- **Patterns**: P20 (Cancel Case), P25 (Cancel Region)
- **Participants**: Safety Officer has emergency authority
- **Cleanup**: Cancel all venues, notify attendees, process refunds
- **Recovery**: Restart possible after emergency resolved

**Emergency Cancellation Flow:**
```erlang
% Safety Officer triggers cancellation
handle_cast({emergency_cancel, Reason, SafetyOfficer}, State) ->
    % Validate authority
    case validate_safety_officer(SafetyOfficer) of
        {ok, true} ->
            % Cancel entire case (P20)
            cancel_all_activities(State),
            % Notify all participants
            broadcast_emergency_notification(Reason),
            % Initiate refund process
            start_refund_workflow(State),
            {noreply, State#state{cancelled = true, cancel_reason = Reason}};
        {error, unauthorized} ->
            {noreply, State}
    end.

% Cancel region (P25)
cancel_region(RegionId, State) ->
    Activities = get_activities_in_region(RegionId, State),
    lists:foreach(fun(A) ->
        gen_yawl:stop(A#activity.pid),
        cleanup_activity(A)
    end, Activities).
```

**Emergency State Machine:**
```
[Normal Operation] --[Emergency Signal]--> [Cancellation Mode]
                                           |
                                           v
                                    [Stop All Activities]
                                           |
                                           v
                                    [Notify Participants]
                                           |
                                           v
                                    [Process Refunds]
                                           |
                                           v
                                    [Emergency Resolved?] --[No]--> [Wait]
                                                              |
                                                             [Yes]
                                                              |
                                                              v
                                                    [Restart Possible]
```

**Speaker Notes:**
- Emergency handling is critical for real events
- Cancellation patterns provide structured abort
- Safety Officer has special authority
- Cleanup is automatic and comprehensive
- Demonstrates exception handling patterns

---

## Slide 46: AGI Symposium - Metrics and Observability

### Tracking Conference Progress

**Bullet Points:**
- **Submission metrics**: Count, acceptance rate, by topic
- **Review metrics**: Completion rate, reviewer load
- **Session metrics**: Attendance, feedback scores
- **Logistics metrics**: Budget tracking, resource utilization
- **Real-time dashboard**: Live status updates

**Telemetry Integration:**
```erlang
% Track submission metrics
record_submission(Submission) ->
    otel_meter:record(?SUBMISSION_COUNTER, 1, #{
        topic => Submission#submission.topic,
        submission_date => Submission#submission.date
    }).

% Track review progress
record_review_complete(Reviewer, Paper, Score) ->
    otel_meter:record(?REVIEW_HISTOGRAM, Score, #{
        reviewer => Reviewer,
        paper => Paper#paper.id
    }).

% Track session attendance
record_attendance(Session, Count) ->
    otel_meter:record(?ATTENDANCE_GAUGE, Count, #{
        session => Session#session.id,
        track => Session#session.track,
        timeslot => Session#session.timeslot
    }).
```

**Dashboard Metrics:**
| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Submissions | 127 | 100+ | ✅ On track |
| Reviews Complete | 89/127 | 90% | ⚠️ Behind |
| Acceptance Rate | 23% | 20-30% | ✅ Normal |
| Sessions Scheduled | 45/50 | 50 | ⚠️ Pending |
| Budget Used | $45,000 | $50,000 | ✅ Under budget |

**Speaker Notes:**
- Observability is crucial for event management
- Real-time metrics enable proactive decisions
- OpenTelemetry integration provides standardized tracking
- Dashboard gives at-a-glance status

---

## Slide 47: [DEMO] AGI Symposium Execution

### Running the Complete Workflow

**Demo Setup:**
```erlang
% Load AGI Symposium specification
{ok, Spec} = wf_yaml_spec:from_yaml_file("agi_symposium_omega.yaml").

% Compile workflow
{ok, Executor} = wf_yawl_executor:compile_workflow(Spec).

% Start execution
{ok, Pid} = wf_yawl_executor:start_workflow(Executor, #{
    conference_date => "2026-06-15",
    location => "San Francisco"
}, []).

% Execute stages
{ok, Receipt1} = wf_yawl_executor:execute_stage(setup_committee).
{ok, Receipt2} = wf_yawl_executor:execute_stage(call_for_papers).
{ok, Receipt3} = wf_yawl_executor:execute_stage(review_process).

% Check state
{ok, State} = wf_yawl_executor:workflow_state(Pid).

% View dashboard
wf_dashboard:open(Pid).
```

**What to Observe:**
- Pattern execution
- State transitions
- Receipt accumulation
- Dashboard updates
- Parallel branch execution

**Speaker Notes:**
- [Perform live demo here]
- Show full workflow execution
- Highlight pattern usage
- Demonstrate observability
- Show receipts and state

---

# Part 5: Conclusion (3-5 slides)

---

## Slide 48: Key Takeaways

### What We've Covered

**Generative Analysis:**
- Specification to verified executable code
- Formal methods ensure correctness
- Receipts provide complete audit trail

**Petri Net Foundation:**
- Mathematical formalism for workflow
- Marking algebra for precise state
- Deterministic execution via mode enumeration

**Pattern Library:**
- 43 patterns cover all workflow control structures
- Patterns compose into complex workflows
- Each pattern formally verified

**Production Ready:**
- Erlang/OTP for fault tolerance
- OpenTelemetry for observability
- Distributed execution support
- Human-in-the-loop integration

**Diagram Suggestion:**
```
Generative Analysis Value Proposition:

[Specification]
      |
      v
[Formal Verification] ----> [Properties Proven]
      |                           |
      v                           v
[Code Generation] ---------> [Verified Execution]
      |                           |
      v                           v
[Runtime Observability] --> [Complete Audit Trail]
```

**Speaker Notes:**
- Generative analysis enables verified workflows
- Formal foundation provides mathematical guarantees
- Pattern library accelerates development
- Production-quality implementation

---

## Slide 49: Benefits and Applications

### Why Use Generative Analysis for Workflows?

**Benefits:**
- **Correctness**: Verified before execution
- **Productivity**: Patterns accelerate development
- **Maintainability**: Declarative specifications
- **Observability**: Complete execution trace
- **Reliability**: Fault tolerance through receipts

**Applications:**
- **Business Process Management**: Order processing, approval flows
- **Scientific Workflows**: Data analysis pipelines
- **DevOps**: CI/CD pipelines, deployment orchestration
- **Event Planning**: Conferences, logistics coordination
- **Compliance**: Regulated processes with audit requirements

**Comparison:**
| Approach | Correctness | Development Speed | Maintainability |
|----------|-------------|-------------------|----------------|
| Manual Code | Low | Slow | Poor |
| BPMN/BPEL | Medium | Medium | Medium |
| **Generative Analysis** | **High** | **Fast** | **Excellent** |

**Speaker Notes:**
- Generative analysis offers unique advantages
- Combines formal methods with practical engineering
- Applicable across many domains
- Superior to alternative approaches

---

## Slide 50: Future Directions

### Where CRE is Going

**Near Term:**
- Visual workflow editor (drag-and-drop patterns)
- Additional verification tools (model checking)
- Performance optimization (large-scale workflows)
- Enhanced dashboard (real-time visualization)

**Medium Term:**
- AI-assisted workflow design
- Automatic pattern recommendation
- ML-based optimization
- Cloud-native deployment

**Long Term:**
- Formal verification of temporal properties
- Cross-language workflow composition
- Distributed ledger integration
- Autonomous workflow adaptation

**Community:**
- Open source contribution guide
- Pattern marketplace
- Integration ecosystem
- Academic collaboration

**Speaker Notes:**
- CRE is actively evolving
- Community involvement welcome
- Research opportunities abound
- Future looks exciting

---

## Slide 51: Resources and Next Steps

### Learn More and Get Started

**Documentation:**
- `/Users/sac/cre/docs/README.md` - Main documentation
- `/Users/sac/cre/docs/ARCHITECTURE.md` - System architecture
- `/Users/sac/cre/docs/YAWL_PATTERNS_REFERENCE.md` - Pattern guide
- `/Users/sac/cre/docs/GEN_PNET_USER_GUIDE.md` - Runtime guide

**Examples:**
- `/Users/sac/cre/examples/` - Sample workflows
- AGI Symposium Omega - Complete case study

**Code:**
- `/Users/sac/cre/src/patterns/` - Pattern implementations
- `/Users/sac/cre/src/core/` - Core runtime
- `/Users/sac/cre/test/` - Test suites

**Getting Started:**
```bash
# Clone repository
git clone https://github.com/your-org/cre.git

# Build
cd cre && rebar3 compile

# Run tests
rebar3 ct

# Start shell
rebar3 shell

# Run first workflow
{ok, Pid} = sequence:start_link(#{}).
```

**Speaker Notes:**
- CRE is open source and well-documented
- Examples help you get started quickly
- Community support available
- Start simple, then explore advanced features

---

## Slide 52: Q&A

### Questions and Discussion

**Potential Discussion Topics:**
- Formal methods in industry adoption
- Pattern library extensions
- Verification techniques
- Performance optimization
- Integration with existing systems

**Contact:**
- GitHub: https://github.com/your-org/cre
- Documentation: https://cre.readthedocs.io
- Email: cre-discuss@example.com

**Speaker Notes:**
- Open floor for questions
- Happy to discuss specific use cases
- Interested in collaboration opportunities
- Thank you for attending!

---

## Appendix: Presentation Adaptation Guide

### For 30-Minute Presentations

**Include:**
- Slides 1-8 (Introduction)
- Slides 9-12 (Core Concepts - selected)
- Slides 31-35 (Case Study highlights)
- Slides 48-52 (Conclusion)

**Skip:**
- Deep technical details (21-30)
- Pattern category deep dives (24-29)
- Verification details (35)

**Demo:**
- Single demo at slide 14 or 47

---

### For 60-Minute Presentations

**Include:**
- All slides from 30-minute version
- Plus: Slides 13-20 (More core concepts)
- Plus: Slides 21-25 (Technical deep dive intro)
- Plus: Full case study (40-47)

**Skip:**
- Some advanced technical slides (26-30)
- Detailed verification (35)

**Demos:**
- Demo at slide 14 (basic)
- Demo at slide 47 (full workflow)

---

### For 90-Minute Presentations

**Include:**
- Complete deck (all 52 slides)
- All technical deep dives
- Extended Q&A

**Demos:**
- Demo at slide 14 (basic pattern)
- Demo at slide 34 (compilation)
- Demo at slide 47 (AGI Symposium)

**Timing:**
- Introduction: 15 minutes
- Core Concepts: 20 minutes
- Technical Deep Dive: 30 minutes
- Case Study: 15 minutes
- Conclusion: 5 minutes
- Q&A: 5 minutes

---

**Document Version:** 1.0
**Last Updated:** 2026-02-07
**Author:** CRE Documentation Team
