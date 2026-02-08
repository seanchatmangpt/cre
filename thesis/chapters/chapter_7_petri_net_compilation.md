# Chapter 7: Petri Net Compilation

## 7.1 Petri Nets as the Executable Model

The compilation of YAWL workflow specifications into executable form rests on a foundational choice of execution model. Rather than inventing a new ad-hoc execution semantics, the CRE system adopts Petri nets as its formal computational substrate. This choice is not arbitrary—it emerges from the deep mathematical foundations of Petri nets, their natural alignment with workflow concerns, and their proven track record in concurrent systems verification.

### 7.1.1 The Petri Net Formalism

A Petri net is a bipartite directed graph consisting of two types of nodes: **places** and **transitions**. Places (typically drawn as circles) represent passive state containers, while transitions (typically drawn as bars or rectangles) represent active state transformations. Directed arcs connect places to transitions (input arcs) and transitions to places (output arcs), forming the net structure.

**Formally**, a Petri net N is a tuple (P, T, A) where:
- P is a finite set of places
- T is a finite set of transitions (with P n T = 0)
- A is a set of arcs (P x T) union (T x P)

The **dynamic behavior** of a Petri net emerges from the distribution of **tokens** across places. A configuration of tokens is called a **marking** M: P -> N, where each place contains a natural number of tokens. The execution of a Petri net proceeds through the firing of enabled transitions, which consumes tokens from input places and produces tokens to output places.

This simple formalism captures the essential elements of workflow execution:
- **Places** represent workflow states (waiting, active, completed)
- **Tokens** represent work items, data, or control flow tokens
- **Transitions** represent tasks, decisions, or events
- **Arcs** represent data flow and control dependencies

### 7.1.2 Why Petri Nets Fit Workflow Execution

The correspondence between Petri net concepts and workflow concerns is strikingly direct. This alignment explains why Petri nets serve as an excellent executable model for workflow systems:

#### 1. Natural State Representation

Workflow execution fundamentally concerns the state of work items as they progress through a business process. Places naturally represent these states:
- A place labeled "pending_review" contains tokens representing submissions awaiting review
- A place labeled "under_review" contains tokens representing submissions currently being reviewed
- A place labeled "approved" contains tokens representing submissions that have been approved

The multiset semantics of Petri nets—allowing multiple tokens in a place—directly model the reality that multiple work items can be in the same state simultaneously.

#### 2. Explicit Concurrency Semantics

Petri nets provide **formal semantics for concurrent execution** through the concept of enabled transitions. When multiple transitions are simultaneously enabled (each has sufficient tokens in its input places), they represent concurrently executable tasks. The Petri net formalism specifies that these transitions can fire in any order, producing the same final marking when the net is sound.

This property directly addresses a core workflow concern: how to execute tasks in parallel while ensuring correct synchronization and avoiding race conditions. The Petri net model guarantees that well-formed nets will converge correctly regardless of execution order.

#### 7.1.3: Workflow Pattern Representation

Every workflow pattern in the van der Aalst catalog has a direct Petri net representation. This one-to-one correspondence means that workflow specifications can be **systematically compiled** to Petri nets without loss of expressiveness.

**Sequence (WCP-01):** Two transitions connected through an intermediate place.

```
  t1      t2
o----->o----->o
```

**Parallel Split (WCP-02):** One transition produces tokens to multiple output places.

```
     t_split
    /  |  \
   v   v   v
  o   o   o
```

**Synchronization (WCP-03):** One transition consumes tokens from multiple input places.

```
  ^   ^   ^
  |   |   |
  o---o---o
     t_sync
```

**Exclusive Choice (WCP-04):** Two transitions share the same input place, representing mutually exclusive alternatives.

```
      p_choice
      /     \
   t_A       t_B
```

This direct mapping enables a **pattern-based compiler**: given a workflow specification expressed using the 43 patterns, the compiler generates a Petri net by composing the pattern modules. The resulting net inherits the formal properties of Petri nets—soundness, liveness, boundedness—while directly executing the specified workflow logic.

### 7.1.4 The gen_pnet Execution Engine

The CRE system implements the Petri net execution model through the `gen_pnet` behavior and its workflow-oriented wrapper `gen_yawl`. These modules provide:

**Declarative Net Definition:** Callbacks specify the net structure:
- `place_lst/0` returns the list of all places in the net
- `trsn_lst/0` returns the list of all transitions
- `preset/1` returns the input places for each transition
- `postset/1` returns the output places for each transition (optional, derived from fire/3)

**Marking Management:** The system maintains the current marking as a map from places to token lists:
```erlang
#{p_start => [token], p_task1 => [], p_task2 => [], p_end => []}
```

**Enablement Testing:** The `is_enabled/3` callback determines if a transition can fire given a marking and user state:
```erlang
is_enabled(t_complete, #{p_task := [_]}, _UsrInfo) -> true;
is_enabled(t_complete, #{p_task := []}, _UsrInfo) -> false.
```

**Transition Firing:** The `fire/3` callback specifies the effect of a transition firing:
```erlang
fire(t_complete, #{p_task := [Token]}, _UsrInfo) ->
    {produce, #{p_task => [], p_next => [Token]}}.
```

The execution engine handles the progress loop: repeatedly finding enabled transitions, selecting one to fire, consuming tokens from its preset, and producing tokens to its postset. This continues until no transitions are enabled (stable marking) or a terminal place is reached.

### 7.1.5 Advantages of the Petri Net Approach

The Petri net compilation approach offers several advantages over alternative execution models:

**Formal Verification:** Petri nets are mathematically well-understood with established techniques for verifying properties such as:
- **Soundness:** Every execution path terminates properly
- **Liveness:** No dead transitions (all can eventually fire)
- **Boundedness:** No place accumulates unbounded tokens
- **Reversibility:** The net can return to initial state

**Tool Support:** A rich ecosystem of Petri net analysis tools exists for:
- State space exploration
- Invariant checking
- Simulation and testing
- Process mining

**Separation of Concerns:** The Petri net model separates:
- **Structure:** The net topology (places, transitions, arcs)
- **State:** The current marking (token distribution)
- **Behavior:** The firing rules (transition semantics)

This separation enables modular development, independent testing, and clear reasoning about workflow correctness.

**Language Independence:** Petri nets provide a workflow execution model that is independent of any specification language. YAWL, BPMN, or any other workflow notation can be compiled to Petri nets, enabling interoperability and comparative analysis.

### 7.1.6 Example: Simple Sequence Net

Consider a simple sequence pattern with two tasks:

```erlang
-module(sequence).
-behaviour(gen_yawl).

place_lst() -> [p_start, p_task1, p_task2, p_end].
trsn_lst() -> [t_start, t_complete1, t_complete2, t_finish].

preset(t_start) -> [p_start];
preset(t_complete1) -> [p_task1];
preset(t_complete2) -> [p_task2];
preset(t_finish) -> [p_task2];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, _UsrInfo) ->
    {produce, #{p_task1 => [token]}};
fire(t_complete1, _Mode, _UsrInfo) ->
    {produce, #{p_task2 => [token]}};
fire(t_complete2, _Mode, _UsrInfo) ->
    {produce, #{p_end => [done]}};
fire(t_finish, _Mode, _UsrInfo) ->
    {produce, #{p_end => [done]}};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.
```

This module defines a Petri net with four places and four transitions. When executed:
1. Initial marking: `#{p_start => [init]}`
2. t_start fires, producing: `#{p_task1 => [token]}`
3. t_complete1 fires, producing: `#{p_task2 => [token]}`
4. t_complete2 fires, producing: `#{p_end => [done]}`
5. Execution terminates

This simple example illustrates the compilation workflow: pattern specification -> Petri net module -> executable workflow. The same approach scales to complex workflows through pattern composition.

---

## 7.2 Markings, Enablement, and Firing

The dynamic execution of Petri nets rests on three core concepts: **markings** (state representation), **enablement** (execution conditions), and **firing** (state transitions). These concepts together define the operational semantics of workflow execution in the CRE system.

### 7.2.1 Markings: Token Distributions as State

A **marking** represents the complete state of a workflow at a point in time. Formally, a marking M is a function M: P -> Bag(T), mapping each place to a multiset (bag) of tokens. In the CRE implementation, markings are represented as Erlang maps:

```erlang
-type marking() :: #{place() => [token()]}.
```

Each place atom maps to a list of tokens currently at that place. The list representation preserves the multiset semantics—multiple identical tokens can appear in a place, and order does not affect the marking's identity (as canonicalized by the hash function).

**Initial Markings:** Every workflow begins with an initial marking, typically a single token in a start place:

```erlang
init_marking(p_start, _UsrInfo) -> [init_token];
init_marking(_Place, _UsrInfo) -> [].
```

**Terminal Markings:** A workflow terminates when it reaches a marking with no enabled transitions. For well-formed workflows, this occurs when a terminal place contains a completion token:

```erlang
#{p_end => [complete], p_other => []}
```

### 7.2.2 Marking Algebra Operations

The `pnet_marking` module provides a complete algebra for manipulating markings. These operations implement the mathematical semantics of Petri nets:

**Multiset Addition (add/2):** Tokens are added to places, respecting multiplicity:

```erlang
M1 = #{p1 => [a, b], p2 => [c]},
M2 = pnet_marking:add(M1, #{p1 => [b], p2 => [d]}),
%% Result: #{p1 => [a, b, b], p2 => [c, d]}
```

**Multiset Subtraction (take/2):** Tokens are removed from places, requiring exact matches with sufficient multiplicity:

```erlang
M1 = #{p1 => [a, b, b], p2 => [c]},
case pnet_marking:take(M1, #{p1 => [a, b]}) of
    {ok, M2} -> %% M2 = #{p1 => [b], p2 => [c]};
    {error, insufficient} -> %% Not enough tokens
end.
```

**Atomic Transition (apply/3):** Consumes and produces tokens in one operation, ensuring consistency:

```erlang
case pnet_marking:apply(Marking, ConsumeMap, ProduceMap) of
    {ok, NewMarking} -> %% Success
    {error, insufficient} -> %% Consumption failed, marking unchanged
end.
```

The atomic nature of `apply/3` is critical: if consumption cannot be satisfied, no tokens are produced and the marking remains unchanged. This prevents partial state updates that could corrupt workflow execution.

### 7.2.3 Modes: Binding Patterns for Transition Firing

In workflow execution, transitions often consume specific tokens from their input places. The **mode** of a transition firing specifies exactly which tokens are consumed. A mode is a map from places to token lists:

```erlang
-type mode() :: #{place() => [token()]}.
```

For example, consider a transition that merges two branches of a parallel split. The mode specifies exactly which tokens are being synchronized:

```erlang
%% Both branches have completed with their respective tokens
Mode = #{p_branch1 => [token1], p_branch2 => [token2]},
%% Transition t_sync consumes both tokens
```

The system enumerates all possible modes for each transition by combining tokens from the preset places. This enumeration enables **dataflow-aware execution**: transitions fire based on specific token values, enabling conditional behavior and data-dependent routing.

### 7.2.4 Enablement: The Guard Condition

A transition is **enabled** in a marking if there exists at least one mode satisfying:
1. For each input place p in the preset, the mode specifies tokens present in M(p)
2. The `is_enabled/3` callback returns true for that mode and user info

The enablement check serves as a **guard condition**, determining when a transition can fire. This check can incorporate:
- Token presence and multiplicity
- User-defined state (workflow variables, case data)
- External conditions (timeouts, resource availability)

**Example: Conditional Enablement**

```erlang
%% Transition only enabled if token contains 'approve' decision
is_enabled(t_approve, #{p_review := [#{decision := approve}]}, _UsrInfo) ->
    true;
is_enabled(t_approve, #{p_review := [_]}, _UsrInfo) ->
    false.
```

**Example: Milestone Enablement**

```erlang
%% Transition only enabled if milestone reached
is_enabled(t_proceed, _Mode, UsrInfo = #{milestone := reached}) ->
    true;
is_enabled(t_proceed, _Mode, _UsrInfo) ->
    false.
```

### 7.2.5 Firing: The Atomic State Transition

When an enabled transition fires, it atomically:
1. Consumes tokens specified by the mode from preset places
2. Executes the transition logic (fire/3 callback)
3. Produces tokens to postset places

The fire/3 callback returns either:
- `{produce, ProduceMap}`: Standard firing, produces new tokens
- `{produce, ProduceMap, NewUsrInfo}`: Firing with user state update
- `abort`: Firing aborted (mode rejected), try another mode

**Example: Simple Forwarding**

```erlang
fire(t_forward, #{p_in := [Token]}, UsrInfo) ->
    {produce, #{p_out => [Token]}, UsrInfo}.
```

**Example: Token Transformation**

```erlang
fire(t_transform, #{p_in := [Data]}, UsrInfo) ->
    Transformed = transform_data(Data),
    {produce, #{p_out => [Transformed]}, UsrInfo}.
```

**Example: Conditional Routing**

```erlang
fire(t_route, #{p_in := [#{type := Type}]}, UsrInfo) ->
    ProduceMap = case Type of
        urgent -> #{p_urgent => [urgent_token]};
        normal -> #{p_normal => [normal_token]}
    end,
    {produce, ProduceMap, UsrInfo}.
```

### 7.2.6 Execution Traces

A sequence of transition firings produces an **execution trace**, recording the chronological evolution of the workflow state. Each trace entry records:
- The transition that fired
- The mode of firing (tokens consumed)
- The tokens produced
- The resulting marking
- The timestamp

**Example Trace: Simple Approval**

```
1. Initial Marking: #{p_start => [request], p_review => [], p_approve => [], p_end => []}
2. Fire t_start: #{p_review => [request], ...}
3. Fire t_review: #{p_approve => [#{decision := approve, request := request}], ...}
4. Fire t_approve: #{p_end => [complete], ...}
```

The execution trace provides a **complete audit trail** of workflow execution, essential for:
- Debugging workflow logic
- Verifying compliance requirements
- Process mining and analysis
- Rollback and recovery

### 7.2.7 Determinism vs. Nondeterminism

Petri net execution exhibits **controlled nondeterminism**: when multiple transitions are enabled, any may fire. However, for well-formed nets, the final marking is independent of firing order (confluence property).

The CRE system manages nondeterminism through:
- **Mode Enumeration:** All possible modes are considered
- **Transition Selection:** Random or priority-based selection
- **User Info:** State can guide transition choice

**Example: Nondeterministic Choice**

```erlang
%% Both t_a and t_b are enabled
%% Firing order does not affect final result
Marking = #{p_choice => [token], p_a => [], p_b => [], p_end => []}
%% Path A: t_a then t_merge
%% Path B: t_b then t_merge
%% Both result: #{p_end => [token]}
```

For workflows requiring **deterministic choice**, conditional enablement or token data guides the execution:

```erlang
is_enabled(t_a, #{p_choice := [#{path := a}]}, _) -> true;
is_enabled(t_b, #{p_choice := [#{path := b}]}, _) -> true;
is_enabled(_, _, _) -> false.
```

### 7.2.8 Advanced Firing Patterns

**Inhibitor Arcs:** Transitions that fire when a place is empty (not directly supported, can be encoded through complementary places).

**Priority Transitions:** Higher-priority transitions fire before lower-priority ones (implemented through mode selection ordering).

**Timeout Transitions:** Transitions that fire if no other transition fires within a time window (implemented through wf_timerq deadline queue).

**Reset Arcs:** Transitions that empty a place regardless of token count (implemented through produce maps with empty token lists).

### 7.2.9 Firing Semantics and Workflow Correctness

The firing semantics directly impact workflow correctness properties:

**Soundness:** A workflow is sound if every execution path reaches the terminal marking. In Petri net terms, the terminal place is the unique sink, reachable from all reachable markings.

**Liveness:** A workflow is live if every transition can eventually fire from any reachable marking. This prevents deadlock where workflow execution halts prematurely.

**Boundedness:** A workflow is bounded if no place accumulates unbounded tokens. This ensures resource limits are not exceeded.

**Reversibility:** A workflow is reversible if the initial marking is reachable from any marking. This enables rollback and compensation.

The CRE system leverages Petri net analysis techniques to verify these properties at compile time, ensuring that workflows are correct before deployment.

### 7.2.10 Example: Complete Execution Trace

Consider a simple approval workflow:

```erlang
-module(approval_workflow).
-behaviour(gen_yawl).

place_lst() -> [p_start, p_review, p_approve, p_reject, p_end].
trsn_lst() -> [t_submit, t_review_approve, t_review_reject, t_complete].

preset(t_submit) -> [p_start];
preset(t_review_approve) -> [p_review];
preset(t_review_reject) -> [p_review];
preset(t_complete) -> [p_approve];
preset(_) -> [].

is_enabled(t_review_approve, #{p_review := [#{decision := approve}]}, _) -> true;
is_enabled(t_review_reject, #{p_review := [#{decision := reject}]}, _) -> true;
is_enabled(_, _, _) -> false.

fire(t_submit, _, _) ->
    {produce, #{p_review => [#{decision := pending}]}};
fire(t_review_approve, _, _) ->
    {produce, #{p_approve => [approved]}};
fire(t_review_reject, _, _) ->
    {produce, #{p_reject => [rejected]}};
fire(t_complete, _, _) ->
    {produce, #{p_end => [complete]}}.
```

**Execution Trace (Approval Path):**

```
State 0: #{p_start => [init], p_review => [], p_approve => [], p_reject => [], p_end => []}
  -> t_submit fires

State 1: #{p_start => [], p_review => [#{decision := pending}], ...}
  -> Reviewer approves, t_review_approve fires

State 2: #{p_review => [], p_approve => [approved], ...}
  -> t_complete fires

State 3: #{p_approve => [], p_end => [complete]}
  -> No enabled transitions, TERMINAL
```

**Execution Trace (Rejection Path):**

```
State 0: #{p_start => [init], ...}
  -> t_submit fires

State 1: #{p_review => [#{decision := pending}], ...}
  -> Reviewer rejects, t_review_reject fires

State 2: #{p_review => [], p_reject => [rejected], ...}
  -> No enabled transitions, TERMINAL
```

These traces demonstrate how the Petri net firing semantics naturally encode workflow logic, with tokens flowing through places and transitions firing based on enablement conditions. The complete trace provides an auditable record of workflow execution, essential for compliance and debugging.
