# Generative Analysis for Constructive Systems - Glossary

**Version:** 1.0
**Date:** 2026-02-07
**Scope:** CRE (Common Runtime Environment) - YAWL Workflow Engine

---

## Preface

This glossary defines all key terms used in the CRE system, covering methodology concepts, Petri net theory, workflow patterns, type system components, swarm coordination, and verification mechanisms. Terms are organized alphabetically with cross-references to related concepts.

---

## A

### abort
**Definition:** Return value indicating a transition firing cannot complete or no transition is enabled.

**Usage Context:** Petri net execution, specifically when:
- No enabled transitions exist in the current marking
- A transition's `fire/3` callback returns `abort` explicitly

**Example:**
```erlang
> gen_pnet:step(Pid).
abort  % No transitions enabled
```

**Related Terms:** [firing](#firing), [transition](#transition), [marking](#marking), [enabled](#enabled)

**Book Chapter:** Petri Net Execution (Chapter 4)

---

### agent (swarm)
**Definition:** An autonomous entity in a swarm system that performs tasks, makes decisions, and coordinates with other agents. In CRE, agents are typically LLMs or humans executing workflow tasks.

**Usage Context:** Swarm coordination, human-in-the-loop workflows, AGI Symposium simulation

**Example:**
```erlang
% Role-to-agent mapping in AGI Symposium
RoleToAgent = #{
    <<"ProgramChair">> => "llm_agent",
    <<"EthicsChair">> => "llm_agent",
    <<"Reviewer">> => "human_agent"
}
```

**Related Terms:** [role](#role), [task](#task), [coordination](#coordination), [swarm](#swarm)

**Book Chapter:** Swarm Coordination (Chapter 12)

---

### AGI Symposium Omega
**Definition:** A complex 20-role, 5-net workflow specification demonstrating all 43 YAWL patterns. Used as the canonical example for swarm workflow execution.

**Usage Context:** Pattern demonstration, system testing, thesis validation

**Components:**
- Root: Symposium net
- Subnets: ProgramThread, OpsThread, CommsThread, IncidentThread, SatelliteSymposium
- 20 human roles
- 43 pattern instances

**Related Terms:** [pattern](#pattern), [YAML spec](#yaml-spec), [compilation](#compilation)

**Book Chapter:** Case Studies (Chapter 15)

---

### and_join (synchronization)
**Definition:** A join type that waits for ALL incoming branches to complete before proceeding. Implements the synchronization pattern.

**Usage Context:** Parallel workflow convergence, WCP-03

**Example:**
```erlang
% AND join configuration
cre_yawl:set_join_type(Workflow, <<"merge">>, 'and_join)
```

**Related Terms:** [or_join](#or_join), [xor_join](#xor_join), [parallel_split](#parallel_split), [synchronization](#synchronization)

**Book Chapter:** Basic Control Flow Patterns (Chapter 5)

---

### arbitrary_cycles
**Definition:** Pattern P10 - Allows workflow execution to loop back to previous states, implementing general cycle constructs.

**Usage Context:** Recursive workflows, retry logic, iterative processing

**Petri Net Structure:**
```
p_loop_body ──► t_check_cond ──► p_condition_met ──► t_proceed ──► p_exit
                  │                                 │
                  └───────── t_loop_back ──────────┘
```

**Module:** `arbitrary_cycles.erl`

**Related Terms:** [structured_loop](#structured_loop), [recursion](#recursion), [milestone](#milestone)

**Book Chapter:** Advanced Control Flow (Chapter 7)

---

### atomic
**Definition:** In CRE, refers to operations that complete entirely or not at all, with no intermediate states visible to other processes.

**Usage Context:** Transition firing, checkpoint creation, marking operations

**Example:**
```erlang
% Atomic apply operation (consume AND produce)
pnet_marking:apply(Marking, ConsumeMap, ProduceMap)
```

**Related Terms:** [receipt](#receipt), [marking](#marking), [transition](#transition)

**Book Chapter:** Type System and Verification (Chapter 9)

---

## B

### bad_place
**Definition:** Error returned when referencing a non-existent place in a marking or Petri net operation.

**Usage Context:** Marking operations, token queries

**Example:**
```erlang
> pnet_marking:get(#{p1 => [a]}, missing).
{error, bad_place}
```

**Related Terms:** [place](#place), [marking](#marking), [insufficient](#insufficient)

**Book Chapter:** Type System (Chapter 8)

---

### binding
**Definition:** A map from variable names to their concrete values, used in colored Petri nets for pattern matching and data flow.

**Type Signature:**
```erlang
-type binding() :: #{var() => term()}.
```

**Usage Context:** Colored Petri nets, token substitution

**Example:**
```erlang
Binding = #{UserId => "alice", RequestId => "req_123"}
```

**Related Terms:** [var](#var), [cmode](#cmode), [token](#token)

**Book Chapter:** Colored Petri Nets (Chapter 8)

---

### blocking_discriminator
**Definition:** Pattern P28 - A discriminator that blocks waiting for the first incoming token, then produces output and ignores subsequent tokens until reset.

**Usage Context:** First-come-first-served scenarios, event aggregation

**Module:** `blocking_discriminator.erl`

**Related Terms:** [discriminator](#discriminator), [cancelling_discriminator](#cancelling_discriminator)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

---

## C

### callback (gen_pnet)
**Definition:** A function that a net module must implement to define its behavior. gen_pnet requires 13 callbacks organized into structure callbacks and interface callbacks.

**Structure Callbacks:**
- `place_lst/0` - Returns all place names
- `trsn_lst/0` - Returns all transition names
- `init_marking/2` - Returns initial marking for a place
- `preset/1` - Returns input places for a transition
- `is_enabled/3` - Checks if transition is enabled
- `fire/3` - Returns tokens produced when transition fires

**Interface Callbacks:**
- `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`
- `code_change/3`, `terminate/2`, `trigger/3`

**Related Terms:** [gen_pnet](#gen_pnet), [net module](#net-module), [transition](#transition)

**Book Chapter:** gen_pnet Reference (Chapter 10)

---

### cancel_activity
**Definition:** Pattern P19 - Allows cancellation of a specific running activity based on external events or conditions.

**Usage Context:** Long-running tasks, timeout handling, external interrupts

**Module:** `cancel_activity.erl`

**Petri Net Structure:**
```
p_running ──► t_cancel ──► p_cancelled
    │            │
    └──► t_complete ──► p_done
```

**Related Terms:** [cancel_case](#cancel_case), [cancel_region](#cancel_region), [milestone](#milestone)

**Book Chapter:** State-Based Patterns (Chapter 7)

---

### cancel_case
**Definition:** Pattern P20 - Allows cancellation of the entire workflow case, terminating all activities simultaneously.

**Usage Context:** Workflow abortion, emergency shutdown, global timeout

**Module:** `cancel_case.erl`

**Related Terms:** [cancel_activity](#cancel_activity), [explicit_termination](#explicit_termination)

**Book Chapter:** State-Based Patterns (Chapter 7)

---

### cancel_mi_activity
**Definition:** Pattern P26 - Cancels one or more activities within a multi-instance pattern while leaving others running.

**Usage Context:** Partial failure handling in parallel processing

**Module:** `cancel_mi_activity.erl`

**Related Terms:** [complete_mi_activity](#complete_mi_activity), [multiple_instances](#multiple_instances)

**Book Chapter:** Multiple Instance Patterns (Chapter 7)

---

### cancel_region
**Definition:** Pattern P25 - Defines a region of the workflow that can be cancelled as a unit, terminating all contained activities.

**Usage Context:** Scoped cancellation, hierarchical exception handling

**Module:** `cancel_region.erl`

**Related Terms:** [cancel_activity](#cancel_activity), [try_region](#try_region)

**Book Chapter:** Advanced Control Flow (Chapter 7)

---

### cancellation_discriminator
**Definition:** Pattern P29 - A discriminator that, once activated, cancels all remaining active branches in its scope.

**Module:** `cancelling_discriminator.erl`

**Related Terms:** [blocking_discriminator](#blocking_discriminator), [cancel_region](#cancel_region)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

### cmode (colored mode)
**Definition:** A combination of a binding and a mode, used in colored Petri nets to enable variable-based token matching and production.

**Type Signature:**
```erlang
-type cmode() :: {binding(), mode()}.
```

**Usage Context:** Colored Petri net execution, token pattern matching

**Example:**
```erlang
CMode = {#{UserId => "alice"}, #{p1 => [{user, UserId}]}}
```

**Related Terms:** [binding](#binding), [mode](#mode), [var](#var)

**Book Chapter:** Colored Petri Nets (Chapter 8)

---

### colored_Petri_net
**Definition:** An extension of Petri nets where tokens carry data (colors) and transitions can match on token patterns via variable bindings.

**Usage Context:** Data-driven workflows, parameterized execution

**Key Features:**
- Tokens are structured data, not just atoms
- Transitions use patterns and guards
- Variables enable data flow between transitions

**Related Terms:** [binding](#binding), [cmode](#cmode), [var](#var), [token](#token)

**Book Chapter:** Colored Petri Nets (Chapter 8)

---

### compilation
**Definition:** The process of transforming a YAWL specification (XML or YAML) into executable gen_pnet module code.

**Pipeline:**
1. Parse specification (YAML/XML)
2. Validate structure
3. Generate Petri net topology
4. Create Erlang module source
5. Load and register module

**Module:** `yawl_compile.erl`

**Related Terms:** [YAML spec](#yaml-spec), [pattern_expansion](#pattern-expansion), [gen_pnet](#gen_pnet)

**Book Chapter:** Compilation (Chapter 11)

---

### complete_mi_activity
**Definition:** Pattern P27 - Signals completion of multi-instance activities, potentially triggering downstream workflow steps.

**Module:** `complete_mi_activity.erl`

**Related Terms:** [cancel_mi_activity](#cancel_mi_activity), [multiple_instances](#multiple_instances)

**Book Chapter:** Multiple Instance Patterns (Chapter 7)

---

### consume_map
**Definition:** A map specifying which tokens to remove from which places during transition firing.

**Type Signature:**
```erlang
-type consume_map() :: #{place() => [token()]}.
```

**Usage Context:** Transition firing, marking updates

**Example:**
```erlang
ConsumeMap = #{p1 => [a], p2 => [b, c]}
```

**Related Terms:** [produce_map](#produce_map), [marking](#marking), [firing](#firing)

**Book Chapter:** Type System (Chapter 8)

---

### coordination
**Definition:** The process of managing interactions between agents in a swarm system, ensuring coherent collective behavior.

**Usage Context:** Swarm execution, agent communication, distributed decision making

**Mechanisms:**
- Message passing
- Shared state synchronization
- Consensus protocols
- Workflow constraints

**Related Terms:** [agent](#agent), [swarm](#swarm), [role](#role)

**Book Chapter:** Swarm Coordination (Chapter 12)

---

### critical_section
**Definition:** Pattern P39 - Ensures mutually exclusive access to a shared resource, preventing concurrent execution of sensitive code sections.

**Module:** `critical_section.erl`

**Usage Context:** Database access, file operations, shared resource protection

**Petri Net Structure:**
```
p_request ──► t_acquire ──► p_lock ──► t_execute ──► p_active ──► t_release ──► p_release
```

**Related Terms:** [resource](#resource), [mutex](#mutex), [concurrency](#concurrency)

**Book Chapter:** Extended Control Flow (Chapter 7)

---

## D

### data_accumulate
**Definition:** WDP-04 - Aggregates data from multiple source tasks into a single target task.

**Module:** `data_accumulate.erl`

**Usage Context:** Result collection, parallel processing aggregation

**Related Terms:** [data_distribute](#data_distribute), [data_transform](#data_transform)

**Book Chapter:** Data Flow Patterns (Chapter 13)

---

### data_distribute
**Definition:** WDP-03 - Distributes data from a source task to multiple recipient tasks.

**Module:** `data_distribute.erl`

**Distribution Types:**
- broadcast - Send to all recipients
- round_robin - Distribute sequentially
- partitioned - Split by key

**Related Terms:** [data_accumulate](#data_accumulate), [data_visibility](#data_visibility)

**Book Chapter:** Data Flow Patterns (Chapter 13)

---

### data_transform
**Definition:** WDP-02 - Transforms data between tasks using custom transformation functions.

**Module:** `data_transform.erl`

**Usage Context:** Format conversion, data enrichment, validation

**Related Terms:** [parameter_passing](#parameter_passing), [data_visibility](#data_visibility)

**Book Chapter:** Data Flow Patterns (Chapter 13)

---

### data_visibility
**Definition:** WDP-05 - Controls data visibility scope within a workflow (local, branch, global).

**Module:** `data_visibility.erl`

**Scopes:**
- local - Single task only
- branch - All tasks in current branch
- global - Entire workflow

**Related Terms:** [data_accumulate](#data_accumulate), [data_distribute](#data_distribute)

**Book Chapter:** Data Flow Patterns (Chapter 13)

---

### deferred_choice
**Definition:** Pattern P16 - Defers branch selection until runtime based on data availability or external events.

**Module:** `deferred_choice.erl`

**Usage Context:** Adaptive workflows, runtime strategy selection

**Related Terms:** [exclusive_choice](#exclusive_choice), [multiple_choice](#multiple_choice)

**Book Chapter:** State-Based Patterns (Chapter 7)

---

### determinant (nondeterminism)
**Definition:** A function that converts nondeterministic choice into deterministic selection using a fixed seed.

**Usage Context:** Reproducible workflow execution, testing

**Module:** `wf_choice.erl`

**Related Terms:** [nondeterministic](#nondeterministic), [seed](#seed)

**Book Chapter:** Pattern Implementation (Chapter 14)

---

### discriminator
**Definition:** Pattern P9 - Waits for the first incoming branch to complete, then produces output without waiting for others.

**Module:** `discriminator.erl`

**Petri Net Structure:**
```
p_branch1 ──┐
p_branch2 ──┼──► t_discriminate ──► p_selected
p_branch3 ──┘               └──► p_discarded
```

**Related Terms:** [blocking_discriminator](#blocking_discriminator), [cancelling_discriminator](#cancelling_discriminator)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

### drain
**Definition:** API function that executes transitions iteratively until no more are enabled or a step limit is reached.

**Signature:**
```erlang
-spec drain(Name :: name(), MaxSteps :: non_neg_integer()) ->
          {ok, [Receipt]} | {error, limit}.
```

**Example:**
```erlang
{ok, Receipts} = gen_pnet:drain(Pid, 100).
```

**Related Terms:** [step](#step), [progress_loop](#progress_loop), [quiescence](#quiescence)

**Book Chapter:** gen_pnet API (Chapter 10)

---

---

## E

### enabled
**Definition:** A transition is enabled when all required input tokens are available according to its mode and the current marking.

**Check Function:**
```erlang
-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              boolean().
```

**Example:**
```erlang
% Transition t1 is enabled if p1 has at least one token
is_enabled(t1, #{p1 := [_]}, _UsrInfo) -> true.
```

**Related Terms:** [firing](#firing), [mode](#mode), [marking](#marking), [preset](#preset)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

### enum_modes
**Definition:** Function that generates all possible firing modes for a transition given its preset places and current marking.

**Module:** `pnet_mode.erl`

**Signature:**
```erlang
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) -> [mode()].
```

**Example:**
```erlang
> pnet_mode:enum_modes([p1, p2], #{p1 => [a,b], p2 => [x]}).
[#{p1 => [a], p2 => [x]}, #{p1 => [b], p2 => [x]}]
```

**Related Terms:** [mode](#mode), [preset](#preset), [marking](#marking)

**Book Chapter:** Mode Enumeration (Chapter 8)

---

### exclusive_choice
**Definition:** Pattern P4 - Selects exactly one branch from multiple alternatives based on conditions (XOR split semantics).

**Module:** `exclusive_choice.erl`

**Petri Net Structure:**
```
        ┌──► p_branch1 (condition A)
p_start ──┼──► p_branch2 (condition B)
        └──► p_branch3 (condition C)
```

**Related Terms:** [simple_merge](#simple_merge), [multiple_choice](#multiple_choice)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

### explicit_termination
**Definition:** Pattern P43 - Explicitly terminates workflow execution when a designated termination condition is reached.

**Module:** `explicit_termination.erl`

**Related Terms:** [implicit_termination](#implicit_termination), [cancel_case](#cancel_case)

**Book Chapter:** Advanced Control Flow (Chapter 7)

---

---

## F

### firing
**Definition:** The execution of an enabled transition, which consumes input tokens and produces output tokens according to the transition's rules.

**Process:**
1. Check if transition is enabled
2. Select a mode (which tokens to consume)
3. Execute fire/3 callback
4. Consume tokens from preset places
5. Produce tokens to postset places
6. Execute trigger/3 for side effects

**Example:**
```erlang
fire(t1, #{p1 => [a]}, _UsrInfo) ->
    {produce, #{p2 => [result]}}.
```

**Related Terms:** [transition](#transition), [mode](#mode), [marking](#marking), [enabled](#enabled)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

---

## G

### gen_pnet
**Definition:** The core OTP gen_server behavior module that implements Petri net workflow execution. It is the "single runtime component" in Joe Armstrong's design principle.

**Key Features:**
- Automatic transition firing via progress loop
- Receipt-based audit trail
- Statistics tracking (fps, token counts)
- Token injection and inspection APIs

**Module:** `gen_pnet.erl`

**Related Terms:** [gen_yawl](#gen_yawl), [net_state](#net-state), [progress_loop](#progress_loop)

**Book Chapter:** gen_pnet Reference (Chapter 10)

---

### gen_yawl
**Definition:** A wrapper around gen_pnet that extends the fire/3 callback to return 3-tuple format with enhanced reply handling.

**Enhanced Return Values:**
```erlang
{produce, ProduceMap}
{produce, ProduceMap, NewUsrInfo}
abort
```

**Module:** `gen_yawl.erl`

**Related Terms:** [gen_pnet](#gen_pnet), [fire/3](#firing)

**Book Chapter:** YAWL Integration (Chapter 11)

---

### generalized_and_join
**Definition:** Pattern P33 - An AND join that synchronizes a dynamic set of branches, waiting for all specified branches to complete.

**Module:** `generalized_and_join.erl`

**Related Terms:** [synchronization](#synchronization), [partial_join](#partial_join)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

### goal_state (verification)
**Definition:** The target marking or condition that a workflow should reach to satisfy correctness properties.

**Usage Context:** Verification proofs, model checking

**Related Terms:** [verification](#verification), [proof](#proof), [invariant](#invariant)

**Book Chapter:** Verification (Chapter 9)

---

## H

### hash (marking)
**Definition:** A canonical SHA-256 hash of a marking, computed from sorted places and tokens to provide order-independent state comparison.

**Module:** `pnet_marking.erl`

**Signature:**
```erlang
-spec hash(Marking :: marking()) -> binary().
```

**Example:**
```erlang
> pnet_marking:hash(#{p => [a,b]}).
> pnet_marking:hash(#{p => [b,a]}).
<<98,247,139,121,...>>  % Same hash!
```

**Related Terms:** [marking](#marking), [receipt](#receipt), [state_comparison](#state-comparison)

**Book Chapter:** Type System (Chapter 8)

---

### human-in-the-loop
**Definition:** A workflow pattern where certain tasks require human intervention or decision making, typically via LLM agent interaction.

**Usage Context:** Approval workflows, decision gates, exception handling

**Components:**
- Role-to-agent mapping
- Task injection interface
- Decision extraction

**Related Terms:** [agent](#agent), [role](#role), [task](#task), [swarm](#swarm)

**Book Chapter:** Swarm Coordination (Chapter 12)

---

---

## I

### implicit_termination
**Definition:** Pattern P11 - Terminates a subprocess automatically when no work remains and all inputs are satisfied.

**Module:** `implicit_termination.erl`

**Related Terms:** [explicit_termination](#explicit_termination), [milestone](#milestone)

**Book Chapter:** Multiple Instance Patterns (Chapter 7)

---

### insufficient
**Definition:** Error returned when trying to consume more tokens than available in a marking.

**Example:**
```erlang
> pnet_marking:take(#{p => [a]}, #{p => [a,a]}).
{error, insufficient}
```

**Related Terms:** [bad_place](#bad_place), [consume_map](#consume_map), [multiset](#multiset)

**Book Chapter:** Type System (Chapter 8)

---

### inject
**Definition:** API function that manually adds tokens to specified places in a running Petri net.

**Signature:**
```erlang
-spec inject(Name :: name(), ProduceMap :: #{atom() => [_]}) ->
          {ok, Receipt} | {error, Reason}.
```

**Usage Context:** External event handling, human task completion

**Example:**
```erlang
{ok, Receipt} = gen_pnet:inject(Pid, #{task_place => [task_result]}).
```

**Related Terms:** [produce_map](#produce_map), [step](#step), [receipt](#receipt)

**Book Chapter:** gen_pnet API (Chapter 10)

---

### interleaved_routing
**Definition:** Pattern P17/P40 - Executes multiple branches in interleaved, non-deterministic order rather than true parallelism.

**Module:** `interleaved_routing.erl`

**Usage Context:** Fair scheduling, resource sharing

**Related Terms:** [parallel_split](#parallel_split), [critical_section](#critical_section)

**Book Chapter:** State-Based Patterns (Chapter 7)

---

### invariant
**Definition:** A property that must hold true for all reachable markings in a Petri net, used for verification and correctness proofs.

**Example:**
```erlang
% "p_done and p_running are mutually exclusive"
invariant(Marking) ->
    not ((maps:get(p_done, Marking, []) =/= []) andalso
          (maps:get(p_running, Marking, []) =/= [])).
```

**Related Terms:** [verification](#verification), [proof](#proof), [marking](#marking)

**Book Chapter:** Verification (Chapter 9)

---

## L

### local_sync_merge
**Definition:** Pattern P37 - Synchronizes branches within a local scope before proceeding.

**Module:** `local_sync_merge.erl`

**Related Terms:** [general_sync_merge](#general_sync_merge), [synchronization](#synchronization)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

### ls (place query)
**Definition:** API function that queries the tokens at a specific place.

**Signature:**
```erlang
-spec ls(Name :: name(), Place :: atom()) -> {ok, [token()]} | {error, #bad_place{}}.
```

**Example:**
```erlang
{ok, Tokens} = gen_pnet:ls(Pid, place1).
```

**Related Terms:** [marking](#marking), [place](#place), [token](#token)

**Book Chapter:** gen_pnet API (Chapter 10)

---

---

## M

### marking
**Definition:** The state of a Petri net, represented as a map from places to lists of tokens (multisets).

**Type Signature:**
```erlang
-type marking() :: #{place() => [token()]}.
```

**Example:**
```erlang
Marking = #{
    start => [init],
    processing => [task1, task2],
    done => []
}
```

**Related Terms:** [place](#place), [token](#token), [multiset](#multiset), [mode](#mode)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

### marking_algebra
**Definition:** The mathematical system of operations on markings, including addition, subtraction, and atomic apply operations.

**Module:** `pnet_marking.erl`

**Operations:**
- `new/1` - Create empty marking
- `add/2` - Multiset union (append)
- `take/2` - Multiset subtraction
- `apply/3` - Atomic consume+produce
- `hash/1` - Canonical hash

**Related Terms:** [multiset](#multiset), [marking](#marking), [atomic](#atomic)

**Book Chapter:** Marking Algebra (Chapter 8)

---

### milestone
**Definition:** Pattern P18 - A state guard that enables an activity only when a specific milestone (condition) has been reached.

**Module:** `milestone.erl`

**Usage Context:** Conditional workflow progression, state-dependent execution

**Petri Net Structure:**
```
p_guard ──► t_set_milestone ──► p_reached ───► t_enable ──► p_active ───► t_complete ──► p_done
```

**Related Terms:** [cancel_activity](#cancel_activity), [deferred_choice](#deferred_choice)

**Book Chapter:** State-Based Patterns (Chapter 7)

---

### mode
**Definition:** A specification of which tokens to consume from each input place when a transition fires. Represented as a map from places to token lists.

**Type Signature:**
```erlang
-type mode() :: #{place() => [token()]}.
```

**Example:**
```erlang
Mode = #{p1 => [a], p2 => [b, c]}.
```

**Related Terms:** [cmode](#cmode), [consume_map](#consume_map), [marking](#marking), [firing](#firing)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

### move
**Definition:** A complete transition firing operation, containing the transition, mode (tokens to consume), and produce map (tokens to produce).

**Type Signature:**
```erlang
-type move() :: #{trsn := trsn(),
                  mode := mode() | cmode(),
                  produce := produce_map()}.
```

**Related Terms:** [firing](#firing), [receipt](#receipt), [transition](#transition)

**Book Chapter:** Type System (Chapter 8)

---

### multiset
**Definition:** A collection that allows duplicate elements, where multiplicity matters but order doesn't. Used for token lists in markings.

**Properties:**
- [a, b, a] is a multiset with element 'a' having multiplicity 2
- Order-invariant: [a, b] = [b, a] as multisets
- Addition: [a] + [a, b] = [a, a, b]
- Subtraction: [a, a, b] - [a] = [a, b]

**Related Terms:** [marking](#marking), [marking_algebra](#marking-algebra), [token](#token)

**Book Chapter:** Marking Algebra (Chapter 8)

---

### multiple_choice
**Definition:** Pattern P6 - Enables multiple branches simultaneously based on conditions (OR split semantics).

**Module:** `multiple_choice.erl`

**Petri Net Structure:**
```
        ┌──► p_branch1
p_start ──┼──► p_branch2
        └──► p_branch3
```

**Related Terms:** [exclusive_choice](#exclusive_choice), [simple_merge](#simple_merge)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

### multiple_instances_sync
**Definition:** Patterns P12-P15 - Create and manage concurrent instances of activities, with various synchronization strategies.

**Variants:**
- P12: No synchronization (fire-and-forget)
- P13: Design-time knowledge (static count)
- P14: Runtime-known count (dynamic but pre-determined)
- P15: Runtime-unknown count (fully dynamic)

**Module:** `multiple_instances_sync.erl`

**Related Terms:** [partial_join](#partial_join), [cancel_mi_activity](#cancel_mi_activity)

**Book Chapter:** Multiple Instance Patterns (Chapter 7)

---

### mutex
**Definition:** Mutual exclusion lock ensuring only one process can access a resource at a time.

**Implementation:** Pattern P39 (critical_section)

**Related Terms:** [critical_section](#critical_section), [resource](#resource)

**Book Chapter:** Extended Control Flow (Chapter 7)

---

## N

### net_state
**Definition:** The internal state record maintained by gen_pnet, containing the marking, callback module, user info, and statistics.

**Record Definition:**
```erlang
-record(net_state, {
    marking   :: #{place() => [token()]},
    net_mod   :: atom(),
    usr_info  :: term(),
    stats     :: #stats{} | undefined,
    tstart    :: integer(),
    cnt       :: non_neg_integer()
}).
```

**Related Terms:** [marking](#marking), [gen_pnet](#gen_pnet), [usr_info](#usr_info)

**Book Chapter:** gen_pnet Reference (Chapter 10)

---

### net_module
**Definition:** A module implementing the gen_pnet behavior callbacks that defines a specific Petri net structure and execution semantics.

**Requirements:**
- Implement all 6 structure callbacks
- Implement all 7 interface callbacks
- Export the required functions

**Related Terms:** [gen_pnet](#gen_pnet), [callback](#callback), [pattern](#pattern)

**Book Chapter:** gen_pnet Reference (Chapter 10)

---

### nondeterministic
**Definition:** Property of a system where multiple execution paths are possible and the choice is not predetermined.

**In CRE:** Controlled via deterministic choice using seeds for reproducibility.

**Related Terms:** [determinant](#determinant), [mode](#mode), [seed](#seed)

**Book Chapter:** Pattern Implementation (Chapter 14)

---

## O

### or_join
**Definition:** A join type that proceeds when ANY incoming branch completes, without waiting for others.

**Related Terms:** [and_join](#and_join), [xor_join](#xor_join), [discriminator](#discriminator)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

---

## P

### parallel_split
**Definition:** Pattern P2 - Splits workflow execution into multiple parallel branches that execute concurrently.

**Module:** `parallel_split.erl`

**Petri Net Structure:**
```
p_start ──► t_split ──┬──► p_branch1
                       ├──► p_branch2
                       └──► p_branch3
```

**Related Terms:** [synchronization](#synchronization), [exclusive_choice](#exclusive_choice)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

### parameter_passing
**Definition:** WDP-01 - Passes parameters (data) between tasks in a workflow.

**Module:** Implemented as data flow in transitions

**Related Terms:** [data_transform](#data_transform), [token](#token)

**Book Chapter:** Data Flow Patterns (Chapter 13)

---

### partial_join
**Definition:** Patterns P30-P36 - Wait for a subset (quorum) of parallel branches to complete before proceeding.

**Variants:**
- P30: Structured partial join
- P31: Blocking partial join
- P32: Cancelling partial join
- P34-P36: Multi-instance partial joins

**Module:** `structured_partial_join.erl`, `blocking_partial_join.erl`, etc.

**Related Terms:** [synchronization](#synchronization), [quorum](#quorum)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

### pattern (workflow)
**Definition:** A reusable workflow structure that solves a common problem, implemented as a gen_yawl behavior module.

**43 Patterns:** All from Van der Aalst taxonomy

**Related Terms:** [pattern_registry](#pattern_registry), [pattern_expansion](#pattern-expansion), [YAML spec](#yaml-spec)

**Book Chapter:** Pattern Reference (Chapter 14)

---

### pattern_expansion
**Definition:** The process of replacing a pattern macro in a YAML specification with its full Petri net structure (places, transitions, flows).

**Module:** `yawl_pattern_expander.erl`

**Example:**
```yaml
pattern_instances:
  - P2_ParallelSplit:
      id: split1
# Expands to full Petri net with places and transitions
```

**Related Terms:** [pattern_registry](#pattern_registry), [yawl_compile](#yawl_compile), [YAML spec](#yaml-spec)

**Book Chapter:** Compilation (Chapter 11)

---

### pattern_registry
**Definition:** A pure helper module that maps pattern identifiers (P1_Sequence, etc.) to their implementing module names.

**Module:** `yawl_pattern_registry.erl`

**Key Functions:**
- `pattern_module/1` - Get module for pattern ID
- `all_patterns/0` - List all 43 patterns
- `validate_pattern/1` - Check if pattern exists

**Example:**
```erlang
> yawl_pattern_registry:pattern_module(<<"P1_Sequence">>).
sequence
```

**Related Terms:** [pattern](#pattern), [pattern_expansion](#pattern-expansion)

**Book Chapter:** Pattern Implementation (Chapter 14)

---

### persist (checkpoint)
**Definition:** YAWL recovery module providing checkpoint/resume functionality for workflow instances.

**Module:** `yawl_recovery.erl`

**Operations:**
- `checkpoint/4,5` - Save workflow state
- `resume/3` - Restore from checkpoint
- `list_checkpoints/2` - Query available checkpoints
- `delete_checkpoint/3` - Remove checkpoint

**Record:**
```erlang
-record(yawl_checkpoint, {
    checkpoint_id,
    spec_id,
    case_id,
    marking,
    data,
    timestamp,
    version
}).
```

**Related Terms:** [receipt](#receipt), [verification](#verification), [replay](#replay)

**Book Chapter:** Recovery and Persistence (Chapter 16)

---

### place
**Definition:** A node in a Petri net that holds tokens. Represents a state location or data buffer.

**Type Signature:**
```erlang
-type place() :: atom().
```

**Examples:** `start`, `processing`, `done`, `p1`

**Related Terms:** [token](#token), [marking](#marking), [transition](#transition)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

### preset
**Definition:** The set of input places for a transition, i.e., the places from which tokens are consumed when the transition fires.

**Callback:**
```erlang
-callback preset(Trsn :: atom()) -> [place()].
```

**Example:**
```erlang
preset(t1) -> [input, control].
```

**Related Terms:** [postset](#postset), [transition](#transition), [mode](#mode)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

### produce_map
**Definition:** A map specifying which tokens to add to which places during transition firing.

**Type Signature:**
```erlang
-type produce_map() :: #{place() => [token()]}.
```

**Example:**
```erlang
ProduceMap = #{output => [result], log => [entry]}.
```

**Related Terms:** [consume_map](#consume_map), [firing](#firing), [marking](#marking)

**Book Chapter:** Type System (Chapter 8)

---

### progress_loop
**Definition:** The automatic execution loop in gen_pnet that continuously finds and fires enabled transitions.

**Implementation:**
- Sends `continue` messages to self
- Calls `progress/1` to find enabled transition
- Fires transition and updates marking
- Tracks statistics

**Related Terms:** [drain](#drain), [step](#step), [firing](#firing)

**Book Chapter:** gen_pnet Reference (Chapter 10)

---

### proof (verification)
**Definition:** A formal argument demonstrating that a workflow satisfies its specification, typically using invariants and state exploration.

**Method:** Receipt replay + invariant checking

**Related Terms:** [verification](#verification), [receipt](#receipt), [invariant](#invariant), [refusal_trace](#refusal-trace)

**Book Chapter:** Verification (Chapter 9)

---

### pnet_types
**Definition:** Module providing total (never-crashing) type validation functions for Petri net data structures.

**Module:** `pnet_types.erl`

**Validators:**
- `is_marking/1`
- `is_mode/1`
- `is_consume_map/1`
- `is_produce_map/1`
- `is_binding/1`
- `is_cmode/1`
- `is_move/1`
- `is_receipt/1`

**Related Terms:** [type_validation](#type-validation), [marking](#marking), [mode](#mode)

**Book Chapter:** Type System (Chapter 8)

---

---

## Q

### quiescence
**Definition:** The state of a Petri net when no transitions are enabled, meaning the workflow cannot progress further without external intervention.

**Detection:**
```erlang
case gen_pnet:step(Pid) of
    abort -> quiescent;
    {ok, _Receipt} -> still_running
end.
```

**Related Terms:** [step](#step), [drain](#drain), [enabled](#enabled)

**Book Chapter:** Petri Net Execution (Chapter 4)

---

### quorum
**Definition:** The minimum number of agents or branches that must agree before a decision can be made in partial join patterns.

**Usage Context:** Distributed decision making, voting systems

**Related Terms:** [partial_join](#partial_join), [consensus](#consensus)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

## R

### receipt
**Definition:** A record of a transition firing, containing hashes of the marking before/after, the move executed, and a timestamp.

**Type Signature:**
```erlang
-type receipt() :: #{before_hash := binary(),
                     after_hash := binary(),
                     move := move(),
                     ts := integer()}.
```

**Usage:** Audit trails, verification, recovery

**Related Terms:** [move](#move), [firing](#firing), [verification](#verification)

**Book Chapter:** Type System (Chapter 8)

---

### recursion
**Definition:** Pattern P22 - Enables a workflow to call itself with modified parameters, implementing recursive algorithms.

**Module:** `recursion.erl`

**Petri Net Structure:**
```
p_start ──►┬──► t_recursive_call ──►► p_recursive ───►► t_return ───► p_result
          │                                              │
          └──► t_base_case ───► p_base ─�───────────────────────┘
```

**Related Terms:** [structured_loop](#structured_loop), [milestone](#milestone)

**Book Chapter:** Extended Control Flow (Chapter 7)

---

### refusal_trace
**Definition:** A sequence of refusals (alternative choices not taken) that explains why a particular execution path was or wasn't taken, used for verification analysis.

**Related Terms:** [verification](#verification), [proof](#proof), [nondeterministic](#nondeterministic)

**Book Chapter:** Verification (Chapter 9)

---

### reply (gen_server)
**Definition:** Response sent from a gen_server callback to a synchronous call, optionally with marking updates.

**Return Values:**
- `{reply, Reply}` - Reply without state change
- `{reply, Reply, ProduceMap}` - Reply with token production
- `noreply` - Defer reply
- `{noreply, ProduceMap}` - Defer and produce

**Related Terms:** [handle_call](#handle_call), [produce_map](#produce_map)

**Book Chapter:** gen_pnet API (Chapter 10)

---

### resource (pattern)
**Definition:** WRP patterns - manage allocation and use of resources in workflows.

**Patterns:**
- WRP-01: Resource creation
- WRP-02: Role allocation
- WRP-03: Resource start
- WRP-04: Role distribution
- WRP-05: Capability allocation

**Related Terms:** [role](#role), [capability](#capability), [critical_section](#critical_section)

**Book Chapter:** Resource Patterns (Chapter 13)

---

### replay (verification)
**Definition:** Re-executing a workflow from a saved state (receipts or checkpoints) to verify correctness or reproduce behavior.

**Module:** `yawl_recovery.erl` (via `resume/3`)

**Related Terms:** [receipt](#receipt), [checkpoint](#checkpoint), [verification](#verification)

**Book Chapter:** Verification (Chapter 9)

---

### role
**Definition:** A functional responsibility in a workflow that may be assigned to an agent (human or LLM).

**Examples:**
- ProgramChair
- Reviewer
- SafetyOfficer

**Usage Context:** Swarm coordination, task assignment

**Related Terms:** [agent](#agent), [task](#task), [swarm](#swarm)

**Book Chapter:** Swarm Coordination (Chapter 12)

---

## S

### seed (determinism)
**Definition:** A numeric value used to initialize the random number generator, ensuring reproducible nondeterministic choice.

**Usage:**
```erlang
yawl_compile:compile(Spec, #{seed => 12345}).
```

**Related Terms:** [determinant](#determinant), [nondeterministic](#nondeterministic)

**Book Chapter:** Pattern Implementation (Chapter 14)

---

### sequence
**Definition:** Pattern P1 - Execute tasks in strict sequential order, one after another.

**Module:** `sequence.erl`

**Petri Net Structure:**
```
p_start ──► t1 ──► p_step1 ──► t2 ──► p_step2 ──► p_end
```

**Related Terms:** [arbitrary_cycles](#arbitrary_cycles), [structured_loop](#structured_loop)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

### simple_merge
**Definition:** Pattern P5 - Merges multiple incoming paths into one output using XOR semantics (only one path should be active).

**Module:** `simple_merge.erl`

**Petri Net Structure:**
```
p_branch1 ──┐
p_branch2 ──┼──► t_merge ──► p_output
p_branch3 ──┘
```

**Related Terms:** [exclusive_choice](#exclusive_choice), [multiple_merge](#multiple_merge)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

### spawn (instance)
**Definition:** Creating a new process or instance for parallel execution in multi-instance patterns.

**Related Terms:** [multiple_instances](#multiple_instances), [instance_pool](#instance-pool)

**Book Chapter:** Multiple Instance Patterns (Chapter 7)

---

### state_property
**Definition:** API function that checks if a predicate holds for the current marking.

**Signature:**
```erlang
-spec state_property(Name :: name(),
                     Pred :: fun((...) -> ok | {error, _}),
                     PlaceLst :: [atom()]) ->
          ok | {error, _}.
```

**Example:**
```erlang
Pred = fun([Tokens]) -> case length(Tokens) of 1 -> ok; _ -> {error, bad_count} end end,
ok = gen_pnet:state_property(Pid, Pred, [place1]).
```

**Related Terms:** [marking](#marking), [verification](#verification), [invariant](#invariant)

**Book Chapter:** gen_pnet API (Chapter 10)

---

### step
**Definition:** API function that fires at most one enabled transition, returning the receipt or `abort` if none enabled.

**Signature:**
```erlang
-spec step(Name :: name()) -> abort | {ok, Receipt}.
```

**Example:**
```erlang
case gen_pnet:step(Pid) of
    abort -> no_progress;
    {ok, Receipt} -> transition_fired
end.
```

**Related Terms:** [drain](#drain), [firing](#firing), [progress_loop](#progress_loop)

**Book Chapter:** gen_pnet API (Chapter 10)

---

### structured_loop
**Definition:** Pattern P21 - Implements while/until loop constructs with proper Petri net semantics.

**Module:** `structured_loop.erl`

**While Loop Structure:**
```
p_init ──► t_enter ──► p_body ───► t_eval ───► p_cond ───┬─► t_again ──┐
                                   │                      │           │
                                   └───────────────────────┴─► t_exit ───► p_done
```

**Related Terms:** [arbitrary_cycles](#arbitrary_cycles), [recursion](#recursion)

**Book Chapter:** Extended Control Flow (Chapter 7)

---

### structured_partial_join
**Definition:** Pattern P30 - Wait for a subset (quorum) of parallel branches to complete.

**Module:** `structured_partial_join.erl`

**Related Terms:** [blocking_partial_join](#blocking_partial_join), [generalized_and_join](#generalized_and_join)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

### structured_sync_merge
**Definition:** Pattern P7 - Merges multiple branches with AND semantics while maintaining partial result availability.

**Module:** `structured_sync_merge.erl`

**Related Terms:** [synchronization](#synchronization), [multiple_merge](#multiple_merge)

**Book Chapter:** Advanced Synchronization (Chapter 6)

---

### swarm
**Definition:** A collection of agents working together on a workflow through coordination patterns, with shared goals and distributed execution.

**CRE Implementation:**
- AGI Symposium Omega specification
- 20 roles with agents
- 5 interconnected nets
- 43 pattern instances

**Related Terms:** [agent](#agent), [role](#role), [coordination](#coordination), [workflow](#workflow)

**Book Chapter:** Swarm Coordination (Chapter 12)

---

### synchronization
**Definition:** Pattern P3 - Wait for all parallel branches to complete before proceeding (AND join semantics).

**Module:** `synchronization.erl`

**Petri Net Structure:**
```
p_branch1 ──┐
p_branch2 ──┼──► t_sync ──► p_merged
p_branch3 ──┘
```

**Related Terms:** [parallel_split](#parallel_split), [generalized_and_join](#generalized_and_join)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

---

## T

### task
**Definition:** A unit of work in a workflow, corresponding to a YAWL task that may be automated, manual, or composite.

**Types:**
- Atomic: Single unit of work
- Composite: Contains sub-workflow
- Manual: Requires human intervention
- Decomposition: References external service

**Related Terms:** [workflow](#workflow), [agent](#agent), [role](#role)

**Book Chapter:** YAWL Specification (Chapter 11)

---

### thread_merge
**Definition:** Pattern P41 - Merges multiple execution threads into a single thread.

**Module:** `thread_merge.erl`

**Related Terms:** [thread_split](#thread_split), [synchronization](#synchronization)

**Book Chapter:** Advanced Control Flow (Chapter 7)

---

### thread_split
**Definition:** Pattern P42 - Splits workflow execution into multiple independent threads.

**Module:** `thread_split.erl`

**Related Terms:** [thread_merge](#thread_merge), [parallel_split](#parallel_split)

**Book Chapter:** Advanced Control Flow (Chapter 7)

---

### token
**Definition:** The fundamental unit of information in a Petri net, representing workflow state or data. Can be any Erlang term.

**Type Signature:**
```erlang
-type token() :: term().
```

**Examples:** `init`, `{user, alice}`, `#{order_id => 123}`, `[1, 2, 3]`

**Related Terms:** [place](#place), [marking](#marking), [multiset](#multiset)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

### trace (execution)
**Definition:** A sequence of receipts representing the complete execution history of a workflow instance.

**Usage:** Verification, debugging, audit

**Related Terms:** [receipt](#receipt), [verification](#verification), [replay](#replay)

**Book Chapter:** Verification (Chapter 9)

---

### transition
**Definition:** A node in a Petri net that consumes tokens from input places and produces tokens to output places when enabled and fired.

**Type Signature:**
```erlang
-type trsn() :: atom().
```

**Components:**
- Preset (input places)
- Postset (output places)
- Guard/Mode (enablement condition)
- Fire action (token production)

**Related Terms:** [place](#place), [firing](#firing), [mode](#mode), [preset](#preset)

**Book Chapter:** Petri Net Theory (Chapter 3)

---

### trigger
**Definition:** Callback that executes side effects when tokens are produced, allowing the net module to filter (pass/drop) individual tokens.

**Signature:**
```erlang
-callback trigger(Place :: atom(), Token :: term(), NetState :: #net_state{}) ->
              pass | drop.
```

**Usage:** Event emission, external integration, logging

**Related Terms:** [firing](#firing), [produce_map](#produce_map)

**Book Chapter:** gen_pnet Reference (Chapter 10)

---

### transient_trigger
**Definition:** Pattern P23 - A trigger that activates once and then becomes inactive.

**Module:** `transient_trigger.erl`

**Related Terms:** [persistent_trigger](#persistent_trigger), [milestone](#milestone)

**Book Chapter:** State-Based Patterns (Chapter 7)

---

---

## U

### usr_info
**Definition:** User-defined state passed through the net instance, accessible to all callbacks and via API query.

**Type:** Any Erlang term (application-defined)

**Access:**
```erlang
% Get via API
UsrInfo = gen_pnet:usr_info(Pid).

% Access in callback
init_marking(Place, UsrInfo) -> ... use UsrInfo ...
```

**Related Terms:** [net_state](#net-state), [gen_pnet](#gen_pnet), [callback](#callback)

**Book Chapter:** gen_pnet Reference (Chapter 10)

---

## V

### verification
**Definition:** The process of proving that a workflow satisfies its correctness properties, using receipts, invariants, and model checking.

**Tools:**
- Receipt replay
- Invariant checking
- Refusal trace analysis
- State space exploration

**Module:** `pnet_verification.erl`

**Related Terms:** [receipt](#receipt), [proof](#proof), [refusal_trace](#refusal-trace), [invariant](#invariant)

**Book Chapter:** Verification (Chapter 9)

---

---

## W

### workflow
**Definition:** A collection of tasks and patterns forming a business process, modeled as a Petri net and executable via gen_pnet.

**Components:**
- Tasks (atomic, composite, manual)
- Flows (connections between tasks)
- Conditions (decision points)
- Data flow (parameter passing)
- Patterns (reusable structures)

**Related Terms:** [task](#task), [pattern](#pattern), [YAML_spec](#yaml-spec)

**Book Chapter:** YAWL Specification (Chapter 11)

---

### YAWL (Yet Another Workflow Language)
**Definition:** A workflow language based on Petri nets with advanced features for data flow, resources, and exception handling. CRE implements YAWL 2.1/2.2.

**Key Features:**
- XML and YAML specifications
- 43 workflow control patterns
- Composite tasks
- Multi-instance activities
- Cancellation regions
- Data visibility

**Related Terms:** [pattern](#pattern), [YAML spec](#yaml-spec), [compilation](#compilation)

**Book Chapter:** YAWL Reference (Chapter 11)

---

### YAML_spec
**Definition:** YAML 0.2 format for specifying YAWL workflows with pattern macros, tasks, flows, and roles.

**Structure:**
```yaml
yawl_yaml_version: "0.2"
name: "WorkflowName"
nets: [...]
pattern_instances:
  - P1_Sequence:
      id: seq1
      ...
tasks: [...]
flows: [...]
```

**Parser:** `wf_yaml_spec.erl`

**Related Terms:** [compilation](#compilation), [pattern_expansion](#pattern-expansion), [pattern_instance](#pattern-instance)

**Book Chapter:** YAWL Specification (Chapter 11)

---

## X

### xor_join
**Definition:** A join type that proceeds when ANY incoming branch completes, without synchronization requirements.

**Related Terms:** [or_join](#or_join), [simple_merge](#simple_merge), [discriminator](#discriminator)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

### xor_split
**Definition:** A split type that selects exactly one branch based on conditions (exclusive choice).

**Related Terms:** [exclusive_choice](#exclusive_choice), [or_split](#or_split), [and_split](#and_split)

**Book Chapter:** Basic Control Flow (Chapter 5)

---

---

## Numerical Index

### Pattern Quick Reference

| ID | Name | Category | Module |
|----|------|----------|--------|
| P1 | Sequence | Basic Control Flow | `sequence.erl` |
| P2 | Parallel Split | Basic Control Flow | `parallel_split.erl` |
| P3 | Synchronization | Basic Control Flow | `synchronization.erl` |
| P4 | Exclusive Choice | Basic Control Flow | `exclusive_choice.erl` |
| P5 | Simple Merge | Basic Control Flow | `simple_merge.erl` |
| P6 | Multiple Choice | Basic Control Flow | `multiple_choice.erl` |
| P7 | Structured Sync Merge | Advanced Sync | `structured_sync_merge.erl` |
| P8 | Multiple Merge | Advanced Sync | `multiple_merge.erl` |
| P9 | Discriminator | Advanced Sync | `discriminator.erl` |
| P10 | Arbitrary Cycles | Advanced Control Flow | `arbitrary_cycles.erl` |
| P11 | Implicit Termination | Multiple Instances | `implicit_termination.erl` |
| P12 | MI No Sync | Multiple Instances | `multiple_instances_sync.erl` |
| P13 | MI Design Time | Multiple Instances | `multiple_instances_sync.erl` |
| P14 | MI Runtime Known | Multiple Instances | `multiple_instances_sync.erl` |
| P15 | MI Runtime Unknown | Multiple Instances | `multiple_instances_sync.erl` |
| P16 | Deferred Choice | State-Based | `deferred_choice.erl` |
| P17 | Interleaved Parallel Routing | State-Based | `interleaved_routing.erl` |
| P18 | Milestone | State-Based | `milestone.erl` |
| P19 | Cancel Activity | State-Based | `cancel_activity.erl` |
| P20 | Cancel Case | State-Based | `cancel_case.erl` |
| P21 | Structured Loop | Extended Control Flow | `structured_loop.erl` |
| P22 | Recursion | Extended Control Flow | `recursion.erl` |
| P23 | Transient Trigger | State-Based | `transient_trigger.erl` |
| P24 | Persistent Trigger | State-Based | `persistent_trigger.erl` |
| P25 | Cancel Region | Extended Control Flow | `cancel_region.erl` |
| P26 | Cancel MI Activity | Extended Control Flow | `cancel_mi_activity.erl` |
| P27 | Complete MI Activity | Extended Control Flow | `complete_mi_activity.erl` |
| P28 | Blocking Discriminator | Advanced Sync | `blocking_discriminator.erl` |
| P29 | Cancelling Discriminator | Advanced Sync | `cancelling_discriminator.erl` |
| P30 | Structured Partial Join | Advanced Sync | `structured_partial_join.erl` |
| P31 | Blocking Partial Join | Advanced Sync | `blocking_partial_join.erl` |
| P32 | Cancelling Partial Join | Advanced Sync | `cancelling_partial_join.erl` |
| P33 | Generalized AND Join | Advanced Sync | `generalized_and_join.erl` |
| P34 | Static Partial Join MI | Multiple Instances | `static_partial_join_mi.erl` |
| P35 | Cancelling Partial Join MI | Multiple Instances | `cancelling_partial_join_mi.erl` |
| P36 | Dynamic Partial Join MI | Multiple Instances | `dynamic_partial_join_mi.erl` |
| P37 | Local Sync Merge | Advanced Sync | `local_sync_merge.erl` |
| P38 | General Sync Merge | Advanced Sync | `general_sync_merge.erl` |
| P39 | Critical Section | Extended Control Flow | `critical_section.erl`` |
| P40 | Interleaved Routing | State-Based | `interleaved_routing.erl` |
| P41 | Thread Merge | Extended Control Flow | `thread_merge.erl` |
| P42 | Thread Split | Extended Control Flow | `thread_split.erl` |
| P43 | Explicit Termination | Extended Control Flow | `explicit_termination.erl` |

---

## Bibliography

1. **Petri Net Theory**
   - Petri, C. A. (1962). "Fundamentals of a theory of asynchronous information flow."
   - Reisig, W. (1985). "Petri Nets: An Introduction."

2. **Workflow Patterns**
   - van der Aalst, W. M. P., et al. (2003). "Workflow Patterns." *Distributed and Parallel Databases*, 14(1), 5-51.
   - Russell, N., et al. (2006). "Workflow Control-Flow Patterns: A Revised View." BPM Center Report.

3. **YAWL**
   - ter Hofstede, A. H. M., et al. (2016). *Modern Business Process Automation: YAWL and its Support Environment*. Springer.

4. **CRE System**
   - Armstrong, J. (2013). *Programming Erlang: Software for a Concurrent World*. Pragmatic Bookshelf.
   - CRE Team (2026). *CRE Architecture and Design*. Internal Documentation.

---

## Document Information

**Title:** Generative Analysis for Constructive Systems - Glossary
**Version:** 1.0
**Date:** 2026-02-07
**Status:** Complete

---

**End of Glossary**
