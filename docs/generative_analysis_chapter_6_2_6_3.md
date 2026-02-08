# Chapter 6.2-6.3: Workflow Patterns as Topology Generators

## 6.2 The 43 Patterns: Classification and Composition

The workflow pattern catalog developed by van der Aalst and colleagues represents one of the most comprehensive taxonomies of control flow structures in process-aware information systems. These 43 patterns are not merely descriptive categories; when treated as generative templates, they become topology generators that can instantiate executable Petri net structures from high-level specifications. In this section, we classify the patterns into coherent categories and establish the rules for their composition.

### 6.2.1 Pattern Taxonomy

The 43 patterns naturally organize into six functional categories based on their control flow semantics:

#### Basic Control Flow Patterns (WCP 1-8)

These patterns form the foundation of workflow specification. They are so fundamental that nearly every workflow language implements them directly.

**WCP-1: Sequence** - The simplest pattern, executing tasks in strict linear order. In Petri net terms, this is a straight path of places connected by transitions. The topology is trivial but essential: `p_start -> t1 -> p1 -> t2 -> p2 -> ... -> p_end`.

**WCP-2: Parallel Split (AND-split)** - Divides execution into multiple concurrent branches. All branches become active simultaneously upon reaching the split transition. The topology fans out from a single place to N parallel paths.

**WCP-3: Synchronization (AND-join)** - The counterpart to parallel split, waiting for all concurrent branches to complete before proceeding. This requires N input places converging on a single synchronization transition that is only enabled when all input places contain tokens.

**WCP-4: Exclusive Choice (XOR-split)** - Selects exactly one branch from multiple alternatives based on runtime conditions. Unlike parallel split, only one path receives a token. The topology includes a branching transition with multiple output arcs, but guard conditions ensure mutual exclusion.

**WCP-5: Simple Merge (XOR-join)** - Combines multiple alternative paths without synchronization. Any incoming branch can proceed; the merge transition is enabled if any input place contains a token.

**WCP-6: Multiple Choice** - An advanced split where multiple branches may be selected simultaneously, but not necessarily all. This generalizes both parallel split (all branches) and exclusive choice (exactly one branch).

**WCP-7: Structured Synchronization Merge** - Combines multiple paths with synchronization semantics. Unlike simple merge, this pattern requires coordination to ensure proper completion semantics.

**WCP-8: Multiple Merge** - Merges multiple paths where each incoming branch is processed independently. The merge point does not distinguish which branch originated a token.

#### Structured Patterns (WCP 9-11, 21-22, 37-43)

These patterns enforce well-structuredness properties that enable formal verification and static analysis.

**WCP-9: Discriminator** - A specialized synchronization that triggers on the *first* branch completion but ignores subsequent completions. Exactly one output token is produced per N input tokens, regardless of completion order. The topology includes a counting mechanism to track first arrival.

**WCP-10: Arbitrary Cycles** - The general loop pattern allowing any place to transition back to a preceding place. This introduces cycles into the Petri net topology, requiring careful analysis to maintain soundness properties.

**WCP-11: Implicit Termination** - The workflow terminates when no further transitions are enabled. This is the default termination semantics in many Petri net formulations.

**WCP-21: Structured Loop** - A loop with a well-defined entry and exit point. The body can repeat multiple times, but control always exits through the designated exit transition. The topology forms a clear cycle with a single entry and exit.

**WCP-22: Recursion** - A workflow invokes itself as a subprocess. In Petri net terms, this requires place semantics that support nested markings or hierarchical net structures.

**WCP-37-43: Advanced Structured Patterns** - Including local synchronization merge, general synchronization merge, critical section, interleaved routing variants, and explicit termination. These patterns provide refined control over complex interactions.

#### Multiple Instance Patterns (WCP 12-15, 26-27, 34-36)

These patterns manage the creation and synchronization of multiple concurrent activity instances.

**WCP-12: Multiple Instance with a Priori Design Time Knowledge** - The number of instances is known at design time. A fixed N instances are created and synchronized.

**WCP-13: Multiple Instance with a Priori Runtime Knowledge** - The number of instances is determined at runtime but before instance creation. The topology includes a dynamic spawning transition based on runtime data.

**WCP-14: Multiple Instance without a Priori Runtime Knowledge** - Instances are created dynamically during execution based on data availability. This requires a self-referential topology where the instance creation transition can be re-enabled as new data arrives.

**WCP-15: Multiple Instance with a Critical Section** - Multiple instances execute concurrently but coordinate through a critical section for shared resource access. This combines parallel execution with mutual exclusion.

**WCP-26: Cancel MI Activity** - Cancels a specific multi-instance activity, terminating all its instances.

**WCP-27: Complete MI Activity** - Completes a multi-instance activity, possibly waiting for running instances to finish.

**WCP-34-36: Partial Join Patterns** - Variants of multi-instance synchronization with partial completion semantics, allowing workflow continuation before all instances complete.

#### State-Based Patterns (WCP 16-18)

These patterns rely on workflow state rather than control flow for routing decisions.

**WCP-16: Deferred Choice** - Multiple branches are enabled, but the choice is deferred until runtime based on which branch actually receives a triggering event. This differs from exclusive choice in that the decision point occurs later in execution.

**WCP-17: Interleaved Routing** - Multiple parallel routes are available, but only one can be active at a time. Execution interleaves between routes in non-deterministic order.

**WCP-18: Milestone** - An activity can only execute if a specified milestone (state condition) has been reached. The topology includes guard transitions that check milestone state before enabling the activity.

#### Cancellation Patterns (WCP 19-20, 23-25, 28-29, 31-32, 35)

These patterns provide controlled termination of workflow elements.

**WCP-19: Cancel Activity** - Terminates a specific running activity. The topology includes a cancellation transition that removes tokens from the activity's active place.

**WCP-20: Cancel Case** - Terminates the entire workflow instance. All active tokens are removed from all places.

**WCP-23-24: Trigger Patterns** - Transient and persistent triggers that can initiate or cancel activities based on external events.

**WCP-25: Cancel Region** - Cancels a scoped region of activities while leaving other parts of the workflow unaffected. The topology must maintain region boundaries to enable selective cancellation.

**WCP-28-29: Discriminator Variants with Cancellation** - Combining first-completion semantics with cancellation of remaining branches.

**WCP-31-32: Partial Join with Cancellation** - Partial completion joins that cancel remaining instances after reaching quorum.

**WCP-35: Cancelling Partial Join for MI** - Multi-instance partial join with cancellation of remaining instances.

#### Advanced Synchronization Patterns (WCP 30, 33, 38, 40-42)

These patterns provide sophisticated synchronization beyond basic AND-join.

**WCP-30: Structured Partial Join** - A partial join with well-defined semantics for how many branches must complete before proceeding.

**WCP-33: Generalized AND-Join** - A flexible synchronization pattern that allows specification of which branches must complete for the join to enable.

**WCP-38: General Synchronization Merge** - Combines multiple paths with configurable synchronization requirements.

**WCP-40-42: Thread Patterns** - Thread split, thread merge, and thread coordination patterns for managing concurrent execution streams.

### 6.2.2 Pattern Composition Rules

Individual patterns are building blocks. Real-world workflows require composing patterns into larger structures. Composition rules define how patterns can be combined while preserving soundness properties.

#### Sequential Composition

The simplest composition: the output place of pattern A becomes the input place of pattern B.

```
Pattern A: p_a_start -> ... -> p_a_end
Pattern B: p_b_start -> ... -> p_b_end

Compose: merge p_a_end with p_b_start
```

**Rule**: Sequential composition preserves soundness if both component patterns are sound. The merged place becomes the connection point.

#### Parallel Composition

Pattern A and Pattern B execute concurrently, triggered by a common split.

```
Pattern A: p_a_start -> ... -> p_a_end
Pattern B: p_b_start -> ... -> p_b_end

Compose: add split transition t_split with outputs to p_a_start and p_b_start
        add join transition t_join with inputs from p_a_end and p_b_end
```

**Rule**: Parallel composition preserves soundness if the split and join transitions form a properly matched pair. Every branch created by the split must eventually reach the join.

#### Nested Composition

Pattern B is embedded within a branch of Pattern A.

```
Pattern A: p_a_start -> (t_choice) -> p_branch1 -> ... -> p_a_end
                                      -> p_branch2 -> ...
Pattern B: p_b_start -> ... -> p_b_end

Compose: merge p_branch2 with p_b_start, p_b_end with continuation to p_a_end
```

**Rule**: Nested composition preserves soundness if the nested pattern's termination points align with the parent pattern's expectations. The nested pattern must be guaranteed to terminate if the parent is to terminate.

#### Composition with Cancellation

When composing patterns with cancellation semantics, special care is required.

```
Pattern A with Cancellation Region R
Pattern B embedded in R

Compose: B's transitions become part of R and are cancelled when R is cancelled
```

**Rule**: Cancellation scopes must form proper hierarchical regions. A pattern cannot simultaneously be inside and outside a cancellation region.

#### Soundness Preservation Theorem

**Theorem**: If patterns P1 and P2 are sound, and they are composed according to the rules above, then the composed pattern P is sound if and only if:
1. All connection places are properly merged (no dangling references)
2. All cycles have proper exit conditions (no infinite loops without progress)
3. All cancellation regions are properly scoped
4. All multi-instance patterns have bounded instance creation

This theorem provides the foundation for automated verification of composed workflows. By checking local conditions at composition boundaries, we can guarantee global soundness properties without exhaustively exploring the entire state space.

### 6.2.3 Pattern Algebra

We can define an algebraic notation for pattern composition that enables concise specification:

```
A >> B       -- Sequential composition (A then B)
A || B       -- Parallel composition (A and B concurrently)
A + B        -- Exclusive choice (A or B)
A * B        -- Interleaved routing (A then B then A...)
A | B        -- Deferred choice (wait for A or B event)
A % n        -- Multiple instance (n copies of A)
A!           -- Cancel A
[n]A         -- Cancellation region n containing A
```

This algebra enables textual specification of complex workflows:

```
(Order || Payment) >> Shipping >> (Email! + SMS!)
```

Specifies: Execute Order and Payment in parallel, then Shipping, then either Email or SMS (cancelling the unchosen option).

The pattern compiler translates this algebraic notation into Petri net topology, applying the composition rules to ensure soundness.

### 6.2.4 Pattern Implementation as Topology Generators

In the CRE framework, each pattern is implemented as a topology generator function. When called with configuration parameters, the function generates a Petri net structure:

```erlang
-spec parallel_split(pos_integer(), [task_id()]) -> #pnet{}.
parallel_split(BranchCount, TaskIds) ->
    Places = [p_split | [branch_id(I) || I <- lists:seq(1, BranchCount)]],
    Transitions = [t_split],
    InitialMarking = #{p_split => [start]},
    Pre = #{t_split => [p_split]},
    Post = #{t_split => [branch_id(I) || I <- lists:seq(1, BranchCount)]},
    #pnet{
        places = Places,
        transitions = Transitions,
        initial_marking = InitialMarking,
        preset = Pre,
        postset = Post
    }.
```

This generated Petri net can then be composed with other patterns using the composition operations. The result is a complete workflow specification ready for execution by the gen_pnet runtime.

---

## 6.3 Loops, Cancellation, and Explicit Termination

Looping constructs, cancellation mechanisms, and termination semantics represent the most challenging aspects of workflow specification. These patterns introduce cycles, external control flow, and completion conditions that require careful handling to maintain workflow soundness. In this section, we examine these patterns in detail, focusing on their Petri net representations and practical applications.

### 6.3.1 Loop Patterns

Loops in workflows enable repeated execution of activities. Unlike simple sequences, loops introduce cycles into the Petri net topology, which can potentially violate soundness properties if not properly structured.

#### Arbitrary Cycles (WCP-10)

The arbitrary cycle pattern allows any place to transition back to any preceding place, creating general cyclic workflows.

**Petri Net Topology:**
```
p_start -> t_a -> p_a -> t_b -> p_b -> t_c -> p_end
                                                ^
                                                |
                                          t_loop (enabled by condition)
```

**Semantics:** A transition t_loop is enabled when a guard condition evaluates to true. When fired, it removes a token from p_b (or an intermediate place) and deposits a token back at p_a (or any earlier place).

**Soundness Considerations:**
Arbitrary cycles can violate the "option to complete" soundness property if the loop condition never becomes false. To ensure soundness, we must verify:
1. The loop condition is not a tautology (it must be able to become false)
2. Each iteration makes progress toward termination
3. The loop cannot create infinite token accumulation

**Implementation in CRE:**
```erlang
arbitrary_cycle(BodyPattern, LoopCondition) ->
    LoopTransition = make_transition(
        fun(Marking) ->
            case LoopCondition(Marking) of
                true -> {enabled, loop_back};
                false -> disabled
            end
        end
    ),
    % Create cycle from body end to body start
    add_transition(BodyPattern, LoopTransition).
```

**Example:** A document approval workflow where documents can be rejected and returned for revision:

```
Submit -> Review -> {Approved -> Publish | Rejected -> Submit}
```

The Rejected branch creates an arbitrary cycle back to Submit. The loop condition (rejection) can occur multiple times but must eventually lead to approval for the workflow to terminate.

#### Structured Loop (WCP-21)

The structured loop pattern constrains cycles to have well-defined entry and exit points. Unlike arbitrary cycles, structured loops maintain a clear boundary between loop body and surrounding workflow.

**Petri Net Topology:**
```
p_before -> t_entry -> p_loop_start -> t_body -> p_loop_body -> t_exit_check -> p_after
                                                         |              ^
                                                         v              |
                                                    t_loop_back (if condition)
```

**Semantics:**
1. t_entry moves control into the loop
2. t_body executes the loop body
3. t_exit_check tests the continuation condition
   - If true: t_loop_back fires, returning to p_loop_start
   - If false: control proceeds to p_after

**Soundness Advantages:**
Structured loops preserve soundness more easily than arbitrary cycles because:
1. The loop has a single entry point (no hidden entry paths)
2. The loop has a single exit point (no multiple exits)
3. The termination condition is explicit at the exit check

**Implementation in CRE:**
```erlang
structured_loop(BodyPattern, ConditionFun) ->
    Entry = transition(p_before, p_loop_start, fun(_) -> enabled end),
    ExitCheck = transition(
        p_loop_body,
        [p_after, p_loop_start],  % Two possible destinations
        fun(Marking) ->
            case ConditionFun(Marking) of
                continue -> {enabled, p_loop_start};  % Loop back
                done -> {enabled, p_after}           % Exit loop
            end
        end
    ),
    compose(BodyPattern, [Entry, ExitCheck]).
```

**Example:** Processing items in a batch until all are processed:

```
for each item in batch:
    process item
    if more items remain: continue
    else: exit
```

The structured loop ensures that every item is processed exactly once, and the loop terminates after the batch is exhausted.

#### Recursion (WCP-22)

The recursion pattern allows a workflow to invoke itself as a subprocess. This is particularly useful for processing hierarchical data structures or implementing divide-and-conquer algorithms.

**Petri Net Topology:**
```
p_start -> t_check -> {base_case -> p_done | recursive -> p_recurse -> (self) -> t_merge -> p_done}
```

**Semantics:**
1. t_check evaluates whether the base case or recursive case applies
2. Recursive case creates a new invocation of the workflow (nested marking)
3. t_merge combines results from recursive calls

**Soundness Considerations:**
Recursion must be well-founded to guarantee termination:
1. Base case must be reachable from all recursive cases
2. Each recursive call must make progress toward the base case
3. Depth must be bounded (implicitly by data, explicitly by limit)

**Implementation Challenge:**
Standard Petri nets do not support nested markings directly. CRE implements recursion through:
1. **Hierarchical markings**: Each place contains a stack of sub-markings
2. **Process spawning**: Each recursive call spawns a new gen_yawl process
3. **Token recursion**: Special tokens that contain complete sub-workflow specifications

### 6.3.2 Cancellation Patterns

Cancellation provides external control over workflow execution, allowing activities to be terminated before their normal completion. Cancellation is essential for handling exceptional conditions, user interruptions, and timeout scenarios.

#### Cancel Activity (WCP-19)

Cancels a specific activity while allowing the workflow to continue.

**Petri Net Topology:**
```
p_start -> t_start -> p_active -> t_complete -> p_done
                    |            ^
                    v            |
               t_cancel ------------
```

**Semantics:**
1. Normal execution: t_start enables activity, t_complete finishes it
2. Cancellation: t_cancel removes token from p_active, optionally performing cleanup
3. After cancellation, workflow continues from t_cancel's output place

**Implementation in CRE:**
```erlang
cancel_activity(ActivityPattern, CancelCondition) ->
    CancelTransition = transition(
        p_active,
        p_cancelled,
        fun(Marking) ->
            case CancelCondition(Marking) of
                true -> {enabled, cancel};
                false -> disabled
            end
        end,
        fun(ActivityPid) ->
            % Send cancellation signal to activity process
            gen_yawl:cancel(ActivityPid)
        end
    ),
    add_transition(ActivityPattern, CancelTransition).
```

**Safety Properties:**
Cancel activity must ensure:
1. Resources are released (no leaks)
2. Partial work is rolled back or compensated
3. No zombie processes remain after cancellation
4. The workflow can continue from a consistent state

**Example:** A user request that can be cancelled while processing:

```
Submit -> Processing -> {Complete -> Result | Cancel -> Cleanup -> Cancelled}
```

If the user clicks cancel during processing, the cleanup transition runs and the workflow ends in the Cancelled state.

#### Cancel Case (WCP-20)

Cancels the entire workflow instance, terminating all activities regardless of their state.

**Petri Net Topology:**
```
Global cancellation transition t_cancel_case:
  Input: external cancellation signal
  Output: p_terminated
  Effect: removes all tokens from all places
```

**Semantics:**
When t_cancel_case fires:
1. All active tokens are removed from all places
2. A single termination token is placed in p_terminated
3. No further transitions can fire (workflow is dead)

**Implementation in CRE:**
```erlang
cancel_case(WorkflowPid) ->
    gen_yawl:cancel(WorkflowPid),
    receive
        {cancelled, WorkflowPid} -> ok
    after 5000 ->
        {error, timeout}
    end.
```

**Use Cases:**
- User aborts the entire workflow
- System shutdown requires workflow termination
- Critical error makes continuation impossible
- Timeout at workflow level

#### Cancel Region (WCP-25)

Cancels a scoped region of activities while leaving other parts of the workflow unaffected.

**Petri Net Topology:**
```
Region R: {p_r1, p_r2, p_r3}
Outside region: p_o1, p_o2

t_cancel_region:
  Effect: removes tokens only from {p_r1, p_r2, p_r3}
  Preserves: tokens in {p_o1, p_o2}
```

**Semantics:**
1. Region boundaries are explicitly defined
2. Cancellation applies only to places within the region
3. Activities outside the region continue normally
4. Workflow can continue from a region exit point

**Implementation in CRE:**
```erlang
cancel_region(WorkflowPid, RegionId) ->
    gen_yawl:cancel_region(WorkflowPid, RegionId),
    receive
        {region_cancelled, WorkflowPid, RegionId} -> ok
    after 5000 ->
        {error, timeout}
    end.
```

**Example:** Parallel processing with independent cancellation:

```
Region 1: Data fetching (can be cancelled)
Region 2: UI updates (continues regardless)
Region 3: Logging (continues regardless)

If Region 1 is cancelled, Regions 2 and 3 continue.
```

#### Cancellation Composition

Cancellation patterns can be composed hierarchically:
- A workflow can have multiple independent cancellation regions
- Regions can be nested (inner region cancellation does not affect outer)
- Cancel case terminates everything regardless of regions

**Composition Rules:**
1. Regions must be properly scoped (no overlap without containment)
2. Cancellation of outer region cancels inner regions
3. Cancellation of inner region does not cancel outer region
4. Cancel case overrides all regional cancellations

### 6.3.3 Explicit Termination (WCP-43)

Explicit termination provides an alternative to implicit termination, allowing workflows to terminate before all possible transitions are disabled.

**Contrast with Implicit Termination:**
- **Implicit (WCP-11)**: Workflow terminates when no transitions are enabled
- **Explicit (WCP-43)**: Workflow can terminate via dedicated termination transitions even when other transitions remain enabled

**Petri Net Topology:**
```
p_active -> t_normal -> p_next
           |
           v
        t_explicit_terminate
           |
           v
        p_terminated (final state)
```

**Semantics:**
1. t_explicit_terminate is enabled when a termination condition is met
2. When fired, it moves the workflow to p_terminated
3. p_terminated is a final marking: no transitions fire from it
4. Even if other transitions were enabled, they cannot fire after termination

**Implementation in CRE:**
```erlang
explicit_termination(WorkflowPattern, TerminationCondition) ->
    TerminateTransition = transition(
        p_active,
        p_terminated,
        fun(Marking) ->
            case TerminationCondition(Marking) of
                true -> {enabled, terminate};
                false -> disabled
            end
        end,
        fun(WorkflowPid) ->
            % Mark workflow as terminated
            gen_yawl:terminate(WorkflowPid),
            % Prevent further transitions
            gen_yawl:disable_all(WorkflowPid)
        end
    ),
    add_transition(WorkflowPattern, TerminateTransition).
```

**Use Cases:**
1. **Early completion**: Sufficient results obtained, no need to continue
2. **Error recovery**: Workflow terminates gracefully after unrecoverable error
3. **Resource constraints**: External resource limits require termination
4. **User decision**: User chooses to stop workflow early

**Example:** A data processing workflow that can terminate early:

```
Process data -> check results -> {enough -> terminate | insufficient -> continue}
```

If the processed data provides sufficient confidence, the workflow terminates explicitly even though more data could be processed.

### 6.3.4 Combining Loops, Cancellation, and Termination

Real-world workflows often combine these patterns:

**Example: Search with Timeout and Cancellation**

```
start -> parallel_split ->
  Branch 1: Search loop (structured loop)
  Branch 2: Timeout monitor (deferred choice)

synchronization:
  - If loop finds result: explicit terminate (cancels timeout monitor)
  - If timeout expires: cancel region (cancels search loop)
  - If user clicks cancel: cancel case (terminates everything)
```

This combined pattern demonstrates:
1. **Loop**: The search repeats until result found or timeout
2. **Cancellation**: Timeout cancels the search region; user cancel terminates everything
3. **Explicit termination**: Finding result terminates workflow before timeout

**Implementation in CRE:**
```erlang
search_workflow(SearchFun, Timeout, ResultCondition) ->
    Workflow = parallel_split([
        % Search loop branch
        structured_loop(
            search_step(SearchFun),
            fun(Marking) -> not ResultCondition(Marking) end
        ),
        % Timeout monitor branch
        deferred_choice(
            [
                {timeout, fun() -> timer:sleep(Timeout) end},
                {result, fun() -> wait_for_result() end}
            ]
        )
    ]),
    add_cancellation_handlers(Workflow, #{
        region_cancel => fun cancel_search/1,
        case_cancel => fun cancel_all/1,
        explicit_terminate => fun terminate_on_result/1
    }).
```

### 6.3.5 Soundness Verification

Workflows with loops, cancellation, and explicit termination require comprehensive soundness verification:

**Verification Properties:**

1. **Option to Complete**: From any reachable marking, can the workflow reach a final marking?
   - Loops: Must have exit conditions that are eventually satisfiable
   - Cancellation: Must not prevent all completion paths
   - Termination: Explicit termination must be reachable when needed

2. **Proper Completion**: Does the workflow terminate only in appropriate final states?
   - Loops: Must not terminate in the middle of iteration
   - Cancellation: Must leave consistent state after cancellation
   - Termination: Must not leave zombie processes or leaked resources

3. **No Dead Transitions**: Are there transitions that become permanently enabled but never fire?
   - Loops: Loop transitions must not starve other transitions
   - Cancellation: Cancellation transitions must not cause permanent enabling
   - Termination: Terminated workflows must have no enabled transitions

4. **Liveness**: Does the workflow make progress toward completion?
   - Loops: Each iteration must make measurable progress
   - Cancellation: Cancellation must not cause livelock
   - Termination: Termination conditions must be detectable

**Verification in CRE:**
```erlang
verify_workflow_soundness(WorkflowPattern) ->
    % State space exploration
    ReachableMarkings = explore_reachable_markings(WorkflowPattern),
    % Deadlock detection
    Deadlocks = find_deadlocks(WorkflowPattern, ReachableMarkings),
    % Soundness checks
    OptionToComplete = check_option_to_complete(ReachableMarkings),
    ProperCompletion = check_proper_completion(WorkflowPattern),
    NoDeadTransitions = check_no_dead_transitions(WorkflowPattern),
    Liveness = check_liveness(WorkflowPattern),

    case {OptionToComplete, ProperCompletion, NoDeadTransitions, Liveness} of
        {true, true, true, true} -> {ok, sound};
        _ -> {error, {unsound, [
            {option_to_complete, OptionToComplete},
            {proper_completion, ProperCompletion},
            {no_dead_transitions, NoDeadTransitions},
            {liveness, Liveness}
        ]}}
    end.
```

This verification ensures that workflows combining loops, cancellation, and termination maintain the essential properties of sound Petri nets, providing reliable execution in production environments.
