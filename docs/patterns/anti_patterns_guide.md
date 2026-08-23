# Anti-Patterns Guide for Generative Analysis

**Common Mistakes and How to Avoid Them in Workflow System Development**

---

## Table of Contents

1. [Documentation Anti-Patterns](#1-documentation-anti-patterns)
2. [Specification Anti-Patterns](#2-specification-anti-patterns)
3. [Pattern Anti-Patterns](#3-pattern-anti-patterns)
4. [Verification Anti-Patterns](#4-verification-anti-patterns)
5. [Swarm Anti-Patterns](#5-swarm-anti-patterns)
6. [Tool Anti-Patterns](#6-tool-anti-patterns)

---

## 1. Documentation Anti-Patterns

### 1.1 Vague Behavior Contracts (BCDs)

**Anti-Pattern**: Writing behavior contracts that lack precision about preconditions, postconditions, or invariants.

**Why It's Problematic**:
- Implementation details become ambiguous
- Different developers interpret requirements differently
- Testing cannot verify unspecified behavior
- Verification gaps emerge in production

**Example - Vague BCD**:
```erlang
%% @doc Executes a workflow step.
%% @end
-spec execute_step(pid()) -> ok | error.

execute_step(Pid) ->
    %% What does this do? What are the preconditions?
    %% What does 'ok' vs 'error' mean specifically?
    gen_server:call(Pid, execute_step).
```

**Example - Precise BCD**:
```erlang
%% @doc Executes a single workflow transition.
%%
%% == Preconditions ==
%% - The workflow process must be in 'running' state
%% - At least one transition must be enabled
%% - The process must not be shutting down
%%
%% == Postconditions ==
%% - On success: Returns {ok, Receipt} where Receipt contains the
%%   transition that fired and tokens produced
%% - On abort: Returns 'abort' if no transitions are enabled
%% - On error: Returns {error, Reason} if process is unreachable
%%
%% == Invariants ==
%% - Marking consistency is maintained atomically
%% - All preset tokens are consumed before postset produced
%%
%% @end
-spec execute_step(pid()) -> {ok, receipt()} | abort | {error, term()}.

execute_step(Pid) when is_pid(Pid) ->
    case gen_server:call(Pid, step, 5000) of
        abort -> abort;
        {ok, Receipt} -> {ok, normalize_receipt(Receipt)};
        {error, _} = Error -> Error
    end.
```

**Prevention Strategies**:
1. Use structured documentation headers with Preconditions/Postconditions/Invariants
2. Include concrete examples for each error case
3. Specify all possible return values with their meanings
4. Document atomicity guarantees
5. Include edge case behavior explicitly

---

### 1.2 Orphaned Diagrams

**Anti-Pattern**: Diagrams that exist without connection to code, or code without corresponding diagrams.

**Why It's Problematic**:
- Diagrams drift from actual implementation
- Developers cannot trust documentation
- Onboarding becomes confusing
- Architecture reviews cannot verify correctness

**Example - Orphaned Diagram**:
```mermaid
%% diagram: workflow-arch.md
sequenceDiagram:
    Client->>Executor: execute_workflow()
    Executor->>Compiler: compile()
    %% This diagram exists but the actual code uses
    %% a different flow with a checkpoint step
```

**Example - Connected Documentation**:
```erlang
%% @doc Workflow execution following the documented architecture:
%%
%% See: docs/diagrams/workflow-execution-sequence.md
%%
%% The execution follows this exact sequence:
%% 1. Load specification (wf_spec:from_xml/1)
%% 2. Compile to gen_pnet module (yawl_compile:compile/2)
%% 3. Initialize with data (gen_yawl:start_link/3)
%% 4. Execute to completion (gen_yawl:drain/2)
%% 5. Persist checkpoints (yawl_recovery:checkpoint/4)
%%
%% Each step corresponds to a verified function with BCDs.
%% @end
```

**Prevention Strategies**:
1. Include diagram references in module headers (`@see`)
2. Use doctests that demonstrate the exact sequence
3. Generate diagrams from code (e.g., using `:moduledoc` with mermaid)
4. Add CI checks that validate diagram-to-code correspondence
5. Use documentation linters to find orphan diagrams

---

### 1.3 Missing Type Specifications

**Anti-Pattern**: Functions without `-spec` attributes or with incomplete type specifications.

**Why It's Problematic**:
- Dialyzer cannot catch type errors
- Documentation is incomplete
- Refactoring becomes dangerous
- Generative AI tools cannot understand constraints

**Example - Missing Types**:
```erlang
%% No spec - what does this accept?
execute_workflow(Spec, Data, Options) ->
    case compile(Spec) of
        {ok, Compiled} -> run(Compiled, Data, Options);
        Error -> Error
    end.
```

**Example - Complete Types**:
```erlang
-spec execute_workflow(Spec :: spec() | file:filename_all(),
                      Data :: map(),
                      Options :: exec_options()) ->
          {ok, exec_result()} | {error, compile_error() | execution_error()}.
```

**Prevention Strategies**:
1. Enable `-Wspec` and `-Wunderspecs` compiler warnings
2. Require `-spec` for all exported functions
3. Use `-dialyzer({[spec_diffs}, error])` in modules
4. Include type definitions in separate `.hrl` files
5. Run Dialyzer in CI with zero tolerance for warnings

---

## 2. Specification Anti-Patterns

### 2.1 Ambiguous Requirements

**Anti-Pattern**: Specifications that allow multiple interpretations, leading to divergent implementations.

**Why It's Problematic**:
- Different implementations behave differently
- Integration points fail unexpectedly
- Tests cannot verify correctness unambiguously
- Generative analysis cannot validate consistency

**Example - Ambiguous Spec**:
```yaml
# workflow.yaml
tasks:
  - name: approve
    description: "Approve or reject the request"
    # What happens if approval times out?
    # What are the valid transition conditions?
```

**Example - Precise Spec**:
```yaml
# workflow.yaml
tasks:
  - name: approve
    description: "Approve or reject the request"
    preconditions:
      - request.status == "pending"
    postconditions:
      - request.status in ["approved", "rejected"]
    timeout:
      duration: 86400  # 24 hours
      on_timeout: "escalate"
    transitions:
      - condition: request.approval == true
        to: "approved"
      - condition: request.approval == false
        to: "rejected"
      - condition: timeout()
        to: "escalate"
```

**Prevention Strategies**:
1. Use formal specification languages (TLA+, Petri nets)
2. Specify all edge cases explicitly
3. Include state transition tables
4. Use constraint-based validation
5. Generate test cases from specifications

---

### 2.2 Implicit Assumptions

**Anti-Pattern**: Relying on unstated assumptions about system behavior, data formats, or timing.

**Why It's Problematic**:
- Failures occur when assumptions don't hold
- Debugging becomes extremely difficult
- Portability suffers across environments
- New contributors make incorrect assumptions

**Example - Implicit Assumptions**:
```erlang
%% Assumes: Mnesia is already started
%% Assumes: Table exists with correct schema
%% Assumes: No concurrent writes
save_checkpoint(CaseId, State) ->
    mnesia:write(#checkpoint{case_id = CaseId, state = State}),
    ok.
```

**Example - Explicit Assumptions**:
```erlang
%% @doc Saves a checkpoint for a workflow case.
%%
%% == Requirements ==
%% - Mnesia must be started (call mnesia:start/0 first)
%% - Table 'checkpoint' must exist (use yawl_recovery:init_schema/0)
%% - Caller must handle concurrent write conflicts
%%
%% == Guarantees ==
%% - Returns ok only if write is durable
%% - Throws badarg if CaseId is not binary()
%% - May abort with transaction conflict if concurrent write
%%
%% @end
-spec save_checkpoint(case_id(), term()) -> ok | no_return().

save_checkpoint(CaseId, State) when is_binary(CaseId) ->
    Transaction = fun() ->
        %% Check assumption: table exists
        case mnesia:table_info(checkpoint, where_to_write) of
            aborted -> mnesia:abort(table_not_exists);
            _ -> ok
        end,
        mnesia:write(#checkpoint{case_id = CaseId, state = State}),
        {atomic, ok}
    end,
    case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> error({checkpoint_failed, Reason})
    end.
```

**Prevention Strategies**:
1. Explicitly state all preconditions in documentation
2. Use runtime assertions to validate assumptions
3. Write tests that violate assumptions (negative tests)
4. Use types to encode constraints at compile time
5. Log assumption violations for debugging

---

### 2.3 Incomplete Error Scenarios

**Anti-Pattern**: Specifications that only describe happy paths, ignoring failure modes.

**Why It's Problematic**:
- Production failures are unhandled
- Error recovery is undefined
- System behavior in degradation is unpredictable
- Generative analysis cannot verify fault tolerance

**Example - Incomplete Error Spec**:
```erlang
%% Only documents success case
-spec compile_workflow(Spec) -> {ok, compiled()}.
compile_workflow(Spec) ->
    %% What if spec is invalid?
    %% What if compilation fails?
    %% What if dependencies missing?
    do_compile(Spec).
```

**Example - Complete Error Spec**:
```erlang
%% @doc Compiles a YAWL specification to executable form.
%%
%% == Return Values ==
%% - {ok, Compiled}: Specification compiled successfully
%% - {error, invalid_spec}: Specification XML is malformed
%% - {error, validation_error, [Error]}: Semantic validation failed
%% - {error, compile_error, Reason}: Code generation failed
%% - {error, dependency_error, Module}: Required dependency unavailable
%%
%% @end
-spec compile_workflow(Spec :: term()) ->
          {ok, compiled()} |
          {error, invalid_spec} |
          {error, {validation_error, [term()]}} |
          {error, {compile_error, term()}} |
          {error, {dependency_error, module()}}.

compile_workflow(Spec) ->
    try
        case validate_spec(Spec) of
            {ok, Validated} ->
                case do_compile(Validated) of
                    {ok, Compiled} -> {ok, Compiled};
                    {error, Reason} -> {error, {compile_error, Reason}}
                end;
            {error, Errors} ->
                {error, {validation_error, Errors}}
        end
    catch
        throw:{invalid_xml, _} -> {error, invalid_spec};
        error:{undef, [{Module, _, _} | _]} -> {error, {dependency_error, Module}}
    end.
```

**Prevention Strategies**:
1. Document all error return values explicitly
2. Use tagged tuples for all return values
3. Write tests for each error path
4. Use fault injection testing
5. Generate error documentation from specs

---

## 3. Pattern Anti-Patterns

### 3.1 Wrong Pattern Choices

**Anti-Pattern**: Using workflow control patterns that don't match the problem domain.

**Why It's Problematic**:
- Workflow becomes unnecessarily complex
- Correctness properties are hard to verify
- Performance suffers from unnecessary overhead
- Soundness properties may be violated

**Example - Wrong Pattern**:
```erlang
%% Using discriminator for what should be synchronization
%% This causes race conditions and lost tokens
%% BAD: Discriminator fires on first completion, ignores others
race_condition_workflow() ->
    Pattern = discriminator,  %% Wrong! Should be synchronization
    execute_parallel_tasks([critical1, critical2, critical3]).
```

**Example - Correct Pattern**:
```erlang
%% Using synchronization to wait for ALL branches
%% Ensures all critical tasks complete before continuation
correct_workflow() ->
    Pattern = synchronization,  %% Correct! Waits for all branches
    execute_parallel_tasks([critical1, critical2, critical3]).
```

**Pattern Selection Guide**:
| Requirement | Use Pattern |
|-------------|-------------|
| Must wait for all branches | Synchronization (WCP-03) |
| First completion wins | Discriminator (WCP-09) |
| Multiple branches, one selected | Exclusive Choice (WCP-04) |
| All branches in any order | Interleaved Routing (WCP-17) |
| N parallel instances | Multi-Instance (WCP-13/15) |

**Prevention Strategies**:
1. Use pattern decision trees
2. Document pattern choice rationale
3. Verify soundness properties match requirements
4. Use formal verification for critical workflows
5. Maintain pattern catalog with use cases

---

### 3.2 Over-Composition

**Anti-Pattern**: Combining too many patterns in a single workflow, creating unmanageable complexity.

**Why It's Problematic**:
- State space explodes exponentially
- Verification becomes intractable
- Debugging is nearly impossible
- Adding features breaks existing behavior

**Example - Over-Composed**:
```erlang
%% TOO MANY PATTERNS - impossible to verify
chaos_workflow() ->
    compose([
        sequence,
        parallel_split,
        discrimination,
        synchronization,
        milestone,
        cancel_region,
        critical_section,
        interleaved_routing,
        deferred_choice
    ]).
```

**Example - Well-Structured**:
```erlang
%% Decomposed into verifiable sub-workflows
structured_workflow() ->
    {ok, Order} = verify(sequence_workflow([validate, process])),
    {ok, Payment} = verify(parallel_workflow([charge_card, update_inventory])),
    {ok, Shipping} = verify(synchronization_workflow([pack, ship])),
    compose_sequence([Order, Payment, Shipping]).
```

**Prevention Strategies**:
1. Limit nesting depth (recommend max 3-4 levels)
2. Use workflow decomposition/subnets
3. Each subnet should have single responsibility
4. Verify subnets independently
5. Use complexity metrics (cyclomatic complexity for workflows)

---

### 3.3 Pattern Misconfiguration

**Anti-Pattern**: Using correct patterns with incorrect configuration parameters.

**Why It's Problematic**:
- Patterns don't behave as expected
- Deadlocks occur unexpectedly
- Liveness properties fail
- Hard to diagnose (pattern is "correct" but configured wrong)

**Example - Misconfigured**:
```erlang
%% Parallel split with wrong branch count
%% Spec says 3 branches but config has 2
parallel_misconfig() ->
    Tasks = [task_a, task_b, task_c],  %% 3 tasks
    parallel_split(2, Tasks).             %% WRONG: count=2
    %% Result: task_c never executes!
```

**Example - Correctly Configured**:
```erlang
%% Validate branch count matches tasks
parallel_correct() ->
    Tasks = [task_a, task_b, task_c],
    BranchCount = length(Tasks),
    case BranchCount >= 2 of
        true -> parallel_split(BranchCount, Tasks);
        false -> error({invalid_branch_count, BranchCount})
    end.
```

**Prevention Strategies**:
1. Use configuration validation at compile time
2. Use type-safe configuration builders
3. Add runtime assertions for critical parameters
4. Document all parameters with constraints
5. Generate tests from configuration schemas

---

## 4. Verification Anti-Patterns

### 4.1 Testing Instead of Proving

**Anti-Pattern**: Relying solely on example-based tests instead of formal verification.

**Why It's Problematic**:
- Tests only cover specific scenarios
- Edge cases remain undiscovered
- Concurrency bugs are missed
- Properties cannot be generalized

**Example - Testing Only**:
```erlang
%% Only tests specific cases - misses general properties
checkpoint_test() ->
    {ok, Cpid} = checkpoint(spec1, case1, marking1, data1),
    {ok, {M1, D1}} = resume(spec1, case1, Cpid),
    ?assertEqual(marking1, M1).
    %% What about:
    %% - Concurrent checkpoints?
    %% - Large data sizes?
    %% - Recovery after crash?
```

**Example - Property-Based**:
```erlang
%% Properties that must hold for ALL inputs
prop_checkpoint_resume_roundtrip() ->
    ?FORALL({Spec, Case, Marking, Data},
            {valid_spec(), valid_case_id(), marking(), data()},
        begin
            {ok, Cpid} = checkpoint(Spec, Case, Marking, Data),
            {ok, {RestoredMarking, RestoredData}} = resume(Spec, Case, Cpid),
            %% Property: Resume returns exact saved state
            (Marking =:= RestoredMarking) andalso (Data =:= RestoredData)
        end).
```

**Prevention Strategies**:
1. Write property-based tests (PropEr, QuickCheck)
2. Specify invariants that must always hold
3. Use model checking for concurrent systems
4. Complement tests with formal proofs
5. Cover orthogonal properties (not overlapping cases)

---

### 4.2 Verification Gaps

**Anti-Pattern**: Verifying some components but not their integration, leaving gaps in correctness guarantees.

**Why It's Problematic**:
- Integration bugs surface in production
- Components work individually but fail together
- Compositional properties are violated
- Soundness doesn't hold at system level

**Example - Gapped Verification**:
```erlang
%% Each component verified separately
-test("compile: valid spec compiles").  %% ✓ Passes
-test("execute: compiled workflow runs").  %% ✓ Passes
%% BUT: Gap - what if compiled module can't be loaded?
%% What if runtime environment different from compile?
```

**Example - Complete Verification**:
```erlang
%% Verify entire pipeline
-test("compile-execute: valid spec compiles, loads, and runs").
compile_execute_roundtrip() ->
    Spec = valid_spec(),
    {ok, Compiled} = compile_workflow(Spec),
    {ok, Module} = load_compiled(Compiled),  %% Load into VM
    {ok, Pid} = start_workflow(Module, #{}),
    {ok, Receipts} = execute_to_completion(Pid),
    ?assert(length(Receipts) > 0).
```

**Prevention Strategies**:
1. Test at multiple levels (unit, integration, system)
2. Use property-based testing across boundaries
3. Verify compositional properties
4. Include end-to-end verification
5. Use TLA+ or similar for system-level verification

---

### 4.3 Shallow Verification

**Anti-Pattern**: Verifying only surface-level properties without checking deep correctness.

**Why It's Problematic**:
- Subtle bugs remain hidden
- Liveness properties may be violated
- Soundness is not genuinely proven
- Race conditions emerge under load

**Example - Shallow**:
```erlang
%% Only checks return value type
shallow_verify() ->
    Result = execute_workflow(wf),
    ?assertMatch({ok, _}, Result).
    %% Doesn't verify: was workflow CORRECTLY executed?
```

**Example - Deep Verification**:
```erlang
%% Checks actual execution properties
deep_verify() ->
    {ok, Result} = execute_workflow(wf),
    #{receipts := Receipts, final_state := FinalState} = Result,

    %% Property 1: All tasks in reachable markings executed
    AllTasks = lists:usort(get_all_tasks(wf)),
    ExecutedTasks = lists:usort([get_task(R) || R <- Receipts]),
    ?assertEqual(AllTasks, ExecutedTasks),

    %% Property 2: Final marking has completion token
    ?assert(maps:is_key(completion, FinalState)),

    %% Property 3: No tokens left in intermediate places
    IntermediatePlaces = get_intermediate_places(wf),
    Empty = [maps:get(P, FinalState, []) =:= [] || P <- IntermediatePlaces],
    ?assert(lists:all(fun(ID) -> ID end, Empty)).
```

**Prevention Strategies**:
1. Verify all stated properties, not just obvious ones
2. Check deep invariants (marking consistency, state validity)
3. Use reachability analysis
4. Verify liveness (no deadlock in reachable states)
5. Use state space exploration for small workflows

---

## 5. Swarm Anti-Patterns

### 5.1 Unconstrained Agents

**Anti-Pattern**: Allowing agents to operate without clear boundaries or constraints.

**Why It's Problematic**:
- Agents modify files outside their scope
- Conflicts arise between parallel agents
- System state becomes unpredictable
- Rollback is impossible

**Example - Unconstrained**:
```erlang
%% Agent can modify anything anywhere
agent_task(AgentId, Task) ->
    spawn(fun() ->
        %% No constraints on what this agent can modify
        Result = execute_anything(Task),
        modify_any_file(Result)
    end).
```

**Example - Constrained**:
```erlang
%% Agent with explicit scope and sandboxing
agent_task(AgentId, Task, Scope) ->
    spawn(fun() ->
        %% Validate scope before execution
        case validate_scope(Task, Scope) of
            {ok, ValidatedTask} ->
                Result = execute_in_sandbox(ValidatedTask, Scope),
                %% Only write within scope
                case within_scope(Result, Scope) of
                    true -> apply_changes(Result);
                    false -> error({scope_violation, Result})
                end;
            {error, Reason} ->
                error({invalid_scope, Reason})
        end
    end).
```

**Prevention Strategies**:
1. Define explicit scopes for each agent
2. Use sandboxing for file operations
3. Implement permission systems
4. Log all agent actions for audit
5. Use capability-based security

---

### 5.2 Bypassing Workflows

**Anti-Pattern**: Direct execution that skips defined workflow procedures.

**Why It's Problematic**:
- Verification is circumvented
- Error handling is skipped
- Audit trails are incomplete
- Recovery procedures fail

**Example - Bypassing**:
```erlang
%% Direct call bypasses workflow
quick_compile(Spec) ->
    %% Skips validation, skips logging, skips state checks
    yawl_compile:compile(Spec, #{}).
```

**Example - Through Workflow**:
```erlang
%% Follow defined workflow
proper_compile(Spec) ->
    %% Step 1: Validate
    ok = yawl_validate:validate(Spec),
    %% Step 2: Log start
    yawl_log:log(compile_start, Spec),
    %% Step 3: Compile
    {ok, Result} = yawl_compile:compile(Spec, #{}),
    %% Step 4: Verify result
    ok = verify_compiled(Result),
    %% Step 5: Log complete
    yawl_log:log(compile_complete, Result),
    {ok, Result}.
```

**Prevention Strategies**:
1. Make direct calls private (prefix with `_`)
2. Expose only workflow APIs
3. Add audit logging to all entry points
4. Use wrapper functions that enforce procedures
5. Code review to detect bypass attempts

---

### 5.3 Missing Coordination

**Anti-Pattern**: Parallel agents operating without coordination mechanisms.

**Why It's Problematic**:
- Race conditions on shared resources
 Duplicate work occurs
- Agents wait unnecessarily
- Deadlocks possible

**Example - Uncoordinated**:
```erlang
%% Two agents modifying same file independently
agent1() -> write_file("config.yaml", NewConfig1).
agent2() -> write_file("config.yaml", NewConfig2).
%% Result: Last writer wins, changes lost
```

**Example - Coordinated**:
```erlang
%% Use coordination primitives
coordinated_write(File, Content) ->
    case global:set_lock({file, File}, [node()], 1) of
        true ->
            try
                write_file(File, Content),
                ok
            after
                global:del_lock({file, File}, [node()])
            end;
        false ->
            {error, locked}
    end.
```

**Prevention Strategies**:
1. Use distributed locks for shared resources
2. Implement transactional operations
3. Use message passing for coordination
4. Design for idempotency
5. Implement conflict resolution

---

## 6. Tool Anti-Patterns

### 6.1 Wrong Tools for the Job

**Anti-Pattern**: Using inappropriate tools for specific tasks.

**Why It's Problematic**:
- Poor performance
- Incorrect results
- Unnecessary complexity
- Maintenance burden

**Tool Selection Guide**:

| Task | Appropriate Tools | Inappropriate Tools |
|------|------------------|---------------------|
| Workflow Specification | YAWL XML/YAML, Petri nets | Ad-hoc code, raw SQL |
| State Management | gen_pnet, gen_server | ETS (without abstraction), Mnesia (for transient) |
| Verification | Concuerror, PropEr, Dialyzer | Manual testing only |
| Documentation | edoc, ex_doc with types | Comments only, external wikis |
| Logging | logger, OTEL | io:format, print statements |
| Build | rebar3 | make (without integration), manual compilation |

**Example - Wrong Tool**:
```erlang
%% Using ETS for workflow state (no verification possible)
store_workflow_state(WorkflowId, State) ->
    ets:insert(workflow_states, {WorkflowId, State}).
```

**Example - Right Tool**:
```erlang
%% Using gen_pnet with verifiable state
start_workflow(Spec) ->
    gen_pnet:start_link(Spec, InitialState).
%% Can now verify: soundness, liveness, reachability
```

**Prevention Strategies**:
1. Create tool selection guidelines
2. Document why each tool was chosen
3. Review tool choices in design phase
4. Benchmark alternative tools
5. Stay updated on ecosystem improvements

---

### 6.2 Misconfiguration

**Anti-Pattern**: Tools configured incorrectly for their intended use.

**Why It's Problematic**:
- Silent failures
- Incorrect behavior
- Performance degradation
- Hard-to-diagnose issues

**Example - Misconfigured Compiler**:
```erlang
%% Missing warnings, wrong options
{erl_opts, [
    %% Missing: debug_info (needed for tools)
    %% Missing: warnings_as_errors
    %% Missing: warn_export_all
    {i, "include"}
]}.
```

**Example - Properly Configured**:
```erlang
%% Comprehensive configuration
{erl_opts, [
    debug_info,                 %% Required for dialyzer, cover, tracing
    warn_export_all,            %% Catch unused exports
    warn_missing_spec,          %% Require specs for all exports
    warn_untyped_record,        %% Require record field types
    warnings_as_errors,         %% Treat warnings as errors
    {d, "deps"},                 %% Include dependencies
    {i, "include"}               %% Include headers
]}.

{dialyzer, [
    {warnings, [
        error_handling,
        race_conditions,
        unmatched_returns,
        underspecs
    ]}
]}.
```

**Prevention Strategies**:
1. Use configuration templates
2. Document configuration options
3. Validate configuration in CI
4. Use configuration linters
5. Share best practices across projects

---

## Quick Reference Summary

### Anti-Pattern Detection Checklist

Before committing code, verify:

- [ ] All public functions have complete `-spec` attributes
- [ ] All error cases are documented and tested
- [ ] Patterns selected match problem domain
- [ ] Workflow depth is reasonable (< 4 levels)
- [ ] Properties tested, not just examples
- [ ] Integration gaps covered by tests
- [ ] Deep invariants verified
- [ ] Agents have explicit scopes
- [ ] Workflow procedures followed
- [ ] Coordination for shared resources
- [ ] Tools appropriate for tasks
- [ ] Tools properly configured

### Related Documentation

- `docs/pnet_marking_api_reference.md` - Complete API documentation example
- `docs/YAWL_PATTERN_REFERENCE.md` - Pattern catalog with soundness properties
- `docs/VERIFICATION_REPORT.md` - Verification methodology
- `src/yawl_recovery.erl` - Example of well-documented BCDs
- `test/yawl_recovery_test.erl` - Example of comprehensive testing

---

**Version**: 1.0.0
**Last Updated**: 2025-02-07
**Maintained By**: CRE Team
