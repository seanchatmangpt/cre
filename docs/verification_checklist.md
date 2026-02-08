# Generative Analysis Verification Checklist

This document provides a comprehensive multi-stage checklist for verifying Generative Analysis artifacts before deployment. Each stage includes both manual and automated verification steps.

---

## Stage 1: BCD Validation

### Purpose
Verify that the Business Context Document (BCD) is complete and properly structured before generating any workflow specification.

### What It Verifies
The BCD defines the business requirements, actors, data flows, and success criteria for the workflow. An incomplete BCD leads to incorrect specifications.

### Checklist

#### 1.1 Document Structure
- [ ] **BCD exists and is accessible**
  - Manual: Verify file exists at expected path
  - Automated: `file:read_file_info("bcd.md")` returns `{ok, _}`

- [ ] **Contains all required sections**
  - Manual: Review document has Introduction, Scope, Stakeholders, Requirements, Data Dictionary
  - Automated: Parse document and validate section headers present

- [ ] **Version information present**
  - Manual: Check for version number and last updated date
  - Automated: Extract version regex `^version:\s+(\d+\.\d+)`

#### 1.2 Business Requirements
- [ ] **Goals clearly defined**
  - Manual: Each goal has a measurable outcome
  - Automated: Scan for goal keywords and verify bullet points follow

- [ ] **Stakeholders identified**
  - Manual: List of actors/roles with responsibilities
  - Automated: Extract stakeholders section, verify non-empty list

- [ ] **Success criteria specified**
  - Manual: Each requirement has acceptance criteria
  - Automated: Parse criteria from requirements section

#### 1.3 Data Dictionary
- [ ] **All data types defined**
  - Manual: Each data field has type, constraints, and description
  - Automated: Parse data dictionary, validate type entries

- [ ] **Variable naming consistent**
  - Manual: Variables follow naming convention (snake_case or camelCase)
  - Automated: Regex check on all variable names

- [ ] **Initial values specified**
  - Manual: Each variable has documented initial value or generation rule
  - Automated: Validate `initial:` field present for each variable

#### 1.4 Workflow Scope
- [ ] **Entry conditions documented**
  - Manual: Clear description of when workflow starts
  - Automated: Extract entry condition section, validate non-empty

- [ ] **Exit conditions documented**
  - Manual: Clear description of completion criteria
  - Automated: Extract exit condition section, validate non-empty

- [ ] **Boundaries defined**
  - Manual: What is in scope vs out of scope clearly stated
  - Automated: Check for scope/boundaries section

---

## Stage 2: Specification Validation

### Purpose
Verify that the YAML specification is well-formed and conforms to the YAWL YAML 0.2 schema.

### What It Verifies
The YAML specification must be syntactically correct and contain all required fields for compilation.

### Checklist

#### 2.1 YAML Syntax
- [ ] **Valid YAML format**
  - Manual: Visual inspection of YAML structure
  - Automated: `wf_yaml_spec:from_yaml_file("workflow.yaml")` returns `{ok, _}`

- [ ] **YAML version specified**
  - Manual: Check `yawl_yaml_version: "0.2"` at top of file
  - Automated: Parse and validate version field

- [ ] **No syntax errors**
  - Manual: No malformed indentation or special characters
  - Automated: Yamerl parser returns without errors

#### 2.2 Required Top-Level Fields
- [ ] **specificationSet present**
  - Manual: Verify top-level `specificationSet:` key exists
  - Automated: Parse check for key presence

- [ ] **uri specified (workflow ID)**
  - Manual: Check non-empty URI string
  - Automated: `wf_yaml_spec:id(Spec) =/= <<>>`

- [ ] **metaData exists**
  - Manual: Title and version present in metadata
  - Automated: Validate metadata map has required keys

- [ ] **rootNet specified**
  - Manual: Root net identifier matches a defined net
  - Automated: `wf_yaml_spec:validate_root_net(Spec)` returns ok

#### 2.3 Net Definitions
- [ ] **At least one net defined**
  - Manual: Verify `nets:` list has entries
  - Automated: `wf_yaml_spec:nets(Spec) =/= []`

- [ ] **Root net exists in nets list**
  - Manual: Cross-reference `rootNet:` value with `nets:` ids
  - Automated: Validate root net is subset of defined nets

- [ ] **Each net has unique ID**
  - Manual: No duplicate net IDs
  - Automated: Check net ID uniqueness

#### 2.4 Node Definitions
- [ ] **All nodes have valid IDs**
  - Manual: IDs are non-empty strings
  - Automated: Validate each node has id field

- [ ] **Node kinds are valid**
  - Manual: Only use: task, condition, inputCondition, outputCondition
  - Automated: Validate against allowed kinds list

- [ ] **Task types are valid**
  - Manual: Tasks have type: human, automated, or service
  - Automated: Validate task types against enum

#### 2.5 Flow Definitions
- [ ] **Flows reference valid nodes**
  - Manual: Each `from:` and `to:` references existing node IDs
  - Automated: `yawl_validate:check_flows(Spec)` returns no reference errors

- [ ] **No self-loops**
  - Manual: No flow has same source and target
  - Automated: Check `from =/= to` for all flows

- [ ] **Predicates well-formed**
  - Manual: Predicate expressions have balanced parentheses
  - Automated: `yawl_validate:is_valid_predicate(Pred)` returns true

---

## Stage 3: Pattern Coverage

### Purpose
Verify that all required YAWL workflow patterns are present and properly configured.

### What It Verifies
The 43 YAWL workflow control patterns must be available for use. Missing patterns prevent certain workflow structures from executing.

### Checklist

#### 3.1 Pattern Registry
- [ ] **All 43 patterns registered**
  - Manual: Review pattern registry for complete list
  - Automated: `length(yawl_pattern_registry:all_patterns()) =:= 43`

- [ ] **Pattern modules exist**
  - Manual: Each pattern has corresponding .erl module
  - Automated: Check module file exists for each pattern

- [ ] **Pattern macros defined**
  - Manual: Each pattern has valid macro reference
  - Automated: `yawl_pattern_registry:pattern_macro(Pid) =/= undefined`

#### 3.2 Pattern Instances
- [ ] **Declared instances valid**
  - Manual: Each pattern instance references registered pattern
  - Automated: Validate all pattern IDs against registry

- [ ] **Required parameters present**
  - Manual: Each pattern instance has its required parameters
  - Automated: Check pattern-specific parameter requirements

- [ ] **Net references valid**
  - Manual: Pattern instance `net:` matches defined net
  - Automated: Validate net references in pattern_instances

#### 3.3 Pattern Usage Index
- [ ] **Index covers all patterns**
  - Manual: `pattern_usage_index` has entry for each used pattern
  - Automated: Compare index keys against pattern_instances

- [ ] **No orphaned patterns**
  - Manual: All referenced patterns have definitions
  - Automated: Validate pattern_registry completeness

#### 3.4 Critical Patterns
- [ ] **P1_Sequence available**
  - Manual: Basic sequential execution
  - Automated: `yawl_pattern_registry:validate_pattern(<<"P1_Sequence">>)`

- [ ] **P2_ParallelSplit available**
  - Manual: AND split for concurrent execution
  - Automated: Pattern validation returns true

- [ ] **P4_ExclusiveChoice available**
  - Manual: XOR split for branching
  - Automated: Pattern validation returns true

- [ ] **P3_Synchronization available**
  - Manual: AND join for convergence
  - Automated: Pattern validation returns true

---

## Stage 4: Type Discipline

### Purpose
Verify that all types are correctly specified and consistent across the specification.

### What It Verifies
Type safety ensures runtime correctness and prevents data flow errors during execution.

### Checklist

#### 4.1 Variable Types
- [ ] **All typed variables declared**
  - Manual: Check `variables:` section for type declarations
  - Automated: Parse net variables, validate types

- [ ] **Types are valid YAWL types**
  - Manual: Only use: string, integer, boolean, float, binary
  - Automated: Validate against allowed type list

- [ ] **Initial values match types**
  - Manual: Initial values are compatible with declared types
  - Automated: Type check initial values against declarations

#### 4.2 Task Data Mappings
- [ ] **Input mappings reference valid variables**
  - Manual: Each input variable exists in net or parent scope
  - Automated: Validate variable references

- [ ] **Output mappings reference valid variables**
  - Manual: Each output variable is properly declared
  - Automated: Validate output variable targets

- [ ] **Mapping types consistent**
  - Manual: Source and target types are compatible
  - Automated: Type compatibility check

#### 4.3 Parameter Types
- [ ] **Pattern parameters typed**
  - Manual: Pattern instance parameters have correct types
  - Automated: Validate parameter type definitions

- [ ] **Multi-instance parameters valid**
  - Manual: min_instances, max_instances are non-negative integers
  - Automated: Range check MI parameters

#### 4.4 Type Validation Using cre_validation
- [ ] **Run type validators**
  - Manual: Review type validation results
  - Automated: `cre_validation:validate_all(Validators, Context)`

- [ ] **No type mismatches**
  - Manual: All type checks pass
  - Automated: Validation returns `ok`, not `{error, _}`

---

## Stage 5: Compilation Success

### Purpose
Verify that the specification compiles to executable code without errors.

### What It Verifies
The YAWL compiler generates valid Erlang modules from the specification. Compilation failures indicate structural problems.

### Checklist

#### 5.1 Compilation Invocation
- [ ] **Compile with yawl_compile**
  - Manual: Run compilation in erlang shell
  - Automated: `{ok, _} = yawl_compile:compile(Spec, #{})`

- [ ] **Compilation succeeds**
  - Manual: No compilation errors reported
  - Automated: Result is `{ok, _}`, not `{error, _}`

#### 5.2 Generated Module Structure
- [ ] **place_lst/0 generated**
  - Manual: Check module has place list export
  - Automated: Validate generated module exports

- [ ] **trsn_lst/0 generated**
  - Manual: Check module has transition list export
  - Automated: Validate generated module exports

- [ ] **init_marking/2 generated**
  - Manual: Check module has initial marking function
  - Automated: Validate generated module exports

- [ ] **preset/1 generated**
  - Manual: Check module has preset function
  - Automated: Validate generated module exports

- [ ] **is_enabled/3 generated**
  - Manual: Check module has enabled check function
  - Automated: Validate generated module exports

- [ ] **fire/3 generated**
  - Manual: Check module has fire function
  - Automated: Validate generated module exports

#### 5.3 Module Code Quality
- [ ] **No syntax errors**
  - Manual: Code compiles with erlc
  - Automated: `compile:file(ModulePath)` returns `{ok, _}`

- [ ] **No undefined functions**
  - Manual: All called functions exist
  - Automated: `xref:d(Module)` returns no errors

- [ ] **No undefined types**
  - Manual: All types used are defined
  - Automated: Dialyzer passes without type errors

#### 5.4 Expanded Elements
- [ ] **Pattern expansion successful**
  - Manual: Pattern instances expanded to places/transitions
  - Automated: `yawl_pattern_expander:expand_patterns_for_net/3`

- [ ] **All preset places in place_lst**
  - Manual: No place in preset that's not in place list
  - Automated: Validate preset is subset of place_lst

- [ ] **All postset places in place_lst**
  - Manual: No place in postset that's not in place list
  - Automated: Validate postset is subset of place_lst

---

## Stage 6: Admissibility Checking

### Purpose
Verify that all workflow constraints are enforced and the workflow is sound.

### What It Verifies
Admissibility ensures the workflow can execute correctly without deadlocks, infinite loops, or unreachable states.

### Checklist

#### 6.1 Structural Soundness
- [ ] **Workflow has input condition**
  - Manual: Net has designated start place
  - Automated: `wf_verify:has_input_condition(Net)` returns true

- [ ] **Workflow has output condition**
  - Manual: Net has designated end place
  - Automated: `wf_verify:has_output_condition(Net)` returns true

- [ ] **All places connected**
  - Manual: No orphan places with no arcs
  - Automated: `wf_verify:connected_places(Net)` returns `{ok, []}`

- [ ] **At least one transition**
  - Manual: Workflow has executable tasks
  - Automated: `wf_verify:has_transitions(Net)` returns true

#### 6.2 Flow Validation
- [ ] **No dead transitions**
  - Manual: Every transition can potentially fire
  - Automated: `wf_verify:dead_transitions(Net)` returns `{ok, []}`

- [ ] **No unreachable states**
  - Manual: All places are reachable from start
  - Automated: Reachability analysis confirms access

- [ ] **Completion option exists**
  - Manual: Path exists from input to output condition
  - Automated: `wf_verify:has_completion_option(Net)` returns `{ok, true}`

#### 6.3 Deadlock Freedom
- [ ] **No circular waits**
  - Manual: No dependency cycles that prevent completion
  - Automated: `wf_verify:deadlock_free(Net)` returns `{ok, true}`

- [ ] **Proper join/split configuration**
  - Manual: Join types match incoming flow counts
  - Automated: `yawl_validate:check_consistency(Spec)` returns no warnings

- [ ] **AND split has AND join**
  - Manual: Parallel branches merge properly
  - Automated: Validate split/join consistency

#### 6.4 Boundedness
- [ ] **Net is structurally bounded**
  - Manual: No place can accumulate infinite tokens
  - Automated: `wf_verify:bounded(Net)` returns `{ok, true}`

- [ ] **No uncontrolled token generation**
  - Manual: No transition produces more than it consumes without compensating path
  - Automated: Incidence matrix analysis confirms boundedness

#### 6.5 Validation Module Results
- [ ] **yawl_validate passes**
  - Manual: Review validation output
  - Automated: `{ok, Warnings} = yawl_validate:validate(Spec)`

- [ ] **No critical errors**
  - Manual: All severity=error issues resolved
  - Automated: Error list is empty

- [ ] **Warnings reviewed**
  - Manual: All warnings understood and acceptable
  - Automated: Document warning disposition

---

## Stage 7: Receipt Verification

### Purpose
Verify that receipts provide complete audit trails for workflow execution.

### What It Verifies
Receipts are immutable records of each state transition, enabling audit trails and replay.

### Checklist

#### 7.1 Receipt Structure
- [ ] **Receipt has before_hash**
  - Manual: Each receipt contains pre-execution marking hash
  - Automated: `maps:is_key(before_hash, Receipt)`

- [ ] **Receipt has after_hash**
  - Manual: Each receipt contains post-execution marking hash
  - Automated: `maps:is_key(after_hash, Receipt)`

- [ ] **Receipt has move**
  - Manual: Each receipt records the transition fired
  - Automated: `maps:is_key(move, Receipt)`

- [ ] **Receipt has timestamp**
  - Manual: Each receipt has monotonically increasing timestamp
  - Automated: `maps:is_key(ts, Receipt)`

#### 7.2 Receipt Content
- [ ] **Move has trsn**
  - Manual: Move records which transition fired
  - Automated: `maps:is_key(trsn, Move)`

- [ ] **Move has mode**
  - Manual: Move records token consumption mode
  - Automated: `maps:is_key(mode, Move)`

- [ ] **Move has produce**
  - Manual: Move records token production map
  - Automated: `maps:is_key(produce, Move)`

#### 7.3 Receipt Type Validation
- [ ] **Using pnet_types validator**
  - Manual: All receipts pass type check
  - Automated: `pnet_types:is_receipt(Receipt)` returns true

- [ ] **All fields correct types**
  - Manual: Hashes are binaries, ts is integer
  - Automated: Type specification validation

#### 7.4 Receipt Chain Integrity
- [ ] **Hashes chain correctly**
  - Manual: Receipt N's after_hash matches Receipt N+1's before_hash
  - Automated: Verify hash chain continuity

- [ ] **Timestamps increase monotonically**
  - Manual: Each receipt has larger timestamp than previous
  - Automated: Check monotonic increase

- [ ] **No missing transitions**
  - Manual: Receipt sequence covers all executed transitions
  - Automated: Compare receipt count to expected execution length

---

## Stage 8: Replay Consistency

### Purpose
Verify that the workflow can be replayed deterministically from receipts.

### What It Verifies
Deterministic replay ensures reproducible execution, which is critical for debugging and audit verification.

### Checklist

#### 8.1 Deterministic Execution
- [ ] **Same seed produces same first move**
  - Manual: Run workflow twice with same seed, compare first transition
  - Automated: `wf_deterministic_replay_test:same_seed_same_first_move_test()`

- [ ] **Same seed produces same hash chain**
  - Manual: Compare complete receipt chains from identical runs
  - Automated: Hash chains are byte-identical

- [ ] **Final marking hash matches**
  - Manual: Same seed results in same final state
  - Automated: `wf_deterministic_replay_test:final_marking_hash_deterministic_test()`

#### 8.2 Receipt-Based Replay
- [ ] **Receipts can reconstruct execution**
  - Manual: Use receipts to replay workflow step-by-step
  - Automated: Replay from receipt sequence produces same outcome

- [ ] **Injection receipts consistent**
  - Manual: Initial injection produces same receipt
  - Automated: `wf_deterministic_replay_test:hash_chain_consistency_test()`

- [ ] **Drain receipts consistent**
  - Manual: Drain sequence produces same receipts
  - Automated: Receipt sequences match byte-for-byte

#### 8.3 Nondeterminism Handling
- [ ] **Choice points use seed**
  - Manual: XOR choices respect random seed
  - Automated: Verify wf_choice uses seed for deterministic selection

- [ ] **Mode enumeration deterministic**
  - Manual: pnet_mode:enum_mode produces same order for same seed
  - Automated: Multiple runs produce identical mode order

- [ ] **Receipt ordering preserved**
  - Manual: Receipts returned in firing order
  - Automated: `wf_deterministic_replay_test:receipts_in_firing_order_test()`

#### 8.4 Audit Log Consistency
- [ ] **Append-only log maintains order**
  - Manual: wf_audit_log preserves receipt sequence
  - Automated: Read receipts in same order as written

- [ ] **Cursor-based reads consistent**
  - Manual: Pagination produces consistent results
  - Automated: Cursor-based reads return same data across reads

- [ ] **Log survives restarts**
  - Manual: Receipts persist across process restart
  - Automated: disk_log persistence verified

---

## Quick Reference Commands

### BCD Validation
```erlang
% Check BCD file exists
file:read_file_info("bcd.md").

% Parse and validate structure
% (Custom parser required for specific BCD format)
```

### Specification Validation
```erlang
% Parse YAML specification
{ok, Spec} = wf_yaml_spec:from_yaml_file("workflow.yaml").

% Validate specification
{ok, Warnings} = yawl_validate:validate(Spec).

% Check flows
yawl_validate:check_flows(Spec).

% Check variables
yawl_validate:check_variables(Spec).
```

### Pattern Coverage
```erlang
% List all patterns
AllPatterns = yawl_pattern_registry:all_patterns().

% Validate specific pattern
true = yawl_pattern_registry:validate_pattern(<<"P1_Sequence">>).

% Get pattern module
yawl_pattern_registry:pattern_module(<<"P4_ExclusiveChoice">>).
```

### Type Discipline
```erlang
% Validate task IDs
ok = cre_validation:validate_task_id(<<"task_1">>).

% Validate all in context
ok = cre_validation:validate_all(Validators, Context).
```

### Compilation Success
```erlang
% Compile specification
{ok, Compiled} = yawl_compile:compile(Spec, #{}).

% Check compiled modules
maps:get(modules, Compiled).

% Verify exports
Module:module_info(exports).
```

### Admissibility Checking
```erlang
% Check soundness
ok = wf_verify:sound(Module).

% Check specific properties
true = wf_verify:has_transitions(Net).
true = wf_verify:has_input_condition(Net).
true = wf_verify:has_output_condition(Net).
{ok, []} = wf_verify:connected_places(Net).
{ok, true} = wf_verify:deadlock_free(Net).
{ok, true} = wf_verify:bounded(Net).
{ok, []} = wf_verify:dead_transitions(Net).
```

### Receipt Verification
```erlang
% Create receipt
R = pnet_receipt:make(BeforeHash, AfterHash, Move).

% Validate receipt type
true = pnet_types:is_receipt(R).

% Check receipt effects
{silent, _} = pnet_receipt:effects(R).
```

### Replay Consistency
```erlang
% Run deterministic test
wf_deterministic_replay_test:deterministic_replay_test().

% Verify hash chain
wf_deterministic_replay_test:hash_chain_consistency_test().

% Open audit log
{ok, Log} = wf_audit_log:open(#{name => test_log, file => tmp}).

% Append and read
ok = wf_audit_log:append(Log, Receipt).
{ok, Receipts, NextCursor} = wf_audit_log:read(Log, 0, 10).
```

---

## Deployment Decision Matrix

| Stage | Critical Issues Block Deployment? | Warnings Block Deployment? | Notes |
|-------|----------------------------------|----------------------------|-------|
| BCD Validation | Yes | No | Missing BCD = no deployment |
| Specification Validation | Yes | Depends | Invalid YAML = no deployment |
| Pattern Coverage | Yes | No | Missing critical patterns = block |
| Type Discipline | Yes | No | Type errors = no deployment |
| Compilation Success | Yes | No | Compilation failure = no deployment |
| Admissibility Checking | Yes | Depends | Deadlock = block; warnings reviewed |
| Receipt Verification | Yes | No | Missing receipt fields = no deployment |
| Replay Consistency | Yes | No | Non-deterministic = no deployment |

---

## Automated Test Suite

To run all verification stages automatically:

```bash
# Compile the project
rebar3 compile

# Run EUnit tests for verification modules
rebar3 eunit --module=yawl_validate
rebar3 eunit --module=cre_validation
rebar3 eunit --module=pnet_types
rebar3 eunit --module=pnet_receipt
rebar3 eunit --module=wf_audit_log
rebar3 eunit --module=wf_deterministic_replay_test
rebar3 eunit --module=wf_verify

# Run pattern coverage tests
rebar3 eunit --module=yawl_pattern_registry

# Run full test suite
rebar3 ct

# Generate coverage report
rebar3 cover --export
```

---

## Appendix: Validation Module Reference

| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `yawl_validate` | YAWL spec validation | `validate/1`, `check_flows/1`, `check_tasks/1` |
| `cre_validation` | Type and field validation | `validate_task_id/1`, `validate_all/2` |
| `pnet_types` | Type validators | `is_marking/1`, `is_receipt/1`, `is_move/1` |
| `wf_verify` | Soundness verification | `sound/1`, `deadlock_free/1`, `bounded/1` |
| `pnet_receipt` | Receipt creation | `make/3`, `effects/1` |
| `wf_audit_log` | Receipt persistence | `open/1`, `append/2`, `read/3` |
| `yawl_pattern_registry` | Pattern lookups | `all_patterns/0`, `validate_pattern/1` |
| `yawl_compile` | Specification compilation | `compile/2`, `generate_module/2` |
| `wf_yaml_spec` | YAML parsing | `from_yaml_file/1`, `validate/1` |

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-07
**Status**: Active
