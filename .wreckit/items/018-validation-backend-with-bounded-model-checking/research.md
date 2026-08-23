# Research: Validation backend with bounded model checking

**Date**: 2025-01-11
**Item**: 018-validation-backend-with-bounded-model-checking

## Research Question
Complex workflows can have subtle bugs like deadlocks, unreachable states, or incorrect synchronization. Need automated validation beyond unit tests to catch these issues before production.

**Motivation:** Provides formal verification of workflow correctness, catches design flaws early, supports confidence in pattern implementations, and is a competitive advantage for workflow reliability.

**Success criteria:**
- Detect at least one known deadlock case
- Bounded exploration completes in reasonable time
- Reports specific validation issues
- Not used in runtime path

**Technical constraints:**
- Validation backend only (not runtime)
- Compile to Petri net or LTS
- Bounded exploration (depth D, token bound K)
- Check: dead transitions, completion option, deadlock

**Signals:** priority: high, urgency: Key quality assurance requirement

## Summary

CRE is a YAWL (Yet Another Workflow Language) workflow engine implemented in Erlang/OTP with 36 of 43 YAWL patterns implemented. The project already has a sophisticated Petri net foundation through the `gen_pnet` library and a comprehensive Petri net type system in place. However, there is currently **no formal validation backend** that performs bounded model checking to detect deadlocks, livelocks, or unreachable states.

The implementation requires creating a new validation backend that:
1. **Compiles YAWL workflows to Petri nets** - leveraging the existing `pnet_*` modules (`pnet_types`, `pnet_marking`, `pnet_mode`)
2. **Performs bounded exploration** - using depth-limited state space exploration with token bounds
3. **Detects workflow defects** - dead transitions, deadlocks, and completion problems
4. **Reports actionable issues** - specific errors with location information

The existing codebase provides excellent building blocks:
- **Petri net infrastructure**: `/Users/sac/cre/src/pnet/` contains `pnet_types.erl`, `pnet_marking.erl`, `pnet_mode.erl` for Petri net operations
- **Workflow patterns**: `/Users/sac/cre/src/patterns/` has 36+ pattern implementations (sequence, parallel_split, synchronization, etc.)
- **YAWL validation**: `/Users/sac/cre/src/yawl/yawl_schema.erl` and `/Users/sac/cre/src/core/yawl_validate.erl` provide specification validation
- **Pattern registry**: `/Users/sac/cre/src/core/yawl_pattern_registry.erl` maps pattern IDs to implementations

## Current State Analysis

### Existing Implementation

#### Petri Net Infrastructure
The codebase has a robust Petri net foundation in `/Users/sac/cre/src/pnet/`:

- **`pnet_types.erl`** (lines 1-558): Type validators for Petri net data structures
  - Defines `place()`, `trsn()`, `token()`, `marking()`, `mode()`, `cmode()`, `move()`, `receipt()` types
  - All validation functions are total (never crash) - safe for use in guards
  - Provides `is_marking/1`, `is_mode/1`, `is_cmode/1`, `is_move/1` validators

- **`pnet_marking.erl`** (lines 1-488): Multiset marking algebra
  - `new/1`: Creates empty marking with given places
  - `get/2`, `set/3`: Basic marking operations
  - `add/2`, `take/2`: Multiset union/subtraction with multiplicity
  - `apply/2`, `apply/3`: Atomic consume+produce operations
  - `hash/1`: Stable hash independent of insertion order (SHA-256)
  - Key insight: The marking representation is `#{place() => [token()]}` - places map to token lists

- **`pnet_mode.erl`** (lines 1-353): Mode enumeration (input token selections)
  - `preset_counts/1`: Counts multiplicity of places in preset list
  - `enum_modes/2`: Enumerates deterministic modes for transition firing
  - `enum_cmodes/4`: Enumerates colored modes with variable bindings
  - Uses `combinations/2` to generate all valid token selections from places

#### YAWL Workflow System
- **`yawl_schema.erl`** (lines 1-1104): YAWL 2.0 XML specification parser and validator
  - Parses YAWL XML specifications into internal format
  - `parse_specification/1`: Reads file or XML string
  - `validate_specification/1`: Checks structure, references, split/join consistency, cycles
  - `to_internal_format/1`: Converts to `cre_yawl` workflow format
  - Cycle detection using DFS (lines 903-951)

- **`yawl_validate.erl`** (lines 1-1197): Comprehensive YAWL specification validator
  - `validate/1`: Main validation API returning errors and warnings
  - `check_tasks/1`: Validates task types, split/join configurations, multi-instance parameters
  - `check_flows/1`: Validates flow references, detects duplicates, isolated nodes, self-loops
  - `check_decompositions/1`: Validates decomposition references and circular references
  - Key validation: XOR split with AND join warning (line 378-383), OR split with XOR join warning (line 384-389)

#### Workflow Patterns
- **`yawl_pattern_registry.erl`** (lines 1-199): Maps 43 pattern IDs to module names
  - Registers patterns P1_Sequence through P43_ExplicitTermination
  - `pattern_module/1`: Looks up module for pattern ID
  - `all_patterns/0`: Returns list of all registered patterns

- **Pattern implementations** in `/Users/sac/cre/src/patterns/`: 36+ YAWL patterns
  - Basic patterns: `sequence.erl`, `parallel_split.erl`, `synchronization.erl`
  - Choice patterns: `exclusive_choice.erl`, `multiple_choice.erl`, `deferred_choice.erl`
  - Advanced patterns: `discriminator.erl`, `n_out_of_m.erl`, `critical_section.erl`
  - Cancellation patterns: `cancel_activity.erl`, `cancel_case.erl`, `cancel_region.erl`

#### Pattern Testing and Validation
- **`yawl_pattern_tests.erl`** (lines 1-200+): Comprehensive test suite for patterns
  - Tests all 15 reference patterns with XES log validation
  - `verify_soundness/1`: Verifies soundness properties for patterns
  - Tests check pattern validity, execution, trace generation, soundness

### Key Files

#### Core Petri Net Modules
- `/Users/sac/cre/src/pnet/pnet_types.erl:1-558` - Type definitions for places, transitions, markings, modes
- `/Users/sac/cre/src/pnet/pnet_marking.erl:1-488` - Marking algebra (add, take, apply operations)
- `/Users/sac/cre/src/pnet/pnet_mode.erl:1-353` - Mode enumeration for transition firing

#### Workflow Specification and Validation
- `/Users/sac/cre/src/yawl/yawl_schema.erl:1-1104` - YAWL XML parsing and validation
- `/Users/sac/cre/src/core/yawl_validate.erl:1-1197` - Comprehensive YAWL validation
- `/Users/sac/cre/src/core/yawl_pattern_registry.erl:1-199` - Pattern ID to module mapping

#### Validation Infrastructure
- `/Users/sac/cre/src/cre_validation.erl:1-491` - Generic validation utilities (field validation, bounds checking)
- `/Users/sac/cre/src/core/yawl_compiled.erl` - (exists, likely compiled workflow representation)

#### Pattern Implementations
- `/Users/sac/cre/src/patterns/sequence.erl` - WCP-01: Sequence pattern
- `/Users/sac/cre/src/patterns/parallel_split.erl` - WCP-02: Parallel split
- `/Users/sac/cre/src/patterns/exclusive_choice.erl` - WCP-04: Exclusive choice
- `/Users/sac/cre/src/patterns/critical_section.erl` - WCP-39: Critical section (potential deadlock case)

#### Dependencies
- `gen_pnet` (GitHub dependency in `rebar.config:36`): Generic Petri net OTP behavior
- `lib_combin` (GitHub dependency in `rebar.config:37`): Combinatorics library likely used for enumeration

## Technical Considerations

### Dependencies

#### External Dependencies
- **gen_pnet**: Generic Petri net OTP behavior library
  - Provides `pnet_net` behavior for implementing Petri nets
  - Already integrated in the project
  - Likely used for runtime execution, not validation

- **lib_combin**: Combinatorics library
  - May provide algorithms for state space enumeration
  - Could be useful for generating all possible firing sequences

#### Internal Modules to Integrate With
- **`pnet_types`**: Use type definitions for validation backend Petri nets
- **`pnet_marking`**: Use marking operations for state tracking during exploration
- **`pnet_mode`**: Use mode enumeration to find all possible transition firings
- **`yawl_schema`**: Parse workflows from YAWL XML or convert internal format
- **`yawl_validate`**: Integrate with existing validation for pre-checks
- **`yawl_pattern_registry`**: Map pattern IDs to implementations for compilation

### Patterns to Follow

#### Existing Validation Pattern
From `/Users/sac/cre/src/core/yawl_validate.erl:158-186`:
```erlang
-spec validate(Spec :: specification()) -> validation_result().
validate(Spec) when is_map(Spec) ->
    AllErrors = lists:flatten([
        check_required_elements(Spec),
        check_tasks(Spec),
        check_flows(Spec),
        check_decompositions(Spec),
        check_variables(Spec),
        check_consistency(Spec)
    ]),
    {Errors, Warnings} = lists:partition(
        fun(#{severity := Sev}) -> Sev =:= error end,
        AllErrors
    ),
    case Errors of
        [] -> {ok, Warnings};
        _ -> {error, Errors ++ Warnings}
    end;
```

The validation backend should follow this pattern:
1. Accept a specification as input
2. Run multiple validation checks
3. Separate errors from warnings
4. Return structured results with specific error codes

#### Petri Net Type Pattern
From `/Users/sac/cre/src/pnet/pnet_types.erl:164-164`:
```erlang
-type marking() :: #{place() => [token()]}.
```

Represent workflow state as markings where:
- Places = workflow nodes (tasks, conditions, synchronization points)
- Tokens = control flow tokens (active instances, pending completions)

#### Error Reporting Pattern
From `/Users/sac/cre/src/core/yawl_validate.erl:110-116`:
```erlang
-type validation_error() :: #{
          type => required | structure | semantic | reference,
          severity => error | warning,
          message => binary(),
          location => binary() | undefined,
          code => atom()
         }.
```

Validation backend should report issues using this same structure for consistency.

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **State space explosion** - Complex workflows may have too many states to explore | High | Implement bounded exploration with configurable depth D and token bound K; use pruning strategies; prioritize error detection over completeness |
| **Compilation complexity** - Converting YAWL patterns to Petri nets correctly is non-trivial | High | Start with simple patterns (sequence, parallel split); reuse existing pattern implementations; validate compilation against known correct patterns |
| **False positives** - Validation may report errors that aren't actual bugs | Medium | Provide clear error messages with execution traces; allow users to suppress known warnings; document known edge cases |
| **Performance issues** - Validation may be slow for large workflows | Medium | Use memoization for repeated sub-patterns; implement incremental validation; provide timeout mechanism |
| **Integration issues** - May conflict with existing validation code | Low | Create separate module namespace (e.g., `yawl_model_checker`); keep as opt-in validation initially; ensure non-interference with runtime |

## Recommended Approach

Based on the research findings, here's the recommended implementation strategy:

### Phase 1: Foundation (Core Petri Net Compilation)
1. **Create `yawl_model_checker` module** in `/Users/sac/cre/src/validate/`
   - Main API for validation backend
   - Follow pattern of `yawl_validate.erl` for structure

2. **Implement workflow to Petri net compilation**
   - Create `yawl_pnet_compiler` module
   - Use existing `pnet_types` for type definitions
   - Compile YAWL tasks → places, flows → transitions
   - Start with patterns: sequence (WCP-01), parallel split (WCP-02), synchronization (WCP-03)

3. **Implement state representation**
   - Use `pnet_marking:marking()` for state
   - Map workflow control flow to token distribution
   - Use `pnet_mode:enum_modes/2` for finding enabled transitions

### Phase 2: Bounded Exploration
1. **Implement bounded depth-first search**
   - Depth limit D (configurable, default 10-20)
   - Token bound K per place (configurable, default 5-10)
   - Track visited states using `pnet_marking:hash/1`

2. **State space traversal**
   - Use `pnet_mode:enum_modes/2` to get all enabled transitions
   - For each mode, apply via `pnet_marking:apply/2` to get successor state
   - Track path from initial state for error reporting

3. **Deadlock detection**
   - State with no enabled transitions but not in final marking
   - Report with full execution trace

### Phase 3: Property Checking
1. **Dead transition detection**
   - Mark all transitions as "potentially fireable" during compilation
   - Track which transitions actually fired during exploration
   - Report transitions never fired within depth bound

2. **Completion option check**
   - Verify at least one path reaches final marking (all tokens at output condition)
   - Use reachability analysis from initial marking

3. **Structured deadlock detection**
   - Detect critical section deadlocks (WCP-39 pattern)
   - Detect resource allocation deadlocks
   - Detect synchronization mismatches (XOR split with AND join)

### Phase 4: Integration and Testing
1. **Integrate with existing validation**
   - Add `check_model_properties/1` to `yawl_validate`
   - Call as optional validation step
   - Combine results with existing validation errors

2. **Create test cases**
   - Known deadlock workflows (e.g., incorrect critical section)
   - Known dead transition workflows (unreachable tasks)
   - Known non-completing workflows (livelock scenarios)

3. **Performance optimization**
   - Implement state caching
   - Add early termination on error discovery
   - Provide progress reporting for long-running validations

### Module Structure
```
/Users/sac/cre/src/validate/
  ├── yawl_model_checker.erl          % Main API
  ├── yawl_pnet_compiler.erl           % Workflow → Petri net
  ├── yawl_explorer.erl                % Bounded state exploration
  ├── yawl_property_checker.erl        % Property verification
  └── yawl_model_checker_tests.erl     % Test suite
```

### API Design (Following Existing Patterns)
```erlang
%% Main validation entry point
-spec check_model(Spec :: yawl_schema:specification(), Options :: map()) ->
    {ok, [validation_error()]} | {error, Reason}.

%% Compile workflow to Petri net
-spec compile_to_pnet(Spec :: yawl_schema:specification()) ->
    {ok, pnet_types:marking(), [pnet_types:trsn()]} | {error, Reason}.

%% Bounded exploration
-spec explore(State :: pnet_types:marking(), Bounds :: bounds()) ->
    {ok, [execution_trace()]} | {error, Reason}.

%% Property checking
-spec check_deadlock(Traces :: [execution_trace()]) ->
    [validation_error()].

-spec check_completion(Traces :: [execution_trace()]) ->
    [validation_error()].

-spec check_dead_transitions(Traces :: [execution_trace()], AllTransitions :: [pnet_types:trsn()]) ->
    [validation_error()].
```

## Open Questions

1. **Scope of initial implementation**: Should we target all 36 implemented patterns or start with a subset (e.g., basic control flow: sequence, parallel split, synchronization, exclusive choice)?

2. **Depth and token bounds**: What are reasonable default values for depth D and token bound K? Should these be configurable per workflow type?

3. **Known deadlock test cases**: Are there existing workflow examples in the codebase that exhibit known deadlock behavior we can use for validation?

4. **Performance requirements**: What is the maximum acceptable validation time for a typical workflow? This will guide our exploration bounds.

5. **Integration path**: Should this be an opt-in validation step (e.g., via command-line flag) or integrated into all `yawl_validate:validate/1` calls?

6. ** LTS vs Petri net**: The item mentions "Compile to Petri net or LTS" - is there a preference? Given the existing `pnet_*` infrastructure, Petri nets seem more natural.

7. **Error reporting granularity**: Should we report individual deadlocks/warnings or aggregate by pattern/region?

8. **Backward compatibility**: Must this work with existing YAWL XML files, or can we focus on the internal CRE workflow format?

9. **Testing strategy**: Should validation be tested against formal models (e.g., model checkers like SPIN) or against known bug patterns from real workflows?

10. **Cancellation semantics**: CRE has cancellation patterns (WCP 19-20, 25) - how should these be modeled in Petri nets for validation?
