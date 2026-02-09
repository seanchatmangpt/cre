# Generative Analysis Design Principles

*Definitive reference for architects applying Generative Analysis methodology to workflow and distributed system design.*

---

## Table of Contents

1. [Core Principles](#core-principles)
2. [Structural Principles](#structural-principles)
3. [Documentation Principles](#documentation-principles)
4. [Verification Principles](#verification-principles)
5. [Collaboration Principles](#collaboration-principles)
6. [Evolution Principles](#evolution-principles)

---

## Core Principles

### 1. Admissible-Before Principle

**Statement:** All analysis constructs must be admissible (well-formed and verifiable) before they are used in composition or execution.

**Rationale:** Generative Analysis treats specifications as executable programs. Invalid specifications cannot be executed, analyzed, or transformed safely. The admissible-before principle ensures that type checking, validation, and soundness verification occur before any dynamic behavior, preventing runtime failures and enabling static reasoning.

**Application Guidelines:**
- Validate all type specifications before compilation
- Verify Petri net soundness (option to complete, proper completion, no dead transitions) before execution
- Check pattern instance correctness before workflow expansion
- Validate YAML/XML specifications before parsing to internal representations

**Anti-Patterns:**
- Deferred validation until runtime ("we'll catch it in production")
- Optimistic parsing that tolerates syntax errors
- Dynamic type checking as the primary validation mechanism
- Skipping validation in "fast path" execution

**CRE Examples:**
- `/Users/sac/cre/src/wf/wf_spec.erl:validate/1` - Validates specification structure before compilation
- `/Users/sac/cre/src/wf/wf_yaml_spec.erl:validate/1` - YAML specification validation before use
- `/Users/sac/cre/src/pnet/pnet_types.erl` - Total type validators that never crash, returning boolean()

**Book Reference:** Chapter 2 - Specification, Chapter 3 - Architecture

---

### 2. Traceability Principle

**Statement:** Every transformation from specification to execution must be traceable with receipts recording before/after state, the operation performed, and timestamps.

**Rationale:** Distributed systems require verifiable execution history. Receipts enable debugging, auditing, replay, and state reconstruction. They provide the "why" and "how" for every state transition, making systems introspectable and accountable.

**Application Guidelines:**
- Record receipts for all state transitions (marking changes, transition firings)
- Include cryptographic hashes for state verification
- Timestamp all events for causal ordering
- Make receipt structures serializable for persistence
- Support receipt queries for execution tracing

**Anti-Patterns:**
- Silent state mutations
- In-memory-only state without audit trail
- Mutable timestamps that can be altered post-facto
- Incomplete transaction logs

**CRE Examples:**
- `/Users/sac/cre/src/pnet/pnet_types.erl:232-236` - Receipt type definition with before_hash, after_hash, move, ts
- `/Users/sac/cre/src/pnet/pnet_marking.erl:320-325` - Stable hashing independent of insertion order
- `/Users/sac/cre/src/wf/wf_xes.erl` - XES event logging for process mining

**Book Reference:** Chapter 4 - Refinement, Chapter 6 - Completion

---

### 3. Single Runtime Component Principle

**Statement:** Systems should have exactly one runtime component (one "real" OTP behavior); everything else is pure helpers/utilities plus message contracts.

**Rationale:** A single runtime component reduces cognitive load, eliminates coordination complexity between multiple runtime types, and creates clear boundaries. Pure helpers are testable, composable, and reusable. Message contracts define the interface without coupling to implementation.

**Application Guidelines:**
- Identify the core runtime behavior (e.g., `gen_server`, `gen_statem`)
- Implement all stateful operations within this behavior
- Extract all pure functions to separate modules
- Define message contracts as types and documentation
- Use patterns and utilities for shared behavior

**Anti-Patterns:**
- Multiple gen_servers managing overlapping state
- Business logic scattered across multiple OTP behaviors
- Impure utility functions with hidden side effects
- Undocumented message protocols

**CRE Examples:**
- `/Users/sac/cre/src/core/gen_yawl.erl` - Single YAWL workflow runtime (wraps gen_pnet)
- `/Users/sac/cre/src/pnet/` - Pure Petri net utilities (marking algebra, types, mode)
- `/Users/sac/cre/src/wf/` - Pure workflow utilities (tasks, scopes, timers)

**Book Reference:** Chapter 3 - Architecture

---

### 4. Total Function Principle

**Statement:** All public functions should be total (never crash) and return tagged results for error conditions.

**Rationale:** Total functions enable reliable composition. When a function cannot crash, callers don't need defensive try/catch. Tagged returns make error handling explicit and force acknowledgment of failure cases.

**Application Guidelines:**
- Use `{ok, Result} | {error, Reason}` patterns for operations that can fail
- Return default values rather than crashing for lookups
- Document preconditions; violate them only with explicit errors
- Use guards for type validation, returning error tuples
- Never let exceptions propagate across module boundaries

**Anti-Patterns:**
- Functions that "should never crash" but do on unexpected input
- Returning `undefined` or `null` for error conditions
- Throwing exceptions for expected failure modes
- Implicit assumptions about input validity

**CRE Examples:**
- `/Users/sac/cre/src/pnet/pnet_marking.erl:186-192` - `get/2` returns `{ok, []}` for missing places (total)
- `/Users/sac/cre/src/pnet/pnet_types.erl:269-272` - `is_place/1` never crashes
- `/Users/sac/cre/src/wf/wf_spec.erl` - Accessors return defaults rather than crashing

**Book Reference:** Chapter 2 - Specification, Chapter 4 - Refinement

---

### 5. Deterministic Nondeterminism Principle

**Statement:** Nondeterministic choices must be made explicit through dedicated choice operators, not hidden in implementation details.

**Rationale:** Nondeterminism is a fact of distributed systems (concurrent execution, message ordering). Making it explicit allows testing, simulation, and formal verification. Hidden nondeterminism creates unreproducible bugs.

**Application Guidelines:**
- Use explicit choice operators (e.g., `lib_combin:pick_from/1`)
- Seed all random operations for reproducibility
- Document sources of nondeterminism
- Provide deterministic execution modes for testing
- Log nondeterministic choices in receipts

**Anti-Patterns:**
- Relying on Erlang process spawning order
- Using unseeded random for business logic
- Race conditions treated as "features"
- Implicit ordering assumptions from maps/sets

**CRE Examples:**
- `/Users/sac/cre/src/patterns/exclusive_choice.erl:224` - Uses `lib_combin:pick_from/1` for explicit choice
- `/Users/sac/cre/src/pnet/pnet_choice.erl` - Deterministic nondeterminism utilities
- Token ordering with `lists:sort` for canonical representations

**Book Reference:** Chapter 4 - Refinement, Chapter 5 - Completion

---

## Structural Principles

### 6. Layered Architecture Principle

**Statement:** Organize code into clear layers: core (runtime), patterns (domain logic), utilities (pure functions), and integration (external interfaces).

**Rationale:** Layered architecture creates clear dependencies and prevents circular references. Each layer has a specific responsibility, making the codebase navigable and maintainable.

**Application Guidelines:**
- **Core Layer:** Single runtime behavior with state management
- **Patterns Layer:** Domain-specific patterns (e.g., YAWL workflow patterns)
- **Utilities Layer:** Pure functions for common operations
- **Integration Layer:** External system adapters (HTTP, database, etc.)
- **API Layer:** Public interfaces and client libraries

**Directory Structure:**
```
src/
├── core/          # Runtime behaviors (gen_yawl, gen_pnet)
├── patterns/      # Domain patterns (WCP-01 through WCP-43)
├── pnet/          # Petri net utilities (pure)
├── wf/            # Workflow utilities (pure)
├── api/           # Public APIs
├── integration/   # External integrations
└── app/           # Application modules
```

**Anti-Patterns:**
- Mixing business logic with runtime concerns
- Circular dependencies between modules
- Utility functions depending on runtime state
- Integration code in core modules

**CRE Examples:**
- `/Users/sac/cre/src/core/` - Core runtime behaviors
- `/Users/sac/cre/src/patterns/` - 43 YAWL workflow control patterns
- `/Users/sac/cre/src/pnet/` - Pure Petri net algebra
- `/Users/sac/cre/src/wf/` - Pure workflow utilities

**Book Reference:** Chapter 3 - Architecture

---

### 7. Literate Modeling Principle

**Statement:** Specifications should be written in literate modeling formats that combine documentation, structure, and semantics in a single source.

**Rationale:** Literate modeling keeps documentation synchronized with code. Specifications become both executable programs and human-readable documentation. This reduces the documentation drift problem.

**Application Guidelines:**
- Use YAML for workflow specifications (human-readable, machine-executable)
- Include metadata (title, description, version) in specifications
- Support comments and documentation within specification formats
- Generate documentation from specifications
- Validate specifications at load time

**Anti-Patterns:**
- Separate documentation that quickly becomes stale
- Binary specification formats
- Specifications without descriptive metadata
- Documentation written in a different language than implementation

**CRE Examples:**
- `/Users/sac/cre/src/wf/wf_yaml_spec.erl` - YAML 0.2 specification parser
- `/Users/sac/cre/test/data/agi_symposium_omega.yaml` - Literate workflow specification
- Pattern registry linking pattern IDs to implementing modules

**Book Reference:** Chapter 2 - Specification

---

### 8. Single Source of Truth Principle

**Statement:** Each piece of information should have exactly one authoritative source; all other references are derived.

**Rationale:** Multiple sources for the same information lead to inconsistency bugs. When truth has a single location, updates are straightforward and correctness is verifiable.

**Application Guidelines:**
- Designate authoritative sources for each data type
- Derive all other representations through transformation
- Make derivation paths explicit and testable
- Cache derived data with invalidation mechanisms
- Document source-of-truth relationships

**Anti-Patterns:**
- Duplicating data across multiple stores
- Manual synchronization between representations
- Implicit derivation logic
- Cached data without invalidation

**CRE Examples:**
- YAML specification is source of truth; compiled modules are derived
- Pattern registry maps pattern IDs to modules (single lookup table)
- Marking is source of truth; hash is derived

**Book Reference:** Chapter 2 - Specification, Chapter 6 - Completion

---

## Documentation Principles

### 9. Executable Documentation Principle

**Statement:** Documentation should be executable as tests, not separate text.

**Rationale:** Executable documentation never becomes stale. It serves as both usage examples and regression tests. Doctests bridge the gap between API documentation and verification.

**Application Guidelines:**
- Include doctests in all public modules
- Use doctests for usage examples
- Run doctests in CI/CD pipeline
- Keep doctests focused and fast
- Document edge cases in doctests

**Anti-Patterns:**
- Examples in comments that aren't tested
- Outdated documentation
- Unverified usage patterns
- Manual test procedures

**CRE Examples:**
- `/Users/sac/cre/src/pnet/pnet_marking.erl:24-88` - Doctests showing API usage
- `/Users/sac/cre/src/pnet/pnet_types.erl:22-104` - Doctests for type validators
- All modules include `doctest_test/0` for EUnit integration

**Book Reference:** Chapter 5 - Completion

---

### 10. Type-First Documentation Principle

**Statement:** Types are the primary documentation; all other documentation should elaborate on type contracts.

**Rationale:** Types are mechanically verifiable and never lie. They form the contract between modules. Additional documentation should explain intent, usage patterns, and constraints not expressible in types.

**Application Guidelines:**
- Export all public types
- Use `-opaque` for abstract types with hidden implementation
- Include type documentation in `-moduledoc`
- Use `-spec` for all public functions
- Run Dialyzer in CI

**Anti-Patterns:**
- Untyped public functions
- Documentation that contradicts type signatures
- Hidden implementation details in type names
- Optional type checking

**CRE Examples:**
- `/Users/sac/cre/src/pnet/pnet_types.erl:134-249` - Complete type definitions with documentation
- `/Users/sac/cre/src/wf/wf_spec.erl:112-186` - Type exports for opaque records

**Book Reference:** Chapter 2 - Specification

---

### 11. Semantic Naming Principle

**Statement:** Names should reflect domain semantics, not implementation details.

**Rationale:** Domain-oriented names make code self-documenting and align the implementation with business concepts. Implementation names change frequently; domain concepts are stable.

**Application Guidelines:**
- Use domain terminology from specifications
- Avoid generic names (data, info, thing)
- Prefix related types/modules (e.g., `pnet_`, `wf_`, `yawl_`)
- Use verbs for functions, nouns for types
- Consider YAWL pattern naming (WCP-01, WCP-02, etc.)

**Anti-Patterns:**
- Names encoding implementation details (e.g., `HashListStruct`)
- Abbreviations that require decoding
- Generic names that provide no information
- Inconsistent naming conventions

**CRE Examples:**
- `pnet_marking` - Petri net marking (domain concept)
- `wf_task` - Workflow task (domain concept)
- `exclusive_choice` - WCP-04 pattern name
- Place/transition names: `p_start`, `p_end`, `t_select_a`

**Book Reference:** Chapter 2 - Specification

---

## Verification Principles

### 12. Soundness Verification Principle

**Statement:** All workflow patterns must satisfy Petri net soundness properties before deployment.

**Rationale:** Soundness guarantees that workflows complete properly, don't deadlock, and have no dead transitions. These are the minimum correctness properties for any workflow system.

**Soundness Properties:**
1. **Option to Complete:** From any reachable state, the final marking is reachable
2. **Proper Completion:** When the final marking is reached, it contains exactly the expected tokens
3. **No Dead Transitions:** Every transition participates in some firing sequence

**Application Guidelines:**
- Implement soundness checkers for all pattern implementations
- Verify soundness at pattern registration time
- Include soundness tests in test suites
- Document soundness properties for each pattern
- Provide counterexamples for unsound specifications

**Anti-Patterns:**
- Assuming patterns are sound without verification
- Relying on testing alone for soundness
- Unchecked pattern composition
- Manual soundness proofs without automation

**CRE Examples:**
- `/Users/sac/cre/test/cre_yawl_SUITE.erl` - Soundness tests for all WCP patterns
- Pattern validation in `yawl_compile:compile/1`
- Petri net structure validation before execution

**Book Reference:** Chapter 3 - Architecture, Chapter 5 - Completion

---

### 13. Property-Based Testing Principle

**Statement:** Use property-based tests to verify invariants across input space, not just hand-selected examples.

**Rationale:** Hand-selected examples test known cases. Property-based tests discover edge cases and verify invariants hold across the entire input space. They find bugs that example-based testing misses.

**Application Guidelines:**
- Identify invariants for each module
- Use PropEr or QuickCheck for property-based testing
- Include generators for custom types
- Test stateful systems with state machine properties
- Run property tests in CI

**Anti-Patterns:**
- Only testing happy paths
- Hardcoded test data
- Tests that verify implementation rather than properties
- Untested edge cases

**CRE Examples:**
- `/Users/sac/cre/test/cre_yawl_SUITE.erl:78-200` - Multiple test cases per pattern
- Marking algebra properties (commutativity, associativity)
- Token multiplicity preservation

**Book Reference:** Chapter 5 - Completion

---

### 14. Test Coverage Minimum Principle

**Statement:** Maintain at least 80% code coverage with 100% coverage for critical paths.

**Rationale:** Coverage metrics provide quantitative confidence in test completeness. Critical paths (security, data integrity, state transitions) require complete coverage.

**Application Guidelines:**
- Measure coverage with `rebar3 cover`
- Set coverage minimums in CI
- Review uncovered code; add tests or document exceptions
- Focus on branch coverage, not just line coverage
- Treat coverage as a floor, not a ceiling

**Anti-Patterns:**
- Low coverage with "we'll add tests later"
- Gaming coverage metrics with useless tests
- Ignoring uncovered edge cases
- Coverage without meaningful assertions

**CRE Examples:**
- CRE maintains 90%+ test coverage (689 of 760 tests passing)
- Coverage reports in `_build/test/cover/`
- Pattern-specific test suites for each WCP pattern

**Book Reference:** Chapter 5 - Completion

---

### 15. Chicago School TDD Principle

**Statement:** Write tests before implementation to drive design, not just verify correctness.

**Rationale:** Test-First Development (Chicago School) ensures that code is testable by construction. Tests serve as design documentation, preventing over-engineering and clarifying interfaces.

**Application Guidelines:**
- Write failing test first
- Implement minimal code to pass test
- Refactor for clarity
- Test one thing per test
- Use descriptive test names

**Anti-Patterns:**
- Writing code before tests
- Tests that merely confirm implementation
- Tests that require knowledge of internals
- Overspecified tests that restrict refactoring

**CRE Examples:**
- EUnit test structure with descriptive test names
- Pattern validation tests written before pattern implementation
- Test files organized by pattern (e.g., `pnet_choice_coverage_SUITE.erl`)

**Book Reference:** Chapter 4 - Refinement, Chapter 5 - Completion

---

## Collaboration Principles

### 16. Interface Segregation Principle

**Statement:** Modules should expose minimal, focused interfaces rather than large, monolithic APIs.

**Rationale:** Small interfaces are easier to understand, test, and version. Clients only depend on what they use. Changes to unused functionality don't affect clients.

**Application Guidelines:**
- Design modules around single responsibilities
- Export only necessary functions
- Use `-export_type` for type exports
- Provide opaque types for data hiding
- Document intended usage patterns

**Anti-Patterns:**
- God modules with many exports
- Exporting all functions "just in case"
- Tight coupling through large interfaces
- Underscore-prefixed "private" exports

**CRE Examples:**
- `pnet_marking` exports only marking algebra operations
- `pnet_types` exports only type validators
- Each pattern module exports only pattern-specific API

**Book Reference:** Chapter 3 - Architecture

---

### 17. Message Contract Principle

**Statement:** All inter-module and inter-process communication should follow explicit, documented message contracts.

**Rationale:** Explicit contracts enable independent development, testing, and evolution. Message contracts form the API between components and should be as carefully designed as function interfaces.

**Application Guidelines:**
- Document message formats with types
- Include message versioning
- Handle unknown messages gracefully
- Use tagged tuples for structured messages
- Provide message constructors and destructors

**Anti-Patterns:**
- Implicit message formats
- Sending raw terms without structure
- Tight coupling through message internals
- Breaking message formats without versioning

**CRE Examples:**
- `gen_yawl` callback messages (trigger, sync, step, drain)
- YAWL pattern state records
- Receipt format for move execution

**Book Reference:** Chapter 3 - Architecture

---

### 18. Handoff Protocol Principle

**Statement:** Define clear handoff protocols between teams and subsystems with acceptance criteria.

**Rationale:** Clear handoffs prevent integration failures. When teams agree on interfaces and acceptance criteria upfront, integration becomes straightforward rather than a source of bugs.

**Application Guidelines:**
- Specify input/output contracts for handoffs
- Include test cases as part of handoff criteria
- Document assumptions and dependencies
- Provide mock implementations for testing
- Review and sign off on handoff agreements

**Anti-Patterns:**
- Informal handoffs with verbal agreements
- Changing interfaces without notification
- Missing acceptance criteria
- Integration as an afterthought

**CRE Examples:**
- Pattern registry documents interface between patterns and compiler
- YAML schema as handoff between workflow designers and implementers
- Test suites define acceptance for each pattern

**Book Reference:** Chapter 6 - Completion

---

## Evolution Principles

### 19. Backward Compatibility Principle

**Statement:** Changes should maintain backward compatibility or provide explicit migration paths.

**Rationale:** Breaking changes cascade through all dependent systems. Explicit migration paths allow controlled updates and prevent production incidents.

**Application Guidelines:**
- Version all public APIs
- Deprecate before removing
- Provide migration guides
- Support multiple versions during transition
- Communicate breaking changes clearly

**Anti-Patterns:**
- Silent breaking changes
- Removing functions without deprecation
- Assuming all clients can update simultaneously
- Undocumented behavior changes

**CRE Examples:**
- YAWL XML and YAML specifications support multiple versions
- gen_pnet callback interface evolution for OTP 25+ compatibility
- Configuration-based feature flags

**Book Reference:** Chapter 6 - Completion

---

### 20. Extension Before Modification Principle

**Statement:** Extend systems through composition and plugins rather than modifying core code.

**Rationale:** Core code is battle-tested and risky to modify. Extensions can be added and removed without affecting stability. Composition enables reuse and flexibility.

**Application Guidelines:**
- Design plugin interfaces for extensibility
- Use behavior modules for extension points
- Support dynamic registration of extensions
- Keep core code minimal and stable
- Provide extension examples

**Anti-Patterns:**
- Modifying core for each new feature
- Copy-pasting code for variations
- Tight coupling to implementation details
- Hard-coded extension logic

**CRE Examples:**
- YAWL patterns as plugins registered in pattern registry
- `gen_yawl` behavior for custom workflow implementations
- Worker callbacks for external task execution

**Book Reference:** Chapter 3 - Architecture, Chapter 6 - Completion

---

### 21. Isolation Principle

**Statement:** Changes should be isolated to prevent cascading failures and simplify testing.

**Rationale:** Isolated changes are safer, easier to test, and can be deployed independently. When failures are contained, systems remain operational despite bugs.

**Application Guidelines:**
- Separate concerns into independent modules
- Use sandboxed execution for untrusted code
- Implement circuit breakers for external dependencies
- Design for failure (graceful degradation)
- Test modules in isolation

**Anti-Patterns:**
- Shared mutable state
- Tightly coupled modules
- Cascading failures from single bugs
- Testing only integrated systems

**CRE Examples:**
- Each pattern module is independently testable
- Petri net utilities have no external dependencies
- Workflow execution isolated in gen_server processes

**Book Reference:** Chapter 3 - Architecture

---

### 22. Observability Principle

**Statement:** Systems must emit events for all state changes, enabling external monitoring and debugging.

**Rationale:** You cannot debug what you cannot see. Comprehensive observability enables production debugging, performance analysis, and system understanding.

**Application Guidelines:**
- Log all state transitions with receipts
- Emit structured events (XES, OpenTelemetry)
- Include correlation IDs for request tracing
- Make observability a core concern, not an add-on
- Support configurable log levels

**Anti-Patterns:**
- Silent failures
- Unstructured log messages
- Debug-only observability
- Manual instrumentation

**CRE Examples:**
- `/Users/sac/cre/src/wf/wf_xes.erl` - XES event logging
- `/Users/sac/cre/src/wf/yawl_telemetry.erl` - OpenTelemetry integration
- Receipt generation for all transition firings

**Book Reference:** Chapter 6 - Completion

---

## Appendix: Principle Checklist

Use this checklist when reviewing architecture and implementation:

### Core Principles
- [ ] Specifications validated before use
- [ ] All state transitions generate receipts
- [ ] Single runtime component identified
- [ ] Functions are total with tagged returns
- [ ] Nondeterminism is explicit

### Structural Principles
- [ ] Clear layer separation
- [ ] Literate modeling format used
- [ ] Single source of truth for each data type
- [ ] Documentation in appropriate layer

### Documentation Principles
- [ ] Doctests for public APIs
- [ ] Types exported and documented
- [ ] Domain-oriented naming
- [ ] Examples are executable

### Verification Principles
- [ ] Soundness verified for patterns
- [ ] Property-based tests for invariants
- [ ] Coverage meets minimum threshold
- [ ] Tests written before implementation

### Collaboration Principles
- [ ] Minimal, focused interfaces
- [ ] Message contracts documented
- [ ] Handoff protocols defined
- [ ] API reviews conducted

### Evolution Principles
- [ ] Backward compatibility maintained
- [ ] Extensions preferred over modifications
- [ ] Changes isolated
- [ ] Observability integrated

---

## Index of CRE Examples

| Principle | File | Line Reference |
|-----------|------|----------------|
| Admissible-Before | `wf_spec.erl` | `validate/1` |
| Traceability | `pnet_types.erl` | `232-236` (receipt) |
| Single Runtime | `gen_yawl.erl` | Module header |
| Total Functions | `pnet_marking.erl` | `186-192` (get/2) |
| Deterministic Choice | `exclusive_choice.erl` | `224` (pick_from) |
| Layered Architecture | Directory structure | `src/{core,patterns,pnet,wf}` |
| Literate Modeling | `wf_yaml_spec.erl` | Full module |
| Single Source of Truth | Pattern registry | `yawl_pattern_registry` |
| Executable Docs | `pnet_marking.erl` | `24-88` (doctests) |
| Type-First Docs | `pnet_types.erl` | `134-249` (types) |
| Soundness | `cre_yawl_SUITE.erl` | `78-200` (tests) |
| Observability | `wf_xes.erl` | Full module |

---

*This document is a living reference. As CRE evolves, principles will be refined and new examples added.*
