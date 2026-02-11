# Research: Documentation suite

**Date**: 2025-01-18
**Item**: 027-documentation-suite

## Research Question
Complex system with novel approach to workflow patterns requires thorough documentation for understanding, usage, and maintenance.

**Motivation:** Enables knowledge transfer, supports onboarding, documents design decisions, provides semantics reference, operational guidance.

**Success criteria:**
- All 5 docs created and complete
- Semantics clearly defined
- Pattern mapping table included
- Testing strategy documented
- Ops guidance provided

**Signals:** priority: medium, urgency: Required for maintainability

## Summary

CRE (Common Runtime Environment) is a sophisticated YAWL workflow engine built on Erlang/OTP with a novel **Petri net-based execution model** following Joe Armstrong's design philosophy: "one real OTP runner (gen_pnet), everything else pure helpers/utilities." The system implements **all 43 YAWL workflow patterns**, features structured cancellation semantics, multiple instance semantics, effect system with receipts, deterministic scheduling with replay support, and comprehensive pattern-based workflow construction.

**Key Finding:** CRE has **extensive but fragmented documentation** across multiple locations and formats:
- ✅ **Architecture documentation** exists (`docs/ARCHITECTURE.md`) - Petri net foundation, gen_pnet/gen_yawl design
- ✅ **Pattern reference** exists (`docs/43_PATTERNS_COMPLETE.md`, `docs/YAWL_PATTERNS_REFERENCE.md`) - All 43 patterns documented
- ✅ **API documentation** exists (`docs/api/core/COMPLETE_API_REFERENCE.md`, `docs/api/core/CLIENT_API_COMPLETE_REFERENCE.md`) - Core client APIs
- ⚠️ **Semantics documentation** is scattered - Pattern semantics exist but not consolidated
- ❌ **Pattern mapping table** missing - No comprehensive map from YAWL patterns to implementation modules
- ❌ **Testing strategy** incomplete - No unified testing guide (item 024 addresses this)
- ❌ **Operational guidance** partial - Deployment docs exist but ops runbooks incomplete

**What needs to be done:**
This item requires creating **5 comprehensive documentation documents**:
1. **Semantics Reference** - Formal semantics for workflow execution, cancellation, multiple instances, effects
2. **Pattern Mapping Table** - Complete mapping of 43 YAWL patterns → implementation modules → files → line numbers
3. **Design Decisions Document** - Rationale for key architectural choices (Petri nets, pure functional design, gen_yawl wrapper)
4. **Testing Strategy Guide** - Comprehensive testing approach (unit, property-based, model checking, benchmarks)
5. **Operational Guidance** - Production deployment, monitoring, troubleshooting, runbooks

## Current State Analysis

### Existing Documentation

#### 1. Architecture Documentation (`docs/ARCHITECTURE.md`)

**File:** `/Users/sac/cre/docs/ARCHITECTURE.md:1-300+`

**Content:**
- Joe Armstrong design philosophy explained
- Petri net foundation documented
- gen_pnet/gen_yawl separation described
- Module organization by directory
- Core OTP behaviors documented (place_lst/0, trsn_lst/0, fire/3, etc.)

**Status:** ✅ Complete - Excellent foundation, clear diagrams

**Gaps:**
- Does not cover newer systems: ln_* modules (Linear Nesting bytecode execution)
- Does not document effect system architecture
- Missing cancellation semantics details
- No multiple instance semantics documentation

#### 2. Pattern Documentation

**`docs/43_PATTERNS_COMPLETE.md:1-190`** - Claims all 43 patterns implemented
- Table of pattern IDs → modules
- Architecture compliance notes
- AGI Symposium Ω support

**`docs/yawl_patterns/README.md:1-81`** - Pattern overview
- Pattern categorization
- DOT diagram references
- Usage examples

**Status:** ⚠️ Pattern lists exist but lack:
- Formal semantics for each pattern
- Execution behavior details
- Integration examples
- Performance characteristics

#### 3. API Documentation

**`docs/api/core/COMPLETE_API_REFERENCE.md`** - Comprehensive API docs
- cre_client API documented
- cre_yawl_client API documented (appears to be stub)
- Type specifications included

**`docs/api/core/CLIENT_API_COMPLETE_REFERENCE.md`** - Client API details
- start_link, eval, cre_reply, stop functions
- Comprehensive -moduledoc and -doc examples

**Status:** ✅ Well-documented following Erlang documentation conventions

**Gaps:**
- Public API surface is fragmented (item 026 identifies this)
- No unified case management API documentation
- Options (scheduler_policy, step_quanta, trace_level, effect_handler) not clearly documented

#### 4. Tutorial and Guide Documentation

**`docs/tutorials/getting_started.md`** - Getting started guide
**`docs/tutorials/basic_patterns_tutorial.md`** - Basic patterns
**`docs/tutorials/advanced_patterns_tutorial.md`** - Advanced patterns
**`docs/tutorials/HANDLER_DEVELOPMENT.md`** - Handler development
**`docs/guides/telemetry.md`** - Telemetry guide
**`docs/guides/timeout_configuration.md`** - Timeout configuration

**Status:** ✅ Good tutorial coverage

**Gaps:**
- No semantics deep-dive
- No pattern mapping reference
- No testing strategy guide
- No operational runbooks

#### 5. Research Documents (Excellent Sources)

**`/Users/sac/cre/.wreckit/items/014-structured-cancellation-semantics/research.md:1-598`**
- Comprehensive cancellation semantics analysis
- Scope types (activity, case, region) documented
- Integration points identified (wf_cancel, wf_scope, yawl_state)
- Open questions documented

**`/Users/sac/cre/.wreckit/items/015-multiple-instance-semantics/research.md:1-795`**
- Multiple instance semantics fully analyzed
- Join policies (all, first_n, n_of_m, discriminator) documented
- Instance spawning strategies (fixed, runtime, dynamic) explained
- Result aggregation strategies identified

**`/Users/sac/cre/.wreckit/items/016-effect-system-with-receipts/research.md:1-933`**
- Effect system architecture detailed
- Receipt system documented
- Idempotency mechanisms explained
- Compensation patterns described

**`/Users/sac/cre/.wreckit/items/021-core-control-flow-pattern-implementations/research.md:1-299`**
- Pattern implementation architecture documented
- Petri net compilation explained
- Pattern registry identified
- Test coverage analyzed

**`/Users/sac/cre/.wreckit/items/024-comprehensive-testing-infrastructure/research.md:1-100`** (partial read)
- Testing infrastructure gaps identified
- Property-based testing needs documented
- Bounded model checking requirements listed

**`/Users/sac/cre/.wreckit/items/026-public-api-surface/research.md:1-284`**
- API fragmentation documented
- Option handling patterns identified
- Public API design recommendations provided

**Status:** ✅ **Excellent research documents** - These are GOLD MINES for documentation content

### Key Files for Documentation Sources

#### Architecture and Design
| File | Lines | Content | Usage |
|------|-------|---------|-------|
| `/Users/sac/cre/docs/ARCHITECTURE.md` | 1-300+ | Petri net foundation, gen_pnet design | Semantics Reference |
| `/Users/sac/cre/.wreckit/items/021-core-control-flow-pattern-implementations/research.md` | 1-299 | Pattern architecture, compilation | Design Decisions |
| `/Users/sac/cre/.wreckit/items/011-compiler-from-pattern-terms-to-executable-form/item.json` | 1-27 | Compilation strategy (bytecode vs continuation) | Design Decisions |
| `/Users/sac/cre/docs/43_PATTERNS_COMPLETE.md` | 1-190 | Pattern implementation list | Pattern Mapping Table |

#### Semantics Sources
| File | Lines | Content | Usage |
|------|-------|---------|-------|
| `/Users/sac/cre/.wreckit/items/014-structured-cancellation-semantics/research.md` | 1-598 | Cancellation scope semantics | Semantics Reference |
| `/Users/sac/cre/.wreckit/items/015-multiple-instance-semantics/research.md` | 1-795 | Multiple instance semantics | Semantics Reference |
| `/Users/sac/cre/.wreckit/items/016-effect-system-with-receipts/research.md` | 1-933 | Effect system semantics | Semantics Reference |
| `/Users/sac/cre/src/core/gen_yawl.erl` | 24-194 | gen_yawl behavior, fire/3 semantics | Semantics Reference |
| `/Users/sac/cre/src/core/gen_pnet.erl` | - | Petri net execution engine | Semantics Reference |

#### Pattern Implementation Sources
| File | Lines | Content | Usage |
|------|-------|---------|-------|
| `/Users/sac/cre/src/patterns/*.erl` | 43 modules | All pattern implementations | Pattern Mapping Table |
| `/Users/sac/cre/src/core/yawl_pattern_registry.erl` | - | Pattern ID → module mapping | Pattern Mapping Table |
| `/Users/sac/cre/docs/yawl_patterns/README.md` | 1-81 | Pattern overview | Pattern Mapping Table |

#### Testing and Operations
| File | Lines | Content | Usage |
|------|-------|---------|-------|
| `/Users/sac/cre/.wreckit/items/024-comprehensive-testing-infrastructure/research.md` | 1-100+ | Testing infrastructure analysis | Testing Strategy |
| `/Users/sac/cre/test/yawl_patterns_test.erl` | 1-948 | Pattern test suite | Testing Strategy |
| `/Users/sac/cre/test/yawl_integration_performance_test.erl` | 1-2037 | Performance tests | Testing Strategy |
| `/Users/sac/cre/docs/DEPLOYMENT.md` | - | Deployment documentation | Operational Guidance |
| `/Users/sac/cre/docs/guides/telemetry.md` | - | Monitoring and telemetry | Operational Guidance |

#### API Documentation
| File | Lines | Content | Usage |
|------|-------|---------|-------|
| `/Users/sac/cre/docs/api/core/COMPLETE_API_REFERENCE.md` | - | Complete API reference | Public API docs |
| `/Users/sac/cre/src/api/cre_client.erl` | 1-552 | Client API with -moduledoc | Public API docs |
| `/Users/sac/cre/.wreckit/items/026-public-api-surface/research.md` | 1-284 | Public API analysis | Public API docs |

### Documentation Patterns and Conventions

#### 1. Markdown Structure Pattern

From `docs/ARCHITECTURE.md`:
```markdown
# Title

**Version:** X.Y.Z
**OTP Support:** 25.0, 26, 27, 28
**Last Updated:** YYYY-MM-DD

## Executive Summary
[High-level overview]

## Core Design Philosophy
[Principles and rationale]

## System Architecture
[Diagrams and component descriptions]

## Module Organization
[Directory structure table]

## Key Components
[Detailed component documentation]
```

#### 2. Code Documentation Pattern

From `src/api/cre_client.erl:67-142`:
```erlang
-module(cre_client).
-moduledoc("""
CRE client API for workflow execution.

Provides gen_server-based client for evaluating workflows
with support for async replies and error handling.
""").

-doc("""
Start a CRE client process.

## Parameters

- `Node`: Node to connect to (atom())
- `Name`: Client process name (atom())
- `Options`: Configuration options (proplists:proplist())

## Returns

- `{ok, Pid}`: Client process started successfully
- `{error, Reason}`: Failed to start client

## Example

```erlang
{ok, Pid} = cre_client:start_link(mynode@host, myclient, []).
```
""").
-spec start_link(Node::atom(), Name::atom(), Options::proplists:proplist()) ->
    {ok, pid()} | {error, term()}.
```

#### 3. Research Document Pattern

From `.wreckit/items/014-structured-cancellation-semantics/research.md`:
```markdown
# Research: [Title]

**Date**: YYYY-MM-DD
**Item**: XXX-item-name

## Research Question
[Problem statement]

## Summary
[High-level findings - 2-3 paragraphs]

## Current State Analysis

### Existing Implementation
[What exists with file:line references]

### Key Files
[Table of files with descriptions]

## Technical Considerations

### Dependencies
[Internal and external modules]

### Patterns to Follow
[Conventions observed in codebase]

## Risks and Mitigations
[Table of risks and mitigations]

## Recommended Approach
[High-level strategy]

## Open Questions
[Areas needing clarification]
```

## Technical Considerations

### Dependencies

**Internal Documentation Sources:**
- Research documents in `.wreckit/items/` - Primary source for semantics and design rationale
- Source code with -moduledoc/-doc attributes - API documentation
- Existing docs in `docs/` directory - Architecture, tutorials, guides
- Test files in `test/` - Usage examples and testing patterns

**External Documentation Standards:**
- Erlang/OTP documentation conventions
- Markdown formatting for all prose docs
- EDoc/ExDoc for generated API documentation
- Mermaid diagrams for architectural visualizations

### Patterns to Follow

**1. Research-Based Documentation:**

The `.wreckit/items/*/research.md` files are **exceptionally well-researched** and should be the primary source for:
- Semantics documentation (items 014, 015, 016)
- Design decisions (items 021, 011, 010)
- API surface analysis (item 026)
- Testing strategy (item 024)

**Pattern:** Extract, synthesize, and format research content into user-facing documentation.

**2. Comprehensive File References:**

All documentation should include:
- **Specific file paths** with line numbers: `/Users/sac/cre/src/patterns/sequence.erl:1-67`
- **Code examples** with syntax highlighting
- **API links** to actual module documentation
- **Cross-references** between related documents

**3. Diagram-Based Explanations:**

From `docs/ARCHITECTURE.md` and `docs/mermaid-diagrams/`:
- Use ASCII diagrams for component architecture
- Use Mermaid diagrams for state machines, sequence flows
- Include Petri net diagrams (DOT format) for patterns

**4. Tabular Reference Material:**

From `docs/43_PATTERNS_COMPLETE.md` and `README.md:91-100`:
- Pattern mapping tables (Pattern ID → Name → Module → Status)
- API function tables (Function → Parameters → Returns → Description)
- Configuration option tables (Option → Type → Default → Description)

**5. Example-Driven Documentation:**

From `docs/tutorials/` and source code doctests:
- Provide executable examples for all major features
- Include common use cases and edge cases
- Show complete workflows, not just snippets
- Demonstrate error handling patterns

### Documentation Structure Recommendations

**Target Documentation Structure:**
```
docs/
├── semantics/
│   ├── INDEX.md                    # Semantics documentation hub
│   ├── workflow_execution.md       # Petri net execution semantics
│   ├── cancellation_semantics.md   # Structured cancellation (from item 014)
│   ├── multiple_instances.md       # Multiple instance semantics (from item 015)
│   └── effect_system.md            # Effect system and receipts (from item 016)
├── patterns/
│   ├── PATTERN_MAPPING_TABLE.md    # Complete pattern → implementation map
│   ├── PATTERN_SEMANTICS.md        # Formal semantics for all 43 patterns
│   └── PATTERN_EXAMPLES.md         # Usage examples for common patterns
├── design/
│   ├── DESIGN_DECISIONS.md         # Architectural rationale (from items 010, 011, 021)
│   ├── PETRI_NET_FOUNDATION.md     # Why Petri nets?
│   └── PURE_FUNCTIONAL_DESIGN.md   # Joe Armstrong philosophy
├── testing/
│   ├── TESTING_STRATEGY.md         # Comprehensive testing guide (from item 024)
│   ├── PROPERTY_TESTING.md         # Property-based testing approach
│   └── PERFORMANCE_BENCHMARKS.md   # Benchmark methodology and results
├── operations/
│   ├── OPERATIONAL_GUIDANCE.md     # Production operations guide
│   ├── DEPLOYMENT.md               # Deployment procedures (exists, needs update)
│   ├── MONITORING.md               # Monitoring and alerting (update telemetry.md)
│   └── RUNBOOKS.md                 # Troubleshooting procedures
└── api/
    ├── PUBLIC_API.md               # Unified public API reference (from item 026)
    └── INTERNAL_API.md             # Internal module documentation
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Documentation Drift** - Code changes faster than docs | High | Establish documentation update workflow; require doc updates in PR checklist; automate API doc generation from source |
| **Information Duplication** - Same content in multiple places | Medium | Create single source of truth; use cross-references; implement DRY (Don't Repeat Yourself) for docs |
| **Incomplete Pattern Coverage** - Some patterns lack examples | Medium | Prioritize high-value patterns (sequence, parallel, choice, cancellation); add examples incrementally |
| **Semantic Complexity** - Petri net semantics are challenging | High | Provide multiple explanation levels (overview, deep-dive, reference); use diagrams and examples; link to research papers |
| **Research Document Access** - Research docs may not be publicly visible | Medium | Ensure `.wreckit/items/` is included in documentation build or extract content to public docs |
| **Diagram Maintenance** - Mermaid/DOT diagrams may become outdated | Low | Version control diagrams; use diagram generation tools where possible; document diagram conventions |
| **Testing Documentation Gaps** - Testing infrastructure is incomplete (item 024) | Medium | Coordinate with item 024 implementation; document testing strategy as infrastructure is built |
| **Ops Runbook Incompleteness** - Production operational patterns may be undocumented | Medium | Interview SREs and ops team; capture incident responses; document common failure scenarios |
| **API Fragmentation** - Multiple API entry points confuse users | High | Coordinate with item 026 (Public API Surface); document unified API facade pattern |
| **Version Compatibility** - Docs may not reflect OTP version differences | Low | Document OTP version requirements; note version-specific features; provide compatibility matrix |

## Recommended Approach

### Phase 1: Semantics Reference (Foundation)

**Objective:** Create comprehensive semantics documentation from research documents.

**1. Workflow Execution Semantics**
```markdown
# Workflow Execution Semantics

## Petri Net Foundation
- Formal definition: (P, T, F, M0)
- Token flow rules
- Transition firing semantics
- Reachability and liveness

## gen_pnet Execution Model
- place_lst/0, trsn_lst/0, init_marking/2, preset/1
- is_enabled/3 guard evaluation
- fire/3 token production
- trigger/3 token filtering

## gen_yawl Extension
- 3-tuple fire/3 returns: {produce, Map} | {produce, Map, UsrInfo}
- usr_info state management
- Timeout handling

## Deterministic Scheduling
- ln_sched modes: deterministic, nondeterministic, replay
- Choice types: xor_selection, defer_race, task_selection, join_order
- Replay semantics
```

**Source from:** `docs/ARCHITECTURE.md`, `.wreckit/items/011-compiler-from-pattern-terms-to-executable-form/item.json`

**2. Cancellation Semantics**
```markdown
# Structured Cancellation Semantics

## Scope Types
- activity: Single task cancellation
- region: Cancellation set from YAWL spec
- case: Entire workflow case cancellation

## Cancellation Propagation
- O(scope size) guarantee
- Integration with wf_cancel and wf_scope
- Compensation hook execution

## Semantics
- Cancellation token format: {cancel, [atom()]}
- Scope resolution algorithm
- Compensation ordering (post-order traversal)
```

**Source from:** `.wreckit/items/014-structured-cancellation-semantics/research.md:1-598`

**3. Multiple Instance Semantics**
```markdown
# Multiple Instance Semantics

## Instance Spawning
- Fixed: M instances at design time
- Runtime: M instances determined at startup
- Dynamic: Unbounded instances from data source

## Join Policies
- all: Wait for all M instances
- first_n: Proceed after N complete
- n_of_m: Quorum-based (N out of M)
- discriminator: First completion triggers

## Result Aggregation
- collect_all: Return all results
- collect_quorum: Return first N results
- merge: Combine results with function
- broadcast: Send results to multiple places
- discard: Drop results
```

**Source from:** `.wreckit/items/015-multiple-instance-semantics/research.md:1-795`

**4. Effect System Semantics**
```markdown
# Effect System and Receipts

## Effect Boundary
- Pure workflow reduction → effect yield → handler execution → effect resume
- Effect spec format: #{module => _, function => _, args => _, options => _}

## Unique Causal IDs
- UUID v4 format for global uniqueness
- Causal ID propagation through receipts

## Idempotency
- Idempotency_key for deduplication
- Receipt lookup before execution

## Compensation
- Compensation functors stored in receipts
- Execution on scope cancellation

## Receipt Format
- causal_id, idempotency_key, spec_hash, result, compensation
- Scope indexing for efficient queries
```

**Source from:** `.wreckit/items/016-effect-system-with-receipts/research.md:1-933`

**Estimated effort:** 5-7 days

### Phase 2: Pattern Mapping Table (Reference)

**Objective:** Create comprehensive pattern → implementation mapping.

**1. Pattern Mapping Table**
```markdown
# YAWL Pattern Mapping Table

## Complete Pattern Index

| WCP ID | Pattern Name | Module | File | Lines | Status | Notes |
|--------|--------------|--------|------|-------|--------|-------|
| WCP-01 | Sequence | `sequence` | `/Users/sac/cre/src/patterns/sequence.erl` | 1-67 | ✅ | Basic sequential execution |
| WCP-02 | Parallel Split | `parallel_split` | `/Users/sac/cre/src/patterns/parallel_split.erl` | 1-787 | ✅ | AND-split with branch distribution |
| WCP-03 | Synchronization | `synchronization` | `/Users/sac/cre/src/patterns/synchronization.erl` | - | ✅ | AND-join for parallel branches |
| ... | ... | ... | ... | ... | ... | ... |

## Pattern Categories

### Basic Control Flow (WCP 1-6)
### Advanced Synchronization (WCP 7-10)
### Multiple Instances (WCP 11-17)
### State-Based Patterns (WCP 18-20)
### Extended Control Flow (WCP 21-28)
### Data Flow Patterns (WDP 1-5)
### Resource Patterns (WRP 1-5)
### Exception Handling (WHP 1-5)

## Module Registry

All patterns registered in: `/Users/sac/cre/src/core/yawl_pattern_registry.erl`
```

**Source from:**
- `docs/43_PATTERNS_COMPLETE.md:12-60` (pattern list)
- `src/patterns/*.erl` (43 modules)
- `src/core/yawl_pattern_registry.erl` (registry)

**Estimated effort:** 3-4 days (requires reading all 43 pattern modules)

### Phase 3: Design Decisions Document (Rationale)

**Objective:** Document architectural decisions with rationale.

**1. Why Petri Nets?**
```markdown
# Design Decision: Petri Net Foundation

## Decision
CRE uses Petri nets as its formal foundation for workflow execution.

## Rationale
- **Formal semantics:** Petri nets provide well-defined execution semantics
- **Compositionality:** Nets compose naturally via place/transition fusion
- **Analysis:** Reachability, liveness, boundedness properties are decidable
- **Visual representation:** Direct mapping to workflow diagrams

## Alternatives Considered
- State machines: Less natural for parallel composition
- Process algebra (π-calculus): More complex, less visual
- Direct execution graph: No formal foundation

## Consequences
- ✅ Formal foundation enables verification
- ✅ Visual diagrams match execution model
- ⚠️ Learning curve for users unfamiliar with Petri nets
- ⚠️ Requires compilation from YAWL to Petri net form
```

**Source from:** `.wreckit/items/021-core-control-flow-pattern-implementations/research.md:26-34`, `docs/ARCHITECTURE.md:1-100`

**2. Why Pure Functional Design?**
```markdown
# Design Decision: Pure Functional Helper Modules

## Decision
All modules except gen_pnet/gen_yawl are pure functional (no side effects).

## Rationale (Joe Armstrong Philosophy)
- **One real OTP runner:** Only gen_pnet maintains state
- **Determinism:** Pure functions enable deterministic replay
- **Testability:** Pure functions are easy to test
- **Reasoning:** No hidden state changes

## Pattern
```erlang
%% Pure function - no side effects
-spec apply_cancellation(Marking::marking(), CancelSet::cancellation_set()) -> marking().
apply_cancellation(Marking, CancelSet) ->
    lists:foldl(fun(Place, Acc) ->
        Acc#{Place => []}
    end, Marking, CancelSet).
```

## Alternatives Considered
- gen_server for each module: Too many processes, complex supervision
- Process dictionary: Hidden state, breaks determinism
- ETS for state: Global mutable state, hard to reason about

## Consequences
- ✅ Deterministic execution
- ✅ Easy testing (no process setup)
- ✅ Clear separation of concerns
- ⚠️ Must pass state explicitly through functions
```

**Source from:** `docs/ARCHITECTURE.md:18-30`, `.wreckit/items/021-core-control-flow-pattern-implementations/research.md:276-292`

**3. Compilation Strategy: Bytecode vs Continuation**
```markdown
# Design Decision: Compilation Strategy

## Decision
Compile pattern terms to executable forms without interpretation overhead.

## Options
1. **Bytecode VM (Strategy S1):** Custom opcodes, tight loop execution
2. **Continuation Network (Strategy S2):** Erlang closures, function calls
3. **Petri Net Compilation (Current):** Compile to gen_pnet net structure

## Rationale for Petri Net Compilation (Current Approach)
- Leverages existing gen_pnet execution engine
- Natural fit for YAWL's graphical representation
- Well-tested and stable
- OTP supervision tree integration

## Future Direction: Linear Nesting (ln_*)
New bytecode-based execution system in development:
- `ln_compile.erl`: Plan to bytecode compiler
- `ln_vm.erl`: Bytecode VM executor
- Opcodes: SEQ_ENTER, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_BACK, CANCEL_SCOPE
- Goal: Eliminate Petri net overhead for tighter latency

## Trade-offs
- Petri net: Mature, visual, but some overhead
- Bytecode: Faster, but requires new execution infrastructure
```

**Source from:** `.wreckit/items/011-compiler-from-pattern-terms-to-executable-form/item.json:14-23`

**Estimated effort:** 4-5 days

### Phase 4: Testing Strategy Guide (Validation)

**Objective:** Document comprehensive testing approach.

**1. Testing Strategy Overview**
```markdown
# CRE Testing Strategy

## Testing Pyramid

### Unit Tests (EUnit)
- **Goal:** Test individual functions in isolation
- **Tools:** EUnit (stdlib), meck (mocking)
- **Coverage:** All pure helper modules, gen_yawl callbacks
- **Example:** `test/yawl_patterns_test.erl:1-948`

### Property-Based Tests
- **Goal:** Validate invariants across all inputs
- **Tools:** Custom generator framework (PropEr not in stdlib)
- **Coverage:** Key invariants (cancellation, multiple instances, effect idempotency)
- **Generators:** Workflows, markings, patterns, random inputs

### Bounded Model Checking
- **Goal:** Detect deadlocks, verify liveness, explore state space
- **Tools:** Custom bounded exploration (depth D, token bound K)
- **Coverage:** Petri net properties, cancellation propagation, join policies
- **Integration:** Item 018 (validation backend)

### Determinism/Replay Tests
- **Goal:** Verify deterministic execution and replay correctness
- **Tools:** ln_sched (deterministic scheduler), ln_trace (tracing)
- **Coverage:** Scheduler modes, replay logs, trace validation
- **Example:** `test/wf_deterministic_replay_test.erl:1-240`

### Performance Benchmarks
- **Goal:** Validate performance targets
- **Benchmarks:**
  - Micro: 10k task steps
  - Parallel: 100 branch parallel join
  - Cancellation: Repeated discriminator cancellation
  - Target: Bounded overhead per step, efficient cancellation
- **Tools:** Timer:tc/1, statistics benchmarks
```

**Source from:** `.wreckit/items/024-comprehensive-testing-infrastructure/research.md:26-100`

**2. Test Organization**
```markdown
## Test Organization

### Test Files by Category

**Pattern Tests:**
- `test/yawl_patterns_test.erl` - All 43 patterns
- `test/yawl_patterns_execution_test.erl` - Pattern state transitions

**Integration Tests:**
- `test/yawl_integration_performance_test.erl` - End-to-end workflows
- `test/yawl_engine_test.erl` - Engine integration

**Determinism Tests:**
- `test/wf_deterministic_replay_test.erl` - Replay correctness

**Persistence Tests:**
- `test/yawl_persistence_test.erl` - State persistence
- `test/yawl_recovery_test.erl` - Crash recovery

**Control/Data/Resource Tests:**
- `test/yawl_control_test.erl` - Control flow
- `test/yawl_data_test.erl` - Data flow
- `test/yawl_resource_test.erl` - Resource allocation

### Running Tests

```bash
# Run all tests
rebar3 eunit

# Run specific test suite
rebar3 eunit --module=yawl_patterns_test

# Run with coverage
rebar3 cover
```
```

**Source from:** `.wreckit/items/024-comprehensive-testing-infrastructure/research.md:55-76`

**Estimated effort:** 3-4 days (coordinate with item 024 implementation)

### Phase 5: Operational Guidance (Production)

**Objective:** Create comprehensive operations manual.

**1. Deployment Guide**
```markdown
# Operational Guidance

## Deployment

### Prerequisites
- Erlang/OTP 25-28
- Build tools: rebar3, git
- Dependencies: See rebar.config

### Build
```bash
rebar3 compile
rebar3 release
```

### Configuration
- `config/sys.config`: Application configuration
- `config/vm.args`: VM parameters
- Environment variables for secrets

### Deployment Options
1. **Direct release:** tarball from _build/default/rel/cre/
2. **Docker:** Dockerfile in repository root
3. **Kubernetes:** terraform/gcp/ (see marketplace deployment)
4. **GCP Marketplace:** Package for marketplace distribution
```

**Source from:** `docs/DEPLOYMENT.md`, `terraform/gcp/README.md`, `marketplace/README.md`

**2. Monitoring and Telemetry**
```markdown
## Monitoring

### Metrics Collection
- **Telemetry:** OpenTelemetry integration
- **Metrics:** Workflow case counts, pattern execution, latency
- **Logging:** Structured logging (logger), XES event logging

### Key Metrics
- Active workflow cases
- Pattern execution rates
- Cancellation frequency
- Effect execution latency
- Error rates by pattern

### Dashboards
- Web dashboard for real-time monitoring
- Grafana integration (if using Prometheus)
- Alert thresholds configured in telemetry handler

### Traces
- XES logs for process mining
- OpenTelemetry traces for distributed tracing
- ln_trace event logs for debugging
```

**Source from:** `docs/guides/telemetry.md`, `src/ln_trace.erl:1-138`

**3. Runbooks**
```markdown
## Troubleshooting Runbooks

### Common Issues

#### Workflow Case Stuck in "running" State
**Symptoms:** Case status is "running" but no progress
**Diagnosis:**
1. Check case state: `wf_engine:case_state(CaseId)`
2. Check enabled transitions: `gen_yawl:get_info(CasePid)`
3. Check for deadlock: Run bounded model check
**Resolution:**
- If deadlock: Fix workflow specification
- If waiting for external event: Check effect handler
- If token exhaustion: Verify marking is not empty

#### High Memory Usage
**Symptoms:** Node memory growing, OOM risk
**Diagnosis:**
1. Check case count: `wf_case_sup:case_count()`
2. Check receipts: `ln_effect:get_receipts(State)`
3. Check trace buffer: `ln_trace:get_all(TraceState)`
**Resolution:**
- Archive old case states
- Prune trace buffers (configure max_events)
- Persist receipts to disk

#### Cancellation Not Propagating
**Symptoms:** Cancel workflow but tasks continue executing
**Diagnosis:**
1. Check cancellation tokens: `wf_cancel:extract_all_cancel_sets(Marking)`
2. Check scope mapping: `wf_scope:bindings(BindingTable, ScopeId)`
3. Check compensation hooks: Verify hooks are registered
**Resolution:**
- Verify scope boundaries in workflow spec
- Check trigger/3 callback integration
- Ensure gen_yawl is processing cancellation tokens

#### Performance Degradation
**Symptoms:** Workflow execution slows over time
**Diagnosis:**
1. Check pattern execution: `ln_sched:get_mode(SchedState)`
2. Profile with: `fprof:apply/3`
3. Check for token explosion: Count tokens in marking
**Resolution:**
- Switch to deterministic scheduler if needed
- Optimize pattern implementations
- Enable token GC if available
```

**Estimated effort:** 4-5 days (requires ops input for runbooks)

## Open Questions

1. **Documentation Build System:**
   - **Question:** What tool should build the documentation site?
   - **Options:** ExDoc, Docusaurus, MkDocs, Hugo, static HTML
   - **Impact:** Affects documentation structure, linking, search
   - **Recommendation:** Use ExDoc for API docs (Erlang standard), Docusaurus for prose docs (excellent search, versioning)

2. **Public vs Internal Documentation:**
   - **Question:** Should research documents (`.wreckit/items/`) be public?
   - **Impact:** Research docs contain excellent content but may include internal discussions
   - **Options:**
     - Make all docs public (transparent, open source philosophy)
     - Extract content to public docs, keep research internal
     - Separate public API docs from internal design docs
   - **Recommendation:** Extract semantic/design content to public docs, keep item tracking internal

3. **Documentation Maintenance Workflow:**
   - **Question:** How to ensure docs stay in sync with code?
   - **Impact:** Documentation drift reduces trust
   - **Options:**
     - Require doc updates in PR checklist
     - Automated API doc generation from source (-moduledoc)
     - Periodic doc audits (monthly/quarterly)
     - Doc coverage metrics (like test coverage)
   - **Recommendation:** Automated API doc generation + PR checklist for prose docs

4. **Pattern Semantics Formalism Level:**
   - **Question:** How formal should semantics documentation be?
   - **Impact:** Affects audience accessibility
   - **Options:**
     - Mathematical (Petri net equations, operational semantics)
     - Practitioner-focused (diagrams, examples, pseudocode)
     - Layered (overview → deep dive → formal reference)
   - **Recommendation:** Layered approach - overview for beginners, formal reference for experts

5. **Coordinate with Item 026 (Public API Surface):**
   - **Question:** Should public API docs be created here or in item 026?
   - **Impact:** Duplication risk
   - **Options:**
     - Create unified public API doc here (027)
     - Create in item 026, reference from here
     - Create stub here, full content in 026
   - **Recommendation:** Create unified public API doc in item 026, link from docs/INDEX.md

6. **Coordinate with Item 024 (Testing Infrastructure):**
   - **Question:** Should testing strategy docs wait for item 024 implementation?
   - **Impact:** Docs may describe unimplemented features
   - **Options:**
     - Write testing strategy docs now based on research
     - Wait for item 024 implementation, then document
     - Document current state, mark TODO for item 024 features
   - **Recommendation:** Document current testing capabilities, add "Future" section for item 024 features

7. **Diagrams and Visualizations:**
   - **Question:** What diagram format to use?
   - **Options:** Mermaid (GitHub-compatible), DOT (Petri nets), PlantUML, ASCII
   - **Current state:** Mix of Mermaid and DOT exists
   - **Recommendation:** Standardize on Mermaid for architecture/state machines, DOT for Petri nets

8. **Documentation Localization:**
   - **Question:** Should docs be translated to other languages?
   - **Impact:** Significantly increases maintenance burden
   - **Recommendation:** Start with English only, consider community translations later

9. **Version-Specific Documentation:**
   - **Question:** How to handle multiple CRE versions?
   - **Options:** Single docs for latest release, versioned docs (like HexDocs), Git branches
   - **Recommendation:** Versioned docs using ExDoc/Docusaurus versioning

10. **Performance Benchmarks in Docs:**
    - **Question:** Should specific benchmark numbers be documented?
    - **Impact:** Numbers change with versions, may mislead
    - **Options:** Concrete numbers, relative comparisons, ranges only
    - **Recommendation:** Document methodology and targets, not specific numbers (avoid date-stamped performance claims)

## Next Steps

1. **Clarify documentation build system** (Open Question #1)
2. **Decide public vs internal doc split** (Open Question #2)
3. **Create docs/semantics/INDEX.md** as semantics hub
4. **Extract cancellation semantics** from item 014 research
5. **Extract multiple instance semantics** from item 015 research
6. **Extract effect system semantics** from item 016 research
7. **Create pattern mapping table** by scanning all 43 pattern modules
8. **Write design decisions document** from items 010, 011, 021 research
9. **Coordinate with item 026** for public API documentation
10. **Coordinate with item 024** for testing strategy documentation
11. **Create operational runbooks** by interviewing ops team
12. **Set up documentation build pipeline** (ExDoc/Docusaurus)
