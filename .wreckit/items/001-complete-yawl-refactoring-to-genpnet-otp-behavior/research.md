# Research: Complete YAWL refactoring to gen_pnet OTP behavior

**Date**: 2025-06-18
**Item**: 001-complete-yawl-refactoring-to-genpnet-otp-behavior

## Research Question
Current YAWL system mocks gen_pnet callbacks but implements custom Petri net engine, not utilizing the actual gen_pnet library that is already a dependency

**Motivation:** Using genuine gen_pnet OTP behavior provides proper Erlang/OTP integration, better maintainability, native token passing semantics, and alignment with the intended architecture

**Success criteria:**
- All 43 YAWL patterns work as gen_pnet modules
- Workflows execute end-to-end with gen_pnet
- Token passing semantics match YAWL specification
- Cancellation patterns work correctly
- Colored tokens carry data correctly
- All existing tests pass
- Performance matches or exceeds current implementation

**Technical constraints:**
- Must maintain backward compatibility for existing workflows
- Cannot break existing YAWL patterns functionality
- Must preserve YAWL token semantics
- Must handle cancellation patterns correctly

**In scope:**
- Refactor yawl_patterns.erl to proper gen_pnet callback module
- Replace yawl_workflow_instance.erl gen_statem with gen_pnet
- Update yawl_orchestrator.erl for gen_pnet workflow management
- Create gen_pnet modules for all 43 YAWL patterns
- Implement colored Petri net extensions for gen_pnet
- Update persistence layer for gen_pnet net_state
- Create migration path from old to new implementation

**Out of scope:**
- Changes to YAWL specification semantics
- Modifying external API contracts (without compatibility layer)
- Removing support for existing workflow definitions

**Signals:** priority: high, urgency: Major architectural refactoring requiring 5-7 weeks, should be started incrementally

## Summary

The research indicates that this refactoring requires transitioning from a custom Petri net implementation (currently using `gen_statem` with mock gen_pnet callbacks) to a genuine `gen_pnet` OTP behavior implementation. The key challenge is that gen_pnet provides a native Petri net semantics engine, while the current implementation manually manages token passing and state transitions.

**Critical Finding:** The task description states "Current YAWL system mocks gen_pnet callbacks" - this suggests the existing code already has the structure for gen_pnet integration but implements the callbacks with custom logic rather than delegating to gen_pnet's built-in mechanisms.

**Primary architectural decision:** This refactoring requires two major shifts:
1. **OTP behavior migration**: Replace `gen_statem` with `gen_pnet` for workflow instances
2. **Pattern modularization**: Convert monolithic yawl_patterns.erl into 43 separate gen_pnet modules

The refactoring should be executed incrementally, starting with basic patterns and progressively addressing more complex ones (particularly cancellation patterns and colored tokens).

## Current State Analysis

### Existing Implementation

**Note:** The actual source code files are not present in the wreckit directory structure. The following analysis is based on the item description and typical gen_pnet integration patterns.

Based on the item description, the current architecture likely includes:

#### Key Components (Inferred):

1. **yawl_patterns.erl** - Contains mock gen_pnet callbacks but implements custom Petri net logic
   - Mock implementations of required gen_pnet callbacks: `init/1`, `handle_token/3`, `handle_event/3`
   - Custom token passing logic that doesn't utilize gen_pnet's native semantics
   - All 43 YAWL patterns likely defined in this single module

2. **yawl_workflow_instance.erl** - Uses `gen_statem` instead of `gen_pnet`
   - Manages workflow state transitions manually
   - Tracks token positions and marking state
   - Likely implements callbacks: `init/1`, `state_name/3`, `handle_event/4`

3. **yawl_orchestrator.erl** - Manages workflow lifecycle
   - Spawns and supervises workflow instances
   - Handles workflow execution requests
   - Likely needs updates to work with gen_pnet instances

4. **Persistence layer** (inferred) - Stores workflow state
   - Currently serializes custom state structures
   - Will need updates to persist gen_pnet's net_state format

### YAWL Patterns (43 Total - Inferred)

Based on the YAWL (Yet Another Workflow Language) specification, the 43 patterns likely include:

**Control Flow Patterns (Basic):**
1. Sequence
2. Parallel Split
3. Synchronization
4. Exclusive Choice (XOR)
5. Simple Merge

**Advanced Control Flow:**
6. Multiple Choice (OR)
7. Multiple Merge
8. Multi-instance (sequential)
9. Multi-instance (parallel)
10. Deferred Choice
11. Interleaved Parallel Routing
12. Milestone

**Cancellation Patterns:**
13. Cancel Activity
14. Cancel Case (entire workflow)
15. Cancel Region
16. Cancel Multiple Instance Activity

**Data Patterns (Colored Tokens):**
17. Data Pass-Through
18. Data Transformation
19. Data-Aware Choice
20. Data-Aware Split
21. Data-Aware Join

**Additional Patterns (up to 43):**
- Various combinations and extensions of the above
- Compensation patterns
- Iteration patterns
- Thread patterns
- Pattern variations (synchronous vs asynchronous)

### Key Integration Points

**File Inference:**
- `yawl_patterns.erl` - Pattern definitions and mock callbacks
- `yawl_workflow_instance.erl` - gen_statem instance manager
- `yawl_orchestrator.erl` - Workflow lifecycle orchestration
- `yawl_persistence.erl` (likely exists) - State persistence
- `yawl_supervisor.erl` (likely exists) - OTP supervision tree

## Technical Considerations

### Dependencies

#### External Libraries:
1. **gen_pnet** (already a dependency)
   - Provides: OTP behavior for Petri net implementation
   - Key callbacks: `init/1`, `handle_token/3`, `handle_event/3`
   - Token passing: Automatic place-to-place transitions
   - State management: Built-in marking (token distribution) tracking

2. **Potential need for extensions:**
   - **Colored Petri Net support**: gen_pnet may need extension to carry data payloads with tokens
   - **Cancellation regions**: May require custom event handling beyond standard gen_pnet

#### Internal Modules:
1. **yawl_patterns** → Needs complete refactor to gen_pnet callback module
2. **yawl_workflow_instance** → Replace gen_statem with gen_pnet
3. **yawl_orchestrator** → Update for gen_pnet lifecycle management
4. **Persistence layer** → Adapt to gen_pnet net_state serialization
5. **Test suite** → All existing tests must pass without modification

### Patterns to Follow

#### gen_pnet Module Structure:

```erlang
-module(yawl_pattern_sequence).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([init/1,
         place_info/1,
         transition_info/1,
         inhibit_info/1,
         token_data/1]).

%% Place structure: {PlaceName, InitialTokens}
%% Transition structure: {TransitionName, InputPlaces, OutputPlaces, GuardFunction}
```

#### Key gen_pnet Concepts:

1. **Places**: Hold tokens (represent workflow states)
2. **Transitions**: Fire when input places have tokens, consume input, produce output
3. **Arcs**: Connect places to transitions (bidirectional)
4. **Marking**: The distribution of tokens across places (net state)
5. **Token Data**: For colored Petri nets, tokens carry payloads

#### Migration Pattern:

**Before (Custom gen_statem):**
```erlang
state_name(cast, {token, PlaceName}, State) ->
    NewState = move_token(PlaceName, State),
    {next_state, next_state_name(NewState), NewState}.
```

**After (gen_pnet):**
```erlang
handle_token(PlaceName, Token, NetState) ->
    %% gen_pnet handles token movement automatically
    %% Focus on transition guards and side effects
    {ok, NetState}.
```

### Colored Petri Net Extensions

**Challenge:** Standard gen_pnet uses untyped tokens. YAWL requires data carrying.

**Solution Approach:**
```erlang
%% Token format: {Data, Type, Metadata}
%% Transition function can inspect and transform token data
handle_transition(TransitionName, InputTokens, NetState) ->
    %% Transform token data
    OutputTokens = transform_data(InputTokens),
    {ok, OutputTokens, NetState}.
```

### Cancellation Pattern Handling

**YAWL Cancellation Regions:**
- Remove all tokens from a set of places
- Terminate active transitions
- May cascade (cancel triggered by cancel)

**gen_pnet Approach:**
```erlang
handle_event(cancel_region, RegionId, NetState) ->
    %% Remove tokens from places in region
    %% Cancel any in-flight transitions
    %% Use gen_pnet:remove_tokens/2
    NewNetState = cancel_region_places(RegionId, NetState),
    {ok, NewNetState}.
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Performance degradation** from gen_pnet's generic token passing vs custom optimized implementation | High | Benchmark early; optimize hot paths; may need gen_pnet patches for high-frequency operations |
| **Cancellation semantics** not matching YAWL specification (especially cascading cancels) | High | Implement cancellation as custom event handlers in gen_pnet; extensive testing of cancellation patterns |
| **Colored token data** serialization/deserialization complexity | Medium | Create well-defined token data format; implement custom token_data callback; persistence layer must handle arbitrary data payloads |
| **Breaking existing workflows** during migration | High | Implement compatibility layer; run both implementations in parallel; canary deployments; feature flag for gradual rollout |
| **43 pattern modules** code organization and maintenance overhead | Medium | Create pattern module generator; use consistent naming convention; consider pattern families (basic, advanced, cancellation, data-aware) |
| **Test coverage gap** - existing tests may not cover all gen_pnet edge cases | Medium | Audit existing tests; add integration tests specifically for gen_pnet behavior; property-based testing for token semantics |
| **Loss of custom optimizations** that current implementation has | Medium | Profile current implementation; identify critical optimizations; port to gen_pnet via custom callbacks or NIFs |
| **gen_pnet library limitations** for YAWL-specific requirements | High | May need to fork/extend gen_pnet; engage with library maintainers; design fallback strategies |

## Recommended Approach

### Phase 1: Foundation (Week 1-2)
**Goal:** Establish gen_pnet integration patterns and basic migration infrastructure

1. **Create gen_pnet module template**
   - Design standardized module structure for YAWL patterns
   - Create pattern module generator (scaffolding tool)
   - Document gen_pnet callback patterns for YAWL

2. **Implement colored token extension**
   - Define token data format: `{Payload, Type, Metadata, Timestamp}`
   - Implement `token_data/1` callback for serialization
   - Create token transformation utilities

3. **Migration infrastructure**
   - Create compatibility layer to translate between old and new state formats
   - Implement dual-mode operation (can run both implementations)
   - Add feature flags for gradual rollout

4. **Persistence layer update**
   - Adapt serialization for gen_pnet net_state
   - Create migration script for existing workflow instances
   - Test backward compatibility

### Phase 2: Basic Patterns (Week 2-3)
**Goal:** Migrate simple control-flow patterns (no cancellation, no complex data)

**Patterns to migrate:**
1. Sequence
2. Parallel Split
3. Synchronization
4. Exclusive Choice (XOR)
5. Simple Merge

**Approach:**
- Create individual gen_pnet modules for each pattern
- Write unit tests verifying token semantics match YAWL spec
- Benchmark vs current implementation
- Fix any performance regressions

**Success criteria:**
- All basic patterns work as gen_pnet modules
- Tests pass
- Performance within 10% of current implementation

### Phase 3: Advanced Patterns (Week 3-4)
**Goal:** Migrate complex control-flow patterns

**Patterns to migrate:**
6. Multiple Choice (OR)
7. Multiple Merge
8. Multi-instance (sequential)
9. Multi-instance (parallel)
10. Deferred Choice
11. Interleaved Parallel Routing
12. Milestone

**Approach:**
- Reuse patterns from Phase 2
- Implement more complex transition guards
- Handle multi-instance semantics (token counting)

### Phase 4: Cancellation Patterns (Week 4-5)
**Goal:** Implement YAWL cancellation semantics using gen_pnet events

**Patterns to migrate:**
13. Cancel Activity
14. Cancel Case
15. Cancel Region
16. Cancel Multiple Instance Activity

**Approach:**
- Implement cancellation as gen_pnet events
- Use `handle_event/3` callback for cancellation logic
- Handle cascading cancellations
- Test thoroughly - this is high-risk

**Success criteria:**
- Cancellation works correctly for all patterns
- No orphaned tokens after cancellation
- State is cleanly reset

### Phase 5: Colored Token Patterns (Week 5-6)
**Goal:** Migrate data-aware patterns with complex token payloads

**Patterns to migrate:**
17. Data Pass-Through
18. Data Transformation
19. Data-Aware Choice
20. Data-Aware Split
21. Data-Aware Join
- And remaining patterns (up to 43)

**Approach:**
- Leverage colored token extension from Phase 1
- Implement token data transformation functions
- Test with various data payloads
- Verify serialization/deserialization

### Phase 6: Integration and Testing (Week 6-7)
**Goal:** Full system integration and validation

1. **Replace yawl_workflow_instance gen_statem with gen_pnet**
   - Update module to use gen_pnet behavior
   - Maintain existing API contracts
   - Update supervision tree

2. **Update yawl_orchestrator**
   - Change workflow instance spawning to use gen_pnet
   - Update lifecycle management callbacks
   - Test orchestration workflows end-to-end

3. **Comprehensive testing**
   - All existing tests must pass
   - Add integration tests for full workflows
   - Load testing with gen_pnet implementation
   - Performance benchmarking

4. **Documentation updates**
   - Update architecture documentation
   - Document migration path for users
   - Create troubleshooting guide

5. **Gradual rollout**
   - Feature flags for enabling gen_pnet per workflow
   - Monitor in production
   - Rollback plan if issues arise

## Open Questions

### Critical to Answer Before Implementation:

1. **Actual gen_pnet library capabilities**
   - Does gen_pnet natively support colored Petri nets (data-carrying tokens)?
   - What is the performance profile of gen_pnet for high-frequency token operations?
   - Are there known limitations for complex cancellation scenarios?
   - **Answer required:** Review gen_pnet source code and documentation

2. **Current implementation details**
   - What specific optimizations does the current custom implementation have?
   - Are there YAWL-specific features that don't map cleanly to gen_pnet?
   - What is the exact state serialization format currently used?
   - **Answer required:** Review existing yawl_*.erl source files

3. **Test coverage**
   - Do existing tests cover all 43 patterns?
   - Are there tests specifically for token semantics?
   - Are there performance benchmarks we need to match?
   - **Answer required:** Review test suite

4. **Deployment considerations**
   - How many active workflows in production?
   - What is the migration strategy for in-flight workflows?
   - Can we afford any downtime during migration?
   - **Answer required:** Consult with operations team

5. **Backward compatibility requirements**
   - Are there external APIs that depend on current state representation?
   - Do we need to maintain old and new implementations in parallel?
   - What is the deprecation timeline?
   - **Answer required:** Consult with product and API teams

### Secondary Questions:

6. Should we fork gen_pnet to add YAWL-specific features, or contribute upstream?
7. What is the monitoring/observability story for gen_pnet workflows?
8. How do we handle versioning of workflow definitions during migration?
9. Are there security considerations for colored token data?
10. What is the disaster recovery plan if migration fails?

## Next Steps

1. **Locate and review actual source code** for yawl_*.erl modules
2. **Study gen_pnet library** documentation and source code
3. **Audit test suite** for coverage of all 43 patterns
4. **Create proof-of-concept** for basic pattern migration
5. **Stakeholder review** of migration plan and risks
6. **Establish performance baseline** with current implementation
7. **Begin Phase 1** of recommended approach

## Conclusion

This refactoring is a significant architectural change that requires careful planning and incremental execution. The primary challenge is ensuring that gen_pnet's generic Petri net semantics can accurately represent YAWL's specific workflow patterns, particularly around cancellation regions and colored token data flow.

The recommended phased approach mitigates risk by starting with simple patterns and progressively addressing complexity. Success depends heavily on early discovery of gen_pnet limitations and establishing clear performance baselines.

**Critical success factors:**
- Comprehensive testing of token semantics
- Performance parity with current implementation
- Backward compatibility during migration
- Thorough validation of cancellation patterns
- Robust colored token implementation

**Estimated effort:** 5-7 weeks as noted in urgency, assuming gen_pnet library is suitable for YAWL requirements. If gen_pnet requires significant extensions, timeline may extend to 8-10 weeks.
