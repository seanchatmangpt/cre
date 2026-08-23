# Item 023: Multiple Instance Pattern Implementations - Summary

## Status: Planning Complete

### Overview

This item implements a comprehensive multiple instance (MI) pattern framework for the CRE workflow substrate. The current codebase has fragmented MI support across multiple modules with significant gaps in synchronization variants, dynamic spawning, and unified semantics.

## Key Findings from Research

### Current State

1. **Partial implementations exist:**
   - `wf_mi.erl` - Pure utilities (✅ working, keep)
   - `n_out_of_m.erl` - Production-ready WCP-22 quorum pattern (✅ reference model)
   - `multiple_instances_sync.erl` - WCP-12 but hardcoded for 4 instances (⚠️ needs generalization)
   - `blocking_discriminator.erl` - WCP-09 discriminator but standalone (⚠️ needs integration)
   - `static_partial_join_mi.erl`, `dynamic_partial_join_mi.erl` - Basic implementations (⚠️ incomplete)

2. **Pattern algebra already implemented:**
   - `wf_term:mi/2` constructor exists (wf_term.erl:226-229)
   - `wf_compile` handles MI patterns (wf_compile.erl:182-187)
   - MI opcodes defined (wf_vm.erl:28-29, 102-103)

3. **Critical gaps:**
   - No unified API facade
   - Missing gen_yawl patterns for "all", "first_n", "discriminator" policies
   - Tests reference non-existent `cre_yawl_patterns` module
   - Cancellation integration incomplete

## Implementation Strategy

### Hybrid Approach: Gen_Yawl Patterns + Bytecode Migration

**Phase 1 (Item 023):** Complete gen_yawl patterns for immediate use
- Create `mi_all_pattern.erl` (generalized "wait for all M")
- Create `mi_first_n_pattern.erl` (proceed after N complete)
- Create `mi_discriminator_pattern.erl` (first wins, cancel rest)
- Create `mi_pattern.erl` unified facade
- Fix test suite
- Verify bytecode execution (wf_exec)
- Integrate cancellation

**Phase 2 (future):** Migrate to bytecode execution as Items 010/011/012 mature
- Bytecode VM may need additional work
- Performance optimization
- Deprecation plan for old patterns

## User Stories (Priority Order)

| Priority | Story | Title |
|----------|-------|-------|
| 1 | US-001 | Create mi_all_pattern gen_yawl module |
| 1 | US-002 | Create mi_first_n_pattern gen_yawl module |
| 1 | US-004 | Create mi_pattern unified facade module |
| 2 | US-003 | Create mi_discriminator_pattern gen_yawl module |
| 2 | US-005 | Fix test suite to use mi_pattern facade |
| 3 | US-006 | Verify and complete bytecode execution (wf_exec) |
| 3 | US-007 | Integrate cancellation support (Item 014) |
| 4 | US-008 | Create documentation and examples |
| 4 | US-009 | Performance benchmark and optimization |

## Success Criteria (From Item Definition)

- ✅ **Fixed MI spawns exact count:** mi_all_pattern supports {fixed, M}
- ✅ **Dynamic MI spawns based on runtime data:** mi_pattern facade supports {dynamic, DataFun, Max}
- ✅ **All sync variants work correctly:** all, first_n, n_of_m, discriminator
- ✅ **Join policies enforced:** validate_join_policy/2 in facade

## Key Decisions

1. **Follow n_out_of_m.erl model:** All new patterns use this as reference
2. **Separate modules + facade:** Each pattern is separate gen_yawl module, unified API via mi_pattern
3. **Keep wf_mi utilities:** Pure functional utilities are working correctly
4. **Hybrid gen_yawl + bytecode:** Maintain backward compatibility, migration path to bytecode
5. **Integration with Item 014:** Add cancellation support via trigger/3 callback

## Dependencies

- **Internal:** gen_yawl behavior, wf_mi utilities, wf_term (Items 010), wf_compile (Item 011), wf_exec (Item 012), wf_cancel (Item 014)
- **External:** lib_combin (deterministic scheduling), yawl_xes (process mining logging)

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Fragmented API confuses users | High | Create unified mi_pattern facade |
| Dynamic spawning unboundedness | High | Add max_instances limit, backpressure |
| Test suite references non-existent module | Medium | Fix tests to use mi_pattern facade |
| Cancellation race conditions | Medium | Use trigger/3 for atomic token filtering |
| Performance of Petri net interpretation | Medium | Benchmark, plan bytecode migration |

## Files Created

1. `/Users/sac/cre/.wreckit/items/023-multiple-instance-pattern-implementations/plan.md`
   - Detailed implementation plan with 5 phases
   - Testing strategy
   - Migration notes

2. `/Users/sac/cre/.wreckit/items/023-multiple-instance-pattern-implementations/SUMMARY.md`
   - This file

3. PRD saved via MCP tool with 9 user stories

## Next Steps

1. **Start implementation with US-001** (mi_all_pattern)
2. Follow reference implementation: n_out_of_m.erl
3. Implement gen_yawl callbacks following model
4. Add comprehensive tests and doctests
5. Proceed through phases in order

## Open Questions (All Resolved)

1. ✅ **Architecture:** Both gen_yawl patterns (now) + bytecode (future) - hybrid approach
2. ✅ **Pattern granularity:** Separate modules + unified facade
3. ✅ **Dynamic spawning:** DataFun returns {more, Data} | done, with max_instances limit
4. ✅ **Test suite fix:** Create mi_pattern facade, update tests
5. ✅ **Cancellation timing:** Completion wins, filter via trigger/3
6. ✅ **Items 010/011 status:** Mostly complete, wf_term and wf_compile already handle MI

## References

- Research: `/Users/sac/cre/.wreckit/items/023-multiple-instance-pattern-implementations/research.md`
- Plan: `/Users/sac/cre/.wreckit/items/023-multiple-instance-pattern-implementations/plan.md`
- Model implementation: `/Users/sac/cre/src/patterns/n_out_of_m.erl`
- Pattern algebra: `/Users/sac/cre/src/wf/wf_term.erl:218-229`
- Compiler: `/Users/sac/cre/src/wf/wf_compile.erl:182-187`
- IDEAS.md: `/Users/sac/cre/IDEAS.md:107-167`

---

**Prepared by:** Claude (Planning Phase)
**Date:** 2025-01-11
**Branch:** wreckit/023-multiple-instance-pattern-implementations
