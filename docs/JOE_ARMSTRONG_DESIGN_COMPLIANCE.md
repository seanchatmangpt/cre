# Joe Armstrong Design Compliance

**CRE Version:** 0.3.0  
**Last Updated:** 2026-02-06

## Design Principle

> **"One real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts."**

## Architecture Compliance

### ✅ Single OTP Runtime Component

**Only OTP processes maintaining state:**

- `gen_pnet` - Core Petri net runtime (OTP gen_server)
- `gen_yawl` - Wrapper around gen_pnet (OTP gen_server)
- Pattern modules (`src/patterns/*.erl`) - Implement `gen_yawl` behavior

**State Management:**
- State lives ONLY in `gen_pnet`/`gen_yawl` processes
- State changes flow through token production/consumption
- No global state, no ETS tables (except for application-level concerns)

### ✅ Pure Helper Modules (Stateless)

All modules in these directories are pure functional utilities:

#### Core Pure Helpers
- `yawl_pattern_registry.erl` - Pattern ID to module name mapping (pure lookups)
- `yawl_pattern_expander.erl` - Pattern instance to Petri net expansion (pure transformations)
- `yawl_compile.erl` - YAWL spec to Erlang code generation (pure code generation)

#### Petri Net Pure Utilities (`src/pnet/`)
- `pnet_types.erl` - Type definitions and validation
- `pnet_marking.erl` - Multiset marking algebra
- `pnet_mode.erl` - Mode enumeration utilities
- `pnet_choice.erl` - Deterministic nondeterminism
- `pnet_receipt.erl` - Receipt tracking and effects

#### Workflow Pure Utilities (`src/wf/`)
- `wf_timerq.erl` - Deadline queue for token injections
- `wf_task.erl` - External task token constructors
- `wf_scope.erl` - Boundary mapping helpers
- `wf_yaml_spec.erl` - YAML specification parsing (pure parsing)
- All other `wf_*` modules - Pure functional utilities

#### YAWL Pure Utilities (`src/yawl/`)
- `yawl_validate.erl` - YAWL specification validation
- All other `yawl_*` modules that are not OTP processes

### ✅ Pattern Modules (Runtime Components)

Pattern modules in `src/patterns/` implement the `gen_yawl` behavior:

- Each pattern = one `gen_yawl` module
- All 43 patterns implemented as `gen_yawl` behaviors
- Patterns maintain state through `gen_yawl`/`gen_pnet` runtime
- No direct state in pattern modules themselves

**Pattern List (All 43):**
- P1: sequence
- P2: parallel_split
- P3: synchronization
- P4: exclusive_choice
- P5: simple_merge
- P6: multiple_choice
- P7: structured_sync_merge
- P8: multiple_merge
- P9: discriminator
- P10: arbitrary_cycles
- P11: implicit_termination
- P12-P15: multiple_instances_sync (variants)
- P16: deferred_choice
- P17: interleaved_routing
- P18: milestone
- P19: cancel_activity
- P20: cancel_case
- P21: structured_loop
- P22: recursion
- P23: transient_trigger
- P24: persistent_trigger
- P25: cancel_region
- P26: cancel_mi_activity
- P27: complete_mi_activity
- P28: blocking_discriminator
- P29: cancelling_discriminator
- P30: structured_partial_join
- P31: blocking_partial_join
- P32: cancelling_partial_join
- P33: generalized_and_join
- P34: static_partial_join_mi
- P35: cancelling_partial_join_mi
- P36: dynamic_partial_join_mi
- P37: local_sync_merge
- P38: general_sync_merge
- P39: critical_section
- P40: interleaved_routing
- P41: thread_merge
- P42: thread_split
- P43: explicit_termination

## Message Contracts

All communication follows message contracts:

- `gen_yawl:inject/2` - Inject tokens into places
- `gen_yawl:step/1` - Execute one step of the Petri net
- `gen_yawl:drain/2` - Drain the net until completion or limit
- `gen_yawl:marking/1` - Get current marking
- `gen_yawl:usr_info/1` - Get user-defined state

## Verification

### Compile-Time Verification
```bash
# All patterns compile as gen_yawl behaviors
rebar3 compile

# Verify no stateful modules outside runtime
grep -r "gen_server\|gen_fsm\|gen_statem" src/pnet/ src/wf/ src/core/yawl_*.erl
# Should return empty (except for gen_yawl itself)
```

### Runtime Verification
- Only `gen_yawl` processes maintain state
- All helper modules are pure functions
- No side effects in helper modules
- State changes only through token production/consumption

## Compliance Checklist

- [x] Single OTP runtime (`gen_pnet` wrapped by `gen_yawl`)
- [x] All pattern modules implement `gen_yawl` behavior
- [x] Pattern registry is pure stateless
- [x] Pattern expander is pure stateless
- [x] Compiler is pure stateless
- [x] YAML parser is pure stateless
- [x] All helper modules documented as pure
- [x] No state in helper modules
- [x] Message contracts defined
- [x] All 43 patterns implemented

## Design Benefits

1. **Determinism**: Pure functions are deterministic and testable
2. **Composability**: Pure functions compose easily
3. **Testability**: No mocking needed for pure functions
4. **Parallelization**: Pure functions can run in parallel safely
5. **Reasoning**: Easier to reason about pure functions
6. **State Isolation**: State only in OTP processes, clearly bounded

## References

- Joe Armstrong: "One real OTP runner, everything else pure helpers"
- CRE Architecture: `docs/ARCHITECTURE.md`
- Pattern Implementation: `docs/patterns/PATTERN_IMPLEMENTATION_GUIDE.md`
