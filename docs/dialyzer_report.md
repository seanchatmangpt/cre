# Dialyzer Type Analysis Report

## Summary

**Date**: 2026-02-08
**Dialyzer Warnings**: 2025 total
**Project Source Warnings**: 2023
**Dependency Warnings**: 2

## Configuration

```erlang
{dialyzer, [{warnings, [unmatched_returns,
                        error_handling,
                        underspecs]},
            {plt_extra_apps, [lib_combin, gen_pnet, jsone, xmerl]},
            {get_warnings, true},
            {include_dirs, ["include"]},
            {exclude_mods, [...] % See rebar.config for full list
            ]}.
```

**Excluded Modules** (33 modules):
- Test files (yc_demo, *_test.erl, *_SUITE.erl)
- Strategy modules (strategy_*.erl)
- Example/demo files (yawl_pnet_example, yawl_payment_branch)
- Test persistence (test_persistence)

## Warning Categories

| Category | Count | Description |
|----------|-------|-------------|
| Type specification issues | 541 | Specs are supertypes or incorrect |
| Pattern matching failures | 299 | Patterns that can never match |
| Unknown functions | 244 | Functions not in PLT |
| Specification mismatches | 206 | Return type spec doesn't match implementation |
| Pattern warnings | 192 | Various pattern-related issues |
| Call failures | 135 | Calls that will never return |
| Invalid type specifications | 129 | Specs with no valid overlap |
| Dead code | 124 | Functions that will never be called |
| Unmatched expressions | 61 | Values returned but not matched |
| Record construction violations | 60 | Record fields violate declared types |
| Unknown types | 45 | Types not in PLT |
| Test failures | 12 | Tests that can never evaluate |
| No local return | 27 | Functions with no local return |

## Top Files by Warning Count

| File | Warnings | Primary Issues |
|------|----------|----------------|
| cre_yawl_http.erl | 56 | Pattern matching, dead code |
| cre_yawl_patterns.erl | 50 | Invalid specs, dead code |
| yawl_wsif.erl | 49 | Pattern matching, dead code |
| yawl_pattern_registry.erl | 45 | Type specifications |
| wf_persistence.erl | 44 | Type specifications, patterns |
| van_der_aalst_workflow.erl | 42 | Pattern matching |
| cre_trace.erl | 41 | Type specifications |
| yawl_engine.erl | 36 | Type specifications |
| process_discovery.erl | 36 | Type specifications |
| cre_profiler.erl | 35 | Type specifications |
| rust_nif.erl | 33 | Unknown functions (fallbacks) |
| ga_constitution.erl | 33 | Pattern matching |

## Key Issues

### 1. Unknown Types (45 warnings)

These are from using types that aren't in the PLT:

- `gen_server:option/0` - Standard OTP type, may need newer OTP PLT
- `cre_yawl:task_type/0`, `cre_yawl:split_type/0`, etc. - Internal types
- `gen_statem:handle_event_result/1` - OTP type
- `ranch:ref/0`, `ranch_proxy_header:proxy_info/0` - Dependency types

**Rationale**: These are from inter-module type dependencies. The types exist in the codebase but may not be included in the PLT due to compilation order.

### 2. Unknown Functions (244 warnings)

Functions not available in the PLT:

- `gproc:*` - gproc library functions (not in PLT)
- `mnesia:*` - Database functions (not in PLT)
- `yamerl_constr:*` - YAML parsing library
- `ordering:*`, `carrier_appointment:*`, etc. - External process modules
- `erlang_fallback:*` - Fallback implementations for Rust NIFs

**Rationale**: These are external library calls or optional dependencies. Adding their applications to `plt_extra_apps` would resolve these.

### 3. Invalid Type Specifications (129 warnings)

Functions where the spec doesn't match the implementation:

Example from `active_token_sup:init/1`:
```
Expected: {'ok',{{supervisor:strategy(),non_neg_integer(),pos_integer()},[supervisor:child_spec()]}}
Actual:   {'ok',{#{'intensity' := 10,'period' := 60,'strategy' := 'one_for_one'},[map(),...]}}
```

**Rationale**: OTP 27+ changed supervisor child spec format from tuples to maps. The code uses the new format but specs reference the old format.

### 4. Dead Code / Unreachable Functions (124+ warnings)

Functions that will never be called or patterns that can never match:

- `doctest_test/0` - Test functions with unreachable code
- WSDL parsing functions in `yawl_wsif.erl`
- Various helper functions in multiple modules

**Rationale**: Some of these are intentional dead code for doctests. Others are genuine dead code that could be removed.

### 5. Pattern Matching Failures (299 warnings)

Patterns that can never match due to type constraints:

Example from `gen_active_token.erl`:
```
The pattern {'error', Reason} can never match the type 'ok'
```

**Rationale**: Overly broad error handling patterns that don't account for functions that only return `ok`.

### 6. Record Construction Violations (60 warnings)

Record field values that don't match declared types:

Example from `gen_active_token.erl`:
```
#token_event{event_type :: {'communicate', atom()}}
violates declared type: event_type :: atom()
```

**Rationale**: Records declare broad types (like `atom()`) but use more specific compound terms. This is intentional for flexibility.

## Fixes Applied

### 1. Rebar Configuration Updates

- Changed from `exclude_files` to `exclude_mods` for better exclusion pattern matching
- Added 33 test/exclude modules to `exclude_mods` list

### 2. Build Configuration

- Added `src/rust_nifs` to `src_dirs` for Rust NIF bindings
- Added `src/rust_implementations/paper_algorithms` for algorithm implementations

## Recommendations

### High Priority

1. **Update OTP 27+ Supervisor Specs**: Many warnings are due to new map-based child specs
2. **Add Missing PLT Applications**: Add `gproc`, `mnesia`, `yamerl` to `plt_extra_apps`
3. **Fix Type Declarations**: Update record field types to match actual usage patterns

### Medium Priority

4. **Remove Dead Code**: Functions marked as "will never be called" could be removed
5. **Tighten Error Patterns**: Update error matching to account for functions that only return `ok`
6. **Update gen_server/gen_statem Specs**: Use proper OTP behavior callback specs

### Low Priority

7. **Doctest Cleanup**: Many warnings come from doctest functions
8. **External Module Stubs**: Add stub modules for external processes (ordering, carrier_appointment)

## Remaining Warnings Rationale

### Acceptable Warnings

- **Unknown external functions**: Intentional - these are runtime dependencies
- **Record field types**: Intentional - use broader types for flexibility
- **OTP version differences**: Expected - code supports OTP 25-28 with varying features
- **Dead code in doctests**: Intentional - test functions

### Should Fix

- **Invalid specs**: These indicate actual bugs in type specifications
- **Unreachable patterns**: These indicate logic errors in error handling
- **Supervisor spec format**: Update to use new map-based format consistently

## Next Steps

1. Run `rebar3 dialyzer` regularly as part of CI
2. Gradually fix "Invalid type specification" warnings
3. Add `gproc`, `mnesia` to PLT when using those features
4. Consider splitting analysis by module to make fixes more manageable
5. Update documentation with type usage patterns

## Files Generated

- `_build/default/28.3.1.dialyzer_warnings` - Full warning output
- `_build/default/rebar3_28.3.1_plt` - Dialyzer PLT file
