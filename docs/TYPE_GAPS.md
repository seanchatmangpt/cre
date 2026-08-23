# Type Specification Gaps

**Generated**: 2026-02-08
**Project**: CRE (Common Runtime Environment)
**Total Dialyzer Warnings**: 2025
**Source Warnings**: 2023
**Dependency Warnings**: 2

---

## Executive Summary

The CRE codebase has significant type specification gaps identified by Dialyzer analysis. While 1,080 `-spec` declarations exist across 247 modules, there are 541 type specification issues, 299 pattern matching failures, and other type-related warnings.

**Warning Categories**:
- Type specification issues: 541
- Pattern matching failures: 299
- Unknown functions: 244
- Specification mismatches: 206
- Invalid type specifications: 129
- Dead code: 124
- Record construction violations: 60
- Unknown types: 45

---

## Critical Type Issues

### 1. OTP 27+ Supervisor Specs (CRITICAL)

**Impact**: 129 warnings

**Issue**: OTP 27 changed supervisor child spec format from tuples to maps, but type specs still reference old tuple format.

**Example**:
```erlang
%% Current (incorrect for OTP 27+)
-spec init(term()) -> {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.

%% Should be (OTP 27+)
-spec init(term()) -> {ok, #{'intensity' := non_neg_integer(), 'period' := pos_integer(), 'strategy' := supervisor:strategy(), 'children' => [map()]}}.
```

**Affected Modules**:
- `src/active/active_token_sup.erl`
- `src/core/gen_yawl_sup.erl`
- All supervisor modules

**Resolution Effort**: 4 hours (batch update all supervisors)

---

### 2. Record Field Type Mismatches (HIGH)

**Impact**: 60 warnings

**Issue**: Record fields declare broad types (like `atom()`) but use specific compound terms.

**Example**:
```erlang
%% In gen_active_token.erl
-record(token_event, {
    event_type :: atom()  %% Declared as atom()
    %% But actually used as: {communicate, atom()}
}).

%% Should be:
-record(token_event, {
    event_type :: atom() | {atom(), atom()}
}).
```

**Affected Modules**:
- `src/active/gen_active_token.erl` (多处)
- `src/patterns/rl_agent.erl`
- `src/mining/*.erl`

**Resolution Effort**: 3 hours (update record definitions)

---

### 3. Invalid Return Type Specs (HIGH)

**Impact**: 206 warnings

**Issue**: Function specs don't match actual implementation return types.

**Example**:
```erlang
%% Spec says it returns {ok, term()} | {error, term()}
-spec some_function() -> {ok, term()} | {error, term()}.

%% But implementation only returns ok
some_function() ->
    ok.

%% Should be:
-spec some_function() -> ok.
```

**Affected Modules**:
- `src/yawl/yawl_engine.erl`
- `src/patterns/*.erl`
- `src/mining/*.erl`

**Resolution Effort**: 6 hours (audit and fix specs)

---

## Module-Specific Type Issues

### Top 12 Files by Warning Count

| File | Warnings | Primary Issues |
|------|----------|----------------|
| `cre_yawl_http.erl` | 56 | Pattern matching, dead code |
| `cre_yawl_patterns.erl` | 50 | Invalid specs, dead code |
| `yawl_wsif.erl` | 49 | Pattern matching, dead code |
| `yawl_pattern_registry.erl` | 45 | Type specifications |
| `wf_persistence.erl` | 44 | Type specifications, patterns |
| `van_der_aalst_workflow.erl` | 42 | Pattern matching |
| `cre_trace.erl` | 41 | Type specifications |
| `yawl_engine.erl` | 36 | Type specifications |
| `process_discovery.erl` | 36 | Type specifications |
| `cre_profiler.erl` | 35 | Type specifications |
| `rust_nif.erl` | 33 | Unknown functions (fallbacks) |
| `ga_constitution.erl` | 33 | Pattern matching |

---

## Unknown Types (45 warnings)

### Missing Type Definitions

**Issue**: Types referenced in specs but not defined in PLT.

**Examples**:
```erlang
%% Standard OTP types not in PLT
gen_server:option/0
gen_statem:handle_event_result/1

%% Internal types not exported
cre_yawl:task_type/0
cre_yawl:split_type/0

%% Dependency types
ranch:ref/0
ranch_proxy_header:proxy_info/0
```

**Resolution**:
1. Add OTP application to `plt_extra_apps`
2. Export internal types with `-export_type([type/0]).`
3. Add stub type definitions for external dependencies

**Effort**: 2 hours

---

## Unknown Functions (244 warnings)

### External Library Calls

**Issue**: Functions called from libraries not included in PLT.

**Categories**:

1. **gproc library** (50+ warnings)
   ```erlang
   gproc:reg/1
   gproc:set_value/2
   gproc:await/1
   ```

2. **mnesia** (80+ warnings)
   ```erlang
   mnesia:transaction/1
   mnesia:dirty_write/1
   mnesia:table_info/2
   ```

3. **yamerl** (30+ warnings)
   ```erlang
   yamerl_constr:file/1
   yamerl:decode/1
   ```

4. **External processes** (40+ warnings)
   ```erlang
   ordering:*
   carrier_appointment:*
   ```

5. **Erlang fallbacks** (44 warnings)
   ```erlang
   erlang_fallback:*
   ```

**Resolution**: Add applications to `plt_extra_apps` in rebar.config

```erlang
{dialyzer, [
    {plt_extra_apps, [
        lib_combin,
        gen_pnet,
        jsone,
        xmerl,
        gproc,      %% Add
        mnesia,     %% Add
        yamerl      %% Add
    ]}
]}.
```

**Effort**: 1 hour

---

## Pattern Matching Failures (299 warnings)

### Overly Broad Error Patterns

**Issue**: Error handling patterns that can never match.

**Example**:
```erlang
%% Function only returns ok
-spec safe_function() -> ok.
safe_function() -> ok.

%% But code tries to match {error, Reason}
case safe_function() of
    ok -> ok;
    {error, Reason} -> handle_error(Reason)  %% Never matches!
end
```

**Resolution**:
1. Remove unreachable error clauses
2. Update functions to return errors when appropriate
3. Use `-dialyzer({nowarn_function, F/A})` for intentional broad patterns

**Effort**: 4 hours

---

## Dead Code (124 warnings)

### Unused Functions

**Issue**: Functions defined but never called (intentionally or accidentally).

**Categories**:
1. Doctest functions (intentional)
2. WSDL parsing (deprecated)
3. Helper functions (unused)
4. Test utilities in source files

**Resolution**:
1. Move doctest functions to test files
2. Remove deprecated code
3. Add `-compile({nowarn_unused_function, [...]})` for intentional unused

**Effort**: 2 hours

---

## Resolution Plan

### Phase 1: Critical Fixes (Week 1)

| Priority | Issue | Effort | Impact |
|----------|-------|--------|--------|
| CRITICAL | OTP 27+ supervisor specs | 4h | Fixes 129 warnings |
| HIGH | Record field types | 3h | Fixes 60 warnings |
| HIGH | Invalid return specs | 6h | Fixes 206 warnings |

**Total**: 13 hours
**Warnings Resolved**: ~395

### Phase 2: PLT Configuration (Week 2)

| Priority | Issue | Effort | Impact |
|----------|-------|--------|--------|
| MEDIUM | Unknown types | 2h | Fixes 45 warnings |
| MEDIUM | Unknown functions | 1h | Fixes 244 warnings |

**Total**: 3 hours
**Warnings Resolved**: ~289

### Phase 3: Code Cleanup (Week 3)

| Priority | Issue | Effort | Impact |
|----------|-------|--------|--------|
| MEDIUM | Pattern matching | 4h | Fixes 299 warnings |
| LOW | Dead code | 2h | Fixes 124 warnings |

**Total**: 6 hours
**Warnings Resolved**: ~423

---

## Type Specification Best Practices

### 1. Always Export Public Types

```erlang
%% Good
-type my_type() :: #{key => value()}.
-export_type([my_type/0]).

%% Bad (type not accessible)
-type my_type() :: #{key => value()}.
%% No -export_type
```

### 2. Use -opaque for Internal Types

```erlang
%% For types that should be opaque
-opaque opaque_state() :: #{internal => term()}.

%% Exported but users cannot match internals
-export_type([opaque_state/0]).
```

### 3. Always Specify Return Types

```erlang
%% Good
-spec my_function(integer()) -> {ok, integer()} | {error, term()}.

%% Bad (no return spec)
-spec my_function(integer()) -> _.
```

### 4. Use Union Types for Errors

```erlang
%% Good
-spec result() -> {ok, success_type()} | {error, error_type()}.

%% Bad (too broad)
-spec result() -> term().
```

### 5. Document Type Constraints

```erlang
%% Good
-type positive_integer() :: pos_integer().
%% @doc Integer greater than zero

-type non_empty_list(T) :: [T, ...].
%% @doc List with at least one element
```

---

## Dialyzer Configuration

### Current rebar.config

```erlang
{dialyzer, [
    {warnings, [
        unmatched_returns,
        error_handling,
        underspecs
    ]},
    {plt_extra_apps, [
        lib_combin,
        gen_pnet,
        jsone,
        xmerl
    ]},
    {get_warnings, true},
    {include_dirs, ["include"]},
    {exclude_mods, [
        yc_demo,
        '*_test.erl',
        '*_SUITE.erl',
        'strategy_*',
        yawl_pnet_example,
        yawl_payment_branch,
        test_persistence
    ]}
]}.
```

### Recommended Updates

```erlang
{dialyzer, [
    {warnings, [
        unmatched_returns,
        error_handling,
        underspecs,
        no_return
    ]},
    {plt_extra_apps, [
        lib_combin,
        gen_pnet,
        jsone,
        xmerl,
        gproc,
        mnesia,
        yamerl,
        inets,
        crypto,
        asn1,
        public_key,
        ssl,
        compiler
    ]},
    {get_warnings, true},
    {include_dirs, ["include"]},
    {exclude_mods, [
        yc_demo,
        '*_test.erl',
        '*_SUITE.erl',
        'strategy_*',
        yawl_pnet_example,
        yawl_payment_branch,
        test_persistence
    ]},
    {plt_location, local},  %% Use local PLT
    {plt_prefix, "cre"}     %% Custom PLT name
]}.
```

---

## Type Checking Workflow

### Before Committing

```bash
# 1. Compile
rebar3 compile

# 2. Run Dialyzer
rebar3 dialyzer

# 3. Check specific module
rebar3 dialyzer -m path/to/module.erl

# 4. Build PLT if needed
rebar3 dialyzer --build_plt
```

### Continuous Integration

```yaml
dialyzer_job:
  script:
    - rebar3 dialyzer --failure_ignored
  artifacts:
    paths:
      - _build/default/dialyzer_warnings
  allow_failure: false
```

---

## Success Metrics

### Target Warning Reduction

| Category | Current | Target | Reduction |
|----------|---------|--------|-----------|
| Type specs | 541 | <50 | 91% |
| Pattern match | 299 | <30 | 90% |
| Unknown types | 45 | 0 | 100% |
| Unknown functions | 244 | <20 | 92% |
| Invalid specs | 129 | <10 | 92% |
| Dead code | 124 | <20 | 84% |
| **TOTAL** | **2023** | **<200** | **90%** |

### Module Compliance Target

- **Critical modules**: 0 warnings
- **High-priority modules**: <5 warnings
- **All modules**: <10 warnings

---

## Acceptable Warnings

Some warnings are acceptable and should be documented:

1. **External dependencies**: Functions from libraries not in PLT
2. **Dynamic calls**: Function calls using `apply/3`
3. **Intentional broad types**: Types deliberately kept flexible
4. **Test utilities**: Helper functions for testing

These should be annotated with `-dialyzer({nowarn_function, ...})` or similar.

---

**Last Updated**: 2026-02-08
**Type Owner**: Development Team
**Next Review**: Weekly during type fix phase
