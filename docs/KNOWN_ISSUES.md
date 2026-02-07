# CRE - Known Issues and Limitations

**Version:** 0.2.0 / 2.1.0
**Last Updated:** 2026-02-06
**Status:** Active Tracking

---

## Overview

This document tracks all known issues, limitations, and unimplemented features in the CRE (Common Runtime Environment) YAWL workflow engine. Issues are categorized by severity and include workarounds where available.

---

## Severity Definitions

| Level | Description | SLA |
|-------|-------------|-----|
| **Critical** | Production-blocking, data loss, security vulnerability | Immediate |
| **High** | Major feature broken, no workaround available | 1 week |
| **Medium** | Feature affected, has workaround available | 2 weeks |
| **Low** | Cosmetic, edge cases, tooling issues | Future release |

---

## Critical Issues

*No critical issues at this time.*

---

## High Severity Issues

### H-001: Integration Test Failures (27 failures)

**Location:** `test/yawl_integration_test.erl`
**Affected:** Workflow boundary validation, end-to-end execution
**Since:** v0.2.0

**Description:**
The `yawl_integration_test` module has 27 failing tests related to workflow boundary validation. Tests fail because `cre_yawl:get_tasks/1` is not properly implemented.

**Error Pattern:**
```erlang
{error, {badmatch, {workflow, ...}}}
in function yawl_integration_test:set_workflow_boundaries/3
```

**Impact:**
- Integration tests cannot validate end-to-end workflow execution
- Workflow boundary validation does not work

**Workaround:**
Use unit tests for individual pattern validation. Skip integration tests for now:

```bash
rebar3 eunit --skip_integration=true
```

**Planned Fix:**
- Implement `cre_yawl:get_tasks/1` - Extract tasks from workflow record (2 hours)
- Implement `cre_yawl:set_workflow_boundaries/3` - Validate workflow boundaries (2 hours)
- Fix workflow parsing logic (4 hours)

**Assigned:** Unassigned
**Target Release:** v0.3.0

---

## Medium Severity Issues

### M-001: Test Isolation - bcrypt already_started

**Location:** `test/yawl_approval_test.erl`
**Affected:** Auth/approval tests (5 tests cancelled)
**Since:** v0.2.0

**Description:**
When running full test suites, bcrypt application may already be started from previous tests, causing `already_started` errors.

**Error:**
```erlang
{error, {badmatch, {error, {already_started, <0.1102.0>}}}}
```

**Impact:**
- 5 approval tests cancelled when test suite runs in certain orders
- Requires serial test execution in some scenarios

**Workaround:**
Run tests serially or restart Erlang node between test runs:

```bash
# Run tests individually
rebar3 eunit --module=yawl_auth_test
rebar3 eunit --module=yawl_approval_test
```

**Planned Fix:**
Add proper application dependency handling in test setup.

**Assigned:** Unassigned
**Target Release:** v0.3.0

---

### M-002: Recovery Test Failures

**Location:** `test/yawl_recovery_test.erl`
**Affected:** Checkpoint and recovery tests
**Since:** v0.2.0

**Description:**
Recovery tests show failures related to concurrent checkpoint handling and atom creation.

**Errors:**
```erlang
% Test 24: list_to_existing_atom failure
{error, badarg, [{erlang, list_to_existing_atom, ["p6"]}]}

% Test 25: Concurrent checkpoint count mismatch
expected: 10
got: 1
```

**Impact:**
- Recovery workflow validation incomplete
- Large data checkpoint tests fail

**Workaround:**
Skip recovery tests during normal development:

```bash
rebar3 eunit --skip=recovery
```

**Planned Fix:**
Review concurrent checkpoint logic and atom handling.

**Assigned:** Unassigned
**Target Release:** v0.3.0

---

### M-003: Mnesia Schema already_exists

**Location:** `test/yawl_integration_test.erl`, `yawl_persistence.erl`
**Affected:** Integration tests, persistence tests
**Since:** v0.2.0

**Description:**
Mnesia schema creation fails when schema already exists from previous test runs or node restart.

**Error:**
```erlang
{aborted, {already_exists, TableName}}
```

**Impact:**
- Tests must handle schema existence
- Requires manual schema cleanup in some cases

**Workaround:**
Code already handles this safely:

```erlang
{error, {already_exists, _}} -> ok
```

**Planned Fix:**
Add proper Mnesia cleanup in test teardown.

**Assigned:** Unassigned
**Target Release:** v0.3.0

---

## Low Severity Issues

### L-001: Unused Record Warnings

**Location:** `src/cre_yawl_patterns.erl`
**Affected:** Compilation output
**Since:** v0.2.0

**Description:**
Three unused record definitions trigger compiler warnings.

**Warnings:**
```erlang
src/cre_yawl_patterns.erl:62: Warning: record 'trsn' is unused
src/cre_yawl_patterns.erl:63: Warning: record 'mult_ari' is unused
src/cre_yawl_patterns.erl:64: Warning: record 'comp' is unused
```

**Impact:**
- Cosmetic only
- Does not affect functionality
- Compilation succeeds

**Workaround:**
None needed - these are reserved for future pattern implementations.

**Planned Fix:**
These will be used when remaining patterns are implemented.

**Assigned:** Unassigned
**Target Release:** When patterns implemented

---

### L-002: Unused Function Warning

**Location:** `src/cre_yawl_patterns.erl`
**Affected:** Compilation output
**Since:** v0.2.0

**Description:**
The `doctest_test/0` function is exported but unused.

**Workaround:**
None needed - test helper function.

**Planned Fix:**
Remove when doctest framework is integrated.

---

### L-003: Cover Tool no_abstract_code Warnings

**Location:** Coverage reports
**Affected:** `rebar3 cover` output
**Since:** v0.2.0

**Description:**
Multiple beam files show `{no_abstract_code}` warnings during coverage analysis.

**Cause:**
Precompiled beam files lack debug information needed for coverage analysis.

**Impact:**
- Coverage reports show 0% for some modules
- Does not affect functionality
- Affects accuracy of coverage metrics

**Workaround:**
Use Dialyzer for static analysis instead of coverage for those modules:

```bash
rebar3 dialyzer
```

**Planned Fix:**
Known Rebar3/Erlang toolchain limitation - no fix planned.

---

### L-004: NATO Module Unused Variables

**Location:** `src/nato/nato_assert.erl`, `src/nato/nato_conf.erl`
**Affected:** NATO approval demo modules
**Since:** v0.2.0

**Warnings:**
```erlang
src/nato/nato_assert.erl:62: Warning: variable 'PostureSets' is unused
src/nato/nato_conf.erl:162: Warning: variable 'Deps' is unused
```

**Impact:**
- Cosmetic only
- Demo code, not production

**Workaround:**
None needed.

**Planned Fix:**
Prefix with underscore in future cleanup.

---

### L-005: YAWL SMS Binary Optimization Warnings

**Location:** `src/yawl_sms.erl:684-703`
**Affected:** SMS module compilation
**Since:** v0.2.0

**Description:**
Compiler emits binary construction warnings for SMS parsing.

**Warnings:**
```erlang
Warning: NOT OPTIMIZED: binary is used in remote call to binary:part/3
Warning: BINARY CREATED: binary is used in call to binary:part/3
```

**Impact:**
- Performance only
- SMS parsing still works correctly
- May affect high-throughput SMS scenarios

**Workaround:**
None needed for normal usage.

**Planned Fix:**
Refactor binary parsing in future optimization pass.

---

### L-006: ETS Transfer Unexpected Message

**Location:** Executor statistics collection
**Affected:** Runtime logs
**Since:** v0.2.0

**Description:**
ETS transfer operations occasionally log unexpected message patterns.

**Impact:**
- Logging noise only
- Does not affect executor functionality
- Non-blocking

**Workaround:**
None needed.

**Planned Fix:**
Clean up message handling in executor stats module.

---

## Unimplemented YAWL Patterns (7 Remaining)

**Status:** 36 of 43 patterns implemented (83.7%)

### Missing by Category

| Pattern ID | Pattern Name | Category | Priority |
|------------|--------------|----------|----------|
| WCP-08 | Structured Synchronization | Advanced Sync | Medium |
| WCP-12 | Partial Join | Advanced Sync | Medium |
| WCP-14 | Multiple Instances No Sync | Multiple Instances | Low |
| WCP-15 | Runtime Knowledge | Multiple Instances | Medium |
| WCP-17 | Multiple Instances Without Prior Knowledge | Multiple Instances | High |
| WCP-19 | Cancel Activity | State-Based | High |
| WCP-20 | Cancel Case | State-Based | High |

**Note:** Some documentation references 17 missing patterns. After audit, only 7 patterns remain unimplemented. The discrepancy is due to:
- Some patterns being split into multiple modules
- Some patterns covered by existing implementations
- Documentation counting planned vs. implemented

**Planned Implementation:**

| Pattern | Effort | Dependencies |
|---------|--------|--------------|
| Structured Synchronization | 2 hours | None |
| Partial Join | 2 hours | None |
| Multiple Instances No Sync | 3 hours | WCP-13 |
| Runtime Knowledge | 4 hours | WCP-13, WCP-14 |
| Cancel Activity | 3 hours | Exception handling |
| Cancel Case | 3 hours | Exception handling |
| Without Prior Knowledge | 6 hours | WCP-13, WCP-14, WCP-15 |

**Total Estimated Effort:** 23 hours

---

## Test Status Summary

| Metric | Count | Percentage |
|--------|-------|------------|
| Total Tests | 1,208 | 100% |
| Passing | 1,178 | 97.5% |
| Failing | 25 | 2.1% |
| Cancelled | 5 | 0.4% |

### Passing Test Modules

| Module | Tests | Coverage |
|--------|-------|----------|
| `yawl_resourcing_test` | 54 | Resource allocation, participants |
| `yawl_scheduling_test` | 48 | Time-based allocation, deadlines |
| `yawl_worklet_test` | 48 | Worklet execution, completion API |
| `yawl_wsif_test` | 59 | SOAP/WSDL services, JSON |
| `yawl_control_test` | 47 | Case lifecycle, transitions |
| `yawl_interface_d_test` | 36 | Exception handling, worklets |
| `yawl_ipc_test` | 26 | Pub/sub messaging, broadcast |

### Failing Test Summary

| Test Suite | Failures | Primary Issue |
|------------|----------|---------------|
| `yawl_integration_test` | ~27 | Missing `get_tasks/1` |
| `yawl_recovery_test` | ~3 | Concurrent checkpoints, atom creation |
| `yawl_approval_test` | ~5 | bcrypt already_started |

---

## Compiler Warnings Summary

**Total Non-Blocking Warnings:** 6

| File | Warning | Type |
|------|---------|------|
| `cre_yawl_patterns.erl` | 3 unused records | Cosmetic |
| `cre_yawl_patterns.erl` | 1 unused function | Test helper |
| `nato_assert.erl` | 1 unused variable | Demo code |
| `nato_conf.erl` | 1 unused variable | Demo code |

**Note:** These are intentionally left for future use or are demo-only code.

---

## Documentation Limitations

### D-001: Outdated Pattern Count References

Some documentation references "17 remaining patterns" which is outdated. Only 7 patterns remain unimplemented.

**Affected Files:**
- `docs/FINAL_MERGE_SUMMARY.md`
- `docs/ARCHITECTURE.md`

**Status:** Will be updated when patterns are completed.

---

## OTP Compatibility Notes

### OTP 28 Support
**Status:** Fully Supported
**Notes:**
- All dependencies updated to git sources to resolve Hex conflicts
- gen_pnet callback compatibility fixed
- Cowboy/cowlib/ranch pinned to 2.16.0

### OTP 25-27 Support
**Status:** Fully Supported
**Notes:** No known issues.

### OTP < 25
**Status:** Dropped
**Notes:** OTP 19-24 no longer supported.

---

## Workarounds Reference

### For Integration Test Failures

```bash
# Run specific test modules only
rebar3 eunit --module=yawl_control_test
rebar3 eunit --module=yawl_resourcing_test

# Or skip integration tests
grep -r "integration" rebar.config
```

### For Recovery Test Failures

```bash
# Skip recovery tests during development
rebar3 eunit --skip=recovery
```

### For Auth/Test Isolation Issues

```bash
# Run tests individually
rebar3 eunit --module=yawl_auth_test
# Then
rebar3 eunit --module=yawl_approval_test
```

### For Coverage Warnings

```bash
# Use Dialyzer for static analysis instead
rebar3 dialyzer
```

---

## Future Fixes Planned

### v0.3.0 Roadmap

1. **Complete 7 remaining YAWL patterns** (23 hours)
2. **Fix integration test infrastructure** (8 hours)
3. **Recovery module improvements** (6 hours)
4. **Test isolation improvements** (4 hours)

### v0.4.0 Roadmap

1. Performance optimization pass
2. Binary parsing cleanup (SMS module)
3. Documentation updates
4. OTP 29 compatibility testing

---

## Contributing

When fixing issues:

1. Update this document when issues are resolved
2. Move resolved issues to `docs/RESOLVED_ISSUES.md`
3. Include issue ID in commit messages (e.g., "Fix M-001: ...")
4. Update test counts after fixes

---

## Changelog

| Date | Changes |
|------|---------|
| 2026-02-06 | Initial creation from FINAL_MERGE_SUMMARY.md data |
| 2026-02-06 | Added current test results (1208 total, 25 failures) |
| 2026-02-06 | Corrected unimplemented pattern count (7, not 17) |

---

**Document Owner:** CRE Development Team
**Next Review:** After v0.3.0 release
