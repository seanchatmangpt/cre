# Build System and CI Integration Implementation Plan

## Implementation Plan Title
Professional CI/CD Pipeline with Unified Build Scripts and Developer Tooling

## Overview
Consolidate and complete CRE's build system and CI/CD integration. The project has comprehensive build configuration and CI workflows, but lacks unified developer scripts, has misconfigured style checking (efmt plugin not installed), and missing CI test execution consistency. This plan creates a complete, professional development workflow with proper CI/CD automation, unified scripts, and working style enforcement.

**Current Status:**
- ✅ rebar3 configuration is comprehensive and production-ready
- ✅ GitHub Actions workflows exist (ci.yml, ci-cd.yml, release.yml)
- ❌ efmt plugin referenced in CI but not installed in rebar.config
- ❌ No unified ci/ directory with build/test/lint scripts
- ❌ CI workflows have inconsistencies (different OTP versions, missing doctests)
- ❌ No pre-commit hooks or local developer automation
- ❌ Style checking fails in CI (efmt not configured)

**Desired End State:**
- ✅ All rebar3 commands work: compile, eunit, ct, dialyzer, xref, edoc
- ✅ Unified ci/ scripts for build, test, lint, coverage
- ✅ efmt plugin properly configured and working
- ✅ Consistent GitHub Actions workflows across OTP 25-28
- ✅ Pre-commit hooks for local quality enforcement
- ✅ All 760+ tests passing (EUnit + Common Test + Doctests)
- ✅ CI/CD badge in README actually reflects working pipeline

## Current State

### What Exists

#### 1. Build Configuration (✅ Complete)

**File:** `/Users/sac/cre/rebar.config:1-125`

Comprehensive rebar3 configuration:
- **19 source directories** configured (src/core, src/pnet, src/wf, src/yawl, src/patterns, etc.)
- **Rust NIF pre-hooks** for native compilation
- **9 external dependencies** from Git (gen_pnet, cowboy, jsx, jsone, jiffy, yamerl, etc.)
- **Test profiles** (test, concuerror, debug) with proper dependencies
- **Dialyzer configuration** with warnings and 34 excluded modules
- **Escript configuration** for standalone executable

**Verified working:**
```bash
rebar3 compile  # ✅ Compiles 248 Erlang modules
rebar3 eunit    # ✅ Runs 98 EUnit test modules
rebar3 ct       # ✅ Runs 9 Common Test suites
rebar3 dialyzer # ✅ Type checking with PLT
rebar3 xref     # ✅ Cross-reference analysis
rebar3 edoc     # ✅ Documentation generation
```

#### 2. Test Infrastructure (✅ Comprehensive but Fragmented)

**Test Files:** 106 test files in `/Users/sac/cre/test/`
- 98 EUnit modules (`*_tests.erl`)
- 9 Common Test suites (`*_SUITE.erl`)
- 123 modules with doctests (`doctest_test()` functions)

**Test Scripts:**
- `/Users/sac/cre/scripts/run_eunit.sh:1-10` - EUnit with yawl_persistence.beam workaround
- `/Users/sac/cre/scripts/run_doctests.sh:1-34` - Auto-discovers and runs doctests
- **Missing:** No script for Common Test, no unified test runner

**Test Count:**
- 760 total tests (689 passing, 71 failing per README)
- 96% pass rate (needs investigation of failing tests)

#### 3. CI/CD Workflows (⚠️ Exist but Have Issues)

**File:** `/Users/sac/cre/.github/workflows/ci-cd.yml:1-520`

Comprehensive 7-stage pipeline:
1. **Build** - OTP 26.3, 27.2, 28.0 matrix
2. **Test** - EUnit with coverage, uploads to Codecov
3. **Integration Test** - Common Test (continue-on-error: true)
4. **Security & Quality** - Dialyzer, XREF, efmt, security audit
5. **Docker** - Build/push to GHCR, SBOM, Trivy scan
6. **Deploy Staging** - Kubernetes deployment
7. **Deploy Production** - Manual approval, rollback on failure

**File:** `/Users/sac/cre/.github/workflows/ci.yml:1-30`

Simpler CI workflow (OTP 25, 26, 27):
```yaml
- run: rebar3 compile
- run: rebar3 xref
- run: rebar3 efmt -c      # ❌ FAILS - plugin not installed
- run: rebar3 eunit
- run: rebar3 dialyzer
- run: rebar3 edoc
```

**Issues:**
1. **efmt not configured** - `{project_plugins, []}.` in rebar.config:48
2. **Missing doctests** - Neither CI workflow runs doctests
3. **Inconsistent OTP versions** - ci.yml uses 25/26/27, ci-cd.yml uses 26/27/28
4. **CT marked optional** - `continue-on-error: true` on Common Test
5. **No ci/ scripts** - Workflows call rebar3 directly instead of unified scripts

#### 4. Static Analysis (⚠️ Partial)

**Dialyzer:** ✅ Configured in `/Users/sac/cre/rebar.config:82-124`
- Warnings: unmatched_returns, error_handling, underspecs
- PLT extra apps: lib_combin, gen_pnet, jsone, xmerl
- 34 excluded modules (tests + known problematic modules)

**XREF:** ✅ Available but not in CI-cd.yml (only in ci.yml)

**EFmt:** ❌ Referenced but not installed
- CI calls `rebar3 efmt -c` (check formatting)
- But `{project_plugins, []}.` - no plugins configured
- Need to add `{project_plugins, [{efmt, "0.12.0"}]}.`

**Elvis:** ❌ Not configured (alternative style checker)

#### 5. Documentation (✅ Complete)

**File:** `/Users/sac/cre/docs/development/build_system.md:1-754`

Comprehensive build system documentation covering:
- System requirements
- Quick reference commands
- Project structure (19 source directories explained)
- Compilation, testing, static analysis
- Development workflow recommendations
- Troubleshooting guide

**Status:** Documentation is thorough and accurate.

### What's Missing

#### 1. Unified CI Scripts (❌ Critical)

**Expected:**
```bash
ci/build.sh    # Unified build script
ci/test.sh     # Unified test runner (EUnit + CT + Doctests)
ci/lint.sh     # Unified style checking (Dialyzer + XREF + efmt)
ci/coverage.sh # Coverage report generation
ci/all.sh      # Run all checks
```

**Actual:** No `ci/` directory exists. Workflows call rebar3 directly.

#### 2. EFmt Plugin Configuration (❌ Critical)

**Current rebar.config:48:**
```erlang
{project_plugins, []}.  % Disabled to prevent git deps being overridden
```

**Problem:** CI workflows call `rebar3 efmt -c` but plugin isn't installed.

**Solution needed:**
```erlang
{project_plugins, [
    {efmt, "0.12.0"}  % Erlang code formatter
]}.
```

#### 3. Pre-commit Hooks (❌ Missing)

No local developer automation:
- No pre-commit validation
- No commit message linting
- No automatic formatting on save
- Developers must remember to run tests manually

#### 4. CI Workflow Consistency (⚠️ Issues)

**ci.yml vs ci-cd.yml:**
- Different OTP versions (25/26/27 vs 26/27/28)
- Different test execution (ct.yml skips doctests)
- Different style checks (ci.yml has efmt, ci-cd.yml has more)
- No unified approach

#### 5. Test Execution Completeness (⚠️ Missing Doctests)

Neither CI workflow runs doctests:
```bash
# Missing from CI:
./scripts/run_doctests.sh  # 123 modules with doctests
```

### Key Discoveries

1. **Critical Finding:** CI badge in README points to non-existent workflow
   - README.md:4: `[![CI/CD](https://github.com/joergen7/cre/workflows/CI%2FCD%20Pipeline/badge.svg)]`
   - File exists (`.github/workflows/ci-cd.yml`) but references wrong name
   - Badge URL expects `ci-cd.yml`, actual workflow name is "CI/CD Pipeline" ✅
   - **Issue:** Workflow exists but has failures (efmt not configured)

2. **Style Check Failure:** `rebar3 efmt -c` called in CI but plugin not installed
   - rebar.config:48 has `{project_plugins, []}.`
   - CI workflows fail at style check stage
   - Need to configure efmt plugin or remove from CI

3. **Fragmented Testing:** Three separate test commands not unified
   - EUnit: `rebar3 eunit` or `./scripts/run_eunit.sh`
   - Doctests: `./scripts/run_doctests.sh`
   - Common Test: `rebar3 ct`
   - No single script runs all tests

4. **Rust NIF Build Risk:** CI assumes Rust toolchain available
   - rebar.config:31-34 pre-hooks build Rust NIFs
   - Graceful failure: `|| echo 'Rust NIF build skipped'`
   - CI may fail if Rust not installed or NIFs required

5. **Dialyzer Exclusions:** 34 modules excluded from type checking
   - Includes test modules (reasonable)
   - Includes source modules (hides bugs): `strategy_*`, `wf_test_net_*`, etc.

6. **Test Failure Investigation Needed:** 71 of 760 tests failing (96% pass rate)
   - Which tests fail? (EUnit, Common Test, or Doctests?)
   - Are failures expected (TODO markers) or bugs?
   - Need to document known test failures

## What We're NOT Doing

This plan is intentionally scoped to address the immediate CI/CD and build system issues. The following are **explicitly out of scope**:

### Out of Scope

1. **Test Repairs** - Not fixing failing tests (71 of 760)
   - This is a separate item: investigate and fix test failures
   - We will ensure tests RUN in CI, not fix failures

2. **Dialyzer Warnings** - Not resolving type checking issues
   - Currently 34 excluded modules
   - Not adding new modules to exclusion list
   - Not fixing existing Dialyzer warnings

3. **Docker/Kubernetes** - Not modifying deployment infrastructure
   - Dockerfile.production exists and works
   - K8s deployment scripts exist
   - CI builds and pushes images (this is fine)

4. **Performance Optimization** - Not optimizing CI execution time
   - Current CI takes 15-20 minutes
   - Not adding caching strategies or parallelization
   - Not optimizing test execution

5. **Alternative Style Checkers** - Not evaluating Elvis vs efmt
   - efmt is referenced in CI, we'll make it work
   - Not switching to Elvis or other formatters

6. **Documentation Generation** - Not automating doc publishing
   - `rebar3 edoc` works but docs not published
   - Not adding doc deployment to CI

7. **Local Development Environment** - Not creating devcontainer updates
   - `.devcontainer/` exists
   - Not modifying container configuration

8. **Release Automation** - Not changing versioning or release process
   - `release.yml` workflow exists for tagged releases
   - Not modifying release automation

### Rationale

This plan focuses on **completing the existing CI/CD foundation** rather than building new features. The workflows exist but have configuration issues (efmt plugin), missing pieces (doctests in CI), and lack developer convenience scripts (ci/ directory). Once this foundation is solid, future work can optimize performance, fix test failures, and improve code quality.

## Implementation Approach

### Strategy

**Incremental Fixes, No Breaking Changes**

1. **Fix critical CI failures first** - Configure efmt plugin
2. **Add missing coverage** - Run doctests in CI
3. **Create developer convenience** - Unified ci/ scripts
4. **Add local automation** - Pre-commit hooks
5. **Standardize workflows** - Align OTP versions, make CI consistent

### Decision Points

#### Q1: EFmt vs Elvis for Style Checking?

**Decision:** Use efmt (already referenced in CI)

**Rationale:**
- CI workflows already call `rebar3 efmt -c`
- efmt is formatter (fixes code) vs Elvis is linter (reports issues)
- Formatters are more actionable than linters
- No need to change CI workflows, just configure plugin

#### Q2: CI Directory vs Enhancing Existing Scripts?

**Decision:** Create `ci/` directory with unified scripts

**Rationale:**
- Existing scripts in `scripts/` are for demos and development
- `ci/` directory is standard convention for CI automation
- Separates concerns: `scripts/` for dev tools, `ci/` for automation
- CI workflows can call `./ci/all.sh` instead of multiple rebar3 commands

#### Q3: Fix Failing Tests or Document Them?

**Decision:** Document known failures, don't fix (out of scope)

**Rationale:**
- Fixing 71 test failures is separate work
- Some failures may be expected (TODO markers, skip markers)
- Focus is on getting tests to RUN in CI, not pass
- Document known failures in `.github/KNOWN_TEST_FAILURES.md`

#### Q4: Include OTP 25 in CI Matrix?

**Decision:** Test OTP 26, 27, 28 only (drop OTP 25)

**Rationale:**
- OTP 25 reaches end-of-life in 2026
- Project targets OTP 28 as primary (per docs)
- ci-cd.yml already uses 26/27/28
- Align ci.yml with ci-cd.yml for consistency

#### Q5: Fail CI on Common Test Errors?

**Decision:** No, keep `continue-on-error: true` for now

**Rationale:**
- Common Test suite may have expected failures
- Investigating CT failures is separate work
- EUnit is the primary test gatekeeper (no continue-on-error)
- Can make CT fail-fast in future once failures are understood

---

## Phases

### Phase 1: Configure EFmt Plugin (Critical - Unblocks CI)

#### Overview
Fix the critical CI failure by installing the efmt plugin that CI workflows reference. Currently CI fails at the style checking stage because `rebar3 efmt -c` is called but the plugin isn't installed.

#### Changes Required:

##### 1. Add EFmt to rebar.config
**File:** `/Users/sac/cre/rebar.config:48`
**Current:**
```erlang
{project_plugins, []}.  % Disable hex plugin to prevent git deps being overridden
```

**New:**
```erlang
{project_plugins, [
    {efmt, "0.12.0"}    % Erlang code formatter
]}.  % Enable efmt for style checking
```

**Rationale:**
- CI workflows call `rebar3 efmt -c` (check formatting)
- Version 0.12.0 is stable and well-tested
- Comment updated to reflect actual usage
- Plugin only for efmt, not hex (prevents git deps override)

##### 2. Verify EFmt Works Locally
**Action:** After updating rebar.config, verify:
```bash
# Install plugin
rebar3 plugins install efmt

# Check formatting (should not error)
rebar3 efmt -c

# Format all files (if needed)
rebar3 efmt -w
```

**Note:** If efmt reports formatting issues, run `rebar3 efmt -w` to auto-format before committing.

#### Success Criteria:

##### Automated Verification:
- [ ] Plugin installs: `rebar3 plugins list | grep efmt`
- [ ] Style check passes: `rebar3 efmt -c` exits with code 0
- [ ] CI workflows pass style check stage

##### Manual Verification:
- [ ] Checkout branch and run `rebar3 efmt -c` locally
- [ ] Verify CI workflow passes "Check formatting with EFmt" step
- [ ] Check that CI badge in README shows green (passing)

**Note:** This is a prerequisite for all other phases. Do not proceed until efmt works.

---

### Phase 2: Create Unified CI Scripts (High Priority)

#### Overview
Create a `ci/` directory with unified build, test, and lint scripts. These scripts consolidate existing fragmented commands into standard automation that can be run locally or in CI.

#### Changes Required:

##### 1. Create ci/build.sh
**File:** `/Users/sac/cre/ci/build.sh` (new file)
**Purpose:** Unified build script for compilation

```bash
#!/usr/bin/env bash
# Build script for CRE project
# Compiles all Erlang modules and Rust NIFs
set -e

echo "=== Building CRE ==="

# Clean previous build
echo "Cleaning previous build artifacts..."
rebar3 clean

# Compile with default profile
echo "Compiling source code (248 modules)..."
rebar3 compile

# Verify compilation succeeded
if [ ! -d "_build/default/lib/cre/ebin" ]; then
    echo "❌ Build failed: ebin directory not created"
    exit 1
fi

MODULE_COUNT=$(ls _build/default/lib/cre/ebin/*.beam 2>/dev/null | wc -l)
echo "✅ Build successful: ${MODULE_COUNT} modules compiled"
```

**Make executable:** `chmod +x ci/build.sh`

##### 2. Create ci/test.sh
**File:** `/Users/sac/cre/ci/test.sh` (new file)
**Purpose:** Unified test runner (EUnit + Common Test + Doctests)

```bash
#!/usr/bin/env bash
# Test script for CRE project
# Runs all test suites: EUnit, Common Test, and Doctests
set -e

echo "=== Running CRE Test Suite ==="
FAILED_TESTS=0

# EUnit tests
echo ""
echo "1/3 Running EUnit tests (98 modules)..."
./scripts/run_eunit.sh || FAILED_TESTS=$((FAILED_TESTS + 1))

# Doctests
echo ""
echo "2/3 Running doctests (123 modules)..."
./scripts/run_doctests.sh || FAILED_TESTS=$((FAILED_TESTS + 1))

# Common Test
echo ""
echo "3/3 Running Common Test suites (9 suites)..."
rebar3 ct || echo "⚠️  Common Test had failures (marked as optional)"

# Summary
echo ""
if [ $FAILED_TESTS -eq 0 ]; then
    echo "✅ All critical tests passed"
else
    echo "⚠️  ${FAILED_TESTS} test category failed"
    exit 1
fi
```

**Make executable:** `chmod +x ci/test.sh`

##### 3. Create ci/lint.sh
**File:** `/Users/sac/cre/ci/lint.sh` (new file)
**Purpose:** Unified style and type checking

```bash
#!/usr/bin/env bash
# Lint script for CRE project
# Runs Dialyzer, XREF, and EFmt style checking
set -e

echo "=== Running Static Analysis ==="

# Dialyzer type checking
echo ""
echo "1/3 Running Dialyzer (type analysis)..."
rebar3 dialyzer

# XREF cross-reference analysis
echo ""
echo "2/3 Running XREF (cross-reference checks)..."
rebar3 xref

# EFmt code formatting
echo ""
echo "3/3 Checking code formatting (EFmt)..."
rebar3 efmt -c

echo ""
echo "✅ All static analysis passed"
```

**Make executable:** `chmod +x ci/lint.sh`

##### 4. Create ci/coverage.sh
**File:** `/Users/sac/cre/ci/coverage.sh` (new file)
**Purpose:** Generate test coverage reports

```bash
#!/usr/bin/env bash
# Coverage script for CRE project
# Generates coverage reports for EUnit tests
set -e

echo "=== Generating Test Coverage ==="

# Run EUnit with coverage
echo "Running tests with coverage enabled..."
rebar3 eunit --cover

# Generate HTML report
echo "Generating coverage report..."
rebar3 cover

# Report location
echo ""
echo "✅ Coverage report generated:"
echo "   HTML: _build/test/cover/index.html"
echo "   Data: _build/test/cover/cre.coverdata"

# Open report if on macOS with DISPLAY set
if [ "$(uname)" = "Darwin" ] && [ -n "$DISPLAY" ]; then
    open _build/test/cover/index.html 2>/dev/null || true
fi
```

**Make executable:** `chmod +x ci/coverage.sh`

##### 5. Create ci/all.sh
**File:** `/Users/sac/cre/ci/all.sh` (new file)
**Purpose:** Run all checks (build + test + lint)

```bash
#!/usr/bin/env bash
# Complete CI check script
# Runs build, test, and lint in sequence
set -e

echo "======================================"
echo "  CRE Complete CI Check"
echo "======================================"

./ci/build.sh
echo ""
./ci/test.sh
echo ""
./ci/lint.sh

echo ""
echo "======================================"
echo "  ✅ All Checks Passed"
echo "======================================"
```

**Make executable:** `chmod +x ci/all.sh`

#### Success Criteria:

##### Automated Verification:
- [ ] All scripts exist: `ls -la ci/*.sh`
- [ ] All scripts executable: `for f in ci/*.sh; do test -x $f; done`
- [ ] Build succeeds: `./ci/build.sh` compiles 248 modules
- [ ] Tests run: `./ci/test.sh` executes EUnit, Doctests, and CT
- [ ] Lint passes: `./ci/lint.sh` completes Dialyzer, XREF, EFmt
- [ ] All checks pass: `./ci/all.sh` completes successfully

##### Manual Verification:
- [ ] Run `./ci/all.sh` from fresh clone
- [ ] Verify each stage output is clear and informative
- [ ] Check that failures are caught (e.g., break a test and verify script fails)
- [ ] Confirm scripts work on both Linux and macOS

**Note:** These scripts are the foundation for CI automation. Test them thoroughly before updating workflows.

---

### Phase 3: Update GitHub Actions Workflows (High Priority)

#### Overview
Standardize CI workflows to use the new unified scripts, align OTP versions, and add missing doctest coverage. This ensures consistent behavior across all CI pipelines.

#### Changes Required:

##### 1. Update .github/workflows/ci.yml
**File:** `/Users/sac/cre/.github/workflows/ci.yml`
**Changes:**
- Align OTP versions with ci-cd.yml (drop OTP 25, add OTP 28)
- Replace direct rebar3 calls with `./ci/` scripts
- Add doctest execution

**Current (simplified):**
```yaml
jobs:
  ci:
    strategy:
      matrix:
        otp: ['25.0', '26.0', '27.0']
    steps:
      - run: rebar3 compile
      - run: rebar3 xref
      - run: rebar3 efmt -c
      - run: rebar3 eunit
      - run: rebar3 dialyzer
      - run: rebar3 edoc
```

**New:**
```yaml
jobs:
  ci:
    name: Run checks and tests over ${{matrix.otp}} and ${{matrix.os}}
    runs-on: ${{matrix.os}}
    strategy:
      matrix:
        otp: ['26.3', '27.2', '28.0']
        os: [ubuntu-latest]
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{matrix.otp}}
          rebar3-version: '3.24.0'

      # Build
      - name: Build
        run: ./ci/build.sh

      # Lint (Dialyzer + XREF + EFmt)
      - name: Lint
        run: ./ci/lint.sh

      # Test (EUnit + Doctests + Common Test)
      - name: Test
        run: ./ci/test.sh

      # Generate Documentation
      - name: Generate Documentation
        run: rebar3 edoc
```

**Key Changes:**
1. OTP versions: 26.3, 27.2, 28.0 (match ci-cd.yml)
2. Use `./ci/build.sh` instead of `rebar3 compile`
3. Use `./ci/lint.sh` instead of separate Dialyzer/XREF/EFmt
4. Use `./ci/test.sh` instead of just `rebar3 eunit` (includes doctests)
5. Keep `rebar3 edoc` separate (not in ci/scripts yet)

##### 2. Update .github/workflows/ci-cd.yml
**File:** `/Users/sac/cre/.github/workflows/ci-cd.yml`
**Changes:**
- Replace direct rebar3 calls with `./ci/` scripts in test and security jobs
- Ensure OTP versions match between workflows

**Changes to lines 88-136 (test job):**
```yaml
test:
  name: Test (OTP ${{ matrix.otp }})
  needs: build
  runs-on: ubuntu-latest
  strategy:
    fail-fast: false
    matrix:
      otp: ['26.3', '27.2', '28.0']

  steps:
    - name: Checkout code
      uses: actions/checkout@v4

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ matrix.otp }}
        rebar3-version: '3.24.0'

    - name: Restore compiled BEAM files
      uses: actions/cache/restore@v4
      with:
        path: _build
        key: beam-${{ github.sha }}-${{ matrix.otp }}-ubuntu-latest
        fail-on-cache-miss: true

    - name: Run tests (EUnit + Doctests + Common Test)
      run: ./ci/test.sh

    - name: Generate coverage report
      run: rebar3 cover

    - name: Upload coverage to Codecov
      uses: codecov/codecov-action@v4
      with:
        files: _build/test/cover/cre.coverdata
        flags: eunit-${{ matrix.otp }}
        name: codecov-${{ matrix.otp }}
        fail_ci_if_error: false
        token: ${{ secrets.CODECOV_TOKEN }}

    - name: Upload coverage artifact
      uses: actions/upload-artifact@v4
      with:
        name: coverage-${{ matrix.otp }}
        path: |
          _build/test/cover/
          _build/test/coverindex.html
        retention-days: 30
```

**Changes to lines 141-178 (integration-test job):**
```yaml
integration-test:
  name: Integration Test (OTP ${{ matrix.otp }})
  needs: build
  runs-on: ubuntu-latest
  strategy:
    fail-fast: false
    matrix:
      otp: ['26.3', '27.2', '28.0']

  steps:
    - name: Checkout code
      uses: actions/checkout@v4

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ matrix.otp }}
        rebar3-version: '3.24.0'

    - name: Restore compiled BEAM files
      uses: actions/cache/restore@v4
      with:
        path: _build
        key: beam-${{ github.sha }}-${{ matrix.otp }}-ubuntu-latest
        fail-on-cache-miss: true

    # Note: test.sh already runs Common Test, but we keep this separate
    # for CI-specific integration test reporting
    - name: Run Common Test
      run: rebar3 ct
      continue-on-error: true

    - name: Upload CT results
      uses: actions/upload-artifact@v4
      if: always()
      with:
        name: ct-results-${{ matrix.otp }}
        path: |
          _build/test/logs/
        retention-days: 30
```

**Changes to lines 183-232 (security job):**
```yaml
security:
  name: Security & Quality (OTP ${{ matrix.otp }})
  needs: build
  runs-on: ubuntu-latest
  strategy:
    fail-fast: false
    matrix:
      otp: ['27.2', '28.0']

  steps:
    - name: Checkout code
      uses: actions/checkout@v4

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ matrix.otp }}
        rebar3-version: '3.24.0'

    - name: Restore compiled BEAM files
      uses: actions/cache/restore@v4
      with:
        path: _build
        key: beam-${{ github.sha }}-${{ matrix.otp }}-ubuntu-latest

    - name: Run static analysis (Dialyzer + XREF + EFmt)
      run: ./ci/lint.sh
      continue-on-error: false  # Changed: fail on lint errors

    - name: Security audit
      run: |
        curl -sfL https://raw.githubusercontent.com/erlang/rebar3/HEAD/rebar3wc -o rebar3wc
        chmod +x rebar3wc
        ./rebar3wc audit || true

    - name: Upload Dialyzer results
      uses: actions/upload-artifact@v4
      if: always()
      with:
        name: dialyzer-results-${{ matrix.otp }}
        path: |
          _build/default/rebar3_crash.dump
          dialyzer_report.md
        retention-days: 30
```

**Key Changes:**
1. Security job now uses `./ci/lint.sh` (includes EFmt, Dialyzer, XREF)
2. Test job uses `./ci/test.sh` (includes EUnit, Doctests, Common Test)
3. Removed duplicate EFmt step (now in lint.sh)
4. Changed `continue-on-error: false` for security (fail on lint issues)

##### 3. Document Known Test Failures
**File:** `/Users/sac/cre/.github/KNOWN_TEST_FAILURES.md` (new file)
**Purpose:** Document expected test failures to distinguish bugs from TODOs

```markdown
# Known Test Failures

This document tracks test failures that are currently tolerated in CI.

## Current Status

- **Total Tests:** 760
- **Passing:** 689 (90.7%)
- **Failing:** 71 (9.3%)

## Expected Failures

### EUnit Tests
<!-- Add known failing EUnit tests here -->

### Common Test Suites
<!-- Add known failing CT suites here -->

### Doctests
- `yawl_schema` - Excluded from doctest runs due to API mismatches

## Investigation Needed

The following test failures need investigation to determine if they are:
- Expected failures (TODO markers, incomplete features)
- Bugs that need fixing
- Test configuration issues

**Action Item:** Create separate work item to investigate and categorize failing tests.

## Last Updated

2026-02-11 - Initial documentation
```

#### Success Criteria:

##### Automated Verification:
- [ ] CI workflow passes: GitHub Actions shows green checkmark
- [ ] All OTP versions tested: 26.3, 27.2, 28.0
- [ ] Doctests run in CI: Check logs for "Running doctests (123 modules)"
- [ ] EFmt check passes: No "command not found" errors
- [ ] CI badge in README shows passing: Badge is green

##### Manual Verification:
- [ ] Push a test commit to verify CI triggers
- [ ] Check CI logs show `./ci/build.sh`, `./ci/test.sh`, `./ci/lint.sh` execution
- [ ] Verify coverage reports upload to Codecov
- [ ] Confirm Common Test results upload as artifacts
- [ ] Test on all three OTP versions (26.3, 27.2, 28.0)

**Note:** This phase makes CI consistent and complete. After this, the README badge should be accurate.

---

### Phase 4: Add Pre-commit Hooks (Medium Priority)

#### Overview
Install pre-commit hooks to run quality checks locally before pushing. This catches issues early and reduces CI failures, improving developer experience.

#### Changes Required:

##### 1. Create .pre-commit-config.yaml
**File:** `/Users/sac/cre/.pre-commit-config.yaml` (new file)
**Purpose:** Configure pre-commit hooks for local validation

```yaml
# Pre-commit hooks for CRE project
# Install: pip install pre-commit && pre-commit install
# Run manually: pre-commit run --all-files

repos:
  # Erlang compilation check
  - repo: local
    hooks:
      - id: erlang-compile
        name: Erlang Compile
        entry: ./ci/build.sh
        language: script
        pass_filenames: false
        files: \.(erl|hrl)$

  # Erlang tests
      - id: erlang-tests
        name: Erlang Tests
        entry: ./ci/test.sh
        language: script
        pass_filenames: false
        files: \.(erl|hrl)$

  # Erlang linting (Dialyzer + XREF + EFmt)
      - id: erlang-lint
        name: Erlang Lint
        entry: ./ci/lint.sh
        language: script
        pass_filenames: false
        files: \.(erl|hrl)$

  # General file checks
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.5.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer
      - id: check-yaml
        exclude: ^(\.github/|_build/)
      - id: check-added-large-files
        args: ['--maxkb=1000']
      - id: check-merge-conflict
```

##### 2. Create Installation Script
**File:** `/Users/sac/cre/scripts/install-pre-commit.sh` (new file)
**Purpose:** One-time setup script for developer tools

```bash
#!/usr/bin/env bash
# Install pre-commit hooks and developer tools
set -e

echo "=== Installing CRE Developer Tools ==="

# Check Python installed (pre-commit requires it)
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 required for pre-commit"
    echo "   Install: brew install python3  # macOS"
    echo "           sudo apt-get install python3  # Ubuntu"
    exit 1
fi

# Install pre-commit
echo ""
echo "1/2 Installing pre-commit..."
pip3 install pre-commit --user

# Install hooks
echo ""
echo "2/2 Installing git hooks..."
pre-commit install

echo ""
echo "✅ Developer tools installed"
echo ""
echo "Pre-commit hooks will now run on git commit:"
echo "  - Compile check (src/**/*.erl)"
echo "  - Test run (EUnit + Doctests + Common Test)"
echo "  - Lint check (Dialyzer + XREF + EFmt)"
echo ""
echo "To skip hooks (not recommended):"
echo "  git commit --no-verify -m 'message'"
```

**Make executable:** `chmod +x scripts/install-pre-commit.sh`

##### 3. Update Documentation
**File:** `/Users/sac/cre/docs/development/build_system.md`
**Add section:** "Pre-commit Hooks" after "Code Quality Standards" (around line 587)

```markdown
### Pre-commit Hooks

CRE uses pre-commit hooks to enforce code quality before pushing.

#### Installation

```bash
# One-time setup
./scripts/install-pre-commit.sh
```

This installs:
- Python pre-commit framework
- Git hooks that run on `git commit`
- Automatic checks for compilation, tests, and linting

#### What Gets Checked

Every commit triggers:
1. **Compile check** - Verifies code compiles
2. **Test run** - Runs EUnit, Doctests, and Common Test
3. **Lint check** - Runs Dialyzer, XREF, and EFmt
4. **File checks** - Trailing whitespace, merge conflicts, large files

#### Skipping Hooks

If you need to bypass hooks (not recommended):
```bash
git commit --no-verify -m "WIP: work in progress"
```

#### Running Hooks Manually

Run all hooks on all files:
```bash
pre-commit run --all-files
```

Run specific hook:
```bash
pre-commit run erlang-compile --all-files
```
```

#### Success Criteria:

##### Automated Verification:
- [ ] Pre-commit config valid: `pre-commit validate-config`
- [ ] Hooks install: `./scripts/install-pre-commit.sh` succeeds
- [ ] Hooks trigger on commit: Modify .erl file and commit
- [ ] Hooks catch issues: Break code and verify commit fails
- [ ] Hooks can be skipped: `git commit --no-verify` works

##### Manual Verification:
- [ ] Install hooks on fresh clone
- [ ] Make a breaking change (add syntax error)
- [ ] Try to commit: Pre-commit should reject the commit
- [ ] Fix the issue and commit: Should succeed
- [ ] Verify all three hooks run (compile, test, lint)

**Note:** Pre-commit hooks are optional for developers. If they're too slow or disruptive, they can be skipped with `--no-verify`.

---

### Phase 5: Create Developer Quick Start Documentation (Low Priority)

#### Overview
Create a quick start guide for developers to set up their environment and run common tasks. This reduces onboarding time and provides a single source of truth for development workflows.

#### Changes Required:

##### 1. Create DEVELOPMENT.md
**File:** `/Users/sac/cre/DEVELOPMENT.md` (new file)
**Purpose:** Quick start guide for developers

```markdown
# CRE Development Quick Start

Quick start guide for setting up development environment and common workflows.

## Prerequisites

- Erlang/OTP 26, 27, or 28
- Rebar3 3.24.0+
- Git
- (Optional) Python 3 + pre-commit for local hooks

## Installation

### 1. Clone Repository

\`\`\`bash
git clone https://github.com/joergen7/cre.git
cd cre
\`\`\`

### 2. Install Dependencies

\`\`\`bash
# Download and compile dependencies
rebar3 compile
\`\`\`

### 3. Run Tests

\`\`\`bash
# Run all tests (EUnit + Doctests + Common Test)
./ci/test.sh

# Run specific test category
./scripts/run_eunit.sh          # EUnit only
./scripts/run_doctests.sh       # Doctests only
rebar3 ct                       # Common Test only
\`\`\`

### 4. Install Pre-commit Hooks (Optional)

\`\`\`bash
./scripts/install-pre-commit.sh
\`\`\`

## Common Workflows

### Making Changes

1. Create feature branch:
   \`\`\`bash
   git checkout -b feature/my-feature
   \`\`\`

2. Make changes and test:
   \`\`\`bash
   # Compile
   ./ci/build.sh

   # Test
   ./ci/test.sh

   # Lint
   ./ci/lint.sh
   \`\`\`

3. Format code:
   \`\`\`bash
   # Check formatting
   rebar3 efmt -c

   # Format all files
   rebar3 efmt -w
   \`\`\`

4. Commit (pre-commit hooks run automatically):
   \`\`\`bash
   git add .
   git commit -m "Feature: description"
   \`\`\`

### Running All Checks

\`\`\`bash
# Complete CI check (build + test + lint)
./ci/all.sh
\`\`\`

### Generating Coverage

\`\`\`bash
./ci/coverage.sh

# View report
open _build/test/cover/index.html  # macOS
xdg-open _build/test/cover/index.html  # Linux
\`\`\`

### Interactive Development

\`\`\`bash
# Start Erlang shell with CRE loaded
rebar3 shell --apps cre

# From shell, run commands:
# 1> cre:start().
# 2> cre:status().
\`\`\`

## Troubleshooting

### "Command not found: rebar3"

Install rebar3:
\`\`\`bash
brew install rebar3  # macOS
sudo apt-get install rebar3  # Ubuntu
\`\`\`

### "PLT not found" Dialyzer Error

Build PLT on first run (takes 1-2 minutes):
\`\`\`bash
rebar3 dialyzer
\`\`\`

### "Module not found" Error

Clean and rebuild:
\`\`\`bash
rebar3 clean
rebar3 compile
\`\`\`

### Pre-commit Hooks Too Slow?

Disable specific hooks or skip:
\`\`\`bash
# Skip for one commit
git commit --no-verify -m "WIP"

# Uninstall hooks
pre-commit uninstall
\`\`\`

## Next Steps

- Read [Build System Documentation](docs/development/build_system.md)
- Read [Architecture Guide](docs/ARCHITECTURE.md)
- Review [Contributing Guidelines](docs/development/contributing.md)

## Getting Help

- GitHub Issues: https://github.com/joergen7/cre/issues
- Documentation: See `docs/` directory
- Erlang/OTP Docs: https://www.erlang.org/doc/
\`\`\`

#### Success Criteria:

##### Automated Verification:
- [ ] File exists: `ls -la DEVELOPMENT.md`
- [ ] Links work: All internal links resolve
- [ ] Code blocks valid: Bash commands run successfully

##### Manual Verification:
- [ ] Follow guide from fresh clone
- [ ] Verify all commands work as documented
- [ ] Check that links to other docs resolve
- [ ] Ensure guide is concise (1-2 page read)

**Note:** This is developer convenience documentation. Not critical for CI/CD but improves onboarding.

---

## Testing Strategy

### Unit Tests

**What to test:**
- CI scripts execute successfully
- Scripts return correct exit codes (0 for success, non-zero for failure)
- Error messages are clear and actionable
- Scripts handle edge cases (missing ebin, compilation failures)

**Key edge cases:**
- Running scripts from wrong directory
- Missing dependencies (Rust not installed)
- Incomplete builds (ebin directory missing)
- Test failures (some tests allowed to fail)

### Integration Tests

**End-to-end scenarios:**
1. **Fresh clone workflow:**
   ```bash
   git clone <repo>
   cd cre
   ./ci/all.sh  # Should complete successfully
   ```

2. **Broken code detection:**
   ```bash
   # Introduce syntax error
   echo "garbage" >> src/test.erl
   ./ci/build.sh  # Should fail
   ```

3. **Style violation detection:**
   ```bash
   # Introduce formatting issue
   # (verify efmt catches it)
   ./ci/lint.sh  # Should fail
   ```

4. **Pre-commit hook trigger:**
   ```bash
   # Modify code
   echo "" >> src/test.erl
   git commit -am "test"  # Hooks should run
   ```

### Manual Testing Steps

1. **Verify CI Scripts:**
   ```bash
   cd /Users/sac/cre
   ./ci/build.sh    # Check for "✅ Build successful"
   ./ci/test.sh     # Check for "✅ All critical tests passed"
   ./ci/lint.sh     # Check for "✅ All static analysis passed"
   ./ci/all.sh      # Check for "✅ All Checks Passed"
   ```

2. **Verify EFmt Plugin:**
   ```bash
   rebar3 plugins list | grep efmt  # Should show efmt
   rebar3 efmt -c                   # Should exit 0
   ```

3. **Verify CI Workflows:**
   - Push to feature branch
   - Watch GitHub Actions run
   - Check all jobs pass (build, test, security)
   - Verify CI badge turns green

4. **Verify Pre-commit Hooks:**
   ```bash
   ./scripts/install-pre-commit.sh
   # Modify a file
   echo "" >> src/test.erl
   git add .
   git commit -m "test"  # Hooks should run
   ```

## Migration Notes

### For Developers

**Breaking Changes:** None

**New Requirements:**
- Python 3 for pre-commit hooks (optional)
- Pre-commit hooks run automatically (can be skipped)

**Workflow Changes:**
- Use `./ci/test.sh` instead of `./scripts/run_eunit.sh`
- Use `./ci/lint.sh` instead of `rebar3 dialyzer`
- Pre-commit hooks catch issues before push

**Migration Steps:**
1. Pull latest changes
2. Run `./scripts/install-pre-commit.sh` (optional)
3. Use new `./ci/` scripts for local development

### For CI/CD

**No Migration Needed:** Workflows are updated incrementally

**Rollback Plan:**
- If CI scripts fail, revert to direct `rebar3` calls
- If efmt fails, remove from `project_plugins` and CI
- Git history allows reverting workflow changes

**Future Improvements:**
- Add OTP 25 back when needed
- Make Common Test fail-fast once failures are understood
- Optimize CI execution time (parallel jobs, caching)

## References

- Research: `/Users/sac/cre/.wreckit/items/029-build-system-and-ci-integration/research.md`
- Build Config: `/Users/sac/cre/rebar.config:1-125`
- CI Workflows:
  - `/Users/sac/cre/.github/workflows/ci.yml:1-30`
  - `/Users/sac/cre/.github/workflows/ci-cd.yml:1-520`
  - `/Users/sac/cre/.github/workflows/release.yml:1-174`
- Test Scripts:
  - `/Users/sac/cre/scripts/run_eunit.sh:1-10`
  - `/Users/sac/cre/scripts/run_doctests.sh:1-34`
- Documentation: `/Users/sac/cre/docs/development/build_system.md:1-754`
- Build System Reference: `/Users/sac/cre/docs/development/build_system.md`

## Success Metrics

After completing this plan, the following metrics should be achieved:

1. **CI Stability:** GitHub Actions passes 100% on main branch
2. **Developer Experience:** New developer can run `./ci/all.sh` in <5 minutes
3. **Code Quality:** All code formatted with efmt (no drift)
4. **Test Coverage:** All 760 tests run in CI (EUnit + CT + Doctests)
5. **Type Safety:** Dialyzer runs without errors in CI
6. **Documentation Gap:** DEVELOPMENT.md provides quick start guide

## Open Issues (Deferred)

The following issues are identified but **out of scope** for this plan:

1. **Test Failures:** 71 of 760 tests failing - needs investigation
2. **Dialyzer Exclusions:** 34 modules excluded - need to reduce
3. **CI Performance:** 15-20 minute runtime - can be optimized
4. **OTP 25 EOL:** Dropping OTP 25 may break older systems
5. **Rust NIF Builds:** Not all environments have Rust toolchain

These should be addressed in separate work items.
