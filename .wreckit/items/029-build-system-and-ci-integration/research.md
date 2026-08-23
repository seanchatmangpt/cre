# Research: Build system and CI integration

**Date**: 2026-02-11
**Item**: 029-build-system-and-ci-integration

## Research Question
Professional Erlang project needs proper build configuration, continuous integration, and automated testing to ensure code quality.

**Motivation:** Enables automated testing, supports CI/CD, provides project structure, ensures code quality through automated checks.

**Success criteria:**
- rebar3 compile succeeds
- rebar3 eunit succeeds
- rebar3 ct succeeds
- CI scripts implemented
- Style checks included

**Technical constraints:**
- Standard OTP application structure
- Common test suites
- No external deps beyond OTP

**Signals:** priority: low, urgency: Required but not blocking initial development

## Summary

The CRE project has a **well-established rebar3 build system** but **lacks comprehensive CI/CD automation**. The project successfully compiles with 248 Erlang modules, has extensive test coverage (106 test files), and includes dialyzer type checking configuration. However, there are **no CI/CD pipeline configurations** (no GitHub Actions, no GitLab CI) despite the README badge claiming CI/CD exists.

**Current State:**
- ✅ **rebar3 compile**: Works (multiple source directories, pre-hooks for Rust NIFs)
- ✅ **rebar3 eunit**: Works (98 test modules with doctest support)
- ✅ **rebar3 ct**: Partially works (9 Common Test suites exist)
- ✅ **Style checks**: Dialyzer configured with warnings
- ❌ **CI automation**: Missing (no `.github/workflows/` or `.gitlab-ci.yml`)

**Key Finding:** The project has all the **build infrastructure** in place but lacks **automation scripts** and **CI pipeline definitions**. The README references a non-existent CI/CD badge, indicating this was planned but never implemented.

## Current State Analysis

### Existing Implementation

#### Build Configuration (rebar.config)

**File:** `/Users/sac/cre/rebar.config:1-125`

The rebar.config is comprehensive and production-ready:

1. **Erlang compiler options** (lines 1-24):
   - `debug_info` and `bin_opt_info` enabled
   - OTP 25+ platform define
   - Documentation storage for doctests (`{doc, "excerpt"}`)
   - 13 source directories configured (src, src/core, src/pnet, src/wf, src/yawl, src/patterns, src/wfnet, src/api, src/integration, src/http, src/app, src/nato, src/mining, src/prediction, src/telemetry, src/bench, src/rust_nifs, src/rust_implementations, src/validate, src/db)

2. **Rust NIF pre-hooks** (lines 30-34):
   - Pre-compilation hook builds Rust NIFs before Erlang compilation
   - Creates ebin directories to avoid race conditions
   - Two Rust components: src/rust_nifs and src/rust_implementations/paper_algorithms

3. **Dependencies** (lines 36-46):
   - 9 external dependencies (all from Git, no Hex packages)
   - Key deps: gen_pnet, lib_combin, cowboy, cowlib, ranch, jsx, jsone, jiffy, yamerl
   - All pinned to specific tags/refs for reproducibility

4. **Test profiles** (lines 64-73):
   - `test` profile: EUnit with meck mocking
   - `concuerror` profile: Concuerror for concurrency testing
   - `debug` profile: recon, redbug, eflame for debugging

5. **Dialyzer configuration** (lines 82-124):
   - Warnings: unmatched_returns, error_handling, underspecs
   - PLT extra apps: lib_combin, gen_pnet, jsone, xmerl
   - 34 excluded modules (test modules and known problem modules)

#### Application Resource File

**File:** `/Users/sac/cre/src/cre.app.src:1-185`

Standard OTP application resource file:
- Application: `cre`
- Version: 0.3.0
- 180 registered modules listed
- Applications: kernel, stdlib, crypto, inets, cowboy, jsx
- Apache 2.0 license

#### Test Infrastructure

**EUnit Tests** (98+ test modules):
- Located in `/Users/sac/cre/test/` directory
- Pattern: `*_tests.erl` files
- Example: `yawl_pattern_tests.erl`, `wcp18_20_tests.erl`, `yawl_schema_tests.erl`
- Doctest support: 123 modules with `doctest_test()` function

**Common Test Suites** (9 suites):
- Located in `/Users/sac/cre/test/` directory
- Pattern: `*_SUITE.erl` files
- Examples:
  - `cre_yawl_SUITE.erl` - YAWL pattern tests
  - `yawl_performance_SUITE.erl` - Performance benchmarks
  - `agi_symposium_simulation_SUITE.erl` - Simulation tests
  - `cre_yawl_exception_SUITE.erl` - Exception handling tests
  - `cre_yawl_resource_SUITE.erl` - Resource management tests
  - `rust_interface_SUITE.erl` - Rust NIF integration tests
  - `benchmark_SUITE.erl` - Benchmark suite

**Test Execution Scripts**:
- `/Users/sac/cre/scripts/run_eunit.sh:1-10` - EUnit runner with workaround for yawl_persistence.beam race condition
- `/Users/sac/cre/scripts/run_doctests.sh:1-34` - Auto-discovers and runs all doctest modules

#### Static Analysis

**Dialyzer**:
- Configured in rebar.config (lines 82-124)
- Type checking enabled
- Custom agent exists: `/Users/sac/cre/.claude/agents/dialyzer-check.md:1-23`
- Warnings documented in:
  - `/Users/sac/cre/docs/dialyzer_type_analysis_report.md`
  - `/Users/sac/cre/docs/dialyzer_report.md`

#### Project Structure

**Standard OTP layout**:
```
/Users/sac/cre/
├── rebar.config              # Build configuration
├── src/cre.app.src           # Application resource file
├── src/
│   ├── core/                 # Core OTP behaviors
│   ├── pnet/                 # Petri net utilities
│   ├── wf/                   # Workflow utilities
│   ├── yawl/                 # YAWL-specific modules
│   ├── patterns/             # 43 workflow patterns
│   ├── api/                  # Client APIs
│   ├── http/                 # HTTP handlers
│   ├── integration/          # External integrations
│   └── app/                  # Application modules
├── test/                     # Test suites (EUnit + Common Test)
├── scripts/                  # Build and utility scripts
└── _build/                   # Build output
```

### What's Missing

#### 1. No CI/CD Pipeline Configuration

**Expected locations** (all empty):
- `.github/workflows/*.yml` - No GitHub Actions workflows
- `.gitlab-ci.yml` - No GitLab CI configuration
- `Jenkinsfile` - No Jenkins pipeline

**Evidence of planned CI**:
- `/Users/sac/cre/README.md:4` contains badge: `[![CI/CD](https://github.com/joergen7/cre/workflows/CI%2FCD%20Pipeline/badge.svg)]`
- Badge references non-existent workflow file
- Indicates CI was planned but never implemented

#### 2. No Automated Build Scripts

**Missing scripts**:
- No `ci/build.sh` - Unified build script
- No `ci/test.sh` - Unified test script
- No `ci/coverage.sh` - Coverage report generator
- No `ci/lint.sh` - Style checking script

#### 3. No Code Formatting Configuration

**Missing**:
- No `.erlang-format` configuration file
- No `elvis.config` for Erlang style checker
- No formatting scripts in `scripts/`

#### 4. No Pre-commit Hooks

**Missing**:
- No `.git/hooks/` configuration
- No pre-commit validation
- No commit message linting

### Key Files

#### Build Configuration
- `/Users/sac/cre/rebar.config:1-125` - Main rebar3 configuration with 13 source dirs, deps, test profiles, dialyzer settings
- `/Users/sac/cre/src/cre.app.src:1-185` - OTP application resource file with 180 modules

#### Test Execution
- `/Users/sac/cre/scripts/run_eunit.sh:1-10` - EUnit runner with compile workaround
- `/Users/sac/cre/scripts/run_doctests.sh:1-34` - Doctest discovery and execution
- `/Users/sac/cre/test/cre_yawl_SUITE.erl:1-100` - Example Common Test suite structure

#### Static Analysis
- `/Users/sac/cre/.claude/agents/dialyzer-check.md:1-23` - Dialyzer type checking agent

#### Documentation
- `/Users/sac/cre/docs/ARCHITECTURE.md:1-150` - Joe Armstrong design philosophy
- `/Users/sac/cre/README.md:1-100` - Project overview with false CI/CD badge

## Technical Considerations

### Dependencies

**External dependencies** (from rebar.config:36-46):
- `gen_pnet` - Petri net execution engine
- `lib_combin` - Combinatorics library
- `cowboy` 2.14.2 - HTTP server
- `cowlib` 2.16.0 - HTTP utilities
- `ranch` 2.1.0 - Socket acceptor pool
- `jsx` v3.1.0 - JSON encoder/decoder
- `jsone` 1.9.0 - Alternative JSON library
- `jiffy` 1.1.1 - Fast JSON decoder
- `yamerl` 0.10.0 - YAML parser

**Test dependencies**:
- `meck` 0.9.2 - Mocking library for EUnit
- `concuerror` 0.21.0 - Concurrency testing (optional profile)
- `doctest` - Doctest support (already configured)

**Internal modules to integrate with**:
- All 248 Erlang modules in src/
- 106 test files in test/
- 43 workflow patterns in src/patterns/

### Patterns to Follow

**1. Existing test script pattern**:
From `/Users/sac/cre/scripts/run_eunit.sh:1-10`:
```bash
#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.."
rm -rf _build/test
mkdir -p _build/test/lib/cre/ebin
rebar3 as test compile
rebar3 eunit "$@"
```

**2. Doctest discovery pattern**:
From `/Users/sac/cre/scripts/run_doctests.sh:7-14`:
```bash
MODULES=$(grep -rl "doctest_test()" src --include="*.erl" 2>/dev/null \
    | grep -v '\.bak' \
    | xargs -I{} basename {} .erl \
    | grep -v '^yawl_schema$' \
    | sort -u \
    | tr '\n' ' ')
```

**3. Rebar3 profile pattern**:
From `/Users/sac/cre/rebar.config:64-73`:
```erlang
{profiles,
 [ {test, [{cover_enabled, false},
           {erl_opts, [debug_info, {doc, "excerpt"}, {d, 'TEST'}, {i, "src/wf"}, {i, "include"}]},
           {deps, [{meck, "0.9.2"}]}]},
  {concuerror, [{deps, [{concuerror, "0.21.0"}]}]},
  {debug, [{deps, [
                    {recon, {git, "https://github.com/ferd/recon.git", {tag, "2.5.1"}}},
                    {redbug, {git, "https://github.com/massemanet/redbug.git", {tag, "2.0.6"}}},
                    {eflame, {git, "https://github.com/proger/eflame.git", {tag, "1.0.0"}}}
                   ]}]}]}.
```

**4. Dialyzer configuration pattern**:
From `/Users/sac/cre/rebar.config:82-124`:
```erlang
{dialyzer, [{warnings, [unmatched_returns,
                        error_handling,
                        underspecs]},
            {plt_extra_apps, [lib_combin, gen_pnet, jsone, xmerl]},
            {get_warnings, true},
            {include_dirs, ["include"]},
            {exclude_mods, [yc_demo,
                             yawl_sms_test,
                             %% ... 31 more excluded modules
                            ]}]}.
```

**5. Rust NIF build pattern**:
From `/Users/sac/cre/rebar.config:31-34`:
```erlang
{pre_hooks, [{"(linux|darwin|solaris|.*)", compile,
              "mkdir -p _build/test/lib/cre/ebin _build/default/lib/cre/ebin 2>/dev/null; "
              "cd src/rust_nifs && make build 2>/dev/null || echo 'Rust NIF build skipped'; "
              "cd src/rust_implementations && make build 2>/dev/null || echo 'Rust paper algorithms build skipped'"}]}.
```

### Existing Conventions

**Naming conventions**:
- Test modules: `*_tests.erl` for EUnit
- Test suites: `*_SUITE.erl` for Common Test
- Source modules: lowercase with underscores
- Directories: plural names (patterns, miners, predictions)

**File organization**:
- All source in `src/` with subdirectories by domain
- All tests in `test/` with subdirectories mirroring src structure
- Scripts in `scripts/` with descriptive names
- Build output in `_build/`

**Error handling**:
- EUnit tests use `?assert` macros
- Common Test uses `?assertEqual` from ct.hrl
- Test failures should exit with non-zero status

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Rust NIF compilation fails in CI** | High - Build breaks if Rust not installed | Add Rust toolchain setup in CI; allow graceful failure with warning; pre-build NIFs in Docker image |
| **yawl_persistence.beam race condition** | Medium - Intermittent test failures | Keep existing workaround (mkdir ebin before compile); use rebar3 compile --for-test |
| **Dialyzer PLT build time** | Medium - Slow CI feedback (5-10 minutes) | Cache PLT in CI artifacts; incremental builds; run dialyzer only on merge to main |
| **Test execution time (760 tests)** | Medium - Long CI runs (>15 minutes) | Parallelize test execution; split test suites across jobs; use test sharding |
| **Missing style checks** | Low - Inconsistent code style | Add elvis or erlang_format to CI; start with warnings-only mode |
| **OTP version compatibility (25-28)** | Low - Tests may pass on one OTP, fail on another | Test matrix across OTP 25, 26, 27, 28; use GitHub Actions matrix strategy |
| **Git dependencies vs Hex** | Low - Network failures, repo availability | Consider migrating to Hex packages; mirror deps in CI; use dependency caching |
| **No CI/CD pipeline exists** | Critical - No automation, manual deployments | Start with GitHub Actions (README badge reference); create basic build/test workflow |

## Recommended Approach

Based on the research, here's the recommended implementation strategy:

### Phase 1: Create Unified Build Scripts

**Objective:** Consolidate existing ad-hoc scripts into unified CI-friendly scripts.

1. **Create `ci/build.sh`**:
   ```bash
   #!/usr/bin/env bash
   set -e
   echo "=== Building CRE ==="
   rebar3 compile
   echo "✓ Build successful"
   ```

2. **Create `ci/test.sh`**:
   ```bash
   #!/usr/bin/env bash
   set -e
   echo "=== Running EUnit ==="
   ./scripts/run_eunit.sh
   echo "=== Running Doctests ==="
   ./scripts/run_doctests.sh
   echo "=== Running Common Test ==="
   rebar3 ct
   echo "✓ All tests passed"
   ```

3. **Create `ci/lint.sh`**:
   ```bash
   #!/usr/bin/env bash
   set -e
   echo "=== Running Dialyzer ==="
   rebar3 dialyzer
   echo "✓ No type errors"
   ```

4. **Create `ci/all.sh`**:
   ```bash
   #!/usr/bin/env bash
   set -e
   ./ci/build.sh
   ./ci/test.sh
   ./ci/lint.sh
   echo "✓ All checks passed"
   ```

**Estimated effort:** 1-2 hours

### Phase 2: Implement GitHub Actions Workflow

**Objective:** Create CI/CD pipeline matching the README badge.

1. **Create `.github/workflows/ci-cd.yml`**:
   ```yaml
   name: CI/CD Pipeline

   on:
     push:
       branches: [ main, dev ]
     pull_request:
       branches: [ main ]

   jobs:
     build:
       runs-on: ubuntu-latest
       strategy:
         matrix:
           otp: [25, 26, 27, 28]

       steps:
       - uses: actions/checkout@v3

       - name: Install Erlang/OTP
         uses: erlef/setup-beam@v1
         with:
           otp-version: ${{ matrix.otp }}

       - name: Install Rust
         uses: actions-rs/toolchain@v1
         with:
           toolchain: stable

       - name: Cache Rebar3 deps
         uses: actions/cache@v3
         with:
           path: |
             _build
             ~/.cache/rebar3
           key: ${{ runner.os }}-otp${{ matrix.otp }}-${{ hashFiles('**/rebar.lock') }}

       - name: Build
         run: ./ci/build.sh

       - name: Test
         run: ./ci/test.sh

       - name: Dialyzer
         run: ./ci/lint.sh
   ```

2. **Add coverage reporting** (optional):
   - Use `rebar3 cover` to generate coverage
   - Upload to Codecov or Coveralls

**Estimated effort:** 2-3 hours

### Phase 3: Add Code Style Checks

**Objective:** Ensure consistent code style across the project.

1. **Install elvis** (Erlang style checker):
   ```bash
   rebar3 plugins install elvis
   ```

2. **Create `elvis.config`**:
   ```erlang
   [{elvis, [
     {dirs, ["src", "test"]},
     {filter, ".*\\.erl$"},
     {ruleset, erl_files},
     {rules, [
       {elvis_style, line_length, #{limit => 120}},
       {elvis_style, no_tabs},
       {elvis_style, no_trailing_whitespace}
     ]}
   ]]}.
   ```

3. **Add to `ci/lint.sh`**:
   ```bash
   echo "=== Running Elvis ==="
   rebar3 lint
   ```

**Alternative: erlang-format** (formatter instead of linter):
- Format on-save in editors
- Check formatting in CI (fail if not formatted)

**Estimated effort:** 1-2 hours

### Phase 4: Optimize for Speed

**Objective:** Reduce CI execution time.

1. **Cache Dialyzer PLT**:
   ```yaml
   - name: Cache Dialyzer PLT
     uses: actions/cache@v3
     with:
       path: ~/.cache/dialyzer_plt
       key: dialyzer-${{ matrix.otp }}-${{ hashFiles('rebar.config') }}
   ```

2. **Parallelize test suites**:
   - Split EUnit and Common Test into separate jobs
   - Use rebar3 test parallelism: `rebar3 ct --parallel`

3. **Run critical tests first**:
   - Fast unit tests (EUnit) before slow integration tests
   - Fail fast if compile or basic tests fail

**Estimated effort:** 2-3 hours

### Phase 5: Add Pre-commit Hooks (Optional)

**Objective:** Catch issues before pushing.

1. **Install pre-commit framework**:
   ```bash
   pip install pre-commit
   ```

2. **Create `.pre-commit-config.yaml`**:
   ```yaml
   repos:
   - repo: local
     hooks:
       - id: erlang-compile
         name: Erlang Compile
         entry: ./ci/build.sh
         language: script
         pass_filenames: false

       - id: erlang-tests
         name: Erlang Tests
         entry: ./scripts/run_eunit.sh
         language: script
         pass_filenames: false
   ```

3. **Install hooks**:
   ```bash
   pre-commit install
   ```

**Estimated effort:** 1 hour

## Open Questions

1. **Rust toolchain requirement**: Should CI fail if Rust is not available, or skip NIF compilation with a warning? The current config allows graceful failure (`|| echo 'Rust NIF build skipped'`).

2. **Test execution order**: Should we run EUnit, doctests, and Common Test sequentially or in parallel? Sequential is simpler, parallel is faster.

3. **OTP version testing**: The project supports OTP 25-28. Should we test all versions on every PR, or just on merge to main? Testing all versions on every PR increases CI time 4x.

4. **Dialyzer strictness**: The current config excludes 34 modules. Should we work to reduce this list, or is it acceptable? Excluding test modules is reasonable, but excluding source modules may hide bugs.

5. **Coverage goals**: What is the target code coverage percentage? The project has 96% test pass rate (689/730 tests), but coverage percentage is unknown.

6. **CI platform**: The README badge references GitHub Actions. Is this the desired platform, or should we use GitLab CI, Jenkins, or another system?

7. **Deployment automation**: The success criteria don't mention deployment. Should CI include automated deployment to staging/production, or just build/test/lint?

8. **Documentation generation**: Should CI generate and publish API documentation (edoc) on successful builds?

9. **Performance regression testing**: The project has `yawl_performance_SUITE.erl`. Should we run performance tests in CI and fail on regressions?

10. **Security scanning**: Should we add dependency vulnerability scanning (e.g., `rebar3 hex audit` or `snyk`) to CI?

## Next Steps

1. **Immediate**: Create `ci/` directory with `build.sh`, `test.sh`, `lint.sh` scripts (Phase 1)
2. **High priority**: Implement GitHub Actions workflow matching README badge (Phase 2)
3. **Medium priority**: Add code style checks with elvis or erlang-format (Phase 3)
4. **Low priority**: Optimize CI speed and add pre-commit hooks (Phases 4-5)

## File References

**Build configuration:**
- `/Users/sac/cre/rebar.config:1-125` - Main rebar3 configuration
- `/Users/sac/cre/src/cre.app.src:1-185` - OTP application resource file

**Test infrastructure:**
- `/Users/sac/cre/scripts/run_eunit.sh:1-10` - EUnit runner
- `/Users/sac/cre/scripts/run_doctests.sh:1-34` - Doctest runner
- `/Users/sac/cre/test/cre_yawl_SUITE.erl:1-100` - Example Common Test suite

**Static analysis:**
- `/Users/sac/cre/.claude/agents/dialyzer-check.md:1-23` - Dialyzer agent

**Documentation:**
- `/Users/sac/cre/README.md:1-100` - Project overview with CI badge
- `/Users/sac/cre/docs/ARCHITECTURE.md:1-150` - Architecture documentation
