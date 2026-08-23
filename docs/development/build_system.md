# CRE Build System and Development Workflow

This document describes the complete build system for CRE (Common Runtime Environment), including rebar3 commands, project structure, compilation processes, testing, and development workflow recommendations.

## Table of Contents

- [System Requirements](#system-requirements)
- [Quick Reference](#quick-reference)
- [Project Structure](#project-structure)
- [rebar3 Commands](#rebar3-commands)
- [Compilation](#compilation)
- [Testing](#testing)
- [Static Analysis](#static-analysis)
- [Documentation Generation](#documentation-generation)
- [Release Building](#release-building)
- [Development Workflow](#development-workflow)
- [Troubleshooting](#troubleshooting)

---

## System Requirements

### Required Versions

- **Erlang/OTP**: 25.0, 26.x, 27.x, or 28.x (primary target for 2026)
- **Rebar3**: 3.0.0 or higher
- **Git**: For dependency management

### Installing Dependencies

```bash
# On macOS with Homebrew
brew install erlang rebar3

# On Ubuntu/Debian
sudo apt-get install erlang rebar3

# Verify installation
erl -version
rebar3 version
```

---

## Quick Reference

| Command | Description |
|---------|-------------|
| `rebar3 compile` | Compile source code |
| `rebar3 eunit` | Run EUnit tests |
| `rebar3 ct` | Run Common Test suites |
| `./scripts/run_eunit.sh` | Run EUnit with compile workaround |
| `./scripts/run_doctests.sh` | Run all doctests |
| `rebar3 dialyzer` | Static type analysis |
| `rebar3 xref` | Cross-reference analysis |
| `rebar3 cover` | Generate test coverage report |
| `rebar3 edoc` | Generate API documentation |
| `rebar3 escriptize` | Build standalone executable |
| `rebar3 shell` | Interactive Erlang shell |
| `rebar3 clean` | Clean build artifacts |

---

## Project Structure

### Source Directory Layout

```
/Users/sac/cre/
├── rebar.config              # Build configuration
├── src/                      # Source code
│   ├── core/                 # Core OTP components (gen_pnet, yawl_compile)
│   ├── pnet/                 # Petri Net pure utilities
│   ├── wf/                   # Workflow utilities
│   ├── yawl/                 # YAWL-specific modules
│   ├── patterns/             # Workflow pattern implementations
│   ├── api/                  # API modules (cre_client, cre_yawl_client)
│   ├── http/                 # HTTP/web modules
│   ├── integration/          # Integration modules (telemetry, smtp)
│   ├── app/                  # Application modules (cre.erl)
│   └── nato/                 # NATO deterrence example
├── test/                     # Test suites
│   ├── *_SUITE.erl          # Common Test suites
│   ├── *_test.erl           # EUnit tests
│   └── *_doctest.erl        # Doctest modules
├── scripts/                  # Build and test scripts
├── docs/                     # Documentation
├── examples/                 # Example workflows
├── priv/                     # Private resources (images, static files)
└── _build/                   # Build output (generated)
```

### Source Subdirectories Explained

| Directory | Purpose | Key Modules |
|-----------|---------|-------------|
| `core/` | Core OTP behaviors | `gen_pnet`, `yawl_compile`, `yawl_compiled`, `yawl_validate` |
| `pnet/` | Petri Net algebra | `pnet_types`, `pnet_marking`, `pnet_mode`, `pnet_choice`, `pnet_receipt` |
| `wf/` | Workflow utilities | `wf_engine`, `wf_timer`, `wf_task`, `wf_scope`, `wf_persistence` |
| `yawl/` | YAWL implementation | `yawl_engine`, `yawl_executor`, `yawl_persistence`, `yawl_patterns` |
| `patterns/` | Workflow patterns | `parallel_split`, `exclusive_choice`, `structured_loop`, etc. |
| `api/` | Client APIs | `cre_client`, `cre_yawl_client` |
| `http/` | HTTP handlers | `yawl_interface_d`, `cre_status_handler`, `cre_history_handler` |
| `integration/` | External integrations | `telemetry`, `gen_smtp_client` |
| `app/` | Application entry | `cre`, `cre_master`, `cre_sup` |

---

## rebar3 Commands

### Basic Build Commands

#### Compilation

```bash
# Compile all source files
rebar3 compile

# Compile with debug info (default)
rebar3 compile

# Force recompilation
rebar3 compile --force
```

#### Cleaning Build Artifacts

```bash
# Clean all build artifacts
rebar3 clean

# Clean specific profile
rebar3 as test clean

# Clean including dependencies
rebar3 clean -a
```

#### Interactive Shell

```bash
# Start Erlang shell with all dependencies loaded
rebar3 shell

# Start with application running
rebar3 shell --apps cre

# Start with specific nodename
rebar3 shell --name mynode@localhost
```

---

## Compilation

### Compiler Options

From `rebar.config`:

```erlang
{erl_opts, [debug_info, bin_opt_info,
              {platform_define, "^[0-9]+", 'OTP_25_PLUS'},
              {doc, "excerpt"},  % Enable documentation for doctests
              {src_dirs, [...]}]}.
```

### Source Directories

The compiler searches these directories for source files:

1. `src/` - Root source directory
2. `src/core/` - Core OTP components
3. `src/pnet/` - Petri Net utilities
4. `src/wf/` - Workflow utilities
5. `src/yawl/` - YAWL-specific modules
6. `src/patterns/` - Workflow patterns
7. `src/api/` - API modules
8. `src/integration/` - Integration modules
9. `src/http/` - HTTP handlers
10. `src/app/` - Application modules
11. `src/nato/` - Example implementations

### Pre-Compilation Hooks

```erlang
{pre_hooks, [{"(linux|darwin|solaris|.*)", compile,
    "mkdir -p _build/test/lib/cre/ebin _build/default/lib/cre/ebin 2>/dev/null; true"}]}.
```

This hook ensures the `ebin` directory exists before compilation, avoiding a race condition with the `yawl_persistence.beam` rename operation.

### Compile Profiles

#### Default Profile

```bash
rebar3 compile
# Output: _build/default/lib/cre/ebin/
```

#### Test Profile

```bash
rebar3 as test compile
# Output: _build/test/lib/cre/ebin/
# Includes test dependencies (meck)
```

#### Concuerror Profile

```bash
rebar3 as concuerror compile
# Output: _build/concuerror/lib/cre/ebin/
# Includes Concuerror for model checking
```

---

## Testing

CRE uses a comprehensive test suite combining EUnit, Common Test, doctests, and Concuerror for model checking.

### EUnit Tests

#### Running All EUnit Tests

```bash
# Standard EUnit run
rebar3 eunit

# Using the workaround script (recommended)
./scripts/run_eunit.sh
```

The workaround script handles an intermittent race condition with `yawl_persistence.beam` file renaming by pre-creating the `ebin` directory.

#### Running Specific Tests

```bash
# Run specific module
./scripts/run_eunit.sh --module=pnet_types

# Run specific test function
./scripts/run_eunit.sh --module=pnet_types --test=test_is_place

# Run multiple modules
./scripts/run_eunit.sh --module='pnet_types,pnet_marking'
```

### Doctests

CRE uses doctests extensively for module-level documentation tests.

#### Running All Doctests

```bash
# Auto-discover and run all doctest modules
./scripts/run_doctests.sh
```

The script:
1. Auto-discovers all modules with `doctest_test/0` functions
2. Excludes `yawl_schema` (which has API mismatch issues)
3. Runs all modules in a single EUnit call for efficiency

#### Running Individual Doctests

```bash
# Run a specific module's doctests
rebar3 eunit --module=pnet_types

# Run from test directory
rebar3 as test eunit --module=pnet_types
```

### Common Test Suites

#### Running All Common Tests

```bash
rebar3 ct
```

#### Running Specific Suites

```bash
# Run a specific suite
rebar3 ct --suite=yawl_engine_SUITE

# Run multiple suites
rebar3 ct --suite=yawl_engine_SUITE --suite=yawl_persistence_SUITE

# Run suites in a directory
rebar3 ct --dir=test/
```

### Test Coverage

#### Generating Coverage Report

```bash
# Generate coverage for all tests
rebar3 cover

# Generate coverage for specific module
rebar3 cover --module=pnet_types

# View coverage report
open _build/test/cover/index.html  # macOS
xdg-open _build/test/cover/index.html  # Linux
```

### Concuerror (Model Checking)

CRE includes Concuerror tests for verifying concurrent properties.

#### Running Concuerror Tests

```bash
# Compile with Concuerror profile
rebar3 as concuerror compile

# Run the Concuerror script
./scripts/run_concuerror.escript
```

The current Concuerror test verifies the NATO deterrence scenario's consensus behavior.

### Test Organization

| Test Type | File Pattern | Location | Purpose |
|-----------|--------------|----------|---------|
| EUnit | `*_test.erl` | `test/` | Unit tests |
| Common Test | `*_SUITE.erl` | `test/` | Integration tests |
| Doctests | `doctest_test/0` | Inline in modules | Documentation examples |
| Concuerror | `*_concuerror_tests.erl` | `test/` | Model checking |

---

## Static Analysis

### Dialyzer (Type Analysis)

#### Running Dialyzer

```bash
# Build PLT and analyze
rebar3 dialyzer

# Analyze without rebuilding PLT
rebar3 dialyzer --rebuild=false
```

#### Dialyzer Configuration

From `rebar.config`:

```erlang
{dialyzer, [
    {warnings, [
        unmatched_returns,
        error_handling,
        underspecs
    ]},
    {plt_extra_apps, [lib_combin, gen_pnet, jsone, xmerl]},
    {get_warnings, true},
    {include_dirs, ["include"]},
    {exclude_apps, []}
]}.
```

#### Common Dialyzer Warnings

- `unmatched_returns` - Function return values not matched
- `error_handling` - Potential error handling issues
- `underspecs` - Missing or incomplete type specifications

### XRef (Cross-Reference Analysis)

#### Running XRef

```bash
# Run XRef analysis
rebar3 xref

# Check for undefined functions
rebar3 xref --apply undefined_functions

# Check for unused functions
rebar3 xref --apply locals_not_used
```

#### XRef Checks

- Undefined function calls
- Calls to deprecated functions
- Unused local functions
- Cross-module dependencies

---

## Documentation Generation

### EDoc (API Documentation)

#### Generating Documentation

```bash
# Generate all documentation
rebar3 edoc

# Output location: doc/
```

#### Viewing Documentation

```bash
# Open index (macOS)
open doc/index.html

# Open index (Linux)
xdg-open doc/index.html
```

### EDoc Configuration

Documentation is generated from module-level `-doc()` attributes and function comments. Ensure your modules include:

```erlang
-module(my_module).
-export([my_function/1]).

-doc """
Documentation string for the module.
""".
-doc(#{since => "0.2.0"}).

-spec my_function(Input :: term()) -> Result :: term().
my_function(Input) ->
    %% Implementation
    ok.
```

---

## Release Building

### Building Standalone Executable

```bash
# Build escript
rebar3 escriptize

# Output: _build/default/bin/cre
```

#### Running the Executable

```bash
# Run the built escript
./_build/default/bin/cre

# This starts an Erlang node with nodename cre@hostname
```

### Escript Configuration

From `rebar.config`:

```erlang
{escript_incl_apps, [lib_combin, jsone]}.
{escript_emu_args, "%%! -connect_all false -sname cre\n"}.
{escript_name, "cre"}.
```

---

## Development Workflow

### Recommended Development Cycle

1. **Write Tests First**
   ```bash
   # Create test file alongside source
   # test/my_module_test.erl for src/my_module.erl
   ```

2. **Implement Module**
   ```bash
   # Edit source in src/
   ```

3. **Compile and Test**
   ```bash
   ./scripts/run_eunit.sh --module=my_module
   ```

4. **Run Static Analysis**
   ```bash
   rebar3 dialyzer
   rebar3 xref
   ```

5. **Check Coverage**
   ```bash
   rebar3 cover
   ```

6. **Generate Documentation**
   ```bash
   rebar3 edoc
   ```

### Making Changes

#### 1. Create a Feature Branch

```bash
git checkout -b feature/my-feature
```

#### 2. Implement Changes

- Add new modules to appropriate `src/` subdirectories
- Add tests to `test/`
- Update documentation as needed

#### 3. Verify Changes

```bash
# Full test suite
./scripts/run_doctests.sh
rebar3 ct

# Static analysis
rebar3 dialyzer
rebar3 xref

# Coverage
rebar3 cover
```

#### 4. Format Code

CRE uses EFmt for code formatting:

```bash
# Format all source files
rebar3 efmt -c

# Format specific file
rebar3 efmt -c src/my_module.erl

# Check formatting without modifying
rebar3 efmt -c -i
```

### Git Workflow

Based on project guidelines:

- **Never rebase, only merge**
- Use Typer for CLI tools
- Keep commits atomic and well-described

### Code Quality Standards

#### Type Specifications

All public functions must have `-spec()` attributes:

```erlang
-spec my_function(Arg1 :: Type1, Arg2 :: Type2) -> ReturnType.
```

#### Documentation

Public modules and functions should have `-doc()` attributes:

```erlang
-doc """
Brief description of the function.

Longer description with details about behavior,
arguments, and return values.
""".
```

---

## Dependency Management

### Project Dependencies

From `rebar.config`:

| Dependency | Version | Purpose |
|------------|---------|---------|
| `gen_pnet` | git (master) | Generic Petri net OTP behavior |
| `lib_combin` | git (ref) | Combinatorics utilities |
| `cowboy` | 2.14.2 | HTTP server |
| `cowlib` | 2.16.0 | HTTP utilities |
| `ranch` | 2.1.0 | Socket acceptor pool |
| `jsx` | v3.1.0 | JSON encoder/decoder |
| `jsone` | 1.9.0 | Alternative JSON encoder |

### Test Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| `meck` | 0.9.2 | Mocking library for tests |
| `concuerror` | 0.21.0 | Model checking for concurrent tests |

### Dependency Overrides

The project uses overrides for OTP 28 compatibility:

```erlang
{overrides, [
    {override, cowboy, [
        {deps, [
            {cowlib, {git, "https://github.com/ninenines/cowlib.git", {tag, "2.16.0"}}},
            {ranch, {git, "https://github.com/ninenines/ranch.git", {tag, "2.1.0"}}}
        ]}
    ]}
]}.
```

### Updating Dependencies

```bash
# Update all dependencies
rebar3 upgrade

# Update specific dependency
rebar3 upgrade cowboy

# Clean and rebuild dependencies
rebar3 clean --all
rebar3 compile
```

---

## Troubleshooting

### Common Issues

#### "Module not found" Error

**Problem**: Compiler cannot find a module.

**Solution**:
1. Check that the module is in a valid source directory
2. Run `rebar3 clean` then `rebar3 compile`
3. Verify the module name matches the filename

#### "PLT not found" Dialyzer Error

**Problem**: Dialyzer PLT needs to be built.

**Solution**:
```bash
# Build PLT (takes time on first run)
rebar3 dialyzer
```

#### EUnit "beam rename" Race Condition

**Problem**: Intermittent failure renaming `yawl_persistence.beam`.

**Solution**: Use the workaround script:
```bash
./scripts/run_eunit.sh
```

#### Doctest "undefined function" Errors

**Problem**: API mismatches in doctests.

**Solution**: The `yawl_schema` module is excluded from doctests due to known API mismatches. For other modules, verify function names and arities match current implementations.

#### Concuerror Build Errors

**Problem**: Concuerror profile won't compile.

**Solution**:
```bash
# Clean and rebuild Concuerror profile
rebar3 as concuerror clean
rebar3 as concuerror compile
```

### Getting Help

- Check existing documentation in `docs/`
- Review test files for usage examples
- Run tests with verbose output for debugging
- Use `rebar3 shell` for interactive debugging

---

## Build Script Reference

### scripts/run_eunit.sh

Runs EUnit with workaround for compile race condition.

```bash
./scripts/run_eunit.sh [eunit options]
```

### scripts/run_doctests.sh

Auto-discovers and runs all doctest modules.

```bash
./scripts/run_doctests.sh
```

### scripts/run_concuerror.escript

Runs Concuerror model checking tests.

```bash
./scripts/run_concuerror.escript
```

### scripts/demo_workflow.sh

Demonstrates a sample workflow execution.

```bash
./scripts/demo_workflow.sh
```

### scripts/approval_worker.sh

Demonstrates human-in-the-loop approval workflow.

```bash
./scripts/approval_worker.sh
```

---

## References

- [rebar3 Documentation](https://rebar3.org/docs/)
- [Erlang/OTP Documentation](https://www.erlang.org/doc/)
- [Concuerror User Guide](https://github.com/parapluu/Concuerror)
- [CRE Architecture](docs/ARCHITECTURE.md)
- [CRE API Reference](docs/API_REFERENCE.md)
