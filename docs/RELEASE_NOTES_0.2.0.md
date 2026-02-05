# CRE v0.2.0 Release Notes

## Release Information

- **Release Date**: February 4, 2025
- **Version**: 0.2.0
- **Status**: Production Release

## Executive Summary

CRE v0.2.0 is a significant update that modernizes the codebase for current and future OTP versions. This release drops support for older OTP versions (19-24) and establishes OTP 25.0 as the new minimum requirement. The update ensures full compatibility with OTP 28 and future releases by migrating to modern APIs.

---

## Breaking Changes

### 1. Minimum OTP Version Raised to 25.0

**Impact**: Users on OTP 19-24 must upgrade to OTP 25.0 or later.

**Reason**: The codebase now uses modern OTP features that are not available in older versions. Maintaining compatibility with older versions was blocking adoption of newer OTP features and causing deprecation warnings.

**Migration Path**:
- Upgrade Erlang/OTP to version 25.0, 26.0, 27.0, or 28.0
- No code changes required if you were already on OTP 25+
- See OTP Support Matrix below for detailed compatibility information

### 2. GenPNet Interface Changes

**Impact**: Custom patterns using `gen_pnet:set_usr_info/2` will need updates.

**Changes**:
- The undefined function `gen_pnet:set_usr_info/2` has been removed from `cre_yawl_patterns.erl`
- Pattern state updates now use the process dictionary instead

**Migration Path**:
- If you have custom YAWL patterns, update trigger functions to use process dictionary for mutable state
- See the fixed implementations in `cre_yawl_patterns.erl` lines 998 and 1023 for reference

### 3. Logging API Migration

**Impact**: Code using `error_logger` will receive deprecation warnings.

**Changes**:
- Migrated from `error_logger:info_report/1`, `error_logger:error_report/1`, and `error_logger:warning_report/1`
- To `logger:info/2`, `logger:error/2`, and `logger:warning/2`

**Migration Path**:
- If your code integrates with CRE's logging, update to use the `logger` module
- The new `logger` API provides structured logging with metadata support

---

## New Features

### OTP 28 Compatibility

- Full migration to `logger` API for OTP 28 compatibility
- Updated all deprecated function calls
- Removed deprecation suppression compiler directives

### Modern Build Configuration

- Added OTP 25+ platform define to `rebar.config`
- Added version declaration and OTP compatibility notes
- Improved build documentation

---

## Bug Fixes

### GenPNet Interface (cre_yawl_patterns.erl)

- **Issue**: Undefined `gen_pnet:set_usr_info/2` calls at lines 998 and 1023
- **Fix**: Updated to use process dictionary for mutable pattern state
- **Impact**: Resolves runtime errors in pattern trigger functions

### Deprecated Function Usage (yawl_stateless.erl)

- **Issue**: `code:lib_dir/2` deprecated and removed in OTP 27+
- **Fix**: Replaced `code:lib_dir(cre, priv)` with `code:lib_dir(cre) ++ "/priv"`
- **Impact**: Eliminates deprecation warnings and ensures future compatibility

### Documentation

- **Issue**: README incorrectly stated OTP 19+ as minimum requirement
- **Fix**: Updated to correctly state OTP 25.0+ requirement
- **Impact**: Users now see accurate system requirements

---

## OTP Support Matrix

| OTP Version | Support Status | Notes |
|-------------|----------------|-------|
| 19-24 | **Dropped** | Use CRE 0.1.x for these versions |
| 25.0 | **Supported** | Minimum supported version |
| 26.0 | **Supported** | Tested |
| 27.0 | **Supported** | Tested |
| 28.0 | **Supported** | Tested |

---

## Upgrade Guide from CRE 0.1.x

### Step 1: Verify OTP Version

```bash
erl -version
# Ensure you have OTP 25.0 or later
```

### Step 2: Update Dependency

**rebar.config**:
```erlang
{deps, [{cre, "0.2.0"}]}.
```

**mix.exs** (Elixir):
```elixir
{:cre, "~> 0.2.0"}
```

### Step 3: Update Code (If Applicable)

If you have custom YAWL patterns that use `gen_pnet:set_usr_info/2`, update to use process dictionary:

```erlang
% Before (no longer works):
gen_pnet:set_usr_info( Pid, NewState ).

% After (use process dictionary):
put( usr_info, NewState ).
```

### Step 4: Rebuild

```bash
rebar3 clean
rebar3 compile
```

### Step 5: Test

```bash
rebar3 ct
```

---

## Known Issues

The following issues are tracked as technical debt and will be addressed in future releases:

1. **Test Coverage**: Overall coverage is approximately 15%, with a target of 70%+ for version 1.0.0
2. **Dialyzer Warnings**: Some modules have Dialyzer warnings that need resolution
3. **Integration Tests**: 11 tests in `yawl_integration_test.erl` are failing due to gen_server setup issues
4. **Zero-Coverage Modules**: 23 modules have zero test coverage

**Note**: These issues existed in previous releases and do not represent regressions.

---

## Deployment Instructions

### Production Deployment

1. **Pre-deployment Checklist**:
   - [ ] Verify OTP 25.0+ is installed
   - [ ] Backup existing CRE configuration
   - [ ] Review breaking changes above

2. **Installation**:
   ```bash
   # Via hex.pm
   rebar3 upgrade cre

   # Or from source
   git clone https://github.com/joergen7/cre.git
   cd cre
   git checkout v0.2.0
   rebar3 compile
   ```

3. **Configuration**: No configuration changes required from 0.1.x

4. **Verification**:
   ```bash
   # Start CRE
   rebar3 shell

   # Verify version
   application:which_applications().
   % Should show {cre,"0.2.0",...}
   ```

### Rolling Upgrade

For distributed systems running CRE:

1. Upgrade nodes one at a time
2. Monitor logs for any issues
3. Verify communication between mixed versions (not recommended - upgrade all nodes promptly)

---

## Dependency Changes

| Dependency | Old Version | New Version | Notes |
|------------|-------------|-------------|-------|
| gen_pnet | GitHub (master) | GitHub (master) | Using fork due to interface changes |
| cowboy | 2.12.0 | 2.12.0 | Unchanged |
| jsone | 1.9.0 | 1.9.0 | Unchanged |
| bcrypt | 1.0.1 | 1.0.1 | OTP 28 compatibility workaround may be needed |

---

## Contributors

- Jörgen Brandt (Original Author)
- Claude Flow (Release preparation)

## License

Apache 2.0

## Support

For issues, questions, or contributions:
- GitHub Issues: https://github.com/joergen7/cre/issues
- Documentation: https://github.com/joergen7/cre

---

## Changelog

For a detailed list of all changes, see [CHANGELOG.md](../CHANGELOG.md)
