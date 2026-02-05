# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-02-04

### Breaking Changes

- **Dropped OTP 19-24 support**: Minimum OTP version is now 25.0
- **GenPNet interface changes**: Pattern state updates now use process dictionary instead of undefined `gen_pnet:set_usr_info/2`
- **Logging API migration**: Migrated from `error_logger` to `logger` for OTP 28 compatibility

### Changed

- **Refactored to modern OTP features**:
  - Replaced `error_logger:info_report/1`, `error_logger:error_report/1`, and `error_logger:warning_report/1` with `logger:info/2`, `logger:error/2`, and `logger:warning/2`
  - Updated `code:lib_dir/2` to `code:lib_dir/1` (deprecated function removed)
  - Removed deprecation suppression compiler directives

- **Updated documentation**:
  - README now correctly states OTP 25.0+ as minimum requirement (was incorrectly stating 19+)
  - Updated version references from 0.1.10 to 0.2.0

### Fixed

- **GenPNet interface**: Fixed undefined `gen_pnet:set_usr_info/2` calls in `cre_yawl_patterns.erl`
  - Trigger functions now use process dictionary for mutable pattern state
  - Lines 998 and 1023 fixed

- **Deprecated function usage**: Replaced `code:lib_dir(cre, priv)` with `code:lib_dir(cre) ++ "/priv"` in `yawl_stateless.erl`

- **Build configuration**:
  - Added OTP 25+ platform define to `rebar.config`
  - Added version declaration and OTP compatibility notes to `rebar.config`

### Technical Debt

- The following items are identified for future releases:
- Improve test coverage from ~15% to 70%+
- Fix 11 failing tests in `yawl_integration_test.erl`
- Address 23 zero-coverage modules
- Fix Dialyzer warnings
- Resolve Xref deprecated function call warnings

### Dependency Updates

- **gen_pnet**: Using fork from GitHub (master branch) due to interface changes
- **cowboy**: 2.12.0
- **jsone**: 1.9.0
- **bcrypt**: 1.0.1 (OTP 28 compatibility workaround may be needed)

### OTP Support Matrix

| OTP Version | Support Status | Notes |
|-------------|----------------|-------|
| 19-24 | **Dropped** | Use CRE 0.1.x for these versions |
| 25.0 | ✅ Supported | Minimum supported version |
| 26.0 | ✅ Supported | Tested |
| 27.0 | ✅ Supported | Tested |
| 28.0 | ⚠️ Partial | bcrypt may need workaround |

### Migration Guide from 0.1.x

1. **Update OTP version**: Ensure you're using OTP 25.0 or later
2. **Update dependency**: Change `{cre, "0.1.10"}` to `{cre, "0.2.0"}` in `rebar.config`
3. **Logging changes**: If you were using `error_logger` in your code, migrate to `logger`
4. **GenPNet patterns**: Pattern state behavior changed slightly - state is now maintained in process dictionary

### Known Issues

1. **bcrypt OTP 28 compatibility**: May require workaround for OTP 28.0
2. **Test coverage**: Overall coverage is ~15%, targeting 70%+ for 1.0.0
3. **Dialyzer**: Has warnings that need to be addressed
4. **Test failures**: 11 tests failing due to gen_server setup issues

## [0.1.10] - Previous Release

### Added
- Initial YAWL workflow patterns implementation
- Stateless execution support
- Persistence layer with Mnesia

### Known Issues
- GenPNet interface breakage (fixed in 0.2.0)
- Deprecated code:lib_dir/2 usage (fixed in 0.2.0)
- Incorrect OTP version documentation (fixed in 0.2.0)

---

[0.2.0]: https://github.com/joergen7/cre/compare/v0.1.10...v0.2.0
[0.1.10]: https://github.com/joergen7/cre/releases/tag/v0.1.10
