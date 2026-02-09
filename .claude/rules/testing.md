---
paths:
  - "test/**/*.erl"
---

# Testing Conventions

## Test Organization
- Unit tests: EUnit (`rebar3 eunit`)
- Integration tests: Common Test (`rebar3 ct`)
- Test files MUST go in `test/`, never in `src/` or project root

## EUnit Standards
- Include `-include_lib("eunit/include/eunit.hrl").`
- Use `?assertEqual`, `?assertMatch`, `?assertException` macros
- Name test functions with `_test` suffix
- Use `_test_() -> [...]` generators for test groups

## Build Before Test
- MUST run `rebar3 compile` before `rebar3 eunit`
- Ensures all modules are compiled and up-to-date
