---
paths:
  - "test/**/*.erl"
---

# Testing Rules

- Unit tests use EUnit. Run with `rebar3 eunit`.
- Integration tests use Common Test. Run with `rebar3 ct`.
- Test files MUST go in `test/`, never in project root or `src/`.
- MUST include `-include_lib("eunit/include/eunit.hrl").` in test modules.
- Use `?assertEqual`, `?assertMatch`, `?assertException` macros for assertions.
- Name test functions with `_test` suffix or use `_test_() -> [...]` generators for groups.
- MUST run `rebar3 compile` before `rebar3 eunit` to verify compilation.
