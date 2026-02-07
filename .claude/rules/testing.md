---
paths:
  - "test/**/*.erl"
---

# Testing Rules

- Tests use EUnit. Run with `rebar3 eunit`.
- Common Test suites run with `rebar3 ct`.
- Test files go in `test/`, never in the project root.
- Use `?assertEqual`, `?assertMatch`, `?assertException` macros.
- Each test module should include `-include_lib("eunit/include/eunit.hrl").`
- Name test functions with `_test` or `_test_` suffix.
- Group related tests with `_test_() -> [...]` generator functions.
