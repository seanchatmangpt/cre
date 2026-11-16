---
name: cre-test
description: Run EUnit and Common Test suites. Use after implementing changes to verify correctness.
---

# CRE Test Skill

Run this skill after implementing changes to verify tests pass.

## Steps

1. Compile first:
   - **Docker**: `docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile`
   - **Native**: `rebar3 compile`

2. Run EUnit tests:
   - **Docker**: `docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit`
   - **Native**: `rebar3 eunit`

3. Analyze results:
   - Count passed/failed tests
   - For failures, identify:
     - Test module and function
     - Expected vs actual values
     - Source file and line
     - Suggested fix

4. Optionally run Common Test:
   - **Docker**: `docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 ct`
   - **Native**: `rebar3 ct`

## Expected Output

- Test count summary (passed/failed)
- Detailed failure analysis if any
- Clear pass/fail status

## Related

- See @.claude/rules/testing.md for test conventions
- Use `cre-compile` skill for compilation-only checks
