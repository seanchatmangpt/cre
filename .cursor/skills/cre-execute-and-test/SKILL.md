---
name: cre-execute-and-test
description: Post-plan execution phase: generate code, compile, fix issues, iterate. Use when implementing a plan or running the build-test-fix loop. Runs doctests and eunit for verification.
---

# CRE Execute and Test

Framework for the execution phase after planning: generate or modify code, then compile and test until everything passes.

## Loop

1. **Generate or edit code** in `src/` or `test/` as required by the plan.
2. **Compile:** `docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile` (or native rebar3 if no Docker).
3. **Run doctests:** `./scripts/run_doctests.sh` (or `docker run ... rebar3 eunit` with doctest modules).
4. **Run EUnit:** `docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit`.
5. **If failures:** Analyze output, fix code or tests, repeat from step 2.

## Verification commands

- Doctests: `./scripts/run_doctests.sh` (uses run_eunit.sh under the hood).
- Full EUnit: `docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit`.
- Single module: `./scripts/run_eunit.sh --module=MODULE`.

## When to use

- After plan mode has produced a plan and you are implementing it
- When the user asks to "execute the plan" or run the build-test-fix cycle
- When iterating on code until tests pass
