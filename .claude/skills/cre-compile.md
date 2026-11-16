---
name: cre-compile
description: Compile and verify Erlang changes. Run after modifying .erl or .hrl files.
---

# CRE Compile Skill

Run this skill after modifying any `.erl` or `.hrl` files to verify compilation succeeds.

## Steps

1. Check Docker availability:
   ```bash
   docker info >/dev/null 2>&1 && echo "docker" || echo "native"
   ```

2. Run compilation:
   - **Docker**: `docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile`
   - **Native**: `rebar3 compile`

3. Check for warnings and errors

4. If compilation fails:
   - Identify the file and line number
   - Explain the error
   - Suggest a fix

## Expected Output

- Clear pass/fail status
- List of any warnings with file:line references
- Compilation time

## Related

- See @.claude/rules/erlang.md for Erlang conventions
- Use `cre-test` skill to run tests after compilation
