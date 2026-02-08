#!/usr/bin/env bash
# Stop hook: block session exit if uncommitted Erlang source changes exist.
# Uses exit code 2 + stderr to feed feedback back to Claude.

INPUT=$(cat)

# Prevent infinite loop when stop hook triggers another stop
if echo "$INPUT" | jq -r '.stop_hook_active // false' 2>/dev/null | grep -q "true"; then
    exit 0
fi

cd "${CLAUDE_PROJECT_DIR:-$(pwd)}" 2>/dev/null || exit 0

# Check for modified or untracked .erl files
CHANGED=$(git diff --name-only 2>/dev/null | grep '\.erl$' || true)
UNTRACKED=$(git ls-files --others --exclude-standard 2>/dev/null | grep '\.erl$' || true)

if [[ -n "$CHANGED" || -n "$UNTRACKED" ]]; then
    ALL_FILES=$(printf '%s\n' $CHANGED $UNTRACKED | sort -u | head -20)
    echo "Uncommitted Erlang source changes detected. Please commit or review before ending session: $ALL_FILES" >&2
    exit 2
fi

exit 0
