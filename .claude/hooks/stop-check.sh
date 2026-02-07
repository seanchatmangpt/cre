#!/bin/bash
# Stop hook: verify no uncommitted source changes are left behind.
# Returns block decision if there are unstaged .erl changes.

INPUT=$(cat)

# Prevent infinite loops
if echo "$INPUT" | jq -e '.stop_hook_active' >/dev/null 2>&1; then
  exit 0
fi

cd "$CLAUDE_PROJECT_DIR" 2>/dev/null || exit 0

# Check for modified .erl source files not yet committed
CHANGED=$(git diff --name-only 2>/dev/null | grep '\.erl$' || true)
UNTRACKED=$(git ls-files --others --exclude-standard 2>/dev/null | grep '\.erl$' || true)

if [ -n "$CHANGED" ] || [ -n "$UNTRACKED" ]; then
  FILES="${CHANGED}${UNTRACKED}"
  jq -n --arg reason "Uncommitted Erlang source changes: $FILES" \
    '{"decision": "block", "reason": $reason}'
else
  exit 0
fi
