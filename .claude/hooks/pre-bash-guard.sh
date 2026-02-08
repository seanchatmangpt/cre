#!/usr/bin/env bash
# PreToolUse hook for Bash: block dangerous commands.
# Exit 0 = allow, Exit 2 = block (stderr fed back to Claude).

INPUT=$(cat)
CMD=$(echo "$INPUT" | jq -r '.tool_input.command // empty' 2>/dev/null)

[[ -z "$CMD" ]] && exit 0

# Block --no-verify on git commits
if echo "$CMD" | grep -q '\-\-no-verify'; then
    echo "--no-verify is not allowed. Commit hooks must run." >&2
    exit 2
fi

# Block force push to main/master
if echo "$CMD" | grep -qE 'git\s+push.*--force.*(main|master)'; then
    echo "Force push to main/master is blocked." >&2
    exit 2
fi

# Block destructive rm on project root or home
if echo "$CMD" | grep -qE 'rm\s+(-rf|-fr)\s+(/|~|\.\s*$)'; then
    echo "Destructive rm on critical paths is blocked." >&2
    exit 2
fi

exit 0
