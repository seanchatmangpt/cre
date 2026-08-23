#!/usr/bin/env bash
# PostToolUse:Bash validator hook
# Validates bash command execution and provides helpful feedback
# Exit 0 = success, Exit 2 = error (with stderr message)

INPUT=$(cat)

# Extract command and exit code from tool result
CMD=$(echo "$INPUT" | jq -r '.tool_input.command // empty' 2>/dev/null)
EXIT_CODE=$(echo "$INPUT" | jq -r '.tool_result.exit_code // 0' 2>/dev/null)
OUTPUT=$(echo "$INPUT" | jq -r '.tool_result.output // empty' 2>/dev/null)

# If command is empty, nothing to validate
[[ -z "$CMD" ]] && exit 0

# Check if command failed (non-zero exit code)
if [[ "$EXIT_CODE" -ne 0 ]]; then
    # Some failures are expected (e.g., grep with no matches, test assertions)
    # Only warn on unexpected failures
    case "$CMD" in
        grep*|git\ diff*|git\ status*|test\ -f*)
            # These commands have exit codes that don't indicate failure
            exit 0
            ;;
        *)
            # Log but don't block - let user see the output
            if echo "$OUTPUT" | grep -qi "error\|failed"; then
                echo "Bash command exited with code $EXIT_CODE: $CMD" >&2
            fi
            ;;
    esac
fi

# Warn if rebar3 compilation or tests are skipped
if echo "$CMD" | grep -q "rebar3"; then
    if echo "$OUTPUT" | grep -q "Project build skipped"; then
        echo "WARNING: rebar3 build was skipped. Check for dependency issues." >&2
    fi
fi

# Warn about potential Docker-only rule violations
# (host system modifications detected)
if [[ "$CMD" =~ \.(sh|bash)$ ]] && ! echo "$CMD" | grep -q "docker"; then
    # Only warn if this looks like a script that might be doing system-level work
    if echo "$CMD" | grep -qE "(install|setup|configure)" && ! echo "$CMD" | grep -q "/scripts/"; then
        echo "NOTE: Remember that per CLAUDE.md, significant work should be done in Docker containers." >&2
    fi
fi

exit 0
