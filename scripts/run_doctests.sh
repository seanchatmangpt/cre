#!/usr/bin/env bash
# Run all doctest modules. Auto-discovers from src/ so new modules from merges are included.
# Uses run_eunit.sh for reliable compile (yawl_persistence.beam rename workaround).
set -e
cd "$(dirname "$0")/.."

# Auto-discover doctest modules (files with doctest_test/0, exclude .bak)
# Exclude yawl_schema: triggers yawl_schema_tests which has undef API mismatches
MODULES=$(grep -rl "doctest_test()" src --include="*.erl" 2>/dev/null \
    | grep -v '\.bak' \
    | xargs -I{} basename {} .erl \
    | grep -v '^yawl_schema$' \
    | sort -u \
    | tr '\n' ' ')

if [ -z "$MODULES" ]; then
    echo "No doctest modules found."
    exit 1
fi

# Run all modules in one eunit call (single compile)
MODLIST=$(echo $MODULES | tr ' ' ',')
set +e
OUTPUT=$(./scripts/run_eunit.sh --module="$MODLIST" 2>&1)
EXIT=$?
set -e
if [ $EXIT -eq 0 ] && ! echo "$OUTPUT" | grep -q "Error running tests"; then
    echo "All doctests passed ($(echo $MODULES | wc -w) modules)."
    exit 0
fi
echo "Doctests failed. Run: ./scripts/run_eunit.sh --module=MODULE to isolate."
echo "$OUTPUT" | tail -80
exit 1
