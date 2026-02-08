#!/usr/bin/env bash
# Run Omega demo with debug output (marking, enabled transitions, block analysis).
# When blocked, dumps cre_debug diagnostic to stderr.
#
# Usage: ./scripts/debug_omega.sh
#        ./scripts/debug_omega.sh --dry-run
set -e
cd "$(dirname "$0")/.."
DRY_RUN=false
for arg in "$@"; do
    case "$arg" in
        --dry-run) DRY_RUN=true ;;
    esac
done
export DEMO_OMEGA=1
export DEMO_OMEGA_DEBUG=1
if [ "$DRY_RUN" = true ]; then
    export ZAI_API_KEY=""
    export DEMO_DRY_RUN=1
fi
rebar3 as test compile >/dev/null 2>&1
rebar3 ct --suite=agi_symposium_simulation_SUITE --case=test_simulator_run >/dev/null 2>&1 || true
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done
CRE_TEST_EBIN="_build/test/lib/cre/test"
PA_ARGS="$PA_ARGS -pa $CRE_TEST_EBIN"
exec erl -noshell $PA_ARGS -run demo_runner main 2>&1
