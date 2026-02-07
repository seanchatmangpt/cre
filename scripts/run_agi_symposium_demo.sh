#!/usr/bin/env bash
# Run AGI Symposium demo with clean output only (no rebar3/CT noise).
# Usage: ZAI_API_KEY=your_key ./scripts/run_agi_symposium_demo.sh
#        ./scripts/run_agi_symposium_demo.sh --dry-run  (no Z.AI, fast)
#        ./scripts/run_agi_symposium_demo.sh --omega --transcript  (Swarm Turing Test mode)
set -e
cd "$(dirname "$0")/.."
DRY_RUN=false
WITH_DASHBOARD=false
OMEGA=false
TRANSCRIPT=false
for arg in "$@"; do
    case "$arg" in
        --dry-run) DRY_RUN=true ;;
        --with-dashboard) WITH_DASHBOARD=true ;;
        --omega) OMEGA=true ;;
        --transcript) TRANSCRIPT=true ;;
    esac
done
if [ "$DRY_RUN" = true ]; then
    export ZAI_API_KEY=""
    export DEMO_DRY_RUN=1
fi
if [ "$OMEGA" = true ]; then
    export DEMO_OMEGA=1
fi
if [ "$WITH_DASHBOARD" = true ]; then
    export DEMO_DASHBOARD=1
fi
if [ "$TRANSCRIPT" = true ]; then
    export DEMO_TRANSCRIPT=1
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
