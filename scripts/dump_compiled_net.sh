#!/usr/bin/env bash
# Dump Omega compiled nets to .erl files for inspection.
# Usage: ./scripts/dump_compiled_net.sh [output_dir]
#        Default: /tmp/omega_compiled_dump
set -e
cd "$(dirname "$0")/.."
OUTPUT_DIR="${1:-/tmp/omega_compiled_dump}"
rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done
CRE_TEST_EBIN="_build/test/lib/cre/test"
PA_ARGS="$PA_ARGS -pa $CRE_TEST_EBIN"
exec erl -noshell $PA_ARGS -eval "dump_compiled_net:main([\"$OUTPUT_DIR\"]), halt(0)." 2>&1
