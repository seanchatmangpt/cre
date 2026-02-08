#!/usr/bin/env bash
# System monitoring script for CRE workflows.
#
# Usage:
#   ./scripts/debug_monitor.sh [interval_ms]
#   ./scripts/debug_monitor.sh 1000
#
set -e
cd "$(dirname "$0")/.."

INTERVAL="${1:-1000}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(monitor_script).
-export([run/0]).

run() ->
    cre_monitor:start_link(#{interval => $INTERVAL}),
    cre_monitor:start_monitoring(#{interval => $INTERVAL}),
    io:format("Monitoring started (interval: ~p ms)~n", [$INTERVAL]),
    io:format("Press Ctrl+C to stop~n"),
    timer:sleep(infinity).

run().
EOF
