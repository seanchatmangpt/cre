#!/usr/bin/env bash
# Advanced tracing script for CRE workflows.
#
# Usage:
#   ./scripts/debug_trace.sh [module] [function] [arity]
#   ./scripts/debug_trace.sh gen_yawl step 1
#   ./scripts/debug_trace.sh gen_pnet fire 3
#
set -e
cd "$(dirname "$0")/.."

MODULE="${1:-gen_yawl}"
FUNCTION="${2:-'_'}"
ARITY="${3:-'_'}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(trace_script).
-export([run/0]).

run() ->
    code:ensure_loaded($MODULE),
    dbg:tracer(),
    dbg:p(all, [c, timestamp]),
    dbg:tpl($MODULE, $FUNCTION, $ARITY, x),
    io:format("Tracing ~p:~p/~p~n", [$MODULE, $FUNCTION, $ARITY]),
    io:format("Press Ctrl+C to stop tracing~n"),
    timer:sleep(infinity).

run().
EOF
