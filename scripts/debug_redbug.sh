#!/usr/bin/env bash
# Safe tracing with redbug (if available).
#
# Usage:
#   ./scripts/debug_redbug.sh [module] [function] [count] [time_ms]
#   ./scripts/debug_redbug.sh gen_yawl step 10 5000
#
set -e
cd "$(dirname "$0")/.."

MODULE="${1:-gen_yawl}"
FUNCTION="${2:-'_'}"
COUNT="${3:-10}"
TIME="${4:-5000}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(redbug_script).
-export([run/0]).

run() ->
    case code:which(redbug) of
        non_existing ->
            io:format("redbug not available. Add to rebar.config:~n"),
            io:format("  {deps, [{redbug, \".*\", {git, \"https://github.com/massemanet/redbug.git\"}}]}.~n");
        _Path ->
            code:ensure_loaded($MODULE),
            redbug:start(#{time => $TIME, msgs => $COUNT}),
            redbug:tp($MODULE, $FUNCTION, []),
            io:format("Tracing ~p:~p/~p (max ~p calls, ~p ms)~n", 
                      [$MODULE, $FUNCTION, '_', $COUNT, $TIME]),
            timer:sleep($TIME + 1000),
            redbug:stop(),
            io:format("Tracing stopped~n")
    end.

run().
EOF
