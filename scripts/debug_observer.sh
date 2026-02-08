#!/usr/bin/env bash
# Start Observer GUI for system inspection.
#
# Usage:
#   ./scripts/debug_observer.sh
#   ./scripts/debug_observer.sh [node@host]
#
set -e
cd "$(dirname "$0")/.."

NODE="${1:-}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

if [ -z "$NODE" ]; then
    erl -pa $PA_ARGS -eval "observer:start()." -s init stop
else
    erl -pa $PA_ARGS -eval "observer:start(), observer:start('$NODE')." -s init stop
fi
