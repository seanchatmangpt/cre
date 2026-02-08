#!/usr/bin/env bash
# Generate flame graph with eflame (if available).
#
# Usage:
#   ./scripts/debug_eflame.sh [output_file]
#   ./scripts/debug_eflame.sh /tmp/flame.txt
#
set -e
cd "$(dirname "$0")/.."

OUTPUT="${1:-/tmp/flame.txt}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(eflame_script).
-export([run/0]).

run() ->
    case code:which(eflame) of
        non_existing ->
            io:format("eflame not available. Add to rebar.config:~n"),
            io:format("  {deps, [{eflame, \".*\", {git, \"https://github.com/proger/eflame.git\"}}]}.~n");
        _Path ->
            eflame:start(),
            Ref = eflame:apply(fun() ->
                timer:sleep(5000)
            end),
            eflame:to_file(Ref, "$OUTPUT"),
            io:format("Flame graph written to $OUTPUT~n"),
            io:format("Generate SVG with: ~/bin/stackcollapse-erl.pl $OUTPUT | flamegraph.pl > flame.svg~n")
    end.

run().
EOF
