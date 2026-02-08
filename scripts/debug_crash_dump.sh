#!/usr/bin/env bash
# Crash dump analysis script.
#
# Usage:
#   ./scripts/debug_crash_dump.sh [dump_file]
#   ./scripts/debug_crash_dump.sh erl_crash.dump
#
set -e
cd "$(dirname "$0")/.."

DUMP_FILE="${1:-erl_crash.dump}"

if [ ! -f "$DUMP_FILE" ]; then
    echo "Error: Dump file not found: $DUMP_FILE"
    exit 1
fi

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(crash_dump_script).
-export([run/0]).

run() ->
    Analysis = cre_debug_advanced:analyze_crash_dump("$DUMP_FILE"),
    io:format("Crash Dump Analysis:~n"),
    io:format("==================~n"),
    io:format("Total lines: ~p~n", [maps:get(total_lines, Analysis, 0)]),
    io:format("Processes: ~p~n", [maps:get(processes, Analysis, 0)]),
    io:format("Ports: ~p~n", [maps:get(ports, Analysis, 0)]),
    io:format("ETS tables: ~p~n", [maps:get(ets_tables, Analysis, 0)]),
    io:format("Atoms: ~p~n", [maps:get(atoms, Analysis, 0)]),
    io:format("Nodes: ~p~n", [maps:get(nodes, Analysis, 0)]),
    io:format("Loaded modules: ~p~n", [maps:get(loaded_modules, Analysis, 0)]).

run().
EOF
