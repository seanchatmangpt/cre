#!/usr/bin/env bash
# System information and diagnostics.
#
# Usage:
#   ./scripts/debug_system_info.sh
#
set -e
cd "$(dirname "$0")/.."

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(system_info_script).
-export([run/0]).

run() ->
    Stats = cre_debug_advanced:system_stats(),
    Mem = cre_debug_advanced:memory_summary(),
    Sched = cre_debug_advanced:scheduler_stats(),
    
    io:format("System Information~n"),
    io:format("==================~n"),
    io:format("Processes: ~p~n", [maps:get(processes, Stats, 0)]),
    io:format("Ports: ~p~n", [maps:get(ports, Stats, 0)]),
    io:format("ETS tables: ~p~n", [maps:get(ets_tables, Stats, 0)]),
    io:format("Atoms: ~p~n", [maps:get(atoms, Stats, 0)]),
    io:format("~nMemory Summary~n"),
    io:format("===============~n"),
    io:format("Total: ~p bytes~n", [maps:get(total, Mem, 0)]),
    io:format("Processes: ~p bytes~n", [maps:get(processes, Mem, 0)]),
    io:format("System: ~p bytes~n", [maps:get(system, Mem, 0)]),
    io:format("ETS: ~p bytes~n", [maps:get(ets, Mem, 0)]),
    io:format("Binary: ~p bytes~n", [maps:get(binary, Mem, 0)]),
    io:format("~nScheduler Information~n"),
    io:format("=====================~n"),
    io:format("Schedulers: ~p~n", [maps:get(schedulers, Sched, 0)]),
    io:format("Online: ~p~n", [maps:get(schedulers_online, Sched, 0)]),
    io:format("Logical processors: ~p~n", [maps:get(logical_processors, Sched, 0)]).

run().
EOF
