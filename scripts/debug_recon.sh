#!/usr/bin/env bash
# Recon debugging script (if recon is available).
#
# Usage:
#   ./scripts/debug_recon.sh [command]
#   ./scripts/debug_recon.sh top_memory
#   ./scripts/debug_recon.sh top_queue
#
set -e
cd "$(dirname "$0")/.."

COMMAND="${1:-help}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(recon_script).
-export([run/0]).

run() ->
    case code:which(recon) of
        non_existing ->
            io:format("recon not available. Install with: rebar3 deps~n");
        _Path ->
            case '$COMMAND' of
                top_memory ->
                    Top = recon:proc_count(memory, 10),
                    io:format("Top 10 processes by memory:~n"),
                    [io:format("  ~p: ~p bytes~n", [P, M]) || {P, M} <- Top];
                top_queue ->
                    Top = recon:proc_count(message_queue_len, 10),
                    io:format("Top 10 processes by message queue:~n"),
                    [io:format("  ~p: ~p messages~n", [P, Q]) || {P, Q} <- Top];
                bin_leak ->
                    BinInfo = recon:bin_leak(10),
                    io:format("Binary leak analysis:~n"),
                    io:format("~p~n", [BinInfo]);
                help ->
                    io:format("Available commands:~n"),
                    io:format("  top_memory  - Top processes by memory~n"),
                    io:format("  top_queue   - Top processes by message queue~n"),
                    io:format("  bin_leak    - Binary leak analysis~n");
                _ ->
                    io:format("Unknown command: ~p~n", ['$COMMAND'])
            end
    end.

run().
EOF
