#!/usr/bin/env bash
# Profiling script for CRE workflows.
#
# Usage:
#   ./scripts/debug_profile.sh [type] [module]
#   ./scripts/debug_profile.sh eprof gen_yawl
#   ./scripts/debug_profile.sh fprof gen_pnet
#
set -e
cd "$(dirname "$0")/.."

TYPE="${1:-eprof}"
MODULE="${2:-gen_yawl}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(profile_script).
-export([run/0]).

run() ->
    case '$TYPE' of
        eprof ->
            eprof:start(),
            eprof:start_profiling([self()]),
            io:format("Profiling with eprof...~n"),
            timer:sleep(5000),
            eprof:stop_profiling(),
            Analysis = eprof:analyze(),
            io:format("~p~n", [Analysis]),
            eprof:stop();
        fprof ->
            fprof:trace(start),
            io:format("Profiling with fprof...~n"),
            timer:sleep(5000),
            fprof:trace(stop),
            fprof:profile(file, "/tmp/fprof.trace"),
            io:format("Profile written to /tmp/fprof.trace~n");
        _ ->
            io:format("Unknown profiler type: ~p~n", ['$TYPE'])
    end.

run().
EOF
