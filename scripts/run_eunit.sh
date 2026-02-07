#!/usr/bin/env bash
# Run EUnit with workaround for intermittent yawl_persistence.beam rename failure.
# Pre-creating the ebin directory before compile avoids the race condition.
set -e
cd "$(dirname "$0")/.."
rm -rf _build/test
mkdir -p _build/test/lib/cre/ebin
rebar3 as test compile
rebar3 eunit "$@"
