#!/bin/sh
# Real rebar3 eunit gate for the effect-claim module only.
# The repo-wide `rebar3 eunit` does not compile (pre-existing syntax errors in other modules);
# this builds a minimal rebar3 project containing just src/core/cre_effect_claim.erl and
# test/effect_claim/*.erl and runs `rebar3 eunit` in it.
set -eu
ROOT=$(cd "$(dirname "$0")/.." && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/src" "$T/test"
cp "$ROOT/src/core/cre_effect_claim.erl" "$T/src/"
cp "$ROOT"/test/effect_claim/*.erl "$T/test/"
cat > "$T/src/cre_effect_claim_gate.app.src" <<'APP'
{application, cre_effect_claim_gate,
 [{description, "effect-claim gate"}, {vsn, "0.0.0"}, {registered, []},
  {applications, [kernel, stdlib, crypto]}]}.
APP
echo '{erl_opts, [debug_info]}.' > "$T/rebar.config"
cd "$T" && rebar3 eunit
