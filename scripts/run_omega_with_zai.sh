#!/usr/bin/env bash
# Run AGI Symposium Ω simulation with Z.AI LLM.
# Requires ZAI_API_KEY. Usage: ZAI_API_KEY=your_key ./scripts/run_omega_with_zai.sh
set -e
cd "$(dirname "$0")/.."
if [ -z "$ZAI_API_KEY" ]; then
    echo "ERROR: ZAI_API_KEY not set. Set it and run again."
    exit 1
fi
rebar3 shell --eval "run_omega_with_zai:main([]), halt(0)."
