#!/usr/bin/env bash
# Run MAPE-K self-play workflow creation with Z.AI LLM.
# LLM generates workflows → execute → monitor → analyze → plan → iterate.
# Requires ZAI_API_KEY.
# Usage: ZAI_API_KEY=your_key ./scripts/run_mapek_selfplay.sh
#        MAPEK_GOAL="Create a review workflow" ZAI_API_KEY=key ./scripts/run_mapek_selfplay.sh
#        MAPEK_MAX_ITER=3 ZAI_API_KEY=key ./scripts/run_mapek_selfplay.sh
#        ZAI_MODEL=glm-4.7 ZAI_API_KEY=key ./scripts/run_mapek_selfplay.sh
set -e
cd "$(dirname "$0")/.."
if [ -z "$ZAI_API_KEY" ]; then
    echo "ERROR: ZAI_API_KEY not set. Set it and run again."
    exit 1
fi
export ZAI_MODEL="${ZAI_MODEL:-glm-4.7-flash}"
rebar3 shell --eval "run_mapek_selfplay:main([]), halt(0)."
