#!/usr/bin/env bash
# Run AGI Symposium simulation with Z.AI LLMs simulating human participants.
# Prints full OTEL script: LLM communication, voting, pattern spans.
# Requires ZAI_API_KEY. Usage: ZAI_API_KEY=your_key ./scripts/run_agi_symposium_zai.sh
set -e
cd "$(dirname "$0")/.."
if [ -z "$ZAI_API_KEY" ]; then
    echo "ERROR: ZAI_API_KEY not set. Set it and run again."
    exit 1
fi
export ZAI_MODEL="${ZAI_MODEL:-glm-4.7-flash}"
echo "=== AGI Symposium + Z.AI LLM Simulation (OTEL script) ==="
echo "Model: ${ZAI_MODEL}"
echo "Participants: Program Chair, Reviewer, Ops Lead, Venue Lead, Press Lead"
echo "Flow: 5 sequential tasks - each role makes a Go/No-Go decision via Z.AI"
echo "OTEL: workflow_start, llm_request, llm_vote, pattern_span per task"
echo ""
OUTPUT=$(rebar3 ct --suite=agi_symposium_simulation_SUITE --case=test_zai_backed_run 2>&1)
echo "$OUTPUT"
if echo "$OUTPUT" | grep -q "Skipped.*test_zai_backed_run"; then
    echo "ERROR: Symposium test was skipped (ZAI_API_KEY may not be active)."
    exit 1
fi
