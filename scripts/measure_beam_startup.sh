#!/usr/bin/env bash
# Simple BEAM VM startup measurement
# Measures time from erl invocation to code execution
#
# Expected performance:
#   - Claude Code Web (gVisor): 420-440ms (measured with OTP 28.3.1)
#   - Local/Docker: 100-200ms (depending on system)
#   - Slow system: 500ms+ (CPU-bound)
#
# If startup times are >1000ms in Claude Code Web, investigate:
#   1. Check system load: uptime
#   2. Check gVisor resource allocation
#   3. Verify OTP installation via: erl -eval 'erlang:system_info(otp_release)' -noshell

set -euo pipefail

ITERATIONS="${1:-10}"
ERL_BIN="${ERL_BIN:-erl}"

echo "BEAM VM Startup Time Measurement"
echo "=================================="
echo ""

# Simple Erlang measurement script
measure_code='
io:format("STARTED~n"),
halt(0).
'

echo "Measuring $ITERATIONS iterations..."
echo ""

total=0
min=""
max=""

for i in $(seq 1 "$ITERATIONS"); do
    # Use time command to measure elapsed time
    elapsed=$( (time -p $ERL_BIN -noshell -eval "$measure_code" 2>&1) 2>&1 | grep real | awk '{print $2}')

    # Convert to milliseconds
    elapsed_ms=$(echo "$elapsed * 1000" | bc)

    echo "Run $i: ${elapsed_ms}ms"

    # Track min/max
    if [[ -z "$min" ]] || (( $(echo "$elapsed < $min" | bc -l) )); then
        min=$elapsed
    fi
    if [[ -z "$max" ]] || (( $(echo "$elapsed > $max" | bc -l) )); then
        max=$elapsed
    fi

    total=$(echo "$total + $elapsed" | bc)
done

mean=$(echo "scale=3; $total / $ITERATIONS" | bc)
min_ms=$(echo "$min * 1000" | bc)
max_ms=$(echo "$max * 1000" | bc)
mean_ms=$(echo "$mean * 1000" | bc)

echo ""
echo "Results (seconds):"
echo "  Mean: ${mean}s"
echo "  Min:  ${min}s"
echo "  Max:  ${max}s"
echo ""
echo "Results (milliseconds):"
echo "  Mean: ${mean_ms}ms"
echo "  Min:  ${min_ms}ms"
echo "  Max:  ${max_ms}ms"
