#!/usr/bin/env bash
# Measure CRE application startup time (BEAM VM + application runtime)
#
# This script measures pure startup time excluding build/compile operations.
# It uses high-resolution timestamps to measure:
#   1. BEAM VM boot time
#   2. Application loading time
#   3. Supervisor tree initialization
#
# Usage:
#   ./scripts/measure_startup_time.sh [iterations]
#
# Example:
#   ./scripts/measure_startup_time.sh 10

set -euo pipefail

ITERATIONS="${1:-5}"
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "============================================"
echo "CRE Startup Time Measurement"
echo "============================================"
echo ""
echo "Measuring BEAM VM + application startup"
echo "Iterations: $ITERATIONS"
echo "Excluding: build tools (rebar3, dialyzer)"
echo ""

cd "$PROJECT_ROOT"

# Ensure compiled first (don't measure compilation)
if [[ ! -f "_build/default/lib/cre/ebin/cre.beam" ]]; then
    echo "ERROR: Project not compiled. Run 'rebar3 compile' first."
    exit 1
fi

# Create measurement script
cat > /tmp/measure_startup.erl <<'EOF'
-module(measure_startup).
-export([run/0]).

run() ->
    % Record startup phases with microsecond precision
    T0 = erlang:monotonic_time(microsecond),

    % Phase 1: Ensure all dependencies started
    T1 = erlang:monotonic_time(microsecond),
    {ok, _} = application:ensure_all_started(cre),
    T2 = erlang:monotonic_time(microsecond),

    % Phase 2: Verify supervisor tree running
    T3 = erlang:monotonic_time(microsecond),
    Pid = whereis(cre_sup),
    true = is_pid(Pid),
    true = is_process_alive(Pid),
    T4 = erlang:monotonic_time(microsecond),

    % Calculate timings (microseconds)
    AppStartTime = T2 - T1,
    VerifyTime = T4 - T3,
    TotalTime = T4 - T0,

    % Output machine-readable format
    io:format("STARTUP_STATS|app_start_us=~p|verify_us=~p|total_us=~p~n",
              [AppStartTime, VerifyTime, TotalTime]),

    % Stop cleanly
    application:stop(cre),
    init:stop().
EOF

# Run measurements
declare -a app_times
declare -a verify_times
declare -a total_times

echo "Running measurements..."
echo ""

for i in $(seq 1 "$ITERATIONS"); do
    echo -n "  Run $i/$ITERATIONS... "

    # Run measurement (capture only STARTUP_STATS line)
    output=$(erl -pa _build/default/lib/*/ebin \
                 -noshell \
                 -s measure_startup run \
                 2>/dev/null | grep "STARTUP_STATS")

    # Parse output
    app_us=$(echo "$output" | sed -n 's/.*app_start_us=\([0-9]*\).*/\1/p')
    verify_us=$(echo "$output" | sed -n 's/.*verify_us=\([0-9]*\).*/\1/p')
    total_us=$(echo "$output" | sed -n 's/.*total_us=\([0-9]*\).*/\1/p')

    app_times+=("$app_us")
    verify_times+=("$verify_us")
    total_times+=("$total_us")

    echo "done (${total_us}μs)"
done

# Calculate statistics
calc_stats() {
    local -n arr=$1
    local sum=0
    local min=${arr[0]}
    local max=${arr[0]}

    for val in "${arr[@]}"; do
        sum=$((sum + val))
        ((val < min)) && min=$val
        ((val > max)) && max=$val
    done

    local mean=$((sum / ${#arr[@]}))
    echo "$mean $min $max"
}

echo ""
echo "============================================"
echo "Results (microseconds)"
echo "============================================"
echo ""

read app_mean app_min app_max < <(calc_stats app_times)
read verify_mean verify_min verify_max < <(calc_stats verify_times)
read total_mean total_min total_max < <(calc_stats total_times)

printf "%-30s %10s %10s %10s\n" "Phase" "Mean" "Min" "Max"
printf "%-30s %10s %10s %10s\n" "-----" "----" "---" "---"
printf "%-30s %10d %10d %10d\n" "Application Start" "$app_mean" "$app_min" "$app_max"
printf "%-30s %10d %10d %10d\n" "Supervisor Verification" "$verify_mean" "$verify_min" "$verify_max"
printf "%-30s %10d %10d %10d\n" "Total Startup" "$total_mean" "$total_min" "$total_max"

echo ""
echo "============================================"
echo "Results (milliseconds)"
echo "============================================"
echo ""

printf "%-30s %10s %10s %10s\n" "Phase" "Mean" "Min" "Max"
printf "%-30s %10s %10s %10s\n" "-----" "----" "---" "---"
printf "%-30s %10.2f %10.2f %10.2f\n" \
    "Application Start" \
    "$(echo "scale=2; $app_mean/1000" | bc)" \
    "$(echo "scale=2; $app_min/1000" | bc)" \
    "$(echo "scale=2; $app_max/1000" | bc)"
printf "%-30s %10.2f %10.2f %10.2f\n" \
    "Supervisor Verification" \
    "$(echo "scale=2; $verify_mean/1000" | bc)" \
    "$(echo "scale=2; $verify_min/1000" | bc)" \
    "$(echo "scale=2; $verify_max/1000" | bc)"
printf "%-30s %10.2f %10.2f %10.2f\n" \
    "Total Startup" \
    "$(echo "scale=2; $total_mean/1000" | bc)" \
    "$(echo "scale=2; $total_min/1000" | bc)" \
    "$(echo "scale=2; $total_max/1000" | bc)"

echo ""
echo "Note: Excludes compilation, build tools, and VM shutdown time"
echo "      Measures only runtime startup (application:start + supervisor tree)"

# Cleanup
rm -f /tmp/measure_startup.erl
