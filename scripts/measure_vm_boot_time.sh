#!/usr/bin/env bash
# Measure pure BEAM VM boot time (no application startup)
#
# This measures the time from process creation to BEAM runtime ready,
# excluding any application logic, compilation, or build tools.
#
# Usage: ./scripts/measure_vm_boot_time.sh [iterations]

set -euo pipefail

ITERATIONS="${1:-10}"
ERL_BIN="${ERL_BIN:-erl}"

echo "============================================"
echo "BEAM VM Boot Time Measurement"
echo "============================================"
echo ""
echo "Measuring: Pure VM initialization time"
echo "Excluding: Application startup, compilation, build tools"
echo "Iterations: $ITERATIONS"
echo "Erlang: $($ERL_BIN -version 2>&1)"
echo ""

# Create measurement module
cat > /tmp/vm_boot_measure.erl <<'EOF'
-module(vm_boot_measure).
-export([main/1]).

main([]) ->
    % At this point, VM is already booted and this module is loaded
    % Measure time for a complete VM boot cycle by spawning new VM
    StartMicro = erlang:system_time(microsecond),

    % Record VM initialization statistics
    {ReductionsTotal, _} = erlang:statistics(runtime),
    ProcessCount = erlang:system_info(process_count),
    MemoryTotal = erlang:memory(total),

    EndMicro = erlang:system_time(microsecond),
    ElapsedMicro = EndMicro - StartMicro,

    % Output stats
    io:format("BOOT_TIME_US=~p~n", [ElapsedMicro]),
    io:format("REDUCTIONS=~p~n", [ReductionsTotal]),
    io:format("PROCESSES=~p~n", [ProcessCount]),
    io:format("MEMORY_BYTES=~p~n", [MemoryTotal]),

    halt(0).
EOF

# Compile measurement module
$ERL_BIN -noshell -eval "compile:file('/tmp/vm_boot_measure.erl', [{outdir, '/tmp'}]), halt()." 2>/dev/null

echo "Running measurements..."
echo ""

declare -a boot_times
declare -a process_counts
declare -a memory_sizes

for i in $(seq 1 "$ITERATIONS"); do
    echo -n "  Run $i/$ITERATIONS... "

    # Measure total elapsed time including VM boot
    start_ns=$(date +%s%N)

    output=$($ERL_BIN -pa /tmp -noshell -s vm_boot_measure main 2>/dev/null)

    end_ns=$(date +%s%N)
    total_us=$(( (end_ns - start_ns) / 1000 ))

    # Parse output
    boot_us=$(echo "$output" | grep "BOOT_TIME_US" | cut -d'=' -f2)
    processes=$(echo "$output" | grep "PROCESSES" | cut -d'=' -f2)
    memory=$(echo "$output" | grep "MEMORY_BYTES" | cut -d'=' -f2)

    boot_times+=("$total_us")
    process_counts+=("$processes")
    memory_sizes+=("$memory")

    echo "done (${total_us}μs)"
done

# Calculate statistics
calc_mean() {
    local -n arr=$1
    local sum=0
    for val in "${arr[@]}"; do
        sum=$((sum + val))
    done
    echo $((sum / ${#arr[@]}))
}

calc_min() {
    local -n arr=$1
    local min=${arr[0]}
    for val in "${arr[@]}"; do
        ((val < min)) && min=$val
    done
    echo $min
}

calc_max() {
    local -n arr=$1
    local max=${arr[0]}
    for val in "${arr[@]}"; do
        ((val > max)) && max=$val
    done
    echo $max
}

echo ""
echo "============================================"
echo "Results"
echo "============================================"
echo ""

boot_mean=$(calc_mean boot_times)
boot_min=$(calc_min boot_times)
boot_max=$(calc_max boot_times)

proc_mean=$(calc_mean process_counts)
mem_mean=$(calc_mean memory_sizes)

printf "Metric                          Mean       Min        Max\n"
printf "═══════════════════════════════════════════════════════════\n"
printf "VM Boot Time (μs)          %9d  %9d  %9d\n" "$boot_mean" "$boot_min" "$boot_max"
printf "VM Boot Time (ms)          %9.2f  %9.2f  %9.2f\n" \
    "$(echo "scale=2; $boot_mean/1000" | bc)" \
    "$(echo "scale=2; $boot_min/1000" | bc)" \
    "$(echo "scale=2; $boot_max/1000" | bc)"
printf "\n"
printf "Initial Process Count      %9d\n" "$proc_mean"
printf "Initial Memory (bytes)     %9d\n" "$mem_mean"
printf "Initial Memory (MB)        %9.2f\n" "$(echo "scale=2; $mem_mean/1048576" | bc)"

echo ""
echo "Note: This measures pure VM boot time from process creation"
echo "      to runtime ready. It does NOT include:"
echo "      - Application initialization (cre:start/2)"
echo "      - Supervisor tree startup"
echo "      - Cowboy web server startup"
echo "      - Config initialization"
echo ""
echo "For full application startup time, use measure_startup_time.sh"

# Cleanup
rm -f /tmp/vm_boot_measure.erl /tmp/vm_boot_measure.beam
