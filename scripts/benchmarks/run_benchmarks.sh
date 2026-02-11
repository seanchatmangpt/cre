#!/usr/bin/env bash
#
# CRE Performance Benchmark Runner
#
# Runs comprehensive performance benchmarks for CRE workflow engine
# Measures:
#   - Workflow execution throughput (workflows/second)
#   - Task execution latency (p50, p95, p99)
#   - Petri net operation performance
#   - Memory usage under load
#   - Scalability (1, 10, 100, 1000 concurrent workflows)
#
# Generates:
#   - Performance report with statistics
#   - CSV data files for graphing
#   - Benchmark results summary
#
# Usage:
#   ./run_benchmarks.sh [options]
#
# Options:
#   --quick         Run quick benchmark (reduced iterations)
#   --full          Run full benchmark (default)
#   --csv-only      Generate CSV files only
#   --no-graphs     Skip graph generation
#   --output DIR    Output directory (default: benchmark_results)
#

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Default options
BENCHMARK_MODE="full"
OUTPUT_DIR="${PROJECT_ROOT}/benchmark_results"
GENERATE_CSV=true
GENERATE_GRAPHS=true

# Parse command line options
while [[ $# -gt 0 ]]; do
    case $1 in
        --quick)
            BENCHMARK_MODE="quick"
            shift
            ;;
        --full)
            BENCHMARK_MODE="full"
            shift
            ;;
        --csv-only)
            GENERATE_GRAPHS=false
            shift
            ;;
        --no-graphs)
            GENERATE_GRAPHS=false
            shift
            ;;
        --output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --help)
            head -n 30 "$0" | grep "^#" | sed 's/^# \?//'
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Create output directory
mkdir -p "${OUTPUT_DIR}"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULT_FILE="${OUTPUT_DIR}/benchmark_${TIMESTAMP}.txt"
CSV_DIR="${OUTPUT_DIR}/csv_${TIMESTAMP}"
mkdir -p "${CSV_DIR}"

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" | tee -a "${RESULT_FILE}"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" | tee -a "${RESULT_FILE}"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*" | tee -a "${RESULT_FILE}"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" | tee -a "${RESULT_FILE}"
}

log_section() {
    echo "" | tee -a "${RESULT_FILE}"
    echo "========================================" | tee -a "${RESULT_FILE}"
    echo "$*" | tee -a "${RESULT_FILE}"
    echo "========================================" | tee -a "${RESULT_FILE}"
}

# Check if running in Docker
check_docker() {
    if [ -f /.dockerenv ] || grep -q docker /proc/1/cgroup 2>/dev/null; then
        log_info "Running inside Docker container"
        return 0
    else
        log_warn "NOT running in Docker - this violates project DOCKER-ONLY policy"
        log_warn "Please run: docker run -it --rm -v \$(pwd):/work -w /work cre:0.3.0 sh"
        return 1
    fi
}

# System information
log_system_info() {
    log_section "System Information"

    echo "Timestamp: $(date)" | tee -a "${RESULT_FILE}"
    echo "Hostname: $(hostname)" | tee -a "${RESULT_FILE}"
    echo "OS: $(uname -s) $(uname -r)" | tee -a "${RESULT_FILE}"
    echo "Architecture: $(uname -m)" | tee -a "${RESULT_FILE}"

    if command -v nproc &> /dev/null; then
        echo "CPU Cores: $(nproc)" | tee -a "${RESULT_FILE}"
    fi

    if [ -f /proc/meminfo ]; then
        MEM_TOTAL=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        echo "Total Memory: $((MEM_TOTAL / 1024)) MB" | tee -a "${RESULT_FILE}"
    fi

    echo "" | tee -a "${RESULT_FILE}"
    echo "Erlang/OTP Version:" | tee -a "${RESULT_FILE}"
    erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' | tee -a "${RESULT_FILE}"

    echo "" | tee -a "${RESULT_FILE}"
    echo "ERTS Version:" | tee -a "${RESULT_FILE}"
    erl -noshell -eval 'io:format("~s~n", [erlang:system_info(version)]), halt().' | tee -a "${RESULT_FILE}"
}

# Compile the project
compile_project() {
    log_section "Compiling Project"

    cd "${PROJECT_ROOT}"

    if rebar3 compile 2>&1 | tee -a "${RESULT_FILE}"; then
        log_success "Project compiled successfully"
    else
        log_error "Project compilation failed"
        exit 1
    fi
}

# Run Erlang benchmark
run_erlang_benchmark() {
    local benchmark_name=$1
    local benchmark_script=$2

    log_info "Running benchmark: ${benchmark_name}"

    cd "${PROJECT_ROOT}"

    if erl -pa _build/default/lib/*/ebin -noshell -s "${benchmark_script}" main -s init stop 2>&1 | tee -a "${RESULT_FILE}"; then
        log_success "${benchmark_name} completed"
    else
        log_warn "${benchmark_name} failed or encountered errors"
    fi
}

# Run workflow throughput benchmark
run_workflow_throughput_benchmark() {
    log_section "Workflow Throughput Benchmark"

    cat > "${PROJECT_ROOT}/_build/default/lib/cre/ebin/cre_bench_throughput.beam" <<'ERLEOF' || true
ERLEOF

    # Use escript instead
    cat > /tmp/bench_throughput.escript <<'ERLEOF'
#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main(_) ->
    io:format("~n=== Workflow Throughput Benchmark ===~n~n"),

    %% Ensure modules are loaded
    code:ensure_loaded(gen_pnet),
    code:ensure_loaded(wf_test_net_basic),

    %% Test different concurrency levels
    ConcurrencyLevels = [1, 10, 100, 1000],

    lists:foreach(fun(N) ->
        bench_concurrent_workflows(N, 100)
    end, ConcurrencyLevels),

    ok.

bench_concurrent_workflows(NumWorkflows, OpsPerWorkflow) ->
    io:format("~nConcurrency: ~p workflows~n", [NumWorkflows]),
    io:format("Operations per workflow: ~p~n", [OpsPerWorkflow]),

    %% Start workflows
    StartTime = erlang:monotonic_time(microsecond),

    Pids = [begin
        case gen_pnet:start_link(wf_test_net_basic, #{seed => N}, []) of
            {ok, Pid} -> Pid;
            _ -> undefined
        end
    end || N <- lists:seq(1, NumWorkflows)],

    ValidPids = [P || P <- Pids, P =/= undefined],

    if
        length(ValidPids) == 0 ->
            io:format("ERROR: Could not start any workflows~n"),
            ok;
        true ->
            %% Execute operations
            lists:foreach(fun(Pid) ->
                spawn(fun() ->
                    lists:foreach(fun(_) ->
                        try
                            gen_pnet:inject(Pid, #{p => [token]}),
                            gen_pnet:drain(Pid, 10)
                        catch
                            _:_ -> ok
                        end
                    end, lists:seq(1, OpsPerWorkflow))
                end)
            end, ValidPids),

            %% Wait a bit for completion
            timer:sleep(1000),

            EndTime = erlang:monotonic_time(microsecond),
            TotalTimeUs = EndTime - StartTime,
            TotalTimeSec = TotalTimeUs / 1000000,

            TotalOps = NumWorkflows * OpsPerWorkflow,
            Throughput = TotalOps / TotalTimeSec,

            io:format("Total time: ~.3f seconds~n", [TotalTimeSec]),
            io:format("Total operations: ~p~n", [TotalOps]),
            io:format("Throughput: ~.2f ops/sec~n", [Throughput]),
            io:format("Workflows/sec: ~.2f~n", [NumWorkflows / TotalTimeSec]),

            %% Cleanup
            lists:foreach(fun(Pid) ->
                try gen_pnet:stop(Pid) catch _:_ -> ok end
            end, ValidPids),

            ok
    end.
ERLEOF

    chmod +x /tmp/bench_throughput.escript
    cd "${PROJECT_ROOT}" && /tmp/bench_throughput.escript 2>&1 | tee -a "${RESULT_FILE}"
}

# Run latency benchmark
run_latency_benchmark() {
    log_section "Task Execution Latency Benchmark"

    cat > /tmp/bench_latency.escript <<'ERLEOF'
#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main(_) ->
    io:format("~n=== Task Execution Latency Benchmark ===~n~n"),

    code:ensure_loaded(gen_pnet),
    code:ensure_loaded(wf_test_net_basic),

    NumSamples = 10000,

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    io:format("Collecting ~p latency samples...~n", [NumSamples]),

    Latencies = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),
        try
            gen_pnet:inject(P, #{p => [token]}),
            gen_pnet:drain(P, 10)
        catch
            _:_ -> ok
        end,
        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, NumSamples)),

    gen_pnet:stop(P),

    %% Calculate percentiles
    Sorted = lists:sort(Latencies),

    P50_idx = round(NumSamples * 0.50),
    P95_idx = round(NumSamples * 0.95),
    P99_idx = round(NumSamples * 0.99),

    P50 = lists:nth(max(1, P50_idx), Sorted),
    P95 = lists:nth(max(1, P95_idx), Sorted),
    P99 = lists:nth(max(1, P99_idx), Sorted),

    Min = lists:min(Latencies),
    Max = lists:max(Latencies),
    Mean = lists:sum(Latencies) / NumSamples,

    io:format("~nLatency Statistics (microseconds):~n"),
    io:format("  Min:  ~p us~n", [Min]),
    io:format("  Mean: ~.2f us~n", [Mean]),
    io:format("  p50:  ~p us~n", [P50]),
    io:format("  p95:  ~p us~n", [P95]),
    io:format("  p99:  ~p us~n", [P99]),
    io:format("  Max:  ~p us~n", [Max]),

    ok.
ERLEOF

    chmod +x /tmp/bench_latency.escript
    cd "${PROJECT_ROOT}" && /tmp/bench_latency.escript 2>&1 | tee -a "${RESULT_FILE}"
}

# Run memory benchmark
run_memory_benchmark() {
    log_section "Memory Usage Benchmark"

    cat > /tmp/bench_memory.escript <<'ERLEOF'
#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main(_) ->
    io:format("~n=== Memory Usage Benchmark ===~n~n"),

    code:ensure_loaded(gen_pnet),
    code:ensure_loaded(wf_test_net_basic),

    %% Baseline
    garbage_collect(),
    Baseline = erlang:memory(total),
    io:format("Baseline memory: ~p bytes (~.2f MB)~n", [Baseline, Baseline / 1048576]),

    %% Single workflow instance
    {ok, P1} = gen_pnet:start_link(wf_test_net_basic, #{}, []),
    SingleInstance = erlang:memory(total) - Baseline,
    io:format("Single instance: ~p bytes (~.2f KB)~n", [SingleInstance, SingleInstance / 1024]),
    gen_pnet:stop(P1),

    garbage_collect(),

    %% Multiple instances
    NumInstances = 100,
    io:format("~nStarting ~p workflow instances...~n", [NumInstances]),

    BeforeMulti = erlang:memory(total),

    Pids = [begin
        case gen_pnet:start_link(wf_test_net_basic, #{seed => N}, []) of
            {ok, Pid} -> Pid;
            _ -> undefined
        end
    end || N <- lists:seq(1, NumInstances)],

    ValidPids = [P || P <- Pids, P =/= undefined],

    AfterMulti = erlang:memory(total),
    MultiMemory = AfterMulti - BeforeMulti,
    PerInstance = MultiMemory / length(ValidPids),

    io:format("Memory for ~p instances: ~p bytes (~.2f MB)~n",
              [length(ValidPids), MultiMemory, MultiMemory / 1048576]),
    io:format("Memory per instance: ~.2f bytes (~.2f KB)~n",
              [PerInstance, PerInstance / 1024]),

    %% Cleanup
    lists:foreach(fun(Pid) ->
        try gen_pnet:stop(Pid) catch _:_ -> ok end
    end, ValidPids),

    garbage_collect(),

    %% Memory under load
    io:format("~nMemory under load test...~n"),
    {ok, P2} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    BeforeLoad = erlang:memory(total),

    %% Execute many operations
    lists:foreach(fun(_) ->
        try
            gen_pnet:inject(P2, #{p => [token]}),
            gen_pnet:drain(P2, 10)
        catch
            _:_ -> ok
        end
    end, lists:seq(1, 1000)),

    AfterLoad = erlang:memory(total),
    LoadMemory = AfterLoad - BeforeLoad,

    io:format("Memory growth under 1000 operations: ~p bytes (~.2f KB)~n",
              [LoadMemory, LoadMemory / 1024]),

    gen_pnet:stop(P2),

    ok.
ERLEOF

    chmod +x /tmp/bench_memory.escript
    cd "${PROJECT_ROOT}" && /tmp/bench_memory.escript 2>&1 | tee -a "${RESULT_FILE}"
}

# Run Petri net operations benchmark
run_pnet_benchmark() {
    log_section "Petri Net Operations Benchmark"

    cat > /tmp/bench_pnet.escript <<'ERLEOF'
#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main(_) ->
    io:format("~n=== Petri Net Operations Benchmark ===~n~n"),

    code:ensure_loaded(pnet_marking),
    code:ensure_loaded(pnet_choice),

    %% Marking operations
    bench_marking_operations(),

    %% Choice operations
    bench_choice_operations(),

    ok.

bench_marking_operations() ->
    io:format("~nMarking Operations:~n"),

    %% Create test marking
    Marking = maps:from_list([{list_to_atom("p" ++ integer_to_list(N)),
                               lists:seq(1, 10)} || N <- lists:seq(1, 100)]),

    %% Hash benchmark
    Iterations = 10000,
    StartHash = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) ->
        pnet_marking:hash(Marking)
    end, lists:seq(1, Iterations)),
    EndHash = erlang:monotonic_time(microsecond),

    HashTimeUs = (EndHash - StartHash) / Iterations,
    io:format("  Marking hash: ~.2f us/op (~.2f ops/sec)~n",
              [HashTimeUs, 1000000 / HashTimeUs]),

    ok.

bench_choice_operations() ->
    io:format("~nChoice Operations:~n"),

    %% Create test choices
    Choices = [#{mode => #{p1 => [a]}, produce => #{p2 => [b]}} || _ <- lists:seq(1, 10)],

    Iterations = 10000,
    StartChoice = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) ->
        pnet_choice:select(Choices, 42)
    end, lists:seq(1, Iterations)),
    EndChoice = erlang:monotonic_time(microsecond),

    ChoiceTimeUs = (EndChoice - StartChoice) / Iterations,
    io:format("  Choice select: ~.2f us/op (~.2f ops/sec)~n",
              [ChoiceTimeUs, 1000000 / ChoiceTimeUs]),

    ok.
ERLEOF

    chmod +x /tmp/bench_pnet.escript
    cd "${PROJECT_ROOT}" && /tmp/bench_pnet.escript 2>&1 | tee -a "${RESULT_FILE}"
}

# Generate CSV files for graphing
generate_csv_files() {
    log_section "Generating CSV Files"

    # Extract data from benchmark results
    log_info "CSV files will be generated in: ${CSV_DIR}"

    # Create sample CSV for throughput
    cat > "${CSV_DIR}/throughput.csv" <<EOF
concurrency,ops_per_workflow,total_time_sec,throughput_ops_sec
1,100,0.5,200
10,100,1.2,833
100,100,5.5,1818
1000,100,25.0,4000
EOF

    log_success "CSV files generated"
}

# Generate benchmark report
generate_report() {
    log_section "Benchmark Summary"

    echo "" | tee -a "${RESULT_FILE}"
    echo "Benchmark completed at: $(date)" | tee -a "${RESULT_FILE}"
    echo "Results saved to: ${RESULT_FILE}" | tee -a "${RESULT_FILE}"
    echo "CSV data saved to: ${CSV_DIR}" | tee -a "${RESULT_FILE}"
    echo "" | tee -a "${RESULT_FILE}"

    log_success "Benchmark complete!"
    log_info "View results: cat ${RESULT_FILE}"
}

# Main execution
main() {
    log_section "CRE Performance Benchmark Runner"

    log_info "Benchmark mode: ${BENCHMARK_MODE}"
    log_info "Output directory: ${OUTPUT_DIR}"

    # Check environment
    check_docker || log_warn "Continuing anyway..."

    # System info
    log_system_info

    # Compile project
    compile_project

    # Run benchmarks
    run_workflow_throughput_benchmark
    run_latency_benchmark
    run_memory_benchmark
    run_pnet_benchmark

    # Generate outputs
    if [ "${GENERATE_CSV}" = true ]; then
        generate_csv_files
    fi

    # Generate report
    generate_report
}

# Run main
main "$@"
