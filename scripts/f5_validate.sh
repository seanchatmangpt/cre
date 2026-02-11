#!/usr/bin/env bash
# =============================================================================
# F5 Validate - Fortune-5 Line Controller Factory CI Script
# =============================================================================
#
# End-to-end CI validation script for dot commands.
# Runs all quality gates and generates evidence pack.
#
# Usage: ./scripts/f5_validate.sh [options]
#
# Exit codes:
#   0 - All PASS
#   1 - Any FAIL
#   2 - Quality gate failure (andon)
#
# Options:
#   --skip-compile      Skip compilation check
#   --skip-sync         Skip evidence sync
#   --skip-evidence     Skip evidence collection
#   --skip-prove        Skip proof verification
#   --skip-bench        Skip benchmarks
#   --skip-andon        Skip andon gate check
#   --output-dir=<dir>  Evidence pack output directory
#   --verbose           Enable verbose output
#
# =============================================================================

set -euo pipefail

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Default options
SKIP_COMPILE=false
SKIP_SYNC=false
SKIP_EVIDENCE=false
SKIP_PROVE=false
SKIP_BENCH=false
SKIP_ANDON=false
OUTPUT_DIR="${PROJECT_ROOT}/evidence-pack"
VERBOSE=false

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASS_COUNT=0
FAIL_COUNT=0

# =============================================================================
# Helper Functions
# =============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $*"
    ((PASS_COUNT++))
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $*"
    ((FAIL_COUNT++))
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

print_header() {
    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$*${NC}"
    echo -e "${BLUE}========================================${NC}"
}

# =============================================================================
# Command Execution Helpers
# =============================================================================

run_step() {
    local step_name="$1"
    shift
    local cmd=("$@")

    log_info "Running: ${step_name}"

    if [[ "${VERBOSE}" == "true" ]]; then
        "${cmd[@]}" 2>&1 | tee -a "${OUTPUT_DIR}/validation.log"
    else
        "${cmd[@]}" >> "${OUTPUT_DIR}/validation.log" 2>&1
    fi

    local exit_code=$?
    if [[ ${exit_code} -eq 0 ]]; then
        log_pass "${step_name}"
        return 0
    else
        log_fail "${step_name} (exit code: ${exit_code})"
        return 1
    fi
}

run_dot_command() {
    local dot_cmd="$1"
    shift

    # Ensure dot escript is available
    if [[ ! -f "${PROJECT_ROOT}/dot" ]]; then
        log_info "Building dot escript..."
        if ! cd "${PROJECT_ROOT}" && rebar3 as prod escriptize; then
            log_fail "Failed to build dot escript"
            return 1
        fi
        # Copy to project root
        if [[ -f "${PROJECT_ROOT}/_build/prod/bin/dot" ]]; then
            cp "${PROJECT_ROOT}/_build/prod/bin/dot" "${PROJECT_ROOT}/dot"
            chmod +x "${PROJECT_ROOT}/dot"
        else
            log_fail "dot escript not found after build"
            return 1
        fi
    fi

    "${PROJECT_ROOT}/dot" "${dot_cmd}" "$@"
}

# =============================================================================
# Validation Steps
# =============================================================================

step_validate() {
    if [[ "${SKIP_COMPILE}" == "true" ]]; then
        log_warn "Skipping validate step"
        return 0
    fi

    print_header "Step 1: Validate (compile with warnings as errors)"

    cd "${PROJECT_ROOT}"

    # First, compile with rebar3
    if ! run_step "rebar3 compile" rebar3 compile; then
        log_fail "Compilation failed"
        return 1
    fi

    # Run dot validate on all workflow modules
    local found_workflow=false
    for module_file in src/wfnet/patterns/*.erl; do
        if [[ -f "${module_file}" ]]; then
            found_workflow=true
            local module_name=$(basename "${module_file}" .erl)

            if run_dot_command validate "${module_name}" --warnings-as-errors; then
                log_pass "validate ${module_name}"
            else
                log_fail "validate ${module_name}"
                return 1
            fi
        fi
    done

    if [[ "${found_workflow}" == "false" ]]; then
        log_warn "No workflow modules found in src/wfnet/patterns/"
    fi

    return 0
}

step_sync() {
    if [[ "${SKIP_SYNC}" == "true" ]]; then
        log_warn "Skipping sync step"
        return 0
    fi

    print_header "Step 2: Sync Evidence Directory"

    # Ensure evidence directory exists
    mkdir -p "${PROJECT_ROOT}/evidence"

    if run_dot_command sync; then
        log_pass "Evidence sync completed"
        return 0
    else
        log_fail "Evidence sync failed"
        return 1
    fi
}

step_evidence() {
    if [[ "${SKIP_EVIDENCE}" == "true" ]]; then
        log_warn "Skipping evidence collection"
        return 0
    fi

    print_header "Step 3: Collect Evidence Pack"

    # Create output directory
    mkdir -p "${OUTPUT_DIR}"

    if run_dot_command evidence --output="${OUTPUT_DIR}" --format=directory; then
        log_pass "Evidence pack collected to ${OUTPUT_DIR}"

        # Copy validation log to evidence pack
        if [[ -f "${OUTPUT_DIR}/validation.log" ]]; then
            cp "${OUTPUT_DIR}/validation.log" "${OUTPUT_DIR}/validation-output.log"
        fi

        return 0
    else
        log_fail "Evidence collection failed"
        return 1
    fi
}

step_prove() {
    if [[ "${SKIP_PROVE}" == "true" ]]; then
        log_warn "Skipping proof verification"
        return 0
    fi

    print_header "Step 4: Run Proofs"

    # Find a workflow module to prove
    local test_module=""
    for module_file in src/wfnet/patterns/*.erl; do
        if [[ -f "${module_file}" ]]; then
            test_module=$(basename "${module_file}" .erl)
            break
        fi
    done

    if [[ -z "${test_module}" ]]; then
        log_warn "No workflow module found for proof"
        return 0
    fi

    if run_dot_command prove "${test_module}" --all; then
        log_pass "Proof verification passed for ${test_module}"

        # Save proof results to evidence pack
        if [[ -d "${OUTPUT_DIR}" ]]; then
            echo "{\"proof\": \"${test_module}\", \"status\": \"pass\", \"timestamp\": $(date +%s)}" \
                > "${OUTPUT_DIR}/proof-result.json"
        fi

        return 0
    else
        log_fail "Proof verification failed for ${test_module}"

        if [[ -d "${OUTPUT_DIR}" ]]; then
            echo "{\"proof\": \"${test_module}\", \"status\": \"fail\", \"timestamp\": $(date +%s)}" \
                > "${OUTPUT_DIR}/proof-result.json"
        fi

        return 1
    fi
}

step_bench() {
    if [[ "${SKIP_BENCH}" == "true" ]]; then
        log_warn "Skipping benchmarks"
        return 0
    fi

    print_header "Step 5: Run Benchmarks"

    # Run quick benchmark
    if run_dot_command bench --iterations=100 --warmup=5 --save="${OUTPUT_DIR}/benchmarks.json"; then
        log_pass "Benchmarks completed"

        # Check for regressions if baseline exists
        local baseline="${PROJECT_ROOT}/evidence/benchmarks.json"
        if [[ -f "${baseline}" ]]; then
            if run_dot_command bench --baseline="${baseline}" --threshold=10; then
                log_pass "No regression detected"
            else
                log_warn "Regression detected in benchmarks"
            fi
        fi

        return 0
    else
        log_fail "Benchmarks failed"
        return 1
    fi
}

step_andon() {
    if [[ "${SKIP_ANDON}" == "true" ]]; then
        log_warn "Skipping andon gate check"
        return 0
    fi

    print_header "Step 6: Andon Gate Check"

    # Run andon gate
    if run_dot_command andon --all; then
        log_pass "All andon gates passed"
        return 0
    else
        log_fail "Andon gate failed"
        return 1
    fi
}

# =============================================================================
# Quality Summary
# =============================================================================

generate_quality_summary() {
    print_header "Quality Summary"

    echo ""
    echo "Total Steps: $((PASS_COUNT + FAIL_COUNT))"
    echo -e "  ${GREEN}Passed:${NC} ${PASS_COUNT}"
    echo -e "  ${RED}Failed:${NC} ${FAIL_COUNT}"
    echo ""

    # Generate JSON summary
    if [[ -d "${OUTPUT_DIR}" ]]; then
        cat > "${OUTPUT_DIR}/quality-summary.json" <<EOF
{
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "passed": ${PASS_COUNT},
  "failed": ${FAIL_COUNT},
  "total": $((PASS_COUNT + FAIL_COUNT)),
  "status": "$([[ ${FAIL_COUNT} -eq 0 ]] && echo "PASS" || echo "FAIL")"
}
EOF
        log_info "Quality summary written to ${OUTPUT_DIR}/quality-summary.json"
    fi

    # Print evidence pack contents
    if [[ -d "${OUTPUT_DIR}" ]]; then
        echo ""
        log_info "Evidence pack contents:"
        ls -la "${OUTPUT_DIR}" || true
    fi
}

# =============================================================================
# Parse Arguments
# =============================================================================

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --skip-compile)
                SKIP_COMPILE=true
                shift
                ;;
            --skip-sync)
                SKIP_SYNC=true
                shift
                ;;
            --skip-evidence)
                SKIP_EVIDENCE=true
                shift
                ;;
            --skip-prove)
                SKIP_PROVE=true
                shift
                ;;
            --skip-bench)
                SKIP_BENCH=true
                shift
                ;;
            --skip-andon)
                SKIP_ANDON=true
                shift
                ;;
            --output-dir=*)
                OUTPUT_DIR="${1#*=}"
                shift
                ;;
            --verbose|-v)
                VERBOSE=true
                shift
                ;;
            --help|-h)
                cat <<EOF
F5 Validate - Fortune-5 Line Controller Factory CI Script

Usage: $0 [options]

Options:
  --skip-compile      Skip compilation check
  --skip-sync         Skip evidence sync
  --skip-evidence     Skip evidence collection
  --skip-prove        Skip proof verification
  --skip-bench        Skip benchmarks
  --skip-andon        Skip andon gate check
  --output-dir=<dir>  Evidence pack output directory
  --verbose, -v       Enable verbose output
  --help, -h          Show this help message

Exit codes:
  0 - All PASS
  1 - Any FAIL
  2 - Quality gate failure (andon)

Example:
  $0 --verbose --output-dir=./evidence-pack
EOF
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done
}

# =============================================================================
# Main
# =============================================================================

main() {
    parse_args "$@"

    # Create output directory
    mkdir -p "${OUTPUT_DIR}"

    # Initialize validation log
    echo "F5 Validation started at $(date -u)" > "${OUTPUT_DIR}/validation.log"

    print_header "Fortune-5 Line Controller Factory Validation"
    log_info "Output directory: ${OUTPUT_DIR}"
    log_info "Project root: ${PROJECT_ROOT}"

    # Run validation steps
    local overall_exit=0

    step_validate || overall_exit=1
    step_sync || overall_exit=1
    step_evidence || overall_exit=1
    step_prove || overall_exit=1
    step_bench || overall_exit=1

    # Andon gate is final - exit code 2 if failed
    if ! step_andon; then
        overall_exit=2
    fi

    # Generate quality summary
    generate_quality_summary

    # Final result
    echo ""
    if [[ ${overall_exit} -eq 0 ]]; then
        log_pass "=== ALL VALIDATIONS PASSED ==="
    elif [[ ${overall_exit} -eq 1 ]]; then
        log_fail "=== VALIDATION FAILED ==="
    else
        log_fail "=== QUALITY GATE FAILED ==="
    fi

    exit ${overall_exit}
}

# Run main
main "$@"
