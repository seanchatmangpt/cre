#!/bin/bash
# CRE GKE Pre-Stop Hook Script
# Triggers graceful shutdown before Kubernetes terminates the pod
#
# This script is called by Kubernetes preStop hook to initiate graceful
# shutdown of CRE before SIGTERM is sent. This ensures:
# - Active workflows complete
# - Mnesia checkpoint is saved
# - Cluster peers are notified
# - Network connections close cleanly
#
# Environment Variables:
#   CRE_NODE_NAME    - Erlang node name (default: cre)
#   CRE_HOSTNAME     - Node hostname (default: hostname -f)
#   CRE_SHUTDOWN_PORT - HTTP port for shutdown API (default: 8080)
#   CRE_SHUTDOWN_TIMEOUT - Max wait time for shutdown (default: 25s)
#   CRE_COOKIE       - Erlang cookie for clustering (default: cre_cookie)
#
# Exit Codes:
#   0 - Shutdown initiated successfully
#   1 - Shutdown failed
#   2 - Node already stopped
#   3 - Timeout waiting for shutdown

set -eo pipefail

# Color output for better readability
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[CRE PRE-STOP]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[CRE PRE-STOP]${NC} $*" >&2
}

log_error() {
    echo -e "${RED}[CRE PRE-STOP]${NC} $*" >&2
}

# Configuration
CRE_NODE_NAME="${CRE_NODE_NAME:-cre}"
CRE_HOSTNAME="${CRE_HOSTNAME:-$(hostname -f)}"
CRE_SHUTDOWN_PORT="${CRE_SHUTDOWN_PORT:-8080}"
CRE_SHUTDOWN_TIMEOUT="${CRE_SHUTDOWN_TIMEOUT:-25}"
CRE_COOKIE="${CRE_COOKIE:-cre_cookie}"
CRE_HOME="/opt/cre"

# Construct full node name
FULL_NODE_NAME="${CRE_NODE_NAME}@${CRE_HOSTNAME}"

log_info "Initiating graceful shutdown for ${FULL_NODE_NAME}"
log_info "Timeout: ${CRE_SHUTDOWN_TIMEOUT}s"

# Function to check if CRE is running
is_cre_running() {
    "${CRE_HOME}/bin/cre" ping "${FULL_NODE_NAME}" >/dev/null 2>&1
}

# Function to trigger shutdown via HTTP API
trigger_shutdown_http() {
    local timeout="${1:-5}"

    log_info "Triggering shutdown via HTTP API on port ${CRE_SHUTDOWN_PORT}"

    # Try HTTP shutdown endpoint first
    local response
    response=$(curl -s -o /dev/null -w "%{http_code}" \
        -X POST \
        -H "Content-Type: application/json" \
        -d "{\"reason\":\"gke_preempt\",\"timeout\":${CRE_SHUTDOWN_TIMEOUT}000}" \
        "http://localhost:${CRE_SHUTDOWN_PORT}/api/shutdown" \
        --connect-timeout "${timeout}" \
        --max-time "${timeout}" \
        2>/dev/null || echo "000")

    if [[ "${response}" == "200" ]] || [[ "${response}" == "202" ]]; then
        log_info "Shutdown triggered successfully via HTTP"
        return 0
    else
        log_warn "HTTP shutdown returned status ${response}"
        return 1
    fi
}

# Function to trigger shutdown via Erlang RPC
trigger_shutdown_rpc() {
    log_info "Triggering shutdown via Erlang RPC"

    # Use erl_call to trigger shutdown
    "${CRE_HOME}/bin/cre" eval \
        "case cre_graceful_shutdown:initiate_shutdown(${CRE_SHUTDOWN_TIMEOUT}000, gke_preemption) of
            ok -> ok;
            {error, Reason} -> {error, Reason}
         end." 2>/dev/null || true

    log_info "Shutdown RPC sent"
}

# Function to wait for active workflows to complete
wait_for_workflows() {
    local timeout="${1}"

    log_info "Waiting for active workflows to complete (max ${timeout}s)"

    local elapsed=0
    local interval=2

    while [[ ${elapsed} -lt ${timeout} ]]; do
        # Check active workflow count
        local active_count
        active_count=$("${CRE_HOME}/bin/cre" eval \
            "cre_graceful_shutdown:get_active_workflow_count()." 2>/dev/null || echo "0")

        # Extract number from output (handle various erlang output formats)
        active_count=$(echo "${active_count}" | grep -oE '[0-9]+' | head -1 || echo "0")

        log_info "Active workflows: ${active_count}"

        if [[ "${active_count}" -eq 0 ]]; then
            log_info "No active workflows remaining"
            return 0
        fi

        sleep "${interval}"
        elapsed=$((elapsed + interval))
    done

    log_warn "Timeout reached with ${active_count} active workflows"
    return 1
}

# Function to verify shutdown status
verify_shutdown_status() {
    log_info "Verifying shutdown status"

    local status
    status=$("${CRE_HOME}/bin/cre" eval \
        "cre_graceful_shutdown:get_shutdown_state()." 2>/dev/null || echo "error")

    log_info "Shutdown state: ${status}"
}

# Main shutdown flow
main() {
    # Check if CRE is running
    if ! is_cre_running; then
        log_warn "CRE node ${FULL_NODE_NAME} is not running"
        exit 2
    fi

    # Try HTTP shutdown first (preferred method)
    if trigger_shutdown_http 3; then
        # Wait for shutdown to complete
        wait_for_workflows "${CRE_SHUTDOWN_TIMEOUT}"
        exit 0
    fi

    # Fallback to RPC method
    trigger_shutdown_rpc
    verify_shutdown_status

    # Wait for workflows to complete
    wait_for_workflows "${CRE_SHUTDOWN_TIMEOUT}"

    log_info "Pre-stop hook completed successfully"
    exit 0
}

# Run main function
main "$@"
