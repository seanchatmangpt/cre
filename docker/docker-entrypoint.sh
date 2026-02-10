#!/bin/bash
# CRE Docker Entry Point Script
# Handles Mnesia clustering, graceful shutdown, and health checks
#
# Environment Variables:
#   CRE_NODE_NAME    - Erlang node name (default: cre)
#   CRE_HOSTNAME     - Node hostname (default: hostname -f)
#   CRE_MODE         - Mode: init|primary|replica (default: init)
#   CRE_CLUSTER_PEERS - Comma-separated list of peer nodes for clustering
#   ERL_MAX_PORTS    - Max ports (default: 65536)
#   ERL_MAX_ETS_TABLES - Max ETS tables (default: 2000)
#   CRE_COOKIE       - Erlang cookie for clustering (default: cre_cookie)

set -eo pipefail

# Color output for better readability
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[CRE ENTRYPOINT]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[CRE ENTRYPOINT]${NC} $*" >&2
}

log_error() {
    echo -e "${RED}[CRE ENTRYPOINT]${NC} $*" >&2
}

# Default values
CRE_NODE_NAME="${CRE_NODE_NAME:-cre}"
CRE_HOSTNAME="${CRE_HOSTNAME:-$(hostname -f)}"
CRE_MODE="${CRE_MODE:-init}"
CRE_CLUSTER_PEERS="${CRE_CLUSTER_PEERS:-}"
CRE_COOKIE="${CRE_COOKIE:-cre_cookie}"
ERL_MAX_PORTS="${ERL_MAX_PORTS:-65536}"
ERL_MAX_ETS_TABLES="${ERL_MAX_ETS_TABLES:-2000}"

# Construct full node name
FULL_NODE_NAME="${CRE_NODE_NAME}@${CRE_HOSTNAME}"
CRE_HOME="/opt/cre"

log_info "Starting CRE node: ${FULL_NODE_NAME}"
log_info "Mode: ${CRE_MODE}"

# Function to wait for peer node to be available
wait_for_peer() {
    local peer="$1"
    local max_wait="${2:-60}"
    local count=0

    log_info "Waiting for peer node: ${peer}"

    while [ "$count" -lt "$max_wait" ]; do
        if "${CRE_HOME}/bin/cre" ping "${peer}" 2>/dev/null; then
            log_info "Peer node ${peer} is available"
            return 0
        fi
        sleep 2
        count=$((count + 2))
    done

    log_warn "Peer node ${peer} not available after ${max_wait}s"
    return 1
}

# Function to join Mnesia cluster
join_cluster() {
    local IFS=','
    read -ra peers <<< "${CRE_CLUSTER_PEERS}"
    local joined=false

    for peer in "${peers[@]}"; do
        # Trim whitespace
        peer=$(echo "$peer" | xargs)

        if [ -z "$peer" ]; then
            continue
        fi

        log_info "Attempting to join cluster with peer: ${peer}"

        # Wait for peer to be available
        if wait_for_peer "$peer" 30; then
            # Execute Mnesia join command via RPC
            "${CRE_HOME}/bin/cre" eval \
                "case mnesia:change_config(extra_db_nodes, ['${peer}']) of
                    {ok, _} -> ok;
                    {error, Reason} -> {error, Reason}
                 end." 2>/dev/null && joined=true

            if $joined; then
                log_info "Successfully joined cluster with ${peer}"
                return 0
            fi
        fi
    done

    if [ -n "${CRE_CLUSTER_PEERS}" ]; then
        log_warn "Could not join any cluster peers, starting in standalone mode"
    fi
    return 0
}

# Function to initialize as primary node
init_primary() {
    log_info "Initializing as primary cluster node"

    # Create Mnesia schema if it doesn't exist
    "${CRE_HOME}/bin/cre" eval \
        "case mnesia:system_info(use_dir) of
            true -> ok;
            false ->
                 case mnesia:create_schema([node()]) of
                     ok -> ok;
                     {error, {already_exists, _}} -> ok
                 end
         end." 2>/dev/null || true

    log_info "Primary node initialized"
}

# Function to initialize as replica node
init_replica() {
    log_info "Initializing as replica node"

    # Join cluster if peers are defined
    if [ -n "${CRE_CLUSTER_PEERS}" ]; then
        join_cluster
    else
        log_warn "No cluster peers defined, starting in standalone mode"
    fi
}

# Function to check if CRE is healthy
health_check() {
    local status
    status=$("${CRE_HOME}/bin/cre" eval "cre_health:check()." 2>/dev/null || echo "error")

    if [ "$status" = "ok" ] || [ "$status" = "\"ok\"" ]; then
        return 0
    else
        return 1
    fi
}

# Function to handle graceful shutdown
graceful_shutdown() {
    log_info "Received shutdown signal, initiating graceful shutdown..."

    # Stop CRE gracefully
    "${CRE_HOME}/bin/cre" stop || true

    # Wait a bit for cleanup
    sleep 5

    log_info "Shutdown complete"
    exit 0
}

# Setup signal handlers
trap graceful_shutdown SIGTERM SIGINT

# Main initialization based on mode
case "${CRE_MODE}" in
    primary|init)
        init_primary
        ;;
    replica)
        init_replica
        ;;
    *)
        log_warn "Unknown mode: ${CRE_MODE}, defaulting to init"
        init_primary
        ;;
esac

# Start CRE
log_info "Starting CRE runtime..."

# Set environment variables for the VM
export ERL_MAX_PORTS
export ERL_MAX_ETS_TABLES

# Execute the CRE command with the node name
# Set the CRE node name via environment variable for the release
export CRE_NODE_NAME="${CRE_NODE_NAME}"

# Get the command to run (default to foreground if not provided)
CRE_CMD="${1:-foreground}"

# The cre release uses its own naming scheme via the vm.args
# Let's pass the command directly to CRE
exec "${CRE_HOME}/bin/cre" "${CRE_CMD}"
