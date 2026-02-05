#!/bin/bash
#
# CRE YAWL Approval Worker
#
# This script polls for pending approval requests and invokes Claude Code
# headless mode for approval decision-making.
#
# Usage:
#   ./approval_worker.sh [--poll-interval SECONDS] [--claude-cmd PATH]
#
# Environment Variables:
#   CRE_APPROVAL_POLL_INTERVAL - Seconds between polls (default: 5)
#   CRE_NODE - Erlang node name (default: cre@localhost)
#   CRE_COOKIE - Erlang cookie for authentication
#   CLAUDE_CMD - Path to Claude CLI (default: claude)
#

set -euo pipefail

# Default configuration
POLL_INTERVAL=${CRE_APPROVAL_POLL_INTERVAL:-5}
CRE_NODE=${CRE_NODE:-"cre@localhost"}
CLAUDE_CMD=${CLAUDE_CMD:-"claude"}
LOG_FILE=${CRE_APPROVAL_LOG_FILE:-"/tmp/cre_approval_worker.log"}
WORKER_ID="worker_$$"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') [$WORKER_ID] $*" | tee -a "$LOG_FILE"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $(date '+%Y-%m-%d %H:%M:%S') [$WORKER_ID] $*" | tee -a "$LOG_FILE"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') [$WORKER_ID] $*" | tee -a "$LOG_FILE"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') [$WORKER_ID] $*" | tee -a "$LOG_FILE"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --poll-interval)
            POLL_INTERVAL="$2"
            shift 2
            ;;
        --claude-cmd)
            CLAUDE_CMD="$2"
            shift 2
            ;;
        --node)
            CRE_NODE="$2"
            shift 2
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --poll-interval SECONDS  Seconds between polls (default: 5)"
            echo "  --claude-cmd PATH        Path to Claude CLI (default: claude)"
            echo "  --node NAME              Erlang node name (default: cre@localhost)"
            echo "  --help                   Show this help message"
            echo ""
            echo "Environment Variables:"
            echo "  CRE_APPROVAL_POLL_INTERVAL  Poll interval in seconds"
            echo "  CRE_NODE                    Erlang node name"
            echo "  CRE_COOKIE                  Erlang cookie"
            echo "  CLAUDE_CMD                  Claude CLI path"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check if Claude CLI is available
if ! command -v "$CLAUDE_CMD" &> /dev/null; then
    log_error "Claude CLI not found at: $CLAUDE_CMD"
    log_info "Install Claude Code or set CLAUDE_CMD to the correct path"
    exit 1
fi

log_info "Starting approval worker"
log_info "Poll interval: ${POLL_INTERVAL}s"
log_info "CRE node: ${CRE_NODE}"
log_info "Claude command: ${CLAUDE_CMD}"

# Signal handler for graceful shutdown
cleanup() {
    log_info "Shutting down approval worker"
    exit 0
}

trap cleanup SIGINT SIGTERM

# Function to call Erlang node
erpc_call() {
    local module=$1
    local function=$2
    local args=$3

    if [ -n "${CRE_COOKIE:-}" ]; then
        erpc -c "$CRE_COOKIE" -n "$CRE_NODE" call "$module" "$function" "$args"
    else
        erpc -n "$CRE_NODE" call "$module" "$function" "$args"
    fi
}

# Function to check if CRE node is available
check_node() {
    erpc_call "erlang" "is_alive" "[]" &> /dev/null
}

# Main polling loop
iteration=0
while true; do
    iteration=$((iteration + 1))

    # Check if CRE node is available
    if ! check_node; then
        log_warning "CRE node not available, waiting..."
        sleep "$POLL_INTERVAL"
        continue
    fi

    # Get pending approvals
    pending=$(erpc_call "yawl_approval" "list_pending" "[]" 2>/dev/null || echo "[]")

    # Parse pending approvals (JSON array format expected)
    if [ "$pending" = "[]" ] || [ -z "$pending" ]; then
        # No pending approvals
        if [ $((iteration % 12)) -eq 0 ]; then
            # Log heartbeat every minute (assuming 5s interval)
            log_info "No pending approvals (heartbeat)"
        fi
    else
        # Process pending approvals
        log_info "Found pending approvals: $pending"

        # Extract checkpoint IDs using jq if available
        if command -v jq &> /dev/null; then
            checkpoint_ids=$(echo "$pending" | jq -r '.[]')

            for checkpoint_id in $checkpoint_ids; do
                log_info "Processing checkpoint: $checkpoint_id"

                # Get checkpoint details
                checkpoint_info=$(erpc_call "yawl_approval" "check_status" "[\"$checkpoint_id\"]" 2>/dev/null)

                if echo "$checkpoint_info" | grep -q '"ok","pending"'; then
                    # Generate approval prompt
                    context=$(erpc_call "yawl_approval" "get_checkpoint_context" "[\"$checkpoint_id\"]" 2>/dev/null || echo "{}")

                    prompt="Review this workflow step for approval:
Checkpoint ID: $checkpoint_id
Context: $context

Please respond with a JSON object:
{
  \"approved\": true or false,
  \"reason\": \"your explanation\"
}

Consider:
1. Is this step safe to execute?
2. Are there any concerns with the context?
3. Should this proceed?"

                    # Call Claude for approval decision
                    log_info "Invoking Claude for approval decision..."

                    claude_response=$("$CLAUDE_CMD" -p "$prompt" --output-format json 2>&1)

                    if [ $? -eq 0 ]; then
                        # Parse Claude response
                        if command -v jq &> /dev/null; then
                            approved=$(echo "$claude_response" | jq -r '.approved // false')
                            reason=$(echo "$claude_response" | jq -r '.reason // ""')

                            if [ "$approved" = "true" ]; then
                                log_success "Checkpoint $checkpoint_id APPROVED: $reason"

                                # Approve the checkpoint
                                erpc_call "yawl_approval" "approve" "[\"$checkpoint_id\", \"claude_worker\", \"$reason\"]" &> /dev/null
                            else
                                log_warning "Checkpoint $checkpoint_id DENIED: $reason"

                                # Deny the checkpoint
                                erpc_call "yawl_approval" "deny" "[\"$checkpoint_id\", \"claude_worker\", \"$reason\"]" &> /dev/null
                            fi
                        else
                            log_warning "jq not available, treating response as approval"
                            erpc_call "yawl_approval" "approve" "[\"$checkpoint_id\", \"claude_worker\", \"Approved (jq unavailable for parsing)\"]" &> /dev/null
                        fi
                    else
                        log_error "Claude invocation failed: $claude_response"
                        # Deny on Claude error
                        erpc_call "yawl_approval" "deny" "[\"$checkpoint_id\", \"claude_worker\", \"Claude invocation failed\"]" &> /dev/null
                    fi
                fi
            done
        else
            log_warning "jq not available, cannot parse pending approvals"
        fi
    fi

    # Wait before next poll
    sleep "$POLL_INTERVAL"
done
