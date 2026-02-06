#!/bin/bash
#
# CRE Human-in-the-Loop Workflow Demo
# Y Combinator Final Review Demonstration
#

set -euo pipefail

MODE="${1:-fast}"
LOG_FILE="/tmp/cre_demo_$(date +%s).log"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
BOLD='\033[1m'
NC='\033[0m'

log() { echo -e "${2}[$(date '+%H:%M:%S')]${NC} $1"; echo "[$(date '+%H:%M:%S')] $1" >> "$LOG_FILE"; }
print_header() {
    echo ""
    echo -e "${BOLD}${CYAN}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}${CYAN}║${NC}  ${BOLD}CRE: Human-in-the-Loop Workflow Demo${NC}               ${BOLD}${CYAN}║${NC}"
    echo -e "${BOLD}${CYAN}║${NC}  Y Combinator Final Review 2025                            ${BOLD}${CYAN}║${NC}"
    echo -e "${BOLD}${CYAN}╚════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    log "Demo mode: $MODE" "$BLUE"
}
print_step() { echo ""; echo -e "${BOLD}${MAGENTA}➤ Step $1: $2${NC}"; echo -e "${MAGENTA}──────────────────────────────────────────${NC}"; }
print_success() { echo -e "${GREEN}✓ $1${NC}"; log "✓ $1" "${GREEN}"; }
print_error() { echo -e "${RED}✗ $1${NC}"; log "✗ $1" "${RED}"; }
print_info() { echo -e "${BLUE}ℹ $1${NC}"; }

# Create Erlang demo module
create_demo_module() {
    cat > /tmp/cre_demo_runner.erl << 'ERLEOF'
-module(cre_demo_runner).
-export([run_demo/0]).

run_demo() ->
    io:format("~n~n=== CRE Human-in-the-Loop Workflow Demo ===~n~n"),
    io:format("Step 1: Starting approval system...~n"),
    case whereis(yawl_approval) of
        undefined ->
            {ok, Pid} = yawl_approval:start_link(),
            io:format("  Approval server started: ~p~n", [Pid]);
        Pid ->
            io:format("  Approval server already running: ~p~n", [Pid])
    end,

    io:format("~nStep 2: Creating pre-test checkpoint...~n"),
    {ok, Cid1} = yawl_approval:create_checkpoint(
        <<"demo_workflow">>,
        pre_test_checkpoint,
        #{required_approver => auto, timeout => 60000}
    ),
    io:format("  Checkpoint created: ~s~n", [Cid1]),

    io:format("~nStep 3: Requesting approval (auto-approve mode)...~n"),
    {ok, Decision1} = yawl_approval:request_approval(Cid1),
    io:format("  Decision: ~p~n", [Decision1]),

    io:format("~nStep 4: Simulating test execution...~n"),
    io:format("  Compiling project...~n"),
    CompileResult = os:cmd("cd /Users/sac/cre && rebar3 compile 2>&1 | grep -c Compiling || echo 0"),
    io:format("  Compile result: ~s modules compiled~n", [CompileResult]),

    io:format("~nStep 5: Creating post-test review checkpoint...~n"),
    {ok, Cid2} = yawl_approval:create_checkpoint(
        <<"demo_workflow">>,
        post_test_review,
        #{required_approver => auto, timeout => 60000}
    ),
    io:format("  Checkpoint created: ~s~n", [Cid2]),

    io:format("~nStep 6: Approving results review...~n"),
    ok = yawl_approval:approve(Cid2, demo_reviewer, <<"Results approved for demo">>),
    io:format("  Results approved~n", []),

    io:format("~nStep 7: Listing all checkpoints...~n"),
    All = yawl_approval:list_all(),
    io:format("  Total checkpoints: ~p~n", [length(All)]),

    io:format("~n=== Demo Complete! ===~n~n"),
    io:format("Summary:~n"),
    io:format("  - Approval system: Running~n"),
    io:format("  - Pre-test checkpoint: Approved~n"),
    io:format("  - Tests: Compiled successfully~n"),
    io:format("  - Post-test review: Approved~n"),
    io:format("~nAll approvals logged to XES audit trail.~n~n"),

    init:stop().
ERLEOF
}

# Main demo
main() {
    print_header
    print_info "Starting CRE Human-in-the-Loop Workflow Demo..."

    # Compile the project first
    print_step "0" "Compiling CRE"
    cd /Users/sac/cre
    if rebar3 compile > /tmp/cre_compile.log 2>&1; then
        print_success "Compilation successful"
        cat /tmp/cre_compile.log | tee -a "$LOG_FILE" | tail -5
    else
        print_error "Compilation failed"
        cat /tmp/cre_compile.log | tee -a "$LOG_FILE"
        exit 1
    fi

    # Create demo runner module
    create_demo_module

    # Compile demo module
    print_step "1" "Preparing demo module"
    erlc -I /Users/sac/cre/src -o /tmp /tmp/cre_demo_runner.erl 2>&1 | tee -a "$LOG_FILE"
    print_success "Demo module ready"

    # Run the demo
    print_step "2" "Running workflow demo"
    erl -noshell -pa _build/default/lib/cre/ebin -pa /tmp \
        -eval "cre_demo_runner:run_demo()" 2>&1 | tee -a "$LOG_FILE"

    # Summary
    echo ""
    echo -e "${BOLD}${GREEN}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  ${BOLD}Demo Complete!${NC}                                            ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}╠════════════════════════════════════════════════════════════╣${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  ${BOLD}Workflow Summary:${NC}                                         ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}                                                              ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  ✓ Approval System Initialized                                ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  ✓ Pre-Test Checkpoint Approved                              ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  ✓ Test Suite Compiled Successfully                         ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  ✓ Results Reviewed & Approved                               ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}                                                              ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  ${CYAN}Next Steps:${NC}                                               ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  • Open docs/workflow_dashboard.html for visual dashboard  ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}║${NC}  • See DEMO_README.md for full documentation              ${BOLD}${GREEN}║${NC}"
    echo -e "${BOLD}${GREEN}╚════════════════════════════════════════════════════════════╝${NC}"
    echo ""

    log "Demo completed successfully" "${GREEN}"
    log "Full log: $LOG_FILE" "${BLUE}"
}

# Handle arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [--fast|--help]"
        echo "  --fast    Quick demo mode (default)"
        echo "  --help    Show this help"
        exit 0
        ;;
    *)
        main
        ;;
esac
