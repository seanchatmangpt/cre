#!/usr/bin/env bash
# SessionStart hook for CRE project
# Bootstraps Erlang/OTP 28+ on cloud environments
#
# Strategy: CACHE -> DOWNLOAD -> BUILD -> ENVIRONMENT -> PROJECT
# Idempotent: lock file prevents redundant execution
#
# Version: 3.0.0

set -euo pipefail

#=============================================================================
# Configuration
#=============================================================================

readonly OTP_VERSION="28.3.1"
readonly OTP_MAJOR=28
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
readonly CACHE_DIR="${PROJECT_ROOT}/.erlmcp"
readonly OTP_DIR="${CACHE_DIR}/otp-${OTP_VERSION}"
readonly OTP_BIN="${OTP_DIR}/bin/erl"
readonly LOCK_FILE="${CACHE_DIR}/cache/sessionstart.lock"
readonly LOG_FILE="${CACHE_DIR}/sessionstart.log"
readonly REBAR3_BIN="${CACHE_DIR}/cache/rebar3"
readonly REBAR3_URL="https://s3.amazonaws.com/rebar3/rebar3"
readonly OTP_PREBUILT_URL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz"
readonly OTP_PREBUILT_BASEURL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}"
readonly OTP_PREBUILT_LINUX_URL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_doc_${OTP_VERSION}.tar.gz"
readonly OTP_SOURCE_URL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz"
readonly CPU_COUNT=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)

#=============================================================================
# Logging (stderr to avoid interfering with hook JSON output)
#=============================================================================

log()     { echo "[$1] $2" | tee -a "$LOG_FILE" >&2; }
info()    { log "INFO" "$*"; }
error()   { log "ERROR" "$*"; }
success() { log "SUCCESS" "$*"; }
phase()   {
    echo "" >&2
    printf '%80s\n' | tr ' ' '=' | tee -a "$LOG_FILE" >&2
    log "PHASE" "$*"
    printf '%80s\n' | tr ' ' '=' | tee -a "$LOG_FILE" >&2
}

init_log() {
    mkdir -p "$CACHE_DIR/cache"
    echo "--- SessionStart $(date -Iseconds) OTP=${OTP_VERSION} ---" >> "$LOG_FILE"
}

#=============================================================================
# OTP Version Detection
#=============================================================================

otp_major() {
    local bin="${1:-erl}"
    if command -v "$bin" &>/dev/null || [[ -f "$bin" ]]; then
        local v
        v=$("$bin" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "0")
        echo "${v%%.*}"
    else
        echo "0"
    fi
}

#=============================================================================
# Phase 1: Cache Check
#=============================================================================

check_cache() {
    phase "1/5 Cache check"
    [[ -f "$OTP_BIN" ]] || return 1
    local major
    major=$(otp_major "$OTP_BIN")
    if [[ $major -ge $OTP_MAJOR ]]; then
        success "OTP major version $major found in cache"
        return 0
    fi
    return 1
}

#=============================================================================
# Platform Detection
#=============================================================================

detect_platform() {
    case "$OSTYPE" in
        linux*)  echo "linux" ;;
        darwin*) echo "macos" ;;
        *)       echo "unknown" ;;
    esac
}

#=============================================================================
# Phase 2A: Check for system OTP (Linux packages)
#=============================================================================

check_system_otp() {
    phase "2/5 Check system OTP packages"
    local bins=(
        "/usr/bin/erl"
        "/usr/local/bin/erl"
        "/opt/erlang/bin/erl"
    )

    for p in "${bins[@]}"; do
        if [[ -f "$p" ]]; then
            local major
            major=$(otp_major "$p")
            if [[ $major -ge $OTP_MAJOR ]]; then
                success "Found system OTP $major at $p"
                mkdir -p "${OTP_DIR}/bin"
                ln -sf "$p" "${OTP_DIR}/bin/erl"
                return 0
            fi
        fi
    done

    info "No suitable system OTP found, will build from source"
    return 1
}

#=============================================================================
# Phase 2B: Download Pre-built OTP (Linux fast path - DISABLED)
#=============================================================================

download_prebuilt() {
    # On Linux, kerl-managed installations are more reliable
    # Skip prebuilt binary - go straight to source build
    return 1
}

#=============================================================================
# Phase 2B: Search Existing OTP (macOS fast path)
#=============================================================================

search_existing_macos() {
    phase "2C/5 Search existing OTP (macOS)"
    local paths=(
        "${OTP_DIR}/lib/erlang/bin/erl"
        "${OTP_DIR}/bin/erl"
        "$HOME/.erlmcp/otp-${OTP_VERSION}/lib/erlang/bin/erl"
        "$HOME/.erlmcp/otp-${OTP_VERSION}/bin/erl"
        "/opt/homebrew/bin/erl"
        "/usr/local/bin/erl"
    )

    for p in "${paths[@]}"; do
        if [[ -f "$p" ]]; then
            local major
            major=$(otp_major "$p")
            if [[ $major -ge $OTP_MAJOR ]]; then
                success "Found OTP $major at $p"
                mkdir -p "${OTP_DIR}/bin"
                local dir
                dir=$(dirname "$p")
                for bin in "$dir"/*; do
                    [[ -f "$bin" ]] && ln -sf "$bin" "${OTP_DIR}/bin/$(basename "$bin")" 2>/dev/null || true
                done
                return 0
            fi
        fi
    done
    return 1
}

#=============================================================================
# Phase 2C: Build from Source (slow fallback, ~6min)
#=============================================================================

build_from_source() {
    phase "2D/5 Build OTP from source (~6min)"
    local tmp="/tmp/otp-build-$$"
    mkdir -p "$tmp" && cd "$tmp"

    # Install build dependencies on Debian/Ubuntu
    if command -v apt-get &>/dev/null; then
        info "Installing build dependencies via apt..."
        if sudo -n true 2>/dev/null; then
            sudo apt-get update -qq || true
            sudo apt-get install -y -qq \
                build-essential autoconf libncurses5-dev \
                libssl-dev libwxgtk3.2-dev libgl1-mesa-dev \
                libglu1-mesa-dev libpng-dev libssh-dev \
                unixodbc-dev xsltproc fop libxml2-utils &>/dev/null || true
        else
            info "No sudo access, skipping system package install"
        fi
    fi

    info "Downloading OTP source..."
    if ! curl -fsSL -o "otp.tar.gz" "$OTP_SOURCE_URL" 2>&1 | tee -a "$LOG_FILE"; then
        error "Failed to download OTP source"
        cd "$PROJECT_ROOT" && rm -rf "$tmp"
        return 1
    fi

    tar xzf "otp.tar.gz" || { cd "$PROJECT_ROOT" && rm -rf "$tmp"; return 1; }
    cd otp_src_*/

    info "Configuring (prefix: $OTP_DIR)..."
    ./configure --prefix="$OTP_DIR" --disable-debug --disable-documentation \
        --without-javac --without-odbc 2>&1 | tee -a "$LOG_FILE" | tail -10 || {
        cd "$PROJECT_ROOT" && rm -rf "$tmp"; return 1;
    }

    info "Building with $CPU_COUNT CPUs..."
    make -j "$CPU_COUNT" 2>&1 | tee -a "$LOG_FILE" | tail -10 || {
        cd "$PROJECT_ROOT" && rm -rf "$tmp"; return 1;
    }

    info "Installing..."
    make install 2>&1 | tee -a "$LOG_FILE" | tail -10 || {
        cd "$PROJECT_ROOT" && rm -rf "$tmp"; return 1;
    }

    cd "$PROJECT_ROOT" && rm -rf "$tmp"

    # Verify
    local major
    major=$(otp_major "$OTP_BIN")
    if [[ $major -ge $OTP_MAJOR ]]; then
        success "OTP $major built and installed"
        return 0
    fi

    error "Build verification failed (got version: $major)"
    return 1
}

#=============================================================================
# Phase 3: Environment Setup
#=============================================================================

setup_environment() {
    phase "3/5 Environment setup"

    # System bins first to preserve standard commands, OTP appended
    export PATH="/usr/bin:/bin:/usr/local/bin:/usr/local/sbin:${OTP_DIR}/bin:${PATH}"
    export CLAUDE_CODE_REMOTE=true
    export ERLMCP_PROFILE=cloud
    export ERLMCP_CACHE="${CACHE_DIR}/cache/"
    export TERM=dumb
    export REBAR_COLOR=none
    export ERL_AFLAGS="-kernel shell_history enabled"

    mkdir -p "$ERLMCP_CACHE"

    # Persist for subsequent Bash tool calls via CLAUDE_ENV_FILE
    if [[ -n "${CLAUDE_ENV_FILE:-}" ]]; then
        {
            echo "export PATH=\"${OTP_DIR}/bin:\$PATH\""
            echo "export CLAUDE_CODE_REMOTE=true"
            echo "export ERLMCP_PROFILE=cloud"
            echo "export ERLMCP_CACHE=\"${CACHE_DIR}/cache/\""
            echo "export TERM=dumb"
            echo "export REBAR_COLOR=none"
            echo "export ERL_AFLAGS=\"-kernel shell_history enabled\""
        } >> "$CLAUDE_ENV_FILE"
        info "Environment variables persisted to CLAUDE_ENV_FILE"
    fi

    # Write env file for other hooks to source
    cat > "${CACHE_DIR}/env.sh" <<ENVEOF
# Generated by SessionStart.sh - source for OTP 28+ environment
export PATH="/usr/bin:/bin:/usr/local/bin:${OTP_DIR}/bin:\$PATH"
export CLAUDE_CODE_REMOTE=true
export ERLMCP_PROFILE=cloud
export ERLMCP_CACHE="${CACHE_DIR}/cache/"
export TERM=dumb
export REBAR_COLOR=none
export ERL_AFLAGS="-kernel shell_history enabled"
ENVEOF

    success "Environment variables set"
    info "  PATH includes ${OTP_DIR}/bin"
    info "  ERLMCP_PROFILE=$ERLMCP_PROFILE"
}

#=============================================================================
# Phase 4: Lock File
#=============================================================================

create_lock() {
    phase "4/5 Lock file creation"
    mkdir -p "$(dirname "$LOCK_FILE")"
    echo "$OTP_VERSION" > "$LOCK_FILE"
    success "Lock file created: $LOCK_FILE"
}

#=============================================================================
# Phase 5: Project Build (rebar3 + deps + compile)
#=============================================================================

ensure_rebar3() {
    if [[ -f "$REBAR3_BIN" && -x "$REBAR3_BIN" ]]; then
        info "rebar3 already cached at $REBAR3_BIN"
        return 0
    fi

    info "Downloading rebar3..."
    mkdir -p "$(dirname "$REBAR3_BIN")"
    if curl -fsSL -o "$REBAR3_BIN" "$REBAR3_URL" 2>&1 | tee -a "$LOG_FILE"; then
        chmod +x "$REBAR3_BIN"
        success "rebar3 downloaded"
        return 0
    fi

    error "Failed to download rebar3"
    return 1
}

patch_cowlib() {
    # cowlib <2.16.0 has an unbound type variable in cow_sse.erl
    # that OTP 28 treats as a hard error. Patch in-place after deps.
    local cow_sse="${PROJECT_ROOT}/_build/default/lib/cowlib/src/cow_sse.erl"
    [[ -f "$cow_sse" ]] || return 0

    if grep -q "when State :: state()" "$cow_sse" 2>/dev/null; then
        info "cowlib already patched for OTP 28"
        return 0
    fi

    if grep -q 'State} | {more, State}\.' "$cow_sse" 2>/dev/null; then
        info "Patching cowlib cow_sse.erl for OTP 28..."
        # Use portable sed syntax (works on both Linux and macOS)
        sed -i.bak 's/-spec parse(binary(), state())/-spec parse(binary(), State)/' "$cow_sse"
        sed -i.bak 's/\t-> {event, parsed_event(), State} | {more, State}\./\t-> {event, parsed_event(), State} | {more, State}\n\twhen State :: state()./' "$cow_sse"
        rm -f "$cow_sse.bak"
        success "cowlib patched"
    fi
}

build_project() {
    phase "5/5 Project build (rebar3 deps + compile)"
    ensure_rebar3 || { error "Cannot build without rebar3"; return 1; }
    cd "$PROJECT_ROOT"

    # Check if already compiled
    local beam_count
    beam_count=$(find "${PROJECT_ROOT}/_build/default/lib/cre/ebin" -name "*.beam" 2>/dev/null | wc -l)
    if [[ "$beam_count" -gt 0 ]]; then
        info "Already compiled ($beam_count beam files), verifying..."
        if "$REBAR3_BIN" compile 2>&1 | tee -a "$LOG_FILE" | tail -5; then
            success "Project compilation verified"
            return 0
        fi
        info "Re-compilation needed..."
    fi

    info "Fetching dependencies..."
    if ! "$REBAR3_BIN" get-deps 2>&1 | tee -a "$LOG_FILE" | tail -10; then
        error "Failed to fetch dependencies"
        return 1
    fi

    patch_cowlib

    info "Compiling project..."
    if ! "$REBAR3_BIN" compile 2>&1 | tee -a "$LOG_FILE" | tail -10; then
        error "Compilation failed"
        return 1
    fi
    success "Project compiled successfully"
}

#=============================================================================
# Main
#=============================================================================

main() {
    init_log
    info "Starting SessionStart.sh (v3.1.0-linux)"
    info "Platform: $(detect_platform)"

    # Phase 1: Cache check
    if ! check_cache; then
        info "OTP not cached, acquiring..."
        local plat acquired=false
        plat=$(detect_platform)

        # Phase 2: Platform-specific acquisition
        if [[ "$plat" == "macos" ]]; then
            search_existing_macos && acquired=true
        elif [[ "$plat" == "linux" ]]; then
            check_system_otp && acquired=true
            download_prebuilt && acquired=true  # Currently disabled, always fails
        fi

        # Fallback: build from source
        if [[ "$acquired" != "true" ]]; then
            build_from_source || { error "All OTP acquisition methods failed"; exit 1; }
        fi
    fi

    # Phases 3-5
    setup_environment
    create_lock
    build_project || info "Project build skipped or failed (non-fatal)"

    success "SessionStart complete"
    exit 0
}

main "$@"
