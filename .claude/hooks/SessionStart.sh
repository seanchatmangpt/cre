#!/usr/bin/env bash
# SessionStart hook for CRE project
# Bootstraps Erlang/OTP 28+ on cloud environments (including gVisor sandbox)
#
# OTP 28.3.1 is available from:
#   - Hex.pm Bob builds (RECOMMENDED): https://builds.hex.pm/builds/otp/
#     └─ Pre-built complete binaries (requires Install script post-processing)
#   - Official source releases: https://github.com/erlang/otp/releases/tag/OTP-28.3.1
#   - Docker: erlang:28.3.1 (unavailable in gVisor)
#
# Strategy: CACHE -> SYSTEM -> HEX.PM PRE-BUILT (+ Install) -> SOURCE (fallback)
# Performance: Pre-built (~2 min) vs source build (~7-10 min)
# Idempotent: lock file prevents redundant execution
#
# Version: 4.2.0-hexpm-optimized

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

# Static binary URLs (pre-built for gVisor compatibility)
# These are minimal static builds hosted on GitHub releases
readonly OTP_STATIC_LINUX_URL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz"
readonly OTP_STATIC_BASE="https://github.com/emqx/erlang-rpm/releases/download"

# Fallback source URL
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
    phase "1/6 Cache check"
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

# Check if running in gVisor sandbox
is_gvisor() {
    [[ -f /proc/version ]] && grep -qi "gvisor" /proc/version 2>/dev/null && return 0
    # Check for limited /proc (gVisor limits /proc access)
    [[ ! -d /proc/sys/vm ]] && return 0
    return 1
}

#=============================================================================
# Phase 2A: Check System OTP (works if pre-installed)
#=============================================================================

check_system_otp() {
    phase "2A/6 Check system OTP"
    local bins=(
        "/usr/bin/erl"
        "/usr/local/bin/erl"
        "/opt/erlang/bin/erl"
        "/opt/homebrew/bin/erl"
        "$HOME/.erlmcp/otp-${OTP_VERSION}/bin/erl"
    )

    for p in "${bins[@]}"; do
        if [[ -f "$p" ]]; then
            local major
            major=$(otp_major "$p")
            if [[ $major -ge $OTP_MAJOR ]]; then
                success "Found system OTP $major at $p"
                mkdir -p "${OTP_DIR}/bin"
                ln -sf "$p" "${OTP_DIR}/bin/erl"
                # Link other binaries from same directory
                local dir
                dir=$(dirname "$p")
                for bin in "$dir"/erl* "$dir"/dialyzer; do
                    [[ -f "$bin" ]] && ln -sf "$bin" "${OTP_DIR}/bin/$(basename "$bin")" 2>/dev/null || true
                done
                return 0
            fi
        fi
    done

    info "No suitable system OTP found"
    return 1
}

#=============================================================================
# Phase 2B: Download Pre-built Binary from Hex.pm (gVisor compatible)
#=============================================================================

download_static_binary() {
    phase "2B/6 Download pre-built OTP (Hex.pm Bob builds)"
    mkdir -p "$OTP_DIR"

    info "Downloading pre-built OTP 28.3.1 from Hex.pm..."

    # Hex.pm Bob builds - Complete pre-built OTP binaries (not partial CI artifacts)
    # These include all binaries, libraries, and boot files
    # URL pattern: https://builds.hex.pm/builds/otp/{arch}/{os_version}/OTP-{version}.tar.gz
    local urls=(
        # Ubuntu 22.04 LTS (most compatible with gVisor)
        "https://builds.hex.pm/builds/otp/amd64/ubuntu-22.04/OTP-${OTP_VERSION}.tar.gz"
        # Ubuntu 20.04 LTS (fallback)
        "https://builds.hex.pm/builds/otp/amd64/ubuntu-20.04/OTP-${OTP_VERSION}.tar.gz"
        # Ubuntu 24.04 LTS (latest)
        "https://builds.hex.pm/builds/otp/amd64/ubuntu-24.04/OTP-${OTP_VERSION}.tar.gz"
    )

    for url in "${urls[@]}"; do
        info "Trying: $url"
        local tarball="${CACHE_DIR}/otp-${OTP_VERSION}.tar.gz"
        local tmp="${CACHE_DIR}/temp-otp-$$"
        mkdir -p "$tmp"

        # Download the tarball
        if curl -fsSL -o "$tarball" "$url" 2>&1 | tee -a "$LOG_FILE"; then
            local file_size
            file_size=$(stat -f%z "$tarball" 2>/dev/null || stat -c%s "$tarball" 2>/dev/null || echo "0")
            info "Downloaded: $((file_size / 1024 / 1024)) MB"

            # Extract to temporary location
            if tar xzf "$tarball" -C "$tmp" 2>&1 | tee -a "$LOG_FILE"; then
                info "Extraction successful"

                # Find OTP-* directory (Bob tarballs extract to OTP-VERSION/ root)
                local otp_extracted
                otp_extracted=$(find "$tmp" -maxdepth 1 -type d -name "OTP-*" 2>/dev/null | head -1)

                if [[ -z "$otp_extracted" ]]; then
                    # Try direct extraction (no OTP-* wrapper)
                    otp_extracted="$tmp"
                fi

                # Verify it has the expected structure
                if [[ -f "$otp_extracted/Install" ]] && [[ -d "$otp_extracted/erts-"* ]]; then
                    info "Running post-install setup (Install script)..."

                    # Run the Install script to finalize the setup
                    # This generates bin/erl from erts-*/bin/erl.src template
                    # and copies boot files from releases/*/
                    if bash "$otp_extracted/Install" -minimal "$otp_extracted" 2>&1 | tee -a "$LOG_FILE"; then
                        info "Install script completed"

                        # Copy the finalized OTP installation to the cache location
                        if cp -r "$otp_extracted"/* "$OTP_DIR/" 2>/dev/null; then
                            rm -rf "$tmp" "$tarball"

                            # Verify the installation works
                            local major
                            major=$(otp_major "${OTP_DIR}/bin/erl" 2>/dev/null || echo "0")
                            if [[ $major -ge $OTP_MAJOR ]]; then
                                success "Pre-built OTP $major installed (via Hex.pm Bob)"
                                return 0
                            else
                                error "Installation verification failed (major=$major, expected>=$OTP_MAJOR)"
                            fi
                        else
                            error "Failed to copy OTP installation"
                        fi
                    else
                        error "Install script failed"
                    fi
                else
                    error "Downloaded file doesn't have expected OTP structure (missing Install or erts-*)"
                fi
            else
                error "Extraction failed"
            fi

            rm -rf "$tmp" "$tarball"
        else
            info "Download failed (may be network or URL issue): $url"
        fi
    done

    info "No pre-built binary available from any source"
    return 1
}

#=============================================================================
# Phase 2C: Search Existing OTP (macOS fast path)
#=============================================================================

search_existing_macos() {
    phase "2C/6 Search existing OTP (macOS)"
    local paths=(
        "$HOME/.erlmcp/otp-${OTP_VERSION}/lib/erlang/bin/erl"
        "$HOME/.erlmcp/otp-${OTP_VERSION}/bin/erl"
        "$HOME/.kerl/installs/${OTP_VERSION}/otp_${OTP_VERSION}/bin/erl"
        "/opt/homebrew/bin/erl"
        "/usr/local/bin/erl"
        "/opt/local/bin/erl"
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
# Phase 2D: Minimal Build from Source (last resort, may fail in gVisor)
#=============================================================================

build_from_source() {
    phase "2D/6 Build OTP from source (may fail in sandbox)"

    # Check for build tools
    if ! command -v gcc &>/dev/null && ! command -v clang &>/dev/null; then
        error "No compiler available - cannot build OTP"
        info "Hint: In gVisor sandbox, use pre-built binary or install OTP on host"
        return 1
    fi

    local tmp="/tmp/otp-build-$$"
    mkdir -p "$tmp" && cd "$tmp"

    info "Downloading OTP source..."
    if ! curl -fsSL -o "otp.tar.gz" "$OTP_SOURCE_URL" 2>&1 | tee -a "$LOG_FILE"; then
        error "Failed to download OTP source"
        cd "$PROJECT_ROOT" && rm -rf "$tmp"
        return 1
    fi

    tar xzf "otp.tar.gz" || { cd "$PROJECT_ROOT" && rm -rf "$tmp"; return 1; }
    cd otp_src_*/

    info "Configuring with minimal options..."
    if ! ./configure --prefix="$OTP_DIR" \
        --disable-debug \
        --disable-documentation \
        --without-javac \
        --without-odbc \
        --without-wx \
        --without-et \
        --without-megaco \
        2>&1 | tee -a "$LOG_FILE" | tail -20; then
        error "Configure failed - likely incompatible with sandbox"
        cd "$PROJECT_ROOT" && rm -rf "$tmp"
        return 1
    fi

    info "Building with $CPU_COUNT CPUs..."
    if ! make -j "$CPU_COUNT" 2>&1 | tee -a "$LOG_FILE" | tail -20; then
        error "Build failed - likely incompatible with sandbox"
        cd "$PROJECT_ROOT" && rm -rf "$tmp"
        return 1
    fi

    info "Installing..."
    if ! make install 2>&1 | tee -a "$LOG_FILE" | tail -10; then
        cd "$PROJECT_ROOT" && rm -rf "$tmp"
        return 1
    fi

    cd "$PROJECT_ROOT" && rm -rf "$tmp"

    local major
    major=$(otp_major "$OTP_BIN")
    if [[ $major -ge $OTP_MAJOR ]]; then
        success "OTP $major built from source"
        return 0
    fi

    error "Build verification failed"
    return 1
}

#=============================================================================
# Phase 3: Environment Setup
#=============================================================================

setup_environment() {
    phase "3/6 Environment setup"

    # System bins first to preserve standard commands, OTP appended
    export PATH="/usr/bin:/bin:/usr/local/bin:${OTP_DIR}/bin:${PATH}"
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
    phase "4/6 Lock file creation"
    mkdir -p "$(dirname "$LOCK_FILE")"
    echo "$OTP_VERSION" > "$LOCK_FILE"
    success "Lock file created: $LOCK_FILE"
}

#=============================================================================
# Phase 5: Rebar3 Setup
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

#=============================================================================
# Phase 6: Project Build
#=============================================================================

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
        # Use portable sed syntax
        sed -i.bak 's/-spec parse(binary(), state())/-spec parse(binary(), State)/' "$cow_sse"
        sed -i.bak 's/\t-> {event, parsed_event(), State} | {more, State}\./\t-> {event, parsed_event(), State} | {more, State}\n\twhen State :: state()./' "$cow_sse"
        rm -f "$cow_sse.bak"
        success "cowlib patched"
    fi
}

build_project() {
    phase "5/6 Project build (rebar3)"
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
# Phase 6: Verify BEAM Ready for Hot Code Swapping
#=============================================================================

verify_beam_ready() {
    local start_time=$1
    phase "6/6 BEAM readiness verification"

    info "Testing BEAM VM boot and hot code loading capabilities..."

    # Create a simple test module for hot code swapping verification
    local test_module="/tmp/beam_ready_test_$$.erl"
    cat > "$test_module" <<'ERLTEST'
-module(beam_ready_test).
-export([version/0]).
version() -> 1.
ERLTEST

    # Test 1: BEAM can boot and execute code
    local beam_boot_start=$(date +%s%N 2>/dev/null || echo "0")
    local boot_result
    boot_result=$("$OTP_BIN" -noshell -eval 'io:format("READY~n"), halt().' 2>&1)
    local beam_boot_end=$(date +%s%N 2>/dev/null || echo "0")

    if [[ "$boot_result" != "READY" ]]; then
        error "BEAM VM failed to boot properly"
        rm -f "$test_module"
        return 1
    fi

    # Test 2: Code compilation and hot loading capability
    local compile_result
    compile_result=$("$OTP_BIN" -noshell -eval "
        Binary = case compile:file('$test_module', [binary, return]) of
            {ok, beam_ready_test, Bin} -> Bin;
            {ok, beam_ready_test, Bin, _Warnings} -> Bin;
            _ -> error
        end,
        case Binary of
            error ->
                io:format('COMPILE_FAILED~n');
            _ ->
                case code:load_binary(beam_ready_test, \"$test_module\", Binary) of
                    {module, beam_ready_test} ->
                        io:format('HOT_LOAD_OK~n');
                    _ ->
                        io:format('HOT_LOAD_FAILED~n')
                end
        end,
        halt().
    " 2>&1)

    rm -f "$test_module"

    if [[ "$compile_result" != "HOT_LOAD_OK" ]]; then
        error "BEAM hot code loading not ready"
        return 1
    fi

    # Calculate timing
    local end_time=$(date +%s%N 2>/dev/null || date +%s)
    local total_time
    if [[ "$start_time" =~ ^[0-9]+$ ]] && [[ "$end_time" =~ ^[0-9]+$ ]]; then
        if [[ ${#start_time} -gt 10 ]]; then
            # Nanosecond precision
            total_time=$(( (end_time - start_time) / 1000000 ))
            local boot_time=$(( (beam_boot_end - beam_boot_start) / 1000000 ))
        else
            # Second precision fallback
            total_time=$(( (end_time - start_time) * 1000 ))
            boot_time=0
        fi
    else
        total_time="N/A"
        boot_time="N/A"
    fi

    success "BEAM VM ready for hot code swapping"
    echo "" >&2
    echo "╔════════════════════════════════════════════════════════════╗" >&2
    echo "║                   STARTUP TIMING REPORT                    ║" >&2
    echo "╠════════════════════════════════════════════════════════════╣" >&2
    if [[ "$total_time" != "N/A" ]]; then
        printf "║  Total startup time:                    %8s ms     ║\n" "$total_time" >&2
        if [[ "$boot_time" != "N/A" ]] && [[ "$boot_time" -gt 0 ]]; then
            printf "║  BEAM VM boot time:                     %8s ms     ║\n" "$boot_time" >&2
        fi
        printf "║  Time to hot-swap ready:                %8s ms     ║\n" "$total_time" >&2
    else
        echo "║  Timing: Available (high-resolution timer unavailable)     ║" >&2
    fi
    echo "╠════════════════════════════════════════════════════════════╣" >&2
    echo "║  ✓ BEAM VM boots successfully                              ║" >&2
    echo "║  ✓ Code compilation working                                ║" >&2
    echo "║  ✓ Hot code loading enabled                                ║" >&2
    echo "║  ✓ Ready for development                                   ║" >&2
    echo "╚════════════════════════════════════════════════════════════╝" >&2
}

#=============================================================================
# Phase 7: Completion Report
#=============================================================================

completion_report() {
    phase "7/7 Session complete"
    local major
    major=$(otp_major "$OTP_BIN")
    info "OTP Version: $major (target: $OTP_MAJOR)"
    info "OTP Path: ${OTP_DIR}/bin/erl"

    if is_gvisor; then
        info "Environment: gVisor sandbox detected"
        info "Note: Some syscalls are limited in gVisor"
    else
        info "Environment: native $(detect_platform)"
    fi

    success "SessionStart complete - ready to develop"
}

#=============================================================================
# Main
#=============================================================================

main() {
    # Capture start time for timing report
    local start_time=$(date +%s%N 2>/dev/null || date +%s)

    init_log
    info "Starting SessionStart.sh (v4.2.0-hexpm-optimized)"
    info "Platform: $(detect_platform)"

    # Phase 1: Cache check (fastest: ~100ms if cached)
    if ! check_cache; then
        info "OTP not cached, acquiring..."
        local plat acquired=false
        plat=$(detect_platform)

        # Phase 2: Platform-specific acquisition (prioritized by speed)
        if [[ "$plat" == "macos" ]]; then
            # macOS: Try existing installation or kerl
            search_existing_macos && acquired=true
        elif [[ "$plat" == "linux" ]]; then
            # Linux: System OTP -> Hex.pm pre-built (FAST) -> Build from source (SLOW)
            check_system_otp && acquired=true
            # Hex.pm Bob builds are complete pre-built binaries with Install script
            # Much faster than building from source (~2 min vs ~7-10 min)
            download_static_binary && acquired=true
        fi

        # Fallback: build from source (5-8 minutes, slower but reliable in gVisor)
        if [[ "$acquired" != "true" ]]; then
            if ! build_from_source; then
                error "All OTP acquisition methods failed"
                info ""
                info "GVisor/Sandbox detected? Build from source often fails."
                info "Solutions:"
                info "  1. Install OTP on the host system outside the sandbox"
                info "  2. Use a pre-built binary from Hex.pm (usually faster)"
                info "  3. Request OTP support in the sandbox environment"
                exit 1
            fi
        fi
    fi

    # Phases 3-7
    setup_environment
    create_lock
    build_project || info "Project build skipped or failed (non-fatal)"
    verify_beam_ready "$start_time"
    completion_report

    exit 0
}

main "$@"
