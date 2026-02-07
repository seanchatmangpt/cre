#!/usr/bin/env bash
# .claude/hooks/SessionStart.sh
# Purpose: Autonomous OTP 28+ initialization for erlmcp
# Spec: CLAUDE.md v2.1.0
# Version: 2.3.0-consolidated
#
# State Machine:
#   Phase 1: Cache check (use cached OTP if available)
#   Phase 2A: Platform detection (Linux/macOS)
#   Phase 2B: Existing OTP search (macOS fast path)
#   Phase 2C: Pre-built download (Linux fast path)
#   Phase 2D: Source build (fallback)
#   Phase 3: Environment setup
#   Phase 4: Lock file creation
#   Phase 5: Project build (rebar3 + deps + compile)
#
# Strategy: CACHE → PLATFORM → EXISTING → DOWNLOAD → BUILD → LOCK → PROJECT
# Idempotency: Lock file prevents re-execution

set -euo pipefail

#==============================================================================
# Constants & Configuration
#==============================================================================

readonly REQUIRED_OTP_VERSION="28.3.1"
readonly REQUIRED_OTP_MAJOR=28
# Derive project root from script location (.claude/hooks/SessionStart.sh → project root)
readonly _SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly ERLMCP_ROOT="$(cd "$_SCRIPT_DIR/../.." && pwd)"
readonly OTP_CACHE_DIR="${ERLMCP_ROOT}/.erlmcp/otp-${REQUIRED_OTP_VERSION}"
readonly OTP_BIN="${OTP_CACHE_DIR}/bin/erl"
readonly LOCK_FILE="${ERLMCP_ROOT}/.erlmcp/cache/sessionstart.lock"
readonly LOG_FILE="${ERLMCP_ROOT}/.erlmcp/sessionstart.log"
readonly BUILD_TEMP_DIR="/tmp/otp-build-$$"
readonly SCRIPT_VERSION="2.5.0-consolidated"

# Rebar3 download URL
readonly REBAR3_URL="https://s3.amazonaws.com/rebar3/rebar3"
readonly REBAR3_BIN="${ERLMCP_ROOT}/.erlmcp/cache/rebar3"

# Pre-built OTP distribution (Linux only, ~30s)
readonly OTP_PREBUILT_URL="https://github.com/seanchatmangpt/erlmcp/releases/download/erlang-28.3.1/erlang-28.3.1-linux-x86_64.tar.gz"
readonly OTP_PREBUILT_SHA256="58f91a25499d962664dc8a5e94f52164524671d385baeebee72741c7748c57d8"

# Source build fallback (~6m)
readonly OTP_RELEASE_URL="https://github.com/erlang/otp/releases/download/OTP-${REQUIRED_OTP_VERSION}/otp_src_${REQUIRED_OTP_VERSION}.tar.gz"

readonly CPU_COUNT=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)

#==============================================================================
# Logging (stderr to avoid interfering with command output capture)
#==============================================================================

log_info() { echo "[INFO] $*" | tee -a "$LOG_FILE" >&2; }
log_error() { echo "[ERROR] $*" | tee -a "$LOG_FILE" >&2; }
log_success() { echo "[SUCCESS] $*" | tee -a "$LOG_FILE" >&2; }
log_phase() {
    echo "" | tee -a "$LOG_FILE" >&2
    echo "================================================================================" | tee -a "$LOG_FILE" >&2
    echo "[PHASE $1/4] $*" | tee -a "$LOG_FILE" >&2
    echo "================================================================================" | tee -a "$LOG_FILE" >&2
}

init_logging() {
    mkdir -p "$(dirname "$LOG_FILE")"
    {
        echo "================================================================================"
        echo "[SessionStart] Timestamp: $(date -Iseconds)"
        echo "[SessionStart] Version: $SCRIPT_VERSION"
        echo "[SessionStart] Working dir: $(pwd)"
        echo "[SessionStart] OTP version: $REQUIRED_OTP_VERSION (major >= $REQUIRED_OTP_MAJOR)"
        echo "[SessionStart] Cache dir: $OTP_CACHE_DIR"
        echo "================================================================================"
    } >> "$LOG_FILE"
}

#==============================================================================
# Platform Detection
#==============================================================================

detect_platform() {
    case "$OSTYPE" in
        linux*)   echo "linux" ;;
        darwin*)  echo "macos" ;;
        *)        echo "unknown" ;;
    esac
}

PLATFORM=$(detect_platform)

#==============================================================================
# Version Checking (MAJOR version only - erlang:system_info returns "28" not "28.3.1")
#==============================================================================

get_otp_major_version() {
    local binary="${1:-erl}"
    if command -v "$binary" &> /dev/null; then
        local version
        version=$("$binary" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "0")
        # Extract major version (first number)
        if [[ "$version" =~ ^[0-9]+ ]]; then
            echo "${version%%.*}"
        else
            echo "0"
        fi
    else
        echo "0"
    fi
}

is_otp_version_acceptable() {
    local major="$1"
    [[ $major -ge $REQUIRED_OTP_MAJOR ]]
}

#==============================================================================
# Phase 1: Cache Check
#==============================================================================

is_otp_cached() {
    if [[ ! -f "$OTP_BIN" ]]; then
        return 1
    fi

    local cached_major
    cached_major=$(get_otp_major_version "$OTP_BIN")

    if is_otp_version_acceptable "$cached_major"; then
        log_success "OTP major version $cached_major found in cache"
        return 0
    else
        log_info "Cached OTP major version $cached_major does not meet requirement (>= $REQUIRED_OTP_MAJOR)"
        return 1
    fi
}

#==============================================================================
# Phase 2B: Search Existing OTP (macOS fast path)
#==============================================================================

search_existing_otp_macos() {
    log_info "Searching for existing OTP installation on macOS..."

    # Search paths in order of preference
    # Note: $HOME/.erlmcp is separate from project .erlmcp
    local search_paths=(
        "${OTP_CACHE_DIR}/lib/erlang/bin/erl"
        "${OTP_CACHE_DIR}/bin/erl"
        "$HOME/.erlmcp/otp-${REQUIRED_OTP_VERSION}/lib/erlang/bin/erl"
        "$HOME/.erlmcp/otp-${REQUIRED_OTP_VERSION}/bin/erl"
        "/opt/homebrew/bin/erl"
        "/usr/local/bin/erl"
        "/opt/local/bin/erl"
    )

    for erl_path in "${search_paths[@]}"; do
        if [[ -f "$erl_path" ]]; then
            local major
            major=$(get_otp_major_version "$erl_path")

            if is_otp_version_acceptable "$major"; then
                log_success "Found OTP $major at: $erl_path"
                echo "$erl_path"
                return 0
            else
                log_info "Version mismatch at $erl_path: $major (need >= $REQUIRED_OTP_MAJOR)"
            fi
        fi
    done

    return 1
}

#==============================================================================
# Phase 2C: Download Pre-built OTP (Linux only)
#==============================================================================

download_prebuilt_otp() {
    log_phase "2C/5" "Attempting pre-built OTP download (Linux fast path)"

    log_info "URL: $OTP_PREBUILT_URL"
    mkdir -p "$OTP_CACHE_DIR"
    local tarball="$OTP_CACHE_DIR/erlang-prebuilt.tar.gz"

    log_info "Downloading pre-built OTP (this may take ~30s)..."
    if ! curl -fsSL -o "$tarball" "$OTP_PREBUILT_URL" 2>&1 | tee -a "$LOG_FILE"; then
        log_info "Pre-built download failed (will try source build)"
        rm -f "$tarball"
        return 1
    fi

    log_info "Verifying SHA256..."
    local actual_sha256
    actual_sha256=$(sha256sum "$tarball" 2>/dev/null | awk '{print $1}' || shasum -a 256 "$tarball" 2>/dev/null | awk '{print $1}')

    if [[ "$actual_sha256" != "$OTP_PREBUILT_SHA256" ]]; then
        log_error "SHA256 mismatch: expected $OTP_PREBUILT_SHA256, got $actual_sha256"
        rm -f "$tarball"
        return 1
    fi

    log_success "SHA256 verified"

    log_info "Extracting OTP to cache directory..."
    local temp_extract_dir="${ERLMCP_ROOT}/.erlmcp/temp-otp-extract"
    mkdir -p "$temp_extract_dir"

    if ! tar xzf "$tarball" -C "$temp_extract_dir"; then
        log_error "Failed to extract pre-built OTP"
        rm -rf "$temp_extract_dir" "$tarball"
        return 1
    fi

    # Handle tar structure: check if contents are in a subdirectory
    local extracted_content
    extracted_content=$(ls -d "$temp_extract_dir"/* 2>/dev/null | head -1)

    if [[ -z "$extracted_content" ]]; then
        log_error "No files extracted from tarball"
        rm -rf "$temp_extract_dir" "$tarball"
        return 1
    fi

    # Move to final location
    rm -rf "$OTP_CACHE_DIR"
    if [[ -d "$extracted_content/bin" && -d "$extracted_content/lib" ]]; then
        # Tarball contains otp-28.3.1 directory with bin/lib subdirs
        mv "$extracted_content" "$OTP_CACHE_DIR"
    else
        # Tarball structure has bin/lib at top level
        mv "$temp_extract_dir" "$OTP_CACHE_DIR"
    fi

    rm -rf "$temp_extract_dir" "$tarball"

    # Verify the pre-built binary actually runs on this platform
    # (catches cross-platform issues, e.g. macOS build on Linux)
    if [[ -f "${OTP_CACHE_DIR}/bin/erl" ]]; then
        # Patch hardcoded ROOTDIR in erl script to use local path
        local erl_script="${OTP_CACHE_DIR}/bin/erl"
        local lib_erlang="${OTP_CACHE_DIR}/lib/erlang"
        if [[ -d "$lib_erlang" ]]; then
            sed -i "s|ROOTDIR=\".*\"|ROOTDIR=\"${lib_erlang}\"|" "$erl_script" 2>/dev/null || true
        fi

        local test_major
        test_major=$(get_otp_major_version "${OTP_CACHE_DIR}/bin/erl")
        if ! is_otp_version_acceptable "$test_major"; then
            log_error "Pre-built binary does not run on this platform (got version: $test_major)"
            rm -rf "$OTP_CACHE_DIR"
            return 1
        fi
    fi

    log_success "Pre-built OTP installed and verified"
    return 0
}

#==============================================================================
# Phase 2D: Build from Source (fallback)
#==============================================================================

build_otp_from_source() {
    log_phase "2D/5" "Building OTP from source (slow path, ~6m)"

    log_info "URL: $OTP_RELEASE_URL"
    mkdir -p "$BUILD_TEMP_DIR"
    cd "$BUILD_TEMP_DIR"

    log_info "Downloading otp_src_${REQUIRED_OTP_VERSION}.tar.gz..."
    local tarball="otp_src_${REQUIRED_OTP_VERSION}.tar.gz"
    if ! curl -fsSL -o "$tarball" "$OTP_RELEASE_URL" 2>&1 | tee -a "$LOG_FILE"; then
        log_error "Failed to download OTP source"
        return 1
    fi

    log_success "Downloaded $(ls -lh "$tarball" | awk '{print $5}')"

    log_info "Extracting archive..."
    if ! tar xzf "$tarball"; then
        log_error "Failed to extract OTP source"
        return 1
    fi

    local src_dir
    src_dir=$(ls -d otp_src_*/ 2>/dev/null | head -1)
    cd "$src_dir"

    log_info "Configuring (prefix: $OTP_CACHE_DIR)..."
    if ! ./configure --prefix="$OTP_CACHE_DIR" --disable-debug --disable-documentation 2>&1 | tee -a "$LOG_FILE" | tail -20; then
        log_error "Configure failed"
        return 1
    fi

    log_info "Building (using $CPU_COUNT CPUs)..."
    if ! make -j "$CPU_COUNT" 2>&1 | tee -a "$LOG_FILE" | tail -20; then
        log_error "Build failed"
        return 1
    fi

    log_info "Installing..."
    if ! make install 2>&1 | tee -a "$LOG_FILE" | tail -20; then
        log_error "Install failed"
        return 1
    fi
    log_success "Installation complete"

    return 0
}

#==============================================================================
# Phase 4: Verify & Cache
#==============================================================================

verify_otp_build() {
    log_phase "2E/5" "Verifying OTP build"

    if [[ ! -f "$OTP_BIN" ]]; then
        log_error "OTP binary not found at $OTP_BIN"
        return 1
    fi

    log_info "Verifying OTP binary: $OTP_BIN"

    # Test if binary is executable and works
    local built_version
    if ! built_version=$("$OTP_BIN" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>&1); then
        log_error "Failed to execute OTP binary: $built_version"
        return 1
    fi

    if [[ -z "$built_version" ]]; then
        log_error "Failed to detect OTP version"
        return 1
    fi

    # Check major version (e.g., "28" matches "28.3.1")
    local required_major="${REQUIRED_OTP_VERSION%%.*}"
    if [[ "$built_version" != "$required_major" ]]; then
        log_error "OTP version mismatch: built=$built_version, required major=$required_major"
        return 1
    fi

    log_success "OTP $built_version verified and cached at $OTP_CACHE_DIR"

    # Show build info
    local erts_version
    erts_version=$("$OTP_BIN" -noshell -eval 'io:format("~s", [erlang:system_info(version)]), halt().' 2>/dev/null || echo "unknown")
    log_info "ERTS Version: $erts_version"

    log_success "Build complete"
    return 0
}

#==============================================================================
# Phase 3: Environment Setup
#==============================================================================

setup_environment() {
    log_phase "3/5" "Environment setup"

    # IMPORTANT: System bins FIRST to preserve commands like head, tail, etc.
    # OTP bin appended at end
    export PATH="/usr/bin:/bin:/usr/local/bin:/usr/local/sbin:${OTP_CACHE_DIR}/bin:${PATH}"
    export CLAUDE_CODE_REMOTE=true
    export ERLMCP_PROFILE=cloud
    export ERLMCP_CACHE="${ERLMCP_ROOT}/.erlmcp/cache/"
    export TERM=dumb
    export REBAR_COLOR=none
    export ERL_AFLAGS="-kernel shell_history enabled"

    mkdir -p "$ERLMCP_CACHE"

    # Persist environment variables across shell invocations (Claude Code on the web)
    if [[ -n "${CLAUDE_ENV_FILE:-}" ]]; then
        {
            echo "export PATH=\"${OTP_CACHE_DIR}/bin:\$PATH\""
            echo "export CLAUDE_CODE_REMOTE=true"
            echo "export ERLMCP_PROFILE=cloud"
            echo "export ERLMCP_CACHE=\"${ERLMCP_CACHE}\""
            echo "export TERM=dumb"
            echo "export REBAR_COLOR=none"
            echo "export ERL_AFLAGS=\"-kernel shell_history enabled\""
        } >> "$CLAUDE_ENV_FILE"
        log_info "Environment variables persisted to CLAUDE_ENV_FILE"
    fi

    log_success "Environment variables set"
    log_info "  CLAUDE_CODE_REMOTE=$CLAUDE_CODE_REMOTE"
    log_info "  ERLMCP_PROFILE=$ERLMCP_PROFILE"
    log_info "  ERLMCP_CACHE=$ERLMCP_CACHE"
    log_info "  PATH includes ${OTP_CACHE_DIR}/bin"
    log_info "  ERLMCP_PROFILE=$ERLMCP_PROFILE"
    return 0
}

#==============================================================================
# Write Environment File (for other hooks to source)
#==============================================================================

write_env_file() {
    local env_file="${ERLMCP_ROOT}/.erlmcp/env.sh"
    local env_dir
    env_dir=$(dirname "$env_file")
    mkdir -p "$env_dir"

    cat > "$env_file" <<EOF
# erlmcp environment - generated by SessionStart.sh
# Source this file to get OTP 28+ environment in other hooks
export ERLMCP_OTP_BIN_PATH="${OTP_CACHE_DIR}/bin"
export ERLMCP_OTP_VERSION="${REQUIRED_OTP_VERSION}"
export ERLMCP_OTP_BIN="${OTP_BIN}"
export ERLMCP_CACHE="${ERLMCP_ROOT}/.erlmcp/cache/"
# System bins FIRST to preserve standard commands
export PATH="/usr/bin:/bin:/usr/local/bin:/usr/local/sbin:\${ERLMCP_OTP_BIN_PATH}:\$PATH"
export CLAUDE_CODE_REMOTE=true
export ERLMCP_PROFILE=cloud
export TERM=dumb
export REBAR_COLOR=none
export ERL_AFLAGS="-kernel shell_history enabled"
EOF

    log_success "Environment file written: $env_file"
    return 0
}

#==============================================================================
# Phase 4: Lock File
#==============================================================================

create_lock_file() {
    log_phase "4/5" "Lock file creation"

    mkdir -p "$(dirname "$LOCK_FILE")"
    echo "$REQUIRED_OTP_VERSION" > "$LOCK_FILE"

    log_success "Lock file created: $LOCK_FILE"
    return 0
}

#==============================================================================
# Phase 5: Ensure rebar3 + project build
#==============================================================================

ensure_rebar3() {
    if [[ -f "$REBAR3_BIN" ]] && [[ -x "$REBAR3_BIN" ]]; then
        log_info "rebar3 already cached at $REBAR3_BIN"
        return 0
    fi

    log_info "Downloading rebar3..."
    mkdir -p "$(dirname "$REBAR3_BIN")"
    if curl -fsSL -o "$REBAR3_BIN" "$REBAR3_URL" 2>&1 | tee -a "$LOG_FILE"; then
        chmod +x "$REBAR3_BIN"
        log_success "rebar3 downloaded"
        return 0
    else
        log_error "Failed to download rebar3"
        return 1
    fi
}

patch_cowlib_otp28() {
    # cowlib <2.16.0 has an unbound type variable in cow_sse.erl that
    # OTP 28 treats as a hard error. Patch it in-place after deps are fetched.
    local cow_sse="${ERLMCP_ROOT}/_build/default/lib/cowlib/src/cow_sse.erl"

    if [[ ! -f "$cow_sse" ]]; then
        return 0
    fi

    # Check if patch is already applied (look for "when State :: state()")
    if grep -q "when State :: state()" "$cow_sse" 2>/dev/null; then
        log_info "cowlib cow_sse.erl already patched for OTP 28"
        return 0
    fi

    # Check if the buggy pattern exists
    if grep -q 'State} | {more, State}\.' "$cow_sse" 2>/dev/null; then
        log_info "Patching cowlib cow_sse.erl for OTP 28 compatibility..."
        # Replace the two-line type spec with the fixed version
        sed -i 's/-spec parse(binary(), state())/-spec parse(binary(), State)/' "$cow_sse"
        sed -i 's/\t-> {event, parsed_event(), State} | {more, State}\./\t-> {event, parsed_event(), State} | {more, State}\n\twhen State :: state()./' "$cow_sse"
        log_success "cowlib cow_sse.erl patched"
    else
        log_info "cowlib cow_sse.erl does not need patching"
    fi

    return 0
}

ensure_project_compiled() {
    log_phase "5/5" "Project build (rebar3 deps + compile)"

    if ! ensure_rebar3; then
        log_error "Cannot build project without rebar3"
        return 1
    fi

    cd "$ERLMCP_ROOT"

    # Check if project is already compiled (beam files exist)
    local beam_count
    beam_count=$(find "${ERLMCP_ROOT}/_build/default/lib/cre/ebin" -name "*.beam" 2>/dev/null | wc -l)
    if [[ "$beam_count" -gt 0 ]]; then
        log_info "Project already compiled ($beam_count beam files), verifying..."
        if "$REBAR3_BIN" compile 2>&1 | tee -a "$LOG_FILE" | tail -5; then
            log_success "Project compilation verified"
            return 0
        fi
        log_info "Re-compilation needed..."
    fi

    # Fetch deps
    log_info "Fetching dependencies..."
    if ! "$REBAR3_BIN" get-deps 2>&1 | tee -a "$LOG_FILE" | tail -10; then
        log_error "Failed to fetch dependencies"
        return 1
    fi
    log_success "Dependencies fetched"

    # Patch cowlib for OTP 28
    patch_cowlib_otp28

    # Compile
    log_info "Compiling project..."
    if ! "$REBAR3_BIN" compile 2>&1 | tee -a "$LOG_FILE" | tail -10; then
        log_error "Compilation failed"
        return 1
    fi
    log_success "Project compiled successfully"
    return 0
}

#==============================================================================
# Cleanup
#==============================================================================

cleanup() {
    if [[ -d "$BUILD_TEMP_DIR" ]]; then
        rm -rf "$BUILD_TEMP_DIR"
    fi
}

#==============================================================================
# Main
#==============================================================================

main() {
    init_logging

    log_info "Starting SessionStart.sh (v$SCRIPT_VERSION)"
    log_info "Platform: $PLATFORM"

    local otp_ready=false

    # Phase 1: Check cache
    log_phase "1/5" "Cache check"
    if is_otp_cached; then
        otp_ready=true
    else
        log_info "OTP not cached, attempting acquisition..."

        # Phase 2A: Platform-specific paths
        if [[ "$PLATFORM" == "macos" ]]; then
            # Phase 2B: Search existing OTP (macOS)
            log_phase "2B/5" "Searching existing OTP (macOS)"

            local existing_erl
            if existing_erl=$(search_existing_otp_macos); then
                # Link existing OTP to cache
                local existing_bin_dir
                existing_bin_dir=$(dirname "$existing_erl")
                mkdir -p "${OTP_CACHE_DIR}/bin"

                for binary in "$existing_bin_dir"/*; do
                    if [[ -f "$binary" ]]; then
                        ln -sf "$binary" "${OTP_CACHE_DIR}/bin/$(basename "$binary")" 2>/dev/null || true
                    fi
                done
                otp_ready=true
            else
                log_info "Pre-built OTP verification failed, cleaning up for fallback..."
                rm -rf "$OTP_CACHE_DIR"
            fi

        elif [[ "$PLATFORM" == "linux" ]]; then
            # Phase 2C: Pre-built (Linux fast path)
            if download_prebuilt_otp; then
                otp_ready=true
            else
                log_info "Pre-built unavailable, falling back to source build..."
            fi
        fi

        # Phase 2D: Source build (fallback, all platforms)
        if [[ "$otp_ready" != "true" ]]; then
            if build_otp_from_source; then
                otp_ready=true
            fi
        fi
    fi

    if [[ "$otp_ready" != "true" ]]; then
        log_error "FATAL: All OTP acquisition methods failed"
        cleanup
        exit 1
    fi

    # Phase 3: Environment
    setup_environment
    write_env_file

    # Phase 4: Lock file
    create_lock_file

    # Phase 5: Project build (rebar3 + deps + compile)
    ensure_project_compiled || log_info "Project build skipped or failed (non-fatal)"

    log_success "SessionStart complete"
    cleanup
    exit 0
}

main "$@"
