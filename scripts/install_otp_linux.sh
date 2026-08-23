#!/usr/bin/env bash
# Install OTP 28 on Debian-based Linux using kerl
#
# This script is a FALLBACK method for local development
# NOT used in Claude Code Web (SessionStart.sh has better options)
#
# Hierarchy (from SessionStart.sh):
#   1. ✅ Cache (fastest, if available)
#   2. ✅ System OTP (if installed)
#   3. ✅ Hex.pm pre-built (Claude Code Web only)
#   4. 🔧 This kerl-based build (fallback, ~15-20 min)
#   5. 📖 Source build (last resort, ~10-20 min)
#
# Use this when:
#   - You want to test kerl locally
#   - You need to build OTP for custom configurations
#   - You prefer kerl over other methods on your local machine
#
# Performance: ~15-20 minutes (includes dependencies, compilation)

set -euo pipefail

readonly OTP_VERSION="28.3.1"
readonly KERL_DIR="$HOME/.kerl"
readonly OTP_INSTALL_DIR="$HOME/.erlmcp/otp-${OTP_VERSION}"

log_info() { echo "[INFO] $*"; }
log_error() { echo "[ERROR] $*" >&2; }

# Install kerl if not present
install_kerl() {
    if command -v kerl &>/dev/null; then
        log_info "kerl already installed"
        return 0
    fi

    log_info "Installing kerl..."
    mkdir -p "$KERL_DIR"
    curl -fsSL https://raw.githubusercontent.com/kerl/kerl/master/kerl -o "$KERL_DIR/kerl"
    chmod +x "$KERL_DIR/kerl"

    # Add to PATH for this session
    export PATH="$KERL_DIR:$PATH"
}

# Install build dependencies
install_build_deps() {
    if ! command -v apt-get &>/dev/null; then
        log_info "Not a Debian-based system, skipping package installation"
        return 0
    fi

    log_info "Installing build dependencies..."
    sudo apt-get update -qq
    sudo apt-get install -y -qq \
        build-essential autoconf libncurses5-dev \
        libssl-dev libwxgtk3.2-dev libgl1-mesa-dev \
        libglu1-mesa-dev libpng-dev libssh-dev \
        unixodbc-dev xsltproc fop libxml2-utils \
        curl git
}

# Build and install OTP with kerl
build_otp() {
    log_info "Building OTP ${OTP_VERSION} with kerl..."

    # Update available builds
    kerl update releases

    # Build OTP
    kerl build "${OTP_VERSION}" "otp-${OTP_VERSION}" || {
        log_error "kerl build failed"
        return 1
    }

    # Install OTP
    kerl install "otp-${OTP_VERSION}" "$OTP_INSTALL_DIR" || {
        log_error "kerl install failed"
        return 1
    }

    log_info "OTP ${OTP_VERSION} installed to $OTP_INSTALL_DIR"
}

# Main
main() {
    install_kerl
    install_build_deps
    build_otp

    # Source the OTP environment
    # shellcheck source=/dev/null
    . "$OTP_INSTALL_DIR/activate"

    log_info "OTP ${OTP_VERSION} is now active!"
    log_info "Run: source $OTP_INSTALL_DIR/activate"
}

main "$@"
