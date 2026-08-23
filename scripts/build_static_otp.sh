#!/usr/bin/env bash
# Build statically-linked Erlang/OTP for gVisor/sandbox environments
#
# ⚠️  DEPRECATED: Use Hex.pm pre-built binaries instead (much faster)
#
# This script builds OTP from source and is meant for:
#   - Custom OTP configurations (patches, features)
#   - Testing custom builds
#   - Building for non-standard platforms
#
# For Claude Code Web, SessionStart.sh uses Hex.pm Bob pre-built binaries
# which are 5-8x faster than this source build approach.
#
# Run this OUTSIDE the sandbox (on your local machine or CI)
# The resulting tarball can be hosted and downloaded by SessionStart.sh
#
# Usage: ./build_static_otp.sh 28.3.1
#
# Output: otp-28.3.1-linux-x86_64-static.tar.gz
#
# Performance comparison:
#   - This script: ~10-20 minutes (source build)
#   - Hex.pm pre-built (recommended): ~2 minutes (download + Install script)

set -euo pipefail

OTP_VERSION="${1:-28.3.1}"
BUILD_DIR="/tmp/otp-static-build-$$"
OUTPUT_DIR="$(pwd)"

log() { echo "[$(date +'%H:%M:%S')] $*"; }
error() { log "ERROR: $*" >&2; exit 1; }

cleanup() {
    log "Cleaning up build directory..."
    rm -rf "$BUILD_DIR"
}
trap cleanup EXIT

log "Building static Erlang/OTP ${OTP_VERSION} for gVisor compatibility"
log "Build directory: $BUILD_DIR"

# Check prerequisites
command -v git >/dev/null || error "git required"
command -v curl >/dev/null || error "curl required"
command -v make >/dev/null || error "make required"
command -v gcc >/dev/null || error "gcc required"

# Install build dependencies
if command -v apt-get >/dev/null 2>&1; then
    log "Installing build dependencies via apt..."
    sudo apt-get update -qq
    sudo apt-get install -y -qq \
        build-essential autoconf libncurses5-dev \
        libssl-dev libwxgtk3.2-dev libgl1-mesa-dev \
        libglu1-mesa-dev libpng-dev libssh-dev \
        unixodbc-dev xsltproc fop libxml2-utils \
        git curl wget
elif command -v brew >/dev/null 2>&1; then
    log "Installing build dependencies via brew..."
    brew install autoconf openssl wxwidgets libssh2 fop
fi

mkdir -p "$BUILD_DIR" && cd "$BUILD_DIR"

# Download OTP source
log "Downloading OTP ${OTP_VERSION} source..."
curl -fsSL -o "otp_src_${OTP_VERSION}.tar.gz" \
    "https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz"

tar xzf "otp_src_${OTP_VERSION}.tar.gz"
cd "otp_src_${OTP_VERSION}"

# Configure with minimal dependencies for sandbox compatibility
log "Configuring OTP with minimal options..."
./configure \
    --prefix="/usr/local" \
    --disable-debug \
    --disable-jit \
    --disable-hipe \
    --disable-native-libs \
    --disable-sctp \
    --disable-dynamic-ssl-lib \
    --disable-sharing-preserving \
    --enable-static-only \
    --without-debugger \
    --without-odbc \
    --without-wx \
    --without-et \
    --without-megaco \
    --without-observer \
    --without-javac \
    --without-docs \
    CFLAGS="-O2 -fPIC" \
    LDFLAGS="-static-libgcc -lpthread -ldl -lm"

log "Building OTP (this will take ~10-20 minutes)..."
make -j"$(nproc)" 2>&1 | tee build.log

log "Installing OTP..."
make install DESTDIR="$BUILD_DIR/otp-install"

# Create relocatable package
log "Creating relocatable package..."
cd "$BUILD_DIR/otp-install"

# Adjust ROOTDIR references for relocatable install
find . -name "erl" -type f -exec sed -i 's|ROOTDIR=.*|ROOTDIR=$(cd "$(dirname "$(dirname "$0")")") && echo "$ROOTDIR"|g' {} \;

# Package it up
log "Packaging..."
tar czf "$OUTPUT_DIR/otp-${OTP_VERSION}-linux-x86_64-static.tar.gz" .

log ""
log "========================================"
log "SUCCESS!"
log "========================================"
log "Output: $OUTPUT_DIR/otp-${OTP_VERSION}-linux-x86_64-static.tar.gz"
log ""
log "⚠️  NOTE: For Claude Code Web, use Hex.pm pre-built instead"
log ""
log "SessionStart.sh now uses Hex.pm Bob builds by default:"
log "  - Much faster (~2 min vs ~10-20 min)"
log "  - Already tested and verified"
log "  - Includes Install script for gVisor compatibility"
log ""
log "Only use this custom build for:"
log "  1. Custom OTP patches or configuration"
log "  2. Non-standard features or patches"
log "  3. Testing custom builds in CI/CD"
log ""
log "If you need to use this custom build:"
log "1. Upload this file to a URL accessible by Claude Code Web"
log "2. Update SessionStart.sh download_static_binary() URLs"
log "3. The sandbox will download and extract it automatically"
log ""
log "Example hosting options:"
log "  - GitHub Releases: https://github.com/USER/REPO/releases"
log "  - S3: aws s3 cp otp-*.tar.gz s3://my-bucket/"
log "  - CloudFlare R2: r2 put otp-*.tar.gz"
log "========================================"
