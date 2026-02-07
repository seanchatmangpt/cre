#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
INSTALL_DIR="${INSTALL_DIR:-/usr/local/bin}"
BINARY_NAME="cre"

echo "==========================================="
echo "CRE Installation Script"
echo "==========================================="
echo ""

check_prerequisites() {
    echo "Checking prerequisites..."

    if ! command -v erlang &> /dev/null && ! command -v erl &> /dev/null; then
        echo "Error: Erlang/OTP is not installed."
        echo "Please install Erlang/OTP 25 or later."
        exit 1
    fi

    if ! command -v rebar3 &> /dev/null; then
        echo "Error: rebar3 is not installed."
        echo "Please install rebar3: https://www.rebar3.org/"
        exit 1
    fi

    erlang_version=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell | sed 's/"//g')
    echo "Found Erlang/OTP version: $erlang_version"
    echo "Found rebar3 at: $(command -v rebar3)"
    echo ""
}

build_project() {
    echo "Building project..."
    cd "$PROJECT_DIR"

    if [ ! -f "rebar.config" ]; then
        echo "Error: rebar.config not found in $PROJECT_DIR"
        exit 1
    fi

    echo "Running: rebar3 compile"
    rebar3 compile

    echo "Building escript..."
    echo "Running: rebar3 escript:build"
    rebar3 escript:build

    if [ ! -f "_build/default/bin/$BINARY_NAME" ]; then
        echo "Error: Failed to build escript"
        exit 1
    fi

    echo "Build successful!"
    echo ""
}

install_binary() {
    echo "Installing binary..."

    if [ ! -d "$INSTALL_DIR" ]; then
        echo "Creating $INSTALL_DIR..."
        mkdir -p "$INSTALL_DIR"
    fi

    if [ ! -w "$INSTALL_DIR" ]; then
        echo "Error: No write permission for $INSTALL_DIR"
        echo "Try running with sudo: sudo ./scripts/install.sh"
        exit 1
    fi

    cp "$PROJECT_DIR/_build/default/bin/$BINARY_NAME" "$INSTALL_DIR/$BINARY_NAME"
    chmod +x "$INSTALL_DIR/$BINARY_NAME"

    echo "Installed: $INSTALL_DIR/$BINARY_NAME"
    echo ""
}

verify_installation() {
    echo "Verifying installation..."

    if command -v "$BINARY_NAME" &> /dev/null; then
        version_info=$("$BINARY_NAME" --version 2>/dev/null || echo "CRE Binary")
        echo "Success: $version_info"
        echo ""
    else
        echo "Warning: $BINARY_NAME not found in PATH"
        echo "You may need to add $INSTALL_DIR to your PATH"
    fi
}

cleanup_on_exit() {
    if [ $? -ne 0 ]; then
        echo ""
        echo "Installation failed! Cleaning up..."
        cd "$PROJECT_DIR"
        make clean > /dev/null 2>&1 || true
    fi
}

trap cleanup_on_exit EXIT

main() {
    check_prerequisites
    build_project
    install_binary
    verify_installation

    echo "==========================================="
    echo "Installation complete!"
    echo "==========================================="
    echo ""
    echo "Quick start:"
    echo "  $BINARY_NAME --help"
    echo ""
}

main
