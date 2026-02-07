#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
INSTALL_DIR="${INSTALL_DIR:-/usr/local/bin}"
BINARY_NAME="cre"

echo "==========================================="
echo "CRE Uninstallation Script"
echo "==========================================="
echo ""

remove_binary() {
    echo "Removing binary..."

    if [ -f "$INSTALL_DIR/$BINARY_NAME" ]; then
        if [ ! -w "$INSTALL_DIR" ]; then
            echo "Error: No write permission for $INSTALL_DIR"
            echo "Try running with sudo: sudo ./scripts/uninstall.sh"
            exit 1
        fi

        rm -f "$INSTALL_DIR/$BINARY_NAME"
        echo "Removed: $INSTALL_DIR/$BINARY_NAME"
    else
        echo "Binary not found: $INSTALL_DIR/$BINARY_NAME"
    fi
    echo ""
}

clean_build_artifacts() {
    echo "Cleaning build artifacts..."

    cd "$PROJECT_DIR"

    if [ -f "Makefile" ]; then
        make clean > /dev/null 2>&1 || echo "Warning: make clean failed"
    else
        if command -v rebar3 &> /dev/null; then
            rebar3 clean > /dev/null 2>&1 || echo "Warning: rebar3 clean failed"
        fi
    fi

    echo "Build artifacts cleaned"
    echo ""
}

verify_uninstall() {
    echo "Verifying uninstallation..."

    if ! command -v "$BINARY_NAME" &> /dev/null; then
        echo "Success: $BINARY_NAME is no longer in PATH"
    else
        echo "Warning: $BINARY_NAME is still accessible"
        echo "It may be installed in a different location:"
        command -v "$BINARY_NAME" || true
    fi
    echo ""
}

main() {
    remove_binary
    clean_build_artifacts
    verify_uninstall

    echo "==========================================="
    echo "Uninstallation complete!"
    echo "==========================================="
    echo ""
}

main
