#!/bin/bash
# Install ggen CLI (Rust implementation)

set -e

echo "Installing ggen CLI..."

# Check if cargo is available
if ! command -v cargo &> /dev/null; then
    echo "Error: cargo not found. Please install Rust first:"
    echo "  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    exit 1
fi

# Build ggen
echo "Building ggen (Rust)..."
cd "$(dirname "$0")/ggen-rust"
cargo build --release

# Install to ~/.local/bin
echo "Installing ggen to ~/.local/bin..."
mkdir -p ~/.local/bin
cp target/release/ggen ~/.local/bin/
chmod +x ~/.local/bin/ggen

# Add to PATH if not already there
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
    echo "Added ~/.local/bin to PATH in ~/.bashrc"
    echo "Run: source ~/.bashrc"
fi

echo "✓ ggen installed successfully!"
echo "  Location: ~/.local/bin/ggen"
echo "  Test: ggen help"
