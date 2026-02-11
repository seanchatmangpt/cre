#!/bin/bash
# Test deterministic generation - verify same ontology produces identical output

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "═══════════════════════════════════════════════════════════════"
echo "Deterministic Generation Test"
echo "Verifying that ggen sync produces identical output on repeat"
echo "═══════════════════════════════════════════════════════════════"
echo

# Create temporary directories for runs
RUN1_DIR=$(mktemp -d)
RUN2_DIR=$(mktemp -d)

cleanup() {
    echo
    echo "Cleaning up temporary directories..."
    rm -rf "$RUN1_DIR" "$RUN2_DIR"
}
trap cleanup EXIT

# Function to run generation and capture output
run_generation() {
    local run_name=$1
    local output_dir=$2

    echo
    echo "[$run_name] Running generation..."

    # Clean apps directory
    rm -rf apps
    mkdir -p apps

    # Run generation
    if command -v ggen &> /dev/null; then
        ggen sync > /dev/null 2>&1
    else
        cargo run --manifest-path ../../Cargo.toml -- sync > /dev/null 2>&1
    fi

    # Copy generated files to output directory
    cp -r apps "$output_dir/"

    # Calculate hash of all generated files
    local hash=$(find apps -name "*.erl" -type f -exec sha256sum {} + | sort | sha256sum | cut -d' ' -f1)
    echo "$hash" > "$output_dir/hash.txt"

    # Count files
    local erl_count=$(find apps -name "*.erl" -type f | wc -l)
    local app_count=$(find apps -maxdepth 1 -type d | tail -n +2 | wc -l)

    echo "[$run_name] Generated $erl_count Erlang files in $app_count apps"
    echo "[$run_name] Output hash: ${hash:0:16}..."
}

# Run generation twice
run_generation "RUN 1" "$RUN1_DIR"
sleep 1  # Small delay to ensure different timestamps if they were included
run_generation "RUN 2" "$RUN2_DIR"

# Compare hashes
echo
echo "═══════════════════════════════════════════════════════════════"
echo "Comparing Results"
echo "═══════════════════════════════════════════════════════════════"

HASH1=$(cat "$RUN1_DIR/hash.txt")
HASH2=$(cat "$RUN2_DIR/hash.txt")

echo "Run 1 hash: ${HASH1:0:16}..."
echo "Run 2 hash: ${HASH2:0:16}..."
echo

if [ "$HASH1" = "$HASH2" ]; then
    echo "✓ DETERMINISTIC: Both runs produced identical output"
    echo "  Full hash: $HASH1"

    # File-by-file verification
    echo
    echo "Verifying file-by-file consistency..."
    diff -r "$RUN1_DIR/apps" "$RUN2_DIR/apps" > /dev/null 2>&1

    if [ $? -eq 0 ]; then
        echo "✓ All files are byte-for-byte identical"
    else
        echo "⚠ Some files differ (may include timestamps or randomness)"
        diff -r "$RUN1_DIR/apps" "$RUN2_DIR/apps" | head -20
    fi

    # Save proof
    mkdir -p evidence
    cat > evidence/deterministic_proof.json <<EOF
{
  "test": "deterministic_generation",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "result": "pass",
  "run1_hash": "$HASH1",
  "run2_hash": "$HASH2",
  "hashes_match": true,
  "generator": "ggen-sync",
  "conclusion": "Generation is deterministic - same ontology produces identical output"
}
EOF

    echo
    echo "Proof saved to evidence/deterministic_proof.json"

    exit 0
else
    echo "✗ NON-DETERMINISTIC: Runs produced different output"
    echo "  Run 1: $HASH1"
    echo "  Run 2: $HASH2"

    # Show differences
    echo
    echo "Analyzing differences..."
    diff -r "$RUN1_DIR/apps" "$RUN2_DIR/apps" | head -50

    # Save proof of failure
    mkdir -p evidence
    cat > evidence/deterministic_proof.json <<EOF
{
  "test": "deterministic_generation",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "result": "fail",
  "run1_hash": "$HASH1",
  "run2_hash": "$HASH2",
  "hashes_match": false,
  "generator": "ggen-sync",
  "conclusion": "Generation is non-deterministic - likely includes timestamps or random values"
}
EOF

    echo
    echo "Failure report saved to evidence/deterministic_proof.json"

    exit 1
fi
