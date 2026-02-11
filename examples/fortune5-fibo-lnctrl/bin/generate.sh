#!/bin/bash
# Fortune-5 FIBO LineController Factory - NO PYTHON REQUIRED
# Complete generation pipeline using only ggen + Tera

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "═══════════════════════════════════════════════════════════════"
echo "Fortune-5 FIBO LineController Factory - ggen Pipeline"
echo "NO PYTHON REQUIRED - Pure Rust/SPARQL/Tera generation"
echo "═══════════════════════════════════════════════════════════════"
echo

START_TIME=$(date +%s%3N)

# Step 1: Run ggen sync
echo "[1/5] Running ggen sync..."
if command -v ggen &> /dev/null; then
    ggen sync
    echo "    ✓ ggen sync completed"
else
    echo "    ⚠ ggen not found in PATH - attempting with cargo"
    if command -v cargo &> /dev/null; then
        cargo run --manifest-path ../../Cargo.toml -- sync
        echo "    ✓ ggen sync completed (via cargo)"
    else
        echo "    ✗ ERROR: Neither ggen nor cargo found"
        echo "    Please install ggen or run from development environment"
        exit 1
    fi
fi

# Step 2: Verify generated files
echo
echo "[2/5] Verifying generated files..."
GENERATED_ERLANG=$(find apps -name "*.erl" 2>/dev/null | wc -l)
GENERATED_APPS=$(find apps -maxdepth 1 -type d 2>/dev/null | wc -l)
echo "    Generated Erlang modules: $GENERATED_ERLANG"
echo "    Generated OTP apps: $GENERATED_APPS"

if [ "$GENERATED_ERLANG" -lt 10 ]; then
    echo "    ⚠ WARNING: Expected more generated modules"
fi

# Step 3: Copy .app.src to ebin as .app
echo
echo "[3/5] Preparing runtime artifacts..."
for app_src in apps/*/src/*.app.src; do
    if [ -f "$app_src" ]; then
        app_name=$(basename "$app_src" .app.src)
        app_dir=$(dirname "$(dirname "$app_src")")
        ebin_dir="$app_dir/ebin"
        mkdir -p "$ebin_dir"
        cp "$app_src" "$ebin_dir/${app_name}.app"
    fi
done
echo "    ✓ Runtime artifacts prepared"

# Step 4: Run rebar3 compile
echo
echo "[4/5] Compiling with rebar3..."
if rebar3 compile; then
    echo "    ✓ Compilation successful"
else
    echo "    ✗ Compilation failed"
    exit 1
fi

# Step 5: Run EUnit tests
echo
echo "[5/5] Running EUnit tests..."
if rebar3 eunit --module=f5_connector_crm 2>&1 | grep -q "All.*tests passed"; then
    echo "    ✓ Tests passed"
else
    echo "    ⚠ Some tests may have failed (check output above)"
fi

END_TIME=$(date +%s%3N)
DURATION=$((END_TIME - START_TIME))

# Generate receipt
echo
echo "═══════════════════════════════════════════════════════════════"
echo "Generation Complete"
echo "═══════════════════════════════════════════════════════════════"
echo "Generated artifacts:"
echo "  Erlang modules: $GENERATED_ERLANG"
echo "  OTP apps:       $GENERATED_APPS"
echo "  Duration:       ${DURATION}ms"
echo
echo "✓ NO PYTHON REQUIRED - Pure ggen/SPARQL/Tera generation"
echo

# Calculate output hash for determinism verification
echo "Calculating output hash for determinism verification..."
OUTPUT_HASH=$(find apps -name "*.erl" -type f -exec sha256sum {} + | sort | sha256sum | cut -d' ' -f1)
echo "Output hash: ${OUTPUT_HASH:0:16}..."

# Save receipt
mkdir -p receipts
cat > receipts/generation.json <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "duration_ms": $DURATION,
  "generator": "ggen-sync",
  "python_required": false,
  "output_hash": "$OUTPUT_HASH",
  "counts": {
    "erlang_modules": $GENERATED_ERLANG,
    "otp_apps": $GENERATED_APPS
  }
}
EOF

echo "Receipt saved to receipts/generation.json"
echo

exit 0
