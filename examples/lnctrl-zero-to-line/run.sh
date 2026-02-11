#!/usr/bin/env bash
# LineController Factory - Quick Demo
# Demonstrates the "." operator manufacturing pipeline

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

echo "================================================================"
echo "LineController Factory - Manufacturing Demo"
echo "================================================================"
echo ""
echo "This demo shows ontology-driven Erlang/OTP generation using"
echo "the '.' operator (represented as ./bin/dot in this example)."
echo ""

# Step 1: Validate
echo "[1/4] Validating ontology and templates..."
./bin/dot validate
echo ""

# Step 2: Generate
echo "[2/4] Running manufacturing pipeline..."
./bin/dot sync
echo ""

# Step 3: Show receipt
echo "[3/4] Viewing build receipt..."
./bin/dot receipt
echo ""

# Step 4: Show generated files
echo "[4/4] Generated artifacts:"
echo ""
echo "Ontology Input:"
find ontology -name "*.ttl" | sed 's/^/  /'
echo ""
echo "SPARQL Queries:"
find sparql -name "*.sparql" | sed 's/^/  /'
echo ""
echo "Tera Templates:"
find templates -name "*.tera" | sed 's/^/  /'
echo ""
echo "Generated Outputs (would be created by full ggen):"
echo "  src/generated/order_fulfillment_line_plan.erl"
echo "  src/order_fulfillment_line_cb.erl"
echo "  src/order_fulfillment_line_app.erl"
echo "  src/order_fulfillment_line_sup.erl"
echo "  src/order_fulfillment_line.app.src"
echo "  test/order_fulfillment_line_tests.erl"
echo "  rebar.config"
echo ""

echo "================================================================"
echo "Demo Complete!"
echo "================================================================"
echo ""
echo "Next Steps:"
echo "  1. Install ggen: cargo install ggen-cli"
echo "  2. Run full pipeline: ./bin/dot sync"
echo "  3. Compile: rebar3 compile"
echo "  4. Test: rebar3 eunit"
echo ""
echo "Read README.md for the full beginner guide."
echo ""
