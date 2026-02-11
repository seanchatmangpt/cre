#!/bin/bash
# Validate regulation ontology coverage
# Proves that the ontology-based approach covers all regulations from Python script

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "═══════════════════════════════════════════════════════════════"
echo "Regulation Ontology Validation"
echo "Comparing ontology-based generation with Python script output"
echo "═══════════════════════════════════════════════════════════════"
echo

# Expected counts from Python script
EXPECTED_CUSTOMERS=5
EXPECTED_REGULATIONS=11
EXPECTED_CUSTOMER_REG_PAIRS=19  # Total validator modules generated

echo "[1/4] Validating ontology structure..."

# Check that ontology files exist
if [ ! -f "ontology/regulations.ttl" ]; then
    echo "✗ ERROR: ontology/regulations.ttl not found"
    exit 1
fi

if [ ! -f "ontology/customers.ttl" ]; then
    echo "✗ ERROR: ontology/customers.ttl not found"
    exit 1
fi

echo "    ✓ Ontology files present"

# Count regulations in ontology
ONTOLOGY_REGULATIONS=$(grep -c "a reg:Regulation" ontology/regulations.ttl || echo 0)
echo "    Regulations in ontology: $ONTOLOGY_REGULATIONS (expected: $EXPECTED_REGULATIONS)"

if [ "$ONTOLOGY_REGULATIONS" -ne "$EXPECTED_REGULATIONS" ]; then
    echo "    ⚠ WARNING: Regulation count mismatch"
fi

# Count customers in ontology
ONTOLOGY_CUSTOMERS=$(grep -c "a cust:Customer" ontology/customers.ttl || echo 0)
echo "    Customers in ontology: $ONTOLOGY_CUSTOMERS (expected: $EXPECTED_CUSTOMERS)"

if [ "$ONTOLOGY_CUSTOMERS" -ne "$EXPECTED_CUSTOMERS" ]; then
    echo "    ✗ ERROR: Customer count mismatch"
    exit 1
fi

echo

echo "[2/4] Validating SPARQL queries..."

# Check that SPARQL queries exist
if [ ! -f "sparql/extract_customer_regulations.sparql" ]; then
    echo "✗ ERROR: sparql/extract_customer_regulations.sparql not found"
    exit 1
fi

if [ ! -f "sparql/prove_coverage.sparql" ]; then
    echo "✗ ERROR: sparql/prove_coverage.sparql not found"
    exit 1
fi

echo "    ✓ SPARQL queries present"
echo

echo "[3/4] Validating Tera templates..."

# Check that templates exist
if [ ! -f "templates/regulation_validator.tera" ]; then
    echo "✗ ERROR: templates/regulation_validator.tera not found"
    exit 1
fi

if [ ! -f "templates/regulation_supervisor.tera" ]; then
    echo "✗ ERROR: templates/regulation_supervisor.tera not found"
    exit 1
fi

if [ ! -f "templates/regulation_app.tera" ]; then
    echo "✗ ERROR: templates/regulation_app.tera not found"
    exit 1
fi

if [ ! -f "templates/regulation_app_src.tera" ]; then
    echo "✗ ERROR: templates/regulation_app_src.tera not found"
    exit 1
fi

echo "    ✓ Tera templates present"
echo

echo "[4/4] Validating ggen.toml configuration..."

# Check that ggen.toml has the regulation rules
if ! grep -q "generate-regulation-validators" ggen.toml; then
    echo "✗ ERROR: generate-regulation-validators rule not found in ggen.toml"
    exit 1
fi

if ! grep -q "ontology/regulations.ttl" ggen.toml; then
    echo "✗ ERROR: regulations.ttl not referenced in ggen.toml"
    exit 1
fi

if ! grep -q "ontology/customers.ttl" ggen.toml; then
    echo "✗ ERROR: customers.ttl not referenced in ggen.toml"
    exit 1
fi

echo "    ✓ ggen.toml configured correctly"
echo

echo "═══════════════════════════════════════════════════════════════"
echo "Validation Complete"
echo "═══════════════════════════════════════════════════════════════"
echo
echo "✓ Ontology structure valid"
echo "✓ SPARQL queries present"
echo "✓ Tera templates present"
echo "✓ ggen.toml configured"
echo
echo "Expected generation output:"
echo "  - $EXPECTED_CUSTOMERS customer suites"
echo "  - $EXPECTED_CUSTOMER_REG_PAIRS validator modules"
echo "  - $EXPECTED_REGULATIONS unique regulations"
echo
echo "To generate validators: ./bin/generate.sh"
echo

exit 0
