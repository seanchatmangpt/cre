#!/bin/bash
# FIBO Ontology Linter - Validation script
set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

ONTOLOGY_FILE="${1:-ontology/f5_line_control.ttl}"
OUTPUT_FILE="${2:-docs/FIBO_ALIGNMENT_REPORT.md}"

echo "=========================================="
echo "FIBO Ontology Linter"
echo "=========================================="
echo "Project: $PROJECT_ROOT"
echo "Ontology: $ONTOLOGY_FILE"
echo "Output: $OUTPUT_FILE"
echo ""

# Ensure ontology file exists
if [ ! -f "$ONTOLOGY_FILE" ]; then
    echo "Error: Ontology file not found: $ONTOLOGY_FILE"
    exit 1
fi

# Create output directory if needed
mkdir -p "$(dirname "$OUTPUT_FILE")"

# Compile the linter module
echo "Compiling FIBO linter..."
cd apps/f5_ontology_tools
erlc -o ebin src/fibo_linter.erl src/f5_ontology_tools_app.erl src/f5_ontology_tools_sup.erl

cd "$PROJECT_ROOT"

# Run the linter
echo ""
echo "Running FIBO linter..."
./scripts/fibo_linter lint "$ONTOLOGY_FILE" "$OUTPUT_FILE"

EXIT_CODE=$?

echo ""
echo "=========================================="
if [ $EXIT_CODE -eq 0 ]; then
    echo "✓ FIBO linting completed successfully"
else
    echo "⚠️  FIBO linting completed with issues"
fi
echo "=========================================="

exit $EXIT_CODE
