#!/bin/bash
# Pre-commit hook for FIBO ontology validation
# To install: ln -s ../../scripts/pre-commit-fibo-linter.sh .git/hooks/pre-commit

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

ONTOLOGY_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.ttl$' || true)

if [ -z "$ONTOLOGY_FILES" ]; then
    # No ontology files changed, skip linting
    exit 0
fi

echo "=========================================="
echo "Pre-commit: FIBO Ontology Validation"
echo "=========================================="

LINT_FAILED=0

for FILE in $ONTOLOGY_FILES; do
    echo ""
    echo "Checking: $FILE"

    # Run FIBO linter
    if ! bash scripts/run_fibo_linter.sh "$FILE" "docs/FIBO_ALIGNMENT_REPORT_${FILE//\//_}.md"; then
        echo "⚠️  FIBO linting failed for: $FILE"
        LINT_FAILED=1
    fi
done

echo ""
echo "=========================================="

if [ $LINT_FAILED -eq 1 ]; then
    echo "⚠️  Pre-commit FIBO validation found issues"
    echo ""
    echo "To bypass this check (not recommended):"
    echo "  git commit --no-verify"
    echo ""
    echo "To fix issues:"
    echo "  1. Review FIBO_ALIGNMENT_REPORT.md"
    echo "  2. Update ontology to use FIBO vocabulary"
    echo "  3. Check ontology/fibo_alignment.ttl for mappings"
    exit 1
else
    echo "✓ Pre-commit FIBO validation passed"
    exit 0
fi
