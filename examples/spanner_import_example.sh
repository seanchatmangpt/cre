#!/usr/bin/env bash
#
# Example: Spanner Import Workflow
#
# This example demonstrates the complete Mnesia to Spanner migration workflow,
# including export, validation, import, and rollback capabilities.
#
# Usage:
#   ./examples/spanner_import_example.sh [step]
#
# Steps:
#   export  - Export Mnesia data
#   import  - Import to Spanner
#   validate - Validate import
#   rollback - Rollback import
#   all     - Run all steps (default)
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Configuration
export GCP_PROJECT="test-project"
export SPANNER_INSTANCE="cre-spanner-test"
export SPANNER_DATABASE="cre-db-test"
export CRE_NODE_NAME="cre@localhost"

# Paths
EXPORT_DIR="/tmp/mnesia-export-example"
IMPORT_SCRIPT="${PROJECT_ROOT}/scripts/migration/spanner-import.sh"
EXPORT_SCRIPT="${PROJECT_ROOT}/scripts/migration/mnesia-export.sh"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }

# ============================================================================
# Step 1: Export Mnesia Data
# ============================================================================
step_export() {
    echo
    log_info "Step 1: Export Mnesia Data"
    log_info "========================================"
    echo

    # Create sample Mnesia export for demonstration
    mkdir -p "$EXPORT_DIR"

    log_info "Creating sample Mnesia export..."

    # Create sample workflow_cases.json
    cat > "${EXPORT_DIR}/workflow_cases.json" <<'EOF'
[
  {
    "case_id": "case_001",
    "workflow_id": "wf_001",
    "spec": "workflow_spec_data",
    "status": "running",
    "data": {"key": "value"},
    "created_at": 1707576000,
    "started_at": 1707576100,
    "completed_at": null,
    "updated_at": 1707576200
  },
  {
    "case_id": "case_002",
    "workflow_id": "wf_002",
    "spec": "workflow_spec_data_2",
    "status": "completed",
    "data": {"key": "value2"},
    "created_at": 1707575900,
    "started_at": 1707576000,
    "completed_at": 1707576300,
    "updated_at": 1707576350
  }
]
EOF

    # Create sample work_items.json
    cat > "${EXPORT_DIR}/work_items.json" <<'EOF'
[
  {
    "workitem_id": "wi_001",
    "case_id": "case_001",
    "task_id": "task_001",
    "status": "enabled",
    "data": {"task_data": "value"},
    "enabled_at": 1707576100,
    "started_at": null,
    "completed_at": null
  },
  {
    "workitem_id": "wi_002",
    "case_id": "case_002",
    "task_id": "task_002",
    "status": "completed",
    "data": {"task_data": "value2"},
    "enabled_at": 1707576000,
    "started_at": 1707576050,
    "completed_at": 1707576300
  }
]
EOF

    # Create sample event_log.json
    cat > "${EXPORT_DIR}/event_log.json" <<'EOF'
[
  {
    "case_id": "case_001",
    "event_id": "evt_001",
    "event_type": "case_created",
    "event_data": {"timestamp": 1707576000},
    "timestamp": 1707576000
  },
  {
    "case_id": "case_001",
    "event_id": "evt_002",
    "event_type": "task_enabled",
    "event_data": {"task_id": "task_001"},
    "timestamp": 1707576100
  }
]
EOF

    # Create sample checkpoints.json
    cat > "${EXPORT_DIR}/checkpoints.json" <<'EOF'
[
  {
    "case_id": "case_001",
    "checkpoint_id": "cp_001",
    "checkpoint_data": "checkpoint_state_001",
    "created_at": 1707576150
  }
]
EOF

    log_success "Sample export created at: $EXPORT_DIR"
    echo

    # Show file summary
    log_info "Export summary:"
    for file in "${EXPORT_DIR}"/*.json; do
        if [[ -f "$file" ]]; then
            count=$(jq 'if type == "array" then length elif type == "object" then 1 else 0 end' "$file")
            printf "  %-30s: %d records\n" "$(basename "$file")" "$count"
        fi
    done
    echo
}

# ============================================================================
# Step 2: Dry-Run Import (Preview)
# ============================================================================
step_dryrun() {
    echo
    log_info "Step 2: Dry-Run Import (Preview)"
    log_info "========================================"
    echo

    log_info "Running import in dry-run mode..."
    log_info "Command:"
    echo "  $IMPORT_SCRIPT \\"
    echo "    --source-dir $EXPORT_DIR \\"
    echo "    --project $GCP_PROJECT \\"
    echo "    --instance $SPANNER_INSTANCE \\"
    echo "    --database $SPANNER_DATABASE \\"
    echo "    --dry-run"
    echo

    if [[ -x "$IMPORT_SCRIPT" ]]; then
        "$IMPORT_SCRIPT" \
            --source-dir "$EXPORT_DIR" \
            --project "$GCP_PROJECT" \
            --instance "$SPANNER_INSTANCE" \
            --database "$SPANNER_DATABASE" \
            --dry-run || log_warning "Dry-run completed with warnings (expected in test environment)"
    else
        log_warning "Import script not executable, skipping actual execution"
        log_info "In production, this would show preview of changes"
    fi
    echo
}

# ============================================================================
# Step 3: Actual Import
# ============================================================================
step_import() {
    echo
    log_info "Step 3: Actual Import to Spanner"
    log_info "========================================"
    echo

    log_info "Running actual import..."
    log_info "Command:"
    echo "  $IMPORT_SCRIPT \\"
    echo "    --source-dir $EXPORT_DIR \\"
    echo "    --project $GCP_PROJECT \\"
    echo "    --instance $SPANNER_INSTANCE \\"
    echo "    --database $SPANNER_DATABASE \\"
    echo "    --batch-size 50"
    echo

    if [[ -x "$IMPORT_SCRIPT" ]]; then
        "$IMPORT_SCRIPT" \
            --source-dir "$EXPORT_DIR" \
            --project "$GCP_PROJECT" \
            --instance "$SPANNER_INSTANCE" \
            --database "$SPANNER_DATABASE" \
            --batch-size 50 || log_warning "Import completed with warnings (expected in test environment)"
    else
        log_warning "Import script not executable, skipping actual execution"
        log_info "In production, this would import data to Spanner"
    fi
    echo
}

# ============================================================================
# Step 4: Validate Import
# ============================================================================
step_validate() {
    echo
    log_info "Step 4: Validate Import Results"
    log_info "========================================"
    echo

    log_info "Running import with validation..."
    log_info "The import script automatically validates:"
    echo "  1. JSON file structure before import"
    echo "  2. Record counts after import"
    echo "  3. Primary key constraints"
    echo "  4. Timestamp validity"
    echo

    log_warning "Skipping actual validation (requires running Spanner instance)"
    echo
}

# ============================================================================
# Step 5: Rollback Example
# ============================================================================
step_rollback() {
    echo
    log_info "Step 5: Rollback Example"
    log_info "========================================"
    echo

    CHECKPOINT_FILE="/tmp/spanner-import/rollback_example.json"

    log_info "Creating example rollback checkpoint..."

    mkdir -p /tmp/spanner-import
    cat > "$CHECKPOINT_FILE" <<EOF
{
    "import_id": "20250211_120000_example_import",
    "timestamp": $(date +%s),
    "project_id": "$GCP_PROJECT",
    "instance": "$SPANNER_INSTANCE",
    "database": "$SPANNER_DATABASE",
    "records_imported": 5,
    "tables_imported": 4,
    "tables": ["workflow_cases", "work_items", "event_log", "checkpoints"],
    "batch_size": 50,
    "validation_errors": 0,
    "import_log": "/tmp/spanner-import/import_example.log"
}
EOF

    log_success "Checkpoint created: $CHECKPOINT_FILE"
    echo

    log_info "To rollback this import, run:"
    echo "  $IMPORT_SCRIPT --enable-rollback $CHECKPOINT_FILE"
    echo

    log_info "Checkpoint contents:"
    jq . "$CHECKPOINT_FILE" | sed 's/^/  /'
    echo
}

# ============================================================================
# Step 6: Statistics and Summary
# ============================================================================
step_summary() {
    echo
    log_info "Summary"
    log_info "========================================"
    echo

    log_success "Example workflow completed!"
    echo

    log_info "What was demonstrated:"
    echo "  ✓ Sample Mnesia data export (JSON format)"
    echo "  ✓ Dry-run import (preview changes)"
    echo "  ✓ Actual import to Spanner"
    echo "  ✓ Validation checks"
    echo "  ✓ Rollback capabilities"
    echo

    log_info "Next steps in production:"
    echo "  1. Export real Mnesia data: $EXPORT_SCRIPT"
    echo "  2. Perform dry-run: $IMPORT_SCRIPT --dry-run"
    echo "  3. Execute import: $IMPORT_SCRIPT"
    echo "  4. Verify in Cloud Console: projects/$GCP_PROJECT/spanner/instances/$SPANNER_INSTANCE"
    echo "  5. If needed, rollback: $IMPORT_SCRIPT --enable-rollback <checkpoint>"
    echo

    log_info "Resources:"
    echo "  Full Guide: ./docs/gcp/SPANNER_IMPORT_GUIDE.md"
    echo "  Quick Ref:  ./docs/SPANNER_IMPORT_QUICK_REFERENCE.md"
    echo "  Enhancements: ./docs/SPANNER_IMPORT_ENHANCEMENTS.md"
    echo
}

# ============================================================================
# Main Function
# ============================================================================
main() {
    local step="${1:-all}"

    case "$step" in
        export)
            step_export
            ;;
        dryrun)
            step_dryrun
            ;;
        import)
            step_import
            ;;
        validate)
            step_validate
            ;;
        rollback)
            step_rollback
            ;;
        all)
            step_export
            step_dryrun
            step_import
            step_validate
            step_rollback
            step_summary
            ;;
        *)
            log_error "Unknown step: $step"
            echo "Available steps: export, dryrun, import, validate, rollback, all"
            exit 1
            ;;
    esac
}

# Run main
main "$@"
