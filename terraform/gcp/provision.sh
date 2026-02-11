#!/usr/bin/env bash
#
# provision.sh - Terraform provisioning for CRE GCP infrastructure
#
# This script provides a complete Terraform workflow for provisioning
# the GCP infrastructure for the Common Runtime Environment (CRE):
#   - terraform init    (initialize backend and providers)
#   - terraform plan    (validate and preview changes)
#   - terraform apply   (apply configuration)
#
# Includes validation steps, state backup, and outputs of important values.
#
# Usage:
#   ./provision.sh [OPTIONS] [COMMAND]
#
# Commands:
#   init                Initialize Terraform backend and providers
#   plan                Generate and review execution plan
#   apply               Apply Terraform configuration (requires confirmation)
#   destroy             Destroy Terraform resources (requires confirmation)
#   full                Run init → plan → apply workflow (default)
#   output              Display key outputs without applying changes
#   validate            Validate Terraform configuration
#   fmt-check           Check code formatting
#   state-backup        Backup Terraform state file
#
# Options:
#   --project ID        GCP project ID (required for init)
#   --bucket BUCKET     GCS bucket for Terraform state (required for init)
#   --prefix PREFIX     State file prefix (default: gcp/production)
#   --region REGION     Primary region (default: us-central1)
#   --environment ENV   Environment: dev, staging, production (default: production)
#   --auto-approve      Auto-approve apply without confirmation
#   --backup            Backup state before apply (default: true)
#   --no-backup         Skip state backup
#   --dry-run           Show what would be done without executing
#   --verbose           Enable verbose output
#   --json              Output in JSON format
#   --help              Show this help message
#
# Environment Variables:
#   TF_VAR_project_id            GCP project ID
#   TF_VAR_region                Primary region
#   TF_VAR_environment           Environment name
#   GCP_PROJECT                  GCP project ID (fallback)
#   GOOGLE_APPLICATION_CREDENTIALS Path to credentials JSON (optional)
#
# Requirements:
#   - terraform >= 1.5.0
#   - gcloud CLI
#   - gsutil (GCS tools)
#   - jq (optional, for JSON parsing)
#
# Exit Codes:
#   0    Success
#   1    General error
#   2    Validation error
#   3    Init failed
#   4    Plan failed
#   5    Apply failed
#   6    State backup failed
#   7    Credentials error
#
# Examples:
#   # Full workflow with interactive confirmation
#   ./provision.sh --project my-project --bucket tf-state
#
#   # Plan only (no changes)
#   ./provision.sh --project my-project plan
#
#   # Auto-approve apply with backup
#   ./provision.sh --project my-project --auto-approve
#
#   # Dry-run to preview all steps
#   ./provision.sh --project my-project --dry-run
#
#   # Initialize backend for dev environment
#   ./provision.sh --project my-project --bucket tf-state --environment dev init
#
#   # Output cluster endpoint and service accounts
#   ./provision.sh output
#

set -euo pipefail

# =============================================================================
# Script metadata and configuration
# =============================================================================

SCRIPT_NAME="$(basename "$0")"
SCRIPT_VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Terraform paths
TF_DIR="${SCRIPT_DIR}"
TF_VARS_DIR="${SCRIPT_DIR}/terraform.tfvars.d"
STATE_BACKUP_DIR="${PROJECT_ROOT}/.terraform-state-backups"
PLAN_FILE="${TF_DIR}/terraform.tfplan"

# Command defaults
COMMAND="full"
DRY_RUN=false
VERBOSE=false
AUTO_APPROVE=false
BACKUP=true
JSON_OUTPUT=false

# Configuration variables
PROJECT_ID="${TF_VAR_project_id:-}"
GCS_BUCKET="${TERRAFORM_STATE_BUCKET:-}"
STATE_PREFIX="${TERRAFORM_STATE_PREFIX:-gcp/production}"
REGION="${TF_VAR_region:-us-central1}"
ENVIRONMENT="${TF_VAR_environment:-production}"

# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

# Tracking
STEP=0
TOTAL_STEPS=0

# =============================================================================
# Utility functions
# =============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

log_debug() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "${CYAN}[DEBUG]${NC} $*"
    fi
}

show_progress() {
    STEP=$((STEP + 1))
    echo -e "\n${BLUE}[${STEP}/${TOTAL_STEPS}]${NC} $*"
}

command_exists() {
    command -v "$1" &>/dev/null
}

print_usage() {
    cat <<EOF
${SCRIPT_NAME} v${SCRIPT_VERSION} - Terraform provisioning for CRE GCP infrastructure

USAGE:
    ${SCRIPT_NAME} [OPTIONS] [COMMAND]

COMMANDS:
    init              Initialize Terraform backend and providers
    plan              Generate and review execution plan
    apply             Apply Terraform configuration
    destroy           Destroy Terraform resources
    full              Run init → plan → apply workflow (default)
    output            Display key outputs
    validate          Validate configuration syntax
    fmt-check         Check code formatting
    state-backup      Backup Terraform state file
    state-restore     Restore Terraform state from backup

OPTIONS:
    --project ID      GCP project ID (required for init)
    --bucket BUCKET   GCS bucket for state (required for init)
    --prefix PREFIX   State file prefix (default: gcp/production)
    --region REGION   Primary region (default: us-central1)
    --environment ENV Environment: dev, staging, production (default: production)
    --auto-approve    Auto-approve apply without confirmation
    --backup          Backup state before apply (default)
    --no-backup       Skip state backup
    --dry-run         Show what would be done
    --verbose         Enable verbose output
    --json            Output in JSON format
    --help            Show this help message

ENVIRONMENT VARIABLES:
    TF_VAR_project_id             GCP project ID
    TF_VAR_region                 Primary region
    TF_VAR_environment            Environment name
    GCP_PROJECT                   GCP project ID (fallback)
    GOOGLE_APPLICATION_CREDENTIALS Service account credentials

EXAMPLES:
    # Full workflow (init → plan → apply)
    ${SCRIPT_NAME} --project my-project --bucket tf-state

    # Plan only (no changes)
    ${SCRIPT_NAME} --project my-project plan

    # Auto-approve apply
    ${SCRIPT_NAME} --project my-project --auto-approve

    # Output cluster details
    ${SCRIPT_NAME} output

    # Destroy resources
    ${SCRIPT_NAME} --auto-approve destroy

EOF
}

# =============================================================================
# Argument parsing
# =============================================================================

parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --project)
                PROJECT_ID="$2"
                export TF_VAR_project_id="$PROJECT_ID"
                shift 2
                ;;
            --bucket)
                GCS_BUCKET="$2"
                export TERRAFORM_STATE_BUCKET="$2"
                shift 2
                ;;
            --prefix)
                STATE_PREFIX="$2"
                export TERRAFORM_STATE_PREFIX="$2"
                shift 2
                ;;
            --region)
                REGION="$2"
                export TF_VAR_region="$2"
                shift 2
                ;;
            --environment)
                ENVIRONMENT="$2"
                export TF_VAR_environment="$2"
                shift 2
                ;;
            --auto-approve)
                AUTO_APPROVE=true
                shift
                ;;
            --backup)
                BACKUP=true
                shift
                ;;
            --no-backup)
                BACKUP=false
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            --json)
                JSON_OUTPUT=true
                shift
                ;;
            --help|-h)
                print_usage
                exit 0
                ;;
            init|plan|apply|destroy|full|output|validate|fmt-check|state-backup|state-restore)
                COMMAND="$1"
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                print_usage
                exit 2
                ;;
        esac
    done
}

# =============================================================================
# Validation functions
# =============================================================================

validate_requirements() {
    log_info "Validating requirements..."

    local missing=()

    command_exists terraform || missing+=("terraform")
    command_exists gcloud || missing+=("gcloud")

    if [[ ${#missing[@]} -gt 0 ]]; then
        log_error "Missing required tools:"
        for tool in "${missing[@]}"; do
            echo "  - $tool"
        done
        return 1
    fi

    # Check Terraform version
    local tf_version
    tf_version=$(terraform version -json 2>/dev/null | jq -r '.terraform_version' 2>/dev/null || echo "unknown")
    log_debug "Terraform version: $tf_version"

    log_success "All requirements satisfied"
    return 0
}

validate_gcp_auth() {
    log_info "Validating GCP authentication..."

    # Check if gcloud is authenticated
    if ! gcloud auth list --filter=status:ACTIVE --format="value(account)" &>/dev/null; then
        log_error "Not authenticated with gcloud. Run: gcloud auth application-default login"
        return 1
    fi

    # Get current project from gcloud if not set
    if [[ -z "$PROJECT_ID" ]]; then
        PROJECT_ID=$(gcloud config get-value project 2>/dev/null || echo "")
        if [[ -z "$PROJECT_ID" ]]; then
            log_error "GCP project ID not specified and not set in gcloud config"
            log_error "Use --project flag or run: gcloud config set project PROJECT_ID"
            return 1
        fi
        export TF_VAR_project_id="$PROJECT_ID"
        log_info "Using GCP project from gcloud config: $PROJECT_ID"
    fi

    log_success "GCP authentication verified"
    return 0
}

validate_terraform_config() {
    log_info "Validating Terraform configuration..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would validate Terraform configuration"
        return 0
    fi

    cd "$TF_DIR" || return 1

    if terraform validate -json 2>/dev/null | jq -e '.valid' > /dev/null 2>&1; then
        log_success "Terraform configuration is valid"
        return 0
    else
        log_error "Terraform configuration validation failed"
        terraform validate
        return 1
    fi
}

validate_backend_config() {
    if [[ -z "$GCS_BUCKET" ]]; then
        log_error "GCS bucket not specified. Use --bucket flag or set TERRAFORM_STATE_BUCKET"
        return 1
    fi

    log_info "Validating GCS backend bucket: $GCS_BUCKET"

    if ! gsutil ls -b "gs://${GCS_BUCKET}" > /dev/null 2>&1; then
        log_error "GCS bucket not accessible: gs://${GCS_BUCKET}"
        return 1
    fi

    log_success "GCS bucket is accessible"
    return 0
}

# =============================================================================
# GCS state backup functions
# =============================================================================

backup_state() {
    if [[ "$BACKUP" != "true" ]] || [[ -z "$GCS_BUCKET" ]]; then
        return 0
    fi

    log_info "Backing up Terraform state..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would backup state to gs://${GCS_BUCKET}/${STATE_PREFIX}"
        return 0
    fi

    mkdir -p "$STATE_BACKUP_DIR"

    local backup_timestamp
    backup_timestamp=$(date -u +"%Y%m%d_%H%M%S")
    local backup_file="${STATE_BACKUP_DIR}/terraform.tfstate.${backup_timestamp}.backup"

    # Download current state from GCS
    if gsutil cp "gs://${GCS_BUCKET}/${STATE_PREFIX}/terraform.tfstate" "$backup_file" 2>/dev/null; then
        log_success "State backed up to: $backup_file"

        # Compress and keep only last 5 backups
        gzip "$backup_file"
        log_debug "Compressed backup: ${backup_file}.gz"

        # Clean old backups (keep last 5)
        local backup_count
        backup_count=$(ls -1 "${STATE_BACKUP_DIR}"/terraform.tfstate.*.backup.gz 2>/dev/null | wc -l)
        if [[ $backup_count -gt 5 ]]; then
            log_debug "Removing old backups (keeping last 5)..."
            ls -1t "${STATE_BACKUP_DIR}"/terraform.tfstate.*.backup.gz | tail -n +6 | xargs rm -f
        fi

        return 0
    else
        log_warning "Could not backup state from GCS (may not exist yet)"
        return 0
    fi
}

list_state_backups() {
    if [[ ! -d "$STATE_BACKUP_DIR" ]]; then
        log_warning "No state backups found"
        return 0
    fi

    log_info "Available state backups:"
    ls -lh "${STATE_BACKUP_DIR}"/terraform.tfstate.*.backup.gz 2>/dev/null | awk '{print $9, "(" $5 ")"}' || true
}

restore_state() {
    local backup_file="$1"

    if [[ ! -f "$backup_file" ]]; then
        log_error "Backup file not found: $backup_file"
        return 1
    fi

    log_warning "Restoring state from backup: $backup_file"
    log_warning "This will overwrite the current state!"

    # Check if backup is gzipped
    if [[ "$backup_file" == *.gz ]]; then
        local temp_file
        temp_file=$(mktemp)
        gunzip -c "$backup_file" > "$temp_file"
        backup_file="$temp_file"
    fi

    if [[ "$DRY_RUN" != "true" ]]; then
        read -p "Are you sure? (yes/no): " -r confirm
        if [[ "$confirm" != "yes" ]]; then
            log_warning "Restore cancelled"
            return 0
        fi
    fi

    if gsutil cp "$backup_file" "gs://${GCS_BUCKET}/${STATE_PREFIX}/terraform.tfstate"; then
        log_success "State restored from backup"
        return 0
    else
        log_error "Failed to restore state"
        return 1
    fi
}

# =============================================================================
# Terraform operations
# =============================================================================

terraform_init() {
    show_progress "Initializing Terraform"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would run: terraform init -backend-config=bucket=$GCS_BUCKET -backend-config=prefix=$STATE_PREFIX"
        return 0
    fi

    cd "$TF_DIR" || return 1

    local backend_args=(
        "-backend=true"
        "-backend-config=bucket=${GCS_BUCKET}"
        "-backend-config=prefix=${STATE_PREFIX}"
        "-upgrade"
    )

    log_debug "Backend config: bucket=${GCS_BUCKET}, prefix=${STATE_PREFIX}"

    if terraform init "${backend_args[@]}"; then
        log_success "Terraform initialized"
        return 0
    else
        log_error "Terraform init failed"
        return 1
    fi
}

terraform_validate() {
    show_progress "Validating Terraform configuration"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would run: terraform validate"
        return 0
    fi

    cd "$TF_DIR" || return 1

    if terraform validate; then
        log_success "Terraform validation passed"
        return 0
    else
        log_error "Terraform validation failed"
        return 1
    fi
}

terraform_fmt_check() {
    show_progress "Checking Terraform code formatting"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would run: terraform fmt -check -recursive"
        return 0
    fi

    cd "$TF_DIR" || return 1

    if terraform fmt -check -recursive 2>/dev/null; then
        log_success "All files properly formatted"
        return 0
    else
        log_warning "Some files need formatting. Run: terraform fmt -recursive"
        return 0
    fi
}

terraform_plan() {
    show_progress "Generating Terraform plan"

    cd "$TF_DIR" || return 1

    local plan_args=(
        "-out=${PLAN_FILE}"
        "-input=false"
    )

    if [[ "$VERBOSE" == "true" ]]; then
        plan_args+=("-no-color")
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would run: terraform plan ${plan_args[*]}"
        return 0
    fi

    if terraform plan "${plan_args[@]}"; then
        log_success "Plan generated: $PLAN_FILE"

        # Show plan summary
        log_info "Plan summary:"
        terraform show -no-color "$PLAN_FILE" | grep -E "^(Plan|No changes)" || true

        return 0
    else
        log_error "Terraform plan failed"
        return 1
    fi
}

terraform_apply() {
    show_progress "Applying Terraform configuration"

    if [[ ! -f "$PLAN_FILE" ]]; then
        log_error "Plan file not found: $PLAN_FILE"
        log_error "Run 'plan' command first or use 'full' command"
        return 1
    fi

    cd "$TF_DIR" || return 1

    # Show plan summary before applying
    log_info "Plan to apply:"
    terraform show -no-color "$PLAN_FILE" | head -50
    echo "..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would run: terraform apply"
        return 0
    fi

    # Confirm if not auto-approved
    if [[ "$AUTO_APPROVE" != "true" ]]; then
        echo
        log_warning "Review the plan above carefully!"
        read -p "Do you want to apply these changes? (yes/no): " -r confirm
        if [[ "$confirm" != "yes" ]]; then
            log_warning "Apply cancelled"
            return 0
        fi
    fi

    # Backup before applying
    if ! backup_state; then
        if [[ "$BACKUP" == "true" ]]; then
            log_error "State backup failed"
            return 1
        fi
    fi

    log_info "Applying changes..."
    if terraform apply -input=false "$PLAN_FILE"; then
        log_success "Terraform apply completed successfully"
        rm -f "$PLAN_FILE"
        return 0
    else
        log_error "Terraform apply failed"
        return 1
    fi
}

terraform_destroy() {
    show_progress "Planning Terraform destroy"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would plan destroy"
        return 0
    fi

    cd "$TF_DIR" || return 1

    log_warning "This will DESTROY all Terraform-managed resources!"
    log_warning "This action cannot be undone!"
    echo

    # Backup before destroying
    if ! backup_state; then
        log_warning "Could not backup state, continuing anyway..."
    fi

    # Confirm destroy
    if [[ "$AUTO_APPROVE" != "true" ]]; then
        read -p "Type 'destroy' to confirm: " -r confirm
        if [[ "$confirm" != "destroy" ]]; then
            log_warning "Destroy cancelled"
            return 0
        fi
    fi

    log_info "Destroying resources..."
    if terraform destroy -auto-approve -input=false; then
        log_success "Terraform destroy completed"
        return 0
    else
        log_error "Terraform destroy failed"
        return 1
    fi
}

terraform_output() {
    show_progress "Displaying Terraform outputs"

    cd "$TF_DIR" || return 1

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would display outputs"
        return 0
    fi

    # Check if state exists
    if [[ ! -f "${TF_DIR}/.terraform/terraform.tfstate" ]] && ! gsutil ls "gs://${GCS_BUCKET}/${STATE_PREFIX}/terraform.tfstate" &>/dev/null; then
        log_warning "No Terraform state found. Run 'apply' first."
        return 0
    fi

    if [[ "$JSON_OUTPUT" == "true" ]]; then
        terraform output -json
    else
        # Display key outputs in human-readable format
        echo
        log_info "GCP Project:"
        terraform output -raw project_id 2>/dev/null || log_warning "  (not available)"

        echo
        log_info "GKE Cluster:"
        terraform output -raw gke_cluster_name 2>/dev/null || log_warning "  (not available)"

        echo
        log_info "Cluster Endpoint (sensitive):"
        log_warning "  Use 'terraform output gke_cluster_endpoint' to view"

        echo
        log_info "Cluster Access Command:"
        terraform output -raw cluster_access_command 2>/dev/null || log_warning "  (not available)"

        echo
        log_info "Service Accounts:"
        log_warning "  Use 'terraform output security.service_accounts' to view"

        echo
        log_info "Load Balancer IPs:"
        log_warning "  Internal: $(terraform output -raw internal_lb_ip 2>/dev/null || echo '(pending)')"
        log_warning "  External: $(terraform output -raw external_lb_ip 2>/dev/null || echo '(pending)')"

        echo
        log_info "All outputs available via:"
        echo "  terraform output -json"
        echo "  terraform output <output_name>"
    fi
}

# =============================================================================
# Full workflow
# =============================================================================

run_full_workflow() {
    TOTAL_STEPS=6

    log_info "Starting full Terraform provisioning workflow"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Environment: $ENVIRONMENT"
    echo

    # Validate prerequisites
    if ! validate_requirements; then
        exit 3
    fi

    if ! validate_gcp_auth; then
        exit 7
    fi

    if ! validate_backend_config; then
        exit 3
    fi

    # Run workflow
    if ! terraform_init; then
        exit 3
    fi

    if ! terraform_validate; then
        exit 2
    fi

    if ! terraform_plan; then
        exit 4
    fi

    if ! terraform_apply; then
        exit 5
    fi

    # Show outputs
    show_progress "Displaying key outputs"
    if terraform_output; then
        log_success "Provisioning completed successfully!"
        echo
        log_info "Next steps:"
        echo "  1. Configure kubectl: $(terraform output -raw cluster_access_command 2>/dev/null || echo '[run terraform output cluster_access_command]')"
        echo "  2. Apply Kubernetes manifests: kubectl apply -f k8s/gcp/"
        echo "  3. Deploy CRE application"
    else
        exit 1
    fi
}

# =============================================================================
# Command dispatcher
# =============================================================================

run_command() {
    case "$COMMAND" in
        init)
            TOTAL_STEPS=2
            validate_requirements || exit 3
            validate_gcp_auth || exit 7
            validate_backend_config || exit 3
            terraform_init || exit 3
            log_success "Terraform initialized successfully"
            ;;

        plan)
            TOTAL_STEPS=4
            validate_requirements || exit 3
            validate_gcp_auth || exit 7
            terraform_validate || exit 2
            terraform_plan || exit 4
            log_success "Plan generated successfully"
            ;;

        apply)
            TOTAL_STEPS=3
            validate_requirements || exit 3
            terraform_apply || exit 5
            log_success "Apply completed successfully"
            ;;

        destroy)
            TOTAL_STEPS=2
            validate_requirements || exit 3
            terraform_destroy || exit 5
            log_success "Destroy completed"
            ;;

        full)
            run_full_workflow
            ;;

        output)
            terraform_output || exit 1
            ;;

        validate)
            TOTAL_STEPS=2
            validate_requirements || exit 3
            terraform_validate || exit 2
            log_success "Validation passed"
            ;;

        fmt-check)
            TOTAL_STEPS=1
            terraform_fmt_check || exit 2
            ;;

        state-backup)
            TOTAL_STEPS=1
            validate_gcp_auth || exit 7
            validate_backend_config || exit 3
            backup_state || exit 6
            log_success "Backup completed"
            ;;

        state-restore)
            list_state_backups
            if [[ $# -gt 0 ]]; then
                restore_state "$1" || exit 6
            else
                log_error "No backup file specified"
                log_error "Usage: $SCRIPT_NAME state-restore /path/to/backup.gz"
                exit 2
            fi
            ;;

        *)
            log_error "Unknown command: $COMMAND"
            print_usage
            exit 2
            ;;
    esac
}

# =============================================================================
# Main execution
# =============================================================================

main() {
    local start_time end_time duration
    start_time=$(date +%s)

    # Parse arguments
    parse_arguments "$@"

    # Fallback to GCP_PROJECT environment variable
    if [[ -z "$PROJECT_ID" ]] && [[ -n "${GCP_PROJECT:-}" ]]; then
        PROJECT_ID="$GCP_PROJECT"
        export TF_VAR_project_id="$PROJECT_ID"
    fi

    # Show configuration
    log_debug "Script configuration:"
    log_debug "  Command: $COMMAND"
    log_debug "  Project: ${PROJECT_ID:-not set}"
    log_debug "  Bucket: ${GCS_BUCKET:-not set}"
    log_debug "  State Prefix: $STATE_PREFIX"
    log_debug "  Region: $REGION"
    log_debug "  Environment: $ENVIRONMENT"
    log_debug "  Auto-approve: $AUTO_APPROVE"
    log_debug "  Backup: $BACKUP"
    log_debug "  Dry-run: $DRY_RUN"
    log_debug "  Verbose: $VERBOSE"

    # Run command
    run_command

    # Calculate duration
    end_time=$(date +%s)
    duration=$((end_time - start_time))

    echo
    log_success "Completed in ${duration}s"
}

# Trap errors
trap 'log_error "Script failed at line $LINENO"; exit 1' ERR

# Run main
main "$@"
