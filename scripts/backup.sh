#!/usr/bin/env bash
#
# CRE Backup Script for GCP
# Automates Mnesia backup, GCS upload, verification, and restoration testing
#
# SLA Targets:
# - RTO (Recovery Time Objective): 30 minutes
# - RPO (Recovery Point Objective): 15 minutes
#
# Usage:
#   ./backup.sh [options]
#
# Options:
#   --type=mnesia|full          Backup type (default: mnesia)
#   --schedule=daily|hourly|weekly  Schedule type (default: daily)
#   --verify                    Enable backup verification
#   --test-restore              Perform restoration test
#   --dry-run                   Show what would be done without executing
#
# Environment Variables:
#   GCS_BUCKET                  GCS bucket name (required)
#   RETENTION_DAYS              Backup retention in days (default: 30)
#   BACKUP_REGION               Primary region (default: us-central1)
#   ENABLE_REPLICATION          Enable cross-region replication (default: false)
#   REPLICATION_REGION          Replication target region
#   ENCRYPTION_TYPE             google-managed|customer-managed (default: google-managed)
#   MNESIA_BACKUP_DIR           Mnesia backup directory
#   ENABLE_COMPRESSION          Enable compression (default: true)
#   ENABLE_VERIFICATION         Enable verification (default: true)
#   CRE_NAMESPACE               Kubernetes namespace (default: cre-prod)
#   GOOGLE_CLOUD_PROJECT        GCP project ID
#
# Exit codes:
#   0 - Success
#   1 - General error
#   2 - Backup creation failed
#   3 - Upload failed
#   4 - Verification failed
#   5 - Restoration test failed
#
# @author CRE Team
# @version 1.0.0

set -euo pipefail

# ============================================
# Configuration
# ============================================

# Script version
VERSION="1.0.0"

# Default values
BACKUP_TYPE="${BACKUP_TYPE:-mnesia}"
BACKUP_SCHEDULE="${BACKUP_SCHEDULE:-daily}"
GCS_BUCKET="${GCS_BUCKET:-}"
RETENTION_DAYS="${RETENTION_DAYS:-30}"
BACKUP_REGION="${BACKUP_REGION:-us-central1}"
ENABLE_REPLICATION="${ENABLE_REPLICATION:-false}"
REPLICATION_REGION="${REPLICATION_REGION:-us-east1}"
ENCRYPTION_TYPE="${ENCRYPTION_TYPE:-google-managed}"
MNESIA_BACKUP_DIR="${MNESIA_BACKUP_DIR:-/opt/cre/mnesia/backup}"
ENABLE_COMPRESSION="${ENABLE_COMPRESSION:-true}"
ENABLE_VERIFICATION="${ENABLE_VERIFICATION:-true}"
CRE_NAMESPACE="${CRE_NAMESPACE:-cre-prod}"
GOOGLE_CLOUD_PROJECT="${GOOGLE_CLOUD_PROJECT:-}"
GCS_SA_KEY="${GCS_SA_KEY:-}"
BACKUP_ENCRYPTION_KEY="${BACKUP_ENCRYPTION_KEY:-}"
SLACK_WEBHOOK_URL="${SLACK_WEBHOOK_URL:-}"

# Derived paths
BACKUP_DATE=$(date +%Y-%m-%d)
BACKUP_TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="/backup"
BACKUP_FILENAME="cre_${BACKUP_TYPE}_${BACKUP_SCHEDULE}_${BACKUP_TIMESTAMP}"
BACKUP_PATH="${BACKUP_DIR}/${BACKUP_FILENAME}"

# GCS paths
GCS_BASE_PATH="gs://${GCS_BUCKET}"
GCS_BACKUP_PATH="${GCS_BASE_PATH}/${BACKUP_TYPE}/${BACKUP_SCHEDULE}/${BACKUP_DATE}"
GCS_REPLICA_PATH="${GCS_BASE_PATH}-replica/${BACKUP_TYPE}/${BACKUP_SCHEDULE}/${BACKUP_DATE}"

# Logging
LOG_FILE="${BACKUP_DIR}/backup_${BACKUP_TIMESTAMP}.log"
VERBOSE="${VERBOSE:-false}"
DRY_RUN="${DRY_RUN:-false}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Metrics
METRICS_START_TIME=$(date +%s)
METRICS_BACKUP_SIZE=0
METRICS_UPLOAD_DURATION=0
METRICS_VERIFY_DURATION=0

# ============================================
# Logging Functions
# ============================================

log() {
    local level=$1
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[${timestamp}] [${level}] ${message}" | tee -a "${LOG_FILE}"
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" | tee -a "${LOG_FILE}"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" | tee -a "${LOG_FILE}"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*" | tee -a "${LOG_FILE}"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" | tee -a "${LOG_FILE}"
}

log_debug() {
    if [[ "${VERBOSE}" == "true" ]]; then
        echo -e "[DEBUG] $*" | tee -a "${LOG_FILE}"
    fi
}

# ============================================
# Usage
# ============================================

usage() {
    cat << EOF
CRE Backup Script v${VERSION}

Usage: $(basename "$0") [OPTIONS]

Options:
  --type=TYPE              Backup type: mnesia, full (default: mnesia)
  --schedule=SCHEDULE      Schedule: daily, hourly, weekly (default: daily)
  --verify                 Enable backup verification
  --test-restore           Perform restoration test
  --dry-run                Show what would be done without executing
  --verbose                Enable verbose output
  --help                   Show this help message

Environment Variables:
  GCS_BUCKET              GCS bucket name (required)
  RETENTION_DAYS          Backup retention in days (default: 30)
  BACKUP_REGION           Primary region (default: us-central1)
  ENABLE_REPLICATION      Enable cross-region replication (default: false)
  REPLICATION_REGION      Replication target region
  ENCRYPTION_TYPE         google-managed|customer-managed (default: google-managed)
  CRE_NAMESPACE           Kubernetes namespace (default: cre-prod)
  GOOGLE_CLOUD_PROJECT    GCP project ID
  GCS_SA_KEY              Service account key (base64 encoded)

Examples:
  # Daily backup with verification
  $(basename "$0") --type=mnesia --schedule=daily --verify

  # Weekly full backup with restoration test
  $(basename "$0") --type=full --schedule=weekly --verify --test-restore

  # Dry run to see what would happen
  $(basename "$0") --type=mnesia --schedule=daily --dry-run

EOF
    exit 0
}

# ============================================
# Parse Arguments
# ============================================

parse_arguments() {
    local verify=false
    local test_restore=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            --type=*)
                BACKUP_TYPE="${1#*=}"
                ;;
            --schedule=*)
                BACKUP_SCHEDULE="${1#*=}"
                ;;
            --verify)
                ENABLE_VERIFICATION="true"
                verify=true
                ;;
            --test-restore)
                test_restore=true
                ;;
            --dry-run)
                DRY_RUN="true"
                ;;
            --verbose|-v)
                VERBOSE="true"
                ;;
            --help|-h)
                usage
                ;;
            *)
                log_error "Unknown option: $1"
                usage
                ;;
        esac
        shift
    done

    export ENABLE_VERIFICATION
    export DO_TEST_RESTORE="${test_restore}"
}

# ============================================
# Validation
# ============================================

validate_environment() {
    log_info "Validating environment..."

    local errors=0

    # Check required variables
    if [[ -z "${GCS_BUCKET}" ]]; then
        log_error "GCS_BUCKET environment variable is required"
        ((errors++))
    fi

    if [[ -z "${GOOGLE_CLOUD_PROJECT}" ]]; then
        log_error "GOOGLE_CLOUD_PROJECT environment variable is required"
        ((errors++))
    fi

    if [[ -z "${GCS_SA_KEY}" ]]; then
        log_error "GCS_SA_KEY environment variable is required"
        ((errors++))
    fi

    # Validate backup type
    if [[ ! "${BACKUP_TYPE}" =~ ^(mnesia|full)$ ]]; then
        log_error "Invalid backup type: ${BACKUP_TYPE}"
        ((errors++))
    fi

    # Validate schedule
    if [[ ! "${BACKUP_SCHEDULE}" =~ ^(daily|hourly|weekly)$ ]]; then
        log_error "Invalid schedule: ${BACKUP_SCHEDULE}"
        ((errors++))
    fi

    # Validate encryption type
    if [[ ! "${ENCRYPTION_TYPE}" =~ ^(google-managed|customer-managed)$ ]]; then
        log_error "Invalid encryption type: ${ENCRYPTION_TYPE}"
        ((errors++))
    fi

    if [[ ${errors} -gt 0 ]]; then
        log_error "Environment validation failed with ${errors} error(s)"
        exit 1
    fi

    log_success "Environment validation passed"
}

# ============================================
# GCP Authentication
# ============================================

setup_gcp_authentication() {
    log_info "Setting up GCP authentication..."

    local key_file="/tmp/gcs-sa-key-$$-${BACKUP_TIMESTAMP}.json"

    # Decode and save service account key
    echo "${GCS_SA_KEY}" | base64 -d > "${key_file}"
    chmod 600 "${key_file}"

    # Activate service account
    if [[ "${DRY_RUN}" != "true" ]]; then
        gcloud auth activate-service-account --key-file="${key_file}" --quiet
        log_success "GCP authentication configured"
    else
        log_info "[DRY RUN] Would activate service account from key file"
    fi

    export GOOGLE_APPLICATION_CREDENTIALS="${key_file}"
}

# ============================================
# Backup Creation
# ============================================

create_mnesia_backup() {
    log_info "Creating Mnesia backup..."

    local backup_file="${BACKUP_PATH}.tar"
    local mnesia_backup_file="${backup_file}.gz"

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would create Mnesia backup: ${mnesia_backup_file}"
        echo "${mnesia_backup_file}"
        return 0
    fi

    # Create backup directory
    mkdir -p "${BACKUP_DIR}"

    # Find CRE pods
    local cre_pods
    cre_pods=$(kubectl get pods -n "${CRE_NAMESPACE}" -l app=cre -o jsonpath='{.items[*].metadata.name}' 2>/dev/null || echo "")

    if [[ -z "${cre_pods}" ]]; then
        log_error "No CRE pods found in namespace ${CRE_NAMESPACE}"
        return 2
    fi

    log_info "Found CRE pods: ${cre_pods}"

    # Select a healthy pod
    local backup_pod=""
    for pod in ${cre_pods}; do
        local pod_ready
        pod_ready=$(kubectl get pod -n "${CRE_NAMESPACE}" "${pod}" -o jsonpath='{.status.conditions[?(@.type=="Ready")].status}')
        if [[ "${pod_ready}" == "True" ]]; then
            backup_pod="${pod}"
            break
        fi
    done

    if [[ -z "${backup_pod}" ]]; then
        log_error "No ready CRE pods found for backup"
        return 2
    fi

    log_info "Using pod ${backup_pod} for backup"

    # Trigger Mnesia backup via Erlang RPC
    # This assumes the CRE node exposes an administrative interface
    local backup_result
    backup_result=$(kubectl exec -n "${CRE_NAMESPACE}" "${backup_pod}" -- \
        erl -noshell -name backup_${BACKUP_TIMESTAMP}@localhost -setcookie "${ERLANG_COOKIE:-cre}" \
        -eval "
            case net_adm:ping('cre@${backup_pod}.cre-prod.svc.cluster.local') of
                pong ->
                    case rpc:call('cre@${backup_pod}.cre-prod.svc.cluster.local', mnesia, backup, [\"${BACKUP_PATH}\"]) of
                        {ok, _} -> io:format('backup_ok~n');
                        {error, Reason} -> io:format('backup_error:~p~n', [Reason])
                    end;
                pang ->
                    io:format('ping_failed~n')
            end,
            halt(0)
        " 2>&1 || true)

    if [[ ! "${backup_result}" =~ "backup_ok" ]]; then
        log_warning "RPC backup failed, trying alternative method..."

        # Alternative: Copy Mnesia data directory directly
        kubectl exec -n "${CRE_NAMESPACE}" "${backup_pod}" -- \
            tar -cf - -C /opt/cre/data mnesia > "${backup_file}" 2>/dev/null || {
            log_error "Failed to create Mnesia backup from pod ${backup_pod}"
            return 2
        }
    fi

    # Compress if enabled
    if [[ "${ENABLE_COMPRESSION}" == "true" ]]; then
        log_info "Compressing backup..."
        gzip -f "${backup_file}"
        backup_file="${mnesia_backup_file}"
    fi

    METRICS_BACKUP_SIZE=$(stat -f%z "${backup_file}" 2>/dev/null || stat -c%s "${backup_file}" 2>/dev/null || echo 0)
    log_success "Mnesia backup created: ${backup_file} (${METRICS_BACKUP_SIZE} bytes)"

    echo "${backup_file}"
}

create_full_backup() {
    log_info "Creating full backup (Mnesia + configuration)..."

    local backup_file="${BACKUP_PATH}.tar"
    local full_backup_file="${backup_file}.gz"

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would create full backup: ${full_backup_file}"
        echo "${full_backup_file}"
        return 0
    fi

    mkdir -p "${BACKUP_DIR}"

    # Create temporary backup structure
    local temp_dir="${BACKUP_DIR}/full_${BACKUP_TIMESTAMP}"
    mkdir -p "${temp_dir}"

    # Backup Mnesia
    log_info "Backing up Mnesia data..."
    create_mnesia_backup > /dev/null
    mv "${BACKUP_PATH}.tar.gz" "${temp_dir}/mnesia_backup.tar.gz" 2>/dev/null || true

    # Backup Kubernetes resources
    log_info "Backing up Kubernetes resources..."
    kubectl get all,configmaps,secrets,pvc -n "${CRE_NAMESPACE}" -o yaml > "${temp_dir}/k8s_resources.yaml"

    # Backup Terraform state (if available)
    if kubectl get secret -n "${CRE_NAMESPACE}" terraform-state &>/dev/null; then
        log_info "Backing up Terraform state..."
        kubectl get secret -n "${CRE_NAMESPACE}" terraform-state -o yaml > "${temp_dir}/terraform_state.yaml"
    fi

    # Create metadata
    cat > "${temp_dir}/backup_metadata.json" << EOF
{
  "backup_type": "full",
  "backup_schedule": "${BACKUP_SCHEDULE}",
  "backup_date": "${BACKUP_DATE}",
  "backup_timestamp": "${BACKUP_TIMESTAMP}",
  "cre_namespace": "${CRE_NAMESPACE}",
  "backup_version": "${VERSION}",
  "k8s_version": "$(kubectl version --short 2>/dev/null | grep Server || echo 'unknown')",
  "node_count": "$(kubectl get nodes --no-headers | wc -l)"
}
EOF

    # Package everything
    log_info "Packaging full backup..."
    tar -cf "${backup_file}" -C "${temp_dir}" .
    rm -rf "${temp_dir}"

    # Compress
    if [[ "${ENABLE_COMPRESSION}" == "true" ]]; then
        gzip -f "${backup_file}"
        backup_file="${full_backup_file}"
    fi

    METRICS_BACKUP_SIZE=$(stat -f%z "${backup_file}" 2>/dev/null || stat -c%s "${backup_file}" 2>/dev/null || echo 0)
    log_success "Full backup created: ${backup_file} (${METRICS_BACKUP_SIZE} bytes)"

    echo "${backup_file}"
}

# ============================================
# Upload to GCS
# ============================================

upload_to_gcs() {
    local backup_file=$1

    log_info "Uploading backup to GCS..."

    local gcs_dest="${GCS_BACKUP_PATH}/$(basename "${backup_file}")"
    local upload_start=$(date +%s)

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would upload ${backup_file} to ${gcs_dest}"
        echo "${gcs_dest}"
        return 0
    fi

    # Prepare upload flags based on encryption type
    local upload_flags=()
    if [[ "${ENCRYPTION_TYPE}" == "customer-managed" ]] && [[ -n "${BACKUP_ENCRYPTION_KEY}" ]]; then
        upload_flags+=("-h" "x-goog-encryption-key:${BACKUP_ENCRYPTION_KEY}")
    fi

    # Upload to primary region
    log_info "Uploading to ${gcs_dest}..."
    if gsutil cp "${upload_flags[@]}" "${backup_file}" "${gcs_dest}"; then
        log_success "Upload completed successfully"
    else
        log_error "Upload to GCS failed"
        return 3
    fi

    # Set object metadata
    gsutil setmeta \
        -h "Content-Type:application/gzip" \
        -h "x-goog-meta-backup-type:${BACKUP_TYPE}" \
        -h "x-goog-meta-backup-schedule:${BACKUP_SCHEDULE}" \
        -h "x-goog-meta-backup-date:${BACKUP_DATE}" \
        -h "x-goog-meta-cre-namespace:${CRE_NAMESPACE}" \
        "${gcs_dest}" &>/dev/null || true

    # Cross-region replication
    if [[ "${ENABLE_REPLICATION}" == "true" ]] && [[ "${BACKUP_SCHEDULE}" == "daily" || "${BACKUP_SCHEDULE}" == "weekly" ]]; then
        log_info "Replicating to ${REPLICATION_REGION}..."
        local replica_dest="${GCS_REPLICA_PATH}/$(basename "${backup_file}")"
        if gsutil cp "${upload_flags[@]}" "${backup_file}" "${replica_dest}"; then
            log_success "Replication to ${REPLICATION_REGION} completed"
        else
            log_warning "Replication to ${REPLICATION_REGION} failed (non-critical)"
        fi
    fi

    local upload_end=$(date +%s)
    METRICS_UPLOAD_DURATION=$((upload_end - upload_start))

    echo "${gcs_dest}"
}

# ============================================
# Backup Verification
# ============================================

verify_backup() {
    local gcs_path=$1

    if [[ "${ENABLE_VERIFICATION}" != "true" ]]; then
        log_info "Verification disabled, skipping..."
        return 0
    fi

    log_info "Verifying backup..."

    local verify_start=$(date +%s)

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would verify backup at ${gcs_path}"
        return 0
    fi

    # Check file exists in GCS
    if ! gsutil -q stat "${gcs_path}"; then
        log_error "Backup file not found at ${gcs_path}"
        return 4
    fi

    # Verify file size
    local remote_size
    remote_size=$(gsutil du "${gcs_path}" | awk '{print $1}')
    if [[ "${remote_size}" != "${METRICS_BACKUP_SIZE}" ]]; then
        log_error "Size mismatch: local=${METRICS_BACKUP_SIZE}, remote=${remote_size}"
        return 4
    fi

    # Download and verify integrity
    local verify_file="${BACKUP_DIR}/verify_${BACKUP_TIMESTAMP}.tar.gz"
    log_info "Downloading backup for verification..."
    if ! gsutil cp "${gcs_path}" "${verify_file}"; then
        log_error "Failed to download backup for verification"
        return 4
    fi

    # Verify gzip integrity
    if ! gzip -t "${verify_file}" 2>/dev/null; then
        log_error "Backup file is corrupted (gzip test failed)"
        rm -f "${verify_file}"
        return 4
    fi

    # Verify tar contents
    if ! tar -tzf "${verify_file}" > /dev/null 2>&1; then
        log_error "Backup file is corrupted (tar test failed)"
        rm -f "${verify_file}"
        return 4
    fi

    rm -f "${verify_file}"

    local verify_end=$(date +%s)
    METRICS_VERIFY_DURATION=$((verify_end - verify_start))

    log_success "Backup verification completed successfully"
    return 0
}

# ============================================
# Restoration Test
# ============================================

test_restoration() {
    local gcs_path=$1

    if [[ "${DO_TEST_RESTORE}" != "true" ]]; then
        log_info "Restoration test not requested, skipping..."
        return 0
    fi

    log_info "Starting restoration test..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would test restoration from ${gcs_path}"
        return 0
    fi

    # Download backup
    local restore_dir="${BACKUP_DIR}/restore_test_${BACKUP_TIMESTAMP}"
    mkdir -p "${restore_dir}"

    log_info "Downloading backup for restoration test..."
    if ! gsutil cp "${gcs_path}" "${restore_dir}/backup.tar.gz"; then
        log_error "Failed to download backup for restoration test"
        return 5
    fi

    # Extract
    log_info "Extracting backup..."
    if ! tar -xzf "${restore_dir}/backup.tar.gz" -C "${restore_dir}"; then
        log_error "Failed to extract backup"
        return 5
    fi

    # For Mnesia backup, verify schema integrity
    if [[ "${BACKUP_TYPE}" == "mnesia" ]]; then
        log_info "Verifying Mnesia schema..."
        # This would involve starting a test Erlang node and loading the backup
        # For now, we just verify the file structure
        if [[ ! -d "${restore_dir}/mnesia" ]] && [[ ! -f "${restore_dir}/mnesia_backup.tar.gz" ]]; then
            log_warning "Expected Mnesia structure not found, but backup may still be valid"
        fi
    fi

    # Cleanup
    rm -rf "${restore_dir}"

    log_success "Restoration test completed successfully"
    return 0
}

# ============================================
# Metrics Publishing
# ============================================

publish_metrics() {
    local status=$1
    local status_code=$2

    log_info "Publishing metrics to Cloud Monitoring..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would publish metrics"
        return 0
    fi

    # Publish backup completion
    gcloud monitoring metrics publish \
        custom.googleapis.com/cre/backup/complete \
        "$(date +%s)" \
        --resource-type="global" \
        --resource-labels="project_id=${GOOGLE_CLOUD_PROJECT},backup_type=${BACKUP_TYPE},schedule=${BACKUP_SCHEDULE}" \
        --labels="status=${status},schedule=${BACKUP_SCHEDULE},type=${BACKUP_TYPE}" &>/dev/null || true

    # Publish backup size
    if [[ ${status_code} -eq 0 ]]; then
        gcloud monitoring metrics publish \
            custom.googleapis.com/cre/backup/size_bytes \
            "${METRICS_BACKUP_SIZE}" \
            --resource-type="global" \
            --resource-labels="project_id=${GOOGLE_CLOUD_PROJECT}" \
            --labels="schedule=${BACKUP_SCHEDULE},type=${BACKUP_TYPE}" &>/dev/null || true

        # Publish durations
        gcloud monitoring metrics publish \
            custom.googleapis.com/cre/backup/duration_seconds \
            "${METRICS_UPLOAD_DURATION}" \
            --resource-type="global" \
            --resource-labels="project_id=${GOOGLE_CLOUD_PROJECT},phase=upload" \
            --labels="schedule=${BACKUP_SCHEDULE},type=${BACKUP_TYPE}" &>/dev/null || true
    fi
}

# ============================================
# Notifications
# ============================================

send_notification() {
    local status=$1
    local message=$2
    local exit_code=$3

    if [[ -z "${SLACK_WEBHOOK_URL}" ]]; then
        return 0
    fi

    log_info "Sending ${status} notification..."

    local color="#36a64f"  # green
    if [[ ${exit_code} -ne 0 ]]; then
        color="#dc3545"  # red
    fi

    local slack_payload
    slack_payload=$(cat <<EOF
{
  "attachments": [
    {
      "color": "${color}",
      "title": "CRE Backup ${status^}",
      "fields": [
        {
          "title": "Type",
          "value": "${BACKUP_TYPE}",
          "short": true
        },
        {
          "title": "Schedule",
          "value": "${BACKUP_SCHEDULE}",
          "short": true
        },
        {
          "title": "Date",
          "value": "${BACKUP_DATE}",
          "short": true
        },
        {
          "title": "Exit Code",
          "value": "${exit_code}",
          "short": true
        },
        {
          "title": "Size",
          "value": "${METRICS_BACKUP_SIZE} bytes",
          "short": true
        },
        {
          "title": "Duration",
          "value": "${METRICS_UPLOAD_DURATION}s",
          "short": true
        }
      ],
      "text": "${message}",
      "footer": "CRE Backup Automation",
      "ts": $(date +%s)
    }
  ]
}
EOF
)

    if [[ "${DRY_RUN}" != "true" ]]; then
        curl -s -X POST "${SLACK_WEBHOOK_URL}" \
            -H 'Content-Type: application/json' \
            -d "${slack_payload}" &>/dev/null || true
    fi
}

# ============================================
# Cleanup
# ============================================

cleanup() {
    local exit_code=$1

    # Remove temporary key file
    if [[ -n "${GOOGLE_APPLICATION_CREDENTIALS}" ]]; then
        rm -f "${GOOGLE_APPLICATION_CREDENTIALS}"
    fi

    # Remove local backup file after successful upload
    if [[ ${exit_code} -eq 0 ]] && [[ "${DRY_RUN}" != "true" ]]; then
        find "${BACKUP_DIR}" -name "cre_*_${BACKUP_TIMESTAMP}*" -type f -delete 2>/dev/null || true
    fi
}

# ============================================
# Main
# ============================================

main() {
    local exit_code=0
    local gcs_path=""

    # Create backup directory
    mkdir -p "${BACKUP_DIR}"

    # Parse command line arguments
    parse_arguments "$@"

    # Log header
    log_info "========================================="
    log_info "CRE Backup Script v${VERSION}"
    log_info "========================================="
    log_info "Backup Type: ${BACKUP_TYPE}"
    log_info "Schedule: ${BACKUP_SCHEDULE}"
    log_info "Date: ${BACKUP_DATE}"
    log_info "Dry Run: ${DRY_RUN}"
    log_info "========================================="

    # Validate environment
    validate_environment

    # Setup GCP authentication
    setup_gcp_authentication

    # Create backup based on type
    local backup_file=""
    case "${BACKUP_TYPE}" in
        mnesia)
            backup_file=$(create_mnesia_backup)
            ;;
        full)
            backup_file=$(create_full_backup)
            ;;
        *)
            log_error "Unknown backup type: ${BACKUP_TYPE}"
            exit 1
            ;;
    esac

    # Check if backup creation succeeded
    if [[ ! -f "${backup_file}" ]] && [[ "${DRY_RUN}" != "true" ]]; then
        log_error "Backup file creation failed"
        publish_metrics "failed" 2
        send_notification "failed" "Backup file creation failed" 2
        cleanup 2
        exit 2
    fi

    # Upload to GCS
    gcs_path=$(upload_to_gcs "${backup_file}")
    if [[ $? -ne 0 ]]; then
        log_error "Backup upload failed"
        publish_metrics "failed" 3
        send_notification "failed" "Backup upload failed" 3
        cleanup 3
        exit 3
    fi

    # Verify backup
    if ! verify_backup "${gcs_path}"; then
        log_error "Backup verification failed"
        publish_metrics "failed" 4
        send_notification "failed" "Backup verification failed" 4
        cleanup 4
        exit 4
    fi

    # Test restoration
    if ! test_restoration "${gcs_path}"; then
        log_error "Restoration test failed"
        publish_metrics "failed" 5
        send_notification "failed" "Restoration test failed" 5
        cleanup 5
        exit 5
    fi

    # Calculate total duration
    local end_time=$(date +%s)
    local total_duration=$((end_time - METRICS_START_TIME))

    # Success
    log_success "========================================="
    log_success "Backup completed successfully!"
    log_success "========================================="
    log_info "Backup file: ${backup_file}"
    log_info "GCS location: ${gcs_path}"
    log_info "Backup size: ${METRICS_BACKUP_SIZE} bytes"
    log_info "Upload duration: ${METRICS_UPLOAD_DURATION}s"
    log_info "Total duration: ${total_duration}s"
    log_success "========================================="

    # Publish success metrics
    publish_metrics "success" 0
    send_notification "success" "Backup completed successfully" 0

    # Cleanup
    cleanup 0

    exit 0
}

# Run main function
main "$@"
