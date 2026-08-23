#!/bin/bash
# CRE GCP Backup Health Check Script
# Validates backup status and health for CRE on GCP
#
# Usage: ./scripts/runbooks/backup_health_check.sh [--namespace NAMESPACE] [--bucket BUCKET]

set -euo pipefail

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Defaults
NAMESPACE="${CRE_NAMESPACE:-cre}"
PROJECT_ID="${CRE_PROJECT_ID:-your-project-id}"
BACKUP_BUCKET="${CRE_BACKUP_BUCKET:-cre-backups}"
REGION="${CRE_REGION:-us-central1}"
MAX_AGE_HOURS="${MAX_AGE_HOURS:-48}"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --namespace|-n)
      NAMESPACE="$2"
      shift 2
      ;;
    --project|-p)
      PROJECT_ID="$2"
      shift 2
      ;;
    --bucket|-b)
      BACKUP_BUCKET="$2"
      shift 2
      ;;
    --region|-r)
      REGION="$2"
      shift 2
      ;;
    --max-age|-a)
      MAX_AGE_HOURS="$2"
      shift 2
      ;;
    --help|-h)
      echo "Usage: $0 [OPTIONS]"
      echo "Options:"
      echo "  --namespace, -n   Kubernetes namespace (default: cre)"
      echo "  --project, -p     GCP project ID"
      echo "  --bucket, -b      GCS backup bucket"
      echo "  --region, -r      GCP region"
      echo "  --max-age, -a     Maximum backup age in hours (default: 48)"
      echo "  --help, -h        Show this help"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Counters
PASS=0
WARN=0
FAIL=0

# Functions
log_info() {
  echo -e "${BLUE}[INFO]${NC} $*"
}

log_pass() {
  echo -e "${GREEN}[PASS]${NC} $*"
  ((PASS++))
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
  ((WARN++))
}

log_error() {
  echo -e "${RED}[FAIL]${NC} $*"
  ((FAIL++))
}

# Calculate timestamp for max age
MAX_AGE_TIMESTAMP=$(date -u -d "${MAX_AGE_HOURS} hours ago" +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || \
  date -u -v-${MAX_AGE_HOURS}H +%Y-%m-%dT%H:%M:%SZ)

# Header
echo "=================================="
echo "CRE Backup Health Check"
echo "=================================="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "Namespace: ${NAMESPACE}"
echo "Project: ${PROJECT_ID}"
echo "Backup Bucket: ${BACKUP_BUCKET}"
echo "Region: ${REGION}"
echo "Max Age: ${MAX_AGE_HOURS} hours"
echo "=================================="
echo

# 1. Check PVC Snapshots
echo "[1/8] Checking PVC snapshots..."
if command -v gcloud &> /dev/null; then
  RECENT_SNAPSHOTS=$(gcloud compute snapshots list \
    --project="${PROJECT_ID}" \
    --filter="creationTimestamp > '${MAX_AGE_TIMESTAMP}' AND labels:snapshot_group='cre-persistent-data'" \
    --format="value(name)" 2>/dev/null | wc -l || echo "0")

  if [[ "${RECENT_SNAPSHOTS}" -gt 0 ]]; then
    log_pass "Recent snapshots found: ${RECENT_SNAPSHOTS}"

    # List recent snapshots
    gcloud compute snapshots list \
      --project="${PROJECT_ID}" \
      --filter="creationTimestamp > '${MAX_AGE_TIMESTAMP}' AND labels:snapshot_group='cre-persistent-data'" \
      --format="table(name,creationTimestamp,status,diskSizeGb)" 2>/dev/null | head -10
  else
    log_error "No recent snapshots (within ${MAX_AGE_HOURS} hours)"
  fi
else
  log_warn "gcloud not found, skipping snapshot check"
fi
echo

# 2. Check GCS Backups
echo "[2/8] Checking GCS backups..."
if command -v gsutil &> /dev/null; then
  if gsutil ls "gs://${BACKUP_BUCKET}/" &>/dev/null; then
    RECENT_GCS=$(gsutil ls -lh "gs://${BACKUP_BUCKET}/**" 2>/dev/null | \
      grep -v "^TOTAL" | awk -v date="${MAX_AGE_TIMESTAMP}" '
      BEGIN {
        "date -u -d \"'"${MAX_AGE_HOURS}"' hours ago\" +%Y-%m-%dT%H:%M:%SZ" | getline max_age
        max_time = mktime(gensub(/[-T:]/, " ", "g", max_age))
      }
      {
        # Extract date from gsutil output (format varies)
        if (match($0, /[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}/)) {
          file_time = mktime(gensub(/[-T:]/, " ", "g", substr($0, RSTART, RLENGTH)))
          if (file_time >= max_time) count++
        }
      }
      END { print count+0 }')

    if [[ "${RECENT_GCS}" -gt 0 ]]; then
      log_pass "Recent GCS backups: ${RECENT_GCS}"

      # Show latest backups
      log_info "Latest backups:"
      gsutil ls -lh "gs://${BACKUP_BUCKET}/**" 2>/dev/null | grep -v "^TOTAL" | tail -5 | \
        awk '{print "  " $NF " (" $2 ")"}'
    else
      log_error "No recent GCS backups (within ${MAX_AGE_HOURS} hours)"
    fi

    # Check bucket size
    BUCKET_SIZE=$(gsutil du -s "gs://${BACKUP_BUCKET}" 2>/dev/null | awk '{print $1}')
    BUCKET_SIZE_MB=$((BUCKET_SIZE / 1024 / 1024))

    if [[ "${BUCKET_SIZE_MB}" -gt 10 ]]; then
      log_pass "Backup bucket size: ${BUCKET_SIZE_MB} MB"
    else
      log_warn "Backup bucket size seems small: ${BUCKET_SIZE_MB} MB"
    fi
  else
    log_error "Cannot access backup bucket: gs://${BACKUP_BUCKET}/"
  fi
else
  log_warn "gsutil not found, skipping GCS check"
fi
echo

# 3. Check Kubernetes VolumeSnapshots
echo "[3/8] Checking Kubernetes VolumeSnapshots..."
if kubectl get volumesnapshot -n "${NAMESPACE}" &>/dev/null; then
  SNAPSHOT_COUNT=$(kubectl get volumesnapshot -n "${NAMESPACE}" -o json 2>/dev/null | \
    jq '[.items[] | select(.status.creationTime >= "'"${MAX_AGE_TIMESTAMP}"'")] | length' 2>/dev/null || echo "0")

  if [[ "${SNAPSHOT_COUNT}" -gt 0 ]]; then
    log_pass "Recent VolumeSnapshots: ${SNAPSHOT_COUNT}"

    kubectl get volumesnapshot -n "${NAMESPACE}" -o custom-columns=NAME:.metadata.name,READY:.status.readyToUse,AGE:.metadata.creationTimestamp | tail -5
  else
    log_warn "No recent VolumeSnapshots found"
  fi
else
  log_warn "VolumeSnapshot CRD not found"
fi
echo

# 4. Check Mnesia Backup Status
echo "[4/8] Checking Mnesia backup status..."
CRE_POD=$(kubectl get pod -n "${NAMESPACE}" -l app=cre -o jsonpath='{.items[0].metadata.name}' 2>/dev/null || echo "")

if [[ -n "${CRE_POD}" ]]; then
  # Check backup directory
  BACKUP_FILES=$(kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    find /opt/cre/backup -name "backup_*" -type f -mtime -"$(echo "$MAX_AGE_HOURS / 24" | bc)" 2>/dev/null | wc -l || echo "0")

  if [[ "${BACKUP_FILES}" -gt 0 ]]; then
    log_pass "Recent Mnesia backups: ${BACKUP_FILES}"

    kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
      ls -lah /opt/cre/backup/ 2>/dev/null | tail -5 || true
  else
    log_warn "No recent Mnesia backups found in /opt/cre/backup/"
  fi

  # Test Mnesia checkpoint
  MNESIA_TEST=$(kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "mnesia:checkpoint_backup([list_to_atom(\"test_checkpoints\")])." 2>/dev/null || echo "error")

  if [[ "${MNESIA_TEST}" == *"ok"* ]]; then
    log_pass "Mnesia checkpoint test successful"
  else
    log_warn "Mnesia checkpoint test failed"
  fi
else
  log_warn "No CRE pods found for Mnesia backup check"
fi
echo

# 5. Check Backup Schedule
echo "[5/8] Checking backup schedules..."

# Check Velero schedules (if using Velero)
if kubectl get schedules -n velero &>/dev/null 2>&1; then
  SCHEDULE_COUNT=$(kubectl get schedules -n velero -o json 2>/dev/null | \
    jq '.items | length' 2>/dev/null || echo "0")

  if [[ "${SCHEDULE_COUNT}" -gt 0 ]]; then
    log_pass "Velero schedules configured: ${SCHEDULE_COUNT}"
    kubectl get schedules -n velero
  else
    log_warn "No Velero schedules found"
  fi
fi

# Check CronJobs for backup
CRONJOBS=$(kubectl get cronjob -n "${NAMESPACE}" -o json 2>/dev/null | \
  jq '[.items[] | select(.metadata.name | contains("backup"))] | length' 2>/dev/null || echo "0")

if [[ "${CRONJOBS}" -gt 0 ]]; then
  log_pass "Backup CronJobs configured: ${CRONJOBS}"
  kubectl get cronjob -n "${NAMESPACE}" | grep backup || true
else
  log_info "No backup CronJobs found (may be using external scheduler)"
fi
echo

# 6. Verify Backup Accessibility
echo "[6/8] Verifying backup accessibility..."

# Test restoring from backup
if command -v gsutil &> /dev/null && [[ -n "${CRE_POD}" ]]; then
  LATEST_BACKUP=$(gsutil ls "gs://${BACKUP_BUCKET}/" 2>/dev/null | grep -i backup | sort -r | head -1 || echo "")

  if [[ -n "${LATEST_BACKUP}" ]]; then
    log_info "Latest backup: ${LATEST_BACKUP}"

    # Test download accessibility
    if gsutil stat "${LATEST_BACKUP}" &>/dev/null; then
      log_pass "Backup is accessible"

      # Check backup file size
      BACKUP_SIZE=$(gsutil stat "${LATEST_BACKUP}" 2>/dev/null | grep "Content-Length" | awk '{print $2}')
      BACKUP_SIZE_MB=$((BACKUP_SIZE / 1024 / 1024))

      if [[ "${BACKUP_SIZE_MB}" -gt 1 ]]; then
        log_pass "Backup size: ${BACKUP_SIZE_MB} MB"
      else
        log_warn "Backup size seems small: ${BACKUP_SIZE_MB} MB"
      fi
    else
      log_error "Cannot access backup: ${LATEST_BACKUP}"
    fi
  fi
fi
echo

# 7. Check Replication Status
echo "[7/8] Checking data replication..."

if [[ -n "${CRE_POD}" ]]; then
  # Check Mnesia replication
  RUNNING_NODES=$(kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "length(mnesia:system_info(running_db_nodes))." 2>/dev/null || echo "0")

  if [[ "${RUNNING_NODES}" -gt 1 ]]; then
    log_pass "Mnesia replicas: ${RUNNING_NODES} nodes"
  else
    log_warn "Mnesia running on single node (no replication)"
  fi

  # Check table storage types
  DISC_COPIES=$(kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "length([T || T <- mnesia:system_info(tables), mnesia:table_info(T, storage_type) == disc_copies])." 2>/dev/null || echo "0")

  log_info "Tables with disc_copies: ${DISC_COPIES}"
else
  log_warn "Cannot check replication status (no CRE pod)"
fi
echo

# 8. Check Backup Retention
echo "[8/8] Checking backup retention..."

if command -v gsutil &> /dev/null; then
  OLD_BACKUPS=$(gsutil ls "gs://${BACKUP_BUCKET}/" 2>/dev/null | \
    while read -r backup; do
      if [[ -n "${backup}" ]]; then
        # Get backup age
        BACKUP_DATE=$(basename "${backup}" | grep -oP '\d{8}' || echo "")
        if [[ -n "${BACKUP_DATE}" ]]; then
          BACKUP_TIME=$(date -u -d "${BACKUP_DATE}" +%s 2>/dev/null || echo "0")
          CURRENT_TIME=$(date +%s)
          AGE_DAYS=$(((CURRENT_TIME - BACKUP_TIME) / 86400))

          if [[ "${AGE_DAYS}" -gt 30 ]]; then
            echo "${backup}"
          fi
        fi
      fi
    done | wc -l || echo "0")

  if [[ "${OLD_BACKUPS}" -eq 0 ]]; then
    log_pass "No backups exceeding 30-day retention"
  else
    log_info "Backups older than 30 days: ${OLD_BACKUPS}"
  fi
fi
echo

# Summary
echo "=================================="
echo "Backup Health Check Summary"
echo "=================================="
echo -e "${GREEN}Passed:${NC} ${PASS}"
echo -e "${YELLOW}Warnings:${NC} ${WARN}"
echo -e "${RED}Failed:${NC} ${FAIL}"
echo

# Recommendations
if [[ "${FAIL}" -gt 0 ]]; then
  echo "=================================="
  echo "Recommendations"
  echo "=================================="
  echo "1. Verify backup schedules are configured correctly"
  echo "2. Check service account permissions for GCS access"
  echo "3. Review backup job logs for errors"
  echo "4. Verify sufficient storage quota for backups"
  echo
fi

# Exit code
if [[ "${FAIL}" -gt 0 ]]; then
  exit 1
elif [[ "${WARN}" -gt 0 ]]; then
  exit 2
else
  exit 0
fi
