#!/bin/bash
# CRE GCP Health Check Script
# Validates CRE deployment health on GCP/GKE
#
# Usage: ./scripts/runbooks/health_check.sh [--namespace NAMESPACE] [--project PROJECT]

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

# Defaults
NAMESPACE="${CRE_NAMESPACE:-cre}"
PROJECT_ID="${CRE_PROJECT_ID:-your-project-id}"
REGION="${CRE_REGION:-us-central1}"
VERBOSE="${VERBOSE:-false}"

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
    --region|-r)
      REGION="$2"
      shift 2
      ;;
    --verbose|-v)
      VERBOSE=true
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [OPTIONS]"
      echo "Options:"
      echo "  --namespace, -n   Kubernetes namespace (default: cre)"
      echo "  --project, -p     GCP project ID"
      echo "  --region, -r      GCP region (default: us-central1)"
      echo "  --verbose, -v     Enable verbose output"
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
FAIL=0
WARN=0

# Functions
log_info() {
  echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*" >&2
  ((WARN++))
}

log_error() {
  echo -e "${RED}[FAIL]${NC} $*" >&2
  ((FAIL++))
}

log_pass() {
  echo -e "${GREEN}[PASS]${NC} $*"
  ((PASS++))
}

# Check function
check() {
  local description="$1"
  local command="$2"

  echo -n "Checking: ${description}... "

  if eval "${command}" > /dev/null 2>&1; then
    log_pass "OK"
    return 0
  else
    log_error "FAILED"
    return 1
  fi
}

# Header
echo "=================================="
echo "CRE GCP Health Check"
echo "=================================="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "Namespace: ${NAMESPACE}"
echo "Project: ${PROJECT_ID}"
echo "Region: ${REGION}"
echo "=================================="
echo

# 1. Cluster Connectivity
echo "[1/10] Cluster Connectivity"
check "kubectl cluster-info" "kubectl cluster-info > /dev/null 2>&1"
check "kubectl get nodes" "kubectl get nodes > /dev/null 2>&1"
echo

# 2. Namespace Exists
echo "[2/10] Namespace"
if kubectl get namespace "${NAMESPACE}" > /dev/null 2>&1; then
  log_pass "Namespace ${NAMESPACE} exists"
else
  log_error "Namespace ${NAMESPACE} does not exist"
fi
echo

# 3. Pod Health
echo "[3/10] Pod Health"
PODS=$(kubectl get pods -n "${NAMESPACE}" -o json 2>/dev/null || echo '{}')
POD_COUNT=$(echo "${PODS}" | jq '.items | length' 2>/dev/null || echo "0")

if [[ "${POD_COUNT}" -eq 0 ]]; then
  log_error "No pods found in namespace ${NAMESPACE}"
else
  READY_PODS=$(echo "${PODS}" | jq '[.items[] | select(.status.phase=="Running")] | length' 2>/dev/null || echo "0")

  if [[ "${READY_PODS}" -eq "${POD_COUNT}" ]]; then
    log_pass "All ${POD_COUNT} pods are Running"
  else
    log_warn "Only ${READY_PODS}/${POD_COUNT} pods are Running"

    if [[ "${VERBOSE}" == "true" ]]; then
      kubectl get pods -n "${NAMESPACE}"
    fi
  fi
fi
echo

# 4. Pod Resource Usage
echo "[4/10] Resource Usage"
if command -v kubectl-top &> /dev/null || kubectl top pods -n "${NAMESPACE}" > /dev/null 2>&1; then
  kubectl top pods -n "${NAMESPACE}" 2>/dev/null || true

  # Check for high resource usage
  HIGH_CPU_PODS=$(kubectl top pods -n "${NAMESPACE}" --no-headers 2>/dev/null | \
    awk '$2 > 90 {print $1}' | wc -l)

  HIGH_MEM_PODS=$(kubectl top pods -n "${NAMESPACE}" --no-headers 2>/dev/null | \
    awk '$3 > 90 {print $1}' | wc -l)

  if [[ "${HIGH_CPU_PODS}" -gt 0 ]]; then
    log_warn "${HIGH_CPU_PODS} pods with >90% CPU"
  fi

  if [[ "${HIGH_MEM_PODS}" -gt 0 ]]; then
    log_warn "${HIGH_MEM_PODS} pods with >90% memory"
  fi
else
  log_warn "Metrics server not available, skipping resource check"
fi
echo

# 5. Service Endpoints
echo "[5/10] Service Endpoints"
SERVICES=$(kubectl get svc -n "${NAMESPACE}" -o json 2>/dev/null || echo '{}')
SERVICE_COUNT=$(echo "${SERVICES}" | jq '.items | length' 2>/dev/null || echo "0")

if [[ "${SERVICE_COUNT}" -gt 0 ]]; then
  log_pass "Found ${SERVICE_COUNT} services"

  for service in $(echo "${SERVICES}" | jq -r '.items[].metadata.name'); do
    ENDPOINTS=$(kubectl get endpoints "${service}" -n "${NAMESPACE}" -o json 2>/dev/null)
    ENDPOINT_COUNT=$(echo "${ENDPOINTS}" | jq '.subsets | length' 2>/dev/null || echo "0")

    if [[ "${ENDPOINT_COUNT}" -gt 0 ]]; then
      log_pass "Service ${service} has endpoints"
    else
      log_warn "Service ${service} has no endpoints"
    fi
  done
else
  log_error "No services found in namespace ${NAMESPACE}"
fi
echo

# 6. Ingress Status
echo "[6/10] Ingress"
INGRESS=$(kubectl get ingress -n "${NAMESPACE}" -o json 2>/dev/null || echo '{}')
INGRESS_COUNT=$(echo "${INGRESS}" | jq '.items | length' 2>/dev/null || echo "0")

if [[ "${INGRESS_COUNT}" -gt 0 ]]; then
  log_pass "Found ${INGRESS_COUNT} ingress resources"

  INGRESS_IP=$(echo "${INGRESS}" | jq -r '.items[0].status.loadBalancer.ingress[0].ip' 2>/dev/null || echo "")

  if [[ -n "${INGRESS_IP}" && "${INGRESS_IP}" != "null" ]]; then
    log_pass "Ingress IP: ${INGRESS_IP}"

    # Test health endpoint
    if command -v curl &> /dev/null; then
      if curl -sf -m 5 "http://${INGRESS_IP}/api/v1/health" > /dev/null 2>&1; then
        log_pass "Health endpoint reachable"
      else
        log_warn "Health endpoint not reachable"
      fi
    fi
  else
    log_warn "Ingress IP not yet assigned"
  fi
else
  log_warn "No ingress resources found"
fi
echo

# 7. PVC Status
echo "[7/10] Persistent Volumes"
PVCS=$(kubectl get pvc -n "${NAMESPACE}" -o json 2>/dev/null || echo '{}')
PVC_COUNT=$(echo "${PVCS}" | jq '.items | length' 2>/dev/null || echo "0")

if [[ "${PVC_COUNT}" -gt 0 ]]; then
  log_pass "Found ${PVC_COUNT} PVCs"

  for pvc in $(echo "${PVCS}" | jq -r '.items[].metadata.name'); do
    STATUS=$(echo "${PVCS}" | jq -r ".items[] | select(.metadata.name==\"${pvc}\") | .status.phase")

    if [[ "${STATUS}" == "Bound" ]]; then
      log_pass "PVC ${pvc} is Bound"
    else
      log_error "PVC ${pvc} status: ${STATUS}"
    fi
  done
else
  log_info "No PVCs found (may be using ephemeral storage)"
fi
echo

# 8. CRE Process Health
echo "[8/10] CRE Process"
CRE_POD=$(kubectl get pod -n "${NAMESPACE}" -l app=cre -o jsonpath='{.items[0].metadata.name}' 2>/dev/null || echo "")

if [[ -n "${CRE_POD}" ]]; then
  # Check if CRE master process is running
  CRE_STATUS=$(kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "erlang:process_info(whereis(cre_master), status)." 2>/dev/null || echo "")

  if [[ "${CRE_STATUS}" == *"waiting"* ]] || [[ "${CRE_STATUS}" == *"running"* ]]; then
    log_pass "CRE master process is running"
  else
    log_warn "CRE master process status unclear"
  fi

  # Check Mnesia
  MNESIA_STATUS=$(kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "mnesia:system_info(is_running)." 2>/dev/null || echo "")

  if [[ "${MNESIA_STATUS}" == *"true"* ]]; then
    log_pass "Mnesia is running"
  else
    log_warn "Mnesia status unclear"
  fi
else
  log_warn "No CRE pods found for process check"
fi
echo

# 9. Recent Events
echo "[9/10] Recent Events"
RECENT_ERRORS=$(kubectl get events -n "${NAMESPACE}" --field-selector=type=Warning \
  --since=1h 2>/dev/null | wc -l || echo "0")

if [[ "${RECENT_ERRORS}" -gt 0 ]]; then
  log_warn "${RECENT_ERRORS} warning events in last hour"

  if [[ "${VERBOSE}" == "true" ]]; then
    kubectl get events -n "${NAMESPACE}" --field-selector=type=Warning --since=1h
  fi
else
  log_pass "No recent warning events"
fi
echo

# 10. GCP Resources
echo "[10/10] GCP Resources"
if command -v gcloud &> /dev/null; then
  # Check GKE cluster
  CLUSTER_STATUS=$(gcloud container clusters describe cre-cluster \
    --region="${REGION}" --project="${PROJECT_ID}" \
    --format="value(status)" 2>/dev/null || echo "")

  if [[ "${CLUSTER_STATUS}" == "RUNNING" ]]; then
    log_pass "GKE cluster is RUNNING"
  elif [[ -n "${CLUSTER_STATUS}" ]]; then
    log_warn "GKE cluster status: ${CLUSTER_STATUS}"
  else
    log_warn "Could not fetch GKE cluster status"
  fi
else
  log_warn "gcloud not found, skipping GCP checks"
fi
echo

# Summary
echo "=================================="
echo "Health Check Summary"
echo "=================================="
echo -e "${GREEN}Passed:${NC} ${PASS}"
echo -e "${YELLOW}Warnings:${NC} ${WARN}"
echo -e "${RED}Failed:${NC} ${FAIL}"
echo

# Exit code
if [[ "${FAIL}" -gt 0 ]]; then
  exit 1
elif [[ "${WARN}" -gt 0 ]]; then
  exit 2
else
  exit 0
fi
