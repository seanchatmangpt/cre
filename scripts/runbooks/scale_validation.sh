#!/bin/bash
# CRE GCP Scale Validation Script
# Validates scaling operations for CRE on GCP/GKE
#
# Usage: ./scripts/runbooks/scale_validation.sh [--namespace NAMESPACE]

set -euo pipefail

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Defaults
NAMESPACE="${CRE_NAMESPACE:-cre}"
DEPLOYMENT_NAME="${CRE_DEPLOYMENT:-cre}"
MIN_REPLICAS="${CRE_MIN_REPLICAS:-3}"
TIMEOUT="${TIMEOUT:-300}"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --namespace|-n)
      NAMESPACE="$2"
      shift 2
      ;;
    --deployment|-d)
      DEPLOYMENT_NAME="$2"
      shift 2
      ;;
    --timeout|-t)
      TIMEOUT="$2"
      shift 2
      ;;
    --help|-h)
      echo "Usage: $0 [OPTIONS]"
      echo "Options:"
      echo "  --namespace, -n   Kubernetes namespace (default: cre)"
      echo "  --deployment, -d  Deployment name (default: cre)"
      echo "  --timeout, -t     Timeout in seconds (default: 300)"
      echo "  --help, -h        Show this help"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Functions
log_info() {
  echo -e "${BLUE}[INFO]${NC} $*"
}

log_pass() {
  echo -e "${GREEN}[PASS]${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
  echo -e "${RED}[FAIL]${NC} $*"
}

wait_for_pods() {
  local expected=$1
  local timeout=$2
  local elapsed=0

  log_info "Waiting for ${expected} pods to be ready..."

  while [[ ${elapsed} -lt ${timeout} ]]; do
    READY=$(kubectl get deployment "${DEPLOYMENT_NAME}" -n "${NAMESPACE}" \
      -o jsonpath='{.status.readyReplicas}' 2>/dev/null || echo "0")

    if [[ "${READY}" -eq "${expected}" ]]; then
      log_pass "All ${expected} pods are ready"
      return 0
    fi

    sleep 5
    elapsed=$((elapsed + 5))
    echo -n "."
  done

  echo
  log_error "Timeout waiting for pods (ready: ${READY}/${expected})"
  return 1
}

check_pod_distribution() {
  log_info "Checking pod distribution across nodes..."

  local node_count=$(kubectl get nodes -o jsonpath='{.items}' | jq '. | length')
  local pod_count=$(kubectl get pods -n "${NAMESPACE}" -l app="${DEPLOYMENT_NAME}" -o json | \
    jq '[.items[] | select(.spec.nodeName != null)] | length')

  if [[ "${pod_count}" -eq 0 ]]; then
    log_warn "No pods found for distribution check"
    return 0
  fi

  # Get distribution
  kubectl get pods -n "${NAMESPACE}" -l app="${DEPLOYMENT_NAME}" -o wide | \
    awk 'NR>1 {print $7}' | sort | uniq -c | while read -r count node; do
      log_info "  Node ${node}: ${count} pod(s)"
    done

  log_pass "Pod distribution check complete"
}

check_resource_requests() {
  log_info "Checking resource requests..."

  local deployment
  deployment=$(kubectl get deployment "${DEPLOYMENT_NAME}" -n "${NAMESPACE}" -o json)

  local cpu_request=$(echo "${deployment}" | \
    jq -r '.spec.template.spec.containers[0].resources.requests.cpu // "none"')
  local mem_request=$(echo "${deployment}" | \
    jq -r '.spec.template.spec.containers[0].resources.requests.memory // "none"')
  local cpu_limit=$(echo "${deployment}" | \
    jq -r '.spec.template.spec.containers[0].resources.limits.cpu // "none"')
  local mem_limit=$(echo "${deployment}" | \
    jq -r '.spec.template.spec.containers[0].resources.limits.memory // "none"')

  log_info "  CPU Request: ${cpu_request}"
  log_info "  CPU Limit: ${cpu_limit}"
  log_info "  Memory Request: ${mem_request}"
  log_info "  Memory Limit: ${mem_limit}"

  if [[ "${cpu_request}" == "none" ]] || [[ "${mem_request}" == "none" ]]; then
    log_warn "Resource requests not fully set - HPA may not work correctly"
  else
    log_pass "Resource requests configured"
  fi
}

check_hpa_status() {
  log_info "Checking HPA status..."

  if kubectl get hpa -n "${NAMESPACE}" "${DEPLOYMENT_NAME}-hpa" &>/dev/null; then
    kubectl get hpa -n "${NAMESPACE}" "${DEPLOYMENT_NAME}-hpa"

    local min_replicas=$(kubectl get hpa -n "${NAMESPACE}" "${DEPLOYMENT_NAME}-hpa" \
      -o jsonpath='{.spec.minReplicas}')
    local max_replicas=$(kubectl get hpa -n "${NAMESPACE}" "${DEPLOYMENT_NAME}-hpa" \
      -o jsonpath='{.spec.maxReplicas}')
    local current_replicas=$(kubectl get hpa -n "${NAMESPACE}" "${DEPLOYMENT_NAME}-hpa" \
      -o jsonpath='{.status.currentReplicas}')

    log_info "  Min: ${min_replicas}, Max: ${max_replicas}, Current: ${current_replicas}"
    log_pass "HPA is configured"
  else
    log_warn "HPA not found for ${DEPLOYMENT_NAME}"
  fi
}

check_cluster_autoscaler() {
  log_info "Checking cluster autoscaler..."

  if kubectl get deployment -n kube-system cluster-autoscaler &>/dev/null; then
    local ca_replicas=$(kubectl get deployment -n kube-system cluster-autoscaler \
      -o jsonpath='{.status.readyReplicas}')

    if [[ "${ca_replicas}" -gt 0 ]]; then
      log_pass "Cluster autoscaler is running"

      # Check logs for recent activity
      local recent_scale=$(kubectl logs -n kube-system -l k8s-app=cluster-autoscaler \
        --tail=100 2>/dev/null | grep -c "scale-up" || echo "0")

      if [[ "${recent_scale}" -gt 0 ]]; then
        log_info "  Recent scale-up events: ${recent_scale}"
      fi
    else
      log_warn "Cluster autoscaler deployment not ready"
    fi
  else
    log_warn "Cluster autoscaler not found"
  fi
}

check_pod_resources() {
  log_info "Checking pod resource usage..."

  if command -v kubectl-top &> /dev/null; then
    kubectl top pods -n "${NAMESPACE}" -l app="${DEPLOYMENT_NAME}"

    # Check for over-provisioned pods
    local high_cpu=$(kubectl top pods -n "${NAMESPACE}" -l app="${DEPLOYMENT_NAME}" \
      --no-headers 2>/dev/null | awk '$2+0 > 80 {print $1}' | wc -l)
    local high_mem=$(kubectl top pods -n "${NAMESPACE}" -l app="${DEPLOYMENT_NAME}" \
      --no-headers 2>/dev/null | awk '$3+0 > 80 {print $1}' | wc -l)

    if [[ "${high_cpu}" -gt 0 ]]; then
      log_warn "${high_cpu} pod(s) with >80% CPU"
    fi

    if [[ "${high_mem}" -gt 0 ]]; then
      log_warn "${high_mem} pod(s) with >80% memory"
    fi
  else
    log_warn "Metrics server not available, skipping resource check"
  fi
}

# Main execution
echo "=================================="
echo "CRE Scale Validation"
echo "=================================="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "Namespace: ${NAMESPACE}"
echo "Deployment: ${DEPLOYMENT_NAME}"
echo "=================================="
echo

# Pre-flight checks
log_info "Running pre-flight checks..."

if ! kubectl get namespace "${NAMESPACE}" &>/dev/null; then
  log_error "Namespace ${NAMESPACE} does not exist"
  exit 1
fi

if ! kubectl get deployment "${DEPLOYMENT_NAME}" -n "${NAMESPACE}" &>/dev/null; then
  log_error "Deployment ${DEPLOYMENT_NAME} does not exist"
  exit 1
fi

log_pass "Pre-flight checks passed"
echo

# Get current state
CURRENT_REPLICAS=$(kubectl get deployment "${DEPLOYMENT_NAME}" -n "${NAMESPACE}" \
  -o jsonpath='{.spec.replicas}')
log_info "Current replica count: ${CURRENT_REPLICAS}"

# Validation checks
echo "=================================="
echo "Scaling Validation Checks"
echo "=================================="
echo

check_resource_requests
echo

check_hpa_status
echo

check_cluster_autoscaler
echo

check_pod_resources
echo

check_pod_distribution
echo

# Scale up test
if [[ "${DEPLOYMENT_NAME}" == "cre" ]]; then
  log_info "Running scale-up test..."

  TARGET_REPLICAS=$((CURRENT_REPLICAS + 2))
  log_info "Scaling to ${TARGET_REPLICAS} replicas..."

  kubectl scale deployment "${DEPLOYMENT_NAME}" --replicas="${TARGET_REPLICAS}" -n "${NAMESPACE}"

  if wait_for_pods "${TARGET_REPLICAS}" "${TIMEOUT}"; then
    log_pass "Scale-up test passed"

    # Scale back down
    log_info "Scaling back to ${CURRENT_REPLICAS} replicas..."
    kubectl scale deployment "${DEPLOYMENT_NAME}" --replicas="${CURRENT_REPLICAS}" -n "${NAMESPACE}"
    wait_for_pods "${CURRENT_REPLICAS}" "${TIMEOUT}"
  else
    log_error "Scale-up test failed"
    exit 1
  fi
fi

# Summary
echo
echo "=================================="
echo "Validation Summary"
echo "=================================="
echo "All validation checks completed"
echo

exit 0
