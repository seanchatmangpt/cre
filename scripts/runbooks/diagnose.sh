#!/bin/bash
# CRE GCP Diagnostic Script
# Collects diagnostic information for troubleshooting CRE on GCP
#
# Usage: ./scripts/runbooks/diagnose.sh [--namespace NAMESPACE] [--output-dir DIR]

set -euo pipefail

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Defaults
NAMESPACE="${CRE_NAMESPACE:-cre}"
OUTPUT_DIR="${OUTPUT_DIR:-./cre-diagnostics-$(date +%Y%m%d_%H%M%S)}"
REGION="${CRE_REGION:-us-central1}"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --namespace|-n)
      NAMESPACE="$2"
      shift 2
      ;;
    --output|-o)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --region|-r)
      REGION="$2"
      shift 2
      ;;
    --help|-h)
      echo "Usage: $0 [OPTIONS]"
      echo "Options:"
      echo "  --namespace, -n   Kubernetes namespace (default: cre)"
      echo "  --output, -o      Output directory (default: ./cre-diagnostics-TIMESTAMP)"
      echo "  --region, -r      GCP region"
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

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*"
}

# Create output directory
mkdir -p "${OUTPUT_DIR}"

# Header
echo "=================================="
echo "CRE GCP Diagnostics"
echo "=================================="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "Namespace: ${NAMESPACE}"
echo "Output: ${OUTPUT_DIR}"
echo "=================================="
echo

# Create metadata file
cat > "${OUTPUT_DIR}/metadata.txt" <<EOF
CRE Diagnostic Collection
Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)
Namespace: ${NAMESPACE}
Region: ${REGION}
Hostname: $(hostname)
User: ${USER}
EOF

log_info "Collecting diagnostic information..."

# 1. Cluster Information
log_info "Collecting cluster information..."
kubectl cluster-info > "${OUTPUT_DIR}/cluster-info.txt" 2>&1
kubectl version > "${OUTPUT_DIR}/kubectl-version.txt" 2>&1
kubectl config view > "${OUTPUT_DIR}/kube-config.txt" 2>&1

# 2. Node Information
log_info "Collecting node information..."
kubectl get nodes > "${OUTPUT_DIR}/nodes.txt" 2>&1
kubectl describe nodes > "${OUTPUT_DIR}/nodes-describe.txt" 2>&1
kubectl top nodes > "${OUTPUT_DIR}/nodes-top.txt" 2>&1 || true

# 3. Pod Information
log_info "Collecting pod information..."
kubectl get pods -n "${NAMESPACE}" > "${OUTPUT_DIR}/pods.txt" 2>&1
kubectl describe pods -n "${NAMESPACE}" > "${OUTPUT_DIR}/pods-describe.txt" 2>&1
kubectl top pods -n "${NAMESPACE}" > "${OUTPUT_DIR}/pods-top.txt" 2>&1 || true

# 4. Service Information
log_info "Collecting service information..."
kubectl get svc -n "${NAMESPACE}" > "${OUTPUT_DIR}/services.txt" 2>&1
kubectl describe svc -n "${NAMESPACE}" > "${OUTPUT_DIR}/services-describe.txt" 2>&1
kubectl get endpoints -n "${NAMESPACE}" > "${OUTPUT_DIR}/endpoints.txt" 2>&1

# 5. Ingress Information
log_info "Collecting ingress information..."
kubectl get ingress -n "${NAMESPACE}" > "${OUTPUT_DIR}/ingress.txt" 2>&1 || true
kubectl describe ingress -n "${NAMESPACE}" > "${OUTPUT_DIR}/ingress-describe.txt" 2>&1 || true

# 6. PVC Information
log_info "Collecting PVC information..."
kubectl get pvc -n "${NAMESPACE}" > "${OUTPUT_DIR}/pvc.txt" 2>&1
kubectl describe pvc -n "${NAMESPACE}" > "${OUTPUT_DIR}/pvc-describe.txt" 2>&1

# 7. Event Information
log_info "Collecting events..."
kubectl get events -n "${NAMESPACE}" --sort-by='.lastTimestamp' > "${OUTPUT_DIR}/events.txt" 2>&1
kubectl get events --all-namespaces --field-selector=type=Warning > "${OUTPUT_DIR}/events-all.txt" 2>&1 || true

# 8. Logs
log_info "Collecting logs..."
mkdir -p "${OUTPUT_DIR}/logs"

for pod in $(kubectl get pods -n "${NAMESPACE}" -o jsonpath='{.items[*].metadata.name}'); do
  log_info "  Collecting logs for ${pod}..."
  kubectl logs -n "${NAMESPACE}" "${pod}" > "${OUTPUT_DIR}/logs/${pod}.log" 2>&1 || true
  kubectl logs -n "${NAMESPACE}" "${pod}" --previous > "${OUTPUT_DIR}/logs/${pod}-previous.log" 2>&1 || true
done

# 9. ConfigMaps and Secrets (sanitized)
log_info "Collecting configuration..."
kubectl get configmaps -n "${NAMESPACE}" > "${OUTPUT_DIR}/configmaps.txt" 2>&1
kubectl describe configmaps -n "${NAMESPACE}" > "${OUTPUT_DIR}/configmaps-describe.txt" 2>&1

# Secrets (names only, no values)
kubectl get secrets -n "${NAMESPACE}" > "${OUTPUT_DIR}/secrets.txt" 2>&1

# 10. Deployment Details
log_info "Collecting deployment details..."
kubectl get deployments -n "${NAMESPACE}" -o yaml > "${OUTPUT_DIR}/deployments.yaml" 2>&1
kubectl get statefulsets -n "${NAMESPACE}" -o yaml > "${OUTPUT_DIR}/statefulsets.yaml" 2>&1 || true
kubectl get daemonsets -n "${NAMESPACE}" -o yaml > "${OUTPUT_DIR}/daemonsets.yaml" 2>&1 || true

# 11. HPA Status
log_info "Collecting HPA status..."
kubectl get hpa -n "${NAMESPACE}" > "${OUTPUT_DIR}/hpa.txt" 2>&1 || true
kubectl describe hpa -n "${NAMESPACE}" > "${OUTPUT_DIR}/hpa-describe.txt" 2>&1 || true

# 12. Network Policies
log_info "Collecting network policies..."
kubectl get networkpolicies -n "${NAMESPACE}" > "${OUTPUT_DIR}/networkpolicies.txt" 2>&1 || true

# 13. Resource Quotas
log_info "Collecting resource quotas..."
kubectl get quota -n "${NAMESPACE}" > "${OUTPUT_DIR}/quota.txt" 2>&1 || true
kubectl describe quota -n "${NAMESPACE}" > "${OUTPUT_DIR}/quota-describe.txt" 2>&1 || true

# 14. CRE Process Information
log_info "Collecting CRE process information..."
CRE_POD=$(kubectl get pod -n "${NAMESPACE}" -l app=cre -o jsonpath='{.items[0].metadata.name}' 2>/dev/null || echo "")

if [[ -n "${CRE_POD}" ]]; then
  # Mnesia status
  kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "mnesia:system_info()." > "${OUTPUT_DIR}/mnesia-status.txt" 2>&1 || true

  # Process list
  kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "erlang:processes()." > "${OUTPUT_DIR}/erlang-processes.txt" 2>&1 || true

  # Memory info
  kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "erlang:memory()." > "${OUTPUT_DIR}/erlang-memory.txt" 2>&1 || true

  # System info
  kubectl exec -n "${NAMESPACE}" "${CRE_POD}" -- \
    /opt/cre/bin/cre_eval "erlang:system_info(info)." > "${OUTPUT_DIR}/erlang-system.txt" 2>&1 || true
fi

# 15. GCP Information
log_info "Collecting GCP information..."
if command -v gcloud &> /dev/null; then
  gcloud container clusters describe cre-cluster --region="${REGION}" > "${OUTPUT_DIR}/gke-cluster.txt" 2>&1 || true
  gcloud compute instances list --project="${CRE_PROJECT_ID:-your-project-id}" > "${OUTPUT_DIR}/gce-instances.txt" 2>&1 || true
fi

# 16. Summary Analysis
log_info "Generating summary analysis..."
cat > "${OUTPUT_DIR}/summary.txt" <<EOF
CRE Diagnostic Summary
======================

Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)
Namespace: ${NAMESPACE}

Cluster Status
--------------
EOF

# Add cluster health to summary
if kubectl cluster-info &>/dev/null; then
  echo "Cluster: Connected" >> "${OUTPUT_DIR}/summary.txt"
else
  echo "Cluster: NOT CONNECTED" >> "${OUTPUT_DIR}/summary.txt"
fi

# Add pod counts
POD_COUNT=$(kubectl get pods -n "${NAMESPACE}" -o json 2>/dev/null | jq '.items | length' 2>/dev/null || echo "0")
RUNNING_COUNT=$(kubectl get pods -n "${NAMESPACE}" -o json 2>/dev/null | jq '[.items[] | select(.status.phase=="Running")] | length' 2>/dev/null || echo "0")

cat >> "${OUTPUT_DIR}/summary.txt" <<EOF
Pods: ${RUNNING_COUNT}/${POD_COUNT} Running

Recent Errors
-------------
EOF

# Add recent errors
kubectl get events -n "${NAMESPACE}" --field-selector=type=Warning --since=1h 2>/dev/null | tail -10 >> "${OUTPUT_DIR}/summary.txt" || true

# Create archive
log_info "Creating diagnostic archive..."
tar -czf "${OUTPUT_DIR}.tar.gz" -C "$(dirname "${OUTPUT_DIR}")" "$(basename "${OUTPUT_DIR}")" 2>/dev/null || true

# Summary
echo
echo "=================================="
echo "Diagnostics Complete"
echo "=================================="
echo "Output: ${OUTPUT_DIR}"
echo "Archive: ${OUTPUT_DIR}.tar.gz"
echo
echo "Files collected:"
ls -1 "${OUTPUT_DIR}/"
echo

log_info "Share the archive file for further analysis:"
echo "  ${OUTPUT_DIR}.tar.gz"

exit 0
