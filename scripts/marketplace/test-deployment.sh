#!/bin/bash
set -e

echo "=========================================="
echo "CRE Marketplace Deployment Test"
echo "=========================================="

PROJECT_ID="${1:?Usage: $0 PROJECT_ID [CLUSTER_NAME] [ZONE]}"
CLUSTER_NAME="${2:-cre-marketplace-test}"
ZONE="${3:-us-central1-a}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# ============================================
# Step 1: Create test GKE cluster
# ============================================
log "Creating test GKE cluster..."

gcloud container clusters create ${CLUSTER_NAME} \
  --project=${PROJECT_ID} \
  --zone=${ZONE} \
  --num-nodes=3 \
  --machine-type=e2-medium \
  --image-type=cOS \
  --disk-type=pd-standard \
  --disk-size=100GB \
  --enable-ip-alias \
  --enable-private-nodes \
  --master-ipv4-cidr=172.16.0.0/28 \
  --enable-shielded-nodes \
  --shielded-secure-boot \
  --shielded-vtpm \
  --shielded-integrity-monitoring \
  --workload-pool=${PROJECT_ID}.svc.id.goog \
  --security-posture=enterprise \
  || error "Failed to create GKE cluster"

log "GKE cluster created successfully"

# ============================================
# Step 2: Get cluster credentials
# ============================================
log "Getting cluster credentials..."

gcloud container clusters get-credentials ${CLUSTER_NAME} \
  --project=${PROJECT_ID} \
  --zone=${ZONE} \
  || error "Failed to get credentials"

log "Credentials configured"

# ============================================
# Step 3: Create namespace
# ============================================
log "Creating CRE namespace..."

kubectl create namespace cre || warn "Namespace already exists"

log "Namespace created"

# ============================================
# Step 4: Deploy CRE via Helm
# ============================================
log "Deploying CRE via Helm..."

helm install cre ./k8s/charts/cre \
  --namespace cre \
  --values ./k8s/charts/cre/values-gke-marketplace.yaml \
  --set image.repository=us-central1-docker.pkg.dev/${PROJECT_ID}/cre/cre \
  --set image.tag=0.3.0 \
  --timeout 10m \
  || error "Helm installation failed"

log "CRE deployed successfully"

# ============================================
# Step 5: Wait for pods to be ready
# ============================================
log "Waiting for CRE pods to be ready..."

kubectl wait --for=condition=ready pod -l app=cre -n cre --timeout=300s \
  || error "Pods did not become ready in time"

log "All CRE pods are ready"

# ============================================
# Step 6: Verify health endpoints
# ============================================
log "Verifying health endpoints..."

kubectl port-forward -n cre svc/cre 4142:4142 &
PF_PID=$!
sleep 5

# Test /health
HEALTH=$(curl -s http://localhost:4142/health)
echo "Health check response: ${HEALTH}"
echo "${HEALTH}" | grep -q '"status":"healthy"' || error "Health check failed"

# Test /ready
READY=$(curl -s http://localhost:4142/ready)
echo "Readiness check response: ${READY}"
echo "${READY}" | grep -q '"status":"healthy"' || error "Readiness check failed"

# Test /startup
STARTUP=$(curl -s http://localhost:4142/startup)
echo "Startup check response: ${STARTUP}"
echo "${STARTUP}" | grep -q '"status":"healthy"' || error "Startup check failed"

# Test /status.json
STATUS=$(curl -s http://localhost:4142/status.json)
echo "Status response: ${STATUS}"
echo "${STATUS}" | grep -q '"status"' || error "Status check failed"

kill ${PF_PID}

log "All health endpoints verified"

# ============================================
# Step 7: Verify StatefulSet
# ============================================
log "Verifying StatefulSet..."

REPLICAS=$(kubectl get statefulset cre -n cre -o jsonpath='{.spec.replicas}')
READY_REPLICAS=$(kubectl get statefulset cre -n cre -o jsonpath='{.status.readyReplicas}')

echo "Expected replicas: ${REPLICAS}, Ready replicas: ${READY_REPLICAS}"

[ "${REPLICAS}" -eq "${READY_REPLICAS}" ] || error "Not all replicas are ready"

log "StatefulSet is healthy"

# ============================================
# Step 8: Verify persistent volumes
# ============================================
log "Verifying persistent volumes..."

PVC_COUNT=$(kubectl get pvc -n cre -l app=cre --no-headers | wc -l)
echo "PVCs created: ${PVC_COUNT}"

[ "${PVC_COUNT}" -eq "${REPLICAS}" ] || error "PVC count does not match replica count"

log "Persistent volumes verified"

# ============================================
# Step 9: Test pod disruption
# ============================================
log "Testing pod disruption budget..."

# Try to delete a pod (should be replaced immediately)
POD_NAME=$(kubectl get pods -n cre -l app=cre -o jsonpath='{.items[0].metadata.name}')
kubectl delete pod ${POD_NAME} -n cre

sleep 10

# Verify pod was replaced
NEW_POD_COUNT=$(kubectl get pods -n cre -l app=cre --no-headers | wc -l)
echo "Pod count after disruption: ${NEW_POD_COUNT}"

[ "${NEW_POD_COUNT}" -eq "${REPLICAS}" ] || error "Pod was not replaced"

log "Pod disruption budget working correctly"

# ============================================
# Step 10: Cleanup
# ============================================
log "Cleaning up..."

helm uninstall cre -n cre || warn "Helm uninstall failed"
kubectl delete namespace cre || warn "Namespace deletion failed"
gcloud container clusters delete ${CLUSTER_NAME} --project=${PROJECT_ID} --zone=${ZONE} --quiet || warn "Cluster deletion failed"

log "=========================================="
echo "ALL TESTS PASSED!"
echo "=========================================="
